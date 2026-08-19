#' Walk a parsed expression and yield every static definition.
#'
#' A *definition* is a node-producing site in the graph model.
#' v1 recognizes:
#'   - `output$x <- ...` and `output[["x"]] <- ...` assignments → kind `"output"`
#'
#' Dynamic forms (`output[[expr]] <- ...`) are skipped: static analysis
#' cannot resolve them. The dynamic case is the foundation for SVT-W001
#' but emitting that warning is a follow-up slice.
#'
#' Returns a list of records: `list(kind, name, namespace, line, col)`.
#' `namespace` is `NA_character_` for top-level definitions; nodes whose
#' enclosing function is a `moduleServer()` body carry the module's
#' identity (file-path-derived; see `module_identity()`). For
#' `kind = "module_server"` rows, `name` is also the module identity
#' rather than the wrapper-function binding name.
#'
#' @noRd
find_definitions <- function(parsed_expr, from_file = NA_character_) {
  acc <- list()
  multi_module_file <- count_module_server_calls(parsed_expr) > 1L
  # Names known to be bound to reactiveValues() — so subsequent
  # `rv$x <- ...` writes can be recognized as value definitions and not
  # confused with arbitrary list-element writes.
  rv_containers <- character()

  # An expression's srcref may be either a single `srcref` (for top-level
  # expressions) or a list of `srcref`s indexed by child position (for the
  # children of a brace block or similar). `child_srcref()` resolves which
  # one applies for `parent[[i]]`.
  pick_srcref <- function(expr) {
    sr <- attr(expr, "srcref")
    if (inherits(sr, "srcref")) return(sr)
    NULL
  }

  child_srcref <- function(parent_expr, i) {
    sr_list <- attr(parent_expr, "srcref")
    if (is.list(sr_list) && i >= 1L && i <= length(sr_list)) {
      candidate <- sr_list[[i]]
      if (inherits(candidate, "srcref")) return(candidate)
    }
    NULL
  }

  loc_from <- function(srcref) {
    if (is.null(srcref)) return(list(line = NA_integer_, col = NA_integer_))
    s <- as.integer(srcref)
    list(line = s[1L], col = s[2L])
  }

  is_assignment <- function(head) {
    is.name(head) && as.character(head) %in% c("<-", "=", "<<-")
  }

  # `output$x` parses as `$(output, x)`; `output[["x"]]` as `[[(output, "x")]]`.
  # Returns the bare name string, or NULL when the LHS is not a static
  # `output$<name>` / `output[["<name>"]]` form.
  output_name <- function(lhs) {
    if (!is.call(lhs) || length(lhs) != 3L) return(NULL)
    op <- as.character(lhs[[1L]])
    base <- lhs[[2L]]
    key <- lhs[[3L]]
    if (!(is.name(base) && identical(as.character(base), "output"))) return(NULL)
    if (op == "$") {
      if (is.name(key)) {
        nm <- as.character(key)
        if (nzchar(nm)) return(nm)
      }
      return(NULL)
    }
    if (op == "[[") {
      if (is.character(key) && length(key) == 1L && !is.na(key) && nzchar(key)) {
        return(key)
      }
      return(NULL)
    }
    NULL
  }

  is_box_use <- function(head) {
    is.call(head) && length(head) == 3L &&
      identical(as.character(head[[1L]]), "::") &&
      identical(as.character(head[[2L]]), "box") &&
      identical(as.character(head[[3L]]), "use")
  }

  is_function_def <- function(expr) {
    is.call(expr) && length(expr) >= 3L && is.name(expr[[1L]]) &&
      as.character(expr[[1L]]) == "function"
  }

  is_module_server_call <- function(head) {
    if (is.name(head)) return(identical(as.character(head), "moduleServer"))
    if (is.call(head) && length(head) == 3L &&
        as.character(head[[1L]]) %in% c("::", ":::") &&
        identical(as.character(head[[2L]]), "shiny") &&
        identical(as.character(head[[3L]]), "moduleServer")) {
      return(TRUE)
    }
    FALSE
  }

  # A Shiny module's server function has `input, output, session` as its
  # first three formals (legacy `callModule` form). The same shape is the
  # signature of the top-level app server, so we only treat it as a module
  # when the function is the RHS of an assignment to a name.
  has_module_signature <- function(fn_expr) {
    if (!is_function_def(fn_expr)) return(FALSE)
    formals_pl <- fn_expr[[2L]]
    arg_names <- names(as.list(formals_pl))
    if (length(arg_names) < 3L) return(FALSE)
    identical(arg_names[1L:3L], c("input", "output", "session"))
  }

  reactive_callees  <- c("reactive", "eventReactive")
  observer_callees  <- c("observe", "observeEvent", "downloadHandler")

  # Returns "reactive", "observer", or NULL — the kind a call expression
  # produces when it appears as the RHS of a named binding. `bindEvent(x, ...)`
  # delegates to its first argument: bindEvent(reactive(...), ...) is still a
  # reactive; bindEvent(observe(...), ...) is still an observer.
  classify_rhs <- function(rhs) {
    if (!is.call(rhs)) return(NULL)
    head <- rhs[[1L]]
    nm <- if (is.name(head)) {
      as.character(head)
    } else if (is.call(head) && length(head) == 3L &&
               as.character(head[[1L]]) %in% c("::", ":::")) {
      as.character(head[[3L]])
    } else {
      return(NULL)
    }
    if (nm %in% reactive_callees) return("reactive")
    if (nm %in% observer_callees) return("observer")
    if (nm == "bindEvent" && length(rhs) >= 2L) {
      return(classify_rhs(rhs[[2L]]))
    }
    NULL
  }

  # Bare-name extraction for the LHS of a named binding. `name <- ...` and
  # `name = ...` both produce a `name` symbol on the LHS; we ignore non-name
  # LHSs (e.g. element assignments — `x[[1]] <- reactive(...)` — which we
  # don't model as named bindings).
  binding_name <- function(lhs) {
    if (is.name(lhs)) {
      nm <- as.character(lhs)
      if (nzchar(nm)) return(nm)
    }
    NULL
  }

  # `reactiveValues(a = ..., b = ...)` → list("a", "b").
  reactive_values_keys <- function(rhs) {
    if (!is.call(rhs)) return(NULL)
    head <- rhs[[1L]]
    nm <- if (is.name(head)) {
      as.character(head)
    } else if (is.call(head) && length(head) == 3L &&
               as.character(head[[1L]]) %in% c("::", ":::")) {
      as.character(head[[3L]])
    } else {
      return(NULL)
    }
    if (nm != "reactiveValues") return(NULL)
    args <- as.list(rhs)[-1L]
    if (!length(args)) return(character())
    arg_names <- names(args)
    if (is.null(arg_names)) arg_names <- rep("", length(args))
    arg_names[nzchar(arg_names)]
  }

  # Static element name for `<container>$<name> <- ...` and
  # `<container>[["<name>"]] <- ...`. Returns the bare name string or NULL.
  rv_write_name <- function(lhs) {
    if (!is.call(lhs) || length(lhs) != 3L) return(NULL)
    op <- as.character(lhs[[1L]])
    base <- lhs[[2L]]
    key <- lhs[[3L]]
    if (!is.name(base)) return(NULL)
    container <- as.character(base)
    if (!nzchar(container)) return(NULL)
    if (op == "$") {
      if (is.name(key)) {
        nm <- as.character(key)
        if (nzchar(nm)) return(list(container = container, name = nm))
      }
      return(NULL)
    }
    if (op == "[[") {
      if (is.character(key) && length(key) == 1L && !is.na(key) && nzchar(key)) {
        return(list(container = container, name = key))
      }
      return(NULL)
    }
    NULL
  }

  emit <- function(record) {
    acc[[length(acc) + 1L]] <<- record
  }

  walk <- function(expr, own_srcref, namespace, enclosing_name) {
    if (!tryCatch({ force(expr); TRUE }, error = function(e) FALSE)) {
      return(invisible())
    }
    if (!is.call(expr)) return(invisible())

    head <- expr[[1L]]
    if (is_box_use(head)) return(invisible())

    if (is_module_server_call(head) && length(expr) >= 3L) {
      loc <- loc_from(own_srcref)
      mod_id <- module_identity(from_file, enclosing_name,
                                multi = multi_module_file)
      emit(list(
        kind = "module_server",
        name = mod_id,
        namespace = NA_character_,
        container = NA_character_,
        line = loc$line,
        col = loc$col,
        wrapper_binding = enclosing_name %||% NA_character_
      ))
      inner_fn <- expr[[3L]]
      inner_ns <- mod_id
      if (is_function_def(inner_fn)) {
        body_expr <- inner_fn[[3L]]
        walk(body_expr, child_srcref(inner_fn, 3L) %||% own_srcref,
             namespace = inner_ns, enclosing_name = NA_character_)
      } else {
        walk(inner_fn, child_srcref(expr, 3L) %||% own_srcref,
             namespace = inner_ns, enclosing_name = NA_character_)
      }
      # Other args (e.g. session = ...) — walk them in the parent context.
      for (i in seq_along(expr)) {
        if (i == 1L || i == 3L) next
        child <- expr[[i]]
        if (is_missing_arg(child)) next
        if (is.null(child)) next
        if (is.symbol(child) && !nzchar(as.character(child))) next
        walk(child, child_srcref(expr, i) %||% pick_srcref(child) %||% own_srcref,
             namespace = namespace, enclosing_name = NA_character_)
      }
      return(invisible())
    }

    if (is_assignment(head) && length(expr) == 3L) {
      lhs <- expr[[2L]]
      rhs <- expr[[3L]]
      loc <- loc_from(own_srcref)

      out_nm <- output_name(lhs)
      if (!is.null(out_nm)) {
        emit(list(
          kind = "output",
          name = out_nm,
          namespace = namespace,
          container = NA_character_,
          line = loc$line,
          col = loc$col
        ))
        walk(rhs, child_srcref(expr, 3L) %||% own_srcref,
             namespace = namespace, enclosing_name = NA_character_)
        return(invisible())
      }

      bind_nm <- binding_name(lhs)
      if (!is.null(bind_nm)) {
        # Legacy module form: `name <- function(input, output, session, ...) body`.
        # We treat this as a module server when assigned (an unassigned
        # function with this signature is the app server itself, not a
        # module).
        if (has_module_signature(rhs)) {
          mod_id <- module_identity(from_file, bind_nm,
                                    multi = multi_module_file)
          emit(list(
            kind = "module_server",
            name = mod_id,
            namespace = NA_character_,
            container = NA_character_,
            line = loc$line,
            col = loc$col,
            wrapper_binding = bind_nm
          ))
          body_expr <- rhs[[3L]]
          walk(body_expr, child_srcref(rhs, 3L) %||% own_srcref,
               namespace = mod_id, enclosing_name = NA_character_)
          return(invisible())
        }

        rv_keys <- reactive_values_keys(rhs)
        if (!is.null(rv_keys)) {
          rv_containers <<- union(rv_containers, bind_nm)
          for (k in rv_keys) {
            emit(list(
              kind = "value",
              name = k,
              namespace = namespace,
              container = bind_nm,
              line = loc$line,
              col = loc$col
            ))
          }
          walk(rhs, child_srcref(expr, 3L) %||% own_srcref,
               namespace = namespace, enclosing_name = NA_character_)
          return(invisible())
        }
        rhs_kind <- classify_rhs(rhs)
        if (!is.null(rhs_kind)) {
          emit(list(
            kind = rhs_kind,
            name = bind_nm,
            namespace = namespace,
            container = NA_character_,
            line = loc$line,
            col = loc$col
          ))
          walk(rhs, child_srcref(expr, 3L) %||% own_srcref,
               namespace = namespace, enclosing_name = NA_character_)
          return(invisible())
        }

        # Generic `name <- function(...) body` — record `name` as the
        # enclosing context so a moduleServer() call inside picks it up.
        if (is_function_def(rhs)) {
          body_expr <- rhs[[3L]]
          walk(body_expr, child_srcref(rhs, 3L) %||% own_srcref,
               namespace = namespace, enclosing_name = bind_nm)
          return(invisible())
        }
      }

      rv_write <- rv_write_name(lhs)
      if (!is.null(rv_write) && rv_write$container %in% rv_containers) {
        emit(list(
          kind = "value",
          name = rv_write$name,
          namespace = namespace,
          container = rv_write$container,
          line = loc$line,
          col = loc$col
        ))
      }

      walk(rhs, child_srcref(expr, 3L) %||% own_srcref,
           namespace = namespace, enclosing_name = NA_character_)
      return(invisible())
    }

    # `enclosing_name` propagates lexically — descending into a brace block,
    # `if`, or `for` keeps us in the same function. The exception is an
    # anonymous function literal (`function(...) body`), whose body has its
    # own (nameless) function context.
    is_fn_def <- is_function_def(expr)
    for (i in seq_along(expr)) {
      child <- expr[[i]]
      if (is_missing_arg(child)) next
      if (is.null(child)) next
      if (is.symbol(child) && !nzchar(as.character(child))) next
      child_enclosing <- if (is_fn_def && i >= 2L) NA_character_ else enclosing_name
      walk(child, child_srcref(expr, i) %||% pick_srcref(child) %||% own_srcref,
           namespace = namespace, enclosing_name = child_enclosing)
    }
  }

  for (i in seq_along(parsed_expr)) {
    walk(parsed_expr[[i]],
         pick_srcref(parsed_expr[[i]]) %||% child_srcref(parsed_expr, i),
         namespace = NA_character_,
         enclosing_name = NA_character_)
  }
  acc
}

#' Build the Definitions table for an app.
#'
#' One row per static definition site across every enumerated file.
#' Columns: `from_file`, `kind`, `name`, `namespace`, `line`, `col`.
#'
#' v1 emits only `kind = "output"` rows; reactive / observer / value /
#' module_server kinds land in follow-up slices.
#'
#' @noRd
build_definitions_table <- function(app_path) {
 svt_memoize(paste0("definitions\x1f", app_path), function() {
  files <- enumerate_app_files(app_path)

  from_files <- character()
  kinds <- character()
  names_v <- character()
  namespaces <- character()
  containers <- character()
  lines <- integer()
  cols <- integer()
  wrappers <- character()

  for (f in files) {
    parsed <- parse_file(app_path, f)
    if (is.null(parsed)) next

    for (def in find_definitions(parsed, from_file = f)) {
      from_files <- c(from_files, f)
      kinds <- c(kinds, def$kind)
      names_v <- c(names_v, def$name)
      namespaces <- c(namespaces, def$namespace %||% NA_character_)
      containers <- c(containers, def$container %||% NA_character_)
      lines <- c(lines, as.integer(def$line))
      cols <- c(cols, as.integer(def$col))
      wrappers <- c(wrappers, def$wrapper_binding %||% NA_character_)
    }
  }

  tibble::tibble(
    from_file = from_files,
    kind = kinds,
    name = names_v,
    namespace = namespaces,
    container = containers,
    line = lines,
    col = cols,
    wrapper_binding = wrappers
  )
 })
}
