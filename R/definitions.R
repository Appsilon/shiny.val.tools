#' Walk a parsed expression and yield every static definition.
#'
#' A *definition* is a node-producing site in the graph model.
#' v1 recognizes:
#'   - `output$x <- ...` / `output[["x"]] <- ...` → kind `"output"`
#'   - `name <- reactive(...)` / `eventReactive(...)` / `bindEvent(reactive(...))`
#'     → kind `"reactive"`
#'   - `name <- observe(...)` / `observeEvent(...)` / `downloadHandler(...)`
#'     → kind `"observer"`
#'   - `reactiveValues(a = ...)` entries and later `rv$a <- ...` writes
#'     → kind `"value"`
#'   - `moduleServer()` call sites, and the legacy
#'     `name <- function(input, output, session, ...)` form
#'     → kind `"module_server"`
#'   - top-level `name <- function(...) ...` definitions → kind `"function"`
#'     (the app's own helpers; spec 06 derives test-surface helpers from them)
#'
#' Dynamic forms (`output[[expr]] <- ...`) are skipped: static analysis
#' cannot resolve them. Dynamic *reads* (`input[[expr]]`) are picked up by
#' the references walker, which is what emits SVT-W001.
#'
#' Returns a list of records: `list(kind, name, namespace, def_call, line, col)`.
#' `def_call` is the RHS call name (`renderPlot`, `eventReactive`, ...) and is
#' `NA` for kinds that have no defining call.
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

  reactive_callees  <- c("reactive", "eventReactive")
  observer_callees  <- c("observe", "observeEvent", "downloadHandler")

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

  walk <- function(expr, own_srcref, namespace, enclosing_name,
                   enclosing_formals = character()) {
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
        wrapper_binding = enclosing_name %||% NA_character_,
        wrapper_formals = paste(enclosing_formals, collapse = ",")
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
        if (!walkable(child)) next
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
          def_call = rhs_call_name(rhs),
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
            wrapper_binding = bind_nm,
            wrapper_formals = paste(formal_names(rhs), collapse = ",")
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
            def_call = rhs_call_name(rhs),
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
          # Only definitions at file top level are helpers. A function
          # bound inside another function body is a local, not an
          # app-level entry point, and spec 06's helper stubs cannot call
          # it. `enclosing_name` is NA exactly at top level.
          if (is.na(enclosing_name %||% NA_character_)) {
            emit(list(
              kind = "function",
              name = bind_nm,
              namespace = namespace,
              container = NA_character_,
              def_call = NA_character_,
              line = loc$line,
              col = loc$col
            ))
          }
          body_expr <- rhs[[3L]]
          walk(body_expr, child_srcref(rhs, 3L) %||% own_srcref,
               namespace = namespace, enclosing_name = bind_nm,
               enclosing_formals = formal_names(rhs))
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
      if (!walkable(child)) next
      child_enclosing <- if (is_fn_def && i >= 2L) NA_character_ else enclosing_name
      child_formals <- if (is_fn_def && i >= 2L) character() else enclosing_formals
      walk(child, child_srcref(expr, i) %||% pick_srcref(child) %||% own_srcref,
           namespace = namespace, enclosing_name = child_enclosing,
           enclosing_formals = child_formals)
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
#' Columns: `from_file`, `kind`, `name`, `namespace`, `container`,
#' `def_call`, `line`, `col`, `wrapper_binding`, `wrapper_formals`.
#' `wrapper_formals` is the comma-joined formal names of a module's
#' wrapper function, which is what a generated `testServer()` scaffold
#' needs to fill `args = list(...)`. `kind` covers every value listed on
#' `find_definitions()`.
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
  def_calls <- character()
  formals_v <- character()
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
      def_calls <- c(def_calls, def$def_call %||% NA_character_)
      lines <- c(lines, as.integer(def$line))
      cols <- c(cols, as.integer(def$col))
      wrappers <- c(wrappers, def$wrapper_binding %||% NA_character_)
      formals_v <- c(formals_v, def$wrapper_formals %||% NA_character_)
    }
  }

  tibble::tibble(
    from_file = from_files,
    kind = kinds,
    name = names_v,
    namespace = namespaces,
    container = containers,
    def_call = def_calls,
    line = lines,
    col = cols,
    wrapper_binding = wrappers,
    wrapper_formals = formals_v
  )
 })
}
