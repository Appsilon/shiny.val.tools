#' Walk a parsed expression and yield every read-site reference.
#'
#' A *reference* is an edge-producing site in the graph model.
#' v1 emits:
#'   - `input$x` and `input[["x"]]` reads → kind `"input"`
#'   - bare-name function calls — `name(...)` — → kind `"call"`. These are
#'     the syntactic form of named-reactive reads in Shiny; the edge-build
#'     step joins them to definitions to identify reactive consumers.
#'
#' Each reference carries `in_def_*` fields recording the enclosing
#' definition (output / reactive / observer / value / module_server) so the
#' edge-build step knows which node owns the read. References outside any
#' definition (e.g. top-level setup code) carry NA in those fields.
#'
#' Dynamic forms (`input[[expr]]`) are skipped: static analysis cannot
#' resolve them.
#'
#' @noRd
find_references <- function(parsed_expr, from_file = NA_character_) {
  acc <- list()
  rv_containers <- character()
  multi_module_file <- count_module_server_calls(parsed_expr) > 1L

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

  has_module_signature <- function(fn_expr) {
    if (!is_function_def(fn_expr)) return(FALSE)
    arg_names <- names(as.list(fn_expr[[2L]]))
    if (length(arg_names) < 3L) return(FALSE)
    identical(arg_names[1L:3L], c("input", "output", "session"))
  }

  output_name <- function(lhs) {
    if (!is.call(lhs) || length(lhs) != 3L) return(NULL)
    op <- as.character(lhs[[1L]])
    base <- lhs[[2L]]
    key <- lhs[[3L]]
    if (!(is.name(base) && identical(as.character(base), "output"))) return(NULL)
    if (op == "$" && is.name(key) && nzchar(as.character(key))) {
      return(as.character(key))
    }
    if (op == "[[" && is.character(key) && length(key) == 1L &&
        !is.na(key) && nzchar(key)) {
      return(key)
    }
    NULL
  }

  binding_name <- function(lhs) {
    if (is.name(lhs)) {
      nm <- as.character(lhs)
      if (nzchar(nm)) return(nm)
    }
    NULL
  }

  reactive_callees <- c("reactive", "eventReactive")
  observer_callees <- c("observe", "observeEvent", "downloadHandler")

  # Returns "reactive" / "observer" / NULL — the kind a call expression
  # produces when it appears as the RHS of a named binding. Mirrors the
  # logic in find_definitions().
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

  # Reads of `input$x` / `input[["x"]]`. Returns one of:
  #   - list(kind = "static", name = <chr>) — a resolvable read
  #   - list(kind = "dynamic")               — `input[[expr]]` (SVT-W001)
  #   - NULL                                  — not an input read
  input_read_classify <- function(expr) {
    if (!is.call(expr) || length(expr) != 3L) return(NULL)
    head <- expr[[1L]]
    if (!is.name(head)) return(NULL)
    op <- as.character(head)
    base <- expr[[2L]]
    key <- expr[[3L]]
    if (!(is.name(base) && identical(as.character(base), "input"))) return(NULL)
    if (op == "$" && is.name(key) && nzchar(as.character(key))) {
      return(list(kind = "static", name = as.character(key)))
    }
    if (op == "[[" && is.character(key) && length(key) == 1L &&
        !is.na(key) && nzchar(key)) {
      return(list(kind = "static", name = key))
    }
    if (op %in% c("$", "[[")) {
      return(list(kind = "dynamic"))
    }
    NULL
  }

  # `pkg::fn` / `pkg:::fn` head decomposition. Returns
  # list(package, name, internal) or NULL. `internal` is TRUE for `:::`
  # access — surfaced as SVT-W201 by the inventory step.
  pkg_call_decomp <- function(head) {
    if (is.call(head) && length(head) == 3L &&
        as.character(head[[1L]]) %in% c("::", ":::") &&
        is.name(head[[2L]]) && is.name(head[[3L]])) {
      pkg <- as.character(head[[2L]])
      nm <- as.character(head[[3L]])
      op <- as.character(head[[1L]])
      if (nzchar(pkg) && nzchar(nm)) {
        return(list(package = pkg, name = nm, internal = identical(op, ":::")))
      }
    }
    NULL
  }

  # `alias$fn` head decomposition. Returns list(alias, name) or NULL.
  # Used for `box::use(alias = pkg)` and `box::use(pkg)` style imports
  # whose call sites are `alias$fn(args)`. Resolution against the
  # imports table happens in the inventory step.
  alias_call_decomp <- function(head) {
    if (is.call(head) && length(head) == 3L &&
        identical(as.character(head[[1L]]), "$") &&
        is.name(head[[2L]]) && is.name(head[[3L]])) {
      alias <- as.character(head[[2L]])
      nm <- as.character(head[[3L]])
      if (nzchar(alias) && nzchar(nm)) return(list(alias = alias, name = nm))
    }
    NULL
  }

  emit_dynamic_input <- function(namespace, in_def, srcref) {
    loc <- loc_from(srcref)
    acc[[length(acc) + 1L]] <<- list(
      kind = "dynamic_input",
      name = NA_character_,
      namespace = namespace %||% NA_character_,
      container = NA_character_,
      package = NA_character_,
      internal = FALSE,
      in_def_kind = in_def$kind %||% NA_character_,
      in_def_name = in_def$name %||% NA_character_,
      in_def_namespace = in_def$namespace %||% NA_character_,
      line = loc$line,
      col = loc$col
    )
  }

  emit_metaprogramming <- function(name, namespace, in_def, srcref) {
    loc <- loc_from(srcref)
    acc[[length(acc) + 1L]] <<- list(
      kind = "metaprogramming",
      name = name,
      namespace = namespace %||% NA_character_,
      container = NA_character_,
      package = NA_character_,
      internal = FALSE,
      in_def_kind = in_def$kind %||% NA_character_,
      in_def_name = in_def$name %||% NA_character_,
      in_def_namespace = in_def$namespace %||% NA_character_,
      line = loc$line,
      col = loc$col
    )
  }

  metaprogramming_callees <- c(
    "do.call", "eval", "evalq", "Recall",
    "match.call", "sys.call", "parse"
  )

  emit_input <- function(name, namespace, in_def, srcref) {
    loc <- loc_from(srcref)
    acc[[length(acc) + 1L]] <<- list(
      kind = "input",
      name = name,
      namespace = namespace %||% NA_character_,
      container = NA_character_,
      package = NA_character_,
      internal = FALSE,
      in_def_kind = in_def$kind %||% NA_character_,
      in_def_name = in_def$name %||% NA_character_,
      in_def_namespace = in_def$namespace %||% NA_character_,
      line = loc$line,
      col = loc$col
    )
  }

  emit_value <- function(name, container, namespace, in_def, srcref) {
    loc <- loc_from(srcref)
    acc[[length(acc) + 1L]] <<- list(
      kind = "value",
      name = name,
      namespace = namespace %||% NA_character_,
      container = container,
      package = NA_character_,
      internal = FALSE,
      in_def_kind = in_def$kind %||% NA_character_,
      in_def_name = in_def$name %||% NA_character_,
      in_def_namespace = in_def$namespace %||% NA_character_,
      line = loc$line,
      col = loc$col
    )
  }

  # `<container>$<name>` / `<container>[["<name>"]]` read decomposition.
  # Returns list(container, name) only for static name forms; NULL otherwise.
  rv_read_decomp <- function(expr) {
    if (!is.call(expr) || length(expr) != 3L) return(NULL)
    head <- expr[[1L]]
    if (!is.name(head)) return(NULL)
    op <- as.character(head)
    base <- expr[[2L]]
    key <- expr[[3L]]
    if (!is.name(base)) return(NULL)
    container <- as.character(base)
    if (!nzchar(container)) return(NULL)
    if (op == "$" && is.name(key) && nzchar(as.character(key))) {
      return(list(container = container, name = as.character(key)))
    }
    if (op == "[[" && is.character(key) && length(key) == 1L &&
        !is.na(key) && nzchar(key)) {
      return(list(container = container, name = key))
    }
    NULL
  }

  emit_call <- function(name, package, namespace, in_def, srcref,
                        container = NA_character_, internal = FALSE) {
    loc <- loc_from(srcref)
    acc[[length(acc) + 1L]] <<- list(
      kind = "call",
      name = name,
      namespace = namespace %||% NA_character_,
      container = container %||% NA_character_,
      package = package %||% NA_character_,
      internal = isTRUE(internal),
      in_def_kind = in_def$kind %||% NA_character_,
      in_def_name = in_def$name %||% NA_character_,
      in_def_namespace = in_def$namespace %||% NA_character_,
      line = loc$line,
      col = loc$col
    )
  }

  walk <- function(expr, own_srcref, namespace, in_def, enclosing_name) {
    if (!tryCatch({ force(expr); TRUE }, error = function(e) FALSE)) {
      return(invisible())
    }

    # Bare-symbol reads aren't references in v1 — only `name(...)` call
    # sites are. Stop here for atoms.
    if (!is.call(expr)) return(invisible())

    head <- expr[[1L]]
    if (is_box_use(head)) return(invisible())

    in_class <- input_read_classify(expr)
    if (!is.null(in_class)) {
      if (in_class$kind == "static") {
        emit_input(in_class$name, namespace, in_def, own_srcref)
      } else {
        emit_dynamic_input(namespace, in_def, own_srcref)
      }
      return(invisible())
    }

    # `rv$x` / `rv[["x"]]` reads where `rv` is a known reactiveValues
    # container. For containers we haven't seen yet (read appears before
    # the `rv <- reactiveValues(...)` line — uncommon but legal), the
    # read is silently skipped; cross-file resolution lives in the
    # inventory layer.
    rv_read <- rv_read_decomp(expr)
    if (!is.null(rv_read) && rv_read$container %in% rv_containers) {
      emit_value(rv_read$name, rv_read$container, namespace, in_def, own_srcref)
      return(invisible())
    }

    # `pkg::fn(...)` / `pkg:::fn(...)` — emit a call ref, then descend
    # into args. `internal = TRUE` for `:::` access.
    pkg <- pkg_call_decomp(head)
    if (!is.null(pkg)) {
      emit_call(pkg$name, pkg$package, namespace, in_def, own_srcref,
                internal = pkg$internal)
      for (i in seq_along(expr)) {
        if (i == 1L) next
        child <- expr[[i]]
        if (is_missing_arg(child)) next
        if (is.null(child)) next
        if (is.symbol(child) && !nzchar(as.character(child))) next
        walk(child, child_srcref(expr, i) %||% pick_srcref(child) %||% own_srcref,
             namespace, in_def, NA_character_)
      }
      return(invisible())
    }

    # `alias$fn(...)` — emit a call ref tagged with the alias as
    # `container`. Resolution against `box::use(alias = pkg)` clauses
    # happens in the inventory step. We still descend into args.
    alias_decomp <- alias_call_decomp(head)
    if (!is.null(alias_decomp)) {
      emit_call(alias_decomp$name, NA_character_, namespace, in_def,
                own_srcref, container = alias_decomp$alias)
      for (i in seq_along(expr)) {
        if (i == 1L) next
        child <- expr[[i]]
        if (is_missing_arg(child)) next
        if (is.null(child)) next
        if (is.symbol(child) && !nzchar(as.character(child))) next
        walk(child, child_srcref(expr, i) %||% pick_srcref(child) %||% own_srcref,
             namespace, in_def, NA_character_)
      }
      return(invisible())
    }

    # Bare-name calls — `name(args)`. Includes named-reactive reads
    # (`total()`), function helpers, and Shiny constructors. The edge build
    # step matches `name` against the Definitions table.
    if (is.name(head) && nzchar(as.character(head))) {
      callee_name <- as.character(head)
      # Skip control-flow heads — `{`, `(`, `if`, `for`, etc. are not
      # function calls in any meaningful sense for the call surface.
      control_heads <- c("{", "(", "if", "for", "while", "repeat", "break",
                         "next", "function", "<-", "<<-", "=", "->", "->>",
                         "$", "[", "[[", "::", ":::", "@", "~", ":", "?",
                         "!", "&", "|", "&&", "||", "+", "-", "*", "/", "^",
                         "%%", "%/%", "<", ">", "<=", ">=", "==", "!=")
      if (!(callee_name %in% control_heads)) {
        if (callee_name %in% metaprogramming_callees) {
          emit_metaprogramming(callee_name, namespace, in_def, own_srcref)
        }
        emit_call(callee_name, NA_character_, namespace, in_def, own_srcref)
      }
    }

    # Module-server site — descend into the inner function body with the
    # module's namespace.
    if (is_module_server_call(head) && length(expr) >= 3L) {
      inner_fn <- expr[[3L]]
      mod_id <- module_identity(from_file, enclosing_name,
                                multi = multi_module_file)
      inner_ns <- mod_id %||% namespace
      mod_def <- list(
        kind = "module_server",
        name = inner_ns,
        namespace = inner_ns
      )
      if (is_function_def(inner_fn)) {
        body_expr <- inner_fn[[3L]]
        walk(body_expr, child_srcref(inner_fn, 3L) %||% own_srcref,
             namespace = inner_ns, in_def = mod_def,
             enclosing_name = NA_character_)
      } else {
        walk(inner_fn, child_srcref(expr, 3L) %||% own_srcref,
             namespace = inner_ns, in_def = mod_def,
             enclosing_name = NA_character_)
      }
      for (i in seq_along(expr)) {
        if (i == 1L || i == 3L) next
        child <- expr[[i]]
        if (is_missing_arg(child)) next
        if (is.null(child)) next
        if (is.symbol(child) && !nzchar(as.character(child))) next
        walk(child, child_srcref(expr, i) %||% pick_srcref(child) %||% own_srcref,
             namespace, in_def, NA_character_)
      }
      return(invisible())
    }

    if (is_assignment(head) && length(expr) == 3L) {
      lhs <- expr[[2L]]
      rhs <- expr[[3L]]

      # Outputs — set the enclosing def for the RHS body.
      out_nm <- output_name(lhs)
      if (!is.null(out_nm)) {
        new_def <- list(kind = "output", name = out_nm, namespace = namespace)
        walk(rhs, child_srcref(expr, 3L) %||% own_srcref,
             namespace, new_def, NA_character_)
        return(invisible())
      }

      bind_nm <- binding_name(lhs)
      if (!is.null(bind_nm)) {
        if (has_module_signature(rhs)) {
          # Legacy module form — body's namespace is the module identity
          # derived from the file path.
          mod_id <- module_identity(from_file, bind_nm,
                                    multi = multi_module_file) %||% bind_nm
          mod_def <- list(kind = "module_server", name = mod_id,
                          namespace = mod_id)
          body_expr <- rhs[[3L]]
          walk(body_expr, child_srcref(rhs, 3L) %||% own_srcref,
               namespace = mod_id, in_def = mod_def,
               enclosing_name = NA_character_)
          return(invisible())
        }

        # Track reactiveValues containers. Currently for downstream tasks;
        # we don't yet emit value references. Track even though unused so
        # the walker stays consistent across passes.
        if (is.call(rhs) && length(rhs) >= 1L) {
          rhs_head <- rhs[[1L]]
          rhs_callee <- if (is.name(rhs_head)) {
            as.character(rhs_head)
          } else if (is.call(rhs_head) && length(rhs_head) == 3L &&
                     as.character(rhs_head[[1L]]) %in% c("::", ":::")) {
            as.character(rhs_head[[3L]])
          } else {
            ""
          }
          if (identical(rhs_callee, "reactiveValues")) {
            rv_containers <<- union(rv_containers, bind_nm)
          }
        }

        rhs_kind <- classify_rhs(rhs)
        if (!is.null(rhs_kind)) {
          new_def <- list(kind = rhs_kind, name = bind_nm, namespace = namespace)
          walk(rhs, child_srcref(expr, 3L) %||% own_srcref,
               namespace, new_def, NA_character_)
          return(invisible())
        }

        if (is_function_def(rhs)) {
          body_expr <- rhs[[3L]]
          walk(body_expr, child_srcref(rhs, 3L) %||% own_srcref,
               namespace, in_def, bind_nm)
          return(invisible())
        }
      }

      walk(rhs, child_srcref(expr, 3L) %||% own_srcref,
           namespace, in_def, NA_character_)
      return(invisible())
    }

    is_fn_def <- is_function_def(expr)
    for (i in seq_along(expr)) {
      child <- expr[[i]]
      if (is_missing_arg(child)) next
      if (is.null(child)) next
      if (is.symbol(child) && !nzchar(as.character(child))) next
      child_enclosing <- if (is_fn_def && i >= 2L) NA_character_ else enclosing_name
      walk(child, child_srcref(expr, i) %||% pick_srcref(child) %||% own_srcref,
           namespace, in_def, child_enclosing)
    }
  }

  empty_def <- list(kind = NA_character_, name = NA_character_,
                    namespace = NA_character_)
  for (i in seq_along(parsed_expr)) {
    walk(parsed_expr[[i]],
         pick_srcref(parsed_expr[[i]]) %||% child_srcref(parsed_expr, i),
         namespace = NA_character_,
         in_def = empty_def,
         enclosing_name = NA_character_)
  }
  acc
}

#' Build the References table for an app.
#'
#' One row per static read site across every enumerated file. Columns:
#'   - `from_file`, `kind` ("input" | "call" | ...)
#'   - `name`, `namespace`, `container`, `package`
#'   - `in_def_*` — the enclosing definition that owns this read
#'   - `line`, `col`
#'
#' v1 covers `input$x` reads and bare-name + `pkg::fn` calls. Subsequent
#' tasks add `rv$x` reads.
#'
#' @noRd
build_references_table <- function(app_path) {
  files <- enumerate_app_files(app_path)

  from_files <- character()
  kinds <- character()
  names_v <- character()
  namespaces <- character()
  containers <- character()
  packages <- character()
  internals <- logical()
  in_def_kinds <- character()
  in_def_names <- character()
  in_def_namespaces <- character()
  lines <- integer()
  cols <- integer()

  for (f in files) {
    parsed <- parse_file(app_path, f)
    if (is.null(parsed)) next

    for (ref in find_references(parsed, from_file = f)) {
      from_files <- c(from_files, f)
      kinds <- c(kinds, ref$kind)
      names_v <- c(names_v, ref$name)
      namespaces <- c(namespaces, ref$namespace %||% NA_character_)
      containers <- c(containers, ref$container %||% NA_character_)
      packages <- c(packages, ref$package %||% NA_character_)
      internals <- c(internals, isTRUE(ref$internal))
      in_def_kinds <- c(in_def_kinds, ref$in_def_kind %||% NA_character_)
      in_def_names <- c(in_def_names, ref$in_def_name %||% NA_character_)
      in_def_namespaces <- c(in_def_namespaces,
                             ref$in_def_namespace %||% NA_character_)
      lines <- c(lines, as.integer(ref$line))
      cols <- c(cols, as.integer(ref$col))
    }
  }

  tibble::tibble(
    from_file = from_files,
    kind = kinds,
    name = names_v,
    namespace = namespaces,
    container = containers,
    package = packages,
    internal = internals,
    in_def_kind = in_def_kinds,
    in_def_name = in_def_names,
    in_def_namespace = in_def_namespaces,
    line = lines,
    col = cols
  )
}
