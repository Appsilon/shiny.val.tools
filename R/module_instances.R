#' Walk a parsed expression and yield every module-instantiation call site.
#'
#' A *module instantiation* is a parent-side call to a Shiny module's
#' wrapper function with a concrete namespace id — `counter_server("c1")`,
#' `counter_server("c2")`. Each call site becomes its own
#' `module_instance` node, with the wrapper's enclosing namespace as
#' the node's namespace and the literal id as the instance's effective
#' namespace.
#'
#' The wrapper set must be supplied — typically the names of every
#' `kind = "module_server"` row in the Definitions table. Calls whose id
#' is not a string literal (`counter_server(input$which)`,
#' `counter_server(paste0(...))`) are skipped: we cannot statically
#' resolve them, and emitting a phantom node would distort the graph.
#'
#' Returns a list of records: `list(wrapper, ns_id, namespace, line, col)`.
#' `namespace` is the parent's namespace at the call site — `NA` for
#' top-level (server.R), or the outer module's name when the wrapper is
#' invoked inside another `moduleServer` body.
#'
#' @noRd
find_module_instances <- function(parsed_expr, wrapper_names,
                                  from_file = NA_character_,
                                  alias_to_module = character()) {
  acc <- list()
  if (!length(wrapper_names)) {
    # Empty wrapper set still walks — we want a stable shape regardless.
    wrapper_names <- character()
  }
  if (is.null(alias_to_module)) alias_to_module <- character()
  multi_module_file <- count_module_server_calls(parsed_expr) > 1L

  # Detect `<alias>$server(...)` or `<alias>$ui(...)` where alias is in
  # our alias-to-module map. Returns the resolved module identity, or
  # NULL when the call shape does not match. Both shapes count as
  # instantiation; the architecture-edge layer dedupes duplicates.
  resolve_dollar_call <- function(head) {
    if (!is.call(head) || length(head) != 3L) return(NULL)
    op <- head[[1L]]
    if (!is.name(op) || !identical(as.character(op), "$")) return(NULL)
    lhs <- head[[2L]]
    rhs <- head[[3L]]
    if (!is.name(lhs) || !is.name(rhs)) return(NULL)
    fn <- as.character(rhs)
    if (!fn %in% c("server", "ui")) return(NULL)
    alias <- as.character(lhs)
    target <- alias_to_module[[alias]]
    if (is.null(target) || !nzchar(target)) return(NULL)
    list(target = target, alias = alias, exported = fn)
  }

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

  is_assignment <- function(head) {
    is.name(head) && as.character(head) %in% c("<-", "=", "<<-")
  }

  binding_name <- function(lhs) {
    if (is.name(lhs)) {
      nm <- as.character(lhs)
      if (nzchar(nm)) return(nm)
    }
    NULL
  }

  # First positional (or `id =`) argument as a string literal. Returns
  # NULL when absent, dynamic, or NA.
  literal_id <- function(call_expr) {
    args <- as.list(call_expr)[-1L]
    if (!length(args)) return(NULL)
    argnames <- names(args)
    if (is.null(argnames)) argnames <- rep("", length(args))
    idx <- which(argnames == "id")
    if (!length(idx)) idx <- which(argnames == "")
    if (!length(idx)) return(NULL)
    val <- args[[idx[1L]]]
    if (is.character(val) && length(val) == 1L && !is.na(val) && nzchar(val)) {
      return(val)
    }
    NULL
  }

  walk <- function(expr, own_srcref, namespace) {
    if (!tryCatch({ force(expr); TRUE }, error = function(e) FALSE)) {
      return(invisible())
    }
    if (!is.call(expr)) return(invisible())

    head <- expr[[1L]]
    if (is_box_use(head)) return(invisible())

    # Wrapper-call detection: a bare-name call whose name is in our wrapper
    # set, with a string-literal id as the first arg.
    if (is.name(head)) {
      callee_name <- as.character(head)
      if (callee_name %in% wrapper_names) {
        id <- literal_id(expr)
        if (!is.null(id)) {
          loc <- loc_from(own_srcref)
          acc[[length(acc) + 1L]] <<- list(
            wrapper = callee_name,
            target_id = NA_character_,
            exported = NA_character_,
            ns_id = id,
            namespace = namespace,
            line = loc$line,
            col = loc$col
          )
        }
      }
    }

    # Box-aliased call detection: `<alias>$server(...)` or `<alias>$ui(...)`
    # where `<alias>` is bound by a `box::use(...)` clause whose target
    # resolves to a known module identity.
    dollar <- resolve_dollar_call(head)
    if (!is.null(dollar)) {
      id <- literal_id(expr)
      if (!is.null(id)) {
        loc <- loc_from(own_srcref)
        acc[[length(acc) + 1L]] <<- list(
          wrapper = dollar$alias,
          target_id = dollar$target,
          exported = dollar$exported,
          ns_id = id,
          namespace = namespace,
          line = loc$line,
          col = loc$col
        )
      }
    }

    # moduleServer body: descend with the module identity as the current
    # namespace. The wrapper binding (the local function name) is set by
    # the assignment branch below via `attr(expr, "svt_wrapper_name")`;
    # the identity itself is file-path-derived.
    if (is_module_server_call(head) && length(expr) >= 3L) {
      inner_fn <- expr[[3L]]
      wrapper_bind <- attr(expr, "svt_wrapper_name")
      inner_ns <- module_identity(from_file, wrapper_bind,
                                  multi = multi_module_file) %||% namespace
      if (is_function_def(inner_fn)) {
        body_expr <- inner_fn[[3L]]
        walk(body_expr, child_srcref(inner_fn, 3L) %||% own_srcref, inner_ns)
      } else {
        walk(inner_fn, child_srcref(expr, 3L) %||% own_srcref, inner_ns)
      }
      for (i in seq_along(expr)) {
        if (i == 1L || i == 3L) next
        child <- expr[[i]]
        if (is_missing_arg(child)) next
        if (is.null(child)) next
        if (is.symbol(child) && !nzchar(as.character(child))) next
        walk(child, child_srcref(expr, i) %||% pick_srcref(child) %||% own_srcref,
             namespace)
      }
      return(invisible())
    }

    if (is_assignment(head) && length(expr) == 3L) {
      lhs <- expr[[2L]]
      rhs <- expr[[3L]]
      bind_nm <- binding_name(lhs)

      if (!is.null(bind_nm) && has_module_signature(rhs)) {
        # Legacy form: `name <- function(input, output, session, ...) body`
        # → body's namespace is the module identity (file-path-derived).
        body_expr <- rhs[[3L]]
        mod_id <- module_identity(from_file, bind_nm,
                                  multi = multi_module_file) %||% bind_nm
        walk(body_expr, child_srcref(rhs, 3L) %||% own_srcref, mod_id)
        return(invisible())
      }

      if (!is.null(bind_nm) && is_function_def(rhs)) {
        # Modern form: walk the body, but tag any moduleServer calls inside
        # with this wrapper name so the body's namespace is set correctly.
        body_expr <- rhs[[3L]]
        walk_with_wrapper(body_expr,
                          child_srcref(rhs, 3L) %||% own_srcref,
                          namespace, bind_nm)
        return(invisible())
      }

      walk(rhs, child_srcref(expr, 3L) %||% own_srcref, namespace)
      return(invisible())
    }

    for (i in seq_along(expr)) {
      child <- expr[[i]]
      if (is_missing_arg(child)) next
      if (is.null(child)) next
      if (is.symbol(child) && !nzchar(as.character(child))) next
      walk(child, child_srcref(expr, i) %||% pick_srcref(child) %||% own_srcref,
           namespace)
    }
  }

  # Variant that knows the enclosing wrapper name. When it encounters
  # `moduleServer(id, fn)`, the body's namespace becomes `wrapper_name`.
  walk_with_wrapper <- function(expr, own_srcref, namespace, wrapper_name) {
    if (!tryCatch({ force(expr); TRUE }, error = function(e) FALSE)) {
      return(invisible())
    }
    if (!is.call(expr)) return(invisible())

    head <- expr[[1L]]
    if (is_box_use(head)) return(invisible())

    if (is_module_server_call(head) && length(expr) >= 3L) {
      inner_fn <- expr[[3L]]
      mod_id <- module_identity(from_file, wrapper_name,
                                multi = multi_module_file) %||% wrapper_name
      if (is_function_def(inner_fn)) {
        body_expr <- inner_fn[[3L]]
        walk(body_expr, child_srcref(inner_fn, 3L) %||% own_srcref, mod_id)
      } else {
        walk(inner_fn, child_srcref(expr, 3L) %||% own_srcref, mod_id)
      }
      for (i in seq_along(expr)) {
        if (i == 1L || i == 3L) next
        child <- expr[[i]]
        if (is_missing_arg(child)) next
        if (is.null(child)) next
        if (is.symbol(child) && !nzchar(as.character(child))) next
        walk_with_wrapper(child,
                          child_srcref(expr, i) %||% pick_srcref(child) %||% own_srcref,
                          namespace, wrapper_name)
      }
      return(invisible())
    }

    # Inside the wrapper body but outside any moduleServer — still walk
    # in the parent namespace (rare, but possible: a wrapper that
    # instantiates other modules before calling moduleServer).
    if (is.name(head) && as.character(head) %in% wrapper_names) {
      id <- literal_id(expr)
      if (!is.null(id)) {
        loc <- loc_from(own_srcref)
        acc[[length(acc) + 1L]] <<- list(
          wrapper = as.character(head),
          target_id = NA_character_,
          exported = NA_character_,
          ns_id = id,
          namespace = namespace,
          line = loc$line,
          col = loc$col
        )
      }
    }

    dollar <- resolve_dollar_call(head)
    if (!is.null(dollar)) {
      id <- literal_id(expr)
      if (!is.null(id)) {
        loc <- loc_from(own_srcref)
        acc[[length(acc) + 1L]] <<- list(
          wrapper = dollar$alias,
          target_id = dollar$target,
          exported = dollar$exported,
          ns_id = id,
          namespace = namespace,
          line = loc$line,
          col = loc$col
        )
      }
    }

    for (i in seq_along(expr)) {
      child <- expr[[i]]
      if (is_missing_arg(child)) next
      if (is.null(child)) next
      if (is.symbol(child) && !nzchar(as.character(child))) next
      walk_with_wrapper(child,
                        child_srcref(expr, i) %||% pick_srcref(child) %||% own_srcref,
                        namespace, wrapper_name)
    }
  }

  for (i in seq_along(parsed_expr)) {
    walk(parsed_expr[[i]],
         pick_srcref(parsed_expr[[i]]) %||% child_srcref(parsed_expr, i),
         namespace = NA_character_)
  }
  acc
}

#' Append `module_instance` nodes to the Nodes table.
#'
#' Called by `build_nodes_table()`. Returns a tibble with the same column
#' shape as the main nodes tibble; rows are concatenated by the caller.
#'
#' @noRd
build_module_instance_nodes <- function(app_path, definitions,
                                        imports = NULL) {
  empty <- tibble::tibble(
    id = character(), type = character(), name = character(),
    namespace = character(), container = character(), fq_name = character(),
    file = character(), line = integer(), col = integer(),
    warnings = list()
  )

  if (!nrow(definitions)) return(empty)
  wrapper_rows <- definitions[definitions$kind == "module_server" &
                                !is.na(definitions$name), , drop = FALSE]

  # Two ways a parent-side call can resolve to a module:
  #   1. Bare-name calls (`counter_server("c1")`): the wrapper binding
  #      (function name in the source) maps to a module identity via the
  #      definitions table.
  #   2. Box-aliased calls (`mod_a$server(id = "a")` / `mod_a$ui(...)`):
  #      the alias binding maps to a module identity via the imports
  #      table's `local_path` column.
  # The bare-name set may be empty in pure rhino apps; the box-alias set
  # may be empty in pure traditional apps. We need at least one to do
  # any work.
  wrapper_to_id <- as.character(wrapper_rows$name)
  names(wrapper_to_id) <- as.character(wrapper_rows$wrapper_binding)
  wrapper_names <- unique(as.character(wrapper_rows$wrapper_binding))
  wrapper_names <- wrapper_names[!is.na(wrapper_names) & nzchar(wrapper_names)]

  module_identities <- unique(as.character(wrapper_rows$name))
  module_identities <- module_identities[!is.na(module_identities)]

  if (is.null(imports)) imports <- build_imports_table(app_path)

  files <- enumerate_app_files(app_path)
  records <- list()

  for (f in files) {
    parsed <- parse_file(app_path, f)
    if (is.null(parsed)) next
    aliases <- file_module_aliases(imports, f, module_identities)
    if (!length(wrapper_names) && !length(aliases)) next
    for (rec in find_module_instances(parsed, wrapper_names,
                                       from_file = f,
                                       alias_to_module = aliases)) {
      rec$from_file <- f
      records[[length(records) + 1L]] <- rec
    }
  }

  if (!length(records)) return(empty)

  ids <- character(length(records))
  types <- rep("module_instance", length(records))
  names_v <- character(length(records))
  namespaces <- character(length(records))
  containers <- character(length(records))
  fq_names <- character(length(records))
  files_v <- character(length(records))
  lines <- integer(length(records))
  cols <- integer(length(records))
  warning_lists <- vector("list", length(records))

  for (i in seq_along(records)) {
    r <- records[[i]]
    target_id <- if (!is.null(r$target_id) && !is.na(r$target_id) &&
                     nzchar(r$target_id)) {
      r$target_id
    } else {
      wrapper_to_id[[r$wrapper]] %||% r$wrapper
    }
    ids[i] <- node_id("module_instance", r$namespace, r$ns_id, target_id)
    names_v[i] <- target_id
    namespaces[i] <- r$namespace %||% NA_character_
    containers[i] <- r$ns_id
    ns_prefix <- if (!is.na(r$namespace) && nzchar(r$namespace)) {
      paste0(r$namespace, "/")
    } else {
      ""
    }
    callee <- if (!is.null(r$exported) && !is.na(r$exported) &&
                  nzchar(r$exported)) {
      paste0(r$wrapper, "$", r$exported)
    } else {
      r$wrapper
    }
    fq_names[i] <- paste0(ns_prefix, callee, "[", r$ns_id, "]")
    files_v[i] <- r$from_file
    lines[i] <- as.integer(r$line)
    cols[i] <- as.integer(r$col)
    warning_lists[[i]] <- character()
  }

  tibble::tibble(
    id = ids,
    type = types,
    name = names_v,
    namespace = namespaces,
    container = containers,
    fq_name = fq_names,
    file = files_v,
    line = lines,
    col = cols,
    warnings = warning_lists
  )
}

#' Map per-file box-import aliases to the module identity they bind.
#'
#' Returns a named character vector whose names are the alias bindings
#' (the symbol the call site looks like — `mod_a` for `box::use(app/view/mod_a)`
#' or `box::use(mod_a = app/view/mod_a)`) and whose values are the module
#' identity (the `local_path`, which is already file-path-derived).
#'
#' Only clauses whose `local_path` matches a known module identity in
#' `module_identities` are returned, so we never invent module-instance
#' nodes pointing at imports that aren't actually modules (e.g. plain
#' utility scripts loaded via `box::use(app/logic/util)`).
#'
#' @noRd
file_module_aliases <- function(imports, from_file, module_identities) {
  if (is.null(imports) || !nrow(imports)) return(character())
  rows <- imports[imports$from_file == from_file &
                    imports$kind == "box_use" &
                    !is.na(imports$local_path), , drop = FALSE]
  if (!nrow(rows)) return(character())

  bindings <- character(nrow(rows))
  ids <- character(nrow(rows))
  for (i in seq_len(nrow(rows))) {
    a <- rows$alias[i]
    if (!is.na(a) && nzchar(a)) {
      bindings[i] <- a
    } else {
      parts <- strsplit(rows$local_path[i], "/", fixed = TRUE)[[1L]]
      bindings[i] <- parts[length(parts)]
    }
    ids[i] <- rows$local_path[i]
  }
  keep <- ids %in% module_identities
  ids <- ids[keep]
  bindings <- bindings[keep]
  if (!length(ids)) return(character())
  out <- ids
  names(out) <- bindings
  out[!duplicated(names(out))]
}
