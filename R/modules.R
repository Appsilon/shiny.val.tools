#' Walk a parsed expression and yield each module's returned-reactives.
#'
#' A module's contract includes the set of reactive values it returns
#' from its server function. Statically we can recognise two shapes for
#' the value position of `moduleServer(id, function(...) {body})`:
#'
#'   - `list(name1 = expr1, name2 = expr2, ...)` — a named list of
#'     reactive entries. Names with non-empty identifiers are reported.
#'   - a bare `name` symbol — the module returns a single binding by
#'     reference. Reported as `name`.
#'
#' Anonymous returns (`reactive({...})` directly), unnamed list entries,
#' and dynamic forms are not reported in v1. Auditors can refine the
#' contract in the doc stub.
#'
#' Returns a named list keyed by module name (the wrapper-function name)
#' whose values are character vectors of returned-reactive names.
#'
#' @noRd
find_module_returns <- function(parsed_expr) {
  acc <- list()

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

  is_function_def <- function(expr) {
    is.call(expr) && length(expr) >= 3L && is.name(expr[[1L]]) &&
      as.character(expr[[1L]]) == "function"
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

  # The "value position" of a moduleServer body is the body's last
  # top-level expression — for `{ a; b; c }` it is `c`; for a single
  # expression body it is the expression itself.
  value_position <- function(body_expr) {
    if (is.call(body_expr) && length(body_expr) >= 1L &&
        is.name(body_expr[[1L]]) &&
        identical(as.character(body_expr[[1L]]), "{")) {
      n <- length(body_expr)
      if (n < 2L) return(NULL)
      return(body_expr[[n]])
    }
    body_expr
  }

  classify_return <- function(val_expr) {
    if (is.null(val_expr)) return(character())
    if (is.name(val_expr)) {
      nm <- as.character(val_expr)
      if (nzchar(nm)) return(nm)
      return(character())
    }
    if (is.call(val_expr) && length(val_expr) >= 1L &&
        is.name(val_expr[[1L]]) &&
        identical(as.character(val_expr[[1L]]), "list")) {
      args <- as.list(val_expr)[-1L]
      if (!length(args)) return(character())
      argnames <- names(args)
      if (is.null(argnames)) argnames <- rep("", length(args))
      keep <- nzchar(argnames)
      return(argnames[keep])
    }
    character()
  }

  walk <- function(expr, enclosing_name) {
    if (!is.call(expr)) return(invisible())
    head <- expr[[1L]]

    if (is_module_server_call(head) && length(expr) >= 3L) {
      inner_fn <- expr[[3L]]
      mod_name <- enclosing_name %||% NA_character_
      if (!is.na(mod_name) && is_function_def(inner_fn)) {
        body_expr <- inner_fn[[3L]]
        ret <- classify_return(value_position(body_expr))
        # Last record wins on duplicate name (uncommon: a single wrapper
        # with multiple moduleServer calls).
        acc[[mod_name]] <<- ret
      }
      # Continue descending in case there are nested defs; pass NA so we
      # don't double-attribute.
      for (i in seq_along(expr)) {
        if (i == 1L) next
        child <- expr[[i]]
        if (is_missing_arg(child)) next
        if (is.null(child)) next
        if (is.symbol(child) && !nzchar(as.character(child))) next
        walk(child, NA_character_)
      }
      return(invisible())
    }

    if (is_assignment(head) && length(expr) == 3L) {
      lhs <- expr[[2L]]
      rhs <- expr[[3L]]
      bind_nm <- binding_name(lhs)
      if (!is.null(bind_nm)) {
        if (has_module_signature(rhs)) {
          # Legacy form: `name <- function(input, output, session, ...) body`
          body_expr <- rhs[[3L]]
          ret <- classify_return(value_position(body_expr))
          acc[[bind_nm]] <<- ret
          walk(body_expr, NA_character_)
          return(invisible())
        }
        if (is_function_def(rhs)) {
          body_expr <- rhs[[3L]]
          walk(body_expr, bind_nm)
          return(invisible())
        }
      }
      walk(rhs, NA_character_)
      return(invisible())
    }

    is_fn_def <- is_function_def(expr)
    for (i in seq_along(expr)) {
      child <- expr[[i]]
      if (is_missing_arg(child)) next
      if (is.null(child)) next
      if (is.symbol(child) && !nzchar(as.character(child))) next
      child_enclosing <- if (is_fn_def && i >= 2L) NA_character_ else enclosing_name
      walk(child, child_enclosing)
    }
  }

  for (i in seq_along(parsed_expr)) {
    walk(parsed_expr[[i]], NA_character_)
  }
  acc
}

#' Build the per-module returned-reactives map for an app.
#'
#' Returns a named list keyed by module name; values are character
#' vectors of returned-reactive names (possibly empty).
#'
#' @noRd
build_module_returns <- function(app_path) {
  files <- enumerate_app_files(app_path)
  acc <- list()
  for (f in files) {
    parsed <- parse_file(app_path, f)
    if (is.null(parsed)) next
    rets <- find_module_returns(parsed)
    for (nm in names(rets)) acc[[nm]] <- rets[[nm]]
  }
  acc
}

#' Auto-extract a module's contract from the graph.
#'
#' A module subgraph's contract — its inputs, outputs, and returned
#' reactives — is auto-extracted from the graph and shown in the doc
#' stub for the developer to confirm.
#'
#' For module `mod_name`:
#'   - inputs   — input nodes whose namespace == mod_name
#'   - outputs  — output nodes whose namespace == mod_name
#'   - returned — returned-reactive names from `build_module_returns()`
#'
#' Returns a list with those three character-vector fields.
#'
#' @noRd
module_contract <- function(mod_name, nodes, returns_map) {
  ns_match <- !is.na(nodes$namespace) & nodes$namespace == mod_name
  inputs  <- sort(unique(nodes$name[ns_match & nodes$type == "input"]))
  outputs <- sort(unique(nodes$name[ns_match & nodes$type == "output"]))
  returned <- as.character(returns_map[[mod_name]] %||% character())
  list(inputs = inputs, outputs = outputs, returned = returned)
}

#' Slice the graph into one subgraph per Shiny module definition.
#'
#' Each module definition produces a module subgraph in addition to any
#' feature subgraphs. Roots are:
#'   - the module's outputs (and any observers it owns)
#'   - the module's returned reactives (resolved against named reactive
#'     bindings in the module's namespace; unresolved names are reported
#'     in the contract but do not become roots)
#'
#' The transitive upstream closure walks edges within the module's own
#' nodes — modules are validated standalone, so the closure is naturally
#' bounded by the namespace's edge surface.
#'
#' @noRd
module_slice <- function(graph, app_path) {
  defs <- build_definitions_table(app_path)
  if (!nrow(defs)) return(list())

  mod_rows <- defs[defs$kind == "module_server" & !is.na(defs$name), , drop = FALSE]
  if (!nrow(mod_rows)) return(list())
  mod_names <- unique(mod_rows$name)

  returns_map <- build_module_returns(app_path)
  modules <- vector("list", length(mod_names))

  for (i in seq_along(mod_names)) {
    mn <- mod_names[i]
    contract <- module_contract(mn, graph$nodes, returns_map)

    ns_nodes <- graph$nodes[!is.na(graph$nodes$namespace) &
                              graph$nodes$namespace == mn, , drop = FALSE]
    root_ids <- ns_nodes$id[ns_nodes$type %in% c("output", "observer")]
    # A returned reactive name is a root when it resolves to a node
    # (a named reactive in the module's namespace).
    if (length(contract$returned)) {
      ret_ids <- ns_nodes$id[ns_nodes$type == "reactive" &
                               ns_nodes$name %in% contract$returned]
      root_ids <- unique(c(root_ids, ret_ids))
    }

    closure <- character()
    for (r in root_ids) {
      closure <- unique(c(closure, upstream_closure(r, graph$nodes, graph$edges)))
    }
    edge_idx <- if (nrow(graph$edges)) {
      which(graph$edges$source_id %in% closure & graph$edges$target_id %in% closure)
    } else {
      integer()
    }

    rec <- new_feature_record(
      name = mn,
      kind = "module",
      roots = root_ids,
      node_ids = closure,
      edge_ids = edge_idx
    )
    rec$contract <- contract
    modules[[i]] <- rec
  }

  modules
}
