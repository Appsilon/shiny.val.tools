#' Derive the canonical module identity for a `moduleServer()` call site.
#'
#' Identity is the relative file path with its extension stripped — e.g.
#' `app/view/mod_card.R` → `app/view/mod_card`, `server.R` → `server`.
#' When a single file contains more than one `moduleServer()` call, the
#' identities are disambiguated as `<path>::<binding>` where `<binding>`
#' is the name of the enclosing function the moduleServer call sits in.
#' A NA / empty `from_file` falls back to the binding name (legacy code
#' paths that have not yet plumbed file context through).
#'
#' @noRd
module_identity <- function(from_file, binding_name = NA_character_,
                            multi = FALSE) {
  if (is.null(from_file) || is.na(from_file) || !nzchar(from_file)) {
    if (is.null(binding_name) || is.na(binding_name) || !nzchar(binding_name)) {
      return(NA_character_)
    }
    return(binding_name)
  }
  base <- sub("\\.[^./]*$", "", from_file)
  if (isTRUE(multi) &&
      !is.null(binding_name) && !is.na(binding_name) && nzchar(binding_name)) {
    return(paste0(base, "::", binding_name))
  }
  base
}

#' Count `moduleServer()` calls in a parsed expression.
#'
#' Used by the per-file walkers to decide whether module identity needs
#' the binding-name suffix to disambiguate multiple modules in one file.
#'
#' @noRd
count_module_server_calls <- function(parsed_expr) {
  count <- 0L
  walk <- function(expr) {
    if (!is.call(expr)) return(invisible())
    head <- expr[[1L]]
    if (is_box_use(head)) return(invisible())
    if (is_module_server_call(head)) count <<- count + 1L
    if (is.name(head) && as.character(head) %in% c("<-", "=", "<<-") &&
        length(expr) == 3L && has_module_signature(expr[[3L]])) {
      count <<- count + 1L
    }
    for (i in seq_along(expr)) {
      child <- expr[[i]]
      if (!walkable(child)) next
      walk(child)
    }
  }
  for (i in seq_along(parsed_expr)) walk(parsed_expr[[i]])
  count
}

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
#' Returns a named list keyed by module identity (file-path-derived; see
#' `module_identity()`) whose values are character vectors of returned-
#' reactive names.
#'
#' @noRd
find_module_returns <- function(parsed_expr, from_file = NA_character_) {
  acc <- list()
  multi <- count_module_server_calls(parsed_expr) > 1L

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
      mod_name <- module_identity(from_file, enclosing_name, multi = multi)
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
        if (!walkable(child)) next
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
          mod_name <- module_identity(from_file, bind_nm, multi = multi)
          if (!is.na(mod_name)) acc[[mod_name]] <<- ret
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
      if (!walkable(child)) next
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
#' Memoized under the run-scoped cache like every other builder (spec 05,
#' "Run-scoped memoization") — it re-parses and re-walks every file in the
#' app, and `module_slice()` calls it once per slice.
#'
#' @noRd
build_module_returns <- function(app_path) {
 svt_memoize(paste0("module_returns\x1f", app_path), function() {
  files <- enumerate_app_files(app_path)
  acc <- list()
  for (f in files) {
    parsed <- parse_file(app_path, f)
    if (is.null(parsed)) next
    rets <- find_module_returns(parsed, from_file = f)
    for (nm in names(rets)) acc[[nm]] <- rets[[nm]]
  }
  acc
 })
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

  # A package target's deliverable is its exported modules. Internal modules
  # are implementation detail: they still appear as child instances inside the
  # exported modules that use them, but they get no record of their own.
  exported <- exported_modules(app_path)
  if (nrow(exported)) {
    mod_names <- exported$module
    record_names <- exported$server_fn
  } else {
    record_names <- mod_names
  }

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
    # Module_instance nodes within the namespace are part of the module's
    # surface area regardless of whether anything in the closure consumes
    # them. A module whose body is purely instantiation (`mod_a$server();
    # mod_b$server()`) has no outputs/returns to root from, so the
    # closure walk would otherwise drop these. They drive parent->child
    # architecture edges and should appear in the per-module widget too.
    inst_ids <- ns_nodes$id[ns_nodes$type == "module_instance"]
    if (length(inst_ids)) {
      closure <- unique(c(closure, inst_ids))
    }
    edge_idx <- if (nrow(graph$edges)) {
      which(graph$edges$source_id %in% closure & graph$edges$target_id %in% closure)
    } else {
      integer()
    }

    rec <- new_feature_record(
      name = record_names[i],
      kind = "module",
      roots = root_ids,
      node_ids = closure,
      edge_ids = edge_idx
    )
    rec$contract <- contract
    if (nrow(exported)) {
      # A package-target record is *named* by the exported server function,
      # because that is what consumers call. `module` keeps the graph's
      # file-derived identity so callers can still join the record back to
      # its node namespaces, which are keyed the other way (spec 02,
      # "Naming and UI pairing").
      rec$module <- mn
      rec$server_fn <- exported$server_fn[i]
      rec$ui_fn <- exported$ui_fn[i]
    }
    modules[[i]] <- rec
  }

  modules
}
