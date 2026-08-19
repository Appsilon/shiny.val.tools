# Run-scoped memoization
# -----------------------
#
# Many builders re-derive the same intermediate results from `app_path`.
# The references table, for instance, is needed by the nodes, edges and
# warnings builders and again by the inventory layer; each derivation
# re-enumerates and re-parses every file. Left uncached, a single
# `svt_validate()` re-parses the app on the order of twenty times, which is
# the dominant cost on any non-trivial app.
#
# `with_svt_cache()` installs a cache for the duration of one top-level
# call; `svt_memoize()` consults it. Outside any scope `svt_memoize()`
# computes directly, so the builders stay pure and independently testable
# (existing tests call them without a scope and see no behavioural change).
#
# Correctness: a run never mutates the app under analysis, so a memoized
# result is byte-identical to a fresh computation — the determinism
# contract in spec 05 is preserved. Keys embed `app_path`, so distinct
# apps within one scope never collide.

svt_cache_env <- new.env(parent = emptyenv())
svt_cache_env$depth <- 0L
svt_cache_env$store <- NULL

#' Evaluate `expr` with a memoization cache active.
#'
#' Re-entrant: nested calls share the outermost cache and only the
#' outermost scope allocates and tears down the store. The store is
#' cleared on exit (including on error) so nothing leaks between runs.
#'
#' @noRd
with_svt_cache <- function(expr) {
  svt_cache_env$depth <- svt_cache_env$depth + 1L
  if (svt_cache_env$depth == 1L) {
    svt_cache_env$store <- new.env(parent = emptyenv())
  }
  on.exit({
    svt_cache_env$depth <- svt_cache_env$depth - 1L
    if (svt_cache_env$depth == 0L) svt_cache_env$store <- NULL
  })
  force(expr)
}

#' Return `compute()`'s value, memoized under `key` for the active scope.
#'
#' With no active scope, computes every time. `NULL` results are cached
#' (checked by key existence, not value), so parse failures aren't retried.
#'
#' @noRd
svt_memoize <- function(key, compute) {
  store <- svt_cache_env$store
  if (is.null(store)) return(compute())
  if (exists(key, envir = store, inherits = FALSE)) {
    return(get(key, envir = store, inherits = FALSE))
  }
  val <- compute()
  assign(key, val, envir = store)
  val
}
