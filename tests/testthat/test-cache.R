test_that("svt_memoize computes once within a cache scope, always outside", {
  calls <- 0L
  compute <- function() {
    calls <<- calls + 1L
    "value"
  }

  # Outside any scope: computed every time.
  expect_equal(svt_memoize("k", compute), "value")
  expect_equal(svt_memoize("k", compute), "value")
  expect_equal(calls, 2L)

  # Inside a scope: computed once, second call served from cache.
  calls <- 0L
  with_svt_cache({
    expect_equal(svt_memoize("k", compute), "value")
    expect_equal(svt_memoize("k", compute), "value")
  })
  expect_equal(calls, 1L)
})

test_that("svt_memoize caches NULL results too", {
  calls <- 0L
  compute <- function() {
    calls <<- calls + 1L
    NULL
  }
  with_svt_cache({
    expect_null(svt_memoize("n", compute))
    expect_null(svt_memoize("n", compute))
  })
  expect_equal(calls, 1L)
})

test_that("the cache is torn down when the outermost scope exits", {
  with_svt_cache(svt_memoize("k", function() 1))
  # A fresh scope must not see the prior scope's entry.
  calls <- 0L
  with_svt_cache({
    svt_memoize("k", function() {
      calls <<- calls + 1L
      2
    })
  })
  expect_equal(calls, 1L)
})

test_that("svt_build_graph parses each file at most once", {
  app_path <- system.file("extdata", "traditional_with_source",
                          package = "shiny.val.tools")

  # The spy mirrors the real memoized parse_file (delegating through
  # svt_memoize) and records only the physical parses. If svt_build_graph
  # opens a cache scope and callers share it, each file is parsed once.
  physical <- character()
  testthat::local_mocked_bindings(
    parse_file = function(app_path, relpath) {
      svt_memoize(paste0("parse\x1f", app_path, "\x1f", relpath), function() {
        physical <<- c(physical, relpath)
        parse(file = file.path(app_path, relpath), keep.source = TRUE)
      })
    }
  )

  parsed <- svt_parse(app_path)
  physical <- character()  # measure only the graph build's own cache scope
  svt_build_graph(parsed)

  # No relpath is parsed more than once across the whole graph build.
  expect_equal(anyDuplicated(physical), 0L)
  expect_true(length(physical) >= 1L)
})
