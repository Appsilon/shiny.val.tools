test_that("find_source_calls finds top-level and conditional source() calls", {
  parsed <- parse(text = "
    library(shiny)
    source('helpers/foo.R')
    if (debug) {
      source('helpers/dev.R')
    }
  ", keep.source = TRUE)

  calls <- find_source_calls(parsed)

  expect_length(calls, 2)
  expect_equal(calls[[1]]$path, "helpers/foo.R")
  expect_false(calls[[1]]$conditional)
  expect_equal(calls[[2]]$path, "helpers/dev.R")
  expect_true(calls[[2]]$conditional)
})

test_that("find_source_calls handles base::source and named file argument", {
  parsed <- parse(text = "
    base::source('a.R')
    source(file = 'b.R')
  ", keep.source = TRUE)

  calls <- find_source_calls(parsed)

  expect_length(calls, 2)
  expect_setequal(vapply(calls, function(c) c$path, character(1)), c("a.R", "b.R"))
})

test_that("find_source_calls treats for/while/tryCatch bodies as conditional", {
  parsed <- parse(text = "
    for (f in files) source(f_file)
    while (cond) source('w.R')
    tryCatch(source('t.R'), error = function(e) NULL)
  ", keep.source = TRUE)

  calls <- find_source_calls(parsed)

  conditionals <- vapply(calls, function(c) c$conditional, logical(1))
  expect_true(all(conditionals))
})

test_that("find_source_calls skips dynamic paths (non-string-literal)", {
  parsed <- parse(text = "
    source(my_path)
    source(paste0('a', '.R'))
  ", keep.source = TRUE)

  calls <- find_source_calls(parsed)

  expect_length(calls, 0)
})

test_that("build_sources_table is a tibble with one row per source() call", {
  app_path <- system.file("extdata", "traditional_with_source",
                          package = "shiny.val.tools")

  sources <- build_sources_table(app_path)

  expect_s3_class(sources, "tbl_df")
  expect_named(sources,
               c("from_file", "path", "to_file", "chdir", "conditional", "line", "col"),
               ignore.order = TRUE)

  # 3 expected source() calls:
  #   app.R -> helpers/foo.R                  (unconditional)
  #   app.R -> helpers/dev.R                  (inside if-block)
  #   helpers/foo.R -> helpers/bar.R          (resolved relative to helpers/)
  expect_equal(nrow(sources), 3)
})

test_that("build_sources_table resolves to_file relative to the calling file", {
  app_path <- system.file("extdata", "traditional_with_source",
                          package = "shiny.val.tools")

  sources <- build_sources_table(app_path)
  # the row from helpers/foo.R: source('bar.R') must resolve to helpers/bar.R
  foo_row <- sources[sources$from_file == "helpers/foo.R", ]

  expect_equal(nrow(foo_row), 1)
  expect_equal(foo_row$path, "bar.R")
  expect_equal(foo_row$to_file, "helpers/bar.R")
})

test_that("find_source_calls captures the chdir argument", {
  parsed <- parse(text = "
    source('a.R')
    source('b.R', chdir = TRUE)
    source('c.R', chdir = FALSE)
    source(file = 'd.R', chdir = TRUE)
  ", keep.source = TRUE)

  calls <- find_source_calls(parsed)

  chdir_flags <- vapply(calls, function(c) c$chdir, logical(1))
  paths <- vapply(calls, function(c) c$path, character(1))
  expect_equal(paths, c("a.R", "b.R", "c.R", "d.R"))
  expect_equal(chdir_flags, c(FALSE, TRUE, FALSE, TRUE))
})

test_that("enumerate_app_files resolves nested source() relative to app root when chdir=FALSE", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(file.path(tmp, "helpers"))
  writeLines(c(
    "library(shiny)",
    "source('helpers/inner.R')",
    "shinyApp(fluidPage(), function(input, output, session) {})"
  ), file.path(tmp, "app.R"))
  # Inner sources `mod.R` with no chdir — R semantics resolve from cwd
  # (app root), so target is mod.R at app root, NOT helpers/mod.R.
  writeLines("source('mod.R')", file.path(tmp, "helpers", "inner.R"))
  writeLines("# top-level helper", file.path(tmp, "mod.R"))

  files <- enumerate_app_files(tmp)
  expect_true("mod.R" %in% files)
  expect_false("helpers/mod.R" %in% files)
})

test_that("enumerate_app_files resolves nested source() relative to file dir when chdir=TRUE", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(file.path(tmp, "helpers"))
  writeLines(c(
    "library(shiny)",
    "source('helpers/inner.R', chdir = TRUE)",
    "shinyApp(fluidPage(), function(input, output, session) {})"
  ), file.path(tmp, "app.R"))
  # Inner sources `mod.R` with chdir = TRUE on its caller — but we are
  # only looking at this `source()` call in isolation. Inside inner.R,
  # `source("mod.R", chdir = TRUE)` should resolve to helpers/mod.R.
  writeLines("source('mod.R', chdir = TRUE)", file.path(tmp, "helpers", "inner.R"))
  writeLines("# nested helper", file.path(tmp, "helpers", "mod.R"))

  files <- enumerate_app_files(tmp)
  expect_true("helpers/mod.R" %in% files)
  expect_false("mod.R" %in% files)
})

test_that("build_sources_table flags the conditional source() correctly", {
  app_path <- system.file("extdata", "traditional_with_source",
                          package = "shiny.val.tools")

  sources <- build_sources_table(app_path)

  cond <- sources[sources$to_file == "helpers/dev.R", ]
  expect_equal(nrow(cond), 1)
  expect_true(cond$conditional)

  uncond <- sources[sources$to_file == "helpers/foo.R", ]
  expect_equal(nrow(uncond), 1)
  expect_false(uncond$conditional)
})
