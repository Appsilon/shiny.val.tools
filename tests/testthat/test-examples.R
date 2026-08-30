test_that("svt_run_example rejects names outside the predefined set", {
  expect_error(svt_run_example("no_such_app"), "should be one of")
  expect_error(svt_run_example("cycle_app"), "should be one of")
})

test_that("svt_run_example choices are exactly the runnable fixture apps", {
  choices <- eval(formals(svt_run_example)$app)

  installed <- list.dirs(system.file("extdata", package = "shiny.val.tools"),
                         full.names = FALSE, recursive = FALSE)
  expect_true(all(choices %in% installed))

  # cycle_app (infinite source() recursion) and box_in_R (no entrypoint) are
  # analysis-only fixtures — offering them would hand the user an app that
  # cannot start.
  expect_false(any(c("cycle_app", "box_in_R") %in% choices))
})

test_that("materialize_example restores the .Rprofile dotfile", {
  dir <- materialize_example("rhino_basic")

  expect_true(file.exists(file.path(dir, ".Rprofile")))
  expect_false(file.exists(file.path(dir, "_Rprofile")))
  expect_true(file.exists(file.path(dir, "app", "main.R")))
})

test_that("materialize_example copies to a fresh directory each call", {
  a <- materialize_example("traditional_basic")
  b <- materialize_example("traditional_basic")

  expect_false(identical(a, b))
  writeLines("x <- 1", file.path(a, "scratch.R"))
  expect_false(file.exists(file.path(b, "scratch.R")))
})

test_that("every offered example app actually starts", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("rhino")

  for (app in eval(formals(svt_run_example)$app)) {
    later::later(function() shiny::stopApp("started"), 2)
    expect_equal(
      svt_run_example(app, launch.browser = FALSE, quiet = TRUE),
      "started",
      info = app
    )
  }
})

# Run an example app's code with the app dir materialized, its `.Rprofile`
# applied and the working directory set, the way `runApp()` would.
with_example_app <- function(app, fn) {
  dir <- materialize_example(app)
  apply_app_profile(dir)
  old_wd <- setwd(dir)
  on.exit(setwd(old_wd), add = TRUE)
  fn(dir)
}

# Load a rhino app's top-level `app/main` module the way `rhino::app()` does.
load_rhino_main <- function() {
  env <- new.env()
  eval(quote(box::use(app/main)), env)
  env$main
}

test_that("traditional_basic renders output through its nested R/utils helper", {
  skip_if_not_installed("shiny")

  with_example_app("traditional_basic", function(dir) {
    shiny::testServer(dir, {
      session$setInputs(x = 3)
      # format_label() lives in R/utils/format.R, which Shiny's autoload does
      # not reach — global.R must source it explicitly.
      expect_equal(output$doubled, "Result: 6")
    })
  })
})

test_that("traditional_with_source renders output through its source() chain", {
  skip_if_not_installed("shiny")

  with_example_app("traditional_with_source", function(dir) {
    shiny::testServer(dir, {
      session$setInputs(n = 5)
      # foo_helper() -> bar_helper(), reached via global.R -> foo.R -> bar.R
      expect_equal(output$out, "foo_helper: 5")
    })
  })
})

test_that("rhino_basic renders its module UI and server", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("rhino")
  skip_if_not_installed("box")

  with_example_app("rhino_basic", function(dir) {
    main <- load_rhino_main()

    expect_silent(htmltools::renderTags(main$ui("app")))
    shiny::testServer(main$server, args = list(id = "app"), {
      expect_false(is.null(output$message))
    })
  })
})

test_that("rhino_multi_module wires mod_a's selection through to mod_b", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("rhino")
  skip_if_not_installed("box")

  with_example_app("rhino_multi_module", function(dir) {
    main <- load_rhino_main()

    expect_silent(htmltools::renderTags(main$ui("app")))
    shiny::testServer(main$server, args = list(id = "app"), {
      session$setInputs(`a-which` = "y")
      expect_equal(output$`b-echo`, "you picked: y")
    })
  })
})
