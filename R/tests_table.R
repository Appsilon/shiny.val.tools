#' Enumerate the R files under an app's test tree.
#'
#' The test tree is enumerated separately from the app (spec 06): test
#' files must never contribute a node, an edge, or an inventory row, so
#' they never reach `enumerate_app_files()`. Only `*.R` is read — there
#' is no JS scan, and a Cypress spec links through the manifest instead.
#'
#' Paths are returned relative to `test_path`, sorted.
#'
#' @noRd
enumerate_test_files <- function(test_path) {
  if (is.null(test_path) || !dir.exists(test_path)) return(character())
  files <- list.files(test_path, pattern = "\\.[Rr]$", recursive = TRUE)
  sort(gsub("\\\\", "/", files))
}

#' Is this file a support file rather than a test file?
#'
#' `setup-*.R` and `helper-*.R` are testthat's own conventions; anything
#' outside a `testthat/` directory (`tests/testthat.R` itself, for
#' instance) is runner plumbing. Support files are recorded so the table
#' describes the whole tree, and excluded from coverage reasoning.
#'
#' @noRd
is_support_file <- function(rel) {
  base <- basename(rel)
  grepl("^(setup|helper)", base) || !grepl("(^|/)testthat/", rel)
}

#' The `@covers` targets declared in a run of comment lines.
#'
#' `# @covers feature: km_plot, km_summary` yields
#' `c("feature:km_plot", "feature:km_summary")`. A comment rather than a
#' function call: it works in any framework, adds no runtime dependency
#' on this package, and is invisible to CI.
#'
#' @noRd
parse_covers <- function(lines) {
  out <- character()
  # `#'` (roxygen) as well as `#`: valtools and srr both put their
  # linking tags in roxygen comments, and a test file that follows that
  # habit should not silently link to nothing.
  hits <- grep("^\\s*#+'?\\s*@covers\\b", lines, value = TRUE)
  for (h in hits) {
    body <- sub("^\\s*#+'?\\s*@covers\\s*", "", h)
    kind <- sub("^([A-Za-z_]+)\\s*:.*$", "\\1", body)
    if (identical(kind, body) || !kind %in% c("feature", "module", "fn")) next
    rest <- sub("^[A-Za-z_]+\\s*:\\s*", "", body)
    names_v <- trimws(strsplit(rest, ",", fixed = TRUE)[[1L]])
    names_v <- names_v[nzchar(names_v)]
    out <- c(out, paste0(kind, ":", names_v))
  }
  unique(out)
}

#' The file's leading comment block — annotations here apply to every
#' test in the file.
#'
#' @noRd
file_level_covers <- function(lines) {
  start <- which(nzchar(trimws(lines)))
  if (!length(start)) return(character())
  i <- start[1L]
  run <- character()
  while (i <= length(lines) && grepl("^\\s*#", lines[i])) {
    run <- c(run, lines[i])
    i <- i + 1L
  }
  parse_covers(run)
}

#' The contiguous comment run immediately above `line`.
#'
#' @noRd
preceding_covers <- function(lines, line) {
  if (is.na(line) || line <= 1L) return(character())
  i <- line - 1L
  run <- character()
  # A single blank line between the annotation and the block it describes
  # is idiomatic and must not break the association.
  while (i >= 1L && !nzchar(trimws(lines[i]))) i <- i - 1L
  while (i >= 1L && grepl("^\\s*#", lines[i])) {
    run <- c(lines[i], run)
    i <- i - 1L
  }
  parse_covers(run)
}

#' Walk one parsed test file and yield a record per `test_that()` block.
#'
#' Collects, from the block body only: the functions it calls, the input
#' ids it sets, the outputs it reads, and the first argument of any
#' `testServer()` call (which is what resolves a module target).
#'
#' @noRd
find_test_blocks <- function(parsed_expr) {
  acc <- list()

  scan_block <- function(body) {
    called <- character()
    inputs <- character()
    outputs <- character()
    server_target <- NA_character_

    walk <- function(expr) {
      if (!walkable(expr)) return(invisible())
      if (!is.call(expr)) return(invisible())

      head <- expr[[1L]]
      nm <- test_callee_name(head)

      # `output$x` / `output[["x"]]` — the reads a test asserts on.
      if (!is.na(nm) && nm %in% c("$", "[[") && length(expr) == 3L &&
          is.name(expr[[2L]]) &&
          identical(as.character(expr[[2L]]), "output")) {
        key <- expr[[3L]]
        val <- if (is.name(key)) as.character(key) else
          if (is.character(key) && length(key) == 1L) key else NA_character_
        if (!is.na(val) && nzchar(val)) outputs <<- c(outputs, val)
      }

      if (!is.na(nm)) {
        called <<- c(called, nm)
        if (nm %in% c("setInputs", "set_inputs")) {
          arg_names <- names(as.list(expr)[-1L])
          if (length(arg_names)) {
            inputs <<- c(inputs, arg_names[nzchar(arg_names)])
          }
        }
        if (nm == "testServer" && length(expr) >= 2L) {
          server_target <<- test_server_target(expr[[2L]])
        }
      }

      for (i in seq_along(expr)) walk(expr[[i]])
      invisible()
    }

    walk(body)
    list(called = unique(called), inputs = unique(inputs),
         outputs = unique(outputs), server_target = server_target)
  }

  walk_top <- function(expr, own_srcref) {
    if (!walkable(expr) || !is.call(expr)) return(invisible())
    nm <- callee_name(expr[[1L]])
    if (!is.na(nm) && nm == "test_that" && length(expr) >= 3L) {
      desc <- expr[[2L]]
      desc <- if (is.character(desc) && length(desc) == 1L) desc else
        NA_character_
      loc <- loc_from(own_srcref)
      span <- if (is.null(own_srcref)) NA_integer_ else
        as.integer(own_srcref)[3L]
      scanned <- scan_block(expr[[3L]])
      acc[[length(acc) + 1L]] <<- c(
        list(desc = desc, line = loc$line, line_end = span), scanned)
      return(invisible())
    }
    for (i in seq_along(expr)) {
      child <- expr[[i]]
      if (!walkable(child)) next
      walk_top(child, child_srcref(expr, i) %||% pick_srcref(child) %||%
                 own_srcref)
    }
    invisible()
  }

  for (i in seq_along(parsed_expr)) {
    walk_top(parsed_expr[[i]],
             pick_srcref(parsed_expr[[i]]) %||% child_srcref(parsed_expr, i))
  }
  acc
}

#' The callee name of a call in a test block.
#'
#' Extends `callee_name()` with the receiver form a `testServer()` body
#' is written in: `session$setInputs(...)` heads on a `$` call, not on a
#' symbol or a `::`, and reading only the latter two would miss every
#' input a test sets.
#'
#' @noRd
test_callee_name <- function(head) {
  nm <- callee_name(head)
  if (!is.na(nm)) return(nm)
  if (is.call(head) && length(head) == 3L &&
      identical(as.character(head[[1L]]), "$") && is.name(head[[3L]])) {
    return(as.character(head[[3L]]))
  }
  NA_character_
}

#' The module binding a `testServer()` call drives, or NA.
#'
#' `testServer(mod_card$server, ...)` yields `mod_card`;
#' `testServer(mod_card_server, ...)` yields `mod_card_server`. Anything
#' else — an app directory, a `test_path()` call — is a feature-level
#' harness and resolves by touched outputs instead.
#'
#' @noRd
test_server_target <- function(arg) {
  if (is.name(arg)) return(as.character(arg))
  if (is.call(arg) && length(arg) == 3L &&
      identical(as.character(arg[[1L]]), "$") && is.name(arg[[2L]])) {
    return(as.character(arg[[2L]]))
  }
  NA_character_
}

#' The canonical empty Tests table.
#' @noRd
empty_tests_table <- function() {
  tibble::tibble(
    file = character(), line = integer(), desc = character(),
    harness = character(), is_support = logical(), filled = logical(),
    md5 = character(),
    annotations = list(), touched_inputs = list(), touched_outputs = list(),
    called_functions = list(), server_target = character()
  )
}

#' Build the Tests table — the sixth intermediate table (spec 06).
#'
#' One row per `test_that()` block found under `test_path`. Built from
#' the test tree and never from the app graph: no row here contributes a
#' node, an edge, or an inventory entry.
#'
#' `file` is recorded relative to `app_path` when the test tree lives
#' inside it, so the artifact carries no absolute path and stays
#' byte-deterministic across machines.
#'
#' @noRd
build_tests_table <- function(test_path, app_path = NULL) {
  if (is.null(test_path) || !dir.exists(test_path)) return(empty_tests_table())
  svt_memoize(paste0("tests\x1f", test_path), function() {
    rels <- enumerate_test_files(test_path)
    prefix <- test_tree_prefix(test_path, app_path)

    rows <- list()
    for (rel in rels) {
      full <- file.path(test_path, rel)
      lines <- tryCatch(readLines(full, warn = FALSE),
                        error = function(e) character())
      parsed <- tryCatch(parse(file = full, keep.source = TRUE),
                         error = function(e) NULL)
      reported <- paste0(prefix, rel)
      file_md5 <- unname(tools::md5sum(full))
      support <- is_support_file(rel)
      file_covers <- file_level_covers(lines)
      if (is.null(parsed)) next

      for (blk in find_test_blocks(parsed)) {
        block_text <- block_source(lines, blk$line, blk$line_end)
        rows[[length(rows) + 1L]] <- tibble::tibble(
          file = reported,
          line = as.integer(blk$line),
          desc = blk$desc,
          harness = if ("testServer" %in% blk$called) "testserver" else "unit",
          is_support = support,
          filled = !any(grepl("SVT scaffold", block_text, fixed = TRUE)),
          md5 = file_md5,
          annotations = list(unique(c(file_covers,
                                      preceding_covers(lines, blk$line)))),
          touched_inputs = list(sort(blk$inputs)),
          touched_outputs = list(sort(blk$outputs)),
          called_functions = list(sort(blk$called)),
          server_target = blk$server_target
        )
      }
    }
    if (!length(rows)) return(empty_tests_table())
    out <- do.call(rbind, rows)
    out[order(out$file, out$line), , drop = FALSE]
  })
}

#' Path prefix that makes a test file's path app-relative.
#'
#' @noRd
test_tree_prefix <- function(test_path, app_path) {
  if (is.null(app_path)) return("")
  tp <- normalizePath(test_path, winslash = "/", mustWork = FALSE)
  ap <- normalizePath(app_path, winslash = "/", mustWork = FALSE)
  if (!startsWith(tp, paste0(ap, "/"))) return("")
  paste0(substring(tp, nchar(ap) + 2L), "/")
}

#' The source lines of one `test_that()` block, from its srcref span.
#'
#' Used only to look for the scaffold skip marker. Reading the raw text
#' rather than deparsing the AST is deliberate: `skip("SVT scaffold ...")`
#' is a string literal, and the raw line is the cheapest exact match.
#' The span is the block's own srcref, so a neighbouring scaffold can
#' never bleed into this block's verdict.
#'
#' @noRd
block_source <- function(lines, line, line_end = NA_integer_) {
  if (is.na(line) || !length(lines)) return(character())
  start <- max(1L, as.integer(line))
  end <- if (is.na(line_end)) start else min(length(lines), as.integer(line_end))
  if (end < start) end <- start
  lines[start:end]
}
