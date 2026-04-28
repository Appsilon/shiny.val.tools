source("bar.R", chdir = TRUE)

foo_helper <- function(x) {
  bar_helper(x) + 1
}
