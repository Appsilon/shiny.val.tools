box::use(
  stats[median],
)

# `median` is declared by the box clause -- the intended call surface.
mid <- function(x) median(x)

# `sd` is NOT declared by the box clause, but `library(stats)` in global.R
# makes it resolve anyway. That is the SVT-W006 case.
spread <- function(x) sd(x)
