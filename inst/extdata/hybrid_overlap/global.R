# A hybrid app: global.R attaches stats the traditional way, while the
# helper file narrows the same package through box. Per spec 01 the box
# clause wins inside that file, so any other stats call there resolves to
# an unexpected call surface -- SVT-W006.
library(stats)
source("R/summarise.R")
