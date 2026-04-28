box::use(
  stringr[str_pad],
)

#' @export
pad_id <- function(x) {
  str_pad(x, width = 6, pad = "0")
}
