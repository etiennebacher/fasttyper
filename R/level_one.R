#' Title
#'
#' @param n
#'
level_one <- function(n = 2) {

  functions <- c("mean", "plot", "data.frame", "library", "c", "length",
                 "list", "paste0", "paste", "cat", "function")
  arguments <- c("a", "b", "x", "foo", "test", "data")

  run_level(n = n, functions = functions, arguments = arguments)

}
