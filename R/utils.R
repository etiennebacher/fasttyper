dplyr_function_expr <- function(function_name, arg) {

  expr <- NULL

  if (function_name == "mutate") {
    operation <- sample(c(" + ", " - ", " * ", " / ", "^2"), 1)
    number <- sample(as.integer(c(1:100)), 1)
    expr <- paste0(arg, " = ", arg, operation, number)
  } else if (function_name == "filter") {
    operation <- sample(c(" > ", " < ", " == "), 1)
    number <- sample(as.integer(c(1:100)), 1)
    expr <- paste0(arg, operation, number)
  } else if (function_name %in% c("arrange", "group_by", "select")) {
    expr <- arg
  }

  return(expr)

}


init_level <- function(level, n) {

  arguments <<- c("a", "b", "x", "foo", "test", "data")

  number_questions <<- n
  number_questions_done <<- 0
  success <<- 0

  if (level == 1) {
    functions <<- c("mean", "plot", "data.frame", "library", "c", "length",
                   "list", "paste0", "paste", "cat", "function")
  } else if (level > 1) {
    functions <<- c("arrange", "filter", "mutate", "drop_na", "length", "unique",
                   "as_tibble", "ggplot", "group_by", "ungroup", "select")
    data_name <<- c("data", "foo", "x", "df")
  }

}
