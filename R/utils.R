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
  } else if (function_name == "arrange" || function_name == "select") {
    expr <- arg
  }

  return(expr)

}
