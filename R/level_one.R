#' Title
#'
#' @param n
#'
level_one <- function(n = 2) {

  functions <- c("mean", "plot", "data.frame", "library", "c", "length",
                 "list", "paste0", "paste", "cat", "function")
  arguments <- c("a", "b", "x", "foo", "test", "data")

  number_questions <- n
  number_questions_done <- 0
  success <- 0

  while(number_questions_done < number_questions) {

    function_name <- sample(functions, 1)
    args <- sample(arguments, 1)

    to_write <- paste0('"',
                       function_name, "(", args, ")",
                       '": ')
    to_compare <- gsub('\\"', "", to_write)
    to_compare <- gsub(": ", "", to_compare)

    x <- readline(to_write)

    if (identical(x, to_compare)) {
      cli::cli_alert_success("")
      success <- success + 1
    } else {
      cli::cli_alert_danger("")
      success <- success
    }

    number_questions_done <- number_questions_done + 1

  }

  cat("\n")
  cli::cli_alert_info("Your score is {success}/{number_questions}.")

}



level_two <- function(n = 2) {

  functions <- c("arrange", "filter", "mutate", "drop_na", "length", "unique",
                 "as_tibble", "ggplot", "group_by", "ungroup", "select")
  arguments <- c("a", "b", "x", "foo", "test", "data")
  data_name <- c("data", "foo", "x", "df")

  number_questions <- n
  number_questions_done <- 0
  success <- 0

  while(number_questions_done < number_questions) {

    data <- sample(data_name, 1)
    function_name <- sample(functions, 1)
    args <- sample(arguments, 1)

    to_write <- paste0('"', data, " %>% ", function_name,
                       "(", dplyr_function_expr(function_name, args),
                       ")", '": ')
    to_compare <- gsub('\\"', "", to_write)
    to_compare <- gsub(": ", "", to_compare)

    x <- readline(to_write)

    if (identical(x, to_compare)) {
      cli::cli_alert_success("")
      success <- success + 1
    } else {
      cli::cli_alert_danger("")
      success <- success
    }

    number_questions_done <- number_questions_done + 1

  }

  cat("\n")
  cli::cli_alert_info("Your score is {success}/{number_questions}.")

}


