#' Level one test
#'
#' @param n Number of questions
#' @name levels
#' @export
level_one <- function(n = 5) {

  init_level(1, n)

  x <- system.time({
    while(number_questions_done < number_questions) {

      function_name <- sample(functions, 1)
      args <- sample(arguments, 1)

      to_write <- paste0('"',
                         function_name, "(", args, ")",
                         '":\n')
      to_compare <- gsub('\\"', "", to_write)
      to_compare <- gsub(":", "", to_compare)
      to_compare <- gsub("\\n", "", to_compare)

      x <- readline(cat(to_write))

      if (identical(x, to_compare)) {
        cli::cli_alert_success("")
        success <- success + 1
      } else {
        cli::cli_alert_danger("")
        success <- success
      }

      number_questions_done <- number_questions_done + 1

    }
  })

  cat("\n")
  cli::cli_alert_info("Score: {success}/{number_questions}")
  cli::cli_alert_info("Time: {round(x[3], 2)}")

}


#' @name levels
#' @export
level_two <- function(n = 5) {

  init_level(2, n)

  x <- system.time({
    while(number_questions_done < number_questions) {

      data <- sample(data_name, 1)
      function_name <- sample(functions, 1)
      args <- sample(arguments, 1)
      args_2 <- sample(arguments, 1)

      to_write <- paste0('"', data, " %>% ", function_name,
                         "(", dplyr_function_expr(function_name, args),
                         ")", '":\n')
      to_compare <- gsub('\\"', "", to_write)
      to_compare <- gsub(":", "", to_compare)
      to_compare <- gsub("\\n", "", to_compare)

      x <- readline(cat(to_write))

      if (identical(x, to_compare)) {
        cli::cli_alert_success("")
        success <- success + 1
      } else {
        cli::cli_alert_danger("")
        success <- success
      }

      number_questions_done <- number_questions_done + 1

    }
  })

  cat("\n")
  cli::cli_alert_info("Score: {success}/{number_questions}")
  cli::cli_alert_info("Time: {round(x[3], 2)}")

}

#' @name levels
#' @export
level_three <- function(n = 5) {

  init_level(3, n)

  x <- system.time({
    while(number_questions_done < number_questions) {

      data <- sample(data_name, 1)
      args <- sample(arguments, 1)

      n_functions <- sample(c(1:3), 1)

      function_calls <- c()
      for (i in 1:n_functions) {
        function_name <- sample(functions, 1)
        args_2 <- sample(arguments, 1)
        function_calls[i] <- paste0(
          " %>% ", function_name,
          "(", dplyr_function_expr(function_name, args),
          ")"
        )
      }
      function_calls <- paste(function_calls, collapse = "")

      to_write <- paste0('"', data, function_calls, '":\n')
      to_compare <- gsub('\\"', "", to_write)
      to_compare <- gsub(":", "", to_compare)
      to_compare <- gsub("\\n", "", to_compare)

      x <- readline(cat(to_write))

      if (identical(x, to_compare)) {
        cli::cli_alert_success("")
        success <- success + 1
      } else {
        cli::cli_alert_danger("")
        success <- success
      }

      number_questions_done <- number_questions_done + 1

    }
  })

  cat("\n")
  cli::cli_alert_info("Score: {success}/{number_questions}")
  cli::cli_alert_info("Time: {round(x[3], 2)}")

}


