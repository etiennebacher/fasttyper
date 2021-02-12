#' Produce questions in console
#'
#' Must be used in a level_*() function. Cannot be used on its own.
#'
#' @param n Number of questions.
#' @param functions Vector of function names.
#' @param arguments Vector of argument names.


run_level <- function(n, functions, arguments) {

  number_questions <- n
  number_questions_done <- 0
  success <- 0

  x <- system.time({

    while(number_questions_done < number_questions) {

      to_write <- paste0('"',
                         sample(functions, 1), "(", sample(arguments, 1), ")",
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

  })

  cat("\n")
  cli::cli_alert_info("Score: {success}/{number_questions}")
  cli::cli_alert_info("Time: {round(x[3], 2)}")

}
