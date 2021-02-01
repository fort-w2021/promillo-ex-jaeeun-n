#' hypothesize course of blood alcohol during a party
#'
#' For simplicity, it is assumed that the drinking speed is constant and
#' the alcohol amount calculated is based on a mix of all drinks drunk during
#' the party.
#'
#' @param age single numeric value
#' @param sex single character value, either "male" or "female"
#' @param height single numeric value
#' @param weight single numeric value
#' @param drinking_time vector of two POSIXct values indicating when person started and ended drinking
#' @param drinks list or vector named with "massn", "hoibe", "wein", or "schnaps" containing the number of portions drunk
#' @return ggplot object
#' @importFrom ggplot2 qplot
#' @importFrom foreach foreach %do%
#' @export
#'
show_me_how_drunk <- function(age, sex, height, weight, drinking_time, drinks) {
  alcohol_at_end_of_party <- tell_me_how_drunk(
    age = age, sex = sex, height = height, weight = weight,
    drinking_time = drinking_time, drinks = drinks
  )

  time_steps <- seq(drinking_time[1], drinking_time[2], "5 mins")

  if (length(time_steps) < 3) {
    output_plot <- qplot(
      x = c(time_steps[1], time_steps[length(time_steps)]),
      y = c(0, alcohol_at_end_of_party),
      geom = "line",
      xlab = "time", ylab = "hypothetic body alcohol per mille"
    )
    return(output_plot)
  }

  steps_to_hypothesize <- time_steps[-c(1, length(time_steps))]

  alcohol_drunk <- get_alcohol(drinks)
  alcohol_per_step <- alcohol_drunk / (length(steps_to_hypothesize) + 1)
  bodywater <- get_bodywater(sex, age, height, weight)

  i = 0
  alcohol_course <- foreach(
    i = seq_along(steps_to_hypothesize), .combine = c) %do%
    get_permille(alcohol_drunk = i * alcohol_per_step, bodywater = bodywater,
                 drinking_time = c(time_steps[1], steps_to_hypothesize[i]))

  qplot(
    x = time_steps,
    y = c(0, alcohol_course, alcohol_at_end_of_party),
    geom = "line",
    xlab = "time", ylab = "hypothetic body alcohol per mille"
  )
}



