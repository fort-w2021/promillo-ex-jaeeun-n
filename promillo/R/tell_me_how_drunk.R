#' calculate the blood alcohol per mille at the end of a party
#'
#' The calculation is based on Whatson's and Widmark's formula.
#'
#' @param age single numeric value
#' @param sex single character value, either "male" or "female"
#' @param height single numeric value
#' @param weight single numeric value
#' @param drinking_time vector of two POSIXct values indicating when person started and ended drinking
#' @param drinks list or vector named with possible drink choices "massn", "hoibe", "wein", and "schnaps" and the number of portions drunk
#' @return blood alcohol per mille as single numeric value
#' @export
#' @import checkmate
tell_me_how_drunk <- function(age, sex = c("male", "female"), height, weight,
                              drinking_time, drinks) {
  sex <- tolower(sex)
  sex <- match.arg(sex)
  assert_number(age, lower = 10, upper = 110)
  assert_number(height, lower = 100, upper = 230)
  assert_number(weight, lower = 40, upper = 300)
  assert_posixct(drinking_time, any.missing = FALSE, sorted = TRUE, len = 2)
  drinks <- unlist(drinks)
  assert_subset(names(drinks),
                choices = c("massn", "hoibe", "wein", "schnaps"),
                empty.ok = FALSE
  )
  assert_numeric(drinks,
                 lower = 0, any.missing = FALSE, min.len = 1
  )

  illegal <- (age < 16 & sum(drinks) > 0) |
    (age < 18 & isTRUE(drinks["schnaps"] > 0))
  if (illegal) {
    warning("\u2639 ...illegal!  \u2639")
  }

  alcohol_drunk <- get_alcohol(drinks)
  bodywater <- get_bodywater(sex, age, height, weight)
  get_permille(alcohol_drunk, bodywater, drinking_time)
}

# utilities --------------------------------------------------------------------

#' calculate the amount of alcohol for a set of alcohols drunk
#'
#' For each of the possible types of drink, a fixed volume of one portion and
#' alcohol concentration is assumed. The alcohol density is fixed at 0.8.
#'
#' @param drinks list or vector named with possible drink choices "massn", "hoibe", "wein", and "schnaps" and the number of portions drunk
#' @return single numeric value
get_alcohol <- function(drinks) {
  volume <- c(
    "massn"    = 1000,
    "hoibe"   = 500,
    "wein"    = 200,
    "schnaps" = 40
  )
  alcohol_concentration <- c(
    "massn"    = 0.06,
    "hoibe"   = 0.06,
    "wein"    = 0.11,
    "schnaps" = 0.4
  )
  alcohol_density <- 0.8

  sum(drinks * volume[names(drinks)] *
        alcohol_concentration[names(drinks)] * alcohol_density)
}


#' calculate the bodywater using basic body properties
#'
#' Whatson's formula is used with Eickner's modification to include the
#' age dependency for females.
#'
#' @param age single numeric value
#' @param sex single character value, either "male" or "female"
#' @param height single numeric value
#' @param weight single numeric value
#' @return single numeric value
get_bodywater <- function(sex = c("male", "female"), age, height, weight) {
  coef <- if (sex == "male") {
    c(2.447, -0.09516, 0.1074, 0.3362)
  } else {
    c(0.203, -0.07, 0.1069, 0.2466)
  }
  t(coef) %*% c(1, age, height, weight)
}


#' calculate the blood alcohol per mille
#'
#' The formula is based on a combination of Whatson's and Widmark's formula.
#'
#' @param alcohol_drunk single numeric value of amount of alcohol consumed; preferably output of function 'get_alcohol'
#' @param bodywater single numeric value of bodywater; preferably output of function 'get_bodywater'
#' @param drinking_time vector of two POSIXct values indicating when person started and ended drinking
#' @return single numeric value
get_permille <- function(alcohol_drunk, bodywater, drinking_time) {
  alcohol_density <- 0.8
  blood_density <- 1.055
  permille <- alcohol_density * alcohol_drunk / (blood_density * bodywater)

  partylength <- difftime(drinking_time[2], drinking_time[1], units = "hours")
  sober_per_hour <- 0.15
  # sobering up starts only after one hour & you can't be more sober than 0:
  max(0, permille - (max(0, partylength - 1) * sober_per_hour))
}
