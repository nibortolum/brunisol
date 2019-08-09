#' Rearrange dates
#'
#' @description This function takes as input a vector of characters representing dates, (i.e. "2015-9-28"), in any format, converts
#' it into iso time and split years, months, and day
#' @param dates a character vector of dates
#'
#' @return
#' a data.frame containing the dates in ISO format, years, months, and days
#' @export
#'
#' @examples
#' year <- c(rep(2014,12), rep(2015, 12), rep(2016, 12))
#' month <- rep(1:12, 3)
#' day <- sample(1:28, 36, replace = TRUE)
#' my_date <- paste(year, month, day, sep="-")
#'
#' rearrange_dates(my_date)
rearrange_dates <- function(dates){
  dates_conv <- parsedate::parse_date(dates)
  years <- lubridate::year(dates_conv)
  months <- lubridate::month(dates_conv)
  days <- lubridate::day(dates_conv)

  return(data.frame(dates = dates_conv, years, months, days))

}
