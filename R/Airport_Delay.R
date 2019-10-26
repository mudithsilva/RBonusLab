library(nycflights13)
library(ggplot2)
library(dplyr)
library(methods)

#' Airport Delay Visualise on a ggplot
#'
#' @return ggplot about airport delays
#' @import nycflights13 ggplot2 dplyr methods
#' @export visualize_airport_delays
#'
#' @examples
#' visualize_airport_delays()


visualize_airport_delays <- function() {

  flights <- nycflights13::flights
  airports <- nycflights13::airports

  mean_data <- dplyr::summarise(dplyr::group_by(flights, dest), M = mean(arr_delay))

  combine_data <- dplyr::inner_join(airports,mean_data, by = c("faa" =  "dest"))

  plot_data <- ggplot(combine_data, aes(x = combine_data$lat, y = combine_data$lon)) +
    geom_point()

  return(plot_data)

}

visualize_airport_delays()
