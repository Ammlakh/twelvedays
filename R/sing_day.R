#' Produces the string for one day of the song.
#'
#' @param dataset A data frame containing information about gifts
#' @param line The number of the line for the day you want to sing about
#' @param phrase_col The variable name for the column in the dataset that
#' contains the gift phrases
#'
#' @return A string singing the line of the song with all gifts for the given day.
#'
#' @import stringr
#' @import dplyr
#' @import glue
#' @import purrr
#' @import xfun
#' @import english
#' @import tidyverse
#' @export
sing_day <- function(dataset, line, phrase_col){

  val <- line
  num_word <- ordinal(val)
  og <- str_glue("On the {num_word} day of Christmas, my true love gave to me,")
  phrase <- pull(dataset, {{phrase_col}})
  phrase[1] <- paste0("and ", phrase[1], ".")
  items <- str_c(phrase[val:1], collapse = ", \n")
  str_glue("{og} \n{items}")

}


