#' Puts the various parts of speech together into a full phrase.
#'
#' @param num An integer
#' @param num_word A string corresponding to the integer
#' @param item A string
#' @param verb A string
#' @param adjective A string
#' @param location A string
#'
#' @return A string containing the words in grammatical order.
#'
#' @import stringr
#' @import glue
#' @import dplyr
#' @import purrr
#'
#' @export


make_phrase <- function(num, num_word, item, verb, adjective, location){
  item <- replace_na(item, "")
  verb <- replace_na(verb, "")
  adjective <- replace_na(adjective, "")
  location <- replace_na(location, "")
  if (num > 1) {
    item <- pluralize_gift(item)
  }
  str_squish(paste(num_word, adjective, item, verb, location, sep = " "))
}

