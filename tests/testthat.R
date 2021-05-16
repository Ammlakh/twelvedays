library(testthat)
library(twelvedays)
library(stringr)
library(glue)
library(dplyr)
library(purrr)
library(tidyr)
library(english)


#make_phrase <- function(num, num_word, item, verb, adjective, location)
#  make_phrase(5, "five", "fruit", "went", "tasty", "home")

test_that("2", {
  expect_equal( as.character(1+1), "2" )
})

test_that("gifts", {
  expect_equal( pluralize_gift("gift"), "gifts" )
})

test_that("gifts2", {
  expect_equal( pluralize_gift("goose"), "geese" )
})

test_that("gifts3", {
  expect_equal( pluralize_gift("puppy"), "puppies" )
})


test_that("five tasty fruits went home", {
  expect_equal( make_phrase(5, "five", "fruit", "went", "tasty", "home"), "five tasty fruits went home")
})

test_that("sing", {
  line <- 1
  phrase_col <- c("one banana", "two apples")
  dataset <- data.frame(phrase_col)
  expect_equal( sing_day(dataset, line, phrase_col), "On the first day of Christmas, my true love gave to me, \nand one banana." )
})



