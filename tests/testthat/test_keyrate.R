
library(purrr)
library(tibble)


context("test keydur")

test_that(" keydur results",
#  expect_equal(keydur(c(2,5,10),6.2,0,3,1) ,1)
  expect_equal(keydur(c(2,5,10),6.2,0,3,1)[[2]][2] ,4.712)
)

test_that(" keydur is a tibble",
  expect_true(is.tibble(keydur(c(2,5,10),6.2,0,3,1)))
)
