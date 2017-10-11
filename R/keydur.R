#' keyrate duration function
#'
#' Calculates keyrate duration of a bond
#'
#' @author wkapga
#'
#' @param keyrates list of keyrates
#'
#' @param ttm time until maturity in years
#' @param coupon in percent
#' @param yield in percent
#' @param freq frequency of coupon payments
#' @param targetdur optional argument what the sum of keyrate durations has to be
#'
#' @return tibble of key rate durations
#'
#' @import dplyr
#' @import purrr
#' @import tibble
#' @import lubridate
#'
#'
#' @export
#' @examples
#'
#' keydur(c(2,5,10),6.2,1,2,1)
#' # A tibble: 3 x 2
#'#kr        val
#'#<dbl>      <dbl>
#'#  1     2 0.06492798
#'#  2     5 4.52113943
#'#  3    10 1.40085786

keydur <- function(keyrates,ttm,coupon,yield,freq,targetdur= NULL) {
  # function to calculate vector of key rate duarations for given keyrates
  # coupon, yield in percent, target duration is optional
  keyrates_expanded <- c(0, keyrates , tail(keyrates,1)*100 ) # add keyrate at 0 and far in future
  tt <- seq((ttm*freq - floor(ttm*freq))/freq, ttm, by = 1/freq ) # vector of occurence of cashflow in time
  cf <- rep(coupon/freq/100, length(tt)) + c( rep(0, length(tt)-1), 1) # cash flows

  dcf <- cf * (1+yield/100)^(-tt) # discounted cash flows
  vec <- dcf * tt / sum(dcf) # duration per cash flow, total duration would be sum(vec)

  kw <- map_dfc(tt, ~ as.tibble(wg(keyrates_expanded,.x))) # get matrix of weights
  k <- as.matrix(kw) %*% as.matrix(vec) %>% as.tibble %>%  flatten_dbl() # matrix multiplication

  # add keyrate at zero to first keyrate and add keyrate far in future to last keyrate
  k <- c(sum(head(k,2)), k[3:(length(k)-2)], sum(tail(k,2)) )

  # use stated duration if given
  if ( ! is.null(targetdur) ) { k <- k * targetdur/sum(k)  }

  return(tibble( kr = keyrates, val=k))
}


wg <- function (keyrates,tt) {
  # helper function for key rate duration
  # find index of first keyrate > tt
  i <- keyrates %>% detect_index(~ .x > tt)
  #spr <- keyrates[i] - keyrates[i-1]
  # list of zeros length of keyrates
  w <- map_dbl(keyrates, ~ 0)

  w <- modify_at(w,i, ~.x + (tt-keyrates[i-1] )/ (keyrates[i] - keyrates[i-1]) )
  w <- modify_at(w,i-1, ~.x + (keyrates[i]-tt )/ (keyrates[i] - keyrates[i-1]) )
  return(unlist(w))
}




