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
#' @export
#'
#' @importFrom purrr map_dfc map_dbl %>% flatten_dbl
#' @importFrom tibble as.tibble tibble
#' @importFrom utils head tail
#'
#' @examples
#'\dontrun{
#' keydur(c(2,5,10),6.2,1,2,1)
#' }

keydur <- function(keyrates,ttm,coupon,yield,freq,targetdur= NULL) {

  # add keyrate at 0 and far in future
  keyrates_expanded <- c(0, keyrates , (tail(keyrates,1)+10) *100 )
  # list of occurence of cashflow in time
  tt <- seq((ttm*freq - floor(ttm*freq))/freq, ttm, by = 1/freq )
  # list of value of cash flows
  cf <- rep(coupon/freq/100, length(tt)) + c( rep(0, length(tt)-1), 1)
  # discounted cash flows
  dcf <- cf * ( 1 + yield / 100 ) ^ (-tt)
  # duration per cash flow, total duration would be sum(vec)
  vec <- dcf * tt / sum( dcf )
  # get matrix of weights
  kw <- map_dfc( tt, ~ as.tibble(wg(keyrates_expanded, .x ) ) )
  # matrix multiplication
  k <- as.matrix( kw ) %*% as.matrix( vec ) %>% as.tibble %>%  flatten_dbl()

  # add keyrate at zero to first keyrate and add keyrate far in future to last keyrate
  k <- c(sum(head(k,2)), k[3:(length(k)-2)], sum( tail(k,2)) )

  # use stated duration if given
  if ( ! is.null(targetdur) ) { k <- k * targetdur/sum(k)  }

  return(tibble( kr = keyrates, val=k))
}


#' wg helper function
#'
#' @param keyrates vector of keyrates
#' @param tt time of cash flow to attribute to keyrates
#'
#' @return a list of weights with length of keyrates, sum = 1
#' @export
#'
#' @examples
#'\dontrun{
#' wg(c(2,5,10),6.2)
#' }
wg <- function (keyrates,tt) {
  # list of zeros length of keyrates
  w <- keyrates * 0

  # find index of first keyrate > tt
  i <-  min(which(keyrates > tt))

  #assign percentages
  w[i] <- (tt-keyrates[i-1] )/ (keyrates[i] - keyrates[i-1])
  w[i-1] <- 1 - w[i]
  return(unlist(w))
}




