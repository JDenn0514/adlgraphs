#' Calculate weighted standard deviation
#' 
#' While this is mainly an 
#' internal function, it is available for everyone since I could not find
#' a function that calculates a weighted standard deviation.
#' 
#' Thank you to the team at Hmisc for the creation of [Hmisc::wtd.var()].
#' This function could not have been created without it. I simplified their
#' source code and then took the square root. 
#' 
#' @param x A numeric vector that you want to calculate the SD
#' @param wt A numeric vector indicating the weights used in the calculation
#' @param na.rm Logical. Indicates if NAs should be removed or not
#' 
#' @returns A number indicating the weighted SD of `x`
#' 
#' @export
wtd_sd <- function(x, wt = NULL, na.rm = TRUE) {

  if (na.rm) {
    # if na.rm is TRUE remove all NAs
    x <- x[!is.na(x)]
    wt <- wt[!is.na(wt)]
  }

  if (is.null(wt)){
    # if wts are NULL just calcualte normal sd
    x <- sd(x, na.rm = na.rm)
    return(x)
  } else{
    # otherwise calculate weighted sd
    sw <- sum(wt)
    xbar <- sum(wt * x) / sw
    var <- sum(wt * ((x - xbar)^2))/(sw - 1)
    x <- sqrt(var)
    return(x)
  }

}
