#' Wrapper function to sleep
#' @param sec  A numeric to sleep (sec).
#' @return No return value, called for side effects.
#' @export
sleep <- function(sec = 5){ Sys.sleep(sec) }

#' Wrapper function to convert into numeric without warnings
#' @param x  A string.
#' @return A numeric or NA.
#' @export
as_numeric_without_warnings <- function(x) { suppressWarnings(as.numeric(x)) }

#' Wrapper function to head 3 items
#' @param x   An object.
#' @return  An object like x with length 3.
#' @export
head_3 <- function(x) utils::head(x ,3)
