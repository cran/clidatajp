#' @importFrom rlang :=
NULL

#' @importFrom rlang .data
NULL

#' @importFrom stats na.omit
NULL

#' @importFrom utils data
NULL

#' @importFrom utils head
NULL

#' Pipe operator
#' 
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#' 
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL


#' Graceful fail
#' 
#' @references \url{https://gist.github.com/kvasilopoulos/47f24348ed75cdb6365312b17f4b914c}
#' @param remote_file A string of remote file.
#' @return      An XML document when successed, or invisible NULL when failed.
#' @export
gracefully_fail <- function(remote_file){
  try_GET <- function(x, ...) {
    tryCatch(
      httr::GET(url = x, httr::timeout(1), ...),
      error   = function(e) conditionMessage(e),
      warning = function(w) conditionMessage(w)
    )
  }
  is_response <- function(x) {
    class(x) == "response"
  }

  # First check internet connection
  if (!curl::has_internet()) {
    message("No internet connection.")
    return(invisible(NULL))
  }
  # Then try for timeout problems
  resp <- try_GET(remote_file)
  if (!is_response(resp)) {
    message(resp)
    return(invisible(NULL))
  }
  # Then stop if status > 400
  if (httr::http_error(resp)) { 
    httr::message_for_status(resp)
    return(invisible(NULL))
  }

  return(resp)
}
