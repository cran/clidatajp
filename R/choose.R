#' Choose data with menu. 
#' @param df           A dataframe
#' @param filter_cols  A string or string vector
#' @param extract      A string
#' @return    If extract is NULL, return a dataframe, 
#'            else return a vector.
#' @examples
#' if(interactive()){
#'   data(climate_world)
#'   climate_world <- 
#'     climate_world %>%
#'     dplyr::mutate_all(stringi::stri_unescape_unicode)
#'   
#'   choose_with_menu(climate_world, filter_cols = "continent")
#'   4  # input
#'   
#'   choose_with_menu(climate_world, filter_cols = c("continent", "country", "station"))
#'   4  # input
#'   3  # input
#'   2  # input
#' }
#' 
#' @export
choose_with_menu <- function(df, filter_cols, extract = NULL){
  for(col in filter_cols){
    options <- unique(df[[col]])
    choice <- options[utils::menu(options)]
    df <- dplyr::filter(df, .data[[col]] == choice)
  }
  if(is.null(extract)) 
    return(df) 
  else 
    return(df[[extract]])
}
