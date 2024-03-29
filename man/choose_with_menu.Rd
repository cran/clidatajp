% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/choose.R
\name{choose_with_menu}
\alias{choose_with_menu}
\title{Choose data with menu.}
\usage{
choose_with_menu(df, filter_cols, extract = NULL)
}
\arguments{
\item{df}{A dataframe}

\item{filter_cols}{A string or string vector}

\item{extract}{A string}
}
\value{
If extract is NULL, return a dataframe,
else return a vector.
}
\description{
Choose data with menu.
}
\examples{
if(interactive()){
  data(climate_world)
  climate_world <- 
    climate_world \%>\%
    dplyr::mutate_all(stringi::stri_unescape_unicode)
  
  choose_with_menu(climate_world, filter_cols = "continent")
  4  # input
  
  choose_with_menu(climate_world, filter_cols = c("continent", "country", "station"))
  4  # input
  3  # input
  2  # input
}

}
