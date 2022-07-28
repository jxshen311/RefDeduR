#' Extract initialism
#' Extract 1st letter of each word and delete all the other letters
#'
#' @param string A character vector (e.g., a column in a data frame)
#'
#' @return A character vector
#' @export
#'
#' @example inst/examples/extract_initialism.R
#'
#'
extract_initialism <- function(string){
  initialism <- gsub('\\b(\\w)\\w*|.','\\U\\1', string, perl = TRUE)

  return(initialism)
}


