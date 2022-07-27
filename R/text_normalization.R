#' Clean and normalize
#'
#' @param doi
#'
#' @return
#' @export
#'
#' @examples
norm_title <- function(title){
  # clean the title string:
  # remove trademark "(TM)",
  # tolower()
  title_norm <- gsub("\\(TM\\)", "", title, ignore.case = TRUE)

  title_norm <- tolower(title_norm)

  # remove punctuation, all space
  title_norm <- removePunctuation(title_norm)
  title_norm <- gsub(" ", "", title_norm)


  return(title_norm)
}

