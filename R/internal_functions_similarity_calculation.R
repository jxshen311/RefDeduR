#' Similarity based on Levenshtein edit distance between 2 strings
#'
#' @param a String 1
#' @param b String 2
#'
#' @return Similarity based on Levenshtein edit distance (range: [0,1]). If both `a` and `b` are `NA`, return `NA`.
#'
#' @keywords internal
#' @noRd
simi_edit <- function(a, b){
  simi <-  1 - (as.numeric(adist(x = a, y = b)) / max(nchar(a), nchar(b)))

  return(simi)
}



#' Pairwise similarity based on Levenshtein edit distance in a list of strings
#'
#' @param data A character vector containing a list of strings
#' @param column_name A string to name the column which stores similarity results
#'
#' @return A data frame with pairwise similarity
#'
#' @keywords internal
#' @noRd
simi_edit_pairwise <- function(data, column_name){
  simi_matrix <- stringsimmatrix(data, method = "lv")

  simi_matrix[lower.tri(simi_matrix, diag = TRUE)] <- NA
  simi_matrix_df <- na.omit(data.frame(as.table(simi_matrix)))
  colnames(simi_matrix_df)[colnames(simi_matrix_df) == "Freq"] <- column_name

  return(simi_matrix_df)
}
