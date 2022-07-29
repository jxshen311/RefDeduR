#' Clean and normalize title in bibliography
#'
#' For title, we do the following string normalization.
#' - remove trademark "(TM)"
#' - convert letters to lowercase
#' - remove punctuation
#' - remove all space
#'
#' @param title A character vector (e.g., a column in a data frame)
#'
#' @return Normalized character vector
#' @export
#'
#' @examples
#' title <- c("Evaluation of the Abbott RealTime (TM) CT assay with the BD ProbeTec (TM) ET assay for the detection of Chlamydia trachomatis in a clinical microbiology laboratory",
#' "Evaluation of the Abbott RealTime CT assay with the BD ProbeTec ET assay for the detection of Chlamydia trachomatis in a clinical microbiology laboratory",
#' "Evaluation of the Abbott RealTime(tm) CT assay with the BD ProbeTec(tm) ET assay for the detection of Chlamydia trachomatis in a clinical microbiology laboratory",
#' "beta-lactam Resistance in Pseudomonas aeruginosa: Current Status, Future Prospects" )
#'
#' norm_title(title)
#'
norm_title <- function(title){
  title_norm <- gsub("\\(TM\\)", "", title, ignore.case = TRUE)

  title_norm <- tolower(title_norm)

  title_norm <- tm::removePunctuation(title_norm)
  title_norm <- gsub(" ", "", title_norm)

  return(title_norm)
}




#' Clean and normalize author in bibliography
#'
#' For author, we do the following string normalization.
#' - replace “and” with “&” (Reduce the effect on dissimilarity)
#' - remove all space (1. Remove extra whitespace  2. Reduce the effect of space-caused dissimilarity)
#' - convert letters to lowercase
#'
#'
#' @param author A character vector (e.g., a column in a data frame)
#' @param rm_punctuation Logical: Does unaccenting characters introduce extra punctuation? If so, these need to be removed. Defaults to `FALSE`.
#'
#' Using `bash iconv` to unaccent characters will introduce extra punctuation (e.g., '`^~\"). If using bash iconv, these punctuation needs to be removed as well. Since we use python now, this is not needed by default.
#'
#'
#' @return Normalized character vector
#' @export
#'
#' @examples
#' # Example 1
#' author <- c("Xia, Z. X. and Dai, W. W. and Xiong, J. P. and Hao, Z. P. and Davidson, V. L. and White, S. and Mathews, F. S.",
#' "Ahmed, M. H. and Koparde, V. N. and Safo, M. K. and Neel Scarsdale, J. and Kellogg, G. E.",
#' "Whitman, C. P." )
#'
#' norm_author(author)
#'
#'
#' # Example 2
#' # é becomes 'e if you use `cat file.bib | iconv -f utf8 -t ascii//TRANSLIT//IGNORE > convert.bib` to unaccent characters. Make `rm_punctuation = TRUE` to remove the extra punctuation intruduced.
#' author2 <- c("Ren'ee")
#'
#' norm_author(author2, rm_punctuation = TRUE)
#'
norm_author <- function(author, rm_punctuation = FALSE){
  author_norm <- gsub("\\band\\b", "&", author)
  author_norm <- gsub(" ", "", author_norm)
  author_norm <- tolower(author_norm)

  if(rm_punctuation){author_norm <- gsub("['`^~\"]", "", author_norm)}

  return(author_norm)
}




#' Clean and normalize journal in bibliography
#'
#' For journal, we do the following string normalization.
#' - convert letters to lowercase
#' - remove punctuation
#' - remove English stop words
#' - remove whitespace from start and end of string; also reduce repeated whitespace inside the string.
#'
#' @param journal A character vector (e.g., a column in a data frame)
#'
#' @return Normalized character vector
#' @export
#'
#' @examples
#' journal <- c("Proteins: Structure, Function and Bioinformatics",
#' "Zoonoses Public Health",
#' "Zoonoses and Public Health")
#'
#' norm_journal(journal)
#'
norm_journal <- function(journal){
  journal_norm <- tolower(journal)

  journal_norm <- tm::removePunctuation(journal_norm)

  journal_norm <- tm::removeWords(journal_norm, tm::stopwords("english"))

  journal_norm <- stringr::str_squish(journal_norm)

  return(journal_norm)
}






