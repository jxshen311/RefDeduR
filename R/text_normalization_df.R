#' Clean and normalize the entire data frame with bibliographic information
#'
#' Normally we have bibliographic information imported as a data frame rather than individual character vectors. This function `norm_df` wraps all (1) text normalization and (2) helper field extraction that are needed in the downstream reference deduplication.
#'
#'
#' @param df A data frame with bibliographic information
#'
#' @return A normalized data frame. New columns containing normalized and extracted data are added to the original data frame. By default, expect 8 more columns compared with the original data frame.
#' @export
#'
#' @details
#' In detail, it normalizes
#' - doi (simply convert to lowercase; add column `doi_norm`)
#' - title (see sub-function [RefDeduR::norm_title()]; add column `title_norm`)
#' - author (see sub-function [RefDeduR::norm_author()]; add column `author_norm`)
#' - journal (see sub-function [RefDeduR::norm_journal()]; add column `journal_norm`)
#' - year (simply convert data type from character to integer; do not add any column)
#' - abstract (see sub-function [RefDeduR::norm_abstract()]; add column `abstract_norm`)
#'
#' Additionally, it extracts
#' - `first_author_last_name` and `first_author_last_name_norm`
#' - `journal_initialism` (apply function [RefDeduR::extract_initialism()] to column `journal_norm`).
#'
#'
#' @seealso Its sub-functions:
#' - [RefDeduR::norm_title()]
#' - [RefDeduR::norm_author()]
#' - [RefDeduR::norm_journal()]
#' - [RefDeduR::norm_abstract()]
#' - [RefDeduR::extract_initialism()]
#'
#' @examples
#' data(bib_example_small)
#'
#' df_new <- norm_df(bib_example_small)
#'
#'
norm_df <- function(df){
  # 1. normalize DOI ----
  df$doi_norm <- tolower(df$doi)



  # 2. normalize title ----
  df$title_norm <- norm_title(df$title)



  # 3. normalize author ----
  df$author_norm <- norm_author(df$author)

  # 3.1 extract first_author_last_name ----
  df$first_author_last_name <- sapply(stringr::str_split(df$author, ",",  n = 2), `[`, 1)

  df$first_author_last_name_norm <- sapply(stringr::str_split(df$author_norm, ",",  n = 2), `[`, 1)



  # 4. normalize journal ----
  df$journal_norm <- norm_journal(df$journal)

  # 4.1 extract initialism from normalized journal ----
  df$journal_initialism <- extract_initialism(df$journal_norm)



  # 5. normalize year ----
  df$year <- as.integer(df$year)



  # 6. normalize abstract ----
  df$abstract_norm <- norm_abstract(df$abstract, df$first_author_last_name)



  return(df)
}
