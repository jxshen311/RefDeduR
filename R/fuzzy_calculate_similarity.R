#' Calculate string similarity between adjacent rows
#'
#' @description The function calculates similarity based on Levenshtein edit distance for columns `"title_norm"`, `"author_norm"`, `"abstract_norm"`, and `"first_author_last_name_norm"` between adjacent rows. Range of similarity is \[0, 1\]. `Similarity == 1` means 100% identical while `Similarity == 0` means completely different.
#'
#' @param df A data frame with bibliographic information that has gone through text normalization. `df` must have the following columns `c("author", "title", "journal", "abstract", "year", "doi", "title_norm", "author_norm", "abstract_norm", "first_author_last_name_norm")`.
#' @param order_by Quoted name of the column by which to order the rows. Defaults to `"title_norm"`.
#'
#' @return A data frame with string similarity results for `"title_norm"`, `"author_norm"`, `"abstract_norm"`, and `"first_author_last_name_norm"`.
#'
#' @details This function is based on the assumption that all records have titles.
#'
#' Computing time estimation according to past experience: ~ 47 sec for a data frame with 3837 rows on a Macbook Pro (Apple M1 Pro chip basic model, memory: 16 GB).
#' @export
#'
#' @examples
#' # load example dataset
#' data(bib_example_small)
#'
#' # text normalization of the data frame
#' df <- norm_df(bib_example_small)
#'
#' # calculate similarity
#' df_simi <- simi_order_df(df, order_by = "title_norm")
#' # df_simi[1, ] stores similarity results between df[1, ] and df[2, ]
#'
simi_order_df <- function(
    df,
    order_by = "title_norm"

){
  # check df ----
  if(missing(df)){
    stop("'Data frame' is missing: Please provide a data frame")
  }
  if(!inherits(df, "data.frame")){
    stop("'df' must be a data frame: Please check the data type using `class(df)`")
  }
  if(!all(c("author", "title", "journal", "abstract", "year", "doi", "title_norm", "author_norm", "abstract_norm", "first_author_last_name_norm") %in% colnames(df))){
    stop('Necessary columns are missing.\n
         Please make sure all the following columns exist:\n
         c("author", "title", "journal", "abstract", "year", "doi") and normalized columns c("title_norm", "author_norm", "abstract_norm", "first_author_last_name_norm")')
  }
  if(!inherits(order_by, "character")){
    stop("Please quote the column name. That is, `class(order_by)` must be character.")
  }


  # Alphabetic order by order_by = "title_norm" ----
  df <- df[order(df[[order_by]]), ]
  rownames(df) <- NULL

  # create "id" column
  df <- tibble::rownames_to_column(df, var = "id")
  df$id <- as.integer(df$id)

  # calculate similarity ----
  df_simi <- data.frame(title_simi = numeric(nrow(df)-1))

  for (ii in 1:(nrow(df)-1)){
    df_simi$title_simi[ii] <-  simi_edit(df$title_norm[ii], df$title_norm[ii+1])

    df_simi$author_simi[ii] <-  simi_edit(df$author_norm[ii], df$author_norm[ii+1])

    df_simi$abstract_simi[ii] <- simi_edit(df$abstract_norm[ii], df$abstract_norm[ii+1])

    df_simi$first_author_last_name_simi[ii] <- simi_edit(df$first_author_last_name_norm[ii], df$first_author_last_name_norm[ii+1])
  }



  df_simi <- tibble::rownames_to_column(df_simi, var = "id")
  df_simi$id <- as.integer(df_simi$id)

  return(df_simi)

}






