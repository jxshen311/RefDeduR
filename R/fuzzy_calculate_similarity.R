# assume all records have title
# result: calculate similarity - alphabetic order: 46.962 sec elapsed
# calculate similarity based on Levenshtein edit distance for columns `"title_norm"`, `"author_norm"`, `"abstract_norm"`, and `"first_author_last_name_norm"` between adjacent rows.

#' Title
#'
#' @param df
#' @param order_by
#'
#' @return
#' @export
#'
#' @examples
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
  df <- rownames_to_column(df, var = "id")
  df$id <- as.integer(df$id)

  # calculate similarity ----
  df_simi <- data.frame(title_simi = numeric(nrow(df)-1))

  for (ii in 1:(nrow(df)-1)){
    df_simi$title_simi[ii] <-  simi_edit_dis(df$title_norm[ii], df$title_norm[ii+1])

    df_simi$author_simi[ii] <-  simi_edit_dis(df$author_norm[ii], df$author_norm[ii+1])

    df_simi$abstract_simi[ii] <- simi_edit_dis(df$abstract_norm[ii], df$abstract_norm[ii+1])

    df_simi$first_author_last_name_simi[ii] <- simi_edit_dis(df$first_author_last_name_norm[ii], df$first_author_last_name_norm[ii+1])
  }



  df_simi <- rownames_to_column(df_simi, var = "id")
  df_simi$id <- as.integer(df_simi$id)

  return(df_simi)

}






