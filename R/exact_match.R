#' Find duplicates by exact match
#'
#' @param df A data frame with bibliographic information that has gone through text normalization. `df` must have at least the following 6 columns `c("author", "title", "journal", "abstract", "year", "doi")`.
#' @param match_by
#' @param double_check
#' @param check_by
#'
#' @return
#' @export
#'
#' @examples
dup_find_exact <- function(
    df,
    match_by, # c("doi_norm", "title", "title_norm")
    double_check = FALSE,
    check_by = "first_author_last_name_norm"
){
  # 0. check df ----
  if(missing(df)){
    stop("'Data frame' is missing: Please provide a data frame")
  }
  else if(!inherits(df, "data.frame")){
    stop("'df' must be a data frame: Please check the data type using `class(df)`")
  }
  if(!all(c("author", "title", "journal", "abstract", "year", "doi") %in% colnames(df))){
    stop('Necessary columns are missing.\n
         Please make sure all the following 6 columns exist:\n
         c("author", "title", "journal", "abstract", "year", "doi")')
  }

  # 1. check the existence of match_by column ----
  if(!(match_by %in% colnames(df))){
    stop('Column to match_by is missing.')
  }

  # 2. subset df according to the existence ----
  df_no <- df[is.na(df[[match_by]]), ]
  df_yes <- df[!is.na(df[[match_by]]), ]

  # 3. order so that the most recent version will be kept ----
  df_yes <- df_yes[order(-df_yes[ ,which(colnames(df_yes)=="year")]), ]

  # reset row names
  row.names(df_no) <- NULL
  row.names(df_yes) <- NULL

  # 4. create helper column: match ----
  df_yes$match <- as.numeric(factor(df_yes[[match_by]], ))

  df_no <- tibble::rownames_to_column(df_no, var = "match")
  df_no <- df_no %>% relocate(match, .after = last_col())
  df_no$match <- as.numeric(df_no$match)
  df_no$match = df_no$match + max(df_yes$match)

  # 5. combine df_yes and df_no ----
  df <- rbind(df_yes, df_no)

  # 6. double_check ----
  if(!double_check){
    return(df)
  } else if (double_check){
    # check the existence of column
    if(!(check_by %in% colnames(df))){
      stop('Column to check against is missing.
      Default to "first_author_last_name_norm"')}

    # double check criteria
    # 1. If title_norm and first_author_last_name_norm are both the same, remove duplicates without review
    # 2. When title_norm is the same but first_author_last_name_norm is different, output the dataframe for manual review
    df_manual_check <- df %>%
      group_by(match) %>%
      filter(length(unique(.data[[check_by]])) >= 2)

    # either review in dataframe format (preview in R or write.xlsx()) or call revtools shiny app
    # call revtools shiny app
    # screen_duplicates(df_manual_check)

    # we can use the override_duplicates() function in synthesisr to manually mark records as unique
    # new_match_vector <- synthesisr::override_duplicates(match number vector, the match number to override)
    # test$match_new <- synthesisr::override_duplicates(test$match, 3825)
    return(list(df, df_manual_check))
  }
}




