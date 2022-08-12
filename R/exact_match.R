#' Find duplicates by exact match
#'
#' @param df A data frame with bibliographic information that has gone through text normalization. `df` must have the following 6 columns `c("author", "title", "journal", "abstract", "year", "doi")`.
#' @param match_by Quoted name of the column with information to match by (e.g., `"doi_norm"`, `"title"`, `"title_norm"`).
#' @param double_check Logical: Is confirmarion against another column needed? Defaults to `FALSE`.
#' @param check_by Quoted name of the column with information to double check by. Not required/ignored if `double_check == FALSE`. Defaults to `"first_author_last_name_norm"` if `double_check == TRUE`.
#'
#' @details
#' - Records with missing information (i.e., NA) in `match_by` column won't be modified.
#' - **Double check criteria:**
#' Within each identified duplicate set, if `check_by` is the same, remain as duplicates without review; if `check_by` is different, output the duplicate set in `df_manual_check` for manual review.
#'
#'
#' @return
#' - If `double_check == FALSE`, return the input `df` with a new column named `"match"`.
#' - If `double_check == TRUE`, return 2 data frames (the input `df` and `df_manual_check`). Syntax `%<-%` must be used in this case to have the function return 2 data frames.
#' @export
#'
#' @examples
#' library(zeallot)  # to use `%<-%`
#'
#' # load example dataset
#' data(bib_example_small)
#'
#' # text normalization of the data frame
#' df <- norm_df(bib_example_small)
#'
#' # example 1:
#' df_1 <- dup_find_exact(df, match_by = "doi_norm", double_check = FALSE)
#' # Alternatively, %<-% will also work
#' df_2 %<-% dup_find_exact(df, match_by = "doi_norm", double_check = FALSE)
#'
#' # example 2:
#' c(df, df_manual_check) %<-% dup_find_exact(df, match_by = "title_norm", double_check = TRUE, check_by = "first_author_last_name_norm")  # Syntax %<-% must be used in this case to have the function return 2 data frames.
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
  if(!inherits(df, "data.frame")){
    stop("'df' must be a data frame: Please check the data type using `class(df)`")
  }
  if(!all(c("author", "title", "journal", "abstract", "year", "doi") %in% colnames(df))){
    stop('Necessary columns are missing.\n
         Please make sure all the following 6 columns exist:\n
         c("author", "title", "journal", "abstract", "year", "doi")')
  }

  # 1. check match_by column ----
  if(missing(match_by)){
    stop("Please specify the column to match by.")
  }
  if(!inherits(match_by, "character")){
    stop("Please quote the column name. That is, `class(match_by)` must be character.")
  }
  if(!(match_by %in% colnames(df))){
    stop('Column to match_by is missing in the data frame.')
  }

  # 2. subset df according to the existence ----
  df_no <- df[is.na(df[[match_by]]), ]
  df_yes <- df[!is.na(df[[match_by]]), ]

  # 3. order so that the most recent version will be kept ----
  df_yes <- df_yes[order(-df_yes[["year"]]), ]

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







#' Find duplicates by exact match and remove them
#'
#' @description
#' - This automatically removes duplicates identified by exact match without manual review.
#' - The most recent version will be retained at removal.
#' - Support deduplication based on multiple columns (one at a time).
#'
#' @details
#' Records with missing information (i.e., NA) in `match_by` column won't be modified.
#'
#'
#' @param df A data frame with bibliographic information that has gone through text normalization. `df` must have the following 6 columns `c("author", "title", "journal", "abstract", "year", "doi")`.
#' @param match_by Quoted name(s) of the column(s) with information to match by (e.g., `"doi_norm"`, `"title"`, `"title_norm"`, `c("doi_norm", "title", "title_norm")`).
#'
#' If supplying a character vector with *multiple* elements, deduplication will be performed in order. For example, if `match_by = c("doi_norm", "title", "title_norm")`, deduplication will be performed first according to `"doi_norm"`, then `"title"`, and finally `"title_norm"`.
#'
#'
#' @return Deduplicated `df`.
#' @export
#'
#' @examples
#' # load example dataset
#' data(bib_example_small)
#'
#' # text normalization of the data frame
#' df <- norm_df(bib_example_small)
#'
#' # deduplicate according to 3 columns in order (one at a time)
#' df_new <- dedu_exact(df, match_by = c("doi_norm", "title", "title_norm"))
dedu_exact <- function(
    df,
    match_by # c("doi_norm", "title", "title_norm"); follow the order
    ){
  for (ii in 1:length(match_by)){
    df <- dup_find_exact(df,
                         match_by = match_by[ii],
                         double_check = FALSE)

    # remove duplicates and helper column
    df <- df[!duplicated(df$match), ]
    df <- select(df, -match)
  }

  return(df)
}

