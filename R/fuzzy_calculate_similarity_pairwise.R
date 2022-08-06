# assign "00" to where NA : "00" is used as the code. if you customize your own partitioning parameter, try to avoid this
# result: calculate similarity - pairwise after partitioning: 1436.371 sec elapsed
#' Calculate pairwise string similarity
#'
#' @description The function calculates pairwise similarity based on Levenshtein edit distance for columns `"title_norm"` and `"abstract_norm"` between records within the same group after partitioning. Range of similarity is \[0, 1\]. `Similarity == 1` means 100% identical while `Similarity == 0` means completely different.
#'
#' @param df A data frame with bibliographic information that has gone through text normalization. `df` must have the following columns `c("title_norm", "abstract_norm")`.
#' @param partition_by Quoted name of the column by which to partition the rows. Defaults to `"first_two_letters_first_author_last_name"`. Can be `FALSE` if prefer not to partition, in which case all records are compared against all others.
#'
#' We recommend ...//////"year", construct a custom "partition" column, FALSE
# how to treat NA, against all or only within NA group
#' Quoted name of the column with information to double check by. Not required/ignored if `double_check == FALSE`. Defaults to `"first_author_last_name_norm"` if `double_check == TRUE`.
#'
#' @return
#' @export
#'
#' @examples
simi_ptn_pair <- function(
    df,
    partition_by = "first_two_letters_first_author_last_name" # "year", construct a custom "partition" column, FALSE
){# check df ----
  if(missing(df)){
    stop("'Data frame' is missing: Please provide a data frame.")
  }
  if(!inherits(df, "data.frame")){
    stop("'df' must be a data frame: Please check the data type using `class(df)`.")
  }
  if(!all(c("title_norm", "abstract_norm") %in% colnames(df))){
    stop('Necessary columns are missing.\n
         Please make sure all the following columns exist:\n
         c("title_norm", "abstract_norm")')
  }


  # partition (author not NA) ----
  # check partition_by


  # treat different partitioning scenarios
  if(partition_by == FALSE){
    df$partition <- as.factor("00")
  }
  else if(partition_by == "first_two_letters_first_author_last_name"){
    if(!("first_author_last_name_norm" %in% colnames(df))){
      stop('Column "first_author_last_name_norm" is missing in the data frame.')
    }

    df$first_author_last_name_norm[which(is.na(df$first_author_last_name_norm))] <- "00"  # assign "00" to where NA
    df$partition <- as.factor(substring(df$first_author_last_name_norm, 1, 2))
  }
  else {
    if(!(partition_by %in% colnames(df))){
      stop('Column to partition_by is missing in the data frame.')
    }

    df[[partition_by]][which(is.na(df[[partition_by]]))] <- "00"  # assign "00" to where NA
    df$partition <- as.factor(df[[partition_by]])
  }

  # create list
  ls_df <- list()
  ls_df_simi <- list()

  # partition and store in the list
  for (ii in 1:nlevels(df$partition)){
    ls_df[[ii]] <- dplyr::filter(df, partition == levels(df$partition)[ii])

    rownames(ls_df[[ii]]) <- NULL  # reset row names

    # assign "id"
    ls_df[[ii]] <- tibble::rownames_to_column(ls_df[[ii]], var = "id")
    ls_df[[ii]]$id <- as.integer(ls_df[[ii]]$id)
    }


  # calculate title and abstract similarity ----
  # 5451.588 sec elapsed on my old mac -> recommend running on HPC is available
  for (ii in 1:nlevels(df$partition)){
    temp_simi_ti <- simi_edit_pairwise(ls_df[[ii]]$title_norm, "title_simi")
    temp_simi_ab <- simi_edit_pairwise(ls_df[[ii]]$abstract_norm, "abstract_simi")
    ls_df_simi[[ii]] <- dplyr::full_join(temp_simi_ti, temp_simi_ab, by = c("Var1", "Var2"))
  }

  return(list(ls_df, ls_df_simi))

}
