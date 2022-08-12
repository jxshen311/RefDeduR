#' Find duplicates by fuzzy match of string similarity between pairwise records
#'
#' @param ls_df A list of data frames containing the partitioned dataset  (i.e., output #1 of [RefDeduR::simi_ptn_pair()]).
#' @param ls_df_simi A list of data frames with string similarity results calculated (i.e., output #2 of [RefDeduR::simi_ptn_pair()]).
#'
#' @param cutoff_title Numeric: cutoff threshold of string similarity for normalized title. Range: \[0, 1\]. Defaults to 0.7.
#'
#' @param cutoff_abstract Numeric: cutoff threshold of string similarity for normalized abstract. Range: \[0, 1\]. Defaults to 0.7.
#'
#' @details **For both cutoffs:**
#'
#' Cutoff thresholds in [RefDeduR::dup_find_fuzzy_adj()] are usually applicable here. Alternatively, you can re-examine the similarity distribution plots by [RefDeduR::plot_simi_dist()] and choose sensible values.
#'
#' @return A data frame listing record id and partition id of duplicate pairs.
#' @export
#'
#' @examples
#' \dontrun{
#' id_dup_pair <- dup_find_fuzzy_pairwise(ls_df, ls_df_simi, cutoff_title = 0.7, cutoff_abstract = 0.7)
#' }
dup_find_fuzzy_pairwise <- function(
    ls_df,
    ls_df_simi,
    cutoff_title = 0.7,
    cutoff_abstract = 0.7){
  # check ----
  if(missing(ls_df)){
    stop("'ls_df' is missing: Please provide a list of data frames containing the partitioned dataset.")
  }
  if(missing(ls_df_simi)){
    stop("'ls_df_simi' is missing: Please provide the corresponding list of data frames with calculated similarity scores.")
  }
  if(!inherits(ls_df, "list")){
    stop("'ls_df' must be a list: Please check the data type using `class(ls_df)`.")
  }
  if(!inherits(ls_df_simi, "list")){
    stop("'ls_df_simi' must be a list: Please check the data type using `class(ls_df_simi)`.")
  }
  if(length(ls_df) != length(ls_df_simi)){
    stop("'ls_df' and 'ls_df_simi' must be paired.")
  }
  if(!(cutoff_title >= 0 & cutoff_title <= 1)){
    stop("Cutoff threshold must be in the range of [0,1].")
  }
  if(!(cutoff_abstract >= 0 & cutoff_abstract <= 1)){
    stop("Cutoff threshold must be in the range of [0,1].")
  }

  # extract ID ----
  id_dup_pair <- data.frame()

  for (ii in 1:length(ls_df)){
    count <- sum((is.na(ls_df_simi[[ii]]$abstract_simi) & ls_df_simi[[ii]]$title_simi >= cutoff_title)  |  ls_df_simi[[ii]]$title_simi >= cutoff_title  |   (!is.na(ls_df_simi[[ii]]$abstract_simi) & ls_df_simi[[ii]]$abstract_simi >= cutoff_abstract))

    if (count > 0) {
      temp <- ls_df_simi[[ii]][which((is.na(ls_df_simi[[ii]]$abstract_simi) & ls_df_simi[[ii]]$title_simi >= cutoff_title)  |  ls_df_simi[[ii]]$title_simi >= cutoff_title  |   (!is.na(ls_df_simi[[ii]]$abstract_simi) & ls_df_simi[[ii]]$abstract_simi >= cutoff_abstract)), c("id_r1", "id_r2")]
      temp$partition_no <- ii
      temp$partition_code <- ls_df[[ii]]$partition[1]

      id_dup_pair <- rbind(id_dup_pair, temp)
    }
  }

  # reset rownames
  rownames(id_dup_pair) <- NULL

  id_dup_pair <- id_dup_pair %>% dplyr::mutate(dplyr::across(c(id_r1, id_r2, partition_no), as.numeric))

  # re-order the columns
  id_dup_pair <- id_dup_pair[ , c("partition_no", "partition_code", "id_r1", "id_r2")]

  return(id_dup_pair)

}
