#' Output potential duplicates determined as requiring manual check by the decision tree
#'
#' @param ls_df A list of data frames containing the partitioned dataset  (i.e., output #1 of [RefDeduR::simi_ptn_pair()]).
#' @param id_dup_pair A data frame listing record id and partition id of duplicate pairs after getting automatic decisions (i.e., output of [RefDeduR::decision_tree_pairwise()]).
#'
#' @return A data frame of duplicate pairs for manual review. Pairing is indicated in `"match"` column.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df_check <- dup_screen_pairwise(ls_df, id_dup_pair)
#' }
dup_screen_pairwise <- function(
    ls_df,
    id_dup_pair
    ){
  # check ----
  if(missing(ls_df)){
    stop("'ls_df' is missing: Please provide a list of data frames containing the partitioned dataset.")
  }
  if(missing(id_dup_pair)){
    stop("'id_dup_pair' is missing: Please provide the corresponding data frame listing record id and partition id of duplicate pairs.")
  }
  if(!inherits(ls_df, "list")){
    stop("'ls_df' must be a list: Please check the data type using `class(ls_df)`.")
  }
  if(!all(c("partition_no", "id_r1", "id_r2", "decision") %in% colnames(id_dup_pair))){
    stop('Necessary columns are missing.\n
         Please make sure id_dup_pair has the following columns: c("partition_no", "id_r1", "id_r2", "decision").')
  }


  # if decision == check ----
  check <- which(id_dup_pair$decision == "check")

  df_check <- data.frame()

  for (ii in check){
    partition <- id_dup_pair$partition_no[ii]
    r1 <- id_dup_pair$id_r1[ii]
    r2 <- id_dup_pair$id_r2[ii]

    r1_row <- which(ls_df[[partition]]$id == r1)
    r2_row <- which(ls_df[[partition]]$id == r2)

    df_check <- dplyr::bind_rows(df_check,
                                 ls_df[[partition]][c(r1_row, r2_row), ])
  }

  df_check$match <- rep(1:(nrow(df_check)/2), each=2)

  # re-order columns
  df_check <- df_check %>%
    relocate(match, .before = everything()) %>%
    relocate(partition, id, .after = last_col())

  return(df_check)

}
