#' Remove duplicates in pairwise comparison
#' @description This function ensures that the most recent record be kept. When a peer-reviewed publication co-exists with a preprint or a conference proceeding, the peer-reviewed version will be kept.
#' @param ls_df A list of data frames containing the partitioned dataset  (i.e., output #1 of [RefDeduR::simi_ptn_pair()]).
#' @param id_dup_pair A data frame listing record id and partition id of duplicate pairs after resolving checked duplicates (i.e., output of [RefDeduR::dup_resolve_pairwise()]).
#' @param to_dataframe Logical: Should we merge the list of data frames into a single data frame? Defaults to `TRUE`.
#'
#' @return The input `ls_df` with duplicates removed. The resulted list of data frames are merged into a single data frame if `to_dataframe == TRUE`. Otherwise, a list of data frames is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' df_2 <- dup_rm_pairwise(ls_df, id_dup_pair, to_dataframe = TRUE)
#' # or
#' ls_df_2 <- dup_rm_pairwise(ls_df, id_dup_pair, to_dataframe = FALSE)
#' }
dup_rm_pairwise <- function(
    ls_df,
    id_dup_pair,
    to_dataframe = TRUE
  ){
  # check ----
  if(missing(ls_df)){
    stop("'ls_df' is missing: Please provide a list of data frames containing the partitioned dataset.")
  }
  if(missing(id_dup_pair)){
    stop("'id_dup_pair' is missing: Please provide the data frame listing record id and partition id of duplicate pairs.")
  }
  if(!inherits(ls_df, "list")){
    stop("'ls_df' must be a list: Please check the data type using `class(ls_df)`.")
  }
  if(!all(c("partition_no", "id_r1", "id_r2", "decision") %in% colnames(id_dup_pair))){
    stop('Necessary columns are missing.\n
         Please make sure id_dup_pair has the following columns: c("partition_no", "id_r1", "id_r2", "decision").')
  }



  # remove duplicates from ls_df
  # ** if decision == "not duplicate", do nothing
  # ** if decision == "duplicate", delete one of the records
  for (ii in which(id_dup_pair$decision == "duplicate")) {

    partition <- id_dup_pair$partition_no[ii]
    r1 <- which(ls_df[[partition]]$id == id_dup_pair$id_r1[ii])
    r2 <- which(ls_df[[partition]]$id == id_dup_pair$id_r2[ii])

    if (any(grepl("xiv|preprint", c(ls_df[[partition]]$journal_norm[r1]), ignore.case = T)) | any(grepl("conference", ls_df[[partition]][r1, which(colnames(ls_df[[partition]]) %in% c("booktitle", "publisher", "note"))], ignore.case = T))) {
      ls_df[[partition]] <- ls_df[[partition]][-r1, ]
    } else if (any(grepl("xiv|preprint", c(ls_df[[partition]]$journal_norm[r2]), ignore.case = T)) | any(grepl("conference", ls_df[[partition]][r2, which(colnames(ls_df[[partition]]) %in% c("booktitle", "publisher", "note"))], ignore.case = T))){
      ls_df[[partition]] <- ls_df[[partition]][-r2, ]
    } else if (any(is.na(ls_df[[partition]]$year[c(r1, r2)]))){
      if (is.na(ls_df[[partition]]$year[c(r1)])) {
        ls_df[[partition]] <- ls_df[[partition]][-r1, ]
      } else {
        ls_df[[partition]] <- ls_df[[partition]][-r2, ]  # if both NA, remove r1
      }
    } else if (ls_df[[partition]]$year[r1] <= ls_df[[partition]]$year[r2]) {
      ls_df[[partition]] <- ls_df[[partition]][-r1, ]
    } else {
      ls_df[[partition]] <- ls_df[[partition]][-r2, ]
    }

  }

  if (to_dataframe == TRUE) {
    df <- do.call("rbind", ls_df)
    rownames(df) <- NULL
    return(df)
  } else {
    return(ls_df)
  }


}
