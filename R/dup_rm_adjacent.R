#' Remove duplicates between adjacent rows
#'
#' @description This function ensures that the most recent record be kept. When a peer-reviewed publication co-exists with a preprint or a conference proceeding, the peer-reviewed version will be kept.
#'
#' @param df A data frame (i.e., output #1 of [RefDeduR::decision_tree_adj()])
#' @param id_dup_pair A data frame listing `id` of potential duplicate pairs (i.e., output #2 of [RefDeduR::decision_tree_adj()])
#'
#' @return The input `df` with duplicates removed.
#' @export
#'
#' @examples
#' \dontrun{
#' df_2 <- dup_rm_adj(df, id_dup_pair)
#' }
dup_rm_adj <- function(
    df,
    id_dup_pair
  ){
  # check ----
  if(missing(df)){
    stop("'df' is missing: Please provide a data frame.")
  }
  if(missing(id_dup_pair)){
    stop("'id_dup_pair' is missing: Please provide the data frame listing `id` of duplicate pairs.")
  }
  if(!all(c("journal_norm", "year") %in% colnames(df))){
    stop('Necessary columns are missing.\n
         Please make sure df has the following columns: c("journal_norm", "year").')
  }
  if(!all(c("id_r1", "id_r2", "decision") %in% colnames(id_dup_pair))){
    stop('Necessary columns are missing.\n
         Please make sure id_dup_pair has the following columns: c("id_r1", "id_r2", "decision").')
  }



  for (ii in which(id_dup_pair_adj$decision == "duplicate")) {

    r1 <- which(df$id == id_dup_pair_adj$id_r1[ii])
    r2 <- which(df$id == id_dup_pair_adj$id_r2[ii])

    if (any(grepl("xiv|preprint", c(df$journal_norm[r1]), ignore.case = T)) | any(grepl("conference", df[r1, which(colnames(df) %in% c("booktitle", "publisher", "note"))], ignore.case = T))) {
      df <- df[-r1, ]
    } else if (any(grepl("xiv|preprint", c(df$journal_norm[r2]), ignore.case = T)) | any(grepl("conference", df[r2, which(colnames(df) %in% c("booktitle", "publisher", "note"))], ignore.case = T))){
      df <- df[-r2, ]
    } else if (any(is.na(df$year[c(r1, r2)]))){
      if (is.na(df$year[c(r2)])) {
        df <- df[-r2, ]
      } else {
        df <- df[-r1, ]  # if both NA, remove r2
      }
    } else if (df$year[r1] >= df$year[r2]) {
      df <- df[-r2, ]
    } else {
      df <- df[-r1, ]
    }

  }

  return(df)
}
