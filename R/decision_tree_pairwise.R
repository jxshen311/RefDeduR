#' Make decisions for potential duplicates
#'
#' @param ls_df A list of data frames containing the partitioned dataset  (i.e., output #1 of [RefDeduR::simi_ptn_pair()]).
#' @param id_dup_pair A data frame listing record id and partition id of duplicate pairs (i.e., output of [RefDeduR::dup_find_fuzzy_pairwise()]).
#'
#' @return `id_dup_pair` with `"decision"` column added.
#' @export
#'
#' @examples
#' \dontrun{
#' id_dup_pair <- decision_tree_pairwise(ls_df, id_dup_pair)
#' }
decision_tree_pairwise <- function(
    ls_df,
    id_dup_pair
){
  id_dup_pair <- id_dup_pair %>% tibble::add_column(decision = NA)


  # Decision tree ----
  for (ii in 1:nrow(id_dup_pair)){
    partition <- id_dup_pair$partition_no[ii]
    r1 <- id_dup_pair$id_r1[ii]
    r2 <- id_dup_pair$id_r2[ii]

    # prepare evaluation criteria
    yes_author_simi <- simi_edit(ls_df[[partition]]$author_norm[r1], ls_df[[partition]]$author_norm[r2]) >= 0.8 # TRUE, FALSE, NA

    yes_first_author <- simi_edit(ls_df[[partition]]$first_author_last_name_norm[r1], ls_df[[partition]]$first_author_last_name_norm[r2]) == 1  # TRUE, FALSE, NA

    yes_year <- abs(ls_df[[partition]]$year[r1] - ls_df[[partition]]$year[r2]) ==  0 | any(grepl("xiv|preprint", c(ls_df[[partition]]$journal_norm[r1], ls_df[[partition]]$journal_norm[r2]), ignore.case = T)) | any(grepl("conference", ls_df[[partition]][c(r1, r2), which(colnames(ls_df[[partition]]) %in% c("booktitle", "publisher", "note"))], ignore.case = T))   # TRUE, FALSE, NA

    yes_journal <- simi_edit(ls_df[[partition]]$journal_initialism[r1], ls_df[[partition]]$journal_initialism[r2]) == 1 | any(grepl("xiv|preprint", c(ls_df[[partition]]$journal_norm[r1], ls_df[[partition]]$journal_norm[r2]), ignore.case = T)) | any(grepl("conference", ls_df[[partition]][c(r1, r2), which(colnames(ls_df[[partition]]) %in% c("booktitle", "publisher", "note"))], ignore.case = T))  # TRUE, FALSE, NA


    # Decision tree
    # is.na() must be the first criteria in if
    if (is.na(yes_author_simi)) {
      if (is.na(yes_year)) {
        # print("check")
        id_dup_pair$decision[ii] <- "check"
      } else if (yes_year){
        if (is.na(yes_journal) | !yes_journal) {
          # print("check")
          id_dup_pair$decision[ii] <- "check"
        } else {
          # print("dup")
          id_dup_pair$decision[ii] <- "duplicate"
        }
      } else {
        # print("not_dup")
        id_dup_pair$decision[ii] <- "not duplicate"
      }
    } else if (yes_author_simi){
      if (yes_first_author) {
        if (is.na(yes_year) & is.na(yes_journal)) {
          # print("check")
          id_dup_pair$decision[ii] <- "check"
        } else if ((yes_year & yes_journal ) | (yes_year & is.na(yes_journal)) | (is.na(yes_year) & yes_journal )) {
          # print("dup")
          id_dup_pair$decision[ii] <- "duplicate"
        } else {
          # print("check")
          id_dup_pair$decision[ii] <- "check"
        }
      } else {
        # print("check")
        id_dup_pair$decision[ii] <- "check"
      }
    } else {
      if (yes_first_author) {
        # 3.1 FA yes
        if (is.na(yes_year)) {
          # print("check")
          id_dup_pair$decision[ii] <- "check"
        } else if (yes_year) {
          if (is.na(yes_journal) | !yes_journal ) {
            # print("check")
            id_dup_pair$decision[ii] <- "check"
          } else {
            # print("dup")
            id_dup_pair$decision[ii] <- "duplicate"
          }
        } else {
          if ( is.na(yes_journal) | !yes_journal ) {
            # print("not_dup")
            id_dup_pair$decision[ii] <- "not duplicate"
          } else {
            # print("check")
            id_dup_pair$decision[ii] <- "check"
          }
        }
      } else if (!yes_first_author) {
        # 3.2 FA no
        if (is.na(yes_year)) {
          if ( is.na(yes_journal) | yes_journal ) {
            # print("check")
            id_dup_pair$decision[ii] <- "check"
          } else {
            # print("not_dup")
            id_dup_pair$decision[ii] <- "not duplicate"
          }
        } else if (yes_year) {
          if ( is.na(yes_journal) | !yes_journal ) {
            # print("not_dup")
            id_dup_pair$decision[ii] <- "not duplicate"
          } else {
            # print("check")
            id_dup_pair$decision[ii] <- "check"
          }
        } else {
          # print("not_dup")
          id_dup_pair$decision[ii] <- "not duplicate"
        }
      }
    }
  }

  return(id_dup_pair)

}
