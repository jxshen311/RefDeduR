#' Make decisions for potential duplicates
#'
#' @description Decisions are made by the decision tree for potential duplicates identified by [RefDeduR::dup_find_fuzzy_adj()].
#'
#' Decisions are added to the `"decision"` column in `id_dup_pair`. There are 3 levels of decisions, "duplicate", "not duplicate", and "check". If "not duplicate"...
#' @param df A data frame (i.e., output #1 of [RefDeduR::dup_find_fuzzy_adj()]).
#' @param id_dup_pair A data frame listing `id` of potential duplicate pairs (i.e., output #2 of [RefDeduR::dup_find_fuzzy_adj()])
#'
#' @return Two data frames. (1) `df` with `"match"` column modified according to the decision tree; (2) `id_dup_pair` with `"decision"` column added.
#' @export
#'
#' @examples
#' \dontrun{
#' c(df, id_dup_pair) %<-% decision_tree_adj(df, id_dup_pair)
#' }
decision_tree_adj <- function(
    df,
    id_dup_pair
){


  id_dup_pair <- id_dup_pair %>% tibble::add_column(decision = NA)


  # * remove duplicates + preprints + conference papers ----
  # ** decision tree
  for (ii in 1:nrow(id_dup_pair)){

    # extract id of record 1
    jj <- id_dup_pair$id_r1[ii]

    # prepare evaluation criteria
    yes_author_simi <- simi_edit(df$author_norm[jj], df$author_norm[jj+1]) >= 0.8 # TRUE, FALSE, NA

    yes_first_author <- simi_edit(df$first_author_last_name_norm[jj], df$first_author_last_name_norm[jj+1]) == 1  # TRUE, FALSE, NA

    yes_year <- abs(df$year[jj] - df$year[jj+1]) ==  0 | any(grepl("xiv|preprint", c(df$journal_norm[jj], df$journal_norm[jj+1]), ignore.case = T)) | any(grepl("conference", df[c(jj, jj+1), which(colnames(df) %in% c("booktitle", "publisher", "note"))], ignore.case = T))   # TRUE, FALSE, NA

    yes_journal <- simi_edit(df$journal_initialism[jj], df$journal_initialism[jj+1]) == 1 | any(grepl("xiv|preprint", c(df$journal_norm[jj], df$journal_norm[jj+1]), ignore.case = T)) | any(grepl("conference", df[c(jj, jj+1), which(colnames(df) %in% c("booktitle", "publisher", "note"))], ignore.case = T))  # TRUE, FALSE, NA


    # Decision tree
    # is.na() must be the first criteria in if
    if (is.na(yes_author_simi)) {
      if (is.na(yes_year)) {
        # print("check")
        # df$decision[c(jj, jj+1)] <- "check"
        id_dup_pair$decision[ii] <- "check"
      } else if (yes_year){
        if (is.na(yes_journal) | !yes_journal) {
          # print("check")
          # df$decision[c(jj, jj+1)] <- "check"
          id_dup_pair$decision[ii] <- "check"
        } else {
          # print("dup")
          # df$decision[jj] <- "duplicate"
          id_dup_pair$decision[ii] <- "duplicate"
        }
      } else {
        # print("not_dup")
        df$match[jj+1] <- max(df$match) + 1
        id_dup_pair$decision[ii] <- "not duplicate"
      }
    } else if (yes_author_simi){
      if (yes_first_author) {
        if (is.na(yes_year) & is.na(yes_journal)) {
          # print("check")
          # df$decision[c(jj, jj+1)] <- "check"
          id_dup_pair$decision[ii] <- "check"
        } else if ((yes_year & yes_journal ) | (yes_year & is.na(yes_journal)) | (is.na(yes_year) & yes_journal )) {
          # print("dup")
          # df$decision[jj] <- "duplicate"
          id_dup_pair$decision[ii] <- "duplicate"
        } else {
          # print("check")
          # df$decision[c(jj, jj+1)] <- "check"
          id_dup_pair$decision[ii] <- "check"
        }
      } else {
        # print("check")
        # df$decision[c(jj, jj+1)] <- "check"
        id_dup_pair$decision[ii] <- "check"
      }
    } else {
      if (yes_first_author) {
        # 3.1 FA yes
        if (is.na(yes_year)) {
          # print("check")
          # df$decision[c(jj, jj+1)] <- "check"
          id_dup_pair$decision[ii] <- "check"
        } else if (yes_year) {
          if (is.na(yes_journal) | !yes_journal ) {
            # print("check")
            # df$decision[c(jj, jj+1)] <- "check"
            id_dup_pair$decision[ii] <- "check"
          } else {
            # print("dup")
            # df$decision[jj] <- "duplicate"
            id_dup_pair$decision[ii] <- "duplicate"
          }
        } else {
          if ( is.na(yes_journal) | !yes_journal ) {
            # print("not_dup")
            df$match[jj+1] <- max(df$match) + 1
            id_dup_pair$decision[ii] <- "not duplicate"
          } else {
            # print("check")
            # df$decision[c(jj, jj+1)] <- "check"
            id_dup_pair$decision[ii] <- "check"
          }
        }
      } else if (!yes_first_author) {
        # 3.2 FA no
        if (is.na(yes_year)) {
          if ( is.na(yes_journal) | yes_journal ) {
            # print("check")
            # df$decision[c(jj, jj+1)] <- "check"
            id_dup_pair$decision[ii] <- "check"
          } else {
            # print("not_dup")
            df$match[jj+1] <- max(df$match) + 1
            id_dup_pair$decision[ii] <- "not duplicate"
          }
        } else if (yes_year) {
          if ( is.na(yes_journal) | !yes_journal ) {
            # print("not_dup")
            df$match[jj+1] <- max(df$match) + 1
            id_dup_pair$decision[ii] <- "not duplicate"
          } else {
            # print("check")
            # df$decision[c(jj, jj+1)] <- "check"
            id_dup_pair$decision[ii] <- "check"
          }
        } else {
          # print("not_dup")
          df$match[jj+1] <- max(df$match) + 1
          id_dup_pair$decision[ii] <- "not duplicate"
        }
      }
    }
  }


  return(list(df, id_dup_pair))
  # ** remove dup records, keep check records ----
  #...............................................................................
  # previous version
  # This is easier, but when the publication year of the preprint or conference proceeding is the same as the peer-reviewed paper, the peer-reviewed paper might be removed.
  # order by match -> year
  # order so that the most recent version will be kept
  # df <- df[with(df, order(match, -year)), ]
  #
  # df_inter <- df %>%
  #   dplyr::filter(!duplicated(match) | !is.na(decision))
  #...............................................................................

}
