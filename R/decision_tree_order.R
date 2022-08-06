decision_tree_order <- function(
    df,
    df_simi,
    cutoff_title = 0.7,
    cutoff_abstract = 0.7
){
  # Assign numbers ----
  df$match <- integer(nrow(b3))
  df$match[1] <- 1

  for (ii in 1:(nrow(df)-1)) {
    if(((is.na(df_simi$abstract_simi[ii]) & df_simi$title_simi[ii] >= cutoff_title)  |  df_simi$title_simi[ii] >= cutoff_title  |   (!is.na(df_simi$abstract_simi[ii]) & df_simi$abstract_simi[ii] >= cutoff_abstract))) {
      df$match[ii+1] <- df$match[ii]
    } else {
      df$match[ii+1] <- df$match[ii]+1
    }
  }


  # Apply decision tree rules on matched records ----
  index  <- which(duplicated(df$match))-1   # number of index = 9

  b3 <- b3 %>% add_column(decision = NA)

  #// save(b3, b3_simi, file = "develop/b3.RData")

  # * remove duplicates + preprints + conference papers ----
  # ** decision tree
  for (ii in index){
    # prepare evaluation criteria
    yes_author_simi <- df_simi$author_simi[ii] >= 0.8 # TRUE, FALSE, NA

    yes_first_author <- df_simi$first_author_last_name_simi[ii] == 1  # TRUE, FALSE, NA

    yes_year <- abs(df$year[ii] - df$year[ii+1]) ==  0 | any(grepl("xiv|preprint", c(df$journal_norm[ii], df$journal_norm[ii+1]), ignore.case = T)) | any(grepl("conference", b3[c(ii, ii+1), which(colnames(b3) %in% c("booktitle", "publisher", "note"))], ignore.case = T))   # TRUE, FALSE, NA

    yes_journal <- simi_edit_dis(df$journal_prefix[ii], df$journal_prefix[ii+1]) == 1 | any(grepl("xiv|preprint", c(df$journal_norm[ii], df$journal_norm[ii+1]), ignore.case = T)) | any(grepl("conference", b3[c(ii, ii+1), which(colnames(b3) %in% c("booktitle", "publisher", "note"))], ignore.case = T))  # TRUE, FALSE, NA


    # Decision tree
    # is.na() must be the first criteria in if
    if (is.na(yes_author_simi)) {
      if (is.na(yes_year)) {
        print("check")
        df$decision[c(ii, ii+1)] <- "check"
      } else if (yes_year){
        if (is.na(yes_journal) | !yes_journal) {
          print("check")
          df$decision[c(ii, ii+1)] <- "check"
        } else {
          print("dup")
          df$decision[ii] <- "duplicate"
        }
      } else {
        print("not_dup")
        df$match[ii+1] <- max(df$match) + 1
      }
    } else if (yes_author_simi){
      if (yes_first_author) {
        if (is.na(yes_year) & is.na(yes_journal)) {
          print("check")
          df$decision[c(ii, ii+1)] <- "check"
        } else if ((yes_year & yes_journal ) | (yes_year & is.na(yes_journal)) | (is.na(yes_year) & yes_journal )) {
          print("dup")
          df$decision[ii] <- "duplicate"
        } else {
          print("check")
          df$decision[c(ii, ii+1)] <- "check"
        }
      } else {
        print("check")
        df$decision[c(ii, ii+1)] <- "check"
      }
    } else {
      if (yes_first_author) {
        # 3.1 FA yes
        if (is.na(yes_year)) {
          print("check")
          df$decision[c(ii, ii+1)] <- "check"
        } else if (yes_year) {
          if (is.na(yes_journal) | !yes_journal ) {
            print("check")
            df$decision[c(ii, ii+1)] <- "check"
          } else {
            print("dup")
            df$decision[ii] <- "duplicate"
          }
        } else {
          if ( is.na(yes_journal) | !yes_journal ) {
            print("not_dup")
            df$match[ii+1] <- max(df$match) + 1
          } else {
            print("check")
            df$decision[c(ii, ii+1)] <- "check"
          }
        }
      } else if (!yes_first_author) {
        # 3.2 FA no
        if (is.na(yes_year)) {
          if ( is.na(yes_journal) | yes_journal ) {
            print("check")
            df$decision[c(ii, ii+1)] <- "check"
          } else {
            print("not_dup")
            df$match[ii+1] <- max(df$match) + 1
          }
        } else if (yes_year) {
          if ( is.na(yes_journal) | !yes_journal ) {
            print("not_dup")
            df$match[ii+1] <- max(df$match) + 1
          } else {
            print("check")
            df$decision[c(ii, ii+1)] <- "check"
          }
        } else {
          print("not_dup")
          df$match[ii+1] <- max(df$match) + 1
        }
      }
    }
  }

  # ** remove dup records, keep check records ----
  # previous version
  # This is easier, but when the publication year of the preprint or conference proceeding is the same as the peer-reviewed paper, the peer-reviewed paper might be removed.
  # order by match -> year
  # order so that the most recent version will be kept
  # b3 <- b3[with(b3, order(match, -year)), ]
  #
  # b3_inter <- b3 %>%
  #   dplyr::filter(!duplicated(match) | !is.na(decision))


  # write.xlsx(b3[c(653, 654, 2412,2413, 3265, 3266), ], file = "dedu_develop_check/alphabetic_remove_duplicate_verify.xlsx")

}
