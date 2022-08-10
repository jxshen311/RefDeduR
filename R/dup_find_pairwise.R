dup_find_fuzzy_pairwise <- function(
    df,
    df_simi,
    cutoff_title = 0.7,
    cutoff_abstract = 0.7){
  # check ----

  ## extract ID ----
  index_pair <- data.frame()

  for (ii in 1:nlevels(b4$partition)){
    count <- sum((is.na(ls_b4_sim[[ii]]$abstract_simi) & ls_b4_sim[[ii]]$title_simi >= 0.6)  |  ls_b4_sim[[ii]]$title_simi >= 0.6  |   (!is.na(ls_b4_sim[[ii]]$abstract_simi) & ls_b4_sim[[ii]]$abstract_simi >= 0.6))

    if (count > 0) {
      temp <- ls_b4_sim[[ii]][which((is.na(ls_b4_sim[[ii]]$abstract_simi) & ls_b4_sim[[ii]]$title_simi >= 0.6)  |  ls_b4_sim[[ii]]$title_simi >= 0.6  |   (!is.na(ls_b4_sim[[ii]]$abstract_simi) & ls_b4_sim[[ii]]$abstract_simi >= 0.6)), c("Var1", "Var2")]
      temp$partition <- ii
      index_pair <- rbind(index_pair, temp)
    }
  }

  rownames(index_pair) <- NULL
  index_pair <- index_pair %>% mutate_if(is.factor, as.integer)

  index_pair <- index_pair %>% add_column(decision = NA)


}
