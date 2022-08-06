dup_find_fuzzy <- function(
    method = "order", # "order" or "pa
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
