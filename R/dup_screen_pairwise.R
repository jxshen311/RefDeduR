#' Title
#'
#' @param ls_df
#' @param id_dup_pair
#'
#' @return
#' @export
#'
#' @examples
dup_screen_pairwise <- function(
    ls_df,
    id_dup_pair
    ){
  # if decision == check ----
  # can call revtools shiny app for visulization, but don't modify directly using revtools
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

  return(df_check)
}
