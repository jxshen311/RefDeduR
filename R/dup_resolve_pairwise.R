#' Manually resolve potential duplicate pairs requiring "check"
#' @description Change decision to "duplicate" or "not duplicate" according to manual review results.
#' @param id_dup_pair A data frame listing record id and partition id of duplicate pairs after getting automatic decisions (i.e., output of [RefDeduR::decision_tree_pairwise()]).
#' @param df_check A data frame of duplicate pairs for manual review (i.e., output of [RefDeduR::dup_screen_pairwise()]).
#' @param match_index Numeric: a vector of `"match"` numbers to change the decision for.
#' @param result Character: "duplicate" or "not duplicate".
#'
#' @return A data frame: the input `id_dup_pair` with `"decision"` column modified accordingly.
#' @export
#'
#' @examples
#' \dontrun{
#' id_dup_pair <- dup_resolve_pairwise(
#' id_dup_pair,
#' df_check,
#' match_index = c(1, 2, 3, 4),
#' result = "not duplicate")
#' }
dup_resolve_pairwise <- function(
    id_dup_pair,
    df_check,
    match_index,
    result # "duplicate", "not duplicate"
  ){
  # check ----
  if(missing(id_dup_pair)){
    stop("'id_dup_pair' is missing: Please provide the data frame listing record id and partition id of duplicate pairs.")
  }
  if(missing(df_check)){
    stop("'df_check' is missing: Please provide the data frame for manual review.")
  }
  if(missing(result)){
    stop('"result" is missing: Please specify whether the decision needs to be changed to "duplicate" or "not duplicate".')
  }
  if(!(all(result %in% c("duplicate", "not duplicate")) & (length(result) == 1))){
    stop('result must be "duplicate" or "not duplicate".')
  }

  match_index <- as.numeric(match_index)

  for (ii in match_index){
    id_r1 <- df_check$id[min(which(df_check$match == ii))]
    id_r2 <- df_check$id[max(which(df_check$match == ii))]
    partition_code <- df_check$partition[min(which(df_check$match == ii))]

    id_dup_pair$decision[which(id_dup_pair$id_r1 == id_r1 & id_dup_pair$id_r2 == id_r2 & id_dup_pair$partition_code == partition_code)] <- result
  }

  return(id_dup_pair)

}
