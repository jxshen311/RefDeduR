
# result: calculate similarity - pairwise after partitioning: 1436.371 sec elapsed
#' Calculate pairwise string similarity
#'
#' @description The function calculates pairwise similarity based on Levenshtein edit distance for columns `"title_norm"` and `"abstract_norm"` between records within the same group after partitioning. Range of similarity is \[0, 1\]. `Similarity == 1` means 100% identical while `Similarity == 0` means completely different.
#'
#' @param df A data frame with bibliographic information that has gone through text normalization. `df` must have the following columns `c("title_norm", "abstract_norm")`.
#' @param partition_by Quoted name of the column by which to partition the rows. Defaults to `"first_two_letters_first_author_last_name"`. Can be `FALSE` if prefer not to partition, in which case all records are compared against all others.
#'
#' Besides the default, `"year"` is another popular partitioning parameter. We recommend the default method if papers in your dataset are not evenly distributed across years. For instance, if most papers are recent, the dafault method will be much more efficient than `"year"`. Additionally, with the prevalence of preprints, partitioning by `"year"` becomes less accurate.
#'
#' In addition, you can also construct a custom "partition" column.
#'
#' @details An artificial code "00" is assigned to cells with missing values in the `partition_by` column and these rows are partitioned into one group. If you customize your own partitioning parameter, try to avoid this artificial code.
#'
#' Computing time estimation according to past experience: ~ 23 min for a data frame with 3837 rows on a Macbook Pro (Apple M1 Pro chip basic model, memory: 16 GB).
#'
#' @return Two list of data frames. (1) A list of data frames containing the partitioned `df`; (2) A list of data frames with string similarity results for `"title_norm"` and `"abstract_norm"`.
#' @export
#'
#' @examples
#' \dontrun{
#' # load example dataset
#' data(bib_example_small)
#'
#' # text normalization of the data frame
#' df <- norm_df(bib_example_small)
#'
#' # calculate similarity
#' c(ls_df, ls_df_simi) %<-% simi_ptn_pair(df, partition_by = "first_two_letters_first_author_last_name")
#' }
simi_ptn_pair <- function(
    df,
    partition_by = "first_two_letters_first_author_last_name" # "year", construct a custom "partition" column, FALSE
){# check df ----
  if(missing(df)){
    stop("'Data frame' is missing: Please provide a data frame.")
  }
  if(!inherits(df, "data.frame")){
    stop("'df' must be a data frame: Please check the data type using `class(df)`.")
  }
  if(!all(c("title_norm", "abstract_norm") %in% colnames(df))){
    stop('Necessary columns are missing.\n
         Please make sure all the following columns exist:\n
         c("title_norm", "abstract_norm")')
  }


  # partition (author not NA) ----
  # check partition_by


  # treat different partitioning scenarios
  if(partition_by == FALSE){
    df$partition <- as.factor("00")
  }
  else if(partition_by == "first_two_letters_first_author_last_name"){
    if(!("first_author_last_name_norm" %in% colnames(df))){
      stop('Column "first_author_last_name_norm" is missing in the data frame.')
    }

    df$first_author_last_name_norm[which(is.na(df$first_author_last_name_norm))] <- "00"  # assign "00" to where NA
    df$partition <- as.factor(substring(df$first_author_last_name_norm, 1, 2))
  }
  else {
    if(!(partition_by %in% colnames(df))){
      stop('Column to partition_by is missing in the data frame.')
    }

    df[[partition_by]][which(is.na(df[[partition_by]]))] <- "00"  # assign "00" to where NA
    df$partition <- as.factor(df[[partition_by]])
  }

  # create list
  ls_df <- list()
  ls_df_simi <- list()

  # partition and store in the list
  for (ii in 1:nlevels(df$partition)){
    ls_df[[ii]] <- dplyr::filter(df, partition == levels(df$partition)[ii])

    rownames(ls_df[[ii]]) <- NULL  # reset row names

    # assign "id"
    ls_df[[ii]] <- tibble::rownames_to_column(ls_df[[ii]], var = "id")
    ls_df[[ii]]$id <- as.integer(ls_df[[ii]]$id)
    }


  # calculate title and abstract similarity ----
  # 5451.588 sec elapsed on my old mac -> recommend running on HPC is available
  for (ii in 1:nlevels(df$partition)){
    temp_simi_ti <- simi_edit_pairwise(ls_df[[ii]]$title_norm, "title_simi")
    temp_simi_ab <- simi_edit_pairwise(ls_df[[ii]]$abstract_norm, "abstract_simi")
    ls_df_simi[[ii]] <- dplyr::full_join(temp_simi_ti, temp_simi_ab, by = c("Var1", "Var2"))

    # rename "Var1", "Var2" to more self-explanatory texts (base function is faster)
    colnames(ls_df_simi[[ii]])[colnames(ls_df_simi[[ii]]) == "Var1"] <- "id_r1"
    colnames(ls_df_simi[[ii]])[colnames(ls_df_simi[[ii]]) == "Var2"] <- "id_r2"
  }

  return(list(ls_df, ls_df_simi))

}
