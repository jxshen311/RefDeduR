# deduplicate based on doi ----
# only deduplicate on the subset with DOI
dedu_exact_match <- function(
    df,
    based_on = c("doi_norm", "title", "title_norm"),
    manual_check = TRUE
    ){
  # 0. check df ----
  if(missing(df)){
    stop("'Data frame' is missing: Please provide a data frame")
  }
  else if(!inherits(df, "data.frame")){
    stop("'df' must be a data frame: Please check the data type using `class(df)`")
  }
  if(!all(c("author", "title", "journal", "abstract", "year", "doi") %in% colnames(df))){
    stop('Necessary columns are missing.\n
         Please make sure all the following 6 columns exist:\n
         c("author", "title", "journal", "abstract", "year", "doi")')
  }



  # 1. based_on includes doi_norm ----
  if("doi_norm" %in% based_on){
    # check necessary information
    if(!("doi_norm" %in% colnames(df))){
      stop('Column "doi_norm" is missing')
    }

    # subset df according to the existence of doi
    df_doi_no <- df[is.na(df$doi_norm), ]
    df_doi_yes <- df[!is.na(df$doi_norm), ]

    # order (so that the most recent version will be kept) and remove duplicates
    df_doi_yes <- df_doi_yes[with(df_doi_yes, order(doi_norm, -year)), ]
    df_doi_yes_uni <- df_doi_yes[!duplicated(df_doi_yes$doi_norm), ]

    # combine with records without doi
    df <- rbind(df_doi_yes_uni, df_doi_no)
  }



  # 2. based_on includes title ----
  if("title" %in% based_on){
    # subset df according to the existence (normally all records have title)
    df_ti_no <- df[is.na(df$title), ]
    df_ti_yes <- df[!is.na(df$title), ]

    # order (so that the most recent version will be kept) and remove duplicates
    df_ti_yes <- df_ti_yes[with(df_ti_yes, order(title, -year)), ]
    df_ti_yes_uni <- df_ti_yes[!duplicated(df_ti_yes$title), ]

    # combine with records without title
    df <- rbind(df_ti_yes_uni, df_ti_no)
  }



  # 3. based_on includes title_norm ----
  if("title_norm" %in% based_on){
    # check necessary information
    if(!("title_norm" %in% colnames(df))){
      stop('Column "title_norm" is missing')
    }

    # subset df according to the existence (normally all records have title)
    df_tinorm_no <- df[is.na(df$title_norm), ]
    df_tinorm_yes <- df[!is.na(df$title_norm), ]


    # order so that the most recent version will be kept
    b2 <- b2[with(b2, order(-year)), ]

    b2$match <- as.numeric(factor(b2$title_norm, ))

    # 1. If title_norm and first_author_last_name_norm are both the same, remove duplicates without review ----
    # 2. When title_norm is the same but first_author_last_name_norm is different, output the dataframe for manual review ----
    b2_manual_check <- b2 %>%
      group_by(title_norm) %>%
      filter(dplyr::n_distinct(first_author_last_name_norm) >= 2)

    # either review in dataframe format (preview in R or write.xlsx()) or call revtools shiny app
    # call revtools shiny app
    # screen_duplicates(b2_manual_check)

    # we can use the override_duplicates() function in synthesisr to manually mark records as unique
    # new_match_vector <- synthesisr::override_duplicates(match number vector, the match number to override)
    # test$match_new <- synthesisr::override_duplicates(test$match, 3825)



    # remove duplicates
    b3 <- b2[!duplicated(b2$match), ]
    # or
    # b3 <- synthesisr::extract_unique_references(b2, b2$match) # this does remove the second row hit

    # remove helper columns
    b3 <- select(b3, -match)




  }

}
