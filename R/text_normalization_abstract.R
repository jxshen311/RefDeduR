#' Clean and normalize abstract in bibliography
#'
#' For abstract, we do the following string normalization.
#' - Remove tailing information such as ". (C) 1998 International Astronautical Federation Published by Elsevier Science Ltd. All rights reserved." and "(C) 2000 Elsevier Science B.V. All rights reserved." according to 6 patterns observed empirically, to reduce the effect of the information.
#' - convert letters to lowercase\
#' - remove whitespace from start and end of string; also reduce repeated whitespace inside the string.
#'
#' @param abstract A character vector (e.g., a column in a data frame)
#' @param first_author_last_name A character vector containing last name of first author, or `FALSE`.
#'
#' If not `FALSE`, index of `abstract` and `first_author_last_name` must be the same for the same bibliographic record. This should not be an issue if analyzing based on a data frame.
#'
#' Last name of first author is used as one of the patterns to remove irrelevant information from abstract and clean the text. An example of the information being removed includes ". (c) Daisuke Fujiwara et al., 2021;"
#'
#' If `FALSE`, abstract normalization according to this pattern will be bypassed.
#'

#'
#' @return Normalized character vector
#' @export
#'
#' @example inst/examples/text_normalization_abstract.R
#'
norm_abstract <- function(abstract, first_author_last_name){
  # Define a custom function that will be used for several times ----
  # F: remove everything from the last "string_detect" on
  remove_from_on <- function(string_detect, text){
    result <- gsub(paste0("(.*)\\b", string_detect, "\\b.*"), "\\1", text, ignore.case = FALSE)

    return(result)
  }




  # 1. Remove tailing information ----
  # * Pattern 1: remove from last occurrence of the pattern ----
  abstract_norm <- gsub(paste0("(.*[^;,])[ ]?[(]c[)][ ]?[(]?\\d{4}[)]?[ ]?\\b.*"), "\\1", abstract, ignore.case = T)
  # pattern example:
  # ". (C) 2000 Elsevier Science B.V. All rights reserved."
  # ". (C) 2015 IAA. Published by Elsevier Ltd. All rights reserved."
  # ". (C) 1998 International Astronautical Federation Published by Elsevier Science Ltd. All rights reserved."
  # ". (c) (2014) Trans Tech Publications, Switzerland."
  # " (c) 2018, Public Health Services, US Dept of Health and Human Services. All rights reserved."
  # Since only the last occurrence is removed, abstract content in cases like "] (c) 2019 The Author(s) The drivers of ocean plankton diversity across archaea, bacteria, eukaryotes, and major virus clades are inferred from both molecular and imaging data acquired by the Tara Oceans project and used to predict the effects of severe warming of the surface ocean on this critical ecosystem by the end of the 21st century. (c) 2019 The Author(s)" won't be over deleted.
  # Also, because of this, repeated patterns like "In this study, we report the first characterization of IMP-1 and VIM-2 MBL-producing K. pneumoniae and E. coli isolates collected from Kasserine Hospital, Tunisia. (c) 2011 The Authors. APMIS (c) 2011 APMIS." will only be removed once.



  # * Pattern 2: "funding", “Copyright”, "This is an open access article" ----
  # ** Abstract containing “Funding|FUNDING” -> remove all from last occurrence ----
  abstract_norm <- remove_from_on("Funding", abstract_norm)
  abstract_norm <- remove_from_on("FUNDING", abstract_norm)


  # ** Abstract containing “Copyright” -> remove all from there ----
  # *** scenario 1 ----
  # removing order: pattern from strong to weak
  abstract_norm <- gsub(paste0("(.*)[ ]?", "[(][cC][)][ ]?Copyright", "\\b.*"), "\\1", abstract_norm, ignore.case = FALSE)

  abstract_norm <- remove_from_on("Crown Copyright", abstract_norm)

  abstract_norm <- remove_from_on("Copyright", abstract_norm)
  # pattern example:
  # ". Crown Copyright"
  # "viable bacteria Copyright"
  # ". (c)Copyright 2021 by the Infectious Diseases and Clinical Microbiology Specialty Society of Turkey."

  # *** scenario 2 ----
  abstract_norm <- gsub("(.*[^,;])[ ]?[(]c[)][,.]?[ ]?This article not subject to .* copyright law\\b.*", "\\1", abstract_norm, ignore.case = T)
  # pattern example:
  # "as well as xerophilic species that are typically not resolved using traditional culture methods. (c), This article not subject to U.S. copyright law."

  # *** scenario 3 ----
  # + remove from last [.] for only the selected rows
  index <- grepl(paste0("\\b", "copyright", "\\b"), abstract_norm, ignore.case = F)

  for (ii in which(index)){
    abstract_norm[ii] <- abstract_norm[ii] %>%
      {gsub(paste0("(.*)\\b", "copyright", "\\b.*"), "\\1", ., ignore.case = F)} %>%
      {gsub("(.*\\.).*", "\\1", .)}
  }
  # pattern example:
  # "This is an open access article, free of all copyright, and may be freely reproduced, distributed, transmitted, modified, built upon, or otherwise used by anyone for any lawful purpose. The work is made available under the Creative Commons CC0 public domain dedication."



  # * Pattern 3: “all rights reserved” ----
  abstract_norm <- gsub(paste0("(.*[^,;])[ ]?","[(]c[)][ ]?", "\\ball[ ]rights[ ]reserved\\b.*"), "\\1", abstract_norm, ignore.case = T)
  # pattern example:
  # ". (C) All Rights Reserved"

  abstract_norm <- remove_from_on("(?i)https://doi.org", abstract_norm)
  # pattern example:
  # ". https://doi.org/10.1289/EHP3145  2018, Public Health Services, US Dept of Health and Human Services. All rights reserved."

  abstract_norm <- gsub(paste0("(.*[^,;])[ ]?[(]c[)][ ]?","FEMS","[ ]\\d{4}\\b.*", "\\ball rights reserved\\b.*"), "\\1", abstract_norm, ignore.case = T)
  # pattern example:
  # ". (c) FEMS 2019. All rights reserved."



  # * Pattern 4: "published by" ----
  # THE ORDER MATTERS. First remove strong patterns, then the last one (anything start with "Published by")
  abstract_norm <- remove_from_on("Published by", abstract_norm)
  # pattern example:
  # ". Published by Elsevier Inc. on behalf of the American College of Allergy, Asthma & Immunology."

  abstract_norm <- remove_from_on("Published under", abstract_norm)
  # pattern example:
  # "Published under licence by IOP Publishing Ltd."



  # * Pattern 5: "the authors" ----
  abstract_norm <- gsub(paste0("(.*[^,;])[ ]?[(]c[)][ ]?", "\\bthe author[(]?s?[)]?\\b.*"), "\\1", abstract_norm, ignore.case = T)
  # pattern examples:
  # ". (c) The Author(s)."
  # ". (c) the author(s), publisher and licensee Libertas Academica Ltd."
  # ". (c) The Author(s) 2019."

  # NOTE
  # (.*[^,;])[ ]?[(]c[)][ ]? is used as a strong pattern indicator.
  # The following patterns were not removed because 1) it is risky to loosen the pattern to single space; 2) most can be removed by the above patterns; 3) the remaining string is relatively short and exert only a small effect
  # ". The Author 2013."
  # ". 2019. The Authors."
  # ". 2020 The Authors."



  # * Pattern 6: Abstract containing last name of first author ----
  if (any(first_author_last_name != FALSE)){
    first_author_last_name_norm <- norm_author(first_author_last_name)

    for (ii in 1:length(abstract)){
      abstract_norm[ii] <- gsub(paste0("(.*[^;,])[ ]?[(]c[)][ ]?", "(?:\\b[A-Za-z]+\\b[.]?[ ]){0,3}", first_author_last_name[ii], "\\b.*"), "\\1", abstract_norm[ii], ignore.case = T)

      abstract_norm[ii] <- gsub(paste0("(.*[^;,])[ ]?[(]c[)][ ]?", "(?:\\b[A-Za-z]+\\b[.]?[ ]){0,3}", first_author_last_name_norm[ii], "\\b.*"), "\\1", abstract_norm[ii], ignore.case = T)
    }
  }
  # pattern examples:
  # ". (c) Buse, et al."
  # ". (c) Daisuke Fujiwara et al., 2021; "

  # NOTE
  # A strong pattern (c) is used to avoid over deleting information. For example, author name may appear in the middle of abstract for some special articles.




  # 2. Other string normalization ----
  # tolower
  abstract_norm <- tolower(abstract_norm)

  # remove whitespace from start and end of string + reduce repeated whitespace inside a string.
  abstract_norm <- stringr::str_squish(abstract_norm)



  return(abstract_norm)
}
