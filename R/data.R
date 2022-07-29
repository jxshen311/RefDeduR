#' Sample data frames containing bibliographic information
#'
#' Data frames containing bibliographic information imported from BibTeX files (file.bib) by [revtools::read_bibliography()]
#'
#'
#' @format Data frames containing bibliographic information with variables such as title, author, journal, volume, abstract. Data type of all columns is character.
#'
#' `bib_example_complete` contains 6384 rows. `bib_example_small` is a subset of `bib_example_complete` and contains only 12 rows.
#'
#' @name RefDeduR_example_dataset
#' @source `bib_example_complete` contains all bibliographic records from searching indoor surface microbiome studies on PubMed, Web of Science, and Scopus on 2022-01-10.
NULL


#' @rdname RefDeduR_example_dataset
#' @format NULL
"bib_example_complete"

#' @rdname RefDeduR_example_dataset
#' @format NULL
"bib_example_small"
