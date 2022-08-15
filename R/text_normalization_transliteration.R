#' Transliterate a text file
#'
#' @description Transliterate a text file and output to another text file.
#'
#' @param file_input Directory/Path to the input file.
#' @param file_output Directory/Path to the out file. No need to create the file in advance. A file with the specified name will be created.
#' @param method A character vector specifying the transliteration rules in order. Defaults to a recommended rule combination - `c("greek_letter-name", "any-ascii")`. See [stringi::stri_trans_general()] documentation for more rules in the database. Customized rules are also allowed (see examples).
#' @param customized Logical. A logical vector (with the same length as `method`) specifying whether the corresponding transliteration rule is a customized rule or not. *Optional if none of the rules is customized (i.e., Defaults to an all-FALSE vector.).*
#'
#' @details Transliteration rule **`"greek_letter-name"`** transliterates common greek letters to their names (e.g., α to alpha, β to beta). See [RefDeduR::ls_greek_letter_to_name()] for the list of transformations.
#'
#'#//TODO: finish details: explain the recommended rules, write documents for ls_greek_letter_to_name
#'
#' All rules in [stringi::stri_trans_general()] are applicable as a rule in `method` here. documentation for more rules in the database. Customized rules are also allowed (see examples).
#'
#' rule explanation
#'
#'
#' @return NULL
#' @export
#'
#' @examples
#' # An example input file has been put in extdata/
#' # We use system.file() to output the path to this example file
#' input <- system.file("extdata", "example_norm_trans.bib", package = "RefDeduR")
#' # The input file looks like this.
#' #> author = {Whitman, C. P., Álvarez-Fraga, L. and Pérez, A á ä},
#' #> title = {β-α-β structural motif},
#' #> Ϛ
#'
#' # Specify the path to the output file. Here I put it in the same directory but you can modify the path to wherever you want to store the output file.
#' output <- system.file("extdata", "output.bib", package = "RefDeduR")
#'
#'
#' # Example 1: default setting
#' norm_transliteration(input, output, method = c("greek_letter-name", "any-ascii"))
#' # For demonstration purpose, output.bib will look like this.
#' #> author = {Whitman, C. P., Alvarez-Fraga, L. and Perez, A a a},
#' #> title = {beta-alpha-beta structural motif},
#' #> Ϛ
#'
#'
#' # Example 2: use other transliteration rules in the database
#' norm_transliteration(input, output, method = c("Greek-en_US/UNGEGN", "any-ascii"))
#' # For demonstration purpose, output.bib will look like this.
#' #> author = {Whitman, C. P., Alvarez-Fraga, L. and Perez, A a a},
#' #> title = {b-a-b structural motif},
#' #> Ϛ
#'
#'
#' # Example 3: use customized transliteration rules
#' id_custom <- "
#' \u03DA > 'Stigma';
#' \u03E0 > 'Sampi';
#' "  # Note that every transliteration needs to be accompanied by a semicolon, including the last line.
#'
#' norm_transliteration(input, output, method = c("greek_letter-name", id_custom, "any-ascii"), customized = c(FALSE, TRUE, FALSE))
#' # For demonstration purpose, output.bib will look like this.
#' #> author = {Whitman, C. P., Alvarez-Fraga, L. and Perez, A a a},
#' #> title = {beta-alpha-beta structural motif},
#' #> Stigma
#'
norm_transliteration <- function(
    file_input,
    file_output,
    method = c("greek_letter-name", "any-ascii"), # in order; could input customized rules
    customized
){# check ----
  if(!inherits(method, "character")){
    stop("'method' must be a character vector.")
  }

  fin  <- file(file_input, open = "r")
  fout  <- file(file_output, open = "w")

  # construct id
  if(missing(customized)){
    customized <- rep(FALSE, length(method))
  } else if(!inherits(customized, "logical")){
    stop("'customized' must be a logical vector.")
  } else if(length(customized) != length(method)){
      stop("Lengths of 'method' and 'customized' must be the same.")
    }

  id_greek_letter_name <- "
  \u0391 > 'Alpha';
  \u0392 > 'Beta';
  \u0393 > 'Gamma';
  \u0394 > 'Delta';
  \u0395 > 'Epsilon';
  \u0396 > 'Zeta';
  \u0397 > 'Eta';
  \u0398 > 'Theta';
  \u0399 > 'Iota';
  \u039A > 'Kappa';
  \u039B > 'Lamda';
  \u039C > 'M';
  \u039D > 'N';
  \u039E > 'Xi';
  \u039F > 'Omicron';
  \u03A0 > 'Pi';
  \u03A1 > 'Rho';
  \u03A3 > 'Sigma';
  \u03A4 > 'Ta';
  \u03A5 > 'Upsilon';
  \u03A6 > 'Phi';
  \u03A7 > 'Chi';
  \u03A8 > 'Psi';
  \u03A9 > 'Omega';
  \u03B1 > 'alpha';
  \u03B2 > 'beta';
  \u03B3 > 'gamma';
  \u03B4 > 'delta';
  \u03B5 > 'epsilon';
  \u03B6 > 'zeta';
  \u03B7 > 'eta';
  \u03B8 > 'theta';
  \u03B9 > 'iota';
  \u03BA > 'kappa';
  \u03BB > 'lamda';
  \u03BC > 'm';
  \u03BD > 'n';
  \u03BE > 'xi';
  \u03BF > 'omicron';
  \u03C0 > 'pi';
  \u03C1 > 'rho';
  \u03C3 > 'sigma';
  \u03C4 > 'ta';
  \u03C5 > 'upsilon';
  \u03C6 > 'phi';
  \u03C7 > 'chi';
  \u03C8 > 'psi';
  \u03C9 > 'omega';
  \u03d5 > 'phi';
  \u03f5 > 'epsilon';
  "

  id <- character()

  for (ii in 1:length(method)){
    if(method[ii] == "greek_letter-name" & customized[ii] == FALSE){
      id_append <- id_greek_letter_name
    } else if(customized[ii] == FALSE){
      id_append <- paste0(":: ", method[ii], ";")
    } else if(customized[ii] == TRUE){
      id_append <- method[ii]
    }

    id  <- paste(id, id_append, sep = "\n")
  }


  while (length(line <- readLines(fin, n = 1, warn = FALSE)) > 0) {
    # do: transliterate the line and write to output file

    transliterated_text <-  stringi::stri_trans_general(line, id, rules=TRUE)
    writeLines(transliterated_text, fout)
  }

  close(fin)
  close(fout)


}
