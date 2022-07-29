data(bib_example_small)

bib_example_small$first_author_last_name <- sapply(stringr::str_split(bib_example_small$author, ",",  n = 2), `[`, 1)


bib_example_small$abstract_norm <- norm_abstract(bib_example_small$abstract, bib_example_small$first_author_last_name)
# or
bib_example_small$abstract_norm <- norm_abstract(bib_example_small$abstract, FALSE)
