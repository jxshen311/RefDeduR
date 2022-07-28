b_example <- read.csv(file = "data/b_example_abstract.csv")

b_example$first_author_last_name <- sapply(stringr::str_split(b_example$author, ",",  n = 2), `[`, 1)


b_example$abstract_norm <- norm_abstract(b_example$abstract, b_example$first_author_last_name)
# or
b_example$abstract_norm <- norm_abstract(b_example$abstract, FALSE)
