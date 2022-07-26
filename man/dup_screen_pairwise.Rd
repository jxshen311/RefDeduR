% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dup_screen_pairwise.R
\name{dup_screen_pairwise}
\alias{dup_screen_pairwise}
\title{Output potential duplicates determined as requiring manual check by the decision tree}
\usage{
dup_screen_pairwise(ls_df, id_dup_pair)
}
\arguments{
\item{ls_df}{A list of data frames containing the partitioned dataset  (i.e., output #1 of \code{\link[=simi_ptn_pair]{simi_ptn_pair()}}).}

\item{id_dup_pair}{A data frame listing record id and partition id of duplicate pairs after getting automatic decisions (i.e., output of \code{\link[=decision_tree_pairwise]{decision_tree_pairwise()}}).}
}
\value{
A data frame of duplicate pairs for manual review. Pairing is indicated in \code{"match"} column.
}
\description{
Output potential duplicates determined as requiring manual check by the decision tree
}
\examples{
\dontrun{
df_check <- dup_screen_pairwise(ls_df, id_dup_pair)
}
}
