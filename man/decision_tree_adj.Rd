% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/decision_tree_adjacent.R
\name{decision_tree_adj}
\alias{decision_tree_adj}
\title{Make decisions for potential duplicates}
\usage{
decision_tree_adj(df, id_dup_pair)
}
\arguments{
\item{df}{A data frame (i.e., output #1 of \code{\link[=dup_find_fuzzy_adj]{dup_find_fuzzy_adj()}})}

\item{id_dup_pair}{A data frame listing \code{id} of potential duplicate pairs (i.e., output #2 of \code{\link[=dup_find_fuzzy_adj]{dup_find_fuzzy_adj()}})}
}
\value{
Two data frames: (1) the input \code{df} with \code{"match"} column modified according to the decision tree; (2) the input \code{id_dup_pair} with \code{"decision"} column added.
}
\description{
Decisions are made by the decision tree for potential duplicates identified by \code{\link[=dup_find_fuzzy_adj]{dup_find_fuzzy_adj()}}.

Decisions are added to the \code{"decision"} column in \code{id_dup_pair}. There could be 3 levels of decisions, "duplicate", "not duplicate", and "check". If the decision is "not duplicate", \code{"match"} column in \code{df} will be  modified.
}
\details{
See manuscript \link{//TODO} for details of the decision tree.
}
\examples{
\dontrun{
c(df, id_dup_pair) \%<-\% decision_tree_adj(df, id_dup_pair)
}
}
