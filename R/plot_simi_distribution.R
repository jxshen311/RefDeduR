#' Generate similarity distribution plot
#'
#' @param df_simi A data frame with string similarity results.
#' @param simi_param A character of the similarity parameter to plot the distribution (i.e., quoted name of the column containing the similarity scores). For example, `"title_simi"`, `"abstract_simi"`, `"author_simi"`, `"first_author_last_name_simi"`.
#'
#' @return A scatterplot of distributions of similarity scores
#' @export
#'
#'
#'
#' @examples
#' \dontrun{
#' # `df_simi` is the data frame resulted from `simi_order_df()`
#' # see the example in the `simi_order_df()` help page for how `df_simi` is generated
#'
#' # Distribution of similarity scores based on normalized title
#' p_ti <- plot_simi_dist(df_simi, "title_simi")
#' p_ti  # show p_ti in the Plots tab
#'
#' # Distribution of similarity scores based on normalized abstract
#' p_ab <- plot_simi_dist(df_simi, "abstract_simi")
#' p_ab  # show p_ab in the Plots tab
#' }
plot_simi_dist <- function(df_simi,
                           simi_param # e.g., "title_simi"
                           ){
  # check df_simi
  if(missing(df_simi)){
    stop("Similarity data frame is missing: Please provide a data frame")
  }
  if(!inherits(df_simi, "data.frame")){
    stop("'df_simi' must be a data frame: Please check the data type using `class(df_simi)`")
  }
  if(!("id" %in% colnames(df_simi))){
    stop('Column "id" is missing.')
  }
  if(!inherits(simi_param, "character")){
    stop("simi_param must be a character or character vector.")
  }

  # substitute a common name for the variable name of interest
  gg_title <- sub("_simi", "", simi_param, fixed = TRUE)

  # plot
  p <- ggplot2::ggplot(df_simi, ggplot2::aes(x=reorder(.data$id, -.data[[simi_param]]), y=.data[[simi_param]])) +
    ggplot2::geom_point(size = 0.25, color = "blue") +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
    ggplot2::labs(x = "Reference ID", y = paste0("Similarity score based on normalized ", gg_title)) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_text(size = 12),
          axis.title = ggplot2::element_text(size = 14, face = "plain"),
          panel.grid = ggplot2::element_blank())

  return(p)
}
