#' Pretty PCA Plot
#'
#' @param obj A PCA object computed with prcomp or princomp
#' @param month an ordered vector with the months
#' @param year an ordered vector with the year
#' @param type `default` or `ggplot` to decide which kind of graph you want to do
#' @param ... optional parameters to be passed to the plot function
#'
#' @return a beautiful plot
#' @export
#'
#' @examples
#'
#' month <- rep(month.name, 3)
#' year <- c(rep(2014, 12), rep(2015, 12), rep(2016, 12))
#'
#' data <- matrix(ncol = 10,
#'                nrow = 36,
#'                data = rnorm(360))
#'
#' data.pca <- prcomp(data)
#' pretty_pca_plot(data.pca, month, year, main = "test plot")
#'
pretty_pca_plot <-
  function(obj, month, year, type = "default", ...) {
    # to do : include rda method

    possible_class <- c("prcomp", "princomp")
    if (!(class(obj) %in% possible_class)) {
      stop("obj must be either computed with prcomp, or princomp")
    }

    if (class(obj) == "prcomp") {
      loadings <- obj$rotation
      p <- nrow(unclass(loadings))
      vx <- colSums(loadings ^ 2)
      varex <- vx / p
      x <- obj$x[, 1]
      y <- obj$x[, 2]
      z <- obj$x[, 3]

    }

    if (class(obj) == "princomp") {
      scores <- obj$scores
      loadings <- obj$loadings
      p <- nrow(unclass(loadings))
      vx <- colSums(loadings ^ 2)
      varex <- vx / p
      x <- scores[, 1]
      y <- scores[, 2]
      z <- scores[, 3]
    }

    # if (class(obj) == "rda") {
    #   scores <- scores(obj)
    #   loadings <- obj$loadings
    #   p <- nrow(unclass(loadings))
    #   vx <- colSums(loadings^2)
    #   varex <-vx/p
    #   x <- scores[, 1]
    #   y <- scores[, 2]
    #   z <- scores[, 3]
    # }


    # arrow destinations
    y_end <- dplyr::lead(y)
    x_end <- dplyr::lead(x)

    #plot

    if (type == "default") {
      plot(
        x,
        y,
        col = as.factor(month),
        pch = as.numeric(as.factor(year)),
        ylab = paste("PC2 (", round(varex[2] * 100, 1), "%)", sep = ""),
        xlab = paste("PC1 (", round(varex[1] * 100, 1), "%)", sep = ""),
        ...
      )

      segments(x, y, x_end, y_end, lty = as.numeric(as.factor(year)))
    }
    else if (type == "ggplot")
    {
      arguments <- list(...)
      df <- data.frame(x, y, z, x_end, y_end, year, month)
      plot.g <- ggplot2::ggplot(df) +
        ggplot2::geom_point(ggplot2::aes(x = x, y = y, col = month, shape = as.factor(year))) +
        ggplot2::geom_segment(ggplot2::aes(x = x, y = y, xend = x_end, yend = y_end, linetype = as.factor(year)), alpha = 0.5) +
        ggplot2::theme_bw() +
        ggplot2::ylab(paste("PC2 (", round(varex[2] * 100, 1), "%)", sep = "")) +
        ggplot2::xlab(paste("PC1 (", round(varex[1] * 100, 1), "%)", sep = "")) +
        ggplot2::scale_color_discrete(name = "Month") +
        ggplot2::scale_linetype_discrete(name = "Year") +
        ggplot2::scale_shape_discrete(name = "Year")

      if(!is.null(arguments$main)){
        plot.g +
          ggplot2::ggtitle(arguments$main)
      }
      else{
        plot.g
      }
    }
    else{
      stop("type must be default or ggplot")
    }
  }
