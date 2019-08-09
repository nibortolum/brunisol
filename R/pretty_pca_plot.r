#' Pretty PCA Plot
#'
#' @description This function takes a PCA object and a dates vector. The dates are processed and split in day, months, and years.
#'     The pca is then plotted and points are connected through time, to see the evolution of the lake communities.
#' @param obj A PCA object computed with prcomp or princomp
#' @param dates a vector of dates
#' @param type `default` or `ggplot` to decide which kind of graph you want to do
#' @param ... optional parameters to be passed to the plot function
#'
#' @return a beautiful plot
#' @export
#'
#' @examples
#'
#' year <- c(rep(2014,12), rep(2015, 12), rep(2016, 12))
#' month <- rep(1:12, 3)
#' day <- sample(1:28, 36, replace = TRUE)
#' my_date <- paste(year, month, day, sep="-")
#'
#'
#' data <- matrix(ncol = 10,
#'                nrow = 36,
#'                data = rnorm(360))
#'
#' data.pca <- prcomp(data)
#' pretty_pca_plot(data.pca, my_date, main = "test plot")
#'
pretty_pca_plot <-
  function(obj, dates, type = "default", ...) {
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


    #rearrange dates

    dates <- rearrange_dates(dates)
    new_order <- order(dates$dates)
    dates <- dates[new_order, ]

    x <- x[new_order]
    y <- y[new_order]
    z <- z[new_order]


    # arrow destinations
    y_end <- dplyr::lead(y)
    x_end <- dplyr::lead(x)



    #plot


    if (type == "default") {
      plot(
        x,
        y,
        col = as.factor(dates$month),
        pch = as.numeric(as.factor(dates$years)),
        ylab = paste("PC2 (", round(varex[2] * 100, 1), "%)", sep = ""),
        xlab = paste("PC1 (", round(varex[1] * 100, 1), "%)", sep = ""),
        ...
      )

      segments(x, y, x_end, y_end, lty = as.numeric(as.factor(dates$years)))
    }
    else if (type == "ggplot")
    {
      arguments <- list(...)
      df <- data.frame(x, y, z, x_end, y_end, year = dates$years, month = month.abb[dates$months])
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
