#' Identify factors discriminating clusters
#'
#' @param data : a matrix of features (with features as column)
#' @param groups : a vector of groups binning the matrix of features
#' @param p.adj : The method to correct the `p-value`. See \link[stats]{p.adjust} for available methods.
#'
#' @details This function seeks to measure the importance of each variable on the composition of clusters. It computes a \link[MASS]{lda} and extracts the scaling as proxy of the importance of the variable on cluster separation.
#'
#' @return a dataframe containing the cumulated LDA scaling scores of each feature, the mean and SD of each feature for each group, and the p-value of the ANOVA computed with each feature as response variable, and the grouping vector as explanatory variable.
#' @export
#'
#' @examples
#' data(iris)
#' my_features <- iris[1:4]
#' my_groups <- iris$Species
#'
#' discrim_factors(my_features, my_groups, "bon")
#'
discrim_factors <- function(data, groups, p.adj = "none"){
  my.lda <- MASS::lda(data, groups)
  my.scalings <- my.lda$scaling
  my.scalings <- rowSums(my.scalings)
  my.scalings <- sort(my.scalings)

  mean.g <- data %>%
    aggregate(by = list(groups), FUN = mean)
  rownames(mean.g) <- mean.g$Group.1
  mean.g <- mean.g %>% dplyr::select(-Group.1)
  rownames(mean.g) <- paste("Mean", rownames(mean.g))
  sd.g <- data %>%
    aggregate(by = list(groups), FUN = sd)
  rownames(sd.g) <- sd.g$Group.1
  sd.g <- sd.g %>% dplyr::select(-Group.1)
  rownames(sd.g) <- paste("SD", rownames(mean.g))

  summary.group <- as.data.frame(cbind(t(mean.g), t(sd.g)))

  summary.group$scaling <- my.scalings[match(rownames(summary.group), names(my.scalings))]

  pval <- vector()
  for (i in 1:length(my.scalings)){
    tmp <- row.names(summary.group)[i]
    val <- cbind(data[tmp], groups)
    colnames(val) <- c("val", "group")
    mod <- aov(val ~ group, val)
    mod.anova <- anova(mod)
    pval[i] <- mod.anova$`Pr(>F)`[1]
  }

  summary.group$pvalue <- p.adjust(pval, p.adj)

  return(summary.group %>%
           dplyr::select(scaling, everything()) %>%
           dplyr::arrange(-abs(scaling)))
}
