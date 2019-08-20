#' Compute discriminating features betyeen cluster
#'
#' @description This function seeks to measure the importance of each variable on the composition of clusters. It computes a \link[MASS]{lda} and extracts the scaling as proxy of the importance of the variable on cluster separation.
#' @param data a matrix or dataframe of features
#' @param groups a vector of groups
#' @param p.adj The method to correct the `p-value`. See \link[stats]{p.adjust} for available methods.
#'
#' @return a named list of pairwise comparisond
#' @export
#'
#' @examples
#' #' data(iris)
#' my_features <- iris[1:4]
#' my_groups <- iris$Species
#'
#' discrim_factors_pairwise(my_features, my_groups, "bon")
#'
discrim_factors_pairwise <- function(data, groups, p.adj = "bon"){
  assoc <- combn(as.character(unique(my_groups)) , 2)

  final <- list()
  for(i in 1:dim(assoc)[2]){
    my_subset <- data[which(groups == assoc[2,i] | groups == assoc[1,i]),]
    my_subgroups <- groups[which(groups == assoc[2,i] | groups == assoc[1,i])]
    my_subgroups <- droplevels(my_subgroups)
    final[[paste(assoc[2,i],"vs.", assoc[1,i])]] <- discrim_factors(my_subset, my_subgroups, "bon")

  }
  return(final)
}
