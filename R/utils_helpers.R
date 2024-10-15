#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

data.type <- c("P/A", "counts", "cover")
Sest.method <- c("average", "chao", "jack1", "jack2", "boot")
method <- c("jaccard","bray", "manhattan", "euclidean", "canberra", "clark", "kulczynski", "gower", "altGower", "morisita", "horn", "mountford", "raup", "binomial", "chao", "cao", "mahalanobis")
transf <- c('none', 'square root', 'fourth root', 'Log (X+1)', 'P/A')
mult <- c(FALSE, TRUE)
pickPlotNames <- c("Power plot", "Density plot", "Both")
pickPlotValues <- c("power", "density", "both")
pickcboNames <- c("Budget", "Precision")
pickcboValues <- c("budget", "precision")
multmodel <- c("single.factor", "nested.symmetric")
