#' Compute Cohen's Kappa Coefficient
#' @details Taken from descTools
#' who based it on Kappa from library(vcd)
#' author: David Meyer
#' see also: kappa in library(psych)
#' Integrated here to reduce dependecy to descTools as it is the only function from DescTools we used
#' Computes the agreement rates Cohen's kappa and weighted kappa and their confidence intervals.
#'
#' @param x can either be a numeric vector or a confusion matrix. 
#' In the latter case x must be a square matrix, in lczexplore, 
#' will take the matrix of agreement weighted by area for all intersected geometries
#' @param y not used here, but allows to compute cross table of x and y as an entry
#' @param weights either one out of \code{"Unweighted"} (default), \code{"Equal-Spacing"}, 
#' \code{"Fleiss-Cohen"}, which will calculate the weights accordingly, 
#' or a user-specified matrix having the same dimensions as x containing the weights for each cell.
#' @param conf.level confidence level of the interval. 
#' If set to \code{NA} (which is the default) no confidence intervals will be calculated.
#' @param \dots further arguments are passed to the function \code{\link{table}}, 
#' allowing i.e. to set \code{useNA}. This refers only to the vector interface.
#' @importFrom stats qnorm
#' @details Cohen's kappa is the diagonal sum of the (possibly weighted) relative frequencies, corrected for expected values and standardized by its maximum value. \cr
#' The equal-spacing weights (see Cicchetti and Allison 1971) are defined by \deqn{1 - \frac{|i - j|}{r - 1}}
#' \code{r} being the number of columns/rows, and the Fleiss-Cohen weights by \deqn{1 - \frac{(i - j)^2}{(r - 1)^2}}
#' The latter attaches greater importance to closer disagreements
#' @references Cohen, J. (1960) A coefficient of agreement for nominal scales. \emph{Educational and Psychological Measurement}, 20, 37-46.
#' Everitt, B.S. (1968), Moments of statistics kappa and weighted kappa. \emph{The British Journal of Mathematical and Statistical Psychology}, 21, 97-103.
#' Fleiss, J.L., Cohen, J., and Everitt, B.S. (1969), Large sample standard errors of kappa and weighted kappa. \emph{Psychological Bulletin}, 72, 332-327.
#' Cicchetti, D.V., Allison, T. (1971) A New Procedure for Assessing Reliability
#' of Scoring EEG Sleep Recordings \emph{American Journal of EEG Technology}, 11, 101-109.
#' @author David Meyer <david.meyer@r-project.org>, some changes and tweaks Andri Signorell <andri@signorell.net> and
#' integrated for areas by Matthieu Gousseff
#' @return the value of this pseudoKappa
#' @export  
#'
#' @examples 
#' # from Bortz et. al (1990) Verteilungsfreie Methoden in der Biostatistik, Springer, pp. 459
#' m <- matrix(c(53,  5, 2,
#'              11, 14, 5,
#'              1,  6, 3), nrow=3, byrow=TRUE,
#'            dimnames = list(rater1 = c("V","N","P"), rater2 = c("V","N","P")) )
#' # confusion matrix interface
#' CohenKappa(m, weight="Unweighted")
CohenKappa <- function (x, y = NULL,
                        weights = c("Unweighted", "Equal-Spacing", "Fleiss-Cohen"),
                        conf.level = NA, ...) {

  if (is.character(weights))
    weights <- match.arg(weights)

  if (!is.null(y)) {
    # we can not ensure a reliable weighted kappa for 2 factors with different levels
    # so refuse trying it... (unweighted is no problem)

    if (!identical(weights, "Unweighted"))
      stop("Vector interface for weighted Kappa is not supported. Provide confusion matrix.")

    # x and y must have the same levels in order to build a symmetric confusion matrix
    x <- factor(x)
    y <- factor(y)
    lvl <- unique(c(levels(x), levels(y)))
    x <- factor(x, levels = lvl)
    y <- factor(y, levels = lvl)
    x <- table(x, y, ...)

  } else {
    d <- dim(x)
    if (d[1L] != d[2L])
      stop("x must be square matrix if provided as confusion matrix")
  }

  d <- diag(x)
  n <- sum(x)
  nc <- ncol(x)
  colFreqs <- colSums(x)/n
  rowFreqs <- rowSums(x)/n

  kappa <- function(po, pc) {
    (po - pc)/(1 - pc)
  }

  std <- function(p, pc, k, W = diag(1, ncol = nc, nrow = nc)) {
    sqrt((sum(p * sweep(sweep(W, 1, W %*% colSums(p) * (1 - k)),
                        2, W %*% rowSums(p) * (1 - k))^2) -
      (k - pc * (1 - k))^2) / crossprod(1 - pc)/n)
  }

  if(identical(weights, "Unweighted")) {
    po <- sum(d)/n
    pc <- as.vector(crossprod(colFreqs, rowFreqs))
    k <- kappa(po, pc)
    s <- as.vector(std(x/n, pc, k))

  } else {

    # some kind of weights defined
    W <- if (is.matrix(weights))
      weights
    else if (weights == "Equal-Spacing")
      1 - abs(outer(1:nc, 1:nc, "-"))/(nc - 1)
    else # weights == "Fleiss-Cohen"
      1 - (abs(outer(1:nc, 1:nc, "-"))/(nc - 1))^2

    po <- sum(W * x)/n
    pc <- sum(W * colFreqs %o% rowFreqs)
    k <- kappa(po, pc)
    s <- as.vector(std(x/n, pc, k, W))
  }

  if (is.na(conf.level)) {
    res <- k
  } else {
    ci <- k + c(1, -1) * qnorm((1 - conf.level)/2) * s
    res <- c(kappa = k, lwr.ci = ci[1], upr.ci = ci[2])
  }

  return(res)

}



# KappaTest <- function(x, weights = c("Equal-Spacing", "Fleiss-Cohen"), conf.level = NA) {
# to do, idea is to implement a Kappa test for H0: kappa = 0 as in
# http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf, pp. 1687
#   print( "still to do...." )

# }
