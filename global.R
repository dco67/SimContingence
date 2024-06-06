 
 MutInfStd <- function(x, ver = 4){
   mutinf <- MutInf(x)
   Exy <- Entropy(x)
   Ex <- Entropy(rowSums(x))
   Ey <- Entropy(colSums(x))
   den <- c(Exy,
            min(Ex, Ey),
            max(Ex, Ey),
            sqrt(Ex * Ey),
            (Ex + Ey) / 2)[ver]
   Maxmutinf <- min(Ex, Ey)
   mutinfstd <- mutinf / den
   return(list(MIxy = mutinf,
               Maxmutinf = Maxmutinf,
               Ex = Ex,
               Ey = Ey,
               Exy = Exy,
               MIxySTD = mutinfstd)
               )
 }
 
 #' Chi-squared test statistic for contingency tables
 #'
 #' Calculates the chi-squared test statistic for a two-way contingency table.
 #' @param tab A \code{K x C} matrix (contingency table) of counts. See details.
 #' @return The calculated value of the chi-squared statistic.
 #' @details Suppose that \code{tab} consists of counts from \eqn{K} populations (rows) in \eqn{C} categories.  The chi-squared test statistic is computed as
 #' \deqn{
 #'   \sum_{i=1}^K \sum_{j=1}^C (E_{ij} - O_{ij})^2/E_{ij},
 #' }{
 #'   \sum_ij (E_ij - O_ij)^2/E_ij,
 #' }
 #' where \eqn{O_{ij}}{O_ij} is the observed number of counts in the \eqn{i}th row and \eqn{j}th column of \code{tab}, and \eqn{E_{ij}}{E_ij} is the expected number of counts under \eqn{H_0} that the populations have indentical proportions in each category:
 #' \deqn{
 #'   E_{ij} = \frac 1 N \sum_{i=1}^K O_{ij} \times \sum_{j=1}^C O_{ij}.
 #' }{
 #'   E_ij = \sum_i O_ij * \sum_j O_ij / N,
 #' }
 #' where \eqn{N} is the total number of counts in \code{tab}.
 #' @examples
 #' # simple contingency table
 #' ctab <- rbind(pop1 = c(5, 3, 0, 3),
 #'                 pop2 = c(4, 10, 2, 5))
 #' colnames(ctab) <- LETTERS[1:4]
 #' ctab
 #' chi2.stat(ctab) # chi^2 test statistic
 #' @export
 chi2.stat <- function(tab) {
   N <- rowSums(tab) # number of fish in each lake
   p0 <- colSums(tab)/sum(N) # MLE of common probability vector
   tabE <- N %o% p0 # expected
   sum(((tab-tabE)^2/tabE)[tabE > 0]) # chi2
 }
 
 #' Likelihood ratio test statistic for contingency tables
 #'
 #' Calculate the likelihood ratio test statistic for general two-way contingency tables.
 #'
 #' @param tab A \code{K x C} matrix (contingency table) of counts. See details.
 #' @return The calculated value of the LRT statistic.
 #' @details Suppose that \code{tab} consists of counts from \eqn{K} populations (rows) in \eqn{C} categories.  The likelihood ratio test statistic is computed as
 #' \deqn{
 #'   2 \sum_{i=1}^K \sum_{j=1}^N O_{ij} \log(p^A_{ij}/p^0_{j}),
 #' }{
 #'   2 \sum_ij O_ij log(p_ij/p_0j),
 #' }
 #' where \eqn{O_{ij}}{O_ij} is the observed number of counts in the \eqn{i}th row and \eqn{j}th column of \code{tab}, \eqn{p^A_{ij} = O_{ij}/\sum_{j=1}^C O_{ij}}{p_ij = O_ij/(\sum_j O_ij)} is the unconstrained estimate of the proportion of category \eqn{j} in population \eqn{i}, and \eqn{p^0_j = \sum_{i=1}^K O_{ij} / \sum_{i=1}^K\sum_{j=1}^C O_{ij}}{p_0j = \sum_i O_ij / \sum_ij O_ij} is the estimate of this proportion under \eqn{H_0} that the populations have indentical proportions in each category.  If any column has only zeros it is removed before calculating the LRT statistic.
 #' @examples
 #' # simple contingency table
 #' ctab <- rbind(pop1 = c(5, 3, 0, 3),
 #'                 pop2 = c(4, 10, 2, 5))
 #' colnames(ctab) <- LETTERS[1:4]
 #' ctab
 #' LRT.stat(ctab) # likelihood ratio statistic
 #' @export
 LRT.stat <- function(tab) {
   K <- nrow(tab) # number of lakes
   N <- rowSums(tab) # number of fish in each lake
   p0 <- colSums(tab)/sum(N) # MLE of common probability vector
   lp0 <- matrix(rep(log(p0), each = K), nrow = K) # log term
   L0 <- sum((tab * lp0)[lp0 > -Inf]) # loglik under H0
   LA <- sum((tab * log(tab/N))[tab > 0]) # loglik under HA
   2 * (LA - L0) # LRT (>= 0)
 }
 
# Source: http://www.cookbook-r.com/Manipulating_data/Converting_between_data_frames_and_contingency_tables/#countstocases-function
# Convert from data frame of counts to data frame of cases.
# `countcol` is the name of the column containing the counts
countsToCases <- function(x, countcol = "Freq") {
  # Get the row indices to pull from x
  idx <- rep.int(seq_len(nrow(x)), x[[countcol]])
  
  # Drop count column
  x[[countcol]] <- NULL
  
  # Get the rows from x
  x[idx, ]
}

# AUTRE MANIÈRE:  library(epitools)
#                 dimnames(tbl) <- list(c("A", "B"), c("A", "B"))
#                 dat <- expand.table(tbl)

# Function to calculate odds ratios and confidence intervals on odds ratios.
# Written by Kevin Middleton, Package abd
# Successes in Column 1
# Treatment of interest in Row 2

odds.ratio <- function(x, conf.level = 0.95){
  rowsums <- rowSums(x)
  p1 <- x[1, 1] / rowsums[1]
  p2 <- x[2, 1] / rowsums[2]
  o1 <- p1 / (1 - p1)
  o2 <- p2 / (1 - p2)
  OR <- o2 / o1
  log.OR <- log(OR)
  SE.log.OR <- sqrt(sum(1/x))
  crit <- qnorm((1 - conf.level)/2, lower.tail = FALSE)
  log.lower <- log.OR - crit * SE.log.OR
  log.upper <- log.OR + crit * SE.log.OR
  lower <- exp(log.lower)
  upper <- exp(log.upper)
  zz <- list(p1 = p1, p2 = p2, o1 = o1, o2 = o2, OR = OR, 
             lower = lower, upper = upper, conf.level = conf.level)
#  class(zz) <- "odds.ratio"
  return(zz)
}


emptyTable <- tibble(A = as.integer(c(NA, NA)), B = as.integer(c(NA, NA)))