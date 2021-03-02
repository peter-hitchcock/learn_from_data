GenInequalityProbs <- function(N, eps, term1fx) {
  ### Term 1 is what's replacing our constant M ###
  term2 <- exp(-2^(eps^2)*N)
  term1 <- term1fx(N)
  cat('\n\n', N)
  cat("\n term1", term1, "\n term2", term2, "\n Probability:", term2 * term1 * 2)
}
lapply(seq(1, 1e3, 100), function(x) GenInequalityProbs(x, eps=.5, term1fx=SPF))