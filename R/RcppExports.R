# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

matMultinom <- function(probmatrix) {
    .Call(`_simstudy_matMultinom`, probmatrix)
}

markovChains <- function(nchains, P, chainLen, state0) {
    .Call(`_simstudy_markovChains`, nchains, P, chainLen, state0)
}

clipVec <- function(id, seq, event) {
    .Call(`_simstudy_clipVec`, id, seq, event)
}

chkNonIncreasing <- function(adjmatrix) {
    .Call(`_simstudy_chkNonIncreasing`, adjmatrix)
}

checkBoundsBin <- function(p1, p2, d) {
    invisible(.Call(`_simstudy_checkBoundsBin`, p1, p2, d))
}

findRhoBin <- function(p1, p2, d) {
    .Call(`_simstudy_findRhoBin`, p1, p2, d)
}

getRhoMat <- function(N, P, TCORR) {
    .Call(`_simstudy_getRhoMat`, N, P, TCORR)
}

getBeta0 <- function(lvec, popPrev, tolerance) {
    .Call(`_simstudy_getBeta0`, lvec, popPrev, tolerance)
}

estAUC <- function(dmatrix, y) {
    .Call(`_simstudy_estAUC`, dmatrix, y)
}

getBeta_auc <- function(covmat, coefs, auc, popPrev, tolerance) {
    .Call(`_simstudy_getBeta_auc`, covmat, coefs, auc, popPrev, tolerance)
}

