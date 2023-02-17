#' @export
outcomeMisclassificationQba <- function(a,
                                        b,
                                        c,
                                        d,
                                        outcomeName = "outcome",
                                        targetName = "target",
                                        comparatorName = "comparator",
                                        sens1 = 1, # outcome sens in target
                                        sens0 = 1, # outcome sens in comparator
                                        spec1 = 1, # outcome spec in target
                                        spec0 = 1, # outcome spec in comparator
                                        alpha = 0.05) {

  # add check that bias params between 0 and 1

  ac <- a + c
  bd <- b + d
  ab <- a + b
  cd <- c + d
  abcd <- a + b + c + d
  observedTable <- tibble::tibble(outcome = c(sprintf("%s [+]", outcomeName), sprintf("%s [-]", outcomeName), "totals"),
                                  target = c(a, c, ac),
                                  comparator = c(b, d, bd),
                                  totals = c(ab, cd, abcd))

  observedOr <- (a/b) / (c/d)
  observedPrevalence <- ab/abcd
  seLogObservedOr <- sqrt(1/a + 1/b + 1/c + 1/d)
  lciObservedOr <- exp(log(observedOr) - qnorm(1 - alpha/2) * seLogObservedOr)
  uciObservedOr <- exp(log(observedOr) + qnorm(1 - alpha/2) * seLogObservedOr)

  A <- (a - (1 - spec1) * (a + c)) / (sens1 - (1 - spec1))
  B <- (b - (1 - spec0) * (b + d)) / (sens0 - (1 - spec0))
  C <- (a + c) - A
  D <- (b + d) - B
  AC <- A + C
  BD <- B + D
  AB <- A + B
  CD <- c + d
  ABCD <- A + B + C + D
  correctedTable <- tibble::tibble(outcome = c(sprintf("%s [+]", outcomeName), sprintf("%s [-]", outcomeName), "totals"),
                                   target = c(A, C, AC),
                                   comparator = c(B, D, BD),
                                   totals = c(AB, CD, ABCD))

  if (A < 1 | B < 1 | C < 1 | D < 1) {
    warning('Parameter combination results in negative cell(s) in corrected 2x2 table.')
    correctedOr <- NA
    correctedPrevalence <- NA
    seLogCorrectedOr <- NA
    lciCorrectedOr <- NA
    uciCorrectedOr <- NA
    logOr <- NA
    seLogOr <- NA
  } else {
    correctedOr <- (A/B) / (C/D)
    correctedPrevalence <- AB/ABCD
    num1 <- ac * a * c * (sens1 + spec1 - 1)^2
    den1 <- (ac * sens1 - a)^2 * (ac * spec1 - c)^2
    num2 <- bd * b * d * (sens0 + spec0 - 1)^2
    den2 <- (bd * sens0 - b)^2 * (bd * spec0 - d)^2
    seLogCorrectedOr <- sqrt((num1 / den1) + (num2 / den2))
    lciCorrectedOr <- exp(log(correctedOr) - qnorm(1 - alpha/2) * seLogCorrectedOr)
    uciCorrectedOr <- exp(log(correctedOr) + qnorm(1 - alpha/2) * seLogCorrectedOr)
  }

  estimates <- tibble::tibble(method = c("observed", "simpleQba"),
                              or = c(observedOr, correctedOr),
                              ciLb = c(lciObservedOr, lciCorrectedOr),
                              ciUb = c(uciObservedOr, uciCorrectedOr),
                              logOr = c(log(observedOr), log(correctedOr)),
                              seLogOr = c(seLogObservedOr, seLogCorrectedOr))

  biasParameters <- tibble::tibble(strata = c(targetName, comparatorName),
                                   sens = c(sens1, sens0),
                                   spec = c(spec1, spec0))

  results <- list(model = "Simple QBA for outcome misclassification",
                  observedPrevalence = observedPrevalence,
                  observedTable = observedTable,
                  correctedPrevalence = correctedPrevalence,
                  correctedTable = correctedTable,
                  biasParameters = biasParameters,
                  estimates = estimates)
  return(results)
}

#' @export
outcomeMisclassificationPie <- function(){
  print("TODO")
}






















