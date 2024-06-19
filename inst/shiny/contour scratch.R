

# example from https://data.ohdsi.org/OutcomeMisclassificationEval/
# ace vs arb in dod ps matched 730d

# at spec=1, sens must be > 0.0088


a = 1313
b = 1198
c = 149568
d = 149683
outcomeName = "Isch Stroke"
targetName = "target"
comparatorName = "comparator"
sens1 = 0.589
sens0 = 0.589
spec1 = 0.997
spec0 = 0.997
alpha = 0.05

outcomeMisclassification(a = a,
                         b = b,
                         c = c,
                         d = d,
                         outcomeName = "outcome",
                         targetName = "target",
                         comparatorName = "comparator",
                         sens1 = sens1, # outcome sens in target
                         sens0 = sens0, # outcome sens in comparator
                         spec1 = spec1, # outcome spec in target
                         spec0 = spec0, # outcome spec in comparator
                         alpha = 0.05)






outcomeMisclassification <- function(a,
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







space <- tibble::as_tibble(expand.grid(sens = seq(0.5, 1, 0.1),
                                       spec = seq(0.9, 1, 0.00001)))
space$correctedOr <- NA

for (i in 1:nrow(space)) { # i=1
  
  a = 1313
  b = 1198
  c = 149568
  d = 149683
  
  sens1 <- space$sens[i]
  sens0 <- space$sens[i]
  spec1 <- space$spec[i]
  spec0 <- space$spec[i]
  
  results <- outcomeMisclassification(a = a,
                                      b = b,
                                      c = c,
                                      d = d,
                                      sens1 = sens1,
                                      sens0 = sens0,
                                      spec1 = spec1,
                                      spec0 = spec0)
  
  correctedOr <- as.numeric(results$estimates[2, 2])
  space$correctedOr[i] <- correctedOr
}


space2 <- space[!is.na(space$correctedOr), ]
#space2$correctedOr <- round(space2$correctedOr, 3)


orNoMisclass <- space2$correctedOr[space2$sens == 1 & space2$spec == 1]
breaks <- as.numeric(quantile(space2$correctedOr, na.rm = TRUE))

p1 <- ggplot2::ggplot(space2, ggplot2::aes(x = sens, y = spec, z = correctedOr)) +
  ggplot2::geom_contour(ggplot2::aes(colour = ..level..), breaks = breaks) +
  ggplot2::annotate("text", x = 1, y = 1, label = orNoMisclass, size = 3) +
  ggplot2::scale_colour_gradient(guide = 'none')
  
  scale_fill_gradient(limits = range(space2$correctedOr), high = 'red', low = 'green') +
  
  ggplot2::scale_x_continuous(breaks = seq(0.5, 1, 0.1), expand = c(0,0)) +
  ggplot2::scale_y_continuous(breaks = seq(0.991, 1, 0.001), expand = c(0,0)) +
  ggplot2::coord_fixed(ylim = c(0.991, 1.01), xlim = c(0.5, 1.025)) +
  #ggplot2::coord_equal(ylim = c(0.95, 1.01)) +
  ggplot2::scale_colour_gradient(guide = 'none')

directlabels::direct.label(p1, list("far.from.others.borders", "calc.boxes", "enlarge.box",
                           hjust = 1, vjust = -.5, box.color = NA, cex = .6,
                           fill = "transparent", "draw.rects"))


View(space2[space2$correctedOr %in% breaks, ])






dat <- expand.grid(Se = seq(0.582, 1, 0.02),
                   Sp = seq(0.762, 1, 0.02))

dat$OR_c <- apply(dat, 1,
                  function(x) episensr::misclassification(matrix(c(126, 92, 71, 224),
                                                       nrow = 2, byrow = TRUE),
                                                type = "exposure",
                                                bias_parms = c(x[1], x[1], x[2], x[2]))$adj.measures[2, 1])
dat$OR_c <- round(dat$OR_c, 2)


breaks <- as.numeric(quantile(dat$OR_c, na.rm = TRUE))


library(ggplot2)
library(directlabels)
p1 <- ggplot(dat, aes(x = Se, y = Sp, z = OR_c)) +
  geom_contour(aes(colour = ..level..), breaks = breaks) +
  annotate("text", x = 1, y = 1, label = "4.32", size = 3) +
  scale_fill_gradient(limits = range(dat$OR_c), high = 'red', low = 'green') +
  scale_x_continuous(breaks = seq(0.5, 1, .1), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0.5, 1, .1), expand = c(0,0)) +
  coord_fixed(ylim = c(0.5, 1.025), xlim = c(0.5, 1.025)) +
  scale_colour_gradient(guide = 'none') +
  xlab("Sensitivity") +
  ylab("Specificity") +
  ggtitle("Estimates of Corrected OR")
direct.label(p1, list("far.from.others.borders", "calc.boxes", "enlarge.box",
                      hjust = 1, vjust = -.5, box.color = NA, cex = .6,
                      fill = "transparent", "draw.rects"))

















