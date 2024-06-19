library(magrittr)
options(scipen=999)

## =============================================================================
## simple QBA function =========================================================
## =============================================================================

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



## =============================================================================
## Derive 2x2 cell counts from OR ==============================================
## =============================================================================

createTable <- function(or,
                        incidence,
                        ac = 1000000,   # target count
                        bd = 1000000) { # comparator count

  abcd <- ac + bd
  ab <- incidence * abcd
  cd <- abcd - ab

  term1 <- or - 1
  term2 <- ab - bd - (or * (ab + ac))
  term3 <- or * ab * ac

  discriminant <- (term2^2) - (4 * term1 * term3)
  if (discriminant < 0) {
    table <- tibble::tibble(outcome = c("[+]", "[-]", "totals"),
                            target = c(NA, NA, NA),
                            comparator = c(NA, NA, NA),
                            totals = c(NA, NA, NA))
    return(table)
  } else if (discriminant > 0) {
    a <- (-term2 - sqrt(discriminant)) / (2 * term1)
  } else {
    a <- (-term2) / (2 * term1) # discriminant = 0
  }
  a <- round(a)
  b <- ab - a
  c <- ac - a
  d <- bd - b
  table <- tibble::tibble(outcome = c("[+]", "[-]", "totals"),
                          target = c(a, c, ac),
                          comparator = c(b, d, bd),
                          totals = c(ab, cd, abcd))
  result <- list(a = a,
                 b = b,
                 c = c,
                 d = d,
                 table = table)
  return(result)
}



## =============================================================================
## Create simulation space =====================================================
## =============================================================================


#oddsRatios <- c(1.000001, 1.25, 1.50, 2, 4, 10)
oddsRatios <- c(1.01, 1.25, 1.50, 2, 4, 10)
sens <- seq(0.05, 1, 0.05)

fullSpace <- tibble::tibble()
incidences <- 10^(-1:-6)

for (incidence in incidences) {
  min <- 1 - incidence
  spec <- seq(min, 1, length = 20)
  sensSpec <- tibble::as_tibble(expand.grid(sens = sens, spec = spec))
  space <- merge(oddsRatios, sensSpec) %>%
    dplyr::rename(or = x) %>%
    dplyr::mutate(incidence = incidence) %>%
    dplyr::arrange(or, sens, spec)
  fullSpace <- dplyr::bind_rows(fullSpace, space) %>%
    dplyr::relocate(incidence, or, sens, spec)
}

getCellCounts <- function(row) {
  values <- createTable(or = row$or, incidence = row$incidence)
  row$a <- values$a
  row$b <- values$b
  row$c <- values$c
  row$d <- values$d
  return(row)
}

cellCounts <- plyr::adply(fullSpace, 1, getCellCounts)


## =============================================================================
## Apply QBA ===================================================================
## =============================================================================


getQbaResults <- function(row) {
  results <- outcomeMisclassification(a = row$a,
                                      b = row$b,
                                      c = row$c,
                                      d = row$d,
                                      outcomeName = "outcome",
                                      targetName = "target",
                                      comparatorName = "comparator",
                                      sens1 = row$sens,
                                      sens0 = row$sens,
                                      spec1 = row$spec,
                                      spec0 = row$spec,
                                      alpha = 0.05)
  row$correctedIncidence <- results$correctedPrevalence
  row$correctedOr <- results$estimates$or[2]
  return(row)
}

correctedOrs <- plyr::adply(cellCounts, 1, getQbaResults)
space <- correctedOrs %>% dplyr::filter(!is.na(correctedOr))
# readr::write_csv(space, "data/corrected_ors.csv")
# space <- readr::read_csv("data/corrected_ors.csv", show_col_types = FALSE)



oddsRatios <- c(1.01, 1.25, 1.50, 2, 4, 10)
sens <- seq(0.05, 1, 0.05)
incidences <- 10^(-1:-6)

empiricalData <- tibble::tibble(Source = c("Optum EHR", "Optum EHR",
                                           "Optum DOD", "Optum DOD",
                                           "IBM CCAE", "IBM CCAE",
                                           "IBM MDCD", "IBM MDCD",
                                           "IBM MDCR", "IBM MDCR"),
                                Method = rep(c("Uncorrected", "Corrected"), 5),
                                Incidence = c(0.004, 0.007, 0.008, 0.009, 0.003, 0.004, 0.015, 0.024, 0.015, 0.008),
                                correctedOr = c(1.07, 1.09, 1.1, 1.16, 1.06, 1.09, 1.18, 1.23, 0.97, 0.9),
                                sens = c(1, 0.380, 1, 0.589, 1, 0.497, 1, 0.502, 1, 0.621),
                                spec = c(1, 0.999, 1, 0.997, 1, 0.999, 1, 0.997, 1, 0.990),
                                simIncidence = c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01))
                                #simIncidence = c(0.001, 0.01, 0.01, 0.01, 0.001, 0.001, 0.01, 0.01, 0.01, 0.01))

# observeredIncidences <- empiricalData$Incidence
# for (observedIncidence in observeredIncidences) {
#  print(incidences[which(abs(incidences - observedIncidence) == min(abs(incidences - observedIncidence)))])
# }


# hack to get legend with contour plot 7 with a legend
extractLegend <- function(plot) {
  step1 <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(plot))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}
sharedLegend <- extractLegend(contourPlot)



contourPlots <- list()
for (incidence in incidences[1:5]) { # incidence <- incidences[1]
  for (or in oddsRatios) { # or <- oddsRatios[1]
    
    dat <- space[space$incidence == incidence & space$or == or, ]
    minSens <- min(dat$sens)
    maxSens <- max(dat$sens)
    minSpec <- min(dat$spec)
    maxSpec <- max(dat$spec)
    breaks <- as.numeric(quantile(dat$correctedOr,
                                  na.rm = TRUE,
                                  type = 3))
    
    if (length(unique(breaks)) == 1) { # all breaks the same
      pointData <- dat[dat$correctedOr %in% breaks, c("correctedOr", "sens", "spec")]
      pointData <- pointData[pointData$sens == 1 & pointData$spec == 1, ]
      pointData$text <- sprintf("OR=%.2f", pointData$correctedOr)
    } else {
      pointData <- dat[dat$correctedOr %in% breaks, c("correctedOr", "sens", "spec")]
      pointData$text <- sprintf("OR=%.2f", pointData$correctedOr)
    }
    
    empiricalPointData <- empiricalData[empiricalData$simIncidence == incidence &
                                          empiricalData$spec != 1 &
                                          empiricalData$Method == "Corrected", ]
    empiricalPointData$text <- sprintf("%s\nOR=%.2f", empiricalPointData$Source, empiricalPointData$correctedOr)
    
    contourPlot <- ggplot2::ggplot(data = dat,
                                   mapping = ggplot2::aes(x = sens,
                                                          y = spec,
                                                          z = correctedOr)) +
      ggplot2::geom_contour(breaks = breaks) +
      ggplot2::geom_point(data = pointData[c(1, nrow(pointData)), ]) +
      ggplot2::geom_text(data = pointData,
                         ggplot2::aes(label = text),
                         size = 3,
                         vjust = 1.25) +
      ggplot2::geom_point(data = empiricalPointData,
                          size = 3,
                          fill = "red",
                          show.legend = FALSE,
                          ggplot2::aes(x = sens,
                                       y = spec,
                                       shape = Source)) +
      #ggplot2::theme(legend.position = "bottom") +
      ggplot2::scale_shape_manual(values = 21:25) +
      ggplot2::coord_cartesian(xlim = c(0 - 0.1, 1 + 0.1),
                               ylim = c(minSpec - (minSpec * incidence * 0.05), maxSpec),expand = TRUE) +
      ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.2), expand = c(0,0)) +
      ggplot2::scale_colour_gradient(guide = 'none') +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank())
    
    contourPlots[[length(contourPlots) + 1]] <- contourPlot
  }
}

## =============================================================================
## Save plots ==================================================================
## =============================================================================

savePlot <- function(plot, exportFolder = "G:/OutcomeMisclassificationEval") {
    fileName <- file.path(exportFolder, paste0(deparse(substitute(plot)), ".png"))
    ggplot2::ggsave(fileName,
                    plot,
                    height = 12,
                    width = 12)
}


row1 <- grid::textGrob("IN=10^-1", rot = 90, gp = grid::gpar(fontsize = 14))
row2 <- grid::textGrob("IN=10^-2", rot = 90, gp = grid::gpar(fontsize = 14))
row3 <- grid::textGrob("IN=10^-3", rot = 90, gp = grid::gpar(fontsize = 14))
row4 <- grid::textGrob("IN=10^-4", rot = 90, gp = grid::gpar(fontsize = 14))
row5 <- grid::textGrob("IN=10^-5", rot = 90, gp = grid::gpar(fontsize = 14))

col0 <- grid::textGrob("")
col1 <- grid::textGrob("OR=1.01", gp = grid::gpar(fontsize = 14))
col2 <- grid::textGrob("OR=1.25", gp = grid::gpar(fontsize = 14))
col3 <- grid::textGrob("OR=1.5", gp = grid::gpar(fontsize = 14))
col4 <- grid::textGrob("OR=2", gp = grid::gpar(fontsize = 14))
col5 <- grid::textGrob("OR=4", gp = grid::gpar(fontsize = 14))
col6 <- grid::textGrob("OR=10", gp = grid::gpar(fontsize = 14))

yLabel <- grid::textGrob("Specificity", rot = 90, gp = grid::gpar(fontsize = 10))
xLabel <- grid::textGrob("Sensitivity", gp = grid::gpar(fontsize = 10))


plotGrob <- gridExtra::arrangeGrob(row1, contourPlots[[1]], contourPlots[[2]], contourPlots[[3]], contourPlots[[4]], contourPlots[[5]], contourPlots[[6]],
                                   row2, contourPlots[[7]], contourPlots[[8]], contourPlots[[9]], contourPlots[[10]], contourPlots[[11]], contourPlots[[12]],
                                   row3, contourPlots[[13]], contourPlots[[14]], contourPlots[[15]], contourPlots[[16]], contourPlots[[17]], contourPlots[[18]],
                                   row4, contourPlots[[19]], contourPlots[[20]], contourPlots[[21]], contourPlots[[22]], contourPlots[[23]], contourPlots[[24]],
                                   row5, contourPlots[[25]], contourPlots[[26]], contourPlots[[27]], contourPlots[[28]], contourPlots[[29]], contourPlots[[30]],
                                   col0, col1, col2, col3, col4, col5, col6,
                                   heights = c(5, 5, 5, 5, 5, 0.5),
                                   widths = c(0.5, 5, 5, 5, 5, 5, 5),
                                   nrow = 6)

plot1 <- gridExtra::grid.arrange(plotGrob,
                                 sharedLegend,
                                 nrow = 2,
                                 # left = yLabel,
                                 # bottom = xLabel,
                                 heights=c(12, 0.5))

ggplot2::ggsave(file.path("G:/OutcomeMisclassificationEval", "space.png"),
                plot1,
                height = 12,
                width = 22)