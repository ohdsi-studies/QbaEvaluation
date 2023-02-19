#' @export
createTable <- function(or,
                        incidence,
                        ac = 1000000,   # target count
                        bd = 1000000) { # comparator count

  # Derive 2x2 cell counts from OR

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

#' @export
getQbaResults <- function(row) {
  results <- QbaEvaluation::outcomeMisclassificationQba(a = row$a,
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

#' @export
drawContourPlot <- function(dat) {

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
                             ylim = c(minSpec - (minSpec * incidence * 0.05), maxSpec), expand = TRUE) +
    ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.2), expand = c(0,0)) +
    ggplot2::scale_colour_gradient(guide = 'none') +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank())

  return(contourPlot)
}

# empiricalData <- tibble::tibble(Source = c("Optum EHR", "Optum EHR",
#                                            "Optum DOD", "Optum DOD",
#                                            "IBM CCAE", "IBM CCAE",
#                                            "IBM MDCD", "IBM MDCD",
#                                            "IBM MDCR", "IBM MDCR"),
#                                 Method = rep(c("Uncorrected", "Corrected"), 5),
#                                 Incidence = c(0.004, 0.007, 0.008, 0.009, 0.003, 0.004, 0.015, 0.024, 0.015, 0.008),
#                                 correctedOr = c(1.07, 1.09, 1.1, 1.16, 1.06, 1.09, 1.18, 1.23, 0.97, 0.9),
#                                 sens = c(1, 0.380, 1, 0.589, 1, 0.497, 1, 0.502, 1, 0.621),
#                                 spec = c(1, 0.999, 1, 0.997, 1, 0.999, 1, 0.997, 1, 0.990),
#                                 simIncidence = c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01))
# simIncidence = c(0.001, 0.01, 0.01, 0.01, 0.001, 0.001, 0.01, 0.01, 0.01, 0.01))

# observeredIncidences <- empiricalData$Incidence
# for (observedIncidence in observeredIncidences) {
#  print(incidences[which(abs(incidences - observedIncidence) == min(abs(incidences - observedIncidence)))])
# }

# hack to get legend with contour plot 7 with a legend
# extractLegend <- function(plot) {
#   step1 <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(plot))
#   step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
#   step3 <- step1$grobs[[step2]]
#   return(step3)
# }
# sharedLegend <- extractLegend(contourPlot)
#
#
# contourPlots <- list()
# for (incidence in incidences[1:5]) { # incidence <- incidences[1]
#   for (or in oddsRatios) { # or <- oddsRatios[1]
#
#     dat <- space[space$incidence == incidence & space$or == or, ]
#     minSens <- min(dat$sens)
#     maxSens <- max(dat$sens)
#     minSpec <- min(dat$spec)
#     maxSpec <- max(dat$spec)
#     breaks <- as.numeric(quantile(dat$correctedOr,
#                                   na.rm = TRUE,
#                                   type = 3))
#
#     if (length(unique(breaks)) == 1) { # all breaks the same
#       pointData <- dat[dat$correctedOr %in% breaks, c("correctedOr", "sens", "spec")]
#       pointData <- pointData[pointData$sens == 1 & pointData$spec == 1, ]
#       pointData$text <- sprintf("OR=%.2f", pointData$correctedOr)
#     } else {
#       pointData <- dat[dat$correctedOr %in% breaks, c("correctedOr", "sens", "spec")]
#       pointData$text <- sprintf("OR=%.2f", pointData$correctedOr)
#     }
#
#     empiricalPointData <- empiricalData[empiricalData$simIncidence == incidence &
#                                           empiricalData$spec != 1 &
#                                           empiricalData$Method == "Corrected", ]
#     empiricalPointData$text <- sprintf("%s\nOR=%.2f", empiricalPointData$Source, empiricalPointData$correctedOr)
#
#     contourPlot <- ggplot2::ggplot(data = dat,
#                                    mapping = ggplot2::aes(x = sens,
#                                                           y = spec,
#                                                           z = correctedOr)) +
#       ggplot2::geom_contour(breaks = breaks) +
#       ggplot2::geom_point(data = pointData[c(1, nrow(pointData)), ]) +
#       ggplot2::geom_text(data = pointData,
#                          ggplot2::aes(label = text),
#                          size = 3,
#                          vjust = 1.25) +
#       ggplot2::geom_point(data = empiricalPointData,
#                           size = 3,
#                           fill = "red",
#                           show.legend = FALSE,
#                           ggplot2::aes(x = sens,
#                                        y = spec,
#                                        shape = Source)) +
#       #ggplot2::theme(legend.position = "bottom") +
#       ggplot2::scale_shape_manual(values = 21:25) +
#       ggplot2::coord_cartesian(xlim = c(0 - 0.1, 1 + 0.1),
#                                ylim = c(minSpec - (minSpec * incidence * 0.05), maxSpec),expand = TRUE) +
#       ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.2), expand = c(0,0)) +
#       ggplot2::scale_colour_gradient(guide = 'none') +
#       ggplot2::theme(axis.title.x = ggplot2::element_blank(),
#                      axis.title.y = ggplot2::element_blank())
#
#     contourPlots[[length(contourPlots) + 1]] <- contourPlot
#   }
# }
#
# ## =============================================================================
# ## Save plots ==================================================================
# ## =============================================================================
#
# savePlot <- function(plot, exportFolder = "G:/OutcomeMisclassificationEval") {
#   fileName <- file.path(exportFolder, paste0(deparse(substitute(plot)), ".png"))
#   ggplot2::ggsave(fileName,
#                   plot,
#                   height = 12,
#                   width = 12)
# }
#
#
# row1 <- grid::textGrob("IN=10^-1", rot = 90, gp = grid::gpar(fontsize = 14))
# row2 <- grid::textGrob("IN=10^-2", rot = 90, gp = grid::gpar(fontsize = 14))
# row3 <- grid::textGrob("IN=10^-3", rot = 90, gp = grid::gpar(fontsize = 14))
# row4 <- grid::textGrob("IN=10^-4", rot = 90, gp = grid::gpar(fontsize = 14))
# row5 <- grid::textGrob("IN=10^-5", rot = 90, gp = grid::gpar(fontsize = 14))
#
# col0 <- grid::textGrob("")
# col1 <- grid::textGrob("OR=1.01", gp = grid::gpar(fontsize = 14))
# col2 <- grid::textGrob("OR=1.25", gp = grid::gpar(fontsize = 14))
# col3 <- grid::textGrob("OR=1.5", gp = grid::gpar(fontsize = 14))
# col4 <- grid::textGrob("OR=2", gp = grid::gpar(fontsize = 14))
# col5 <- grid::textGrob("OR=4", gp = grid::gpar(fontsize = 14))
# col6 <- grid::textGrob("OR=10", gp = grid::gpar(fontsize = 14))
#
# yLabel <- grid::textGrob("Specificity", rot = 90, gp = grid::gpar(fontsize = 10))
# xLabel <- grid::textGrob("Sensitivity", gp = grid::gpar(fontsize = 10))
#
#
# plotGrob <- gridExtra::arrangeGrob(row1, contourPlots[[1]], contourPlots[[2]], contourPlots[[3]], contourPlots[[4]], contourPlots[[5]], contourPlots[[6]],
#                                    row2, contourPlots[[7]], contourPlots[[8]], contourPlots[[9]], contourPlots[[10]], contourPlots[[11]], contourPlots[[12]],
#                                    row3, contourPlots[[13]], contourPlots[[14]], contourPlots[[15]], contourPlots[[16]], contourPlots[[17]], contourPlots[[18]],
#                                    row4, contourPlots[[19]], contourPlots[[20]], contourPlots[[21]], contourPlots[[22]], contourPlots[[23]], contourPlots[[24]],
#                                    row5, contourPlots[[25]], contourPlots[[26]], contourPlots[[27]], contourPlots[[28]], contourPlots[[29]], contourPlots[[30]],
#                                    col0, col1, col2, col3, col4, col5, col6,
#                                    heights = c(5, 5, 5, 5, 5, 0.5),
#                                    widths = c(0.5, 5, 5, 5, 5, 5, 5),
#                                    nrow = 6)
#
# plot1 <- gridExtra::grid.arrange(plotGrob,
#                                  sharedLegend,
#                                  nrow = 2,
#                                  # left = yLabel,
#                                  # bottom = xLabel,
#                                  heights=c(12, 0.5))
#
# ggplot2::ggsave(file.path("G:/OutcomeMisclassificationEval", "space.png"),
#                 plot1,
#                 height = 12,
#                 width = 22)
