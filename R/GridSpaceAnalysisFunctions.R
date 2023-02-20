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
