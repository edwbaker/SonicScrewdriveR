#' @importFrom ggplot2 aes_string coord_polar element_line geom_segment ggplot scale_x_continuous scale_y_continuous theme_bw
directionPlot <- function(angles,
                          speeds,
                          angle.unit="degrees",
                          plot.mean=TRUE) {
  #Please CRAN
  Angles <- Speeds <- NULL


  angles <- convert2radians(angles, input=angle.unit)
  #Named before the row is appended. data.frame(0, 0) names its columns X0 and
  #X0.1, which match nothing, so rbind() failed for every input.
  data <- data.frame(Angles=angles, Speeds=speeds)
  data <- rbind(data, data.frame(Angles=0, Speeds=0))

  plot <- ggplot(data, aes_string(x = "Angles", y = "Speeds")) +
    coord_polar(start=pi) +
    geom_segment(aes(y = 0, xend = Angles, yend = Speeds)) +
    scale_x_continuous(limits = c(-pi,pi),
                       breaks=c(0, pi/2, pi, -pi/2, pi/4),
                       labels = c(0,90,180,-90, "")
                       ) +
    scale_y_continuous(breaks=NULL) +
    labs(x='', y='') +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    theme(panel.grid.major.y = element_line(color="grey")) +
    theme(axis.text.y = element_blank())
  if (plot.mean) {
    mean.x = mean(abs(angles))
    mean.y = mean(speeds)
    plot <- plot +     geom_segment(aes(x = -mean.x, y = mean.y, xend = mean.x, yend = mean.y))
  }
  return(plot)
}
