directionPlot <- function(angles,
                          speeds,
                          angle.unit="degrees",
                          plot.mean=TRUE) {
  angles <- convert2radians(angles, input=angle.unit)
  data <- data.frame(Angles = angles, Speeds = speeds)
  data <- rbind(data, data.frame(Angles = 0, Speeds = 0))

  plot <- ggplot(data, aes(x = Angles, y = Speeds)) +
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
