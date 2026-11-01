#' @importFrom ggplot2 .data aes coord_polar element_blank element_line geom_segment ggplot labs scale_x_continuous scale_y_continuous theme theme_bw
directionPlot <- function(angles,
                          speeds,
                          angle.unit="degrees",
                          plot.mean=TRUE) {
  angles <- convert2radians(angles, input.unit=angle.unit)
  #The axis of this plot runs over (-pi, pi], half a turn either side of north,
  #so a bearing is wrapped into that range. convert2radians() gives [0, 2*pi)
  #for a compass bearing, and ggplot2 drops a value outside a scale's limits
  #without a word, so every direction above 180 degrees was removed from the
  #plot: a bearing of 270 degrees is drawn where -90 is, which is the same place
  #on the compass, rather than not drawn at all. The modulo also brings a
  #bearing of more than one turn onto the compass.
  angles <- angles %% (2*pi)
  angles[angles > pi] <- angles[angles > pi] - 2*pi
  #Named before the row is appended. data.frame(0, 0) names its columns X0 and
  #X0.1, which match nothing, so rbind() failed for every input.
  data <- data.frame(Angles=angles, Speeds=speeds)
  data <- rbind(data, data.frame(Angles=0, Speeds=0))

  #The columns are named through .data, as elsewhere in the package. aes_string()
  #has been deprecated since ggplot2 3.0.0 and warned on every call.
  plot <- ggplot(data, aes(x = .data$Angles, y = .data$Speeds)) +
    coord_polar(start=pi) +
    geom_segment(aes(y = 0, xend = .data$Angles, yend = .data$Speeds)) +
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
    #The mean of a set of directions is the direction of their resultant, not
    #the mean of their numbers: 350 and 10 degrees average to 180, due south.
    #mean(abs(angles)) avoided that by folding west onto east, so a set of
    #easterly directions and the matching westerly set gave the same answer, and
    #it was drawn as an arc either side of north, which can show a distance from
    #north but never a direction.
    mean.cos <- mean(cos(angles))
    mean.sin <- mean(sin(angles))
    #Directions that cancel have no mean direction. atan2(0, 0) is 0, so the
    #segment would be drawn pointing north on no evidence.
    if (sqrt(mean.cos^2 + mean.sin^2) > sqrt(.Machine$double.eps)) {
      #atan2() answers over (-pi, pi], which is the range the axis runs over, so
      #the mean needs no wrapping of its own.
      mean.a <- atan2(mean.sin, mean.cos)
      #One segment, so it is given a row of its own. Taken from the data of the
      #plot it was a single value against as many rows as there were directions,
      #which ggplot2 warns about. It is drawn from the centre, as each direction
      #is.
      plot <- plot + geom_segment(
        aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
        data = data.frame(x=mean.a, y=0, xend=mean.a, yend=mean(speeds))
      )
    }
  }
  return(plot)
}
