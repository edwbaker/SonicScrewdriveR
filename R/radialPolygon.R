#' @importFrom graphics polygon
#' @export
radialPolygon <- function(
    angle1,angle2,
    radius1,radius2,
    col="grey",
    rot=-pi,
    angleinc=0.01,
    reverse=TRUE
) {
  if (length(angle1) == 1 & length(angle2) == 1) {
    while (angle1 > angle2) {
      angle2 <- angle2 + 2*pi
    }
  }
  if (reverse) {
    angle1 <- -angle1
    angle2 <- -angle2
  }
  min_a <- min(c(angle1, angle2))
  max_a <- max(c(angle1, angle2))

  angles<-seq(min_a,max_a,by=angleinc)
  angles[length(angles)]<-max_a

  if (length(radius1) == 1 & length(radius2) == 1) {
    inner_x <- cos(angles)*radius1
    inner_y <- sin(angles)*radius1
    outer_x <- cos(angles)*radius2
    outer_y <- sin(angles)*radius2
  } else {
    if (length(radius1) == 1) {
      if (radius1 == 0) {
        inner_x <- 0
        inner_y <- 0
      } else {
        inner_x <- cos(angles)*radius1
        inner_y <- sin(angles)*radius1
      }
    } else {
      inner_x <- radius1*cos(angle1)
      inner_y <- radius1*sin(angle1)
    }
    if (length(radius2) == 1) {
      outer_x <- cos(angles)*radius2
      outer_y <- sin(angles)*radius2
    } else {
      outer_x <- radius2*cos(angle2)
      outer_y <- radius2*sin(angle2)
    }
  }

  xpos<-c(inner_x,rev(outer_x))
  ypos<-c(inner_y,rev(outer_y))
  if (rot != 0) {
    xrot <- xpos*cos(rot) + ypos*sin(rot)
    yrot <- -xpos*sin(rot) + ypos*cos(rot)
    xpos <- xrot
    ypos <- yrot
  }

  polygon(xpos,ypos,col=col,border=col)
}
