GenXPointsAndY <- function(line_pts, n=20, sign_flip_prop=0) {
  ### Generate points about line drawn in [-1,1]x[-1,1] and #
  # true classification Y. #
  #Args: n = points to gen, line pts is where tto draw line seg, sign flip
  # prop is how many labels to misclassify if don't want fully linearly separable dat ###
  # Col 1 and 2 are resp x and y axis bounds
  x1_bounds <- sort(line_pts[, 1])
  x2_bounds <- sort(line_pts[, 2])
  x1 <- runif(n, x1_bounds[1], x1_bounds[2])
  x2 <- runif(n, x2_bounds[1], x2_bounds[2])
  xdt <- data.table("x_int"=0, x1, x2)
  # Find the slope of line  
  slope <- (line_pts[2, 2]-line_pts[1, 2])/(line_pts[2, 1]-line_pts[1, 1])
  # Mult x vector by slope and subtract from max/min y depending on direction
  if(slope < 0) {
    xdt$on_line <- max(xdt$x2) - (min(xdt$x1)-xdt$x1)*slope
  } else {
    xdt$on_line <- min(xdt$x2) - (min(xdt$x1)-xdt$x1)*slope
  }
  y <- ifelse(xdt[1:nrow(xdt), "x2"] < xdt[1:nrow(xdt), "on_line"], -1, 1)
  # Flip the sign of a rounded proportion of the y's for non-linearly separable data 
  if (sign_flip_prop) {
    y[round(runif(round(length(y)*sign_flip_prop, 0), 1, length(y)), 0)] <- 
      y[round(runif(round(length(y)*sign_flip_prop, 0), 1, length(y)), 0)] * -1 
  }
  xdt$y <- y
    xdt  
}
SanityCheck <- function(xdt) {
  ### On line? ###
  sanity_check <- ggplot(xdt, aes(x1, on_line)) + 
    geom_point(pch=21, fill="white", size=6) + 
    geom_line(data=line_pts, aes(pt1, pt2)) + ga + ap + ylab("")
  sanity_check
}
Vis <- function(xdt) {
  ### Visualize ###
  vis <- ggplot(xdt, aes(x1, x2, fill=as.factor(y))) + 
    geom_point(pch=21, size=6) + 
    geom_line(inherit.aes=FALSE, linetype="dashed", color="black", data=line_pts, size=2, aes(pt1, pt2)) + 
    ga + ap + scale_fill_manual(values=cf_vals)
  vis  
}
line_pts <- DefLinePts()
xdt <- GenXPointsAndY(line_pts, 40, .1)