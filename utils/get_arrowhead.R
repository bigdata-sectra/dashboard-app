# function get arrowhead() returns coordinates of a the arrowhead of a line
get_arrowhead <- function (fromPoint, toPoint){
  
  # dx,dy = arrow line vector
  dx <- toPoint$x - fromPoint$x;
  dy <- toPoint$y - fromPoint$y;
  
  # normalize
  length <- sqrt(dx * dx + dy * dy);
  unitDx <- dx / length;
  unitDy <- dy / length;
  
  # increase this to get a larger arrow head
  arrowHeadBoxSize = 0.00005;
  
  arrowPoint1 <- list(x = (toPoint$x - unitDx * arrowHeadBoxSize - unitDy * arrowHeadBoxSize),
                      y = (toPoint$y - unitDy * arrowHeadBoxSize + unitDx * arrowHeadBoxSize));
  arrowPoint2 <- list(x = (toPoint$x - unitDx * arrowHeadBoxSize + unitDy * arrowHeadBoxSize),
                      y = (toPoint$y - unitDy * arrowHeadBoxSize - unitDx * arrowHeadBoxSize));
  
  return( mapply(c, arrowPoint1, toPoint, arrowPoint2) )
  
}