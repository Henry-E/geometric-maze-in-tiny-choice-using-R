# Adapated from http://stackoverflow.com/questions/13894715/draw-equidistant-points-on-a-spiral
#
# centerX-- X origin of the spiral.
# centerY-- Y origin of the spiral.
# radius--- Distance from origin to outer arm.
# sides---- Number of points or sides along the spiral's arm.
# coils---- Number of coils or full rotations. (Positive numbers spin clockwise, negative numbers spin counter-clockwise)
# rotation- Overall rotation of the spiral. ('0'=no rotation, '1'=360 degrees, '180/360'=180 degrees)
# chord---- Distance between points to plot
#

GeneratePointsOnASpiral <- function(
  coils = 5, radius = 20, chord = 4, numInputs = 10, rotation = 0.2){
  
  # value of theta corresponding to end of last coil ¯\_(ツ)_/¯
  thetaMax <- coils * 2 * pi
  
  # How far to step away from center for each side
  awayStep <- radius / thetaMax
  
  # Distance between points to plot, I'm making this a function input
  chord <- chord
  
  centreX <- 0 # round(numInputs / 2)
  centreY <- 0 # round(numInputs / 2)

  theta <- chord / awayStep
  
  # Have to work out how many iterations it will go in order to
  # prepopulate a matrix
  numSteps <- 100 #floor(thetaMax / (chord / away)) + 1
  spiralPoints <- matrix(NA, nrow=numSteps, ncol=2)
  colnames(spiralPoints) <- c("x", "y")
  currentPoint <- 1
  currentRotation <- 0
  distanceFromCentre <- 1
  
  while(currentPoint <= numSteps){
    # How far away from centre
    away <- awayStep * theta
    
    # How far around the centre
    around <- theta + rotation
    
    # Convert 'around' and 'away' to X and Y
#     spiralPoints[currentPoint, "x"] <- round(centreX + cos(around) * away)
#     spiralPoints[currentPoint, "y"] <- round(centreY + sin(around) * away)
    
    spiralPoints[currentPoint, "x"] <- round(centreX + cos(currentRotation) * distanceFromCentre)
    spiralPoints[currentPoint, "y"] <- round(centreY + sin(currentRotation) * distanceFromCentre)
    
    currentRotation <- currentRotation + 0.5
    distanceFromCentre <- distanceFromCentre + 1

    
    theta <- theta + (chord / away)
    currentPoint <- currentPoint + 1
  }
  
  data.frame(spiralPoints)
}
# 
# this <- GeneratePointsOnASpiral()
# ggplot(this, aes(x=x, y=y)) + 
#   geom_point() + 
#   geom_path()  #+ 
#   scale_x_discrete() +
#   scale_y_discrete()

# function that calculates the next point in the spiral
CalcNextPoint <- function(x=0, y=0, angleOffset = 1, distOffset = 10){
  distance <- sqrt(x^2 + y^2)
  clockwise <- sample(c(-1, 1), 1)
  distance <- clockwise * (distance + distOffset)
  
  angle <- atan2(y, x) - atan2(1, 0)  # * 180 / pi
  angle <- (angle + runif(1, min=0, max=angleOffset)) # * pi
#   angle <- (angle + (angleOffset * pi))
  
  point <- matrix(NA, nrow=length(x), ncol=2)
  point[, 1] <- round(cos(angle) * distance) # x
  point[, 2] <- round(sin(angle) * distance) # y
  point
}
# # 
# numPoints <- 100
# spiralPoints <- data.frame(matrix(NA, nrow=numPoints, ncol=2))
# colnames(spiralPoints) <- c("x", "y")
# spiralPoints[1, ] <- c(10, 10)
# 
# for(currentPoint in (2:numPoints)){
#   spiralPoints[currentPoint, ] <- CalcNextPoint(spiralPoints[currentPoint - 1, "x"],
#                                                 spiralPoints[currentPoint - 1, "y"],
#                                                 distOffset=rpois(1, lambda=4))
# }
# 
# 
# ggplot(spiralPoints, aes(x=x, y=y)) + 
#   geom_point() + 
#   geom_path()
# 
# this <- head(spiralPoints)
# round(sqrt(this$x^2 + this$y^2))
# round(sqrt(pointsOnASmallSquare$x^2 + pointsOnASmallSquare$y^2))
