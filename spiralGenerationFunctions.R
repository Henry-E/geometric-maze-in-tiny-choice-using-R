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
  coils = 5, radius = 20, chord = 4, numInputs = 10, rotation = 0){
  
  # value of theta corresponding to end of last coil ¯\_(ツ)_/¯
  thetaMax <- coils * 2 * pi
  
  # How far to step away from center for each side
  awayStep <- radius / thetaMax
  
  # Distance between points to plot, I'm making this a function input
  chord <- chord
  
  centreX <- round(numInputs / 2)
  centreY <- round(numInputs / 2)

  theta <- chord / awayStep
  
  # Have to work out how many iterations it will go in order to
  # prepopulate a matrix
  numSteps <- floor(thetaMax / (chord / away)) + 1
  spiralPoints <- matrix(NA, nrow=numSteps, ncol=2)
  colnames(spiralPoints) <- c("x", "y")
  currentPoint <- 1
  
  while(theta <= thetaMax){
    # How far away from centre
    away <- awayStep * theta
    
    # How far around the centre
    around <- theta + rotation
    
    # Convert 'around' and 'away' to X and Y
    spiralPoints[currentPoint, "x"] <- round(centreX + cos(around) * away)
    spiralPoints[currentPoint, "y"] <- round(centreY + sin(around) * away)
    
    theta <- theta + (chord / away)
    currentPoint <- currentPoint + 1
  }
  
  data.frame(spiralPoints)
}

this <- GeneratePointsOnASpiral()
ggplot(this, aes(x=x, y=y)) + geom_point() + geom_path()

