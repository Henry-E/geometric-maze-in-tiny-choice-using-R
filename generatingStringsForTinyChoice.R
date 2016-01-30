library(dplyr)
library(ggplot2)
library(stringr)

# load the functions for generating shapes
# better off keeping them in separate files I think?

# There's a better way to do this I'm sure
# generate all possible combination then unique to remove duplicates
gridSize <- 4
pointsOnASmallSquare <- t(combn(rep(seq(gridSize), 2), 2))
pointsOnASmallSquare <- unique(pointsOnASmallSquare)
pointsOnASmallSquare <- data_frame(x = pointsOnASmallSquare[, 1], y = pointsOnASmallSquare[, 2])
# ggplot(pointsOnASmallSquare, aes(x=x, y=y)) + geom_point()

# this is all messing around with dataframes to make the coords into strings
pointsOnASmallSquare <- data.frame(lapply(pointsOnASmallSquare, as.character), stringsAsFactors=FALSE)
pointsOnASmallSquare$name <- paste(pointsOnASmallSquare$x, pointsOnASmallSquare$y, sep = ",")

# as a test we're shifting all the points down by one space and
# seeing if that works as expected in tiny choice
numPoints <- length(pointsOnASmallSquare[[1]])
shiftedPoints <- c(numPoints, seq(numPoints - 1))
pointsOnASmallSquare$destination <- pointsOnASmallSquare[shiftedPoints, "name"]
pointsOnASmallSquare$text <- sample(c("l", "r", "u", "d"), numPoints, replace = TRUE)

for(point in seq(numPoints)){
  this<- str_c("=", 
      pointsOnASmallSquare[point, "name"],
      "=\n\n",
      pointsOnASmallSquare[point, "text"],
      " -> ",
      pointsOnASmallSquare[point, "destination"],
      "\n\n")
  cat(this)
}


