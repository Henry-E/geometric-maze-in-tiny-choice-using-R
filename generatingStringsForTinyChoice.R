library(dplyr)
library(ggplot2)
library(stringr)

# There's a better way to do this I'm sure
# generate all possible combination then unique to remove duplicates
pointsOnASmallSquare <- t(combn(rep(c(1, 2), 2), 2))
pointsOnASmallSquare <- unique(pointsOnASmallSquare)
pointsOnASmallSquare <- data_frame(x = pointsOnASmallSquare[, 1], y = pointsOnASmallSquare[, 2])
pointsOnASmallSquare <- data.frame(lapply(pointsOnASmallSquare, as.character), stringsAsFactors=FALSE)
pointsOnASmallSquare$name <- paste(pointsOnASmallSquare$x, pointsOnASmallSquare$y, sep = ",")
numPoints <- length(pointsOnASmallSquare[[1]])
shiftedPoints <- c(numPoints, seq(numPoints - 1))
pointsOnASmallSquare$destination <- pointsOnASmallSquare[shiftedPoints, "name"]
pointsOnASmallSquare$text <- c("l", "r", "u", "d")

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