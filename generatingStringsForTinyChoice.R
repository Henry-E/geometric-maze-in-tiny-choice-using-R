library(dplyr)
library(ggplot2)
library(stringr)
library(readr)

# load the functions for generating shapes
# better off keeping them in separate files I think?
source("spiralGenerationFunctions.r")

mostCommonWords <- read_csv("mostCommonWords.csv")
colnames(mostCommonWords) <- c("words")


# There's a better way to do this I'm sure
# generate all possible combination then
radius <- 200
pointsOnASmallSquare <- t(combn(rep(seq(-radius, radius), 2), 2))
#  unique to remove duplicates
pointsOnASmallSquare <- unique(pointsOnASmallSquare)
pointsOnASmallSquare <- data_frame(x = pointsOnASmallSquare[, 1], y = pointsOnASmallSquare[, 2])
# and any points greater than the radius
pointsOnASmallSquare$distance <-  round(sqrt(pointsOnASmallSquare$x^2 + pointsOnASmallSquare$y^2))
pointsOnASmallSquare <- filter(pointsOnASmallSquare, distance <= radius)
# ggplot(pointsOnASmallSquare, aes(x=x, y=y)) + geom_point()
# sort them so that 0,0 is the first in the array
pointsOnASmallSquare <- arrange(pointsOnASmallSquare, abs(x), abs(y))




# We're testing out the operations on a data frame first, then
# expanding out to use a list of data frames, which we loop over
portalDestinations <- list()

# Poisson distributed distance values, lambda = 4
poissonRandomObservations <-rpois(nrow(pointsOnASmallSquare), lambda=4)

newLocations <- CalcNextPoint(pointsOnASmallSquare$x,
                                        pointsOnASmallSquare$y,
                                        distOffset=poissonRandomObservations)
newLocations <- data_frame(x = newLocations[, 1], 
                           y = newLocations[, 2])

portalDestinations[[length(portalDestinations) + 1 ]] <- newLocations

# Lognormal distributed distance values, stddev = 1, mean = 2
lognormalRandomObservations <- rlnorm(nrow(pointsOnASmallSquare), meanlog = 2, sdlog = 1)

newLocations <- CalcNextPoint(pointsOnASmallSquare$x,
                              pointsOnASmallSquare$y,
                              distOffset=lognormalRandomObservations)
newLocations <- data_frame(x = newLocations[, 1], 
                           y = newLocations[, 2])

portalDestinations[[length(portalDestinations) + 1 ]] <- newLocations

# normally distributed distance values, stddev = 3, mean = 0
normalRandomObservations <- rnorm(nrow(pointsOnASmallSquare), mean = 0, sd = 3)

newLocations <- CalcNextPoint(pointsOnASmallSquare$x,
                              pointsOnASmallSquare$y,
                              distOffset=normalRandomObservations)
newLocations <- data_frame(x = newLocations[, 1], 
                           y = newLocations[, 2])

portalDestinations[[length(portalDestinations) + 1 ]] <- newLocations

# uniformly distributed distance values, min = 1, max = 6
uniformRandomObservations <- runif(nrow(pointsOnASmallSquare), min = 1, max = 6)

newLocations <- CalcNextPoint(pointsOnASmallSquare$x,
                              pointsOnASmallSquare$y,
                              distOffset=uniformRandomObservations)
newLocations <- data_frame(x = newLocations[, 1], 
                           y = newLocations[, 2])

portalDestinations[[length(portalDestinations) + 1 ]] <- newLocations



randomLocations <- function(numObs, radius) sample(seq(-(radius-1), (radius-1)), numObs, replace = TRUE)

for(currentPortal in seq(length(portalDestinations))){
  newLocations <- portalDestinations[[currentPortal]]
  # make sure none of the portalDestinations are off the map
  newLocations$distance <- round(sqrt(newLocations$x^2 + newLocations$y^2))
  isOutsideRange <- newLocations$distance > radius
  numOutsideRange <- sum(isOutsideRange)
  # if they are replace them with random location inside radius
  replacementLocations <- data.frame(matrix(NA, nrow=numOutsideRange, ncol=2))
  colnames(replacementLocations) <- c("x", "y")
  replacementLocations$x <- randomLocations(numOutsideRange, radius)
  replacementLocations$y <- randomLocations(numOutsideRange, radius)
  newLocations[isOutsideRange, "x"] <- replacementLocations$x
  newLocations[isOutsideRange, "y"] <- replacementLocations$y
  # just check it all worked out ok
  newLocations$distance <- round(sqrt(newLocations$x^2 + newLocations$y^2))
  
  # calculate the relative distance change for looking up text
  newLocations$relativeDistance <- pointsOnASmallSquare$distance - newLocations$distance
  # add radius + 1 to make sure the indexes are > 0
  newLocations$relativeDistance <- newLocations$relativeDistance + (radius * 2) + 1
  # sample all the words
  numWords <- max(newLocations$relativeDistance)
  theseWords <- sample_n(mostCommonWords, numWords)
  # make sure these words aren't sampled again, this doesn't work right now
  # mostCommonWords <- mostCommonWords[!(mostCommonWords %in% theseWords)]
  portalLabels <- theseWords[newLocations$relativeDistance, ]
  
  # convert the x, y coords in the their concatenated string name
  newLocations <- newLocations[c("x", "y")]
  newLocations <- data.frame(lapply(newLocations, as.character), stringsAsFactors=FALSE)
  destinationName <- data_frame(name = paste(newLocations$x, newLocations$y, sep = ","))
  
  # replace all the rows corresponding to the victory condition (the radius)
  # with the win name and label
  portalLabels[pointsOnASmallSquare$distance == radius, ] <- "huzzah"
  destinationName[pointsOnASmallSquare$distance == radius, ] <- "win"
  
  # destination string name is all we really care about now
  portalDestinations[[currentPortal]] <- data_frame(destName=destinationName[[1]], label=portalLabels[[1]]) 
  data_frame(destName=destinationName[[1]], label=portalLabels[[1]]) 
}

# this is all messing around with dataframes to make the coords into strings
pointsOnASmallSquare <- data.frame(lapply(pointsOnASmallSquare, as.character), stringsAsFactors=FALSE)
pointsOnASmallSquare$name <- paste(pointsOnASmallSquare$x, pointsOnASmallSquare$y, sep = ",")

# as a test we're shifting all the points down by one space and
# seeing if that works as expected in tiny choice
# numPoints <- length(pointsOnASmallSquare[[1]])
# shiftedPoints <- c(numPoints, seq(numPoints - 1))
# pointsOnASmallSquare$destination <- pointsOnASmallSquare[shiftedPoints, "name"]
# pointsOnASmallSquare$text <- sample(c("l", "r", "u", "d"), numPoints, replace = TRUE)

numPoints <- nrow(pointsOnASmallSquare)
portalStrings <- data.frame(matrix("",nrow=numPoints, ncol=1), stringsAsFactors=FALSE)

for(portal in seq(length(portalDestinations))){
  currentPortal <- data.frame(portalDestinations[[portal]])
  portalStrings <- data_frame(str_c(portalStrings[[1]],
                         currentPortal$label,
                         " -> ",
                         currentPortal$destName,
                         "\n"), stringsAsFactors = FALSE)
}

fullGame <- data.frame(matrix("",nrow=numPoints, ncol=1), stringsAsFactors=FALSE)

fullGame <- str_c(fullGame[[1]],
             # the heading for each point
             "=", 
             pointsOnASmallSquare$name,
             "=\n",
             # the description, currently only the distance from the centre
             sprintf("%03s", pointsOnASmallSquare$distance),
             " / ",
             as.character(radius),      
             "\n\n",
             portalStrings[[1]],
             "\n"
             )

fullGame <- str_c(fullGame, collapse="")

sink("output.txt")
cat(fullGame)
sink()
