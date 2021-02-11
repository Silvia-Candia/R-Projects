#logistic map

#setwd("~/PhD project/R projects")

#Define the logistic map x_(n+1_) as a function of lambda and x_n 
#(in this form the maximum value for xn is 1, xn = N/K) :
xn1 <- function(lambda,xn) {
  lambda*xn*(1-xn)
}

#Generate a sequence for different lambda parameters:
lambda <- seq(1.2,4, by=0.2)
#Initial population
x0 <- 0.001
#The number of xn points:
npoints <- 50

#The data will be stored in a dataframe, the columns will be the different lambdas.
#Create data frame with npoints rows and n(lambda) columns plus 1 for the time step:
logmap <- data.frame(matrix(0, nrow=npoints, ncol=(length(lambda)+1)))
colnames(logmap)[1] <- "x"
logmap$x <- 1:npoints

#I calculate xn for each lambda. Columns are named with the respective l value:
for(j in 2:ncol(logmap)) {
  #assign column name
  colnames(logmap)[j] <- paste("l", lambda[j-1], sep="")
  #calculate:
  #initialise population with x0:
  logmap[1,j] <- x0
  for(i in 2:nrow(logmap)) {
    logmap[i,j] <- xn1(lambda[j-1], logmap[(i-1),j])
  }
}

#plot the results
#plot.new()
#to use ggplot I need to convert the dataframe to long format:
logmap_long <- gather_(logmap, "lambda", "value", colnames(logmap[2:ncol(logmap)]))

print(ggplot(logmap_long, aes(x,value)) 
      + geom_point(aes(colour=lambda), shape=1) + scale_shape(solid=FALSE)
      + geom_line(aes(colour=lambda))
      
      + scale_y_continuous(c(0.1)))