

### Load data
data <- read.csv("US_Kickstarters.csv")


### Create dataset that contains only success or faliure states
SuccFail <- data[(data$state == "failed") | (data$state == "successful"), ]


set.seed(210)
sub <- sample(1:nrow(SuccFail), size = 10000)
subset <- SuccFail[sub,]


colClass <- rep(NA, times = nrow(subset))
for (i in 1:nrow(subset)){
  
  if (subset$state[i] == "failed"){
    
    colClass[i] <- "red"
  }
  
  if (subset$state[i] == "successful"){
    
    colClass[i] <- "blue"
  }
 
}

plot(subset[,c(7,9,11,16,17)], col = colClass, lower.panel = NULL, main = "Continuous Variables",
     cex.main = 1.5, cex.lab = 1.5)
legend(x = 0.1, y = 0.55, legend = c("successful", "failed"), col = c("blue","red"),
      pch = c(1,1), cex = 2)

createHist <- function(variable, name){
  
  hist(variable, main = paste("Histogram of ", name, sep=""), xlab = name,
       cex.main = 1.8, cex.lab = 1.8, cex.axis = 1.8)
  abline(v = mean(variable), col = "green", lwd = 3)
  abline(v = median(variable), col = "purple", lwd = 3)
  
}
plot.new()

legend("center",legend = c("Mean","Median"), col = c("green","purple"), lwd = 3, lty = 1,
       cex = 1.5)


par(mfrow=c(3,2))
createHist(subset$goal, "Goal")
createHist(subset$pledged, "Pledged")
createHist(subset$backers, "Backers")
createHist(subset$RunTime, "RunTime")
createHist(subset$PercentGoal, "PercentGoal")


subset$pledged <- subset$pledged + 0.01
subset$backers <- subset$backers + 1
subset$PercentGoal <- subset$PercentGoal + 0.01


par(mfrow = c(1,1))
plot(log(subset[,c(7,9,11,17)]), col = colClass, lower.panel = NULL, 
     main = "Log of Continuous Variables", cex.main = 2, cex.lab = 2,
     cex.axis = 2)
legend(x = 0.1, y = 0.48, legend = c("successful", "failed"), col = c("blue","red"),
       pch = c(1,1), cex = 1.5)


par(mfrow=c(2,2))
createHist(log(subset$goal), "Log of Goal")
createHist(log(subset$pledged), "Log of Pledged")
createHist(log(subset$backers), "Log of Backers")
createHist(log(subset$PercentGoal), "Log of PercentGoal")


Y1 <- table(subset$Month[subset$Year == "2009"])

Y1 <- c(0,0,0,0,1,2,0,4,3,4,5,3)
Y2 <- as.vector(table(subset$Month[subset$Year == "2010"]))
Y3 <- as.vector(table(subset$Month[subset$Year == "2011"]))
Y4 <- as.vector(table(subset$Month[subset$Year == "2012"]))
Y5 <- as.vector(table(subset$Month[subset$Year == "2013"]))
Y6 <- as.vector(table(subset$Month[subset$Year == "2014"]))
Y7 <- as.vector(table(subset$Month[subset$Year == "2015"]))
Y8 <- as.vector(table(subset$Month[subset$Year == "2016"]))
Y9 <- as.vector(table(subset$Month[subset$Year == "2017"]))

MONTHS <- seq(from = 1 , to = 108)


VALUES <- c(cbind(Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9))

col1 <- rep("green", times = 12)
col2 <- rep("red", times = 12)
col3 <- rep("blue", times = 12)
col4 <- rep("black", times = 12)
col5 <- rep("yellow", times = 12)
col6 <- rep("purple", times = 12)
col7 <- rep("brown", times = 12)
col8 <- rep("grey", times = 12)
col9 <- rep("orange", times = 12)

cols <- c(cbind(col1,col2,col3,col4,col5,col6,col7,col8,col9))

par(mfrow=c(1,1))
plot(MONTHS, VALUES, xlim = c(5,109), col = cols, pch = 16,
     main = "# of Projects Through Time", xlab = "# of Months (Start: May 2009)",
     ylab = "# of Projects", cex.main = 2, cex.lab = 1.5, cex = 1.2, cex.axis = 1.5)
lines(MONTHS,VALUES, xlim = c(5,109))
legend("topleft", legend = c("2009","2010","2011","2012","2013","2014","2015","2016","2017"),
       pch = 16, col = c("green","red", "blue","black","yellow","purple","brown","grey",
                         "orange"), cex = 1.1)


### Usa lattice package to seperate graphs by categories
library(plotly)

vals1 <- as.vector(table(US.SuccFail$main_category[US.SuccFail$state == "failed"]))
vals2 <- as.vector(table(US.SuccFail$main_category[US.SuccFail$state == "successful"]))

plot_ly(data = US.SuccFail, x = ~levels(US.SuccFail$main_category), y = ~vals1, 
        type = 'bar', name = 'Failed',
        text = ~vals1, textposition = 'auto') %>%
  add_trace(y = ~vals2, name = 'Successful', text = ~vals2) %>%
  layout(yaxis = list(title = "Number of Kickstarters"),
         xaxis = list(title = "Main Categories"), barmode = 'stack',
         font = list(size = 20))


















