library(lattice)
library(car)


US <- read.csv("US_Kickstarters.csv")

US$'log(goal)' <- log(US$goal)
US$'log(pledged)' <- log(US$pledged + 0.01)
US$'log(backers)' <- log(US$backers + 1)
US$'log(PercentGoal)' <- log(US$PercentGoal + 0.01)

US.SuccFail <- US[(US$state == "failed") | (US$state == "successful"), ]

set.seed(210)
sub <- sample(1:nrow(US.SuccFail), size = 10000)
US.SuccFail <- US.SuccFail[sub,]


colClass <- rep(NA, times = nrow(US.SuccFail))
for (i in 1:nrow(US.SuccFail)){
  
  if (US.SuccFail$state[i] == "failed"){
    
    colClass[i] <- "red"
  }
  
  if (US.SuccFail$state[i] == "successful"){
    
    colClass[i] <- "blue"
  }
  
}





### density plots for untransformed variables
densityplot(~US.SuccFail$PercentGoal|US.SuccFail$main_category, main="Density Plot by Main Category", xlab="Percent Goal Reached",pch=20, col = colClass)

densityplot(~US.SuccFail$backers|US.SuccFail$main_category, main="Density Plot by Main Category", xlab="Number of Backers",pch=20, col = colClass)

densityplot(~US.SuccFail$goal|US.SuccFail$main_category, main="Density Plot by Main Category", xlab="Project Goal",pch=20, col = colClass)

### density plots for transformed variables 
densityplot(~US.SuccFail$'log(PercentGoal'|US.SuccFail$main_category, main="Density Plot by Main Category", xlab="log(Percent Goal Reached)",pch=20, col = colClass)

densityplot(~US.SuccFail$'log(backers)'|US.SuccFail$main_category, main="Density Plot by Main Category", xlab="log(Number of Backers)",pch=20, col = colClass)

densityplot(~US.SuccFail$'log(goal)'|US.SuccFail$main_category, main="Density Plot by Main Category", xlab="log(Project Goal)",pch=20, col = colClass)

### boxplots for transformed variables 

bwplot(~US.SuccFail$'log(PercentGoal'|US.SuccFail$main_category, main="Box Plot by Main Category", xlab="log(Percent Goal Reached)",pch=20, col = colClass)

bwplot(~US.SuccFail$'log(backers)'|US.SuccFail$main_category, main="Box Plot by Main Category", xlab="log(Number of Backers)",pch=20, col = colClass)

bwplot(~US.SuccFail$'log(goal)'|US.SuccFail$main_category, main="Box Plot by Main Category", xlab="log(Project Goal)",pch=20, col = colClass)

### scatterplots for untransformed variables 

xyplot(US.SuccFail$PercentGoal~US.SuccFail$backers|US.SuccFail$main_category, main="Scatterplot of Percent Goal Reached vs. Number of Backers by Main Categories", xlab="Percent Goal Reached",ylab="Number of Backers",pch=20, col = colClass) 

xyplot(US.SuccFail$PercentGoal~US.SuccFail$goal|US.SuccFail$main_category, main="Scatterplot of Percent Goal Reached vs. Project Goal by Main Categories", xlab="Percent Goal Reached",ylab="Project Goal",pch=20, col = colClass) 

### scatterplots for transformed variables

xyplot(US.SuccFail$PercentGoal~US.SuccFail$backers|US.SuccFail$main_category, main="Scatterplot of log(Percent Goal Reached) vs. log(Number of Backers) by Main Categories", xlab="log(Percent Goal Reached)",ylab="log(Number of Backers)",pch=20, col = colClass)

xyplot(US.SuccFail$'log(PercentGoal)'~US.SuccFail$'log(goal)'|US.SuccFail$main_category, main="Scatterplot of log(Percent Goal Reached) vs. log(Project Goal) by Main Categories", xlab="log(Percent Goal Reached)",ylab="log(Project Goal)",pch=20, col = colClass)



###########################################

bwplot(~US.SuccFail$backers|US.SuccFail$main_category, main="Box Plot by Main Category", xlab="Number of Backers",pch=20, col = colClass)
bwplot(~US.SuccFail$'log(backers)'|US.SuccFail$main_category,
       main=list("Box Plot by Main Category", fontsize = 18),
       xlab=list("log(Number of Backers)",fontsize = 18),pch=20,
       col = colClass, scale = list(cex = 1.5))


bwplot(~US.SuccFail$pledged|US.SuccFail$main_category, main="Box Plot by Main Category", xlab="Pledged",pch=20, col = colClass)
bwplot(~US.SuccFail$'log(pledged)'|US.SuccFail$main_category, 
       main=list("Box Plot by Main Category",fontsize = 18), 
       xlab=list("log(Percent Goal Reached)",fontsize = 18),pch=20,
       col = colClass, scale = list(cex = 1.5))




xyplot(US.SuccFail$'log(backers)'~US.SuccFail$'log(pledged)'|US.SuccFail$main_category, 
       main=list("Scatterplot of log(Backers) vs. log(Pledged) by Main Categories", fontsize = 18),
       xlab=list("log(Backers)", fontsize = 18),ylab=list("log(Pledged)", fontsize = 18),
       pch=20, col = colClass, scale = list(cex=1.5))









