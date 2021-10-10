#### Linear Model


### Load in data
data <- read.csv("US_Kickstarters.csv")

### Create TimeDiff variables and add it to the data frame
data$launched <- as.Date(data$launched)
TimeDiff <- as.numeric(difftime(time1 = data$launched, time2 = min(data$launched),
                                units = "days"))
data[20] <- TimeDiff
colnames(data)[20] <- "TimeDiff"


### Create dataset containing only Live states
Live <- data[data$state == "live", ]

### Create dataset that contains only success or faliure states
SuccFail <- data[(data$state == "failed") | (data$state == "successful"), ]


#### Projects with backers and projects without backers should be treated as seperate groups
#### For the linear model, focus on projects that have backers
SuccFail <- SuccFail[SuccFail$backers >= 1,]


### Take a sample of 100,000. then split that into training and test datasets
set.seed(210)
subset.index <- sample(1:nrow(SuccFail), size = 100000)
SUBSET <- SuccFail[subset.index,]

### Create a training and a test data set from SuccFail
### Take the indexes for a traing dataset 3/4 the size of the main data
train.index <- sample(1:nrow(SUBSET), size = 0.75*nrow(SUBSET))

### Create traning dataset
TRAIN <- SUBSET[train.index, ]

### Create a test dataset
TEST <- SUBSET[-train.index, ]


### Set month and year as factor variables
TRAIN$Month <- as.factor(TRAIN$Month)
TRAIN$Year <- as.factor(TRAIN$Year)
TEST$Month <- as.factor(TEST$Month)
TEST$Year <- as.factor(TEST$Year)



#### Modeling

### The intercept is the value when all other predictors are zero. 
### If you have no backers, you can't have money.
### Remove intercept
model1 <- lm(pledged ~ goal + RunTime + backers + TimeDiff + Month + Year + category - 1,
             data = TRAIN)
summary(model1)

### Plot the model
par(mfrow = c(2,2))
plot(model1) ### This will take a sec.

### This is a very bad model. Explore possible power transformation
library(car)

### Shift variables with values of 0 by a minimum amount
X <- cbind(TRAIN$goal,TRAIN$RunTime,TRAIN$backers,TRAIN$TimeDiff + 1)
trans.x <- powerTransform(X) ### This will take a sec.
summary(trans.x)

### Based on this summary, apply a log transform to goal
### a sqare root transform to RunTime, a log transform to backers
### and no tranformation to TimeDiff

model2 <- lm(pledged ~ log(goal) + I(RunTime^0.5) + log(backers) + TimeDiff 
             + Month + Year + category - 1,
             data = TRAIN)
summary(model2)

### Plot new model
plot(model2) ### This will take a sec

### Variance is still not constant.
### In addition, the current model will allow for a negative response.
### You can't have negative money.

### Explore a power transformation for pledged
trans.y <- powerTransform(model2) ### This will take a sec
summary(trans.y)

### Based on this summary, we will apply a log transformation to pledged

model3 <- lm(log(pledged) ~ log(goal) + I(RunTime^0.5) + log(backers) + TimeDiff 
             + Month + Year + category - 1,
             data = TRAIN)
summary(model3)

### Plot new model
plot(model3) ### This will take a bit

### In the current model, TimeDiff and Year are not significant
### Use the likelihood ratio test to remove variables
#### Ho: variable is insiginificant
#### Ha: variable is significant

library(lmtest)

### Test removal of Year
model4 <- lm(log(pledged) ~ log(goal) + I(RunTime^0.5) + log(backers) + TimeDiff 
             + Month + category - 1,
             data = TRAIN)
lrtest(model3, model4)

### Do not remove Year

### Test removal of TimeDiff

model5 <- lm(log(pledged) ~ log(goal) + I(RunTime^0.5) + log(backers)
             + Month + Year + category - 1,
             data = TRAIN)
lrtest(model3,model5)

### Remove TimeDiff
model5 <- lm(log(pledged) ~ log(goal) + I(RunTime^0.5) + log(backers)
             + Month + Year + category - 1,
             data = TRAIN)


summary(model5)
str(TRAIN)
levels(TRAIN$Month)
levels(TRAIN$Year)


### Plot the new model
par(mfrow=c(2,2))
plot(model5,cex.lab = 1, cex.main = 1.5, cex.axis = 1.7) ### This will take a bit

### Not much has changed.
### Look at the residuals across each predictor

par(mfrow=c(3,1))
plot(log(TRAIN$goal), model5$residuals)
plot(I(TRAIN$RunTime^0.5), model5$residuals)
plot(log(TRAIN$backers), model5$residuals)

### The problem is with backers. There is a clear cone shape.


### For fun, lets just see what happens if we remove backers
model6 <- lm(log(pledged) ~ log(goal) + I(RunTime^0.5)
             + Month + Year + category - 1,
             data = TRAIN)
summary(model6)

### Plot model
par(mfrow=c(2,2))
plot(model6)

### WWWWHHHHHHHAAAAAATTT???? This model looks great. Is it too good to be true???

### Compare model 5 and model 6.
### Use marginal model plots to see how well the model fits the data and each variable.
par(mfrow=c(1,1))
library(car)
mmps(model5) ### This will take a while...

### The modeling of goal seems to be a bit ok
### RunTime is modeled pretty well
### Backers and the data as a whole is not modeled well.

mmps(model6) ### This will take a while...

### RunTime isn't modeled well
### Goal and the overall data is modeled pretty well.

### Use Added Variables Plot to look at co-linearity.
### If the blue line is close to/ approximately horizontal, there is no co-linearity

avPlots(model5, terms = ~log(goal) + I(RunTime^0.5) + log(backers))
### This will take a bit...

### As we can see, backers has the largest problem with co-linearity in Model 5

avPlots(model6, terms = ~log(goal) + I(RunTime^0.5))

### The spread between the start and end of the line for goal is a bit large
### The spread between the start and end of the line for RunTime isn't too big



### So what now? HELP!!!
par(mfrow=c(1,1))
cor(log(TRAIN$backers),log(TRAIN$goal))

mod <- lm(log(backers) ~ category, data = TRAIN)
summary(mod)
anova(mod) ### There is high association between backers and sub category


### Make predictions, plot confidence intervals

pred1 <- predict(model5, newdata = TRAIN, interval = "prediction", level = 0.95)
head(pred1)
MSE.LOG1 <- mean((log(TRAIN$pledged) - pred1[,1])^2)
MSE.LOG1

MSE1 <- mean((TRAIN$pledged - exp(pred1[,1]))^2)
MSE1


pred2 <- predict(model5, newdata = TEST, interval = "prediction", level = 0.95)
head(pred2)
MSE.LOG2 <- mean((log(TEST$pledged) - pred2[,1])^2)
MSE.LOG2

MSE2 <- mean((TEST$pledged - exp(pred2[,1]))^2)
MSE2








plot(pred2[seq(1,100),1], pch = 16, ylim = c(0,17), ylab = "log(pledged)",
     main = "Log(Pledged) Predictions", xlab = "First 100 Predictions",
     cex.lab = 1.5, cex.main = 1.5, cex = 1.1, cex.axis = 1.7)
segments(x0 = seq(1,100), y0 = pred2[seq(1,100),1],
         x1 = seq(1,100), y1 = pred2[seq(1,100),2], lwd = 1.5)
segments(x0 = seq(1,100), y0 = pred2[seq(1,100),1],
         x1 = seq(1,100), y1 = pred2[seq(1,100),3], lwd = 1.5)
points(log(TEST$pledged[seq(1,100)]), pch = 15, col = "red", cex = 1.1)
legend("topleft",legend = c("Predicted Value", "True Value", "95% Prediction Interval"),
       pch = c(16,15), col = c("black","red","black"), lty=c(0,0,1))



MonConf <- confint(model5, parm = seq(4,15))
MonConf[1,]



plot(model5$coefficients[seq(4,15)], xlab = "Month", ylab = "Log(Estimated Effect)",
     main = "Month Level Effects", 
     ylim = c(min(model5$coefficients[seq(4,15)]) - 0.3, 
              max(model5$coefficients[seq(4,15)]) + 0.3),
     pch = 16, cex.lab = 1.5, cex.main = 1.5, cex = 1.5, cex.axis = 1.7)
segments(x0 = seq(1,12), y0 = model5$coefficients[seq(4,15)],
         x1 = seq(1,12), y1 = MonConf[,1], lwd = 1.5)
segments(x0 = seq(1,12), y0 = model5$coefficients[seq(4,15)],
         x1 = seq(1,12), y1 = MonConf[,2], lwd = 1.5)



model5.1 <- lm(log(pledged) ~ log(goal) + I(RunTime^0.5) + log(backers)
               + Year + Month + category - 1,
               data = TRAIN)
summary(model5.1)

model5.2 <- lm(log(pledged) ~ log(goal) + I(RunTime^0.5) + log(backers) + category
               + Month + Year  - 1,
               data = TRAIN)
summary(model5.2)




YearConf <- confint(model5.1, parm = seq(4,12))
YearConf

Y <- c("2009","2010","2011","2012","2013","2014","2015","2016","2017")

plot(Y, model5.1$coefficients[seq(4,12)], xlab = "Year", ylab = "Log(Estimated Effect)",
     main = "Year Level Effects", 
     ylim = c(min(model5.1$coefficients[seq(4,12)]) - 0.2, 
              max(model5.1$coefficients[seq(4,12)]) + 0.2),
     pch = 16,cex.lab = 1.5, cex.main = 1.5, cex = 1.5, cex.axis = 1.7)
segments(x0 = seq(2009,2017), y0 = model5.1$coefficients[seq(4,12)],
         x1 = seq(2009,2017), y1 = YearConf[,1], lwd = 1.5)
segments(x0 = seq(2009,2017), y0 = model5.1$coefficients[seq(4,12)],
         x1 = seq(2009,2017), y1 = YearConf[,2], lwd = 1.5)


CATS <- confint(model5.2, parm = seq(4,162))
CATS




#colfun <- colorRampPalette(c("pink","red","yellow","green","blue","cyan","magenta","purple","black"))
length(colors())

set.seed(210)
COLORS <- sample(colors()[seq(2,657)], size = 159)

set.seed(210)
PCH <- sample(seq(15,20), size = 159, replace = T)


plot(seq(1,159),model5.2$coefficients[seq(4,162)], col = COLORS, pch = PCH, cex = 1.2,
     ylim = c(min(CATS[,1])-0.1, max(CATS[,2])+0.1),
     main = "Subcategory Effects", xlab = "Subcategory #", ylab = "Log(Estimated Effect)",
     cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.7)
segments(x0 = seq(1,159), y0 = model5.2$coefficients[seq(4,162)],
         x1 = seq(1,159), y1 = CATS[,1], lwd = 1.5, col = COLORS)
segments(x0 = seq(1,159), y0 = model5.2$coefficients[seq(4,162)],
         x1 = seq(1,159), y1 = CATS[,2], lwd = 1.5, col = COLORS)

plot.new()
legend("center",legend = levels(TRAIN$category[seq(1,159)]), pch = PCH,
       col = COLORS, ncol = 7)

summary(model5)
confint(model5)

table(TRAIN$Year)        
str(TRAIN)                


levels(data$main_category)
table(data$category)

ART <- names(which(table((data[data$main_category == "Art",])$category) > 0))
COMICS<- names(which(table((data[data$main_category == "Comics",])$category) > 0))
CRAFTS<- names(which(table((data[data$main_category == "Crafts",])$category) > 0))
DANCE<- names(which(table((data[data$main_category == "Dance",])$category) > 0))
DESIGN<- names(which(table((data[data$main_category == "Design",])$category) > 0))
FASHION<- names(which(table((data[data$main_category == "Fashion",])$category) > 0))
FILMVIDEO<- names(which(table((data[data$main_category == "Film & Video",])$category) > 0))
FOOD<- names(which(table((data[data$main_category == "Food",])$category) > 0))
GAMES<- names(which(table((data[data$main_category == "Games",])$category) > 0))
JOURNAL<- names(which(table((data[data$main_category == "Journalism",])$category) > 0))
MUSIC<- names(which(table((data[data$main_category == "Music",])$category) > 0))
PHOTO<- names(which(table((data[data$main_category == "Photography",])$category) > 0))
PUBLISH<- names(which(table((data[data$main_category == "Publishing",])$category) > 0))
TECH<- names(which(table((data[data$main_category == "Technology",])$category) > 0))
THEATER<- names(which(table((data[data$main_category == "Theater",])$category) > 0))






ARTest <- model5.2$coefficients[c(paste("category",ART,sep = ""))]
COMICSest<-model5.2$coefficients[c(paste("category",COMICS,sep = ""))]
CRAFTSest<-model5.2$coefficients[c(paste("category",CRAFTS,sep = ""))]
DANCEest<- model5.2$coefficients[c(paste("category",DANCE,sep = ""))]
DESIGNest<- model5.2$coefficients[c(paste("category",DESIGN,sep = ""))]
FASHIONest<- model5.2$coefficients[c(paste("category",FASHION,sep = ""))]
FILMVIDEOest<-model5.2$coefficients[c(paste("category",FILMVIDEO,sep = ""))]
FOODest<-model5.2$coefficients[c(paste("category",FOOD,sep = ""))]
GAMESest<-model5.2$coefficients[c(paste("category",GAMES,sep = ""))]
JOURNALest<-model5.2$coefficients[c(paste("category",JOURNAL,sep = ""))]
MUSICest<-model5.2$coefficients[c(paste("category",MUSIC,sep = ""))]
PHOTOest<-model5.2$coefficients[c(paste("category",PHOTO,sep = ""))]
PUBLISHest<- model5.2$coefficients[c(paste("category",PUBLISH,sep = ""))]
TECHest<- model5.2$coefficients[c(paste("category",TECH,sep = ""))]
THEATERest<- model5.2$coefficients[c(paste("category",THEATER,sep = ""))]










ARTci <- confint(model5.2, parm = c(paste("category",ART,sep = "")))
COMICSci<- confint(model5.2, parm = c(paste("category",COMICS,sep = "")))
CRAFTSci<- confint(model5.2, parm = c(paste("category",CRAFTS,sep = "")))
DANCEci<- confint(model5.2, parm = c(paste("category",DANCE,sep = "")))
DESIGNci<-confint(model5.2, parm = c(paste("category",DESIGN,sep = "")))
FASHIONci<-confint(model5.2, parm = c(paste("category",FASHION,sep = "")))
FILMVIDEOci<-confint(model5.2, parm = c(paste("category",FILMVIDEO,sep = "")))
FOODci<- confint(model5.2, parm = c(paste("category",FOOD,sep = "")))
GAMESci<-confint(model5.2, parm = c(paste("category",GAMES,sep = "")))
JOURNALci<-confint(model5.2, parm = c(paste("category",JOURNAL,sep = "")))
MUSICci<- confint(model5.2, parm = c(paste("category",MUSIC,sep = "")))
PHOTOci<- confint(model5.2, parm = c(paste("category",PHOTO,sep = "")))
PUBLISHci<-confint(model5.2, parm = c(paste("category",PUBLISH,sep = "")))
TECHci<- confint(model5.2, parm = c(paste("category",TECH,sep = "")))
THEATERci<-confint(model5.2, parm = c(paste("category",THEATER,sep = "")))





plot(seq(1,171), type = "n", ylim = c(-0.175367, 3.75555),
     main = "Subcategory Effects", xlab = "Subcategory #", ylab = "Log(Estimated Effect)",
     cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.7)

AddCI <- function(x, Coeffs, CI, pointnum, COL){
  points(x,Coeffs, pch = rep(c(15,16,17,18), length.out = pointnum), cex = 1.2,
         col = COL)
  segments(x0 = x, y0 = Coeffs,
           x1 = x, y1 = CI[,1], lwd = 1.5, col = COL)
  segments(x0 = x, y0 = Coeffs,
           x1 = x, y1 = CI[,2], lwd = 1.5, col = COL)
  
  
}


AddCI(x= seq(1,13), Coeffs = ARTest, CI= ARTci, 
      pointnum = length(ART), COL = "red")
AddCI(x= seq(14,19), Coeffs = COMICSest, CI= COMICSci, 
      pointnum = length(COMICS), COL = "blue")
AddCI(x= seq(20,34), Coeffs = CRAFTSest, CI= CRAFTSci, 
      pointnum = length(CRAFTS), COL = "green")
AddCI(x= seq(35,39), Coeffs = DANCEest, CI= DANCEci, 
      pointnum = length(DANCE), COL = "orange")
AddCI(x= seq(40,46), Coeffs = DESIGNest, CI= DESIGNci, 
      pointnum = length(DESIGN), COL = "purple")
AddCI(x= seq(47,55), Coeffs = FASHIONest, CI= FASHIONci, 
      pointnum = length(FASHION), COL = "pink")
AddCI(x= seq(56,75), Coeffs = FILMVIDEOest, CI= FILMVIDEOci, 
      pointnum = length(FILMVIDEO), COL = "black")
AddCI(x= seq(76,88), Coeffs = FOODest, CI= FOODci, 
      pointnum = length(FOOD), COL = "yellow")
AddCI(x= seq(89,96), Coeffs = GAMESest, CI= GAMESci, 
      pointnum = length(GAMES), COL = "grey")
AddCI(x= seq(97,102), Coeffs = JOURNALest, CI= JOURNALci, 
      pointnum = length(JOURNAL), COL = "tan")
AddCI(x= seq(103,121), Coeffs = MUSICest, CI= MUSICci, 
      pointnum = length(MUSIC), COL = "maroon")
AddCI(x= seq(122,128), Coeffs = PHOTOest, CI= PHOTOci, 
      pointnum = length(PHOTO), COL = "cyan")
AddCI(x= seq(129,146), Coeffs = PUBLISHest, CI= PUBLISHci, 
      pointnum = length(PUBLISH), COL = "darkolivegreen")
AddCI(x= seq(147,162), Coeffs = TECHest, CI= TECHci, 
      pointnum = length(TECH), COL = "aquamarine")
AddCI(x= seq(163,170), Coeffs = THEATERest, CI= THEATERci, 
      pointnum = length(THEATER), COL = "magenta")

legend("topright",legend = levels(data$main_category),
       col = c("red","blue","green","orange","purple","pink",
               "black","yellow","grey","tan","maroon","cyan","darkolivegreen",
               "aquamarine","magenta"),ncol = 5,
       text.col = c("red","blue","green","orange","purple","pink",
                    "black","yellow","grey","tan","maroon","cyan","darkolivegreen",
                    "aquamarine","magenta"), text.width =15, cex = 1.25)


plot.new()
legend("center",legend = c(ART,COMICS,CRAFTS,DANCE,DESIGN,FASHION,
                           FILMVIDEO,FOOD,GAMES,JOURNAL,MUSIC,PHOTO,PUBLISH,
                           TECH,THEATER), pch = c(rep(c(15,16,17,18), 
                                                      length.out = length(ART)),
                                                  rep(c(15,16,17,18), 
                                                      length.out = length(COMICS)),
                                                  rep(c(15,16,17,18), 
                                                      length.out = length(CRAFTS)),
                                                  rep(c(15,16,17,18), 
                                                      length.out = length(DANCE)),
                                                  rep(c(15,16,17,18), 
                                                      length.out = length(DESIGN)),
                                                  rep(c(15,16,17,18), 
                                                      length.out = length(FASHION)),
                                                  rep(c(15,16,17,18), 
                                                      length.out = length(FILMVIDEO)),
                                                  rep(c(15,16,17,18), 
                                                      length.out = length(FOOD)),
                                                  rep(c(15,16,17,18), 
                                                      length.out = length(GAMES)),
                                                  rep(c(15,16,17,18), 
                                                      length.out = length(JOURNAL)),
                                                  rep(c(15,16,17,18), 
                                                      length.out = length(MUSIC)),
                                                  rep(c(15,16,17,18), 
                                                      length.out = length(PHOTO)),
                                                  rep(c(15,16,17,18), 
                                                      length.out = length(PUBLISH)),
                                                  rep(c(15,16,17,18), 
                                                      length.out = length(TECH)),
                                                  rep(c(15,16,17,18), 
                                                      length.out = length(THEATER))),
       col = c(rep("red", 
                   length.out = length(ART)),
               rep("blue", 
                   length.out = length(COMICS)),
               rep("green", 
                   length.out = length(CRAFTS)),
               rep("orange", 
                   length.out = length(DANCE)),
               rep("purple", 
                   length.out = length(DESIGN)),
               rep("pink", 
                   length.out = length(FASHION)),
               rep("black",
                   length.out = length(FILMVIDEO)),
               rep("yellow", 
                   length.out = length(FOOD)),
               rep("grey", 
                   length.out = length(GAMES)),
               rep("tan", 
                   length.out = length(JOURNAL)),
               rep("maroon", 
                   length.out = length(MUSIC)),
               rep("cyan", 
                   length.out = length(PHOTO)),
               rep("darkolivegreen", 
                   length.out = length(PUBLISH)),
               rep("aquamarine", 
                   length.out = length(TECH)),
               rep("magenta", 
                   length.out = length(THEATER))),
       ncol = 8)




levels(data$main_category)
length(levels(TRAIN$category))
colors()

                 