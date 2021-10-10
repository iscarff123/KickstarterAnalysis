

data <- read.csv("US_Kickstarters.csv")
data$launched <- as.Date(data$launched)

### Create new variable to keep track of how far in time a new project
### is from the first
TimeDiff <- as.numeric(difftime(time1 = data$launched, time2 = min(data$launched),
                                units = "days"))

head(TimeDiff)

hist(TimeDiff)
abline(v = mean(TimeDiff), col = "red")
abline(v = median(TimeDiff), col = "blue")

#str(TimeDiff)


data[20] <- TimeDiff
colnames(data)[20] <- "TimeDiff"




### Create dataset containing only Live states
Live <- data[data$state == "live", ]

### Create dataset that contains only success or faliure states
SuccFail <- data[(data$state == "failed") | (data$state == "successful"), ]


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


STATE <- ifelse(TRAIN$state == "failed", 0, 1)
head(STATE, n = 10)



str(TRAIN)
TRAIN$Month <- as.factor(TRAIN$Month)
TRAIN$Year <- as.factor(TRAIN$Year)
str(TRAIN)

TEST$Month <- as.factor(TEST$Month)
TEST$Year <- as.factor(TEST$Year)


## Do not use percent goal because it gives the answer. 
## Do not use main categories because it is explained by sub categories


model1 <- glm(STATE ~ goal + RunTime + backers + TimeDiff + Month + Year + category,
                  family = "binomial", data  = TRAIN, maxit = 300, epsilon = 1e-8,
                  trace = T)
summary(model1)


### Test for power transformations on continuous variables
### Shift variables with 0 values

library(car)
X <- cbind(TRAIN$goal,TRAIN$RunTime,TRAIN$backers + 1,TRAIN$TimeDiff + 1)
trans.x <- powerTransform(X)
summary(trans.x)

### Apply transformations
TRAIN$backers <- TRAIN$backers + 1
model2 <- glm(STATE ~ log(goal) + I(RunTime^0.5) + log(backers) + TimeDiff
              + Month + Year + category,
              family = "binomial", data  = TRAIN, maxit = 300, epsilon = 1e-8,
              trace = T)
summary(model2)
### AIC = 25242

### Timediff might already explain Month and Year
### Timediff is barely significant

model2.2 <- glm(STATE ~ log(goal) + I(RunTime^0.5) + log(backers) + TimeDiff
               + Year + category,
                family = "binomial", data  = TRAIN, maxit = 300, epsilon = 1e-8,
                trace = T)
summary(model2.2)
### AIC = 25240

### Separate, each year is not significant, but the whole variable is.


anova(model2,model2.2, test = "Chisq")



model3 <- glm(STATE ~ log(goal) + I(RunTime^0.5) + log(backers) + TimeDiff + category,
              family = "binomial", data  = TRAIN, maxit = 300, epsilon = 1e-8,
              trace = T)
summary(model3)

confint(model3, parm = c(2,3,4,5))
### AIC = 25330

### Use anova to test removal of month and year

anova(model2.2,model3, test = "Chisq")
### Year is significant

## Due to large sample size, p-value is signifiacnt.
# Practically, not signfi. the difference deviance and AIC are not too different.
# TimeDiff probably already explains Year. Remove year.


### Predict over training data set for model2

pre.train.mod2 <- predict(model2, newdata = TRAIN, type = "response")
### Predict glm gives predictions in logit scale. 
### Select type = response to give probabalities

head(pre.train.mod2)

### Add confusion matrix
### Set arbitrary success boundry at 0.75

pre.train.mod2.IsSucc <- ifelse(pre.train.mod2 >= 0.75, 1, 0)
pre.train.mod2.ConfMat <- table(pre.train.mod2.IsSucc, STATE)
pre.train.mod2.ConfMat

### Accuracy
pre.train.mod2.Acc <- (pre.train.mod2.ConfMat[1,1] + pre.train.mod2.ConfMat[2,2]) / 
  sum(pre.train.mod2.ConfMat)
paste("Model 2 has an accuracy of ", pre.train.mod2.Acc * 100, "% across the training data set")





### Predict over training data set for model3

pre.train.mod3 <- predict(model3, newdata = TRAIN, type = "response")
### Predict glm gives predictions in logit scale. 
### Select type = response to give probabalities

head(pre.train.mod3)

### Add confusion matrix
### Set arbitrary success boundry at 0.75

pre.train.mod3.IsSucc <- ifelse(pre.train.mod3 >= 0.75, 1, 0)
pre.train.mod3.ConfMat <- table(pre.train.mod3.IsSucc, STATE)
pre.train.mod3.ConfMat

### Accuracy
pre.train.mod3.Acc <- (pre.train.mod3.ConfMat[1,1] + pre.train.mod3.ConfMat[2,2]) / 
  sum(pre.train.mod3.ConfMat)
paste("Model 3 has an accuracy of ", pre.train.mod3.Acc * 100, "% across the training data set")





### Now make predictions on test data sets

### Make another STATE variable for test data

STATE.test <- ifelse(TEST$state == "failed", 0, 1)

### Model 2
pre.test.mod2 <- predict(model2, newdata = TEST, type = "response")

### Predict glm gives predictions in logit scale. 
### Select type = response to give probabalities

head(pre.test.mod2)

### Add confusion matrix
### Set arbitrary success boundry at 0.75

pre.test.mod2.IsSucc <- ifelse(pre.test.mod2 >= 0.75, 1, 0)
pre.test.mod2.ConfMat <- table(pre.test.mod2.IsSucc, STATE.test)
pre.test.mod2.ConfMat

### Accuracy
pre.test.mod2.Acc <- (pre.test.mod2.ConfMat[1,1] + pre.test.mod2.ConfMat[2,2]) / 
  sum(pre.test.mod2.ConfMat)
paste("Model 2 has an accuracy of ", pre.test.mod2.Acc * 100, "% across the test data set")



### Model 3
pre.test.mod3 <- predict(model3, newdata = TEST, type = "response")

### Predict glm gives predictions in logit scale. 
### Select type = response to give probabalities

head(pre.test.mod3)

### Add confusion matrix
### Set arbitrary success boundry at 0.75

pre.test.mod3.IsSucc <- ifelse(pre.test.mod3 >= 0.75, 1, 0)
pre.test.mod3.ConfMat <- table(pre.test.mod3.IsSucc, STATE.test)
pre.test.mod3.ConfMat

### Accuracy
pre.test.mod3.Acc <- (pre.test.mod3.ConfMat[1,1] + pre.test.mod3.ConfMat[2,2]) / 
  sum(pre.test.mod3.ConfMat)
paste("Model 3 has an accuracy of ", pre.test.mod3.Acc * 100, "% across the test data set")



### try to make logistic regression plot

### In training data set
### Goal
model.goal <- glm(STATE ~ log(goal), data = TRAIN, family = "binomial")
summary(model.goal)
range(TRAIN$goal)
x <- seq(99,50001, 0.01)
y <- predict(model.goal, list(goal = x),type="response")
plot(TRAIN$goal,STATE, xlab = "Goal", ylab = "Prob(Success)", 
     main = "The effect of Goal on Probability of Success")
lines(x,y, lwd = 2, col = "blue")

### RunTime
model.RunTime <- glm(STATE ~ I(RunTime^0.5), data = TRAIN, family = "binomial")
summary(model.goal)
range(TRAIN$RunTime)
x1 <- seq(0,61, 0.01)
y1 <- predict(model.RunTime, list(RunTime = x1),type="response")
plot(TRAIN$RunTime,STATE, xlab = "RunTime", ylab = "Prob(Success)", 
     main = "The effect of RunTime on Probability of Success")
lines(x1,y1, lwd = 2, col = "blue")

### Backers
model.backers <- glm(STATE ~ log(backers), data = TRAIN, family = "binomial")
summary(model.backers)
range(TRAIN$backers)
x2 <- seq(0,219384, 0.01)
y2 <- predict(model.backers, list(backers = x2),type="response")
plot(TRAIN$backers,STATE, xlab = "Backers", ylab = "Prob(Success)", 
     main = "The effect of Backers on Probability of Success")
lines(x2,y2, lwd = 2, col = "blue")


### TimeDiff
model.TimeDiff <- glm(STATE ~ TimeDiff, data = TRAIN, family = "binomial")
summary(model.TimeDiff)
range(TRAIN$TimeDiff)
x3 <- seq(8,3175, 0.01)
y3 <- predict(model.TimeDiff, list(TimeDiff = x3),type="response")
plot(TRAIN$TimeDiff,STATE, xlab = "Time Difference", ylab = "Prob(Success)", 
     main = "The effect of TimeDiff on Probability of Success")
lines(x3,y3, lwd = 2, col = "blue")



par(mfrow = c(2,2))
plot(TRAIN$goal,STATE, xlab = "Goal", ylab = "Prob(Success)", 
     main = "The effect of Goal on Prob(Success)")
lines(x,y, lwd = 2, col = "blue")
plot(TRAIN$RunTime,STATE, xlab = "RunTime", ylab = "Prob(Success)", 
     main = "The effect of RunTime on Prob(Success)")
lines(x1,y1, lwd = 2, col = "blue")
plot(TRAIN$backers,STATE, xlab = "Backers", ylab = "Prob(Success)", 
     main = "The effect of Backers on Prob(Success)")
lines(x2,y2, lwd = 2, col = "blue")
plot(TRAIN$TimeDiff,STATE, xlab = "Time Difference", ylab = "Prob(Success)", 
     main = "The effect of TimeDiff on Prob(Success)")
lines(x3,y3, lwd = 2, col = "blue")



### Accuracy
library(ROCR)

par(mfrow = c(2,2))


pred1 <- prediction(pre.train.mod2, STATE)
eval1 <- performance(pred1, "acc")
plot(eval1, main = "Accuracy Curve, Training, Model 1", cex.lab = 1.5, cex.main = 1.5)
points(x = 0.75, y = pre.train.mod2.Acc, col = "red", pch = 16, cex = 2)

pred2 <- prediction(pre.train.mod3, STATE)
eval2 <- performance(pred2, "acc")
plot(eval2, main = "Accuracy Curve, Training, Model 2", cex.lab = 1.5, cex.main = 1.5)
points(x = 0.75, y = pre.train.mod3.Acc, col = "red", pch = 16, cex = 2)

pred3 <- prediction(pre.test.mod2, STATE.test)
eval3 <- performance(pred3, "acc")
plot(eval3, main = "Accuracy Curve, Test, Model 1", cex.lab = 1.5, cex.main = 1.5)
points(x = 0.75, y = pre.test.mod2.Acc, col = "red", pch = 16, cex = 2)

pred4 <- prediction(pre.test.mod3, STATE.test)
eval4 <- performance(pred4, "acc")
plot(eval4, main = "Accuracy Curve", cex.lab = 1.5, cex.main = 1.5)
points(x = 0.75, y = pre.test.mod3.Acc, col = "red", pch = 16, cex = 2)


### ROC
roc1 <- performance(pred1, "tpr","fpr")
plot(roc1, main = "ROC Curve, Training, Model 1", cex.lab = 1.5, cex.main = 1.5)
TPr1 <- pre.train.mod2.ConfMat[2,2]/sum(pre.train.mod2.ConfMat[2,])
FPr1 <- pre.train.mod2.ConfMat[1,2]/sum(pre.train.mod2.ConfMat[1,])
points(x = FPr1, y = TPr1 , col = "red", pch = 16, cex = 2)


roc2 <- performance(pred2, "tpr","fpr")
plot(roc2, main = "ROC Curve, Training, Model 2", cex.lab = 1.5, cex.main = 1.5)
TPr2 <- pre.train.mod3.ConfMat[2,2]/sum(pre.train.mod3.ConfMat[2,])
FPr2 <- pre.train.mod3.ConfMat[1,2]/sum(pre.train.mod3.ConfMat[1,])
points(x = FPr2, y = TPr2 , col = "red", pch = 16, cex = 2)


roc3 <- performance(pred3, "tpr","fpr")
plot(roc3, main = "ROC Curve, Test, Model 1", cex.lab = 1.5, cex.main = 1.5)
TPr3 <- pre.test.mod2.ConfMat[2,2]/sum(pre.test.mod2.ConfMat[2,])
FPr3 <- pre.test.mod2.ConfMat[1,2]/sum(pre.test.mod2.ConfMat[1,])
points(x = FPr3, y = TPr3 , col = "red", pch = 16, cex = 2)





pred4 <- prediction(pre.test.mod3, STATE.test)
eval4 <- performance(pred4, "acc")
plot(eval4, main = "Accuracy Curve", cex.lab = 1.5, cex.main = 1.5)
points(x = 0.75, y = pre.test.mod3.Acc, col = "red", pch = 16, cex = 2)



roc4 <- performance(pred4, "tpr","fpr")
plot(roc4, main = "ROC Curve", cex.lab = 1.5, cex.main = 1.5)
TPr4 <- pre.test.mod3.ConfMat[2,2]/sum(pre.test.mod3.ConfMat[,2])
FPr4 <- pre.test.mod3.ConfMat[2,1]/sum(pre.test.mod3.ConfMat[,1])
points(x = FPr4, y = TPr4 , col = "red", pch = 16, cex = 2)


plot(NULL,xlim=c(0,1), ylim=c(0,1), xlab = "Cutoff/False Positive Rate",
     ylab = "Accuracy/True Positive Rate", main = "Accuracy/ROC Curves",
     cex.lab = 1.3, cex.main = 1.5, cex.axis = 1.5)
points(eval4@x.values[[1]], eval4@y.values[[1]], type = "l", col = "blue", lwd = 2)
points(x = 0.75, y = pre.test.mod3.Acc, col = "red", pch = 16, cex = 2)
points(roc4@x.values[[1]],roc4@y.values[[1]], col = "green", type = "l", lwd = 2, lty = 2)
points(x = FPr4, y = TPr4 , col = "red", pch = 16, cex = 2)
legend("bottomright", legend = c("Accuracy", "ROC"), lty = c(1,2), lwd = c(2,2),
       col = c("blue", "green"))













model3.2 <- glm(STATE ~ category + log(goal) + I(RunTime^0.5) + log(backers) + TimeDiff,
              family = "binomial", data  = TRAIN, maxit = 300, epsilon = 1e-8,
              trace = T)

summary(model3.2)

length(model3.2$coefficients[seq(2,159)])

Coeffs <- model3.2$coefficients[seq(2,159)]
STDEr <- summary(model3.2)[["coefficients"]][,2][seq(2,159)]

Lower <- rep(NA, times  = 158)
for (i in 1:158){
  
  Lower[i] <- Coeffs[i] - (2*STDEr[i])
}
rm(i)

Upper <- rep(NA, times  = 158)
for (i in 1:158){
  
  Upper[i] <- Coeffs[i] + (2*STDEr[i])
}
rm(i)

CI <- data.frame(Lower,Upper)


set.seed(355)
COLORS <- sample(colors()[seq(2,657)], size = 158)

set.seed(210)
PCH <- sample(seq(15,20), size = 159, replace = T)


plot(seq(1,158),Coeffs, col = COLORS, pch = PCH, cex = 1.2,
     ylim = c(min(CI[,1])-0.1, max(CI[,2])+0.1),
     main = "Subcategory Log Odds Effects",
     sub = "Base Category: 3D Printing", xlab = "Subcategory #", ylab = "Estimated Log Odds Effect",
     cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.7)
segments(x0 = seq(1,158), y0 = Coeffs,
         x1 = seq(1,158), y1 = CI[,1], lwd = 1.5, col = COLORS)
segments(x0 = seq(1,158), y0 = Coeffs,
         x1 = seq(1,158), y1 = CI[,2], lwd = 1.5, col = COLORS)


which(CI[,2] == max(CI[,2]))
CI[83,]
Coeffs[83]


plot(seq(1,157),Coeffs[-83], col = COLORS[-1], pch = PCH, cex = 1.2,
     ylim = c(min(CI[c(-83),1])-0.1, max(CI[c(-83),2])+0.1),
     main = "Subcategory Log Odds Effects",
     sub = "Base Category: 3D Printing", xlab = "Subcategory #", ylab = "Estimated Log Odds Effect",
     cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.7)
segments(x0 = seq(1,157), y0 = Coeffs,
         x1 = seq(1,157), y1 = CI[c(-83),1], lwd = 1.5, col = COLORS[-1])
segments(x0 = seq(1,157), y0 = Coeffs,
         x1 = seq(1,157), y1 = CI[c(-83),2], lwd = 1.5, col = COLORS[-1])
abline(h = 0, lwd = 1.5, col = "black")

plot.new()
ind <- seq(2,159)
levs <- levels(TRAIN$category)
levs <- levs[c(-1,-83)]
legend("center",legend = levs, pch = PCH,
       col = COLORS[-1], ncol = 7)



View(CI[c(-83),1])


