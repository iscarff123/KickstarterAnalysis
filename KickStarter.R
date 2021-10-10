



### Use the 2018 data set for kickstarters
data <- read.csv("ks-projects-201801.csv")
View(data)


##### Variables











#### Look at structure of data
str(data)












#### There are kickstarter states that are undefined. Look closer
data.und <- data[data$state == "undefined",]
View(data.und)




data <- data[data$state != "undefined",]
table(data$state)


str(data)





length(levels(data$category))
length(levels(data$main_category))

levels(data$currency)

aud <- length(which(data$currency == "AUD"))
cad <- length(which(data$currency == "CAD"))
chf <- length(which(data$currency == "CHF"))
dkk <- length(which(data$currency == "DKK"))
eur <- length(which(data$currency == "EUR"))
gbp <- length(which(data$currency == "GBP"))
hkd <- length(which(data$currency == "HKD"))
jpy <- length(which(data$currency == "JPY"))
mxn <- length(which(data$currency == "MXN"))
nok <- length(which(data$currency == "NOK"))
nzd <- length(which(data$currency == "NZD"))
sek <- length(which(data$currency == "SEK"))
sgd <- length(which(data$currency == "SGD"))
usd <- length(which(data$currency == "USD"))


CUR.SLICES <- c(aud,cad,chf,dkk,eur,gbp,hkd,jpy,mxn,nok,nzd,sek,sgd,usd)
CUR.LABELS <- levels(data$currency)





table(data$country)[[1]]











makePie <- function(slices, labels, title){

  percent <- round(slices/sum(slices)*100)
  labels <- paste(labels,percent, sep = "  ")
  labels <- paste(labels,"%",sep="")
  labels <- paste(labels,slices, sep = " = ")
  pie(slices, labels = labels, main = title,col = c("blue","red"),init.angle = 105)
  
  labels
}

makePie(slices = c(nrow(US.sub), nrow(data.US)-nrow(US.sub)),
        labels = c(paste("$100 <= Goal <= $50,000:"),
                   "Goal < $100, Goal > $50,000:"))





makePie(CUR.SLICES, CUR.LABELS, "Currency")




levels(data$state)

CANC <- length(which(data$state == "canceled"))
FAIL <- length(which(data$state == "failed"))
LIVE <- length(which(data$state == "live"))
SUCC <- length(which(data$state == "successful"))
SUSP <- length(which(data$state == "suspended"))

ST.SLICES <- c(CANC, FAIL, LIVE, SUCC, SUSP)
ST.LABELS <- levels(data$state[-6])

makePie(ST.SLICES, ST.LABELS, "State")


levels(data$country)
table(data$country)









data.US <- data[data$country == "US", ]
which(data.US$usd_goal_real == 0.01)
View(data.US[c(235206,245415),])

ind <- which((data.US$usd_goal_real >= 100) & (data.US$usd_goal_real <= 50000))
head(ind)

US.sub <- data.US[ind,]
length(levels(sub$main_category))

write.csv(US.sub, file = "USsub.csv")




###################### HYPothesis
#### certain factors have pos/neg effects on the response



hist(US.sub$usd_goal_real)

range(US.sub$usd_goal_real)
median(US.sub$usd_goal_real)
mean(US.sub$usd_goal_real)



range(data.US$usd_goal_real)
mean(data.US$usd_goal_real)
median(data.US$usd_goal_real)


range(data.US$usd_pledged_real)
mean(data.US$usd_pledged_real)
median(data.US$usd_pledged_real)



range(data.US$backers)
mean(data.US$backers)
median(data.US$backers)

range(data.US$RunTime)
mean(data.US$RunTime)
median(data.US$RunTime)



data.US[17] <- round(data.US$usd_pledged_real / data.US$usd_goal_real, digits = 5) * 100
colnames(data.US)[17] <- "PercentGoal"


range(data.US$PercentGoal)
mean(data.US$PercentGoal)
median(data.US$PercentGoal)


which(data.US$PercentGoal == 10427789)
View(data.US[285278,])


hist(data.US$PercentGoal)
abline(v = mean(data.US$RunTime), col = "red", lwd = 2, lty = 1)
abline(v = median(data.US$RunTime), col = "green", lwd = 2, lty = 2)
legend("topright", legend = c("mean","median"), col = c("red","green"), 
       lwd = c(2,2), lty = c(1,2))


###### find mean, median for usd equiv pledge and goal




####### add pie charts for success and failed data sets
####### 


data.notLive <- data[data$state != "live",]

table(data.notLive$state)

data.succ <- data.notLive[data.notLive$state == "successful",]
table(data.succ$state)

data.fail <- data.notLive[data.notLive$state != "successful",]
table(data.fail$state)



##### Success Data


aud <- length(which(data.succ$currency == "AUD"))
cad <- length(which(data.succ$currency == "CAD"))
chf <- length(which(data.succ$currency == "CHF"))
dkk <- length(which(data.succ$currency == "DKK"))
eur <- length(which(data.succ$currency == "EUR"))
gbp <- length(which(data.succ$currency == "GBP"))
hkd <- length(which(data.succ$currency == "HKD"))
jpy <- length(which(data.succ$currency == "JPY"))
mxn <- length(which(data.succ$currency == "MXN"))
nok <- length(which(data.succ$currency == "NOK"))
nzd <- length(which(data.succ$currency == "NZD"))
sek <- length(which(data.succ$currency == "SEK"))
sgd <- length(which(data.succ$currency == "SGD"))
usd <- length(which(data.succ$currency == "USD"))

CUR.SLICES <- c(aud,cad,chf,dkk,eur,gbp,hkd,jpy,mxn,nok,nzd,sek,sgd,usd)
CUR.LABELS <- levels(data$currency)

makePie(CUR.SLICES, CUR.LABELS, "Currency (Succesful)")



#### Fail data

aud <- length(which(data.fail$currency == "AUD"))
cad <- length(which(data.fail$currency == "CAD"))
chf <- length(which(data.fail$currency == "CHF"))
dkk <- length(which(data.fail$currency == "DKK"))
eur <- length(which(data.fail$currency == "EUR"))
gbp <- length(which(data.fail$currency == "GBP"))
hkd <- length(which(data.fail$currency == "HKD"))
jpy <- length(which(data.fail$currency == "JPY"))
mxn <- length(which(data.fail$currency == "MXN"))
nok <- length(which(data.fail$currency == "NOK"))
nzd <- length(which(data.fail$currency == "NZD"))
sek <- length(which(data.fail$currency == "SEK"))
sgd <- length(which(data.fail$currency == "SGD"))
usd <- length(which(data.fail$currency == "USD"))

CUR.SLICES <- c(aud,cad,chf,dkk,eur,gbp,hkd,jpy,mxn,nok,nzd,sek,sgd,usd)
CUR.LABELS <- levels(data$currency)

makePie(CUR.SLICES, CUR.LABELS, "Currency (Failed)")









