### This R code will be the starting part of the project
### Its purpose is to clean and prepare the data for model building

### Please note that each command may take a bit of time to run because of the data size.

### Reminder: variable descriptions can be found in the Exploratory Analysis HTML file.



### Load data
data <- read.csv("ks-projects-201801.csv")



### Grab US Kickstarters
US.full <- data[data$country == "US", ]


### Set the Launch and Deadline variables as date variables
US.full$deadline <- as.Date(US.full$deadline)
US.full$launched <- as.Date(US.full$launched)

### Create a RunTime variable
RunTime <- rep(NA, times = nrow(US.full))

### Use a for loop to calculate the RunTime
for (i in 1:length(RunTime)){
  
  ### Calculate the difference in time and stores it into RunTime
  RunTime[i] <- difftime(US.full$deadline[i], US.full$launched[i], units = "days")
  
}

### Add RunTime variable to the data
US.full[16] <- RunTime
colnames(US.full)[16] <- "RunTime"




### Extract projects that have goals between $100 and $50,000 and a max of 60 day run time
index <- which((US.full$usd_goal_real >= 100) & (US.full$usd_goal_real <= 50000) & 
                 (US.full$RunTime <= 60))
US <- US.full[index,]

### Remove rows with undefined State variables
US <- US[US$state != "undefined",]


### Calculate percentage of goal reached and add it to the data
US[17] <- round(US$usd_pledged_real / US$usd_goal_real, digits = 5) * 100
colnames(US)[17] <- "PercentGoal"


### Create Month Variable
#### Grabs the month of the launched date and adds it to the US data
US[18] <- as.factor(format(as.Date(US$launched), "%m"))
colnames(US)[18] <- "Month"


### Create Year Variable
US[19] <- as.factor(format(as.Date(US$launched), "%Y"))
colnames(US)[19] <- "Year"

write.csv(US, file = "US_Kickstarters.csv", row.names = F, col.names = T)





