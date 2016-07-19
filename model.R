#user input: YES, NO
library(dplyr)
library(lubridate)
library(ggplot2)

#create an empty dataframe for first time users
userdata = data.frame()
userdata <- data.frame(Date=as.Date(character()),
                       Action=character(), 
                       EnergySaved=numeric(), 
                       stringsAsFactors=FALSE) 

#user input
input_date <- readline("yyyy-mm-dd ") %>% ymd()
input_action <- readline("YES or NO ")

#What energy?
energytype <- readline("Electricity, Water, or Trees ")

if (energytype == "Electricity") {
  unit = "(Electrivity, kilowatt hours)"
} else if (energytype == "Water") {
  average = 3.5
  unit = "(Water, litre)"
} else if (energytype == "Trees") {
  average = 2.3
  unit = "(Trees, number)"
}

#add input to the dataframe- function trial 1
add_inputs <- function(input_date, input_action) {
  n = nrow(userdata)
  if (n != 0) {
    if(input_date == userdata$Date[n]) {
      userdata$Action[n] <- input_action
      userdata$EnergySaved[n] <- ifelse (input_action == "YES", average, 0)
    } else {
      saved <- ifelse (input_action == "YES", average, 0)
      userdata <- rbind(userdata, c(input_date, input_action, saved))
    }
  } else {
    saved <- ifelse (input_action == "YES", average, 0)
    userdata <- rbind(userdata, c(input_date, input_action, saved))
  }
  input_date <- NULL
  input_action <- NULL
}



#add input to the dataframe- function trial 2
add_input <- function (input_date, input_action){
  n <- nrow(userdata)
  if(is.na(userdata$EnergySaved) & !is.null(input_action)) {
    userdata$Date[1] <- input_date
    userdata$Action[1] <- input_action
    userdata$EnergySaved[1] <- ifelse (input_action == "YES", average, 0)
    input_date <- NULL
    input_action <- NULL
  } else if (!is.na(userdata$EnergySaved) & !is.null(input_action)) {
    if(input_date == userdata$Date[n]) {
      userdata$Action[n] <- input_action
      userdata$EnergySaved[n] <- ifelse (input_action == "YES", average, 0)
      
    } else {
      saved <- ifelse (input_action == "YES", average, 0)
      userdata <- rbind(userdata, c(input_date, input_action, saved))
    }
    input_date <- NULL
    input_action <- NULL
  }
}


#Visualization
sample <- read.csv("sampledata.csv")
View(sample)
sample$Date <- mdy(sample$Date)
sample <- sample %>% mutate(TotalEnergySaved = cumsum(EnergySaved))
ggplot(data = sample, aes(x = Date, y = TotalEnergySaved)) + geom_line(color = "blue") + 
  geom_point(color = "blue", size = 1) + theme_classic() + 
  labs(title ="How Much Energy Did You Save?", y = paste("Total Energy Saved", "\n", unit))

#modeling
fit <- lm(TotalEnergySaved ~ Date, data = sample)
summary(fit)

predDate <- readline("Week, Month, 6 months, or Year")
last <- nrow(sample)
if (predDate == "Week") {
  FutureDate = sample$Date[last] + 7
} else if (predDate == "Month") {
  FutureDate = sample$Date[last] + 30
} else if (predDate == "6 months") {
  FutureDate = sample$Date[last] + 183
} else if (predDate == "Year") {
  FutureDate = sample$Date[last] + 365
} else print("Prediction unavailable or incorrect input format")
preDate <- NULL

FutureEnergySaved <- predict(fit, data.frame(Date = FutureDate))

#visualization with mode
ggplot(data = sample, aes(x = Date, y = TotalEnergySaved)) + geom_line(color = "blue") + 
  geom_point(color = "blue", size = 1) + theme_classic() + 
  labs(title = "How Much Energy Did You Save?", y = paste("Total Energy Saved", "\n", unit)) + 
  geom_smooth(method = "lm", color = "black", se = FALSE, size = 1, formula = y ~ x)



