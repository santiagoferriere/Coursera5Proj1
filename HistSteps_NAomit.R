data <- read.csv("activity.csv", header = TRUE, na.strings = "NA")
library(lubridate)
library(dplyr)
library(ggplot2)

data <- mutate(data, date = ymd(data$date))
## con esto transformo la columna de class character a date
data <- mutate(data, date.f = as.factor(data$date))
data.na <- na.omit(data)
## la nueva col data.f será un factor
qplot(data= data.na,  steps, facets = .~ date.f, xlab = "days", main = "Hist of steps per day") 
#######################
mnday <- tapply(data.na$steps, data.na$date, mean )
#mnday <- rename(mnday, c ("days" = "V1"))
mdday <- tapply(data.na$steps, data.na$date, median )
sdday <- tapply(data.na$steps, data.na$date, sd )
frame <- rbind( mnday, mdday, sdday)
print(frame)
#######################
plot(mnday, xlab = "days", ylab = "mean steps", main = "mn Steps")
#######################
mnint <- tapply(data.na$steps, data.na$interval, mean )
mnint <- as.data.frame(mnint)
mnint <- rownames_to_column(mnint, var = "interval")
#el intervalo antes era row.name y ahora es una col más
max <- max(mnint[,2])
#averiguo el máximo
print(mnint[ mnint[, 2] == max, ])
#####################




