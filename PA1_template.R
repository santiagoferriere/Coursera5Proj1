data <- read.csv("activity.csv", header = TRUE, na.strings = "NA")
library(lubridate)
library(dplyr)
library(ggplot2)
library(tibble)

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
library(VIM)
data.na.rep <- kNN( data = data)
####################
qplot(data= data.na.rep,  steps, facets = .~ date.f, xlab = "days", main = "Hist of steps per day")                           
####################
data.na.rep <- mutate(data.na.rep , daydate = as.Date(data.na.rep$date))
#creo la nueva col en donde este en class date
weekdays1 <- c('lunes', 'martes', 'miercoles', 'jueves', 'viernes')
data.na.rep$daytype <- factor((weekdays(data.na.rep$date) %in% weekdays1), 
                              levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))
## es lo mismo que hace mutate = as.factor

by.daytype <- split( data.na.rep,  data.na.rep$daytype)
#separo de acuerdo al daytype (ie weekend | weekday)

by.int1 <- as.data.frame(tapply( by.daytype[["weekday"]]$steps
                                 ,  by.daytype[["weekday"]]$interval, mean))
by.int2 <- as.data.frame(tapply( by.daytype[["weekend"]]$steps,
                                 by.daytype[["weekend"]]$interval, mean))
## hago tapply con la media
mn.daytype <- merge ( by.int1, by.int2)
## fundo los data.frames de acuerdo a su intervalo (por default)
names(mn.daytype)[1] <- "weekdays"
names(mn.daytype)[2] <- "weekends"
# renaming cols
###############################
mn.daytype <- rownames_to_column(mn.daytype, var = "interval")
#el intervalo antes era row.name y ahora es una col más
#library(sjmisc)
#mn.daytype <- rotate_df( mn.daytype)
########################
#hist( mn.daytype, x = mn.daytype$in y = mn.daytype$weekdays )
ggplot(mn.daytype, aes(  y = weekdays, x = interval )) + 
        geom_point( aes( y = weekdays), color = "red")           
####################
plot(mn.daytype, x = mn.daytype$interval, y = mn.daytype$weekdays)
