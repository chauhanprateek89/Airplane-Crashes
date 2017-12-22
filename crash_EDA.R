plane_data = read.csv("Airplane_Crashes_and_Fatalities_Since_1908.csv", header = T)

library("ggplot2")
if(!require("date")) install.packages("date")
if(!require("plyr")) install.packages("plyr")
if(!require("dplyr")) install.packages("dplyr")
if(!require("data.table")) install.packages("data.table")
if(!require("plotly")) install.packages("plotly")
if(!require("magrittr")) install.packages("magrittr")
library("plyr")
library("dplyr")
library("data.table")
library("plotly")
library("magrittr")


# Change the Date format
plane_data$Date <- as.Date(plane_data$Date, "%m/%d/%Y")

# Extract the Year, Month and Day from the Date
plane_data$Year = as.integer(format(plane_data$Date, "%Y"))
plane_data$Month = as.integer(format(plane_data$Date, "%m"))
plane_data$Day = as.integer(format(plane_data$Date, "%d"))

# Sum accidents by Year, Month and Day
data.year = plane_data['Year']
data.year = data.frame(table(data.year))
ggplot(data.year, aes(x=data.year, y=Freq, group=1)) + 
  geom_line(linetype=1, size=2, color="blue") + 
  geom_point(size=4, shape=21, fill="red") + 
  xlab("Year") + ylab("Number of Crashes") + 
  ggtitle("Total Number of Crashes Through The Years") + 
  scale_x_discrete(breaks=seq(1908, 2009, 10))

data.month = plane_data['Month']
data.month = data.frame(table(data.month))
ggplot(data.month, aes(x=data.month, y=Freq, group=1)) + 
  geom_line(linetype=1, size=2, color="blue") + 
  geom_point(size=4, shape=21, fill="red") + 
  xlab("Month") + ylab("Number of Crashes") + 
  ggtitle("Total Number of Crashes Through The Months") 

data.day = plane_data['Day']
data.day = data.frame(table(data.day))
ggplot(data.day, aes(x=data.day, y=Freq, group=1)) + 
  geom_line(linetype=1, size=2, color="blue") + 
  geom_point(size=4, shape=21, fill="red") + 
  xlab("Day") + ylab("Number of Crashes") + 
  ggtitle("Total Number of Crashes Through The Days") 

# Removing any fatalities that are NA and only keeping those with at least 1 fatality
fatalities = as.data.table(subset(plane_data, plane_data$Fatalities >= 1 & !is.na(plane_data$Fatalities)))

#Sum Fatalities by Year, Month and Day
yearly_fatalities = fatalities[, sum(Fatalities), by=Year]
names(yearly_fatalities) = c("Year", "Fatalities")
ggplot(yearly_fatalities, aes(x=Year, y=Fatalities, group=1)) + 
  geom_line(linetype=1, size=2, color="blue") + 
  geom_point(size=4, shape=21, fill="red") + 
  xlab("Year") + 
  ylab("Number of Fatalities") + 
  ggtitle("Total Number of Fatalities Through The Years") + 
  scale_x_continuous(breaks=seq(1908, 2009, 10))

monthly_fatalities = fatalities[, sum(Fatalities), by=Month]
names(monthly_fatalities) = c("Month", "Fatalities")
ggplot(monthly_fatalities, aes(x=Month, y=Fatalities, group=1)) + 
  geom_line(linetype=1, size=2, color="blue") + 
  geom_point(size=4, shape=21, fill="red") + 
  xlab("Month") + 
  ylab("Number of Fatalities") + 
  ggtitle("Total Number of Fatalities Through The Months") + 
  scale_x_continuous(breaks=seq(0, 12, 1))

daily_fatalities = fatalities[, sum(Fatalities), by=Day]
names(daily_fatalities) = c("Day", "Fatalities")
ggplot(daily_fatalities, aes(x=Day, y=Fatalities, group=1)) + 
  geom_line(linetype=1, size=2, color="blue") + 
  geom_point(size=4, shape=21, fill="red") + 
  xlab("Day") + ylab("Number of Fatalities") + 
  ggtitle("Total Number of Fatalities Through The Days") + 
  scale_x_continuous(breaks=seq(1, 31, 2))

# Removing any people Aboard that are NA and only keeping those with at least 1 Aboard
passengers = as.data.table(subset(plane_data, plane_data$Aboard >= 1 & !is.na(plane_data$Aboard)))

#Sum Passengers by Year, Month and Day
yearly_passengers = passengers[, sum(Aboard), by=Year]
names(yearly_passengers) = c("Year", "Passengers")
ggplot(yearly_passengers, aes(x=Year, y=Passengers, group=1)) + 
  geom_line(linetype=1, size=2, color="blue") + 
  geom_point(size=4, shape=21, fill="red") + 
  xlab("Year") + ylab("Number of Passengers") + 
  ggtitle("Total Number of Passengers Through The Years") + 
  scale_x_continuous(breaks=seq(1908, 2009, 10))

monthly_passengers = passengers[, sum(Aboard), by=Month]
names(monthly_passengers) = c("Month", "Passengers")
ggplot(monthly_passengers, aes(x=Month, y=Passengers, group=1)) + 
  geom_line(linetype=1, size=2, color="blue") + 
  geom_point(size=4, shape=21, fill="red") + 
  xlab("Month") + ylab("Number of Passengers") + 
  ggtitle("Total Number of Passengers Through The Months") + 
  scale_x_continuous(breaks=seq(0, 12, 1))

daily_passengers = passengers[, sum(Aboard), by=Day]
names(daily_passengers) = c("Day", "Passengers")
ggplot(daily_passengers, aes(x=Day, y=Passengers, group=1)) + 
  geom_line(linetype=1, size=2, color="blue") + 
  geom_point(size=4, shape=21, fill="red") + 
  xlab("Day") + 
  ylab("Number of Passengers") + 
  ggtitle("Total Number of Passengers Through The Days") + 
  scale_x_continuous(breaks=seq(1, 31, 2))

#Calculating the number of survivors
plane_data$Survivor = plane_data$Aboard - plane_data$Fatalities

# Removing any people Aboard that are NA and only keeping those with at least 1 Aboard
survivors = as.data.table(subset(plane_data, plane_data$Survivor >= 1 & !is.na(plane_data$Survivor)))

#Sum Survivors by Year, Month and Day
yearly_survivors = survivors[, sum(Survivor), by=Year]
names(yearly_survivors) = c("Year", "Survivors")
ggplot(yearly_survivors, aes(x=Year, y=Survivors, group=1)) + 
  geom_line(linetype=1, size=2, color="blue") + 
  geom_point(size=4, shape=21, fill="red") + 
  xlab("Year") + ylab("Number of Survivors") + 
  ggtitle("Total Number of Survivors Through The Years") + 
  scale_x_continuous(breaks=seq(1908, 2009, 10))

monthly_survivors = survivors[, sum(Survivor), by=Month]
names(monthly_survivors) = c("Month", "Survivors")
ggplot(monthly_survivors, aes(x=Month, y=Survivors, group=1)) + 
  geom_line(linetype=1, size=2, color="blue") + 
  geom_point(size=4, shape=21, fill="red") + 
  xlab("Month") + ylab("Number of Survivors") + 
  ggtitle("Total Number of Survivors Through The Months") + 
  scale_x_continuous(breaks=seq(0, 12, 1))

daily_survivors = survivors[, sum(Survivor), by=Day]
names(daily_survivors) = c("Day", "Survivors")
ggplot(daily_survivors, aes(x=Day, y=Survivors, group=1)) + 
  geom_line(linetype=1, size=2, color="blue") + 
  geom_point(size=4, shape=21, fill="red") + 
  xlab("Day") + 
  ylab("Number of Survivors") + 
  ggtitle("Total Number of Survivors Through The Days") + 
  scale_x_continuous(breaks=seq(1, 31, 2))

#Combine the Yearly Passengers, Survivors and Fatalities so as to produce a single graph for them
yearly_data = merge(yearly_passengers, yearly_fatalities, by = "Year")
yearly_data$Survivor = yearly_data$Passengers - yearly_data$Fatalities

ggplot(yearly_data, aes(x=Year)) +
  geom_line(aes(y=Passengers, color = "Passengers"),linetype=1, size=1.5, color="blue") +
  geom_point(aes(y=Passengers, color = "Passengers"),size=4, shape=18) +
  geom_line(aes(y=Fatalities, color = "Fatalities"),linetype=1, size=1.5, color="red") +
  geom_point(aes(y=Fatalities, color = "Fatalities"),size=4, shape=18) +
  geom_line(aes(y=Survivor, color = "Survivor"),linetype=1, size=1.5, color="purple") +
  geom_point(aes(y=Survivor, color = "Survivor"),size=4, shape=18) +
  xlab("Year") +
  ylab("Count") +
  ggtitle("Total Number of Passengers, Survivors & Fatalities Through The Years") +
  scale_x_continuous(breaks=seq(1908, 2009, 10)) + 
  theme_bw() + 
  theme(legend.position=c(0.3, 0.7))

#Combine the Monthly Passengers and Fatalities so as to produce a single graph for them
monthly_data = merge(monthly_passengers, monthly_fatalities, by = "Month")
monthly_data$Survivor = monthly_data$Passengers - monthly_data$Fatalities
ggplot(monthly_data, aes(x=Month)) + 
  geom_line(aes(y=Passengers, color = "Passengers"),linetype=1, size=1.5, color="blue") +
  geom_point(aes(y=Passengers, color = "Passengers"),size=4, shape=18) +
  geom_line(aes(y=Fatalities, color = "Fatalities"),linetype=1, size=1.5, color="red") +
  geom_point(aes(y=Fatalities, color = "Fatalities"),size=4, shape=18) +
  geom_line(aes(y=Survivor, color = "Survivor"),linetype=1, size=1.5, color="purple") +
  geom_point(aes(y=Survivor, color = "Survivor"),size=4, shape=18) +
  xlab("Month") +
  ylab("Count") +
  ggtitle("Total Number of Passengers, Survivors & Fatalities Through The Months") +
  scale_x_continuous(breaks=seq(1, 12, 1)) + 
  theme_bw() + 
  theme(legend.position=c(0.3, 0.8))

#Combine the Daily Passengers and Fatalities so as to produce a single graph for them
daily_data = merge(daily_passengers, daily_fatalities, by = "Day")
daily_data$Survivor = daily_data$Passengers - daily_data$Fatalities
ggplot(daily_data, aes(x=Day)) + 
  geom_line(aes(y=Passengers, color = "Passengers"),linetype=1, size=1.5, color="blue") +
  geom_point(aes(y=Passengers, color = "Passengers"),size=4, shape=18) +
  geom_line(aes(y=Fatalities, color = "Fatalities"),linetype=1, size=1.5, color="red") +
  geom_point(aes(y=Fatalities, color = "Fatalities"),size=4, shape=18) +
  geom_line(aes(y=Survivor, color = "Survivor"),linetype=1, size=1.5, color="purple") +
  geom_point(aes(y=Survivor, color = "Survivor"),size=4, shape=18) +
  xlab("Day") +
  ylab("Count") +
  ggtitle("Total Number of Passengers, Survivors & Fatalities Through The Days") +
  scale_x_continuous(breaks=seq(1, 31, 1)) + 
  theme_bw() + 
  theme(legend.position=c(0.5, 0.85))

# Summing Fatalities by Operator
operator = aggregate(plane_data$Fatalities, by=list(Operator=plane_data$Operator), FUN=sum)
names(operator) = c("Operator", "Fatalities")

#Drop the rows with an NA for Fatalities
operator = subset(operator, Fatalities != "NA")

#Drop the 1st row since it has no Operator name
operator = operator[-c(1),]

operator = operator[ order(-operator$Fatalities), ]

ggplot(operator[1:20,], aes(x=reorder(factor(Operator), Fatalities), y=Fatalities, alpha=Fatalities)) + 
  geom_bar(stat = "identity", fill="blue") + 
  xlab("Operator") + 
  ylab("Fatalities") + 
  ggtitle("Top 20 Operators with the most Fatalities") + 
  coord_flip()

# Summing Fatalities by Plane Type
plane_type = aggregate(plane_data$Fatalities, by=list(Type=plane_data$Type), FUN=sum)
names(plane_type) = c("Type", "Fatalities")

#Drop the rows with an NA for Fatalities
plane_type = subset(plane_type, Fatalities != "NA")

plane_type = plane_type[ order(-plane_type$Fatalities), ]

ggplot(plane_type[1:20,], aes(x=reorder(factor(Type), Fatalities), y=Fatalities, alpha=Fatalities)) + 
  geom_bar(stat = "identity", fill="blue") + 
  xlab("Plane Type") + 
  ylab("Fatalities") + 
  ggtitle("Top 20 Plane Types with the most Fatalities") + 
  coord_flip()

# Sum accidents by Operator
data.operator = plane_data['Operator']
data.operator = data.frame(table(data.operator))
data.operator = data.operator[-c(1),]
data.operator = data.operator[ order(-data.operator$Freq), ]

ggplot(data.operator[1:20,], aes(x=reorder(factor(data.operator), Freq), y=Freq, alpha=Freq)) + 
  geom_bar(stat = "identity", fill="blue") + 
  xlab("Operator") + 
  ylab("Number of Crashes") + 
  ggtitle("Top 20 Operators with the most Crashes") + 
  coord_flip()

# Sum accidents by Plane Type
data.type = plane_data['Type']
data.type = data.frame(table(data.type))
data.type = data.type[-c(1),]
data.type = data.type[ order(-data.type$Freq), ]

ggplot(data.type[1:20,], aes(x=reorder(factor(data.type), Freq), y=Freq, alpha=Freq)) + 
  geom_bar(stat = "identity", fill="blue") + 
  xlab("Plane Type") + 
  ylab("Number of Crashes") + 
  ggtitle("Top 20 Plane Types with the most Crashes") + 
  coord_flip()
