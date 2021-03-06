library(ggplot2)
library(readr)

install.packages("animation")
require(animation)



air <- read.csv("Airplane_Crashes_and_Fatalities_Since_1908.csv", header = T)

airfrance = air[air$Operator == 'Air France',] #select certain only
american = air[air$Operator == 'American Airlines',] #select certain only
aeroflot  = air[air$Operator == 'Aeroflot',] #select certain only
usmil  = air[air$Operator == 'Military - U.S. Air Force',] #select certain only
indian  = air[air$Operator == 'Indian Airlines',] #select certain only

#exclude rows with missing data
exc.Numbers = (is.na((airfrance$Fatalities) | is.na(airfrance$Aboard)))
airfrance = airfrance[!exc.Numbers, ]

airfrance$Date = as.character((airfrance$Date)) #format the date
airfrance$Date = as.Date(airfrance$Date, "%m%d%Y") #Format the date
airfrance = airfrance[order(airfrance$Date), ] #reorder the data by date

#Exclude rows with missing data,
exc.Numbers = (is.na(american$Fatalities) | is.na(american$Aboard))
american = american[!exc.Numbers, ]

american$Date = as.character(american$Date) #Format the date
american$Date = as.Date(american$Date, "%m/%d/%Y") #Format the date
american = american[order(american$Date),] #Reorder the data by date

#Exclude rows with missing data,
exc.Numbers = (is.na(usmil$Fatalities) | is.na(usmil$Aboard))
usmil = usmil[!exc.Numbers, ]

usmil$Date = as.character(usmil$Date) #Format the date
usmil$Date = as.Date(usmil$Date, "%m/%d/%Y") #Format the date
usmil = usmil[order(usmil$Date),] #Reorder the data by date


#Exclude rows with missing data,
exc.Numbers = (is.na(aeroflot$Fatalities) | is.na(aeroflot$Aboard))
aeroflot = aeroflot[!exc.Numbers, ]

aeroflot$Date = as.character(aeroflot$Date) #Format the date
aeroflot$Date = as.Date(aeroflot$Date, "%m/%d/%Y") #Format the date
aeroflot = aeroflot[order(aeroflot$Date),] #Reorder the data by date


#Exclude rows with missing data,
exc.Numbers = (is.na(indian$Fatalities) | is.na(indian$Aboard))
indian = indian[!exc.Numbers, ]

indian$Date = as.character(indian$Date) #Format the date
indian$Date = as.Date(indian$Date, "%m/%d/%Y") #Format the date
indian = indian[order(indian$Date),] #Reorder the data by date

#Set up run totals...

runsum_airfrance = cumsum(airfrance$Fatalities)
runsum_airfrance2 = cumsum(airfrance$Aboard)
runsum_airfrance = data.frame(cbind(runsum_airfrance, airfrance['Date']), runsum_airfrance2)
runsum_airfrance$Operator = 'Air France'
colnames(runsum_airfrance) = c('Fatalities_runsum', 'Date', 'Aboard_runsum', 'Operator')

runsum_american = cumsum(american$Fatalities)
runsum_american2 = cumsum(american$Aboard)
runsum_american = data.frame(cbind(runsum_american, american['Date']), runsum_american2)
runsum_american$Operator = 'American Airlines'
colnames(runsum_american) = c('Fatalities_runsum', 'Date', 'Aboard_runsum', 'Operator')

runsum_aeroflot = cumsum(aeroflot$Fatalities)
runsum_aeroflot2 = cumsum(aeroflot$Aboard)
runsum_aeroflot = data.frame(cbind(runsum_aeroflot, aeroflot['Date']), runsum_aeroflot2)
runsum_aeroflot$Operator = 'Aeroflot'
colnames(runsum_aeroflot) = c('Fatalities_runsum', 'Date', 'Aboard_runsum', 'Operator')

runsum_usmil = cumsum(usmil$Fatalities)
runsum_usmil2 = cumsum(usmil$Aboard)
runsum_usmil = data.frame(cbind(runsum_usmil, usmil['Date']), runsum_usmil2)
runsum_usmil$Operator = 'US Military'
colnames(runsum_usmil) = c('Fatalities_runsum', 'Date', 'Aboard_runsum', 'Operator')

runsum_indian = cumsum(indian$Fatalities)
runsum_indian2 = cumsum(indian$Aboard)
runsum_indian = data.frame(cbind(runsum_indian, indian['Date']), runsum_indian2)
runsum_indian$Operator = 'Indian Airlines'
colnames(runsum_indian) = c('Fatalities_runsum', 'Date', 'Aboard_runsum', 'Operator')

#Combine and tweak the data to be plotted,
x = rbind(runsum_airfrance, runsum_american, runsum_indian, runsum_usmil, runsum_aeroflot)

y = data.frame(Fatalities_runsum = c(1,1,1,1), Date = c("1933-10-31","1933-10-31",
                                                        "1933-10-31","1933-10-31"), Aboard_runsum = c(1,1,1,1), Operator = 
                 c("Indian Airlines", "US Military", "Aeroflot", "American Airlines"))
x = rbind(x,y)
x = x[order(x$Date),]

#Set animation time,
ani.options(interval = 0.1, nmax = 100)

i=1

#Loop through the rows and save the gif...
saveGIF(while (i<dim(x)[1]) {
  
  print(m <- qplot(x$Date[1:i], 
                   x$Aboard_runsum[1:i], 
                   size = (x$Fatalities_runsum[1:i]),
                   alpha=x$Aboard_runsum[1:i],
                   main = "Crash Data for 5 Airlines",
                   col = x$Operator[1:i],
                   xlab = "Date",
                   ylab = "No. of People Aboad (Cumulative)", na.rm=TRUE) + theme_bw() + ylim(0,10000)  + 
          theme(panel.background = element_rect(fill = "black"),
                panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = c(0.25, 0.65)) +
          scale_size_continuous(limits = c(0,8000), name = 'No. of Fatalities (Cumulative)') + 
          scale_color_discrete(name = "Airline Operator") + 
          scale_alpha(guide = 'none'))
  
  i=i+1
  
}, movie.name = "crash_ani.gif", convert = "convert", ani.width = 500, 
ani.height = 500)