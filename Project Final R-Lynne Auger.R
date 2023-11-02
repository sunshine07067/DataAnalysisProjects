
# read in the files of interest

NY <- read.csv("C:\\Users\\owlsc\\OneDrive\\Documents\\aWGU\\new-york-city.csv", header=T)
CH <- read.csv("C:\\Users\\owlsc\\OneDrive\\Documents\\aWGU\\chicago.csv", header=T)
WA <- read.csv("C:\\Users\\owlsc\\OneDrive\\Documents\\aWGU\\washington.csv",header=T)


library(ggplot2)
library(tidyverse)

#  I look at the data to see what is here

head(NY)
head(CH)
head(WA)

# I will want to look at the time duration in minutes so I add that column

NY[,'Trip.Minutes']<- round(NY$Trip.Duration/60,2)
head(NY)    # check to make sure it is what I expected
CH[,'Trip.Minutes']<- round(CH$Trip.Duration/60,2)
WA[,'Trip.Minutes']<- round(WA$Trip.Duration/60,2)


# I will start by looking at sex of users and portion of subscribers

# Question A: Which gender uses the Bikeshare the most?

# Looking at our data we can see that the Washington data file does not contain Gender data 
# I will continue to look at just the NY and Chicago (CH) files for gender

NY_gender <- NY[,'Gender']           # pull out the column of interest
head(NY_gender)                      # check to make sure it is as needed
CH_gender <- CH[,'Gender']           # do the same for Chicago
NYG <- table(factor(NY_gender))      # gets the count per gender for NY
CHG <- table(factor(CH_gender))      # gets the count per gender for CH
Gender <- data.frame(rbind(NYG,CHG)) # combines the 
t(Gender)
Gender$V1 <- NULL
Gender                               # creates a table of the values of gender per city


# I will collect the gender data per city and combine into one file to plot

ny <- rep('NY', 300000)           # set up a column filled with "NY" so the city's data can be tracked
NY_gender <- cbind(NY_gender, ny) # bind it to NY_gender
head(NY_gender)                   # look to see if it's what I want
ch<- rep('CH', 300000)            # set up a column filled with "CH" so the city's data can be tracked
CH_gender <- cbind(CH_gender, ch) #bind it
head(CH_gender)                   # check it
NYCHgenders <-(rbind(NY_gender,CH_gender)) # bind the two together as two rows
colnames(NYCHgenders)<- c('gender', 'city') # change the column headers
tail(NYCHgenders)                 #check it for CH
head(NYCHgenders)
str(NYCHgenders)                  #check structure
NYCHgenders<- data.frame(NYCHgenders)

NYCHgenders %>%
  group_by(city,gender) %>%
  ggplot(aes(x=city, fill=gender))+
  geom_bar(position=position_dodge(0.93))+
  labs(title="Gender of Bikeshare Riders per City", y="Number of Riders") +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_fill_discrete(labels=c("Unknown", "Female", "Male"))

## The data shows that men use the Bikeshare service three times as much as women do. 
## There is a significant amount of unknown gender in Chicago which could shift the data.
## This could represent the actual trend or just this sample data selected from the population.

##Question B: Are Bikeshare riders loyal repeat customers?

#I will now look at user type. WA contains data for this a swell as the others. I will
# be using the 'ny' and 'ch' columns set up for the gender section above


NY_user <- NY[,'User.Type']    # pull out the column of interest
CH_user <- CH[,'User.Type']    # pull out the column of interest
WA_user <- WA[,'User.Type']    # pull out the column of interest
NY_user <- cbind(NY_user, ny)  # bind NY_user and ny
head(NY_user)                  # look to see if it's what I want

CH_user <- cbind(CH_user, ch)  #bind it                 
wa <- rep('WA', 300000)        # the NY and CH columns have already been made, WA has not
WA_user <- cbind(WA_user, wa)  # bind it
 
NYCHWAusers <-(rbind(NY_user,CH_user, WA_user)) # bind the three together as rows
colnames(NYCHWAusers)<- c('user', 'city')       # change the column names
head(NYCHWAusers)
NYCHWAusers <- data.frame(NYCHWAusers)          #  change structure
NYCHWAusers <- subset(NYCHWAusers, user == "Subscriber" | user == "Customer") # remove unwanted 
NYCHWA_table <- table(NYCHWAusers)    # create table of user type per city
NYCHWA_table

NYCHWAusers %>%                                      # plot data
  group_by(city,user) %>%
  ggplot(aes(x=city, fill=user))+
  geom_bar(position=position_dodge(0.9))+
  labs( title="Status of Bikeshare Riders per City") +
  theme(plot.title = element_text(hjust=0.5))

## The plot shows that in all cities, most riders are Subscribers suggesting that
## much of the business is repeat. Since these are major cities, I would  have expected 
## to see a larger amount of one time users or Customers: People may use the bikes to 
## tour the city when visiting. The data suggests that the people of these cities find
## this mode of transportation to be preferred. 


# Question 1 What is the avg amount of time the bikes are rented?

par(mfrow= c(1,3))
boxplot(x=NY$Trip.Minutes, ylim=c(0,40), xlab = "NY", ylab="minutes") # create a plot of NY
boxplot(x=CH$Trip.Minutes, ylim=c(0,40), xlab = "CH", # create a plot of CH
        main= "Trip Durations/City")
boxplot(x=WA$Trip.Minutes, ylim=c(0,40), xlab = "WA") # create a plot of WA

## Questions I could use help with...
## How can I get the Title to span more than the center plot?
## Is there a way to add the summary of stats to the plot... ie: TripStats below

#Create a chart of the statistics for Trip Duration

NYs <- summary(NY$Trip.Minutes)
CHs <-summary(CH$Trip.Minutes)
WAs <-summary(WA$Trip.Minutes)
TripStats <-rbind(NYs, CHs, WAs)
TripStats
## Looking at the results across all three cities, we can see the median rental time is
## between 10-12 minutes. The TripStats chart indicates the average time of rental is 15-20 
## minutes. We can see from the whiskers that trip duration increases from NY to Ch and then 
## even more in WA. This suggests that NY and CH use the bikes for specific uses.



#Question 2: Which days of the week are the bikes rented the most?

NY$day <-wday(NY$Start.Time, week_start=1, label=TRUE) # extract the day from Start.Time
CH$day <-wday(CH$Start.Time, week_start=1, label=TRUE) # extract the day from Start.Time
WA$day <-wday(WA$Start.Time, week_start=1, label=TRUE) # extract the day from Start.Time
head(NY$day)

NY_day <- cbind(NY$day, ny)                            # Bind the city to the data
CH_day <- cbind(CH$day, ch)                            # Bind the city to the data
WA_day <- cbind(WA$day, wa)                            # Bind the city to the data
head(NY_day)                                           # check it
Days <- data.frame(rbind(NY_day, CH_day, WA_day))      # put them into one file
colnames(Days)<- c('day', 'city')                      # change the column names
tail(Days)                                             # check it
Days <- data.frame(Days)                               # make it a data frame
str(Days)                                              # check the structure

Days %>%                                               # plot
  group_by(day,city) %>%
  ggplot(aes(x=day, fill=city)) +
  geom_bar(position=position_dodge(0.9)) +
  labs( title="Rentals by City per Day of Week", y='Number of Rentals') +
  scale_x_discrete(labels=c('Mon', 'Tues', 'Wed', 'Thurs', 'Fri', 'Sat', 'Sun')) +
  theme(plot.title = element_text(hjust=0.5))

RentalChart <- cbind(summary(NY$day), summary(CH$day),summary(WA$day)) # start a chart by combining the columns
colnames(RentalChart)<- c('NY', 'CH', 'WA')            # correct the column names
RentalChart 
summary(RentalChart)  # display
## From this sample data we can see that there are no clear trends overall. Saturday and Sunday use of Bikeshare is less 
## than the rest of the week. Chicago usage is relatively level while Washington peaks slightly on Wednesday along with NY 
## and drops during the weekend. The data suggest that Wednesday is the most widely used day overall with 
## slight indication that the service may be used for traveling to and from work or school. It interesting to note that each 
## city's mean usage is exactly the same  at 42857 rentals.



# Question 3: What time of day do the most rentals occur?

# Make a function for plotting

Scatter_timeVSdruation_day=function(dataIn){
  
    #setup for title printing
    lab <- deparse(substitute(dataIn))        # prepare the file name for use in the title
    labNum <- strtoi(substring(lab,3,4))      # extract the number from the file name
    labcity <- substring(lab,1,2)             # extract the city from the file name
    Tlabel <- if(labNum <= 24) "Hours" else "Minutes"  # determine if the number is hours or minutes
  
  dataIn %>%
    group_by(hour, day)  %>%
    ggplot(aes(x=hour, y= Trip.Minutes, color = day)) +
    geom_point()+
    labs( title=paste("Starting time of rentals in",labcity,"for Trip durations less than", labNum, Tlabel), y ='Trip Duration (min)') +
    scale_x_discrete(name="Time of Day", breaks = c('03:00', '06:00', '09:00','12:00','15:00','18:00', '21:00'), labels= c('3', '6', '9', '12', '15', '18', '21'))  +
    theme(plot.title = element_text(hjust=0.5))                                                     
}


# For this I would like to look at a Scatter plot of rental time of day by rental duration and view it with respect to day.
# I start by taking the hour and minute of the start time of the rentals and save it in a column in the NY file
ttd <- format(as.POSIXct(NY$Start.Time), format = "%H:%M")  # grab the hour and minute
head(ttd)                                                        # check it
NY$hour <- ttd                                              # add it to the file
head(NY$hour)                                                     # check it

# From looking at the data previously I know there are outliers and there are a lot of data points. So...
# I am going to adjust the file to zoom in on where most of the trip durations are 
NY24 <- NY[NY$Trip.Minutes < 1440,]    # This is looking at rentals for just one day 60 min * 24 hours
NY4  <- NY[NY$Trip.Minutes < 240,]     # 4 hours ... to zoom in
NY50 <- NY[NY$Trip.Minutes < 50,]      # 50 minutes
# After viewing the different plots, I find that it is advantageous to look at 4hour and 50 minutes. 
# The trend seems to be clearer.

# Plotting all 0-4 hour duration rentals leads to a fruitful plot however, I would like to take a closer look. 

Scatter_timeVSdruation_day(NY4)

# So here I look at 0-50 minute rentals

Scatter_timeVSdruation_day(NY50)
# Zooming allows us to see that there is a trend in the time of rentals. Between 6 and 9 am on Monday through 
# Friday, rentals are abundant and picks up again around 5:30 pm. The usage is more spread out in the evening.


# This section I will look at Chicago using the same methods as for NY

ttd <- format(as.POSIXct(CH$Start.Time), format = "%H:%M")  # grab the hour and minute
head(ttd)                                                   # check it
CH$hour <- ttd                                              # add it to the file
head(CH$hour)                                               # check it
CH4  <- CH[CH$Trip.Minutes < 240,]     # 4 hours ... to zoom in
CH50 <- CH[CH$Trip.Minutes < 50,]      # 50 minutes


# Plotting all 0-4 hour duration rentals leads to a fruitful plot however I would like to take a closer look. 

Scatter_timeVSdruation_day(CH4)

# here I look at 0-50 minute rentals

Scatter_timeVSdruation_day(CH50)
# Zooming in we can see that there is a trend in the time of rentals. Between 6 and 9 am on Monday through Friday  
# rentals are abundant and picks up again around 5:00 pm, a bit earlier than NY. The usage is more spread out in 
# the evening.

# Here I prepare the data to look at Washington

ttd <- format(as.POSIXct(WA$Start.Time), format = "%H:%M")  # grab the hour and minute
head(ttd)                                                   # check it
WA$hour <- ttd                                              # add it to the file
head(WA$hour)                                               # check it
WA4  <- WA[WA$Trip.Minutes < 240,]     # 4 hours ... to zoom in
WA50 <- WA[WA$Trip.Minutes < 50,]      # 50 minutes


# Plotting all 0-4 hour duration rentals leads to a fruitful plot however, I would like to take a closer look. 

Scatter_timeVSdruation_day(WA4)

# here I look at 0-50 minute rentals

Scatter_timeVSdruation_day(WA50)
# Zooming in we can see that there is less of a trend in the time of rentals compared to NY and CH. Between 6 and 9 am on 
# Monday through Friday there are a lot of rentals for short periods of time as well as between 5 and 10 pm.
# The usage is more spread out in the evening which is the trend in all of the cities we have looked at. Saturday and Sunday
# have a lot of rentals spiking around 3 am and 12 noon. 

# Overall, it appears that the Bikeshare service is very widely used. The morning trends seem to be more consistent across
# the cities while the evening and weekend trends somewhat blur. While there is quite a bit of variety in rental times, The 
# majority tend to cluster in the 15-20 minute range. This may suggest that the bikes are being used to travel to a targeted 
# place. On Sundays the bikes are very popular with start times from 9 - 3 and lasting much longer than the 20 minute average
# rental. A consistent trend that I did not foresee is the bike rentals from midnight through 4 am. Could this be a popular
# time when restaurant staff gets off of their shift? Such questions could lead to a more in depth study of the usage trends.

