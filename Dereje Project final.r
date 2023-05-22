####Dereje Mengist Project
---
library(lattice)
library(ggplot2)
library(dplyr)
#1. read the data 
trips<-read.csv("ExData_trips.csv", header = TRUE, sep = ",") 

#2. selecting variables 
trips <- subset(trips, select=c(id, duration,
      start_station, end_station, 
      birth_date,gender,subscription_type))

#3. first we get rid off the gender factor with no value 
miss <- trips$gender == "" 
trips$gender[miss] <- NA

#after assigning NA for the non-used argument, 
#we can now avoid all missing values(NA)

nonmiss <- complete.cases(trips[c("id", "duration",
                        "start_station", "end_station", 
                       "birth_date","gender","subscription_type")])
trips <- trips[nonmiss, ]

#4. the function is stored in a separate file 
source("seconds2hours.R") 
#5
trips$hours <- seconds2hours(trips$duration)
#6
source("calc_age.R") #the same as previous function,
#its stored in the working directory 
#7. use this function to add an extra variable age to trips.
trips$age <- calc_age(trips$birth_date,
                      thisyear = 2011)
#8. Add a variable agecat that creates 3 categories based on age
trips$agecat <- cut(trips$age, breaks = c(0,25, 59, Inf),
                   labels = c('junior', 'adult', 'senior'))

#9 create a table with the average duration in hours 

with(trips, tapply(hours, list(agecat, gender), mean))

#10 create a histogram of the duration.
png("histogramDuration.png")
histogram(~ trips$duration, data=trips)

#11 histogram of the duration

id1<-trips$hours<1
den<-density(trips$duration[id1])
hist(trips$duration[id1], col = "grey",
          xlab = "Duration in seconds",
          main = "Duration of trips less than 1 hr", 
          freq = FALSE)
lines(den, col = 'blue', lwd = 2, lty = 2)
png("histogramDuration1hr.png")

#12  Create a table with the proportion of trips 

triphours <- cut(trips$hours,
               breaks=c(-Inf, 1, Inf),
               labels = c('less than a hour',
                          'one hour or longer'),
               right = FALSE)

prop.table(table(triphours))
#13 Create a boxplot

png("boxplotduration.png")
p13 <- ggplot(trips[id1,], aes(x = agecat, y = duration)) +
  geom_boxplot(fill = "grey")+
  labs(title = "Duration of age category with gender")
p13


#14 Plot both genders on different plot using facets
png('BoxplotGendervsDuration.png')
p14 <- ggplot(trips[id1,], aes(x=gender, y=duration)) +
  geom_boxplot(fill = "grey")+labs(title = 'Duration of age category with gender')+
  facet_grid(.~agecat)
  
p14

