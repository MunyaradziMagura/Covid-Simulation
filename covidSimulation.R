
#install.packages("plyr")
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("gridExtra")
#install.packages("reshape")
library(reshape)
library(gridExtra)
library(plyr)
library(ggplot2)
library(dplyr)
# total number of people in the simulation
numPopulation <- 1000

#total number of people a person can meet, a person can also meet the same person more than once 
maxMeet <- 10

# how long someone can spread the virus
infectiousPeriod <- 10

# number of patient Zeros within the simulation
numPatientZero <- 5

# how many days the pandemic runs for 
runTime <- 20

# percentage chance of a person dying  
deathPercentage <- 2/100

# infection percentage 
infectionPercentage <- 30/100

# natural immunity after getting infected and surviving  
HumanImmunity <- 5 / 100

# vaccine 
vaccineInjection = HumanImmunity * 4

# the chance someone will be hospitalized after being sick
HospitalizationRate <- 5/100

# oldest someone can get hospitalized
hospitazationOldest <- 50

# youngest someone can get hospitalized
hospitazationYoungest <- 5

# a person has a random age between 1 & 99
# a person is assigned a random gender 
# a person is born healthy
# a person is immunized if not their chance is a default 1 
# a person has 0 Hospitalizations to begin with
# a person has 0 connections to begin with
# a person can be infectious for 10 days 
# create data frame 
People <<- data.frame (
  
  age =  c(round(runif(numPopulation, min=1, max=99))),   # 0 - 99
  
  gender = c(rpois(n = numPopulation, lambda = 10)),   # [Female, Male]
  
  Vitality = c(rep("Healthy",numPopulation)),  #[Healthy, Sick, Dead]
  
  immunised = c(rep(1,numPopulation)),  #  has this person been immunized?
  
  Hospitalizations = c(rep(0, numPopulation)), # if a person has been hospitalized
  
  connection = c(rep(0, numPopulation)), # if a sick person connects with this person
  
  infectious = c(rep(0, numPopulation)), #  if this person is sick they can infect others
  
  infectedToday = c(rep(0, numPopulation)) # True if the person was infected today
)
# add sick people randomly
People$Vitality[sample(nrow(People),numPatientZero)] <- "Sick"

# increase infectious rate 
People <- within(People, infectious[Vitality == 'Sick'] <- infectiousPeriod)

# change numbers to male and female 
People$gender <- ifelse(People$gender %% 2 == 0, "Female", "Male")

# stores daily events
PandemicData <<- data.frame (
  day = c(1)
)

#  add people to the day dataframe
PandemicData <<- cbind(PandemicData, People[!names(People) %in% names(PandemicData)])

# Day 1 results 
print("DAY")
print(1)
print("Deaths")
print(nrow(subset(People, Vitality== "Dead")))
print("Sick")
print(nrow(subset(People, Vitality== "Sick")))
print("Hospitalizations")
print(nrow(subset(People, Hospitalizations > 0)))
print("Healthy")
print(nrow(subset(People, Vitality== "Healthy")))




# each day of the pandemic 
for (Currentday in 2:runTime){
  print("DAY")
  print(Currentday)
  print("Deaths")
  print(nrow(subset(People, Vitality== "Dead")))
  print("Sick")
  print(nrow(subset(People, Vitality== "Sick")))
  print("Hospitalizations")
  print(nrow(subset(People, Hospitalizations > 0)))
  print("Healthy")
  print(nrow(subset(People, Vitality== "Healthy")))
  print("###########################################")
  
  # dataframe to capture the daily results 
  dayResults <<- data.frame (
    day = c(Currentday)
  )
  
  # people met by sick people 
  met <- unique(c(round(runif(nrow(subset(People, Vitality != "Dead" & Vitality == "Sick")) * round(runif(1, 0,maxMeet)), min=1, max=numPopulation))))
  
  # if someone is already infected decrements their infectious period
  People$infectious <- ifelse(People$Vitality != "Dead" & People$infectious > 0,People$infectious -1,People$infectious)
  
  #people get immunity from the virus 
  People$immunised<- ifelse(People$Vitality != "Dead" & People$Vitality == "Sick" & People$infectious <= 0,HumanImmunity,People$immunised)
  
  # person is now healthy
  People$Vitality <- ifelse(People$Vitality != "Dead" & People$Vitality == "Sick" & People$infectious <= 0,"Healthy",People$Vitality)
  
  # Met person has a chance of death after getting the virus, if they do not die they will get sick 
  People$Vitality[met] <- ifelse(People$Vitality != "Dead" & People$Vitality[met] == "Healthy" & round(runif(1),2) * People$immunised[met] < infectionPercentage,  if(round(runif(1),2) < deathPercentage) "Dead" else "Sick","Healthy")
  
  # older people & those below 5 will have a higher chance of being hospitalized than younger people 
  People$Hospitalizations <-ifelse(People$Vitality != "Dead" & People$Vitality == "Sick" & (People$age > 50 | People$age < 5), if(round(runif(1),2) < HospitalizationRate + (People$age / 100)) People$Hospitalizations + 1 else People$Hospitalizations, if(round(runif(1),2) < HospitalizationRate) People$Hospitalizations + 1 else People$Hospitalizations)
  
  # make day data frame 
  dayResults <<- cbind(dayResults, People[!names(People) %in% names(dayResults)])
  
  # append daily results to pandemic data frame
  PandemicData <- rbind.fill(PandemicData, dayResults)
}


# PLOT LOGIC

# get the gender distribution
genderDistribution <- aggregate(People[c('gender')], by=list(People$gender == "Female"), FUN=length)
names(genderDistribution) <- c("gender", "total")
genderDistribution$gender <- ifelse(genderDistribution$gender == FALSE, "Male", "Female")




cases <- data.frame(
  titles = c("Total Cases","Total Deaths", "Active Cases"),
  # when simulation ended i.e. final day of the pandemic << active cases 
  data = c(nrow(PandemicData[PandemicData$Vitality == "Sick",]),nrow(People[People$Vitality == "Dead",]),nrow(People[People$Vitality == "Sick",]) )
)




# number of sick broken down by gender 
totalSick <- PandemicData[which(PandemicData$Vitality == 'Sick'),] %>% group_by(day,gender) %>% summarise(total = n())




# how many sick, Healthy & dead are there per day
simulationDailyTotal = data.frame(PandemicData %>% group_by(day,Vitality) %>% summarise(total = n()))
simulationDailyTotalHealthy = simulationDailyTotal[simulationDailyTotal$Vitality == "Healthy",]
simulationDailyTotalDead = simulationDailyTotal[simulationDailyTotal$Vitality == "Dead",]
simulationDailyTotalSick = simulationDailyTotal[simulationDailyTotal$Vitality == "Sick",]




# infected sick compared to those immunized. 1 being not immunised, and less than 1 being immunused
ImmunizedOrNot <- PandemicData[PandemicData$Vitality == "Sick",] %>% group_by(day,Vitality, immunised) %>% summarise(total = n())



# GENERATE PLOTS

#Population gender breakdown
ggplot(data=genderDistribution, aes(x=gender, y=total)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=total), vjust=-0.3, size=3.5)+
  labs(x='Gender', y='Total Population', title='Population gender breakdown')+theme_minimal()




# active cases, total deaths, and total infected
ggplot(cases, aes(x=titles, y=data, fill=titles)) +
  geom_bar(stat="identity")+theme_minimal()




# Daily Infections by Gender
ggplot(totalSick, aes(fill=gender, y=total, x=day)) + 
  geom_bar(position='dodge', stat='identity')+
  labs(x='Pandemic Days', y='Total infections', title='Daily Infections by Gender')+theme_minimal()




# Daily Infection sick compared to how many of them are immunized
ggplot(ImmunizedOrNot, aes(fill=immunised, y=total, x=day)) + 
  geom_bar(position='dodge', stat='identity')+
  labs(x='Pandemic Days', y='Total infections', title='Daily Infections by Gender')+theme_minimal()




# How many people are healthy daily
p1 = ggplot(simulationDailyTotalHealthy, aes(x = day, y = total,)) +
  geom_line(stat="identity",color='lightgreen',size=2)+labs(x='Pandemic Days', y='Total Healthy', title='Healthy individuals in population')+theme_minimal()

# how many people died per day
p2 = ggplot(simulationDailyTotalDead, aes(x = day, y = total)) +
  geom_line(stat="identity",color='darkblue',size=3)+labs(x='Pandemic Days', y='Total Dead', title='Daily Dead Total')+theme_minimal()

# how many people are sick daily
p3 = ggplot(simulationDailyTotalSick, aes(x = day, y = total)) +
  geom_line(stat="identity",color='darkred',size=2)+labs(x='Pandemic Days', y='Total Sick', title='Sick individuals in population')+theme_minimal()

grid.arrange(p1, p2, p3, ncol=1)



