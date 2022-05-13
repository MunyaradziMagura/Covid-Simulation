
#install.packages("plyr")
library("plyr")


# total number of people in the simulation
numPopulation <- 100

#total number of people a person can meet, a person can also meet the same person more than once 
maxMeet <- 10

# how long someone can spread the virus
infectiousPeriod <- 10

# number of patinet Zeros within the simulation
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
length(People[Vitality == "Sick"])
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
  
  # dataframe to capture the daily results 
  dayResults <<- data.frame (
    day = c(Currentday)
  )
  
  # people met by sick people 
  met <- unique(c(round(runif(nrow(subset(People, Vitality == "Sick")) * round(runif(1, 0,maxMeet)), min=1, max=numPopulation))))
  print("People met")
  print(length(met))
  
  # will they get infected
  #People$Vitality[People$Vitality[met] == "Healthy"] <- ifelse(People$Vitality[met] == "Healthy" & , "Healthy", "Sick")
  
  # if someone is already infected decrements their infectious period
  People$infectious <- ifelse(People$infectious > 0,People$infectious -1,People$infectious)
  #people get immunity from the virus 
  People$immunised<- ifelse(People$Vitality == "Sick" & People$infectious <= 0,HumanImmunity,People$immunised)
  # person is now healthy
  People$Vitality <- ifelse(People$Vitality == "Sick" & People$infectious <= 0,"Healthy",People$Vitality)
  
  # Met person has a chance of death after getting the virus, if they do not die they will get sick 
  People$Vitality[met] <- ifelse(People$Vitality[met] == "Healthy" & round(runif(1),2) * People$immunised[met] < infectionPercentage,  if(round(runif(1),2) < deathPercentage) "Dead" else "Sick","Healthy")
  
  # older people & those below 5 will have a higher chance of being hospitalized than younger people 
  People$Hospitalizations <-ifelse(People$Vitality == "Sick" & (People$age > 50 | People$age < 5), if(round(runif(1),2) < HospitalizationRate) People$Hospitalizations + 1 else People$Hospitalizations,People$Hospitalizations)
  
  print("###########################################")
  # make day data frame 
  dayResults <<- cbind(dayResults, People[!names(People) %in% names(dayResults)])
  
  # append daily results to pandemic data frame
  PandemicData <- rbind.fill(PandemicData, dayResults)
}

