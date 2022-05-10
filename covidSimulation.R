# total number of people in the simulation
numPopulation = 10

#total number of people a person can meet, a person can also meet the same person more than once 
meetPopulation = 10

# how long someone can spread the virus
infectiousPeriod = 10

# number of patinet Zeros within the simulaton
numPatientZero = 3

# a person has a random age between 1 & 99
# a person is assigned a random gender 
# a person is born healthy
# a person is immunized 
# a person has 0 Hospitalizations to begin with
# a person has 0 connections to begin with
# a person can be infectious for 10 days 
# create data frame 
People <<- data.frame (

  age =  c(round(runif(1, min=1, max=99))),   # 0 - 99
  
  gender = c(if (round(runif(1, min=1, max=99)) %% 2) "Female" else "Male"),   # [Female, Male]
  
  Vitality = c("Healthy"),  #[healthy, sick, dead]
  
  immunised = c("Immunized"),  # True or False 
  
  Hospitalizations = c(0), # if a person has been hospitalized
  
  connection = c(0), # if a sick person connects with this person
  
  infectious = c(0), #  if this person is sick they can infect others
  
  infectedToday = c(FALSE) # True if the person was infected today
)

# create people & infect a random number
createPeople <- function(populationSize, numInfected) {
  # number of infected 
  infected <- numInfected
  
  # create people
  for(person in 2:populationSize){
    # create a person and add them to the people dataframe
    People[nrow(People) + 1,] <<- c(round(runif(1, min=1, max=99)), if (round(runif(1, min=1, max=99)) %% 2) "Female" else "Male","Healthy","immunized",0,0,0,FALSE) 
  }
  
  # infect patient Zero's 
  while (infected > 0){
    victom <- round(runif(1, min=1, max=populationSize))
    if (People$Vitality[victom] == "Healthy"){
      infectPerson(victom)
      infected <- infected -1 
    }
  }
}

# this function is for infecting healthy people with covid people
infectPerson <- function(person){
  People$Vitality[person] <<- "Sick"
}



createPeople(numPopulation, numPatientZero) # create population poll
People
