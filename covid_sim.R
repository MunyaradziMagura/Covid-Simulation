# dependiences
if(!require('hash')) {
  install.packages('hash')
  library('hash')
}
# Population size 
numPopulation <- 100

# individuals in population
# each person will have a key i.e. 1 and a value [Healthy,Alive,Not Immune]
# the first index of the value represents a persons vitality, Healthy or Sick
# the second index represents a persons life, Alive or Dead 
# the third index represents a persons immunisation status, Immune or Not Immune
people <- hash()

# how many people are infected with the virus at the start of the pandemic?
patientZero <- 2

# Vaccine rollout day (must be less than pandemic period). Patients can only be immunised after this date 
vaccineRollOut = 20
# virus invect

# create a person for every individual in the population
inilisePeople <- function(populationSize) {
  # person infected
  unlucky<-list(runif(patientZero, min=1, max=populationSize))
  unlucky<-round(unlucky)
  print(unlucky)
  
  for(persons in 1:populationSize + 1){
    # people begin life healthy or sick, Alive & Not Immune 
    if(persons %in% unlucky){
      # this person has been unfortunately infected 
      people[persons] <- c("sick","Alive","Not Immune")
      }else{
      # this person has been fortunate and is healthy 
      people[persons] <- c("Healthy","Alive","Not Immune")
    }

  }
}


infectPatient <- function(patient){
  ##### ERROR HERE v
  values(people[toString(patient)])[1] <- "sick"
}

inilisePeople(numPopulation) # create people
print(people["92"])
