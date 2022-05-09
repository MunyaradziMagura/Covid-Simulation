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

# virus invect

# create a person for every individual in the population
inilisePeople <- function(populationSize) {
  for(persons in 1:populationSize + 1){
    # people begin life, Healthy, Alive & Not Immune
    people[persons] <- c("Healthy","Alive","Not Immune")
  }
}

inilisePeople(numPopulation) # create people

print(values(people["1"])[1]) # get specific value

