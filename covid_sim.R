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

# create a person for every individual in the population
inilisePeople <- function(populationSize) {
  for(persons in 0:populationSize){
    # people begin life, Healthy, Alive & Not Immune
    people[persons] <- list("Healthy","Alive","Not Immune")
  }
}

inilisePeople(numPopulation) # create people
print(people)
