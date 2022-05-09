# dependiences
if(!require('hash')) {
  install.packages('hash')
  library('hash')
}
# Population size 
numPopulation <- 100
# individuals in population
# each person will have a key i.e. 1 and a value [False,False,False]
# the first index of the value represents a persons vitality, True = Healthy & False = Sick
# the second index represents a persons life, True = Alive & False = Dead 
# the third index represents a persons immunisation status, True = immune & False = Not Immune
people <- hash()

# create a person for every individual in the population
inilisePeople <- function(populationSize) {
  for(persons in 0:populationSize){
    # people begin life, Healthy, Alive & Not Immune
    people[persons] <- []
  }
}

inilisePeople(numPopulation) # call the function named my_function
