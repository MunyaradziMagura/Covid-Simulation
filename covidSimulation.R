# total number of perople in the simulation
N_population = 10



People <- data.frame (
  # 0 - 99
  age <- c(), 
  # [Other, Female, Male]
  gender <- c(), 
  #[healthy, sick, dead]
  Vitality <- c(),
  # True or False 
  immunised <- c(),
  # if a person has been hospitalized
  Hospitalizations <- c()
)

# create data frame 
#covid_Data Frame <- data.frame()
createPeople <- function(populationSize) {
  for(person in 1:populationSize){
    # a person has a random age between 1 & 99
    # a person is assigned a random gender 
    # a person is born healthy
    # a person is immunized 
    # a person has 0 Hospitalizations to begin with
    
  }
}
# this function is for infecting healthy people with covid people
infectPerson <- funcion(person){
  People$Vitality[person] <- "sick"
}

# this function is for killing people
killPerson <- function(person){
  People$Vitality[person] <- "dead"
}

# this function is for making someone healthy
healPerson <- function(person){
  People$Vitality[person] <- "healthy"
}

# this function is for immunising people
immunisePerson <- function(person){
  People$Vitality[person] <- "immunised"
}


createPeople(N_population)
People
