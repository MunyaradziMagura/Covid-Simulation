# total number of perople in the simulation
N_population = 10

#total number of people a person can meet 
meet_population = 10

infectious_period = 10

# a person has a random age between 1 & 99
# a person is assigned a random gender 
# a person is born healthy
# a person is immunized 
# a person has 0 Hospitalizations to begin with
# a person has 0 connections to begin with
# a person can be infectious for 10 days 
# create data frame 
People <<- data.frame (

  age <-  c(as.numeric(round(runif(1, min=1, max=99)))),   # 0 - 99
  
  gender <- c(if (round(runif(1, min=1, max=99)) %% 2) "Female" else "Male"),   # [Female, Male]
  
  Vitality <- c("Healthy"),  #[healthy, sick, dead]
  
  immunised <- c("immunized"),  # True or False 
  
  Hospitalizations <- c(0), # if a person has been hospitalized
  
  connection <- c(0), # if a sick person connects with this person
  
  infectious <- c(0), #  if this person is sick they can infect others
  
  infectedToday <- c(FALSE) # True if the person was infected today
)

# create people
createPeople <- function(populationSize) {
  for(me in 2:populationSize){
    # create a person and add them to the people dataframe
    People[nrow(People) + 1,] <<- c(as.numeric(round(runif(1, min=1, max=99))), if (round(runif(1, min=1, max=99)) %% 2) "Female" else "Male","Healthy","immunized",as.numeric(0),as.numeric(0),as.numeric(0),FALSE) 
  }
}

# this function is for infecting healthy people with covid people
infectPerson <- function(person){
  People$Vitality[person] <<- "sick"
}

# this function is for killing people
killPerson <- function(person){
  People$Vitality[person] <<- "dead"
}

# this function is for making someone healthy
healPerson <- function(person){
  People$Vitality[person] <<- "healthy"
}

# this function is for immunizing people
immunisePerson <- function(person){
  People$Vitality[person] <<- "immunised"
}

createPeople(N_population) # create population poll
People
