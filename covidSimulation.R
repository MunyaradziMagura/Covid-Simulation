# total number of perople in the simulation
N_population = 10


# a person has a random age between 1 & 99
# a person is assigned a random gender 
# a person is born healthy
# a person is immunized 
# a person has 0 Hospitalizations to begin with
# create data frame 
People <<- data.frame (

  age =  c(30),   # 0 - 99
  
  gender = c("Female"),   # [Female, Male]
  
  Vitality = c("Healthy"),  #[healthy, sick, dead]
  
  immunised = c("immunized"),  # True or False 
  
  Hospitalizations = c(0) # if a person has been hospitalized
)


# create people
createPeople <- function(populationSize) {
  for(me in 1:populationSize){
    People[nrow(People) + 1,] <<- c(as.numeric(round(runif(1, min=1, max=99))), if (round(runif(1, min=1, max=99)) %% 2) "Female" else "Male","Healthy","immunized",as.numeric(0)) 
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
