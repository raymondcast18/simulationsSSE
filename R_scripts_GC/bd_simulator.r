########################
#  Birth Death Models  #
########################
#     Grace Made      #
########################

#Set seed allows for you to control randomness, stats tool
#Population is a vector
set.seed(0)

p_birth <- 0.1
p_death <- 0.05
p_no_event <- 1 - p_birth - p_death 
#Probability 
steps <- 100
population <- 1

for (i in 1:steps) {
#If you still are alive draw again  
  if (tail(population, 1) > 0) {
#Decide what happens, something or nothing    
    temp <- sample(x = c(-1, 0, 1),
                   size =1,
                   prob = c(p_death, p_no_event, p_birth))
#Apply
    population <- c(population,
                    tail(population, 1) + temp)
    next
  }
#Changed the code to be 0 if there is no members of the population
else {population[i + 1] <- 0}
#If you have nobody. can have no event of a birth event  
  # if(tail(population, 1) == 0){
  #   temp <- sample(x = c(0,1),
  #                  size=1,
  #                  prob = c(p_no_event / (p_no_event + p_birth),
  #                           p_birth / p_no_event + p_birth))
  #   population <- c(population,
  #                   tail(population, 1) + temp)
}
plot(population, type = "l")
