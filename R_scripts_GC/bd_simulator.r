########################
#  Birth Death Models  #
########################
#     Grace Made      #
########################

#Set seed allows for you to control randomness, stats tool
#Population is a vector
set.seed(0)

birth1 <- 0.1
birth2 <- 0.1
death1 <- 0.05
death2 <- 0.05
trans_1_2<-0.5
trans_2_1<-0.5
#Probability 
#Max_Time 
steps <- 100
curr_time <- 0
max_taxa <- 100
population <- c(1, 1)

#Dataframe containing information about taxa and past events and time
# Format:
# Col1: # of taxa in state 1, Col2: # of taxa in state 2, Col3 time, Col4 Event
taxa_table_df <- data.frame(
  Taxa_1 = population[1],
  Taxa_2 = population[2],
  Time = curr_time,
  Event = 'N/A'
)

while(curr_time <= steps & sum(population) <= max_taxa & sum(population)!=0) {
  # birth of state 1
  birth_prob1 <- population[1] * birth1
  # birth of state 2
  birth_prob2 <- population[2] * birth2
  # death of state 1
  death_prob1 <- population[1] * death1
  # death of state 2
  death_prob2 <- population[2] * death2
  #q-matrix
  #
  #       1     2
  #q = 1 [-, rate 1 to 2]
  #    2 [rate 2 to 1, -]
  trans_matrix <- matrix(c(0, population[1]*trans_1_2, population[2]*trans_2_1, 0), nrow = 2, byrow = TRUE)
  
  #probability anything can happen
  all_rates <- c(birth_prob1, birth_prob2, death_prob1, death_prob2, trans_matrix[1, 2], trans_matrix[2, 1])
  next_time <- rexp(1, rate = 1/sum(all_rates))
  print(next_time)
  curr_time = 101
  
}
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


#Grace added with help of Sean 10-01

#Need to make rate relatice
relative <- cumsum(all_rates / sum(all_rates))

#Draw an event from a uniform distribution
draw <- runif(1)
#Series of else if to detering where the draw falls along the "stick"
if (draw < relative[1]) {
  print("speciation state 1")
} else if (draw < relative[2]) {
  print("speciation state 2")
} else if (draw < relative[3]) {
  print("death state 1")
} else if (draw < relative[4]) {
  print("death state 2")
} else if (draw < relative[5]) {
  print("transition from state 1 to 2")
} else if (draw < relative[6]) {
  print("transition from state 2 to 1")
}
