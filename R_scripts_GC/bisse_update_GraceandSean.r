set.seed(0)
#rates
birth1<- 0.1
birth2<- 0.9
death1<- 0.5
death2<- 0.3
q12<- 0.4
q21<- 0.8
#Preventions
stop_time <- 1000
curr_time <- 0
max_taxa <- 100

sim_BiSSE = function(birth1, birth2, death1, death2, q12, q21, stop_time, curr_time, max_taxa){
  
  #Change to have binary states
  population <- c(1,1)
  
  # Running list of IDs
  taxa_id <- rep(list(1), sum(population))
  
  # Taxa Relationships contains the parent-daughter relationships between taxa that will be added as the process is simulated
  taxa_relationships <- data.frame(
    Taxa_ID = unlist(taxa_id) ,
    d_1 = rep(0, length(taxa_id)),
    d_2 = rep(0, length(taxa_id)),
    p = rep(0, length(taxa_id))
  )
  
  
  #Make a table
  taxa_table <- data.frame(
    Taxa_1 = population[1],
    Taxa_2 = population[2],
    Time = curr_time,
    Event = 'Start'
  )
  #The condition for this while loop prevents the current time from exceding steps, population exceding max taxa, and the population being below zero
  while (curr_time <= stop_time && sum(population) <= max_taxa && sum(population)!=0) {
    #find the prob of birth in state 1
    birth_prob1 <- population[1] * birth1
    #find the prob of birth in state 2
    birth_prob2 <- population[2] * birth2
    #find the prob of death in state 1
    death_prob1 <- population[1] * death1
    #find the prob of death in state 2
    death_prob2 <- population[2] * death2
    #Q matrix
    trans_matrix<- matrix(c(0, population[1]*q12, population[2]*q21, 0), nrow = 2, byrow = TRUE)
    #Make all the rate relative
    all_rates <- c(birth_prob1, birth_prob2, death_prob1, death_prob2, trans_matrix[1,2], trans_matrix[2,1])
    next_time <- rexp(1, rate=1/sum(all_rates))
    #Make it end at 100
    curr_time = curr_time + next_time
  
  
  relative <- cumsum(all_rates / sum(all_rates))
  
  #Draw an event
  draw <- runif(1)
  if (draw < relative[1]) {
    print("speciation state 1")
    population[1] = population[1] + 1
    event = "speciation state 1"
  } else if (draw < relative[2]) {
    print("speciation state 2")
    population[2] = population[2] + 1
    event = "speciation state 2"
  } else if (draw < relative[3]) {
    print("death state 1")
    population[1] = population[1] - 1
    event = "death state 1"
  } else if (draw < relative[4]) {
    print("death state 2")
    population[2] = population[2] - 1
    event = "death state 2"
  } else if (draw < relative[5]) {
    print("transition from state 1 to 2")
    population[1] = population[1] - 1
    population[2] = population[2] + 1
    event = "transition from state 1 to 2"
  } else if (draw < relative[6]) {
    print("transition from state 2 to 1")
    population[2] = population[2] - 1
    population[1] = population[1] + 1
    event = "transition from state 2 to 1"
  }
  
  
  taxa_table_row= c( population[1],
                     population[2],
                         curr_time,
                             event 
      )
  
  taxa_table = rbind(taxa_table, taxa_table_row)
  
  
  
  }
  return(taxa_table)

}

result <- sim_BiSSE(birth1, birth2, death1, death2, q12, q21, stop_time, curr_time, max_taxa)
  

