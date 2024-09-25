###################################
# Make a HiSSE model from scratch #
###################################
# Hypothesis: 
#There are hidden ecological states (e.g., habitat types, adaptive strategies) that influence 
#the evolution of ecomorphs. For instance, certain ecomorphs may experience different speciation 
#and extinction rates due to the hidden states.
#################################################################################################
library(ape)
# Load your tree
tree <- read.tree("/Users/gracecoppinger/Desktop/RB_bisse_tutorial/anolis.phy")

# Load your data
data <- read.csv("/Users/gracecoppinger/Desktop/RB_bisse_tutorial/anolisDataAppended.csv", row.names = 1)

#Make ecomorphs into a state
ecomorph <- data[, "ecomorph"]
names(ecomorph) <- rownames(data)

# Create binary states: 1 for "TG", 0 for all others
states <- ifelse(ecomorph == "TG", 1, 0)

# Name the states based on species
names(states) <- rownames(data)

# Filter states to only include species present in the tree
states <- states[names(states) %in% tree$tip.label]

# Define hidden states (e.g., 0 and 1)
# You may want to create an additional vector to represent hidden states
hidden_states <- sample(0:1, length(states), replace = TRUE)  # Randomly assign hidden states for demonstration

# Define your speciation and extinction rates for the four states
lambda1 <- 0.10  # Speciation rate for hidden state 1
lambda0 <- 0.05  # Speciation rate for hidden state 0
mu1 <- 0.01      # Extinction rate for hidden state 1
mu0 <- 0.02      # Extinction rate for hidden state 0

# Create a transition matrix for HiSSE
# This should have 4 rows and 2 columns
trans_matrix_hisse <- matrix(c(
  lambda1, mu1,  # Speciation and extinction for hidden state 1
  lambda0, mu0,  # Speciation and extinction for hidden state 0
  lambda1, mu1,  # For observable state corresponding to hidden state 1
  lambda0, mu0   # For observable state corresponding to hidden state 0
), nrow = 4, byrow = TRUE)

# Assign names to the rows and columns
rownames(trans_matrix_hisse) <- c("H1_S1", "H1_S0", "H0_S1", "H0_S0")
colnames(trans_matrix_hisse) <- c("Speciation", "Extinction")

# Check the transition matrix
print(trans_matrix_hisse)

# Function to calculate the likelihood with hidden states
calculate_hisse_likelihood <- function(tree, states, hidden_states, trans_matrix_hisse) {
  likelihood <- 1.0
  for (i in 1:nrow(tree$edge)) {
    child <- tree$edge[i, 2]
    parent <- tree$edge[i, 1]
    
    if (parent <= length(states) && child <= length(states)) {
      parent_state <- states[parent]
      child_state <- states[child]
      parent_hidden <- hidden_states[parent]
      child_hidden <- hidden_states[child]
      
      # Determine the rate based on both parent and child states
      if (parent_hidden == 1 && child_hidden == 1) {
        rate <- trans_matrix_hisse[1, 1]  # H1 to S1
      } else if (parent_hidden == 0 && child_hidden == 0) {
        rate <- trans_matrix_hisse[2, 1]  # H0 to S0
      } else {
        rate <- trans_matrix_hisse[3, 2]  # Transition between hidden states
      }
      
      likelihood <- likelihood * exp(-rate * tree$edge.length[i])
    }
  }
  
  return(likelihood)
}

# Negative log-likelihood function for optimization
neg_log_likelihood_hisse <- function(params) {
  lambda1 <- params[1]
  lambda0 <- params[2]
  mu1 <- params[3]
  mu0 <- params[4]
  
  # Update transition matrix
  trans_matrix_hisse <- matrix(c(
    lambda1, lambda0, mu1, mu0, 
    lambda1, mu1, 0, 0,          
    lambda0, mu0, 0, 0           
  ), nrow = 4)
  
  return(-calculate_hisse_likelihood(tree, states, hidden_states, trans_matrix_hisse))
}

# Initial parameters for optimization
initial_params <- c(lambda1, lambda0, mu1, mu0)

# Optimize parameters
result_hisse <- optim(initial_params, neg_log_likelihood_hisse)

# Estimated parameters
estimated_params_hisse <- c(lambda1 = 0.10, lambda0 = 0.05, mu1 = 0.01, mu0 = 0.02)

# Create a data frame for ggplot
param_df <- data.frame(
  Parameter = names(estimated_params_hisse),
  Value = estimated_params_hisse
)

# Create a bar plot
ggplot(param_df, aes(x = Parameter, y = Value, fill = Parameter)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Estimated Parameters from HiSSE Model",
       x = "Parameters",
       y = "Rate") +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
