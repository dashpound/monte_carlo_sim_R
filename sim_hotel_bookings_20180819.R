#===========================================================================
# Define variables
#===========================================================================
n <- 100 #number of rooms
revenue <- 150 #Revenue per room per night
var_expense <- 30 #Variable expenses per room
pen_expense <- 200 #Cost for each guest overbooked
p_noshow <- 0.05 #Probability that a guest no shows

# Ensure Reproducability
set.seed(42)

# Number of rooms to simulate
n_rooms_simmed <- seq(90,120, by=1) 

# Number of simulations to run
sim <- max(n_rooms_simmed)-min(n_rooms_simmed)

#===========================================================================
# Create empty vectors to store outputs
#===========================================================================

v_booked_rooms <- vector('numeric', 0)
v_no_shows <- vector('numeric', 0)
v_occupancy <- vector('numeric', 0)
v_expenses <- vector('numeric', 0)
v_revenues <- vector('numeric', 0)
v_profits <- vector('numeric', 0)

#===========================================================================
# Simulation
#===========================================================================
for(i in 1:sim) {
  # Vector of rooms booked
  v_booked <- seq(1,n_rooms_simmed[i], by=1)

  # Vector of random numbers, assume uniform distribution
  rando <- runif(n=n_rooms_simmed[i])
  
  # Convert to dataframe
  df <- data.frame(rando,v_booked)
  
  # Create indicator in dataframe, if there is a noshow
  df$no_show_ind <- ifelse(df$rando <= p_noshow,1,0)
  
  # Create create cumulative column for no shows
  df$cum_noshow <- ave(df$no_show_ind, FUN=cumsum)
  
  # Create column of room booked - no shows
  df$occupancy <- df$v_booked - df$cum_noshow
  
  # Create column of revenues
  df$revenues <- (1-df$no_show_ind) * revenue
  
  # Create column of expenses
  df$expenses <- (1-df$no_show_ind) * var_expense
  
  # Create overbooked indicator 
  df$ob_ind <- ifelse(df$occupancy >= n,1,0)
  
  # Create penalty for overbooking 
  df$ob_cost <- df$ob_ind * pen_expense
  
  # Create total cost function 
  df$t_cost <- df$expenses + df$ob_cost
  
  #create profit column
  df$profit <- df$revenue - df$t_cost
  
  #===========================================================================
  # View & Store output of simulation
  #===========================================================================
  
  # Number of rooms booked
  v_booked_rooms[[i]] <- n_rooms_simmed[i]
  
  # Number of no shows
  v_no_shows[[i]] <- sum(df$no_show_ind)
  
  # Occupancy
  v_occupancy[[i]]<- n_rooms_simmed[i] - sum(df$no_show_ind)
  
  # Total Expenses
  v_expenses[[i]]<- sum(df$t_cost)
  
  # Total Revenues
  v_revenues[[i]]<- sum(df$revenues)
  
  # Total Profit
  v_profits[[i]] <- sum(df$profit)
  
  }

#===========================================================================
# View Total outputs
#===========================================================================

v_booked_rooms
v_no_shows
v_occupancy
v_expenses 
v_revenues
v_profits

best_solution <- which.max(v_profits)

#===========================================================================
# Final Reccomendation based on simulaton
#===========================================================================

v_booked_rooms[best_solution]
v_no_shows[best_solution]
v_occupancy[best_solution]
v_expenses[best_solution]
v_revenues[best_solution]
v_profits[best_solution]
