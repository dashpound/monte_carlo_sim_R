#===========================================================================
# Define variables
#===========================================================================
# Import triangle library
library(triangle)

# Research & Development costs; fixed expense
# Assume that R&D costs are paid in first year
min_cost <- 3000000
exp_cost <- 4000000
max_cost <- 6000000

# Cost to produce a machine; variable costs
min_cost_p_machine <- 12000
exp_cost_p_machine <- 14000
max_cost_p_machine <- 18000 

# Sales per year
min_sales <- 50
exp_sales <- 250
max_sales <- 350

# Revenue per machine; variable revenue
rev_p_machine <- 23000

# Market life of the product
min_ml <- 3
max_ml <- 8

# Cost of capital is 15%
disc_rate <- .15

#===========================================================================
# Create empty vectors to store outputs
#===========================================================================

results_periods <- vector('numeric', 0)
results_fixed_costs <- vector('numeric', 0)
results_var_costs <- vector('numeric', 0)
results_revenues <- vector('numeric', 0)
results_profits <- vector('numeric', 0)

#===========================================================================
# initialize simulations
#===========================================================================
sim = 10000

for(i in 1:sim){
  # Ensure Reproducability
  set.seed(i)
  
  #===========================================================================
  # Generate vectors of periods
  #===========================================================================
  
  # Select a random machine market life
  random_ml <- sample(min_ml:max_ml,1)
  
  # Create a vector of periods
  periods <- seq(1, random_ml, by=1)
  
  #===========================================================================
  # Generate vectors of fixed costs (R&D)
  #===========================================================================
  
  # Create a vector of min costs; paid in time 1
  v_min_cost <- integer(random_ml)
  v_min_cost[1] <- min_cost
  
  # Create a vector of exp costs; paid in time 1
  v_exp_cost <- integer(random_ml)
  v_exp_cost[1] <- exp_cost
  
  # Create a vector of max costs; paid in time 1
  v_max_cost <- integer(random_ml)
  v_max_cost[1] <- max_cost
  
  #===========================================================================
  # Generate vectors of random variables
  #===========================================================================
  
  # Create a vector of random costs; paid in time 1; used triangular distribution
  v_rng_cost <- integer(random_ml)
  rng_cost <- rtriangle(1, a=min_cost, b=max_cost, c=exp_cost)
  v_rng_cost[1] <- rng_cost
  
  # Create a vector of random sales; used triangular distribution
  rng_sales <- integer(random_ml)
  rng_sales <- rtriangle(random_ml, a=min_sales, b=max_sales, c=exp_sales)
  
  # Create a vector of random cost per machine; used triangular distribution
  rng_cost_p_machine <- integer(random_ml)
  rng_cost_p_machine <- rtriangle(random_ml, a=min_cost_p_machine,
                                  b=max_cost_p_machine, c=exp_cost_p_machine)
  
  #===========================================================================
  # Create dataframe
  #===========================================================================
  
  df <- data.frame(periods,v_min_cost, v_exp_cost, v_max_cost,v_rng_cost,
                   min_sales, exp_sales, max_sales, rng_sales,
                   rev_p_machine, disc_rate, 
                   min_cost_p_machine, exp_cost_p_machine, max_cost_p_machine,
                   rng_cost_p_machine
                   )
  
  #===========================================================================
  # Calculate revenue and expenses
  #===========================================================================
  
  df$rng_revenue <- df$rng_sales * rev_p_machine
  df$rng_var_expense <- df$rng_cost_p_machine * df$rng_sales
  
  #===========================================================================
  # Present Value
  #===========================================================================
  
  # Create a quick present value function
  pv_fun <- function(cf, rate, per){
    cf/(1+rate)^per
  }
  
  # Calculate net present value
  df$pv_rng_cost <- pv_fun(df$v_rng_cost,df$disc_rate, df$periods)
  df$pv_rng_var_exp <- pv_fun(df$rng_var_expense, df$disc_rate, df$periods)
  df$pv_rng_revenue <- pv_fun(df$rng_revenue, df$disc_rate, df$periods)
  
  #===========================================================================
  # Calculate Profit
  #===========================================================================
  
  df$pv_profit <- df$pv_rng_revenue - (df$pv_rng_cost + df$pv_rng_var_exp)

  #===========================================================================
  # View & Store output of simulation
  #===========================================================================
  
  # Number of periods
  results_periods[[i]] <- max(df$periods)
  
  # Fixed Costs
  results_fixed_costs[[i]] <- sum(df$pv_rng_cost)
  
  # variable_costs
  results_var_costs[[i]]<- sum(df$pv_rng_var_exp)
  
  # Total Revenues
  results_revenues[[i]]<- sum(df$pv_rng_revenue)
  
  # Total Profit
  results_profits[[i]] <- sum(df$pv_profit)
  
}

#===========================================================================
# Output
#===========================================================================

# Print an example 
ex = 1

results_periods[ex]
results_fixed_costs[ex]
results_var_costs[ex]
results_revenues[ex]
results_profits[ex]

# Probability that a profit the NPV will be greater than 0
sum(results_profits>0)/sim

# Average PV profits of 10,000 Simulations
mean(results_profits)
  
