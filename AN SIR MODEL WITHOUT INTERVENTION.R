
##Adding changes in the IR model to develop SIR model
## 1. Evidence synthesis
initial_number_susceptible <- 10000000 # Adding susceptibles to IR model
initial_number_infected <- 1 # Considering one infected at start time
initial_number_recovered <- 0 
Duration_of_infectiousness <- 7 
recovery_rate <- 1/Duration_of_infectiousness
Reproduction_number <- 2.28 #R0== beta/gamma,
infection_rate <- Reproduction_number*(1/Duration_of_infectiousness) #beta
follow_up_duration <- 300
## 2. Create model input using parameters
initial_state_values <- c(S = initial_number_susceptible, # Adding susceptibles
                          I = initial_number_infected, 
                          R = initial_number_recovered) 

parameters <- c(beta = infection_rate, # Adding beta parameter 
                gamma = recovery_rate) 

times <- seq(from = 0, to = follow_up_duration, by = 1) 

## 3. Create a model using input values

cohort_model <- function(time, state, parameters) {     
  with(as.list(c(state, parameters)), { 
    N <- S+I+R # Total population
    lambda <- beta * I/N #Calculating lambda as function of beta
    dS <- -lambda * S               
    dI <- lambda * S - gamma * I    
    dR <- gamma * I   
    return(list(c(dS, dI, dR)))                             
  })
}
## 4. Return a dataframe as output using model equations
output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = cohort_model,
                            parms = parameters))
## 5. Exploratory Data Analysis (Visualisation) of the output
dat <- output %>% pivot_longer(cols = c(S,I,R),
                               names_to = "state",
                               values_to = "value")
dat %>% ggplot(aes(x = time,
                   y = value,
                   color = state,
                   group = state))+
  geom_line()+
  xlab("Time(Days)")+
  ylab("Number of persons")+
  labs(title = "Changes in Suceptible, Infected and Recovered")

