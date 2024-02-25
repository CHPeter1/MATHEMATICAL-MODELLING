##MODELLING AN EPIDEMIC
##Why do we model epidemics?
#Predict the current conditions
#Predict future progress of any given epidemic
#Understand the mechanisms behind the epidemic
#Understand which interventions might be successful

##How do we model epidemics?
#Identify key biological processes
#Abstract them into a mathematical model
#Investigate effect(s) of changing parameters

## SI MODEL
#Assume
#fixed population of constant size (S+I=N)
#All members of the population are in contact

#Rate at which a susceptible person gets infected depends upon:
#The number or proportion of infected persons
#The transmission rate β

##Rate of infection
#We are going to focus on frequency dependent transmission
#This is the most appropriate for models in humans
#Livestock models may use density dependent transmission
library(deSolve)
library(tidyverse)
library(DiagrammeR)

##Understanding the model
grViz("digraph flowchart {
      graph [layout = dot,
       rankdir = LR]
      node [fontname = Helvetica, shape = oval,]
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      # edge definitions with the node IDs
      tab1 -> tab2;
      
      }

      [1]: 'INFECTED'
      [2]: 'RECOVERED'
      ")

##1.Evidence synthesis
#As a pre-requisite to model building, we need to undertake evidence 
#synthesis exercise to extract required parameters from the literature or by conducting a study.

initial_number_infected <- 1000
initial_number_recovered <- 0 
Duration_of_infectiousness <- 7 
recovery_rate <- 1/Duration_of_infectiousness
follow_up_duration <- 30

##2. Create model input using parameters
#Once parameters are obtained, they are placed into three major 
#parameter inputs viz initial values for each compartment, 
#transition parameters from one compartment to another, and the time 
#stamps for which modelling is required.

initial_state_values <- c(I = initial_number_infected, 
                          R = initial_number_recovered) 

parameters <- c(gamma = recovery_rate) 
times <- seq(from = 0, to = follow_up_duration, by = 1) 

##3. Create a model function using input values
#The model is created from the above mentioned parameters 
#by specifying differential equations. In R, a function is an 
#object so that the researcher is able to pass control to the function, 
#along with arguments that may be necessary for the function to accomplish 
#the actions. For performing differntial equations, we create a function with 
#compartment as well as transition parameter at varied timestamps.

cohort_model <- function(time, state, parameters) {     
  with(as.list(c(state, parameters)), {  
    dI <- -gamma * I
    dR <- gamma * I
    return(list(c(dI, dR)))                             
  })
}

##4. Return a dataframe as output using model equations
#A data frame is constructed by application of the differential model. 
#Herein, Odinary Differential Equations (ode) function takes initial compartment values, 
#timestamps, parameters and the model function as arguments.

output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = cohort_model,
                            parms = parameters))

##5. Exploratory Data Analysis of the output
#To understand the output let us first see the dimensions of our dataframe

dim(output)

#To look at the first 5 values of the data

head(output,5)

#Since, more than one column is representing the state of 
#the person (Infected or recovered), we need to transform 
#the data into tidy format. A tidy data format is said to 
#present when each column represents one variable, each row 
#has one observation and each cell has one value.

dat <- output %>% pivot_longer(cols = c(I,R),
                               names_to = "state",
                               values_to = "value")
head(dat,5)     

#Visualization is a recommended exploratory data analysis method. 
#Let us see how infected and recovered compartments were changing in the model.

dat %>% ggplot(aes(x = time,
                   y = value,
                   color = state,
                   group = state))+
  geom_line()+
  xlab("Time(Days)")+
  ylab("Number of persons")+
  labs(title = "Changes in Infected and Recovered individuals")
