#This time we will use just the : operator. 
#Use the : operator to create a sequence of consecutive integers starting at 12 and ending at 73 and save it in an object x, 
#then determine the length of object x.
x <- 12:73
length(x)



# Create a vector containing all the positive odd numbers smaller than 100.
# The numbers should be in ascending order
seq(1,99,2)


#Create a vector of numbers that starts at 6, 
#does not go beyond 55, and adds numbers in increments of 4/7. 
#So the first three numbers will be 6, 6+4/7, and 6+8/7. 
#How many numbers does the list have? Use only one line of code to answer both questions.
x<-seq(6, 55, 4/7)
length(x)


#Use the $ operator to access the population size data and store 
#it in the object pop.
#Then use the sort function to redefine pop so that it is sorted.
#Finally use the [ operator to report the smallest population size.
# Access population values from the dataset and store it in pop
pop <- murders$pop
# Sort the object and save it in the same object 
pop <- sort(pop)
# Report the smallest population size 
pop[1]


# Access population from the dataset and store it in pop
pop <- murders$population
# Use the command order to find the vector of indexes that order pop and store in object ord
ord <-order(pop)
# Find the index number of the entry with the smallest population size
ord[1]


# Find the index of the smallest value for variable total 
which.min(murders$total)


# Define the variable i to be the index of the smallest state
i <- which.min(murders$population)
# Define variable states to hold the states
states <- murders$state
# Use the index you just defined to find the state with the smallest population
states[i]


# Define a variable states to be the state names 
states <- murders$state 

# Define a variable ranks to determine the population size ranks 
ranks <- rank(murders$population)
# Create a data frame my_df with the state name and its rank
my_df <- data.frame(states,  ranks)


# Define a variable states to be the state names from the murders data frame
states <- murders$state
# Define a variable ranks to determine the population size ranks 
ranks <- rank(murders$population) 
# Define a variable ind to store the indexes needed to order the population values
ind <- order(murders$population)
# Create a data frame my_df with the state name and its rank and ordered from least populous to most 
my_df<- data.frame(states[ind],ranks[ind])


# Using new dataset 
library(dslabs)
data(na_example)
# Checking the structure 
str(na_example)
# Find out the mean of the entire dataset 
mean(na_example)
# Use is.na to create a logical index ind that tells which entries are NA
ind <- is.na(na_example)
# Determine how many NA ind has using the sum function
sum(ind)


# Note what we can do with the ! operator
x <- c(1, 2, 3)
ind <- c(FALSE, TRUE, FALSE)
x[!ind]
# Create the ind vector
library(dslabs)
data(na_example)
ind <- is.na(na_example)
# We saw that this gives an NA
mean(na_example)
# Compute the average, for entries of na_example that are not NA 
mean(na_example[!ind])


# Assign city names to `city` 
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")
# Store temperature values in `temp`
temp <- c(35, 88, 42, 84, 81, 30)
# Convert temperature into Celsius and overwrite the original values of 'temp' with these Celsius values
temp<-(temp-32) *5/9
# Create a data frame `city_temps` 
city_temps <- data.frame(city,temp)


# Define an object `x` with the numbers 1 through 100
x <- seq(1:100)
# Compute the sum 
sum(1/x^2)


# Load the data
library(dslabs)
data(murders)
# Store the per 100,000 murder rate for each state in murder_rate
murder_rate <- murders$total/murders$population*100000
# Calculate the average murder rate in the US
mean(murder_rate)


# Store the murder rate per 100,000 for each state, in `murder_rate`
murder_rate <- murders$total / murders$population * 100000
# Store the `murder_rate < 1` in `low` 
low <- murder_rate<1
low


# Store the murder rate per 100,000 for each state, in murder_rate
murder_rate <- murders$total/murders$population*100000
# Store the murder_rate < 1 in low 
low <- murder_rate < 1
# Get the indices of entries that are below 1
which(low)


# Store the murder rate per 100,000 for each state, in murder_rate
murder_rate <- murders$total/murders$population*100000
# Store the murder_rate < 1 in low 
low <- murder_rate < 1
# Names of states with murder rates lower than 1
murders$state[low]


# Store the murder rate per 100,000 for each state, in `murder_rate`
murder_rate <- murders$total/murders$population*100000
# Store the `murder_rate < 1` in `low` 
low <- murder_rate < 1
# Create a vector ind for states in the Northeast and with murder rates lower than 1. 
ind <- low &  murders$region=="Northeast"
# Names of states in `ind` 
murders$state[ind] 


# Store the murder rate per 100,000 for each state, in murder_rate
murder_rate <- murders$total/murders$population*100000
# Compute the average murder rate using `mean` and store it in object named `avg`
avg <-mean(murder_rate)
# How many states are < avg ? Check using sum 
sum(murder_rate < avg)


# Store the 5 abbreviations in abbs. (remember that they are character vectors)
abbs <- c("MA", "ME", "MI", "MO", "MU") 
# Use the `which` command and `!` operator to find out which index abbreviations are not actually part of the dataset and store in `ind`
ind <- which(!abbs%in%murders$abb)
# Names of abbreviations in `ind`
abbs[ind]


# Loading data
library(dslabs)
data(murders)
# Loading dplyr
library(dplyr)
# Redefine murders so that it includes a column named rate with the per 100,000 murder rates
murders <- mutate(murders,rate=(total/population)*100000)
murders


# Note that if you want ranks from highest to lowest you can take the negative and ten compute the ranks 
x <- c(88, 100, 83, 92, 94)
rank(-x)
# Defining rate
rate <-  murders$total/ murders$population * 100000
# Redefine murders to include a column named rank
# with the ranks of rate from highest to lowest
murders <- mutate(murders, rank = rank(-rate))


# Load dplyr
library(dplyr)
# Use select to only show state names and abbreviations from murders
select(murders,state,abb)


# Add the necessary columns
murders <- mutate(murders, rate = total/population * 100000, rank = rank(-rate))
# Filter to show the top 5 states with the highest murder rates
filter(murders,rank<6)


# Use filter to create a new data frame no_south
no_south <- filter(murders,region!="South") 
# Use nrow() to calculate the number of rows
nrow(no_south)


# Create a new data frame called murders_nw with only the states from the northeast and the west
murders_nw <-filter(murders,region %in% c("Northeast","West"))
# Number of states (rows) in this category 
nrow(murders_nw)




# Store the 3 abbreviations in a vector called `abbs` (remember that they are character vectors and need quotes)
abbs<-c("AK","MI","IA")
# Match the abbs to the murders$abb and store in ind
ind<-match(abbs,murders$abb)
# Print state names from ind
murders$state[ind]


# Store the 5 abbreviations in `abbs`. (remember that they are character vectors)
abbs <- c("MA", "ME", "MI", "MO", "MU")
# Use the %in% command to check if the entries of abbs are abbreviations in the the murders data frame
abbs %in% murders$abb

# Create a new data frame called murders_nw with only the states from the northeast and the west
murders_nw <-filter(murders,region %in% c("Northeast","West"))
# Number of states (rows) in this category 
nrow(murders_nw)


# add the rate column
murders <- mutate(murders, rate =  total / population * 100000, rank = rank(-rate))
# Create a table, call it `my_states`, that satisfies both the conditions 
my_states <- filter(murders, region %in% c("Northeast", "West") & rate < 1)
# Use select to show only the state name, the murder rate and the rank
select(my_states, state, rate, rank)


# Load library
library(dplyr)
## Define the rate column
murders <- mutate(murders, rate =  total / population * 100000, rank = rank(-rate))
# show the result and only include the state, rate, and rank columns, all in one line
filter(murders,region %in% c("Northeast","West") & rate<1) %>%
  select(state,rate,rank)


# Loading the libraries
library(dplyr)
data(murders)
# Create new data frame called my_states (with specifications in the instructions)
my_states <- murders %>% mutate(rate =  total / population * 100000, rank = rank(-rate)) %>% filter(region %in% c("Northeast","West") & rate<1) %>% select(state,rate,rank)


# Load the datasets and define some variables
library(dslabs)
data(murders)
population_in_millions <- murders$population/10^6
total_gun_murders <- murders$total
plot(population_in_millions, total_gun_murders)
# Transform population using the log10 transformation and save to object log10_population
log10_population <- log(murders$population,base=10)
# Transform total gun murders using log10 transformation and save to object log10_total_gun_murders
log10_total_gun_murders <- log(murders$total,base=10)
# Create a scatterplot with the log scale transformed population and murders 
plot(log10_population,log10_total_gun_murders)


# Store the population in millions and save to population_in_millions 
population_in_millions <- murders$population/10^6
# Create a histogram of this variable
hist(population_in_millions)


# Create a boxplot of state populations by region for the murders dataset
boxplot(population~region,data=murders)


#First, determine the average height in this dataset. Then create a logical vector ind with the indices for those individuals who are above average height.
#How many individuals in the dataset are above average height?
ind <- heights$height > mean(heights$height)
sum(ind)


#How many individuals in the dataset are above average height and are female?
fem <- heights$height > mean(heights$height) & heights$sex=="Female"
sum(fem)


#Use the ifelse function to write one line of 
#code that assigns to the object new_names the state abbreviation 
#when the state name is longer than 8 characters 
#and assigns the state name when the name is not 
#longer than 8 characters.
new_names<-ifelse(nchar(murders$state)>8,murders$abb,murders$state)
new_names


#We will define a function sum_n for this exercise.
#Create a function sum_n that for any given value, say n, creates the vector 1:n, and then computes the sum of the integers from 1 to n.
#Use the function you just defined to determine the sum of integers from 1 to 5,000.
# Create function called `sum_n`
sum_n<-function(n)
{
  x<-1:n
  sum(x)
}
# Use the function to determine the sum of integers from 1 to 5000
sum_n(5000)


# Write a function compute_s_n with argument n that for any given n computes the sum of 1 + 2^2 + ...+ n^2
compute_s_n <- function(n){
  x <- 1:n
  sum(x^2)
}
# Report the value of the sum when n=10
compute_s_n(10)


# Define a function and store it in `compute_s_n`
compute_s_n <- function(n){
  x <- 1:n
  sum(x^2)
}
# Create a vector for storing results
s_n <- vector("numeric", 25)
# Assign values to `n` and `s_n`
for(i in 1:25){
  s_n[i] <- compute_s_n(i)
}


# Define the function
compute_s_n <- function(n){
  x <- 1:n
  sum(x^2)
}
# Define the vector of n
n <- 1:25
# Define the vector to store data
s_n <- vector("numeric", 25)
for(i in n){
  s_n[i] <- compute_s_n(i)
}
# Check that s_n is identical to the formula given in the instructions.
identical(s_n,(n*(n+1)*(2*n+1))/6)


