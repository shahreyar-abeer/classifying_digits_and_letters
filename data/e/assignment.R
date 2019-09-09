# install a package
install.packages("dplyr")
library(dplyr)

# upload data into e_1 dataframe
e_1 <- read.csv("e_1.csv", sep=",")
View(e_1)
# remove first column
e_1 <- e_1[,2:17]

# create function for nr_pix
nr_pix <- function(data){
  sum(data) }
# run number of pixels function 
nr_pix(e_1)
# works

# create height function
height <- function(data){
  count(data[rowSums(data==1)>0,])  }
# run height function
height(e_1)

# this_new should be the variable you put all your functions into
# for each function do this_new$_____ <- name of function
this_new <- NULL
this_new$nr_pix <- nr_pix(e_1)
this_new$height <- height(e_1)
print(this_new)

# this_new is a list containing nr_pix and height
# paste is a base function that pastes stuff together 

e_1string <- paste(unlist(this_new), sep = ",")
print(e_1string)
