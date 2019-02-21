library(tidyverse)



#Shape and size data from the 6 seleceted lines 
load("clean_wings.rda")
#For this assignment HA = high altitude and LA = low altitdude


#Hypthesis 1: The altitude the population originates from does have an effect on wing size
#Flies from a HA population are predicted to have larger wings than flies from a LA population
#Going to shuffle wing size (CS) and calculate the diffrence between the means

#This is saying that CS is not numeric when I run this. is.numeric(altshuffle$CS) and is.numeric(wings$CS) both return true
#Not sure how to fix this problem but I think the code is correct otherwise. 
#Commented out this section for the homework to advoid a lot of error messages 

set.seed(1203)
CS_perm <- numeric(1000)

#loop to calculate vector
for (i in 1:1000) {
  #scramble CS variable
   perm <- sample(nrow(wings))
   altshuffle <- transform(wings, CS=CS[perm])
   #calculate the diffrence in means
   #This says my CS is not numeric
   #is.numeric(altshuffle$CS) and is.numeric(wings$CS) both return true
   HA_perm <- subset(altshuffle, Alt == "HA") 
   LA_perm <- subset(altshuffle, Alt == "LA")
   HA_CS_mean <- mean(HA_perm$CS)
   LA_CS_mean <- mean(LA_perm$CS)
   CS_perm[i] <- HA_CS_mean - LA_CS_mean
 }

HA_obs_wings <- subset(wings, Alt == "HA") 
LA_obs_wings <- subset(wings, Alt == "LA")
HA_obs_mean <- mean(HA_obs_wings$CS)
LA_obs_mean <- mean(LA_obs_wings$CS)

 CS_observed <- HA_obs_mean - LA_obs_mean
 #Now I want to add the observed value to the permuted data set
 CS_all <- c(CS_observed, CS_perm)

 #calculating the p-value
 p_val <- 2*mean(CS_all >= CS_observed)
 p_val
 
 #now a t-test. This gives a diffrent p-value than above. 
 t_test <- t.test(CS~Alt, data = wings)
 t_test$p.value

#Hypothesis 2: Altitude also has an effect on wing shape 
#This is a much harder test because shape is not a univariate measure. 

#To test this, I am going to calaulate the diffrence bettween the LA and HA mean shape for only females (as sex can create a confounding effect)
#Then I will do a purmutation test where I scramble the Alt assignment of the flies but keep the all other predictors constant (1000 replicates)
#Then, I will select only female flies again and compute the magitude of diffrence in the means. 
#From this I will ask how many times the permutated magnitude is larger than the observed. 


#Calculate mean shape of HA and mean shape of LA in the observed data 
#first subseting the data and extracting the landmarks (cols 6:101) as a matrix, then calculating
#If I have time, I should come back and write these as functions... 
HA_observed_mean <- (wings
             %>% subset(Alt == "HA" & Sex == "f", drop = T ) 
             %>% select(6:101)
             %>% colMeans()
  )
LA_observed_mean <-(wings
             %>% subset(Alt == "LA" & Sex == "f", drop = T )
             %>% select(6:101)
             %>% colMeans()
  )
#The magnitude of the diffrence between the two means 
#Adding the function to make the calculation code below more readable (takes the absolute value of the vector sum)
norm_vec <- function(x) sqrt(sum(x^2))

PD_observed <- norm_vec(HA_observed_mean - LA_observed_mean)


#Creating a vector to record 1000 observations
set.seed(1203)
PD_perm <- numeric(1000)

#loop to calculate vector
for (i in 1:1000) {
  #scramble Sex variable
  perm <- sample(nrow(wings))
  sexshuffle <- transform(wings, Alt = Alt[perm])
  #get new mean shape vectors for high and low
  HA_mean <- (sexshuffle
                %>% subset(Alt == "HA" & Sex == "f", drop = T ) 
                %>% select(6:101)
                %>% colMeans()
            )
  LA_mean <- (sexshuffle
              %>% subset(Alt == "LA" & Sex == "f", drop = T ) 
              %>% select(6:101)
              %>% colMeans()
            )
  #compute and store magnitude in diffrence between the means 
  PD_perm[i] <- norm_vec(HA_mean - LA_mean)
}

#Now I want to add the observed value to the permuted data set 
PD_all <- c(PD_observed, PD_perm)

#calculating the p-value 
#From this, I would say that Sex doesn't matter (or at least doesn't have a large effect) for the observed change in shape.
#However, the distribution (below) makes me question my results. 
p_val <- mean(PD_all >= PD_observed)
p_val

#My observed distance is the largest calulated PD by far. 

hist(PD_all)
abline(v=PD_observed,col="red")
