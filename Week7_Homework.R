library(R2jags)
library(coda)
library(lattice)
library(dotwhisker)

####Setting up my model and assumptions made
#I based my model off the linear model I used last week for the homework (part1, no interaction term) so I can directly compare the two results 
#This is a simple linear model, as I know that my data are linear and follow a normal distribution 
#Although usually a log scale is more appropriate for measurements, because all the measurements we work with are similar, the data remain linear in this range
#I set up my proiors based on the published effect sizes for altitude and sex in natural populations of Drosophila (Pitchers et al. 2012 Evolution)
#This paper used a number of natural African populations, but did not include the populations from my data set 

#I am asking if high altitude populations are larger than low altituude populations. 

######Data 
load("clean_wings.rda")


#Additive linear model for last week
lm1 <- lm(CS ~ Sex + Alt, data = wings)
summary(lm1)

#Number of rows int the df
N <- nrow(wings)

#Making facotors numaric. 
#for sex 1 = f and 2 = m 
#For Alt HA = 1 and LA = 2
wings$Sex <- as.numeric(wings$Sex)
wings$Alt <- as.numeric(wings$Alt)


#Model
#Had to hard code the Alt because the loop wouldn't work. I don't know if what I did was right
bayesmod <- with(wings, jags(model.file='Week7_New.bug'
                          , parameters=c("b_Sex", "b_Alt")
                          , data = list('CS' = CS, 'Alt'=Alt, 'Sex'=Sex, 'N'=N)
                          , n.chains = 4
                          , inits=NULL
)) 

#Diognostic plots 
#The trace looks pretty stable and I think this is good. 
#Even with a fairly uniformative prior, the sex effect matches very well with the model from last week (lm1) and the priors from Pitchers at al (2016)
#However, the CI are really big, more informative priors could help with this?
#Alt values make sense becuase I centred the female mean at zero and these are effects in addtion to this. 
print(bayesmod)
plot(bayesmod)
traceplot(bayesmod)

#This shows the eggects more clearly, although both sex and LA overlap zero because of the large CI 
dwplot(bayesmod)

