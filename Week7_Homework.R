library(R2jags)
library(coda)
library(lattice)
library(dotwhisker)

####Setting up my model and assumptions made
#I based my model off the linear model I used last week for the homework (part1, no interaction term) so I can directly compare the two results 
##This is a simple linear model, as I know that my data are linear and follow a normal distribution
## BMB (at least up to a reasonable approximation)
#Although usually a log scale is more appropriate for measurements, because all the measurements we work with are similar, the data remain linear in this range
#I set up my priors based on the published effect sizes for altitude and sex in natural populations of Drosophila (Pitchers et al. 2012 Evolution)
#This paper used a number of natural African populations, but did not include the populations from my data set 
## BMB: OK

#I am asking if high altitude populations are larger than low altitude populations. 

######Data 
load("clean_wings.rda")


#Additive linear model for last week
lm1 <- lm(CS ~ Sex + Alt, data = wings)
summary(lm1)

#Number of rows int the df
N <- nrow(wings)

#Making factors numeric. 
#for sex 1 = f and 2 = m 
#For Alt HA = 1 and LA = 2
wings$Sex <- as.numeric(wings$Sex)
wings$Alt <- as.numeric(wings$Alt)


#Model
##Had to hard code the Alt because the loop wouldn't work. I don't know if what I did was right

## Make sure your case matches in filenames
bayesmod <- with(wings, jags(model.file='week7_New.bug'
                          , parameters=c("b_Sex", "b_Alt")
                          , data = list('CS' = CS, 'Alt'=Alt, 'Sex'=Sex, 'N'=N)
                          , n.chains = 4
                          , inits=NULL
)) 

#Diagnostic plots 
#The trace looks pretty stable and I think this is good. 
#Even with a fairly uninformative prior, the sex effect matches very well with the model from last week (lm1) and the priors from Pitchers at al (2016)
#However, the CI are really big, more informative priors could help with this?
#Alt values make sense becks I centred the female mean at zero and these are effects in addition to this. 
print(bayesmod)
plot(bayesmod)
traceplot(bayesmod)

#This shows the effects more clearly, although both sex and LA overlap zero because of the large CI 
## JD: doesn't work for me unfortunately. Version problems.
## dwplot(bayesmod)

## Grade 2/3 Good
