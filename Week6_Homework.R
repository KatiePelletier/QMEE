library(dotwhisker)
library(stats)


load("clean_wings.rda")

#Hypothesis: genetic background (in this case, the source population my flies are derived from) has an effect on wing size, with flies from high altitude populations being larger
#We also know that sex has an effect on size and that sex effects interact with genetic background. I think this is also the case for my animals 



#Intercept is ef43/f
#First a model where Line and sex are additive (independent)
mod1 <- lm(CS ~ Line + Sex, data = wings)

summary(mod1)

par(mfrow=c(2,2))
plot(mod1)

#The relationship look linear here.  
#However the variance is not the same across the data set, we already knew this from previous plots 
#The residuals are also not normal. Likley because we are missing something in the model.
#This model is ignoring the fact that these lines were derived from two diffrent source populations (low and high altitude)
#This could be one factor contributing to the non-normality of my residulals. 

#Now a model where Line and sex can interact 
mod2 <- lm(CS ~ Line*Sex, data = wings)


summary(mod2)

par(mfrow=c(2,2))
plot(mod2)
#These plots look the same as above 



#Quickly comparing these two models with an anova test, the sum of squares of the residuals are similar but there is a significant  diffrence between the two models
#This indicates that there is an interaction between genetic background and sex in my flies. 
anova(mod1, mod2)


#Comparing the coefficents in the two models. 
#These appear to have similar coefficents for all of the terms that are predicted by both models
#In fact, many of the interaction coeffiecnt predictions cross the intercept value (95% CI), except in the case of zi425 and zi418
#Accounting for these effects was enough to make a significant reduction to the residual sum of squares between these two models 
dwplot(list(additive=mod1, interaction=mod2))

       