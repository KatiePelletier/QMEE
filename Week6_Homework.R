library(dotwhisker)
library(stats) ## BMB: don't need to load this explicitly (gets auto-loaded)


load("clean_wings.rda") ## BMB: thanks for saving/loading!

#Hypothesis: genetic background (in this case, the source population my flies are derived from) has an effect on wing size, with flies from high altitude populations being larger
#We also know that sex has an effect on size and that sex effects interact with genetic background. I think this is also the case for my animals 

#Intercept is ef43/f
#First a model where Line and sex are additive (independent)
mod1 <- lm(CS ~ Line + Sex, data = wings)

summary(mod1)

op <- par(mfrow=c(2,2))
plot(mod1)
par(op)

#The relationship looks linear here.  
#However the variance is not the same across the data set, we already knew this from previous plots 
#The residuals are also not normal. Likely because we are missing something in the model.
#This model is ignoring the fact that these lines were derived from two different source populations (low and high altitude)
##This could be one factor contributing to the non-normality of my residuals.

## BMB: I think I might see a quadratic pattern here, which would explain both residuals-fitted
## and scale-location (heteroscedasticity) pattern
plot(mod1,which=1)

## BMB: But none of the patterns I see here look particularly large/concerning

#Now a model where Line and sex can interact 
mod2 <- lm(CS ~ Line*Sex, data = wings)


summary(mod2)

op <- par(mfrow=c(2,2))
plot(mod2)
par(op)
#These plots look the same as above 
## BMB: may have gotten rid of 


#Quickly comparing these two models with an anova test, the sum of squares of the residuals are similar but there is a significant  difference between the two models
##This indicates that there is an interaction between genetic background and sex in my flies.
## BMB: try not to say "there is an interaction".  You know there is an interaction,
##  because it's biology.  Say you can detect it, or that there's a clear difference
## in the difference between sexes among genetic backgrounds.
anova(mod1, mod2)


#Comparing the coefficients in the two models. 
#These appear to have similar coefficients for all of the terms that are predicted by both models
#In fact, many of the interaction coefficient predictions cross the intercept value (95% CI), except in the case of zi425 and zi418
#Accounting for these effects was enough to make a significant reduction to the residual sum of squares between these two models 
dwplot(list(additive=mod1, interaction=mod2))+geom_vline(xintercept=0,lty=2)

## reformulate model so that intercept=mean across lines
mod2B <- update(mod2,contrasts=list(Line=contr.sum))
dwplot(list(additive=mod1, interaction=mod2B))+geom_vline(xintercept=0,lty=2)

## BMB: focus attention on sex effects only ...
library(dplyr)
library(broom)
library(stringr)
tt <- (list(additive=tidy(mod1),interaction=tidy(mod2))
    %>% bind_rows(.id="model")
    %>% filter(str_detect(term,"Sexm"))
)
dwplot(tt)+geom_vline(xintercept=0,lty=2)

library(emmeans)
plot(emmeans(mod2,~Sex|Line))
## vignette("interactions")
## BMB: this is probably what you're most interested in ...
plot(pairs(emmeans(mod2,~Sex|Line)))

## BMB: score=2.25
