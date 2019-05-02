##Libraries 
library(lme4)
library(car)
library(effects)

#Data 
load("clean_wings.rda")

#Maximal model: for Centroid size, this would include Sex, Line and Altitude effects 
#Line effects are nested within altitude effects (Alt) as each line comes from either the high or low altitude population
#For this data set, I probably do not have enough groups sampled to use a mixed model, a linear model is probably a better choice for this
# In this model, I listed sex and alt as fixed effects and line as a random effect 
# For what I am interested in, I have sampled all possible sexes (male and female) and all alt (high and low)
# I sampled only three isogenic lines from each population to represent some of the genetic diversity
## JD: Why??

mod_lmm1 <- lmer(CS ~ Sex + Alt + (1 | Alt:Line), data = wings)

summary(mod_lmm1)

#from class notes, some diognositics
#fitted vs residual looks good
plot(mod_lmm1)
#fitted-location plot also looks ok. 
plot(mod_lmm1, sqrt(abs(resid(.))) ~ fitted(.),
     type=c("p","smooth"), col.line="red")

# I see a significant diffrence between the groups 
## JD: Don't lead with this. Say what you see and stick the significance (or clarity) in as you go.
Anova(mod_lmm1)
#The plots shouw the expected response with females and high alt flies being bigger. 
## JD: Good
plot(allEffects(mod_lmm1))

## Grade 2.1/3 good
