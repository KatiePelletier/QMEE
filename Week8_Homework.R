##Libraries 
library(ggplot2)

##Data
load("clean_wings.rda")

#My data is continuous size data so I am using a gamma distribution with a log link function 

glm_mod1 <- glm(CS ~ Line*Sex, family=Gamma(link="log"), data = wings)
summary(glm_mod1)

#Running linear mod from previous week as comparison
lm_mod1 <- lm(CS ~ Line*Sex, data = wings)

#These diagnostic plots look almost identical. 
#This is because the two models I ran are very similar. 
plot(glm_mod1)  ## BMB: fat-tailed distribution?
plot(lm_mod1)   ## BMB: slightly better

#Because the linear model is the simpler option and had a simpler fit for my data, I would go ahead with using this in my analysis.

## BMB: seems reasonable.

## scale-location plot slightly better for LM
op <- par(mfrow=c(1,2))
plot(glm_mod1,which=3)
plot(lm_mod1,which=3)
par(op)

              




