##Libraries 
library(ggplot2)

##Data
load("clean_wings.rda")

#My data is contius size data so I am using a gamma distribution with a log link function 

glm_mod1 <- glm(CS ~ Line*Sex, family=Gamma(link="log"), data = wings)
summary(glm_mod1)

#Running linear mod from previous week as comparison
lm_mod1 <- lm(CS ~ Line*Sex, data = wings)

#These diagnostic plots look almost identical. 
#This is because the two models I ran are very similar. 
plot(glm_mod1)
plot(lm_mod1)

#Because the linear model is the simplier option and had a simpler fit for my data, I would go ahead with using this in my analysis.



              




