library(tidyverse)

wings <- read_csv("PureLines_Clean.csv")

wings$Line <- factor(wings$Line)
wings$Real_Sex <- factor(wings$Real_Sex)


#for this week I want to visulize the relationship between size and shape in my data.
#first, to be able to visulize shape data, I need to do a PC decomposition

PCs <- prcomp(wings_alt[, 6:101])
 
#adding PC values to the wings data frame 
#We keep the first 56 PCs (number of demetions in data) explains ~99.95% of varriance 
 
summary(PCs)

wings_PCs <- data.frame(wings_alt, PCs$x[,1:56]) 

#Now I will plot the wing size vs PC1. Because there is a scaling step during shape data collection, no PCs will nessisarity be assosiated with size
#Assosiation between size and shape indicates an allometric effect on wing shape 
#The vectors are going to be really short because we don't typically see a large varriation in wing size within a line 
#Plotting real data with a trendline
#if anything, these realationships look hypoallometric 


ggplot(wings_PCs, aes(x = log(CS), y = PC1, color = Real_Sex)) + 
  geom_point(alpha = 0.2, size = 0.5) +
  geom_smooth() +
  facet_wrap(~Line)



