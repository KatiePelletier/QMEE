########## Loading Libraries #########
library(tidyverse)

######################################

#reading in pure line data
wings_raw <- read_tsv("PureLines_New.dat", col_names = TRUE)

#checking the table 
#summary(wings_raw)
#dim(wings_raw)
#names(wings_raw)
#wings_raw[1:10, 1:10]

#Removing the extra stuff from the TPS file that is useless (pre-spline landmarks, date, time, tags, sex)
#Here, the sex is listed as MF for everything because this is how the TPS file is built, the actual sex is in the file name 
wings1 <- wings_raw[,-c(1,3:6,8:10,12)]
#names(wings1)

#Need to extract the sex, fly ID and line from the file name
wings2 <- separate(wings1, "File", into= c("Crap", "Perp", "Line", "Slide", "Sex", "Fly"), sep = "_")
#names(wings2)

#getting rid of the crap (file path and my initials)
wings3 <- wings2[, -c(1)]
#names(wings3)

## JD: As we said in class, this could all be done with one clean pipeline
## Don't rename things in loops; either keep the names simple (my preference)
## or move forward consistently
#Making the wings object something easier to type
wings <- wings3
#names(wings)
#summary(wings)

#Making line and sex and scale factors 
# JD: This scares me; you shouldn't be keeping track of your labeling system in code
# Have a table or something
levels(wings$Line) <- c("ef81", "ef96", "ef43", "zi192", "zi251", "zi418")
wings$Line <- factor(wings$Line)
levels(wings$Sex) <- c("f", "m")
wings$Sex <- factor(wings$Sex)
wings$Scale <- factor(wings$Scale)
levels(wings_raw$Scale) <- c("0.01134", "0.00694")

###### Now I can do a calculation with my data! Calculating the mean size (CS for centroid size in data set) for each line 

LineMean_Size <- (wings
      %>% group_by(Line)
      %>% summarise(mean = mean(CS))
)
LineMean_Size

## JD: score 2. (1=poor, 2=fine, 3=excellent)

