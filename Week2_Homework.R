########## Loading Libraries #########
library(tidyverse)

######################################

#reading in pure line data
wings_raw <- read_tsv("~/Dropbox/KatiePelletier/AfPop/Data/PureLines_New.dat", col_names = TRUE)


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

#Making the wings object something easier to type
wings <- wings3
#names(wings)
#summary(wings)

#Making line and sex and scale factors 
levels(wings$Line) <- c("ef81", "ef96", "ef43", "zi192", "zi251", "zi418")
wings$Line <- factor(wings$Line)
levels(wings$Sex) <- c("f", "m")
wings$Sex <- factor(wings$Sex)
wings$Scale <- factor(wings$Scale)
levels(wings_raw$Scale) <- c("0.01134", "0.00694")

#Using a boxplot I want to look for outliers in centroid size 
ggplot(wings, aes(y = CS, x = Line, color = Sex)) + 
  geom_boxplot()
#There is one weird outlier in the EF96 female flies that is really small (like a male fly). I think it might be sexed wrong and want to remove this from the data set 

