########## Loading Libraries #########
library(tidyverse)

######################################

#reading in pure line data
wings_raw <- read_tsv("PureLines_New.dat", col_names = TRUE)


#Creating a variable to remove uninformative/duplicate tags (file number, file path on lab PC, landmarks for creating spline, date and time file was made, sex (this tag is always MF, not infomrative), Perp (tag of imager used to track undergraduate work, spliner introduces error))
Crap_Col <- c("CPFile","Crap", "O1x", "O1y", "O2x", "O2y", "Date", "Time", "Tags", "Sex", "Perp")


#Cleaning the raw data to extract important info from file names and create unique fly IDs within each line (can be used to select flies for genotyping later)
wings <- (wings_raw 
          %>% separate("File", into= c("Crap", "Spline", "Line", "Slide", "Real_Sex", "Fly"), sep = "_")
          %>% select(-Crap_Col)
          %>% unite(Fly_ID, Slide, Fly)
)

#Making line and sex and scale factors 
levels(wings$Line) <- c("ef81", "ef96", "ef43", "zi192", "zi251", "zi418")
wings$Line <- factor(wings$Line)
levels(wings$Real_Sex) <- c("f", "m")
wings$Sex <- factor(wings$Real_Sex)
wings$Scale <- factor(wings$Scale)
levels(wings_raw$Scale) <- c("0.01134", "0.00694")


#I want to make sure that the fly IDs are unique within each line and sex. Should not be more than 1  
#The computer creates these numbers but the starting point has to be set by the human imaging the slide
print(wings
      %>% group_by(Line, Fly_ID, Sex)
      %>% summarise(count = n())
      %>% filter(count > 1))

#Using a boxplot I want to look for outliers in centroid size 
ggplot(wings, aes(y = CS, x = Line, color = Sex)) + 
  geom_boxplot()

#There is one weird outlier in the EF96 female flies that is really small (like a male fly). I think it might be sexed wrong and want to remove this from the data set because I don't trust it
#this is the smallest ef96 f in the data set 

#Fist I will identify the flyID of this animal 
print(wings
      %>% filter(Line == "ef96", Real_Sex == "f")
      %>% top_n(-1, CS)
      %>% select(Fly_ID, Line, Real_Sex)
  )


#removing the outlier from the data set 
clean_wings <- wings[!(wings$Fly_ID == "3_09.tif" & wings$Real_Sex == "f" & wings$Line == "ef96"),]

#checking the boxplot. The outlier is gone!!!! 
ggplot(clean_wings, aes(y = CS, x = Line, color = Sex)) + 
  geom_boxplot()


