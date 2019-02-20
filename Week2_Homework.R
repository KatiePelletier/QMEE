########## Loading Libraries #########
library(tidyverse)

######################################

#reading in pure line data
wings_raw <- read_tsv("PureLines_New.dat", col_names = TRUE)


#Creating a variable to remove uninformative/duplicate tags (file number, file path on lab PC, landmarks for creating spline, date and time file was made, sex (this tag is always MF, not informative), Perp (tag of imager used to track undergraduate work, spliner introduces error))
Crap_Col <- c("CPFile","Crap", "O1x", "O1y", "O2x", "O2y", "Date", "Time", "Tags", "Sex", "Perp")
<<<<<<< HEAD
More_crap <- c("Wings", "KP", "AfPop", "PL", "folder", "folder2")
=======
## BMB: nice.
>>>>>>> 71b6f68ba35ae480271766d26736320e9412b5aa

#Cleaning the raw data to extract important info from file names and create unique fly IDs within each line (can be used to select flies for genotyping later)
wings <- (wings_raw 
          %>% separate("File", into= c("Crap", "Spline", "Line", "Slide", "Real_Sex", "Fly"), sep = "_")
          %>% select(-Crap_Col)
          %>% separate("Spline", into = c("Wings", "KP", "AfPop", "PL", "folder", "folder2", "Spliner"))
          %>% select(-More_crap)
          %>% separate("Fly", into = c("Fly", "tiff"))
          %>% select(-"tiff")
          %>% unite(Fly_ID, Slide, Fly)
          
)


#Making line and sex and scale factors 

wings$Line <- factor(wings$Line)
wings$Real_Sex <- factor(wings$Real_Sex)
wings$Scale <- factor(wings$Scale)
<<<<<<< HEAD
wings$Spliner <- factor(wings$Spliner)

=======
## BMB: why setting this on wings_raw ... ??
levels(wings_raw$Scale) <- c("0.01134", "0.00694")
>>>>>>> 71b6f68ba35ae480271766d26736320e9412b5aa

## BMB: slightly safer to do this with factor()
wings <- (wings
    ## don't need explicit factor levels when they're alphabetical anyway?
    %>% mutate(Line=factor(Line),
    ## curious: why levels not in numeric order?   
               Scale=factor(Scale,levels=c("0.01134","0.00694")),
               Sex=factor(Real_Sex,levels=c("f","m")))
)
               
               
               



#I want to make sure that the fly IDs are unique within each line and sex. Should not be more than 1  
#The computer creates these numbers but the starting point has to be set by the human imaging the slide
print(wings
<<<<<<< HEAD
      %>% group_by(Line, Fly_ID)
      %>% summarize(Fly_ID)
)
=======
      %>% group_by(Line, Fly_ID, Sex)
      %>% summarise(count = n())
      %>% filter(count > 1))
>>>>>>> 8901daaeb43ae8a74cc261c12bd15d96af4a563f

## BMB: do this to check automatically ...
stopifnot(all(with(wings,table(Line,Fly_ID,Sex)) %in% c(0,1)))

## why not reorder lines by CS?
wings <- wings %>% mutate(Line=fct_reorder(Line,CS))
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

## or
clean_wings <- filter(wings, !(Fly_ID=="3_09.tif" & Real_Sex=="f" & Line == "ef96"))

#checking the boxplot. The outlier is gone!!!! 
ggplot(clean_wings, aes(y = CS, x = Line, color = Real_Sex)) + 
  geom_boxplot()

<<<<<<< HEAD
write_csv(clean_wings, "~/Dropbox/KatiePelletier/AfPop/Data/PureLines_Clean.csv", col_names = TRUE)

=======
## BMB: nice.
## score: 2.5 (2 = fine, 3=excellent)
>>>>>>> 71b6f68ba35ae480271766d26736320e9412b5aa

