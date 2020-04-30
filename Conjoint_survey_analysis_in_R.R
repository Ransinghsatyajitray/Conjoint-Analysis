
#Multi level full profile conjoint analysis:----

#Reading the data from xlsx

setwd("D:/Conjoint Analysis")

library(readxl)

library(conjoint)

library(dplyr)


x<-c(1,2,3,4)   #Variable 1 has 4 Level
y<-c(1,2,3)     #Variable 2 has 3 Level
z<-c(1,2,3,4,5) #Varaible 3 has 5 level

data_test<-as.data.frame(cbind(x,y,z)) #intermediate table

all_comb_var_lbl <- expand(data_test,x,y,z) # Finding possible combination of the variable level

#From this table we can delete the implausible combination to create variable Profile
#Profile can further be used for rating

#Loading Survey Score Results:
Survey_raw <- read_excel("Conjoint_raw_data_30042020.xlsx",sheet=1)

#Arranging the Survey Result Data in Longitudinal Format:
Survey_conj <- pivot_longer(data=Survey_raw,cols=Profile1:Profile49,names_to = "name",values_to = "Rating") %>% select(Rating)

#Profile (Plausible combination of Variable Level)
Profiles <- read_excel("Conjoint_raw_data_30042020.xlsx",sheet=2)


#Variable Levels:
levels_var <- read_excel("Conjoint_raw_data_30042020.xlsx",sheet=3) %>% select(Levels)


#Conjoint Analysis----

library(conjoint)

#For one line in the survey result only
caModel(y=Survey_raw[1,1:49],x=Profiles[,1:3])#1:3 represent the coded attribute level numbers in the data table

class(Survey_conj[,1])
#For full model and data in the survey result

Conjoint(as.data.frame(Survey_conj[,1]),as.data.frame(Profiles[,1:3]),z=levels_var[,1])


#Get importance of each attributes
caImportance(as.data.frame(Survey_raw[,1:49]),as.data.frame(Profiles[,1:3]))

# #Running a K mean clustering on observations
# caSegmentation(survey_result[2:31],data[,6:10],c=3)
