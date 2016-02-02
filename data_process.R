require(readr)
require(dplyr)

options(dplyr.print_max = Inf)
options(dplyr.width = Inf)


setwd("~/AV/Last man standing")

train <- read_csv("./Train_Fyxd0t8.csv")
test <- read_csv("./Test_C1XBIYq.csv")
sample_submission <- read_csv("./Sample_Submission_Psj3sjG.csv")

train$trainFlag <- 1
test$trainFlag <- 0
test$Crop_Damage <- NA

alldata <- rbind(train, test)

alldata$Season_cropType <- as.integer(as.factor(paste(alldata$Season, alldata$Crop_Type, sep = "_")))
alldata$Season_soilType <- as.integer(as.factor(paste(alldata$Season, alldata$Soil_Type, sep = "_")))
alldata$cropType_soilType <- as.integer(as.factor(paste(alldata$Crop_Type, alldata$Soil_Type, sep = "_")))
alldata$insect_cnt_by_dose_freq <- alldata$Estimated_Insects_Count/alldata$Number_Doses_Week
alldata$total_dosage <- alldata$Number_Doses_Week * alldata$Number_Weeks_Used
alldata$weeks_since_pesticide <- alldata$Number_Weeks_Used + alldata$Number_Weeks_Quit
alldata$weeks_used_by_weeks_quit <- alldata$Number_Weeks_Used / alldata$Number_Weeks_Quit

save(alldata, file = "./alldata_LMS.RData")
