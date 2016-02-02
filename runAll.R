require(readr)
require(caret)
require(xgboost)

setwd("F:/AV/Last man standing")
set.seed(2135)
source("./source.R")

load("./alldata_LMS.RData")

train <- alldata[alldata$trainFlag == 1,]
test <- alldata[alldata$trainFlag == 0,]

train$int_1 <- train$Estimated_Insects_Count/train$total_dosage
train[is.na(train)] <- -1
train[train == Inf] <- -1
train[train == -Inf] <- -1

test$int_1 <- test$Estimated_Insects_Count/test$total_dosage
test[is.na(test)] <- -1
test[test == Inf] <- -1
test[test == -Inf] <- -1

feature.names <- names(train) [!names(train) %in% c("ID", "Crop_Damage", "trainFlag", "Season_cropType", "Season_soilType", "cropType_soilType")]




############################# RUN THE XGB MODEL #############################
param <- list(objective = "multi:softprob",
              num_class = 3,
              max_depth = 8,
              eta = 0.01,
              subsample = 0.7,
              colsample_bytree = 0.8,
              min_child_weight = 40,
              max_delta_step = 3,
              gamma = 0.3,
              eval_metric = "mlogloss")

dtrain <- xgb.DMatrix(data = data.matrix(train[, feature.names]), label = train$Crop_Damage)
watchlist <- list(train = dtrain)

clf <- xgb.train(params = param,
                 data = dtrain,
                 nround = 900,
                 print.every.n = 20,
                 watchlist = watchlist)

tpreds <- predict(clf, data.matrix(test[, feature.names]))
tmat <- get_proba(tpreds, Id = test$ID)
write_csv(tmat, "./XGB_Commit1_test.csv")
s_test <- get_predict(tmat, isTest = TRUE)
write_csv(s_test, "./XGB_COMMIT_1.csv")                   # public LB: 84.9%

##################################################################################
########################## POST PROCESS MODEL OUTPUTS ############################
##################################################################################

train <- read_csv("train.csv")
train$predicted <- NA
test <- read_csv("test.csv")
pred <- read_csv("./XGB_COMMIT_1.csv")
names(pred)[2] <- "predicted"
test$Crop_Damage <- NA
test <- merge(test, pred)

md <- rbind(train, test)
md <- md[order(md$ID),]

############### intermittent values correction ###############
output <- data.frame(ID = character(), Estimated_Insects_Count = numeric(), Soil_Type = numeric(), Number_Doses_Week = numeric(),
                     Crop_Damage = numeric(), predicted = numeric(), block_length = numeric())

i = 1

while(i < nrow(md)) {
  
  rm(block, block1, block2, block3, block4)
  cat("\nBlock starting at row: ", i)
  ret <- findBlock(i)
  block <- ret$block
  
  if(nrow(block) < 3) {
    
    cat("\n Block length: ", nrow(block), "..... SKIPPING!")
    df <- block[, c("ID", "Estimated_Insects_Count", "Soil_Type", "Number_Doses_Week","Crop_Damage", "predicted")]
    df$block_length <- rep(nrow(block), nrow(block))
    output <- rbind(output, df)
    
  }else{
    # find the sub-blocks and mini-blocks inside a block
    # with varying Crop_Type and Soil_Type
    ct <- length(unique(block$Crop_Type))
    
  
    if(ct == 1){
      
      block1 <- block[block$Soil_Type == 0,]
      block2 <- block[block$Soil_Type == 1,]
      if(nrow(block1) > 0){
        
        output <- rbind(output, treat_block(block1))
      }
      
      if(nrow(block2) > 0){
        
        output <- rbind(output, treat_block(block2))
        
      }
      
      
    }
    else{
      
      block1 <- block[block$Crop_Type == 0 & block$Soil_Type == 0,]
      block2 <- block[block$Crop_Type == 0 & block$Soil_Type == 1,]
      block3 <- block[block$Crop_Type == 1 & block$Soil_Type == 0,]
      block4 <- block[block$Crop_Type == 1 & block$Soil_Type == 1,]
      
      if(nrow(block1) > 0){
        
        output <- rbind(output, treat_block(block1))
      }
      
      if(nrow(block2) > 0){
        
        output <- rbind(output, treat_block(block2))
      }
      
      if(nrow(block3) > 0){
        
        output <- rbind(output, treat_block(block3))
      }
      
      if(nrow(block4) > 0){
        
        output <- rbind(output, treat_block(block4))
      }
      
    }
  }
  
  i <- ret$ns
  
}

tid <- test$ID
test_corrected_v1 <- output[output$ID %in% tid,]
setdiff(tid, test_corrected_v1$ID)
a <- md[md$ID == "F00155944",][c("ID", "Estimated_Insects_Count", "Soil_Type", "Number_Doses_Week","Crop_Damage", "predicted")]
a$block_length <- 1
test_corrected_v1 <- rbind(test_corrected_v1, a)

write_csv(output, "./output94p.csv")
write_csv(test_corrected_v1, "./test_corrected_v1.csv")

############## the above code alone gave me 94.2% on the LB ##########

# Next part is to correct the end of block entries where 
# The blocks end with a test entry and the last train 
# entry in the block has Crop_Damage = 0
########## find the blocks ending with NAs ########
i <- 1
count <- 0
idx <- c()
while(i < nrow(md)) {
  
  ret <- findBlock(i)
  bl <- ret$block
  
  k <- 0
  if(length(unique(bl$Soil_Type)) == 2){
    
    b1 <- bl[bl$Soil_Type == 0,]
    k <- nrow(b1)
    
  }
  
  na_check = FALSE
  if(k != 0){
    
    na_check <- is.na(bl$Crop_Damage[nrow(bl)]) | is.na(bl$Crop_Damage[k])
  }else{
    
    na_check <- is.na(bl$Crop_Damage[nrow(bl)])
  }
  
  if(nrow(bl) >= 3 & na_check){
    
    count <- count + 1
    idx <- c(idx, i)
    cat("\n Match found at row:", i)
  } 
  
  i <- ret$ns
}

################## correct the takeLast function falacies of handing NAs in the end of time blocks ################
o2 <- data.frame(ID = character(), Estimated_Insects_Count = numeric(), Soil_Type = numeric(), Number_Doses_Week = numeric(),Crop_Damage = numeric(), 
                 predicted = numeric(), block_length = numeric(), corrected = numeric())


tmp94 <- read_csv("output94p.csv")
ss <- tmp94
dd <- rbind(train[c("ID", "Crop_Type", "Crop_Damage")], test[, c("ID", "Crop_Type", "Crop_Damage")])
names(dd)[3] <- "CD"
ss <- merge(ss, dd)

for(index in 1:length(idx)){
  
  cat("\nTreating block starting at row: ", idx[index])
  
  err_block <- findBlock2(idx[index])
  ct <- length(unique(err_block$Crop_Type))
  if(ct == 1){
    
    block1 <- err_block[err_block$Soil_Type == 0,]
    block2 <- err_block[err_block$Soil_Type == 1,]
    
    if(nrow(block1) > 0){
      
      o2 <- rbind(o2, end_of_block_correction(block1))
    }
    
    if(nrow(block2) > 0){
      
      o2 <- rbind(o2, end_of_block_correction(block2))
    }
    
    
  }else {
    
    block1 <- err_block[err_block$Crop_Type == 0 & err_block$Soil_Type == 0,]
    block2 <- err_block[err_block$Crop_Type == 0 & err_block$Soil_Type == 1,]
    block3 <- err_block[err_block$Crop_Type == 1 & err_block$Soil_Type == 0,]
    block4 <- err_block[err_block$Crop_Type == 1 & err_block$Soil_Type == 1,]
    
    if(nrow(block1) > 0){
      
      o2 <- rbind(o2, end_of_block_correction(block1))
    }
    
    if(nrow(block2) > 0){
      
      o2 <- rbind(o2, end_of_block_correction(block2))
    }
    
    if(nrow(block3) > 0){
      
      o2 <- rbind(o2, end_of_block_correction(block3))
    }
    if(nrow(block4) > 0){
      
      o2 <- rbind(o2, end_of_block_correction(block4))
    }
    
  }
  
}


tc94 <- read_csv("./test_corrected_v1.csv")
tc94 <- merge(tc94, o2[, c("ID", "corrected")], all.x = TRUE)
tc94$revised <- ifelse(is.na(tc94$corrected), tc94$Crop_Damage, tc94$corrected)

tc94$final <- ifelse(is.na(tc94$revised), tc94$predicted,
                     ifelse(tc94$revised > 0 & tc94$revised < 1, 0,
                            ifelse(tc94$revised > 1 & tc94$revised < 2, 1, tc94$revised)))


###### save the submissions #########

sub <- tc94[, c("ID", "final")]
names(sub)[2] <- "Crop_Damage"
write_csv(sub, "./XGB_Commit1_modified_version14.csv") # final submission - public LB: 96.4%, private LB: 96.09%
