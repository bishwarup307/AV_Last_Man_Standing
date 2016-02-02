# reshape the xgboost probabilities for multi:softprob
get_proba <- function(preds, Id = NULL, num_class = 3){
  
  p <- data.frame(t(matrix(preds, nrow = num_class, ncol = length(preds)/num_class)))
  cnames <- c("0", "1", "2")
  names(p) <- cnames
  
  if(is.null(Id)){
    
    return(p)
    
  } else {
    
    p <- cbind(id = Id, p)
    return(p)
    
  }
  
}

# hardcoded class value
get_predict <- function(predMatrix, isTest = FALSE) {
  
  if(ncol(predMatrix) > 3) {
    
    k <- apply(predMatrix[, -1], 1, which.max) - 1
    if(isTest) {
      df <- cbind.data.frame(ID = predMatrix[, 1], Crop_Damage = k)
    } else {
      
      df <- cbind.data.frame(ID = predMatrix[, 1], predicted_class = k)
    }
    return(df)
  }
  else {
    
    k <- apply(predMatrix[, -1], which.max) - 1
    return(k)
    
  }
  
}

# find time blocks of similar Estimated_Insects_Counts
# returns the relevant block and start point of the next block
# assumption: max block length = 100

findBlock <- function(i) {
  
  d <- c()
  for (j in 1:100) {
    
    k <- abs(md$Estimated_Insects_Count[i] - md$Estimated_Insects_Count[i + j])
    d <- c(d, k)
  }
  
  ct <- min(which(d > 1))
  st <- i
  en <- i + ct - 1
  ak <- md[i:en,]
  
  new_start <- i + ct
  
  p <- list(block = ak, ns = new_start)
  return(p)
}

# same function just returning the block this time
# i wrote it for achieving something else
# could be ignored
findBlock2 <- function(i) {
  
  d <- c()
  for (j in 1:100) {
    
    k <- abs(ss$Estimated_Insects_Count[i] - ss$Estimated_Insects_Count[i + j])
    d <- c(d, k)
  }
  
  ct <- min(which(d > 1))
  st <- i
  en <- i + ct - 1
  ak <- ss[i:en,]
  
  return(ak)
}

# find the test entries inside a block
findTest <- function(df) {
  
  non_na <- which(is.na(df$Crop_Damage))
  
  return(non_na)
  
}

# find the train entries inside the block
findTrain <- function(df) {
  
  non_na <- which(!is.na(df$Crop_Damage))
  
  return(non_na)
  
}

# in a block search for the previous and next non NA Crop_Damage
find_non_na <- function(df, i){
  
  if(i == 1){
    
    return(-1)
    
  }else{
    
    for(j in 1:(i-1)){
      
      if(!is.na(df$Crop_Damage[i - j])){
        
        p1 <- i - j
        break
        
      }
      
    }
    for(j in 1:(nrow(df) - i)){
      
      if(!is.na(df$Crop_Damage[i + j])){
        
        p2 <- i + j
        break
      }
      
    }
    
    p <- list(last = p1, nxt = p2)
    return(p)
  }
  
}

# if a blocks starts with NA i.e. if the starting point
# of a block is a test entry just impute with 0
# this is just an assumption on my part keeping an eye on
# the pattern present in the data

takeFirst <- function(block) {
  
  if(is.na(block$Crop_Damage[1])){
    
    block$Crop_Damage[1] <- 0
  }
  return(block)
}

# if a block ends with NA i.e. if the ending point of a
# block is a test entry just impute with the last present
# Crop_Damage value in the block 
# This is a more vulnerable assumption but strong enough
# to lift the score above 90%
# correction will be applied to this assumption going ahead

takeLast <- function(block) {
  
  if(is.na(block$Crop_Damage[nrow(block)])){
    
    last_present <- max(findTrain(block))
    block$Crop_Damage[nrow(block)] <- block$Crop_Damage[last_present]
    
  }
  return(block)
}

# impute the intermittent test entries in a block with the mean of
# previous and next train entries

impute_intermittent <- function(block, index){
  
  p <- find_prev_non_na(block, index)
  block$Crop_Damage[index] <- (block$Crop_Damage[p$last] + block$Crop_Damage[p$nxt])/2
  return(block)
}


# this function rectifies any error made due to the assumption of the takeLast
# function by observing the pattern in Numnber_Doses_Week values in the block
# this asks for manual user input interactively in the run time. I 
# put it in that way to verify my assumption of the pattern of Number_Doses_Week
# values in the data. This is automated once the pattern is confirmed.

end_of_block_correction <- function(block) {
  
  if(nrow(block) == 1){
    
    block$corrected <- block$Crop_Damage
    
  }else{
    
    cd <- unique(block$CD[!is.na(block$CD)])
    if(length(cd) == 1 & is.na(block$CD[nrow(block)])){
      
      if(cd == 0){
        
        k <- block$Number_Doses_Week
        k_diff <- diff(k)
        is_reverse <- sum(k_diff < 0)
        if(is_reverse == 0){
          
          block$corrected <- block$Crop_Damage
        }else{
          
          pt <- min(which(k_diff < 0))
          
          if(nrow(block) >= 5 | pt == 1){
            
            cat("\n----------------------------")
            cat("\nBlock starting ID: ", block$ID[1])
            cat("\nBlock length: ", nrow(block))
            cat("\nBlock Number_Doeses_Week: ", block$Number_Doses_Week)
            cat("\nReverse indices: ", which(k_diff < 0))
            
            n <- readline(prompt="\nEnter the index you want to use: ")
            n <- as.integer(n)
            if(n == -1){
              
              block$corrected <- block$Crop_Damage
            }else{
              block$corrected <- block$Crop_Damage
              block$corrected[(n+1):nrow(block)] <- 1  
              
            }
            
          }else {
            
            block$corrected <- block$Crop_Damage
            block$corrected[(pt+1):nrow(block)] <- 1
          }
        }
      } else {
        
        block$corrected <- block$Crop_Damage
      }
    } else {
      
      block$corrected <- block$Crop_Damage
      
    }
  }
  
  return(block)
}

# the automated version of the above function
end_of_block_correction_automated <- function(block) {
  
  if(nrow(block) == 1){
    
    block$corrected <- block$Crop_Damage
    
  }else{
    
    cd <- unique(block$CD[!is.na(block$CD)])
    if(length(cd) == 1 & is.na(block$CD[nrow(block)])){
      
      if(cd == 0){
        
        k <- block$Number_Doses_Week
        k_diff <- diff(k)
        is_reverse <- sum(k_diff < 0)
        if(is_reverse == 0){
          
          block$corrected <- block$Crop_Damage
        }else{
          
          L = nrow(block)
          reverse_indices <- which(k_diff < 0)
          if(L == 2 & min(reverse_indices) == 1){
            
            block$corrected <- block$Crop_Damage
            block$corrected[2] <- 1
            
          }else{
            
            HL <- L/2
            r1 <- reverse_indices[reverse_indices < HL]
            r2 <- reverse_indices[reverse_indices >= HL]
            if(length(r2) == 0){
              
              block$corrected <- block$Crop_Damage
            }else {
              
              cpt <- min(r2)
              block$corrected <- block$Crop_Damage
              block$corrected[(cpt+1):nrow(block)] <- 1
              
            }
            
          }
          
        }
      } else {
        
        block$corrected <- block$Crop_Damage
      }
    } else {
      
      block$corrected <- block$Crop_Damage
      
    }
  }
  
  return(block)
}

# checks if there is any non-integer values between 0 and 1 from the
# impute_intermittent function present in the block
btwn_zero_one <- function(block){
  
  entries <- sum(block$Crop_Damage > 0 & block$Crop_Damage < 1, na.rm = TRUE)
  return(entries)
  
}

# if there are values in the block where the previous present train entry is 0
# and the next train entry is 1, observe the Number_Doses_Week pattern and
# make adjustments to the values like 0.5, 0.75 to make them either 0 or 1
adjust_zero_ones <- function(block){
  
  dp <- which(block$Crop_Damage > 0 & block$Crop_Damage < 1)
  p1 <- min(dp) - 1
  p2 <- max(dp) + 1
  df <- block[p1:p2, ]
  nd <- df$Number_Doses_Week
  diff_nd <- diff(nd)
  
  if(sum(diff_nd < 0) == 0){
    
    return(block)
  } else{
    ind <- min(which(diff_nd < 0))
    df$Crop_Damage[1: ind] <- 0
    df$Crop_Damage[(ind + 1):nrow(df)] <- 1
    dfID <- df$ID
    block <- block[!block$ID %in% dfID,]
    block <- rbind(block, df)
    block <- block[order(block$ID),]
    return(block)
  }
}

# apply the imputation algorithm to any block of size > 2
treat_block <- function(bl){
  
  if(nrow(bl) < 3){
    
    cat("\n block length: ", nrow(bl), "..... SKIPPING!")
    if(nrow(bl) > 0) {
      
      df <- bl[, c("ID", "Estimated_Insects_Count", "Soil_Type", "Number_Doses_Week","Crop_Damage", "predicted")]
      df$block_length <- rep(nrow(bl), nrow(bl))
      
    }
    
  }else {
    
    bl <- takeFirst(bl)
    bl <- takeLast(bl)
    to_predict <- findTest(bl)
    
    if(length(to_predict) != 0){
      for(index in to_predict){
        
        bl <- impute_intermittent(bl, index)
        
      }
      
      check_confusion <- btwn_zero_one(bl)
      if(check_confusion > 0){
        
        bl <- adjust_zero_ones(bl)
        
      }
      
      df <- bl[, c("ID", "Estimated_Insects_Count", "Soil_Type", "Number_Doses_Week","Crop_Damage", "predicted")]
      df$block_length <- rep(nrow(bl), nrow(bl))
      
    }else{
      
      df <- bl[, c("ID", "Estimated_Insects_Count", "Soil_Type", "Number_Doses_Week","Crop_Damage", "predicted")]
      df$block_length <- rep(nrow(bl), nrow(bl))
      
    }
  }
  
  return(df)
}

