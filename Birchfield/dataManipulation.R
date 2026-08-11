library(tidyverse)
library(lubridate)

baseDirectory <- 'D:/Documents/_UCLA/Holbrook/CrossLong/'
dataDirectory <- paste0(baseDirectory, 'Data/')
BirchfieldDirectory <- paste0(baseDirectory, 'Birchfield/')
setwd(BirchfieldDirectory)

corticalThicknessPipelineNames <- c(
  'FSCross',
  'FSLong',
  'ANTsCross',
  'ANTsNative',
  'ANTsSST',
  'ANTsXNetCross',
  'ANTsXNetLong'
)

corticalThicknessCsvs <- list() 
corticalThicknessData <- list() 

for(k in 1:length(corticalThicknessPipelineNames)){ 
  corticalThicknessCsvs[[k]] <- paste0(dataDirectory, 
                                       'reconciled_', 
                                       corticalThicknessPipelineNames[k], 
                                       '.csv')
}
rm(k)

for(k in 1:length(corticalThicknessPipelineNames)){
  
  corticalThicknessData[[k]] <- 
    read.csv(corticalThicknessCsvs[[k]]) %>%
    select(c(1:8, 12, 43)) %>%
    mutate(
      ID = as.numeric(as.factor(ID)),
      THICKNESS_SUM = thickness.left.entorhinal + thickness.right.entorhinal,
      MCI = ifelse(DIAGNOSIS=="LMCI", 1, 0), 
      AD = ifelse(DIAGNOSIS=="AD", 1, 0),
      MALE = ifelse(SEX=="M", 1, 0)
    )
  
  corticalThicknessData[[k]]$EXAM_DATE <- as_date(corticalThicknessData[[k]]$EXAM_DATE)
  
  corticalThicknessData[[k]] <- corticalThicknessData[[k]] %>%
    group_by(ID) %>% 
    arrange(ID, EXAM_DATE) %>% 
    mutate(
      VISIT_NUMBER = row_number(),
      FIRST_VISIT = min(EXAM_DATE),
      INITIAL_AGE = min(AGE),
      DAYS = as.numeric(EXAM_DATE - FIRST_VISIT),
      YEARS = DAYS/365
      ) %>%
    ungroup() %>%
  
    select(-thickness.left.entorhinal, 
           -thickness.right.entorhinal, 
           -IMAGE_ID, 
           -VISIT, 
           -EXAM_DATE, 
           -DIAGNOSIS,
           -FIRST_VISIT,
           -AGE,
           -SEX) %>%
    rename(MM_SCORE = MMSCORE) %>%
    relocate(ID, 
             VISIT_NUMBER,
             DAYS,
             YEARS,
             MCI,
             AD,
             INITIAL_AGE,
             MALE,
             MM_SCORE,
             THICKNESS_SUM)
  
}
rm(k)

saveRDS(corticalThicknessData, "corticalThicknessData.rds")