
# data wrangling
library(tidyverse)
library(forcats)
library(stringr)
library(caTools)

# data assessment/visualizations
library(DT)
library(data.table)
library(pander)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(corrplot)
library(VIM) 
library(knitr)
library(vcd)
library(caret)


# model
library(xgboost)
library(MLmetrics)
library('randomForest') 
library('rpart')
library('rpart.plot')
library('car')
library('e1071')
library(vcd)
library(ROCR)
library(pROC)
library(VIM)
library(glmnet) 

#read in the data
train <- read.csv('C:/Users/SeaBur/Documents/GitProjects/Titanic_Head Start/Project1/train.csv',stringsAsFactors = T)
test <- read.csv('C:/Users/SeaBur/Documents/GitProjects/Titanic_Head Start/Project1/test.csv',stringsAsFactors = T)



# create ID columns for test and train flags
train$set <- "train"
test$set <- "test"

test$Survived <- NA

#combine/union data sets
full <- rbind(train,test)

#look at the data
str(full)

#data dims
dim(full)

#unique values per column
lapply(full, function(x) length(unique(x)))


missing_values <- full %>% summarize_all(funs(sum(is.na(.))/n()))
missing_values

missing_values <- gather(missing_values,key="feature", value="missing_pct")

missing_values %>% 
  ggplot(aes(x=reorder(feature,missing_pct),y=missing_pct)) +
  geom_bar(stat="identity",fill="green")+
  coord_flip()+theme_bw() 



checkColumn = function(df,colname) {
                                      testData = df[[colname]]
                                        numMissing = max(sum(is.na(testdata)|is.nan(testData)|testData==''),0)
  
                                      if (class(testData)=='numeric' | 
                                          class(testData)=='Date' | 
                                          class(testData)=='difftime' | 
                                          class(testData)== 'integer')
                                        {
                                            list(
                                                  'col' = colname,
                                                   'class'=class(testData),
                                                   'num'=length(testData)-numMissing,
                                                   'numMissing' = numMissing,
                                                   'numInfinite' = sum(is.infinite(testData)),
                                                   'avgVal'= mean(testData,na.rm=TRUE),
                                                   'minVal'=round(min(testData,na.rm=TRUE)),
                                                   'maxVal'=round(max(testData,na.rm=TRUE)))
                                      }
                                        else
                                        {
                                          list('col'= colname,
                                               'class'= class(testData),
                                               'num'= length(testData) - numMissing,
                                               'numMissing'=numMissing,
                                               'numInfinite'= NA,
                                               'avgVal'= NA,
                                               'minVal'= NA,
                                               'maxVal' = NA)
                                          
                                        }
                                            }
                                                  
                                          checkAllCols = function(df) {
                                            resDF = data.frame()
                                              for (colName in names(df)) {
                                                resDF = rbind(resDF,as.data.frame(checkColumn(df=df,colname=colName)))
                                              }
  resDF
}

                                          
datatable(checkAllCols(full),style = "bootstrap",class = "table-condensed", options = list(dom='tp',scrollx = TRUE))                                          


#testing git for shits and giggles




