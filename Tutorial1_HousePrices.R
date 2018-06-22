#load necessary packages
library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(ggrepel)
library(gridExtra)
library(scales)
library(Rmisc)
library(randomForest)
library(psych)
library(xgboost)


library(caret)


##########################################################

train <- read.csv("Y:/RStudio/House Prices/train(1).csv", stringsAsFactors = F) #read in as character strings as most variables require FE
test <- read.csv("Y:/RStudio/House Prices/test(1).csv", stringsAsFactors = F)

#3.2 Data size and structure
dim(train)
str(train[,c(1:10, 81)])  #display first 10 variables and the response variable

#getting rid of IDs but keeping the test IDs in a vector. These are needed for submission.
test_labels <- test$Id
test$Id <- NULL
train$Id <- NULL


test$SalePrice <- NA    #add in sale price to test dataset
all <- rbind(train, test)    #combine both datasets
dim(all)

#4 Exploring some of the most important variables
#ggplot to explore some of the most important variables: the response variable sale price
ggplot(data = all[!is.na(all$SalePrice),], aes(x=SalePrice)) + 
        geom_histogram(fill = "blue", binwidth = 10000) +
        scale_x_continuous(breaks = seq(0, 800000, by = 100000), labels = comma)
summary(all$SalePrice)

#4.2 The most important numeric predictors
#the most important numeric predictors: which numeric variables have high correlation with sale price
numericVars <- which(sapply(all, is.numeric))    #index vector numeric variables
numericVarNames <- names(numericVars)     #saving names vector for later use
cat('There are', length(numericVars), 'numeric variables')
##There are 37 numeric variables

all_numVar <- all[, numericVars]
cor_numVars <- cor(all_numVar, use = "pairwise.complete.obs")    #correlations of all numeric variables

#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVars[,'SalePrice'], decreasing = TRUE))
#select only high correlations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
?apply
?abs
cor_numVars <- cor_numVars[CorHigh, CorHigh]

corrplot.mixed(cor_numVars, tl.col="black", tl.pos = "lt")
?corrplot.mixed

#overall quality appears to have the best correlation with sale price
ggplot(data = all[!is.na(all$SalePrice),], aes(x=factor(OverallQual), y=SalePrice)) +
  geom_boxplot(col = "blue") + labs(x='Overall Quality') + 
  scale_y_continuous(breaks = seq(0, 800000, by=100000), labels = comma)

#now check 2nd highest correlation: above grade living area
ggplot(data = all[!is.na(all$SalePrice),], aes(x=GrLivArea, y=SalePrice)) + 
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks = seq(0, 800000, by=100000), labels = comma) +
  geom_text_repel(aes(label = ifelse(all$GrLivArea[!is.na(all$SalePrice)] > 4500, rownames(all),''))     #FIND OUTLIER
)
#more detail on the two outliers
all[c(524,1299), c('SalePrice','GrLivArea','OverallQual')]

#5 Missing data, label encoding, and factorizing variables
#5.1 Completeness of the data
NaCol <- which(colSums(is.na(all)) > 0) #identifies all cols with >0 missing values
sort(colSums(sapply(all[NaCol], is.na)), decreasing = TRUE) #summary of all missing value

cat('There are', length(NaCol), 'columns with missing values')

#5.2 Inputting missing data - fixing the 34 predictors that have missing data - just look at garage and basement sections
#replace all 159 missing GarageYrBlt: Year garage was built values with the values in YearBuilt (this is similar to YearRemodAdd, 
#which also defaults to YearBuilt if no remodeling or additions)
 
all$GarageYrBlt[is.na(all$GarageYrBlt)] <- all$YearBuilt[is.na(all$GarageYrBlt)]
#check if all 157 NAs are the same observations among the variables with 157/159 NAs
length(which(is.na(all$GarageType) & is.na(all$GarageFinish) & is.na(all$GarageCond) & is.na(all$GarageQual)))
#find two additional NAs
?kable
kable(all[!is.na(all$GarageType) & is.na(all$GarageFinish), c('GarageCars', 'GarageArea', 'GarageType', 'GarageCond', 'GarageQual', 'GarageFinish')])
#fix house 2127 as it does seem to have a garage
#Therefore, there should be 158 houses without a Garage. To fix house 2127, 
#imputate the most common values (modes) for GarageCond, GarageQual, and GarageFinish.
all$GarageCond[2127] <- names(sort(-table(all$GarageCond)))[1]
all$GarageQual[2127] <- names(sort(-table(all$GarageQual)))[1]
all$GarageFinish[2127] <- names(sort(-table(all$GarageFinish)))[1]

#display "fixed" house
kable(all[2127, c('GarageYrBlt','GarageCars', 'GarageArea', 'GarageType', 'GarageCond', 'GarageQual', 'GarageFinish')])

#garagecars and garagearea
#fixing 3 values for house 2577
all$GarageCars[2577] <- 0
all$GarageArea[2577] <- 0
all$GarageType[2577] <- NA

#check if NAs of the character variables are now all 158
length(which(is.na(all$GarageType) & is.na(all$GarageCond) & is.na(all$GarageFinish) & is.na(all$GarageQual)))

#Now, the 4 character variables related to garage all have the same set of 158 NAs, which correspond to 'No Garage'.
#I will fix all of them in the remainder of this section

#Garage Location
