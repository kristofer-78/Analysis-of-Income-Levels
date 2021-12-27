# packages being used --------------------------------------------------------
set.seed(12345)
library(lessR)
library(caret)
library(plyr)
library(rattle)
library(ggplot2)
library(psych)
library(RANN)
library(gridExtra) # this has a package that allows us to plot multiple ggplots
# onto one page like par(mfrow)


# Import Data set ------------------------------------------------------------

proj <- read.csv('C:/Users/kris2/Downloads/data 511/proj')

proj <- proj[,-2] # delete variable occupation

summary(proj) # looks like occupation was deleted from the data set

# imputing values for capital gains -------------------------------------------

describe(proj$capital.gain, na.rm = TRUE) 


proj$capital.gain[proj$capital.gain == 99999] <- NA 
# this told r to set all subsets of capital gains equal to 99999 to be NA

summary(is.na(proj$capital.gain))  
# we see 159 values are labeled NA now


# we need mean and sd for de-standardizing
cgm <- mean(proj$capital.gain, na.rm = TRUE) # assigning mean to cgm
cgsd <- sd(proj$capital.gain, na.rm = TRUE) # assigning sd to cgsd


# we will now replace the missing values 
# using preProcess from the caret package

imputation_model <- preProcess(proj, method = c('knnImpute'))
proj.imp <- predict(imputation_model, proj)

head(proj.imp) # we see that we need to now de-standardize our values

proj$cg.imp <- round(proj.imp$capital.gain * cgsd + cgm, 5)


proj$capital.gain <-
  ifelse(
    test = is.na(proj$capital.gain) == TRUE,
    yes = proj.imp$capital.gain
    * cgsd + cgm,
    no = proj$capital.gain)


summary(is.na(proj$capital.gain)) # no more NA's after imputting

proj$cg.imp <- NULL
# I like to remove the cg.imp from the data set since capital gains is now equal
# to cg.imp so why have both in the data set? you can even see this is true
# by doing the following:
# isTRUE(proj3$capital.gain == proj3$cg.imp)

summary(proj)


# reclassifying education -----------------------------------------------------

proj$educ <- revalue(proj$education,
                      c("Preschool"="low",
                        "1st-4th"="low",
                        "5th-6th"="low",
                        "7th-8th"="low",
                        "9th"="low",
                        "HS-grad"="low",
                        "10th"='low', 
                        "11th"='low',
                        '12th'='low',
                        "Some-college"="high",
                        'Assoc-acdm' = 'high',
                        'Assoc-voc'='high',
                        'Bachelors'='high',
                        'Masters'='high',
                        'Doctorate'='high',
                        'Prof-school'='high'))

proj$education <- NULL # removing the education variable

# lets factorize educ 

proj$educ <- as.factor(proj$educ)

summary(proj)

# looking at income & educ-----------------------------------------------------

# lets factorize the income variable

proj$income <- as.factor(proj$income)

summary(proj$income)

7841/(24720+7841)

# it appears that 7841/(24720+7841) or 24.08 % of the individuals in the data
# set are in the high income bracket

t1 <- table(proj$income, proj$educ)
t1
round(prop.table(t1,2)*100,2)


# it appears that people with a higher education have a 3-fold increase in the
# likely-hood of having a high income as compared to people with a low 
# education level. (Shocking!! :0 )

# reclassifying relationship --------------------------------------------------

proj$relationship <- as.factor(proj$relationship)
summary(proj$relationship)
proj$rel <- revalue(proj$relationship,
                     c("Husband"="HusWife",
                       "Wife"="HusWife",
                       "Unmarried"="Other",
                       "Own-child"="Other",
                       "Not-in-family"="Other",
                       "Other-relative"='Other'))
summary(proj$rel)
proj$relationship <- NULL # removing the relationship variable


# looking at rel and income ---------------------------------------------------

t2 <- table(proj$income,proj$rel)
t2
round(prop.table(t2,2)*100,2)

# it appears that people who are married have almost a 7-fold increase in the
# likely-hood of having a high income as compared to people who
# are not married. (EVEN MORE Shocking!!!! :0 )

# partition data set into a train and test data set (50%) ---------------------


inTrain <- createDataPartition(y = proj$income, p=.50,list = FALSE)
# this partitioned our data set into a train data set and a test data set

proj.tr <- proj[inTrain,] # train

proj.te <- proj[-inTrain,] # test

summary(proj.tr$income)
summary(proj.te$income)

# looks like the train and test data set have equal amounts of data partitioned

# validating the partition for capital gains ----------------------------------
proj.tr$part <- rep('train', nrow(proj.tr))
proj.te$part <- rep('test', nrow(proj.te))

proj.all <- rbind(proj.tr, proj.te)


grid.arrange(
  
  ggplot(data = proj.all, aes(x = as.factor(part), y =capital.gain, 
                               color = as.factor(part))) +
    geom_boxplot(aes()) +
    xlab("Partitons") +
    ylab("Capital Gain") +
    labs(title = 'Boxplot of Capital Gain by partitions') +
    scale_color_brewer(palette="Dark2") +
    geom_jitter(shape=16, position=position_jitter(0.2))
  
  ,
  
  ggplot(data = proj.all, aes(x = as.factor(part), y =capital.loss, 
                               color = as.factor(part))) +
    geom_boxplot(aes()) +
    xlab("Partitons") +
    ylab("Capital Loss") +
    labs(title = 'Boxplot of Capital Loss by partitions') +
    scale_color_brewer(palette="Dark2") +
    geom_jitter(shape=16, position=position_jitter(0.2))
  
  ,nrow=1, ncol = 2)
# just by eyeballing it, it appears that the distributions are very similar 
# in the train & test for both capital gain / loss

kruskal.test(capital.gain ~ as.factor(part), data = proj.all)
kruskal.test(capital.loss ~ as.factor(part), data = proj.all)
# we use kruskal.test for non categorical variables when wanting to test 
# whether the medians are different between two groups
# by the p-values they look similar


# validating partition for educ and rel ---------------------------------------

# we will use the chi.square test or the prop.test for categorical variables to
# test differences in proportions between two groups


grid.arrange(
  
  ggplot(data = proj.all, aes(x = as.factor(part), y =educ, 
                               color = as.factor(part))) +
    geom_boxplot(aes()) +
    xlab("Partitions") +
    ylab("Education") +
    labs(title = 'Boxplot of Education by partitions') +
    scale_color_brewer(palette="Dark2") +
    geom_jitter(shape=16, position=position_jitter(0.2))
  
  ,
  
  ggplot(data = proj.all, aes(x = as.factor(part), y =rel, 
                               color = as.factor(part))) +
    geom_boxplot(aes()) +
    xlab("Partitons") +
    ylab("Relationships") +
    labs(title = 'Boxplot of Relationships by partitions') +
    scale_color_brewer(palette="Dark2") +
    geom_jitter(shape=16, position=position_jitter(0.2))
  
  ,nrow=1, ncol = 2)



summary(proj.tr$educ)
summary(proj.te$educ)

pt1 <- matrix(c(8835,8972,7446,7308), nrow = 2)

colnames(pt1) <- c('High','Low')
rownames(pt1) <- c('Train','Test')

pt1

# chisq.test(pt1, correct = FALSE)
prop.test(pt1, correct = FALSE) # same as chi.test

summary(proj.tr$rel)
summary(proj.te$rel)

pt2 <- matrix(c(7378,7383,8903,8897), nrow = 2)

colnames(pt2) <- c('HusWife','Other')
rownames(pt2) <- c('Train','Test')

pt2

# chisq.test(pt2, correct = FALSE)  # same test as prop.test
prop.test(pt2, correct = FALSE)


# removing the training and test variables (part) ----------------------------

# we no longer need this in our data analysis

proj.te$part <- NULL
proj.tr$part <- NULL


# Establishing the baseline performance --------------------------------------

summary(proj.tr$income)

# all greater than 50k model;

t3 <- matrix(c(0,0,0,12360,3921,12360+3921,12360,3921,12360+3921),nrow = 3)
colnames(t3) <- c('<= 50k','>50k','total')
rownames(t3) <- c('<= 50k','>50k','total')
t3

# all less than 50k model;

t4 <- matrix(c(12360,3921,12360+3921,0,0,0,12360,3921,12360+3921),nrow = 3)
colnames(t4) <- c('<= 50k','>50k','total')
rownames(t4) <- c('<= 50k','>50k','total')
t4


(3921/16281) # model 1
(12360/16281) # model 2

# we will use the second model as our baseline model and have to beat a 76.92% 



# cleaning up data set ------------------------------------------------------

summary(proj.te)

# gotta factorize!!
proj.te$income <- as.factor(proj.te$income)
proj.te$educ <- as.factor(proj.te$educ)
proj.te$rel <- as.factor(proj.te$rel)
proj.tr$income <- as.factor(proj.tr$income)
proj.tr$educ <- as.factor(proj.tr$educ)
proj.tr$rel <- as.factor(proj.tr$rel)



# 10 fold cart model & decision tree (method = rpart2)------------


# because we are already provided a train and test data set we do not need to
# partition our data set so we will now perform the 10-fold cross validation

TC <- trainControl(
  method = "cv",
  number = 10)


fit <- train(income ~ ., 
             data = proj.tr,
             method = "rpart2",
             trControl = TC)

fancyRpartPlot(fit$finalModel)
# looks like 76% of the individuals in the training data set have income below
# 50K
# relOther >=.5 means this is the non married category thus

# those that are not married 6% have income greater than 50k while those that
# are married have 45% in high income

# those not married with capital gains < 7074, 5% have high income while those 
# with capital gains >= 7074 2% have high income

# those that are married and have high education 
# 40% of these individuals have high income, with a support of 25%!

# checking for over fitting ---------------------------------------

fit$resample
max(fit$resample[,1]) - min(fit$resample[,1])


# looks like all folds are within 2% of each others accuracy, so no overfitting!

# apply the model to the test data set ----------------------------

testsetpreds <- predict(fit, proj.te)

table(proj.te$income,testsetpreds)

(10630+2727)/(10630+1193+1730+2727)
# the accuracy is 82.05% which is more than the baseline at 75.92%!!!



