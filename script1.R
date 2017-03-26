#install.packages("plsdepot")
#install.packages("pls")
#install.packages("ggpairs")
library(reshape2)
library(Metrics)
load("C:/Users/Kuntal/Documents/GitHub/ahrf2016r/data/ahrf_county_train.rda")
load("C:/Users/Kuntal/Documents/GitHub/ahrf2016r/data/ahrf_county_layout.rda")

dataset <- ahrf_county_train

get_set <- function (code,fieldname){
  code_regexp <- paste(code,'..',sep='')
  selected <- subset(dataset, select = grep(code_regexp, names(dataset)))
  selected <- data.frame(county=dataset$F04437,selected)
  numcols <- as.numeric(dim(selected)[2])
  if (numcols < 4){
    #print(fieldname)
    #print ("Estimating 2013 data from 2010 data")
    newcol <- paste(code,'13',sep='')
    maxcol <- paste(code,'10',sep='')
    selected[newcol] <- selected[maxcol]
  }
  projected <- melt(selected, id.vars=c("county"))
  projected$year <- paste('20',substr(projected$variable,7,9),sep="")
  projected$value <- as.numeric(projected$value)
  colnames(projected)[3] <- fieldname
  projected <- projected[,-2]
  projected <- projected[order(projected$county,projected$year),]
}

#code <- "F09571"
#fieldname <- "fld"
#inpatients <- get_set(code,fieldname)
#summary(inpatients)

############ UTILIZATION #################

utilization_columns <- read.csv("H:/datathon/utilization_columns.csv")

utilization <- data.frame(get_set(trimws(toString(utilization_columns[1,1]))
                                  ,trimws(toString(utilization_columns[1,2]))))

for (i in 2:nrow(utilization_columns)){
  code <- trimws(toString(utilization_columns[i,1]))
  #print(code)
  fieldname <- trimws(toString(utilization_columns[i,2]))
  #print(fieldname)
  utilization <- merge(utilization,get_set(code,fieldname),by=c("year","county"))
}

utilization
utilization$Outpatient.Visits <- utilization$`Outpat Visits in Long Term Hosp`+utilization$`Outpat Visits in ST Gen Hosp` +utilization$`Outpat Visits in ST Non-Gen Hsp`+utilization$`Outpatient Visits - Other` +utilization$`Emergency Room Visits`

summary(utilization)

utilization <- utilization[c(1,2,3,13,9,10,11,12)]
#require(gpairs)
#gpairs(utilization[,-c(1,2)])
utilization<-utilization[!(is.na(utilization$Inpatient.Days)),]

################# HEALTH PROFESSION ##################

profession_columns <- read.csv("H:/datathon/profession_columns.csv")
colnames(profession_columns)[1]<-"code"


profession <- data.frame(get_set(trimws(toString(profession_columns[1,1]))
                                 ,trimws(toString(profession_columns[1,2]))))

for (i in 2:nrow(profession_columns)){
  code <- trimws(toString(profession_columns[i,1]))
  #print(code)
  fieldname <- trimws(toString(profession_columns[i,2]))
  #print(fieldname)
  
  new_dataset <- get_set(code,fieldname)
  
  if (!('2013' %in% new_dataset$year)){
    if ('2014' %in% new_dataset$year){
      new_dataset$year[new_dataset$year=="2014"] <- "2013"
    }
    else{
      print (fieldname)
      print (2013)
    }
  }
  if (!('2005' %in% new_dataset$year)){
    print (fieldname)
    print (2005)
  }
  if (!('2010' %in% new_dataset$year)){
    
    print (fieldname)
    print (2010)
  }
  
  profession <- merge(profession,new_dataset,by=c("year","county"))
}

summary(profession)


############## HEALTH FACILITIES #############

facilities_columns <- read.csv("H:/datathon/facilities_columns.csv")

facilities <- data.frame(get_set(trimws(toString(facilities_columns[1,1]))
                                 ,trimws(toString(facilities_columns[1,2]))))

for (i in 2:nrow(facilities_columns)){
  code <- trimws(toString(facilities_columns[i,1]))
  fieldname <- trimws(toString(facilities_columns[i,2]))
  facilities <- merge(facilities,get_set(code,fieldname),by=c("year","county"))
}

summary(facilities)
#require(gpairs)
#gpairs(facilities[,-c(1,2)])


############## EXPENDITURE #############

expenditure_columns <- read.csv("H:/datathon/expenditure_columns.csv")

expenditure <- data.frame(get_set(trimws(toString(expenditure_columns[1,1]))
                                  ,trimws(toString(expenditure_columns[1,2]))))

for (i in 2:nrow(expenditure_columns)){
  code <- trimws(toString(expenditure_columns[i,1]))
  fieldname <- trimws(toString(expenditure_columns[i,2]))
  expenditure <- merge(expenditure,get_set(code,fieldname),by=c("year","county"))
}

summary(expenditure)
#require(gpairs)
#gpairs(expenditure[,-c(1,2)])


############## POPULATION #############

population_columns <- read.csv("H:/datathon/population_columns.csv")
colnames(population_columns)[1]<-"code"

population <- data.frame(get_set(trimws(toString(population_columns[1,1]))
                                 ,trimws(toString(population_columns[1,2]))))
if (!('2010' %in% population$year)){
  if ('2009' %in% population$year){
    population$year[population$year=="2009"] <- "2010"
  }
  else if ('2011' %in% population$year){
    population$year[population$year=="2011"] <- "2010"
  }
  else{
    print (fieldname)
    print (2010)
  }
}

for (i in 2:nrow(population_columns)){
  code <- trimws(toString(population_columns[i,1]))
  #print(code)
  fieldname <- trimws(toString(population_columns[i,2]))
  #print(fieldname)
  #population <- merge(population,get_set(code,fieldname),by=c("year","county"))
  new_dataset <- get_set(code,fieldname)
  
  if (!('2013' %in% new_dataset$year)){
    if ('2014' %in% new_dataset$year){
      new_dataset$year[new_dataset$year=="2014"] <- "2013"
    }
    else if ('2012' %in% new_dataset$year){
      new_dataset$year[new_dataset$year=="2012"] <- "2013"
    }
    else{
      print (fieldname)
      print (2013)
    }
  }
  if (!('2005' %in% new_dataset$year)){
    if ('2000' %in% new_dataset$year){
      new_dataset$year[new_dataset$year=="2000"] <- "2005"
    }
    else{
      print (fieldname)
      print (2005)
    }
  }
  if (!('2010' %in% new_dataset$year)){
    if ('2009' %in% new_dataset$year){
      new_dataset$year[new_dataset$year=="2009"] <- "2010"
    }
    else if ('2011' %in% new_dataset$year){
      new_dataset$year[new_dataset$year=="2011"] <- "2010"
    }
    else{
      print (fieldname)
      print (2010)
    }
  }
  population <- merge(population,new_dataset,by=c("year","county"))
}

summary(population)
#require(gpairs)
#gpairs(population[,-c(1,2)])

############# CORRELATION TESTS ########################

#creating a function for highly correlated columns
highcor <- function(df) {
  #create an empty dataframe
  correlated <- data.frame(col1 = character(), col2 = character(), rat = numeric(), stringsAsFactors = FALSE)
  #create a correlation matrix from train data
  df2 <- cor(df, use = "complete.obs", method = "pearson")
  #pick out highly correlated pairs of columns from correlation matrix
  for (i in (1:nrow(df2))) {
    for (j in (1:ncol(df2))) {
      if (df2[i, j] >= 0.7 & df2[i, j] != 1 & i > j) {
        
        #add to existing dataframe
        new <- data.frame(col1 = rownames(df2)[i], col2 = colnames(df2)[j], rat = df2[i, j])
        correlated <- rbind(correlated, new)
      }
    }
  }
  #write resultant dataset to CSV - mind the working directory
  return (correlated)
}


#numeric columns only - all except 1 and 2

# Utilization
highcor(utilization[-c(1,2)])

# Health Profession
highcor(profession[-c(1,2)])

# Health Facilities
highcor(facilities[-c(1,2)])

# Expenditure
highcor(expenditure[-c(1,2)])

# Population
highcor(population[-c(1,2)])

############# MERGE #################

final_data <- merge (utilization, profession, by=c("year","county") )
final_data <- merge (utilization, facilities, by=c("year","county") )
final_data <- merge (final_data,population, by=c("year","county"))

final_data$year_num <- (as.numeric(final_data$year)-2000)/5

final_data[is.na(final_data)] <- 0

states_county <- ahrf_county_train[c("F12424","F04437")]
colnames(states_county)[1] <- "state"
colnames(states_county)[2] <- "county"

final_data <- merge(states_county,final_data, by = "county")
final_data$state[final_data$state=='GU']<-'VI'
final_data$state <- as.factor(final_data$state)

hist(log(final_data$Outpatient.Visits))
hist(log(final_data$Inpatient.Days))

counties <- unique(final_data$county)
test_counties <- counties[sample(seq(1:2000),500)]
final_train <- final_data[!(final_data$county %in% test_counties),]
final_test <- final_data[final_data$county %in% test_counties,]

final_train <- final_train[order(final_train$county),]
final_test <- final_test[order(final_test$county),]


################## PCA ##################

prin_comp <- prcomp(final_train[-c(1:9)], scale. = T)
print(prin_comp)

#outputs the standard deviation of variables
prin_comp$scale

#compute standard deviation of each principal component
std_dev <- prin_comp$sdev

#compute variance
pr_var <- std_dev^2
pr_var[1:10]

#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]

#scree plot
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")
#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#add a training set with principal components
pca_train <- data.frame(final_train[c(2,4,5)], prin_comp$x)

#we are interested in first 3 PCAs + 
pca_train <- pca_train[,1:6]


#transform test into PCA
pca_test <- predict(prin_comp, newdata = final_test[-c(1:9)])

pca_test <- as.data.frame(pca_test)

pca_test <- data.frame(pca_test[,1:3],state=final_test$state)


######## Linear Model #########

# Inpatients

pca_train_i <- pca_train[-3]
lm_i <- lm(Inpatient.Days ~ .,data = pca_train_i)
summary(lm_i)

#make prediction on test data
pred_test <- predict(lm_i, pca_test)
pred_test[final_test$Total.Number.Hospitals==0 | pred_test < 0] <- 0

pred_train <- predict(lm_i, pca_train)
pred_train[final_train$Total.Number.Hospitals==0 | pred_train < 0] <- 0

rmse(final_test$Inpatient.Days,pred_test)
rmse(final_train$Inpatient.Days,pred_train)

cor(final_test$Inpatient.Days,pred_test)**2
cor(final_train$Inpatient.Days,pred_train)**2

residuals <- final_test$Inpatient.Days-pred_test
plot(residuals, main = "Inpatients Residuals - Linear Model")

# Outpatients
pca_train_o <- pca_train[-2]

lm_o <- lm(Outpatient.Visits ~ .,data = pca_train_o)
summary(lm_o)

#make prediction on test data
pred_test <- predict(lm_o, pca_test)

pred_test[final_test$Total.Number.Hospitals==0 | pred_test < 0] <- 0

pred_train <- predict(lm_o, pca_train)

pred_train[final_train$Total.Number.Hospitals==0 | pred_train < 0] <- 0

rmse(final_test$Outpatient.Visits,pred_test)
rmse(final_train$Outpatient.Visits,pred_train)

cor(final_test$Outpatient.Visits,pred_test)**2
cor(final_train$Outpatient.Visits,pred_train)**2

residuals <- final_test$Outpatient.Visits-pred_test
plot(residuals, main = "Outpatients Residuals - Linear Model")


######## Random Forest #########
require(randomForest)

# Inpatients

pca_train_i <- pca_train[-3]
rf_i <- randomForest(Inpatient.Days ~ .,data = pca_train_i)
rf_i

#make prediction on test data
pred_test <- predict(rf_i, pca_test)

pred_test[final_test$Total.Number.Hospitals==0 | pred_test < 0] <- 0

pred_train <- predict(rf_i, pca_train)

pred_train[final_train$Total.Number.Hospitals==0 | pred_train < 0] <- 0

rmse(final_test$Inpatient.Days,pred_test)
rmse(final_train$Inpatient.Days,pred_train)

cor(final_test$Inpatient.Days,pred_test)**2
cor(final_train$Inpatient.Days,pred_train)**2

residuals <- final_test$Inpatient.Days-pred_test
plot(residuals, main = "Inpatients Residuals - Random Forest")

# Outpatients
pca_train_o <- pca_train[-2]

rf_o <- randomForest(Outpatient.Visits ~ .,data = pca_train_o)
rf_o

#make prediction on test data
pred_test <- predict(rf_o, pca_test)

pred_test[final_test$Total.Number.Hospitals==0 | pred_test < 0] <- 0

pred_train <- predict(rf_o, pca_train)

pred_train[final_train$Total.Number.Hospitals==0 | pred_train < 0] <- 0

rmse(final_test$Outpatient.Visits,pred_test)
rmse(final_train$Outpatient.Visits,pred_train)

cor(final_test$Outpatient.Visits,pred_test)**2
cor(final_train$Outpatient.Visits,pred_train)**2

residuals <- final_test$Outpatient.Visits-pred_test
plot(residuals, main = "Outpatients Residuals - Random Forest")

######## SVM #########
require(e1071)

# Inpatients

pca_train_i <- pca_train[-3]
svm_i <- svm(Inpatient.Days ~ .,data = pca_train_i)
svm_i

#make prediction on test data
pred_test <- predict(svm_i, pca_test)

pred_test[final_test$Total.Number.Hospitals==0 | pred_test < 0] <- 0

pred_train <- predict(svm_i, pca_train)

pred_train[final_train$Total.Number.Hospitals==0 | pred_train < 0] <- 0

rmse(final_test$Inpatient.Days,pred_test)
rmse(final_train$Inpatient.Days,pred_train)

cor(final_test$Inpatient.Days,pred_test)**2
cor(final_train$Inpatient.Days,pred_train)**2

residuals <- final_test$Inpatient.Days-pred_test
plot(residuals, main = "Inpatients Residuals - SVM")


# Outpatients
pca_train_o <- pca_train[-2]

svm_o <-svm(Outpatient.Visits ~ .,data = pca_train_o)
svm_o

#make prediction on test data
pred_test <- predict(svm_o, pca_test)

pred_test[final_test$Total.Number.Hospitals==0 | pred_test < 0] <- 0

pred_train <- predict(svm_o, pca_train)

pred_train[final_train$Total.Number.Hospitals==0 | pred_train < 0] <- 0

rmse(final_test$Outpatient.Visits,pred_test)
rmse(final_train$Outpatient.Visits,pred_train)

cor(final_test$Outpatient.Visits,pred_test)**2
cor(final_train$Outpatient.Visits,pred_train)**2

residuals <- final_test$Outpatient.Visits-pred_test
plot(residuals, main = "Outpatients Residuals - SVM")

########### EVALUATE #############
verify_train <- data.frame(final_train$Inpatient.Days,pred_train)
verify_test <- data.frame(final_test$Inpatient.Days,pred_test)




