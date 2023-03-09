---
title: Predicting death of hospitalized patients by variables based on Padua-Score
  and Charlson-Index
author: Shiran Ben-Meir and Vadim Litvinov
output:
  html_document:
    theme: spacelab
    toc: true # table of content true
    toc_depth: 2  # upto three depths of headings (specified by #, ## and ###)
    toc_float: true
    arg: default
  
---


*The data that we are working on has taken from the final 3rd year project.


In this project we would like to examine the impact of several health conditions and diseases on the chances of hospitalized patients to die. In addition, we will be assisted by age and sex features as predictors.

Those health conditions and diseases mainly derived from Charlson-Comobidity-Index and Padua-score. The Charlson is a model that predicts the one-year mortality for a patient. The Padua score is a 11 features model for assessing the risk of venous throboembolism (VTE) among hospitalized medical patients.

This data contains large number of medical and other features that will be ignored for the sake of focusing the Charlson-Comobidity-Index, Padua-score, sex and age.

```{r include=FALSE}
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("class")
#install.packages("gmodels")
#install.packages("pROC")
#install.packages("devtools")
#install_github("vqv/ggbiplot")
#install.packages("factoextra")
#install.packages("factoextra")
#install.packages("kernlab")
#install.packages("rstatix")

```


```{r include=FALSE}
library(ggplot2)
library(dplyr)
library(ggplot2)
library(class)
library(ggplot2)
library(gmodels)
library(pROC)
library(RColorBrewer)
library(factoextra)
library(devtools)
library(ggbiplot)
library(C50)
library(e1071)
library(caTools)
library(caret)
library(klaR)
library(kernlab)
library(caret)
library(randomForest)

getwd()

```


```{r include=FALSE}
charlson_data <- read.csv("RprojectData.csv", stringsAsFactors = FALSE)
charlson_data <- charlson_data[sample(1:nrow(charlson_data)),]
```



# EDA

First we will remove all the features that do not belong to Charlson-Comobidity-Index, Padua-score, sex or age.

```{r include=FALSE}
charlson_data = subset(charlson_data, select = c(2,11,12,13,14,15,16,17,18,19,20,21,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,88,81) )
```


And then additional removal will be perfomed. First the variables: Charlson.Leukemia, Charlson.lymphoma, Charlson.lymphoma.or.leukemia, Charlson.Metastatic.solid.tumor.3, Charlson.Metastatic.solid.tumor will be removed beacuse they all contained in the variable CPPS_ActiveCancer.
In addition, the features: CPPS_ReducedMobility, CPPS_MIorCVA and Charlson.Hemiplegia will be removed due to absence of positive ("1") samples.
Moreover, the feature Charlson.severe.renal.disease will be removed because it is included in Charlson.moderate.to.severe.renal.disease. From the same reason, the feature "CPPS_Over70" is removed beacuse the feature "age" is a numerical expression of it.

```{r}
charlson_data = subset(charlson_data, select = c(1,2,3,4,5,8,10,11,12,14,15,16,17,18,22,25,26,27,28,29,31,32,33,34))
str(charlson_data)
```


This data contains information about 18,890 patients. All variables are binary except age variable.

gender - male/female.

CPPS_KnownThrombophilia- the patient has a known Thrombophilic condition.

CPPS_ActiveCancer- active cancer known.

CPPS_PreviousVTE- the patient had in the past venous throbo-embolism (VTE)

CPPS_ReducedMobility- patient has low mobility.

CPPS_HeartRespiratoryFailure - the patient had Heart or Respiratory failure.

CPPS_InfectionRheumatologicalDisorder- if the patient has infection asssociated with rheumatological disorder.

CPPS_Obesity- if patient has BMI over 30.

CPPS_OngoingHormonalTreatment- if patient gets hormonal treatment.

Charlson.Myocardial.Infarct - Myocardial Infarction (Heart Attack).

Charlson.Pulmonary.congestion - Pulmonary congestion condition.

Charlson.PVD - Peripheral vascular disease.

Charlson.DB - Diabetes disease.

Charlson.DM+ retinopathy/neuropathy/nephropathy - retinopathy or neuropathy or nephropathy caused by Diabetes mellitus. 

Charlson.cerebrovascular.disease - cerebrovascular disease.

Charlson.Dementia - Dementia disease.

Charlson.chronic.pulmonary.diseas - chronic pulmonary disease. 

Charlson.Connective.tissue.disease - Connective tissue disease.

Charlson.Peptic.ulcer.disease - Ulcer condition.

Charlson.AIDS -  Acquired Immune Deficiency Syndrome. 

Charlson.moderate.to.severe.renal.disease - moderate or severe kidney disease.

Charlson.liver.disease - Liver disease.

age - numeric, age of the patient.

death - binary, died or not. -> classification variable




For the sake of more readable variables' their names have changed
```{r include=FALSE}
charlson_data<-rename(charlson_data, c("male.0.female.1"="gender",
"Charlson.Myocardial.Infarct"="Myocardial.Infarct", "Charlson.Pulmonary.congestion" = "Pulmonary.congestion",
"Charlson.PVD" = "Peripheral vascular disease","Charlson.DB" = "Diabetes",
"Charlson.cerebrovascular.disease" = "cerebrovascular.disease",
"Charlson.Dementia" = "Dementia",
"Charlson.chronic.pulmonary.diseas" = "chronic.pulmonary.disease",
"Charlson.Connective.tissue.disease" = "Connective.tissue.disease",
"Charlson.Peptic.ulcer.disease" = "Peptic.ulcer.disease",
"Charlson.AIDS" = "AIDS",
"Charlson.liver.disease" = "liver.disease",
"Charlson.DM...retinopathy...neuropathy...nephropathy" = "DM+retinopathy/neuropathy/nephropathy",
"CPPS_KnownThrombophilia" = "known_Thrombophilia", 
"CPPS_ActiveCancer" = "active_cancer",
 "CPPS_PreviousVTE" = "previous_VTE",
 "CPPS_ReducedMobility" =  "reduced_mobility",
 "CPPS_HeartRespiratoryFailure" = "Heart/Respiratory_Failure", 
 "CPPS_InfectionRheumatologicalDisorder" = "infectionRheumatologicalDisorder",
"CPPS_Obesity" = "Obesity",
"CPPS_OngoingHormonalTreatment" = "hormonal_treatment",
"Charlson.moderate.to.severe.renal.disease" = "moderate-severe renal"))
```

removing missing values
```{r}
charlson_data <- na.omit(charlson_data)
```



## Gender variable {.tabset .tabset-fade}

Now we will explore the binary gender variable.
```{r}
temp_data= charlson_data
temp_data$gender[temp_data$gender == "0"] <- "male"
temp_data$gender[temp_data$gender == "1"] <- "female"
```

### cases
```{r}
table(temp_data$gender)
```

### percentage
```{r}
round(prop.table(table(temp_data$gender)), digits = 2)
```

### plot
```{r}
ggplot(data = temp_data, aes(x= gender))+ geom_bar(stat = "count", fill= "#d2b48c")+ 
  ggtitle("Gender Distribution")+theme(plot.title = element_text(hjust = 0.5))

```


we can see that the distribution is roughly equal between genders.



## Myocardial Infarction variable {.tabset .tabset-fade}

Now we will explore the Myocardial Infarction variable
```{r}
temp_data= charlson_data
temp_data$Myocardial.Infarct[temp_data$Myocardial.Infarct == "0"] <- "didn't have M.I"
temp_data$Myocardial.Infarct[temp_data$Myocardial.Infarct == "1"] <- "had M.I"

```

### cases
```{r}
table(temp_data$Myocardial.Infarct)
```

### percentage
```{r}
round(prop.table(table(temp_data$Myocardial.Infarct)), digits = 2)
```

### plot
```{r}
ggplot(data = temp_data, aes(x= Myocardial.Infarct))+ geom_bar(stat = "count", fill= "#00cc99")+ 
  ggtitle("Number of patients that had Myocardial Infarction (M.I)")+theme(plot.title = element_text(hjust = 0.5))
```


We can see that there are more patients that ~10% of patients had heart attack. 



## active cancer variable {.tabset .tabset-fade}

Now we will explore the active cancer variable
```{r}
temp_data= charlson_data

temp_data$active_cancer[temp_data$active_cancer == "0"] <- "No active cancer"
temp_data$active_cancer[temp_data$active_cancer == "1"] <- "active cancer"

```

### cases
```{r}
table(temp_data$active_cancer)
```

### percentage
```{r}
round(prop.table(table(temp_data$active_cancer)), digits = 2)
```

### plot
```{r}
ggplot(data = temp_data, aes(x= active_cancer))+ geom_bar(stat = "count", fill= "#cb478e")+ 
  ggtitle("Number of patients that hava active cancer")+theme(plot.title = element_text(hjust = 0.5))
```

We can see that ~20% have active cancer.



## age variable {.tabset .tabset-fade}

Now we will visualize the age distribution

### summary
```{r}
summary(charlson_data$age)
```


### plot
```{r}
Age <- cut(charlson_data$age, breaks = c(0,20,40,60,80,100,Inf),labels = c("0-20","20-40","40-60","60-80","80-100","100+"))
age_death_table= aggregate(death ~ Age, charlson_data, sum)

ggplot(charlson_data, aes(x= Age))+ geom_bar(stat = "count", fill= "#ff8989")+ 
  ggtitle("Age Distribution")+theme(plot.title = element_text(hjust = 0.5))
```


### age dist. by death
```{r}
charlson_data[, 24] <- as.numeric(as.character( charlson_data[, 24] ))
age_death_table

```

### pie chart
```{r}
myPalette <- brewer.pal(5, "Set2") 
pie(age_death_table$death, labels = age_death_table$Age, border="white", col=myPalette )
```


We can see that the distribution is roughly normal (slightly Left-skewed distribution). Moreover the range of ages is between 18 to 116.
We can see that most of the patients that died were at the age range of 60-80. 


## death classification binary variable {.tabset .tabset-fade}

Now we will present the classification binary variable- death 
```{r}
temp_data= charlson_data
temp_data$death[temp_data$death == "0"] <- "alive patient"
temp_data$death[temp_data$death == "1"] <- "dead patient"
```

### cases
```{r}
table(temp_data$death)

```

### percentage
```{r}
round(prop.table(table(temp_data$death)), digits = 2)

```

### plot
```{r}
ggplot(data = temp_data, aes(x= death))+ geom_bar(stat = "count", fill= "#ff8243")+ 
  ggtitle("Number of dead patients")+theme(plot.title = element_text(hjust = 0.5))
```
 
 
We can see that the data is approximately balanced regarding to the death variable.



```{r include=FALSE}
charlson_data[, 1] <- as.integer(as.character( charlson_data[, 1] ))
charlson_data[, 23] <- as.integer(as.character( charlson_data[, 23] ))
charlson_data[, 24] <- as.integer(as.character( charlson_data[, 24] ))

```






## PCA 
```{r warning=FALSE}
temp=charlson_data
dat <- sapply( temp, as.numeric )
pca_result <-prcomp(dat, center = TRUE, scale=TRUE)
ggbiplot(pca_result, ellipse=TRUE, groups= charlson_data$death)

```

As one can see there is a reasonable separation respectively to death variable.

All variables that at the same direction has the same has positive correlation and the variables that are at the opposite direction has negative correlation. 

For example, the variables active cancer and Dementia are correlated with death variable.




The age variable has been normalized to the range of 0 to 1.
```{r include=FALSE}
#normalization function:
normalize <- function(x){
  return((x-min(x)) / (max(x)-min(x)))
}
#normalization
n_charlson_data= as.data.frame(lapply(charlson_data[23], normalize))
```



```{r include=FALSE}
#merging data
normalizedData <-data.frame(charlson_data)
normalizedData <- normalizedData[,-c(23)]
normalizedData <-cbind(normalizedData, n_charlson_data)

```




# Feature Selection
```{r}
temp= normalizedData
temp$death <- as.character(temp$death)
temp$death <- as.factor(temp$death)
fit_rf = randomForest(death~., data=temp)
# Create an importance based on mean decreasing gini
importance(fit_rf)

```


As we can learn from the table, the attributes that have the highest contribution to death prediction (according to Gini-Index) are: reduced mobility, active cancer and age.

We can assume that those strong attributes will also have high weight in the following learning algorithms.




# supervised learning algorithms
creating test and training sets.

The partition that has been chosen is train-set (70%) and test-set (30%).
```{r}
train_charlson_data= normalizedData[1:13218,]
test_charlson_data= normalizedData[13219:18884,]
train_labels= normalizedData[1:13218, 23]
test_labels= normalizedData[13219:18884, 23]

```



## KNN {.tabset .tabset-fade}

The first algorithm that will be performed is KNN. In order to improve the KNN algorithm performance, three different odd K values will be used. After comparison, we chose k=19 because it yielded the best accuracy.

### k=1
```{r}
test_pred <- knn(train = train_charlson_data, test = test_charlson_data , cl= train_labels, k=1)
CrossTable(x= test_labels, y= test_pred, prop.chisq = FALSE)
```

### k=3
```{r}
test_pred <- knn(train = train_charlson_data, test = test_charlson_data , cl= train_labels, k=3)
CrossTable(x= test_labels, y= test_pred, prop.chisq = FALSE)

```

### k=19
```{r}
test_pred <- knn(train = train_charlson_data, test = test_charlson_data , cl= train_labels, k=19)
CrossTable(x= test_labels, y= test_pred, prop.chisq = FALSE)

```
## {.unlisted .unnumbered}


```{r include=FALSE}
roc_knn<- roc(as.numeric(test_charlson_data$death) ,as.numeric(test_pred))
```

```{r}
auc(roc_knn)
```
```{r include=FALSE}

```


By looking at the confusion matrix, we can point out that there are 0 FP predictions.

The accuracy calculated from the matrix is 99.96%. The area under the ROC curve is 0.9881 which is nearly 1- almost perfect prediction model.





## Decision Tree{.tabset .tabset-fade}

The second algorithm that has been chosen is Decision Tree via C50 package.
```{r}
dt_model <- C5.0(x=train_charlson_data[-23], y=as.factor(train_charlson_data$death))
```
After 3 runs, an average of 9 nodes were in the tree models (the tree changes every time due to data randomization). It also known as 9 decisions/questions of "yes or no". 

### plot
```{r}
summary(dt_model)
```

### Cross Table
```{r}
dt_pred <- predict(dt_model, test_charlson_data)
CrossTable(test_charlson_data$death, dt_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual death', 'preticted death'))

```  
## {.unlisted .unnumbered}


```{r include=FALSE}
#creating ROC and calculating area under the curve
roc_dt<- roc(as.numeric(test_charlson_data$death) ,as.numeric(dt_pred))

```



```{r}
auc(roc_dt)

#calculating accuracy of the algorithm from the confusion matrix
acuuracy_dt= sum(as.numeric(test_charlson_data$death) == dt_pred) / length(dt_pred)
acuuracy_dt
```

In the summary it also shown that there are in average only 6 attributes in usage out of the 18 predictors in general, and the highest percentage of usage is if there is a metastatic solid tumor stage 3 to the patient.

In most of the trees the variables that has the highest Information-Gain are reduced-mobility and then active cancer and liver disease.

The calculated accuracy from the confusion matrix is 0.7917402 and the AUC is 0.6326. 




## Naive Bayes{.tabset .tabset-fade}

```{r, warning=FALSE}
training <- train_charlson_data
test <- test_charlson_data
training$death <- as.factor(training$death)
nb_mod <- NaiveBayes(death ~ ., data = training)
pred <- predict(nb_mod, test)

# Confusion Matrix
tab <- table(pred$class, test$death)
caret::confusionMatrix(tab)
```


```{r include=FALSE}
#creating ROC and calculating area under the curve
roc_nb<- roc(as.numeric(as.character(test$death)) ,as.numeric(as.character(pred$class)))
```

```{r}
auc(roc_nb)
```


We can see that the accuracy is 0.776 and the AUC value is 0.6483.
In addition, we can learn from the low Kappa value (0.3155) that there are high chances to get to the right predcition by randomly.





## SVM{.tabset .tabset-fade}

```{r}
classifier = svm(formula = death ~ ., 
                 data = train_charlson_data, 
                 type = 'C-classification', 
                 kernel = 'linear') 


svm_pred = predict(classifier, newdata = test_charlson_data[-23]) 
cm = table(test_charlson_data[, 23], svm_pred) 
cm

```

```{r include=FALSE}
#creating ROC and calculating area under the curve
roc<- roc(as.numeric(test_charlson_data$death) ,as.numeric(svm_pred))
auc(roc)
```

```{r}
#calculating accuracy of the algorithm from the confusion matrix
acuuracy= sum(as.numeric(as.character(test$death)) == svm_pred) / length(svm_pred)
acuuracy
```
Here we chose to use linear kernel.
We can see that the accuracy is 0.7922 and the AUC is 0.6227.





# Unsupervised learning


## kmeans - paired variables{.tabset .tabset-fade}

```{r}
set.seed(2000)
patient_clusters4 <- kmeans(normalizedData, 4)
normalizedData$cluster <- patient_clusters4$cluster
```


```{r include=FALSE}
set.seed(2000)
patient_clusters2 <- kmeans(normalizedData, 2)
#normalizedData$cluster <- patient_clusters2$cluster
```


Lets also look at some attributes of our clusters:

### death and active cancer
```{r}
aggregate(data = normalizedData, death ~ cluster, mean)
aggregate(data = normalizedData, active_cancer ~ cluster, mean)
```

### obesity and diabetes
```{r}
aggregate(data = normalizedData, Obesity ~cluster, mean)
aggregate(data = normalizedData, Diabetes ~cluster, mean)
```





## kmeans - plots{.tabset .tabset-fade}

For visualization, we chose to present the cluster plot by k=4 (as what we have been working on until now) and k=2 (the value that has the best separation)

### k=4
```{r}
fviz_cluster(patient_clusters4, data = normalizedData, choose.vars = NULL, stand = TRUE,
  axes = c(1, 2), geom = c("point", "text"), repel = FALSE,
  show.clust.cent = TRUE, ellipse = TRUE, ellipse.type = "convex",
  ellipse.level = 0.95, ellipse.alpha = 0.1, shape = NULL,
  pointsize = 0.3, labelsize = 0, main = "Cluster plot",
  xlab = NULL, ylab = NULL, outlier.color = "black",
  outlier.shape = 19, outlier.pointsize = pointsize,
  outlier.labelsize = labelsize, ggtheme = theme_grey())


```

### k=2
```{r}
fviz_cluster(patient_clusters2, data = normalizedData, choose.vars = NULL, stand = TRUE,
  axes = c(1, 2), geom = c("point", "text"), repel = FALSE,
  show.clust.cent = TRUE, ellipse = TRUE, ellipse.type = "convex",
  ellipse.level = 0.95, ellipse.alpha = 0.1, shape = NULL,
  pointsize = 0.3, labelsize = 0, main = "Cluster plot",
  xlab = NULL, ylab = NULL, outlier.color = "black",
  outlier.shape = 19, outlier.pointsize = pointsize,
  outlier.labelsize = labelsize, ggtheme = theme_grey())

```



# conclusion and comparison
By considering and comparing all area under curve (AUC) and accuracy values we found out that KNN algorithm has significantly better performance than all other three algorithms (accuracy - 0.9996).

All other three algorithms have approximately the same accuracy value. Following KNN comes Decision Tree algorithm with accuracy of 0.791. In the third place comes Naïve Bayes with accuracy value of 0.776 and the algorithm with the poorest performance is SVM with accuracy value of 0.7299.

We can assume that due to the fact that the data has relatively large amount of samples (18890) the majority of neighbors in KNN will provide the right classification with small amount of anomalies.

Regarding k-means algorithm, it provided a reasonable separation only with k=2.