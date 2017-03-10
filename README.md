
## Pratical Machines Learning Project-Assignment Writeup

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it.This project will use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. 

### Data Cleaning
Before starting the project, load machine learning and plot packages that will be used in this project. 
```{r}
>library(caret)
>library(rattle)
>library(randomForest)
```
First, we should load both training data and testing data into R. Remove all the error string "#DIV/0!" and all the empty cells and replace with NA. 

```{r}
train_source="http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test_source="http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
training_document=read.csv(url(train_source), na.strings=c("NA","#DIV/0!",""))
testing_document=read.csv(url(test_source), na.strings=c("NA","#DIV/0!",""))
View(training_document)
[1] 19622   160
View(testing_document)
[1]  20 160
```
### Cross Validation
After loading the data, it shows that training set has 19622 rows and prediction set has 20 rows of data with 160 variables. In order to cross validate the model, we use "createDataPartition" function to split the training data into two seperate sub datasets, one is  "training" set with 60% of data and another one is "testing" set with 40% of data.

```{r}
intrain=createDataPartition(training_document $classe, p=0.6, list=FALSE)
training = training_document [intrain, ]
testing= training_document [-intrain, ]
dim(training); dim(testing)
[1] 11776   160
[1] 7846  160
```
I will use training set to find the best model and use testing set to verify our findings, and use the model we selected according to training and testing set to predict the "classe" in the prediction set. 

Some of the variables have no variability, use nearZeroVar function in caret package to identify the variables with very little variability and would not likely to become predictors. And then remove these variables. 

```{r}
> badcols=nearZeroVar(training)
> training1=training[,-badcols]
> testing1=testing[,-badcols]
> nsvvariable=names(training1)
> nsvvariable
> dim(training1)
> dim(testing1)
[1] 11776   132
[1] 7846  132


> finaltesting1=testing_document[,-badcols]
> dim(finaltesting1)
[1]  20 132
```
After removing these new zero variance variables, I narrow it down to 132 variables for training, testing and prediction set. But some of the variables have no single value in it. I also checked the columns with NA values. In the training, testing and final prediction datasets. Columns are either all with values or without any values. In this case, I don't need to choose the variables with a lot of missing values, the next step is to remove all the variables with missing values.

```{r}
> missing=which(colSums(is.na(training1))>0)
> training2=training1[,-c(missing)]
> dim(training2)
 [1] 11776    59
 
> missing=which(colSums(is.na(testing1))>0)
> testing2=testing1[,-c(missing)]
> dim(testing2)
[1] 7846   59

> missing=which(colSums(is.na(finaltesting1))>0)
> finaltesting2=finaltesting1[,-c(missing)]
> dim(finaltesting2)
[1] 20 59
```
Now, we only have 59 variables in these three sets. But the first column is a just increasing natural numbers so we can remove the first column. And for the prediciton set, we can also remove the last column and use our final model to predict classe variable. 

```{r}
> training2=training2[c(-1)]
> dim(training2)
[1] 11776    58
> testing2=testing2[c(-1)]
> dim(testing2)
[1] 7846   58
> finaltesting=finaltesting2[c(-1, -58)]
> dim(finaltesting)
[1] 20 57
```

### Model selection and evaluation
After rerange the data into proporiate format, we can use training set to fit the model. The first model we choose is the random forest. 

```{r}
> modelfit1=randomForest(classe ~ ., data=training2)
```
We can use this modelfit to predict the testing dataset to see accuracy of the prediction. Given two variables (prediction and observation), the mean squared error and R-squared are calculated. For two factors, the overall agreement rate and Kappa are determined.
```{r}
> postResample(predictions1, testing2$classe)
 Accuracy     Kappa 
0.9978333 0.9972596 
```
The accuracy is 0..997833 which is quite close to 1.

```{r}
> predictions1=predict(modelfit1, testing2, type = "class")
> confusionMatrix(predictions1, testing2$classe)

Confusion Matrix and Statistics

          Reference
Prediction    A    B    C    D    E
         A 2232    0    0    0    0
         B    0 1518    3    0    0
         C    0    0 1363    6    0
         D    0    0    2 1280    2
         E    0    0    0    0 1440

Overall Statistics
                                          
               Accuracy : 0.9983          
                 95% CI : (0.9972, 0.9991)
    No Information Rate : 0.2845          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.9979          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: A Class: B Class: C Class: D Class: E
Sensitivity            1.0000   1.0000   0.9963   0.9953   0.9986
Specificity            1.0000   0.9995   0.9991   0.9994   1.0000
Pos Pred Value         1.0000   0.9980   0.9956   0.9969   1.0000
Neg Pred Value         1.0000   1.0000   0.9992   0.9991   0.9997
Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
Detection Rate         0.2845   0.1935   0.1737   0.1631   0.1835
Detection Prevalence   0.2845   0.1939   0.1745   0.1637   0.1835
Balanced Accuracy      1.0000   0.9998   0.9977   0.9974   0.9993
```
The accuracy is 99.83% which is pretty good prediction. In this case, I can use the random forest to predict the final prediction set with 20 cases. 

### Out of sample error
The out of sample error is the error rate you get on new data set. In my case, it's the error rate after running the predict() function on the 4 testing set. 

The expected out-of-sample error is 100%-99.83% = 0.17%.

### Prediction 
```{r}
prediction2=predict(modelfit1, finaltesting2, type = "class")
prediction2

[1] B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
```

### Conclusion
The random forest algorithm appears to perform very accurate estimation for training and testing dataset. So in this project, I use random forest method to predict the final prediction data. 


