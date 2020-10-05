### October 3, 2020

### Palmer Penguins Analysis
*Overview*

This project is to explore the Palmer Penguin Data and see if
a model can be built to identify penguin species based on body measurements.

### Description
The palmerpenguins data contains size measurements for three penguin species observed on three islands in the Palmer Archipelago, Antarctica.

![Palmer Penguins](https://github.com/allisonhorst/palmerpenguins/blob/master/man/figures/lter_penguins.png)
Artwork by Allison Horst @allison_horst.


### Citation
Horst AM, Hill AP, Gorman KB (2020). palmerpenguins: Palmer
Archipelago (Antarctica) penguin data. R package version 0.1.0.
https://allisonhorst.github.io/palmerpenguins/. doi:
10.5281/zenodo.3960218.

Data originally published in:

Gorman KB, Williams TD, Fraser WR (2014). Ecological sexual dimorphism and environmental variability within a community of Antarctic penguins (genus Pygoscelis). PLoS ONE 9(3):e90081. https://doi.org/10.1371/journal.pone.0090081

These data were collected from 2007 - 2009 by Dr. Kristen Gorman with the Palmer Station Long Term Ecological Research Program, part of the US Long Term Ecological Research Network. The data were imported directly from the Environmental Data Initiative (EDI) Data Portal, and are available for use by CC0 license (“No Rights Reserved”) in accordance with the Palmer Station Data Policy.

### Analysis Variables And Software

#Install Remote Package if not already available

#install.packages("remotes")

#Install Palmer Penguin Data
#remotes::install_github("allisonhorst/palmerpenguins")
#citation(“palmerpenguins”) #> #> To cite palmerpenguins in publications use: #> #> Horst AM, Hill AP, Gorman KB (2020). palmerpenguins: Palmer #> Archipelago (Antarctica) penguin data. R package version 0.1.0. #> https://allisonhorst.github.io/palmerpenguins/. doi: #> 10.5281/zenodo.3960218. #> #> A BibTeX entry for LaTeX users is #> #> @Manual{, #> title = {palmerpenguins: Palmer Archipelago (Antarctica) penguin data}, #> author = {Allison Marie Horst and Alison Presmanes Hill and Kristen B Gorman}, #> year = {2020}, #> note = {R package version 0.1.0}, #> doi = {10.5281/zenodo.3960218}, #> url = {https://allisonhorst.github.io/palmerpenguins/}, #> } #> Also help from Jason Brownlee on machine learning in R February 3, 2016 article
#Load Palmer Penguin library and data set
library(palmerpenguins)
data(package = 'palmerpenguins')
#Load libraries we need for dataframe manipulation and plotting
library(tidyr)
library(dplyr)
## 
## Attaching package: 'dplyr'
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
library(ggplot2)
library(gridExtra)
## 
## Attaching package: 'gridExtra'
## The following object is masked from 'package:dplyr':
## 
##     combine
library(caret)
## Loading required package: lattice
library(ellipse)
## 
## Attaching package: 'ellipse'
## The following object is masked from 'package:graphics':
## 
##     pairs
library(cluster) 
library(fpc)
library(factoextra)
## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa
library(knitr)
#Initial look at the reduced data set
head(penguins)
## # A tibble: 6 x 8
##   species island bill_length_mm bill_depth_mm flipper_length_~ body_mass_g sex  
##   <fct>   <fct>           <dbl>         <dbl>            <int>       <int> <fct>
## 1 Adelie  Torge~           39.1          18.7              181        3750 male 
## 2 Adelie  Torge~           39.5          17.4              186        3800 fema~
## 3 Adelie  Torge~           40.3          18                195        3250 fema~
## 4 Adelie  Torge~           NA            NA                 NA          NA <NA> 
## 5 Adelie  Torge~           36.7          19.3              193        3450 fema~
## 6 Adelie  Torge~           39.3          20.6              190        3650 male 
## # ... with 1 more variable: year <int>
Some data will likely not be necesary for prediction purposes like “sex” and “year”.
#Look at the data set columns and data types.
str(penguins)
## tibble [344 x 8] (S3: tbl_df/tbl/data.frame)
##  $ species          : Factor w/ 3 levels "Adelie","Chinstrap",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ island           : Factor w/ 3 levels "Biscoe","Dream",..: 3 3 3 3 3 3 3 3 3 3 ...
##  $ bill_length_mm   : num [1:344] 39.1 39.5 40.3 NA 36.7 39.3 38.9 39.2 34.1 42 ...
##  $ bill_depth_mm    : num [1:344] 18.7 17.4 18 NA 19.3 20.6 17.8 19.6 18.1 20.2 ...
##  $ flipper_length_mm: int [1:344] 181 186 195 NA 193 190 181 195 193 190 ...
##  $ body_mass_g      : int [1:344] 3750 3800 3250 NA 3450 3650 3625 4675 3475 4250 ...
##  $ sex              : Factor w/ 2 levels "female","male": 2 1 1 NA 1 2 1 2 NA NA ...
##  $ year             : int [1:344] 2007 2007 2007 2007 2007 2007 2007 2007 2007 2007 ...
Essentially, we have three species: “Adelie”, “Chinstrap”, and “Gentoo”.
The penguins are located across three islands: “Biscoe”, “Dream”, and “Torgersen”
#Look at number of species in data set
table(penguins$species)
## 
##    Adelie Chinstrap    Gentoo 
##       152        68       124
#Taking a looka tht dimensions of the reduced data set
dim(penguins)
## [1] 344   8
There are 344 rows, matching the count of penguins with 8 columns.
However, the data does include some “NA” or non measured penguins. Those rows will need ## to be dropped for the machine learning analysis.
#Copy data into new dataframe and look at Column names for calculations

df <- penguins
names(df)
## [1] "species"           "island"            "bill_length_mm"   
## [4] "bill_depth_mm"     "flipper_length_mm" "body_mass_g"      
## [7] "sex"               "year"
#Initial plot to see which parameters might yield clues about species types
#Species vs body mass

bp<-ggplot(aes(x = species, y = body_mass_g, fill=species), data = df) + geom_boxplot() + coord_flip()

bp + scale_fill_hue(l=40, c=35)
## Warning: Removed 2 rows containing non-finite values (stat_boxplot).

![Body Mass Box Plot]()
  ### It appears for body mass that the Chinstrap and Adelie penguins share some ### similarities, while the Gentoo penguins are heavier.
#Initial plot to see which parameters might yield clues about species types
#Species vs bill length
bp<-ggplot(aes(x = species, y = bill_length_mm, fill=species), data = df) + geom_boxplot() + coord_flip()

bp + scale_fill_hue(l=40, c=35)
## Warning: Removed 2 rows containing non-finite values (stat_boxplot).

![Bill Length Box Plot]()
  ### On the other hand, the Chinstrap and Gentoo penguins share similar ### bill length.
#Initial plot to see which parameters might yield clues about species types
#Species vs bill depth
bp<-ggplot(aes(x = species, y = bill_depth_mm, fill=species), data = df) + geom_boxplot() + coord_flip()

bp + scale_fill_hue(l=40, c=35)
## Warning: Removed 2 rows containing non-finite values (stat_boxplot).
![Bill Depth Box Plot]() 
Chinstrap and Adelie penguins have overlap in bill depth.
#Initial plot to see which parameters might yield clues about species types
#Species vs flipper length
bp<-ggplot(aes(x = species, y = flipper_length_mm, fill=species), data = df) + geom_boxplot() + coord_flip()

bp + scale_fill_hue(l=40, c=35)
## Warning: Removed 2 rows containing non-finite values (stat_boxplot).
![Flipper Length Box Plot]()
  ### And lastly, Chinstrap and Adelie penguins have some overlap in flipper length
#Grid analysis of histograms of the parameters: Length and Width and the Petal Length and Width.
# "species"           "island"            "bill_length_mm"    "bill_depth_mm"     "flipper_length_mm"
# "body_mass_g"       "sex"               "year"     

p1 <- ggplot(aes(x = bill_length_mm, fill = species), data = df) +
  geom_histogram()

p2 <- ggplot(aes(x = bill_depth_mm, fill = species), data = df) +
   geom_histogram()

p3 <- ggplot(aes(x = flipper_length_mm, fill = species), data = df) + geom_histogram()

p4 <- ggplot(aes(x = body_mass_g, fill = species), data = df) +
   geom_histogram()


grid.arrange(p1, p2, p3, p4, ncol = 2)
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
## Warning: Removed 2 rows containing non-finite values (stat_bin).
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
## Warning: Removed 2 rows containing non-finite values (stat_bin).
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
## Warning: Removed 2 rows containing non-finite values (stat_bin).
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
## Warning: Removed 2 rows containing non-finite values (stat_bin).

![Histogram Plot]() 
Again, using the histograms, we the same overlaps in a more colorful fashion.
#Looking at which penguins were measure on which islands

ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(alpha = 0.8) +
  scale_fill_manual(values = c("darkorange","purple","cyan4"),
                    guide = FALSE) +
  theme_minimal() +
  facet_wrap(~species, ncol = 1) +
  coord_flip()

  ![Count By Island Plot]()
  ### It would appear for the study that Adelie penguins can be found on all ### three islands, which Chinstraps were exclusive to only Dream and ### Gentoo exclusive only to Biscoe.
penguins %>%
  select(species, body_mass_g, ends_with("_mm")) %>%
  GGally::ggpairs(aes(color = species)) +
  scale_colour_manual(values = c("darkorange","purple","cyan4")) +
  scale_fill_manual(values = c("darkorange","purple","cyan4"))
## Registered S3 method overwritten by 'GGally':
##   method from   
##   +.gg   ggplot2
## Warning in ggally_statistic(data = data, mapping = mapping, na.rm = na.rm, :
## Removed 2 rows containing missing values

## Warning in ggally_statistic(data = data, mapping = mapping, na.rm = na.rm, :
## Removed 2 rows containing missing values

## Warning in ggally_statistic(data = data, mapping = mapping, na.rm = na.rm, :
## Removed 2 rows containing missing values

## Warning in ggally_statistic(data = data, mapping = mapping, na.rm = na.rm, :
## Removed 2 rows containing missing values

## Warning in ggally_statistic(data = data, mapping = mapping, na.rm = na.rm, :
## Removed 2 rows containing missing values

## Warning in ggally_statistic(data = data, mapping = mapping, na.rm = na.rm, :
## Removed 2 rows containing missing values
## Warning: Removed 2 rows containing non-finite values (stat_boxplot).

## Warning: Removed 2 rows containing non-finite values (stat_boxplot).

## Warning: Removed 2 rows containing non-finite values (stat_boxplot).

## Warning: Removed 2 rows containing non-finite values (stat_boxplot).
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
## Warning: Removed 2 rows containing non-finite values (stat_bin).
## Warning: Removed 2 rows containing non-finite values (stat_density).
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
## Warning: Removed 2 rows containing non-finite values (stat_bin).
## Warning: Removed 2 rows containing missing values (geom_point).
## Warning: Removed 2 rows containing non-finite values (stat_density).
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
## Warning: Removed 2 rows containing non-finite values (stat_bin).
## Warning: Removed 2 rows containing missing values (geom_point).

## Warning: Removed 2 rows containing missing values (geom_point).
## Warning: Removed 2 rows containing non-finite values (stat_density).
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
## Warning: Removed 2 rows containing non-finite values (stat_bin).
## Warning: Removed 2 rows containing missing values (geom_point).

## Warning: Removed 2 rows containing missing values (geom_point).

## Warning: Removed 2 rows containing missing values (geom_point).
## Warning: Removed 2 rows containing non-finite values (stat_density).

![Pairwise Plot]()
  ### The pairwise graph sums up our previous findings.
#Before modeling, it is good to look to see if there is any clustering in
#the data set

#Variables used in clustering "Island", bill_length_mm", "bill_depth_mm",
#"flipper_length_mm", and "body_mass_g"

#Seed for starting number used to generate a sequence of random numbers
set.seed(20)

#Creating new dataframe for clustering.
data_for_clustering <- df

#Dropping "NA" values
data_for_clustering_no_na <- drop_na(data_for_clustering)

#kmeans data clustering partitioning (assuming 3 centers or clusters).

#Using Species as the target, with features "bill_length_mm" "bill_depth_mm"     #"flipper_length_mm" and "body_mass_g" 

clusters_penguins <- kmeans(data_for_clustering_no_na[,4:6], centers = 3)

plotcluster(data_for_clustering_no_na[,3:6], clusters_penguins$cluster, color = TRUE, shade = TRUE, xlab="", ylab="")
## Warning in plot.window(...): "color" is not a graphical parameter
## Warning in plot.window(...): "shade" is not a graphical parameter
## Warning in plot.xy(xy, type, ...): "color" is not a graphical parameter
## Warning in plot.xy(xy, type, ...): "shade" is not a graphical parameter
## Warning in axis(side = side, at = at, labels = labels, ...): "color" is not a
## graphical parameter
## Warning in axis(side = side, at = at, labels = labels, ...): "shade" is not a
## graphical parameter
## Warning in axis(side = side, at = at, labels = labels, ...): "color" is not a
## graphical parameter
## Warning in axis(side = side, at = at, labels = labels, ...): "shade" is not a
## graphical parameter
## Warning in box(...): "color" is not a graphical parameter
## Warning in box(...): "shade" is not a graphical parameter
## Warning in title(...): "color" is not a graphical parameter
## Warning in title(...): "shade" is not a graphical parameter
![Cluster Plot]() 
#Clustering does indicate that there is some separation in the penguin data #that might lend itself for modeling.
#Creating clustering table to exam if there is proper separation
penguins_no_na <- drop_na(penguins)

table(clusters_penguins$cluster, penguins_no_na$species)
##    
##     Adelie Chinstrap Gentoo
##   1      0         0     61
##   2     34        15     57
##   3    112        53      1
#It appears that some of the penguins aren’t properly categorized due to the #feature overlaps
#Modeling Work #Goal is to determine if a model can be built to predict the species of penguin #based on measurements
#Data will be split into a test set and a validation set.
#Create a Validation Dataset

#Starting with clean dataframe and removing "NA" values


data_for_model <- df

#Dropping "NA" values
data_for_model_no_na <- drop_na(data_for_model)


set.seed(300)

validation_index <- createDataPartition(data_for_model_no_na$species, p=0.80, list=FALSE)

# select 20% of the data for validation
validation <- data_for_model_no_na[-validation_index,]


# use the remaining 80% of data to training and testing the models
data_for_model_no_na <- data_for_model_no_na[validation_index,]
## Warning: The `i` argument of ``[`()` can't be a matrix as of tibble 3.0.0.
## Convert to a vector.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_warnings()` to see where this warning was generated.
#Summary of the data set
dim(data_for_model_no_na)
## [1] 268   8
dim(validation)
## [1] 65  8
###Validation and Modeling dataframes have correct proportions
# list types for each attribute
sapply(data_for_model_no_na, class)
##           species            island    bill_length_mm     bill_depth_mm 
##          "factor"          "factor"         "numeric"         "numeric" 
## flipper_length_mm       body_mass_g               sex              year 
##         "integer"         "integer"          "factor"         "integer"
# take a peek at the first 5 rows of the data
head(data_for_model_no_na)
## # A tibble: 6 x 8
##   species island bill_length_mm bill_depth_mm flipper_length_~ body_mass_g sex  
##   <fct>   <fct>           <dbl>         <dbl>            <int>       <int> <fct>
## 1 Adelie  Torge~           39.1          18.7              181        3750 male 
## 2 Adelie  Torge~           39.5          17.4              186        3800 fema~
## 3 Adelie  Torge~           40.3          18                195        3250 fema~
## 4 Adelie  Torge~           36.7          19.3              193        3450 fema~
## 5 Adelie  Torge~           39.3          20.6              190        3650 male 
## 6 Adelie  Torge~           39.2          19.6              195        4675 male 
## # ... with 1 more variable: year <int>
# list the levels for the class
levels(data_for_model_no_na$species)
## [1] "Adelie"    "Chinstrap" "Gentoo"
#Looking at the above data reads, the modeling dataframe checks out.
# summarize the class distribution
percentage <- prop.table(table(data_for_model_no_na$species)) * 100
cbind(freq=table(data_for_model_no_na$species), percentage=round(percentage,1))
##           freq percentage
## Adelie     117       43.7
## Chinstrap   55       20.5
## Gentoo      96       35.8
# summarize attribute distributions
summary(data_for_model_no_na)
##       species          island    bill_length_mm  bill_depth_mm  
##  Adelie   :117   Biscoe   :135   Min.   :32.10   Min.   :13.10  
##  Chinstrap: 55   Dream    : 97   1st Qu.:39.20   1st Qu.:15.57  
##  Gentoo   : 96   Torgersen: 36   Median :44.05   Median :17.30  
##                                  Mean   :43.88   Mean   :17.14  
##                                  3rd Qu.:48.40   3rd Qu.:18.62  
##                                  Max.   :59.60   Max.   :21.20  
##  flipper_length_mm  body_mass_g       sex           year     
##  Min.   :172.0     Min.   :2700   female:137   Min.   :2007  
##  1st Qu.:190.0     1st Qu.:3550   male  :131   1st Qu.:2007  
##  Median :197.0     Median :4000                Median :2008  
##  Mean   :200.8     Mean   :4192                Mean   :2008  
##  3rd Qu.:212.2     3rd Qu.:4750                3rd Qu.:2009  
##  Max.   :231.0     Max.   :6050                Max.   :2009
#Visualize Data set


# split input and output (not using "Island" feature)
x <- data_for_model_no_na[,3:6]
y <- data_for_model_no_na[,1]
# barplot for class breakdown
plot(y)
![Class Bar Plot]() 
# scatterplot matrix
featurePlot(x=x, y=y$species, plot="ellipse")
 ![BScatter Plot Matrix]()
# box and whisker plots for each attribute
featurePlot(x=x, y=y$species, plot="box")
 ![Feature Box Plot]()
# density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y$species, plot="density", scales=scales)
 ![Feature Density Plot]()
#Evaluate 5 different Algorithms

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10, savePredictions = TRUE)
metric <- "Accuracy"
# a) linear algorithms
set.seed(7)
fit.lda <- train(species~., data=data_for_model_no_na, method="lda", metric=metric, trControl=control)


# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(species~., data=data_for_model_no_na, method="rpart", metric=metric, trControl=control)

# kNN
set.seed(7)
fit.knn <- train(species~., data=data_for_model_no_na, method="knn", metric=metric, trControl=control)

# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(species~., data=data_for_model_no_na, method="svmRadial", metric=metric, trControl=control)

# Random Forest
set.seed(7)
fit.rf <- train(species~., data=data_for_model_no_na, method="rf", metric=metric, trControl=control)
# summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
## 
## Call:
## summary.resamples(object = results)
## 
## Models: lda, cart, knn, svm, rf 
## Number of resamples: 10 
## 
## Accuracy 
##           Min.   1st Qu.   Median      Mean   3rd Qu.      Max. NA's
## lda  0.9629630 1.0000000 1.000000 0.9962963 1.0000000 1.0000000    0
## cart 0.8888889 0.9348291 0.962963 0.9589642 0.9910714 1.0000000    0
## knn  0.6428571 0.7425214 0.792735 0.7769638 0.8148148 0.8846154    0
## svm  0.9615385 1.0000000 1.000000 0.9961538 1.0000000 1.0000000    0
## rf   0.9615385 1.0000000 1.000000 0.9924501 1.0000000 1.0000000    0
## 
## Kappa 
##           Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
## lda  0.9430380 1.0000000 1.0000000 0.9943038 1.0000000 1.0000000    0
## cart 0.8269231 0.8958654 0.9424867 0.9355850 0.9862745 1.0000000    0
## knn  0.4117647 0.5935413 0.6606441 0.6409066 0.7018256 0.8164706    0
## svm  0.9379475 1.0000000 1.0000000 0.9937947 1.0000000 1.0000000    0
## rf   0.9379475 1.0000000 1.0000000 0.9880132 1.0000000 1.0000000    0
#At first blush, lda has the highest accuracy with SVM and rf following closely
# Here we visually compare accuracy of the models
dotplot(results)
![Accuracy Dot Plot]() 
# Summarize Best Model
print(fit.lda)
## Linear Discriminant Analysis 
## 
## 268 samples
##   7 predictor
##   3 classes: 'Adelie', 'Chinstrap', 'Gentoo' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 241, 240, 242, 241, 241, 242, ... 
## Resampling results:
## 
##   Accuracy   Kappa    
##   0.9962963  0.9943038
# estimate skill of best model on the validation dataset
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$species)
## Confusion Matrix and Statistics
## 
##            Reference
## Prediction  Adelie Chinstrap Gentoo
##   Adelie        29         0      0
##   Chinstrap      0        13      0
##   Gentoo         0         0     23
## 
## Overall Statistics
##                                      
##                Accuracy : 1          
##                  95% CI : (0.9448, 1)
##     No Information Rate : 0.4462     
##     P-Value [Acc > NIR] : < 2.2e-16  
##                                      
##                   Kappa : 1          
##                                      
##  Mcnemar's Test P-Value : NA         
## 
## Statistics by Class:
## 
##                      Class: Adelie Class: Chinstrap Class: Gentoo
## Sensitivity                 1.0000              1.0        1.0000
## Specificity                 1.0000              1.0        1.0000
## Pos Pred Value              1.0000              1.0        1.0000
## Neg Pred Value              1.0000              1.0        1.0000
## Prevalence                  0.4462              0.2        0.3538
## Detection Rate              0.4462              0.2        0.3538
## Detection Prevalence        0.4462              0.2        0.3538
## Balanced Accuracy           1.0000              1.0        1.0000
###In Summary, the data set is rather small with only 344 measurements. ###However, there appears to be enough variance in the features to be able ###to use modeling to predict the penguin species based on the measurements. ###Possible next steps would be to gather more penguin data.

###Software used:
R and R Studio






Resources
[Git Bash documentation](https://git-scm.com/doc)


### Credits

https://Udacity.com

https://guides.github.com/features/mastering-markdown/