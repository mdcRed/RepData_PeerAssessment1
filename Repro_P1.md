# Reproducible Research - Self Monitor Activity Dataset Analysis
My D. Coyne  
March 9, 2015  

    
---

### Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. In this assignment one will analyze a self monintor dataset;  the data is collected by an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.  **The objective of the assignment is to show how tool such as knitr and R-mardow would help analysts in creating reproducible data analysis research project.**

### About the Dataset

The variables included in this dataset are:

    * **steps:**  Number of steps taking in a 5-minute interval (missing values are coded as NA)

    * **date:** The date on which the measurement was taken in YYYY-MM-DD format

    * **interval:**  Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Loading and preprocessing the data


```r
dat <-  data.frame(read.table("~/Documents/Coursera/ReproR/GitTest/RepData_PeerAssessment1/activity.csv", header=TRUE, sep=","))
## for testing the git hub
d <- na.omit(dat);
plot(d);
```

![](Repro_P1_files/figure-html/Load_initEx-1.png) 


## What is mean total number of steps taken per day?



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
