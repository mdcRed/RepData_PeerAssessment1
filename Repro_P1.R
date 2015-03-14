# Title:  ToothGrowth data analysis
# Class:  Statistical Inference (Coursera.org)
# Name:   My D. Coyne


# Set working directory

setwd("~/Documents/Coursera/ReproR/GitTest/RepData_PeerAssessment1")

# Load packages in order to generate pdf documentation afterwards
require(knitr)
require(markdown)

# Create .md, .html, and .pdf files
knit("Repro_P1.Rmd")
knit2html("Repro_P1.Rmd", "Repro_P1.html")
#markdownToHTML('TG_.Rmd', 'TG_.html', options=c("use_xhml"))
system("pandoc -s Repro_P1.html -o Repro_P1.pdf")

## Loading and preprocessing the data
dat <-  data.frame(read.table
                   ("~/Documents/Coursera/ReproR/GitTest/RepData_PeerAssessment1/activity.csv"
                    , header=TRUE, sep=","))
## Initial exploratory of the data set
## remove data with NA's

df <- na.omit(dat);
nrow(df)

## Number of days spanned
tDays <- unique(dat$date)
length(timeSpan)

## Number of days spanned without NA's
t <- unique(d$date)
length(t)


## What is mean total number of steps taken per day?

# 1.  Calculate the total number of steps taken per day

totalStepsPerDay <-aggregate(x=df$steps,list(date=df$date), FUN=sum, na.rm=TRUE);

# 2.  Histogram of Total steps per day.  
library(ggplot2)
##
##h = hist(totalStepsPerDay$x
##         ,main="Histogram of total steps taken per day"
##         ,xlab="Total Steps"
##         ,ylab ="Frequency"
##         ,breaks = 20, density=20
##         ,col = "darkgoldenrod1", border = "blue")

## Using ggplot for better presentation
h <-ggplot(data=totalStepsPerDay, aes(x=x))+
          ggtitle("Histogram of total steps per day (binwidth=500)") + 
          xlab("Total Steps")
h + geom_histogram(binwidth=500, aes(fill = ..count..)) +
    scale_fill_gradient("Count", low="#fccde5", high="#ae017e")



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?




