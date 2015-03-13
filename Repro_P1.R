# Title:  ToothGrowth data analysis
# Class:  Statistical Inference (Coursera.org)
# Name:   My D. Coyne


# Set working directory

setwd("~/Documents/Coursera/Reproducible/RepData_PeerAssessment1")

# Load packages in order to generate pdf documentation afterwards
require(knitr)
require(markdown)

# Create .md, .html, and .pdf files
knit("Repro_P1.Rmd")
knit2html("Repro_P1.Rmd", "Repro_P1.html")
#markdownToHTML('TG_.Rmd', 'TG_.html', options=c("use_xhml"))
system("pandoc -s Repro_P1.html -o Repro_P1.pdf")

## Loading and preprocessing the data
dat <-  data.frame(read.table("~/Documents/Coursera/ReproR/GitTest/RepData_PeerAssessment1/activity.csv", header=TRUE, sep=","))
## for testing the git hub
d <- na.omit(dat);
plot(d);


## What is mean total number of steps taken per day?



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?




