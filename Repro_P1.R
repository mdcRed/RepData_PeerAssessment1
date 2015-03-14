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
length(tDays)

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

# 3. Calculate mean and median of the total steps taken per day

mu <- mean (totalStepsPerDay$x)
mu

med <- median (totalStepsPerDay$x)
med


## What is the average daily activity pattern?

## Plot of times series for all days
#timePlot <- ggplot(data=df, aes(x = date, y = steps, group=1, colour = "green")) +
#            theme_bw() +
#            theme(panel.grid.major = element_blank())
         
#timePlot <- timePlot + geom_line()
#timePlot

## Calcualte average steps takeen per day
library(scales)
aveStepsPerInterval <-aggregate(x=df$steps,list(Interval=df$interval), FUN=mean, na.rm=TRUE);

x_breaks <- c(0,55,100,155,200,255,300,355,400, 455, 500, 555,
              600, 655, 700, 755, 800, 855, 900, 955, 1000, 1055, 
              1100, 1155, 1200, 1255, 1300, 1355, 1400, 1455, 1500, 1555, 
              1600, 1655, 1700, 1755, 1800, 1855, 1900, 1955, 2000, 2055,
              2100,2155,2200,2255,2300,2355)
 
avePlot <- ggplot(data=aveStepsPerInterval, aes(x = Interval, y = x, group=1)) +
  ggtitle("Average steps taken per interval, across all observed days") + 
  xlab("Interval") +
  ylab("Average Steps Per Interval") +
  scale_x_discrete( breaks=x_breaks) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


avePlot <- avePlot + 
           geom_point(aes(colour = x)) +
           geom_line(color="#41b6c4")
avePlot

# Calulate the max point
mx <- round(max(aveStepsPerInterval$x));
indx <- which(aveStepsPerInterval$x==max(aveStepsPerInterval$x));
mxInterval <- aveStepsPerInterval$Interval[indx]
mxInterval

## Find maxinum number of st




## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?




