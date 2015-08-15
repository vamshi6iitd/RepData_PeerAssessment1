##Peer Assessment 1 Submission

###Section I : Loading and preprocessing the data

Show any code that is needed to

**1.Load the data (i.e. read.csv())**

*Solution:*
```{r}
activity<-read.csv("activity.csv",header = T,stringsAsFactors = F)
```

**2.Process/transform the data (if necessary) into a format suitable for your analysis**

*Solution:*
```{r}
activity$dayofweek<-format(as.Date(activity$date),"%A")
```

**3.What is mean total number of steps taken per day?**

*Solution:*
```{r}
sum(activity$steps,na.rm = T)/length(unique(activity$date))
```

### Section II : For this part of the assignment, you can ignore the missing values in the dataset.


**1.Calculate the total number of steps taken per day**

*Solution:*
```{r}
activity_fil<-activity[!is.na(activity$steps),]
daywiseSteps<-tapply(activity_fil$steps,activity_fil$date,sum)
```

**2.If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day**

*Solution:*
```{r}
hist(daywiseSteps,breaks=30,lwd=2,col="skyblue",xlab = "Number of Steps",ylab = "Number of Days",main = "Histogram of Steps taken per day")
```

**3.Calculate and report the mean and median of the total number of steps taken per day**

*Solution:*
```{r}
mean(daywiseSteps)
median(daywiseSteps)
```
### Section III : What is the average daily activity pattern?

**1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)**

*Solution:*
```{r}
intervalSteps<-tapply(activity_fil$steps,activity_fil$interval,mean)
plot(as.numeric(names(intervalSteps)),intervalSteps,type="l",col="red",xlab = "Interval",ylab = "Average number of steps",main = "Average number of steps over 5 minute intervals of a day")
```

**2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?**

*Solution:*
```{r}
names(intervalSteps[intervalSteps==max(intervalSteps)])
```

### Section IV : Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

**1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)**

*Solution:*
```{r}
summary(is.na(activity))
```

From the above summary, we can infer that there are **ONLY** 2304 rows in which value of "steps" is missing.

**2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.**

*Solution:*

```{r}
replValue<-function(x){
        as.numeric(intervalSteps[names(intervalSteps)==x])
}
```

A function that outputs the average steps of the interval which we input.

```{r}
replacementValues<-sapply(activity[is.na(activity$steps),3],replValue)
head(replacementValues)
```

Showing head of the list of step values that can replace in the NAs, obtained through the replValue function generated in the preceding question.

**3.Create a new dataset that is equal to the original dataset but with the missing data filled in.**

*Solution:*
```{r}
activity_repl<-activity
activity_repl$steps[is.na(activity_repl$steps)]<-replacementValues
head(activity_repl)
head(activity)
```

New dataset activity_repl has been created with replaced 'steps' values as we can see from the comparision of activity repl and activity data frames.

**4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?**

*Solution:*
```{r}
daywiseSteps2<-tapply(activity_repl$steps,activity_repl$date,sum)
hist(daywiseSteps2,breaks=30,lwd=2,col="skyblue",xlab = "Number of Steps",ylab = "Number of Days",main = "Histogram of Steps taken per day")
mean(daywiseSteps2)
median(daywiseSteps2)
```

No there is NO significant difference between the histogram, and mean of the daywise steps because of imputting the missing values.

But, median has changed from 10765 to 10766.19 as we have replaced a large number of NAs by average values of which 10766.19 is one. 

### Section V : Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

**1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.**

*Solution:*
```{r}
activity_repl$weekcat<-"weekday"
activity_repl$weekcat[activity_repl$dayofweek %in% c("Saturday","Sunday")]<-"weekend"
activity_repl$weekcat<-as.factor(activity_repl$weekcat)
summary(activity_repl$weekcat)
```

**2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.**

*Solution:*
```{r}
library(reshape2)
library(ggplot2)
data<-melt(activity_repl,id.vars=c("weekcat","interval"),measure.vars=c("steps"))
datacast<-dcast(data,weekcat+interval~variable,mean)
ggplot(datacast,aes(interval,steps))+facet_grid(weekcat~.)+geom_line(col="blue")+xlab("Time Interval")+ylab("Average number of Steps")+ggtitle("Comparision of average number of steps over weekdays vs weekend")
```

##                      THE END
