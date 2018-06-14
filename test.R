#Cleaning the environment
rm(list = setdiff(ls(), lsf.str()))
#installing required palcahges
wants <- c("ggplot2","dplyr","plotly")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
for (pkg in wants) {library(pkg, character.only = TRUE)}
#cleaning the console
clc <- function() cat(rep("\n", 50))
clc()



activity <- read.csv("data/activity.csv")
activity$date <- as.Date(activity$date)
head(activity)
str(activity)


total_day <- activity %>% group_by(date) %>%summarise(total_steps=sum(steps,na.rm=TRUE),na=mean(is.na(steps)))
head(total_day)
ggplot(data=total_day, aes(x=date, y=total_steps)  )  +   geom_bar(stat="identity") + 
  geom_hline(yintercept = median(total_day$total_steps), color="blue") + 
  geom_hline(yintercept = mean(total_day$total_steps)  , color="red") +
  annotate("text", x = as.Date(min(total_day$date)), y = median(total_day$total_steps)+500 ,label = "Median", color="blue") +
  annotate("text", x = as.Date(min(total_day$date)), y = mean(total_day$total_steps)+500 ,label = "Mean", color="red")   


 ggplot(data=total_day, aes(total_steps)  )  +  geom_histogram() + labs(x = "total steps per day") +
 geom_vline(xintercept = mean(total_day$total_steps), color="blue")  

 
 mean_steps <- mean(total_day$total_steps,na.rm=TRUE)
 median_steps <- median(total_day$total_steps,na.rm=TRUE) 
 
 
 daily_patterns <- activity %>% group_by(interval) %>% summarise(average=mean(steps,na.rm=TRUE)) 
 ggplot(data=daily_patterns, aes(x=interval, y=average)) + geom_line(stat = "identity")
 
 
 
 max_numb_steps_interval <- max(daily_patterns$average)
 
 
 
 
 
 na_number <- sum(is.na(activity$steps))
 without_NAs <- numeric(nrow(activity))
 for (i in 1:nrow(activity))
 {
   if (is.na(activity[i,"steps"])==TRUE)
   {
     without_NAs[i]<-filter(daily_patterns,interval==activity[i,"interval"]) %>% select(average)
   } 
   else
   {
     without_NAs[i]<-activity[i,"steps"]
   }
   
 }
 activity_without_NAs<-mutate(activity,steps_no_NAs=without_NAs)
 head(activity_without_NAs)
 
 
 
 
 
 
 total_day_noNAs <- activity_without_NAs %>% mutate(steps_no_NAs=as.numeric(steps_no_NAs)) %>% group_by(date) %>% summarise(total_steps=sum(steps_no_NAs))
 ggplot(data=total_day_noNAs, aes(total_steps)  )  +  geom_histogram() + labs(x = "total steps per day") +
geom_vline(xintercept = mean(total_day_noNAs$total_steps), color="blue")  
 
 
 
 
 activity_without_NAs <- mutate(activity_without_NAs, WeekDay= weekdays(date))  
 activity_without_NAs <- mutate(activity_without_NAs, WeekEnd= (activity_without_NAs$WeekDay=="Saturday" | activity_without_NAs$WeekDay=="Sunday"))  
 total_day_noNAs_grouped <- activity_without_NAs  %>% group_by(interval,WeekEnd) %>% summarise(average=mean(as.numeric(steps_no_NAs)) )
 total_day_noNAs_grouped
 labels <- c('FALSE' = "Weekdays", 'TRUE' = "Weekend")
 ggplot(data=total_day_noNAs_grouped, aes(x=interval, y=average)) + geom_line(stat = "identity")+ facet_grid(WeekEnd~.~., labeller=labeller(WeekEnd = labels)) 
 
 
 
 

with(total_day_noNAs,(WeekDay=="Saturday" | WeekDay=="Sunday"))
(total_day_noNAs$WeekDay=="Saturday" | total_day_noNAs$WeekDay=="Sunday")
a <- mutate(total_day_noNAs, WeekEnd= (if (WeekDay=="Saturday" | WeekDay=="Sunday") {1} else {0}))  