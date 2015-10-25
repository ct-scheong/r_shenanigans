#####################################################################################
# Title: Data Code for Correlation One Data Scientist Challenge
# Author: Ka Ieng Shery Cheong
# Contact: datatista@gmail.com
# Date: October 20, 2015

#####################################################################################
# Data Cleaning Phase

# set working directory. change accordingly.
# setwd("~/Desktop")
setwd("./data/")

# import apache log file
logfile <- read.table('access.log',sep="[")

# clean log file by setting delimiters
logfile$V2 <- gsub('] GET /click?article_id=', ',', logfile$V2, fixed=TRUE)
logfile$V2 <- gsub('&user_id=', ',', logfile$V2, fixed=TRUE)
logfile$V2 <- gsub(' HTTP/1.1 ', ',', logfile$V2, fixed=TRUE)
logfile$V2 <- gsub(' ', ',', logfile$V2, fixed=TRUE)

# separate file name by delimiter into separate columns
logfile2 <- strsplit(logfile$V2,",")
log.df <- data.frame(matrix(unlist(logfile2), nrow = length(logfile2),byrow=T))

# rename log file columns
cols <- c("time_logged","article_id","user_id","status_code","byte_size")
colnames(log.df) <- cols

# import database, file was exported from SQL
data.df <- read.csv("mydata.csv", sep=";",header=F)

# rename column names
cols2 <- c("content_id", "email_id", "user_id", "article_id", "author_id", "topic_id", "type_id", "submission_time", "topics_name", "type_name")
colnames(data.df) <- cols2

# truncate dates in both dataframes for easier formatting purposes
data.df$submission_time <- substr(data.df$submission_time, 1, 10)
log.df$time <- substr(log.df$time, 1, 11)

# convert dates in both dataframes to date/POSIXct objects
require(lubridate)
data.df$submission_time <- ymd(data.df$submission_time)
log.df$time <- dmy(log.df$time)

# check class types for both df
sapply(data.df, class)
sapply(log.df, class)

# convert integers to factor types for the database
nms <- c("content_id", "email_id","user_id","article_id","author_id","topic_id","type_id") 
data.df[nms] <- lapply(data.df[nms], as.factor) 

# merge the two db, so if a particular article has been opened by a user, it will have a status. Otherwise, it will be NA.
require(plyr)
full.df <- join(data.df,log.df,by=c("article_id","user_id"),match= "first")

# check count of statuses
summary(full.df$status_code)

# if status is not NA, we mark that article as opened ("1") by the user it was sent to; otherwise it is "0".
full.df <- within(full.df,
             clicked <- ifelse(is.na(status_code),0,1)
                )

# convert dependent variable to factor
full.df$clicked <- as.factor(full.df$clicked)

# check class type
sapply(full.df,class)

# save as a csv
# write.csv(full.df,file="full.csv")

############################################################################################################################
# Time series analysis Phase

# convert all submission times to week number
full.df$week <- week(full.df$submission_time)

# aggregate clicks by week
clicks_per_week<- aggregate(as.numeric(as.character(clicked)) ~ week , data = full.df , FUN = sum)
sent_per_week<- aggregate(as.numeric(as.character(clicked)) ~ week , data = full.df, FUN=length)

# combine to one df and rename

clicks.df <- cbind(sent_per_week,clicks_per_week[,2])
names(clicks.df)[names(clicks.df)=="as.numeric(as.character(clicked))"] <- "sent"
names(clicks.df)[names(clicks.df)=="clicks_per_week[, 2]"] <- "clicked"
clicks.df$click_rate <- clicks.df$clicked/clicks.df$sent

max(clicks.df$click_rate)
min(clicks.df$click_rate)

require(ggplot2)

# plot freq of both, by week
ggplot(clicks.df, aes(week)) + 
  geom_line(aes(y = sent, colour = "Sent")) + 
  geom_line(aes(y = clicked, colour = "Clicked")) + ggtitle("Weekly Sent vs. Clicked Article Links")

# plot click rate, by week
ggplot(clicks.df, aes(week)) + 
  geom_line(aes(y = click_rate, colour = "Click Rate")) + ggtitle("Weekly Overall Click Rates")

############################################################################################################################
# Data Visualization Phase

# plot the top 10 most frequent article types which are predicted to be clicked
par(las=1, mar=c(12,5,3,3))
barplot(sort(pred_clicked_table, decreasing=TRUE)[1:10],cex.names=.75,las=2,ylab="Freq",main="Top predicted articles types that attract user clicks")

# EDA
reduced.df <- full.df[,c("clicked","topics_name","type_name")]
reduced.df$article_type<- paste(reduced.df$topics_name,reduced.df$type_name)
reduced.table <- data.frame(table(reduced.df$article_type,reduced.df$click))

click.df <- reduced.df[reduced.df$clicked==1,]
click.summary <- count(click.df,c("type_name","topics_name"))
click.summary <- click.summary[order(-click.summary$freq),]

sent.summary <- count(reduced.df,c("type_name","topics_name"))
sent.summary <- sent.summary[order(-sent.summary$freq),]

# rename click freq and sent freq for easier distinguishment
names(click.summary)[names(click.summary)=="freq"] <- "click_freq"
names(sent.summary)[names(sent.summary)=="freq"] <- "sent_freq"

# merge both frequency tables
full.summary <- join(sent.summary,click.summary,by=c("type_name","topics_name"),match= "first")
full.summary$click_rate <- full.summary$click_freq/full.summary$sent_freq
full.summary$article_type<- paste(full.summary$topics_name,full.summary$type_name)

# Plot top 10 sent articles
#ggplot(data=reduced.table[order(-reduced.table$Freq),][1:10,], aes(x=Var1, y=Freq, fill=Var2)) + geom_bar(stat="identity")

# Find top 5 article types most frequently sent
top5_sent_articles <- full.summary[order(-full.summary$sent_freq),][1:5,"article_type"]

# check click rates for frequently sent articles
full.summary[order(-full.summary$sent_freq),][1:5,]

#subset all rows to just those in the top 5 and plot the sent frequency against their click freqency
reduced.df2 <- reduced.df[reduced.df$article_type %in% top5_sent_articles,]
qplot(article_type, data=reduced.df2, geom="bar", fill=factor(clicked)) + ggtitle("Top 5 article types most frequently sent")

# Find top 5 article types with highest click rate
top5_clicked_articles <- full.summary[order(-full.summary$click_rate),][1:5,"article_type"]

# check click rates for frequently sent articles
full.summary[order(-full.summary$click_rate),][1:5,]

# get overall click rate
aggregate(full.df$clicked, by=list(full.df$clicked), FUN=length)

#subset all rows to just those in the top 5 and plot the sent frequency against their click freqency
reduced.df3 <- reduced.df[reduced.df$article_type %in% top5_clicked_articles,]
qplot(article_type, data=reduced.df3, geom="bar", fill=factor(clicked)) + ggtitle("Top 5 article types with highest click rate")

#####################################################################################
# Modeling Phase

# inital dataset too big to run models, sample ~10% from full.db
sample.df <- full.df[sample(nrow(full.df), 100000), ]

# cluster users together who share similar attributes, for easier modelling purposes. tweak the number of centers to find optimal number.
user_group <- kmeans(sample.df[,c("clicked","topic_id","type_id","author_id")],10)
sample.df$usergroup <- user_group$cluster

# run naive bayes classifier to predict user click based on user id, author id, topic id, and type id.
require(e1071)
model_NB <- naiveBayes(clicked ~ usergroup + topic_id + type_id + week, data = sample.df)

# get the NB model probabilities for clicking and attach to the sample df
model.pred <- predict(model_NB,sample.df,type="raw")
sample.df$prob_NB <- model.pred[,2]

# convert NB probs to classification labels, try lower threshold from 0.5 to 0.4
sample.df$click_NB <- ifelse(sample.df$prob_NB > 0.4,1,0)

# cross freq table
table_NB <- table(sample.df$clicked,sample.df$click_NB)

# try adaboost
require(adabag)
model_AB <- boosting(clicked ~ usergroup + topic_id + type_id + week,data = sample.df,boos=TRUE,mfinal = 10,control = rpart.control(cp = -1))

# attach the model probabilities and class labels to the sample df
sample.df$prob_AB <- model_AB$prob[,2]
sample.df$click_AB <- model_AB$class

# cross freq table
table_AB <- table(sample.df$clicked,sample.df$click_AB)

# get avg probability of clicks, by usergroup, by week
weekly_click <- aggregate(prob_AB ~ usergroup + week, data = sample.df,FUN=mean)

# sort the sample df by user_group, then by descending Adaboost predicted prob
attach(sample.df)
sample.df.sorted <- sample.df[order(usergroup, -prob_AB),]
detach(sample.df)

# concatenate the topics and type to get the article type
sample.df.sorted$article_type<- paste(sample.df.sorted$topics_name,sample.df.sorted$type_name)

# among all the article types predicted as clicked by a user, sort by descending frequency
summary <- count(sample.df.sorted[sample.df.sorted$click_AB == "1",], c("article_type","usergroup"))
attach(summary)
summary <- summary[order(usergroup,-freq),]
detach(summary)

# summarize by frequency
require(data.table)
summary_top<-data.table(summary)
summary_top[,.SD[order(freq,decreasing=TRUE)[1]],by=usergroup]

# summarize by avg prob
avg_prob <- aggregate(prob_AB ~ usergroup + article_type, data = sample.df.sorted,FUN=mean)
avg_top <-data.table(avg_prob)
avg_top <- avg_top[,.SD[order(prob_AB,decreasing=TRUE)[1]],by=usergroup]
avg_top[order(usergroup)]

# create a df and related table for the predicted clicks
pred_clicked_articles<- sample.df.sorted[sample.df.sorted$click_AB == "1",]
pred_clicked_table <- table(pred_clicked_articles$article_type)
