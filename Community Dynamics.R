library(readr)
webforum_3_ <- read_csv("C:/Users/asus/Downloads/webforum (3).csv")
w = webforum_3_
View(w)

#loads libraries
install.packages("ggplot2")
library(ggplot2)
library(scales)
install.packages("plyr")
library(plyr)
install.packages("gridExtra")
library(gridExtra)
install.packages("lubridate")
library(lubridate)
install.packages("ggpubr")
library(ggpubr)
install.packages("tidyverse")
library(tidyverse)
install.packages("Hmisc")
library(Hmisc)
install.packages("corrplot")
library(corrplot)
install.packages("rlang", dependencies = TRUE)
library(rlang)
install.packages('Rcpp', dependencies = TRUE)
install.packages('ggplot2', dependencies = TRUE)
install.packages('data.table', dependencies = TRUE)
install.packages("reshape2")

#Remove anonymous authors
w=subset(w,AuthorID!="-1")
View(w)

#Formatting the date
w$Date = as.Date(w$Date)
ggplot(w, aes(x=Date, colour="blue") + geom_histogram(binwidth=30, colour="white") +
         scale_x_date(labels = date_format("%Y-%b"),
                      breaks = seq(min(w$Date)-5, max(w$Date)+5, 30),
                      limits = c(as.Date("2002-01-16"), as.Date("2011-12-31"))) +
         ylab("Frequency") + xlab("Year and Month") + theme_dark()+
         theme(axis.text.x=element_text(angle=90, hjust=1)))

#remove posts with 0 words
w=subset(w,w$WC!="0")

#Histogram plot of number of entries by day 2002-01-16 Until 2011-12-31
hist(w$Date, breaks = 30, main="Number of Entries Over Time", xlab="Time")

#Narrowing the the scope to only 2005
w2006=w[which(w$Date>'2005-01-01' & w$Date <'2005-12-31'),]

#Histogram plot of number of entries by day in 2005
hist(w2006$Date, breaks=30, freq=TRUE, main="Number of Entries in 2005", xlab="Time")

#Finding the most active thread
threadFreq=count(w, "ThreadID")

#Only looking at threads that have over 335 posts
manyPostThread= threadFreq[threadFreq$freq>335,]
View(manyPostThread)

#Show the full data for the thread with more than 335 posts
topThreadsPOsts= subset(w, ThreadID %in% manyPostThread$ThreadID)
topThreadsPOsts
View(topThreadsPOsts)

#subset of data before highest populated day
wfPrePeak = w[which(w$Date > '2004-10-01' & w$Date < '2005-10-01'),]
hist(wfPrePeak$Date, main = "Year before Peak", xlab = "Day", ylab = "Amount of Posts",
     freq = TRUE, breaks = 30)
wc = wfPrePeak[,6]
analytic = wfPrePeak[,7]
clout = wfPrePeak[,8]
authentic = wfPrePeak[,9]
tone = wfPrePeak[,10]
ppron = wfPrePeak[,11]
i = wfPrePeak[,12]
we = wfPrePeak[,13]
you = wfPrePeak[,14]
shehe= wfPrePeak[,15]
they= wfPrePeak[,16]
number= wfPrePeak[,17]
affect= wfPrePeak[,18]
posemo = wfPrePeak[,19]
negemo = wfPrePeak[,20]
anx = wfPrePeak[,21]
anger= wfPrePeak[,22]
social= wfPrePeak[,23]
family= wfPrePeak[,24]
friend = wfPrePeak[,25]
work= wfPrePeak[,26]
leisure= wfPrePeak[,27]
home = wfPrePeak[,28]
money= wfPrePeak[,29]
relig= wfPrePeak[,30]
swear= wfPrePeak[,31]
QMark= wfPrePeak[,32]
df = data.frame(wc, analytic, clout, authentic, tone, ppron, i, we, you, shehe, they, number,
                affect, posemo, negemo, anx,
                anger, social, family, friend, work, leisure, home, money, relig, swear, QMark)
cor_1 = round(cor(df), 3)
cor_1
View(cor_1)

#Calculates significance levels for the correlations
cor_2 = rcorr(as.matrix(df))
cor_2

# Creates a correlogram with the significance test
M = cor_2$r
p_mat = cor_2$P
corrplot(M, method="color", type= "lower",
         tl.col = "darkblue", tl.srt = 45,
         p.mat = p_mat, sig.level = 0.01,
         title = "Correlations in the year before the peak",
         mar=c(0,0,1,0))

#subset of data after highest populated day
wfPostPeak = w[which(w$Date > '2005-10-02' & w$Date < '2006-10-02'),]
hist(wfPostPeak$Date, main = "Month after Peak", xlab = "Day", ylab = "Amount of Posts",
     freq = TRUE, breaks = 30)
wc = wfPostPeak[,6]
analytic = wfPostPeak[,7]
clout = wfPostPeak[,8]
authentic = wfPostPeak[,9]
tone = wfPostPeak[,10]
ppron = wfPostPeak[,11]
i = wfPostPeak[,12]
we = wfPostPeak[,13]
you = wfPostPeak[,14]
shehe= wfPostPeak[,15]
they= wfPostPeak[,16]
number= wfPostPeak[,17]
affect= wfPostPeak[,18]
posemo = wfPostPeak[,19]
negemo = wfPostPeak[,20]
anx = wfPostPeak[,21]
anger= wfPostPeak[,22]
social= wfPostPeak[,23]
family= wfPostPeak[,24]
friend = wfPostPeak[,25]
work= wfPostPeak[,26]
leisure= wfPostPeak[,27]
home = wfPostPeak[,28]
money= wfPostPeak[,29]
relig= wfPostPeak[,30]
swear= wfPostPeak[,31]
QMark= wfPostPeak[,32]
df = data.frame(wc, analytic, clout, authentic, tone, ppron, i, we, you, shehe, they, number,
                affect, posemo, negemo, anx,
                anger, social, family, friend, work, leisure, home, money, relig, swear, QMark)
cor_1 = round(cor(df), 3)
cor_1

#Calculates significance levels for the correlations
cor_2 = rcorr(as.matrix(df))
cor_2

# Creates a correlogram with the significance test
M = cor_2$r
p_mat = cor_2$P
corrplot(M, method="color", type= "lower",
         tl.col = "darkblue", tl.srt = 45,
         p.mat = p_mat, sig.level = 0.01, title = "Correlations in the year after the peak",
         mar=c(0,0,1,0))

#Finds correlations between data fields in the whole dataset
wc = w[,6]
analytic = w[,7]
clout = w[,8]
authentic = w[,9]
tone = w[,10]
ppron = w[,11]
i = w[,12]
we = w[,13]
you = w[,14]
shehe= w[,15]
they= w[,16]
number= w[,17]
affect= w[,18]
posemo = w[,19]
negemo = w[,20]
anx = w[,21]
anger= w[,22]
social= w[,23]
family= w[,24]
friend = w[,25]
work= w[,26]

leisure= w[,27]
home = w[,28]
money= w[,29]
relig= w[,30]
swear= w[,31]
QMark= w[,32]
df = data.frame(wc, analytic, clout, authentic, tone, ppron, i, we, you, shehe, they, number,
                affect, posemo, negemo, anx,
                anger, social, family, friend, work, leisure, home, money, relig, swear, QMark)
cor_1 = round(cor(df), 3)
cor_1

#Calculates significance levels for the correlations
cor_2 = rcorr(as.matrix(df))
cor_2

# Creates a correlogram with the significance test
M = cor_2$r
p_mat = cor_2$P
corrplot(M, method="color", type= "lower",
         tl.col = "darkblue", tl.srt = 45,
         p.mat = p_mat, sig.level = 0.01 , title = "Correlations for the whole dataset",
         mar=c(0,0,1,0))
as.date(w$Date)

#find correlations in the first year
WStart = w[which(w$Date > '2002-01-01' & w$Date < '2002-12-30'),]
wc = WStart[,6]
analytic = WStart[,7]
clout = WStart[,8]
authentic = WStart[,9]
tone = WStart[,10]
ppron = WStart[,11]
i = WStart[,12]
we = WStart[,13]
you = WStart[,14]
shehe= WStart[,15]
they= WStart[,16]
number= WStart[,17]
affect= WStart[,18]
posemo = WStart[,19]
negemo = WStart[,20]
anx = WStart[,21]
anger= WStart[,22]
social= WStart[,23]
family= WStart[,24]
friend = WStart[,25]
work= WStart[,26]
leisure= WStart[,27]
home = WStart[,28]
money= WStart[,29]
relig= WStart[,30]
swear= WStart[,31]
QMark= WStart[,32]
df = data.frame(wc, analytic, clout, authentic, tone, ppron, i, we, you, shehe, they, number,
                affect, posemo, negemo, anx,
                anger, social, family, friend, work, leisure, home, money, relig, swear, QMark)
cor_1 = round(cor(df), 3)
cor_1

#Calculates significance levels for the correlations
cor_2 = rcorr(as.matrix(df))
cor_2

# Creates a correlogram with the significance test
M = cor_2$r
p_mat = cor_2$P
corrplot(M, method="color", type= "lower",
         tl.col = "darkblue", tl.srt = 45,
         p.mat = p_mat, sig.level = 0.01 , title = "Correlations for 2002",
         mar=c(0,0,1,0))
#find correlations in the last year
WEnd = w[which(w$Date > '2011-01-01' & w$Date < '2011-12-30'),]
wc = WEnd[,6]
analytic = WEnd[,7]
clout = WEnd[,8]
authentic = WEnd[,9]
tone = WEnd[,10]
ppron = WEnd[,11]
i = WEnd[,12]
we = WEnd[,13]
you = WEnd[,14]
shehe= WEnd[,15]
they= WEnd[,16]
number= WEnd[,17]
affect= WEnd[,18]
posemo = WEnd[,19]
negemo = WEnd[,20]
anx = WEnd[,21]
anger= WEnd[,22]
social= WEnd[,23]
family= WEnd[,24]
friend = WEnd[,25]
work= WEnd[,26]
leisure= WEnd[,27]
home = WEnd[,28]
money= WEnd[,29]
relig= WEnd[,30]
swear= WEnd[,31]
QMark= WEnd[,32]
df = data.frame(wc, analytic, clout, authentic, tone, ppron, i, we, you, shehe, they, number,
                affect, posemo, negemo, anx,
                anger, social, family, friend, work, leisure, home, money, relig, swear, QMark)
cor_1 = round(cor(df), 3)
cor_1
#Calculates significance levels for the correlations
cor_2 = rcorr(as.matrix(df))
cor_2
# Creates a correlogram with the significance test
M = cor_2$r
p_mat = cor_2$P
corrplot(M, method="color", type= "lower",
         tl.col = "darkblue", tl.srt = 45,
         p.mat = p_mat, sig.level = 0.01 , title = "Correlations for 2011",
         mar=c(0,0,1,0))

#for the line graphs the code was edited for different subsets rather than creating another set
of code for each graph
#Plot Start, End and Summary graphs
WEnd$monthPosts = substr(WEnd$Date,6,7)
fortime=aggregate(WEnd[6:10],WEnd[33],mean)
fortime = melt(fortime, id.vars='monthPosts')
fortimeplot=ggplot(fortime, aes(x=monthPosts, y=value, colour=variable,
                                group=variable))+geom_line() + ggtitle("LIWC End", NULL)
WStart$monthPosts = substr(WStart$Date,6,7)
fortime=aggregate(WStart[6:10],WStart[33],mean)
fortime = melt(fortime, id.vars='monthPosts')
fortimeplot=ggplot(fortime, aes(x=monthPosts, y=value, colour=variable,
                                group=variable))+geom_line() + ggtitle("LIWC Start", NULL)
WSummary$monthPosts = substr(WSummary$Date,6,7)
fortime=aggregate(WSummary[6:10],WSummary[33],mean)
fortime = melt(fortime, id.vars='monthPosts')
fortimeplot=ggplot(fortime, aes(x=monthPosts, y=value, colour=variable,
                                group=variable))+geom_line() + ggtitle("LIWC Summary", NULL)
Fortimeplot
#Plot graph for thread with most posts
WStart = W[which(W$Date > '2002-01-01' & W$Date < '2002-12-30'),]
WEnd = W[which(W$Date > '2011-01-01' & W$Date < '2011-12-30'),]
CommonThread = subset(W, W$ThreadID == "252620")
CommonThread$FollowingPost = CommonThread$Date-min(CommonThread$Date)
CommonThread = aggregate(CommonThread, CommonThread[34],mean)
CommonThread$PostID = NULL
CommonThread$ThreadID = NULL
CommonThread$AuthorID = NULL
CommonThread$Date = NULL
CommonThread$Time = NULL
CommonThread$postHour = NULL
CommonThread$monthYear = NULL
CommonThread[11:34] = NULL
CommonThread[1:5] = NULL
CommonThread30days=melt(CommonThread,id.vars='FollowingPost')
CommonThread30daysPlot=ggplot(CommonThread30days,aes(x=FollowingPost,y=value,co
                                                     lour=variable,group=variable))+geom_line()+ggtitle("LIWC Following Post")
CommonThread30daysPlot