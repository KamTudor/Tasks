#Task02a
#Part IV
Data<-read.csv('http://jonsmitchell.com/data/beren.csv', stringsAsFactors = F)
write.csv(Data, 'rawdata.csv', quote = F)

length(Data)
nrow(Data)
ncol(Data)
colnames(Data)
head(Data)

Data[1,]
Data[2,]
Data[1:3,]
Data[1:3, 4]
Data[1:5,1:3]

#How would you find the date of the 257th observation?
Data[257, 1:3]

#Task 02a Part V-i
Feeds<-which(Data[,9]=='bottle')
berenMilk<-Data[Feeds,]
head(berenMilk)

#How many rows: 332. Each represents a bottle feed.
nrow(berenMilk)

Feeds<-which(Data[,'event']=='bottle')
head(Feeds)

Feeds<-which(Data$event=='bottle')
head(Feeds)
#both [] and the $ operator can be used to pull a variable from a dataset.
#using the $ operator to call a column, and brackets for [row, column] can achieve the same result

#Task 02a V-ii
dayID<-apply(Data, 1, function(x) paste(x[1:3], collapse='-'))
dateID<-sapply(dayID, as.Date, format="%Y-%m-%d", origin="2019-04-18")
Data$age<-dateID-dateID[which(Data$event=='birth')]

head(Data)

beren2<-Data

beren3<-beren2[order(beren2$age),]

head(Data)
head(beren2)
head(beren3)

write.csv(beren3, 'beren_new.csv', quote = F, row.names = FALSE)

#Task 02b
setwd('C:\\Users\\Kambr\\Desktop\\Evolution\\Tasks\\Task_02')
#Question 1: Hypothesis I requires data that isn't present, Hypothesis II is extremely vague

#Task 02b Part IV

Feeds<-which(beren3$event=="bottle")
avgMilk<-mean(beren3$value[Feeds])

#Question 2: avgMilk is in ounces
#The value column contains the numbers associated with the units column, it is the number of ounces from each feed
#the brackets are used to call a specific data point, Feeds is a vector of the beren3 data points that are bottle feeds
#just calling beren3$value would give the value column from every event, it would mostly be NAs
avgFeed<-tapply(beren3$value[Feeds], beren3$age[Feeds], mean) 
varFeed<-tapply(beren3$value[Feeds], beren3$age[Feeds], var) 
totalFeed<-tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeed<-tapply(beren3$value[Feeds], beren3$age[Feeds], length) 

avgFeed
varFeed
totalFeed
numFeed

#Task 02b partV

cor(beren3$value[Feeds], beren3$age[Feeds])

cor.test(beren3$value[Feeds],beren3$age[Feeds])

berenCor<-cor.test(beren3$value[Feeds], beren3$age[Feeds])
summary(berenCor)

berenANOVA<-aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])
berenANOVA
boxplot(beren3$value[Feeds] ~ beren3$caregiver[Feeds], 
  xlab= "who gave the bottle", ylab= "amount of milk consumed (oz")

#Task 02b Part VI
?par
par(las=1, mar=c(5,5,1,1), mgp=c(2,0.5,0), tck=-0.01)

plot(as.numeric(names(totalFeed)), 
     totalFeed, type="b", pch=16, 
     xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col="red")     
dev.off()
