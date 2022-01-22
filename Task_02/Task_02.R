#This reads in the file off the internet
Data<-read.csv('http://jonsmitchell.com/data/beren.csv', stringsAsFactors = F)
#This saves the data to wd to look at in Excel
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

#Which events are bottles
Feeds<-which(Data[,9]=='bottle')
berenMilk<-Data[Feeds,]
head(berenMilk)
#How many rows? Each represents a bottle feed.
nrow(berenMilk)

Feeds<-which(Data[,'event']=='bottle')
head(Feeds)

Feeds<-which(Data$event=='bottle')
head(Feeds)
#both [] and the $ operator can be used to pull a variable from a dataset.
#using the $ operator to call a column, and brackets for [row, column] can achieve the same result

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
