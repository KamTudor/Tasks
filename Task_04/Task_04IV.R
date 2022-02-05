results<-(read.csv("http://jonsmitchell.com/data/biol112labresults.csv",stringsAsFactors=F))
write.csv(results,'biol112labresults.csv', quote=F)
counts<-results[,c("yellow","red","green","blue","black","tan")]
backgrounds<-c("White","Red","Yellow","Green","Blue","Black")
backgroundCol<-c("white","#d53e4f","#fee08b","#abdda4","#3288bd","black")
calcChi(counts[1,])
Chisqs<-apply(counts,1,calcChi)
plotChis(counts)

#par("mar")
#par(mar=c(1,1,1,1))

#when the chi^2 value is low, the observed values are even and near the expected
#as the values differ further from the expected, the chi-square value increases
#even though 5/6 values are the same on the x^2=300 plot, the observed values are 
#furthest from the expected values
#going back to the last plot, it seems like evenness doesn't matter if the values 
#are far from the expected
Avg<-mean(Chisqs)
Avg
#The average chi^2 value is much higher than the critical value, so there is a
#noticeable difference between the observed and expected values

backgroundAvgs<-tapply(Chisqs,results[3],mean)
backgroundAvgs
propSig<-length(which(Chisqs> 11.70))/length(Chisqs)
percSig<-round(100*propSig)
percSig
#92% having a significant p value doesn't really surprise me
#Probably not, I think a subconscious (or not) desire to see an obvious ratio
#change might effect the results 
#plus we're comparing the results to a "well behaved" simulation
#that felt like a spoiler

par(las=1,mar=c(4,4,1,1), mgp=c(2,0.5,0),tck=-0.01, cex.axis=1)
plot(1,1,xlim=c(0,400),ylim=c(1,8.5), xlab="", ylab="", type="n",yaxt="n")
axis(2, at=1:length(backgrounds), labels=(backgrounds))
mtext(side=1, expression(chi^2), cex=1.75, line=2.5)
counter<-1
for (i in backgrounds) {
  Data<-Chisqs[which(results[,3]==i)]
  addHist(Y=counter, Dat=Data, Color=backgroundCol[counter])
  counter<- counter+1
}
abline(v=11.70, lty=2, lwd=2, col="black")
#not a lot of difference between the backgrounds
#red had more insignificant trials than the others

#Part VI
Simulation<-simDraws(10000)
addHist(Y=7,Dat=Simulation,Color="lightgray")
mtext(side=2,at=7,line=0,"simulated")
abline(v=11.70,lty=2,lwd=2)

Fit<-c(1,1,1,1,1,1)
names(Fit)<-1:6
Simulation2<-simDraws(1e4, w=Fit)
addHist(Y=8,Dat=Simulation2, Color=rgb(0,0,0,0.25))

Fit<-c(0.1,1,1,1,1,1)
names(Fit)<-1:6
Simulation3<-simDraws(1e4,w=Fit)
addHist(Y=8, Dat=Simulation3, Color=rgb(0,0,0,0.25))

Fit<-c(0.5,0.6,0.7,1,1,1)
names(Fit)<-1:6
Simulation4<- simDraws(1e4, w=Fit)
addHist (Y=8,Dat=Simulation4, Color=rgb(0,0,0,0.25))

Fit<-c(0.1,0.2,0.3,0.4,0.5,1)
names(Fit)<-1:6
Simulation5<- simDraws(1e4, w=Fit)
addHist (Y=8,Dat=Simulation5, Color=rgb(0,0,0,0.25))

Fit<-c(0.1,0.1,0.1,0.1,0.1,1)
names(Fit)<-1:6
Simulation6<- simDraws(1e4, w=Fit)
addHist (Y=8,Dat=Simulation6, Color=rgb(0,0,0,0.25))
mtext(side=2,at=8,line=0,"sel. sim.")

Simulation7<-c(Simulation2,Simulation3,Simulation4,Simulation5,Simulation6)
addHist(Y=8, Dat=Simulation7, Color=rgb(0,0,1,0.25))

#I'm really not sure what to make of the results
#I was going to come to office hours but campus closed
#Since both the simulation and all the student data primarily had
#chi^2 values greater than the critical value, the observed numbers deviated from
#the expected numbers. So selection occurred in both the computer-ran labs and
#the human-ran labs. I think selection was slightly weaker overall in the human labs
#which, if true, isn't really what I expected

#I *think* mutation would increase chi^2 values
#But I can't get the extra credit part completed, so I'm not sure
