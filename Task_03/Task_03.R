#Task 03 II
trueMean1<-5
trueSD1<-5
population1<-rnorm(1e6, trueMean1, trueSD1)

trueMean2<-4
trueSD2<-5
population2<-rnorm(1e6, trueMean2, trueSD2)

Size<-50
Sample1<-sample(population1, Size)
Sample2<-sample(population2, Size)

boxplot(Sample1, Sample2)

#III

source("http://jonsmitchell.com/code/simFxn04.R")
MatGrandma<-makeFounder("grandma_mom")
MatGrandpa<-makeFounder("grandpa_mom")
PatGrandma<-makeFounder("grandma_da")
PatGrandpa<-makeFounder("grandpa_da")
#MatGranndma
#head(MatGrandma)
#nrow(MatGrandma)
#PatGrandpa
#head(PatGrandpa)
#nrow(PatGrandpa)

Alan<-makeBaby(PatGrandma, PatGrandpa)
#Alan
#head(Alan)
#nrow(Alan)
Brenda<-makeBaby(MatGrandma, MatGrandpa)
Focus<-makeBaby(Brenda,Alan)
#head(Focus)
ToMom<-length(grep("mom", Focus))/length(Focus)
#ToMom
ToMomMom<-length(grep("grandma_mom", Focus))/length(Focus)
#The number of shared genes should be ~0.25
ToMomMom
#it's a little less than that

ToMomDad<-length(grep("grandpa_mom", Focus))/length(Focus)
ToMomDad                 
ToDadMom<-length(grep("grandma_da", Focus))/length(Focus)
ToDadDad<-length(grep("grandpa_da", Focus))/length(Focus)
ToDadMom
ToDadDad

mean(c(ToMomMom,ToMomDad,ToDadDad,ToDadMom))

Sibling_01<-makeBaby(Brenda,Alan)
ToSib<-length(intersect(Focus,Sibling_01))/length(Focus)
#The number of shared genes should be ~0.5
ToSib 
#it's a litte more than that

ManySiblings<-replicate(1e3,length(intersect(Focus, makeBaby(Brenda, Alan)))/length(Focus))
?quantile
quantile(ManySiblings)
mean(ManySiblings)

plot(density(ManySiblings), main="", 
     xlab="proportion shared genes")

#Task 03 IV

HWE<-function(p) {
  aa<-p^2
  ab<-2*p*(1-p)
  bb<- (1-p)^2
  return (c(aa=aa,ab=ab,bb=bb))
}

HWE(0.5)

plot(1,1, type="n", xlim=c(0,1), ylim=c(0,1),
      xlab="freq. allele a", ylab="geno. freq.")

p<-seq(from=0, to=1, by=0.01)
GenoFreq<-t(sapply(p,HWE))

lines(p, GenoFreq[,"aa"], lwd=2,col="red")
lines(p, GenoFreq[,"ab"], lwd=2, col="purple")
lines(p, GenoFreq[,"bb"],lwd=2, col="blue")

legend("top",legend=c("aa","ab","bb"), col=c("red","purple","blue"), lty=1, lwd=2,bty="n")

Pop<-simPop(500)
points(Pop[,"freqa"],Pop[,"Genotypes.aa"]/500, pch=21,bg="red")
#Yes, the frequency matches the expectation
Pop<-simPop(50)

points(Pop[,"freqa"],Pop[,"Genotypes.aa"]/50, pch=22,bg="red")

#Task 03 V
library(learnPopGen)
x<-genetic.drift(Ne=200,nrep=5,pause=0.01)
PopSizes<-5:50
Samples<-rep(PopSizes,5)
tExt<-sapply(Samples,function(x) nrow(simPop(x, 500)))
#tExt
Line<-lm(tExt~Samples)
summary(Line)
Line$coef
plot(Samples,tExt)
abline(Line)
Line2<-lm(tExt~Samples+0)
summary(Line2)
summary(Line)
#Line2$coef
#Line$coef
#Line 2 doesn't show an intercept
#?lm()
#lm(y ~ 0 + x) "removes the implied intercept term"

#there are a lot of outlying toward the upper right of the plot
#and the distance of the points from the line increases with population size
#as the population grows, there is a greater chance that an allele will remain in the population?



# Task 03 Extra Credit
Line3<-rlm(tExt~Samples)
plot(Samples,tExt)
abline(Line3)
Line3$coef
summary(Line3)
#summary(Line)
#Slope for Line is larger than the slope for Line3.
#so the robust model is correcting for the extreme points and making the line more representative

