library(learnPopGen)
?coalescent.plot
coalescent.plot()
coalescent.plot()
coalescent.plot()
#each simulation begins with 10 alleles, this can be changed with the n= argument
#the average time to fixation for the three simulations I ran was 10 generations
#the average number of offspring was 6.9, with a variance of 3.03
#fitness doesn't factor into these simulations
#

#install.packages("coala")
#install.packages("rehh", dep=T)
#install.packages("assertthat", dep=T)
#install.packages("RcppArmadillo", dep=T)
#install.packages("https://cran.r-project.org/src/contrib/Archive/scrm/scrm_1.7.3-1.tar.gz", repos=NULL, type="source")
#install.packages("https://cran.r-project.org/src/contrib/Archive/coala/coala_0.6.0.tar.gz", repos=NULL, type="source")
#install.packages("coala")
#install.packages("phytools")
library(coala)
library(phytools)

model<-coal_model(sample_size=5,loci_number=10,loci_length=500,ploidy=2)+ feat_mutation(10)+
  feat_mutation(10)+
  feat_recombination(10)+
  sumstat_trees()+
  sumstat_nucleotide_div()
stats <-simulate(model, nsim=1)
Diversity<-stats$pi
#Diversity
#the numbers are all different, the differences are caused by mutation and recombination

Nloci<-length(stats$trees)
t1<-read.tree(text=stats$trees[[1]][1])
plot(t1)
axisPhylo()
#the number of tips represents a locus, since each individual is diploid, the number of 
#tips (loci) is twice the number of individuals

Age1<- max(nodeHeights(t1))
t2<-read.tree(text=stats$trees[[2]][1])
plot(t2)
axisPhylo()

#the most recent common ancestor for t2 is much younger than for t1
Age2<- max(nodeHeights(t2))
#The most recent common ancestor for t2 is ~0.456, for t1 is ~1.981 

#there are a lot of differences between the two trees
par(mfrow=c(1,2))
plot(t1)
axisPhylo()
plot(t2)
axisPhylo()


compare.chronograms(t1,t2)

t1_1<-read.tree(text=stats$trees[[1]][1])
t1_2<-read.tree(text=stats$trees[[1]][2])
compare.chronograms(t1_1,t1_2)

for (locus in 1:Nloci) {
  ntrees<-length(stats$trees[[locus]])
  for (n in 1:ntrees) {
    if(locus==1 && n==1) {
      outPhy<- read.tree(text=stats$trees[[locus]][n])
    }
 else {
    outPhy<-ape:::c.phylo(outPhy, read.tree(text=stats$trees[[locus]][n]))
}
  }
    }

par(mfrow=c(1,1))
densityTree(outPhy)

model3<- coal_model(10,50) +
  feat_mutation(par_prior("theta", sample.int(100,1)))+
  sumstat_nucleotide_div()
stats<- simulate(model3,nsim=40)

mean_pi<-sapply(stats, function(x) mean(x$pi))
theta<-sapply(stats, function(x) x$pars[["theta"]])
plot(mean_pi,theta)
lm(theta~mean_pi)
abline(lm(theta~mean_pi))
