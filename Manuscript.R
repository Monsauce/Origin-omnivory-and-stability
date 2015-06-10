#R script to run analysis and generate figures for "Omnivory and stability in an experimental food web"
library(RCurl)

####Snail analyses####
#read in Snail data from GitHub
snail.URL <- getURL("https://raw.githubusercontent.com/Monsauce/Size-does-matter-/master/Snail.csv")
snail<-read.csv(text=snail.URL)

#pull out only crayfish treatments and order factors
snail.subset<-snail[snail$Species%in%c("Limosus", "Propinquus","Rusticus ","Virilis "),]
snail.subset$Species<- factor(snail.subset$Species, levels=c("Limosus","Rusticus ", "Propinquus", "Virilis "))

#remove day 49 is missing a value for mesocosm 36
snail.subset<-snail.subset[snail.subset$Day!=49,]

#pool across replicates
snail.subset.mean<-ddply(.data=snail.subset, .variables=.(Day, Treatment, Species, Origin), .fun= summarise, Mean = mean(Density))


#plot Figure S1 
Figure.S1<-ggplot(snail.subset.mean, aes(x = Day, y = Mean))+geom_point()+facet_grid(Treatment~Species)+
  stat_smooth(se=F, colour="black", size=1)+
  theme_minimal()


#calculate LD50 for each species and treatment 
library(MASS)

#Limosus, Omnivore 
LO.snail<-snail.subset[snail.subset$Species=="Limosus" & snail.subset$Treatment %in% "Omnivore",] 
attach(LO.snail)
a=cbind(Alive,Consumed)
LO.snail.output<-glm(a~Day, family=binomial, by="Replicate")

dose.p(LO.snail.output,p=0.5)


#Rusticus, Omnivore 
RO.snail<-snail.subset[snail.subset$Species=="Rusticus " & snail.subset$Treatment %in% "Omnivore",] 
RO.snail.mean<-ddply(.data=RO.snail, .variables=.(Day, Treatment, Species), .fun= summarise, Alive = mean(Alive), Consumed=mean(Consumed))
attach(RO.snail.mean)
b=cbind(Alive,Consumed)
RO.snail.output<-glm(b~Day, family=binomial)

dose.p(RO.snail.output,p=0.5)












