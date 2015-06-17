#R script to run analysis and generate figures for "Omnivory and stability in an experimental food web"
library(RCurl)
library(ggplot2)
library(plyr)

####Snail analyses####
#read in snail data from GitHub
snail.URL <- getURL("https://raw.githubusercontent.com/Monsauce/Size-does-matter-/master/Snail.csv")
snail<-read.csv(text=snail.URL)

#convert negatives to zeroes in Snail$Consumed 
snail$Consumed[snail$Consumed<0]<- 0

#pull out only crayfish treatments and order factors
snail.subset<-snail[snail$Species%in%c("Limosus", "Propinquus","Rusticus ","Virilis "),]
snail.subset$Species<- factor(snail.subset$Species, levels=c("Limosus","Rusticus ", "Propinquus", "Virilis "))

#remove day 49 is missing a value for mesocosm 36
snail.subset<-snail.subset[snail.subset$Day!=49,]

#pool across replicates
snail.subset.mean<-ddply(.data=snail.subset, .variables=.(Day, Trophic, Species, Origin), .fun= summarise, Mean = mean(Density))

#pool across replicates and day 
snail.subset.mean.day<-ddply(.data=snail.subset, .variables=.(Trophic, Species, Origin, Replicate), .fun= summarise, Mean = mean(Density))

#plot Figure S1 
Figure.S1<-ggplot(snail.subset.mean, aes(x = Day, y = Mean))+geom_point()+facet_grid(Trophic~Species)+
  stat_smooth(se=F, colour="black", size=1)+
  theme_minimal()


#calculate LD75 for each species and treatment 
library(MASS)

#Limosus, Omnivore 
LO.snail<-snail.subset[snail.subset$Species=="Limosus" & snail.subset$Trophic %in% "Omnivore",] 
attach(LO.snail)
model <- function(df){glm(cbind(Alive,Consumed)~Day, family=binomial, data = df)} 

LO.snail.model.outputs<-ddply(LO.snail, "Replicate", function(x){
  model.output <- model(x)
  df.out <- dose.p(model.output,p=0.25)
  return(df.out)
})  


#Limosus, Predator 
LP.snail<-snail.subset[snail.subset$Species=="Limosus" & snail.subset$Trophic %in% "Predator",] 
attach(LP.snail)
model <- function(df){glm(cbind(Alive,Consumed)~Day, family=binomial, data = df)} 

LP.snail.model.outputs<-ddply(LP.snail, "Replicate", function(x){
  model.output <- model(x)
  df.out <- dose.p(model.output,p=0.25)
  return(df.out)
})  


#Rusticus, Omnivore 
RO.snail<-snail.subset[snail.subset$Species=="Rusticus " & snail.subset$Trophic %in% "Omnivore",] 
attach(RO.snail)
model <- function(df){glm(cbind(Alive,Consumed)~Day, family=binomial, data = df)} 

RO.snail.model.outputs<-ddply(RO.snail, "Replicate", function(x){
  model.output <- model(x)
  df.out <- dose.p(model.output,p=0.25)
  return(df.out)
}) 


#Rusticus, Predator 
RP.snail<-snail.subset[snail.subset$Species=="Rusticus " & snail.subset$Trophic %in% "Predator",] 
attach(RP.snail)
model <- function(df){glm(cbind(Alive,Consumed)~Day, family=binomial, data = df)} 

RP.snail.model.outputs<-ddply(RP.snail, "Replicate", function(x){
  model.output <- model(x)
  df.out <- dose.p(model.output,p=0.25)
  return(df.out)
}) 


#Propinquus, Omnivore
PO.snail<-snail.subset[snail.subset$Species=="Propinquus" & snail.subset$Trophic %in% "Omnivore",] 
attach(PO.snail)
model <- function(df){glm(cbind(Alive,Consumed)~Day, family=binomial, data = df)} 

PO.snail.model.outputs<-ddply(PO.snail, "Replicate", function(x){
  model.output <- model(x)
  df.out <- dose.p(model.output,p=0.25)
  return(df.out)
}) 


#Propinquus, Predator
PP.snail<-snail.subset[snail.subset$Species=="Propinquus" & snail.subset$Trophic %in% "Predator",] 
attach(PP.snail)
model <- function(df){glm(cbind(Alive,Consumed)~Day, family=binomial, data = df)} 

PP.snail.model.outputs<-ddply(PP.snail, "Replicate", function(x){
  model.output <- model(x)
  df.out <- dose.p(model.output,p=0.25)
  return(df.out)
}) 


#Virilis, Omnivore
VO.snail<-snail.subset[snail.subset$Species=="Virilis " & snail.subset$Trophic %in% "Omnivore",] 
attach(VO.snail)
model <- function(df){glm(cbind(Alive,Consumed)~Day, family=binomial, data = df)} 

VO.snail.model.outputs<-ddply(VO.snail, "Replicate", function(x){
  model.output <- model(x)
  df.out <- dose.p(model.output,p=0.25)
  return(df.out)
}) 


#Virilis, Predator
VP.snail<-snail.subset[snail.subset$Species=="Virilis " & snail.subset$Trophic %in% "Predator",] 
attach(VP.snail)
model <- function(df){glm(cbind(Alive,Consumed)~Day, family=binomial, data = df)} 

VP.snail.model.outputs<-ddply(VP.snail, "Replicate", function(x){
  model.output <- model(x)
  df.out <- dose.p(model.output,p=0.25)
  return(df.out)
}) 


LD75.model.output <- rbind(LO.snail.model.outputs,RO.snail.model.outputs, PO.snail.model.outputs, VO.snail.model.outputs,
                           LP.snail.model.outputs, RP.snail.model.outputs, PP.snail.model.outputs, VP.snail.model.outputs)

LD75.model.output$Species<-snail.subset.mean.day$Species
LD75.model.output$Trophic<-snail.subset.mean.day$Trophic
colnames(LD75.model.output)[2] <- "LD75"



LD75.model.output.mean<-ddply(.data=LD75.model.output, .variables=.(Species, Trophic), .fun= summarise, Mean = mean(LD75), SE=sd(LD75)/sqrt(length(LD75)))

#plot Figure 2
Figure.2<-ggplot(LD75.model.output.mean, aes(x = Species, y = Mean, fill=Trophic))+
  geom_bar(stat = "identity",position="dodge")+xlab("Species")+ylab("Day when 75% of snails consumed")+
  scale_fill_manual(values=c("black", "grey"))+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE),width=.2,position=position_dodge(.9))+
  theme_minimal()

#run two-way ANOVA and TukeyHSD to determine differences between Species
ANOVA1<-aov(LD75~Species*Trophic, data=LD75.model.output)
TukeyANOVA1<-TukeyHSD(ANOVA1)

#Table S1
Table.S1<-TukeyANOVA1$Species

####Algae analyses####  
library(zoo)

#read in algae data from GitHub
algae.URL <- getURL("https://raw.githubusercontent.com/Monsauce/Size-does-matter-/master/Algae.csv")
algae<-read.csv(text=algae.URL)

#pull out only crayfish treatments and order factors
algae.subset<-algae[algae$Species%in%c("Limosus", "Propinquus","Rusticus ","Virilis "),]
algae.subset$Species<- factor(algae.subset$Species, levels=c("Limosus","Rusticus ", "Propinquus", "Virilis "))

#reduce spatial heterogenity and take rolling mean across 3 days 
algae.subset.roll<-ddply(.data=algae.subset, .variables=.(Trophic, Species, Replicate,Origin,Guild), .fun= transform, Mean = rollmean(Density, 3, fill=NA, align="center"))
algae.subset.roll<-algae.subset.roll[algae.subset.roll$Day%in%c("3","9","15","19","21","27","33","39","45","51","57"),]

#pool data across Replicates 
algae.subset.roll.mean<-ddply(.data=algae.subset.roll, .variables=.(Trophic, Species, Day,Origin,Guild), .fun= summarise, Mean = mean(Mean))

library(mgcv)
mod.1<-gam(Mean~s(Day)+s(Day, by=Guild)+Guild+Trophic*Origin, data=algae.subset.roll)

mod.2<-gam(Mean~s(Day)+Guild+Trophic*Origin, data=algae.subset.roll) 

#compare with ANOVA if interaction is significant keep it 
anova(mod.1, mod.2, test='Chisq')

#Origin not significant and dropped

mod.3<-gam(Mean~s(Day, by=Guild)+Guild+Trophic, data=algae.subset.roll)#used this model 

#model selection 
AIC(mod.1, mod.2, mod.3)

#inspect mod.3 results
summary(mod.3)

#plot Figure 3a
Figure.3.a<-ggplot(algae.subset.roll.mean, aes(x=Day,y=Mean, colour= Trophic))+geom_point()+facet_wrap(Species~Guild)+
  stat_smooth(se=F, size=1, method="loess")+
  scale_colour_manual(values=c("black", "grey"))+
  scale_y_log10(breaks=c(20,100,500))+
  ylab("Periphyton density (ug/ml)")+
  theme_minimal()

#plot Figure 3b
library(gamm4)
algae.subset.roll$Day <- as.numeric(algae.subset.roll$Day)
algae.subset.roll$Day <- algae.subset.roll$Day + rnorm(341,0,0.1)
mod.3<-gamm4(Mean~s(Day, by=Guild)+Guild+Trophic, data=algae.subset.roll,REML=F)
vis.gam(mod.3$gam, view=c("Day","Guild"), color = "topo", theta=140, phi=10, ticktype="detailed")

####Stable isotope analyses####
#read in stable isotope data from GitHub
SIA.URL <- getURL("https://raw.githubusercontent.com/Monsauce/Size-does-matter-/master/SIA.csv")
SIA<-read.csv(text=SIA.URL)

#pull out only crayfish treatments and order factors
SIA$Species<- factor(SIA$Species, levels=c(" Limosus","Rusticus", "Propinquus", "Virilis"))

#pool across Replicate and calulate Mean, SD
SIA.mean<-ddply(.data=SIA, .variables=.(Species,Trophic), .fun= summarise, MeanC = mean(d15C), MeanN=mean(d15N))
SIA.SD<-ddply(.data=SIA, .variables=.(Species, Trophic), .fun= summarise, SDC = sd(d15C), SDN = sd(d15N))
SIA.DF<-merge(SIA.mean,SIA.SD)

#plot Figure 4
Figure.4<-ggplot(SIA.DF, aes(x =MeanC, y = MeanN, colour=Trophic))+geom_point(aes(shape = Species, size=3))+
  geom_errorbar(aes(ymin=MeanN-SDN, ymax=MeanN+SDN))+
  geom_errorbarh(aes(xmax = MeanC + SDC, xmin = MeanC - SDC))+
  theme_minimal()+
  scale_colour_manual(values = c("black", "grey"))+
  scale_shape_manual(values=c(0,15,2,17))+
  ylab(expression(paste(Delta, "N")))+
  xlab(expression(paste(Delta, "C")))

#run two-way ANOVA and TukeyHSD to determine differences between Species 
ANOVA3<- aov(d15C ~ Origin*Trophic, data=SIA)
TukeyHSD(ANOVA3)

ANOVA4<- aov(d15N ~ Origin*Trophic, data=SIA)
summary(ANOVA4)
TukeyHSD(ANOVA4)

####CV analyses####

#calculate CV for each Species, Guild, Origin, Replicate
algae.cv<-ddply(.data=algae.subset.roll, .variables=.(Species, Guild, Trophic, Replicate), .fun= summarise, CV = sd(Mean)/mean(Mean))

#pool across Replicates 
algae.cv.mean<-ddply(.data=algae.cv, .variables=.(Species, Guild, Trophic), .fun= summarise, Mean = mean(CV), SE=sd(CV)/sqrt(length(CV)))

#plot Figure 5
Figure.5<-ggplot(algae.cv.mean, aes(x =Species, y = Mean, fill=Trophic))+geom_bar(stat = "identity",position="dodge")+xlab("Species")+ylab("Coefficent of variation (CV)")+
  theme_minimal()+scale_fill_manual(values=c("black", "grey"))+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE),width=.2,position=position_dodge(.9))+
  scale_y_continuous(limits=c(0, 1.5))+
  facet_wrap(~Guild, scales="free")

#run two-way ANOVA and TukeyHSD to determine differences between Trophic and Species  
ANOVA5<- aov(CV ~ Trophic*Guild, data=algae.cv)
TukeyHSD(ANOVA5)

####Crayfish lengths
length.URL <- getURL("https://raw.githubusercontent.com/Monsauce/Size-does-matter-/master/Lengths.csv")
length<-read.csv(text=length.URL)

length$Species<- factor(length$Species, levels=c("Limosus","Rusticus ", "Propinquus", "Virilis "))#order factors

length.mean<-ddply(.data=length, .variables=.(Trophic, Species), .fun= summarise, Mean = mean(Length), SE=sd(Length)/sqrt(length(Length)))

#plot Figure S2
Figure.S2<-ggplot(length.mean, aes(x = Species, y = Mean, fill=Trophic))+ geom_bar(stat = "identity",position="dodge")+xlab("Species")+
  ylab("Length")+theme_minimal()+
  scale_fill_manual(values=c("black", "grey"))+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE),width=.2,position=position_dodge(.9))

#run one-way ANOVA and TukeyHSD to determine differences between Species  
ANOVA6 <- aov(Length ~ Species, data=length)
TukeyHSD(ANOVA6)







