#R script to run analysis and generate figures for "Omnivory and stability in an experimental food web"
library(RCurl)

####Snail analyses####
#read in Snail data from GitHub
snail.URL <- getURL("https://raw.githubusercontent.com/Monsauce/Size-does-matter-/master/Snail.csv")
snail<-read.csv(text=snail.URL)

#conver negatives to zeroes in Snail$Consumed 
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



LD75.model.output.mean<-ddply(.data=LD50.model.output, .variables=.(Species, Trophic), .fun= summarise, Mean = mean(LD50), SE=sd(LD50)/sqrt(length(LD50)))

#plot Figure 2
Figure.2<-ggplot(LD75.model.output.mean, aes(x = Species, y = Mean, fill=Trophic))+
  geom_bar(stat = "identity",position="dodge")+xlab("Species")+ylab("Day when 75% of snails consumed")+
  scale_fill_manual(values=c("black", "grey"))+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE),width=.2,position=position_dodge(.9))+
  theme_minimal()

#run two-way ANOVA and TukeyHSD to determine differences between species
ANOVA1<-aov(LD75~Species*Trophic, data=LD75.model.output)
TukeyHSD(ANOVA1)


  
  
  
  
  
  
