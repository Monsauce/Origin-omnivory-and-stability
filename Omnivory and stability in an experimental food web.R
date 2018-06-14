#R script to run analysis and generate figures for "Omnivory and stability in an experimental food web"
library(RCurl)
library(ggplot2)
library(plyr)
library(magrittr)
library(dplyr)
library(cowplot)


####Snail analyses####
#read in snail data from GitHub
snail.URL <- getURL("https://raw.githubusercontent.com/Monsauce/Origin-omnivory-and-stability/master/Snail.csv")
snail<-read.csv(text=snail.URL)

#convert negatives to zeroes in Snail$Consumed 
snail$Consumed[snail$Consumed<0]<- 0

#pull out only snail and crayfish treatments and order factors
snail.subset<-snail[snail$Species%in%c("Limosus", "Propinquus","Rusticus","Virilis", "Snail"),]

#remove day 49 is missing a value for mesocosm 36
snail.subset<-snail.subset[snail.subset$Day!=49,]

#pool across replicates
snail.subset.mean<-ddply(.data=snail.subset, .variables=.(Day, Trophic, Species, Origin), .fun= summarise, Mean = mean(Density))

#pool across replicates and day 
snail.subset.mean.day<-ddply(.data=snail.subset, .variables=.(Trophic, Species, Origin, Replicate), .fun= summarise, Mean = mean(Density))

#remove snail for figure
snail.subset.mean.crayfish<-snail.subset.mean[snail.subset.mean$Species%in%c("Limosus", "Propinquus","Rusticus","Virilis"),]

#order factors
snail.subset.mean.crayfish$Species<- factor(snail.subset.mean.crayfish$Species, levels=c("Limosus","Rusticus", "Propinquus", "Virilis"))

#snail-only for figure
snail.subset.mean.snail<-snail.subset.mean[snail.subset.mean$Species%in%c("Snail"),]

#plot Figure S1A
Figure.S1A<-ggplot(snail.subset.mean.crayfish, aes(x = Day, y = Mean))+
  geom_point()+
  ylab("Mean number of snails remaining")+
  facet_grid(Trophic~Species)+
  stat_smooth(se=F, colour="black", size=1)+
  theme_classic()+
  theme(panel.border=element_rect(colour="black",fill=NA))+
  theme(strip.background = element_blank())
  
#plot Figure S1B
Figure.S1B<-ggplot(snail.subset.mean.snail, aes(x = Day, y = Mean))+
    geom_point()+
    ylab("Mean number of snails remaining")+
    stat_smooth(se=F, colour="black", size=1)+
    theme_classic()+
    theme(panel.border=element_rect(colour="black",fill=NA))+
    theme(strip.background = element_blank())
    
#merge plots together
FigureS1<-plot_grid(Figure.S1A, Figure.S1B, labels = c("A", "B"), ncol = 1)


#calculate LD75 for each species and treatment 
library(MASS)

#snail
Snail.snail<-snail.subset[snail.subset$Species=="Snail",] 
attach(Snail.snail)
model <- function(df){glm(cbind(Alive,Consumed)~Day, family=binomial, data = df)} 

Snail.snail.model.outputs<-ddply(Snail.snail, "Replicate", function(x){
  model.output <- model(x)
  df.out <- dose.p(model.output,p=0.25)
  return(df.out)
})  


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
RO.snail<-snail.subset[snail.subset$Species=="Rusticus" & snail.subset$Trophic %in% "Omnivore",] 
attach(RO.snail)
model <- function(df){glm(cbind(Alive,Consumed)~Day, family=binomial, data = df)} 

RO.snail.model.outputs<-ddply(RO.snail, "Replicate", function(x){
  model.output <- model(x)
  df.out <- dose.p(model.output,p=0.25)
  return(df.out)
}) 


#Rusticus, Predator 
RP.snail<-snail.subset[snail.subset$Species=="Rusticus" & snail.subset$Trophic %in% "Predator",] 
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
VO.snail<-snail.subset[snail.subset$Species=="Virilis" & snail.subset$Trophic %in% "Omnivore",] 
attach(VO.snail)
model <- function(df){glm(cbind(Alive,Consumed)~Day, family=binomial, data = df)} 

VO.snail.model.outputs<-ddply(VO.snail, "Replicate", function(x){
  model.output <- model(x)
  df.out <- dose.p(model.output,p=0.25)
  return(df.out)
}) 


#Virilis, Predator
VP.snail<-snail.subset[snail.subset$Species=="Virilis" & snail.subset$Trophic %in% "Predator",] 
attach(VP.snail)
model <- function(df){glm(cbind(Alive,Consumed)~Day, family=binomial, data = df)} 

VP.snail.model.outputs<-ddply(VP.snail, "Replicate", function(x){
  model.output <- model(x)
  df.out <- dose.p(model.output,p=0.25)
  return(df.out)
}) 


LD75.model.output <- rbind(LO.snail.model.outputs,PO.snail.model.outputs, RO.snail.model.outputs, VO.snail.model.outputs,
                           LP.snail.model.outputs, PP.snail.model.outputs, RP.snail.model.outputs, VP.snail.model.outputs, Snail.snail.model.outputs)

LD75.model.output$Species<-snail.subset.mean.day$Species
LD75.model.output$Trophic<-snail.subset.mean.day$Trophic
LD75.model.output$Origin<-snail.subset.mean.day$Origin
colnames(LD75.model.output)[2] <- "LD75"

#order factors
LD75.model.output$Species<- factor(LD75.model.output$Species, levels=c("Limosus","Rusticus", "Propinquus", "Virilis","Snail"))


#plot Figure 2A
Figure.2A<-ggplot(LD75.model.output, aes(x = Species, y =LD75))+
  geom_boxplot(aes(colour = Trophic))+
  xlab("Species")+
  ylab("Day when 75% mortality observed (LD75)")+
  scale_colour_manual(values=c("Black", "Dark Grey", "Red"))+
  theme_classic()+theme(panel.border=element_rect(colour="black",fill=NA))+
  theme(strip.background = element_blank())

#run two-way ANOVA and TukeyHSD to determine differences between Species and food web modules
ANOVA.LD75<-aov(LD75~Trophic*Species, data=LD75.model.output)
TukeyANOVA.LD75<-TukeyHSD(ANOVA.LD75)

#Table S1
Table.S1<-TukeyANOVA.LD75$Species


####Algae analyses####  
#read in algae data from GitHub
algae.URL <- getURL("https://raw.githubusercontent.com/Monsauce/Origin-omnivory-and-stability/master/Algae.csv")
algae<-read.csv(text=algae.URL)

#read in tile data
tile.URL <- getURL("https://raw.githubusercontent.com/Monsauce/Origin-omnivory-and-stability/master/Tile.csv")
tile<-read.csv(text=tile.URL)

#merge two data frames
algae<-merge(algae, tile, by=c("Species", "Replicate", "Day", "Trophic"))

#final-initial pecentange function 
difffunc<-function(x){
  y<- ((x$Density[which(x$Day == max(x$Day))]-x$Density[which(x$Day == min(x$Day))])/x$Density[which(x$Day == min(x$Day))])*100
  return(y)
}


#calculate mean percent difference for each species x tile x replicate
algae.diff<-ddply(algae, .variables=.(Species, Trophic, Replicate, Tile), .fun= difffunc)

#rename V1
colnames(algae.diff)[5]<-"per.diff"

#get range information
algae.diff.day<-ddply(algae, .variables=.(Species, Trophic, Replicate, Tile), .fun=summarize, max.day=max(Day), min.day=min(Day))
algae.diff$max.day<-algae.diff.day$max.day
algae.diff$min.day<-algae.diff.day$min.day

#add range
algae.diff<-ddply(algae.diff, .(), .fun= transform, range = max.day-min.day)

#remove inf
algae.diff <- algae.diff[!algae.diff$per.diff %in% c("Inf"), ]


#divide by range to standardize sampling time 
algae.diff<-ddply(algae.diff, .variables=.(Species, Trophic, Replicate, Tile), .fun=transform, std.per.diff=per.diff/range)

#subset crayfish
algae.diff.cray<-algae.diff[algae.diff$Species %in% c("Rusticus","Limosus","Virilis","Propinquus","Snail"),]

algae.diff.cray.mean<-ddply(algae.diff.cray, .variables=.(Species, Trophic, Replicate), .fun=summarise, mean.per.diff=mean(std.per.diff))

#order factors
algae.diff.cray.mean$Species<- factor(algae.diff.cray.mean$Species, levels=c("Limosus","Rusticus", "Propinquus", "Virilis", "Snail"))

#run ANOVA for snail and crayfish TukeyHSD to determine differences between Species and food web modules
ANOVA.algae<-aov(mean.per.diff~Trophic*Species, algae.diff.cray.mean)
TukeyANOVA.algae<-TukeyHSD(ANOVA.algae)

#Table S2
Table.S2<-TukeyANOVA.algae$Species


#plot Figure 2B
Figure.2B<-ggplot(algae.diff.cray.mean, aes(x = Species, y = mean.per.diff))+
  geom_boxplot(aes(colour = Trophic))+
  theme_classic()+
  theme(panel.border=element_rect(colour="black",fill=NA))+
  theme(strip.background = element_blank())+
  xlab("Species")+
  ylab("Standardized percentage algae density change")+
  scale_color_manual(values=c("Black", "Dark Grey", "Red"))

#make Figure 2
Figure2<-plot_grid(Figure.2A, Figure.2B, labels = c("A", "B"), ncol = 1)


####CV analyses####

#subset crayfish
algae.cray<-algae[algae$Species %in% c("Rusticus","Limosus","Virilis","Propinquus", "Snail"),]

#calculate CV for each Species, Guild, Origin, Replicate
algae.cray.cv.tile<-ddply(algae.cray, .variables=.(Species, Trophic, Replicate, Tile ), .fun= summarise, CV = sd(Density)/mean(Density))

#calculate CV for each replicate
algae.cray.cv.rep<-ddply(algae.cray.cv.tile, .variables=.(Species, Trophic, Replicate), .fun= summarise, mean = mean(CV))

#order factors
algae.cray.cv.rep$Species<- factor(algae.cray.cv.rep$Species, levels=c("Limosus","Rusticus", "Propinquus", "Virilis", "Snail"))


#plot Figure 3
Figure.3<-ggplot(algae.cray.cv.rep, aes(x =Species, y = mean))+
  geom_boxplot(aes(colour = Trophic))+
  theme_classic()+
  theme(panel.border=element_rect(colour="black",fill=NA))+
  theme(strip.background = element_blank())+
  xlab("Species")+
  ylab("Coefficent of variation (CV)")+
  scale_color_manual(values=c("Black", "Dark Grey", "Red"))+
  scale_y_continuous(limits=c(0, 1.5))

#run two-way ANOVA and TukeyHSD to determine differences between Species and food web module  
ANOVA.CV<- aov(mean ~ Trophic*Species, data=algae.cray.cv.rep)
TukeyANOVA.CV<-TukeyHSD(ANOVA.CV)

#Table S3
Table.S3<-TukeyANOVA.CV$Species








