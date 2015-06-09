#R script to run analysis and generate figures for "Omnivory and stability in an experimental food web"
library(RCurl)

####Snail analyses####
#read in Snail data from GitHub
snail.URL <- getURL("https://raw.githubusercontent.com/Monsauce/Omnivory-and-stability-in-an-experimental-food-web/master/Snail.csv")
snail<-read.csv(text=Snail.URL)

#pull out only crayfish treatments and order factors
snail.subset<-Snail[Snail$Species%in%c("Limosus", "Propinquus","Rusticus ","Virilis "),]
snail.subset$Species<- factor(snail.subset$Species, levels=c("Limosus","Rusticus ", "Propinquus", "Virilis "))

# remove day 49 is missing a value for mesocosm 36
snail.subset<-snail.subset[snail.subset$Day!=49,]

#pool treatments together and calculate average 
snail.subset.mean<-ddply(.data=snail.subset, .variables=.(Treatment, Origin), .fun= summarise, Mean = mean(Density))


