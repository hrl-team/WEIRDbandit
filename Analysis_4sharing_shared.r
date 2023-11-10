##Analysis Script for the Outcome context-dependence is not WEIRD
##By subcomarc@gmail.com
#
#DOI:TBD
#
#This script is meant to be used in conjunction with the data file "Data_4sharing.csv"

#Additionally, 
##Human development index (HDI) analyses need official data from UNDP (https://www.hdr.undp.org/en/data)
#here we used the file 2020_Statistical_Annex_Table_1.xlsx
##Cultural distance measure values were extracted with permision from https://world.culturalytics.com/
##The MAPAMUNDI plot uses file "worldcities.csv" downloaded for free from https://simplemaps.com/data/world-cities


#This script and its companions allow the user to reproduce the main stats and figures of the manuscript
#Analyses and figures from the supplementary material are not included here, but can be easily reproduced using this same code

############### ################################
#Preloading settings, directories and libraries#
############### ################################
rm(list=ls()) ## To clean environment
# setwd() ## make sure you set your directory!

library("easystats")
library("nlme")
library('xtable')
library('knitr')
library('lme4')
library('car')
library('multcomp')
library('emmeans')
library("Hmisc")
options(java.parameters = "-Xmx20G") #prevent pesky Excel from exhausting poor little java. HAS TO BE LOADED BEFORE XLSX package. NOT NECESSARY IF YOU DONT HAVE EXCEL DATA FILES
library("openxlsx") #NECESSARY FOR UNITED NATIONS DATA
library("xlsx") #ECESSARY FOR UNITED NATIONS DATA
library("ggplot2")
library("viridis")
library("logspline")
library("nls2")
library("MASS")
library("dplyr")
library("ggeffects")
library('magrittr')
library("MuMIn") #R2 for glmer and lmer, updated according to Nakagawa 2017
library("boot")
library("cowplot")
library("readxl")
library("sjstats")
library("afex")
library("effectsize")
library("readr")
library("pracma")
library("zoo")
library("maps")
library("forcats")
library("mosaic")
library("wesanderson")
library("performance")
library("ggthemes")
library("RColorBrewer")
library("colorblindr")
library("colorspace")
library("rstatix")
library("DHARMa")
library("gghalves")
library("reshape2")
library('Cairo') #make sure you have cairo installed in your system, otherwise CairoPDF will not work
library("ggcorrplot")
library('ggspatial')
library("sf")
library("rnaturalearth")
library("forcats")
library("httpgd")
library("sensemakr")
library("konfound")
library("rstanarm")
library("brms")


##Open plot interphase
hgd() #open browser to see plots, not necessary if you are using Rstudio
hgd_browse() #open browser to see plots, not necessary if you are using Rstudio


######################################################
#LOAD PARTICIPANT DATA AND PARAMETERS, NO EXCLUSIONS##
######################################################

#all files available at OSF: https://osf.io/yebm9/
#load behavioral data and questionnaires
CompleteDF <- read.csv('WEIRD__CDep_NoExclusions.csv') #Load full dataset
#if all you care about are behavioral analyses, you are all set

#if you actually want to fit the models, you need to send the data to Matlab and fit the models there
#we provide the script: 
#"Fitting_RL.m" 
#and the joint functions: 
#"COMPLETE_model_fitting_simulations.m"
#"riskmodelData.m"
#"rislmodelRecovery.m"
#to do exactly that. 

#load fitted parameters per participant
ExplicitParams <- read.csv(file="SimmedataSCALINGLOT.csv", header = TRUE)#fitted parameters from SCALING model (LOTTERY)
AbsoUtiFreeParams <- read.csv(file="SimmedataSCALINGRL.csv", header = TRUE)#fitted parameters from SCALING model (RL) 

#some renaming for convenience
colnames(ExplicitParams)[1] <- "Participant.Private.ID"
colnames(AbsoUtiFreeParams)[1] <- "Participant.Private.ID"
colnames(AbsoUtiFreeParams)[7] <- "MeanSimmedAbsolutFree"
colnames(AbsoUtiFreeParams)[2] <- "Condition"

#merge fitted parameters with behavioral data

ExplicitParams <- merge(ExplicitParams, distinct(data.frame("Participant.Private.ID"=CompleteDF$Participant.Private.ID, 
                                                            "Language"=CompleteDF$Language, "WhichPhase"=3, "Condition"=CompleteDF$Condition, "Country"=CompleteDF$Country)))

AbsoUtiFreeParams<-  merge(AbsoUtiFreeParams,  distinct(data.frame("Participant.Private.ID"=CompleteDF$Participant.Private.ID, 
                                                                   "Language"=CompleteDF$Language, "Condition"=CompleteDF$Condition, "Country"=CompleteDF$Country)))


#########
#FIGURES#
#########


##Human development index (HDI)
#needs official data from UNDP (https://www.hdr.undp.org/en/data)
#here we used the file 2020_Statistical_Annex_Table_1.xlsx


HDI_data <-data.frame(read.xlsx('020_Statistical_Annex_Table_1.xlsx', 1, stringsAsFactors=FALSE))
HDI_data <- HDI_data[c(8:nrow(HDI_data)), c(1:13)] #remove gunk
HDI_data <- HDI_data[-c(67, 121, 159, 193:nrow(HDI_data)),] #remove gunk
HDI_data <- HDI_data[,-c(4, 6, 8, 10, 12)] #remove gunk
colnames(HDI_data) <- c("Rank", "Country","HDI","LifeExpectancy","SchoolYears","GNI","GNIminusHDI", "HDI2018")
HDI_data$HDI <- as.numeric(HDI_data$HDI)
HDI_data$Rank <- as.numeric(HDI_data$Rank)
HDI_data <- HDI_data[HDI_data$Country %in% c("Israel","France","United States","China","Chile","Argentina","Japan","Iran (Islamic Republic of)","India","Russian Federation","Morocco"),] #Get only the countries in our study
HDI_data$Country <- c("USA", "Israel", "Japan","France", "Chile","Argentina","Russia","Iran","China","Morocco","India")

HowManyCountries<-  11#nrow(HDI_data)
getPalette <- colorRampPalette(c("orange1","red2","blue3","brown4"), bias = 1.5, interpolate = c("linear"), space = "Lab")
HDI_colors <- getPalette(HowManyCountries)
HDI_data$Colors <- HDI_colors

HDI_plot<-ggplot(data=HDI_data) +
  geom_col(aes(reorder(Country,-HDI), HDI), color="black", fill=HDI_data$Colors) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=22, face="bold"),
        legend.text = element_text(angle=0, face="bold", colour="black", size="22"),
        strip.text.x =  element_text(angle=0, face="bold", colour="black", size="22"),
        legend.title = element_text(angle=0, face="bold", colour="black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line=element_blank(), panel.border=element_blank(), 
        axis.text.y=element_text(size=22, face="bold"), axis.title.y=element_text(size=22),
        plot.title=element_text(size=30, face="bold", color="black")) +
  labs(title = "Human Development \n Index", 
       x = "", 
       y = "") 


##Cultural distance measure
#Values extracted with permision from https://world.culturalytics.com/


DistanceCountries <- c("Argentina", "Chile", "China", "France", "India", "Iran", "Israel", "Japan", "Morocco", "Russia", "USA")
DistanceUSA <- c(0.0638, 0.0627, 0.1618, 0.1195, 0.0845, 0.0959, 0.1060, 0.1222, 0.1573, 0.1369, 0)
DistanceIndia <- c(0.0525, 0.0491, 0.1474, 0.2811, 0, 0.0669, 0.1454, 0.1200, 0.0975, 0.0814, 0.0845)
DistanceDF <- data.frame(factor(DistanceCountries), DistanceUSA, DistanceIndia)
DistanceDF$factor.DistanceCountries. <- fct_relevel(DistanceDF$factor.DistanceCountries., "USA", "Israel", "Japan","France", "Chile","Argentina","Russia","Iran","China","Morocco","India")

Distance_plot<-ggplot(data=DistanceDF) +
  geom_point(data=DistanceDF, aes(x=DistanceUSA, y=DistanceIndia, fill=factor.DistanceCountries.), color="black", shape=21, stroke=2.5, size=5, #color=c("#ea1259ff"), 
             alpha=1) +
  scale_fill_manual(values = HDI_colors, aesthetics = "fill") +
  geom_text(data=DistanceDF, aes(x=DistanceUSA, y=DistanceIndia, label=DistanceCountries), color="black", size=5, #color=c("#ea1259ff"), 
            alpha=1, nudge_y = -0.015) +
  theme_bw() +
  theme(legend.text = element_text(angle=0, face="bold", colour="black", size="22"),
        strip.text.x =  element_text(angle=0, face="bold", colour="black", size="22"),
        legend.title = element_text(angle=0, face="bold", colour="black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border=element_blank(),
        axis.line.x = element_line(linewidth=2),
        axis.line.y = element_line(linewidth=2),
        axis.text.x=element_text(size=22, face="bold"), axis.title.x=element_text(size=22),
        axis.text.y=element_text(size=22, face="bold"), axis.title.y=element_text(size=22),
        plot.title=element_text(size=30, face="bold", color="black")) +
  labs(title = "Cultural distance", 
       x = "from USA", 
       y = "from India") 



##MAPAMUNDI plot
##uses file "worldcities.csv" downloaded for free from https://simplemaps.com/data/world-cities

world_map <- map_data("world")
# world_map$region <- factor(world_map$region)
world_map <-world_map[!world_map$region %in% c("Antarctica"),]
CompleteCountries <- c( "USA", "Israel", "Japan","France", "Chile","Argentina","Russia","Iran","China","Morocco","India")
complete_map <-world_map[world_map$region %in% CompleteCountries,]
complete_map$region <- factor(complete_map$region)
complete_map$region <- factor(complete_map$region)
complete_map$region <- fct_relevel(complete_map$region, "USA", "Israel", "Japan","France", "Chile","Argentina","Russia","Iran","China","Morocco","India")
#add capitals
WorldCities <-  read.csv(file="worldcities.csv", header = TRUE)
WorldCities[WorldCities$country %in% "United States",]$country <- "USA" 
SelectedCountries <- WorldCities %>% filter(city %in% c("Tokyo", "Paris","Chennai","Beijing","Haifa", "Santiago","Moscow","New Brunswick", "Tehran", "Buenos Aires","Rabat"),
                                            country %in% CompleteCountries) 
SelectedCountries <- SelectedCountries[1:length(CompleteCountries),] 
SelectedCountries$country <- factor(SelectedCountries$country)
SelectedCountries$country <- fct_relevel(SelectedCountries$country, "USA", "Israel", "Japan","France", "Chile","Argentina","Russia","Iran","China","Morocco","India")

mmdf <-ggplot() +  
  geom_map(dat= world_map, map = world_map, aes(map_id=region), fill="lightblue", color="lightblue", alpha=1) +
  # geom_map(dat= complete_map, map = world_map, aes(map_id=region, fill=region),  color="black", alpha=0.7) + #for coloring countries
  # scale_fill_manual(values = HDI_colors, aesthetics = c("fill")) +
  expand_limits(x = world_map$long + 1, y = world_map$lat + 2) +
  geom_point(dat= SelectedCountries, aes(lng, lat, color=country), shape=20, size=7) +  #for coloring cities
  scale_color_manual(values = HDI_colors) +
  theme_bw() +
  theme(legend.text = element_blank(),
        legend.position = 0,
        strip.text.x =  element_blank(),
        legend.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line=element_blank(), panel.border=element_blank(), 
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        plot.title=element_blank()) +
  labs(x='', y='')



##BEHAVIORAL LEARNING, TRANSFER AND LOTTERY PREPROCESSING
##

#Learning dataset (#REMEMBER, IF YOU KEEP CONDITION AS AN ARGUMENT, DELTAEVFACTOR WILL BE DISAGREGATED, ADJUST ACCORDINGLY)
#Also, this is something I would usually do with dyplyr, but this approach, while lengthy, is more transparent.
#Of course, now that the logic of what we are doing is clear, after I'm done processing these datasets with an r-base step by step approach,
#I'll go back to dyplyr for the remaining things.


e<- evalq(aggregate(list(ChoseGoodorBad=ChoseGoodorBad), list(deltaEVfactor=deltaEVfactor, WhichPhase=WhichPhase), mean), CompleteDF) #Accuracy
eLang<-evalq(aggregate(list(ChoseGoodorBad=ChoseGoodorBad), list(deltaEVfactor=deltaEVfactor, Language=Language, WhichPhase=WhichPhase), mean), CompleteDF) #mean per participant
ePart<-evalq(aggregate(list(ChoseGoodorBad=ChoseGoodorBad), list(deltaEVfactor=deltaEVfactor, Language=Language, WhichPhase=WhichPhase, Participant.Private.ID=Participant.Private.ID), mean), CompleteDF) #mean per participant
ese<-evalq(aggregate(list(ChoseGoodorBad=ChoseGoodorBad), list(deltaEVfactor=deltaEVfactor, WhichPhase=WhichPhase), function(x) sd(x)/sqrt(length(x))), eLang) #Standard error #SWITCH TO ELANG WHEN USING ALL COUNTRIES
eseSD<-evalq(aggregate(list(ChoseGoodorBad=ChoseGoodorBad), list(deltaEVfactor=deltaEVfactor, WhichPhase=WhichPhase), function(x) sd(x)), eLang) #Standard error #SWITCH TO ELANG WHEN USING ALL COUNTRIES
esepart <- evalq(aggregate(list(ChoseGoodorBad=ChoseGoodorBad), list(deltaEVfactor=deltaEVfactor, WhichPhase=WhichPhase, Language=Language), function(x) sd(x)/sqrt(length(x))), ePart)
esepartSD <- evalq(aggregate(list(ChoseGoodorBad=ChoseGoodorBad), list(deltaEVfactor=deltaEVfactor, WhichPhase=WhichPhase, Language=Language), function(x) sd(x)), ePart)

#ese<-evalq(aggregate(list(ChoseGoodorBad=ChoseGoodorBad), list(deltaEVfactor=deltaEVfactor, WhichPhase=WhichPhase), function(x) sd(x)/sqrt(length(x))), ePart) #Standard error #SWITCH TO ELANG WHEN USING ALL COUNTRIES
e$se<-1
e$se<-ese$ChoseGoodorBad
e$sd<-1
e$sd<-eseSD$ChoseGoodorBad
#ese<-evalq(aggregate(list(ChoseGoodorBad=ChoseGoodorBad), list(deltaEVfactor=deltaEVfactor, Language=Language, WhichPhase=WhichPhase), function(x) sd(x)/sqrt(length(x))), epart) #Standard error
eLang$se<-esepart$ChoseGoodorBad
eLang$sd<-esepartSD$ChoseGoodorBad

fLang<-eLang[eLang$WhichPhase %in% c("1"),]
fPart<-ePart[ePart$WhichPhase %in% c("1"),]
f<-e[e$WhichPhase %in% c("1"),]
fPart$deltaEVfactor <- factor(fPart$deltaEVfactor)
fPart$deltaEVfactor <- fct_relevel(fPart$deltaEVfactor, "5", "0.5") 
fLang$deltaEVfactor <- factor(fLang$deltaEVfactor) 
fLang$deltaEVfactor <- fct_relevel(fLang$deltaEVfactor, "5", "0.5") 
f$deltaEVfactor <- factor(f$deltaEVfactor) 
f$deltaEVfactor <- fct_relevel(f$deltaEVfactor, "5", "0.5") 


fLang$Country<-factor(fLang$Language, labels = c("Morocco", "China", "USA", 
                                                 "Argentina", "Chile","France", "Israel", "Japan", "Iran", "Russia", "India"))
fPart$Country<-factor(fPart$Language, labels = c("Morocco", "China", "USA", 
                                                 "Argentina", "Chile","France", "Israel", "Japan", "Iran", "Russia", "India"))
fLang$Country<-fct_relevel(fLang$Country, "USA", "Israel", "Japan","France", "Chile","Argentina","Russia","Iran","China","Morocco","India")
fPart$Country<-fct_relevel(fPart$Country, "USA", "Israel", "Japan","France", "Chile","Argentina","Russia","Iran","China","Morocco","India")


LearningPerCountry<-fLang #HERE IS THE LEARNING DATA FOR PLOTTING
LearningPerParticipant<-fPart
LearningALLCountries<-f

fLang<-eLang[eLang$WhichPhase %in% c("2"),]
fPart<-ePart[ePart$WhichPhase %in% c("2"),]
f<-e[e$WhichPhase %in% c("2"),]
fPart$deltaEVfactor <- factor(fPart$deltaEVfactor)
fLang$deltaEVfactor <- factor(fLang$deltaEVfactor) 
f$deltaEVfactor <- factor(f$deltaEVfactor) 

fPart$deltaEVfactor <- fct_relevel(fPart$deltaEVfactor, "7.25", "6.75", "2.25", "1.75")
fPart$deltaEVfactor <- fct_relevel(fPart$deltaEVfactor, "6.75", "2.25", "1.75") 
fLang$deltaEVfactor <- fct_relevel(fLang$deltaEVfactor, "7.25", "6.75", "2.25", "1.75") 
f$deltaEVfactor <- fct_relevel(f$deltaEVfactor, "7.25", "6.75", "2.25", "1.75") 


fLang$Country<-factor(fLang$Language, labels = c("Morocco", "China", "USA", 
                                                 "Argentina", "Chile","France", "Israel", "Japan", "Iran", "Russia", "India"))
fPart$Country<-factor(fPart$Language, labels = c("Morocco", "China", "USA", 
                                                 "Argentina", "Chile","France", "Israel", "Japan", "Iran", "Russia", "India"))
fLang$Country<-fct_relevel(fLang$Country, "USA", "Israel", "Japan","France", "Chile","Argentina","Russia","Iran","China","Morocco","India")
fPart$Country<-fct_relevel(fPart$Country, "USA", "Israel", "Japan","France", "Chile","Argentina","Russia","Iran","China","Morocco","India")


TransferPerCountry<-fLang #HERE IS THE TRANSFER DATA FOR PLOTTING
TransferPerParticipant<-fPart
TransferALLCountries<-f


gLang<-eLang[eLang$WhichPhase %in% c("3") & eLang$deltaEVfactor %in% c( "7.25", "6.75", "2.25", "1.75"),]
gPart<-ePart[ePart$WhichPhase %in% c("3") & ePart$deltaEVfactor %in% c( "7.25", "6.75", "2.25", "1.75"),]
g<-e[e$WhichPhase %in% c("3")  & e$deltaEVfactor %in% c( "7.25", "6.75", "2.25", "1.75"),]

gPart$deltaEVfactor <- factor(gPart$deltaEVfactor) 
gLang$deltaEVfactor <- factor(gLang$deltaEVfactor) 
g$deltaEVfactor <- factor(g$deltaEVfactor) 

gPart$deltaEVfactor <- fct_relevel(gPart$deltaEVfactor, "7.25", "6.75", "2.25", "1.75") 
gLang$deltaEVfactor <- fct_relevel(gLang$deltaEVfactor, "7.25", "6.75", "2.25", "1.75") 
g$deltaEVfactor <- fct_relevel(g$deltaEVfactor, "7.25", "6.75", "2.25", "1.75") 


gLang$Country<-factor(gLang$Language, labels = c("Morocco", "China", "USA", 
                                                 "Argentina", "Chile","France", "Israel", "Japan", "Iran", "Russia", "India"))
gPart$Country<-factor(gPart$Language, labels = c("Morocco", "China", "USA", 
                                                 "Argentina", "Chile","France", "Israel", "Japan", "Iran", "Russia", "India"))
gLang$Country<-fct_relevel(gLang$Country, "USA", "Israel", "Japan","France", "Chile","Argentina","Russia","Iran","China","Morocco","India")
gPart$Country<-fct_relevel(gPart$Country, "USA", "Israel", "Japan","France", "Chile","Argentina","Russia","Iran","China","Morocco","India")

ExplicitTransferPerCountry<-gLang #HERE IS THE EXPLICIT DATA FOR THE TRANSFER CHOICES FOR PLOTTING
ExplicitTransferPerParticipant<-gPart
ExplicitTransferALLCountries<-g


gLang<-eLang[eLang$WhichPhase %in% c("3") & eLang$deltaEVfactor %in% c( "9","6.5","4","1.5"),]
gPart<-ePart[ePart$WhichPhase %in% c("3") & ePart$deltaEVfactor %in% c( "9","6.5","4","1.5"),]
g<-e[e$WhichPhase %in% c("3")  & e$deltaEVfactor %in% c( "9","6.5","4","1.5"),]

gPart$deltaEVfactor <- factor(gPart$deltaEVfactor) 
gLang$deltaEVfactor <- factor(gLang$deltaEVfactor) 
g$deltaEVfactor <- factor(g$deltaEVfactor) 

gPart$deltaEVfactor <- fct_relevel(gPart$deltaEVfactor, "9","6.5","4","1.5") 
gLang$deltaEVfactor <- fct_relevel(gLang$deltaEVfactor, "9","6.5","4","1.5") 
g$deltaEVfactor <- fct_relevel(g$deltaEVfactor, "9","6.5","4","1.5") 

gLang$Country<-factor(gLang$Language, labels = c("Morocco", "China", "USA", 
                                                 "Argentina", "Chile","France", "Israel", "Japan", "Iran", "Russia", "India"))
gPart$Country<-factor(gPart$Language, labels = c("Morocco", "China", "USA", 
                                                 "Argentina", "Chile","France", "Israel", "Japan", "Iran", "Russia", "India"))
gLang$Country<-fct_relevel(gLang$Country, "USA", "Israel", "Japan","France", "Chile","Argentina","Russia","Iran","China","Morocco","India")
gPart$Country<-fct_relevel(gPart$Country, "USA", "Israel", "Japan","France", "Chile","Argentina","Russia","Iran","China","Morocco","India")

ExplicitPerCountry<-gLang #HERE IS THE EXPLICIT DATA FOR PLOTTING
ExplicitPerParticipant<-gPart
ExplicitALLCountries<-g


#redo real quick for the sim vs. data plot per country per decision context
e<- evalq(aggregate(list(ChoseGoodorBad=ChoseGoodorBad), list(deltaEVfactor=deltaEVfactor, WhichPhase=WhichPhase, Condition=Condition), mean), CompleteDF) #Accuracy
eLang<-evalq(aggregate(list(ChoseGoodorBad=ChoseGoodorBad), list(deltaEVfactor=deltaEVfactor, Language=Language, WhichPhase=WhichPhase, Condition=Condition), mean), CompleteDF) #mean per participant
ePart<-evalq(aggregate(list(ChoseGoodorBad=ChoseGoodorBad), list(deltaEVfactor=deltaEVfactor, Language=Language, WhichPhase=WhichPhase, Participant.Private.ID=Participant.Private.ID, Condition=Condition), mean), CompleteDF) #mean per participant
ese<-evalq(aggregate(list(ChoseGoodorBad=ChoseGoodorBad), list(deltaEVfactor=deltaEVfactor, WhichPhase=WhichPhase, Condition=Condition), function(x) sd(x)/sqrt(length(x))), eLang) #Standard error #SWITCH TO ELANG WHEN USING ALL COUNTRIES
eseSD<-evalq(aggregate(list(ChoseGoodorBad=ChoseGoodorBad), list(deltaEVfactor=deltaEVfactor, WhichPhase=WhichPhase, Condition=Condition), function(x) sd(x)), eLang) #Standard error #SWITCH TO ELANG WHEN USING ALL COUNTRIES
esepart <- evalq(aggregate(list(ChoseGoodorBad=ChoseGoodorBad), list(deltaEVfactor=deltaEVfactor, WhichPhase=WhichPhase, Language=Language, Condition=Condition), function(x) sd(x)/sqrt(length(x))), ePart)
esepartSD <- evalq(aggregate(list(ChoseGoodorBad=ChoseGoodorBad), list(deltaEVfactor=deltaEVfactor, WhichPhase=WhichPhase, Language=Language, Condition=Condition), function(x) sd(x)), ePart)

#ese<-evalq(aggregate(list(ChoseGoodorBad=ChoseGoodorBad), list(deltaEVfactor=deltaEVfactor, WhichPhase=WhichPhase), function(x) sd(x)/sqrt(length(x))), ePart) #Standard error #SWITCH TO ELANG WHEN USING ALL COUNTRIES
e$se<-1
e$se<-ese$ChoseGoodorBad
e$sd<-1
e$sd<-eseSD$ChoseGoodorBad
#ese<-evalq(aggregate(list(ChoseGoodorBad=ChoseGoodorBad), list(deltaEVfactor=deltaEVfactor, Language=Language, WhichPhase=WhichPhase), function(x) sd(x)/sqrt(length(x))), epart) #Standard error
eLang$se<-esepart$ChoseGoodorBad
eLang$sd<-esepartSD$ChoseGoodorBad


fLang<-eLang[eLang$WhichPhase %in% c("1","2"),]  #HERE IS THE RL DATA FOR PLOTTING
fPart<-ePart[ePart$WhichPhase %in% c("1","2"),]
f<-e[e$WhichPhase %in% c("1","2"),]

fPart$deltaEVfactor <- factor(fPart$deltaEVfactor) 
fLang$deltaEVfactor <- factor(fLang$deltaEVfactor) 
f$deltaEVfactor <- factor(f$deltaEVfactor) 

fPart$deltaEVfactor <- fct_relevel(fPart$deltaEVfactor, "5", "0.5", "7.25", "6.75", "2.25", "1.75") 
fLang$deltaEVfactor <- fct_relevel(fLang$deltaEVfactor,  "5", "0.5","7.25", "6.75", "2.25", "1.75") 
f$deltaEVfactor <- fct_relevel(f$deltaEVfactor,  "5", "0.5","7.25", "6.75", "2.25", "1.75") 



fLang$Country<-factor(fLang$Language, labels = c("Morocco", "China", "USA", 
                                                 "Argentina", "Chile","France", "Israel", "Japan", "Iran", "Russia", "India"))
fPart$Country<-factor(fPart$Language, labels = c("Morocco", "China", "USA", 
                                                 "Argentina", "Chile","France", "Israel", "Japan", "Iran", "Russia", "India"))
fLang$Country<-fct_relevel(fLang$Country, "USA", "Israel", "Japan","France", "Chile","Argentina","Russia","Iran","China","Morocco","India")
fPart$Country<-fct_relevel(fPart$Country, "USA", "Israel", "Japan","France", "Chile","Argentina","Russia","Iran","China","Morocco","India")

RLPerCountry<-fLang #HERE IS ALL THE RL DATA FOR PLOTTING
RLPerParticipant<-fPart
RLALLCountries<-f


gLang<-eLang[eLang$WhichPhase %in% c("3") & eLang$deltaEVfactor %in% c( "9","6.5","4","1.5", "7.25", "6.75", "2.25", "1.75"),]
gPart<-ePart[ePart$WhichPhase %in% c("3") & ePart$deltaEVfactor %in% c( "9","6.5","4","1.5", "7.25", "6.75", "2.25", "1.75"),]
g<-e[e$WhichPhase %in% c("3")  & e$deltaEVfactor %in% c( "9","6.5","4","1.5", "7.25", "6.75", "2.25", "1.75"),]

gPart$deltaEVfactor <- factor(gPart$deltaEVfactor)
gLang$deltaEVfactor <- factor(gLang$deltaEVfactor) 
g$deltaEVfactor <- factor(g$deltaEVfactor) 

gPart$deltaEVfactor <- fct_relevel(gPart$deltaEVfactor, "9","6.5","4","1.5", "7.25", "6.75", "2.25", "1.75")
gLang$deltaEVfactor <- fct_relevel(gLang$deltaEVfactor, "9","6.5","4","1.5", "7.25", "6.75", "2.25", "1.75") 
g$deltaEVfactor <- fct_relevel(g$deltaEVfactor, "9","6.5","4","1.5", "7.25", "6.75", "2.25", "1.75") 


gLang$Country<-factor(gLang$Language, labels = c("Morocco", "China", "USA", 
                                                 "Argentina", "Chile","France", "Israel", "Japan", "Iran", "Russia", "India"))
gPart$Country<-factor(gPart$Language, labels = c("Morocco", "China", "USA", 
                                                 "Argentina", "Chile","France", "Israel", "Japan", "Iran", "Russia", "India"))
gLang$Country<-fct_relevel(gLang$Country, "USA", "Israel", "Japan","France", "Chile","Argentina","Russia","Iran","China","Morocco","India")
gPart$Country<-fct_relevel(gPart$Country, "USA", "Israel", "Japan","France", "Chile","Argentina","Russia","Iran","China","Morocco","India")

COMPLETEExplicitPerCountry<-gLang #HERE IS THE COMPLETE EXPLICIT DATA FOR PLOTTING
COMPLETEExplicitPerParticipant<-gPart
COMPLETEExplicitALLCountries<-g



#Plot RL Learning phase
#Remember the processed datasets we have for this:
# LearningPerCountry
# LearningPerParticipant
# LearningALLCountries

LearningPlot <- ggplot() + 
  geom_errorbar(data=LearningALLCountries, aes(x=deltaEVfactor, ymax=ChoseGoodorBad+(se), ymin=ChoseGoodorBad-(se)),
                width=0.05, color="black", size=1.5,  position = position_nudge(x = 0.2)) +
  geom_segment(data=LearningALLCountries, aes(x=(c(2:1)-0.05),xend=(c(2:1)+0.05), y=ChoseGoodorBad-(se*1.96), yend=ChoseGoodorBad-(se*1.96) ), color="black",
               size=1.5,  position = position_nudge(x = 0.2)) +
  geom_segment(data=LearningALLCountries, aes(x=(c(2:1)-0.05),xend=(c(2:1)+0.05), y=ChoseGoodorBad+(se*1.96), yend=ChoseGoodorBad+(se*1.96) ), color="black",
               size=1.5,  position = position_nudge(x = 0.2)) +
  geom_segment(data=LearningALLCountries, aes(x=(c(2:1)-0.05),xend=(c(2:1)-0.05), y=ChoseGoodorBad+(se*1.96), yend=ChoseGoodorBad-(se*1.96) ), color="black",
               size=1.5,  position = position_nudge(x = 0.2)) +
  geom_segment(data=LearningALLCountries, aes(x=(c(2:1)+0.05),xend=(c(2:1)+0.05), y=ChoseGoodorBad+(se*1.96), yend=ChoseGoodorBad-(se*1.96) ), color="black",
               size=1.5,  position = position_nudge(x = 0.2)) +
  geom_segment(data=LearningALLCountries, aes(x=(c(2:1)-0.05),xend=(c(2:1)+0.05), y=ChoseGoodorBad, yend=ChoseGoodorBad), color="black",
               size=1.5,  position = position_nudge(x = 0.2)) +
  geom_errorbar(data=LearningPerCountry, aes(x=deltaEVfactor, ymax=ChoseGoodorBad+(se), ymin=ChoseGoodorBad-(se), colour=Country),
                width=0.05, size=1.5,  position = position_dodge(width = .2)) +
  geom_point(data=LearningPerCountry, aes(x = deltaEVfactor, y = ChoseGoodorBad, colour = Country), position = position_dodge(width = .2), size = 15, shape = 20, alpha = 1)+
  scale_color_manual(values = HDI_colors, aesthetics = "color") +
  geom_hline(yintercept=0.5, linetype=3, size=4) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_x_discrete(labels = c("10p (75%) vs 10p (25%)\n EV difference = 5", "1p (75%) vs 1p (25%)\n EV difference = 0.5"), expand = expansion(mult=0, add=0.3)) +
  theme_bw() +
  theme(legend.text = element_text(angle=0, face="bold", colour="black", size="22"),
        strip.text.x =  element_text(angle=0, face="bold", colour="black", size="22"),
        legend.title = element_text(angle=0, face="bold", colour="black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line=element_line(size = 1), 
        panel.border=element_blank(), 
        axis.text.x=element_text(size=40), axis.title.x=element_text(size=40),
        axis.text.y=element_text(size=40), axis.title.y=element_text(size=40),
        plot.title=element_text(size=40, face="bold", color="black")) +
  labs(title = "Learning phase\n\n", 
       x = "Decision contexts", 
       y = "P(Correct)") 



#differences between learning contexts

LearningDiffs <- LearningPerParticipant[LearningPerParticipant$deltaEVfactor %in% "0.5",]
LearningDiffs$ChoseGoodorBad <- LearningPerParticipant[LearningPerParticipant$deltaEVfactor %in% "0.5",]$ChoseGoodorBad - LearningPerParticipant[LearningPerParticipant$deltaEVfactor %in% "5",]$ChoseGoodorBad 

#I already coded the preprocessing in R base. The principle is the same so I'm switching back to dyplyr
LearningDiffsPERCOUNTRY <- LearningDiffs %>% group_by(Country,deltaEVfactor) %>% summarise(deltaEVfactor=deltaEVfactor, WhichPhase=WhichPhase, ChoseGBmean=mean(ChoseGoodorBad), ChoseGBse=parameters::standard_error(ChoseGoodorBad)) %>% distinct()
LearningDiffsPERCOUNTRY$se <- LearningDiffsPERCOUNTRY$ChoseGBse
LearningDiffsPERCOUNTRY$ChoseGoodorBad <- LearningDiffsPERCOUNTRY$ChoseGBmean
LearningALLCOUNTRIESDiffs <- LearningDiffsPERCOUNTRY %>% group_by(deltaEVfactor) %>%  summarise(deltaEVfactor=deltaEVfactor, WhichPhase=WhichPhase, ChoseGBmean=mean(ChoseGoodorBad), ChoseGBse=parameters::standard_error(ChoseGoodorBad)) %>% distinct()
LearningALLCOUNTRIESDiffs$se <- LearningALLCOUNTRIESDiffs$ChoseGBse
LearningALLCOUNTRIESDiffs$ChoseGoodorBad <- LearningALLCOUNTRIESDiffs$ChoseGBmean

LearningDiffPlot <- ggplot() + 
  geom_errorbar(data=LearningALLCOUNTRIESDiffs, aes(x=deltaEVfactor, ymax=ChoseGoodorBad+(se), ymin=ChoseGoodorBad-(se)),
                width=0.05, color="black", size=1.5,  position = position_nudge(x = 0.2)) +
  geom_segment(data=LearningALLCOUNTRIESDiffs, aes(x=(c(1:1)-0.05),xend=(c(1:1)+0.05), y=ChoseGoodorBad-(se*1.96), yend=ChoseGoodorBad-(se*1.96) ), color="black",
               size=1.5,  position = position_nudge(x = 0.2)) +
  geom_segment(data=LearningALLCOUNTRIESDiffs, aes(x=(c(1:1)-0.05),xend=(c(1:1)+0.05), y=ChoseGoodorBad+(se*1.96), yend=ChoseGoodorBad+(se*1.96) ), color="black",
               size=1.5,  position = position_nudge(x = 0.2)) +
  geom_segment(data=LearningALLCOUNTRIESDiffs, aes(x=(c(1:1)-0.05),xend=(c(1:1)-0.05), y=ChoseGoodorBad+(se*1.96), yend=ChoseGoodorBad-(se*1.96) ), color="black",
               size=1.5,  position = position_nudge(x = 0.2)) +
  geom_segment(data=LearningALLCOUNTRIESDiffs, aes(x=(c(1:1)+0.05),xend=(c(1:1)+0.05), y=ChoseGoodorBad+(se*1.96), yend=ChoseGoodorBad-(se*1.96) ), color="black",
               size=1.5,  position = position_nudge(x = 0.2)) +
  geom_segment(data=LearningALLCOUNTRIESDiffs, aes(x=(c(1:1)-0.05),xend=(c(1:1)+0.05), y=ChoseGoodorBad, yend=ChoseGoodorBad), color="black",
               size=1.5,  position = position_nudge(x = 0.2)) +
  geom_errorbar(data=LearningDiffsPERCOUNTRY, aes(x=deltaEVfactor, ymax=ChoseGoodorBad+(se), ymin=ChoseGoodorBad-(se), colour=Country),
                width=0.05, size=1.5,  position = position_dodge(width = .2)) +
  geom_point(data=LearningDiffsPERCOUNTRY, aes(x = deltaEVfactor, y = ChoseGoodorBad, colour = Country), position = position_dodge(width = .2), size = 15, shape = 20, alpha = 1)+
  scale_color_manual(values = HDI_colors, aesthetics = "color") +
  geom_hline(yintercept=0, linetype=3, size=4) +
  scale_y_continuous(limits = c(-0.5, 0.5), breaks = seq(-0.5, 0.5, by = 0.2)) +
  scale_x_discrete(labels = c("ΔEV = 0.5 minus\n ΔEV = 5"), expand = expansion(mult=0, add=0.3)) +
  theme_bw() +
  theme(legend.text = element_text(angle=0, face="bold", colour="black", size="22"),
        strip.text.x =  element_text(angle=0, face="bold", colour="black", size="22"),
        legend.title = element_text(angle=0, face="bold", colour="black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line=element_line(size = 1), 
        panel.border=element_blank(), 
        axis.text.x=element_text(size=40), axis.title.x=element_text(size=40),
        axis.text.y=element_text(size=40), axis.title.y=element_text(size=40),
        plot.title=element_text(size=40, face="bold", color="black")) +
  labs(title = "Learning phase\n\n", 
       x = "", 
       y = "Difference") 



#Plot RL Transfer phase
#Remember the processed datasets we have for this:

# TransferPerCountry
# TransferPerParticipant
# TransferALLCountries

TransferPlot <- ggplot() + #f, aes(x = deltaEVfactor, y = ChoseGoodorBad)
  geom_errorbar(data=TransferALLCountries, aes(x=deltaEVfactor, ymax=ChoseGoodorBad+(se), ymin=ChoseGoodorBad-(se)),
                width=0.05, color="black", size=1.5,  position = position_nudge(x = 0.2)) +
  geom_segment(data=TransferALLCountries, aes(x=(c(4:1)-0.05),xend=(c(4:1)+0.05), y=ChoseGoodorBad-(se*1.96), yend=ChoseGoodorBad-(se*1.96) ), color="black", #its 3:1 for NoDoms, 4:1 for everyone
               size=1.5,  position = position_nudge(x = 0.2)) +
  geom_segment(data=TransferALLCountries, aes(x=(c(4:1)-0.05),xend=(c(4:1)+0.05), y=ChoseGoodorBad+(se*1.96), yend=ChoseGoodorBad+(se*1.96) ), color="black",
               size=1.5,  position = position_nudge(x = 0.2)) +
  geom_segment(data=TransferALLCountries, aes(x=(c(4:1)-0.05),xend=(c(4:1)-0.05), y=ChoseGoodorBad+(se*1.96), yend=ChoseGoodorBad-(se*1.96) ), color="black",
               size=1.5,  position = position_nudge(x = 0.2)) +
  geom_segment(data=TransferALLCountries, aes(x=(c(4:1)+0.05),xend=(c(4:1)+0.05), y=ChoseGoodorBad+(se*1.96), yend=ChoseGoodorBad-(se*1.96) ), color="black",
               size=1.5,  position = position_nudge(x = 0.2)) +
  geom_segment(data=TransferALLCountries, aes(x=(c(4:1)-0.05),xend=(c(4:1)+0.05), y=ChoseGoodorBad, yend=ChoseGoodorBad), color="black",
               size=1.5,  position = position_nudge(x = 0.2)) +
  geom_errorbar(data=TransferPerCountry, aes(x=deltaEVfactor, ymax=ChoseGoodorBad+(se), ymin=ChoseGoodorBad-(se), colour=Country),
                width=0.05, size=1.5,  position = position_dodge(width = .2)) +
  geom_point(data=TransferPerCountry, aes(x = deltaEVfactor, y = ChoseGoodorBad, colour = Country), position = position_dodge(width = .2), size = 15, shape = 20, alpha = 1)+
  scale_color_manual(values = HDI_colors, aesthetics = "color") +
  geom_hline(yintercept=0.5, linetype=3, size=4) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_x_discrete(labels = c("10p (75%) vs 1p (25%)\n EV difference = 7.25", "10p (75%) vs 1p (75%)\n EV difference = 6.75",
                              "10p (25%) vs 1p (25%)\n EV difference = 2.25", "10p (25%) vs 1p (75%)\n EV difference = 1.75")) +
  theme_bw() +
  theme(legend.text = element_text(angle=0, face="bold", colour="black", size="22"),
        strip.text.x =  element_text(angle=0, face="bold", colour="black", size="22"),
        legend.title = element_text(angle=0, face="bold", colour="black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line=element_line(size = 1), 
        panel.border=element_blank(), 
        axis.text.x=element_text(size=40), axis.title.x=element_text(size=40),
        axis.text.y=element_text(size=40), axis.title.y=element_text(size=40),
        plot.title=element_text(size=40, face="bold", color="black")) +
  labs(title = "Transfer phase\n\n", 
       x = "Decision contexts", 
       y = "P(Correct)") 

#Plot Lottery Risk Benchmark phase
#Remember the processed datasets we have for this:

# ExplicitPerCountry
# ExplicitPerParticipant
# ExplicitALLCountries

LotteryPlot <- ggplot() +
  geom_errorbar(data=ExplicitALLCountries, aes(x=deltaEVfactor, ymax=ChoseGoodorBad+(se), ymin=ChoseGoodorBad-(se), color=Country),
                width=0.05, color="black", size=1.5,  position = position_nudge(x = 0.2)) +
  geom_segment(data=ExplicitALLCountries, aes(x=(c(4:1)-0.05),xend=(c(4:1)+0.05), y=ChoseGoodorBad-(se*1.96), yend=ChoseGoodorBad-(se*1.96) ), color="black",
               size=1.5,  position = position_nudge(x = 0.2)) +
  geom_segment(data=ExplicitALLCountries, aes(x=(c(4:1)-0.05),xend=(c(4:1)+0.05), y=ChoseGoodorBad+(se*1.96), yend=ChoseGoodorBad+(se*1.96) ), color="black",
               size=1.5,  position = position_nudge(x = 0.2)) +
  geom_segment(data=ExplicitALLCountries, aes(x=(c(4:1)-0.05),xend=(c(4:1)-0.05), y=ChoseGoodorBad+(se*1.96), yend=ChoseGoodorBad-(se*1.96) ), color="black",
               size=1.5,  position = position_nudge(x = 0.2)) +
  geom_segment(data=ExplicitALLCountries, aes(x=(c(4:1)+0.05),xend=(c(4:1)+0.05), y=ChoseGoodorBad+(se*1.96), yend=ChoseGoodorBad-(se*1.96) ), color="black",
               size=1.5,  position = position_nudge(x = 0.2)) +
  geom_segment(data=ExplicitALLCountries, aes(x=(c(4:1)-0.05),xend=(c(4:1)+0.05), y=ChoseGoodorBad, yend=ChoseGoodorBad), color="black",
               size=1.5,  position = position_nudge(x = 0.2)) +
  geom_errorbar(data=ExplicitPerCountry, aes(x=deltaEVfactor, ymax=ifelse(ChoseGoodorBad+(se)<=1,ChoseGoodorBad+(se), 1), ymin=ChoseGoodorBad-(se), colour=Country),
                width=0.05, size=1.5,  position = position_dodge(width = .2)) +
  geom_point(data=ExplicitPerCountry, aes(x = deltaEVfactor, y = ChoseGoodorBad, colour = Country), position = position_dodge(width = .2), size = 15, shape = 20, alpha = 1)+
  scale_color_manual(values = HDI_colors, aesthetics = "color") +
  geom_hline(yintercept=0.5, linetype=3, size=4) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_x_discrete(labels = c("10p (100%) vs 1p (100%)\n EV difference = 9",
                              "10p (75%) vs 1p (100%)\n EV difference = 6.5",
                              "10p (50%) vs 1p (100%)\n EV difference = 4.00", "10p (25%) vs 1p (100%)\n EV difference = 1.5")) +  
  theme_bw() +
  theme(legend.text = element_text(angle=0, face="bold", colour="black", size="22"),
        strip.text.x =  element_text(angle=0, face="bold", colour="black", size="22"),
        legend.title = element_text(angle=0, face="bold", colour="black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line=element_line(size = 1), 
        panel.border=element_blank(), 
        axis.text.x=element_text(size=40), axis.title.x=element_text(size=40),
        axis.text.y=element_text(size=40), axis.title.y=element_text(size=40),
        plot.title=element_text(size=40, face="bold", color="black")) +
  labs(title = "Explicit phase RISK\n\n", 
       x = "Decision contexts", 
       y = "P(Correct)") 


#Plot Lottery Transfer contexts phase
#Remember the processed datasets we have for this:

# ExplicitTransferPerCountry
# ExplicitTransferPerParticipant
# ExplicitTransferALLCountries

LotteryPlotTransfer <- ggplot() + 
  geom_errorbar(data=ExplicitTransferALLCountries, aes(x=deltaEVfactor, ymax=ChoseGoodorBad+(se), ymin=ChoseGoodorBad-(se), color=Country),
                width=0.05, color="black", size=1.5,  position = position_nudge(x = 0.2)) +
  geom_segment(data=ExplicitTransferALLCountries, aes(x=(c(4:1)-0.05),xend=(c(4:1)+0.05), y=ChoseGoodorBad-(se*1.96), yend=ChoseGoodorBad-(se*1.96) ), color="black",
               size=1.5,  position = position_nudge(x = 0.2)) +
  geom_segment(data=ExplicitTransferALLCountries, aes(x=(c(4:1)-0.05),xend=(c(4:1)+0.05), y=ChoseGoodorBad+(se*1.96), yend=ChoseGoodorBad+(se*1.96) ), color="black",
               size=1.5,  position = position_nudge(x = 0.2)) +
  geom_segment(data=ExplicitTransferALLCountries, aes(x=(c(4:1)-0.05),xend=(c(4:1)-0.05), y=ChoseGoodorBad+(se*1.96), yend=ChoseGoodorBad-(se*1.96) ), color="black",
               size=1.5,  position = position_nudge(x = 0.2)) +
  geom_segment(data=ExplicitTransferALLCountries, aes(x=(c(4:1)+0.05),xend=(c(4:1)+0.05), y=ChoseGoodorBad+(se*1.96), yend=ChoseGoodorBad-(se*1.96) ), color="black",
               size=1.5,  position = position_nudge(x = 0.2)) +
  geom_segment(data=ExplicitTransferALLCountries, aes(x=(c(4:1)-0.05),xend=(c(4:1)+0.05), y=ChoseGoodorBad, yend=ChoseGoodorBad), color="black",
               size=1.5,  position = position_nudge(x = 0.2)) +
  geom_errorbar(data=ExplicitTransferPerCountry, aes(x=deltaEVfactor, ymax=ifelse(ChoseGoodorBad+(se)<=1,ChoseGoodorBad+(se), 1), ymin=ChoseGoodorBad-(se), colour=Country),
                width=0.05, size=1.5,  position = position_dodge(width = .2)) +
  geom_point(data=ExplicitTransferPerCountry, aes(x = deltaEVfactor, y = ChoseGoodorBad, colour = Country), position = position_dodge(width = .2), size = 15, shape = 20, alpha = 1)+
  scale_color_manual(values = HDI_colors, aesthetics = "color") +
  geom_hline(yintercept=0.5, linetype=3, size=4) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_x_discrete(labels = c( "10p (75%) vs 1p (25%)\n EV difference = 7.25",
                               "10p (75%) vs 1p (75%)\n EV difference = 6.75",
                              "10p (25%) vs 1p (25%)\n EV difference = 2.25", "10p (25%) vs 1p (75%)\n EV difference = 1.75")) +   
  # facet_wrap(vars(Sex)) +
  theme_bw() +
  theme(legend.text = element_text(angle=0, face="bold", colour="black", size="22"),
        strip.text.x =  element_text(angle=0, face="bold", colour="black", size="22"),
        legend.title = element_text(angle=0, face="bold", colour="black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line=element_line(size = 1), 
        panel.border=element_blank(), 
        axis.text.x=element_text(size=40), axis.title.x=element_text(size=40),
        axis.text.y=element_text(size=40), axis.title.y=element_text(size=40),
        plot.title=element_text(size=40, face="bold", color="black")) +
  labs(title = "Explicit phase TRANSFER\n\n", 
       x = "Decision contexts", 
       y = "P(Correct)") 


#Plot heatmaps for the country pairwise contrasts, for Lottery and RL.

#RL
CDFCOUNTR<- CompleteDF
CDFCOUNTR$Country <- factor(CDFCOUNTR$Country)
CDFCOUNTR$Country <- fct_relevel(CDFCOUNTR$Country, "USA", "Israel", "Japan","France", "Chile","Argentina","Russia","Iran","China","Morocco","India")
EXPLICITPHASE12_1 <- glmer(ChoseGoodorBad ~ Country*WhichPhase + (1|Participant.Private.ID), 
                         family=binomial, control=glmerControl(optimizer="bobyqa", 
                                                               optCtrl = list(maxfun = 1000000)), CDFCOUNTR[CDFCOUNTR$WhichPhase %in% c("2","3")
                                                                                                             & CDFCOUNTR$deltaEVfactor %in% c("1.75"),])
##quick excerpt to calculate the euclidean distances for the heatmap###


Estimates175<- data.frame(emmeans(EXPLICITPHASE12_1, pairwise~Country*WhichPhase, type="response", infer=TRUE, adjust="none")$emmeans)
Contrasts175<- data.frame(emmeans(EXPLICITPHASE12_1, pairwise~Country*WhichPhase, type="response", infer=TRUE, adjust="none")$contrasts)

d2 <-dist(as.matrix(Estimates175[Estimates175$WhichPhase %in% "2",]$prob))
d2 <- as.matrix(d2)
d2[upper.tri(d2)]<- NA

d3 <-dist(as.matrix(Estimates175[Estimates175$WhichPhase %in% "3",]$prob))
d3 <- as.matrix(d3)
d3[upper.tri(d3)]<- NA

df2 <- reshape2::melt(d2, varnames = c("Country2", "Country1"), na.rm=TRUE)
df3 <- reshape2::melt(d3, varnames = c("Country2", "Country1"), na.rm=TRUE)

df2$WhichPhase <- 2 
df3$WhichPhase <- 3 

Distances175 <- rbind(df2, df3)
Distances175$WhichPhase <- factor(Distances175$WhichPhase)
Distances175$Country1 <- factor(Distances175$Country1, labels = c("USA", "Israel", "Japan","France", "Chile","Argentina","Russia","Iran","China","Morocco","India"))
Distances175$Country2 <- factor(Distances175$Country2, labels = c("USA", "Israel", "Japan","France", "Chile","Argentina","Russia","Iran","China","Morocco","India"))

#get pvals and apply BH correction

require(reshape2)
mymat <- as.data.frame(Contrasts175)

LS <- lapply(seq_along(mymat), function(i){
  colsplit(mymat[, i], "/", paste0(colnames(mymat)[i], letters[1:2]))
})
mydf<-data.frame(do.call('cbind', LS))
mydf<-mydf[ , colSums(is.na(mydf)) == 0]

LS <- lapply(seq_along(mydf), function(i){
  colsplit(mydf[, i], " ", paste0(colnames(mydf)[i], letters[1:2]))
})
mydf<-data.frame(do.call('cbind', LS))
mydf<-mydf[ , colSums(is.na(mydf)) == 0]


LS <- lapply(seq_along(mydf), function(i){
  colsplit(mydf[, i], " ", paste0(colnames(mydf)[i], letters[1:2]))
})
mydf<-data.frame(do.call('cbind', LS))
mydf<-mydf[ , colSums(is.na(mydf)) == 0]

mydf<-mydf %>% filter(contrastaba == contrastbbb)

SignifCont2 <- mydf %>% filter(contrastaba=="WhichPhase2")
SignifCont2$p.valueaaa<- p.adjust(SignifCont2$p.valueaaa, method = "BH", n = length(SignifCont2$p.valueaaa))
#SignifCont2 <- SignifCont2 %>% filter(p.valueaaa<0.05)

SignifCont3 <- mydf %>% filter(contrastaba=="WhichPhase3")
SignifCont3$p.valueaaa<- p.adjust(SignifCont3$p.valueaaa, method = "BH", n = length(SignifCont3$p.valueaaa))
#SignifCont3 <- SignifCont3 %>% filter(p.valueaaa<0.05)

SignifContAll <- rbind(SignifCont2, SignifCont3)
Distances175filt <- Distances175 %>% filter(value!=0)
SignifContAll$distance <- Distances175filt$value

Distances175$WhichPhase <- factor(Distances175$WhichPhase, labels = c("RL", "Lottery"))
Distance175perf <- ggplot (Distances175, aes(Country1, Country2, fill=value)) +
  geom_tile(color = "white") +
  scale_fill_binned_diverging(n.breaks = 10) +
  coord_fixed() +
  facet_wrap(vars(WhichPhase)) +
  theme_bw() +
  theme(legend.text = element_text(angle=0, face="bold", colour="black", size="22"),
        strip.text.x =  element_text(angle=0, face="bold", colour="black", size="22"),
        legend.title = element_text(angle=0, face="bold", colour="black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line=element_line(size = 1), 
        panel.border=element_blank(), 
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=22, face="bold"),
        axis.text.y=element_text(size=22, face="bold"), axis.title.y=element_text(size=22),
        plot.title=element_text(size=20, face="bold", color="black")) +
  labs(title = "Delta EV = 1.75", 
       x = " ", 
       y = " ") 

PvalPlotUti_des <- edit_colors(Distance175perf, desaturate, amount = 1)
PvalPlotUti_BW <- cowplot::ggdraw(PvalPlotUti_des)


#Comparative figure with the values of the scaling parameters from RL and Lottery
#remember: ExplicitParams
#AbsoUtiFreeParams

RLUTI <-distinct(subset(AbsoUtiFreeParams, select = c(UtifromUtiFree, Country, Participant.Private.ID)))
RLUTI$type <- "RL"
colnames(RLUTI)[1] <- "param"
RiskPhaseParams <- distinct(subset(ExplicitParams, select = c(UtilityfromExpl, Country, Participant.Private.ID)))
RiskPhaseParams$type <- "Lottery"
colnames(RiskPhaseParams)[1] <- "param"

ALLSCALINGPARAMS <- rbind(RiskPhaseParams, RLUTI)
ALLSCALINGPARAMS$Country <- factor(ALLSCALINGPARAMS$Country)
ALLSCALINGPARAMS$Country <- fct_relevel(ALLSCALINGPARAMS$Country, "USA", "Israel", "Japan","France", "Chile","Argentina","Russia","Iran","China","Morocco","India")
ALLSCALINGPARAMS$type <- factor(ALLSCALINGPARAMS$type)

#Plot cross-country half violin for the RL and Lottery SCALING parameter values



#Plot heatmaps for the country pairwise contrasts, for Lottery and RL SCALING parameter values

# UtiMod <- lm(param ~ Country*type, 
#              ALLSCALINGPARAMS)

UtiMod <- lmer(param ~ Country*type + (1|Participant.Private.ID), 
                        control=lmerControl(optimizer="bobyqa", 
                        optCtrl = list(maxfun = 1000000)), 
              ALLSCALINGPARAMS)

Anova(UtiMod)
EstimatesUTI<- data.frame(emmeans(UtiMod, pairwise~Country*type, type="response", infer=TRUE, adjust="none")$emmeans)
ContrastsUTI<- data.frame(emmeans(UtiMod, pairwise~Country*type, type="response", infer=TRUE, adjust="none")$contrasts)

d2 <-dist(as.matrix(EstimatesUTI[EstimatesUTI$type %in% "RL",]$emmean))
d2 <- as.matrix(d2)
d2[upper.tri(d2)]<- NA

d3 <-dist(as.matrix(EstimatesUTI[EstimatesUTI$type %in% "Lottery",]$emmean))
d3 <- as.matrix(d3)
d3[upper.tri(d3)]<- NA

df2 <- reshape2::melt(d2, varnames = c("Country2", "Country1"), na.rm=TRUE)
df3 <- reshape2::melt(d3, varnames = c("Country2", "Country1"), na.rm=TRUE)

df2$type <- "RL" 
df3$type <- "Lottery" 

DistancesUTI <- rbind(df2, df3)
DistancesUTI$type <- factor(DistancesUTI$type)
DistancesUTI$Country1 <- factor(DistancesUTI$Country1, labels = c("USA", "Israel", "Japan","France", "Chile","Argentina","Russia","Iran","China","Morocco","India"))
DistancesUTI$Country2 <- factor(DistancesUTI$Country2, labels = c("USA", "Israel", "Japan","France", "Chile","Argentina","Russia","Iran","China","Morocco","India"))

#get pvals and apply BH correction

require(reshape2)
mymat <- as.data.frame(ContrastsUTI)

LS <- lapply(seq_along(mymat), function(i){
  colsplit(mymat[, i], " - ", paste0(colnames(mymat)[i], letters[1:2]))
})
mydf<-data.frame(do.call('cbind', LS))
mydf<-mydf[ , colSums(is.na(mydf)) == 0]

LS <- lapply(seq_along(mydf), function(i){
  colsplit(mydf[, i], " ", paste0(colnames(mydf)[i], letters[1:2]))
})
mydf<-data.frame(do.call('cbind', LS))
mydf<-mydf[ , colSums(is.na(mydf)) == 0]


mydf<-mydf %>% filter(contrastab==contrastbb)

SignifCont2 <- mydf %>% filter(contrastab=="RL")
SignifCont2$p.valueaa<- p.adjust(SignifCont2$p.valueaa, method = "BH", n = length(SignifCont2$p.valueaa))
# SignifCont2 <- SignifCont2 %>% filter(p.valueaaa<0.05)

SignifCont3 <- mydf %>% filter(contrastab=="Lottery")
SignifCont3$p.valueaa<- p.adjust(SignifCont3$p.valueaa, method = "BH", n = length(SignifCont3$p.valueaa))
# SignifCont3 <- SignifCont3 %>% filter(p.valueaaa<0.05)

SignifContAll <- rbind(SignifCont2, SignifCont3)
DistancesUTIfilt <- DistancesUTI %>% filter(value!=0)
SignifContAll$distance <- DistancesUTIfilt$value

Distance175perfParams <- ggplot (DistancesUTI, aes(Country1, Country2, fill=value)) +
  geom_tile(color = "white") +
  scale_fill_binned_diverging(n.breaks = 10) +
  coord_fixed() +
  facet_wrap(vars(type)) +
  theme_bw() +
  theme(legend.text = element_text(angle=0, face="bold", colour="black", size="22"),
        strip.text.x =  element_text(angle=0, face="bold", colour="black", size="22"),
        legend.title = element_text(angle=0, face="bold", colour="black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line=element_line(size = 1), 
        panel.border=element_blank(), 
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=22, face="bold"),
        axis.text.y=element_text(size=22, face="bold"), axis.title.y=element_text(size=22),
        plot.title=element_text(size=20, face="bold", color="black")) +
  labs(title = "SCALING parameter values", 
       x = " ", 
       y = " ") 

PvalPlotUti_des <- edit_colors(Distance175perfParams, desaturate, amount = 1)
PvalPlotUti_BW_Params <- cowplot::ggdraw(PvalPlotUti_des)

#save all figures as pdfs

ggsave("HDI.pdf", HDI_plot, dpi = 300, width = 15, height =25, units =  "cm")
ggsave("Cultural_Distance_plot.pdf", Distance_plot, dpi = 300, width = 25, height =25, units =  "cm")
ggsave("Mapamundi.pdf", mmdf, dpi = 300, width = 35, height =25, units =  "cm")
ggsave("LearningPlot.pdf", LearningPlot, dpi = 300, width = 35, height =30, units =  "cm")
ggsave("LearningDiffPlot.pdf", LearningDiffPlot, dpi = 300, width = 30, height =30, units =  "cm")
ggsave("TransferPlot.pdf", TransferPlot, dpi = 300, width = 80, height =30, units =  "cm")
ggsave("LotteryPlot.pdf", LotteryPlot, dpi = 300, width = 80, height =30, units =  "cm")
ggsave("LotteryPlotTransfer.pdf", LotteryPlotTransfer, dpi = 300, width = 80, height =30, units =  "cm")
ggsave("Distance175perf.pdf", PvalPlotUti_BW, dpi = 300, width = 80, height =30, units =  "cm")
ggsave("Distance175perfParams.pdf", PvalPlotUti_BW_Params, dpi = 300, width = 80, height =30, units =  "cm")

###BUT WAIT!!! What about the stats? Here you go:
###############################################
#LEARNING PHASE


LEARNINGPHASE0 <- glmer(ChoseGoodorBad ~ 1 + (1|Participant.Private.ID), 
                        family=binomial, control=glmerControl(optimizer="bobyqa", 
                                                              optCtrl = list(maxfun = 1000000)), CompleteDF[CompleteDF$WhichPhase %in% c("1"),]) #convergence forced me to remove random slope here

LEARNINGPHASE1 <- glmer(ChoseGoodorBad ~ deltaEVfactor + (deltaEVfactor|Participant.Private.ID), 
                     family=binomial, control=glmerControl(optimizer="bobyqa", 
                    optCtrl = list(maxfun = 1000000)), CompleteDF[CompleteDF$WhichPhase %in% c("1"),])

LEARNINGPHASE1_bis <- glmer(ChoseGoodorBad ~ Country + (deltaEVfactor|Participant.Private.ID), 
                        family=binomial, control=glmerControl(optimizer="bobyqa", 
                        optCtrl = list(maxfun = 1000000)), CompleteDF[CompleteDF$WhichPhase %in% c("1"),])

LEARNINGPHASE2 <- glmer(ChoseGoodorBad ~ Country*deltaEVfactor + (deltaEVfactor|Participant.Private.ID), 
                        family=binomial, control=glmerControl(optimizer="bobyqa", 
                                                              optCtrl = list(maxfun = 1000000)), CompleteDF[CompleteDF$WhichPhase %in% c("1"),])

#save these models
save(LEARNINGPHASE1, LEARNINGPHASE2, file = "LEARNINGPHASEMODELS.RData")

anova(LEARNINGPHASE0,LEARNINGPHASE1,LEARNINGPHASE2)
Anova(LEARNINGPHASE2)

#Quick bayes factor of frequentist nested models
bayesfactor_models(LEARNINGPHASE1,LEARNINGPHASE2, denominator = LEARNINGPHASE1)


#Wagenmakers/Dunham operation to get the evidence strength (AICc weight ratio) for the best model
ModNull = exp((AICc(LEARNINGPHASE1)-AICc(LEARNINGPHASE1))/-2) #best model to the left
ModFull = exp((AICc(LEARNINGPHASE2)-AICc(LEARNINGPHASE1))/-2)

NullWeight = ModNull / (ModNull + ModFull)
FullWeight = ModFull / (ModNull + ModFull)
EvidenceStrength = FullWeight/NullWeight #full is x times as likely...




                             
#that's it, hope you enjoyed the ride! :)
#(you might be wondering, were are the stats? 
#well, there was no fancy coding to produce them, 
#so just follow the instructions on the paper and 
#you should be able to reproduce them just fine)

#of course, if you find yourself unable to perform the stas,
#or you see that your results do not reproduce ours,
#don't hesitate to contact us at subcomarc@gmail.com

#cheers!

