load("/Users/danna/Desktop/applied regression/project folder/Final_Survey_Data.RData")


load("/Users/danna/Desktop/applied regression/project folder/SVM+Weighted.RData")
load("/Users/danna/Desktop/applied regression/project folder/Terrorism.RData")


#============================================================================================================================================================================================#
#How the Public Defines Terrorism Replication Code
#Connor Huff & Josh Kertzer
#Last modified: June 1, 2017

## This R file contains the code necessary to replicate the analysis in the main text Its companion R file ("Terrorism_Replication_2.R") replicates the analysis in the appendix.
# All of the following analyses were carried out using R version 3.1.1 on a 2.5 GHz Intel Core i5 iMac running OS X El Capitan 10.11.6


##The script which follows is comprised of two components. Component 1 will read in the survey data from qualtrics and then clean the data to prepare for analyses. Component 2 will replicate the analyses in the main text. 
#To save time in replicating, if you don't want to build the dataframe yourself, you can start on line 603, which loads "Terrorism.RDATA"
rm(list=ls())

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#loading packages
ipak <- function(pkg){  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if(length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
sapply(pkg, require, character.only = TRUE)
}

packages <- c("car", "lubridate", "snow", "xtable", "tm", "stringr") 
ipak(packages)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Loading a RData object containing all of the survey data used in the manuscript and supplementary appendix. 
setwd("/Users/danna/Desktop/applied regression")
#Set dataframe to path of choice

load("/Users/danna/Desktop/applied regression/project folder/Final_Survey_Data.RDATA")

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#To analyze this we're going to create new version of the dataset, with a variable for the round (1-8), and treatment
dat2 <- data.frame(ID=rep(dat$pid, each=8), Round=c(rep(rep(1:8, each=1),nrow(dat))), Tactic=NA, Casualties=NA, Actor=NA, Ideology=NA, Motivation=NA, Target=NA, Location=NA)

#Go through the dataset in blocks by participant ID. This will give us the randomization received by each individual for each round
for (i in unique(dat$pid)){
  block <- which(dat2$ID==i) #Find block in new dataset pertaining to participant i 
  loc <- which(dat$pid==i) #Find location in old dataset pertaining to participant i
  
  #Now, for that participant, figure out the order the feature rows appeared in the table
  for(k in 1:8){
    dat2$Tactic[block][k] <- as.character(dat[loc, which(colnames(dat)==paste("Tactic", k, sep=""))]) ##this gives the correct column:as.character(dat[loc, which(colnames(dat)==paste("Tactic", k, sep=""))]
    dat2$Casualties[block][k] <- as.character(dat[loc, which(colnames(dat)==paste("Casualties", k, sep=""))])
    dat2$Actor[block][k] <- as.character(dat[loc, which(colnames(dat)==paste("Actor", k, sep=""))])
    dat2$Ideology[block][k] <- as.character(dat[loc, which(colnames(dat)==paste("Ideology", k, sep=""))])
    dat2$Motivation[block][k] <- as.character(dat[loc, which(colnames(dat)==paste("Motivation", k, sep=""))])
    dat2$Target[block][k] <- as.character(dat[loc, which(colnames(dat)==paste("Target", k, sep=""))])
    dat2$Location[block][k] <- as.character(dat[loc, which(colnames(dat)==paste("Location", k, sep=""))])
    
  }
}


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Now merge back in the main Dependent variable: Terrorism Binary
dat2$TerrorBinary <- NA

data <- dat
#Using the same code as above to grab the dependent variable for each individual i for each round
for (i in unique(data$pid)){
  block <- which(dat2$ID==i) #Find block in new dataset pertaining to participant i 
  loc <- which(data$pid==i) #Find location in old dataset pertaining to participant i
  
  
  ##the main dependent variable
  dat2$TerrorBinary[block][1] <- as.character(data[loc, which(colnames(data)=="terrbin1")]) 
  dat2$TerrorBinary[block][2] <- as.character(data[loc, which(colnames(data)=="terrbin2")])   
  dat2$TerrorBinary[block][3] <- as.character(data[loc, which(colnames(data)=="terrbin3")]) 
  dat2$TerrorBinary[block][4] <- as.character(data[loc, which(colnames(data)=="terrbin4")]) 
  dat2$TerrorBinary[block][5] <- as.character(data[loc, which(colnames(data)=="terrbin5")]) 
  dat2$TerrorBinary[block][6] <- as.character(data[loc, which(colnames(data)=="terrbin6")]) 
  dat2$TerrorBinary[block][7] <- as.character(data[loc, which(colnames(data)=="terrbin7")]) 
  dat2$TerrorBinary[block][8] <- as.character(data[loc, which(colnames(data)=="terrbin8")]) 
  
  
}


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Specify levels of factors including reference level and ordering

##Specifying less violent incidents as the baseline
dat2$Tactic <- factor(dat2$Tactic, levels=c("protest", "hostage taking", "shooting", "bombing"))
dat2$Casualties <- factor(dat2$Casualties, levels=c("were no individuals", "was one individual", "were two individuals", "were ten individuals"))


##setting individual as the baseline, this will allow us to explore varying intentionality
dat2$Actor <- factor(dat2$Actor, levels=c("individual", "individual with a history of mental illness",
                                          "group", "organization", 
                                          "organization with ties to the United States", 
                                          "organization with ties to a foreign government"))

dat2$Ideology <- factor(dat2$Ideology,levels=c("an", "a Christian", "a Muslim", "a left-wing", "a right-wing"))

dat2$Motivation <- factor(dat2$Motivation, levels=c("News reports suggest the individual had been in an ongoing personal dispute with one of the targets.",
                                                    "News reports suggest that there was no clear motivation for the incident.", 
                                                    "News reports suggest the incident was motivated by hatred towards the target.",
                                                    "News reports suggest the incident was motivated by the goal of changing government policy.",
                                                    "News reports suggest the incident was motivated by the goal of overthrowing the government."))

dat2$Target <- factor(dat2$Target, levels=c("military facility", "police station", "school", 
                                            "Christian community center", "church", 
                                            "Muslim community center", "mosque",
                                            "Jewish community center", "synagogue"))

dat2$Location <- factor(dat2$Location, levels=c("the United States", "a foreign democracy", "a foreign democracy with a history of human rights violations",
                                                "a foreign dictatorship", "a foreign dictatorship with a history of human rights violations"))

#1 is a yes, 0 is a no
dat2$TerrorBinary <- recode(dat2$TerrorBinary, "c('2')=0; c('1')=1") 
dat2$TerrorBinary <- as.numeric(dat2$TerrorBinary)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##Generating factor variables which will be used for the interactions

#creating factor variables
dat2$Tactic_Casualty_Interaction <- NA
dat2$Tactic_Casualty_Interaction[which(dat2$Tactic=="bombing" & dat2$Casualties=="were no individuals")] <- "Bombing no casualties"
dat2$Tactic_Casualty_Interaction[which(dat2$Tactic=="shooting" & dat2$Casualties=="were no individuals")] <- "Shooting no casualties"
dat2$Tactic_Casualty_Interaction[which(dat2$Tactic=="shooting" & dat2$Casualties=="was one individual")] <- "Shooting one casualty"
dat2$Tactic_Casualty_Interaction[which(dat2$Tactic=="shooting" & dat2$Casualties=="were two individuals")] <- "Shooting two casualties"
dat2$Tactic_Casualty_Interaction[which(dat2$Tactic=="shooting" & dat2$Casualties=="were ten individuals")] <- "Shooting ten casualties"
dat2$Tactic_Casualty_Interaction[which(dat2$Tactic=="hostage taking" & dat2$Casualties=="were no individuals")] <- "Hostage taking no casualties"
dat2$Tactic_Casualty_Interaction[which(dat2$Tactic=="hostage taking" & dat2$Casualties=="was one individual")] <- "Hostage taking one casualty"
dat2$Tactic_Casualty_Interaction[which(dat2$Tactic=="hostage taking" & dat2$Casualties=="were two individuals")] <- "Hostage taking two casualties"
dat2$Tactic_Casualty_Interaction[which(dat2$Tactic=="hostage taking" & dat2$Casualties=="were ten individuals")] <- "Hostage taking ten casualties"

##Ording levels of the factor
dat2$Tactic_Casualty_Interaction <- factor(dat2$Tactic_Casualty_Interaction, levels=c("Bombing no casualties", 
                                                                                      "Shooting no casualties", "Shooting one casualty", "Shooting two casualties", "Shooting ten casualties",
                                                                                      "Hostage taking no casualties", "Hostage taking one casualty", "Hostage taking two casualties", "Hostage taking ten casualties"))





#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##Interaction between religion and mental illness
dat2$Religion_Mental_Illness_Interaction <- NA
dat2$Religion_Mental_Illness_Interaction[which(dat2$Actor=="individual" & dat2$Ideology=="an")] <- "An Individual"
dat2$Religion_Mental_Illness_Interaction[which(dat2$Actor=="individual" & dat2$Ideology=="a Christian")] <- "Christian Individual"
dat2$Religion_Mental_Illness_Interaction[which(dat2$Actor=="individual" & dat2$Ideology=="a Muslim")] <- "Muslim Individual"
dat2$Religion_Mental_Illness_Interaction[which(dat2$Actor=="individual" & dat2$Ideology=="a left-wing")] <- "Left-Wing Individual"
dat2$Religion_Mental_Illness_Interaction[which(dat2$Actor=="individual" & dat2$Ideology=="a right-wing")] <- "Right-Wing Individual"
dat2$Religion_Mental_Illness_Interaction[which(dat2$Actor=="individual with a history of mental illness" & dat2$Ideology=="an")] <- "Individual Mental Illness"
dat2$Religion_Mental_Illness_Interaction[which(dat2$Actor=="individual with a history of mental illness" & dat2$Ideology=="a Christian")] <- "Christian Individual Mental Illness"
dat2$Religion_Mental_Illness_Interaction[which(dat2$Actor=="individual with a history of mental illness" & dat2$Ideology=="a Muslim")] <- "Muslim Individual Mental Illness"
dat2$Religion_Mental_Illness_Interaction[which(dat2$Actor=="individual with a history of mental illness" & dat2$Ideology=="a left-wing")] <- "Left-Wing Individual Mental Illness"
dat2$Religion_Mental_Illness_Interaction[which(dat2$Actor=="individual with a history of mental illness" & dat2$Ideology=="a right-wing")] <- "Right-Wing Individual Mental Illness"


dat2$Religion_Mental_Illness_Interaction <- factor(dat2$Religion_Mental_Illness_Interaction, 
                                                   levels=c("An Individual", 
                                                            "Christian Individual", "Christian Individual Mental Illness",
                                                            "Muslim Individual", "Muslim Individual Mental Illness",
                                                            "Left-Wing Individual", "Left-Wing Individual Mental Illness",
                                                            "Right-Wing Individual", "Right-Wing Individual Mental Illness"))


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Motivation Tactic Interaction
dat2$Motivation_Tactic_Interaction <- NA

dat2$Motivation_Tactic_Interaction[which(dat2$Motivation=="News reports suggest the individual had been in an ongoing personal dispute with one of the targets." 
                                         & dat2$Tactic=="protest")] <- "Personal Dispute, Protest"
dat2$Motivation_Tactic_Interaction[which(dat2$Motivation=="News reports suggest the individual had been in an ongoing personal dispute with one of the targets."
                                         & dat2$Tactic=="hostage taking")] <- "Personal Dispute, Hostage Taking"
dat2$Motivation_Tactic_Interaction[which(dat2$Motivation=="News reports suggest the individual had been in an ongoing personal dispute with one of the targets." 
                                         & dat2$Tactic=="shooting")] <- "Personal Dispute, Shooting"
dat2$Motivation_Tactic_Interaction[which(dat2$Motivation=="News reports suggest the individual had been in an ongoing personal dispute with one of the targets." 
                                         & dat2$Tactic=="bombing")] <- "Personal Dispute, Bombing"


dat2$Motivation_Tactic_Interaction[which(dat2$Motivation=="News reports suggest that there was no clear motivation for the incident." 
                                         & dat2$Tactic=="protest")] <- "Motivation Unclear, Protest"
dat2$Motivation_Tactic_Interaction[which(dat2$Motivation=="News reports suggest that there was no clear motivation for the incident."
                                         & dat2$Tactic=="hostage taking")] <- "Motivation Unclear, Hostage Taking"
dat2$Motivation_Tactic_Interaction[which(dat2$Motivation=="News reports suggest that there was no clear motivation for the incident." 
                                         & dat2$Tactic=="shooting")] <- "Motivation Unclear, Shooting"
dat2$Motivation_Tactic_Interaction[which(dat2$Motivation=="News reports suggest that there was no clear motivation for the incident." 
                                         & dat2$Tactic=="bombing")] <- "Motivation Unclear, Bombing"


dat2$Motivation_Tactic_Interaction[which(dat2$Motivation=="News reports suggest the incident was motivated by hatred towards the target." 
                                         & dat2$Tactic=="protest")] <- "Motivation Hatred, Protest"
dat2$Motivation_Tactic_Interaction[which(dat2$Motivation=="News reports suggest the incident was motivated by hatred towards the target."
                                         & dat2$Tactic=="hostage taking")] <- "Motivation Hatred, Hostage Taking"
dat2$Motivation_Tactic_Interaction[which(dat2$Motivation=="News reports suggest the incident was motivated by hatred towards the target." 
                                         & dat2$Tactic=="shooting")] <- "Motivation Hatred, Shooting"
dat2$Motivation_Tactic_Interaction[which(dat2$Motivation=="News reports suggest the incident was motivated by hatred towards the target." 
                                         & dat2$Tactic=="bombing")] <- "Motivation Hatred, Bombing"


dat2$Motivation_Tactic_Interaction[which(dat2$Motivation=="News reports suggest the incident was motivated by the goal of changing government policy." 
                                         & dat2$Tactic=="protest")] <- "Motivation Policy, Protest"
dat2$Motivation_Tactic_Interaction[which(dat2$Motivation=="News reports suggest the incident was motivated by the goal of changing government policy."
                                         & dat2$Tactic=="hostage taking")] <- "Motivation Policy, Hostage Taking"
dat2$Motivation_Tactic_Interaction[which(dat2$Motivation=="News reports suggest the incident was motivated by the goal of changing government policy." 
                                         & dat2$Tactic=="shooting")] <- "Motivation Policy, Shooting"
dat2$Motivation_Tactic_Interaction[which(dat2$Motivation=="News reports suggest the incident was motivated by the goal of changing government policy." 
                                         & dat2$Tactic=="bombing")] <- "Motivation Policy, Bombing"


dat2$Motivation_Tactic_Interaction[which(dat2$Motivation=="News reports suggest the incident was motivated by the goal of overthrowing the government." 
                                         & dat2$Tactic=="protest")] <- "Motivation Overthrow, Protest"
dat2$Motivation_Tactic_Interaction[which(dat2$Motivation=="News reports suggest the incident was motivated by the goal of overthrowing the government."
                                         & dat2$Tactic=="hostage taking")] <- "Motivation Overthrow, Hostage Taking"
dat2$Motivation_Tactic_Interaction[which(dat2$Motivation=="News reports suggest the incident was motivated by the goal of overthrowing the government." 
                                         & dat2$Tactic=="shooting")] <- "Motivation Overthrow, Shooting"
dat2$Motivation_Tactic_Interaction[which(dat2$Motivation=="News reports suggest the incident was motivated by the goal of overthrowing the government." 
                                         & dat2$Tactic=="bombing")] <- "Motivation Overthrow, Bombing"


dat2$Motivation_Tactic_Interaction <- factor(dat2$Motivation_Tactic_Interaction, 
                                             levels=c("Motivation Unclear, Protest", "Motivation Unclear, Hostage Taking", "Motivation Unclear, Shooting", "Motivation Unclear, Bombing", 
                                                      "Personal Dispute, Protest", "Personal Dispute, Hostage Taking", "Personal Dispute, Shooting", "Personal Dispute, Bombing",                                                             
                                                      "Motivation Hatred, Protest", "Motivation Hatred, Hostage Taking", "Motivation Hatred, Shooting", "Motivation Hatred, Bombing", 
                                                      "Motivation Policy, Protest", "Motivation Policy, Hostage Taking", "Motivation Policy, Shooting", "Motivation Policy, Bombing", 
                                                      "Motivation Overthrow, Protest", "Motivation Overthrow, Hostage Taking", "Motivation Overthrow, Shooting", "Motivation Overthrow, Bombing"))


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Actor Tactic Interaction
dat2$Actor_Tactic_Interaction <- NA

dat2$Actor_Tactic_Interaction[which(dat2$Actor=="individual" & dat2$Tactic=="protest")] <- "Individual, Protest"
dat2$Actor_Tactic_Interaction[which(dat2$Actor=="individual" & dat2$Tactic=="hostage taking")] <- "Individual, Hostage Taking"
dat2$Actor_Tactic_Interaction[which(dat2$Actor=="individual" & dat2$Tactic=="shooting")] <- "Individual, Shooting"
dat2$Actor_Tactic_Interaction[which(dat2$Actor=="individual" & dat2$Tactic=="bombing")] <- "Individual, Bombing"

dat2$Actor_Tactic_Interaction[which(dat2$Actor=="individual with a history of mental illness" & dat2$Tactic=="protest")] <- "Individual Mental Illness, Protest"
dat2$Actor_Tactic_Interaction[which(dat2$Actor=="individual with a history of mental illness" & dat2$Tactic=="hostage taking")] <- "Individual Mental Illness, Hostage Taking"
dat2$Actor_Tactic_Interaction[which(dat2$Actor=="individual with a history of mental illness" & dat2$Tactic=="shooting")] <- "Individual Mental Illness, Shooting"
dat2$Actor_Tactic_Interaction[which(dat2$Actor=="individual with a history of mental illness" & dat2$Tactic=="bombing")] <- "Individual Mental Illness, Bombing"

dat2$Actor_Tactic_Interaction[which(dat2$Actor=="group" & dat2$Tactic=="protest")] <- "Group, Protest"
dat2$Actor_Tactic_Interaction[which(dat2$Actor=="group" & dat2$Tactic=="hostage taking")] <- "Group, Hostage Taking"
dat2$Actor_Tactic_Interaction[which(dat2$Actor=="group" & dat2$Tactic=="shooting")] <- "Group, Shooting"
dat2$Actor_Tactic_Interaction[which(dat2$Actor=="group" & dat2$Tactic=="bombing")] <- "Group, Bombing"

dat2$Actor_Tactic_Interaction[which(dat2$Actor=="organization" & dat2$Tactic=="protest")] <- "Organization, Protest"
dat2$Actor_Tactic_Interaction[which(dat2$Actor=="organization" & dat2$Tactic=="hostage taking")] <- "Organization, Hostage Taking"
dat2$Actor_Tactic_Interaction[which(dat2$Actor=="organization" & dat2$Tactic=="shooting")] <- "Organization, Shooting"
dat2$Actor_Tactic_Interaction[which(dat2$Actor=="organization" & dat2$Tactic=="bombing")] <- "Organization, Bombing"

dat2$Actor_Tactic_Interaction[which(dat2$Actor=="organization with ties to the United States" & dat2$Tactic=="protest")] <- "Organization US Ties, Protest"
dat2$Actor_Tactic_Interaction[which(dat2$Actor=="organization with ties to the United States" & dat2$Tactic=="hostage taking")] <- "Organization US Ties, Hostage Taking"
dat2$Actor_Tactic_Interaction[which(dat2$Actor=="organization with ties to the United States" & dat2$Tactic=="shooting")] <- "Organization US Ties, Shooting"
dat2$Actor_Tactic_Interaction[which(dat2$Actor=="organization with ties to the United States" & dat2$Tactic=="bombing")] <- "Organization US Ties, Bombing"

dat2$Actor_Tactic_Interaction[which(dat2$Actor=="organization with ties to a foreign government" & dat2$Tactic=="protest")] <- "Organization Foreign Ties, Protest"
dat2$Actor_Tactic_Interaction[which(dat2$Actor=="organization with ties to a foreign government" & dat2$Tactic=="hostage taking")] <- "Organization Foreign Ties, Hostage Taking"
dat2$Actor_Tactic_Interaction[which(dat2$Actor=="organization with ties to a foreign government" & dat2$Tactic=="shooting")] <- "Organization Foreign Ties, Shooting"
dat2$Actor_Tactic_Interaction[which(dat2$Actor=="organization with ties to a foreign government" & dat2$Tactic=="bombing")] <- "Organization Foreign Ties, Bombing"

dat2$Actor_Tactic_Interaction <- factor(dat2$Actor_Tactic_Interaction, 
                                        levels=c("Individual, Protest", "Individual, Hostage Taking", "Individual, Shooting", "Individual, Bombing",
                                                 "Individual Mental Illness, Protest", "Individual Mental Illness, Hostage Taking", "Individual Mental Illness, Shooting", "Individual Mental Illness, Bombing",
                                                 "Group, Protest", "Group, Hostage Taking", "Group, Shooting", "Group, Bombing",
                                                 "Organization, Protest", "Organization, Hostage Taking", "Organization, Shooting", "Organization, Bombing",
                                                 "Organization US Ties, Protest", "Organization US Ties, Hostage Taking", "Organization US Ties, Shooting", "Organization US Ties, Bombing",
                                                 "Organization Foreign Ties, Protest", "Organization Foreign Ties, Hostage Taking", "Organization Foreign Ties, Shooting", "Organization Foreign Ties, Bombing"))



#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##Actor Motivation Interaction
dat2$Actor_Motivation_Interaction <- NA

dat2$Actor_Motivation_Interaction[which(dat2$Actor=="individual" & 
                                          dat2$Motivation=="News reports suggest the individual had been in an ongoing personal dispute with one of the targets.")] <- "Individual, Personal Dispute"
dat2$Actor_Motivation_Interaction[which(dat2$Actor=="individual" 
                                        & dat2$Motivation=="News reports suggest that there was no clear motivation for the incident.")] <- "Individual, Unclear Motivation"
dat2$Actor_Motivation_Interaction[which(dat2$Actor=="individual" 
                                        & dat2$Motivation=="News reports suggest the incident was motivated by hatred towards the target.")] <- "Individual, Hatred"
dat2$Actor_Motivation_Interaction[which(dat2$Actor=="individual" 
                                        & dat2$Motivation=="News reports suggest the incident was motivated by the goal of changing government policy.")] <- "Individual, Policy"
dat2$Actor_Motivation_Interaction[which(dat2$Actor=="individual" 
                                        & dat2$Motivation=="News reports suggest the incident was motivated by the goal of overthrowing the government.")] <- "Individual, Overthrow"


dat2$Actor_Motivation_Interaction[which(dat2$Actor=="individual with a history of mental illness" & 
                                          dat2$Motivation=="News reports suggest the individual had been in an ongoing personal dispute with one of the targets.")] <- "Individual Mental Illness, Personal Dispute"
dat2$Actor_Motivation_Interaction[which(dat2$Actor=="individual with a history of mental illness" 
                                        & dat2$Motivation=="News reports suggest that there was no clear motivation for the incident.")] <- "Individual Mental Illness, Unclear Motivation"
dat2$Actor_Motivation_Interaction[which(dat2$Actor=="individual with a history of mental illness" 
                                        & dat2$Motivation=="News reports suggest the incident was motivated by hatred towards the target.")] <- "Individual Mental Illness, Hatred"
dat2$Actor_Motivation_Interaction[which(dat2$Actor=="individual with a history of mental illness" 
                                        & dat2$Motivation=="News reports suggest the incident was motivated by the goal of changing government policy.")] <- "Individual Mental Illness, Policy"
dat2$Actor_Motivation_Interaction[which(dat2$Actor=="individual with a history of mental illness" 
                                        & dat2$Motivation=="News reports suggest the incident was motivated by the goal of overthrowing the government.")] <- "Individual Mental Illness, Overthrow"


dat2$Actor_Motivation_Interaction[which(dat2$Actor=="group" & 
                                          dat2$Motivation=="News reports suggest the individual had been in an ongoing personal dispute with one of the targets.")] <- "Group, Personal Dispute"
dat2$Actor_Motivation_Interaction[which(dat2$Actor=="group" 
                                        & dat2$Motivation=="News reports suggest that there was no clear motivation for the incident.")] <- "Group, Unclear Motivation"
dat2$Actor_Motivation_Interaction[which(dat2$Actor=="group" 
                                        & dat2$Motivation=="News reports suggest the incident was motivated by hatred towards the target.")] <- "Group, Hatred"
dat2$Actor_Motivation_Interaction[which(dat2$Actor=="group" 
                                        & dat2$Motivation=="News reports suggest the incident was motivated by the goal of changing government policy.")] <- "Group, Policy"
dat2$Actor_Motivation_Interaction[which(dat2$Actor=="group" 
                                        & dat2$Motivation=="News reports suggest the incident was motivated by the goal of overthrowing the government.")] <- "Group, Overthrow"


dat2$Actor_Motivation_Interaction[which(dat2$Actor=="organization" & 
                                          dat2$Motivation=="News reports suggest the individual had been in an ongoing personal dispute with one of the targets.")] <- "Organization, Personal Dispute"
dat2$Actor_Motivation_Interaction[which(dat2$Actor=="organization" 
                                        & dat2$Motivation=="News reports suggest that there was no clear motivation for the incident.")] <- "Organization, Unclear Motivation"
dat2$Actor_Motivation_Interaction[which(dat2$Actor=="organization" 
                                        & dat2$Motivation=="News reports suggest the incident was motivated by hatred towards the target.")] <- "Organization, Hatred"
dat2$Actor_Motivation_Interaction[which(dat2$Actor=="organization" 
                                        & dat2$Motivation=="News reports suggest the incident was motivated by the goal of changing government policy.")] <- "Organization, Policy"
dat2$Actor_Motivation_Interaction[which(dat2$Actor=="organization" 
                                        & dat2$Motivation=="News reports suggest the incident was motivated by the goal of overthrowing the government.")] <- "Organization, Overthrow"


dat2$Actor_Motivation_Interaction[which(dat2$Actor=="organization with ties to the United States" & 
                                          dat2$Motivation=="News reports suggest the individual had been in an ongoing personal dispute with one of the targets.")] <- "Organization US Ties, Personal Dispute"
dat2$Actor_Motivation_Interaction[which(dat2$Actor=="organization with ties to the United States" 
                                        & dat2$Motivation=="News reports suggest that there was no clear motivation for the incident.")] <- "Organization US Ties, Unclear Motivation"
dat2$Actor_Motivation_Interaction[which(dat2$Actor=="organization with ties to the United States" 
                                        & dat2$Motivation=="News reports suggest the incident was motivated by hatred towards the target.")] <- "Organization US Ties, Hatred"
dat2$Actor_Motivation_Interaction[which(dat2$Actor=="organization with ties to the United States" 
                                        & dat2$Motivation=="News reports suggest the incident was motivated by the goal of changing government policy.")] <- "Organization US Ties, Policy"
dat2$Actor_Motivation_Interaction[which(dat2$Actor=="organization with ties to the United States" 
                                        & dat2$Motivation=="News reports suggest the incident was motivated by the goal of overthrowing the government.")] <- "Organization US Ties, Overthrow"


dat2$Actor_Motivation_Interaction[which(dat2$Actor=="organization with ties to a foreign government" & 
                                          dat2$Motivation=="News reports suggest the individual had been in an ongoing personal dispute with one of the targets.")] <- "Organization Foreign Ties, Personal Dispute"
dat2$Actor_Motivation_Interaction[which(dat2$Actor=="organization with ties to a foreign government" 
                                        & dat2$Motivation=="News reports suggest that there was no clear motivation for the incident.")] <- "Organization Foreign Ties, Unclear Motivation"
dat2$Actor_Motivation_Interaction[which(dat2$Actor=="organization with ties to a foreign government" 
                                        & dat2$Motivation=="News reports suggest the incident was motivated by hatred towards the target.")] <- "Organization Foreign Ties, Hatred"
dat2$Actor_Motivation_Interaction[which(dat2$Actor=="organization with ties to a foreign government" 
                                        & dat2$Motivation=="News reports suggest the incident was motivated by the goal of changing government policy.")] <- "Organization Foreign Ties, Policy"
dat2$Actor_Motivation_Interaction[which(dat2$Actor=="organization with ties to a foreign government" 
                                        & dat2$Motivation=="News reports suggest the incident was motivated by the goal of overthrowing the government.")] <- "Organization Foreign Ties, Overthrow"


dat2$Actor_Motivation_Interaction <- factor(dat2$Actor_Motivation_Interaction, 
                                            levels=c("Individual, Personal Dispute", "Individual, Unclear Motivation", "Individual, Hatred", "Individual, Policy", "Individual, Overthrow",
                                                     "Individual Mental Illness, Personal Dispute", "Individual Mental Illness, Unclear Motivation", "Individual Mental Illness, Hatred", "Individual Mental Illness, Policy", "Individual Mental Illness, Overthrow",
                                                     "Group, Personal Dispute", "Group, Unclear Motivation", "Group, Hatred", "Group, Policy", "Group, Overthrow",
                                                     "Organization, Personal Dispute", "Organization, Unclear Motivation", "Organization, Hatred", "Organization, Policy", "Organization, Overthrow",
                                                     "Organization US Ties, Personal Dispute", "Organization US Ties, Unclear Motivation", "Organization US Ties, Hatred", "Organization US Ties, Policy", "Organization US Ties, Overthrow",
                                                     "Organization Foreign Ties, Personal Dispute", "Organization Foreign Ties, Unclear Motivation", "Organization Foreign Ties, Hatred", "Organization Foreign Ties, Policy", "Organization Foreign Ties, Overthrow"))


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##Target and Tactic Interaction
dat2$Target_Tactic_Interaction <- NA

dat2$Target_Tactic_Interaction[which(dat2$Target=="military facility" & dat2$Tactic=="protest")] <- "Military, Protest"
dat2$Target_Tactic_Interaction[which(dat2$Target=="military facility" & dat2$Tactic=="hostage taking")] <- "Military, Hostage Taking"
dat2$Target_Tactic_Interaction[which(dat2$Target=="military facility" & dat2$Tactic=="shooting")] <- "Military, Shooting"
dat2$Target_Tactic_Interaction[which(dat2$Target=="military facility" & dat2$Tactic=="bombing")] <- "Military, Bombing"

dat2$Target_Tactic_Interaction[which(dat2$Target=="police station" & dat2$Tactic=="protest")] <- "Police, Protest"
dat2$Target_Tactic_Interaction[which(dat2$Target=="police station" & dat2$Tactic=="hostage taking")] <- "Police, Hostage Taking"
dat2$Target_Tactic_Interaction[which(dat2$Target=="police station" & dat2$Tactic=="shooting")] <- "Police, Shooting"
dat2$Target_Tactic_Interaction[which(dat2$Target=="police station" & dat2$Tactic=="bombing")] <- "Police, Bombing"

dat2$Target_Tactic_Interaction[which(dat2$Target=="school" & dat2$Tactic=="protest")] <- "School, Protest"
dat2$Target_Tactic_Interaction[which(dat2$Target=="school" & dat2$Tactic=="hostage taking")] <- "School, Hostage Taking"
dat2$Target_Tactic_Interaction[which(dat2$Target=="school" & dat2$Tactic=="shooting")] <- "School, Shooting"
dat2$Target_Tactic_Interaction[which(dat2$Target=="school" & dat2$Tactic=="bombing")] <- "School, Bombing"

dat2$Target_Tactic_Interaction[which(dat2$Target=="Christian community center" & dat2$Tactic=="protest")] <- "Christian Center, Protest"
dat2$Target_Tactic_Interaction[which(dat2$Target=="Christian community center" & dat2$Tactic=="hostage taking")] <- "Christian Center, Hostage Taking"
dat2$Target_Tactic_Interaction[which(dat2$Target=="Christian community center" & dat2$Tactic=="shooting")] <- "Christian Center, Shooting"
dat2$Target_Tactic_Interaction[which(dat2$Target=="Christian community center" & dat2$Tactic=="bombing")] <- "Christian Center, Bombing"

dat2$Target_Tactic_Interaction[which(dat2$Target=="church" & dat2$Tactic=="protest")] <- "Church, Protest"
dat2$Target_Tactic_Interaction[which(dat2$Target=="church" & dat2$Tactic=="hostage taking")] <- "Church, Hostage Taking"
dat2$Target_Tactic_Interaction[which(dat2$Target=="church" & dat2$Tactic=="shooting")] <- "Church, Shooting"
dat2$Target_Tactic_Interaction[which(dat2$Target=="church" & dat2$Tactic=="bombing")] <- "Church, Bombing"


dat2$Target_Tactic_Interaction[which(dat2$Target=="Muslim community center" & dat2$Tactic=="protest")] <- "Muslim Center, Protest"
dat2$Target_Tactic_Interaction[which(dat2$Target=="Muslim community center" & dat2$Tactic=="hostage taking")] <- "Muslim Center, Hostage Taking"
dat2$Target_Tactic_Interaction[which(dat2$Target=="Muslim community center" & dat2$Tactic=="shooting")] <- "Muslim Center, Shooting"
dat2$Target_Tactic_Interaction[which(dat2$Target=="Muslim community center" & dat2$Tactic=="bombing")] <- "Muslim Center, Bombing"


dat2$Target_Tactic_Interaction[which(dat2$Target=="mosque" & dat2$Tactic=="protest")] <- "Mosque, Protest"
dat2$Target_Tactic_Interaction[which(dat2$Target=="mosque" & dat2$Tactic=="hostage taking")] <- "Mosque, Hostage Taking"
dat2$Target_Tactic_Interaction[which(dat2$Target=="mosque" & dat2$Tactic=="shooting")] <- "Mosque, Shooting"
dat2$Target_Tactic_Interaction[which(dat2$Target=="mosque" & dat2$Tactic=="bombing")] <- "Mosque, Bombing"


dat2$Target_Tactic_Interaction[which(dat2$Target=="Jewish community center" & dat2$Tactic=="protest")] <- "Jewish Center, Protest"
dat2$Target_Tactic_Interaction[which(dat2$Target=="Jewish community center" & dat2$Tactic=="hostage taking")] <- "Jewish Center, Hostage Taking"
dat2$Target_Tactic_Interaction[which(dat2$Target=="Jewish community center" & dat2$Tactic=="shooting")] <- "Jewish Center, Shooting"
dat2$Target_Tactic_Interaction[which(dat2$Target=="Jewish community center" & dat2$Tactic=="bombing")] <- "Jewish Center, Bombing"


dat2$Target_Tactic_Interaction[which(dat2$Target=="synagogue" & dat2$Tactic=="protest")] <- "Synagogue, Protest"
dat2$Target_Tactic_Interaction[which(dat2$Target=="synagogue" & dat2$Tactic=="hostage taking")] <- "Synagogue, Hostage Taking"
dat2$Target_Tactic_Interaction[which(dat2$Target=="synagogue" & dat2$Tactic=="shooting")] <- "Synagogue, Shooting"
dat2$Target_Tactic_Interaction[which(dat2$Target=="synagogue" & dat2$Tactic=="bombing")] <- "Synagogue, Bombing"


dat2$Target_Tactic_Interaction <- factor(dat2$Target_Tactic_Interaction, 
                                         levels=c("Military, Protest", "Military, Hostage Taking", "Military, Shooting", "Military, Bombing",
                                                  "Police, Protest", "Police, Hostage Taking", "Police, Shooting", "Police, Bombing",
                                                  "School, Protest", "School, Hostage Taking", "School, Shooting", "School, Bombing",
                                                  "Christian Center, Protest", "Christian Center, Hostage Taking", "Christian Center, Shooting", "Christian Center, Bombing",
                                                  "Church, Protest", "Church, Hostage Taking", "Church, Shooting", "Church, Bombing",
                                                  "Muslim Center, Protest", "Muslim Center, Hostage Taking", "Muslim Center, Shooting", "Muslim Center, Bombing",
                                                  "Mosque, Protest", "Mosque, Hostage Taking", "Mosque, Shooting", "Mosque, Bombing",
                                                  "Jewish Center, Protest", "Jewish Center, Hostage Taking", "Jewish Center, Shooting", "Jewish Center, Bombing",
                                                  "Synagogue, Protest", "Synagogue, Hostage Taking", "Synagogue, Shooting", "Synagogue, Bombing"))


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##Merging in Covariates including ideology and the individual level weights
load("/Users/danna/Desktop/applied regression/project folder/Terrorism.RDATA")

#ideology
data$ideorate_numeric <- as.numeric(as.character(data$ideorate))
data$conservative <- as.numeric((data$ideorate_numeric==1|data$ideorate_numeric==2)) ##either extremely conservative or conservative
data$liberal <- as.numeric((data$ideorate_numeric==6|data$ideorate_numeric==7)) ##either extremely liberal or liberal


##Islamoprejudice (these three variables measure "islamoprejudice" as defined in Imhoff and Recker (2012)
#recode to create an islamoprejudice score where high values correspond to high on the islamoprejudice scale
data$shar_ethic_numeric <- as.numeric(as.character(data$shar_ethic))
data$shar_ethic_recoded <- recode(data$shar_ethic_numeric, "c('1')=5; c('2')=4; c('3')=3; c('4')=2; c('5')=1",as.factor.result=FALSE)  
data$islamoprejudice_score <- (as.numeric(as.character(data$islam_arch))+as.numeric(as.character(data$shar_ethic_recoded)) + as.numeric(as.character(data$islam_terr)))/15

data$islamoprejudicehigh <- as.numeric(data$islamoprejudice_score>mean(data$islamoprejudice_score, na.rm=T) )
data$islamoprejudicelow <- as.numeric(data$islamoprejudice_score<mean(data$islamoprejudice_score, na.rm=T) )


##Islam Secular Critique
data$islamosecular_score <- (as.numeric(as.character(data$relig_lit))+as.numeric(as.character(data$chrch_stat)))/10

data$islamosecularhigh <- as.numeric(data$islamosecular_score>mean(data$islamosecular_score, na.rm=T) )
data$islamosecularlow <- as.numeric(data$islamosecular_score<mean(data$islamosecular_score, na.rm=T) )


dat2$Conservative <- NULL
dat2$Liberal <- NULL
dat2$webal2 <- NULL

#Islamo Prejudice
dat2$HighIslamoPrej <- NA
dat2$LowIslamoPrej <- NA

dat2$HighIslamoSec <- NA
dat2$LowIslamoSec <- NA



#ideorate, birthyr, education, male
dat2$Ideorate <- NA
dat2$Birthyr <- NA
dat2$Education <- NA
dat2$Male <- NA


#Using the same code as above to grab the dependent variable for each individual i for each round. However, this time it will merge in the conservative, liberal, and weight variables
for (i in unique(data$pid)){
  block <- which(dat2$ID==i) #Find block in new dataset pertaining to participant i 
  loc <- which(data$pid==i) #Find location in old dataset pertaining to participant i
  
  
  ##conservative
  dat2$Conservative[block][1] <- as.character(data[loc, which(colnames(data)=="conservative")]) 
  dat2$Conservative[block][2] <- as.character(data[loc, which(colnames(data)=="conservative")])   
  dat2$Conservative[block][3] <- as.character(data[loc, which(colnames(data)=="conservative")]) 
  dat2$Conservative[block][4] <- as.character(data[loc, which(colnames(data)=="conservative")]) 
  dat2$Conservative[block][5] <- as.character(data[loc, which(colnames(data)=="conservative")]) 
  dat2$Conservative[block][6] <- as.character(data[loc, which(colnames(data)=="conservative")]) 
  dat2$Conservative[block][7] <- as.character(data[loc, which(colnames(data)=="conservative")]) 
  dat2$Conservative[block][8] <- as.character(data[loc, which(colnames(data)=="conservative")]) 
  
  
  ##liberal
  dat2$Liberal[block][1] <- as.character(data[loc, which(colnames(data)=="liberal")]) 
  dat2$Liberal[block][2] <- as.character(data[loc, which(colnames(data)=="liberal")])   
  dat2$Liberal[block][3] <- as.character(data[loc, which(colnames(data)=="liberal")]) 
  dat2$Liberal[block][4] <- as.character(data[loc, which(colnames(data)=="liberal")]) 
  dat2$Liberal[block][5] <- as.character(data[loc, which(colnames(data)=="liberal")]) 
  dat2$Liberal[block][6] <- as.character(data[loc, which(colnames(data)=="liberal")]) 
  dat2$Liberal[block][7] <- as.character(data[loc, which(colnames(data)=="liberal")]) 
  dat2$Liberal[block][8] <- as.character(data[loc, which(colnames(data)=="liberal")]) 
  
  
  ##Also merging in weights which will be used at the bottom of the script 
  dat2$webal2[block][1] <- as.character(data[loc, which(colnames(data)=="webal2")]) 
  dat2$webal2[block][2] <- as.character(data[loc, which(colnames(data)=="webal2")])   
  dat2$webal2[block][3] <- as.character(data[loc, which(colnames(data)=="webal2")]) 
  dat2$webal2[block][4] <- as.character(data[loc, which(colnames(data)=="webal2")]) 
  dat2$webal2[block][5] <- as.character(data[loc, which(colnames(data)=="webal2")]) 
  dat2$webal2[block][6] <- as.character(data[loc, which(colnames(data)=="webal2")]) 
  dat2$webal2[block][7] <- as.character(data[loc, which(colnames(data)=="webal2")]) 
  dat2$webal2[block][8] <- as.character(data[loc, which(colnames(data)=="webal2")]) 
  
  
  
  ##High IslamoPrej
  dat2$HighIslamoPrej[block][1] <- as.character(data[loc, which(colnames(data)=="islamoprejudicehigh")]) 
  dat2$HighIslamoPrej[block][2] <- as.character(data[loc, which(colnames(data)=="islamoprejudicehigh")])   
  dat2$HighIslamoPrej[block][3] <- as.character(data[loc, which(colnames(data)=="islamoprejudicehigh")]) 
  dat2$HighIslamoPrej[block][4] <- as.character(data[loc, which(colnames(data)=="islamoprejudicehigh")]) 
  dat2$HighIslamoPrej[block][5] <- as.character(data[loc, which(colnames(data)=="islamoprejudicehigh")]) 
  dat2$HighIslamoPrej[block][6] <- as.character(data[loc, which(colnames(data)=="islamoprejudicehigh")]) 
  dat2$HighIslamoPrej[block][7] <- as.character(data[loc, which(colnames(data)=="islamoprejudicehigh")]) 
  dat2$HighIslamoPrej[block][8] <- as.character(data[loc, which(colnames(data)=="islamoprejudicehigh")]) 
  
  
  ##Low IslamoPrej
  dat2$LowIslamoPrej[block][1] <- as.character(data[loc, which(colnames(data)=="islamoprejudicelow")]) 
  dat2$LowIslamoPrej[block][2] <- as.character(data[loc, which(colnames(data)=="islamoprejudicelow")])   
  dat2$LowIslamoPrej[block][3] <- as.character(data[loc, which(colnames(data)=="islamoprejudicelow")]) 
  dat2$LowIslamoPrej[block][4] <- as.character(data[loc, which(colnames(data)=="islamoprejudicelow")]) 
  dat2$LowIslamoPrej[block][5] <- as.character(data[loc, which(colnames(data)=="islamoprejudicelow")]) 
  dat2$LowIslamoPrej[block][6] <- as.character(data[loc, which(colnames(data)=="islamoprejudicelow")]) 
  dat2$LowIslamoPrej[block][7] <- as.character(data[loc, which(colnames(data)=="islamoprejudicelow")]) 
  dat2$LowIslamoPrej[block][8] <- as.character(data[loc, which(colnames(data)=="islamoprejudicelow")]) 
  
  
  ##High IslamoSec
  dat2$HighIslamoSec[block][1] <- as.character(data[loc, which(colnames(data)=="islamosecularhigh")]) 
  dat2$HighIslamoSec[block][2] <- as.character(data[loc, which(colnames(data)=="islamosecularhigh")])   
  dat2$HighIslamoSec[block][3] <- as.character(data[loc, which(colnames(data)=="islamosecularhigh")]) 
  dat2$HighIslamoSec[block][4] <- as.character(data[loc, which(colnames(data)=="islamosecularhigh")]) 
  dat2$HighIslamoSec[block][5] <- as.character(data[loc, which(colnames(data)=="islamosecularhigh")]) 
  dat2$HighIslamoSec[block][6] <- as.character(data[loc, which(colnames(data)=="islamosecularhigh")]) 
  dat2$HighIslamoSec[block][7] <- as.character(data[loc, which(colnames(data)=="islamosecularhigh")]) 
  dat2$HighIslamoSec[block][8] <- as.character(data[loc, which(colnames(data)=="islamosecularhigh")]) 
  
  
  ##Low IslamoSec
  dat2$LowIslamoSec[block][1] <- as.character(data[loc, which(colnames(data)=="islamosecularlow")]) 
  dat2$LowIslamoSec[block][2] <- as.character(data[loc, which(colnames(data)=="islamosecularlow")])   
  dat2$LowIslamoSec[block][3] <- as.character(data[loc, which(colnames(data)=="islamosecularlow")]) 
  dat2$LowIslamoSec[block][4] <- as.character(data[loc, which(colnames(data)=="islamosecularlow")]) 
  dat2$LowIslamoSec[block][5] <- as.character(data[loc, which(colnames(data)=="islamosecularlow")]) 
  dat2$LowIslamoSec[block][6] <- as.character(data[loc, which(colnames(data)=="islamosecularlow")]) 
  dat2$LowIslamoSec[block][7] <- as.character(data[loc, which(colnames(data)=="islamosecularlow")]) 
  dat2$LowIslamoSec[block][8] <- as.character(data[loc, which(colnames(data)=="islamosecularlow")]) 
  
  
  ##male
  dat2$Male[block][1] <- as.character(data[loc, which(colnames(data)=="male")]) 
  dat2$Male[block][2] <- as.character(data[loc, which(colnames(data)=="male")])   
  dat2$Male[block][3] <- as.character(data[loc, which(colnames(data)=="male")]) 
  dat2$Male[block][4] <- as.character(data[loc, which(colnames(data)=="male")]) 
  dat2$Male[block][5] <- as.character(data[loc, which(colnames(data)=="male")]) 
  dat2$Male[block][6] <- as.character(data[loc, which(colnames(data)=="male")]) 
  dat2$Male[block][7] <- as.character(data[loc, which(colnames(data)=="male")]) 
  dat2$Male[block][8] <- as.character(data[loc, which(colnames(data)=="male")]) 
  
  
  
  ##ideorate
  dat2$Ideorate[block][1] <- as.character(data[loc, which(colnames(data)=="ideorate")]) 
  dat2$Ideorate[block][2] <- as.character(data[loc, which(colnames(data)=="ideorate")])   
  dat2$Ideorate[block][3] <- as.character(data[loc, which(colnames(data)=="ideorate")]) 
  dat2$Ideorate[block][4] <- as.character(data[loc, which(colnames(data)=="ideorate")]) 
  dat2$Ideorate[block][5] <- as.character(data[loc, which(colnames(data)=="ideorate")]) 
  dat2$Ideorate[block][6] <- as.character(data[loc, which(colnames(data)=="ideorate")]) 
  dat2$Ideorate[block][7] <- as.character(data[loc, which(colnames(data)=="ideorate")]) 
  dat2$Ideorate[block][8] <- as.character(data[loc, which(colnames(data)=="ideorate")]) 
  
  
  ##birthyr
  dat2$Birthyr[block][1] <- as.character(data[loc, which(colnames(data)=="birthyr")]) 
  dat2$Birthyr[block][2] <- as.character(data[loc, which(colnames(data)=="birthyr")])   
  dat2$Birthyr[block][3] <- as.character(data[loc, which(colnames(data)=="birthyr")]) 
  dat2$Birthyr[block][4] <- as.character(data[loc, which(colnames(data)=="birthyr")]) 
  dat2$Birthyr[block][5] <- as.character(data[loc, which(colnames(data)=="birthyr")]) 
  dat2$Birthyr[block][6] <- as.character(data[loc, which(colnames(data)=="birthyr")]) 
  dat2$Birthyr[block][7] <- as.character(data[loc, which(colnames(data)=="birthyr")]) 
  dat2$Birthyr[block][8] <- as.character(data[loc, which(colnames(data)=="birthyr")]) 
  
  
  ##education
  dat2$Education[block][1] <- as.character(data[loc, which(colnames(data)=="education")]) 
  dat2$Education[block][2] <- as.character(data[loc, which(colnames(data)=="education")])   
  dat2$Education[block][3] <- as.character(data[loc, which(colnames(data)=="education")]) 
  dat2$Education[block][4] <- as.character(data[loc, which(colnames(data)=="education")]) 
  dat2$Education[block][5] <- as.character(data[loc, which(colnames(data)=="education")]) 
  dat2$Education[block][6] <- as.character(data[loc, which(colnames(data)=="education")]) 
  dat2$Education[block][7] <- as.character(data[loc, which(colnames(data)=="education")]) 
  dat2$Education[block][8] <- as.character(data[loc, which(colnames(data)=="education")]) 
  
  
  
  
}


#Convert case variable to numeric for clustered SE to work
dat2$ID_numeric <- as.numeric(dat2$ID)

##Saving the cleaned dataset "dat2" as a csv
save(file="Terrorism.RDATA", x=dat2)


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#loading data
load("/Users/danna/Desktop/applied regression/project folder/Terrorism.RDATA")

#============================================================================================================================================================================================#
##Converting survey weights to numeric 
dat2$webal2 <- as.numeric(dat2$webal2)

#============================================================================================================================================================================================#
##Estimating the Model for Main Analyses

#Do parallel processing - speeds things up
cl <- makeCluster(8,"SOCK")

##Function to cluster bootstrap standard errors -- will be used for all analyses
clusterBootS2 <- function(dat2){
  i <- sample(unique(dat2$ID_numeric),length(unique(dat2$ID_numeric)),replace=TRUE)
  row.nums <- NULL
  for (j in 1:length(i)){
    row.nums <- c(row.nums, which(dat2$ID_numeric==i[j]))
  }
  return(dat2[row.nums,])
}

##Function to run model and then cluster standard errors
bootConjoint <- function(...){
  temp <- clusterBootS2(dat2)
  mod.temp <- lm(TerrorBinary ~  Tactic + Casualties + Actor + Ideology + Motivation + Target + Location, weights=webal2, data=temp)
  return(coef(mod.temp))
}

clusterExport(cl, list("dat2", "bootConjoint", "clusterBootS2"))

system.time(boot.full2 <- parSapply(cl, rep(1,1500), bootConjoint)) 

#============================================================================================================================================================================================#
#Generating a Matrix where each row is a treatment category, the first column is the coefficient estimate, and the second and fourth columns give the 95% confidence intervals, while the third and fifth give the 90% 
plot.mat2 <- cbind(apply(boot.full2, 1, mean), apply(boot.full2, 1, quantile, c(0.025)), apply(boot.full2, 1, quantile, c(0.05)), apply(boot.full2, 1, quantile, c(0.95)), apply(boot.full2,1,quantile, c(0.975)))[-1,]


# the library for the mutate function 
library(tidyverse)

#relationships


# how I loaded in the variables 
load("/Users/danna/Desktop/applied regression/project folder/Final_Survey_Data.RData")
load("/Users/danna/Desktop/applied regression/project folder/SVM+Weighted.RData")
load("/Users/danna/Desktop/applied regression/project folder/Terrorism.RData")

#check for missingness

#had no missingness-- final survey data
missmap(dat)
dev.copy(pdf,"../Users/danna/Desktop/applied regression/project folder/missmap.pdf")
dev.off()

#terrorism data -- did have missingness
missmap(dat2, color ="red")
dev.copy(pdf,"../applied regression/project folder/terrormap.pdf")
dev.off()

#check to see which variables in particular
missingdata <- dat2[0,]
for(i in 1:ncol(missingdata)) {
  missingdata[1,i] <- length(which(is.na(dat2[,i])))
}

# variable mean imputation
dat2$HIP_vmi <- dat2$HighIslamoPrej
dat2$HIP_vmi[which(is.na(dat2$HighIslamoPrej))] <- mean(dat2$HighIslamoPrej, na.rm=TRUE)

dat2$LIP_vmi <- dat2$LowIslamoPrej
dat2$LIP_vmi[which(is.na(dat2$LowIslamoPrej))] <- mean(dat2$LowIslamoPrej, na.rm=TRUE)

dat2$HIS_vmi <- dat2$HighIslamoSec
dat2$HIS_vmi[which(is.na(dat2$HighIslamoSec))] <- mean(dat2$HighIslamoSec, na.rm=TRUE)

dat2$LIS_vmi <- dat2$LowIslamoSec
dat2$LIS_vmi[which(is.na(dat2$LowIslamoSec))] <- mean(dat2$LowIslamoSec, na.rm=TRUE)

#recode tb
dat2<-mutate(dat2,TerrorBinarynew= ifelse(TerrorBinary == "2", "0", "1"))
dat2$TerrorBinarynew = as.numeric(dat2$TerrorBinarynew)

#class check
class(dat2$TerrorBinarynew)


####### conditional hypothesis testing
#dummy variable for muslim
dat2 <- mutate(dat2, Ideologynew = ifelse(Ideology == "a Muslim", "1", "0")) 

#numeric
dat2$Ideologynew = as.numeric(dat2$Ideologynew)

# I just checked the data type 
class(dat2$Ideologynew)

#dummy for actor
dat2 <- mutate(dat2, Actordummy = ifelse(Actor == "individual with a history of mental illness", "1", "0")) 

dat2$Actordummy = as.numeric(dat2$Actordummy)

# check the class 
class(dat2$Actordummy)

#target dummy
dat2 <- mutate(dat2, Targetdummy = ifelse(Target == "Muslim community center", "1", "0")) 

#logit
muslimfit<-glm(TerrorBinarynew~Ideologynew+Actordummy+Targetdummy,family=binomial,data=dat2)
stargazer(muslimfit)


# Anova table to verify the calc
ssefitmuslim<- sum((muslimfit$residuals)^2)
anova(muslimfit)
ssefitmuslim

#conditional hypothesis
ideologyfit<-glm(TerrorBinarynew~Ideologynew+Actordummy+Ideologynew*Actordummy,family=binomial, data=dat2)
stargazer(ideologyfit)
summary(ideologyfit)

idsse<- sum((fit3$residuals)^2)


# Anova table to verify the calc
ssefit2<- sum((ideologyfit$residuals)^2)
ssefit2
anova(ideologyfit)

#marginal effect plot
#marginal effect plot for conditional hypothesis 1

interplot(m=ideologyfit, var1="Actordummy", var2="Ideologynew")+
  aes(color = "pink") + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") + xlab("Ideology (Non-Muslim =0, Muslim. =1)") + 
  ylab("Marginal Effect of Actor(dummy) on Ideology (Ideologynew)") + labs(title="Marginal Effect of Mental Illness Likelihood of on Perceived \nTerrorism",
                                                                               subtitle="TerrorBinary~Ideologynew+Actordummy+IdeologynewActordummy") +
  theme(axis.text=element_text(size=12))

#robustness tests

# # of influential variables

#testing robustness now 
d1 <- cooks.distance(ideologyfit)
dat2 <- cbind(dat2, d1)
dat2 <- dat2[order(dat2$d1, decreasing=TRUE),]

## Estimate models without "blue", or without top 3 largest Cooks D influencers
sub3 <- dat2[-(1:3),]
fit3<-glm(TerrorBinarynew~Ideologynew+Actordummy+Ideologynew*Actordummy,family=binomial,data=sub3)
summary(fit3)
ssecook<- sum((fit3$residuals)^2)

## Estimate models without "blue", or without top 10 largest Cooks D influencers
sub10 <- dat2[-(1:10169),]
fit10<-glm(TerrorBinarynew~Ideologynew+Actordummy+Ideologynew*Actordummy, family=binomial,data=sub3)
summary(fit10)

plot(ideologyfit)
dev.copy(pdf,"idfit")
dev.off()



#--TARGET
#another interactive hypothesis-- the likelihood that act = terror will decrease if 
                                      #non-muslim actor & attack is on muslim community

#dummy for community
dat2 <- mutate(dat2, Targetdummy = ifelse(Target == "Muslim community center", "1", "0")) 
dat2 <- mutate(dat2, nonmuslim = ifelse(Ideology == "a Muslim", "1", "0")) 


#dummy variable for target, read as numbers

dat2$Targetdummy = as.numeric(dat2$Targetdummy)

# check the class 
class(dat2$Targetdummy)

#numeric
dat2$nonmuslim = as.numeric(dat2$nonmuslim)

#check the class
class(dat2$nonmuslim)


#dummy variable for target, read as numbers
#dat2$Motivationdummy = as.numeric(dat2$Motivationdummy)

# check the class 
#class(dat2$Motivationdummy)

#ols reg resting interac. hypothesis.
communityfit<-glm(TerrorBinarynew~nonmuslim+Targetdummy+nonmuslim*Targetdummy, family=binomial,data=dat2)
summary(communityfit)
stargazer(communityfit)

comRSE<- sum((communityfit$residuals)^2)


#marginal effects of community fit
interplot(m=communityfit, var1="nonmuslim", var2="Targetdummy")+
  aes(color = "orange") + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") + xlab("Targetdummy (Non-Muslim =0, Muslim. =1)") + 
  ylab("Marginal Effect of Actor (dummy) on Target (dummy)") + labs(title="Marginal Effect of Target on Perceived Terrorism",
                                                                                  subtitle="TerrorBinary~nonmuslim+Targetdummy+nonmuslim*Targetdummy") +
  theme(axis.text=element_text(size=12))

#robustness of model

#third inter hypo. mediareports*community

# # of influential points
cooks.distance(communityfit)


#testing the robustness now

#testing robustness now 
communitydist <- cooks.distance(communityfit)
dat2 <- cbind(dat2, communitydist)
dat2 <- dat2[order(dat2$communitydist, decreasing=TRUE),]

## Estimate models without "blue", or without top 3 largest Cooks D influencers
sub4 <- dat2[-(1:3),]
fit4<-glm(TerrorBinarynew~Targetdummy+nonmuslim+Targetdummy*nonmuslim, data=sub4)
summary(fit4)
ssecook<- sum((fi4$residuals)^2)

## Estimate models without "blue", or without top 10 largest Cooks D influencers
check <- dat2[-(1:10169),]
communitymodel3<-glm(TerrorBinarynew~Targetdummy+nonmuslim+Targetdummy*nonmuslim, data=sub4)
summary(communitymodel3)

#if it comes up with the d1 duplicate, uncomment and do this
#dat2[37]<-NULL
#----
#dummy motivation
dat2<-mutate(dat2, Motivationdummy = ifelse(Motivation == "News reports suggest that there was no clear motivation for the incident.", "1","0"))

motivefit<-glm(TerrorBinarynew~Motivationdummy+Ideologynew+Motivationdummy*Ideologynew, family=binomial, data=sub4)
summary(motivefit)

