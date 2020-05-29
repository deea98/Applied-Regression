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
dat2$TerrorBinary <- as.numeric(dat2$TerrorBinary, "c('2')=0; c('1')=1") 
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
data$shar_ethic_recoded <- as.numeric(data$shar_ethic_numeric, "c('1')=5; c('2')=4; c('3')=3; c('4')=2; c('5')=1")

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
load("Terrorism.RDATA")

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

#Now add baseline categories: these will be zeroes on the plot which will allow us to make easy comparisons for the reader
plot.mat2b <- rbind(rep(0,5), plot.mat2[1:3,], rep(0,5), plot.mat2[4:6,], rep(0,5), plot.mat2[7:11,], rep(0,5), plot.mat2[12:15,], rep(0,5), 
                    plot.mat2[16:19,], rep(0,5), plot.mat2[c(20:27),], rep(0,5), plot.mat2[28:31,])



#============================================================================================================================================================================================#
#==============================================================FIGURE 2A=====================================================================================================================#
#============================================================================================================================================================================================#
##Figure 2A: How Tactics Affect Perceptions that an Incident is Terrorism


#making the label for the plot
textLabTactic <- c("Protest", "Hostage Taking", "Shooting", "Bombing")

pdf("fig2a.pdf", height=5, width=5.5)
par(oma=c(0,1,0,0), mar=c(4,0,1,0)) #Old margins: mar=c(5.1,1.1,4.1,0)
plot(plot.mat2b[1:length(textLabTactic),1], length(textLabTactic):1, type="n", axes=FALSE, xlab="Average marginal component effect (AMCE)", ylab="",xlim=c(-0.2,0.8))
for (i in 1:length(textLabTactic)){
  points(plot.mat2b[i,1], length(textLabTactic)-(i-1), pch=16)
  lines(plot.mat2b[i,c(2,5)], rep(length(textLabTactic)-(i-1),2))
  lines(plot.mat2b[i,c(3,4)], rep(length(textLabTactic)-(i-1),2), lwd=2)    
  text(-0.13, length(textLabTactic)-(i-1), textLabTactic[i], cex=0.9)      
}
abline(v=0)
axis(1)
dev.off()


#============================================================================================================================================================================================#
#==============================================================FIGURE 2B=====================================================================================================================#
#============================================================================================================================================================================================#
##Figure 2B: How Casualties Affect Perceptions that an Incident is Terrorism
textLabFigure1B <- c("No Casualties", "One Casualty", "Two Casualties", "Ten Casualties")

##Casualties Plot
pdf("fig2b.pdf", height=5, width=5.5)
par(oma=c(0,1,0,0), mar=c(4,0,1,0)) 
plot(plot.mat2b[1:length(textLabFigure1B),1], length(textLabFigure1B):1, type="n", axes=FALSE, xlab="Average marginal component effect (AMCE)", ylab="",xlim=c(-0.2,0.8))
points(plot.mat2b[5,1], 4, pch=16) 
points(plot.mat2b[6,1], 3, pch=16) 
points(plot.mat2b[7,1], 2, pch=16) 
points(plot.mat2b[8,1], 1, pch=16) 
lines(plot.mat2b[5,c(2,5)], c(4,4))
lines(plot.mat2b[6,c(3,4)], c(3,3), lwd=2) 
lines(plot.mat2b[7,c(3,4)], c(2,2), lwd=2) 
lines(plot.mat2b[8,c(3,4)], c(1,1), lwd=2) 
text(-.13, 4, "No Casualties", cex=0.9)
text(-.13, 3, "One Casualty", cex=0.9)
text(-.13, 2, "Two Casualties", cex=0.9)
text(-.13, 1, "Ten Casualties", cex=0.9)
abline(v=0)
axis(1)
dev.copy(pdf,"../applied regression/project folder/fig2b.pdf")
dev.off()

#============================================================================================================================================================================================#
#===============================================================Figure 3A=====================================================================================================================#
#============================================================================================================================================================================================#
##Figure 3A: How Location Affects Perceptions that an Incident is Terrorism

textLocation <- c("United States", "Foreign Democracy", "Foreign Democracy HR Violations", "Foreign Dictatorship", "Foreign Dictatorship HR Violations")


pdf("fig3a.pdf", height=5, width=5.5)
par(oma=c(0,1,0,0), mar=c(4,0,1,0))
plot(plot.mat2b[34:38,1], length(textLocation):1, type="n", axes=FALSE, xlab="Average marginal component effect (AMCE)", ylab="",xlim=c(-0.5,0.5), ylim=c(0.75,5.25))
points(plot.mat2b[34,1], 5, pch=16) 
points(plot.mat2b[35,1], 4, pch=16) 
points(plot.mat2b[36,1], 3, pch=16)
points(plot.mat2b[37,1], 2, pch=16)
points(plot.mat2b[38,1], 1, pch=16)
lines(plot.mat2b[34,c(2,5)], c(5,5)) 
lines(plot.mat2b[35,c(3,4)], c(4,4), lwd=2) 
lines(plot.mat2b[36,c(3,4)], c(3,3), lwd=2)
lines(plot.mat2b[37,c(3,4)], c(2,2), lwd=2)
lines(plot.mat2b[38,c(3,4)], c(1,1), lwd=2)
text(-.25, 5, "United States", cex=0.9)
text(-.25, 4, "Foreign Democracy", cex=0.9)
text(-.25, 3, "Foreign Democracy\n HR Violations", cex=0.9)
text(-.25, 2, "Foreign Dictatorship", cex=0.9)
text(-.25, 1, "Foreign Dictatorship\n HR Violations", cex=0.9)
abline(v=0)
axis(1)
dev.off()




#============================================================================================================================================================================================#
#===============================================================Figure 3B=====================================================================================================================#
#============================================================================================================================================================================================#
##Figure 3B: How Target Affects Perceptions that an Incident is Terrorism


textTarget <- c("Military Facility", "Police Station", "School", "Christian Center", "Church", "Muslim Center", "Mosque",
                "Jewish Center", "Synagogue")


pdf("fig3b.pdf", height=5, width=5.5)
par(oma=c(0,1,0,0), mar=c(4,0,1,0))
plot(plot.mat2b[25:33,1], length(textTarget):1, type="n", axes=FALSE, xlab="Average marginal component effect (AMCE)", ylab="",xlim=c(-0.5,0.5))
points(plot.mat2b[25,1], 9, pch=16) 
points(plot.mat2b[26,1], 8, pch=16) 
points(plot.mat2b[27,1], 7, pch=16) 
points(plot.mat2b[28,1], 6, pch=16) 
points(plot.mat2b[29,1], 5, pch=16) 
points(plot.mat2b[30,1], 4, pch=16) 
points(plot.mat2b[31,1], 3, pch=16)
points(plot.mat2b[32,1], 2, pch=16)
points(plot.mat2b[33,1], 1, pch=16)
lines(plot.mat2b[25,c(2,5)], c(9,9)) 
lines(plot.mat2b[26,c(3,4)], c(8,8), lwd=2)
lines(plot.mat2b[27,c(3,4)], c(7,7), lwd=2)
lines(plot.mat2b[28,c(3,4)], c(6,6), lwd=2)
lines(plot.mat2b[29,c(3,4)], c(5,5), lwd=2)
lines(plot.mat2b[30,c(3,4)], c(4,4), lwd=2) 
lines(plot.mat2b[31,c(3,4)], c(3,3), lwd=2)
lines(plot.mat2b[32,c(3,4)], c(2,2), lwd=2)
lines(plot.mat2b[33,c(3,4)], c(1,1), lwd=2)
text(-.25, 9, "Military Facility", cex=0.9)
text(-.25, 8, "Police Station", cex=0.9)
text(-.25, 7, "School", cex=0.9)
text(-.25, 6, "Christian Center", cex=0.9)
text(-.25, 5, "Church", cex=0.9)
text(-.25, 4, "Muslim Center", cex=0.9)
text(-.25, 3, "Mosque", cex=0.9)
text(-.25, 2, "Jewish Center", cex=0.9)
text(-.25, 1, "Synagogue", cex=0.9)
abline(v=0)
axis(1)
dev.off()



#============================================================================================================================================================================================#
#===============================================================Figure 4=====================================================================================================================#
#===========================================================================================================================================================================================#
##Figure 4: The Political Purposiveness of the Actors

textActor <- c("Individual", "Individual Mental", "Group", "Organization", "Org US Ties", "Org Foreign Ties")

pdf("fig4.pdf", height=5, width=5.5)
par(oma=c(0,1,0,0), mar=c(4,0,1,0))
plot(plot.mat2b[9:14,1], length(textActor):1, type="n", axes=FALSE, xlab="Average marginal component effect (AMCE)", ylab="",xlim=c(-0.5,0.5))
points(plot.mat2b[9,1], 6, pch=16) 
points(plot.mat2b[10,1], 5, pch=16)
points(plot.mat2b[11,1], 4, pch=16)
points(plot.mat2b[12,1], 3, pch=16)
points(plot.mat2b[13,1], 2, pch=16)
points(plot.mat2b[14,1], 1, pch=16)
lines(plot.mat2b[9,c(2,5)], c(5,5)) 
lines(plot.mat2b[10,c(3,4)], c(5,5), lwd=2) 
lines(plot.mat2b[11,c(3,4)], c(4,4), lwd=2)
lines(plot.mat2b[12,c(3,4)], c(3,3), lwd=2)
lines(plot.mat2b[13,c(3,4)], c(2,2), lwd=2)
lines(plot.mat2b[14,c(3,4)], c(1,1), lwd=2)
text(-.3, 6, "Individual", cex=0.9)
text(-.3, 5, "Individual Mental", cex=0.9)
text(-.3, 4, "Group", cex=0.9)
text(-.3, 3, "Organization", cex=0.9)
text(-.3, 2, "Org US Ties", cex=0.9)
text(-.3, 1, "Org Foreign Ties", cex=0.9)
abline(v=0)
axis(1)
dev.off()



#============================================================================================================================================================================================#
#===============================================================Figure 5=====================================================================================================================#
#============================================================================================================================================================================================#
##Figure 5: The Social Categorization of the Actor

textLabMotivation <- c("No Ideology", "Christian", "Muslim", "Left-Wing", "Right-Wing")

pdf("fig5.pdf", height=5, width=5.5)
par(oma=c(0,1,0,0), mar=c(4,0,1,0))
plot(plot.mat2b[1:length(textLabMotivation),1], length(textLabMotivation):1, type="n", axes=FALSE, xlab="Average marginal component effect (AMCE)", ylab="",xlim=c(-0.5,0.5))
points(plot.mat2b[15,1], 5, pch=16) 
points(plot.mat2b[16,1], 4, pch=16) 
points(plot.mat2b[17,1], 3, pch=16) 
points(plot.mat2b[18,1], 2, pch=16) 
points(plot.mat2b[19,1], 1, pch=16) 
lines(plot.mat2b[15,c(2,5)], c(5,5))
lines(plot.mat2b[16,c(2,5)], c(4,4), lwd=2)
lines(plot.mat2b[17,c(3,4)], c(3,3), lwd=2) 
lines(plot.mat2b[18,c(3,4)], c(2,2), lwd=2) 
lines(plot.mat2b[19,c(3,4)], c(1,1), lwd=2) 
text(-.25, 5, "No Ideology", cex=0.9)
text(-.25, 4, "Christian", cex=0.9)
text(-.25, 3, "Muslim", cex=0.9)
text(-.25, 2, "Left-Wing", cex=0.9)
text(-.25, 1, "Right-Wing", cex=0.9)
abline(v=0)
axis(1)
dev.off()



#============================================================================================================================================================================================#
#===============================================================Figure 6=====================================================================================================================#
#============================================================================================================================================================================================#
##Figure 6: The Motivation Attributed to the Actor

textLabMotivation <- c("Personal Dispute", "Unclear Motivation", "Hatred", "Policy Change", "Government Overthrow")

pdf("fig6.pdf", height=5, width=5.5)
par(oma=c(0,1,0,0), mar=c(4,0,1,0))
plot(plot.mat2b[1:length(textLabMotivation),1], length(textLabMotivation):1, type="n", axes=FALSE, xlab="Average marginal component effect (AMCE)", ylab="",xlim=c(-0.5,0.5))
points(plot.mat2b[20,1], 5, pch=16) 
points(plot.mat2b[21,1], 4, pch=16) 
points(plot.mat2b[22,1], 3, pch=16) 
points(plot.mat2b[23,1], 2, pch=16) 
points(plot.mat2b[24,1], 1, pch=16) 
lines(plot.mat2b[20,c(2,5)], c(5,5))
lines(plot.mat2b[21,c(2,5)], c(4,4), lwd=2)
lines(plot.mat2b[22,c(3,4)], c(3,3), lwd=2) 
lines(plot.mat2b[23,c(3,4)], c(2,2), lwd=2) 
lines(plot.mat2b[24,c(3,4)], c(1,1), lwd=2) 
text(-.25, 5, "Personal Dispute", cex=0.9)
text(-.25, 4, "Unclear Motivation", cex=0.9)
text(-.25, 3, "Hatred", cex=0.9)
text(-.25, 2, "Policy Change", cex=0.9)
text(-.25, 1, "Government Overthrow", cex=0.9)
abline(v=0)
axis(1)
dev.off()


#============================================================================================================================================================================================#
#===============================================================Table 2=====================================================================================================================#
#============================================================================================================================================================================================#
#Table 2: Predicted Probabilities that 39 incidents are classified as terrorism

#Load weighted conjoint model
conjoint.mod <- get(load("/Users/danna/Desktop/applied regression/project folder/SVM+Weighted.RData"))

#Get locations of non-zero coefficients:
#1 constant + tactics treat (4 levels) + casualties treat (4 levels) + actor treat (6 levels) + ideology treat (5 levels) + motivation treat (5 levels) + target treat (9 levels) + location treat (5 levels) = 39 lower order terms

i <- which(conjoint.mod$coefs != 0) #1684 of the 5658 coefficients are significant
j <- sort(union(i,1:39)) #Add all main effects, even those with 0 effect lower-order terms
probs.data <- data.frame(cbind(conjoint.mod$coefs[j], conjoint.mod$names.out[j]))
colnames(probs.data) <- c("Coefficient", "Description")

events <- matrix(NA, nrow=39, ncol=nrow(probs.data)) #Label each event
rownames(events) <- c("KKK Selma Bombing", "Shooting of Police in Brooklyn (12/20/14)", "Hamas attack on IDF in Khan Yunis (12/24/14)", "Charleston church Shooting (06/17/15)", "Seattle Jewish Federation Shooting (07/28/06)", "Chattanooga shootings (07/16/15)", "Overland Park Jewish Community Center Shooting (04/13/14)", "Pentagon Metro Shooting (03/04/10)", "Zif School Bombing (09/17/02) ", "Porte de Vincennes hostage situation (01/09/15)", "Pakistan Army General HQ hostage situation (10/10/09)", "Zvornik Police Station shooting (04/27/15)", "Fort Hood Shootings (11/05/09)", "ETA Sanguesa car bombing (05/30/03)", "Aksu bombing (08/19/10)", "Marysville Pilchuck High School shooting (10/24/14)", "Knoxville Unitarian Universalist Church shooting (07/27/08)", "Nag Hammadi massacre (01/07/10)", "Lombard Islamic School bombing (08/12/12)", "Shebaa Farms incident (01/28/15)", "Contra attack in Quilali (11/11/1987)", "Bombing of Shiraa village mosque (12/30/2014)", "Rocori High School shooting (09/24/03)", "Kehilat Bnei Torah synagogue attack (11/18/14)", "Shooting of Police in Oakland (03/21/09)", "East Selma Church Shooting (09/20/15)", "Newport Church hostage situation (07/30/06)", "Camp Integrity Suicide bombing (08/07/15)", "St. Columbanus Church Shooting (11/26/12)", "Rosemary Anderson High School shooting (12/13/14)", "University of Alabama Huntsville (12/02/10)", "UCLA Black Lives Matter Protest (10/08/15)", "University of California Tuition Hike Protests (03/18/15)", "Shooting of George Tiller (05/31/09)", "Copenhagen Synagogue Shooting (02/14/15)", "Islamic Community Center of Phoenix Demonstrations (10/10/15)", "Dallas Police HQ Shooting (06/13/15)", "Camp Shelby Shootings (08/05/15)", "Poe Elementary School Bombing (09/15/59)")
for (i in 1:nrow(events)){
  events[i,] <- c(rep(0, 39), rep(NA,(ncol(events)-39)))
}

#Now fill the main effect results for each event (Note: this is drawn from Terror_Classifications_Reordered_10182015.csv; these are correct, even though the column ordering is different) 
events[,1] <- rep(1,39) #Intercept
events[,2] <- c(1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1) #Bombing
events[,3] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) #Hostage-taking
events[,4] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0) #Protest
events[,5] <- c(0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 2, 0, 1, 1, 0) #Shooting
events[,6] <- c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0) #One casualty
events[,7] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0) #No casualties
events[,8] <- c(0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1) #Ten casualties
events[,9] <- c(1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0) #Two casualties
events[,10] <- c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0) #Actor: group
events[,11] <- c(0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 0) #Actor: individual
events[,12] <- c(0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1) #Actor: individual with mental illness
events[,13] <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) #Actor: organization
events[,14] <- c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) #Actor: organization with foreign ties
events[,15] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) #Actor: organization with US ties
events[,16] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) #Identity: Christian
events[,17] <- c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0) #Identity: left-wing
events[,18] <- c(0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) #Identity: Muslim
events[,19] <- c(0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) #Identity: Right wing
events[,20] <- c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1) #Identity: unspecified
events[,21] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1) #No clear motivation
events[,22] <- c(0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0) #Hatred
events[,23] <- c(1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0) #Changing Policy
events[,24] <- c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) #Overthrowing Government
events[,25] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0) #Personal dispute
events[,26] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) #Christian community center
events[,27] <- c(1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0)#Church
events[,28] <- c(0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) #Jewish community center
events[,29] <- c(0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0) #Military facility
events[,30] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0) #Mosque
events[,31] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) #Muslim community center
events[,32] <- c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0) #Police station
events[,33] <- c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1) #School
events[,34] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0) #Synagogue
events[,35] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0) #Foreign democracy
events[,36] <- c(0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) #Foreign democracy HR violations
events[,37] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) #Foreign dictatorship
events[,38] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) #Foreign dictatorship HR violations
events[,39] <- c(1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1) #US


#Now change variable names to avoid duplication
probs.data$Description <- gsub("Actor_individual.with.a.history.of.mental.illness","Actor_with.a.history.of.mental.illness",probs.data$Description)
probs.data$Description <- gsub("Actor_organization.with.ties.to.a.foreign.government","Actor_with.ties.to.a.foreign.government",probs.data$Description)
probs.data$Description <- gsub("Actor_organization.with.ties.to.the.United.States","Actor_with.ties.to.the.United.States",probs.data$Description)
probs.data$Description <- gsub("Location_a.foreign.democracy.with.a.history.of.human.rights.violations","Location_a.foreign.dem.with.a.history.of.human.rights.violations",probs.data$Description)
probs.data$Description <- gsub("Location_a.foreign.dictatorship.with.a.history.of.human.rights.violations","Location_a.foreign.dict.with.a.history.of.human.rights.violations",probs.data$Description)


fillInteractions <- function(incident, ref){
  loc <- which(incident==0) #Find all the lower-order cells that don't occur
  for (i in loc){
    lab <- ref$Description[i] #Get name of variable
    j <- grep(lab,ref$Description)
    if (i %in% j){
      j <- j[-which(j==i)]
    }
    incident[j] <- 0
  }
  incident[is.na(incident)] <- 1 #Set missing values to 1
  return(incident)
}

#Do this in a loop, for each row in a matrix of events 
for (i in 1:nrow(events)){
  events[i,] <- fillInteractions(events[i,], probs.data)
}

#Logit transformation
logit <- function(xb){
  1/(1+exp(-xb))
}

results <- logit(events %*% as.numeric(as.character(probs.data$Coefficient)))

table <- as.data.frame(cbind(rownames(results)[order(results)],round(results[order(results)],digits=2))) 
xtable(table)



#============================================================================================================================================================================================#
#===============================================================APPLICATION III: SAN BERNADINO=====================================================================================================================#
#============================================================================================================================================================================================#
#Predicted Probabilities From "Application II: Demonstrating Framing Effects"

#San Bernardino 
sanbernadino <- sanbernadino2 <- sanbernadino3 <- sanbernadino4 <- c(rep(0, 39), rep(NA,nrow(probs.data)-39)) #0s for main effects, NAs for interactions
sanbernadino[c(1,5,8,14,18,23,33,39)] <- 1 #Intercept, shooting, ten casualties, org w foreign ties, Muslim, changing policy, school, US
sanbernadino2[c(1,5,8,11,18,21,33,39)] <- 1 #Intercept, shooting, ten casualties, individual, muslim,no clear motivation,school,US
sanbernadino3[c(1,5,8,12,18,21,33,39)] <- 1 #Intercept, shooting, ten casualties, individual w mental illness, muslim,no clear motivation,school,US
sanbernadino4[c(1,5,8,12,20,21,33,39)] <- 1 #Intercept, shooting, ten casualties, individual w mental illness, unspecified,no clear motivation,school,US

#Now fill in interactions
sanbernadino <- fillInteractions(sanbernadino,probs.data)
sanbernadino2 <- fillInteractions(sanbernadino2,probs.data)
sanbernadino3 <- fillInteractions(sanbernadino3,probs.data)
sanbernadino4 <- fillInteractions(sanbernadino4,probs.data)


logit(sanbernadino %*% as.numeric(as.character(probs.data$Coefficient))) #81.57%
logit(sanbernadino2 %*% as.numeric(as.character(probs.data$Coefficient))) #50.3%
logit(sanbernadino3 %*% as.numeric(as.character(probs.data$Coefficient))) #45.9%
logit(sanbernadino4 %*% as.numeric(as.character(probs.data$Coefficient))) #30.6%

#-------------------------extending-------------------
#relationships

#dummy variable for muslim
dat2 <- mutate(dat2, Ideologynew = ifelse(Ideology == "a Muslim", "1", "0")) 

#dummy for actor
dat2 <- mutate(dat2, Actordummy = ifelse(Actor == "individual with a history of mental illness", "0", "1")) 

ideologyfit<- lm(TerrorBinary~Ideologynew+Actordummy+(Ideologynew)*(Actordummy), data=dat2)
summary(ideologyfit)







#============================================================================================================================================================================================#
#===============================================================APPLICATION III: ARTICLES MENTIONING TERRORISM AND DEBATE=====================================================================================================================#
#============================================================================================================================================================================================#
###This script reads in batches of 500 articles downloaded from Lexis Nexis about the shootings at Charleston and Forthood respectively
##The text files are saved in two different directories: one for Charleston and the other for Fort Hood. The script reads in all these text files.
#Because of terms of use agreements, the raw text from the Lexis Nexis articles are not included in this replication dataset, and users should download the articles from Lexis Nexis themselves; see the detailed description in Appendix §6.

##Read in the respective files

##Charlestion
#c.files <- list.files()
#article.list.charleston <- list()

#for(i in 1:length(c.files)){
#  article.list.charleston[[i]] <- scan(eval(c.files[i]), what="character", sep="\n")
#}

#article.list.charleston <- unlist(article.list.charleston)

##Fort Hood
#fh.files <- list.files()

#article.list.forthood <- list()
#for(i in 1:length(fh.files)){
#  article.list.forthood[[i]] <- scan(eval(fh.files[i]), what="character", sep="\n")
#}

#article.list.forthood <- unlist(article.list.forthood)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##We then parse the articles for Charleston and Forthood, using the article number indexing for each article to separate the long character string into unique newspaper articles above

#---------------------------------------------
##Charleston Section

##Parsing the text file into unique articles
#articles.charleston <- list()

##index for the start of each unique article
#a.charleston <- which(grepl("[[:digit:]]+ of 2297 DOCUMENTS", article.list.charleston))

##append the length of the article.id so that the last article will go the end (since it is not g oing to be indexed as part of the above code)
#article.id.charleston <- c(a.charleston, length(article.list.charleston))

##for loop grabbing each unique article 
#for(i in 1:(length(article.id.charleston)-1)){ ##indexing length(article.id)-1 so that for loop doesn't look for a number that doesn't exist
#  articles.charleston[[i]] <- article.list.charleston[article.id.charleston[i]:(article.id.charleston[i+1]-1)] 
#}

#---------------------------------------------
##Fort Hood Section

##Parsing the text file into unique articles
#articles.forthood <- list()

##index for the start of each unique article
#a.forthood <- which(grepl("[[:digit:]]+ of 2809 DOCUMENTS", article.list.forthood))

##append the length of the article.id so that the last article will go the end (since it is not g oing to be indexed as part of the above code)
#article.id.forthood <- c(a.forthood, length(article.list.forthood))

##for loop grabbing each unique article 
#for(i in 1:(length(article.id.forthood)-1)){ ##indexing length(article.id)-1 so that for loop doesn't look for a number that doesn't exist
#  articles.forthood[[i]] <- article.list.forthood[article.id.forthood[i]:(article.id.forthood[i+1]-1)] 
#}


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##Subsetting all articles to be between a time window of the incident. This section of the script extracts the dates for each newspaper article.

#months <- c("January", 
#            "February", 
#            "March", 
#            "April", 
#            "May", 
#            "June", 
#            "July", 
#            "August", 
#            "September", 
#            "October",
#            "November",
#            "December")

#pattern <- paste(months, collapse = "|")

#---------------------------------------------
##Charleston Section


#dates.vec.charleston <- list()
##getting all the dates for the articles. The output of this for loop is a list with dates as the first element. Some elements have trailing strings which will be parsed in the next for loop.
#for(i in 1:length(articles.charleston)){
#  d <- NULL
#  d <- which(grepl(pattern, articles.charleston[[i]][1:20]))  ##searching for months within the first 20 lines
#  dates.vec.charleston[[i]] <- articles.charleston[[i]][d] 
#}


#---------------------------------------------
##Forthood Section

#dates.vec.forthood <- list()
##getting all the dates for the articles. The output of this for loop is a last with dates as the first element. Some elements have trailing strings which will be parsed in the next for loop.
#for(i in 1:length(articles.forthood)){
#  d <- NULL
#  d <- which(grepl(pattern, articles.forthood[[i]][1:20]))  ##searching for months within the first 20 lines
#  dates.vec.forthood[[i]] <- articles.forthood[[i]][d] 
#}


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##Will split the strings on the year, and then grab the years using the string match
#years <- 1970:2016 ##possible date ranges in lexis nexis search
#years <- as.character(years) 
#all.years <- paste(years, collapse="|") ##collapsing with or statement so can search through

#---------------------------------------------
##Charleston Section

##for loop splitting the dates
#dates.clean.charleston <- NULL
#for(i in 1:length(dates.vec.charleston)){
#  a <- strsplit(dates.vec.charleston[[i]][1], split=all.years)[[1]][1] ##splitting the string on year
#  z <- str_trim(a) ##trimming extra whitespace
#  b <- str_match(string=dates.vec.charleston[[i]][1], pattern=all.years) ##grabbing the year which was split on (strsplit does not keep what it splits on)
#  dates.clean.charleston[i] <- paste(z, b, sep=" ") ##pasting the month and day with the year
#}


#---------------------------------------------
##Forthood Section

##for loop splitting the dates
#dates.clean.forthood <- NULL
#for(i in 1:length(dates.vec.forthood)){
#  a <- strsplit(dates.vec.forthood[[i]][1], split=all.years)[[1]][1] ##splitting the string on year
#  z <- str_trim(a) ##trimming extra whitespace
#  b <- str_match(string=dates.vec.forthood[[i]][1], pattern=all.years) ##grabbing the year which was split on (strsplit does not keep what it splits on)
#  dates.clean.forthood[i] <- paste(z, b, sep=" ") ##pasting the month and day with the year
#}



#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##converting to a common format
#dates.w.time.charleston <- paste(dates.clean.charleston, "12:39")
#new.date.charleston <- as.Date(dates.w.time.charleston, format = "%B %d, %Y %H:%M")


##converting to a common format that will be easier to parse
#dates.w.time.forthood <- paste(dates.clean.forthood, "12:39")
#new.date.forthood <- as.Date(dates.w.time.forthood, format = "%B %d, %Y %H:%M")


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##constraining dates to be within a particular date wondow (note the strict equality)
#before.attack.charleston <- "2015-06-16" ##need day before attack due to inequality below
#upper.date.charleston <- "2015-07-02" ##specify window and go one day after  
#close.dates.ind.charleston <- before.attack.charleston < new.date.charleston & new.date.charleston < upper.date.charleston


##constraining dates to be within a particular date wondow (note the strict equality)
#before.attack.forthood <- "2009-11-04" ##need day before attack due to inequality below (note in the year-month-day format)
#upper.date.forthood <- "2009-11-19" ##specify window and go one day after  (currently set for a month and a day)
#close.dates.ind.forthood <- before.attack.forthood < new.date.forthood & new.date.forthood < upper.date.forthood


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------
##Charleston Section

##subsetting to only articles within the two week date window
#close.articles.charleston <-articles.charleston[close.dates.ind.charleston]


##subsetting to only dates within the two week date window (will use this in the plots later)
#close.dates.charleston <- new.date.charleston[close.dates.ind.charleston]



#---------------------------------------------
##Forthood Section

##subsetting to only articles within the two week date window
#close.articles.forthood <-articles.forthood[close.dates.ind.forthood]


##subsetting to only dates within the two week date window (will use this in the plots later)
#close.dates.forthood <- new.date.forthood[close.dates.ind.forthood]

##eliminating articles and dates for which the date is unspecified (don't need to do this for Charleston since there aren't any)
#close.articles.forthood.clean <- close.articles.forthood[-which(is.na(close.dates.forthood))]
#close.dates.forthood.clean <- close.dates.forthood[-which(is.na(close.dates.forthood))]



#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#---------------------
##Charleston
#close.dates.charleston.clean <- close.dates.charleston
#close.articles.charleston.clean <- close.articles.charleston 
#c.data <- close.articles.charleston.clean


##cleaning. Will have a standardized format where every word is separated by a single splace
#clean.c.data <- list()

##for loop cleaning the text
#for(i in 1:length(c.data)){
#  clean.c.data[[i]] <- gsub("-", " ", c.data[[i]])
#  clean.c.data[[i]] <- paste(clean.c.data[[i]], collapse=c("\n"))
#  clean.c.data[[i]] <- removePunctuation(clean.c.data[[i]])
#  clean.c.data[[i]] <- gsub("[\r\n]", " ", clean.c.data[[i]])
#  clean.c.data[[i]] <- gsub("\\s+", " ", clean.c.data[[i]])
#  
#}

##for loop creating a list where each element is a word
#c.split.text <- list()
#for(i in 1:length(clean.c.data)){
#  c.split.text[[i]] <- unlist(strsplit(clean.c.data[[i]], " "))
#}

#---------------------------------------------
##Forthood Section
#fh.data <- close.articles.forthood.clean

##cleaning. Goal is to have a standardized format where every word is separated by a single splace
#clean.fh.data <- list()

##for loop cleaning the text
#for(i in 1:length(fh.data)){
#  clean.fh.data[[i]] <- gsub("-", " ", fh.data[[i]])
#  clean.fh.data[[i]] <- paste(clean.fh.data[[i]], collapse=c("\n"))
#  clean.fh.data[[i]] <- removePunctuation(clean.fh.data[[i]])
#  clean.fh.data[[i]] <- gsub("[\r\n]", " ", clean.fh.data[[i]])
#  clean.fh.data[[i]] <- gsub("\\s+", " ", clean.fh.data[[i]])
#}

##for loop creating a list where each element is a word
#fh.split.text <- list()
#for(i in 1:length(clean.fh.data)){
#  fh.split.text[[i]] <- unlist(strsplit(clean.fh.data[[i]], " "))
#}

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Proportion of articles mentioning terrorism

##for loop searching for terrorism within each list element (where each list element is an article split into each unique word)
#c.terror.matches <- list()
#for(i in 1:length(c.split.text)){
#  c.terror.matches[[i]] <- which(unlist(lapply(c.split.text[[i]], function(x) grepl("terrorism|terrorist", x))))
#}


##for loop denoting whether terrorism is mentioned in a given article (this will be used to subset to only articles mentioning terrorism)
#c.terrorism.mentioned <- NULL
#for(i in 1:length(c.terror.matches)){
#  c.terrorism.mentioned[i] <- as.numeric(sum(c.terror.matches[[i]]) > 0)
#}

#---------------------------------------------

##for loop searching for terrorism within each list element (where each list element is an article split into each unique word)
#fh.terror.matches <- list()
#for(i in 1:length(fh.split.text)){
#  fh.terror.matches[[i]] <- which(unlist(lapply(fh.split.text[[i]], function(x) grepl("terrorism|terrorist", x))))
#}


##for loop denoting whether terrorism is mentioned in a given article (this will be used to subset to only articles mentioning terrorism)
#fh.terrorism.mentioned <- NULL
#for(i in 1:length(fh.terror.matches)){
#  fh.terrorism.mentioned[i] <- as.numeric(sum(fh.terror.matches[[i]]) > 0)
#}

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#df.c <- data.frame(c.terrorism.mentioned = c.terrorism.mentioned, close.dates.charleston.clean, stringsAsFactors=FALSE)
#c.all.articles.by.date <- as.data.frame(table(df.c$close.dates.charleston.clean))

#c.all.articles.by.date$ArticlesMentioningTerrorism <- aggregate(df.c$c.terrorism.mentioned, by=list(Category=df.c$close.dates.charleston.clean), FUN=sum)$x
#names(c.all.articles.by.date) <- c("date", "TotalArticles", "ArticlesMentioningTerrorism")
#c.all.articles.by.date$date <- as.Date(c.all.articles.by.date$date)

#c.all.articles.by.date$ProportionMentioningTerrorism <- c.all.articles.by.date$ArticlesMentioningTerrorism/c.all.articles.by.date$TotalArticles


#---------------------------------------------
#df.fh <- data.frame(fh.terrorism.mentioned = fh.terrorism.mentioned, close.dates.forthood.clean, stringsAsFactors=FALSE)
#fh.all.articles.by.date <- as.data.frame(table(df.fh$close.dates.forthood.clean))

#fh.all.articles.by.date$ArticlesMentioningTerrorism <- aggregate(df.fh$fh.terrorism.mentioned, by=list(Category=df.fh$close.dates.forthood.clean), FUN=sum)$x
#names(fh.all.articles.by.date) <- c("date", "TotalArticles", "ArticlesMentioningTerrorism")
#fh.all.articles.by.date$date <- as.Date(fh.all.articles.by.date$date)

#fh.all.articles.by.date$ProportionMentioningTerrorism <- fh.all.articles.by.date$ArticlesMentioningTerrorism/fh.all.articles.by.date$TotalArticles


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#pdf("Figure_7A.pdf")
#pdf("Figure_7A.pdf")
#plot(seq(nrow(c.all.articles.by.date)), c.all.articles.by.date$ProportionMentioningTerrorism, type="l", lwd=3, 
#     ylab="Proportion of Articles Mentioning Terrorism or Terrorist", 
#     xlab="Weeks After Incident", xaxt="n", main="Proportion of Articles", ylim=c(0, 1.15)) 
#lines(seq(nrow(fh.all.articles.by.date)), fh.all.articles.by.date$ProportionMentioningTerrorism, lwd=3, lty=2)
#axis(side=1, at=c(7, 14), labels = c("1 week", "2 weeks"))
#legend("topright", legend=c("Charleston", "Fort Hood"), lty=c(1,2), lwd=3)
#dev.off()


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Proportion mentioning debate over time
##for loop searching for whether the word debate is mentioned within each list element (where each list element is an article split into each unique word)
#c.debate.matches <- list()
#for(i in 1:length(c.split.text)){
#  c.debate.matches[[i]] <- which(unlist(lapply(c.split.text[[i]], function(x) grepl("debate", x))))
#}


##for loop denoting whether debate is mentioned in a given article (this will be used to subset to only articles mentioning debate)
#c.debate.mentioned <- NULL
#for(i in 1:length(c.debate.matches)){
#  c.debate.mentioned[i] <- as.numeric(sum(c.debate.matches[[i]]) > 0)
#}

#---------------------------------------------
##for loop searching for whether the word debate is mentioned within each list element (where each list element is an article split into each unique word)
#fh.debate.matches <- list()
#for(i in 1:length(fh.split.text)){
#  fh.debate.matches[[i]] <- which(unlist(lapply(fh.split.text[[i]], function(x) grepl("debate", x))))
#}


##for loop denoting whether debate is mentioned in a given article (this will be used to subset to only articles mentioning debate)
#fh.debate.mentioned <- NULL
#for(i in 1:length(fh.debate.matches)){
#  fh.debate.mentioned[i] <- as.numeric(sum(fh.debate.matches[[i]]) > 0)
#}


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#df.c$c.debate.mentioned <- c.debate.mentioned
#df.c$c.debate.mentioned <- c.debate.mentioned
#c.all.articles.by.date$DebateMentioned <- aggregate(df.c$c.debate.mentioned, by=list(Category=df.c$close.dates.charleston.clean), FUN=sum)$x

#c.all.articles.by.date$ProportionMentioningDebate <- c.all.articles.by.date$DebateMentioned/c.all.articles.by.date$TotalArticles


#df.fh$fh.debate.mentioned <- fh.debate.mentioned

#fh.all.articles.by.date$DebateMentioned <- aggregate(df.fh$fh.debate.mentioned, by=list(Category=df.fh$close.dates.forthood.clean), FUN=sum)$x

#fh.all.articles.by.date$ProportionMentioningDebate <- fh.all.articles.by.date$DebateMentioned/fh.all.articles.by.date$TotalArticles

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#pdf("Figure_7B.pdf")
#pdf("Figure_7B.pdf")
#plot(seq(nrow(c.all.articles.by.date)), c.all.articles.by.date$ProportionMentioningDebate, type="l", lwd=3, 
#     ylab="Proportion of Articles Mentioning Debate", 
#     xlab="Weeks After Incident", xaxt="n", main="Proportion of Articles", ylim=c(0, 1.15)) 
#lines(seq(nrow(fh.all.articles.by.date)), fh.all.articles.by.date$ProportionMentioningDebate, lwd=3, lty=2)
#axis(side=1, at=c(7, 14), labels = c("1 week", "2 weeks"))
#legend("topright", legend=c("Charleston", "Fort Hood"), lty=c(1,2), lwd=3)
#dev.off()

#--------extension begins
# detecting missingness

#had no missingness-- final survey data
missmap(dat)
dev.copy(pdf,"../Users/danna/Desktop/applied regression/project folder/missmap.pdf")
dev.off()

#terrorism data -- did have missingness
missmap(dat2)
dev.copy(pdf,"../applied regression/project folder/terrormap.pdf")
dev.off()


#check to see which variables in particular
missingdata <- dat2[0,]
for(i in 1:ncol(missingdata)) {
  missingdata[1,i] <- length(which(is.na(dat2[,i])))
}

#fixing missingness 
#variable mean imp for highislamoprej

#doesnt work V again, these two variables were having issues due to the factor argument that i
#could not figure out.
dat2$highip_vmi <- dat2$islamoprejudicehigh 
dat2$islamoprejudicehigh_vmi[which(is.na(dat2$islamoprejudicehigh))] <- mean(dat2$islamoprejudicehigh, na.rm=TRUE)

dat2$highislamop_vmi <- dat2$highislamoprej








