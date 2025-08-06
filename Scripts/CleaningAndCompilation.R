# 1. Get everything combined into one object ----
library(tidyverse)
library(readr)
library(dplyr)
library(Rcpp)
library(readxl)
library(magrittr)
library(stringr)
library(tidyr)
library(ggplot2)

getwd()

# Load in all data
cxsp1 <- read_excel("Cox Spring/CXSP_1_23.xlsx")
cxsp2 <- read_excel("Cox Spring/CXSP_2_23.xlsx")
cxsp3 <- read_excel("Cox Spring/CXSP_1_24.xlsx")
cxsp4 <- read_excel("Cox Spring/CXSP_2_24.xlsx")
emig1 <- read_excel("Emigration Canyon/EMIG_1_23.xlsx")
emig2 <- read_excel("Emigration Canyon/EMIG_2_23.xlsx")
emig3 <- read_excel("Emigration Canyon/EMIG_1_24.xlsx")
emig4 <- read_excel("Emigration Canyon/EMIG_2_24.xlsx")
frnk1 <- read_excel("Franklin Basin/FRNK_1_23.xlsx")
frnk2 <- read_excel("Franklin Basin/FRNK_2_23.xlsx")
frnk3 <- read_excel("Franklin Basin/FRNK_1_24.xlsx")
frnk4 <- read_excel("Franklin Basin/FRNK_2_24.xlsx")
mocr1 <- read_excel("Monte Cristo/MOCR_1_23.xlsx")
mocr2 <- read_excel("Monte Cristo/MOCR_2_23.xlsx")
mocr3 <- read_excel("Monte Cristo/MOCR_1_24.xlsx")
mocr4 <- read_excel("Monte Cristo/MOCR_2_24.xlsx")
mttp1 <- read_excel("Mount Timpanogos/MTTP_1_23.xlsx")
mttp2 <- read_excel("Mount Timpanogos/MTTP_2_23.xlsx")
mttp3 <- read_excel("Mount Timpanogos/MTTP_1_24.xlsx")
mttp4 <- read_excel("Mount Timpanogos/MTTP_2_24.xlsx")
smmh1 <- read_excel("Smith and Morehouse/SMMH_1_23.xlsx")
smmh2 <- read_excel("Smith and Morehouse/SMMH_2_23.xlsx")
smmh3 <- read_excel("Smith and Morehouse/SMMH_1_24.xlsx")
smmh4 <- read_excel("Smith and Morehouse/SMMH_2_24.xlsx")
soap1 <- read_excel("Soapstone Basin/SOAP_1_23.xlsx")
soap2 <- read_excel("Soapstone Basin/SOAP_2_23.xlsx")
soap3 <- read_excel("Soapstone Basin/SOAP_1_24.xlsx")
soap4 <- read_excel("Soapstone Basin/SOAP_2_24.xlsx")
tmpf1 <- read_excel("Temple Fork/TMPF_1_23.xlsx")
tmpf2 <- read_excel("Temple Fork/TMPF_2_23.xlsx")
tmpf3 <- read_excel("Temple Fork/TMPF_1_24.xlsx")
tmpf4 <- read_excel("Temple Fork/TMPF_2_24.xlsx")

## Change classes / add visit column ----
# Check for any class issues and standardize. Also add a visit number column to differentiate visits

cxsp1 %>% dplyr::summarise_all(class)
cxsp1 <- cxsp1 %>% dplyr::mutate(Visit = 1)
cxsp1 <- cxsp1 %>% dplyr::filter(cxsp1$DBH != "<2") # Get rid of small non-hosts
cxsp1$DBH = as.numeric(cxsp1$DBH)
cxsp1$Height = as.numeric(cxsp1$Height)
cxsp1$`Canopy Height` = as.numeric(cxsp1$`Canopy Height`)
cxsp1$uniqueID <- paste(cxsp1$`Tag #`, cxsp1$Species, sep="") # give trees unique ID to check for duplicates
cxsp1$WSB <- NA

cxsp1[duplicated(cxsp1$uniqueID),] # We're good



cxsp2 %>% dplyr::summarise_all(class)
cxsp2 <- cxsp2 %>% dplyr::mutate(Visit = 2)
cxsp2 <- cxsp2 %>% dplyr::filter(cxsp2$DBH != "<2")
cxsp2$DBH = as.numeric(cxsp2$DBH)
cxsp2$Height = as.numeric(cxsp2$Height)
cxsp2$`Canopy Height` = as.numeric(cxsp2$`Canopy Height`)
cxsp2$uniqueID <- paste(cxsp2$`Tag #`, cxsp2$Species, sep="") # give trees unique ID to check for duplicates
cxsp2$WSB <- NA

cxsp2[duplicated(cxsp2$uniqueID),] # we're good


cxsp3 %>% dplyr::summarise_all(class)
cxsp3 <- cxsp3 %>% dplyr::mutate(Visit = 3)
cxsp3 <- cxsp3 %>% dplyr::filter(cxsp3$DBH != "<2")
cxsp3$DBH = as.numeric(cxsp3$DBH)
cxsp3$Height = as.numeric(cxsp3$Height)
cxsp3$`Canopy Height` = as.numeric(cxsp3$`Canopy Height`)
cxsp3$uniqueID <- paste(cxsp3$`Tag #`, cxsp3$Species, sep="") # give trees unique ID to check for duplicates
cxsp3 <- cxsp3 %>% dplyr::select(-c("Sap/SK")) # Get rid of sap/sk column
cxsp3$WSB <- NA

cxsp3[duplicated(cxsp3$uniqueID),]

cxsp4 %>% dplyr::summarise_all(class)
cxsp4 <- cxsp4 %>% dplyr::mutate(Visit = 4)
cxsp4 <- cxsp4 %>% dplyr::filter(cxsp4$DBH != "<2")
cxsp4$DBH = as.numeric(cxsp4$DBH)
cxsp4$Height = as.numeric(cxsp4$Height)
cxsp4$`Canopy Height` = as.numeric(cxsp4$`Canopy Height`)
cxsp4$uniqueID <- paste(cxsp4$`Tag #`, cxsp4$Species, sep="") # give trees unique ID to check for duplicates
cxsp4 <- cxsp4 %>% dplyr::select(-c("Sap/SK"))
cxsp4$WSB <- NA

cxsp4[duplicated(cxsp4$uniqueID),]

# combine new sap/sk with old data to get same size objects - actually, we're just going to get rid of sk/sap since we're not going to be using it
# cxsp1 <- merge(cxsp1, cxsp3[, c("uniqueID", "Sap/SK")], by = "uniqueID", all.x = TRUE)

# Emigration Canyon

emig1 %>% dplyr::summarise_all(class) # coerce HT and DBH to numeric
emig1 <- emig1 %>% dplyr::filter(emig1$DBH != "< 2")
emig1$Height = as.numeric(emig1$Height)
emig1$DBH = as.numeric(emig1$DBH)
emig1 <- emig1 %>% dplyr::mutate(Visit = 1)
emig1$uniqueID <- paste(emig1$`Tag #`, emig1$Species, sep="") # give trees unique ID to check for duplicates
emig1[duplicated(emig1$uniqueID),]
emig1$WSB <- NA # Add WSB column
#emig1$uniqueID[duplicated(emig1$uniqueID)] # PICO repeated, couldn't find, ABLA fixed

emig2 %>% dplyr::summarise_all(class) # coerce HT and DBH to numeric
emig2 <- emig2 %>% dplyr::filter(emig2$DBH != "< 2")
emig2$Height = as.numeric(emig2$Height)
emig2$DBH = as.numeric(emig2$DBH)
emig2 <- emig2 %>% dplyr::mutate(Visit = 2)
emig2$uniqueID <- paste(emig2$`Tag #`, emig2$Species, sep="") # give trees unique ID to check for duplicates
emig2[duplicated(emig2$uniqueID),]
emig2$WSB <- NA # Add WSB column
#emig2$uniqueID[duplicated(emig2$uniqueID)] # same abla again - fixed

emig3 %>% dplyr::summarise_all(class) # coerce HT and DBH to numeric
emig3 <- emig3 %>% dplyr::filter(emig3$DBH != "< 2")
names(emig3)[1] <- "Site ID"
names(emig3)[2] <- "Tag #"
emig3$Height = as.numeric(emig3$Height)
emig3$DBH = as.numeric(emig3$DBH)
emig3 <- emig3 %>% dplyr::mutate(Visit = 3)
emig3$uniqueID <- paste(emig3$`Tag #`, emig3$Species, sep="") # give trees unique ID to check for duplicates
emig3[duplicated(emig3$uniqueID),]
emig3 <- emig3 %>% dplyr::select(-c("Sap/SK"))

emig4 %>% dplyr::summarise_all(class) # coerce HT and DBH to numeric
emig4 <- emig4 %>% dplyr::filter(emig4$DBH != "< 2")
names(emig4)[1] <- "Site ID"
names(emig4)[2] <- "Tag #"
emig4$Height = as.numeric(emig4$Height)
emig4$DBH = as.numeric(emig4$DBH)
emig4 <- emig4 %>% dplyr::mutate(Visit = 4)
emig4$uniqueID <- paste(emig4$`Tag #`, emig4$Species, sep="") # give trees unique ID to check for duplicates
emig4[duplicated(emig4$uniqueID),]
emig4 <- emig4 %>% dplyr::select(-c("Sap/SK"))

# Franklin Basin

frnk1 %>% dplyr::summarise_all(class) # coerce HT and DBH to numeric
frnk1$Height = as.numeric(frnk1$Height)
frnk1$DBH = as.numeric(frnk1$DBH)
frnk1 <- frnk1 %>% dplyr::mutate(Visit = 1)
frnk1$uniqueID <- paste(frnk1$`Tag #`, frnk1$Species, sep="") # give trees unique ID to check for duplicates
frnk1[duplicated(frnk1$uniqueID),]
frnk1 <- frnk1[-which(frnk1$uniqueID == "7328ABLA" & frnk1$DBH == 2.3), ]
#frnk1$uniqueID[duplicated(frnk1$uniqueID)] # fixed one, 7328 appears to be true duplicate. We'll remove when we get to analysis
frnk1$WSB <- NA

frnk2 %>% dplyr::summarise_all(class) # coerce HT and DBH to numeric
frnk2$Height = as.numeric(frnk2$Height)
frnk2$DBH = as.numeric(frnk2$DBH)
frnk2 <- frnk2 %>% dplyr::mutate(Visit = 2)
frnk2$uniqueID <- paste(frnk2$`Tag #`, frnk2$Species, sep="") # give trees unique ID to check for duplicates
frnk2[duplicated(frnk2$uniqueID),]
frnk2 <- frnk2[-which(frnk2$uniqueID == "7328ABLA" & frnk2$DBH == 2.3), ]
frnk2$WSB <- NA

frnk3 %>% dplyr::summarise_all(class) # coerce HT and DBH to numeric
frnk3$Height = as.numeric(frnk3$Height)
frnk3$DBH = as.numeric(frnk3$DBH)
frnk3 <- frnk3 %>% dplyr::mutate(Visit = 3)
frnk3$uniqueID <- paste(frnk3$`Tag #`, frnk3$Species, sep="") # give trees unique ID to check for duplicates
frnk3[duplicated(frnk3$uniqueID),]
frnk3 <- frnk3[-which(frnk3$uniqueID == "7328ABLA" & frnk3$DBH == 2.3), ]
frnk3$`Tag #`[which(frnk3$uniqueID == "8POTR" & frnk3$DBH ==2.8)] <- 6
frnk3$uniqueID <- paste(frnk3$`Tag #`, frnk3$Species, sep="") # make unique names again with correction
frnk3 <- frnk3 %>% dplyr::select(-c("Sap/Sk"))
frnk3$WSB <- NA


frnk4 %>% dplyr::summarise_all(class) # coerce HT and DBH to numeric
frnk4$Height = as.numeric(frnk4$Height)
frnk4$DBH = as.numeric(frnk4$DBH)
frnk4 <- frnk4 %>% dplyr::mutate(Visit = 4)
frnk4$uniqueID <- paste(frnk4$`Tag #`, frnk4$Species, sep="") # give trees unique ID to check for duplicates
frnk4[duplicated(frnk4$uniqueID),]
frnk4 <- frnk4[-which(frnk4$uniqueID == "7328ABLA" & frnk4$DBH == 2.3), ]
frnk4$`Tag #`[which(frnk4$uniqueID == "8POTR" & frnk4$DBH ==2.8)] <- 6
frnk4$uniqueID <- paste(frnk4$`Tag #`, frnk4$Species, sep="") 
frnk4 <- frnk4 %>% dplyr::select(-c("Sap/Sk"))
frnk4$WSB <- NA

# Monte Cristo

mocr1 %>% dplyr::summarise_all(class) # coerce HT and DBH to numeric
mocr1$Height = as.numeric(mocr1$Height)
mocr1$DBH = as.numeric(mocr1$DBH)
mocr1 <- mocr1 %>% dplyr::mutate(Visit = 1)
mocr1$uniqueID <- paste(mocr1$`Tag #`, mocr1$Species, sep="") # give trees unique ID to check for duplicates
mocr1[duplicated(mocr1$uniqueID),]
#mocr1$uniqueID[duplicated(mocr1$uniqueID)] # think we're clear here
mocr1$WSB <- NA

mocr2 %>% dplyr::summarise_all(class) # coerce HT and DBH to numeric
mocr2$Height = as.numeric(mocr2$Height)
mocr2$DBH = as.numeric(mocr2$DBH)
mocr2 <- mocr2 %>% dplyr::mutate(Visit = 2)
mocr2$uniqueID <- paste(mocr2$`Tag #`, mocr2$Species, sep="") # give trees unique ID to check for duplicates
mocr2[duplicated(mocr2$uniqueID),]
#mocr2$uniqueID[duplicated(mocr2$uniqueID)] # we good
mocr2$WSB <- NA

mocr3 %>% dplyr::summarise_all(class) # coerce HT and DBH to numeric
mocr3$Height = as.numeric(mocr3$Height)
mocr3$DBH = as.numeric(mocr3$DBH)
mocr3 <- mocr3 %>% dplyr::mutate(Visit = 3)
mocr3$uniqueID <- paste(mocr3$`Tag #`, mocr3$Species, sep="") # give trees unique ID to check for duplicates
mocr3[duplicated(mocr3$uniqueID),]
mocr3$WSB <- NA
mocr3 <- mocr3 %>% dplyr::select(-c("Sap/SK"))

mocr4 %>% dplyr::summarise_all(class) # coerce HT and DBH to numeric
mocr4$Height = as.numeric(mocr4$Height)
mocr4$DBH = as.numeric(mocr4$DBH)
mocr4 <- mocr4 %>% dplyr::mutate(Visit = 4)
mocr4$uniqueID <- paste(mocr4$`Tag #`, mocr4$Species, sep="") # give trees unique ID to check for duplicates
mocr4[duplicated(mocr4$uniqueID),]
mocr4$WSB <- NA
mocr4 <- mocr4 %>% dplyr::select(-c("Sap/SK"))


# Mount Timpanogos

mttp1 %>% dplyr::summarise_all(class) # coerce HT and DBH to numeric
mttp1$Height = as.numeric(mttp1$Height)
mttp1$DBH = as.numeric(mttp1$DBH)
mttp1 <- mttp1 %>% dplyr::mutate(Visit = 1)
mttp1$uniqueID <- paste(mttp1$`Tag #`, mttp1$Species, sep="") # give trees unique ID to check for duplicates
mttp1[duplicated(mttp1$uniqueID),]
mttp1$WSB <- NA
#mttp1$uniqueID[duplicated(mttp1$uniqueID)] # we good bitch


mttp2 %>% dplyr::summarise_all(class) # coerce HT and DBH to numeric
mttp2$Height = as.numeric(mttp2$Height)
mttp2$DBH = as.numeric(mttp2$DBH)
mttp2 <- mttp2 %>% dplyr::mutate(Visit = 2)
mttp2$uniqueID <- paste(mttp2$`Tag #`, mttp2$Species, sep="") # give trees unique ID to check for duplicates
mttp2[duplicated(mttp2$uniqueID),]
mttp2$WSB <- NA
#mttp2$uniqueID[duplicated(mttp2$uniqueID)] # we good

mttp3 %>% dplyr::summarise_all(class) # coerce HT and DBH to numeric
mttp3$Height = as.numeric(mttp3$Height)
mttp3$DBH = as.numeric(mttp3$DBH)
mttp3 <- mttp3 %>% dplyr::mutate(Visit = 3)
mttp3$uniqueID <- paste(mttp3$`Tag #`, mttp3$Species, sep="") # give trees unique ID to check for duplicates
mttp3[duplicated(mttp3$uniqueID),]
mttp3 <- mttp3 %>% dplyr::select(-c("Sap/SK"))
mttp3$WSB <- NA


mttp4 %>% dplyr::summarise_all(class) # coerce HT and DBH to numeric
mttp4$Height = as.numeric(mttp4$Height)
mttp4$DBH = as.numeric(mttp4$DBH)
mttp4 <- mttp4 %>% dplyr::mutate(Visit = 4)
mttp4$uniqueID <- paste(mttp4$`Tag #`, mttp4$Species, sep="") # give trees unique ID to check for duplicates
mttp4[duplicated(mttp4$uniqueID),]
mttp4 <- mttp4 %>% dplyr::select(-c("Sap/SK"))
mttp4$WSB <- NA

# Smith and Morehouse

smmh1 %>% dplyr::summarise_all(class) # coerce DBH to numeric
smmh1$DBH = as.numeric(smmh1$DBH)
smmh1 <- smmh1 %>% dplyr::mutate(Visit = 1)
smmh1$uniqueID <- paste(smmh1$`Tag #`, smmh1$Species, sep="") # give trees unique ID to check for duplicates
smmh1[duplicated(smmh1$uniqueID),]
#smmh1$uniqueID[duplicated(smmh1$uniqueID)] # fixed, need to re-load
smmh1$WSB <- NA

smmh2 %>% dplyr::summarise_all(class) # coerce HT and DBH to numeric
smmh2 <- smmh2 %>% dplyr::filter(smmh2$DBH != "<2") # Get rid of non-hosts we don't need
smmh2$Height = as.numeric(smmh2$Height)
smmh2$DBH = as.numeric(smmh2$DBH)
smmh2 <- smmh2 %>% dplyr::mutate(Visit = 2)
smmh2$uniqueID <- paste(smmh2$`Tag #`, smmh2$Species, sep="")
smmh2[duplicated(smmh2$uniqueID),]
smmh2$WSB <- NA
smmh2 <- smmh2[-which(smmh2$uniqueID == "5504ABLA" & smmh2$Height == 8.3), ] # get rid of mistake tree

smmh3 %>% dplyr::summarise_all(class) # coerce HT and DBH to numeric
smmh3 <- smmh3 %>% dplyr::filter(smmh3$DBH != "<2")
smmh3$Height = as.numeric(smmh3$Height)
smmh3$DBH = as.numeric(smmh3$DBH)
smmh3 <- smmh3 %>% dplyr::mutate(Visit = 3)
smmh3$uniqueID <- paste(smmh3$`Tag #`, smmh3$Species, sep="")
smmh3[duplicated(smmh3$uniqueID),]
#smmh3 <- smmh3 %>% tidyr::drop_na(`Site ID`) # drop excess NA's
smmh3 <- smmh3 %>% dplyr::select(-c("Sap/SK"))
smmh3$WSB <- NA

smmh4 %>% dplyr::summarise_all(class) # coerce HT and DBH to numeric
smmh4 <- smmh4 %>% dplyr::filter(smmh4$DBH != "<2")
smmh4$Height = as.numeric(smmh4$Height)
smmh4$DBH = as.numeric(smmh4$DBH)
smmh4 <- smmh4 %>% dplyr::mutate(Visit = 4)
smmh4$uniqueID <- paste(smmh4$`Tag #`, smmh4$Species, sep="")
smmh4[duplicated(smmh4$uniqueID),]
#smmh4 <- smmh4 %>% tidyr::drop_na(`Site ID`)
smmh4 <- smmh4 %>% dplyr::select(-c("Sap/SK"))
smmh4$WSB <- NA

# Soapstone Basin

soap1 %>% dplyr::summarise_all(class) # coerce DBH to numeric and canopy height to character
soap1$`Canopy Height` = as.character(soap1$`Canopy Height`)
soap1$DBH = as.numeric(soap1$DBH)
soap1$Height = as.numeric(soap1$Height)
soap1 <- soap1 %>% dplyr::mutate(Visit = 1)
soap1$uniqueID <- paste(soap1$`Tag #`, soap1$Species, sep="") # give trees unique ID to check for duplicates
soap1[duplicated(soap1$uniqueID),]
soap1$WSB <- NA

soap2 %>% dplyr::summarise_all(class) # coerce DBH to numeric and canopy height to chr
soap2$`Canopy Height` = as.character(soap2$`Canopy Height`)
soap2$Height = as.numeric(soap2$Height)
soap2$DBH = as.numeric(soap2$DBH)
soap2 <- soap2 %>% dplyr::mutate(Visit = 2)
soap2$uniqueID <- paste(soap2$`Tag #`, soap2$Species, sep="") # give trees unique ID to check for duplicates
soap2[duplicated(soap2$uniqueID),]
soap2$WSB <- NA


soap3 %>% dplyr::summarise_all(class) # coerce DBH to numeric and canopy height to chr
soap3$`Canopy Height` = as.character(soap3$`Canopy Height`)
soap3$Height = as.numeric(soap3$Height)
soap3$DBH = as.numeric(soap3$DBH)
soap3 <- soap3 %>% dplyr::mutate(Visit = 3)
soap3$uniqueID <- paste(soap3$`Tag #`, soap3$Species, sep="") # give trees unique ID to check for duplicates
soap3[duplicated(soap3$uniqueID),]
soap3$WSB <- NA
soap3 <- soap3 %>% dplyr::select(-c("Sap/SK"))

soap4 %>% dplyr::summarise_all(class) # coerce DBH to numeric and canopy height to chr
soap4$`Canopy Height` = as.character(soap4$`Canopy Height`)
soap4$Height = as.numeric(soap4$Height)
soap4$DBH = as.numeric(soap4$DBH)
soap4 <- soap4 %>% dplyr::mutate(Visit = 4)
soap4$uniqueID <- paste(soap4$`Tag #`, soap4$Species, sep="") # give trees unique ID to check for duplicates
soap4[duplicated(soap4$uniqueID),]
soap4$WSB <- NA
soap4 <- soap4 %>% dplyr::select(-c("Sap/SK"))

# Temple Fork

tmpf1 %>% dplyr::summarise_all(class) # coerce DBH to numeric and can ht to chr
tmpf1$`Canopy Height` = as.character(tmpf1$`Canopy Height`)
tmpf1$DBH = as.numeric(tmpf1$DBH)
tmpf1$Height = as.numeric(tmpf1$Height)
tmpf1 <- tmpf1 %>% dplyr::mutate(Visit = 1)
tmpf1$uniqueID <- paste(tmpf1$`Tag #`, tmpf1$Species, sep="") # give trees unique ID to check for duplicates
tmpf1[duplicated(tmpf1$uniqueID),]
tmpf1$WSB <- NA


tmpf2 %>% dplyr::summarise_all(class) # coerce DBH to numeric and can ht to chr
tmpf2$`Canopy Height` = as.character(tmpf2$`Canopy Height`)
tmpf2$DBH = as.numeric(tmpf2$DBH)
tmpf2$Height = as.numeric(tmpf2$Height)
tmpf2 <- tmpf2 %>% dplyr::mutate(Visit = 2)
tmpf2$uniqueID <- paste(tmpf2$`Tag #`, tmpf2$Species, sep="") # give trees unique ID to check for duplicates
tmpf2[duplicated(tmpf2$uniqueID),]
tmpf2$WSB <- NA


tmpf3 %>% dplyr::summarise_all(class) # coerce DBH to numeric and can ht to chr
tmpf3$`Canopy Height` = as.character(tmpf3$`Canopy Height`)
tmpf3$DBH = as.numeric(tmpf3$DBH)
tmpf3$Height = as.numeric(tmpf3$Height)
tmpf3 <- tmpf3 %>% dplyr::mutate(Visit = 3)
tmpf3$uniqueID <- paste(tmpf3$`Tag #`, tmpf3$Species, sep="") # give trees unique ID to check for duplicates
tmpf3[duplicated(tmpf3$uniqueID),]
tmpf3 <- tmpf3 %>% dplyr::select(-c("Sap/SK"))

tmpf4 %>% dplyr::summarise_all(class) # coerce DBH to numeric and can ht to chr
tmpf4$`Canopy Height` = as.character(tmpf4$`Canopy Height`)
tmpf4$DBH = as.numeric(tmpf4$DBH)
tmpf4$Height = as.numeric(tmpf4$Height)
tmpf4 <- tmpf4 %>% dplyr::mutate(Visit = 4)
tmpf4$uniqueID <- paste(tmpf4$`Tag #`, tmpf4$Species, sep="") # give trees unique ID to check for duplicates
tmpf4[duplicated(tmpf4$uniqueID),]
tmpf4 <- tmpf4 %>% dplyr::select(-c("Sap/SK"))

## Combine files for cleaning, then separate into host / non-host files for each site ----

# All variables changed classes to character so class changes need to be done again, ugh!

combined <- rbind(cxsp1, cxsp2, cxsp3, cxsp4, emig1, emig2, emig3, emig4, frnk1, frnk2, frnk3, frnk4, mocr1, mocr2, mocr3, mocr4, mttp1, mttp2, mttp3, mttp4, smmh1, smmh2, smmh3, smmh4, soap1, soap2, soap3, soap4, tmpf1, tmpf2, tmpf3, tmpf4)
head(combined)
combined$`Tag #` = as.numeric(combined$`Tag #`)
combined$`DBH` = as.numeric(combined$`DBH`) # changed all <1 and <2 (clumps) to NA
combined$`Height` = as.numeric(combined$`Height`) # Heights not recorded are NA's
combined$`Canopy Height` = as.numeric(combined$`Canopy Height`) # Same as height
combined$`Dead / Alive` = as.numeric(combined$`Dead / Alive`)
combined$`Gouting` = as.numeric(combined$`Gouting`)
combined$`Broom (#)` = as.numeric(combined$`Broom (#)`) # need to change NA's to 0
combined$`Broom (#)` <- combined$`Broom (#)` %>% tidyr::replace_na(0) # change Na's in broom var to 0

## Make sure all variables are entered the same ----

# Site ID
unique(combined$`Site ID`) # some NA sites?
which(is.na(combined$`Site ID`))
#combined$`Site ID` <- combined$`Site ID` %>% tidyr::replace_na("FRNK") # Replace NA is FRNK

# Tag #
unique(combined$`Tag #`)
which(is.na(combined$`Tag #`)) # no NA's!

# Species
unique(combined$Species) # Some weird ones, let's find out where they are
#combined %>% dplyr::filter(Species == "PI") # SMMH #22 is PICO
#combined <- combined %>% replace(combined == "PI", "PICO") # Replace mistake
#combined %>% dplyr::filter(Species == "POE") # SMMH again - looks like PIEN, but we'll doublecheck after replacement
#combined <- combined %>% replace(combined == "POE", "PIEN")
#combined %>% dplyr::filter(`Tag #` == 71) # no repeats for Species so we're good!
combined %>% dplyr::filter(Species == "Blue spruce") # Just one at SMMH of course
combined <- combined %>% replace(combined == "Blue spruce", "PIPU")
combined %>% dplyr::filter(Species == "POC") # Shocker, SMMH again - looks like PICO
combined <- combined %>% replace(combined == "POC", "PICO")

# DBH
unique(combined$DBH)
# make a quick histogram
  ggplot(combined, aes(x=DBH)) +
  geom_histogram(bins = 10) +
  xlim(-1, 50)                    # looks like what we'd expect, nice Poisson distribution
  
# Investigating Temple Fork
tmpf <- combined %>% filter(combined$`Site ID` == "TMPF")
  ggplot(tmpf, aes(x=DBH)) +
    geom_histogram(bins = 10) +
  xlim(-1, 50) 

  
# In plot-level stats, we missed a mistake here, tree 158 is way too big
combined$DBH[combined$`Tag #` == 158 & combined$`Site ID` == "TMPF" ] <- 8.4
  
# Height
# make a quick histogram
ggplot(combined, aes(x=Height)) +
    geom_histogram(bins = 10) +
    xlim(0, 150)                  # Again, looks like what we'd expect, nice Poisson distribution

# Canopy Ht
# make a quick histogram
ggplot(combined, aes(x=`Canopy Height`)) +
  geom_histogram(bins = 10) +
  xlim(0, 75) +
  ylim(0, 100)                    # Not exactly Poisson

# Dead/alive
unique(combined$`Dead / Alive`) # some NA's that shouldn't be there?
na <- combined %>% filter(is.na(`Dead / Alive`)) # look at all NA's

# Fixed all the mistakes in the data files

#na <- combined %>% filter(combined$`Site ID` == "MOCR") %>% filter(is.na(`Dead / Alive`)) %>% filter(is.na(Notes)) # trees without notes should be labelled dead or alive
#combined$`Tag #`[combined$`Site ID` == "MOCR" & is.na(combined$`Dead / Alive`) & is.na(combined$Notes)]
#combined[-which(combined$`Tag #` == 5028 & combined$`Site ID` == "MOCR"), ] # drop 5028 from data
#combined$`Dead / Alive`[combined$`Tag #` == 5160 & combined$`Site ID` == "MOCR"] <- 1 # change 5160 to alive b/c we know it was alive

#na <- combined %>% filter(is.na(`Dead / Alive`) & combined$`Site ID` == "SMMH") # we have three trees to deal with
#combined <- combined[-which(combined$`Tag #` == 5414 & combined$`Site ID` == "SMMH"), ] # remove 5414
#combined$`Dead / Alive`[combined$`Tag #` == 5395 & combined$`Site ID` == "SMMH"] <- 1 # change other two to alive
#combined$`Dead / Alive`[combined$`Tag #` == 5504 & combined$`Site ID` == "SMMH"] <- 1

#na <- combined %>% filter(is.na(`Dead / Alive`) & combined$`Site ID` == "FRNK")
#combined$`Dead / Alive`[combined$`Tag #` == 5888 & combined$`Site ID` == "FRNK"] <- 1
#combined <- combined[-which(combined$`Tag #` == 5953 & combined$`Site ID` == "FRNK"), ]
#combined$`Dead / Alive`[combined$`Tag #` == 7122 & combined$`Site ID` == "FRNK"] <- 1
#combined$`Dead / Alive`[combined$`Tag #` == 7192 & combined$`Site ID` == "FRNK"] <- 1

#na <- combined %>% filter(is.na(`Dead / Alive`) & combined$`Site ID` == "CXSP")
#combined <- combined[-which(combined$`Tag #` == 4 & combined$Species == "POTR" & combined$`Site ID` == "CXSP"), ]
#combined <- combined[-which(combined$`Tag #` == 20 & combined$Species == "PSMEG" & combined$`Site ID` == "CXSP"), ]

# All Na's taken care of

# Infestation Location
unique(combined$`Inf Location`) # Let's make everything the same again
combined <- combined %>% replace(combined == "Bo", "BO")
combined <- combined %>% replace(combined == "Br", "BR")
combined <- combined %>% replace(combined == "Bo, Br", "BR, BO")
combined <- combined %>% replace(combined == "Br, Bo", "BR, BO")
combined <- combined %>% replace(combined == "BO,BR", "BR, BO")
combined <- combined %>% replace(combined == "Bo,Br", "BR, BO")
combined <- combined %>% replace(combined == "B", "BO")

# Gouting
unique(combined$Gouting)

# Broom #
unique(combined$`Broom (#)`)

# Beetle
unique(combined$Beetle) # lots of versions with CR in it, lets change those to just TB and combos to the same code
combined <- combined %>% replace(combined == "TB-CR", "TB")
combined <- combined %>% replace(combined == "CR", "TB")
combined <- combined %>% replace(combined == "TB (Cr)", "TB")
combined <- combined %>% replace(combined == "TB, CR", "TB")
combined <- combined %>% replace(combined == "TB (CR)", "TB")
combined <- combined %>% replace(combined == "Cr", "TB")
combined <- combined %>% replace(combined == "Cr (this yr", "TB")
combined <- combined %>% replace(combined == "Fe, Cr", "TB, FE")
combined <- combined %>% replace(combined == "TB(FE)", "TB, FE")

# Rot
unique(combined$Rot)
combined %>% dplyr::filter(Rot == "1 - 1")
combined$Rot[combined$`Tag #` == 467 & combined$`Site ID` == "EMIG"] <- "-" # change mistake

# NL
unique(combined$NL) # kind of expected this, gotta change spacing of ratings
combined <- combined %>% replace(combined == "1 - 1", "1-1")
combined <- combined %>% replace(combined == "1 - 2", "1-2")
combined <- combined %>% replace(combined == "1 - 3", "1-3")
combined <- combined %>% replace(combined == "1 - 4", "1-4")
combined <- combined %>% replace(combined == "1 - 5", "1-5")
combined <- combined %>% replace(combined == "1 -- 3", "1-3")
combined <- combined %>% replace(combined == "1 -2", "1-2")
combined <- combined %>% replace(combined == "NA", NA)

# NLW
unique(combined$NLW) # change some spacing again and also a 1 and 0 somewhere
combined <- combined %>% replace(combined == "1 - 6", "1-6")
combined <- combined %>% replace(combined == "4 - 1", "4-1")
combined <- combined %>% replace(combined == "3 - 1", "3-1")
combined <- combined %>% replace(combined == "3 - 0", "3-0")
combined <- combined %>% replace(combined == "4 - 17", "4-17")
combined <- combined %>% replace(combined == "4 - 4", "4-4")
combined <- combined %>% replace(combined == "2 - 51", "2-51")
combined <- combined %>% replace(combined == "2 - 0", "2-0")
combined <- combined %>% replace(combined == "1 - 0", "1-0")
combined <- combined %>% replace(combined == "4 - 5", "4-5")
combined <- combined %>% replace(combined == "3 - 3", "3-3")
combined <- combined %>% replace(combined == "4 - 11", "4-11")
combined <- combined %>% replace(combined == "2 - 1", "2-1")
combined <- combined %>% replace(combined == "2 - 2", "2-2")
combined <- combined %>% replace(combined == "2 - 5", "2-5")
combined <- combined %>% replace(combined == "3 - 2", "3-2")
combined <- combined %>% replace(combined == "4 - 0", "4-0")
combined %>% dplyr::filter(NLW == "1")
combined$NLW[combined$`Tag #` == 7349 & combined$`Site ID` == "FRNK"] <- "-"
combined$NLW[combined$`Tag #` == 7486 & combined$`Site ID` == "FRNK"] <- "-"

# NM
unique(combined$NM)
combined %>% dplyr::filter(NM == "44927")
combined$NM[combined$`Tag #` == 77 & combined$`Site ID` == "MTTP" & combined$Visit == 1 ] <- "1-1" # Fix mistake for first visit

# NMW
unique(combined$NMW)
combined <- combined %>% replace(combined == "3 - 12", "3-12")
combined <- combined %>% replace(combined == "3 - 8", "3-8")
combined <- combined %>% replace(combined == "3 - 15", "3-15")
combined <- combined %>% replace(combined == "4 - 3", "4-3")

# NH
unique(combined$NH)
combined <- combined %>% replace(combined == "1  - 3", "1-3")

# NHW
unique(combined$NHW)
combined <- combined %>% replace(combined == "4 - 30", "4-30")
combined <- combined %>% replace(combined == "1 - 17", "1-17")
combined <- combined %>% replace(combined == "4 - 2", "4-2")
combined <- combined %>% replace(combined == "1 - 7", "1-7")
combined <- combined %>% replace(combined == "2 - 9", "2-9")

# SL
unique(combined$SL)

# SLW
unique(combined$SLW)
combined <- combined %>% replace(combined == "4 - 7", "4-7")
combined <- combined %>% replace(combined == "1 - 41", "1-41")
combined <- combined %>% replace(combined == "4 - 19", "4-19")
combined <- combined %>% replace(combined == "4 - 13", "4-13")
combined <- combined %>% replace(combined == "3 - 4", "3-4")

# SM
unique(combined$SM)

# SMW
unique(combined$SMW)
combined <- combined %>% replace(combined == "3 - 22", "3-22")
combined <- combined %>% replace(combined == "4 - 33", "4-33")
combined <- combined %>% replace(combined == "1 - 9", "1-9")
combined <- combined %>% replace(combined == "2 - 4", "2-4")

# SH
unique(combined$SH)

# SHW
unique(combined$SHW)
combined <- combined %>% replace(combined == "2 - 14", "2-14")
combined <- combined %>% replace(combined == "2 - 7", "2-7")
combined <- combined %>% replace(combined == "4 - 21", "4-21")

# Visit
unique(combined$Visit)

# WSB
unique(combined$WSB)

## Get new site data - hosts and non-hosts are combined for now ----

library(writexl)

emig <- combined %>% dplyr::filter(`Site ID` == "EMIG") 
#write_xlsx(emig,"C:/Users/bwap/Documents/Project Files/Analysis/Cleaning and Compilation/Cleaning/emig.xlsx")

frnk <- combined %>% dplyr::filter(`Site ID` == "FRNK")
#write_xlsx(frnk,"C:/Users/bwap/Documents/Project Files/Analysis/Cleaning and Compilation/Cleaning/frnk.xlsx")

mocr <- combined %>% dplyr::filter(`Site ID` == "MOCR")
#write_xlsx(mocr,"C:/Users/bwap/Documents/Project Files/Analysis/Cleaning and Compilation/Cleaning/mocr.xlsx")

mttp <- combined %>% dplyr::filter(`Site ID` == "MTTP")
#write_xlsx(mttp,"C:/Users/bwap/Documents/Project Files/Analysis/Cleaning and Compilation/Cleaning/mttp.xlsx")

cxsp <- combined %>% dplyr::filter(`Site ID` == "CXSP")
#write_xlsx(cxsp,"C:/Users/bwap/Documents/Project Files/Analysis/Cleaning and Compilation/Cleaning/cxsp.xlsx")

smmh <- combined %>% dplyr::filter(`Site ID` == "SMMH")
#write_xlsx(smmh,"C:/Users/bwap/Documents/Project Files/Analysis/Cleaning and Compilation/Cleaning/smmh.xlsx")

soap <- combined %>% dplyr::filter(`Site ID` == "SOAP")
#write_xlsx(soap,"C:/Users/bwap/Documents/Project Files/Analysis/Cleaning and Compilation/Cleaning/soap.xlsx")

tmpf <- combined %>% dplyr::filter(`Site ID` == "TMPF")
#write_xlsx(tmpf,"C:/Users/bwap/Documents/Project Files/Analysis/Cleaning and Compilation/Cleaning/tmpf.xlsx")

# Export final cleaned file and all site data

#write_xlsx(combined,"C:/Users/bwap/Documents/Project Files/Analysis/Cleaning and Compilation/Cleaning/combined_cleaned2.xlsx")

# I think I'll end this here and start another file for stats

# We need to get this cleaned file straight as far as the trees we're going to be using. 
# We have some discrepency between the amount of live for each visit. Let's try to find out where these are and get them straight before analysis

# I think this is okay for now. There's cases for all these trees to be inculded at this point I think
abla <- combined %>% dplyr::filter(Species == "ABLA")
table(abla$Visit, abla$`Site ID`)


