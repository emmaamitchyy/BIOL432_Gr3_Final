# BIOL 432 - Group 3 
# Final Project - Draft 1 
# Analysis of the COVID Metabolomics Dataset: 


# Load Packages: 
library(ggplot2)
library(tidyverse)
library(randomForest)
library(vegan)
library(pheatmap)
library(caret)

# Load datasets: 
feature_A <- read.csv("/Users/emmamitchell/Downloads/FeatDatA.csv")
feature_B <- read.csv("/Users/emmamitchell/Downloads/FeatDatB.csv")
feature_C <- read.csv("/Users/emmamitchell/Downloads/FeatDatC.csv")
feature_CT <- read.csv("/Users/emmamitchell/Downloads/FeatDatCT.csv")


summary(feature_A)
summary(feature_B)
summary(feature_C)
summary(feature_CT)

#clean data: 
clean_data <- function(df) {
  df$Class.name <- as.factor(df$Class.name) #covid status 
  dfSex <- as.factor(df$Sex)
  df$Age <- as.numeric(df$Age)
  return(na.omit(df)) #remove missing values
}

feature_A <- clean_data(feature_A)
feature_B <- clean_data(feature_B)
feature_C <- clean_data(feature_C)
feature_CT <- clean_data(feature_CT)




