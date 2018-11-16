# Google Trends Lab Project
# MET CS688 A1 Web Analytics and Mining (Fall 2018)
# Student: Tzupin Kuo



# Google Trends
rm(list=ls()); cat("\014")  # Clear all
library(readr)

# Read csv file
GT.Data <- read.csv(paste0('Data/', 'geoMap.csv'),
                    stringsAsFactors = FALSE,
                    skip = 2,
                    blank.lines.skip = TRUE,
                    header = T)

colnames(GT.Data) <- c("Region", "GB", "GG")
GT.Data[1:5,]

# Covert to numeric values
zGB <- as.numeric(GT.Data$GB)
zGG <- as.numeric(GT.Data$GG)

# Place back to dataframe
GT.Data$GB <- zGB
GT.Data$GG <- zGG

# Find NA and replace with zero
ix1 <- which(is.na(GT.Data$GB))
GT.Data$GB[ix1] <- 0
ix2 <- which(is.na(GT.Data$GG))
GT.Data$GG[ix2] <- 0



# Q1: Which are the states where GG is smaller than 1? Find those and replace them with zero.
# Ans: South Dakota, Maine, Idaho  (The R code is on the above)



# Q2: For How Many States GB > GG?
# Ans: 46
nrow(subset(GT.Data, GT.Data$GB > GT.Data$GG))



# Q3: Find any states where GG+10 > GB
# Ans: only Washington state
subset(GT.Data, GT.Data$GG+10 > GT.Data$GB)



# Q4: What is the % of states for which GG+10 > GB?
# Ans: 0.02173913
nrow(subset(GT.Data, GT.Data$GG+10 > GT.Data$GB))/nrow(GT.Data)



# Q5: What is the ratio GG/GB for the state of New Hampshire? 
# Ans: 0.5
gg.new.hampshire <- GT.Data$GG[GT.Data$Region == "New Hampshire"]
gb.new.hampshire <- GT.Data$GB[GT.Data$Region == "New Hampshire"]
ratio <- gg.new.hampshire/gb.new.hampshire 
ratio

# Q6: Create a Bar Plot of GG & GB values for each state.
library(ggplot2)   
library(reshape2)

GT.Data.m <- melt(GT.Data, id.vars='Region')  # Melting data  usgin melting function

ggplot(GT.Data.m, aes(Region, value)) +   
  geom_bar(aes(fill = variable), width = 0.7, 
           position = position_dodge(width=0.7), 
           stat="identity") + theme(legend.position="top",
                                    legend.title = element_blank(),
                                    axis.text.x=element_text(angle=60, hjust=1), 
                                    axis.title.y=element_text())     # Grouped Bar plot by Retion

