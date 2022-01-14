#### The Database!

setwd("")

###Necessary documents
      eggcrop     <-  read.csv("cropped_has_nas.csv") ##Contains all the egg info (this is the NA-Complete dataset)
      clutchinfo  <-  read.csv("clutch_information_nona.csv") ##Information about clutches in the NA-Removed dataset
      clutchinfocrop  <-  read.csv("clutch_information_hasna.csv") ##Information about clutches in the NA-Complete dataset
    
#### Necessary packages  ####
    ##Installation
        install.packages("ggplot2")
        install.packages("lme4")
        install.packages("lmerTest")
        install.packages("MuMIn")
        install.packages("GGally")
        install.packages("dplyr")
        install.packages("interplot")
    
    ##Loading
        library(ggplot2)
        library(lme4)
        library(lmerTest)
        library(MuMIn)
        library(GGally)
        library(dplyr)        
        library(interplot)
    
    ##Citing
        RStudio.Version()               #Gives current RStudio version and citation
        citation()                      #Gives current R version and citation
        citation(package="lme4")
        citation(package="lmerTest")
        citation(package="ggplot2")
        citation(package="MuMIn")
        citation(package="GGally")
        citation(package="dplyr")
        citation(package="interplot")
        

####Removing rows with NAs - this is the NA-Removed dataset ####
      eggnona <- data.frame(eggcrop$EggRank,eggcrop$DaysAfterJan1,eggcrop$YearlingF, eggcrop$YearlingM, 
                            eggcrop$LayingDate,eggcrop$EggVolume,eggcrop$EggLength,
                            eggcrop$EggWidth,eggcrop$FemaleRing,eggcrop$MaleRing,
                            eggcrop$Season,eggcrop$WingLength,eggcrop$ClutchSize,eggcrop$TotalAvgPoints,
                            eggcrop$Last30Temp,eggcrop$Last30Precip,eggcrop$BroodID)
      colnames(eggnona) <- c("EggRank","DaysAfterJan1", "YearlingF", "YearlingM", 
                             "LayingDate", "EggVolume", "EggLength", 
                             "EggWidth", "FemaleRing","MaleRing",
                             "Season","WingLength","ClutchSize","TotalAvgPoints",
                             "Last30Temp","Last30Precip","BroodID")
      eggnona <- na.omit(eggnona)    

#####Changing some variable categories####  
      eggnona$BroodID <- as.character(eggnona$BroodID)          
      eggnona$Season <- as.character(eggnona$Season)
      
      eggcrop$BroodID <- as.character(eggcrop$BroodID)          
      eggcrop$Season <- as.character(eggcrop$Season)
      
      colSums(!is.na(eggcrop)) ##Number of values for each variable in the NA-complete dataset
    
##Boxplots of our variables to see how they look  ####
              
    ##Dependent variables
        par(mfrow=c(1,3))         
            boxplot(eggcrop$EggVolume)
            boxplot(eggcrop$EggWidth)
            boxplot(eggcrop$EggLength)
        par(mfrow=c(1,1))  
        
    ##Independent variables
        par(mfrow=c(2,5))
            boxplot(eggcrop$DaysAfterJan1)
            boxplot(eggcrop$EggRank)
            boxplot(eggcrop$ClutchSize)
            boxplot(eggcrop$WingLength)
            boxplot(eggcrop$TotalAvgPoints)
            boxplot(eggcrop$Last30Temp)
            boxplot(eggcrop$Last30Precip)  ##Some outliers in the tail end but to my knowledge they are all correct measurements, so we keep
        par(mfrow=c(1,1))
        
##Histograms of our variables to see how they look compared to the pre-NA removal ####
    ##I want to be sure we still have a similar sort of distribution
        
    ##Dependent variables ####
        par(mfrow=c(1,2))
            hist(eggcrop$EggVolume, breaks=50)     ##All our valid volume measurements
                hist(eggnona$EggVolume, breaks=25, 
                     main = "Histogram of egg volumes in cm3", xlab = "", ylab="")     ##All the NA-complete volume measurements
            hist(eggcrop$EggWidth, breaks=50)     ##All our valid width measurements
                hist(eggnona$EggWidth, breaks=25, 
                     main = "Histogram of egg widths in mm", xlab = "", ylab="")     ##All the NA-complete width measurements
            hist(eggcrop$EggLength, breaks=50)     ##All our valid volume measurements
                hist(eggnona$EggLength, breaks=25,
                     main = "Histogram of egg lengths in mm", xlab = "", ylab="")     ##All the NA-complete width measurements
        par(mfrow=c(1,1))
        
    ##Independent variables ####   
        par(mfrow=c(2,1))
            hist(eggcrop$DaysAfterJan1, breaks=50)           ##All our valid days since Jan1
                hist(eggnona$DaysAfterJan1, breaks=50)             ##All the NA-complete days since Jan1
            hist(eggcrop$EggRank, breaks=50)           ##All the valid egg ranks
                hist(eggnona$EggRank, breaks=50)             ##All the NA-complete egg ranks
            hist(eggcrop$ClutchSize, breaks=50)              ##All our valid clutch sizes
                hist(eggnona$ClutchSize, breaks=25)                ##All the NA-complete clutch sizes
            hist(eggcrop$WingLength, breaks=50)              ##All our valid wing lengths
                hist(eggnona$WingLength, breaks=50)                ##All the NA-complete wing lengths
            hist(eggcrop$TotalAvgPoints, breaks=50)          ##All our valid average point sizes
                hist(eggnona$TotalAvgPoints, breaks=50)            ##All the NA-complete average point sizes
            hist(eggcrop$Last30Temp, breaks=50)              ##All our valid average temp of last 30 days
                hist(eggnona$Last30Temp, breaks=50)                ##All the NA-complete average temp of last 30 days
            hist(eggcrop$Last30Precip, breaks=50)            ##All our valid total precipitation of last 30 days
                hist(eggnona$Last30Precip, breaks=50)              ##All the NA-complete total precipitation of last 30 days
        par(mfrow=c(1,1))   
        
    ##Only hiccup is maybe Last30Precip, where the head loses more values than expected

##GGPairs to see correlations between variables ####          
      ##Correlations between our independent variables
            update_geom_defaults("point", list(alpha=0.05))     
            corrindep <- data.frame(eggnona$DaysAfterJan1, eggnona$YearlingF, eggnona$YearlingM, eggnona$ClutchSize, eggnona$TotalAvgPoints, eggnona$WingLength, #Owl independent variables
                              eggnona$Last30Precip, eggnona$Last30Temp)  #Environment independent variables        
            ggpairs(corrindep,
                    columnLabels = c("Days after Jan 1", "Yearling female","Yearling male", "Clutch size", "Average point size", "Wing length","Precipitation","Temperature"),
                    upper = list(continuous = "points"),
                    lower = list(continuous = "density"))
            
            tempdaysjan <- lm(Last30Temp ~ DaysAfterJan1, data=eggnona)
            summary(tempdaysjan)
            
            ##NOTE: Very strong correlation between "days after January 1st" and "average temperature of last 30 days" - removing the former in favor of the latter
      
      ##Correlations for all variables
            corrall   <- data.frame(eggnona$EggLength, eggnona$EggWidth, eggnona$EggVolume, #Our dependent variables
                              eggnona$EggRank, eggnona$ClutchSize, eggnona$YearlingF, eggnona$YearlingM, eggnona$TotalAvgPoints, eggnona$WingLength, #Owl independent variables
                              eggnona$Last30Precip, eggnona$Last30Temp) #Environment independent variables
            ggpairs(corrall,  
                    columnLabels = c("Egg length","Egg width","Egg volume",  
                                     "Egg rank", "Clutch size", "Yearling female","Yearling male", "Average point size",  "Wing length",
                                     "Precipitation","Temperature"),
                    upper = list(continuous = "points"),
                    lower = list(continuous = "density"))
            
                       
#### Averages ####
    ##No NAS ####
          ##Mean and SD for egg length
                mean(eggnona$EggLength)
                sd(eggnona$EggLength)
          
          ##Mean and SD for egg width
                mean(eggnona$EggWidth)
                sd(eggnona$EggWidth)
          
          ##Mean and SD for egg volume
                mean(eggnona$EggVolume)
                sd(eggnona$EggVolume)
          
          ##Mean and SD for the number of eggs we have data for
                mean(clutchinfo$EggNumber)
                sd(clutchinfo$EggNumber)
          
          ##Mean and SD for the size of the clutch
                mean(clutchinfo$ClutchSize)
                sd(clutchinfo$ClutchSize)
          
          ##Mean and SD for average number of clutches by a female we have data for
                fclutches <- data.frame(clutchinfo$FemaleRing)
                          fclutches <- fclutches %>% count(clutchinfo.FemaleRing)                  
                          colnames(fclutches) <- c("FemaleRing", "TotalClutchesF")
                          
                mean(fclutches$TotalClutchesF)
                sd(fclutches$TotalClutchesF)
          
          ##Mean and SD for average number of clutches by a male we have data for
                mclutches <- data.frame(clutchinfo$MaleRing)
                          mclutches <- mclutches %>% count(clutchinfo.MaleRing)                  
                          colnames(mclutches) <- c("MaleRing", "TotalClutchesM")
                      
                mean(mclutches$TotalClutchesM)
                sd(mclutches$TotalClutchesM)  
                
    ##Includes NAs ####
          ##Mean and SD for egg length
                mean(eggcrop$EggLength)
                sd(eggcrop$EggLength)
                
          ##Mean and SD for egg width
                mean(eggcrop$EggWidth)
                sd(eggcrop$EggWidth)
                
          ##Mean and SD for egg volume
                mean(eggcrop$EggVolume)
                sd(eggcrop$EggVolume)
                
          ##Mean and SD for the number of eggs we have data for
                mean(clutchinfocrop$EggNumber)
                sd(clutchinfocrop$EggNumber)
                
          ##Mean and SD for the size of the clutch
                mean(clutchinfocrop$ClutchSize)
                sd(clutchinfocrop$ClutchSize)
                
          ##Mean and SD for average number of clutches by a female we have data for
                fclutches <- data.frame(clutchinfocrop$FemaleRing)
                          fclutches <- na.omit(fclutches)
                          fclutches <- fclutches %>% count(clutchinfocrop.FemaleRing)                  
                          colnames(fclutches) <- c("FemaleRing", "TotalClutchesF")
                  
                mean(fclutches$TotalClutchesF)
                sd(fclutches$TotalClutchesF)
                
           ##Mean and SD for average number of clutches by a male we have data for
                mclutches <- data.frame(clutchinfocrop$MaleRing)
                          mclutches <- na.omit(mclutches)
                          mclutches <- mclutches %>% count(clutchinfocrop.MaleRing)                  
                          colnames(mclutches) <- c("MaleRing", "TotalClutchesM")
                
                mean(mclutches$TotalClutchesM)
                sd(mclutches$TotalClutchesM)  
    
#### Models  ####            
    ##Width ####
          ##Model with all variables
                fullwid <- lmer(
                              EggWidth ~ EggRank + YearlingF + YearlingM + ClutchSize 
                              + TotalAvgPoints + WingLength
                              + Last30Precip + Last30Temp 
                              + TotalAvgPoints:Last30Precip + TotalAvgPoints:Last30Temp + TotalAvgPoints:EggRank
                              + (1|FemaleRing) + (1|MaleRing) + (1|BroodID) + (1|Season),
                            data=eggnona)    
                summary(fullwid)    
                step(fullwid)  
          
          ##Simplified model
                stepwid <- lmer(
                              EggWidth ~ Last30Temp
                              + (1|FemaleRing) + (1|MaleRing) + (1|BroodID) + (1|Season),
                            data=eggnona) 
                summary(stepwid)
                
          AIC(stepwid, fullwid)      
          
          ##Simplified model on NA-complete dataset (to compare the effects seen)
                comparwid <- lmer(
                            EggWidth ~ Last30Temp
                            + (1|FemaleRing) + (1|MaleRing) + (1|BroodID) + (1|Season),
                            data=eggcrop) 
                summary(comparwid)
          
              r.squaredGLMM(fullwid)
              r.squaredGLMM(stepwid)
              r.squaredGLMM(comparwid)
              
                  
    ##Length ####
          ##Model with all variables
                fulllen <- lmer(
                              EggLength ~ EggRank + YearlingF + YearlingM + ClutchSize 
                              + TotalAvgPoints + WingLength
                              + Last30Precip + Last30Temp 
                              + TotalAvgPoints:Last30Precip + TotalAvgPoints:Last30Temp + TotalAvgPoints:EggRank
                              + (1|FemaleRing) + (1|MaleRing) + (1|BroodID) + (1|Season),
                          data=eggnona)    
                summary(fulllen)   
                step(fulllen)  
          
          ##Simplified model           
                steplen <- lmer(
                            EggLength ~ EggRank + Last30Precip
                            + (1|FemaleRing) + (1|MaleRing) + (1|BroodID) + (1|Season),
                        data=eggnona)  
                summary(steplen)   
          
          AIC(fulllen, steplen) 
                
          ##Simplified model on NA-complete dataset (to compare the effects seen)
                comparlen <- lmer(
                                EggLength ~ EggRank + Last30Precip
                                + (1|FemaleRing) + (1|MaleRing) + (1|BroodID) + (1|Season),
                        data=eggcrop)
                summary(comparlen)
          
              r.squaredGLMM(fulllen)
              r.squaredGLMM(steplen)
              r.squaredGLMM(comparlen)
                
    ##Volume ####
          ##Model with all variables
                fullvol <- lmer(
                            EggVolume ~ EggRank + YearlingF + YearlingM + ClutchSize 
                            + TotalAvgPoints + WingLength
                            + Last30Precip + Last30Temp 
                            + TotalAvgPoints:Last30Precip + TotalAvgPoints:Last30Temp + TotalAvgPoints:EggRank
                            + (1|FemaleRing) + (1|MaleRing) + (1|BroodID) + (1|Season),
                        data=eggnona)    
                summary(fullvol)   
                step(fullvol) 
          
          ##Simplified model
                stepvol <- lmer(
                            EggVolume ~ EggRank
                            + (1|FemaleRing) + (1|MaleRing) + (1|BroodID) + (1|Season), 
                        data=eggnona)
                summary(stepvol)    
          
          AIC(fullvol, stepvol) 
                
          ##Simplified model on NA-complete dataset (to compare the effects seen)
                comparvol <- lmer(
                              EggVolume ~ EggRank
                              + (1|FemaleRing) + (1|MaleRing) + (1|BroodID) + (1|Season), 
                          data=eggcrop)
                summary(comparvol)
          
              r.squaredGLMM(fullvol)
              r.squaredGLMM(stepvol)
              r.squaredGLMM(comparvol)
            
              
#### Graphs ####
    ## Egg Width ####
          #Egg width plotted against average temperature of last 30 days
          
          ##NO NAS DATASET
                ggplot(eggnona, aes(x=Last30Temp,y=EggWidth)) + 
                      geom_point(size = 5, alpha = 0.4, color="orange") +
                      ylab("Egg width (mm)") + xlab("Average temperature of last 30 days (°C)") +
                      theme(panel.background = element_rect(fill = 'gray43')) +
                      xlim(1,21) +  scale_y_continuous(limits = c(27,34), breaks = seq(26, 34, by = 1)) + 
                      geom_abline(slope = 2.434e-02, intercept = 30.35, size=6, alpha=0.4, color="saddlebrown")  +
                      geom_abline(slope = 2.434e-02, intercept = 30.35, size=2, alpha=0.8, color="saddlebrown") 
          
          ##CROPPED DATASET
                ggplot(eggcrop, aes(x=Last30Temp,y=EggWidth)) + 
                      geom_point(size = 5, alpha = 0.4, color="orange") +
                      ylab("Egg width (mm)") + xlab("Average temperature of last 30 days (°C)") +
                      theme(panel.background = element_rect(fill = 'gray43')) +
                      xlim(1,21) +  scale_y_continuous(limits = c(27,34), breaks = seq(26, 34, by = 1)) + 
                      geom_abline(slope = -0.00182, intercept = 30.61, size=6, alpha=0.4, color="saddlebrown")  +
                      geom_abline(slope = -0.00182, intercept = 30.61, size=2, alpha=0.8, color="saddlebrown")      
         
                
    ## Egg Length ####
          #Egg length plotted against egg rank, and egg length plotted against total precipitation of the last 30 days 
                
          ##NO NAS DATASET
                ggplot(eggnona, aes(x=EggRank,y=EggLength)) + 
                      geom_point(size = 5, alpha = 0.4, color="darkolivegreen2") +
                      ylab("Egg length (mm)") + xlab("Laying rank of egg") +
                      scale_y_continuous(breaks = seq(35, 45, by = 2)) +
                      theme(panel.background = element_rect(fill = 'gray43')) +
                      geom_abline(slope = 0.040 , intercept = 39.56, size=6, alpha=0.4, color="forestgreen")  +
                      geom_abline(slope = 0.040 , intercept = 39.56, size=2, alpha=1, color="forestgreen") 
                
                ggplot(eggnona, aes(x=Last30Precip,y=EggLength)) + 
                      geom_point(size = 5, alpha = 0.4, color="lightblue") +
                      ylab("Egg length (mm)") + xlab("Total precipitation of last 30 days (mm)") +
                      scale_y_continuous(breaks = seq(35, 45, by = 2)) +
                      theme(panel.background = element_rect(fill = 'gray43')) +
                      geom_abline(slope = -0.004 , intercept = 39.56, size=7, alpha=0.4, color="dodgerblue")  +
                      geom_abline(slope = -0.004 , intercept = 39.56, size=2, alpha=0.8, color="dodgerblue") 
                
                
          ##NA COMPLETE DATASET
                ggplot(eggcrop, aes(x=EggRank,y=EggLength)) + 
                      geom_point(size = 5, alpha = 0.4, color="darkolivegreen2") +
                      ylab("Egg length (mm)") + xlab("Laying rank of egg") +
                      theme(panel.background = element_rect(fill = 'gray43')) +
                      geom_abline(slope = 0.021, intercept = 39.71 , size=15, alpha=0.4, color="forestgreen")  +
                      geom_abline(slope = 0.021, intercept = 39.71 , size=2, alpha=0.8, color="forestgreen") 
                
                ggplot(eggcrop, aes(x=Last30Precip,y=EggLength)) + 
                      geom_point(size = 5, alpha = 0.4, color="lightblue") +
                      ylab("Egg length (mm)") + xlab("Total precipitation of last 30 days (mm)") +
                      theme(panel.background = element_rect(fill = 'gray43')) +
                      geom_abline(slope = -0.005, intercept = 39.71 , size=15, alpha=0.4, color="dodgerblue")  +
                      geom_abline(slope = -0.005 , intercept = 39.71 , size=2, alpha=0.8, color="dodgerblue")            
                 
                
    ## Egg Volume ####
          #Egg volume plotted against egg rank
          
          ##NO NAS DATASET
              ggplot(eggnona,aes(x=EggRank,y=EggVolume)) + 
                      geom_point(size = 5, alpha = 0.3, color="hotpink1") +
                      ylab("Egg volume (cm3)") + xlab("Laying rank of egg") +
                      theme(panel.background = element_rect(fill = 'gray43')) +
                      geom_abline(slope = 0.027, intercept = 19.26, size=5.5, alpha=0.3, color="white") +
                      geom_abline(slope = 0.027, intercept = 19.26, size=2, alpha=0.8, color="grey25") +
                      xlim(0,11.7) + ylim(14,25)
            
          ##CROPPED DATASET
              ggplot(eggcrop,aes(x=EggRank,y=EggVolume)) + 
                      geom_point(size = 5, alpha = 0.3, color="hotpink1") +
                      ylab("Egg volume (cm3)") + xlab("Laying rank of egg") +
                      theme(panel.background = element_rect(fill = 'gray43')) +
                      geom_abline(slope = 0.019, intercept = 19.33, size=5.5, alpha=0.3, color="white") +
                      geom_abline(slope = 0.019, intercept = 19.33, size=2, alpha=0.8, color="grey25") +
                      xlim(0,11.7) + ylim(14,25)
              
              