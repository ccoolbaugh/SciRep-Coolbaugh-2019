Extended Duration Cold Exposure Reproducible Analysis
================
Crystal Coolbaugh
2019-07-12

-   [Reproducible Analysis](#reproducible-analysis)
-   [What is R Markdown?](#what-is-r-markdown)
-   [Getting Started](#getting-started)
-   [Tidy Data](#tidy-data)
-   [Setup Requirements](#setup-requirements)
-   [Custom Functions](#custom-functions)
    -   [Mean Difference for Multiple Variables](#mean-difference-for-multiple-variables)
    -   [Bootstrap Confidence Intervals for Difference in Means](#bootstrap-confidence-intervals-for-difference-in-means)
    -   [Wilcox Signed Rank Test for Multiple Variables](#wilcox-test-for-multiple-variables)
-   [Figure Parameters](#figure-parameters)
-   [Table 2: Subject Demographics and Environmental Conditions Summary](#table-2-subject-demographics-and-environmental-conditions-summary)
-   [Figure 3: Cooling Dose Concept](#figure-3-cooling-dose-concept)
-   [MRI Data Analysis](#mri-data-analysis)
    -   [Import and Filter MRI Data](#import-and-filter-mri-data)
    -   [Calculate the Difference in Fat-Signal Fraction between Thermoneutral and Cold Exposure Conditions](#calculate-the-difference-in-fat-signal-fraction-between-thermoneutral-and-cold-exposure-conditions)
-   [Figure 4: Effect of Cold Exposure on Mean Fat-Signal Fraction in each Tissue ROI](#figure-4-effect-of-cold-exposure-on-mean-fat-signal-fraction-in-each-tissue-roi)
-   [Figure 5: Effect of Cold Exposure on Mean Fat-Signal Fraction in each Tissue Decade](#figure-5-effect-of-cold-exposure-on-mean-fat-signal-fraction-in-each-tissue-decade)
-   [Figure 6: Cold Exposure Dynamically Alters Mean Fat-Signal Fraction in Brown Adipose Tissue](#figure-6-cold-exposure-dynamically-alters-mean-fat-signal-fraction-in-brown-adipose-tissue)
    -   [Spearman Rank Correlation: Mean FSF vs. Normalized Cooling Dose](#spearman-rank-correlation-mean-fsf-vs-normalized-cooling-dose)
-   [Figure 7: Mean Fat-Signal Fraction Response Tracks with Thermal Sensation](#figure-7-mean-fat-signal-fraction-response-tracks-with-thermal-sensation)
    -   [Spearman Rank Correlation: Mean FSF vs. Thermal Sensation](#spearman-rank-correlation-mean-fsf-vs-thermal-sensation)
-   [Figure S1: Simulated Effect of Temperature on Fat-Signal Fraction Bias](#figure-s1-computer-simulation-of-the-effect-of-temperature-on-fsf-bias)  
    

## Reproducible Analysis
We created a R Markdown document to share how data were analyzed and plotted in our _Scientific Reports_ article: "Cold Exposure Induces Dynamic, Heterogeneous Alterations in Human Brown Adipose Tissue Lipid Content."  

## What is R Markdown?
R Markdown documents are a tool to format text, code, and output in markdown language. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

## Getting Started
To get started, you must have **R** and **RStudio** installed on your computer. You can download each of these programs using the following links: 

* [The R Project for Statistical Computing](https://www.r-project.org/ "R Homepage")
* [R Studio](https://www.rstudio.com/products/rstudio/download/ "RStudio Download")

We used R version 3.3.3 (2017-03-06) and RStudio version 1.1.463 for Mac OS to create this code. 

For clarity, some of the code used to generate tables and figures are not displayed in this file. To view the complete R code, **download** and **open** the R markdown file (.Rmd) in R studio.  

## Tidy Data
All data pertaining to the statistical analyses and figures presented in the manuscript are stored in the **/tidy_data folder** in this repository. Please refer to the **data dictionary** included in the tidy data folder to learn more about the contents of each file. 

## Setup Requirements
Statistical analyses included in this R markdown file require specific **R** libraries. These libraries must be installed on your computer prior to running the code. 

If you have not used R previously, run the following code in your RStudio console to install the necessary packages prior to executing the R markdown file. 

  
``` r  
install.packages("tidyverse")
install.packages("stringr")
install.packages("Hmisc")
install.packages("scales")
install.packages("cowplot")
install.packages("knitr")
```


Once the **R** libraries are installed on your computer, the following code initializes the package for use in the subsequent analyses.

  
``` r  
library(tidyverse)  #tidy data and plots
library(stringr)    #string operations
library(Hmisc)      #Harrell miscellaneous
library(cowplot)    #plot format tools 
library(scales)     #scale functions for visualization
library(knitr)      #r markdown format tools
#library(gdtools)    #utilities for graphical rendering - uncomment if saving transparency to vector format (e.g. eps or svg)
#library(svglite)    #an svg graphics device - uncomment if saving transparency to vector format (e.g. eps or svg)
```

## Custom Functions
### Mean Difference for Multiple Variables
Calculate difference in the thermoneutral and cold exposure means. Due to the formatting of the data frame, the inverse of the difference is used to reflect the change as thermoneutral - cold exposure. The function returns the value in a format that can be merged with the existing data frame structure.

  
``` r  
mdfun <- function(outcome, group) {
  meandif <- -diff(tapply(outcome, group, mean, na.rm=TRUE))
  meandif <- round(meandif[1],2)
}
```

### Bootstrap Confidence Intervals for Difference in Means
The `bootdiff` functions were used to calculate the confidence interval for the difference in the thermoneutral and cold exposure means. Individual functions return the lower and upper confidence limits to add to the total data frame. 

Thank you to Jennifer Thompson, MPH for writing this function. Please refer to her excellent write up for more details:  <http://biostat.mc.vanderbilt.edu/wiki/pub/Main/JenniferThompson/ms_mtg_18oct07.pdf>.
  
    
``` r  

bootdiffL <- function(outcome, group) {
 
 #Use the smean.cl.boot function to bootstrap means for the variable y
 #for each treatment a and b; this code uses 1000 samples
 a <- attr(smean.cl.boot(outcome[group==levels(group)[1]], B=1000, reps=TRUE, na.rm=TRUE),'reps')
 b <- attr(smean.cl.boot(outcome[group==levels(group)[2]], B=1000, reps=TRUE, na.rm=TRUE),'reps')

 #Calculate the 2.5 and 97.5 percentiles of the differences in bootstrapped means
 a.b <- quantile(b-a, c(.025,.975), na.rm=TRUE)
 
#Make results more print friendly
 P_2.5 <- -round(a.b[[1]],2)
 
 #Return only numeric value of 2.5 percentile
 return(P_2.5)

}


  
bootdiffH <- function(outcome, group) {
 
 #Use the smean.cl.boot function to bootstrap means for the variable y
 #for each treatment a and b; this code uses 1000 samples
 a <- attr(smean.cl.boot(outcome[group==levels(group)[1]], B=1000, reps=TRUE, na.rm=TRUE),'reps')
 b <- attr(smean.cl.boot(outcome[group==levels(group)[2]], B=1000, reps=TRUE, na.rm=TRUE),'reps')

 #Calculate the 2.5 and 97.5 percentiles of the differences in bootstrapped means
 a.b <- quantile(b-a, c(.025,.975), na.rm=TRUE)
 
 #Make results more print friendly
 P_97.5 <- -round(a.b[[2]],2)
 
 #Return only numeric value of 2.5 percentile
 return(P_97.5)

}
```  

### Wilcox Test for Multiple Variables
Perform paired wilcoxon signed rank test on difference of means. Function returns the test p-value in a format that can be merged with the existing data frame structure.  
  
    
``` r
wilcoxfun <- function(outcome, group) {
  wilcox.test(outcome ~ group, paired=TRUE, alternative = "two.sided", na.rm=TRUE)$p.value
}
```  
  
## Figure Parameters
Specify the format, labels, and color settings for printing figures.  
  
    
``` r
#Plot Theme
theme_set(theme_classic(base_size = 14)) 

# Decade Colors Palette
TabPal<-c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD","#e377c2","#BCBD22", "#7f7f7f", "#8c564b", "#17becf")

#Labels for Decades - Short and Long Format
DecLabShort<-c("0%","10%","20%","30%","40%","50%","60%","70%","80%","90%")
DecLab<-c("Decade: 0-10%","Decade: 10-20%","Decade: 20-30%","Decade: 30-40%","Decade: 40-50%","Decade: 50-60%","Decade: 60-70%","Decade: 70-80%","Decade: 80-90%","Decade: 90-100%")

#Decade Names for Facet Plots
names(DecLab)<-c("BAT0","BAT10","BAT20","BAT30","BAT40","BAT50","BAT60","BAT70","BAT80","BAT90")

```

## Table 2: Subject Demographics and Environmental Conditions Summary
Univariate statistics (e.g. mean, standard deviation, min, and max) were used to summarize subject demographics and environmental test conditions. Summary variables are labelled with the corresponding statistic in the displayed table.  
  
    
``` r
# Import Table 2 Tidy Data
Tab2.Data<-read.csv(file=file.path("tidy_data","data_tab2_subject_demographics.csv"),header=TRUE)

```
  
    
``` r
# Summarize Mean, Standard Deviation, Min, Max
Demo.Sum<- Tab2.Data %>% dplyr::select(Age:MRICDNorm) %>%
  mutate_all(funs(Mean = mean, SD = sd, Min = min, Max = max), na.rm=TRUE)

#Display Table
Demo.Sum

# Outdoor Conditions
OutT<-wilcox.test(Tab2.Data$PCP_Out_t,Tab2.Data$Out._t, paired=TRUE, alternative = "two.sided", na.rm=TRUE)$p.value
OutH<-wilcox.test(Tab2.Data$PCP_Out_h,Tab2.Data$Out._h, paired=TRUE, alternative = "two.sided", na.rm=TRUE)$p.value

# Indoor Conditions
InT<-wilcox.test(Tab2.Data$PCP_In_t,Tab2.Data$In_t, paired=TRUE, alternative = "two.sided", na.rm=TRUE)$p.value
InH<-wilcox.test(Tab2.Data$PCP_In_h,Tab2.Data$In_h, paired=TRUE, alternative = "two.sided", na.rm=TRUE)$p.value

# Cooling Dose
CoolDose<-wilcox.test(Tab2.Data$PCPCoolDose,Tab2.Data$MRICoolDose, paired=TRUE, alternative = "two.sided", na.rm=TRUE)$p.value
NCoolDose<-wilcox.test(Tab2.Data$PCPCDNorm,Tab2.Data$MRICDNorm, paired=TRUE, alternative = "two.sided", na.rm=TRUE)$p.value

```
  
  
## Figure 3: Cooling Dose Concept
Conceptual model illustrating the calculation of normalized cooling dose. Simulated water temperature (°C) vs. time (min) plots comparing two protocols that begin and end at the same temperatures but follow linear or step cooling gradient profiles (**A**). To calculate normalized cooling dose, the following steps are completed: (1) relative water temperatures (°C) are expressed as the change (Δ) in temperature from the starting or thermoneutral temperature (**B**), (2) cooling dose (°C\*min) is calculated from the area under the relative water temperature curve (**C**), and (3) normalized cooling dose (°C\*min\*m^-2) is found by dividing cooling dose by body surface area to account for differences in participant body size (**D**).   

  
``` r
# Import Simulated Cooling Dose Data 
CoolDose.Data<-read.csv(file=file.path("tidy_data","data_fig3_cooldose_simulation.csv"),header=TRUE)

# Temperature Data
Temp.Data<-CoolDose.Data %>% select(Duration, Temp1, Temp2)
Temp.Data<-Temp.Data %>% gather(Profile,Temperature,Temp1:Temp2)

# Relative Temperature Data
RelTemp.Data<-CoolDose.Data %>% select(Duration, Temp1Rel, Temp2Rel)
RelTemp.Data<-RelTemp.Data %>% gather(Profile,RelTemp,Temp1Rel:Temp2Rel)

# CoolDose Data
CD.Data<-CoolDose.Data %>% select(Duration, CD1, CD2)
CD.Data<-CD.Data %>% gather(Profile,CoolDose,CD1:CD2)

# CoolDose Data
CDNorm.Data<-CoolDose.Data %>% select(Duration, CD1Norm, CD2Norm)
CDNorm.Data<-CDNorm.Data %>% gather(Profile,CoolDoseNorm,CD1Norm:CD2Norm)

# Fig 3a - Water Temperature Profiles
Fig3a<-Temp.Data %>% ggplot(aes(x=Duration, y=Temperature, linetype=Profile)) +
  geom_line(size=1.2) + 
  scale_linetype_manual(values=c("solid", "dotted"),labels=c("Linear","Step")) +
  scale_x_continuous(breaks=seq(0,60,10)) +
  scale_y_continuous(limits=c(10,32), breaks=c(10,12,15,18,22,27,32)) +
  labs(caption="",
       x = "Time (min)", y=expression(paste("Water Temperature (",degree,"C)")))+
  theme(legend.title=element_blank(),legend.position=c(0.8, 0.7))

# Fig 3b - Relative Water Temperature Profiles
Fig3b<-RelTemp.Data %>% ggplot(aes(x=Duration, y=RelTemp, linetype=Profile)) +
  geom_line(size=1.2) + 
  scale_linetype_manual(values=c("solid", "dotted"),labels=c("Linear","Step")) +
  scale_x_continuous(breaks=seq(0,60,10)) +
  scale_y_continuous(limits=c(0,22), breaks=c(0,5,10,14,17,20,22)) +
  labs(caption=expression(paste(Delta,"= Thermoneutral - Temperature")),
       x = "Time (min)", y=expression(paste(Delta, " Water Temperature (",degree,"C)")))+
  theme(legend.title=element_blank(),legend.position=c(0.8, 0.3))

# Fig 3c - Cooling Dose Profile
Fig3c<-CD.Data %>% ggplot(aes(x=Duration, y=CoolDose, linetype=Profile)) +
  geom_line(size=1.2) + 
  scale_linetype_manual(values=c("solid", "dotted"),labels=c("Linear","Step")) +
  scale_x_continuous(breaks=seq(0,60,10)) +
  scale_y_continuous(limits=c(0,900), breaks=c(0,660,869)) +
  labs(caption="",
       x ="Time (min)", y=expression(paste("Cooling Dose (",degree,"C*min)")))+
  theme(legend.title=element_blank(),legend.position=c(0.15, 0.7))

# Fig 3d - Normalized Cooling Dose Profile
Fig3d<-CDNorm.Data %>% ggplot(aes(x=Duration, y=CoolDoseNorm, linetype=Profile)) +
  geom_line(size=1.2) + 
  scale_linetype_manual(values=c("solid", "dotted"),labels=c("Linear (412.5)","Step (413.8)")) +
  scale_x_continuous(breaks=seq(0,60,10)) +
  scale_y_continuous(limits=c(0,450), breaks=c(0,413)) +
  labs(caption=expression("Linear Body Surface Area=1.6"~m^2~"; Step Body Surface Area=2.1"~m^2),
       x ="Time (min)", y=expression(paste("Normalized Cooling Dose (",degree,"C*min*m"^"-2",")"))) +
  theme(legend.title=element_blank(),legend.position=c(0.15, 0.7))

# Grid Plots
# align left side
plot3l<-align_plots(Fig3a,Fig3c,align='v',axis='l')
# align right side
plot3r<-align_plots(Fig3b,Fig3d,align='v',axis='l')
# create top row
top_row3<-plot_grid(plot3l[[1]],plot3r[[1]],labels=c("A","B"),ncol=2,align='h')
# create bottom row
bot_row3<-plot_grid(plot3l[[2]],plot3r[[2]],labels=c("C","D"),ncol=2,align='h')
# combine top and bottom rows
Fig3<-plot_grid(top_row3,bot_row3,nrow=2)

# Display Figure
Fig3

# Save Plot - uncomment to save plot as a tiff file
#save_plot("Fig3.tiff", Fig3, base_width=15, base_aspect_ratio = 1.618, base_height=NULL)

```
  
  
## MRI Data Analysis
Analyses were performed on the fat-water MRI data to determine the effect of cold exposure on mean fat-signal fraction values in the different tissues of interest (e.g. muscle, brown adipose tissue, and subcutaneous adipose tissue). Voxels were analyzed according to their position in the manually segmented region of interest (ROI) and according to their initial fat-signal fraction value. Data within the brown adipose tissue ROI were analyzed using all possible fat-signal fraction values (BAT) and with different thresholds applied (BAT40t = 40-100%; BAT50t = 50-100%).  
  
### Import and Filter MRI Data
Import fat-water MRI data and separate into ROI and Decade data frames. Identify the data points associated with the thermoneutral ("TN") condition (i.e. the initial image acquisition) and the end of the cooling protocol ("CE") using the final image acquisition achieved in all seven participants.  

  
``` r
# Import MRI Data
MRI.Data<-read.csv(file=file.path("tidy_data","data_fig4-7_mri_results.csv"),header=TRUE)

#Trim Data 
#exclude: BSA,Set Temperature, Relative Temperature, Median, SD, SE, CI calculations
Trim.Data<-MRI.Data %>% dplyr::select(SID,nScan,Time,Water:Mean) 

#Convert Mean Fat-Signal Fractions to Percentages
Trim.Data$Mean<-Trim.Data$Mean*100

#BAT Decade Data Only
BATDec.Data<-Trim.Data %>% dplyr::filter(stringr::str_detect(ROIName,"^BAT")) %>% dplyr::filter(stringr::str_detect(ROIName,"0$"))

#WAT Decade Data Only
WATDec.Data<-Trim.Data %>% dplyr::filter(stringr::str_detect(ROIName,"^WAT")) %>% dplyr::filter(stringr::str_detect(ROIName,"0$"))

#MUS Decade Data Only
MUSDec.Data<-Trim.Data %>% dplyr::filter(stringr::str_detect(ROIName,"^MUS")) %>% dplyr::filter(stringr::str_detect(ROIName,"0$"))

#BAT, BAT_40t, BAT_50t, WAT, MUS Data Only
ROI.Data<-Trim.Data %>% dplyr::filter(stringr::str_detect(ROIName,"^BAT$")|stringr::str_detect(ROIName,"t$")|stringr::str_detect(ROIName,"^WAT$")|stringr::str_detect(ROIName,"^MUS$")) 

#Thermoneutral & Cold Exposure Data
#BAT Decade Data - Replace Scan # with TN or CE Flag
TnCeDec<-BATDec.Data %>% dplyr::filter(nScan==4|nScan==24) %>% mutate(nScan=factor(ifelse(nScan == 4,"TN","CE")))

#WATDecade Data - Replace Scan # with TN or CE Flag
TnCeWAT<-WATDec.Data %>% dplyr::filter(nScan==4|nScan==24) %>% mutate(nScan=factor(ifelse(nScan == 4,"TN","CE")))

#MUSDecade Data - Replace Scan # with TN or CE Flag
TnCeMUS<-MUSDec.Data %>% dplyr::filter(nScan==4|nScan==24) %>% mutate(nScan=factor(ifelse(nScan == 4,"TN","CE")))

#ROI Data
TnCeRoi<-ROI.Data %>% dplyr::filter(nScan==4|nScan==24) %>% mutate(nScan=factor(ifelse(nScan == 4,"TN","CE")))

#Thermoneutral Data Only
#BAT Decade Data 
TnDec<-TnCeDec %>% dplyr::filter(nScan=="TN")

#WATDecade Data 
TnWAT<-TnCeWAT %>% dplyr::filter(nScan=="TN")

#MUSDecade Data 
TnMUS<-TnCeMUS %>% dplyr::filter(nScan=="TN")

#ROI Data
TnRoi<-TnCeRoi %>% dplyr::filter(nScan=="TN")

```  


  
### Calculate the Difference in Fat-Signal Fraction between Thermoneutral and Cold Exposure Conditions
Univariate statistics (mean and standard deviation) were used to summarize the TN data. Nonparametric Wilcoxon tests were used to determine whether mean fat-signal fraction values differed between the TN and CE conditions. For a better description of the data, we also calculated the difference in means (TN - CE) as a descriptive statistic and the bootstrapped 95% confidence interval (1000 samples).  
  
    
``` r
#Summarize Thermoneutral Data
## Mean & Standard Deviation 
# BAT Decades
TnDec <- TnDec %>% group_by(ROIName) %>% mutate(DecMean=round(mean(Mean),3), DecSD=round(sd(Mean),3))

# WAT Decades
TnWAT <- TnWAT %>% group_by(ROIName) %>% mutate(WatMean=round(mean(Mean),3), WatSD=round(sd(Mean),3))

# MUS Decades
TnMUS <- TnMUS %>% group_by(ROIName) %>% mutate(MusMean=round(mean(Mean),3), MusSD=round(sd(Mean),3))

# Total ROI
TnRoi <- TnRoi %>% group_by(ROIName) %>% mutate(RoiMean=round(mean(Mean),3), RoiSD=round(sd(Mean),3))

# Compare BAT & SAT at Thermoneutral
TnRoiBat<-TnRoi %>% filter(ROIName=="BAT") 
TnRoiWat<-TnRoi %>% filter(ROIName=="WAT") 
TnRoip<-wilcox.test(TnRoiBat$Mean,TnRoiWat$Mean, paired=TRUE, alternative = "two.sided", na.rm=TRUE)$p.value

## Mean Difference - TN and CE
# BAT Decades
TnCeDec <- TnCeDec %>% group_by(ROIName) %>% mutate(MeanDiff=mdfun(Mean,nScan))

# WAT Decades
TnCeWAT <- TnCeWAT %>% group_by(ROIName) %>% mutate(MeanDiff=mdfun(Mean,nScan))

# MUS Decades
TnCeMUS <- TnCeMUS %>% group_by(ROIName) %>% mutate(MeanDiff=mdfun(Mean,nScan))

# ROI Data
TnCeRoi <- TnCeRoi %>% group_by(ROIName) %>% mutate(MeanDiff=mdfun(Mean,nScan))

## Bootstrap Confidence Intervals for Differences in Means
#Set Random Number Generator - replicate randomization of bootstrap values
set.seed(1)

#BAT Decades
TnCeDec <- TnCeDec %>% group_by(ROIName) %>% mutate(P_2.5=bootdiffL(Mean,nScan))
TnCeDec <- TnCeDec %>% group_by(ROIName) %>% mutate(P_97.5=bootdiffH(Mean,nScan))

#WAT Decades
TnCeWAT <- TnCeWAT %>% group_by(ROIName) %>% mutate(P_2.5=bootdiffL(Mean,nScan))
TnCeWAT <- TnCeWAT %>% group_by(ROIName) %>% mutate(P_97.5=bootdiffH(Mean,nScan))

#MUS Decades
TnCeMUS <- TnCeMUS %>% group_by(ROIName) %>% mutate(P_2.5=bootdiffL(Mean,nScan))
TnCeMUS <- TnCeMUS %>% group_by(ROIName) %>% mutate(P_97.5=bootdiffH(Mean,nScan))

#ROI Decades
TnCeRoi <- TnCeRoi %>% group_by(ROIName) %>% mutate(P_2.5=bootdiffL(Mean,nScan))
TnCeRoi <- TnCeRoi %>% group_by(ROIName) %>% mutate(P_97.5=bootdiffH(Mean,nScan))

## Wilcoxon Signed Rank Test - Difference in Means between TN and CE
# BAT Decades
TnCeDec <- TnCeDec %>% group_by(ROIName) %>% mutate(Pvalue=wilcoxfun(Mean,nScan))

# WAT Decades
TnCeWAT <- TnCeWAT %>% group_by(ROIName) %>% filter(ROIName=="WAT70" | ROIName=="WAT80" | ROIName=="WAT90") %>% mutate(Pvalue=wilcoxfun(Mean,nScan))

# MUS Decades
TnCeMUS <- TnCeMUS %>% group_by(ROIName) %>% filter(ROIName=="MUS0" | ROIName=="MUS10") %>% mutate(Pvalue=wilcoxfun(Mean,nScan))

# ROI Data
TnCeRoi <- TnCeRoi %>% group_by(ROIName) %>% mutate(Pvalue=wilcoxfun(Mean,nScan))

# Format BAT Decade Results into Table
TnCe.boot <- data.frame(Decade=DecLabShort, Mean_SD = c(paste(TnDec$DecMean[1:10]," (",TnDec$DecSD[1:10],")",sep="")),
                        MeanDiff_CI = c(paste(TnCeDec$MeanDiff[1:10], " (",
                                              TnCeDec$P_2.5[1:10], ", ", TnCeDec$P_97.5[1:10], ")", sep="")))
TnCe.boot

# Format WAT Decade Results into Table
TnCeW.boot <- data.frame(Decade=DecLabShort[8:10], Mean_SD = c(paste(TnWAT$WatMean[8:10]," (",TnWAT$WatSD[8:10],")",sep="")),
                         MeanDiff_CI = c(paste(TnCeWAT$MeanDiff[1:3], " (",
                                                        TnCeWAT$P_2.5[1:3], ", ", TnCeWAT$P_97.5[1:3], ")", sep="")))
TnCeW.boot

# Format MUS Decade Results into Table
TnCeM.boot <- data.frame(Decade=DecLabShort[1:2], Mean_SD = c(paste(TnMUS$MusMean[1:2]," (",TnMUS$MusSD[1:2],")",sep="")),
                         MeanDiff_CI = c(paste(TnCeMUS$MeanDiff[1:2], " (",
                                                        TnCeMUS$P_2.5[1:2], ", ", TnCeMUS$P_97.5[1:2], ")", sep="")))
TnCeM.boot

# Format ROI Results into Table
TnCeRoi.boot <- data.frame(ROI=c("BAT","WAT","MUS","BAT40t","BAT50t"), Mean_SD = c(paste(TnRoi$RoiMean[1:5]," (",TnRoi$RoiSD[1:5],")",sep="")),MeanDiff_CI = c(paste(TnCeRoi$MeanDiff[1:5], " (",TnCeRoi$P_2.5[1:5], ", ", TnCeRoi$P_97.5[1:5], ")", sep="")))

TnCeRoi.boot

```
  


## Figure 4: Effect of Cold Exposure on Mean Fat-Signal Fraction in each Tissue ROI  
Comparison of thermoneutral and cold exposure mean fat-signal fractions (%) for muscle (MUS), brown adipose tissue (BAT), and subcutaneous adipose tissue (SAT). Mean fat-signal fractions for BAT were calculated using three threshold conditions: 0-100%, 40-100%, and 50-100%. The center line in each box indicates the mean, and the top and bottom of the box show the 95% bootstrapped confidence intervals (1000 samples). Wilcoxon signed rank tests were performed to assess statistical comparisons: *_P_<0.05.  

  
``` r
#Bootstrap Parameters (CI=0.95,B=1000,na.rm=True,reps=False)

#Change in Mean FSF TN to CE - All Voxels in each ROI: Muscle, Brown Adipose Tissue, Subcutaneous Adipose Tissue
#FSF displayed as a percentage (%)
Fig4<-TnCeRoi %>% ggplot(aes(x=ROIName,y=Mean,color=nScan,fill=nScan)) +
  stat_summary(fun.data="mean_cl_boot",na.rm=TRUE,geom="crossbar",width=0.4, alpha=0.4,
               position=position_dodge(width=-0.5)) +
  annotate("text",x=c(1,3,4),y=c(13,77,80), label = '*', size=8) +
  scale_x_discrete(limits=c("MUS","BAT","BAT40t","BAT50t","WAT"),labels=c("MUS","BAT\n (0-100%)","BAT\n (\u2265 40%)","BAT\n (\u2265 50%)","SAT")) +
  scale_y_continuous(limits=c(0,100), breaks=c(0,20,40,60,80,100)) +
  scale_color_manual(values=c("#1F77B4", "#D62728"),breaks=c("TN","CE"),guide=FALSE) +
  scale_fill_manual(values=c("#1F77B4", "#D62728"),breaks=c("TN","CE"),labels=c("Thermoneutral","Cold Exposure")) +
  labs(x="Region of Interest",y="Mean FSF (%)") +
  background_grid(major="y") +
  theme(legend.title=element_blank(), legend.position =c(0.8,0.25))


# Display Plot
Fig4

# Save Plot - uncomment to save plot as a tiff file
#save_plot("Fig4.tiff", Fig4, base_width=6, base_aspect_ratio = 1.618, base_height=NULL)
```
  
  
## Figure 5: Effect of Cold Exposure on Mean Fat-Signal Fraction in each Tissue Decade
Comparison of thermoneutral and cold exposure mean fat-signal fractions (%) for decade group assignments in brown adipose tissue (BAT;**A**), muscle (MUS; **B**), and subcutaneous adipose tissue (SAT;**C**). Image voxels included in each tissue region of interest were assigned to a decade group according to the voxel's initial fat-signal fraction value (i.e. thermoneutral). For example, a fat-signal fraction value of 33% was assigned to the 30% decade group. A minimum of 60 voxels were required for the decade group to be included in the summary analysis. The center line in each box indicates the mean, and the top and bottom of the box show the 95% bootstrapped confidence intervals (1000 samples). Wilcoxon signed rank tests were performed to assess statistical comparisons: *_P_<0.05.  
  
    
``` r

#Bootstrap Parameters (CI=0.95,B=1000,na.rm=True,reps=False)

#BAT Decades
Fig5a<-TnCeDec %>% ggplot(aes(x=ROIName,y=Mean,color=nScan,fill=nScan)) +
  stat_summary(fun.data="mean_cl_boot",na.rm=TRUE,geom="crossbar",width=0.4,alpha=0.4,
               position=position_dodge(width=-0.5)) +
  annotate("text", x=c(1,2,3,7,8,9,10), y=c(18, 25, 35,70,78,88,98),label='*', size=8) +
  scale_x_discrete(limits=c("BAT0","BAT10","BAT20","BAT30","BAT40","BAT50","BAT60","BAT70","BAT80","BAT90"),labels=DecLabShort) +
  scale_y_continuous(limits=c(0,100), breaks=c(0,20,40,60,80,100)) +
  scale_color_manual(values=c("#1F77B4", "#D62728"),breaks=c("TN","CE"),guide=FALSE) +
  scale_fill_manual(values=c("#1F77B4", "#D62728"),breaks=c("TN","CE"),labels=c("Thermoneutral","Cold Exposure")) +
  labs(x="BAT FSF Decade",y="Mean FSF (%)") +
  background_grid(major="y") +
  theme(legend.title=element_blank(),legend.position = c(0.8, 0.25))

#MUS Decades
Fig5b<-TnCeMUS %>% ggplot(aes(x=ROIName,y=Mean,color=nScan,fill=nScan)) +
  stat_summary(fun.data="mean_cl_boot",na.rm=TRUE,geom="crossbar",width=0.4,alpha=0.4,
               position=position_dodge(width=-0.5)) +
  annotate("text", x=c(1), y=c(18),label='*', size=8) +
  scale_x_discrete(limits=c("MUS0","MUS10"),labels=DecLabShort[1:2]) +
  scale_y_continuous(limits=c(0,100), breaks=c(0,20,40,60,80,100)) +
  scale_color_manual(values=c("#1F77B4", "#D62728"),breaks=c("TN","CE"),guide=FALSE) +
  scale_fill_manual(values=c("#1F77B4", "#D62728"),breaks=c("TN","CE"),labels=c("Thermoneutral","Cold Exposure")) +
  labs(x="MUS FSF Decade",y="Mean FSF (%)") +
  background_grid(major="y") +
  theme(legend.title=element_blank(),legend.position = c(0.8, 0.75))

#WAT Decades
Fig5c<-TnCeWAT %>% ggplot(aes(x=ROIName,y=Mean,color=nScan,fill=nScan)) +
  stat_summary(fun.data="mean_cl_boot",na.rm=TRUE,geom="crossbar",width=0.4,alpha=0.4,
               position=position_dodge(width=-0.5)) +
  annotate("text", x=c(1,2,3), y=c(85,90,100),label='*', size=8) +
  scale_x_discrete(limits=c("WAT70","WAT80","WAT90"),labels=DecLabShort[8:10]) +
  scale_y_continuous(limits=c(0,100), breaks=c(0,20,40,60,80,100)) +
  scale_color_manual(values=c("#1F77B4", "#D62728"),breaks=c("TN","CE"),guide=FALSE) +
  scale_fill_manual(values=c("#1F77B4", "#D62728"),breaks=c("TN","CE"),labels=c("Thermoneutral","Cold Exposure")) +
  labs(x="SAT FSF Decade",y="Mean FSF (%)") +
  background_grid(major="y") +
  theme(legend.title=element_blank(),legend.position = c(0.8, 0.25))


# Grid Plots
# align left most side top and bottom rows
plots <- align_plots(Fig5b, Fig5a, align='v',axis='l')
# create bottom row of figure
bottom_row<-plot_grid(plots[[1]],Fig5c,labels=c('B','C'), nrow=1, ncol=2, align = 'h')
# combine top and bottom rows
Fig5<-plot_grid(plots[[2]],bottom_row,labels=c('A',''),ncol=1)
Fig5

# Save Plot - uncomment to save plot as a tiff file
#save_plot("Fig5.tiff", Fig5, base_width=12, base_aspect_ratio = 1.618, base_height=NULL)

```

 
 
## Figure 6: Cold Exposure Dynamically Alters Mean Fat-Signal Fraction in Brown Adipose Tissue  
Facet plot of the mean fat-signal fraction (%) in each brown adipose tissue decade plotted with respect to the normalized cooling dose (°C*min/m^2). Individual data points  indicate the respective values calculated from each fat-water magnetic resonance image acquired for a participant. Least squares best-fit lines are overlaid on the mean fat-signal fraction data.  
  
  
``` r
# Create Labels for Individual Facet Plots (Spearman Rho, P Value) - See Section Below for Calculations
FacetTxt <- data.frame(
  labs = c("0.30 * ', P<0.001'", "0.29 * ', P<0.001'", "0.21 * ', P=0.004'", "0.15 * ', P=0.04'", "0.04 * ', P=0.63'",
            "-0.10 * ', P=0.21'", "-0.21 * ', P=0.006'", "-0.28 * ', P=0.001'", "-0.29 * ', P<0.001'", "-0.27 * ', P<0.001'"),
  ROIName = c("BAT0","BAT10","BAT20","BAT30","BAT40","BAT50","BAT60","BAT70","BAT80","BAT90"), 
  x = rep(150, 10),
  y = c(85, 85, 85, 85, 85, 15, 15, 15, 15, 15)
)

# Facet Plot: BAT Decades (Normalized Cooling Dose) 
Fig6<-BATDec.Data %>% 
  ggplot(aes(x=CDNorm,y=Mean)) +
  geom_jitter(aes(fill=ROIName),pch=21,width=0.2,size=1.5) +
  geom_smooth(method=lm, se=FALSE, color="black",size=0.7) +
  coord_cartesian(ylim=c(0,100)) +
  scale_fill_manual(values=TabPal) +
  background_grid(major="y") +
  labs(x=expression(paste("Normalized Cooling Dose (",degree,"C*min*m"^"-2",")")), 
       y="Mean FSF (%)") +
  facet_wrap(~ROIName, nrow=2, ncol=5, labeller = labeller(ROIName = DecLab)) +
  theme(strip.text.x=element_text(size=10),legend.position="none")

# Add Facet Labels
Fig6<-Fig6 + geom_text(aes(x, y, label=paste("rho ==", labs), group=NULL), size=4, 
          color = "grey10", data=FacetTxt, parse = T)

#Display Plot
Fig6

# Save Plot - uncomment to save plot as a tiff file
#save_plot("Fig6.tiff", Fig6, base_width=12, base_aspect_ratio = 1.618, base_height=NULL)
```
  
  
### Spearman Rank Correlation: Mean FSF vs. Normalized Cooling Dose
Correlation (ρ) between mean fat-signal fraction (FSF) and normalized cooling dose for each decade group was evaluated with the Spearman rank test.  

  
``` r
#Create Decade Data Frames
Bat0<-BATDec.Data %>% dplyr::filter(ROIName=="BAT0") %>% dplyr::select(CDNorm,Mean)
Bat10<-BATDec.Data %>% dplyr::filter(ROIName=="BAT10") %>% dplyr::select(CDNorm,Mean)
Bat20<-BATDec.Data %>% dplyr::filter(ROIName=="BAT20") %>% dplyr::select(CDNorm,Mean)
Bat30<-BATDec.Data %>% dplyr::filter(ROIName=="BAT30") %>% dplyr::select(CDNorm,Mean)
Bat40<-BATDec.Data %>% dplyr::filter(ROIName=="BAT40") %>% dplyr::select(CDNorm,Mean)
Bat50<-BATDec.Data %>% dplyr::filter(ROIName=="BAT50") %>% dplyr::select(CDNorm,Mean)
Bat60<-BATDec.Data %>% dplyr::filter(ROIName=="BAT60") %>% dplyr::select(CDNorm,Mean)
Bat70<-BATDec.Data %>% dplyr::filter(ROIName=="BAT70") %>% dplyr::select(CDNorm,Mean)
Bat80<-BATDec.Data %>% dplyr::filter(ROIName=="BAT80") %>% dplyr::select(CDNorm,Mean)
Bat90<-BATDec.Data %>% dplyr::filter(ROIName=="BAT90") %>% dplyr::select(CDNorm,Mean)

#Calculate Correlation Coefficients - Spearman Rank Method (nonparametric test)
#exact p-values were not calculated due to ties
cor.test(Bat0$CDNorm,Bat0$Mean, method='spearman')
cor.test(Bat10$CDNorm,Bat10$Mean, method='spearman')
cor.test(Bat20$CDNorm,Bat20$Mean, method='spearman')
cor.test(Bat30$CDNorm,Bat30$Mean, method='spearman')
cor.test(Bat40$CDNorm,Bat40$Mean, method='spearman')
cor.test(Bat50$CDNorm,Bat50$Mean, method='spearman')
cor.test(Bat60$CDNorm,Bat60$Mean, method='spearman')
cor.test(Bat70$CDNorm,Bat70$Mean, method='spearman')
cor.test(Bat80$CDNorm,Bat80$Mean, method='spearman')
cor.test(Bat90$CDNorm,Bat90$Mean, method='spearman')

```  


## Figure 7: Mean Fat-Signal Fraction Response Tracks with Thermal Sensation  
Facet plot of the mean fat-signal fraction (%) in each brown adipose tissue decade plotted with respect to thermal sensation. Thermal sensation values were logged on a continuous integer scale between 50 (i.e. "Neutral") and 0 (i.e. "Very Cold"; "V.Cold"). Individual data points indicate the respective values calculated from each fat-water magnetic resonance image acquired for a participant. Least squares best-fit lines are overlaid on the mean fat-signal fraction data.  

  
``` r
# Create Labels for Individual Facet Plots (Spearman Rho, P Value) - See Section Below for Calculations
FacetTxt2 <- data.frame(
  labs = c("0.18 * ', P=0.02'", "0.17 * ', P=0.02'", "0.06 * ', P=0.41'", "-0.01 * ', P=0.91'", "-0.12 * ', P=0.12'",
           "-0.23 * ', P=0.002'", "-0.33 * ', P<0.001'", "-0.36 * ', P<0.001'", "-0.38 * ', P<0.001'", "-0.35 * ', P<0.001'"),
  ROIName = c("BAT0","BAT10","BAT20","BAT30","BAT40","BAT50","BAT60","BAT70","BAT80","BAT90"), 
  x = rep(34, 10),
  y = c(85, 85, 85, 85, 85, 15, 15, 15, 15, 15)
)

# Facet Plot: BAT Decades (Normalized Cooling Dose) 
Fig7<-BATDec.Data %>% 
  ggplot(aes(x=tGUI,y=Mean)) +
  geom_jitter(aes(fill=ROIName),pch=21,width=0.2,size=1.5) +
  geom_smooth(method=lm, se=FALSE, color="black", size=0.7) +
  coord_cartesian(ylim=c(0,100)) +
  scale_fill_manual(values=TabPal) +
  scale_x_reverse(lim=c(51,-1),
                  breaks=c(0,50),
                  labels=c("V.Cold","Neutral")) +
  background_grid(major="y") +
  labs(x="Thermal Sensation", y="Mean FSF (%)") +
  facet_wrap(~ROIName, nrow=2, ncol=5, labeller = labeller(ROIName = DecLab)) +
  theme(strip.text.x=element_text(size=10),legend.position="none", axis.text.x=element_text(angle=30,vjust=0.75))

# Add Facet Labels
Fig7<-Fig7 + geom_text(aes(x, y, label=paste("rho ==", labs), group=NULL), size=4, 
          color = "grey10", data=FacetTxt2, parse = T)

#Display Plot
Fig7

# Save Plot - uncomment to save plot as a tiff file
#save_plot("Fig7.tiff", Fig7, base_width=12, base_aspect_ratio = 1.618, base_height=NULL)

```
  
  
### Spearman Rank Correlation: Mean FSF vs. Thermal Sensation  
Correlation (ρ) between mean fat-signal fraction (FSF) and thermal sensation for each decade group was evaluated with the Spearman rank test.  
 
   
``` r

#Create Decade Data Frames
Bat0<-BATDec.Data %>% dplyr::filter(ROIName=="BAT0") %>% dplyr::select(tGUI,Mean)
Bat10<-BATDec.Data %>% dplyr::filter(ROIName=="BAT10") %>% dplyr::select(tGUI,Mean)
Bat20<-BATDec.Data %>% dplyr::filter(ROIName=="BAT20") %>% dplyr::select(tGUI,Mean)
Bat30<-BATDec.Data %>% dplyr::filter(ROIName=="BAT30") %>% dplyr::select(tGUI,Mean)
Bat40<-BATDec.Data %>% dplyr::filter(ROIName=="BAT40") %>% dplyr::select(tGUI,Mean)
Bat50<-BATDec.Data %>% dplyr::filter(ROIName=="BAT50") %>% dplyr::select(tGUI,Mean)
Bat60<-BATDec.Data %>% dplyr::filter(ROIName=="BAT60") %>% dplyr::select(tGUI,Mean)
Bat70<-BATDec.Data %>% dplyr::filter(ROIName=="BAT70") %>% dplyr::select(tGUI,Mean)
Bat80<-BATDec.Data %>% dplyr::filter(ROIName=="BAT80") %>% dplyr::select(tGUI,Mean)
Bat90<-BATDec.Data %>% dplyr::filter(ROIName=="BAT90") %>% dplyr::select(tGUI,Mean)

#Calculate Correlation Coefficients - Spearman Rank Method (nonparametric test)
#exact p values are not computed due to possible ties in the data set
cor.test(Bat0$tGUI,Bat0$Mean, method='spearman')
cor.test(Bat10$tGUI,Bat10$Mean, method='spearman')
cor.test(Bat20$tGUI,Bat20$Mean, method='spearman')
cor.test(Bat30$tGUI,Bat30$Mean, method='spearman')
cor.test(Bat40$tGUI,Bat40$Mean, method='spearman')
cor.test(Bat50$tGUI,Bat50$Mean, method='spearman')
cor.test(Bat60$tGUI,Bat60$Mean, method='spearman')
cor.test(Bat70$tGUI,Bat70$Mean, method='spearman')
cor.test(Bat80$tGUI,Bat80$Mean, method='spearman')
cor.test(Bat90$tGUI,Bat90$Mean, method='spearman')

```
## Figure S1: Computer Simulation of the Effect of Temperature on FSF Bias      
Plot the effect of temperature change on the quantification of FSF at each of the BAT FSF Decade.  
```{r FigS1, echo=FALSE}

# Import Simulation Data
Sim.Data<-read.csv(file=file.path("tidy_data","data_S1_temp_simulation.csv"),header=TRUE)

# Reformat to Long Form
Sim.Long<-Sim.Data %>% gather(FSF,Bias,FSF_0.05:FSF_0.95)

# Plot FSF Bias vs. Temperature
FigS1<-Sim.Long %>% ggplot(aes(x=Temperature, y=(Bias*1000), color=FSF)) +
  geom_line(size=0.8) + 
  scale_color_manual(name="BAT FSF Decade",values=TabPal,labels=DecLabShort) +
  scale_x_continuous(limits=c(36,40), breaks=seq(36,40,1)) +
  scale_y_continuous(limits=c(-2,5), breaks=seq(-2,5,1)) + 
  labs(x=expression(paste("Temperature (",degree,"C)")), 
       y=expression(paste("FSF Bias (x",10^-3,")"))) +
  background_grid(major="y") +
  theme(legend.position =c(0.2,0.75))
  
#Display Plot
FigS1

# Save Plot - uncomment to save plot as a tiff file
#save_plot("FigS1.tiff", FigS1, base_width=12, base_aspect_ratio = 1.618, base_height=NULL)

```
