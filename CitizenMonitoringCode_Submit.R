######################################
# Citizen Monitoring Manuscript Code 
# Author: Autumn Poisson
# Date: 6/8/2018
######################################

#loading LAGOS and other libraries
library(LAGOSNE)
library(tidyverse)
library(dplyr)
library(reshape2)
library(data.table)
library(ggplot2)
library(extrafont)
library(Cairo)

#set temporal constraints
first_year = 1980
last_year = 2010

# set working directory
setwd("C:/Users/FWL/Documents/CitizenMonitoringLAGOSNE")

# Reference Information

######## Citizen Monitoring Program Names ##########################
#
#   MI = 'MI_CORPS_CHEM'
#   IN = 'IN_chemistry'
#   RI = 'RI_URIWW_CHEM'
#   NY = 'NY_CSLAP' 
#   MN = 'MN_MPCA_SECCHI' 
#   NH = 'NH_VLAP' | 'NH_VLAP_1995_2014' 
#   MO = 'MO_LMVP_CHEM' <- MO_UM_SLAP is a state agency program not CM
#
###### States with Non-citizen monitoring programs #################
#
#   Michigan
#   New York
#   Minnesota 
#   New Hampshire
#   Missouri (MO_UM_SLAP_1978_2013)
#   
####### STATE ZONE IDS ############################
#
#    State_3 = MI
#   State_12 = IN
#    State_8 = RI
#    State_5 = NY
#   State_14 = MN
#   State_16 = NH
#   State_15 = MO
#
###################################################

#load LAGOS-NE dataset
dt <- lagosne_load(version = "1.087.1", format = "rds")

#Program type not reflected into epi_nutri/secchi so can't go there first, must combine with source program. 
Programs <- select(dt$lagos_source_program, programtype, programname)

######################### Removing Duplicates #####################

#logslakeid and state_name in one file. 
id_state <- select(dt$locus, lagoslakeid, state_zoneid)
state_stuff <- select(dt$state, state, state_name, state_zoneid)
lagos1 <- left_join(id_state, state_stuff, by = "state_zoneid")
lagos_nutri <- left_join(lagos1, dt$epi_nutr, by = "lagoslakeid")

# SECCHI #
secchi_dup <- dt$secchi
# secchi1 is all the secchi data with state information added
secchi_dup1 <- select(secchi_dup, lagoslakeid, sampledate, secchi, programname, sampleyear, samplemonth)
#list of duplicate sampledates and lagoslakeids
secchi_NODUP <- secchi_dup1[!duplicated(secchi_dup1[1:3]),]

# NUTRIENTS
limno_nutri <- select(dt$epi_nutr, lagoslakeid, sampledate, tp, chla, no2no3, tkn, tn,
                      programname, programtype, samplemonth, sampleyear)
# TP
TP_dup <- select(limno_nutri, lagoslakeid, sampledate, tp, programname, sampleyear, samplemonth)
TP_NODUP <- TP_dup[!duplicated(TP_dup[1:3]),]

# CHLA
CHLA_dup <- select(limno_nutri, lagoslakeid, sampledate, chla, programname, sampleyear, samplemonth)
CHLA_NODUP <- CHLA_dup[!duplicated(CHLA_dup[1:3]),]

# TN
# first calculate TN for lakes with TKN and NO2NO3 data
limno_nutri$tn_calculated <- limno_nutri$tkn + limno_nutri$no2no3
limno_nutri$tn_combined <- limno_nutri$tn
limno_nutri$tn_combined[which(is.na(limno_nutri$tn_combined) == T)] <- limno_nutri$tn_calculated[which(is.na(limno_nutri$tn_combined) == T)]

TN_dup <- select(limno_nutri, lagoslakeid, sampledate, tn_combined, programname, sampleyear, samplemonth)
TN_NODUP <- TN_dup[!duplicated(TN_dup[1:3]),]

#################################################################################################################

#update programs
TP_update <- left_join(TP_NODUP, Programs, by = 'programname')
CHLA_update <- left_join(CHLA_NODUP, Programs, by = 'programname')
secchi_update <- left_join(secchi_NODUP, Programs, by = 'programname')
TN_update <- left_join(TN_NODUP, Programs, by = 'programname')

#remove lakeids where there is nothing sampled for tp, chla or secchi, keep when sampled even once
lakes_TP <- TP_update[!(is.na(TP_update$tp)),]
lakes_CHLA <- CHLA_update[!(is.na(CHLA_update$chla)),]
lakes_secchi <- secchi_update[!(is.na(secchi_update$secchi)),]
lakes_TN <- TN_update[!(is.na(TN_update$tn_combined)),]

#select only summer sampling period
##separate sample date into Y, M, D so we have a date of month for summer cutoffs
#reformating
lakes_TP$sampledate2 <- as.Date(lakes_TP$sampledate, format="%m/%d/%Y", "%d")
lakes_CHLA$sampledate2 <- as.Date(lakes_CHLA$sampledate, format="%m/%d/%Y", "%d")
lakes_secchi$sampledate2 <- as.Date(lakes_secchi$sampledate, format="%m/%d/%Y", "%d")
lakes_TN$sampledate2 <- as.Date(lakes_TN$sampledate, format="%m/%d/%Y", "%d")
#pulling out date
lakes_TP$DoM <- format(as.Date(lakes_TP$sampledate2, format="%Y-%m-%d"), "%d")
lakes_CHLA$DoM <- format(as.Date(lakes_CHLA$sampledate2, format="%Y-%m-%d"), "%d")
lakes_secchi$DoM <- format(as.Date(lakes_secchi$sampledate2, format="%Y-%m-%d"), "%d")
lakes_TN$DoM <- format(as.Date(lakes_TN$sampledate2, format="%Y-%m-%d"), "%d")

lakes_TP$sampledate2 = NULL
lakes_CHLA$sampledate2 = NULL
lakes_secchi$sampledate2 = NULL
lakes_TN$sampledate2 = NULL

################ Getting only summer samples ############################

#create a data subset which only includes samples that were collected in summer (June 15-Sept 15)
#coincides with lake stratification period

#TP
TPdata.june.1 <- subset(lakes_TP, samplemonth==6 & DoM>=15)
TPdata.june.2 <- subset(lakes_TP, samplemonth==7 & DoM<15)
TPdata.june <- rbind(TPdata.june.1, TPdata.june.2)

TPdata.july.1 <-  subset(lakes_TP, samplemonth==7 & DoM>=15)
TPdata.july.2 <- subset(lakes_TP, samplemonth==8 & DoM<15)
TPdata.july <- rbind(TPdata.july.1, TPdata.july.2)

TPdata.aug.1 <-  subset(lakes_TP, samplemonth==8 & DoM>=15)
TPdata.aug.2 <-  subset(lakes_TP, samplemonth==9 & DoM<15)
TPdata.aug <-  rbind(TPdata.aug.1, TPdata.aug.2)

tp.data.summer <- rbind(TPdata.aug, TPdata.june, TPdata.july)

#CHLA
CHLAdata.june.1 <- subset(lakes_CHLA, samplemonth==6 & DoM>=15)
CHLAdata.june.2 <- subset(lakes_CHLA, samplemonth==7 & DoM<15)
CHLAdata.june <- rbind(CHLAdata.june.1, CHLAdata.june.2)

CHLAdata.july.1 <-  subset(lakes_CHLA, samplemonth==7 & DoM>=15)
CHLAdata.july.2 <- subset(lakes_CHLA, samplemonth==8 & DoM<15)
CHLAdata.july <- rbind(CHLAdata.july.1, CHLAdata.july.2)

CHLAdata.aug.1 <-  subset(lakes_CHLA, samplemonth==8 & DoM>=15)
CHLAdata.aug.2 <-  subset(lakes_CHLA, samplemonth==9 & DoM<15)
CHLAdata.aug <-  rbind(CHLAdata.aug.1, CHLAdata.aug.2)

CHLA.data.summer <- rbind(CHLAdata.aug, CHLAdata.june, CHLAdata.july)

#secchi
secchidata.june.1 <- subset(lakes_secchi, samplemonth==6 & DoM>=15)
secchidata.june.2 <- subset(lakes_secchi, samplemonth==7 & DoM<15)
secchidata.june <- rbind(secchidata.june.1, secchidata.june.2)

secchidata.july.1 <-  subset(lakes_secchi, samplemonth==7 & DoM>=15)
secchidata.july.2 <- subset(lakes_secchi, samplemonth==8 & DoM<15)
secchidata.july <- rbind(secchidata.july.1, secchidata.july.2)

secchidata.aug.1 <-  subset(lakes_secchi, samplemonth==8 & DoM>=15)
secchidata.aug.2 <-  subset(lakes_secchi, samplemonth==9 & DoM<15)
secchidata.aug <-  rbind(secchidata.aug.1, secchidata.aug.2)

secchi.data.summer <- rbind(secchidata.aug, secchidata.june, secchidata.july)

# TN
TNdata.june.1 <- subset(lakes_TN, samplemonth==6 & DoM>=15)
TNdata.june.2 <- subset(lakes_TN, samplemonth==7 & DoM<15)
TNdata.june <- rbind(TNdata.june.1, TNdata.june.2)

TNdata.july.1 <-  subset(lakes_TN, samplemonth==7 & DoM>=15)
TNdata.july.2 <- subset(lakes_TN, samplemonth==8 & DoM<15)
TNdata.july <- rbind(TNdata.july.1, TNdata.july.2)

TNdata.aug.1 <-  subset(lakes_TN, samplemonth==8 & DoM>=15)
TNdata.aug.2 <-  subset(lakes_TN, samplemonth==9 & DoM<15)
TNdata.aug <-  rbind(TNdata.aug.1, TNdata.aug.2)

TN.data.summer <- rbind(TNdata.aug, TNdata.june, TNdata.july)


#only want years between 1980 to 2010
TP.data.years <- subset(tp.data.summer, sampleyear>=first_year & sampleyear<=last_year)
CHLA.data.years <- subset(CHLA.data.summer, sampleyear>=first_year & sampleyear<=last_year)
secchi.data.years <- subset(secchi.data.summer, sampleyear>=first_year & sampleyear<=last_year)
TN.data.years <- subset(TN.data.summer, sampleyear>=first_year & sampleyear<=last_year)

#create a column "category" where each lake id is aligned with either a Citizen monitoring program or a Non-Citizen monitoring program
TP.data.years$category <- ifelse(TP.data.years$programtype == "Citizen Science", "Citizen monitoring", "Non-citizen monitoring")
CHLA.data.years$category <- ifelse(CHLA.data.years$programtype == "Citizen Science", "Citizen monitoring", "Non-citizen monitoring")
secchi.data.years$category <- ifelse(secchi.data.years$programtype == "Citizen Science", "Citizen monitoring", "Non-citizen monitoring")
TN.data.years$category <- ifelse(TN.data.years$programtype == "Citizen Science", "Citizen monitoring", "Non-citizen monitoring")

#try to fix the MO_UM_SLAP_1978_2013 issue (was categorized incorrectly)
TP.data.years <- transform(TP.data.years, category = ifelse(programname == "MO_UM_SLAP_1978_2013", "Non-citizen monitoring", category))
CHLA.data.years <- transform(CHLA.data.years, category = ifelse(programname == "MO_UM_SLAP_1978_2013", "Non-citizen monitoring", category))
secchi.data.years <- transform(secchi.data.years, category = ifelse(programname == "MO_UM_SLAP_1978_2013", "Non-citizen monitoring", category))
TN.data.years <- transform(TN.data.years, category = ifelse(programname == "MO_UM_SLAP_1978_2013", "Non-citizen monitoring", category))

#assigning state name
CHLA.data.years$state <- ifelse(grepl('^MI_', CHLA.data.years$programname), "Michigan",
                                ifelse(grepl('^IN_', CHLA.data.years$programname), "Indiana",
                                       ifelse(grepl('^RI_', CHLA.data.years$programname), "Rhode Island",
                                              ifelse(grepl('^MN_', CHLA.data.years$programname), "Minnesota",
                                                     ifelse(grepl('^NY_', CHLA.data.years$programname), "New York",
                                                            ifelse(grepl('^NH_', CHLA.data.years$programname), "New Hampshire",
                                                                   ifelse(grepl('^MO_', CHLA.data.years$programname), "Missouri",NA)))))))

TP.data.years$state <- ifelse(grepl('^MI_', TP.data.years$programname), "Michigan",
                              ifelse(grepl('^IN_', TP.data.years$programname), "Indiana",
                                     ifelse(grepl('^RI_', TP.data.years$programname), "Rhode Island",
                                            ifelse(grepl('^MN_', TP.data.years$programname), "Minnesota",
                                                   ifelse(grepl('^NY_', TP.data.years$programname), "New York",
                                                          ifelse(grepl('^NH_', TP.data.years$programname), "New Hampshire",
                                                                 ifelse(grepl('^MO_', TP.data.years$programname), "Missouri",NA)))))))

secchi.data.years$state <- ifelse(grepl('^MI_', secchi.data.years$programname), "Michigan",
                                  ifelse(grepl('^IN_', secchi.data.years$programname), "Indiana",
                                         ifelse(grepl('^RI_', secchi.data.years$programname), "Rhode Island",
                                                ifelse(grepl('^MN_', secchi.data.years$programname), "Minnesota",
                                                       ifelse(grepl('^NY_', secchi.data.years$programname), "New York",
                                                              ifelse(grepl('^NH_', secchi.data.years$programname), "New Hampshire",
                                                                     ifelse(grepl('^MO_', secchi.data.years$programname), "Missouri",NA)))))))

TN.data.years$state <- ifelse(grepl('^MI_', TN.data.years$programname), "Michigan",
                              ifelse(grepl('^IN_', TN.data.years$programname), "Indiana",
                                     ifelse(grepl('^RI_', TN.data.years$programname), "Rhode Island",
                                            ifelse(grepl('^MN_', TN.data.years$programname), "Minnesota",
                                                   ifelse(grepl('^NY_', TN.data.years$programname), "New York",
                                                          ifelse(grepl('^NH_', TN.data.years$programname), "New Hampshire",
                                                                 ifelse(grepl('^MO_', TN.data.years$programname), "Missouri",NA)))))))

#remove data from unwanted states: states of interest = SOI
TP_SOI <- TP.data.years[!(is.na(TP.data.years$state)),]
CHLA_SOI <- CHLA.data.years[!(is.na(CHLA.data.years$state)),]
SECCHI_SOI <- secchi.data.years[!(is.na(secchi.data.years$state)),]
TN_SOI <- TN.data.years[!(is.na(TN.data.years$state)),]

# proportion of samples by CS and non
TN_prop_allsamples <- as.data.frame(TN_SOI %>% 
  group_by(category) %>%
  tally())
TN_prop_allsamples$Prop <- TN_prop_allsamples$n/nrow(TN_SOI)

TP_prop_allsamples <- as.data.frame(TP_SOI %>% 
                                      group_by(category) %>%
                                      tally())
TP_prop_allsamples$Prop <- TP_prop_allsamples$n/nrow(TP_SOI)

chla_prop_allsamples <- as.data.frame(CHLA_SOI %>% 
                                      group_by(category) %>%
                                      tally())
chla_prop_allsamples$Prop <- chla_prop_allsamples$n/nrow(CHLA_SOI)

secchi_prop_allsamples <- as.data.frame(SECCHI_SOI %>% 
                                      group_by(category) %>%
                                      tally())
secchi_prop_allsamples$Prop <- secchi_prop_allsamples$n/nrow(SECCHI_SOI)


#uniq combos of lagoslakeid and category - to get number of lakes sampled by both program type/category
TPdata.uniquelakes <- TP_SOI[!duplicated(TP_SOI[c("lagoslakeid","category")] ), ]
CHLAdata.uniquelakes <- CHLA_SOI[!duplicated(CHLA_SOI[c("lagoslakeid","category")] ), ]
SECCHIdata.uniquelakes <- SECCHI_SOI[!duplicated(SECCHI_SOI[c("lagoslakeid","category")] ), ]
TNdata.uniquelakes <- TN_SOI[!duplicated(TN_SOI[c("lagoslakeid","category")] ), ]

#count # of duplicate lagoslakeids
TPdata.lakeDUPS <- TPdata.uniquelakes[!duplicated(TPdata.uniquelakes[c("lagoslakeid")] ), ]
# 5279-4865=414
CHLAdata.lakeDUPS <- CHLAdata.uniquelakes[!duplicated(CHLAdata.uniquelakes[c("lagoslakeid")] ), ]
# 4764-4537=227
SECCHIdata.lakeDUPS <- SECCHIdata.uniquelakes[!duplicated(SECCHIdata.uniquelakes[c("lagoslakeid")] ), ]
# 8416-6391=2025
TNdata.lakeDUPS <- TNdata.uniquelakes[!duplicated(TNdata.uniquelakes[c("lagoslakeid")] ), ]
# 3430-3368= 62

###########################################
######### Q1 Proportion Plots #############
###########################################

####################SECCHI#########################

SECCHI_proportion <- SECCHI_SOI

SECCHIplot <- ggplot(SECCHI_proportion, aes(x = sampleyear, fill = category)) + geom_bar(position = "fill", width=2) + ggtitle(paste0("A) Water clarity",' (n=', nrow(SECCHI_SOI), ')')) +
  labs(x="Year",y="Proportion of data") + scale_fill_discrete(name = "Program type") + theme(legend.position="none") +
  theme(text = element_text(size=8)) + theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black"),
  plot.title=element_text(size=6),
  axis.text.x=element_text(size=6),
  axis.text.y=element_text(size=6),
  axis.title.x=element_text(size=6),
  axis.title.y=element_text(size=6))

############### Total Phosphorous ##################

TP_proportion <- TP_SOI

TPplot <- ggplot(TP_proportion, aes(x = sampleyear, fill = category)) + geom_bar(position = "fill", width = 2) + ggtitle(paste0("B) Phosphorus", ' (n=', nrow(TP_SOI), ')')) +
  labs(x="Year",y="Proportion of data") +scale_fill_discrete(name = "Program type") + theme(legend.position="none") +
  theme(text = element_text(size=8)) + 
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title=element_text(size=6),
        axis.text.x=element_text(size=6),
        axis.text.y=element_text(size=6),
        axis.title.x=element_text(size=6),
        axis.title.y=element_text(size=6))
######################## TN ########################
TN_proportion <- TN_SOI

TNplot <- ggplot(TN_proportion, aes(x = sampleyear, fill = category)) + geom_bar(position = "fill", width = 2) + ggtitle(paste0("C) Nitrogen", ' (n=', nrow(TN_SOI), ')')) +
  labs(x="Year",y="Proportion of data") +scale_fill_discrete(name = "Program type") + theme(legend.position="none") +
  theme(text = element_text(size=8)) + theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black"),
  plot.title=element_text(size=6),
  axis.text.x=element_text(size=6),
  axis.text.y=element_text(size=6),
  axis.title.x=element_text(size=6),
  axis.title.y=element_text(size=6))


##################Chlorophyll######################
CHLA_proportion <- CHLA_SOI

CHLAplot <- ggplot(CHLA_proportion, aes(x = sampleyear, fill = category)) + geom_bar(position = "fill", width = 2) + ggtitle(paste0("D) Algal biomass", ' (n=', nrow(CHLA_SOI), ')')) +
  labs(x="Year",y="Proportion of data") + scale_fill_discrete(name = "Program type", labels=c('Citizen','Non')) + theme(legend.position= c(0.72, 0.18)) +
  theme(text = element_text(size=6)) + theme(legend.title=element_blank(), 
  legend.margin=margin(c(1,1,1,1))) + theme(legend.text=element_text(size=7)) +
  theme(legend.key.size = unit(0.5,"line")) + theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black"),
  plot.title=element_text(size=6),
  axis.text.x=element_text(size=6),
  axis.text.y=element_text(size=6),
  axis.title.x=element_text(size=6),
  axis.title.y=element_text(size=6))

########## Multiplot ##########

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(SECCHIplot, TPplot, TNplot, CHLAplot, cols=4)

##CairoPDF(file="proportionPlot.pdf", width=11, height=5, family="Helvetica", pointsize=12) #old
#png(filename='ExportedFigures/Fig1proportionPlot_wTN.png', width=6, height=2.5, units='in', res=300)
#  multiplot(SECCHIplot, TPplot, TNplot, CHLAplot, cols=4)
#dev.off()

########################
### Q2 Temporal Data ###
########################

#separate out citizen monitoring and non-citizen monitoring 
CM_TP_data <- subset(TP_SOI, category == "Citizen monitoring")
nonCM_TP_data <- subset(TP_SOI, category == "Non-citizen monitoring")

CM_CHLA_data <- subset(CHLA_SOI, category == "Citizen monitoring")
nonCM_CHLA_data <- subset(CHLA_SOI, category == "Non-citizen monitoring")

CM_secchi_data <- subset(SECCHI_SOI, category == "Citizen monitoring")
nonCM_secchi_data <- subset(SECCHI_SOI, category == "Non-citizen monitoring")

CM_TN_data <- subset(TN_SOI, category == "Citizen monitoring")
nonCM_TN_data <- subset(TN_SOI, category == "Non-citizen monitoring")

#################### 
# Total Phosphorus #
####################

#CM - removing duplicates of sample year + lagoslakeid combo
CM_TP_temporal_plot <- CM_TP_data[!duplicated(CM_TP_data[c("lagoslakeid","sampleyear")] ), ] 
#selecting out the lagoslakeids so we can count each time they appear
CM_TP_temporal_count <- select(CM_TP_temporal_plot, lagoslakeid)
#count (CM_TP_temporal_count=lagoslakeid, Freq=number of years sampled)
CM_TP_temporal_count <- as.data.frame(table(CM_TP_temporal_count))
CM_TP_temporal_count$category <- "citizen monitoring"
#rename column headers
names(CM_TP_temporal_count) <- c("lagoslakeid", "count", "category")

CM_TP_temporal_count$yeargroup <- ifelse(CM_TP_temporal_count$count <= 5, "1-5",
                                         ifelse(CM_TP_temporal_count$count > 5 & CM_TP_temporal_count$count <= 10, "6-10",
                                                ifelse(CM_TP_temporal_count$count > 10 & CM_TP_temporal_count$count <= 15, "11-15",
                                                       ifelse(CM_TP_temporal_count$count >= 16, ">15", NA))))

CM_TP_data$lagoslakeid = as.factor(CM_TP_data$lagoslakeid)
CM_TP_temporal_count$category = NULL
CM_TP_yearsdata <- left_join(CM_TP_data, CM_TP_temporal_count, by = "lagoslakeid")

#NON-CM - removing duplicates of sample year and lagoslakeids
nonCM_TP_temporal_plot <- nonCM_TP_data[!duplicated(nonCM_TP_data[c("lagoslakeid","sampleyear")] ), ] 
#selecting out the lagoslakeids so we can count each time they appear
nonCM_TP_temporal_count <- select(nonCM_TP_temporal_plot, lagoslakeid)
#count (nonCM_TP_temporal_count=lagoslakeid, Freq=number of years sampled)
nonCM_TP_temporal_count <- as.data.frame(table(nonCM_TP_temporal_count))
nonCM_TP_temporal_count$category <- "Non-citizen monitoring"
#rename column headers
names(nonCM_TP_temporal_count) <- c("lagoslakeid", "count", "category")
#change the data type of lagoslakeid so we can combine with program info

nonCM_TP_temporal_count$yeargroup <- ifelse(nonCM_TP_temporal_count$count <= 5, "1-5",
                                            ifelse(nonCM_TP_temporal_count$count > 5 & nonCM_TP_temporal_count$count <= 10, "6-10",
                                                   ifelse(nonCM_TP_temporal_count$count > 10 & nonCM_TP_temporal_count$count <= 15, "11-15",
                                                          ifelse(nonCM_TP_temporal_count$count >= 16, ">15", NA))))

nonCM_TP_data$lagoslakeid = as.factor(nonCM_TP_data$lagoslakeid)
nonCM_TP_temporal_count$category = NULL
nonCM_TP_yearsdata <- left_join(nonCM_TP_data, nonCM_TP_temporal_count, by = "lagoslakeid")

TP_temporal_data1 <- bind_rows(CM_TP_yearsdata, nonCM_TP_yearsdata)

TP_temporal_data1$yeargroup <- factor(TP_temporal_data1$yeargroup, levels = c("1-5", "6-10", "11-15", ">15"))

#uniq combos of lagoslakeid and category - to get number of lakes sampled by both program type/category
TPdata.uniquelakes2 <- TP_temporal_data1[!duplicated(TP_temporal_data1[c("lagoslakeid","category")] ), ]

#count # of duplicate lagoslakeids
TPdata.lakeDUPS2 <- TPdata.uniquelakes2[!duplicated(TPdata.uniquelakes2[c("lagoslakeid")] ), ]
#

#################### 
# Total Nitrogen #
####################

#CM - removing duplicates of sample year + lagoslakeid combo
CM_TN_temporal_plot <- CM_TN_data[!duplicated(CM_TN_data[c("lagoslakeid","sampleyear")] ), ] 
#selecting out the lagoslakeids so we can count each time they appear
CM_TN_temporal_count <- select(CM_TN_temporal_plot, lagoslakeid)
#count (CM_TN_temporal_count=lagoslakeid, Freq=number of years sampled)
CM_TN_temporal_count <- as.data.frame(table(CM_TN_temporal_count))
CM_TN_temporal_count$category <- "citizen monitoring"
#rename column headers
names(CM_TN_temporal_count) <- c("lagoslakeid", "count", "category")

CM_TN_temporal_count$yeargroup <- ifelse(CM_TN_temporal_count$count <= 5, "1-5",
                                         ifelse(CM_TN_temporal_count$count > 5 & CM_TN_temporal_count$count <= 10, "6-10",
                                                ifelse(CM_TN_temporal_count$count > 10 & CM_TN_temporal_count$count <= 15, "11-15",
                                                       ifelse(CM_TN_temporal_count$count >= 16, ">15", NA))))

CM_TN_data$lagoslakeid = as.factor(CM_TN_data$lagoslakeid)
CM_TN_temporal_count$category = NULL
CM_TN_yearsdata <- left_join(CM_TN_data, CM_TN_temporal_count, by = "lagoslakeid")

#NON-CM - removing duplicates of sample year and lagoslakeids
nonCM_TN_temporal_plot <- nonCM_TN_data[!duplicated(nonCM_TN_data[c("lagoslakeid","sampleyear")] ), ] 
#selecting out the lagoslakeids so we can count each time they appear
nonCM_TN_temporal_count <- select(nonCM_TN_temporal_plot, lagoslakeid)
#count (nonCM_TN_temporal_count=lagoslakeid, Freq=number of years sampled)
nonCM_TN_temporal_count <- as.data.frame(table(nonCM_TN_temporal_count))
nonCM_TN_temporal_count$category <- "Non-citizen monitoring"
#rename column headers
names(nonCM_TN_temporal_count) <- c("lagoslakeid", "count", "category")
#change the data type of lagoslakeid so we can combine with program info

nonCM_TN_temporal_count$yeargroup <- ifelse(nonCM_TN_temporal_count$count <= 5, "1-5",
                                            ifelse(nonCM_TN_temporal_count$count > 5 & nonCM_TN_temporal_count$count <= 10, "6-10",
                                                   ifelse(nonCM_TN_temporal_count$count > 10 & nonCM_TN_temporal_count$count <= 15, "11-15",
                                                          ifelse(nonCM_TN_temporal_count$count >= 16, ">15", NA))))

nonCM_TN_data$lagoslakeid = as.factor(nonCM_TN_data$lagoslakeid)
nonCM_TN_temporal_count$category = NULL
nonCM_TN_yearsdata <- left_join(nonCM_TN_data, nonCM_TN_temporal_count, by = "lagoslakeid")

TN_temporal_data1 <- bind_rows(CM_TN_yearsdata, nonCM_TN_yearsdata)

TN_temporal_data1$yeargroup <- factor(TN_temporal_data1$yeargroup, levels = c("1-5", "6-10", "11-15", ">15"))

#uniq combos of lagoslakeid and category - to get number of lakes sampled by both program type/category
TNdata.uniquelakes2 <- TN_temporal_data1[!duplicated(TN_temporal_data1[c("lagoslakeid","category")] ), ]

#count # of duplicate lagoslakeids
TNdata.lakeDUPS2 <- TNdata.uniquelakes2[!duplicated(TNdata.uniquelakes2[c("lagoslakeid")] ), ]
#
#write.csv(TNdata.lakeDUPS2, "TN_years_data.csv")

#################
# Chlorophyll A #
#################

#CM - removing duplicates of sample year + lagoslakeid combo
CM_CHLA_temporal_plot <- CM_CHLA_data[!duplicated(CM_CHLA_data[c("lagoslakeid","sampleyear")] ), ] 
#selecting out the lagoslakeids so we can count each time they appear
CM_CHLA_temporal_count <- select(CM_CHLA_temporal_plot, lagoslakeid)
#count (CM_CHLA_temporal_count=lagoslakeid, Freq=number of years sampled)
CM_CHLA_temporal_count <- as.data.frame(table(CM_CHLA_temporal_count))
CM_CHLA_temporal_count$category <- "citizen monitoring"
#rename column headers
names(CM_CHLA_temporal_count) <- c("lagoslakeid", "count", "category")

CM_CHLA_temporal_count$yeargroup <- ifelse(CM_CHLA_temporal_count$count <= 5, "1-5",
                                           ifelse(CM_CHLA_temporal_count$count > 5 & CM_CHLA_temporal_count$count <= 10, "6-10",
                                                  ifelse(CM_CHLA_temporal_count$count > 10 & CM_CHLA_temporal_count$count <= 15, "11-15",
                                                         ifelse(CM_CHLA_temporal_count$count >= 16, ">15", NA))))

CM_CHLA_data$lagoslakeid = as.factor(CM_CHLA_data$lagoslakeid)
CM_CHLA_temporal_count$category = NULL
CM_CHLA_yearsdata <- left_join(CM_CHLA_data, CM_CHLA_temporal_count, by = "lagoslakeid")

#NON-CM - removing duplicates of sample year and lagoslakeids
nonCM_CHLA_temporal_plot <- nonCM_CHLA_data[!duplicated(nonCM_CHLA_data[c("lagoslakeid","sampleyear")] ), ] 
#selecting out the lagoslakeids so we can count each time they appear
nonCM_CHLA_temporal_count <- select(nonCM_CHLA_temporal_plot, lagoslakeid)
#count (nonCM_CHLA_temporal_count=lagoslakeid, Freq=number of years sampled)
nonCM_CHLA_temporal_count <- as.data.frame(table(nonCM_CHLA_temporal_count))
nonCM_CHLA_temporal_count$category <- "Non-citizen monitoring"
#rename column headers
names(nonCM_CHLA_temporal_count) <- c("lagoslakeid", "count", "category")
#change the data type of lagoslakeid so we can combine with program info

nonCM_CHLA_temporal_count$yeargroup <- ifelse(nonCM_CHLA_temporal_count$count <= 5, "1-5",
                                              ifelse(nonCM_CHLA_temporal_count$count > 5 & nonCM_CHLA_temporal_count$count <= 10, "6-10",
                                                     ifelse(nonCM_CHLA_temporal_count$count > 10 & nonCM_CHLA_temporal_count$count <= 15, "11-15",
                                                            ifelse(nonCM_CHLA_temporal_count$count >= 16, ">15", NA))))

nonCM_CHLA_data$lagoslakeid = as.factor(nonCM_CHLA_data$lagoslakeid)
nonCM_CHLA_temporal_count$category = NULL
nonCM_CHLA_yearsdata <- left_join(nonCM_CHLA_data, nonCM_CHLA_temporal_count, by = "lagoslakeid")

CHLA_temporal_data1 <- bind_rows(CM_CHLA_yearsdata, nonCM_CHLA_yearsdata)

CHLA_temporal_data1$yeargroup <- factor(CHLA_temporal_data1$yeargroup, levels = c("1-5", "6-10", "11-15", ">15"))

CHLAdata.uniquelakes2 <- CHLA_temporal_data1[!duplicated(CHLA_temporal_data1[c("lagoslakeid","category")] ), ]
CHLAdata.lakeDUPS2 <- CHLAdata.uniquelakes2[!duplicated(CHLAdata.uniquelakes2[c("lagoslakeid")] ), ]
#


##########
# SECCHI #
##########

#CM - removing duplicates of sample year + lagoslakeid combo
CM_SECCHI_temporal_plot <- CM_secchi_data[!duplicated(CM_secchi_data[c("lagoslakeid","sampleyear")] ), ] 
#selecting out the lagoslakeids so we can count each time they appear
CM_SECCHI_temporal_count <- select(CM_SECCHI_temporal_plot, lagoslakeid)
#count (CM_SECCHI_temporal_count=lagoslakeid, Freq=number of years sampled)
CM_SECCHI_temporal_count <- as.data.frame(table(CM_SECCHI_temporal_count))
CM_SECCHI_temporal_count$category <- "Citizen monitoring"
#rename column headers
names(CM_SECCHI_temporal_count) <- c("lagoslakeid", "count", "category")

CM_SECCHI_temporal_count$yeargroup <- ifelse(CM_SECCHI_temporal_count$count <= 5, "1-5",
                                             ifelse(CM_SECCHI_temporal_count$count > 5 & CM_SECCHI_temporal_count$count <= 10, "6-10",
                                                    ifelse(CM_SECCHI_temporal_count$count > 10 & CM_SECCHI_temporal_count$count <= 15, "11-15",
                                                           ifelse(CM_SECCHI_temporal_count$count >= 16, ">15", NA))))

CM_secchi_data$lagoslakeid = as.factor(CM_secchi_data$lagoslakeid)
CM_SECCHI_temporal_count$category = NULL
CM_SECCHI_yearsdata <- left_join(CM_secchi_data, CM_SECCHI_temporal_count, by = "lagoslakeid")

#NON-CM - removing duplicates of sample year and lagoslakeids
nonCM_SECCHI_temporal_plot <- nonCM_secchi_data[!duplicated(nonCM_secchi_data[c("lagoslakeid","sampleyear")] ), ] 
#selecting out the lagoslakeids so we can count each time they appear
nonCM_SECCHI_temporal_count <- select(nonCM_SECCHI_temporal_plot, lagoslakeid)
#count (nonCM_SECCHI_temporal_count=lagoslakeid, Freq=number of years sampled)
nonCM_SECCHI_temporal_count <- as.data.frame(table(nonCM_SECCHI_temporal_count))
nonCM_SECCHI_temporal_count$category <- "Non-citizen monitoring"
#rename column headers
names(nonCM_SECCHI_temporal_count) <- c("lagoslakeid", "count", "category")
#change the data type of lagoslakeid so we can combine with program info

nonCM_SECCHI_temporal_count$yeargroup <- ifelse(nonCM_SECCHI_temporal_count$count <= 5, "1-5",
                                                ifelse(nonCM_SECCHI_temporal_count$count > 5 & nonCM_SECCHI_temporal_count$count <= 10, "6-10",
                                                       ifelse(nonCM_SECCHI_temporal_count$count > 10 & nonCM_SECCHI_temporal_count$count <= 15, "11-15",
                                                              ifelse(nonCM_SECCHI_temporal_count$count >= 16, ">15", NA))))

nonCM_secchi_data$lagoslakeid = as.factor(nonCM_secchi_data$lagoslakeid)
nonCM_SECCHI_temporal_count$category = NULL
nonCM_SECCHI_yearsdata <- left_join(nonCM_secchi_data, nonCM_SECCHI_temporal_count, by = "lagoslakeid")

SECCHI_temporal_data1 <- bind_rows(CM_SECCHI_yearsdata, nonCM_SECCHI_yearsdata)

SECCHI_temporal_data1$yeargroup <- factor(SECCHI_temporal_data1$yeargroup, levels = c("1-5", "6-10", "11-15", ">15"))

SECCHIdata.uniquelakes2 <- SECCHI_temporal_data1[!duplicated(SECCHI_temporal_data1[c("lagoslakeid","category")] ), ]
SECCHIdata.lakeDUPS2 <- SECCHIdata.uniquelakes2[!duplicated(SECCHIdata.uniquelakes2[c("lagoslakeid")] ), ]

############### Q2 PLOT ###################
SECCHI_temporal_plot <- ggplot(SECCHI_temporal_data1, aes(x = yeargroup, fill = category)) + geom_bar(position = "fill") + ggtitle(paste0("A) Water clarity", ' (n=', nrow(SECCHI_temporal_data1), ')')) +
  labs(x="Years of data",y="Proportion of data") + theme(legend.position="none") + 
  theme(text = element_text(size=8)) + theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black"),
  plot.title=element_text(size=6),
  axis.text.x=element_text(size=6),
  axis.text.y=element_text(size=6),
  axis.title.x=element_text(size=6),
  axis.title.y=element_text(size=6))

TP_temporal_plot <- ggplot(TP_temporal_data1, aes(x = yeargroup, fill = category)) + geom_bar(position = "fill") + ggtitle(paste0("B) Phosphorus", ' (n=', nrow(TP_temporal_data1), ')')) +
  labs(x="Years of data",y="Proportion of data") + theme(legend.position="none") + 
  theme(text = element_text(size=8)) + theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black"),
  plot.title=element_text(size=6),
  axis.text.x=element_text(size=6),
  axis.text.y=element_text(size=6),
  axis.title.x=element_text(size=6),
  axis.title.y=element_text(size=6))

TN_temporal_plot <- ggplot(TN_temporal_data1, aes(x = yeargroup, fill = category)) + geom_bar(position = "fill") + ggtitle(paste0("C) Nitrogen", ' (n=', nrow(TN_temporal_data1), ')')) +
  labs(x="Years of data",y="Proportion of data") + theme(legend.position="none") + 
  theme(text = element_text(size=8)) + theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black"),
  plot.title=element_text(size=6),
  axis.text.x=element_text(size=6),
  axis.text.y=element_text(size=6),
  axis.title.x=element_text(size=6),
  axis.title.y=element_text(size=6))

CHLA_temporal_plot <- ggplot(CHLA_temporal_data1, aes(x = yeargroup, fill = category)) + geom_bar(position = "fill") + ggtitle(paste0("D) Algal biomass", ' (n=', nrow(CHLA_temporal_data1), ')')) +
  labs(x="Years of data",y="Proportion of data") + theme(legend.position= c(0.72, 0.18)) + scale_fill_discrete(name = "Program type", labels=c('Citizen','Non')) +
  theme(text = element_text(size=8)) + theme(legend.title=element_blank(), legend.margin=margin(c(1,1,1,1))) + 
  theme(legend.text=element_text(size=7)) +
  theme(legend.key.size = unit(0.5,"line")) + theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title=element_text(size=6),
        axis.text.x=element_text(size=6),
        axis.text.y=element_text(size=6),
        axis.title.x=element_text(size=6),
        axis.title.y=element_text(size=6))

multiplot(SECCHI_temporal_plot, TP_temporal_plot, TN_temporal_plot, CHLA_temporal_plot, cols=4)
##CairoPDF(file="TemporalPlot.pdf", width=11, height=5, family="Helvetica", pointsize=12)#old
#png(filename='ExportedFigures/Fig2proportionPlot_wTN.png', width=6, height=2.5, units='in', res=300)
#  multiplot(SECCHI_temporal_plot, TP_temporal_plot, TN_temporal_plot, CHLA_temporal_plot, cols=4)
#dev.off()

#write.csv(SECCHIdata.uniquelakes2, "Secchi_TemporalData_20JUN2018.csv")
#write.csv(CHLAdata.uniquelakes2, "CHLA_TemporalData_20JUN2018.csv")
#write.csv(TPdata.uniquelakes2, "TP_TemporalData_20JUN2018.csv")

## Count number of lakes in each year group
SECCHI_temporal_data1 %>% 
  group_by(yeargroup, category) %>%
  tally()

CHLA_temporal_data1 %>% 
  group_by(yeargroup, category) %>%
  tally()

TP_temporal_data1 %>% 
  group_by(yeargroup, category) %>%
  tally()

TN_temporal_data1 %>% 
  group_by(yeargroup, category) %>%
  tally()


###############
### Q3 BIAS ###
###############

# extract data from various LAGOS datasets to combine into single table

# LAKE SIZE 
lakearea_df <- data.frame(lagoslakeid = dt$iws$lagoslakeid, lakearea_ha = dt$iws$iws_lakeareaha)

# FOREST & AGRICULTURE
# total forest <- evergreen + deciduous + mixed
# ag <- pature 81 + row crops 82
lulc_df <- data.frame(lagoslakeid = dt$iws.lulc$lagoslakeid, total_ag_2006_pct = dt$iws.lulc$iws_nlcd2006_pct_81 + dt$iws.lulc$iws_nlcd2006_pct_82 ,
                      total_forest_2006_pct = dt$iws.lulc$iws_nlcd2006_pct_41 + dt$iws.lulc$iws_nlcd2006_pct_42 + dt$iws.lulc$iws_nlcd2006_pct_43)

# RESIDENTIAL DEVELOPMENT 
# development gradient: used residential, high & low intensity (NLCD 2006)
resident_develop_df <- data.frame(lagoslakeid = dt$buffer100m.lulc$lagoslakeid, resident_dvlp = dt$buffer100m.lulc$buffer100m_nlcd2006_pct_21 
                                  + dt$buffer100m.lulc$buffer100m_nlcd2006_pct_22)

# WETLANDS 
wetlands_df <- data.frame(lagoslakeid = dt$iws.conn$lagoslakeid, all_wetland_dissolved_pct = dt$iws.conn$iws_wl_allwetlandsdissolved_overlapping_area_pct)

# ADD STATE ZONE ID
locus_df <- data.frame(lagoslakeid = dt$locus$lagoslakeid, state_zoneid = dt$locus$state_zoneid) 

#link program information to lagoslakeid.
# both of these tables (nutri.data.years & secchi.data.years) contain all the lakeids that are sample for tp, chla and secchi
TP_lakeids <- select(TP_SOI, lagoslakeid, programname, category, state)
CHLA_lakeids <- select(CHLA_SOI, lagoslakeid, programname, category, state)
SECCHI_lakeids <- select(SECCHI_SOI, lagoslakeid, programname, category, state)
TN_lakeids <- select(TN_SOI, lagoslakeid, programname, category, state)

lakeids <- bind_rows(TP_lakeids, TN_lakeids, CHLA_lakeids, SECCHI_lakeids)
unique_lakeids <- lakeids[!duplicated(lakeids[c("lagoslakeid", "category")] ), ]

# create data frame of all lakes for gradients defined above
all_lakes_bias_df = left_join(locus_df, lakearea_df, by='lagoslakeid')
all_lakes_bias_df = left_join(all_lakes_bias_df, lulc_df, by='lagoslakeid')
all_lakes_bias_df = left_join(all_lakes_bias_df, resident_develop_df, by='lagoslakeid')
all_lakes_bias_df = left_join(all_lakes_bias_df, wetlands_df, by='lagoslakeid')

# keeping only complete cases...this is perhaps not ideal, as may be true NAs for some of the freshwater variables
all_lakes_bias_df2 <- all_lakes_bias_df[complete.cases(all_lakes_bias_df), ]

# correlation analysis
correlation_lakes <- cor(all_lakes_bias_df2[,c(3:7)])
# forest and ag = -.63 

#create a lake bias for the lakes with samples and also include program information
sampled_lakes_bias_df <- left_join(unique_lakeids, all_lakes_bias_df, by = 'lagoslakeid')
#find number of lakes sampled by both programs
test <- sampled_lakes_bias_df %>% 
  group_by(lagoslakeid) %>% 
  filter(n()>1)
#4376

#bind sampled lakes and all lakes into one big table for plotting
all_lakes_bias_df$category <- "All lakes"
all_lakes_bias_df$programname <- NA
all_lakes_bias_df$state <- NA

#    State_3 = MI
#   State_12 = IN
#    State_8 = RI
#    State_5 = NY
#   State_14 = MN
#   State_16 = NH
#   State_15 = MO

#add state name to the all lakes df and then remove the states we are not interested in
all_lakes_bias_df$state <-  ifelse(all_lakes_bias_df$state_zoneid == "State_3", "Michigan",
                                   ifelse(all_lakes_bias_df$state_zoneid == "State_12", "Indiana",
                                          ifelse(all_lakes_bias_df$state_zoneid == "State_8", "Rhode Island",
                                                 ifelse(all_lakes_bias_df$state_zoneid == "State_14", "Minnesota",
                                                        ifelse(all_lakes_bias_df$state_zoneid == "State_5", "New York",
                                                               ifelse(all_lakes_bias_df$state_zoneid == "State_16", "New Hampshire",
                                                                      ifelse(all_lakes_bias_df$state_zoneid == "State_15", "Missouri",NA))))))) 

SOI_lakes_bias_df <- all_lakes_bias_df[!(is.na(all_lakes_bias_df$state)),]
SOI_lakes_bias_df$secchi.avg <- NA

#get secchi average values and add to the sampled lakes df
allsecchi.year.avg <- aggregate(SECCHI_SOI$secchi, by=list(SECCHI_SOI$lagoslakeid, SECCHI_SOI$sampleyear), FUN=mean)
allsecchi.avg <- aggregate(allsecchi.year.avg$x, by=list(allsecchi.year.avg$Group.1), FUN=mean)

#rename columns 
names(allsecchi.avg) <- c("lagoslakeid", "secchi.avg")
sampled_lakes_bias_df <- left_join(sampled_lakes_bias_df, allsecchi.avg, by = 'lagoslakeid')

#bind the sampled lakes with the lakes from States of Interest 
bind_sampled_SOI_lakes <- rbind(sampled_lakes_bias_df, SOI_lakes_bias_df)

## CODES ##
#lake_size = 'lakearea_ha' #lake size
#forest_pct = 'total_forest_2006_pct'  #% forest
#road_dens = 'roaddensity_mperha' #road density
#urban_pct = 'resident_dvlp' #% urban
#wetland_pct = 'all_wetland_dissolved_pct'#%wetland 

###################### Box Plots #########################

color_vector <- c("#00BA38", "#F8766D", "#00BFC4")
box_labels <- c('All lakes','Citizen','Non')

# LAKE SIZE (log transformed)
allstates_lake_area <- ggplot(bind_sampled_SOI_lakes, aes(x = category, y = log(lakearea_ha))) + 
  geom_boxplot(aes(fill=category, group=category)) + 
  ggtitle("(a) Lake size") +
  labs(x = "", y = "log (Lake area (ha))") +
  scale_y_continuous(limits = c(0, 12)) +
  labs(fill = "Sampling Group") + 
  theme(legend.position="none") + 
  theme(text = element_text(size=8)) +
  scale_fill_manual(values=c(color_vector[1], color_vector[2], color_vector[3]))+
  theme(plot.margin = unit(c(0.1,0.1,-0.2,0.1), "cm")) +
  scale_x_discrete(labels=box_labels) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                             panel.background = element_blank(), axis.line = element_line(colour = "black"))

# RESIDENTIAL DEVELOPMENT
allstates_resident_dvlp <- ggplot(bind_sampled_SOI_lakes, aes(x = category, y = resident_dvlp)) + 
  geom_boxplot(aes(fill=category, group=category)) + 
  ggtitle("(c) Residential development") + 
  labs(x = "", y = "Residential development (%)") + 
  scale_y_continuous(limits = c(0, 50)) + 
  theme(legend.position="none")+ 
  theme(text = element_text(size=8)) +
  scale_fill_manual(values=c(color_vector[1], color_vector[2], color_vector[3]))+
  theme(plot.margin = unit(c(0.1,0.1,-0.2,0.1), "cm")) +
  scale_x_discrete(labels=box_labels)+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                             panel.background = element_blank(), axis.line = element_line(colour = "black"))

# AGRICULTURE
allstates_ag <- ggplot(bind_sampled_SOI_lakes, aes(x = category, y = total_ag_2006_pct)) + 
  geom_boxplot(aes(fill=category, group=category)) + 
  ggtitle("(e) Agriculture") + 
  labs(x = "", y = "Agriculture (%)") + 
  scale_y_continuous(limits = c(0, 100)) +
  labs(fill = "Sampling Group") + 
  theme(legend.position="none") + 
  theme(text = element_text(size=8)) +
  scale_fill_manual(values=c(color_vector[1], color_vector[2], color_vector[3]))+
  theme(plot.margin = unit(c(0.1,0.1,-0.2,0.1), "cm")) +
  scale_x_discrete(labels=box_labels)+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                             panel.background = element_blank(), axis.line = element_line(colour = "black"))

# PERCENT FOREST
allstates_percent_forest <- ggplot(bind_sampled_SOI_lakes, aes(x = category, y = total_forest_2006_pct)) + 
  geom_boxplot(aes(fill=category, group=category)) + 
  ggtitle("(b) Forest") + 
  labs(x = "", y = "Forest (%)") + 
  labs(fill = "Sampling Group") +
  scale_y_continuous(limits = c(0, 100))+ 
  theme(legend.position="none")+ 
  theme(text = element_text(size=8)) +
  scale_fill_manual(values=c(color_vector[1], color_vector[2], color_vector[3]))+
  theme(plot.margin = unit(c(0.1,0.1,-0.2,0.1), "cm")) +
  scale_x_discrete(labels=box_labels)+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                             panel.background = element_blank(), axis.line = element_line(colour = "black"))

# PERCENT WETLAND
allstates_percent_wetland <- ggplot(bind_sampled_SOI_lakes, aes(x = category, y = all_wetland_dissolved_pct)) + 
  geom_boxplot(aes(fill=category, group=category)) + 
  ggtitle("(d) Wetland") + 
  labs(x = "", y = "Wetland (%)") + 
  labs(fill = "Sampling Group") +
  scale_y_continuous(limits = c(0, 25))+ 
  theme(legend.position="none")+ 
  theme(text = element_text(size=8)) +
  scale_fill_manual(values=c(color_vector[1], color_vector[2], color_vector[3]))+
  theme(plot.margin = unit(c(0.1,0.1,-0.2,0.1), "cm")) +
  scale_x_discrete(labels=box_labels)+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                             panel.background = element_blank(), axis.line = element_line(colour = "black"))

# SECCHI BIAS
allstates_secchi_bias_plot <- ggplot(bind_sampled_SOI_lakes, aes(x = category, y = secchi.avg)) + 
  geom_boxplot(aes(fill=category, group=category)) + 
  ggtitle("(f) Lake clarity") + 
  labs(x = "", y = "Secchi depth (m)") + 
  labs(fill = "Sampling Group") + 
  scale_y_continuous(limits = c(0, 7))+
  theme(legend.position="none")+ 
  theme(text = element_text(size=8)) +
  scale_fill_manual(values=c(color_vector[2], color_vector[3]))+
  theme(plot.margin = unit(c(0.1,0.1,-0.2,0.1), "cm")) +
  scale_x_discrete(labels=box_labels)+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                             panel.background = element_blank(), axis.line = element_line(colour = "black"))

#CairoPDF(file="BoxPlot.pdf", width=11, height=8.5, family="Helvetica", pointsize=12)
multiplot(allstates_lake_area,
          allstates_resident_dvlp,
          allstates_ag,
          allstates_percent_forest,
          allstates_percent_wetland,cols=2)
png(filename='ExportedFigures/Fig4BoxPlots.png', width=4.5, height=4.5, units='in', res=300)
multiplot(allstates_lake_area,allstates_resident_dvlp,allstates_ag,allstates_percent_forest,allstates_percent_wetland,cols=2)
dev.off()

##########
# T-test #
##########

all_pairwise_df <- data.frame(col1=c("a","b"))
for (i in 6:11) {
  temp <- pairwise.t.test(bind_sampled_SOI_lakes[,i], bind_sampled_SOI_lakes$category, p.adjust.method = 'bonf', alternative = "two.sided")
  temp_df <- as.data.frame(temp$p.value)
  all_pairwise_df <- cbind(all_pairwise_df, temp_df)
  temp <-NULL
  temp_df <-NULL
}
#trying log transformation of lake area
pairwise.t.test(log(bind_sampled_SOI_lakes$lakearea_ha), bind_sampled_SOI_lakes$category, p.adjust.method = 'bonf', alternative = "two.sided")

col_vec <- c('Area_ALL','Area_CM','AG_ALL','AG_CM','Forest_ALL','Forest_CM','Res_ALL','Res_CM','Wetland_ALL','Wetland_CM', 'Secchi_ALL', 'Secchi_CM')
all_pairwise_df[,1] = NULL
colnames(all_pairwise_df) <- col_vec
