#       Higher Diploma in Data Analytics
#         Final Year Project
#           26th April 2021
#
# Student name: Kieran Sexton
#
#
###########################################

# install packages
#install.packages('RMySQL', dependencies = TRUE)  
#install.packages('dplyr', dependencies = TRUE)
#install.packages('corrplot', dependencies = TRUE)
#install.packages('ggplot2', dependencies = TRUE)
#install.packages('tidyr', dependencies = TRUE)
#install.packages('caret', dependencies = TRUE)
#install.packages('e1071', dependencies = TRUE)
#install.packages('randomForest', dependencies = TRUE)
#install.packages('stringr', dependencies = TRUE)
#install.packages('pROC', dependencies = TRUE)
#install.packages('tidyverse', dependencies = TRUE)
#install.packages('reshape2', dependencies = TRUE)
#install.packages('multiROC', dependencies = TRUE)
#install.packages('dummies', dependencies = TRUE)
#install.packages('gridExtra', dependencies = TRUE)

# source packages
library(RMySQL)
library(dplyr)
library(corrplot)
library(ggplot2)
library(tidyr)
library(caret)
library(e1071)
library(randomForest)
library(stringr)
library(pROC)
library(tidyverse)  
library(reshape2)
library(multiROC)
library(dummies)
library(gridExtra)

# set working dir
setwd("")

# read in all 964K rows and do initial exploratory analysis
fullBER <- as.data.frame(read.csv("BERPublicsearch.csv", header=TRUE, sep=","))
str(fullBER)
dim(fullBER)

# drop the attributes with characters that are problematic for MYSQL (these attributes are irrelevant to the analysis)
fullBER = subset(fullBER, select = -c(PredominantRoofType,FirstWallDescription,FirstEnerProdComment))

# check the percentage of each rating
fullBER %>%
  group_by(EnergyRating) %>% 
  summarise(Number = n()) %>%
  mutate(Percent = prop.table(Number)*100) 

# plot of the rating distribution
fullBER %>%
  group_by(EnergyRating) %>% 
  summarise(Number = n()) %>%
  mutate(Percent = prop.table(Number)*100) %>% 
  ggplot(aes(EnergyRating, Percent)) + 
  geom_col(aes(fill = EnergyRating)) +
  labs(title = "BERCategory Percentage") +
  geom_text(aes(label = sprintf("%.1f%%", Percent)), vjust = -0.5)

#*************************************************************************************************************
# 1. Generate data for trend and statistical analysis (Tableau and SPSS)
# rename all Dublin suburbs to Dublin City
fullBER$CountyName <- gsub("Dublin \\d+$", "Dublin City", fullBER$CountyName)  
fullBER$CountyName <- gsub("Dublin 6W", "Dublin City", fullBER$CountyName)  
# fix the dwellings to collate simiilar types and reduce the comparisons                      
fullBER$DwellingTypeDescr <- recode(fullBER$DwellingTypeDescr,
                                    "Mid-floor apartment" = "Apartment",
                                    "Top-floor apartment" = "Apartment",
                                    "Ground-floor apartment" = "Apartment",
                                    "Mid-terrace house" = "Terraced House",
                                    "End of terrace house" = "Terraced House"
)                           

# aggregate data for Tableau  
dataBERSPSS <- filter(fullBER, grepl("2020|2018|2016|2014|2012|2010|2019|2017|2015|2013|2011", Year_of_Construction)) %>%  select(BerRating,EnergyRating,CO2Rating,CountyName,DwellingTypeDescr,Year_of_Construction)
# subset for 2020 for SPSS
dataBERSPSS2020 <- subset(dataBERSPSS, Year_of_Construction=="2020")

#*************************************************************************************************************
# 2. Subset for assessments in 2020 as the most recent year evaluated against the current DEAP version (Nov 2019)
assesedBER2020 <- filter(fullBER, grepl("2020", DateOfAssessment, fixed = TRUE))
dim(assesedBER2020)

#examine - reflects all of the classes
assesedBER2020 %>% 
  group_by(EnergyRating) %>% 
  summarise(Number = n()) %>%
  mutate(Percent = prop.table(Number)*100) 

# plot it
assesedBER2020 %>%
  group_by(EnergyRating) %>% 
  summarise(Number = n()) %>%
  mutate(Percent = prop.table(Number)*100) %>% 
  ggplot(aes(EnergyRating, Percent)) + 
  geom_col(aes(fill = EnergyRating)) +
  labs(title = "BERCategory Percentage") +
  geom_text(aes(label = sprintf("%.1f%%", Percent)), vjust = -0.5)

# run statistical tests to compare this sample with full data set
# first test for normality - non normal 
ks.test(fullBER$BerRating, pnorm)
ks.test(assesedBER2020$BerRating, pnorm)

# apply non parametric test 
# assessedBER2020 BERmeasure can be said to be statistically significantly different to the full data population at the 5% level of significance
wilcox.test(fullBER$BerRating, assesedBER2020$BerRating)

#*************************************************************************************************************
# 3. Descriptive Statistics on the working data sample
summary(assesedBER2020)
str (assesedBER2020)

mean(assesedBER2020$BerRating)
median(assesedBER2020$BerRating)
var(assesedBER2020$BerRating)
sd(assesedBER2020$BerRating)

# box plot - lot of outliers (positively skewed distribution)
boxplot(assesedBER2020$BerRating, names = c("2020"), horizontal = TRUE, col = "red", xlab="BER Value", main = "BER distribution 2020 assessments")

# QQ Plot - check normal distribution
qqnorm(assesedBER2020$BerRating, col= "blue", main = "2020 Assessed BER")
qqline(assesedBER2020$BerRating, col="red")
#
min(assesedBER2020$BerRating)
max(assesedBER2020$BerRating)

#examine outliers
assesedBER2020[assesedBER2020$BerRating<0,]
assesedBER2020[assesedBER2020$BerRating>2000,]

# write the files for back up 
write.csv(assesedBER2020, "assesedBER2020.csv", row.names=FALSE)

# Write to MySQL database 
# *** SET GLOBAL local_infile = true; **** on MySQL prior to write
con_schema <- dbConnect(MySQL(),
                        user = 'root',
                        password = 'Project1',  # this should be in an encrypted file for product
                        host = 'localhost',
                        dbname='seai_ber_schema')

# one column is problematic to query so rename before writing
colnames(assesedBER2020)[colnames(assesedBER2020) == "GroundFloorArea.sq.m."] <- "TotalFloorArea"
names(assesedBER2020)

# create the table and load the data
dbWriteTable(conn = con_schema, name = 'assesedBER2020_Full_Schema', value = assesedBER2020, row.names=FALSE, overwrite=TRUE, sep =',')

#*************************************************************************************************************
# 4. Now Create the DataWareHouse in the MySQL Client 
#    In the Datawarehouse, create the 'retrofit' DataMart using the SQL supplied with this project
#*************************************************************************************************************

#*************************************************************************************************************
# 5. change the DB connection to the DWH and read in the retrofit datamart 
con_datamart <- dbConnect(MySQL(),
                          user = 'root',
                          password = 'Project1',  # this should be in an encrypted file for product
                          host = 'localhost',
                          dbname='projectdatamart')

# read in the DataMart for retrofit (53 retrofit attributes)
retrofitBER <- dbReadTable(conn = con_datamart, "retrofitdatamart") 
str(retrofitBER)

# save it as csv for backup
write.csv(retrofitBER, "retrofit_BER.csv", row.names=FALSE)

# Write the DataMarts for SPSS and Tableau 
dbWriteTable(conn = con_datamart, name = 'dataBERSPSS', value = dataBERSPSS, row.names=FALSE, overwrite=TRUE, sep =',')
dbWriteTable(conn = con_datamart, name = 'dataBERSPSS2020', value = dataBERSPSS2020, row.names=FALSE, overwrite=TRUE, sep =',')
# save for back up
write.csv(dataBERSPSS, "dataBERSPSS.csv", row.names=FALSE)
write.csv(dataBERSPSS2020, "dataBERSPSS2020.csv", row.names=FALSE)

#*************************************************************************************************************
# 6. read in from the csv file in case of DB access issues or running from another machine
retrofitBER <- as.data.frame(read.csv("retrofit_BER.csv", header=TRUE, sep=",", strip.white= TRUE, stringsAsFactors = FALSE, na.strings=c("","NA")))

#  remove all leading and trailing blanks
retrofitBER <- retrofitBER %>%
  mutate_if(is.character, str_trim)

# Need to set BERCategory as categorical
retrofitBER$BERCategory = as.factor(retrofitBER$BERCategory)

# define a mode function for imputing data for nominal NA variables
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#*************************************************************************************************************
# 7. BER Rating distribution plot - classes are imbalanced 
group_by(BERCategory) %>% 
  summarise(Number = n()) %>%
  mutate(Percent = prop.table(Number)*100) %>% 
  ggplot(aes(BERCategory, Percent)) + 
  geom_col(aes(fill = BERCategory)) +
  labs(title = "BERCategory Percentage") +
  geom_text(aes(label = sprintf("%.1f%%", Percent)), vjust = -0.5)

retrofitBER %>%
  group_by(BERCategory) %>% 
  summarise(Number = n()) %>%
  mutate(Percent = prop.table(Number)*100) 

#*************************************************************************************************************
# 8. Data exploration - examine distribution for a selection of insulation and heating variables
r1 <- ggplot(data = retrofitBER) + geom_histogram(aes(x = YearBuilt), bins = 50, colour = "black", fill = "red") + labs(y = "Number of Properties")
min(retrofitBER$YearBuilt)
max(retrofitBER$YearBuilt)
#examine outliers
retrofitBER[retrofitBER$YearBuilt>2021,]

#
r2 <-  ggplot(data = retrofitBER) + geom_histogram(aes(x = TotalFloorArea), bins = 30, colour = "black", fill = "red") + labs(y = "Number of Properties")
min(retrofitBER$TotalFloorArea)
max(retrofitBER$TotalFloorArea)
#
r3 <-  ggplot(data = retrofitBER) + geom_histogram(aes(x = UValueWindow), bins = 20, colour = "black", fill = "red") + labs(y = "Number of Properties")
min(retrofitBER$UValueWindow)
max(retrofitBER$UValueWindow)
#
r4 <-  ggplot(data = retrofitBER) + geom_histogram(aes(x = DistributionLosses), bins = 30, colour = "black", fill = "red") + labs(y = "Number of Properties")
min(retrofitBER$DistributionLosses)
max(retrofitBER$DistributionLosses)
#
r5 <-  ggplot(data = retrofitBER) + geom_histogram(aes(x = PrimaryEnergyMainSpace), bins = 30, colour = "black", fill = "red") + labs(y = "Number of Properties")
#
r6 <-  ggplot(data = retrofitBER) + geom_histogram(aes(x = CO2MainSpace), bins = 30, colour = "black", fill = "red") + labs(y = "Number of Properties")
#
grid.arrange(r1, r2, r3, r4, r5,r6, top = "Retrofit variable distribution", ncol=2, nrow =3)


#*************************************************************************************************************
# 9. Update variable formats e.g. discrete to categorical
str(retrofitBER)
# start with binning year by examining distribution and defining breaks
sort(unique(retrofitBER$YearBuilt))
# drop the rows where the year is greater than 2020 (as this is data assessed in could not look into the future)
retrofitBER<-subset(retrofitBER[retrofitBER$YearBuilt<2021,],)
# cut and label
retrofitBER$YearBuilt = cut(retrofitBER$YearBuilt, breaks= c(1750, 1800, 1850, 1890, 1900, 1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1985, 1990, 1995, 1997, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021), ordered_result=TRUE) 

#*************************************************************************************************************
# 10. Label cleaning, NA handling including imputation and feature selection 
sapply(retrofitBER, function(x) sum(is.na(x)))

# Property Type: different percentage for different classes so it does have an impact - keep it
retrofitBER %>% 
  group_by(BERCategory,Type) %>% 
  tally() %>% 
  complete(Type, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(Type, percentage, fill = BERCategory)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()
# names are ok and no NAs
unique(retrofitBER[c("Type")])

# Lagging Jacket and Insulation Thickness - Very few rows populated (76K NAs) - remove both of these
retrofitBER %>% 
  group_by(BERCategory,LaggingJacketType) %>% 
  tally() %>% 
  complete(LaggingJacketType, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(LaggingJacketType, percentage, fill = BERCategory)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()

# Type of Heating system: different percentage per classes so possible impact on BER - keep this
retrofitBER %>% 
  group_by(BERCategory,HeatType) %>% 
  tally() %>% 
  complete(HeatType, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(HeatType, percentage, fill = BERCategory)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()

# Fix the strings and drop the NAs as ther is no way to impute the heat type - 768 rows
unique(retrofitBER[c("HeatType")])
retrofitBER$HeatType[retrofitBER$HeatType == "Bulk LPG (propane or butane)"] <- "Bulk LPG"
retrofitBER$HeatType[retrofitBER$HeatType == "Electricity - On-peak Night-Ra"] <- "Electricity"
retrofitBER$HeatType[retrofitBER$HeatType == "Electricity - Off-peak Night-R"] <- "Electricity"
retrofitBER$HeatType[retrofitBER$HeatType == "Electricity - Standard Domesti"] <- "Electricity"
retrofitBER$HeatType[retrofitBER$HeatType == "Solid Multi-Fuel"] <- "Multi-Fuel"
retrofitBER$HeatType[retrofitBER$HeatType == "Wood Pellets (bulk supply for"] <- "Wood Pellets"
retrofitBER$HeatType[retrofitBER$HeatType == "Solid Multi-Fuel"] <- "Multi-Fuel"
retrofitBER$HeatType[retrofitBER$HeatType == "Manufactured Smokeless Fuel"] <- "Smokeless"
retrofitBER$HeatType[retrofitBER$HeatType == "Wood Pellets (in bags for seco"] <- "Wood Pellets"
retrofitBER$HeatType[retrofitBER$HeatType == "Peat Briquettes"] <- "Briquettes"
retrofitBER <- retrofitBER %>% 
  filter_at(vars(HeatType), any_vars(!is.na(.)))

# Type of Water Heating system: different percentage for different classes so it does have an impact- keep it
retrofitBER %>% 
  group_by(BERCategory,WaterHeatType) %>% 
  tally() %>% 
  complete(WaterHeatType, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(WaterHeatType, percentage, fill = BERCategory)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()

# water heating - Fix the strings as per heat type and no need to drop the NAs as they were the same as the Heat type NAs
unique(retrofitBER[c("WaterHeatType")])
retrofitBER$WaterHeatType[retrofitBER$WaterHeatType == "Bulk LPG (propane or butane)"] <- "Bulk LPG"
retrofitBER$WaterHeatType[retrofitBER$WaterHeatType == "Electricity - On-peak Night-Ra"] <- "Electricity"
retrofitBER$WaterHeatType[retrofitBER$WaterHeatType == "Electricity - Off-peak Night-R"] <- "Electricity"
retrofitBER$WaterHeatType[retrofitBER$WaterHeatType == "Electricity - Standard Domesti"] <- "Electricity"
retrofitBER$WaterHeatType[retrofitBER$WaterHeatType == "Wood Pellets (in bags for seco"] <- "Wood Pellets"
retrofitBER$WaterHeatType[retrofitBER$WaterHeatType == "Wood Pellets (bulk supply for"] <- "Wood Pellets"
retrofitBER$WaterHeatType[retrofitBER$WaterHeatType == "Manufactured Smokeless Fuel"] <- "Smokeless"
retrofitBER$WaterHeatType[retrofitBER$WaterHeatType == "Biodiesel from renewable sourc"] <- "BioDeisel"
retrofitBER$WaterHeatType[retrofitBER$WaterHeatType == "Solid Multi-Fuel"] <- "Multi-Fuel"
retrofitBER$WaterHeatType[retrofitBER$WaterHeatType == "Peat Briquettes"] <- "Briquettes"

# Sealed Porch: different percentage for different classes so it does have an impact- keep it
retrofitBER %>% 
  group_by(BERCategory,SealedPorch) %>% 
  tally() %>% 
  complete(SealedPorch, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(SealedPorch, percentage, fill = BERCategory)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  xlab(expression(paste(bold('Sealed Porch')))) +
  ylab(expression(paste(bold('Percentage')))) +
  theme(axis.title.x = element_text(face="bold", size=12),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=14)) +
  theme(axis.title.y = element_text(face="bold", size=12),
        axis.text.y  = element_text(angle=90, vjust=0.5, size=16)) 

# Porch - 768 rows and no way to determine if they have or have not
unique(retrofitBER[c("SealedPorch")])
retrofitBER <- retrofitBER %>% filter_at(vars(SealedPorch), any_vars(!is.na(.)))

# House Structure: different percentage for different classes so it does have an impact- keep it
retrofitBER %>% 
  group_by(BERCategory,StructureType) %>% 
  tally() %>% 
  complete(StructureType, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(StructureType, percentage, fill = BERCategory)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()

# Structure - 13K 'Please Select' rows (not filled in). 
# All but 2 of the A1s contain 'please select' so this causes a problem in the model training
# need to impute - nominal variable so impute the mode (masonry)
unique(retrofitBER[c("StructureType")])
nrow(retrofitBER[retrofitBER$StructureType == "Please select",])
# seems to be no pattern to the structure type in relation to the BER category so use the mode
Mode(retrofitBER$StructureType)
retrofitBER$StructureType[retrofitBER$StructureType  == "Please select"]  <- Mode(retrofitBER$StructureType)
# fix the names
retrofitBER$StructureType[retrofitBER$StructureType == "Timber or Steel Frame"] <- "Timber or Steel"
retrofitBER$StructureType[retrofitBER$StructureType == "Insulated Conctete Form"] <- "Insulated Concrete"

#Flueless Gas Fires: very little difference between the classes for yes or no - remove it 
retrofitBER %>% 
  group_by(BERCategory,NoOfFluelessGasFires) %>% 
  tally() %>% 
  complete(NoOfFluelessGasFires, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(NoOfFluelessGasFires, percentage, fill = BERCategory)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()

# ventilation : keep it
retrofitBER %>% 
  group_by(BERCategory,VentilationMethod ) %>% 
  tally() %>% 
  complete(VentilationMethod , fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(VentilationMethod , percentage, fill = BERCategory)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()

# ventilation method - rename categories, no NA to handle
unique(retrofitBER[c("VentilationMethod")])
retrofitBER$VentilationMethod[retrofitBER$VentilationMethod == "Natural vent."] <- "Natural"
retrofitBER$VentilationMethod[retrofitBER$VentilationMethod == "Whole house extract vent."] <- "Whole house"
retrofitBER$VentilationMethod[retrofitBER$VentilationMethod == "Bal.whole mech.vent no heat re"] <- "No heat recvr"
retrofitBER$VentilationMethod[retrofitBER$VentilationMethod == "Bal.whole mech.vent heat recvr"] <- "Heat recvr"
retrofitBER$VentilationMethod[retrofitBER$VentilationMethod == "Pos input vent.- outside"] <- "Vent outside"
retrofitBER$VentilationMethod[retrofitBER$VentilationMethod == "Pos input vent.- loft"] <- "Vent loft"

# Suspended wooden floor: keep it
retrofitBER %>% 
  group_by(BERCategory,SuspendedWoodenFloor ) %>% 
  tally() %>% 
  complete(SuspendedWoodenFloor , fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(SuspendedWoodenFloor , percentage, fill = BERCategory)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()

# wooden floor - rename categories
unique(retrofitBER[c("SuspendedWoodenFloor")])
retrofitBER$SuspendedWoodenFloor[retrofitBER$SuspendedWoodenFloor == "Yes (Unsealed)"] <- "UnSealed"
retrofitBER$SuspendedWoodenFloor[retrofitBER$SuspendedWoodenFloor == "Yes (Sealed)"] <- "Sealed"

# ThermoControledBoiler, impact on the ratings: keep it
retrofitBER %>% 
  group_by(BERCategory,ThermoControledBoiler) %>% 
  tally() %>% 
  complete(ThermoControledBoiler            , fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(ThermoControledBoiler, percentage, fill = BERCategory)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()

# OilBoilerThermo- keep it
retrofitBER %>% 
  group_by(BERCategory,OilBoilerThermo) %>% 
  tally() %>% 
  complete(OilBoilerThermo, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(OilBoilerThermo, percentage, fill = BERCategory)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()

# OilBoilerInside- very little impact on the ratings - remove it
retrofitBER %>% 
  group_by(BERCategory,OilBoilerInside) %>% 
  tally() %>% 
  complete(OilBoilerInside, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(OilBoilerInside, percentage, fill = BERCategory)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()

# check the values for the boilers
unique(retrofitBER[c("ThermoControledBoiler")])
unique(retrofitBER[c("OilBoilerThermo")])

# ThermalMassCategory - big impact - keep it
retrofitBER %>% 
  group_by(BERCategory,ThermalMassCategory) %>% 
  tally() %>% 
  complete(ThermalMassCategory, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(ThermalMassCategory, percentage, fill = BERCategory)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()

# WaterStorageVolume - only 9 populated rows and no way to impute what the values might be so - remove it    
unique(retrofitBER[c("WaterStorageVolume")])

# Type of Warm Air Heating: very little difference between the classes for yes or no - remove it
retrofitBER %>% 
  group_by(BERCategory,WarmAirHeatingSystem) %>% 
  tally() %>% 
  complete(WarmAirHeatingSystem, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(WarmAirHeatingSystem, percentage, fill = BERCategory)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  xlab(expression(paste(bold('Warm Air Heating System')))) +
  theme(axis.title.x = element_text(face="bold", size=12),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=14)) +
  theme(axis.title.y = element_text(face="bold", size=12),
        axis.text.y  = element_text(angle=90, vjust=0.5, size=16)) 

# NoStoreys: keep it
retrofitBER %>% 
  group_by(BERCategory,NoStoreys) %>% 
  tally() %>% 
  complete(NoStoreys, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(NoStoreys, percentage, fill = BERCategory)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()

# NoOfChimneys: keep it
retrofitBER %>% 
  group_by(BERCategory,NoOfChimneys) %>% 
  tally() %>% 
  complete(NoOfChimneys, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(NoOfChimneys, percentage, fill = BERCategory)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()

# NoOfOpenFlues: keep it
retrofitBER %>% 
  group_by(BERCategory,NoOfOpenFlues) %>% 
  tally() %>% 
  complete(NoOfOpenFlues, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(NoOfOpenFlues, percentage, fill = BERCategory)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()

# NoOfFansAndVents: keep it
retrofitBER %>% 
  group_by(BERCategory,NoOfFansAndVents) %>% 
  tally() %>% 
  complete(NoOfFansAndVents, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(NoOfFansAndVents, percentage, fill = BERCategory)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()

# NoOfSidesSheltered: keep it
retrofitBER %>% 
  group_by(BERCategory,NoOfSidesSheltered) %>% 
  tally() %>% 
  complete(NoOfSidesSheltered, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(NoOfSidesSheltered, percentage, fill = BERCategory)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()

# NoOilBoilerHeatingPumps: keep it
retrofitBER %>% 
  group_by(BERCategory,NoOilBoilerHeatingPumps) %>% 
  tally() %>% 
  complete(NoOilBoilerHeatingPumps, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(NoOilBoilerHeatingPumps, percentage, fill = BERCategory)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()

# NoGasBoilerHeatingPumps: keep it
retrofitBER %>% 
  group_by(BERCategory,NoGasBoilerHeatingPumps) %>% 
  tally() %>% 
  complete(NoGasBoilerHeatingPumps, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(NoGasBoilerHeatingPumps, percentage, fill = BERCategory)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()

# NoCentralHeatingPumps: keep it
retrofitBER %>% 
  group_by(BERCategory,NoCentralHeatingPumps) %>% 
  tally() %>% 
  complete(NoCentralHeatingPumps, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(NoCentralHeatingPumps, percentage, fill = BERCategory)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()

# Remove the categorical columns based on the above analysis
retrofitBER <- retrofitBER %>%
  select(
    -LaggingJacketType, -InsulationThickness, -NoOfFluelessGasFires, -OilBoilerInside, -WaterStorageVolume, -WarmAirHeatingSystem)

# check all NAs have been handled
sapply(retrofitBER, function(x) sum(is.na(x)))

# correct the data type from character to factor
retrofitBER$HeatType = as.factor(retrofitBER$HeatType)
retrofitBER$WaterHeatType = as.factor(retrofitBER$WaterHeatType)
retrofitBER$SealedPorch= as.factor(retrofitBER$SealedPorch)
retrofitBER$StructureType= as.factor(retrofitBER$StructureType)
retrofitBER$VentilationMethod= as.factor(retrofitBER$VentilationMethod)
retrofitBER$SuspendedWoodenFloor= as.factor(retrofitBER$SuspendedWoodenFloor)
retrofitBER$Type= as.factor(retrofitBER$Type)
retrofitBER$ThermoControledBoiler= as.factor(retrofitBER$ThermoControledBoiler)
retrofitBER$ThermalMassCategory= as.factor(retrofitBER$ThermalMassCategory)
retrofitBER$OilBoilerThermo = as.factor(retrofitBER$OilBoilerThermo)
str(retrofitBER)
#*************************************************************************************************************
# 11. examine numeric data relationship to BERCateogry 
numeric_Test <- retrofitBER %>%
  select_if(is.numeric) 
#
str(numeric_Test)
#
corMat <- cor(numeric_Test)
corrplot(corMat, type="upper", number.cex = 0.5, number.digits=1, tl.cex	= 0.8, tl.col = "black")

# Check if there is a difference in distribution to help decide which of the correlated variables could be removed

# TotalFloorArea is significantly different for the upper ratings so keep it
b1 <- ggplot(
  data = retrofitBER,
  aes(y = TotalFloorArea, x = BERCategory, color = BERCategory)) +
  theme(legend.position="none") +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=14)) +
  theme(axis.title.y = element_text(face="bold", size=12),
        axis.text.y  = element_text(angle=90, vjust=0.5, size=16)) +
  geom_boxplot()

#------------------------------------------------
# this almost has the same distribution as TotalFloorArea so remove it 
b2<-  ggplot(
  data = retrofitBER,
  aes(y = FloorArea, x = BERCategory, color = BERCategory)) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=14)) +
  theme(axis.title.y = element_text(face="bold", size=12),
        axis.text.y  = element_text(angle=90, vjust=0.5, size=16)) +
  geom_boxplot()

#------------------------------------------------
# different distribution to TotalFloorArea so keep it
b3 <-  ggplot(
  data = retrofitBER,
  aes(y = WallArea, x = BERCategory, color = BERCategory)) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=14)) +
  theme(axis.title.y = element_text(face="bold", size=12),
        axis.text.y  = element_text(angle=90, vjust=0.5, size=16)) +
  geom_boxplot()

#------------------------------------------------
# WindowArea  is significantly different for the upper ratings so keep it
b4 <-  ggplot(
  data = retrofitBER,
  aes(y = WindowArea, x = BERCategory, color = BERCategory)) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=14)) +
  theme(axis.title.y = element_text(face="bold", size=12),
        axis.text.y  = element_text(angle=90, vjust=0.5, size=16)) +
  geom_boxplot()

#------------------------------------------------ 
# very little difference across categories of BER so remove it
b5 <-  ggplot(
  data = retrofitBER,
  aes(y = DoorArea, x = BERCategory, color = BERCategory)) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=14)) +
  theme(axis.title.y = element_text(face="bold", size=12),
        axis.text.y  = element_text(angle=90, vjust=0.5, size=16)) +
  geom_boxplot()

#------------------------------------------------  
# hugely different so keep it
b7<-  ggplot(
  data = retrofitBER,
  aes(y = PercentageDraughtStripped, x = BERCategory, color = BERCategory)) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=14)) +
  theme(axis.title.y = element_text(face="bold", size=12),
        axis.text.y  = element_text(angle=90, vjust=0.5, size=16)) +
  geom_boxplot()

#------------------------------------------------ 
# PrimaryEnergyLighting is significantly different for the upper ratings so keep it
b8 <-  ggplot(
  data = retrofitBER,
  aes(y = PrimaryEnergyLighting, x = BERCategory, color = BERCategory)) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=14)) +
  theme(axis.title.y = element_text(face="bold", size=12),
        axis.text.y  = element_text(angle=90, vjust=0.5, size=16)) +
  geom_boxplot()

#------------------------------------------------ 
# hugely different so keep it
b9 <-  ggplot(
  data = retrofitBER,
  aes(y = DistributionLosses, x = BERCategory, color = BERCategory)) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=14)) +
  theme(axis.title.y = element_text(face="bold", size=12),
        axis.text.y  = element_text(angle=90, vjust=0.5, size=16)) +
  geom_boxplot()

#------------------------------------------------ 
# almost the exact same as PrimaryEnergyLighting so possibly remove it 
b10 <-  ggplot(
  data = retrofitBER,
  aes(y = DeliveredLightingEnergy, x = BERCategory, color = BERCategory)) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=14)) +
  theme(axis.title.y = element_text(face="bold", size=12),
        axis.text.y  = element_text(angle=90, vjust=0.5, size=16)) +
  geom_boxplot()

#------------------------------------------------ 
# almost the exact same as PrimaryEnergyLighting so possibly remove one of them
b11 <-  ggplot(
  data = retrofitBER,
  aes(y = CO2Lighting, x = BERCategory, color = BERCategory)) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=14)) +
  theme(axis.title.y = element_text(face="bold", size=12),
        axis.text.y  = element_text(angle=90, vjust=0.5, size=16)) +
  geom_boxplot()

#------------------------------------------------ 
# hugely different so keep it
b12 <-  ggplot(
  data = retrofitBER,
  aes(y = DeliveredEnergyMainSpace, x = BERCategory, color = BERCategory)) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=14)) +
  theme(axis.title.y = element_text(face="bold", size=12),
        axis.text.y  = element_text(angle=90, vjust=0.5, size=16)) +
  geom_boxplot()

#------------------------------------------------ 
# almost the exact same as DeliveredEnergyMainSpace so possibly remove one of them
b13<-  ggplot(
  data = retrofitBER,
  aes(y = PrimaryEnergyMainSpace, x = BERCategory, color = BERCategory)) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=14)) +
  theme(axis.title.y = element_text(face="bold", size=12),
        axis.text.y  = element_text(angle=90, vjust=0.5, size=16)) +
  geom_boxplot()

#------------------------------------------------ 
# similar but slightly different at the higher ratins so keep it
b14<-  ggplot(
  data = retrofitBER,
  aes(y = CO2MainSpace, x = BERCategory, color = BERCategory)) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=14)) +
  theme(axis.title.y = element_text(face="bold", size=12),
        axis.text.y  = element_text(angle=90, vjust=0.5, size=16)) +
  geom_boxplot()

# plot them all
grid.arrange(b1, b2, b3,b4, b5, ncol=2, nrow =3) 
grid.arrange(b7, b8,b9,b10,b11, ncol=2, nrow =3)
grid.arrange(b12, b13,b14, ncol=2, nrow =2)

#*************************************************************************************************************
# 12. handle the outliers - first find them from the above plots 
# first energy lighting
out <- boxplot.stats(retrofitBER$PrimaryEnergyLighting)$out
out_ind <- which(retrofitBER$PrimaryEnergyLighting %in% c(out))
sort(out_ind, decreasing = TRUE)
# order
temp <- retrofitBER[out_ind, ]
temp[order(temp$PrimaryEnergyLighting, decreasing = TRUE), ]

# floor area
out <- boxplot.stats(retrofitBER$TotalFloorArea)$out
out_ind <- which(retrofitBER$TotalFloorArea %in% c(out))
sort(out_ind, decreasing = TRUE)
# order - these are not that different
temp <- retrofitBER[out_ind, ]
temp[order(temp$TotalFloorArea, decreasing = TRUE), ]

#  window area
out <- boxplot.stats(retrofitBER$WindowArea)$out
out_ind <- which(retrofitBER$WindowArea %in% c(out))
sort(out_ind, decreasing = TRUE)
# order - one specific outlier to remove
temp <- retrofitBER[out_ind, ]
temp[order(temp$WindowArea, decreasing = TRUE), ]

#  Energy main space
out <- boxplot.stats(retrofitBER$DeliveredEnergyMainSpace)$out
out_ind <- which(retrofitBER$DeliveredEnergyMainSpace %in% c(out))
sort(out_ind, decreasing = TRUE)
# order - one specific outlier to remove
temp <- retrofitBER[out_ind, ]
temp[order(temp$DeliveredEnergyMainSpace, decreasing = TRUE), ]

# remove outlier observations -  primary energy and window area 
retrofitBER <- retrofitBER[-c(33742, 15503, 27325, 6382, 63991, 9320, 33273, 41022, 24325), ]

# Re-run the above plots to check 

# Based on this analysis remove these variables based on correlation and matching distributions 
retrofitBER <- retrofitBER %>%
  select(
    -FloorArea, -DoorArea, -CO2Lighting, -DeliveredLightingEnergy)

#*************************************************************************************************************
# 13. write the final file to a DataMart and for backup
# Save and split the data into the retrofit data sets
str(retrofitBER)
write.csv(retrofitBER, "finalretrofitBER.csv", row.names = FALSE)
dbWriteTable(conn = con_datamart, name = 'retrofit_cleaned_datamart', value = retrofitBER, row.names=FALSE, overwrite=TRUE, sep =',')

# create the two files for the different types of retrofit e..g insulation and heating 
insulatorBER <- retrofitBER %>%
  select(Type,YearBuilt,BERCategory,TotalFloorArea,WallArea,WindowArea,NoStoreys,UValueWindow,UValueDoor,UValueRoof,
         UValueFloor,UValueWall,HeatType,WaterHeatType,NoOfChimneys,NoOfOpenFlues,NoOfFansAndVents,SealedPorch,StructureType,
         SuspendedWoodenFloor,VentilationMethod,PercentageDraughtStripped,NoOfSidesSheltered,NoOilBoilerHeatingPumps,ThermoControledBoiler, 
         NoGasBoilerHeatingPumps,PrimaryEnergyLighting,LowEnergyLightingPercent   
  )
str(insulatorBER)

heatingBER <- retrofitBER %>%
  select(Type,YearBuilt,BERCategory,TotalFloorArea,WallArea,WindowArea,NoStoreys,HeatType,WaterHeatType,NoOfChimneys,NoOfOpenFlues,NoOfFansAndVents,
         SealedPorch,StructureType, SuspendedWoodenFloor,VentilationMethod,PercentageDraughtStripped,NoOfSidesSheltered,NoOilBoilerHeatingPumps,ThermoControledBoiler, 
         NoGasBoilerHeatingPumps,PrimaryEnergyLighting,LowEnergyLightingPercent,DistributionLosses,WHMainSystemEff,WHEffAdjFactor,
         ThermalMassCategory,PrimaryEnergyPumpsFans,PrimaryEnergyMainWater,PrimaryEnergySupplementaryWater,DeliveredEnergyMainSpace,DeliveredEnergySecondarySpace,
         PrimaryEnergyMainSpace,PrimaryEnergySecondarySpace,CO2MainSpace,CO2SecondarySpace,NoCentralHeatingPumps,OilBoilerThermo
  )
str(heatingBER)

# save to DataMarts
dbWriteTable(conn = con_datamart, name = 'insulator_cleaned_datamart', value = insulatorBER, row.names=FALSE, overwrite=TRUE, sep =',')
dbWriteTable(conn = con_datamart, name = 'heating_cleaned_datamart', value = heatingBER, row.names=FALSE, overwrite=TRUE, sep =',')

#*************************************************************************************************************
# 14. Split the data into training and validation sets -  73,917 observations
# Random select of 70/30 split for training/validation
dt = sort(sample(nrow(retrofitBER), nrow(retrofitBER)*.7))

# insulator - 51,741 training/22,176 Validation; 27 variables
BERTrainInsul<-insulatorBER[dt,]
BERValInsul <-insulatorBER[-dt,]
dim(BERTrainInsul)
dim(BERValInsul)

# heating - 51,741 training/22,176 Validation; 38 variables
BERTrainHeat<-heatingBER[dt,]
BERValHeat <-heatingBER[-dt,]
dim(BERTrainHeat)
dim(BERValHeat)

#*************************************************************************************************************
# Start the modeling 
#*************************************************************************************************************
## 15.  random forest
#-------------------------------------------------------------------------------
## 15.A starting with  default parameters and all variables in the data frame - Insulation
rf_Insul_default <- randomForest(BERCategory ~., data = BERTrainInsul)
# predict using the random forest model and examine the confusion matrix
pred_insul_rf <- predict(rf_Insul_default, BERValInsul[,-3])
CM_RF_I_Def <- confusionMatrix(pred_insul_rf, BERValInsul$BERCategory, mode = "prec_recall")
# print confusion matrix
CM_RF_I_Def
# examine the primary metrics
print(paste("Macro Recall:", mean(CM_RF_I_Def$byClass[, "Recall"])))
print(paste("Macro Precision:", mean(CM_RF_I_Def$byClass[, "Precision"])))
print(paste("Macro F1 Score:", mean(CM_RF_I_Def$byClass[, "F1"]))) 
print(paste("Macro Specificity:", mean(CM_RF_I_Def$byClass[, "Specificity"]))) 

# plot the OOB error versus the number of trees - error is constant after about 100 trees
plot(rf_Insul_default)
legend("top", colnames(rf_Insul_default$err.rate), fill=1:ncol(rf_Insul_default$err.rate))

# what are the important parameters in the model
varImpPlot(rf_Insul_default, sort=T, n.var = 20, main = 'Top 20 Feature Importance', pch = 22, cex=1)

#-------------------------------------------------------------------------------
## 15.B Reduce the dataset to the most important parameters and re evaluate
insulatorBER_Fix <- insulatorBER %>%
  select(UValueWall,UValueWindow,BERCategory,YearBuilt,TotalFloorArea,WallArea,PrimaryEnergyLighting,UValueFloor,WindowArea, UValueRoof,
         UValueDoor, NoOfFansAndVents,PercentageDraughtStripped, NoOfSidesSheltered, ThermoControledBoiler, HeatType
  )
str(insulatorBER_Fix)

# create training and validation data from the reduced dataset
dt = sort(sample(nrow(insulatorBER_Fix), nrow(insulatorBER_Fix)*.7))
BERTrainInsul_Fix<-insulatorBER_Fix[dt,]
BERValInsul_Fix <-insulatorBER_Fix[-dt,]
dim(BERTrainInsul_Fix)
dim(BERValInsul_Fix)

# re-run with the reduced features
rf_Insul_default_Fix <- randomForest(BERCategory ~., data = BERTrainInsul_Fix)
# predict using the random forest model and examine the confusion matrix
pred_insulfix_rf <- predict(rf_Insul_default_Fix, BERValInsul_Fix[,-3])
CM_RF_I_Import <- confusionMatrix(pred_insulfix_rf, BERValInsul_Fix$BERCategory,  mode = "prec_recall")
# examine
CM_RF_I_Import
# print the primary metrics
print(paste("Macro Recall:", mean(CM_RF_I_Import$byClass[, "Recall"])))
print(paste("Macro Precision:", mean(CM_RF_I_Import$byClass[, "Precision"])))
print(paste("Macro F1 Score:", mean(CM_RF_I_Import$byClass[, "F1"]))) 
print(paste("Macro Specificity:", mean(CM_RF_I_Import$byClass[, "Specificity"]))) 

#-------------------------------------------------------------------------------
## 15.C Rerun with balanced data- upsample
x <- as.data.frame(BERTrainInsul_Fix %>% select(-BERCategory))
y <- as.factor(BERTrainInsul_Fix$BERCategory)
# oversample because there are so few observations for the A1 class
BERTrainInsulFix_BalUp <- upSample(x, y, yname = "BERCategory")

# sample it for computational reasons
dt = sort(sample(nrow(BERTrainInsulFix_BalUp), nrow(BERTrainInsulFix_BalUp)*.2))
sampleInsul_TrainBalFix <- BERTrainInsulFix_BalUp[dt,]
dim(sampleInsul_TrainBalFix)
table(sampleInsul_TrainBalFix$BERCategory)

# re-run with the new parameters and balanced
rf_Insul_default_Fix_Bal <- randomForest(BERCategory ~., data = sampleInsul_TrainBalFix)
# predict using the random forest model and examine the confusion matrix
pred_insulfix_bal_rf <- predict(rf_Insul_default_Fix_Bal, BERValInsul_Fix[,-3])
CM_RF_I_Bal <- confusionMatrix(pred_insulfix_bal_rf, BERValInsul_Fix$BERCategory, mode = "prec_recall")
#examine
CM_RF_I_Bal
# print the primary metrics
print(paste("Macro Recall:", mean(CM_RF_I_Bal$byClass[, "Recall"])))
print(paste("Macro Precision:", mean(CM_RF_I_Bal$byClass[, "Precision"])))
print(paste("Macro F1 Score:", mean(CM_RF_I_Bal$byClass[, "F1"]))) 
print(paste("Macro Specificity:", mean(CM_RF_I_Bal$byClass[, "Specificity"]))) 

#-------------------------------------------------------------------------------
## 15.D default random forest - heat
rf_Heat_default <- randomForest(BERCategory ~., data = BERTrainHeat)
# predict using the random forest model and examine the confusion matrix
pred_heat_rf <- predict(rf_Heat_default, BERValHeat[,-3])
CM_RF_H_Def <- confusionMatrix(pred_heat_rf, BERValHeat$BERCategory, mode = "prec_recall")
# examine
CM_RF_H_Def
# print primary metrics
print(paste("Macro Recall:", mean(CM_RF_H_Def$byClass[, "Recall"])))
print(paste("Macro Precision:", mean(CM_RF_H_Def$byClass[, "Precision"])))
print(paste("Macro F1 Score:", mean(CM_RF_H_Def$byClass[, "F1"]))) 
print(paste("Macro Specificity:", mean(CM_RF_H_Def$byClass[, "Specificity"]))) 

# plot the OOB error versus the number of trees
plot(rf_Heat_default)
legend("top", colnames(rf_Heat_default$err.rate), fill=1:ncol(rf_Heat_default$err.rate))

# what are the important parameters in the model
varImpPlot(rf_Heat_default, sort=T, n.var = 20, main = 'Top 20 Feature Importance', pch = 22, cex=1.1)

#-------------------------------------------------------------------------------
## 15.E reduce the dataset to the most important parameters and reevaluate
heatingBER_Fix <-  heatingBER %>%
  select(PrimaryEnergyMainSpace,CO2MainSpace,BERCategory,DeliveredEnergyMainSpace,PrimaryEnergyLighting,TotalFloorArea,DistributionLosses,YearBuilt,
         WHMainSystemEff, PrimaryEnergyMainWater, PrimaryEnergySecondarySpace, DeliveredEnergySecondarySpace, CO2SecondarySpace,
         WindowArea, WallArea
  )
str(heatingBER_Fix)

# create new training and validation data from this dataset
dt = sort(sample(nrow(heatingBER_Fix), nrow(heatingBER_Fix)*.7))
BERTrainHeat_Fix<-heatingBER_Fix[dt,]
BERValHeat_Fix <-heatingBER_Fix[-dt,]
dim(BERTrainHeat_Fix)
dim(BERValHeat_Fix)

# re-run with the new parameters
rf_Heat_default_Fix <- randomForest(BERCategory ~., data = BERTrainHeat_Fix)
# predict using the random forest model and examine the confusion matrix
pred_heatfix_rf <- predict(rf_Heat_default_Fix, BERValHeat_Fix[,-3])
CM_RF_H_Import <- confusionMatrix(pred_heatfix_rf, BERValHeat_Fix$BERCategory, mode = "prec_recall")
#examine
CM_RF_H_Import
# print the primary metrics
print(paste("Macro Recall:", mean(CM_RF_H_Import$byClass[, "Recall"])))
print(paste("Macro Precision:", mean(CM_RF_H_Import$byClass[, "Precision"])))
print(paste("Macro F1 Score:", mean(CM_RF_H_Import$byClass[, "F1"]))) 
print(paste("Macro Specificity:", mean(CM_RF_H_Import$byClass[, "Specificity"]))) 

#-------------------------------------------------------------------------------
## 15.F Rerun with balanced data - upsample
x <- as.data.frame(BERTrainHeat_Fix %>% select(-BERCategory))
y <- as.factor(BERTrainHeat_Fix$BERCategory)
# upsample
BERTrainHeatFix_BalUp <- upSample(x, y, yname = "BERCategory")
# random sample it for computational reasons
dt = sort(sample(nrow(BERTrainHeatFix_BalUp), nrow(BERTrainHeatFix_BalUp)*.2))
sampleHeat_TrainBalFix <- BERTrainHeatFix_BalUp[dt,]
dim(sampleHeat_TrainBalFix)
table(sampleHeat_TrainBalFix$BERCategory)

# re-run with the new parameters
rf_Heat_default_Fix_Bal <- randomForest(BERCategory ~., data = sampleHeat_TrainBalFix)
# predict using the random forest model and examine the confusion matrix - 91%
pred_heatfix_rf_bal <- predict(rf_Heat_default_Fix_Bal, BERValHeat_Fix[,-3])
CM_RF_H_Bal <- confusionMatrix(pred_heatfix_rf_bal, BERValHeat$BERCategory, mode = "prec_recall")
# examine
CM_RF_H_Bal
#print the primary metrics
print(paste("Macro Recall:", mean(CM_RF_H_Bal$byClass[, "Recall"])))
print(paste("Macro Precision:", mean(CM_RF_H_Bal$byClass[, "Precision"])))
print(paste("Macro F1 Score:", mean(CM_RF_H_Bal$byClass[, "F1"]))) 
print(paste("Macro Specificity:", mean(CM_RF_H_Bal$byClass[, "Specificity"]))) 

#-------------------------------------------------------------------------------
## 15.G Precision-Recall and AUC for the important feature model as the best heating model
rf_pred <- predict(rf_Heat_default_Fix, BERValHeat_Fix, type = 'prob') 
# plot the Precision-Recall - https://github.com/WandeRum/multiROC
rf_pred <- data.frame(rf_pred)
colnames(rf_pred) <- paste(colnames(rf_pred), "pred_RF", sep="_")
# prepare 
true_label <- dummies::dummy(BERValHeat_Fix$BERCategory, sep = ".")
true_label <- data.frame(true_label)
colnames(true_label) <- gsub(".*?\\.", "", colnames(true_label))
colnames(true_label) <- paste(colnames(true_label), "true", sep = "_")
final_df <- cbind(true_label, rf_pred)

roc_res <- multi_roc(final_df, force_diag=T)
pr_res <- multi_pr(final_df, force_diag=T)

plot_roc_df <- plot_roc_data(roc_res)
plot_pr_df <- plot_pr_data(pr_res)

plot_roc_df <- plot_roc_df[plot_roc_df$Group!="Macro" & plot_roc_df$Group!="Micro", ]
plot_pr_df <- plot_pr_df[plot_pr_df$Group!="Macro" & plot_pr_df$Group!="Micro", ]
# Precision - Recall AUC 
pr_res$AUC
# plot ROC
ggplot(plot_roc_df, aes(x = 1-Specificity, y=Sensitivity)) +
  geom_path(aes(color = Group), size=1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               colour='grey', linetype = 'dotdash') +
  ggtitle("RF ROC for Heat Data") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))
# plot precision and recall
ggplot(plot_pr_df, aes(x=Recall, y=Precision)) + 
  geom_path(aes(color = Group), size=1.5) + 
  ggtitle("RF Precision/Recall Curve for Heating data") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))

#-------------------------------------------------------------------------------
## 15.H Tune algorithm performance - Insulator
# Define the control - k-fold cross validation using k = 10 and a random search to make it performant
trControl <- trainControl(method = "cv", number = 10, search = "random")
# take a sample for performance reasons
BERTrain <- insulatorBER_Fix %>% sample_frac(0.2)

# get best mtry (number of predictors sampled for splitting at each node) - start at 200 based on the OOB error graph 
tuneGrid <- expand.grid(.mtry = c(1: 15))
rf_mtry <- caret::train(BERCategory~.,
                        data = BERTrain,
                        method = "rf",
                        metric = "Accuracy",
                        tuneGrid = tuneGrid,
                        trControl = trControl,
                        importance = TRUE,
                        ntree = 200,
                        na.action=na.exclude)
summary(rf_mtry)
# Store best value of mtry 

max(rf_mtry$results$Accuracy) 
best_mtry<- rf_mtry$bestTune$mtry
best_mtry

# apply the best mtry to the tuning grid for following iterations
tunegrid <- expand.grid(.mtry=best_mtry)

# Search for best maxnodes (tree depth)
store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(5: 60)) {
  rf_maxnode <- caret::train(BERCategory ~.,
                             data = BERTrain,
                             method = "rf",
                             metric = "Accuracy",
                             tuneGrid = tuneGrid,
                             trControl = trControl,
                             importance = TRUE,
                             maxnodes = maxnodes,
                             ntree = 200,
                             na.action=na.exclude)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)

# search for best ntrees
store_maxtrees <- list()
for (ntree in c(100, 200, 250, 300, 350, 400, 450, 500, 550, 600)) {
  rf_maxtrees <- caret::train(BERCategory ~.,
                              data = BERTrain,
                              method = "rf",
                              metric = "Accuracy",
                              tuneGrid = tuneGrid,
                              trControl = trControl,
                              importance = TRUE,
                              maxnodes = 47,
                              ntree = ntree,
                              na.action=na.exclude)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

# display best settings for model
best_mtry    #13
store_maxnode  #47
store_maxtrees  #250
summary(results_tree)

# create the final model with the best parameters (maxnodes significantly influencing model metrics so remove for now)
fit_rf_I <- caret::train(BERCategory ~.,
                       BERTrain,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       ntree = 250,
                       na.action=na.exclude)

#evaluate final insulator model
prediction <- predict(fit_rf_I, BERValInsul_Fix[-3])
CM_RF_I_Tune <- confusionMatrix(prediction, BERValInsul_Fix$BERCategory, mode = "prec_recall")
#examine
CM_RF_I_Tune
# print the primary metrics
print(paste("Macro Recall:", mean(CM_RF_I_Tune$byClass[, "Recall"])))
print(paste("Macro Precision:", mean(CM_RF_I_Tune$byClass[, "Precision"], na.rm = TRUE)))
print(paste("Macro F1 Score:", mean(CM_RF_I_Tune$byClass[, "F1"], na.rm = TRUE))) 
print(paste("Macro Specificity:", mean(CM_RF_I_Tune$byClass[, "Specificity"]))) 

#-------------------------------------------------------------------------------
## 15.I Precision-Recall and AUC for the tuned model as the best insulator model
rf_pred_RF_I <- predict(fit_rf_I, BERValInsul_Fix, type = 'prob') 
# plot the Precision-Recall - https://github.com/WandeRum/multiROC
rf_pred_RF_I <- data.frame(rf_pred_RF_I)
colnames(rf_pred_RF_I) <- paste(colnames(rf_pred_RF_I), "pred_RF", sep="_")

true_label <- dummies::dummy(BERValInsul_Fix$BERCategory, sep = ".")
true_label <- data.frame(true_label)
colnames(true_label) <- gsub(".*?\\.", "", colnames(true_label))
colnames(true_label) <- paste(colnames(true_label), "true", sep = "_")
final_df <- cbind(true_label, rf_pred_RF_I)

roc_res <- multi_roc(final_df, force_diag=T)
pr_res <- multi_pr(final_df, force_diag=T)

plot_roc_df <- plot_roc_data(roc_res)
plot_pr_df <- plot_pr_data(pr_res)

plot_roc_df <- plot_roc_df[plot_roc_df$Group!="Macro" & plot_roc_df$Group!="Micro", ]
plot_pr_df <- plot_pr_df[plot_pr_df$Group!="Macro" & plot_pr_df$Group!="Micro", ]
# Precision - Recall AUC 
pr_res$AUC

# plot ROC
ggplot(plot_roc_df, aes(x = 1-Specificity, y=Sensitivity)) +
  geom_path(aes(color = Group), size=1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               colour='grey', linetype = 'dotdash') +
  ggtitle("RF ROC for Insulator Data") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))
# plot precision and recall
ggplot(plot_pr_df, aes(x=Recall, y=Precision)) + 
  geom_path(aes(color = Group), size=1.5) + 
  ggtitle("RF Precision/Recall Curve for Insulator Data") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))

#-------------------------------------------------------------------------------
## 15.J Tune Heat 
trControl <- trainControl(method = "cv", number = 10, search = "random")

# create a sample for performance reasons
BERTrain <- BERTrainHeat_Fix %>% sample_frac(0.2)
dim(BERTrain)

# get best mtry (number of predictors sampled for spliting at each node) - start at 200 based on the OOB error graph
tuneGrid <- expand.grid(.mtry = c(1: 15))
rf_mtry <- caret::train(BERCategory~.,
                        data = BERTrain,
                        method = "rf",
                        metric = "Accuracy",
                        tuneGrid = tuneGrid,
                        trControl = trControl,
                        importance = TRUE,
                        ntree = 200,
                        na.action=na.exclude)
summary(rf_mtry)
options(warn=0)
# Store best value of mtry
max(rf_mtry$results$Accuracy)
best_mtry<- rf_mtry$bestTune$mtry
best_mtry

# apply the best mtry to the tuning grid for following iterations
tunegrid <- expand.grid(.mtry=best_mtry)

# Search for best maxnodes (tree depth)
store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(5: 70)) {
  rf_maxnode <- caret::train(BERCategory ~.,
                             data = BERTrain,
                             method = "rf",
                             metric = "Accuracy",
                             tuneGrid = tuneGrid,
                             trControl = trControl,
                             importance = TRUE,
                             maxnodes = maxnodes,
                             ntree = 200,
                             na.action=na.exclude)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)

# search for best ntrees
store_maxtrees <- list()
for (ntree in c(100, 200, 250, 300, 350, 400, 450, 500, 550, 600)) {
  rf_maxtrees <- caret::train(BERCategory ~.,
                              data = BERTrain,
                              method = "rf",
                              metric = "Accuracy",
                              tuneGrid = tuneGrid,
                              trControl = trControl,
                              importance = TRUE,
                              maxnodes = 67,
                              ntree = ntree,
                              na.action=na.exclude)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

# display best settings for model
best_mtry    #14
store_maxnode  #67
store_maxtrees  #250
summary(results_tree)

# create the final model with the best parameters 
# maxnodes is causing the accuracy to drop to 45% which is far less then the untuned model so will not restrict the number of terminal nodes
fit_rf_h <- caret::train(BERCategory ~.,
                         BERTrain,
                         method = "rf",
                         metric = "Accuracy",
                         tuneGrid = tuneGrid,
                         trControl = trControl,
                         importance = TRUE,
                         ntree = 550,
                         na.action=na.exclude)

# evaluate final insulator model against the original validation data
prediction_rfh <- predict(fit_rf_h, BERValHeat_Fix[-3])
CM_RF_H_Tune <- confusionMatrix(prediction_rfh, BERValHeat_Fix$BERCategory, mode = "prec_recall")
# examine
CM_RF_H_Tune
# print the primary metrics
print(paste("Macro Recall:", mean(CM_RF_H_Tune$byClass[, "Recall"])))
print(paste("Macro Precision:", mean(CM_RF_H_Tune$byClass[, "Precision"],  na.rm = TRUE)))
print(paste("Macro F1 Score:", mean(CM_RF_H_Tune$byClass[, "F1"],  na.rm = TRUE))) 
print(paste("Macro Specificity:", mean(CM_RF_H_Tune$byClass[, "Specificity"]))) 

#******************************************************************************************************
# 16. Naive Bayes 
#******************************************************************************************************
# look at the correlation plot for feature independence
numeric_Test <- insulatorBER %>%
  select_if(is.numeric) 

corMat <- cor(numeric_Test)
corrplot(corMat, type="upper", number.cex = 0.5, number.digits=1, tl.cex	= 0.7)

#  TotalFloorarea correlates strongly with PrimaryEnergyLighting and WallArea 
a1 <- ggplot(
  data = insulatorBER,
  aes(y = PrimaryEnergyLighting, x = BERCategory, color = BERCategory)) +
  geom_boxplot()

# TotalFloorArea is a very similar plot to the energy plots
a2 <- ggplot(
  data = insulatorBER,
  aes(y = TotalFloorArea, x = BERCategory, color = BERCategory)) +
  geom_boxplot()

grid.arrange(a1, a2, top = "Examination of distribution", ncol=1, nrow =2)

# create a new data frame with TotalFlooArea and WallAREa removed - retain Primary Energy Lighting. Remove low energy light as it is primarily zeros
insulatorBER_NB <- insulatorBER %>%
  select(
    -TotalFloorArea, -WallArea, -LowEnergyLightingPercent)

# check
numeric_Test <- insulatorBER_NB %>%
  select_if(is.numeric) 

# test for normality of the numeric variables - all are non gaussian distributions
numeric_Test %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

# set up the training data
dt = sort(sample(nrow(insulatorBER_NB), nrow(insulatorBER_NB)*.7))
BERTrainInsul_NB<-insulatorBER_NB[dt,]
BERValInsul_NB <-insulatorBER_NB[-dt,]
dim(BERTrainInsul_NB)
dim(BERValInsul_NB)

# sample for computational reasons
BERTrainInsul_NB <- BERTrainInsul_NB %>% sample_frac(0.2)
str(BERTrainInsul_NB)

#-------------------------------------------------------------------------------
## 16.A Default NB 
features <- setdiff(names(BERTrainInsul_NB), "BERCategory")
x.h <- BERTrainInsul_NB[, features] # Set X as list of features
y.h <- BERTrainInsul_NB$BERCategory # Set Y as BER Category

# default execution with no tuning
nb_insul_default <- caret::train(
  x = x.h,
  y = y.h,
  method = "nb")

# evaluate
predNB_def <- predict(nb_insul_default, newdata = BERValInsul_NB)
CM_NB_I_Def <- confusionMatrix(predNB_def, BERValInsul_NB$BERCategory, mode = "prec_recall")
# examine
CM_NB_I_Def
# print the primary metrics
print(paste("Macro Recall:", mean(CM_NB_I_Def$byClass[, "Recall"])))
print(paste("Macro Precision:", mean(CM_NB_I_Def$byClass[, "Precision"])))
print(paste("Macro F1 Score:", mean(CM_NB_I_Def$byClass[, "F1"]))) 
print(paste("Macro Specificity:", mean(CM_NB_I_Def$byClass[, "Specificity"]))) 

#-------------------------------------------------------------------------------
## 16.B Tuned NB with cross validation

# set up ten fold cross validation
train_control <- trainControl(
  method = "cv", 
  number = 10
)

# set tuning parameters tune (fl is leplace correction; userkernel is distribution type, adjust is bandwidth adjustment)
search_grid <- expand.grid(
  usekernel = c(TRUE,FALSE),
  fL = 1:3,
  adjust = seq(1, 3, by = 1)
)

# execute with tuning
nb_insul <- caret::train(
  x = x.h,
  y = y.h,
  method = "nb",
  trControl = train_control,
  tuneGrid = search_grid,
  preProc = c("scale", "center")
)

# display the params
nb_insul

# test against the validation set
predNB <- predict(nb_insul, newdata = BERValInsul_NB)
CM_NB_I_Tune <- confusionMatrix(predNB, BERValInsul_NB$BERCategory, mode = "prec_recall")
# examine
CM_NB_I_Tune

# plot important features
plot(varImp(nb_insul))

# print the primary metrics
print(paste("Macro Recall:", mean(CM_NB_I_Tune$byClass[, "Recall"])))
print(paste("Macro Precision:", mean(CM_NB_I_Tune$byClass[, "Precision"])))
print(paste("Macro F1 Score:", mean(CM_NB_I_Tune$byClass[, "F1"]))) 
print(paste("Macro Specificity:", mean(CM_NB_I_Tune$byClass[, "Specificity"]))) 

# plot important features
plot(varImp(nb_insul))

#-------------------------------------------------------------------------------
## 16.C NB with balanced data

# balance and test (upsample creates many samples - days to run - need to random sample the upsample )
table(BERTrainInsul_NB$BERCategory)
a.i <- as.data.frame(BERTrainInsul_NB %>% select(-BERCategory))
b.i <- as.factor(BERTrainInsul_NB$BERCategory)
# up sample (based on the 20% sample)
insulatorBER_Train_NB_BAL <- upSample(a.i, b.i, yname = "BERCategory")
table(insulatorBER_Train_NB_BAL$BERCategory)
str(insulatorBER_Train_NB_BAL)

# set up to run NB 
features <- setdiff(names(insulatorBER_Train_NB_BAL), "BERCategory")
x.i <- insulatorBER_Train_NB_BAL[, features] # Set X as list of features
y.i <- insulatorBER_Train_NB_BAL$BERCategory # Set Y as BER Category

# execute
nb_insul_Bal <- caret::train(
  x = x.i,
  y = y.i,
  method = "nb",
  trControl = train_control,
  tuneGrid = search_grid,
  preProc = c("center","scale")
)

# test against the validation set
predNB_Bal <- predict(nb_insul, newdata = BERValInsul_NB)
CM_NB_I_BaL_Tune <- confusionMatrix(predNB, BERValInsul_NB$BERCategory,  mode = "prec_recall")
# examine
CM_NB_I_BaL_Tune
# print metrics
print(paste("Macro Recall:", mean(CM_NB_I_BaL_Tune$byClass[, "Recall"])))
print(paste("Macro Precision:", mean(CM_NB_I_BaL_Tune$byClass[, "Precision"])))
print(paste("Macro F1 Score:", mean(CM_NB_I_BaL_Tune$byClass[, "F1"]))) 
print(paste("Macro Specificity:", mean(CM_NB_I_BaL_Tune$byClass[, "Specificity"]))) 

#-------------------------------------------------------------------------------
## 16.D Precision-Recall/AUC
nb_pred <- predict(nb_insul, BERValInsul_NB, type = 'prob') 
# plot the Precision-Recall
nb_pred <- data.frame(nb_pred)
colnames(nb_pred) <- paste(colnames(nb_pred), "pred_NB", sep="_")

true_label <- dummies::dummy(BERValInsul_NB$BERCategory, sep = ".")
true_label <- data.frame(true_label)
colnames(true_label) <- gsub(".*?\\.", "", colnames(true_label))
colnames(true_label) <- paste(colnames(true_label), "true", sep = "_")
final_df <- cbind(true_label, nb_pred)

roc_res <- multi_roc(final_df, force_diag=T)
pr_res <- multi_pr(final_df, force_diag=T)

plot_roc_df <- plot_roc_data(roc_res)
plot_pr_df <- plot_pr_data(pr_res)

plot_roc_df <- plot_roc_df[plot_roc_df$Group!="Macro" & plot_roc_df$Group!="Micro", ]
plot_pr_df <- plot_pr_df[plot_pr_df$Group!="Macro" & plot_pr_df$Group!="Micro", ]
# Precision - Recall AUC 
pr_res$AUC
# plot ROC
ggplot(plot_roc_df, aes(x = 1-Specificity, y=Sensitivity)) +
  geom_path(aes(color = Group), size=1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               colour='grey', linetype = 'dotdash') +
  ggtitle("NB ROC for Insulator Data") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))
# plot precision recall
ggplot(plot_pr_df, aes(x=Recall, y=Precision)) + 
  geom_path(aes(color = Group), size=1.5) + 
  ggtitle("NB Precision/Recall Curve for Insulator Data") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))
#-------------------------------------------------------------------------------
## 16.E Default NB for Heat data
#check correlation
numeric_Test <- heatingBER %>%
  select_if(is.numeric) 

corMat <- cor(numeric_Test)
corrplot(corMat, type="upper", number.cex = 0.5, number.digits=1, tl.cex	= 0.7)

# plots are very similar - remove two of these features
p1 <- ggplot(
  data = heatingBER,
  aes(y = DeliveredEnergyMainSpace, x = BERCategory, color = BERCategory)) +
  geom_boxplot()

p2 <- ggplot(
  data = heatingBER,
  aes(y = PrimaryEnergyMainSpace, x = BERCategory, color = BERCategory)) +
  geom_boxplot()

p3 <- ggplot(
  data = heatingBER,
  aes(y = CO2MainSpace, x = BERCategory, color = BERCategory)) +
  geom_boxplot()

# similar correlations for Secondary Energy
p4 <- ggplot(
  data = heatingBER,
  aes(y = DeliveredEnergySecondarySpace, x = BERCategory, color = BERCategory)) +
  geom_boxplot()

p5 <-ggplot(
  data = heatingBER,
  aes(y = PrimaryEnergySecondarySpace, x = BERCategory, color = BERCategory)) +
  geom_boxplot()

p6 <-ggplot(
  data = heatingBER,
  aes(y = CO2SecondarySpace, x = BERCategory, color = BERCategory)) +
  geom_boxplot()

grid.arrange(p1, p2, p3, p4, p5,p6, top = "Examination of Distribution", ncol=2, nrow =3)

# create a new data frame - water heat type is removed as it correlates directly to heat type
heatingBER_NB <- heatingBER %>%
  select(
    -TotalFloorArea, -WallArea, -DeliveredEnergyMainSpace, -CO2MainSpace, -DeliveredEnergySecondarySpace, -CO2SecondarySpace, -WaterHeatType, -LowEnergyLightingPercent)

numeric_Test <- heatingBER_NB %>%
  select_if(is.numeric) 

# visualize this
numeric_Test %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()
#
str(heatingBER_NB)

# split into training and validation
dt = sort(sample(nrow(heatingBER_NB), nrow(heatingBER_NB)*.7))
BERTrainHeat_NB<-heatingBER_NB[dt,]
BERValHeat_NB <-heatingBER_NB[-dt,]
dim(BERTrainHeat_NB)
dim(BERValHeat_NB)

# sample for computational reasons
BERTrainHeat_NB <- BERTrainHeat_NB %>% sample_frac(0.2)
str(BERTrainHeat_NB)

# RUN NB with the control parameters set earlier (10 k-fold cross validation)
features <- setdiff(names(BERTrainHeat_NB), "BERCategory")
x.h <- BERTrainHeat_NB[, features] # Set X as list of features
y.h <- BERTrainHeat_NB$BERCategory # Set Y as BER Category

# run default
nb_heat_def <- caret::train(
  x = x.h,
  y = y.h,
  method = "nb"
)

# test against the validation set
predNBH_Def <- predict(nb_heat_def, newdata = BERValHeat_NB)
CM_NB_H_Def <- confusionMatrix(predNBH_Def, BERValHeat_NB$BERCategory,  mode = "prec_recall")
# examine
CM_NB_H_Def
# print the primary metrics
print(paste("Macro Recall:", mean(CM_NB_H_Def$byClass[, "Recall"])))
print(paste("Macro Precision:", mean(CM_NB_H_Def$byClass[, "Precision"])))
print(paste("Macro F1 Score:", mean(CM_NB_H_Def$byClass[, "F1"]))) 
print(paste("Macro Specificity:", mean(CM_NB_H_Def$byClass[, "Specificity"]))) 

#-------------------------------------------------------------------------------
## 16.F  NB Tuned for  Heat data
nb_heat <- caret::train(
  x = x.h,
  y = y.h,
  method = "nb",
  trControl = train_control,
  tuneGrid = search_grid,
  preProc = c("scale", "center")
)

# view the params used
nb_heat

# test against the validation set
predNB_H <- predict(nb_heat, newdata = BERValHeat_NB)
CM_NB_H_Tune <- confusionMatrix(predNB_H, BERValHeat_NB$BERCategory,  mode = "prec_recall")
# examine
CM_NB_H_Tune
# print metrics
print(paste("Macro Recall:", mean(CM_NB_H_Tune$byClass[, "Recall"])))
print(paste("Macro Precision:", mean(CM_NB_H_Tune$byClass[, "Precision"])))
print(paste("Macro F1 Score:", mean(CM_NB_H_Tune$byClass[, "F1"]))) 
print(paste("Macro Specificity:", mean(CM_NB_H_Tune$byClass[, "Specificity"]))) 

#-------------------------------------------------------------------------------
## 16.G NB with balanced data

# examine the distribution and balance for class imbalance
table(BERTrainHeat_NB$BERCategory)
a.h <- as.data.frame(BERTrainHeat_NB %>% select(-BERCategory))
b.h <- as.factor(BERTrainHeat_NB$BERCategory)
# Balance - upsample is based on the 20% sample...
heatingBER_Train_NB_BAL <- upSample(a.h, b.h, yname = "BERCategory")
table(heatingBER_Train_NB_BAL$BERCategory)
str(heatingBER_Train_NB_BAL)

# RUN NB with the control parameters set earlier (10 k-fold cross validation)
features <- setdiff(names(heatingBER_Train_NB_BAL), "BERCategory")
x.h <- heatingBER_Train_NB_BAL[, features] # Set X as list of features
y.h <- heatingBER_Train_NB_BAL$BERCategory # Set Y as BER Category
# execute
nb_heat_bal <- caret::train(
  x = x.h,
  y = y.h,
  method = "nb",
  trControl = train_control,
  tuneGrid = search_grid,
  preProc = c("scale", "center")
)

# test against the validation set
predNB_Bal <- predict(nb_heat_bal, newdata = BERValHeat_NB)
CM_NB_H_BaL_Tune <- confusionMatrix(predNB_Bal, BERValHeat_NB$BERCategory, mode = "prec_recall")
# examine
CM_NB_H_BaL_Tune
# print metrics
print(paste("Macro Recall:", mean(CM_NB_H_BaL_Tune$byClass[, "Recall"])))
print(paste("Macro Precision:", mean(CM_NB_H_BaL_Tune$byClass[, "Precision"])))
print(paste("Macro F1 Score:", mean(CM_NB_H_BaL_Tune$byClass[, "F1"]))) 
print(paste("Macro Specificity:", mean(CM_NB_H_BaL_Tune$byClass[, "Specificity"]))) 

# plot it
plot(varImp(nb_heat))

#-------------------------------------------------------------------------------
## 16.H NB Precision-Recall/AUC for tuned model
nb_pred <- predict(nb_heat, BERValHeat_NB, type = 'prob') 
# prepare and plot Precision-Recall
nb_pred <- data.frame(nb_pred)
colnames(nb_pred) <- paste(colnames(nb_pred), "pred_NB", sep="_")

true_label <- dummies::dummy(BERValHeat_NB$BERCategory, sep = ".")
true_label <- data.frame(true_label)
colnames(true_label) <- gsub(".*?\\.", "", colnames(true_label))
colnames(true_label) <- paste(colnames(true_label), "true", sep = "_")
final_df <- cbind(true_label, nb_pred)

roc_res <- multi_roc(final_df, force_diag=T)
pr_res <- multi_pr(final_df, force_diag=T)

plot_roc_df <- plot_roc_data(roc_res)
plot_pr_df <- plot_pr_data(pr_res)

plot_roc_df <- plot_roc_df[plot_roc_df$Group!="Macro" & plot_roc_df$Group!="Micro", ]
plot_pr_df <- plot_pr_df[plot_pr_df$Group!="Macro" & plot_pr_df$Group!="Micro", ]
# Precision - Recall AUC 
pr_res$AUC

# plot ROC
ggplot(plot_roc_df, aes(x = 1-Specificity, y=Sensitivity)) +
  geom_path(aes(color = Group), size=1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               colour='grey', linetype = 'dotdash') +
  ggtitle("NB ROC for Heat Data") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))
# Plot precision - recall
ggplot(plot_pr_df, aes(x=Recall, y=Precision)) + 
  geom_path(aes(color = Group), size=1.5) + 
  ggtitle("NB Precision/Recall Curve for Heat Data") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))

#-------------------------------------------------------------------------------
## 16.I NB Heat with manual binning of numerical
w1 <- ggplot(data = heatingBER_NB) + geom_histogram(aes(x = PrimaryEnergyLighting), bins = 10, colour = "black", fill = "red")
min(heatingBER_NB$PrimaryEnergyLighting)
max(heatingBER_NB$PrimaryEnergyLighting)
#
w2 <- ggplot(data = heatingBER_NB) + geom_histogram(aes(x = PercentageDraughtStripped), bins = 10, colour = "black", fill = "red")
min(heatingBER_NB$PercentageDraughtStripped)
max(heatingBER_NB$PercentageDraughtStripped)
#
w4 <- ggplot(data = heatingBER_NB) + geom_histogram(aes(x = DistributionLosses), bins = 10, colour = "black", fill = "red")
min(heatingBER_NB$DistributionLosses)
max(heatingBER_NB$DistributionLosses)
#
w5 <- ggplot(data = heatingBER_NB) + geom_histogram(aes(x = WHMainSystemEff), bins = 10, colour = "black", fill = "red")
min(heatingBER_NB$WHMainSystemEff)
max(heatingBER_NB$WHMainSystemEff)
#
w6 <- ggplot(data = heatingBER_NB) + geom_histogram(aes(x = WHEffAdjFactor), bins = 10, colour = "black", fill = "red") 
min(heatingBER_NB$WHEffAdjFactor)
max(heatingBER_NB$WHEffAdjFactor)
#
w7 <- ggplot(data = heatingBER_NB) + geom_histogram(aes(x = PrimaryEnergyPumpsFans), bins = 10, colour = "black", fill = "red") 
min(heatingBER_NB$PrimaryEnergyPumpsFans)
max(heatingBER_NB$PrimaryEnergyPumpsFans)
#
w8 <- ggplot(data = heatingBER_NB) + geom_histogram(aes(x = PrimaryEnergyMainWater), bins = 10, colour = "black", fill = "red") 
min(heatingBER_NB$PrimaryEnergyMainWater)
max(heatingBER_NB$PrimaryEnergyMainWater)
#
w9 <- ggplot(data = heatingBER_NB) + geom_histogram(aes(x = PrimaryEnergySupplementaryWater), bins = 10, colour = "black", fill = "red") 
min(heatingBER_NB$PrimaryEnergySupplementaryWater)
max(heatingBER$PrimaryEnergySupplementaryWater)
#
w10 <- ggplot(data = heatingBER_NB) + geom_histogram(aes(x = PrimaryEnergyMainSpace), bins = 10, colour = "black", fill = "red") 
min(heatingBER_NB$PrimaryEnergyMainSpace)
max(heatingBER_NB$PrimaryEnergyMainSpace)
#
w11 <- ggplot(data = heatingBER_NB) + geom_histogram(aes(x = PrimaryEnergySecondarySpace), bins = 10, colour = "black", fill = "red") 
min(heatingBER_NB$PrimaryEnergySecondarySpace)
max(heatingBER_NB$PrimaryEnergySecondarySpace)
#
grid.arrange(w1,w2, w4, w5,w6,w7,w8,w9,w10,w11, top = "Title of the page", ncol=4, nrow =3)

# find min and max of discrete features
min(heatingBER_NB$NoStoreys, na.rm = TRUE)
max(heatingBER_NB$NoStoreys, na.rm = TRUE)
#
min(heatingBER_NB$NoOfChimneys, na.rm = TRUE)
max(heatingBER_NB$NoOfChimneys, na.rm = TRUE)
#
min(heatingBER_NB$NoOfOpenFlues, na.rm = TRUE)
max(heatingBER_NB$NoOfOpenFlues, na.rm = TRUE)
#
min(heatingBER_NB$NoOfFansAndVents, na.rm = TRUE)
max(heatingBER_NB$NoOfFansAndVents, na.rm = TRUE)
#
min(heatingBER_NB$NoOfSidesSheltered, na.rm = TRUE)
max(heatingBER_NB$NoOfSidesSheltered, na.rm = TRUE)
#
min(heatingBER_NB$NoOilBoilerHeatingPumps, na.rm = TRUE)
max(heatingBER_NB$NoOilBoilerHeatingPumps, na.rm = TRUE)
#
min(heatingBER_NB$NoCentralHeatingPumps, na.rm = TRUE)
max(heatingBER_NB$NoCentralHeatingPumps, na.rm = TRUE)
# remaining discrete numeric to factors
heatingBER_NB$NoStoreys = cut(heatingBER_NB$NoStoreys, 5, labels=c('One','Two', 'Three','Four','Five')) 
heatingBER_NB$NoOfChimneys = cut(heatingBER_NB$NoOfChimneys, 10, labels=c('None','One','Two', 'Three','Four','Five','Six','Seven','Eight','Nine')) 
heatingBER_NB$NoOfOpenFlues = cut(heatingBER_NB$NoOfOpenFlues, 9, labels=c('None','One','Two', 'Three','Four','Five','Six','Seven','Eight')) 
heatingBER_NB$NoOfFansAndVents = cut(heatingBER_NB$NoOfFansAndVents, 20, labels=c('None','One','Two', 'Three','Four','Five','Six','Seven','Eight','Nine','Ten','Eleven','Twelve','Thirteen','Fourteen','Fifteen','Sixteen','Seventeen','Eighteen','Nineteen')) 
heatingBER_NB$NoOfSidesSheltered = cut(heatingBER_NB$NoOfSidesSheltered, 5, labels=c('None','One','Two', 'Three','Four')) 
heatingBER_NB$NoOilBoilerHeatingPumps = cut(heatingBER_NB$NoOilBoilerHeatingPumps, 3, labels=c('None','One','Two')) 
heatingBER_NB$NoGasBoilerHeatingPumps = cut(heatingBER_NB$NoGasBoilerHeatingPumps, 12, labels=c('None','One','Two', 'Three','Four','Five','Six','Seven','Eight','Nine','Ten','Eleven')) 
heatingBER_NB$NoCentralHeatingPumps = cut(heatingBER_NB$NoCentralHeatingPumps, 15, labels=c('None','One','Two', 'Three','Four','Five','Six','Seven','Eight','Nine','Ten','Eleven','Twelve','Thirteen','Fourteen'))

# Bining of numeric features - decide how many bins per feature
heatingBER_NB$PrimaryEnergyLighting = cut(heatingBER_NB$PrimaryEnergyLighting, 20) 
heatingBER_NB$PercentageDraughtStripped = cut(heatingBER_NB$PercentageDraughtStripped, 10) 
heatingBER_NB$DistributionLosses = cut(heatingBER_NB$DistributionLosses, 50) 
heatingBER_NB$WHMainSystemEff = cut(heatingBER_NB$WHMainSystemEff, 20)
heatingBER_NB$WHEffAdjFactor = cut(heatingBER_NB$WHEffAdjFactor, 4) 
heatingBER_NB$PrimaryEnergyPumpsFans = cut(heatingBER_NB$PrimaryEnergyPumpsFans, 50) 
heatingBER_NB$PrimaryEnergyMainWater = cut(heatingBER_NB$PrimaryEnergyMainWater, 100) 
heatingBER_NB$PrimaryEnergySupplementaryWater = cut(heatingBER_NB$PrimaryEnergySupplementaryWater,  100) 
heatingBER_NB$PrimaryEnergyMainSpace = cut(heatingBER_NB$PrimaryEnergyMainSpace, 500)
heatingBER_NB$PrimaryEnergySecondarySpace = cut(heatingBER_NB$PrimaryEnergySecondarySpace, 300)

# there are a number of features that have little or no impact as per the importance plots so remove them
heatingBER_NBFixed <- heatingBER_NB %>%
  select(
    -WindowArea, -SealedPorch, -StructureType, -NoOfSidesSheltered, -SuspendedWoodenFloor, -Type, -NoStoreys)
str(heatingBER_NBFixed)

# split data
dt = sort(sample(nrow(heatingBER_NBFixed), nrow(heatingBER_NBFixed)*.7))
BERTrainHeat_NB<-heatingBER_NBFixed[dt,]
BERValHeat_NB <-heatingBER_NBFixed[-dt,]
dim(BERTrainHeat_NB)
dim(BERValHeat_NB)

#sample
heatingBER_Train_NB <- BERTrainHeat_NB %>% sample_frac(0.2)
str(heatingBER_Train_NB)

# run for the manually binned data with tuning
features <- setdiff(names(heatingBER_Train_NB), "BERCategory")
x.h <- heatingBER_Train_NB[, features] # Set X as list of features
y.h <- heatingBER_Train_NB$BERCategory # Set Y as BER Category

# naive bayes using caret - no need for preProc out when the numeric features have been manually binned
nb_heat_man <- caret::train(
  x = x.h,
  y = y.h,
  method = "nb",
  trControl = train_control,
  tuneGrid = search_grid
)

# test against the validation set
predNB <- predict(nb_heat_man, newdata = BERValHeat_NB)
CM_NB_H_BaL_Manual <- confusionMatrix(predNB, BERValHeat_NB$BERCategory, mode = "prec_recall")
CM_NB_H_BaL_Manual
# print primary metrics
print(paste("Macro Recall:", mean(CM_NB_H_BaL_Manual$byClass[, "Recall"])))
print(paste("Macro Precision:", mean(CM_NB_H_BaL_Manual$byClass[, "Precision"])))
print(paste("Macro F1 Score:", mean(CM_NB_H_BaL_Manual$byClass[, "F1"]))) 
print(paste("Macro Specificity:", mean(CM_NB_H_BaL_Manual$byClass[, "Specificity"]))) 
#******************************************************************************************************
# 17. Support Vector Machine
#******************************************************************************************************
## 17.A - start with default
svm_Insul_default <- svm(BERCategory ~., data = BERTrainInsul)
# predict using the svm model and examine the confusion matrix
pred_insul_svm <- predict(svm_Insul_default, BERValInsul[,-3])
CM_SVM_I_Def <- confusionMatrix(pred_insul_svm, BERValInsul$BERCategory, mode = "prec_recall")
#examine
CM_SVM_I_Def
# print metrics
print(paste("Macro Recall:", mean(CM_SVM_I_Def$byClass[, "Recall"])))
print(paste("Macro Precision:", mean(CM_SVM_I_Def$byClass[, "Precision"], na.rm = TRUE)))
print(paste("Macro F1 Score:", mean(CM_SVM_I_Def$byClass[, "F1"], na.rm = TRUE))) 
print(paste("Macro Specificity:", mean(CM_SVM_I_Def$byClass[, "Specificity"]))) 

# summarise
summary(svm_Insul_default)
plot(pred_insul_svm)

# important variables 
w <- t(svm_Insul_default$coefs) %*% svm_Insul_default$SV   # weight vectors
w <- apply(w, 2, function(v){sqrt(sum(v^2))})  # weight
w <- sort(w, decreasing = T)
print(w)
# plot to determine relationship 
ggplot(data = insulatorBER, aes(x = YearBuilt, y=VentilationMethod, color = BERCategory, shape = BERCategory)) +
  geom_point(size = 2) +
  scale_shape_manual(values=seq(0,14))

ggplot(data = insulatorBER, aes(x = YearBuilt, y=UValueFloor, color = BERCategory, shape = BERCategory)) +
  geom_point(size = 2) +
  scale_shape_manual(values=seq(0,14))

ggplot(data = insulatorBER, aes(x = UValueWindow, y=UValueFloor, color = BERCategory, shape = BERCategory)) +
  geom_point(size = 2) +
  scale_shape_manual(values=seq(0,14))

ggplot(data = insulatorBER, aes(x = YearBuilt, y=UValueWindow, color = BERCategory, shape = BERCategory)) +
  geom_point(size = 2) +
  scale_shape_manual(values=seq(0,14))

ggplot(data = insulatorBER, aes(x = UValueRoof, y=UValueFloor, color = BERCategory, shape = BERCategory)) +
  geom_point(size = 2) +
  scale_shape_manual(values=seq(0,14))

ggplot(data = insulatorBER, aes(x = UValueFloor, y=StructureType, color = BERCategory, shape = BERCategory)) +
  geom_point(size = 2) +
  scale_shape_manual(values=seq(0,14))

ggplot(data = insulatorBER, aes(x = YearBuilt, y=Type, color = BERCategory, shape = BERCategory)) +
  geom_point(size = 2) +
  scale_shape_manual(values=seq(0,14))

ggplot(data = insulatorBER, aes(x = UValueFloor, y=VentilationMethod, color = BERCategory, shape = BERCategory)) +
  geom_point(size = 2) +
  scale_shape_manual(values=seq(0,14))

# encoding 
insul_SVM <- insulatorBER

# create encode function
encode <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}

# encode categorical
sapply(insul_SVM, is.factor)
insul_SVM$Type = encode(insul_SVM[["Type"]])
insul_SVM$HeatType = encode(insul_SVM[["HeatType"]])
insul_SVM$WaterHeatType = encode(insul_SVM[["WaterHeatType"]])
insul_SVM$SealedPorch = encode(insul_SVM[["SealedPorch"]])
insul_SVM$StructureType = encode(insul_SVM[["StructureType"]])
insul_SVM$SuspendedWoodenFloor = encode(insul_SVM[["SuspendedWoodenFloor"]])
insul_SVM$VentilationMethod = encode(insul_SVM[["VentilationMethod"]])
insul_SVM$ThermoControledBoiler = encode(insul_SVM[["ThermoControledBoiler"]])
insul_SVM$YearBuilt = encode(insul_SVM[["YearBuilt"]])

# scaling of numeric
insul_SVM[, -c(3)] <- scale(insul_SVM[, -c(3)])
str(insul_SVM)

# split into train and validation
dt = sort(sample(nrow(insul_SVM), nrow(insul_SVM)*.7))
# insulator - 51,759 training; 170 variables
BERTrainInsul<-insul_SVM[dt,]
BERValInsul <-insul_SVM[-dt,]
dim(BERTrainInsul)
dim(BERValInsul)

# train on a sample for computational reasons
BERTrain_SVMI_Sample <- BERTrainInsul %>% sample_frac(0.2)
str(BERTrain_SVMI_Sample)

# find best tuning parameters - linear best Cost = 100; Radial best cost =100 and gamma = 0.5
tuneInsul <- tune(svm, BERCategory ~., data = BERTrain_SVMI_Sample, 
                  ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100), gamma = c(0.5,1,2,3,4)))

plot(tuneInsul)
# examine
tuneInsul
bestmodI = tuneInsul$best.model
summary(bestmodI)

#-------------------------------------------------------------------------------
## 17.B SVM with tuned radial kernel and best tuning parameters
svm_insul_rad <- svm(BERCategory~., data=BERTrain_SVMI_Sample, 
                     method="C-classification", scale = FALSE, kernel="radial", 
                     gamma=0.5, cost=100)

# test radial against the validation set
predSVM_Insul_TuneR <- predict(svm_insul_rad, newdata = BERValInsul)
CM_SVM_I_Tune_R <- confusionMatrix(predSVM_Insul_TuneR, BERValInsul$BERCategory, mode = "prec_recall")
# examine
CM_SVM_I_Tune_R
#print metrics
print(paste("Macro Recall:", mean(CM_SVM_I_Tune_R$byClass[, "Recall"])))
print(paste("Macro Precision:", mean(CM_SVM_I_Tune_R$byClass[, "Precision"])))
print(paste("Macro F1 Score:", mean(CM_SVM_I_Tune_R$byClass[, "F1"]))) 
print(paste("Macro Specificity:", mean(CM_SVM_I_Tune_R$byClass[, "Specificity"]))) 

# test radial against the validation set
predSVM_Insul_TuneP <- predict(svm_insul_poly, newdata = BERValInsul)
confusionMatrix(predSVM_Insul_TuneP, BERValInsul$BERCategory)

#-------------------------------------------------------------------------------
## 17.C SVM with linear kernel and best tuning parameters - set probably so its possible to plot the ROC
svm_insul_lin <- svm(BERCategory~., data=BERTrain_SVMI_Sample, 
                     method="C-classification", scale = FALSE, kernel="linear", probability=TRUE, 
                     cost=100)

# test linear against the validation set
predSVM_Insul_TuneL <- predict(svm_insul_lin, newdata = BERValInsul)
confusionMatrix(predSVM_Insul_TuneL, BERValInsul$BERCategory)
#
predSVM_Insul_TuneL <- predict(svm_insul_lin, newdata = BERValInsul)
CM_SVM_I_Tune_L <- confusionMatrix(predSVM_Insul_TuneL, BERValInsul$BERCategory,  mode = "prec_recall")
CM_SVM_I_Tune_L
#
print(paste("Macro Recall:", mean(CM_SVM_I_Tune_L$byClass[, "Recall"])))
print(paste("Macro Precision:", mean(CM_SVM_I_Tune_L$byClass[, "Precision"], na.rm = TRUE )))
print(paste("Macro F1 Score:", mean(CM_SVM_I_Tune_L$byClass[, "F1"], na.rm = TRUE))) 
print(paste("Macro Specificity:", mean(CM_SVM_I_Tune_L$byClass[, "Specificity"]))) 
#-------------------------------------------------------------------------------
## 17.D SVM with balanced data and best kernel - linear
x <- as.data.frame(BERTrainInsul %>% select(-BERCategory))
y <- as.factor(BERTrainInsul$BERCategory)
# downSample function
balancedBERTrain <- downSample(x, y, yname = "BERCategory")
table(balancedBERTrain$BERCategory)
str(balancedBERTrain)

# run
svm_insul_lin_bal <- svm(BERCategory~., data=balancedBERTrain, 
                         method="C-classification", scale = FALSE, kernel="linear", 
                         gamma=0.5, cost=100)

# test against the validation set
predSVM_Insul_Tune_Bal <- predict(svm_insul_lin_bal, newdata = BERValInsul)
CM_SVM_I_Tune_Bal_L <- confusionMatrix(predSVM_Insul_Tune_Bal, BERValInsul$BERCategory,  mode = "prec_recall")
# examine
CM_SVM_I_Tune_Bal_L
# print metrics
print(paste("Macro Recall:", mean(CM_SVM_I_Tune_Bal_L$byClass[, "Recall"])))
print(paste("Macro Precision:", mean(CM_SVM_I_Tune_Bal_L$byClass[, "Precision"], na.rm = TRUE )))
print(paste("Macro F1 Score:", mean(CM_SVM_I_Tune_Bal_L$byClass[, "F1"], na.rm = TRUE))) 
print(paste("Macro Specificity:", mean(CM_SVM_I_Tune_Bal_L$byClass[, "Specificity"]))) 
# details
svm_insul_lin_bal
#-------------------------------------------------------------------------------
## 17.E AUC and Precision-Recall for best model
svm_predI <- predict(svm_insul_lin_bal, BERValInsul, probability=TRUE) 
# plot the Precision-Recall - prepare the data
svm_predI <- data.frame(attr(svm_predI, "probabilities"))
colnames(svm_predI) <- paste(colnames(svm_predI), "pred_SVM", sep="_")
#
true_label <- dummies::dummy(BERValInsul$BERCategory, sep = ".")
true_label <- data.frame(true_label)
colnames(true_label) <- gsub(".*?\\.", "", colnames(true_label))
colnames(true_label) <- paste(colnames(true_label), "true", sep = "_")
final_df <- cbind(true_label, svm_predI)

roc_res <- multi_roc(final_df, force_diag=T)
pr_res <- multi_pr(final_df, force_diag=T)

plot_roc_df <- plot_roc_data(roc_res)
plot_pr_df <- plot_pr_data(pr_res)

plot_roc_df <- plot_roc_df[plot_roc_df$Group!="Macro" & plot_roc_df$Group!="Micro", ]
plot_pr_df <- plot_pr_df[plot_pr_df$Group!="Macro" & plot_pr_df$Group!="Micro", ]
# Precision - Recall AUC 
pr_res$AUC
# plot ROC
ggplot(plot_roc_df, aes(x = 1-Specificity, y=Sensitivity)) +
  geom_path(aes(color = Group), size=1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               colour='grey', linetype = 'dotdash') +
  ggtitle("SVM ROC for Insulation Data") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))
# plot precision-recall
ggplot(plot_pr_df, aes(x=Recall, y=Precision)) + 
  geom_path(aes(color = Group), size=1.5) + 
  ggtitle("SVM Precision/Recall Curve for Insulator Data") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))
#-------------------------------------------------------------------------------
## 17.F Heating SVM - starting with default
svm_heat_default <- svm(BERCategory ~., data = BERTrainHeat)
# predict using the svm model and examine the confusion matrix 
pred_heat_svm <- predict(svm_heat_default, BERValHeat[,-3])
CM_SVM_H_Def <- confusionMatrix(pred_heat_svm, BERValHeat$BERCategory, mode = "prec_recall")
#examine
CM_SVM_H_Def
#print metrics
print(paste("Macro Recall:", mean(CM_SVM_H_Def$byClass[, "Recall"])))
print(paste("Macro Precision:", mean(CM_SVM_H_Def$byClass[, "Precision"],  na.rm = TRUE)))
print(paste("Macro F1 Score:", mean(CM_SVM_H_Def$byClass[, "F1"],  na.rm = TRUE))) 
print(paste("Macro Specificity:", mean(CM_SVM_H_Def$byClass[, "Specificity"]))) 

# summarise
summary(svm_heat_default)
plot(pred_heat_svm)
# important variables 
w <- t(svm_heat_default$coefs) %*% svm_heat_default$SV   # weight vectors
w <- apply(w, 2, function(v){sqrt(sum(v^2))})  # weight
w <- sort(w, decreasing = T)
print(w)

# tuning - plot some of the heat relationships to understand the relationships
ggplot(data = heatingBER, aes(x = DistributionLosses, y=DeliveredEnergyMainSpace, color = BERCategory, shape = BERCategory)) +
  geom_point(size = 2) +
  scale_shape_manual(values=seq(0,14))

ggplot(data = heatingBER, aes(x = WHMainSystemEff, y=WHEffAdjFactor, color = BERCategory, shape = BERCategory)) +
  geom_point(size = 2) +
  scale_shape_manual(values=seq(0,14))

ggplot(data = heatingBER, aes(x = PrimaryEnergyMainSpace, y=CO2MainSpace, color = BERCategory, shape = BERCategory)) +
  geom_point(size = 2) +
  scale_shape_manual(values=seq(0,14))

# encoding 
heat_SVM <- heatingBER

# create encode funtion
encode <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}

# encode categorical
sapply(heat_SVM, is.factor)
heat_SVM$Type = encode(heat_SVM[["Type"]])
heat_SVM$HeatType = encode(heat_SVM[["HeatType"]])
heat_SVM$WaterHeatType = encode(heat_SVM[["WaterHeatType"]])
heat_SVM$SealedPorch = encode(heat_SVM[["SealedPorch"]])
heat_SVM$StructureType = encode(heat_SVM[["StructureType"]])
heat_SVM$SuspendedWoodenFloor = encode(heat_SVM[["SuspendedWoodenFloor"]])
heat_SVM$VentilationMethod = encode(heat_SVM[["VentilationMethod"]])
heat_SVM$ThermoControledBoiler = encode(heat_SVM[["ThermoControledBoiler"]])
heat_SVM$OilBoilerThermo = encode(heat_SVM[["OilBoilerThermo"]])
heat_SVM$ThermalMassCategory = encode(heat_SVM[["ThermalMassCategory"]])
heat_SVM$YearBuilt = encode(heat_SVM[["YearBuilt"]])

# scaling of numeric
heat_SVM[, -c(3)] <- scale(heat_SVM[, -c(3)])
str(heat_SVM)

# split into train and validation
dt = sort(sample(nrow(heat_SVM), nrow(heat_SVM)*.7))
# insulator - 51,759 training; 170 variables
BERTrainHeat<-heat_SVM[dt,]
BERValHeat <-heat_SVM[-dt,]
dim(BERTrainHeat)
dim(BERValHeat)

# train on a sample for computational reasons
BERTrain_SVMH_Sample <- BERTrainHeat %>% sample_frac(0.2)
str(BERTrain_SVMH_Sample)

# find best tuning parameters - linear best Cost = 100; Radial best cost =100 and gamma = 0.5
tuneHeat <- tune(svm, BERCategory ~., data = BERTrain_SVMH_Sample,
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100), gamma = c(0.5,1,2,3,4)))

# examine
tuneHeat
bestmodH = tuneHeat$best.model
summary(bestmodH)

#-------------------------------------------------------------------------------
## 17.H SVM with tuned radial kernel and best tuning parameters
svm_heat_rad <- svm(BERCategory~., data=BERTrain_SVMH_Sample, 
                    method="C-classification", scale = FALSE, kernel="radial", 
                    gamma=0.5, cost=100)

# test radial against the validation set
predSVM_Heat_TuneR <- predict(svm_heat_rad, newdata = BERValHeat)
CM_SVM_H_Tune_R <- confusionMatrix(predSVM_Heat_TuneR, BERValHeat$BERCategory, mode = "prec_recall")
#examine
CM_SVM_H_Tune_R
#print metrics
print(paste("Macro Recall:", mean(CM_SVM_H_Tune_R$byClass[, "Recall"])))
print(paste("Macro Precision:", mean(CM_SVM_H_Tune_R$byClass[, "Precision"])))
print(paste("Macro F1 Score:", mean(CM_SVM_H_Tune_R$byClass[, "F1"]))) 
print(paste("Macro Specificity:", mean(CM_SVM_H_Tune_R$byClass[, "Specificity"]))) 

#-------------------------------------------------------------------------------
## 17.I SVM with tuned linear kernel and best tuning parameters -  set probably so its possible to plot the ROC
svm_heat_lin <- svm(BERCategory~., data=BERTrain_SVMH_Sample, probability=TRUE,
                    method="C-classification", scale = FALSE, kernel="linear", 
                    cost=100)

# evaluate linear
predSVM_Heat_TuneL <- predict(svm_heat_lin, newdata = BERValHeat)
CM_SVM_H_Tune_L <- confusionMatrix(predSVM_Heat_TuneL, BERValHeat$BERCategory,  mode = "prec_recall")
# examine
CM_SVM_H_Tune_L
#print metrics
print(paste("Macro Recall:", mean(CM_SVM_H_Tune_L$byClass[, "Recall"])))
print(paste("Macro Precision:", mean(CM_SVM_H_Tune_L$byClass[, "Precision"])))
print(paste("Macro F1 Score:", mean(CM_SVM_H_Tune_L$byClass[, "F1"]))) 
print(paste("Macro Specificity:", mean(CM_SVM_H_Tune_L$byClass[, "Specificity"]))) 
# details
svm_heat_lin
#-------------------------------------------------------------------------------
## 17.K SVM Heat Precision-Recall/ROC for best model
svm_pred <- predict(svm_heat_lin, BERValHeat, probability=TRUE) 
# prepare
svm_pred <- data.frame(attr(svm_pred, "probabilities"))
colnames(svm_pred) <- paste(colnames(svm_pred), "pred_SVM", sep="_")

true_label <- dummies::dummy(BERValHeat$BERCategory, sep = ".")
true_label <- data.frame(true_label)
colnames(true_label) <- gsub(".*?\\.", "", colnames(true_label))
colnames(true_label) <- paste(colnames(true_label), "true", sep = "_")
final_df <- cbind(true_label, svm_pred)

roc_res <- multi_roc(final_df, force_diag=T)
pr_res <- multi_pr(final_df, force_diag=T)

plot_roc_df <- plot_roc_data(roc_res)
plot_pr_df <- plot_pr_data(pr_res)

plot_roc_df <- plot_roc_df[plot_roc_df$Group!="Macro" & plot_roc_df$Group!="Micro", ]
plot_pr_df <- plot_pr_df[plot_pr_df$Group!="Macro" & plot_pr_df$Group!="Micro", ]
# Precision - Recall AUC 
pr_res$AUC
# plot ROC
ggplot(plot_roc_df, aes(x = 1-Specificity, y=Sensitivity)) +
  geom_path(aes(color = Group), size=1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               colour='grey', linetype = 'dotdash') +
  ggtitle("SVM ROC for Heat Data") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))
# plot precision - recall
ggplot(plot_pr_df, aes(x=Recall, y=Precision)) + 
  geom_path(aes(color = Group), size=1.5) + 
  ggtitle("SVM Precision/Recall Curve for Heat Data") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))
