library(dplyr)
library(pwr)
library(pxR)

# read in px file as a data frame called ireland_temperature
ireland_temperature <- as.data.frame (read.px("MTM02.px"))
# Print the structure of the dataframe ghg_by_sector_by_year
str(ireland_temperature)
# create new dataframe called ghg_summary_year and using grep select the Greenhouse Gas Emissions
ireland_temperature_mean <- ireland_temperature[grepl("Mean Temperature",ireland_temperature$Statistic),]
# Create new column called Year 
ireland_temperature_mean["Year"] <- ireland_temperature_mean$Month

ireland_temperature_mean$Year <- as.Date(format(date,'%Y'))
ireland_temperature_mean$Year <- substr(ireland_temperature_mean$Year, 0, 4)
ireland_temperature_mean$Year <- as.factor(ireland_temperature_mean$Year)
ireland_temp_mean_no_na <- na.omit(ireland_temperature_mean)
hist(ireland_temp_mean_no_na$value, col= "red")
barplot()

# Make a summary of the data ireland_temp_mean_no_na by Year, median temp by year
Data_Summary <- ireland_temp_mean_no_na %>% group_by(Year) %>% summarize(Median_Temperature_C=median(value) )
# Delete Row with 2019 data as not full year
Median_temp_2018<- Data_Summary[-c(62),] 
#Median_temp_2018 <- as.numeric(Median_temp_2018$Year)
plot(Median_temp_2018$Year, Median_temp_2018$Median_Temperature_C, col='red')
str(Data_Summary)
str(Data_Summary)
barplot(1:2, col ="green")
#Print the structure of the dataframe ghg_summary_year
str(ghg_summary_year)

library(XLConnect)
wb = loadWorkbook("GHG_Final data_1990-2017_website.xlsx")
epa_2019_summary <- readWorksheet(wb, sheet = "1", header = TRUE)

colnames(epa_2019_summary)
str(epa_2019_summary)
na.omit(epa_2019_summary)

epa_2019_summary$Year <- as.factor(epa_2019_summary$Year)
epa_2019_summary$Chemical.industry <- as.numeric(epa_2019_summary$Chemical.industry)
epa_2019_summary$Metal.industry <- as.numeric(epa_2019_summary$Metal.industry)
str(epa_2019_summary)



res <- cor(epa_2019_summary)
round(res, 2)

# Create new dataframe that only has Transport C02 data and Year
epa_transport <- epa_2019_summary[,c(1,13)]
trans_engine_year <- as.data.frame (t(engine_by_c02))
# join car engine data to road transport data by Year
engine_by_c02 <- merge(epa_transport,car_sales_year,by="Year")
engine_by_c02_reduced <- engine_by_c02[,c(1,2,3,4)]
str(engine_by_c02)

library(ggplot2)
ggplot(engine_by_c02, aes(x = Year, y =Units , colour = Engine.Type)) +
  geom_point() +
  facet_wrap( ~ Engine.Type)

pairs(engine_by_c02_reduced)
library(ggplot2)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)

charts.data <- epa_2019_summary
library(ggplot2) 
# Create a bar graph using ggplot to plot the total GHG by year
ggplot(data=charts.data,aes(Year,National.Total))+
  theme(panel.grid.minor = element_line(colour = "grey"), plot.title = element_text(size = rel(1)),axis.text.x = element_text(angle=90, vjust=1), strip.text.x = element_text(size = 12, colour = "black", face = "bold")) +  
  geom_point(alpha = 0.6, position = position_jitter(w = 0.1, h = 0.0), aes(colour=factor(Year)), size =4)+
  geom_bar(stat="identity",fill="steelblue")+
  ggtitle("Greenhouse Gas Emissions") +
  scale_size_area() + 
  xlab("Year")+ 
  ylab(" Mt CO2eq")

summary.data.frame(epa_2019_summary)

p4 <- ggplot() + geom_bar(aes(y = National.Total, x = Year, fill = product), data = charts.data,
                          stat="identity")

#Change the class of year & engine type to be a categorising factor
str(epa_summary_year)
trans_epa_summary_year <- as.data.frame (t(epa_summary_year))
colnames(trans_epa_summary_year)
colnames(trans_epa_summary_year) <- c("Mt CO2eq")

car_sales_year$Year <- as.factor(car_sales_year$Year)
car_sales_year$Engine.Type <- as.factor(car_sales_year$Engine.Type)
car_sales_year <-na.omit(car_sales_year)
library(fBasics)
basicStats(car_sales_year$Units)


# Create a bar graph using ggplot to plot the total car sales by year
ggplot(data=car_sales_year,aes(Year,Units,Engine.Type))+
  theme(panel.grid.minor = element_line(colour = "grey"), plot.title = element_text(size = rel(1)),axis.text.x = element_text(angle=90, vjust=1), strip.text.x = element_text(size = 12, colour = "black", face = "bold")) +  
  geom_point(alpha = 0.6, position = position_jitter(w = 0.1, h = 0.0), aes(colour=factor(Year)), size =4)+
  geom_bar(stat="identity",fill="steelblue")+
  ggtitle("Car sales by engine type ") +
  scale_size_area() + 
  xlab("Year")+ 
  ylab(" Units of cars sold") 








# combine all of the electric data from each csv file into one dataset called AllNICrimeData.
co2_electric_data <- list.files('electric/', recursive = TRUE, full.names = TRUE)
co2_2019 <- do.call(rbind, lapply(co2_electric_data, read.csv,colClasses="character",na.strings="?"))
co2_2019$EffectiveTime <- as.Date(co2_2019$EffectiveTime,format="%d/%m/%Y")
co2_2019$Value <- as.numeric(co2_2019$Value)
str(co2_2019)
co2_2019["Year"] <- co2_2019$EffectiveTime
# Create new column called Year 
co2_2019$Year <- as.Date(format(date,'%Y'))
co2_2019$Year <- substr(co2_2019$Year, 0, 4)
co2_2019$Year <- as.factor(co2_2019$Year)
str(co2_2019)
#transposed_co2_2019 <- as.data.frame (t(co2_2019))
library(ggplot2)
theme_set(theme_minimal())
# Basic line plot
ggplot(data = co2_2019, aes(x = EffectiveTime, y = Value))+
  geom_line(color = "#00AFBB", size = 2)

library(openxlsx)
# car sales from 2007 to 2018 from https://stats.beepbeep.ie/
car_sales_year <- read.xlsx("passenger-cars-by-engine-type_combined.xlsx")
car_sales_year$Engine.Type <- as.factor(car_sales_year$Engine.Type)
car_sales_year <-na.omit(car_sales_year)
str(car_sales_year)
#transposed_sales <- as.data.frame (t(car_sales_year))
#transposed_sales$Year <- as.numeric(car_sales_year$Year)
#str(transposed_sales)

# Plot of car sales by engine type by year
qplot(Year, Units, colour = Engine.Type, data=car_sales_year)

#Change the class of year & engine type to be a categorising factor

library(fBasics)
basicStats(car_sales_year$Units)

# Remove NA's 
co2_2019 <- na.omit(co2_2019)
str(co2_2019)
#********************************************************************************
#********************* Hypothesis testing****************************************

# Create a box Plot CO2 by Year and color by Year
library("ggpubr")
ggboxplot(co2_2019, x = "Year", y = "Value", 
          color = "Year", palette = c("#00AFBB", "#E7B800"),
          ylab = "C02 intensity", xlab = "Year ")

library(ggplot2)  
# Plot distributions of energy consumed in 2019 by hour
ggplot(co2_2019[co2_2019$REGION == 'ROI',], aes(x=Value),      
                 binwidth=1000000,
                 colour="black", fill="white") +
  geom_density(alpha=.5, fill="Red")  

library(ggplot2)
ggplot(co2_2019, aes(x=Value)) + 
  geom_histogram(aes(y=..density..),      
                 binwidth=7,
                 colour="black", fill="white") +
  geom_density(alpha=.5, fill="#FF6666")  


#Get distribution summary stats
summary(co2_2019$Value)
library(psych)
describe(co2_2019$Value)
hist(co2_2019$Value)
hist(co2_2019, 
     main="Histogram for Air Passengers", 
     xlab="Value", 
     border="blue", 
     col="green",
     xlim=c(100,700),
     las=1, 
     breaks=5)

#create sample size of 5000 from the c02_2019 data
co2_2019_sample <- co2_2019[c(as.integer(runif(5000,1,nrow(co2_2019)))),]

#Get distribution summary stats for the smapled data
summary(co2_2019_sample$CO2.EMISSIONS..tCO2.hr.)
# Using the qqnorm plot to see if the data is normal
qqnorm(co2_2019_sample$CO2.EMISSIONS..tCO2.hr.)
# Fitting a line to the data
qqline(co2_2019_sample$CO2.EMISSIONS..tCO2.hr.)

# Test for normality using the Shapiro-Wilks test
normality_test <- shapiro.test(co2_2019_sample$Value)
normality_test$p.value
# p-value is 3.993182e-33 so data is not normal

#Running the Anderson-Darling test for normality with the sampled data
library(nortest)
normality_test2 <- ad.test(co2_2019_sample$Value)
normality_test2$p.value
# Data is much less (3.7e-24) than 0.05 so not normal

library(pwr)

# Run Cohen.ES test to determine the h value required for size = small
cohen.ES(test = "p", size = "small")

# Power analysis for two proportions with equal samples
power_test <-pwr.2p.test(h = 0.2, n =1000 , sig.level =.05,power = NULL )
plot(power_test)

# As the data is not normal we can use the Wilcox.text() function 
# to get the test statistic (W) as well as the p value
wilcox.test(Value ~ Year, data = co2_2019 )
# The p-value returned was 2.2e-16 which is <0.05 so reject the null hypothesis.


#************************************ NOT USED IN FINAL REPORT*************************
#energy_balance_by_type_and_year <- as.data.frame (read.px("SEI01.px"))
#energy_production_by_type_and_year <- as.data.frame (read.px("SEI02.px"))
#fuel_used_in_electricity <- as.data.frame (read.px("SEI04.px"))
#gross_energy_by_type_by_year <- as.data.frame (read.px("SEI05.px"))
#fuel_con_by_sector_by_year <- as.data.frame (read.px("SEI05.px"))
#ghg_to_air_sector_by_year <- as.data.frame (read.px("EAA10.px"))


# read in px file as a data frame called ghg_by_sector_by_year
ghg_by_sector_by_year <- as.data.frame (read.px("EAA09.px"))
# Print the structure of the dataframe ghg_by_sector_by_year
str(ghg_by_sector_by_year)
# create new dataframe called ghg_summary_year and using grep select the Greenhouse Gas Emissions
ghg_summary_year <- ghg_by_sector_by_year[grepl("Greenhouse Gas Emissions",ghg_by_sector_by_year$Statistic),]
#Print the structure of the dataframe ghg_summary_year
str(ghg_summary_year)

library(ggplot2) 
# Create a bar graph using ggplot to plot the total GHG by year
ggplot(data=ghg_summary_year,aes(Year,Value,Statistic))+
  theme(panel.grid.minor = element_line(colour = "grey"), plot.title = element_text(size = rel(1)),axis.text.x = element_text(angle=90, vjust=1), strip.text.x = element_text(size = 12, colour = "black", face = "bold")) +  
  geom_point(alpha = 0.6, position = position_jitter(w = 0.1, h = 0.0), aes(colour=factor(Year)), size =4)+
  geom_bar(stat="identity",fill="steelblue")+
  ggtitle("Greenhouse Gas Emissions (CO2,N2O,CH4,HFC,PFC,SF6) ") +
  scale_size_area() + 
  xlab("Year")+ 
  ylab(" 000 Tonnes CO2 Equivalents")
