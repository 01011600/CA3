library(dplyr)
library(pwr)
library(pxR)
library(ggplot2)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)
library(XLConnect)
library(fBasics)
library(openxlsx)
library(psych)
library(nortest)
library(XLConnect)
#*********************************************dataset1***************************************************
# Load the dataset GHG_Final data_1990-2017_website.xlsx
wb = loadWorkbook("GHG_Final data_1990-2017_website.xlsx")
# read the first page of the workbook and use the first line as the column names
epa_2019_summary <- readWorksheet(wb, sheet = "1", header = TRUE)
# Print the column names of the dataframe
colnames(epa_2019_summary)
# remove any NA values
na.omit(epa_2019_summary)
# Change the Year column to a factor
epa_2019_summary$Year <- as.factor(epa_2019_summary$Year)
# Change the data type to numeric
epa_2019_summary$Chemical.industry <- as.numeric(epa_2019_summary$Chemical.industry)
# Change the data type to numeric
epa_2019_summary$Metal.industry <- as.numeric(epa_2019_summary$Metal.industry)
# show the strusture of dataframe
str(epa_2019_summary)
# create new dataframe called charts.data
charts.data <- epa_2019_summary

# Create a bar graph using ggplot to plot the total GHG by year
ggplot(data=charts.data,aes(Year,National.Total))+
  theme(panel.grid.minor = element_line(colour = "grey"), plot.title = element_text(size = rel(1)),axis.text.x = element_text(angle=90, vjust=1), strip.text.x = element_text(size = 12, colour = "black", face = "bold")) +  
  geom_point(alpha = 0.6, position = position_jitter(w = 0.1, h = 0.0), aes(colour=factor(Year)), size =4)+
  geom_bar(stat="identity",fill="steelblue")+
  ggtitle("Greenhouse Gas Emissions") +
  scale_size_area() + 
  xlab("Year")+ 
  ylab(" Mt CO2eq")
#**************************************dataset2********************************************************************
# Combine all of the web-scraped CO2 electric generation data from each csv file into one dataset called co2_electric_data
co2_electric_data <- list.files('electric/', recursive = TRUE, full.names = TRUE)
co2_2019 <- do.call(rbind, lapply(co2_electric_data, read.csv,colClasses="character",na.strings="?"))
# change the data type of column EffectiveTime to date
co2_2019$EffectiveTime <- as.Date(co2_2019$EffectiveTime,format="%d/%m/%Y")
# Change the data type of Value to numeric
co2_2019$Value <- as.numeric(co2_2019$Value)
# Create a new column called Year
co2_2019["Year"] <- co2_2019$EffectiveTime
# change the data type to date format
co2_2019$Year <- as.Date(format(date,'%Y'))
# make a substring of date which only contains year
co2_2019$Year <- substr(co2_2019$Year, 0, 4)
# Change Year to a Factor
co2_2019$Year <- as.factor(co2_2019$Year)
# Print the structure of the dataframe
str(co2_2019)
# Set sheme of graph
theme_set(theme_minimal())
# Create a basic time series plot of c02 v time
ggplot(data = co2_2019, aes(x = EffectiveTime, y = Value))+
  geom_line(color = "#00AFBB", size = 2)
#******************************************dataset3***************************************************

# car sales from 2007 to 2018 from https://stats.beepbeep.ie/
# Read in xlsx file into dataframe called car_sales_year
car_sales_year <- read.xlsx("passenger-cars-by-engine-type_combined.xlsx")
# change data type of engine to factor
car_sales_year$Engine.Type <- as.factor(car_sales_year$Engine.Type)
# Remove any NA's
car_sales_year <-na.omit(car_sales_year)
# Print the structure of the dataframe
str(car_sales_year)

# Plot of car sales by engine type by year
qplot(Year, Units, colour = Engine.Type, data=car_sales_year)

# Create new dataframe that only has Transport C02 data and Year
epa_transport <- epa_2019_summary[,c(1,13)]
# join car engine data to road transport data by Year
engine_by_c02 <- merge(epa_transport,car_sales_year,by="Year")
# Keep columns 1,2,3 & 4
engine_by_c02_reduced <- engine_by_c02[,c(1,2,3,4)]
# Print the structure of the dataframe
str(engine_by_c02_reduced)
# Use Pairs to see if any relationships exist
pairs(engine_by_c02_reduced)


#********************************************************************************
#********************* Hypothesis testing****************************************
#Get distribution summary stats
summary(co2_2019$Value)
library(psych)
describe(co2_2019$Value)

# Create a histogram of the co2 intensity data 
library(ggplot2)
ggplot(co2_2019, aes(x=Value)) + 
  geom_histogram(aes(y=..density..),      
                 binwidth=7,
                 colour="black", fill="white") +
  geom_density(alpha=.5, fill="#FF6666")  
# Using the qqnorm plot to see if the data is normal
qqnorm(co2_2019$Value)
# Fitting a line to the data
qqline(co2_2019$Value)

#create sample size of 5000 from the c02_2019 data
co2_2019_sample <- co2_2019[c(as.integer(runif(5000,1,nrow(co2_2019)))),]

# Test for normality using the Shapiro-Wilks test
normality_test <- shapiro.test(co2_2019_sample$Value)
normality_test$p.value
# p-value is 3.321296e-38 so data is not normal

#Running the Anderson-Darling test for normality with the sampled data
library(nortest)
normality_test2 <- ad.test(co2_2019$Value)
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

# from the psych library use describe to get the stats from ireland ireland_temp_mean_no_na$value
describe(ireland_temp_mean_no_na$value)
#Running the Anderson-Darling test for normality with the sampled data
normality_test3 <- ad.test(ireland_temp_mean_no_na$value)
normality_test3$p.value

#**********************Results*****************************

# Create a box Plot CO2 by Year and color by Year
library(ggpubr)
ggboxplot(co2_2019, x = "Year", y = "Value", 
          color = "Year", palette = c("#00AFBB", "#E7B800"),
          ylab = "C02 intensity", xlab = "Year ")
