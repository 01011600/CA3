library(pxR)
energy_balance_by_type_and_year <- as.data.frame (read.px("SEI01.px"))
energy_production_by_type_and_year <- as.data.frame (read.px("SEI02.px"))
energy_imports_by_type_and_year <- as.data.frame (read.px("SEI03.px"))
fuel_used_in_electricity <- as.data.frame (read.px("SEI04.px"))
gross_energy_by_type_by_year <- as.data.frame (read.px("SEI05.px"))
fuel_con_by_sector_by_year <- as.data.frame (read.px("SEI05.px"))
ghg_by_sector_by_year <- as.data.frame (read.px("EAA09.px"))
ghg_to_air_sector_by_year <- as.data.frame (read.px("EAA10.px"))
ghg_domestic <- as.data.frame (read.px("EAA11.px"))
str(fuel_type_by_year)

library('dplyr')
ghg_summary_year <- ghg_by_sector_by_year %>% group_by(Year, Statistic) %>% summarize( Value=sum(value))
# Create a new dataframe called Limavady_data that contains records that have 
# Limavady in the column Locality,Townland and Town
ghg_summary_year<-ghg_summary_year[grepl("Greenhouse Gas Emissions",ghg_summary_year$Statistic),]

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

# combine all of the crime data from each csv file into one dataset called AllNICrimeData.
#CO2_data <- list.files('CO2/', recursive = TRUE, full.names = TRUE)
#CO2_data_COMBINED <- do.call(rbind, lapply(CO2_data, read.xlsx))

# Write the Crime data files to CSV file called AllNICrimeData
#write.csv(AllNICrimeData, file = "AllNICrimeData.csv")