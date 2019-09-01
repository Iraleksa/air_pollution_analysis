pacman::p_load(readr,dplyr,tidyverse,autoplotly,ggplot2,lubridate,caret,plotly,reshape2,openair)


#Reading data ----


data_all <-  read.csv("Final Test/Student _Challendge//Data/Raw/2019_01_Gener_qualitat_aire_BCN.csv",header = TRUE, sep=",", encoding = "UTF-8")

# Data exploration data
summary(data_all)
str(data_all)
hist(data_all$valor_no2)
sapply(data_all,class)
mean(is.na(data_all))
head(data_all)

### Initial data preprocessing ####

data_all$valor_o3 <- gsub(' µg/m³', '', data_all$valor_o3)
data_all$valor_no2 <- gsub(' µg/m³', '', data_all$valor_no2)
data_all$valor_pm10 <- gsub(' µg/m³', '', data_all$valor_pm10)

data_all$valor_o3<-as.numeric(data_all$valor_o3)
data_all$valor_no2<-as.numeric(data_all$valor_no2)
data_all$valor_pm10<-as.numeric(data_all$valor_pm10)
data_all$longitud<-as.numeric(data_all$longitud)
data_all$latitud<-as.numeric

#  Replacing NA with 0

data_all$valor_no2[is.na(data_all$valor_no2)] <- 0
data_all$valor_o3[is.na(data_all$valor_o3)] <- 0
data_all$valor_pm10[is.na(data_all$valor_pm10)] <- 0

# Date transformation to POSIXct format
data_all$DateTime <- as.POSIXct(data_all$dateTime,tz = "UTC",origin = "1970/01/01 00:00:00", "%Y/%m/%d %H:%M:%S")

# Create "year" attribute with lubridate 
data_all$year <- year(data_all$DateTime)
data_all$month <-month(data_all$DateTime, label = TRUE, abbr = FALSE)
data_all$week <- week(data_all$DateTime)
data_all$weekday<-wday(data_all$DateTime, label = T,abbr = F, week_start = getOption("lubridate.week.start", 1))
data_all$day <- day(data_all$DateTime)
data_all$hour <- hour(data_all$DateTime)

# Rename stations and pollutants
data_all$nom_cabina <- gsub('Barcelona - ', '', data_all$nom_cabina)

# Summorizing all values for polution
data_all <- data_all %>% mutate(total_pollution = valor_no2 + valor_o3 + valor_pm10)
data_all <- as.data.frame(data_all)

# Select only Jan 2019
# january_data <- filter(data_all, year == 2019 & month == 'January')
january_data <- data_all
january_data <- january_data[,c(19, 1:18,20:26)]
# january_data<-january_data %>%  dplyr:: rename(date = DateTime)

#### Data exploration:Patterns in pollutants occurance withtin the location ####

# Selecting data for plots
pollutants_data<-data_all %>% select('nom_cabina', 'valor_no2','valor_o3','valor_pm10','total_pollution')
pollutants_data_melt <- melt(pollutants_data, na.rm = TRUE, id.vars = "nom_cabina")

colnames(pollutants_data_melt)[2] <-"pollutant"

all_pollution <- colSums(pollutants_data[,2:4])
all_pollution <- as.data.frame(all_pollution)

all_pollution['Pollutor'] <- row.names(all_pollution)
colnames(all_pollution)[1] <-"values"

# Plot 1: Total pollution during Jan'19 by each pollutor in all locations.
p <-ggplot(all_pollution, aes(Pollutor, values))+geom_bar(stat = "identity",aes(fill = Pollutor))+ggtitle("Pollution during Jan'19 by each pollutor in all locations")
 ggplotly(p)

# Plot 2: pollution values distribution among the stations.
# From this plot we can see, that Sants s the least polluted location. 

ggplot(data=pollutants_data_melt, aes(x=reorder(nom_cabina, pollutant), y=value, fill=pollutant)) +
    geom_bar(stat="identity")+ theme_classic()+theme(axis.text.x = element_text(angle=60, hjust=1))

# Plot 3: pollution values distribution among the stations . The same, as previous plot, but different display

# From this chart we can see, that Observ Fabra area is pollutted by tropospheric Ozone much higher, than any other area.
# The most polluted locations are Eixample, Gràcia, Observ Fabra and Vall Hebron. 
pollutants_data %>% group_by(nom_cabina) %>% summarise_all(funs(sum))  

pollution_distribution <- pollutants_data %>% group_by(nom_cabina) %>% summarise_all(funs(sum)) %>%
  gather("Type", "Value",- nom_cabina) %>%
ggplot(aes(nom_cabina, Value, fill = Type)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle=35, hjust=1))
ggplotly(pollution_distribution)

# Plot 4: pollution values distribution among the stations . 
# From this plot we can see, that valor_no2 is themost problematic pollutant is Nitrogen dioxide (valor_no2)
valor_no2 <- ggplot(data=data_all, aes(x=reorder(nom_cabina, -valor_no2), y=valor_no2, fill=nom_cabina)) +
  geom_bar(stat="identity")+ggtitle(" ")+ theme_classic()+
  theme(axis.text.x = element_text(angle=60, hjust=1),plot.title = element_text(hjust = 0.1),legend.position = "none")

valor_o3 <- ggplot(data=data_all, aes(x=reorder(nom_cabina, -valor_o3), y=valor_o3, fill=nom_cabina)) +
  geom_bar(stat="identity")+ggtitle("")+ theme_classic()+
  theme(axis.text.x = element_text(angle=60, hjust=1),plot.title = element_text(hjust = 0.1),legend.position = "none")

valor_pm10 <- ggplot(data=data_all, aes(x=reorder(nom_cabina, -valor_pm10), y=valor_pm10, fill=nom_cabina)) +
  geom_bar(stat="identity")+ggtitle("")+ theme_classic()+
  theme(axis.text.x = element_text(angle=60, hjust=1),plot.title = element_text(hjust = 0.1),legend.position = "none")

p <- subplot(valor_no2,valor_o3, valor_pm10)

p %>% layout(annotations = list(
  list(x = 0.1 , y = 1.05, text = "valor_no2", showarrow = F, xref='paper', yref='paper'),
  list(x = 0.5 , y = 1.05, text = "valor_o3", showarrow = F, xref='paper', yref='paper'),
  list(x = 0.9 , y = 1.05, text = "valor_pm10", showarrow = F, xref='paper', yref='paper'))) %>%
  layout(showlegend = FALSE)

# How values for each station are distributed
pollutants_data_melt$value[pollutants_data_melt$value==0] <- NA
ph<- ggplot(pollutants_data_melt, aes(x=value)) + geom_histogram(color="darkblue", fill="lightblue", stat="count")+
  xlab("Station") + ylab("value")+ggtitle("Values pollution distribution")+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~nom_cabina,scales = "free_x")

ph<- ph + theme(axis.text.x = element_text(face="plain", color="black", 
                                           size=8,angle=0),
                axis.text.y = element_text(face="plain", color="darkgrey", 
                                           size=10, angle=0))

ggplotly(ph)

# PLot 5. How values for each station are distributed

ph<- ggplot(pollutants_data_melt, aes(y=value, x=nom_cabina)) + geom_histogram(color="darkblue", fill="lightblue", stat="identity")+
  xlab("Station") + ylab("value")+ggtitle("Distribution of different pollutants per station")+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~pollutant,scales = "free_x")

ph<- ph + theme(axis.text.x = element_text(face="plain", color="black", 
                                           size=8,angle=45),
                axis.text.y = element_text(face="plain", color="darkgrey", 
                                           size=10, angle=0))

ggplotly(ph)

#### Data exploration:Patterns in pollutants occurance withtin the daytime ####

# Plot 6. Time dependancy exploration


pollutants_data_hour<-january_data %>% select('hour', 'valor_no2','valor_o3','valor_pm10','total_pollution')
pollutants_data_hour <- pollutants_data_hour %>% group_by(hour) %>% summarise_all(funs(sum))
pollutants_data_hour_melt <- melt(pollutants_data_hour, na.rm = TRUE, id.vars = "hour")
colnames(pollutants_data_hour_melt)[2] <-"pollutant"

dh<-ggplot(pollutants_data_hour_melt, aes(y=value, x=hour)) + geom_histogram(color="darkblue", fill="lightblue", stat="identity")+
 ggtitle("Pollution during the day")+
  theme(plot.title = element_text(hjust = 0.5))+
  # scale_x_discrete(name ="Hour")+
  facet_wrap(~pollutant,scales = "free_x")

dh<- dh + theme(axis.text.x = element_text(face="plain", color="black", 
                                           size=8,angle=45),
                axis.text.y = element_text(face="plain", color="darkgrey", 
                                           size=10, angle=0))

ggplotly(dh)

# Plot 7.

p1<-ggplot(pollutants_data_hour_melt, aes(y=value, x=hour)) + geom_line()+
  ggtitle("Pollution during the day")+
  theme(plot.title = element_text(hjust = 0.5))+
    facet_wrap(~pollutant,scales = "free_x")

p1<- p1 + theme(axis.text.x = element_text(face="plain", color="black", 
                                           size=8,angle=45),
                axis.text.y = element_text(face="plain", color="darkgrey", 
                                           size=10, angle=0))

ggplotly(p1)

# Plot 8.Analyse time dependancy in each station - stacked bars
   
# ---- Run block start 
pollutants_data_hour_st<-january_data %>% select('nom_cabina','hour', 'valor_no2','valor_o3','valor_pm10','total_pollution')
pollutants_data_hour_st <- pollutants_data_hour_st %>% group_by(nom_cabina,hour) %>% summarise_all(funs(sum))
pollutants_data_hour_st_melt <- melt(pollutants_data_hour_st, na.rm = TRUE, id.vars = c('nom_cabina','hour'))
colnames(pollutants_data_hour_st_melt)[3] <-"pollutant"


dh<-ggplot(pollutants_data_hour_st_melt, aes(y=value, x=hour,fill=pollutant)) + geom_histogram(stat="identity")+
  ggtitle("Pollution during the day")+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~nom_cabina,scales = "free_x")

dh<- dh + theme(axis.text.x = element_text(face="plain", color="black", 
                                           size=8,angle=45),
                axis.text.y = element_text(face="plain", color="darkgrey", 
                                           size=10, angle=0))

ggplotly(dh)

# ---- Run block end

#  Plot 9. Analyse time dependancy in each station - lines

# ---- Run block start 
# pollutants_data_hour_st_line<-january_data %>% select('nom_cabina','hour', 'valor_no2','valor_o3','valor_pm10','total_pollution')
pollutants_data_hour_st_line<-january_data %>% select('nom_cabina','hour', 'valor_no2','valor_o3','valor_pm10')
pollutants_data_hour_st_line <- pollutants_data_hour_st_line %>% group_by(nom_cabina,hour) %>% summarise_all(funs(sum))
pollutants_data_hour_st_line_melt <- melt(pollutants_data_hour_st_line, na.rm = TRUE, id.vars = c('nom_cabina','hour'))
colnames(pollutants_data_hour_st_line_melt)[3] <-"pollutant"

pollutants_data_hour_st_line_melt$hour <- as.numeric(pollutants_data_hour_st_line_melt$hour)
# rename pollutants
pollutants_data_hour_st_line_melt$pollutant<- gsub('valor_no2', 'Nitrogen_dioxide', pollutants_data_hour_st_line_melt$pollutant)
pollutants_data_hour_st_line_melt$pollutant<- gsub('valor_o3', 'tropospheric_Ozone', pollutants_data_hour_st_line_melt$pollutant)
pollutants_data_hour_st_line_melt$pollutant<- gsub('valor_pm10', 'Suspended_particles', pollutants_data_hour_st_line_melt$pollutant)


pollutants_data_hour_st_line_melt$value[pollutants_data_hour_st_line_melt$value==0] <- NA


plot_AP_mon <-  ggplot(pollutants_data_hour_st_line_melt,aes(x=factor(hour),y=value,
                                        group=factor(pollutant),color=factor(pollutant))) +geom_line(aes()) +geom_point() +
  xlab("Hours") + ylab("Pollution level")+
facet_wrap(~nom_cabina,scales = "free_x")

ggplotly(plot_AP_mon)


#  Plot 10-13. Time Variation


openair::timeVariation(january_data,pollutant = "valor_no2",normalise = FALSE,cols = "red")
openair::timeVariation(january_data,pollutant = "valor_o3",normalise = FALSE,cols = "blue")
openair::timeVariation(january_data,pollutant = "valor_pm10",normalise = FALSE,cols = "green")
openair::timeVariation(january_data,pollutant = c("valor_no2", "valor_o3", "valor_pm10"),normalise = FALSE)

# ---- Run block end

#### Summary ####

  


# 1.1. Does the pollution level depend on the time of the day?
 # Yes for O3 (tropospheric Ozone), NO2 (Nitrogen dioxide) pollution level varies during the day, but pollution by PM10 (Suspended particles) is . 

# 1.2. Analyse the differences between locations
# Pollution measure for some locatinos are missing, so looks like the most polluted locations are Eixample, Gràcia, Observ Fabra and Vall Hebron.
# While Sants is the less polluted.
# To do final conclusions it is needed to  check the reason of missing data

# Another observation is that nitrogen dioxide and tropospheric ozone are codependant pollutant and it make sence, as 
# the majority of tropospheric ozone formation occurs when nitrogen oxides.
# We can see it on plots for all location except Observ Fabra. This would be interesting to invetigate - is it issue with the data or some specifics of the location?
# There is no data about tropospheric ozone in Sants and Poblenou, 
# most probably it is data collection issue rather than the fact, that this pollutant is not present in these locations.


# 1.3. What's the most problematic pollutant?

# The most problematic pollutant is Nitrogen dioxide


