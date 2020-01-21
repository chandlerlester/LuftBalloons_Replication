library(tidyr)
library(dplyr)
library(magrittr)

# This is code for cleaning and creating the dataset form 99 Luftballons by Del Negro & Otrok
#   This paper looked at what was driving housing prices prior to the 2008 recession 
#   Before the recession housing prices were being driven by local factors, but what is driving them now?
#########################################################################################################################

# Begin with BEA data 
# Import Per capita income data 
SQINC1__ALL_AREAS_1948_2019 <- read.csv("Per_cap_inc/SQINC1__ALL_AREAS_1948_2019.csv", header=T, check.names=FALSE)

#Drop some variables 
myvars<-names(SQINC1__ALL_AREAS_1948_2019) %in% c("GeoFIPS","TableName","IndustryClassification","Description","Unit")
newdata1 <-SQINC1__ALL_AREAS_1948_2019[!myvars]

# Gather years into one column 
data1<-gather(newdata1, year, data, 4:289)

# Delete early data that has volatility 
data2<-data1[-c(1:23552),]

# Get rid of line codes, turn the multiple rows into columns 
data3<-data2%>%group_by(LineCode)%>%mutate(group_id=row_number())
data4<-data3%>%spread(LineCode, data)%>%select(-group_id)
data4<-data4%>%rename(Income="1",Population="2",Per_cap_Inc="3") #rename the new rows to makes sense 
data4<-data4[1:6] # drop columns we don't need 
data4<-data4%>%group_by("Reigon") 
data4<-data4%>%drop_na #drop non-existent reigons 
data4<-data4[1:6]

# Seperate the data into years and quarters to match home sales data 
data5<-separate(data4, year, c("year", "quarter"), sep=":Q")

# Fix some variables 
library(openintro) # for changing state names to abbreviations 
data5$year<-as.numeric(data5$year)  #make years and quarters numeric 
data5$quarter<-as.numeric(data5$quarter)
data5$GeoName<-gsub(" [*]",'',data5$GeoName) #get rid of asteriks on Alaska and Hawaii 
data6<-data5%>%filter(data5$GeoName%in%state.name)# get rid of the predetermined reigons 
data6$GeoName<-state2abbr(data6$GeoName) #change from state names to state abreviations 
data6<-data6%>%rename(state="GeoName")
# Rename the final data 
final_percapinc <-data6 

#Remove all access data
rm(list=setdiff(ls(),"final_percapinc"))


###############################################################################################################################
library(readxl)
#Import the data set on state housing sales 
HPI_PO_state <- read_excel("HPI_PO_state.xls")
HPI_data2<-HPI_PO_state%>%rename(year="yr", quarter="qtr")

# Combine the datasets 
combined1<-left_join(final_percapinc, HPI_data2, by = c("state", "year", "quarter"))
combined1<-combined1[!(names(combined1)%in%c("Warning"))]

################################################################################################################################
# Import FRED data
library(fredr)#to get the data
library(purrr)#to pull multiple series at one 

fredr_set_key("fdfb11170350fa8831cbed74b73f256c")

FREData1<-(if (requireNamespace("purrr", quietly = TRUE)) {
  
  library(purrr)
  purrr::map_dfr(c("FEDFUNDS", "GDPDEF", "GDPC1", "MORTGAGE30US", "BPCCRO1Q156NBEA"), fredr)
  
  # Using purrr::pmap_dfr() allows you to use varying optional parameters
  params <- list(
    series_id = c("FEDFUNDS", "GDPDEF", "GDPC1", "MORTGAGE30US",  "BPCCRO1Q156NBEA"),
    frequency = c("q","q","q","q", "q")
  )
  
  purrr::pmap_dfr(
    .l = params,
    .f = ~ fredr(series_id = .x, observation_start = as.Date("1980-01-01"), frequency = .y)
  )
  
})

FREData2<-(if (requireNamespace("purrr", quietly = TRUE)) {
  
  library(purrr)
  purrr::map_dfr(c("TOTRESNS","PCEPILFE"), fredr)
  
  # Using purrr::pmap_dfr() allows you to use varying optional parameters
  params <- list(
    series_id = c("TOTRESNS", "PCEPILFE"),
    frequency = c("m","m")
  )
  
  purrr::pmap_dfr(
    .l = params,
    .f = ~ fredr(series_id = .x, observation_start = as.Date("1980-01-01"), frequency = .y)
  )
  
})

# Unstack the series 
FREData1<-FREData1%>%group_by(series_id)%>%mutate(group_id=row_number())
FREData1<-FREData1%>%spread(series_id, value)%>%select(-group_id)

# Unstack the other series 
FREData2<-FREData2%>%group_by(series_id)%>%mutate(group_id=row_number())
FREData2<-FREData2%>%spread(series_id, value)%>%select(-group_id)

# Join FRED DATA 
FREData<-left_join(FREData2, FREData1, by = c("date"))

# Make the year match other dates
FREData_new<-separate(FREData, date, c("year", "quarter", "day"), sep="-")
FREData_new<-subset(FREData_new, quarter%in%c("01", "04", "07", "10"))
FREData_new$quarter<-gsub("01","1",FREData_new$quarter) 
FREData_new$quarter<-gsub("04","2",FREData_new$quarter) 
FREData_new$quarter<-gsub("07","3",FREData_new$quarter) 
FREData_new$quarter<-gsub("10","4",FREData_new$quarter) 

FREData_new<-FREData_new[,-3] #drop day

# make year and quarter numeric
FREData_new$year<-as.numeric(FREData_new$year)  #make years and quarters numeric 
FREData_new$quarter<-as.numeric(FREData_new$quarter)

##################################################################################################################################
# Join more data 
# Combine FRED and other data 
combined2<-left_join(combined1, FREData_new, by = c("year", "quarter"))

# Get rid of rows with no housing data 
Luft_balloons_data<-combined2%>%drop_na(index_nsa)

# Remove other data from workspace 
rm(list=setdiff(ls(),"Luft_balloons_data"))

################################################################################################################################
# get supplemental census data, since we need some additional population data to estimate per capita income 
library(censusapi)

# set up my API key
Sys.setenv(CENSUS_KEY="9dec005530b7713ec6c0c507f10838d329e2689d")
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_KEY")

apis<-listCensusApis()

mygeo <-listCensusMetadata(name="pep/int_population", vintage=2000, type="geography") # check geography options
myvars <-listCensusMetadata(name="pep/int_population", vintage=2000, type="variables") # check geography options 

monthly_pop<-getCensus(name="pep/int_population", vintage= 2000, vars=c("GEONAME", "DATE_", "POP", "STATE"), region="state")

# Fix variable values 
monthly_pop$quarter<-c(3)
monthly_pop$year<-as.numeric(monthly_pop$DATE_) + c(1999)
myvars<-names(monthly_pop) %in% c("state","DATE_", "STATE")
monthly_pop <-monthly_pop[!myvars]
monthly_pop<-monthly_pop%>%rename(state="GEONAME")

# Fix the states so that they are abbreviations
monthly_pop<-monthly_pop%>%filter(monthly_pop$state%in%state.name)# get rid of the predetermined reigons 
monthly_pop$state<-state2abbr(monthly_pop$state) #change from state names to state abreviations 
monthly_pop<-filter(monthly_pop, year<2010) #get rid of the overlapping year, 2010 

###########################################################################################################################
# Now join the data 
Luft_balloons_data<-left_join(Luft_balloons_data, monthly_pop, by = c("year", "quarter", "state")) 
Luft_balloons_data$Population<-as.numeric(Luft_balloons_data$Population)#make population a numeric 
Luft_balloons_data$POP<-as.numeric(Luft_balloons_data$POP) #also make POP a numeric 
Luft_balloons_data<-Luft_balloons_data%>%group_by(state,year)%>%fill(POP, .direction = "downup")# fill missing quarters with projections
Luft_balloons_data<-replace_na(Luft_balloons_data,replace=list(Luft_balloons_data$POP>2010)) #replace NAs with zeros 
Luft_balloons_data$Population<-rowSums(Luft_balloons_data[,c("Population", "POP")], na.rm = TRUE) # Add Population columns together 
Luft_balloons_data<-Luft_balloons_data[,1:16]#drop POP variable

final_data<-filter(Luft_balloons_data, year >=2000)#filter out years with missing population data 
final_data$Income<-as.numeric(final_data$Income) #make Income a numeric variable 
final_data<-transform(final_data, Per_cap_Inc = Income/Population*1000000) # calculate per capita income 
#final_data<-transform(final_data, diff = Per_cap_income-as.numeric(Per_cap_Inc)) # check against BEA calculations 
rm(list=setdiff(ls(),"final_data"))


#############################################################################################################################
#check data for stationarity and trends 
library(zoo)
library(ggplot2)
library(intrval)
# Make a quarter year variable
final_data2<-final_data%>% unite("yearq", c("year", "quarter"), sep="-", remove=FALSE)
final_data2$yearq<-as.yearqtr(final_data2$yearq, format = "%Y-%q")

#Change to growth rates etc, transform the data 
deflated_data<-final_data2%>%mutate(inflation=(PCEPILFE)/PCEPILFE[1])
deflated_data$inflation[1]=1
deflated_data<-deflated_data%>%mutate(rPer_cap_Inc=Per_cap_Inc/inflation) 
deflated_data<-deflated_data%>%mutate(rTOTRESNS=TOTRESNS/inflation)
deflated_data<-deflated_data%>%mutate(rGDPC1=GDPC1/inflation)
deflated_data<-deflated_data%>%mutate(rindex_nsa=index_nsa/inflation)
growth_data<- deflated_data %>% group_by(state) %>%mutate(index_nsag=100*(rindex_nsa-lag(rindex_nsa,1))/lag(rindex_nsa,1))

#Calculate the average annualized 
growth_data<-growth_data%>%filter(year>2000 & year<2019)%>%filter(state!="AK")%>%filter(state!="HI")
averag <-aggregate(growth_data$index_nsag, by=list(growth_data$state), mean, na.rm=TRUE)
averag<-averag%>%mutate(x=17*x) #multiple to turn into percents and properly annualize data (18 years of data)

#Calculate the average annualized for 2001-2005
growth_data2<-growth_data%>%filter(year<2006)
averag2 <-aggregate(growth_data2$index_nsag, by=list(growth_data2$state), mean, na.rm=TRUE)
averag2<-averag2%>%mutate(x=4*x) #multiple to turn into percents and properly annualize data (5 years of data)

###################################################################################################################################

# Check to see if annualized growth rates are similar to figure 1 from the paper 
# Change some variable names 
averag<-averag%>%rename(state="Group.1")
averag2<-averag2%>%rename(state="Group.1")

# Create the barchart 
ggplot(data=averag2, aes(x=region, y=x)) +
  geom_bar(stat="identity", color="blue", fill="black", width=.45)+
  theme_minimal()+
  labs(x="State", y="Annualized Growth Rates")

# Check updated data
ggplot(data=averag, aes(x=region, y=x)) +
  geom_bar(stat="identity", color="blue", fill="black", width=.45)+
  theme_minimal()+
  labs(x="State", y="Annualized Growth Rates")

# Redo this barchart as a map? 
library(maps)
library(viridis)
library(gridExtra)

us_states <- map_data("state")
us_states<-us_states%>%rename(state="region")

us_states$state<-state2abbr(us_states$state)
state_plot<-left_join(us_states, averag)

# Plot of states for 2001-2018
newmap<-ggplot(data = state_plot,
       aes(x = long, y = lat,
           group = group, fill = x))+ 
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  geom_polygon(color = "black", size = 0.1) + 
  theme_void() +
  labs(title = "Annualized Growth Rates 2001-2018", fill="Percent")+
  scale_fill_viridis(option="plasma")

# Check what it would have looked like in the original paper 
state_plot2<-left_join(us_states, averag2)

oldmap<-ggplot(data = state_plot2,
       aes(x = long, y = lat,
           group = group, fill = x))+ 
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  geom_polygon(color = "black", size = 0.1) + 
  theme_void() +
  labs(title = "Annualized Growth Rates 2001-2005", fill="Percent")+
  scale_fill_viridis(option="plasma")


grid.arrange(oldmap, newmap, nrow = 1)
rm(list=setdiff(ls(),"growth_data"))
####################################################################################################################################

# Export data for dynamic factor analysis in MATLAB

#first need to code up regions -- start with small sub regions (9) (should be coded in numbers from census data)
growth_data$subregion[growth_data$state %in% c("ME", "NH", "VT", "MA", "CT", "RI")]<-"New England"
growth_data$subregion[growth_data$state %in% c("NY", "PA", "NJ")]<-"Mid-Atlantic"
growth_data$subregion[growth_data$state %in% c("ND", "SD", "NE", "KS", "MN", "IA", "MO")]<-"Great Plains"
growth_data$subregion[growth_data$state %in% c("WI", "MI", "IL", "IN", "OH")]<-"Great Lakes"
growth_data$subregion[growth_data$state %in% c("WA", "OR", "CA")]<-"Pacific West"
growth_data$subregion[growth_data$state %in% c("ID", "NV", "UT", "AZ", "NM", "CO", "WY", "MT")]<-"Mountain West"
growth_data$subregion[growth_data$state %in% c("TX", "OK", "AR", "LA")]<-"West South"
growth_data$subregion[growth_data$state %in% c("MS", "AL", "TN", "KY")]<-"East South"
growth_data$subregion[growth_data$state %in% c("FL", "GA", "SC", "NC", "VA", "WV", "MD", "DE")]<-"Atlantic South"

#Code up regions used in original analysis (5)
growth_data$region[growth_data$state %in% c("ME", "NH", "VT", "MA", "CT", "RI", "NY", "PA", "NJ")]<-"Northeast"
growth_data$region[growth_data$state %in% c("ND", "SD", "NE", "KS", "MN", "IA", "MO","WI", "MI", "IL", "IN", "OH")]<-"Midwest"
growth_data$region[growth_data$state %in% c("WA", "OR", "CA","ID", "NV", "UT", "AZ", "NM", "CO", "WY", "MT")]<-"West"
growth_data$region[growth_data$state %in% c("TX", "OK", "AR", "LA")]<-"Southwest"
growth_data$region[growth_data$state %in% c("MS", "AL", "TN", "KY","FL", "GA", "SC", "NC", "VA", "WV", "MD", "DE")]<-"Southeast"


write.csv(growth_data,"growth_data.csv")
