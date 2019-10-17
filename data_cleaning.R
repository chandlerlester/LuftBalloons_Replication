library(tidyr)
library(dplyr)
library(DataCombine)
library(magrittr)
library(openintro)
library(tidyselect)

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

FREData<-(if (requireNamespace("purrr", quietly = TRUE)) {
  
  library(purrr)
  purrr::map_dfr(c("TOTRESNS", "FEDFUNDS", "GDPDEF", "GDPC1", "MORTGAGE30US", "BPCCRO1Q156NBEA"), fredr)
  
  # Using purrr::pmap_dfr() allows you to use varying optional parameters
  params <- list(
    series_id = c("TOTRESNS", "FEDFUNDS", "GDPDEF", "GDPC1", "MORTGAGE30US",  "BPCCRO1Q156NBEA"),
    frequency = c("q","q","q","q","q", "q")
  )
  
  purrr::pmap_dfr(
    .l = params,
    .f = ~ fredr(series_id = .x, observation_start = as.Date("1980-01-01"), frequency = .y)
  )
  
})

# Unstack the series 
FREData<-FREData%>%group_by(series_id)%>%mutate(group_id=row_number())
FREData<-FREData%>%spread(series_id, value)%>%select(-group_id)

# Make the year match other dates
FREData1<-separate(FREData, date, c("year", "quarter", "day"), sep="-")
FREData1$quarter<-gsub("01","1",FREData1$quarter) 
FREData1$quarter<-gsub("04","2",FREData1$quarter) 
FREData1$quarter<-gsub("07","3",FREData1$quarter) 
FREData1$quarter<-gsub("10","4",FREData1$quarter) 

FREData1<-FREData1[,-3] #drop day

# make year and quarter numeric
FREData1$year<-as.numeric(FREData1$year)  #make years and quarters numeric 
FREData1$quarter<-as.numeric(FREData1$quarter)

##################################################################################################################################

# Combine FRED and other data 
combined2<-left_join(combined1, FREData1, by = c("year", "quarter"))

# Get rid of rows with no housing data 
Luft_balloons_data<-combined2%>%drop_na(index_nsa)

# Remove other data from workspace 
rm(list=setdiff(ls(),"Luft_balloons_data"))

################################################################################################################################
# get supplemental census data 
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


###########################################################################################################################
# Now join the data 

Luft_balloons_data<-left_join(Luft_balloons_data, monthly_pop, by = c("year", "quarter", "state"))
Luft_balloons_data$Population<-as.numeric(Luft_balloons_data$Population)
Luft_balloons_data$POP<-as.numeric(Luft_balloons_data$POP)
Luft_balloons_data<-replace_na(Luft_balloons_data,replace=list(Luft_balloons_data$POP>=2010))
Luft_balloons_data$Population<-rowSums(Luft_balloons_data[,c("Population", "POP")], na.rm = TRUE)

Luft_balloons_data<-Luft_balloons_data[,1:14]
