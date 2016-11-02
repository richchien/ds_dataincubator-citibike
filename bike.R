library(RCurl)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(data.table)

# Download setup 
s <- seq(1:12)
s1 <- paste0("0", s[1:9])
s1 <- append(s1, s[10:12])

downloadsetup <- s1

for(i in 1:length(downloadsetup))  {
  filename <- paste0("2015",downloadsetup[i,1],"citibike-tripdata.zip")
  zipfile <- paste0("https://s3.amazonaws.com/tripdata/2015", downloadsetup[i,1], "-citibike-tripdata.zip")
  download.file(zipfile, filename)
  unzipped <- unzip(filename, list=T)
  unzip(filename)
  finalname <- paste0("2015", downloadsetup[i,1], "-citibike-tripdata.zip")
  file.rename(as.character(unzipped[1,1]), finalname)
  file.remove(filename)				
}

datafolder <- "C:/Users/RC3258/Documents/Rprojects/citibike/data"
setwd(datafolder)

# set paths for plyr
paths <- dir(pattern = "\\.csv$")
names(paths) <- basename(paths)
df <- ldply(paths, fread)
df$tripduration <- as.numeric(df$tripduration)
names(df) <- gsub(" ", "_", names(df))

# What is the median trip duration, in seconds?
x <- median(df$tripduration)
sprintf("%.10f", x)
629.0000000000

# What fraction of rides start and end at the same station?
df %>%
select(start_station_id, end_station_id) %>%
mutate(se_same = ifelse(start_station_id==end_station_id, 1, 0)) -> station
d<-data.frame(table(station$se_same))
x <- d$Freq[2]/d$Freq[1]
sprintf("%.10f", x)
0.0228697215

# what is the standard deviation of the number of stations visited by a bike?
df %>%
select(start_station_id, end_station_id, bikeid) %>%
gather(variable, value, -bikeid) %>%
group_by(bikeid) %>%
summarize(count=n()) -> res
x <- sd(res$count)
sprintf("%.10f", x)
657.8235632455


# What is the difference, in seconds, between the longest and shortest average durations?
df %>%
select(tripduration, .id) %>%
group_by(.id) %>% summarize(mean=mean(tripduration)) -> res
x <- max(res$mean) - min(res$mean)
sprintf("%.10f", x)v
430.5702959700

# distance of a long-range bike traip
library(geosphere)
df$dist<-distm(c(start_station_latitude, start_station_longitude), c(end_station_latitude, end_station_longitude), fun = distHaversine)

lat1<-start_station_latitude
long1<-start_station_longitude
lat2<-end_station_latitude
end_station_longitude
gcd.slc <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
  return(d) # Distance in km
}

# What fraction of rides exceed their corresponding time limit?
# subscribers rides will have total per month less than 45
# customers rides will have rides greater than 30
df$starttime_c <- as.POSIXct(df$starttime, format = "%m/%d/%Y %H:%M")
df$stoptime_c <- as.POSIXct(df$stoptime, format = "%m/%d/%Y %H:%M")

df%>%
select(.id, usertype, starttime_c, stoptime_c) %>%
mutate(timediff=stoptime_c-starttime_c) -> timeres
timeres$timediffx <- gsub(" mins", "", timeres$timediff)
timeres %>%
mutate(overtimec=ifelse(usertype=="Customer" & timediffx > 30, 'overtime_cust', 'withintime_cust')) %>%
mutate(overtimes=ifelse(usertype=="Subscriber" & timediffx > 45, 'overtime_su', 'withintime_su')) -> timeres1
d<-data.frame(table(timeres1$overtimec, timeres1$overtimes))
x<-(d$Freq[2] + d$Freq[3])/nrow(timeres)
sprintf("%.10f", x)
0.3626106099

# Hourly usage fraction of a station to be the fraction of all rides starting at that station that leave during a specific 
df %>%
mutate(hr=hour(starttime_c)) %>%
# hourly use by station
select(tripduration, start_station_id, hr) %>%
group_by(hr, start_station_id) %>% summarize(hourlyuse=sum(tripduration)) -> timeres1
timeres1 %>%
group_by(start_station_id) %>% summarize(totaluse=sum(hourlyuse))-> timeres2
timeres1<-left_join(timeres1, timeres2, by='start_station_id')
timeres1 %>%
mutate(usefraction=hourlyuse/totaluse) -> timeres1
# hourly use general system
df %>%
select(tripduration, start_station_id, hr) %>%
group_by(hr) %>% summarize(hourlyuse=sum(tripduration), count=n()) -> timeressys
x<-sum(timeressys$hourlyuse)
timeressys %>%
mutate(usefraction=hourlyuse/x) -> timeressys

timeres1 %>%
group_by(hr, start_station_id) %>% summarize(minuse=min(usefraction), maxuse=max(usefraction))-> timeres3
names(timeressys)<-c('hr', 'hruse', 'sysusefrac')
timeres3<-left_join(timeres3, timeressys, by='hr')
timeres3 %>%
mutate(minratio=minuse/sysusefrac) %>%
mutate(maxratio=maxuse/sysusefrac) %>%
gather(variable, value, minratio:maxratio) -> timeres4
x<-max(timeres4$value)
sprintf("%.10f", x)
56.8957204881
