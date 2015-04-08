setwd("~/Dropbox/Statistics/STA242/Assignment1")
source("readFile.R")
source("Clean Data.R")
setwd("~/Dropbox/Statistics/STA242/Assignment1/data")
filenames = list.files("~/Dropbox/Statistics/STA242/Assignment1/data")

results=list()
results=sapply(filenames,readFile)

#man_2006/woman_2006 get HOMETOWN from HOMETOWNTIM
for (i in c(8,20))
{
  results[[i]]$HOMETOWN=HOMETWONTIME(i)
}

#TIME variable
for (i in 1:length(filenames))
{
  names(results[[i]])=NameTime(i)
  results[[i]]$TIMEINSEC=TimeFormat(i)$TIME
  results[[i]]$GUIDLINE=TimeFormat(i)$GUIDLINE
  results[[i]]$Gender=GenderYear(i)$Gender
  results[[i]]$Year=GenderYear(i)$Year
}

for (i in c(11,12,23,24))
{
  results[[i]]$GUIDLINE=SpecGuidline(i)
}

results[[11]]$PLACE=as.numeric(gsub(intToUtf8(0xA0),"",results[[11]]$PLACE))
results[[16]]$AG=as.numeric(gsub("XX","",results[[16]]$AG))

Name=results[[1]]$NAME
Place=results[[1]]$PLACE
Age=results[[1]]$AG
Hometown=results[[1]]$HOMETOWN
Time=results[[1]]$TIMEINSEC
Guidline=results[[1]]$GUIDLINE
Gender=results[[1]]$Gender
Year=results[[1]]$Year

for(i in 2:24)
{
  Name=c(Name,results[[i]]$NAME)
  Place=c(Place,results[[i]]$PLACE)
  Age=c(Age,results[[i]]$AG)
  Hometown=c(Hometown,results[[i]]$HOMETOWN)
  Time=c(Time,results[[i]]$TIMEINSEC)
  Guidline=c(Guidline,results[[i]]$GUIDLINE)
  Gender=c(Gender,results[[i]]$Gender)
  Year=c(Year,results[[i]]$Year)
}

data=data.frame(Name,Place,Age,Hometown,Time,Guidline,Gender,Year,stringsAsFactors=FALSE)
write.table(data,"~/Dropbox/Statistics/STA242/Assignment1/final")
