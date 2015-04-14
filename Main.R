setwd("~/Dropbox/Statistics/STA242/Assignment1")
source("readFile.R")
source("Clean Data.R")
library(stringr)

setwd("~/Dropbox/Statistics/STA242/Assignment1/data")
filenames = list.files()

results=list()
results=sapply(filenames,readFile)

Filenum=length(filenames)

#Get HOMETOWN from HOMETOWNETIM

c=unlist(sapply(c(1:Filenum),HOMETOWNNETTIMfile))
for (i in c)
{
  results[[i]]$HOMETOWN=HOMETWONTIME(i)
}

#Get DIV/TOT column
c=unlist(sapply(c(1:Filenum),NonDIVTOTfile))
for (i in c)
{
  results[[i]]$`DIV/TOT`=NA
}

#Get GUIDLINE from NETTIME

for (i in c(11,12,23,24))   #if time available, write a function to identity this
{
  results[[i]]$GUIDLINE=SpecGuidline(i)
}

c=unlist(sapply(c(1:Filenum),NonGUIDLINEfile))
for (i in c)
{
  results[[i]]$GUIDLINE=NA
}

#Special Case for data class
results[[11]]$PLACE=as.numeric(gsub(intToUtf8(0xA0),"",results[[11]]$PLACE))
results[[16]]$AG=as.numeric(gsub("XX","",results[[16]]$AG))

#Get Useful Data
UsefulData=lapply(results,DataExtract)
UsefulData=do.call(rbind, UsefulData)

write.table(UsefulData,file="~/Dropbox/Statistics/STA242/Assignment1/UsefulData")