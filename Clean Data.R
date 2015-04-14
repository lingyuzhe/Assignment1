#man_2006/woman_2006 get HOMETOWN from HOMETOWNTIM

HOMETOWNNETTIMfile=
  function(i)
  {
    if(any(grepl("HOMETOWNNETTIM",names(results[[i]]))))
    {True=i}
  }

HOMETWONTIME=
function(i)
{
  HOMETOWNNETTIM=results[[i]]$HOMETOWNNETTIM
  pattern = "([a-zA-Z]+?)\\s+([0-9]+)+(:)+([0-9]+)+(:)+([0-9]+)"
  HOMETOWN= gsub(pattern, "\\1", HOMETOWNNETTIM)
  pattern = "([a-zA-Z]+?)\\s+([0-9]+)+(:)+([0-9]+)"
  HOMETOWN= gsub(pattern, "\\1", HOMETOWN)
  pattern = "([0-9]+)+(:)+([0-9]+)+(:)+([0-9]+)"
  HOMETOWN= gsub(pattern, "", HOMETOWN)
  HOMETOWN=gsub("^ ", "", HOMETOWN)
  HOMETOWN=gsub(" $", "", HOMETOWN)
  HOMETOWN
}

#get GUIDLINE from NETTIME
SpecGuidline=
  function(i)
  {
    pattern="\\s+([0-9]+[0-9]+[[:punct:]]+[0-9]+)"
    T=gsub(pattern,"0:\\1",results[[i]]$NETTIM)
    pattern="([0-9]+[[:punct:]]+[0-9]+[[:punct:]]+[0-9]+)+(.)"
    GUIDLINE=gsub(pattern,"\\2",T)
    GUIDLINE
  }

# Identify the data without DIV/TOT column

NonGUIDLINEfile=
  function(i)
  {
    if(!any(grepl("GUIDLINE",names(results[[i]]))))
    {True=i}
  }

# Identify the data without DIV/TOT column

NonDIVTOTfile=
  function(i)
  {
    if(!any(grepl("DIV/TOT",names(results[[i]]))))
    {True=i}
  }

#Get useful data for analysis
DataExtract=
  function(r)
  {
    r[c("YEAR","NAME","GENDER","AG","HOMETOWN","DIV/TOT","PLACE","TIME","GUIDLINE")]
  }

setwd("~/Dropbox/Statistics/STA242/Assignment1")
UsefulData=read.table("UsefulData",stringsAsFactors = FALSE)

#Get DIV&TOT
DIV.TOT=UsefulData$`DIV.TOT`[!is.na(UsefulData$`DIV.TOT`)]
DIV.TOT=gsub(intToUtf8(0xA0),"",DIV.TOT)
DIV.TOT=strsplit(DIV.TOT, "/")
UsefulData$DIV[!is.na(UsefulData$`DIV.TOT`)]=as.numeric(sapply(DIV.TOT, function(x){x[1]}))
UsefulData$TOT[!is.na(UsefulData$`DIV.TOT`)]=as.numeric(sapply(DIV.TOT, function(x){x[2]}))
remove(DIV.TOT)


#Get BirthYear
UsefulData$BirthYear=UsefulData$YEAR-UsefulData$AG

#Change Time Format

T=gsub("$"," ",UsefulData$TIME)
T2=gsub("$"," ",UsefulData$TIME[is.na(UsefulData$GUIDLINE)])
pattern="\\s+([0-9]+[0-9]+[[:punct:]]+[0-9]+)"
T=gsub(pattern,"0:\\1",T)
T2=gsub(pattern,"0:\\1",T2)
pattern="([0-9]+[[:punct:]]+[0-9]+[[:punct:]]+[0-9]+)+(.)"
TIME=as.difftime(gsub(pattern,"\\1",T), units = "mins")
UsefulData$`Time(mins)`=as.numeric(TIME)
UsefulData$GUIDLINE[is.na(UsefulData$GUIDLINE)]=gsub(pattern,"\\2",T2)
UsefulData=UsefulData[!is.na(UsefulData$`Time(mins)`),]
remove(T)
remove(T2)
remove(TIME)

# Get Hometown State/Country
library(stringr)
CS=str_trim(UsefulData$HOMETOWN, side = "both")
pattern="([[:blank:]])+([A-Z])+([A-Z])"
STATE=str_trim(str_extract(CS,pattern),side = "both")
state.abb[51]="DC"
STATE_CODE=match(STATE,state.abb)
STATE= state.abb[STATE_CODE]
UsefulData$STATE=STATE
UsefulData$STATE_CODE=STATE_CODE
UsefulData$CITY=tolower(gsub(pattern,"",str_trim(UsefulData$HOMETOWN, side = "both")))
remove(CS)
remove(STATE)
remove(STATE_CODE)
remove(state.abb)

#Set Id Frequency Experience
UsefulData$NAME=tolower(str_trim(UsefulData$NAME, side = "both"))
UsefulData$Identify=paste(UsefulData$NAME,UsefulData$BirthYear,UsefulData$HOMETOWN,UsefulData$GENDER)
UsefulData$Id=unclass(factor(UsefulData$Identify))
frequency=data.frame(table(UsefulData$Id))
UsefulData$Frequency=frequency$Freq[UsefulData$Id]

frequency=UsefulData[c("Identify","Frequency")]
frequency=frequency[order(frequency$Identify),]
Experience=
  function(i)
  {
    experience=frequency[frequency$Frequency==i,]
    rep(c(0:(i-1)),times=dim(experience)[1]/i)
  }
UsefulData=UsefulData[order(UsefulData$Identify),]
UsefulData=UsefulData[order(UsefulData$Frequency),]
UsefulData$Experience=unlist(lapply(c(1:max(UsefulData$Frequency)),Experience))

remove(frequency)

#GENDER
UsefulData$GENDER[UsefulData$GENDER == "men"] = 0
UsefulData$GENDER[UsefulData$GENDER == "women"] = 1
UsefulData$GENDER=as.numeric(UsefulData$GENDER)

#Generate DataforAnalysis

DataforAnalysis=UsefulData[c("YEAR","Identify","AG","BirthYear","GENDER","Frequency","Experience","STATE","STATE_CODE","CITY","TOT","GUIDLINE","Time(mins)","PLACE")]
write.table(DataforAnalysis,"~/Dropbox/Statistics/STA242/Assignment1/DataforAnalysis")

