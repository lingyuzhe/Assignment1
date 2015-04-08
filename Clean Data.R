#man_2006/woman_2006 get HOMETOWN from HOMETOWNTIM
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


#Change GUN/GUNTIM to TIME,Delete #&*,Change Time Format
NameTime=
function(i)
{
    x=gsub("GUNTIM","TIME",names(results[[i]]))
    x=gsub("GUN","TIME",x)
    x
}

TimeFormat=
  function(i)
 {
    T=gsub("$"," ",results[[i]]$TIME)
    pattern="\\s+([0-9]+[0-9]+[[:punct:]]+[0-9]+)"
    T=gsub(pattern,"0:\\1",T)
    pattern="([0-9]+[[:punct:]]+[0-9]+[[:punct:]]+[0-9]+)+(.)"
    TIME=as.difftime(gsub(pattern,"\\1",T), units = "secs")
    TIME=as.numeric(TIME)
    GUIDLINE=gsub(pattern,"\\2",T)
    data.frame(TIME,GUIDLINE,stringsAsFactors=FALSE)
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

#get Gender and Year from Filenames
GenderYear=
function(i)
{
  pattern="([a-zA-Z]+)+([0-9]+)+([a-zA-Z]+)+([[:punct:]])+([0-9]+)"
  Gender=gsub(pattern,"\\1",filenames[i])
  Year=as.numeric(gsub(pattern,"\\5",filenames[i]))
  data.frame(Gender,Year,stringsAsFactors=FALSE)
}


