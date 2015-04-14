FindHeader=
function(x)
{
  txt=readLines(file(x))
  k=match("=",substr(txt,1,1))
  txt[k]=gsub(' ','x',txt[k])
  txt[k]=gsub("(\\w)\\b","\\U\\1",txt[k], perl=TRUE)
  txt[k]=gsub ('x','=',txt[k])
  txt[k]=gsub ('X',' ',txt[k])
  txt[k]=strsplit(txt[k], " ")
  w=nchar(txt[k][[1]])+1
  if(is.na(k)){Header=character()} else
  {Header=read.fwf(x,widths=w,skip = k-2,n=1,comment.char = '',stringsAsFactors=FALSE)}
  Header=gsub(" ","",Header)
  Header=gsub(intToUtf8(0xA0),"",Header)
  Header=toupper(Header)
  Header=gsub("GUNTIM","TIME",Header)
  Header=gsub("GUN","TIME",Header)
  list(txt=txt,k=k,w=w,Header=Header)
}

FindBody=
function(t)
{
  pattern="([0-9]+):([0-9]+)"
  rows=grep(pattern,t)
  Body=t[rows]
}


readFile=
function(x)
  {
    pattern="([a-zA-Z]+)+([0-9]+)+([a-zA-Z]+)+([[:punct:]])+([0-9]+)"
    GENDER=gsub(pattern,"\\1",x)
    YEAR=as.numeric(gsub(pattern,"\\5",x))
    
    txt=FindHeader(x)$txt
      
    k=FindHeader(x)$k
    w=FindHeader(x)$w
    Header=unlist(FindHeader(x)$Header)
    if(is.na(k)){
      if (GENDER=="men"){
        y=gsub(pattern,"women\\2\\3\\4\\5",x)
        w=FindHeader(y)$w
        Header=unlist(FindHeader(y)$Header)
                        } else
                         { 
                         y=gsub(pattern,"men\\2\\3\\4\\5",x)
                         w=FindHeader(y)$w
                         Header=unlist(FindHeader(y)$Header)
                          }
                        }
    Body=textConnection (unlist(FindBody(txt)))
    data=read.fwf(Body,widths=w,comment.char = '',stringsAsFactors=FALSE)
    close(Body)
    
    names(data)=Header
    data$GENDER=GENDER
    data$YEAR=YEAR
    data
}
