setwd("~/Dropbox/Statistics/STA242/Assignment1/data")
readFile=
  function(x)
{
    txt = readLines(file(x))
    k=match("=",substr(txt, 1,1))
    txt[k]=gsub(' ','x',txt[k])
    txt[k]=gsub("(\\w)\\b","\\U\\1",txt[k], perl=TRUE)
    txt[k]=gsub ('x','=',txt[k])
    txt[k]=gsub ('X',' ',txt[k])
    txt[k]=strsplit(txt[k], " ",)
    w=nchar(txt[k][[1]])+1
    y=read.fwf(x,widths=w,skip = (k-2),comment.char = '')
    names(y)=t(y)[,1]
    y=y[-c(1,2),]
    y=y[complete.cases(y),]
    write.table(y,file=x)  #this will replace the original data
    y2=read.table(x)
  }


x="men10Mile_1999"
t=readFile(x)


