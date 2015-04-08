readFile=
  function(x)
  {
    l=match("#",substr(readLines(file(x)),1,1))
    if (is.na(l)) {l=match(" #",substr(readLines(file(x)),1,2))}
    if (x=="women10Mile_2001") {txt = readLines(file("men10Mile_2001"))}  else
    {txt = readLines(file(x))}
    
    k=match("=",substr(txt,1,1))
    txt[k]=gsub(' ','x',txt[k])
    txt[k]=gsub("(\\w)\\b","\\U\\1",txt[k], perl=TRUE)
    txt[k]=gsub ('x','=',txt[k])
    txt[k]=gsub ('X',' ',txt[k])
    txt[k]=strsplit(txt[k], " ")
    w=nchar(txt[k][[1]])+1
    
    if (is.na(l)) {data=read.fwf(x,widths=w,skip = (k-2),comment.char = '')} else
    {data=read.fwf(x,widths=w,skip = (k-2),n=(l+1-k),comment.char = '')}
    
    if (is.na(t(data)[,1][1])) {header=t(read.fwf("men10Mile_2001",widths=w,skip = (k-2)))[,1]} else
    {header=t(data)[,1]}
    header=gsub(" ","",header)
    header=gsub(intToUtf8(0xA0),"",header)
    header=toupper(header)
    data=data[-c(1,2),]
    data=data[complete.cases(data),]
    write.table(data,"~/Dropbox/Statistics/STA242/Assignment1/z")
    data=read.table("~/Dropbox/Statistics/STA242/Assignment1/z",stringsAsFactors=FALSE)
    names(data)=header
    data
  }
