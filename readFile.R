readFile=
  function(x)
{
    txt = readLines(file(x))
    k=match("=",substr(txt, 1,1))
    txt[k]=strsplit(txt[k], " ",)
    w=nchar(txt[k][[1]])+1
    x=read.fwf(x,widths=w,skip = (k-2),)
    names(x)=t(x)[,1]
    x=x[-c(1,2),]
    x
    close(txt)
}

x="men10Mile_1999"
y=readFile(x)