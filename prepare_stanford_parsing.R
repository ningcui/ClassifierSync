# clean annotation and save to file for parser
library(data.table)

Sys.setlocale(locale="Chinese") 
setwd("G:/Nicole/")
sFrame <- as.data.frame(fread(input = "preprocessing/sFrame_encoding.csv",stringsAsFactors = F))
#sFrame <- read.csv("ClassifierBase/CCD/data/sFrame.csv",fileEncoding="UTF-8",stringsAsFactors=FALSE,nrows = 200)
Encoding(sFrame[,1])<-"UTF-8"
Encoding(sFrame[,2])<-"UTF-8"
Encoding(sFrame[,3])<-"UTF-8"
Encoding(sFrame[,4])<-"UTF-8"

del.anno.keep.space <- function(sentences){
    gsub("(.*?)/..?.? ","\\1 ",sentences)
}

sentences <- del.anno.keep.space(sFrame$sentence)

path <- "preprocessing/classifiers-toParse.txt"
write.table(sentences,path,fileEncoding="UTF-8",quote = F,row.names = F,col.names = F,eol = "\n")
