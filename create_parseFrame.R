### create parseFrame ###

library(data.table)
Sys.setlocale(locale="Chinese") 
setwd("G:/Nicole/")
options(stringsAsFactors = FALSE)

sFrame <- as.data.frame(fread(input = "preprocessing/sFrame_encoding.csv",stringsAsFactors = F))
#sFrame <- read.csv("ClassifierBase/CCD/data/sFrame.csv",fileEncoding="UTF-8",stringsAsFactors=FALSE,nrows = 200)
Encoding(sFrame[,1])<-"UTF-8"
Encoding(sFrame[,2])<-"UTF-8"
Encoding(sFrame[,3])<-"UTF-8"
Encoding(sFrame[,4])<-"UTF-8"

# read from file
sentences <- readLines("stanford-parser-full-2015-04-20/data/classifiers-toParse.txt")
Encoding(sentences) <- "UTF-8"

#check length of sentences -->number of words, including comma ect.
sentence.length <- function(x){
  length(strsplit(x,split=" ")[[1]])
}
length <- unlist(lapply(X = sentences,FUN = sentence.length))

parseFrame <- data.frame(sentence=sentences,words=length,
                         corpus = sFrame$corpus,
                         classifier=sFrame$classifier,
                         sFrame=rownames(sFrame))

write.csv(parseFrame,"preprocessing/parseFrame_complete.csv",
          fileEncoding="UTF-8",row.names=F)

#delete rubbish
delete <- grep("href http weibo com",parseFrame$sentence)
parseFrame_trimmed <- parseFrame[-delete,]

#sort
i <- order(parseFrame_trimmed$words)
parseFrame_trimmed <- parseFrame_trimmed[i,]

#maybe just delete long sentences?
long <-which(parseFrame_trimmed$words>100) 
parseFrame_trimmed <- parseFrame_trimmed[-long,]
#437 sentences with more than 100 words
#110 sentences with more than 110 words

short <-which(parseFrame_trimmed$words<4) #delete
parseFrame_trimmed <- parseFrame_trimmed[-short,]

write.csv(parseFrame_trimmed,"preprocessing/parseFrame_trimmed.csv",
          fileEncoding="UTF-8",row.names=F)

# 
#overview
par(mfrow = c(1, 2))

library(plotrix)
myboxplot <-function(x,hight,title){
  table <- t(summary(x))
  boxplot(x,ylab="Length in words")
  title(title)
  mtext(paste(length(x)," sentences ","(",
              round(length(x)/860256*100,2),"%)",sep = ""),cex=0.9)
  addtable2plot(x=0.3,y=hight,table = table,ypad = -0.2,cex=0.9) #add table
}

myhist <-function(x,hight,title){
  # table <- t(summary(x))
  hist(x,xlab="Sentence length in words",main=title,ylab="Counts")
  mtext(paste(length(x)," sentences ","(",
              round(length(x)/860256*100,2),"%)",sep = ""),cex=0.9)
  # addtable2plot(x=-47,y=hight,table = table,ypad = -0.2,cex=0.9) #add table
}

# original vs. trimmed
myboxplot(parseFrame$words,-140,"Sentence length in original data")
myboxplot(parseFrame_trimmed$words,-27.75,"Sentence length in trimmed data")
#par(mfrow = c(1, 1))

par(mfrow = c(1, 2))
myhist(parseFrame$words,-319365,"Original data")
myhist(parseFrame_trimmed$words,-27.75,"Trimmed data")
mypoint <- locator(1) 
par(mfrow = c(1, 1))

#Weibo vs. LCMC/UCLA

par(mfrow = c(2, 2))
WEIBO <- which(parseFrame$corpus=="Weibo")
myhist(parseFrame[WEIBO,]$words,-319365,"Original data - Weibo")
myhist(parseFrame[-WEIBO,]$words,-27.75,"Original data - LCMC/UCLA")
WEIBO <- which(parseFrame_trimmed$corpus=="Weibo")
myhist(parseFrame_trimmed[WEIBO,]$words,-319365,"Trimmed data - Weibo")
myhist(parseFrame_trimmed[-WEIBO,]$words,-27.75,"Trimmed data - LCMC/UCLA")
mypoint <- locator(1) 
par(mfrow = c(1, 1))

#split into parts
get.border <- function(df,parts){
  total <- nrow(df)
  size <- round(total/parts)
  from <- 0
  to <- 0
  for(i in 1:parts){
    from[i] <- size*(i-1)+1
    to[i] <- size*i  
  }
  to[length(to)]<-nrow(df)
  return(list(from,to))
}
border <-get.border(parseFrame_trimmed,8)
border[[1]][1]
border[[2]][1]

#length of each file
length<-length(border[[1]][1]:border[[2]][1])#106908
length_last<-length(border[[1]][8]:border[[2]][8])#106911
length*7+length_last==nrow(parseFrame_trimmed)#good!

# parseFrame$sentence <- del.anno.keep.space(parseFrame$sentence)
# parseFrame_trimmed$sentence <- del.anno.keep.space(parseFrame_trimmed$sentence)

#8 files, each 106908 rows

#write file
dir.create("G:/Nicole/stanford-parser-full-2015-04-20/data/classifiers")
for(i in 1:length(border[[1]])){
  path <- paste("G:/Nicole/stanford-parser-full-2015-04-20/data/classifiers/","classifiers-toParse",i,".txt",sep = "")
  from<-border[[1]][i]
  to<-border[[2]][i]
  write.table(parseFrame_trimmed[from:to,]$sentence,path,fileEncoding="UTF-8",quote = F,row.names = F,col.names = F,eol = "\n")
}
