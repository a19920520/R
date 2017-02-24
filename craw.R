
#��HTML���M��

install.packages("bitops")
install.packages("RCurl")
install.packages("XML")

#��r�����M��(�٨S�Ψ�)

install.packages("RColorBrewer")
install.packages("wordcloud")

library(bitops)
library(RCurl)
library(XML)

library(RColorBrewer)
library(wordcloud)

#���إߤ@��list�ΨӦs���U�����

data <- list()

#�j�骦�C�ӭ��������}

for( i in 1000:1001){
  #�C�@�����Ʀr�ܴ�
  tmp <- paste(i, '.html', sep='')
  #��e��ɤW�����@��URL
  url <- paste('https://www.ptt.cc/bbs/Japan_Travel/index', tmp, sep='')
  #���URL���㭶��l�X�s�U��
  html <- htmlParse(getURL(url))
  #�D��C�Ӥ��������}
  url.list <- xpathSApply(html, "//div[@class='title']/a[@href]", xmlAttrs)
  #��C�ӳs���g�Jlist��
  data <- rbind(data, paste('https://www.ptt.cc', url.list, sep=''))
}

#�g�@�Ө�ƥsgetdoc

getdoc <- function(line){
  #�C��line��https��h���Y
  start <- regexpr('https', line)[1]
  #�C��line��html��h����
  end <- regexpr('html', line)[1]
  
  #�P�O��ɰ���
  if(start != -1 & end != -1){
    #Ū���C��URL,end+3���wŪ��html��l
    url <- substr(line, start, end+3)
    #���o�C�Ӻ��}����l�X
    html <- htmlParse(getURL(url), encoding='UTF-8')
    #���o�bmain��������r
    doc <- xpathSApply(html, "//div[@id='main-content']", xmlValue)
    #����C�Ӻ��}�s������
    name <- strsplit(url, '/')[[1]][6]
    #��峹�s�줣�P�s����txt�Hname�R�W
    write(doc, gsub('html', 'txt', name))
  }      
}

#��sapply�Ӷ]getdoc����ƨӧ�data�����C�ӳs��������r�g���ӧO��r��
sapply(data, getdoc)

#��r���ήM��

install.packages("rJava")
install.packages("E:/R/tmcn_0.1-4.zip", repos = NULL, type = "win.binary")
install.packages("E:/R/Rwordseg_0.2-1.zip", repos = NULL, type = "win.binary")
install.packages("NLP")
install.packages("tm")

library(rJava)
library(tmcn)
library(Rwordseg)
library(NLP)
library(tm)


d.corpus <- Corpus(DirSource("E:/R/txt"), list(language = NA))
d.corpus <- tm_map(d.corpus, removePunctuation)
d.corpus <- tm_map(d.corpus, removeNumbers)

d.corpus <- tm_map(d.corpus, function(word) {
  gsub("[A-Za-z0-9]", "", word)
})

#------------------------------------------

install.packages("RMySQL")
install.packages("DBI")

library(RMySQL)
library(DBI)

con <- dbConnect(MySQL(), dbname = "craw", username="root", password="live711043")
# dbSendQuery(con,"SET NAMES utf8")
dbSendQuery(con,"SET NAMES big5")
table <- dbGetQuery(con, "select * from latest")
#latest is MySQL's table

table[,3]
write.table(table[,3], file="latest_title.txt")

d.corpus <- Corpus(DirSource("E:/R/craw" , mode="text"), list(language = NA))
d.corpus <- tm_map(d.corpus, removePunctuation)
d.corpus <- tm_map(d.corpus, removeNumbers)
d.corpus <- tm_map(d.corpus, function(word) {
  gsub("[A-Za-z0-9]", "", word)
})

inspect(d.corpus)

d.corpus <- tm_map(d.corpus, segmentCN, nature = TRUE)
d.corpus <- tm_map(d.corpus, PlainTextDocument)
d.corpus <- Corpus(VectorSource(d.corpus))
tdm <- TermDocumentMatrix(d.corpus, control = list(wordLengths = c(2, Inf)))



inspect(tdm)