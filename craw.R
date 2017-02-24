
#爬HTML的套件

install.packages("bitops")
install.packages("RCurl")
install.packages("XML")

#文字雲的套件(還沒用到)

install.packages("RColorBrewer")
install.packages("wordcloud")

library(bitops)
library(RCurl)
library(XML)

library(RColorBrewer)
library(wordcloud)

#先建立一個list用來存等下的資料

data <- list()

#迴圈爬每個頁面的網址

for( i in 1000:1001){
  #每一頁的數字變換
  tmp <- paste(i, '.html', sep='')
  #把前後補上成為一個URL
  url <- paste('https://www.ptt.cc/bbs/Japan_Travel/index', tmp, sep='')
  #把該URL的整頁原始碼存下來
  html <- htmlParse(getURL(url))
  #挑選每個內頁的網址
  url.list <- xpathSApply(html, "//div[@class='title']/a[@href]", xmlAttrs)
  #把每個連結寫入list中
  data <- rbind(data, paste('https://www.ptt.cc', url.list, sep=''))
}

#寫一個函數叫getdoc

getdoc <- function(line){
  #每個line的https的h當頭
  start <- regexpr('https', line)[1]
  #每個line的html的h當尾
  end <- regexpr('html', line)[1]
  
  #判別何時停止
  if(start != -1 & end != -1){
    #讀取每個URL,end+3指定讀到html的l
    url <- substr(line, start, end+3)
    #取得每個網址的原始碼
    html <- htmlParse(getURL(url), encoding='UTF-8')
    #取得在main的內頁文字
    doc <- xpathSApply(html, "//div[@id='main-content']", xmlValue)
    #先把每個網址編號切割
    name <- strsplit(url, '/')[[1]][6]
    #把文章存到不同編號的txt以name命名
    write(doc, gsub('html', 'txt', name))
  }      
}

#用sapply來跑getdoc的函數來把data中的每個連結內的文字寫成個別文字檔
sapply(data, getdoc)

#文字切割套件

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