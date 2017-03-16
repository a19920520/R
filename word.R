library("RColorBrewer")
library("wordcloud")

library(jsonlite)
url <- 'E:/ETL/countforcloud2.json'
wordcount <- fromJSON(url, flatten = TRUE)

wordcount_first <- data.frame(word = wordcount[,1], freq = wordcount[,2], stringsAsFactors = FALSE)
wordcount_first$freq <- as.numeric(wordcount_first$freq)
wordcloud(wordcount_first$word, wordcount_first$freq, min.freq = 2000, random.order = F, ordered.colors = F, colors = rainbow(length(row.names(wordcount_first))))


# for(i in 1:34){
#   tmp <- tfidf[i,1]
#   time <- tfidf[i,2]
#   total <- paste(time, tmp, sep = ' ')
#   for( j in 1:10){
#    a  <- unlist(total)
#   }
# }

url_new <- 'E:/ETL/tfidf_test.json'
tfidf <- fromJSON(url_new, flatten = T)

test <- tfidf[[1]][4]
test <- as.data.frame(test)

colnames(test)[1]
test[1,1]
test_title <- paste(colnames(test)[1],test[1,1])

allmonth_top10 <- paste("")
for(i in 1:34){
  currenttime <- tfidf[i,2]
  # currenttime <- tfidf[i,1]
  top10_list <- paste("")
  for(j in 1:10){
    top10_number <- paste(colnames(as.data.frame(tfidf[[1]][i]))[j],tfidf[[i,1]][j,j])
    top10_list <- paste(top10_list, top10_number, sep = " ")
  }
  allmonth_top10 <- paste(allmonth_top10, top10_list, sep = " ")
}

allmonth_top10 <- paste("")
for(i in 1:34){
  currenttime <- tfidf[i,2]
  # currenttime <- tfidf[i,1]
  top10_list <- paste(currenttime)
  for(j in 1:10){
    top10_number <- paste(colnames(as.data.frame(tfidf[[1]][i]))[j],tfidf[[i,1]][j,j])
    top10_list <- cbind(top10_list, top10_number)
  }
  allmonth_top10 <- rbind(allmonth_top10, top10_list)
}

for(i in 2:34){
  if( colnames(as.data.frame(tfidf[[1]][i])[1]) ==
  colnames(as.data.frame(tfidf[[1]][i+1])[1]) ){
    print(colnames(as.data.frame(tfidf[[1]][i])[1]))
  }else{
    print(colnames(as.data.frame(tfidf[[1]][i+1])[1]))
  }
}

allmonth_top10 <- paste("")
for(i in 1:34){
  currenttime <- tfidf[i,2]
  # currenttime <- tfidf[i,1]
  top10_list <- paste(currenttime)
  for(j in 1:10){
    if(is.na(colnames(as.data.frame(tfidf[[1]][i]))[j])){
      top10_number <- paste("no chr","0")
    }else{
      top10_number <- paste(colnames(as.data.frame(tfidf[[1]][i]))[j],tfidf[[i,1]][j,j])
    }
    top10_list <- cbind(top10_list, top10_number)
  }
  allmonth_top10 <- rbind(allmonth_top10, top10_list)
}

allmonth_top10 <- paste("")
for(i in 1:34){
  currenttime <- tfidf[i,2]
  # currenttime <- tfidf[i,1]
  top10_list <- paste(currenttime)
  for(j in 1:10){
    if(is.na(colnames(as.data.frame(tfidf[[1]][i]))[j])){
      top10_number <- paste("no chr")
    }else{
      top10_number <- paste(colnames(as.data.frame(tfidf[[1]][i]))[j])
    }
    top10_list <- cbind(top10_list, top10_number)
  }
  allmonth_top10 <- rbind(allmonth_top10, top10_list)
}

for(i in 2:33){
  for(j in 1:10){
    if(is.na(colnames(as.data.frame(tfidf[[1]][i]))[j])){
      title_chr <- paste("no chr")
    }else{
      title_chr <- paste(colnames(as.data.frame(tfidf[[1]][i]))[j])
    }
    if(title_list %in% title_chr){
      title_list <- rbind(title_list, title_chr)
    }
  }
  
}

# table <- read.csv("e:/etl/tripadvisor_top1review_tfidf.csv", header=F, stringsAsFactors=FALSE)
Sys.setlocale(category = "LC_ALL", locale = "us")
# Sys.setlocale(category = "LC_ALL", locale = "cht")
table <- read.csv("e:/etl/tripadvisor_top1review_tfidf_comma.csv", header=T,  encoding = "UTF-8")
table$X <- NULL

table_mat <- matrix()
table_list <- c()

count_1=1
txt_1 <- table[1,1]
while(!is.na(strsplit(txt_1[1],"\t",fixed = T,perl = F,useBytes = T)[[1]][count_1])){
  table_chr <- strsplit(txt_1[1],"\t",fixed = T,perl = F,useBytes = T)[[1]][count_1]
  count_1 = count_1+1
  table_list <- cbind(table_list, table_chr)
}
table_mat <- rbind(table_mat, table_list)

for (i in 2:1279){
  txt <- table[i,1]
  count_2=2
  while(!is.na(strsplit(txt, "\t")[[1]][count_2])){
    table_chr <- strsplit(txt, "\t")[[1]][count_2]
    count_2 = count_2+1
    table_list <- cbind(table_list, table_chr)
  }
  table_mat <- rbind(table_mat, table_list)
}

