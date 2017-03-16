install.packages("RMongo")

library(rJava)
library(RMongo)


# library(rmongodb)
# host <- "10.120.37.119:27017"
# username <- "mongo"
# password <- "mongo"
# db <- "project"
# 
# mongo<-mongo.create(host=host,db=db,username=username,password=password)


mongo <- mongoDbConnect("project", "10.120.37.119", 27017)
auth <- dbAuthenticate(mongo, "mongo", "mongo")

Sys.setlocale(category = "LC_ALL", locale = "eng")
# Sys.setlocale(category = "LC_ALL", locale = "cht")

dbGetQuery(mongo, "content", "", 0, 10)

#印出時間跟次數(預設)
test_monthly <- dbAggregate(mongo, "content", c('{"$match":{content:{"$regex":"新宿御苑"}}}',
                                                '{"$group": {"_id": "$period",count: {"$sum": 1 }}}',
                                                '{"$sort": {"_id": -1 }}'))
test_monthly <- as.data.frame(test_monthly, stringsAsFactors = F)

#把時間隱藏只印次數
test_monthly <- dbAggregate(mongo, "content", c('{"$match":{content:{"$regex":"新宿御苑"}}}',
                                                '{"$group": {"_id": "$period",count: {"$sum": 1 }}}',
                                                '{"$sort": {"_id": -1 }}',
                                                '{"$project":{_id: 0 ,count : 1}}'))
test_monthly <- as.data.frame(test_monthly, stringsAsFactors = F)

# month <- NULL
# for(i in 1:nrow(test_monthly)){
#   month <- rbind(month,strsplit(test_monthly$test_monthly,",")[[i]][1])
# }


#把字串的時間與次數切割出來,並存成兩列
month <- NULL
count <- NULL
for(i in 1:nrow(test_monthly)){
  # month <- rbind(month,strsplit((strsplit(test_monthly$test_monthly,",")[[i]][1]),":")[[1]][2])
  month <- rbind(month,substr(comb_monthly[k,1],12,17))
  count <- rbind(count,gsub("}","",strsplit((strsplit(test_monthly$test_monthly,",")[[i]][2]),":")[[1]][2]))
}

test_monthly <- as.data.frame(cbind(month,count), stringsAsFactors = F)
colnames(test_monthly) <- c("month","count")
test_monthly$count <- as.numeric(test_monthly$count)


# =============================================================

library("jsonlite")

# mongo <- mongoDbConnect("project", "10.120.37.119", 27017)
# auth <- dbAuthenticate(mongo, "mongo", "mongo")
# 
# url <- "E:/Rmongo/tokyo_onlyspot_treated3.json"
# txt <- fromJSON(url, flatten = TRUE)
# 
# view_list <- txt1[1:10]

mongo <- mongoDbConnect("project", "10.120.37.119", 27017)
auth <- dbAuthenticate(mongo, "mongo", "mongo")

url1 <- "E:/Rmongo/w2v397.json"
txt1 <- fromJSON(url1, flatten = TRUE)
txt1 <-as.data.frame(txt1)

view_list <- txt1[2:397,1]

monthly_list <- NULL
for(i in 2017:2000){
  year_id <- i
  for( j in 12:1){
    month_id <- j
    if(j <= 9){
      month_id <- paste("0", month_id, sep = "")
    }
    monthly_list <- rbind(monthly_list, paste(year_id, month_id, sep = ""))
  }
}

monthly_list <- as.data.frame(monthly_list, stringsAsFactors = F)

# total_monthly <- NULL
for(i in 1:length(view_list)){
  command <- c('{"$match":{city_treated:{"$regex":"關鍵字"}}}',
               '{"$group": {_id: "$periodnew",count: {"$sum": 1 }}}',
               '{"$sort": {_id: -1 }}',
               '{"$project":{count : 1}}')
  #在哪個部分選:content, city_treated
  word <- view_list[i]
  command <- gsub("關鍵字", word, command)

  part_monthly <- dbAggregate(mongo, "tripcontentcut", command) #在哪個資料庫 alltravel, tripcontentcut
  part_monthly <- as.data.frame(part_monthly, stringsAsFactors = F)

#==========================================================================================================
    
  # bind_list <- NULL
  # 
  # for(l in 1:nrow(part_monthly)){
  #   if(substr(part_monthly[l,1],12,17) %in% substr(part_monthly[l+1,1],12,17)){
  #     part_number1 <- (gsub("}", "", strsplit((strsplit(part_monthly$part_monthly, ",")[[l]][2]), ":")[[1]][2]))
  #     part_number2 <- (gsub("}", "", strsplit((strsplit(part_monthly$part_monthly, ",")[[l+1]][2]), ":")[[1]][2]))
  #     part_number <- as.numeric(part_number1) + as.numeric(part_number2)
  # 
  #     N <- paste('"count" :', part_number)
  #     N <- gsub("  ", " ", N)
  # 
  #     part_replace <- ('{ "_id" : "197001" , "count" : 0}')
  #     part_replace <- gsub(197001, substr(part_monthly[l,1],12,17), part_replace)
  #     part_replace <- gsub('"count" : 0', N, part_replace)
  # 
  #     part_monthly[l,1] <- part_replace
  #     # part_monthly <- part_monthly[(-i-1),1]
  #     # part_monthly <- as.data.frame(part_monthly, stringsAsFactors = F)
  #     #
  #     # i <- i + 1
  #     # nrow_part_monthly <- nrow_part_monthly - 1
  # 
  #     bind_list <- c(bind_list, l+1)
  # 
  #     l <- l + 2
  # 
  #     print(l)
  #   }else{
  #     part_number <- (gsub("}", "", strsplit((strsplit(part_monthly$part_monthly, ",")[[l]][2]), ":")[[1]][2]))
  #     N <- paste('"count" :', part_number)
  #     N <- gsub("  ", " ", N)
  #     part_replace <- ('{ "_id" : "197001" , "count" : 0}')
  #     part_replace <- gsub(197001, substr(part_monthly[l,1],12,17), part_replace)
  #     part_replace <- gsub('"count" : 0', N, part_replace)
  # 
  #     part_monthly[l,1] <- part_replace
  # 
  #     print(l)
  #   }
  # 
  # }
  # 
  # for(m in length(bind_list):1){
  #   delete_number <- 0 - (bind_list[m])
  #   part_monthly <- part_monthly[delete_number, 1]
  #   part_monthly <- as.data.frame(part_monthly, stringsAsFactors = F)
  # }

  #==========================================================================================================
    
  comb_monthly <- part_monthly
  
  # 補上關鍵次出現0次的月份
  for(j in 1:nrow(monthly_list)){
    if(monthly_list[j,1] %in% substr(comb_monthly[j,1],12,17)){
      print(j)
    }else{
      month_replace <- ('{ "_id" : "197001" , "count" : 0}') #預設填空月份值
      month_replace <- gsub(197001, monthly_list[j,1] , month_replace) #取代為空缺的月份值
      comb_monthly1 <- as.data.frame(comb_monthly[1:(j-1),1], stringsAsFactors = F)
      colnames(comb_monthly1) <- "year_month"
      if(j == 1){
        comb_monthly2 <- as.data.frame(comb_monthly[(j+1):nrow(comb_monthly),1], stringsAsFactors = F)
        # comb_monthly <- rbind(month_replace, comb_monthly1)
      }else{
        comb_monthly2 <- as.data.frame(comb_monthly[j:nrow(comb_monthly),1], stringsAsFactors = F)
        # comb_monthly <- rbind(comb_monthly1, month_replace)
      }
      colnames(comb_monthly2) <- "year_month"
      
      if(j == 1){
        comb_monthly <- rbind(month_replace, comb_monthly1)
      }else{
        comb_monthly <- rbind(comb_monthly1, month_replace)
      }

      colnames(comb_monthly) <- "year_month"
      comb_monthly <- rbind(comb_monthly, comb_monthly2)
    }
  }
  comb_monthly <- comb_monthly[-217:-1000,]
  comb_monthly <- as.data.frame(comb_monthly, stringsAsFactors = F)
  
  sort_month <- NULL
  sort_count <- NULL
  for(k in 1:nrow(comb_monthly)){
    # sort_month <- rbind(sort_month,strsplit((strsplit(part_monthly$part_monthly,",")[[k]][1]),":")[[1]][2])
    # substr(sort_month[1],3,8)
    # sort_month <- rbind(sort_month,substr(comb_monthly[k,1],12,17))
    sort_count <- rbind(sort_count,gsub("}","",strsplit((strsplit(comb_monthly$comb_monthly,",")[[k]][2]),":")[[1]][2]))
  }
  # total_monthly <- as.data.frame(cbind(total_monthly,sort_count), stringsAsFactors = F)
  sort_count <- as.data.frame(sort_count, stringsAsFactors = F)
  monthly_list <- as.data.frame(cbind(monthly_list, sort_count), stringsAsFactors = F)
}

# colnames(monthly_list) <- view_list
colnames(monthly_list) <- c("yy-mm",view_list)


# write.table(monthly_list, file = "monthly_list.csv", sep = ",", row.names = F)
write.table(monthly_list, file = "monthly_list_travel.csv", sep = ",", row.names = F)


# test_list <- NULL
# for(i in 1:length(view_list)){
#   test_name <- NULL
#   test_name <- paste(view_list[i], "_count", sep = "")
#   test_list <- cbind(test_list, test_name)
#   test_list[i] <- (i)
# }

monthly_list <- NULL

for(i in 2016:2008){
  year_id <- i
  for( j in 12:1){
    month_id <- j
    if(j <= 9){
      month_id <- paste("0", month_id, sep = "")
    }
    monthly_list <- rbind(monthly_list, paste(year_id,month_id, sep = ""))
  }
}

#==================================================================

comb_monthly <- test_monthly

for(i in 1:nrow(monthly_list)){
  if(monthly_list[i] %in% substr(comb_monthly[i,1],12,17)){
    print("ok")
  }else{
    month_replace <- ('{ "_id" : "200001" , "count" : 0}')
    month_replace <- gsub(200001, monthly_list[i] , month_replace)
    comb_monthly1 <- as.data.frame(comb_monthly[1:(i-1),1], stringsAsFactors = F)
    colnames(comb_monthly1) <- "year_month"
    comb_monthly2 <- as.data.frame(comb_monthly[i:nrow(comb_monthly),1], stringsAsFactors = F)
    colnames(comb_monthly2) <- "year_month"
    comb_monthly <- rbind(comb_monthly1, month_replace)
    colnames(comb_monthly) <- "year_month"
    comb_monthly <- rbind(comb_monthly, comb_monthly2)
  }
}

comb_monthly <- comb_monthly[-109:-120,]

# for(i in nrow(monthly_list)){
#   print(monthly_list[i])
#   print(substr(test_monthly[i,1],12,17))
# }

#==================================================================


test_dately <- dbAggregate(mongo, "content", c('{"$match":{content:{"$regex":"明治神宮"}}}',
                                               '{"$group":{_id: "$pub_datetime",count: {"$sum": 1 }}}',
                                               '{ $project : { _id : 0,
		                                                           title : 1,
                                                               month : { $substr: ["$_id", 4, 3] },
                                                          		 day : { $substr: ["$_id", 8, 2] },
                                                          		 year : { $substr: ["$_id", 20, 4] },
                                                          		 count : 1 }}',
                                               '{ $sort : { year: -1, month : -1, count : -1 }}'))
test_dately <- as.data.frame(test_dately, stringsAsFactors = F)

#====================================================================

i <- 9
command <- c('{"$match":{city_treated:{"$regex":"關鍵字"}}}',
             '{"$group": {_id: "$period",count: {"$sum": 1 }}}',
             '{"$sort": {_id: -1 }}',
             '{"$project":{count : 1}}')
#在哪個部分選:content, city_treated
word <- view_list[i]
command <- gsub("關鍵字", word, command)

part_monthly <- dbAggregate(mongo, "tripcontentcut", command) #在哪個資料庫 alltrival, tripcontentcut
part_monthly <- as.data.frame(part_monthly, stringsAsFactors = F)
part_monthly_tr <- part_monthly

bind_list <- NULL

# nrow_part_monthly <- nrow(part_monthly)
# for(i in 1:nrow_part_monthly){
for(i in 1:nrow(part_monthly)){
  if(substr(part_monthly[i,1],12,17) %in% substr(part_monthly[i+1,1],12,17)){
    part_number1 <- (gsub("}", "", strsplit((strsplit(part_monthly$part_monthly, ",")[[i]][2]), ":")[[1]][2]))
    part_number2 <- (gsub("}", "", strsplit((strsplit(part_monthly$part_monthly, ",")[[i+1]][2]), ":")[[1]][2]))
    part_number <- as.numeric(part_number1) + as.numeric(part_number2)
    
    N <- paste('"count" :', part_number)
    N <- gsub("  ", " ", N)
    
    part_replace <- ('{ "_id" : "197001" , "count" : 0}')
    part_replace <- gsub(197001, substr(part_monthly[i,1],12,17), part_replace)
    part_replace <- gsub('"count" : 0', N, part_replace)
    
    part_monthly[i,1] <- part_replace
    # part_monthly <- part_monthly[(-i-1),1]
    # part_monthly <- as.data.frame(part_monthly, stringsAsFactors = F)
    # 
    # i <- i + 1
    # nrow_part_monthly <- nrow_part_monthly - 1
    
    bind_list <- c(bind_list, i+1)
    
    i <- i + 2
  }else{
    part_number <- (gsub("}", "", strsplit((strsplit(part_monthly$part_monthly, ",")[[i]][2]), ":")[[1]][2]))
    N <- paste('"count" :', part_number)
    N <- gsub("  ", " ", N)
    part_replace <- ('{ "_id" : "197001" , "count" : 0}')
    part_replace <- gsub(197001, substr(part_monthly[i,1],12,17), part_replace)
    part_replace <- gsub('"count" : 0', N, part_replace)
    
    part_monthly[i,1] <- part_replace
    
  }
  
}

j <- 1

for(j in length(bind_list):1){
  delete_number <- 0 - (bind_list[j])
  part_monthly <- part_monthly[delete_number, 1]
  part_monthly <- as.data.frame(part_monthly, stringsAsFactors = F)
}

#=============================================================================================

monthly_list_final <- monthly_list
monthly_list_travel <- monthly_list
monthly_list_ptt <- monthly_list

for(i in 1:nrow(monthly_list)){
  for(j in 2:ncol(monthly_list)){
    monthly_list_final[i,j] <- as.numeric(monthly_list_travel[i,j]) + as.numeric(monthly_list_ptt[i,j])
  }
}

write.table(monthly_list_final, file = "monthly_list_final.csv", sep = ",", row.names = F)

#=============================================================================================

library(rJava)
library(RMongo)

#R連進Mongo的port
mongo <- mongoDbConnect("project", "10.120.37.119", 27017)
auth <- dbAuthenticate(mongo, "mongo", "mongo")

url1 <- "E:/Rmongo/w2v397.json"
txt1 <- fromJSON(url1, flatten = TRUE)
txt1 <-as.data.frame(txt1)

view_list <- txt1[2:397,1]

#產生年月列表
monthly_list <- NULL
for(i in 2017:2000){
  year_id <- i
  for( j in 12:1){
    month_id <- j
    if(j <= 9){
      month_id <- paste("0", month_id, sep = "")
    }
    monthly_list <- rbind(monthly_list, paste(year_id, month_id, sep = ""))
  }
}

monthly_list <- as.data.frame(monthly_list, stringsAsFactors = F)

#依序抓取關鍵字table
for(i in 1:length(view_list)){
  command <- c('{"$match":{city_treated:{"$regex":"關鍵字"}}}',
               '{"$group": {_id: "$periodnew",count: {"$sum": 1 }}}',
               '{"$sort": {_id: -1 }}',
               '{"$project":{count : 1}}')
  #在哪個部分選:content, city_treated
  word <- view_list[i]
  command <- gsub("關鍵字", word, command)
  
  part_monthly <- dbAggregate(mongo, "tripcontentcut", command) #在哪個資料庫 alltravel, tripcontentcut
  part_monthly <- as.data.frame(part_monthly, stringsAsFactors = F)
  
  comb_monthly <- part_monthly
  
  # 補上關鍵次出現0次的月份
  for(j in 1:nrow(monthly_list)){
    if(monthly_list[j,1] %in% substr(comb_monthly[j,1],12,17)){
      print(j)
    }else{
      month_replace <- ('{ "_id" : "197001" , "count" : 0}') #預設填空月份值
      month_replace <- gsub(197001, monthly_list[j,1] , month_replace) #取代為空缺的月份值
      comb_monthly1 <- as.data.frame(comb_monthly[1:(j-1),1], stringsAsFactors = F)
      colnames(comb_monthly1) <- "year_month"
      if(j == 1){
        comb_monthly2 <- as.data.frame(comb_monthly[(j+1):nrow(comb_monthly),1], stringsAsFactors = F)
      }else{
        comb_monthly2 <- as.data.frame(comb_monthly[j:nrow(comb_monthly),1], stringsAsFactors = F)
      }
      colnames(comb_monthly2) <- "year_month"
      
      if(j == 1){
        comb_monthly <- rbind(month_replace, comb_monthly1)
      }else{
        comb_monthly <- rbind(comb_monthly1, month_replace)
      }
      
      colnames(comb_monthly) <- "year_month"
      comb_monthly <- rbind(comb_monthly, comb_monthly2)
    }
  }
  comb_monthly <- comb_monthly[-217:-1000,]
  comb_monthly <- as.data.frame(comb_monthly, stringsAsFactors = F)
  
  #轉每個欄位的字串成為時序int
  sort_month <- NULL
  sort_count <- NULL
  for(k in 1:nrow(comb_monthly)){
    sort_count <- rbind(sort_count,gsub("}","",strsplit((strsplit(comb_monthly$comb_monthly,",")[[k]][2]),":")[[1]][2]))
  }
  sort_count <- as.data.frame(sort_count, stringsAsFactors = F)
  monthly_list <- as.data.frame(cbind(monthly_list, sort_count), stringsAsFactors = F)
}

colnames(monthly_list) <- c("yy-mm",view_list)

write.table(monthly_list, file = "monthly_list_travel.csv", sep = ",", row.names = F)
