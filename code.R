#1
web <- seq()


for(i in 0:9){
  #网页。10页，每页25
  url1 <- paste('http://book.douban.com/top250?start=',25*i,'&filter=&type=',sep="")
  #按行读取
  web1 <- readLines(url1,encoding="UTF-8")
  web <- c(web, web1)  
}


#书名
name <- web[grep(' <div class="pl2">',web)+6]
#作者、出版商等信息
other <- web[grep('<p class="pl">',web)]
#评分
score<-web[grep('<div class="star',web)+2]

#将数据解析到book对象中
book.names <- 0 #书名
book.authors <- 0;#作者
book.Publishing_House<- 0;#出版社
book.Publishing_date <- 0;#出版时间
book.price <- 0;#定价
book.scores <- 0#评分

#解析数据
others <- 0
for(i in 1:length(name)){
  #书名*
  book.names[i]<-substring(name[i],17)#去掉书名前的空白
  others[i] <- substring(other[i],28,nchar(other[i])-4)
  temp<-strsplit(others[i],split = "/")[[1]]
  #作者*
  book.authors[i] = temp[1];
  #出版社
  book.Publishing_House[i] = temp[length(temp)-2]; 
  #出版日期
  book.Publishing_date[i] = temp[length(temp)-1];
  #价格*
  book.price[i] = temp[length(temp)]; 
  #评分*
  book.scores [i] <- substring(score[i],45,nchar(score[i])-7)
}
book.names <- sub('>','',book.names)
book.authors <- sub('>','',book.authors)
book.Publishing_House <- sub('>','',book.Publishing_House)
book.Publishing_date <- sub('>','',book.Publishing_date)
book.price <- sub('>','',book.price)
book.scores <- sub('>','',book.scores)		


# 合成为数据框
temp1 <- data.frame('书名'=book.names,
                    '作者'=book.authors,
                    '价格'= book.price,
                    '评分' = book.scores)

write.csv(temp1,"dbbookTop250.csv") 




#2
book <- read.csv("dbbookTop250.csv")
authors <- book.authors
#最多国家most_country
#截取】/])
authors[5] <- gsub("J.K.罗琳","[英] J.K.罗琳",authors[5])
authors[5] <- gsub("\\((.*?)\\)","",authors[5])



temp2 <- strsplit(authors,']')
temp2 <- sapply(temp2,head,1)
temp2 <- strsplit(temp2,'】')
temp2 <- sapply(temp2,head,1)
temp2 <- strsplit(temp2,')')
temp2 <- sapply(temp2,head,1)
temp2

temp3 <- c(temp2)
temp3 <- gsub("明|清","中",temp3)
temp3 <- gsub("德国","德",temp3)
temp3 <- gsub("俄罗斯","俄",temp3)
temp3 <- gsub("意大利","意",temp3)
temp3 <- gsub("美国","美",temp3)
temp3 <- gsub("法国","法",temp3)
temp3 <- gsub("日本","日",temp3)

temp4 <- temp3
for (i in 1:250){
  temp_char <- temp4[i]
  temp_char2 <- substring(temp_char,1,1)
  if (temp_char2!= '['&&temp_char2!= '【'&&temp_char2!='('){
    temp4[i] <- "[中"
  }
}
temp4 <- gsub("[\\[\\【\\(]","",temp4)
temp4

country_count <- table(temp4)
print(country_count)

#最大值 都转成人民币
print(book.price)
book.price <- gsub("元","",book.price)
book.price <- gsub(" ","",book.price)
book.price <- gsub("CNY","",book.price)
book.price[134] <- ("79.34")
print(book.price[134])
book.price <- as.numeric(book.price)
max(book.price)

#最小值
min(book.price)

#价格分布
  #直方
price <- c(book.price)
price <- as.numeric(price)

hist(price)
  #散点
plot(price)

#峰度
library(moments)
kurtosis(price)

#偏度
skewness(price)

