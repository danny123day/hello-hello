data<-read.csv(file ="full_table_0136.csv", header=TRUE)
data$datatime = strptime(paste(data$date,data$time),format = "%Y-%m-%d %H:%M")

data$datatime= as.character(data$datatime)

rfm<-data.frame(id=data$SRCNO, 
                date=data$datatime,
                sales=data$SALE_AMT)

#補NA值
mean_sales = mean(  rfm$sales[ !is.na(rfm$sales) ]  )
rfm$sales[ is.na(rfm$sales) ] <- mean_sales
head(rfm)


rfm$sales[ is.na(rfm$sales) ] #檢查有沒有NA

#算R
date_now = format(Sys.time(), "%Y-%m-%d %H:%M") ;date_now
#unclass(as.POSIXct(datanow)) 把日期轉換成秒數
second= as.vector(unclass( as.POSIXct(as.Date(date_now ))) - unclass(as.POSIXct( as.Date(data$datatime) )))

#加入Recency
rfm <- cbind( rfm , recency=second ,frequency="" ,money="" )

#length(second)
#dim(rfm)
head(rfm)
#which(is.na(rfm$date))




id_each = unique(rfm$id) #id有幾個變數
id_order = rfm[order(rfm$id , decreasing = FALSE) ,] #id排序


RFM = data.frame( ) #算出RFM後的dateframe



for( i in 1:length(id_each) ){
  
  place = which( rfm$id == id_each[i] ) 
  f = length(place)
  
  m=0
  df = data.frame() ;df #同一個人購買記錄
  #取同一個顧客購買記錄
  for( j in 1 : length(place)){
      
        df = rbind(df, rfm[place[j],] )

        m = df$sales[j] + m
        df$frequency<-f
        df$money<-m
        
  }
  df <- df[order(df$recency , decreasing = FALSE) ,] #R排序
  
  

  RFM <- rbind(RFM,df[1,c(1,4,5,6)])

  
}
RFM

View(RFM)
dim(RFM)


#clusterinig

mean_recency <- mean(RFM$recency) ;mean_recency
mean_frequency <- mean(RFM$frequency) ; mean_frequency
mean_money <- mean(RFM$money) ; mean_money

#RFM<- RFM[,-5]
RFM<-cbind(RFM,group="")
head(RFM)
RFM$group = ""

for( i in 1:length(RFM$id)){
  if(RFM$recency[i] >= mean_recency && RFM$frequency[i] >= mean_frequency && RFM$money[i] >= mean_money){
    RFM$group[i] <- "Group1"
  }else if(RFM$recency[i] >= mean_recency && RFM$frequency[i] < mean_frequency && RFM$money[i] >= mean_money){
    RFM$group[i] <- "Group2"
  }else if(RFM$recency[i] < mean_recency && RFM$frequency[i] >= mean_frequency && RFM$money[i] >= mean_money){
    RFM$group[i] <- "Group3"
  }else if(RFM$recency[i] < mean_recency && RFM$frequency[i] < mean_frequency && RFM$money[i] >= mean_money){
    RFM$group[i] <- "Group4"
  }else if(RFM$recency[i] >= mean_recency && RFM$frequency[i] >= mean_frequency && RFM$money[i] < mean_money){
    RFM$group[i] <- "Group5"
  }else if(RFM$recency[i] >= mean_recency && RFM$frequency[i] < mean_frequency && RFM$money[i] < mean_money){
    RFM$group[i] <- "Group6"
  }else if(RFM$recency[i] < mean_recency && RFM$frequency[i] >= mean_frequency && RFM$money[i] < mean_money){
    RFM$group[i] <- "Group7"
  }else{
    RFM[i,5] <- "Group8"
  }
}

#檢查有沒有NA
#RFM$group[ is.na(RFM$group) ] 


lbls <- c("Group1","Group2","Group3","Group4","Group5","Group6","Group7","Group8")
pct <- prop.table(table(RFM$group))
lbls <- paste(lbls, "-",round(pct,2)*100) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(table(RFM$group),labels = lbls, col=rainbow(length(lbls)),
    main="Rank Distribution"
)

