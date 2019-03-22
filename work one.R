Sys.setlocale(category = "LC_ALL", locale = "cht")

###goal 選取候選人(自己的fb)貼文議題，民眾對於議題的感受程度
#韓國瑜kh,陳其邁gc

setwd("~/中山大學政研所2019.Jan 資料採購-20190304T124038Z-001/中山大學政研所2019.Jan 資料採購")
library(dplyr)
library(tm)
library(readr)
jan<- read_csv("201801_data.csv",locale = locale(encoding = "UTF8"))
feb<- read_csv("201802_data.csv",locale = locale(encoding = "UTF8"))
mar<- read_csv("201803_data.csv",locale = locale(encoding = "UTF8"))
apr<- read_csv("201804_data.csv",locale = locale(encoding = "UTF8"))
may<- read_csv("201805_data.csv",locale = locale(encoding = "UTF8"))
jun<- read_csv("201806_data.csv",locale = locale(encoding = "UTF8"))
jul<- read_csv("201807_data.csv",locale = locale(encoding = "UTF8"))
aug<- read_csv("201808_data.csv",locale = locale(encoding = "UTF8"))
sep<- read_csv("201809_data.csv",locale = locale(encoding = "UTF8"))
oct<- read_csv("201810_data.csv",locale = locale(encoding = "UTF8"))
nov<- read_csv("201811_data.csv",locale = locale(encoding = "UTF8"))
dec<- read_csv("201812_data.csv",locale = locale(encoding = "UTF8"))
jan19<- read_csv("201901_data.csv",locale = locale(encoding = "UTF8"))


#七月
GL7 <-filter(jul,grepl("韓國瑜", jul$Page_Name)==TRUE&grepl("高雄選韓國瑜News", jul$Page_Name)==FALSE&grepl("韓國瑜粉絲團", jul$Page_Name)==FALSE&grepl("韓國瑜新聞網", jul$Page_Name)==FALSE&grepl("韓國瑜民間粉絲團", jul$Page_Name)==FALSE)
Air7 <-filter(jul,grepl("陳其邁", jul$Page_Name)==TRUE&grepl("陳其邁的潛水日記",jul$Page_Name)==FALSE)

##資料整理
##合併資料
date111=rbind(jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov) # 1~11月數據
date121=rbind(dec,jan19) #12~01月數

#選舉前的資料篩選(只要候選人fb)
khb <-filter(date111,grepl("韓國瑜", date111$Page_Name)==TRUE&grepl("高雄選韓國瑜News", date111$Page_Name)==FALSE&grepl("韓國瑜粉絲團", date111$Page_Name)==FALSE&grepl("韓國瑜新聞網", date111$Page_Name)==FALSE&grepl("韓國瑜民間粉絲團", date111$Page_Name)==FALSE)
gcb <-filter(date111,grepl("陳其邁", date111$Page_Name)==TRUE&grepl("陳其邁的潛水日記",date111$Page_Name)==FALSE)


##選舉後的資料篩選(只要候選人fb)
kha <-filter(date121,grepl("韓國瑜", date121$Page_Name)==TRUE&grepl("高雄選韓國瑜News", date121$Page_Name)==FALSE&grepl("韓國瑜粉絲團", date121$Page_Name)==FALSE&grepl("韓國瑜新聞網", date121$Page_Name)==FALSE&grepl("韓國瑜民間粉絲團", date121$Page_Name)==FALSE)
gca <-filter(date121,grepl("陳其邁", date121$Page_Name)==TRUE&grepl("陳其邁的潛水日記",date121$Page_Name)==FALSE)
  
#選舉前韓國瑜的發文次數303，選後235；選前陳其邁613，選後44
#設定時間
khb$Date = as.POSIXct(khb$Date,format = "%Y/%m/%d %H:%M:%S")
gcb$Date = as.POSIXct(gcb$Date,format = "%Y/%m/%d %H:%M:%S")
kha$Date = as.POSIXct(kha$Date,format = "%Y/%m/%d %H:%M:%S")
gca$Date = as.POSIXct(gca$Date,format = "%Y/%m/%d %H:%M:%S")



plot(khb$Date, khb$LIKE_COUNT, type = "p", col="blue")###WHY USE LIKE COUNT??
lines(gcb$Date, gcb$LIKE_COUNT,type="p", col="red")

##兩候選人All_Reaction_Count趨勢圖
plot(khb$Date, khb$All_Reaction_Count,xlab ="Date",ylab = "count"  ,type = "l",col = "blue")
lines(gcb$Date, gcb$All_Reaction_Count,col = "green")

plot(kha$Date, kha$All_Reaction_Count,xlab ="Date",ylab = "count"  ,type = "l",col = "blue")
lines(gca$Date, gca$All_Reaction_Count,col = "green")


#####################
date111$Page_Name_nchar=nchar(date111$Message)
date121$Page_Name_nchar=nchar(date121$Message)
ggscatter(date121,x="All_Reaction_Count",y="LIKE_COUNT", add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")
ggqqplot(date121$All_Reaction_Count)  #常態分佈
ggqqplot(date121$LIKE_COUNT)          #常態分佈

cor(chen[6:14])%>%corrplot.mixed(lower = "pie",tl.cex=0.6)
# method = c("pearson", "kendall", "spearman")


###以每月為分析依據
library(ggplot2)
khb <- data.frame( 
  month = c(7, 8, 9, 10, 11,12), 
  posts = khb) 
print(khb) 
ggplot(khb, aes(x = month, y = posts)) + geom_bar(stat = "identity") + labs(title="韓國瑜")


##############################
library(readLines)
readLines(Message)
install.packages("wordcloud")
library(wordcloud)
d <- data.frame(word = names(wordtable), freq = as.numeric(wordtable))
