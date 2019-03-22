###資料分析
## 第一步：讀入資料

Sys.setlocale(category ='LC_ALL')


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

View(jan)
##取出JAN中的資料看第一條的字串
jan$Message[1]
nchar(jan$Message[1])
##取出JAN中的資料Message 每筆總共數，計算字串長度
nchar(jan$Message)
##取出JAN中的Page_Name資，指定查詢
grep("韓國瑜",jan$Page_Name)#36 韓
grep("陳其邁",jan$Page_Name)# 33 陳
grep("柯文哲",jan$Page_Name) ## 第一篇說柯，第432再說柯

regexpr("韓國瑜", jan$Message)

##合併資料
alldata=rbind(jan, feb, mar, apr, may,jun,
              jul, aug, sep, oct, nov, dec, jan19)

save(alldata, file="alldata.rda") ##save new data

##設定日期時間格式
alldata$Date=as.POSIXct(alldata$Date,format="%Y/%m/%d %H:%M:%S")## format 定義

##資料整理
##挑選候選人fb資料1#grepl模糊比對，明子有韓的
kh <-filter(alldata,grepl("韓國瑜", alldata$Page_Name)==TRUE&grepl("高雄選韓國瑜News",alldata$Page_Name)==FALSE&grepl("韓國瑜粉絲團", alldata$Page_Name)==FALSE&grepl("韓國瑜新聞網", alldata$Page_Name)==FALSE&grepl("韓國瑜民間粉絲團", alldata$Page_Name)==FALSE&grepl("高雄在地韓國瑜News", alldata$Page_Name)==FALSE&grepl("侯友宜 盧秀燕 韓國瑜 北中南連線", alldata$Page_Name)==FALSE)
gc <-filter(alldata,grepl("陳其邁", alldata$Page_Name)==TRUE&grepl("陳其邁的潛水日記",alldata$Page_Name)==FALSE)

##挑選候選人fb資料2
kh1 <- alldata %>% 
  filter(Page_Name == "韓國瑜") %>%
  select(Page_Name, Page_ID, Date, All_Reaction_Count, LIKE_COUNT, Comment_Count,
         Share_Count, Message, Type) ## 沒有挑選全部
gc1 <- alldata %>% 
  filter(Page_Name == "陳其邁 Chen Chi-Mai") %>%
  select(Page_Name, Page_ID, Date, All_Reaction_Count, LIKE_COUNT, Comment_Count,
         Share_Count, Message, Type) ## 沒有挑選全部

## 分月份?? 1 group_byzp 分群的意思
khMonth <-kh%>%group_by(month=format(Date,"%Y%m"))%>%count()%>%mutate(type="khan")
khMonth <-kh%>%group_by(type=Type)%>%count()
gcMonth <-gc%>%group_by(month=format(Date,"%Y%m"))%>%count()%>%mutate(type="gchen")

month_rank=rbind(khMonth, gcMonth)%>%arrange((month))
month_rank22=rbind(khMonth, gcMonth)
## 分月份?? 2
khMonth1 <-kh%>%group_by(month=format(Date,"%m"))%>%count()%>%mutate(type="Han1")
gcMonth1 <-gc%>%group_by(month=format(Date,"%m"))%>%count()%>%mutate(type="Ke1")

month_rank2=rbind(khMonth1, gcMonth1)

month_rank1=rbind(khMonth1, gcMonth1)%>%arrange((month))

##畫圖套件
library(ggplot2)

###BAR# 兩位比較
ggplot(month_rank,aes(x=month,y=n,fill=type))+
  geom_bar(stat="identity",position = "dodge")
ggplot(month_rank22,aes(x=month,y=n,fill=type))+
  geom_bar(stat="identity",position = "dodge")
#
ggplot(month_rank,aes(x=month,y=n,fill=type))+
  geom_bar(stat="identity")
# LINE
ggplot(month_rank,aes(x=month,y=n,group=type,color=type))+geom_line()

# BOXPLOT
ggplot(month_rank,aes(x=month,y=n))+geom_boxplot()

# POINT   #head取資料的前幾位，在沒有排去前
ggplot(kh,aes(x=Share_Count,y=All_Reaction_Count))+
  geom_point()
ggplot(head(gc,657),aes(x=Share_Count,y=All_Reaction_Count))+
  geom_point()

###################

library(ggpubr)
kh$mes_nchar=nchar(kh$Message)
gc$mes_nchar=nchar(gc$Message)
ggscatter(gc,x="All_Reaction_Count",y="LIKE_COUNT", add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")
ggqqplot(gc$All_Reaction_Count)  #常態分佈
ggqqplot(gc$LIKE_COUNT)          #常態分佈

