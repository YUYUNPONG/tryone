library(dplyr)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(wordcloud2)
library(wordcloud)
library(ggpubr)
library(topicmodels)
library(tidytext)
library(jiebaRD)
library(jiebaR)
library(readr)
data04=data.table::fread('201804_data.csv',data.table = F,encoding = 'UTF-8')
data05=data.table::fread('201805_data.csv',data.table = F,encoding = 'UTF-8')
data06=data.table::fread('201806_data.csv',data.table = F,encoding = 'UTF-8')
data07=data.table::fread('201807_data.csv',data.table = F,encoding = 'UTF-8')
data08=data.table::fread('201808_data.csv',data.table = F,encoding = 'UTF-8')
data09=data.table::fread('201809_data.csv',data.table = F,encoding = 'UTF-8')
data10=data.table::fread('201810_data.csv',data.table = F,encoding = 'UTF-8')
data11=data.table::fread('201811_data.csv',data.table = F,encoding = 'UTF-8')

###合併
alldata=rbind(data04,data05,data06,data07,data08,data09,data10,data11)

##設定日期時間格式
alldata$Date=as.POSIXct(alldata$Date,format="%Y/%m/%d %H:%M:%S")## format 定義
#韓參選2018-4-9
all=alldata%>%filter(Date>="2018-4-9"&Date<"2018-11-24")

##只有個人
###樣本沒有(高雄在地韓國瑜News)
kh <-filter(alldata,grepl("韓國瑜", alldata$Page_Name)==TRUE&
              grepl("高雄選韓國瑜News",alldata$Page_Name)==FALSE&
              grepl("韓國瑜粉絲團", alldata$Page_Name)==FALSE&
              grepl("韓國瑜新聞網", alldata$Page_Name)==FALSE&
              grepl("韓國瑜民間粉絲團", alldata$Page_Name)==FALSE&
              grepl("高雄在地韓國瑜News", alldata$Page_Name)==FALSE&
              grepl("侯友宜 盧秀燕 韓國瑜 北中南連線", alldata$Page_Name)==FALSE)
kc <-filter(alldata,grepl("陳其邁", alldata$Page_Name)==TRUE&
              grepl("陳其邁的潛水日記",alldata$Page_Name)==FALSE)
#fan
khfan <-filter(alldata,grepl("韓國瑜", alldata$Page_Name))
kcfan <-filter(alldata,grepl("陳其邁", alldata$Page_Name))

khc = rbind(kh, kc)
khcfan = rbind(khfan, kcfan)

#先分月
## 分月份 group_byzp 分群的意思
kh1 <-kh%>%group_by(month=format(Date,"%m"))%>%count()%>%mutate(type="kh")
kc1 <-kc%>%group_by(month=format(Date,"%m"))%>%count()%>%mutate(type="kc")
khc <-khc%>%group_by(month=format(Date,"%m"))%>%count()%>%mutate(type="kc")

library(ggplot2) 
### 總po文??
#方法一
khc%>%group_by(Page_Name)%>%count()%>%ggplot(aes(Page_Name,n))+
  geom_bar(stat = "identity")+
  ggtitle("貼文數統計")+
  theme(plot.title = element_text(hjust = 0.5))
#方法ˇ二
ggplot() + geom_bar(data = khc, aes(x=Page_Name)) ### 總po文

#候選人個月比較
###?? 這兩個可不可以一起(兩者之間的比較)
ggplot(kh1,aes(x=month,y=n,fill=type))+
  geom_bar(stat="identity",position = "dodge")  
ggplot(kc1,aes(x=month,y=n,fill=type))+
  geom_bar(stat="identity",position = "dodge")


###總fan po文
ggplot() + geom_bar(data = khcfan, aes(x=Page_Name))

###貼文跟喜歡的相比  為什麼後面的統計不是數字??
ggplot() + geom_bar( data = khc, 
                     aes(x=Page_Name, y=LIKE_COUNT, group = Page_Name),
                     stat="identity")
ggplot() + geom_bar( data = khcfan, 
                     aes(x=Page_Name, y=LIKE_COUNT, group = Page_Name),
                     stat="identity")

### 比較有fan的貼文種類
khcfan%>%group_by(Page_Name,Type)%>%summarize(n=n())%>%mutate(freq=n/sum(n))%>%ggplot(aes(Type,freq,fill=Page_Name))+
  geom_bar(stat="identity",position = "dodge")+
  ggtitle("貼文種類")+
  theme(plot.title = element_text(hjust = 0.5))

####挑選只有經濟
alleconomic <-filter(khc,grepl("經濟", khc$Message)==TRUE)
#經濟分月
alleconomic1 <-alleconomic%>%group_by(month=format(Date,"%m"))%>%count()%>%mutate(type="alleconomic")

ggplot(alleconomic1,aes(x=month,y=n,fill=type))+
  geom_bar(stat="identity",position = "dodge")

###TM
library(tidyverse)
all_msg = khcfan%>% group_by(Page_Name) %>% 
  mutate(messageByName = paste0(Message, collapse = ""))
id = which(duplicated(all_msg$Page_Name) == FALSE)
all_msg=all_msg[id,c(2,19)]

kc_msg = kc%>% group_by(Page_Name) %>% 
  mutate(messageByName = paste0(Message, collapse = ""))
id = which(duplicated(kc_msg$Page_Name) == FALSE)
kc_msg=kc_msg[id,c(2,19)]

kh_msg = kh%>% group_by(Page_Name) %>% 
  mutate(messageByName = paste0(Message, collapse = ""))
id = which(duplicated(kh_msg$Page_Name) == FALSE)
kh_msg=kh_msg[id,c(2,19)]

#經濟
economic = alleconomic%>% group_by(Page_Name) %>% 
  mutate(messageByName = paste0(Message, collapse = ""))
id = which(duplicated(economic$Page_Name) == FALSE)
economic=economic[id,c(2,19)]


## Jieba 切詞
library(jiebaRD)
library(jiebaR)
cutter <- worker("tag",stop_word ="stopwords-u8.txt",user = "user.txt" ,encoding = "UTF-8",bylines = T)

dic = c("韓國瑜", "國瑜", "高雄人","貨出的去","人進得來",
        "高雄發大財","發大財","經濟","北漂","輕軌","口號","劉家昌","韓粉","氣爆",
        "youtuber","全台首富","假新聞","滅火器","鳳山","民主","we care","相信高雄",
        "看好未來","溫暖","科學園區","亞洲新灣區","半導體","觀光","產業","升級","南南合作",
        "接軌","發展","長照","好生活","共享","綠能","循環","青年","智慧城市","招商","循環",
        "引資","雙語",,"施工","創投","臨托","交通","空污","路網","賣菜郎","賣菜",
        "看見大海","縣市合併","住宅","正義","撕裂","又老又窮")
new_user_word(cutter, dic)

myFUN<- function(str) {
  str = gsub("[^[:alpha:]]|[A-Za-z0-9]", "", str)
  seg = cutter[str]
  result = seg
}
segment_all = apply(matrix(all_msg$messageByName), MARGIN = 1, myFUN)
segment_han = apply(matrix(kh_msg$messageByName), MARGIN = 1, myFUN)
segment_chen= apply(matrix(kc_msg$messageByName), MARGIN = 1, myFUN)
##經濟
segment_economic= apply(matrix(economic$messageByName), MARGIN = 1, myFUN)


## 看不懂??
xseg = worker("tag",stop_word ="stopwords-u8.txt",user = "user.txt" ,encoding = "UTF-8",bylines = T) 
xtext2 = NULL
for (i in 1:length(all_msg$messageByName)){
  t0 = all_msg$messageByName[i]
  t1 = xseg <= t0
  xtext2 = c(xtext2,paste0(t1,collapse=" "))
}
text_df = data_frame(doc_id = 1:length(xtext2), text = xtext2)
library(stringr)
tok99 = function(t) str_split(t,"[ ]{1,}")

td1 = unnest_tokens(text_df,word, text, token=tok99)
td2 = td1 %>%
  count(doc_id,word,sort=T) %>%
  ungroup() %>%
  bind_tf_idf(word,doc_id, n)
td_tfidf = arrange(td2,desc(tf_idf))
td_tfidf


## 篩選出詞頻較高者並做出文字雲

hanfreq=data.frame(table(segment_han[[1]]))
chenfreq=data.frame(table(segment_chen[[1]]))
top_han=hanfreq%>%arrange(desc(Freq))%>%head(150)
top_chen=chenfreq%>%arrange(desc(Freq))%>%head(150)
wordcloud(top_chen$Var1,top_chen$Freq,random.order = F, ordered.colors = F, colors=rainbow(1000))
wordcloud(top_han$Var1,top_han$Freq,random.order = F, ordered.colors = F, colors=rainbow(1000))
##經濟
economicfreq=data.frame(table(segment_economic[[1]]))
top_economic=economicfreq%>%arrange(desc(Freq))%>%head(150)
wordcloud(top_economic$Var1,top_economic$Freq,random.order = F, ordered.colors = F, colors=rainbow(1000))

## 做出字詞關聯圖
#陳韓
top_han=top_han%>%head(30)
top_chen=top_chen%>%head(30)
topword=merge(top_chen,top_han,by="Var1",all = TRUE)
colnames(topword) = c("words","chen","han")
rownames(topword) = topword$words
topword= topword[,-1]
topword[is.na(topword)]<-0
CoMatrix = as.matrix(topword) %*% t(as.matrix(topword))
total_occurrences <- rowSums(CoMatrix)
smallid = which(total_occurrences < median(total_occurrences))
co_occurrence_d = CoMatrix / total_occurrences
co_occurrence_s = co_occurrence_d[-as.vector(smallid),-as.vector(smallid)]
require(igraph)
graph <- graph.adjacency(round(co_occurrence_s*10),
                         mode="undirected",
                         diag=FALSE)
plot(graph,
     vertex.label=names(data),
     edge.arrow.mode=0,
     vertex.size=1,
     edge.width=E(graph)$weight,
     layout=layout_with_fr)


## LDA


rownames(hanfreq) = hanfreq$Var1
handtm=subset(hanfreq)%>%select(Freq)
dtm_lda <- LDA(t(handtm), k = 4, control = list(seed = 1234))
dtm_topics <- tidy(dtm_lda, matrix = "beta")
top_terms <- dtm_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  theme(axis.text.y=element_text(colour="black"))
############################
rownames(chenfreq) = chenfreq$Var1
chendtm=subset(chenfreq)%>%select(Freq)
dtm_lda <- LDA(t(chendtm), k = 4, control = list(seed = 1234))
dtm_topics <- tidy(dtm_lda, matrix = "beta")
top_terms <- dtm_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  theme(axis.text.y=element_text(colour="black"))
###################### 經濟
rownames(economicfreq) = economicfreq$Var1
economicdtm=subset(economicfreq)%>%select(Freq)
dtm_lda <- LDA(t(economicdtm), k = 6, control = list(seed = 1234))
dtm_topics <- tidy(dtm_lda, matrix = "beta")
top_terms <- dtm_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  theme(axis.text.y=element_text(colour="black"))

## 情緒詞統計

ntuPosEmo=data.table::fread("ntu-positive.txt", header = T,sep="\r",quote = "", stringsAsFactors = F,encoding = "UTF-8")
ntuNegEmo=data.table::fread("ntu-negative.txt", header = T,sep="\r",quote = "", stringsAsFactors = F,encoding = "UTF-8")
han_pos=hanfreq%>%merge(x=.,y=ntuPosEmo,by.x="Var1",by.y="word")%>%summarize(Emo="han_pos",Value=sum(Freq)/232)
han_neg=hanfreq%>%merge(x=.,y=ntuNegEmo,by.x="Var1",by.y="word")%>%summarize(Emo="han_neg",Value=sum(Freq)/232)
chen_pos=chenfreq%>%merge(x=.,y=ntuPosEmo,by.x="Var1",by.y="word")%>%summarize(Emo="chen_pos",Value=sum(Freq)/364)
chen_neg=hanfreq%>%merge(x=.,y=ntuNegEmo,by.x="Var1",by.y="word")%>%summarize(Emo="chen_neg",Value=sum(Freq)/364)
Emotion=rbind(han_pos,han_neg,chen_pos,chen_neg)
ggplot(Emotion,aes(x=Emo,y=Value,fill=Emo))+
  geom_bar(stat = "identity")+ggtitle("情緒詞統計(平均每篇文章)")

