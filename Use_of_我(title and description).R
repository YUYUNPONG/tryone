source("C:/Users/tung/Desktop/UNI/Information/Han_Chen_data/Datasets.r")

HGY <- c(length(grep("我", july$Message)),length(grep("我", Aug$Message)),length(grep("我", Sep$Message)),length(grep("我", Oct$Message)),length(grep("我", Nov$Message)),length(grep("我", Dec$Message)))

CCM <- c(length(grep("我", cjuly$Message)),length(grep("我", cAug$Message)),length(grep("我", cSep$Message)),length(grep("我", cOct$Message)),length(grep("我", cNov$Message)),length(grep("我", cDec$Message)))

HGY1 <- c(length(grep("我", july$Link_Title)),length(grep("我", Aug$Link_Title)),length(grep("我", Sep$Link_Title)),length(grep("我", Oct$Link_Title)),length(grep("我", Nov$Link_Title)),length(grep("我", Dec$Link_Title)))

CCM1 <- c(length(grep("我", cjuly$Link_Title)),length(grep("我", cAug$Link_Title)),length(grep("我", cSep$Link_Title)),length(grep("我", cOct$Link_Title)),length(grep("我", cNov$Link_Title)),length(grep("我", cDec$Link_Title)))

HGY2 <- c(length(grep("我", july$`Link Description` )),length(grep("我", Aug$`Link Description` )),length(grep("我", Sep$`Link Description` )),length(grep("我", Oct$`Link Description` )),length(grep("我", Nov$`Link Description` )),length(grep("我", Dec$`Link Description` )))

CCM2 <- c(length(grep("我", cjuly$`Link Description` )),length(grep("我", cAug$`Link Description` )),length(grep("我", cSep$`Link Description` )),length(grep("我", cOct$`Link Description` )),length(grep("我", cNov$`Link Description` )),length(grep("我", cDec$`Link Description` )))

IinALLHAN<-HGY + HGY1 +HGY2
IinALLCHEN<-CCM + CCM1 +CCM2


HanChenI2Count = data.frame(time  = month ,Han =IinALLHAN, Chen = IinALLCHEN)
colnames(HanChenI2Count ) <- c("time","韓國瑜","陳其邁")


KaoI2 <- melt(HanChenI2Count [, c("time","韓國瑜","陳其邁")], id="time")

colnames(KaoI2) <- c("month", "Candidate", "Use_of_我")

ggplot(KaoI2) + geom_line(aes(x=month, y=Use_of_我, color=Candidate)) + labs(title="Kaohsiung Use of 我")
