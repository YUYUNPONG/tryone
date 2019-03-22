JulyHan <-(X201807_data$Page_Name, "韓國瑜")

AugHan <-(X201808_data$Message)
SepHan <-("韓國瑜", X201809_data$Message)
OctHan <-("韓國瑜", X201810_data$Message)
NovHan <-("韓國瑜", X201811_data$Message)
DecHan <-("韓國瑜", X201812_data$Message)

july <- subset(X201807_data, select = c("Page_Name", "Message"), subset = (Page_Name == ("韓國瑜")))
Aug <- subset(X201808_data, select = c("Page_Name", "Message"), subset = (Page_Name == ("韓國瑜")))
Sep <- subset(X201809_data, select = c("Page_Name", "Message"), subset = (Page_Name == ("韓國瑜")))
Oct <- subset(X201810_data, select = c("Page_Name", "Message"), subset = (Page_Name == ("韓國瑜")))
Nov <- subset(X201811_data, select = c("Page_Name", "Message"), subset = (Page_Name == ("韓國瑜")))
Dec <- subset(X201812_data, select = c("Page_Name", "Message"), subset = (Page_Name == ("韓國瑜")))
Jan <- subset(X201901_data, select = c("Page_Name", "Message"), subset = (Page_Name == ("韓國瑜")))
Han2018 <- c(length(july$Page_Name), length(Aug$Page_Name), length(Sep$Page_Name), length(Oct$Page_Name), length(Nov$Page_Name), length(Dec$Page_Name), length(Jan$Page_Name))
Han2018
date <- c("July", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan")





cjuly <- subset(X201807_data, select = c("Page_Name", "Date", "Message"), subset = (Page_Name == ("陳其邁 Chen Chi-Mai")))
cjuly
cAug <- subset(X201808_data, select = c("Page_Name", "Date", "Message"), subset = (Page_Name == ("陳其邁 Chen Chi-Mai")))
cSep <- subset(X201809_data, select = c("Page_Name", "Date", "Message"), subset = (Page_Name == ("陳其邁 Chen Chi-Mai")))
cOct <- subset(X201810_data, select = c("Page_Name", "Date", "Message"), subset = (Page_Name == ("陳其邁 Chen Chi-Mai")))
cNov <- subset(X201811_data, select = c("Page_Name", "Date", "Message"), subset = (Page_Name == ("陳其邁 Chen Chi-Mai")))
cDec <- subset(X201812_data, select = c("Page_Name", "Date", "Message"), subset = (Page_Name == ("陳其邁 Chen Chi-Mai")))
cJan <- subset(X201901_data, select = c("Page_Name", "Date", "Message"), subset = (Page_Name == ("陳其邁 Chen Chi-Mai")))
Chen2018 <- c(length(cjuly$Page_Name), length(cAug$Page_Name), length(cSep$Page_Name), length(cOct$Page_Name), length(cNov$Page_Name), length(cDec$Page_Name), length(cJan$Page_Name))
Chen2018

kao <- data.frame(Han2018, Chen2018, date)

kao


