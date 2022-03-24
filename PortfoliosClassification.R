### PACKAGES MANAGEMENT ####
options(install.packages.check.source = "no")

pckgs<-c("readxl", "tidyverse", "ggrepel","RColorBrewer","ggpubr")

pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {
  install.packages(pckg,repos="https://cloud.r-project.org/",
                   quiet=TRUE, type="binary")}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}

##### CALCULATING THE NEW COLUMNS ####
# setwd
df<-read_excel("DJ30_data_1.xlsx")
colnames(df)<-c("CompanyName","Ticker","IndexConstituents","MarketCapitalization","TotalEquity","DayClosePrice112019","DayClosePrice12312019","TotalRevenue","TotalExp","InterestExpense","CostGoods","TotalAssets2019","TotalAssets2018")

df<-df %>%
  mutate(SmallBig = ifelse(is.na(df$MarketCapitalization), NA,
                    ifelse(df$MarketCapitalization<=median(df$MarketCapitalization), "SMALL", "BIG")))

df<-df %>%
  mutate(Book2Market = ifelse(df$TotalEquity<0, NA,
                           ifelse(df$MarketCapitalization==0, NA, df$TotalEquity/df$MarketCapitalization)))

df<-df %>%
  mutate(HighLow = ifelse(is.na(df$MarketCapitalization)|is.na(df$TotalEquity)|(df$MarketCapitalization==0)|(df$TotalEquity<0), NA,
                          ifelse(df$Book2Market<=quantile(df$Book2Market,0.3,na.rm=TRUE), "LOW", 
                           ifelse(df$Book2Market<=quantile(df$Book2Market,0.7,na.rm=TRUE),"MEDIUM","HIGH"))))
df<-df %>%
  mutate(RMW = ifelse(df$TotalEquity<=0, NA, (df$TotalRevenue-df$CostGoods-df$TotalExp+df$InterestExpense)/df$TotalEquity))

df<-df %>%
  mutate(RobustWeak = ifelse(is.na(df$TotalRevenue)|is.na(df$TotalExp)|is.na(df$InterestExpense)|is.na(df$CostGoods)|is.na(df$TotalEquity)|(df$TotalEquity<=0), NA,
                          ifelse(df$RMW<=quantile(df$RMW,0.3,na.rm=TRUE), "WEAK", 
                                 ifelse(df$RMW<=quantile(df$RMW,0.7,na.rm=TRUE),"MEDIUM","ROBUST"))))

df<-df %>%
  mutate(CMA = ifelse(df$TotalAssets2018==0, NA, (df$TotalAssets2019-df$TotalAssets2018)/df$TotalAssets2018))

df<-df %>%
  mutate(ConservativeAggressive = ifelse(is.na(df$TotalAssets2019)|is.na(df$TotalAssets2018), NA,
                             ifelse(df$CMA<=quantile(df$CMA,0.3,na.rm=TRUE), "CONSERVATIVE",
                                    ifelse(df$CMA<=quantile(df$CMA,0.7,na.rm=TRUE),"MEDIUM","AGGRESSIVE"))))

# If the new columns are previously calculated
# setwd
# df<-read_excel("DJ30_data_2.xlsx")
# str(df)
# colnames(df)<-c("CompanyName","Ticker","IndexConstituents","MarketCapitalization","TotalEquity","DayClosePrice112019","DayClosePrice12312019","TotalRevenue","TotalExp","InterestExpense","CostGoods","TotalAssets2019","TotalAssets2018","SmallBig","Book2Market","HighLow","RMW","RobustWeak","CMA","ConservativeAggressive")

#### CHARTS ####
df$SmallBig <- factor(df$SmallBig, ordered = TRUE, levels = c("SMALL", "BIG"))

df1<-df[!is.na(df$SmallBig) & !is.na(df$HighLow),]
df1$HighLow <- factor(df1$HighLow, ordered = TRUE, levels = c("LOW", "MEDIUM", "HIGH"))

set.seed(42)

gg1 <- ggplot(df1, aes(x=SmallBig, y=HighLow, color = HighLow, label=Ticker)) +
  geom_text_repel(size=3, segment.color = 'transparent', max.overlaps=15,
                  position=position_jitter(height = 0.2, width=0.2)) + 
  scale_colour_manual(values=c("red", "blue", "darkgreen")) +
  labs(y="High-Low") +
  theme_bw() + theme(legend.position="none", axis.text.x = element_blank(), axis.title.x = element_blank()) 

df2<-df[!is.na(df$SmallBig) & !is.na(df$RobustWeak),]
df2$RobustWeak <- factor(df2$RobustWeak, ordered = TRUE, levels = c("WEAK", "MEDIUM", "ROBUST"))

set.seed(42)

gg2 <- ggplot(df2, aes(x=SmallBig, y=RobustWeak, color = RobustWeak, label=Ticker)) +
  geom_text_repel(size=3, segment.color = 'transparent', max.overlaps=15,
                  position=position_jitter(height = 0.2, width=0.2)) +
  scale_colour_manual(values=c("red", "blue", "darkgreen")) +
  labs(y="Robust-Weak") +
  theme_bw() + theme(legend.position="none", axis.text.x = element_blank(), axis.title.x = element_blank()) 

df3<-df[!is.na(df$SmallBig) & !is.na(df$ConservativeAggressive),]
df3$ConservativeAggressive <- factor(df3$ConservativeAggressive, ordered = TRUE, levels = c("AGGRESSIVE", "MEDIUM", "CONSERVATIVE"))

set.seed(42)

gg3 <- ggplot(df3, aes(x=SmallBig, y=ConservativeAggressive, color = ConservativeAggressive, label=Ticker)) +
  geom_text_repel(size=3, segment.color = 'transparent', max.overlaps=15,
                  position=position_jitter(height = 0.2, width=0.2)) +
  scale_colour_manual(values=c("red", "blue", "darkgreen")) +
  labs(y="Conservative-Aggressive", x="Small-Big") +
  theme_bw() + theme(legend.position="none") 

ggarrange(gg1, gg2, gg3, 
          labels = c("BSHL", "BSRW", "BSCA"),
          ncol = 1, nrow = 3)

ggarrange(gg1, gg2, gg3, 
          ncol = 1, nrow = 3, align = "v")

#### TABLES ####
df[(df$SmallBig=="BIG") & (df$HighLow=="HIGH"),1]
# filter(df,(SmallBig=="BIG" & HighLow=="HIGH"))
df[(df$SmallBig=="BIG") & (df$HighLow=="MEDIUM"),1]
df[(df$SmallBig=="BIG") & (df$HighLow=="LOW"),1]

df[(df$SmallBig=="SMALL") & (df$HighLow=="HIGH"),1]
df[(df$SmallBig=="SMALL") & (df$HighLow=="MEDIUM"),1]
df[(df$SmallBig=="SMALL") & (df$HighLow=="LOW"),1]

df[(df$SmallBig=="BIG") & (df$RobustWeak=="ROBUST"),1]
df[(df$SmallBig=="BIG") & (df$RobustWeak=="MEDIUM"),1]
df[(df$SmallBig=="BIG") & (df$RobustWeak=="WEAK"),1]

df[(df$SmallBig=="SMALL") & (df$RobustWeak=="ROBUST"),1]
df[(df$SmallBig=="SMALL") & (df$RobustWeak=="MEDIUM"),1]
df[(df$SmallBig=="SMALL") & (df$RobustWeak=="WEAK"),1]

df[(df$SmallBig=="BIG") & (df$ConservativeAggressive=="CONSERVATIVE"),1]
df[(df$SmallBig=="BIG") & (df$ConservativeAggressive=="MEDIUM"),1]
df[(df$SmallBig=="BIG") & (df$ConservativeAggressive=="AGGRESSIVE"),1]

df[(df$SmallBig=="SMALL") & (df$ConservativeAggressive=="CONSERVATIVE"),1]
df[(df$SmallBig=="SMALL") & (df$ConservativeAggressive=="MEDIUM"),1]
df[(df$SmallBig=="SMALL") & (df$ConservativeAggressive=="AGGRESSIVE"),1]