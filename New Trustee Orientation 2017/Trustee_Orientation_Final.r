## @knitr setup

# Packages and set Color Palette
require(RColorBrewer)
colorpalette=brewer.pal(8, 'Set1')
colorpalette12 <- brewer.pal(12, 'Set3')
require(zoo)
require(xts)
require(tidyr)
require(ggplot2)
require(tidyr)
require(reshape2)
require(scales)
require(PerformanceAnalytics)
require(knitr)
require(gridExtra)
### Load custom functions and Priv Markets latest Data
source(file = "P:/IMD/Karl/R projects/Public Performance/Scripts/Functions.R")
load('P:/IMD/Karl/R projects/private investment performance/pmedata.rdata')
source('P:/IMD/Karl/R projects/basic financial.r',echo=FALSE)
### Data from Public Performance

#bring in all manager benchmarks and transform data
bm <- unique(mgrinfo[which(mgrinfo$Open=='Y'), 'SSCode'])
bms<-read.csv('P:/IMD/Karl/R projects/Public Performance/BenchmarkData/Index_Returns.csv', stringsAsFactors = F)
bms$Date <- as.Date(bms$Date,format='%m/%d/%Y')
bm.data <- xts(bms[,-1], bms[,1])
bm.data=bm.data/100
ac.key <- read.csv('P:/IMD/Karl/R projects/Public Performance/AssetClassKey.csv', stringsAsFactors = F)
comp <- read.csv('P:/IMD/Karl/R projects/Public Performance/Composite.csv', stringsAsFactors = F)
c.bm <- read.csv("P:/IMD/Karl/R projects/Public Performance/BenchmarkData/Comp_BM.csv", stringsAsFactors = F)
c.bm$Date <- as.Date(c.bm$Date, format='%m/%d/%Y')
comp.bm <- xts(c.bm[,-1], c.bm[,1])
comp.bm <- na.fill(comp.bm, 0)
comp.bm <- comp.bm/100
sub.ac <- read.csv('P:/IMD/Karl/R projects/Public Performance/SubAssetClass.csv', stringsAsFactors = F)
ac.dat <- read.csv("P:/IMD/Karl/R projects/Public Performance/Composites/Asset.Class.csv", stringsAsFactors = F)
ac.dat$Date <- as.Date(ac.dat$Date, format='%m/%d/%Y')
ac.dat$NetReturn <- ac.dat$NetReturn/100
comp.dat <- read.csv("P:/IMD/Karl/R projects/Public Performance/Composites/Composites.csv", stringsAsFactors = F)
comp.dat$Date <- as.Date(comp.dat$Date, format='%m/%d/%Y')
comp.dat$NetReturn <- comp.dat$NetReturn/100
subac.f <- read.csv("P:/IMD/Karl/R projects/Public Performance/Composites/Sub.AC.csv", stringsAsFactors = F)
subac.f$Date <- as.Date(subac.f$Date, format='%m/%d/%Y')
subac.f$NetReturn <- subac.f$NetReturn/100
bm.info<-read.csv('P:/IMD/Karl/R projects/Public Performance/BenchmarkMapping.csv', stringsAsFactors = F)
pw.file <- read.csv(paste0("P:/IMD/Karl/R projects/Total Fund Active Weights/Interim Policy BM.wide.csv"),stringsAsFactors = F)
pw.file$Date <- as.Date(pw.file$Date,format='%m/%d/%Y')

## @knitr total_return

# Import data from NEPC Total Fund File
file <- read.csv('P:/IMD/Karl/R projects/Total Fund Active Weights/Attribution/NEPC Data.csv',stringsAsFactors = F)
file$Date <- as.yearmon(file$Date,format='%m/%d/%Y')
# Subset file by Total Fund and Benchmark Return Data
ret <- subset(file, file$Asset.Class=="Total Fund", select = c("Date","Port.Return","BM.Return"))
tf.return <- zoo(ret[,-1],ret[,1])
tf.gd.return <- gd(tf.return)
colnames(tf.gd.return) <- c("Total Fund", "Policy Benchmark")
tf.gd.ret.long <- gather(data.frame(Date=index(tf.gd.return), coredata(tf.gd.return)), Name, Return, -Date)
# Plot just the total return of Total Fund vs Benchmark
tf.return.plot <- ggplot(tf.gd.ret.long, aes(x=Date, y=Return, colour=Name))+geom_line()+
  ggtitle("Total Fund & Interim Policy Benchmark Performance")+ylab("Growth of a Dollar")+xlab("")+
  scale_x_yearmon()+ theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5))
print(tf.return.plot)


## @knitr tf_brinson

# Subset file by Total Fund Attribution Data
total <- subset(file, file$Asset.Class=="Total Fund", select = 
                  c("Date","Allocation", "Selection","Interaction","Residual", "Total"))
total.z <-zoo(total[,-1]/100, total[,1])
mv.tot <- subset(file, file$Asset.Class=="Total Fund", select = c("Date","BMV"))
mv.tot <- zoo(mv.tot[,-1], mv.tot[,1])
gdminus1.tot <- gdminus1(total.z[ ,-5])
gd.tot <- gd(total.z[,'Total'])-1
#converts to a long form data frame
total.df=gg(gdminus1.tot,"Total","Contribution")
#seperate the positive and negative values
allpos=subset(total.df,Contribution>0)
allneg=subset(total.df,Contribution<0)
#create stacked bar chart
total.attribution<- ggplot()+
  geom_bar(data=allpos,aes(x=Date,y=Contribution,fill=Total),stat="identity")+
  geom_bar(data=allneg,aes(x=Date,y=Contribution,fill=Total),stat="identity")+
  geom_line(data = gd.tot, aes(x=Index, y=gd.tot[,2]), colour='navy', size=.5)+
  ylab("Cumulative Excess Return (Line)")+scale_y_continuous(labels=percent)+
  scale_fill_manual(values = colorpalette, name = "Attribution") +
  scale_x_yearmon() + ggtitle("Total Fund Brinson Attribution") + xlab("")

tot.va <- mv.tot * total.z
dl <- dim(tot.va)[1]
tr.12 <- data.frame(Period = "One Year",`Value Add` = round(sum(tot.va[((dl - 11):dl), "Total"])/1000000,2), row.names = NULL)
tr.36 <- data.frame(Period = "Three Year",`Value Add` = round(sum(tot.va[((dl - 35):dl), "Total"])/1000000,2), row.names = NULL)
tr.60 <- data.frame(Period = "Five Year", `Value Add` = round(sum(tot.va[((dl - 59):dl), "Total"])/1000000,2), row.names = NULL)
tr.120 <- data.frame(Period = "Ten Year",`Value Add` = round(sum(tot.va[((dl - 119):dl), "Total"])/1000000,2), row.names = NULL)
tr.va <- rbind(tr.12,tr.36, tr.60, tr.120)
va.tf.plot <- ggplot(tr.va,aes(x=Period,y=Value.Add, fill=Period))+
  geom_bar(stat="identity",position=position_dodge())+
  ggtitle("Total Fund $ Value Add by Trailing Periods")+
  ylab("in Millions")+labs(fill="Total")+scale_y_continuous(labels = scales::dollar)+
  scale_fill_manual(values=colorpalette)+
  theme(plot.title = element_text(hjust = 0.5))
grid.arrange(total.attribution,va.tf.plot,ncol=2)

## @knitr tf_breakdown

# Allocation Affect only
allocation.only <- subset(file, file$Asset.Class=="Total Fund", select = 
                            c("Date","Allocation"))
allocation.z <-zoo(allocation.only[,-1]/100, allocation.only[,1])
r12.allo <-  apply.rolling(allocation.z, FUN = 'Return.annualized', width = 36, scale=12)
r12.allo <- r12.allo[-(1:35)]
#create rolling 36 month line chart
r36.allocation <- ggplot()+ 
  geom_line(data = r12.allo, aes(x=Index, y=coredata(r12.allo)), colour='blue', size=.5) + 
  ylab("Allocation Effect")+scale_y_continuous(labels=percent)+
  scale_x_yearmon() + ggtitle("Total Fund Allocation Effect \n Rolling 3 Year") + 
  xlab("") + theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5))
# Selection Affect only
selection.only <- subset(file, file$Asset.Class=="Total Fund", select = 
                           c("Date","Selection"))
selection.z <-zoo(selection.only[,-1]/100, selection.only[,1])
r12.sel <-  apply.rolling(selection.z, FUN = 'Return.annualized', width = 36, scale=12)
r12.sel <- r12.sel[-(1:35)]
#create rolling 36 month line chart
r36.selection <- ggplot()+ 
  geom_line(data = r12.sel, aes(x=Index, y=coredata(r12.sel)), colour='blue', size=.5) + 
  ylab("Selection Effect")+scale_y_continuous(labels=percent)+
  scale_x_yearmon() + ggtitle("Total Fund Selection Effect \n Rolling 3 Year") + 
  xlab("") + theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5))
grid.arrange(r36.allocation,r36.selection,ncol=2)


## @knitr saa
saa <- read.csv('P:/IMD/Karl/R projects/Total Fund Active Weights/SAA.changes.csv',stringsAsFactors = F)
saa$Date <- as.Date(saa$Date,format='%m/%d/%Y')
saa.ld <- gather(saa, `Asset Class`,Weight,-Date)
saa.plot <- ggplot()+ geom_bar(data=saa.ld,aes(x=Date,y=Weight,fill=`Asset Class`),stat="identity")+
  scale_y_continuous(labels = percent) + scale_x_date(date_labels = "%Y") +
  scale_fill_manual(values = colorpalette, name = "Asset Class") + xlab("") + 
  ylab("Target Weight")+ theme(plot.title = element_text(hjust = 0.5))
print(saa.plot)

## @knitr tf_weights

# Allocation by $ Value
# weights
pw.ma <- read.csv(paste0("P:/IMD/Karl/R projects/Total Fund Active Weights/PolicyWeightsMA.csv"),stringsAsFactors = F)
pw.ma$Date <- as.Date(pw.ma$Date,format='%m/%d/%Y')
ma.pw.zoo <- zoo(pw.ma[,-1], pw.ma[,1])
#pull private market values
total.cats <- c("Total PE", "Total RE", "Total POPP", "Total OPP", "Total PD","Total FARM")
names <- c("Private Equity", "Real Estate", "Opp. Equity", "Opp. Debt", "Private Debt", "Farmland")
mv.tot <- list()
for(i in total.cats) {
  cv <- y.v[[i]]
  hv <- y.hv[[i]]
  comb <- mergesum.z(cv, hv)
  mv.tot[[i]] <- comb
}
priv.mvs <- do.call(merge, mv.tot)
priv.mvs <- na.fill(priv.mvs, 0)
priv.mvs$`Private Markets` <- rowSums(priv.mvs)
val.date <- time(priv.mvs)[dim(priv.mvs)[1]]
#Get all public composite data
public.cats <- c('DOM', 'Intl','IL', 'FI', 'MA', 'OC', 'AC')
mvlist <- list()
for(i in public.cats){
  if(i == "DOM"|i == "Intl"){
    d <- subset(comp.dat, comp.dat$Composite==i,select = c('Date','MktVal'))
    d.zoo <- zoo(d[ ,-1], d[ ,1])
    if(i == "DOM"){
      rfp <- subset(mgr.data, mgr.data$FundID == "A1PN", select = c('Date','MktVal'))
      rfp.zoo <- zoo(rfp[ ,-1], rfp[ ,1])
      total <- merge(d.zoo, rfp.zoo)
      total <- na.fill(total, 0)
      d.zoo <- zoo(rowSums(total), time(total))
    }
    mvlist[[i]] <- d.zoo
  } else {
    d <- subset(ac.dat, ac.dat$Asset.Class==i,select = c('Date','MktVal'))
    d.zoo <- zoo(d[,-1], d[,1])
    mvlist[[i]] <- d.zoo
    #mvlist = c(mvlist, list(d.zoo)) 
  }
}
pub.mvs <- do.call(merge, mvlist)
pub.mvs <- na.fill(pub.mvs, fill = 0)
#Multi-asset class Apportioning across asset classes
ma.mvs <- ma.pw.zoo * pub.mvs$MA
i.d <- time(ma.mvs)[1]
saa.change <- time(ma.mvs)[dim(ma.mvs)[1]]
comb <- merge(pub.mvs, ma.mvs)
comb <- na.fill(comb, 0)
both <- intersect(names(pub.mvs), names(ma.mvs))
only1 <- setdiff(names(pub.mvs), both)
comb$DOM <- comb$DOM.pub.mvs + comb$DOM.ma.mvs
comb$Intl <- comb$Intl.pub.mvs + comb$Intl.ma.mvs
comb$IL <- comb$IL.pub.mvs + comb$IL.ma.mvs
comb$FI <- comb$FI.pub.mvs + comb$FI.ma.mvs
comb$Cash <- comb$OC + comb$AC
ma <- window(comb$MA, start = i.d, end = saa.change)
ma <- ma * 0
MA <- rbind(ma, window(comb$MA, start = "2015-04-30"))
pub.mvs.adj <- comb[, c("DOM", "Intl", "IL", "FI", "Cash")]
pub.mvs.adj <- merge(pub.mvs.adj, MA)
#Combine and calc active weights
tf <- merge(pub.mvs.adj, priv.mvs$`Private Markets`)
#turn data into quarter end data
quarter.end <- endpoints(tf, on = "quarters")
quarterly <- period.apply(tf, INDEX = quarter.end, FUN = last)
start.qtr <- time(quarterly[dim(quarterly)[1]-40])
tf.qtr <- window(quarterly, start = start.qtr, end = val.date)
timeline <- as.yearmon(time(tf.qtr))
tf.qtr <-na.fill(tf.qtr, 0)
colnames(tf.qtr) <- c("Domestic Equity", "International Equity", "Public Inflation-Linked", 
                      "Public Fixed Income", "Cash", "Multi-Asset Class","Private Markets")
tf <- rowSums(tf.qtr)
tf.weights <- tf.qtr/tf
tf.qtr.tidy <- melt(cbind(data.frame(Date=as.yearmon(time(tf.weights))), as.data.frame(tf.weights)), id.vars="Date")
colnames(tf.qtr.tidy)[2] <- "Asset Class"
#plot of just the market values
allocation <- ggplot()+
  geom_bar(data=tf.qtr.tidy,aes(x=Date,y=value,fill=`Asset Class`),stat="identity")+
  ggtitle("Quarterly Weights by Asset Class") + scale_y_continuous(labels = percent) + 
  scale_x_yearqtr(format = "%Y")+
  scale_fill_manual(values = colorpalette, name = "Asset Class") + xlab("") + 
  ylab("Weight")
tf <- merge(pub.mvs.adj, priv.mvs)
tf <- tf[,-13]
quarter.end <- endpoints(tf, on = "quarters")
quarterly <- period.apply(tf, INDEX = quarter.end, FUN = last)
start.qtr <- time(quarterly[dim(quarterly)[1]-40])
tf.qtr <- window(quarterly, start = start.qtr, end = val.date)
tf.qtr <-na.fill(tf.qtr, 0)

#plot of the active weights
pw.zoo <- zoo(pw.file[,-1], pw.file[,1])
#pw.z$`Private Markets` <- pw.z$RE + pw.z$PE + pw.z$PD + pw.z$POPP + pw.z$OPP + pw.z$FARM
#pw.zoo <- pw.z[,-c(5,6,7,10,11,12)]
colnames(pw.zoo) <- c("Domestic Equity","International Equity", "Public Inflation-Linked", 
                      "Public Fixed Income","Cash", "Multi-Asset Class", "Private Equity",
                      "Real Estate","Private Opportunistic Equity","Opportunistic Debt",
                      "Private Debt","Farmland & Infrastructure")
tf.mvs <- zoo(rowSums(coredata(tf.qtr)), timeline)
assets <- colnames(tf.qtr)
aw.list <- list()
for(a in assets){
  m <- as.data.frame(coredata(tf.qtr))
  mvs.zoo <- zoo(m, timeline)
  pct <- mvs.zoo[,a]/tf.mvs
  aw.list[[a]] <- pct
  #aw.list <- c(aw.list, list(pct))
}
weight.actual <- do.call(merge, aw.list)
colnames(weight.actual) <-colnames(pw.zoo)
#colnames(weight.actual) <- assets
qtr.end <- endpoints(pw.zoo, on = "quarters")
quarterly.pw <- period.apply(pw.zoo, INDEX = qtr.end, FUN = last)
pw.qtr <- window(quarterly.pw, start = "1998-03-31", end = val.date)
weight.policy <- zoo(coredata(pw.qtr), timeline)
over.under <- weight.actual - weight.policy
alloc.de=gg(over.under,"Allocation","Difference")
allpos=subset(alloc.de,Difference>0)
allneg=subset(alloc.de,Difference<0)
active.weight <- ggplot()+
  geom_bar(data=allpos,aes(x=Date,y=Difference,fill=Allocation),stat="identity")+
  geom_bar(data=allneg,aes(x=Date,y=Difference,fill=Allocation),stat="identity")+xlab("")+
  scale_y_continuous(labels=percent) + labs(fill='Allocation') + 
  ylab('Active Weight') +
  scale_fill_manual(values = colorpalette12, name = 'Asset Class') +
  ggtitle("Total Fund Active Weights \n Relative to Interim SAA Benchmark")+
  theme(plot.title = element_text(hjust = 0.5))
grid.arrange(allocation,active.weight,ncol=2)

## @knitr equities

d=subset(ac.dat, ac.dat$Asset.Class=='EQ', select = c('Date','NetReturn'))
d.xts <- xts(d[,-1], d[,1])
i.d <- as.character(time(d.xts)[1])
b=comp.bm[,c("Date",paste0('EQ',".BM"))]
bm.dat <- b[paste0(i.d,"/"),]
data <- merge(d.xts, bm.dat)
name <- ac.key[which(ac.key$Asset.Class=='EQ'), 'ACName']
colnames(data) <- c(name, 'Benchmark')
charts.PerformanceSummary(data, ylog = TRUE, wealth.index = TRUE, main=paste(name,'Performance Summary'), 
                          colorset = c('blue','darkorange'))


## @knitr eq_attribution

pw.file <- read.csv(paste0("P:/IMD/Karl/R projects/Public Performance/PolicyWeights",'EQ','.csv'),stringsAsFactors = F)
pw.file$Date <- as.Date(pw.file$Date,format='%m/%d/%Y')
n <- 120
timeline <- as.yearmon(pw.file[((dim(pw.file)[1]-(n-1)):dim(pw.file)[1]),'Date'])
assets <- colnames(pw.file)[-1]
weight.policy <- zoo(pw.file[((dim(pw.file)[1]-(n-1)):dim(pw.file)[1]), assets], timeline)

#get market values, returns, and calculate weights
mvlist <- list()
ret.list <- list()
for(i in assets){
  d <- subset(subac.f, subac.f$SubAC==i,select = c('Date','MktVal'))
  d.xts <- xts(d[,-1], d[,1])
  colnames(d.xts) <- i
  mvlist = c(mvlist, list(d.xts))
  dr <- subset(subac.f, subac.f$SubAC==i, select = c("Date", "NetReturn"))
  dr$NetReturn <- dr$NetReturn
  dr.xts <- xts(dr[,-1], dr[,1])
  colnames(dr.xts) <- i
  ret.list = c(ret.list, list(dr.xts))
}
mvs <- do.call(merge, mvlist)
mvs <- na.fill(mvs, fill = 0)
mvs.data <- mvs[((dim(mvs)[1]-(n-1)):dim(mvs)[1]),]
eq.mv=subset(ac.dat, ac.dat$Asset.Class=='EQ', select = c('Date',"MktVal"))
eq.mv$Date = as.yearmon(eq.mv$Date)
eq.mv.z <- zoo(eq.mv[,-1],eq.mv[,1])
eq.mv.z <- eq.mv.z[((length(eq.mv.z)-(n-1)):length(eq.mv.z)),]
total <- zoo(rowSums(mvs.data), timeline)
aw.list <- list()
for(i in assets){
  m <- as.data.frame(coredata(mvs.data))
  mvs.zoo <- zoo(m, timeline)
  pct <- mvs.zoo[,i]/coredata(eq.mv.z)
  aw.list <- c(aw.list, list(pct))
}
weight.actual <- do.call(merge, aw.list)
colnames(weight.actual) <- assets
over.under <- weight.actual - weight.policy
ra <- do.call(merge, ret.list)
colnames(ra) <- assets
ra <- as.data.frame(coredata(ra))
return.actual <- zoo(ra[((dim(mvs)[1]-(n-1)):dim(mvs)[1]),], timeline)
return.actual <- na.fill(return.actual, 0)
m <- ncol(return.actual)
bm.name <- paste0(assets, '.BM')
return.benchmark=zoo(comp.bm[((dim(comp.bm)[1]-(n-1)):dim(comp.bm)[1]), bm.name], timeline)
return.benchmark <- na.fill(return.benchmark, fill = 0)

#calculate returns (asset class benchmark simply need to be pulled from file above)
ca <- subset(ac.dat, ac.dat$Asset.Class == 'EQ', select = c("Date", "NetReturn"))
ca.xts <- xts(ca[,-1], ca[,1])
ca.xts <- as.data.frame(coredata(ca.xts[((dim(ca.xts)[1]-(n-1)):dim(ca.xts)[1]),]))
composite.actual <- zoo(ca.xts,timeline)
ac.bm <- comp.bm[ ,paste0('EQ','.BM')]
ac.bm <- as.data.frame(coredata(ac.bm))
composite.benchmark <- zoo(ac.bm[((dim(comp.bm)[1]-(n-1)):dim(comp.bm)[1]),], timeline)
excess=composite.actual-composite.benchmark
#Selection effect
selection <- weight.policy*(return.actual-return.benchmark)
#selection <- na.fill(selection, fill = 0)
colnames(selection)=paste0(assets,".sel")

#allocation effect
allocation <- return.benchmark*(weight.actual-weight.policy)
#allocation <- na.fill(allocation, fill = 0)
colnames(allocation)=paste0(assets,".allo")

#interaction effect
interaction <- (return.actual-return.benchmark)*(weight.actual-weight.policy)
#interaction <- na.fill(interaction, fill = 0)
colnames(interaction)=paste0(assets,".inter")

attrib.df=merge(selection,allocation,interaction)
attrib.fr=frong(attrib.df,composite.actual,composite.benchmark)

#Plots of Attribution Analysis
#plot just the cumulative attribution effects
attrib.sum <- sumdfbycol(attrib.fr,c(".sel",".allo",".inter"),
                         c("Selection","Allocation","Interaction"))
attrib.sum <- cumsum(attrib.sum)
attrib.sum <- gg(attrib.sum,"Attribution","Growth_of_Dollar")
attrpos <- subset(attrib.sum,Growth_of_Dollar>=0)
attrneg <- subset(attrib.sum,Growth_of_Dollar<0)
excess.ret <- apply.rolling(excess, FUN = 'Return.annualized', width = 12, scale=12)
excess.ret <- coredata(excess.ret)
eq.attribution <- ggplot()+
  geom_bar(data=attrpos,aes(x=Date,y=Growth_of_Dollar,fill=Attribution),stat="identity")+
  geom_bar(data=attrneg,aes(x=Date,y=Growth_of_Dollar,fill=Attribution),stat="identity")+
  scale_fill_manual(values = colorpalette) +
  scale_y_continuous(labels=percent) + ggtitle("Brinson Return Attribution")+
  theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5))
print(eq.attribution)

## @knitr eq_rolling

attribroll36=roll.attr(attrib.df,36,".allo",
                       "Allocation",
                       composite.actual,
                       composite.benchmark)
roll.36 <- data.frame("Date"=index(attribroll36), "Allocation"=coredata(attribroll36))
r36.allocation <- ggplot()+
  geom_line(data=roll.36,aes(x=Date,y=Allocation), colour='blue', size=0.5)+
  ggtitle("Rolling 36 Month Allocation Effect")+
  scale_fill_manual(values = colorpalette) + scale_x_yearmon(format = "%Y")+
  scale_y_continuous('36 Month Annualized Excess Return',labels=percent) +
  theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5))
attribroll36=roll.attr(attrib.df,36,".sel",
                       "Selection",
                       composite.actual,
                       composite.benchmark)
roll.36 <- data.frame("Date"=index(attribroll36), "Selection"=coredata(attribroll36))
r36.selection <- ggplot()+
  geom_line(data=roll.36,aes(x=Date,y=Selection), colour='blue', size=0.5)+
  ggtitle("Rolling 36 Month Selection Effect")+
  scale_fill_manual(values = colorpalette) + scale_x_yearmon(format = "%Y")+
  scale_y_continuous('36 Month Annualized Excess Return',labels=percent) +
  theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5))
grid.arrange(r36.allocation, r36.selection, ncol=2)

## @knitr e_portfolios

#E Portfolio Turnaround
internal <- mgrinfo[which(mgrinfo$Asset.Class == "EQ" & mgrinfo$Open =='Y' & 
                            mgrinfo$Internal == 'Y'), 'FundID']
i.excess <- list()
e.mv.list <- list()
r12.list <- list()
for(i in internal){
  d=subset(mgr.data, mgr.data$FundID == i, select = c('Date','NetReturn','MktVal'))
  s.name <- mgrinfo[which(mgrinfo$FundID==i),'ShortName']
  mv = xts(d[,'MktVal'],d[,1])
  e.mv.list[[s.name]] = mv
  d.xts <- xts(d[,"NetReturn"], d[,1])
  i.d <- as.character(time(d.xts)[1])
  b.code <- mgrinfo[which(mgrinfo$FundID == i),'SSCode']
  bench <- bm.data[paste0(i.d, '/') , b.code]
  data <- merge(d.xts, bench)
  act.ex <- data[ ,1] - data[ ,2]
  i.excess[[s.name]] <-act.ex
  r12 <- apply.rolling(act.ex,FUN = 'Return.annualized', width = 12)
  r12.list[[s.name]] <- r12
}
i.data <- do.call(merge,i.excess)
colnames(i.data) <- names(i.excess)
i.data = na.fill(i.data,0)
mv.data <- do.call(merge,e.mv.list)
mv.data = na.fill(mv.data,0)
colnames(mv.data) = names(e.mv.list)
total=rowSums(mv.data)
weights=mv.data/total
wa = weights * i.data
#wa$Total = round(rowSums(wa)*10000,1)
wa$Total=rowSums(wa)
wa.trailing = wa[((dim(wa)[1]-(36-1)):dim(wa)[1]),]
total.gd=round((gd(wa.trailing$Total)-1)*10000,2)
wa.e <- ggplot(total.gd, aes(x=Index, y=Total))+geom_point(colour='blue')+ 
  stat_smooth(method="loess", colour='darkorange')+
  ggtitle("E Portfolios Cumulative Excess Performance")+ylab("Excess Return in bps")+xlab("")+
  scale_x_date(date_labels = "%m/%Y")+ 
  theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5))
print(wa.e)

## @knitr eq_privates

#Calculate peformance stats for pe and opp eq
#get ending values
e.v.pe = y.v[["Total PE"]]
e.v.popp = y.v[["Total POPP"]]
e.v <- e.v.pe + e.v.popp

#get cash flows
cf.pe <- y.cf[["Total PE"]]
cf.pe = cf.pe[index(cf.pe) <= valdate]
cf.popp <- y.cf[["Total POPP"]]
cf.popp = cf.popp[index(cf.popp) <= valdate]
comb.cf <- mergesum.z(cf.pe,cf.popp,e.v)
r2k <- bench.lst$`^RUT`
r2k = r2k[index(r2k) <= valdate]
beg.date <- index(cf.pe)[1]
r2k = r2k[index(r2k) >= beg.date]
ans.pe = pestats(comb.cf,r2k)
sir.new <- data.frame("IRR" = ans.pe$irr*100, "Benchmark" = ans.pe$ind.irr*100,
                      row.names = "Total PE")
pme.new <- data.frame("irr" = ans.pe$irr*100, "tvpi" = ans.pe$tvpi,
                      row.names = "Total PE")
colnames(pme.new) = c("Fund IRR", "Fund TVPI")

#get rest of info from pme.df
total.cats <- c("Total PE Current Portfolio", "Total PE Legacy Portfolio", "Total POPP")
names <- c("Private Equity & \n Priv Opp. Equity", "Current Private Equity", "Legacy Private Equity", 
           "Private Opp. Equity")
#pull just return data and benchmarks
pms.returns <- list()
for(i in total.cats) {
  pme <- pme.df[which(rownames(pme.df)==i),]
  irr <- pme[ ,"Fund IRR"]
  if(pme$Portfolio == "PE") {bm <- pme[ ,"Russell 2K Dollar Matched IRR"]} else
    if(pme$Portfolio == "POPP") {bm <- pme[ ,"Fixed 8 Dollar Matched IRR"]}
  ret.df <- data.frame("IRR"=irr,"Benchmark"=bm)
  pms.returns[[i]] <- ret.df
}
sir <- do.call(rbind,pms.returns)
sir = rbind(sir.new, sir)
sir$id <- 1:length(names)
sir.long <- gather(sir, id, Return)
colnames(sir.long)[2] <- "Type"
sir.long$Return <- round(sir.long$Return/100,4)
priv.returns <- ggplot(sir.long)+
  geom_bar(aes(x=id,y=Return, fill=Type),stat="identity",position= "dodge")+
  ggtitle("Since Inception IRR and Dollar \n Matched Benchmark IRR")+
  scale_x_continuous("",breaks=1:length(names),labels=names)+
  xlab("") + scale_fill_manual(values=c("darkorange","Blue"))+ylab("IRR")+
  scale_y_continuous(labels=percent)+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5))

#pull pme, irr, and tvpi data and graph
pme.tot <- list()
pme.bm <- vector()
pme.bm = c(pme.bm, ans.pe$pme)
for(i in total.cats) {
  pme <- pme.df[which(rownames(pme.df)==i),]
  irr.tvpi <- pme[ ,c("Fund IRR","Fund TVPI")]
  pme.tot[[i]] <- irr.tvpi
  if(pme$Portfolio == "PE") {bm <- pme[ ,"Russell 2K PME"]} else
    if(pme$Portfolio == "POPP") {bm <- pme[ ,"Fixed 8 PME"]}
  pme.bm <- c(pme.bm,bm)
}
comb.pme <- do.call(rbind, pme.tot)
comb.pme = rbind(pme.new, comb.pme)
rownames(comb.pme) <- names
pmepal=(brewer.pal(n=10,name="Spectral"))[c(1,3,8,9,10)]
max.y=max(comb.pme[ ,"Fund IRR"])   
min.y=min(comb.pme[ ,"Fund IRR"])   
yinc=(max(.05,(max.y-min.y)))/40   
max.y=-.005+(.1*(floor(max.y*10)))   
min.x=min(comb.pme[ ,"Fund TVPI"])-.05
pme.R2k=cut(pme.bm,c(0,.8,.95,1.05,1.2,100),labels=FALSE)
palette(pmepal)
pme.plot <- ggplot(comb.pme, aes(x=`Fund TVPI`,y=`Fund IRR`))+ 
  geom_text(label=rownames(comb.pme), size=3, colour = pme.R2k)+
  xlab("TVPI")  + ggtitle("Private Equties Summary")+
  annotate("text",y = max.y, x = min.x,label ="PME is > 1.2", size=2.5, colour=pmepal[5], hjust=0)+
  annotate("text",y=max.y-yinc,x=min.x,label="PME is 1.05 to 1.2",size=2.5,colour=pmepal[4],hjust=0)+
  annotate("text",y=max.y-(yinc*2),x=min.x,label="PME is .95 to 1.05",size=2.5,colour=pmepal[3],hjust=0)+
  annotate("text",y=max.y-(yinc*3),x=min.x,label="PME is .8 to .95",size=2.5,colour=pmepal[2],hjust=0)+
  annotate("text",y=max.y-(yinc*4),x=min.x,label="PME is < .8",size=2.5,colour=pmepal[1],hjust=0)+
  theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5))
grid.arrange(priv.returns,pme.plot,ncol=2)

## @knitr fi_summary

d=subset(ac.dat, ac.dat$Asset.Class=='FI', select = c('Date','NetReturn'))
d.xts <- xts(d[,-1], d[,1])
i.d <- as.character(time(d.xts)[1])
b=comp.bm[,c("Date",paste0('FI',".BM"))]
bm.dat <- b[paste0(i.d,"/"),]
data <- merge(d.xts, bm.dat)
name <- ac.key[which(ac.key$Asset.Class=='FI'), 'ACName']
colnames(data) <- c(name, 'Benchmark')
charts.PerformanceSummary(data, ylog = TRUE, wealth.index = TRUE, 
                          main=paste(name,'Performance Summary'),
                          colorset = c('blue','darkorange'))

## @knitr fi_attribution

pw.file <- read.csv(paste0("P:/IMD/Karl/R projects/Public Performance/PolicyWeights",'FI','.csv'),
                    stringsAsFactors = F)
pw.file$Date <- as.Date(pw.file$Date,format='%m/%d/%Y')
n <- 120
timeline <- as.yearmon(pw.file[((dim(pw.file)[1]-(n-1)):dim(pw.file)[1]),'Date'])
assets <- colnames(pw.file)[-1]
weight.policy <- zoo(pw.file[((dim(pw.file)[1]-(n-1)):dim(pw.file)[1]), assets], timeline)

#get market values, returns, and calculate weights
mvlist <- list()
ret.list <- list()
for(i in assets){
  d <- subset(comp.dat, comp.dat$Composite==i,select = c('Date','MktVal'))
  d.xts <- xts(d[,-1], d[,1])
  colnames(d.xts) <- i
  mvlist = c(mvlist, list(d.xts))
  dr <- subset(comp.dat, comp.dat$Composite==i, select = c("Date", "NetReturn"))
  dr$NetReturn <- dr$NetReturn
  dr.xts <- xts(dr[,-1], dr[,1])
  colnames(dr.xts) <- i
  ret.list = c(ret.list, list(dr.xts))
}
mvs <- do.call(merge, mvlist)
mvs <- na.fill(mvs, fill = 0)
mvs.data <- mvs[((dim(mvs)[1]-(n-1)):dim(mvs)[1]),]
total <- zoo(rowSums(mvs.data), timeline)
aw.list <- list()
for(i in assets){
  m <- as.data.frame(coredata(mvs.data))
  mvs.zoo <- zoo(m, timeline)
  pct <- mvs.zoo[,i]/total
  aw.list <- c(aw.list, list(pct))
}
weight.actual <- do.call(merge, aw.list)
colnames(weight.actual) <- assets
over.under <- weight.actual - weight.policy
ra <- do.call(merge, ret.list)
colnames(ra) <- assets
ra <- as.data.frame(coredata(ra))
return.actual <- zoo(ra[((dim(mvs)[1]-(n-1)):dim(mvs)[1]),], timeline)
return.actual <- na.fill(return.actual, 0)
m <- ncol(return.actual)
bm.name <- paste0(assets, '.BM')
return.benchmark=zoo(comp.bm[((dim(comp.bm)[1]-(n-1)):dim(comp.bm)[1]), bm.name], timeline)
return.benchmark <- na.fill(return.benchmark, fill = 0)

#calculate returns (asset class benchmark simply need to be pulled from file above)
ca <- subset(ac.dat, ac.dat$Asset.Class == 'FI', select = c("Date", "NetReturn"))
ca.xts <- xts(ca[,-1], ca[,1])
ca.xts <- as.data.frame(coredata(ca.xts[((dim(ca.xts)[1]-(n-1)):dim(ca.xts)[1]),]))
composite.actual <- zoo(ca.xts,timeline)
ac.bm <- comp.bm[ ,paste0('FI','.BM')]
ac.bm <- as.data.frame(coredata(ac.bm))
composite.benchmark <- zoo(ac.bm[((dim(comp.bm)[1]-(n-1)):dim(comp.bm)[1]),], timeline)
excess=composite.actual-composite.benchmark
#Selection effect
selection <- weight.policy*(return.actual-return.benchmark)
#selection <- na.fill(selection, fill = 0)
colnames(selection)=paste0(assets,".sel")

#allocation effect
allocation <- return.benchmark*(weight.actual-weight.policy)
#allocation <- na.fill(allocation, fill = 0)
colnames(allocation)=paste0(assets,".allo")

#interaction effect
interaction <- (return.actual-return.benchmark)*(weight.actual-weight.policy)
#interaction <- na.fill(interaction, fill = 0)
colnames(interaction)=paste0(assets,".inter")

attrib.df=merge(selection,allocation,interaction)
attrib.fr=frong(attrib.df,composite.actual,composite.benchmark)

#Plots of Attribution Analysis
#plot just the cumulative attribution effects
attrib.sum <- sumdfbycol(attrib.fr,c(".sel",".allo",".inter"),
c("Selection","Allocation","Interaction"))
attrib.sum <- cumsum(attrib.sum)
attrib.sum <- gg(attrib.sum,"Attribution","Growth_of_Dollar")
attrpos <- subset(attrib.sum,Growth_of_Dollar>=0)
attrneg <- subset(attrib.sum,Growth_of_Dollar<0)
excess.ret <- apply.rolling(excess, FUN = 'Return.annualized', width = 12, scale=12)
excess.ret <- coredata(excess.ret)
fi.attribution <- ggplot()+
  geom_bar(data=attrpos,aes(x=Date,y=Growth_of_Dollar,fill=Attribution),stat="identity")+
  geom_bar(data=attrneg,aes(x=Date,y=Growth_of_Dollar,fill=Attribution),stat="identity")+
  scale_fill_manual(values = colorpalette) +
  scale_y_continuous(labels=percent) + ggtitle("Brinson Return Attribution")
print(fi.attribution)

## @knitr fi_rolling

attribroll36=roll.attr(attrib.df,36,".allo",
                       "Allocation",
                       composite.actual,
                       composite.benchmark)
roll.36 <- data.frame("Date"=index(attribroll36), "Allocation"=coredata(attribroll36))
r36.allocation <- ggplot()+
  geom_line(data=roll.36,aes(x=Date,y=Allocation), colour='blue', size=0.5)+
  ggtitle("Rolling 36 Month Allocation Effect")+
  scale_fill_manual(values = colorpalette) + scale_x_yearmon(format = "%Y")+
  scale_y_continuous('36 Month Annualized Excess Return',labels=percent) +
  theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5))
attribroll36=roll.attr(attrib.df,36,".sel",
                       "Selection",
                       composite.actual,
                       composite.benchmark)
roll.36 <- data.frame("Date"=index(attribroll36), "Selection"=coredata(attribroll36))
r36.selection <- ggplot()+
  geom_line(data=roll.36,aes(x=Date,y=Selection), colour='blue', size=0.5)+
  ggtitle("Rolling 36 Month Selection Effect")+
  scale_fill_manual(values = colorpalette) + scale_x_yearmon(format = "%Y")+
  scale_y_continuous('36 Month Annualized Excess Return',labels=percent) +
  theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5))
grid.arrange(r36.allocation, r36.selection, ncol=2)

## @knitr fi_privates

total.cats <- c("Total OPP", "Total PD")
names <- c("Opportunistic Debt", "Private Debt")
#pull just return data and benchmarks
pms.returns <- list()
for(i in total.cats) {
  pme <- pme.df[which(rownames(pme.df)==i),]
  irr <- pme[ ,"Fund IRR"]
    if(pme$Portfolio == "OPP") {bm <- pme[ ,"Fixed 8 Dollar Matched IRR"]} else
      if(pme$Portfolio == "PD") {bm <- pme[ ,"Lev Loan+250 Dollar Matched IRR"]}
  ret.df <- data.frame("IRR"=irr,"Benchmark"=bm)
  pms.returns[[i]] <- ret.df
}
sir <- do.call(rbind,pms.returns)
sir$id <- 1:length(names)
sir.long <- gather(sir, id, Return)
colnames(sir.long)[2] <- "Type"
sir.long$Return <- round(sir.long$Return/100,4)
priv.returns <- ggplot(sir.long)+
  geom_bar(aes(x=id,y=Return, fill=Type),stat="identity",position= "dodge")+
  ggtitle("Since Inception IRR and Dollar \n Matched Benchmark IRR")+
  scale_x_continuous("",breaks=1:length(names),labels=names)+
  xlab("") + scale_fill_manual(values=c("darkorange","Blue"))+ylab("IRR")+
  scale_y_continuous(labels=percent)+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5))
#pull pme, irr, and tvpi data and graph
pme.tot <- list()
pme.bm <- vector()
for(i in total.cats) {
  pme <- pme.df[which(rownames(pme.df)==i),]
  irr.tvpi <- pme[ ,c("Fund IRR","Fund TVPI")]
  pme.tot[[i]] <- irr.tvpi
    if(pme$Portfolio == "OPP") {bm <- pme[ ,"Fixed 8 PME"]} else
      if(pme$Portfolio == "PD") {bm <- pme[ ,"Lev Loan+250 PME"]}
  pme.bm <- c(pme.bm,bm)
}
comb.pme <- do.call(rbind, pme.tot)
rownames(comb.pme) <- names
#comb.pme$name <- rownames(comb.pme)
pmepal=(brewer.pal(n=10,name="Spectral"))[c(1,3,8,9,10)]
max.y=max(comb.pme[ ,"Fund IRR"])   
min.y=min(comb.pme[ ,"Fund IRR"])   
yinc=(max(.05,(max.y-min.y)))/40   
max.y=-.005+(.1*(floor(max.y*10)))   
min.x=min(comb.pme[ ,"Fund TVPI"])-.05
#next line fails
pme.R2k=cut(pme.bm,c(0,.8,.95,1.05,1.2,100),labels=FALSE)
palette(pmepal)
pme.plot <- ggplot(comb.pme, aes(x=`Fund TVPI`,y=`Fund IRR`))+ 
  geom_text(label=rownames(comb.pme), size=3, colour = pme.R2k)+
  xlab("TVPI")  + ggtitle("Private Fixed Income Summary")+
  annotate("text",y = max.y, x = min.x,label ="PME is > 1.2", size=2.5, colour=pmepal[5], hjust=0)+
  annotate("text",y=max.y-yinc,x=min.x,label="PME is 1.05 to 1.2",size=2.5,colour=pmepal[4],hjust=0)+
  annotate("text",y=max.y-(yinc*2),x=min.x,label="PME is .95 to 1.05",size=2.5,colour=pmepal[3],hjust=0)+
  annotate("text",y=max.y-(yinc*3),x=min.x,label="PME is .8 to .95",size=2.5,colour=pmepal[2],hjust=0)+
  annotate("text",y=max.y-(yinc*4),x=min.x,label="PME is < .8",size=2.5,colour=pmepal[1],hjust=0)+
  theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5))
grid.arrange(priv.returns,pme.plot,ncol=2)

## @knitr real_estate_sum

#pull all categories
total.cats <- c("Total RE","Total RE Current Portfolio", "Total RE Legacy Portfolio")
names <- c("Real Estate", "Current Real Estate", "Legacy Real Estate")
#pull just return data and benchmarks
pms.returns <- list()
for(i in total.cats) {
  pme <- pme.df[which(rownames(pme.df)==i),]
  irr <- pme[ ,"Fund IRR"]
    if(pme$Portfolio == "RE") {bm <- pme[ ,"ODCE Dollar Matched IRR"]} else
      if(pme$Portfolio == "FARM") {bm <- pme[ ,"CPIxFE+350 Dollar Matched IRR"]}
  ret.df <- data.frame("IRR"=irr,"Benchmark"=bm)
  pms.returns[[i]] <- ret.df
}
sir <- do.call(rbind,pms.returns)
sir$id <- 1:length(names)
sir.long <- gather(sir, id, Return)
colnames(sir.long)[2] <- "Type"
sir.long$Return <- round(sir.long$Return/100,4)
re.returns <- ggplot(sir.long)+
  geom_bar(aes(x=id,y=Return, fill=Type),stat="identity",position= "dodge")+
  ggtitle("Since Inception IRR and Dollar \n Matched Benchmark IRR")+
  scale_x_continuous("",breaks=1:length(names),labels=names)+
  xlab("") + scale_fill_manual(values=c("darkorange","Blue"))+ylab("IRR")+
  scale_y_continuous(labels=percent)+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5))
#pull pme, irr, and tvpi data and graph
pme.tot <- list()
pme.bm <- vector()
for(i in total.cats) {
  pme <- pme.df[which(rownames(pme.df)==i),]
  irr.tvpi <- pme[ ,c("Fund IRR","Fund TVPI")]
  pme.tot[[i]] <- irr.tvpi
    if(pme$Portfolio == "RE") {bm <- pme[ ,"ODCE PME"]} else
      if(pme$Portfolio == "FARM") {bm <- pme[ ,"CPIxFE+350 PME"]}
  pme.bm <- c(pme.bm,bm)
}
comb.pme <- do.call(rbind, pme.tot)
rownames(comb.pme) <- names
#comb.pme$name <- rownames(comb.pme)
pmepal=(brewer.pal(n=10,name="Spectral"))[c(1,3,8,9,10)]
max.y=max(comb.pme[ ,"Fund IRR"])   
min.y=min(comb.pme[ ,"Fund IRR"])   
yinc=(max(.05,(max.y-min.y)))/40   
max.y = -.005+(.1*(floor(max.y*10)))   
min.x = min(comb.pme[ ,"Fund TVPI"]) - .1
#next line fails
pme.R2k=cut(pme.bm,c(0,.8,.95,1.05,1.2,100),labels=FALSE)
palette(pmepal)
re.pme.plot <- ggplot(comb.pme, aes(x=`Fund TVPI`,y=`Fund IRR`))+ 
  geom_text(label=rownames(comb.pme), size=3, colour = pme.R2k)+
  xlab("TVPI")  + ggtitle("Real Estate Summary")+
  annotate("text",y = max.y, x = min.x,label ="PME is > 1.2", size=2.5, colour=pmepal[5], hjust=0)+
  annotate("text",y=max.y-yinc,x=min.x,label="PME is 1.05 to 1.2",size=2.5,colour=pmepal[4],hjust=0)+
  annotate("text",y=max.y-(yinc*2),x=min.x,label="PME is .95 to 1.05",size=2.5,colour=pmepal[3],hjust=0)+
  annotate("text",y=max.y-(yinc*3),x=min.x,label="PME is .8 to .95",size=2.5,colour=pmepal[2],hjust=0)+
  annotate("text",y=max.y-(yinc*4),x=min.x,label="PME is < .8",size=2.5,colour=pmepal[1],hjust=0)+
  theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5))
grid.arrange(re.returns,re.pme.plot,ncol=2)
