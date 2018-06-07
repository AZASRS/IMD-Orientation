## @knitr setup_mc
require(xts)
require(PerformanceAnalytics)
require(lubridate)
require(scales)
require(RColorBrewer)
require(ggplot2)
require(reshape2)
require(knitr)
require(tidyr)
require(xtable)
source(file = "P:/IMD/Karl/R projects/Public Performance/Scripts/Functions.R")
colorpalette=brewer.pal(8, 'Set1')

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


mgrinfo <- read.csv('P:/IMD/Karl/R projects/Public Performance/Mgrinfo.csv',stringsAsFactors = F)
ac.key <- read.csv('P:/IMD/Karl/R projects/Public Performance/AssetClassKey.csv', stringsAsFactors = F)


bm <- unique(mgrinfo[which(mgrinfo$Open=='Y'), 'SSCode'])
bms<-read.csv('P:/IMD/Karl/R projects/Public Performance/BenchmarkData/Index_Returns.csv', stringsAsFactors = F)
bms$Date <- as.Date(bms$Date,format='%m/%d/%Y')
bm.data <- xts(bms[,-1], bms[,1])
bm.data=bm.data/100
ma.data=read.csv("P:/IMD/Karl/R projects/Public Performance/HistoricalData/ma.data.csv", stringsAsFactors = F)
ma.data$Date =as.Date(ma.data$Date,format="%m/%d/%Y")
mgr.data <- read.csv("P:/IMD/Karl/R projects/Public Performance/HistoricalData/AggMgrData.csv", stringsAsFactors = F)
mgr.data$Date <- as.Date(mgr.data$Date, format='%m/%d/%Y')
mgr.data$NetReturn <- mgr.data$NetReturn/100


data <- xts(ma.data[,c(2,4)], ma.data[,1])
data=data/100
name <- ac.key[which(ac.key$Asset.Class=='MA'), 'ACName']
colnames(data) <- c(name, 'Benchmark')
d <- subset(mgr.data, mgr.data$FundID == 'A1VY', select = c('Date','NetReturn'))
d.xts <- xts(d[,-1], d[,1])
s.name = mgrinfo[which(mgrinfo$FundID=='A1VY'),"FundName"]
data$Bridgewater =d.xts
data.60= data[((dim(data)[1]-(60-1)):dim(data)[1]),]
data.120= data[((dim(data)[1]-(120-1)):dim(data)[1]),]


bw.gd=gd(data.120$Bridgewater)-gd(data.120$Benchmark)


## @knitr marketvalues
funds=mgrinfo[which(mgrinfo$Asset.Class=="MA"),"FundID"]
mv.list=list()
for(f in funds){
  mv=subset(mgr.data,mgr.data$FundID==f,c("Date","MktVal"))
  name=mgrinfo[which(mgrinfo$FundID==f),"ShortName"]
  mv.xts=xts(mv[,-1],mv[,1])
  mv.list[[name]]=mv.xts
}
mvs=merge(mv.list$Bridgewater,mv.list$Windham)
mvs.60=mvs[((dim(mvs)[1]-(60-1)):dim(mvs)[1]),]
mvs.60=na.fill(mvs.60,0)
colnames(mvs.60)=c("Bridgewater","Windham")
mvs.df=data.frame("Date"=time(mvs.60),coredata(mvs.60))
mvs.long=gather(mvs.df, Portfolio, `Market Value`,-Date)
mvs.long$`Market Value`=round(mvs.long$`Market Value`/1000000000,3)
ma.mvs <- ggplot()+
  geom_bar(data=mvs.long,aes(x=Date,y=`Market Value`,fill=Portfolio),stat="identity", size=.1)+
  ggtitle("Multi Asset Class Market Values") + ylab("Billions") +
  scale_fill_manual(values = colorpalette) + xlab("")+theme(plot.title = element_text(hjust = 0.5))
ma.mvs


## @knitr ten_year_absolute
chart.CumReturns(data.120, main='Multi-Asset Class Absolute Performance',wealth.index = TRUE, legend.loc = 'topleft', colorset=colorpalette)


## @knitr summary
charts.PerformanceSummary(data.120, ylog = TRUE, wealth.index = TRUE, main = 'Multi-Asset Class Performance Summary', colorset=colorpalette)


## @knitr bw
data.120=cbind(data.120, data.120$Bridgewater-data.120$Benchmark)

bw.120.abs=ggplot(data.120, aes(x=Index, y=Bridgewater))+geom_point(colour='blue')+ 
  stat_smooth(method="loess", colour='darkorange')+
  ggtitle("Bridgewater Monthly Absolute Performance")+ylab("Return")+xlab("")+
  scale_x_date(date_labels = "%m/%Y")+ 
  theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5))

bw.120.rel=ggplot(data.120, aes(x=Index, y=Bridgewater.1))+geom_point(colour='blue')+ 
  stat_smooth(method="loess", colour='darkorange')+
  ggtitle("Bridgewater Monthly Excess Performance")+ylab("Return")+xlab("")+
  scale_x_date(date_labels = "%m/%Y")+ 
  theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5))

bw.gd.plot=ggplot(bw.gd, aes(x=Index, y=Bridgewater))+geom_point(colour='blue')+ 
  stat_smooth(method="loess", colour='darkorange')+
  ggtitle("Bridgewater Cumulative Excess Performance")+ylab("Return")+xlab("")+
  scale_x_date(date_labels = "%m/%Y")+ 
  theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5))

multiplot(bw.120.rel,bw.gd.plot)





## @knitr other
chart.RelativePerformance(data.120[,-2], data.120[ ,2], colorset = colorpalette, 
                          main = 'Cumulative Relative performance: Multi-Asset Class and Bridgewater', 
                          ylog = TRUE, legend.loc = 'topleft', ylab = 'Excess Return')
charts.RollingPerformance(data.120, width = 36, 
                          main=paste(name, 'Rolling 3 Year Performance'), 
                          colorset=colorpalette, legend.loc = 'topleft')



bw.gd.plot=ggplot(bw.gd, aes(x=Index, y=Bridgewater))+geom_point(colour='blue')+ 
  stat_smooth(method="loess", colour='darkorange')+
  ggtitle("Bridgewater Cumulative Excess Performance")+ylab("Return")+xlab("")+
  scale_x_date(date_labels = "%m/%Y")+ 
  theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5))

## Test Code

chart.Bar(data.120[ ,3], main='Bridgewater Absolute Performance',legend.loc = 'topleft')


n=120
assign(paste0('data.',n),
       data[((dim(data)[1]-(n-1)):dim(data)[1]),]
)
assign(paste0('bw.',n,'.plot'),
       ggplot(data.120, aes(x=Index, y=Bridgewater))+geom_point(colour='blue')+ 
         stat_smooth(method="loess", colour='darkorange')+
         ggtitle("Bridgewater Performance")+ylab("Return")+xlab("")+
         scale_x_date(date_labels = "%m/%Y")+ 
         theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5))
)

bw.plot=ggplot(data, aes(x=Index, y=Bridgewater))+geom_point(colour='blue')+ 
  stat_smooth(method="loess", colour='darkorange')+
  ggtitle("Bridgewater Performance")+ylab("Return")+xlab("")+
  scale_x_date(date_labels = "%m/%Y")+ 
  theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5))
