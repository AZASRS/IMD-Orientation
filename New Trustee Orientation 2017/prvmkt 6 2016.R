## ---- setup ----
require(zoo)
require(lubridate)
require(xtable)
load('P:/IMD/Karl/R projects/private investment performance/pmedata.rdata')
portirr=read.csv('P:/IMD/Karl/R projects/private investment performance/p2pirrs.csv')
portmat=read.csv('P:/IMD/Karl/R projects/private investment performance/portmat.csv')
tfundvals=read.csv('P:/IMD/Karl/R projects/private investment performance/tfundvals.csv')
tfundvals=zoo(tfundvals$vals,as.Date(tfundvals$dates,format='%m/%d/%Y'))

## ---- curinv ----
shortn.tot=c("Total PE","Total RE","Total PD","Total POPP","Total FARM")
shortn=gsub("Total ","",shortn.tot)
snt.ind=match(shortn.tot,rownames(pme.df))
sn.ind=match(shortn,portmat$portshort)
Portfolio=as.character(portmat$portlongname[sn.ind])
Portfolio=c(Portfolio,"Total")
Portfolio=gsub("Farmland","Farmland and Infrastructure",Portfolio)
Millions=pme.df$Cash.Adj.NAV[snt.ind]
Millions=(c(Millions,sum(Millions)))/1000000
Percent=round(100*Millions/as.numeric(tfundvals[lastcfdate]),2)
Millions=round(Millions)
Millions=format(Millions,big.mark=",",justify="right")
valuesdf=data.frame(Portfolio,Millions,Percent)
colnames(valuesdf)=c("Portfolio","$ Millions","Percent of Total Fund")
valx=xtable(valuesdf,align="llrr")
print(valx,scalebox=1,floating=FALSE,include.rownames=FALSE)

## ---- retsum -----
portirr=portirr[c(-3,-8,-9),]
irrmat=matrix(portirr$Inception,byrow=TRUE,ncol=2)
irrmat=cbind(irrmat,irrmat[,1]-irrmat[,2])
irrmat=format(irrmat,digits=2)
namemat=matrix(portirr$X,byrow=TRUE,ncol=2)
portsum=cbind(namemat[,2],irrmat)
rownames(portsum)=namemat[,1]
colnames(portsum)=c("Benchmark","Portfolio Return","Benchmark Return","Excess Return")
portsumx=xtable(portsum,align="llrrr")
print(portsumx,scalebox=.75,floating=FALSE)

## ---- curlegpe ----
shortn=c("Total PE Current Portfolio","Total PE Legacy Portfolio","Total PE") 
snt.ind=match(shortn,rownames(pme.df))
Millions=round(pme.df$Cash.Adj.NAV[snt.ind]/1000000)
Millions=format(Millions,big.mark=",",justify="right")
benchrho=pme.df$Russell.2K.Dollar.Matched.IRR[snt.ind]
portrho=pme.df$Fund.IRR[snt.ind]
excessrho=portrho-benchrho
curlegdf=data.frame(Millions,portrho,benchrho,excessrho)
colnames(curlegdf)=c("Cash Adjusted NAV","Portfolio Return","Benchmark Return","Excess Return")
rownames(curlegdf)=shortn
curlegdfx=xtable(curlegdf,align='lrrrr')
print(curlegdfx,scalebox=.75,floating=FALSE)

## ---- curlegre ----
shortn=c("Total RE Current Portfolio","Total RE Legacy Portfolio","Total RE") 
snt.ind=match(shortn,rownames(pme.df))
Millions=round(pme.df$Cash.Adj.NAV[snt.ind]/1000000)
Millions=format(Millions,big.mark=",",justify="right")
benchrho=pme.df$ODCE.Dollar.Matched.IRR[snt.ind]
portrho=pme.df$Fund.IRR[snt.ind]
excessrho=portrho-benchrho
curlegdf=data.frame(Millions,portrho,benchrho,excessrho)
colnames(curlegdf)=c("Cash Adjusted NAV","Portfolio Return","Benchmark Return","Excess Return")
rownames(curlegdf)=shortn
curlegdfx=xtable(curlegdf,align='lrrrr')
print(curlegdfx,scalebox=.75,floating=FALSE)



