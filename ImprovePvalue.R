#ABtestdata <- read.csv('data1012.csv',header = T,encoding = 'UTF-8',sep=',')
ABtestdata <- read.csv('ABdata.txt',header = T,encoding = 'GBK',sep='\t')
names(ABtestdata)
str(ABtestdata)
ABtestdata$cal_date=as.Date(as.character(ABtestdata$cal_date))

library(dplyr)
ABtestdata=rename(ABtestdata,cal_date=X.U.FEFF.cal_date)
dcast(plum,length+time~survival,value.var = 'count')
shapiro.test(as.numeric(data2[,i]))
shapiro.test(diff_mu)
t.test(diff_mu,mu=0,alternative = "two.sided")$p.value

wilcox.test(x,y,var.equal = TRUE)$p.value
wilcox.test(diff_mu,mu=0,alternative = "two.sided")$p.value
library(reshape2)

Noexperiment='160616_fld_appxp'
ABtestdata$value[ABtestdata$experiment=='160616_fld_appxp'&ABtestdata$cal_index=='单UV收入'&ABtestdata$cal_date<'2016-10-09']

date=unique(data$cal_date)
presults=data.frame()
for (i in 1: length(date) ){
data=subset(ABtestdata,experiment=='160616_fld_appxp'&cal_index=='单UV收入'&cal_date<=date[i],select=c(cal_date,version,value))
data2=dcast(data,cal_date~version,value.var = 'value',fun.aggregate = mean)
diff_mu=as.numeric(data2[,4])-as.numeric(data2[,2])
trynext=try(t.test(diff_mu,mu=0,alternative = "two.sided")$p.value)
if ('try-error' %in% class(trynext)) next 
p=t.test(diff_mu,mu=0,alternative = "two.sided")$p.value
presult=data.frame(d=date[i],pvalue=p)
#presults=rbind(presult,presults)
print (presult)
}

t.test(data2$Control[-1],data2$`Treatment-B`[-1],alternative = "two.sided")

Pvalue_compute5=function(alpha=0.05,Noexperiment,indictor){
data=subset(ABtestdata,experiment==Noexperiment&cal_index==indictor,select=c(cal_date,version,value))
num_version=length(unique(data$version))
if (num_version<=1){
  pvalue=data.frame()
  pvalues<-rbind(pvalues,pvalue)
} else if (num_version>1) {
  data2=dcast(data,cal_date~version,value.var = 'value',fun.aggregate = mean)
  eachversion<-unique(data$version)
  exversion<-setdiff(eachversion,'Control') 
  n=dim(data2)[2]
  
  for(i in 3:n){
    diff_mu=as.numeric(data2[,i])-as.numeric(data2[,2])
    if (length(diff_mu[!is.na(diff_mu)])<=3|var(diff_mu,na.rm = T)==0){
      pvalue<-data.frame()
    }
   else if(shapiro.test(diff_mu)$p.value>alpha){#判断是否满足正态分布
      pvalue<-data.frame(d=Sys.Date()-1,Noexperiment,exversion=names(data2)[i],indictor=indictor,
                         p=t.test(diff_mu,mu=0,alternative = "two.sided")$p.value)#进行t检验时，如果数值有空则默认删除
      #pvalues<-rbind(pvalues,pvalue) 
    }
    
    else  {
      pvalue<-data.frame(d=Sys.Date()-1,Noexperiment,exversion=names(data2)[i],indictor=indictor,
                         p=wilcox.test(diff_mu,mu=0,alternative = "two.sided")$p.value ) 
      #pvalues<-rbind(pvalues,pvalue)
    }
    pvalues<-rbind(pvalues,pvalue)
  }
}
return(pvalues)

}

Pvalue_compute(Noexperiment='160616_fld_appxp',indictor='CR')
Pvalue_compute5(Noexperiment='160616_fld_appxp',indictor='单UV收入')
Pvalue_compute4(Noexperiment='160616_fld_appxp',indictor='CR')
Pvalue_compute2(Noexperiment='160420_fld_csanj',indictor='首次搜索到提交订单时长中位数(min)')

subset(ABtestdata,experiment=='160420_fld_csanj'&cal_index=='首次搜索到提交订单时长中位数(min)')

Pvalue_compute2=function(alpha=0.05,Noexperiment,indictor){
data=subset(ABtestdata,experiment==Noexperiment&cal_index==indictor,select=c(cal_date,version,value))
groupdata=data%>%group_by( experiment)%>%summarise(count=n_distinct(version))%>%filter(count>1)


#把数据从长变宽
data2=dcast(data,cal_date~version,value.var = 'value')
eachversion<-unique(data2$version)
exversion<-setdiff(eachversion,'Control')
n=dim(data2)[2]
pvalues<-data.frame()

for(i in 3:n){
  diff_mu=as.numeric(data2[,i])-as.numeric(data2[,2])
  
  if (length(diff_mu)<=3){
    pvalue<-data.frame()
  }
  if(shapiro.test(diff_mu)$p.value>alpha|length(diff_mu)>3){#判断是否满足正态分布
    pvalue<-data.frame(d=Sys.Date()-1,Noexperiment,exversion=names(data2)[i],indictor=indictor,
               p=t.test(diff_mu,mu=0,alternative = "two.sided")$p.value)#进行t检验时，如果数值有空则默认删除
    #pvalues<-rbind(pvalues,pvalue) 
  }
  
  if(shapiro.test(diff_mu)$p.value<=alpha&length(diff_mu)>3)  {
    pvalue<-data.frame(d=Sys.Date()-1,Noexperiment,exversion=names(data2)[i],indictor=indictor,
           p=wilcox.test(diff_mu,mu=0,alternative = "two.sided")$p.value ) 
    #pvalues<-rbind(pvalues,pvalue)
  }
  pvalues<-rbind(pvalues,pvalue)
}

return(pvalues)
}

160616_fld_appxp

Pvalue_compute3(Noexperiment='160616_fld_appxp',indictor='单UV收入')
Pvalue_compute4(Noexperiment='160420_fld_csanj',indictor='核对->完成UV%')
dcast(data,cal_date~version,value.var = 'value')

subset(ABtestdata,experiment=='160516_fld_fxlg'&cal_index=='搜索->完成UV%')

Pvalue_compute5(Noexperiment='160628_fld_slist',indictor='成功订单首次搜索到首次填写中位数(min)')

outline=subset(ABtestdata,experiment=='160628_fld_slist'&cal_index=='成功订单首次搜索到首次填写中位数(min)')
outline=subset(ABtestdata,experiment=='160616_fld_appxp'&cal_index=='单UV收入')
long=dcast(outline,cal_date~version,value.var = 'value',fun.aggregate = mean)#fun.aggregate = mean
is.na(long[,5])
diff_mu=long[,5]-long[,2]
var(diff_mu,na.rm = T)==0
complete.cases(diff_mu)
sum(!is.na(diff_mu))
length(diff_mu[!is.na(diff_mu)])

subset(ABtestdata,experiment=='160824_fli_carm'&cal_index=='填写->核对UV%')
接送机购买率

subset(ABtestdata,experiment=='160420_fld_szanj'&cal_index=='接送机购买率')
ABtestdata
condition=experiment_version.index[experiment_version>10]
experiments=condition.unique() 

indictors=unique(ABtestdata$cal_index)
withincontrol<-with(ABtestdata,experiment[version=='Control'])#剔除那些没有对照组的实验

experiments<-unique(withincontrol)

results<-data.frame()#1027
for (i  in 1:length(experiments) ){
  for (indictor in indictors){
    print(indictor)
    print(experiments[i])
    trynext=try(Pvalue_compute5(alpha=0.05,Noexperiment=experiments[i],indictor))
    if ('try-error' %in% class(trynext)) next  
    result<-Pvalue_compute5(alpha=0.05,Noexperiment=experiments[i],indictor)
    results<-rbind(results,result)
  }
}
#1312

groupdata=ABtestdata%>%group_by( experiment)%>%summarise(count=n_distinct(version))%>%filter(count>1)
