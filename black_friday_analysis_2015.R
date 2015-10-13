####################################################################################
#directly connect to MYSQL
library(RODBC)
library(timeDate)
library(lubridate)
library(plyr)

#Extract sales information from dealer track
#Filters that are applied: year 2010-2014, new sale, retail sale, and pricing ratio in [0.7,1.2]
conn<-odbcConnect('mysql1','***','***') 
t_nov_dec<-sqlQuery(conn,"select trans_date, actual_msrp, vehicle_price_net_rebate, new_used from fct_alg_full where vehicle_price_net_rebate<>'NULL' and actual_msrp<>'NULL'
                    and trans_date>='2010-01-01' and trans_date<='2014-12-31' and new_used in ('N','U') and sale_type in ('R')",stringsAsFactors=FALSE)
save(t_nov_dec,file='C:\\black Friday ticket\\year_2015_data.RData')
t_nov_dec$sale_over_msrp<-t_nov_dec$vehicle_price_net_rebate/t_nov_dec$actual_msrp
t_nov_dec<-t_nov_dec[t_nov_dec$sale_over_msrp>0.7 & t_nov_dec$sale_over_msrp<1.2,]

#questions to answer
#·         Are there notable savings on Black Friday or are they similar to the savings experienced at the last few days of any month?
#·         Has the discount increased in the last few years on Black Friday?
#·         Are there large discounts also available also on the Saturday following Black Friday or cyber Monday?
#only focus on the last 7 days for every month
end_ndays=7
t_nov_dec$end_of_month<-as.numeric(with(t_nov_dec,((month(trans_date)%in%c(1,3,5,7,8,10,12))&(day(trans_date)>=(32-end_ndays))|
                                                     ((month(trans_date)%in%c(4,6,9,11))&(day(trans_date)>=(31-end_ndays)))|
                                                     ((year(trans_date)!=2012)&(month(trans_date)==2)&(day(trans_date)>=(29-end_ndays)))|
                                                     ((year(trans_date)==2012)&(month(trans_date)==2)&(day(trans_date)>=(30-end_ndays))))))
t_nov_dec$month<-month(t_nov_dec$trans_date)
t_nov_dec$year<-year(t_nov_dec$trans_date)
thxgiving_dates<-as.Date(c('2010-11-26','2011-11-25','2012-11-23','2013-11-29','2014-11-28'))
t_nov_dec$thxgiving<-as.numeric(t_nov_dec$trans_date%in%thxgiving_dates)
t_nov_dec$blkfriday<-as.numeric(t_nov_dec$trans_date%in%(thxgiving_dates+1))
t_nov_dec$folsatday<-as.numeric(t_nov_dec$trans_date%in%(thxgiving_dates+2))
t_nov_dec$cybmonday<-as.numeric(t_nov_dec$trans_date%in%(thxgiving_dates+4))


#find out mean discount rate in Black Friday, following Saturday, and cyber Monday in last few years
t_blk<-ddply(t_nov_dec[t_nov_dec$blkfriday==1,],.(year),summarize,
             mean_msrp=mean(actual_msrp),mean_sale_price=mean(vehicle_price_net_rebate),
             mean_sale_ratio=mean(sale_over_msrp),num_sales=length(actual_msrp))
t_blk$sale_ratio_mean<-with(t_blk,mean_sale_price/mean_msrp)

t_fst<-ddply(t_nov_dec[t_nov_dec$folsatday==1,],.(year),summarize,
             mean_msrp=mean(actual_msrp),mean_sale_price=mean(vehicle_price_net_rebate),
             mean_sale_ratio=mean(sale_over_msrp),num_sales=length(actual_msrp))
t_fst$sale_ratio_mean<-with(t_fst,mean_sale_price/mean_msrp)

t_cmd<-ddply(t_nov_dec[t_nov_dec$cybmonday==1,],.(year),summarize,
             mean_msrp=mean(actual_msrp),mean_sale_price=mean(vehicle_price_net_rebate),
             mean_sale_ratio=mean(sale_over_msrp),num_sales=length(actual_msrp))
t_cmd$sale_ratio_mean<-with(t_cmd,mean_sale_price/mean_msrp)

t_last<-ddply(t_nov_dec[t_nov_dec$end_of_month==1,],.(year),summarize,
              mean_msrp=mean(actual_msrp),mean_sale_price=mean(vehicle_price_net_rebate),
              mean_sale_ratio=mean(sale_over_msrp),num_sales=length(actual_msrp))
t_last$sale_ratio_mean<-with(t_last,mean_sale_price/mean_msrp)

#build daily mean discount rate for years 2010, 2011, 2012, 2013, and 2014
col_names<-names(t_2010)
t_2010<-ddply(t_nov_dec[t_nov_dec$year==2010,],.(trans_date),summarize,num_sales=length(sale_over_msrp),
              num_sales_discount=sum(sale_over_msrp<1),mean_sale_ratio=mean(sale_over_msrp),
              mean_sales_discount_dissale=mean(sale_over_msrp[sale_over_msrp<1]),
              mean_msrp=mean(actual_msrp),mean_sale_price=mean(vehicle_price_net_rebate))
t_2010<-t_2010[with(t_2010,order(mean_sale_ratio,decreasing=FALSE)),]
t_2010$blk<-with(t_2010,trans_date%in%c(thxgiving_dates+1))
t_2010$fst<-with(t_2010,trans_date%in%c(thxgiving_dates+2))
t_2010$cmn<-with(t_2010,trans_date%in%c(thxgiving_dates+4))
t_2010$sales_have_dis<-with(t_2010,num_sales_discount/num_sales)
t_2010<-t_2010[,col_names]

t_2011<-ddply(t_nov_dec[t_nov_dec$year==2011,],.(trans_date),summarize,num_sales=length(sale_over_msrp),
              num_sales_discount=sum(sale_over_msrp<1),mean_sale_ratio=mean(sale_over_msrp),
              mean_sales_discount_dissale=mean(sale_over_msrp[sale_over_msrp<1]),
              mean_msrp=mean(actual_msrp),mean_sale_price=mean(vehicle_price_net_rebate))
t_2011<-t_2011[with(t_2011,order(mean_sale_ratio,decreasing=FALSE)),]
t_2011$blk<-with(t_2011,trans_date%in%c(thxgiving_dates+1))
t_2011$fst<-with(t_2011,trans_date%in%c(thxgiving_dates+2))
t_2011$cmn<-with(t_2011,trans_date%in%c(thxgiving_dates+4))
t_2011$sales_have_dis<-with(t_2011,num_sales_discount/num_sales)
t_2011<-t_2011[,col_names]

t_2012<-ddply(t_nov_dec[t_nov_dec$year==2012,],.(trans_date),summarize,num_sales=length(sale_over_msrp),
              mean_sales_discount_dissale=mean(sale_over_msrp[sale_over_msrp<1]),
              num_sales_discount=sum(sale_over_msrp<1),mean_sale_ratio=mean(sale_over_msrp),
              mean_msrp=mean(actual_msrp),mean_sale_price=mean(vehicle_price_net_rebate))
t_2012<-t_2012[with(t_2012,order(mean_sale_ratio,decreasing=FALSE)),]
t_2012$blk<-with(t_2012,trans_date%in%c(thxgiving_dates+1))
t_2012$fst<-with(t_2012,trans_date%in%c(thxgiving_dates+2))
t_2012$cmn<-with(t_2012,trans_date%in%c(thxgiving_dates+4))
t_2012$sales_have_dis<-with(t_2012,num_sales_discount/num_sales)
t_2012<-t_2012[,col_names]

t_2013<-ddply(t_nov_dec[t_nov_dec$year==2013,],.(trans_date),summarize,num_sales=length(sale_over_msrp),
              mean_sales_discount_dissale=mean(sale_over_msrp[sale_over_msrp<1]),
              num_sales_discount=sum(sale_over_msrp<1),mean_sale_ratio=mean(sale_over_msrp),
              mean_msrp=mean(actual_msrp),mean_sale_price=mean(vehicle_price_net_rebate))
t_2013<-t_2013[with(t_2013,order(mean_sale_ratio,decreasing=FALSE)),]
t_2013$blk<-with(t_2013,trans_date%in%c(thxgiving_dates+1))
t_2013$fst<-with(t_2013,trans_date%in%c(thxgiving_dates+2))
t_2013$cmn<-with(t_2013,trans_date%in%c(thxgiving_dates+4))
t_2013$sales_have_dis<-with(t_2013,num_sales_discount/num_sales)
t_2013<-t_2013[,col_names]

t_2014<-ddply(t_nov_dec[t_nov_dec$year==2014,],.(trans_date),summarize,num_sales=length(sale_over_msrp),
              mean_sales_discount_dissale=mean(sale_over_msrp[sale_over_msrp<1]),
              num_sales_discount=sum(sale_over_msrp<1),mean_sale_ratio=mean(sale_over_msrp),
              mean_msrp=mean(actual_msrp),mean_sale_price=mean(vehicle_price_net_rebate))
t_2014<-t_2014[with(t_2014,order(mean_sale_ratio,decreasing=FALSE)),]
t_2014$blk<-with(t_2014,trans_date%in%c(thxgiving_dates+1))
t_2014$fst<-with(t_2014,trans_date%in%c(thxgiving_dates+2))
t_2014$cmn<-with(t_2014,trans_date%in%c(thxgiving_dates+4))
t_2014$sales_have_dis<-with(t_2014,num_sales_discount/num_sales)
t_2014<-t_2014[,col_names]

#save raw data as csv files
write.csv(t_2010,'C:\\black Friday ticket\\t_2010.csv',quote=FALSE,row.names=FALSE)
write.csv(t_2011,'C:\\black Friday ticket\\t_2011.csv',quote=FALSE,row.names=FALSE)
write.csv(t_2012,'C:\\black Friday ticket\\t_2012.csv',quote=FALSE,row.names=FALSE)
write.csv(t_2013,'C:\\black Friday ticket\\t_2013.csv',quote=FALSE,row.names=FALSE)
write.csv(t_2014,'C:\\black Friday ticket\\t_2014.csv',quote=FALSE,row.names=FALSE)
