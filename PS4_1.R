# Set working dir
setwd("D:/Assignment/R_Assignment04")
# Load the libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(fields)
library(maps)
library(RNetCDF)
library(ggalt)
library(RColorBrewer)

#1.1散点图  pH值与水温的散点图
#Import Data
Keeling_Data   <- read.csv("F_envwater_moni_hour-1.csv",header=T,encoding="UTF-8") 
Data           <- as_tibble(Keeling_Data)
# Check the class
class(Data)
head(Data)
tail(Data)

Pollute_pH   <-  Data          %>%
    mutate(monitor_time = as.Date(monitor_time,format='%d/%m/%Y')) %>%
    filter(pollute_name=="pH值")  %>%
    group_by(monitor_time)  %>%
    summarise(Daily_pH=mean(aver_value)) %>%
    filter(monitor_time>= as.Date("2019-04-09"))  %>%
    select(Daily_pH)
Pollute_tem   <-  Data          %>%
  mutate(monitor_time = as.Date(monitor_time,format='%d/%m/%Y')) %>%
  filter(pollute_name=="水温")  %>%
  group_by(monitor_time)  %>%
  summarise(Daily_tem=mean(aver_value)) %>%
  select(Daily_tem)

  
pollute  <- bind_cols(Pollute_pH,Pollute_tem)
Pollute_tem_select <- Pollute_tem[ Pollute_tem$Daily_tem > 14 & 
                                Pollute_tem$Daily_tem < 18 ,]


ggplot(Pollute_tem, aes(x=monitor_time, y=Daily_tem)) + 
  geom_point(aes(color=state),size=0.5) +   # draw points
  geom_smooth(method="loess", se=F) + # draw smoothing line
  geom_encircle(aes(x=monitor_time, y=Daily_tem), 
                data=Pollute_tem_select, 
                color="violet", 
                size=2, 
                expand=0.05) +
  labs(subtitle="Temperture Vs pH", 
       y="pH", 
       x="Temperture", 
       title="Scatterplot + smooth", 
       caption="Source: Shenzhen")


#时间序列图  两城市的降雨时序图
# Load data
STORM_LAKE_P_value    <-  read.csv("STORM LAKE.csv",header=T)
STORM_LAKE_P_value[is.na(STORM_LAKE_P_value)]      <- 0
SIOUX_RAPIDS_P_value  <- read.csv("SIOUX RAPIDS 4 E,.csv",header=T)
SIOUX_RAPIDS_P_value[is.na(SIOUX_RAPIDS_P_value)]  <- 0
DATA                  <- as_tibble(bind_rows(STORM_LAKE_P_value,SIOUX_RAPIDS_P_value))
Data1                  <- DATA %>%
  select(DATE,PRCP,NAME)               %>%
  mutate(date=as.Date(DATE))      %>%
  select(date,PRCP,NAME)            
# labels and breaks for X axis text
brks <- Data1$date[seq(1, length(Data1$date), 365)]
lbls <- year(brks)

ggplot(Data1, aes(x=date)) + 
  geom_line(aes(y=PRCP, col=NAME)) + 
  labs(title="Time Series of PRCP in two citys", 
       subtitle="Drawn from Long Data format", 
       caption="Source: Rainfall", 
       y="RRCP", 
       color=NULL) +  # title and caption
  scale_x_date(labels = lbls, breaks = brks) +  # change to monthly ticks and labels
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 9),  # rotate x axis text
        panel.grid.minor = element_blank())+ # turn off minor grid
  scale_color_discrete(name="NAME") +
  facet_wrap( ~ NAME,nrow=2)



#直方图
day_as1  <-  separate (Data,monitor_time,into=c("ymd","whatever"),sep = " ")
Contaminant_COD1 <- day_as1  %>%
  select(ymd,pollute_name,aver_value)  %>%
  filter(pollute_name=="化学需氧量") %>%
  group_by(ymd)  %>%
  summarize(aver_value=mean(aver_value))%>%
  mutate(pollute_name="化学需氧量")


day_as2  <- separate(Contaminant_COD1,ymd,into=c("day","month","year"),sep = "/") %>%
  mutate(monitor_time_ym=paste(month,year,sep="-"))  

Contaminant_COD <- day_as2  %>%
  select(monitor_time_ym,pollute_name,aver_value)  %>%
  filter(pollute_name=="化学需氧量") %>%
  group_by(monitor_time_ym) 
 
Contaminant_COD1 <- Contaminant_COD %>%
  filter(aver_value<=15) %>%
  mutate(pollute_name="一、二类地表水COD")
Contaminant_COD2 <- Contaminant_COD %>%
  filter(15< aver_value& aver_value<=20) %>%
  mutate(pollute_name="三类地表水COD")
Contaminant_COD3 <- Contaminant_COD %>%
  filter(20<aver_value& aver_value<=30) %>%
  mutate(pollute_name="四类地表水COD")
Contaminant_COD4 <- Contaminant_COD %>%
  filter(30<aver_value& aver_value<=40) %>%
  mutate(pollute_name="五类地表水COD")
Contaminant_COD5 <- Contaminant_COD %>%
  filter(40<aver_value) %>%
  mutate(pollute_name="不达标")


Contaminant_COD  <- as_tibble(bind_rows(Contaminant_COD1,Contaminant_COD2,Contaminant_COD3,
                                        Contaminant_COD4,Contaminant_COD5))  %>%
                   mutate(aver_value=1)

ggplot(Contaminant_COD,aes(x=monitor_time_ym,y=aver_value,fill=pollute_name),stat=count)+
   geom_bar(stat="identity",colour="black")+
   theme( panel.grid.major=element_blank(),panel.grid.minor=element_blank())


#箱线图
day_as1  <-  separate (Data,monitor_time,into=c("ymd","whatever"),sep = " ")
Contaminant_COD1 <- day_as1  %>%
  select(ymd,pollute_name,aver_value)  %>%
  filter(pollute_name=="化学需氧量") %>%
  group_by(ymd)  %>%
  summarize(aver_value=mean(aver_value))%>%
  mutate(pollute_name="化学需氧量")


day_as2  <- separate(Contaminant_COD1,ymd,into=c("day","month","year"),sep = "/") %>%
              mutate(monitor_time_ym=paste(month,year,sep="-"))  

Contaminant_COD <- day_as2  %>%
  select(monitor_time_ym,pollute_name,aver_value)  %>%
  filter(pollute_name=="化学需氧量") %>%
  group_by(monitor_time_ym) 


ggplot(Contaminant_COD,aes(x=monitor_time_ym,y=aver_value,fill=monitor_time_ym))+ #”fill=“设置填充颜色
  stat_boxplot(geom = "errorbar",width=0.4,aes(color="black"))+ #由于自带的箱形图没有胡须末端没有短横线，使用误差条的方式补上
  geom_boxplot(size=0.5,fill="white",outlier.fill="white",outlier.color="white")+ #size设置箱线图的边框线和胡须的线宽度，fill设置填充颜色，outlier.fill和outlier.color设置异常点的属性
  geom_jitter(aes(fill=monitor_time_ym),width =0.2,shape = 21,size=1.5)+ #设置为向水平方向抖动的散点图，width指定了向水平方向抖动，不改变纵轴的值
  scale_fill_manual(values = c("red", "blue","yellow","pink","grey","green","aquamarine1",
                               "violet","orange","black","grey50","aliceblue","cyan"))+  #设置填充的颜色
  scale_color_manual(values=c("black","black","black"))+ #设置散点图的圆圈的颜色为黑色
  ggtitle("Box-Plot")+ #设置总的标题
  theme_bw()+ #背景变为白色
  ylim(c(0,60))+
  theme(legend.position="none", #不需要图例
        plot.title = element_text(size=15,face="bold",hjust = 0.5), #设置总标题的字体属性
        panel.grid.major = element_blank(), #不显示网格线
        panel.grid.minor = element_blank())+
  ylab("COD")+xlab("Date") #设置x轴和y轴的标题


#图像图
#加载包，用于画出绿色渐变色
library(RColorBrewer)
# Open the NetCDF file
ex.nc     <- open.nc("Monthly_NDVI.20191201.120E_30N_122E_32N.nc")

# Print the variables and attributes
print.nc(ex.nc)
# Read the variables
# Lat
Lat       <- var.get.nc(ex.nc, "lat") #纬度
# Lon
Lon       <- var.get.nc(ex.nc, "lon") #精度
# Monthly long term mean, surface temperature [degC]
NDVI     <- var.get.nc(ex.nc, "MOD13C2_006_CMG_0_05_Deg_Monthly_NDVI") 

# Close the NetCDF file
close.nc(ex.nc)

# Original Lat is in increasing order, we don't need to reverse it

# Plot
image.plot(Lon, Lat, NDVI,
           col=brewer.pal(8,"Greens"),
           horizontal=T, useRaster=T,
           legend.shrink=0.75, axis.args=list(cex.axis = 1.25), 
           legend.width=1, legend.mar=2,
           legend.args=list(text="NDVI ",cex=1.25), #长江入海口陆地植被覆盖率          
           xlab='',ylab='',midpoint=T, axes=F, ann=F
)
title(xlab="",cex.lab=1.25,font.lab=2)
axis(1,at=pretty(Lon),tck=-0.015,lwd=2,cex.axis=1.25,font=1)
title(ylab="",cex.lab=1.25,font.lab=2)
axis(2,at=pretty(Lat),tck=-0.015,lwd=2,cex.axis=1.25,font=1,las=1)
title(main=paste("Coverage rate of surface vegetation（NDVI) at the mouth of the Yangtze River in December."),
      cex.main=1,font.main=3)

# Add map
map('world',add=T,lwd=0.75,col="black")

# Add a box
box(lwd=2)

row(NDVI)