## London Jet

##Library used for RFM Analysis
library(dplyr)
library(rfm)
library(lubridate)
library (ggplot2)
library(gridExtra)

##Import the complete data set to r
London_Jet<-read.csv("clipboard",sep = "\t", header = TRUE)

##Data pre-processing
jetdata<-London_Jet
str(jetdata)
colSums(is.na(jetdata))

##Add transition day field
jetdata$LastTransDay<- 1

##Convert filed to character
jetdata$LastTransYear<-as.character(jetdata$LastTransYear)
jetdata$LastTransMonth<-as.character(jetdata$LastTransMonth)
jetdata$LastTransDay<-as.character(jetdata$LastTransDay)

##Combine LastTransYear, LastTransMonth and LastTransDay
jetdata$Date<-paste(jetdata$LastTransYear,jetdata$LastTransMonth,jetdata$LastTransDay)

##Convert date to date format
jetdata$Date<-as.Date(jetdata$Date,format = "%Y%m%d")

##Remove the field which are not required
jetdata1<-jetdata[-c(2:8,10,12:14,16:21)]
summary(jetdata1)

## Define the analysis date
analysis_date<-max(jetdata1$Date)
analysis_date

## Calculate the Recency, Frequency and Monetary value for each customer
rfm_jetdata<- jetdata1 %>% group_by(CustID) %>% summarise(Recency= as.numeric(analysis_date- max(Date)), 
                                                     frequency= Num_Games,Monetary=sum(Tot_Sales))

## Plot to understand the distribution of Recencey, Frequency and Monetary
r<-ggplot(rfm_jetdata) + geom_density(aes(x=Recency))
f<-ggplot(rfm_jetdata) + geom_density(aes(x=frequency))
m<-ggplot(rfm_jetdata) + geom_density (aes(x=Monetary))
grid.arrange(r,f,m, nrow=3)

##Calculate RFM Score
summary(rfm_jetdata)
##Calculate recency score
rfm_jetdata$R_Score<-0
rfm_jetdata$R_Score[rfm_jetdata$Recency>=822]<-1
rfm_jetdata$R_Score[rfm_jetdata$Recency>=550 & rfm_jetdata$Recency<822]<-2
rfm_jetdata$R_Score[rfm_jetdata$Recency>=456 & rfm_jetdata$Recency<550]<-3
rfm_jetdata$R_Score[rfm_jetdata$Recency>=214 & rfm_jetdata$Recency<456]<-4
rfm_jetdata$R_Score[rfm_jetdata$Recency<214]<-5

##Calculate Frequency Score
rfm_jetdata$F_Score<-0
rfm_jetdata$F_Score[rfm_jetdata$frequency>=8]<-5
rfm_jetdata$F_Score[rfm_jetdata$frequency>=5 & rfm_jetdata$frequency<8]<-4
rfm_jetdata$F_Score[rfm_jetdata$frequency>=3 & rfm_jetdata$frequency<5]<-3
rfm_jetdata$F_Score[rfm_jetdata$frequency>=2 & rfm_jetdata$frequency<3]<-2
rfm_jetdata$F_Score[rfm_jetdata$frequency<=1]<-1

##Calculate Monetary Score
rfm_jetdata$M_Score<-0
rfm_jetdata$M_Score[rfm_jetdata$Monetary>=2000]<-5
rfm_jetdata$M_Score[rfm_jetdata$Monetary>=660 & rfm_jetdata$Monetary<2000 ]<-4
rfm_jetdata$M_Score[rfm_jetdata$Monetary>=476 & rfm_jetdata$Monetary<660 ]<-3
rfm_jetdata$M_Score[rfm_jetdata$Monetary>=190 & rfm_jetdata$Monetary<476 ]<-2
rfm_jetdata$M_Score[rfm_jetdata$Monetary<190]<-1

##Combine the R-Score, F_Score and M_Score
rfm_jetdata<-rfm_jetdata%>% mutate(RFM_Score=R_Score*100+F_Score*10+M_Score)

##Create the segments based on the RFM score
rfm_jetdata$Segement<-"Others"
rfm_jetdata$Segement[which(rfm_jetdata$RFM_Score %in% c(555,545,455,445,454,444,544,554))]<-"Premium Customer"
rfm_jetdata$Segement[which(rfm_jetdata$RFM_Score %in% c(433,434,443,344,333,534,343,543,334,354,335,353,345,553,453,533))]<-"Top Customers"
rfm_jetdata$Segement[which(rfm_jetdata$RFM_Score %in% c(322,332,323,233,232,223,222,432,522,244,452,523,242,234,424,532,532,442
                                                        ,342,243,542,422,442,243,524,254,324,253,324,252,224,423,245,552,255))]<-"Likley to be Top Customers"
rfm_jetdata$Segement[which(rfm_jetdata$RFM_Score %in% c(134,143,144,142,145,124,135,123,122,133,132,153,154,155,152,125))]<-"Customer not recent"
rfm_jetdata$Segement[which(rfm_jetdata$RFM_Score %in% c(311,511,412,512,312,411,313,413,513))]<-"Customer not frequent"

##Calculate the percentage of customer in each segment and plot the same
divison<-rfm_jetdata%>%count(Segement)%>%mutate(Percentage=(n/3000)*100)
ggplot(data=divison, aes(x=Segement,y=n,fill=Segement))+geom_bar(stat = "Identity")

##Merge the rfm dataset to the original data set based on the customer ID
?merge
jetdatafinal<-merge(jetdata,rfm_jetdata,by="CustID")

## Export the final merged data with segementation in .csv format
write.csv(jetdatafinal, file = "C:\\Users\\Sunil.Samuel\\Desktop\\Personal\\Training\\Data Analytics for business statergy\\R\\Capstone Project\\London Jet\\Customer Segmentation.csv")
