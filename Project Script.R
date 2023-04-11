library(dplyr)
library(stringr)
library(data.table)
library(ggplot2)
library(mondate)
library(zoo)
#to resolve the local time issue:
#Sys.setlocale("LC_TIME", "C")
#Anil
df <- read.csv(file= 'C:/Users/asus/Documents/GitHub/csbtc/Bitcoin Historical Data - Investing.com (1).csv')

#Elcin
#df <- read.csv(file= 'C:/Users/Acer/OneDrive - ADA University/Documents/GitHub/csbtc/Bitcoin Historical Data - Investing.com (1).csv')

#########Data Manipulation########
names(df)[names(df) == 'Vol.'] <- "Vol"
names(df)[names(df) == 'Change..'] <- "Change"


df$Price <- as.numeric(sub(",", "", df$Price, fixed=TRUE))
df$Open <- as.numeric(sub(",", "", df$Open, fixed=TRUE))
df$High <- as.numeric(sub(",", "", df$High, fixed=TRUE))
df$Low <- as.numeric(sub(",", "", df$Low, fixed=TRUE))


df$contains_str <- ifelse(grepl("K", df$Vol), "K", 
                          ifelse(grepl('M', df$Vol),'M',
                                 ifelse(grepl('B', df$Vol),'B',"")))

rep_str = c('K'='','B'='','M'='')
df$Vol <- str_replace_all(df$Vol, rep_str)
df$Vol <- as.numeric(df$Vol)
df$Date <- as.Date(df$Date, format = "%b %d, %Y")


df$Vol <- ifelse(df$contains_str == 'K',df$Vol*1000,
       ifelse(df$contains_str == 'M', df$Vol*1000000, 
              ifelse(df$contains_str == 'B', df$Vol*1000000000,df$Vol)))
df$contains_str <- NULL

df$Change <- as.numeric(sub("%", "", df$Change, fixed=TRUE))/100

df$quarter <- paste(year(df$Date),quarters(df$Date))
df_price_quarterly <- df %>% group_by(quarter) %>% summarise(avg = mean(Price))
growth <- integer(50)
for (i in nrow(df_price_quarterly)) {
  if (i==1) { 
    growth[i]=0
  }
  else {
    growth[i]=((df_price_quarterly[i,2]-df_price_quarterly[i-1,2])/df_price_quarterly[i-1,2])*100
  }
}
growth <- data.frame(growth)
growth <- t(growth)
df_price_quarterly = df_price_quarterly[-c(52),]

plot(x = as.yearqtr(df_price_quarterly$quarter, format = '%Y Q%q'), y = df_price_quarterly$avg, type = 'l')

#########Potential Drivers########
#Anil
df_drivers <- read.csv(file= 'C:/Users/asus/Documents/GitHub/csbtc/current.csv')
#Elcin
#df_drivers <- read.csv('C:/Users/Acer/OneDrive - ADA University/Documents/GitHub/csbtc/current.csv')

df_drivers <- df_drivers %>% select(c('sasdate','UNRATE','CPIAUCSL', 'CPILFESL', 'FEDFUNDS', 
                      'S.P.500', 'S.P..indust', 'S.P.div.yield',
                      'S.P.PE.ratio')) 
df_drivers = df_drivers[-c(1,2),]
df_drivers$sasdate <- as.Date(df_drivers$sasdate, format = "%m/%d/%Y") 
df_drivers <- df_drivers %>% filter (df_drivers$sasdate >= "2010-06-01")
df_drivers <- cbind(df_drivers,df_price_quarterly$quarter)

combined_df <- cbind(df_price_quarterly,df_drivers)
ggplot(combined_df, aes(x=as.yearqtr(combined_df$quarter, format = '%Y Q%q'))) + 
  geom_line(aes(y = combined_df$avg), color = "darkred") + 
  geom_line(aes(y = combined_df$UNRATE), color="steelblue", linetype="twodash") 
