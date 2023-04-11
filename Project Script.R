library(dplyr)
library(stringr)
library(data.table)
library(ggplot2)
library(mondate)
library(zoo)
#Anil
#df <- read.csv(file= 'C:/Users/asus/Documents/GitHub/csbtc/Bitcoin Historical Data - Investing.com (1).csv')

#Elcin
df <- read.csv(file= 'C:/Users/Acer/OneDrive - ADA University/Documents/GitHub/csbtc/Bitcoin Historical Data - Investing.com (1).csv')

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

plot(x = as.yearqtr(df_price_quarterly$quarter, format = '%Y Q%q'), y = df_price_quarterly$avg, type = 'l')
