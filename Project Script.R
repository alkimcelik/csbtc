library(dplyr)
library(stringr)
library(data.table)
library(ggplot2)
library(mondate)
library(zoo)
library(VGAM)
library(MASS)
library(car)
library(forecast)
library(conflicted)
library(tidyr)
library(scales)
conflict_prefer('select', 'dplyr')
conflict_prefer('filter', 'dplyr')
change_func <- function(col1, col2){
  change <- ((col1 - col2)/col2)*100
  return(change)
}
my_theme = theme(panel.grid = element_line(color = '#e6e6e6'),
                 panel.background = element_rect(fill = 'white'),
                 plot.title = element_text(hjust = .5, size = 28, colour = '#ffa500'),
                 text = element_text(family = 'Georgia'),
                 axis.text = element_text(size = 10),
                 axis.title = element_text(size = 18, family = 'Georgia', face = 'bold'),
                 axis.line = element_line(colour = '#737373', size = 1),
                 strip.background = element_rect(colour = "black", fill = "white"),
                 strip.text = element_text(face = 'bold'))  
#to resolve the local time issue:
#Sys.setlocale("LC_TIME", "C")
#Anil
#df <- read.csv(file= 'C:/Users/asus/Documents/GitHub/csbtc/Bitcoin Historical Data - Investing.com (1).csv')

#Elcin
#df <- read.csv(file= 'C:/Users/Acer/OneDrive - ADA University/Documents/GitHub/csbtc/Bitcoin Historical Data - Investing.com (1).csv')

#Alkim
df <- read.csv(file= 'C:/Users/alkim/OneDrive/Documents/GitHub/csbtc/Bitcoin Historical Data - Investing.com (1).csv')


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
df$transformed_price <- box
df_price_quarterly <- df %>% group_by(quarter) %>% summarise(price = mean(Price))
df_price_quarterly$lag <- lag(df_price_quarterly$price)
df_price_quarterly$change <- change_func(df_price_quarterly$price, df_price_quarterly$lag)
df_price_quarterly = df_price_quarterly[-c(52),] #last quarter not finished
df_price_quarterly$lag <- na.fill(df_price_quarterly$lag, 0)
df_price_quarterly$change <- na.fill(df_price_quarterly$change, 0)

plot(x = as.yearqtr(df_price_quarterly$quarter, format = '%Y Q%q'), y = df_price_quarterly$price, type = 'l')

#######PLOTTING#######
plot.new()
#Bitcoin price
ggplot(df, aes(x = Date, y = Price)) +
  geom_line(color = 'darkgreen') + theme_minimal() + ylab('Price ($)') +
  title('Bitcoin Daily Price between July 2010 to April 2023')

#Bitcoin price starting 2017
ggplot(df %>% filter(Date >= '2017-01-01'), aes(x = Date, y = Price)) +
  geom_line(color = 'darkgreen') + theme_minimal() + ylab('Price ($)') +
  title('Bitcoin Daily Price between January 2017 to April 2023')

#Time series decomposition
price_ts <- ts(df[order(df$Date),]$Price, start = c(2010,7), frequency = 365)
decomp <- decompose(price_ts, type = 'multiplicative')
plot(decomp)

#Other factors plots
#CPI
ggplot(melt(data.table(df_drivers %>% select(sasdate,CPIAUCSL,CPILFESL)), id.vars = c('sasdate')), aes(x=sasdate)) + 
  geom_line(aes(y = value, color = variable), linewidth = 0.8) + 
  theme_minimal() + ylab('CPI') + xlab('Quarters') +
  ggtitle('Quarterly CPI Values between 2010 Q3 and 2023 Q1') 

#Federal Funds Rate
ggplot(df_drivers, aes(x=sasdate)) + 
  geom_line(aes(y = FEDFUNDS/100),color = '#D55E00', linewidth = 0.8) + 
  theme_minimal() + ylab('Federal Funds Rate') + xlab('Quarters') +
  scale_y_continuous(labels = percent_format()) +
  scale_x_date(breaks = as.Date(c("2010-06-01", "2014-03-01", '2018-09-01', '2022-12-01')), 
               labels = c('2010 Q3', '2014 Q2', '2018 Q4', '2023 Q1')) +
  ggtitle('Quarterly Federal Funds Rates between 2010 Q3 and 2023 Q1') 



#Lag correlation
bit_ts = df %>%
  filter(Date > as.Date('2017-01-01')) %>%
  arrange(Date) %>%
  dplyr::select(Price) %>%
  as.matrix() %>%
  ts()

gglagplot(bit_ts, do.lines = F) + my_theme +
  scale_color_continuous(low = "#b37400", high = "#ffc04d", breaks = c(1, 366, 731), labels = c('2017', '2018', '2019')) + 
  scale_y_continuous(breaks = c(0, 20000, 40000, 60000), 
                     labels = c('$0', '$20,000', '$40,000', '$60,000')) +
  scale_x_continuous(breaks = c(20000, 40000, 60000), 
                     labels = c('$20,000', '$40,000', '$60,000'))
  

#########Potential Drivers########
#Anil
#df_drivers <- read.csv(file= 'C:/Users/asus/Documents/GitHub/csbtc/current.csv')
#Elcin
#df_drivers <- read.csv('C:/Users/Acer/OneDrive - ADA University/Documents/GitHub/csbtc/current.csv')

#Alkim
df_drivers <- read.csv('C:/Users/alkim/OneDrive/Documents/GitHub/csbtc/current.csv')

df_drivers <- df_drivers %>% select(c('sasdate','UNRATE','CPIAUCSL', 'CPILFESL', 'FEDFUNDS', 
                      'S.P.500', 'S.P..indust', 'S.P.div.yield',
                      'S.P.PE.ratio')) 
df_drivers = df_drivers[-c(1,2),]
df_drivers$sasdate <- as.Date(df_drivers$sasdate, format = "%m/%d/%Y") 
df_drivers <- df_drivers %>% filter (df_drivers$sasdate >= "2010-06-01")
df_drivers$lag_unrate <- lag(df_drivers$UNRATE)
df_drivers$unrate_change <- change_func(df_drivers$UNRATE, df_drivers$lag_unrate)
df_drivers <- cbind(df_drivers,df_price_quarterly$quarter)


combined_df <- cbind(df_price_quarterly,df_drivers[,-c(1,ncol(df_drivers))])
combined_df$normalized_price <- (combined_df$normalized_price - min(combined_df$normalized_price)) / (max(combined_df$normalized_price) - min(combined_df$normalized_price))
ggplot(combined_df, aes(x=as.yearqtr(combined_df$quarter, format = '%Y Q%q'))) + 
  geom_line(aes(y = log(combined_df$price)), color = "darkred") + 
  geom_line(aes(y = combined_df$UNRATE), color="steelblue", linetype="twodash") 
