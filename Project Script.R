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
library(reshape2)
library(psych)

conflict_prefer('select', 'dplyr')
conflict_prefer('filter', 'dplyr')
conflict_prefer('lag', 'dplyr')
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
df_drivers <- read.csv('C:/Users/alkim/OneDrive/Documents/GitHub/csbtc/current.csv')


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

df$quarter <- paste(mondate::year(df$Date),quarters(df$Date))
df_price_quarterly <- df %>% group_by(quarter) %>% summarise(price = mean(Price))
df_price_quarterly$lag <- lag(df_price_quarterly$price)
df_price_quarterly$change <- change_func(df_price_quarterly$price, df_price_quarterly$lag)
df_price_quarterly = df_price_quarterly[-c(52),] #last quarter not finished
df_price_quarterly$lag <- na.fill(df_price_quarterly$lag, 0)
df_price_quarterly$change <- na.fill(df_price_quarterly$change, 0)


df_drivers <- df_drivers %>% select(c('sasdate','UNRATE','CPIAUCSL', 'CPILFESL', 'FEDFUNDS', 
                                      'S.P.500', 'S.P..indust', 'S.P.div.yield',
                                      'S.P.PE.ratio')) 
colnames(df_drivers) <- c('Date', 'Unemp_Rate', 'CPIAUCSL', 'CPILFESL', 'Fed_Funds',
                          'SP_500', 'SP_Industrial', 'SP_Dividend_Yield', 'SP_PE_Ratio')
df_drivers = df_drivers[-c(1,2),]
df_drivers$Date <- as.Date(df_drivers$Date, format = "%m/%d/%Y") 
df_drivers <- df_drivers %>% filter (df_drivers$Date >= "2010-06-01")
df_drivers$lag_unrate <- lag(df_drivers$Unemp_Rate)
df_drivers$unrate_change <- change_func(df_drivers$Unemp_Rate, df_drivers$lag_unrate)
df_drivers$SP_500_lag <- lag(df_drivers$SP_500)
df_drivers$SP_500_change <- change_func(df_drivers$SP_500, df_drivers$SP_500_lag)
df_drivers$CPIAUCSL_lag <- lag(df_drivers$CPIAUCSL)
df_drivers$CPIAUCSL_change <- change_func(df_drivers$CPIAUCSL, df_drivers$CPIAUCSL_lag)
df_drivers$CPILFESL_lag <- lag(df_drivers$CPILFESL)
df_drivers$CPILFESL_change <- change_func(df_drivers$CPILFESL, df_drivers$CPILFESL_lag)
df_drivers <- cbind(df_drivers,df_price_quarterly$quarter)



combined_df <- cbind(df_price_quarterly,df_drivers[,-c(1,ncol(df_drivers))])
combined_df$normalized_price <- (combined_df$normalized_price - min(combined_df$normalized_price)) / (max(combined_df$normalized_price) - min(combined_df$normalized_price))


plot(x = as.yearqtr(df_price_quarterly$quarter, format = '%Y Q%q'), y = df_price_quarterly$price, type = 'l')

#######PLOTTING#######
plot.new()
#Bitcoin price
ggplot(df, aes(x = Date, y = Price)) +
  geom_line(color = 'darkgreen') +  ylab('Price ($)') +
  ggtitle('Bitcoin Daily Price between July 2010 to April 2023') +
  theme(axis.text.x = element_text(size = 12)) 

#Bitcoin price starting 2017
ggplot(df %>% filter(Date >= '2017-01-01'), aes(x = Date, y = Price)) +
  geom_line(color = 'darkgreen') + ylab('Price ($)') +
  ggtitle('Bitcoin Daily Price between January 2017 to April 2023')

#Bitcoin Growth Rate
ggplot(combined_df, aes(x = as.yearqtr(combined_df$quarter, format = '%Y Q%q'))) +
  geom_line(aes(y = change/100),color = 'darkgreen', linewidth = 1) + 
  geom_point(aes(y = change/100),color = 'darkgreen') +
  ylab('Growth Rate') + xlab('Quarters') +
  ggtitle('Bitcoin Quarterly Growth Rate between 2010 Q3 to 2023 Q1') +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_yearqtr(breaks = seq(min(as.yearqtr(combined_df$quarter)), 
                               max(as.yearqtr(combined_df$quarter)), 
                               by = 1),
                  labels = function(x) format(x, "%Y Q%q"),
                  expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

combined_df_2017 <- combined_df %>% filter(as.yearqtr(df_price_quarterly$quarter, format = '%Y Q%q') >= '2017 Q1')

ggplot(combined_df_2017, aes(x = as.yearqtr(combined_df_2017$quarter, format = '%Y Q%q'))) +
  geom_line(aes(y = change/100),color = 'darkgreen', linewidth = 1) + 
  geom_point(aes(y = change/100),color = 'darkgreen') +
  ylab('Growth Rate') + xlab('Quarters') +
  ggtitle('Bitcoin Quarterly Growth Rate between 2017 Q1 to 2023 Q1') +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_yearqtr(breaks = seq(min(as.yearqtr(combined_df_2017$quarter)), 
                               max(as.yearqtr(combined_df_2017$quarter)), 
                               by = 1/4),
                  labels = function(x) format(x, "%Y Q%q"),
                  expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

#Time series decomposition - Bitcoin
price_ts <- ts(df[order(df$Date),]$Price, start = c(2010,7), frequency = 365)
decomp <- decompose(price_ts, type = 'multiplicative')

df_decomp <- data.frame(
  x = time(price_ts),
  trend = decomp$trend,
  seasonal = decomp$seasonal,
  random = decomp$random
)

# melt the data frame into long format
df_comp_long <- melt(df_decomp, id.vars = 'x')

# create the plot
ggplot(df_comp_long, aes(x = x, y = value)) +
  geom_line() +
  facet_wrap(~variable, scales = 'free', nrow = 3) +
  labs(x = 'Date', y = NULL) +
  theme_bw()

#Time series decomposition - S&P 500
sp500_ts <- ts(df_drivers[order(df_drivers$Date),]$SP_500, start = c(2010,3), frequency = 4)
decomp_sp500 <- decompose(sp500_ts, type = 'multiplicative')

df_decomp_sp500 <- data.frame(
  x = time(sp500_ts),
  trend = decomp_sp500$trend,
  seasonal = decomp_sp500$seasonal,
  random = decomp_sp500$random
)

# melt the data frame into long format
df_comp_long_sp500 <- melt(df_decomp_sp500, id.vars = 'x')

# create the plot
ggplot(df_comp_long_sp500, aes(x = x, y = value)) +
  geom_line() +
  facet_wrap(~variable, scales = 'free', nrow = 3) +
  labs(x = 'Date', y = NULL) +
  theme_bw()
#Other factors plots
#S&P 500
ggplot(df_drivers, aes(x=Date)) + 
  geom_line(aes(y = SP_500),color = 'firebrick', linewidth = 0.8) + 
  ylab('Price ($)') + xlab('Quarters') +
  scale_x_date(breaks = as.Date(c("2010-06-01", "2014-03-01", '2018-09-01', '2022-12-01')), 
               labels = c('2010 Q3', '2014 Q2', '2018 Q4', '2023 Q1')) +
  ggtitle('Quarterly S&P 500 between 2010 Q3 and 2023 Q1') 

#CPI
ggplot(melt(data.table(df_drivers %>% select(Date,CPIAUCSL_change,CPILFESL_change)), 
            id.vars = c('Date')), aes(x=Date)) + 
  geom_line(aes(y = value/100, color = variable), linewidth = 0.8) + 
  scale_color_manual(name = '' ,values = c("#0072B2", "#D55E00"), labels = c('CPI', 'CPI Core')) + # Add color legend with blue and orange colors
  ylab('Growth Rate') + xlab('Quarters') +
  scale_x_date(breaks = as.Date(c("2010-06-01", "2014-03-01", '2018-09-01', '2022-12-01')), 
               labels = c('2010 Q3', '2014 Q2', '2018 Q4', '2023 Q1')) +
  ggtitle('CPI Growth between 2010 Q3 and 2023 Q1') +
  scale_y_continuous(labels = scales::percent_format())

#Federal Funds Rate
ggplot(df_drivers, aes(x=Date)) + 
  geom_line(aes(y = Fed_Funds/100),color = 'chocolate4', linewidth = 0.8) + 
  theme_minimal() + ylab('Federal Funds Rate') + xlab('Quarters') +
  scale_y_continuous(labels = percent_format()) +
  scale_x_date(breaks = as.Date(c("2010-06-01", "2014-03-01", '2018-09-01', '2022-12-01')), 
               labels = c('2010 Q3', '2014 Q2', '2018 Q4', '2023 Q1')) +
  ggtitle('Quarterly Federal Funds Rates between 2010 Q3 and 2023 Q1') 


#SP 500 Change
ggplot(df_drivers, aes(x=Date)) + 
  geom_line(aes(y = SP_500_change/100),color = 'firebrick', linewidth = 0.8) + 
  theme_minimal() + ylab('Change') + xlab('Quarters') +
  scale_y_continuous(labels = percent_format()) +
  scale_x_date(breaks = as.Date(c("2010-06-01", "2014-03-01", '2018-09-01', '2022-12-01')), 
               labels = c('2010 Q3', '2014 Q2', '2018 Q4', '2023 Q1')) +
  ggtitle('S&P 500 Quarterly Change between 2010 Q3 and 2023 Q1') 

#Unemployment
ggplot(df_drivers, aes(x=Date)) + 
  geom_line(aes(y = Unemp_Rate/100),color = 'dodgerblue3', linewidth = 0.8) + 
  theme_minimal() + ylab('Unemployment Rate') + xlab('Quarters') +
  scale_y_continuous(labels = percent_format()) +
  scale_x_date(breaks = as.Date(c("2010-06-01", "2014-03-01", '2018-09-01', '2022-12-01')), 
               labels = c('2010 Q3', '2014 Q2', '2018 Q4', '2023 Q1')) +
  ggtitle('Unemployment Rate between 2010 Q3 and 2023 Q1') 

#Lag correlation
bit_ts = df %>%
  filter(Date > as.Date('2017-01-01')) %>%
  arrange(Date) %>%
  dplyr::select(Price) %>%
  as.matrix() %>%
  ts()

gglagplot(bit_ts, do.lines = F) + my_theme +
  scale_color_continuous(low = "#00008B", high = "#ADD8E6", breaks = c(183, 548, 913, 1278, 1643, 2008), 
                         labels = c('2017', '2018', '2019', '2020', '2021', '2022')) + 
  scale_y_continuous(breaks = c(0, 20000, 40000, 60000), 
                     labels = c('$0', '$20,000', '$40,000', '$60,000')) +
  scale_x_continuous(breaks = c(20000, 40000, 60000), 
                     labels = c('$20,000', '$40,000', '$60,000')) +
  ggtitle('Correlation Graph of the Bitcoin Price')

# Correlation heatmap
correlation <- round(cor(combined_df[,c(2,5:11)], use = "complete.obs"),2)
correlation[upper.tri(correlation)] <- NA
correlation <- na.omit(reshape2::melt(correlation))

# Create ggplot without NA values and move y-ticks to the right side
ggplot(data = correlation, aes(x = Var2, y = Var1, fill = value)) + 
  geom_tile() +
  geom_text(aes(label = sprintf("%1.2f", value)), size = 4) + # show correlation values with 2 decimal places
  scale_fill_gradient2(low = "red", high = "green", limit = c(-1, 1), name = "Correlation") +
  scale_x_discrete(expand = c(0,0)) + # remove gray areas in x-axis
  scale_y_discrete(expand = c(0,0)) + # remove gray areas in y-axis
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank()) +
  ggtitle('Correlation Heatmap')
  

#########Potential Drivers########
#Anil
#df_drivers <- read.csv(file= 'C:/Users/asus/Documents/GitHub/csbtc/current.csv')
#Elcin
#df_drivers <- read.csv('C:/Users/Acer/OneDrive - ADA University/Documents/GitHub/csbtc/current.csv')





