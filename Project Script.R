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
library(vars)
library(lmtest)
library(tseries)
conflict_prefer('select', 'dplyr')
conflict_prefer('filter', 'dplyr')
conflict_prefer('lag', 'dplyr')
conflict_prefer('melt', 'reshape2')
change_func <- function(col1, col2){
  change <- ((col1 - col2)/col2)*100
  return(change)
}
calc_AIC <- function(model){
  n <- length(model$residuals)
  k <- length(coef(model))
  AIC <- n*log(sum(model$residuals^2)/n) + 2*k
  return(AIC)
}
calc_AIC_var <- function(model){
  n <- length(model$varresult$change$residuals)
  k <- length(model$varresult$change$coefficients)
  AIC <- n*log(sum(model$varresult$change$residuals^2)/n) + 2*k
  return(AIC)
}
my_theme = theme(panel.grid = element_line(color = '#e6e6e6'),
                 panel.background = element_rect(fill = 'white'),
                 plot.title = element_text(hjust = .5, size = 12, colour = 'black'),
                 text = element_text(family = 'Georgia'),
                 axis.text = element_text(size = 10),
                 axis.title = element_text(size = 18, family = 'Georgia', face = 'bold'),
                 axis.line = element_line(colour = '#737373', size = 1),
                 strip.background = element_rect(colour = "black", fill = "white"),
                 strip.text = element_text(face = 'bold'))  
#to resolve the local time issue:
Sys.setlocale("LC_TIME", "C")

#Anil
#df <- read.csv(file= 'C:/Users/asus/Documents/GitHub/csbtc/Bitcoin Historical Data - Investing.com (1).csv')
#df_drivers <- read.csv(file= 'C:/Users/asus/Documents/GitHub/csbtc/current.csv')
##Mert 
#df <- read.csv(file= "/Users/mertbasaran/Documents/GitHub/csbtc/Bitcoin Historical Data - Investing.com (1).csv")
#df_drivers <- read.csv(file = "/Users/mertbasaran/Documents/GitHub/csbtc/current.csv")

#Elcin
#df <- read.csv(file= 'C:/Users/Acer/OneDrive - ADA University/Documents/GitHub/csbtc/Bitcoin Historical Data - Investing.com (1).csv')
#df_drivers <- read.csv('C:/Users/Acer/OneDrive - ADA University/Documents/GitHub/csbtc/current.csv')

#Alkim
df <- read.csv(file= 'C:/Users/alkim/OneDrive/Documents/GitHub/csbtc/Bitcoin Historical Data - Investing.com (1).csv')
df_drivers <- read.csv('C:/Users/alkim/OneDrive/Documents/GitHub/csbtc/current.csv')
#Deniz
#df <- read.csv(file="/Users/denizuygur/Documents/GitHub/csbtc/Bitcoin Historical Data - Investing.com (1).csv")
#df_drivers <- read.csv(file = "/Users/denizuygur/Documents/GitHub/csbtc/current.csv")


#########Data Manipulation########
names(df)[names(df) == 'Vol.'] <- "Vol"
names(df)[names(df) == 'Change..'] <- "Change"
names(df)[names(df) == 'X...Date'] <- "Date"


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
df_price_quarterly <- df %>% group_by(quarter) %>% summarise(price = exp(mean(log((Price)))))
#df_price_quarterly <- df %>% group_by(quarter) %>% summarise(price = mean(Price))
df_price_quarterly$lag <- lag(df_price_quarterly$price)
df_price_quarterly$change <- change_func(df_price_quarterly$price, df_price_quarterly$lag)
df_price_quarterly$lag <- na.fill(df_price_quarterly$lag, 0)
df_price_quarterly$change <- na.fill(df_price_quarterly$change, 0)
df_price_quarterly_without_2023_q2 <- df_price_quarterly[-nrow(df_price_quarterly),]

df_drivers <- df_drivers %>% select(c('sasdate','UNRATE','CPIAUCSL', 'CPILFESL', 'FEDFUNDS', 
                                      'S.P.500', 'S.P..indust', 'S.P.div.yield',
                                      'S.P.PE.ratio')) 
colnames(df_drivers) <- c('Date', 'Unemp_Rate', 'CPIAUCSL', 'CPILFESL', 'Fed_Funds',
                          'SP_500', 'SP_Industrial', 'SP_Dividend_Yield', 'SP_PE_Ratio')
df_drivers = df_drivers[-c(1,2),]
df_drivers$Date <- as.Date(df_drivers$Date, format = "%m/%d/%Y") 
df_drivers <- df_drivers %>% filter (df_drivers$Date >= "2010-09-01")
df_drivers$lag_unrate <- lag(df_drivers$Unemp_Rate)
df_drivers$unrate_change <- change_func(df_drivers$Unemp_Rate, df_drivers$lag_unrate)
df_drivers$SP_500_lag <- lag(df_drivers$SP_500)
df_drivers$SP_500_change <- change_func(df_drivers$SP_500, df_drivers$SP_500_lag)
df_drivers$CPIAUCSL_lag <- lag(df_drivers$CPIAUCSL)
df_drivers$CPIAUCSL_change <- change_func(df_drivers$CPIAUCSL, df_drivers$CPIAUCSL_lag)
df_drivers$CPILFESL_lag <- lag(df_drivers$CPILFESL)
df_drivers$CPILFESL_change <- change_func(df_drivers$CPILFESL, df_drivers$CPILFESL_lag)
#df_price_quarterly <- df_price_quarterly[-1]
df_drivers <- cbind(df_drivers,df_price_quarterly_without_2023_q2$quarter)



combined_df <- cbind(df_price_quarterly_without_2023_q2,df_drivers[,-c(1,ncol(df_drivers))])
#combined_df$normalized_price <- (combined_df$normalized_price - min(combined_df$normalized_price)) / (max(combined_df$normalized_price) - min(combined_df$normalized_price))
#combined_df <- combined_df[-c(1:11),]


plot(x = as.yearqtr(df_price_quarterly_without_2023_q1$quarter, format = '%Y Q%q'), y = df_price_quarterly_without_2023_q1$price, type = 'l')

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
ggplot(combined_df, aes(x = as.yearqtr(quarter, format = '%Y Q%q'))) +
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
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7)) 

adf.test(combined_df$change)
# combined_df_2017 <- combined_df %>% filter(as.yearqtr(df_price_quarterly_without_2023_q1$quarter, format = '%Y Q%q') >= '2017 Q1')
# 
# ggplot(combined_df_2017, aes(x = as.yearqtr(combined_df_2017$quarter, format = '%Y Q%q'))) +
#   geom_line(aes(y = change/100),color = 'darkgreen', linewidth = 1) + 
#   geom_point(aes(y = change/100),color = 'darkgreen') +
#   ylab('Growth Rate') + xlab('Quarters') +
#   ggtitle('Bitcoin Quarterly Growth Rate between 2017 Q1 to 2023 Q1') +
#   scale_y_continuous(labels = scales::percent_format()) +
#   scale_x_yearqtr(breaks = seq(min(as.yearqtr(combined_df_2017$quarter)), 
#                                max(as.yearqtr(combined_df_2017$quarter)), 
#                                by = 1/4),
#                   labels = function(x) format(x, "%Y Q%q"),
#                   expand = c(0, 0)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

#Time series decomposition - Bitcoin
price_ts <- ts(combined_df[order(combined_df$quarter),]$change, start = c(2010,3), frequency = 4)
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
  theme_bw()+
  ggtitle("Bitcoin Growth Additive Time Series Decomposition")

#Bitcoin Growth Acf Plot
acf_result <- acf(price_ts, na.action = na.pass, main = 'Bitcoin Growth Rate Autocorrelation Graph')


acf_df <- data.frame(lag = acf_result$lag, acf = acf_result$acf)

ggplot(acf_df, aes(x = lag*4, y = acf)) +
  geom_bar(stat = "identity", width = 0.3) +
  geom_hline(yintercept = c(-1.96/sqrt(length(df_decomp$random)), 1.96/sqrt(length(df_decomp$random))), linetype = "dashed") +
  xlab("Lag") +
  ylab("Autocorrelation") +
  ggtitle("Bitcoin Growth ACF Plot")  

#Pacf plot
pacf_result <- pacf(price_ts, na.action = na.pass)

pacf_df <- data.frame(lag = pacf_result$lag, pacf = pacf_result$acf)

ggplot(pacf_df, aes(x = lag*4, y = pacf)) +
  geom_bar(stat = "identity", width = 0.3) +
  geom_hline(yintercept = c(-1.96/sqrt(length(df_decomp$random)), 1.96/sqrt(length(df_decomp$random))), linetype = "dashed") +
  xlab("Lag") +
  ylab("Partial Autocorrelation") +
  ggtitle("Bitcoin Growth PACF Plot")

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
  theme_bw()+
  ggtitle("S&P 500 Multiplicative Time Series Decomposition")

#Other factors plots
#S&P 500
ggplot(df_drivers, aes(x=Date)) + 
  geom_line(aes(y = SP_500_change),color = 'firebrick', linewidth = 0.8) + 
  ylab('Price ($)') + xlab('Quarters') +
  scale_x_date(breaks = as.Date(c("2010-12-01", "2014-03-01", '2018-09-01', '2023-03-01')), 
               labels = c('2010 Q4', '2014 Q1', '2018 Q3', '2023 Q1')) +
  ggtitle('S&P 500 Quarterly Growth Rate between 2010 Q4 and 2023 Q1') +
  theme_minimal()

#CPI
ggplot(melt(data.table(df_drivers %>% select(Date,CPIAUCSL_change,CPILFESL_change)), 
            id.vars = c('Date')), aes(x=Date)) + 
  geom_line(aes(y = value/100, color = variable), linewidth = 0.8) + 
  scale_color_manual(name = '' ,values = c("#0072B2", "#D55E00"), labels = c('CPI', 'CPI Core')) + # Add color legend with blue and orange colors
  ylab('Growth Rate') + xlab('Quarters') +
  scale_x_date(breaks = as.Date(c("2010-12-01", "2014-06-01", '2018-12-01', '2023-03-01')), 
               labels = c('2010 Q4', '2014 Q2', '2018 Q4', '2023 Q1')) +
  ggtitle('CPI Growth between 2010 Q4 and 2023 Q1') +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()

#Federal Funds Rate
ggplot(df_drivers, aes(x=Date)) + 
  geom_line(aes(y = Fed_Funds/100),color = 'chocolate4', linewidth = 0.8) + 
  theme_minimal() + ylab('Federal Funds Rate') + xlab('Quarters') +
  scale_y_continuous(labels = percent_format()) +
  scale_x_date(breaks = as.Date(c("2010-12-01", "2014-06-01", '2018-12-01', '2023-03-01')), 
               labels = c('2010 Q4', '2014 Q2', '2018 Q4', '2023 Q1')) +
  ggtitle('Quarterly Federal Funds Rates between 2010 Q4 and 2023 Q1') 


#SP 500 Change
ggplot(df_drivers, aes(x=Date)) + 
  geom_line(aes(y = SP_500_change/100),color = 'firebrick', linewidth = 0.8) + 
  theme_minimal() + ylab('Change') + xlab('Quarters') +
  scale_y_continuous(labels = percent_format()) +
  scale_x_date(breaks = as.Date(c("2010-12-01", "2014-06-01", '2018-12-01', '2023-03-01')), 
               labels = c('2010 Q4', '2014 Q2', '2018 Q4', '2023 Q1')) +
  ggtitle('S&P 500 Quarterly Change between 2010 Q4 and 2023 Q1') 

#Unemployment
ggplot(df_drivers, aes(x=Date)) + 
  geom_line(aes(y = Unemp_Rate/100),color = 'dodgerblue3', linewidth = 0.8) + 
  theme_minimal() + ylab('Unemployment Rate') + xlab('Quarters') +
  scale_y_continuous(labels = percent_format()) +
  scale_x_date(breaks = as.Date(c("2010-12-01", "2014-06-01", '2018-12-01', '2023-03-01')), 
               labels = c('2010 Q4', '2014 Q2', '2018 Q4', '2023 Q1')) +
  ggtitle('Unemployment Rate between 2010 Q4 and 2023 Q1') 

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



#part_C  
#Growth ratelerde stationarity testi yapip stationary hale getirecek miyiz
ts_data <- ts(combined_df$change, start = c(2013,3), frequency = 4)
forecasts_ar <- list()
forecasts_ar[1:2] <- NA
for (i in 2:(length(ts_data)-1)){
  train_df <- ts_data[1:i]
  ar1_model <- ar(train_df, order = 1, method = 'ols')
  forecasts_ar[i+1] <- forecast(ar1_model,h=1)$mean
}

forecasts_ar <- unlist(forecasts_ar)
forecasts_with_realized <- cbind(combined_df %>% select(quarter, change), forecasts_ar)
forecasts_with_realized_long <- melt(forecasts_with_realized, 
     id.vars = c('quarter'))
forecasts_with_realized_long$quarter <- as.yearqtr(forecasts_with_realized_long$quarter, format = "%Y Q%q")

ggplot(forecasts_with_realized_long, aes(x=quarter)) + 
  geom_line(aes(y = value/100, color = variable), linewidth = 0.5) + 
  geom_point(aes(y = value/100, color = variable)) + 
  scale_color_manual(name = '' ,values = c("#0072B2", "#D55E00"), labels = c('Actual', 'Forecasted')) + # Add color legend with blue and orange colors
  ylab('Growth Rate') + xlab('Quarters') +
  scale_x_yearqtr(breaks = seq(min(as.yearqtr(forecasts_with_realized_long$quarter)), 
                               max(as.yearqtr(forecasts_with_realized_long$quarter)), 
                               by = 1),
                  labels = function(x) format(x, "%Y Q%q"),
                  expand = c(0, 0)) +
  ggtitle('Actual vs. Forecasted Growth Rate in AR(1) Model') +
  scale_y_continuous(labels = scales::percent_format())

forecasts_with_realized_extracted_ar <- forecasts_with_realized[-c(1:2),]
RMSE_AR <- mean((forecasts_with_realized_extracted_ar$change - forecasts_with_realized_extracted_ar$forecasts_ar)**2)**0.5

#part d
var_data <- combined_df %>% select(change, Unemp_Rate, CPILFESL, Fed_Funds, SP_500_change)
forecasts_var <- list()
forecasts_var[1:3] <- NA
for (i in 3:(nrow(var_data)-1)){
  train_df <- var_data[1:i,]
  var_model <- VAR(train_df, p = 1, type = 'none')
  forecasts_var[i+1] <- predict(var_model,n.ahead=1)$fcst$change[1]
}
forecasts_var <- unlist(forecasts_var)


forecasts_with_realized_with_var <- cbind(combined_df %>% select(quarter, change), forecasts_ar,forecasts_var )
forecasts_with_realized_with_var_long <- melt(forecasts_with_realized_with_var, 
                                     id.vars = c('quarter'))
forecasts_with_realized_with_var_long$quarter <- as.yearqtr(forecasts_with_realized_with_var_long$quarter, format = "%Y Q%q")

ggplot(forecasts_with_realized_with_var_long, aes(x=quarter)) + 
  geom_line(aes(y = value/100, color = variable), linewidth = 0.5) + 
  geom_point(aes(y = value/100, color = variable)) + 
  scale_color_manual(name = '' ,values = c("#0072B2", "#D55E00", "red"), labels = c('Actual', 'AR Forecasted', 'VAR Forecasted')) + # Add color legend with blue and orange colors
  ylab('Growth Rate') + xlab('Quarters') +
  scale_x_yearqtr(breaks = seq(min(as.yearqtr(forecasts_with_realized_with_var_long$quarter)), 
                               max(as.yearqtr(forecasts_with_realized_with_var_long$quarter)), 
                               by = 1),
                  labels = function(x) format(x, "%Y Q%q"),
                  expand = c(0, 0)) +
  ggtitle('Actual vs. Forecasted Growth Rates in AR(1) and VAR(1) Model') +
  scale_y_continuous(labels = scales::percent_format())

forecasts_with_realized_with_var_extracted <- forecasts_with_realized_with_var[-c(1:7),]
RMSE_AR_VAR <- mean((forecasts_with_realized_with_var_extracted$change - forecasts_with_realized_with_var_extracted$forecasts_var)**2)**0.5

#part e
grangertest(change ~ Unemp_Rate , order = 1, data = var_data)
grangertest(change ~ CPILFESL  , order = 1, data = var_data)
grangertest(change ~ Fed_Funds  , order = 1, data = var_data)
grangertest(change ~ SP_500_change , order = 1, data = var_data)
var_model <- VAR(var_data, p = 1, type = 'none')
print(causality(var_model, cause = 'change')$Granger)

#part f
lags <- 1:3
var_model1 <- VAR(var_data, p = 1, type = 'none')
var_model2 <- VAR(var_data, p = 2, type = 'none')
var_model3 <- VAR(var_data, p = 3, type = 'none')
calc_AIC_var(var_model1)
calc_AIC_var(var_model2)
calc_AIC_var(var_model3)
forecasts_var3 <- list()
aic3 <- list()
forecasts_var3[1:5] <- NA
for (i in 5:(nrow(var_data)-1)){
  train_df <- var_data[1:i,]
  var_model <- VAR(train_df, p = 3, type = 'none')
  forecasts_var3 <- c(forecasts_var3,predict(var_model,n.ahead=1)$fcst$change[1])
}

forecasts_var3 <- unlist(forecasts_var3)

forecasts_with_realized_with_var_3 <- cbind(combined_df %>% select(quarter, change), forecasts_ar,forecasts_var, forecasts_var3)
forecasts_with_realized_with_var_3_long <- melt(forecasts_with_realized_with_var_3, 
                                              id.vars = c('quarter'))
forecasts_with_realized_with_var_3_long$quarter <- as.yearqtr(forecasts_with_realized_with_var_3_long$quarter, format = "%Y Q%q")

ggplot(forecasts_with_realized_with_var_3_long, aes(x=quarter)) + 
  geom_line(aes(y = value/100, color = variable), linewidth = 0.5) + 
  geom_point(aes(y = value/100, color = variable)) + 
  scale_color_manual(name = '' ,values = c("#0072B2", "#D55E00", "red", 'darkgreen'), labels = c('Actual', 'AR(1)', 'VAR(1)','VAR(3)')) + # Add color legend with blue and orange colors
  ylab('Growth Rate') + xlab('Quarters') +
  scale_x_yearqtr(breaks = seq(min(as.yearqtr(forecasts_with_realized_with_var_3_long$quarter)), 
                               max(as.yearqtr(forecasts_with_realized_with_var_3_long$quarter)), 
                               by = 1),
                  labels = function(x) format(x, "%Y Q%q"),
                  expand = c(0, 0)) +
  ggtitle('Actual vs. Forecasted Growth Rates in AR(1), VAR(1), and VAR(3) Models') +
  scale_y_continuous(labels = scales::percent_format())

forecasts_with_realized_with_var_3_extracted <- forecasts_with_realized_with_var_3[-c(1:18),]
RMSE_VAR3 <- mean((forecasts_with_realized_with_var_3_extracted$change - forecasts_with_realized_with_var_3_extracted$forecasts_var3)**2)**0.5



#######PART II#######
####DATA MANIPULATION####
df$month <- paste(mondate::year(df$Date),mondate::month(df$Date))
df_price_monthly <- df %>% group_by(month) %>% summarise(price = mean(Price))
df_price_monthly <- df %>%
  group_by(month = format(Date, "%Y-%m")) %>%
  dplyr::summarize(price = mean(Price))
df_price_monthly <- df_price_monthly[-c((nrow(df_price_monthly)-1):nrow(df_price_monthly)),]
df_price_monthly <- df_price_monthly[-c(1:31),]
df_price_monthly$lag <- lag(df_price_monthly$price)
df_price_monthly$change <- change_func(df_price_monthly$price, df_price_monthly$lag)
#df_price_monthly = df_price_monthly[-c(52),] #last quarter not finished
df_price_monthly$lag <- na.fill(df_price_monthly$lag, 0)
df_price_monthly$change <- na.fill(df_price_monthly$change, 0)

#Anil
#df_drivers_monthly <- read.csv(file= 'C:/Users/asus/Documents/GitHub/csbtc/current_monthly.csv')
#Elcin
#df_drivers_monthly <- read.csv(file= 'C:/Users/Acer/OneDrive - ADA University/Documents/GitHub/csbtc/current_monthly.csv')
#Alkim
df_drivers_monthly <- read.csv(file= 'C:/Users/alkim/OneDrive/Documents/GitHub/csbtc/current_monthly.csv')
#Deniz
#df_drivers_monthly <- read.csv(file= "/Users/denizuygur/Documents/GitHub/csbtc/current_monthly.csv")
df_drivers_monthly <- df_drivers_monthly %>% select(c('sasdate','UNRATE','CPIAUCSL', 'FEDFUNDS', 
                                      'S.P.500', 'S.P..indust', 'S.P.div.yield',
                                      'S.P.PE.ratio')) 
colnames(df_drivers_monthly) <- c('Date', 'Unemp_Rate', 'CPIAUCSL', 'Fed_Funds',
                          'SP_500', 'SP_Industrial', 'SP_Dividend_Yield', 'SP_PE_Ratio')
df_drivers_monthly = df_drivers_monthly[-1,]
df_drivers_monthly$Date <- as.Date(df_drivers_monthly$Date, format = "%m/%d/%Y") 
df_drivers_monthly <- df_drivers_monthly %>% filter (df_drivers_monthly$Date >= "2013-02-01")
df_drivers_monthly$lag_unrate <- lag(df_drivers_monthly$Unemp_Rate)
df_drivers_monthly$unrate_change <- change_func(df_drivers_monthly$Unemp_Rate, df_drivers_monthly$lag_unrate)
df_drivers_monthly$SP_500_lag <- lag(df_drivers_monthly$SP_500)
df_drivers_monthly$SP_500_change <- change_func(df_drivers_monthly$SP_500, df_drivers_monthly$SP_500_lag)
df_drivers_monthly$CPIAUCSL_lag <- lag(df_drivers_monthly$CPIAUCSL)
df_drivers_monthly$CPIAUCSL_change <- change_func(df_drivers_monthly$CPIAUCSL, df_drivers_monthly$CPIAUCSL_lag)
df_drivers_monthly$monthnum <- mondate::month(df_drivers_monthly$Date)
#df_drivers_monthly$CPILFESL_lag <- lag(df_drivers_monthly$CPILFESL)
#df_drivers_monthly$CPILFESL_change <- change_func(df_drivers_monthly$CPILFESL, df_drivers_monthly$CPILFESL_lag)
#df_drivers_monthly <- df_drivers_monthly[-c((nrow(df_drivers_monthly)-1):nrow(df_drivers_monthly)),]
#df_drivers_monthly <- cbind(df_drivers_monthly,df_price_monthly$month)
#combined_df_monthly <- cbind(df_price_monthly,df_drivers_monthly[,-c(1,ncol(df_drivers_monthly))])
#combined_df$normalized_price <- (combined_df$normalized_price - min(combined_df$normalized_price)) / (max(combined_df$normalized_price) - min(combined_df$normalized_price))
#combined_df_monthly <- combined_df_monthly[-c(1:31),]

#Part h
t <- nrow(df_drivers)  # number of full quarters in the sample
#T <- nrow(df_drivers_monthly)
#K <- 3
df_drivers_monthly_selected <- df_drivers_monthly %>% select(Date, Unemp_Rate, CPIAUCSL_change, Fed_Funds, SP_500_change, monthnum)
X2 <- df_drivers_monthly_selected %>% filter(monthnum %in% c(2,5,8,11))
X2 <- X2[-1,]
X2 <- X2 %>% select(Unemp_Rate, CPIAUCSL_change, Fed_Funds, SP_500_change)
X1 <- df_drivers_monthly_selected %>% filter(monthnum %in% c(1,4,7,10))
X1 <- X1 %>% select(Unemp_Rate, CPIAUCSL_change, Fed_Funds, SP_500_change)
X0 <- df_drivers_monthly_selected %>% filter(monthnum %in% c(3,6,9,12))
X0 <- X0 %>% select(Unemp_Rate, CPIAUCSL_change, Fed_Funds, SP_500_change)
X_1 <- df_drivers_monthly_selected %>% filter(monthnum %in% c(2,5,8,11))
X_1 <- X_1[-nrow(X_1),]
X_1 <- X_1 %>% select(Unemp_Rate, CPIAUCSL_change, Fed_Funds, SP_500_change)
df_price_quarterly_extracted <- df_price_quarterly[-c(1:11),]
autoreg_y <- lag(df_price_quarterly_extracted$change)

midas_data <- data.frame(df_price_quarterly_extracted$change,autoreg_y,X2,X1,X0,X_1)
midas_data[is.na(midas_data)] <- 0
colnames(midas_data) <- c("response","autoreg_y","X_Unemp_Rate_2","X_CPIAUCSL_change_2","X_Fed_Funds_2","X_SP_500_change_2",
                          "X_Unemp_Rate_1","X_CPIAUCSL_change_1","X_Fed_Funds_1","X_SP_500_change_1",
                          "X_Unemp_Rate_0","X_CPIAUCSL_change_0","X_Fed_Funds_0","X_SP_500_change_0",
                          "X_Unemp_Rate_-1","X_CPIAUCSL_change_-1","X_Fed_Funds_-1","X_SP_500_change_-1")
midas_data1 <- midas_data[,-c((ncol(midas_data)-7):ncol(midas_data))]
midas_data2 <- midas_data[,-c((ncol(midas_data)-3):ncol(midas_data))]
midas_data3 <- midas_data
model_midasK1 <- lm(response~.,data=midas_data1)
summary(model_midasK1)
calc_AIC(model_midasK1)
model_midasK2 <- lm(response~.,data=midas_data2)
model_midasK3 <- lm(response~.,data=midas_data3)
calc_AIC(model_midasK1)
calc_AIC(model_midasK2)
calc_AIC(model_midasK3)
#task h
forecasts_midas <- list()
forecasts_midas[1] <- NA
for (i in 1:(nrow(midas_data1)-1)){
  train_df <- midas_data1[1:i,]
  midas_model <- lm(response~.,data=train_df)
  forecasts_midas[i+1] <- predict(midas_model,midas_data1[i+1,2:ncol(midas_data1)])
}

forecasts_midas <- unlist(forecasts_midas)


forecasts_with_realized <- cbind(df_price_quarterly_extracted %>% select(quarter, change), forecasts_midas)
forecasts_with_realized_long <- melt(forecasts_with_realized, 
                                     id.vars = c('quarter'))
forecasts_with_realized_long$quarter <- as.yearqtr(forecasts_with_realized_long$quarter, format = "%Y Q%q")

ggplot(forecasts_with_realized_long, aes(x=quarter)) + 
  geom_line(aes(y = value/100, color = variable), linewidth = 0.5) + 
  geom_point(aes(y = value/100, color = variable)) + 
  scale_color_manual(name = '' ,values = c("#0072B2", "#D55E00"), labels = c('Actual', 'Forecasted')) + # Add color legend with blue and orange colors
  ylab('Growth Rate') + xlab('Quarters') +
  scale_x_yearqtr(breaks = seq(min(as.yearqtr(forecasts_with_realized_long$quarter)), 
                               max(as.yearqtr(forecasts_with_realized_long$quarter)), 
                               by = 1),
                  labels = function(x) format(x, "%Y Q%q"),
                  expand = c(0, 0)) +
  ggtitle('Actual vs. Forecasted Growth Rate in MIDAS Model') +
  scale_y_continuous(labels = scales::percent_format())


#task i
step.model <- step(lm(response~.,data=midas_data1), direction = "backward", trace = TRUE, steps = 1)
step.model <- step(lm(response~.-X_CPIAUCSL_change_1,data=midas_data1), direction = "backward", trace = TRUE, steps = 1)
step.model <- step(lm(response~.-X_CPIAUCSL_change_1-X_Fed_Funds_1,data=midas_data1), direction = "backward", trace = TRUE, steps = 1)
step.model <- step(lm(response~.-X_CPIAUCSL_change_1-X_Fed_Funds_1-X_SP_500_change_1,data=midas_data1), direction = "backward", trace = TRUE, steps = 1)



forecasts_midas <- list()
forecasts_midas[1] <- NA
for (i in 1:(nrow(midas_data1)-1)){
  train_df <- midas_data1[1:i,]
  #midas_model <- lm(response~autoreg_y+X_SP_500_change_1,data=train_df)
  midas_model <- lm(response~autoreg_y + X_SP_500_change_2 + X_Unemp_Rate_2 +
                      X_CPIAUCSL_change_2 + X_Fed_Funds_2 + X_Unemp_Rate_1 + X_SP_500_change_1,data=train_df)
  forecasts_midas[i+1] <- predict(midas_model,midas_data1[i+1,2:ncol(midas_data1)])
}

forecasts_midas <- unlist(forecasts_midas)


forecasts_with_realized_midas <- cbind(df_price_quarterly_extracted %>% select(quarter, change), forecasts_midas)
forecasts_with_realized_midas_long <- melt(forecasts_with_realized_midas, 
                                     id.vars = c('quarter'))
forecasts_with_realized_midas_long$quarter <- as.yearqtr(forecasts_with_realized_midas_long$quarter, format = "%Y Q%q")
#task j
ggplot(forecasts_with_realized_midas_long, aes(x=quarter)) + 
  geom_line(aes(y = value/100, color = variable), linewidth = 0.5) + 
  geom_point(aes(y = value/100, color = variable)) + 
  scale_color_manual(name = '' ,values = c("#0072B2", "#D55E00"), labels = c('Actual', 'Forecasted')) + # Add color legend with blue and orange colors
  ylab('Growth Rate') + xlab('Quarters') +
  scale_x_yearqtr(breaks = seq(min(as.yearqtr(forecasts_with_realized_midas_long$quarter)), 
                               max(as.yearqtr(forecasts_with_realized_midas_long$quarter)), 
                               by = 1),
                  labels = function(x) format(x, "%Y Q%q"),
                  expand = c(0, 0)) +
  ggtitle('Actual vs. Forecasted Growth Rate in AR(1) Model') +
  scale_y_continuous(labels = scales::percent_format())


#predictions <- predict(model_midas)
#library(rms)
#model_midasK1 <- ols(response~.,data=midas_data1)
#step.model1 <-fastbw(model_midasK1,rule="aic",sls=0.1)
  
# plot_df <- cbind(df_price_quarterly_extracted %>% select(quarter, change),model_midas$fitted.values)
# colnames(plot_df)<-c("Quarter","Actual","Fitted")
# plot_df$Quarter <- as.yearqtr(plot_df$Quarter, format = "%Y Q%q")
# ggplot(plot_df,aes(x=Quarter)) +
#   geom_line(aes(y=Actual, color="blue")) +
#   geom_line(aes(y=Fitted,color="red"))
# labs(x = 'Date', y = NULL)


#RMSE
forecasts_with_realized_midas <- forecasts_with_realized_midas[-1,]
RMSE_midas <- mean((forecasts_with_realized_midas$change - forecasts_with_realized_midas$forecasts_midas)**2)**0.5
RMSE_midas

#part k
all_forecasts <- cbind((df_price_quarterly %>% select(quarter, change))[-c(1:12),], 
      forecasts_ar,forecasts_var, forecasts_var3,forecasts_midas[-1])
all_forecasts <- all_forecasts[-c(1:18),]
colnames(all_forecasts) <- c('quarter', 'change', 'forecasts_ar', 
                             'forecasts_var1', 'forecasts_var3', 'forecasts_midas')
RMSE_ar_partk <- mean((all_forecasts$change - all_forecasts$forecasts_ar )**2)**0.5
RMSE_var1_partk <- mean((all_forecasts$change - all_forecasts$forecasts_var1 )**2)**0.5
RMSE_var3_partk <- mean((all_forecasts$change - all_forecasts$forecasts_var3 )**2)**0.5
RMSE_midas_partk <- mean((all_forecasts$change - all_forecasts$forecasts_midas )**2)**0.5

#part l
var_data_partl <- df_drivers_monthly_selected %>% select(-c(Date,monthnum))
var_data_partl <- cbind(df_price_monthly$change,var_data_partl)
var_data_partl <- na.fill(var_data_partl, 0)
lags <- 1:12
optimal_lag <- VARselect(var_data_partl, lag.max = 12, type = 'none')$selection['AIC(n)']
forecasts_partl <- list()
aic_partl <- list()
forecasts_partl[1:5] <- NA
for (i in 5:(nrow(var_data_partl)-1)){
  train_df <- var_data_partl[1:i,]
  var_model <- VAR(train_df, p = optimal_lag, type = 'none')
  forecasts_partl <- c(forecasts_partl,predict(var_model,n.ahead=1)$fcst$df_price_monthly.change[1])
}

forecasts_partl <- unlist(forecasts_partl)

#AR
ts_data_partl <- ts(df_price_monthly$change, start = c(2013,2), frequency = 12)
forecasts_ar_partl <- list()
forecasts_ar_partl[1:2] <- NA
for (i in 2:(length(ts_data_partl)-1)){
  train_df <- ts_data_partl[1:i]
  ar1_model <- ar(train_df, order = 1)
  forecasts_ar_partl[i+1] <- forecast(ar1_model,h=1)$mean
}

forecasts_ar_partl <- unlist(forecasts_ar_partl)
#VAR(1)

forecasts_var1 <- list()
forecasts_var1[1:3] <- NA
for (i in 3:(nrow(var_data_partl)-1)){
  train_df <- var_data_partl[1:i,]
  var_model <- VAR(train_df, p = 1, type = 'none')
  forecasts_var1[i+1] <- predict(var_model,n.ahead=1)$fcst$df_price_monthly.change[1]
}
forecasts_var1 <- unlist(forecasts_var1)

forecasts_compr <- cbind(df_price_monthly %>% select(month, change), forecasts_ar_partl,forecasts_var1, forecasts_partl)
forecasts_compr_long <- melt(forecasts_compr,id.vars = c('month'))

forecasts_compr_long$month <-as.Date(as.yearmon(forecasts_compr_long$month))
ggplot(forecasts_compr_long, aes(x=month)) + 
  geom_line(aes(y = value/100, color = variable), linewidth = 0.5) + 
  #geom_point(aes(y = value/100, color = variable)) + 
  scale_color_manual(name = '' ,values = c("#0072B2", "#D55E00", "red", 'darkgreen'), labels = c('Actual', 'AR(1)', 'VAR(1)','VAR(3)')) + # Add color legend with blue and orange colors
  ylab('Growth Rate') + xlab('Month') +
  ggtitle('Actual vs. Forecasted Growth Rates in AR(1), VAR(1), and VAR(3) Models') +
  scale_y_continuous(labels = scales::percent_format())

forecasts_compr_extracted <- forecasts_compr[-c(1:18),]
RMSE_VAR_partl <- mean((forecasts_compr_extracted$change - forecasts_compr_extracted$forecasts_partl)**2)**0.5
RMSE_VAR_partl



