library("readxl")
df <- read.csv('C:/Users/Acer/OneDrive - ADA University/Documents/GitHub/csbtc/current.csv')
df <- df %>% select(c('sasdate','UNRATE','CPIAUCSL', 'CPILFESL', 'FEDFUNDS', 
                'S.P.500', 'S.P..indust', 'S.P.div.yield',
                'S.P.PE.ratio')) 
df = df[-c(1,2),]
df$sasdate <- as.Date(df$sasdate, format = "%m/%d/%Y") 
