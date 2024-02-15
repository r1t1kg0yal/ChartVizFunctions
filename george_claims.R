


library(ChartVizFunctions)

library(quantmod)
library(Quandl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(fredr)
library(lubridate)
library(zoo)
library(stringr)
library(devtools)
library(readxl)
library(ustfd)
library(scales)
library(purrr)
library(tidyr)
library(imputeTS)


Quandl.api_key("Zs467eqKxC9Xw4r6wWGV")

invisible(getSymbols('ICNSA', src='FRED'))

ICNSA<-as.data.frame(ICNSA)


ICNSA$date<-as.Date(rownames(ICNSA))



invisible(getSymbols('ICSA', src='FRED'))

ICSA<-as.data.frame(ICSA)

ICSA$date<-as.Date(rownames(ICSA))


ICNSA<-left_join(ICNSA,ICSA)


invisible(getSymbols('CCNSA', src='FRED'))

CCNSA<-as.data.frame(CCNSA)

CCNSA$date<-as.Date(rownames(CCNSA))


invisible(getSymbols('CCSA', src='FRED'))

CCSA<-as.data.frame(CCSA)

CCSA$date<-as.Date(rownames(CCSA))

CCNSA<-left_join(CCNSA,CCSA)%>%
  mutate(date=date+7)


ICNSA<-left_join(ICNSA,CCNSA)


invisible(getSymbols('LNU01000000', src='FRED'))

LNU01000000<-as.data.frame(LNU01000000)

LNU01000000$date<-as.Date(rownames(LNU01000000))

ICNSA<-left_join(ICNSA,LNU01000000)

colnames(ICNSA)<-c("ICNSA","date","ICSA" ,"CCNSA","CCSA","LF")

end<-nrow(ICNSA)

#ld<-ICNSA$date[end]+7


#lICNSA<-234084
#llf<-lastNotNa(ICNSA$LF)
#end<-nrow(ICNSA)+1


#ICNSA[end,] <- c(lICNSA, lastdate, llf)



ICNSA$LF<-na_interpolation(ICNSA$LF)

#tail(ICNSA)

#colnames(ICNSA)


ICNSA<-ICNSA%>%
  mutate(adjICNSA=ifelse(ICNSA<600000,ICNSA,600000))%>%
  mutate(adjICSA=ifelse(ICSA<600000,ICSA,600000))%>%
  mutate(adjCCNSA=ifelse(CCNSA<6500000,CCNSA,6500000))%>%
  mutate(adjCCSA=ifelse(CCSA<6500000,CCSA,6500000))%>%
  mutate(LFi=LF/LF[1])%>%
  mutate(iICNSA=ICNSA/LFi)%>%
  mutate(iadjICNSA=adjICNSA/LFi)%>%
  mutate(iICSA=ICSA/LFi)%>%
  mutate(iadjICSA=adjICSA/LFi)%>%
  mutate(iCCNSA=CCNSA/LFi)%>%
  mutate(iadjCCNSA=adjCCNSA/LFi)%>%
  mutate(iCCSA=CCSA/LFi)%>%
  mutate(iadjCCSA=adjCCSA/LFi)%>%
  mutate(SIC=ICNSA-ICSA)%>%
  mutate(SCC=CCNSA-CCSA)%>%
  dplyr::mutate(year = lubridate::year(date),
                week = lubridate::week(date))

variables = list(ICNSA,ICSA,CCNSA,CCSA )
claims <- variables %>% reduce(full_join, by='date')%>%
  select(-c((contains(".y"))))%>%
  select(-c(CCSA))
colnames(claims)<- gsub(".x", "", colnames(claims))
claims<-as.data.frame(claims)



end<-nrow(claims)

claims$date <- as.Date(claims$date)
#colnames(claims)

g <- create_heatmap_plot(
  data = claims,             # Name of your dataframe
  var_name = "ICNSA", # Name of variable (must be in quotes)
  start_year = 1992,               # Start year
  end_year = 2024,                 # End year
  x_axis_breaks = 3,               # Frequency of x axis year labels
  title = "Initial Claims NSA",    # Plot title
  frequency = "weekly",           # Frequency of data (monthly or weekly)- this will through a "plot_label" error if it's not correct
  change_space = "level",          # Can be "level" or "yoy", if you want the heatmap to display "yoy" changes
  num_decimals = 0,                # Number of decimals to display on heatmap
  include_cell_numbers = TRUE,     # Whether you want to display the numbers on the heatmap at all
  scaling_power = 3,               # Divides the numbers displayed on the heatmap by 10^x
  override_subtitle = "1000s",        # Provide your own subtitle
  color_1 = "indianred",           # Low color
  color_2 = "dodgerblue",          # High color
  mute_color_1 = FALSE,            # Whether you want to mute the low color
  mute_color_2 = TRUE,             # Whether you want to mute the high color
  flip_colors = TRUE               # Flip the color scheme (low becomes high)
)
g + labs(ylab("Weeks"))
