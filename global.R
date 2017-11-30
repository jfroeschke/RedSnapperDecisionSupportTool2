## global.R
library(tidyverse)
library(shiny)
library(shinydashboard)
library(leaflet)
library(highcharter)
library(shinyWidgets) # enhanced radio button
library(reshape2)

Total <- read_csv("Total.csv")
Total[30,2:6] <- NA ##remove 2010 (i.e., oil spill year)
Total$star <- c(rep(NA, 29), 1750000,rep(NA,6))
Private <- read_csv("PrivateAngling.csv")
Private[30,2:6] <- NA ##remove 2010 (i.e., oil spill year)
Private$star <- c(rep(NA, 29), 1750000,rep(NA,6))
ForHire <- read_csv("ForHire.csv")
ForHire[30,2:6] <- NA ##remove 2010 (i.e., oil spill year)
ForHire$star <- c(rep(NA, 29), 1750000,rep(NA,6))
###Import landings data
# allRec <- read_csv("AllRecreationalLandings.csv")


# 
# 
# 
# 
# llRec <- read_csv("AllRecreationalLandings.csv")
# allRec[30,2:6] <- NA ##remove 2010 (i.e., oil spill year)
# allRec$star <- c(rep(NA, 29), 1750000,rep(NA,6))


##Preformat landings data for summary charts
# x <- filter(allRec, YEAR!=2010) %>% 
#   select(-star) %>% 
#   melt(id="YEAR")
# colnames(x) <- c("Year", "State", "Landings")


# recLandingsPlot <- highchart() %>% 
#   hc_xAxis(categories =allRec$YEAR) %>% 
#   # hc_xAxis(categories =allRec$YEAR,
#   #          plotBands=list(
#   #            list(color= "rgba(100, 0, 0, 0.1)",
#   #                 from=allRec$YEAR[29],
#   #                 to=allRec$YEAR[30]))) %>% 
#   hc_add_series(name = "Florida", data = allRec$FLW, type="line") %>%
#   hc_add_series(name = "Alabama", data = allRec$AL, type="line") %>%
#   hc_add_series(name = "Mississippi", data = allRec$MS, type="line") %>%
#   hc_add_series(name = "Louisiana", data = allRec$LA, type="line") %>%
#   hc_add_series(name = "Texas", data = allRec$TX, type="line") %>%
#   hc_add_series(name = "Oil spill: data omitted in 2010", type = 'scatter',data=allRec$star,
#                 marker=list(symbol="cross"),color='black') %>% 
#   hc_add_theme(hc_theme_smpl()) %>% 
#   hc_yAxis(title = list(text = "Landings (lbs ww)"),
#            labels = list(style = list(color = "#000000", fontWeight="bold"))) %>% 
#   hc_exporting(enabled = TRUE, url="https://export.highcharts.com",
#                filename = "Recreational Landings") %>% 
#   hc_title(text = "Recreational red snapper landings")