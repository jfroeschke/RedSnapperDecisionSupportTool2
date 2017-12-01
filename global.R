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
Private <- read_csv("PrivateAngling2.csv")
Private[30,2:6] <- NA ##remove 2010 (i.e., oil spill year)
Private$star <- c(rep(NA, 29), 0.325,rep(NA,6))
###Private2 is for Alternative 5 only
Private2 <- read_csv("PrivateAngling.csv")
Private2[30,2:6] <- NA ##remove 2010 (i.e., oil spill year)
Private2$star <- c(rep(NA, 29), 1000000,rep(NA,6))
ForHire <- read_csv("ForHire2.csv")
ForHire[30,2:6] <- NA ##remove 2010 (i.e., oil spill year)
ForHire$star <- c(rep(NA, 29), 0.325,rep(NA,6))

ForHire2 <- read_csv("ForHire.csv")
ForHire2[30,2:6] <- NA ##remove 2010 (i.e., oil spill year)
ForHire2$star <- c(rep(NA, 29), 750000,rep(NA,6))

##Load data for Alternative 6
## rasterize.R for details.
load("rsBiomass/mappedData.RData")

# pal <- colorNumeric(
#   palette = "viridis",
#   domain = Fig7mid$layer, reverse=TRUE)
# 
# leaflet() %>% setView(-85, 25, zoom=2) %>% 
#   addTiles() %>% 
#   addPolygons(data=Fig7mid, color = ~pal(layer),weight=1,
#               opacity=.6, fillOpacity=0.5, group='biomass') %>% 
#   addLegend("bottomright",pal = pal, 
#             values = Fig7mid$layer,title = "Index of biomass - with artificial strucures",
#             opacity = 0.5) %>% 
#   addPolygons(data=FL)

#########Description for UI
Alt1Text <- HTML("For a red snapper state management program to be enacted,
a portion of the recreational sector ACL would need to be designated for that 
state.  The recreational sector ACL is currently divided into separate private
angling and federal for-hire component ACLs.  This action addresses how to 
apportion the component ACLs of the recreational sector to provide a portion of
the recreational ACL to a state that has an approved state management program
for red snapper.  A state would establish its state management program through
a state-specific plan amendment.  For states that do not participate in state
management, management would continue with the remaining private angling and
federal for-hire component ACLs.
<p><br>

Allocation is an inherently controversial issue because a limited resource is 
divided among competing user groups, each of which benefits from receiving the 
largest portion possible.  In this action, the Council is determining the method
to calculate the apportionment, not the percentage each state would receive.  
The percentages would change based on the data used in the calculation equation.
Additionally, the landings are subject to high levels of uncertainty, especially
for Mississippi, and should be viewed with caution.  Regardless of the 
alternative selected, in some years, each state’s landings exceeds its average 
landings.  This means that requiring a state with an active state management 
program to constrain its catches to a fixed percentage of the recreational 
sector ACL could restrict the fluctuations in annual landings that occur in 
some years. Alternative 1 (No Action) would not apportion the recreational 
sector ACL among the states.  Separate management of the private angling and 
for-hire components’ harvest of red snapper would continue throughout federal 
waters of the Gulf through 2022.  Currently, there is no expressed state 
allocation; the proportion of the total recreational landings made up by 
each state varies from year to year. Tables are provided for landings by the 
recreational sector as a whole, the private angling component, and the federal 
for-hire component.  

<p><br>

<b>Alternative 1:</b> No Action - Do not establish an 
                 allocation of the recreational sector component ACLs that 
                 may be used for state management programs. <p><br>")

Alt2Text <- HTML("<b>Alternative 2</b> provides four options to apportion the 
                 recreational sector ACL based on the average proportion of 
                 historical landings for various time series that end in 2015.
                 The tables below provide the resulting percentages from 
                 apportioning the private angling (57.7%) and for hire (42.3%) 
                components of the ACL by state.  For the private angling sector, 
                 the sum of the state private angling ACLs for each alternative
                 totals 100% of the private angling ACL.  The federal for-hire 
                 component, with 42.3% of the recreational sector ACL, would 
                 remain under federal management but the allocation percentages
                are provided for reference.")

Alt3Text <- HTML("<b>Alternative 3</b> provides four options to apportion the 
                 recreational sector ACL based on the average proportion of 
                 historical landings for various time series ending in 2009.  
                 The difference between <b>Alternative 2</b> and <b>3</b> is that the time 
                 series ends in 2015 under Alternative 2 and ends in 2009 under 
                 Alternative 3.  For <b>Alternative 3</b>, the tables below provide the 
                 resulting percentages from apportioning the private angling 
                 component ACL by state for <b>Alternative 3</b>.")

Alt4Text <- HTML("<b>Alternative 4</b> provides options for excluding particular years
                 from the historical landings averages:  2006 (<b>Option 4a</b>), 2010 
                 (<b>Option 4b</b>), 2014 (<b>Option 4c</b>), and 2015 (<b>Option 4d</b>).  Options
                 to exclude 2006 and 2010 are due to impacts that affected 
                 recreational fishing opportunities during or immediately 
                 preceding those years.  Hurricane Katrina struck late in the 
                 fishing season of 2005, therefore landings from 2006 are 
                 provided for exclusion.  The Deepwater Horizon MC252 oil spill 
                 began in April 2010, prior to the opening of the 2010 
                 recreational red snapper season.  Due to the complexity 
                 associated with assigning landings between components given 
                 the substantial fishery closures and the extended federal season,
                 landings from 2010 should be viewed with caution and are not 
                 included for any alternatives in Tables 2.2.1 and 2.2.2.  
                 The Southeast Regional Office has excluded 2010 landings in all
                 season projection analyses for similar reasons.  Options to
                 exclude landings from 2014 and 2015 are provided because these
                 years were not included in the allocation formula used to 
                 calculate the private angling and federal for-hire components 
                 allocation in Amendment 40, and because the headboat 
                 collaborative pilot program operated during those years.  
                 The options under Alternative 4 may be selected individually, 
                 or multiple options could be selected alongside any of Options 
                 a-d under <b>Alternative 2</b> or <b>3</b>.  In Amendment 40 (GMFMC 2014a), 
                 the Council chose to exclude landings from 2010 (<b>Option 4b</b>) 
                 from the allocation formula, but did not exclude landings from 
                 2006 (<b>Option 4a</b>).")

Alt5Text <- HTML("
<b>Alternative 5</b>:  Establish an allocation of the recreational sector ACL that may 
be used for state management programs by apportioning the private angling ACL 
and federal for-hire ACL among the states based on each state’s average of the 
best ten years of historical landings for the years 1986-2015.
<p>
Alternative 5 would apportion the recreational sector ACL by 
averaging each state’s highest 10 years of red snapper landings for each 
component for the years 1986-2015, and then converting the average landings
into percentages.")

Alt6Text <- HTML("<b>Alternative 6</b> would establish an allocation of the 
recreational sector ACL that may be used for state management programs by
apportioning the private angling ACL and federal for-hire ACL among the states 
based on spatial abundance of red snapper biomass and recreational trips using")

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}
#   hc_add_theme(hc_theme_smpl()) %>% 
#   hc_yAxis(title = list(text = "Landings (lbs ww)"),
#            labels = list(style = list(color = "#000000", fontWeight="bold"))) %>% 
#   hc_exporting(enabled = TRUE, url="https://export.highcharts.com",
#                filename = "Recreational Landings") %>% 
#   hc_title(text = "Recreational red snapper landings")