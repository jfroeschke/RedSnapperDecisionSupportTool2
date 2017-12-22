library(shinydashboard)

ui <- function(request){
  
  dashboardPage(skin="red",
  
  dashboardHeader(title="Red snapper"),
  
  dashboardSidebar(
    
    sidebarMenu(
      tags$head(includeCSS("Style.css")),
      tags$head(includeCSS("Style2.css")),
      menuItem(" ", tabName = "menu1"),
      #menuItem("Menu 2", tabName = "menu2", icon = icon("gears")),
      br(),
      br(),
      div(img(src="logo.png"), style="text-align: center;"),
      div(tags$a(href="mailto: portal@gulfcouncil.org", h4("portal@gulfcouncil.org")), align="center"),
      div(br()),
      br(),
      br(),
      HTML("<h5 id='title' style='text-align:center;' >Gulf of Mexico <br> Fishery Management Council <br> 2203 North Lois Avenue, Suite 1100 <br>
     Tampa, Florida 33607 USA <br> P: 813-348-1630")
      
    )
    
  ),
  
  dashboardBody(
    
    tabItems(
      
      tabItem(tabName = "menu1",
              ##Note these 3 lines load the modal box
              ## Uncomment to turn on when ready
             includeHTML('pageLoadHTML4.html'),
              includeScript('modalJS.js'),
              includeCSS('modalStyle.css'),
              fluidRow(
                
                column(width = 6,
                       
                       
                       tabBox(id = "tabP1", height=750,
                              side = "left",    
                              tabPanel("Alternative 1", p(Alt1Text)
                                       ),
                              
                        
                              tabPanel("Alternative 2", p(Alt2Text),
                                       hr(),
                                       radioButtons(inputId = "Alt2Radio", 
                                                    label = "Alternative 2 options", 
                                                    choices = c("Option 2a: 1986 - 2015",
                                                                "Option 2b: 1996 - 2015",
                                                                "Option 2c 2006 - 2015", 
                                                                "Option 2d: 50% of average historical landings for the years 1986-2015 and 50% of average historical landings for the years 2006-2015"), 
                                                    selected = "Option 2a: 1986 - 2015",  width='600px'),
                                       hr(),
                                       box(tableOutput("summaryTable"), width=6),
                                       box(tableOutput("summaryTableForHire"), width=6)
                                                                        ),
                              tabPanel("Alternative 3", p(Alt3Text),
                                       radioButtons(inputId = "Alt3Radio", 
                                                    label = "Alternative 3 options", 
                                                    choices = c("Option 3a: 1986 - 2009",
                                                                "Option 3b: 1996 - 2009",
                                                                "Option 3c: 2006 - 2009", 
                                                                "Option 3d: 50% of average historical landings for the years 1986-2009 and 50% of average historical landings for the years 2006-2009"), 
                                                    selected = "Option 3a: 1986 - 2009",  width='600px'),
                                     
                                       # p('* More than one option may be selected'),
                                       # p('** Not applicable to Alternative 3'),
                                       hr(),
                                       box(tableOutput("summaryTableAlt3"), width=6),
                                       box(tableOutput("summaryTableForHireAlt3"), width=6)
                                      ),
                              tabPanel("Alternative 4", p(Alt4Text),
                                       fluidRow(
                                         column(3),
                                         column(6,
                                       selectInput("selectOption", multiple=FALSE,
                                                   h3("Select start year"),
                                                   c("Option a: 1986"="OptionA",
                                                     "Option b: 1996"="OptionB",
                                                     "Option c: 2006"="OptionC",
                                                     "Option d: 1986 and 2006"="OptionD"),
                                                   selected=c("OptionA"))
                                       ),
                                       column(3)
                                       ), ##end fluidRow
                                       fluidRow(
                                         column(3),
                                         column(6,
                                       selectInput("selectAlternative", multiple=FALSE,
                                                   h3("Select end year"),
                                                   c("Alternative 2: 2015"="ALT2",
                                                     "Alternative 3: 2009"="ALT3"),
                                                   selected=c("ALT2"))
                                         ),
                                       column(3)
                                       ), ##end fluidRow
                                       
                              fluidRow(
                                column(3),
                                column(6,
                              uiOutput("conditionalInput"),
                              conditionalPanel(
                                condition = "input.selectAlternative == 'ALT2'",
                                checkboxGroupInput("ALT2", "Alt 2",
                                                   c("Exlude 2006" = 2006,
                                                     "Exlude 2010" = 2010,
                                                     "Exlude 2014" = 2014,
                                                     "Exlude 2015" = 2015),
                                                   selected=2010)
                                                      ),
                              conditionalPanel(
                                condition = "input.selectAlternative == 'ALT3'",
                                checkboxGroupInput("ALT3", "Alternative 3",
                                                   c("Exlude 2006" = 2006
                                                          ))#,
                                # box(tableOutput("test"), width=6)
                              )),
                              column(3)
                                ), #endfluid row
                              hr(),
                              box(tableOutput("summaryTableAlt4Private"), width=6),
                              box(tableOutput("summaryTableAlt4ForHire"), width=6)
                              
                              #box(tableOutput("test"), width=6)
                              ),
                                       
                            
                              tabPanel("Alternative 5", p(Alt5Text),
                                      # box(
                                      #   sliderInput("topNumber", "Select number of years to include:", sep="",min = 5, max = 15, value = c(10)),
                                      #   sliderInput("Year", "Select Years:", sep="",min = 1986, max = 2015, value = c(1986,2015))
                                      #   ,width=12),
                                      
                                      fluidRow(
                                        column(3),
                                        column(6,
                                        sliderInput("topNumber", "Select number of years to include:", sep="",min = 5, max = 15, value = c(10)),
                                        sliderInput("Year", "Select Years:", sep="",min = 1986, max = 2015, value = c(1986,2015))),
                                        column(3)),
                                        hr(),
                                     
                                      box(tableOutput("out32"), width=6),
                                      box(tableOutput("out32ForHire"), width=6)
                                      
                                      #box(tableOutput("topNdata"), width=12)
                                      ), #end tabpanel 5
                              
                              tabPanel("Alternative 6", p(Alt6Text),
                                       #bookmarkButton(label="Save settings"),
                                       fluidRow(
column(6, radioGroupButtons(inputId = "Id073", 
                             label = "Select recreational component:", choices = c("Total", 
                                                          "Private", "For-hire"), individual = TRUE, 
                             checkIcon = list(yes = tags$i(class = "fa fa-circle", 
                                                           style = "color: steelblue"), 
                                              no = tags$i(class = "fa fa-circle-o", 
                                                          style = "color: steelblue")))), 
column(6, pickerInput(inputId = "TimeSeriesSelect", 
                      label = "*Select time series for trips/landings",
                      choices = list("Select years" = c("1986 - 2015","1996 - 2015", "2006 - 2015",#"50% 1986 - 2015: 50% 2006 -2015",
                                                        "1986 - 2009", "1996 - 2009", "2006 - 2009" 
                                                        #"50% 1986 - 2015: 50% 2006 -2009"
                                                        )))),
column(12, div(HTML("<h4 id='A6title' style='text-align:center;' ><b>**Select an option</b>"))
      )
  ),
##############Experimental with action buttons
fluidRow(
  column(12,
         actionButton("goButton", "6a:  25% biomass, 75% trips"),
         actionButton("goButton2", "6b:  50% biomass, 50% trips"),
         actionButton("goButton3", "6c:  75% biomass, 0% trips")
  )
),
##############Experimental with action buttons


                                       fluidRow(
# column(5,HTML('<a  href="https://gulfcouncilportal.shinyapps.io/RedSnapperDecisionSupportTool/?_inputs_&a1=0.5&ALT2=%222010%22&Alt2Radio=%22Option%202a%3A%201986%20-%202015%22&ALT3=null&Alt3Radio=%22Option%203a%3A%201986%20-%202009%22&b1=0&c1=0.5&map_bounds=%7B%22north%22%3A38.0653923513325%2C%22east%22%3A-73.1689453125%2C%22south%22%3A14.6898813666188%2C%22west%22%3A-102.83203125%7D&map_center=%7B%22lng%22%3A-88%2C%22lat%22%3A27%7D&map_groups=%22biomass%22&map_zoom=5&selectAlternative=%22ALT2%22&selectOption=%22OptionA%22&sidebarCollapsed=false&sidebarItemExpanded=null&tabP1=%22Alternative%206%22&tabP2=%22Alternative%206%22&topNumber=10&Year=%5B1986%2C2015%5D">
#                 <h4 class="btn btn-default action-button" style="fontweight:600">Option 6b:  50% biomass, 50% trips</h4>
#                                             </a>')),
#column(2, HTML('<b>Or </b>')),
#column(12, div(HTML("<h4 id='A6Note' style='text-align:center;' ><b>Results displayed in orange box below</b>")))
),
#                                        fluidRow(
# column(12, HTML('<a  href="https://gulfcouncilportal.shinyapps.io/RedSnapperDecisionSupportTool/?_inputs_&a1=0.25&ALT2=%222010%22&Alt2Radio=%22Option%202a%3A%201986%20-%202015%22&ALT3=null&Alt3Radio=%22Option%203a%3A%201986%20-%202009%22&b1=0&c1=0.75&map_bounds=%7B%22north%22%3A38.0653923513325%2C%22east%22%3A-73.1689453125%2C%22south%22%3A14.6898813666188%2C%22west%22%3A-102.83203125%7D&map_center=%7B%22lng%22%3A-88%2C%22lat%22%3A27%7D&map_groups=%22biomass%22&map_zoom=5&selectAlternative=%22ALT2%22&selectOption=%22OptionA%22&sidebarCollapsed=false&sidebarItemExpanded=null&tabP1=%22Alternative%206%22&tabP2=%22Alternative%206%22&topNumber=10&Year=%5B1986%2C2015%5D">
#                 <h4 class="btn btn-default action-button" style="fontweight:600">Option 6c:  75% biomass, 25% trips</h4>
#                                             </a>'))),
# fluidRow(
#   column(12, HTML('<a  href="https://gulfcouncilportal.shinyapps.io/RedSnapperDecisionSupportTool/?_inputs_&a1=0.25&ALT2=%222010%22&Alt2Radio=%22Option%202a%3A%201986%20-%202015%22&ALT3=null&Alt3Radio=%22Option%203a%3A%201986%20-%202009%22&b1=0&c1=0.75&map_bounds=%7B%22north%22%3A38.0653923513325%2C%22east%22%3A-73.1689453125%2C%22south%22%3A14.6898813666188%2C%22west%22%3A-102.83203125%7D&map_center=%7B%22lng%22%3A-88%2C%22lat%22%3A27%7D&map_groups=%22biomass%22&map_zoom=5&selectAlternative=%22ALT2%22&selectOption=%22OptionA%22&sidebarCollapsed=false&sidebarItemExpanded=null&tabP1=%22Alternative%206%22&tabP2=%22Alternative%206%22&topNumber=10&Year=%5B1986%2C2015%5D">
#                 <h4 class="btn btn-default action-button" style="fontweight:600">Use landings data only</h4>
#                                             </a>'))),

#hr(),
fluidRow(
    column(2),
    column(8,
           div(HTML("<h4 id='A6Inst' style='text-align:center;' > <b>Or...create your own option by <br> specifying percentages for variables in green boxes to calculate weighted allocations.</b></h4>"))),
    column(2)),
           
                                       
                                       fluidRow(
                                         column(2),
                                         column(2,  inline_numericInput(numericInput("a1", label = "Biomass", value = 50, min=0, max=100, step=1))),
                                         column(2, inline_numericInput(numericInput("b1", label = "Trips", value = 25, min=0, max=100, step=1))),
                                         column(2, inline_numericInput(numericInput("c1", label = "***Landings", value = 25, min=0, max=100, step=1))),
                          
                                         column(2, 
                                                inlineCSS("#check { border: 3px solid #00c0ef;;border-style: dashed;"),
                                                tableOutput("check")),
                                         column(2)
                                       
                                         ###
),
                                       fluidRow(
                                         column(12,align="center",tableOutput("dfToolTable")),
                                         inlineCSS("#x2 { border: 3px solid #f39c12;border-style: dashed;"),
                                         column(12,align="center", tableOutput("x2")),
                                         bsAlert("alert"),
                                        
                                         column(12,align="center",br(),
                                                bookmarkButton(label="Save settings")),
                                         column(12, HTML("<br>*2010 data were excluded from all times series considered here.<br>**Options were developed by the Gulf Council at their October 2017 meeting.  <br> ***An option to consider landings as a weighting factor was added for covenience but has not been reviewed by the Gulf Council and is intended for exploratory use only."))
                                       
                                       )
                                        ),
                              
                              width = NULL)
                       
                
                      ),
                
  ##############################################################################
                column(width = 6,
                       
                       tabBox(id = "tabP2", height=600,
                              side = "left",    
                              tabPanel("Alternative 1", p(""),
                                       fluidRow(column(12,
                                                      imageOutput("rsImage")
                                      #div(img(src="IMG_3627_RS.JPG"), style="text-align: center;width='5%';")
                                       #div(id='pic',img(src="IMG_3627_RS.JPG"))))),
                                       #div(HTML('<img src="www/IMG_3627_RS.JPG" alt="" width="50%" height="30%" />')
                                       ))),
                              tabPanel("Alternative 2", p(""),
                                       highchartOutput("landingsChart"),
                                       highchartOutput("landingsChartForHire")),
                              tabPanel("Alternative 3", p(" "),
                                       highchartOutput("landingsChartAlt3"),
                                       highchartOutput("landingsChartForHireAlt3")), ##UD
                              tabPanel("Alternative 4", p(" "),
                                       highchartOutput("landingsChartAlt4"),
                                       highchartOutput("landingsChartAlt4ForHire")),
                                       
                                      
                              tabPanel("Alternative 5", p(""),
                                       highchartOutput("topNlandingsOut"),
                                       highchartOutput("topNlandingsOutForHire")),
                              tabPanel("Alternative 6", p("Interactive map of red snapper biomass in the Gulf of Mexico.
                                                          Note, this may take a moment to load, please be patient."),
                                       leafletOutput('map',height=600),
                                       p("Data source: Mandy Karnauskas, John F. Walter III, Matthew D. Campbell, Adam G. Pollack, J. Marcus Drymon & Sean Powers.
2017. Red Snapper Distribution on Natural Habitats and Artificial Structures in the Northern Gulf of Mexico.Marine and Coastal Fisheries Vol. 9 , Iss. 1,2017")),
                              width = NULL
                       ) 
                       
                )
                
              )
              
      ),
      
      tabItem(tabName = "menu2",
              
              fluidRow(
                
                column(width = 4,
                       
                       valueBox(253, 
                                "Test", 
                             
                                width = NULL)
                       
                )
                
              )
              
      )
      
    )
    
  )
  
)
}