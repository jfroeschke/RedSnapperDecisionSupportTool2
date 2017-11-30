library(shinydashboard)

shinyUI <- dashboardPage(
  
  dashboardHeader(title="Red snapper"),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem("Management alternatives", tabName = "menu1", icon = icon("question-circle")),
      menuItem("Menu 2", tabName = "menu2", icon = icon("gears"))
      
    )
    
  ),
  
  dashboardBody(
    
    tabItems(
      
      tabItem(tabName = "menu1",
              
              fluidRow(
                
                column(width = 6,
                       
                       
                       tabBox(id = "tabP1", height=600,
                              side = "left",    
                              tabPanel("Alternative 1", p("No Action - Do not establish an allocation of the recreational sector component ACLs that may be used for state management programs.")
                                       ),
                              
                        
                              tabPanel("Alternative 2", p("Establish an allocation of the recreational sector ACL that may be used for state management programs by apportioning the private angling ACL and federal for-hire ACL among the states based on the average of historical landings for the years:"),
                                       radioButtons(inputId = "Alt2Radio", 
                                                    label = "Alternative 2 options", 
                                                    choices = c("Option 2a: 1986 - 2015",
                                                                "Option 2b: 1996 - 2015",
                                                                "Option 2c 2006 - 2015", 
                                                                "Option 2d: 50% of average historical landings for the years 1986-2015 and 50% of average historical landings for the years 2006-2015"), 
                                                    selected = "Option 2a: 1986 - 2015",  width='600px'),
                                    
                                       box(tableOutput("summaryTable"), width=6),
                                       box(tableOutput("summaryTableForHire"), width=6)
                                                                        ),
                              tabPanel("Alternative 3", p("Establish an allocation of the recreational sector ACL that may be used for state management programs by apportioning the private angling ACL and federal for-hire ACL among the states based on the average of historical landings for the years:"),
                                       radioButtons(inputId = "Alt3Radio", 
                                                    label = "Alternative 3 options", 
                                                    choices = c("Option 3a: 1986 - 2009",
                                                                "Option 3b: 1996 - 2009",
                                                                "Option 3c: 2006 - 2009", 
                                                                "Option 3d: 50% of average historical landings for the years 1986-2009 and 50% of average historical landings for the years 2006-2009"), 
                                                    selected = "Option 3a: 1986 - 2009",  width='600px'),
                                     
                                       p('* More than one option may be selected'),
                                       p('** Not applicable to Alternative 3'),
                                       box(tableOutput("summaryTableAlt3"), width=6),
                                       box(tableOutput("summaryTableForHireAlt3"), width=6)
                                      ),
                              tabPanel("Alternative 4", p("In calculating state apportionments under Alternative 2 or 3, exclude from the selected time series, as appropriate:"),
                                       selectInput("selectOption", multiple=FALSE,
                                                   h3("Select start year"),
                                                   c("Option a: 1986"="OptionA",
                                                     "Option b: 1996"="OptionB",
                                                     "Option c: 2006"="OptionC",
                                                     "Option d: 1986 and 2006"="OptionD"),
                                                   selected=c("OptionA")),
                                       selectInput("selectAlternative", multiple=FALSE,
                                                   h3("Select end year"),
                                                   c("Alternative 2: 2015"="ALT2",
                                                     "Alternative 3: 2009"="ALT3"),
                                                   selected=c("ALT2")),
                                       
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
                                checkboxGroupInput("ALT3", "Alternative 4: Select year(s) to exclude from calculations",
                                                   c("Exlude 2006" = 2006
                                                          ))#,
                                # box(tableOutput("test"), width=6)
                              ),
                              box(tableOutput("summaryTableAlt4Private"), width=6),
                              box(tableOutput("summaryTableAlt4ForHire"), width=6)
                              #box(tableOutput("test"), width=6)
                              ),
                                       
                            
                              tabPanel("Alternative 5", p("Establish an allocation of the recreational sector ACL that may be used for state management programs by apportioning the private angling ACL and federal for-hire ACL among the states based on each state's average of the best ten years of historical landings for the years 1986-2015."),
                                      box(
                                        sliderInput("topNumber", "Select number of years to include:", sep="",min = 5, max = 15, value = c(10)),
                                        sliderInput("Year", "Select Years:", sep="",min = 1986, max = 2015, value = c(1986,2015))
                                        ,width=12),
                                     
                                      box(tableOutput("out32"), width=6),
                                      box(tableOutput("out32ForHire"), width=6)#,
                                      #box(tableOutput("topNdata"), width=12)
                                      ), #end tabpanel 5
                              tabPanel("Alternative 6", p("Establish an allocation of the recreational sector ACL that may be used for state management programs by apportioning the private angling ACL and federal for-hire ACL among the states based on spatial abundance of red snapper biomass and recreational trips using:"),
                                       checkboxInput("Option6a", HTML("<b>Option 6a:</b> 25% biomass, 75% trips"), FALSE),
                                       checkboxInput("Option6b", HTML("<b>Option 6b:</b> 50% biomass, 50% trips"), TRUE),
                                       checkboxInput("Option6c", HTML("<b>Option 6c:</b> 75% biomass, 25% trips"), FALSE)
                                        ),
                              
                              width = NULL)
                       
                
                      ),
                
  ##############################################################################
                column(width = 6,
                       
                       tabBox(id = "tabP2", height=600,
                              side = "left",    
                              tabPanel("Alternative 1", p("")),
                              tabPanel("Alternative 2", p(""),
                                       highchartOutput("landingsChart"),
                                       highchartOutput("landingsChartForHire")),
                              tabPanel("Alternative 3", p(" "),
                                       highchartOutput("landingsChartAlt3"),
                                       highchartOutput("landingsChartForHireAlt3")), ##UD
                              tabPanel("Alternative 4", p("Tab 1 : right"),
                                       highchartOutput("landingsChartAlt4"),
                                       highchartOutput("landingsChartAlt4ForHire")),
                                       
                                      
                              tabPanel("Alternative 5", p(""),
                                       highchartOutput("topNlandingsOut"),
                                       highchartOutput("topNlandingsOutForHire")),
                              tabPanel("Alternative 6", p("Tab 3 : right")),
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
