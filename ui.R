library(shinydashboard)

shinyUI <- dashboardPage(skin="red",
  
  dashboardHeader(title="Red snapper"),
  
  dashboardSidebar(
    
    sidebarMenu(
      tags$head(includeCSS("Style.css")),
      menuItem("Management alternatives", tabName = "menu1", icon = icon("question-circle")),
      menuItem("Menu 2", tabName = "menu2", icon = icon("gears")),
      div(img(src="logo.png"), style="text-align: center;")
      
    )
    
  ),
  
  dashboardBody(
    
    tabItems(
      
      tabItem(tabName = "menu1",
              
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
                                     
                                       p('* More than one option may be selected'),
                                       p('** Not applicable to Alternative 3'),
                                       hr(),
                                       box(tableOutput("summaryTableAlt3"), width=6),
                                       box(tableOutput("summaryTableForHireAlt3"), width=6)
                                      ),
                              tabPanel("Alternative 4", p(Alt4Text),
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
                                       
                            
                              tabPanel("Alternative 5", p(Alt5Text),
                                      box(
                                        sliderInput("topNumber", "Select number of years to include:", sep="",min = 5, max = 15, value = c(10)),
                                        sliderInput("Year", "Select Years:", sep="",min = 1986, max = 2015, value = c(1986,2015))
                                        ,width=12),
                                     
                                      box(tableOutput("out32"), width=6),
                                      box(tableOutput("out32ForHire"), width=6)#,
                                      #box(tableOutput("topNdata"), width=12)
                                      ), #end tabpanel 5
                              tabPanel("Alternative 6", p(Alt6Text),
                                       checkboxInput("Option6a", HTML("<b>Option 6a:</b> 25% biomass, 75% trips"), FALSE),
                                       checkboxInput("Option6b", HTML("<b>Option 6b:</b> 50% biomass, 50% trips"), TRUE),
                                       checkboxInput("Option6c", HTML("<b>Option 6c:</b> 75% biomass, 25% trips"), FALSE),
                                       hr(),
                                       # textInputRow(inputId="xlimitsmin", label="x-min", value = 0.0),
                                       # textInputRow(inputId="xlimitsmax", label="x-max", value = 0.5),
                                       div(class="column-fluid",
                                           div(class="span3",numericInput("xlimitsmin", label = "x-min", value = 0.0)),
                                           div(class="span3",numericInput("xlimitsmax", label = "x-max", value = 0.5)),
                                           div(class="span3",numericInput("ylimitsmin", label = "y-min", value = 0.5)),
                                           div(class="span3",numericInput("ylimitsmax", label = "y-max", value = 1.0))
                                       ),
                                       #box( numericInput("obs1", "Observations:", 10, min = 1, max = 100, width=50),
                                            #numericInput("obs2", "Observations:", 10, min = 1, max = 100, width=50),
                                            #numericInput("obs3", "Observations:", 10, min = 1, max = 100, width=50)),
                                       box(tableOutput("dfToolTable"),width=12)#,
                                       #box(tableOutput("testtable"),width=12)
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
