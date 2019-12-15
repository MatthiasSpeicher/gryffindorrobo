library(shiny)
library(shinydashboard)
library(leaflet)
library(RJSONIO)
library(rjson)
library(githubinstall)
#library(rCharts)
library(readr)
library(shinythemes)
library(devtools)
#install_github("nik01010/dashboardthemes")
library(dashboardthemes)
library(geojsonio)
#devtools::install_github("RinteRface/shinydashboardPlus")
library(shinyWidgets)
library(rgdal)
#install.packages("tinytex")
#tinytex::install_tinytex()
library(markdown)
library(rmarkdown)
library(shinycssloaders)
library(ggplot2)
#install_github("MatthiasSpeicher/gryffindorrobo")
library(gryffindorrobo)

header <- dashboardHeader(
    title = shinyDashboardLogoDIY(boldText =  tagList(shiny::icon("robot"),
                                                      "Gryffindor"),
                                  mainText = "Robo-Advisor",
                                  badgeText = "Group 2",
                                  badgeBackColor = "#40E0D0",
                                  badgeTextColor = "white"),
    titleWidth = 300

)


sidebar <- dashboardSidebar(width = 280,
                            sidebarMenu(id = "tabs",
                                        menuItem("Identify your goals",
                                                 tabName = "tab1"),
                                        menuItem("Verify Risk Preference",
                                                 tabName = "tab2"),
                                        menuItem("Geographical Preferences",
                                                 tabName = "tab3"),
                                        menuItem("Industry Preferences",
                                                 tabName = "tab4"),
                                        menuItem("Portfolio Construction",
                                                 tabName = "tab5")
                            ), disable = TRUE
)


body <- dashboardBody(
    # shinyDashboardThemes(
    #     theme = "purple_gradient"
    #     #theme = "grey_dark"
    # ),
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),

    tabItems(

        # First Tab: Risk Evaluation
        tabItem(tabName = "tab1", h2("Risk Evaluation"), # tab item header
                fluidRow(
                    column(width = 4,
                           box(title = "Set Parameters",
                               width = 0.25,
                               uiOutput("initial_wealth"),
                               hr(style = "border-color: grey;"),
                               uiOutput("rpref"),
                               "Estimate your risk on the scale from 1 to 7,
                           where 1 is the lowest tolerance towards risk,
                           and 7 is the highest",
                               hr(style="border-color: grey;"),
                               fluidRow(
                                   column(6, uiOutput("inv_horizon")),
                                   column(6,
                                          sliderTextInput(
                                              inputId = "speed",
                                              label = "Simulation Speed:",
                                              #grid = TRUE,
                                              #force_edges = TRUE,
                                              choices = c("Very Slow",
                                                          "Slow",
                                                          "Moderate",
                                                          "Fast",
                                                          "Very Fast"),
                                              selected = "Moderate"
                                          ),
                                          br(),
                                          br(),
                                          h5(tags$b("Initialize Simulation:"),
                                             align = "left"),
                                          column(4, uiOutput("resetbutton")),
                                          column(4, uiOutput("startbutton")),
                                          column(4, uiOutput("play"))
                                   )
                               ),
                               tags$p("Press the", tags$em("Play"),
                                      "button in order to initiate the simulation.
                                   The button turns yellow. With the",
                                      tags$em("Pause"), "button you can interrupt
                                   the simulation and try to press the green
                                   button in order to see one possible
                                   realization. Use the red", tags$em("Stop"),
                                      "button to reset the simulation and start
                                   anew.")

                           ), # end of box (delete ,)

                           actionBttn(
                               inputId = "button1",
                               label = "Next",
                               style = "unite",
                               color = "success"
                           )

                    ),
                    ################ end of first column object#####################################
                    ################################################################################

                    column(width = 8,
                           tabBox(
                               id = "tabset1",
                               title = tagList(shiny::icon("dice"),
                                               "Portfolio Simulation"),
                               width = 12,
                               height = "550",
                               tabPanel(
                                   title = tagList(shiny::icon("chart-bar"),
                                                   "Histogram"),
                                   plotOutput("distPlot", height = "500")
                               ),

                               tabPanel(
                                   title = tagList(shiny::icon("info"), "Details"),
                                   "In order to get a good sense about the risks the
                         investor is faced with, this graph plots SIMULATED
                         and therefore POTENTIAL wealth evolvements, using the
                         paramteres provided.",
                                   br(),
                                   br(),
                                   "The higher the bar corresponding to a certain wealth
                         level, the more likely will you end up with a final
                         wealth equal to that amount.
                         The blue vertical line corresponds to the average
                         terminal wealth an investor can expect when investing
                         in the proposed portfolio.",
                                   br(),
                                   br(),
                                   "To get a feeling for the riskyness, also a down- and
                         upward boundary are included. The red vertical line
                         indicated the level of wealth the investor can expect
                         to have at least in 90 out of 100 cases. Please be
                         aware, that in 10 percent of the cases your terminal
                         wealth after the investment period will be below this
                         value.",
                                   br(),
                                   br(),
                                   "The green vertical line on the other hand aims to
                         indicate the upside potential. Investing in the
                         proposed portfolio will return a terminal wealth that
                         is below that threshold in 90 out of 100 cases. However
                         there is a 10 percent chance that the terminal wealth
                         lies above this threshold.",
                                   br(),
                                   br(),
                                   "Please note, that these data is not actual historical
                         stock data, but simulated. The goal of this graph is to
                         provide an intuitive picture of what may happen to your
                         initial capital invested at a certain risk level.",
                                   br(),
                                   br(),
                                   "The boxes below this graph summerize
                         some main features of your investment."
                               )
                           ),

                           # Dynamic valueBoxes
                           withSpinner(valueBoxOutput("horizonBox")),
                           valueBoxOutput("returnBox"),
                           valueBoxOutput("stdBox"),
                           valueBoxOutput("avgBox"),
                           valueBoxOutput("uplimBox"),
                           valueBoxOutput("lowlimBox")
                    )
                )
                ###################### end of fluid row ########################################
        ),
        ###################### end of first tab item####################################
        ################################################################################


        ######## Second Tab: Risk Evaluation ###########################################
        tabItem(tabName = "tab2", h2("Risk Evaluation"), # tab item header
                fluidRow(
                    column(width = 4,
                           box(
                               title = "Estimate your risk preference",
                               width = 0.25,
                               sliderInput("rpref2","Risk: ",
                                           value = 4,
                                           min = 1,
                                           max = 7,
                                           step = 1),
                               "Estimate your risk on the scale from 1 to 7,
                                   where 1 is the lowest tolerance towards risk,
                                   and 7 is the highest"
                           ),

                           fluidRow(
                               actionBttn(
                                   inputId = "button2",
                                   label = "Back",
                                   style = "unite",
                                   color = "danger"
                               ),

                               actionBttn(
                                   inputId = "button3",
                                   label = "Next",
                                   style = "unite",
                                   color = "success"
                               ))

                    ),
                    ################# end of first column object ###################################
                    ################################################################################

                    column(width = 8,
                           tabBox(
                               id = "tabset1",
                               title = tagList(shiny::icon("dice"),
                                               "Portfolio Simulation"),
                               width = 12,
                               height = "550",
                               tabPanel(title = tagList(shiny::icon("chart-bar"),
                                                        "Histogram"),
                                        plotOutput("distPlotFinish",
                                                   height = "500")),
                               tabPanel(title = tagList(shiny::icon("info"),
                                                        "Details"),
                                        "The goal of this graph is to facilitate
                                  the choice of the right risk preference for
                                  the investor. Essentially it is a copy of the
                                  graph of the previous plot.",
                                        br(),
                                        "In order to get a feeling on how the choice
                                  of the risk
                                  preference might impact the final wealth, the
                                  user might choose different levels of the risk
                                  preference and take the one that suits best.")
                           ),

                           # Dynamic valueBoxes
                           withSpinner(valueBoxOutput("horizonBox1")),
                           valueBoxOutput("returnBox1"),
                           valueBoxOutput("stdBox1"),
                           valueBoxOutput("avgBox1"),
                           valueBoxOutput("uplimBox1"),
                           valueBoxOutput("lowlimBox1")

                    )

                )
                ############# end of fluidrow ##################################################
                ################################################################################
        ),
        ######### end of second tab item ###############################################
        ################################################################################

        # Third Tab: Geographical Preferences
        tabItem(tabName = "tab3",
                h2("Geographical Preferences"), # tab item header

                ######## Inputs to let the user choose regions via map
                tabBox(
                    id = "tabset1",
                    height = "550",
                    title = tagList(shiny::icon("map-pin"),
                                    "Geographical Preferences"),
                    width = 8,
                    tabPanel(
                        title = tagList(shiny::icon("globe-americas"), "Map"),
                        withSpinner(leafletOutput("mymap", height = "500"))
                    ),

                    tabPanel(
                        title = tagList(shiny::icon("info"), "Details"),
                        "With the ticking boxes on the right hand side of the map
                    the investor has the possibility to (de)select regions
                    arround the world he or she does(n't) want to invest in. By
                    default all countries arround the world are selected. If for
                    any reason the investor doesn't want exposure to a certain
                    region in the portfolio, it might be deselected.",
                        br(),
                        br(),
                        "Please note: for diversification reasons it is highly
                    recommended not to deselect too many countries. However,
                    before going to the next tab, our algorithm will evaluate
                    the potential diversification possible, with the current
                    selection. If there is too less diversification potential,
                    you will be asked to select more regions or sectors
                    respectively.",
                        br(),
                        br(),
                        "Special Hint: Try out to invest in Antarctica in any case
                    ;-)"
                    )
                ),

                ######## Inputs to let the user switch on and of industries
                tabBox(
                    title = tagList(shiny::icon("industry"),
                                    "Industry Preferences"),
                    width = 4,
                    id = "tabset1",
                    height = "550",
                    tabPanel(
                        title = tagList(shiny::icon("clipboard-check"), "Sectors"),
                        multiInput(
                            inputId = "industry1",
                            label = "Industries",
                            choices = c(
                                "Banks" = "banks",
                                "Resources" = "resources",
                                "Chemicals" = "chemicals",
                                "Construction" = "construction",
                                "Financials" = "financials",
                                "Food" = "food",
                                "Health" = "health",
                                "Industrial" = "industrial",
                                "Insurance" = "insurance",
                                "Energy" = "energy",
                                "Personal" = "personal",
                                "Tech" = "tech",
                                "Telecom" = "telecom",
                                "Utilities" = "utilities"
                            ),

                            width = "100%",
                            options = list(
                                selected_header = "I don't want to invest in:",
                                non_selected_header = "Industries",
                                limit = 13 ####the user should't deselect all industries
                            )
                        )
                    ),


                    tabPanel(title = tagList(shiny::icon("info"), "Details"),
                             "Many investors today have preferences towards
                           certain industries or do not want to (for example for
                           ethical reasons) do not want to invest in other
                           sectors. This freedom is enabled with this selection.
                           By default, the portfolio calculated in the next
                           step does include all available industries, but the
                           user can avoid exposure to a certain industry by
                           pushing it to the right side of the graph.",
                             br(),
                             br(),
                             "By clicking a second time on a deselcted industry,
                           it is selected again.",
                             br(),
                             br(),
                             "Please note: for diversification reasons it is
                           highly recommended not to deselect too many
                           industries. However, before going to the next tab,
                           our algorithm will evaluate the potential
                           diversification possible, with the current selection.
                           If there is too less diversification potential, you
                           will be asked to select more regions or sectors
                           respectively.")
                ),

                hr(style = "border-color: grey;"),

                fluidRow(
                    actionBttn(
                        inputId = "button4",
                        label = "Back",
                        style = "unite",
                        color = "danger"
                    ),

                    actionBttn(
                        inputId = "button5",
                        label = "Next",
                        style = "unite",
                        color = "royal"
                    )
                ),

                tableOutput("table"),
                plotOutput("testgraph"),
                textOutput("Error_regions"),
                useSweetAlert()

        ),
        ######### end of third tab item ################################################
        ################################################################################


        # Fourth Tab: Portfolio Construction"
        tabItem(tabName = "tab4",
                h2("Portfolio Construction"), # tab item header
                fluidRow(
                    plotOutput("ourPF"),
                    radioButtons("format",
                                 "Document format",
                                 c("PDF", "HTML", "Word"),
                                 inline = TRUE),

                    downloadButton("downloadReport"),
                ),
                fluidRow( # Dynamic valueBoxes
                    valueBoxOutput("expectedValue"),
                    valueBoxOutput("yearlygain"),
                    valueBoxOutput("maxdrawdown"),
                    valueBoxOutput("std"),
                    valueBoxOutput("sharpe"),
                    valueBoxOutput("equity")
                ),

                actionBttn(
                    inputId = "button6",
                    label = "Restart",
                    style = "unite",
                    color = "danger"
                )

        )
        ######### end of fourth tab item ###############################################
        ################################################################################
    )
    ##### end of all tab items #####################################################
    ################################################################################
)
# End of the dashboardBody######################################################
################################################################################

ui <- dashboardPage(header, sidebar, body,tags$head(
    tags$style(
        HTML(".shiny-notification {
                    height: 100px;
                    width: 800px;
                    position:fixed;
                    top: calc(50% - 50px);;
                    left: calc(50% - 400px);;
                    }
                    "
        )))

)
