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


shinyServer(function(input, output, session) {

############################ Risk Preference ###################################
################################################################################


### Histogram Simulation ######################################################

sim <- reactiveValues() # reactive to store all reactive variables
sim$resetindicator <- 0 # used to change button labels
sim$resetindicator1 <- 1 # used to change the play btn design
sim$numb <- c()
sim$data <- c()
sim$terminal_wealth <- c()
diffusion <- c(0.01, 0.02, 0.05, 0.1, 0.15, 0.25, 0.3)
drift <- c(0.001, 0.005, 0.015, 0.02, 0.03, 0.06, 0.07)
draws <- 1000
speed <- seq(1000, 100, length.out = 5)

### dynamic reset button label #################################################

output$resetbutton <- renderUI({
    actionBttn(
        inputId = "reset",
        label = NULL,
        style = "material-circle",
        color = "danger",
        icon = icon("stop")
    )
})


output$rpref <- renderUI({
    if (sim$resetindicator == 0) {
        ID <- "rpref"
    } else {
        ID <- "uselessID"
    }

    sliderInput(inputId = ID, "Estimate your risk preference:",
                value = input$rpref, min = 1, max = 7, step = 1)

})

output$inv_horizon <- renderUI({
    if (sim$resetindicator == 0) {
        ID1 <- "inv_horizon"
    } else {
        ID1 <- "uselessID1"
    }

    if (is.null(input$inv_horizon)) {
        val <- 10
    } else {
        val <- input$inv_horizon
    }

    knobInput(
        inputId = ID1,
        label = "What is your investment horizon?",
        value = val,
        min = 1,
        max = 30,
        displayPrevious = TRUE,
        lineCap = "round",
        fgColor = "#428BCA",
        inputColor = "#428BCA",
        width = 150,
        height = 150)
})


output$initial_wealth <- renderUI({
    if (sim$resetindicator == 0) {
        ID2 <- "initial_wealth"
    } else {
        ID2 <- "uselessID2"
    }

    if (is.null(input$initial_wealth) || is.na(input$initial_wealth)) {
        val1 <- 10000
    } else if (input$initial_wealth == 0) {
        val1 <- 10000
    } else if (input$initial_wealth < 0) {
        val1 <- abs(input$initial_wealth)
    } else if (input$initial_wealth > 0) {
        val1 <- input$initial_wealth
    }

    numericInput(inputId = ID2,
                 "How much money would you like to invest?:",
                 value = val1,
                 min = 0,
                 max = NA)
})

output$play <- renderUI({
    if (sim$resetindicator == 0 || sim$resetindicator1 == 1) {
        ID3 <- "play"
        design <- "primary"
        icn <- icon("play")
    } else if (sim$resetindicator1 == 0) {
        ID3 <- "stop"
        design <- "warning"
        icn <- icon("pause")
    }

    actionBttn(
        inputId = ID3,
        label = NULL,
        style = "material-circle",
        color = design,
        icon = icn
    )
})

observeEvent(input$stop,{
    sim$resetindicator1 <- 1
})

observeEvent(input$play,{
    sim$resetindicator1 <- 0
})

### dynamic start button label #################################################

output$startbutton <- renderUI({
    actionBttn(
        inputId = "nextdraw",
        label = NULL,
        style = "material-circle",
        color = "success",
        icon = icon("step-forward")
    )
})

### Random draw function for individual draws ##################################

rand_draw <- function() {
    req(input$initial_wealth)
    req(input$rpref)
    req(input$inv_horizon)

    sim$resetindicator <- 1 # change button label

    sim$numb <- input$initial_wealth * exp((drift[input$rpref] - (1 / 2) *
                                                (diffusion[input$rpref]) ^ 2)*
                                               input$inv_horizon +
                                               diffusion[input$rpref] *
                                               sqrt(input$inv_horizon)*
                                               rnorm(1))
    sim$data <<- c(sim$data, sim$numb)

    sim$data
}

### when next-draw button is pressed ###########################################

observeEvent(input$nextdraw,{
    rand_draw()
})

session1 <- reactiveValues()
session1$timer <- reactiveTimer(Inf)


observeEvent(
    input$play,
    {session1$timer <- reactiveTimer(speed[which(c("Very Slow",
                                                   "Slow",
                                                   "Moderate",
                                                   "Fast",
                                                   "Very Fast") ==
                                                     input$speed)]) # 100
    observeEvent(session1$timer(), {
        rand_draw()
    })
######### end of subevent ######################################################
    })


observeEvent(input$stop, {
    session1$timer<- reactiveTimer(Inf)
})


## when reset button is pressed, set everything to original values plus set seed
observeEvent(input$reset, {

    sim$resetindicator <- 0
    sim$numb <- c(0)
    sim$data <- c(0)
})


#### main plot output - histogram first tab ####################################
################################################################################

output$distPlot <- renderPlot({

    if (sum(sim$data) == 0) {
        return() # no plot if reset everything was reset

    } else if (length(sim$data) > 300) {
        # automatically reset after # draws to exit
        sim$resetindicator <- 0
        sim$numb <- c(0)
        sim$data <- c(0)
    }

    hist(sim$data[sim$data < input$initial_wealth * input$rpref],
         breaks = seq(from = 0, to = (input$initial_wealth *
                                          input$rpref),
                      by = (input$initial_wealth * input$rpref) / 30),
         xlim = c(0, input$initial_wealth * input$rpref),
         ylim = c(0, 200),
         xlab = "Terminal Wealth",
         main = "Potential Evolvement of Wealth")

    grid()

    points(
        x = input$initial_wealth,
        y = 0,
        pch = 24,
        bg = "grey",
        cex = 2
    )

######## include a vertical line indicating the mean ###########################
    abline(
        v = mean(sim$data),
        col = "blue",
        lwd = 2,
        lty = 2
    )

######## include a vertical line indicating the 90% percentile #################
    abline(
        v = sim$data[order(sim$data)[length(sim$data) * 0.9]],
        col = "green",
        lwd = 2,
        lty = 2
    )

######## inlude a vertical line indicating the 10% percentile ##################
    abline(
        v = sim$data[order(sim$data)[length(sim$data) * 0.1]],
        col = "red",
        lwd = 2,
        lty = 2
    )

    legend(
        "topright",
        legend = c(
            "90 out of 100 boundary",
            "10 out of 100 boundary",
            "Average Terminal Wealth",
            "Initial Investment"
        ),

        col = c("green",
                "red",
                "blue",
                "grey"),

        lty = c(2, 2, 2, NA),
        pch = c(NA, NA, NA, 24),
        box.lty = 0,
        cex = 1.2
    )
})

### end of main plot - histogram first tab #####################################
################################################################################

### start of main plot - histogram second tab ##################################
################################################################################

output$distPlotFinish <- renderPlot({
    sim$terminal_wealth <- input$initial_wealth *
        exp((drift[input$rpref2] - (1 / 2) *
                 (diffusion[input$rpref2]) ^ 2) *
                input$inv_horizon + diffusion[input$rpref2] *
                sqrt(input$inv_horizon) * rnorm(1:draws)
        )

    hist(
        sim$terminal_wealth[sim$terminal_wealth >= 0 &
                                sim$terminal_wealth <
                                input$initial_wealth * 7],
        breaks = seq(
            from = 0,
            to = (input$initial_wealth * 7),
            by = (input$initial_wealth * 7) / 30
        ),
        xlim = c(0, input$initial_wealth * 7),
        xlab = "Terminal Wealth",
        main = "Potential Evolvement of Wealth"
    )
    grid()

    points(
        x = input$initial_wealth,
        y = 0,
        pch = 24,
        bg = "grey",
        cex = 2
    )

######## include a vertical line indicating the mean ###########################
    abline(
        v = mean(sim$terminal_wealth),
        col = "blue",
        lwd = 2,
        lty = 2
    )

######## include a vertical line indicating the 90% percentile #################
    abline(
        v = sim$terminal_wealth[order(sim$terminal_wealth)[draws * 0.9]],
        col = "green",
        lwd = 2,
        lty = 2
    )

######## include a vertical line indicating the 10% percentile #################
    abline(
        v = sim$terminal_wealth[order(sim$terminal_wealth)[draws * 0.1]],
        col = "red",
        lwd = 2,
        lty = 2
    )

    legend(
        "topright",
        legend = c(
            "90 out of 100 boundary",
            "10 out of 100 boundary",
            "Average Terminal Wealth",
            "Initial Investment"
        ),

        col = c("green", "red", "blue", "grey"),
        lty = c(2, 2, 2, NA),
        pch = c(NA, NA, NA, 24),
        box.lty = 0,
        cex = 1.2
    )
})
############ end of mainplot - histogram second tab ############################
################################################################################

# Set the selected input of the first slider equal to the second et vice versa
observe({
    updateSliderInput(session, "rpref2", value = input$rpref)
})

observe({
    updateSliderInput(session, "rpref", value = input$rpref2)
})


############ Country and Industry Subsetting ###################################
################################################################################

######## To make the code better readable, the webscrapping process is placed #
######## in a seperate file ###################################################

source("robodata.R")
#load("staticdata/datas.RData")

####### The output file of the webscraping script is called "OVR" and contains #
####### all available information in one data frame. This is split up into the #
####### dates, the commodity index, the two debt indices to be left with the ###
####### different equity indices. ##############################################

dates <- ovr[, 1]
commodities <- ovr[, 47]
longbond <- ovr[, 48]
shortbond <- ovr[, 49]
benchmark <- ovr[, 50]
data <- ovr[, -c(1, 47:50)]

####### in order to make the data frame subsettable, it must be in an reactive #
####### enviornment. ###########################################################

makeReactiveBinding("data")


####### Actual subsetting, where the data is then used for the PF building #####
################################################################################

####### Get webscraped Data (the ovr file from the scrapping script) ###########

####### use the data frame in a reactive enviornment ###########################

newData <- reactive({
######### Again, the data has to be used within and outside the reactive #######
######### enviornment. #########################################################
    data <- ovr[, -c(1, 47:50)]

######### Subsetting by region - the user selects the region he or she does not
######### want to be invested in ###############################################

    if (!("North America" %in% input$mymap_groups)) {
        data <- data[ , -which(names(data) %in%
                                   grep("US", names(data), value = TRUE))]
    }

    if (!("Europe" %in% input$mymap_groups)) {
        data <- data[ , -which(names(data) %in%
                                   grep("EU", names(data), value = TRUE))]
    }

    if (!("Asia" %in% input$mymap_groups)) {
        data <- data[ , -which(names(data) %in%
                                   grep("AS", names(data), value = TRUE))]
    }

    if (!("Africa" %in% input$mymap_groups)) {
        data <- data[ , -which(names(data) %in%
                                   grep("Africa", names(data), value = TRUE))]
    }

    if (!("Australia" %in% input$mymap_groups)) {
        data <- data[ , -which(names(data) %in%
                                   grep("Australia", names(data),
                                        value = TRUE))]
    }

    if (!("Latin America" %in% input$mymap_groups)) {
        data <- data[ , -which(names(data) %in%
                                   grep("Latinamerica", names(data),
                                        value = TRUE))]
    }

    ######### Subsetting by industry - the user chooses the indutries he or she ####
    ######### does not want to invest in ###########################################

    if (("North America" %in% input$mymap_groups) ||
        ("Europe" %in% input$mymap_groups) ||
        ("Asia" %in% input$mymap_groups)) {

        if ("banks" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                       grep("banks", names(data), value = TRUE))]
        }

        if ("resources" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                       grep("resources", names(data),
                                            value = TRUE))]
        }

        if ("chemicals" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                       grep("chemicals", names(data),
                                            value = TRUE))]
        }

        if ("construction" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                       grep("construction", names(data),
                                            value = TRUE))]
        }

        if ("financials" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                       grep("financials", names(data),
                                            value = TRUE))]
        }

        if ("food" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                       grep("food", names(data), value = TRUE))]
        }

        if ("health" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                       grep("health", names(data), value = TRUE))]
        }

        if ("industrial" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                       grep("industrial", names(data),
                                            value = TRUE))]
        }

        if ("insurance" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                       grep("insurance", names(data),
                                            value = TRUE))]
        }

        if ("energy" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                       grep("energy", names(data), value = TRUE))]
        }

        if ("personal" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                       grep("personal", names(data),
                                            value = TRUE))]
        }

        if ("tech" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                       grep("tech", names(data), value = TRUE))]
        }

        if ("telecom" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                       grep("telecom", names(data),
                                            value = TRUE))]
        }


        if ("utilities" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                       grep("utilities", names(data),
                                            value = TRUE))]
        }
    }

    data

    ######### End of subsetting, where the data is used for PF building ############
    ################################################################################
})


####### Initiation of the map, where the user can (de)select regions ###########
################################################################################

####### Read multiple shape files with standardized names ######################
####### all available countries are grouped by continent #######################

region <- c("africa",
            "antarctica",
            "asia", "europe",
            "northamerica",
            "oceania",
            "latinamerica")

groups <- c("Africa",
            "Antarctica",
            "Asia",
            "Europe",
            "North America",
            "Oceania",
            "Latin America")

colors <- c("red", "blue", "green", "yellow",
            "purple", "turquoise", "grey")

for (i in region) {
    filestest.i <- geojson_read(as.character(
        paste(getwd(),
              "Map",
              paste(i,
                    "geo.json",
                    sep = "."),
              sep = "/")),
        what = "sp")

    assign(as.character(paste("files", i, sep = ".")), filestest.i)
}

####### remove the "dummy"-file ################################################
rm(filestest.i)


####### initiate the map built with leaflet ####################################
################################################################################

foundmap <- leaflet() %>%

    setView(lng = 0, lat = 30, zoom = 1.5) %>%

    addProviderTiles(providers$Stamen.TonerLite,
                     options = providerTileOptions(noWrap = TRUE))


####### add multiple layers to combine the single polygons #####################

for (reg.N in 1:length(region)) {
    reg <- region[reg.N] # gives the region "code"
    tmp <- get(paste("files", reg, sep = ".")) #gives the file name

    foundmap <- foundmap %>%
        addPolygons(data = tmp,
                    fillColor = colors[reg.N],
                    color = "#000000",
                    opacity = 1,
                    fillOpacity = 0.7,
                    dashArray = "3",
                    stroke = TRUE,
                    weight = 1.5,
                    smoothFactor = 0.2,
                    label = paste(groups[reg.N]),
                    group = paste(groups[reg.N])
        )
}

####### set up layer controls to make regions selectable #######################

foundmap <- foundmap %>%
    addLayersControl(overlayGroups = groups,
                     options = layersControlOptions(collapsed = FALSE)) %>% hideGroup("Antarctica")



####### integrate the map into shiny ###########################################

output$mymap <- renderLeaflet({foundmap})

##### Dataspliting and optimizing###############################################
################################################################################

datasplit <- function(subdata, updateProgress = NULL){
    if (ncol(subdata) > 15) {

        flor <- floor(ncol(subdata) / 15)
        remaining <- ncol(subdata) - 15 * flor
        lastdata <- flor + 1

        for (n in 1:flor){

            data.n  <- subdata[((n - 1) * 15 + 1):(n * 15)]
            assign(as.character(paste("data", as.character(n), sep = "")), data.n)
        }

        data.lastdata <- subdata[(flor * 15):ncol(subdata)]
        assign(as.character(paste("data", as.character((flor + 1)), sep = "")),
               data.lastdata)

        finaldata <- data.frame(matrix(nrow = nrow(subdata)))[, -1]

        new_row <- data.frame(x = rnorm(1), y = rnorm(1))

        for (n in 1:(flor + 1)){

            dataopt<- minvarpf(get(paste("data", n, sep = "")))
            assign(as.character(paste("dataopt", as.character(n),
                                      sep = "")), dataopt)

            if (is.function(updateProgress)) {
                text <- "Please don't turn off your Computer"
                updateProgress(detail = text)
            }


            finaldata <- cbind(finaldata, dataopt)
        }
        return(finaldata)
    } else {
        finaldata <- subdata
        return(finaldata)
    }
    ### End of Dataspliting function #################################
}

### call the portfolios according to the user's input ##########################

################################################################################

output$ourPF <- renderPlot({

    # Create the Progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Calculating Optimal Portfolio", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())

    updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
            value <- progress$getValue()
            value <- value + (progress$getMax() - value) / 3
        }
        progress$set(value = value, detail = detail)
    }

    ##### short bond only ##########################################################

    if (input$rpref2 == 1 && input$inv_horizon <= 5) {

        portfoliofinal <<- indexpf(as.data.frame(shortbond))
        equityinvestment <<- 0

        portfolioplot <- cbind(ovr$Date, portfoliofinal)
        names(portfolioplot) <- c("Date", "Portfolio")
        portfolioplot$Date <- strptime(as.character(portfolioplot$Date),
                                       "%d/%m/%Y")
        portfolioplot$Date <- as.Date(format(portfolioplot$Date, "%Y-%m-%d"))
        portfolioplot <- cbind(portfolioplot, benchmark)

        plotfinal <- ggplot() +
            geom_line(data = portfolioplot, aes(
                Date, Portfolio, group = 1, colour = "Your Portfolio"), size = 1) +
            geom_line(data = portfolioplot, aes(
                Date, benchmark, group = 1,
                colour = "MSCI World Benchmark"), size = 1) +
            labs(x = "Year", y = "Portfolio Developement",
                 title = "Short Bond Portfolio") +
            scale_x_date(date_breaks = "2 years") +
            scale_colour_manual("", values = c("Your Portfolio" = "blue",
                                               "MSCI World Benchmark" = "grey")) +
            theme(legend.position=c(.9, .1))
        theme_minimal()
    }

    ##### long bond only ###########################################################

    if ((input$rpref2 == 2 && input$inv_horizon <= 5) ||
        (input$rpref2 == 1 && input$inv_horizon > 5 &&
         input$inv_horizon <= 10)) {

        portfoliofinal <<- indexpf(as.data.frame(longbond))
        equityinvestment <<- 0

        portfolioplot <- cbind(ovr$Date, portfoliofinal)
        names(portfolioplot) <- c("Date", "Portfolio")
        portfolioplot$Date <- strptime(as.character(portfolioplot$Date),
                                       "%d/%m/%Y")
        portfolioplot$Date <- as.Date(format(portfolioplot$Date, "%Y-%m-%d"))
        portfolioplot <- cbind(portfolioplot, benchmark)

        plotfinal <- ggplot() +
            geom_line(data = portfolioplot, aes(
                Date, Portfolio, group = 1, colour = "Your Portfolio"), size = 1) +
            geom_line(data = portfolioplot, aes(
                Date, benchmark, group = 1,
                colour = "MSCI World Benchmark"), size = 1) +
            labs(x = "Year", y = "Portfolio Developement",
                 title = "Long Bond Portfolio") +
            scale_x_date(date_breaks = "2 years") +
            scale_colour_manual("", values = c("Your Portfolio" = "blue",
                                               "MSCI World Benchmark" = "grey")) +
            theme(legend.position=c(.9, .1))
        theme_minimal()
    }



    ##### minimum variance PF ######################################################

    if ((input$rpref2 == 3 && input$inv_horizon <= 5) ||
        #redundant but included so that the number of conitions = number of PFs
        (input$rpref2 == 3 && input$inv_horizon > 5 &&
         input$inv_horizon <= 10) ||
        (input$rpref2 == 2 && input$inv_horizon > 5)) {

        ####### split the required input df into sub-df's to make them optimizable #####
        finaldata <- datasplit(newData(), updateProgress)
        portfoliofinal <<- minvarpf(finaldata)
        equityinvestment <<- 1

        ####### include the performance plot in Shiny ##################################
        portfolioplot <- cbind(ovr$Date, portfoliofinal)
        names(portfolioplot) <- c("Date", "Portfolio")
        portfolioplot$Date <- strptime(as.character(portfolioplot$Date),
                                       "%d/%m/%Y")
        portfolioplot$Date <- as.Date(format(portfolioplot$Date, "%Y-%m-%d"))
        portfolioplot <- cbind(portfolioplot, benchmark)

        plotfinal <- ggplot() +
            geom_line(data = portfolioplot, aes(
                Date, Portfolio, group = 1, colour = "Your Portfolio"), size = 1) +
            geom_line(data = portfolioplot, aes(
                Date, benchmark, group = 1,
                colour = "MSCI World Benchmark"), size = 1) +
            labs(x = "Year", y = "Portfolio Developement",
                 title = "Minimum Variance Portfolio") +
            scale_x_date(date_breaks = "2 years") +
            scale_colour_manual("", values = c("Your Portfolio" = "blue",
                                               "MSCI World Benchmark" = "grey")) +
            theme(legend.position=c(.9, .1))
        theme_minimal()

    }

    ###### Equity + longbond overweight bond #######################################

    if (input$rpref2 == 1 && input$inv_horizon > 10) {

        finaldata <- datasplit(newData(), updateProgress)

        finalpf <- optimpf(finaldata)

        longbondindexed <- indexpf(as.data.frame(longbond))
        portfoliofinal <<- equityanddebtpf(finalpf, longbondindexed, 0.2)


        portfolioplot <- cbind(ovr$Date,portfoliofinal)
        names(portfolioplot) <- c("Date","Portfolio")
        portfolioplot$Date <- strptime(as.character(portfolioplot$Date),
                                       "%d/%m/%Y")
        portfolioplot$Date <- as.Date(format(portfolioplot$Date, "%Y-%m-%d"))
        portfolioplot <- cbind(portfolioplot, benchmark)

        plotfinal <- ggplot() +
            geom_line(data = portfolioplot, aes(
                Date, Portfolio, group = 1, colour = "Your Portfolio"), size = 1) +
            geom_line(data = portfolioplot, aes(
                Date, benchmark, group = 1,
                colour = "MSCI World Benchmark"), size = 1) +
            labs(x = "Year", y = "Portfolio Developement",
                 title = "Sharpe Ratio optimized Equity
           Portfolio with 80% Longterm Debt") +
            scale_x_date(date_breaks = "2 years")+
            scale_colour_manual("", values = c("Your Portfolio" = "blue",
                                               "MSCI World Benchmark" = "grey")) +
            theme(legend.position = c(.9, .1))
        theme_minimal()

        equityinvestment <<- 0.2
    }

    ############################ risk parity #######################################
    if ((input$rpref2 == 4 && input$inv_horizon <= 5) ||
        (input$rpref2 == 5 && input$inv_horizon <= 5) ||
        (input$rpref2 == 4 && input$inv_horizon > 5 && input$inv_horizon <= 10) ||
        (input$rpref2 == 5 && input$inv_horizon > 5 && input$inv_horizon <= 10) ||
        (input$rpref2 == 3 && input$inv_horizon > 10) ||
        (input$rpref2 == 4 && input$inv_horizon > 10)) {

        finaldata <- datasplit(newData(), updateProgress)

        finalpf <- optimpf(finaldata)

        longbonddf <- as.data.frame(longbond)
        commoditydf <- as.data.frame(commodities)

        portfoliofinal <<- riskparitypf(finalpf, longbonddf, commoditydf)
        equityinvestment <<- as.numeric(portfoliofinal[2])
        portfoliofinal <<- as.data.frame(portfoliofinal[1])

        portfolioplot <- cbind(ovr$Date, portfoliofinal)
        names(portfolioplot) <- c("Date", "Portfolio")
        portfolioplot$Date <- strptime(as.character(portfolioplot$Date),
                                       "%d/%m/%Y")
        portfolioplot$Date <- as.Date(format(portfolioplot$Date, "%Y-%m-%d"))
        portfolioplot <- cbind(portfolioplot, benchmark)

        plotfinal <- ggplot() +
            geom_line(data = portfolioplot, aes(
                Date, Portfolio, group = 1, colour = "Your Portfolio"), size = 1) +
            geom_line(data = portfolioplot, aes(
                Date, benchmark, group = 1,
                colour = "MSCI World Benchmark"), size = 1) +
            labs(x = "Year", y = "Portfolio Developement",
                 title = "Risk Parity Portfolio with
           Sharpe Ratio optimized Equity Part") +
            scale_x_date(date_breaks = "2 years") +
            scale_colour_manual("", values = c("Your Portfolio" = "blue",
                                               "MSCI World Benchmark" = "grey")) +
            theme(legend.position=c(.9, .1))
        theme_minimal()

    }

    ########################### equity + longbond overweight equity ################
    if ((input$rpref2 == 6 && input$inv_horizon <= 5) ||
        (input$rpref2 == 6 && input$inv_horizon > 5 &&
         input$inv_horizon <= 10) ||
        (input$rpref2 == 5 && input$inv_horizon > 10)) {

        finaldata <- datasplit(newData(), updateProgress)

        finalpf <- optimpf(finaldata)

        longbondindexed <- indexpf(as.data.frame(longbond))
        portfoliofinal <<- equityanddebtpf(finalpf, longbondindexed, 0.8)
        equityinvestment <<- 0.8

        portfolioplot <- cbind(ovr$Date, portfoliofinal)
        names(portfolioplot) <- c("Date", "Portfolio")
        portfolioplot$Date <- strptime(as.character(portfolioplot$Date),
                                       "%d/%m/%Y")
        portfolioplot$Date <- as.Date(format(portfolioplot$Date,
                                             "%Y-%m-%d"))
        portfolioplot <- cbind(portfolioplot, benchmark)

        plotfinal <- ggplot() +
            geom_line(data = portfolioplot, aes(
                Date, Portfolio, group = 1, colour = "Your Portfolio"), size = 1) +
            geom_line(data = portfolioplot, aes(
                Date, benchmark, group = 1,
                colour = "MSCI World Benchmark"), size = 1) +
            labs(x = "Year", y = "Portfolio Developement",
                 title = "Sharpe Ratio optimized Equity
           Portfolio with 20% Longterm Debt") +
            scale_x_date(date_breaks = "2 years") +
            scale_colour_manual("", values = c("Your Portfolio" = "blue",
                                               "MSCI World Benchmark" = "grey")) +
            theme(legend.position=c(.9, .1))
        theme_minimal()

    }

    ################################ Pure Equity? ##################################
    if ((input$rpref2 == 6 && input$inv_horizon > 10) ||
        (input$rpref2 == 7 && input$inv_horizon <= 5) ||
        (input$rpref2 == 7 && input$inv_horizon > 5 && input$inv_horizon <= 10) ||
        (input$rpref2 == 7 && input$inv_horizon > 10)) {

        finaldata <- datasplit(newData(),updateProgress)

        portfoliofinal <<- optimpf(finaldata)
        equityinvestment <<- 1

        portfolioplot <- cbind(ovr$Date, portfoliofinal)
        names(portfolioplot) <- c("Date", "Portfolio")
        portfolioplot$Date <- strptime(as.character(portfolioplot$Date),
                                       "%d/%m/%Y")
        portfolioplot$Date <- as.Date(format(portfolioplot$Date,
                                             "%Y-%m-%d"))
        portfolioplot <- cbind(portfolioplot, benchmark)

        plotfinal <- ggplot()+
            geom_line(data = portfolioplot, aes(
                Date, Portfolio, group = 1, colour = "Your Portfolio"), size = 1) +
            geom_line(data = portfolioplot, aes(
                Date, benchmark, group = 1,
                colour = "MSCI World Benchmark"), size = 1) +
            labs(x = "Year", y = "Portfolio Developement",
                 title = "Sharpe Ratio optimized 100% Equity Portfolio") +
            scale_x_date(date_breaks = "2 years") +
            scale_colour_manual("", values = c("Your Portfolio" = "blue",
                                               "MSCI World Benchmark" = "grey")) +
            theme(legend.position=c(.9, .1))
        theme_minimal()
    }
    plotfinal
})



### PDF Download Handler - option for the user to downloas her personal report #

output$downloadReport <- downloadHandler(

    filename <- function() {
        paste("PF-Factsheet",
              sep = ".",
              switch(input$format,
                     PDF = "pdf",
                     HTML = "html",
                     Word = "docx")
        )
    },

    content <- function(file) {
        src <- normalizePath("report.Rmd")

        ####### temporarily switch to the temp dir, in case you do not have write ######
        ####### permission to the current working directory ############################

        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src,
                  "report.Rmd",
                  overwrite = TRUE)


        out <- render("report.Rmd",
                      switch(input$format,
                             PDF = pdf_document(),
                             HTML = html_document(),
                             Word = word_document())
        )

        file.rename(out, file)

    }
)


#### ValueBoxes ################################################################
################################################################################

### Boxes for Final Output Page ################################################
output$expectedValue <- renderValueBox({
    valueBox(
        paste0(round(((1 + (averagereturn(portfoliofinal))) ^
                          input$inv_horizon) * input$initial_wealth), "$"),
        paste0("Expected wealth after ", input$inv_horizon, " years" ),
        icon = icon("hand-holding-usd"),
        color = "blue"
    )
})

output$yearlygain <- renderValueBox({
    valueBox(
        paste0(round((averagereturn(portfoliofinal) * 100)), "%"),
        "Expected yearly return",
        icon = icon("chart-line"),
        color = "green"
    )
})

output$maxdrawdown <- renderValueBox({
    valueBox(
        paste0(round((maxdrawdown(portfoliofinal) * 100)), "%"),
        "Maximum Drawdown",
        icon = icon("angle-double-down"),
        color = "red"
    )
})

output$equity <- renderValueBox({
    valueBox(
        paste0(round(equityinvestment * 100, 2), "%"),
        "Invested in Equity",
        icon = icon("balance-scale"),
        color = "red"
    )
})

output$sharpe <- renderValueBox({
    valueBox(
        round((averagereturn(portfoliofinal) / yearlystd(portfoliofinal)), 2),
        "Sharpe Ratio",
        icon = icon("calculator"),
        color = "green"
    )
})

output$std <- renderValueBox({
    valueBox(
        paste0(round((yearlystd(portfoliofinal) * 100)), "%"),
        "Standard Deviation",
        icon = icon("square-root-alt"),
        color = "blue"
    )
})

################################################################################


output$horizonBox <- renderValueBox({
    valueBox(
        paste0(input$inv_horizon, " years"),
        "Investment Horizon",
        icon = icon("hourglass-half"),
        color = "blue"
    )
})

output$returnBox <- renderValueBox({
    valueBox(
        paste0(drift[input$rpref] * 100, "%"),
        "Return",
        icon = icon("chart-line"),
        color = "green"
    )
})

output$stdBox <- renderValueBox({
    valueBox(
        paste0(diffusion[input$rpref] * 100, "%"),
        "Standard Deviation",
        icon = icon("square-root-alt"),
        color = "red"
    )
})

output$avgBox <- renderValueBox({
    if (sum(sim$data) == 0) {
        valueBox(
            paste0(NA, "$"),
            "Average Value",
            icon = icon("hand-holding-usd"),
            color = "blue"
        )

    } else {
        valueBox(
            paste0(round(mean(sim$data)), "$"),
            "Average Value",
            icon = icon("hand-holding-usd"),
            color = "blue"
        )
    }
})

output$uplimBox <- renderValueBox({
    if (sum(sim$data) == 0) {
        valueBox(
            paste0(NA, "$"),
            "90% Limit Profit",
            icon = icon("greater-than"),
            color = "green"
        )

    } else {
        valueBox(
            paste0(round(sim$data[order(sim$data)[length(sim$data) * 0.9]] -
                             input$initial_wealth), "$"),
            "90% Limit Profit",
            icon = icon("greater-than"),
            color = "green"
        )
    }
})

output$lowlimBox <- renderValueBox({
    if (sum(sim$data) == 0) {
        valueBox(
            paste0(NA, "$"),
            "10% Limit Loss",
            icon = icon("less-than"),
            color = "red"
        )

    } else {
        valueBox(
            paste0(round(sim$data[order(sim$data)[length(sim$data) * 0.1]] -
                             input$initial_wealth), "$"),
            "10% Limit Loss",
            icon = icon("less-than"),
            color = "red"
        )
    }
})

output$horizonBox1 <- renderValueBox({
    valueBox(
        paste0(input$inv_horizon, " years"),
        "Investment Horizon",
        icon = icon("hourglass-half"),
        color = "blue"
    )
})

output$returnBox1 <- renderValueBox({
    valueBox(
        paste0(drift[input$rpref2] * 100, "%"),
        "Return",
        icon = icon("chart-line"),
        color = "green"
    )
})

output$stdBox1 <- renderValueBox({
    valueBox(
        paste0(diffusion[input$rpref2] * 100, "%"),
        "Standard Deviation",
        icon = icon("square-root-alt"),
        color = "red"
    )
})

output$avgBox1 <- renderValueBox({
    valueBox(
        paste0(round(mean(sim$terminal_wealth)), "$"),
        "Average Value",
        icon = icon("hand-holding-usd"),
        color = "blue"
    )
})

output$uplimBox1 <- renderValueBox({
    valueBox( ############ the following line is supposed to be too long ;-)
        paste0(round(sim$terminal_wealth[order(sim$terminal_wealth)[draws * 0.9]] -
                         input$initial_wealth), "$"),
        "90% Limit Profit",
        icon = icon("greater-than"),
        color = "green"
    )
})

output$lowlimBox1 <- renderValueBox({
    valueBox( ############ the following line is supposed to be too long ;-)
        paste0(round(sim$terminal_wealth[order(sim$terminal_wealth)[draws * 0.1]] -
                         input$initial_wealth), "$"),
        "10% Limit Loss",
        icon = icon("less-than"),
        color = "red"
    )
})

#### Switch Buttons ############################################################
################################################################################

### Switch Tabs with action buttons ############################################

observeEvent(
    input$button1, {
        newtab <- switch(input$tabs, "tab1" = "tab2")
        updateTabItems(session, "tabs", newtab)
    }
)

observeEvent(
    input$button2, {
        newtab <- switch(input$tabs, "tab2" = "tab1")
        updateTabItems(session, "tabs", newtab)
    }
)

observeEvent(
    input$button3, {
        newtab <- switch(input$tabs, "tab2" = "tab3")
        updateTabItems(session, "tabs", newtab)
    }
)

observeEvent(
    input$button4, {
        newtab <- switch(input$tabs, "tab3" = "tab2")
        updateTabItems(session, "tabs", newtab)
    }
)

observeEvent( # include error message, when all regions are deselected #####
              input$button5, {
                  if ("Antarctica" %in% input$mymap_groups) {
                      sendSweetAlert(
                          session = session,
                          title = "No investment in Penguin-Land",
                          text = tags$embed(src = "https://media.giphy.com/media/jxETRYAi2KReel7pqy/giphy.gif",
                                            width = "450px",
                                            height = "500px")
                      )
                  } else if (length(input$mymap_groups) < 3 &&
                             (!("North America" %in% input$mymap_groups) &&
                              !("Europe" %in% input$mymap_groups) &&
                              !("Asia" %in% input$mymap_groups))) {
                      sendSweetAlert(
                          session = session,
                          title = "Error Message",
                          text = "For diversification purposes, please
                          select more inputs.",
                          type = "error"
                      )
                  } else {
                      newtab <- switch(input$tabs, "tab3" = "tab4")
                      updateTabItems(session, "tabs", newtab)
                  }
              }
)

observeEvent(
    input$button6, {
        shinyjs::js$refresh()
        # newtab <- switch(input$tabs, "tab4" = "tab3")
        # updateTabItems(session, "tabs", newtab)
    }
)

# end of server function #######################################################
################################################################################

})
