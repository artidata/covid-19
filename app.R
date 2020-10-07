library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(ggplot2)
library(scales)
library(tidyr)
library(readr)
library(dplyr)
ui <- dashboardPage(
    
    dashboardHeader(title="COVID-19",
                    tags$li(a(href="http://apps.artidata.io",
                              icon("home"),
                              title="apps.artidata.io",
                              style="cursor: pointer;"),
                            class="dropdown")),
    
    dashboardSidebar(sidebarMenu(id="tab",width = 240,
        menuItem("Timeline", tabName = "timeline"),
        
        conditionalPanel(
            condition = "input.tab == 'timeline'",
            
            selectInput(
                inputId = "place",
                label = "Country/Place",
                choices = readRDS("database/unique place.rds"),
                selected = "World"),
            
                
            radioButtons(
                inputId = "attr1",
                label = NULL,
                choices = c("new addition","cumulative"),
                selected = "new addition",
                inline = T),
            
            checkboxGroupInput(
                inputId = "attr2", 
                label = "Select attribute(s):",
                choices = c("confirmed","deaths","recovered","tests"),
                selected = c("confirmed","deaths","recovered","tests")),
            
            dateRangeInput(
                inputId = "daterange", 
                label = "Date range:",
                start = "2020-01-01",
                end   = readRDS("database/download date.rds")-1,
                min = "2020-01-01",
                max = readRDS("database/download date.rds")-1,
                startview = "year"),
            
            # checkboxInput(
            #     inputId = "manualSize",
            #     label = "Manual Size",
            #     value =  F),
            # 
            # conditionalPanel(
            #     condition = "input.manualSize == true",
            #     
            #     numericInput(
            #         inputId = "height",
            #         label = "Height (px)",
            #         value = 400,
            #         width = "50%"),
            #     
            #     numericInput(
            #         inputId = "width",
            #         label = "Width (px)",
            #         value = 800,
            #         width = "50%")),
            
            fluidRow(
                column(
                    width = 11, #seems more central, although 12 is the default
                    align = "center",
                    actionButton(
                        inputId = "execute",
                        color = "green",
                        label = "RE-PLOT",
                        style = "color: #fff; background-color: #00a65a; border-color: #00a65a;"),
                    
                    tags$head(tags$style(HTML("#execute:hover{background-color:#008d4c !important;}"))))))
        #,menuItem("Widgets", icon = icon("th"), tabName = "widgets",badgeLabel = "new", badgeColor = "green")
    )),
    dashboardBody(
        
        tags$head(tags$script('
                                var widthBox = 0;
                                $(document).on("shiny:connected", function(e) {
                                    widthBox = document.getElementById("box").offsetWidth-52;
                                    Shiny.onInputChange("widthBox", widthBox);
                                });
                                $(window).resize(function(e) {
                                    newWidthBox =  document.getElementById("box").offsetWidth-52;
                                    if(widthBox != newWidthBox){
                                      Shiny.onInputChange("widthBox", newWidthBox);
                                    }
                                });
                            ')),
        tabItems(
            tabItem(tabName = "timeline",
                    fluidRow(
                        
                        uiOutput(outputId  = "box")))#,
                    # box(width = 12,
                    #     collapsible = F,
                    #     solidHeader = F,
                    #     tableOutput(outputId = "foo")))
            #,
            
            # 
            # tabItem(tabName = "widgets",
            #         h2("Widgets tab content"),
            #         textOutput("tabs"))
        )
    ))

server <- function(input,output,session) {
    observeEvent(input$tab, {
        showNotification(HTML(paste0("Data downloaded from covid19datahub.io </br>at ",readRDS("database/download time.rds"))),
                         duration=NULL)
        showNotification("Update parameter(s) on the sidebar such as Country/Place and click RE-PLOT button to generate a new plot output.",
                         duration=10,type="message")})
    sizePlot <- reactive({
        req(input$widthBox)
        # if(input$manualSize){
        #     list(width = input$width, height = input$height)  
        # } else{
            list(width = input$widthBox, height = input$widthBox/2)
        #}
    })
    
    sizePlotD <- sizePlot %>% debounce(500)
    
    plotCurrent <- reactive({
        
        input$execute
        isolate({
            df <- read_csv(paste0("database/",input$attr1,"/",input$place,".csv"))
            df <- pivot_longer(df,!date,names_to="variable",values_to = "value")
            df <- filter(df,variable%in% input$attr2 & date>=input$daterange[1] & date<=input$daterange[2])
            plot <- ggplot(data=df)+
                geom_col(aes(x=date,y=value,fill=variable),width=1,color="white")+
                facet_grid("variable",scales="free_y")+
                theme_light()+
                labs(title=paste0(input$place,"'s COVID-19 daily ",ifelse(input$attr1=="new addition","new","cumulative") ," cases (",input$daterange[1]," to ",input$daterange[2],")"),fill=NULL,x=NULL,y=NULL)+
                scale_fill_manual(values=c("tests"="#009CF1",
                                           "confirmed"="#F05E84",
                                           "recovered"="#39A700",
                                           "deaths"="#919191"))+
                scale_y_continuous(labels = comma)+
                scale_x_date(breaks="months",date_labels = "%B-%d")+
                theme(legend.position="top",
                      strip.background = element_blank(),
                      strip.text.x = element_blank(),
                      axis.text = element_text(size=rel(1.1)),
                      legend.text = element_text(size=rel(1.1)),
                      title = element_text(size=rel(1.2)))
            return(plot)
        })
    })
    
    output$box <- renderUI({
        # req(input$manualSize)
        # validate(need(is.na(input$manualSize)==F,message = "LOADING"))
        output$plot <- renderPlot({
            plotCurrent()})
        
        tagList(box(
            solidHeader = F,
            collapsible = F,
            width = 12,
            withSpinner(
                plotOutput(
                    outputId  = "plot",
                    width = sizePlotD()$width,
                    height = sizePlotD()$height),
                color = getOption("spinner.color", default = "#3c8dbc"))))})
}
shinyApp(ui, server)
