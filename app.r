library(shiny)
library(ggplot2)
library(plotly)
library(highcharter)
library(tidyr)
library(dplyr)

df = read.csv("Medicaid_Opioid_Prescribing_Geographic_2013.csv",skip = 4)

for (i in 2014:2017) {
  df_one_year = read.csv(paste("Medicaid_Opioid_Prescribing_Geographic_", i, ".csv", sep = ""), 
                         header = TRUE, skip = 4)
  df <- rbind(df, df_one_year)
}

df_rate <- cbind(df[ , 1:4], df[ , 14:19])
df_num <- df[ , 1:9]
rate_long <- gather(df_rate, stat, count, Opioid.Prescribing.Rate:Managed.Care.Long.Acting.Opioid.Prescribing.Rate) 
rate_long$type = "rate"
num_long <- gather(df_num, stat, count, Opioid.Claims:Fee.for.Service.Long.Acting.Opioid.Claims) 
num_long$type = "num"

num_long$count <- as.numeric(gsub(",", "", num_long$count))

long <- rbind(rate_long, num_long)

long$State.Name <- as.character(long$State.Name)

long <- long[, !(names(long) %in% c("State.FIPS", "State.Abbreviation"))]
long_heatmap <- long[long$State.Name!="National" & long$type == "rate", ]
long_heatmap$count <- as.integer(long_heatmap$count)
long_bar <- long[long$State.Name=="National", ]
state_choices <- unique(as.vector(df$State.Name))
# Define UI for random distribution app ----
ui <- navbarPage(
  
  title = "Opioid Claims",
  tabPanel ("All State, Single Year",
      sidebarLayout(
          sidebarPanel(
               # Input: Slider for the number of observations to generate ----
               sliderInput(inputId = "year",
                           "Year of data:",
                           value = 2013,
                           min = 2013,
                           max = 2017,
                           step = 1,
                           ticks = FALSE,
                           sep = ""),
               width = 3
             ),
             
             # Main panel for displaying outputs ----
          mainPanel(
               highchartOutput(outputId = "heatmap", height = "800px")
          )
               
      )
    ),
  tabPanel ("National, All Years",
            sidebarLayout(
              sidebarPanel(
                # Input: Slider for the number of observations to generate ----
                radioButtons("buttons", label = NULL, selected = "num",
                             choiceNames = list("Number of claims", "Presciption rate"),
                             choiceValues = list("num", "rate"))
                ),
              
              # Main panel for displaying outputs ----
              mainPanel(
                highchartOutput(outputId = "bar", height = "800px")
              )
              
            )
  ),
  tabPanel ("Single State, All Years",
            sidebarLayout(
              sidebarPanel(
                selectInput(inputId = "state", label = "Select a state:",
                            choices = as.list(state_choices))
               ),
                
                # Main panel for displaying outputs ----
                mainPanel(
                  plotlyOutput("lines", height="800px")
                )
                
              )
  )
)

tooltip <- JS("function(){
                  return this.series.yAxis.categories[this.point.y] + ', ' +
                         this.series.xAxis.categories[this.point.x] + ': <b>' +
                         Highcharts.numberFormat(this.point.count, 2)+' percent';
               ; }")
# Define server logic for random distribution app ----
server <- function(input, output) {
  one_year <- reactive({
    filter(long_heatmap, Year == input$year)
  })
  
  output$heatmap <- renderHighchart({
    hc <- hchart(long_heatmap[long_heatmap$Year==input$year,], "heatmap", hcaes(x = stat, y = State.Name, value = count),
                 turboThreshold = 0) %>% 
      hc_colorAxis(stops = list( list(0.25, "#F2D1CE"), 
                                 list(0.5, "#F2B2AC"), 
                                 list(0.75, "#F29188"),
                                 list(1, "#ED7768")),
                   max = 15
                   ) %>% 
      hc_yAxis(reversed = TRUE, offset = 10, tickLength = 0,
               gridLineWidth = 0, minorGridLineWidth = 0, title = "State",
               labels = list(style = list(fontSize = "10 px"))) %>% 
      hc_xAxis(categories = list("Fee for Service, Long Acting", "Fee for Service", "Long Acting",
                                 "Managed Care, Long Acting", "Mangaed Care", "General"),
               title = NULL) %>%
      hc_tooltip(formatter = tooltip) %>%
      hc_title(text = "Opioid Prescribing Rate") %>% 
      hc_legend(layout = "vertical", verticalAlign = "top",
                align = "right", valueDecimals = 0) %>% 
      hc_size(height = 800)
      hc
  })
  
  output$bar <- renderHighchart({
    hchart(long_bar[long_bar$type == input$buttons, ], "column",
                  hcaes(x = Year, y = count, group = stat))
  })
  
  output$lines <- renderPlotly({
    
  })
}

shinyApp(ui=ui, server=server)
