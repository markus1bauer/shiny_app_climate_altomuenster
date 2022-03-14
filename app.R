# Shiny ####



### Packages ###
library(here)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(shiny)
library(shinydashboard)
library(rsconnect)

### Start ###
rm(list = ls())
setwd(here())

setAccountInfo(name='markusbauer',
               token='BB9C744354C19D1217D8FAD42760C1ED',
               secret='5XG8BIuY7IF40mc6OO7TUSMzJbZEoe4lH5Q8aEGf')

# A Load data ####

data.temp <- read_csv("data/cdc_download_2021-12-25_19_51/data/data_OBS_DEU_P1M_T2M.csv", 
                 col_names = T, 
                 na = c("", "na", "NA"), 
                 col_types = 
                   cols(
                     .default = "?",
                     Zeitstempel = col_date(format = "%Y-%m-%d")
                     )) %>%
  rename(avg_month = Wert, date = Zeitstempel) %>%
  mutate(year = year(date))
 
data.prec <- read_csv("data/cdc_download_2021-12-25_19_52/data/data_OBS_DEU_P1M_RR.csv", 
                      col_names = T, 
                      na = c("", "na", "NA"), 
                      col_types = 
                        cols(
                          .default = "?",
                          Zeitstempel = "D"
                        )) %>%
  rename(avg_month = Wert, date = Zeitstempel) %>%
  mutate(year = year(date))

themeMB <- function(){
  theme(
    panel.background = element_rect(fill = "white"),
    text  = element_text(size = 9, color = "black"),
    strip.text = element_text(size = 10),
    axis.text = element_text(angle = 0, hjust = 0.5, size = 9, color = "black"),
    axis.title = element_text(angle = 0, hjust = 0.5, size = 9, color = "black"),
    axis.line = element_line(),
    legend.key = element_rect(fill = "white"),
    legend.position = "none",
    legend.margin = margin(0, 0, 0, 0, "cm"),
    plot.margin = margin(0, 0, 0, 0, "cm")
  )
}


# B ui.R ####

## 1 header ####

header <- dashboardHeader(
  title = "Climate Altomünster",
  titleWidth = 300,
  tags$li(a(href = 'https://github.com/markus1bauer', 
            icon("github", 
                 lib = "font-awesome",
                 height = "30px"),
            style = "padding-top:15px; padding-bottom:15px;
            padding-left:15px; padding-right:15px; font-size: 20px"),
          class = "dropdown")
  )

## 2 sidebar ####

sidebar <- dashboardSidebar(
  width = 300,
  sidebarMenu(
    
    ### a Year range ####
    menuItem(sliderInput(inputId = "year_range",
                         label = "Select year range",
                         min = as.Date("1955-01-01"),
                         max = as.Date("2021-11-01"),
                         value = c(as.Date("1955-01-01"), as.Date("2021-11-01")),
                         timeFormat = "%Y"
                         )
             ),
    
    ### b Radio buttons ####
    menuItem(radioButtons(
      inputId = "avg", 
      label = "Show:",
      selected = c("avg_month"),
      choices = c("Year" = "avg_year", "Month" = "avg_month")
      )
      ),
    
    ### d Check box ####
    menuItem(
      checkboxInput(
        inputId = "smoother", 
        label = strong("Overlay smooth trend line"), 
        value = FALSE
        )
      ),
    
    ### e Smoother Span ####
    menuItem(conditionalPanel(
      condition = "input.smoother == true",
      sliderInput(
        inputId = "smoother_span",
        label = "Smoother span:",
                  min = 0.01, 
                  max = 1, 
                  value = 0.25, 
                  step = 0.01),
      HTML("Higher values give more smoothness.")
      )
      )
    )
  )


## 3 body ####

body <- dashboardBody(
  tabsetPanel(
    type = "tabs",
    id = "tab_selected",
    tabPanel(
      title = "Temperature",
      plotOutput(outputId = "plot_temp",
                 dblclick = "plot_dbclick",
                 brush = brushOpts(
                   id = "plot_brush",
                   resetOnNew = TRUE
                   )
                 )
    ),
    tabPanel(
      title = "Precipitation",
      plotOutput(outputId = "plot_prec")
    )
    )
  )

ui <- dashboardPage(header, sidebar, body,
                    skin = "blue")


# C server.R ####

server <- function(input, output) {
  
  ranges <- reactiveValues(x_range = NULL, y_range = NULL)
  
  ## 1 Temperature ####
  output$plot_temp <- renderPlot({
    
    ### a Data ####
    if(input$avg == "avg_year"){
      data <- data.temp %>%
        filter(date >= input$year_range[1] & 
                 date <= input$year_range[2]) %>%
        select(avg_month, year) %>%
        group_by(year) %>%
        summarise(avg = mean(avg_month))
    } else {
      data <- data.temp %>%
        filter(date >= input$year_range[1] & 
                 date <= input$year_range[2]) %>%
        select(avg = avg_month, year = date)
    }
    
    ### b Plot ####
    if(input$smoother) {
      ggplot(data, aes(y = avg, x = year)) +
        geom_line() +
        geom_point(data = data, 
                   color = "black") +
        geom_text_repel(data = data %>% slice_max(avg, n = 10),
                        aes(label = year),
                        force_pull   = 0, 
                        nudge_y      = Inf,
                        direction    = "x",
                        angle = 90,
                        hjust        = 0,
                        segment.size = 0.2,
                        max.iter = 1e4, max.time = 1
        ) +
        geom_point(data = data %>% slice_max(avg, n = 10), 
                   aes(label = year), 
                   color = "red", size = 2) +
        geom_smooth(method = "loess", span = input$smoother_span) +
        coord_cartesian(xlim = ranges$x_range,
                        ylim = ranges$y_range,
                        expand = FALSE) +
        scale_y_continuous(breaks = seq(-10, 30, 2)) +
        scale_x_continuous(breaks = seq(1950, 2030, 10)) +
        labs(y = "Temperature [C°]", x = "Year") +
        themeMB()
      } else {
        ggplot(data, aes(y = avg, x = year)) +
          geom_line() +
          geom_point(data = data, 
                     color = "black") +
          geom_text_repel(data = data %>% slice_max(avg, n = 10),
                          aes(label = year),
                          force_pull   = 0, 
                          nudge_y      = Inf,
                          direction    = "x",
                          angle = 90,
                          hjust        = 0,
                          segment.size = 0.2,
                          max.iter = 1e4, max.time = 1
                          ) +
          geom_point(data = data %>% slice_max(avg, n = 10),
                     aes(label = year),
                     color = "red", size = 2) +
          coord_cartesian(xlim = ranges$x_range,
                          ylim = ranges$y_range,
                          expand = FALSE) +
          scale_y_continuous(breaks = seq(-10, 30, 2)) +
          scale_x_continuous(breaks = seq(1950, 2030, 10)) +
          labs(y = "Temperature [C°]", x = "Year") +
          themeMB()
        }
      
    })
  
  observeEvent(input$plot_dbclick, {
    brush <- input$plot_brush
    if (!is.null(brush)) {
      ranges$x_range <- c(brush$xmin, brush$xmax)
      ranges$y_range <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x_range <- NULL
      ranges$y_range <- NULL
    }
  })

  ## 2 Precepitation ####
  output$plot_prec <- renderPlot({
    
      ### a Data ####
      if(input$avg == "avg_year"){
        data <- data.prec %>%
          filter(date >= input$year_range[1] & 
                   date <= input$year_range[2]) %>%
          select(avg_month, year) %>%
          group_by(year) %>%
          summarise(avg = mean(avg_month))
      } else {
        data <- data.prec %>%
          filter(date >= input$year_range[1] & 
                   date <= input$year_range[2]) %>%
          select(avg = avg_month, year = date)
      }
      
      ### b Plot ####
      if(input$smoother) {
        ggplot(data, aes(y = avg, x = year)) +
          geom_line() +
          geom_point(data = data, 
                     color = "black") +
          geom_text_repel(data = data %>% slice_min(avg, n = 10),
                          aes(label = year),
                          force_pull   = 0, 
                          nudge_y      = -Inf,
                          direction    = "x",
                          angle = 90,
                          hjust        = 0,
                          segment.size = 0.2,
                          max.iter = 1e4, max.time = 1
          ) +
          geom_point(data = data %>% slice_min(avg, n = 10), 
                     aes(label = year), 
                     color = "red", size = 2) +
          geom_smooth(method = "loess", span = input$smoother_span) +
          scale_y_continuous(breaks = seq(-100, 400, 20)) +
          scale_x_continuous(breaks = seq(1950, 2050, 10)) +
          labs(y = "Precepitation [mm]", x = "Year") +
          themeMB()
      } else {
        ggplot(data, aes(y = avg, x = year)) +
          geom_line() +
          geom_point(data = data, 
                     color = "black") +
          geom_text_repel(data = data %>% slice_min(avg, n = 10),
                          aes(label = year),
                          force_pull   = 0, 
                          nudge_y      = -Inf,
                          direction    = "x",
                          angle = 90,
                          hjust        = 0,
                          segment.size = 0.2,
                          max.iter = 1e4, max.time = 1
          ) +
          geom_point(data = data %>% slice_min(avg, n = 10),
                     aes(label = year),
                     color = "red", size = 2) +
          scale_y_continuous(breaks = seq(-100, 400, 20)) +
          scale_x_continuous(breaks = seq(1950, 2050, 10)) +
          labs(y = "Precepitation [mm]", x = "Year") +
          themeMB()
      }
  })
  
}

# D Run the app ####

shinyApp(ui = ui, server = server)
