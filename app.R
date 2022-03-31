# Shiny app
# Climate Altomuenster ####
# Markus Bauer 
# 2022-03-23



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ###############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Packages ###
library(here)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(ggiraph)
library(shiny)
library(shinydashboard)
library(rsconnect)

### Start ###
rm(list = ls())
setwd(here())
setAccountInfo(name='markusbauer',
               token='BB9C744354C19D1217D8FAD42760C1ED',
               secret='5XG8BIuY7IF40mc6OO7TUSMzJbZEoe4lH5Q8aEGf')


#### Load data ####

data.temp <- read_csv(
  "data/cdc_download_2021-12-25_19_51/data/data_OBS_DEU_P1M_T2M.csv",
  col_names = T, 
  na = c("", "na", "NA"), 
  col_types = 
    cols(
      .default = "?",
      Zeitstempel = col_date(format = "%Y-%m-%d")
    )
  ) %>%
  rename(avg_month = Wert, date = Zeitstempel) %>%
  mutate(year = year(date),
         avg_month = round(avg_month, digits = 1))
 
data.prec <- read_csv(
  "data/cdc_download_2021-12-25_19_52/data/data_OBS_DEU_P1M_RR.csv",
  col_names = T, 
  na = c("", "na", "NA"), 
  col_types = 
    cols(
      .default = "?",
      Zeitstempel = "D"
    )
  ) %>%
  rename(avg_month = Wert, date = Zeitstempel) %>%
  mutate(year = year(date),
         avg_month = round(avg_month, digits = 0))

theme_mb <- function(){
  theme(
    panel.background = element_rect(fill = "white"),
    text  = element_text(size = 12, color = "black", family = "Arial"),
    strip.text = element_text(size = 12),
    axis.text =
      element_text(angle = 0, hjust = 0.5, size = 12, color = "black"),
    axis.title =
      element_text(angle = 0, hjust = 0.5, size = 12, color = "black"),
    axis.line = element_line(),
    legend.key = element_rect(fill = "white"),
    legend.position = "none",
    legend.margin = margin(0, 0, 0, 0, "cm"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  )
}


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B App #######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## 1 UI #######################################################################

### a Header ------------------------------------------------------------------
header <- dashboardHeader(
  title = "Climate Altomünster",
  titleWidth = 300,
  tags$li(a(href = 'https://github.com/markus1bauer/shiny_app_climate_altomuenster', 
            icon("github", 
                 lib = "font-awesome",
                 height = "30px"),
            style = "padding-top:15px; padding-bottom:15px;
            padding-left:15px; padding-right:15px; font-size: 20px"),
          class = "dropdown")
  )

### b Sidebar -----------------------------------------------------------------
sidebar <- dashboardSidebar(
  width = 300,
  sidebarMenu(
    
    #### Year range ####
    menuItem(
      sliderInput(
        inputId = "year_range",
        label = "Select year range",
        min = as.Date("1955-01-01"),
        max = as.Date("2021-11-01"),
        value = c(as.Date("1955-01-01"), as.Date("2021-11-01")),
        timeFormat = "%Y"
        )
      ),
    
    #### Radio buttons ####
    menuItem(
      radioButtons(
        inputId = "avg", 
        label = "Show:",
        selected = c("avg_month"),
        choices = c("Year" = "avg_year", "Month" = "avg_month")
        )
      ),
    
    #### Check box ####
    menuItem(
      checkboxInput(
        inputId = "smoother", 
        label = strong("Overlay smooth trend line"), 
        value = FALSE
        )
      ),
    
    #### Smoother Span ####
    menuItem(
      conditionalPanel(
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


### c Body --------------------------------------------------------------------
body <- dashboardBody(
  tabsetPanel(
    type = "tabs",
    id = "tab_selected",
    
    tabPanel(
      title = "Temperature",
      girafeOutput(outputId = "plot_temp", height = "60vh", width = "80vh")
      ),
    
    tabPanel(
      title = "Precipitation",
      girafeOutput(outputId = "plot_prec")
      )
    ),
  box(htmlOutput("text"))
  )

### d Dashboard page ---------------------------------------------------------
ui <- dashboardPage(
  header, sidebar, body,
  skin = "blue",
  tags$head(tags$style(HTML('* {font-family: "Arial"};')))
)


## 2 Server ###################################################################

server <- function(input, output) {
  
  ### a Temperature ----------------------------------------------------------
  output$plot_temp <- renderGirafe({
    
    if(input$avg == "avg_year"){
      ### Year data ###
      data <- data.temp %>%
        filter(date >= input$year_range[1] & 
                 date <= input$year_range[2]) %>%
        select(avg_month, date) %>%
        group_by(year = lubridate::floor_date(date, "year")) %>%
        summarise(avg = mean(avg_month)) %>%
        mutate(avg = round(avg, digits = 1),
               tooltip = c(paste0(avg, " °C",
                                  "\n", year(year))))
    } else {
      ### Month data ###
      data <- data.temp %>%
        filter(date >= input$year_range[1] & 
                 date <= input$year_range[2]) %>%
        select(avg = avg_month, year = date) %>%
        mutate(tooltip = c(paste0(avg, " °C",
                                  "\n", year(year), "-", month(year))))
    }
    
    #### General plot temperature ####
    plot <- ggplot(data, aes(y = avg, x = year)) +
      geom_line() +
      geom_text_repel(data = data %>% slice_max(avg, n = 10),
                      aes(label = year(year)),
                      force_pull   = 0, 
                      nudge_y      = Inf,
                      direction    = "x",
                      angle = 90,
                      hjust        = 0,
                      segment.size = 0.2,
                      max.iter = 1e4, max.time = 1
      ) +
      geom_point_interactive(aes(tooltip = tooltip, data_id = tooltip)) +
      geom_point_interactive(data = data %>% slice_max(avg, n = 10),
                             aes(tooltip = tooltip, data_id = tooltip),
                             color = "red", size = 2) +
      scale_y_continuous(expand = expansion(mult = c(0.05, .15))) +
      scale_x_date(date_labels = "%Y",
                   date_breaks = "10 years",
                   minor_breaks = "5 years",
                   limits = as.Date(c(input$year_range[1], input$year_range[2]))) +
      labs(y = "Temperature [C°]", x = "Year") +
      theme_mb()
    
    #### Smoother ####
    if(input$smoother) {
      girafe(
        ggobj = plot +
          geom_smooth(method = "loess", span = input$smoother_span),
        options = list(
          opts_hover_inv(css = "opacity:0.1;"),
          opts_hover(css = "fill:red;"),
          opts_sizing(rescale = TRUE),
          opts_zoom(max = 2),
          opts_toolbar(position = "bottomright")
        )
      )
    } else {
      
      girafe(
        ggobj = plot,
        options = list(
          opts_hover_inv(css = "opacity:0.1;"),
          opts_hover(css = "fill:red;"),
          opts_sizing(rescale = TRUE),
          opts_zoom(max = 2),
          opts_toolbar(position = "bottomright")
        )
      )
    }
    
  })
  

  ### b Precipitation ---------------------------------------------------------
  output$plot_prec <- renderGirafe({
    
      #### Data ####
    if(input$avg == "avg_year"){
      ### Year data ###
      data <- data.prec %>%
        filter(date >= input$year_range[1] & 
                 date <= input$year_range[2]) %>%
        select(avg_month, date) %>%
        group_by(year = lubridate::floor_date(date, "year")) %>%
        summarise(avg = mean(avg_month)) %>%
        mutate(avg = round(avg, digits = 0),
               tooltip = c(paste0(avg, " mm",
                                  "\n", year(year))))
    } else {
      ### Month data ###
      data <- data.prec %>%
        filter(date >= input$year_range[1] & 
                 date <= input$year_range[2]) %>%
        select(avg = avg_month, year = date) %>%
        mutate(tooltip = c(paste0(avg, " mm",
                                  "\n", year(year), "-", month(year))))
    }
    
    #### General plot precipitation ####
    plot <- ggplot(data, aes(y = avg, x = year)) +
      geom_line() +
      geom_text_repel(data = data %>% slice_min(avg, n = 10),
                      aes(label = year(year)),
                      force_pull   = 0, 
                      nudge_y      = -Inf,
                      direction    = "x",
                      angle = 90,
                      hjust        = 0,
                      segment.size = 0.2,
                      max.iter = 1e4, max.time = 1
      ) +
      geom_point_interactive(aes(tooltip = tooltip, data_id = tooltip)) +
      geom_point_interactive(data = data %>% slice_min(avg, n = 10),
                             aes(tooltip = tooltip, data_id = tooltip),
                             color = "red", size = 2) +
      scale_y_continuous(expand = expansion(mult = c(0.15, 0.05))) +
      scale_x_date(date_labels = "%Y",
                   date_breaks = "10 years",
                   limits = as.Date(c(input$year_range[1], input$year_range[2]))) +
      labs(y = "Precipitation [mm]", x = "Year") +
      theme_mb()
    
      #### Smoother ####
    if(input$smoother) {
        girafe(
          ggobj = plot +
            geom_smooth(method = "loess", span = input$smoother_span),
          options = list(
            opts_hover_inv(css = "opacity:0.1;"),
            opts_hover(css = "fill:red;"),
            opts_sizing(width = .7),
            opts_zoom(max = 2),
            opts_toolbar(position = "bottomright")
            )
          )
      } else {
        girafe(
          ggobj = plot,
          options = list(
            opts_hover_inv(css = "opacity:0.1;"),
            opts_hover(css = "fill:red;"),
            opts_sizing(width = .7),
            opts_zoom(max = 2),
            opts_toolbar(position = "bottomright")
            )
          )
      }
  })
  
### c Text -------------------------------------------------------------------
  output$text <- renderUI({
    div(
      br(),
      br(),
      "The highest or lowest values are marked with",
      HTML("<span style = color:red><strong>red points</strong></span>."),
      br(),
      br(),
      "This dashboard is from ",
      a("Markus Bauer",
        href="https://orcid.org/0000-0001-5372-4174",
        target="_blank"),
      "and the code can be found on ",
      a("GitHub.",
        href="https://github.com/markus1bauer/shiny_app_demo",
        target="_blank"),
      br(),
      br(),
      "Data was retrieved  from ",
      a("DWD Climate Data Center (CDC): Monthly station observations of precipitation in mm for Germany, v21.3, last accessed: 2021-12-25", 
        href="https://cdc.dwd.de/portal/",
        target="_blank"),
      br(),
      a("Weather station: Altomünster-Maisbrunn", 
        href="https://www.hnd.bayern.de/niederschlag/inn/altomuenster-maisbrunn-142/stammdaten",
        target="_blank"),
      br(),
      br(),
      "Status: 2022-03-23",
      br(),
      br(),
      br()
    )
    })
}


## 3 Run app ################################################################

shinyApp(ui = ui, server = server)
