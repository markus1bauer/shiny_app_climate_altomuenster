# Shiny app
# Climate Altomuenster ####
# Markus Bauer 
# 2022-03-23



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

# A Load data ####

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
  mutate(year = year(date))
 
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
  mutate(year = year(date))

themeMB <- function(){
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
    menuItem(
      sliderInput(
        inputId = "year_range",
        label = "Select year range",
        #style = "font-family: 'arial'; font-si12pt",
        min = as.Date("1955-01-01"),
        max = as.Date("2021-11-01"),
        value = c(as.Date("1955-01-01"), as.Date("2021-11-01")),
        timeFormat = "%Y"
        )
      ),
    
    ### b Radio buttons ####
    menuItem(
      radioButtons(
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


## 3 body ####

body <- dashboardBody(
  tabsetPanel(
    type = "tabs",
    id = "tab_selected",
    
    tabPanel(
      title = "Temperature",
      girafeOutput(outputId = "plot_temp")
      ),
    
    tabPanel(
      title = "Precipitation",
      girafeOutput(outputId = "plot_prec")
      )
    ),
  htmlOutput("text")
  )

ui <- dashboardPage(
  header, sidebar, body,
  skin = "blue",
  tags$head(tags$style(HTML('* {font-family: "Arial"};')))
)


# C server.R ####

server <- function(input, output) {
  
  ## 1 Temperature ####
  output$plot_temp <- renderGirafe({
    
    ### a Data ####
    if(input$avg == "avg_year"){
      data <- data.temp %>%
        filter(date >= input$year_range[1] & 
                 date <= input$year_range[2]) %>%
        select(avg_month, date) %>%
        group_by(year = lubridate::floor_date(date, "year")) %>%
        summarise(avg = mean(avg_month)) %>%
        mutate(avg = round(avg, digits = 1))
    } else {
      data <- data.temp %>%
        filter(date >= input$year_range[1] & 
                 date <= input$year_range[2]) %>%
        select(avg = avg_month, year = date) %>%
        mutate(avg = round(avg, digits = 1))
    }
    
    ### b General plot temperature ####
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
      geom_point_interactive(aes(tooltip = avg, data_id = avg)) +
      geom_point_interactive(data = data %>% slice_max(avg, n = 10),
                             aes(tooltip = avg, data_id = avg),
                             color = "red", size = 2) +
      scale_y_continuous(expand = expansion(mult = c(0.05, .15))) +
      scale_x_date(date_labels = "%Y",
                   date_breaks = "10 years",
                   limits = as.Date(c(input$year_range[1], input$year_range[2]))) +
      labs(y = "Temperature [C°]", x = "Year") +
      themeMB()
    
    ### c Smoother ####
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
  

  ## 2 Precipitation ####
  output$plot_prec <- renderGirafe({
    
      ### a Data ####
    if(input$avg == "avg_year"){
      data <- data.prec %>%
        filter(date >= input$year_range[1] & 
                 date <= input$year_range[2]) %>%
        select(avg_month, date) %>%
        group_by(year = lubridate::floor_date(date, "year")) %>%
        summarise(avg = mean(avg_month)) %>%
        mutate(avg = round(avg, digits = 0))
    } else {
      data <- data.prec %>%
        filter(date >= input$year_range[1] & 
                 date <= input$year_range[2]) %>%
        select(avg = avg_month, year = date) %>%
        mutate(avg = round(avg, digits = 0))
    }
    
    ### b General plot precipitation ####
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
      geom_point_interactive(aes(tooltip = avg, data_id = avg)) +
      geom_point_interactive(data = data %>% slice_min(avg, n = 10),
                             aes(tooltip = avg, data_id = avg),
                             color = "red", size = 2) +
      scale_y_continuous(expand = expansion(mult = c(0.15, 0.05))) +
      scale_x_date(date_labels = "%Y",
                   date_breaks = "10 years",
                   limits = as.Date(c(input$year_range[1], input$year_range[2]))) +
      labs(y = "Precipitation [mm]", x = "Year") +
      themeMB()
    
      ### c Smoother ####
    if(input$smoother) {
        girafe(
          ggobj = plot +
            geom_smooth(method = "loess", span = input$smoother_span),
          options = list(
            opts_hover_inv(css = "opacity:0.1;"),
            opts_hover(css = "fill:red;"),
            opts_sizing(width = .7),
            opts_zoom(max = 5),
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
            opts_zoom(max = 5),
            opts_toolbar(position = "bottomright")
            )
          )
      }
  })
  
## 3 Text ####
  
  output$text <- renderUI({
    div(
      br(),
      br(),
      "Highest or lowest values are marked with red points.",
      br(),
      br(),
      "This dashboard is from ",
      a("Markus Bauer",
        href="https://orcid.org/0000-0001-5372-4174",
        target="_blank"),
      "and the code can be found on ",
      a("GitHub",
        href="https://github.com/markus1bauer/shiny_app_demo",
        target="_blank"),
      br(),
      br(),
      "Data was retrieved  from ",
      a("DWD Climate Data Center (CDC): Monthly station observations of precipitation in mm for Germany, v21.3, last accessed: 2021-12-25", 
        href="https://cdc.dwd.de/portal/",
        target="_blank"),
      br(),
      br(),
      br()
    )
    })
  
  
}

# D Run the app ####

shinyApp(ui = ui, server = server)
