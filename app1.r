## app.R ##
library(shinydashboard)
library(flexdashboard)
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(DT)
library(ggthemes)
library(ggplot2)
library(plotly)
library(highcharter)

## 讀入所需資料(原始資料包資料)

# cbind(grDevices::col2rgb(col.raw),3) -> col
# c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A") -> col.raw
# data_cluster <- fread('./20160613_data_cluster.csv')
# radar.data <- read.csv('./20160613_Radar_plot.csv') %>%
#   mutate_each(funs(round(.,digits=2)),-index)
# radar.data$mean <- rowMeans(radar.data[,-1])
data <- fread('./20160811_data.csv')
power <- fread('iconv -f big5 -t utf8 origin_data/power_twomonth.csv')
options(scipen = 100)
ui <- dashboardPage(
  dashboardHeader(title = "Power Hackathon Asia",titleWidth = 300),
  dashboardSidebar(
    sidebarMenu(
      menuItem("台北市用電量概況", tabName = "overview", icon = icon("bar-chart")),
      menuItem("各行政區用電量概況", tabName = "area", icon = icon("opera"),
               menuSubItem('北投區', tabName = "bato", icon = icon("opera")),
               menuSubItem('士林區', tabName = "4lin", icon = icon("opera")),
               menuSubItem('內湖區', tabName = "neihu", icon = icon("opera")),
               menuSubItem('松山區', tabName = "songhsan", icon = icon("opera")),
               menuSubItem('中山區', tabName = "chungshan", icon = icon("opera")),
               menuSubItem('大同區', tabName = "datong", icon = icon("opera")),
               menuSubItem('萬華區', tabName = "wenhua", icon = icon("opera")),
               menuSubItem('中正區', tabName = "chungchen", icon = icon("opera")),
               menuSubItem('大安區', tabName = "daan", icon = icon("opera")),
               menuSubItem('信義區', tabName = "shinyi", icon = icon("opera")),
               menuSubItem('南港區', tabName = "nago", icon = icon("opera")),
               menuSubItem('文山區', tabName = "wenshan", icon = icon("opera"))),
      menuItem("節電對象之輪廓", tabName = "compare", icon = icon("opera"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'overview',
              fluidRow(
                column(
                  width = 4,
                  box(width = NULL, status = 'info', height = 400, title = "選擇 :",
                      selectInput('overview_time_big', 
                                  label = "用電大戶里查詢(雙月制)：", 
                                  choices = unique(power$統計年月),
                                  selected = 10407),
                      selectInput('overview_n_big', 
                                  label = "用電大戶前幾名：", 
                                  choices = c(1:10),
                                  selected = 5),
                      fluidRow(
                        column(width = 6,
                               selectInput('overview_time_growth1', 
                                           label = "耗電成長里查詢(雙月制) - 基期：", 
                                           choices = unique(power$統計年月),
                                           selected = 10405)), # 第一個算不出成長
                        column(width = 6,
                               selectInput('overview_time_growth2', 
                                           label = "耗電成長里查詢(雙月制) - 當期：", 
                                           choices = unique(power$統計年月),
                                           selected = 10407))
                      ),
                      selectInput('overview_n_growth', 
                                  label = "成長幅度前幾名：", 
                                  choices = c(1:10),
                                  selected = 5)
                  )
                ),
                column(
                  width = 8,
                  tabBox(
                    width = NULL,
                    tabPanel(h5('節電大戶'), status = 'info', height = 400,
                             plotlyOutput("overview_big_plot")),
                    tabPanel(h5('成長大戶'), status = 'info', height = 400,
                             plotlyOutput("overview_growth_plot"))
                  )
                )
              )
      )
      
    )
  )
)

server <- function(input, output) {
  output$overview_big_plot <- renderPlotly({
    power %>% 
      filter(統計年月 == input$overview_time_big) %>% 
      arrange(desc(售電量度數)) %>% 
      slice(1:input$overview_n_big) %>% 
      ggplot(aes(x = reorder(行政區域, desc(售電量度數)), y = 售電量度數)) +
      geom_bar(stat = 'identity') +
      labs(x = '行政區域') -> plot_tmp
    ggplotly(plot_tmp)
  })
  
  output$overview_growth_plot <- renderPlotly({
    power %>% 
      filter(統計年月 %in% c(input$overview_time_growth1,input$overview_time_growth2)) %>%
      group_by(行政區域) %>% 
      arrange(desc(統計年月)) %>% 
      mutate(base = lead(售電量度數, 1),
             用電量成長比例 = (售電量度數 - base)/base) %>% 
      ungroup %>% 
      arrange(desc(用電量成長比例)) %>% 
      slice(1:input$overview_n_growth) %>% 
      ggplot(aes(x = reorder(行政區域, desc(用電量成長比例)), y = 用電量成長比例)) +
      geom_bar(stat = 'identity') +
      labs(x = '行政區域') -> plot_tmp1
    ggplotly(plot_tmp1)
  })
}

shinyApp(ui, server) 













