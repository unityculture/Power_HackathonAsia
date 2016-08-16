## app.R ##
library(shinydashboard)
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(plotly)
library(highcharter)
library(leaflet)
library(rgdal)
library(maptools)
library(magrittr)
library(ggthemes)

## 讀入所需資料(原始資料包資料)

c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A") -> col.raw# data_cluster <- fread('./20160613_data_cluster.csv')
cbind(grDevices::col2rgb(col.raw),3) -> col
# radar.data <- read.csv('./20160613_Radar_plot.csv') %>%
#   mutate_each(funs(round(.,digits=2)),-index)
# radar.data$mean <- rowMeans(radar.data[,-1])
data <- fread('iconv -f big5 -t utf8 ./data_hack.csv')
data %>% select(統計年月,縣市,行政區域,每人平均用電度數) -> power
# power <- fread('iconv -f big5 -t utf8 origin_data/power_twomonth.csv')
power_area <- power %>% mutate(鄉鎮 = substring(行政區域,1,3))
sfn <- readOGR(dsn = 'map/Village_NLSC_1050219.shp',
               layer='Village_NLSC_1050219', stringsAsFactors = F,
               verbose = F)
## demo專用
radar<- read.csv('20160613_Radar_plot.csv') %>%
  mutate_each(funs(round(.,digits=2)),-index)
radar$mean <- rowMeans(radar[,-1])
### demo用雷達圖
hcmix = highchart() %>% 
  hc_chart(polar = TRUE, type = "line") %>% 
  hc_xAxis(categories = radar$index,
           tickmarkPlacement = 'on',
           lineWidth = 0) %>% 
  hc_yAxis(gridLineInterpolation = 'polygon',
           lineWidth = 0,
           min = 0, max = 1) %>% 
  hc_series(
    list(
      name = "Cluster-3",
      data = radar[,c(2)],
      pointPlacement = 'on',color=col.raw[1]),
    list(
      name = "Cluster-2",
      data = radar[,c(3)],
      pointPlacement = 'on',color=col.raw[2]),
    list(
      name = "Cluster-1",
      data = radar[,c(4)],
      pointPlacement = 'on',color=col.raw[3]),
    list(
      name = "Cluster-4",
      data = radar[,c(5)],
      pointPlacement = 'on',color=col.raw[4]),
    list(
      name = "Cluster-5",
      data = radar[,c(6)],
      pointPlacement = 'on',color=col.raw[5]),
    list(
      name = "Overall-Mean",
      data = radar[,c(10)],
      pointPlacement = 'on',color='#474747'))

options(scipen = 100)
ui <- dashboardPage(
  dashboardHeader(title = "Power Hackathon Asia",titleWidth = 300),
  dashboardSidebar(
    sidebarMenu(
      menuItem("台北市用電量概況", tabName = "overview", icon = icon("bar-chart")),
      menuItem("各行政區用電量概況", tabName = "area", icon = icon("plug")),
      menuItem("節電對象之輪廓", tabName = "compare", icon = icon("user")),
      menuItem("節電對象分群範例", tabName = "demo", icon = icon("sitemap"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'overview',
              fluidRow(
                tags$iframe(src = "https://unityculturesheng.carto.com/viz/35497786-1716-11e6-bc60-0e3ff518bd15/embed_map",
                                   width="100%" ,height="520")),
              fluidRow(
                column(
                  width = 4,
                  box(width = NULL, status = 'warning', title = "用電大戶基本查詢:",
                      solidHeader = TRUE, collapsible = T, 
                      selectInput('overview_time_big', 
                                  label = "用電大戶里查詢(雙月制)：", 
                                  choices = unique(power$統計年月),
                                  selected = 10505),selectInput('overview_n_big', 
                                                                label = "用電大戶前幾名：", 
                                                                choices = c(1:30),
                                                                selected = 5)
                  ),
                  box(width = NULL, status = 'warning', title = "用電成長幅度基本查詢:",
                      solidHeader = TRUE, collapsible = T, 
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
                                           selected = 10505))
                      ),
                      selectInput('overview_n_growth', 
                                  label = "成長幅度前幾名：", 
                                  choices = c(1:30),
                                  selected = 5)
                  )
                ),
                column(
                  width = 8,
                  tabBox(
                    width = NULL,
                    tabPanel(h5('用電大戶'), status = 'info', height = 400,
                             plotlyOutput("overview_big_plot")
                    ),
                    tabPanel(h5('用電大戶所在位置'), status = 'info', height = 400,
                             leafletOutput("map_big")
                    ),
                    tabPanel(h5('用電成長大戶'), status = 'info', height = 400,
                             plotlyOutput("overview_growth_plot")
                    ),
                    tabPanel(h5('用電成長大戶所在位置'), status = 'info', height = 400,
                             leafletOutput("map_growth")
                    ),
                    tabPanel(h5('歷年月各村里每人用電度數'), status = 'info', height = 400,
                             plotlyOutput('yearmap')
                    )
                  )
                )
             ),
             fluidRow(
               column(
                 width = 4,
                 box(width = NULL, status = 'info', title = "省電大戶基本查詢:",
                     solidHeader = TRUE, collapsed = T, collapsible = T, 
                     selectInput('overview_time_small', 
                                 label = "省電大戶里查詢(雙月制)：", 
                                 choices = unique(power$統計年月),
                                 selected = 10407),
                     selectInput('overview_n_small', 
                                 label = "省電大戶前幾名：前幾名：", 
                                 choices = c(1:30),
                                 selected = 5)
                 ),
                 box(width = NULL, status = 'info', title = "省電成長幅度基本查詢:",
                     solidHeader = TRUE, collapsed = T, collapsible = T, 
                     fluidRow(
                       column(width = 6,
                              selectInput('overview_time_decay1', 
                                          label = "省電成長里查詢(雙月制) - 基期：", 
                                          choices = unique(power$統計年月),
                                          selected = 10405)), # 第一個算不出成長
                       column(width = 6,
                              selectInput('overview_time_decay2', 
                                          label = "省電成長里查詢(雙月制) - 當期：", 
                                          choices = unique(power$統計年月),
                                          selected = 10407))
                     ),
                     selectInput('overview_n_decay', 
                                 label = "省電成長幅度前幾名：", 
                                 choices = c(1:30),
                                 selected = 5)
                     )
             ),
             column(
               width = 8,
               tabBox(
                 width = NULL,
                 tabPanel(h5('省電大戶'), status = 'info', height = 400,
                          plotlyOutput("overview_small_plot")),
                 tabPanel(h5('省電大戶所在位置'), status = 'info', height = 400,
                          leafletOutput("map_sml")
                 ),
                 tabPanel(h5('省電成長大戶'), status = 'info', height = 400,
                          plotlyOutput("overview_decay_plot")),
                 tabPanel(h5('省電幅度高之所在位置'), status = 'info', height = 400,
                          leafletOutput("map_sml_growth")
                 ),
                 tabPanel(h5('歷年各村里省電度數'), status = 'info', height = 400,
                          plotlyOutput("yearmap1")
                 )
               )
             )
           )
    ),
    tabItem(tabName = "compare",
            fluidRow(
              column(
                width = 4,
                box(width = NULL, status = 'info', height = 500, title = "人口特性比較",
                    selectInput('n_big', 
                                label = "用電大戶前幾名：", 
                                choices = c(1:10),
                                selected = 5),
                    selectInput('n_small', 
                                label = "省電大戶前幾名：", 
                                choices = c(1:10),
                                selected = 5),
                    infoBoxOutput("approvalBox2",width = 100),
                    selectInput('yearmonth', 
                                label = "選擇年月：", 
                                choices = unique(power$統計年月),
                                selected = 10407)
                )
              ),
              box(width = 8, status = 'info', height = 500, title = "選擇 :",
                  highchartOutput(outputId = "big_small_competion")
                )
             )
    ),
    tabItem(tabName = 'area',
            fluidRow(
              column(
                width = 4,
                box(width = NULL, status = 'warning', height = 500, title = "用電大戶基本查詢: ",
                    solidHeader = T,
                    selectInput('area_c', 
                                label = "選擇村里", 
                                choices = unique(power_area$鄉鎮),
                                selected = "北投區"),
                    selectInput('detail_time_big', 
                                label = "用電大戶里查詢(雙月制)：", 
                                choices = unique(power$統計年月),
                                selected = 10407),
                    selectInput('detail_n_big', 
                                label = "用電大戶前幾名：", 
                                choices = c(1:10),
                                selected = 5),
                    fluidRow(
                      column(width = 6,
                             selectInput('detail_time_growth1', 
                                         label = "用電成長里查詢(雙月制) - 基期：", 
                                         choices = unique(power$統計年月),
                                         selected = 10405)), # 第一個算不出成長
                      column(width = 6,
                             selectInput('detail_time_growth2', 
                                         label = "用電成長里查詢(雙月制) - 當期：", 
                                         choices = unique(power$統計年月),
                                         selected = 10407))
                    ),
                    selectInput('detail_n_growth', 
                                label = "用電成長幅度前幾名：", 
                                choices = c(1:10),
                                selected = 5)
                )
              ),
              column(
                width = 8,
                tabBox(
                  width = NULL,
                  tabPanel(h5('用電大戶'), status = 'info', height = 400,
                           plotlyOutput("detail_big_plot")),
                  tabPanel(h5('用電成長大戶'), status = 'info', height = 400,
                           plotlyOutput("detail_growth_plot"))
                )
              )
            ),
            fluidRow(
              column(
                width = 4,
                box(width = NULL, status = 'info', height = 500, title = "省電大戶基本查詢:",
                    solidHeader = T,
                    selectInput('area_d', 
                                label = "選擇村里", 
                                choices = unique(power_area$鄉鎮),
                                selected = "北投區"),
                    selectInput('detail_time_small', 
                                label = "省電大戶里查詢(雙月制)：", 
                                choices = unique(power$統計年月),
                                selected = 10407),
                    selectInput('detail_n_small', 
                                label = "省電大戶前幾名：", 
                                choices = c(1:10),
                                selected = 5),
                    fluidRow(
                      column(width = 6,
                             selectInput('detail_time_decay1', 
                                         label = "省電成長里查詢(雙月制) - 基期：", 
                                         choices = unique(power$統計年月),
                                         selected = 10405)), # 第一個算不出成長
                      column(width = 6,
                             selectInput('detail_time_decay2', 
                                         label = "省電成長里查詢(雙月制) - 當期：", 
                                         choices = unique(power$統計年月),
                                         selected = 10407))
                    ),
                    selectInput('detail_n_decay', 
                                label = "省電成長幅度前幾名：", 
                                choices = c(1:10),
                                selected = 5)
                )
              ),
              column(
                width = 8,
                tabBox(
                  width = NULL,
                  tabPanel(h5('省電大戶'), status = 'info', height = 400,
                           plotlyOutput("detail_small_plot")),
                  tabPanel(h5('省電成長大戶'), status = 'info', height = 400,
                           plotlyOutput("detail_decay_plot"))
                )
              )
            )
    ),
    tabItem(tabName = "demo",
            fluidRow(
              column(
                width = 4,
                box(width = NULL, status = 'info', height = 500,
                    infoBoxOutput("approvalBox",width = 100),
                    selectInput('yearmonth', 
                                label = "選擇年月：", 
                                choices = unique(power$統計年月),
                                selected = 10407),
                    infoBoxOutput("人口特性",width = 100),
                    infoBoxOutput("單身小資",width = 100),
                    infoBoxOutput("高知識",width = 100),
                    infoBoxOutput("銀髮族",width = 100)
                )
              ),
              box(width = 8, status = 'info', height = 500, title = "選擇 :",
                  highchartOutput(outputId = "demo")
              )
            ),
            fluidRow(
              tags$iframe(src = "https://unityculturesheng.carto.com/viz/973a42ea-315b-11e6-b013-0e3a376473ab/embed_map",
                          width="100%" ,height="520"))
    )
  )
 )
)


server <- function(input, output) {
  output$overview_big_plot <- renderPlotly({
    power %>% 
      filter(統計年月 == input$overview_time_big) %>% 
      arrange(desc(每人平均用電度數)) %>% 
      slice(1:input$overview_n_big) %>% 
      ggplot(aes(x = reorder(行政區域, desc(每人平均用電度數)), y = 每人平均用電度數)) +
      geom_bar(stat = 'identity',fill= "#4595D6") + ## 加顏色崇甫
      theme_bw(base_family="STHeiti") +
      labs(x = '行政區域') + 
      theme(axis.text.x=element_text(angle =20 ,size=10),
            axis.text.y=element_text(size=15),axis.title.x=element_text(size=15),
            axis.title.y=element_text(size=20),plot.title = element_text(size = rel(1.5)))-> plot_tmp
    ggplotly(plot_tmp)
  })
  
  output$overview_growth_plot <- renderPlotly({
    power %>% 
      filter(統計年月 %in% c(input$overview_time_growth1,input$overview_time_growth2)) %>%
      group_by(行政區域) %>% 
      arrange(desc(統計年月)) %>% 
      mutate(base = lead(每人平均用電度數, 1),
             用電量成長比例 = (每人平均用電度數 - base)/base) %>% 
      ungroup %>% 
      arrange(desc(用電量成長比例)) %>% 
      slice(1:input$overview_n_growth) %>% 
      ggplot(aes(x = reorder(行政區域, desc(用電量成長比例)), y = 用電量成長比例)) +
      geom_bar(stat = 'identity',fill= "#4595D6") + ## 加顏色崇甫
      theme_bw(base_family="STHeiti") +
      labs(x = '行政區域') + 
      theme(axis.text.x=element_text(angle =20 ,size=10),
            axis.text.y=element_text(size=15),axis.title.x=element_text(size=15),
            axis.title.y=element_text(size=20),plot.title = element_text(size = rel(1.5))) -> plot_tmp1
    ggplotly(plot_tmp1)
  })
  
  ## 省電大戶 by lofu
  output$overview_small_plot <- renderPlotly({
    power %>% 
      filter(統計年月 == input$overview_time_small) %>% 
      arrange(每人平均用電度數) %>% 
      slice(1:input$overview_n_small) %>% 
      ggplot(aes(x = reorder(行政區域,每人平均用電度數), y = 每人平均用電度數)) +
      geom_bar(stat = 'identity',fill= "#4595D6") + ## 加顏色崇甫
      theme_bw(base_family="STHeiti") +
      labs(x = '行政區域') + 
      theme(axis.text.x=element_text(angle =20 ,size=10),
            axis.text.y=element_text(size=15),axis.title.x=element_text(size=15),
            axis.title.y=element_text(size=20),plot.title = element_text(size = rel(1.5))) -> plot_tmp2
    ggplotly(plot_tmp2)
  })
  
  ## 省電成長大戶 by lofu
  output$overview_decay_plot <- renderPlotly({
    power %>% 
      filter(統計年月 %in% c(input$overview_time_decay1,input$overview_time_decay2)) %>%
      group_by(行政區域) %>% 
      arrange(desc(統計年月)) %>% 
      mutate(base = lead(每人平均用電度數, 1),
             省電量成長比例 = (每人平均用電度數 - base)/base) %>% 
      ungroup %>% 
      arrange(省電量成長比例) %>% 
      slice(1:input$overview_n_decay) %>% 
      ggplot(aes(x = reorder(行政區域, 省電量成長比例), y = 省電量成長比例)) +
      geom_bar(stat = 'identity',fill= "#4595D6") + ## 加顏色崇甫
      theme_bw(base_family="STHeiti") +
      labs(x = '行政區域') + 
      theme(axis.text.x=element_text(angle =45 ,size=10),
            axis.text.y=element_text(size=15),axis.title.x=element_text(size=15),
            axis.title.y=element_text(size=20),plot.title = element_text(size = rel(1.5))) -> plot_tmp3
    ggplotly(plot_tmp3)
  })
  
  output$yearmap <- renderPlotly({
    power %>% 
      filter(統計年月 == input$overview_time_big) %>% 
      arrange(desc(每人平均用電度數)) %>% 
      slice(1:input$overview_n_big) %$% 行政區域 -> vname_big
    
    power %>% 
      filter(行政區域 %in% vname_big) %>% 
      ggplot(aes(x = 統計年月 %>% as.factor() , y = 每人平均用電度數 , group = 行政區域 , color = 行政區域)) + 
      geom_line() + 
      geom_point() + 
      theme_bw(base_family="STHeiti") + 
      labs(x = '統計年月', title = "歷年月每人平均用電度數") + 
      theme(axis.text.x=element_text(angle =20 ,size=10),
            axis.text.y=element_text(size=15),axis.title.x=element_text(size=15),
            axis.title.y=element_text(size=20),plot.title = element_text(size = rel(1.5)))+
      coord_cartesian(ylim = c(250,1200)) -> temp
      ggplotly(temp)
    
  })
  
  output$yearmap1 <- renderPlotly({
    power %>% 
      filter(統計年月 == input$overview_time_small) %>% 
      arrange(每人平均用電度數) %>% 
      slice(1:input$overview_n_small) %$% 行政區域 -> vname_small
    
    power %>% 
      filter(行政區域 %in% vname_small) %>% 
      ggplot(aes(x = 統計年月 %>% as.factor() , y = 每人平均用電度數 , group = 行政區域 , color = 行政區域)) + 
      geom_line() + 
      geom_point() + 
      theme_bw(base_family="STHeiti") + 
      labs(x = '統計年月', title = "歷年月每人平均省電度數") + 
      theme(axis.text.x=element_text(angle =20 ,size=10),
            axis.text.y=element_text(size=15),axis.title.x=element_text(size=15),
            axis.title.y=element_text(size=20),plot.title = element_text(size = rel(1.5)))+
      coord_cartesian(ylim = c(0,500)) -> temp_small
    ggplotly(temp_small)
    
  })
  
  output$map_big <- renderLeaflet({
    power %>% 
      filter(統計年月 == input$overview_time_big) %>% 
      arrange(desc(每人平均用電度數)) %>% 
      slice(1:input$overview_n_big) %$% 行政區域 -> vname_big
    leaflet(sfn %>% subset(paste0(sfn$T_Name, sfn$V_Name) %in% vname_big)) %>%
      addPolygons(
        stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5
      ) %>% 
      setView(lng = 121.536862, lat = 25.045235, zoom = 11) %>% addTiles()
 })
  
  output$map_growth <- renderLeaflet({
    power %>% 
      filter(統計年月 %in% c(input$overview_time_growth1,input$overview_time_growth2)) %>%
      group_by(行政區域) %>% 
      arrange(desc(統計年月)) %>% 
      mutate(base = lead(每人平均用電度數, 1),
             用電量成長比例 = (每人平均用電度數 - base)/base) %>% 
      ungroup %>% 
      arrange(desc(用電量成長比例)) %>% 
      slice(1:input$overview_n_growth) %$% 行政區域 -> vname_growth
    leaflet(sfn %>% subset(paste0(sfn$T_Name, sfn$V_Name) %in% vname_growth)) %>%
      addPolygons(
        stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5
      ) %>% 
      setView(lng = 121.536862, lat = 25.045235, zoom = 11) %>% addTiles()
  })
  
  ## 省電地圖
  output$map_sml <- renderLeaflet({
    power %>% 
      filter(統計年月 == input$overview_time_small) %>% 
      arrange(desc(每人平均用電度數)) %>% 
      slice(1:input$overview_n_small) %$% 行政區域 -> vname_big
    leaflet(sfn %>% subset(paste0(sfn$T_Name, sfn$V_Name) %in% vname_big)) %>%
      addPolygons(
        stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5
      ) %>% 
      setView(lng = 121.536862, lat = 25.045235, zoom = 11) %>% addTiles()
  })
  
  output$map_sml_growth <- renderLeaflet({
    power %>% 
      filter(統計年月 %in% c(input$overview_time_decay1,input$overview_time_decay2)) %>%
      group_by(行政區域) %>% 
      arrange(desc(統計年月)) %>% 
      mutate(base = lead(每人平均用電度數, 1),
             省電量成長比例 = (每人平均用電度數 - base)/base) %>% 
      ungroup %>% 
      arrange(省電量成長比例) %>% 
      slice(1:input$overview_n_decay) %$% 行政區域 -> vname_sml_growth
    leaflet(sfn %>% subset(paste0(sfn$T_Name, sfn$V_Name) %in% vname_sml_growth)) %>%
      addPolygons(
        stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5
      ) %>% 
      setView(lng = 121.536862, lat = 25.045235, zoom = 11) %>% addTiles()
  })
  
  output$big_small_competion <- renderHighchart({
    data %>% 
      filter(統計年月 == input$yearmonth) %>%
      select(行政區域,每戶平均用電度數,
                 女性比例, 男性比例,
                 少年人口比例,青年人口比例,壯年人口比例,老年人口比例,總人數,有偶人數,
                 綜合所得中位數, 大學以上比例, 大學比例, 大學以下比例) %>% 
      mutate(扶老比_log = log10((1-青年人口比例-壯年人口比例-少年人口比例)/(青年人口比例+壯年人口比例)),
                 有偶比例 = 有偶人數/((1-少年人口比例)*總人數),
                 平均教育程度 = 大學以上比例*6+大學比例*4+大學以下比例*0,
                 女男比 = round(女性比例/男性比例,4),
                 綜合所得中位數_log = log10(綜合所得中位數),
                 每戶平均用電度數_log = log10(每戶平均用電度數)) %>% 
      select(行政區域,綜合所得中位數_log,平均教育程度,有偶比例,女男比,扶老比_log,每戶平均用電度數_log) %>%
      mutate_each(funs(scale(.)),-行政區域) %>% #崇甫續寫
      gather('index','value',-行政區域) %>% 
      group_by(index) %>% 
      mutate(value = (value-min(value))/(max(value)-min(value))) %>% 
      spread(index,value) %>% 
      arrange(desc(每戶平均用電度數_log)) %>% #挑出最上與最下
      slice(1:input$n_big) %>% 
      select(2:7) %>% 
      summarise_each(funs(mean)) %>% 
      mutate(group = "top") %>% 
      bind_rows(
        data %>% 
          filter(統計年月 == input$yearmonth) %>% 
          select(行政區域,每戶平均用電度數,
                     女性比例, 男性比例,
                     少年人口比例,青年人口比例,壯年人口比例,老年人口比例,總人數,有偶人數,
                     綜合所得中位數, 大學以上比例, 大學比例, 大學以下比例) %>% 
          mutate(扶老比_log = log10((1-青年人口比例-壯年人口比例-少年人口比例)/(青年人口比例+壯年人口比例)),
                     有偶比例 = 有偶人數/((1-少年人口比例)*總人數),
                     平均教育程度 = 大學以上比例*6+大學比例*4+大學以下比例*0,
                     女男比 = round(女性比例/男性比例,4),
                     綜合所得中位數_log = log10(綜合所得中位數),
                     每戶平均用電度數_log = log10(每戶平均用電度數)) %>% 
          select(行政區域,綜合所得中位數_log,平均教育程度,有偶比例,女男比,扶老比_log,每戶平均用電度數_log) %>%
          mutate_each(funs(scale(.)),-行政區域) %>% #崇甫續寫 
          gather('index','value',-行政區域) %>% 
          group_by(index) %>% 
          mutate(value = (value-min(value))/(max(value)-min(value))) %>% 
          spread(index,value) %>% 
          arrange(每戶平均用電度數_log) %>% #挑出最上與最下
          slice(1:input$n_small) %>% 
          select(2:7) %>% 
          summarise_each(funs(mean)) %>% 
          mutate(group = "down")
      ) %>% 
      gather(index,value,1:6) %>% 
      spread(group,value) %>% as.data.frame()-> temp_rada
    
    highchart() %>% 
      hc_chart(polar = TRUE, type = "line") %>% 
      hc_title(text = "用電大戶與省電大戶的人口特性比較") %>% 
      hc_xAxis(categories = temp_rada$index,
               tickmarkPlacement = 'on',
               lineWidth = 0) %>% 
      hc_yAxis(gridLineInterpolation = 'polygon',
               lineWidth = 0,
               min = 0, max = 1) %>% 
      hc_series(
        list(
          name = "省電大戶",
          data = temp_rada[,c(2)],
          pointPlacement = 'on',color=col.raw[8]),
        list(
          name = "用電大戶",
          data = temp_rada[,c(3)],
          pointPlacement = 'on',color='#474747')
      )
  })
  
  ################### 各行政區part #####
  output$detail_big_plot <- renderPlotly({
    power_area %>% 
      filter(統計年月 == input$detail_time_big) %>% 
      filter(鄉鎮 == input$area_c) %>% 
      arrange(desc(每人平均用電度數)) %>% 
      slice(1:input$detail_n_big) %>% 
      ggplot(aes(x = reorder(行政區域, desc(每人平均用電度數)), y = 每人平均用電度數)) +
      geom_bar(stat = 'identity',fill= "#4595D6") + ## 加顏色崇甫
      theme_bw(base_family="STHeiti") +
      labs(x = '行政區域') + 
      theme(axis.text.x=element_text(angle =20 ,size=10),
            axis.text.y=element_text(size=15),axis.title.x=element_text(size=15),
            axis.title.y=element_text(size=20),plot.title = element_text(size = rel(1.5))) -> plot_tmp_area
    ggplotly(plot_tmp_area)
  })
  
  output$detail_growth_plot <- renderPlotly({
    power_area %>% 
      filter(統計年月 %in% c(input$detail_time_growth1,input$detail_time_growth2)) %>%
      filter(鄉鎮 == input$area_c) %>% 
      group_by(行政區域) %>% 
      arrange(desc(統計年月)) %>% 
      mutate(base = lead(每人平均用電度數, 1),
             用電量成長比例 = (每人平均用電度數 - base)/base) %>% 
      ungroup %>% 
      arrange(desc(用電量成長比例)) %>% 
      slice(1:input$detail_n_growth) %>% 
      ggplot(aes(x = reorder(行政區域, desc(用電量成長比例)), y = 用電量成長比例)) +
      geom_bar(stat = 'identity',fill= "#4595D6") + ## 加顏色崇甫
      theme_bw(base_family="STHeiti") +
      labs(x = '行政區域') + 
      theme(axis.text.x=element_text(angle =20 ,size=10),
            axis.text.y=element_text(size=15),axis.title.x=element_text(size=15),
            axis.title.y=element_text(size=20),plot.title = element_text(size = rel(1.5))) -> plot_tmp_area1
    ggplotly(plot_tmp_area1)
  })
  
  ## 省電大戶 by lofu
  output$detail_small_plot <- renderPlotly({
    power_area %>% 
      filter(統計年月 == input$detail_time_small) %>% 
      filter(鄉鎮 == input$area_d) %>% 
      arrange(每人平均用電度數) %>% 
      slice(1:input$detail_n_small) %>% 
      ggplot(aes(x = reorder(行政區域,每人平均用電度數), y = 每人平均用電度數)) +
      geom_bar(stat = 'identity',fill= "#4595D6") + ## 加顏色崇甫
      theme_bw(base_family="STHeiti") +
      labs(x = '行政區域') + 
      theme(axis.text.x=element_text(angle =20 ,size=10),
            axis.text.y=element_text(size=15),axis.title.x=element_text(size=15),
            axis.title.y=element_text(size=20),plot.title = element_text(size = rel(1.5))) -> plot_tmp_area2
    ggplotly(plot_tmp_area2)
  })
  
  ## 省電成長大戶 by lofu
  output$detail_decay_plot <- renderPlotly({
    power_area %>% 
      filter(統計年月 %in% c(input$detail_time_decay1,input$detail_time_decay2)) %>%
      filter(鄉鎮 == input$area_d) %>% 
      group_by(行政區域) %>% 
      arrange(desc(統計年月)) %>% 
      mutate(base = lead(每人平均用電度數, 1),
             省電量成長比例 = (每人平均用電度數 - base)/base) %>% 
      ungroup %>% 
      arrange(省電量成長比例) %>% 
      slice(1:input$detail_n_decay) %>% 
      ggplot(aes(x = reorder(行政區域, 省電量成長比例), y = 省電量成長比例)) +
      geom_bar(stat = 'identity',fill= "#4595D6") + ## 加顏色崇甫
      theme_bw(base_family="STHeiti") +
      labs(x = '行政區域') + 
      theme(axis.text.x=element_text(angle =20 ,size=10),
            axis.text.y=element_text(size=15),axis.title.x=element_text(size=15),
            axis.title.y=element_text(size=20),plot.title = element_text(size = rel(1.5))) -> plot_tmp_area3
    ggplotly(plot_tmp_area3)
  })
  
  output$demo <- renderHighchart({
    hcmix
  })
  
  output$人口特性 <- renderInfoBox({
    infoBox(
      "Property of Population", paste0("Six Special Index",""), icon = icon("users"),
      color = "yellow",fill =TRUE
    )
  })
  output$單身小資 <- renderInfoBox({
    infoBox(
      "Cluster-5",p("。Single",br(),"。Female"), icon = icon("female"),
      color = "red",fill =TRUE
    )
  })
  output$高知識 <- renderInfoBox({
    infoBox(
      "Cluster-1", p("。High-Education",br(),"。High-Income"), icon = icon("money"),
      color = "green",fill =TRUE
    )
  })
  
  output$銀髮族 <- renderInfoBox({
    infoBox(
      "Cluster-3", p("。Elderly",br(),"。Low-Income"), icon = icon("battery-empty"),
      color = "light-blue",fill =TRUE
    )
  })
}

shinyApp(ui, server) 













