library(shiny)
library(shinydashboard)
library(plotly)
library(dashboardthemes)
library(shinydashboardPlus)
library(dplyr)
library(sf)
library(DT)
library(ggplot2)
library(plotly)
library(shinyjs)

source("kpsource.R")

#header
dropdownMenuCustom <- function() {
  tags$li(class = "dropdown",
          tags$a(href = "https://jatim.lapor.go.id/instansi/pemerintah-provinsi-jawa-timur", 
                 target = "_blank", 
                 icon("right-to-bracket"), 
                 "Website SP4N Lapor")
  )
}
header <- dashboardHeader(title = tagList(span(class = "logo-lg", "Dashboard"),
                                          img(
                                            src = "https://raw.githubusercontent.com/akbaarrmaulana/akbaarrepo/main/lapor.png",
                                            style = "width: 35px"
                                          )),
                          titleWidth = 320,
                          dropdownMenuCustom())
sidebar <- dashboardSidebar(width = 320,
                            sidebarMenu(
                              menuItem("Tentang Dashboard",
                                       tabName = "span",
                                       icon = icon("mobile-screen-button")),
                              menuItem("Statistik Tingkat Provinsi",
                                       tabName = "home",
                                       icon = icon("hotel")),
                              menuItem("Statistik Tingkat Kabupaten/Kota",
                                       tabName = "kabkot",
                                       icon = icon("building"))
                            ))
body <- dashboardBody(
  shinyDashboardThemeDIY(
    ### General
    appFontFamily = "Arial",
    appFontColor = "rgb(0,0,0)",
    primaryFontColor = "rgb(0,0,0)",
    infoFontColor = "rgb(0,0,0)",
    successFontColor = "rgb(0,0,0)",
    warningFontColor = "rgb(0,0,0)",
    dangerFontColor = "rgb(0,0,0)",
    bodyBackColor = "rgb(248,248,248)",
    
    ### Header
    logoBackColor = "rgb(128,0,0)",  # Warna maroon untuk logo
    headerButtonBackColor = "rgb(238,238,238)",
    headerButtonIconColor = "rgb(75,75,75)",
    headerButtonBackColorHover = "rgb(210,210,210)",
    headerButtonIconColorHover = "rgb(0,0,0)",
    
    headerBackColor = "rgb(128,0,0)",  # Warna maroon untuk header
    headerBoxShadowColor = "#aaaaaa",
    headerBoxShadowSize = "2px 2px 2px",
    
    ### Sidebar
    sidebarBackColor = cssGradientThreeColors(
      direction = "down",
      colorStart = "rgb(128,0,0)",  # Maroon atas
      colorMiddle = "rgb(153,0,0)",  # Gradasi lebih terang di tengah
      colorEnd = "rgb(102,0,0)",  # Maroon lebih gelap di bawah
      colorStartPos = 0,
      colorMiddlePos = 50,
      colorEndPos = 100
    ),
    sidebarPadding = 0,
    
    sidebarMenuBackColor = "transparent",
    sidebarMenuPadding = 0,
    sidebarMenuBorderRadius = 0,
    
    sidebarShadowRadius = "3px 5px 5px",
    sidebarShadowColor = "#aaaaaa",
    
    sidebarUserTextColor = "rgb(255,255,255)",
    
    sidebarSearchBackColor = "rgb(153,0,0)",  # Warna maroon terang
    sidebarSearchIconColor = "rgb(255,255,255)",
    sidebarSearchBorderColor = "rgb(153,0,0)",
    
    sidebarTabTextColor = "rgb(255,255,255)",
    sidebarTabTextSize = 13,
    sidebarTabBorderStyle = "none none solid none",
    sidebarTabBorderColor = "rgb(102,0,0)",  # Maroon gelap untuk border tab
    sidebarTabBorderWidth = 1,
    
    sidebarTabBackColorSelected = cssGradientThreeColors(
      direction = "right",
      colorStart = "rgb(179,0,0)",  # Gradasi maroon terang saat dipilih
      colorMiddle = "rgb(153,0,0)",
      colorEnd = "rgb(128,0,0)",
      colorStartPos = 0,
      colorMiddlePos = 30,
      colorEndPos = 100
    ),
    sidebarTabTextColorSelected = "rgb(255,255,255)",
    sidebarTabRadiusSelected = "0px 20px 20px 0px",
    
    sidebarTabBackColorHover = cssGradientThreeColors(
      direction = "right",
      colorStart = "rgb(153,0,0)",  # Gradasi maroon lebih terang saat hover
      colorMiddle = "rgb(128,0,0)",
      colorEnd = "rgb(102,0,0)",
      colorStartPos = 0,
      colorMiddlePos = 30,
      colorEndPos = 100
    ),
    sidebarTabTextColorHover = "rgb(255,255,255)",
    sidebarTabBorderStyleHover = "none none solid none",
    sidebarTabBorderColorHover = "rgb(102,0,0)",
    sidebarTabBorderWidthHover = 1,
    sidebarTabRadiusHover = "0px 20px 20px 0px",
    
    ### Boxes
    boxBackColor = "rgb(255,255,255)",
    boxBorderRadius = 5,
    boxShadowSize = "0px 1px 1px",
    boxShadowColor = "rgba(0,0,0,.1)",
    boxTitleSize = 16,
    boxDefaultColor = "rgb(210,214,220)",
    boxPrimaryColor = "rgba(44,222,235,1)",
    boxInfoColor = "rgb(210,214,220)",
    boxSuccessColor = "rgba(0,255,213,1)",
    boxWarningColor = "rgb(244,156,104)",
    boxDangerColor = "rgb(255,88,55)",
    
    tabBoxTabColor = "rgb(255,255,255)",
    tabBoxTabTextSize = 14,
    tabBoxTabTextColor = "rgb(0,0,0)",
    tabBoxTabTextColorSelected = "rgb(0,0,0)",
    tabBoxBackColor = "rgb(255,255,255)",
    tabBoxHighlightColor = "rgba(44,222,235,1)",
    tabBoxBorderRadius = 5,
    
    ### Inputs
    buttonBackColor = "rgb(245,245,245)",
    buttonTextColor = "rgb(0,0,0)",
    buttonBorderColor = "rgb(200,200,200)",
    buttonBorderRadius = 5,
    
    buttonBackColorHover = "rgb(235,235,235)",
    buttonTextColorHover = "rgb(100,100,100)",
    buttonBorderColorHover = "rgb(200,200,200)",
    
    textboxBackColor = "rgb(255,255,255)",
    textboxBorderColor = "rgb(200,200,200)",
    textboxBorderRadius = 5,
    textboxBackColorSelect = "rgb(245,245,245)",
    textboxBorderColorSelect = "rgb(200,200,200)",
    
    ### Tables
    tableBackColor = "rgb(255,255,255)",
    tableBorderColor = "rgb(240,240,240)",
    tableBorderTopSize = 1,
    tableBorderRowSize = 1
  ),
  tags$style(HTML("
      .info-box { min-height: 125px; }  /* Set the desired height here */
      .info-box-icon { height: 125px; line-height: 125px; }
      
      .box.box-solid.box-success>.box-header {
        color:#fff;
        background:#800000
      }
      .box.box-solid.box-success{
      border-bottom-color:#fff;
      border-left-color:#fff;
      border-right-color:#fff;
      border-top-color:#fff;
      }
      
      .box.box-success .box-title {
          color: white !important;
      }
      .box.box-primary .box-header {
        display: none;
      }
      .box.box-primary {
        border-right: 2px solid #d3d3d3; 
        border-bottom: 2px solid #d3d3d3;
        border-top: 2px solid #ffffff;
        border-left: 2px solid #ffffff;
      }
    ")),
  tags$script(HTML("
      $(document).on('click', '.nav-item', function() {
        var text = $(this).find('.nav-text-left').text().trim();
        Shiny.setInputValue('selectedNav', text);
      });
    ")),
  
  # Custom CSS for styling
  tags$style(HTML("
      .nav-item {
        display: flex;
        justify-content: space-between;
        padding: 10px;
        border-bottom: 1px solid #e5e5e5;
        cursor: pointer;
      }
      .nav-item:hover {
        background-color: maroon;  /* Change hover background color to maroon */
        color: white;  /* Change text color on hover */
      }
      .nav-text-left {
        font-weight: bold;
        color: #333;
      }
      .nav-text-right {
        color: maroon;  /* Set number color to maroon */
        font-weight: bold;
      }
      .nav-item:hover .nav-text-left,
      .nav-item:hover .nav-text-right {
        color: white;  /* Change both text and number color to white on hover */
      }
      
      .full-width-button {
          width: 100%;
          background-color: #800000; /* Maroon color */
          color: white;
          font-weight: bold;
          border-radius: 8px;
          padding: 10px;
          font-size: 16px;
        }
        .centered-buttons {
          display: flex;
          justify-content: center;
          margin-bottom: 10px;
        }
      
    ")),
  tags$style(HTML("
    .content-wrapper, .right-side {
        overflow-y: auto;
    }
    .content {
        overflow-y: auto;
    }
  ")),
  div(
    id="main_content",
    tabItems(
      tabItem(
        tabName = "span",
        titlePanel(
          h2(strong("Dashboard Pengaduan Pelayanan Publik Nasional"),
             style="text-align:center;",style = "margin-bottom:-20px;",style = "margin-top:-20px;")),
        br(),
        fluidRow(
          box(title = strong("Tentang Dashboard"), width = 12,
              p("Dashboard Pengaduan Pelayanan Publik Nasional adalah sebuah alat interaktif yang 
                dirancang untuk menyajikan", tags$strong("visualisasi data pengaduan publik di Jawa Timur"),". Dengan 
                tujuan meningkatkan transparansi dan akuntabilitas dalam pengelolaan pengaduan, 
                dashboard ini menawarkan berbagai grafik dan analisis yang memudahkan pemantauan 
                dan evaluasi data pengaduan serta aspirasi masyarakat.
                Dashboard ini adalah bagian dari dukungan terhadap",
                tags$strong("Sistem Pengelolaan Pengaduan Pelayanan Publik Nasional (SP4N) Layanan Aspirasi dan 
                Pengaduan Online Rakyat (LAPOR!)"), "yang berfokus pada 
                aspek-aspek terkait pengelolaan pengaduan. Penggunaan dashboard ini diharapkan 
                dapat memberikan wawasan yang lebih baik bagi pengambil keputusan dan pemangku 
                kepentingan dalam memahami tren pengaduan serta merumuskan kebijakan yang lebih 
                responsif terhadap kebutuhan masyarakat. Dengan fitur-fitur yang mudah digunakan, 
                dashboard ini bertujuan untuk mempercepat proses analisis dan meningkatkan kualitas 
                layanan publik di Jawa Timur.",
                style = "text-align:justify"),
              p("Dalam tampilan utama dashboard, pengguna dapat melihat pilihan menu untuk", tags$strong("Statistik Tingkat 
                Provinsi"), "dan", tags$strong("Statistik Tingkat Kabupaten/Kota"),", yang memungkinkan pemantauan pengaduan berdasarkan 
                wilayah. Di halaman ini juga disediakan video panduan yang membantu pengguna memahami fungsi dan 
                tujuan SP4N, yaitu untuk meningkatkan transparansi dan efektivitas dalam penanganan pengaduan 
                publik.",
                style = "text-align:justify")
              )
        ),
        fluidRow(
          box(title = strong("Detail Informasi SP4N LAPOR!"),
              column(6, div(class = "centered-buttons", actionButton("web", "Tentang LAPOR!", 
                                                                     class = "full-width-button",
                                                                     onclick = "window.open('https://www.lapor.go.id/tentang', '_blank')", 
                                                                     class = "btn-primary"))),
              column(6, div(class = "centered-buttons", actionButton("vid", "Video LAPOR!", 
                                                                     class = "full-width-button",
                                                                     onclick = "window.open('https://youtu.be/pVJlNv0Zgd0?si=v4suxzhQ8Zde4gDj', '_blank')", 
                                                                     class = "btn-primary"))),
              width = 12)
        ),
        fluidRow(
          box(title = strong("Definisi Operasional Variabel"), width = 12, 
              DTOutput("definitionTable"))
        )
      ),
      tabItem(
        tabName = "home",
        fluidRow(
          width = 12,
          div(box(selectInput("tahun","Tahun",
                              choices = c("All",unique(data$Tahun)), selected = "2023"),
                  width = 2, 
                  status = "primary",height = 70),
              box(selectInput("bulan","Bulan",
                              choices = c("All",unique(data$Bulan))),
                  width = 2, 
                  status = "primary",height = 90),
              box(selectInput("var","Status Pengaduan",
                              choices = coln),width = 2,
                  status = "primary",height = 90),
              infoBoxOutput(outputId = "jml", width = 6),
              style = "margin-top:20px")),
        fluidRow(
          column(width = 7,
                 box(
                   title = uiOutput("dmap"),
                   width = 12,
                   height = "150px",
                   plotlyOutput("map"),
                   solidHeader = T,
                   status = "success"
                 )),
          column(width = 5,
                 box(
                   title = uiOutput("toph"),
                   radioButtons(inputId = "radbut", " ", choices = c("Tertinggi","Terendah"),inline = T),
                   width = 12,
                   height = "auto",
                   plotOutput("bar", height = "350px"),
                   solidHeader = T,
                   status = "success"
                 ))
        ),
        fluidRow(
          column(
            width = 7,
            box(
              title = uiOutput("tbark"),
              width = 12,
              height = "150px",
              plotOutput("bark"),
              solidHeader = T,
              status = "success"
            )
          ),
          column(
            width = 5,
            box(
              title = uiOutput("tbarko"),
              width = 12,
              height = "150px",
              plotOutput("barko"),
              solidHeader = T,
              status = "success"
            )
          )
        )
      ),
      tabItem(
        tabName = "kabkot",
        fluidRow(
          box(
            width = 12,
            title = NULL,
            status = "primary",
            solidHeader = F,
            column(width = 2,
                   selectInput("kk","Kabupaten/Kota",
                               choices = unique(dataa$kabkott), selected = "Kota Surabaya"),
                   selectInput("tahun2","Tahun",
                               choices = c("All",unique(data$Tahun))),
                   selectInput("bulan2","Bulan",
                               choices = c("All",unique(data$Bulan)))),
            column(width = 3,
                   div(imageOutput("logopr",
                                   width = "100%", height = "120px"), 
                       style = "margin-left:30px")),
            column(width = 4,
                   div(class = "nav-item",
                       div(class = "nav-text-left", "Total Pengaduan"),
                       div(class = "nav-text-right", textOutput("t1"))
                   ),
                   div(class = "nav-item",
                       div(class = "nav-text-left", "Pengaduan Belum Terverifikasi"),
                       div(class = "nav-text-right", textOutput("t2"))  
                   ),
                   div(class = "nav-item",
                       div(class = "nav-text-left", "Pengaduan Belum Ditindaklanjuti"),
                       div(class = "nav-text-right", textOutput("t3"))  
                   ),
                   div(class = "nav-item",
                       div(class = "nav-text-left", "Pengaduan Diproses"),
                       div(class = "nav-text-right", textOutput("t4"))  
                   )
                   ),
            column(width = 3,
                   div(class = "nav-item",
                       div(class = "nav-text-left", "Pengaduan Selesai"),
                       div(class = "nav-text-right", textOutput("t5"))  
                   ),
                   div(class = "nav-item",
                       div(class = "nav-text-left", "Pengaduan Ditunda"),
                       div(class = "nav-text-right", textOutput("t6"))  
                   ),
                   div(class = "nav-item",
                       div(class = "nav-text-left", "Pengaduan Diarsip"),
                       div(class = "nav-text-right", textOutput("t7"))  
                   )
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            box(
              title = uiOutput("tline"),
              width = 12,
              height = "150px",
              plotOutput("cline"),
              solidHeader = T,
              status = "success"
            )
          ),
        )
      )
    )
  )
)
ui = dashboardPage(header = header,
                   sidebar = sidebar,
                   body = body)

server <- function(input,output,session){
  dataserver <- reactive({
    if(input$bulan=="All"){
      if(input$tahun=="All"){
        ds <- data
      }else if(input$tahun==2023){
        ds <- df2023
      }else{
        ds <- df2024
      }
    }else{
      fil <- paste(input$bulan, input$tahun, sep = " ")
      ds <- data %>% filter(periode_update==fil)
    }
    ds
  })
  
  datakota <- reactive({
    if(input$bulan=="All"){
      if(input$tahun=="All"){
        ds <- kota
      }else if(input$tahun==2023){
        ds <- kota2023
      }else{
        ds <- kota2024
      }
    }else{
      fil <- paste(input$bulan, input$tahun, sep = " ")
      ds <- kota %>% filter(periode_update==fil)
    }
    ds
  })
  datakab <- reactive({
    if(input$bulan=="All"){
      if(input$tahun=="All"){
        ds <- kab
      }else if(input$tahun==2023){
        ds <- kab2023
      }else{
        ds <- kab2024
      }
    }else{
      fil <- paste(input$bulan, input$tahun, sep = " ")
      ds <- kab %>% filter(periode_update==fil)
    }
    ds
  })
  
   output$jml <- renderValueBox({
    if(input$var=="All"){
      tit <- tags$h4(paste("Total Pengaduan"), style = "font-Weight:bold")
    }else{
      tit <- tags$h4(paste("Jumlah Pengaduan ",input$var), style = "font-Weight:bold")
    }
     if(input$tahun=="All"){
      st = "2023-2024"
      a1 <- data %>%
        group_by(`KabupatenKota`) %>%
        summarise(X= sum(get(input$var)))
      a2  <- sum(a1$X)
    }else if(input$bulan=="All"){
      st = input$tahun
      if(input$tahun==2023){
        a1 <- df2023 %>%
          group_by(`KabupatenKota`) %>%
          summarise(X= sum(get(input$var)))
        a2 <- sum(a1$X)
      }else if(input$tahun==2024){
        a1 <- df2024 %>%
          group_by(`KabupatenKota`) %>%
          summarise(X= sum(get(input$var)))
        a2 <- sum(a1$X)
      }
    }else{
      fil <- paste(input$bulan, input$tahun, sep = " ")
      df1 <- data %>% filter(periode_update==fil)
      a2 <- df1 %>% summarise(sum(get(input$var)))
      st <- fil
    }
    infoBox(tit,
            value = tags$h6(paste("Tahun",st)),
            subtitle = tags$h4(paste(sum(a2))),
            color = "maroon",
            fill = F,
            icon = icon("comments"))
  })
   
   output$dmap <- renderUI({
     if(input$tahun=="All"){
       a <- "2023-2024"
     }else{
       a <- input$tahun
     }
     if(input$bulan=="All"){
       b <- ""
     }else{
       b <- input$bulan
     }
     
     if(input$var=="All"){
       tit <- paste("Sebaran Total Pengaduan",b,a)
     }else{
       tit <- paste("Sebaran Pengaduan",input$var,b,a)
     }
     strong(tit)
   })
   
   output$toph <- renderUI({
     if(input$tahun=="All"){
       a <- "2023-2024"
     }else{
       a <- input$tahun
     }
     if(input$bulan=="All"){
       b <- ""
     }else{
       b <- input$bulan
     }
     if(input$var=="All"){
       tit <- paste("Top 5 Pengaduan",input$radbut,b,a)
     }else{
       tit <- paste("Top 5 Pengaduan",input$var,input$radbut,b,a)
     }
     strong(tit)
   })
   
   output$map <- renderPlotly({
     if(input$tahun=="All"){
       a1 <- data %>%
         group_by(`KabupatenKota`) %>%
         summarise(X= sum(get(input$var)))
       st <- ""
     }else if(input$bulan=="All"){
       st <- input$tahun
       if(input$tahun==2023){
         a1 <- df2023 %>%
           group_by(`KabupatenKota`) %>%
           summarise(X= sum(get(input$var)))
       }else if(input$tahun==2024){
         a1 <- df2024 %>%
           group_by(`KabupatenKota`) %>%
           summarise(X= sum(get(input$var)))
       }
     }else{
       fil <- paste(input$bulan, input$tahun, sep = " ")
       st <- fil
       a1 <- data %>% filter(periode_update==fil) %>%
         group_by(`KabupatenKota`) %>%
         summarise(X=sum(get(input$var)))
     }
     
     merged_data <- jatim %>%
       left_join(a1, by = c("ADM2_EN" = "KabupatenKota"))
     
     p2 <- ggplot(data = merged_data) +
       geom_sf(aes(fill = X, text = ADM2_EN))+
       scale_fill_gradient(low = "white", high = "maroon")+
       theme_minimal()+
         xlim(111, 115) + 
         ylim(9, 6.5) + 
       theme(plot.title = element_text(hjust = 0.5, size = 20))
     ggplotly(p2)
   })
   
   output$bar <- renderPlot({
     if(input$radbut=="Terendah"){
       dtot <- dataserver() %>% 
         group_by(KabupatenKota) %>% 
         summarize(total_pengaduan = sum(get(input$var), na.rm = TRUE)) %>% 
         arrange(desc(total_pengaduan))%>% 
         slice_min(order_by = total_pengaduan, n = 5)
       arr <- -dtot$total_pengaduan
     }else{
       dtot <- dataserver() %>% 
         group_by(KabupatenKota) %>% 
         summarize(total_pengaduan = sum(get(input$var), na.rm = TRUE)) %>% 
         arrange(desc(total_pengaduan))%>% 
         slice_max(order_by = total_pengaduan, n = 5)
       arr <- dtot$total_pengaduan
     }
     p2 <- ggplot(dtot, aes(x = reorder(KabupatenKota, arr), y = total_pengaduan)) + 
       geom_bar(stat = "identity", fill = "maroon") +  
       labs(x = "Kabupaten/Kota", y = "Jumlah Pengaduan") +  
       theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, margin = margin(t = 10))) + 
       geom_text(aes(label = total_pengaduan), vjust = -0.5, 
                 color = "black", size = 5) +
       coord_flip()
     p2
   })
   
   max_barko_value <- reactive({
     dfgab <- rbind(datakota(),datakab())
       dtot <- dfgab %>% 
         group_by(KabupatenKota) %>% 
         summarize(total_pengaduan = sum(get(input$var), na.rm = TRUE)) %>% 
         arrange(desc(total_pengaduan))
    
     max(dtot$total_pengaduan, na.rm = TRUE)
   })
   
   output$bark <- renderPlot({
         dtot <- datakab() %>% 
           group_by(KabupatenKota) %>% 
           summarize(total_pengaduan = sum(get(input$var), na.rm = TRUE)) %>% 
           arrange(desc(total_pengaduan))
         arr <- -dtot$total_pengaduan
     
     ylim_max <- max_barko_value() * 1.05
     p3 <- ggplot(dtot, aes(x = reorder(KabupatenKota, arr), y = total_pengaduan)) + 
       geom_bar(stat = "identity", fill = "maroon") +  
       labs(x = "Kabupaten", y = "Jumlah Pengaduan") +  
       theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5, margin = margin(t = 10))) + 
       geom_text(aes(label = total_pengaduan), vjust = -0.5, 
                 color = "black", size = 3)+
       ylim(0,ylim_max)
     p3
   })
   
   output$barko <- renderPlot({
    dtot <- datakota() %>% 
         group_by(KabupatenKota) %>% 
         summarize(total_pengaduan = sum(get(input$var), na.rm = TRUE)) %>% 
         arrange(desc(total_pengaduan))
       arr <- -dtot$total_pengaduan
     ylim_max <- max_barko_value() * 1.05
     p4 <- ggplot(dtot, aes(x = reorder(KabupatenKota, arr), y = total_pengaduan)) + 
       geom_bar(stat = "identity", fill = "maroon") +  
       labs(x = "Kota", y = "Jumlah Pengaduan") +  
       theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5, margin = margin(t = 10))) + 
       geom_text(aes(label = total_pengaduan), vjust = -0.5, 
                 color = "black", size = 3)+
       ylim(0,ylim_max)
     p4
   })
   
   output$tbark <- renderUI({
     if(input$tahun=="All"){
       a <- "2023-2024"
     }else{
       a <- input$tahun
     }
     if(input$bulan=="All"){
       b <- ""
     }else{
       b <- input$bulan
     }
     if(input$var=="All"){
       tit <- paste("Jumlah Pengaduan Setiap Kabupaten -",b,a)
     }else{
       tit <- paste("Jumlah Pengaduan",input$var,"Setiap Kabupaten -",b,a)
     }
     strong(tit)
   })
   output$tbarko <- renderUI({
     if(input$tahun=="All"){
       a <- "2023-2024"
     }else{
       a <- input$tahun
     }
     if(input$bulan=="All"){
       b <- ""
     }else{
       b <- input$bulan
     }
     if(input$var=="All"){
       tit <- paste("Jumlah Pengaduan Setiap Kota -",b,a)
     }else{
       tit <- paste("Jumlah Pengaduan",input$var,"Setiap Kota -",b,a)
     }
     strong(tit)
   })
   
   output$logopr <- renderImage({
     ppp <- paste0("img/",input$kk,".png")
     list(src=ppp, deleteFile=F,
          height = 160,
          width = 160)
   })
   
   df2 <- reactive({
     aaa <- peng[peng$colm==input$selectedNav,"coln"]
     if(input$tahun2=="All"){
       datanya <- dataa %>% 
         group_by(kabkott) %>% 
         summarize(total_pengaduan = sum(aaa, na.rm = TRUE)) %>% 
         arrange(desc(total_pengaduan))
     }else if(input$tahun2==2023){
       datanya <- data2023 %>% 
         group_by(kabkott) %>% 
         summarize(total_pengaduan = sum(aaa, na.rm = TRUE)) %>% 
         arrange(desc(total_pengaduan))
     }else{
       datanya <- data2024 %>% 
         group_by(kabkott) %>% 
         summarize(total_pengaduan = sum(aaa, na.rm = TRUE)) %>% 
         arrange(desc(total_pengaduan))
     }
     datanya
   })
   
   df3 <- reactive({
     if(input$bulan2=="All"){
       if(input$tahun2=="All"){
         df <- dataat
       }else if(input$tahun2==2023){
         df <- df2023t
       }else{
         df <- df2024t
       }
     }else{
       if(input$tahun2=="All"){
         df <- dataat
       }else{
         fil <- paste(input$bulan2, input$tahun2, sep = " ")
         df <- dataa %>% filter(periode_update==fil) %>%
           group_by(kabkott)
       }
     }
     df
   })
   
   output$t1 <- renderText({
     df3()[df3()$kabkott==input$kk,"All"][[1]]
   })
   output$t2 <- renderText({
     df3()[df3()$kabkott==input$kk,"Belum Terverifikasi"][[1]]
   })
   output$t3 <- renderText({
     df3()[df3()$kabkott==input$kk,"Belum Ditindaklanjuti"][[1]]
   })
   output$t4 <- renderText({
     df3()[df3()$kabkott==input$kk,"Diproses"][[1]]
   })
   output$t5 <- renderText({
     df3()[df3()$kabkott==input$kk,"Selesai"][[1]]
   })
   output$t6 <- renderText({
     df3()[df3()$kabkott==input$kk,"Ditunda"][[1]]
   })
   output$t7 <- renderText({
     df3()[df3()$kabkott==input$kk,"Diarsip"][[1]]
   })
   

   output$cline <- renderPlot({
     if(input$tahun2=="All"){
       df <- dfk
       ti <- paste("Tren", input$selectedNav, "di",input$kk, "Tahun 2023-2024")
     }else if(input$tahun2==2023){
       df <- dfk23
       ti <- paste("Tren", input$selectedNav, "di",input$kk, "Tahun 2023")
     }else{
       df <- dfk24
       ti <- paste("Tren", input$selectedNav, "di",input$kk, "Tahun 2024")
     }
     
     
     aaa <- cat(peng[peng$colm==input$selectedNav,"coln"])
     df4 <- df %>% filter(kabkott==input$kk)
     if(input$selectedNav=="Total Pengaduan" || is.null(input$selectedNav)){
       ynya <- df4$All
     }else if(input$selectedNav=="Pengaduan Belum Terverifikasi"){
       ynya <- df4$`Belum Terverifikasi`
     }else if(input$selectedNav=="Pengaduan Belum Ditindaklanjuti"){
       ynya <- df4$`Belum Ditindaklanjuti`
     }else if(input$selectedNav=="Pengaduan Diproses"){
       ynya <- df4$Diproses
     }else if(input$selectedNav=="Pengaduan Selesai"){
       ynya <- df4$Selesai
     }else if(input$selectedNav=="Pengaduan Ditunda"){
       ynya <- df4$Ditunda
     }else{
       ynya <- df4$Diarsip
     }
     ggplot(df4, aes(x = periode_update, y = ynya)) +
       geom_line(color = "maroon", size = 1) +
       geom_point(color = "orange", size = 4) + # menambahkan titik pada garis
       labs(title = aaa,
            x = "Periode Update",
            y = "Jumlah Pengaduan") +
       theme_minimal() +
       scale_x_date(date_labels = "%b %y", date_breaks = "1 month")
   })
   output$tline <- renderUI({
     if(input$tahun2=="All"){
       a <- "2023-2024"
     }else{
       a <- input$tahun
     }
     if(input$bulan2=="All"){
       b <- ""
     }else{
       b <- input$bulan
     }
     strong(paste("Tren",input$selectedNav,"Bulanan",b,a))
   })
   
   output$definitionTable <- renderDT({
     datatable(
       defi,
       escape = FALSE, 
       options = list(pageLength = 10, dom = 't', ordering = FALSE),
       rownames = FALSE,
       style = "bootstrap" # This gives it the knitr-like look in shinydashboard
     )
   })
   
}

shinyApp(ui = ui, server = server, options = list(launch.browser = T))
