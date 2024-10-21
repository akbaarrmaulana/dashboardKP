library(shiny)
library(shinydashboard)
library(plotly)
library(dashboardthemes)
library(shinydashboardPlus)
library(dplyr)
library(sf)
library(ggplot2)
library(plotly)

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
header <- dashboardHeader(title = tagList(span(class = "logo-lg", "Dashboard SP4N Lapor"),
                                          img(
                                            src = "https://raw.githubusercontent.com/akbaarrmaulana/akbaarrepo/main/lapor.png",
                                            style = "width: 35px"
                                          )),
                          titleWidth = 320,
                          dropdownMenuCustom())
sidebar <- dashboardSidebar(width = 320,
                            sidebarMenu(
                              menuItem(".SP4N LAPOR",
                                       tabName = "span",
                                       icon = icon("mobile-screen-button")),
                              menuItem("Home",
                                       tabName = "home",
                                       icon = icon("home"))
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
    ")),
  div(
    id="main_content",
    tabItems(
      tabItem(
        tabName = "span",
        titlePanel(
          h2(strong("Sistem Pengelolaan Pengaduan Pelayanan Publik Nasional (SP4N) - Layanan Aspirasi dan Pengaduan Online Rakyat (LAPOR)"),
             style="text-align:center;",style = "margin-bottom:-20px;",style = "margin-top:-20px;")),
        br(),
        carousel(
          width = 12,
          id = "mycr",
          carouselItem(
            caption = "YouTube Video",
            tags$iframe(
              width = "100%",
              height = "500",
              src = "https://www.youtube.com/embed/pVJlNv0Zgd0",
              frameborder = "0",
              allowfullscreen = TRUE
            )
          )
          ),
        fluidRow(
          box(title = strong("Tentang LAPOR!"), width = 12,
              p("Pengelolaan pengaduan pelayanan publik di setiap organisasi penyelenggara di Indonesia belum terkelola secara efektif dan terintegrasi. 
                Masing-masing organisasi penyelenggara mengelola pengaduan secara parsial dan tidak terkoordinir dengan baik. Akibatnya terjadi duplikasi 
                penanganan pengaduan, atau bahkan bisa terjadi suatu pengaduan tidak ditangani oleh satupun organisasi penyelenggara, dengan alasan pengaduan 
                bukan kewenangannya. Oleh karena itu, untuk mencapai visi dalam good governance maka perlu untuk mengintegrasikan sistem pengelolaan pengaduan 
                pelayanan publik dalam satu pintu. Tujuannya, masyarakat memiliki satu saluran pengaduan secara Nasional.",
                style = "text-align:justify"),
              p("Untuk itu Pemerintah Republik Indonesia membentuk Sistem Pengelolaan Pengaduan Pelayanan Publik Nasional (SP4N) - Layanan Aspirasi dan 
                Pengaduan Online Rakyat (LAPOR!) adalah layanan penyampaian semua aspirasi dan pengaduan masyarakat Indonesia melalui beberapa kanal 
                pengaduan yaitu website www.lapor.go.id, SMS 1708 (Telkomsel, Indosat, Three), Twitter @lapor1708 serta aplikasi mobile (Android dan iOS). 
                Lembaga pengelola SP4N-LAPOR! adalah Kementerian Pendayagunaan Aparatur Negara dan Reformasi Birokrasi (Kementerian PANRB) sebagai Pembina 
                Pelayanan Publik, Kantor Staf Presiden (KSP) sebagai Pengawas Program Prioritas Nasional dan Ombudsman Republik Indonesia sebagai Pengawas 
                Pelayanan Publik. LAPOR! telah ditetapkan sebagai Sistem Pengelolaan Pengaduan Pelayanan Publik Nasional (SP4N) berdasarkan Peraturan Presiden 
                Nomor 76 Tahun 2013 dan Peraturan Menteri Pendayagunaan Aparatur Negara dan Reformasi Birokrasi Nomor 3 Tahun 2015.",
                style = "text-align:justify"),
              p("SP4N-LAPOR! dibentuk untuk merealisasikan kebijakan “no wrong door policy” yang menjamin hak masyarakat agar pengaduan dari manapun dan jenis 
                apapun akan disalurkan kepada penyelenggara pelayanan publik yang berwenang menanganinya. SP4N bertujuan agar:",
                style = "text-align:justify"),
              HTML(
                "
                <ul>
                  <li>Penyelenggara dapat mengelola pengaduan dari masyarakat secara sederhana, cepat, 
                  tepat, tuntas, dan terkoordinasi dengan baik;</li>
                  <li>Penyelenggara memberikan akses untuk partisipasi masyarakat dalam menyampaikan pengaduan; dan</li>
                  <li>Meningkatkan kualitas pelayanan publik.</li>
                "
              ),
              br(),
              p("Source : https://jatim.lapor.go.id"))
        )
      ),
      tabItem(
        tabName = "home",
        fluidRow(
          width = 12,
          div(box(selectInput("tahun","",
                              choices = c("All",unique(data$Tahun)), selected = "2023"),
                  width = 2, 
                  title = strong("Filter Tahun"),height = 70),
              box(selectInput("bulan","",
                              choices = c("All",unique(data$Bulan))),
                  width = 2, 
                  title = strong("Filter Bulan"),height = 90),
              box(selectInput("var","",
                              choices = coln),width = 3,
                  title = strong("Filter Status"),height = 90),
              infoBoxOutput(outputId = "jml", width = 5),
              style = "margin-top:20px")),
        fluidRow(
          column(width = 7,
                 box(
                   uiOutput("dmap"),
                   width = 12,
                   height = "150px",
                   plotlyOutput("map")
                 )),
          column(width = 5,
                 box(
                   radioButtons(inputId = "radbut", " ", choices = c("Minimum","Maximum"),inline = T),
                   width = 12,
                   height = "auto",
                   plotOutput("bar", height = "350px")
                 ))
        )
      )
    )
  )
)
ui = dashboardPage(header = header,
                   sidebar = sidebar,
                   body = body)

server <- function(input,output,session){
   output$jml <- renderValueBox({
    if(input$tahun=="All"){
      st = "All"
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
    infoBox(tags$h4(paste("Jumlah ",input$var), style = "font-Weight:bold"),
            value = tags$h6(st),
            subtitle = tags$h4(paste(sum(a2))),
            color = "red",
            fill = F,
            icon = icon("comments"))
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
       scale_fill_gradient(low = "white", high = "red")+
       labs(title = paste("Sebaran",input$var,st,sep = " "))+
       theme_minimal()+
         xlim(111, 115) + 
         ylim(9, 6.5) + 
       theme(plot.title = element_text(hjust = 0.5, size = 20))
     ggplotly(p2)
   })
   
   output$bar <- renderPlot({
     if(input$tahun == "All"){
       if(input$radbut=="Minimum"){
         dtot <- data %>% 
           group_by(KabupatenKota) %>% 
           summarize(total_pengaduan = sum(get(input$var), na.rm = TRUE)) %>% 
           arrange(desc(total_pengaduan))%>% 
           slice_min(order_by = total_pengaduan, n = 5)
         arr <- -dtot$total_pengaduan
       }else{
         dtot <- data %>% 
           group_by(KabupatenKota) %>% 
           summarize(total_pengaduan = sum(get(input$var), na.rm = TRUE)) %>% 
           arrange(desc(total_pengaduan))%>% 
           slice_max(order_by = total_pengaduan, n = 5)
         arr <- dtot$total_pengaduan
       }
     }else if(input$tahun==2023){
       if(input$radbut=="Minimum"){
         dtot <- df2023 %>% 
           group_by(KabupatenKota) %>% 
           summarize(total_pengaduan = sum(get(input$var), na.rm = TRUE)) %>% 
           arrange(desc(total_pengaduan))%>% 
           slice_min(order_by = total_pengaduan, n = 5)
         arr <- -dtot$total_pengaduan
       }else{
         dtot <- df2023 %>% 
           group_by(KabupatenKota) %>% 
           summarize(total_pengaduan = sum(get(input$var), na.rm = TRUE)) %>% 
           arrange(desc(total_pengaduan))%>% 
           slice_max(order_by = total_pengaduan, n = 5)
         arr <- dtot$total_pengaduan
       }
     }else{
       if(input$radbut=="Minimum"){
         dtot <- df2024 %>% 
           group_by(KabupatenKota) %>% 
           summarize(total_pengaduan = sum(get(input$var), na.rm = TRUE)) %>% 
           arrange(desc(total_pengaduan))%>% 
           slice_min(order_by = total_pengaduan, n = 5)
         arr <- -dtot$total_pengaduan
       }else{
         dtot <- df2024 %>% 
           group_by(KabupatenKota) %>% 
           summarize(total_pengaduan = sum(get(input$var), na.rm = TRUE)) %>% 
           arrange(desc(total_pengaduan))%>% 
           slice_max(order_by = total_pengaduan, n = 5)
         arr <- dtot$total_pengaduan
       }
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
}

shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
