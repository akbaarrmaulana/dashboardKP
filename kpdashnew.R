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

source("kpsourcenew.R")

############
#header
dropdownMenuCustom <- function() {
  tags$li(class = "dropdown",
          tags$a(href = "https://opendata.jatimprov.go.id", 
                 target = "_blank", 
                 icon("right-to-bracket"), 
                 "Open Data Jatim")
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
                              menuItem("Statistikm Penilaian Pengunjung",
                                       tabName = "nilai",
                                       icon = icon("mobile-screen-button")),
                              menuItem("Update Data",
                                       tabName = "dataup",
                                       icon = icon("hotel"))
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
    logoBackColor = "rgb(0,0,128)",  # Warna navy untuk logo
    headerButtonBackColor = "rgb(238,238,238)",
    headerButtonIconColor = "rgb(75,75,75)",
    headerButtonBackColorHover = "rgb(210,210,210)",
    headerButtonIconColorHover = "rgb(0,0,0)",
    
    headerBackColor = "rgb(0,0,128)",  # Warna navy untuk header
    headerBoxShadowColor = "#aaaaaa",
    headerBoxShadowSize = "2px 2px 2px",
    
    ### Sidebar
    sidebarBackColor = cssGradientThreeColors(
      direction = "down",
      colorStart = "rgb(0,0,128)",  # Navy atas
      colorMiddle = "rgb(0,0,192)",  # Gradasi biru di tengah
      colorEnd = "rgb(0,0,255)",  # Biru terang di bawah
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
    
    sidebarSearchBackColor = "rgb(0,0,192)",  # Warna biru terang
    sidebarSearchIconColor = "rgb(255,255,255)",
    sidebarSearchBorderColor = "rgb(0,0,192)",
    
    sidebarTabTextColor = "rgb(255,255,255)",
    sidebarTabTextSize = 13,
    sidebarTabBorderStyle = "none none solid none",
    sidebarTabBorderColor = "rgb(0,0,128)",  # Navy untuk border tab
    sidebarTabBorderWidth = 1,
    
    sidebarTabBackColorSelected = cssGradientThreeColors(
      direction = "right",
      colorStart = "rgb(0,0,255)",  # Gradasi biru terang saat dipilih
      colorMiddle = "rgb(0,0,192)",
      colorEnd = "rgb(0,0,128)",
      colorStartPos = 0,
      colorMiddlePos = 30,
      colorEndPos = 100
    ),
    sidebarTabTextColorSelected = "rgb(255,255,255)",
    sidebarTabRadiusSelected = "0px 20px 20px 0px",
    
    sidebarTabBackColorHover = cssGradientThreeColors(
      direction = "right",
      colorStart = "rgb(0,0,192)",  # Gradasi biru lebih terang saat hover
      colorMiddle = "rgb(0,0,128)",
      colorEnd = "rgb(0,0,255)",
      colorStartPos = 0,
      colorMiddlePos = 30,
      colorEndPos = 100
    ),
    sidebarTabTextColorHover = "rgb(255,255,255)",
    sidebarTabBorderStyleHover = "none none solid none",
    sidebarTabBorderColorHover = "rgb(0,0,128)",
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
        background:#000080
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
          background-color: #000080; /* Maroon color */
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
  tags$head(
    tags$style(HTML("
        .bg-blue {
          background-color: #000080 !important; /* Warna hijau kustom */
          color: #FFFFFF !important;          /* Warna teks */
          border-color: #FFFFFF !important;   /* Warna border */
        }
      "))
  ),
  div(
    id="main_content",
    tabItems(
      tabItem(
        tabName = "nilai",
        titlePanel(
          h2(strong("Statistik Penilaian Pengunjung Portal Open Data Jatim"),
             style="text-align:center;",style = "margin-bottom:-20px;",style = "margin-top:-20px;")),
        br(),
        fluidRow(
          width = 12,
          div(box(selectInput("tahun","Tahun",
                              choices = c("All",unique(dfn$Tahun)), selected = "2023"),
                  width = 3, 
                  status = "primary",height = 70),
              box(selectInput("bulan","Bulan",
                              choices = c("All",unique(dfn$Bulan))),
                  width = 3, 
                  status = "primary",height = 90),
              infoBoxOutput(outputId = "jml", width = 6),
              style = "margin-top:20px")),
        fluidRow(
          box(
            title = tags$a(
              href = "https://opendata.jatimprov.go.id",  
              target = "_blank",                         
              strong("Tentang Portal Open Data Jatim"),      
              style = "color: white; text-decoration: none;" 
            ),
            collapsible = TRUE, 
            collapsed = T,
            width = 12,         
            solidHeader = TRUE, 
            status = "success", 
            "Portal Open Data Jatim adalah sebuah platform digital yang dikelola 
            oleh Pemerintah Provinsi Jawa Timur untuk menyediakan akses terbuka 
            terhadap data-data publik dari berbagai sektor. Portal ini dirancang 
            untuk meningkatkan transparansi dan akuntabilitas pemerintahan dengan 
            mempermudah masyarakat, peneliti, maupun pelaku usaha dalam mengakses 
            informasi yang relevan dan terpercaya. 
            Melalui portal ini, data seperti kesehatan, pendidikan, ekonomi, hingga 
            infrastruktur disajikan dalam format yang mudah diakses dan diunduh, 
            sehingga dapat dimanfaatkan untuk analisis, penelitian, maupun pengembangan 
            inovasi. Dengan pendekatan berbasis data, portal ini juga mendukung pengambilan 
            keputusan yang lebih efektif baik oleh pemerintah maupun pemangku kepentingan 
            lainnya. Kehadiran Open Data Jatim mencerminkan komitmen pemerintah daerah untuk 
            mendorong keterbukaan informasi sekaligus memberdayakan masyarakat dalam membangun 
            solusi yang berkelanjutan."
          )
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
    # Menentukan judul InfoBox
    tit <- tags$h4("Total Pengunjung Yang Memberi Penilaian", 
                   style = "font-weight:bold; white-space: normal; word-wrap: break-word;")
    
    # Menentukan periode berdasarkan input bulan tahun
    if (input$bulan == "All" & input$tahun == "All") {
      st <- "Semua Periode"
      a1 <- dfn %>% 
        summarise(total = sum(sangatpuas + puas + cukup+ tidakpuas + sangattidakpuas, na.rm = T))
      a2 <- a1$total
    } else if (input$bulan == "All") {
      st <- input$tahun
      a1 <- dfn %>%
        filter(Tahun == input$tahun) %>%
        summarise(total = sum(sangatpuas + puas + cukup+ tidakpuas + sangattidakpuas, na.rm = T))
      a2 <- a1$total
    } else {
      st <- paste(input$bulan, input$tahun, sep = " ")
      a1 <- dfn %>%
        filter(Bulan == input$bulan & Tahun == input$tahun) %>%
        summarise(total = sum(sangatpuas + puas + cukup+ tidakpuas + sangattidakpuas, na.rm = T))
      a2 <- a1$total
    }
    
    # Membuat InfoBox
    infoBox(tit,
            value = tags$h6(paste("Periode:", st)),
            subtitle = tags$h4(paste(a2, "Respon")),
            color = "blue",
            fill = F,
            icon = icon("chart-bar"))
  })
  
}

shinyApp(ui = ui, server = server, options = list(launch.browser = T))