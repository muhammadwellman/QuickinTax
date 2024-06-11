library(shiny)
library(shinyWidgets)
library(bslib)
library(shinydashboard)

# Bagian UI (User Interface)
ui <- navbarPage(
  title = "QuickinTax",
  id = "main_navbar",
  theme = bslib::bs_theme(
    bootswatch = "flatly",
    primary = "#002147",
    secondary = "#FFC107"
  ),
  
  tags$head(
    tags$style(HTML("
      .nav > li > a {
        color: #FFC107 !important;
        background-color: #002147 !important;
      }
      .nav > li > a:hover {
        background-color: #001533 !important;
      }
      .nav > li.active > a {
        background-color: #FFC107 !important;
        color: #002147 !important;
      }
      .panel-custom {
        border-color: #002147;
      }
      .panel-custom > .panel-heading {
        background-color: #002147;
        color: white;
        text-align: center; /* Center text in panel heading */
      }
      .panel-custom > .panel-body {
        background-color: #f5f5f5;
      }
      .panel-custom .panel-title {
        font-size: 16px;
        text-align: center; /* Center text in panel title */
      }
      .panel-custom .panel-body h3 {
        font-size: 14px;
        text-align: center; /* Center text in panel body */
      }
    "))
  ),
  
  tabPanel(
    title = "Home",
    icon = icon("home"),
    fluidPage(
      fluidRow(
        column(
          width = 12,
          tags$h3("Selamat Datang di QuickinTax"),
          tags$p("QuickinTax adalah solusi cepat dan tepat untuk menghitung pajak Anda. Dengan antarmuka yang mudah digunakan, Anda dapat memasukkan data keuangan Anda dan mendapatkan hasil perhitungan pajak dengan akurat."),
          tags$br(),
          "Mulai sekarang dan rasakan kemudahan dalam menghitung pajak dengan QuickinTax!",
          tags$br(),
          tags$br(),
          actionButton("mulai_hitung", label = "Mulai Hitung", icon = icon("calculator"), onclick = "Shiny.onInputChange('navbarSelected', 'Input');")
        )
      )
    )
  ),
  
  tabPanel(
    title = "Input",
    icon = icon("edit"),
    fluidPage(
      fluidRow(
        column(
          width = 12,
          tags$h3("Masukkan Data Keuangan Anda"),
          tags$p("Silakan masukkan data keuangan Anda pada kolom-kolom di bawah ini untuk menghitung pajak Anda."),
          tags$br(),
          numericInput("gaji", "Gaji Bulanan (Rp)", value = 10000000, width = "100%"),
          pickerInput("status", "Status Pernikahan", choices = c("Tidak Menikah", "Menikah"), options = "list(live-search = TRUE)", width = "100%"),
          numericInput("tanggungan", "Jumlah Tanggungan", value = 0, min = 0, width = "100%"),
          numericInput("bonus", "Bonus Tahunan (Rp)", value = 0, width = "100%"),
          numericInput("lainnya", "Penghasilan Lainnya (Rp)", value = 0, width = "100%")
        )
      )
    )
  ),
  
  tabPanel(
    title = "Hasil",
    icon = icon("chart-bar"),
    fluidPage(
      fluidRow(
        column(
          width = 12,
          tags$h3("Hasil Perhitungan Pajak Anda"),
          tags$p("Berikut adalah hasil perhitungan pajak berdasarkan data yang Anda masukkan."),
          tags$br(),
          uiOutput("hasil")
        )
      )
    )
  ),
  
  tabPanel(
    title = "Kritik dan Saran",
    icon = icon("envelope"),
    fluidPage(
      fluidRow(
        column(
          width = 12,
          tags$h3("Kritik dan Saran"),
          tags$p("Kami sangat menghargai kritik dan saran dari Anda untuk pengembangan aplikasi ini lebih lanjut."),
          tags$br(),
          textAreaInput("kritik_saran", "Masukkan Kritik dan Saran Anda", "", width = "100%", height = "200px"),
          actionButton("submit_kritik_saran", "Kirim", class = "btn-primary")
        )
      )
    )
  ),
  
  tabPanel(
    title = "Profil",
    icon = icon("user"),
    fluidPage(
      fluidRow(
        column(
          width = 12,
          tags$h3("Tim RProg Berkah FEB UGM 2024"),
          tags$p("Aplikasi QuickinTax dikembangkan oleh tim dari FEB UGM. Kami berkomitmen untuk memberikan solusi terbaik dalam menghitung pajak Anda."),
          tags$br(),
          tags$ul(
            tags$li("Muhammad Wellman (23/511905/EK/24282)"),
            tags$li("Afifah Ainun (23/512034/EK/24298)"),
            tags$li("Christy Karen (23/512224/EK/24310)"),
            tags$li("Fahmadia Nuuro (23/514401/EK/24367)"),
            tags$li("Susana Aseta (23/512838/EK/24334)"),
            tags$li("Luthfida Aufa (23/516653/EK/24487)"),
            tags$li("M. Alif Dzaki (23/522831/EK/24743)")
          )
        )
      )
    )
  )
)

# Bagian Server
server <- function(input, output, session) {
  observeEvent(input$mulai_hitung, {
    updateNavbarPage(session, "main_navbar", selected = "Input")
  })
  
  hasil <- reactive({
    gaji <- input$gaji
    status <- input$status
    tanggungan <- input$tanggungan
    bonus <- input$bonus
    lainnya <- input$lainnya
    
    total_gaji <- gaji * 12 + bonus + lainnya * 12
    
    # Menghitung PTKP
    if (status == "Tidak Menikah" && tanggungan == 0) {
      ptkp <- 54000000
    } else if (status == "Tidak Menikah" && tanggungan == 1) {
      ptkp <- 58500000
    } else if (status == "Tidak Menikah" && tanggungan == 2) {
      ptkp <- 63000000
    } else if (status == "Tidak Menikah" && tanggungan == 3) {
      ptkp <- 67500000
    } else if (status == "Menikah" && tanggungan == 0) {
      ptkp <- 58500000
    } else if (status == "Menikah" && tanggungan == 1) {
      ptkp <- 63000000
    } else if (status == "Menikah" && tanggungan == 2) {
      ptkp <- 67500000
    } else {
      ptkp <- 67500000 + (tanggungan - 2) * 4500000
    }
    
    # Menghitung PKP
    pkp <- total_gaji - ptkp
    
    # Menghitung Pajak Penghasilan
    if (pkp <= 50000000) {
      pajak <- 0.05 * pkp
    } else if (pkp <= 250000000) {
      pajak <- 2500000 + 0.15 * (pkp - 50000000)
    } else if (pkp <= 500000000) {
      pajak <- 32500000 + 0.25 * (pkp - 250000000)
    } else {
      pajak <- 95000000 + 0.3 * (pkp - 500000000)
    }
    
    list(
      total_gaji = total_gaji,
      ptkp = ptkp,
      pkp = pkp,
      pajak = pajak
    )
  })
  
  output$hasil <- renderUI({
    hasil_perhitungan <- hasil()
    
    tags$div(
      tags$h4("Ringkasan Perhitungan"),
      tags$hr(),
      fluidRow(
        column(
          width = 4,
          tags$div(
            class = "panel panel-custom",
            tags$div(class = "panel-heading", tags$h3(class = "panel-title", "Total Gaji Setahun")),
            tags$div(class = "panel-body", tags$h3(style = "text-align: center;", paste("Rp", format(hasil_perhitungan$total_gaji, big.mark = ".", decimal.mark = ",", scientific = FALSE))))
          )
        ),
        column(
          width = 4,
          tags$div(
            class = "panel panel-custom",
            tags$div(class = "panel-heading", tags$h3(class = "panel-title", "Penghasilan Tidak Kena Pajak (PTKP)")),
            tags$div(class = "panel-body", tags$h3(style = "text-align: center;", paste("Rp", format(hasil_perhitungan$ptkp, big.mark = ".", decimal.mark = ",", scientific = FALSE))))
          )
        ),
        column(
          width = 4,
          tags$div(
            class = "panel panel-custom",
            tags$div(class = "panel-heading", tags$h3(class = "panel-title", "Penghasilan Kena Pajak (PKP)")),
            tags$div(class = "panel-body", tags$h3(style = "text-align: center;", paste("Rp", format(hasil_perhitungan$pkp, big.mark = ".", decimal.mark = ",", scientific = FALSE))))
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          tags$div(
            class = "panel panel-custom",
            tags$div(class = "panel-heading", tags$h3(class = "panel-title", "Pajak Penghasilan")),
            tags$div(class = "panel-body", tags$h3(style = "text-align: center;", paste("Rp", format(hasil_perhitungan$pajak, big.mark = ".", decimal.mark = ",", scientific = FALSE))))
          )
        )
      )
    )
  })
}

# Jalankan aplikasi
shinyApp(ui = ui, server = server)