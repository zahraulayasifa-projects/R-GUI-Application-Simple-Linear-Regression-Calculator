#Langkah 1: Panggil library "shiny" dan "shinythemes" 
library(shiny) 
library(shinythemes)
library(nortest)
library(lmtest)

#Langkah 2: Membuat tampilan ui 
ui <- fluidPage( 
  theme = shinytheme("flatly"), 
  navbarPage( 
    "--KALKULATOR REGRESI LINIER SEDERHANA--", 
    # Tab 1: Inventor 
    tabPanel( 
      "Inventor", 
      h4("GUI R -- KALKULATOR REGRESI LINIER SEDERHANA", style = "textalign:center"), 
      br(), 
      br(), 
      h5("Inventor:", style = "text-align:center"), 
      h5("Zahra Ulaya Sifa", style = "text-align:center"), 
      h5("Bachelor Degree of Statistics", style = "text-align:center"), 
      h5("Diponegoro University", style = "text-align:center"),
      br(),
      h5("Contact:", style = "text-align:center"),
      h5(
        tags$a("LinkedIn: Zahra Ulaya Sifa", 
               href = "https://www.linkedin.com/in/zahraulayasifa/", 
               target = "_blank", 
               style = "color: #66ccff; text-align:center; display:block;")
      ),
      h5(
        tags$a("Email: zahraulayasifa.work@gmail.com", 
               href = "mailto:zahraulayasifa.work@gmail.com", 
               target = "_blank", 
               style = "color: #66ccff; text-align:center; display:block;")
      ),
      h5(
        tags$a("Instagram: @zhrauysf", 
               href = "https://www.instagram.com/zhrauysf/", 
               target = "_blank", 
               style = "color: #66ccff; text-align:center; display:block;")
      )
    ), 
    
    # Tab 2: Tinjauan Pustaka 
    tabPanel( 
      "Tinjauan Pustaka", 
      fluidRow( 
        column(12, 
               h2("Regresi Linier Sederhana"), 
               p("Regresi linier sederhana adalah salah satu metode statistik yang paling umum digunakan dalam analisis data. Metode ini digunakan untuk memahami dan mengukur hubungan antara dua variabel, yaitu variabel independen (x) dan variabel dependen (y). Dalam konteks regresi linier sederhana, tujuan utama adalah untuk memodelkan hubungan sejauh mungkin antara variabel independen dan variabel dependen."), 
               p(HTML("Persamaan matematis dari regresi linier sederhana dapat dinyatakan sebagai berikut: y = &beta;<sub>0</sub> + &beta;<sub>1</sub>x + &epsilon;.")), 
               p("Dalam persamaan tersebut:"), 
               tags$ul( 
                 tags$li("y adalah variabel dependen, yang merupakan variabel yang ingin diprediksi atau dijelaskan."), 
                 tags$li("x adalah variabel independen, yang digunakan sebagai prediktor atau penyebab potensial dari variasi dalam y."), 
                 tags$li(HTML("&beta;<sub>0</sub> adalah intercept, yaitu nilai y ketika x=0.")),  # Gunakan HTML untuk simbol beta 
                 tags$li(HTML("&beta;<sub>1</sub> adalah slope, yang mengukur seberapa besar perubahan dalam y yang terkait dengan perubahan satu unit dalam x.")),  # Gunakan HTML untuk simbol beta 
                 tags$li(HTML("&epsilon; adalah kesalahan acak, yang merupakan faktor yang tidak dapat dijelaskan oleh model. Kesalahan ini menggambarkan variasi dalam y yang tidak dipengaruhi oleh x.")) # Gunakan HTML untuk simbol epsilon 
               ), 
               p(HTML("Adapun rumus untuk mengestimasi parameter &beta;<sub>0</sub> dan &beta;<sub>1</sub> adalah, sebagai berikut:")), 
               p(HTML("&beta;<sub>0</sub> = (n(&Sigma;xiyi)(&Sigma;xi)(&Sigma;yi))/(n&Sigma;(xi^2)-(&Sigma;xi)^2)")), # Gunakan HTML untuk simbol beta dan sigma 
               p(HTML("&beta;<sub>1</sub> = ((&Sigma;yi)(&Sigma;xi)^2(&Sigma;xi)(&Sigma;xiyi))/(n(&Sigma;(xi)^2)-(&Sigma;xi)^2)")),  
               p("Dalam persamaan tersebut:"), 
               tags$ul( 
                 tags$li("n = jumlah data"), 
                 tags$li("xi = Variabel independen/bebas"), 
                 tags$li("yi = Variabel dependen/terikat"), 
               ), 
               p(HTML("Tujuan analisis regresi linier sederhana adalah untuk memperkirakan parameter &beta;<sub>0</sub> dan &beta;<sub>1</sub> serta mengevaluasi sejauh mana model ini sesuai dengan data observasi. Proses ini melibatkan perhitungan nilai terbaik untuk &beta;<sub>0</sub> dan &beta;<sub>1</sub> sehingga model sesuai dengan data sebaik mungkin. Pengukuran sejauh mana model sesuai dengan data sering dilakukan dengan menggunakan statistik seperti R-squared (koefisien determinasi) yang mengukur seberapa besar variasi dalam y yang dapat dijelaskan oleh x.")), 
               p("Penggunaan regresi linier sederhana sangat luas dan ditemukan dalam berbagai disiplin ilmu. Beberapa aplikasi umum dari regresi linier sederhana meliputi:"), 
               tags$ul( 
                 tags$li("Prediksi: Regresi linier sederhana digunakan untuk memprediksi nilai variabel dependen berdasarkan nilai variabel independen. Misalnya, memprediksi penjualan berdasarkan biaya iklan."), 
                 tags$li("Identifikasi Tren: Regresi linier sederhana digunakan untuk mengidentifikasi tren atau pola dalam data, yang dapat membantu dalam pengambilan keputusan dan perencanaan."), 
                 tags$li("Pengendalian Proses: Di bidang industri, regresi linier digunakan untuk mengendalikan dan mengoptimalkan proses-proses produksi dengan mengidentifikasi faktor-faktor kunci yang memengaruhi hasil."), 
                 tags$li("Pengambilan Keputusan: Analisis regresi membantu dalam pengambilan keputusan berdasarkan bukti statistik tentang hubungan antara variabel-variabel yang relevan.") 
               ) 
        ) 
      ) 
    ), 
    
    # Tab 3: Input Data 
    tabPanel( 
      "Input Data",
      # Deskripsi di awal
      h4("Deskripsi Input Data", style = "text-align:center;"),
      p("- Silakan unggah file CSV atau TXT yang berisi dua variabel numerik untuk dianalisis menggunakan regresi linier sederhana."),
      p("- Format file yang didukung adalah dengan menggunakan pemisah koma (,), titik koma (;), atau tab. Silahkan pilih salah satu sesuai data Anda agar kalkulator dapat berjalan."),
      p("- Pastikan file memiliki header (nama kolom) dan hanya terdiri dari dua kolom data numerik."),
      p("- Pastikan Tanda desimal dalam file adalah menggunakan titik (.)"),
      p("Contoh data:", 
        tags$a("Contoh Data Regresi Linier Sederhana", 
               href = "https://drive.google.com/file/d/1E6QZgS0gyyQX5SJryUpecRW2GfXpmt2n/view?usp=sharing", 
               target = "_blank", 
               style = "color:#66ccff;")
      ),
      tags$hr(),
      sidebarLayout( 
        sidebarPanel( 
          fileInput("file", "Pilih File CSV atau TXT", multiple = FALSE, accept = c(".csv",".txt")), 
          tags$hr(), 
          checkboxInput("header", "Header", TRUE), 
          radioButtons("pemisah", "Separator", 
                       choices = c(Koma = ",", Titik_Koma = ";", Tab = "\t"), 
                       selected = ","), 
          tags$hr(), 
          radioButtons("disp", "Display Data", 
                       choices = c(Head = "head", All = "all"), 
                       selected = "head") 
        ), 
        mainPanel( 
          tableOutput("contents") 
        ) 
      ) 
    ), 
    
    # Tab 4: Model Awal Regresi Linier Sederhana 
    tabPanel( 
      "Model Awal Regresi Linier Sederhana", 
      selectInput("x", "Pilih Variabel X (Independen)", choices = NULL), 
      selectInput("y", "Pilih Variabel Y (Dependen)", choices = NULL), 
      verbatimTextOutput("regression_output"),
      h4("Interpretasi Hasil:", style = "margin-top:20px;"),
      verbatimTextOutput("interpretation")
    ),
    
    # Tab 5: Uji-Uji Asumsi
    tabPanel(
      "Uji-Uji Asumsi",
      h3("1. Uji Normalitas Residual", style = "text-align:center;"),
      radioButtons("uji_normal", "Pilih Jenis Uji Normalitas",
                   choices = c("Shapiro-Wilk" = "shapiro",
                               "Kolmogorov-Smirnov" = "ks",
                               "Anderson-Darling" = "ad"),
                   selected = "shapiro"),
      numericInput("alfa", "Masukkan Taraf Signifikansi (α):", value = 0.05, step = 0.01),
      helpText("Umumnya digunakan nilai 0.05 atau 0.01"),
      plotOutput("qqplot", width = "400px", height = "400px"),
      verbatimTextOutput("normalitas_result"),
      h4("Interpretasi:"),
      verbatimTextOutput("interpretation_normalitas"),
      h3("2. Uji Linearitas", style = "text-align:center;"),
      plotOutput("scatterplot", width = "400px", height = "400px"),
      h4("Interpretasi:"),
      verbatimTextOutput("interpretation_linearitas"),
      h3("3. Uji Homoskedastisitas (Uji Breusch-Pagan)", style = "text-align:center;"),
      verbatimTextOutput("bptest_result"),
      h4("Interpretasi:"),
      verbatimTextOutput("interpretation_homoskedastisitas"),
      h3("4.Uji Non-Autokorelasi (Durbin-Watson Test)", style = "text-align:center;"),
      verbatimTextOutput("autokorelasi_result"),
      h4("Interpretasi:"),
      verbatimTextOutput("interpretation_autokorelasi")
    ),
    
    # Tab 6: Uji Signifikansi
    tabPanel(
      "Uji Signifikansi",
      h3("1. Uji F (Uji Kecocokan Model)", style = "text-align:center;"),
      verbatimTextOutput("uji_F_result"),
      h4("Interpretasi:"),
      verbatimTextOutput("interpretation_uji_F"),
      h3("2. Uji t (Uji Signifikansi)", style = "text-align:center;"),
      verbatimTextOutput("uji_t_result"),
      h4("Interpretasi:"),
      verbatimTextOutput("interpretation_uji_t")
    ),
    
    #Tab 7: Model Akhir Regresi Linier Sederhana
    tabPanel(
      "Model Akhir Regresi Linier Sederhana",
      h4("Model Awal Regresi Linier Sederhana:"),
      verbatimTextOutput("model_awal"),
      h4("Model Akhir Regresi Linier Sederhana:"),
      verbatimTextOutput("model_akhir")
    )
  ) 
) 


#Langkah 3: Membuat syntax server 
server <- function(input, output, session) { 
  data <- reactive({ 
    req(input$file) 
    tryCatch({ 
      df <- read.csv(input$file$datapath, header = input$header, sep = 
                       input$pemisah) 
      df 
    }, error = function(e) { 
      stop(safeError(e)) 
      return(NULL) 
    }) 
  }) 
  
  output$contents <- renderTable({ 
    data_to_display <- data() 
    if (!is.null(data_to_display)) { 
      if (input$disp == "head") { 
        return(head(data_to_display)) 
      } else { 
        return(data_to_display) 
      } 
    } 
  }) 
  
  observe({ 
    data_to_select <- data() 
    if (!is.null(data_to_select)) { 
      updateSelectInput(session, "x", choices = names(data_to_select)) 
      updateSelectInput(session, "y", choices = names(data_to_select)) 
    } 
  }) 
  
  output$regression_output <- renderPrint({ 
    x <- input$x 
    y <- input$y 
    if (!is.null(x) && !is.null(y)) { 
      data_to_regress <- data() 
      if (!is.null(data_to_regress)) { 
        regression_model <- lm(data_to_regress[, y] ~ data_to_regress[, x]) 
        #fit <- lm(regresi[,input$vardep] ~ regresi[,input$varindep]) 
        summary(regression_model) 
      } 
    } 
  })

  output$interpretation <- renderPrint({
    x <- input$x
    y <- input$y
    if (!is.null(x) && !is.null(y)) {
      data_to_regress <- data()
      if (!is.null(data_to_regress)) {
        model <- lm(data_to_regress[, y] ~ data_to_regress[, x])
        summary_model <- summary(model)
        coef <- summary_model$coefficients
        
        intercept <- coef[1, 1]
        slope <- coef[2, 1]
        r_squared <- summary_model$r.squared
        residual_error <- summary_model$sigma
        
        cat("Model awal regresi linier sederhana:\n")
        cat("Y =", intercept, "+", slope, "* X + ε\n\n")
        
        cat("Interpretasi:\n")
        cat("→ β₀ (Intercept) =", intercept,
            "→ Saat", x, "= 0,", y, "diperkirakan =", intercept, "\n")
        cat("→ β₁ (Slope) =", slope,
            "→ Setiap kenaikan 1 unit pada", x, "diperkirakan menaikkan", y, "sebesar", slope, "\n")
        cat("→ ε (Residual Std. Error) =", residual_error,
            "→ Rata-rata galat prediksi model\n")
        cat("→ R-squared =", r_squared,
            "→", r_squared * 100, "% variasi pada", y, "dapat dijelaskan oleh", x)
      }
    }
  })
  
  output$qqplot <- renderPlot({
    x <- input$x
    y <- input$y
    if (!is.null(x) && !is.null(y)) {
      data_to_regress <- data()
      if (!is.null(data_to_regress)) {
        model <- lm(data_to_regress[, y] ~ data_to_regress[, x])
        residuals <- residuals(model)
        qqnorm(residuals, main = "QQ Plot of Residuals")
        qqline(residuals, col = "red")
      }
    }
  })
  
  output$normalitas_result<-renderPrint({
    x <- input$x
    y <- input$y
    
    if (!is.null(x) && !is.null(y)) {
      data_to_regress <- data()
      if (!is.null(data_to_regress)) {
        model <- lm(data_to_regress[, y] ~ data_to_regress[, x])
        residuals <- residuals(model)
        
        if (input$uji_normal == "shapiro") {
          shapiro.test(residuals)
        } else if (input$uji_normal == "ks") {
          ks.test(residuals, "pnorm", mean(residuals), sd(residuals))
        } else if (input$uji_normal == "ad") {
          ad.test(residuals)
        }
      }
    }
    
  })
    
  output$interpretation_normalitas <- renderPrint({
    x <- input$x
    y <- input$y
    alfa <- input$alfa
    
    if (!is.null(x) && !is.null(y)) {
      data_to_regress <- data()
      if (!is.null(data_to_regress)) {
        model <- lm(data_to_regress[, y] ~ data_to_regress[, x])
        residuals <- residuals(model)
        
        if (input$uji_normal == "shapiro") {
          pval <- shapiro.test(residuals)$p.value
          uji <- "Shapiro-Wilk"
        } else if (input$uji_normal == "ks") {
          pval <- ks.test(residuals, "pnorm", mean(residuals), sd(residuals))$p.value
          uji <- "Kolmogorov-Smirnov"
        } else if (input$uji_normal == "ad") {
          pval <- ad.test(residuals)$p.value
          uji <- "Anderson-Darling"
        }
        
        cat("Uji", uji, "\n\n")
        cat("H0: Residual berdistribusi normal","\n")
        cat("H1: Residual tidak berdistribusi normal","\n\n")
        cat("Daerah Kritis:","\n")
        cat("Tolak H0 jika nilai signifikansi < α","\n\n")
        
        cat("p-value =", round(pval, 4), "\n")
        cat("Taraf signifikansi (α) =", alfa, "\n\n")
        
        
        if (pval < alfa) {
          cat("→ Karena p-value <", alfa, ", maka tolak H₀\n")
          cat("→ Kesimpulan: Residual tidak berdistribusi normal\n")
        } else {
          cat("→ Karena p-value ≥", alfa, ", maka gagal tolak H₀\n")
          cat("→ Kesimpulan: Residual berdistribusi normal\n")
        }
      }
    }
  })
  
  output$scatterplot <- renderPlot({
    x <- input$x
    y <- input$y
    if (!is.null(x) && !is.null(y)) {
      data_to_regress <- data()
      if (!is.null(data_to_regress)) {
        model <- lm(data_to_regress[[y]] ~ data_to_regress[[x]])
        
        residuals <- residuals(model)
        fitted_vals <- fitted(model)
        
        # Standarisasi residual dan prediksi
        zres <- scale(residuals)
        zpred <- scale(fitted_vals)
        
        plot(zpred, zres,
             xlab = "ZPRED (Standardized Predicted Value)",
             ylab = "ZRES (Standardized Residual)",
             main = "Scatterplot ZRES vs ZPRED",
             pch = 19, col = "#66ccff")
      }
    }
  })
  
  output$interpretation_linearitas <- renderPrint({
    x <- input$x
    y <- input$y
    if (!is.null(x) && !is.null(y)) {
      data_to_regress <- data()
      if (!is.null(data_to_regress)) {
        cat("Jika sebaran data acak atau tidak membentuk pola, maka asumsi linearitas terpenuhi\n")
        cat("Namun, jika pola titik membentuk pola, maka asumsi linearitas tidak terpenuhi\n")
      }
    }
  })

  
  output$bptest_result <- renderPrint({
    x <- input$x
    y <- input$y
    if (!is.null(x) && !is.null(y)) {
      data_bp <- data()
      if (!is.null(data_bp)) {
        model <- lm(data_bp[[y]] ~ data_bp[[x]])
        bptest(model)
      }
    }
  })
  
  output$interpretation_homoskedastisitas <- renderPrint({
    x <- input$x
    y <- input$y
    alfa <- input$alfa
    if (!is.null(x) && !is.null(y)) {
      data_bp <- data()
      if (!is.null(data_bp)) {
        model <- lm(data_bp[[y]] ~ data_bp[[x]])
        bp_result <- bptest(model)
        pval <- bp_result$p.value
        
        cat("p-value =", round(pval, 4), "\n")
        cat("Taraf signifikansi (α) =", alfa, "\n\n")
        
        if (pval > alfa) {
          cat("→ Karena p-value >", alfa, ", maka residual memiliki varians yang konstan\n")
          cat("→ Kesimpulan: Asumsi homoskedastisitas terpenuhi\n")
        } else {
          cat("→ Karena p-value <", alfa, ", maka residual memiliki varians yang tidak konstan (terdapat heteroskedastisitas)\n")
          cat("→ Kesimpulan: Asumsi homoskedastisitas TIDAK terpenuhi\n")
        }
      }
    }
  })
  
  output$autokorelasi_result<-renderPrint({
    x <- input$x
    y <- input$y
    if (!is.null(x) && !is.null(y)) {
      data_autokor <- data()
      if (!is.null(data_autokor)) {
        model <- lm(data_autokor[[y]] ~ data_autokor[[x]])
        dwtest(model)
      }
    }
  })
  
  output$interpretation_autokorelasi <- renderPrint({
    x <- input$x
    y <- input$y
    alfa <- input$alfa
    if (!is.null(x) && !is.null(y)) {
      data_autokor <- data()
      if (!is.null(data_autokor)) {
        model <- lm(data_autokor[[y]] ~ data_autokor[[x]])
        dw_result <- dwtest(model)
        pval <- dw_result$p.value
        
        cat("Durbin-Watson Test\n")
        cat("p-value =", round(pval, 4), "\n")
        cat("Taraf signifikansi (α) =", alfa, "\n\n")
        
        if (pval > alfa) {
          cat("→ Karena p-value >", alfa, ", maka tidak ada autokorelasi yang signifikan\n")
          cat("→ Kesimpulan: Asumsi non-autokorelasi terpenuhi\n")
        } else {
          cat("→ Karena p-value <", alfa, ", maka terdapat autokorelasi residual\n")
          cat("→ Kesimpulan: Asumsi non-autokorelasi TIDAK terpenuhi\n")
        }
      }
    }
  })
  
  output$uji_F_result <- renderPrint({
    x <- input$x
    y <- input$y
    
    if (!is.null(x) && !is.null(y)) {
      data_to_regress <- data()
      if (!is.null(data_to_regress)) {
        model <- lm(data_to_regress[, y] ~ data_to_regress[, x])
        summary(model)
      }
    }
  })
  
  output$interpretation_uji_F <- renderPrint({
    x <- input$x
    y <- input$y
    alfa <- input$alfa
    
    if (!is.null(x) && !is.null(y)) {
      data_to_regress <- data()
      if (!is.null(data_to_regress)) {
        model <- lm(data_to_regress[, y] ~ data_to_regress[, x])
        fstat <- summary(model)$fstatistic
        fval <- fstat["value"]
        df1 <- fstat["numdf"]
        df2 <- fstat["dendf"]
        pval <- pf(fval, df1, df2, lower.tail = FALSE)
        
        cat("Uji F (Uji Kecocokan Model\n\n")
        cat("H0: β0 = β1 = 0 atau Model tidak sesuai\n")
        cat("H1: βi ≠ 0 atau paling sedikit satu i model regresi sesuai \n\n")
        cat("Taraf signifikansi (α) =", alfa, "\n")
        cat("Statistik F =", round(fval, 4), "\n")
        cat("p-value =", round(pval, 4), "\n")
        cat("Derajat bebas (df1) =", df1, "dan (df2) =", df2, "\n\n")
        
        if (pval < alfa) {
          cat("Karena p-value <", alfa, ", maka H0 ditolak.\n")
          cat("→ Kesimpulan: Model regresi linier sederhana cocok.\n")
          cat("→ Artinya, model regresi dapat digunakan untuk memprediksi", y)
        } else {
          cat("Karena p-value >=", alfa, ", maka H0 gagal ditolak.\n")
          cat("→ Kesimpulan: Model regresi linier sederhana tidak cocok.\n")
          cat("→ Artinya, model regresi tidak dapat digunakan untuk memprediksi", y)
        }
      }
    }
  })
  
  output$uji_t_result<-renderPrint({
    x <- input$x
    y <- input$y
    alfa <- input$alfa
    model <- lm(data()[, input$y] ~ data()[, input$x])
    summary(model)$coefficients
  })
  
  output$interpretation_uji_t <- renderPrint({
    x <- input$x
    y <- input$y
    alfa <- input$alfa
    model <- lm(data()[, input$y] ~ data()[, input$x])
    coef <- summary(model)$coefficients
    
    pval <- coef[2, 4] # p-value untuk slope
    
    cat("Uji t (Uji Signifikansi)\n\n")
    cat("H0: βj = 0 atau koefisien parameter tidak sesuai \n")
    cat("H1: βj ≠ 0 atau koefisien parameter sesuai \n\n")
    cat("p-value =", round(pval, 4), "\n")
    cat("Taraf signifikansi (α) =", alfa, "\n\n")
    
    if (pval < alfa) {
      cat("Karena p-value <", alfa, ", maka tolak H₀\n")
      cat("→ Kesimpulan: Koefisien parameter regresi sesuai dengan kata lain variabel", x, "berpengaruh terhadap variabel", y ,"\n")
    } else {
      cat("Karena p-value ≥", alfa, ", maka gagal tolak H₀\n")
      cat("→ Kesimpulan: Koefisien regresi tidak sesuai dengan kata lain variabel", x, "berpengaruh terhadap variabel", y, "\n")
    }
  })
  
  output$model_awal<-renderPrint({
    x <- input$x
    y <- input$y
    if (!is.null(x) && !is.null(y)) {
      data_to_regress <- data()
      if (!is.null(data_to_regress)) {
        model <- lm(data_to_regress[, y] ~ data_to_regress[, x])
        summary_model <- summary(model)
        coef <- summary_model$coefficients
        
        intercept <- coef[1, 1]
        slope <- coef[2, 1]
        
        cat("Model awal regresi linier sederhana:\n")
        cat("Y =", intercept, "+", slope, "* X + ε\n\n")
      }
    }
      })
  
  output$model_akhir <- renderPrint({
    x <- input$x
    y <- input$y
    alfa <- input$alfa
    if (!is.null(x) && !is.null(y)) {
      data_to_regress <- data()
      if (!is.null(data_to_regress)) {
        model <- lm(data_to_regress[, y] ~ data_to_regress[, x])
        summary_model <- summary(model)
        coef <- summary_model$coefficients
        
        intercept <- coef[1, 1]
        slope <- coef[2, 1]}}
    
    model <- lm(data()[, input$y] ~ data()[, input$x])
    summary_model <- summary(model)
    anova_model <- anova(model)
    
    # Uji F
    pval_f <- anova_model$`Pr(>F)`[1]
    cat("Uji F: Apakah model regresi linier sederhana cocok?\n")
    
    if (pval_f < alfa) {
      cat("Jawaban: IYA, model cocok (karena p-value <", alfa, ")\n\n")
      
      # Lanjut Uji t
      cat("Uji t: Apakah koefisien regresi sesuai?\n")
      pval_t <- coef(summary_model)[2, 4]  # koefisien x
      
      if (pval_t < alfa) {
        cat("Jawaban: IYA, koefisien signifikan (karena p-value <", alfa, ")\n")
        cat("KESIMPULAN: MODEL AKHIR SAMA DENGAN MODEL AWAL, YAITU \n")
        cat("Y =", intercept, "+", slope, "* X + ε\n\n")
      } else {
        cat("Jawaban: TIDAK, koefisien tidak signifikan (karena p-value >=", alfa, ")\n")
        cat("KESIMPULAN: MODEL AKHIR MENJADI, YAITU \n")
        cat("Y =", intercept, "+", "+ ε\n\n")
      }
      
    } else {
      cat("Jawaban: TIDAK, model tidak cocok (karena p-value >=", alfa, ")\n")
      cat("KESIMPULAN ANALISIS: TIDAK BISA MEMPEROLEH MODEL REGRESI")
    }
  })
  
} 

#Langkah 4: Panggil program R shiny 
shinyApp(ui = ui, server = server)