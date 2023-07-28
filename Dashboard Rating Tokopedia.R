# import shiny package
library(shiny)
library(shinydashboard)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(rsconnect)

setwd("C:/Users/Kridha/Downloads/Tokopedia Rating")
df = read.csv("Data.csv",header=TRUE,sep=';')
head(df)
df2 = read.csv("data_numerik.csv",header=TRUE,sep=';')
head(df2)

df$lama_balas_chat <- as.integer(df$lama_balas_chat)
df = na.omit(df)
df

#creating model for prediction
model= lm(rating~ ulasan+produk_terjual+produk_dijual+lama_bergabung+lama_balas_chat+voucher,data=df)
summary(model)


nama_toko = df$nama_toko
kota = df$kota
rating = df$rating
ulasan = df$ulasan
dijual = df$produk_dijual
terjual = df$produk_terjual
bergabung = df$lama_bergabung
balas_chat = df$lama_balas_chat
voucher = df$voucher

pierating <- data.frame(
  Rating=c("Rating 4,7","Rating 4,8","Rating 4,9","Rating 5"),
  value=c(9,18,55,11),
  percent=c("10%", "19%", "59%", "11%")
)
pievoucher <- data.frame(
  Voucher = c("Toko dengan Voucher", "Toko Tanpa Voucher"),
  value_vc = c(36,57),
  percent_vc = c("39%", "61%")
)

barrating <- data.frame(
  Rating=c("4.7","4.8","4.9","5"),
  value_rt=c(9,18,55,11)
)

barterjual <- data.frame(
  terjual=c("0-1000","1001-5000","5001-10000",">10000"),
  value=c(45,26,9,13)
)

data = df[3:8]
data
by_kota <- group_by(data, kota)
by_kota
# summarise on groupped data.
by_kota1 <- by_kota %>% group_by(kota) %>%
  summarise(ulasan=sum(ulasan),
            terjual = sum(produk_terjual),
            dijual = min(produk_dijual),
            lama_bergabung = max(lama_bergabung),
            balas_chat = max(balas_chat),
            .groups = 'drop'
  )
by_kota1

kota1 = by_kota1$kota
ulasan1 = by_kota1$ulasan
dijual1 = by_kota1$dijual
terjual1 = by_kota1$terjual
bergabung1 = by_kota1$lama_bergabung
balas_chat1 = by_kota1$balas_chat

# define dashboard page layout
ui <- dashboardPage(
  dashboardHeader(title="Dashboard Tokopedia Rating",titleWidth = 300),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Pendahuluan",tabName = "pendahuluan",icon=icon("database")),
      menuItem("Data",tabName = "data",icon=icon("folder")),
      menuItem("Visualisasi",tabName = "visual",icon=icon("chart-simple")),
      menuItem("Regresi Linear",tabName="regresi",icon=icon("magnifying-glass-chart"))
    )
  ),
  dashboardBody(
    tabItems(tabItem(tabName = "visual",
                     column(12,
                            column(12,
                                   fluidRow(
                                     h1(strong("Visualisasi dan Analisis")),
                                     tabBox(id="visualisasi", width=NULL, height = "360px", selected = "Summary",
                                            tabPanel("Summary",
                                                     verbatimTextOutput(outputId="summary")),
                                            tabPanel("Barplot", 
                                                     box(selectInput(inputId="bar",
                                                                 label="Choose Variable for Bar Chart",
                                                                 choices=list("Rating"=1,"Produk Terjual"=2),selected = 1)),
                                                     box(plotOutput(outputId="barchart")),
                                                     h4(strong("Interpretasi: Mayoritas toko memiliki rating 4.9 dan mayoritas toko telah menjual 0-1000 produk"))),
                                                     
                                            tabPanel("Histogram",
                                                     selectInput(inputId = "varhist",
                                                                 label = "Choose Variable for Histogram",
                                                                 choices = list("rating" = 1, "ulasan" = 2, "produk_dijual" = 3, "lama_bergabung"=4, "lama_chat"=5),
                                                                 selected = 1),
                                                     plotOutput(outputId="hist")),
                                            tabPanel("Cleveland Plot",
                                                     selectInput(inputId="clev",
                                                                     label="Choose Variable for Cleveland Plot",
                                                                     choices=list("Ulasan"=1,"Produk Terjual"=2,
                                                                                  "Produk Dijual"=3,"Lama Bergabung"=4,"Lama Balas Chat"=5),
                                                                     selected = 1),
                                                     plotOutput(outputId="cleveland")),
                                            tabPanel("Boxplot",
                                                     selectInput(inputId="boxp",
                                                                 label="Choose Variable for Box Plot",
                                                                 choices=list("Ulasan"=1,"Produk Terjual"=2,
                                                                              "Produk Dijual"=3,"Lama Bergabung"=4,"Lama Balas Chat"=5),
                                                                 selected = 1),
                                                     plotOutput(outputId="plotbox")),
                                            tabPanel("Scatterplot",
                                                     selectInput(inputId = "x",
                                                                 label = "Pilih variabel x:",
                                                                 choices = names(df2)),
                                                     selectInput(inputId = "y",
                                                                 label = "Pilih variabel y:",
                                                                 choices = names(df2)),
                                                     plotOutput("scatterplot")),
                                            tabPanel("Pie Chart",
                                                     selectInput(inputId = "pie",
                                                                 label = "Choose Variable for Pie Chart",
                                                                 choices = list("Rating"=1, "Voucher"=2),
                                                                 selected = 1),
                                                     plotOutput(outputId = "piechart"))
                                            )
                                   )))
                     ),tabItem(tabName = "data",
                               dataTableOutput(outputId = "table")),
             tabItem(tabName = "regresi",
                     headerPanel("Prediksi Rating Toko"),
                     sidebarPanel(
                       numericInput('ulasan', 'Ulasan', "0,1,2"),
                       numericInput('dijual', 'Produk Dijual', "0,1,2"),
                       numericInput('terjual', 'Produk Terjual', "0,1,2"),
                       numericInput('lama_bergabung', 'Lama Bergabung Tokopedia', "0,1,2"),
                       numericInput('lama_dibalas', 'Lama Chat Dibalas', "0,1,2"),
                       numericInput('voucher', 'Voucher (1= ada & 0 = tidak ada)', "0,1,2"),
                     ),
                     
                     mainPanel(
                       h4('You entered'),
                       verbatimTextOutput("prediksi"),
                     )
             ),
             
             tabItem(tabName = "pendahuluan",
                     verbatimTextOutput(outputId="anggota"),
                     column(12,
                            column(12,
                                   fluidRow(
                                     h1(strong("Pendahuluan")),
                                     tabBox(id = "pendahuluan",width = NULL,height = "360px",selected = "Pendahuluan",
                                            tabPanel("Pendahuluan",
                                                     selectInput(inputId = "sel_Var",
                                                                 label = "Choose Variables",
                                                                 list("Marketplace","Rating")),
                                                     htmlOutput("pendahuluan1")),
                                            tabPanel("Landasan Teori",
                                                     selectInput(inputId = "sel_Year5",
                                                                 label = "Choose Year",
                                                                 list("Consumer Rating"," Exploratory Data Analysis")),
                                                     htmlOutput("pendahuluan2")),
                                            tabPanel("Sumber Data dan Metodologi",
                                                     selectInput(inputId = "sel_Region2",
                                                                 label = "Choose Region",
                                                                 list("Sumber Data","Metodologi")),
                                                     htmlOutput("pendahuluan3")),
                                     )
                                   ),
                                   
                            ))
                     
             ) 
             )
  )
)

server <- function(input, output){
  output$hist = renderPlot({
    if (input$varhist == 1){
      hist(rating,
           main="Histogram of Rating",
           xlab="rating",
           xlim=c(4,5),
           col="darkmagenta",
           freq=FALSE)
    }else if (input$varhist == 2){
      hist(ulasan,
           main = "Histogram Ulasan",
           xlab = "ulasan",
           col = "orange",
           freq = FALSE)
    } else if (input$varhist == 3){
      hist(dijual,
           main = "Histogram Produk Dijual",
           xlab = "produk dijual",
           col = "blue",
           freq = FALSE)
    } else if (input$varhist == 4){
      hist(bergabung,
           main ="Histogram Lama Bergabung",
           xlab = "lama bergabung (tahun)",
           col = "pink",
           freq = FALSE)
    } else {
      hist(balas_chat,
           main = "Histogram Lama Balas Chat",
           xlab = "lama balas chat (jam)",
           col = "lightskyblue",
           freq = FALSE)
    }
    
  })
  
  
  output$plotbox = renderPlot({
    if (input$boxp == 1){
      boxplot(ulasan,xlab="Ulasan",col="red")
    }else if (input$boxp == 2){
      boxplot(dijual,xlab="Produk Dijual",col="green")
    }else if (input$boxp == 3){
      boxplot(terjual,xlab="Produk Terjual",col="pink")
    }else if (input$boxp == 4){
      boxplot(bergabung,xlab="Lama Bergabung",col="blue")
    }else{
      boxplot(balas_chat,xlab="Lama Balas Chat",col="yellow")
    }
  })
  
  output$cleveland = renderPlot({
    if (input$clev == 1){
      dotchart(sort(ulasan1),
               pch = 19,
               col = 1,
               cex = 1,
               pt.cex = 3,
               labels = kota1) 
    }else if (input$clev == 2){
      dotchart(sort(dijual1),
               pch = 19,
               col = 2,
               cex = 1,
               pt.cex = 3,
               labels = kota1)
    }else if (input$clev == 3){
      dotchart(sort(terjual1),
               pch = 19,
               col = 3,
               cex = 1,
               pt.cex = 3,
               labels = kota1)
    }else if (input$clev == 4){
      dotchart(sort(bergabung1),
               pch = 19,
               col = 4,
               cex = 1,
               pt.cex = 3,
               labels = kota1)
    }else{
      dotchart(sort(balas_chat1),
               pch = 19,
               col = 5,
               cex = 1,
               pt.cex = 3,
               labels = kota1)
    }
  })
  output$prediksi <- renderPrint({
    
    ulasan = input$ulasan
    produk_terjual = input$dijual
    lama_bergabung = input$terjual
    lama_balas_chat = input$lama_bergabung 
    voucher = input$lama_dibalas
    produk_dijual = input$voucher
    data <- data.frame(ulasan,produk_terjual,produk_dijual,lama_bergabung,lama_balas_chat,voucher)
    data
    prediction <- predict(model, newdata = data)
    prediction
    cat("Hasil Prediksi:\n")
    cat(predict(model, newdata = data))
  })
  output$scatterplot <- renderPlot({
    ggplot(df, aes_string(x = input$x, y = input$y)) + 
      geom_point(aes(color = factor(rating))) + #warna geompoint berdasarkan rating
      scale_color_manual(values = c("#D62B2A","#E6BD45", "#E2E26A", "#A6E26A", "#356E63",
                                    "#2166AC", "#A66AE2", "#2E6ADE")) +
      theme_classic() + 
      theme(legend.position = "none")+
      geom_smooth(method = "lm", formula = "y ~ x")
  })
  output$barchart = renderPlot({
    if (input$bar == 1){
      barplot(height=barrating$value_rt, names=barrating$Rating, 
              col="#69b3a2",
              horiz=T, las=2,
              main = "Barchart Rating Toko",
              xlab = "Banyak Toko",
              ylab = "Rating"
      )
    }else{
      barplot(height=barterjual$value, names=barterjual$terjual, 
              col="blue",
              horiz=T, las=2,
              main = "Barchart Produk Terjual",
              xlab = "Banyak Toko"
      )
    }
  })
  output$piechart <- renderPlot({
    if (input$pie == 1){
      ggplot(pierating, aes(x="", y=value, fill=Rating)) +
        geom_bar(stat="identity", width=1, color="white") +
        geom_text(aes(label = percent),
                  position = position_stack(vjust = 0.5))+
        coord_polar("y", start=0) +
        theme_void()
    }else {
      ggplot(pievoucher, aes(x="", y=value_vc, fill=Voucher)) +
        geom_bar(stat="identity", width=1, color="white") +
        geom_text(aes(label = percent_vc),
                  position = position_stack(vjust = 0.5))+
        coord_polar("y", start=0) +
        theme_void()
    }
  })
  output$pendahuluan1 <- renderUI({
    req(input$sel_Var)
    if (input$sel_Var == "Marketplace"){
      return(HTML(paste("Perkembangan teknologi dan informasi yang semakin cepat di era modern ini berakibat pada pergeseran perilaku manusia, salah satunya dalam bidang bisnis terutama pada kegiatan jual beli. Perubahan tersebut dapat dilihat dan ditemui pada saat ini yaitu keberadaan marketplace jika dahulu penjual dan pembeli bertemu langsung pada tempat dan waktu yang sama, namun dengan marketplace penjual dan pembeli dapat melakukan transaksi tanpa bertemu langsung, hal tersebut dapat mengefisiensi waktu dan biaya. Marketplace merupakan sekumpulan tempat berjualan online di internet yang menjual produk atau jasa tertentu (Zakky, dkk., 2017). Salah satu marketplace yang populer dan merupakan salah satu karya anak bangsa adalah Tokopedia. Tokopedia merupakan salah satu marketplace yang populer di Indonesia, pada 2016 Tokopedia berada di peringkat 9.",
                        " ",
                        sep = "<br/>")))
    }
    
    
    
    else {
      return(HTML(paste("Rating merupakan bentuk lain dari sebuah opini yang diberikan oleh banyak orang dan menjadi evaluasi rata-rata dari para pembeli-pemberi rating- terhadap perbedaan fitur dari produk ataupun service penjual (Filieri, 2014) dan menjadi representasi dari opini konsumen dengan skala yang spesifik (Lackermair et al, 2013). Kepercayaan dan kepuasan pelanggan berupa review dan rating dari pelanggan merupakan salah satu faktor yang penting bagi keberlangsungan bisnis pada marketplace  baik dari pihak aplikasi maupun bagi penjual. Hal tersebut dikarena pelanggan mempertimbangkan membeli produk atau jasa melalui review  dan rating  dari pelanggan sebelumnya. Selain itu dari pihak pengembang aplikasi, pelanggan akan memberikan trust  atau kepercayaan terhadap marketplace tersebut sehingga dalam hal jual beli pelanggan akan mengakses aplikasi tersebut. oleh karena itu, review dan rating pelanggan merupakan salah satu hal penting dan merupakan salah satu faktor keberlangsungan suatu bisnis.",
                        sep = "<br/>")))
    }
    
    
    
    
  })
  
  output$pendahuluan2 <- renderUI({
    req(input$sel_Year5)
    if (input$sel_Year5 == "Consumer Rating"){
      return(HTML(paste("Consumer rating atau peringkat pelanggan merupakan opini atau review yang diberikan oleh pelanggan dalam bentuk skala yang ditentukan, biasanya pada marketplace ditentukan oleh banyaknya bintang. Banyaknya bintang dapat diinterpretasikan sebagai kualitas atas suatu barang yang dijual secara online (Mukhopadhyay & Chung, 2015). Rating dibuat oleh pelanggan yang telah melakukan pembelian online dan dipublikasikan baik dalam website ataupun aplikasi marketplace tersebut pada toko penjual merupakan salah satu feedback atau umpan balik yang diberikan oleh pelanggan kepada penjual. Biasanya, rating adalah salah satu cara untuk memberikan umpan balik yang dilakukan oleh konsumen kepada penjual (Dellarocas, 2003).",
                        " ","Rating dijadikan sebagai salah satu sumber informasi mengenai penjual dan produknya sehingga dapat dijadikan sebagai rujukan dalam menentukan pembelian maupun untuk kepentingan rekomendasi atau promosi ke konsumen. Presepsi seorang konsumen termasuk kepercayaan yang sebagian besar dipengaruhi oleh rating konsumen dalam e-commerce  (Wulff et al., 2015; Simonson dan Rosen, 2014; Kostyk, 2016). Sehingga consumer rating menjadi salah satu hal yang penting dalam bisnis tersebut.",
                        " ",
                        sep = "<br/>")))
    }
    else{
      return(HTML(paste("Exploratory Data Analysis diperkenalkan oleh ahli statistik John W. Tukey pada tahun 1977. Exploratory Data Analysis dapat disebut sebagai pekerjaan detektif, hal tersebut dikarenakan Exploratory Data Anaysis dikerjakan dengan melakukan eksplorasi terhadap data tanpa memiliki ide atau prasangka terhadap informasi yang akan didapatkan. Exploratory Data Analysis dapat didefinisikan sebagai pendekatan analisis data dengan berbagai teknik terutama dalam bentuk visualisasi grafis dengan tujuan untuk mengambil variabel yang penting dan diperlukan serta untuk memaksimalkan wawasan dalam dataset tersebut.  Exploratory Data Analysis adalah salah satu tahap dalam data science untuk mengenal dan memahami data, selain itu Exploratory Data Analysis juga merupakan suatu teknik pencarian heuristic untuk menemukan hubungan signifikan antara variabel pada dataset yang besar.",
                        " ","Secara definisi Exploratory Data Analysis mengacu  pada  proses kritis  dalam  melakukan  pengamatan awal  pada  data untuk menemukan pola, anomali, untuk menguji hipotesis dan untuk memeriksa asumsi dengan bantuan statistika deskriptif dan grafik.   Dengan  melakukan  EDA,  kita dapat  lebih  memahami kondisi  dataset  yang  kita miliki.  Sehingga,  kita  dapat  memulai  pembentukan  model  Machine  Learning  yang baik. Memahami dataset termasuk ke dalam beberapa hal yaitu variabel selection, identifikasi outliers, identifikasi missing values, memahami hubungan antar variabel, dan lain-lain.",
                        " ",
                        sep = "<br/>")))}
  })
  
  output$pendahuluan3 <- renderUI({
    req(input$sel_Region2)
    if (input$sel_Region2 == "Sumber Data"){
      return(HTML(paste("Data yang digunakan pada analisis ini didapat dengan melakukan survey terhadap toko fashion pada marketplace Tokopedia dengan lokasi toko yang berada pada kota JABODETABEK, Bandung, Semarang, Yogyakarta, Denpasar, dan Surabaya. Metode sampling yang digunakan dalam pengambilan data ini adalah dengan menggunakan metode quota sampling. Kuota sampling merupakan sebuah metode dalam pengambilan sampel yang dilakukan dengan mengambil sejumlah kuota sampel dari populasi dan menghentikan pengambilan setelah kuota terpenuhi.",
                        " ","Dataset yang telah didapat tersimpan dalam format CSV atau Comma Separated Value. Dataset ini berisi 9 variabel, yaitu nama toko, rating, ulasan, kota, produk terjual, produk dijual, lama bergabung, lama balas chat, dan voucher. Variabel nama toko merepresentasikan nama toko fashion yang terpilih untuk dijadikan sampel, variabel rating merepresentasikan rating toko tersebut, variabel ulasan merepresentasikan banyaknya ulasan atau review pada toko tersebut, variabel kota merupakan asal kota toko tersebut, produk terjual merepresentasikan banyaknya produk yang sudah berhasil terjual oleh toko tersebut, produk dijual merupakan banyaknya produk yang ditawarkan(dijual) oleh toko baik yang sudah terjual maupun belum terjual, lama bergabung merupakan variabel yang merepresentasikan berapa lama toko dibuka dalam marketplace Tokopedia, lama balas chat merupakan variabel yang merepresentasikan berapa lama penjual melayani pembeli lewat fitur chat yang telah tersedia, dan voucher merupakan variabel yang merepresentasikan apakah toko memberikan voucher atau tidak dengan penggunaan dummy variabel yaitu 0 jika toko tidak memberikan voucher dan 1 jika toko memberikan voucher.
",
                        " ",
                        sep = "<br/>")))
    }
    else{
      return(HTML(paste("Metode yang digunakan dalam analisis ini merupakan metode kuantitatif dengan melakukan analisis statistika deskriptif dan visualisasi secara grafis.  Metode kuantitatif Pendekatan kuantitatif menurut Arikuto (2016, dalam Abadiyah & Purwanto, 2016) yaitu pendekatan penelitian menggunakan angka dengan mengumpulkan data, menjelaskan data, serta menjelaskan hasilnya. Sedangkan metode analisis visualisasi melalui grafik merupakan suatu metode analisis dengan memperhatikan pola data yang disajikan secara grafis.  Dalam melakukan analisis secara grafis perlu dilakukan data preprocessing yang meliputi identifikasi dan handling missing values, identifikasi dan handling outliers, transformasi data, dan lain-lain. Kemudian data yang telah melewati tahapan tersebut divisualisasikan sesuai dengan tipe datanya.
",
                        " ",
                        sep = "<br/>")))
    }
  })
  
  
  output$summary = renderPrint({summary(df)})
  
  output$table = renderDataTable({df})
}
shinyApp(ui = ui, server = server)
