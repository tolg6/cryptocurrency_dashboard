
library(shiny)
library(shinydashboard)
library(shinythemes)
library(readxl)
library(lubridate)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)
library(gridExtra)
library(shinyKnobs)
library(tidyquant)

#BITCOIN
btc = read.csv("C:/Users/tolga/Desktop/guncel_btc.csv")
str(btc)
btc$Date = as_date(btc$Date)
btc[,2:7] = apply(btc[,2:7],2,as.character);btc[,2:7] = apply(btc[,2:7],2,as.numeric)
na = which(is.na(btc$Close))
btc = btc[-na,]

#ETHEREUM
eth = read.csv("C:/Users/tolga/Desktop/guncel_eth.csv")
eth$Date = as_date(eth$Date)
eth[,2:7] = apply(eth[,2:7],2,as.character);eth[,2:7] = apply(eth[,2:7],2,as.numeric)
na = which(is.na(eth$Close))
eth = eth[-na,]

#LITECOIN
ltc = read.csv("C:/Users/tolga/Desktop/guncel_ltc.csv")
ltc$Date = as_date(ltc$Date)
ltc[,2:7] = apply(ltc[,2:7],2,as.character);ltc[,2:7] = apply(ltc[,2:7],2,as.numeric)
na = which(is.na(ltc$Close))
ltc = ltc[-na,]

# Günlük Değişim
btc$degisim = btc$Open-btc$Close
eth$degisim =eth$Open-eth$Close
ltc$degisim = ltc$Open-ltc$Close




ui <- dashboardPage(
    skin = "red",
    dashboardHeader(title = "Crypto Dashboard"),
    
    dashboardSidebar(sidebarMenu(
        menuItem("BITCOIN",tabName = "btc",icon = icon("bitcoin")),
        menuItem("ETHEREUM",tabName = "eth",icon = icon("ethereum")),
        menuItem("LİTECOİN",tabName = "ltc",icon = icon("dashboard")),
        menuItem("Grafikler",tabName = "grf",icon = icon("dashboard"))
    )),
    
    dashboardBody(
        tabItems(
            tabItem(tabName = "btc",box(width = 1.5,dateRangeInput(inputId = "datebtc",label = "Tarih Aralığı Seç",min = min(btc$Date),max = max(btc$Date),separator = "-",start = "2020-11-07",end = "2021-02-06")),
                    box(plotlyOutput("btcplot")),
                    box(plotOutput("btcvolume")),
                    box(status = "warning",title = "ATH Sıralaması",solidHeader = T,sliderInput(inputId = "btcslide",label = "Geçmiş All Time High Değerleri",min = 1,max = 10,value = 7)),
                    
                    box(width = 2,status = "primary",tableOutput("athbtc")),
                    valueBoxOutput("btcbox"),
                    valueBoxOutput("btcbox1"),
                    valueBoxOutput("btcbox2")
                    
                    ),
                    
            tabItem(tabName = "eth",box(width = 1.5,dateRangeInput(inputId = "dateeth",label = "Tarih Aralığı Seç",min = min(eth$Date),max = max(eth$Date),separator = "-",start = "2020-11-07",end = "2021-02-06")),
                    box(plotlyOutput("ethplot")),
                    box(plotOutput("ethvolume")),
                    box(status = "warning",title = "ATH Sıralaması",solidHeader = T,sliderInput(inputId = "ethslide",label = "Geçmiş All Time High Değerleri ",min = 1,max = 10,value = 7)),
                    box(width = 2,status = "danger",tableOutput("atheth")),
                    valueBoxOutput("ethbox"),
                    valueBoxOutput("ethbox1"),
                    valueBoxOutput("ethbox2")),
            tabItem(tabName = "ltc",box(width = 1.5,dateRangeInput(inputId = "dateltc",label = "Tarih Aralığı Seç",min = min(ltc$Date),max = max(ltc$Date),separator = "-",start = "2020-11-07",end = "2021-02-06")),
                    box(plotlyOutput("ltcplot")),
                    box(plotOutput("ltcvolume")),
                    box(status = "warning",title = "ATH Sıralaması",solidHeader = T,sliderInput(inputId = "ltcslide",label = "Geçmiş All Time High Değerleri ",min = 1,max = 10,value = 7)),
                    box(width = 2,status = "success",tableOutput("athltc")),
                    valueBoxOutput("ltcbox"),
                    valueBoxOutput("ltcbox1"),
                    valueBoxOutput("ltcbox2")),
            tabItem(tabName = "grf",
                    box(plotOutput("multplt")),
                    box(plotOutput("p1p2")),
                    box(plotOutput("plot3")),
                    box(plotOutput("vlm")))
        )
    )
    
)


server <- function(input, output){
    #btcdata = eventReactive({filter(btc,Date>=input$datebtc[1]&Date<=input$datebtc[2])})
    #ethdata = filter(eth,Date>=input$dateeth[1]&Date<=input$dateeth[2])
    #ltcdata = filter(ltc,Date>=input$dateltc[1]&Date<=input$dateltc[2])
    output$btcplot = renderPlotly({btc%>%filter(Date>=input$datebtc[1]&Date<=input$datebtc[2])%>%plot_ly(x =~Date,type="candlestick",open =~Open,close = ~Close,high = ~High, low =~Low)%>%
            layout(plot_bgcolor = "#000000",paper_bgcolor = "#000000",title = "Bitcoin")
    
    })
    output$ethplot = renderPlotly({eth%>%filter(Date>=input$dateeth[1]&Date<=input$dateeth[2])%>%plot_ly(x =~Date,type="candlestick",open =~Open,close = ~Close,high = ~High, low =~Low,Volume = TRUE)%>%
            layout(plot_bgcolor = "#000000",paper_bgcolor = "#000000",title = "Ethereum")})
    
    output$ltcplot = renderPlotly({ltc%>%filter(Date>=input$dateltc[1]&Date<=input$dateltc[2])%>%plot_ly(x =~Date,type="candlestick",open =~Open,close = ~Close,high = ~High, low =~Low)%>%
            layout(plot_bgcolor = "#000000",paper_bgcolor = "#000000",title = "Litecoin")})
    
    output$btcvolume = renderPlot(btc%>%filter(Date>=input$datebtc[1]&Date<=input$datebtc[2])%>%ggplot(aes(x = Date,y = Volume))+geom_area(fill="#087EB0", alpha=0.5)+geom_line(color="#087EB0")+geom_ma(ma_fun = SMA,n =1,color = "red")+theme_minimal()+ggtitle("Bitcoin Hacim Grafiği"))
    output$ethvolume = renderPlot(eth%>%filter(Date>=input$dateeth[1]&Date<=input$dateeth[2])%>%ggplot(aes(x = Date,y = Volume))+geom_area(fill="#087EB0", alpha=0.5)+geom_line(color="#087EB0")+geom_ma(ma_fun = SMA,n =1,color = "red")+theme_minimal()+ggtitle("Ethereum Hacim Grafiği"))
    output$ltcvolume = renderPlot(ltc%>%filter(Date>=input$dateltc[1]&Date<=input$dateltc[2])%>%ggplot(aes(x = Date,y = Volume))+geom_area(fill="#087EB0", alpha=0.5)+geom_line(color="#087EB0")+geom_ma(ma_fun = SMA,n =1,color = "red")+theme_minimal()+ggtitle("Litecoin Hacim Grafiği"))
    # Table
    sortbtc = select(btc,Date,High,Low);sortbtc = sortbtc[order(sortbtc$High,decreasing = T),]
    output$athbtc = renderTable({head(sortbtc,input$btcslide)})
    sorteth = select(eth,Date,High,Low);sorteth = sorteth[order(sorteth$High,decreasing = T),]
    output$atheth = renderTable({head(sorteth,input$ethslide)})
    sortltc = select(ltc,Date,High,Low);sortltc = sortltc[order(sortltc$High,decreasing = T),]
    output$athltc = renderTable({head(sortltc,input$ltcslide)})
    #output$degisim = renderPlotly({btc%>%filter(Date>=input$datebtc[1]&Date<=input$datebtc[2])%>%plot_ly(x = ~Date, y = ~degisim, type = 'scatter', mode = 'lines')})
    output$multplt = renderPlot({ggplot(btc,aes(x = Date,y = Close))+geom_line(aes(color = "black"))+geom_line(aes(x = Date,y = ltc$Close))+geom_line(aes(x = Date,y = eth$Close,color = "green"))+scale_y_log10()+ggtitle("BTC-ETH-LTC")+theme_light()})
    
    
    #Plot-2
    p1 = ggplot(btc,aes(x = Date,y = Close))+geom_line()+geom_area(fill = "#00f5ff",alpha=0.5)+ggtitle("BTC-USD")+theme_light()
    p2 = ggplot(eth,aes(x = Date,y = Close))+geom_line()+geom_area(fill = "#00f5ff",alpha=0.5)+ggtitle("ETH-USD")+theme_light()
    p3 = ggplot(ltc,aes(x = Date,y = Close))+geom_line()+geom_area(fill = "#00f5ff",alpha=0.5)+ggtitle("LTC-USD")+theme_light()
    output$p1p2 = renderPlot({grid.arrange(p1,p2,p3)})
    
    #Plot-3
    q1 = ggplot(btc,aes(x = Date,y = degisim))+geom_line(color ="#00008b")+ggtitle("BTC Günlük Değişim")+theme_light()
    q2 = ggplot(eth,aes(x = Date,y = degisim))+geom_line(color ="#00008b")+ggtitle("ETH Günlük Değişim")+theme_light()
    q3 = ggplot(ltc,aes(x = Date,y = degisim))+geom_line(color ="#00008b")+ggtitle("LTC Günlük Değişim")+theme_light()
    output$plot3 = renderPlot({grid.arrange(q1,q2,q3)})
    
    #Plot-4 
    a1 = ggplot(btc,aes(x = Date,y = Volume))+geom_line()+geom_area(fill = "#00cd00",alpha = .5)+ggtitle("BTC Hacim")+theme_light()
    a2 = ggplot(eth,aes(x = Date,y = Volume))+geom_line()+geom_area(fill = "#00cd00",alpha = .5)+ggtitle("ETH Hacim")+theme_light()
    a3 = ggplot(ltc,aes(x = Date,y = Volume))+geom_line()+geom_area(fill = "#00cd00",alpha = .5)+ggtitle("LTC Hacim")+theme_light()
    output$vlm = renderPlot({grid.arrange(a1,a2,a3)})
    
    # Box-BTC
    output$btcbox = renderValueBox({valueBox(paste(round(btc%>%filter(Date >=input$datebtc[1]&Date<=input$datebtc[2])%>%select(High)%>%max(),0),"$"),"Seçilen Zaman Aralığındaki En Yüksek Fiyat",icon = icon("list"),color = "green")})
    output$btcbox1 = renderValueBox({valueBox(paste(round(btc%>%filter(Date >=input$datebtc[1]&Date<=input$datebtc[2])%>%select(High)%>%min()),"$"),"Seçilen Zaman Aralığındaki En Düşük Fiyat",icon = icon("list"),color = "red")})
    output$btcbox2 = renderValueBox({valueBox(paste("%",round((btc[length(btc$High),]$High)*100/btc[1,]$Open,0)),"2015-2021 Arası Büyüme Yüzdesi",icon = icon("list"),color = "blue")})
    #Box-ETH
    output$ethbox = renderValueBox({valueBox(paste(round(eth%>%filter(Date >=input$dateeth[1]&Date<=input$dateeth[2])%>%select(High)%>%max(),0),"$"),"Seçilen Zaman Aralığındaki En Yüksek Fiyat",icon = icon("list"),color = "green")})
    output$ethbox1 = renderValueBox({valueBox(paste(round(eth%>%filter(Date >=input$dateeth[1]&Date<=input$dateeth[2])%>%select(High)%>%min()),"$"),"Seçilen Zaman Aralığındaki En Düşük Fiyat",icon = icon("list"),color = "red")})
    output$ethbox2 = renderValueBox({valueBox(paste("%",round((eth[length(eth$High),]$High)*100/eth[1,]$Open,0)),"2015-2021 Arası Büyüme Yüzdesi",icon = icon("list"),color = "blue")})
    #Box-LTC
    output$ltcbox = renderValueBox({valueBox(paste(round(ltc%>%filter(Date >=input$dateltc[1]&Date<=input$dateltc[2])%>%select(High)%>%max(),0),"$"),"Seçilen Zaman Aralığındaki En Yüksek Fiyat",icon = icon("list"),color = "green")})
    output$ltcbox1 = renderValueBox({valueBox(paste(round(ltc%>%filter(Date >=input$dateltc[1]&Date<=input$dateltc[2])%>%select(High)%>%min()),"$"),"Seçilen Zaman Aralığındaki En Düşük Fiyat",icon = icon("list"),color = "red")})
    output$ltcbox2 = renderValueBox({valueBox(paste("%",round((ltc[length(ltc$High),]$High)*100/ltc[1,]$Open,0)),"2015-2021 Arası Büyüme Yüzdesi",icon = icon("list"),color = "blue")})
}


shinyApp(ui = ui, server = server)










