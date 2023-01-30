#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(rsconnect)
library(DBI)
library(knitr)
library(kableExtra)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(shiny)
library(shinyFiles)
library(shinyAce)
library(writexl)
library(readr)
library(plotly)
library(shinythemes)
# library(BatchGetSymbols)
library(forecast)
library(shinycssloaders)
library(lubridate)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

   options(shiny.maxRequestSize=300*1024^2)
   
######################################### IMPORT BASE #########################################

   Base <- eventReactive(input$button, {
      validate(
         need(input$file1 != "", "Por favor, inserir o arquivo baixado da B3 e tratado previamente ou selecionar as ações no campo AÇÕES!")
      )
      base <- as.data.frame(read.csv(input$file1$datapath,sep=",", encoding = "UTF-8"))
      
      base$CODBDI <- str_replace(base$CODBDI,",",".")
      base$CODBDI <- as.numeric(base$CODBDI)
      
      base$TPMERC <- str_replace(base$TPMERC,",",".")
      base$TPMERC <- as.numeric(base$TPMERC)
      
      base$PREABE <- str_replace(base$PREABE,",",".")
      base$PREABE <- as.numeric(base$PREABE)
      
      base$PREMAX <- str_replace(base$PREMAX,",",".")
      base$PREMAX <- as.numeric(base$PREMAX)

      base$PREMED <- str_replace(base$PREMED,",",".")
      base$PREMED <- as.numeric(base$PREMED)
      
      base$PREMIN <- str_replace(base$PREMIN,",",".")
      base$PREMIN <- as.numeric(base$PREMIN)
      
      base$PREULT <- str_replace(base$PREULT,",",".")
      base$PREULT <- as.numeric(base$PREULT)
      
      base$QUATOT <- str_replace(base$QUATOT,",",".")
      base$QUATOT <- as.numeric(base$QUATOT)
      
      base$VOLTOT <- str_replace(base$VOLTOT,",",".")
      base$VOLTOT <- as.numeric(base$VOLTOT)
      
      base$DT_PREGAO <- ymd(base$DT_PREGAO)
      
      ativo <- as.numeric(input$tp_ativo)
      
      ifelse(ativo==0,
             base01 <- base %>% 
               dplyr::mutate(PREABE = PREABE/100, PREMAX = PREMAX/100, PREMIN = PREMIN/100, 
                    PREMED = PREMED/100, PREULT = PREULT/100) %>% 
               dplyr::mutate(VARIACAO = (PREULT-PREABE)/PREABE,  PER_VOLUME = QUATOT/VOLTOT) %>% 
               dplyr::filter(PREULT<=input$limitador),
                  base01 <- base %>% 
                  dplyr::mutate(PREABE = PREABE/100, PREMAX = PREMAX/100, PREMIN = PREMIN/100, 
                         PREMED = PREMED/100, PREULT = PREULT/100) %>% 
                  dplyr::mutate(VARIACAO = ((PREULT-PREABE)/PREABE)*100,  
                         PER_VOLUME = print(TOTNEG/QUATOT), digits=8) %>% 
                  dplyr::filter(CODBDI==ativo) %>% 
                  dplyr::filter(PREULT<=input$limitador))
      
      base01 <- as.data.frame(base01)
      
      base01[is.na(base01)] <- 0
      
      return(base01)
      
   })
   
 Base1 <- eventReactive(input$stckdireto, {

    bg <- input$dtstckselect[1]
    lst <- input$dtstckselect[2]
    base <- as.data.frame(read.csv(input$file1$datapath,sep=";", encoding = "UTF-8"))
    base$DT_PREGAO <- as.Date.POSIXct(paste0(substr(base$DT_PREGAO,7,10),"/",substr(base$DT_PREGAO,4,6),substr(base$DT_PREGAO,1,2)),"%Y-%m-%d", origin = "1984-01-01")

    ifelse(bg==lst,
           base <- base %>% dplyr::filter(DT_PREGAO >= bg),
           base <- base %>% dplyr::filter(DT_PREGAO >= bg & DT_PREGAO <= ls))

    base <- as.data.frame(base[2])

    colnames(base) <- c("PREABE", "PREMAX", "PREMIN", "PREULT", "VOLTOT", "PREAJUST", "DT_PREGAO", "CODNEG", "PREOFC", "PREOFV")

    base01 <- base %>%
       mutate(VARIACAO = (PREULT-PREABE)/PREABE)

    base01 <- as.data.frame(base01)

    base01[is.na(base01)] <- 0

    return(base01)

 })
 # 
 #Input
 
 # Base_01 <- eventReactive(input$button, {
 #   
 #    base <- Base0()
 #    
 #   ativo <- as.numeric(input$tp_ativo)
 #   
 #   
 #   ifelse(ativo==0,
 #          base01 <- base %>% 
 #             mutate(PREABE = PREABE/100, PREMAX = PREMAX/100, PREMIN = PREMIN/100, PREMED = PREMED/100, PREULT = PREULT/100) %>% mutate(VARIACAO = (PREULT-PREABE)/PREABE,  PER_VOLUME = QUATOT/VOLTOT) %>% filter(PREULT<=input$limitador),
 #          base01 <- base %>% 
 #      mutate(PREABE = PREABE/100, PREMAX = PREMAX/100, PREMIN = PREMIN/100, PREMED = PREMED/100, PREULT = PREULT/100) %>% mutate(VARIACAO = ((PREULT-PREABE)/PREABE)*100,  PER_VOLUME = print(TOTNEG/QUATOT), digits=8) %>% 
 #      filter(CODBDI==ativo) %>% 
 #      filter(PREULT<=input$limitador))
 #   
 #   base01 <- as.data.frame(base01)
 #   
 #   base01[is.na(base01)] <- 0
 #   
 #   return(base01)
 # 
 # })
 
 #Base download direto ações
 
 # Base_01 <- eventReactive(input$stckdireto, {
 #    
 #    base <- Base1()
 #    
 #           base01 <- base %>% 
 #              mutate(VARIACAO = (PREULT-PREABE)/PREABE)
 # 
 #    base01 <- as.data.frame(base01)
 #    
 #    base01[is.na(base01)] <- 0
 #    
 #    return(base01)
 #    
 # })
 
################################# Tabela legenda Códigos B3 #################################
 
cod_BDI <- eventReactive(input$tabelao, {
   
   base <- Base()
   
   codigos_bdi <- base %>% 
     dplyr::group_by(CODBDI) %>% 
     summarise("TIPOS" = sum(base$TIPO_DE_REG), n=n()) %>%
     dplyr::mutate(valor = ifelse(CODBDI == 2,  'LOTE PADRÃO',
   ifelse(CODBDI == 6,  'CONCORDATÁRIAS',
   ifelse(CODBDI == 10, 'DIREITOS E RECIBOS',
   ifelse(CODBDI == 12, 'FUNDOS IMOBILIÁRIOS',
   ifelse(CODBDI == 14, 'CERTIFIC. INVESTIMENTO / DEBÊNTURES / TÍTULOS DIVIDA PÚBLICA',
   ifelse(CODBDI == 18, 'OBRIGAÇÕES',
   ifelse(CODBDI == 22, 'BÔNUS (PRIVADOS)',
   ifelse(CODBDI == 26, 'APÓLICES / BÔNUS / TÍTULOS PÚBLICOS',
   ifelse(CODBDI == 32, 'EXERCÍCIO DE OPÇÕES DE COMPRA DE ÍNDICE',
   ifelse(CODBDI == 33, 'EXERCÍCIO DE OPÇÕES DE VENDA DE ÍNDICE',
   ifelse(CODBDI == 38, 'EXERCÍCIO DE OPÇÕES DE COMPRA',
   ifelse(CODBDI == 42, 'EXERCÍCIO DE OPÇÕES DE VENDA',
   ifelse(CODBDI == 46, 'LEILÃO DE TÍTULOS NÃO COTADOS',
   ifelse(CODBDI == 48, 'LEILÃO DE PRIVATIZAÇÃO',
   ifelse(CODBDI == 50, 'LEILÃO',
   ifelse(CODBDI == 51, 'LEILÃO FINOR',
   ifelse(CODBDI == 52, 'LEILÃO FINAM',
   ifelse(CODBDI == 53, 'LEILÃO FISET',
   ifelse(CODBDI == 54, 'LEILÃO DE AÇÕES EM MORA',
   ifelse(CODBDI == 56, 'VENDAS POR ALVARÁ JUDICIAL',
   ifelse(CODBDI == 58, 'OUTROS',
   ifelse(CODBDI == 60, 'PERMUTA POR AÇÕES',
   ifelse(CODBDI == 61, 'META',
   ifelse(CODBDI == 62, 'TERMO',
   ifelse(CODBDI == 66, 'DEBÊNTURES COM DATA DE VENCIMENTO ATÉ 3 ANOS',
   ifelse(CODBDI == 68, 'DEBÊNTURES COM DATA DE VENCIMENTO MAIOR QUE 3 ANOS',
   ifelse(CODBDI == 70, 'FUTURO COM MOVIMENTAÇÃO CONTÍNUA',
   ifelse(CODBDI == 71, 'FUTURO COM RETENÇÃO DE GANHO',
   ifelse(CODBDI == 74, 'OPÇÕES DE COMPRA DE ÍNDICES',
   ifelse(CODBDI == 75, 'OPÇÕES DE VENDA DE ÍNDICES',
   ifelse(CODBDI == 78, 'OPÇÕES DE COMPRA',
   ifelse(CODBDI == 82, 'OPÇÕES DE VENDA',
   ifelse(CODBDI == 83, 'DEBÊNTURES E NOTAS PROMISSÓRIAS',
   ifelse(CODBDI == 96, 'FRACIONÁRIO',
   ifelse(CODBDI == 99, 'TOTAL GERAL',""))))))))))))))))))))))))))))))))))))
   
   return(codigos_bdi)
   
})
 
################################# Gráfico Valorização #################################

rank <- eventReactive(input$button, {
   
   base01 <- Base()
   
   base_loli <- base01 %>% 
     dplyr::filter(PREULT<=input$limitador) %>% 
     dplyr::select(CODNEG, VARIACAO, PER_VOLUME) %>% 
     dplyr::group_by(VARIACAO) %>% 
     dplyr::arrange(desc(VARIACAO), desc(PER_VOLUME)) 
   
   base_loli <- unique(base_loli)
   
   base_loli <- head(base_loli,20) 
   
   base_loli %>%
     dplyr::arrange(desc(VARIACAO)) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
     dplyr::mutate(name=factor(CODNEG, levels=CODNEG)) %>%   # This trick update the factor levels
      ggplot(aes(x=name, y=VARIACAO)) +
      geom_segment( aes(xend=name, yend=0)) +
      geom_point( size=4, color="orange") +
      coord_flip() +
      theme_bw() +
      xlab("")
   
})

#Download direto  

# rank1 <- eventReactive(input$stckdireto, {
#    
#    base01 <- Base1()
#    
#    base_loli <- base01 %>% 
#       select(CODNEG, VARIACAO) %>% 
#       group_by(VARIACAO) %>% 
#       arrange(desc(VARIACAO)) 
#    
#    base_loli <- unique(base_loli)
#    
#    base_loli <- head(base_loli,20) 
#    
#    base_loli %>%
#       arrange(desc(VARIACAO)) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
#       mutate(name=factor(CODNEG, levels=CODNEG)) %>%   # This trick update the factor levels
#       ggplot(aes(x=name, y=VARIACAO)) +
#       geom_segment( aes(xend=name, yend=0)) +
#       geom_point( size=4, color="orange") +
#       coord_flip() +
#       theme_bw() +
#       xlab("")
#    
# })

################################# Gráfico desvalorização #################################

rank_desc <- eventReactive(input$button, {
   
   base01 <- Base()
   
   base_loli1 <- base01 %>% 
     dplyr::filter(PREULT<=input$limitador) %>% 
     dplyr::select(CODNEG, VARIACAO) %>% 
     dplyr::group_by(VARIACAO) %>% 
     dplyr::arrange(VARIACAO) 
   
   base_loli1 <- unique(base_loli1)
   
   base_loli1 <- head(base_loli1,20) 
   
   base_loli1 %>%
     dplyr::arrange(VARIACAO) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
      mutate(name=factor(CODNEG, levels=CODNEG)) %>%   # This trick update the factor levels
      ggplot(aes(x=name, y=VARIACAO)) +
      geom_segment( aes(xend=name, yend=0)) +
      geom_point( size=4, color="orange") +
      coord_flip() +
      theme_bw() +
      xlab("")
   
})

#Download Direto#
# 
# rank_desc1 <- eventReactive(input$stckdireto, {
#    
#    base01 <- Base1()
#    
#    base_loli1 <- base01 %>% 
#       select(CODNEG, VARIACAO) %>% 
#       group_by(VARIACAO) %>% 
#       arrange(VARIACAO) 
#    
#    base_loli1 <- unique(base_loli1)
#    
#    base_loli1 <- head(base_loli1,20) 
#    
#    base_loli1 %>%
#       arrange(VARIACAO) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
#       mutate(name=factor(CODNEG, levels=CODNEG)) %>%   # This trick update the factor levels
#       ggplot(aes(x=name, y=VARIACAO)) +
#       geom_segment( aes(xend=name, yend=0)) +
#       geom_point( size=4, color="orange") +
#       coord_flip() +
#       theme_bw() +
#       xlab("")
#    
# })

################################# Gráfico Ações #################################

################################# ARIMA #################################

stockcheck <- eventReactive(input$stock, {
   
   # acao <- c(paste(input$nomestck,".SA",sep = ""),input$nomestck)
   bg <- input$dtstck[1]
   lst <- input$dtstck[2]
   data <- Base()
   
   # bench <- '^BVSP' 
   # data <- BatchGetSymbols(tickers = acao, bench.ticker = bench,
   #                       first.date = bg,last.date = lst)
   
   # tabelastck <- as.data.frame(data[2])
   
   tabelastck <- data %>% dplyr::select(DT_PREGAO, PREABE, PREMAX, PREMIN, PREULT, VOLTOT, DT_PREGAO, CODNEG) %>% dplyr::filter(CODNEG == input$nomestck)

   #BASE DE AÇÕES#
   
   fecha <- tabelastck %>% dplyr::select(PREULT)
   
   #MODELO ARIMA#
   
   ifelse(input$tp_modelo == 1,
   
   {arimado <- auto.arima(fecha)
   arimado1 <- forecast(arimado, lst-bg)
   
   x <- matrix(0,as.integer(lst-bg),1)
   x <- as.matrix(x)
   
   base_arima <- as.data.frame(cbind(x,arimado1$upper,arimado1$lower,arimado1$mean,x,x,x))
   
   base_arima <- base_arima[,-c(3,5)]
   
   nomes <- c("df.tickers.ref.date",colnames(tabelastck[,-1]))
   
   colnames(base_arima) <- nomes
   
   for (i in 1:nrow(base_arima)) {
     base_arima[i,1] = i                            
                                    }
   
   i = 1
   j = 0
   
   for (i in 1:as.integer(lst-bg)) {
      
      base_arima$df.tickers.ref.date[i] <- tabelastck$DT_PREGAO[nrow(tabelastck)]+j
      
      i=i+1
      j=j+1
      
   }
   
   base_arima$df.tickers.ref.date <- as.Date(base_arima$df.tickers.ref.date, origin="1970-01-01")
  
   #Gráfico#
   
   fig <- plot_ly(base_arima, x = ~base_arima$df.tickers.ref.date, y = ~base_arima$df.tickers.price.high, type = 'scatter', mode = 'lines',
                  line = list(color = 'transparent'),
                  showlegend = FALSE, name = 'High') 
   fig <- fig %>% add_trace(y = ~base_arima$df.tickers.price.low, type = 'scatter', mode = 'lines',
                            fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
                            showlegend = FALSE, name = 'Low') 
   fig <- fig %>% add_trace(x = ~base_arima$df.tickers.ref.date, y = ~base_arima$df.tickers.price.close, type = 'scatter', mode = 'lines',
                            line = list(color='rgb(0,100,80)'),
                            name = 'Close') 
   fig <- fig %>% layout(title = paste("Preço de ações ao longo do tempo",input$nomestck),
                         paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                         xaxis = list(title = "Days",
                                      gridcolor = 'rgb(255,255,255)',
                                      showgrid = TRUE,
                                      showline = FALSE,
                                      showticklabels = TRUE,
                                      tickcolor = 'rgb(127,127,127)',
                                      ticks = 'outside',
                                      zeroline = FALSE),
                         yaxis = list(title = "Valor (R$)",
                                      gridcolor = 'rgb(255,255,255)',
                                      showgrid = TRUE,
                                      showline = FALSE,
                                      showticklabels = TRUE,
                                      tickcolor = 'rgb(127,127,127)',
                                      ticks = 'outside',
                                      zeroline = FALSE))

   fig
   },
   
   ################################# GBM #################################
   
               ifelse(input$tp_modelo == 2, 
                   
                   {
                   # heads <- GetIbovStocks()
                   # heads <- c(heads$tickers)
                   # heads <- paste(heads, sep = "", ".SA")
                   
                   # BVSP <- BatchGetSymbols(tickers = heads, bench.ticker = '^BVSP', first.date = bg,last.date = lst)
                   BVSP <- data %>% dplyr::select(PREABE, PREMAX, PREMIN, PREULT, VOLTOT, DT_PREGAO, CODNEG)
                   
                   # BVSP1 <- as.data.frame(BVSP[2])
                   
                   Fec_BVSP <- BVSP$PREULT
                   Fec_BVSP <- Fec_BVSP[!is.na(Fec_BVSP)]
                   Fec_BVSP <- as.data.frame(Fec_BVSP)
                   
                   ret_Fec_BVSP <- BVSP$VARIACAO
                   ret_Fec_BVSP <- ret_Fec_BVSP[!is.na(ret_Fec_BVSP)]
                   retorno_mercado <- mean(ret_Fec_BVSP)
                   
                   ret_acao <- (tabelastck$PREULT)
                   
                   Mandela <- rnorm(n = 1000, mean = , sd = )
                   mean <- mean(Mandela)
                   sigma <- sd(ret_acao)/sqrt(as.integer(lst-bg))
                   
                   Rf <- 0.1375
                   B <- var(ret_acao)/var(Fec_BVSP)
                   Rm <- Rf  + B * (retorno_mercado - Rf)
                   
                   #colocar distribui??o para adquirir par?metros
                   paths <- 1000
                   #count<- 250
                   count<- as.integer(lst-bg)
                   interval<-1/count
                   sample <- matrix(0,nrow=(count+1),ncol=paths)
                   sample[1,] <- as.data.frame(ret_acao)[nrow(as.data.frame(ret_acao)),1]
                   for(i in 1:paths)
                   {
                      #    sample[1,i]<- as.data.frame(ret_acao)[nrow(as.data.frame(ret_acao)),1]
                      for(j in 2:(count+1))
                      {
                         sample[j,i]<-sample[j-1,i]*exp(interval*(Rm-((sigma)^2)/2)+(sqrt(interval))*rnorm(1)*sigma) #Expression for Geometric Brownian Motion
                      }
                   }
                   
                   sample1 <- as.numeric(rowMeans(sample))
                   sample1 <- as.data.frame(sample1)
                   
                   sample2 <- rowMeans(sample) + apply(sample, 1, sd) * 1.64
                   sample3 <- rowMeans(sample) - apply(sample, 1, sd) * 1.64
                   
                   sample4 <- data.frame(c(1:as.integer(lst-bg+1)),sample1,sample2,sample3)
                   
                   nm <- c("x","md", "sd1", "sd2")
                   
                   colnames(sample4) <- nm
                   
                   #Gráfico#
                   
                   fig <- plot_ly(sample4, x = ~x, y = ~sd1, type = 'scatter', mode = 'lines',
                                  line = list(color = 'transparent'),
                                  showlegend = FALSE, name = 'High') 
                   fig <- fig %>% add_trace(y = ~sd2, type = 'scatter', mode = 'lines',
                                            fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
                                            showlegend = FALSE, name = 'Low') 
                   fig <- fig %>% add_trace(x = ~x, y = ~md, type = 'scatter', mode = 'lines',
                                            line = list(color='rgb(0,100,80)'),
                                            name = 'Close') 
                   fig <- fig %>% layout(title = paste("Preço de ações ao longo do tempo",input$nomestck),
                                         paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                                         xaxis = list(title = "Days",
                                                      gridcolor = 'rgb(255,255,255)',
                                                      showgrid = TRUE,
                                                      showline = FALSE,
                                                      showticklabels = TRUE,
                                                      tickcolor = 'rgb(127,127,127)',
                                                      ticks = 'outside',
                                                      zeroline = FALSE),
                                         yaxis = list(title = "Valor (R$)",
                                                      gridcolor = 'rgb(255,255,255)',
                                                      showgrid = TRUE,
                                                      showline = FALSE,
                                                      showticklabels = TRUE,
                                                      tickcolor = 'rgb(127,127,127)',
                                                      ticks = 'outside',
                                                      zeroline = FALSE))
                   
                   fig
                   
                   },
                        ifelse(input$tp_modelo == 3,
                               {
                                  fig <- tabelastck %>% plot_ly(x = ~tabelastck$DT_PREGAO, type="candlestick",
                                    open = ~tabelastck$PREABE, close = ~tabelastck$PREULT,
                                    high = ~tabelastck$PREMAX, low = ~tabelastck$PREMIN) 
                                  fig <- fig %>% layout(title = paste("Preço de ações ao longo do tempo",input$nomestck))
                                  
                                  fig
                              }
                              )
                   )
   )
   
   return(fig)
   
})

#Download Exemplo#

output$exmpl <- downloadHandler(
   filename = function() {
      paste("base_diaria1", ".csv", sep="")
   },
   content = function(file) {
      write.csv2(base_diaria1, file)
   }
)

#Outputs#

observeEvent(input$button, {output$Base_B3 <- DT::renderDataTable({datatable(Base())})})
#observeEvent(input$stckdireto, {output$Base_B3 <- DT::renderDataTable({Base1()})})

output$CODS <- DT::renderDataTable({cod_BDI()})

observeEvent(input$button, {output$RANK <- renderPlot({rank()})})
#observeEvent(input$stckdireto, {output$RANK <- renderPlot({rank1()})})

observeEvent(input$button, {output$RANK1 <- renderPlot({rank_desc()})})
#observeEvent(input$stckdireto, {output$RANK1 <- renderPlot({rank_desc1()})})

output$stck <- renderPlotly({stockcheck()})
 
})


