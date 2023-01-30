#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(rsconnect)
library(shinythemes)
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
# library(BatchGetSymbols)
library(openssl)
library(shinybusy)
library(shinyWidgets)
library(readr)

rsconnect::setAccountInfo(name='seagullskf', token='54D077C2B3987ED60247EACF0EF288CA', secret='HPYlykFEOnR4ENEv7rOFS/iJil6qkVU2eU2v87fh')

base_diaria1 <- read_delim("base_diaria1.csv",
                           ";", escape_double = FALSE, locale = locale(encoding = "ASCII"),
                           trim_ws = TRUE)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("journal"),
                  add_busy_spinner(spin = "fading-circle"),
    # Application title
    titlePanel(paste("Análise Diária da Bolsa - ", Sys.Date())),
    img(src = "B3.png", height = 100, width = 100, align = "bottom-right"),
    h6("NOTA:"),
    p('Esse aplicativo não deve ser utilizado como sugestão, tendo apenas fins educativos e informativos.', style = "font-family: 'arial'; font-si8pt"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            p("Upload arquivo de cotações históricas baixado no site da B3", style = "font-family: 'arial'; font-si10pt"),
            
            hr(),
            
            fileInput("file1", "Inserir arquivo CSV tratado previamente",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            downloadButton("exmpl", "Baixar arquivo exemplo da B3"),
            
            hr(),
            
            p("Opções de consulta", style = "font-family: 'arial'; font-si10pt"),
            
            hr(),
            
            selectInput("tp_ativo", label = "Tipo de Ativo", choices = list("Ação"=2,"Fundos Imobiliários"=12, "Certificados de Investimentos"=14, "TUDO" = 0), selected = 0),
            sliderInput("limitador", "Limitador de Preço do Ativo:",
                        min = 0, max = 999999, value = c(10), step = 10, dragRange = TRUE),
            actionButton("button", "Consultar a Base"),
            actionButton("tabelao", "Consultar Codigos"),
            
            hr(),
            
            p("Consulta base apenas ações", style = "font-family: 'arial'; font-si10pt"),
            
            hr(),
            
            pickerInput("stck_select", "AÇÕES", unique(c(base_diaria1$CODNEG)), multiple = TRUE, selected = "Favor Selecionar para sua análise", list(`actions-box` = TRUE,
                        `live-search` = TRUE,
                        `deselect-all-text` = "Limpar a seleção",
                        `select-all-text` = "Seleção completa",
                        `none-selected-text` = "Favor Selecionar para sua análise"
            ), width = 250),
            
            dateRangeInput("dtstckselect", "Período de análise", start = Sys.Date()-1, end = Sys.Date()-1, format = "dd/mm/yy", separator = " / "),
            
            hr(),            
            
            #actionButton("stckdireto", "Consultar Base Ações"),
            
            hr(),
          
            p("Análise histórica e preditiva do Papel", style = "font-family: 'arial'; font-si10pt"),
            
            hr(),
            
            textInput("nomestck", label = h3("Nome da Ação"), value = "Inserir código do papel!"),
            
            selectInput("tp_modelo", label = "Tipo de Modelo Preditivo", choices = list("ARIMA"=1,"Geometric Brownian Motion"=2, "Histórico" = 3), selected = 3),
            
            
            
            dateRangeInput("dtstck", "Período de análise", start = Sys.Date() - 365, end = Sys.Date(), format = "dd/mm/yy", separator = " / "),
            
            actionButton("stock", "Consultar Ação")
            ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
            tabPanel("BASE B3",DT::dataTableOutput("Base_B3")),
            
            tabPanel("RANKING ALTAS",plotOutput("RANK")),
            
            tabPanel("RANKING BAIXAS",plotOutput("RANK1")),
            
            tabPanel("CONSULTA HISTÓRICO AÇÃO",plotlyOutput("stck")),
            
            tabPanel("CÓDIGOS B3",dataTableOutput("CODS"))
            )
                     )
            )
        )
    )
