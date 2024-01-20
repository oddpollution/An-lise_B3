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
library(RMySQL)
library(plotly)

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
            p(h5("Exemplo de arquivo de cotações históricas baixado no site da B3"), style = "font-family: 'arial'; font-si10pt"),
            
            hr(),
            
            # fileInput("file1", "Inserir arquivo CSV tratado previamente",
            #           multiple = FALSE,
            #           accept = c("text/csv",
            #                      "text/comma-separated-values,text/plain",
            #                      ".csv")),
            downloadButton("exmpl", "Baixar arquivo exemplo da B3"),
            
            # hr(),
            # 
            # p(h5("Upload de arquivo de cotações históricas baixado no site da B3"), style = "font-family: 'arial'; font-si10pt"),
            # 
            # # UPLOAD DA BASE
            # checkboxInput("upload", "Upload da base"),
            # conditionalPanel(
            #   condition = "input.upload == true",
            #   fileInput("file1", "Inserir arquivo CSV tratado previamente",
            #             multiple = FALSE,
            #             accept = c("text/csv",
            #                        "text/comma-separated-values,text/plain",
            #                        ".csv"))
            # ),
            
            hr(),
            
            p(h5("Upload de arquivo ou conexão com servidor MySQL para obter arquivo de cotações históricas baixado no site da B3"), style = "font-family: 'arial'; font-si10pt"),

            # Conexão SQL
            selectInput("upload", 
                        label = "Opção para upload da base", 
                        choices = c("Upload do arquivo",
                                       "Conexão banco de dados MySQL"), 
                        selected = "Upload do arquivo"),
            conditionalPanel(
              condition = "input.upload == 'Upload do arquivo'",
              fileInput("file1", "Inserir arquivo CSV tratado previamente",
                        multiple = FALSE,
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv"))),
            conditionalPanel(
              condition = "input.upload == 'Conexão banco de dados MySQL'",
              textInput("db_user","Usuário do Banco de Dados", value = "root"),
              passwordInput("db_password","Senha do Banco de Dados", value = "********"),
              textInput("db_name","Nome do Banco de Dados", value = ""),
              textInput("db_table","Tabela do Banco de Dados", value = ""),
              textInput("db_host","IP do Banco de Dados", value = "127.0.0.1"),
              textInput("db_port","Port do Banco de Dados", value = "3306")
            ),
            
            hr(),
            
            p(h5("Opções de consulta"), style = "font-family: 'arial'; font-si10pt"),
            
            hr(),
            
            selectInput("tp_ativo", label = "Tipo de Ativo", choices = list("Ação"=2,"Fundos Imobiliários"=12, "Certificados de Investimentos"=14, "TUDO" = 0), selected = 0),
            numericInput("min", "Minimum", 0),
            numericInput("max", "Maximum", 100),
            sliderInput("limitador", "Limitador de Preço do Ativo:",
                        min = 0, max = 999999, value = c(10), step = 10, dragRange = TRUE),
            numericInput("selic", "SELIC:", 0.10, min = 0, max = 0.99,step = 0.001),
            dateRangeInput("dtstckselect1", "Período de análise", start = Sys.Date()-1, end = Sys.Date()-1, format = "dd/mm/yy", separator = " / "),
            
            hr(),
            
            actionButton("button", "Consultar a Base"),
            actionButton("tabelao", "Consultar Codigos"),
            
            hr(),
            
            p(h5("Seleção de possíveis ativos para portfolio"), style = "font-family: 'arial'; font-si10pt"),
            
            hr(),
            
            checkboxInput("conditional_Markowitz", label = "Habilitar análise de Portfólio"),
            
            conditionalPanel(
              condition = "input.conditional_Markowitz == 1",
            pickerInput(inputId = "stck_select",label = "Ações", "", multiple = TRUE, selected = "Favor Selecionar para sua análise", list(`actions-box` = TRUE,
                        `live-search` = TRUE,
                        `deselect-all-text` = "Limpar a seleção",
                        `select-all-text` = "Seleção completa",
                        `none-selected-text` = "Favor Selecionar para sua análise"
            ), width = 250),
            
            # dateRangeInput("dtstckselect", "Período de análise", start = Sys.Date()-1, end = Sys.Date()-1, format = "dd/mm/yy", separator = " / "),
            
            hr(),            
            
            actionButton("workmark", "Gerar Portfolio Eficiente")),
            
            hr(),
          
            p(h5("Análise histórica e preditiva do Papel"), style = "font-family: 'arial'; font-si10pt"),
            
            hr(),
            
            checkboxInput("conditional_Prediction", label = "Habilitar análise de Preditiva"),
            
            conditionalPanel(
              condition = "input.conditional_Prediction == 1",
            pickerInput(inputId = "nomestck", label = "Nome da Ação","", multiple = FALSE, selected = "Favor Selecionar para sua análise", list(`actions-box` = TRUE, `live-search` = TRUE, `deselect-all-text` = "Limpar a seleção", `none-selected-text` = "Favor Selecionar para sua análise"), width = 250),
            
            selectInput("tp_modelo", label = "Tipo de Modelo Preditivo", choices = list("ARIMA"=1,"Geometric Brownian Motion"=2, "Histórico" = 3), selected = 3),
            
            
            
            dateRangeInput("dtstck", "Período de análise", start = Sys.Date() - 365, end = Sys.Date(), format = "dd/mm/yy", separator = " / "),
            
            actionButton("stock", "Consultar Ação"))
            ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
            tabPanel("BASE B3",DT::dataTableOutput("Base_B3")),
            
            tabPanel("RANKING ALTAS",plotOutput("RANK")),
            
            tabPanel("RANKING BAIXAS",plotOutput("RANK1")),
            
            tabPanel("CONSULTA HISTÓRICO AÇÃO",plotlyOutput("stck")),
            
            tabPanel("CÓDIGOS B3",dataTableOutput("CODS")),

            # tabPanel("PORTFOLIO EFICIENTE",dataTableOutput("strategy"))
                        
            tabPanel("PORTFOLIO EFICIENTE",fluidRow(plotlyOutput("mark_line")),
                                           fluidRow(dataTableOutput("strategy")))
            )
                     )
            )
        )
    )
