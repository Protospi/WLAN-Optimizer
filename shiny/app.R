
# ---------------------------------------------------------------------------------------------

# Robo rastreador 15/02/2021

# ---------------------------------------------------------------------------------------------

# Carrega Pacotes
library(shiny)
library(tidyverse)
library(Rcpp)

# ---------------------------------------------------------------------------------------------

# Implementacao rastreador em cpp
sourceCpp('rastreador.cpp')

# ---------------------------------------------------------------------------------------------

# Define Interface do usuario 
ui <- fluidPage(
  
  # Define titulo do aplicativo
  titlePanel("Posicionador"),
  
  # Define Barra Lateral
  sidebarLayout(
    
    # Define painel da barra lateral
    sidebarPanel(
  
      # Seleciona Arquivo
      fileInput("arquivo", "Escolha um arquivo csv",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"),
                buttonLabel = "Procurar",
                placeholder = "Nenhum arquivo"),
      
      # Linha Horizontal
      tags$hr(),
      
      # Seleciona maximo de Mbps por Pa
      sliderInput("mbps",
                  "Máximo de Mbps por Pa",
                  min = 140, 
                  max = 160,
                  value = 150),
      
      # Linha Horizontal
      tags$hr(),
      
      uiOutput('uu'),
      
      # Linha Horizontal
      tags$hr(),
      
      # Seleciona minimo de Pd's cobertos
      sliderInput("discreto",
                  "Granularidade do Espaço Discreto",
                  min = 10, 
                  max = 150,
                  value = 110),
      
      # Linha Horizontal
      tags$hr(),
      
      # Botao de execucao do algoritmo
      actionButton("aciona", "Posicione", class = "btn-success"),
      
      # Linha Horizontal
      tags$hr(),
      
      # Tamanho do Painel lateral
      width = 2
      
      ),
      
      # Painel Principal
      mainPanel(
        
        # Aparencia fluida
        fluidRow(
          
          # Coluna
          column(8,
                 plotOutput("grafico")   
                 
          ),
          
          # Coluna
          column(4,
                 tableOutput("tabela") 
                 
          )
            
       )
             
    )
    
  )
    
)

# ---------------------------------------------------------------------------------------------

# Define servidor
server <- function(input, output) {
  
    # Declara objeto reativo
    dados <- reactive({
      
      # Declara caminho do arquivo
      inFile <- input$arquivo
      
      # Condicao para carregar arquivo
      if (is.null(inFile)){
        
        # Leitura do Arquivo
        wlan_completa <- read_csv("clientes.csv",
                                  col_names = c("x", "y", "Mbps"))
        
      } else {
        
        # Leitura do Arquivo
        wlan_completa <- read_csv(inFile$datapath,
                                  col_names = c("x", "y", "Mbps"),
                                  skip = 1,
                                  col_types = "ddd")
      }
      
      # Retorna dados
      return(wlan_completa)
      
    })  
  
    # Renderiza Slider input
    output$uu <- renderUI({
      
      # Declara wlan
      wlan_completa <- dados()
      
      # Seleciona minimo de Pd's cobertos
      sliderInput("pds",
                  "Mínimo de Pd's Cobertos",
                  min = round(80 * nrow(wlan_completa) / 100) + 1 , 
                  max = nrow(wlan_completa) + 1,
                  value = round(95 * nrow(wlan_completa) / 100) + 1,
                  step = 1)
        
    })
  
    # Declara objeto reativo
    modelo <- eventReactive(input$aciona, {
      
        # Declara wlan
        wlan_completa <- dados()
        
        # Declara granularidade
        gran <- input$discreto
        
        # Insere colunas de indice
        wlan <- wlan_completa %>%
                  mutate(indice = 1:nrow(wlan_completa))
        
        # Declara base de circulos espaco de solucoes
        centro_x <- rep(seq(1, max(wlan_completa$x) + 10, length.out = gran), each = gran)  
        centro_y <- rep(seq(1, max(wlan_completa$x) + 10, length.out = gran), times = gran) 
        
        # Declara vencedores
        vencedores <- NULL
        
        # Contador de pontos cobertos
        contador <- 0
        
        # Declara total megabites
        Mbps <- NULL
        
        # Vetor de Pa's que violam condicao de Mbps
        viola <- 0
        
        # Laco para para varrear espaco
        while(contador < input$pds){
          
          # Numero de Pontos cobertos
          cobertura <- NULL
          
          # Declara Inicio
          mais_populoso <- 1
          
          # Chama funcao
          rastreado <- rastreia(intervalo_x = gran,
                                intervalo_y = gran,
                                wlanx = wlan$x,
                                wlany = wlan$y,
                                centro_x = centro_x,
                                centro_y = centro_y,
                                indice = wlan$indice,
                                mais_populoso = mais_populoso,
                                viola = viola)
          
          # Atualiza total de megas
          megas <- wlan %>% 
            filter(indice %in% as.integer(rastreado[[1]])) %>%
            summarise(total = sum(Mbps)) %>%
            pull(total)
          
          # Condicao verifica Mbps
          if(megas < input$mbps){
            
            # Atualiza totais de Mbps
            Mbps <- c( Mbps, megas)
            
            # Atualiza vencedores
            vencedores <- c(vencedores, rastreado[[2]])
            
            # Remove pontos ja cobertos
            wlan <- wlan %>% filter(!indice %in% as.integer(rastreado[[1]]) )
            
            # Atualiza contador
            contador <- contador + length(rastreado[[1]])
            
          } else {
            
            # Insere indice de pa que viola condicoes
            viola <- c(viola, rastreado[[2]])
            
          }
          
        }
        
        # Gera data frame de resultados
        resultados <- list(tibble(x = centro_x[vencedores],
                                  y = centro_y[vencedores],
                                  Mbps_total = Mbps,
                                  vencedores = vencedores),
                           gran)
        
        # Retorno do objeto reativo
        return(resultados)
        
    })
  
    # Renderiza grafico
    graf <- eventReactive(input$aciona, {
      
        # Declara wlan
        wlan_completa <- dados()
        
        if(input$aciona == 0){
          
            # Grafico Base
            ggplot()+
              geom_point(aes(x = x, y = y, color = Mbps),
                         data = wlan_completa,
                         alpha = 0.8,
                         size = 3)+
              ggtitle("Pontos de Acesso")+
              theme(plot.title = element_text(size=27, face="bold", hjust = 0.5),
                    axis.text.x = element_text(face="bold", size=20),
                    axis.text.y = element_text(face="bold", size=20,
                                               margin = margin(t = 0, r = 0, b = 0, l = 7)),
                    text = element_text(face="bold", size=21),
                    axis.title.x = element_text(size=26, face="bold"),
                    axis.title.y = element_text(size=26, face="bold"))
          
        } else {
      
            # Declara base de circulos espaco de solucoes
            centro_x <- rep(seq(1, max(wlan_completa$x) + 10, length.out = modelo()[[2]]), each = modelo()[[2]])  
            centro_y <- rep(seq(1, max(wlan_completa$x) + 10, length.out = modelo()[[2]]), times = modelo()[[2]]) 
            
            # Declara data frame de circulos
            circulos_df <- tibble(grau = 1:360)
            
            # Declara nome de colunas
            colunas <- c("x", "y")
            
            # Grafico Base
            p <- ggplot()+
              geom_point(aes(x = x, y = y, color = Mbps),
                         data = wlan_completa,
                         alpha = 0.8, size = 3) 
            
            # Desenha Pa's vermelhos
            p <- p + geom_point(modelo()[[1]],
                                mapping =  aes(x = x, y = y, fill = "red"),
                                color = "red",
                                size =  4,
                                alpha = 0.9) +
              ggtitle("Mínimo Ótimo")+
              theme(plot.title = element_text(size=27, face="bold", hjust = 0.5),
                    axis.text.x = element_text(face="bold", size=20),
                    axis.text.y = element_text(face="bold", size=20,
                                               margin = margin(t = 0, r = 0, b = 0, l = 7)),
                    text = element_text(face="bold", size=21),
                    axis.title.x = element_text(size=26, face="bold"),
                    axis.title.y = element_text(size=26, face="bold"))+
              scale_fill_identity(name = "Pa's", guide = 'legend', labels = c('')) 
            
            # Laco para construcao de variaveis 
            for(i in modelo()[[1]]$vencedores){
              
              # Popula Circulos
              circulos_df <- circulos_df %>%
                mutate(x = centro_x[i] + 85 * cos(1:360),
                       y = centro_y[i] + 85 * sin(1:360)) %>%
                rename_with(.fn = ~paste0(., as.character(i)),
                            .cols = all_of(colunas))
            }
            
            # Popula grafico com circulos
            for(i in modelo()[[1]]$vencedores){
              
              # Separa df
              df <- circulos_df[ , c( paste0("x", i), paste0("y", i) ) ] %>%
                rename_with(~paste0(c("x", "y")))
              
              # Insere circulos solucao otima
              p = p + geom_point(mapping = aes(x = x,
                                               y = y),
                                 data = df,
                                 color = "orange",
                                 size = 2.5,
                                 alpha = 0.1)
              
            }
            
            # Declara data frame de circulos
            circulos_df <- tibble(grau = 1:360)
            
            # Laco para construcao de variaveis 
            for(i in modelo()[[1]]$vencedores){
              
              # Popula Circulos
              circulos_df <- circulos_df %>%
                mutate(x = centro_x[i] + 62.5 * cos(1:360),
                       y = centro_y[i] + 62.5 * sin(1:360)) %>%
                rename_with(.fn = ~paste0(., as.character(i)),
                            .cols = all_of(colunas))
            }
            
            # Popula grafico com circulos
            for(i in modelo()[[1]]$vencedores){
              
              # Separa df
              df <- circulos_df[ , c( paste0("x", i), paste0("y", i) ) ] %>%
                rename_with(~paste0(c("x", "y")))
              
              # Insere circulos solucao otima
              p = p + geom_point(mapping = aes(x = x,
                                               y = y),
                                 data = df,
                                 color = "orange",
                                 size = 1.7,
                                 alpha = 0.15)
            }
            
            # Declara data frame de circulos
            circulos_df <- tibble(grau = 1:360)
            
            # Laco para construcao de variaveis 
            for(i in modelo()[[1]]$vencedores){
              
              # Popula Circulos
              circulos_df <- circulos_df %>%
                mutate(x = centro_x[i] + 40 * cos(1:360),
                       y = centro_y[i] + 40 * sin(1:360)) %>%
                rename_with(.fn = ~paste0(., as.character(i)),
                            .cols = all_of(colunas))
            }
            
            # Popula grafico com circulos
            for(i in modelo()[[1]]$vencedores){
              
              # Separa df
              df <- circulos_df[ , c( paste0("x", i), paste0("y", i) ) ] %>%
                rename_with(~paste0(c("x", "y")))
              
              # Insere circulos solucao otima
              p = p + geom_point(mapping = aes(x = x,
                                               y = y),
                                 data = df,
                                 color = "orange",
                                 size = 1,
                                 alpha = 0.4)
              
            }
            
            # Declara data frame de circulos
            circulos_df <- tibble(grau = 1:360)
            
            # Laco para construcao de variaveis 
            for(i in modelo()[[1]]$vencedores){
              
              # Popula Circulos
              circulos_df <- circulos_df %>%
                mutate(x = centro_x[i] + 20 * cos(1:360),
                       y = centro_y[i] + 20 * sin(1:360)) %>%
                rename_with(.fn = ~paste0(., as.character(i)),
                            .cols = all_of(colunas))
            }
            
            # Popula grafico com circulos
            for(i in modelo()[[1]]$vencedores){
              
              # Separa df
              df <- circulos_df[ , c( paste0("x", i), paste0("y", i) ) ] %>%
                rename_with(~paste0(c("x", "y")))
              
              # Insere circulos solucao otima
              p = p + geom_point(mapping = aes(x = x,
                                               y = y),
                                 data = df,
                                 color = "orange",
                                 size = 0.7,
                                 alpha = 0.7)
            }
            
            # Imprime grafico
            p 
            
        }
    
   } )
    
   # Saida grafico
   output$grafico <- renderPlot({
     
     graf()
     
     }, height = 700, width = 800)
   
   # Declara evento reativo de tabela
   tab <- eventReactive(input$aciona,{
     
     # Declara tabela
     tabela <- modelo()[[1]] %>% 
       select(1:3) %>% 
       mutate(Pa = 1:nrow(modelo()[[1]])) %>% 
       relocate(Pa, .before = x) %>% 
       rename("Mpbs" = "Mbps_total")
     
     # Desenha tabela
     tabela
     
   })
    
   # Renderiza grafico
   output$tabela <- renderTable({
     
      # Desenha tabela
      tab()
     
   })   
}

# ---------------------------------------------------------------------------------------------

shinyApp(ui, server)

# ---------------------------------------------------------------------------------------------
