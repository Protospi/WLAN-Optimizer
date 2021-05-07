
# --------------------------------------------------------------------------

# Algoritimo em C++ 64000 circulos

# --------------------------------------------------------------------------

# Importa pacotes
library(tidyverse)
library(ggplot2)
library(Rcpp)

# Importa dados
wlan_completa <- read_csv("dados/clientes.csv",
                          col_names = c("x", "y", "Mbps"))

# Insere colunas de indice
wlan <- wlan_completa %>%
  mutate(indice = 1:nrow(wlan_completa))

# Declara base de circulos espaco de solucoes
centro_x <- rep(c(0,100,200,300,400,500,600,700,800), times = 9)  
centro_y <- rep(c(0,100,200,300,400,500,600,700,800), each = 9)

# --------------------------------------------------------------------------

# Implementacao de conta_pontos em cpp
sourceCpp('scripts/conta_pontos.cpp')

# --------------------------------------------------------------------------

# Declara vencedores
vencedores <- NULL

# Contador de pontos cobertos
contador <- 0

# Declara total megabites
Mbps <- NULL

# Vetor de Pa's que violam condicao de Mbps
viola <- NULL

# Marca tempo de inicio
inicio <- Sys.time()

# Laco para para varrear espaco
while(contador < 475){
  
  # Numero de Pontos cobertos
  cobertura <- NULL
  
  # Declara Inicio
  mais_populoso <- 1
  
  # Laco para determinar maior contagem
  for(i in 2:81){
    
    # Condicao para comparar circulos com maior quantidade de pontos
    if( (length(conta_pontos(x = centro_x[i],
                             y = centro_y[i],
                             wlanx = wlan$x,
                             wlany = wlan$y,
                             indice = wlan$indice)) >= 
         length(conta_pontos(x = centro_x[mais_populoso],
                             y = centro_y[mais_populoso],
                             wlanx = wlan$x,
                             wlany = wlan$y,
                             indice = wlan$indice)) ) &
        (!i %in% viola) ){
      
      # Atualiza circulo mais populoso
      mais_populoso <- i
      
      # Atualiza pontos cobertos por circulo mais populoso
      cobertura <- conta_pontos(x = centro_x[mais_populoso],
                                y = centro_y[mais_populoso],
                                wlanx = wlan$x,
                                wlany = wlan$y,
                                indice = wlan$indice)
      
    }
    
  }
  
  # Atualiza total de megas
  megas <- wlan %>% 
    filter(indice %in% as.integer(cobertura)) %>%
    summarise(total = sum(Mbps)) %>%
    pull(total)
  
  # Condicao verifica Mbps
  if(megas < 150){
    
    # Atualiza totais de Mbps
    Mbps <- c( Mbps, megas)
    
    # Atualiza vencedores
    vencedores <- c(vencedores, mais_populoso)
    
    # Remove pontos ja cobertos
    wlan <- wlan %>% filter(!indice %in% as.integer(cobertura) )
    
    # Atualiza contador
    contador <- contador + length(cobertura)
    
    # Verbaliza passos do laco
    print(contador)
    
  } else {
    
    # Insere indice de pa que viola condicoes
    viola <- c(viola, mais_populoso)
    
  }
  
}

# Marca tempo de inicio
fim <- Sys.time()

# Tempo gasto
fim - inicio

# Quantidade de Pontos de Acessos utilizados
length(vencedores)

# Imprime Mbps
Mbps

# --------------------------------------------------------------------------


# Gera data frame de resultados
resultados <- tibble(x = centro_x[vencedores],
                     y = centro_y[vencedores],
                     Mbps_total = Mbps)

# --------------------------------------------------------------------------

# Declara data frame de circulos
circulos_df <- tibble(grau = 1:360)

# Declara nome de colunas
colunas <- c("x", "y")

# Laco para construcao de variaveis 
for(i in vencedores){
  
  # Popula Circulos
  circulos_df <- circulos_df %>%
    mutate(x = centro_x[i] + 85 * cos(1:360),
           y = centro_y[i] + 85 * sin(1:360)) %>%
    rename_with(.fn = ~paste0(., as.character(i)),
                .cols = all_of(colunas))
}

# Grafico Base
p <- ggplot()+
  geom_point(aes(x = x, y = y, color = Mbps),
             data = wlan_completa,
             alpha = 0.8) 

# Popula grafico com circulos
for(i in vencedores){
  
  # Separa df
  df <- circulos_df[ , c( paste0("x", i), paste0("y", i) ) ] %>%
    rename_with(~paste0(c("x", "y")))
  
  # Insere circulos solucao otima
  p = p + geom_point(mapping = aes(x = x,
                                   y = y),
                     data = df,
                     color = "orange",
                     size = 2.5,
                     alpha = 0.05)
  
}

# Desenha centros
p <- p + geom_point(resultados,
                    mapping =  aes(x = x, y = y, fill = "red"),
                    color = "red",
                    size =  3,
                    alpha = 0.9) +
  ggtitle("Solução Ótima para os Pontos de Acesso",
          subtitle = "Solução de 17 Pa's no espaço discreto de 9 x 9")+
  theme(plot.title = element_text(size=12),
        plot.subtitle = element_text(size=10))+
  scale_fill_identity(name = "Pa's", guide = 'legend', labels = c('')) 


# --------------------------------------------------------------------------

# Declara data frame de circulos
circulos_df <- tibble(grau = 1:360)

# Laco para construcao de variaveis 
for(i in vencedores){
  
  # Popula Circulos
  circulos_df <- circulos_df %>%
    mutate(x = centro_x[i] + 60 * cos(1:360),
           y = centro_y[i] + 60 * sin(1:360)) %>%
    rename_with(.fn = ~paste0(., as.character(i)),
                .cols = all_of(colunas))
}

# Popula grafico com circulos
for(i in vencedores){
  
  # Separa df
  df <- circulos_df[ , c( paste0("x", i), paste0("y", i) ) ] %>%
    rename_with(~paste0(c("x", "y")))
  
  # Insere circulos solucao otima
  p = p + geom_point(mapping = aes(x = x,
                                   y = y),
                     data = df,
                     color = "orange",
                     size = 1.5,
                     alpha = 0.05)
  
}

# --------------------------------------------------------------------------

# Declara data frame de circulos
circulos_df <- tibble(grau = 1:360)

# Laco para construcao de variaveis 
for(i in vencedores){
  
  # Popula Circulos
  circulos_df <- circulos_df %>%
    mutate(x = centro_x[i] + 40 * cos(1:360),
           y = centro_y[i] + 40 * sin(1:360)) %>%
    rename_with(.fn = ~paste0(., as.character(i)),
                .cols = all_of(colunas))
}

# Popula grafico com circulos
for(i in vencedores){
  
  # Separa df
  df <- circulos_df[ , c( paste0("x", i), paste0("y", i) ) ] %>%
    rename_with(~paste0(c("x", "y")))
  
  # Insere circulos solucao otima
  p = p + geom_point(mapping = aes(x = x,
                                   y = y),
                     data = df,
                     color = "orange",
                     size = 1,
                     alpha = 0.05)
  
}


# --------------------------------------------------------------------------

# Declara data frame de circulos
circulos_df <- tibble(grau = 1:360)

# Laco para construcao de variaveis 
for(i in vencedores){
  
  # Popula Circulos
  circulos_df <- circulos_df %>%
    mutate(x = centro_x[i] + 20 * cos(1:360),
           y = centro_y[i] + 20 * sin(1:360)) %>%
    rename_with(.fn = ~paste0(., as.character(i)),
                .cols = all_of(colunas))
}

# Popula grafico com circulos
for(i in vencedores){
  
  # Separa df
  df <- circulos_df[ , c( paste0("x", i), paste0("y", i) ) ] %>%
    rename_with(~paste0(c("x", "y")))
  
  # Insere circulos solucao otima
  p = p + geom_point(mapping = aes(x = x,
                                   y = y),
                     data = df,
                     color = "orange",
                     size = 0.5,
                     alpha = 0.05)
  
}

# --------------------------------------------------------------------------

# Imprime grafico
p
