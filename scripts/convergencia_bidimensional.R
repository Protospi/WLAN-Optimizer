#--------------------------------------------------------------------------

# Algoritimo de Convergencia Bidimensional

#--------------------------------------------------------------------------

# Importa pacotes
library(tidyverse)
library(Rcpp)
library(gridExtra)

#--------------------------------------------------------------------------

# Declara data frame de graficos
p <- list()

# Declara contador
indice_grafico <- 1

# Laco para discretizar x
for(i in  c(8, 80, 150)){
  
  # Laco para y discretizar y
  for(j in   c(8, 80, 150)){
    
    # Declara grid de centro dos circulos
    centro_x <- rep(seq(0, 800, length.out = i ), each = j )  
    centro_y <- rep(seq(0, 800, length.out = j ), times = i )
    
    # Declara data frame de pontos de busca
    centros <- tibble(x = centro_x, y = centro_y) 
    
    # Calcula numero de pontos
    pontos <-  i * j
    
    # Calcula Indices
    ids <- paste0("(",i , ",", j, ")")
    
    # Popula df
    p[[ indice_grafico ]] <-  ggplot(centros,aes(x = x, y = y)) +
      geom_point(size = 0.01, alpha = 4 / sqrt(i + j), color = "red")+
      theme_bw() +
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            plot.title = element_text(face="bold", size=22),
            axis.text.x = element_text(face="bold", size=17),
            axis.text.y = element_text(face="bold", size=17,
                                       margin = margin(t = 0, r = 0, b = 0, l = 5)),
            text = element_text(face="bold", size=16),
            axis.title.x = element_text(size=18, face="bold"),
            axis.title.y = element_text(size=18, face="bold"))+
      ggtitle(bquote(M[.(ids)]))
    
    # Atualiza indices graficos
    indice_grafico <- indice_grafico + 1
    
  }
  
}

# Arranja grid
do.call(grid.arrange, c(p))

#--------------------------------------------------------------------------

# Importa dados
wlan_completa <- read_csv("dados/clientes.csv",
                          col_names = c("x", "y", "consumo"))

# Insere colunas de indice
wlan_id <- wlan_completa %>%
  mutate(indice = 1:nrow(wlan_completa))

# Implementacao de conta_pontos em cpp
sourceCpp('scripts/rastreador.cpp')

# Declara data frame de performance
performance <- tibble(intervalos_x = rep(seq(8, 150, by = 1),
                                         each = 143 ),
                      intervalos_y = rep(seq(8, 150, by = 1 ),
                                         times = 143 ),
                      PA = rep(0,20449),
                      tempo = rep(0,20449))

# Declara contador
indice_perfor <- 1

# Laco para intervalo x
for(intervalo_x in seq(8, 150, by = 1)){
  
  # Laco para verificar convergencia
  for(intervalo_y in seq(8, 150, by = 1)){
    
    # Declara banco de dados
    wlan <- wlan_id
    
    # Declara grid de centro dos circulos
    centro_x <- rep(seq(0, 800, length.out = intervalo_x), each = intervalo_y)  
    centro_y <- rep(seq(0, 800, length.out = intervalo_y), times = intervalo_x)
    
    # Declara vencedores
    vencedores <- NULL
    
    # Contador de pontos cobertos
    contador <- 0
    
    # Declara total megabites
    Mbps <- NULL
    
    # Vetor de Pa's que violam condicao de Mbps
    viola <- numeric(1)
    
    # Inicia contador de tempo
    inicio <- Sys.time()
    
    # Laco para para varrear espaco
    while(contador < 475){
      
      # Numero de Pontos cobertos
      cobertura <- NULL
      
      # Declara Inicio
      mais_populoso <- 1
      
      # Chama funcao
      rastreado <- rastreia(intervalo_x = intervalo_x,
                            intervalo_y = intervalo_y,
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
        summarise(total = sum(consumo)) %>%
        pull(total)
      
      # Condicao verifica Mbps
      if(megas < 150){
        
        # Atualiza totais de Mbps
        Mbps <- c( Mbps, megas)
        
        # Atualiza vencedores
        vencedores <- c(vencedores, rastreado[[2]])
        
        # Remove pontos ja cobertos
        wlan <- wlan %>% filter(!indice %in% as.integer(rastreado[[1]]) )
        
        # Atualiza contador
        contador <- contador + length(rastreado[[1]])
        
        # Verbaliza passos da funcao
        print(paste0("Indice: ", intervalo_x, " - ",
                     intervalo_y, " - ",
                     "Contador: ", contador))
        
      } else {
        
        # Insere indice de pa que viola condicoes
        viola <- c(viola, rastreado[[2]])
        
      }
      
    }
    
    # Finaliza contador de tempo
    fim <- Sys.time()
    
    # Atualiza tempo
    performance$tempo[indice_perfor] <- fim - inicio
    
    # popula Quantidade de Pontos de Acessos utilizados
    performance$PA[indice_perfor] <- length(vencedores)
    
    # Incrementa contador
    indice_perfor <- indice_perfor + 1
    
  }  
  
}

# ------------------------------------------------------------------------

# Load RColorBrewer
library(RColorBrewer)

# Gera 6 cores
coul <- brewer.pal(8, "RdBu") 

# Grafico de superficie de Pas
ggplot(performance, aes(intervalos_x, intervalos_y, z = PA)) +
  geom_contour_filled(binwidth = 1) +
  scale_fill_manual(values = coul)+
  ggtitle("Mínimos Locais e Globais")+
  theme(plot.title = element_text(size=27, face="bold", hjust = 0.5),
        axis.text.x = element_text(face="bold", size=20),
        axis.text.y = element_text(face="bold", size=20),
        text = element_text(face="bold", size=21),
        axis.title.x = element_text(size=26, face="bold"),
        axis.title.y = element_text(size=26, face="bold")) +
  xlab(expression(M["( i )"])) +
  ylab(expression(M["( j )"])) +
  guides(fill=guide_legend(title="Pa's")) +
  coord_cartesian(xlim = c(8, 150), ylim = c(8, 150), expand = FALSE)+
  scale_y_continuous(breaks = c(8,seq(15,150, 15)))+
  scale_x_continuous(breaks = c(8,seq(15,150, 15)))

# ------------------------------------------------------------------------

# Gera 6 cores
coul <- brewer.pal(5, "RdBu") 

# Adiciona cores
coul <- colorRampPalette(coul)(9)

# Grafico de Superficie de Tempo
ggplot(performance, aes(intervalos_x, intervalos_y, z = tempo))+
  geom_contour_filled()+
  scale_fill_manual(values = coul)+
  ggtitle("Custo Computacional")+
  theme(plot.title = element_text(size=27, face="bold", hjust = 0.5),
        axis.text.x = element_text(face="bold", size=20),
        axis.text.y = element_text(face="bold", size=20),
        text = element_text(face="bold", size=21),
        axis.title.x = element_text(size=26, face="bold"),
        axis.title.y = element_text(size=26, face="bold")) +
  xlab(expression(M["( i )"])) +
  ylab(expression(M["( j )"])) +
  guides(fill=guide_legend(title="Tempo(s)")) +
  coord_cartesian(xlim = c(8, 150), ylim = c(8, 150), expand = FALSE)+
  scale_y_continuous(breaks = c(8,seq(15,150, 15)))+
  scale_x_continuous(breaks = c(8,seq(15,150, 15)))

# ------------------------------------------------------------------------


