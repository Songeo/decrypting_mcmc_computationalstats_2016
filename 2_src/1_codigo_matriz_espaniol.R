rm(list = ls())


# Librerías
sapply(c("tidyverse", "forcats", "stringr", "lubridate"), 
       require, character.only = T)
theme_set(theme_light())
letras <- c(letters[1:3], #"ch", 
            letters[4:which(letters == 'n')], "ñ", 
            letters[(which(letters == 'n')+1):length(letters)], 
            "á", "é", "í", "ó", "ú") %>% 
  toupper()

# Función de matriz de transición
Matriz_Trans <- function(textoin){
  # textoin <- textooctavio
  
  # 0. Letras
  letras <- c(letters[1:3], #"ch", 
              letters[4:which(letters == 'n')], "ñ", 
              letters[(which(letters == 'n')+1):length(letters)], 
              "á", "é", "í", "ó", "ú") %>% 
    toupper()
  
  # 1. Matriz de transición
  n <- length(letras) + 1
  trans.mat <- matrix(0, n, n)
  rownames(trans.mat) <- c(toupper(letras), "")
  colnames(trans.mat) <- c(toupper(letras), "")
  ultimaletra <- ""
  
  for (linea in 1:length(textoin)) {
    if (linea %% 1000 ==0) {cat("Line", linea,"\n")}
    
    for (pos in 1:nchar(textoin[linea])) {
      curletter <- substring( textoin[linea], pos, pos)
      
      if (T %in% c(curletter == letras)) {
        trans.mat[rownames(trans.mat) == ultimaletra,
                  colnames(trans.mat) == curletter] <- 
          trans.mat[rownames(trans.mat) == ultimaletra,
                    colnames(trans.mat) == curletter] + 1
        ultimaletra <- curletter
      } else {
        if (ultimaletra != "") {
          trans.mat[rownames(trans.mat) == ultimaletra, n] <- 
            trans.mat[rownames(trans.mat) == ultimaletra, n] + 1
          ultimaletra=""
        }
      }
    }
    
    curletter=""
    if (ultimaletra != "") {
      trans.mat[rownames(trans.mat) == ultimaletra, n] <- 
        trans.mat[rownames(trans.mat) == ultimaletra, n]+1
    }
    ultimaletra <- ""
    
  }
  trans.prob.mat <- sweep(trans.mat + 1, # matriz por los ceros
        1, 
        rowSums(trans.mat + 1), 
        FUN="/")
  
}

texto.octavio <- readLines("textos/octavio.txt") %>% toupper
texto.luisjuan <- readLines("textos/luisjuan.txt") %>% toupper
texto.miserab <- readLines("textos/Les-Miserables.txt") %>% toupper
# En la terminal se cambia
# iconv -f WINDOWS-1252 -t UTF-8 cancionescafe.txt > cancionescafeutf8.txt 
texto.cafe <- readLines("textos/cancionescafeutf8.txt") %>% toupper

mat.poemas <- Matriz_Trans(textoin = texto.octavio)
mat.canciones <- Matriz_Trans(textoin = texto.luisjuan)
mat.cafetacuba <- Matriz_Trans(textoin = texto.cafe)
mat.miserables <- Matriz_Trans(textoin = texto.miserab)

# saveRDS(mat.cafetacuba, "matriz/matriz.cafetacuba.rds")
# saveRDS(mat.canciones, "matriz/matriz.canciones.rds")
# saveRDS(mat.poemas, "matriz/matriz.poemas.rds")
# saveRDS(mat.miserables, "matriz/matriz.miserables.rds")

mat.poemas <- readRDS("matriz/matriz.poemas.rds")
mat.canciones <- readRDS("matriz/matriz.canciones.rds")
mat.cafetacuba <- readRDS("matriz/matriz.cafetacuba.rds")
mat.miserables <- readRDS("matriz/matriz.miserables.rds")

tt <- mat.cafetacuba %>% 
  data.frame(check.names = F) %>% 
  mutate(tipo = 'canciones de café tacuba',
         inicial = rownames(.)) %>%
  rbind(
    mat.canciones %>% 
      data.frame(check.names = F) %>% 
      mutate(tipo = 'canciones luis miguel y juan gabriel',
             inicial = rownames(.)) 
  ) %>%
  rbind(
    mat.poemas %>% 
      data.frame(check.names = F) %>% 
      mutate(tipo = 'poemas en español',
             inicial = rownames(.)) 
  ) %>%
  rbind(
    mat.miserables %>% 
      data.frame(check.names = F) %>% 
      mutate(tipo = 'los miserables',
             inicial = rownames(.)) 
  ) %>% 
  gather(var.lab, var.val, -tipo, -inicial) 
table(tt$var.lab)
table(tt$inicial)

tt$var.lab <- factor(tt$var.lab, 
                     levels = c(letras, "V33"), 
                     labels = c(letras, ""))
tt$inicial <- factor(tt$inicial, levels =  c(letras, ""))

(gg <- ggplot(tt, aes(var.lab, ( factor(inicial)) )) + 
  geom_tile(aes(fill = var.val)) + 
  scale_fill_gradient(low="white", 
                      # high="#3a7580",
                      high="#063d51",
                      limits=c(0,1)) + 
  ylab("Condicionado a Primera Letra") + 
  xlab("Probabilidad de Segunda Letra") + 
  theme(axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 6.5), 
        strip.text = element_text(size  = 11)
        ) +
  labs(fill = "Prob.") +
  facet_wrap(~tipo, scales = 'free'))
ggsave(plot =gg, filename = "graphs/matrices.png", width = 8, height = 7)