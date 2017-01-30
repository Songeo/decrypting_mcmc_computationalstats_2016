rm(list = ls())

# Funciones C++
Rcpp::sourceCpp("1_funciones.cpp")


# Librerías
sapply(c("tidyverse", "forcats", "stringr", "lubridate"), 
       require, character.only = T)
theme_set(theme_light())
letras <- c(letters[1:3], #"ch", 
            letters[4:which(letters == 'n')], "ñ", 
            letters[(which(letters == 'n')+1):length(letters)], 
            "á", "é", "í", "ó", "ú") %>% 
  toupper()


# Texto
correctTxt <- "No sabía de tristezas ni de lágrimas ni nada que me hicieran llorar yo sabía de cariño de ternura porque a mí desde pequeño eso me enseñó mamá" %>% toupper()

# Codificación
original <- str_split_fixed(correctTxt, "", n = str_length(correctTxt)) %>% 
  as.vector() %>% 
  factor(levels = c(letras, " ")) %>% as.numeric()
length(cadenacod)

set.seed(161210)
coded <- Decode( abcd = sample(1:32, size = 32), cadenacod = cadenacod)

paste(factor(coded, levels = 1:33, labels = c(letras, " ")), collapse = "")
paste(factor(original, levels = 1:33, labels = c(letras, " ")), collapse = "")



# Log probability
trans.mat <- read_rds("matriz/matriz.poemas.rds")
dim(trans.mat)

set.seed(161210)
LogProb(cadenacod = coded, mattrans = trans.mat)
# sum(log(LogProb( abcd = samp.prb, cadenacod = cadenacod, mattrans = trans.mat)))

# MCMC desencriptacion
samp.prb <- sample(1:32, size = 32)
codigodecripi <- Decripi(abcd = samp.prb, cadenacod = auxdecode,
                         mattrans = trans.mat, niters = 10000)
paste(factor(codigodecripi, levels = 1:33, labels = c(letras, " ")), collapse = "")
paste(factor(auxdecode, levels = 1:33, labels = c(letras, " ")), collapse = "")

replicate(100, ProP(1:33))


proposal = sample(1:32,2) # select 2 letters to switch

prop.mapping = samp.prb
prop.mapping[proposal[1]] = samp.prb[proposal[2]]
prop.mapping[proposal[2]] = samp.prb[proposal[1]]
auxdecode <- Decode( abcd = prop.mapping, cadenacod = cadenacod)
LogProb( cadenacod = auxdecode, mattrans = trans.mat)



log.prob <- function(decoded, trans.mat) {
  logprob=0
  
  lastletter=""
  for (i in 1:nchar(decoded)) {
    curletter=substring(decoded,i,i)
    if (curletter %in% toupper(letras)) {
      logprob=logprob+log(trans.prob.mat[rownames(trans.mat)==lastletter,
                                         colnames(trans.mat)==curletter])
      lastletter=curletter
    } else {
      if (lastletter!="") {
        logprob=logprob + log(trans.prob.mat[rownames(trans.mat)==lastletter,33])
        lastletter=""
      }
    }
  }
  
  if (lastletter!="") {
    logprob=logprob+log(trans.prob.mat[rownames(trans.mat)==lastletter,33])
    lastletter=""
  }
  logprob
}




# iteraciones
iters=7500
mapping <- samp.prb
cur.decode = Decode(mapping, coded)
cur.loglike= LogProb(cur.decode, mat.poemas)
max.loglike=cur.loglike
max.decode=cur.decode
trans.mat = mat.poemas
cur.decode.vec = rep(NA, 10) 

while (i<=iters) {
  proposal = sample(1:32,2) # select 2 letras to switch
  prop.mapping = mapping
  prop.mapping[proposal[1]] = mapping[proposal[2]]
  prop.mapping[proposal[2]] = mapping[proposal[1]]
  
  prop.decode = Decode(prop.mapping,coded)
  prop.loglike = LogProb(prop.decode, trans.mat)

  if (runif(1) < exp(prop.loglike - cur.loglike)) {
    mapping=prop.mapping
    cur.decode=prop.decode
    cur.loglike=prop.loglike
    
    if (cur.loglike > max.loglike) {
      max.loglike=cur.loglike
      max.decode=cur.decode
    }
    
    print(cur.loglike)
    # cat(i,cur.decode,"\n")
    i=i+1
  }
  
  if(i %in% seq(2500, 7500, by = 2500)) {
    pos <- i/2500
    cur.decode.vec[pos] <- cur.decode
  }
}
