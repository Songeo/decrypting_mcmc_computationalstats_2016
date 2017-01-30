
# librerías
sapply(c("tidyverse", "forcats", "stringr", "lubridate"), 
       require, character.only = T)
theme_set(theme_light())

trans.mat <- read_rds("matriz/matriz.poemas.rds")


letras <- c(letters[1:3], #"ch", 
            letters[4:which(letters == 'n')], "ñ", 
            letters[(which(letters == 'n')+1):length(letters)], 
              "á", "é", "í", "ó", "ú") %>% 
  toupper()
trans.prob.mat <- read_rds("matriz/matriz.poemas.rds")

decode <- function(mapping,coded) {
  coded=toupper(coded)
  decoded=coded
  for (i in 1:nchar(coded)) {
    if (substring(coded,i,i) %in% toupper(letras)) {
      substring(decoded,i,i)=toupper(letras[mapping==substring(coded,i,i)])
    }
  }
  decoded
}


log.prob <- function(mapping,decoded) {
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


# correctTxt = "No sabía de tristezas ni de lágrimas ni nada que me hicieran llorar yo sabía de cariño de ternura porque a mí desde pequeño eso me enseñó mamá eso me enseñó mamá eso y muchas cosas más yo jamás sufrí yo jamás lloré yo era muy feliz yo vivía yo vivía muy bien Yo vivía tan distinto algo hermoso algo divino lleno de felicidad yo sabía de alegrías la belleza de la vida pero no de soledad pero no de soledad"
correctTxt = "No sabía de tristezas ni de lágrimas ni nada que me hicieran llorar yo sabía de cariño de ternura porque a mí desde pequeño eso me enseñó mamá"
coded = decode(sample(toupper(letras)),correctTxt) # randomly scramble the text

mapping = sample(toupper(letras)) # initialize a random mapping
i=1
iters=40000
cur.decode=decode(mapping, coded)
cur.loglike=log.prob(mapping,cur.decode)
max.loglike=cur.loglike
max.decode=cur.decode

cur.decode.vec = rep(NA, 40000/2500) 

while (i<=iters) {
  proposal = sample(1:32,2) # select 2 letras to switch
  prop.mapping = mapping
  prop.mapping[proposal[1]] = mapping[proposal[2]]
  prop.mapping[proposal[2]] = mapping[proposal[1]]
  
  prop.decode = decode(prop.mapping,coded)
  prop.loglike = log.prob(prop.mapping,prop.decode)
  
  if (runif(1) < exp(prop.loglike-cur.loglike)) {
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
  
  if(i %in% seq(2500, iters, by = 2500)) {
    pos <- i/2500
    cur.decode.vec[pos] <- cur.decode
  }
}

replicate(10, sample(letras, 2))

# resultados <- cur.decode.vec
# saveRDS(resultados, "graphs/resultados.rds")