rm(list = ls())

# librerías
sapply(c("tidyverse", "forcats", "stringr", "lubridate"), 
       require, character.only = T)
theme_set(theme_light())

# reference <- readLines("textos/octavio.txt")
# reference <- toupper(reference)

reference=readLines("textos/jane_austen.txt")
reference=toupper(reference)

trans.mat=matrix(0,27,27)
rownames(trans.mat)=colnames(trans.mat)=c(toupper(letters),"")
lastletter=""

for (ln in 1:length(reference)) {
  if (ln %% 1000 ==0) {cat("Line",ln,"\n")}
  
  for (pos in 1:nchar(reference[ln])) {
    curletter=substring(reference[ln],pos,pos)
    
    
    if (curletter %in% toupper(letters)) {
      trans.mat[rownames(trans.mat)==lastletter,
                colnames(trans.mat)==curletter]=
        trans.mat[rownames(trans.mat)==lastletter,
                  colnames(trans.mat)==curletter]+1
      lastletter=curletter
    } else {
      if (lastletter!="") {
        trans.mat[rownames(trans.mat)==lastletter,27]=
          trans.mat[rownames(trans.mat)==lastletter,27]+1
        lastletter=""
      }
    }
  }
  curletter=""
  if (lastletter != "") {
    trans.mat[rownames(trans.mat)==lastletter,27]=
      trans.mat[rownames(trans.mat)==lastletter,27]+1
  }
  lastletter=""
}

trans.prob.mat <- sweep(trans.mat+1,1,rowSums(trans.mat+1),FUN="/")
trans.prob.mat


tt <- trans.prob.mat %>% as.data.frame() %>% 
  mutate(inicial = rownames(.)) %>% 
  gather(var.lab, var.val, 1:27) %>% 
  mutate(var.lab = as.character(fct_recode(var.lab, ` ` = "V27")))
table(tt$var.lab)
table(tt$inicial)

ggplot(tt, aes(var.lab, fct_rev( factor(inicial)) )) + 
  geom_tile(aes(fill = var.val)) + 
  scale_fill_gradient(low="white",high="black",limits=c(0,1)) + 
  labs(x="Probability of Second Letter",y="Conditioning on First Letter",fill="Prob") +
  # scale_y_discrete(limits = rev(levels(melt(trans.prob.mat)$Var1))) +
  coord_equal()



decode <- function(mapping,coded) {
  coded=toupper(coded)
  decoded=coded
  for (i in 1:nchar(coded)) {
    if (substring(coded,i,i) %in% toupper(letters)) {
      substring(decoded,i,i)=toupper(letters[mapping==substring(coded,i,i)])
    }
  }
  decoded
}


log.prob <- function(mapping,decoded) {
  logprob=0
  
  lastletter=""
  for (i in 1:nchar(decoded)) {
    curletter=substring(decoded,i,i)
    if (curletter %in% toupper(letters)) {
      logprob=logprob+log(trans.prob.mat[rownames(trans.mat)==lastletter,
                                         colnames(trans.mat)==curletter])
      lastletter=curletter
    } else {
      if (lastletter!="") {
        logprob=logprob + log(trans.prob.mat[rownames(trans.mat)==lastletter,27])
        lastletter=""
      }
    }
  }
  
  if (lastletter!="") {
    logprob=logprob+log(trans.prob.mat[rownames(trans.mat)==lastletter,27])
    lastletter=""
  }
  logprob
}


correctTxt="ENTER HAMLET HAM TO BE OR NOT TO BE THAT IS THE QUESTION WHETHER TIS NOBLER IN THE MIND TO SUFFER THE SLINGS AND ARROWS OF OUTRAGEOUS FORTUNE OR TO TAKE ARMS AGAINST A SEA OF TROUBLES AND BY OPPOSING END"
coded = decode(sample(toupper(letters)),correctTxt) # randomly scramble the text

mapping = sample(toupper(letters)) # initialize a random mapping
i=1
iters=1000
cur.decode=decode(mapping, coded)
cur.loglike=log.prob(mapping,cur.decode)
max.loglike=cur.loglike
max.decode=cur.decode

while (i<=iters) {
  proposal = sample(1:26,2) # select 2 letters to switch
  prop.mapping = mapping
  prop.mapping[proposal[1]] = mapping[proposal[2]]
  prop.mapping[proposal[2]] = mapping[proposal[1]]
  
  prop.decode=decode(prop.mapping,coded)
  prop.loglike=log.prob(prop.mapping,prop.decode)
  
  if (runif(1) < exp(prop.loglike-cur.loglike)) {
    mapping=prop.mapping
    cur.decode=prop.decode
    cur.loglike=prop.loglike
    
    if (cur.loglike > max.loglike) {
      max.loglike=cur.loglike
      max.decode=cur.decode
    }
    
    # cat(i,cur.decode,"\n")
    print(cur.loglike)
    i=i+1
  }
}