# Georgios Gerasimos Leventopoulos csd4152
# Assignment 4

rm(list=ls())
data <- c()
seed <- 1293992
set.seed(seed)
len <- 1000
n <- 101
alphabet <- c("A", "C", "G", "T")
for(i in 1:n){
  nam <- paste(">seq", i, sep="")
  seq <- sample(alphabet, size=len, replace=TRUE)
  seq.str <- paste(seq, collapse="")
  data <- c(data, nam, seq.str)
}
data




getPPM <- function(k=10){
  ppm <- matrix(0, nrow=4, ncol=k)
  for(i in 1:k){
    occs <- sample(1:100, 4, replace=TRUE)
    ppm[,i] <- occs
  }
  row.names(ppm) <- alphabet
  pwm <- t(t(ppm)/colSums(ppm))
  pwm <- log2(pwm/0.25)
  return(pwm)
}

pwm <- getPPM(10)

write.table(data, file=paste("seqFile_", seed, ".FA", sep=""), col.names=F, row.names=F, quote=F)
write.table(pwm, file=paste("pwm_", seed, ".txt", sep=""), col.names=F, row.names=T, quote=F, sep="\t")

getScores <- function(v, pwm){
  v <- unlist(strsplit(v, split=""))
  scores <- c()
  for(i in 1:(length(v) - ncol(pwm) + 1)){
    s <- v[i:(i+ncol(pwm)-1)]
    score <- 0
    j <- 1
    for(si in s){
      score = score + pwm[si, j]
      j <- j+1
    }
    scores <- c(scores, score)
  }
  return(as.numeric(scores)    )
}


getStat <- function(data, pwm, FUN="max"){
  j <- 0
  allscores <- c()
  for(i in seq(from=2, length(data), by=2)){
    j <- j+1
    seq <- data[i]##unlist(strsplit(data[i], split=""))
    scores <- getScores(data[i], pwm)
    allscores <- c(allscores, max(scores))
  }
  return(allscores)
}

scores <- getStat(data, pwm)
id.sort <- order(scores, decreasing=F)
scores[id.sort]

min.id <- id.sort[1]
max.id <- id.sort[ length(id.sort) ]
max.id
min.id
id.sort[median(1:n)]
median(scores)
mean(scores)

