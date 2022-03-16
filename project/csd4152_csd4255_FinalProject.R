# R Programming Language Project
# Georgios Gerasimos Leventopoulos csd4152
# Nikolaos Kontonasios csd4255

library(abc)
library(bayestestR)
library(dplyr)
library(ggplot2)

#----------------------------- Helper Functions -----------------------------#
fact <- function(n) {
  if(n <= 1)
    return (1)
  return (n * fact(n-1))
}
denominator <- function(n){
  return (fact(n)/(fact(2)*(fact(n-2))))
}
# Ypologizei to k kai to epistrefei
findK <- function(count, n){
  return (count/denominator(n))
}
# Ypologizei to w kai to epistrefei
findW <- function(S, n){
  a = 0
  for(i in 1:(n-1))
    a = a + 1/i
  return (S/a)
}

#----------------------------- Question 1 -----------------------------#
observation = readLines("http://139.91.162.101/teaching/hy390_2021/ms_obs_final.out")
# observation = readLines("ms_obs_final.out") # UNCOMMENT IF NEEDED
simstats = readLines("http://139.91.162.101/teaching/hy390_2021/ms_sim_final.out")
# simstats = readLines("ms_sim_final.out") # UNCOMMENT IF NEEDED

observation = strsplit(observation,"")
nrow = length(observation)
count = 0
obs = matrix(0, nrow = 1, ncol = 2, byrow = TRUE)
# Tsekaroume oles tis grammes me oles tis grammes
for(i in 1:nrow){
  for(j in (i+1): nrow){
    if(j > nrow) break
    for(r in 1:length(observation[[i]])){
      if(observation[[i]][r] != observation[[j]][r])
        count = count+1
    }
  }
}
# O pinakas obs periexei sthn proth sthlh to w kai sthn deyterh sthlh to k gia to arxeio "ms_obs_final.out"
obs[1,1] = findW(length(observation[[1]]),nrow)
obs[1,2] = findK(count,nrow)
simstats = strsplit(simstats, "")
n = length(simstats)
count = 0
end = nrow
pos = 1
sims = matrix(0,nrow = 10000, ncol = 2,byrow = TRUE)

# O sims pinakas periexei sthn 1h sthlh ta 10000 "W" kai sthn 2h sthlh ta 10000 "K", gia to arxeio "ms_sim_final.out"
# kanoume to idio pragma opos ston obs pinaka alla ayth thn fora gia ola ta datasets
for(i in 1:n){
  print(i)
  rowSize = length(simstats[[i]])
  if(rowSize == 0) {
    sims[pos,1] = findW(length(simstats[[i-1]]), nrow)
    sims[pos,2] = findK(count, nrow)
    pos = pos+1
    count = 0
    end = i + nrow
    next
  }
  for(j in (i+1):end){
    if(j > end) break
    for(k in 1:rowSize){
      if(simstats[[i]][k] != simstats[[j]][k])
        count = count+1
    }
  }
}

pars = readLines("http://139.91.162.101/teaching/hy390_2021/pars_final.txt")
# pars = readLines("pars_final.txt")  # UNCOMMENT IF NEEDED
options(digits=15)
pars = as.double(pars)

# UNCOMMENT IF NEEDED
#library(abc)
#library(bayestestR)
#library(dplyr)
#library(ggplot2)


# Ypologizo ton rithnmo ayxhshs mesw tou paketou abc opos zhteitai sthn ekfwnhsh
myabc = abc(target=obs, param=pars, sumstat=sims, tol=0.1, method="loclinear", hcorr=TRUE)
summary(myabc)

#----------------------------- Question 2 -----------------------------#
# Xrhsimopoio to adj.vaues gia na valo os dedomena sthn ci()  tis times apo thn posterior katanomh meta thn diorthosh loclinear
ci.hdi <- ci(myabc$adj.values, method = "HDI")
ci.hdi

#----------------------------- Question 3 -----------------------------#
mean_value = mean(myabc$adj.values)

# Xrhsimopoio thn synarthsh density gia na vro tis piknothtes ton 3on katanaomon apo ta deigamta
prior = density(pars)       # h prior katanomh ths ekthetikhs ayxhshs
before = density(myabc$unadj.values[,1]) # posterior prin thn diorthosh
after = density( myabc$adj.values[,1])   # posterior meta thn diorthosh


plot(prior$x, prior$y, ylim=c(0, max(prior$y, before$y, after$y )), type='l', col='black')  # prior katanomh ths ekthetikhs ayxhshs me mayro xrwma
abline(v=mean_value, col="green")                   # oi times tou mean gia thn parametro ths ekthetikhs auxhshs me prasino xroma (katheth grammh)
abline(v=ci.hdi$CI_low, col = "red")                # aristerh timh apo to credible interval me kokkino xroma (katheth grammh)
abline(v=ci.hdi$CI_high, col = "red")               # dixia timh apo to credible interval me kokkino xroma (katheth grammh)
points(after$x, after$y,  type='l', col='cyan')     # posterior prin thn diorthosh me cyan xroma
points(before$x, before$y,  type='l', col='yellow') # posterior meta thn diorthosh me kitrino xroma

#----------------------------- THE END -----------------------------#