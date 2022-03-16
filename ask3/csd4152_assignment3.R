#-------------------------------------------------------------------------------------------
# Georgios Gerasimos Leventopoulos csd4152  Assignment 3  R Programming Language
#-------------------------------------------------------------------------------------------

library(seqLogo)
# library(ggseqlogo)  # uncomment if it doesn't work with seqLogo library

# ------------------------------- Askhsh 1 ------------------------------------------------
# Omoiomorfh katanomh  (Continuous uniform distribution)
a = runif(n=100, min=0, max=100)

# Kanonikh katanomh (Normal distribution)
b = rnorm(n=100, mean=50, sd=15)

# Dionymikh katanomh  (Binomial distribution)
c = rbinom(n=100, size=1000, prob=0.1)

# Poisson katanomh  (Poisson distribution)
d = rpois(n=100, lambda=10)

katanomes = c(a,b,c,d)
katanomes

# Information in matrices is stored by default in a column-wise fashion
m = matrix(data=katanomes, nrow=100, ncol=4)
colnames(m) = c("omoiomorfh", "kanoniki", "dionimiki", "poisson")
m

#1.1 mean (mesos oros)
m1 = mean(m[,"omoiomorfh"])
m1
m2 = mean(m[,"kanoniki"])
m2
m3 = mean(m[,"dionimiki"])
m3
m4 = mean(m[,"poisson"])
m4

#1.2 median (diamesos)
print(median(m[,"omoiomorfh"]))
print(median(m[,"kanoniki"]))
print(median(m[,"dionimiki"]))
print(median(m[,"poisson"]))

# 1.3 pososto timon pou yparxoun sto diasthma [mean -10, mean+10] , opou posostotimon=(athroisma/plhthos):

cat(sum((m1-10)<=m[,"omoiomorfh"] & m[,"omoiomorfh"]<=(m1+10)), "%\n")

cat(sum((m2-10)<=m[,"kanoniki"] & m[,"kanoniki"]<=(m2+10)), "%\n")

cat(sum((m3-10)<=m[,"dionimiki"] & m[,"dionimiki"]<=(m3+10)), "%\n")

cat(sum((m4-10)<=m[,"poisson"] & m[,"poisson"]<=(m4+10)), "%\n")

#1.4 tipikh apoklish (sd)
print(sd(m[,"omoiomorfh"]))
print(sd(m[,"kanoniki"]))
print(sd(m[,"dionimiki"]))
print(sd(m[,"poisson"]))

#1.5  quantile
quantile(m[,"omoiomorfh"])
quantile(m[,"kanoniki"])
quantile(m[,"dionimiki"])
quantile(m[,"poisson"])

# H diamesos antistoixei sto quantile q = 50%

# a' tropos
sum = 0
rows_sum = rep(0, 100) # arxikopoio to vector me 100 mhdenika values.
for (i in 1:100){   
  for (j in 1:4){
    sum = sum + m[i,j]
  }
  rows_sum[i] = sum  # apothikeyo to sum se kathe row
  sum = 0
}
rows_sum

# b' tropos
rows_sum1 = c()
for (i in 1:100)
  rows_sum1[i] = sum(m[i,])
rows_sum1


# Sxetika me tis parametrous otan arxikopoio tis katanomes kai tis times mean kai median pou ypologisa ana sthlh,
# kai symfona me dikes mou parathrhseis se syndiasmo wikipedia parathro oti yparxei kapoia sxesh.
# Sygkekrimena:

# Gia thn Omoiomorfh katanomh to mean kai to median einai isa me (max+min)/2
# https://en.wikipedia.org/wiki/Continuous_uniform_distribution

# Gia thn Kanonikh katanomh to mean kai to median einai isa me thn parametro mean
# https://en.wikipedia.org/wiki/Normal_distribution

# Gia thn Dionimiki katanomh to mean iso me tis parametrous (size*prob)
# eno to median einai iso me to upper_bound(size*prob) h' me to lower_bound(size*prob) 
# https://en.wikipedia.org/wiki/Binomial_distribution

# Gia thn Poisson katanomh to mean einai iso me thn parametro lamda 
# kai to median einai perirou iso me to lowwer bound (lamda+1/3 - 0.02/lamda)
# https://en.wikipedia.org/wiki/Poisson_distribution


# ------------------------------- Askhsh 2 ------------------------------------------------

# Omoiomorfh katanomh
a = runif(n=20, min=0, max=100)

# Kanonikh katanomh
b = rnorm(n=20, mean=50, sd=15)

# Dionymikh katanomh
c = rbinom(n=20, size=1000, prob=0.1)

# Poisson katanomh
d = rpois(n=20, lambda=10)

aa = sample(x=a, size=100, replace = TRUE)
bb = sample(x=b, size=100, replace = TRUE)
cc = sample(x=c, size=100, replace = TRUE)
dd = sample(x=d, size=100, replace = TRUE)

katanomes1 = c(aa,bb,cc,dd)

# Information in matrices is stored by default in a column-wise fashion
mm = matrix(data=katanomes1, nrow=100, ncol=4)
colnames(mm) = c("omoiomorfh", "kanoniki", "dionimiki", "poisson")
#mm

# mean (mesos oros)
print(mean(mm[,"omoiomorfh"]))
print(mean(mm[,"kanoniki"]))
print(mean(mm[,"dionimiki"]))
print(mean(mm[,"poisson"]))

# tipikh apoklish (sd)
print(sd(mm[,"omoiomorfh"]))
print(sd(mm[,"kanoniki"]))
print(sd(mm[,"dionimiki"]))
print(sd(mm[,"poisson"]))


# Den parathro poly megales diafores se sxesh me thn askhsh 1

# To mean meionetai ligo sthn omoiomorfh kai sthn kanonikh katanomh eno sthn dionymikh kai sthn poisson einai sxedon idio.

#    mean        Askhsh 1   Askhsh2
#  omoiomorfh   51.83592    46.10388
#  kanoniki     51.01287    42.80156
#  dionimiki    100.46      99.87
#  poissonn     10.06       10.99


# H tipiki apoklisi ayxanetai ligo sthn kanonikh katanomh eno meionetai ligo stis ypoloipes katanomes.

#     sd         Askhsh 1   Askhsh2
#  omoiomorfh   29.0984     27.411
#  kanoniki     14.48221    19.15914
#  dionimiki    10.10992    8.895187
#  poisson      3.377944    2.721018

# ------------------------------- Askhsh 3 ------------------------------------------------
# getwd()  # an den vriskei to path na balo to arxeio mesa sto working directory

a = read.table(file="C:/Users/leven/Desktop/R/ask3/pwm.txt", colClasses = "character") #diabasma arxeiou
#a
m1 = matrix("", nrow=20, ncol=11) # arxikopoihsh enos kenou matrix
m1

# spao enan enan tous xarakthres kai tous vazo sto matrix
for(i in 1:20) m1[i,] =  unlist(strsplit(a[i,], split=""))
m1

countMatrix = matrix(0, nrow=4, ncol=11)
rownames(countMatrix) = c("A", "C", "G", "T") # onomazo tis grammes
# metrao poses fores vlepo enan xarakthra kai ekmetaleyomai to gegonos oti h R exei onomata stis grammes ths
for(i in 1:20) for(j in 1:11) countMatrix[m1[i,j], j] = countMatrix[m1[i,j], j] + 1
countMatrix

#metatroph tou countaMatrix se matrix apo syxnothtes
freqmat = matrix(0, nrow=4, ncol=11)
cs = colSums(countMatrix)
freqmat = t(t(countMatrix)/cs)
freqmat

tempMatrix = freqmat/0.25+1e-6 # prostheto to 10^-6 gia na apofygo to -oo  (ama bgei log(0))
pwm = log2(tempMatrix)
#To PWM () pou antistoixei sto motivo auto einai to exis:
pwm

#To seqLogo pou antistoixei sto motivo auto einai to exis:
seqLogo(freqmat)


# H pio synthrhmenes sthles einai aytes pou allazoun gramma ligorero
# Oi theseis 4 kai 7 einai perissotero synthrhmenes giati exoyn perissotero "T" ,
# elaxisto "C" sthn 4h thesh kai elaxisto "G" sthn 7h thesh.

# Oi theseis 3 einai h ligotero synthrhmenh. Periexei oles tis vaseis dna se poli mirko vathmo thn kathe mia.
