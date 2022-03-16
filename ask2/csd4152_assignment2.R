# Georgios Gerasimos Leventopoulos csd4152
# Assignment 2

#1
x = sample(x=1:10, size=1000, replace=TRUE)
x
#1.a
mean(x) #mesh timh

#1.b
median(x) #diamesos

#1.c
# h mesh timi (mean) einai to (athroisma apo ena synolo aritmon) / (to plhthos twn arithmwn)

# h diamesos (median) parexei ena xrhsimo metro tou kentrou apo ena synolo dedomenon
# Einai o mesaios arithmos se mia taxinomhmenh lista apo arithmous (taxinomhmenh se ayxousa h fthinousa seira)

# Sygkrinontas to median me to mean mporoume na paroume mia idea gia thn katanomh enos synolou dedomenon.

#1.d
table(x)

# Mou fainetai logiko to apotelesma afou to athroisma tou plithous ton arithmon apo 1 eos 10 einai 1000
# Epishs parathro oti to plhthos emfanhshs kathe arithnou einai konta sto 5

#1.e
a = rep(0, 1000) # Arxikopoihsh vector 1000 theseon me mhdenika
n = 10
for(i in 1:n){
  x = sample(1:10, size=1000, replace=TRUE) #xilies tixaies times apo ena eos 10
  a = a + x # element-wise praxeis
}

sum(a>=0 & a<=10)
sum(a>10 & a<=20)
sum(a>20 & a<=30)
sum(a>30 & a<=40)
sum(a>40 & a<=50)
sum(a>50 & a<=60)
sum(a>60 & a<=70)
sum(a>70 & a<=80)
sum(a>80 & a<=90)
sum(a>90 & a<=100)

# Yparxoun pio polles times metaxi 50-60 se sxesh me 0-10 gia ton idio logo me to erothma d.
# Afou h entolh sample(1:10, size=1000, replace=TRUE) dinei times konta sto 5 kai epeidh ayto tha ginei 10 fores
# (10*5 = 50), einai logiko na exoume times metaxi 50 kai 60.


#2 #enas arithmos metalaxeon - dionymiki katanomh
x = rbinom(n=1, size=10000, prob=0.001)
x


#3  arithmos metalaxeon se 100 tmhmata dna apo 10000 baseis to kathe ena tmhma
x = rbinom(n=100, size=10000, prob=0.001)
x
#3.i
sum(x==0)

#3.ii
sum(x>=5)

#3.iii
# a tropos
mean(x) 
# b tropos
sum(x)/100

#3.iv  Poses fores exoume max metalaxeis? px an max metalaxeis=19, poses forew exoume dei to 19?
temp = max(x)
sum(x == temp)

#3.v  
max(x)

#4
findMax = function(n, a, b){
  k = runif(n, min=a, max=b) #omoiomorfh katanomh
  max = -1000000 #arxikopoio to max me mia poli mikri timi
  for (i in 1:100){
    if(k[i] > max){
      max = k[i]
    }
  }
  return (max)
}

maxValue = findMax(100, 40, 60) # vazo tyxaies times n=100,a=40,b=60
print(maxValue)

#4.a
maxu = rep(0, 100)
for (i in 1:100){
  maxVal = findMax(100, 40, 60)
  maxu[i] = maxVal
}
print(maxu)


findMaxNumbers = function(n, a, b){
  maxu = rep(0,100)
  for(i in 1:100){
    k = runif(n, min=a, max=b) 
    maxu[i] = max(k)
  }
  return (maxu)
}

m = findMaxNumbers(20,30,100)
print(m)