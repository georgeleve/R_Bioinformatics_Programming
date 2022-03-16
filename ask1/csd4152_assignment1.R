# Georgios Gerasimos Leventopoulos csd4152
cat("------------------------------------------------------------------------------------------------------------------")
#1
a = 1:10*2
a
#2
length(a)
#3
sum(a > 5 & a < 15)
cat((sum(a >= 5 & a <= 15)/length(a)) * 100, "%\n")
#4
#4.a
k = c(2^0, 2^1, 2^3, 2^4, 2^5, 2^6, 2^7, 2^8, 2^9, 2^10)
k
#4.b  Thelo olous tous arithmous 1/v gia 1<=v<=5
b = c(1/1, 1/2, 1/3, 1/4, 1/5)
b
#4.c (Xrhsimopoio thn synarthsh log pou einai me vash to e)
log(b)
#5
u = runif(100, 0, 10)
u
mean(u) # mesh timh tou u
sd(u)   # typiki apoklhsh (standard deviation)
var(u)  # diakimansh (variance)
#6
sum(u < 4)
sum(u >= 4 & u <= 8)
sum(u > 8)
#7
# Nai, ta apotelesmata mou fainontai logika afou an ta athroisoume mas dinoun 1 (dhadh 100%)
# h pithanothta o arithmos na einai < 4 einai peripou 35%
# h pithanothta o arithmos na einai < 4 kai >8 einai peripou 35%
# h pithanothta o arithmos na einai  >8 einai peripou 30%
# An ta athrisoume dhladh mas bgazoyn 100% pithanothta
cat((sum(u < 4)/length(u))*100, "%\n") 
cat((sum(u >= 4 & u <= 8)/length(u))*100, "%\n") 
cat((sum(u > 8)/length(u))*100, "%\n")
#8
k = rpois(1000, 5) #1000 tyxaies times apo poisson katanomh
#8.a
sum(k == 0)
#8.b
sum(k >= 12)/length(k)
#8.c
# Sthn sygkekrimenh taxi o kathe mathitis exei dexthei enan arithmo klhseon pou proerxontai apo mia poisson katanomh.
# opou gia to sygkekrimeno sxoleio o arithmos ton klhseon gia tous perissoterous mathites einai konta sto 5.
# Ara oi mathites dexontai kata meso oro 5 klhseis hmerisios 
# To pio pithano einai oti o mathiths A einai apo allo sxoleio, opou sto allo sxoleio o arithmos ton kleiseon exei katanomh konta sto 12.
# Mporei o mathitis A na einai mathitis apo allo sxoleio alla den mporoume na to poume me sigouria (omos einai poli pithano).
cat("------------------------------------------------------------------------------------------------------------------")