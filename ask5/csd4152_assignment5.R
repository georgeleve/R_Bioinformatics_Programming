rm(list=ls())

## consider the following dataset *attachement*
## It comprises two groups of individuals. The first 20 are males and the next 20 are females.
## Use the lm function to find the differential expressed genes.
## which one has the minimum p-value (if the first answer 1, if the second, answer 2 etc)

## 2. what is the difference between the means (absolute value) between the males and females for the gene with the minimum p-value

## 3a. what is the corrected p.value after the bonferroni correction
## Does this gene (with the minimum p-value) reject the null hypothesis if the bonferroni test is applied at a significance level of 0.05

## 4. consider the dataset 2 (data2).
## It consists of males and females as the first one, but half of them are young (in the odd columns)
## and the other half are old (in the even columns).
## question 4a: do you think you should consider a second factor (besides the gender) to model the gene expression?
## question 4b: assuming that there is no interaction between the age and the gender, build a linear model and find the genes with
## differential gene expression at the significance threshold 0.05 after a bonferroni correction. How many genes do you detect?

## 4c Which genes has the smallest p-value?
## 4d If you would do the analysis without considering the second grouping (the age grouping) how many genes would you detect as being differentially expressed?
##

## 4e what is the effect of the age an the expression value?
## 4f what is the effect of the gender on the epression value?

## Gene Ontology analysis

nrow <- 10000
ncol <- 20
k <- 10

set.seed(1000392)

getData <- function(k, nrow=1, ncol=10, offset=3){
  xs = lapply(1:nrow, function(x){
    #mean=runif(1, k-offset, k+offset);
    mean=-1
    while(mean < 0){
      mean=rnorm(1, k, offset)
    }
    x <- -1
    while(sum(x<0) > 0){
      x = rnorm(ncol, mean,1)
    }
    return(list(m=mean, x = x))
  })
  
  return(xs)
}


getd <- getData(k=k, nrow=nrow, ncol=ncol, offset=1)
xs = t(sapply(getd, function(x){x$x}))
ms  <- sapply(getd, function(x){x$m})

a <- matrix(rnorm(nrow*ncol, k,1), nrow=nrow, ncol=ncol)
b  <- xs
data <- cbind(a,b)
write.table(data, file="quiz2_data1.csv", col.names=F, row.names=F, quote=F)

dim(data)

grp1 <- factor(rep(c(0,1), each=ncol))

getdf <- function(v, a){
  ml = lm(v~a)
  pvalue = summary(ml)$coefficients[2,4]
  return(pvalue)
}

pvalues = apply(data, 1, getdf, a=grp1)
##plot(abs(ms-k), log(pvalues))

## a 1
geneWithMinimumPvalue = which.min(pvalues)
geneWithMinimumPvalue

min(pvalues)

## answer 2
difMean = mean(data[geneWithMinimumPvalue, 1:ncol]) - mean(data[geneWithMinimumPvalue, (ncol+1):(ncol(data))])
abs(difMean)


## answer 3
min.pvalue = min(pvalues)
min.pvalue

cor.pvalues.fdr = p.adjust ( pvalues, method="BH")
min(cor.pvalues.fdr)

## it rejects the null
cor.pvalues = p.adjust ( pvalues, method="bonferroni")
length(which(cor.pvalues < 0.05))
min(cor.pvalues)
cor.pvalues[geneWithMinimumPvalue]


ageEffect  <- matrix( rnorm(ncol*nrow, 5, 2), ncol=ncol, nrow=nrow)

data2 <- data
data2[,seq(from=2, to=2*ncol, by=2)] <- data2[,seq(from=2, to=2*ncol, by=2)] + ageEffect
write.table(data2, file="quiz2_data2.csv", col.names=F, row.names=F, quote=F)


grp2 <- factor(rep(c(0,1), ncol))

getdf2 <- function(v, a, b){
  ml = lm(v~a+b)
  pvalue = summary(ml)$coefficients[2,4]
  return(pvalue)
}

lm(data2[878,]~grp1+grp2)

lm(data2[1666,]~grp1+grp2)

pvalues2 <- apply(data2, 1, getdf2, grp1, grp2)
##plot(ms, log(pvalues2))


pvalues2.notgrp2 <- apply(data2, 1, getdf, grp1)
##plot(ms, log(pvalues2.notgrp2))

##plot(log(pvalues2), log(pvalues2.notgrp2))
##abline(a=0, b=1)

fdr.adjust.2 = p.adjust(pvalues2, method="fdr")
which.min(fdr.adjust.2)

fdr.adjust.2.nogrp = p.adjust(pvalues2.notgrp2, method="fdr")
which.min(fdr.adjust.2.nogrp)

fdr.adjust.2.nogrp

## answer 4a
best2 <- which.min(fdr.adjust.2)
best2
min.pvalue2 = min(fdr.adjust.2)
min.pvalue2

## number of genes with small p-values
sum(fdr.adjust.2 < 0.05)

## number of genes with small p-values without  the grp2
sum(fdr.adjust.2.nogrp < 0.05)


min(fdr.adjust.2.nogrp)

## common genes
getd3 <- getData(k=k, nrow=nrow, ncol=ncol, offset=1)
xs3 = t(sapply(getd3, function(x){x$x}))
ms3  <- sapply(getd3, function(x){x$m})
a3 <- matrix(rnorm(nrow*ncol, k,1), nrow=nrow, ncol=ncol)
b3  <- xs3
data3 <- cbind(a3,b3)

write.table(data3, file="quiz2_data3.csv", quote=FALSE, col.names=FALSE, row.names=FALSE)

dim(data3)
pvalues3 <- apply(data3, 1, getdf, grp1)
pvalues3.fdr  <- p.adjust(pvalues3, method="fdr")
sig.genes.3 = which(pvalues3.fdr < 0.05)
pvalues1.fdr = p.adjust(pvalues, method="fdr")
sig.genes.1 = which(pvalues1.fdr < 0.05)

common = length(intersect(sig.genes.1, sig.genes.3))
common
c1 = length(sig.genes.1)
c2 = length(sig.genes.3)

common.pvalue = phyper(q=common-1, m=c2, n=nrow(a3)-c2, k=c1, lower.tail=FALSE)
common.pvalue