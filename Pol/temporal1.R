#################################################################
### SESSION  MEMDA - UPC/UB
###
### ASSOCIATION RULES 
### 
#################################################################
### References:
###
### https://rpubs.com/joaquin_ar/397172
#################################################################
# INSTALLING AND LOADING THE ARULES PACKAGE

#install.packages("arules")
#install.packages("arulesViz")
#install.packages("tidyverse")
library(arules)
library(arulesViz)
library(FactoMineR)
library(tidyverse)

# THE TIC 2002 SURVEY
# READING THE DATA
tic_tt <- readRDS("data/songs_outlied.rds")
dim(tic_tt)
str(tic_tt)
summary(tic_tt)
# THE FIRST COLUMN CONTAINS THE ROW IDENTIFIERS

row.names(tic_tt)

# LETS SEE THE DATAFRAME 

names(tic_tt)
head(tic_tt)
summary(tic_tt)

#################################################################
### NOW LETS ENRICH THE RULES WITH THE SOCIODEMOGRAPHIC        
### INFORMATION
##############################################################

# LETS TRANSFORM OUR INPUT DATA tic_tt AS A TRANSACTIONS FILE
# ALL VARS MUST BE CATEGORICAL OR LOGICAL
# NOTE: BETTER UNDERSTANDING WHEN HAVING ALL CATEGORICAL, I.E.,
# TRANSFORM ALSO YOUR LOGICAL VARS INTO FACTORS WITH 2 LEVELS

str(tic_tt)
for (j in 1:ncol(tic_tt)) {if(class(tic_tt[,j])=="logical") tic_tt[,j]<- as.factor(tic_tt[,j])}
summary(tic_tt)
str(tic_tt)

### TRANSFORMATION OF THE DB INTO A TRANSACTIONAL DB
ttr2 <- as(tic_tt,"transactions")

# HOW MANY TRANSACTIONS AND ITEMS?
ttr2

# SUMMARY OF ttr2
summary(ttr2)
class(ttr2)

# FUNCTION inspect
inspect(ttr2[1:6])
transactionInfo(ttr2[1:10])
SIZE <- size(ttr2)
summary(SIZE)
quantile(SIZE, probs = seq(0,1,0.1))
data.frame(SIZE) %>%
    ggplot(aes(x = SIZE)) +
    geom_histogram() +
    labs(title = "Distribución del tamaño de las transacciones",
         x = "Tamaño") +
    theme_bw()
# LOOKING THE SUPPORT OF ITEMS
itemFrequency(ttr2)
itemFrequencyPlot(ttr2, topN=5, cex.names=0.5)
itemFrequencyPlot(ttr2,cex.names=0.4)
itemFrequencyPlot(ttr2, support=0.01, cex.names = 0.5)
itemFrequencyPlot(ttr2,type="absolute", cex.names = 0.3)
itemFrequencyPlot(ttr2, topN=100,type="absolute", cex.names = 0.6)
frequency_items <- itemFrequency(x = ttr2, type = "relative")
frequency_items %>% sort(decreasing = TRUE) %>% head(5)

# NOW, LETS FIND THE ASSOCIATION RULES

itemsets <- apriori(data = ttr2,
                    parameter = list(support = 0.05,
                                     minlen = 1,
                                     maxlen = 5,
                                     target = "frequent itemset"))
summary(itemsets)
inspect(itemsets[1:5])
top_20_itemsets <- sort(itemsets, by = "support", decreasing = TRUE)[1:20]
(inspect(top_20_itemsets))
inspect(sort(itemsets[size(itemsets) > 1], decreasing = TRUE)[1:20])
itemsets_filtrado <- arules::subset(itemsets,
                                    subset = items %in% "Sexe=Dona")
inspect(itemsets_filtrado[1:10])

itemsets_filtrado <- arules::subset(itemsets,
                                    subset = items %ain% c("Sexe=Dona", "Nivell.d.estudis=Secundaris"))
inspect(itemsets_filtrado[1:10])

######
rules = apriori (ttr2, parameter = list (support=0.01, confidence=0.4, maxlen = 5, minlen=2))
rules
rules = apriori (ttr2, parameter = list (support=0.01, confidence=0.8, maxlen = 5, minlen=2))
rules = apriori (ttr2, parameter = list (support=0.05, confidence=0.8, maxlen = 5, minlen=2))
rules = apriori (ttr2, parameter = list (support=0.35, confidence=0.9, maxlen = 5, minlen=2))
rules

# WHAT DO WE HAVE?
summary(rules)
inspect(sort(x = rules, decreasing = TRUE, by = "confidence")[1:3])

# LETS INSPECT THE RULES
inspect(rules[1:2])
inspect(rules[4:6])

filtrado_reglas <- subset(x = rules,
                          subset = lhs %ain% c("Sexe=Dona", "Nivell.d.estudis=Secundaris"))
inspect(filtrado_reglas)
filtrado_reglas
filtrado_reglas <- subset(x = rules,
                          subset = lhs %in% c("Sexe=Dona", "Nivell.d.estudis=Secundaris"))
inspect(filtrado_reglas)
filtrado_reglas
#Searching for redundant rules
reglas_redundantes <- rules[is.redundant(x = rules, measure = "confidence")]
reglas_redundantes

reglas_Noredund <- rules[!is.redundant(x = rules, measure = "confidence")]
reglas_Noredund

reglas_Noredund <- sort(reglas_Noredund,by="lift")
inspect(reglas_Noredund[1:2])
write(reglas_Noredund, file = "rules.csv", sep = ",", col.names = NA)

# MINING THE RULES BY CONFIDENCE

myrules1 = sort(rules, by = "confidence")
length(myrules1)
inspect(myrules1[1:3])

# MINING THE RULES BY LIFT

myrules2 = sort(rules, by = "lift")
inspect(myrules2[1:5])


# LETS INSPECT THE RULES HAVING RHS Tr?mits.per.Internet.amb.l.Adm?.

rules_tramits_admo <- subset(rules, subset = rhs %in% "Tr.mits.per.Internet.amb.l.Adm..=TRUE")
rules_tramits_admo

rules_tramits_admo <- subset(rules, subset = rhs %in% "Tr.mits.per.Internet.amb.l.Adm..=FALSE")
rules_tramits_admo

#Ha.comprat.per.Internet.=FALSE
rules_ha_comprat_Internet <- subset(rules, subset = rhs %in% "Ha.comprat.per.Internet.=FALSE")
rules_ha_comprat_Internet
sorted_rules_comprat_Internet <- sort(rules_ha_comprat_Internet, by="lift")
inspect(sorted_rules_comprat_Internet[1:5])


###########################################################
# PRACTICE OF MBA USING APRIORI
# DATA GROCERIES
###########################################################

# LOADING THE DATA (IT GOES WITH THE ARULES PACKAGE

data(Groceries) 

Groceries

# LETS SEE THE FIRST TRANSACTIONS OF GROCERIES

as(Groceries[1:6],"data.frame")

# LETS DO A SUMMARY OF ALL TRANSACTIONS

summary(Groceries)

# LETS SEE THE 169 ITEMS

itemInfo(Groceries)

# LETS INSPECT THE LONGER TRANSACTIONS


nt <- which(size(Groceries) > 200)
inspect(Groceries[size(Groceries)>20,])

# LETS SEE THE SUPPORT OF SINGLE ITEMS

itemFrequency(Groceries)        

# LET VISUALISE THE TOP ITEMS

itemFrequencyPlot(Groceries, topN=40,type="absolute", cex.names = 0.7)

#######################################################
# FINDING THE RULES WITH APRIORI
#######################################################

rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.6, maxlen=3))
rules
summary(rules)
options(digits=2)
inspect(rules[1:6])

# MINING THE RULES
# INSPECT RULES BY LIFT

myrules = sort(rules, by = "lift")
inspect(myrules[1:10])

# TARGETTING THE RULES

lhs_chocolate <- subset(rules, subset = lhs %in% "specialty chocolate")
inspect(sort(lhs_chocolate,by="lift"))

####EXTRA##########
#ECLAT

eclatDTrans<-eclat(ttr2)
eclatDTrans<-eclat(ttr2, parameter = list(support=0.4, minlen=2, maxlen=10))
inspect(eclatDTrans)

## Select a subset of rules using partial matching on the items 
## in the right-hand-side and a quality measure


## Mine frequent itemsets with Eclat.
eclatDTrans <- eclat(ttr2, parameter = list(supp = 0.5))

## Display the 5 itemsets with the highest support.
orderedItemsets <- sort(eclatDTrans)
inspect(orderedItemsets)

top5 <- sort(eclatDTrans)[1:5]
inspect(top5)

## Get the itemsets as a list
as(items(top5), "list")

## Get the itemsets as a binary matrix
as(items(top5), "matrix")

## Get the itemsets as a sparse matrix, a ngCMatrix from package Matrix.
## Warning: for efficiency reasons, the ngCMatrix you get is transposed 
as(items(top5), "ngCMatrix")

###Visualizing Results
inspect(rules)
library(arulesViz)
plot(rules, measure = c("support", "lift"), shading = "confidence")
#order == number of items inside the rules
plot(rules, method = "two-key plot")
plot(rules, method = "grouped")