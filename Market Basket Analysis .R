#########################################
####### DATASET VenditeOnLine.csv #######
#########################################


# caricare i pacchetti arules e arulesViz per le analisi di market basket
library(arules) # per l'implementazione delle regole associative
library(arulesViz) # per la visualizzazione grafica delle regole associative

VenditeOnLine<-file.choose()
VenditeOnLine<-read.csv2(VenditeOnLine,header=TRUE,sep=";") 

dim(VenditeOnLine) # dimensioni del dataset
colnames(VenditeOnLine) # individuazione del nome delle variabili

# trasformazione del dataset VenditeOnLine in datasset transazionale in
funzione delle variabili Items e CustomerID
VenditeOnLine<-
  as(split(VenditeOnLine[,"CustomerID"],VenditeOnLine[,"Items"]),"transactions"
  )

summary(VenditeOnLine) # descrizione dei dati
inspect(head(VenditeOnLine,7)) # visualizzazione delle prime 7 transazioni

# distribuzione di frequenza dei prodotti
VenditeOnLine_freq<-itemFrequency(VenditeOnLine) 

# distribuzione di frequenza ordinata rispetto alle frequenze assolute
VenditeOnLine_freq<-sort(VenditeOnLine_freq,decreasing=TRUE) 

# grafico a barre della frequenza dei 7 prodotti più frequentemente acquistati
barplot(head(VenditeOnLine_freq,7),ylim=c(0,0.5),col="light
blue",main="Distribuzione di frequenza dei 7 prodotti più frequentemente
acquistati") 

# identificazione delle regole associative con algoritmo Apriori caratterizzato
# da livello minimo di support pari al 3,5% delle transazioni e da livello
# minimo di confidence pari al 90%
VenditeOnLine.rules<-
  apriori(VenditeOnLine,parameter=list(supp=0.035,conf=0.9,target="rules")) 

summary(VenditeOnLine.rules) # sintesi dei dati

# scatterplot delle regole associative che permette la selezione arbitraria 
# di regole associative
plot(VenditeOnLine.rules,engine="interactive") 

# scatter plot delle regole associative individuate secondo i parametri 
# di interesse
plot(VenditeOnLine.rules) 

# scatter plot delle regole associative individuate secondo i parametri 
# di interesse e riportante l'ordine di grandezza delle regole
plot(VenditeOnLine.rules,method="two-key plot") 

# visualizzazione delle regole con grafico a palloncini
plot(VenditeOnLine.rules,method="grouped") 

# estrazione delle 6 regole associative con maggior confidence
VenditeOnLine_confidence<-head(sort(VenditeOnLine.rules,by="confidence"),6) 

# grafico a frecce delle 6 regole associative con maggiore confidence
plot(VenditeOnLine_confidence,method="paracoord",shading="confidence")