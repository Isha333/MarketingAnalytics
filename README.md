# MarketingAnalytics
Digital Marketing Analytics explained with R projects 

#market baseket analysis 

libraries we need for this 
```{r}
library(arules)
library(arulesViz)
library(RColorBrewer)
library(visNetwork)
library(igraph)
```
'arules' has a specific data structure called Transactions

load the data set into RMD  -----
```{r}
Basket=readxl::read_xlsx("Cafe Coffee Night.xlsx",col_names = TRUE)
```
Analyse the data types and values 
```{r}
head(Basket)
str(Basket)
```
convert character to factor 
```{r}
column=c("Bill Number","Item Desc","Category")
Basket[column]= lapply(Basket[column], as.factor)
```

aggregating the basket with split function who will take first parameter and aggregate according to second 
lets take the items in the basket and group them according to invoice number 
how many baskets had same items

```{r}
Basket.Aggregate=split(Basket$`Item Desc`, Basket$`Bill Number`)
head(Basket.Aggregate)
```

we need to understand buying habits , increase a basket has only one item repeatedly then its not helping us 
so we need to remove such duplicate aggregations 

```{r}
Basket.Aggr2=list()
for(i in 1:length(Basket.Aggregate)){
  Basket.Aggr2[[i]]=unique(Basket.Aggregate[[i]])
}
head(Basket.Aggr2)
```
transaction is a special data structure defined in arules package. converting the basket list to transaction. 
we use apriori algorithm for market basket analysis. Transactions can use apriori algorithim  

```{r}
Transactions=as(Basket.Aggr2,"transactions")
summary(Transactions)
```
Plot the item frequency plot absolute frequency 
```{r}
itemFrequencyPlot(Transactions,type="absolute",col="lightblue", topN=20, horiz = TRUE, cex.names=0.6,
                  main="Absolute item frequency")
```
Plot the item frequency plot relative frequency 
```{r}
itemFrequencyPlot(Transactions,type="relative",col="lightblue", topN=20, horiz = TRUE, cex.names=0.6,
                  main="Relative item frequency")
```
now if a rule has high support confidence and lift it is a good association.
if it has low support and high lift it is not a good rule 

find good lift
```{r}
Rules.set=apriori(data=Transactions, parameter = list(support = 0.001, 
                                                   confidence = 0.15,
                                                   minlen = 2))

Rules.set <- Rules.set[!is.redundant(Rules.set)]
inspect(sort(Rules.set, by="lift"))
```
find good confidence 
```{r}
inspect(sort(Rules.set,by='confidence'))
```
find good support
```{r}
inspect(sort(Rules.set,by='support'))
```
library(arulesViz)
library(RColorBrewer)
we are using this library to plot the colour as a gradient rather than a single colour
the plot function now works on the basis of the arulesViz package

```{r}
plot(Rules.set,control=list(col=brewer.pal(11,"Spectral")))
```
```{r}
subrules=head(sort(Rules.set,by="lift"),10)
plot(subrules,method="graph")
```
here we are converting arules2 in to a data frame
Rule: {A}=>{B}
Probability(A)-LHS Support
gives us the probability of A on the LHS
```{r}
rules_df=as(Rules.set,"data.frame")
rules_df$LHSSupport=rules_df$support/rules_df$confidence
```
Probability(B)-RHS Support
gives us the probability of B on the RHS
```{r}
rules_df$RHSSupport=rules_df$confidence/rules_df$lift
print(rules_df)
```
here we can finally use the print command rather than inspect as it has been converted in to a data frame

interactve network graph
```{r}
ig <- plot( Rules.set, method="graph", control=list(type="items") )

ig_df <- toVisNetworkData(ig, idToLabel = FALSE)

visNetwork(ig_df$nodes, ig_df$edges) %>%
visNodes(size = 10) %>%
visLegend() %>%
visEdges(smooth = FALSE) %>%
visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
visInteraction(navigationButtons = TRUE) %>%
visEdges(arrows = 'from') %>%
visPhysics(
solver = "barnesHut",
maxVelocity = 35,
forceAtlas2Based = list(gravitationalConstant = -6000))
```



