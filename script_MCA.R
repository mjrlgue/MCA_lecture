
library(FactoMineR)

############################################
############### Hobbiers ####################
############################################
data("hobbies")

# Transform the TV variable as factor
hobbies[,"TV"] = as.factor(hobbies[,"TV"])

# MCA with the graphs given by default
res.mca <- MCA(hobbies,quali.sup=19:22,quanti.sup=23)

summary(res.mca)

# Graph of the eigenvalues
barplot(res.mca$eig[,2],main="Eigenvalues", names.arg=1:nrow(res.mca$eig))
lines(x=c(0,1000),y=c(1/18,1/18)*100) ## J=18. Those components/axis/synthethic variables that are below the line cannot be interpreted

# Graphs of the individuals
plot(res.mca,invisible=c("var","quali.sup"),cex=.5,label="none",title="Graph of the individuals") 
plot(res.mca,invisible=c("var","quali.sup"),cex=.5,label="none",title="Graph of the individuals", habillage="Gardening") 

# Graphs of the categories
plot(res.mca,invis=c("ind","quali.sup"),col.var=c(rep(c("black","red"),17),"black",rep("red",4)),title="Graph of the active categories")
plot(res.mca,invisible=c("ind","var"),hab="quali", palette=palette(c("blue","maroon","darkgreen","black","red")), title="Graph of the supplementary categories")

# Graphs of the variables
plot(res.mca,choix="var",title="Graph of the variables")
plot(res.mca,choix="quanti.sup",title="Graph of the continuous variables")

# Confidence ellipses around the categories for the first 4 variables
plotellipses(res.mca, cex=0.2, magnify=12, keepvar=1:4)

dimdesc(res.mca)

names(res.mca)

################################################
#### Genetically Modified Organisms ######
##############################################

library(FactoMineR)
gmo <- read.table("http://factominer.free.fr/book/gmo.csv",header=TRUE,sep=";")
summary(gmo[,1:16])
### only one observation for Very favourable in Position Al.H. Not enough data to draw conclusions
### basically the same happens with Position.Culture

## put them as Favourable
levels(gmo$Position.Al.H)[4] <- levels(gmo$Position.Al.H)[1] 
levels(gmo$Position.Culture) <- c("Favourable","Somewhat Against","Totally opposed","Favourable")
summary(gmo[,1:16])

summary(gmo[,17:21])
res <- MCA(gmo, ncp=5, quali.sup=17:21, graph = FALSE)
res
res <- MCA(gmo, ncp=5, quali.sup=17:21, graph=FALSE, level.ventil=0.05)
plot.MCA(res,invisible=c("var","quali.sup"),label=FALSE)
plot.MCA(res,invisible=c("ind","quali.sup"),label=FALSE)
plot.MCA(res,invisible=c("ind","quali.sup"))
plot.MCA(res, col.quali.sup="brown", invisible=c("quanti.sup","ind","var"))

###########################################
######## tea #####################
####################################

tea <- read.table("http://factominer.free.fr/book/tea.csv",header=TRUE,sep=";")
summary(tea)
res.mca<-MCA(tea, quanti.sup=22, quali.sup=c(19:21,23:36))


###########################################
######## Perfume #####################
####################################
perfume <- read.table("http://factominer.free.fr/book/perfume.csv",header=TRUE,sep=";",row.names=1)
res.perfume <- MCA(perfume)
plot.MCA(res.perfume, invisible="var", col.ind="black")
plot.MCA(res.perfume, invisible="ind", col.var="black")

