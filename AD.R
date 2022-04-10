install.packages("FactoMineR")
install.packages("factoextra")
install.packages("readxl")
install.packages("ggplot2")
install.packages("stringr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggrepel")

library(ggrepel)
library(stringr)
library(stringr)
library(FactoMineR)
library(factoextra)
library(readxl)
library(dplyr)
library(ggplot2)
library(corrplot)

data1 <- read_xlsx("C:\\Users\\azza\\Desktop\\analyse des donnees\\AD.xlsx")
df <- data.frame(data1) 
colnames(df)[2]<-"sexe"
colnames(df)[3]<-"age"
colnames(df)[4]<-"csp"
colnames(df)[7]<-"serie"
colnames(df)[6]<-"genre"

#Stat des
#sexe
sexe <- df$sexe
freq1=table((sexe))
pct1 <- round(freq1/sum(freq1)*100)
lbls1 <- c("Homme","femme")
lbls1 <- paste(lbls1, pct1) 
lbls1 <- paste(lbls1,"%",sep="") 
pie(freq1,labels = lbls1, col=rainbow(length(lbls1)),main="Répartition selon le sexe")

#age
lbls2 <- c("[15,20[","[21,25[","[26,30[","[31,35[","36 ou plus")
df1 <- data.frame(df$age) %>% group_by(df$age) %>% count(df$age)
barplot(df1$n,names.arg=lbls2,col="#69b3a2",
        main="Nombre de personne âr classe d'âge",
        xlab="Classe d'âge",
        ylab="nombre de personnes")

#Série la plus regardée 
serie <- df$serie
freq3=table(serie)/100
pct3 <- round(freq3/sum(freq3)*100)
lbls3 <- names(freq3)
lbls3 <- paste(lbls3, pct3) 
lbls3 <- paste(lbls3,"%",sep="")
data2 <- as.data.frame(pct3)
ggplot(data2, aes(x = "", y = Freq , fill = serie)) +
  geom_col() +
  geom_label_repel(data = data2,
                   aes(label = lbls3),
                   size = 2.5, nudge_x = 1, show.legend = FALSE) +
  coord_polar(theta = "y")+ # pour avoir le pie chart
  guides(fill = guide_legend(title = "Les séries:"))+ 
  # scale_fill_brewer(palette = "Orange")+
  ylab("")+xlab("")

#csp
csp<- df$csp
freq4<-table(csp)/100
pct4 <- round(freq4/sum(freq1)*100)
lbls4 <- names(freq4)
lbls4 <- paste(lbls4, pct4) 
lbls4 <- paste(lbls4,"%",sep="")
pie(freq4,labels = lbls4, col=rainbow(length(lbls4))
    ,main="Répartition selon la catégorie socio-professionnelle")
#PCA 
#1st PCA

first_CPA_df <- df[,23:37]
name <- function(ch) {
  stri_replace_all_regex(ch,"Sur.une.échelle.de.1.à.5...évaluez.chacune.de.ces.séries...","")
}
colnames(first_CPA_df)<- name(colnames(first_CPA_df))
res.pca1 <- PCA (first_CPA_df,graph=FALSE)

#combien d'axe a conserver
eig.val1 <- get_eigenvalue(res.pca1)#taux d'inertie cumulé et kayser
eig.val1
fviz_eig(res.pca1, addlabels = TRUE, ylim = c(0, 50)) #Coude

#Graphique des variables:
var1 <- get_pca_var(res.pca1)
var1
#visualiser les variables : cercle de correlation
head(var1$coord)
fviz_pca_var(res.pca1, col.var = "black",axes = 1:2) 
fviz_pca_var(res.pca1, col.var = "black",axes = 2:3) 
fviz_pca_var(res.pca1, col.var = "black",axes = 3:4) 
#fviz_pca_var(res.pca1, col.var = "black",axes = 2:3) 
#fviz_pca_var(res.pca1, col.var = "black",axes = 2:4) 
#fviz_pca_var(res.pca1, col.var = "black",axes = 3:4) 
# Cos2: qualité de répresentation
head(var1$cos2)
corrplot(var1$cos2, is.corr=FALSE)
#ou fviz_cos2(res.pca1, choice = "var", axes = 1:2)
# Contributions aux composantes principales
head(var1$contrib)
fviz_contrib(res.pca1, choice = "var", axes = 1, top = 14)
#Dimension description
res.desc1 <- dimdesc(res.pca1, axes = c(1,2), proba = 0.05)
# Description de la dimension 1
res.desc1$Dim.1 
# Description de la dimension 2
res.desc$Dim.2

#Description des individus
ind1 <- get_pca_ind(res.pca1)
ind1
# Coordonnées des individus
head(ind1$coord)
# Qualité des individus
head(ind1$cos2)
# Contributions des individus
head(ind1$contrib)
fviz_pca_ind (res.pca1, col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE # Évite le chevauchement de texte
)
#suivant la qualité de representation sur l'axe
fviz_pca_ind (res.pca1, pointsize = "cos2",
              pointshape = 21, fill = "#E7B800",
              repel = TRUE # Évite le chevauchement de texte
)
# Contribution totale sur le 1er axe et le 2eme axe
fviz_contrib(res.pca1, choice = "ind", axes = 1:2, top=30)
fviz_pca_ind(res.pca1 ,
             geom.ind = "point", # Montre les points seulement (mais pas le "text")
             col.ind = res.pca1$ind$cos2, # colorer by groups
             addEllipses = TRUE, # Ellipses de concentration
             legend.title = "Groups"
)
#les variables et les individus:
fviz_pca_biplot(res.pca1, repel = TRUE,
                col.var = "#2E9FDF", # Couleur des variables
                col.ind = "#696969"  # Couleur des individues
)

