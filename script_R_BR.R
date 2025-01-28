a=4
install.packages("readxl")
library(readxl)
resultats_pres2022 <- read_excel("D:/2_travaille_lpsig/1_EXO/9_Programmation/R/data/resultats_pres2022.xlsx")
View(resultats_pres2022)
summary(resultats_pres2022$Part_Macron)
summary(resultats_pres2022$Part_abs)
moyenneMacron <- mean(resultats_pres2022$Nb_Macron)
moyenneMacron #an appelle la variable créée
moyenneMacron <- mean(resultats_pres2022$Part_Macron)
moyenneMacron
paste("La moyenne des parts de vote Macron est de ", round(moyenneMacron, digits = 2), "%")
abs = mean(resultats_pres2022$Part_abs)
paste("L'abstention moyenne est de" abs "%")
paste("L'abstention moyenne est de", roung(abs, digitd = 2), "%")
paste("L'abstention moyenne est de", round(abs, digitd = 2), "%")
paste("L'abstention moyenne est de", round(abs), "%")
paste("L'abstention moyenne est de", round(abs, digits = 2), "%")
#M1
requete1 <- resultats_pres2022 [resultats_pres2022$Part_Macron > 10,]
install.packages("dplyr")
requet2 <- resultats_pres2022[resultats_pres2022$Part_LePen <60,]
requet2 <- resultats_pres2022[resultats_pres2022$Part_LePen > 30 & resultats_pres2022$Part_LePen< 60,]
#M2
requeteJado <- resultats_pres2022 [resultats_pres2022$Part_Jadot > 20 & resultats_pres2022$Part_Jadot < 40 ,]
voteJadot2040 = requeteJado
voteJadot2040 = mean(requeteJado)
voteJadot2040 = (requeteJado)
voteRochefort <- resultats_pres2022 [resultats_pres2022$Libelle == 'Rochefort' & resultats_pres2022$Code_dep == '17',]
#M3
voteSaintes <- resultats_pres2022 [resultats_pres2022$Libelle == 'Saintes',]
write.csv(x = voteSaintes, file = "D:/2_travaille_lpsig/1_EXO/9_Programmation/R/",append = FALSE,quote = FALSE,sep = ",", eol = "\r\n", na = "",dec = ".", row.names = , col.names = , qmethod = "double", fileEncoding = "UTF-8")
write.csv(x = voteSaintes, file = "D:/2_travaille_lpsig/1_EXO/9_Programmation/R/", sep = ",", qmethod = "double", fileEncoding = "UTF-8")
write.csv("voteSaintes", file = "D:/2_travaille_lpsig/1_EXO/9_Programmation/R/")
write.csv("voteSaintes", file = "C:\Users\MB\Desktop")
write.csv("voteSaintes", file = "C:/Users/MB/Desktop")
write.csv("voteSaintes.csv", file = "C:/Users/MB/Desktop")
write.csv("voteSaintes", file = "C:/Users/MB/Desktop/voteSaintes.csv")
write.csv(voteSaintes, file = "C:/Users/MB/Desktop/voteSaintes2.csv")
voteCandidats1_4 <- resultats_pres2022 [ , c("Part_Macron", "Part_LePen", "Part_Melenchon", "Part_Pecresse")]
#M4
voteSaintes1_3 <- resultats_pres2022[resultats_pres2022$Libelle_dep == "Saintes", c("Part_Macron", "Part_LePen", "Part_Melenchon")]
voteSaintes_Candidats1_3 <- voteSaintes[ , c("Part_Macron", "Part_LePen", "Part_Melenchon")]
voteSaintes_Candidats1_3 <- voteSaintes[ , c("Code_BV", "Part_Macron", "Part_LePen", "Part_Melenchon")]
sd (voteSaintes_Candidats1_3)
sd (voteSaintes_Candidats1_3$Part_Macron, voteSaintes_Candidats1_3$Part_LePen, voteSaintes_Candidats1_3$Part_Melenchon)
sd (voteSaintes_Candidats1_3$Part_Macron)
sd (voteSaintes_Candidats1_3$Part_Macron, voteSaintes_Candidats1_3$Part_LePen)
sd (voteSaintes_Candidats1_3("Part_Macron", "Part_LePen"))
sd (voteSaintes_Candidats1_3$Part_Macron)
sd (voteSaintes_Candidats1_3$Part_LePen)
sd (voteSaintes_Candidats1_3$Part_Melenchon)
voteCandidats1_4$colonneTest <- NA
voteCandidats1_4$colonneTest <- voteCandidats1_4$Part_Macron + voteCandidats1_4$Part_Pecresse
install.packages("tidyr")
Transpo <- voteCandidats1_4 %>% pivot_longer(
cols = c(Part_Macron, Part_LePen, Part_Melenchon, Part_Pecresse),
names_to="Candidat", values_to="Parts_vote")
#M5
voteSaintes_Candidats1_3 <- ifelse(voteCandidats1_4$Part_Macron >30, "MacronSup30", NA)
voteSaintes_Candidats1_3 <- ifelse(voteCandidats1_4$Part_Macron >30, voteCandidats1_4$colonneTest = "MacronSup30", NA)
voteCandidats1_4$colonneTest <- ifelse(voteCandidats1_4$Part_Macron >30, "MacronSup30", NA)
savehistory(file="D:\2_travaille_lpsig\1_EXO\9_Programmation\R\taf\script_R_BR.R")
savehistory(file="C:/Users/MB/Desktop/script_R_BR.R")
sd (voteSaintes_Candidats1_3$Part_Macron)
sd (voteSaintes_Candidats1_3$Part_Macron)
View(voteSaintes1_3)
voteSaintes_Candidats1_3 <- voteSaintes[ , c("Part_Macron", "Part_LePen", "Part_Melenchon")]
View(voteSaintes_Candidats1_3)
install.packages("ggplot2")
col1 <- c("Macron", "Melenchon", "Le Pen", "Pecresse")
col2 <- c(mean(voteCandidats1_4$Part_Macron), mean(voteCandidats1_4$Part_Melenchon), mean(voteCandidats1_4$Part_LePen), mean(voteCandidats1_4$Part_Pecresse))
data <- data.frame(group=col1, value=col2)
ggplot(data= data, aes(x=reorder(group, -value), y=value, fill=group)) + geom_bar(stat="identify") +
col1 <- c("Macron", "Melenchon", "Le Pen", "Pecresse")
col1 <- c("Macron", "Melenchon", "Le Pen", "Pecresse")
col2 <- c(mean(voteCandidats1_4$Part_Macron), mean(voteCandidats1_4$Part_Melenchon), mean(voteCandidats1_4$Part_LePen), mean(voteCandidats1_4$Part_Pecresse))
data <- data.frame(group=col1, value=col2)

ggplot(data= data, aes(x=reorder(group, -value), y=value, fill=group)) + geom_bar(stat="identify") +
ggplot(data, aes(x=reorder(group, value), y=value, fill=group))+geom_bar(stat="identity")+
library(ggplot2)
ggplot(data, aes(x=reorder(group, value), y=value, fill=group))+geom_bar(stat="identity")+
+ geom_text(aes(label=round(value, digits = 2)), vjust=1.6, color="white", size=3.5)+scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157", "#6091f6"))
ggplot(data, aes(x=reorder(group, value), y=value, fill=group))+geom_bar(stat="identity") + geom_text(aes(label=round(value, digits = 2)), vjust=1.6, color="white", size=3.5)+scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157", "#6091f6"))
#M7
ggplot(data, aes(x=reorder(group, value), y=value, fill=group))+geom_bar(stat="identity")+
+ geom_text(aes(label=round(value, digits = 2)), vjust=1.6, color="white", size=3.5)+scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157", "#6091f6"))
ggplot(data, aes(x=reorder(group, value), y=value, fill=group))+geom_bar(stat="identity") + geom_text(aes(label=round(value, digits = 2)), vjust=1.6, color="white", size=3.5)+scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157", "#6091f6"))
ggplot(data, aes(x="", y=value , fill=group)) +
geom_bar(stat="identity", width=1) +
geom_col() +
coord_polar("y", start=0) + # coord_polar permet de convertir des barres en cercles
# On ajoute les valeurs de % de vote sur le graph et on personnalise les couleurs
geom_text(aes(label = round(value, digits=2)), position = position_stack(vjust = 0.5)) +
scale_fill_manual(values = c("#0a3895", "#b831f3", "#f33157", "#6091f6"))
col1 <- c("Melenchon", "Jadot", "Hidalgo")
View(requete1)
requeteGauche <- resultats_pres2022[ , c ("Part_Melenchon", "Part_Jadot", "Part_Hidalgo")]
col2 <- c(mean(resultats_pres2022$Part_Melenchon), mean(resultats_pres2022$Part_Jadot), mean(resultats_pres2022$Part_Hidalgo))
data <- data.frame(group=col1, value=col2)
ggplot(data, aes(x="", y=value , fill=group)) +
geom_bar(stat="identity", width=1) +
geom_col() +
coord_polar("y", start=0) +
geom_text(aes(label = round(value, digits=2)), position = position_stack(vjust = 0.5)) +
scale_fill_manual(values = c("#f64d28", "#36c65b", "#f7acf6"))
geom_text(aes(label = round(value, digits=2)), position = position_stack(vjust = 0.5)) +
View(voteSaintes)
voteLaRochelle <- resultats_pres2022[resultats_pres2022$Libelle_dep == "La Rochelle", c("Part_Hidalgo", "Part_Jadot", "Part_Melenchon")]
col1 <- c("Hidalgo", "Jadot", "Melenchon")
col2 <- c(mean(voteLaRochelle$Part_Hidalgo), mean(voteLaRochelle$Part_Jadot), mean(voteLaRochelle$Part_Melenchon))
data <- data.frame(group=col1, value=col2)
ggplot(data, aes(x="", y=value , fill=group)) + geom_bar(stat="identity", width=1) + geom_col() + coord_polar("y", start=0) +
geom_text(aes(label = round(value, digits=2)), position = position_stack(vjust = 0.5)) + scale_fill_manual(values = c("#f7acf6", "#36c65b", "#f64d28"))
voteRochefort_Candidats1_4 <- voteRochefort [ , c("Code_BV", "Part_Macron", "Part_LePen", "Part_Melenchon", "Part_Pecresse")]
ggplot(data=reformat, aes(x=candidats,y=parts,fill=candidats)) +
  geom_bar(stat="identity") +
  facet_wrap(~Code_BV) +
  ggtitle("Parts de vote dans les BV de Rochefort") +
  theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
  theme(axis.text.x = element_text(angle=90)) +
  scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157", "#6091f6"))


db <- "postgres"
db_host <- "localhost"
db_port <- "5432"
db_user <- "postgres"
db_pass <- "vfq562M%"
conn <- dbConnect(RPostgres::Postgres(),dbname = db,host = db_host,port = db_port,user = db_user,password = db_pass)
library(RPostgres)
library(RPostgreSQL)
db <- "postgres"
db_host <- "localhost"
db_port <- "5432"
db_user <- "postgres"
db_pass <- "vfq562M%"
conn <- dbConnect(RPostgres::Postgres(),dbname = db,host = db_host,port = db_port,user = db_user,password = db_pass)
requete <- dbGetQuery(conn, 'SELECT * from php.form;')
View(requete)
requeteDELL <- dbGetQuery(conn, 'DELETE FROM php.form WHERE "tyepdemande = 1;')
requeteDELL <- dbGetQuery(conn, 'DELETE FROM php.form WHERE "typedemande = 1;')
requeteDELL <- dbGetQuery(conn, 'DELETE FROM php.form WHERE "typedemande" = 1;')
#PDF exercice 2_3
install.packages("sf")
read_sf("D:\2_travaille_lpsig\1_EXO\9_Programmation\R\data\test.geojson")
library(sf)
read_sf("D:\2_travaille_lpsig\1_EXO\9_Programmation\R\data\test.geojson")
read_sf("D:\test.geojson")
read_sf("D:\test\communes.shp")
testSIG = read_sf("D:/test/communes.shp")
View(testSIG)
testSIG = read_sf("D:/test/tetst.geojson")
testSIG = read_sf("D:/test/test.geojson")
View(testSIG)
library(readxl)
library(readxl)
resultats_pres2022 <- read_excel("D:/2_travaille_lpsig/1_EXO/9_Programmation/R/data/resultats_pres2022.xlsx")
View(resultats_pres2022)
Part_Marcon <- resultats_pres2022$Part_Macron
Part_LePen <- resultats_pres2022$Part_LePen
cor (Part_Marcon , Part_LePen)
Part_Jadot <- resultats_pres2022$Part_Jadot
# Définition des variables x et y et représentation de la droite
ggplot(resultats_pres2022, aes(x=Part_Macron,
y=Part_Jadot)) + geom_point(size=0.5) +
scale_x_continuous(breaks = seq(1, 50, by = 5)) +
scale_y_continuous(breaks = seq(1, 50, by = 5)) +
stat_smooth(method = "lm",
col = "#C42126",
se = FALSE,
size = 1)
library(ggplot2)
# Définition des variables x et y et représentation de la droite
ggplot(resultats_pres2022, aes(x=Part_Macron,
y=Part_Jadot)) + geom_point(size=0.5) +
scale_x_continuous(breaks = seq(1, 50, by = 5)) +
scale_y_continuous(breaks = seq(1, 50, by = 5)) +
stat_smooth(method = "lm",
col = "#C42126",
se = FALSE,
size = 1)
tabMultiVarie <- resultats_pres2022[, c("Code_dep", "Code_com", "Part_Macron", "Part_LePen", "Part_Melenchon", "Part_Pecresse", "Part_Jadot", "Part_Roussel", "Part_Zemmour", "Part_Hidalgo")]
matriceCor <- tabMultiVarie[, c("Part_Macron", "Part_LePen", "Part_Melenchon", "Part_Pecresse", "Part_Jadot", "Part_Roussel", "Part_Zemmour", "Part_Hidalgo")]
cor (marticeCor)
View(conn)
View(matriceCor)
cor (matriceCor)
Part_Zemmour <- resultats_pres2022$Part_Zemmour
ggplot(resultats_pres2022, aes(x=Part_Le*,
ggplot(resultats_pres2022, aes(x=Part_LePen,
y=Part_Zemmour)) + geom_point(size=0.5) +
scale_x_continuous(breaks = seq(1, 50, by = 5)) +
scale_y_continuous(breaks = seq(1, 50, by = 5)) +
stat_smooth(method = "lm",
col = "#C42126",
se = FALSE,
coords_axe2 <- resultat$ind$coord[ , c("Dim.2")]