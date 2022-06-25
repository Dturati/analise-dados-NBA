setwd("/media/david/49be745a-c2c5-4774-8249-bd01af752abe/mestrado/topicos_experimentos/scripts_R/trabalho")
seassons = read.csv("all_seasons.csv")
View(seassons)
View(seassons$age)
max(seassons$age)

hist(seassons$age)
seassons_filter_2020_21 = seassons
shapiro.test(seassons[seassons$age == 20,]$pts)
shapiro.test(seassons[seassons$age == 21,]$pts)
shapiro.test(seassons[seassons$age == 22,]$pts)
shapiro.test(seassons[seassons$age == 23,]$pts)
shapiro.test(seassons[seassons$age == 24,]$pts)
shapiro.test(seassons[seassons$age == 25,]$pts)

shapiro.test(seassons$age)

ad.test(seassons$age)


tnumber <- function(data) {
  
  if (data < 25) {
    return(25);
  }
  
  if (data >= 25 & data < 30) {
    return(30);
  }
  
  if (data >= 30 & data < 35) {
    return(35);
  }
  
  if (data >= 35 & data < 40) {
    return(40);
  }
  
  if (data >= 40 & data < 45) {
    return(40);
  }
  
  if (data >= 45) {
    return(45);
  }
  
}
idades = c()
for (p in seassons_filter$age) {
  idades <- append(idades, tnumber(p))
}
length(idades)
length(seassons_filter_2020_21$age)


seassons_filter = seassons_filter_2020_21
tnumber <- function(data) {
  
  if (data < 175) {
    return(176);
  }
  
  if (data >= 175 & data < 180) {
    return(180);
  }
  
  if (data >= 180 & data < 185) {
    return(185);
  }
  
  if (data >= 185 & data < 190) {
    return(190);
  }
  
  if (data >= 190 & data < 195) {
    return(195);
  }
  
  if (data >= 195 & data < 200) {
    return(200);
  }
  
  if (data >= 200 & data < 205) {
    return(205);
  }
  
  if (data >= 205 & data < 210) {
    return(210);
  }
  
  if (data >= 210 & data < 215) {
    return(215);
  }
  
  if (data >= 215) {
    return(220);
  }
  
}
altura = c()
for (p in seassons_filter$player_height) {
  altura <- append(altura, tnumber(p))
}


length(altura)
length(idades)

length(seassons_filter_2020_21$player_height)
length(seassons_filter_2020_21$grupo_idades)

seassons_filter_2020_21$grupo_idades <- factor(idades)
seassons_filter_2020_21$grupo_altura <- factor(altura)
View(seassons_filter_2020_21)

am1 = slice(seassons_filter_2020_21[seassons_filter_2020_21$grupo_idades == "25",],(1:500))
am2 = slice(seassons_filter_2020_21[seassons_filter_2020_21$grupo_idades == "30",],(1:500))
am3 = slice(seassons_filter_2020_21[seassons_filter_2020_21$grupo_idades == "35",],(1:500))
am4 = slice(seassons_filter_2020_21[seassons_filter_2020_21$grupo_idades == "40",],(1:500))


library(plyr)
am = rbind.fill(am1,am2,am3, am4)

View(am)
am$exp_pts = exp(am$pts)
View(am)
am$exp_pts
shapiro.test(am[am$grupo_idades == 25,]$log_pts)
shis

shapiro.test(am[am$age == 20,]$pts)
shapiro.test(am[am$age == 21,]$pts)
shapiro.test(am[am$age == 25,]$pts)
shapiro.test(am[am$age == 30,]$pts)
shapiro.test(am[am$age == 35,]$pts)
shapiro.test(am[am$age == 40,]$pts)

shapiro.test(am[am$grupo_idades == 25,]$pts)
shapiro.test(am[am$grupo_idades == 30,]$pts)
shapiro.test(am[am$grupo_idades == 35,]$pts)
shapiro.test(am[am$grupo_idades == 40,]$pts)

hist(am$exp_pts)

library(nortest)
ad.test(am[am$grupo_idades == 25,]$log_pts)

#filrado níveis
seassons_filter_2020_21 = am
View(seassons_filter_2020_21)
View(seassons_filter_2020_21)
#dados exploratórios sobre idade
summary(seassons_filter_2020_21$age)



#analise  exploratórios sobre altura
summary(seassons_filter_2020_21$player_height)
hist(seassons_filter_2020_21$player_height)
shapiro.test(seassons_filter_2020_21$player_height)
library(nortest)
ad.test(seassons_filter_2020_21$age)

#analise  exploratórios sobre paíse
hist(seassons_filter_2020_21$country)
summary(seassons_filter)
seassons_filter_2020_21$pais =  install.packages("skimr")
skimr::skim(seassons_filter_2020_21$country)
plot( as.factor( seassons_filter_2020_21$country )) 

#analise exploratórios sobre pontos
hist(seassons_filter_2020_21$pts)
summary(seassons_filter_2020_21$pts)
plot(seassons_filter_2020_21$pts)
ad.test(seassons_filter_2020_21$pts)
library(plyr)
ddply(seassons_filter_2020_21, ~age, function(data) summary(seassons_filter_2020_21$pts))
seassons_filter_2020_21$pts


#analise exploratórios sobre gp
hist(seassons_filter_2020_21$gp)
summary(seassons_filter_2020_21$gp)
plot(seassons_filter_2020_21$gp)
shapiro.test(seassons_filter_2020_21$gp)
library(plyr)
ddply(seassons_filter_2020_21, ~age, function(data) summary(seassons_filter_2020_21$pts))
seassons_filter_2020_21$pts

#analise  exploratórios sobre draft
hist(seassons_filter_2020_21$draft_year)
seassons_filter_2020_21$pais =  install.packages("skimr")
skimr::skim(seassons_filter_2020_21$country)

#analise  exploratórios sobre rebotes
hist(seassons_filter_2020_21$reb)
summary(seassons_filter_2020_21$reb)

#analise  exploratórios sobre assistencias
hist(seassons_filter_2020_21$ast)
summary(seassons_filter_2020_21$ast)


seassons_filter_2020_21$pais =  install.packages("skimr")
skimr::skim(seassons_filter_2020_21$country)


require( ggplot2 ) 
qplot( seassons_filter_2020_21$country )

qqPlot(seassons_filter_2020_21$pts)
qqPlot(seassons_filter_2020_21$age)
hist(seassons_filter_2020_21$pts)
length(seassons_filter_2020_21$age)
summary(seassons_filter_2020_21$age)

hist(seassons$age)

#dos fatores
seassons_filter_2020_21$age <- factor(seassons_filter_2020_21$age)
seassons_filter_2020_21$player_height <- factor(seassons_filter_2020_21$player_height)
seassons_filter_2020_21$player_weight <- factor(seassons_filter_2020_21$player_weight)
seassons_filter_2020_21$college <- factor(seassons_filter_2020_21$college)
seassons_filter_2020_21$season <- factor(seassons_filter_2020_21$season)
seassons_filter_2020_21$player_name <- factor(seassons_filter_2020_21$player_name)
View(seassons_filter_2020_21)


############################# Análise estatistica   #################################################################33
############################# Idade por pontos      ##########################

shapiro.test(seassons_filter_2020_21[seassons_filter_2020_21$grupo_idades == 25,]$pts)
shapiro.test(seassons_filter_2020_21[seassons_filter_2020_21$grupo_idades == 30,]$pts)

library(nortest)
ad.test(seassons_filter_2020_21$pts)

hist(seassons_filter_2020_21$pts)
plot(pts, data=seassons_filter_2020_21) # boxplot

library(coin)
kruskal_test(pts ~ grupo_idades, data=seassons_filter_2020_21, distribution="asymptotic")
library(PMCMRplus)
View(seassons_filter_2020_21)
pts_age <- PMCMRplus::kwAllPairsConoverTest(pts ~ grupo_idades , data=seassons_filter_2020_21, p.adjust.method="bonferroni")
pts_age
summary(pts_age)
plot(pts_age)


#quantidade de potos por pessoas
par(mfrow = c(2, 2))
hist(seassons_filter_2020_21[seassons_filter_2020_21$grupo_idades == "25",]$pts, xlab = "Pontos", main = "Pontos marcados pelos atletas de 25 anos")
hist(seassons_filter_2020_21[seassons_filter_2020_21$grupo_idades == "30",]$pts)
hist(seassons_filter_2020_21[seassons_filter_2020_21$grupo_idades == "35",]$pts)
hist(seassons_filter_2020_21[seassons_filter_2020_21$grupo_idades == "40",]$pts)


summary(seassons_filter_2020_21[seassons_filter_2020_21$grupo_idades == "25",])
length(seassons_filter_2020_21[seassons_filter_2020_21$grupo_idades == 25,])
View(seassons_filter_2020_21)

############################# Idade por rebotes   ##########################

library(nortest)
ad.test(seassons_filter_2020_21$reb)

hist(seassons_filter_2020_21$reb)
plot(reb, data=seassons_filter_2020_21) # boxplot
boxplot(reb  ~ grupo_idades, data=seassons_filter_2020_21)
library(coin)
kruskal_test(reb ~ grupo_idades, data=seassons_filter_2020_21, distribution="asymptotic")
#install.packages("Rmpfr")
#install.packages("PMCMRplus")
library(PMCMRplus)
age_reb = PMCMRplus::kwAllPairsConoverTest(reb ~ grupo_idades , data=seassons_filter_2020_21, p.adjust.method="bonferroni")
age_reb
summary(age_reb)
plot(age_reb)



############################# Altura e rebotes ##########################
ad.test(seassons_filter_2020_21$reb)
View(seassons_filter_2020_21)
kruskal_test(reb ~ player_height, data=seassons_filter_2020_21, distribution="asymptotic")

reb_height = PMCMRplus::kwAllPairsConoverTest(reb ~ grupo_altura , data=seassons_filter_2020_21, p.adjust.method="bonferroni")
library(stats)
airwise.wilcox.test(seassons_filter_2020_21$reb ,seassons_filter_2020_21$player_height, p.adjust.method = "bonf")
reb_height
summary(reb_height)
plot(reb_height)
View(seassons_filter_2020_21)


hapiro.test(ide3[ide3$IDE == "VStudio",]$logTime)

res1 = seassons_filter_2020_21[seassons_filter_2020_21$grupo_altura == "220",]
View(res)

res2 = seassons_filter_2020_21[seassons_filter_2020_21$grupo_altura == "205",]
View(res2)

seassons_filter_2020_21[seassons_filter_2020_21$grupo_idades == "205",]

library(phia)
interaction_graph = interactionMeans(m1)
plot(interaction_graph)

library(nortest)
ad.test(m1$residuals)
lillie.test(m1$residuals)


############################# Numero de jogos disputados gp por idade por temporada      ##########################

shapiro.test(seassons_filter_2020_21$gp)
View(seassons_filter_2020_21)
library(nortest)
ad.test(seassons_filter_2020_21$gp)

hist(seassons_filter_2020_21$gp)
plot(gp ~ age, data = seassons_filter_2020_21) # boxplot
summary(seassons_filter_2020_21$gp)

library(coin)
#ao aplicar o teste de Kruskal podemos regeitar a hipotese nula de que a 
kruskal_test(gp ~ age, data=seassons_filter_2020_21, distribution="asymptotic")
library(PMCMRplus)
gp_age = PMCMRplus::kwAllPairsConoverTest(gp ~ age , data=seassons_filter_2020_21, p.adjust.method="bonferroni")
gp_age
summary(pts_age)
plot(pts_age)



############### idade por net rating ###########################################

kruskal_test(net_rating ~ age, data=seassons_filter_2020_21, distribution="asymptotic")
rating = PMCMRplus::kwAllPairsConoverTest(net_rating ~ player_height , data=seassons_filter_2020_21, p.adjust.method="bonferroni")
rating
summary(rating)
plot(rating)
View(seassons_filter_2020_21)


res2 = pairwise.wilcox.test(seassons_filter_2020_21$pts ,seassons_filter_2020_21$age, p.adjust.method = "bonf")
summary(res2)
plot(res2)
boxplot(res2)

library(phia)
interaction_graph = interactionMeans(m1)
plot(interaction_graph)

library(nortest)
ad.test(m1$residuals)
lillie.test(m1$residuals)

############################# temporadas  por  pontos ##########################
seassons_filter = seassons
tnumber <- function(data) {
  return(as.numeric(unlist(strsplit(data, "-"))[1]))
}
x = c()
for (p in seassons_filter$season) {
  x <- append(x, tnumber(p))
}


hist(seassons_filter$season)
seassons_filter$season
seassons_filter$newSeason =  x
seassons_filter$newSeasn <- factor(seassons_filter$newSeason)
View(seassons_filter)
boxplot(pts ~ newSeason, data = seassons_filter)

View(seassons)
kruskal_test(pts ~ newSeasn, data=seassons_filter, distribution="asymptotic")

gp_pts= PMCMRplus::kwAllPairsConoverTest(pts ~ newSeasn , data=seassons_filter_2020_21, p.adjust.method="bonferroni")
gp_pts
summary(gp_pts)
plot(gp_pts)


res2 = pairwise.wilcox.test(seassons_filter_2020_21$pts ,seassons_filter_2020_21$gp, p.adjust.method = "bonf")
summary(res2)
plot(res2)
boxplot(res2)




