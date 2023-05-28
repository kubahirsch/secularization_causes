install.packages("fansi")
install.packages("fastmap")
install.packages("labeling")
install.packages("ggcorrplot")
install.packages("ggthemes")
library(modelr)
library(kableExtra)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(knitr)
library(forcats)
library(tidyverse)
library(ggcorrplot)
library(gridExtra)
library(reshape2)
library(haven)




#wczytujemy tablicę dla hapinnes 2016 
happiness2016 <- read.csv ("2016.csv",
                   header = TRUE)

#kraje w rankingu od najszęśliwszego w 2016 roku
head(happiness2016)

#zobaczmy to na wykresie
ggplot(happiness2016) + geom_point(aes(x= Happiness.Rank, y = Happiness.Score), colour = "skyblue")+
  theme_tufte()+xlab("Kraje") + ylab("Wynik szczęścia")

#spoko nic ciekawego, spójrzy teraz jak wygląda wynik szęścia na GPD per capita

ggplot(happiness2016) + geom_point(aes(x= Economy..GDP.per.Capita., y = Happiness.Score), colour = "skyblue")+
  theme_tufte()

#hmm ok, przypadek? sprawdźmy czy mamy jakieś zależności dla całości

#dla 20 najszczęśliwszych i 20 najmniej

happiest <- happiness2016[1:20, ]
unhappiest <- happiness2016[138:157, ]

#usuńmy dane nienumeryczne

head(bezkraju)

bezkraju <- happiest[, -1]
bezkraju <- happiest[, -2]
head(bezkraju)
bezkraju <- bezkraju[, -1]
head(bezkraju)

#teraz sprawdźmy korelacje

corhap <- cor(bezkraju , method='spearman')
ggcorrplot(corhap, method = "square")

#to samo dla biednych

bezkrajubieda <- unhappiest[, -1]
bezkrajubieda <- bezkrajubieda[, -1]
head(bezkrajubieda)

corunhap <- cor(bezkrajubieda , method='spearman')
ggcorrplot(corunhap, method = "square")

#teraz dla całości

całośćbezkraju <- happiness2016[, -1]
całośćbezkraju <- całośćbezkraju[, -1]
corall <- cor(całośćbezkraju , method='spearman')
ggcorrplot(corall, method = "square")

#spoko nic ciekawego, spójrzy teraz jak wygląda wynik szęścia na GPD per capita

ggplot(stat1) + geom_point(aes(x= Economy..GDP.per.Capita., y = Happiness.Score), colour = "skyblue")+
  theme_tufte()

#jakby trochę to zmienić 

ggplot(stat1, aes(x = Economy..GDP.per.Capita., y = Happiness.Score)) +
  geom_jitter(colour = "red") +
  geom_smooth(span = .3, colour = "black") +
  theme_tufte()
# teraz podzielmy bazę na kontynenty

Europa <- stat1[which(stat1$Region == "Western Europe" | stat1$Region == "Central and Eastern Europe"), ]

Azja <- stat1[which(stat1$Region == "Eastern Asia" | stat1$Region == "Southeastern Asia" | 
                      stat1$Region == "Southern Asia"), ]

Afryka <- stat1[which(stat1$Region == "Middle East and Northern Africa" |
                        stat1$Region == "Sub-Saharan Africa"), ]  

Australia <- stat1[which(stat1$Region == "Australia and New Zealand"),]

AmerykaPln <- stat1[which(stat1$Region == "North America"),]

AmerykaLac <- stat1[which(stat1$Region == "Latin America and Caribbean"),]

# teraz dodajmy dane, żeby nie miały zbyt małej próbki

Ameryki <- stat1[which(stat1$Region == "North America" | stat1$Region == "Latin America and Caribbean"), ]

AzjaAustralia <- stat1[which(stat1$Region == "Eastern Asia" | stat1$Region == "Southeastern Asia" | 
                               stat1$Region == "Southern Asia" | stat1$Region =="Australia and New Zealand" ), ]

#teraz sprawdźmy to samo dla regionów

ggplot(Europa, aes(x = Economy..GDP.per.Capita., y = Happiness.Score)) +
  geom_jitter(colour = "blue") +
  geom_smooth(span = .3, colour = "black") +
  theme_tufte()

ggplot(AzjaAustralia, aes(x = Economy..GDP.per.Capita., y = Happiness.Score)) +
  geom_jitter(colour = "orange") +
  geom_smooth(span = .3, colour = "grey") +
  theme_clean()

ggplot(Afryka, aes(x = Economy..GDP.per.Capita., y = Happiness.Score)) +
  geom_jitter(colour = "red") +
  geom_smooth(span = .3, colour = "black") +
  theme_tufte()

ggplot(Ameryki, aes(x = Economy..GDP.per.Capita., y = Happiness.Score)) +
  geom_jitter(colour = "darkblue") +
  geom_smooth(span = .3, colour = "aquamarine") +
  theme_tufte()

#jak widać tylko w Afryce i Europie , dolny wskaźnik GDP nie jest odwzorowany w Happiness Score, sprawdźmy z czym to się wiąże

Afrykabezkraju <- Afryka[, -1]
Afrykabezkraju <- Afrykabezkraju[, -1]

corafr <- cor(Afrykabezkraju , method='spearman')
ggcorrplot(corafr, method = "square")

Europabezkraju <- Europa[, -1]
Europabezkraju <-Europabezkraju[, -1]
coreur <- cor(Europabezkraju , method='spearman')
ggcorrplot(coreur, method = "square")




#Zmieniam ustawienia R tak, żeby nie było notacji typu e+07
options(scipen = 999)

#wczytuje dane o pkb

gpd_1960_2020 <- read.csv("gpd_1960_2020.csv",header = TRUE)
head(gpd_1960_2020)
gpd_1999_2020 <- read.csv("gpd_1999_2022.csv",header = TRUE)
melt(gpd_1999_2020)
years <- colnames(gpd_1999_2020)
USA_gdp <- gpd_1999_2020[172,]
unlist(USA_gdp[1,], use.names = FALsSE)
USA_gdp_df <- data.frame()

ggplot(USA_gdp) + geom_line(aes(x= years, y = chrstcat), colour = "skyblue")+
  theme_tufte()+xlab("Rok") + ylab("Ilość katolików")
USA_gdp
#wczytuje dane o religiach


national_religion <- read.csv("national_religion.csv",
                              header = TRUE)


# wykres dla USA i katolików

national_religion_usa <-national_religion[2:15,]

ggplot(national_religion_usa) + geom_line(aes(x= year, y = chrstcat), colour = "skyblue")+
  theme_tufte()+xlab("Rok") + ylab("Ilość katolików")






