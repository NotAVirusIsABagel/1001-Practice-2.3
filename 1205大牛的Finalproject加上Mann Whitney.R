library(tidyr)
library(dplyr)
library(terra)
library(rgbif)
library(sp)
library(leaflet)
library(ggplot2)
library(emmeans)

raw.data <- read_excel("C:/Users/User/Desktop/碩一上/RRRRR/0924-in-class-practice/excel spawning.xlsx")

#select the countries needed----
spawning.country <- raw.data %>% 
  filter(Country %in% c('Indonesia', 'Japan', 'Philippines', 'Taiwan', 'Australia', 'Fiji')) %>%
  filter(Genus %in% c('Diploastrea', 'Galaxea', 'Lobophyllia', 'Coelastrea', 'Dipsastraea', 'Favites', 
                        'Goniastrea', 'Leptoria', 'Merulina', 'Pectinia', 'Platygyra', 'Porites'))

#delete the unneeded columns----
spawning.country[ , c('Subsite', 'O_n', 'Depth_m', 'N', 'No_start', 'Quality_start', 'No_end', 'Quality_end', 
                      'Gamete_release', 'Situation', 'Timezone', 'Reference', 'Comments')] <- list(NULL)
spawning.country$color <- ifelse(spawning.country$Country == "Japan", " #ee2c2c",
                                ifelse(spawning.country$Country == "Taiwan", "dodgerblue",
                                       ifelse(spawning.country$Country == "Philippines","forestgreen", "gold")))

#Mapping----
leaflet() %>%
  addTiles() %>%
  setView(lng = 121.0, lat = 20, zoom = 3.5) %>% # Center the map on Taiwan
  addCircleMarkers(
    lng = spawning.country$Longitude,
    lat = spawning.country$Latitude,
    popup = spawning.country$Site,
    radius = 2,
    fillOpacity = 0.6,
    color = spawning.country$color)

#trials----
# Genus vs. DoSRtNFM
genus.day <- ggplot(data = spawning.country,
                    aes(x = Genus, y = DoSRtNFM))+
  geom_boxplot()
genus.day

#Country vs. DoSRtNFM----
Country.day <- ggplot(data = spawning.country,
                    aes(x = Country, y = DoSRtNFM))+
  geom_boxplot()
Country.day

#twjp----
spawning.tw.jp <- raw.data %>% 
  filter(Country %in% c('Japan', 'Taiwan')) %>%
  filter(Genus %in% c('Galaxea', 'Lobophyllia', 'Coelastrea', 
                      'Dipsastraea', 'Favites', 'Goniastrea', 
                      'Platygyra', 'Porites'))
spawning.tw.jp[ , c('Subsite', 'O_n', 'Depth_m', 'N', 'No_start', 'Quality_start', 'No_end', 'Quality_end', 'Gamete_release', 'Situation', 'Timezone', 'Reference', 'Comments')] <- list(NULL)

genus.tj.day <- ggplot(data = spawning.tw.jp,
                    aes(x = Genus, y = DoSRtNFM, color = Country))+
  geom_boxplot()
genus.tj.day

#check residual----
model <- aov(DoSRtNFM ~ Genus + Country * Genus, data = spawning.tw.jp)
summary(model)
plot(model, which=c(1,2))


#Mann-Whitney
Coelastrea <- spawning.tw.jp %>%
  filter(Genus %in% "Coelastrea",
         Country %in% c("Taiwan", "Japan"))
Coelastrea.test <- wilcox.test(DoSRtNFM ~ Country, data = Coelastrea)
print(Coelastrea.test)

Dipsastraea <- spawning.tw.jp %>%
  filter(Genus %in% "Dipsastraea",
         Country %in% c("Taiwan", "Japan"))
Dipsastraea.test <- wilcox.test(DoSRtNFM ~ Country, data = Dipsastraea)
print(Dipsastraea.test)

Favites <- spawning.tw.jp %>%
  filter(Genus %in% "Favites",
         Country %in% c("Taiwan", "Japan"))
Favites.test <- wilcox.test(DoSRtNFM ~ Country, data = Favites)
print(Favites.test)

Galaxea <- spawning.tw.jp %>%
  filter(Genus %in% "Galaxea",
         Country %in% c("Taiwan", "Japan"))
Galaxea.test <- wilcox.test(DoSRtNFM ~ Country, data = Galaxea)
print(Galaxea.test)

Goniastrea <- spawning.tw.jp %>%
  filter(Genus %in% "Goniastrea",
         Country %in% c("Taiwan", "Japan"))
Goniastrea.test <- wilcox.test(DoSRtNFM ~ Country, data = Goniastrea)
print(Goniastrea.test)

Lobophyllia <- spawning.tw.jp %>%
  filter(Genus %in% "Lobophyllia",
         Country %in% c("Taiwan", "Japan"))
Lobophyllia.test <- wilcox.test(DoSRtNFM ~ Country, data = Lobophyllia)
print(Lobophyllia.test)

Platygyra <- spawning.tw.jp %>%
  filter(Genus %in% "Platygyra",
         Country %in% c("Taiwan", "Japan"))
Platygyra.test <- wilcox.test(DoSRtNFM ~ Country, data = Platygyra)
print(Platygyra.test)

Porites <- spawning.tw.jp %>%
  filter(Genus %in% "Porites",
         Country %in% c("Taiwan", "Japan"))
Porites.test <- wilcox.test(DoSRtNFM ~ Country, data = Porites)
print(Porites.test)




#emmeans----
 #Ho: In different Genus, Japan=Taiwan
 #H1: In different Genus, Japan is not equal to Taiwan
emmeans(model, specs= pairwise~Country * Genus, 
        at=list(Genus='Coelastrea'), Country=c('Japan', 'Taiwan'))
emmeans(model, specs= pairwise~Country * Genus, 
        at=list(Country=c('Japan', 'Taiwan'), Genus='Dipsastraea'))
emmeans(model, specs= pairwise~Country * Genus, 
        at=list(Country=c('Japan', 'Taiwan'), Genus='Favites'))
emmeans(model, specs= pairwise~Country * Genus, 
        at=list(Country=c('Japan', 'Taiwan'), Genus='Galaxea'))
emmeans(model, specs= pairwise~Country * Genus, 
        at=list(Country=c('Japan', 'Taiwan'), Genus='Goniastrea'))
emmeans(model, specs= pairwise~Country * Genus, 
        at=list(Country=c('Japan', 'Taiwan'), Genus='Lobophyllia'))
emmeans(model, specs= pairwise~Country * Genus, 
        at=list(Country=c('Japan', 'Taiwan'), Genus='Platygyra'))
emmeans(model, specs= pairwise~Country * Genus, 
        at=list(Country=c('Japan', 'Taiwan'), Genus='Porites'))

#anova&tukey----沒有常態分布所以不能用
spawning.aov <- aov(DoSRtNFM ~ Genus + Country %in% Genus, data = spawning.tw.jp)
summary(spawning.aov)

TukeyHSD(spawning.aov, which = "Genus:Country")

#filter----
Coelastrea <- spawning.tw.jp %>%
  filter(Genus %in% "Coelastrea",
         Country %in% c("Taiwan", "Japan"))
Coelastrea.aov <- aov(DoSRtNFM ~ Country, data = Coelastrea)
summary(Coelastrea.aov)

Dipsastraea <- spawning.tw.jp %>%
  filter(Genus %in% "Dipsastraea",
         Country %in% c("Taiwan", "Japan"))
Dipsastraea.aov <- aov(DoSRtNFM ~ Country, data = Dipsastraea)
summary(Dipsastraea.aov)

#plus fiji----
spawning.tw.jp.f <- raw.data %>% 
  filter(Country %in% c('Japan', 'Taiwan', 'Fiji')) %>%
  filter(Genus %in% c('Diploastrea', 'Galaxea', 'Lobophyllia', 'Coelastrea', 'Dipsastraea', 'Favites', 
                      'Goniastrea', 'Leptoria', 'Merulina', 'Pectinia', 'Platygyra', 'Porites'))
spawning.tw.jp.f[ , c('Subsite', 'O_n', 'Depth_m', 'N', 'No_start', 'Quality_start', 'No_end', 'Quality_end', 
                    'Gamete_release', 'Situation', 'Timezone', 'Reference', 'Comments')] <- list(NULL)
genus.tjf.day <- ggplot(data = spawning.tw.jp.f,
                       aes(x = Genus, y = DoSRtNFM, color = Country))+
  geom_boxplot()
genus.tjf.day

# split date----
split.data <- raw.data %>%
  separate(Date, into = c("date", "month", "year"), sep = "/")
max(summary(as.factor(split.data$year))) #use 2016 data

class(split.data$date)
class(split.data$month)

combine.date.2016 <- split.data %>% 
  filter(year %in% "2016") %>% 
  mutate(month_date = paste0(month, date))

ggplot(combine.date.2016, aes(x = month_date, y = Site)) + geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
