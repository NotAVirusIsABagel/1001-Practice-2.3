---
  title: "idk"
author: "Annie"
date: "2024-11-26"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
library(dplyr)
library(terra)
library(rgbif)
library(sp)
library(leaflet)
library(ggplot2)
library(readxl)


raw.data <- read_excel("C:/Users/User/Desktop/碩一上/RRRRR/0924-in-class-practice/excel spawning.xlsx")

# select the countries needed 
spawning.country <- raw.data %>% 
  filter(Country %in% c('Indonesia', 'Japan', 'Philippines', 'Taiwan', 'Australia', 'Fiji')) %>%
  filter(Genus %in% c('Diploastrea', 'Galaxea', 'Lobophyllia', 'Coelastrea', 'Dipsastraea', 'Favites', 
                      'Goniastrea', 'Leptoria', 'Merulina', 'Pectinia', 'Platygyra', 'Porites'))

# delete the unneeded columns
spawning.country[ , c('Subsite', 'O_n', 'Depth_m', 'N', 'No_start', 'Quality_start', 'No_end', 'Quality_end', 
                      'Gamete_release', 'Situation', 'Timezone', 'Reference', 'Comments')] <- list(NULL)
spawning.country$color <- ifelse(spawning.country$Country == "Japan", " #ee2c2c",
                                 ifelse(spawning.country$Country == "Taiwan", "dodgerblue",
                                        ifelse(spawning.country$Country == "Philippines","forestgreen", "gold")))

# Mapping
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
----#trials----
# Genus vs. DoSRtNFM
genus.day <- ggplot(data = spawning.country,
                    aes(x = Genus, y = DoSRtNFM))+
  geom_boxplot()
genus.day

#Country vs. DoSRtNFM
Country.day <- ggplot(data = spawning.country,
                      aes(x = Country, y = DoSRtNFM))+
  geom_boxplot()
Country.day

------#twjp--------

spawning.tw.jp <- raw.data %>% 
  filter(Country %in% c('Japan', 'Taiwan')) %>%
  filter(Genus %in% c('Galaxea', 'Lobophyllia', 'Coelastrea', 'Dipsastraea', 'Favites', 
                      'Goniastrea', 'Platygyra', 'Porites'))
spawning.tw.jp[ , c('Subsite', 'O_n', 'Depth_m', 'N', 'No_start', 'Quality_start', 'No_end', 'Quality_end', 
                    'Gamete_release', 'Situation', 'Timezone', 'Reference', 'Comments')] <- list(NULL)

genus.tj.day <- ggplot(data = spawning.tw.jp,
                       aes(x = Genus, y = DoSRtNFM, color = Country))+
  geom_boxplot()
genus.tj.day

----#anova&tukey
  spawning.aov <- aov(DoSRtNFM ~ Genus + Genus %in% Country, data = spawning.tw.jp)
summary(spawning.aov)

  aov_genus <- aov(DoSRtNFM ~ Genus, data = spawning.tw.jp)
summary(aov_genus)

#國家與物種的關係(好像沒什麼用)
# ANOVA檢驗國家和物種的交互作用對spawning time的影響
aov_interaction <- aov(DoSRtNFM ~ Country * Genus, data = spawning.tw.jp)
summary(aov_interaction)


TukeyHSD(spawning.aov)

----#filter----
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

----# with fiji----
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


```
----#nested ANOVA
  nested.aov <- aov(DoSRtNFM ~ Country + Genus %in% Country, data = spawning.tw.jp)
summary(nested.aov)
#Country 的 p 值（0.00249）：表示!不同國家!之間的繁殖時間有顯著差異。
#Genus:Country 的 p 值（5.76e-12）：表示某個國家內部，!不同物種!的繁殖時間也有顯著差異。

ggplot(data = spawning.tw.jp, aes(x = Genus, y = DoSRtNFM, fill = Genus)) +
  geom_boxplot() +
  facet_wrap(~ Country) +
  labs(title = "Nested ANOVA Data Distribution", x = "Genus", y = "Spawning Time") +
  theme_minimal()



----#PERMANOVA (好像沒有複雜到需要用這個)(因為有數值缺失或空行或負數所以bray不能用)(放棄)
  # 安裝並載入 vegan 包 菜包

library(vegan)

#spawning.tw.jp <- spawning.tw.jp[spawning.tw.jp$DoSRtNFM >= 0, ]
#spawning.tw.jp <- spawning.tw.jp[complete.cases(spawning.tw.jp$DoSRtNFM), ]
#spawning.tw.jp <- spawning.tw.jp[!is.na(spawning.tw.jp$DoSRtNFM), ]


# 假設你的數據為 spawning.tw.jp
# 計算 Bray-Curtis 距離矩陣
#dist_matrix <- vegdist(spawning.tw.jp[, c("DoSRtNFM")], method = "bray")

# PERMANOVA 分析
#result <- adonis2(dist_matrix ~ Country + Genus, data = spawning.tw.jp)
#print(result)

---#i have no idea wat im doin
  # 確保必要欄位為數字型態
  
spawning.tw.jp$Latitude <- as.numeric(spawning.tw.jp$Latitude)
spawning.tw.jp$DoSRtNFM <- as.numeric(spawning.tw.jp$DoSRtNFM)

# 依緯度對地點名稱進行排序
spawning.tw.jp <- spawning.tw.jp %>%
  arrange(Latitude) %>%
  mutate(Site = factor(Site, levels = unique(Site)))  # 把地點設為因子型態，依緯度排列


library(ggplot2)

ggplot(spawning.tw.jp, aes(x = DoSRtNFM, y = Site)) +
  geom_point(aes(color = Country), size = 3, alpha = 0.7) +  # 根據國家上色
  labs(
    x = "Spawning Time (Day of Year)",
    y = "Site (Ordered by Latitude)",
    title = "Major Spawning Time by Site",
    color = "Country"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # 調整 y 軸標籤大小


spawning.tw.jp <- spawning.tw.jp %>%
  arrange(Country, Latitude) %>%
  mutate(Site = factor(Site, levels = unique(Site)))

spawning.tw.jp <- spawning.tw.jp %>%
  group_by(Site) %>%
  summarise(mean_time = mean(DoSRtNFM, na.rm = TRUE)) %>%
  arrange(mean_time) %>%
  mutate(Site = factor(Site, levels = unique(Site)))

spawning.tw.jp$Site <- factor(
  spawning.tw.jp$Site,
  levels = c("Site1", "Site3", "Site2", "Site4")  # 按需求調整順序
)

ggplot(spawning.tw.jp, aes(x = DoSRtNFM, y = Site)) +
  geom_point(aes(color = Country), size = 3, alpha = 0.7) +
  geom_vline(xintercept = c(120, 150), linetype = "dashed", color = "red") +  # 標示範圍
  labs(
    x = "Spawning Time (Day of Year)",
    y = "Site (Ordered by Latitude)",
    title = "Major Spawning Time by Site",
    color = "Country"
  ) +
  theme_minimal()
