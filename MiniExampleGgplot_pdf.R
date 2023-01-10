################################################################################
#
# Minibeispiele für pdf-Output
# Ulrike Niemann, Januar 2023
#
################################################################################
# ------------------------------------------------------------------------------
# librarys laden
#
library(tidyverse)  # inklusive ggplot2
#library(plotly)     # interaktive Grafiken
#library(DT)         # interaktive Tabellen
library(gapminder)  # Beispieldaten
library(knitr)
# ------------------------------------------------------------------------------
#
# Daten anschauen
#datatable(gapminder, rownames = FALSE, options = list(pageLength = 5))
kable(gapminder)
# ------------------------------------------------------------------------------
# Daten aufbereiten: filtern, Spalten umbenennen
#
data <- gapminder %>% 
  filter(year >= 1957 & country != "Kuwait") %>% 
  rename(Jahr = year,
         Land = country,
         Kontinent = continent,
         Lebenserwartung = lifeExp,
         Einwohner = pop,
         BIPproKopf = gdpPercap)
#
# animiertes Bubble-Chart ------------------------------------------------------
bubble <- ggplot(data, 
                 aes(x = BIPproKopf, y = Lebenserwartung, color = Kontinent)) +
  geom_point(aes(size = Einwohner, frame = Jahr, ids = Land)) +
  scale_x_log10()
#ggplotly(bubble)
print(bubble)
#
# Regression für 2007 ----------------------------------------------------------
reg <- ggplot(data %>% filter(Jahr == 2007), 
            aes(x = BIPproKopf, y = Lebenserwartung)) +
  geom_point(aes(color = Kontinent)) + 
  geom_smooth(method = lm) + 
  scale_x_log10()
#ggplotly(reg)
print(reg)
#
# Balken BIP nach Kontinent ----------------------------------------------------
tab <- data %>% 
  group_by(Kontinent) %>% 
  summarize(BIPproKopf = mean(BIPproKopf)) 

bar <- ggplot(tab, aes(Kontinent, BIPproKopf, fill = Kontinent)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = BIPproKopf %>% round())) +
  theme(axis.text.y=element_blank()) 
#ggplotly(bar)
print(bar)
#
################################################################################