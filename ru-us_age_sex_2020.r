library(tidyverse)
library(ggplot2)
library(forcats)
library(scales)
library(ggthemes)
library(purrr)
library(ggtext)
library(extrafont)
library(glue)
library(here)

here::i_am("ru-us_age_sex_2020.r")

dem_files = list("United States" = here("data","United States of America-2020.csv"),"Russia" = here("data","Russian Federation-2020.csv"))

pyramid <- map_dfr(dem_files,read_csv,.id="name")


pyramid <- pyramid %>% pivot_longer(cols = c("M","F"), names_to = "gender")

pyramid$id <- seq_along(pyramid$Age)

pyramid$Age <- fct_reorder(pyramid$Age,pyramid$id)

pyramid <- pyramid %>% 
  group_by(name) %>% 
  mutate(percent = value/sum(value) * 100) %>% 
  as.data.frame() %>% 
  mutate(percent = round(percent,2))

pyramid <- pyramid %>% 
  group_by(name) %>% 
  mutate(total_pop = sum(value)) %>% 
  ungroup() %>%
  mutate(facet_lbl = glue("<b>{name}</b><br/><i>2020 Population: </i>{format(total_pop,big.mark=',')}"))

age_adjust <- c("75-79","80-84","85-89","90-94","95-99","100+")

pyramid <- pyramid %>% mutate(perc_dodge = case_when(
  !(Age %in% age_adjust) & gender == "F" ~ -percent/2+.2,
  !(Age %in% age_adjust) & gender == "M" ~ percent/2+.2,
  Age %in% age_adjust & gender == "F" ~ -percent -.5,
  Age %in% age_adjust & gender == "M" ~ percent + .5))

pyramid <- pyramid %>% 
  group_by(name, Age) %>% 
  mutate(surplus = abs(diff(percent)))

pyramid <- pyramid %>% 
  group_by(name,Age) %>%
  mutate(ranks = order(order(percent,decreasing=T))) %>% 
  mutate(surplus = ifelse(ranks == 1, surplus, 0))


abs_perc <- function(x) { 
  return(paste0(abs(x),"%"))
  }

`%notin%` <- Negate("%in%")  

male.col <- "#b0e0e6"
fm.col <- "#fc8eac"
male.surp.col <- "#5c9499"
fm.surp.col <- "#ad87a8"

pyr <- ggplot(pyramid, aes(x = Age)) +
  geom_linerange(
    data = pyramid[pyramid$gender == "M",],
    aes(ymin = 0, ymax = percent, color = "Male"),
    size = 10) +
  geom_linerange(
    data = pyramid[pyramid$gender == "F", ],
    aes(ymin = 0, ymax = -percent, color = "Female"),
    size = 10) +
  geom_linerange(
    data = pyramid[pyramid$gender == "F",],
    aes(ymin = -percent+surplus, ymax = -percent, color = "Female Surplus"),
    size = 10) +
  geom_linerange(
    data = pyramid[pyramid$gender == "M",],
    aes(ymin = percent - surplus, ymax = percent, color = "Male Surplus"),
    size = 10) +
  facet_wrap(~facet_lbl, ncol = 2, scales = "fixed") +
  scale_y_continuous(breaks = c(-3,-2,-1,0,1,2,3), labels = abs_perc) +
  scale_color_manual(labels = c("Male","Female", "Male Surplus", "Female Surplus"),
                     breaks = c("Male","Female","Male Surplus","Female Surplus"), 
                     name="Sex", values = c(male.col, fm.col, male.surp.col,fm.surp.col)) +
  geom_hline(yintercept = 0, color = "#696969", size = .5) +
  labs(title = "Russia vs. U.S. Population Pyramid, 2020", caption="<i style='color:#0F5257'>Source:</i> populationpyramid.net. Viz by @nmill092.",
       y = "Share of Total 2020 Population, %") +
  geom_text(aes(label = paste0(percent,"%"), 
                y = perc_dodge), 
            family = "Roboto Mono", size=5) + 
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(size = .075, color = "black"),
    axis.ticks.y = element_blank(),
    axis.text.x = element_markdown(family = "Roboto Mono", size = 10, face="italic"),
    axis.text.y = element_text(family = "Roboto Light", size = 14),
    plot.background = element_rect(fill="#F0F8FF"),
    panel.background = element_blank(),
    strip.text = element_markdown(family = "Roboto", margin = margin(b = 40), size = 20),
    strip.background = element_blank(),
    plot.title = element_text(family = "Roboto Slab ExtraBold", size = 40, hjust = .5, margin=margin(b=20,t=30)),
    axis.title.y = element_blank(),
    axis.title.x = element_text(family = "Roboto Light", size = 16, margin = margin(t = 15)),
    plot.caption = element_markdown(hjust=.5,size = 14, color = "#696969"),
    plot.caption.position = "panel",
    legend.title = element_blank(),
    legend.direction = "horizontal",
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key.width = unit(50,"points"),
    legend.key = element_blank(),
    legend.box.just = "left",
    legend.position = "bottom",
    legend.spacing = unit(10,"points"),
    legend.text = element_text(family = "Roboto Condensed", size = 15)
  ) + coord_flip()

pyr

ggsave(filename = here("plot","U.S. vs Russia Age-Sex Pyramid, 2020.png"), plot = pyr, width = 1668, height = 913,units = "px",dpi=300,bg = "white")