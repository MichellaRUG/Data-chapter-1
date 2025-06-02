rm(list=ls())


usethis::use_git_config(user.name = "MichellaRUG", user.email = "m.ligtelijn@rug.nl")

usethis::use_git()        # Initializes git for your project
usethis::use_github()     # Pushes project to GitHub

#Load packages
library(usethis) 
library(readxl)
library(dplyr)
library(glmmTMB)
library(factoextra)
library(ggplot2)
library(piecewiseSEM)
library(ggplot2)
library(DHARMa)
library(emmeans)
library(gridExtra)
library(viridis)
library(gridExtra)
library(grid)
library(mgcv)
library(patchwork)
library(MuMIn)
library(ggeffects)
library(ggpubr)
library(tidyr)


#Load data

df <- read_excel("G:/My Drive/PhD/Data/Big data set/big_data_ch3_310325.xlsx", sheet = 2)


Make modifications to the data
```{r}
# throw out the lines for which there is no biomass data
df2 <- df %>% filter(!is.na(biomass))

# remove un-used columns
df2$airtemp_ave <- NULL
df2$airtemp_sd <- NULL
df2$soiltemp_ave <- NULL
df2$soiltemp_sd <- NULL
df2$comments <- NULL

#current dataset is 371 obs and 22 variables. Remove all NAs
df2 <- na.omit(df2)


### now split 2022 and 2023. I will work from df2 (347 obs.)
df2_2022 <- df2 %>% filter(year == 2022)
df2_2023 <- df2 %>% filter(year == 2023)
```

#Part 1 
First test the effect of time
When making models, the DHARMa plots do not look good at all. So, we use a simple correlation for now
```{r}
cor.test(df2_2022$biomass, df2_2022$weeknr, method = "spearman")
cor.test(df2_2023$biomass, df2_2023$weeknr, method = "spearman")
```


To reinforce the point that time has an effect. Over time you see biomass and environmental variables change. It does not change linear, so there is more than time that affects this. To minimize the effect of time we are going to make smaller seasons. The season division is based on insect biomass dynamics. We do not make the cut-off in a peak, but between peaks to make the seasons similar.

Make the seasons
```{r}
df2 <- df2 %>%
  mutate(season = case_when(
    weeknr < 23 ~ "early",
    weeknr >= 23 & weeknr <= 29 ~ "mid",
    weeknr > 29 ~ "late"
  ))

df2_2022 <- df2_2022 %>%
  mutate(season = case_when(
    weeknr < 23 ~ "early",
    weeknr >= 23 & weeknr <= 29 ~ "mid",
    weeknr > 29 ~ "late"
  ))

df2_2023 <- df2_2023 %>%
  mutate(season = case_when(
    weeknr < 23 ~ "early",
    weeknr >= 23 & weeknr <= 29 ~ "mid",
    weeknr > 29 ~ "late"
  ))

df2$season <- as.factor(df2$season)
df2_2022$season <- as.factor(df2_2022$season)
df2_2023$season <- as.factor(df2_2023$season)
```


Make model for 2022 
```{r}
model_seasons2022 <- glmmTMB(biomass ~ season + (1|meadowid), data = df2_2022, family = gaussian(), REML=FALSE)
summary(model_seasons2022) #AIC=1302.70
plot(simulateResiduals(model_seasons2022, refit=FALSE))


model_seasons2022_log <- glmmTMB(log(biomass+1) ~ season + (1|meadowid), data = df2_2022, family = gaussian(), REML=FALSE)
summary(model_seasons2022_log) #AIC=431.3
plot(simulateResiduals(model_seasons2022_log, refit=FALSE))


model_seasons2022_sqrt <- glmmTMB(sqrt(biomass) ~ season + (1|meadowid), data = df2_2022, family = gaussian(), REML=FALSE)
summary(model_seasons2022_sqrt) #AIC=632.4
plot(simulateResiduals(model_seasons2022_sqrt, refit=FALSE))

AICc(model_seasons2022_sqrt, model_seasons2022_log, model_seasons2022)

#best model so we make REML true
model_seasons2022_log <- glmmTMB(log(biomass+1) ~ season + (1|meadowid), data = df2_2022, family = gaussian(), REML=TRUE)
summary(model_seasons2022_log)
plot(simulateResiduals(model_seasons2022_log, refit=FALSE))

emmeans_results <- emmeans(model_seasons2022_log, pairwise ~ season, adjust = "tukey")
emmeans_results$contrasts
```

Make graph for 2022
```{r}
season_x_pos <- c(early = 21, mid = 25, late = 30)  
y_max <- 100  
y_offset <- 95 

biomass_graph_2022 <- ggplot(df2_2022, aes(x = weeknr, y = biomass)) +
  geom_point(color = "lightgrey") +  
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 15), color = "black", linewidth = 1.2) +  
  geom_vline(xintercept = c(22.5, 29.5), color = "black", linetype = "dashed", linewidth = 1) +  
  labs(x = NULL, y = "Biomass (in g)") +
  theme_minimal() +
  scale_x_continuous(
    limits = c(14, 36), 
    breaks = seq(14, 36, by = 1),
    labels = NULL,  
    sec.axis = sec_axis(
      transform = ~ .,  
      breaks = c(14, 18, 23, 27,31),  
      labels = c("Apr", "May", "Jun", "Jul","Aug")  
    )
  ) +
  scale_y_continuous(limits = c(-10, 110)) +
  annotate("text", x = 36, y = y_max / 2, label = "2022", angle = 90, hjust = 0, fontface = "bold")

biomass_graph_2022

ggsave("biomass2022_plot.png", dpi = 300, width = 8, height = 6)

library(mgcv)

#this part is to check if the k value is the right value. We set it at 15 to avoid over fitting.
fit <- gam(biomass ~ s(weeknr, bs = "cs", k = ), data = df2_2022, method = "REML")
gam.check(fit)
```

Make model for 2023
```{r}
model_seasons2023 <- glmmTMB(biomass ~ season + (1|meadowid), data = df2_2023, family = gaussian(), REML=FALSE)
hist(log(df2_2023$biomass+1), breaks=30)
summary(model_seasons2023) #AIC=1558.2
plot(simulateResiduals(model_seasons2023, refit=FALSE))

res <- simulateResiduals(model_seasons2023, refit=FALSE)
testDispersion(res) #no overdisperson, so we can keep the gaussian. Let's try to improve it a bit by using data transformations.


model_seasons2023_log <- glmmTMB(log(biomass+1) ~ season + (1|meadowid), data = df2_2023, family = gaussian(), REML=FALSE)
summary(model_seasons2023_log) #AIC=480.4
plot(simulateResiduals(model_seasons2023_log, refit=FALSE))

emmeans_results <- emmeans(model_seasons2023_log, pairwise ~ season, adjust = "tukey")
emmeans_results$contrasts


model_seasons2023_sqrt <- glmmTMB(sqrt(biomass) ~ season + (1|meadowid), data = df2_2023, family = gaussian(), REML=FALSE)
summary(model_seasons2023_sqrt) #AIC=717.1	
plot(simulateResiduals(model_seasons2023_sqrt, refit=FALSE))

emmeans_results <- emmeans(model_seasons2023_sqrt, pairwise ~ season, adjust = "tukey")
emmeans_results$contrasts


AICc(model_seasons2023_sqrt, model_seasons2023_log, model_seasons2023)

#sqrt model assumptions look better, but the AIC for the log is much better. I choose the log one.

#best model so we make REML true
model_seasons2023_log <- glmmTMB(log(biomass+1) ~ season + (1|meadowid), data = df2_2023, family = gaussian(), REML=TRUE)
summary(model_seasons2023_log) 
plot(simulateResiduals(model_seasons2023_log, refit=FALSE))

emmeans_results <- emmeans(model_seasons2023_log, pairwise ~ season, adjust = "tukey")
emmeans_results$contrasts
```

Make graph for 2023
```{r}
season_x_pos <- c(early = 21, mid = 25, late = 30)  
y_max <- 100  
y_offset <- 95 

biomass_graph_2023 <- ggplot(df2_2023, aes(x = weeknr, y = biomass)) +
  geom_point(color = "lightgrey") +  
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 15), color = "black", linewidth = 1.2) +  
  geom_vline(xintercept = c(22.5, 29.5), color = "black", linetype = "dashed", linewidth = 1) +  
  labs(x = "Week number", y = "Biomass (in g)") +
  theme_minimal() +
  scale_x_continuous(
    limits = c(14, 36),  
    breaks = seq(14, 36, by = 1),  
    labels = seq(14, 36, by = 1) 
  ) +
  scale_y_continuous(limits = c(-10, 110)) +
  
  annotate("text", x = 36, y = y_max / 2, label = "2023", angle = 90, hjust = 0, fontface = "bold")

biomass_graph_2023

ggsave("biomass2023_plot.png", dpi = 300, width = 8, height = 6)

fit <- gam(biomass ~ s(weeknr, bs = "cs", k = 20), data = df2_2023, method = "REML")
gam.check(fit)
```


Combine the biomass graph for 2022 and 2023
```{r}
# Combine the two plots into one image (on top of each other)
combined_plot <- grid.arrange(biomass_graph_2022, biomass_graph_2023, nrow = 2)

ggsave("G:/My Drive/PhD/Data/Big data set/combined_biomass_graphs.png", plot = combined_plot, width = 8.27, height = 9, units = "in")
```



Make combined plot the Mo way
```{r}
plot_biomass <- ggplot(df2, aes(x = weeknr, y = biomass)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 15), color = "black", linewidth = 1.2) +
  facet_wrap(~ year, ncol = 1, strip.position = "right") +
  labs(x = "Week number", y = "Biomass (in g)", title = NULL) +
  geom_vline(xintercept = c(22.5, 29.5), color = "black", linetype = "dashed", linewidth = 1) +
  theme_bw() +
  theme(strip.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        strip.placement = "outside",
        strip.background = element_blank())

plot_biomass

ggsave("G:/My Drive/PhD/Data/Big data set/combined_plot_biomass.png", plot = plot_biomass, width = 8.27, height = 9, units = "in")
```



This was for all the fields combined, but I also want the graphs for the fields separate. I do not need a model.

First make GPI200_ave a factor
```{r}
df2_2022$GPI200_ave <-as.factor(df2_2022$GPI200_ave)
df2_2023$GPI200_ave <-as.factor(df2_2023$GPI200_ave)
```

2022
```{r}
df2_2022$GPI200_ave <- factor(round(as.numeric(as.character(df2_2022$GPI200_ave)), 4))

color_palette <- c(
  "721.5882" = "#0000FF",          #129014
  "725.2601" = "#00FFFF",          #160077
  "726.4191" = "#00FF7F",          #143023
  "726.4521" = "#7FFF00",          #143016
  "728.6193" = "#FFFF00",          #162027
  "728.9842" = "#FF8000",          #583032
  "729.1374" = "#FF0000",          #160086
  "729.7510" = "#BF0000"           #130005
)

biomass_separate_graph_2022 <- ggplot(df2_2022, aes(x = weeknr, y = biomass, color = GPI200_ave)) +
  geom_point(color = "lightgrey") +  
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE, aes(color = GPI200_ave), linewidth = 1.2) +  
  geom_vline(xintercept = c(22.5, 29.5), color = "black", linetype = "dashed", linewidth = 1) +  
  labs(title = "2022", x = "Week number", y = "Biomass (in g)") +
  scale_color_manual(values = color_palette) + 
  theme_minimal() +
  
  scale_x_continuous(
    limits = c(14, 35),  
    breaks = seq(14, 35, by = 1),
    labels = seq(14, 35, by = 1),
    sec.axis = sec_axis(
      transform = ~ ., 
      breaks = c(14, 18, 23, 27, 31), 
      labels = c("Apr", "May", "Jun", "Jul", "Aug")
    )
  ) +
  scale_y_continuous(limits = c(-10, 110))

biomass_separate_graph_2022

ggsave("biomass_separate_graph_2022.png", dpi = 300, width = 8, height = 6)
```

2023
```{r}
df2_2023$GPI200_ave <- factor(round(as.numeric(as.character(df2_2023$GPI200_ave)), 4))
#this was to match the colors between years. Not yet updated 
color_palette <- c(                #2022     #2023
  "722.8824" = "#0000FF",          #129014  #129014
  "723.1764" = "#007FFF",          #        #100028   
  "723.4091" = "#00FFFF",          #        #240063
  "725.2613" = "#00FF7F",          #        #660024
  "725.6838" = "#00FF00",          #160077  #160077
  "726.5923" = "#7FFF00",          #143023  #143016  
  "726.7349" = "#FFFF00",          #143016  #143023
  "728.4129" = "#FFBF00",          #162027  #583032
  "729.4211" = "#FF8000",          #583032  #128006
  "729.5847" = "#FF4000",          #160086  #162027
  "729.8442" = "#FF0000",          #130005  #160086
  "730.1301" = "#BF0000"                    #130005
)


biomass_separate_graph_2023 <- ggplot(df2_2023, aes(x = weeknr, y = biomass, color = GPI200_ave)) +
  geom_point(color = "lightgrey") +  
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE, aes(color = GPI200_ave), linewidth = 1.2) + 
  geom_vline(xintercept = c(22.5, 29.5), color = "black", linetype = "dashed", linewidth = 1) +  
  labs(title = "2023", x = "Week number", y = "Biomass (in g)") +
  scale_color_manual(values = color_palette) +  
  theme_minimal() +
  
  scale_x_continuous(
    limits = c(14, 35),  
    breaks = seq(14, 35, by = 1),
    labels = seq(14, 35, by = 1),
    sec.axis = sec_axis(
      trans = ~ ., 
      breaks = c(14, 18, 23, 27, 31), 
      labels = c("Apr", "May", "Jun", "Jul", "Aug")
    )
  ) +
  scale_y_continuous(limits = c(-10, 110))

biomass_separate_graph_2023

ggsave("biomass_separate_graph_2023.png", dpi = 300, width = 8, height = 6)
```

Combine the graphs
```{r}
combined_plot <- grid.arrange(biomass_separate_graph_2022, biomass_separate_graph_2023, nrow = 2)

ggsave("G:/My Drive/PhD/Data/Big data set/combined_biomass_separate_graphs.png", plot = combined_plot, width = 8.27, height = 7, units = "in")
```

For the results I want to know the average biomass per year
```{r}
mean(df2_2022$biomass)
mean(df2_2023$biomass)
```



Boxplot of the biomass per season per year and combine it

2022
```{r}
df2_2022$season <- factor(df2_2022$season, levels = c("early", "mid", "late"))

y_max <- 100
y_arrow <- y_max - 15  
x_max <- max(as.numeric(df2_2022$season))

boxplot2022 <- ggplot(df2_2022, aes(x = season, y = biomass)) +
  geom_boxplot(fill = "lightgrey", color = "black") +
  theme_minimal() +
  labs(
    title = NULL,
    x = NULL,
    y = "Biomass (g)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  # Early vs Mid
  geom_bracket(
    xmin = 1, xmax = 2, y.position = y_arrow + 8, label = "<0.01",
    tip.length = 0.01, label.size = 3, fontface = "italic", size = 1.1
  ) +
  # Early vs Late
  geom_bracket(
    xmin = 1, xmax = 3, y.position = y_arrow + 1, label = "<0.01",
    tip.length = 0.01, label.size = 3, fontface = "italic", size = 1.1
  ) +
  # Mid vs Late
  geom_bracket(
    xmin = 2, xmax = 3, y.position = y_arrow - 6, label = "0.5876",
    tip.length = 0.01, label.size = 3, fontface = "italic", size = 1.1
  ) +
  annotate("text",
           x = x_max + 0.5,
           y = y_max - 2,  
           label = "2022",
           hjust = 1,
           size = 5,
           fontface = "bold") +
  scale_y_continuous(limits = c(0, 100))

boxplot2022
```

2023
```{r}
df2_2023$season <- factor(df2_2023$season, levels = c("early", "mid", "late"))

y_max <- 100
y_arrow <- y_max - 15  
x_max <- max(as.numeric(df2_2023$season))

boxplot2023 <- ggplot(df2_2023, aes(x = season, y = biomass)) +
  geom_boxplot(fill = "lightgrey", color = "black") +
  theme_minimal() +
  labs(
    title = NULL,
    x = "Season",
    y = "Biomass (g)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(),
    axis.ticks.x = element_blank()
  ) +
  # Early vs Mid
  geom_bracket(
    xmin = 1, xmax = 2, y.position = y_arrow + 8, label = "<0.01",
    tip.length = 0.01, label.size = 3, fontface = "italic", size = 1.1
  ) +
  # Early vs Late
  geom_bracket(
    xmin = 1, xmax = 3, y.position = y_arrow + 1, label = "<0.01",
    tip.length = 0.01, label.size = 3, fontface = "italic", size = 1.1
  ) +
  # Mid vs Late
  geom_bracket(
    xmin = 2, xmax = 3, y.position = y_arrow - 6, label = "0.5876",
    tip.length = 0.01, label.size = 3, fontface = "italic", size = 1.1
  ) +
  annotate("text",
           x = x_max + 0.5,
           y = y_max - 2,  
           label = "2023",
           hjust = 1,
           size = 5,
           fontface = "bold") +
  scale_y_continuous(limits = c(0, 100))

boxplot2023
```

Combine the graphs
```{r}
combined_boxplot <- ggarrange(
  boxplot2022, boxplot2023,
  ncol = 1, nrow = 2,
  labels = c("A", "B"),
  align = "v"
)

combined_boxplot

ggsave("G:/My Drive/PhD/Data/Big data set/combined_biomass_boxplots.png", plot = combined_boxplot, width = 7, height = 10, units = "in")
```

Same boxplots, but the Mo method
```{r}
brackets_df <- data.frame(
  year = c(2022, 2022, 2022, 2023, 2023, 2023),
  xmin = c(1, 1, 2, 1, 1, 2),
  xmax = c(2, 3, 3, 2, 3, 3),
  y.position = c(105, 115, 125, 105, 115, 125),  # adjust these based on your data range
  label = c("<0.01", "<0.01", "0.59", "<0.001", "<0.01", "<0.01")
)

# Ensure season is ordered
df2$season <- factor(df2$season, levels = c("early", "mid", "late"))

# Base plot
boxplot_biomass <- ggplot(df2, aes(x = season, y = biomass)) +
  geom_boxplot(outlier.alpha = 0.3, fill = "gray80") +
  facet_wrap(~ year, ncol = 1, strip.position = "right") +
  geom_bracket(
    data = brackets_df,
    aes(xmin = xmin, xmax = xmax, y.position = y.position, label = label),
    inherit.aes = FALSE,
    tip.length = 0.01, label.size = 3, fontface = "italic", size = 1.1
  ) +
  labs(x = "Season", y = "Biomass (in g)", title = NULL) +
  theme_bw() +
  theme(strip.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        strip.placement = "outside",
        strip.background = element_blank())

boxplot_biomass

ggsave("G:/My Drive/PhD/Data/Big data set/combined_boxplot_biomass.png", plot = boxplot_biomass, width = 8, height = 11, units = "in")
```




Now we have to make similar graphs for the environmental variables
Soil moisture
2022
```{r}
season_x_pos <- c(early = 21, mid = 25, late = 30)

sm_graph_2022 <- ggplot(df2_2022, aes(x = weeknr, y = sm_ave)) +
  geom_point(color = "lightgrey") + 
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 15), color = "black", linewidth = 1.2) +  
  geom_vline(xintercept = c(22.5, 29.5), color = "black", linetype = "dashed", linewidth = 1) + 
  labs(title = "2022", x = "Week number", y = "average soil moisture (in %)") +
  theme_minimal() +
  scale_x_continuous(
    limits = c(14, 35),
    breaks = seq(14, 35, by = 1),
    labels = seq(14, 35, by = 1),
    sec.axis = sec_axis(
      trans = ~ .,
      breaks = c(14, 18, 23, 27, 31),
      labels = c("Apr", "May", "Jun", "Jul", "Aug")
    )
  ) +
  scale_y_continuous(limits = c(0, 90))

sm_graph_2022

ggsave("sm2022_plot.png", dpi = 300, width = 8, height = 6)
```

2023
```{r}
season_x_pos <- c(early = 21, mid = 25, late = 30)

sm_graph_2023 <- ggplot(df2_2023, aes(x = weeknr, y = sm_ave)) +
  geom_point(color = "lightgrey") + 
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 15), color = "black", linewidth = 1.2) +  
  geom_vline(xintercept = c(22.5, 29.5), color = "black", linetype = "dashed", linewidth = 1) +  
  labs(title = "2023", x = "Week number", y = "average soil moisture (in %)") +
  theme_minimal() +
  scale_x_continuous(
    limits = c(14, 35),
    breaks = seq(14, 35, by = 1),
    labels = seq(14, 35, by = 1),
    sec.axis = sec_axis(
      trans = ~ .,
      breaks = c(14, 18, 23, 27, 31),
      labels = c("Apr", "May", "Jun", "Jul", "Aug")
    )
  ) +
  scale_y_continuous(limits = c(0, 90))

sm_graph_2023

ggsave("sm2023_plot.png", dpi = 300, width = 8, height = 6)
```

Combine the graphs
```{r}
combined_plot <- grid.arrange(sm_graph_2022, sm_graph_2023, nrow = 2)

ggsave("G:/My Drive/PhD/Data/Big data set/combined_sm_graphs.png", plot = combined_plot, width = 8.27, height = 7, units = "in")
```


Vegetation height
2022
```{r}
season_x_pos <- c(early = 21, mid = 25, late = 30)

veghgt_graph_2022 <- ggplot(df2_2022, aes(x = weeknr, y = veghgt_ave)) +
  geom_point(color = "lightgrey") +  
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 15), color = "black", linewidth = 1.2) +  
  geom_vline(xintercept = c(22.5, 29.5), color = "black", linetype = "dashed", linewidth = 1) +  
  labs(title = "2022", x = "Week number", y = "average vegetation height (in cm)") +
  theme_minimal() +
  scale_x_continuous(
    limits = c(14, 35),
    breaks = seq(14, 35, by = 1),
    labels = seq(14, 35, by = 1),
    sec.axis = sec_axis(
      transform = ~ .,
      breaks = c(14, 18, 23, 27, 31),
      labels = c("Apr", "May", "Jun", "Jul", "Aug")
    )
  ) +
  scale_y_continuous(limits = c(0, 70))

veghgt_graph_2022

ggsave("veghgt2022_plot.png", dpi = 300, width = 8, height = 6)
```

2023
```{r}
season_x_pos <- c(early = 21, mid = 25, late = 30)

veghgt_graph_2023 <- ggplot(df2_2023, aes(x = weeknr, y = veghgt_ave)) +
  geom_point(color = "lightgrey") +  
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 15), color = "black", linewidth = 1.2) +  
  geom_vline(xintercept = c(22.5, 29.5), color = "black", linetype = "dashed", linewidth = 1) +  
  labs(title = "2023", x = "Week number", y = "average vegetation height (in cm)") +
  theme_minimal() +
  scale_x_continuous(
    limits = c(14, 35),
    breaks = seq(14, 35, by = 1),
    labels = seq(14, 35, by = 1),
    sec.axis = sec_axis(
      transform = ~ .,
      breaks = c(14, 18, 23, 27, 31),
      labels = c("Apr", "May", "Jun", "Jul", "Aug")
    )
  ) +
  scale_y_continuous(limits = c(0, 70))

veghgt_graph_2023

ggsave("veghgt2023_plot.png", dpi = 300, width = 8, height = 6)
```

Combine the graphs
```{r}
combined_plot <- grid.arrange(veghgt_graph_2022, veghgt_graph_2023, nrow = 2)

ggsave("G:/My Drive/PhD/Data/Big data set/combined_veghgt_graphs.png", plot = combined_plot, width = 8.27, height = 7, units = "in")
```


Soil resistance
2022
```{r}
season_x_pos <- c(early = 21, mid = 25, late = 30)

soilres_graph_2022 <- ggplot(df2_2022, aes(x = weeknr, y = soilres_ave)) +
  geom_point(color = "lightgrey") +  
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 15), color = "black", linewidth = 1.2) +  
  geom_vline(xintercept = c(22.5, 29.5), color = "black", linetype = "dashed", linewidth = 1) + 
  labs(
    title = "2022",
    x = "Week number",
    y = expression("average soil resistance (in kg/cm"^2*")")
  ) +
  theme_minimal() +
  scale_x_continuous(
    limits = c(14, 35),
    breaks = seq(14, 35, by = 1),
    labels = seq(14, 35, by = 1),
    sec.axis = sec_axis(
      trans = ~ .,
      breaks = c(14, 18, 23, 27, 31),
      labels = c("Apr", "May", "Jun", "Jul", "Aug")
    )
  ) +
  scale_y_continuous(limits = c(0, 70))

soilres_graph_2022

ggsave("soilres2022_plot.png", dpi = 300, width = 8, height = 6)
```

2023
```{r}
season_x_pos <- c(early = 21, mid = 25, late = 30)

soilres_graph_2023 <- ggplot(df2_2023, aes(x = weeknr, y = soilres_ave)) +
  geom_point(color = "lightgrey") + 
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 15), color = "black", linewidth = 1.2) +
  geom_vline(xintercept = c(22.5, 29.5), color = "black", linetype = "dashed", linewidth = 1) +
  labs(
    title = "2023",
    x = "Week number",
    y = expression("average soil resistance (in kg/cm"^2*")")
  ) +
  theme_minimal() +
  scale_x_continuous(
    limits = c(14, 35),
    breaks = seq(14, 35, by = 1),
    labels = seq(14, 35, by = 1),
    sec.axis = sec_axis(
      trans = ~ .,
      breaks = c(14, 18, 23, 27, 31),
      labels = c("Apr", "May", "Jun", "Jul", "Aug")
    )
  ) +
  scale_y_continuous(limits = c(0, 70))

soilres_graph_2023

ggsave("soilres2023_plot.png", dpi = 300, width = 8, height = 6)
```

Combine the graphs
```{r}
combined_plot <- grid.arrange(soilres_graph_2022, soilres_graph_2023, nrow = 2)

ggsave("G:/My Drive/PhD/Data/Big data set/combined_soilres_graphs.png", plot = combined_plot, width = 8.27, height = 7, units = "in")
```

Calculate correlation between soil moisture and soil resistance per year
```{r}
cor(df2_2022$soilres_ave, df2_2022$sm_ave, use = "complete.obs", method = "pearson")
cor(df2_2023$soilres_ave, df2_2023$sm_ave, use = "complete.obs", method = "pearson")
```


Make very big graph with vegetation height, soil moisture, soil resistance, and mowing in one.

```{r}
df2$year <- as.factor(df2$year)

df_long <- df2 %>%
  pivot_longer(cols = c(veghgt_ave, sm_ave, soilres_ave, knmitemp_ave),
               names_to = "variable", values_to = "value") %>%
  mutate(variable = recode(variable,
                           veghgt_ave = "average vegetation height (in cm)",
                           sm_ave = "average soil moisture (in %)",
                           soilres_ave = "average soil resistance (in kg/cm²)",
                           knmitemp_ave = "average air temperature (in °C)")) %>%
  mutate(variable = factor(variable,
                           levels = c("average vegetation height (in cm)",
                                      "average soil moisture (in %)",
                                      "average soil resistance (in kg/cm²)",
                                      "average air temperature (in °C)")))

plot_all_env <- ggplot(df_long, aes(x = weeknr, y = value)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "gam",
              formula = y ~ s(x, bs = "cs", k = 15),
              color = "black", linewidth = 1.1) +
  geom_vline(xintercept = c(22.5, 29.5),
             color = "black", linetype = "dashed", linewidth = 1) +
  facet_grid(rows = vars(variable),
             cols = vars(year),
             scales = "free_y",
             switch = "y") +
  labs(x = "Week number", y = NULL) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    strip.background = element_blank(),
    strip.placement = "outside",
    panel.spacing = unit(1, "lines")
  )

plot_all_env


ggsave("G:/My Drive/PhD/Data/Big data set/plot_all_env.png", plot = plot_all_env, width = 10, height = 14, units = "in")
```



Make comparison between 2022 and 2023 per season for the different variables
Add seasons to dataset df2

Make the seasons
```{r}
df2 <- df2 %>%
  mutate(season = case_when(
    weeknr < 23 ~ "early",
    weeknr >= 23 & weeknr <= 29 ~ "mid",
    weeknr > 29 ~ "late"
  ))

df2$season <- as.factor(df2$season)
```

Model biomass
```{r}
model_biomass <- glmmTMB(biomass ~ season * year, data = df2, family = gaussian(), REML=FALSE)
summary(model_biomass) #AIC=2876.1
plot(simulateResiduals(model_biomass, refit=FALSE))

model_biomass_log <- glmmTMB(log(biomass+1) ~ season * year, data = df2, family = gaussian(), REML=FALSE)
summary(model_biomass_log) #AIC=912.4
plot(simulateResiduals(model_biomass_log, refit=FALSE))

emmeans_results <- emmeans(model_biomass_log, pairwise ~ season, adjust = "tukey")
emmeans_results$contrasts


model_biomass_sqrt <- glmmTMB(sqrt(biomass) ~ season * year, data = df2, family = gaussian(), REML=FALSE)
summary(model_biomass_sqrt) #AIC=1354.9
plot(simulateResiduals(model_biomass_sqrt, refit=FALSE))

emmeans_results <- emmeans(model_biomass_sqrt, pairwise ~ year, adjust = "tukey")
emmeans_results$contrasts


AICc(model_biomass_sqrt, model_biomass_log, model_biomass)



#best model so we make REML true
model_biomass_log <- glmmTMB(log(biomass+1) ~ season * year, data = df2, family = gaussian(), REML=TRUE)
summary(model_biomass_log) #AIC=926.9
plot(simulateResiduals(model_biomass_log, refit=FALSE))

emmeans_results <- emmeans(model_biomass_log, pairwise ~ year|season, adjust = "tukey")
emmeans_results$contrasts
```

Graph biomass
```{r}
df2$season <- factor(df2$season, levels = c("early", "mid", "late"))

y_max <- max(df2$biomass, na.rm = TRUE)
y_arrow <- y_max + 10

boxplot_biomass <- ggplot(df2, aes(x = season, y = biomass)) +
  geom_boxplot(aes(fill = factor(year)), color = "black", position = position_dodge(width = 0.8)) +  
  scale_fill_manual(values = c("2022" = "lightblue", "2023" = "lightgreen")) +  
  theme_minimal() +
  labs(
    title = NULL,
    x = "Season",  
    y = "Biomass (g)",
    fill = "Year"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(),  
    axis.ticks.x = element_line()  
  ) +
  
  # Early
  geom_bracket(
    xmin = 0.75, xmax = 1.25, y.position = y_arrow, label = "0.2824",
    tip.length = 0.01, label.size = 3, fontface = "italic", size = 0.8
  ) +
  
  # Mid
  geom_bracket(
    xmin = 1.75, xmax = 2.25, y.position = y_arrow, label = "<0.01",
    tip.length = 0.01, label.size = 3, fontface = "italic", size = 0.8
  ) +
  
  # Late
  geom_bracket(
    xmin = 2.75, xmax = 3.25, y.position = y_arrow, label = "0.1427",
    tip.length = 0.01, label.size = 3, fontface = "italic", size = 0.8
  )

boxplot_biomass

ggsave("G:/My Drive/PhD/Data/Big data set/biomass_season.png", plot = boxplot_biomass, width = 7, height = 10, units = "in")
```



Model soil moisture
```{r}
model_sm <- glmmTMB(sm_ave ~ season * year, data = df2, family = gaussian(), REML=FALSE)
summary(model_sm) #AIC=2854.8
plot(simulateResiduals(model_sm, refit=FALSE))

model_sm_log <- glmmTMB(log(sm_ave+1) ~ season * year, data = df2, family = gaussian(), REML=FALSE)
summary(model_sm_log) #AIC=527.0
plot(simulateResiduals(model_sm_log, refit=FALSE))

model_sm_sqrt <- glmmTMB(sqrt(sm_ave) ~ season * year, data = df2, family = gaussian(), REML=FALSE)
summary(model_sm_sqrt) #AIC=1171.7
plot(simulateResiduals(model_sm_sqrt, refit=FALSE))

AICc(model_sm_sqrt, model_sm_log, model_sm)



#best model so we make REML true
model_sm_log <- glmmTMB(log(sm_ave+1) ~ season * year, data = df2, family = gaussian(), REML=TRUE)
summary(model_sm_log) #AIC=548.2
plot(simulateResiduals(model_sm_log, refit=FALSE))

emmeans_results <- emmeans(model_sm_log, pairwise ~ year|season, adjust = "tukey")
emmeans_results$contrasts
```

Graph soil moisture
```{r}
df2$season <- factor(df2$season, levels = c("early", "mid", "late"))

# Define y_arrow for positioning the brackets
y_max <- max(df2$sm_ave, na.rm = TRUE)
y_arrow <- y_max + 10

# Create the boxplot
boxplot_sm <- ggplot(df2, aes(x = season, y = sm_ave)) +
  geom_boxplot(aes(fill = factor(year)), color = "black", position = position_dodge(width = 0.8)) +  # Ensure boxes are separated
  scale_fill_manual(values = c("2022" = "lightblue", "2023" = "lightgreen")) +  # Custom color for years
  theme_minimal() +
  labs(
    title = NULL,
    x = "Season",  # Label the x-axis
    y = "average soil moisture (%)",
    fill = "Year"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(),  # Ensure x-axis labels are visible
    axis.ticks.x = element_line()  # Ensure x-axis ticks are visible
  ) +
  
  # Bracket: Early
  geom_bracket(
    xmin = 0.75, xmax = 1.25, y.position = y_arrow, label = "<0.01",
    tip.length = 0.01, label.size = 3, fontface = "italic", size = 0.8
  ) +
  
  # Bracket: Mid
  geom_bracket(
    xmin = 1.75, xmax = 2.25, y.position = y_arrow, label = "<0.01",
    tip.length = 0.01, label.size = 3, fontface = "italic", size = 0.8
  ) +
  
  # Bracket: Late
  geom_bracket(
    xmin = 2.75, xmax = 3.25, y.position = y_arrow, label = "<0.01",
    tip.length = 0.01, label.size = 3, fontface = "italic", size = 0.8
  )

boxplot_sm

ggsave("G:/My Drive/PhD/Data/Big data set/sm_season.png", plot = boxplot_sm, width = 7, height = 10, units = "in")
```




Model soil resistance
```{r}
model_soilres <- glmmTMB(soilres_ave ~ season * year, data = df2, family = gaussian(), REML=FALSE)
summary(model_soilres) #AIC=2723.1
plot(simulateResiduals(model_soilres, refit=FALSE))

model_soilres_log <- glmmTMB(log(soilres_ave+1) ~ season * year, data = df2, family = gaussian(), REML=FALSE)
summary(model_soilres_log) #AIC=582.6
plot(simulateResiduals(model_soilres_log, refit=FALSE))

model_soilres_sqrt <- glmmTMB(sqrt(soilres_ave) ~ season * year, data = df2, family = gaussian(), REML=FALSE)
summary(model_soilres_sqrt) #AIC=1144.2
plot(simulateResiduals(model_soilres_sqrt, refit=FALSE))

AICc(model_soilres_sqrt, model_soilres_log, model_soilres)



#best model so we make REML true
model_soilres_log <- glmmTMB(log(soilres_ave+1) ~ season * year, data = df2, family = gaussian(), REML=TRUE)
summary(model_soilres_log) #AIC=602.8
plot(simulateResiduals(model_soilres_log, refit=FALSE))

emmeans_results <- emmeans(model_soilres_log, pairwise ~ year|season, adjust = "tukey")
emmeans_results$contrasts
```

Graph soil resistance
```{r}
df2$season <- factor(df2$season, levels = c("early", "mid", "late"))

y_max <- max(df2$soilres_ave, na.rm = TRUE)
y_arrow <- y_max + 10

boxplot_soilres <- ggplot(df2, aes(x = season, y = soilres_ave)) +
  geom_boxplot(aes(fill = factor(year)), color = "black", position = position_dodge(width = 0.8)) + 
  scale_fill_manual(values = c("2022" = "lightblue", "2023" = "lightgreen")) +  
  theme_minimal() +
  labs(
    title = NULL,
    x = "Season",  
    y = "average soil resistance (in N/kg2)",
    fill = "Year"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(), 
    axis.ticks.x = element_line() 
  ) +
  
  # Early
  geom_bracket(
    xmin = 0.75, xmax = 1.25, y.position = y_arrow, label = "<0.01",
    tip.length = 0.01, label.size = 3, fontface = "italic", size = 0.8
  ) +
  
  # Mid
  geom_bracket(
    xmin = 1.75, xmax = 2.25, y.position = y_arrow, label = "0.0597",
    tip.length = 0.01, label.size = 3, fontface = "italic", size = 0.8
  ) +
  
  # Late
  geom_bracket(
    xmin = 2.75, xmax = 3.25, y.position = y_arrow, label = "<0.01",
    tip.length = 0.01, label.size = 3, fontface = "italic", size = 0.8
  )

boxplot_soilres

ggsave("G:/My Drive/PhD/Data/Big data set/soilres_season.png", plot = boxplot_soilres, width = 7, height = 10, units = "in")
```




Model vegetation height
```{r}
model_veghgt <- glmmTMB(veghgt_ave ~ season * year, data = df2, family = gaussian(), REML=FALSE)
summary(model_veghgt) #AIC=2587.2
plot(simulateResiduals(model_veghgt, refit=FALSE))

model_veghgt_log <- glmmTMB(log(veghgt_ave+1) ~ season * year, data = df2, family = gaussian(), REML=FALSE)
summary(model_veghgt_log) #AIC=583.9
plot(simulateResiduals(model_veghgt_log, refit=FALSE))

model_veghgt_sqrt <- glmmTMB(sqrt(veghgt_ave) ~ season * year, data = df2, family = gaussian(), REML=FALSE)
summary(model_veghgt_sqrt) #AIC=1085.5
plot(simulateResiduals(model_veghgt_sqrt, refit=FALSE))

AICc(model_veghgt_sqrt, model_veghgt_log, model_veghgt)



#best model so we make REML true
model_veghgt_log <- glmmTMB(log(veghgt_ave+1) ~ season * year, data = df2, family = gaussian(), REML=TRUE)
summary(model_veghgt_log) #AIC=604
plot(simulateResiduals(model_veghgt_log, refit=FALSE))

emmeans_results <- emmeans(model_veghgt_log, pairwise ~ year|season, adjust = "tukey")
emmeans_results$contrasts
```

Graph vegetation height
```{r}
df2$season <- factor(df2$season, levels = c("early", "mid", "late"))

y_max <- max(df2$veghgt_ave, na.rm = TRUE)
y_arrow <- y_max + 10

boxplot_veghgt <- ggplot(df2, aes(x = season, y = veghgt_ave)) +
  geom_boxplot(aes(fill = factor(year)), color = "black", position = position_dodge(width = 0.8)) +  
  scale_fill_manual(values = c("2022" = "lightblue", "2023" = "lightgreen")) +  
  theme_minimal() +
  labs(
    title = NULL,
    x = "Season",  
    y = "average vegetation height (in cm)",
    fill = "Year"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(), 
    axis.ticks.x = element_line()  
  ) +
  
  # Early
  geom_bracket(
    xmin = 0.75, xmax = 1.25, y.position = y_arrow, label = "<0.01",
    tip.length = 0.01, label.size = 3, fontface = "italic", size = 0.8
  ) +
  
  # Mid
  geom_bracket(
    xmin = 1.75, xmax = 2.25, y.position = y_arrow, label = "0.0752",
    tip.length = 0.01, label.size = 3, fontface = "italic", size = 0.8
  ) +
  
  # Late
  geom_bracket(
    xmin = 2.75, xmax = 3.25, y.position = y_arrow, label = "0.0711",
    tip.length = 0.01, label.size = 3, fontface = "italic", size = 0.8
  )

boxplot_veghgt

ggsave("G:/My Drive/PhD/Data/Big data set/veghgt_season.png", plot = boxplot_veghgt, width = 7, height = 10, units = "in")
```



Model mowing
```{r}
model_mowing <- glmmTMB(mowing ~ season * year, data = df2, family = gaussian(), REML=FALSE)
summary(model_mowing) #AIC=1822.1
plot(simulateResiduals(model_mowing, refit=FALSE))

model_mowing_log <- glmmTMB(log(mowing+1) ~ season * year, data = df2, family = gaussian(), REML=FALSE)
summary(model_mowing_log) #AIC=730.5
plot(simulateResiduals(model_mowing_log, refit=FALSE))

model_mowing_sqrt <- glmmTMB(sqrt(mowing) ~ season * year, data = df2, family = gaussian(), REML=FALSE)
summary(model_mowing_sqrt) #AIC=905.0
plot(simulateResiduals(model_mowing_sqrt, refit=FALSE))

AICc(model_mowing_sqrt, model_mowing_log, model_mowing)


#best model so we make REML true
model_mowing_log <- glmmTMB(log(mowing+1) ~ season * year, data = df2, family = gaussian(), REML=TRUE)
summary(model_mowing_log) 
plot(simulateResiduals(model_mowing_log, refit=FALSE))

emmeans_results <- emmeans(model_mowing_log, pairwise ~ year|season, adjust = "tukey")
emmeans_results$contrasts
```

Graph mowing
```{r}
df2$season <- factor(df2$season, levels = c("early", "mid", "late"))

# Define y_arrow for positioning the brackets
y_max <- max(df2$mowing, na.rm = TRUE)
y_arrow <- y_max + 2

# Create the boxplot
boxplot_mowing <- ggplot(df2, aes(x = season, y = mowing)) +
  geom_boxplot(aes(fill = factor(year)), color = "black", position = position_dodge(width = 0.8)) +  # Ensure boxes are separated
  scale_fill_manual(values = c("2022" = "lightblue", "2023" = "lightgreen")) +  # Custom color for years
  theme_minimal() +
  labs(
    title = NULL,
    x = "Season",  # Label the x-axis
    y = "Weeks since mowing",
    fill = "Year"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(),  # Ensure x-axis labels are visible
    axis.ticks.x = element_line()  # Ensure x-axis ticks are visible
  ) +
  
  # Bracket: Early
  geom_bracket(
    xmin = 0.75, xmax = 1.25, y.position = y_arrow, label = "0.5140",
    tip.length = 0.01, label.size = 3, fontface = "italic", size = 0.8
  ) +
  
  # Bracket: Mid
  geom_bracket(
    xmin = 1.75, xmax = 2.25, y.position = y_arrow, label = "0.0367",
    tip.length = 0.01, label.size = 3, fontface = "italic", size = 0.8
  ) +
  
  # Bracket: Late
  geom_bracket(
    xmin = 2.75, xmax = 3.25, y.position = y_arrow, label = "0.5890",
    tip.length = 0.01, label.size = 3, fontface = "italic", size = 0.8
  ) +
  theme_bw()

boxplot_mowing

ggsave("G:/My Drive/PhD/Data/Big data set/mowing_season.png", plot = boxplot_mowing, width = 7, height = 7, units = "in")
```



Make the mowing model and graph again, but this time with only the fields that were sampled in both years

Make selection dataset. exclude fields that were not sampled in both years
```{r}
df2_selection <- df2[df2$meadowid != "100028" &
                       df2$meadowid != "128006" &
                       df2$meadowid != "660024" &
                       df2$meadowid != "240063", ]
```


Model mowing with selection dataset
```{r}
model_mowing <- glmmTMB(mowing ~ season * year, data = df2_selection, family = gaussian(), REML=FALSE)
summary(model_mowing) #AIC=1466.6
plot(simulateResiduals(model_mowing, refit=FALSE))

model_mowing_log <- glmmTMB(log(mowing+1) ~ season * year, data = df2_selection, family = gaussian(), REML=FALSE)
summary(model_mowing_log) #AIC=592.9
plot(simulateResiduals(model_mowing_log, refit=FALSE))

model_mowing_sqrt <- glmmTMB(sqrt(mowing) ~ season * year, data = df2_selection, family = gaussian(), REML=FALSE)
summary(model_mowing_sqrt) #AIC=733.7
plot(simulateResiduals(model_mowing_sqrt, refit=FALSE))

AICc(model_mowing_sqrt, model_mowing_log, model_mowing)


#best model so we make REML true
model_mowing_log <- glmmTMB(log(mowing+1) ~ season * year, data = df2_selection, family = gaussian(), REML=TRUE)
summary(model_mowing_log) 
plot(simulateResiduals(model_mowing_log, refit=FALSE))

emmeans_results <- emmeans(model_mowing_log, pairwise ~ year|season, adjust = "tukey")
emmeans_results$contrasts
```

Graph mowing with selection dataset
```{r}
df2_selection$season <- factor(df2_selection$season, levels = c("early", "mid", "late"))

# Define y_arrow for positioning the brackets
y_max <- max(df2_selection$mowing, na.rm = TRUE)
y_arrow <- y_max + 2

# Create the boxplot
boxplot_mowing_selection <- ggplot(df2_selection, aes(x = season, y = mowing)) +
  geom_boxplot(aes(fill = factor(year)), color = "black", position = position_dodge(width = 0.8)) +  # Ensure boxes are separated
  scale_fill_manual(values = c("2022" = "lightblue", "2023" = "lightgreen")) +  # Custom color for years
  theme_minimal() +
  labs(
    title = NULL,
    x = "Season",  # Label the x-axis
    y = "Weeks since mowing",
    fill = "Year"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(),  # Ensure x-axis labels are visible
    axis.ticks.x = element_line()  # Ensure x-axis ticks are visible
  ) +
  
  # Bracket: Early
  geom_bracket(
    xmin = 0.75, xmax = 1.25, y.position = y_arrow, label = "0.1537",
    tip.length = 0.01, label.size = 3, fontface = "italic", size = 0.8
  ) +
  
  # Bracket: Mid
  geom_bracket(
    xmin = 1.75, xmax = 2.25, y.position = y_arrow, label = "0.1467",
    tip.length = 0.01, label.size = 3, fontface = "italic", size = 0.8
  ) +
  
  # Bracket: Late
  geom_bracket(
    xmin = 2.75, xmax = 3.25, y.position = y_arrow, label = "0.7415",
    tip.length = 0.01, label.size = 3, fontface = "italic", size = 0.8
  ) +
  theme_bw()

boxplot_mowing_selection

ggsave("G:/My Drive/PhD/Data/Big data set/mowing_season_selection.png", plot = boxplot_mowing_selection, width = 7, height = 7, units = "in")
```


