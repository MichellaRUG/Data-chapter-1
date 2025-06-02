rm(list=ls())

usethis::use_git_config(user.name = "MichellaRUG", user.email = "m.ligtelijn@rug.nl")
usethis::create_github_token()     # Copy the token from browser
gitcreds::gitcreds_set()           # Paste the token in R
usethis::use_git()                 # Initialize Git (if not yet done)
usethis::use_github()              # Create & push repo to GitHub

#run the following 3 lines IN THE CONSOLE to update the newest version of the code to the GitHub website
system("git add .")
system('git commit -m "last version"')
system("git push")



#Load packages
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
```

Load data
```{r}
df <- read_excel("G:/My Drive/PhD/Data/Big data set/big_data_ch3_310325.xlsx", sheet = 2)
```

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

Make the seasons
```{r}
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

df2_2022$season <- as.factor(df2_2022$season)
df2_2023$season <- as.factor(df2_2023$season)
```





#Part II
This part is about how management (LUI/GPI) affect the biomass per season. I want to make very similar graphs as before and the analysis is also very similar. 

There is a difference between seasons, so I will make separete plots for every season. First make three new datasets
```{r}
df2_2022_early <- subset(df2_2022, weeknr >= 14 & weeknr <= 22)
df2_2022_mid   <- subset(df2_2022, weeknr >= 23 & weeknr <= 29)
df2_2022_late  <- subset(df2_2022, weeknr >= 30 & weeknr <= 35)

df2_2023_early <- subset(df2_2023, weeknr >= 15 & weeknr <= 22)
df2_2023_mid   <- subset(df2_2023, weeknr >= 23 & weeknr <= 29)
df2_2023_late  <- subset(df2_2023, weeknr >= 30 & weeknr <= 34)
```


This code is a lot of try outs. See below for the finals ones.
```{r}
model_LUI2022 <- glmmTMB(biomass ~ LUI2016 + season, data = df2_2022, family = gaussian(), REML=FALSE)
summary(model_LUI2022) #AIC=1301.1
plot(simulateResiduals(model_LUI2022, refit=FALSE))

res <- simulateResiduals(model_LUI2022, refit=FALSE)
testDispersion(res) #no overdisperson, so we can keep the gaussian. Let's try to improve it a bit by using data transformations.

model_LUI2022_sqrt <- glmmTMB(sqrt(biomass) ~ LUI2016 + season, data = df2_2022, family = gaussian(), REML=FALSE)
summary(model_LUI2022_sqrt) #AIC=630.7
plot(simulateResiduals(model_LUI2022_sqrt, refit=FALSE))
model_LUI2022_sqrt <- update(model_LUI2022_sqrt,REML=T)
summary(model_LUI2022_sqrt)

model_LUI2022_log <- glmmTMB(log(biomass+1) ~ LUI2016 + season, data = df2_2022, family = gaussian(), REML=FALSE)
summary(model_LUI2022_log) #AIC=429.9
plot(simulateResiduals(model_LUI2022_log, refit=FALSE))
model_LUI2022_log <- update(model_LUI2022_log,REML=T)
summary(model_LUI2022_log)

AICc(model_LUI2022, model_LUI2022_log, model_LUI2022_sqrt)

emmeans_results <- emmeans(model_LUI2022_log, pairwise ~ season, adjust = "tukey")
emmeans_results$contrasts

#include interaction
#best model
model_LUI2022_sqrt <- glmmTMB(sqrt(biomass) ~ LUI2016 * season, data = df2_2022, family = gaussian(), REML=FALSE)
summary(model_LUI2022_sqrt) #AIC=634
plot(simulateResiduals(model_LUI2022_sqrt, refit=FALSE))
model_LUI2022_sqrt <- update(model_LUI2022_sqrt,REML=T)
summary(model_LUI2022_sqrt)

model_LUI2022_sqrt <- glmmTMB(sqrt(biomass) ~ GPI200_ave * season, data = df2_2022, family = gaussian(), REML=TRUE)
summary(model_LUI2022_sqrt) #AIC=630.7
plot(simulateResiduals(model_LUI2022_sqrt, refit=FALSE))
model_LUI2022_sqrt <- update(model_LUI2022_sqrt,REML=T)
summary(model_LUI2022_sqrt)


#GPI
df2_2022_early <- df2_2022[df2_2022$season=="early",]
model_GPI2022_early_sqrt <- glmmTMB(sqrt(biomass) ~ GPI200_ave, data = df2_2022_early, family = gaussian(), REML=F)
summary(model_GPI2022_early_sqrt) #AIC=630.7
plot(simulateResiduals(model_GPI2022_early_sqrt, refit=FALSE))
model_GPI2022_early_sqrt <- update(model_GPI2022_early_sqrt,REML=T)
summary(model_GPI2022_early_sqrt)

#make the graph for the early
# extract model predictions for moisture
pred_earlygpi200 <- ggpredict(model_GPI2022_early_sqrt,terms=c("GPI200_ave"),back_transform=T)
# extract observed data
df2_2022_early$pred_biomass_early <- predict(model_GPI2022_early_sqrt,newdata=df2_2022_early,re.form = NULL, type="response")


# Plot
plot_gpi200_early <- ggplot() +
  geom_point(data = df2_2022_early, aes(x = GPI200_ave, y = biomass), alpha = 0.5) +
  geom_line(data = pred_earlygpi200, aes(x = x, y = predicted), color = "blue", lwd = 1) +
  geom_ribbon(data = pred_earlygpi200, aes(x = x, ymin = conf.low, ymax = conf.high), fill = "blue", alpha = 0.2) +
  labs(x = "gpi", y = "biomass", title = "model based graph") +
  theme_minimal()+
  theme(axis.title = element_text(size=16,face="bold"),
        axis.text = element_text(size=14),
        legend.title = element_text(size=15,face="bold"),
        legend.text = element_text(size=15),
        title = element_text(size=17,face="bold"))
print(plot_gpi200_early)


#the graphs is based on the model. bm values are real but the gpi predicted. instead of a smooth line from ggplot



model_GPI2022_mid_sqrt <- glmmTMB(sqrt(biomass) ~ GPI200_ave, data = df2_2022[df2_2022$season=="mid",], family = gaussian(), REML=F)
summary(model_GPI2022_mid_sqrt) #AIC=630.7
plot(simulateResiduals(model_GPI2022_mid_sqrt, refit=FALSE))
model_GPI2022_mid_sqrt <- update(model_GPI2022_mid_sqrt,REML=T)
summary(model_GPI2022_mid_sqrt)

model_GPI2022_late_sqrt <- glmmTMB(sqrt(biomass) ~ GPI200_ave, data = df2_2022[df2_2022$season=="late",], family = gaussian(), REML=F)
summary(model_GPI2022_late_sqrt) #AIC=630.7
plot(simulateResiduals(model_GPI2022_late_sqrt, refit=FALSE))
model_GPI2022_late_sqrt <- update(model_GPI2022_late_sqrt,REML=T)
summary(model_GPI2022_late_sqrt)

```
In this model we have an interaction, which makes a post-hoc not possible/hard to interpret. As long as we do not have the LUI changing over time, there is not really an interaction right... For now I am leaving the interaction out. Update: added the interaction again. Discussed this with Mo. Still do not fully grasp why... Alo make seperate models for each season and add models results to the plots

### LUI ###

Make model for 2022 LUI
```{r}
#early
df2_2022_early <- df2_2022[df2_2022$season=="early",]
model_LUI2022_early <- glmmTMB(biomass ~ LUI2016, data = df2_2022_early, family = gaussian(), REML=F)
summary(model_LUI2022_early) #AIC=533.0
plot(simulateResiduals(model_LUI2022_early, refit=FALSE))

df2_2022_early_sqrt <- df2_2022[df2_2022$season=="early",]
model_LUI2022_early_sqrt <- glmmTMB(sqrt(biomass) ~ LUI2016, data = df2_2022_early, family = gaussian(), REML=F)
summary(model_LUI2022_early_sqrt) #AIC=284.9
plot(simulateResiduals(model_LUI2022_early_sqrt, refit=FALSE))

df2_2022_early_log <- df2_2022[df2_2022$season=="early",]
model_LUI2022_early_log <- glmmTMB(log(biomass+1) ~ LUI2016, data = df2_2022_early, family = gaussian(), REML=F)
summary(model_LUI2022_early_log) #AIC=219.5
plot(simulateResiduals(model_LUI2022_early_log, refit=FALSE))


AICc(model_LUI2022_early, model_LUI2022_early_sqrt, model_LUI2022_early_log)

#best model
model_LUI2022_early_log <- update(model_LUI2022_early_log,REML=T)
summary(model_LUI2022_early_log) #AIC=432.5



#mid
df2_2022_mid <- df2_2022[df2_2022$season=="mid",]
model_LUI2022_mid <- glmmTMB(biomass ~ LUI2016, data = df2_2022_mid, family = gaussian(), REML=F)
summary(model_LUI2022_mid) 
plot(simulateResiduals(model_LUI2022_mid, refit=FALSE))

df2_2022_mid_sqrt <- df2_2022[df2_2022$season=="mid",]
model_LUI2022_mid_sqrt <- glmmTMB(sqrt(biomass) ~ LUI2016, data = df2_2022_mid, family = gaussian(), REML=F)
summary(model_LUI2022_mid_sqrt) 
plot(simulateResiduals(model_LUI2022_mid_sqrt, refit=FALSE))

df2_2022_mid_log <- df2_2022[df2_2022$season=="mid",]
model_LUI2022_mid_log <- glmmTMB(log(biomass+1) ~ LUI2016, data = df2_2022_mid, family = gaussian(), REML=F)
summary(model_LUI2022_mid_log)
plot(simulateResiduals(model_LUI2022_mid_log, refit=FALSE))


AICc(model_LUI2022_mid, model_LUI2022_mid_sqrt, model_LUI2022_mid_log)

#best model
model_LUI2022_mid_log <- update(model_LUI2022_mid_log,REML=T)
summary(model_LUI2022_mid_log) 


#late
df2_2022_late <- df2_2022[df2_2022$season=="late",]
model_LUI2022_late <- glmmTMB(biomass ~ LUI2016, data = df2_2022_late, family = gaussian(), REML=F)
summary(model_LUI2022_late) 
plot(simulateResiduals(model_LUI2022_late, refit=FALSE))

df2_2022_late_sqrt <- df2_2022[df2_2022$season=="late",]
model_LUI2022_late_sqrt <- glmmTMB(sqrt(biomass) ~ LUI2016, data = df2_2022_late, family = gaussian(), REML=F)
summary(model_LUI2022_late_sqrt) 
plot(simulateResiduals(model_LUI2022_late_sqrt, refit=FALSE))

df2_2022_late_log <- df2_2022[df2_2022$season=="late",]
model_LUI2022_late_log <- glmmTMB(log(biomass+1) ~ LUI2016, data = df2_2022_late, family = gaussian(), REML=F)
summary(model_LUI2022_late_log)
plot(simulateResiduals(model_LUI2022_late_log, refit=FALSE))


AICc(model_LUI2022_late, model_LUI2022_late_sqrt, model_LUI2022_late_log)

#best model
model_LUI2022_late_sqrt <- update(model_LUI2022_late_sqrt,REML=T)
summary(model_LUI2022_late_sqrt) 
```

Make graph for 2022 LUI
```{r}
#Early
# extract model predictions 
pred_early_2022_lui <- ggpredict(model_LUI2022_early_log,terms=c("LUI2016"),back_transform=T)
# extract observed data
df2_2022_early$pred_biomass_early <- predict(model_LUI2022_early_log,newdata=df2_2022_early,re.form = NULL, type="response")


plot_lui_2022_early <- ggplot() +
  geom_point(data = df2_2022_early, aes(x = LUI2016, y = biomass), alpha = 0.5) +
  geom_line(data = pred_early_2022_lui, aes(x = x, y = predicted), color = "lightgrey", lwd = 1) +
  geom_ribbon(data = pred_early_2022_lui, aes(x = x, ymin = conf.low, ymax = conf.high), fill = "lightgrey", alpha = 0.2) +
  labs(x = NULL, y = "Biomass (in g)", title = NULL) +
  xlim(c(0.05,0.45)) +
  ylim(c(0,100)) +
  theme_minimal()+
  theme(axis.title = element_text(size=14,face="bold"),
        axis.text = element_text(size=12),
        legend.title = element_text(size=13,face="bold"),
        legend.text = element_text(size=13),
        title = element_text(size=14,face="bold"))
print(plot_lui_2022_early)


#mid
# extract model predictions 
pred_mid_2022_lui <- ggpredict(model_LUI2022_mid_sqrt,terms=c("LUI2016"),back_transform=T)
# extract observed data
df2_2022_mid$pred_biomass_mid <- predict(model_LUI2022_mid_sqrt,newdata=df2_2022_mid,re.form = NULL, type="response")

plot_lui_2022_mid <- ggplot() +
  geom_point(data = df2_2022_mid, aes(x = LUI2016, y = biomass), alpha = 0.5) +
  geom_line(data = pred_mid_2022_lui, aes(x = x, y = predicted), color = "lightgrey", lwd = 1) +
  geom_ribbon(data = pred_mid_2022_lui, aes(x = x, ymin = conf.low, ymax = conf.high), fill = "lightgrey", alpha = 0.2) +
  labs(x = NULL, y = "Biomass (in g)", title = NULL) +
  xlim(c(0.05,0.45)) +
  ylim(c(0,100)) +
  theme_minimal()+
  theme(axis.title = element_text(size=14,face="bold"),
        axis.text = element_text(size=12),
        legend.title = element_text(size=13,face="bold"),
        legend.text = element_text(size=13),
        title = element_text(size=14,face="bold"))
print(plot_lui_2022_mid)

#late
# extract model predictions 
pred_late_2022_lui <- ggpredict(model_LUI2022_late_sqrt,terms=c("LUI2016"),back_transform=T)
# extract observed data
df2_2022_late$pred_biomass_late <- predict(model_LUI2022_late_sqrt,newdata=df2_2022_late,re.form = NULL, type="response")

plot_lui_2022_late <- ggplot() +
  geom_point(data = df2_2022_late, aes(x = LUI2016, y = biomass), alpha = 0.5) +
  geom_line(data = pred_late_2022_lui, aes(x = x, y = predicted), color = "lightgrey", lwd = 1) +
  geom_ribbon(data = pred_late_2022_lui, aes(x = x, ymin = conf.low, ymax = conf.high), fill = "lightgrey", alpha = 0.2) +
  labs(x = "Land-use intensity", y = "Biomass (in g)", title = NULL) +
  xlim(c(0.05,0.45)) +
  ylim(c(0,100)) +
  theme_minimal()+
  theme(axis.title = element_text(size=14,face="bold"),
        axis.text = element_text(size=12),
        legend.title = element_text(size=13,face="bold"),
        legend.text = element_text(size=13),
        title = element_text(size=14,face="bold"))
print(plot_lui_2022_late)
```

Combine the plots for the three seasons
```{r}
combined_plot <- grid.arrange(plot_lui_2022_early, plot_lui_2022_mid, plot_lui_2022_late, nrow = 3)

ggsave("G:/My Drive/PhD/Data/Big data set/combined_biomass_LUI_2022.png", plot = combined_plot, width = 5, height = 10, units = "in")
```



Make model for 2023 LUI
```{r}
#early
df2_2023_early <- df2_2023[df2_2023$season=="early",]
model_LUI2023_early <- glmmTMB(biomass ~ LUI2016, data = df2_2023_early, family = gaussian(), REML=F)
summary(model_LUI2023_early) #AIC=533.0
plot(simulateResiduals(model_LUI2023_early, refit=FALSE))

df2_2023_early_sqrt <- df2_2023[df2_2023$season=="early",]
model_LUI2023_early_sqrt <- glmmTMB(sqrt(biomass) ~ LUI2016, data = df2_2023_early, family = gaussian(), REML=F)
summary(model_LUI2023_early_sqrt) #AIC=284.9
plot(simulateResiduals(model_LUI2023_early_sqrt, refit=FALSE))

df2_2023_early_log <- df2_2023[df2_2023$season=="early",]
model_LUI2023_early_log <- glmmTMB(log(biomass+1) ~ LUI2016, data = df2_2023_early, family = gaussian(), REML=F)
summary(model_LUI2023_early_log) #AIC=219.5
plot(simulateResiduals(model_LUI2023_early_log, refit=FALSE))


AICc(model_LUI2023_early, model_LUI2023_early_sqrt, model_LUI2023_early_log)

#best model
model_LUI2023_early_log <- update(model_LUI2023_early_log,REML=T)
summary(model_LUI2023_early_log) #AIC=432.5


#mid
df2_2023_mid <- df2_2023[df2_2023$season=="mid",]
model_LUI2023_mid <- glmmTMB(biomass ~ LUI2016, data = df2_2023_mid, family = gaussian(), REML=F)
summary(model_LUI2023_mid) 
plot(simulateResiduals(model_LUI2023_mid, refit=FALSE))

df2_2023_mid_sqrt <- df2_2023[df2_2023$season=="mid",]
model_LUI2023_mid_sqrt <- glmmTMB(sqrt(biomass) ~ LUI2016, data = df2_2023_mid, family = gaussian(), REML=F)
summary(model_LUI2023_mid_sqrt) 
plot(simulateResiduals(model_LUI2023_mid_sqrt, refit=FALSE))

df2_2023_mid_log <- df2_2023[df2_2023$season=="mid",]
model_LUI2023_mid_log <- glmmTMB(log(biomass+1) ~ LUI2016, data = df2_2023_mid, family = gaussian(), REML=F)
summary(model_LUI2023_mid_log)
plot(simulateResiduals(model_LUI2023_mid_log, refit=FALSE))


AICc(model_LUI2023_mid, model_LUI2023_mid_sqrt, model_LUI2023_mid_log)

#best model
model_LUI2023_mid_log <- update(model_LUI2023_mid_log,REML=T)
summary(model_LUI2023_mid_log) 


#late
df2_2023_late <- df2_2023[df2_2023$season=="late",]
model_LUI2023_late <- glmmTMB(biomass ~ LUI2016, data = df2_2023_late, family = gaussian(), REML=F)
summary(model_LUI2023_late) 
plot(simulateResiduals(model_LUI2023_late, refit=FALSE))

df2_2023_late_sqrt <- df2_2023[df2_2023$season=="late",]
model_LUI2023_late_sqrt <- glmmTMB(sqrt(biomass) ~ LUI2016, data = df2_2023_late, family = gaussian(), REML=F)
summary(model_LUI2023_late_sqrt) 
plot(simulateResiduals(model_LUI2023_late_sqrt, refit=FALSE))

df2_2023_late_log <- df2_2023[df2_2023$season=="late",]
model_LUI2023_late_log <- glmmTMB(log(biomass+1) ~ LUI2016, data = df2_2023_late, family = gaussian(), REML=F)
summary(model_LUI2023_late_log)
plot(simulateResiduals(model_LUI2023_late_log, refit=FALSE))


AICc(model_LUI2023_late, model_LUI2023_late_sqrt, model_LUI2023_late_log)

#best model
model_LUI2023_late_log <- update(model_LUI2023_late_log,REML=T)
summary(model_LUI2023_late_log) 
```

Make graph for 2023 LUI
```{r}
#Early
# extract model predictions 
pred_early_2023_lui <- ggpredict(model_LUI2023_early_log,terms=c("LUI2016"),back_transform=T)
# extract observed data
df2_2023_early$pred_biomass_early <- predict(model_LUI2023_early_log,newdata=df2_2023_early,re.form = NULL, type="response")

plot_lui_2023_early <- ggplot() +
  geom_point(data = df2_2023_early, aes(x = LUI2016, y = biomass), alpha = 0.5) +
  geom_line(data = pred_early_2023_lui, aes(x = x, y = predicted), color = "lightgrey", lwd = 1) +
  geom_ribbon(data = pred_early_2023_lui, aes(x = x, ymin = conf.low, ymax = conf.high), fill = "lightgrey", alpha = 0.2) +
  labs(x = NULL, y = NULL, title = NULL) +
  xlim(c(0.05,0.45)) +
  ylim(c(0,100)) +
  theme_minimal()+
  theme(axis.title = element_text(size=14,face="bold"),
        axis.text = element_text(size=12),
        legend.title = element_text(size=13,face="bold"),
        legend.text = element_text(size=13),
        title = element_text(size=14,face="bold"))
print(plot_lui_2023_early)


#mid
# extract model predictions 
pred_mid_2023_lui <- ggpredict(model_LUI2023_mid_log,terms=c("LUI2016"),back_transform=T)
# extract observed data
df2_2023_mid$pred_biomass_mid <- predict(model_LUI2023_mid_log,newdata=df2_2023_mid,re.form = NULL, type="response")

plot_lui_2023_mid <- ggplot() +
  geom_point(data = df2_2023_mid, aes(x = LUI2016, y = biomass), alpha = 0.5) +
  geom_line(data = pred_mid_2023_lui, aes(x = x, y = predicted), color = "lightgrey", lwd = 1) +
  geom_ribbon(data = pred_mid_2023_lui, aes(x = x, ymin = conf.low, ymax = conf.high), fill = "lightgrey", alpha = 0.2) +
  labs(x = NULL, y = NULL, title = NULL) +
  xlim(c(0.05,0.45)) +
  ylim(c(0,100)) +
  theme_minimal()+
  theme(axis.title = element_text(size=14,face="bold"),
        axis.text = element_text(size=12),
        legend.title = element_text(size=13,face="bold"),
        legend.text = element_text(size=13),
        title = element_text(size=14,face="bold"))
print(plot_lui_2023_mid)

#late
# extract model predictions 
pred_late_2023_lui <- ggpredict(model_LUI2023_late_log,terms=c("LUI2016"),back_transform=T)
# extract observed data
df2_2023_late$pred_biomass_late <- predict(model_LUI2023_late_log,newdata=df2_2023_late,re.form = NULL, type="response")

plot_lui_2023_late <- ggplot() +
  geom_point(data = df2_2023_late, aes(x = LUI2016, y = biomass), alpha = 0.5) +
  geom_line(data = pred_late_2023_lui, aes(x = x, y = predicted), color = "lightgrey", lwd = 1) +
  geom_ribbon(data = pred_late_2023_lui, aes(x = x, ymin = conf.low, ymax = conf.high), fill = "lightgrey", alpha = 0.2) +
  labs(x = "Land-use intensity", y = NULL, title = NULL) +
  xlim(c(0.05,0.45)) +
  ylim(c(0,100)) +
  theme_minimal()+
  theme(axis.title = element_text(size=14,face="bold"),
        axis.text = element_text(size=12),
        legend.title = element_text(size=13,face="bold"),
        legend.text = element_text(size=13),
        title = element_text(size=14,face="bold"))
print(plot_lui_2023_late)
```

Combine the plots for the three seasons
```{r}
combined_plot <- grid.arrange(plot_lui_2023_early, plot_lui_2023_mid, plot_lui_2023_late, nrow = 3)

ggsave("G:/My Drive/PhD/Data/Big data set/combined_biomass_LUI_2023.png", plot = combined_plot, width = 5, height = 10, units = "in")
```




Make new sub datasets for the big graph 2022
```{r}
#early
df2_2022_early_lui_sub <- df2_2022_early[,c("LUI2016", "biomass")]
df2_2022_early_lui_sub$season <- "Early" 
df2_2022_early_lui_sub$year <- 2022

pred_early_2022_lui$season <- "Early"
pred_early_2022_lui$year <- 2022


#mid
df2_2022_mid_lui_sub <- df2_2022_mid[,c("LUI2016", "biomass")]
df2_2022_mid_lui_sub$season <- "Mid" 
df2_2022_mid_lui_sub$year <- 2022

pred_mid_2022_lui$season <- "Mid"
pred_mid_2022_lui$year <- 2022

#late
df2_2022_late_lui_sub <- df2_2022_late[,c("LUI2016", "biomass")]
df2_2022_late_lui_sub$season <- "Late" 
df2_2022_late_lui_sub$year <- 2022

pred_late_2022_lui$season <- "Late"
pred_late_2022_lui$year <- 2022
```

Make new sub datasets for the big graph 2023
```{r}
#early
df2_2023_early_lui_sub <- df2_2023_early[,c("LUI2016", "biomass")]
df2_2023_early_lui_sub$season <- "Early" 
df2_2023_early_lui_sub$year <- 2023

pred_early_2023_lui$season <- "Early"
pred_early_2023_lui$year <- 2023


#mid
df2_2023_mid_lui_sub <- df2_2023_mid[,c("LUI2016", "biomass")]
df2_2023_mid_lui_sub$season <- "Mid" 
df2_2023_mid_lui_sub$year <- 2023

pred_mid_2023_lui$season <- "Mid"
pred_mid_2023_lui$year <- 2023

#late
df2_2023_late_lui_sub <- df2_2023_late[,c("LUI2016", "biomass")]
df2_2023_late_lui_sub$season <- "Late" 
df2_2023_late_lui_sub$year <- 2023

pred_late_2023_lui$season <- "Late"
pred_late_2023_lui$year <- 2023
```


All lui graphs combined into one big one LUI
```{r}
df2_lui <- rbind(df2_2022_early_lui_sub, df2_2022_mid_lui_sub, df2_2022_late_lui_sub, df2_2023_early_lui_sub, df2_2023_mid_lui_sub, df2_2023_late_lui_sub)

df2_lui$season <- factor(df2_lui$season, levels=c("Early", "Mid", "Late"))
table(df2_lui$season)

df2_lui_pred <- rbind(pred_early_2022_lui, pred_mid_2022_lui, pred_late_2022_lui, pred_early_2023_lui, pred_mid_2023_lui, pred_late_2023_lui)

df2_lui_pred$season <- factor(df2_lui_pred$season, levels=c("Early", "Mid", "Late"))
table(df2_lui_pred$season)

plot_lui <- ggplot() +
  geom_point(data = df2_lui, aes(x = LUI2016, y = biomass), alpha = 0.5) +
  geom_line(data = df2_lui_pred, aes(x = x, y = predicted), color = "grey50", lwd = 1) +
  geom_ribbon(data = df2_lui_pred, aes(x = x, ymin = conf.low, ymax = conf.high), fill = "grey70", alpha = 0.2) +
  facet_grid(factor(season) ~ year, scales = "free") +
  labs(x = "Land-use intensity", y = "Biomass (in g)", title = NULL) +
  theme_bw()+
  theme(axis.title = element_text(size=10,face="bold"),
        axis.text = element_text(size=8),
        legend.title = element_text(size=9,face="bold"),
        legend.text = element_text(size=9),
        title = element_text(size=10,face="bold"))

label_df <- data.frame(
  year = c(2022, 2022, 2022, 2023, 2023, 2023),
  season = factor(c("Early", "Mid", "Late", "Early", "Mid", "Late"), 
                  levels = c("Early", "Mid", "Late")),
  x = c(0.4, 0.4, 0.4, 0.4, 0.2, 0.4),
  y = c(95, 95, 95, 95, 95, 95),  
  label = c("p=0.458", "p=0.468", "p=0.552", "p=0.881", "p=0.002, 
            estimate = 7.91", "p=0.361") 
)


plot_lui <- plot_lui +
  geom_text(data = label_df,
            aes(x = x, y = y, label = label),
            size = 3, color = "black", fontface = "bold")


ggsave("G:/My Drive/PhD/Data/Big data set/combined_biomass_LUI.png", plot = plot_lui, width = 5, height = 7, units = "in")
```









### GPI 200 m###

Make model for 2022 GPI
```{r}
str(df2_2022)
df2_2022$GPI200_ave <- as.numeric(as.character(df2_2022$GPI200_ave))

#early
df2_2022_gpi_early <- df2_2022[df2_2022$season=="early",]
model_gpi2022_early <- glmmTMB(biomass ~ GPI200_ave, data = df2_2022_early, family = gaussian(), REML=F)
summary(model_gpi2022_early) 
plot(simulateResiduals(model_gpi2022_early, refit=FALSE))

df2_2022_early_sqrt <- df2_2022[df2_2022$season=="early",]
model_gpi2022_early_sqrt <- glmmTMB(sqrt(biomass) ~ GPI200_ave, data = df2_2022_early, family = gaussian(), REML=F)
summary(model_gpi2022_early_sqrt) 
plot(simulateResiduals(model_gpi2022_early_sqrt, refit=FALSE))

df2_2022_early_log <- df2_2022[df2_2022$season=="early",]
model_gpi2022_early_log <- glmmTMB(log(biomass+1) ~ GPI200_ave, data = df2_2022_early, family = gaussian(), REML=F)
summary(model_gpi2022_early_log) 
plot(simulateResiduals(model_gpi2022_early_log, refit=FALSE))


AICc(model_gpi2022_early, model_gpi2022_early_sqrt, model_gpi2022_early_log)

#best model
model_gpi2022_early_log <- update(model_gpi2022_early_log,REML=T)
summary(model_gpi2022_early_log) 


#mid
df2_2022_mid <- df2_2022[df2_2022$season=="mid",]
model_gpi2022_mid <- glmmTMB(biomass ~ GPI200_ave, data = df2_2022_mid, family = gaussian(), REML=F)
summary(model_gpi2022_mid) 
plot(simulateResiduals(model_gpi2022_mid, refit=FALSE))

df2_2022_mid_sqrt <- df2_2022[df2_2022$season=="mid",]
model_gpi2022_mid_sqrt <- glmmTMB(sqrt(biomass) ~ GPI200_ave, data = df2_2022_mid, family = gaussian(), REML=F)
summary(model_gpi2022_mid_sqrt) 
plot(simulateResiduals(model_gpi2022_mid_sqrt, refit=FALSE))

df2_2022_mid_log <- df2_2022[df2_2022$season=="mid",]
model_gpi2022_mid_log <- glmmTMB(log(biomass+1) ~ GPI200_ave, data = df2_2022_mid, family = gaussian(), REML=F)
summary(model_gpi2022_mid_log)
plot(simulateResiduals(model_gpi2022_mid_log, refit=FALSE))


AICc(model_gpi2022_mid, model_gpi2022_mid_sqrt, model_gpi2022_mid_log)

#best model
model_gpi2022_mid_log <- update(model_gpi2022_mid_log,REML=T)
summary(model_gpi2022_mid_log) 


#late
df2_2022_late <- df2_2022[df2_2022$season=="late",]
model_gpi2022_late <- glmmTMB(biomass ~ GPI200_ave, data = df2_2022_late, family = gaussian(), REML=F)
summary(model_gpi2022_late) 
plot(simulateResiduals(model_gpi2022_late, refit=FALSE))

df2_2022_late_sqrt <- df2_2022[df2_2022$season=="late",]
model_gpi2022_late_sqrt <- glmmTMB(sqrt(biomass) ~ GPI200_ave, data = df2_2022_late, family = gaussian(), REML=F)
summary(model_gpi2022_late_sqrt) 
plot(simulateResiduals(model_gpi2022_late_sqrt, refit=FALSE))

df2_2022_late_log <- df2_2022[df2_2022$season=="late",]
model_gpi2022_late_log <- glmmTMB(log(biomass+1) ~ GPI200_ave, data = df2_2022_late, family = gaussian(), REML=F)
summary(model_gpi2022_late_log)
plot(simulateResiduals(model_gpi2022_late_log, refit=FALSE))


AICc(model_gpi2022_late, model_gpi2022_late_sqrt, model_gpi2022_late_log)

#best model
model_gpi2022_late_log <- update(model_gpi2022_late_log,REML=T)
summary(model_gpi2022_late_log) 
```

Make graph for 2022 GPI
```{r}
#Early
pred_early_2022_gpi <- ggpredict(model_gpi2022_early_log,terms=c("GPI200_ave"),back_transform=T)
df2_2022_early$pred_biomass_early <- predict(model_gpi2022_early_log,newdata=df2_2022_early,re.form = NULL, type="response")



plot_gpi_2022_early <- ggplot() +
  geom_point(data = df2_2022_early, aes(x = GPI200_ave, y = biomass), alpha = 0.5) +
  geom_line(data = pred_early_2022_gpi, aes(x = x, y = predicted), 
            color = "lightgrey", lwd = 1) +
  geom_ribbon(data = pred_early_2022_gpi, 
              aes(x = x, ymin = conf.low, ymax = conf.high), 
              fill = "lightgrey", alpha = 0.2) +
  labs(x = NULL, y = NULL, title = NULL) +
  scale_x_continuous(breaks = seq(720, 730, by = 2),
                     labels = scales::number_format(accuracy = 1)) +
  xlim(c(721,731)) +
  ylim(c(0,100)) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 13),
    title = element_text(size = 14, face = "bold")
  )

print(plot_gpi_2022_early)


#mid
pred_mid_2022_gpi <- ggpredict(model_gpi2022_mid_log,terms=c("GPI200_ave"),back_transform=T)
df2_2022_mid$pred_biomass_mid <- predict(model_gpi2022_mid_log,newdata=df2_2022_mid,re.form = NULL, type="response")

plot_gpi_2022_mid <- ggplot() +
  geom_point(data = df2_2022_mid, aes(x = GPI200_ave, y = biomass), alpha = 0.5) +
  geom_line(data = pred_mid_2022_gpi, aes(x = x, y = predicted), color = "lightgrey", lwd = 1) +
  geom_ribbon(data = pred_mid_2022_gpi, aes(x = x, ymin = conf.low, ymax = conf.high), fill = "lightgrey", alpha = 0.2) +
  labs(x = NULL, y = NULL, title = NULL) +
  xlim(c(721,731)) +
  ylim(c(0,100)) +
  theme_minimal()+
  theme(axis.title = element_text(size=14,face="bold"),
        axis.text = element_text(size=12),
        legend.title = element_text(size=13,face="bold"),
        legend.text = element_text(size=13),
        title = element_text(size=14,face="bold"))
print(plot_gpi_2022_mid)

#late
pred_late_2022_gpi <- ggpredict(model_gpi2022_late_log,terms=c("GPI200_ave"),back_transform=T)
df2_2022_late$pred_biomass_late <- predict(model_gpi2022_late_log,newdata=df2_2022_late,re.form = NULL, type="response")

plot_gpi_2022_late <- ggplot() +
  geom_point(data = df2_2022_late, aes(x = GPI200_ave, y = biomass), alpha = 0.5) +
  geom_line(data = pred_late_2022_gpi, aes(x = x, y = predicted), color = "lightgrey", lwd = 1) +
  geom_ribbon(data = pred_late_2022_gpi, aes(x = x, ymin = conf.low, ymax = conf.high), fill = "lightgrey", alpha = 0.2) +
  labs(x = "Grassland productivity index", y = NULL, title = NULL) +
  xlim(c(721,731)) +
  ylim(c(0,100)) +
  theme_minimal()+
  theme(axis.title = element_text(size=14,face="bold"),
        axis.text = element_text(size=12),
        legend.title = element_text(size=13,face="bold"),
        legend.text = element_text(size=13),
        title = element_text(size=14,face="bold"))

plot_gpi_2022_late
```

Combine the plots for the three seasons
```{r}
combined_plot <- grid.arrange(plot_gpi_2022_early, plot_gpi_2022_mid, plot_gpi_2022_late, nrow = 3)

ggsave("G:/My Drive/PhD/Data/Big data set/combined_biomass_GPI_2022.png", plot = combined_plot, width = 5, height = 10, units = "in")
```



Make model for 2023 GPI
```{r}
str(df2_2023)
df2_2023$GPI200_ave <- as.numeric(as.character(df2_2023$GPI200_ave))


#early
df2_2023_gpi_early <- df2_2023[df2_2023$season=="early",]
model_gpi2023_early <- glmmTMB(biomass ~ GPI200_ave, data = df2_2023_early, family = gaussian(), REML=F)
summary(model_gpi2023_early) 
plot(simulateResiduals(model_gpi2023_early, refit=FALSE))

df2_2023_early_sqrt <- df2_2023[df2_2023$season=="early",]
model_gpi2023_early_sqrt <- glmmTMB(sqrt(biomass) ~ GPI200_ave, data = df2_2023_early, family = gaussian(), REML=F)
summary(model_gpi2023_early_sqrt) 
plot(simulateResiduals(model_gpi2023_early_sqrt, refit=FALSE))

df2_2023_early_log <- df2_2023[df2_2023$season=="early",]
model_gpi2023_early_log <- glmmTMB(log(biomass+1) ~ GPI200_ave, data = df2_2023_early, family = gaussian(), REML=F)
summary(model_gpi2023_early_log) 
plot(simulateResiduals(model_gpi2023_early_log, refit=FALSE))


AICc(model_gpi2023_early, model_gpi2023_early_sqrt, model_gpi2023_early_log)

#best model
model_gpi2023_early_log <- update(model_gpi2023_early_log,REML=T)
summary(model_gpi2023_early_log) 


#mid
df2_2023_gpi_mid <- df2_2023[df2_2023$season=="mid",]
model_gpi2023_mid <- glmmTMB(biomass ~ GPI200_ave, data = df2_2023_mid, family = gaussian(), REML=F)
summary(model_gpi2023_mid) 
plot(simulateResiduals(model_gpi2023_mid, refit=FALSE))

df2_2023_mid_sqrt <- df2_2023[df2_2023$season=="mid",]
model_gpi2023_mid_sqrt <- glmmTMB(sqrt(biomass) ~ GPI200_ave, data = df2_2023_mid, family = gaussian(), REML=F)
summary(model_gpi2023_mid_sqrt) 
plot(simulateResiduals(model_gpi2023_mid_sqrt, refit=FALSE))

df2_2023_mid_log <- df2_2023[df2_2023$season=="mid",]
model_gpi2023_mid_log <- glmmTMB(log(biomass+1) ~ GPI200_ave, data = df2_2023_mid, family = gaussian(), REML=F)
summary(model_gpi2023_mid_log)
plot(simulateResiduals(model_gpi2023_mid_log, refit=FALSE))


AICc(model_gpi2023_mid, model_gpi2023_mid_sqrt, model_gpi2023_mid_log)

#best model
model_gpi2023_mid_log <- update(model_gpi2023_mid_log,REML=T)
summary(model_gpi2023_mid_log) 


#late
df2_2023_gpi_late <- df2_2023[df2_2023$season=="late",]
model_gpi2023_late <- glmmTMB(biomass ~ GPI200_ave, data = df2_2023_late, family = gaussian(), REML=F)
summary(model_gpi2023_late) 
plot(simulateResiduals(model_gpi2023_late, refit=FALSE))

df2_2023_late_sqrt <- df2_2023[df2_2023$season=="late",]
model_gpi2023_late_sqrt <- glmmTMB(sqrt(biomass) ~ GPI200_ave, data = df2_2023_late, family = gaussian(), REML=F)
summary(model_gpi2023_late_sqrt) 
plot(simulateResiduals(model_gpi2023_late_sqrt, refit=FALSE))

df2_2023_late_log <- df2_2023[df2_2023$season=="late",]
model_gpi2023_late_log <- glmmTMB(log(biomass+1) ~ GPI200_ave, data = df2_2023_late, family = gaussian(), REML=F)
summary(model_gpi2023_late_log)
plot(simulateResiduals(model_gpi2023_late_log, refit=FALSE))


AICc(model_gpi2023_late, model_gpi2023_late_sqrt, model_gpi2023_late_log)

#best model
model_gpi2023_late_log <- update(model_gpi2023_late_log,REML=T)
summary(model_gpi2023_late_log) 
```

Make graph for 2023 GPI
```{r}
#Early
# extract model predictions 
pred_early_2023_gpi <- ggpredict(model_gpi2023_early_log,terms=c("GPI200_ave"),back_transform=T)
# extract observed data
df2_2023_early$pred_biomass_early <- predict(model_gpi2023_early_log,newdata=df2_2023_early,re.form = NULL, type="response")

plot_gpi_2023_early <- ggplot() +
  geom_point(data = df2_2023_early, aes(x = GPI200_ave, y = biomass), alpha = 0.5) +
  geom_line(data = pred_early_2023_gpi, aes(x = x, y = predicted), color = "lightgrey", lwd = 1) +
  geom_ribbon(data = pred_early_2023_gpi, aes(x = x, ymin = conf.low, ymax = conf.high), fill = "lightgrey", alpha = 0.2) +
  labs(x = NULL, y = NULL, title = NULL) +
  xlim(c(721,731)) +
  ylim(c(0,100)) +
  theme_minimal()+
  theme(axis.title = element_text(size=14,face="bold"),
        axis.text = element_text(size=12),
        legend.title = element_text(size=13,face="bold"),
        legend.text = element_text(size=13),
        title = element_text(size=14,face="bold"))
print(plot_gpi_2023_early)



#mid
# extract model predictions 
pred_mid_2023_gpi <- ggpredict(model_gpi2023_mid_sqrt,terms=c("GPI200_ave"),back_transform=T)
# extract observed data
df2_2023_mid$pred_biomass_mid <- predict(model_gpi2023_mid_sqrt,newdata=df2_2023_mid,re.form = NULL, type="response")

plot_gpi_2023_mid <- ggplot() +
  geom_point(data = df2_2023_mid, aes(x = GPI200_ave, y = biomass), alpha = 0.5) +
  geom_line(data = pred_mid_2023_gpi, aes(x = x, y = predicted), color = "lightgrey", lwd = 1) +
  geom_ribbon(data = pred_mid_2023_gpi, aes(x = x, ymin = conf.low, ymax = conf.high), fill = "lightgrey", alpha = 0.2) +
  labs(x = NULL, y = NULL, title = NULL) +
  xlim(c(721,731)) +
  ylim(c(0,100)) +
  theme_minimal()+
  theme(axis.title = element_text(size=14,face="bold"),
        axis.text = element_text(size=12),
        legend.title = element_text(size=13,face="bold"),
        legend.text = element_text(size=13),
        title = element_text(size=14,face="bold"))
print(plot_gpi_2023_mid)

#late
# extract model predictions 
pred_late_2023_gpi <- ggpredict(model_gpi2023_late_sqrt,terms=c("GPI200_ave"),back_transform=T)
# extract observed data
df2_2023_late$pred_biomass_late <- predict(model_gpi2023_late_sqrt,newdata=df2_2023_late,re.form = NULL, type="response")

plot_gpi_2023_late <- ggplot() +
  geom_point(data = df2_2023_late, aes(x = GPI200_ave, y = biomass), alpha = 0.5) +
  geom_line(data = pred_late_2023_gpi, aes(x = x, y = predicted), color = "lightgrey", lwd = 1) +
  geom_ribbon(data = pred_late_2023_gpi, aes(x = x, ymin = conf.low, ymax = conf.high), fill = "lightgrey", alpha = 0.2) +
  labs(x = "Grassland productivity index", y = NULL, title = NULL) +
  xlim(c(721,731)) +
  ylim(c(0,100)) +
  theme_minimal()+
  theme(axis.title = element_text(size=14,face="bold"),
        axis.text = element_text(size=12),
        legend.title = element_text(size=13,face="bold"),
        legend.text = element_text(size=13),
        title = element_text(size=14,face="bold"))
print(plot_gpi_2023_late)
```

Combine the plots for the three seasons
```{r}
combined_plot <- grid.arrange(plot_gpi_2023_early, plot_gpi_2023_mid, plot_gpi_2023_late, nrow = 3)

# Save the combined plot to a file
ggsave("G:/My Drive/PhD/Data/Big data set/combined_biomass_GPI_2023.png", plot = combined_plot, width = 5, height = 10, units = "in")
```




Combine all the graphs to one BIG graph. This is the old version of the big graph
```{r}
#title_2022 <- textGrob(
#"2022",
# x = 0.3, y = 0.1,
# just = "center",
# gp = gpar(fontsize = 24, fontface = "bold", col = "black"))

#title_2023 <- textGrob(
#"2023",
#x = 1, y= 1,
# just = "center",
# gp = gpar(fontsize = 24, fontface = "bold", col = "black"))


#row1 <- ggarrange(plot_lui2022_early, plot_gpi2022_early, plot_lui2023_early, plot_gpi2023_early, ncol = 4)
#row2 <- ggarrange(plot_lui2022_mid,   plot_gpi2022_mid,   plot_lui2023_mid,   plot_gpi2023_mid,   ncol = 4)
#row3 <- ggarrange(plot_lui2022_late,  plot_gpi2022_late,  plot_lui2023_late,  plot_gpi2023_late,  ncol = 4)

#label_row <- function(label) {
#text_grob(label, rot = 90, face = "bold", size = 16)
#}

#label1 <- label_row("Early (weeks 14-22)")
#label2 <- label_row("Mid (weeks 23-29)")
#label3 <- label_row("Late (weeks 30-34)")

#row1_labeled <- ggarrange(label1, row1, ncol = 2, widths = c(0.05, 1))
#row2_labeled <- ggarrange(label2, row2, ncol = 2, widths = c(0.05, 1))
#row3_labeled <- ggarrange(label3, row3, ncol = 2, widths = c(0.05, 1))

#main_plot_with_rows <- ggarrange(
#row1_labeled, row2_labeled, row3_labeled, 
# ncol = 1, 
# heights = c(1, 1, 1)
#)


#all_lui_gpi <- ggarrange(
# top_titles, 
#  main_plot_with_rows, 
# ncol = 1, 
#  heights = c(0.1, 1)
#)


#ggsave(
#  filename = "G:/My Drive/PhD/Data/Big data set/all_lui_gpi.png",
#  plot = final_plot,
#  width = 16,
#  height = 10,
#  units = "in",
#  bg = "white"
#)
```



Make new sub datasets for the big graph 2022 GPI
```{r}
#early
df2_2022_early_gpi_sub <- df2_2022_gpi_early[,c("GPI200_ave", "biomass")]
df2_2022_early_gpi_sub$season <- "Early" 
df2_2022_early_gpi_sub$year <- 2022

pred_early_2022_gpi$season <- "Early"
pred_early_2022_gpi$year <- 2022

#mid
df2_2022_mid_gpi_sub <- df2_2022_mid[,c("GPI200_ave", "biomass")]
df2_2022_mid_gpi_sub$season <- "Mid" 
df2_2022_mid_gpi_sub$year <- 2022

pred_mid_2022_gpi$season <- "Mid"
pred_mid_2022_gpi$year <- 2022

#late
df2_2022_late_gpi_sub <- df2_2022_late[,c("GPI200_ave", "biomass")]
df2_2022_late_gpi_sub$season <- "Late" 
df2_2022_late_gpi_sub$year <- 2022

pred_late_2022_gpi$season <- "Late"
pred_late_2022_gpi$year <- 2022
```

Make new sub datasets for the big graph 2023 GPI
```{r}
#early
df2_2023_early_gpi_sub <- df2_2023_gpi_early[,c("GPI200_ave", "biomass")]
df2_2023_early_gpi_sub$season <- "Early" 
df2_2023_early_gpi_sub$year <- 2023

pred_early_2023_gpi$season <- "Early"
pred_early_2023_gpi$year <- 2023

#mid
df2_2023_mid_gpi_sub <- df2_2023_mid[,c("GPI200_ave", "biomass")]
df2_2023_mid_gpi_sub$season <- "Mid" 
df2_2023_mid_gpi_sub$year <- 2023

pred_mid_2023_gpi$season <- "Mid"
pred_mid_2023_gpi$year <- 2023

#late
df2_2023_late_gpi_sub <- df2_2023_late[,c("GPI200_ave", "biomass")]
df2_2023_late_gpi_sub$season <- "Late" 
df2_2023_late_gpi_sub$year <- 2023

pred_late_2023_gpi$season <- "Late"
pred_late_2023_gpi$year <- 2023
```

All gpi graphs combined into one big one GPI
```{r}
str(df2_gpi)


df2_gpi <- rbind(df2_2022_early_gpi_sub, df2_2022_mid_gpi_sub, df2_2022_late_gpi_sub, df2_2023_early_gpi_sub, df2_2023_mid_gpi_sub, df2_2023_late_gpi_sub)

df2_gpi$season <- factor(df2_gpi$season, levels=c("Early", "Mid", "Late"))
table(df2_gpi$season)

df2_gpi_pred <- rbind(pred_early_2022_gpi, pred_mid_2022_gpi, pred_late_2022_gpi, pred_early_2023_gpi, pred_mid_2023_gpi, pred_late_2023_gpi)

df2_gpi_pred$season <- factor(df2_gpi_pred$season, levels=c("Early", "Mid", "Late"))
table(df2_gpi_pred$season)

label_df$year <- factor(label_df$year)
df2_gpi$year <- factor(df2_gpi$year)
df2_gpi_pred$year <- factor(df2_gpi_pred$year)



plot_gpi <- ggplot() +
  geom_point(data = df2_gpi, aes(x = GPI200_ave, y = biomass), alpha = 0.5) +
  geom_line(data = df2_gpi_pred, aes(x = x, y = predicted), color = "grey50", lwd = 1) +
  geom_ribbon(data = df2_gpi_pred, aes(x = x, ymin = conf.low, ymax = conf.high), fill = "grey70", alpha = 0.2) +
  facet_grid(factor(season) ~ year, scales = "free") +
  labs(x = "Grassland productivity index", y = "Biomass (in g)", title = NULL) +
  theme_bw()+
  theme(axis.title = element_text(size=10,face="bold"),
        axis.text = element_text(size=8),
        legend.title = element_text(size=9,face="bold"),
        legend.text = element_text(size=9),
        title = element_text(size=10,face="bold"))
plot_gpi

label_df <- data.frame(
  year = c(2022, 2022, 2022, 2023, 2023, 2023),
  season = factor(c("Early", "Mid", "Late", "Early", "Mid", "Late"), 
                  levels = c("Early", "Mid", "Late")),
  x = c(729, 729, 729, 729, 725, 729),
  y = c(58, 90, 78, 58, 90, 78),  
  label = c("p=0.686", "p=0.103", "p=0.053", "p=0.0.760", "p=0.004, 
            estimate = 0.076", "p=0.182") 
)


plot_gpi <- plot_gpi +
  geom_text(data = label_df,
            aes(x = x, y = y, label = label),
            size = 3, color = "black", fontface = "bold")



ggsave("G:/My Drive/PhD/Data/Big data set/combined_biomass_GPI200.png", plot = plot_gpi, width = 5, height = 7, units = "in")
```




### GPI ###

Make model for 2022 GPI
```{r}
str(df2_2022)
df2_2022$GPI <- as.numeric(as.character(df2_2022$GPI))

#early
df2_2022_gpi_early <- df2_2022[df2_2022$season=="early",]
model_gpi2022_early <- glmmTMB(biomass ~ GPI, data = df2_2022_early, family = gaussian(), REML=F)
summary(model_gpi2022_early) 
plot(simulateResiduals(model_gpi2022_early, refit=FALSE))

df2_2022_early_sqrt <- df2_2022[df2_2022$season=="early",]
model_gpi2022_early_sqrt <- glmmTMB(sqrt(biomass) ~ GPI, data = df2_2022_early, family = gaussian(), REML=F)
summary(model_gpi2022_early_sqrt) 
plot(simulateResiduals(model_gpi2022_early_sqrt, refit=FALSE))

df2_2022_early_log <- df2_2022[df2_2022$season=="early",]
model_gpi2022_early_log <- glmmTMB(log(biomass+1) ~ GPI, data = df2_2022_early, family = gaussian(), REML=F)
summary(model_gpi2022_early_log) 
plot(simulateResiduals(model_gpi2022_early_log, refit=FALSE))


AICc(model_gpi2022_early, model_gpi2022_early_sqrt, model_gpi2022_early_log)

#best model
model_gpi2022_early_log <- update(model_gpi2022_early_log,REML=T)
summary(model_gpi2022_early_log) 


#mid
df2_2022_mid <- df2_2022[df2_2022$season=="mid",]
model_gpi2022_mid <- glmmTMB(biomass ~ GPI, data = df2_2022_mid, family = gaussian(), REML=F)
summary(model_gpi2022_mid) 
plot(simulateResiduals(model_gpi2022_mid, refit=FALSE))

df2_2022_mid_sqrt <- df2_2022[df2_2022$season=="mid",]
model_gpi2022_mid_sqrt <- glmmTMB(sqrt(biomass) ~ GPI, data = df2_2022_mid, family = gaussian(), REML=F)
summary(model_gpi2022_mid_sqrt) 
plot(simulateResiduals(model_gpi2022_mid_sqrt, refit=FALSE))

df2_2022_mid_log <- df2_2022[df2_2022$season=="mid",]
model_gpi2022_mid_log <- glmmTMB(log(biomass+1) ~ GPI, data = df2_2022_mid, family = gaussian(), REML=F)
summary(model_gpi2022_mid_log)
plot(simulateResiduals(model_gpi2022_mid_log, refit=FALSE))


AICc(model_gpi2022_mid, model_gpi2022_mid_sqrt, model_gpi2022_mid_log)

#best model
model_gpi2022_mid_log <- update(model_gpi2022_mid_log,REML=T)
summary(model_gpi2022_mid_log) 


#late
df2_2022_late <- df2_2022[df2_2022$season=="late",]
model_gpi2022_late <- glmmTMB(biomass ~ GPI, data = df2_2022_late, family = gaussian(), REML=F)
summary(model_gpi2022_late) 
plot(simulateResiduals(model_gpi2022_late, refit=FALSE))

df2_2022_late_sqrt <- df2_2022[df2_2022$season=="late",]
model_gpi2022_late_sqrt <- glmmTMB(sqrt(biomass) ~ GPI, data = df2_2022_late, family = gaussian(), REML=F)
summary(model_gpi2022_late_sqrt) 
plot(simulateResiduals(model_gpi2022_late_sqrt, refit=FALSE))

df2_2022_late_log <- df2_2022[df2_2022$season=="late",]
model_gpi2022_late_log <- glmmTMB(log(biomass+1) ~ GPI, data = df2_2022_late, family = gaussian(), REML=F)
summary(model_gpi2022_late_log)
plot(simulateResiduals(model_gpi2022_late_log, refit=FALSE))


AICc(model_gpi2022_late, model_gpi2022_late_sqrt, model_gpi2022_late_log)

#best model
model_gpi2022_late_log <- update(model_gpi2022_late_log,REML=T)
summary(model_gpi2022_late_log) 
```

Make graph for 2022 GPI
```{r}
#Early
pred_early_2022_gpi <- ggpredict(model_gpi2022_early_log,terms=c("GPI"),back_transform=T)
df2_2022_early$pred_biomass_early <- predict(model_gpi2022_early_log,newdata=df2_2022_early,re.form = NULL, type="response")



plot_gpi_2022_early <- ggplot() +
  geom_point(data = df2_2022_early, aes(x = GPI, y = biomass), alpha = 0.5) +
  geom_line(data = pred_early_2022_gpi, aes(x = x, y = predicted), 
            color = "lightgrey", lwd = 1) +
  geom_ribbon(data = pred_early_2022_gpi, 
              aes(x = x, ymin = conf.low, ymax = conf.high), 
              fill = "lightgrey", alpha = 0.2) +
  labs(x = NULL, y = NULL, title = NULL) +
  scale_x_continuous(breaks = seq(720, 730, by = 2),
                     labels = scales::number_format(accuracy = 1)) +
  xlim(c(721,731)) +
  ylim(c(0,100)) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 13),
    title = element_text(size = 14, face = "bold")
  )

print(plot_gpi_2022_early)


#mid
pred_mid_2022_gpi <- ggpredict(model_gpi2022_mid_log,terms=c("GPI"),back_transform=T)
df2_2022_mid$pred_biomass_mid <- predict(model_gpi2022_mid_log,newdata=df2_2022_mid,re.form = NULL, type="response")

plot_gpi_2022_mid <- ggplot() +
  geom_point(data = df2_2022_mid, aes(x = GPI, y = biomass), alpha = 0.5) +
  geom_line(data = pred_mid_2022_gpi, aes(x = x, y = predicted), color = "lightgrey", lwd = 1) +
  geom_ribbon(data = pred_mid_2022_gpi, aes(x = x, ymin = conf.low, ymax = conf.high), fill = "lightgrey", alpha = 0.2) +
  labs(x = NULL, y = NULL, title = NULL) +
  xlim(c(721,731)) +
  ylim(c(0,100)) +
  theme_minimal()+
  theme(axis.title = element_text(size=14,face="bold"),
        axis.text = element_text(size=12),
        legend.title = element_text(size=13,face="bold"),
        legend.text = element_text(size=13),
        title = element_text(size=14,face="bold"))
print(plot_gpi_2022_mid)

#late
pred_late_2022_gpi <- ggpredict(model_gpi2022_late_log,terms=c("GPI"),back_transform=T)
df2_2022_late$pred_biomass_late <- predict(model_gpi2022_late_log,newdata=df2_2022_late,re.form = NULL, type="response")

plot_gpi_2022_late <- ggplot() +
  geom_point(data = df2_2022_late, aes(x = GPI, y = biomass), alpha = 0.5) +
  geom_line(data = pred_late_2022_gpi, aes(x = x, y = predicted), color = "lightgrey", lwd = 1) +
  geom_ribbon(data = pred_late_2022_gpi, aes(x = x, ymin = conf.low, ymax = conf.high), fill = "lightgrey", alpha = 0.2) +
  labs(x = "Grassland productivity index", y = NULL, title = NULL) +
  xlim(c(721,731)) +
  ylim(c(0,100)) +
  theme_minimal()+
  theme(axis.title = element_text(size=14,face="bold"),
        axis.text = element_text(size=12),
        legend.title = element_text(size=13,face="bold"),
        legend.text = element_text(size=13),
        title = element_text(size=14,face="bold"))

plot_gpi_2022_late
```

Combine the plots for the three seasons
```{r}
combined_plot <- grid.arrange(plot_gpi_2022_early, plot_gpi_2022_mid, plot_gpi_2022_late, nrow = 3)

ggsave("G:/My Drive/PhD/Data/Big data set/combined_biomass_GPI_2022.png", plot = combined_plot, width = 5, height = 10, units = "in")
```



Make model for 2023 GPI
```{r}
str(df2_2023)
df2_2023$GPI <- as.numeric(as.character(df2_2023$GPI))


#early
df2_2023_gpi_early <- df2_2023[df2_2023$season=="early",]
model_gpi2023_early <- glmmTMB(biomass ~ GPI, data = df2_2023_early, family = gaussian(), REML=F)
summary(model_gpi2023_early) 
plot(simulateResiduals(model_gpi2023_early, refit=FALSE))

df2_2023_early_sqrt <- df2_2023[df2_2023$season=="early",]
model_gpi2023_early_sqrt <- glmmTMB(sqrt(biomass) ~ GPI, data = df2_2023_early, family = gaussian(), REML=F)
summary(model_gpi2023_early_sqrt) 
plot(simulateResiduals(model_gpi2023_early_sqrt, refit=FALSE))

df2_2023_early_log <- df2_2023[df2_2023$season=="early",]
model_gpi2023_early_log <- glmmTMB(log(biomass+1) ~ GPI, data = df2_2023_early, family = gaussian(), REML=F)
summary(model_gpi2023_early_log) 
plot(simulateResiduals(model_gpi2023_early_log, refit=FALSE))


AICc(model_gpi2023_early, model_gpi2023_early_sqrt, model_gpi2023_early_log)

#best model
model_gpi2023_early_log <- update(model_gpi2023_early_log,REML=T)
summary(model_gpi2023_early_log) 


#mid
df2_2023_gpi_mid <- df2_2023[df2_2023$season=="mid",]
model_gpi2023_mid <- glmmTMB(biomass ~ GPI, data = df2_2023_mid, family = gaussian(), REML=F)
summary(model_gpi2023_mid) 
plot(simulateResiduals(model_gpi2023_mid, refit=FALSE))

df2_2023_mid_sqrt <- df2_2023[df2_2023$season=="mid",]
model_gpi2023_mid_sqrt <- glmmTMB(sqrt(biomass) ~ GPI, data = df2_2023_mid, family = gaussian(), REML=F)
summary(model_gpi2023_mid_sqrt) 
plot(simulateResiduals(model_gpi2023_mid_sqrt, refit=FALSE))

df2_2023_mid_log <- df2_2023[df2_2023$season=="mid",]
model_gpi2023_mid_log <- glmmTMB(log(biomass+1) ~ GPI, data = df2_2023_mid, family = gaussian(), REML=F)
summary(model_gpi2023_mid_log)
plot(simulateResiduals(model_gpi2023_mid_log, refit=FALSE))


AICc(model_gpi2023_mid, model_gpi2023_mid_sqrt, model_gpi2023_mid_log)

#best model
model_gpi2023_mid_log <- update(model_gpi2023_mid_log,REML=T)
summary(model_gpi2023_mid_log) 


#late
df2_2023_gpi_late <- df2_2023[df2_2023$season=="late",]
model_gpi2023_late <- glmmTMB(biomass ~ GPI, data = df2_2023_late, family = gaussian(), REML=F)
summary(model_gpi2023_late) 
plot(simulateResiduals(model_gpi2023_late, refit=FALSE))

df2_2023_late_sqrt <- df2_2023[df2_2023$season=="late",]
model_gpi2023_late_sqrt <- glmmTMB(sqrt(biomass) ~ GPI, data = df2_2023_late, family = gaussian(), REML=F)
summary(model_gpi2023_late_sqrt) 
plot(simulateResiduals(model_gpi2023_late_sqrt, refit=FALSE))

df2_2023_late_log <- df2_2023[df2_2023$season=="late",]
model_gpi2023_late_log <- glmmTMB(log(biomass+1) ~ GPI, data = df2_2023_late, family = gaussian(), REML=F)
summary(model_gpi2023_late_log)
plot(simulateResiduals(model_gpi2023_late_log, refit=FALSE))


AICc(model_gpi2023_late, model_gpi2023_late_sqrt, model_gpi2023_late_log)

#best model
model_gpi2023_late_log <- update(model_gpi2023_late_log,REML=T)
summary(model_gpi2023_late_log) 
```

Make graph for 2023 GPI
```{r}
#Early
# extract model predictions 
pred_early_2023_gpi <- ggpredict(model_gpi2023_early_log,terms=c("GPI"),back_transform=T)
# extract observed data
df2_2023_early$pred_biomass_early <- predict(model_gpi2023_early_log,newdata=df2_2023_early,re.form = NULL, type="response")

plot_gpi_2023_early <- ggplot() +
  geom_point(data = df2_2023_early, aes(x = GPI, y = biomass), alpha = 0.5) +
  geom_line(data = pred_early_2023_gpi, aes(x = x, y = predicted), color = "lightgrey", lwd = 1) +
  geom_ribbon(data = pred_early_2023_gpi, aes(x = x, ymin = conf.low, ymax = conf.high), fill = "lightgrey", alpha = 0.2) +
  labs(x = NULL, y = NULL, title = NULL) +
  xlim(c(721,731)) +
  ylim(c(0,100)) +
  theme_minimal()+
  theme(axis.title = element_text(size=14,face="bold"),
        axis.text = element_text(size=12),
        legend.title = element_text(size=13,face="bold"),
        legend.text = element_text(size=13),
        title = element_text(size=14,face="bold"))
print(plot_gpi_2023_early)



#mid
# extract model predictions 
pred_mid_2023_gpi <- ggpredict(model_gpi2023_mid_sqrt,terms=c("GPI"),back_transform=T)
# extract observed data
df2_2023_mid$pred_biomass_mid <- predict(model_gpi2023_mid_sqrt,newdata=df2_2023_mid,re.form = NULL, type="response")

plot_gpi_2023_mid <- ggplot() +
  geom_point(data = df2_2023_mid, aes(x = GPI, y = biomass), alpha = 0.5) +
  geom_line(data = pred_mid_2023_gpi, aes(x = x, y = predicted), color = "lightgrey", lwd = 1) +
  geom_ribbon(data = pred_mid_2023_gpi, aes(x = x, ymin = conf.low, ymax = conf.high), fill = "lightgrey", alpha = 0.2) +
  labs(x = NULL, y = NULL, title = NULL) +
  xlim(c(721,731)) +
  ylim(c(0,100)) +
  theme_minimal()+
  theme(axis.title = element_text(size=14,face="bold"),
        axis.text = element_text(size=12),
        legend.title = element_text(size=13,face="bold"),
        legend.text = element_text(size=13),
        title = element_text(size=14,face="bold"))
print(plot_gpi_2023_mid)

#late
# extract model predictions 
pred_late_2023_gpi <- ggpredict(model_gpi2023_late_sqrt,terms=c("GPI"),back_transform=T)
# extract observed data
df2_2023_late$pred_biomass_late <- predict(model_gpi2023_late_sqrt,newdata=df2_2023_late,re.form = NULL, type="response")

plot_gpi_2023_late <- ggplot() +
  geom_point(data = df2_2023_late, aes(x = GPI, y = biomass), alpha = 0.5) +
  geom_line(data = pred_late_2023_gpi, aes(x = x, y = predicted), color = "lightgrey", lwd = 1) +
  geom_ribbon(data = pred_late_2023_gpi, aes(x = x, ymin = conf.low, ymax = conf.high), fill = "lightgrey", alpha = 0.2) +
  labs(x = "Grassland productivity index", y = NULL, title = NULL) +
  xlim(c(721,731)) +
  ylim(c(0,100)) +
  theme_minimal()+
  theme(axis.title = element_text(size=14,face="bold"),
        axis.text = element_text(size=12),
        legend.title = element_text(size=13,face="bold"),
        legend.text = element_text(size=13),
        title = element_text(size=14,face="bold"))
print(plot_gpi_2023_late)
```

Combine the plots for the three seasons
```{r}
combined_plot <- grid.arrange(plot_gpi_2023_early, plot_gpi_2023_mid, plot_gpi_2023_late, nrow = 3)

# Save the combined plot to a file
ggsave("G:/My Drive/PhD/Data/Big data set/combined_biomass_GPI_2023.png", plot = combined_plot, width = 5, height = 10, units = "in")
```







Make new sub datasets for the big graph 2022 GPI
```{r}
#early
df2_2022_early_gpi_sub <- df2_2022_gpi_early[,c("GPI", "biomass")]
df2_2022_early_gpi_sub$season <- "Early" 
df2_2022_early_gpi_sub$year <- 2022

pred_early_2022_gpi$season <- "Early"
pred_early_2022_gpi$year <- 2022

#mid
df2_2022_mid_gpi_sub <- df2_2022_mid[,c("GPI", "biomass")]
df2_2022_mid_gpi_sub$season <- "Mid" 
df2_2022_mid_gpi_sub$year <- 2022

pred_mid_2022_gpi$season <- "Mid"
pred_mid_2022_gpi$year <- 2022

#late
df2_2022_late_gpi_sub <- df2_2022_late[,c("GPI", "biomass")]
df2_2022_late_gpi_sub$season <- "Late" 
df2_2022_late_gpi_sub$year <- 2022

pred_late_2022_gpi$season <- "Late"
pred_late_2022_gpi$year <- 2022
```

Make new sub datasets for the big graph 2023 GPI
```{r}
#early
df2_2023_early_gpi_sub <- df2_2023_gpi_early[,c("GPI", "biomass")]
df2_2023_early_gpi_sub$season <- "Early" 
df2_2023_early_gpi_sub$year <- 2023

pred_early_2023_gpi$season <- "Early"
pred_early_2023_gpi$year <- 2023

#mid
df2_2023_mid_gpi_sub <- df2_2023_mid[,c("GPI", "biomass")]
df2_2023_mid_gpi_sub$season <- "Mid" 
df2_2023_mid_gpi_sub$year <- 2023

pred_mid_2023_gpi$season <- "Mid"
pred_mid_2023_gpi$year <- 2023

#late
df2_2023_late_gpi_sub <- df2_2023_late[,c("GPI", "biomass")]
df2_2023_late_gpi_sub$season <- "Late" 
df2_2023_late_gpi_sub$year <- 2023

pred_late_2023_gpi$season <- "Late"
pred_late_2023_gpi$year <- 2023
```

All gpi graphs combined into one big one GPI
```{r}
df2_gpi <- rbind(df2_2022_early_gpi_sub, df2_2022_mid_gpi_sub, df2_2022_late_gpi_sub, df2_2023_early_gpi_sub, df2_2023_mid_gpi_sub, df2_2023_late_gpi_sub)

df2_gpi$season <- factor(df2_gpi$season, levels=c("Early", "Mid", "Late"))
table(df2_gpi$season)

df2_gpi_pred <- rbind(pred_early_2022_gpi, pred_mid_2022_gpi, pred_late_2022_gpi, pred_early_2023_gpi, pred_mid_2023_gpi, pred_late_2023_gpi)

df2_gpi_pred$season <- factor(df2_gpi_pred$season, levels=c("Early", "Mid", "Late"))
table(df2_gpi_pred$season)

label_df$year <- factor(label_df$year)
df2_gpi$year <- factor(df2_gpi$year)
df2_gpi_pred$year <- factor(df2_gpi_pred$year)



plot_gpi <- ggplot() +
  geom_point(data = df2_gpi, aes(x = GPI, y = biomass), alpha = 0.5) +
  geom_line(data = df2_gpi_pred, aes(x = x, y = predicted), color = "grey50", lwd = 1) +
  geom_ribbon(data = df2_gpi_pred, aes(x = x, ymin = conf.low, ymax = conf.high), fill = "grey70", alpha = 0.2) +
  facet_grid(factor(season) ~ year, scales = "free") +
  labs(x = "Grassland productivity index", y = "Biomass (in g)", title = NULL) +
  theme_bw()+
  theme(axis.title = element_text(size=10,face="bold"),
        axis.text = element_text(size=8),
        legend.title = element_text(size=9,face="bold"),
        legend.text = element_text(size=9),
        title = element_text(size=10,face="bold"))
plot_gpi

label_df <- data.frame(
  year = c(2022, 2022, 2022, 2023, 2023, 2023),
  season = factor(c("Early", "Mid", "Late", "Early", "Mid", "Late"), 
                  levels = c("Early", "Mid", "Late")),
  x = c(729, 729, 729, 729, 725, 729),
  y = c(58, 90, 78, 58, 90, 78),  
  label = c("p=0.718", "p=0.377", "p=0.105", "p=0.552", "p=0.012, 
            estimate = 0.056", "p=0.361") 
)


plot_gpi <- plot_gpi +
  geom_text(data = label_df,
            aes(x = x, y = y, label = label),
            size = 3, color = "black", fontface = "bold")



ggsave("G:/My Drive/PhD/Data/Big data set/combined_biomass_GPI.png", plot = plot_gpi, width = 5, height = 7, units = "in")
```