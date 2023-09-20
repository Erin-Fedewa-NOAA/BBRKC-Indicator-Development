# notes ----
# Examine fecundity and mean egg size of BBRKC ovigerous females (Gardener et al study)  

# Erin Fedewa
# last updated: 2020/4/22

#2024 to do:
  #Add in proportion full/proportion empty as presented in SAFE chp
  #Consult with Ben Daly on GOA RKC clutch data for context on reproductive failures 
  #(i.e. to set threshold for "red flag" concerns)

# load ----
library(tidyverse)
library(janitor)

# data ----
dat <- read_csv("./Data/BBRKC_fecundity.csv")

#Clean up names
dat %>%
  clean_names() %>%
  rename(Year=year_3) -> data

#BBRKC Egg Weight  ----

#Plots
data %>%
  group_by(Year) %>%
  summarise(MEW = mean (indv_egg_wt_g )*1000,
            SD_EW = sd(indv_egg_wt_g )*1000 ) %>%
  ggplot(aes(Year, MEW)) +
    geom_bar(stat = "identity") +
   theme_bw() +
    ylab("Mean individual egg weight (mg)") +
   geom_errorbar(aes(ymin = MEW - SD_EW, ymax = MEW + SD_EW), width=.2,
                position=position_dodge(.9))

#ANOVA
m1 <- aov(indv_egg_wt_g ~ as.factor(Year), data = data)
summary(m1)
TukeyHSD(m1)  

#Fit fecundity regressions
lm_fit <- lm(fecundity ~ cl_mm + Year, data=data)
summary(lm_fit)
#Save predictions of the model
predicted_df <- data.frame(fec_pred = predict(lm_fit, data), fec=data$fecundity)

data %>%
  mutate(fecundity = fecundity/1000) %>%
  select(Year, cl_mm, fecundity) %>%
  ggplot(aes(x = cl_mm, y = fecundity, group = as.factor(Year), colour = as.factor(Year)))+
  geom_point() + 
  geom_smooth(method = "lm") +
  scale_fill_brewer() +
  labs(y = "Fecundity (thousands of embryos)", x = "Carapace Length (mm)")+
  theme_bw()+
  theme(legend.title = element_blank()) +
  theme(panel.grid = element_blank()) 


  