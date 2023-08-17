# notes ----
#Create master csv of ecosystem indicators 
# Assess collinearity b/w BBRKC indicators for BAS
# Assess appropriate lags b/w indicators and response (Mature male biomass)
#Create indicator timeseries plot 


# Erin Fedewa
# last updated: 2022/8/22

# load ----
library(tidyverse)
library(corrplot)
library(cowplot)
library(mgcv)

# data ----
econ<- read_csv("./Data/BBRKCsoceconindicators.csv")
#Still need 2021/2022 updates for incidental catch!

#Ecosystem data to combine
invert <- read_csv("./Output/BBbenthic_timeseries.csv")
pred <- read_csv("./Output/BBpred_timeseries.csv")
env <- read_csv("./Output/environmental_timeseries.csv")
recruit <- read_csv("./Output/prerecruit_timeseries.csv")
d95 <- read_csv("./Output/D95_output.csv")

# combine indices and save output
invert %>%
  select(YEAR, Total_Benthic) %>%
  full_join(pred %>%
              select(YEAR, Pcod)) %>%
  full_join(env) %>%
  full_join(recruit %>%
              rename(YEAR=SURVEY_YEAR)) %>%
  full_join(d95 %>%
               select(YEAR, mature_female, mature_male)) %>%
  rename_all(~c("year", "beninvert_cpue",
                "pcod_cpue", "bottom_temp", "cp_extent", "mean_ao", "recruit_abun", 
                "mat_fem_d95", "mat_male_d95")) %>%
  filter(year >= 1980) %>%
  arrange(year) -> eco_ind

write_csv(eco_ind, "./Data/BBRKCindicators.csv")
#Still need 2021/2022 updates for chla, wind stress and OA! 

#Assess collinearity b/w indicators 
eco_ind %>% 
  select(-year) %>%
  cor(use = "complete.obs") %>%
  corrplot(method="number")

################################################
#Ecosystem Plots ----

eco_ind <- read_csv("./Data/BBRKCindicators.csv")

eco_ind %>%
  ## pH
  select(year, pH) %>%
  ggplot(aes(x = year, y = pH))+
  geom_point(size=3)+
  geom_line() +
  #geom_smooth(method = gam, formula = y~s(x, bs = "cs")) +
  geom_hline(aes(yintercept = mean(pH, na.rm=TRUE)), linetype = 5)+
  geom_hline(aes(yintercept = quantile(pH, .10, na.rm=TRUE)), linetype = 3)+
  geom_hline(aes(yintercept = quantile(pH, .90, na.rm=TRUE)), linetype = 3)+
  annotate("rect", xmin=2021.5 ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "pH", x = "") +
  theme_bw() +
  ggtitle("Spring pH")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) -> ph


eco_ind %>%
  ## Pcod CPUE
  select(year, pcod_cpue) %>%
  ggplot(aes(x = year, y = pcod_cpue))+
  geom_point(size=3)+
  geom_line() +
  #geom_smooth(method = gam, formula = y~s(x, bs = "cs")) +
  geom_hline(aes(yintercept = mean(pcod_cpue, na.rm=TRUE)), linetype = 5)+
  geom_hline(aes(yintercept = quantile(pcod_cpue, .10, na.rm=TRUE)), linetype = 3)+
  geom_hline(aes(yintercept = quantile(pcod_cpue, .90, na.rm=TRUE)), linetype = 3)+
  annotate("rect", xmin=2021.5 ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "Pacific Cod CPUE (1000t/km2)", x = "") +
  theme_bw() +
  ggtitle("Pacific Cod CPUE")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) -> cod 

eco_ind %>%
  ## Invert CPUE
  select(year, beninvert_cpue) %>%
  ggplot(aes(x = year, y = beninvert_cpue))+
  geom_point(size=3)+
  geom_line() +
  #geom_smooth(method = gam, formula = y~s(x, bs = "cs")) +
  geom_hline(aes(yintercept = mean(beninvert_cpue, na.rm=TRUE)), linetype = 5)+
  geom_hline(aes(yintercept = quantile(beninvert_cpue, .10, na.rm=TRUE)), linetype = 3)+
  geom_hline(aes(yintercept = quantile(beninvert_cpue, .90, na.rm=TRUE)), linetype = 3)+
  annotate("rect", xmin=2021.5 ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "Benthic Invert CPUE (1000t/km2)", x = "") +
  theme_bw() +
  ggtitle("Benthic Invert CPUE")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) -> invert 

eco_ind %>%
  #Summer Bottom Temp  
  select(year, bottom_temp ) %>%
  ggplot(aes(x = year, y = bottom_temp ))+
  geom_point(size=3)+
  geom_line() +
  geom_hline(aes(yintercept = mean(bottom_temp, na.rm=TRUE)), linetype = 5)+
  geom_hline(aes(yintercept = quantile(bottom_temp, .10, na.rm=TRUE)), linetype = 3)+
  geom_hline(aes(yintercept = quantile(bottom_temp, .90, na.rm=TRUE)), linetype = 3)+
  annotate("rect", xmin=2021.5 ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = expression("Temperature ("*~degree*C*")"), x = "") +
  theme_bw() +
  ggtitle("St. Matthew Summer Bottom Temperature")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) -> sumtemp

eco_ind %>%
  #Cold Pool Spatial Extent  
  select(year, cp_extent) %>%
  ggplot(aes(x = year, y = cp_extent))+
  geom_point(size=3)+
  geom_line() +
  geom_hline(aes(yintercept = mean(cp_extent, na.rm=TRUE)), linetype = 5)+
  geom_hline(aes(yintercept = quantile(cp_extent, .10, na.rm=TRUE)), linetype = 3)+
  geom_hline(aes(yintercept = quantile(cp_extent, .90, na.rm=TRUE)), linetype = 3)+
  annotate("rect", xmin=2021.5 ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "Spatial extent (nmi2)", x = "") +
  theme_bw() +
  ggtitle("Cold Pool Spatial Extent")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) -> cp

eco_ind %>%
  ## Pre-recruit abundance  
  select(year, recruit_abun) %>%
  ggplot(aes(x = year, y = recruit_abun))+
  geom_point(size=3)+
  geom_line() +
  geom_hline(aes(yintercept = mean(recruit_abun, na.rm=TRUE)), linetype = 5)+
  geom_hline(aes(yintercept = quantile(recruit_abun, .10, na.rm=TRUE)), linetype = 3)+
  geom_hline(aes(yintercept = quantile(recruit_abun, .90, na.rm=TRUE)), linetype = 3)+
  annotate("rect", xmin=2021.5 ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "Recruit Abundance (millions crab)", x = "") +
  theme_bw() +
  ggtitle("SMBKC Recruit Abundance")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) -> recruit

eco_ind %>%
  ## Mature female area occupied  
  select(year, mat_fem_d95) %>%
  ggplot(aes(x = year, y = mat_fem_d95))+
  geom_point(size=3)+
  geom_line() +
  geom_hline(aes(yintercept = mean(mat_fem_d95, na.rm=TRUE)), linetype = 5)+
  geom_hline(aes(yintercept = quantile(mat_fem_d95, .10, na.rm=TRUE)), linetype = 3)+
  geom_hline(aes(yintercept = quantile(mat_fem_d95, .90, na.rm=TRUE)), linetype = 3)+
  annotate("rect", xmin=2021.5 ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "Area occupied (nm2)", x = "") +
  theme_bw() +
  ggtitle("Mature Female Area Occupied")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) -> femarea

eco_ind %>%
  ## Mature male area occupied  
  select(year, mat_male_d95) %>%
  ggplot(aes(x = year, y = mat_male_d95))+
  geom_point(size=3)+
  geom_line() +
  geom_hline(aes(yintercept = mean(mat_male_d95, na.rm=TRUE)), linetype = 5)+
  geom_hline(aes(yintercept = quantile(mat_male_d95, .10, na.rm=TRUE)), linetype = 3)+
  geom_hline(aes(yintercept = quantile(mat_male_d95, .90, na.rm=TRUE)), linetype = 3)+
  annotate("rect", xmin=2021.5 ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "Area occupied (nm2)", x = "") +
  theme_bw() +
  ggtitle("Mature Male Area Occupied")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) -> malearea

## Create combined plots 
plot_grid(cp, sumtemp, cod, invert + theme(legend.position = "none"),  
          label_size = 10,
          hjust = -4.5, vjust = 2.5,
          nrow = 4, align = "hv", axis = "l")  -> Fig1a
ggsave(filename = "./Figs/Fig1a.png", device = "png", width = 6, height = 12, 
       dpi = 300)

plot_grid(recruit, femarea, malearea + theme(legend.position = "none"),  
          label_size = 10,
          hjust = -4.5, vjust = 2.5,
          nrow = 3, align = "hv", axis = "l")  -> Fig1b
ggsave(filename = "./Figs/Fig1b.png", device = "png", width = 6, height = 12, 
       dpi = 300)

#############################################
#Socioeconomic Plots ----

econ %>%
  ## Vessels
  select(Year, Vessels) %>%
  ggplot(aes(x = Year, y = Vessels))+
  geom_point(size=3)+
  geom_line() +
  #geom_smooth(method = gam, formula = y~s(x, bs = "cs")) +
  geom_hline(aes(yintercept = mean(Vessels, na.rm=TRUE)), linetype = 5)+
  geom_hline(aes(yintercept = quantile(Vessels, .10, na.rm=TRUE)), linetype = 3)+
  geom_hline(aes(yintercept = quantile(Vessels, .90, na.rm=TRUE)), linetype = 3)+
  annotate("rect", xmin=2019.5 ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = expression(atop("Number of", "Active Vessels")), x = "")+
  scale_x_continuous(breaks = x_axis$breaks, labels = x_axis$labels)+
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Vessels active in fishery")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) -> vessels 

econ %>%
  ## Bycatch
  select(Year, Bycatch) %>%
  ggplot(aes(x = Year, y = Bycatch))+
  geom_point(size=3)+
  geom_line() +
  #geom_smooth(method = gam, formula = y~s(x, bs = "cs")) +
  geom_hline(aes(yintercept = mean(Bycatch, na.rm=TRUE)), linetype = 5)+
  geom_hline(aes(yintercept = quantile(Bycatch, .10, na.rm=TRUE)), linetype = 3)+
  geom_hline(aes(yintercept = quantile(Bycatch, .90, na.rm=TRUE)), linetype = 3)+
  annotate("rect", xmin=2019.5 ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = expression(atop("SMBKC Bycatch", "Biomass (t)")), x = "")+
  scale_x_continuous(breaks = x_axis$breaks, labels = x_axis$labels)+
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("SMBKC Male Bycatch")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) ->bycatch

econ %>%
  ## Potlifts
  select(Year, Total_Potlifts ) %>%
  ggplot(aes(x = Year, y = Total_Potlifts))+
  geom_point(size=3)+
  geom_line() +
  #geom_smooth(method = gam, formula = y~s(x, bs = "cs")) +
  geom_hline(aes(yintercept = mean(Total_Potlifts, na.rm=TRUE)), linetype = 5)+
  geom_hline(aes(yintercept = quantile(Total_Potlifts, .10, na.rm=TRUE)), linetype = 3)+
  geom_hline(aes(yintercept = quantile(Total_Potlifts, .90, na.rm=TRUE)), linetype = 3)+
  annotate("rect", xmin=2019.5 ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = expression(atop("Number of", "crab pots")), x = "")+
  scale_x_continuous(breaks = x_axis$breaks, labels = x_axis$labels)+
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Total Potlifts")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) -> potlifts

econ %>%
  ## CPUE
  select(Year, CPUE) %>%
  ggplot(aes(x = Year, y = CPUE))+
  geom_point(size=3)+
  geom_line() +
  #geom_smooth(method = gam, formula = y~s(x, bs = "cs")) +
  geom_hline(aes(yintercept = mean(CPUE, na.rm=TRUE)), linetype = 5)+
  geom_hline(aes(yintercept = quantile(CPUE, .10, na.rm=TRUE)), linetype = 3)+
  geom_hline(aes(yintercept = quantile(CPUE, .90, na.rm=TRUE)), linetype = 3)+
  annotate("rect", xmin=2019.5 ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = expression(atop("Mean # of retained", "BKC per potlift")), x = "")+
  scale_x_continuous(breaks = x_axis$breaks, labels = x_axis$labels)+
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("CPUE")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5))-> cpue

econ %>%
  ## Price per pound
  select(Year, Price_per_pound) %>%
  ggplot(aes(x = Year, y = Price_per_pound))+
  geom_point(size=3)+
  geom_line() +
  #geom_smooth(method = gam, formula = y~s(x, bs = "cs")) +
  geom_hline(aes(yintercept = mean(Price_per_pound, na.rm=TRUE)), linetype = 5)+
  geom_hline(aes(yintercept = quantile(Price_per_pound, .10, na.rm=TRUE)), linetype = 3)+
  geom_hline(aes(yintercept = quantile(Price_per_pound, .90, na.rm=TRUE)), linetype = 3)+
  annotate("rect", xmin=2019.5 ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = expression(atop("Commercial Value", "per lb landings")), x = "")+
  scale_x_continuous(breaks = x_axis$breaks, labels = x_axis$labels)+
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Ex-vessel Price per pound")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5))->price

econ %>%
  ## Revenue Share 
  select(Year, Revenue_share) %>%
  ggplot(aes(x = Year, y = Revenue_share))+
  geom_point(size=3)+
  geom_line() +
  #geom_smooth(method = gam, formula = y~s(x, bs = "cs")) +
  geom_hline(aes(yintercept = mean(Revenue_share, na.rm=TRUE)), linetype = 5)+
  geom_hline(aes(yintercept = quantile(Revenue_share, .10, na.rm=TRUE)), linetype = 3)+
  geom_hline(aes(yintercept = quantile(Revenue_share, .90, na.rm=TRUE)), linetype = 3)+
  annotate("rect", xmin=2019.5 ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "Revenue Share", x = "")+
  scale_x_continuous(breaks = x_axis$breaks, labels = x_axis$labels)+
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Ex-vessel revenue share")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5))-> rev

econ %>%
  ## Pcod Biomass
  select(Year, Processors) %>%
  ggplot(aes(x = Year, y = Processors))+
  geom_point(size=3)+
  geom_line() +
  #geom_smooth(method = gam, formula = y~s(x, bs = "cs")) +
  geom_hline(aes(yintercept = mean(Processors, na.rm=TRUE)), linetype = 5)+
  geom_hline(aes(yintercept = quantile(Processors, .10, na.rm=TRUE)), linetype = 3)+
  geom_hline(aes(yintercept = quantile(Processors, .90, na.rm=TRUE)), linetype = 3)+
  annotate("rect", xmin=2019.5 ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = expression(atop("# of Active", "Processors")), x = "")+
  scale_x_continuous(breaks = x_axis$breaks, labels = x_axis$labels)+
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Processors active in fishery")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5))->proc

econ %>%
  ## Local Quotient
  select(Year, Local_Quotient) %>%
  ggplot(aes(x = Year, y = Local_Quotient))+
  geom_point(size=3)+
  geom_line() +
  #geom_smooth(method = gam, formula = y~s(x, bs = "cs")) +
  geom_hline(aes(yintercept = mean(Local_Quotient, na.rm=TRUE)), linetype = 5)+
  geom_hline(aes(yintercept = quantile(Local_Quotient, .10, na.rm=TRUE)), linetype = 3)+
  geom_hline(aes(yintercept = quantile(Local_Quotient, .90, na.rm=TRUE)), linetype = 3)+
  annotate("rect", xmin=2019.5 ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = expression(atop("Ex-vessel", "value share")), x = "")+
  scale_x_continuous(breaks = x_axis$breaks, labels = x_axis$labels)+
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Local quotient of catch landed in St. Paul")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5))-> lq

econ %>%
  ## TaC Utilization
  select(Year, TAC_Utilization) %>%
  ggplot(aes(x = Year, y = TAC_Utilization))+
  geom_point(size=3)+
  geom_line() +
  #geom_smooth(method = gam, formula = y~s(x, bs = "cs")) +
  geom_hline(aes(yintercept = mean(TAC_Utilization, na.rm=TRUE)), linetype = 5)+
  geom_hline(aes(yintercept = quantile(TAC_Utilization, .10, na.rm=TRUE)), linetype = 3)+
  geom_hline(aes(yintercept = quantile(TAC_Utilization, .90, na.rm=TRUE)), linetype = 3)+
  annotate("rect", xmin=2019.5 ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = expression(atop("% of TAC", "Harvested")), x = "")+
  scale_x_continuous(breaks = x_axis$breaks, labels = x_axis$labels)+
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("TAC Utilization")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5))-> tac

## Create combined plots -----
#Create Figure for ESP doc - 
plot_grid(vessels, tac, potlifts,cpue, price  + theme(legend.position = "none"),  
          label_size = 10,
          hjust = -4.5, vjust = 2.5,
          nrow = 5, align = "hv", axis = "l")  -> Fig2a

## write plot
ggsave(filename = "./Figs/Fig2a.png", device = "png", width = 6, height = 12, 
       dpi = 300)

plot_grid(rev, proc, lq, bycatch + theme(legend.position = "none"),  
          label_size = 10,
          hjust = -4.5, vjust = 2.5,
          nrow = 4, align = "hv", axis = "l")  -> Fig2b

## write plot
ggsave(filename = "./Figs/Fig2b.png", device = "png", width = 6, height = 10, 
       dpi = 300)





