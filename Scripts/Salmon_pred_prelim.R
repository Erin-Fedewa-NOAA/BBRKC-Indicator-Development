#Prelim plot of sockeye salmon prey

library(tidyverse)
library(ggsci)
library(RColorBrewer)

dat <- read.csv("./Data/Salmon_diet_prelim.csv")

dat %>%
  mutate(Age0_pollock = (Age0_pollock/Sum)*100,
         Amphipods = (Amphipods/Sum)*100,
         Arrow_worm = (Arrow_worm/Sum)*100,
         Copepod = (Copepod/Sum)*100, 
         Crustaceans = (Crustaceans/Sum)*100,
         Fish = (Fish/Sum)*100,
         Krill = (Krill/Sum)*100,
         Pteropods = (Pteropods/Sum)*100,
         Other = (Other/Sum)*100) %>%
  select(1:10) %>%
  filter(Year != "Grand_Total",
         Year != 2013) %>%
  pivot_longer(2:10, names_to = "Prey_Item", values_to = "Percent") -> dat2
  
dat2 %>%
ggplot(aes(fill=Prey_Item, y=Percent, x=Year )) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_jco()
  

  