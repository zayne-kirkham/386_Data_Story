#############
# Libraries #
#############
library(tidyverse)
library(ggmosaic)
library(patchwork)
library(ggh4x) 
###############################
# Read in and split the data  #
###############################
data <- read_csv("filtered_cleaned_traffic_data.csv")
data <- data %>% 
  mutate(Violation_Type = case_when(Violation_Type == 1 ~ "Citation",
                                    Violation_Type == 0 ~ "Warning"))

alcohol_night_df <- data %>% 
  filter(Alcohol == "Yes")%>%
  filter(shift == 'Night')

alcohol_day_df <- data %>% 
  filter(Alcohol == "Yes")%>%
  filter(shift == 'Day')

no_alcohol_night_df <- data %>% 
  filter(Alcohol == "No")%>%
  filter(shift == 'Night')

no_alcohol_day_df <- data %>% 
  filter(Alcohol == "No")%>%
  filter(shift == 'Day')
  

################
# Context Plot #
################
context_plot <- data %>% 
  ggplot()+
  geom_mosaic(aes(x = product(shift, Alcohol), 
                  fill = shift)) +
  labs(y="Shift", 
       x="Alcohol", 
       title = "Alcohol Presence by Shift",
       subtitle = "The variables by which the subsequent plots are split on")

##########################
# alcohol | Night mosaic #
##########################
alcohol_night_df%>%
  ggplot()+
    geom_mosaic(aes(x = product(Violation_Type, season),
                    alpha=Gender,
                    fill = Race))  +
    theme(axis.text.x = element_text(angle = 90, 
                                     hjust = 1, 
                                     vjust = .5),
          legend.title = element_text(size = 10), 
          legend.text = element_text(size = 8),
          legend.key.size = unit(.5, "lines"),
          legend.position = c(1.15, 0.5)) + 
  guides(y = guide_axis(n.dodge = 1, check.overlap = T),
         y.sec = guide_axis_manual(breaks = unit(c(.93), "npc"),
                                   labels = expression("HISPANIC:Warning"))
         )+
    scale_alpha_discrete(range=c(.5,1))+
    labs(y="Race:Violation", 
         x="Sex:Season", 
         title = "Alcohol = YES | Shift = Night") -> alcohol_night_plot

########################
# alcohol | Day mosaic #
########################
alcohol_day_df%>%
  ggplot()+
  geom_mosaic(aes(x = product(Violation_Type, season),
                  alpha=Gender,
                  fill = Race))  +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1, 
                                   vjust = .5),
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 8),
        legend.key.size = unit(.5, "lines"),
        legend.position = c(1.15, 0.5)) + 
  guides(y = guide_axis(n.dodge = 1, check.overlap = T),
         y.sec = guide_axis_manual(breaks = unit(c(.93), "npc"),
                               labels = expression("HISPANIC:Warning")),
         )+
  scale_alpha_discrete(range=c(.5,1))+
  labs(y = "Race:Violation",
       x="Sex:Season", 
       title = "Alcohol = YES | Shift = Day") -> alcohol_day_plot

#############################
# No Alcohol | Night mosaic #
#############################
no_alcohol_night_df%>%
  ggplot()+
  geom_mosaic(aes(x = product(Violation_Type, season),
                  alpha=Gender,
                  fill = Race))  +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1, 
                                   vjust = .5),
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 8),
        legend.key.size = unit(.5, "lines")) + 
  guides(y = guide_axis(n.dodge = 1, check.overlap = T),
         shape = guide_legend(override.aes = list(size = 0.5)),
         color = guide_legend(override.aes = list(size = 0.5)))+
  scale_alpha_discrete(range=c(.5,1))+
  labs(y="Race:Violation", 
       x="Sex:Season", 
       title = "Alcohol = No | Shift = Night") -> no_alcohol_night_plot

###########################
# No Alcohol | Day mosaic #
###########################

no_alcohol_day_df%>%
  ggplot()+
  geom_mosaic(aes(x = product(Violation_Type, season),
                  alpha=Gender,
                  fill = Race))  +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1, 
                                   vjust = .5),
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 8),
        legend.key.size = unit(.5, "lines")) + 
  guides(y = guide_axis(n.dodge = 1, check.overlap = T),
         shape = guide_legend(override.aes = list(size = 0.5)),
         color = guide_legend(override.aes = list(size = 0.5)))+
  scale_alpha_discrete(range=c(.5,1))+
  labs(y="Race:Violation", 
       x="Sex:Season", 
       title = "Alcohol = No | Shift = Day") -> no_alcohol_day_plot


#############################
# Stitching it all together #
#############################
context_plot

alcohol_day_plot 
alcohol_night_plot

no_alcohol_day_plot
no_alcohol_night_plot
  
