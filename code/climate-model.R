# Code for "Overwintering strategies drive climate change impacts on body mass in northern mammals"
# Leach et al. 2026
source("code/packages.R")
source("code/variables.R")
#load data
climate <-read.csv(climate_path)
density <-read.csv(density_path)
spring.hare <-read.csv(spring.hare_path) %>%
  mutate(Sex=as.factor(Sex))
spring.red <-read.csv(spring.red_path)%>%
  mutate(Sex=as.factor(Sex))
spring.mice <-read.csv(spring.mice_path)%>%
  mutate(Sex=as.factor(Sex))

###########################################
#Climate models
climate$mid_Year <- climate$Spring_Year - 1961
temp.mod<-glmmTMB(temp~mid_Year, data = climate)
summary(temp.mod)
snow.mod<-glmmTMB(snow~mid_Year, data = climate)
summary(snow.mod)
climate$mid_Year <- climate$Year - 1961
GDD.mod<-glmmTMB(GDD~mid_Year, data = climate)
summary(GDD.mod)

#Plots
# Temperature over time
tempplot <- ggplot(climate, aes(x = Spring_Year, y = temp)) +
  geom_point(color = "gray", size = 1) +  
  stat_smooth(method = "glm", formula = y ~ x, color = "darkred", linewidth= 1.3, fill = "darkred", alpha=0.2, se=TRUE) + 
  labs(x = "Year", y = "Mean annual winter temp (°C)  ") +
  annotate("text", x = 1961, y = -8, label = "A",  size = 8) +
  theme_classic() +  
  theme(axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        axis.title.x = element_text(face="bold", size = 19, margin = margin(t=18)),
        axis.title.y = element_text(face="bold", size = 19, margin = margin(r=18)))
# Snow over time
snowplot <- ggplot(climate, aes(x = Spring_Year, y = snow)) +
  geom_point(color = "gray", size = 1) +  
  stat_smooth(method = "glm", formula = y ~ x, color = "darkblue", linewidth= 1.3, fill = "darkblue", alpha=0.2, se=TRUE, linetype="dashed") +
  labs(x = "Year",y = "Proportion of days with ≥ 15 cm of snow") +
  annotate("text", x = 1961, y = 160, label = "C",  size = 8) +
  theme_classic() +  
  theme(axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        axis.title.x = element_text(face="bold", size = 19, margin = margin(t=18)),
        axis.title.y = element_text(face="bold", size = 19, margin = margin(r=18)))
# GDD over time
GDDplot <- ggplot(climate, aes(x = Year, y = GDD)) +
  geom_point(color = "gray", size = 1) +  
  stat_smooth(method = "glm", formula = y ~ x, color = "darkgreen", linewidth= 1.3, fill = "darkgreen", alpha=0.2, se=TRUE) + 
  labs(x = "Year", y = "Annual GDD") +
  annotate("text", x = 1961, y = 960, label = "B",  size = 8) +
  theme_classic() +  
  theme(axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        axis.title.x = element_text(face="bold", size = 19, margin = margin(t=18)),
        axis.title.y = element_text(face="bold", size = 19, margin = margin(r=18)))
#plot together
grid.arrange(tempplot, GDDplot, snowplot,  
             nrow = 1, ncol = 3, padding = unit(2, "line"))


