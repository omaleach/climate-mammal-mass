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

######################################################
# Mean mass 
hare_mass <- spring.hare %>%
  filter(Year>=1978)  %>% #one observation 1977
  group_by(Year) %>%
  summarise(
    meanmass = mean(Weight, na.rm = TRUE))
meanhare <- glmmTMB(meanmass ~ Year, data = hare_mass, family = gaussian(link=identity))
summary(meanhare)
r.squaredGLMM(meanhare)

red_mass <- spring.red %>%
  group_by(Year) %>%
  summarise(
    meanmass = mean(Weight, na.rm = TRUE))
meanred <- glmmTMB(meanmass ~ Year, data = red_mass, family = gaussian(link=identity))
summary(meanred)
r.squaredGLMM(meanred)

mice_mass <- spring.mice %>%
  group_by(Year) %>%
  summarise(
    meanmass = mean(Weight, na.rm = TRUE))
meanmice <- glmmTMB(meanmass ~ Year, data = mice_mass, family = gaussian(link=identity))
summary(meanmice)
r.squaredGLMM(meanmice)

#plot mean mass
# Spring hares
hare.mean <- ggplot(data = hare_mass, aes(x = Year, y = meanmass)) +
  geom_point(color = "grey", size = 1) +  
  stat_smooth(method = "glm", formula = y ~ x, color = "black") + 
  labs(x = "Year", y = "Mean Spring Body Mass (g)", title = "A") +
  theme_classic() +  
  theme(
    plot.title=element_text(hjust = 0, size=20),
    axis.text.x = element_text(size=16),
    axis.text.y = element_text(size=16),
    axis.title.x = element_text(face="bold", size = 19, margin = margin(t=18)),
    axis.title.y = element_text(face="bold", size = 19, margin = margin(r=18)))
hare.mean
# Spring redbacks
red.mean <- ggplot(data = red_mass, aes(x = Year, y = meanmass)) +
  geom_point(color = "grey", size = 1) +  
  stat_smooth(method = "glm", formula = y ~ x, color = "black") + 
  labs(x = "Year", y = "", title = "B") +
  theme_classic() +  
  theme(
    plot.title=element_text(hjust = 0, size=20),
    axis.text.x = element_text(size=16),
    axis.text.y = element_text(size=16),
    axis.title.x = element_text(face="bold", size = 19, margin = margin(t=18)),
    axis.title.y = element_text(face="bold", size = 19, margin = margin(r=18)))
red.mean
# Spring mice 
mice.mean <- ggplot(data = mice_mass, aes(x = Year, y = meanmass)) +
  geom_point(color = "grey", size = 1) +  
  stat_smooth(method = "glm", formula = y ~ x, color = "gray") + 
  labs(x = "Year", y = "", title = "C") +
  theme_classic() +  
  theme(
    plot.title=element_text(hjust = 0, size=20),
    axis.text.x = element_text(size=16),
    axis.text.y = element_text(size=16),
    axis.title.x = element_text(face="bold", size = 19, margin = margin(t=18)),
    axis.title.y = element_text(face="bold", size = 19, margin = margin(r=18)))
mice.mean
# Arrange the plots
grid.arrange(hare.mean, red.mean, mice.mean,
             nrow = 1, ncol = 3, padding = unit(4, "line"))
