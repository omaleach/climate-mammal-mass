# Code for "Overwintering strategies drive climate change impacts on body mass in northern mammals"
# Leach et al. 2026
rm(list=ls())
library(dplyr)
library(visreg)
library(lubridate)
library(car)
library(ggplot2)
library(glmmTMB)
library(gridExtra)
library(grid)
library(performance)
library(MuMIn)
library(livimals)
library(patchwork)

#load data
climate <-read.csv("climate.csv")
spring.hare <-read.csv("spring.hare.csv") %>%
  mutate(Sex=as.factor(Sex))
spring.red <-read.csv("spring.red.csv")%>%
  mutate(Sex=as.factor(Sex))
spring.mice <-read.csv("spring.mice.csv")%>%
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

# SPECIES MODELS ###################################
###########################
#HARES
#Mass over time
spring.hare$YearCentered <- spring.hare$Year - 1977
spring.hare$DofYc <- spring.hare$DofY - mean(spring.hare$DofY, na.rm = TRUE)
year.hare.mod <- glmmTMB(Weight ~ YearCentered + DofYc + sp_hare + Sex + (1|Eartag), data = spring.hare, family = gaussian(link = identity))
summary(year.hare.mod)
r.squaredGLMM(year.hare.mod)
#Mass with climate variables
spring.hare.mod <- glmmTMB(Weight ~ temp + snow + GDD + DofY + sp_hare + Sex + (1|Eartag), data = spring.hare, family = gaussian(link=identity))
summary(spring.hare.mod)
r.squaredGLMM(spring.hare.mod)

#observations
n <- length(spring.hare$Eartag)
print(n)
hareID <- length(unique(spring.hare$Eartag))
print(hareID)

#############
#REDBACK
#Mass over time
spring.red$YearCentered <- spring.red$Year - 1974
spring.red$DofYc <- spring.red$DofY- mean(spring.red$DofY, na.rm = TRUE)
year.red.mod <- glmmTMB(Weight ~ YearCentered + DofYc + sp_redback + Sex + (1|Eartag), data = spring.red, family = gaussian(link=identity))
summary(year.red.mod)
r.squaredGLMM(year.red.mod)
#Mass with climate variables
spring.red.mod <- glmmTMB(Weight ~ temp + snow + GDD + DofY + sp_redback + Sex + (1|Eartag), data = spring.red, family = gaussian(link=identity))
summary(spring.red.mod)
r.squaredGLMM(spring.red.mod)

#obervations
n <- length(spring.red$Eartag)
print(n)
redID <- length(unique(spring.red$Eartag))
print(redID)

###############
#MICE
#Mass over time
spring.mice$YearCentered <- spring.mice$Year - 1977
spring.mice$DofYc <- spring.mice$DofY- mean(spring.mice$DofY, na.rm = TRUE)
year.mice.mod <- glmmTMB(Weight ~ YearCentered + DofYc + sp_mice + Sex + (1|Eartag), data = spring.mice, family = gaussian(link=identity))
summary(year.mice.mod)
r.squaredGLMM(year.mice.mod)
#Mass with climate variables
spring.mice.mod <- glmmTMB(Weight ~ temp + snow + GDD + DofY + sp_mice + Sex + (1|Eartag), data = spring.mice, family = gaussian(link=identity))
summary(spring.mice.mod)
r.squaredGLMM(spring.mice.mod)

#observations
n <- length(spring.mice$Eartag)
print(n)
miceID <- length(unique(spring.mice$Eartag))
print(miceID)

####################
#plot
hare.mass <- ggplot(data = spring.hare, aes(x = Year, y = Weight)) +
  geom_point(color = "black", size = 1, alpha=0.2) + 
  stat_smooth(method = "glm", formula = y ~ x, color = "black") + 
  labs(x = "", y = "Spring Body Mass (g)", title = "A") +
  add_livimal("hare", x=1980, y=2600, size = 0.55) +
  coord_cartesian(clip = "off") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0, size=20),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=14),
    axis.title.y = element_text(face="bold", size=16),
    axis.title.x = element_blank())
hare.mass
# Density line
hare.density <- ggplot(data = density, aes(x = Year, y = sp_hare)) +
  geom_line(color = "#56B4E9", size = 1) +
  labs(x = "Year", y = "Spring density (#/ha)") +
  theme_classic() +
  theme(
    axis.text = element_text(size=14),
    axis.title.x = element_text(face="bold", size=16),
    axis.title.y = element_text(face="bold", size=16))
hare.density
# Combine vertically
hare.combined <- hare.mass / hare.density + plot_layout(heights = c(3, 1))
hare.combined

#redback voles
red.mass <- ggplot(data = spring.red, aes(x = Year, y = Weight)) +
  geom_point(color = "black", size = 1, alpha=0.3) + 
  stat_smooth(method = "glm", formula = y ~ x, color = "black") +
  labs(x = "", y = "Spring Body Mass (g)", title = "B") +
  add_livimal("vole", x=1983, y=46, size = 0.5) +
  coord_cartesian(clip = "off") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0, size=20),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=14),
    axis.title.y = element_blank(), 
    axis.title.x = element_blank())

red.density <- ggplot(data = density, aes(x = Year, y = sp_redback)) +
  geom_line(color = "#AA3377", size = 1) +
  labs(x = "Year", y = "Spring density") +
  theme_classic() +
  theme(
    axis.text = element_text(size=14),
    axis.title.x = element_text(face="bold", size=16),
    axis.title.y = element_blank())

red.combined <- red.mass / red.density + plot_layout(heights = c(3, 1))
red.combined

#mice
mice.mass <- ggplot(data = spring.mice, aes(x = Year, y = Weight)) +
  geom_point(color = "black", size = 1, alpha=0.4) + 
  stat_smooth(method = "glm", formula = y ~ x, color = "grey") +
  labs(x = "", y = "Spring Body Mass (g)", title = "C") +
  add_livimal("deermouse", x=1982, y=44, size = 0.4) +
  coord_cartesian(clip = "off") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0, size=20),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=14),
    axis.title.y = element_blank(), 
    axis.title.x = element_blank())

mice.density <- ggplot(data = density, aes(x = Year, y = sp_mice)) +
  geom_line(color = "#E69F00", size = 1) +
  labs(x = "Year", y = "Spring density") +
  theme_classic() +
  theme(
    axis.text = element_text(size=14),
    axis.title.x = element_text(face="bold", size=16),
    axis.title.y = element_blank())

mice.combined <- mice.mass / mice.density + plot_layout(heights = c(3, 1))
mice.combined

final.plot <- hare.combined | red.combined | mice.combined
final.plot

############################################
### Using predict on temp and weight
#Hare
temphare<-expand.grid(temp=seq(-19, -8, by=0.1), GDD=mean(spring.hare$GDD), snow=mean(spring.hare$snow, na.rm = TRUE),
                      DofY=mean(spring.hare$DofY), sp_hare=mean(spring.hare$sp_hare, na.rm=TRUE), Sex="1", Eartag=NA)
temphare$pred <- predict(spring.hare.mod, newdata=temphare, type="response", re.form=NULL)
temphare$pred.se <- predict(spring.hare.mod, newdata=temphare, type="response", se.fit=TRUE, re.form=NULL)$se.fit

#redbacks 
tempred<-expand.grid(temp=seq(-21, -8, by=0.1), GDD=mean(spring.red$GDD), snow=mean(spring.red$snow, na.rm = TRUE),
                     DofY=mean(spring.red$DofY), sp_redback=mean(spring.red$sp_redback, na.rm=TRUE), Sex="1", Eartag=NA)
tempred$pred <- predict(spring.red.mod, newdata=tempred, type="response", re.form=NULL)
tempred$pred.se <- predict(spring.red.mod, newdata=tempred, type="response", se.fit=TRUE, re.form=NULL)$se.fit

#mice
tempmice<-expand.grid(temp=seq(-19, -8, by=0.1), GDD=mean(spring.mice$GDD), snow=mean(spring.mice$snow, na.rm = TRUE),
                      DofY=mean(spring.mice$DofY), sp_mice=mean(spring.mice$sp_mice, na.rm=TRUE), Sex="1", Eartag=NA)
tempmice$pred <- predict(spring.mice.mod, newdata=tempmice, type="response", re.form=NULL)
tempmice$pred.se <- predict(spring.mice.mod, newdata=tempmice, type="response", se.fit=TRUE, re.form=NULL)$se.fit

###########
setempplot <- ggplot() +
  geom_ribbon(data = temphare,
              aes(x = temp,
                  ymin = (((temphare$pred - temphare$pred.se) - 1340.463) / 1340.463) * 100,
                  ymax = (((temphare$pred + temphare$pred.se) - 1340.463) / 1340.463) * 100),
              fill = "#56B4E9", alpha = 0.2) +
  geom_line(data = temphare, linewidth = 1.2,
            aes(x = temp, y = (((pred - 1340.463) / 1340.463) * 100), 
                color = factor("Snowshoe hare", levels = c("Snowshoe hare", "Red-backed vole", "Deer Mice")))) +
  add_livimal("hare", x=-20.8, y=11.5, size = 0.28, color= "#56B4E9") +
  annotate("text", x = -19, y = 11.5, label = "Snowshoe hares",
           hjust = 0, size = 5, color = "black") +
  geom_ribbon(data = tempred,
              aes(x = temp,
                  ymin = (((tempred$pred - tempred$pred.se) - 24.19271) / 24.19271) * 100,
                  ymax = (((tempred$pred + tempred$pred.se) - 24.19271) / 24.19271) * 100),
              fill = "#AA3377", alpha = 0.2) +
  geom_line(data = tempred, linewidth = 1.2,
            aes(x = temp, y = (((tempred$pred - 24.19271) / 24.19271) * 100),
                color = factor("Red-backed vole", levels = c("Snowshoe hare", "Red-backed vole", "Deer Mice")))) +
  add_livimal("vole", x=-20, y=9.5, size = 0.23, color= "#AA3377") +
  annotate("text", x = -19, y = 9.53, label = "Red-backed voles",
           hjust = 0, size = 5, color = "black") +
  geom_ribbon(data = tempmice,
              aes(x = temp,
                  ymin = (((tempmice$pred - tempmice$pred.se) - 27.01022) / 27.01022) * 100,
                  ymax = (((tempmice$pred + tempmice$pred.se) - 27.01022) / 27.01022) * 100),
              fill = "#E69F00", alpha = 0.2) +
  geom_line(data = tempmice, linewidth = 1.2, linetype = "dashed",
            aes(x = temp, y = (((tempmice$pred - 27.01022) / 27.01022) * 100),
                color = factor("Deer Mice", levels = c("Snowshoe hare", "Red-backed vole", "Deer Mice")))) +
  add_livimal("deermouse", x=-20.4, y=7.7, size = 0.21, color= "#E69F00") +
  annotate("text", x = -19, y = 7.65, label = "Deer mice",
           hjust = 0, size = 5, color = "black") +
  geom_hline(yintercept = 0, linetype = "solid", color = "grey") +
  xlab("Mean annual winter temperature (°C)") +
  ylab("% Change in body mass (g)") +
  ylim(-7, 12) +
  xlim(-21, -8) +
  theme_classic() +
  scale_color_manual(values = c(
    "Snowshoe hare" = "#56B4E9",  
    "Red-backed vole" = "#AA3377",  
    "Deer Mice" = "#E69F00"
  )) +
  theme(
    axis.title.x = element_text(face="bold", size = 20, margin = margin(t=18)),
    axis.title.y = element_text(face="bold", size = 20),
    axis.text = element_text(size = 18),
    legend.position = "none", 
    legend.title = element_blank(),
    legend.text = element_blank(),
    legend.justification = c("left", "top"), 
    labs(colour = "Species") +
      guides(fill = "none"))
setempplot

############################################
### Using predict on GDD and weight
#hare
GDDhare<-expand.grid(GDD=seq(557, 919, by=1), temp=mean(spring.hare$temp), snow=mean(spring.hare$snow, na.rm = TRUE),
                     DofY=mean(spring.hare$DofY), sp_hare=mean(spring.hare$sp_hare, na.rm=TRUE), Sex="1", Eartag=NA)
GDDhare$pred <- predict(spring.hare.mod, newdata=GDDhare, type="response", re.form=NULL)
GDDhare$pred.se <- predict(spring.hare.mod, newdata=GDDhare, type="response", se.fit=TRUE, re.form=NULL)$se.fit

#redbacks 
GDDred<-expand.grid(GDD=seq(498, 940, by=1), temp=mean(spring.red$temp), snow=mean(spring.red$snow, na.rm = TRUE),
                    DofY=mean(spring.red$DofY), sp_redback=mean(spring.red$sp_redback, na.rm=TRUE), Sex="1", Eartag = NA)
GDDred$pred <- predict(spring.red.mod, newdata=GDDred, type="response", re.form=NULL)
GDDred$pred.se <- predict(spring.red.mod, newdata=GDDred, type="response", se.fit=TRUE, re.form=NULL)$se.fit

#mice
GDDmice<-expand.grid(GDD=seq(557, 940, by=1), temp=mean(spring.mice$temp), snow=mean(spring.mice$snow, na.rm = TRUE),
                     DofY=mean(spring.mice$DofY), sp_mice=mean(spring.mice$sp_mice, na.rm=TRUE), Sex="1", Eartag = NA)
GDDmice$pred <- predict(spring.mice.mod, newdata=GDDmice, type="response", re.form=NULL)
GDDmice$pred.se <- predict(spring.mice.mod, newdata=GDDmice, type="response", se.fit=TRUE, re.form=NULL)$se.fit

#With S.E
seGDDplot <- ggplot() +
  geom_ribbon(data = GDDmice,
              aes(x = GDD,
                  ymin = ((pred - pred.se -25.49612) / 25.49612) * 100,
                  ymax = ((pred + pred.se - 25.49612) / 25.49612) * 100),
              fill = "#E69F00", alpha = 0.2, show.legend = FALSE) +
  geom_line(data = GDDmice,
            aes(x = GDD, y = ((pred - 25.49612) / 25.49612) * 100),
            color = "#E69F00", linewidth = 1.2, show.legend = FALSE, linetype= "solid") +
  geom_ribbon(data = GDDhare,
              aes(x = GDD,
                  ymin = ((pred - pred.se - 1287.130) / 1287.130) * 100,
                  ymax = ((pred + pred.se - 1287.130) / 1287.130) * 100),
              fill = "#56B4E9", alpha = 0.2, show.legend = FALSE) +
  geom_line(data = GDDhare,
            aes(x = GDD, y = ((pred - 1287.130) / 1287.130) * 100),
            color = "#56B4E9", linewidth = 1.2, show.legend = FALSE, linetype= "solid")+
  geom_ribbon(data = GDDred,
              aes(x = GDD,
                  ymin = ((pred - pred.se - 24.77063) / 24.77063) * 100,
                  ymax = ((pred + pred.se - 24.77063) / 24.77063) * 100),
              fill = "#AA3377", alpha = 0.2, show.legend = FALSE) +
  geom_line(data = GDDred,
            aes(x = GDD, y = ((pred - 24.77063) / 24.77063) * 100),
            color = "#AA3377", linewidth = 1.2, show.legend = FALSE, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "solid", color = "grey") +
  xlab("Annual summer GDD") +
  ylab("") +
  ylim(-7, 12) +
  xlim(498, 940) +
  theme_classic() +
  theme(axis.title.x = element_text(face="bold", size = 20, margin = margin(t=18)),
        axis.title.y = element_text(face="bold", size = 20),
        axis.text = element_text(size = 18),
        legend.position = "none")
seGDDplot

############################################
### Using predict on snow and weight
#hare
snowhare<-expand.grid(snow=seq(2, 154, by=1), temp=mean(spring.hare$temp), GDD=mean(spring.hare$GDD),
                      DofY=mean(spring.hare$DofY), sp_hare=mean(spring.hare$sp_hare, na.rm=TRUE), Sex="1", Eartag=NA)
snowhare$pred <- predict(spring.hare.mod, newdata=snowhare, type="response", re.form=NULL)
snowhare$pred.se <- predict(spring.hare.mod, newdata=snowhare, type="response", se.fit=TRUE, re.form=NULL)$se.fit

#redbacks 
snowred<-expand.grid(snow=seq(1, 154, by=1), temp=mean(spring.red$temp), GDD=mean(spring.red$GDD),
                     DofY=mean(spring.red$DofY), sp_redback=mean(spring.red$sp_redback, na.rm=TRUE), Sex="1", Eartag=NA)
snowred$pred <- predict(spring.red.mod, newdata=snowred, type="response", re.form=NULL)
snowred$pred.se <- predict(spring.red.mod, newdata=snowred, type="response", se.fit=TRUE, re.form=NULL)$se.fit

#mice
snowmice<-expand.grid(snow=seq(1, 154, by=1), temp=mean(spring.mice$temp), GDD=mean(spring.mice$GDD),
                      DofY=mean(spring.mice$DofY), sp_mice=mean(spring.mice$sp_mice, na.rm=TRUE), Sex="1", Eartag=NA)
snowmice$pred <- predict(spring.mice.mod, newdata=snowmice, type="response", re.form=NULL)
snowmice$pred.se <- predict(spring.mice.mod, newdata=snowmice, type="response", se.fit=TRUE, re.form=NULL)$se.fit


#With S.E
sesnowplot <- ggplot() +
  geom_ribbon(data = snowmice,
              aes(x = snow,
                  ymin = ((pred - pred.se - 26.98605) / 26.98605) * 100,
                  ymax = ((pred + pred.se - 26.98605) / 26.98605) * 100),
              fill = "#E69F00", alpha = 0.2, show.legend = FALSE) +
  geom_line(data = snowmice,
            aes(x = snow, y = ((pred - 26.98605) / 26.98605) * 100),
            color = "#E69F00", linewidth = 1.2, show.legend = FALSE, linetype = "dashed") +
  geom_ribbon(data = snowhare,
              aes(x = snow,
                  ymin = ((pred - pred.se - 1336.841) / 1336.841) * 100,
                  ymax = ((pred + pred.se - 1336.841) / 1336.841) * 100),
              fill = "#56B4E9", alpha = 0.2, show.legend = FALSE) +
  geom_line(data = snowhare,
            aes(x = snow, y = ((pred - 1336.841) / 1336.841) * 100),
            color = "#56B4E9", linewidth = 1.2, show.legend = FALSE)+
  geom_ribbon(data = snowred,
              aes(x = snow,
                  ymin = ((pred - pred.se - 24.75279) / 24.75279) * 100,
                  ymax = ((pred + pred.se - 24.75279) / 24.75279) * 100),
              fill = "#AA3377", alpha = 0.2, show.legend = FALSE) +
  geom_line(data = snowred,
            aes(x = snow, y = ((pred - 24.75279) / 24.75279) * 100),
            color = "#AA3377", linewidth = 1.2, show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "solid", color = "grey") +
  xlab("Proportion of days with\n snowdepth ≥ 15cm") +
  ylab("") +
  ylim(-7, 12) +
  xlim(0, 154) +
  theme_classic() +
  theme(axis.title.x = element_text(face="bold", size = 20, margin = margin(t=18)),
        axis.title.y = element_text(face="bold", size = 20),
        axis.text = element_text(size = 18),
        legend.position = "none")
sesnowplot

# Arrange the plots 
setempplot + seGDDplot + sesnowplot

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

######################################################
#PROJECTED CLIMATE WARMING GRAPHS

#load historic and future climate data
IPCCdat<-read.csv("IPCCdat.csv")

#See future warming projections
# Extract 2025 base temperature 
base_temp_2025 <- IPCCdat %>%
  filter(Spring_Year == 2025) %>%
  pull(temp)
base_temp_2025

# Extract 2100 projected temps under each scenario
temp_2100_ssp126 <- IPCCdat %>%
  filter(Spring_Year == 2100) %>%
  pull(temp_ssp126)
temp_2100_ssp126

temp_2100_ssp585 <- IPCCdat %>%
  filter(Spring_Year == 2100) %>%
  pull(temp_ssp585)
temp_2100_ssp585

# Calculate warming relative to 2025 base temp
warming_ssp126 <- temp_2100_ssp126 - base_temp_2025
warming_ssp126
warming_ssp585 <- temp_2100_ssp585 - base_temp_2025
warming_ssp585


#FORECAST HARES
spring.hare.mod <- glmmTMB(Weight ~ temp + DofY + sp_hare + Sex + (1|Eartag), data = spring.hare, family = gaussian(link=identity))
summary(spring.hare.mod)
# Define constants
DofY <- mean(spring.hare$DofY, na.rm = TRUE)
sp_hare <- mean(spring.hare$sp_hare, na.rm = TRUE)
sex <- "1"  
Eartag <-NA

#build prediction dataframe
hare_pred <- function(temp_column, temp_label) {
  IPCCdat %>%
    filter(!is.na(.data[[temp_column]])) %>%
    transmute(Spring_Year,
              temp = .data[[temp_column]],
              DofY = DofY,
              sp_hare = sp_hare,
              Sex = sex,
              Eartag = NA,
              Scenario = temp_label)}
hare_obs <- hare_pred("temp", "Observed")
hare_26  <- hare_pred("temp_ssp126", "RCP 2.6")
hare_85  <- hare_pred("temp_ssp585", "RCP 8.5")
hare_all <- bind_rows(hare_obs, hare_26, hare_85)

hare_all$pred <- predict(spring.hare.mod, newdata = hare_all, type = "response", re.form = NA)
hare_all$pred.se <- predict(spring.hare.mod, newdata = hare_all, type = "response", se.fit = TRUE, re.form = NA)$se.fit

# Filter for years
hare_all <- hare_all %>% filter(Spring_Year >= 1977, Spring_Year <= 2100)

#FORECAST VOLES
spring.red.mod <- glmmTMB(Weight ~ temp + DofY + sp_redback + Sex, data = spring.red, family = gaussian(link=identity))
summary(spring.red.mod)
# Define constants
DofY <- mean(spring.red$DofY, na.rm = TRUE)
sp_redback <- mean(spring.red$sp_redback, na.rm = TRUE)
sex <- "1" 
Eartag <-NA
#build prediction dataframe
red_pred <- function(temp_column, temp_label) {
  IPCCdat %>%
    filter(!is.na(.data[[temp_column]])) %>%
    transmute(Spring_Year,
              temp = .data[[temp_column]],
              DofY = DofY,
              sp_redback = sp_redback,
              Sex = factor("1", levels = levels(spring.red$Sex)),
              Scenario = temp_label)}
red_obs <- red_pred("temp", "Observed")
red_26  <- red_pred("temp_ssp126", "RCP 2.6")
red_85  <- red_pred("temp_ssp585", "RCP 8.5")
red_all <- bind_rows(red_obs, red_26, red_85)

red_all$pred <- predict(spring.red.mod, newdata = red_all, type = "response", re.form = NA)
red_all$pred.se <- predict(spring.red.mod, newdata = red_all, type = "response", se.fit = TRUE, re.form = NA)$se.fit

red_all <- red_all %>% filter(Spring_Year >= 1977, Spring_Year <= 2100)


#FORECAST MICE
spring.mice.mod <- glmmTMB(Weight ~ temp + DofY + sp_mice + Sex, data = spring.mice, family = gaussian(link=identity))
summary(spring.mice.mod)
# Define constants
DofY <- mean(spring.mice$DofY, na.rm = TRUE)
sp_mice <- mean(spring.mice$sp_mice, na.rm = TRUE)
sex <- "1" 
Eartag <-NA
#build prediction dataframe
mice_pred <- function(temp_column, temp_label) {
  IPCCdat %>%
    filter(!is.na(.data[[temp_column]])) %>%
    transmute(Spring_Year,
              temp = .data[[temp_column]],
              DofY = DofY,
              sp_mice = sp_mice,
              Sex = factor("1", levels = levels(spring.mice$Sex)),
              Scenario = temp_label)}
mice_obs <- mice_pred("temp", "Observed")
mice_26  <- mice_pred("temp_ssp126", "RCP 2.6")
mice_85  <- mice_pred("temp_ssp585", "RCP 8.5")
mice_all <- bind_rows(mice_obs, mice_26, mice_85)

mice_all$pred <- predict(spring.mice.mod, newdata = mice_all, type = "response", re.form = NA)
mice_all$pred.se <- predict(spring.mice.mod, newdata = mice_all, type = "response", se.fit = TRUE, re.form = NA)$se.fit

mice_all <- mice_all %>% filter(Spring_Year >= 1977, Spring_Year <= 2100)

#plot
pred_mice<-ggplot(mice_all, aes(x = Spring_Year, y = pred, color = Scenario)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2025, color = "grey40", linetype = "dashed", size = 0.8) + 
  geom_ribbon(aes(ymin = pred - pred.se, ymax = pred + pred.se, fill = Scenario), alpha = 0.2, color = NA) +
  labs(x = "Year", y = "" ,
       color = "Scenario", fill = "Scenario") +
  theme_classic() +
  scale_x_continuous(breaks = seq(1980, 2100, 40), expand = c(0.03, 0.03), limits = c(1977, 2100)) +  
  scale_color_manual(values = c("Observed" = "orange", "RCP 2.6" = "darkgreen", "RCP 8.5" = "darkred")) +
  scale_fill_manual(values = c("Observed" = "orange", "RCP 2.6" = "darkgreen", "RCP 8.5" = "darkred")) +
  theme(legend.position = c(0.80, 0.85),
        axis.title.y = element_blank(),     
        legend.text = element_text(size = 16),         
        legend.title = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))
pred_mice

#PLOT ALL
#hare
pred_hare<-ggplot(hare_all, aes(x = Spring_Year, y = pred, color = Scenario)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2025, color = "grey40", linetype = "dashed", size = 0.8) + 
  geom_ribbon(aes(ymin = pred - pred.se, ymax = pred + pred.se, fill = Scenario), alpha = 0.2, color = NA) +
  labs(x = "Year", y = "Spring Body Mass (g)",
       color = "Scenario", fill = "Scenario") +
  add_livimal("hare", x=1984, y=1342, size = 0.3) +
  coord_cartesian(clip = "off") +
  theme_classic() +
  scale_x_continuous(breaks = seq(1980, 2100, 40), expand = c(0.03, 0.03), limits = c(1977, 2100)) +  
  scale_color_manual(values = c("Observed" = "orange", "RCP 2.6" = "darkgreen", "RCP 8.5" = "darkred")) +
  scale_fill_manual(values = c("Observed" = "orange", "RCP 2.6" = "darkgreen", "RCP 8.5" = "darkred")) + 
  theme(legend.position = "none",
        axis.title.y = element_text(face="bold", size = 19),
        axis.title.x = element_text(face="bold", size = 19),
        axis.text = element_text(size = 14),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 5))
pred_hare
#vole
pred_red<-ggplot(red_all, aes(x = Spring_Year, y = pred, color = Scenario)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2025, color = "grey40", linetype = "dashed", size = 0.8) + 
  geom_ribbon(aes(ymin = pred - pred.se, ymax = pred + pred.se, fill = Scenario), alpha = 0.2, color = NA) +
  labs(x = "Year", y = "",
       color = "Scenario", fill = "Scenario") +
  add_livimal("vole", x=1991, y=26.7, size = 0.3) +
  coord_cartesian(clip = "off") +
  theme_classic() +
  scale_x_continuous(breaks = seq(1980, 2100, 40), expand = c(0.03, 0.03), limits = c(1974, 2100)) +  
  scale_color_manual(values = c("Observed" = "orange", "RCP 2.6" = "darkgreen", "RCP 8.5" = "darkred")) +
  scale_fill_manual(values = c("Observed" = "orange", "RCP 2.6" = "darkgreen", "RCP 8.5" = "darkred")) + 
  theme(legend.position = "none",
        axis.title.y = element_blank(),     
        axis.title.x = element_text(face="bold", size = 19),
        axis.text = element_text(size = 14),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 5))
pred_red
#mice
pred_mice<-ggplot(mice_all, aes(x = Spring_Year, y = pred, color = Scenario)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2025, color = "grey40", linetype = "dashed", size = 0.8) + 
  geom_ribbon(aes(ymin = pred - pred.se, ymax = pred + pred.se, fill = Scenario), alpha = 0.2, color = NA) +
  labs(x = "Year", y = "" ,
       color = "Scenario", fill = "Scenario") +
  add_livimal("deermouse", x=1989, y=27.25, size = 0.25) +
  coord_cartesian(clip = "off") +
  theme_classic() +
  scale_x_continuous(breaks = seq(1980, 2100, 40), expand = c(0.03, 0.03), limits = c(1977, 2100)) +  
  scale_color_manual(values = c("Observed" = "orange", "RCP 2.6" = "darkgreen", "RCP 8.5" = "darkred")) +
  scale_fill_manual(values = c("Observed" = "orange", "RCP 2.6" = "darkgreen", "RCP 8.5" = "darkred")) +
  theme(legend.position = c(0.80, 0.85),
        axis.title.y = element_blank(),     
        legend.text = element_text(size = 16),         
        legend.title = element_text(size = 15, face = "bold"),
        axis.title.x = element_text(face="bold", size = 19),
        axis.text = element_text(size = 14),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 5))
pred_mice

# Arrange the plots
grid.arrange(pred_hare, pred_red, pred_mice,
             nrow = 1, ncol = 3)

#######################################
#HYPOTHESES GRAPHS
#####################################
library(cowplot)
library(grid)
library(gridExtra)
#Hypothetical Graphs
################################################
#Exposed H1
set.seed(5)
x <- c(rnorm(200, mean = -8, 4))
group <- c(rep("C", 200))
df1 <- data.frame(x, group)

cols <- c("#72D8FF")

exposed1<- ggplot(df1, aes(x = x, fill = group)) +
  geom_density(alpha = 0.8, color = NA, bw = 2.5) + 
  ggtitle("Exposed") +
  ylab(expression(bold("H1"))) + 
  xlab(NULL) +
  geom_vline(xintercept = 0, color = "grey", size = 0.5)+
  geom_text(aes(x = -8, y = 0.05, label = "Delta~Temp"), parse = TRUE, size = 4)+
  geom_text(aes(x = -14, y = 0.095, label = "A"), size = 5)+
  guides(color = FALSE) +
  theme_minimal() + 
  scale_fill_manual(values = cols) + 
  theme(
    plot.title=element_text(hjust = 0.5, size=20),
    legend.position = "none",
    axis.text.y = element_blank(),  
    axis.ticks.y = element_blank(),  
    axis.ticks.x = element_blank(),  
    axis.line = element_line(color = "black", size = 0.5),  
    panel.grid = element_blank()            
  ) +
  scale_x_continuous(breaks = c(0), labels = c("0"), limits = c(-20, 20)) +  
  ylim(0, 0.10)
exposed1


#Exposed H2
set.seed(5)
x <- c(rnorm(200, mean = 3, 4))
group <- c(rep("A", 200))
df2 <- data.frame(x, group)

cols <- c("#99CC33")

exposed2<- ggplot(df2, aes(x = x, fill = group)) +
  geom_density(alpha = 0.8, color = NA, bw = 2.5) + 
  ylab(expression(bold("H2"))) + 
  xlab(NULL) +
  geom_vline(xintercept = 0, color = "grey", size = 0.5)+
  geom_text(aes(x = 3,  y = 0.05,  label = "Delta~GDD"), parse = TRUE, size = 4) +
  geom_text(aes(x = -14, y = 0.095, label = "D"), size = 5) +
  guides(color = FALSE) +
  theme_minimal() + 
  scale_fill_manual(values = cols) +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),  
    axis.ticks.y = element_blank(),  
    axis.ticks.x = element_blank(),  
    axis.line = element_line(color = "black", size = 0.5),
    panel.grid = element_blank() 
  ) +
  scale_x_continuous(breaks = c(0), labels = c("0"), limits = c(-20, 20)) +  
  ylim(0, 0.10)
exposed2


#H3
set.seed(5)
x <- c(rnorm(200, mean = -8, 4),
       rnorm(200, mean = 3, 4),
       rnorm(200, mean = -3, sd = 4))
group <- c(rep("A", 200), rep("B", 200), rep("C", 200))
df3 <- data.frame(x, group)

cols <- c("A"="#72D8FF", "B"="#99CC33", "C"="#FF9900")
alphas <- c("A" = 0.4,  
            "B" = 0.4,  
            "C" = 1)  
exposed3<- ggplot(df3, aes(x = x, fill = group)) +
  geom_density(aes(alpha = group), color = NA, bw = 2.5) +
  ylab(expression(bold("H3"))) + 
  xlab(NULL) +
  geom_vline(xintercept = 0, color = "grey", size = 0.5)+
  geom_text(aes(x = -3,  y = 0.05,  label = "Delta~Time"), parse = TRUE, size = 4) +
  geom_text(aes(x = -14, y = 0.095, label = "G"), size = 5) +
  guides(color = FALSE) +
  theme_minimal() + 
  scale_fill_manual(values = cols) +
  scale_alpha_manual(values = alphas) +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),  
    axis.ticks.y = element_blank(),  
    axis.ticks.x = element_blank(),  
    axis.line = element_line(color = "black", size = 0.5),
    panel.grid = element_blank() 
  ) +
  scale_x_continuous(breaks = c(0), labels = c("0"), limits = c(-15, 15)) +  
  ylim(0, 0.10)
exposed3

################################################
# Covered
# H1
set.seed(5)
x <- c(rnorm(200, mean = 0, 4))
group <- c(rep("C", 200))
cover1 <- data.frame(x, group)

cols <- c("#72D8FF")

cover1<- ggplot(cover1, aes(x = x, fill = group)) +
  geom_density(alpha = 0.8, color = NA, bw = 2.5) + 
  ggtitle("Covered") +
  ylab(expression(bold("H1"))) + 
  xlab(NULL) +
  geom_vline(xintercept = 0, color = "grey", size = 0.5)+
  geom_text(aes(x = 0,   y = 0.05,  label = "Delta~Temp"), parse = TRUE, size = 4) +
  geom_text(aes(x = -14, y = 0.095, label = "C"), size = 5) +
  guides(color = FALSE) +
  theme_minimal() + 
  scale_fill_manual(values = cols) + 
  theme(
    plot.title=element_text( hjust = 0.5, size=20),
    legend.position = "none",
    axis.text.y = element_blank(),  
    axis.ticks.y = element_blank(),  
    axis.ticks.x = element_blank(),  
    axis.line = element_line(color = "black", size = 0.5),
    panel.grid = element_blank()      
  ) +
  scale_x_continuous(breaks = c(0), labels = c("0"), limits = c(-15, 15)) +  
  ylim(0, 0.10)
cover1

#Covered H2
set.seed(5)
x <- c(rnorm(200, mean = 8, 4))
group <- c(rep("A", 200))
cover2 <- data.frame(x, group)

cols <- c("#99CC33")

cover2<- ggplot(cover2, aes(x = x, fill = group)) +
  geom_density(alpha = 0.8, color = NA, bw = 2.5) + 
  ylab(expression(bold("H2"))) + 
  xlab(NULL) +
  geom_vline(xintercept = 0, color = "grey", size = 0.5)+
  geom_text(aes(x = 8,  y = 0.05,  label = "Delta~GDD"), parse = TRUE, size = 4) +
  geom_text(aes(x = -14, y = 0.095, label = "F"), size = 5) +
  guides(color = FALSE) +
  theme_minimal() + 
  scale_fill_manual(values = cols) + 
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),  
    axis.ticks.y = element_blank(),  
    axis.ticks.x = element_blank(),  
    axis.line = element_line(color = "black", size = 0.5),
    panel.grid = element_blank()      
  ) +
  scale_x_continuous(breaks = c(0), labels = c("0"), limits = c(-15, 15)) +  
  ylim(0, 0.10)
cover2

#cover H3
set.seed(5)
x <- c(rnorm(200, mean = -0, 4),
       rnorm(200, mean = 8, 4),
       rnorm(200, mean = 3, sd = 4))
group <- c(rep("A", 200), rep("B", 200), rep("C", 200))
cover3 <- data.frame(x, group)

cols <- c("A" = "#72D8FF","B" = "#99CC33", "C" = "#FF9900")  
alphas <- c("A" = 0.4, "B" = 0.4, "C" = 1)  

cover3 <- ggplot(cover3, aes(x = x, fill = group)) +
  geom_density(aes(alpha = group), color = NA, bw = 2.5) +
  ylab(expression(bold("H3"))) + 
  xlab(NULL) +
  geom_vline(xintercept = 0, color = "grey", size = 0.5) +
  geom_text(aes(x = 3,  y = 0.05,  label = "Delta~Time"), parse = TRUE, size = 4) +
  geom_text(aes(x = -14, y = 0.095, label = "I"), size = 5) +
  guides(color = FALSE, alpha = FALSE) +   
  theme_minimal() + 
  scale_fill_manual(values = cols) +
  scale_alpha_manual(values = alphas) +    
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),  
    axis.ticks.y = element_blank(),  
    axis.ticks.x = element_blank(),  
    axis.line = element_line(color = "black", size = 0.5),
    panel.grid = element_blank()      
  ) +
  scale_x_continuous(breaks = c(0), labels = c("0"), limits = c(-15, 15)) +  
  ylim(0, 0.10)
cover3

################################################
#Inbetween
#Med H1
set.seed(5)
x <- c(rnorm(200, mean = -4, 4))
group <- c(rep("C", 200))
med1 <- data.frame(x, group)

cols <- c("#72D8FF")

med1<- ggplot(med1, aes(x = x, fill = group)) +
  geom_density(alpha = 0.8, color = NA, bw = 2.5) + 
  ggtitle("Intermediate") +
  ylab(expression(bold("H1"))) + 
  xlab(NULL) +
  geom_vline(xintercept = 0, color = "grey", size = 0.5)+
  geom_text(aes(x = -4, y = 0.05, label = "Delta~Temp"), parse = TRUE, size = 4) +
  geom_text(aes(x = -14, y = 0.095, label = "B"), size = 5) +
  guides(color = FALSE) +
  theme_minimal() + 
  scale_fill_manual(values = cols) + 
  theme(
    plot.title=element_text(hjust = 0.5, size=20),
    legend.position = "none",
    axis.text.y = element_blank(),  
    axis.ticks.y = element_blank(),  
    axis.ticks.x = element_blank(),  
    axis.line = element_line(color = "black", size = 0.5),
    panel.grid = element_blank()) +
  scale_x_continuous(breaks = c(0), labels = c("0"), limits = c(-15, 15)) +  
  ylim(0, 0.10)
med1

#Med H2
set.seed(5)
x <- c(rnorm(200, mean = 4, 3.5))
group <- c(rep("A", 200))
med2 <- data.frame(x, group)

cols <- c("#99CC33")

med2<- ggplot(med2, aes(x = x, fill = group)) +
  geom_density(alpha = 0.8, color = NA, bw = 2.5) + 
  ylab(expression(bold("H2"))) + 
  xlab(NULL) +
  geom_vline(xintercept = 0, color = "grey", size = 0.5)+
  geom_text(aes(x = 4,  y = 0.05,  label = "Delta~GDD"), parse = TRUE, size = 4) +
  geom_text(aes(x = -14, y = 0.095, label = "E"), size = 5)+
  guides(color = FALSE) +
  theme_minimal() + 
  scale_fill_manual(values = cols) + 
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),  
    axis.ticks.y = element_blank(),  
    axis.ticks.x = element_blank(),  
    axis.line = element_line(color = "black", size = 0.5),
    panel.grid = element_blank()) +
  scale_x_continuous(breaks = c(0), labels = c("0"), limits = c(-15, 15)) +  
  ylim(0, 0.10)
med2

#Med H3
set.seed(5)
x <- c(rnorm(200, mean = -4, 4), 
       rnorm(200, mean = 4, 4),
       rnorm(200, mean = 0, 4))
group <- c(rep("A", 200), rep("B", 200), rep("C", 200))
med3 <- data.frame(x, group)

cols <- c("A"="#72D8FF", "B"="#99CC33", "C"="#FF9900")
alphas <- c("A" = 0.4,  
            "B" = 0.4,  
            "C" = 1)  

med3 <- ggplot(med3, aes(x = x, fill = group)) +
  geom_density(aes(alpha = group), color = NA, bw = 2.5) +
  ylab(expression(bold("H3"))) + 
  xlab(NULL) +
  geom_vline(xintercept = 0, color = "grey", size = 0.5) +
  geom_text(aes(x = 0,  y = 0.05,  label = "Delta~Time"), parse = TRUE, size = 4) +
  geom_text(aes(x = -14, y = 0.095, label = "H"), size = 5) +
  guides(color = FALSE, alpha = FALSE) +  
  theme_minimal() + 
  scale_fill_manual(values = cols) +
  scale_alpha_manual(values = alphas) +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),  
    axis.ticks.y = element_blank(),  
    axis.ticks.x = element_blank(),  
    axis.line = element_line(color = "black", size = 0.5),
    panel.grid = element_blank() ) +
  scale_x_continuous(breaks = c(0), labels = c("0"), limits = c(-15, 15)) +
  ylim(0, 0.10)
med3

#Combine
H123<-plot_grid(exposed1, med1, cover1, exposed2, med2, cover2, exposed3, med3, cover3,  nrow = 3, ncol = 3)
H123
y.grob <- textGrob("Fitness (w)", 
                   gp=gpar(fontface="bold", col="black", fontsize=20), rot=90)
x.grob <- textGrob("Body Mass (g)", 
                   gp=gpar(fontface="bold", col="black", fontsize=20))
#add to plot 
grid.arrange(arrangeGrob(H123, left = y.grob, bottom = x.grob, padding = unit(2, "line")))

