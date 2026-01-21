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
