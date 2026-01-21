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
