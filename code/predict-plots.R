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
