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

#######################################
#HYPOTHESES GRAPHS
#####################################

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

