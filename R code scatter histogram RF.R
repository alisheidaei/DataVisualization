rm(list = ls())
setwd("Z:\\Kimiya\\Multiple outcome exposure modeling\\Prepare result paper multivariate")

library(ggpubr)
library(dplyr)

##### CA #####
RFData <- read.csv("Prediction ca all years - RF.csv")
PltData <- data.frame(var1 = RFData$obs,
                      var2 = RFData$pred,
                      fill = 1) %>%
  filter(var1 < max(var1))

quantile(c(PltData$var1, PltData$var2), prob = c(0.005,0.995))
Vmax <- max(PltData$var1)

# Scatter plot
sp <- ggscatter(PltData, x = "var1", y = "var2",
                palette = "jco",
                size = 3, alpha = 0.1)+
  xlab("Observed") + ylab("Expected") +
  border() +
  ggtitle("CA") +
  ylim(0,Vmax) + xlim(0, Vmax) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red")

# Marginal density plot of x (top panel) and y (right panel)
xplot <- ggplot(PltData, aes(x = var1, fill = fill)) +
  geom_density(aes(y = ..scaled..), alpha = 0.5) + 
  labs(title = "Histogram of Observed Values", x = "", y = "Density") +
  theme_classic() +
  theme(legend.position = "none") +
  ylim(0,1) + xlim(0, Vmax) 

yplot <- ggplot(PltData, aes(x = var2, fill = fill)) +
  geom_density(aes(y = ..scaled..), alpha = 0.5) + 
  labs(title = "Histogram of Predicted Values", x = "", y = "Density") +
  theme_classic() +
  theme(legend.position = "none") +
  ylim(0,1) + xlim(0, Vmax) 

ggarrange(xplot, yplot, ncol = 1)
yplot <- yplot + rotate()


# Arranging the plot
Plt1 <- ggarrange(xplot, NULL, sp, yplot, labels = c("","",""), 
                  ncol = 2, nrow = 2,  align = "hv", 
                  widths = c(2, 1), heights = c(1, 2),
                  common.legend = F)
ggsave(plot = Plt1, "Observed vs predicted CA RF.pdf", width = 10, height = 10)




##### EC ##### 
RFData <- read.csv("Prediction ec all years - RF.csv")
PltData <- data.frame(var1 = RFData$obs,
                      var2 = RFData$pred,
                      fill = 1) %>%
  filter(var1 < max(var1))

quantile(c(PltData$var1, PltData$var2), prob = c(0.05,0.995))
Vmax <- max(PltData$var1)

# Scatter plot
sp <- ggscatter(PltData, x = "var1", y = "var2",
                palette = "jco",
                size = 3, alpha = 0.1)+
  xlab("Observed") + ylab("Expected") +
  border() +
  ggtitle("EC") +
  ylim(0,Vmax) + xlim(0, Vmax) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red")


# Marginal density plot of x (top panel) and y (right panel)
xplot <- ggplot(PltData, aes(x = var1, fill = fill)) +
  geom_density(aes(y = ..scaled..), alpha = 0.5) + 
  labs(title = "Histogram of Observed Values", x = "", y = "Density") +
  theme_classic() +
  theme(legend.position = "none") +
  ylim(0,1) + xlim(0, Vmax) 

yplot <- ggplot(PltData, aes(x = var2, fill = fill)) +
  geom_density(aes(y = ..scaled..), alpha = 0.5) + 
  labs(title = "Histogram of Predicted Values", x = "", y = "Density") +
  theme_classic() +
  theme(legend.position = "none") +
  ylim(0,1) + xlim(0, Vmax) 

ggarrange(xplot, yplot, ncol = 1)
yplot <- yplot + rotate()


# Arranging the plot
Plt1 <- ggarrange(xplot, NULL, sp, yplot, labels = c("","",""), 
                  ncol = 2, nrow = 2,  align = "hv", 
                  widths = c(2, 1), heights = c(1, 2),
                  common.legend = F)
ggsave(plot = Plt1, "Observed vs predicted EC RF.pdf", width = 10, height = 10)



##### SI ##### 
RFData <- read.csv("Prediction si all years - RF.csv")
PltData <- data.frame(var1 = RFData$obs,
                      var2 = RFData$pred,
                      fill = 1) %>%
  filter(var1 < max(var1))

quantile(c(PltData$var1, PltData$var2), prob = c(0.05,0.995))
Vmax <- max(PltData$var1)

# Scatter plot
sp <- ggscatter(PltData, x = "var1", y = "var2",
                palette = "jco",
                size = 3, alpha = 0.1)+
  xlab("Observed") + ylab("Expected") +
  border() +
  ggtitle("SI") +
  ylim(0,Vmax) + xlim(0, Vmax) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red")


# Marginal density plot of x (top panel) and y (right panel)
xplot <- ggplot(PltData, aes(x = var1, fill = fill)) +
  geom_density(aes(y = ..scaled..), alpha = 0.5) + 
  labs(title = "Histogram of Observed Values", x = "", y = "Density") +
  theme_classic() +
  theme(legend.position = "none") +
  ylim(0,1) + xlim(0, Vmax) 

yplot <- ggplot(PltData, aes(x = var2, fill = fill)) +
  geom_density(aes(y = ..scaled..), alpha = 0.5) + 
  labs(title = "Histogram of Predicted Values", x = "", y = "Density") +
  theme_classic() +
  theme(legend.position = "none") +
  ylim(0,1) + xlim(0, Vmax) 

ggarrange(xplot, yplot, ncol = 1)
yplot <- yplot + rotate()


# Arranging the plot
Plt1 <- ggarrange(xplot, NULL, sp, yplot, labels = c("","",""), 
                  ncol = 2, nrow = 2,  align = "hv", 
                  widths = c(2, 1), heights = c(1, 2),
                  common.legend = F)
ggsave(plot = Plt1, "Observed vs predicted SI RF.pdf", width = 10, height = 10)


#####  SO4 ##### 
RFData <- read.csv("Prediction so4 all years - RF.csv")
PltData <- data.frame(var1 = RFData$obs,
                      var2 = RFData$pred,
                      fill = 1) %>%
  filter(var1 < max(var1))

quantile(c(PltData$var1, PltData$var2), prob = c(0.05,0.995))
Vmax <- max(PltData$var1)

# Scatter plot
sp <- ggscatter(PltData, x = "var1", y = "var2",
                palette = "jco",
                size = 3, alpha = 0.1)+
  xlab("Observed") + ylab("Expected") +
  border() +
  ggtitle("SO4") +
  ylim(0,Vmax) + xlim(0, Vmax) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red")


# Marginal density plot of x (top panel) and y (right panel)
xplot <- ggplot(PltData, aes(x = var1, fill = fill)) +
  geom_density(aes(y = ..scaled..), alpha = 0.5) + 
  labs(title = "Histogram of Observed Values", x = "", y = "Density") +
  theme_classic() +
  theme(legend.position = "none") +
  ylim(0,1) + xlim(0, Vmax) 

yplot <- ggplot(PltData, aes(x = var2, fill = fill)) +
  geom_density(aes(y = ..scaled..), alpha = 0.5) + 
  labs(title = "Histogram of Predicted Values", x = "", y = "Density") +
  theme_classic() +
  theme(legend.position = "none") +
  ylim(0,1) + xlim(0, Vmax) 

ggarrange(xplot, yplot, ncol = 1)
yplot <- yplot + rotate()


# Arranging the plot
Plt1 <- ggarrange(xplot, NULL, sp, yplot, labels = c("","",""), 
                  ncol = 2, nrow = 2,  align = "hv", 
                  widths = c(2, 1), heights = c(1, 2),
                  common.legend = F)
ggsave(plot = Plt1, "Observed vs predicted SO4 RF.pdf", width = 10, height = 10)
