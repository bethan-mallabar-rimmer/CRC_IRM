#install.packages('patchwork')
library(ggplot2)
library(patchwork)

HGtheme=theme_bw()+ #a graph theme made by Harry Green @ Exeter:
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5))

#ROCAUC values from Supplmentary Table 7:
bv_4049 <- data.frame(x=c("All","Female","Male"), min=c(0.6,0.57,0.6), low=c(0.6,0.57,0.6), mid=c(0.68,0.68,0.72), top=c(0.76,0.79,0.82), max=c(0.76,0.79,0.82))

bv_5059 <- data.frame(x=c("All","Female","Male"), min=c(0.75,0.73,0.72), low=c(0.75,0.73,0.72), mid=c(0.78,0.78,0.76), top=c(0.81,0.83,0.81), max=c(0.81,0.83,0.81))

bv_6069 <- data.frame(x=c("All","Female","Male"), min=c(0.72,0.71,0.68), low=c(0.72,0.71,0.68), mid=c(0.75,0.76,0.72), top=c(0.77,0.8,0.75), max=c(0.77,0.8,0.75))

bv_7079 <- data.frame(x=c("All","Female","Male"), min=c(0.72,0.66,0.69), low=c(0.72,0.66,0.69), mid=c(0.77,0.76,0.75), top=c(0.82,0.86,0.80), max=c(0.82,0.86,0.80))

bv_4049_p <- ggplot(bv_4049, aes(x=x, ymin = min, lower = low, middle = mid, upper = top, ymax = max)) +
  geom_boxplot(stat = "identity") + ylim(c(0.5, 1)) + ylab("ROCAUC") + xlab("Age 40-49") + HGtheme

bv_5059_p <- ggplot(bv_5059, aes(x=x, ymin = min, lower = low, middle = mid, upper = top, ymax = max)) +
  geom_boxplot(stat = "identity") + ylim(c(0.5, 1)) + xlab("Age 50-59") + HGtheme + theme(axis.title.y=element_blank(),
                                                                                axis.text.y=element_blank(),
                                                                                axis.ticks.y=element_blank())

bv_6069_p <- ggplot(bv_6069, aes(x=x, ymin = min, lower = low, middle = mid, upper = top, ymax = max)) +
  geom_boxplot(stat = "identity") + ylim(c(0.5, 1)) + xlab("Age 60-69") + HGtheme + theme(axis.title.y=element_blank(),
                                                                                axis.text.y=element_blank(),
                                                                                axis.ticks.y=element_blank())

bv_7079_p <- ggplot(bv_7079, aes(x=x, ymin = min, lower = low, middle = mid, upper = top, ymax = max)) +
  geom_boxplot(stat = "identity") + ylim(c(0.5, 1)) + xlab("Age 70-79") + HGtheme + theme(axis.title.y=element_blank(),
                                                                                axis.text.y=element_blank(),
                                                                                axis.ticks.y=element_blank())

bv_4049_p + bv_5059_p + bv_6069_p + bv_7079_p + plot_layout(nrow = 1)
