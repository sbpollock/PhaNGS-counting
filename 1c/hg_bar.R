#Import libraries
library(ggplot2)

#Import .csv with data formatted appropriately 
#	group	x		y1	y2	y3
#	GFP		Input	4	5	1
#	ZNF2	Input	3	5	2
#	GFP		HeLa	2	10	3
#	ZNF2	HeLa	2	10	4
#	...		...		...		...

raw <- read.table("hg_titer_raw.csv", sep = ",", header = TRUE)
raw$mean <- apply(raw[,c("y1","y2","y3")],1,mean)
raw$sd <- apply(raw[,c("y1","y2","y3")],1,sd)
raw$x <- factor(raw$x, levels = c("Input","No Cells","HeLa","HeLaGFP"))

#Plot
ggplot(raw, aes(x = x, y = raw$mean, fill = group)) +
theme_classic() + 

#Add error bars
geom_errorbar(aes(
ymax=mean+sd, 
ymin=mean-sd, 
width=0.2), size=0.5, position=position_dodge(.9)) + 

#Set columns and colors
geom_bar(position=position_dodge(), stat="identity") +
scale_fill_manual(values=c("green","grey"), labels=c("a-GFP","a-ZNF2")) +

#Add points
geom_point(data=raw, aes(y=y1, group=group),stat="identity",position=position_dodge(width=0.9),shape=1) +
geom_point(data=raw, aes(y=y2, group=group),stat="identity",position=position_dodge(width=0.9),shape=1) +
geom_point(data=raw, aes(y=y3, group=group),stat="identity",position=position_dodge(width=0.9),shape=1) +

#Set scale
scale_y_log10(expand = c(0,0)) + 
coord_cartesian(ylim=c(1e4,1e12)) +

#Labels and theme
labs(x = "", y = "Log titer (cfu/mL)") +
theme(
legend.position="top",
legend.title=element_blank(),
legend.key.height=unit(2,"line"),
legend.text=element_text(family = "Arial", size = 18), 
axis.line = element_line(colour = "black"), #change the borders
axis.title = element_text(family = "Arial", color = "black", size = 18),
axis.text.x = element_text(angle=45, hjust=1, family = "Arial", color = "black", size = 18),
axis.text.y = element_text(family = "Arial", color = "black", size = 18),
)

#Export image
ggsave("hg_titer.eps", device = "eps", width = 8, height = 4, units="in", dpi = 300)
