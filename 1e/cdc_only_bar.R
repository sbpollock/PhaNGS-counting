#Import libraries
library(ggplot2)

#Import .csv with data formatted appropriately 
#	x			group	y1	y2	y3
#	ANPEP.01	OFF		2	3	4
#	ANPEP.02	OFF		5	6	7
#	...			...		...	...	...

raw <- read.table("oe_raw_cdc.csv", sep = ",", header = TRUE)
raw$mean <- apply(raw[,c("y1","y2","y3")],1,mean,na.rm=TRUE)
raw$sd <- apply(raw[,c("y1","y2","y3")],1,sd,na.rm=TRUE)
raw2 <- raw[grepl("CDCP1",raw$x),]

#Plot
ggplot(data=raw2, aes(x = x, y = raw2$mean, fill = group)) +
theme_classic() + 

#Add error bars
geom_errorbar(aes(
ymax=mean+sd, 
ymin=mean-sd, 
width=0.1), position=position_dodge(.9)) +

#Set columns and colors
geom_bar(data=raw2,position=position_dodge(), stat="identity", color="black") +
#scale_colour_manual(values=c("black","black")) +
scale_fill_manual(values=c("grey","cadetblue1"), labels=c("CDCP1 OFF    ","CDCP1 ON  ")) +

#Add points
geom_point(data=raw2, aes(y=y1, group=group),stat="identity",position=position_dodge(width=0.9),shape=1) +
geom_point(data=raw2, aes(y=y2, group=group),stat="identity",position=position_dodge(width=0.9),shape=1) +
geom_point(data=raw2, aes(y=y3, group=group),stat="identity",position=position_dodge(width=0.9),shape=1) +

#Add background line
geom_hline(yintercept = 2, size=0.25, linetype="dashed", color="black") +

#Set scale
coord_cartesian(ylim=c(1,150)) +

#Labels and theme
labs(x = "", y = "Enrichment") +
theme(
legend.position="top",
legend.key.width=unit(2,"line"),
legend.title=element_blank(),
legend.text=element_text(face = "bold", family = "Arial", size = 11), 
plot.title = element_text(hjust = 0.5, face = "bold"),
axis.line.x = element_blank(), 
axis.line.y = element_line(colour = "black"), #change the borders
axis.title = element_text(family = "Arial", color = "black", face = "bold", size = 14),
axis.text.x = element_text(family = "Arial", color = "black", face = "bold", size = 14, angle=45, hjust=1),
axis.ticks.x = element_blank(),
axis.text.y = element_text(family = "Arial", color = "black", face = "bold", size = 14),
)

#Export image
ggsave("oe_cdc_cdc.eps", device = "eps", width = 4, height = 3, units="in", dpi = 300)
