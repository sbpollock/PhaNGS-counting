#Import libraries
library(ggplot2)
library(ggrepel)

#Import .csv 
raw <- read.table("MS_vs_PhaNGS.csv", sep = ",", header = TRUE, row.names=1)

#Plot
ggplot(raw, aes(x = MS, y = PhaNGS, label = rownames(raw))) +
geom_vline(xintercept = 1, size=0.25, colour="grey") + geom_hline(yintercept = 1, size=0.25, colour="grey") +
geom_abline(slope=1,linetype="dashed",colour="grey") +
geom_errorbar(aes(ymin = PhaNGS - PhaNGS_var, ymax = PhaNGS + PhaNGS_var), colour="black", size=0.25) +
geom_errorbarh(aes(xmin = MS - MS_var, xmax = MS + MS_var), colour="black", size=0.25) +
geom_point(aes(size=peptide_count, colour=Sample)) +
theme_minimal() +
geom_text_repel(box.padding = 0.5, point.padding = 1.7, size=4, fill="white", color="black", segment.size=0.25) +

#Set scale
scale_size(name="Peptide count\n(Abundance)", breaks=c(1,25,50,75),range=c(1,9)) +
scale_x_log10(expand = c(0,0), limits=c(0.1,10)) +
scale_y_log10(expand = c(0,0), limits=c(0.1,10)) + 
scale_colour_manual(values=c("red","blue"), labels=c("MCF10A\n(EVtoKRAS)","P493-6\n(ONtoOFF)")) +

#Labels
labs(x = "Fold-change MS", y = "Fold-change PhaNGS") +

#Theme
theme(
legend.title = element_text(size = 12, family = "Arial", face = "bold"), 
legend.text = element_text(size = 11, family = "Arial"),
legend.key.height = unit(2,"line"),
plot.title = element_text(hjust = 0.5, face = "bold", size=16, family = "Arial"),
axis.title = element_text(family = "Arial", color = "black", size = 16), 
axis.ticks.length=unit(0.25,"cm"),
axis.text.x = element_text(family = "Arial", color = "black", face = "bold", size = 14, margin=margin(b=10,unit="pt")),
axis.text.y = element_text(family = "Arial", color = "black", face = "bold", size = 14, margin=margin(l=10,unit="pt"))
) 

#Finish and export figure
ggsave("MS_vs_PhaNGS.eps", device="eps", dpi = 300, width=6, height=4, units="in")
