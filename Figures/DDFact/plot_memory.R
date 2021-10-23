library(reshape)
library(ggplot2)
library("RColorBrewer") 

## NOTE: This assumes we are excuting inside of the paper/suppfigs directory (in the same directory
##       as memory_scaling.csv).
t <- read.csv("memory_scaling.csv", check.names=FALSE, header=TRUE)
mt <- melt(t, id="Total Read Counts")
names(mt) <- c("Read_Count", "Method", "Memory_(bytes)")

# skipget yellow
pal <- brewer.pal(9, "Set1")[c(1,2,3,4,5,9)]

mt[,'MemoryGB'] <- mt[,'Memory_(bytes)'] / 1000000

ggplot(data=mt, aes_string(x="Read_Count", y="MemoryGB", fill="Method")) + 
  geom_bar(stat="identity", position ="dodge") + 
  theme_classic() + scale_y_continuous(expand = c(0, 0)) + 
  scale_fill_manual(values=pal) + xlab("Number of Reads (in millions)") + 
  ylab("Max Memory Usage (in GB)") + 
  scale_x_continuous(breaks=c(30,60,90,120)) + 
  theme(text=element_text(size=19), legend.position="bottom")
