library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(grid)
library(gridExtra)

## NOTE: This assumes we are excuting inside of the paper/suppfigs directory (in the same directory
##       as memory_scaling.csv).
t <- read.csv("time_scaling.csv", check.names=FALSE, header=TRUE)
print(t)
mt <- melt(t, id="Total Read Counts")
names(mt) <- c("Read_Count", "Method", "Time")
print(mt)
# skipget yellow
pal <- brewer.pal(9, "Set1")[c(1,2,3,4,5,9)]

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
   
   plots <- list(...)
   position <- match.arg(position)
   g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
   legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
   lheight <- sum(legend$height)
   lwidth <- sum(legend$width)
   gl <- lapply(plots, function(x) x + theme(legend.position="none"))
   gl <- c(gl, ncol = ncol, nrow = nrow)
   
   combined <- switch(position,
                      "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                             legend,
                                             ncol = 1,
                                             heights = unit.c(unit(1, "npc") - lheight, lheight)),
                      "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 2,
                                            widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
   
   grid.newpage()
   grid.draw(combined)
   
   # return gtable invisibly
   invisible(combined)
   
}

ycut <- 13000 

global <- ggplot(data=mt, aes_string(x="Read_Count", y="Time", fill="Method")) + 
  geom_bar(stat="identity", position ="dodge") + 
  theme_classic() + scale_y_continuous(expand = c(0, 0)) + 
  scale_fill_manual(values=pal) + 
  xlab("") + #Number of Reads (in millions)") + 
  ylab("Wall Clock Time (in sec)") + 
  scale_x_continuous(breaks=c(30,60,90,120)) + theme(line = element_blank()) + 
  theme(text=element_text(size=14)) + geom_hline(yintercept = ycut, color="red", linetype="dashed") + annotate("text", 47,size=3, ycut+10000, label = "limit of zommed plot", color = "red")

local <- ggplot(data=mt, aes_string(x="Read_Count", y="Time", fill="Method")) + 
  geom_bar(stat="identity", position ="dodge") + 
  theme_classic() + scale_y_continuous(expand = c(0, 0)) + 
  scale_fill_manual(values=pal) + 
  xlab("Number of Reads (in millions)") + 
  ylab("Wall Clock Time (in sec)") + 
  scale_x_continuous(breaks=c(30,60,90,120)) + 
  theme(text=element_text(size=14)) + coord_cartesian(ylim=c(0, ycut)) + theme(line = element_blank()) +  theme(legend.position="none")   

#vp <- viewport(width=0.3, height=0.5, x=0.2, y=0.7)
#print(global)
#print(local, vp = vp)

grid_arrange_shared_legend(global, local, ncol=1, nrow=2)
p <- ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_wrap(~class)
p <- ggplotGrob(p)
ggsave(p, 'test.pdf')


