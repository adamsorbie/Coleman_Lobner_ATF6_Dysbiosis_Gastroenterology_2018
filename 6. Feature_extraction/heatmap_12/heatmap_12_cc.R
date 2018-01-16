library(tidyverse)
library(RColorBrewer)
library(reshape2)
library(gtools)

# arcsine transform function 
#asinTransform <- function(p) { asin(sqrt(p/100)) }



# read input 
df_tg <- read.csv("tg_recipients.csv", header = TRUE, check.names = FALSE)
df_fl <- read.csv("fl_recipients.csv", header = TRUE, check.names = FALSE)
df_tg_prev <- read.csv("tg_recipients_prev.csv", header = TRUE, check.names = FALSE)
df_fl_prev <- read.csv("fl_recipients_prev.csv", header = TRUE, check.names = FALSE)

minval <- 0.025

df_tg[df_tg<minval]=0
df_fl[df_fl<minval]=0


df_tg_melt <- melt(df_tg)
#df_tg_melt[3] <- lapply(df_tg_melt[3], asinTransform)
df_tg_melt[3] <- lapply(df_tg_melt[3], log2)
df_tg_melt[mapply(is.infinite, df_tg_melt)] <- NA
df_tg_melt[is.na(df_tg_melt)]<- 0


df_fl_melt <- melt(df_fl)
#df_fl_melt[3] <- lapply(df_fl_melt[3], asinTransform)
df_fl_melt[3] <- lapply(df_fl_melt[3], log2)
df_fl_melt[mapply(is.infinite, df_fl_melt)] <- NA
df_fl_melt[is.na(df_fl_melt)]<- -10


tg_melt <- melt(df_tg_prev)
fl_melt <- melt(df_fl_prev)


hm.palette <- colorRampPalette((brewer.pal(9,"Greens")), space='Lab')
hm.palette2 <- colorRampPalette((brewer.pal(6, "Reds")), space="Lab")



p_abundance_fl <- ggplot(df_fl_melt, aes(x = variable , y = OTU, fill = value)) + geom_tile() + coord_fixed(expand = FALSE) +
  scale_fill_gradientn(colours = hm.palette(10)) + theme(legend.position = "none") +
  theme(axis.text.y =element_text(size = 6), axis.text.x = element_blank()) +
  ylab(NULL) + xlab(NULL) + geom_vline(xintercept = 8.5, color = "black") 
p_abundance_fl + labs(title= "fl/fl donor 12wk caecal content", subtitle= "                  NT                         T") 



p_abundance_tg <- ggplot(df_tg_melt, aes(x = variable , y = OTU, fill = value)) + geom_tile() + coord_fixed(expand = FALSE) +
  scale_fill_gradientn(colours = hm.palette(10)) + theme(legend.position = "bottom") + 
  theme(axis.text.x=element_blank(), axis.text.y=element_blank()) + 
  ylab(NULL) + xlab(NULL) +  geom_vline(xintercept = 1.5, color = "black") 
print(p_abundance_tg + labs(title="tg/tg donor", subtitle = " NT                    T"))




p_prev_fl <- ggplot(fl_melt, aes(x = variable , y = OTU, fill = value)) + geom_tile() + coord_fixed() + 
  scale_fill_gradientn(colours = hm.palette2(10)) +  theme(legend.position = "none") + 
  theme(axis.text = element_blank()) + geom_vline(xintercept = 1.5, color = "black") +
  xlab(NULL) + ylab(NULL) 
print(p_prev_fl + ggtitle(" NT T"))


p_prev_tg <- ggplot(tg_melt, aes(x = variable , y = OTU, fill = value)) + geom_tile() + coord_fixed() + 
  scale_fill_gradientn(colours = hm.palette2(10)) + theme(legend.position = "bottom")  +
  theme(axis.text = element_blank()) + geom_vline(xintercept = 1.5, color = "black") +
  xlab(NULL) + ylab(NULL)
print(p_prev_tg + ggtitle(" NT T"))
