library(tidyverse)
library(RColorBrewer)
library(reshape2)
library(gtools)

# arcsine transform function 
#asinTransform <- function(p) { asin(sqrt(p/100)) }



# read input 
df_t <- read.csv("t_5wk_mean.csv", header = TRUE, check.names = FALSE)
df_nt <- read.csv("nt_5wk_mean.csv", header = TRUE, check.names = FALSE)
df_t_prev <- read.csv("t_5wk_prev.csv", header = TRUE, check.names = FALSE)
df_nt_prev <- read.csv("nt_5wk_prev.csv", header = TRUE, check.names = FALSE)

minval <- 0.025

df_t[df_t<minval]=0
df_nt[df_nt<minval]=0


df_t_melt <- melt(df_t)
#df_t_melt[3] <- lapply(df_tg_melt[3], asinTransform)
df_t_melt[3] <- lapply(df_tg_melt[3], log2)
df_t_melt[mapply(is.infinite, df_t_melt)] <- NA
df_t_melt[is.na(df_t_melt)]<- 0


df_nt_melt <- melt(df_nt)
#df_nt_melt[3] <- lapply(df_fl_melt[3], asinTransform)
df_nt_melt[3] <- lapply(df_nt_melt[3], log2)
df_nt_melt[mapply(is.infinite, df_nt_melt)] <- NA
df_nt_melt[is.na(df_nt_melt)]<- -10


t_melt <- melt(df_t_prev)
nt_melt <- melt(df_nt_prev)


hm.palette <- colorRampPalette((brewer.pal(11,"RdYlGn")), space='Lab')




p_abundance_nt <- ggplot(df_nt_melt, aes(x = variable , y = OTU, fill = value)) + geom_tile() + coord_fixed(expand = FALSE) +
  scale_fill_gradientn(colours = hm.palette(10)) + theme(legend.position = "none") +
  theme(axis.text.y =element_text(size = 7), axis.text.x = element_blank()) +
  ylab(NULL) + xlab(NULL) + geom_vline(xintercept = 8.5, color = "black") 
print(p_abundance_nt + labs(title= "non-tumor mean"))#, subtitle= "                  NT                         T") 



p_abundance_t <- ggplot(df_t_melt, aes(x = variable , y = OTU, fill = value)) + geom_tile() + coord_fixed(expand = FALSE) +
  scale_fill_gradientn(colours = hm.palette(10)) + theme(legend.position = "bottom") + 
  theme(axis.text.x=element_blank(), axis.text.y=element_blank()) + 
  ylab(NULL) + xlab(NULL) +  geom_vline(xintercept = 1.5, color = "black") 
print(p_abundance_t + labs(title="tumor mean")) #, subtitle = " NT                    T"))




p_prev_nt <- ggplot(nt_melt, aes(x = variable , y = OTU, fill = value)) + geom_tile() + coord_fixed() + 
  scale_fill_gradientn(colours = hm.palette(10)) +  theme(legend.position = "none") + 
  theme(axis.text = element_blank()) + geom_vline(xintercept = 1.5, color = "black") +
  xlab(NULL) + ylab(NULL) 
print(p_prev_nt + ggtitle("NT"))


p_prev_t <- ggplot(t_melt, aes(x = variable , y = OTU, fill = value)) + geom_tile() + coord_fixed() + 
  scale_fill_gradientn(colours = hm.palette(10)) + theme(legend.position = "bottom")  +
  theme(axis.text = element_blank()) + geom_vline(xintercept = 1.5, color = "black") +
  xlab(NULL) + ylab(NULL)
print(p_prev_t + ggtitle("T"))
