setwd('~/CrossLong')

var_ratio <- read.csv("~/CrossLong/data/model_results_FINAL.txt", sep="")

library(ggplot2)
library(brainGraph)
library(tidyr)
library(plyr)
library(gridExtra)

data(dkt)
var_ratio$Method <- rep(c('Cross-sectional','Long-SST','Long-native',
                          'Cross-sectional','Long-SST'),each=62*3)
var_ratio$Regions <- rep(dkt$name,15)
var_ratio$Variable <- rep(rep(c('ratio','sigma','tau'),each=62),5)
var_ratio$Software <- c(rep('ANTs',186*3),rep('FS',186*2))

###############################PLOTS##################################


#
####
####### compare all ANTs
####
#

df <- var_ratio[var_ratio$Variable=='ratio'&var_ratio$Software=='ANTs',
                c(5,7,9,12:14)]

colnames(df) <- c('Lower','Median','Upper','Method','Regions','Variable')
df$Regions <- factor(df$Regions, levels=unique(df$Regions))
limits <- aes(ymax = Upper, ymin=Lower)
#separ <- df$Lower[df$Method=='Longitudinal2'] >
# df$Upper[df$Method=='Longitudinal1']

p1 <- ggplot(data=df, aes(colour=Method, y=Median, x=Regions)) +
  geom_point() + geom_errorbar(limits) +
  ylab('Variance ratio') +
  xlab('Cortical region') +
  theme(axis.text.x  = element_text(angle=45,hjust=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_y_continuous(breaks=c(2,3,4,5,6,7,8)) 

ggsave('Figures/variance_ratios_withScr.pdf',p1, device=pdf, height=4,width=11)

################################ANTs2FS###############################

#
###
##### compare ANTs long-sst to FS long-sst
###
#

df <- var_ratio[var_ratio$Variable=='ratio'&
                  var_ratio$Method=='Long-SST',
                c(5,7,9,12:15)]

colnames(df) <- c('Lower','Median','Upper','Method','Regions','Variable','Software')
df$Regions <- factor(df$Regions, levels=unique(df$Regions))
limits <- aes(ymax = Upper, ymin=Lower)
#separ <- df$Lower[df$Method=='Longitudinal2'] >
# df$Upper[df$Method=='Longitudinal1']

p2 <- ggplot(data=df, aes(colour=Software, y=Median, x=Regions)) +
  geom_point() + geom_errorbar(limits) +
  ylab('Variance ratio') +
  xlab('Cortical region') +
  theme(axis.text.x  = element_text(angle=45,hjust=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_y_continuous(breaks=c(2,3,4,5,6,7,8)) 

ggsave('Figures/variance_ratios_ANTs2FS.pdf',p2, device=pdf, height=4,width=11)

