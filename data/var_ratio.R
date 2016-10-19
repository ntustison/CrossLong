setwd('/Users/ntustison/Documents/Academic/InProgress/CrossLong/')
# makes figures based on posterior distributions of precision ratios
# r^k_l and r^k_c.
# commented out section used to create var_ratios.csv

# updated_ratios.txt features posterior summaries of r^k_c (first 62
# lines) and r^k_l (last 62 lines).  we will pull of 2.5 percentile,
# medians, and 97.5 percentile
var_ratio <- read.table("data/updated_ratios.txt",
                        quote="\"", comment.char="")
library(ggplot2)
library(brainGraph)
library(tidyr)
library(plyr)

# since the first 62 lines of updated_ratios.txt pertain to
# cross-sectional and last 62 pertain to longutidinal,
# longGthan (longitudinal greater than cross-sectional) is posterior
# median (col 7) of r^k_l minus that of r^k_c for each region k.
longGthan <- rep(0, 62)
for(i in 1:62){
  longGthan[i] <- var_ratio[i+62,7]-var_ratio[i,7]
}

# normalized
longGthan_norm <- rep(0, 62)
for(i in 1:62){
  longGthan_norm[i] <- (var_ratio[i+62,7]-var_ratio[i,7])/
    (var_ratio[i+62,7]+var_ratio[i,7])
}


# # makes var_ratios_2.csv
# names <- dk[c(2:30,33,34,36:64,67,68),]$name #same as dkt$names
# X <- cbind(names,as.numeric(longGthan), as.numeric(longGthan_norm))
#
# write.csv(X,file='Template/var_ratios_2.csv',col.names = FALSE,
#           row.names = TRUE)



##############################plots###################################

#
####
####### plot 95% posterior credible intervals and medians of r^k_l,c
####
#

group <- c(rep('Cross-sectional',62),rep('Longitudinal',62))
ids <- c(dk[c(2:30,33,34,36:64,67,68),]$name,
         dk[c(2:30,33,34,36:64,67,68),]$name)
longGthan <- c(longGthan, longGthan)
var_ratio <- data.frame(as.numeric(var_ratio$V5),
                   as.numeric(var_ratio$V7),
                   as.numeric(var_ratio$V9),ids,group,longGthan)
colnames(var_ratio) <- c('one','two','three','Regions','Method','longGthan')
var_ratio <- as.data.frame(var_ratio)
var_ratio <- var_ratio[order(longGthan,c(1:62,1:62)),]
var_ratio$Regions <- factor(var_ratio$Regions, levels=unique(var_ratio$Regions))
limits <- aes(ymax = three, ymin=one)

ggplot(data=var_ratio, aes(colour=Method, y=two, x=Regions)) +
  geom_point() + geom_errorbar(limits) +
  geom_vline(xintercept=24.5, alpha=.7) +
  geom_vline(xintercept=5.5, alpha=.4) +
  geom_vline(xintercept=37.5, alpha=.4) +
  ylab(expression(r^k)) +
  xlab('Regions') +
  theme(axis.text.x  = element_text(angle=45,hjust=1))+
  scale_y_continuous(breaks=c(2,3,4,5,6))
ggsave('Figures/credibleIntervals.pdf', width=11, height=4)

#
####
####### median r^k_l minus median r^k_c
####
#

# updated_ratios.txt features posterior summaries of r^k_c (first 62
# lines) and r^k_l (last 62 lines).  we will pull of 2.5 percentile,
# medians, and 97.5 percentile
var_ratio <- read.table("data/updated_ratios.txt",
                        quote="\"", comment.char="")

# since the first 62 lines of updated_ratios.txt pertain to
# cross-sectional and last 62 pertain to longutidinal,
# longGthan (longitudinal greater than cross-sectional) is posterior
# median (col 7) of r^k_l minus that of r^k_c for each region k.
longGthan <- rep(0, 62)
for(i in 1:62){
  longGthan[i] <- var_ratio[i+62,7]-var_ratio[i,7]
}

# normalized
longGthan_norm <- rep(0, 62)
for(i in 1:62){
  longGthan_norm[i] <- (var_ratio[i+62,7]-var_ratio[i,7])/
    (var_ratio[i+62,7]+var_ratio[i,7])
}

ids <- dk[c(2:30,33,34,36:64,67,68),]$name
df <- data.frame(c(longGthan,longGthan_norm),c(ids,ids),
                 c(rep('unnormalized',62),rep('normalized',62)))
colnames(df) <- c('longGthan', 'Regions', 'Statistic')
df <- df[order(c(df$longGthan[df$Statistic=='unnormalized'],
                 df$longGthan[df$Statistic=='unnormalized']),c(1:62,1:62)),]
df$Regions <- factor(df$Regions, levels=unique(df$Regions))

ggplot(data=df, aes(colour=Statistic, y=longGthan, x=Regions)) +
  geom_point()+
  geom_vline(xintercept=24.5, alpha=.7) +
  geom_vline(xintercept=5.5, alpha=.4) +
  geom_vline(xintercept=37.5, alpha=.4) +
  ylab(expression(r[l]^k - r[c]^k)) +
  theme(axis.text.x  = element_text(angle=45,hjust=1))
ggsave('Figures/differences_medians.pdf', width=11, height=4)


