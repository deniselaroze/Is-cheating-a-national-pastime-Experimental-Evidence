

################################################################
## Paper: Is Cheating a national pastime? Experimental evidence
## Author code: Denise Laroze
## Year: 2018
################################################################



library(foreign)
library(ggplot2)
library(readstata13)
library(RColorBrewer)
library(rms)
theme_set(theme_bw())
library(plyr)
library(stargazer)
library(gridExtra)
library(clusterSEs)
library(car)



rm(list=ls())

setwd("C:/Users/Denise Laroze Prehn/Dropbox/CESS-Santiago/Archive/Tax Compliance Moscow/Raw Data")
fig.path <- "Figures"
v<-"Feb2018"

dat <- read.dta13("mastern_final2018.dta")
subject<-read.dta13("subjects_2018.dta")
#online.chile<-read.csv("CESS_Panel_DS_Stgo_2017.csv")
#names(rus)<- tolower(names(rus))


##########################
### Figures
############################
###########

###########################
### Die by type Figure 1
###########################

subject<-subject[ subject$include_data==1,]


subject$ind_typenew3[subject$ind_typenew2==1] <- "Consistent Maximal"
subject$ind_typenew3[subject$ind_typenew2==2] <- "Consistent Partial"
subject$ind_typenew3[subject$ind_typenew2==3] <- "Consistent Honest"
subject$ind_typenew3[subject$ind_typenew2==4] <- "Other"


pt<-prop.table(table(subject$realdie, subject$ind_typenew3), 2)
pt<-as.data.frame(pt)

names(pt)<-c("die", "type", "prop" )
#???die$perform_high_lab<-ifelse(die$perform_high==1, "High Performance", "Low Performance")
pt$type <- factor(pt$type, levels = c("Consistent Maximal", "Consistent Partial", "Consistent Honest", "Other"))



d<-ggplot(pt, aes(x = die, y = prop, colour=type, fill=type)) + geom_bar(stat = "identity" ,position="dodge") + 
  ylab("Fraction") + scale_y_continuous( limits = c(0,1)) +
  xlab("") + labs(colour="", fill="") +
  geom_hline(yintercept = 1/6, lty="dashed", col="red")+
  scale_fill_manual(values= c("red", "darkgreen", "blue", "grey60"), guide = guide_legend(title = "")) +
  theme(legend.position="bottom")
d
ggsave(paste0("die_types",v, ".pdf"), path=fig.path, width = 9, height = 6)


#######################################
### Histogram of declarations Figure 2
######################################

p.df<-dat[dat$ind_typenew2==2,]
p.df<-p.df[p.df$include_data==1,]


p.df<-p.df[!is.na(p.df$country_code), ]



p.df<-cdata[cdata$ind_typenew2==2,]
p.df$country_code2[p.df$country_code==1] <- "Chile"
p.df$country_code2[p.df$country_code==2] <- "Russia"
p.df$country_code2[p.df$country_code==3] <- "U.K."


ggplot(p.df, aes(declared_part_av)) + 
  geom_histogram( aes(y=..density..), col="grey50", fill="grey60", bins=30)+
  scale_x_continuous(breaks=c(0, .1, .2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))+
  facet_wrap(~ country_code2)+
  xlab("") + theme(strip.text = element_text(size=15))
    
ggsave(paste0("hist_declared",v, ".pdf"), path=fig.path, width = 12, height = 6)

