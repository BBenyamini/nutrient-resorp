#################################################################################
################################Data Analysis Nutrient Resorption vs. Nutient Concentration#################
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(car)

##########################Preparation######################

mycolors <- palette(gray.colors(6, start= 0.3, end =0.9))
###### Preparing data for analysis

#import litter data

cols <- c(seq(3,19,2),23,27,28)#vector for selecting columns
data <- X230420_PLANT_JEREMY#renaming data
litter_data <- data[,cols]#selecting columns
litter_data <-na.omit(litter_data)#omitting NA's

litter_data$Season <- as.factor(litter_data$Season)#converting season to factor
litter_data$Region <- factor(litter_data$Region, c("UPL", "KBPL", "NPL"))#converting region to factor, establishing order in Factor so that boxplots are given in right order. 
litter_data$plotID <- as.factor(litter_data$plotID)#converting plotID to factor

WR <- litter_data[litter_data$Season=="WR",]#creating datasets for each season
SR <- litter_data[litter_data$Season=="SR",]
WD <- litter_data[litter_data$Season=="WD",]
SD <- litter_data[litter_data$Season=="SD",]

plot(litter_data)#overview of data

####################Analysis##############################

col_vec <- c(rep(1,12), rep(2,7), rep(3, 12))#color vector for different regions

par(mfrow=c(4,2))#adjusting number of window pane layout

#####################Barplots and Boxplots for individual seasons#######

#####Exp. Aluminium, Weak Rain Season

barplot(height = WR$Al_mg_kg_dry_weight, names.arg =WR$plotID,las=2, main="Al concentration in litter samples from the Weak Rain season (mg/kg)", col=col_vec)

Boxplot(Al_mg_kg_dry_weight~Region, data=WR, col = mycolors[c(1,2,3)], id.method="y", main="Al concentration in litter samples from the Weak Rain season (mg/kg), by region", id=WR$plotID)

means <- tapply(WR$Al_mg_kg_dry_weight, WR$Region, mean)#calculate mean value by region
points(means, pch=17, cex=1.5, col="black")#add means as triangles to each boxplot

#####################Barplots and Boxplots grouped for the whole year####

par(mfrow=c(4,2))

col_vec <- c(rep(1,7),rep(2,11), rep(3, 12))#color vector for different Regions, for SD UPL +1 and KBPL +2
#litter_data <- litter_data[!litter_data$Region=="KBPL",] #excluding KBPL
#litter_data$Region <- as.factor(litter_data$Region)
#levels(litter_data$Region) <- c("Sedimentary", "Felsic")

#############Exp. Alumminium, whole year

barplot(height = tapply(litter_data$Al_mg_kg_dry_weight, litter_data$plotID, mean), names.arg =levels(litter_data$plotID),las=2, main="Al concentration in litter samples in litter samples across the year (mg/kg)", col=col_vec, ylab= "", cex.main=2.3, cex.axis=2.3, xlab="")

Boxplot(Al_mg_kg_dry_weight~Region, data=litter_data, col=c(0,5),i7d.method="y", main="Regional Al concentration in litter samples across the year (mg/kg)", id=litter_data$plotID, ylab= "", cex.main=2.3, cex.axis=2.3, xlab="")

means <- tapply(litter_data$Al_mg_kg_dry_weight, litter_data$Region, mean)
points(means, pch=17, cex=1.5, col="black")

#####################Boxplot dependent on Region###########
#####Aluminium
p1 <- ggplot(litter_data,aes(Region, Al_mg_kg_dry_weight, fill=Region))+geom_boxplot( show.legend = FALSE)+ggtitle("Al concentration per Region and per Season in mg/kg")+theme(plot.title = element_text(hjust = 0.5)) + scale_fill_brewer(palette = "Greys")+theme(axis.title.y = element_blank())
#creating boxplot using ggplot, for Al grouped for Region and Season. Ggtitle adds the title with a correction to show it in the middle of the plot and scale_fill allows display in grey tones to match the other plots. 

#####Calcium

p2 <- ggplot(litter_data,aes(Region, Ca_mg_kg_dry_weight, fill=Region))+geom_boxplot( show.legend = FALSE)+ggtitle("Ca concentration per Region and per Season in mg/kg")+theme(plot.title = element_text(hjust = 0.5)) + scale_fill_brewer(palette = "Greys")+theme(axis.title.y = element_blank())
#creating boxplot using ggplot, for Ca grouped for Region and Season. Ggtitle adds the title with a correction to show it in the middle of the plot and scCae_fill Calows display in grey tones to match the other plots. 

#####Iron

p3 <- ggplot(litter_data,aes(Region, Fe_mg_kg_dry_weight, fill=Region))+geom_boxplot( show.legend = FALSE)+ggtitle("Fe concentration per Region and per Season in mg/kg")+theme(plot.title = element_text(hjust = 0.5)) + scale_fill_brewer(palette = "Greys")+theme(axis.title.y = element_blank())
#creating boxplot using ggplot, for Fe grouped for Region and Season. Ggtitle adds the title with a correction to show it in the middle of the plot and scale_fill allows display in grey tones to match the other plots. 

#####Kalium

p4 <- ggplot(litter_data,aes(Region, K_mg_kg_dry_weight, fill=Region, fill=Region))+geom_boxplot( show.legend = FALSE)+ggtitle("K concentration per Region and per Season in mg/kg")+theme(plot.title = element_text(hjust = 0.5)) + scale_fill_brewer(palette = "Greys")+theme(axis.title.y = element_blank())
#creating boxplot using ggplot, for K grouped for Region and Season. Ggtitle adds the title with a correction to show it in the middle of the plot and scale_fill allows display in grey tones to match the other plots. 

#####Magnesium

p5 <- ggplot(litter_data,aes(Region, Mg_mg_kg_dry_weight, fill=Region))+geom_boxplot( show.legend = FALSE)+ggtitle("Mg concentration per Region and per Season in mg/kg")+theme(plot.title = element_text(hjust = 0.5)) + scale_fill_brewer(palette = "Greys")+theme(axis.title.y = element_blank())
#creating boxplot using ggplot, for Mg grouped for Region and Season. Ggtitle adds the title with a correction to show it in the middle of the plot and scale_fill allows display in grey tones to match the other plots. 

#####Mangan

p6 <- ggplot(litter_data,aes(Region, Mn_mg_kg_dry_weight, fill=Region))+geom_boxplot( show.legend = FALSE)+ggtitle("Mn concentration per Region and per Season in mg/kg")+theme(plot.title = element_text(hjust = 0.5)) + scale_fill_brewer(palette = "Greys")+theme(axis.title.y = element_blank())
#creating boxplot using ggplot, for Mn grouped for Region and Season. Ggtitle adds the title with a correction to show it in the middle of the plot and scale_fill allows display in grey tones to match the other plots.

#####Sodium

p7 <- ggplot(litter_data,aes(Region, Na_mg_kg_dry_weight, fill=Region))+geom_boxplot( show.legend = FALSE)+ggtitle("Na concentration per Region and per Season in mg/kg")+theme(plot.title = element_text(hjust = 0.5)) + scale_fill_brewer(palette = "Greys")+theme(axis.title.y = element_blank())
#creating boxplot using ggplot, for Na grouped for Region and Season. Ggtitle adds the title with a correction to show it in the middle of the plot and scale_fill allows display in grey tones to match the other plots.

####Phosphor
colors <- brewer.pal(n = 4, name = "Greys")

p8 <- ggplot(litter_data,aes(Region, P_mg_kg_dry_weight, fill=Region))+geom_boxplot()+ggtitle("P concentration per Region and per Season in mg/kg")+theme(plot.title = element_text(hjust = 0.5))+theme(axis.title.y = element_blank())
#creating boxplot using ggplot, for P grouped for Region and Season. Ggtitle adds the title with a correction to show it in the middle of the plot and scale_fill allows display in grey tones to match the other plots. Changing labels to full season names and adjusting the colours to match the other plots. 

grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8, nrow=4)#arranging on one sheet

#####################Resorption efficiencies##############

#import fresh leaves data


col_vec <- c(rep(1,3),rep(2,11), rep(3, 16))#color vector for different regions, for SD UPL +1 and KBPL +2

litter_data <- litter_data[,1:10]#only selecting columns that have values in them, all other information is stored in the fresh_leaves data set anyway. 

litter_data <- litter_data %>% group_by(plotID) %>% 
  summarise(across(everything(), mean),
            .groups = 'drop')  %>%
  as.data.frame()#averaging the data across seasons so that we have one value per plot. 


common <- inner_join(litter_data, fresh_leaves)#finding common plots between data sets
common <- common[4:26,]#removing mafic region
common$slope <- as.factor(common$slope)#converting slope to factor


##using Ca as an inert tracer to correct for mass loss. 

tracer <- common$Ca_mg_kg_dry_weight/common$Ca# as Ca is never resorbed we assume that Ca remained constant and therefore correct by the persentage by which the Ca sample increased. 

common$tracer <- tracer
par(mfrow=c(1,1))

barplot(height =  ((common$Al_mg_kg_dry_weight/tracer)/common$Al)*-1, names.arg =common$plotID,las=2, main="Resorption of Al as a factor", ylab= "")#creating barplot to test

#creating boxplot grouped by region

mycolors <- brewer.pal(n = 4, name = "Greys")

#####K
p4_resorp <- ggplot(common,aes(region, (((K_mg_kg_dry_weight/tracer)/K)-1)*-1, fill=region))+geom_boxplot( show.legend = FALSE)+ggtitle("Resorption of K by region, corrected using Ca as tracer")+theme(plot.title = element_text(size = 16))+ scale_fill_brewer(palette = "Greys")+theme(axis.title.y = element_blank(), axis.title.x = element_blank(), axis.text.x =element_text(size=16), axis.text.y= element_text(size=16))+ scale_y_continuous(labels = function(x) paste0(x*100, "%"))
#subtracted 1 to see not how much remains, but how much was resorbed, adjusted scales for percent

###P
p8_resorp <- ggplot(common,aes(region, (((P_mg_kg_dry_weight/tracer)/P)-1)*-1, fill=region))+geom_boxplot( show.legend=FALSE)+ggtitle("Resorption of P by region, corrected using Ca as tracer")+theme(plot.title = element_text(size=16))+ scale_fill_brewer(palette = "Greys")+theme(axis.title.y = element_blank(), axis.title.x = element_blank(), axis.text.x =element_text(size=16), axis.text.y= element_text(size=16))+ scale_y_continuous(labels = function(x) paste0(x*100, "%"))
#subtracted 1 to see not how much remains, but how much was resorbed, adjusted scales for percent

#####################Litter######################

mycolors <- c("#F7F7F7", "#CCCCCC", "#969696", "#636363", "#252525")

p4_litter <- ggplot(common, aes(region, K_mg_kg_dry_weight/tracer, fill=region))+geom_boxplot( show.legend=FALSE)+ggtitle("K conc. in mg/kg in litter by region, corrected using Ca as tracer")+theme(plot.title = element_text(size=16))+ scale_fill_brewer(palette = "Greys")+theme(axis.title.y = element_blank(), axis.title.x = element_blank(), axis.text.x =element_text(size=16), axis.text.y= element_text(size=16))
p8_litter <- ggplot(common, aes(region, P_mg_kg_dry_weight/tracer, fill=region))+geom_boxplot(show.legend=FALSE)+ggtitle("P conc. in mg/kg in litter by region, corrected using Ca as tracer")+theme(plot.title = element_text(size=16))+scale_fill_brewer(palette = "Greys")+theme(axis.title.y = element_blank(), axis.title.x = element_blank(), axis.text.x =element_text(size=16), axis.text.y= element_text(size=16))

###################Fresh Leaves#####
fresh_leaves <- fresh_leaves[11:34,]

p4_fresh_leaves <- ggplot(fresh_leaves, aes(region, K, fill=region))+geom_boxplot( show.legend=FALSE)+ggtitle("K conc. in mg/kg in canopy (fresh leaves) by region")+theme(plot.title = element_text(size=16))+ scale_fill_brewer(palette = "Greys")+theme(axis.title.y = element_blank(), axis.title.x = element_blank(), axis.text.x =element_text(size=16), axis.text.y= element_text(size=16))
p8_fresh_leaves <- ggplot(fresh_leaves, aes(region, P, fill=region))+geom_boxplot( show.legend=FALSE)+ggtitle("P conc. in mg/kg in canopy (fresh leaves) by region")+theme(plot.title = element_text(size=16))+ scale_fill_brewer(palette = "Greys")+theme(axis.title.y = element_blank(), axis.title.x = element_blank(), axis.text.x =element_text(size=16), axis.text.y= element_text(size=16))

####################Bulk Soil########

#import soil data

soil_data <- X223_soil_spec
soil_data <- soil_data[, c(1, 29:33, 35:37)]#removing unnecessary columns

soil_data <- soil_data %>% group_by(plotID) %>% 
  summarise(across(everything(), mean),
            .groups = 'drop')  %>%
  as.data.frame()#averaging the data across seasons so that we have one value per plot. 

common_soil <- inner_join(soil_data, common)

#####K

p4_soil <- ggplot(common_soil,aes(region, K_ICPOES_spec, fill=region))+geom_boxplot( show.legend = FALSE)+ggtitle("K conc. in mg/kg, in bulk soil by region")+theme(plot.title = element_text(size=16))+ scale_fill_brewer(palette = "Greys")+theme(axis.title.y = element_blank(), axis.title.x = element_blank(), axis.text.x =element_text(size=16), axis.text.y= element_text(size=16))

###P

p8_soil <- ggplot(common_soil,aes(region, P_ICPOES_spec, fill=region))+geom_boxplot( show.legend = FALSE)+ggtitle("P conc. in mg/kg, in bulk soil by region")+theme(plot.title = element_text(size=16))+ scale_fill_brewer(palette = "Greys")+theme(axis.title.y = element_blank(), axis.title.x = element_blank(), axis.text.x =element_text(size=16), axis.text.y= element_text(size=16))


#####################Arranging everything in one frame

grid.arrange(p4_soil, p8_soil,p4_fresh_leaves, p8_fresh_leaves, p4,p8,p4_litter, p8_litter, nrow=4)#arranging soil (below), fresh leaf conc. (below) and resorption on one sheet

###########Plant Available Soil Bulk conc. for P and K#############

soil_phys_chem <- X223_soil_spec_new_measurements#renaming
soil_phys_chem <- na.omit(soil_phys_chem)#removing rows with NA
soil_phys_chem <- soil_phys_chem %>% group_by(plotID) %>% 
  summarise(across(everything(), mean),
            .groups = 'drop')  %>%
  as.data.frame()#averaging across plot ID
common_soil1 <- inner_join(common_soil,soil_phys_chem)#joining dfs
common_soil1 <- common_soil1[,c(1,3,6,13, 17, 19,20, 23, 27, 35, 36, 37)]#selecting for specific columns we need to reduce the size of the df

common_soil1$K_ICPOES_spec/common_soil1$`K_avail_mg_g`
common_soil1$P_ICPOES_spec/common_soil1$`P_avail_mg_g`

par(mfrow=c(1,2))
p4_soil_alt <- ggplot(common_soil1,aes(region, K_avail_mg_g, fill=region))+geom_boxplot( show.legend = FALSE)+ggtitle("Plant Available K conc. in mg/kg, in bulk soil by region")+theme(plot.title = element_text(size=16))+ scale_fill_brewer(palette = "Greys")+theme(axis.title.y = element_blank(), axis.title.x = element_blank(), axis.text.x =element_text(size=16), axis.text.y= element_text(size=16) )#boxplots for alternative soil measurements
p8_soil_alt <- ggplot(common_soil1,aes(region, P_avail_mg_g, fill=region))+geom_boxplot( show.legend = FALSE)+ggtitle("Plant Available P conc. in mg/kg, in bulk soil by region")+theme(plot.title = element_text(size=16))+ scale_fill_brewer(palette = "Greys")+theme(axis.title.y = element_blank(), axis.title.x = element_blank(), axis.text.x =element_text(size=16), axis.text.y= element_text(size=16))

grid.arrange(p4_soil, p8_soil, p4_soil_alt, p8_soil_alt, nrow=2)                      


################################################Regresssions###########################


###########P resorption rate vs. soil P conc. #########
#x-axis should be nutrient conc. and y-axis should be resorption rate. The different regions should be shown as different symbols Afterwards a linear model should go through the data.
par(mfrow=c(1,1))

pchs <- as.numeric(factor(common_soil$region))+3#creating vector with pch data in it

P_resorp <- (((common_soil$P_mg_kg_dry_weight/tracer)/common_soil$P)-1)*-1#creating vector with resorption efficiencies
P_conc <- common_soil$P_ICPOES_spec#creating vector with conc. 


plot(P_conc, P_resorp, pch=pchs, lwd=2, xlab="Soil P conc.", ylab="P resorption rate", main="P resorption rate vs. Soil P conc. ", cex.main=2, cex.lab=2)#creating plot with points, regions differentiated by pch
legend("topleft", y=NULL, legend =c("Overall, R2 = 0.08326, p-value = 0.182, DF = 21", "Sedimentary, R2 = 0.6913, p-value = 0.00151, DF = 9 ", "Felsic, R2 = 0.1182, p-value = 0.274, DF = 10"), lty=c(5,1,5), pch=c(26,5,4), bg="White", col=c("Black", "Red", "Blue"), cex=1.5, bty="n")
lmP1 <- lm(P_resorp~P_conc)#creating linear model
summary(lmP1)#R2 0.08326, p-value of 0.182 and 21 DF
par(mfrow=c(2,2))
plot(lmP1)#looking at residual plots
abline(lmP1, col="black", lty=5)#adding regression line

###########Individual Regressions ########
#x-axis should be nutrient conc. and y-axis should be resorption rate. The different regions should be shown as different symbols Afterwards a linear model should go through the data.
#Sedimentary
common_soil_sed <- common_soil[common_soil$region=="Sedimentary",]#selecting for only sedimentary region
P_resorp <- (((common_soil_sed$P_mg_kg_dry_weight/common_soil_sed$tracer)/common_soil_sed$P)-1)*-1
P_conc <- common_soil_sed$P_ICPOES_spec

lmPsed1 <- lm(P_resorp~P_conc)#linear model for individual regression
summary(lmPsed1)#R2 of 0.6913, p-value of 0.00151, and 9DF
par(mfrow=c(2,2))
plot(lmPsed1)

abline(lmPsed1, col="red", lty=1)


#Felsic
common_soil_fel <- common_soil[common_soil$region=="Felsic",]
P_resorp <- (((common_soil_fel$P_mg_kg_dry_weight/common_soil_fel$tracer)/common_soil_fel$P)-1)*-1
P_conc <- common_soil_fel$P_ICPOES_spec

lmPfel1 <- lm(P_resorp~P_conc)
summary(lmPfel1)#R2 of 0.1182, p-value of 0.274 and 10 DF
par(mfrow=c(2,2))
plot(lmPfel1)

abline(lmPfel1, col="blue", lty=5)

###########K resorption rate vs. soil K conc. ########
#x-axis should be nutrient conc. and y-axis should be resorption rate. The different regions should be shown as different symbols Afterwards a linear model should go through the data.

par(mfrow=c(1,1))
K_resorp <- (((common_soil$K_mg_kg_dry_weight/tracer)/common_soil$K)-1)*-1
K_conc <- common_soil$K_ICPOES_spec

plot(K_conc, K_resorp, pch=pchs, lwd=2, xlab="Soil K conc.", ylab="K resorption rate", main="K resorption rate vs. Soil K conc. ", cex.main=2, cex.lab=2)
legend("bottomright", y=NULL, legend =c("Overall, R2 = 0.218, p-value = 0.0247, DF = 21", "Sedimentary, R2 = 0.1081, p-value = 0.32616, DF = 9", "Felsic, R2 = 0.5136, p-value = 0.00872, Df = 10"),lty=c(1, 5, 1), pch=c(4,5), bg="White", col=c("Black", "Red", "Blue"), cex=1.5, bty="n")
lmK1 <- lm(K_resorp~K_conc)
summary(lmK1)#R2 = 0.218, p-value=0.0247, DF=21
par(mfrow=c(3,2))
plot(lmK1)
hist(lmK1$residuals)#histogram of residuals
abline(lmK1, col="black", lty=1)

###########Individual Regressions ########
#x-axis should be nutrient conc. and y-axis should be resorption rate. The different regions should be shown as different symbols Afterwards a linear model should go through the data.
#Sedimentary
common_soil_sed <- common_soil[common_soil$region=="Sedimentary",]
K_resorp <- (((common_soil_sed$K_mg_kg_dry_weight/common_soil_sed$tracer)/common_soil_sed$K)-1)*-1
K_conc <- common_soil_sed$K_ICPOES_spec


lmKsed2 <- lm(K_resorp~K_conc)
summary(lmKsed2)# R2 = 0.1081, p-value= 0.32616, DF = 9
par(mfrow=c(2,2))
plot(lmKsed2)

abline(lmKsed2, col="red", lty=5)

#Felsic
common_soil_fel <- common_soil[common_soil$region=="Felsic",]
K_resorp <- (((common_soil_fel$K_mg_kg_dry_weight/common_soil_fel$tracer)/common_soil_fel$K)-1)*-1
K_conc <- common_soil_fel$K_ICPOES_spec


lmKfel1 <- lm(K_resorp~K_conc)
summary(lmKfel1)#R2 = 0.5136, p-value 0.00872, DF= 10
par(mfrow=c(2,2))
plot(lmKfel1)

abline(lmKfel1, col="blue", lty=1)


#############Regressions with plant available soil bulk conc. ############

#x-axis should be nutrient conc. and y-axis should be resorption rate. The different regions should be shown as different symbols Afterwards a linear model should go through the data.
par(mfrow=c(1,1))

pchs <- as.numeric(factor(common_soil1$region))+3


P_resorp <- (((common_soil1$P_mg_kg_dry_weight/tracer)/common_soil1$P)-1)*-1
P_conc <- common_soil1$P_avail_mg_g


plot(P_conc, P_resorp, pch=pchs, lwd=2, xlab="Soil P conc.", ylab="P resorption rate", main="P resorption rate vs. Soil P conc. ")
legend("bottomleft", y=NULL, legend =c("Sedimentary", "Felsic"), pch=c(4,5), bg="White", title="Region")

lmP1 <- lm(P_resorp~P_conc)
summary(lmP1)
par(mfrow=c(2,2))
plot(lmP1)

abline(lmP1, col="red", lty=3)

###################Regression for Resoprtion rate vs. Canopy nutrient conc.
pchs <- as.numeric(factor(common_soil$region))+3


par(mfrow=c(1,1))
K_resorp <- (((common_soil$K_mg_kg_dry_weight/tracer)/common_soil$K)-1)*-1
K_conc_canopy <- common_soil$K


plot(K_conc_canopy, K_resorp, pch=pchs, lwd=2, xlab="Soil K conc.", ylab="K resorption rate", main="K resorption rate vs. Soil K conc. ")
legend("bottomright", y=NULL, legend =c("Sedimentary", "Felsic"), pch=c(4,5), bg="White", title="Region")
lmKcan1 <- lm(K_resorp~K_conc_canopy)
summary(lmKcan1)
par(mfrow=c(2,2))
plot(lmKcan1)
abline(lmKcan1, col="red", lty=4)

#no significant relationship but the residual plots look good. According topaper we would expect an inverse relationship. 

pchs <- as.numeric(factor(common_soil$region))+3

par(mfrow=c(1,1))
P_resorp <- (((common_soil$P_mg_kg_dry_weight/tracer)/common_soil$P)-1)*-1
P_conc_canopy <- common_soil$P


plot(P_conc_canopy, P_resorp, pch=pchs, lwd=2, xlab="Foliar P conc.", ylab="P resorption rate", main="P resorption rate vs. Foliar P conc. ")
legend("bottomright", y=NULL, legend =c("Sedimentary", "Felsic"), pch=c(4,5), bg="White", title="Region")
lmPcan1 <- lm(P_resorp~P_conc_canopy)
summary(lmPcan1)
par(mfrow=c(2,2))
plot(lmPcan1)
abline(lmPcan1, col="red", lty=4)

#slightly signigicant relationship with P, residual plots also look ok. This is the relationship they reported in their study. 
#Very similar R^2 as in other study, slightly lower p value. Its probably worth mentioning that this relationship was reaffirmed. 