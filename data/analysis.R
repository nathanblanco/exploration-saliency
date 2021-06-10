



cong_data <- read.table('congruent.txt', header=F)
comp_data <- read.table('competition.txt', header=F)
bl_data <- read.table('baseline.txt', header=F)

names(cong_data) <- c('subj', 'phase', 'trial', 'cond', 'res', 'rt', 'reward', 'total', 'image', 'r1', 'r2', 'r3', 'r4')
names(comp_data) <- c('subj', 'phase', 'trial', 'cond', 'res', 'rt', 'reward', 'total', 'image', 'r1', 'r2', 'r3', 'r4')
names(bl_data) <- c('subj', 'phase', 'trial', 'cond', 'res', 'rt', 'reward', 'total', 'image', 'r1', 'r2', 'r3', 'r4')



#############################################################################
# 	SWITCH ANALYSES

cong_data$stay <- NULL
for (i in 1:length(cong_data[,1])) {
    cong_data$stay[i] <- as.numeric(cong_data[i,]$res == cong_data[i-1,]$res)
    
    if (cong_data[i,]$trial == 1) { cong_data$stay[i] <- 0 }
    
    }
    
comp_data$stay <- NULL
for (i in 1:length(comp_data[,1])) {
    comp_data$stay[i] <- as.numeric(comp_data[i,]$res == comp_data[i-1,]$res)
    
    if (comp_data[i,]$trial == 1) { comp_data$stay[i] <- 0 }
    
    }
    
bl_data$stay <- NULL
for (i in 1:length(bl_data[,1])) {
    bl_data$stay[i] <- as.numeric(bl_data[i,]$res == bl_data[i-1,]$res)
    
    if (bl_data[i,]$trial == 1) { bl_data$stay[i] <- 0 }
    
    }
       

      
cong_stay_total <- tapply(cong_data$stay, cong_data$subj, sum)
for (i in 1:length(cong_stay_total)) {
	x <- binom.test(cong_stay_total[i], 100, 0.25)
	print(x)
	
}

comp_stay_total <- tapply(comp_data$stay, comp_data$subj, sum)
for (i in 1:length(comp_stay_total)) {
	x <- binom.test(comp_stay_total[i], 100, 0.25)
	print(x)
	
}

bl_stay_total <- tapply(bl_data$stay, bl_data$subj, sum)
for (i in 1:length(bl_stay_total)) {
	x <- binom.test(bl_stay_total[i], 100, 0.25)
	print(x)
	
}



t.test(cong_stay_total, comp_stay_total, var.equal=T)
t.test(bl_stay_total, comp_stay_total, var.equal=T)
t.test(cong_stay_total, bl_stay_total, var.equal=T)


cong_stay_props <- tapply(cong_data$stay, cong_data$subj, mean)
comp_stay_props <- tapply(comp_data$stay, comp_data$subj, mean)
bl_stay_props <- tapply(bl_data$stay, bl_data$subj, mean)
jpeg('graphs/switch_hists.jpg')
par(mfrow=c(1,3))
hist(cong_stay_props, main= "Congruent", xlab="Proportion of repeat responses", col='grey', xlim=c(0,1))
hist(comp_stay_props, main= "Competition", xlab="Proportion of repeat responses", col='grey', xlim=c(0,1))
hist(bl_stay_props, main= "Baseline", xlab="Proportion of repeat responses", col='grey', xlim=c(0,1))
dev.off()

###########################

graph_data <- NULL
graph_data$switches <- c(1-(cong_stay_total)/100, 1-(comp_stay_total)/100, 1-(bl_stay_total)/100)
graph_data$group <- factor(c(rep("Congruent", length(cong_stay_total)),
							 rep("Competition", length(comp_stay_total)),
							 rep("Baseline", length(bl_stay_total))))
graph_data <- as.data.frame(graph_data)

#jpeg('graphs/switch_box.jpg')
boxplot(switches ~ group, data=graph_data,
 ylab = 'Proportion of switch responses', col='white', ylim=c(0,1),
 main = 'Response Switching', names.arg=c('Congruent', 'Competition', 'Baseline'))
stripchart(switches ~ group, data=graph_data, vertical = TRUE,  
    method = "jitter", add = TRUE, pch = 19, col = c('blue', 'red', 'green'))


abline(0.83, 0, col='red')
abline(0.65, 0, col='red')

x <- aov( switches ~ group, data=graph_data)
summary(x)
#dev.off()

# SWITCH PROPORTIONS ANOVA
#> x <- aov( switches ~ group, data=graph_data)
#> summary(x)
#             Df Sum Sq Mean Sq F value   Pr(>F)    
#group         2  2.237  1.1184   17.42 2.83e-07 ***
#Residuals   107  6.871  0.0642                     
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

t.test(graph_data[graph_data$group=='Congruent',]$switches, graph_data[graph_data$group=='Competition',]$switches,var.equal=T)
t.test(graph_data[graph_data$group=='Baseline',]$switches, graph_data[graph_data$group=='Competition',]$switches,var.equal=T)
t.test(graph_data[graph_data$group=='Congruent',]$switches, graph_data[graph_data$group=='Baseline',]$switches,var.equal=T)

d.t.unpaired(3.035, 37, 37)
d.t.unpaired(3.2223, 37, 36)
d.t.unpaired(5.5738, 37, 36)

#################################################################

cong_choices <- tapply(cong_data$reward, list(cong_data$subj, cong_data$reward), length)
cong_choices[is.na(cong_choices)] <- 0

comp_choices <- tapply(comp_data$reward, list(comp_data$subj, comp_data$reward), length)
comp_choices[is.na(comp_choices)] <- 0

bl_choices <- tapply(bl_data$reward, list(bl_data$subj, bl_data$reward), length)
bl_choices[is.na(bl_choices)] <- 0

cong_props <- cong_choices[,4]/100
comp_props <- comp_choices[,4]/100
bl_props <- bl_choices[,4]/100

t.test(cong_props, comp_props, var.equal=T)
t.test(bl_props, comp_props, var.equal=T)
t.test(cong_props, bl_props, var.equal=T)


results <- NULL

results$props <- c(cong_props, comp_props, bl_props)
results$cond <- factor(c(rep(1, length(cong_props)),
						rep(2, length(comp_props)),
						rep(3, length(bl_props))))
						
results <- as.data.frame(results)

x <- aov(props~cond, data=results)
summary(x)

t.test(cong_props, comp_props, var.equal=T)
t.test(cong_props, bl_props, var.equal=T)
t.test(bl_props, comp_props, var.equal=T)

d.t.unpaired<-function(t.val,n1,n2){
  d<-t.val*sqrt((n1+n2)/(n1*n2))
  names(d)<-"effect size d"
  return(d)
}

d.t.unpaired(3.93, 37, 37)

d.t.unpaired(0.57193, 36, 37)

####################################################################

cong_props <- cong_choices[,1]/100
comp_props <- comp_choices[,1]/100
bl_props <- bl_choices[,1]/100

results <- NULL

results$props <- c(cong_props, comp_props, bl_props)
results$cond <- factor(c(rep(1, length(cong_props)),
						rep(2, length(comp_props)),
						rep(3, length(bl_props))))
						
results <- as.data.frame(results)

x <- aov(props~cond, data=results)
summary(x)

t.test(cong_props, comp_props, var.equal=T)
t.test(cong_props, bl_props, var.equal=T)
t.test(bl_props, comp_props, var.equal=T)


d.t.unpaired(2.6014, 37, 37)
d.t.unpaired(3.2182, 37, 36)
d.t.unpaired(0.65299, 37, 36)

###################################################################

#jpeg('graphs/choice_props')
par(mfrow=c(1,3))





#################


bl_block1 <- bl_data[bl_data$trial <= 20,]
bl_block2 <- bl_data[bl_data$trial > 20 & bl_data$trial <= 40,]
bl_block3 <- bl_data[bl_data$trial > 40 & bl_data$trial <= 60,]
bl_block4 <- bl_data[bl_data$trial > 60 & bl_data$trial <= 80,]
bl_block5 <- bl_data[bl_data$trial > 80 & bl_data$trial <= 100,]


bl_block1_choices <- tapply(bl_block1$reward, list(bl_block1$subj, bl_block1$reward), length)
bl_block1_choices[is.na(bl_block1_choices)] <- 0

bl_block2_choices <- tapply(bl_block2$reward, list(bl_block2$subj, bl_block2$reward), length)
bl_block2_choices[is.na(bl_block2_choices)] <- 0

bl_block3_choices <- tapply(bl_block3$reward, list(bl_block3$subj, bl_block3$reward), length)
bl_block3_choices[is.na(bl_block3_choices)] <- 0

bl_block4_choices <- tapply(bl_block4$reward, list(bl_block4$subj, bl_block4$reward), length)
bl_block4_choices[is.na(bl_block4_choices)] <- 0

bl_block5_choices <- tapply(bl_block5$reward, list(bl_block5$subj, bl_block5$reward), length)
bl_block5_choices[is.na(bl_block5_choices)] <- 0


# means
bl_line10_means <- c( mean(bl_block1_choices[,4]), 
             mean(bl_block2_choices[,4]),
             mean(bl_block3_choices[,4]),
             mean(bl_block4_choices[,4]),
             mean(bl_block5_choices[,4]))
             
bl_line3_means <- c( mean(bl_block1_choices[,3]), 
             mean(bl_block2_choices[,3]),
             mean(bl_block3_choices[,3]),
             mean(bl_block4_choices[,3]),
             mean(bl_block5_choices[,3]))
             
bl_line2_means <- c( mean(bl_block1_choices[,2]), 
             mean(bl_block2_choices[,2]),
             mean(bl_block3_choices[,2]),
             mean(bl_block4_choices[,2]),
             mean(bl_block5_choices[,2]))
             
bl_line1_means <- c( mean(bl_block1_choices[,1]), 
             mean(bl_block2_choices[,1]),
             mean(bl_block3_choices[,1]),
             mean(bl_block4_choices[,1]),
             mean(bl_block5_choices[,1]))

# standard deviations
bl_line10_sds <- c( sd(bl_block1_choices[,4]), 
             sd(bl_block2_choices[,4]),
             sd(bl_block3_choices[,4]),
             sd(bl_block4_choices[,4]),
             sd(bl_block5_choices[,4]))
             
bl_line3_sds <- c( sd(bl_block1_choices[,3]), 
             sd(bl_block2_choices[,3]),
             sd(bl_block3_choices[,3]),
             sd(bl_block4_choices[,3]),
             sd(bl_block5_choices[,3]))
             
bl_line2_sds <- c( sd(bl_block1_choices[,2]), 
             sd(bl_block2_choices[,2]),
             sd(bl_block3_choices[,2]),
             sd(bl_block4_choices[,2]),
             sd(bl_block5_choices[,2]))
             
bl_line1_sds <- c( sd(bl_block1_choices[,1]), 
             sd(bl_block2_choices[,1]),
             sd(bl_block3_choices[,1]),
             sd(bl_block4_choices[,1]),
             sd(bl_block5_choices[,1]))

# Ns
bl_line10_lengths <- c( length(bl_block1_choices[,4]), 
             length(bl_block2_choices[,4]),
             length(bl_block3_choices[,4]),
             length(bl_block4_choices[,4]),
             length(bl_block5_choices[,4]))
             
bl_line3_lengths <- c( length(bl_block1_choices[,3]), 
             length(bl_block2_choices[,3]),
             length(bl_block3_choices[,3]),
             length(bl_block4_choices[,3]),
             length(bl_block5_choices[,3]))
             
bl_line2_lengths <- c( length(bl_block1_choices[,2]), 
             length(bl_block2_choices[,2]),
             length(bl_block3_choices[,2]),
             length(bl_block4_choices[,2]),
             length(bl_block5_choices[,2]))
             
bl_line1_lengths <- c( length(bl_block1_choices[,1]), 
             length(bl_block2_choices[,1]),
             length(bl_block3_choices[,1]),
             length(bl_block4_choices[,1]),
             length(bl_block5_choices[,1]))
             
bl_line10_ses <- bl_line10_sds/sqrt(bl_line10_lengths)
bl_line3_ses <- bl_line3_sds/sqrt(bl_line3_lengths)
bl_line2_ses <- bl_line2_sds/sqrt(bl_line2_lengths)
bl_line1_ses <- bl_line1_sds/sqrt(bl_line1_lengths)



plot(bl_line10_means/20, type='l', lty=2, col = 'blue', 
	 ylim=c(0,1), ylab='Proportion of selections', 
	 xlab="Block (20 trials)", main='Baseline', cex.lab= 1.4, cex.axis=1.3, lwd=2)
lines(bl_line3_means/20, col = 'green', lwd=3)
lines(bl_line2_means/20, col = 'yellow', lwd=3)
lines(bl_line1_means/20, col = 'red', lwd=3)

legend(1, 1.0, c("10", "3", "2", "1"), 
	   col = c("blue", "green", "yellow", "red"), 
	   lty = c(2,1,1,1), cex = 1.2)

x <- seq(5)
segments(x, bl_line10_means/20+bl_line10_ses/20, x, bl_line10_means/20-bl_line10_ses/20, col='blue')
segments(x, bl_line3_means/20+bl_line3_ses/20, x, bl_line3_means/20-bl_line3_ses/20, col='green')
segments(x, bl_line2_means/20+bl_line2_ses/20, x, bl_line2_means/20-bl_line2_ses/20, col='yellow')
segments(x, bl_line1_means/20+bl_line1_ses/20, x, bl_line1_means/20-bl_line1_ses/20, col='red')

#dev.off()


#################


comp_block1 <- comp_data[comp_data$trial <= 20,]
comp_block2 <- comp_data[comp_data$trial > 20 & comp_data$trial <= 40,]
comp_block3 <- comp_data[comp_data$trial > 40 & comp_data$trial <= 60,]
comp_block4 <- comp_data[comp_data$trial > 60 & comp_data$trial <= 80,]
comp_block5 <- comp_data[comp_data$trial > 80 & comp_data$trial <= 100,]


comp_block1_choices <- tapply(comp_block1$reward, list(comp_block1$subj, comp_block1$reward), length)
comp_block1_choices[is.na(comp_block1_choices)] <- 0

comp_block2_choices <- tapply(comp_block2$reward, list(comp_block2$subj, comp_block2$reward), length)
comp_block2_choices[is.na(comp_block2_choices)] <- 0

comp_block3_choices <- tapply(comp_block3$reward, list(comp_block3$subj, comp_block3$reward), length)
comp_block3_choices[is.na(comp_block3_choices)] <- 0

comp_block4_choices <- tapply(comp_block4$reward, list(comp_block4$subj, comp_block4$reward), length)
comp_block4_choices[is.na(comp_block4_choices)] <- 0

comp_block5_choices <- tapply(comp_block5$reward, list(comp_block5$subj, comp_block5$reward), length)
comp_block5_choices[is.na(comp_block5_choices)] <- 0


# means
comp_line10_means <- c( mean(comp_block1_choices[,4]), 
             mean(comp_block2_choices[,4]),
             mean(comp_block3_choices[,4]),
             mean(comp_block4_choices[,4]),
             mean(comp_block5_choices[,4]))
             
comp_line3_means <- c( mean(comp_block1_choices[,3]), 
             mean(comp_block2_choices[,3]),
             mean(comp_block3_choices[,3]),
             mean(comp_block4_choices[,3]),
             mean(comp_block5_choices[,3]))
             
comp_line2_means <- c( mean(comp_block1_choices[,2]), 
             mean(comp_block2_choices[,2]),
             mean(comp_block3_choices[,2]),
             mean(comp_block4_choices[,2]),
             mean(comp_block5_choices[,2]))
             
comp_line1_means <- c( mean(comp_block1_choices[,1]), 
             mean(comp_block2_choices[,1]),
             mean(comp_block3_choices[,1]),
             mean(comp_block4_choices[,1]),
             mean(comp_block5_choices[,1]))

# standard deviations
comp_line10_sds <- c( sd(comp_block1_choices[,4]), 
             sd(comp_block2_choices[,4]),
             sd(comp_block3_choices[,4]),
             sd(comp_block4_choices[,4]),
             sd(comp_block5_choices[,4]))
             
comp_line3_sds <- c( sd(comp_block1_choices[,3]), 
             sd(comp_block2_choices[,3]),
             sd(comp_block3_choices[,3]),
             sd(comp_block4_choices[,3]),
             sd(comp_block5_choices[,3]))
             
comp_line2_sds <- c( sd(comp_block1_choices[,2]), 
             sd(comp_block2_choices[,2]),
             sd(comp_block3_choices[,2]),
             sd(comp_block4_choices[,2]),
             sd(comp_block5_choices[,2]))
             
comp_line1_sds <- c( sd(comp_block1_choices[,1]), 
             sd(comp_block2_choices[,1]),
             sd(comp_block3_choices[,1]),
             sd(comp_block4_choices[,1]),
             sd(comp_block5_choices[,1]))

# Ns
comp_line10_lengths <- c( length(comp_block1_choices[,4]), 
             length(comp_block2_choices[,4]),
             length(comp_block3_choices[,4]),
             length(comp_block4_choices[,4]),
             length(comp_block5_choices[,4]))
             
comp_line3_lengths <- c( length(comp_block1_choices[,3]), 
             length(comp_block2_choices[,3]),
             length(comp_block3_choices[,3]),
             length(comp_block4_choices[,3]),
             length(comp_block5_choices[,3]))
             
comp_line2_lengths <- c( length(comp_block1_choices[,2]), 
             length(comp_block2_choices[,2]),
             length(comp_block3_choices[,2]),
             length(comp_block4_choices[,2]),
             length(comp_block5_choices[,2]))
             
comp_line1_lengths <- c( length(comp_block1_choices[,1]), 
             length(comp_block2_choices[,1]),
             length(comp_block3_choices[,1]),
             length(comp_block4_choices[,1]),
             length(comp_block5_choices[,1]))
             
comp_line10_ses <- comp_line10_sds/sqrt(comp_line10_lengths)
comp_line3_ses <- comp_line3_sds/sqrt(comp_line3_lengths)
comp_line2_ses <- comp_line2_sds/sqrt(comp_line2_lengths)
comp_line1_ses <- comp_line1_sds/sqrt(comp_line1_lengths)



plot(comp_line10_means/20, type='l', lty=2, col = 'blue', 
	 ylim=c(0,1), ylab='Proportion of selections', 
	 xlab="Block (20 trials)", main='Competition', cex.lab= 1.4, cex.axis=1.3, lwd=2)
lines(comp_line3_means/20, col = 'green', lwd=3)
lines(comp_line2_means/20, col = 'yellow', lwd=3)
lines(comp_line1_means/20, col = 'red', lwd=3)

# legend(1, 1.0, c("10", "3", "2", "1"), 
	   # col = c("blue", "green", "yellow", "red"), 
	   # lty = c(2,1,1,1), cex = 1.2)

x <- seq(5)
segments(x, comp_line10_means/20+comp_line10_ses/20, x, comp_line10_means/20-comp_line10_ses/20, col='blue')
segments(x, comp_line3_means/20+comp_line3_ses/20, x, comp_line3_means/20-comp_line3_ses/20, col='green')
segments(x, comp_line2_means/20+comp_line2_ses/20, x, comp_line2_means/20-comp_line2_ses/20, col='yellow')
segments(x, comp_line1_means/20+comp_line1_ses/20, x, comp_line1_means/20-comp_line1_ses/20, col='red')


#dev.off()


##################

cong_block1 <- cong_data[cong_data$trial <= 20,]
cong_block2 <- cong_data[cong_data$trial > 20 & cong_data$trial <= 40,]
cong_block3 <- cong_data[cong_data$trial > 40 & cong_data$trial <= 60,]
cong_block4 <- cong_data[cong_data$trial > 60 & cong_data$trial <= 80,]
cong_block5 <- cong_data[cong_data$trial > 80 & cong_data$trial <= 100,]


cong_block1_choices <- tapply(cong_block1$reward, list(cong_block1$subj, cong_block1$reward), length)
cong_block1_choices[is.na(cong_block1_choices)] <- 0

cong_block2_choices <- tapply(cong_block2$reward, list(cong_block2$subj, cong_block2$reward), length)
cong_block2_choices[is.na(cong_block2_choices)] <- 0

cong_block3_choices <- tapply(cong_block3$reward, list(cong_block3$subj, cong_block3$reward), length)
cong_block3_choices[is.na(cong_block3_choices)] <- 0

cong_block4_choices <- tapply(cong_block4$reward, list(cong_block4$subj, cong_block4$reward), length)
cong_block4_choices[is.na(cong_block4_choices)] <- 0

cong_block5_choices <- tapply(cong_block5$reward, list(cong_block5$subj, cong_block5$reward), length)
cong_block5_choices[is.na(cong_block5_choices)] <- 0


# means
cong_line10_means <- c( mean(cong_block1_choices[,4]), 
             mean(cong_block2_choices[,4]),
             mean(cong_block3_choices[,4]),
             mean(cong_block4_choices[,4]),
             mean(cong_block5_choices[,4]))
             
cong_line3_means <- c( mean(cong_block1_choices[,3]), 
             mean(cong_block2_choices[,3]),
             mean(cong_block3_choices[,3]),
             mean(cong_block4_choices[,3]),
             mean(cong_block5_choices[,3]))
             
cong_line2_means <- c( mean(cong_block1_choices[,2]), 
             mean(cong_block2_choices[,2]),
             mean(cong_block3_choices[,2]),
             mean(cong_block4_choices[,2]),
             mean(cong_block5_choices[,2]))
             
cong_line1_means <- c( mean(cong_block1_choices[,1]), 
             mean(cong_block2_choices[,1]),
             mean(cong_block3_choices[,1]),
             mean(cong_block4_choices[,1]),
             mean(cong_block5_choices[,1]))

# standard deviations
cong_line10_sds <- c( sd(cong_block1_choices[,4]), 
             sd(cong_block2_choices[,4]),
             sd(cong_block3_choices[,4]),
             sd(cong_block4_choices[,4]),
             sd(cong_block5_choices[,4]))
             
cong_line3_sds <- c( sd(cong_block1_choices[,3]), 
             sd(cong_block2_choices[,3]),
             sd(cong_block3_choices[,3]),
             sd(cong_block4_choices[,3]),
             sd(cong_block5_choices[,3]))
             
cong_line2_sds <- c( sd(cong_block1_choices[,2]), 
             sd(cong_block2_choices[,2]),
             sd(cong_block3_choices[,2]),
             sd(cong_block4_choices[,2]),
             sd(cong_block5_choices[,2]))
             
cong_line1_sds <- c( sd(cong_block1_choices[,1]), 
             sd(cong_block2_choices[,1]),
             sd(cong_block3_choices[,1]),
             sd(cong_block4_choices[,1]),
             sd(cong_block5_choices[,1]))

# Ns
cong_line10_lengths <- c( length(cong_block1_choices[,4]), 
             length(cong_block2_choices[,4]),
             length(cong_block3_choices[,4]),
             length(cong_block4_choices[,4]),
             length(cong_block5_choices[,4]))
             
cong_line3_lengths <- c( length(cong_block1_choices[,3]), 
             length(cong_block2_choices[,3]),
             length(cong_block3_choices[,3]),
             length(cong_block4_choices[,3]),
             length(cong_block5_choices[,3]))
             
cong_line2_lengths <- c( length(cong_block1_choices[,2]), 
             length(cong_block2_choices[,2]),
             length(cong_block3_choices[,2]),
             length(cong_block4_choices[,2]),
             length(cong_block5_choices[,2]))
             
cong_line1_lengths <- c( length(cong_block1_choices[,1]), 
             length(cong_block2_choices[,1]),
             length(cong_block3_choices[,1]),
             length(cong_block4_choices[,1]),
             length(cong_block5_choices[,1]))
             
cong_line10_ses <- cong_line10_sds/sqrt(cong_line10_lengths)
cong_line3_ses <- cong_line3_sds/sqrt(cong_line3_lengths)
cong_line2_ses <- cong_line2_sds/sqrt(cong_line2_lengths)
cong_line1_ses <- cong_line1_sds/sqrt(cong_line1_lengths)



plot(cong_line10_means/20, type='l', lty= 2, col = 'blue', 
	 ylim=c(0,1), ylab='Proportion of selections', 
	 xlab="Block (20 trials)", main='Congruent', cex.lab= 1.4, cex.axis=1.3, lwd=2)
lines(cong_line3_means/20, col = 'green', lwd=3)
lines(cong_line2_means/20, col = 'yellow', lwd=3)
lines(cong_line1_means/20, col = 'red', lwd=3)

# legend(1, 1.0, c("10", "3", "2", "1"), 
	   # col = c("blue", "green", "yellow", "red"), 
	   # lty = c(2,1,1,1), cex = 1.2)

x <- seq(5)
segments(x, cong_line10_means/20+cong_line10_ses/20, x, cong_line10_means/20-cong_line10_ses/20, col='blue')
segments(x, cong_line3_means/20+cong_line3_ses/20, x, cong_line3_means/20-cong_line3_ses/20, col='green')
segments(x, cong_line2_means/20+cong_line2_ses/20, x, cong_line2_means/20-cong_line2_ses/20, col='yellow')
segments(x, cong_line1_means/20+cong_line1_ses/20, x, cong_line1_means/20-cong_line1_ses/20, col='red')



################################################################
 subjects <- unique(cong_data$subj) 
 
 graphs_per_row = 8
 
 par(mfrow=c( ceiling(length(subjects) / graphs_per_row), graphs_per_row))
 #par(mfrow=c(3,5))
 
 for (i in subjects) {
		graphdata <- cong_data[cong_data$subj == i,]
 
 
 		block1 <- graphdata[graphdata$trial <= 20,]
 		block2 <- graphdata[graphdata$trial > 20 & graphdata$trial <= 40,]
 		block3 <- graphdata[graphdata$trial > 40 & graphdata$trial <= 60,]
 		block4 <- graphdata[graphdata$trial > 60 & graphdata$trial <= 80,]
 		block5 <- graphdata[graphdata$trial > 80 & graphdata$trial <= 100,]
 
         block1_choices <- c(0,0,0,0)
         block2_choices <- c(0,0,0,0)
         block3_choices <- c(0,0,0,0)
         block4_choices <- c(0,0,0,0)
         block5_choices <- c(0,0,0,0)
 
 
 		block1_choices[1] <- length(block1[block1$reward == 1,]$reward)
 		block1_choices[2] <- length(block1[block1$reward == 2,]$reward)
 		block1_choices[3] <- length(block1[block1$reward == 3,]$reward)
 		block1_choices[4] <- length(block1[block1$reward == 10,]$reward)
 		block1_choices[is.na(block1_choices)] <- 0
 		
 		block2_choices[1] <- length(block2[block2$reward == 1,]$reward)
 		block2_choices[2] <- length(block2[block2$reward == 2,]$reward)
 		block2_choices[3] <- length(block2[block2$reward == 3,]$reward)
 		block2_choices[4] <- length(block2[block2$reward == 10,]$reward)
 		block2_choices[is.na(block2_choices)] <- 0
 		
 		
 		block3_choices[1] <- length(block3[block3$reward == 1,]$reward)
 		block3_choices[2] <- length(block3[block3$reward == 2,]$reward)
 		block3_choices[3] <- length(block3[block3$reward == 3,]$reward)
 		block3_choices[4] <- length(block3[block3$reward == 10,]$reward)
 		block3_choices[is.na(block3_choices)] <- 0
 		
 		
 		block4_choices[1] <- length(block4[block4$reward == 1,]$reward)
 		block4_choices[2] <- length(block4[block4$reward == 2,]$reward)
 		block4_choices[3] <- length(block4[block4$reward == 3,]$reward)
 		block4_choices[4] <- length(block4[block4$reward == 10,]$reward)
 		block4_choices[is.na(block4_choices)] <- 0
 		
 		
 		block5_choices[1] <- length(block5[block5$reward == 1,]$reward)
 		block5_choices[2] <- length(block5[block5$reward == 2,]$reward)
 		block5_choices[3] <- length(block5[block5$reward == 3,]$reward)
 		block5_choices[4] <- length(block5[block5$reward == 10,]$reward)
 		block5_choices[is.na(block5_choices)] <- 0
 
 
 
 
 		# means
 		line10 <- c( block1_choices[4]/20, 
 					 block2_choices[4]/20,
 					 block3_choices[4]/20,
 					 block4_choices[4]/20,
 					 block5_choices[4]/20)
 	 
 		line3 <- c( block1_choices[3]/20, 
 					 block2_choices[3]/20,
 					 block3_choices[3]/20,
 					 block4_choices[3]/20,
 					 block5_choices[3]/20)
 	 
 		line2 <- c( block1_choices[2]/20, 
 					 block2_choices[2]/20,
 					 block3_choices[2]/20,
 					 block4_choices[2]/20,
 					 block5_choices[2]/20)
 	 
 		line1 <- c( block1_choices[1]/20, 
 					 block2_choices[1]/20,
 					 block3_choices[1]/20,
 					 block4_choices[1]/20,
 					 block5_choices[1]/20)
 
 		plot(line10, type='l', col = 'blue', ylim=c(0,1), ylab='',
 		xlab="Block", mar = c(0.1,0.1,0.1,0.1), main=toString(i))
 		lines(line3, col = 'green')
 		lines(line2, col = 'yellow')
 		lines(line1, col = 'red')
 
 
 	   }
 	   
 	   
 	   
################################################################
 subjects <- unique(comp_data$subj) 
 
 graphs_per_row = 8
 
 par(mfrow=c( ceiling(length(subjects) / graphs_per_row), graphs_per_row))
 #par(mfrow=c(3,5))
 
 for (i in subjects) {
		graphdata <- comp_data[comp_data$subj == i,]
 
 
 		block1 <- graphdata[graphdata$trial <= 20,]
 		block2 <- graphdata[graphdata$trial > 20 & graphdata$trial <= 40,]
 		block3 <- graphdata[graphdata$trial > 40 & graphdata$trial <= 60,]
 		block4 <- graphdata[graphdata$trial > 60 & graphdata$trial <= 80,]
 		block5 <- graphdata[graphdata$trial > 80 & graphdata$trial <= 100,]
 
         block1_choices <- c(0,0,0,0)
         block2_choices <- c(0,0,0,0)
         block3_choices <- c(0,0,0,0)
         block4_choices <- c(0,0,0,0)
         block5_choices <- c(0,0,0,0)
 
 
 		block1_choices[1] <- length(block1[block1$reward == 1,]$reward)
 		block1_choices[2] <- length(block1[block1$reward == 2,]$reward)
 		block1_choices[3] <- length(block1[block1$reward == 3,]$reward)
 		block1_choices[4] <- length(block1[block1$reward == 10,]$reward)
 		block1_choices[is.na(block1_choices)] <- 0
 		
 		block2_choices[1] <- length(block2[block2$reward == 1,]$reward)
 		block2_choices[2] <- length(block2[block2$reward == 2,]$reward)
 		block2_choices[3] <- length(block2[block2$reward == 3,]$reward)
 		block2_choices[4] <- length(block2[block2$reward == 10,]$reward)
 		block2_choices[is.na(block2_choices)] <- 0
 		
 		
 		block3_choices[1] <- length(block3[block3$reward == 1,]$reward)
 		block3_choices[2] <- length(block3[block3$reward == 2,]$reward)
 		block3_choices[3] <- length(block3[block3$reward == 3,]$reward)
 		block3_choices[4] <- length(block3[block3$reward == 10,]$reward)
 		block3_choices[is.na(block3_choices)] <- 0
 		
 		
 		block4_choices[1] <- length(block4[block4$reward == 1,]$reward)
 		block4_choices[2] <- length(block4[block4$reward == 2,]$reward)
 		block4_choices[3] <- length(block4[block4$reward == 3,]$reward)
 		block4_choices[4] <- length(block4[block4$reward == 10,]$reward)
 		block4_choices[is.na(block4_choices)] <- 0
 		
 		
 		block5_choices[1] <- length(block5[block5$reward == 1,]$reward)
 		block5_choices[2] <- length(block5[block5$reward == 2,]$reward)
 		block5_choices[3] <- length(block5[block5$reward == 3,]$reward)
 		block5_choices[4] <- length(block5[block5$reward == 10,]$reward)
 		block5_choices[is.na(block5_choices)] <- 0
 
 
 
 
 		# means
 		line10 <- c( block1_choices[4]/20, 
 					 block2_choices[4]/20,
 					 block3_choices[4]/20,
 					 block4_choices[4]/20,
 					 block5_choices[4]/20)
 	 
 		line3 <- c( block1_choices[3]/20, 
 					 block2_choices[3]/20,
 					 block3_choices[3]/20,
 					 block4_choices[3]/20,
 					 block5_choices[3]/20)
 	 
 		line2 <- c( block1_choices[2]/20, 
 					 block2_choices[2]/20,
 					 block3_choices[2]/20,
 					 block4_choices[2]/20,
 					 block5_choices[2]/20)
 	 
 		line1 <- c( block1_choices[1]/20, 
 					 block2_choices[1]/20,
 					 block3_choices[1]/20,
 					 block4_choices[1]/20,
 					 block5_choices[1]/20)
 
 		plot(line10, type='l', col = 'blue', ylim=c(0,1), ylab='',
 		xlab="Block", mar = c(0.1,0.1,0.1,0.1), main=toString(i))
 		lines(line3, col = 'green')
 		lines(line2, col = 'yellow')
 		lines(line1, col = 'red')
 
 
 	   }

###############################################################

subjects <- unique(bl_data$subj) 
 
 graphs_per_row = 8
 
 par(mfrow=c( ceiling(length(subjects) / graphs_per_row), graphs_per_row))
 #par(mfrow=c(3,5))
 
 for (i in subjects) {
		graphdata <- bl_data[bl_data$subj == i,]
 
 
 		block1 <- graphdata[graphdata$trial <= 20,]
 		block2 <- graphdata[graphdata$trial > 20 & graphdata$trial <= 40,]
 		block3 <- graphdata[graphdata$trial > 40 & graphdata$trial <= 60,]
 		block4 <- graphdata[graphdata$trial > 60 & graphdata$trial <= 80,]
 		block5 <- graphdata[graphdata$trial > 80 & graphdata$trial <= 100,]
 
         block1_choices <- c(0,0,0,0)
         block2_choices <- c(0,0,0,0)
         block3_choices <- c(0,0,0,0)
         block4_choices <- c(0,0,0,0)
         block5_choices <- c(0,0,0,0)
 
 
 		block1_choices[1] <- length(block1[block1$reward == 1,]$reward)
 		block1_choices[2] <- length(block1[block1$reward == 2,]$reward)
 		block1_choices[3] <- length(block1[block1$reward == 3,]$reward)
 		block1_choices[4] <- length(block1[block1$reward == 10,]$reward)
 		block1_choices[is.na(block1_choices)] <- 0
 		
 		block2_choices[1] <- length(block2[block2$reward == 1,]$reward)
 		block2_choices[2] <- length(block2[block2$reward == 2,]$reward)
 		block2_choices[3] <- length(block2[block2$reward == 3,]$reward)
 		block2_choices[4] <- length(block2[block2$reward == 10,]$reward)
 		block2_choices[is.na(block2_choices)] <- 0
 		
 		
 		block3_choices[1] <- length(block3[block3$reward == 1,]$reward)
 		block3_choices[2] <- length(block3[block3$reward == 2,]$reward)
 		block3_choices[3] <- length(block3[block3$reward == 3,]$reward)
 		block3_choices[4] <- length(block3[block3$reward == 10,]$reward)
 		block3_choices[is.na(block3_choices)] <- 0
 		
 		
 		block4_choices[1] <- length(block4[block4$reward == 1,]$reward)
 		block4_choices[2] <- length(block4[block4$reward == 2,]$reward)
 		block4_choices[3] <- length(block4[block4$reward == 3,]$reward)
 		block4_choices[4] <- length(block4[block4$reward == 10,]$reward)
 		block4_choices[is.na(block4_choices)] <- 0
 		
 		
 		block5_choices[1] <- length(block5[block5$reward == 1,]$reward)
 		block5_choices[2] <- length(block5[block5$reward == 2,]$reward)
 		block5_choices[3] <- length(block5[block5$reward == 3,]$reward)
 		block5_choices[4] <- length(block5[block5$reward == 10,]$reward)
 		block5_choices[is.na(block5_choices)] <- 0
 
 
 
 
 		# means
 		line10 <- c( block1_choices[4]/20, 
 					 block2_choices[4]/20,
 					 block3_choices[4]/20,
 					 block4_choices[4]/20,
 					 block5_choices[4]/20)
 	 
 		line3 <- c( block1_choices[3]/20, 
 					 block2_choices[3]/20,
 					 block3_choices[3]/20,
 					 block4_choices[3]/20,
 					 block5_choices[3]/20)
 	 
 		line2 <- c( block1_choices[2]/20, 
 					 block2_choices[2]/20,
 					 block3_choices[2]/20,
 					 block4_choices[2]/20,
 					 block5_choices[2]/20)
 	 
 		line1 <- c( block1_choices[1]/20, 
 					 block2_choices[1]/20,
 					 block3_choices[1]/20,
 					 block4_choices[1]/20,
 					 block5_choices[1]/20)
 
 		plot(line10, type='l', col = 'blue', ylim=c(0,1), ylab='',
 		xlab="Block", mar = c(0.1,0.1,0.1,0.1), main=toString(i))
 		lines(line3, col = 'green')
 		lines(line2, col = 'yellow')
 		lines(line1, col = 'red')
 
 
 	   }

