



cong_adult_data <- read.table('congruent_adults.txt', header=F)
comp_adult_data <- read.table('competition_adults.txt', header=F)
bl_adult_data <- read.table('baseline_adults.txt', header=F)

names(cong_adult_data) <- c('subj', 'phase', 'trial', 'cond', 'res', 'rt', 'reward', 'total', 'image', 'r1', 'r2', 'r3', 'r4')
names(comp_adult_data) <- c('subj', 'phase', 'trial', 'cond', 'res', 'rt', 'reward', 'total', 'image', 'r1', 'r2', 'r3', 'r4')
names(bl_adult_data) <- c('subj', 'phase', 'trial', 'cond', 'res', 'rt', 'reward', 'total', 'image', 'r1', 'r2', 'r3', 'r4')

length(unique(bl_adult_data$subj))
length(unique(comp_adult_data$subj))
length(unique(cong_adult_data$subj))

#############################################################################
# 	SWITCH ANALYSES

cong_adult_data$stay <- NULL
for (i in 1:length(cong_adult_data[,1])) {
    cong_adult_data$stay[i] <- as.numeric(cong_adult_data[i,]$res == cong_adult_data[i-1,]$res)
    
    if (cong_adult_data[i,]$trial == 1) { cong_adult_data$stay[i] <- 0 }
    
    }
    
comp_adult_data$stay <- NULL
for (i in 1:length(comp_adult_data[,1])) {
    comp_adult_data$stay[i] <- as.numeric(comp_adult_data[i,]$res == comp_adult_data[i-1,]$res)
    
    if (comp_adult_data[i,]$trial == 1) { comp_adult_data$stay[i] <- 0 }
    
    }
    
bl_adult_data$stay <- NULL
for (i in 1:length(bl_adult_data[,1])) {
    bl_adult_data$stay[i] <- as.numeric(bl_adult_data[i,]$res == bl_adult_data[i-1,]$res)
    
    if (bl_adult_data[i,]$trial == 1) { bl_adult_data$stay[i] <- 0 }
    
    }
       

      
cong_adult_stay_total <- tapply(cong_adult_data$stay, cong_adult_data$subj, sum)
for (i in 1:length(cong_adult_stay_total)) {
	x <- binom.test(cong_adult_stay_total[i], 100, 0.25)
	print(x)
	
}

comp_adult_stay_total <- tapply(comp_adult_data$stay, comp_adult_data$subj, sum)
for (i in 1:length(comp_adult_stay_total)) {
	x <- binom.test(comp_adult_stay_total[i], 100, 0.25)
	print(x)
	
}

bl_adult_stay_total <- tapply(bl_adult_data$stay, bl_adult_data$subj, sum)
for (i in 1:length(bl_adult_stay_total)) {
	x <- binom.test(bl_adult_stay_total[i], 100, 0.25)
	print(x)
	
}



t.test(cong_adult_stay_total, comp_adult_stay_total, var.equal=T)
t.test(bl_adult_stay_total, comp_adult_stay_total, var.equal=T)
t.test(cong_adult_stay_total, bl_adult_stay_total, var.equal=T)


cong_adult_stay_props <- tapply(cong_adult_data$stay, cong_adult_data$subj, mean)
comp_adult_stay_props <- tapply(comp_adult_data$stay, comp_adult_data$subj, mean)
bl_adult_stay_props <- tapply(bl_adult_data$stay, bl_adult_data$subj, mean)
#jpeg('graphs/adult_switch_hists.jpg')
par(mfrow=c(1,3))
hist(cong_adult_stay_props, main= "Congruent", xlab="Proportion of repeat responses", col='grey', xlim=c(0,1))
hist(comp_adult_stay_props, main= "Competition", xlab="Proportion of repeat responses", col='grey', xlim=c(0,1))
hist(bl_adult_stay_props, main= "Baseline", xlab="Proportion of repeat responses", col='grey', xlim=c(0,1))
#dev.off()

###########################

graph_adult_data <- NULL
graph_adult_data$switches <- c(1-(cong_adult_stay_total)/100, 1-(comp_adult_stay_total)/100, 1-(bl_adult_stay_total)/100)
graph_adult_data$group <- factor(c(rep("Congruent", length(cong_adult_stay_total)),
							 rep("Competition", length(comp_adult_stay_total)),
							 rep("Baseline", length(bl_adult_stay_total))))
graph_adult_data <- as.data.frame(graph_adult_data)

#jpeg('graphs/switch_box.jpg')
boxplot(switches ~ group, data=graph_adult_data,
 ylab = 'Proportion of switch responses', col='white', ylim=c(0,1),
 main = 'Response Switching', names.arg=c('Congruent', 'Competition', 'Baseline'))
stripchart(switches ~ group, data=graph_adult_data, vertical = TRUE,  
    method = "jitter", add = TRUE, pch = 19, col = c('blue', 'red', 'green'))


abline(0.83, 0, col='red')
abline(0.65, 0, col='red')

x <- aov( switches ~ group, data=graph_adult_data)
summary(x)
#dev.off()

#> x <- aov( switches ~ group, data=graph_adult_data)
#> summary(x)
#            Df Sum Sq Mean Sq F value Pr(>F)
#group        2 0.0523 0.02616   1.628  0.205
#Residuals   63 1.0125 0.01607          


t.test(graph_adult_data[graph_adult_data$group=='Congruent',]$switches, graph_adult_data[graph_adult_data$group=='Competition',]$switches,var.equal=T)
t.test(graph_adult_data[graph_adult_data$group=='Baseline',]$switches, graph_adult_data[graph_adult_data$group=='Competition',]$switches,var.equal=T)
t.test(graph_adult_data[graph_adult_data$group=='Congruent',]$switches, graph_adult_data[graph_adult_data$group=='Baseline',]$switches,var.equal=T)

d.t.unpaired(3.035, 37, 37)
d.t.unpaired(3.2223, 37, 36)
d.t.unpaired(5.5738, 37, 36)

#################################################################

cong_adult_choices <- tapply(cong_adult_data$reward, list(cong_adult_data$subj, cong_adult_data$reward), length)
cong_adult_choices[is.na(cong_adult_choices)] <- 0

comp_adult_choices <- tapply(comp_adult_data$reward, list(comp_adult_data$subj, comp_adult_data$reward), length)
comp_adult_choices[is.na(comp_adult_choices)] <- 0

bl_adult_choices <- tapply(bl_adult_data$reward, list(bl_adult_data$subj, bl_adult_data$reward), length)
bl_adult_choices[is.na(bl_adult_choices)] <- 0

cong_adult_props <- cong_adult_choices[,4]/100
comp_adult_props <- comp_adult_choices[,4]/100
bl_adult_props <- bl_adult_choices[,4]/100




results <- NULL

results$props <- c(cong_adult_props, comp_adult_props, bl_adult_props)
results$cond <- factor(c(rep(1, length(cong_adult_props)),
						rep(2, length(comp_adult_props)),
						rep(3, length(bl_adult_props))))
						
results <- as.data.frame(results)

x <- aov(props~cond, data=results)
summary(x)



t.test(cong_adult_props, comp_adult_props, var.equal=T)
t.test(cong_adult_props, bl_adult_props, var.equal=T)
t.test(bl_adult_props, comp_adult_props, var.equal=T)
# comp was lower than the others

d.t.unpaired<-function(t.val,n1,n2){
  d<-t.val*sqrt((n1+n2)/(n1*n2))
  names(d)<-"effect size d"
  return(d)
}

d.t.unpaired(3.93, 37, 37)

d.t.unpaired(0.57193, 36, 37)

####################################################################

cong_adult_props <- cong_adult_choices[,1]/100
comp_adult_props <- comp_adult_choices[,1]/100
bl_adult_props <- bl_adult_choices[,1]/100

results <- NULL

results$props <- c(cong_adult_props, comp_adult_props, bl_adult_props)
results$cond <- factor(c(rep(1, length(cong_adult_props)),
						rep(2, length(comp_adult_props)),
						rep(3, length(bl_adult_props))))
						
results <- as.data.frame(results)

x <- aov(props~cond, data=results)
summary(x)

#> x <- aov(props~cond, data=results)
#> summary(x)
#            Df Sum Sq Mean Sq F value  Pr(>F)   
#cond         2 0.5222 0.26110   7.685 0.00103 **
#Residuals   63 2.1405 0.03398                   
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

t.test(cong_adult_props, comp_adult_props, var.equal=T)
t.test(cong_adult_props, bl_adult_props, var.equal=T)
t.test(bl_adult_props, comp_adult_props, var.equal=T)
# comp is higher than the others

d.t.unpaired(2.6014, 37, 37)
d.t.unpaired(3.2182, 37, 36)
d.t.unpaired(0.65299, 37, 36)

###################################################################

#jpeg('graphs/choice_props')
par(mfrow=c(1,3))





#################


bl_adult_block1 <- bl_adult_data[bl_adult_data$trial <= 20,]
bl_adult_block2 <- bl_adult_data[bl_adult_data$trial > 20 & bl_adult_data$trial <= 40,]
bl_adult_block3 <- bl_adult_data[bl_adult_data$trial > 40 & bl_adult_data$trial <= 60,]
bl_adult_block4 <- bl_adult_data[bl_adult_data$trial > 60 & bl_adult_data$trial <= 80,]
bl_adult_block5 <- bl_adult_data[bl_adult_data$trial > 80 & bl_adult_data$trial <= 100,]


bl_adult_block1_choices <- tapply(bl_adult_block1$reward, list(bl_adult_block1$subj, bl_adult_block1$reward), length)
bl_adult_block1_choices[is.na(bl_adult_block1_choices)] <- 0

bl_adult_block2_choices <- tapply(bl_adult_block2$reward, list(bl_adult_block2$subj, bl_adult_block2$reward), length)
bl_adult_block2_choices[is.na(bl_adult_block2_choices)] <- 0

bl_adult_block3_choices <- tapply(bl_adult_block3$reward, list(bl_adult_block3$subj, bl_adult_block3$reward), length)
bl_adult_block3_choices[is.na(bl_adult_block3_choices)] <- 0

bl_adult_block4_choices <- tapply(bl_adult_block4$reward, list(bl_adult_block4$subj, bl_adult_block4$reward), length)
bl_adult_block4_choices[is.na(bl_adult_block4_choices)] <- 0

bl_adult_block5_choices <- tapply(bl_adult_block5$reward, list(bl_adult_block5$subj, bl_adult_block5$reward), length)
bl_adult_block5_choices[is.na(bl_adult_block5_choices)] <- 0


# means
bl_adult_line10_means <- c( mean(bl_adult_block1_choices[,4]), 
             mean(bl_adult_block2_choices[,4]),
             mean(bl_adult_block3_choices[,4]),
             mean(bl_adult_block4_choices[,4]),
             mean(bl_adult_block5_choices[,4]))
             
bl_adult_line3_means <- c( mean(bl_adult_block1_choices[,3]), 
             mean(bl_adult_block2_choices[,3]),
             mean(bl_adult_block3_choices[,3]),
             mean(bl_adult_block4_choices[,3]),
             mean(bl_adult_block5_choices[,3]))
             
bl_adult_line2_means <- c( mean(bl_adult_block1_choices[,2]), 
             mean(bl_adult_block2_choices[,2]),
             mean(bl_adult_block3_choices[,2]),
             mean(bl_adult_block4_choices[,2]),
             mean(bl_adult_block5_choices[,2]))
             
bl_adult_line1_means <- c( mean(bl_adult_block1_choices[,1]), 
             mean(bl_adult_block2_choices[,1]),
             mean(bl_adult_block3_choices[,1]),
             mean(bl_adult_block4_choices[,1]),
             mean(bl_adult_block5_choices[,1]))

# standard deviations
bl_adult_line10_sds <- c( sd(bl_adult_block1_choices[,4]), 
             sd(bl_adult_block2_choices[,4]),
             sd(bl_adult_block3_choices[,4]),
             sd(bl_adult_block4_choices[,4]),
             sd(bl_adult_block5_choices[,4]))
             
bl_adult_line3_sds <- c( sd(bl_adult_block1_choices[,3]), 
             sd(bl_adult_block2_choices[,3]),
             sd(bl_adult_block3_choices[,3]),
             sd(bl_adult_block4_choices[,3]),
             sd(bl_adult_block5_choices[,3]))
             
bl_adult_line2_sds <- c( sd(bl_adult_block1_choices[,2]), 
             sd(bl_adult_block2_choices[,2]),
             sd(bl_adult_block3_choices[,2]),
             sd(bl_adult_block4_choices[,2]),
             sd(bl_adult_block5_choices[,2]))
             
bl_adult_line1_sds <- c( sd(bl_adult_block1_choices[,1]), 
             sd(bl_adult_block2_choices[,1]),
             sd(bl_adult_block3_choices[,1]),
             sd(bl_adult_block4_choices[,1]),
             sd(bl_adult_block5_choices[,1]))

# Ns
bl_adult_line10_lengths <- c( length(bl_adult_block1_choices[,4]), 
             length(bl_adult_block2_choices[,4]),
             length(bl_adult_block3_choices[,4]),
             length(bl_adult_block4_choices[,4]),
             length(bl_adult_block5_choices[,4]))
             
bl_adult_line3_lengths <- c( length(bl_adult_block1_choices[,3]), 
             length(bl_adult_block2_choices[,3]),
             length(bl_adult_block3_choices[,3]),
             length(bl_adult_block4_choices[,3]),
             length(bl_adult_block5_choices[,3]))
             
bl_adult_line2_lengths <- c( length(bl_adult_block1_choices[,2]), 
             length(bl_adult_block2_choices[,2]),
             length(bl_adult_block3_choices[,2]),
             length(bl_adult_block4_choices[,2]),
             length(bl_adult_block5_choices[,2]))
             
bl_adult_line1_lengths <- c( length(bl_adult_block1_choices[,1]), 
             length(bl_adult_block2_choices[,1]),
             length(bl_adult_block3_choices[,1]),
             length(bl_adult_block4_choices[,1]),
             length(bl_adult_block5_choices[,1]))
             
bl_adult_line10_ses <- bl_adult_line10_sds/sqrt(bl_adult_line10_lengths)
bl_adult_line3_ses <- bl_adult_line3_sds/sqrt(bl_adult_line3_lengths)
bl_adult_line2_ses <- bl_adult_line2_sds/sqrt(bl_adult_line2_lengths)
bl_adult_line1_ses <- bl_adult_line1_sds/sqrt(bl_adult_line1_lengths)



plot(bl_adult_line10_means/20, type='l', lty=2, col = 'blue', 
	 ylim=c(0,1), ylab='Proportion of selections', 
	 xlab="Block (20 trials)", main='Baseline', cex.lab= 1.4, cex.axis=1.3, lwd=2)
lines(bl_adult_line3_means/20, col = 'green', lwd=3)
lines(bl_adult_line2_means/20, col = 'yellow', lwd=3)
lines(bl_adult_line1_means/20, col = 'red', lwd=3)

# legend(1, 1.0, c("10", "3", "2", "1"), 
	   # col = c("blue", "green", "yellow", "red"), 
	   # lty = c(2,1,1,1), cex = 1.2)

x <- seq(5)
segments(x, bl_adult_line10_means/20+bl_adult_line10_ses/20, x, bl_adult_line10_means/20-bl_adult_line10_ses/20, col='blue')
segments(x, bl_adult_line3_means/20+bl_adult_line3_ses/20, x, bl_adult_line3_means/20-bl_adult_line3_ses/20, col='green')
segments(x, bl_adult_line2_means/20+bl_adult_line2_ses/20, x, bl_adult_line2_means/20-bl_adult_line2_ses/20, col='yellow')
segments(x, bl_adult_line1_means/20+bl_adult_line1_ses/20, x, bl_adult_line1_means/20-bl_adult_line1_ses/20, col='red')

#dev.off()


#################


comp_adult_block1 <- comp_adult_data[comp_adult_data$trial <= 20,]
comp_adult_block2 <- comp_adult_data[comp_adult_data$trial > 20 & comp_adult_data$trial <= 40,]
comp_adult_block3 <- comp_adult_data[comp_adult_data$trial > 40 & comp_adult_data$trial <= 60,]
comp_adult_block4 <- comp_adult_data[comp_adult_data$trial > 60 & comp_adult_data$trial <= 80,]
comp_adult_block5 <- comp_adult_data[comp_adult_data$trial > 80 & comp_adult_data$trial <= 100,]


comp_adult_block1_choices <- tapply(comp_adult_block1$reward, list(comp_adult_block1$subj, comp_adult_block1$reward), length)
comp_adult_block1_choices[is.na(comp_adult_block1_choices)] <- 0

comp_adult_block2_choices <- tapply(comp_adult_block2$reward, list(comp_adult_block2$subj, comp_adult_block2$reward), length)
comp_adult_block2_choices[is.na(comp_adult_block2_choices)] <- 0

comp_adult_block3_choices <- tapply(comp_adult_block3$reward, list(comp_adult_block3$subj, comp_adult_block3$reward), length)
comp_adult_block3_choices[is.na(comp_adult_block3_choices)] <- 0

comp_adult_block4_choices <- tapply(comp_adult_block4$reward, list(comp_adult_block4$subj, comp_adult_block4$reward), length)
comp_adult_block4_choices[is.na(comp_adult_block4_choices)] <- 0

comp_adult_block5_choices <- tapply(comp_adult_block5$reward, list(comp_adult_block5$subj, comp_adult_block5$reward), length)
comp_adult_block5_choices[is.na(comp_adult_block5_choices)] <- 0


# means
comp_adult_line10_means <- c( mean(comp_adult_block1_choices[,4]), 
             mean(comp_adult_block2_choices[,4]),
             mean(comp_adult_block3_choices[,4]),
             mean(comp_adult_block4_choices[,4]),
             mean(comp_adult_block5_choices[,4]))
             
comp_adult_line3_means <- c( mean(comp_adult_block1_choices[,3]), 
             mean(comp_adult_block2_choices[,3]),
             mean(comp_adult_block3_choices[,3]),
             mean(comp_adult_block4_choices[,3]),
             mean(comp_adult_block5_choices[,3]))
             
comp_adult_line2_means <- c( mean(comp_adult_block1_choices[,2]), 
             mean(comp_adult_block2_choices[,2]),
             mean(comp_adult_block3_choices[,2]),
             mean(comp_adult_block4_choices[,2]),
             mean(comp_adult_block5_choices[,2]))
             
comp_adult_line1_means <- c( mean(comp_adult_block1_choices[,1]), 
             mean(comp_adult_block2_choices[,1]),
             mean(comp_adult_block3_choices[,1]),
             mean(comp_adult_block4_choices[,1]),
             mean(comp_adult_block5_choices[,1]))

# standard deviations
comp_adult_line10_sds <- c( sd(comp_adult_block1_choices[,4]), 
             sd(comp_adult_block2_choices[,4]),
             sd(comp_adult_block3_choices[,4]),
             sd(comp_adult_block4_choices[,4]),
             sd(comp_adult_block5_choices[,4]))
             
comp_adult_line3_sds <- c( sd(comp_adult_block1_choices[,3]), 
             sd(comp_adult_block2_choices[,3]),
             sd(comp_adult_block3_choices[,3]),
             sd(comp_adult_block4_choices[,3]),
             sd(comp_adult_block5_choices[,3]))
             
comp_adult_line2_sds <- c( sd(comp_adult_block1_choices[,2]), 
             sd(comp_adult_block2_choices[,2]),
             sd(comp_adult_block3_choices[,2]),
             sd(comp_adult_block4_choices[,2]),
             sd(comp_adult_block5_choices[,2]))
             
comp_adult_line1_sds <- c( sd(comp_adult_block1_choices[,1]), 
             sd(comp_adult_block2_choices[,1]),
             sd(comp_adult_block3_choices[,1]),
             sd(comp_adult_block4_choices[,1]),
             sd(comp_adult_block5_choices[,1]))

# Ns
comp_adult_line10_lengths <- c( length(comp_adult_block1_choices[,4]), 
             length(comp_adult_block2_choices[,4]),
             length(comp_adult_block3_choices[,4]),
             length(comp_adult_block4_choices[,4]),
             length(comp_adult_block5_choices[,4]))
             
comp_adult_line3_lengths <- c( length(comp_adult_block1_choices[,3]), 
             length(comp_adult_block2_choices[,3]),
             length(comp_adult_block3_choices[,3]),
             length(comp_adult_block4_choices[,3]),
             length(comp_adult_block5_choices[,3]))
             
comp_adult_line2_lengths <- c( length(comp_adult_block1_choices[,2]), 
             length(comp_adult_block2_choices[,2]),
             length(comp_adult_block3_choices[,2]),
             length(comp_adult_block4_choices[,2]),
             length(comp_adult_block5_choices[,2]))
             
comp_adult_line1_lengths <- c( length(comp_adult_block1_choices[,1]), 
             length(comp_adult_block2_choices[,1]),
             length(comp_adult_block3_choices[,1]),
             length(comp_adult_block4_choices[,1]),
             length(comp_adult_block5_choices[,1]))
             
comp_adult_line10_ses <- comp_adult_line10_sds/sqrt(comp_adult_line10_lengths)
comp_adult_line3_ses <- comp_adult_line3_sds/sqrt(comp_adult_line3_lengths)
comp_adult_line2_ses <- comp_adult_line2_sds/sqrt(comp_adult_line2_lengths)
comp_adult_line1_ses <- comp_adult_line1_sds/sqrt(comp_adult_line1_lengths)



plot(comp_adult_line10_means/20, type='l', lty=2, col = 'blue', 
	 ylim=c(0,1), ylab='Proportion of selections', 
	 xlab="Block (20 trials)", main='Competition', cex.lab= 1.4, cex.axis=1.3, lwd=2)
lines(comp_adult_line3_means/20, col = 'green', lwd=3)
lines(comp_adult_line2_means/20, col = 'yellow', lwd=3)
lines(comp_adult_line1_means/20, col = 'red', lwd=3)

# legend(1, 1.0, c("10", "3", "2", "1"), 
	   # col = c("blue", "green", "yellow", "red"), 
	   # lty = c(2,1,1,1), cex = 1.2)

x <- seq(5)
segments(x, comp_adult_line10_means/20+comp_adult_line10_ses/20, x, comp_adult_line10_means/20-comp_adult_line10_ses/20, col='blue')
segments(x, comp_adult_line3_means/20+comp_adult_line3_ses/20, x, comp_adult_line3_means/20-comp_adult_line3_ses/20, col='green')
segments(x, comp_adult_line2_means/20+comp_adult_line2_ses/20, x, comp_adult_line2_means/20-comp_adult_line2_ses/20, col='yellow')
segments(x, comp_adult_line1_means/20+comp_adult_line1_ses/20, x, comp_adult_line1_means/20-comp_adult_line1_ses/20, col='red')


#dev.off()


##################

cong_adult_block1 <- cong_adult_data[cong_adult_data$trial <= 20,]
cong_adult_block2 <- cong_adult_data[cong_adult_data$trial > 20 & cong_adult_data$trial <= 40,]
cong_adult_block3 <- cong_adult_data[cong_adult_data$trial > 40 & cong_adult_data$trial <= 60,]
cong_adult_block4 <- cong_adult_data[cong_adult_data$trial > 60 & cong_adult_data$trial <= 80,]
cong_adult_block5 <- cong_adult_data[cong_adult_data$trial > 80 & cong_adult_data$trial <= 100,]


cong_adult_block1_choices <- tapply(cong_adult_block1$reward, list(cong_adult_block1$subj, cong_adult_block1$reward), length)
cong_adult_block1_choices[is.na(cong_adult_block1_choices)] <- 0

cong_adult_block2_choices <- tapply(cong_adult_block2$reward, list(cong_adult_block2$subj, cong_adult_block2$reward), length)
cong_adult_block2_choices[is.na(cong_adult_block2_choices)] <- 0

cong_adult_block3_choices <- tapply(cong_adult_block3$reward, list(cong_adult_block3$subj, cong_adult_block3$reward), length)
cong_adult_block3_choices[is.na(cong_adult_block3_choices)] <- 0

cong_adult_block4_choices <- tapply(cong_adult_block4$reward, list(cong_adult_block4$subj, cong_adult_block4$reward), length)
cong_adult_block4_choices[is.na(cong_adult_block4_choices)] <- 0

cong_adult_block5_choices <- tapply(cong_adult_block5$reward, list(cong_adult_block5$subj, cong_adult_block5$reward), length)
cong_adult_block5_choices[is.na(cong_adult_block5_choices)] <- 0


# means
cong_adult_line10_means <- c( mean(cong_adult_block1_choices[,4]), 
             mean(cong_adult_block2_choices[,4]),
             mean(cong_adult_block3_choices[,4]),
             mean(cong_adult_block4_choices[,4]),
             mean(cong_adult_block5_choices[,4]))
             
cong_adult_line3_means <- c( mean(cong_adult_block1_choices[,3]), 
             mean(cong_adult_block2_choices[,3]),
             mean(cong_adult_block3_choices[,3]),
             mean(cong_adult_block4_choices[,3]),
             mean(cong_adult_block5_choices[,3]))
             
cong_adult_line2_means <- c( mean(cong_adult_block1_choices[,2]), 
             mean(cong_adult_block2_choices[,2]),
             mean(cong_adult_block3_choices[,2]),
             mean(cong_adult_block4_choices[,2]),
             mean(cong_adult_block5_choices[,2]))
             
cong_adult_line1_means <- c( mean(cong_adult_block1_choices[,1]), 
             mean(cong_adult_block2_choices[,1]),
             mean(cong_adult_block3_choices[,1]),
             mean(cong_adult_block4_choices[,1]),
             mean(cong_adult_block5_choices[,1]))

# standard deviations
cong_adult_line10_sds <- c( sd(cong_adult_block1_choices[,4]), 
             sd(cong_adult_block2_choices[,4]),
             sd(cong_adult_block3_choices[,4]),
             sd(cong_adult_block4_choices[,4]),
             sd(cong_adult_block5_choices[,4]))
             
cong_adult_line3_sds <- c( sd(cong_adult_block1_choices[,3]), 
             sd(cong_adult_block2_choices[,3]),
             sd(cong_adult_block3_choices[,3]),
             sd(cong_adult_block4_choices[,3]),
             sd(cong_adult_block5_choices[,3]))
             
cong_adult_line2_sds <- c( sd(cong_adult_block1_choices[,2]), 
             sd(cong_adult_block2_choices[,2]),
             sd(cong_adult_block3_choices[,2]),
             sd(cong_adult_block4_choices[,2]),
             sd(cong_adult_block5_choices[,2]))
             
cong_adult_line1_sds <- c( sd(cong_adult_block1_choices[,1]), 
             sd(cong_adult_block2_choices[,1]),
             sd(cong_adult_block3_choices[,1]),
             sd(cong_adult_block4_choices[,1]),
             sd(cong_adult_block5_choices[,1]))

# Ns
cong_adult_line10_lengths <- c( length(cong_adult_block1_choices[,4]), 
             length(cong_adult_block2_choices[,4]),
             length(cong_adult_block3_choices[,4]),
             length(cong_adult_block4_choices[,4]),
             length(cong_adult_block5_choices[,4]))
             
cong_adult_line3_lengths <- c( length(cong_adult_block1_choices[,3]), 
             length(cong_adult_block2_choices[,3]),
             length(cong_adult_block3_choices[,3]),
             length(cong_adult_block4_choices[,3]),
             length(cong_adult_block5_choices[,3]))
             
cong_adult_line2_lengths <- c( length(cong_adult_block1_choices[,2]), 
             length(cong_adult_block2_choices[,2]),
             length(cong_adult_block3_choices[,2]),
             length(cong_adult_block4_choices[,2]),
             length(cong_adult_block5_choices[,2]))
             
cong_adult_line1_lengths <- c( length(cong_adult_block1_choices[,1]), 
             length(cong_adult_block2_choices[,1]),
             length(cong_adult_block3_choices[,1]),
             length(cong_adult_block4_choices[,1]),
             length(cong_adult_block5_choices[,1]))
             
cong_adult_line10_ses <- cong_adult_line10_sds/sqrt(cong_adult_line10_lengths)
cong_adult_line3_ses <- cong_adult_line3_sds/sqrt(cong_adult_line3_lengths)
cong_adult_line2_ses <- cong_adult_line2_sds/sqrt(cong_adult_line2_lengths)
cong_adult_line1_ses <- cong_adult_line1_sds/sqrt(cong_adult_line1_lengths)



plot(cong_adult_line10_means/20, type='l', lty= 2, col = 'blue', 
	 ylim=c(0,1), ylab='Proportion of selections', 
	 xlab="Block (20 trials)", main='Congruent', cex.lab= 1.4, cex.axis=1.3, lwd=2)
lines(cong_adult_line3_means/20, col = 'green', lwd=3)
lines(cong_adult_line2_means/20, col = 'yellow', lwd=3)
lines(cong_adult_line1_means/20, col = 'red', lwd=3)

# legend(1, 1.0, c("10", "3", "2", "1"), 
	   # col = c("blue", "green", "yellow", "red"), 
	   # lty = c(2,1,1,1), cex = 1.2)

x <- seq(5)
segments(x, cong_adult_line10_means/20+cong_adult_line10_ses/20, x, cong_adult_line10_means/20-cong_adult_line10_ses/20, col='blue')
segments(x, cong_adult_line3_means/20+cong_adult_line3_ses/20, x, cong_adult_line3_means/20-cong_adult_line3_ses/20, col='green')
segments(x, cong_adult_line2_means/20+cong_adult_line2_ses/20, x, cong_adult_line2_means/20-cong_adult_line2_ses/20, col='yellow')
segments(x, cong_adult_line1_means/20+cong_adult_line1_ses/20, x, cong_adult_line1_means/20-cong_adult_line1_ses/20, col='red')



################################################################
 subjects <- unique(cong_adult_data$subj) 
 
 graphs_per_row = 6
 
 par(mfrow=c( ceiling(length(subjects) / graphs_per_row), graphs_per_row))
 #par(mfrow=c(3,5))
 
 for (i in subjects) {
		graphdata <- cong_adult_data[cong_adult_data$subj == i,]
 
 
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
 subjects <- unique(comp_adult_data$subj) 
 
 graphs_per_row = 6
 
 par(mfrow=c( ceiling(length(subjects) / graphs_per_row), graphs_per_row))
 #par(mfrow=c(3,5))
 
 for (i in subjects) {
		graphdata <- comp_adult_data[comp_adult_data$subj == i,]
 
 
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

subjects <- unique(bl_adult_data$subj) 
 
 graphs_per_row = 6
 
 par(mfrow=c( ceiling(length(subjects) / graphs_per_row), graphs_per_row))
 #par(mfrow=c(3,5))
 
 for (i in subjects) {
		graphdata <- bl_adult_data[bl_adult_data$subj == i,]
 
 
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

