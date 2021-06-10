


############################################################

congruent.data.adult <- read.table('EBM_aics_congruent_adults.txt', header=T)
competition.data.adult <- read.table('EBM_aics_competition_adults.txt', header=T)
baseline.data.adult <- read.table('EBM_aics_baseline_adults.txt', header=T)


########
median(congruent.data.adult$ebm_weight)
median(competition.data.adult$ebm_weight)
median(baseline.data.adult$ebm_weight)

mean(congruent.data.adult$ebm_weight)
mean(competition.data.adult$ebm_weight)
mean(baseline.data.adult$ebm_weight)

sd(congruent.data.adult$ebm_weight)
sd(competition.data.adult$ebm_weight)
sd(baseline.data.adult$ebm_weight)


median(congruent.data.adult$ebm_Bev)
median(competition.data.adult$ebm_Bev)
median(baseline.data.adult$ebm_Bev)

sd(congruent.data.adult$ebm_Bev)
sd(competition.data.adult$ebm_Bev)
sd(baseline.data.adult$ebm_Bev)

par(mfrow=c(3,1))
hist(congruent.data.adult$ebm_weight, xlim=c(0,1), ylim=c(0,25), breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), main='Congruent', xlab="Phi", col = 'grey')
hist(competition.data.adult$ebm_weight, xlim=c(0,1), ylim=c(0,25), breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), main='Competition', xlab='Phi', col = 'grey')
hist(baseline.data.adult$ebm_weight, xlim=c(0,1), ylim=c(0,25), breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), main='Baseline', xlab='Phi', col = 'grey')

wilcox.test(congruent.data.adult$ebm_weight , competition.data.adult$ebm_weight, correct=T)

wilcox.test(baseline.data.adult$ebm_weight, congruent.data.adult$ebm_weight ,  correct=T)
wilcox.test(baseline.data.adult$ebm_weight, competition.data.adult$ebm_weight, correct=T)


wilcox.test(congruent.data.adult$ebm_Bev , competition.data.adult$ebm_Bev, correct=T)
wilcox.test(baseline.data.adult$ebm_Bev, congruent.data.adult$ebm_Bev,  correct=T)
wilcox.test(baseline.data.adult$ebm_Bev , competition.data.adult$ebm_Bev, correct=T)




results <- NULL

results$phi <- c(congruent.data.adult$ebm_weight, competition.data.adult$ebm_weight, baseline.data.adult$ebm_weight)
results$beta <- c(congruent.data.adult$ebm_Bev, competition.data.adult$ebm_Bev, baseline.data.adult$ebm_Bev)


results$cond <- factor(c(rep(1, length(congruent.data.adult$ebm_weight)),
						rep(2, length(competition.data.adult$ebm_weight)),
						rep(3, length(baseline.data.adult$ebm_weight))))
						
results <- as.data.frame(results)

x <- aov(phi~cond, data=results)
summary(x)

x <- aov(beta~cond, data=results)
summary(x)

t.test(congruent.data.adult$ebm_weight , competition.data.adult$ebm_weight, var.equal=T)

t.test(baseline.data.adult$ebm_weight, congruent.data.adult$ebm_weight ,  var.equal=T)
t.test(baseline.data.adult$ebm_weight, competition.data.adult$ebm_weight, var.equal=T)


##################

par(mfrow=c(3,1))
barplot(congruent.data.adult$ebm_weight[order(congruent.data.adult$ebm_weight)], ylim=c(0,1), main='Congruent')
barplot(competition.data.adult$ebm_weight[order(competition.data.adult$ebm_weight)], ylim=c(0,1), main='Competition')
barplot(baseline.data.adult$ebm_weight[order(baseline.data.adult$ebm_weight)], ylim=c(0,1), main='Baseline')


#par(mfrow=c(3,1))
par(mfrow=c(1,3))
hist(baseline.data.adult$ebm_weight, xlim=c(0,1), ylim=c(0,35),
 	breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), 
 	main='Baseline', xlab='Phi', col = 'blue', density=45, border='black', cex.lab=1.3, cex.axis=1.3)
hist(competition.data.adult$ebm_weight, xlim=c(0,1), ylim=c(0,35), 
	breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), 
	main='Competition', xlab='Phi', col = 'red', density=45, border='black', cex.lab=1.3, cex.axis=1.3)
hist(congruent.data.adult$ebm_weight, xlim=c(0,1), ylim=c(0,35), 
	breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), 
	main='Congruent', xlab="Phi", col = 'green', density=45, border='black', cex.lab=1.3, cex.axis=1.3)

###########################################################

par(mfrow=c(1,3))
hist(log(baseline.data.adult$ebm_Bev),  ylim=c(0,35), xlim=c(-35,10), breaks=c(-35,-30,-25,-20,-15,-10,-5,0,5,10), 
 	main='Baseline', xlab='log(Beta)', col = 'blue', density=45, border='black', cex.lab=1.3, cex.axis=1.3)
 	
hist(log(competition.data.adult$ebm_Bev),  ylim=c(0,35), xlim=c(-35,10), breaks=c(-35,-30,-25,-20,-15,-10,-5,0,5,10), 
	main='Competition', xlab='log(Beta)', col = 'red', density=45, border='black', cex.lab=1.3, cex.axis=1.3)
	
hist(log(congruent.data.adult$ebm_Bev),  ylim=c(0,35), xlim=c(-35,10), breaks=c(-35,-30,-25,-20,-15,-10,-5,0,5,10), 
	main='Congruent', xlab="log(Beta)", col = 'green', density=45, border='black', cex.lab=1.3, cex.axis=1.3)

##############################################################


congruent.ratio <- congruent.data.adult$Lag_Bev/congruent.data.adult$Lag_Blag
competition.ratio <- competition.data.adult$Lag_Bev/competition.data.adult$Lag_Blag
baseline.ratio <- baseline.data.adult$Lag_Bev/baseline.data.adult$Lag_Blag

RL_comp <- read.table('RL_aics_competition_adults.txt', header=T)
RL_cong <- read.table('RL_aics_congruent_adults.txt', header=T)
RL_bl <- read.table('RL_aics_baseline_adults.txt', header=T)

comp.fit <- as.numeric(RL_comp$RL_aic < competition.data.adult$ebm_aic)
cong.fit <- as.numeric(RL_cong$RL_aic < congruent.data.adult$ebm_aic)
bl.fit <- as.numeric(RL_bl$RL_aic < baseline.data.adult$ebm_aic)
t.test(comp.fit, bl.fit, var.equal=T)
t.test(cong.fit, bl.fit, var.equal=T)


mean(comp.fit)
mean(cong.fit)
mean(bl.fit)

props <- c( length(cong.fit[cong.fit==0]), 
			length(comp.fit[comp.fit==0]), 
			length(bl.fit[bl.fit==0]), 
			length(cong.fit[cong.fit==1]), 
			length(comp.fit[comp.fit==1]), 
			length(bl.fit[bl.fit==1]))
 
chisq.test(props, correct=F)

props <- matrix(props, 3)

chisq.test(props, correct=F)

props <- c( length(cong.fit[cong.fit==0]), 
			length(bl.fit[bl.fit==0]), 
			length(cong.fit[cong.fit==1]), 
			length(bl.fit[bl.fit==1]))
			
props <- matrix(props, 2)

chisq.test(props, correct=F)

props <- c( length(comp.fit[comp.fit==0]), 
			length(bl.fit[bl.fit==0]), 
			length(comp.fit[comp.fit==1]), 
			length(bl.fit[bl.fit==1]))
			
props <- matrix(props, 2)

chisq.test(props, correct=F)




> mean(comp.fit)
[1] 0.4594595
> mean(cong.fit)
[1] 0.5135135
> mean(bl.fit)
[1] 0.05263158
> 

wilcox.test(congruent.ratio, competition.ratio, correct=T)
wilcox.test(congruent.ratio , baseline.ratio, correct=T)
wilcox.test(baseline.ratio , competition.ratio, correct=T)

wilcox.test(congruent.data.adult$Lag_Blag, competition.data.adult$Lag_Blag, correct=T)
wilcox.test(congruent.data.adult$Lag_Blag, baseline.data.adult$Blag, correct=T)
wilcox.test(baseline.data.adult$Lag_Blag, competition.data.adult$Lag_Blag, correct=T)

wilcox.test(congruent.data.adult$Lag_Bev, competition.data.adult$Lag_Bev, correct=T)
wilcox.test(congruent.data.adult$Lag_Bev, baseline.data.adult$Bev, correct=T)
wilcox.test(baseline.data.adult$Lag_Bev, competition.data.adult$Lag_Bev, correct=T)

median(congruent.ratio)
median(competition.ratio)
median(baseline.ratio)

#mean(congruent.ratio)
#mean(competition.ratio)
#mean(baseline.ratio)


median(congruent.data.adult$Lag_Bev)
median(competition.data.adult$Lag_Bev)
median(baseline.data.adult$Lag_Bev)

median(congruent.data.adult$Lag_Blag)
median(competition.data.adult$Lag_Blag)
median(baseline.data.adult$Lag_Blag)

##################################
t.test(congruent.ratio, competition.ratio, var.equal=T)
t.test(congruent.ratio , baseline.ratio, var.equal=T)
t.test(baseline.ratio , competition.ratio, var.equal=T)


t.test(congruent.data.adult$Lag_Blag, competition.data.adult$Lag_Blag, var.equal=T)
t.test(congruent.data.adult$Lag_Blag, baseline.data.adult$Blag, var.equal=T)
t.test(baseline.data.adult$Blag, competition.data.adult$Lag_Blag, var.equal=T)

t.test(congruent.data.adult$Lag_Bev, competition.data.adult$Lag_Bev, var.equal=T)
t.test(congruent.data.adult$Lag_Bev, baseline.data.adult$Bev, var.equal=T)
t.test(baseline.data.adult$Bev, competition.data.adult$Lag_Bev, var.equal=T)



results <- NULL

results$ratio <- c(congruent.ratio, competition.ratio, baseline.ratio)
results$Blag <- c(congruent.data.adult$Lag_Blag, competition.data.adult$Lag_Blag, baseline.data.adult$Blag)
results$Bev <- c(congruent.data.adult$Lag_Bev, competition.data.adult$Lag_Bev, baseline.data.adult$Bev)


results$cond <- factor(c(rep(1, length(congruent.ratio)),
						rep(2, length(competition.ratio)),
						rep(3, length(baseline.ratio))))
						
results <- as.data.adult.frame(results)

x <- aov(ratio~cond, data=results)
summary(x)

x <- aov(Blag~cond, data=results)
summary(x)

x <- aov(Bev~cond, data=results)
summary(x)







###########################

graph_data <- NULL
graph_data$switches <- c(congruent.ratio, competition.ratio, baseline.ratio)
graph_data$group <- factor(c(rep("Congruent", length(congruent.ratio)),
							 rep("Competition", length(competition.ratio)),
							 rep("Baseline", length(baseline.ratio))))
graph_data <- as.data.adult.frame(graph_data)

boxplot(switches ~ group, data=graph_data,
 ylab = 'Proportion of switch responses', col='white', ylim=c(0,10),
 main = 'Response Switching', names.arg=c('Congruent', 'Competition'))
stripchart(switches ~ group, data=graph_data, vertical = TRUE,  
    method = "jitter", add = TRUE, pch = 19, col = c('blue', 'red', 'green'))



###########################

graph_data <- NULL
graph_data$switches <- c(congruent.data.adult$ebm_weight, competition.data.adult$ebm_weight, baseline.data.adult$ebm_weight)
graph_data$group <- factor(c(rep("Congruent", length(congruent.data.adult$ebm_weight)),
							 rep("Competition", length(competition.data.adult$ebm_weight)),
							 rep("Baseline", length(baseline.data.adult$ebm_weight))))
graph_data <- as.data.adult.frame(graph_data)

boxplot(switches ~ group, data=graph_data,
 ylab = 'Proportion of switch responses', col='white', ylim=c(0,1),
 main = 'Response Switching', names.arg=c('Congruent', 'Competition'))
stripchart(switches ~ group, data=graph_data, vertical = TRUE,  
    method = "jitter", add = TRUE, pch = 19, col = c('blue', 'red', 'green'))