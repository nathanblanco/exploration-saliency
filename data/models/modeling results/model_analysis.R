


############################################################

congruent.data <- read.table('EBM_aics_congruent.txt', header=T)
competition.data <- read.table('EBM_aics_competition.txt', header=T)
baseline.data <- read.table('EBM_aics_baseline.txt', header=T)


########
median(congruent.data$ebm_weight)
median(competition.data$ebm_weight)
median(baseline.data$ebm_weight)

mean(congruent.data$ebm_weight)
mean(competition.data$ebm_weight)
mean(baseline.data$ebm_weight)

sd(congruent.data$ebm_weight)
sd(competition.data$ebm_weight)
sd(baseline.data$ebm_weight)


median(congruent.data$ebm_Bev)
median(competition.data$ebm_Bev)
median(baseline.data$ebm_Bev)

sd(congruent.data$ebm_Bev)
sd(competition.data$ebm_Bev)
sd(baseline.data$ebm_Bev)

par(mfrow=c(3,1))
hist(congruent.data$ebm_weight, xlim=c(0,1), ylim=c(0,25), breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), main='Congruent', xlab="Phi", col = 'grey')
hist(competition.data$ebm_weight, xlim=c(0,1), ylim=c(0,25), breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), main='Competition', xlab='Phi', col = 'grey')
hist(baseline.data$ebm_weight, xlim=c(0,1), ylim=c(0,25), breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), main='Baseline', xlab='Phi', col = 'grey')

wilcox.test(congruent.data$ebm_weight , competition.data$ebm_weight, correct=T)

wilcox.test(baseline.data$ebm_weight, congruent.data$ebm_weight ,  correct=T)
wilcox.test(baseline.data$ebm_weight, competition.data$ebm_weight, correct=T)


wilcox.test(congruent.data$ebm_Bev , competition.data$ebm_Bev, correct=T)
wilcox.test(baseline.data$ebm_Bev, congruent.data$ebm_Bev,  correct=T)
wilcox.test(baseline.data$ebm_Bev , competition.data$ebm_Bev, correct=T)




results <- NULL

results$phi <- c(congruent.data$ebm_weight, competition.data$ebm_weight, baseline.data$ebm_weight)
results$beta <- c(congruent.data$ebm_Bev, competition.data$ebm_Bev, baseline.data$ebm_Bev)


results$cond <- factor(c(rep(1, length(congruent.data$ebm_weight)),
						rep(2, length(competition.data$ebm_weight)),
						rep(3, length(baseline.data$ebm_weight))))
						
results <- as.data.frame(results)

x <- aov(phi~cond, data=results)
summary(x)

x <- aov(beta~cond, data=results)
summary(x)



t.test(congruent.data$ebm_weight , competition.data$ebm_weight, var.equal=T)

t.test(baseline.data$ebm_weight, congruent.data$ebm_weight ,  var.equal=T)
t.test(baseline.data$ebm_weight, competition.data$ebm_weight, var.equal=T)

##################

par(mfrow=c(3,1))
barplot(congruent.data$ebm_weight[order(congruent.data$ebm_weight)], ylim=c(0,1), main='Congruent')
barplot(competition.data$ebm_weight[order(competition.data$ebm_weight)], ylim=c(0,1), main='Competition')
barplot(baseline.data$ebm_weight[order(baseline.data$ebm_weight)], ylim=c(0,1), main='Baseline')


par(mfrow=c(3,1))
#par(mfrow=c(1,3))
hist(baseline.data$ebm_weight, xlim=c(0,1), ylim=c(0,35),
 	breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), 
 	main='Baseline', xlab='Phi', col = 'blue', density=45, border='black', cex.lab=1.3, cex.axis=1.3)
hist(competition.data$ebm_weight, xlim=c(0,1), ylim=c(0,35), 
	breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), 
	main='Competition', xlab='Phi', col = 'red', density=45, border='black', cex.lab=1.3, cex.axis=1.3)
hist(congruent.data$ebm_weight, xlim=c(0,1), ylim=c(0,35), 
	breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), 
	main='Congruent', xlab="Phi", col = 'green', density=45, border='black', cex.lab=1.3, cex.axis=1.3)


breaks=c(-35,-30,-25,-20,-15,-10,-5,0,5,10), 

par(mfrow=c(1,3))
hist(log(baseline.data$ebm_Bev),  ylim=c(0,35), xlim=c(-35,10), breaks=c(-35,-30,-25,-20,-15,-10,-5,0,5,10), 
 	main='Baseline', xlab='log(Beta)', col = 'blue', density=45, border='black', cex.lab=1.3, cex.axis=1.3)
 	
hist(log(competition.data$ebm_Bev),  ylim=c(0,35), xlim=c(-35,10), breaks=c(-35,-30,-25,-20,-15,-10,-5,0,5,10), 
	main='Competition', xlab='log(Beta)', col = 'red', density=45, border='black', cex.lab=1.3, cex.axis=1.3)
	
hist(log(congruent.data$ebm_Bev),  ylim=c(0,35), xlim=c(-35,10), breaks=c(-35,-30,-25,-20,-15,-10,-5,0,5,10), 
	main='Congruent', xlab="log(Beta)", col = 'green', density=45, border='black', cex.lab=1.3, cex.axis=1.3)


congruent.ratio <- congruent.data$Lag_Bev/congruent.data$Lag_Blag
competition.ratio <- competition.data$Lag_Bev/competition.data$Lag_Blag
baseline.ratio <- baseline.data$Lag_Bev/baseline.data$Lag_Blag

RL_comp <- read.table('RL_aics_competition.txt', header=T)
RL_cong <- read.table('RL_aics_congruent.txt', header=T)
RL_bl <- read.table('RL_aics_baseline.txt', header=T)

comp.fit <- as.numeric(RL_comp$RL_aic < competition.data$ebm_aic)
cong.fit <- as.numeric(RL_cong$RL_aic < congruent.data$ebm_aic)
bl.fit <- as.numeric(RL_bl$RL_aic < baseline.data$ebm_aic)
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

wilcox.test(congruent.data$Lag_Blag, competition.data$Lag_Blag, correct=T)
wilcox.test(congruent.data$Lag_Blag, baseline.data$Blag, correct=T)
wilcox.test(baseline.data$Lag_Blag, competition.data$Lag_Blag, correct=T)

wilcox.test(congruent.data$Lag_Bev, competition.data$Lag_Bev, correct=T)
wilcox.test(congruent.data$Lag_Bev, baseline.data$Bev, correct=T)
wilcox.test(baseline.data$Lag_Bev, competition.data$Lag_Bev, correct=T)

median(congruent.ratio)
median(competition.ratio)
median(baseline.ratio)

#mean(congruent.ratio)
#mean(competition.ratio)
#mean(baseline.ratio)


median(congruent.data$Lag_Bev)
median(competition.data$Lag_Bev)
median(baseline.data$Lag_Bev)

median(congruent.data$Lag_Blag)
median(competition.data$Lag_Blag)
median(baseline.data$Lag_Blag)

##################################
t.test(congruent.ratio, competition.ratio, var.equal=T)
t.test(congruent.ratio , baseline.ratio, var.equal=T)
t.test(baseline.ratio , competition.ratio, var.equal=T)


t.test(congruent.data$Lag_Blag, competition.data$Lag_Blag, var.equal=T)
t.test(congruent.data$Lag_Blag, baseline.data$Blag, var.equal=T)
t.test(baseline.data$Blag, competition.data$Lag_Blag, var.equal=T)

t.test(congruent.data$Lag_Bev, competition.data$Lag_Bev, var.equal=T)
t.test(congruent.data$Lag_Bev, baseline.data$Bev, var.equal=T)
t.test(baseline.data$Bev, competition.data$Lag_Bev, var.equal=T)



results <- NULL

results$ratio <- c(congruent.ratio, competition.ratio, baseline.ratio)
results$Blag <- c(congruent.data$Lag_Blag, competition.data$Lag_Blag, baseline.data$Blag)
results$Bev <- c(congruent.data$Lag_Bev, competition.data$Lag_Bev, baseline.data$Bev)


results$cond <- factor(c(rep(1, length(congruent.ratio)),
						rep(2, length(competition.ratio)),
						rep(3, length(baseline.ratio))))
						
results <- as.data.frame(results)

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
graph_data <- as.data.frame(graph_data)

boxplot(switches ~ group, data=graph_data,
 ylab = 'Proportion of switch responses', col='white', ylim=c(0,10),
 main = 'Response Switching', names.arg=c('Congruent', 'Competition'))
stripchart(switches ~ group, data=graph_data, vertical = TRUE,  
    method = "jitter", add = TRUE, pch = 19, col = c('blue', 'red', 'green'))



###########################

graph_data <- NULL
graph_data$switches <- c(congruent.data$ebm_weight, competition.data$ebm_weight, baseline.data$ebm_weight)
graph_data$group <- factor(c(rep("Congruent", length(congruent.data$ebm_weight)),
							 rep("Competition", length(competition.data$ebm_weight)),
							 rep("Baseline", length(baseline.data$ebm_weight))))
graph_data <- as.data.frame(graph_data)

boxplot(switches ~ group, data=graph_data,
 ylab = 'Proportion of switch responses', col='white', ylim=c(0,1),
 main = 'Response Switching', names.arg=c('Congruent', 'Competition'))
stripchart(switches ~ group, data=graph_data, vertical = TRUE,  
    method = "jitter", add = TRUE, pch = 19, col = c('blue', 'red', 'green'))