




par(mfrow=c(2,3))



hist(baseline.data$ebm_weight, xlim=c(0,1), ylim=c(0,35),
 	breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), 
 	main='Baseline', xlab='Phi', col = 'blue', density=45, border='black', cex.lab=1.3, cex.axis=1.3)
hist(competition.data$ebm_weight, xlim=c(0,1), ylim=c(0,35), 
	breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), 
	main='Competition', xlab='Phi', col = 'red', density=45, border='black', cex.lab=1.3, cex.axis=1.3)
hist(congruent.data$ebm_weight, xlim=c(0,1), ylim=c(0,35), 
	breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), 
	main='Congruent', xlab="Phi", col = 'green', density=45, border='black', cex.lab=1.3, cex.axis=1.3)


hist(baseline.adult.data$ebm_weight, xlim=c(0,1), ylim=c(0,35),
 	breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), 
 	main='Baseline', xlab='Phi', col = 'blue', density=45, border='black', cex.lab=1.3, cex.axis=1.3)
hist(competition.adult.data$ebm_weight, xlim=c(0,1), ylim=c(0,35), 
	breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), 
	main='Competition', xlab='Phi', col = 'red', density=45, border='black', cex.lab=1.3, cex.axis=1.3)
hist(congruent.adult.data$ebm_weight, xlim=c(0,1), ylim=c(0,35), 
	breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), 
	main='Congruent', xlab="Phi", col = 'green', density=45, border='black', cex.lab=1.3, cex.axis=1.3)