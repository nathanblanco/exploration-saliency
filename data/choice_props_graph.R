
par(mfrow=c(2,3))

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

#legend(1, 1.0, c("10", "3", "2", "1"), 
#	   col = c("blue", "green", "yellow", "red"), 
#	   lty = c(2,1,1,1), cex = 1.2)

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

#legend(1, 1.0, c("10", "3", "2", "1"), 
#	   col = c("blue", "green", "yellow", "red"), 
#	   lty = c(2,1,1,1), cex = 1.2)

x <- seq(5)
segments(x, cong_line10_means/20+cong_line10_ses/20, x, cong_line10_means/20-cong_line10_ses/20, col='blue')
segments(x, cong_line3_means/20+cong_line3_ses/20, x, cong_line3_means/20-cong_line3_ses/20, col='green')
segments(x, cong_line2_means/20+cong_line2_ses/20, x, cong_line2_means/20-cong_line2_ses/20, col='yellow')
segments(x, cong_line1_means/20+cong_line1_ses/20, x, cong_line1_means/20-cong_line1_ses/20, col='red')




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

#legend(1, 1.0, c("10", "3", "2", "1"), 
#	   col = c("blue", "green", "yellow", "red"), 
#	   lty = c(2,1,1,1), cex = 1.2)

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

#legend(1, 1.0, c("10", "3", "2", "1"), 
#	   col = c("blue", "green", "yellow", "red"), 
#	   lty = c(2,1,1,1), cex = 1.2)

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

#legend(1, 1.0, c("10", "3", "2", "1"), 
#	   col = c("blue", "green", "yellow", "red"), 
#	   lty = c(2,1,1,1), cex = 1.2)

x <- seq(5)
segments(x, cong_adult_line10_means/20+cong_adult_line10_ses/20, x, cong_adult_line10_means/20-cong_adult_line10_ses/20, col='blue')
segments(x, cong_adult_line3_means/20+cong_adult_line3_ses/20, x, cong_adult_line3_means/20-cong_adult_line3_ses/20, col='green')
segments(x, cong_adult_line2_means/20+cong_adult_line2_ses/20, x, cong_adult_line2_means/20-cong_adult_line2_ses/20, col='yellow')
segments(x, cong_adult_line1_means/20+cong_adult_line1_ses/20, x, cong_adult_line1_means/20-cong_adult_line1_ses/20, col='red')


