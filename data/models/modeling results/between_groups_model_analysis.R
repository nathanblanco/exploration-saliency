
congruent.data <- read.table('EBM_aics_congruent.txt', header=T)
competition.data <- read.table('EBM_aics_competition.txt', header=T)
baseline.data <- read.table('EBM_aics_baseline.txt', header=T)

congruent.data.adult <- read.table('EBM_aics_congruent_adults.txt', header=T)
competition.data.adult <- read.table('EBM_aics_competition_adults.txt', header=T)
baseline.data.adult <- read.table('EBM_aics_baseline_adults.txt', header=T)





results <- NULL

results$phi <- c(congruent.data$ebm_weight, competition.data$ebm_weight, baseline.data$ebm_weight,
				 congruent.data.adult$ebm_weight, competition.data.adult$ebm_weight, baseline.data.adult$ebm_weight)



results$beta <- c(congruent.data$ebm_Bev, competition.data$ebm_Bev, baseline.data$ebm_Bev,
				  congruent.data.adult$ebm_Bev, competition.data.adult$ebm_Bev, baseline.data.adult$ebm_Bev)


results$cond <- factor(c(rep(1, length(congruent.data$ebm_weight)),
						rep(2, length(competition.data$ebm_weight)),
						rep(3, length(baseline.data$ebm_weight)),
						
						rep(1, length(congruent.data.adult$ebm_weight)),
						rep(2, length(competition.data.adult$ebm_weight)),
						rep(3, length(baseline.data.adult$ebm_weight))
						
						))

results$group <- factor(c(rep(1, length(congruent.data$ebm_weight)),
						rep(1, length(competition.data$ebm_weight)),
						rep(1, length(baseline.data$ebm_weight)),
						
						rep(2, length(congruent.data.adult$ebm_weight)),
						rep(2, length(competition.data.adult$ebm_weight)),
						rep(2, length(baseline.data.adult$ebm_weight))
						
						))

						
results <- as.data.frame(results)

x <- aov(phi~group*cond, data=results)
summary(x)

x <- aov(beta~group*cond, data=results)
summary(x)

