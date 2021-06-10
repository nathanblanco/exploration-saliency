congruent_ages <- c(54, 53, 65, 66, 58, 62, 56, 56, 59, 64, 49, 54, 54, 63, 67, 63, 49,
                     57, 55, 53, 58, 53, 57, 58, 59, 53, 48, 50, 55, 56, 58, 59, 53, 49, 56, 56, 63)
                     
                     
                                           
baseline_ages <- c(57, 58, 57, 57, 66, 49, 67, 64, 64, 52, 67, 51, 57, 60, 56, 51, 63, 63, 63, 59, 
					59, 55, 50, 67, 53, 64, 62, 61, 63, 54, 61, 53, 50, 52, 69, 66)
					
					
competition_ages <- c(60, 50, 65, 58, 50, 55, 59, 58, 48, 54, 62, 58, 63, 
					63, 58, 54, 58, 50, 59, 60, 48, 53, 56, 57, 58, 62, 48,
					 63, 57, 58, 57, 58, 48, 62, 57, 62, 58)	
					 
					 
t.test(congruent_ages, baseline_ages, var.equal=T)
t.test(competition_ages, baseline_ages, var.equal=T)
t.test(competition_ages, congruent_ages, var.equal=T)

par(mfrow=c(3,1))
hist(congruent_ages)
hist(competition_ages)
hist(baseline_ages)


results <- NULL

results$ages <- c(congruent_ages, baseline_ages, competition_ages)
results$cond <- factor(c(  rep('congruent', length(congruent_ages)),
						   rep('baseline', length(baseline_ages)),
						   rep('competition', length(competition_ages))))
						   
results <- as.data.frame(results)

x <- aov(ages~cond, data=results)

# 1: F
congruent_gender = c(0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 
					 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 
					 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 
					 1, 1, 1, 0, 0, 1, 1)
					 
baseline_gender <- c(1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 
					 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 
					 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1)
					 
competition_gender <- c(0, 1, 0, 1, 0, 0, 1, 1, 1, 0, 
					  0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1,
					  1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0)
					  
					  
props <- c(  sum(congruent_gender), length(congruent_gender) - sum(congruent_gender),
			 sum(baseline_gender), length(baseline_gender) - sum(baseline_gender),
			 sum(competition_gender), length(competition_gender) - sum(competition_gender))

props			 
props <- matrix(props, 2)
props

chisq.test(props)



###########

adult_genders <- matrix(c(22,14,1,19,18,0,19,13,2), 3)

chisq.test(adult_genders)

chisq.test(adult_genders[1:2,])



# adult vs. children gender chi-square

genders <- matrix(c(58, 52, 60, 45), 2)

chisq.test(genders)


genders <- matrix(c(58, 52, 0, 60, 45, 3), 3)
genders

chisq.test(genders)