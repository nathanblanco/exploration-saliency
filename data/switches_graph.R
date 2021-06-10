
par(mfrow=c(1,2))


graph_data <- NULL
graph_data$switches <- c(1-(cong_stay_total)/100, 1-(comp_stay_total)/100, 1-(bl_stay_total)/100)
graph_data$group <- factor(c(rep("Congruent", length(cong_stay_total)),
							 rep("Competition", length(comp_stay_total)),
							 rep("Baseline", length(bl_stay_total))))
graph_data <- as.data.frame(graph_data)

#jpeg('graphs/switch_box.jpg')
boxplot(switches ~ group, data=graph_data,
 ylab = 'Proportion of switch responses', col='white', ylim=c(0,1),
 main = 'Children', names.arg=c('Congruent', 'Competition', 'Baseline'), cex.lab=1.2, cex.names=0.8, cex.axis=1.2)
stripchart(switches ~ group, data=graph_data, vertical = TRUE,  
    method = "jitter", add = TRUE, pch = 19, col = c('blue', 'red', 'green'))


abline(0.83, 0, col='red')
abline(0.65, 0, col='red')




graph_adult_data <- NULL
graph_adult_data$switches <- c(1-(cong_adult_stay_total)/100, 1-(comp_adult_stay_total)/100, 1-(bl_adult_stay_total)/100)
graph_adult_data$group <- factor(c(rep("Congruent", length(cong_adult_stay_total)),
							 rep("Competition", length(comp_adult_stay_total)),
							 rep("Baseline", length(bl_adult_stay_total))))
graph_adult_data <- as.data.frame(graph_adult_data)

#jpeg('graphs/switch_box.jpg')
boxplot(switches ~ group, data=graph_adult_data,
 ylab = 'Proportion of switch responses', col='white', ylim=c(0,1),
 main = 'Adults', names.arg=c('Congruent', 'Competition', 'Baseline'), cex.lab=1.2, cex.names=0.8, cex.axis=1.2)
stripchart(switches ~ group, data=graph_adult_data, vertical = TRUE,  
    method = "jitter", add = TRUE, pch = 19, col = c('blue', 'red', 'green'))
    
    
abline(0.83, 0, col='red')
abline(0.65, 0, col='red')
