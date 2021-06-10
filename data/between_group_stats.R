

cong_props <- cong_choices[,4]/100
comp_props <- comp_choices[,4]/100
bl_props <- bl_choices[,4]/100


cong_adult_props <- cong_adult_choices[,4]/100
comp_adult_props <- comp_adult_choices[,4]/100
bl_adult_props <- bl_adult_choices[,4]/100

cong_props_low <- cong_choices[,1]/100
comp_props_low <- comp_choices[,1]/100
bl_props_low <- bl_choices[,1]/100

cong_adult_props_low <- cong_adult_choices[,1]/100
comp_adult_props_low <- comp_adult_choices[,1]/100
bl_adult_props_low <- bl_adult_choices[,1]/100




results <- NULL

results$props <- c(cong_props, comp_props, bl_props,
				   cong_adult_props, comp_adult_props, bl_adult_props)
				   
results$props_low <- c(cong_props_low, comp_props_low, bl_props_low,
				   cong_adult_props_low, comp_adult_props_low, bl_adult_props_low)

results$cond <- factor(c(
						rep(1, length(cong_props)),
						rep(2, length(comp_props)),
						rep(3, length(bl_props)),
						rep(1, length(cong_adult_props)),
						rep(2, length(comp_adult_props)),
						rep(3, length(bl_adult_props))
						))
						
results$group <- factor(c(
						rep(1, length(cong_props)),
						rep(1, length(comp_props)),
						rep(1, length(bl_props)),
						rep(2, length(cong_adult_props)),
						rep(2, length(comp_adult_props)),
						rep(2, length(bl_adult_props))
						))
						
						
results <- as.data.frame(results)

x <- aov(props~group*cond, data=results)
summary(x)

x <- aov(props_low~group*cond, data=results)
summary(x)

###--------------------------------------------------


results <- NULL

results$props <- c(cong_adult_props, comp_adult_props, bl_adult_props)

results$props_low <- c(cong_adult_props_low, comp_adult_props_low, bl_adult_props_low)

results$cond <- factor(c(rep(1, length(cong_adult_props)),
						rep(2, length(comp_adult_props)),
						rep(3, length(bl_adult_props))))
						
results <- as.data.frame(results)

x <- aov(props~cond, data=results)
summary(x)

x <- aov(props_low~cond, data=results)
summary(x)

t.test(cong_adult_props, comp_adult_props, var.equal=T)
t.test(cong_adult_props, bl_adult_props, var.equal=T)
t.test(bl_adult_props, comp_adult_props, var.equal=T)


d.t.unpaired<-function(t.val,n1,n2){
  d<-t.val*sqrt((n1+n2)/(n1*n2))
  names(d)<-"effect size d"
  return(d)
}

length(cong_adult_props)
length(comp_adult_props)
length(bl_adult_props)


d.t.unpaired(3.8634, 37, 34) # cong comp

d.t.unpaired(1.5041, 37, 37) # cong bl

d.t.unpaired(2.9398, 37, 34) # bl comp


t.test(cong_adult_props_low, comp_adult_props_low, var.equal=T)
t.test(cong_adult_props_low, bl_adult_props_low, var.equal=T)
t.test(bl_adult_props_low, comp_adult_props_low, var.equal=T)

d.t.unpaired(3.399, 37, 34) # cong comp

d.t.unpaired(1.0264, 37, 37) # cong bl

d.t.unpaired(3.2324, 37, 34) # bl comp


#########################################################################################


graph_data <- NULL
graph_data$switches <- c(1-(cong_stay_total)/100, 1-(comp_stay_total)/100, 1-(bl_stay_total)/100,
						 1-(cong_adult_stay_total)/100, 1-(comp_adult_stay_total)/100, 1-(bl_adult_stay_total)/100)
						 
graph_data$cond <- factor(c(rep("Congruent", length(cong_stay_total)),
							 rep("Competition", length(comp_stay_total)),
							 rep("Baseline", length(bl_stay_total)),
							 rep("Congruent", length(cong_adult_stay_total)),
							 rep("Competition", length(comp_adult_stay_total)),
							 rep("Baseline", length(bl_adult_stay_total))
							 ))
							 
graph_data$group <- factor(c(
						rep(1, length(cong_props)),
						rep(1, length(comp_props)),
						rep(1, length(bl_props)),
						rep(2, length(cong_adult_props)),
						rep(2, length(comp_adult_props)),
						rep(2, length(bl_adult_props))
						))							 
							 
graph_data <- as.data.frame(graph_data)


x <- aov( switches ~ cond*group, data=graph_data)
summary(x)

x <- aov( switches ~ cond, data=graph_data[graph_data$group==2,])
summary(x)

adult_graph_data <- graph_data[graph_data$group==2,]

t.test(adult_graph_data[adult_graph_data$cond=='Congruent',]$switches, adult_graph_data[adult_graph_data$cond=='Competition',]$switches,var.equal=T)
t.test(adult_graph_data[adult_graph_data$cond=='Baseline',]$switches, adult_graph_data[adult_graph_data$cond=='Competition',]$switches,var.equal=T)
t.test(adult_graph_data[adult_graph_data$cond=='Congruent',]$switches, adult_graph_data[adult_graph_data$cond=='Baseline',]$switches,var.equal=T)
