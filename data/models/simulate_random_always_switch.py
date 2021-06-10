import os, sys, signal
from numpy import *
from random import randint, shuffle



n_sims = 10000
n_trials = 100

f = open('random_always_switch_sims.txt', 'w+')

rewards = [1, 2, 3, 10]

for i in range(n_sims):
	print(i)

	shuffle(rewards)
	
	last_response = -1
	
	response = -1
	
	for j in range(n_trials):
		
		response = randint(0,3)
		
		while response == last_response:
			response = randint(0,3)
			
		payoff =  rewards[response]
		
		f.write(str(i)+'\t'+str(response)+'\t'+str(payoff)+'\n')
		
		last_response = response
		