import os, sys, signal
#from openfile import openfile
# import switch_model
# import RL_auto
import RL_Lag
# import RL
# import bias_model
# import mixture_model1
import exploration_bonus
import exploration_bonus_mixture
from numpy import *


def openfile(filename, n_trials):
    myfile = open(filename)
    alldata = myfile.readlines()
    
    for i in range(len(alldata)):
        alldata[i]=alldata[i].split()
        
    allsubjdata = []
    subj = 'x'
    subjindex = -1
    for line in alldata:
        if int(line[2]) <= n_trials:
            if (line[0] != subj):
                subj = line[0]
                allsubjdata.append([line])
                subjindex += 1
            else:
                allsubjdata[subjindex].append(line)
    
    #print allsubjdata
    return allsubjdata




#Fits = []
#files = os.listdir(dir)

def get_aic(nllh, n_params):
    aic = 2*nllh + 2*n_params
    return aic




f = open("modeling results/EBM_always_switch_sims_fits.txt", "w+")

f.write('subj ebm_aic ebm_Bev ebm_weight\n')

n_trials = 100


beh_data = openfile('random_always_switch_sims.txt', n_trials)

for subj_data in beh_data[:]:
	
		subj = subj_data[0][0]
	
		#[eb_nllh, eb_Bev, eb_Blag] = exploration_bonus.do_fit(subj_data, 4, 6, 4)
		[ebm_nllh, ebm_Bev, ebm_weight] = exploration_bonus_mixture.do_fit(subj_data, 1, 2, 4)
	
		outputs =  [get_aic(ebm_nllh, 3), ebm_Bev, ebm_weight]
	
		f.write(subj+'\t')
		for i in outputs:
			f.write(str(i)+'\t')
		f.write('\n')
		f.flush()


