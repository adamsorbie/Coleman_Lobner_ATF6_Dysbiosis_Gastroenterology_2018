
# coding: utf-8

# In[1]:

import pandas as pd 


# In[2]:

otu_table = 'merged_otu.tab'
map_file = 'merged_map.tab'
sample_ids = 'sample_ids.txt'
timepoint_1 = 'wk1.txt'
status = 'Phenotype'
wk_5 = "5wk.txt"

# set input variables

otu = pd.read_table(otu_table , sep='\t', index_col=0)
metadata = pd.read_table(map_file ,sep='\t', index_col=0) 
ids = pd.read_table(sample_ids, sep='\t')
faecal_1 = pd.read_table(timepoint_1, sep='\t')
w5 = pd.read_table(wk_5, sep='\t')
# load data and metadata 


# In[3]:

sum_row = otu.sum()
min_sum = min(otu.sum())
normalise = otu * 100 / sum_row 

# normalise data: relative abundance - multiply each row by 100 and divide by the sum of the row


# In[4]:

non_zero_columns = normalise.astype(bool).sum(axis=1) 

# drop rows which are zero in more than 25% of columns

otu_filtered = normalise[(normalise > 0).sum(axis=1) >= 54]

otu_filtered.shape


# In[5]:

# drop associated samples 

id_list = ids['#SampleID'].tolist()
train_wk5 = w5['#SampleID'].tolist()
association = faecal_1['#SampleID'].tolist()

# create list of wk1 samples 

training = [x for x in id_list if x in train_wk5]
# filter id_list so dataset only includes wk5 samples


test_train = otu_filtered[training]
# create filtered df to train on 
test_train.to_csv("5wk_otu.csv", sep=',',encoding='utf-8')

metadata_trans = metadata.transpose()
metadata_asc = metadata_trans[training]
metadata_asc = metadata_asc.transpose()
metadata_asc.to_csv("metadata_asc.csv", sep=',',encoding='utf-8')

wk1 = [x for x in id_list if x in association]
wk1_predict = otu_filtered[wk1]
wk1_predict.to_csv("association_to_predict.csv", sep=',', encoding='utf-8')


# In[ ]:



