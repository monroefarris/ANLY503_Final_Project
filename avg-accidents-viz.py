import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import os 

# storing home dir if needed
home_dir = os.getcwd()

# changing to data dir to read in data
data_dir = "/data"
os.chdir(os.getcwd() + data_dir)

dirs = os.listdir() # getting all years of data

final_df = pd.DataFrame()
for dir in dirs: # looping over all years of data
    if not dir.startswith('.'):
        viz_data = pd.DataFrame() # create new df 
        print('Reading in {} data'.format(dir))
        x = pd.read_csv(dir + '/combined-data.csv', low_memory=False) # read in tidy data for year of interes
        year = dir # storing year as a var
        x = x[['CaseId', 'Month']] # subsetting to only include vars needed in this viz
        x['Year'] = year # adding year to df to identify 
        final_df = final_df.append(x) # appending each year to final viz dataframe
# grouping by month and counting number of unique id's to get a count of number of accidents for that month
final_df = final_df.groupby(['Year', 'Month'])['CaseId'].nunique().reset_index().rename(columns = {'CaseId':'NumAccidents'})
#final_df = final_df.groupby(['Year', 'Month'])['CaseId'].apply(wavg, )

sns.lineplot(data=final_df, x="Month", y="NumAccidents", hue="Year")
plt.suptitle('Number of Fatal Accidents per Month from 1975 - 2019')
plt.show()