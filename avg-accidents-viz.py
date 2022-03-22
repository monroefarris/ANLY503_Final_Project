#%%
import pandas as pd
import plotly.express as px
import datetime as dt
import os 

#%%
# storing home dir if needed
home_dir = os.getcwd()

# changing to data dir to read in data
data_dir = "/data"
os.chdir(os.getcwd() + data_dir)

dirs = os.listdir() # getting all years of data

#%%
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

#%%
# subsetting by every 5 years 
final_df = final_df[(pd.to_numeric(final_df['Year']) % 5 == 0)]

# converting month to datetime object and gettign month name
final_df['Month'] = pd.to_datetime(final_df['Month'], format = "%m")
final_df['Month'] = final_df['Month'].dt.strftime('%B')

#%%

fig = px.line(final_df, x="Month", y="NumAccidents", color='Year', title='Number of Fatal Accidents per Month from 1975 - 2019')
fig.show()
# saving initial static viz for reference 
# sns.lineplot(data=final_df, x="Month", y="NumAccidents", hue="Year")
# plt.xticks(rotation = 45) 
# plt.suptitle('Number of Fatal Accidents per Month from 1975 - 2019')
# #plt.show()
os.chdir(home_dir)
fig.write_html('avg_accidents.html')

# %%
