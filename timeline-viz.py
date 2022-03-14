import plotly.express as px
import pandas as pd

accident_df = pd.read_csv('./data/1975/cleaned-accident.csv')
accident_df = accident_df[accident_df['WeatherCondition'] != 1.0]
state_df = accident_df.groupby(['StateAbbv', 'Month', 'WeatherCondition']).size().reset_index(name='counts')
print(state_df)
state_max_df = state_df.groupby(['StateAbbv', 'Month'])['counts'].max().reset_index(name='count_max')
print(state_max_df)
final_df = state_max_df.merge(state_df, how='inner', left_on=['StateAbbv', 'Month', 'count_max'], right_on=['StateAbbv', 'Month', 'counts'])
print(final_df)

fig = px.choropleth(
    locations=final_df['StateAbbv'],  # Spatial coordinates
    color=final_df['WeatherCondition'],  # Data to be color-coded
    locationmode='USA-states',  # set of locations match entries in `locations`,
    color_discrete_map={'Clear': 'green',
                        'Rain': 'blue',
                        'Sleet': 'lightblue',
                        'Snow': 'white',
                        'Fog, Smog, Smoke': 'grey',
                        'Severe Crosswinds': 'yellow',
                        'Blowing Sand, Soil, Dirt': 'red',
                        'Other': 'brown',
                        'Cloudy': 'pink',
                        'Freezing Rain': 'purple'},
    animation_frame=final_df['Month'],
    custom_data=[final_df['WeatherCondition'], final_df['StateAbbv'], final_df['count_max']]
)

fig.update_layout(
    title_text='Number of Fatal Car Crashes (1975)',
    geo_scope='usa'
)
fig.update_traces(
    hovertemplate=
    '<b>Weather Condition</b>: %{customdata[0]}<br>' +
    '<b>State</b>: %{customdata[1]}<br>' +
    '<b>Number of Accidents</b>: %{customdata[2]}</b>',
)

fig.show()
