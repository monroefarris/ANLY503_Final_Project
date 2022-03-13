import plotly.graph_objects as go
import pandas as pd

accident_df = pd.read_csv('./data/1975/cleaned-accident.csv')
accident_df = accident_df[accident_df['WeatherCondition'] != 1.0]
state_df = accident_df.groupby(['StateAbbv', 'WeatherCondition']).size().reset_index(name='counts')
print(state_df)
state_max_df = state_df.groupby('StateAbbv')['counts'].max().reset_index(name='count_max')
print(state_max_df)
final_df = state_max_df.merge(state_df, how='inner', left_on=['StateAbbv', 'count_max'], right_on=['StateAbbv', 'counts'])
print(final_df)
exit()

fig = go.Figure(data=go.Choropleth(
    locations=state_df['StateAbbv'],  # Spatial coordinates
    z=state_df['counts'].astype(float),  # Data to be color-coded
    locationmode='USA-states',  # set of locations match entries in `locations`
    colorscale='Reds',
    colorbar_title="Number of Accidents",
))

# df = px.data.election()
# geojson = px.data.election_geojson()
#
# fig = px.choropleth(df, geojson=geojson, color="winner",
#                     locations="district", featureidkey="properties.district",
#                     projection="mercator", hover_data=["Bergeron", "Coderre", "Joly"]
#                    )
# fig.update_geos(fitbounds="locations", visible=False)
# fig.update_layout(margin={"r":0,"t":0,"l":0,"b":0})
# fig.show()

fig.update_layout(
    title_text='Number of Fatal Car Crashes (1975)',
    geo_scope='usa',  # limite map scope to USA
)

fig.show()
