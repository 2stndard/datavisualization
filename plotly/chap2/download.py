
import pandas as pd
import pypistats
import plotly.express as px
data_plotly = pypistats.overall("plotly", total=True, format="pandas")
data_seaborn = pypistats.overall("seaborn", total=True, format="pandas")
data_bokeh = pypistats.overall("bokeh", total=True, format="pandas")
data_hvplot = pypistats.overall("hvplot", total=True, format="pandas")

data_plotly = data_plotly.groupby("category").get_group("without_mirrors").sort_values("date").loc[:, ['date', 'downloads']].rename(columns={'downloads':'plotly'})
data_seaborn = data_seaborn.groupby("category").get_group("without_mirrors").sort_values("date").loc[:, ['date', 'downloads']].rename(columns={'downloads':'seaborn'})
data_bokeh = data_bokeh.groupby("category").get_group("without_mirrors").sort_values("date").loc[:, ['date', 'downloads']].rename(columns={'downloads':'bokeh'})
data_hvplot = data_hvplot.groupby("category").get_group("without_mirrors").sort_values("date").loc[:, ['date', 'downloads']].rename(columns={'downloads':'hvplot'})

data = pd.merge(left = data_plotly , right = data_seaborn, how = "inner", on = "date")
data = pd.merge(left = data , right = data_bokeh, how = "inner", on = "date")
data = pd.merge(left = data , right = data_hvplot, how = "inner", on = "date")

fig = px.line(data, x="date", y=data.columns, 
              title='최근 6개월간 패키지 다운로드수',
              labels=dict(value="다운로드수", date="Date")
)

fig.update_layout(
    legend_title="패키지 이름",
    )

fig.show()


config = {
  'toImageButtonOptions': {
    'format': 'svg', # one of png, svg, jpeg, webp
    'filename': 'custom_image',
    'height': 500,
    'width': 700,
    'scale': 1 # Multiply title/legend/axis/canvas sizes by this factor
  }
}

fig.show(config = config)
