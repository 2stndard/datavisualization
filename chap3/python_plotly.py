import pandas as pd
from dfply import *
  import plotly.express as px
import plotly.graph_objects as go

df_입학생_p = pd.read_excel('C:/R/git/datavisualization/chap3/2021_연도별_입학자수.xlsx', sheet_name='Sheet0', header=(1, 2), nrows=400)
df_입학생_p = df_입학생_p >> select(0, 1, 2, 4, 8, 28, 30)
df_입학생_p.columns = ['연도', '시도', '전체', '전문대', '일반대', '석사', '박사']

fig = px.line(x=["a","b","c"], y=[1,3,2], title="sample figure")
print(fig)


list_연도 = df_입학생_p.loc[df_입학생_p['시도'] == '전체' , '연도']

list_일반대 = df_입학생_p.loc[df_입학생_p['시도'] == '전체' , '일반대']

list_전문대 = df_입학생_p.loc[df_입학생_p['시도'] == '전체' , '전문대']

list_석사 = df_입학생_p.loc[df_입학생_p['시도'] == '전체' , '석사']

list_박사 = df_입학생_p.loc[df_입학생_p['시도'] == '전체' , '박사']


fig = pe.line(x = list_연도, y = list_일반대)

fig.show()


fig1 = go.Figure()
fig1.add_trace(go.Scatter(x = list_연도, y = list_일반대, mode='lines+markers', name = '일반대'))
fig1.add_trace(go.Scatter(x = list_연도, y = list_전문대, mode='lines+markers', name = '전문대'))
fig1.add_trace(go.Scatter(x = list_연도, y = list_석사, mode='lines+markers', name = '석사'))
fig1.add_trace(go.Scatter(x = list_연도, y = list_박사, mode='lines+markers', name = '박사'))
fig1.show()


# import seaborn as sns
# import matplotlib.pyplot as plt
# import matplotlib.font_manager as fm
font_list = [font.name for font in fm.fontManager.ttflist]
font_list

df_입학생.head()
df_입학생.info()
df_입학생.describe()
df_입학생.loc[1:5]
df_입학생.shape[0]

df_입학생.index

df_입학생_전체 = df_입학생 >> filter_by(X.시도 == '전체')
df_입학생_전체.info()
plt.rcParams['axes.unicode_minus'] = False
plt.rcParams['font.family'] = 'Malgun Gothic'
sns.lineplot(x='연도', y='전체', data=df_입학생_전체)
plt.show()

