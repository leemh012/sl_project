import requests
import pandas as pd

req = requests.get("http://berkeleyearth.lbl.gov/air-quality/maps/cities/China/Beijing/Beijing.json")
json_data = req.json()
json_data.keys()

day_data = json_data['day_data']
data1 = pd.DataFrame(day_data,columns=['date','raw_data'])
data1['china_PM25'] = data1['raw_data'].apply(lambda x:x[1])
data1.drop(['raw_data'], axis=1, inplace=True)
data1.to_csv('china_pm25.csv')

data1['date'] = pd.to_datetime(data1['date'])
data1.set_index('date', inplace=True)

data2 = pd.read_csv("./final_data4.csv", encoding="CP949")
data2['date'] = pd.to_datetime(data2['date'])
data2.set_index("date", inplace=True)

data1 = data1[data1.index[229]:data1.index[1733]]
data3 = pd.concat([data2, data1], axis=1)
data3['china_PM25'] = data3['china_PM25'].shift(1)

data3.drop(data3.index[0], axis=0, inplace=True)

data3.to_csv("final_data5.csv", sep=",", encoding="CP949")
