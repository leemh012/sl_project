import pandas as pd
import os
from datetime import datetime, time

dir_list = os.listdir()
dir_list[:-4]


file_name = './{}/data_past_day ({}).xls'

pd.read_excel(file_name.format(2014, 1)).drop([0])

make_first_data = True
for i in dir_list[:-4]:
    for j in range(len(os.listdir("./{}".format(i)))):
        print(i, j)
        if make_first_data == True:
            total_data = pd.read_excel(file_name.format(i, j)).drop([0]).sort_values(['날짜'])
            make_first_data = False
        else:
            df1 =  pd.read_excel(file_name.format(i, j)).drop([0]).sort_values(['날짜'])
            total_data = pd.concat([total_data, df1], axis=0)

total_data.columns = ['date', 'PM10', 'PM2.5', 'O3', 'NO2', 'CO', 'SO2']

total_data.to_csv("airquality.csv", sep=',')
data2 = pd.read_csv("./statistical_learning.csv", encoding="CP949")
data2["일시"] = pd.to_datetime(data2["일시"], format="%Y-%m-%d")
total_data['date'] = pd.to_datetime(total_data["date"], format="%Y-%m-%d")
data2.rename(columns={'일시':'date'}, inplace=True)
data2.columns
data2.set_index("date", inplace=True)
total_data.set_index("date", inplace=True)
total_data.shape
data2.shape
data1_index = set(total_data.index.tolist())
data2_index = set(data2.index.tolist())

analysis_data = data2.join(total_data, how='outer')
analysis_data.shape
analysis_data.to_csv("sl_data.csv", sep=',', encoding="CP949")

weather_data = pd.read_csv('final_data5.csv', encoding="CP949")
weather_data['date'] = pd.to_datetime(weather_data['date'], format="%Y-%m-%d")
weather_data.set_index("date", inplace=True)
list1 =  weather_data.iloc[:,28:33].columns.tolist()
list1

weather_data.drop(columns=list1, axis=1, inplace=True)


stock_data = pd.read_csv('stock2.csv')
stock_data.columns = ['date', '187750', '045060', '083550', '045520', '022100']
stock_data['date'] = pd.to_datetime(stock_data['date'], format="%Y%m%d")
stock_data['date'] = pd.to_datetime(stock_data['date'], format="%Y-%m-%d")
stock_data.set_index("date", inplace=True)
stock_data = stock_data
stock_data
pd.concat([weather_data, stock_data], axis=1)
len(set(weather_data.index.tolist()))


final_data2 = pd.concat([weather_data, stock_data], axis=1)

final_data2.iloc[:,32:38] = final_data2.iloc[:,32:38].fillna(method="bfill")
final_data2
final_data2.to_csv("final_data6.csv", sep=',', encoding="CP949")
