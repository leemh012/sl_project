from selenium import webdriver
import pandas as pd
from bs4 import BeautifulSoup
import time, math

driver = webdriver.Chrome(executable_path=r'C:/Users/LeeMH/Downloads/chromedriver/chromedriver.exe')
driver = webdriver.PhantomJS('C:/Users/LeeMH/Downloads/chromedriver/phantomjs/phantomjs-2.1.1-windows/bin/phantomjs')
driver.implicitly_wait(3)
driver.get('https://www.airkorea.or.kr/web/caiCalculator?pMENU_NO=147')
data = pd.read_csv("final_data2.csv", encoding="CP949")
value = data.iloc[:,41:47]

cai_list = []
cai_grade_list = []
cai_item_list = []
for i in range(value.shape[0]):
    #g10001:아황산, g10002:일산화탄소, g10003: 오전, g10006:이산화 질소, g10007: PM10, g10008: PM2.5
    if i % 100 == 0:
        print(i)
    driver.find_element_by_name('g10001').send_keys(str(round(value.loc[i,"SO2"],3)))
    driver.find_element_by_name('g10002').send_keys(str(round(value.loc[i,"CO"],1)))
    driver.find_element_by_name('g10003').send_keys(str(round(value.loc[i,"O3"],3)))
    if math.isnan(float(value.loc[i,"NO2"])) == True:
        driver.find_element_by_name('g10006').send_keys("0")
    else:
        driver.find_element_by_name('g10006').send_keys(str(round(value.loc[i,"NO2"],3)))
    try:
        driver.find_element_by_name('g10007').send_keys(str(int(value.loc[i,"PM10"])))
    except ValueError:
        driver.find_element_by_name('g10007').send_keys("0")
    try:
        driver.find_element_by_name('g10008').send_keys(str(int(value.loc[i,"PM2.5"])))
    except ValueError:
        driver.find_element_by_name('g10007').send_keys("0")

    driver.find_element_by_xpath('//*[@id="cont_body"]/p[2]/a[1]').click()
    time.sleep(1.5)
    html = driver.page_source
    soup = BeautifulSoup(html, 'html.parser')
    cai = soup.findAll("th",{"id":"KHAI"})[0].text
    cai_grade = soup.findAll("td",{"id":"KHAI_GRADE"})[0].text
    cai_item = soup.findAll("td",{"id":"KHAI_ITEM"})[0].text
    cai_list.append(cai)
    cai_grade_list.append(cai_grade)
    cai_item_list.append(cai_item)
    driver.find_element_by_xpath('//*[@id="cont_body"]/p[2]/a[2]').click()

data['종합대기상태'] = cai_grade_list
data['종합대기지수'] = cai_list
data['주오염물질'] = cai_item_list

cai_list
cai_grade_list
cai_item_list

data.to_csv("final_data3.csv", sep=",", encoding="CP949")
