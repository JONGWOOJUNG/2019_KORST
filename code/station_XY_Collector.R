# -*- coding: utf-8 -*-

import json
import os
import requests
import datetime

# Request Param Setting
req_url = "https://www.bikeseoul.com/app/station/getStationRealtimeStatus.do"
req_header = { 'User-Agent' : 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/67.0.3396.87 Safari/537.' }
req_param = {
  "stationGrpSeq" : "1",
}

now = datetime.datetime.now().strftime('%Y-%m-%d %H-%M-%S')  # 현재시간
current_dir = os.getcwd()  # 현재 디렉토리 경로

fp_csv = open(current_dir + "/zones.csv", mode="w", encoding="utf-8")
fp_csv.write("CODE, NAME, X, Y" + "\n")

# Collect Stations by sequential request
idx = 0

while(idx < 60):
  idx += 1
req_param['stationGrpSeq'] = str(idx)
res = requests.get(req_url, headers=req_header, params=req_param)
res_text = res.text

res_json = json.loads(res_text)
zones = res_json['realtimeList']

if len(zones) == 0: # No zone in Square idx
  continue
else:
  for zone in zones:
  print(zone['stationName'], " X: ", zone['stationLatitude'], " Y: " , zone['stationLongitude']) #스테이션이름
name = zone['stationName']
code = name[:name.find('.')]
name = '"' + "'" + name[(name.find('.') + 1):] + "'" + '"'
fp_csv.write(code + "," + name + "," + zone['stationLatitude'] + "," + zone['stationLongitude'] + "\n")
fp_csv.close()