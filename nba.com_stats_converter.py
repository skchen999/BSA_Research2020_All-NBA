#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jan 24 21:30:10 2020

@author: RyanAlco
"""


import json as json
import pandas as pd


with open('nba_stats_2020.txt') as json_file:
    data = json.load(json_file)

players = data['log']['entries'][4]['response']['content']

data_dict = json.loads(players['text'])

data_new = data_dict['resultSets'][0]

headers = data_new['headers']
players = data_new['rowSet']


dic = {}

for i in range(0,40):
    dic[headers[i]] = [players[j][i] for j in range(0, len(players))]


df19_20 = pd.DataFrame(dic)

df19_20_final = df19_20[['PLAYER_NAME', 'PTS', 'FTA', 'TD3']]

df19_20_final.to_csv('19_20_Stats.csv')
