##################################### IMPORT PACKAGES #####################################

import csv
import json
import pandas as pd
import torch
from dplython import select, DplyFrame, X, arrange, count, sift, head, summarize, group_by, tail, mutate
import os.path
import requests
import zipfile

##################################### DOWNLOAD/UPLOAD #####################################

name = input("Enter file name ")

with zipfile.ZipFile(name + ".zip", 'r') as zip_ref:
    zip_ref.extractall()

colspecs = [
    (0, 1),
    (2, 6),
    (6, 8),    
    (8, 10),
    (10, 12),
    (12, 23), 
    (24, 26), 
    (27, 38), 
    (39, 48),
    (49, 51), 
    (52, 55), 
    (56, 69), 
    (69, 82), 
    (82, 95), 
    (95, 108), 
    (108, 121), 
    (121, 133), 
    (134, 146), 
    (147, 151), 
    (152, 169), 
    (170, 187), 
    (188, 200), 
    (201, 202),
    (202, 209),
    (210, 216),
    (217, 229),
    (230, 241),
    (242, -1)]

df = pd.read_fwf(name + ".TXT", 
skiprows=1,
#skipfooter=2117000, 
colspecs=colspecs, 
names=['TIPO_DE_REG', 
'DT_PREGAO_ANO', 
'DT_PREGAO_MES', 
'DT_PREGAO_DIA',
'CODBDI',
'CODNEG',
'TPMERC',
'NOMRES',
'NOME_DO_PAPEL',
'PRAZOT',
'MODREF',
'PREABE',
'PREMAX',
'PREMIN',
'PREMED',
'PREULT',
'PREOFC',
'PREOFV',
'TOTNEG',
'QUATOT',
'VOLTOT',
'PREEXE',
'INDOPC',
'DATVEN',
'FATCOT',
'PTOEXE',
'CODISI',
'DISMES'
])

print(df)

##################################### TRANSFORM #####################################

#df['DT_PREGAO'] = df['DT_PREGAO_ANO'] + "/" + df['DT_PREGAO_MES'] + "/" + df['DT_PREGAO_DIA']

df = DplyFrame(df) >> mutate(DT_PREGAO = X.DT_PREGAO_ANO + "-" + X.DT_PREGAO_MES + "-" + X.DT_PREGAO_DIA)

print(df)

##################################### EXPORT #####################################

df.to_csv('base.csv',index=False,header=True)


