import pandas as pd

df = pd.read_csv('/home/tellgreenpo/Documents/Insa/5A/r/projet/dataset/varmod_postes_2020.csv',sep=';')
count = df = df.groupby(['COD_VAR']).size()
print(len(count))
