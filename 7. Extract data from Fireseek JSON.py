# Python program to extract tSeq and href from JSON file coming from Foldseek
# If this is the first time you run the script, install the packages (remove the #):
# pip install json

import json # package for working with JSON data
import os # package for interacting with the operating system
import csv # package for reading and writing CSV files

os.chdir(r"Y:\Andrea\enzymes data\Test alignment") # change the working directory to the specified path

# Apri il file JSON
with open("file.json", "r", encoding="utf-8") as f:
    json_data = json.load(f)

# Funzione per estrarre tSeq e href
def estrai_dati(json_data):
    risultati = []
    # json_data è una lista, quindi cicliamo direttamente su di essa
    for item in json_data:
        tSeq = item.get("tSeq")
        href_list = [link.get("url") for link in item.get("href", [])]
        risultati.append({
            "tSeq": tSeq,
            "href": href_list
        })
    return risultati

# Uso della funzione
dati_estratti = estrai_dati(json_data)
for entry in dati_estratti:
    print(f"tSeq: {entry['tSeq']}, href: {entry['href']}")

for item in json_data:
    print(item)
