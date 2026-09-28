# BEFORE USING THIS SCRIPT, OPEN THE EXCEL FILES COMING FROM AGILENT SOFTWARE, ENABLE EDIT AND SAVE THEM. NO IDEA WHY, BUT YOU GOTTA DO IT. BESOS.
# If this is the first time you run the script, install the packages (remove the #):
# pip install pandas
# pip install openpyxl

import csv #package for reading and writing CSV files
import os #package for interacting with the operating system
import pandas as pd #package for data manipulation and analysis
from openpyxl import load_workbook #package for reading and writing Excel files

os.chdir(r"H:\HPLC agilent\HetBioCat\Idania") #change the working directory to the specified path

input_xlsx = "Abiotic Thiolysis CoAs.xlsx"  #change the name of the input file (with extension)
output_csv = "Abiotic Thiolysis CoAs.csv" #change the name of the output file you want (with .csv extension)

xls = pd.ExcelFile(input_xlsx, engine="openpyxl")

all_data = []
all_molecules = set()

for sheet in xls.sheet_names:
    df = pd.read_excel(
        xls,
        sheet_name=sheet,
        header=None
    )

    sample_name = df.iloc[2, 2]  # C3
    if pd.isna(sample_name):
        continue

    row_data = {"Sample": sample_name}

    data = df.iloc[13:, [0, 5]]  # Name (A) e Area (F)
    data.columns = ["Name", "Area"]

    for _, r in data.iterrows():
        if pd.isna(r["Name"]):
            break
        row_data[r["Name"]] = r["Area"]
        all_molecules.add(r["Name"])

    all_data.append(row_data)

df_out = pd.DataFrame(all_data)
df_out = df_out.reindex(columns=["Sample"] + sorted(all_molecules))
df_out.to_csv(output_csv, index=False, sep=";", decimal=".") #change decimal="." to decimal="," in case excel reads wrong. For spanish excel use ".", for english use ",".

print("CSV created:", output_csv)