# Script to merge all CSV files in a folder side by side and save as an Excel file
# If you never ran this script before, you may need to install the required libraries:
# pip install pandas

import os # Package for interacting with the operating system
import glob # Package for finding files and directories matching a specified pattern
import pandas as pd # Package for data manipulation and analysis

def merge_all_csv_side_by_side(input_folder):
    # Find all CSV files in the specified folder
    filenames = sorted(glob.glob(os.path.join(input_folder, '*.csv')))

    if not filenames:
        print("No CSV files found in the folder.")
        return

    # List to hold DataFrames for each CSV file
    dfs = []

    for file in filenames:
        df = pd.read_csv(file, header=None)
        # Rename columns with file name to avoid conflicts
        name = os.path.splitext(os.path.basename(file))[0]
        df.columns = [f"{name}_Col{i+1}" for i in range(df.shape[1])]
        dfs.append(df)

    # Concatenate all DataFrames side by side
    merged_df = pd.concat(dfs, axis=1)

    # Save as Excel
    output_path = os.path.join(input_folder, "merged_all.xlsx")
    merged_df.to_excel(output_path, index=False)

    print(f"Merged XLSX saved to {output_path}")

input_folder = r"C:\Users\andrearizzo\Desktop\ \Work\Carpeta temporal" # Change this to your folder path containing CSV files
merge_all_csv_side_by_side(input_folder)