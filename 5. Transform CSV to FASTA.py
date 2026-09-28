# Python program to convert CSV to FASTA

import csv #package for reading and writing CSV files
import os #package for interacting with the operating system

os.chdir(r"Y:\Andrea\enzymes data\UMG_SP_2\Foldseek") #change the working directory to the specified path

# Read the CSV content into a list
with open('new sequences csv.csv', newline='') as csvfile: #change the name of the input file (with extension)
    spamreader = csv.reader(csvfile, delimiter=';', quotechar='|')
    rows = list(spamreader)  # Save all rows to reuse later

# Print the rows
for row in rows:
    print(', '.join(row))

# Write to FASTA
with open('new sequences csv.fasta', 'w') as fasta_file: #change the name of the output file you want (with .fasta extension)
    for row in rows:
        if len(row) >= 2:
            header = row[0]
            sequence = row[1]
            fasta_file.write(f">{header}\n{sequence}\n")
