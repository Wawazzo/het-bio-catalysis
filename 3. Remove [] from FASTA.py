# Python program to remove square brackets from FASTA header names.

import os #package for interacting with the operating system

os.chdir(r"C:\Users\arizzo\Desktop\carpeta\python\fasta brackets names") # change the working directory to the specified path

def extract_names_and_rename(input_fasta, output_fasta): # Function to extract names from square brackets and rename headers
    with open(input_fasta, 'r') as infile, open(output_fasta, 'w') as outfile:
        for line in infile:
            if line.startswith('>'):
                # Extract the content between square brackets
                start = line.find('[')
                end = line.find(']')
                if start != -1 and end != -1:
                    new_name = line[start+1:end].strip()
                    outfile.write(f">{new_name}\n")
                else:
                    raise ValueError("No square brackets found in header line.")
            else:
                outfile.write(line)

extract_names_and_rename('banana.fasta', 'mela.fasta') # change the input and output file names as needed