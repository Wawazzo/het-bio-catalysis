# Python program to remove line breaks from FASTA files

import csv #package for reading and writing CSV files
import os #package for interacting with the operating system

os.chdir("C:/Users/arizzo/Downloads/uniprotkb_hydrolase_AND_taxonomy_id_833_2025_05_14") #change the working directory to the specified path

def reformat_fasta(input_file, output_file):
    with open(input_file, 'r') as infile, open(output_file, 'w') as outfile:
        sequence_lines = []
        header = None

        for line in infile:
            line = line.strip()
            if line.startswith('>'):
                # Write previous entry if present
                if header:
                    outfile.write(header + '\n')
                    outfile.write(''.join(sequence_lines) + '\n')
                header = line
                sequence_lines = []
            else:
                sequence_lines.append(line)
        
        # Write last entry
        if header:
            outfile.write(header + '\n')
            outfile.write(''.join(sequence_lines) + '\n')

# Use it
reformat_fasta('uniprotkb_hydrolase_AND_taxonomy_id_833_2025_05_14.fasta', 'output.fasta') #change the name of the input file (with extension) and the output file you want (with .fasta extension)
