# Python program to reduce the number of sequences in a FASTA file by randomly selecting a specified number of sequences and saving them to a new FASTA file.
# Need to install biopython if not already installed:
# pip install biopython

import random # package for generating random numbers and selections
from Bio import SeqIO # package for reading and writing sequence files
import os #package for interacting with the operating system

os.chdir(r"Y:\Andrea\enzymes data\UMG_SP_2\new urethanases") #change the working directory to the specified path

# -------- Configuration --------
input_fasta = "all_urethanases.fasta"  # input FASTA file
output_fasta = "subset.fasta"      # output FASTA file
num_sequences = 300                # number of sequences to extract

# -------- Reading sequences --------
sequences = list(SeqIO.parse(input_fasta, "fasta"))

# Check if the requested number of sequences is available
if num_sequences > len(sequences):
    raise ValueError(f"Requested {num_sequences} sequences, but the file contains only {len(sequences)}")

# -------- Random Sampling --------
subset = random.sample(sequences, num_sequences)

# -------- Writing to File --------
SeqIO.write(subset, output_fasta, "fasta")

print(f"Created {output_fasta} with {num_sequences} sequences extracted randomly.")
