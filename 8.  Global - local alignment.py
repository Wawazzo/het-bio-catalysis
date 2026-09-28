# Python program to compute global and local alignment similarities between a query sequence and multiple target sequences.

# Need to install biopython and pandas if not already installed:
# pip install biopython pandas

import os # package for interacting with the operating system
from Bio import SeqIO # package for reading and writing sequence files
from Bio.Align import PairwiseAligner, substitution_matrices # package for pairwise sequence alignment
import pandas as pd # package for data manipulation and analysis

# === 1. Set working directory ===
os.chdir(r"Y:\Andrea\enzymes data\UMG_SP_2\new urethanases") # change the working directory to the specified path

# === 2. Load sequences ===
queries = list(SeqIO.parse("AbPURase.fasta", "fasta")) # change the name of the query file (with .fasta extension)
targets = list(SeqIO.parse("all_urethanases + additions.fasta", "fasta")) # change the name of the target file (with .fasta extension)

# === 3. Substitution matrix creation ===
matrix = substitution_matrices.load("BLOSUM62") # set to any matrix you want. BLOSUM62 is the most used. Others are BLOSUM50, PAM250, etc...

# === 4. Gap penalties. Default values are -10 and -0.5 ===
gap_open = -10 # cost of opening a gap
gap_extend = -0.5 # cost of extending a gap

# === 5. Setup global and local aligners ===
aligner_global = PairwiseAligner()
aligner_global.mode = 'global'
aligner_global.substitution_matrix = matrix
aligner_global.open_gap_score = gap_open
aligner_global.extend_gap_score = gap_extend

aligner_local = PairwiseAligner()
aligner_local.mode = 'local'
aligner_local.substitution_matrix = matrix
aligner_local.open_gap_score = gap_open
aligner_local.extend_gap_score = gap_extend

# === 6. Define site residues (manual, 1-based indices) ===
site_residues_indices = [38, 9, 12, 13, 35, 197, 271, 277, 279, 24, 25, 27]

# === 7. Compute similarities ===
results = []

for t in targets:
    row = {"Target_ID": t.id}
    seq2 = str(t.seq)

    for q in queries:
        seq1 = str(q.seq)

        # ---- Global percent identity ----
        length_global = min(len(seq1), len(seq2))
        identities_global = sum(a == b for a, b in zip(seq1, seq2))
        percent_global = identities_global / length_global if length_global > 0 else 0

        # ---- Local percent identity ----
        alignment_local = aligner_local.align(seq1, seq2)[0]
        blocks_q, blocks_t = alignment_local.aligned
        identities_local = 0
        length_local = 0
        for (start_q, end_q), (start_t, end_t) in zip(blocks_q, blocks_t):
            block_q = seq1[start_q:end_q]
            block_t = seq2[start_t:end_t]
            length_local += len(block_q)
            identities_local += sum(a == b for a, b in zip(block_q, block_t))
        percent_local = identities_local / length_local if length_local > 0 else 0

        # ---- Site similarity using BLOSUM62 with per-residue normalization ----
        site_score = 0
        for idx in site_residues_indices:
            if idx <= len(seq1) and idx <= len(seq2):
                res1 = seq1[idx-1]
                res2 = seq2[idx-1]
                score = matrix.get((res1,res2), matrix.get((res2,res1), 0))
                max_score = matrix.get((res1,res1), 0)
                score_norm = score / max_score if max_score > 0 else 0
                site_score += max(0, score_norm)
        percent_site = site_score / len(site_residues_indices) if len(site_residues_indices) > 0 else 0

        # ---- Add to row with column names per query ----
        row[f"{q.id}_Global"] = percent_global
        row[f"{q.id}_Local"] = percent_local
        row[f"{q.id}_Site"] = percent_site

    results.append(row)

# === 8. Create DataFrame ===
df = pd.DataFrame(results)

# === 9. Format values ===
for col in df.columns:
    if col != "Target_ID":
        df[col] = df[col].apply(lambda x: f"{x:.4f}".replace('.', ','))

# === 10. Save CSV ===
df.to_csv("combined_similarity_matrix.csv", sep=";", index=False) # change the name of the output file you want (with .csv extension)

print(df.head())

