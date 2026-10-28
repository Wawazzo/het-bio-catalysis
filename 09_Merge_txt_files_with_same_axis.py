# Python program to merge multiple .txt files with two columns (X and Y) into a single tab-delimited .txt file.
# For example, two .txt files outputting from a CD or an ATR-FTIR.

import os # package for interacting with the operating system
import glob # package for file path manipulation

def merge_txt_files(input_folder):
    filenames = sorted(glob.glob(os.path.join(input_folder, '*.dpt'))) #change .txt to your file extension if it is different

    if not filenames:
        print("No .txt files found in the folder.")
        return

    shared_x = []
    all_y_columns = []
    headers = ["X"]  # First column header

    for idx, file in enumerate(filenames):
        with open(file, 'r') as f:
            lines = [line.strip().split() for line in f if line.strip()]
            x_vals = [float(pair[0]) for pair in lines]
            y_vals = [float(pair[1]) for pair in lines]

            if idx == 0:
                shared_x = x_vals
            else:
                if x_vals != shared_x:
                    raise ValueError(f"File {file} has mismatched X values.")

            all_y_columns.append(y_vals)
            headers.append(os.path.splitext(os.path.basename(file))[0])

    # Write to tab-delimited TXT file
    output_path = os.path.join(input_folder, "merged.txt") # Change the output file name as needed
    with open(output_path, 'w') as out_file:
        out_file.write('\t'.join(headers) + '\n')
        for i in range(len(shared_x)):
            row = [f"{shared_x[i]:.3f}".replace('.', ',')] + [str(col[i]).replace('.', ',') for col in all_y_columns]
            out_file.write('\t'.join(row) + '\n')

    print(f"Merged TXT saved to {output_path}")

# 🔧 Set your folder path here
input_folder = r"Y:\Andrea\ATR\2026 01 15 Aitor y Maelle\2026 01 16" # here:
merge_txt_files(input_folder)
