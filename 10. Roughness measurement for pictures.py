# Script to analyze macroscopic roughness of SEM images using local standard deviation.
# Change parameters at the end of the script to suit your needs.

# If this is the first time you run the script, install the packages (remove the #):
# pip install opencv-python numpy matplotlib scikit-image

import os # package for interacting with the operating system
import cv2 # package for image processing
import numpy as np # package for numerical operations
import matplotlib.pyplot as plt # package for plotting
from skimage.filters import gaussian # package for Gaussian filtering
from skimage.util import img_as_ubyte # package for converting images to 8-bit unsigned byte format
from matplotlib import cm, colors # package for colormaps and color normalization


# --- Script definition for one image ---
def analyze_macroscopic_roughness(image_path, window_size=50, crop_bottom=132, sigma=5):
    img = cv2.imread(image_path, cv2.IMREAD_GRAYSCALE)
    if img is None:
        raise FileNotFoundError(f"Cannot open {image_path}")
    original = img.copy()

    # --- Crop bottom bar ---
    if crop_bottom > 0:
        img_analysis = img[:-crop_bottom, :]
        bottom_bar = img[-crop_bottom:, :].copy()
    else:
        img_analysis = img
        bottom_bar = None

    img_analysis = img_as_ubyte(img_analysis)

    # --- Gaussian smoothing ---
    img_smooth = gaussian(img_analysis, sigma=sigma)

    # --- Local roughness ---
    h, w = img_smooth.shape
    rough_map = np.zeros_like(img_smooth)
    for i in range(0, h - window_size, window_size):
        for j in range(0, w - window_size, window_size):
            window = img_smooth[i:i+window_size, j:j+window_size]
            rough_map[i:i+window_size, j:j+window_size] = np.std(window)

    # --- Normalisation ---
    rough_map = (rough_map - rough_map.min()) / (rough_map.max() - rough_map.min() + 1e-8)
    global_roughness = np.mean(rough_map)

    # --- Heatmap ---
    rough_rgb = plt.cm.inferno(rough_map)[:, :, :3]

    # Add back bottom bar
    if bottom_bar is not None:
        bottom_rgb = np.stack([bottom_bar]*3, axis=2) / 255.0
        combined_rgb = np.vstack((rough_rgb, bottom_rgb))
    else:
        combined_rgb = rough_rgb


    # --- Save ONLY heatmap image, with global roughness in the title ---
    fig, ax = plt.subplots(figsize=(6, 6))
    ax.imshow(combined_rgb)
    ax.set_title(f"Macroscopic roughness (index={global_roughness:.3f})", fontsize=12)
    ax.axis('off')

    # Colorbar
    mappable = cm.ScalarMappable(norm=colors.Normalize(vmin=0, vmax=1), cmap='inferno')
    mappable.set_array([])
    cbar = fig.colorbar(mappable, ax=ax, fraction=0.046, pad=0.04)
    cbar.set_label('Roughness level')

    plt.tight_layout()

    out_img = os.path.splitext(image_path)[0] + "_macro_roughness_map.png"
    plt.savefig(out_img, dpi=600, bbox_inches='tight')
    plt.close(fig)

    # --- Save histogram data to TXT (tab-separated) ---
    hist, bin_edges = np.histogram(rough_map.flatten(), bins=200)

    # compute bin centers instead of edges
    bin_centers = (bin_edges[:-1] + bin_edges[1:]) / 2

    out_txt = os.path.splitext(image_path)[0] + "_roughness_histogram.txt"
    with open(out_txt, "w") as f:
        f.write("bin_center\tcount\n")
        for i in range(len(hist)):
            f.write(f"{bin_centers[i]:.6f}\t{hist[i]}\n")

    print(f"✅ {os.path.basename(image_path)} → roughness = {global_roughness:.3f}")
    print(f"   Heatmap saved: {out_img}")
    print(f"   Histogram data saved: {out_txt}\n")

    return global_roughness



# --- Batch processing ---
def batch_analyze_folder(folder_path, extensions=(".tif", ".png", ".jpg"), **kwargs):
    print(f"🔍 Scanning folder: {folder_path}\n")

    for file in os.listdir(folder_path):
        if file.lower().endswith(extensions):
            image_path = os.path.join(folder_path, file)
            try:
                analyze_macroscopic_roughness(image_path, **kwargs)
            except Exception as e:
                print(f"⚠️ Error processing {file}: {e}")

    print("📊 Analysis complete.\n")



# --- Script execution ---
if __name__ == "__main__":
    folder = r"Y:\Andrea\SEM\2025 10 27 - SEM CIPU foam\Roughness test" # change the path to your folder containing SEM images
    batch_analyze_folder(
        folder_path=folder,
        window_size=10,    # <-- Size of the local window. 10 pixels (10x10 square) is a good starting point.
        crop_bottom=132,   # <-- Pixels to crop from bottom (e.g., to remove scale bar). Set to 0 to disable, set 132 for SEM images.
        sigma=2            # <-- Gaussian blur sigma. Helps reduce noise before calculating roughness. 2 is a good starting point.
    )
