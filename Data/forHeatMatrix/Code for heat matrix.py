import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np
import matplotlib.colors as mcolors
import matplotlib as mpl

#Load Taxi data
'''data1 = pd.read_csv(".../forHeatMatrix/BorderMatrix_Taxi_GM_OD.csv", index_col=0)
data2 = pd.read_csv(".../forHeatMatrix/TaxiZone_region&division.csv")
data3 = pd.read_csv(".../forHeatMatrix/Taxi_GM_OD_summary_results.csv")'''
#Load County data
data1 = pd.read_csv(".../forHeatMatrix/BorderMatrix_Migration_GM_OD.csv", index_col=0)
data2 = pd.read_csv(".../forHeatMatrix/US_region&division.csv")
data3 = pd.read_csv(".../forHeatMatrix/Migration_GM_OD_summary_results.csv")

# Pivot data3 to match the structure of the heatmap
significance = data3.pivot(index='From', columns='To', values='Significance')
#print(significance)
# Extract the order of states from data2
state_order = data2['State'].tolist()

# Reorder rows and columns in significance to match data1
significance = significance.reindex(state_order)
significance = significance[state_order]
# Convert all values in 'significance' to strings
significance = significance.astype(str)
# Replace 'NA' with '' (an empty string)
significance = significance.replace('nan', '')
# Reorder rows and columns in data1
data1 = data1.reindex(state_order)
data1 = data1[state_order]

# Create a colormap that assigns a color to NaN values
cmap = mcolors.LinearSegmentedColormap.from_list(
    "mycmap", [(0, "red"), (0.5, "white"), (1, "green")]
)

# Customize seaborn appearance
sns.set(font_scale=1.3)  # Set font size

# Create a figure and axes
fig, ax = plt.subplots()

# Set annotation size
mpl.rcParams['font.size'] = 14
# Create heatmap
heatmap = sns.heatmap(
    data1, # Your data
    annot=significance,# Display data values
    annot_kws={"color": 'black'},
    center=0,
    fmt='s',
    linewidths=0,
    #linecolor="white",
    vmin=-2,
    vmax=2,
    xticklabels=True,
    yticklabels=True,
    square=True,
    cbar=False,
    cbar_kws={"shrink": .5},
    cmap=cmap,
    ax=ax
)

# Add a diagonal line
plt.plot([0, data1.shape[1]], [0, data1.shape[0]], color='r', linewidth=1)

# Create a new axes for the colorbar
cbar_ax = fig.add_axes([0.8, 0.73, 0.01, 0.25])  # [left, bottom, width, height]

# Add the colorbar to the new axes
cb = fig.colorbar(ax.collections[0], cax=cbar_ax)

#move y-axis to the right
heatmap.yaxis.tick_right()
labels = heatmap.get_yticklabels()
heatmap.set_yticklabels(labels, rotation=0)

# Add division lines (close these four rows of code when running the Taxi matrix)
divisions = [12, 21, 38]
for div in divisions:
    heatmap.axhline(div, color='white', linewidth=4)
    heatmap.axvline(div, color='white', linewidth=4)


# Display the figure
plt.show()