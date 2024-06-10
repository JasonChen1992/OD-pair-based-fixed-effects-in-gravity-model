## Addressing the Fixed Effects in Gravity Model Based on Higher-Order Origin-Destination Pairs

### Abstract
The gravity model is a classical spatial interaction model for estimating geographical flows between regions, such as trade and migration. A key to enhancing the model performance is addressing the fixed effects (FE) to capture unobserved group-based characteristics. Traditionally, this is done by grouping flows sharing the same origin or destination. However, this approach ignores the directionality and spatial heterogeneity of FE. In this study, we propose a new method that addresses FE of flows based on higher-order origin-destination (OD) pairs. Specifically, we estimate FE by grouping flows that share the same OD dyad at an upper geographic level. We conducted two case studies using U.S. migration flows and for-hire vehicle trips in New York City. The results demonstrate that our method can enhance model performance and reveal the directionality, reciprocity, regionality, and temporal regularity of geographical flows.

#Read this instruction carefully before running the codes.

### Requirements
- **R and Python** are required to run the codes.
- **ArcGIS Pro** is required for reproducing some figures.
- **Photoshop** is used to enhance the layout and add necessary labels and elements. (option)

### Data and Code Organization
- **Figure 2 and Figure 5**: Drawn using ArcGIS Pro. Data and shapefiles required for these figures are provided.
- **Figure 3 and Figure 6**: Derived using Python. The necessary code and data can be found in the folder `forHeatMatrix`, extracted from `ForHeatMatrix&Shapefile.zip`.
- **Other Figures and Tables**: Generated using R. To improve reproducibility, ready-to-use data are provided and can be directly used in the code.

### Running the Code
1. **Python**: Navigate to the `forHeatMatrix` folder and run the provided scripts to generate Figure 3 and Figure 6.
2. **R**: Follow the instructions in the provided R scripts to generate other figures and tables. Ensure that the ready-to-use data is correctly referenced in the scripts.
3. **ArcGIS Pro**: Use the provided data and shapefiles to reproduce Figure 2 and Figure 5.
4. **Photoshop**: Enhance the figures as necessary, adding labels and elements such as black squares in Figure 3 and the reference category polygon in Figures 3 & 6.

### Notes
- The study involves using a new method to address fixed effects in the gravity model by grouping flows based on higher-order origin-destination pairs.
- Two case studies are provided: US migration flows and for-hire vehicle trips in New York City.
- The results demonstrate that the new method enhances model performance and reveals directional, reciprocal, regional, and temporal patterns of geographical flows.



###References
- Yuzhou Chen, Qiwei Ma & Ran Tao (26 Apr 2024): Addressing the fixed effects in gravity model based on higher-order origin-destination pairs, International Journal of Geographical Information Science, DOI: 10.1080/13658816.2024.2343763



