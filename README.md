# From pandemic to endemic: Spatial-temporal patterns of influenza incidence in a Swiss canton, 1918-1924

## Paper

submitted

## Data

The data is public available via Zenodo:
<br >
<br >
ADD

## Content of this repository

### Structure

```
.
+-- R
+-- data
+-- data_raw
+-- output

```

### `R` folder 

This folder contains all R scripts.
  
  - `data_preparation.R` : prepares the data for the analysis
  - `Figure1.R` : code to create Figure 1
  - `Figure2.R` : code to create Figure 2
  - `Figure3.R` : code to create Figure 3
  - `function_maps_jenks.R` : function of to create maps using Jenks Natural Breaks
  - `function_maps_hotspots.R` : function of the a spatial hotspot analysis 
  - `Figure4.R` : code to create Figure 4
  - `function_regression.R` : function of the robust negative binomial regression 
  - `Table2.R`  : code to create Table 2
  
### `data` folder

This folder contains the created data from `data_preparation.R` 

### `data_raw` folder
This folder contains the raw data for the analysis:
  - `Data_Spanishflu.xlsx` : "Spanish flu" data (1918-1919)
  - `Data_Influenza_Bern.xlsx` : Influenza data (1920-1924)
  - `Data_Population.xlsx` : Population for each municipality
  - `Tb_death.xlsx` : Number of tuberculosis death
  - `Fabrik_Statistik_1929.xlsx` : Information about the factories
  - `Cofactors_1918.csv` : Co-factors for regression analysis

### `output` folder

This folder contains all outputs.

### `master.R` 

This skript contains information of the used R packages, R scripts, plotting parameters etc.

[![CC BY-NC-SA 4.0][cc-by-nc-sa-image]][cc-by-nc-sa]

[cc-by-nc-sa]: http://creativecommons.org/licenses/by-nc-sa/4.0/
[cc-by-nc-sa-image]: https://licensebuttons.net/l/by-nc-sa/4.0/88x31.png

