

# SFNI Processed by AVV Metadata

This file contains the metadata of the Spanish National Forest Inventory (SFNI), processed by Aitor Vázquez Veloso using R.

💡 **SFNI** = Spanish National Forest Inventory (known as IFN, Inventario Forestal Nacional, in Spanish) 

💡 **MFE** = Spanish Forest Map (known as MFE, Mapa Forestal Español, in Spanish)

### Resources:
- 🔗 [SFNI website](https://www.miteco.gob.es/es/biodiversidad/temas/inventarios-nacionales/inventario-forestal-nacional.html) and [MFE website](https://www.miteco.gob.es/es/biodiversidad/temas/inventarios-nacionales/mapa-forestal-espana.html)
- 📚 Original SFNI data documentation for [SFNI2](https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/servicios/banco-datos-naturaleza/090471228013528a_tcm30-278472.xls), [SFNI3](https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/servicios/banco-datos-naturaleza/documentador_bdcampo_ifn3_tcm30-282240.pdf), and [SFNI4](https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/temas/inventarios-nacionales/ifn/ifn4/documentador_ifn4_campo_tcm30-536595.pdf).

⚠ **Note**: The metadata structure will follow a consistent format across all fields. All columns are common across the three SFNI versions used, though some may have different codes as specified in each case.

- **variable_name**: Explanation of the variable name [units] (*special comments about the variable*)

### ⚠ Issues Found During Data Curation

- 📗 **SFNI2**
	- **Coordinates**:
		- Plot coordinates for Asturias, Cantabria, Navarra, and Baleares were incorrect. Since plots in these regions are marked as class *N* in SFNI3, their locations cannot be corrected.
		- **Granada**: 28 plots had incorrect coordinates, of which only 2 were corrected using SFNI3 coordinates (class A, subclass 1).
		- **Alicante and Albacete**: 62 plots (61 in Alicante, 1 in Albacete) had incorrect coordinates, with 34 of these corrected using SFNI3 coordinates (class A, subclass 1).
		- **Note**: Information related to plot and tree positions was removed for cases where corrections were not possible. In the final dataset, some plots appear to be located in Portugal and in the sea between the Balearic Islands and Valencia, but they are preserved in their original form.
		- Overview:
		![[02_PhD/hd_models/data/overview_sfni_plot_location_distribution.png]]
	- **Plot and Tree Information**:
		- A total of 66 trees from plots (IFN_PLOT_ID: "IFN2_5_2130", "IFN2_5_2334", "IFN2_5_2579", "IFN2_8_3600", "IFN2_8_3601", "IFN2_25_1955") lack plot information. These trees are included in the dataset, but their plot position data is missing.
		- A total of 31,156 plots contain no trees. They are included in the dataset, but estimated information fields remain empty.
		- 🔢 Initial dataset includes 93,272 plots and 853,287 trees.

- 📘 **SFNI3**
	- **Coordinates**: Plot coordinates are accurate.
	- **Merging and Duplicates**:
		- After merging, some plots with common "Origen," "Estadillo," and "Clase" values appeared duplicated but lacked a "Subclase" value. These entries were saved separately for reference and removed from the dataset to avoid duplication.
	- **Plot and Tree Information**:
		- All trees are associated with plot information.
		- A total of 15,748 plots contain no trees. These are included in the dataset, but estimated information fields remain empty.
		- 🔢 Initial dataset includes 99,047 plots and 1,398,537 trees.

- 📕 **SFNI4**
	- **Coordinates**:
		- Plot *IFN4_47_242_A_1* had incorrect coordinates, which were corrected using SFNI3 data. Tree coordinates were recalculated accordingly.
	- **Merging and Duplicates**:
		- After merging, some plots with common "Origen," "Estadillo," and "Clase" values were duplicated but lacked a "Subclase" value. These entries were saved separately for reference and removed from the dataset to avoid duplication.
	- **Plot and Tree Information**:
		- All trees are associated with plot information.
		- A total of 5,182 plots contain no trees. These are included in the dataset, but estimated information fields remain empty.
		- 🔢 Initial dataset includes 57,723 plots and 1,121,874 trees.

- 🌳🌲 **Tree Species**:
	- After data harmonization, 223,703 trees were found to belong to species with codes that do not appear in SFNI documentation. These species codes include: *5, 3, 91, 93, 4, 90, 9, 83, NA, 2, 239, 295, 297, 6, 369, 283, 269, 646, 946, 746, 846, 626, 826, 926, 726*.


---
## 🟢 Plot Data

- **INVENTORY_ID**: Unique code for each SFNI version [none]
- **Province**: Original SFNI province code [none]
- **Province_name**: Original province name [none]
- **N_plot**: Original SFNI plot code [none]
- **Class**: Original SFNI class code [none] (not available in SFNI2)
- **Subclass**: Original SFNI subclass code [none] (not available in SFNI2)
- **PLOT_ID**: Unique plot code for each SFNI version, combining province, plot, class, and subclass original codes [none] (*SFNI2 only uses province and plot codes, as it lacks class and subclass codes*)
- **PLOT_ID_short**: Code for each plot using province and plot original codes only [none] (*Can be used to match SFNI2 plots with corresponding plots in SFNI3 and SFNI4*)
- **IFN_PLOT_ID**: Unique plot code across all SFNI versions, consisting of INVENTORY_ID and PLOT_ID [none]
- **T**: Mean age of the dominant species in the plot [years]
- **sp_1**: Code for the dominant tree species in the plot (based on SFNI classification) according to basal area distribution by species [none]
- **sp_2**: Code for the secondary tree species in the plot (based on SFNI classification) according to basal area distribution by species [none]
- **sp_3**: Code for the tertiary tree species in the plot (based on SFNI classification) according to basal area distribution by species [none]
- **N**: Plot density [trees/ha]
- **N_0_75**: Plot density for trees in the diameter class from 0 to 75 cm [trees/ha]
- **N_75_125**: Plot density for trees in the diameter class from 75 to 125 cm [trees/ha]
- **N_125_175**: Plot density for trees in the diameter class from 125 to 175 cm [trees/ha]
- **N_175_225**: Plot density for trees in the diameter class from 175 to 225 cm [trees/ha]
- **N_225_275**: Plot density for trees in the diameter class from 225 to 275 cm [trees/ha]
- **N_275_325**: Plot density for trees in the diameter class from 275 to 325 cm [trees/ha]
- **N_325_375**: Plot density for trees in the diameter class from 325 to 375 cm [trees/ha]
- **N_375_425**: Plot density for trees in the diameter class from 375 to 425 cm [trees/ha]
- **N_425_**: Plot density for trees in the diameter class larger than 425 cm [trees/ha]
- **N_sp_1**: Plot density for species 1 [trees/ha]
- **N_sp_2**: Plot density for species 2 [trees/ha]
- **N_sp_3**: Plot density for species 3 [trees/ha]
- **N_alive**: Plot density for living trees [trees/ha]
- **N_dead**: Plot density for dead trees [trees/ha]
- **dg**: Plot quadratic mean diameter [cm]
- **dbh_min**: Minimum tree diameter at breast height, considering all trees in the plot [cm]
- **dbh_mean**: Mean tree diameter at breast height, considering all trees in the plot [cm]
- **dbh_max**: Maximum tree diameter at breast height, considering all trees in the plot [cm]
- **Do**: Dominant plot diameter [cm]
- **G**: Plot basal area [m²/ha]
- **g_min**: Minimum tree basal area, considering all trees in the plot [cm²]
- **g_mean**: Mean tree basal area, considering all trees in the plot [cm²]
- **g_max**: Maximum tree basal area, considering all trees in the plot [cm²]
- **G_sp_1**: Basal area for species 1 [m²/ha]
- **G_sp_2**: Basal area for species 2 [m²/ha]
- **G_sp_3**: Basal area for species 3 [m²/ha]
- **G_alive**: Basal area for living trees [m²/ha]
- **G_dead**: Basal area for dead trees [m²/ha]
- **h_min**: Minimum tree height, considering all trees in the plot [m]
- **h_mean**: Mean tree height, considering all trees in the plot [m]
- **h_max**: Maximum tree height, considering all trees in the plot [m]
- **Ho**: Dominant plot height [m]
- **slenderness**: Height-diameter ratio, based on mean height and diameter values [none]
- **dominant_slenderness**: Height-diameter ratio, based on dominant height and diameter values [none]
- **SDI**: Stand Density Index, based on the reference *r* value [trees/ha]
- **S**: Hart-Becking competition index for simple rows [none]
- **S_staggered**: Hart-Becking competition index for staggered rows [none]
- **X_UTM**: UTM X-coordinate of the plot center, using the WGS84 UTM projection system [m] (not actual values in original dataset)
- **Y_UTM**: UTM Y-coordinate of the plot center, using the WGS84 UTM projection system [m] (not actual values in original dataset)
- **Longitude**: Longitude of the plot center, using the WGS84 geographic system [degrees] (not actual values in original dataset)
- **Latitude**: Latitude of the plot center, using the WGS84 geographic system [degrees] (not actual values in original dataset)
- **zone**: UTM zone of the plot center in the WGS84 UTM projection system [none]
- **ALTITUD1**: Altitude (value 1) of the plot center, as provided in original SFNI data [m] (see SFNI documentation)
- **ALTITUD2**: Altitude (value 2) of the plot center, as provided in original SFNI data [m] (see SFNI documentation)
- **Altitude**: Average altitude of the plot center [m]
- **MAXPEND1**: Maximum slope (value 1) of the plot, as provided in original SFNI data [degrees] (see SFNI documentation)
- **MAXPEND2**: Maximum slope (value 2) of the plot, as provided in original SFNI data [degrees] (see SFNI documentation)
- **Slope_max**: Average maximum slope of the plot [degrees]
- **Stand_origin**: Origin of the stand (natural/artificial/naturalized) [none]
- **Stand_artificial_origin**: Details on the origin of the stand if artificial [none]
- **Species_mixture**: Classification of stand purity, based on basal area of the two most dominant species [none]
- **Stand_type**: Classification based on tree origin in the stand (high/medium/coppice forest) [none]
- **Age_class**: Classification by natural age class of the stand (seedling/thicket/pole/timber stage) [none]
- **Structure**: Details of common forest structures [none]
- **fito-x**: Data from fitoclimatic regions (*Mapa de Subregiones Fitoclimáticas de España Peninsular y Balear*, accessible [here](https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/mapa_subregiones_fitoclim_descargas.html))
- **biogeo-x**: Data from biogeographic regions (*Regiones Biogeográficas de España*, accessible [here](https://www.miteco.gob.es/es/cartografia-y-sig/ide/descargas/biodiversidad/regiones-biogeograficas.html))
- **veget-x**: Data from vegetation series maps (*Mapa de Series de Vegetación*, accessible [here](https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/memoria_mapa_series_veg_descargas.html))
- **climate_region**: Summary of climate regions, covering both land and sea zones (Atlántica, Mediterránea, Macaronésica, Alpina) [none]
- **ADDITIONAL_VARIABLES**: Additional variables from SFNI data, detailed in the SFNI documentation. To distinguish sources, a prefix identifies the SFNI edition (*SFNI2_*, *SFNI3_*, *SFNI4_*), with a suffix indicating the original dataset (e.g., *_pce* for *pcespparc*).


---
## 🌳 Tree Data

- **INVENTORY_ID**: Unique code for each SFNI version [none]
- **province**: Original SFNI province code [none]
- **n_plot**: Original SFNI plot code [none]
- **class**: Original SFNI class code [none] (not available in SFNI2)
- **subclass**: Original SFNI subclass code [none] (not available in SFNI2)
- **n_tree**: Original SFNI tree code [none] (in SFNI3 and SFNI4, includes all measured trees in the plot, including those from previous SFNI versions that are now harvested or dead)
- **PLOT_ID**: Unique plot code for each SFNI version, using province, plot, class, and subclass original codes [none] (*SFNI2 uses only province and plot codes as it lacks class and subclass codes*)
- **PLOT_ID_short**: Code for each plot using only province and plot original codes [none] (*Used to match SFNI2 plots with the same plot in SFNI3 and SFNI4*)
- **IFN_PLOT_ID**: Unique plot code across all SFNI versions, consisting of INVENTORY_ID and PLOT_ID [none]
- **TREE_ID**: Unique tree code for each SFNI version, using province, plot, class, subclass, and tree number original codes [none] (*SFNI2 uses only province, plot, and tree codes as it lacks class and subclass codes*)
- **TREE_ID_IFN2**: Unique tree code according to the SFNI2 tree label, using province, plot, and tree number from SFNI2 [none] (*Allows matching a tree in SFNI2 with the same tree in SFNI3*)
- **TREE_ID_IFN3**: Unique tree code according to the SFNI3 tree label, using province, plot, class, subclass, and tree number from SFNI3 [none] (*Allows matching a tree in SFNI3 with the same tree in SFNI4*)
- **TREE_ID_IFN4**: Unique tree code according to the SFNI4 tree label, using province, plot, class, subclass, and tree number from SFNI4 [none] (*Allows matching a tree in SFNI4 with future SFNI versions*)
- **IFN_TREE_ID**: Unique tree code across all SFNI versions, consisting of INVENTORY_ID and TREE_ID [none]
- **species**: Tree species code according to SFNI classification [none]
- **dbh_1**: Diameter at breast height, measurement 1 [mm]
- **dbh_2**: Diameter at breast height, measurement 2 [mm]
- **dbh**: Mean diameter at breast height [cm]
- **circumference**: Tree circumference at breast height [cm]
- **g**: Tree basal area at breast height [cm²]
- **g_ha**: Tree basal area per hectare at breast height, calculated using the expansion factor [m²/ha]
- **bal**: Cumulative basal area of trees larger than the subject tree [m²/ha]
- **h**: Total tree height [m]
- **slenderness**: Tree height-diameter ratio [none]
- **expan**: Expansion factor [none]
- **quality**: Tree quality code according to SFNI classification [none]
- **shape**: Tree cubication form according to SFNI classification [none]
- **special_param**: Tree special parameters according to SFNI classification [none]
- **dead**: Binary code for tree status (0 = alive, 1 = dead) [none]
- **bearing**: Bearing, calculated from the plot center to the tree, in centesimal degrees, starting from north and measured clockwise [g]
- **distance**: Distance from the plot center to the tree [m]
- **x_rel**: Relative X-coordinate of the tree with respect to the plot center [m]
- **y_rel**: Relative Y-coordinate of the tree with respect to the plot center [m]
- **x_utm**: X-coordinate of the tree in the WGS84 UTM projection coordinate system [m] (not actual values of the plot center provided in the original dataset)
- **y_utm**: Y-coordinate of the tree in the WGS84 UTM projection coordinate system [m] (not actual values of the plot center provided in the original dataset)
- **longitude**: Tree longitude in the WGS84 coordinate system [degrees] (not actual values of the plot center provided in the original dataset)
- **latitude**: Tree latitude in the WGS84 coordinate system [degrees] (not actual values of the plot center provided in the original dataset)
- **tree_type**: Value distinguishing between conifers and broadleaved species [none]
- **additional_variables**: Additional variables from the SFNI data, detailed in the original SFNI documentation.

---

## ☀ Climate Data 

 ℹ The database used to extract climate information was WorldClim; original data must be downloaded from its [official website](https://www.worldclim.org/data/index.html)

- 📂 **0.4.1.sfni_historic_monthly_climate_data**: historic climate data for the period 1951-2021, including minimum, maximum and mean temperature (ºC), accumulated precipitation (mm) and Martonne Aridity Index per month. 
	- ⚠ *Note: the file size was too large (1.2 GB), so the data has been split into several files. You can request the merged document via email.*
- 💾 **0.4.1.sfni_historic_yearly_climate_data.rdata**: historic annual climate data for the period 1951-2021, including minimum, maximum, and mean temperature (ºC), annual accumulated precipitation (mm) and Martonne Aridity Index
- 💾 **0.4.1.sfni_70years_period_climate_data.rdata**: historic climate data averaged for the period 1951-2021, including minimum, maximum, and mean temperature (ºC), accumulated precipitation (mm) and Martonne Aridity Index for the already mentioned period
- 💾 **0.4.1.sfni_2000-2020_period_climate_data.rdata**: historic climate data averaged for the period 2000-2020, including minimum, maximum, and mean temperature (ºC), accumulated precipitation (mm) and Martonne Aridity Index for the already mentioned period
- 💾 **0.4.1.sfni_future_climate_data_MIROC6_SSP*.rdata**: future climate data for the periods 2021-2040, 2041-2060, 2061-2080, and 2081-2100, including minimum, maximum, and mean temperature (ºC), annual accumulated precipitation (mm) and Martonne Aridity Index for the already mentioned period. 
	- ⚠ *Note: the * symbol in the name of the document refers to the Shared Socioeconomic Pathway (SSP) used to derive the values. All the values were extracted from predictions of the MIROC6 model.*
- 💾 **0.4.1.worldclim_climate_data.rdata**: file with all the previous data