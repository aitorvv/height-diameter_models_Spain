# ***One model to rule them all: a nationwide height-diameter model for 91 Spanish forest species***

### :computer: :floppy_disk: :bar_chart: *Original data, code and results related to the study*
---

:bulb::brain: ***Each file and/or folder code corresponds to the script used to generate it, ensuring that all (dataset + script + output) share the same code***

:warning: :scroll: ***Remember to update the script paths in your working directory if you plan to use that code***

---

## :file_folder: Folder Content

- `0.0_support_data_report.r` `0.0_support_plot_functions.r` `0.0_support_tree_functions.r`
	- :bulb: *purpose*: support functions to create a data report and calculate variables related with both trees and plots
	- :floppy_disk: :arrow_right: :computer: *input*: None
	- :computer: :arrow_right: :floppy_disk: *output*: None
- `0.1_sfni_harmonisation.r`
	- :bulb: *purpose*: it uses the original datasets to merge information for different tables, rename columns, fix errors in the original data and group the 3 SFNI editions in a single tree and plot data set
	- :floppy_disk: :arrow_right: :computer: *input*: `1_data/1_raw/IFN2/*` `1_data/1_raw/IFN3/*` `1_data/1_raw/IFN4/*`
	- :computer: :arrow_right: :floppy_disk: *output*: temporal datasets with variables not used in the beginning (`1_data/2_processed/tmp/*`) and harmonised data set (`1_data/2_processed/0.1_sfni_harmonised.rdata`)
- `0.2_sfni_forest_data.r`
	- :bulb: *purpose*: code to calculate basic tree and plot variables with some forest meaning like the expansion factor and plot density
	- :floppy_disk: :arrow_right: :computer: *input*: harmonised data set (`1_data/2_processed/0.1_sfni_harmonised.rdata`)
	- :computer: :arrow_right: :floppy_disk: *output*: previous dataset with additional forest variables (`1_data/2_processed/0.2_sfni_forest_data.rdata`)
- `0.3.0_sfni_position_data` `0.3.1_explore_plot_distribution` `0.3.2_sfni_corrected_position_data`
	- :bulb: *purpose*: code developed to calculate plot and tree position variables (like latitude and longitude) based on the original data. After finding mistakes in some plots they were corrected is possible (`0.3.1`) and the process was run again (`0.3.2`) with some adaptations
	- :floppy_disk: :arrow_right: :computer: *input*: already processed forest data (`1_data/2_processed/0.2_sfni_forest_data.rdata`)
	- :computer: :arrow_right: :floppy_disk: *output*: dataset with forest and position data (`1_data/2_processed/0.3.2_sfni_corrected_position_data.rdata`)
- `0.4.0_climatic_classification` 
	- :bulb: *purpose*: code developed to assign climatic variables by geographic regions to each plot. As climatic variables are obtained from different datasets, variables are names with a prefix referred to the original data source:
		- *fito*: data obtained from fitoclimatic regions (*Mapa de Subregiones Fitoclimáticas de España Peninsular y Balear*, accesible [here](https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/mapa_subregiones_fitoclim_descargas.html))
		- *biogeo*: data obtained from biogeographic regions (*Regiones Biogeográficas de España*, accesible [here](https://www.miteco.gob.es/es/cartografia-y-sig/ide/descargas/biodiversidad/regiones-biogeograficas.html))
		- *veget*: data obtained from vegetation series maps (*Mapa de Series de Vegetación*, accesible [here](https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/memoria_mapa_series_veg_descargas.html))
	- :floppy_disk: :arrow_right: :computer: *input*: already corrected position data (`1_data/2_processed/0.3.2_sfni_corrected_position_data.rdata`)
	- :computer: :arrow_right: :floppy_disk: *output*: dataset with forest and position data (`1_data/2_processed/0.4.0_climatic_classification.rdata`)
- `0.4.1.worldclim_climate_data` 
	- :bulb: *purpose*: code developed to extract climate data for each plot using WorldClim database and functions already created to extract information by plot location in *WGS84* 
	- :floppy_disk: :arrow_right: :computer: *input*: already corrected position data (`1_data/2_processed/0.3.2_sfni_corrected_position_data.rdata`)
	- :computer: :arrow_right: :floppy_disk: *output*: different datasets are provided:
		- *historic monthly climate data (tmax, tmean, tmin, prec, Martonne)*: `1_data/2_processed/0.4.1.sfni_historic_monthly_climate_data.rdata` in a single file and `1_data/2_processed/0.4.1/` folder in several files with lower weight
		- *historic yearly climate data (tmax, tmean, tmin, prec, Martonne)*: `0.4.1.sfni_historic_yearly_climate_data.rdata`
		- *historic climate averaged for 70 years period, from 1951 to 2021 (tmax, tmean, tmin, prec, Martonne)*: `0.4.1.sfni_70years_period_climate_data.rdata`
		-  *historic climate averaged for 20 years period, from 2000 to 2020 (tmax, tmean, tmin, prec, Martonne)*:  `0.4.1.sfni_2000-2020_period_climate_data.rdata`
		- *future climate data averaged on 20 years period (2020-2040, 2040-2060, 2060-2080, 2080-2100) for an specific SSPs (1, 2, 3 and 5) using MIROC6 model (tmax, tmean, tmin, prec, Martonne)*: `0.4.1.sfni_future_climate_data_MIROC6_SSPx.rdata`
		- *all the previous data in just one file:* `0.4.1.worldclim_climate_data.rdata
		- ⚠ Some files are too large to be uploaded to GitHub. Consider contacting us to receive them through a different channel.
- `1.0_hd_support_functions`
	- :bulb: *purpose*: support functions to manage data after harmonisation
	- :floppy_disk: :arrow_right: :computer: *input*: None
	- :computer: :arrow_right: :floppy_disk: *output*: None
- `1.1_sfni_data_merge_and_report.r`
	- :bulb: *purpose*: load all the previous data already harmonised, merge them and create a data report
	- :floppy_disk: :arrow_right: :computer: *input*: already processed and harmonised data (`1_data/2_processed/9.0.2_sfni_forest_data.rdata`, `1_data/2_processed/0.4.0_climatic_classification.rdata`, `1_data/2_processed/0.3.2_sfni_corrected_position_data.rdata`)
	- :computer: :arrow_right: :floppy_disk: *output*: merged data with some of the most useful columns (`1_data/2_processed/1.1_sfni_clean_data.rdata`) and with all the columns except climate from WorldClim (`1_data/2_processed/1.1_sfni_all_data.rdata`)
- `1.2_include_useful_labels`
	- :bulb: *purpose*: uses the previous dataset to include some labels to classify trees and plots by extracting information from the original SFNI
	- :floppy_disk: :arrow_right: :computer: *input*: data set already merged with all the available columns (`1_data/2_processed/1.1_sfni_all_data.rdata`)
	- :computer: :arrow_right: :floppy_disk: *output*: previous data set with additional columns included and with tree and plot data not useful for the analysis removed (`1_data/2_processed/1.2_sfni_all_data_to_hd_models.rdata`)
- `1.3_graph_hd_relationships.r`
	- :bulb: *purpose*: graph height-diameter relationships for each species and data set groups selected
	- :floppy_disk: :arrow_right: :computer: *input*: SFNI species codes (`1_data/1_raw/SFNI4_species_codes.csv`) and data already curated for the analysis (`1_data/2_processed/1.2_sfni_all_data_to_hd_models.rdata`)
	- :computer: :arrow_right: :floppy_disk: *output*: graphs and summary with the number of records per species for each species (`3_figures/1.3_hd_baseline_graphs/species/`) and group of data (`3_figures/1.3_hd_baseline_graphs/data/`)
- `2.0_hd_equations.r`
	- :bulb: *purpose*:  height-diameter equations compilation
	- :floppy_disk: :arrow_right: :computer: *input*: None
	- :computer: :arrow_right: :floppy_disk: *output*: None
- `2.0_support_functions.r`
	- :bulb: *purpose*:  functions to support the analysis and data management of point 2
	- :floppy_disk: :arrow_right: :computer: *input*: None
	- :computer: :arrow_right: :floppy_disk: *output*: None
- `2.1_hd_all_base_model_fit.r`
	- :bulb: *purpose*: fit base hd model for each data set (SFNI edition) from a total of 73 model candidates
	- :floppy_disk: :arrow_right: :computer: *input*: SFNI species codes (`1_data/1_raw/SFNI4_species_codes.csv`), number of records per species (`3_figures/1.3_hd_baseline_graphs/species/n_values_by_species.csv`) and data already curated for the analysis (`1_data/2_processed/1.2_sfni_all_data_to_hd_models.rdata`)
	- :computer: :arrow_right: :floppy_disk: *output*: initial workspace to reuse later with the inputs after its adaptation (`1_data/2_processed/2.1_hd_model_data_workspace.rdata`); for each species and group of data, model stats (`3_figures/2.1_hd_model_selection_by_df/stats/`), coefficients (`3_figures/2.1_hd_model_selection_by_df/coefficients/`) and their compilation in a single file (`3_figures/2.1_hd_all_base_model_fit/all_models.rdata`)
- `2.2_hd_top_base_model_fit.r`
	- :bulb: *purpose*: select the top models (5 or more if tie) and fit them again for all the species selected and the full dataset (this time 5 models instead of 73 to simplify)
	- :floppy_disk: :arrow_right: :computer: *input*: workspace from previous file with data already prepared to be used (`1_data/2_processed/2.1_hd_model_data_workspace.rdata`) and stats and coefficients compilation from the base models fit (`3_figures/2.1_hd_all_base_model_fit/all_models.rdata`)
	- :computer: :arrow_right: :floppy_disk: *output*: for each species, model stats (`3_figures/2.2_hd_top_base_model_fit/stats/`), coefficients (`3_figures/2.2_hd_top_base_model_fit/coefficients/`) and their compilation in a single file (`3_figures/2.2_hd_top_base_model_fit/all_models.rdata`)
- `2.3_fit_nlme_models_all_combis.r`
	- :bulb: *purpose*: select the top models (5 or more if tie) and fit them again for all the species selected and the full dataset (this time 5 models instead of 73 to simplify)
	- :floppy_disk: :arrow_right: :computer: *input*: workspace from previous file with data and coefficients (`1_data/2_processed/2.2_hd_top_model_selection.rdata`) to be used when fitting models
	- :computer: :arrow_right: :floppy_disk: *output*: for each model combination, results of the fit process (`1_data/2_processed/2.3_fit_nlme_models/*`) and a summary of the metrics for all the models and combinations tested (`1_data/2_processed/2.3_all_models_metrics.rdata`)
- `2.3_fit_nlme_models_all_combis-structure_tests`: similar model with new tested structures
- `2.4_best_model-selection_and_tables.r`
	- :bulb: *purpose*: select the better model based on their metrics
	- :floppy_disk: :arrow_right: :computer: *input*: metrics of all the models tested (`1_data/2_processed/2.3_all_models_metrics.rdata`); results of the best model evaluation (`1_data/2_processed/2.3_best_model_fe_good_order.rdata`)
	- :computer: :arrow_right: :floppy_disk: *output*: table with the top models metrics to show on the article (`3_figures/2.4-5_best_model/2.4_top_base_model_table.tex`): table of the best models metrics (`3_figures/2.4-5_best_model/2.4.fe_coefs_table.tex`) and the initial data (`3_figures/2.4-5_best_model/2.4.initial_data_summary_table.tex`)
- `2.5_best_model_graphs.r`
	- :bulb: *purpose*: graph results of the best model to show the model performance
	- :floppy_disk: :arrow_right: :computer: *input*: metrics of all the models tested (`1_data/2_processed/2.3_all_models_metrics.rdata`) and species codes (`1_data/1_raw/SFNI4_species_codes.csv`); original dataset to export it in a proper format (`1_data/2_processed/1.2_sfni_all_data_to_hd_models.rdata`)
	- :computer: :arrow_right: :floppy_disk: *output*: graphs and table showing the best model performance (`3_figures/2.4-5_best_model/`); original dataset filtered with all the trees used in model fitting (`1_data/2_processed/2.5_best_model_data-plots_to_graph.csv`)
- `2.6_cluster_bootstrap.r`
	- :bulb: *purpose*: cluster bootstrap analysis code
	- :floppy_disk: :arrow_right: :computer: *input*: results of the best model evaluation (`1_data/2_processed/2.3_best_model_fe_good_order.rdata`)
	- :computer: :arrow_right: :floppy_disk: *output*: results for each iteration of the cluster bootstrap analysis (`1_data/2_processed/2.6/*`)
- `2.7_get_cluster_bootstrap_metrics.r`
	- :bulb: *purpose*: extracting quantile, median and metrics from cluster bootstrap results
	- :floppy_disk: :arrow_right: :computer: *input*: results of the cluster bootstrap analysis (`1_data/2_processed/2.6/*`)
	- :computer: :arrow_right: :floppy_disk: *output*: metrics results of all the cluster bootstrap analysis averaged (`1_data/2_processed/2.7_cluster_bootstrap_results.rdata`)
- `2.8_cluster_bootstrap_metrics_exploration.r`
	- :bulb: *purpose*: explore results from cluster bootstrap analysis and export tables
	- :floppy_disk: :arrow_right: :computer: *input*: results of the cluster bootstrap analysis already processed (`1_data/2_processed/2.7_cluster_bootstrap_results.rdata`)
	- :computer: :arrow_right: :floppy_disk: *output*: preliminar tables to present the metrics and parameters of the cluster bootstrap analysis (`3_figures/2.8_cluster_tables/*` )

- `9.x.x_file_title.r`
	- :bulb: *purpose*: code used to run the same process at the high-performance computer, adapting paths
	- :floppy_disk: :arrow_right: :computer: *input*: same as original code
	- :computer: :arrow_right: :floppy_disk: *output*: same as original code

