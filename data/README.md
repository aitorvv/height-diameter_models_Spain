<div style="text-align: center;">

*Original data, code and results related to the scientific article titled*

# ***One model to rule them all: a nationwide height-diameter model for 91 Spanish forest species***

</div>


## :open_file_folder: Folder Content

---

:warning: Due to file size restrictions, the raw data and part of the processed one was not uploaded to GitHub. However, you can access it through this  [OneDrive link](https://uvaes-my.sharepoint.com/:f:/g/personal/aitor_vazquez_veloso_uva_es/Ekf3mkjKZRhAsWMddelRczYBsTuhLNRf1qy7x_30ImVW4w?e=Y7hiiP). If the link is not working, please let us know.

:bulb: If you are looking for the full Spanish Forest National Inventory (SFNI) dataset already merged and harmonised, just download the file `1.1_sfni_all_data.rdata` in the [OneDrive link](https://uvaes-my.sharepoint.com/:f:/g/personal/aitor_vazquez_veloso_uva_es/Ekf3mkjKZRhAsWMddelRczYBsTuhLNRf1qy7x_30ImVW4w?e=Y7hiiP) on use that [link](https://uvaes-my.sharepoint.com/:u:/g/personal/aitor_vazquez_veloso_uva_es/ETe8uDHcPbFJqvgEtO1bsaIBdKUnCjzD9Hz65j7D8nzYvA?e=vggano). 

:thinking::bulb: If you are interested in SFNI data, consider taking a look at that [GitHub repository](https://github.com/aitorvv/SFNI_Spanish_Forest_National_Inventory-ready_to_use/) where further updates to that dataset will be uploaded.

:computer: :brain: :floppy_disk: Check [here](../scripts/README.md) to see how the data was processed

:books: :floppy_disk: Data documentation is available [here](../output/metadata/SFNI_metadata_avv.md)

---

### :file_folder: Folder Content

- :open_file_folder: ***1_raw***: initial datasets

  - :sunny: WorldClim data required must be downloaded from its [official website](https://www.worldclim.org/data/index.html)
  - :deciduous_tree::evergreen_tree: Spanish Forest National Inventory (SFNI) data required must be downloaded from its [official website](https://www.miteco.gob.es/es/biodiversidad/temas/inventarios-nacionales/inventario-forestal-nacional.html), through the links above, or accessed via this [GitHub repository](https://github.com/aitorvv/SFNI_Spanish_Forest_National_Inventory-ready_to_use/) 
     - :deciduous_tree: Acess to the [SFNI2](https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/ifn2.html)
     - :deciduous_tree: Acess to the [SFNI3](https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/ifn3.html)
     - :deciduous_tree: Acess to the [SFNI4](https://www.miteco.gob.es/es/biodiversidad/temas/inventarios-nacionales/inventario-forestal-nacional/cuarto_inventario.html)
  - :open_file_folder: ***IFNx*** folders contain the original SFNI datasets
  - :open_file_folder: ***mapas_series_vegetacion*** folder contains the original *Vegetation Series Maps* for Spain, downloaded from [here](https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/memoria_mapa_series_veg_descargas.html)
  - :open_file_folder: ***regiones_biogeograficas_espana*** folder contains the original *Spanish Biogeographic Regions*, downloaded from [here](https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/regiones_biogeograficas.html)
  - :open_file_folder: ***subregiones_fitoclimaticas*** folder contains the original *Spanish Fitoclimatic Subregions*, downloaded from [here](https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/mapa_subregiones_fitoclim.html)
  - :floppy_disk: ***ine_provincias.csv*** file contains the Spanish province codes
  - :floppy_disk: ***SFNI4_species_codes.csv*** file contains the species names and codes for SFNI4


- :open_file_folder: ***2_processed***: analyzed datasets

:bulb::brain: ***Each file and/or folder code corresponds to the script used to generate it, ensuring that all (dataset + script + output) share the same code***

  - :computer: :brain: :floppy_disk: Check [here](../scripts/README.md) to see how the data was processed
  - :open_file_folder: ***0.4.1*** contains temporary files with historical monthly climate data for each SFNI plot. These were merged into the *0.4.1.sfni_historic_monthly_climate_data.rdata* file
  - :open_file_folder: ***2.3_fit_nlme_models*** contains fitted models using *nlme* for each random effect and fixed effect combination
  - :open_file_folder: ***2.6*** contains cluster bootstrap analysis results. The main file used is  *2.6_cluster_bootstrap_results_1000.rdata*, but an additional file, *2.6_cluster_bootstrap_results_3.rdata* is included to explore its contents, as the primary file may be too large for a standard computer to handle