<div style="text-align: center;">

*Original data, code and results related to the scientific article titled*

# ***One model to rule them all: a nationwide height-diameter model for 91 Spanish forest species***

</div>

<!--:bulb: Have a look at the original poster  [here](http://dx.doi.org/10.13140/RG.2.2.27865.94564). -->

<!--
:bookmark: Poster DOI: <!-- http://dx.doi.org/10.13140/RG.2.2.27865.94564 -->

<!--
:open_file_folder: Repository DOI: <!-- [![DOI](https://zenodo.org/badge/713296626.svg)](https://zenodo.org/doi/10.5281/zenodo.12772484) -->

<!--
📜 Manuscript DOI: <!-- https://doi.org/10.1016/j.ecolmodel.2024.110912 -->

---

## :sparkles: Highlights 

- A total of 1,512,721 observations representing 91 species were collected from the Spanish National Forest Inventory.
- Ninety-five different baseline equations using dbh as the sole predictor were tested.
- A unified non linear mixed-effect model was fitted, incorporating all species and observations.
- Variables addressing stand origin, species mixture, and biogeographic region were included as fixed effects.
- The model is simple in structure, relies on easily measurable predictors and is well-suited for field applications.


## :book: Abstract

Developing reliable quantitative tools is essential for accurately monitoring and predicting tree and forest growth in the context of sustainable forest management. In common growth and yield modeling systems, diameter at breast height (dbh) and total tree height are key variables for estimating and predicting metrics such as total volume, biomass, and carbon content. However, measuring tree height in the field is more challenging and time-consuming compared to dbh. To address this, height-diameter (h-d) relationship models provide a practical alternative, enabling the estimation of tree heights using only dbh measurements.

In this study, new h-d models were developed for the main forest species in Spain. A total of 1,512,721 observations, representing 91 species, were collected from Spanish National Forest Inventory sample plots for analysis. The best baseline equations out of 95 alternatives using dbh as a unique predictor was selected to fit a unique non linear mixed-effect model. The results indicate that different equations can achieve similar levels of performance. Furthermore, incorporating variables such as stand origin, species mixture, and biogeographic region enhances model predictability, particularly when applied across broad geographic areas.

The proposed models are simple in structure and rely on easily obtainable predictors, making them practical for field application and minimizing the need for complex measurements. Their integration into widely-used forest growth and yield simulators, such as SIMANFOR, further facilitates their adoption by forest practitioners and managers, supporting informed and effective forest management decisions.

## :dart: Graphical abstract

![ga](./output/graphical_abstract.jpg)


---

## 🛠 Tools developed

:point_right: As an outcome of this work, you can utilize the following resources available [here](./tools/):

- <img src="https://avatars.githubusercontent.com/u/111344993?s=200&v=4" alt="simanfor_logo" width="20"> ***A new [SIMANFOR](www.simanfor.es) model***: the model developed in this study was implemented into the simulator. You can access it by selecting the *Calcular alturas* model during the scenario creation screen. Additionally, models without a specific height-diameter equation will use it when needed. Check the [models documentation](https://github.com/simanfor/modelos) for more information
- :computer: :1234: ***Excel calculator***: explore that [*Excel calculator file*](./tools/) (both English and Spanish) to run the models developed in this study without effort

![](./tools/screenshots/excel_result.png)

- :computer: :scientist: ***R functions***: explore these [*R functions*](./tools/) to run the models developed in this study without needing to code
- :computer: :snake: ***Python functions***: explore these [*Python functions*](./tools/) to run the models developed in this study without needing to code
  
---

## :file_folder: Repository Contents

- :open_file_folder: [***bibliography***](./bibliography/): compilation of all the literature cited or consulted during the creation of the document
- :open_file_folder: [***data***](./data/): raw and processed data, check [here](./data/README.md) for a detailed description
- :open_file_folder: [***output***](./output/): figures, charts, tables and additional resources included in the document, check [here](./output/README.md) for a detailed description
- :open_file_folder: [***scripts***](./scripts/): compilation of the code used for data curation, analysis and outputs included in the document, check [here](./scripts/README.md) for a detailed description
- :open_file_folder: [***tools***](./tools/): resources developed trough this study, check [here](./tools/README.md) for a detailed description
  
---

## :thinking: How to use the resouces of that repository

:dizzy: To download the information of that repository, you can follow this [guide](https://docs.github.com/en/repositories/working-with-files/using-files/downloading-source-code-archives).

:recycle: To reproduce the analysis, users must:

- :floppy_disk: **Data**: 
  - :sunny: WorldClim data required must be downloaded from its [official website](https://www.worldclim.org/data/index.html)
  - :deciduous_tree::evergreen_tree: Spanish Forest National Inventory (SFNI) data required must be downloaded from its [official website](https://www.miteco.gob.es/es/biodiversidad/temas/inventarios-nacionales/inventario-forestal-nacional.html), through the links above, or accessed via this [GitHub repository](https://github.com/aitorvv/SFNI_Spanish_Forest_National_Inventory-ready_to_use/) 

- :computer: **Prerequisites: installation and code**: *[R](https://cran.r-project.org/bin/windows/base/)* must be installed to run the code with the libraries used in each script. *[RStudio](https://posit.co/download/rstudio-desktop/)* was used to develop the code. Some analyses may require high computational power, which could lead to out-of-memory issues on a standard computer. Access to high-performance computing services is strongly recommended in such cases.

- :scroll: **Usage**: follow the numerical order of the scripts to reproduce each step correctly

---

## :books: <img src="https://avatars.githubusercontent.com/u/111344993?s=200&v=4" alt="simanfor_logo" width="30">    Additional Information

To better understand how SIMANFOR works, you can explore its [website](https://www.simanfor.es/), [GitHub repository](https://github.com/simanfor), [manual](https://github.com/simanfor/manual), [YouTube playlist](https://www.youtube.com/playlist?list=PLsdzTKpJZZa7vn5zGpn07-bd0Nce-fMhJ) or even the [last paper](https://doi.org/10.1016/j.ecolmodel.2024.110912). 

---

## ℹ License

The content of this repository is under the [MIT license](./LICENSE).

---

## 🔗 About the authors



#### Aitor Vázquez Veloso:

[![](https://github.com/aitorvv.png?size=50)](https://github.com/aitorvv) 

[![Email](https://img.shields.io/badge/Email-D14836?logo=gmail&logoColor=white)](mailto:aitor.vazquez.veloso@uva.es)
[![ORCID](https://img.shields.io/badge/ORCID-0000--0003--0227--506X-green?logo=orcid)](https://orcid.org/0000-0003-0227-506X)
[![Google Scholar](https://img.shields.io/badge/Google%20Scholar-4285F4?logo=google-scholar&logoColor=white)](https://scholar.google.com/citations?user=XNMn1cUAAAAJ&hl=es&oi=ao)
[![ResearchGate](https://img.shields.io/badge/ResearchGate-00CCBB?logo=researchgate&logoColor=white)](https://www.researchgate.net/profile/Aitor_Vazquez_Veloso)
[![LinkedIn](https://img.shields.io/badge/LinkedIn-blue?logo=linkedin)](https://linkedin.com/in/aitorvazquezveloso/)
[![X](https://img.shields.io/badge/X-1DA1F2?logo=x&logoColor=white)](https://twitter.com/aitorvv)
[<img src="https://media.licdn.com/dms/image/v2/D4D0BAQFazHOlOJO50A/company-logo_200_200/company-logo_200_200/0/1692170343519/universidad_de_valladolid_logo?e=1747872000&v=beta&t=1mTS-xC7h3L_DQATdt6hpqjWGgW_Am3MXKnjYwcOVZs" alt="Description" width="22">](https://portaldelaciencia.uva.es/investigadores/178830/detalle)

#### Sheng-I Yang:


<img src="https://warnell.uga.edu/sites/default/files/styles/square_400x400/public/Yang_photo1.jpg?itok=w2sseGLS" alt="Description" width="50"> 

[![ORCID](https://img.shields.io/badge/ORCID-0000--0002--4689--2628-green?logo=orcid)](https://orcid.org/0000-0002-4689-2628) 
[![ResearchGate](https://img.shields.io/badge/ResearchGate-00CCBB?logo=researchgate&logoColor=white)](https://www.researchgate.net/profile/Sheng-I-Yang) 
[![LinkedIn](https://img.shields.io/badge/LinkedIn-blue?logo=linkedin)](https://www.linkedin.com/in/sheng-i-yang-b40713ba/) 
[<img src="https://media.licdn.com/dms/image/v2/D560BAQE-PbmFpTkcrQ/company-logo_200_200/company-logo_200_200/0/1687803751321/university_of_georgia_logo?e=1747872000&v=beta&t=h9hk5eeZ4eSSOXpyN8ofSgMMLkfEVS6kid04NIwsAjY" alt="Description" width="22">](https://portaldelaciencia.uva.es/investigadores/181874/detalle)

#### Bronson P. Bullock:

<img src="https://warnell.uga.edu/sites/default/files/styles/square_400x400/public/Bullock_Bronson.jpg?itok=tpmOn0ZE" alt="Description" width="50"> 

[![ORCID](https://img.shields.io/badge/ORCID-0000--0003--0227--506X-green?logo=orcid)](https://orcid.org/0000-0002-8783-7334) 
[![ResearchGate](https://img.shields.io/badge/ResearchGate-00CCBB?logo=researchgate&logoColor=white)](https://www.researchgate.net/profile/Bronson-Bullock) 
[![LinkedIn](https://img.shields.io/badge/LinkedIn-blue?logo=linkedin)](https://www.linkedin.com/in/bronsonbullock-pmrc/) 
[<img src="https://media.licdn.com/dms/image/v2/D560BAQE-PbmFpTkcrQ/company-logo_200_200/company-logo_200_200/0/1687803751321/university_of_georgia_logo?e=1747872000&v=beta&t=h9hk5eeZ4eSSOXpyN8ofSgMMLkfEVS6kid04NIwsAjY" alt="Description" width="22">](https://warnell.uga.edu/directory/people/dr-bronson-p-bullock)


#### Felipe Bravo Oviedo:

[![](https://github.com/Felipe-Bravo.png?size=50)](https://github.com/Felipe-Bravo) 

[![ORCID](https://img.shields.io/badge/ORCID-0000--0001--7348--6695-green?logo=orcid)](https://orcid.org/0000-0001-7348-6695) 
[![ResearchGate](https://img.shields.io/badge/ResearchGate-00CCBB?logo=researchgate&logoColor=white)](https://www.researchgate.net/profile/Felipe-Bravo-11) 
[![LinkedIn](https://img.shields.io/badge/LinkedIn-blue?logo=linkedin)](https://www.linkedin.com/in/felipebravooviedo) 
[![X](https://img.shields.io/badge/X-1DA1F2?logo=x&logoColor=white)](https://twitter.com/fbravo_SFM) 
[<img src="https://media.licdn.com/dms/image/v2/D4D0BAQFazHOlOJO50A/company-logo_200_200/company-logo_200_200/0/1692170343519/universidad_de_valladolid_logo?e=1747872000&v=beta&t=1mTS-xC7h3L_DQATdt6hpqjWGgW_Am3MXKnjYwcOVZs" alt="Description" width="22">](https://portaldelaciencia.uva.es/investigadores/181874/detalle)


---

<div style="text-align: center;">

[One model to rule them all: a nationwide height-diameter model for 91 Spanish forest species](https://github.com/aitorvv/height-diameter_models_Spain) 

</div>