# Mapping the ghost: Estimating probabilistic snow leopard distribution across Mongolia

Data and code used for an country-wide occupancy survey of snow leopards in Mongolia, accompanying the paper below. 

Data: `data\Mongolia_occupancy_inputs.Rdata` contains the results of a survey of 1017 20x20km sampling units, out of a total of 1200 sampling units identified as potential snow leopard habitat (183 could not be sampled for various reasons). This represents a near complete survey of potential snow leopard habitat in Mongolia, nearly 500,000 square kilometers. Please see the paper for more detail on data collection.

Code: to reproduce the analyses in the paper, 

- run *occupancy-analysis.R* to fit the main occupancy models (these are also saved in the `\output` folder), do model selection, and plot covariate effects
- run *occupancy-goodness-of-fit.R* to calculate the c-hat statistic giving an indication of model fit for the best model
- run *comparing-maps.R* to compare the occupancy results with similar metrics generated using a presence-only analysis (using MaxEnt) or an expert map generated through qualitative discussion (reproduces Figure 3 in the paper).

Code in *occupancy-data-preproc.R* is not needed but included for completeness. It converts the csv files in `data\csv`, which contain various input datasets used by the occupancy model, into a single .Rdata file (`data\Mongolia_occupancy_inputs.Rdata`), which is then used by the scripts above. Some minimal pre-processing (excluding ununsed variables, renaming for consistency, etc) is performed. 

Paper: Mapping the ghost: Estimating probabilistic snow leopard distribution across Mongolia. (2021). Gantulga Bayandonoi, Koustubh Sharma, Justine Shanti Alexander, Purevjav Lkhagvajav, Ian Durbach, Darryl MacKenzie, Chimeddorj Buyanaa, Bariushaa Munkhtsog, Munkhtogtokh Ochirjav, Sergelen Erdenebaatar, Bilguun Batkhuyag, Nyamzav Battulga, Choidogjamts Byambasuren, Bayartsaikhan Uudus, Shar Setev, Lkhagvasuren Davaa, Khurel-Erdene Agchbayar, Naranbaatar Galsandorj, David Borchers.

