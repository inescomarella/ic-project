# Analysis of habitat selection and competitive relationships

This work is useful to model occupancy considering the detection per site.

## Motivation
This project is part of my Scientific Initiation project, entitled "Habitat selection and competitive relationships among carnivores in a fragmented landscape". The work was funded by FAPES/Vale 2015.


## Analysis steps
There are 3 steps, the first prepare the data, the second analyse the data, and the third prepare the output so you can analyse the results. I don't have an script to the first step because I've made it manually, but I'll decribe next. 


### Preparing the data
We used occurence based on presence-absence, so it's not possible to estimate abundance. Each especies has it's own occurence table formated as a sites x occasion table, so the site are in the columns and the occasion in the rows. an occasion is the period of time you are considering to register the species occurence, it can be a day, 3 days, 5 days, etc. I've tests some occasion based on works published, so I could choose what is best.

I included an extra-step in this case to remove the species name so I could share my results. Because it's an extra I names the script 0-prep-data.R, bt it's not crucial to run the data.

### Model analysis
The modeling is divided in 4 steps, 1. input variables and species data, 2. analyse the detection bias, 3. model the occupancy prediction and 4. export the outputs. This script is a kind of hard to run because many steps are important to decide what to do in the next so it's necessary to pay atention, specially to rename the outputs.

Let's see how it works:






## Modifications on the script
Detecção1 = detection-models -> first step in detection models to check if it is null, or determined by time or by the covariates
Detecção2 = detection-pVar -> intermediate step, in case detection is determined by covariates, models different combination of covariates.
p_var = detection-covariates -> covariates bias on detection
DeteccaoUA = detection-persite -> detection bias per site
Ocupacao1 = occupancy-models -> first step in occupancy models to check the explanatory covariates
Ocupacao2 = occupancy-covariates -> covariates mean on occupancy
OcupacaoUA = occupancy-persite -> occupancy prediction per site
