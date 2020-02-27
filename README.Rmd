# Analysis of habitat selection and competitive relationships

This work is useful to model occupancy considering the detection per site.

## Motivation
This project is part of my Scientific Initiation project, entitled "Habitat selection and competitive relationships among carnivores in a fragmented landscape". The work was funded by FAPES/Vale 2015.

## Modifications on the script
Changed the languange
Detecção1 = detection-models -> first step in detection models to check if it is null, or determined by time or by the covariates
Detecção2 = detection-pVar -> intermediate step, in case detection is determined by covariates, models different combination of covariates.
p_var = detection-covariates -> covariates bias on detection
DeteccaoUA = detection-persite -> detection bias per site
Ocupacao1 = occupancy-models -> first step in occupancy models to check the explanatory covariates
Ocupacao2 = occupancy-covariates -> covariates mean on occupancy
OcupacaoUA = occupancy-persite -> occupancy prediction per site
