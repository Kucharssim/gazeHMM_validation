# GazeHMM
Algorithm for classifying gaze data into eye movement events using a hidden Markov model.

## Structure

- *algorithm*: R functions for gazeHMM algorithm  
- *manuscript*: files for compiling the preprint manuscript
- *simulation*: R scripts for running the simulation study  
  + *preregistration*: files for compiling the preregistration of the simulation study  
- *validation*: R scripts for running the validation of gazeHMM  

## Reproduction
The preprint manuscript can be reproduced by running *gazeHMM_validation/manuscript/preprint_Luken_Kucharsky_Visser_Classifying_Eye_Movement_Events.Rmd*. Several files are required for the reproduction:

- Simulation results - can be obtained by running *parameter_recovery_simulation.R* and *parameter_recovery_simulation_exploration.R*; the simulation takes a lot of time to run and thus, the results are included in the repository, i.e., *part_X.Rdata* and *part_3_expl.Rdata*; an image of R after the simulation was run is contained in *results_image.Rdata*
- Raw data and fitted algorithm data for the Andersson et al. (2017) data set: Those can be obtained by placing the data of the original article in *validation/data* and running *validation_Andersson2017.R*
- Fitted algorithm data for the Ehinger et al. (2019) data set, which can be obtained by placing the .EDF (EyeLink) files of the original article in *validation/data* and running *validation_Ehinger2019.R*

***
Check out the R package for gazeHMM under https://github.com/maltelueken/gazeHMM!
