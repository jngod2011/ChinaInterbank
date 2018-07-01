#!/bin/bash

#SBATCH --job-name=RMB3ergm.R
#SBATCH --partition=broadwl
#SBATCH --nodes=4
#SBATCH --time=36:00:00
#SBATCH --mem-per-cpu=3000

# receive an email when job starts, ends, and fails
#SBATCH --mail-type=BEGIN,END,DAIL

# Load the default version of hello.
module load R

# Use R CMD BATCH to run Rhello.R.
R CMD BATCH --no-save --no-restore RMB3ergm.R



