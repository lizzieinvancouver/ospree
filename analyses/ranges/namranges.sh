#!/bin/bash

#SBATCH -p wolkovich

#SBATCH -n 4

#SBATCH -N 1

#SBATCH -t 0-100:00:00

#SBATCH --mem 100000

#SBATCH -o hostname.out

#SBATCH -e hostname.err

#SBATCH --mail-type=ALL

#SBATCH --mail-user=cchamberlain@g.harvard.edu

source new-modules.sh
export R_LIBS_USER=$HOME/apps/R:$R_LIBS_USER
module load gcc/7.1.0-fasrc01 R_core/3.5.1-fasrc02
module load gcc/7.1.0-fasrc01 R_packages/3.5.1-fasrc02
module load gdal/2.3.0-fasrc01
module load geos/3.6.2-fasrc01
module load proj/5.0.1-fasrc01



R CMD BATCH --quiet --no-restore --save /n/wolkovich_lab/Lab/Cat/Get_range_nam_odyssey.R ranges
