#!/bin/bash

#SBATCH -p shared

#SBATCH -n 1

#SBATCH -N 1

#SBATCH -t 0-40:00:00

#SBATCH --mem 150000

#SBATCH -o hostname.out

#SBATCH -e hostname.err

#SBATCH --mail-type=ALL

#SBATCH --mail-user=cchamberlain@g.harvard.edu

source new-modules.sh

export R_LIBS_USER=$HOME/apps/R:$R_LIBS_USER
module load gcc/8.2.0-fasrc01 openmpi/3.1.1-fasrc01 R_core/3.6.1-fasrc01 R_packages/3.6.1-fasrc01
module load gdal/2.3.0-fasrc01
module load geos/3.6.2-fasrc01
module load proj/5.0.1-fasrc01


R CMD BATCH --quiet --no-restore --save /n/wolkovich_lab/Lab/Cat/Get_range_nam_odys.R ranges
