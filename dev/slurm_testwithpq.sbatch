#!/usr/bin/bash

## Resource Requests
#SBATCH --partition=batch
#SBATCH --error="/home/groups/chse_quant/UserFolders/Kyle/logs/long/err/testwithpq_%A_%a.err"
#SBATCH --verbose
#SBATCH --output="/home/groups/chse_quant/UserFolders/Kyle/logs/long/testwithpq_%A_%a.out"
#SBATCH --mem 250G
#SBATCH --time 05:30:00
#SBATCH -J testwithpq

./test_with_pq.R
