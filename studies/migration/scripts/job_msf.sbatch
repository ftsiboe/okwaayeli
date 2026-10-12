#!/bin/bash -l

#SBATCH --job-name=msf_mig
#SBATCH --time=0-50:00:00    
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=3
#SBATCH --mem=28G 
#SBATCH --array=1-60   # PLACEHOLDER: set to nrow(model_specifications) -- see scripts/README.md
#SBATCH --mail-user=ftsiboe@hotmail.com
#SBATCH --mail-type=ALL
#SBATCH --output=/dev/null

module purge
module load RStudio-Server/2023.09.1+494-foss-2023a-Java-11-R-4.3.2

# Path to 004 differs by checkout: studies/migration/scripts/ in the okwaayeli
# monorepo, scripts/ standalone. Pick whichever is present rather than assuming,
# so the same job file submits from either. The script itself resolves the study
# root through scripts/_paths.R once it starts.
if [ -f studies/migration/scripts/004_MSF_migration_study.R ]; then
  MSF_SCRIPT=studies/migration/scripts/004_MSF_migration_study.R
elif [ -f scripts/004_MSF_migration_study.R ]; then
  MSF_SCRIPT=scripts/004_MSF_migration_study.R
else
  echo "job_msf.sbatch: cannot find 004_MSF_migration_study.R from $(pwd)" >&2
  exit 1
fi

R --no-save -q < "$MSF_SCRIPT"
