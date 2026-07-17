#!/bin/bash -l

#SBATCH --job-name=run_all
#SBATCH --time=0-05:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=10G
#SBATCH --array=5
#SBATCH --mail-user=ftsiboe@hotmail.com
#SBATCH --mail-type=ALL
#SBATCH --output=/dev/null

# =============================================================================
#  WHICH STUDIES RUN
# =============================================================================
# The --array directive above decides. It is NOT always 1-9 -- check it before
# submitting, or you will run whatever the last person needed.
#
# Prefer overriding it on the command line: an override beats the directive and
# leaves no edit to forget or to commit by accident.
#
#   sbatch --array=1-9   studies/run_data_and_match_for_all.sbatch   # all
#   sbatch --array=5     studies/run_data_and_match_for_all.sbatch   # land only
#   sbatch --array=2,5   studies/run_data_and_match_for_all.sbatch   # res + land
#   sbatch --array=1-3   studies/run_data_and_match_for_all.sbatch   # a range
#   sbatch --array=1-9%2 studies/run_data_and_match_for_all.sbatch   # 2 at a time
#
# Listed tasks run as separate array jobs, in parallel if the queue allows.
#
# Do NOT override --job-name. Every 001 guards on allowed_jobnames = "run_all";
# under any other name they all quit() immediately and the job "succeeds" having
# done nothing.
#
# 004 (the frontier) is NOT here -- it is per study, e.g.
#   sbatch studies/land_tenure/scripts/job_msf.sbatch
#
# =============================================================================
#  THE CASE NUMBER AND THE SCRIPT'S GUARD MUST AGREE
# =============================================================================
# Each 001_DATA_*.R opens with run_only_for(id = N, allowed_jobnames = "run_all"),
# which quits when SLURM_ARRAY_TASK_ID != N. So the case label below and that N
# are one fact stored in two files, and nothing checks them. When they disagree
# the task exits 0 having done nothing: it prints "Skipping script. Task ID = 4
# Expected = 6" and the mail reports success. Both financial_inclusion (guarded
# to 6 while running as 4) and societal_peace_and_cohesion (pointed at a path in
# land_tenure that never existed) sat broken this way.
#
#   task  study                          id in its 001
#   1     disability                     1
#   2     resource_extraction            2
#   3     education                      3
#   4     financial_inclusion            4
#   5     land_tenure                    5
#   6     societal_peace_and_cohesion    6
#   7     time_poverty                   7
#   8     ag_services                    8
#   9     input_dealers                  (no guard -- runs whenever case 9 fires)
#
# Scripts live under <study>/scripts/ once a study is consolidated, and at the
# study root before that. Check the path exists before adding or moving a case:
# a missing file fails the task loudly, which is the better half of the problem.
# The mismatched guard is the half that fails quietly.
#
# --array must cover every case. Add a study, extend the range.

module purge
module load RStudio-Server/2023.09.1+494-foss-2023a-Java-11-R-4.3.2

case "$SLURM_ARRAY_TASK_ID" in
  1)
    echo "Running Disability Study"
    R --no-save -q < studies/disability/scripts/001_DATA_disability_study.R
    R --no-save -q < studies/disability/scripts/002_MATCHING_disability_study.R
    R --no-save -q < studies/disability/scripts/003_TREATMENT_disability_study.R
    ;;

  2)
    echo "Running Resource Extraction Study"
    R --no-save -q < studies/resource_extraction/scripts/001_DATA_resource_extraction_study.R
    R --no-save -q < studies/resource_extraction/scripts/002_MATCHING_resource_extraction_study.R
    R --no-save -q < studies/resource_extraction/scripts/003_TREATMENT_resource_extraction_study.R
    ;;

  3)
    echo "Running Education Study"
    R --no-save -q < studies/education/scripts/001_DATA_education_study.R
    R --no-save -q < studies/education/scripts/002_MATCHING_education_study.R
    R --no-save -q < studies/education/scripts/003_TREATMENT_education_study.R
    ;;

  4)
    echo "Running Financial Inclusion Study"
    R --no-save -q < studies/financial_inclusion/scripts/001_DATA_financial_inclusion_study.R
    R --no-save -q < studies/financial_inclusion/scripts/002_MATCHING_financial_inclusion_study.R
    R --no-save -q < studies/financial_inclusion/scripts/003_TREATMENT_financial_inclusion_study.R
    ;;

  5)
    echo "Running Land Tenure Study"
    R --no-save -q < studies/land_tenure/scripts/001_DATA_land_tenure_study.R
    R --no-save -q < studies/land_tenure/scripts/002_MATCHING_land_tenure_study.R
    R --no-save -q < studies/land_tenure/scripts/003_TREATMENT_land_tenure_study.R
    ;;

  6)
    echo "Running Societal Peace and Cohesion Study"
    R --no-save -q < studies/societal_peace_and_cohesion/scripts/001_DATA_societal_peace_and_cohesion_study.R
    R --no-save -q < studies/societal_peace_and_cohesion/scripts/002_MATCHING_societal_peace_and_cohesion_study.R
    R --no-save -q < studies/societal_peace_and_cohesion/scripts/003_TREATMENT_societal_peace_and_cohesion_study.R
    ;;

  7)
    echo "Running Time Poverty Study"
    R --no-save -q < studies/time_poverty/scripts/001_DATA_time_poverty_study.R
    R --no-save -q < studies/time_poverty/scripts/002_MATCHING_time_poverty_study.R
    R --no-save -q < studies/time_poverty/scripts/003_TREATMENT_time_poverty_study.R
    ;;

  8)
    echo "Running Ag Services Study"
    R --no-save -q < studies/ag_services/scripts/001_DATA_ag_services_study.R
    R --no-save -q < studies/ag_services/scripts/002_MATCHING_ag_services_study.R
    R --no-save -q < studies/ag_services/scripts/003_TREATMENT_ag_services_study.R
    ;;

  9)
    echo "Running Input Dealers Study"
    R --no-save -q < studies/input_dealers/scripts/001_DATA_input_dealers_study.R
    R --no-save -q < studies/input_dealers/scripts/002_MATCHING_input_dealers_study.R
    R --no-save -q < studies/input_dealers/scripts/003_TREATMENT_input_dealers_study.R
    ;;
esac
