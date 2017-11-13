#!/bin/bash

## ACOUSTIC DATA -----
# prepare
qsub -N prepare_a -V -cwd -j y -S /bin/bash -m a -M efc29@uclive.ac.nz -r yes -l mem_free=8G,h_vmem=8G runRscript.sh ./code/01_prepare_data.R
# acoustic lags
qsub -N pad -hold_jid prepare_a -V -cwd -j y -S /bin/bash -m a -M efc29@uclive.ac.nz -r yes -l mem_free=8G,h_vmem=8G runRscript.sh ./code/02_calculate_lags.R --window_length 4
# random effect models
qsub -N mar -hold_jid pad -pe multi_thread 2 -V -cwd -j y -S /bin/bash -m a -M efc29@uclive.ac.nz -r yes -l mem_free=8G,h_vmem=8G runRscript.sh ./code/03_acoustic_models_random_effects.R --ncores 2
#fixed effect models
qsub -N maf -hold_jid pad -pe multi_thread 16 -V -cwd -j y -S /bin/bash -m a -M efc29@uclive.ac.nz -r yes -l mem_free=8G,h_vmem=8G runRscript.sh ./code/04_acoustic_models_fixed_effects.R --ncores 16

## VISUAL DATA ----
# visual lags
qsub -N pvd -hold_jid prepare_a -V -cwd -j y -S /bin/bash -m a -M efc29@uclive.ac.nz -r yes -l mem_free=8G,h_vmem=8G runRscript.sh ./code/11_prepare_data_visual.R --window_length 4
# random effect models
qsub -N mvr -hold_jid pvd -pe multi_thread 2 -V -cwd -j y -S /bin/bash -m a -M efc29@uclive.ac.nz -r yes -l mem_free=8G,h_vmem=8G runRscript.sh ./code/12_visual_models_random_effects.R --ncores 2
# fixed effect models
qsub -N mvf -hold_jid pvd -pe multi_thread 8 -V -cwd -j y -S /bin/bash -m a -M efc29@uclive.ac.nz -r yes -l mem_free=8G,h_vmem=8G runRscript.sh ./code/13_visual_models_fixed_effects.R --ncores 8
