#!/bin/bash
 
/share/apps/R/3.2.3/bin/Rscript --no-save --no-restore "$@" --this_task $SGE_TASK_ID
