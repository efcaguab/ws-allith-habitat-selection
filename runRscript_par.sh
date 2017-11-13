#!/bin/bash
# 
Rscript --no-save --no-restore "$@" --this_task $SGE_TASK_ID
