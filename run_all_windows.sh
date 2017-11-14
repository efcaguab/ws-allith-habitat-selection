#!/bin/bash

make WL=8
find ./data/processed -type f -print0 | xargs -0 mv -t .data/processed/results_window8/
make WL=4
find ./data/processed -type f -print0 | xargs -0 mv -t .data/processed/results_window4/
make WL=2
find ./data/processed -type f -print0 | xargs -0 mv -t .data/processed/results_window2/
