#!/bin/bash

make WL=8
OUT=$?
if [ $OUT -eq 0 ];then
  mkdir ./data/processed/results_window8/
  find ./data/processed -type f -print0 | xargs -0 mv -t ./data/processed/results_window8/
  make WL=4
  OUT2=$?
  if [ $OUT2 -eq 0 ];then
    mkdir ./data/processed/results_window4/
    find ./data/processed -type f -print0 | xargs -0 mv -t ./data/processed/results_window4/
    make WL=2
    OUT3=$?
    if [ $OUT3 -eq 0 ];then
      ./data/processed/results_window2/
      find ./data/processed -type f -print0 | xargs -0 mv -t ./data/processed/results_window2/
    fi
  fi
fi
   
