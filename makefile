all: analize

analize: prepare

prepare: ./data/processed/AllDetections.RData

raw_data: ./data/raw/AllDetections_Lith.csv ./data/raw/AllEvents_Lith.csv ./data/raw/ArrayEvents_Lith.csv ./data/raw/WSTags_Lith.csv ./data/raw/Stations_Lith.csv

./data/processed/AllDetections.RData: ./code/01_prepare_data.R raw_data 
	Rscript $< 

