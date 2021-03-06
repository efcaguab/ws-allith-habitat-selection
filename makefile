all: analize

WL?=8
CORES?=4

analize: acoustic visual

acoustic: ./data/processed/models_acoustic_random.rds ./data/processed/models_acoustic_fixed/model_1.rds

./data/processed/AllDetections.RData: ./code/01_prepare_data.R ./data/raw/AllDetections_Lith.csv ./data/raw/AllEvents_Lith.csv ./data/raw/ArrayEvents_Lith.csv ./data/raw/WSTags_Lith.csv ./data/raw/Stations_Lith.csv
	Rscript $< 

./data/processed/probability_acoustic_detection.rds: ./code/02_calculate_lags.R	./data/processed/AllDetections.RData
	Rscript $< --window_length $(WL) --ncores $(CORES)

./data/processed/models_acoustic_random.rds: ./code/03_acoustic_models_random_effects.R ./data/processed/probability_acoustic_detection.rds
	Rscript $< --ncores $(CORES)

./data/processed/models_acoustic_fixed/model_1.rds: ./code/04_acoustic_models_fixed_effects.R ./data/processed/probability_acoustic_detection.rds
	Rscript $< --ncores $(CORES)

visual: ./data/processed/models_visual_random.rds ./data/processed/models_visual_fixed.rds

./data/processed/probability_visual_detection.rds: ./code/11_prepare_data_visual.R ./data/raw/Survey_Effort_Lith.csv ./data/raw/Survey_Effort_Lith.csv ./data/processed/AllDetections.RData
	Rscript $< --window_length $(WL) --ncores $(CORES)

./data/processed/models_visual_random.rds: ./code/12_visual_models_random_effects.R ./data/processed/probability_visual_detection.rds
	Rscript $< --ncores $(CORES)

./data/processed/models_visual_fixed.rds: ./code/13_visual_models_fixed_effects.R ./data/processed/probability_visual_detection.rds
	Rscript $< --ncores $(CORES)
