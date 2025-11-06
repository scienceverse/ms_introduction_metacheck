
base_dir <- getwd() #here, same as the directory of this jupyter notebook file, in Rstudio you would need this command:
#base_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
base_dir

data_folder <- '/data'

figures_folder <- '/figures'

bayesian <- '/bayesian'
frequentist <- '/frequentist'

results_folder <- '/results'
results_mb <- '/results_memory_bias'
results_sb <- '/results_subjective_belief'
results_gambles <- '/results_gambles'


make_sure_path_exists <- function(main_dir,sub_dir){
    ifelse(!dir.exists(file.path(main_dir, sub_dir)), dir.create(file.path(main_dir, sub_dir)), 'directory already existent')
}

make_sure_path_exists(base_dir, data_folder)
make_sure_path_exists(base_dir, results_folder)
make_sure_path_exists(base_dir, figures_folder)

data_dir <- paste0(base_dir,data_folder)
results_dir <- paste0(base_dir,results_folder)

make_sure_path_exists(results_dir, results_mb)
make_sure_path_exists(results_dir, results_sb)
make_sure_path_exists(results_dir, results_gambles)

make_sure_path_exists(paste0(results_dir, results_mb),bayesian)
make_sure_path_exists(paste0(results_dir, results_sb),bayesian)
make_sure_path_exists(paste0(results_dir, results_gambles),bayesian)

make_sure_path_exists(paste0(results_dir, results_mb),frequentist)
make_sure_path_exists(paste0(results_dir, results_sb),frequentist)
make_sure_path_exists(paste0(results_dir, results_gambles),frequentist)
