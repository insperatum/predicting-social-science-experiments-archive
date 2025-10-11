n_workers = parallel::detectCores() # Parallelize code across all cores (reduce this number if memory consumption is too high)
use_analysis_cache = T # Set to false to run all analyses from scratch
n_runs = 32 # Number of repeats of main analysis (sampling different "control groups" and outcomes)

# future::plan(future.callr::callr, workers = n_workers);
future::plan("multisession", workers = n_workers);
progressr::handlers("cli")
options(future.globals.maxSize = 5 * 1e9) # 5GB
