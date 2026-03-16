# n_workers = ceiling(parallel::detectCores()*0.5) # Parallelize code across half of cores (reduce this number if memory consumption is too high)

n_workers = parallel::detectCores()

n_runs = 32 # Number of repeats of main analysis (sampling different "control groups" and outcomes)
n_subsample_pilot = 20
use_analysis_cache = T

# future::plan(future.callr::callr, workers = n_workers);
future::plan("multisession", workers = n_workers);
progressr::handlers("cli")
options(future.globals.maxSize = 5 * 1e9) # 5GB
