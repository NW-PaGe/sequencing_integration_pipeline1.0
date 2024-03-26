source("renv/activate.R")

if(file.exists(file.path(Sys.getenv("USERPROFILE"),"Anaconda3/envs/seq_env/python.exe"))){
  Sys.setenv(RETICULATE_PYTHON = file.path(Sys.getenv("USERPROFILE"),"Anaconda3/envs/seq_env/python.exe"))
}else if(file.exists(file.path(Sys.getenv("USERPROFILE"),".conda/envs/seq_env/python.exe"))){
  Sys.setenv(RETICULATE_PYTHON = file.path(Sys.getenv("USERPROFILE"),".conda/envs/seq_env/python.exe"))
}
