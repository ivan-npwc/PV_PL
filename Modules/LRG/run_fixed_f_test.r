# superseded: the whole test (build with fixed f, export, HF1, comparison) now runs from one file:
#   source("C:/Users/usato/SSL_UI/GIT/PV_PL/Modules/LRG/Agisoft_create_model_fixed_f.r")
# (flight list and settings are at the top of that file). compare_fixed_f.R remains as a stand-alone comparison.
source(file.path(dirname(sys.frame(1)$ofile), "Agisoft_create_model_fixed_f.r"))
