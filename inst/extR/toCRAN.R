


#file.copy(from = file.path('../tzh/R', paste0(c(
  # 'format_named' # now lives only in \pkg{ThomasJeffersonUniv}
#), '.R')), to = './R', overwrite = TRUE)

devtools::load_all('../packageAdvanced')
removeLocalPackage('ThomasJeffersonUniv')
updateDESCRIPTION('.')
checkDocument('.')
checkRelease('.')

