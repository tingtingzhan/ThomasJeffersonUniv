


#file.copy(from = file.path('../tzh/R', paste0(c(
  # 'format_named' # now lives only in \pkg{ThomasJeffersonUniv}
#), '.R')), to = './R', overwrite = TRUE)

library(adv.tzh) # devtools::install_github('tingtingzhan/adv.tzh')
removeLocalPackage('ThomasJeffersonUniv')
updateDESCRIPTION('.')
document_('.')
release_('.')

