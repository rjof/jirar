#message('library paths:\n', paste('... ', .libPaths(), sep='', collapse='\n'))

#message('The working directory is:\n   ', getwd())

#chrome.portable = file.path(getwd(), '/app', 'GoogleChromePortable/App/Chrome-bin/chrome.exe')

#launch.browser = function(appUrl) {
#    shell(sprintf('"%s" --app=%s', 
#                  chrome.portable, 
#                  appUrl))
#}	
#shiny::runApp('./app/shiny', launch.browser=launch.browser)
shiny::runApp('./app/shiny/R', launch.browser=T)