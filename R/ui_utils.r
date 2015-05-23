examples.add.url.input = function() {
  app = eventsApp()
   ui = fluidPage(
    wellPanel(
      textOutput("mytext"),
      actionButton("mybtn","go")
    )
  )
  buttonHandler("mybtn", function(app=getApp(),...){
    clientData = app$session$clientData
    query <- parseQueryString(clientData$url_search)
    url = paste0(names(query),"=",query, collapse="; ")
    cat(url)
    setText("mytext",url)
  })
  runEventsApp(app,ui=ui, launch.browser=TRUE)
  runEventsApp(app,ui=ui,launch.browser=rstudio::viewer)
}

urlInput = function(id = "app_url") {
  li = list(
    tags$head(
      tags$script("
        $(document).ready(function() {
          $('#app_hash').val(window.location.search);
        });",
        type = 'text/javascript'
      )
    ),
    tags$input(id = id, type = 'text', style = 'display:none;')
  )
  li

}

