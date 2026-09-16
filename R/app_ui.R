app_ui <- function() {
  shiny::fluidPage(
    title = "Structured Expert Elicitation-AMCHSS",
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      shiny::tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Cormorant+Garamond:wght@600;700&family=Source+Sans+3:wght@400;600;700&display=swap"
      ),
      shiny::tags$script(shiny::HTML(
        "document.addEventListener('shiny:connected', function() {
          var lastSent = 0;
          function activity() {
            var now = Date.now();
            if (now - lastSent > 30000) {
              lastSent = now;
              Shiny.setInputValue('ee_activity', now, {priority: 'event'});
            }
          }
          ['click', 'keydown', 'mousemove', 'touchstart'].forEach(function(name) {
            document.addEventListener(name, activity, {passive: true});
          });
          activity();
        });
        Shiny.addCustomMessageHandler('ee_oidc_redirect', function(url) {
          window.location.assign(url);
        });"
      ))
    ),
    shiny::uiOutput("root")
  )
}
