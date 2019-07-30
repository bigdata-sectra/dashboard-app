# dashboard-app
docker run --rm -it --name dashboard-app -p 5556:5556 --env-file env.list -v ${PWD}:/usr/src/code -w /usr/src/code rocker/shiny-verse /bin/bash
R
install.packages("fasttime")
install.packages("leaflet")
install.packages("plotly")
install.packages("shinycssloaders")
install.packages("shinydashboard")

shiny::runApp(port=5556, launch.browser = FALSE, host='0.0.0.0')

^C
quit()
exit