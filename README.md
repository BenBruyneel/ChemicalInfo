# Chemical Info

A simple R shiny application for the sometimes tedious job of calculating masses (actually m/z) of ions as seen in mass spectrometry

Calculations were checked against the site [enviPat Web](https://www.envipat.eawag.ch/index.php) (whose calculations have been dependable/correct so far).
The calculations in the application make use of the R package [enviPat](https://cran.r-project.org/web/packages/enviPat/index.html) and routines coming from my own [massSpectrometryR](https://github.com/BenBruyneel/massSpectrometryR) package.

Please note: I've attempted to check the calculations as much as humanly possible, but can give no guarentees on the workings or correctness of the results

The application has been deployed on [shinyapps.io](https://www.shinyapps.io/) of [Posit](https://posit.co/) (formerly RStudio) : [ChemicalInfo](https://benbruyneel.shinyapps.io/ChemicalInfo/). Please note that as I'm using a free account, the application will be deactivated after more than a certain number of hours of usage. The deployment on shinyapps.io is meant for demonstration purposes: if you wish to use it on a regular basis I kindly as/would like to suggest you to download the code and run it locally or to deploy the application on your own (posit connect) server. That's the way I use it myself at work ;)

A blog-post on the development and use can be found here: [massSpectrometryR manual](https://benbruyneel.github.io/massSpectrometryR/)

Work in progress!

June, 13th 2023
