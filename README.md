
<!-- README.md is generated from README.Rmd. Please edit that file -->

# silvia

<!-- badges: start -->
<!-- badges: end -->

Le but de silvia est d’estimer et de visualiser les stocks et les flux
de carbone sur un territoire.

## Installation

Il est possible de télécharcher la version en développement de Silvia
depuis [GitHub](https://github.com/) avec :

``` r
# install.packages("devtools")
devtools::install_github("silvia-team/silvia")
```

Avant d’utiliser les fonctions du package, il faut définir un dossier
(de préférence vide) dans lequel les fichiers téléchargés seront
stockés. La fonction `setup_path()` permet de vérifier que le dossier
choisi existe bien à l’emplacement renseigné, et d’inititialiser les
sous-fichers dans lesquels les données téléchargées seront stockées.

## Example d’initialisation

Voici un exemple d’initialisation de l’outil.

``` r
library(silvia)
#> Warning in CPL_gdal_init(): GDAL Message 1: Unable to find driver DODS to unload
#> from GDAL_SKIP environment variable.

#> Warning in CPL_gdal_init(): GDAL Message 1: Unable to find driver DODS to unload
#> from GDAL_SKIP environment variable.
#> Please make sure you are connected to the internet.

data_path <- "C:/Users/bohnenkl/Documents/data_silvia"
data_path <- setup_path(data_path = data_path)
#> Warning in dir.create(here(data_path, "territory")):
#> 'C:\Users\bohnenkl\Documents\data_silvia\territory' existe déjà
#> Warning in dir.create(here(data_path, "copernicus")):
#> 'C:\Users\bohnenkl\Documents\data_silvia\copernicus' existe déjà
#> Warning in dir.create(here(data_path, "corine_land_cover")):
#> 'C:\Users\bohnenkl\Documents\data_silvia\corine_land_cover' existe déjà
#> Warning in dir.create(here(data_path, "bd_foret")):
#> 'C:\Users\bohnenkl\Documents\data_silvia\bd_foret' existe déjà
#> 
#> Downloaded data will be stored at C:/Users/bohnenkl/Documents/data_silvia
```
