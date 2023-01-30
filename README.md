
<!-- README.md is generated from README.Rmd. Please edit that file -->

# silvia

<!-- badges: start -->
<!-- badges: end -->

Le but de silvia est d’estimer et de visualiser les stocks et les flux
de carbone sur un territoire.

## Contexte

À l’échelle globale, les sols et les forêts (y compris les produits
issus du bois) stockent, sous forme de biomasse vivante ou morte, 3 à 4
fois plus de carbone que l’atmosphère. Toute variation négative ou
positive de ces stocks, même relativement faible, peut influer sur les
émissions de gaz à effet de serre. La séquestration nette de dioxyde de
carbone (CO2) est un flux net positif de l’atmosphère vers ces
réservoirs qui se traduit finalement par une augmentation des stocks.
Les enjeux actuels et futurs de la séquestration de carbone combinés
avec la fragilité de ces puits de carbone naturels motivent le besoin de
mieux suivre les flux de carbone entre sols, biomasse et atmosphère.

L’idée de ce projet Silvia est d estimer les flux de carbone entre sols,
biomasse et atmosphère, à partir d’un outil Excel développé par l’ADEME,
appelé
[Aldo](https://www.territoires-climat.ademe.fr/actualite/loutil-aldo-pour-une-premiere-estimation-de-la-sequestration-carbone-dans-les-sols-et-la-biomasse)

## Installation

Il est possible de télécharcher la version en développement de Silvia
depuis [GitHub](https://github.com/) avec :

``` r
# install.packages("devtools")
devtools::install_github("silvia-team/silvia")
```

## Example d’initialisation

Avant d’utiliser les fonctions du package, il faut définir un dossier
(de préférence vide) dans lequel les fichiers téléchargés seront
stockés.

La fonction `setup_path()` permet de vérifier que le dossier choisi
existe bien à l’emplacement renseigné, et d’inititialiser les
sous-fichers dans lesquels les données téléchargées seront stockées. Ces
sous-fichiers sont :

-   `territory` -\> périmètre du territoire choisi
-   `corine_land_cover` -\> couches d’occupation des sols du territoire
    pour les années sélectionnées
-   `copernicus` -\> couches d’imperméabilisation des sols du territoire
    pour les années 2012 et 2015
-   `bd_foret` -\> couche d’occupation des sols détaillée pour les
    espaces forestiers (BD Forêt Version 2)

Voici un exemple d’initialisation de l’outil.

``` r
library(silvia)

data_path <- "C:/Users/bohnenkl/Documents/data_silvia"
data_path <- setup_path(data_path = data_path)
```
