
# LitiigiiDatabase

<!-- badges: start -->
<!-- badges: end -->

The goal of LitiigiiDatabase is to calculate provision for litigations by automatically updating litigations solutions on public website and geting user input on coefficients used for provisions depending on the solutions received. The data is stored inside a MySQL database and it is private. The main script used to update litigation data is stored inside inst/extdata/script_actualizare_sentinte.R. It connects to the API of portal.just.ro which is the public repository for litigations in Romania.

## Installation

You can install the development version of LitiigiiDatabase from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("fizic37/LitigiiRisc")
```
## IMPORTANT on installation
Installing the app will not work since the data used is private and stored on an internal MySQL server.

## Deplyment to production
Deployment to production is handled with docker image (built with Dockerfile inside the repository) and docker volumes. The docker is ran with the following command: docker run \-d \-it \-p 0.0.0.0:5000:80 \--name Litigii -v litigii_volume:/build_zone/R/reactivedata  -v logs_litigii:/build_zone/R/logs \litigii:latest

where litigii is the docker image built with docker build -t risk_management -f Dockerfile LitiigiiRisc/Dockerfile

The docker volumes created and used by the app are: litigii_volume and logs_litigii  These volumes are fed with data by the app. The current deplyment is not yet used in production.