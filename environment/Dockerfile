# hash:sha256:ef3c52ce02edbab89ee25229b4185e7b862bd6bd9dd5f3e193076f4a25ca6e6e
FROM registry.codeocean.com/codeocean/r-studio:r-4.5.1-1.2204.0-rstudio-server-2025.05.1-513-ubuntu22.04

ARG DEBIAN_FRONTEND=noninteractive

RUN Rscript -e 'remotes::install_version("Hmisc", "5.2-4")' \
    && Rscript -e 'remotes::install_version("Matrix", "1.7-4")' \
    && Rscript -e 'remotes::install_version("Rmisc", "1.5.1")' \
    && Rscript -e 'remotes::install_version("apaTables", "2.0.8")' \
    && Rscript -e 'remotes::install_version("broom", "1.0.10")' \
    && Rscript -e 'remotes::install_version("corrr", "0.4.5")' \
    && Rscript -e 'remotes::install_version("fixest", "0.13.2")' \
    && Rscript -e 'remotes::install_version("furrr", "0.3.1")' \
    && Rscript -e 'remotes::install_version("future", "1.67.0")' \
    && Rscript -e 'remotes::install_version("future.callr", "0.10.2")' \
    && Rscript -e 'remotes::install_version("ggnewscale", "0.5.2")' \
    && Rscript -e 'remotes::install_version("ggstance", "0.3.7")' \
    && Rscript -e 'remotes::install_version("ggtext", "0.1.2")' \
    && Rscript -e 'remotes::install_version("kableExtra", "1.4.0")' \
    && Rscript -e 'remotes::install_version("knitr", "1.50")' \
    && Rscript -e 'remotes::install_version("lme4", "1.1-37")' \
    && Rscript -e 'remotes::install_version("metafor", "4.8-0")' \
    && Rscript -e 'remotes::install_version("patchwork", "1.3.2")' \
    && Rscript -e 'remotes::install_version("pracma", "2.4.6")' \
    && Rscript -e 'remotes::install_version("progressr", "0.17.0")' \
    && Rscript -e 'remotes::install_version("reshape", "0.8.10")' \
    && Rscript -e 'remotes::install_version("scales", "1.4.0")' \
    && Rscript -e 'remotes::install_version("tidyverse", "2.0.0")'

COPY postInstall /
RUN /postInstall
