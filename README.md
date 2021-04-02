# Exploring Topic-Metadata Relationships with the STM: A Bayesian Approach

In this study, we estimate a [Structural Topic Model (STM)](https://doi.org/10.1080/01621459.2016.1141684) (Roberts et al., 2016) and explore the relationship between topic proportions and metadata, providing enhanced analytical tools with improved statistical properties.

### Folder Structure

We assume the following folder structure for this project:
```
    .
    ├── code       # Contains one R file (emnlp_2021.R), used to estimate topic model and topic-metadata relationships
    ├── data       # Contains data relevant to our analyses (e.g., estimated topic model)
    │              #   - Data was created with emnlp_2021.R (see description of data in code file)
    │              #   - Raw data (Twitter posts + metadata) can be found on https://figshare.com/s/7a728fcb6d67a67fc3d6
    ├── plots      # Contains all plots from the paper; code to create plots can be found in emnlp_2021.R
```
