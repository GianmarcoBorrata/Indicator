
# Indicator Package

The Indicator package is a versatile tool designed for constructing
composite indicators, imputing missing data, evaluating imputation
results, and normalizing data. It offers a range of functions to
streamline the process of handling complex datasets, making it an
essential resource for researchers, analysts, and data scientists.

# Key Features

- **Composite Indicator Construction**: Implement various composite
  indicators such as the Mazziotta-Pareto Index, Adjusted
  Mazziotta-Pareto Index, Geometric aggregation, Linear aggregation, and
  more.
- **Missing Data Imputation**: Utilize techniques like Linear Regression
  Imputation, Hot Deck Imputation, etc., to fill in missing values
  effectively.
- **Evaluation Metrics**: Assess the quality of missing data imputation
  using metrics like R^2, RMSE, and MAE for informed decision-making.
- **Data Normalization**: Standardize and normalize data using methods
  like Standardization by Adjusted Mazziotta-Pareto method,
  Normalization by Adjusted Mazziotta-Pareto method, and others.

# Installation

You can install the Indicator package from CRAN using:
<https://CRAN.R-project.org/package=Indicator>

# install.packages(“devtools”)

devtools::install_github(“username/Indicator”)

# References

- OECD/European Union/EC-JRC (2008), “Handbook on Constructing Composite
  Indicators: Methodology and User Guide”, OECD Publishing, Paris,
  <DOI:10.1787/533411815016>
- Matteo Mazziotta & Adriano Pareto (2018), “Measuring Well-Being Over
  Time: The Adjusted Mazziotta–Pareto Index Versus Other
  Non-compensatory Indices”, Social Indicators Research, Springer,
  vol. 136(3), pages 967-976, April, <DOI:10.1007/s11205-017-1577-5>
- De Muro P., Mazziotta M., Pareto A. (2011), “Composite Indices of
  Development and Poverty: An Application to MDGs”, Social Indicators
  Research, Volume 104, Number 1, pp. 1-18,
  <DOI:10.1007/s11205-010-9727-z>
