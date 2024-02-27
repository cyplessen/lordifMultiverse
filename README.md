# lordifMultiverse

`lordifMultiverse` is an R package designed for detecting differential item functioning (DIF) using logistic regression in a multiverse analysis approach. This package allows the user to test various thresholds and criteria for DIF in a flexible and comprehensive manner.

## Installation

You can install the development version from [GitHub](https://github.com/yourusername/lordifMultiverse) with:

``` r
# install.packages("devtools")
devtools::install_github("yourusername/lordifMultiverse")
```

## Usage

Here's a basic example of how to use lordifMultiverse:

``` r
library(lordifMultiverse)


# Assuming 'items' is a dataframe of item responses and 'group' is a binary group variable
results <- lordif_multiverse(items = your_data, group = your_group_variable)
```

For more detailed usage and examples, please refer to the vignettes:

``` r
browseVignettes(package = "lordifMultiverse")
```

## Features

- Perform logistic regression-based DIF detection.
- Use lordifWrapper function to test for DIF across multiple criteria and thresholds.
- Handle warnings gracefully during the DIF detection process.
- Offer verbose output for detailed analysis tracking.

## Contributing

Contributions are welcome, and they are considered on a case-by-case basis. If you would like to contribute, please follow these steps:

- Fork the repository on GitHub.
- Create a new branch for your feature or fix.
- Commit your changes to the branch.
- Push your branch to GitHub.
- Submit a pull request to the lordifMultiverse repository.

Please note that by contributing, you agree that your contributions will be licensed under its MIT License.

## License

This package is free and open-source software licensed under the MIT License.

## Acknowledgements

This package was developed by Constantin Yves Plessen. Special thanks to contributors and users who have offered valuable feedback and suggestions.

## Contact

If you have any questions or feedback, please open an issue on the GitHub repository.
