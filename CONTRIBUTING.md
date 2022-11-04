# DoubleML - Contributing Guidelines <a href="https://docs.doubleml.org"><img src="man/figures/logo.png" align="right" width = "120" /></a>

DoubleML is a community effort.
Everyone is welcome to contribute.
All contributors should adhere to this contributing guidelines
and our [code of conduct](https://github.com/DoubleML/doubleml-for-r/blob/main/CODE_OF_CONDUCT.md).
The contributing guidelines are particularly helpful to get started for your first contribution.

## Submit a Bug Report :bug:
To submit a **bug report**, you can use our
[issue template for bug reports](https://github.com/DoubleML/doubleml-for-r/issues/new/choose).

- A good bug report contains a **minimum reproducible code snippet**, like for example

```R
library(DoubleML)
library(mlr3)
library(mlr3learners)
library(data.table)
set.seed(2)
ml_g = lrn("regr.ranger", num.trees = 10, max.depth = 2)
ml_m = ml_g$clone()
obj_dml_data = make_plr_CCDDHNR2018(alpha = 0.5)
dml_plr_obj = DoubleMLPLR$new(obj_dml_data, ml_g, ml_m)
dml_plr_obj$fit()
dml_plr_obj$summary()
```

- State the **result you would have expected** and the **result you actually got**.
In case of an exception the full traceback is appreciated.

- State the **versions of your code** by running the following lines and copy-paste the result.

```R
sessionInfo()
packageVersion('DoubleML')
packageVersion('mlr3')
```

## Submit a Feature Request :bulb:
We welcome **feature requests and suggestions** towards improving and/or extending the DoubleML package.
For feature requests you can use the corresponding
[issue template](https://github.com/DoubleML/doubleml-for-r/issues/new/choose).

## Submit a Question or Start a Discussion
We use **[GitHub Discussions](https://github.com/DoubleML/doubleml-for-r/discussions)** to give the community a platform
for asking questions about the DoubleML package and for discussions on topics related to the package.

## Contribute Code :computer:
Everyone is welcome to contribute to the DoubleML code base.
The following guidelines and hints help you to get started.

### Development Workflow
In the following, the recommended way to contribute to DoubleML is described in detail.
If you are just starting to work with Git and GitHub, we recommend to read [Happy Git and GitHub for the useR](https://happygitwithr.com/index.html) and the [chapter on Git and GitHub](https://r-pkgs.org/git.html) in Hadley Wickham's book R Packages.
The most important steps are: To **fork** the repo, then **add your changes** and finally submit a **pull-request**.
1. **Fork** the [DoubleML repo](https://github.com/DoubleML/doubleml-for-r)
by clicking on the Fork button (this requires a GitHub account).

2. **Clone** your fork to your local machine via
```bash
$ git clone git@github.com:YourGitHubAccount/doubleml-for-r.git
$ cd doubleml-for-r
```

3. Create a **feature branch** via
```bash
$ git checkout -b my_feature_branch
```

4. (Optionally) you can add the `upstream` remote.
```bash
$ git remote add upstream https://github.com/DoubleML/doubleml-for-r.git
```
This allows you to easily keep your repository in synch via
```bash
$ git fetch upstream
$ git merge upstream/main
```

5. **Develop** your code changes. A helpful resource for package development in R
is Hadley Wickham's [R Packages](https://r-pkgs.org/preface.html) and [rOpenSci Packages](https://devguide.ropensci.org/).
The changes can be added and pushed via
```bash
$ git add your_new_file your_modified_file
$ git commit -m "A commit message which briefly summarizes the changes made"
$ git push origin my_feature_branch
```

6. Generate a **pull request** from your fork.
Please follow our guidelines for pull requests.
When opening the PR you will be guided with a checklist.

### Checklist for Pull Requests (PR)
- [x] The **title** of the pull request summarizes the changes made.

- [x] The PR contains a **detailed description** of all changes and additions
(you may want to comment on the diff in GitHub).

- [x] **References** to related issues or PRs are added.

- [x] The code passes `R CMD check` and **all (unit) tests**.
To check your code for common problems, run
```R
devtools::check()
```
By default, this runs all tests. In case you only want to run the tests, run
```R
devtools::test()
```

- [x] If you add an **enhancements** or **new feature**, **unit tests**
(with a certain level of coverage) are **mandatory** for getting the PR merged.

- [x] Check whether your changes adhere to the **"mlr-style" standards**.
For the check you can use the following code
```R
require(styler)
remotes::install_github("pat-s/styler@mlr-style")
styler::style_pkg(style = styler::mlr_style) # entire package
styler::style_file(<file>, style = styler::mlr_style) # specific file
```

If your PR is still **work in progress**, please consider marking it a **draft PR**
(see also [here](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/creating-a-pull-request)).

### Unit Tests and Test Coverage
We use the **testthat** package for unit testing.
Unit testing is considered to be a fundamental part of the development workflow.
We recommend to read the [chapter on testing](https://r-pkgs.org/tests.html) of Hadley Wickham's book R Packages.
The tests are located in the `tests/testthat` subfolder.
The test coverage is determined with the `covr` package.
Coverage reports for the package, PRs, branches etc. are available from
[codecov](https://app.codecov.io/gh/DoubleML/doubleml-for-r).
It is mandatory to equip new features with an appropriate level of unit test coverage.
To **run all unit tests** (for further option see the [devtools docu](https://devtools.r-lib.org/reference/test.html)) call
```R
devtools::test()
```
For a unit test coverage report you can run
```R
covr::report()
```
or, alternatively,
```R
devtools::test_coverage()
```

### Contribute a New Model Class
The **DoubleML package** is particularly designed in a flexible way to make it **easily extendable** with regard to
**new model classes**.
**Contributions** in this direction **are very much welcome**, and we are happy to help authors to integrate their models in the
DoubleML OOP structure.
If you need assistance, just open an issue or contact one of the maintainers
[@MalteKurz](https://github.com/MalteKurz) or [@PhilippBach](https://github.com/PhilippBach).

The **abstract base class `DoubleML` implements all core functionalities** based on a linear Neyman orthogonal score
function.
To contribute a new model class, you only need to **specify all nuisance functions** that need to be estimated for the
new model class (e.g. regressions or classifications).
Furthermore, the **score components for the Neyman orthogonal score function need to be implemented**.
All other functionality is automatically available via inheritance from the abstract base class.
<!---TODO: Add a model template for the R package DoubleML
A **template for new model classes** is available
[here](https://github.com/DoubleML/doubleml-docs/blob/main/model_templates/double_ml_model_template.py).--->

## Contribute Documentation :books:

The **documentation** of DoubleML is generated with **roxygen2**. The corresponding website
for the R API documentation is generated using **pkgdown** and hosted at
[https://docs.doubleml.org/r/stable](https://docs.doubleml.org/r/stable/).
The website [https:://docs.doubleml.org](https://docs.doubleml.org) is built with
sphinx.
The source code for the website, user guide, example gallery, etc. is available in a separate repository
[https://github.com/DoubleML/doubleml-docs](https://github.com/DoubleML/doubleml-docs).

### Contribute to the API Documentation
The **documentation** of DoubleML is generated with **roxygen2**. The corresponding website
for the R API documentation is generated using **pkgdown**.
To build the documentation of the package run
```R
devtools::document()
```
To build the documentation website, run (for more details, see the [pkgdown documentation](https://pkgdown.r-lib.org/))
```R
pkgdown::build_site()
```

### Contribute to the User Guide and Documentation
The **documentation of DoubleML** is hosted at [https://docs.doubleml.org](https://docs.doubleml.org).
The **source code** for the website, user guide, example gallery, etc. is available in a **separate repository
[doubleml-docs](https://github.com/DoubleML/doubleml-docs)**.
Changes, issues and PRs for the documentation (except the API documentation) should be discussed in the 
[doubleml-docs](https://github.com/DoubleML/doubleml-docs) repo.
We welcome contributions to the user guide, especially case studies for the
[example gallery](https://docs.doubleml.org/stable/examples/index.html).
A step-by-step guide for contributions to the example gallery is available
[here](https://github.com/DoubleML/doubleml-docs/wiki/Contribute-to-our-Website-and-Example-Gallery).
