# renv Setup and Usage

This project uses [renv](https://rstudio.github.io/renv/) for R dependency management to ensure reproducible package environments.

## For New Contributors

When you first clone this repository and open it in R/RStudio:

1. The `.Rprofile` file will automatically source `renv/activate.R`
2. renv will bootstrap itself if not already installed
3. Run `renv::restore()` to install all package dependencies at the versions specified in `renv.lock`

```r
# Restore packages from lockfile
renv::restore()
```

## For Maintainers

### After Adding New Package Dependencies

When you add new packages to the project (via `DESCRIPTION` or by installing packages directly):

```r
# Update renv.lock with new dependencies
renv::snapshot()

# Commit the updated renv.lock file
# git add renv.lock
# git commit -m "Update renv.lock with new dependencies"
```

### Checking Package Status

```r
# See which packages have changed
renv::status()
```

### Updating Packages

```r
# Update a specific package
renv::update("packagename")

# Update all packages
renv::update()

# Snapshot the changes
renv::snapshot()
```

## Initial Setup (Already Done)

The renv infrastructure has already been set up for this project. The following files/directories are part of the renv configuration:

- `.Rprofile` - Sources `renv/activate.R` to activate renv on project startup
- `renv/activate.R` - renv bootstrap script
- `renv/settings.dcf` - renv configuration settings
- `renv/.gitignore` - Excludes renv cache from version control
- `renv.lock` - (To be created) Lockfile specifying exact package versions

## More Information

- [renv Documentation](https://rstudio.github.io/renv/)
- [renv Introduction Vignette](https://rstudio.github.io/renv/articles/renv.html)
