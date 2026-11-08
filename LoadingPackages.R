##
# Loading and Optionally Installing Packages
# Keith Chamberlain
##


install_missing_packages <- function(packages){
    '
    Install missing packages checks to see if a package is in the installed
    packages (returns a T/F for each installed package comparison), if it is,
    i.e. if there isnt a TRUE for any x vs Package combination, then install it.
    Returns a list of installed packages.
    '
    lapply(packages,
           FUN = function(x) {
               if(!any(x %in% data.frame(installed.packages())$Package)) {
                   install.packages(x)
                   c(x, "Installed")
               } else {c(x, "Already Installed")}
       })
}


load_packages <- function(packages){
    '
    Loads packages from a character vector. Returns a list of package loading
    statuses.
    '
    lapply(packages, function(x) {
        eval(parse(text=paste("library(",x, ")")))
        c(x, "loaded")
    })
}
