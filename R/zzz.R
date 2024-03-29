.onAttach <- function(libname, pkgname){
    ver <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), 
                    fields=c("Version", "Date"))
    packageStartupMessage(paste(pkgname, ver[1], "\t", ver[2]))
    invisible(NULL)
}

.onLoad <- function(libname, pkgname) {
    #options(Matrix.warnDeprecatedCoerce = 2L)
    invisible(NULL)
}

.onUnload <- function(libpath){
    invisible(NULL)
}

