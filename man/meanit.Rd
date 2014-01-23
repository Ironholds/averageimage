\name{meanit}
\alias{meanit}
\title{meanit}
\usage{
  meanit(files = NULL, urls = NULL, save.file,
    sample = NULL, imagetype)
}
\arguments{
  \item{files}{a vector of absolute file names that contain
  PNGs or JPEGs}

  \item{urls}{a vector of absolute URLs that refer to PNGs
  or JPEGs. Priority is given to files, not urls; submit
  one or the other.}

  \item{save.file}{the absolute file name to save the
  composite image into}

  \item{sample}{the number of files or URLs to take from
  the validated list. If it's left as NULL, meanit assumes
  you want all of them.}

  \item{imagetype}{the format of the image. Currently
  accepts "jpeg" or "png"}
}
\description{
  Generate composite images from a list of PNGs or JPEGs
}
\details{
  Meanit is the primary function of the package, and ties
  the internal methods and functions together in generating
  a composite image. A vector of absolute file names or
  URLs is provided, and meanit validates that the strings
  look like images, validates that they resolve to a valid
  address, and then retrieves the images and generates a
  composite of them.

  To deal with images of different sizes, the smallest
  dimensions of listed images are calculated, and each
  image is then trimmed to match those dimensions. This
  trimming operates from the middle of the image, rather
  than any edge, in order to prioritise retaining the
  'important' bits.
}
\section{Caveats}{
  Meanit accepts both PNGs (greyscale or full) and JPEGs,
  although the list of files or URLs provided should only
  refer to one type of image. If JPEGs and PNGs are
  combined, it won't work - if full and grayscale PNGs are
  provided, the returned image will be grayscale.
}

