meanit
======

An R package for creating composite PNGs or JPEGs

__Author:__ Oliver Keyes<br>
__License:__ [MIT](http://opensource.org/licenses/MIT)<br>
__Status:__ Stable

Description
======
meanit is an [R](http://www.r-project.org/) package designed to generate composite images. It currently handles grayscale and full PNGs, as well as JPEGs, through the sole user-accessible function (_also_ called 'meanit').

Using the package is relatively simple and explained in the .Rd help files, but to summarise: meanit takes a list of provided absolute filenames or URLs, validates them, retrieves the images they link to, trims them to a consistent size and generates a single composite image of the same file format.

Things you cannot do with meanit and should feel bad for even thinking of
======
* Mix JPEGs and PNGs in your file list;
* Mix full PNGs and grayscale PNGs (you _can_, but the resulting image will be grayscale);
* Teach a robot how to love.

Dependencies
======
* [RCurl](http://cran.r-project.org/web/packages/RCurl/index.html)
* [jpeg] (http://cran.r-project.org/web/packages/jpeg/index.html)
* [png] (http://cran.r-project.org/web/packages/png/index.html)