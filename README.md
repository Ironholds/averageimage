averageimage
======

An R package for creating composite PNGs or JPEGs

__Author:__ [Oliver Keyes](http://ironholds.org)<br/>
__License:__ [MIT](http://opensource.org/licenses/MIT)<br/>
__Status:__ Stable

Description
======
averageimage is an R package designed to generate composite images. It currently handles grayscale and full PNGs, as well as JPEGs, through the sole user-accessible function (_also_ called 'meanit').

Using the package is relatively simple and explained in the .Rd help files, but to summarise: provide a list or vector of:

* already-read-in JPEGs or PNGs, or;
* URLs linking to JPEGs or PNGS, or;
* absolute file locations linking to JPEGs or PNGS.

Combine it with the name of a save file, and meanit will take the images you're pointing to, trim them to a consistent size and generate a single composite image of the same type.

Usage
======
Things you can do with averageimage and absolutely should:

* Generate beautiful composite JPEGs or PNGs.
* Join me in pushing R over the threshold of Zawinski's law.

Things you cannot do with averageimage and should feel bad for even thinking of:

* Mix JPEGs and PNGs in your file list;
* Mix full PNGs and grayscale PNGs (you _can_, but the resulting image will be grayscale);
* Teach a robot how to love.

Dependencies
======
* [downloader](https://cran.r-project.org/web/packages/downloader/index.html)
* [png](https://cran.r-project.org/web/packages/jpeg/index.html)
* [jpeg](https://cran.r-project.org/web/packages/png/index.html)
