# COSMOD_project
The program is designed to work with COSmological MODels (2014-2022). Corresponding scripts plot all figures in the papers:

1)
High-redshift long gamma-ray bursts Hubble diagram as a test of basic cosmological relations
June 2020 Monthly Notices of the Royal Astronomical Society 496(2):1530–1544
DOI: 10.1093/mnras/staa1548

2)
A crucial test of the phantom closed cosmological model
October 2020 Monthly Notices of the Royal Astronomical Society Letters 499(1):L101-L104
DOI: 10.1093/mnrasl/slaa167

Schema in detail:

The multi-platform Fortran.f95 program COSMOD has the clear-code interface (file COSMOD Citadel) 
for working the scripts (file COSMOD_Scripts) 
with following plot the figures (file COSMOD_Graphics.f95) and tables (COSMOD_Tables).
The final figures and tables are preparing to upload to the overleaf website (COSMOD_overleaf).

The COSMOD creates work directory in parallel folder of the code directory (variable WorkDir). 
Additional paths can be written in COSMOD_paths, including catalogue directories.
Special algorithm parameters are shown in COSMOD_Config.
The COSMOD use the special modules, such as:
Cosmology -- cosmological models
global -- global variables and supporting functions
GNUplot -- the fortran--GNUplot interracting for plotting graphics
main -- the main of COSMOD
math -- some mathematical functions

You can ask me any questions via arhath.sis@gmail.com && arhath.sis@yandex.ru .

