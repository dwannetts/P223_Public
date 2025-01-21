README

Over 27 years, between 1981 and 2008, the AMIRA P223 project series produced an extensive body of EM modelling and inversion programs used by the minerals, environmental and defence industries for planning and interpreting EM surveys, and for the development of new EM exploration instruments. Historically, the software generated by these projects has been available only to the project sponsors and their designated contractors. All programs are now commercially available through EMIT’s Maxwell graphical user interface. From 2010, the Fortran 90 source code for all programs will be open source. The models for both forward and inverse modelling include 
* a general 3D full domain finite elements (Loki class), 
* 3D compact finite-elements (Samaya class), 
* 2.5D full-domain finite-elements (Arjuna class), 
* multiple 3D plates embedded in a multi-layered host (Leroi class) and a 
* 1D layered earth (Airbeo and Beowulf). 

The programs can be used for any frequency or time-domain airborne, ground or downhole EM system. Sources can include multi-vertex closed loops, grounded wires, magnetic dipoles and plane waves. Receiver types can include multi-vertex loops, grounded wires, and magnetic and electric dipoles. Survey types include the variety of airborne configurations, fixed sources with independent surface or downhole receiver lines, moving sources with multiple fixed-offset receivers and magnetotellurics.Inversion for all model classes is based on an iterative, damped SVD method which concentrates on those parameters that most affect the data whilst ignoring those that are irrelevant.

---

Codes were released to the general public in 2010. This Gitlab repository makes the codes more easily available to interested users and, more importantly, provides a straightforward mechanism to update codes.

Unlike the original release, this site provides source files, Makefile(s) and examples; no executables are provided. Executables can be compiled after editing (supplied) Makefiles to suit your particular environment.  Codes have been compiled using GFortran (4.7+) and Intel ifort (versions from 2009 onwards).  Documentation may be generated using doxygen.

Program operation has been simplified but continues to run from the command line through shell scripts.  Such scripts are provided for linux systems and are easily adapted to Windows.  Both scripts and executables can be installed by typing `make install` which copies by the script and the executable to `~/bin` by default.  Program operation is through `script input` where `script` is the program name and `input` is the name of the `CFL` file containing input data and the name of the `INV` file if a code is run in inversion mode.  After a successful run, the `OUT` and `MV1` or `MF1` files are created.
    
The definitive reference for the codes remains Raiche et al (2007) (http://dx.doi.org/10.1071/ASEG2007ab114). Annetts and Cucuzza (2018) (https://doi.org/10.1071/ASEG2018abW8_3D) provided an update as to how the code had faired in the 10 years since the initial release.

I welcome any inquiry surrounding the codes and their use.

David Annetts, Aegis Geophysics

01 January, 2025

david.annetts@iinet.net.au
