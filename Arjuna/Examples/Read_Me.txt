  Welcome to Arjuna 2.1.0  29 January 2004
  Please also read Arjuna.txt

  This is the second release of an EMGUI compliant Arjuna.

     CHANGES to Version 2.1.0 from from Version 2.0.4
     ------------------------------------------------

   1. In the previous version, full Arjuna acuracy was obtained only when the 
      receiver was coincident with a mesh node.  Inter-node accuracy has been 
      improved significantly by performing the inter-node interpolation in 
      the spatial domain rather than uin the wave-number domain as was done 
      previously.

   2. An error causing inaccurate and in-loop responses was identified and corrected

   Arjuna computes the 3D ground and downhole EM response of a general
   heterogeneous 2-D structure (cross section).  The cross section lies
   in the East-West vertical plane.  Lithology is allowed to vary East-
   West and with depth.  It is constant in the North-South direction.
   Similarly topography can vary East-West but not North-South.
   Arjuna is capable of modelling high contrasts accurately.

   The program can be used for inductive sources but not grounded wires.
   Magnetic dipole sources can be above, on or below the surface.  They
   can dip but their azimuth is required to lie along the East-West plane,
   perpendicular to strike.  Dipole receivers can have any dip or azimuth.

   Loop transmitters and receivers must lie on the surface.  They are
   are modelled as an area integration of magnetic dipoles rather than a
   line integral of electric dipoles.  Members of the equivalent dipole
   are given the elevation and dip angle of the ground cells in which
   they reside, thus allowing loops draped on topography to be modelled
   accurately.  Transmitter loops can be polygonal but separated receiver
   loops must be rectangular.

   Surveys can be defined with one or more groups of receivers assigned
   to each transmitter position or for transmitter and single loop or
   magnetic dipole receiver to move at constant offset.

   Time-domain responses are computed by first computing frequency-domain
   responses over several decades. The frequency-domain response is then
   extrapolated back to zero frequency.  A cubic spline interpolation of the
   imaginary component of the magnetic field is used to represent the
   frequency-domain response over the whole spectrum.  This spline is
   converted to time domain step-response using a Hankel transform.
   The response is stacked over several cycles and then convolved with the
   transmitter system waveform.  On-time and off time channels are permitted.

   When used in time-domain mode, all frequency-domain computations are
   saved in a file ArjunAir.frq which allows them to be reused for
   different time-domain systems as long as the model and all geometric
   factors remain the same.  These additional time-domain computations
   can be done in less than one percent of the time required by
   the original computation in most cases.

   The frequency-domain solutions are solved by transformation into a mixed
   spatial-Fourier domain (kx,y,z,f) where kx is the wavenumber corresponding
   to the strike direction.  In this domain, an isoparametric finite-element
   method is used to solve a coupled system of differential equations for the
   along-strike components of the magnetic and electric fields.  The global
   matrix equation is solved using the frontal solution method.  The vertical
   and transverse strike magnetic field components are then computed in this
   domain.  After obtaining solutions for 21 wavenumber values, the three
   magnetic field components are then transformed into the (x,y,z) domain.

   It is assumed that data entry will be made using one of two
   graphical user interface - either EMGUI (Encom) or Maxwell (EMIT).
   The mesh property specification now based on lithologies.

  Note that we now use borehole system components labelled:
     Axial, Slope and Horizontal.  Please read borehole.pdf

  Note the convention regarding the dip angle adopted in June 2003
  for magnetic dipole transmitters and receivers.
  The user will specify the dip of a vertical loop (horizontal dipole)
     as SXDIP = 0  rather than 90 degrees as previously.

  The user will specify the dip of a horizontal loop (vertical dipole)
     as SXDIP = 90  rather than 0 degrees as previously.


 *********************
    MESH DESIGN      *
 *********************

   A good conservative starting point for mesh design would be a domain
   that extends 2 km horizontally and 1 km deep.  The top 4 rows should be
   about 12 m high with a 6 percent expansion factor used from row 5 to the
   mesh bottom.  Suggested horizontal cell width is 40 m with 20 m near
   boundaries where the conductivity taks big jumps.
   Receivers shoud be at least 300 m from the left and right boundaries.

  -------------------------------------------
  To run Arjuna in normal mode, type

    Aj2 name1  nam2

    where name1 is the input control file name1.cfl

    name1.cfl will be copied to Arjuna.cfl and Arjuna will run

  When the program is finished it will produce

    Arjuna.out which will be renamed name2.out
    Arjuna.frq which will be renamed name2.frq
    Arjuna.amx which will be renamed name2.amx
    Arjuna.log which won't be renamed

    name2.out  (standard verbose output file)
    name2.frq  (for time-domain only)
    name2.log  (error file)
    name2.amx  (extended AMIRA output file)

  -------------------------------------------
  As with most P223E programs, if all you want to do is test
  time-domain waveforms, receiver channels or compare B or dB/dt
  you can make use of the results of previously computed models
  as long a transmitter-receiver geometry, the survey and the model
  remain the same.
  -------------------------------------------
  To run Arjuna in reuse mode (for time-domain), type

    Aj2R name1  name2  name3

    where name1 is the input control file name1.cfl and
    the frequency-domain information in name3.frq will be used.

    name1.cfl will be copied to Arjuna.cfl and Arjuna will run

  When the program is finished it will produce the output files
  described above.

 Arjuna.exe has been compiled and optimised for the Pentium Pro chip
 using a Lahey compiler.

 Note


  Test models.
  ------------

  Note that in addition to the Arjuna.f90, Arjuna.exe, Aj2.bat, Aj2R.bat &
  the usual  .cfl, .out, .amx, .frq files, there are .tif and .jpg files.

  The ,tif files are pictures of the model
  The .jpg are Maxwell plots where applicable.

  These test results are various types of systems run over or through
  a pair of dipping plates, unless otherwise noted.

  Also note that Arjuna requires loop vertices to be specified in
  clockwise order.  If the user specifies them in anti-clockwise
  order, Arjuna will reorder them.  If they are not expressed in
  sequential order, either clockwise or anti-clockwise, the
  results may seem strange.   If the user truly wants negatives,
  these can be specified by the sign of the input waveform or
  current.


     Example 1: In-Loop Survey - Time-domain
     =======================================

   In_Loop.cfl specifies an in-loop survey over a weathered pipe
   containing a conductive pod.  The topography consists of a high
   level region in the West ramping down over the pipe to a lower
   level region in the East


     Example 2: SlimBoris - Frequency-domain
     =======================================

   SlimBoris.cfl  uses the model of Example 1 to illustrate the use of
   SURVEY_OPTION 2, to model a 3D downhole magnetic dipole dipole
   system passing through both the weathered zone and the conductive
   pod in the pipe.


     Example 3: Fixed Loop - Time-domain
     ====================================

  2dipplate.cfl illustrates the use of SURVEY_OPTION 1, with a fixed
  loop transmitter and a surface receiver array.  The model consists of
  two dipping parallel plates.


     Example 4: Fixed Loop with downhole receivers - Time-domain
     ===========================================================

   Fldwsf.cfl uses a fixed loop source with two receiver groups, one
   along the surface and one in a drillhole passing through a vertical
   plate model.

