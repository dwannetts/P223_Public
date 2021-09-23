  Welcome to Leroi - P223F Version 6.4    17 September 2004
  Please also read Leroi.txt

     CHANGES: 6.4.0 from 6.1.1   Input format has NOT changed !
     -------------------------   =============================

     1. Errors for grounded wire receivers have been corrected.

     2. Half the work required for implementing non-zero PLUNGE has been
        completed.  This has consisted of changing the single plate Green's 
        function computation routines to full numerical Gauss-Legenre
        integration, replacing the analytic integrations down dip.  This 
        has resulted in marginal improvements in both speed and accuracy.

     3. The inter-plate Green's functions have been phrased more coherently
        corresponding to the derivation based on vertical VMD and VED
        potentials.  The same routines are now used for same plate and 
        inter-plate Green's functions.

  -------------------------------------------
  To run Leroi in normal mode, type

    Lr2 name1  nam2

    where name1 is the input control file name1.cfl

    name1.cfl will be copied to Leroi.cfl and Leroi will run

  When the program is finished it will produce

    Leroi.out which will be renamed name2.out
    Leroi.frq which will be renamed name2.frq
    Leroi.amx which will be renamed name2.amx
    Leroi.log which won't be renamed

    name2.out  (standard verbose output file)
    name2.frq  (for time-domain only)
    name2.log  (error file)
    name2.amx  (extended AMIRA output file)

  -------------------------------------------
  As with most P223F programs, if all you want to do is test
  time-domain waveforms, receiver channels or compare B or dB/dt
  you can make use of the results of previously computed models
  as long a transmitter-receiver geometry, the survey and the model
  remain the same.
  -------------------------------------------
  To run Leroi in reuse mode (for time-domain), type

    Lr2R name1  name2  name3

    where name1 is the input control file name1.cfl and
    the frequency-domain information in name3.frq will be used.

    name1.cfl will be copied to Leroi.cfl and Leroi will run

  When the program is finished it will produce the output files
  described above.

 Leroi.exe has been compiled and optimised for the Pentium Pro chip
 using a Lahey compiler.

 Note


  Test models.
  ------------

  Note that in addition to the Leroi.f90, Leroi.exe, lr2.bat, lr2R.bat &
  the usual  .cfl, .out, .amx, .frq files, there are .tif and .jpg files.

  The ,tif files are pictures of the model
  The .jpg are Maxwell plots where applicable.

  These test results are various types of systems run over or through
  a pair of dipping plates, unless otherwise noted.

  Also note that Leroi requires loop vertices to be specified in
  clockwise order.  If the user specifies them in anti-clockwise
  order, Leroi will reorder them.  If they are not expressed in
  sequential order, either clockwise or anti-clockwise, the
  results may seem strange.   If the user truly wants negatives,
  these can be specified by the sign of the input waveform or
  current.


     Example 1: SlimBoris - Frequency-domain
     =======================================

   SlimBoris.cfl  illustrates the use of SURVEY_OPTION 2, to model a
   3D downhole magnetic dipole dipole system.  The model consists of
   two parallel plates 30 m. apart.  The borehole intersects both of them.
   central prism.


     Example 2: Fixed Loops - Time-domain
     ====================================

  FL3G2p2.cfl illustrates the use of SURVEY_OPTION 1, with 3 loop transmitters.
  There are two receiver groups, one a string of surface magnetic dipoles
  and the other, drill hole receivers.  Note the way that 3 loops and
  2 receiver groups require 6 events to be specified.


     Example 3: Coincident Loops - Time-domain
     =========================================

   Cdnt2p.cfl specifies 5 loops, 2 rectangular, 2 pentagons and one hexagon
   over the usual 2 plate model.


     Example 4: In-Loop Survey - Time-domain
     =======================================

   Cntrl2p.cfl specifies an overlapping in-lop survey over the 2 plates.


     Example 5: CSAMT - Frequency-domain
     ===================================

   CSAMT.cfl specifies two orthogonal bipole transmitters and
   crossed electric dipole receivers co-located with vertical
   magnetic dipole receivers.

     Example 6: Grounded wire source - Time-domain
     =============================================

   GWTD.cfl has a grounded wire running north-south with a parallel line
   of grounded wire receivers and an east-west line of VMD receivers.

     Example 7: Loop source - Loop receiver - Time-domain
     ====================================================

   LpLp.cfl has a single loop transmitter and 5 receiver loops

     Example 8: HCP - Frequency-domain
     =================================

   HCP.cfl computes the horizontal coplanar magnetic dipole-dipole
   response over two plates.

     Example 9: VCA - Frequency-domain
     =================================

   VCA.cfl computes the response of a vertical coaxial magnetic
   dipole-dipole response over two plates.

     Example 10: Borehole sign check - Time-domain
     =============================================

   Signs.cfl specifies a rectangular loop source on top of a 100 ohm-m
   uniform half space and several borehole receiver groups.  It was used
   to check the signs of the Ax - SL - HR components


