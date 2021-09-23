  Welcome to Marco version 4.4.0 10 June, 2003

      CHANGE to Version 4.4.0 from from Version 4.3.0
      -----------------------------------------------

     1. CHANGED CONVENTION:  for specifying magnetic dipole source dip
        -------------------  Vertical dipole NOW has SXDIP = 90 degrees
                             Horizontal dipole NOW has SXDIP = 0

     2. Changed Borehole system (Axial, Slope, Horizontal)
        For a vertical borehole, HR, the horizontal component
        points west (9 o'clock) and SL (slope) points north
        (12 o'clock)

        For a non-vertical borehole, the Slope component lies
        in the vertical plane containing the borehole.

     3. PPM output for single offset frequency-domain systems

     4. The frequency-domain current wasnt used in previous versions.
        It is now through call to FD_CURNT

     5. Error in borehole component assignment in WRITE_TD fixed

  Please also read Marco.txt


  -------------------------------------------
  To run Marco in normal mode, type

    Mrc2 name1  nam2

    where name1 is the input control file name1.cfl

    name1.cfl will be copied to Marco.cfl and Marco will run

  When the program is finished it will produce

    Marco.out which will be renamed name2.out
    Marco.frq which will be renamed name2.frq
    Marco.amx which will be renamed name2.amx
    Marco.log which won't be renamed

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
  To run Marco in reuse mode (for time-domain), type

    Mrc2R name1  name2  name3

    where name1 is the input control file name1.cfl and
    the frequency-domain information in name3.frq will be used.

    name1.cfl will be copied to Marco.cfl and Marco will run

  When the program is finished it will produce the output files
  described above.

 Marco.exe has been compiled and optimised for the Pentium Pro chip

 Marco.exe has been compiled and optimised for the pentium Pro chip
 using a Lahey compiler.


  Sample control file description
  -------------------------------

     Example 1: SlimBoris - Frequency-domain
     =======================================

   SlimBoris.cfl illustrates the use of SURVEY_OPTION 2, to model a
   3D downhole magnetic dipole dipole system.  The model consists of
   a conductive prism in the middle of which is a yet more conductive
   prism.   It was constructed using four prisms on each side of the
   central prism.


     Example 2: TD_LOOPS - Time-domain
     =================================

  TD_LOOPS.cfl illustrates the use of SURVEY_OPTION 1, with loop tansmitters
  of differing numbers of corners.  Electrode, loop and magnetic dipole
  receivers are used, some on surface and others downhole.  A mixture of units
  is used with some output in borehole components and other in vertical-east-
  north components.  The model is a uniform half-space in order to concentrate
  on the variations of transmitters and receivers.

     Example 3: MTD_IP - Time-domain
     ===============================

   MTD_IP.cfl illustrates another use of SURVEY_OPTION 1 for electrode
   sources and a mixture of on-time and off-time responses over a paleo
   channel with an IP response.


     Example 4: MTD_IP_MIP - Time-domain
     ===================================

   MTD_IP_MIP.cfl is the same model as MTD_IP except that there are
   also magnetic field responses.


     Example 5: CL_halo_S1 - Time-domain
     ===================================

   CL_halo_S1.cfl consists of a conductive prism in the middle of which is a
   yet more conductive prism.  It was constructed using four prisms on each
   side of the central prism.  It uses three surface coincident loops plus a
   fourth sub-surface loop.


     Example 6: Cntrl_halo_S1 - Time-domain
     ======================================

   Cntrl_halo_S1.cfl consists of the same model as CL_halo_S1.cfl
   but using the central loop option.


     Example 7: HCP - Frequency-domain
     =================================

   HCP.cfl computes the horizontal coplanar magnetic dipole-dipole
   response over sea ice.


     Example 8: CSAMT - Frequency-domain
     ===================================

   CSAMT.cfl computes the response of a simple prism under a clay layer
   with IP response.  The sources are two orthogonal bipole transmitters.
   The receivers co-located vertical magnetic dipoles plus 100 m electrodes
   parallel and perpendicular to the transmitter wires.


