!   PROGRAM Loki
!--------------------------------------------------------------------------------------------------
 Module KG_Metadata
!--------------
! 
! Module designed to hold 'constant character options'
    Implicit NONE

    Character (Len = 40), Parameter :: PNAME = 'Loki'
    Character (Len = 40), Parameter :: PVERS = '3.0.3'
    Character (Len = 40), Parameter :: PDATE = '02 February, 2021'
    Character (Len = 40), Parameter :: PAUT1 = 'CSIRO Electromagnetic Modelling Group'
    Character (Len = 40), Parameter :: PAUT2 = 'Art Raiche, Fred Sugeng and Glenn Wilson'
    Character (Len = 40), Parameter :: PAUT3 = 'David Annetts (david.annetts@csiro.au)'
    Character (Len = 40), Parameter :: PPROJ = 'AMIRA P223F'
    Character (Len = 80), Parameter :: PRELS = 'GPL-2.0 (Full text at https://opensource.org/licenses/gpl-2.0.php)'    

End Module KG_Metadata
!--------------------------------------------------------------------------------------------------
!   ======= ====          =============     =============
!
!                            LANGUAGE: ANSI STANDARD FORTRAN 95
!                            ----------------------------------
!
!	3.0.3
!	1.	DWA: start setup for module namespaces
!
!   3.0.2
!   1.  DWA: When DO3D .eq. 2, Loki will read and use frequencies from Loki.frq. Previously
!            Loki would compute frequencies and assume that values in Loki.frq were the same.
!            They need not be.
!
!   3.0.1
!   1.  DWA: Minor formatting changes to output
!   2.  DWA: bug fix -- wrong CG iterations written to Loki.frq
!
!   ========================================================================================
!
!  This is the final P223F version of Loki.
!  It has been revised to fit the P223F framework.  Loki works well for inductive sources
!  and receivers but not for galvanic computations due to our use of the incomplete Green's
!  tensor operator function to compute fields.  Until that is changed, Loki cannot be applied
!  to galvanic surveying or MT.
!
!  Although the principle reason for revision was inversion, the inversion capability was
!  not completed.  Loki does contain the software modules necessary for inversion (RESJAC
!  and NLSQ)  The software links between these modules and the rest of the program have not
!  been completed.  The perspicacious user may wish to do the work necessary to complete
!  these links.
!
!    Parallel Processing
!    -------------------
!  Loki  is an ideal candidate for parallel processing because there would be a linear
!  decrease in execution time as a function of the number of processors. This is because
!  Loki computes an independent solution for each frequency for each transmitter position
!  due to the need for an iterative solution.  At the time of release, Loki could solve a
!  60,000 cell problem  in less than two minutes per frequency per position.
!
!  During P223E, David Annetts set up Loki for a 10 processor Linux cluster which is why
!  we were able to do so much testing.  That cluster gradually disintegrated because we
!  hadn't planned on doing more Loki development.  Towards the end of the project, there
!  Was renewed interest from some sponsors in using Loki on parallel clusters.
!
!  Simply using a parallel Fortran compiler bindly will introduce gross inefficiency.
!  Creating an efficient parallel version of Loki isn't hard, it simply requires explicit
!  assignment of different source and frequencies to what CPUs are available.  In turn, this
!  requires setting up a dialogue such that each CPU knows what to call for when it has
!  finished its current assignment.  It also requires using the "head" to assemble the
!  various computations into meaningful output.  If this is done properly, Loki could become
!  a practical tool for inductive EM inversion.
!
!  For advice on parallelising Loki, contact
!
!  David.Annetts@csiro.au
!
!===========================================================================!
!                                                                           !
!                              Program Description                          !
!                                                                           !
!===========================================================================!
!
!
!  Modelling
!  ---------
!
!  Loki models the ground/downhole EM response (dB/dt or B) for any
!  arbitrary 3D structure (including topography) using an edge-element
!  version of the finite-element technique.  The program initially solves
!  for the vector electric fields on the element edges.  Green's
!  function projectors are used to compute the frequency-domain magnetic
!  field at or electric field the receivers. Hankel filters are used to
!  compute dB/dt and step B for time-domain modelling.
!
!  The program can be used for polygonal loop or grounded wire sources or
!  magnetic dipole sources.  Response can be computed for magnetic dipole,
!  grounded wire or loop receivers.  Surveys can be defined with one or
!  more groups of receivers assigned to each transmitter position or for
!  transmitter and single loop or magnetic dipole receiver to move at
!  constant offset.
!
!  Loki is capable of modelling high contrasts accurately. This version
!  uses hexehedral elements with linear shape functions.
!
!  This version uses edge-basis hexehedral elements with linear shape
!  functions.
!
!  The preferred graphical interface for data entry is Maxwell.
!
!  The mesh property specification is based on lithologies.
!
!  Inversion
!  ---------
!
!  Loki is is capable of inverting the EM response to recover a
!  generally heterogeneous 3D structure in a limited 3D domain embedded
!  in an otherwise uniform half-space. Inversion is based on the nonlinear
!  least square iterative inversion based on the Gauss-Newton method with
!  damping of the singular value spectrum of the Jacobian matrix; the
!  so-called Jupp-Vozoff or damped eigenparameter algorithm. The Jacobian
!  matrix is constructed using the domain differentiation method.
!
!
!===========================================================================!
!                                                                           !
!                                  Systems                                  !
!                                                                           !
!===========================================================================!
!
!
!  Loki can be used in frequency or time-domain mode to model most
!  ground or borehole systems. It can be used for magnetic dipole, closed
!  loop or dipole transmitters. It can be used for magnetic dipole, closed
!  loop or dipole receivers. Closed and open loop transmitters and receivers
!  must lie on the earth surface. Magnetic dipole transmitters can be on,
!  above or below the surface.
!
!  All transmitters in a survey must be of the same type: either, open or
!  closed loops or magnetic dipoles.
!
!  Regardless of transmitter type, all types of receivers can be used in
!  the same survey as long as each group has only one type of receiver.
!
!
!  Survey Geometry Conventions
!  ---------------------------
!
!  Except for magnetotellurics, there are basically two options: fixed loop
!  with one or more receiver groups and generalised slingram: one or more
!  magnetic dipole receivers moving at fixed offset(s) with respect to a
!  moving transmitter (loop or magnetic dipole).  Transmitter and receiver
!  coordinates are specified in terms of Easting, Northing and RLs (relative
!  level) that increases negatively downwards.
!
!  For generalised slingram, there is an option to specify only the
!  coordinates of the first transmitter position and then the others are
!  determined by loop interval and survey azimuth.
!
!  The survey azimuth is defined as 0 degrees pointing north and positive
!  clockwise.  For surface lines, the X (radial) component will point along
!  the survey azimuth.  The Y (tangential) component will be the horizontal
!  transverse component.  The Z component is vertical.
!
!  For downhole surveys, the U-V-A system will be used.  A is the axial
!  component, V (horizontal) is the horizontal component perpendicular to the
!  receiver azimuth and U (slope) is the component (perpendicular to the
!  Axial component) in the vertical plane determined by the receiver azimuth.
!  For a vertical hole (DIP = 0) U points along the survey azimuth.
!
!
!  Time-domain waveform sign convention
!  ------------------------------------
!
!  Loki assumes that the specified transmitter current will start at 0 or
!  some low value, and rise to a positive maximum before going to zero or
!  oscillating about zero with small magnitudes.  In this case, dI/dt will
!  be computed using the negative I so that the early off-time response is
!  positive for vertical fields.
!
!
!===========================================================================!
!                                                                           !
!                             File Conventions                              !
!                                                                           !
!===========================================================================!
!
!
!  The input control file, now called Loki.cfl is read from logical
!  unit number 3.
!
!  For inversion, the data to be inverted must be contained in Loki.inv
!  from the logial unit number 13. The format of Loki.inv is exactly
!  identical to Leroi.inv to enable ease of portability between inversion
!  programs.
!
!  Frequency-domain data stored in Loki.frq data is written
!  to logical unit number 7.
!
!  Verbose output data file for both forward modelling and inversion
!  is stored in Loki.out, written to logical unit number 4.
!
!  Messages about data or runtime errors are now written in a file
!  called Loki.log on logical unit 9.
!
!  For plotting, the output data file for forward modelling, now called
!  Loki.mf1, is written to logical unit 14.
!
!  For imaging, the output data file for inverse modelling, called
!  Loki.mv1, is written to logical unit 14.
!
!
!   UNIT #   UNIT ID      FILE ID      Function                 Invert    Model
!   ------   -------      -------      --------                 ------   -------
!      3       NR       Loki.cfl       Input control file          x       x
!      4       NW       Loki.out       Verbose output data         x       x
!      7       ND       Loki.frq       F-D data for T-D reuse              x
!      8       NS       Loki.sty       F-D sensitivities           x
!      9       NLG      Loki.log       Data error messages         x       x
!     10       NM       Loki.res       Element resistivity         x
!     13       NR1      Loki.inv       Data to be inverted         x
!     14       np      Loki.mv1       Inversion plotting output   x
!     14       np      Loki.mf1       Forward model output                x
!
!
!===========================================================================!
!                                                                           !
!              Description of data records for LOKI.CFL                   !
!                                                                           !
!===========================================================================!
!
!
!  All records are in list directed (free) format except for
!  the TITLE in RECORD 1.
!
!    NOTE:  All distances and locations are to be specified in metres.
!    ----   All angles are to be specified in degrees.
!
!      In plan view, the coordinate system used for input and output
!      has X positive to the North and Y positive to the East.
!      Depth is described as RLs or relative levels.
!      RL increases negatively downwards.
!
!***************************************************************************
!
!** RECORD 1:  TITLE - up to 120 characters
!
!** RECORD 2:  TDFD, DO3D, ISYS, PRFL, ISTOP
!
!      TDFD = 1 => time-domain modelling - STANDARD OPTION
!           = 2 => frequency-domain modelling
!
!           = 0 => time-domain modelling - USER CONTROL OPTION
!
!            Numerical experiments where the frequency-domain responses of layered
!            layered 1/2 space models were transformed to time-domain for a wide
!            range of resistivities has indicated that 6 points per decade is
!            adequate frequency discretisation, even at the high end.  Moreover,
!            for perfect frequency-domain data, only 3 frequencies are required
!            from 0.1 to 10 Hz to maintain an accuracy of better than .1 percent for
!            the models studied.
!
!            The need to go to 1 MHz depends upon receiver channels and conductivity.
!            A 10,000 ohm-m 1/2 space requires extended range if the earliest window
!            opening is within .28 ms of signal turn-off.  A 1 ohm-m 1/2 space allows
!            normal range if the first window open time is > .002 ms.
!
!            In a 3D program, this is pretty hard to control because of non-uniform
!            resistivity distribution so the safe option of going to 1 MHz is chosen
!            if the first window opens earlier than .28 ms after signal turn-off.  This
!            requires computation for 34 frequencies.
!
!            This is over-ridden by setting TDFD = 0 in which case the user needs to specify
!            the minimum and maximum frequencies plus points per decade in RECORD 2.1
!
!==========================================================================
!      DO3D = -1  INVERSION
!                 In this case, NPLATE = 0 is not allowed.
!
!       In this release, inversion can be done only for surface magnetic dipole
!       or loop sources and magnetic dipole receivers
!
!==========================================================================
!
!      DO3D =  1 or 2 for MODELLING
!
!      DO3D =  1  computes response of 3-D heterogeneities and prints
!                 voltages as measured by receivers.
!
!           =  2  (time-domain only)
!                 instead of computing the frequency-domain responses
!                 (95-99 percent of Samaya usual computation time) use
!                 previously computed frequency-domain responses contained
!                 in file Samaya.frq to calculate 3-D time-domain responses.
!
!      ISYS = 0 : Standard output processing
!           = 2 : Sampo processing => ABS (Bz/Bx) = vertical / radial
!           = 4 : Utem processing
!           = 6 : MT processing
!
!      PRFL = 1  prints response in profile mode.
!                Each column contains the response for one channel
!                (or frequency) for all stations on the profile.
!
!           = 11 as above but include scattered fields in .OUT file
!
!           = 0  prints responses in temporal or frequency mode.
!                Each column contains the responses for one receiver
!                position for either all channels (if TDFD = 1) or
!                for all frequencies (if TDFD = 2).
!
!           = 10 as above but include scattered fields in .OUT file
!
!     ISTOP = 0  read the input data and run the specified models.
!           = 1  read the input data, print the model description
!                  and STOP so that the model description can be verified.
!                  REMEMBER to change ISTOP to 0 once the models have been
!                  verified.
!   ______________________________________________________________________________________
!
!    only if TDFD = 0
!**  RECORD 2.1:  MIN_FREQ, MAX_FREQ, IPPD, KHSQ
!
!       MIN_FREQ - Lowest frequency for time-domain transform
!       MAX_FREQ - Highest frequency for time-domain transform
!
!           IPPD =  0 : 3 points per decade will be used between MIN_FREQ and 10 KHz
!                       6 points per decade will be used between 10 KHz and MAX_FREQ
!                =  3 :  3 points per decade will be used between MIN_FREQ and MAX_FREQ
!                =  6 :  6 points per decade will be used between MIN_FREQ and MAX_FREQ
!                = 12 : 12 points per decade will be used between MIN_FREQ and MAX_FREQ
!
!      The frequency range is chosen as a compromise between accuracy and computation time.
!      The default TDFD = 1 is overly generous and often by using a smaller frequency set,
!      one can achieve accurate results in less time.
!
!      However, layered halfspace computations are very quick.  Moreover one needs a wider
!      frequency range to accommodate the very early and very late times to which the 3D part
!      make negligible contribution.  Thus, the default halfspace frequency spectrum is from
!      0.001 Hz to 100 MHz at 6 points per decade.  This too can be over-ridden by setting
!      KHSQ = 1 in which case the user-defined spectrum will also apply to the halfspace as
!      well as the 3D part.
!
!      KHSQ = 0 : This spectrum will be applied to the 3D part only.
!      KHSQ = 1 : This spectrum will be applied to both the layered earth
!                and the 3D computations.
!
!   ---------------------------------------------------------------------------------------
!
!         WAVEFORM & WINDOWS FOR UTEM SYSTEMS
!         -----------------------------------
!
!** RECORD 3 (time-domain):  TXFREQ, NCHNL
!
!      TXFREQ : Base frequency for UTEM
!      NCHNL  : Number of channels (Must be 10 or 20)
!
!%% SKIP to RECORD 5
!
!         WAVEFORM & WINDOWS FOR NORMAL TIME-DOMAIN SYSTEMS
!         -------------------------------------------------
!
!** RECORD 3 (time-domain):  STEP, NSX, NCHNL, KRXW, REFTYM, OFFTIME
!
!      STEP = 0 : Compute dB/dt for all magnetic dipole receivers.
!           = 1 : Compute B for all magnetic dipole receivers.
!
!      NSX =  number of points needed to describe 1/2 cycle of the transmitter
!             waveform.  A bipolar waveform is assumed.  Thus for a system
!             like Sirotem or EM37, NSX = 4, one point each for the start
!             and end of the two ramps.
!             For an ideal step turnoff system set NSX = 1
!
!      NCHNL - number of receiver channels
!
!      KRXW = 1 : receiver channels will be read in terms of start and end
!                 times in ms relative to REFTYM.
!
!           = 2 : receiver channels will be read in terms of midpoints (relative
!                  to REFTYM) and channel widths.
!
!      REFTYM - Time (in ms) from which TMS or TOPN & TCLS are measured.  For
!               example, this could be signal off-time or start of downward ramp.
!
!      OFFTIME - time (milliseconds) between end of one pulse and the start of
!                the next pulse (of opposite sign) since a bipolar waveform is
!                assumed.  For systems which have a signal which is always on,
!                OFFTIME = 0.
!
!** RECORD 4.1 (time-domain):  (TXON(J), TXAMP(J), J = 1,NSX)
!
!      TXON(J) = digitised time (in milliseconds)
!                In most cases, TXON(1) = 0, TXON(NSX) = pulse on-time
!
!      TXAMP(J) = transmitter current in amps at time TXON(J)
!
!
!   For KRXW = 1
!** RECORD 4.2 (time-domain):  (TOPN(J), TCLS(J), J=1, NCHNL)
!
!              Start and end times (in ms) of receiver windows.
!              (measured from REFTYM.)
!
!%% SKIP to RECORD 5
!
!   For KRXW = 2
!** RECORD 4.2 (time-domain):  (TMS(J), J=1,NCHNL)
!** RECORD 4.3 (time-domain):  (WIDTH(J), J=1,NCHNL)
!
!        TMS(J) -  centre of receiver window J in ms.
!                  measured from REFTYM.
!      WIDTH(J) -  width of receiver window J in ms.
!
!
!         CONTROL PARAMETERS FOR FREQUENCY-DOMAIN SYSTEMS
!         -----------------------------------------------
!
!** RECORD 3 (frequency-domain):  NFRQ
!
!      NFRQ - number of frequencies
!
!** RECORD 4 (frequency-domain):  (FREQ(J), CURNT(J), J = 1,NFRQ)
!
!       FREQ(J) - frequency in Hz
!      CURNT(J) - current in amps for Jth frequency.  For the MT option
!                 current is ignored but a value must be entered.
!
!         SURVEY INFORMATION
!         ------------------
!
!** RECORD 5:  SURVEY_TYPE
!
!      SURVEY_TYPE = 1 : GENERAL OPTION for separate setup of transmitter and
!                        receiver arrays.  Open and closed loops are not shape
!                        restricted.   Inductive and galvanic sources &
!                        receivers are permitted.
!                        This would be the correct choice for downhole surveys
!                        using surface loop transmitters or for CSAMT..
!
!                  = 2 : MOVING LOOP SURVEY with one or more magnetic dipole
!                        receivers moving at fixed horizontal offsets with
!                        respect to rectangular loop.
!                        (Central loop = 1 receiver at zero offset)
!
!                  = 3 : SURFACE MAGNETIC DIPOLE-DIPOLE SURVEY with one or more
!                        magnetic dipole receivers moving at fixed horizontal
!                        offsets with respect to magnetic dipole transmitter on
!                        or above ground
!
!                  = 4 : COINCIDENT LOOP SURVEY with rectangular loop
!
!                  = 5 : BOREHOLE MAGNETIC DIPOLE-DIPOLE SURVEY
!                        Single magnetic dipole receiver moving downhole at
!                        fixed offset with a magnetic dipole transmitter
!
!                  = 6 : MAGNETOTELLURICS (not operable in this version)
!
!===========================================================================
!
!          OUTPUT UNITS DEFINITION FOR ALL SURVEY OPTIONS: if SURVEY_TYPE = 1
!          units can be specified individually for each receiver group.
!          ------------------------------------------------------------------
!
!            UNITS = 1 : volts
!                    2 : millivolts
!                    3 : microvolts
!                    4 : nanovolts
!
!              Only for un-normalised response from magnetic dipole receivers
!              -------------------------------------------------------------
!                   11 : nanoteslas / sec
!                   12 : picoteslas / sec
!
!                   21 : nanoteslas
!                   22 : picoteslas
!
!              Only for normalised response from magnetic dipole receivers
!              -----------------------------------------------------------
!                   31 : simple ratio
!                   32 : percent
!                   33 : ppt - parts per thousand
!                   34 : ppm - parts per million
!                   35 : ppb - parts per billion
!
!              Only for point electric field receivers
!              ---------------------------------------
!
!            UNITS = 41 : volts per metre
!                    42 : millivolts per metre
!                    43 : microvolts per metre
!                    44 : nanovolts per metre
!
!          ----------------------------------------------
!
!
!  DEFINITION OF MAGNETIC DIPOLE RECEIVER COMPONENT & PLOT CONVENTIONS FOR ALL SURVEY OPTIONS:
!  ------------------------------------------------------------------------------------------
!
!    Define CMP(J), the component selection for magnetic dipole and point electric receivers.
!    For inversion, this will define the data components of Line J that are to be inverted.
!    For modelling, these will govern output for Line J.
!
!    For coincident loop or electric dipole receivers or Sampo, Samaya sets CMP = 1
!
!    In what follows, depending upon the value of IDH:
!
!      X is shorthand for the X, U or N component
!      Y is shorthand for the Y, V or S component
!      Z is shorthand for the Z, A or W component
!
!      CMP(J) =   1 : model or invert on X (U,N) data only for Line(J)
!             =   2 : model or invert on Y (V,S) data only for Line(J)
!             =   3 : model or invert on Z (A,W) data only for Line(J)
!             =  12 : model or invert on X (U,N) and Y (V,S) data for Line(J)
!             =  13 : model or invert on Z (A,W) and X (U,N) data for Line(J)
!             =  23 : model or invert on Z (A,W) and Y (V,S) data for Line(J)
!             = 123 : model or invert on all three Line(J) components
!
!        The order of the integers is unimportant; eg, CMP = 312 will give the
!        same result as 123.
!
!
!          For vertical coxial dipole surveys,
!              the X component is computed: set CMP = 1
!          For vertical coplanar dipole surveys (broadside),
!             the Y component is computed: set CMP = 2
!          For horizontal coplanar dipole surveys,
!             the Z component is computed: set CMP = 3
!
!    Specify ground or downhole surveys
!
!      IDH = 0 for all surface surveys.
!          = 1 or 2 for downhole magnetic dipole receivers only
!
!
!          Set IDH = 0 for Surface Receiver Components
!          -------------------------------------------
!
!    The X component is horizontal and lies along the nominated survey line azimuth
!    The Y component is horizontal and lies perpendicular to the survey line azimuth
!    The Z component is vertical.
!
!
!          Set IDH = 1 for Downhole receivers - U, V, A convention
!          -------------------------------------------------------
!
!    A, the axial component, lies along the local receiver dipole axis.
!
!    U, the slope component is orthogonal to the local Rx dip and lies in the vertical
!                    plane whose azimuth is determined by the local Rx azimuth.
!
!    V, horizontal component is orthogonal to U & A.
!
!
!          Set IDH = 2 for UTEM Downhole receivers - W, N, S convention
!          ------------------------------------------------------------
!
!    W, the axial component, lies along the local receiver dipole axis.
!
!    N, the out-section component is orthogonal W and has zero horizontal
!                    component in the direction of the survey line azimuth.
!
!    S, the in-section component = N x W, orthogonal to W & N.
!
!
!
!          PLOT CONVENTION
!          ---------------
!
!      IPLT(J) = 1 : plot response of Line J at receiver location
!              = 2 : plot response of Line J at transmitter-receiver midpoint
!              = 3 : plot response of Line J at transmitter location
!
!      This definition allows the user to specify different plot conventions
!      for each component by defining separate lines (and hence different IPLT)
!      for each component.
!
!
!*******************************************
!
! RECORDS 6-9 for SURVEY_TYPE = 1 : Fixed Tx
!===========================================
!
!** RECORD 6:  NLINES, MRXL, NTX, SOURCE_TYPE, MXVRTX, TXMNT
!
!      NLINES - number of lines of data to be modelled or inverted.
!               For this option, a line of data consists of specifying a
!               single transmitter plus a line of receivers.
!
!               Different RECEIVER types are allowed for different lines
!               but ALL receivers in any line must be of the same type:
!               magnetic dipoles, electric dipoles or rectangular loops.
!
!               The same SOURCE type must be used for all lines in the
!               modelling or inversion project
!
!      MRXL - maximum number of receiver positions per line.
!
!      NTX = number of distinct transmitter positions for source
!            (loop, magnetic dipole, or electric bipole or dipole)
!
!            Note that the same transmitter position can be used for more than one
!            line by combining it with different receiver groups.  For example one
!            line might consist of a transmitter position and a group of surface
!            receivers and another might consist of the same transmitter position
!            and a group of downhole receivers.
!
!            If only one line of receivers is used for every transmitter position,
!            NLINES = NTX
!
!      SOURCE_TYPE = 1 : general loop    - vertex locations will be specified
!                  = 2 : grounded wire   - path + endpoints will be specified
!                  = 3 : magnetic dipole - location & orientation will be specified
!
!      MXVRTX - maximum number of vertices for all sources.
!               If SOURCE_TYPE = 3, magnetic dipole, set MXVRTX = 1
!
!      TXMNT = NTRN, number of turns for closed loop source (SOURCE_TYPE = 1)
!
!            = 1 for open loop or grounded electrodes source (SOURCE_TYPE = 2)
!
!            = TXMNT if SOURCE_TYPE = 3 : Tx dipole moment (turns * area in m^2)
!
!
!  TRANSMITTER LOCATIONS FOR SURVEY_TYPE = 1
!  ==========================================
!
!** For SOURCE_TYPE = 1 or 2 (closed loop & grounded wire)
!   ------------------------------------------------------
!
!     For each source position, specify the number of vertices followed by the
!     easting, northing of each vertex.
!     Loops are restricted to lie horizontally; ie one depth for each open or closed loop.
!
!     If SOURCE_TYPE = 1, the program will connect the first vertex
!                         to the last to complete the loop.
!                         DON'T SPECIFY THE SAME VERTEX MORE THAN ONCE.
!
!     If SOURCE_TYPE = 2, the program assumes that the wire is grounded
!                         at the first and last (NVRTX) vertex.
!
!
!   SPECIFY_TX: DO FOR J = 1 TO NTX
!
!** RECORD 7.J.0: NVRTX(J), TXZ(J)
!
!         NVRTX(J) = number of vertices for transmitter loop J
!           TXZ(J) = elevation of loop J
!             (TXZ > 0 for loops in air
!             (TXZ < 0 for loops below air-earth or air-sea interface)
!
!     SPECIFY_VERTICES: DO FOR I = 1 TO NVRTX
!**** RECORDS 7.J.I: SXE(I,J), SXN(I,J)
!
!       SXE(I,J) = east coordinate of vertex I for loop position J
!       SXN(I,J) = north coordinate of vertex I for loop position J
!
!     END SPECIFY_VERTICES FOR TX POSITION J
!
!     Note that the current will flow in the direction specified
!     by the order of the vertices.  Clockwise order will yield a
!     positive time-domain magnetic field in the loop centre.
!
!   END SPECIFY_TX
!
!
!** For SOURCE_TYPE = 3 (magnetic dipole source)
!   --------------------------------------------
!
!   SPECIFY_TX: DO FOR J = 1 TO NTX
!** RECORD 7.J: SDE(J), SDN(J), SDZ(J), TXCLN(J), TXAZM(J)
!
!       SDE(J) - easting  of transmitter J
!       SDN(J) - northing of transmitter J
!       SDZ(J) - ground clearance height (positive above) of transmitter J
!     TXCLN(J) - inclination in degrees of transmitter J.
!                0 = vertical; 90 = horizontal
!     TXAZM(J) - azimuth of transmitter J
!                0 - north; 90 = east
!----------------------------------------------------------------------------
!
!   LINE & RECEIVER SPECIFICATION FOR SURVEY_TYPE = 1
!   -------------------------------------------------
!
!     For each Line J:  J = 1, NLINES, RECORDS 8.J and 9.J are read in pairs
!     -------------------------------
!
!** RECORD 8.J.1: LINE(J), IDTX(J), RX_TYPE(J), NRX(J), UNITS(J)
!        LINE(J) - line number for Line J
!        IDTX(J) - transmitter index for Line J; ie, IDTX is an integer from 1 to NTX.
!                  It specifies which of the NTX transmitter positions is to be used
!                  for LINE(J).  In the case where the same transmitter position is
!                  to be used for more than one line of receivers, it is important to
!                  group the lines such that all the lines corresponding to the first
!                  transmitter position are followed by all the lines for the
!                  second position etc.
!
!         NRX(J) = number of receivers for Line J
!
!     RX_TYPE(J) = 1 for magnetic dipole receivers 
!====================================================================
!====================================================================
!
!XXXX   RX_TYPE = 2 or 3 is non-operable for this version of Loki
!                = 2 for electric dipole receivers
!                    not allowed if SOURCE_TYPE = 3 (magnetic dipole transmitters)
!
!                = 3 (frequency domain only)  for point electric field measurements.
!                    not allowed if SOURCE_TYPE = 3 (magnetic dipole transmitters)
!
!         This program requires that receiver electrodes be buried at least 1 mm below surface.
!         This is forced by parameter MIN_RXED IN SUBROUTINE READ_SYSTEM_AND_SURVEY_DATA
!
!====================================================================
!====================================================================
!
!       UNITS(J) - defined above at end of RECORD 5 description
!
!
!   Enter 8.J.2 only for magnetic dipole & point electric receivers: (RX_TYPE = 1 or 3)
!   Skip 8.J.2 for electric dipole receivers: (RX_TYPE = 2)
!   ---------------------------------------------------------
!** RECORD 8.J.2: (RX_TYPE = 1 only) CMP(J), SV_AZM(J),KNORM(J), IPLT(J), IDH(J), RXMNT(J)
!** RECORD 8.J.2: (RX_TYPE = 3 only) CMP(J), SV_AZM(J)
!
!          CMP(J) : Component selection. See definitions below RECORD 5 (SURVEY_TYPE)
!
!       SV_AZM(J) : azimuth of survey line J.  SV_AZM = 0 pointing north.
!                   It is positive clockwise.
!
!           For surface surveys, SV_AZM orients the X & Y components.
!               The X (radial) component lies along SV_AZM
!               The Y (transverse) component is perpendicular to SV_AZM
!               Z = vertical component.
!
!           If IDH = 2, SV_AZM orients the U & V components.
!
!       KNORM(J) = 0 : output is not normalised.  UNITS(J) < 30
!                = 1 : output is normalised to TOTAL DC primary (air) field.  UNITS(J) > 30
!
!                = 2 for MAGNETIC DIPOLE SOURCE (SOURCE_TYPE = 3)
!                    magnetic dipole receiver output is normalised to AXIAL DC primary.
!
!                = 2 for CLOSED LOOP SOURCE (SOURCE_TYPE = 1)
!                    magnetic dipole receiver output is normalised to VERTICAL DC primary.
!
!       Normalisation (KNORM > 0) is allowed for frequency-domain and
!       B field time-domain computations.
!       It is not allowed for time-domain dB/dt output.
!
!
!       IPLT(J) = 1 : plot response of Line J at receiver location
!               = 2 : plot response of Line J at transmitter-receiver midpoint
!               = 3 : plot response of Line J at transmitter midpoint
!
!           IDH = 0 : surface receivers
!               = 1 : downhole receivers.  Use UVA processing
!               = 2 : downhole receivers.  Use UTEM processing
!
!       RXMNT(J) - dipole receiver moment (area * turns) for Line J
!                  (magnetic dipole only)
!
!
!     SPECIFY_RECEIVER_LOCATIONS
!     --------------------------
!    Under each line 8.J, enter records I = 1, NRX(J) records for each receiver on Line J.
!
!     For magnetic dipole receivers:
!     -----------------------------
!     Enter the easting (RXE), northing (RXN) and ground clearance
!     (positive RXZ = above, negative implies below ground)
!     for each receiver I on Line J.  For downhole processing (IDH > 0)
!     BHDIP(I,J) and BHAZM(I,J) are also required.
!
!     For IDH = 0 (X,Y,Z) processing
!**   RECORD 9.J.I  RXE(I,J), RXN(I,J), RXZ(I,J)
!
!     For IDH = 1 or 2 (U,V,A) downhole processing
!**   RECORD 9.J.I  RXE(I,J), RXN(I,J), RXZ(I,J), BHDIP(I,J), BHAZM(I,J)
!
!
!     For electric dipole receivers (voltage):
!     ---------------------------------------
!     This version of Samaya requires that electric dipole receivers be horizontal.
!     Thus only one depth is required. RXZ must be < or = 0.
!     The output is voltage computed as the integral of the electric field.
!     Enter the easting (RXE), northing (RXN) for each electrode followed by receiver depth.
!
!**   RECORD 9.J.I  RXE(I,J,1), RXN(I,J,1), RXE(I,J,2), RXN(I,J,2), RXZ(I,J)
!
!     For point electric field output:
!     -------------------------------
!
!     This is used for MT and also for measurements down a hole referenced to a single point.
!     Enter the easting (RXE), northing (RXN) and depth (RXZ) for each point.
!
!**   RECORD 9.J.I  RXE(I,J), RXN(I,J), RXZ(I,J)
!
!
!
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!%% SKIP to RECORD 10 for model specification
!----------------------------------------------
!
!===============================================================================
!
!_________________________________________________
!*************************************************
!
! RECORDS 6-8 for SURVEY_TYPE = 2 : Moving Loop Tx
!=================================================
!
!    MRXTX = the number of dipole receivers moving at fixed horizontal offsets
!    with respect to each rectangular loop position.  Loops are specified by
!    length, width  and the coordinates of loop centres.
!
!    For central loop, MRXTX = 1 receiver at zero offset
!
!    NTXL will be the number of distinct transmitter lines.
!    NLINES = NTXL * MRXTX for surveys with multiple offset receivers
!                        per transmitter position
!
!    NTXL transmitter lines need to be specified separately to avoid
!    excessive computation times for redundant transmitter positions.
!
! ====================================
!
!** RECORD 6:  NTXL, MRXL, MRXTX, NTRN, ISTAT
!
!        NTXL - number of distinct transmitter lines
!        MRXL - maximum number of transmitter or receiver positions per line
!               Each line consists of 1 transmitter with one constant offset receiver.
!
!       MRXTX = number of fixed offset receivers per loop position
!
!        NTRN = number of turns in transmitter loop
!
!       ISTAT = 1 : Transmitter positions will be specified by incremental
!                   distances along the survey azimuth SV_AZM, starting
!                   from the easting and northing of the first position.
!
!             = 2 : All transmitter positions will be specified by eastings
!                   and northings
!
!   SPECIFY NTXL records (J = 1, NTXL) one for each line of transmitters
!      Each of these records 7.J will be followed by MRXTX + 1 records 8.I
!      containing receiver information and line number.
!
!** RECORD 7.J  NRX(J),TXLNGTH(J),TXWDTH(J),SV_AZM(J),SDE(1,J),SDN(1,J)
!
!            NRX(J) : number of transmitter positions on Line J
!        TXLNGTH(J) : length of survey loop along Line J
!         TXWDTH(J) : width of survey loop perpendicular to Line J
!         SV_AZM(J) : azimuth of Tx Line J in degrees. North = 0, East = 90
!                       X direction (radial) is defined positive along SV_AZM
!                       Y direction (tangential) is positive along SV_AZM + 90
!          SDE(1,J) : east coordinate of first transmitter loop centre for Line J
!          SDN(1,J) : north coordinate of first transmitter loop centre for Line J
!
!   Only if ISTAT = 1
!** RECORD 8.J  DSTAT(I,J), I= 2,NRX(J))
!
!        DSTAT(I,J) : station spacing between position I and I-1 on Line J (NRX(J) - 1 entries)
!
!          Example :  if there were 30 equi-spaced loop positions 50 m apart on the
!                     first transmitter line and the initial position was 6500 east
!                     and 1000 north, then RECORD 8.J would be 29*50.
!                     Spacings need not be uniform: eg, 10*50 9*25 10*50
!
!   Only if ISTAT = 2
!** RECORD 8.J  SDE(I,J),SDN(I,J), I= 2,NRX(J))
!                 eastings and northings of transmitter centre positions 2 to NRX(J)
!
!   Specify I = 1, MRXTX records, one for each receiver line offset on transmitter line J
!   -------------------------------------------------------------------------------------
!** RECORD 9.I:  LINE(I), CMP(I), XRXOF(I), YRXOF(I), ZRXOF(I), RXMNT(I),UNITS(I), KNORM(I), IPLT(I)
!
!         LINE(I) - line number for Ith fixed-offset receiver on Jth Tx line
!         CMP(I)  - Component selection. See definitions below RECORD 5 (SURVEY_TYPE)
!
!         XRXOF(I) = offset of Ith receiver along X direction (SV_AZM) on Jth Tx line
!                      (positive if receiver leads transmitter)
!         YRXOF(I) = offset of Ith receiver along Y direction (SV_AZM + 90) on Jth Tx line
!         ZRXOF(I) = ground clearance of Ith receiver (above is positive)
!         RXMNT(I) - Receiver moment    (turns * area in m^2)
!
!         UNITS(I) & IPLT(I) are also defined below RECORD 5 description
!
!         KNORM(I) = 0 : output is not normalised.  UNITS(J) < 30
!                  = 1 : output is normalised to total primary (air) field.  UNITS > 30
!                  = 2 : magnetic dipole receiver output is normalised to VERTICAL DC primary.
!
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!%% SKIP to RECORD 10 for model specification
!----------------------------------------------
!
!===============================================================================
!
!___________________________________________________
!***************************************************
!
! RECORDS 6-8 for SURVEY_TYPE = 3 : Moving Dipole Tx
!===================================================
!
!    MRXTX = the number of dipole receivers moving at fixed horizontal offsets
!    with respect to surface magnetic dipole transmitter
!
!    NTXL will be the number of distinct transmitter lines.
!    NLINES = NTXL * MRXTX for surveys with multiple offset receivers
!                        per transmitter position
!
!    NTXL transmitter lines need to be specified separately to avoid
!    excessive computation times for redundant transmitter positions.
!
!** RECORD 6:  NTXL, MRXL, MRXTX, TXMNT, ISTAT
!
!        NTXL - number of transmitter dipole lines
!        MRXL - maximum number of transmitter or receiver positions per line
!               Each line consists of 1 transmitter with one constant offset receiver.
!       MRXTX - number of fixed offset receivers per loop position
!       TXMNT - transmitter moment (turns * area in m^2)
!
!       ISTAT = 1 : Transmitter positions will be specified by incremental
!                   distances along the survey azimuth SV_AZM, starting
!                   from the easting and northing of the first position.
!
!             = 2 : All transmitter positions will be specified by eastings
!                   and northings
!
!   SPECIFY NTXL records (J = 1, NTXL) one for each line of transmitters
!      Each of these records 7.J will be followed by MRXTX + 1 records 8.I
!      containing receiver information and line number.
!
!** RECORD 7.J  NRX(J),TXCLN(J),TXAZM(J),SDZ0(J),SV_AZM(J),SDE(1,J),SDN(1,J)
!
!          NRX(J) : number of transmitter positions on line J
!
!          TXCLN(J) : inclination in degrees of transmitter J.
!                     0 = vertical; 90 = horizontal
!          TXAZM(J) - azimuth of dipole transmitter axis in degrees
!                     0 - north; 90 = east
!         SV_AZM(J) : azimuth of Tx Line J in degrees. North = 0, East = 90
!                       X direction (radial) is defined positive along SV_AZM
!                       Y direction (tangential) is positive along SV_AZM + 90
!          SDE(1,J) : east coordinate of first transmitter position for Line J
!          SDN(1,J) : north coordinate of first transmitter position for Line J
!           SDZ0(J) : Magnetic dipole Tx ground clearance for Line J
!
!   Only if ISTAT = 1
!** RECORD 8.J  DSTAT(I,J), I= 2,NRX(J))
!
!        DSTAT(I,J) : station spacing between position I and I-1 on Line J (NRX(J) - 1 entries)
!
!          Example :  if there were 30 equi-spaced loop positions 50 m apart on the
!                     first transmitter line and the initial position was 6500 east
!                     and 1000 north, then RECORD 8.J would be 29*50.
!                     Spacings need not be uniform: eg, 10*50 9*25 10*50
!
!   Only if ISTAT = 2
!** RECORD 8.J  SDE(I,J),SDN(I,J), I= 2,NRX(J))
!                 eastings and northings of transmitter centre positions 2 to NRX(J)
!
!   Specify I = 1, MRXTX records, one for each receiver offset on transmitter line J
!   --------------------------------------------------------------------------------
!** RECORD 9.I:  LINE(I), CMP(J), XRXOF(I), YRXOF(I), ZRXOF(I), RXMNT(I),UNITS(I), KNORM(I), IPLT(I)
!
!         LINE(I) - line number for Ith fixed-offset receiver on Jth Tx line
!         CMP(J)  - Component selection. See definitions below RECORD 5 (SURVEY_TYPE)
!
!         XRXOF(I) = offset of Ith receiver along X direction (SV_AZM) on Jth Tx line
!                      (positive if receiver leads transmitter)
!         YRXOF(I) = offset of Ith receiver along Y direction (SV_AZM + 90) on Jth Tx line
!         ZRXOF(I) = ground clearance of Ith receiver (above is positive)
!         RXMNT(I) - Receiver moment    (turns * area in m^2)
!
!         UNITS(I) & IPLT(I) are also defined below RECORD 5 description
!
!         KNORM(I) = 0 : output is not normalised.  UNITS(J) < 30
!                  = 1 : output is normalised to total primary (air) field
!                  = 2 : output is normalised to magnetic dipole AXIAL DC primary.   ( UNITS > 30 )
!
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!%% SKIP to RECORD 10 for model specification
!----------------------------------------------
!
!===============================================================================
!
!__________________________________________________
!**************************************************
!
! RECORDS 6-8 for SURVEY_TYPE = 4 : Coincident Loop
!==================================================
!
!** RECORD 6:  NTXL, MRXL, MRXTX, NTRN, ISTAT
!
!      NLINES - number of lines
!        MRXL - maximum number of transmitter positions per line
!        NTRN = number of turns in transmitter loop
!       ISTAT = 1 : Transmitter positions will be specified by incremental
!                   distances along the survey azimuth SV_AZM, starting
!                   from the easting and northing of the first position.
!
!             = 2 : All transmitter positions will be specified by eastings
!                   and northings
!
!   SPECIFY NLINES records (J = 1, NTXL) one for each line of transmitters
!      Each of these records 7.J will be followed by MRXTX + 1 records 8.I
!      containing receiver information and line number.
!
!** RECORD 7.J  NRX(J),TXLNGTH(J),TXWDTH(J),SV_AZM(J),SDE(1,J),SDN(1,J)
!
!            NRX(J) : number of transmitter positions on Line J
!        TXLNGTH(J) : length of survey loop along Line J
!         TXWDTH(J) : width of survey loop perpendicular to Line J
!         SV_AZM(J) : azimuth of Tx Line J in degrees. North = 0, East = 90
!                       X direction (radial) is defined positive along SV_AZM
!                       Y direction (tangential) is positive along SV_AZM + 90
!          SDE(1,J) : east coordinate of first transmitter loop centre for Line J
!          SDN(1,J) : north coordinate of first transmitter loop centre for Line J
!
!   Only if ISTAT = 1
!** RECORD 8.J  DSTAT(I,J), I= 2,NRX(J))
!
!        DSTAT(I,J) : station spacing between position I and I-1 on Line J (NRX(J) - 1 entries)
!
!          Example :  if there were 30 equi-spaced loop positions 50 m apart on the
!                     first transmitter line and the initial position was 6500 east
!                     and 1000 north, then RECORD 8.J would be 29*50.
!                     Spacings need not be uniform: eg, 10*50 9*25 10*50
!
!   Only if ISTAT = 2
!** RECORD 8.J  SDE(I,J),SDN(I,J), I= 2,NRX(J))
!                 eastings and northings of transmitter centre positions 2 to NRX(J)
!
!** RECORD 9 LINE(J), UNITS(J)
!
!         LINE(J) - line number
!         UNITS(J) are also defined below RECORD 5 description
!
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!%% SKIP to RECORD 10 for model specification
!----------------------------------------------
!
!_________________________________________________
!*************************************************
!
! RECORDS 6-8 for SURVEY_TYPE = 5 : Downhole Probe
!=================================================
!
!      Borehole fixed offset Tx-Rx system - Single magnetic dipole receiver
!      moving downhole at fixed offset with a magnetic dipole transmitter
!
!** RECORD 6:  NLINES, MRXL, TXMNT, IDH
!
!      NLINES - number of downhole survey lines
!      MRXL   - maximum number of transmitter or receiver positions per line
!               Each line consists of 1 transmitter with one constant offset receiver.
!
!      TXMNT  - transmitter moment (turns * area in m^2)
!
!      IDH    = 1 : conventional U, V, A processing
!             = 2 : UTEM S, N, W processing
!             = 0 : X, Y, Z output
!
!
!
!   SPECIFY TX LINES: J = 1, NLINES
!   -------------------------------
!** RECORD 7.J.  LINE(J), SV_AZM(J), NRX(J), CMP(J), OFFSET(J), RXMNT(J), UNITS(I), KNORM(I), IPLT(I)
!
!       LINE(J)   - Line number for line J
!       SV_AZM(J) - reference azimuth for line J
!       NRX(J)    - number of transmitter positions down line J
!       CMP(J)    - Component selection. See definitions below RECORD 5 (SURVEY_TYPE)
!       OFFSET(J) - axial offset (metres) between transmitter and receiver.
!                   (positive if receiver is above transmitter)
!       RXMNT(J)  - receiver moment    (turns * area in m^2)
!
!       UNITS(I) & IPLT(I) are also defined below RECORD 5 description
!
!       KNORM(J) = 0 : output is not normalised.  UNITS(J) < 30
!                = 1 : output is normalised to total primary (air) field.  UNITS > 30
!                = 2 : output is normalised to magnetic dipole AXIAL DC primary.   ( UNITS > 30 )
!
!
!     SPECIFY I = 1, NRX(J) positions and orientations under each line 7.J
!     ---------------------------------------------------------------------
!**   RECORD 8.J.I  SDE(I,J), SDN(I,J), SDZ(I,J), BHDIP(I,J), BHAZM(I,J)
!
!       SDE, SDN, SDZ are the easting, northing and RL of the Ith transmitter position of Line J.
!       BHDIP, BHAZM are the dip and azimuth of the borehole at location I of borehole J.
!       BHDIP = 0 for horizontal dipoles.  Azimuth: North = 0 ; East = 90
!
!
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!%% SKIP to RECORD 10 for model specification
!----------------------------------------------
!
!***************************************************
       ! FORGET IT UNTIL THE GREEN'S TENSOR REVISION IS COMPLETED.
!XXX
!XXX        ! RECORDS 6-8 for SURVEY_TYPE = 6 : Magnetotellurics     
!XXX        !===================================================
!XXX !
!XXX        !** For SURVEY_TYPE = 6 (magnetic field receivers)
!XXX        !   ==============================================
!XXX        !
!XXX        !** RECORD 6: NHRX, NERX, IPM, IZXX, IZXY, IZYX, IZYY
!XXX        !
!XXX        !       NERX = number of electric dipole receiver positions
!XXX        !
!XXX        !       NHRX = number of magnetic field receiver positions
!XXX        !              (must not exceed NERX)
!XXX        !
!XXX        !       IPM  = 0 : impedances are expressed as Inphase and Quadrature.
!XXX        !            = 1 : impedances are expressed as Phase and Magnitude.
!XXX        !
!XXX        !       IZXX = 1 : include ZXX data for inversion
!XXX        !            = 0 : do not invert on ZXX data
!XXX        !
!XXX        !       IZXY, IZYX and IZYY are similarly defined for ZXY, ZYX and ZYY data
!XXX        !       They need not be specified necessary if inversion is not required.
!XXX        !
!XXX        !   SPECIFY magnetic receivers: DO for J1 = 1 to NHRX
!XXX        !** RECORD 7.J1: RXE(J1), RXN(J1)
!XXX        !
!XXX        !       RXE(J1) = east coordinate of magnetic receiver J1
!XXX        !       RXN(J1) = north coordinate of magnetic receiver J1
!XXX        !
!XXX        !
!XXX        !   SPECIFY electric receivers: DO for J2 = 1 to NERX
!XXX        !** RECORD 8.J2: RXE(J2), RXN(J2), IDHE(J2),
!XXX        !
!XXX        !       RXE(J2) = east coordinate of electric receiver J2
!XXX        !       RXN(J2) = north coordinate of electric receiver J2
!XXX        !
!XXX        !       IDHE(J2) = magnetic field receiver index used for impedance computation
!XXX        !                  for electric receiver J2.  (0 < IDHE < NHRX+1)
!XXX        !
!XXX        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!XXX        !%% SKIP to RECORD 10 for model specification
!XXX        !----------------------------------------------
!XXX 
!=============================================================================
!
!          LITHOLOGY & STRUCTURE FOR LOKI & LOKI_AIR
!          =========================================
!
!         SET MESH SIZE, ACCURACY & SOLVER OPTIONS
!         ----------------------------------------
!
!**  RECORD 10:  NNORTH, NZ, NEAST, NLITH, KACC, SOLVER, OUTPUT
!
!                The heterogeneous domain must be divided into
!                a mesh which is defined by the user.
!
!      NNORTH - number of domain nodes in North-South direction.
!          NZ - number of domain nodes in depth-direction.
!       NEAST - number of domain nodes in East-West direction.
!
!       NLITH - number of distinct lithologial units
!
!        KACC = 1 => standard solver with a balance between runtime & accuracy
!             = 2 => higher accuracy but much slower.
!
!      SOLVER = 1  direct matrix solution
!             = 2  iterative matrix solution
!
!        OUTPUT =  10 => If PPM available use PPM and ignore fields in Loki.amx
!        OUTPUT = -10 => Loki.amx file will Ignore PPM data and print field values
!
!         This version is restricted to SOLVER = 2.
!         Other options will become available with subsequent releases.
!
!         Iterative solvers are much faster for computations with a single
!         source position but each source position requires a new solution.
!
!         Direct solvers are very slow for single source position but
!         subsequent source positions only require a few percent of
!         the original solution time
!
!
!          DEFINE LITHOLOGIES
!          ------------------
!
!** RECORD 11.1: RES(1), SIG_T(1), RMU(1), REPS(1), CHRG(1), CTAU(1), CFREQ(1)
!** RECORD 11.2: RES(2), SIG_T(2), RMU(2), REPS(2), CHRG(2), CTAU(2), CFREQ(2)
!     .
!
!** RECORD 11.N: RES(N), SIG_T(N), RMU(N), REPS(N), CHRG(N), CTAU(N), CFREQ(N)
!
!           N = NLITH
!      RES(I) - cell resistivity
!    SIG_T(I) - Conductance (conductivity-thicknes product)  (NOT USED)
!               A value for SIG_T must be entered even though it isn't used.
!               It is necessary because of the form of the data base in EMGUI
!      RMU(I) - relative layer magnetic permeability for LITH_INDEX(I)
!     REPS(I) - relative layer dielectric constant (permitivity for LITH_INDEX(I)
!     CHRG(I) - Cole-Cole layer chargeability for LITH_INDEX(I)
!     CTAU(I) - Cole-Cole layer time constant for LITH_INDEX(I)
!    CFREQ(I) - Cole-Cole layer frequency constant for LITH_INDEX(I)
!
!    Default values:  RMU = 1   REPS = 1   CHRG = 0   CTAU = 0   CFREQ = 1
!
!    The default means no magnetic permeability contrast (MU = 4 PI * 10^(-7))
!                      no dielectric constant contrast  (EPSILON = 8.854215E-12)
!                      and no IP effects (no Cole-Cole)
!
!     NOTE:  A value for SIG_T must be entered even though it isn't used.
!            It is necessary because of the form of the data base in EMGUI
!
!
!                 NODE LOCATIONS:   NNORTH * NZ * NEAST records
!                 ---------------------------------------------
!
!       The ordering below is best pictured by taking one 2D sheet at a time
!       along the North-South axis from South to North.  For each sheet,
!       nodes and associated lithologies are read a row at a time
!       (West to East) from the top (surface) to the bottom.
!
!       The lithology of each hexahedral cell is associated with the top,
!       southern, western node defining that cell.
!
!**  RECORDS 12.(I,J,K):  I, K, J, NLOC, ZLOC, ELOC, LITH
!
!      1 <= I <= NNORTH is the node index increasing from South to North
!      1 <= K <= NZ      is the node index increasing from surface to bottom
!      1 <= J <= NEAST  is the node index increasing from West to East
!
!      NLOC(J,I,K) = the north coordinate of node (J,I,K) in metres.
!                    For fixed I,K, it must increase with increasing J.
!
!      ZLOC(J,I,K) = the relative level of node (J,I,K) in metres.
!                    For fixed I,J, it must increase negatively with
!                    increasing K; ie,
!
!      ELOC(J,I,K) = the east coordinate of node (J,I,K) in metres.
!                    For fixed J,K, it must increase with increasing I.
!
!
!      Each cell (or mesh element) is defined by its eight corner nodes.
!      Each cell is identified by the indices of the node of its top,
!      western, southern corner.
!
!      Thus LITH (J,I,K) will be the lithology index of the cell
!      defined by nodes:
!
!      (I, K, J),   (I+1, K,   J),   (I, K+1, J),   (I+1, K+1, J),
!      (I, K, J+1), (I+1, K+1, J+1), (I, K+1, J+1), (I+1, K+1, J+1),
!
!      LITH is not read if I = NNORTH, or if J = NEAST or if K = NZ
!
!      There are NNORTH * NZ * NEAST records to be read in below, one
!      for each permutation of I, J, K
!
!===========================================================================!
!                                                                           !
!     CONTROL FILE DESCRIPTION FINISHED FOR FORWARD MODELLING OPTIONS       !
!                                (DO3D > 0)                                 !
!                                                                           !
!                          CONTINUE FOR INVERSION                           !
!                                (DO3D = -1)                                !
!                                                                           !
!===========================================================================!
!
!
!                        INVERSION INPUTS FOR LOKI
!                        -------------------------
!
!   INVERSION CONTROLS
!   ==================
!
!** RECORD 16: MAXITS, CNVRG, NFIX, MV1PRT, OUTPRT
!
!      MAXITS - The inversion will run for ITS iterations unless one of the two
!               convergence criteria designated by CNVRG is satisfied.
!
!               (Suggested value: MAXITS = 90).
!
!      CNVRG = 1 => Iterations will proceed unless the error can no
!                   longer be reduced.
!
!            = 2 => Iterations will not proceed any further if the
!                   RMS (root mean square) error is less than a user
!                   specified percent (PCTCNV in RECORD 16.1).
!
!      NFIX - The number of parameters that will be constrained. If all
!             parameters are free to vary without restriction, NFIX = 0.
!
!             If NFIX > 0, NFIX records describing the constraint must be
!             entered as RECORDS 16.2.
!
!      CNVRG = 1 => iterations will proceed using the stopping criteria above
!                   and the default derivative step, starting with 6 percent
!                   and testing 3 percent after RSVT reaches 0.01.
!
!            = 2 => iterations will not proceed any further if the
!                   RMS (root mean square) error is less than a user
!                   specified percent (PCTCNV in RECORD 16.1).
!                   Uses the default derivative step.
!
!            = 3 => reverse the default (start with 3 percent and test 6 percent)
!
!            = 4 => set a fixed numerical derivative step (override default)
!
!      NFIX - the number of parameters that will be constrained.  If all
!             parameters are free to vary without restriction, NFIX = 0
!
!             If NFIX > 0, NFIX records describing the constraint must be
!             entered as RECORDS 16.2
!
!      MV1PRT refers to the output print level in Loki.mv1.
!
!      OUTPRT refers to the output print level in Loki.out.
!
!            =  0  No output DURING inversion. The final model set AFTER inversion,
!                  but NOT the final model data, is written to output files.
!
!            =  1  as above plus final model data.
!
!            =  2  as above plus intermediate model sets after each iteration.
!
!            =  3  as above plus intermediate model data after each iteration.
!
!    Only if CNVRG = 2:
!    ------------------
!
!**  RECORD 16.1: PCTCNV
!
!      PCTCNV - terminate inversion if SQRT {SUMSQ / WSUM } < PCTCNV.
!
!
!    Only if NFIX > 0:
!    -----------------
!
!**  RECORD 16.2: CTYPE, LITH_INDX, KPAR, ELAS(KPAR), LBND(KPAR), UBND(KPAR)
!
!     CTYPE = 1 -> parameter is fixed to a priori model value. Only LITH_INDX and
!                  KPAR need to be defined.
!
!           = 2 -> frictional restraint. Only LITH_INDX, KPAR and ELAS(KPAR) need
!                  to be defined.
!
!           = 3 -> buffered boundaries. All parameters above must be defined.
!
!     LITH_INDX - lithology number for which the resistivity will be constrained
!                 (with respect to the order of the lithologies read in the control
!                 file).
!
!     KPAR = 1 -> lithology resistivity.

!     ELAS(KPAR) - Elasticity of resistivity (0 < ELAS < 1) for lithology LITH_INDX.
!     LBND(KPAR) - Lower bound of resitivity for lithology LITH_INDX.
!     UBND(KPAR) - Upper bound of resistivity for lithology LITH_INDX.
!
!       Note:
!       -----
!
!       After each iteration, the inversion will compute a proposed update for
!       each model parameter, DELPAR.
!
!       For CTYPE = 1 where the model parameter NM is associated with LITH_INDX, then
!       DELPAR(NM) = 0.
!
!       For CTYPE = 2 where the model parameter NM is associated with LITH_INDX, then
!       DELPAR(NM)' = ELAS * DELPAR(NM). This serves as a frictional restraint or rubber
!       band preventing a parameter from making the full change suggested by the inversion
!       algorithm. It used when a parameter value is thought to be known but allows more
!       latitute than parameter fixation.
!
!       For CTYPE = 3 where the model parameter NM is associated with LITH_INDX, then
!       DELPAR(NM)' = ELAS * DELPAR(NM) provided that LBND =< DELPAR(NM)' =< UBND. If it
!       exceeds either bound, it is set to that bound.
!
!
!   End of LOKI.CFL.
!
!===========================================================================!
!
!               Description of data records for LOKI.INV
!               ========================================
!
!   Note:
!   -----
!
!   The format of Loki.inv is identical to Leroi.inv and Samaya.inv to to
!   enable ease of portability between inversion programs.
!
!   Any number of comment lines can be inserted ONLY at the beginning of
!   Loki.inv. These must be designated by making the first character
!   either / or \ as the first characters of the line.
!
!   Blank lines or any other line whose first character is NOT / or \
!   signifies the start of data records.
!
!
!---------------------------------------------------------------------------!
!
!
!             DATA & DATA WEIGHTS
!             ===================
!
!    In what follows, the channel reference term PCHNL refers to all components
!    for each frequency or time-domain channel.
!
!    Frequency-domain
!    ----------------
!
!    Channels are ordered from the lowest to the highest frequency.
!
!          PCHNL = 1 to NFRQ for the in-phase (real).
!                = NFRQ+1 to 2*NFRQ for the quadrature (imaginary).
!
!    Time-domain
!    -----------
!
!          PCHNL = NCHNL, ordered from earliest to latest time.
!
!
!**  RECORD 1: KCMP, ORDER, FIQ
!
!
!      In what follows: 1 = X for surface surveys and U for downhole surveys.
!                       2 = Y for surface surveys and V for downhole surveys.
!                       3 = Z for surface surveys and A for downhole surveys.
!
!
!      KCMP =   1 : only X (U) data
!           =   2 : only Y (V) data
!           =   3 : only Z (A) data
!           =  13 : X data followed by Z data (U data followed by A data)
!           =  31 : Z data followed by X data (A data followed by U data)
!           =  23 : Y data followed by Z data (V data followed by A data)
!           =  32 : Z data followed by Y data (A data followed by V data)
!           = 123 : X data, Y data, Z data (U,V,A)
!           = 312 : Z data, X data, Y data (A,U,V)
!           =  41 : Zxy data
!           =  42 : Zyx data
!           =  43 : Zxy and Zyx
!           =  44 : Full tensor (Zxy, Zyx, Zxx, Zyy) data.
!
!     ORDER = 1 : the first data component for all times (frequencies) is followed
!                 by the next data component for all times (frequencies) in the
!                 order specified by KCMP
!           = 2 : all of the data components for each channel are followed by
!                 all of the data components for the next channel in the order
!                 specified by KCMP
!
!       FIQ is required for frequency domain only.
!
!       FIQ = 1 : for each component the inphase and quadrature are paired (I,Q)
!           = 2 : for each component the inphase and quadrature are paired (Q,I)
!           = 3 : all the inphase data grouped followed by all the quadrature data
!           = 4 : all the quadrature data grouped followed by all the inphase data
!                 followed by NFRQ quadrature data for that station.
!
!
!**  RECORD 2: DATA_FLOOR
!
!
!      Any data value whose absolute magnitude is less than DATA_FLOOR will be
!      weighted to zero.
!
!      For TIME-DOMAIN only one value is required.
!
!      For FREQUENCY-DOMAIN, 2 * NFRQ values must be entered.
!
!      If FIQ = 1 or 3 : The first NFRQ values refer to Inphase (real) measurements
!                        for each frequency followed by NFRQ values for each
!                        Quadrature (imaginary) measurement,
!
!      If KCMP = 2 or 4 : The first NFRQ values refer to Quadrature (imaginary)
!                         measurements for each frequency followed by NFRQ values
!                         for each Inphase measurement.
!
!      Loki assumes that each group will start from the floor for the
!      lowest frequency and proceed sequentially to that for the highest.
!
!
!      Data Weights (Data rejection)
!      -----------------------------
!
!      Individual data points can be weighted to zero (rejected) by giving them
!      zero value and using a non-zero data floor.
!
!      Individual frequencies/channels can be weighted to zero by using a data
!      floor higher than their maximum value.
!
!      All the data for specific stations can be weighted to zero by specifying
!      those stations in RECORD 4.
!
!
!**  RECORD 3: N0STAT, N0CHNL, N0PTS
!
!
!      N0STAT - number of stations for which all the data will be weighted to zero.
!               These must be specified in RECORD 4.
!               Otherwise, set N0STAT = 0 if data from all stations is to be used.
!
!      N0CHNL - number of PCHNLs for which all the data will be weighted to zero.
!               These must be specified in RECORD 5
!               Otherwise, set N0STAT = 0 if data from all stations is to be used.
!
!      N0PTS  - number of data points not covered by K0STAT & K0CHNL which will be
!               weighted to zero.  These must be specified in RECORD 6
!
!      Time Domain
!      -----------
!
!      N0CHNL is the number of time-domain channels to be rejected.
!      Set N0CHNL = 0 if all chanels are to be used.
!
!      If N0CHNL > 0, these channels must be specified in RECORD 5.
!
!      If NCHNL < 0, all channels outside the range specified in RECORD 5 will
!                    be rejected.
!
!      Frequency Domain
!      ----------------
!
!      N0CHNL allows the user to reject inphase or quadrature data for inversion.
!
!      N0CHNL = 0 : invert on inphase and quadrature data.
!             = 1 : invert on inphase data only.
!             = 2 : invert on quadrature data only.
!
!
!     ----------------------------------------------------------------------
!     only if N0STAT > 0 : N0STAT records follow, J = 1,N0STAT
!**   RECORD 4.J:  KLINE(J), KSTAT(J)
!
!        all data from station KSTAT(J) of line KLINE(J) will be rejected.
!     ----------------------------------------------------------------------
!
!     ----------------------------------------------------------------------
!     only if time domain and N0CHNL > 0
!**   RECORD 5:  K0CHNL(1:N0CHNL) - indices of channels for which all data
!                                   will be rejected
!     ----------------------------------------------------------------------
!
!     ----------------------------------------------------------------------
!     only if time domain and N0CHNL < 0
!**   RECORD 5:  KSTART, KEND - data from all channels with indices < KSTART
!                               or indices > KEND will be rejected.
!     ----------------------------------------------------------------------
!
!     ----------------------------------------------------------------------
!     only if N0PTS > 0
!**   RECORD 6:  (J0CH(I),J0ST(I)), I = 1,N0PTS)
!        PCHNL and station indices of individual points to be weighted to 0.
!        using the above PCHNL ordering convention
!     ----------------------------------------------------------------------
!
!
!    DATA ENTRY
!    ==========
!
!
!    DO for each receiver J of each line K:
!
!**  RECORD 7.J.0: LINE(J), KSTAT(I,J), DATA(I,J,K), K = 1 to NPCHNL
!
!          LINE(K) = Line number of line K
!       KSTAT(J,K) = station index for receiver J of Line K
!      DATA(I,J,K) = consists of all the data (channels & components) associated
!                    with KSTAT(J,K) of Line K, in the format specified by KCMP in RECORD 1.
!
!
!===========================================================================!
!                                                                           !
!                           End Loki input data                           !
!                                                                           !
!===========================================================================!


 MODULE KG_Filter_coefficients
!--------------------------

 IMPLICIT NONE

 INTEGER, PARAMETER :: JNLO=-250, JNHI=150, NDEC_JN=15, QL=SELECTED_REAL_KIND(12,80)
 INTEGER J9
 REAL(KIND=QL) WJ0(JNLO:JNHI), WJ1(JNLO:JNHI), WCOS(-200:99), DELCOS, SHFTJN
 SAVE

!  Filter restored to original 7 February, 2000 (artificial shift removed)

!  J0 filter coefficients computed from the Niels Christensen program, FILCOA
!  for the following parameters:
!
!   ANY =  0      AMY =  0      NDEC = 15       NLO = -250        NHI =  150
!   IOPT = 1   ISHIFT = 0      OMEGA = .3 PI    EPS = 1.0E-12      SC = 3.257209
!      A = 0.162875             DEL0 = 0.14314998               ERROR =  1.4032E-08

 DATA SHFTJN /0.14314998_QL/
 DATA (WJ0(J9), J9= -250,-161)/ &
  2.86608135867D-18,  3.34160553102D-18,  3.89602601168D-18,  4.54243283439D-18,  5.29608785801D-18,  6.17478510356D-18, &
  7.19927087644D-18,  8.39373359283D-18,  9.78637487555D-18,  1.14100754027D-17,  1.33031712306D-17,  1.55103589191D-17, &
  1.80837508313D-17,  2.10841055215D-17,  2.45822622636D-17,  2.86608135867D-17,  3.34160553102D-17,  3.89602601168D-17, &
  4.54243283439D-17,  5.29608785801D-17,  6.17478510356D-17,  7.19927087644D-17,  8.39373359283D-17,  9.78637487555D-17, &
  1.14100754027D-16,  1.33031712306D-16,  1.55103589191D-16,  1.80837508313D-16,  2.10841055215D-16,  2.45822622636D-16, &
  2.86608135867D-16,  3.34160553102D-16,  3.89602601168D-16,  4.54243283439D-16,  5.29608785801D-16,  6.17478510356D-16, &
  7.19927087644D-16,  8.39373359283D-16,  9.78637487555D-16,  1.14100754027D-15,  1.33031712306D-15,  1.55103589191D-15, &
  1.80837508313D-15,  2.10841055215D-15,  2.45822622636D-15,  2.86608135867D-15,  3.34160553102D-15,  3.89602601168D-15, &
  4.54243283439D-15,  5.29608785801D-15,  6.17478510356D-15,  7.19927087644D-15,  8.39373359283D-15,  9.78637487555D-15, &
  1.14100754027D-14,  1.33031712306D-14,  1.55103589191D-14,  1.80837508313D-14,  2.10841055215D-14,  2.45822622636D-14, &
  2.86608135867D-14,  3.34160553102D-14,  3.89602601168D-14,  4.54243283439D-14,  5.29608785801D-14,  6.17478510356D-14, &
  7.19927087644D-14,  8.39373359283D-14,  9.78637487555D-14,  1.14100754027D-13,  1.33031712306D-13,  1.55103589191D-13, &
  1.80837508313D-13,  2.10841055215D-13,  2.45822622636D-13,  2.86608135867D-13,  3.34160553102D-13,  3.89602601168D-13, &
  4.54243283439D-13,  5.29608785801D-13,  6.17478510356D-13,  7.19927087644D-13,  8.39373359283D-13,  9.78637487555D-13, &
  1.14100754027D-12,  1.33031712306D-12,  1.55103589191D-12,  1.80837508313D-12,  2.10841055215D-12,  2.45822622636D-12/
 DATA (WJ0(J9),J9= -160,-71)/ &
  2.86608135867D-12,  3.34160553102D-12,  3.89602601168D-12,  4.54243283439D-12,  5.29608785801D-12,  6.17478510356D-12, &
  7.19927087644D-12,  8.39373359283D-12,  9.78637487555D-12,  1.14100754027D-11,  1.33031712306D-11,  1.55103589191D-11, &
  1.80837508313D-11,  2.10841055215D-11,  2.45822622636D-11,  2.86608135867D-11,  3.34160553102D-11,  3.89602601168D-11, &
  4.54243283439D-11,  5.29608785801D-11,  6.17478510356D-11,  7.19927087644D-11,  8.39373359283D-11,  9.78637487555D-11, &
  1.14100754027D-10,  1.33031712306D-10,  1.55103589191D-10,  1.80837508313D-10,  2.10841055215D-10,  2.45822622636D-10, &
  2.86608135867D-10,  3.34160553102D-10,  3.89602601168D-10,  4.54243283439D-10,  5.29608785801D-10,  6.17478510356D-10, &
  7.19927087644D-10,  8.39373359283D-10,  9.78637487555D-10,  1.14100754027D-09,  1.33031712306D-09,  1.55103589191D-09, &
  1.80837508313D-09,  2.10841055215D-09,  2.45822622636D-09,  2.86608135867D-09,  3.34160553102D-09,  3.89602601168D-09, &
  4.54243283439D-09,  5.29608785801D-09,  6.17478510356D-09,  7.19927087644D-09,  8.39373359283D-09,  9.78637487555D-09, &
  1.14100754027D-08,  1.33031712306D-08,  1.55103589191D-08,  1.80837508313D-08,  2.10841055215D-08,  2.45822622636D-08, &
  2.86608135867D-08,  3.34160553102D-08,  3.89602601168D-08,  4.54243283439D-08,  5.29608785801D-08,  6.17478510356D-08, &
  7.19927087644D-08,  8.39373359283D-08,  9.78637487555D-08,  1.14100754027D-07,  1.33031712306D-07,  1.55103589191D-07, &
  1.80837508313D-07,  2.10841055215D-07,  2.45822622635D-07,  2.86608135866D-07,  3.34160553102D-07,  3.89602601167D-07, &
  4.54243283438D-07,  5.29608785799D-07,  6.17478510354D-07,  7.19927087640D-07,  8.39373359277D-07,  9.78637487545D-07, &
  1.14100754026D-06,  1.33031712304D-06,  1.55103589187D-06,  1.80837508307D-06,  2.10841055205D-06,  2.45822622620D-06/
 DATA (WJ0(J9),J9= -70,19)/ &
  2.86608135842D-06,  3.34160553063D-06,  3.89602601105D-06,  4.54243283340D-06,  5.29608785643D-06,  6.17478510107D-06, &
  7.19927087248D-06,  8.39373358656D-06,  9.78637486561D-06,  1.14100753870D-05,  1.33031712056D-05,  1.55103588795D-05, &
  1.80837507685D-05,  2.10841054221D-05,  2.45822621060D-05,  2.86608133369D-05,  3.34160549143D-05,  3.89602594894D-05, &
  4.54243273495D-05,  5.29608770041D-05,  6.17478485378D-05,  7.19927048056D-05,  8.39373296541D-05,  9.78637388116D-05, &
  1.14100738267D-04,  1.33031687328D-04,  1.55103549604D-04,  1.80837445571D-04,  2.10840955776D-04,  2.45822465035D-04, &
  2.86607886087D-04,  3.34160157229D-04,  3.89601973751D-04,  4.54242289050D-04,  5.29607209800D-04,  6.17476012564D-04, &
  7.19923128912D-04,  8.39367085119D-04,  9.78627543681D-04,  1.14099178031D-03,  1.33029214523D-03,  1.55099630479D-03, &
  1.80831234191D-03,  2.10831111434D-03,  2.45806862870D-03,  2.86583158466D-03,  3.34120966900D-03,  3.89539861933D-03, &
  4.54143849891D-03,  5.29451197347D-03,  6.17228756167D-03,  7.19531268313D-03,  8.38746058912D-03,  9.77643350230D-03, &
  1.13943208262D-02,  1.32782050079D-02,  1.54707967971D-02,  1.80210634703D-02,  2.09847837166D-02,  2.44249145050D-02, &
  2.84115778193D-02,  3.30213524808D-02,  3.83353639832D-02,  4.44353673090D-02,  5.13965627145D-02,  5.92752031985D-02, &
  6.80880607240D-02,  7.77794366644D-02,  8.81696149649D-02,  9.88766639298D-02,  1.09202052802D-01,  1.17971700371D-01, &
  1.23332521049D-01,  1.22530035854D-01,  1.11753240889D-01,  8.62569960973D-02,  4.11899187108D-02, -2.61456504772D-02, &
 -1.11691705121D-01, -1.97411432453D-01, -2.44254055664D-01, -1.95918893763D-01, -1.49300191739D-02,  2.33634698676D-01, &
  3.13582629541D-01, -4.47760615930D-03, -3.86535797015D-01, -3.87589109967D-03,  4.18653972543D-01, -4.16298788795D-01/
 DATA (WJ0(J9),J9= 20,109)/ &
  2.34448877498D-01, -9.52158343728D-02,  3.09020778713D-02, -8.49535839509D-03,  2.06835506815D-03, -4.67185821059D-04, &
  1.02086153218D-04, -2.20830053233D-05,  4.76413760468D-06, -1.02705545675D-06,  2.21421979164D-07, -4.77750910705D-08, &
  1.03340738634D-08, -2.25102276694D-09,  4.99715357680D-10, -1.16500471179D-10,  3.03986897639D-11, -9.72611811870D-12, &
  3.99994042396D-12, -2.00348565820D-12,  1.11608417099D-12, -6.50767639555D-13,  3.86180817012D-13, -2.30659587418D-13, &
  1.38093695980D-13, -8.27455585993D-14,  4.95961642994D-14, -2.97302965597D-14,  1.78224472343D-14, -1.06841897105D-14, &
  6.40498685290D-15, -3.83968417568D-15,  2.30182896520D-15, -1.37991039489D-15,  8.27234374391D-16, -4.95913890248D-16, &
  2.97292643817D-16, -1.78222228351D-16,  1.06841401468D-16, -6.40497544674D-17,  3.83968128138D-17, -2.30182807939D-17, &
  1.37991004842D-17, -8.27234560136D-18,  4.95913797287D-18, -2.97292590016D-18,  1.78222272891D-18, -1.06841382487D-18, &
  6.40497431324D-19, -3.83968224515D-19,  2.30182767120D-19, -1.37990980321D-19,  8.27234414081D-20, -4.95914134387D-20, &
  2.97292537295D-20, -1.78222241286D-20,  1.06841455108D-20, -6.40497317742D-21,  3.83968156424D-21, -2.30182923671D-21, &
  1.37990955793D-21, -8.27234267383D-22,  4.95914046240D-22, -2.97292739490D-22,  1.78222209690D-22, -1.06841436161D-22, &
  6.40497753124D-23, -3.83968088314D-23,  2.30182784256D-23, -1.37991049701D-23,  8.27234475022D-24, -4.95913958682D-24, &
  2.97292559305D-24, -1.78222330828D-24,  1.06841371450D-24, -6.40497639510D-25,  3.83968184851D-25, -2.30182842033D-25, &
  1.37990966066D-25, -8.27234682962D-26,  4.95914083158D-26, -2.97292634049D-26,  1.78222222810D-26, -1.06841489841D-26, &
  6.40497251344D-27, -3.83968281228D-27,  2.30182702533D-27, -1.37991000702D-27,  8.27234181627D-28, -4.95914207635D-28/
 DATA WJ0(110:150)/ &
  2.97292963477D-28, -1.78222420371D-28,  1.06841425086D-28, -6.40497412376D-29,  3.83968377606D-29, -2.30182957681D-29, &
  1.37991153609D-29, -8.27235098582D-30,  4.95914332316D-30, -2.97292528486D-30,  1.78222312353D-30, -1.06841451903D-30, &
  6.40498122076D-31, -3.83968474142D-31,  2.30183015458D-31, -1.37991188353D-31,  8.27234597206D-32, -4.95914031749D-32, &
  2.97292858145D-32, -1.78222357152D-32,  1.06841478804D-32, -6.40498282844D-33,  3.83968570659D-33, -2.30182876031D-33, &
  1.37991104718D-33, -8.27234805187D-34,  4.95914156225D-34, -2.97292932767D-34,  1.78222401887D-34, -1.06841414093D-34, &
  6.40497895409D-35, -3.83968338099D-35,  2.30182933903D-35, -1.37991139355D-35,  8.27235013127D-36, -4.95914281087D-36, &
  2.97292752582D-36, -1.78222294016D-36,  1.06841440910D-36, -6.40498056176D-37,  3.83968434477D-37/

!  J1 filter coefficients computed from the Niels Christensen program, FILCOA
!  for the following parameters:
!
!   ANY =  1      AMY =  0      NDD+C = 15       NLO = -250        NHI =  150
!   IOPT = 1   ISHIFT = 0      OMD+GA = .3 PI    D+PS = 1.0D-12      SC = 3.257209
!      A = 0.162875             DD+L0 = 0.14314998               D+RROR =  1.4032D-08

 DATA (WJ1(J9),J9= -250,-161)/ &
  2.67560875879D-35,  3.63710586576D-35,  4.94412310292D-35,  6.72082533724D-35,  9.13599687416D-35,  1.24190757379D-34, &
  1.68819499732D-34,  2.29485865865D-34,  3.11953078380D-34,  4.24055410750D-34,  5.76442432690D-34,  7.83590704850D-34, &
  1.06517903247D-33,  1.44795792522D-33,  1.96829085937D-33,  2.67560875879D-33,  3.63710586576D-33,  4.94412310292D-33, &
  6.72082533724D-33,  9.13599687416D-33,  1.24190757379D-32,  1.68819499732D-32,  2.29485865865D-32,  3.11953078380D-32, &
  4.24055410750D-32,  5.76442432690D-32,  7.83590704850D-32,  1.06517903247D-31,  1.44795792522D-31,  1.96829085937D-31, &
  2.67560875879D-31,  3.63710586576D-31,  4.94412310292D-31,  6.72082533724D-31,  9.13599687416D-31,  1.24190757379D-30, &
  1.68819499732D-30,  2.29485865865D-30,  3.11953078380D-30,  4.24055410750D-30,  5.76442432690D-30,  7.83590704850D-30, &
  1.06517903247D-29,  1.44795792522D-29,  1.96829085937D-29,  2.67560875879D-29,  3.63710586576D-29,  4.94412310292D-29, &
  6.72082533724D-29,  9.13599687416D-29,  1.24190757379D-28,  1.68819499732D-28,  2.29485865865D-28,  3.11953078380D-28, &
  4.24055410750D-28,  5.76442432690D-28,  7.83590704850D-28,  1.06517903247D-27,  1.44795792522D-27,  1.96829085937D-27, &
  2.67560875879D-27,  3.63710586576D-27,  4.94412310292D-27,  6.72082533724D-27,  9.13599687416D-27,  1.24190757379D-26, &
  1.68819499732D-26,  2.29485865865D-26,  3.11953078380D-26,  4.24055410750D-26,  5.76442432690D-26,  7.83590704850D-26, &
  1.06517903247D-25,  1.44795792522D-25,  1.96829085937D-25,  2.67560875879D-25,  3.63710586576D-25,  4.94412310292D-25, &
  6.72082533724D-25,  9.13599687416D-25,  1.24190757379D-24,  1.68819499732D-24,  2.29485865865D-24,  3.11953078380D-24, &
  4.24055410750D-24,  5.76442432690D-24,  7.83590704850D-24,  1.06517903247D-23,  1.44795792522D-23,  1.96829085937D-23/
 DATA (WJ1(J9),J9= -160,-71)/ &
  2.67560875879D-23,  3.63710586576D-23,  4.94412310292D-23,  6.72082533724D-23,  9.13599687416D-23,  1.24190757379D-22, &
  1.68819499732D-22,  2.29485865865D-22,  3.11953078380D-22,  4.24055410750D-22,  5.76442432690D-22,  7.83590704850D-22, &
  1.06517903247D-21,  1.44795792522D-21,  1.96829085937D-21,  2.67560875879D-21,  3.63710586576D-21,  4.94412310292D-21, &
  6.72082533724D-21,  9.13599687416D-21,  1.24190757379D-20,  1.68819499732D-20,  2.29485865865D-20,  3.11953078380D-20, &
  4.24055410750D-20,  5.76442432690D-20,  7.83590704850D-20,  1.06517903247D-19,  1.44795792522D-19,  1.96829085937D-19, &
  2.67560875879D-19,  3.63710586576D-19,  4.94412310292D-19,  6.72082533724D-19,  9.13599687416D-19,  1.24190757379D-18, &
  1.68819499732D-18,  2.29485865865D-18,  3.11953078380D-18,  4.24055410750D-18,  5.76442432690D-18,  7.83590704850D-18, &
  1.06517903247D-17,  1.44795792522D-17,  1.96829085937D-17,  2.67560875879D-17,  3.63710586576D-17,  4.94412310292D-17, &
  6.72082533724D-17,  9.13599687416D-17,  1.24190757379D-16,  1.68819499732D-16,  2.29485865865D-16,  3.11953078380D-16, &
  4.24055410750D-16,  5.76442432690D-16,  7.83590704850D-16,  1.06517903247D-15,  1.44795792522D-15,  1.96829085937D-15, &
  2.67560875879D-15,  3.63710586576D-15,  4.94412310292D-15,  6.72082533724D-15,  9.13599687416D-15,  1.24190757379D-14, &
  1.68819499732D-14,  2.29485865865D-14,  3.11953078380D-14,  4.24055410750D-14,  5.76442432690D-14,  7.83590704849D-14, &
  1.06517903247D-13,  1.44795792522D-13,  1.96829085938D-13,  2.67560875878D-13,  3.63710586577D-13,  4.94412310288D-13, &
  6.72082533728D-13,  9.13599687406D-13,  1.24190757380D-12,  1.68819499729D-12,  2.29485865868D-12,  3.11953078372D-12, &
  4.24055410758D-12,  5.76442432666D-12,  7.83590704871D-12,  1.06517903240D-11,  1.44795792527D-11,  1.96829085917D-11/
 DATA (WJ1(J9),J9= -70,19)/ &
  2.67560875891D-11,  3.63710586515D-11,  4.94412310317D-11,  6.72082533541D-11,  9.13599687462D-11,  1.24190757324D-10, &
  1.68819499736D-10,  2.29485865695D-10,  3.11953078363D-10,  4.24055410221D-10,  5.76442432542D-10,  7.83590703194D-10, &
  1.06517903172D-09,  1.44795791998D-09,  1.96829085611D-09,  2.67560874206D-09,  3.63710585268D-09,  4.94412304898D-09, &
  6.72082528725D-09,  9.13599669890D-09,  1.24190755523D-08,  1.68819493996D-08,  2.29485859113D-08,  3.11953059487D-08, &
  4.24055386543D-08,  5.76442370102D-08,  7.83590618983D-08,  1.06517882412D-07,  1.44795762309D-07,  1.96829016283D-07, &
  2.67560770231D-07,  3.63710352883D-07,  4.94411942636D-07,  6.72081747305D-07,  9.13598412795D-07,  1.24190492063D-06, &
  1.68819059152D-06,  2.29484968860D-06,  3.11951559104D-06,  4.24052372735D-06,  5.76437203602D-06,  7.83580400571D-06, &
  1.06516106220D-05,  1.44792293329D-05,  1.96822917833D-05,  2.67548981332D-05,  3.63689436167D-05,  4.94371845248D-05, &
  6.72010067340D-05,  9.13461935181D-05,  1.24165945005D-04,  1.68772580859D-04,  2.29400955289D-04,  3.11793204874D-04, &
  4.23764974965D-04,  5.75897507579D-04,  7.82597702990D-04,  1.06332133421D-03,  1.44456435715D-03,  1.96195766368D-03, &
  2.66401748131D-03,  3.61551958902D-03,  4.90456094796D-03,  6.64729428357D-03,  9.00112880743D-03,  1.21689223295D-02, &
  1.64231258930D-02,  2.20996958736D-02,  2.96400942278D-02,  3.95385050500D-02,  5.24078149405D-02,  6.87615215337D-02, &
  8.91013723344D-02,  1.13192375541D-01,  1.40192739735D-01,  1.66618485339D-01,  1.87030308669D-01,  1.89612379729D-01, &
  1.61380285157D-01,  8.29859362099D-02, -4.46335736689D-02, -2.01737898138D-01, -2.84006740802D-01, -1.90854624427D-01, &
  1.45861570853D-01,  3.42338340245D-01,  5.72930699760D-02, -4.71068534718D-01,  2.63969067746D-01,  8.25956507901D-02/
 DATA (WJ1(J9),J9= 20,109)/ &
 -2.22236420794D-01,  2.04428998525D-01, -1.44401888321D-01,  9.24618900674D-02, -5.69896615248D-02,  3.45697730305D-02, &
 -2.08227940873D-02,  1.25054653306D-02, -7.50178808640D-03,  4.49828025678D-03, -2.69688071237D-03,  1.61678766116D-03, &
 -9.69249547051D-04,  5.81052166908D-04, -3.48332124427D-04,  2.08819730575D-04, -1.25184162926D-04,  7.50459390809D-05, &
 -4.49888596104D-05,  2.69701130091D-05, -1.61681580285D-05,  9.69255610555D-06, -5.81053473294D-06,  3.48332405883D-06, &
 -2.08819791213D-06,  1.25184175990D-06, -7.50459418954D-07,  4.49888602168D-07, -2.69701131398D-07,  1.61681580566D-07, &
 -9.69255611161D-08,  5.81053473425D-08, -3.48332405911D-08,  2.08819791219D-08, -1.25184175991D-08,  7.50459418957D-09, &
 -4.49888602168D-09,  2.69701131398D-09, -1.61681580566D-09,  9.69255611161D-10, -5.81053473425D-10,  3.48332405911D-10, &
 -2.08819791219D-10,  1.25184175991D-10, -7.50459418957D-11,  4.49888602168D-11, -2.69701131398D-11,  1.61681580566D-11, &
 -9.69255611161D-12,  5.81053473425D-12, -3.48332405911D-12,  2.08819791219D-12, -1.25184175991D-12,  7.50459418957D-13, &
 -4.49888602168D-13,  2.69701131398D-13, -1.61681580566D-13,  9.69255611161D-14, -5.81053473425D-14,  3.48332405911D-14, &
 -2.08819791219D-14,  1.25184175991D-14, -7.50459418957D-15,  4.49888602168D-15, -2.69701131398D-15,  1.61681580566D-15, &
 -9.69255611161D-16,  5.81053473425D-16, -3.48332405911D-16,  2.08819791219D-16, -1.25184175991D-16,  7.50459418957D-17, &
 -4.49888602168D-17,  2.69701131398D-17, -1.61681580566D-17,  9.69255611161D-18, -5.81053473425D-18,  3.48332405911D-18, &
 -2.08819791219D-18,  1.25184175991D-18, -7.50459418957D-19,  4.49888602168D-19, -2.69701131398D-19,  1.61681580566D-19, &
 -9.69255611161D-20,  5.81053473425D-20, -3.48332405911D-20,  2.08819791219D-20, -1.25184175991D-20,  7.50459418957D-21/
 DATA WJ1(110:150)/ &
 -4.49888602168D-21,  2.69701131398D-21, -1.61681580566D-21,  9.69255611161D-22, -5.81053473425D-22,  3.48332405911D-22, &
 -2.08819791219D-22,  1.25184175991D-22, -7.50459418957D-23,  4.49888602168D-23, -2.69701131398D-23,  1.61681580566D-23, &
 -9.69255611161D-24,  5.81053473425D-24, -3.48332405911D-24,  2.08819791219D-24, -1.25184175991D-24,  7.50459418957D-25, &
 -4.49888602168D-25,  2.69701131398D-25, -1.61681580566D-25,  9.69255611161D-26, -5.81053473425D-26,  3.48332405911D-26, &
 -2.08819791219D-26,  1.25184175991D-26, -7.50459418957D-27,  4.49888602168D-27, -2.69701131398D-27,  1.61681580566D-27, &
 -9.69255611161D-28,  5.81053473425D-28, -3.48332405911D-28,  2.08819791219D-28, -1.25184175991D-28,  7.50459418957D-29, &
 -4.49888602168D-29,  2.69701131398D-29, -1.61681580566D-29,  9.69255611161D-30, -5.81053473425D-30/

!  Niels Christensen shifted cosine filter:
!  12 points per decade, OMEGA = .3 PI

  DATA DELCOS /.00632173D0 /
  DATA (WCOS (J9), J9 = -200, -21)/ &
  3.27764748749D-18,  3.97096058632D-18,  4.81092858166D-18,  5.82857304036D-18,  7.06147744874D-18,  8.55517523993D-18, &
  1.03648314276D-17,  1.25572799515D-17,  1.52134919784D-17,  1.84315663162D-17,  2.23303523839D-17,  2.70538395400D-17, &
  3.27764748749D-17,  3.97096058632D-17,  4.81092858166D-17,  5.82857304036D-17,  7.06147744874D-17,  8.55517523993D-17, &
  1.03648314276D-16,  1.25572799515D-16,  1.52134919784D-16,  1.84315663162D-16,  2.23303523839D-16,  2.70538395400D-16, &
  3.27764748749D-16,  3.97096058632D-16,  4.81092858166D-16,  5.82857304036D-16,  7.06147744874D-16,  8.55517523993D-16, &
  1.03648314276D-15,  1.25572799515D-15,  1.52134919784D-15,  1.84315663162D-15,  2.23303523839D-15,  2.70538395400D-15, &
  3.27764748749D-15,  3.97096058632D-15,  4.81092858166D-15,  5.82857304036D-15,  7.06147744874D-15,  8.55517523993D-15, &
  1.03648314276D-14,  1.25572799515D-14,  1.52134919784D-14,  1.84315663162D-14,  2.23303523839D-14,  2.70538395400D-14, &
  3.27764748749D-14,  3.97096058632D-14,  4.81092858166D-14,  5.82857304036D-14,  7.06147744874D-14,  8.55517523993D-14, &
  1.03648314276D-13,  1.25572799515D-13,  1.52134919784D-13,  1.84315663162D-13,  2.23303523839D-13,  2.70538395400D-13, &
  3.27764748749D-13,  3.97096058632D-13,  4.81092858166D-13,  5.82857304036D-13,  7.06147744874D-13,  8.55517523993D-13, &
  1.03648314276D-12,  1.25572799515D-12,  1.52134919784D-12,  1.84315663162D-12,  2.23303523839D-12,  2.70538395400D-12, &
  3.27764748749D-12,  3.97096058632D-12,  4.81092858166D-12,  5.82857304036D-12,  7.06147744874D-12,  8.55517523993D-12, &
  1.03648314276D-11,  1.25572799515D-11,  1.52134919784D-11,  1.84315663162D-11,  2.23303523839D-11,  2.70538395400D-11, &
  3.27764748749D-11,  3.97096058632D-11,  4.81092858166D-11,  5.82857304036D-11,  7.06147744874D-11,  8.55517523993D-11, &
  1.03648314276D-10,  1.25572799515D-10,  1.52134919784D-10,  1.84315663162D-10,  2.23303523839D-10,  2.70538395400D-10, &
  3.27764748749D-10,  3.97096058632D-10,  4.81092858166D-10,  5.82857304036D-10,  7.06147744874D-10,  8.55517523993D-10, &
  1.03648314276D-09,  1.25572799515D-09,  1.52134919784D-09,  1.84315663162D-09,  2.23303523839D-09,  2.70538395400D-09, &
  3.27764748749D-09,  3.97096058632D-09,  4.81092858166D-09,  5.82857304036D-09,  7.06147744874D-09,  8.55517523993D-09, &
  1.03648314276D-08,  1.25572799515D-08,  1.52134919784D-08,  1.84315663162D-08,  2.23303523839D-08,  2.70538395400D-08, &
  3.27764748749D-08,  3.97096058632D-08,  4.81092858166D-08,  5.82857304036D-08,  7.06147744874D-08,  8.55517523992D-08, &
  1.03648314276D-07,  1.25572799515D-07,  1.52134919784D-07,  1.84315663162D-07,  2.23303523839D-07,  2.70538395400D-07, &
  3.27764748748D-07,  3.97096058631D-07,  4.81092858163D-07,  5.82857304032D-07,  7.06147744866D-07,  8.55517523979D-07, &
  1.03648314273D-06,  1.25572799511D-06,  1.52134919777D-06,  1.84315663149D-06,  2.23303523815D-06,  2.70538395358D-06, &
  3.27764748674D-06,  3.97096058499D-06,  4.81092857928D-06,  5.82857303614D-06,  7.06147744122D-06,  8.55517522657D-06, &
  1.03648314038D-05,  1.25572799093D-05,  1.52134919033D-05,  1.84315661826D-05,  2.23303521464D-05,  2.70538391177D-05, &
  3.27764741237D-05,  3.97096045276D-05,  4.81092834413D-05,  5.82857261799D-05,  7.06147669760D-05,  8.55517390427D-05, &
  1.03648290523D-04,  1.25572757278D-04,  1.52134844670D-04,  1.84315529598D-04,  2.23303286305D-04,  2.70537973035D-04, &
  3.27763997594D-04,  3.97094723005D-04,  4.81090482791D-04,  5.82853080445D-04,  7.06140233231D-04,  8.55504167951D-04, &
  1.03645938870D-03,  1.25568576016D-03,  1.52127408052D-03,  1.84302307509D-03,  2.23279769616D-03,  2.70496162210D-03/

  DATA (WCOS (J9), J9= -20, 99)/ &
  3.27689631886D-03,  3.96962511374D-03,  4.80855324839D-03,  5.82435024343D-03,  7.05396657593D-03,  8.54182367870D-03, &
  1.03410843805D-02,  1.25150721296D-02,  1.51384287367D-02,  1.82981828574D-02,  2.20932012652D-02,  2.66326428704D-02, &
  3.20280504750D-02,  3.83817031757D-02,  4.57529090015D-02,  5.41138165506D-02,  6.32336060872D-02,  7.25429239280D-02, &
  8.07814005943D-02,  8.56648215301D-02,  8.29754131995D-02,  6.61728839009D-02,  2.49099879313D-02, -5.25662370332D-02, &
 -1.77257695902D-01, -3.38275600250D-01, -4.82415902998D-01, -4.55992280486D-01, -7.52812327135D-02,  6.65970979261D-01, &
  8.99170503986D-01, -3.96592370781D-01, -1.38198747238D+00,  1.66395693227D+00, -9.30334922154D-01,  3.30012032268D-01, &
 -8.19311720454D-02,  1.48662188728D-02, -2.13960121462D-03,  2.89777944084D-04, -4.10252655190D-05,  5.96303531789D-06, &
 -8.72916816254D-07,  1.28031659199D-07, -1.87886052472D-08,  2.75763186999D-09, -4.04758530392D-10,  5.94101668614D-11, &
 -8.72020580969D-12,  1.27995006152D-12, -1.87869546474D-13,  2.75750390141D-14, -4.04729332639D-15,  5.94004630834D-16, &
 -8.70764639675D-17,  1.27459963186D-17, -1.82944370627D-18,  2.67836880337D-19, -3.04833935943D-20,  1.64313000801D-21, &
  3.01142825752D-21, -5.21478596825D-22,  1.37002813677D-21, -6.52797182652D-22,  1.40079856288D-22, -1.40667671784D-22, &
  1.70033730143D-23, -2.74453364807D-23,  2.41787117103D-23, -1.78716987481D-23,  4.99883433782D-24, -4.06084044984D-24, &
  2.89670334941D-24, -8.77965537372D-25,  1.21194987045D-25, -1.74181776862D-25,  1.50307641169D-25, -1.09826064382D-25, &
  3.14586965779D-26, -2.51308231025D-26,  1.77594485992D-26, -1.17543940755D-26,  8.42024121640D-28, -1.10510759608D-27, &
  9.31619291992D-28, -6.75339996352D-28,  1.97531071217D-28, -1.55371775135D-28,  1.08953022579D-28, -7.17780762223D-29, &
  2.55398099963D-29, -6.99012347840D-30,  5.76787420019D-30, -4.15016624873D-30,  1.23507827864D-30, -9.59703688264D-31, &
  6.68070421281D-31, -4.37770918800D-31,  1.57257106203D-31, -1.06708053061D-31,  3.57322505765D-32, -2.54887457918D-32, &
  7.72541668811D-33, -5.92277283725D-33,  4.09438835539D-33, -1.32259081936D-33,  1.67919911757D-33, -2.76812163102D-34, &
  2.21131777864D-34,  5.28010221339D-35,  1.03429563330D-34, -7.40916006860D-36,  9.72409086858D-36, -8.19752817047D-36, &
 -2.58911797964D-36, -3.98829026336D-36,  1.78104494324D-37, -3.32579083872D-37,  3.00732538418D-37, -2.24730545742D-37/

 END MODULE KG_Filter_coefficients

 MODULE KG_Input_routines
!--------------------------

!** CONTAINS: READ_SYSTEM_DATA, READ_MODEL_DATA, SET_FRQ

 Use iso_Fortran_env
 Use KG_Metadata
 IMPLICIT NONE

! SYSTEM & LITHOLOGY DIMENSIONS
! -----------------------------

 INTEGER, PARAMETER :: NPROP=7, QL=SELECTED_REAL_KIND(12,80)
 Integer, Parameter :: FVERS = 690
 REAL, PARAMETER :: PI=3.141592654, PI2=PI/2., R2D=180./PI,  D2R=PI/180.
 INTEGER NR,NW,ND,NS,NLG,NRI,np,MSG,MXERR,DO3D,TDFD,IPPD,STEP,NSX,PRFL,ISTOP,KRXW, &
         MCHNL,NCHNL,NFRQ,NFT,CMPMT(4),KMPMT(4),MCMP,KHSQ,SOURCE_TYPE,SURVEY_TYPE,   &
         NLINES,MLINES,NTX,MXVRTX,MQVR,MXRS,ISYS,KTX,K1,MXTX,NTXL,J,JS,JT,JF,JV,JR,  &
         MRXTX,MRXL,NLITH,NPULS,NTYRP,NTYPLS,NPPD,MD1,MD2,NERX,NHRX,IPM,NHID,NM,NMP, &
         OUTPUT,KACC,SOLVER,INRM,KFRQE
 INTEGER, ALLOCATABLE, DIMENSION(:) :: LINE,IPLT,IDH,NVRTX,UNITS,KNORM,NRX,RX_TYPE,CMP,NRGTX,NRXTX, &
                                       IDHE,HEADER_ID,KHID
 INTEGER, ALLOCATABLE, DIMENSION(:,:)  :: KRGTX,RXID,KNORM2,NCTD,LNTR,NCMP
 REAL MIN_FREQ,MAX_FREQ,T0,T0SX,OFFTYM,REFTYM,PULSE,RXFMNT,TXFREQ,TXMNT,ZMAX,ZMIN,SV_AZM
 REAL, ALLOCATABLE, DIMENSION(:) :: TXON,WAVEFORM,CURNT,TRP,TMS,WTMS,TOPN,TCLS,FREQ,SWX,TXZ,SXZ,SVAZM,SDZ0, &
                                    TXLNGTH,TXWDTH,TXCLN,TXAZM,SXDIP,SXAZM,RHOTRP,RXMNT,RXOFF,RXOTX
 REAL, ALLOCATABLE, DIMENSION(:,:) :: SWY,LYTH,SXE,SXN,ZRXTX,RXDIP,RXAZM,BHDIP,BHAZM,RXZ,XRXOF,YRXOF,ZRXOF,DSTAT
 REAL, ALLOCATABLE, DIMENSION(:,:,:) :: XRXTX,YRXTX,RXE,RXN
 REAL, ALLOCATABLE :: BFTL(:,:,:,:)
 REAL(KIND=QL) ECNTRD,NCNTRD,QD,QFRQ1,QFRQ2,FQQ
 REAL(KIND=QL), ALLOCATABLE :: SDN0(:),SDE0(:)
 REAL(KIND=QL), ALLOCATABLE, DIMENSION(:,:)   :: SXED,SXND,CLCD,MTPLT
 REAL(KIND=QL), ALLOCATABLE, DIMENSION(:,:,:) :: YXZPLT,RXED,RXND
 LOGICAL PRTSEC, INVERT, MT
 CHARACTER (LEN=120) INP,TITLE
 
 Integer :: tvals(8)
 DATA NR,NW,ND,NLG,NRI,np,NMP,NM,NS /3,4,7,9,13,14,25,10,8/

! Specific parameters for Loki
! ------------------------------
 INTEGER NE,NN,NZ,NEAST,NNORTH,NSZ,NEL,NER,NNL,NNR,NZT,NZB,NAIR
 INTEGER NLYR,NPLT,JL,MXA,MXB,JP,MXAB,BEG,FIN,JA,JB,JAB,NAB,NAJ,NBJ,JP2,JAB2, &
         NAB2,MXRHO,NRMGT,MXCL2,NPAR
 INTEGER,ALLOCATABLE :: LITH(:,:,:)
 INTEGER, ALLOCATABLE :: LITHL(:), NEDGE(:,:)
 INTEGER,ALLOCATABLE, DIMENSION(:) :: NA,NB,NCELL2,LITHP
 REAL CELLW
 REAL, ALLOCATABLE, DIMENSION(:) :: RES,THK,SIG0,RMU,REPS,CHRG,CTAU,CFREQ,SIG_T,CHRGP,CTAUP, &
                                    CFREQP,PLTOP,XCNTR,YCNTR,PLNGTH,PLWDTH,DZM,PLAZM,PLDIP,  &
                                    DIP,ROT,PLROT,DA,DB,MPAR,PLG,PLUNJ
 REAL, ALLOCATABLE, DIMENSION(:,:) :: XCELL,YCELL,ZCELL
 REAL(KIND=QL),ALLOCATABLE, DIMENSION(:) :: RMUD,THKD
 REAL, ALLOCATABLE, DIMENSION(:,:)   :: ESURF,NSURF,ZSURF
 REAL, ALLOCATABLE, DIMENSION(:,:,:) :: XRM,YRM,ELOC,NLOC,ZLOC
 REAL(KIND=QL), ALLOCATABLE :: ESURFD(:,:),NSURFD(:,:),ZSURFD(:,:)
 REAL(KIND=QL), ALLOCATABLE :: ELOCD(:,:,:),NLOCD(:,:,:),ZLOCD(:,:,:)
 LOGICAL NESWAP

! Inversion specific parameters
! -----------------------------
 LOGICAL READ_RES_FILE,JCBN
 INTEGER MV1PRT,OUTPRT
 INTEGER NPART,NBNT,NBN,I,K,NSTAT,KCMP,KPRT
 INTEGER KTS,NDATA,KCHNL,MAXITS,CNVRG,INVPRT
 INTEGER,ALLOCATABLE,DIMENSION(:) :: XWTS,VSTAT
 INTEGER, ALLOCATABLE :: RWTS(:,:,:,:)
 INTEGER,ALLOCATABLE,DIMENSION(:,:,:) :: RWTS1
 INTEGER, ALLOCATABLE, DIMENSION(:) :: CXPAR,NCMPL,KMP
 REAL PARPCT,PCTCNV,S1
 REAL,ALLOCATABLE,DIMENSION(:) :: XPART,XPAR,DATA_FLOOR
 REAL, ALLOCATABLE, DIMENSION(:) :: XDATA,DNORM,XMODL,UBND,LBND,ELAS
 REAL, ALLOCATABLE :: RDATA(:,:,:,:)
 REAL,ALLOCATABLE,DIMENSION(:,:,:) :: RDATA1
 LOGICAL, ALLOCATABLE :: SINGLE(:)

 CONTAINS

   SUBROUTINE READ_SYSTEM_AND_SURVEY_DATA
!  --------------------------------------

!***  Called by MAIN
!***  Calls CUBSPL, CALL CONFIG_ID, WRITE_LOG_FILE


! DESCRIPTION OF SURVEY VARIABLES REQUIRED FOR MODELLING ENGINES
! --------------------------------------------------------------

!    SURVEY_TYPE = 1 : general survey
!                = 2 : moving rectangular loop Tx with fixed offset MD Rx
!                = 3 : magnetic dipole Tx with fixed offset MD Rx
!                = 4 : coincident loop
!                = 5 : borehole MD Tx with constant offset Rx
!                = 6 : magnetotellurics
!
!    SOURCE_TYPE = 1 : general loop
!                = 2 : grounded wire
!                = 3 : magnetic dipole
!                = 4 : coincident loop
!                = 5 : plane wave
!
!            NTX = number of transmitters specified
!       NVRTX(J) = number of vertices for transmitter J
!         MXVRTX - maximum number of vertices for any transmitter
!       SXE(K,J) = local east coordinate of vertex K for loop position J (taken from SXED)
!       SXN(K,J) = local coordinate of vertex K for loop position J
!       TXZ(J)   = ground clearance of dipole Tx J
!       SXZ(J)   = depth of dipole Tx J
!       SXDIP(J) = dip (in radians) of dipole J (eg; vertical = 0, horizontal = 90)
!       SXAZM(J) = azimuth (in radians) of dipole J (north = 0, east = 90)
!
!       NRXTX(J) - number of receivers for transmitter J
!          MRXTX - maximum number of receivers for any transmitter
!      RXID(I,J) - RX_TYPE of receiver I for transmitter J. 1 => MD; 2 => ED; 3 => point E field;  4=> cdnt loop.
!           MQVR - maximum number of vertices for all receivers
!   XRXTX(I,J,K) - north coordinate of the Kth vertex of the Ith receiver of transmitter J
!   YRXTX(I,J,K) - east coordinate of the Kth vertex of the Ith receiver of transmitter J
!     ZRXTX(I,J) - depth of the Ith receiver of transmitter J
!     XRXOF(I,L) - X offset along SV_AZM of Rx I on Tx line L.
!     YRXOF(I,L) - Y offset perpendicular to SV_AZM of Rx I on Tx line L.
!     ZRXOF(I,L) - ground clearance of Rx I on Tx line L.
!
!                  SURVEY_TYPE = 1
!                  ---------------
!      LNTR(1,L) : Tx index for Line L.   LNTR(2,L) = LNTR(1,L)
!      LNTR(3,L) : Rx index  for start of Line L
!      LNTR(4,L) : Rx index for finish of Line L
!
!                  SURVEY_TYPE > 1
!                  ---------------
!      LNTR(1,L) : Tx index for start of Line L
!      LNTR(2,L) : Tx index for finish of Line L
!      LNTR(3,L) : Rx index for Line L.   LNTR(4,L) = LNTR(3,L)
!
!      The Rx index for variable LNTR is defined with reference to specific
!      transmitters rather than the global number.  Tx indices are global.
!
!
! ADDITIONAL SURVEY VARIABLES REQUIRED FOR OUTPUT
! -----------------------------------------------
!
!    NLINES          - number of survey lines
!    NRX(I)          - the number of receivers in Line I.
!    RX_TYPE(I)      - Receiver type for Line I
!    RXMNT(I)        - dipole receiver moment (area * turns) for Line I
!    UNITS(I)        - units for line I
!    KNORM(I)        - normalisation indicator for line I
!    YXZPLT(1:3,I,L) - GPS east, GPS north, RL  plot coordinate for Ith receiver of Line L
!
!    TXMNT - moment for dipole transmitter
!
! INTERMEDIATE SURVEY VARIABLES USED IN READ_SYSTEM_AND_SURVEY_DATA
! -----------------------------------------------------------------
!
!     SV_AZM     - survey azimuth in degrees
!     RXN(I,J,K) - local north coordinate of vertex K of Rx I of Group J
!     RXE(I,J,K) - local east coordinate of the Kth vertex of the Ith receiver of receiver group J
!     RXZ(I,J)   - depth (z is positive down) of Rx I of Group J

 IMPLICIT NONE
 REAL, PARAMETER :: MIN_RXED=0.001   ! Minimum source & receiver electrode depths
 INTEGER NTRN,KTXP,IDH5
 REAL A1
 REAL(KIND=QL), ALLOCATABLE, DIMENSION(:,:,:) :: QD1,QD2

!      Initialise some variables.

 T0SX = 100.
 MXERR = 0             !  Initialise input error flag
 PRTSEC = .FALSE.
 REFTYM = 0.
 INVERT = .FALSE.
 KTS = -9              ! "Iteration" control for modelling tasks
 MRXTX = 1
 MXVRTX = 4
 SV_AZM = 0.
 NERX = 0
 CMPMT = 1
 KMPMT = 1
 MCMP = 3               !  Number of possible spatial components if not MT

!  Reproduce input data with no assignments and rewind file.

 Call Date_and_time(Values = tvals)
 Write (NW, 1) PNAME, PVERS, PDATE, PAUT1, PAUT2, PAUT3, PPROJ, PRELS, &
              compiler_version(), compiler_options(), &
              tvals(1:3), tvals(5:7)
Write ( *, 1) PNAME, PVERS, PDATE, PAUT1, PAUT2, PAUT3, PPROJ, PRELS, &
              compiler_version(), compiler_options(), &
              tvals(1:3), tvals(5:7)
WRITE(NW,'(T11,A/T11,A/)') 'INPUT DATA', '----------'

 REWIND NR

 READ(NR,'(A)') TITLE
 WRITE(NW,'(/1X,A)') TITLE

! Read model control & print parameters

 READ(NR,*)  TDFD, DO3D, ISYS, PRFL, ISTOP
 WRITE(NW,3) TDFD, DO3D, ISYS, PRFL, ISTOP
 IF (DO3D > 0) THEN
   IF (PRFL > 9) THEN
     PRTSEC = .TRUE.
     PRFL = PRFL - 10
   END IF
 ELSE IF (DO3D < 0) THEN
   INVERT = .TRUE.
   DO3D = 1
 END IF

 IF (DO3D == -2) THEN
   INRM = 2
 ELSE IF (DO3D == -1) THEN
   INRM = 1
 ELSE
   INRM = 1
 END IF

!   TDFD = 0, 1 or 2 for user controlled TD, standard TD or FD respectively.
!   DO3D = 2 => use old FD data from Loki.frq.
!        = 1 => compute new  plate model.
!        = 0 => compute layered 1/2 space model only.
!   PRFL - indicates profile or decay curve output plus scattered field output option.
!  ISTOP - read data and stop if ISTOP = 1

 KHSQ = 0
 IF (TDFD < 2) THEN
   IF (ISYS == 2) THEN
     CALL WRITE_LOG_FILE (NLG,40,MXERR,2)
   ELSE IF (ISYS == 6) THEN
     CALL WRITE_LOG_FILE (NLG,20,MXERR,2)
   ELSE IF (ISYS /= 0 .AND. ISYS /= 4) THEN
     CALL WRITE_LOG_FILE (NLG,16,MXERR,2)
   END IF
 ELSE
   IF (ISYS /= 0 .AND. ISYS /= 2 .AND. ISYS /= 6) CALL WRITE_LOG_FILE (NLG,17,MXERR,2)
 END IF

 IF (TDFD == 0) THEN
   READ(NR,*) MIN_FREQ,MAX_FREQ,IPPD,KHSQ
   WRITE(NW,7) KHSQ,IPPD,MIN_FREQ,MAX_FREQ
   IF (KHSQ == 1) THEN
     WRITE(NW,16)
     WRITE(*,16)
   END IF
 END IF

 IF (PRFL /= 0 .AND. PRFL /= 1) THEN
   CALL WRITE_LOG_FILE (NLG,2,MXERR,1)
   PRFL = 1
 END IF

 STEP = 2
 IF (TDFD < 2) THEN
   IF (DO3D == 1) THEN
     OPEN(ND,FILE = 'Loki.frq',STATUS = 'REPLACE')
   ELSE IF (DO3D == 2) THEN
     OPEN(ND,FILE = 'Loki.frq',STATUS = 'OLD')
   ELSE IF (DO3D > 2) THEN
     DO3D = 1
     CALL WRITE_LOG_FILE (NLG,3,MXERR,1)
   END IF

   NFT = 1                                      ! Used for dimensioning similar TD & FD arrays
   ALLOCATE (CURNT(NFT))

   IF (ISYS == 0) THEN                    ! Time-domain parameters
     READ(NR,*)  STEP, NSX, NCHNL, KRXW, REFTYM, OFFTYM
     WRITE(NW,4) STEP, NSX, NCHNL, KRXW, REFTYM, OFFTYM
     IF (KRXW /= 1 .AND. KRXW /= 2) CALL WRITE_LOG_FILE (NLG,5,MXERR,2)
   ELSE IF (ISYS == 4) THEN   ! UTEM: dB/dt from triangular current or
     READ (NR,*) TXFREQ,NCHNL
     WRITE(NW,40) TXFREQ,NCHNL
     IF (NCHNL /=10 .AND. NCHNL /= 20) CALL WRITE_LOG_FILE (NLG,18,MXERR,2)
     NSX = 1
     CURNT(1) = 1.            ! Step B response for full duty cycle rectangular pulse
     STEP = 4
     OFFTYM = 0
     REFTYM = 0.
     PULSE = .5 / TXFREQ
   END IF
   MCHNL = NCHNL
   ALLOCATE (TXON(NSX), WAVEFORM(NSX),TMS(NCHNL),WTMS(NCHNL),TOPN(NCHNL),TCLS(NCHNL),SWX(NSX),SWY(NSX,3))
   TXON=0.; WAVEFORM=0.; TMS=0.; WTMS=0.; TOPN=0.; TCLS=0; SWX=0; SWY=0

   IF (ISYS == 0) THEN                    ! Time-domain parameters
     READ(NR,*) (TXON(J),WAVEFORM(J), J = 1,NSX)  ! Read in source waveform.
     CURNT(1) = MAXVAL (WAVEFORM)

     WRITE(NW,5)
     DO J = 1, NSX
       WRITE(NW,'(3X,I4,F13.3,5X,G13.4)') J,TXON(J),WAVEFORM(J)
     END DO

     IF (KRXW == 1) THEN
       READ(NR,*) (TOPN(J) ,TCLS(J), J = 1,NCHNL)
       CALL SET_TIME_ORDER (NCHNL,TOPN,TCLS)       ! Ensure that channels go from early to late
       TMS = (TOPN + TCLS) / 2.
       WTMS = TCLS - TOPN
     ELSE
       READ(NR,*) TMS(1:NCHNL)
       READ(NR,*) WTMS(1:NCHNL)
       CALL SET_TIME_ORDER (NCHNL,TMS,WTMS)
       TCLS= TMS + WTMS /2.
       TOPN= TMS - WTMS /2.
     END IF
     PULSE = 1.E-3 * (OFFTYM + MAXVAL (TXON) )
     SWX = 1.E-3 * TXON
     SWY(1:NSX,3) = WAVEFORM(1:NSX)

   ELSE IF (ISYS == 4) THEN   ! UTEM: dB/dt from triangular current or
     SWX(1) = 0.              !       B from full duty rectangular pulse
     SWY(1,1) = 1

     TCLS(NCHNL) = 1000. * PULSE
     TOPN(NCHNL) = TCLS(NCHNL) / 2.
     DO J = NCHNL-1,1,-1
       TCLS(J) = TCLS(J+1) / 2.
       TOPN(J) = TCLS(J) / 2.
     END DO
     TMS = (TOPN + TCLS) / 2.
     WTMS = TCLS - TOPN

   END IF

   WRITE(NW,6) REFTYM

   TOPN = TOPN + REFTYM
   TCLS = TCLS + REFTYM
   DO J = 1,NCHNL
     WRITE(NW,'(7X,I4,2F12.3,F11.3,F12.3,F11.3)') J,TOPN(J),TCLS(J),WTMS(J),TMS(J)+REFTYM,TMS(J)
     IF ( TOPN(J) <= 0) CALL WRITE_LOG_FILE (NLG,7,MXERR,2)
   END DO
   TOPN = 1.E-3 * TOPN
   TCLS = 1.E-3 * TCLS

 ELSE IF (TDFD == 2) THEN          ! Frequency-domain systems
  ! Allocate TD arays to unit dimensions and null quantity.

  NCHNL = 1 ; NSX = 1 ; NTYRP = 1
  ALLOCATE (TXON(NSX),WAVEFORM(NSX),TMS(NCHNL),WTMS(NCHNL),TOPN(NCHNL), &
            TCLS(NCHNL),SWX(NSX),SWY(NSX,3),TRP(NTYRP))
  SWX = 0. ; SWY = 0. ; WAVEFORM = 0. ; TMS = 0. ; WTMS = 0. ; TOPN = 0. ; TCLS = 0.

   OPEN(ND,FILE = 'Loki.frq',STATUS = 'REPLACE')
   STEP = 2
   READ(NR,*)  NFRQ
   WRITE(NW,8) NFRQ
   NFT = NFRQ                        ! Used for dimensioning similar TD & FD arrays
   MCHNL = 2*NFRQ
   IF (ISYS == 2) MCHNL = NFRQ
   ALLOCATE( FREQ(NFRQ), CURNT(NFT))

! Read & write source waveform.

   IF (ISYS /= 6) THEN
     WRITE(NW,9)
     DO J = 1, NFRQ
       READ(NR,*) FREQ(J),CURNT(J)
       WRITE(NW,'(3X,I4,G13.4,5X,G13.4)') J,FREQ(J),CURNT(J)
     END DO
   ELSE
     WRITE(NW,39)
     DO J = 1, NFRQ
       READ(NR,*) FREQ(J)
       WRITE(NW,'(3X,I4,G13.4)') J,FREQ(J)
     END DO
   END IF
 END IF


! Survey Information
! ------------------
! Read in absolute locations in double precision variables and convert to
! REAL body centred coordinates before entering the computation realm.

 NTRN = 1; TXMNT = 1; NTXL = 1; MT = .FALSE.

 READ(NR,*)  SURVEY_TYPE
 WRITE(NW,10) SURVEY_TYPE
 IF (ISYS == 4) THEN              ! Reverse channels for UTEM output
   CALL SET_TMSR (NCHNL,TMS)      ! (late to early)
   CALL SET_TMSR (NCHNL,WTMS)
 END IF

 SELECT CASE (SURVEY_TYPE)                        ! Set NLINES
 CASE (1)                                         ! General Source-Receiver Option
   READ(NR,*) NLINES,MRXL,NTX,SOURCE_TYPE,MXVRTX,A1
   NTXL = NTX
   IF (SOURCE_TYPE < 3) THEN
     NTRN = INT (A1)
     WRITE(NW,11) NLINES,MRXL,NTX,SOURCE_TYPE,MXVRTX,NTRN
   ELSE IF (SOURCE_TYPE == 3) THEN
     TXMNT = A1
     WRITE(NW,21) NLINES,NTX,SOURCE_TYPE,MXVRTX,TXMNT
   END IF
   ALLOCATE (HEADER_ID(NLINES))
   HEADER_ID = 100
   IF (TDFD == 2) HEADER_ID = 200

   IF (NTX > NLINES) CALL WRITE_LOG_FILE (NLG,11,MXERR,2)
   MXTX = NTX
   NTXL = NTX

 CASE (2)                                  !  Moving Loop Tx - Fixed MD Rx offset
   SOURCE_TYPE = 1
   READ(NR,*) NTXL, MRXL, MRXTX, NTRN
   TXMNT = REAL (NTRN)
   WRITE(NW,23) NTXL, MRXL, MRXTX, NTRN
   NLINES = NTXL * MRXTX
   IF (MRXTX == 0) NLINES = NTXL
   MXTX = NTXL * MRXL
   MXVRTX = 4
   ALLOCATE (RXE(MRXL,NLINES,1),RXN(MRXL,NLINES,1))
   RXN = 0.; RXE = 0.
   NHID = 1
   ALLOCATE (HEADER_ID(NLINES))
   HEADER_ID = 110                         ! 110 & 210 imply magnetic dipole receivers on the surface
   IF (TDFD == 2) HEADER_ID = 210

 CASE (3)                                  !  Moving MD Tx   - Fixed MD Rx offset
   SOURCE_TYPE = 3
   READ(NR,*)   NTXL, MRXL, MRXTX, TXMNT
   WRITE(NW,24) NTXL, MRXL, MRXTX, TXMNT
   NLINES = NTXL * MRXTX
   MXTX = NTXL * MRXL
   MXVRTX = 1
   ALLOCATE (RXE(MRXL,NLINES,1),RXN(MRXL,NLINES,1))
   RXN = 0.; RXE = 0.
   NHID = 1
   ALLOCATE (HEADER_ID(NLINES))
   HEADER_ID = 110                         ! 110 & 210 imply magnetic dipole receivers on the surface
   IF (TDFD == 2) HEADER_ID = 210

 CASE (4)                                  !  Moving Coincident Loop Tx - Rx
   SOURCE_TYPE = 4
   READ(NR,*) NLINES, MRXL, NTRN
   NTXL = NLINES
   TXMNT = REAL (NTRN)
   WRITE(NW,26) NLINES, MRXL, NTRN
   MXTX = NLINES * MRXL
   MXVRTX = 4
   MRXTX = 1
   ALLOCATE (RXE(MRXL,NLINES,1),RXN(MRXL,NLINES,1))
   RXN = 0.; RXE = 0.
   NHID = 1
   ALLOCATE (HEADER_ID(NLINES))
   HEADER_ID = 140

 CASE (5)                                  !  Downhole MD Tx_Rx probe
   SOURCE_TYPE = 3
   READ(NR,*)  NLINES, MRXL, IDH5, TXMNT
   WRITE(NW,25) NLINES, MRXL, IDH5, TXMNT
   ALLOCATE (RXE(MRXL,NLINES,1),RXN(MRXL,NLINES,1))
   MXTX = NLINES * MRXL
   MXVRTX = 1
   MRXTX = 1
   NTXL = NLINES
   RXN = 0.; RXE = 0.
   NHID = 1
   ALLOCATE (HEADER_ID(NLINES))
   HEADER_ID = 110                     ! 110 & 210 imply downhole magnetic dipole output : X-Y-Z
   IF (TDFD == 2) HEADER_ID = 210      ! 111 & 211 imply downhole magnetic dipole output : U-V-A
   HEADER_ID = HEADER_ID + IDH5        ! 112 & 212 imply downhole magnetic dipole output : W-N-S

 CASE (6)                                  !  Magnetotellurics
   WRITE(NW,91)
   WRITE(*,91)
   STOP
   MT = .TRUE.
   SOURCE_TYPE = 5
   NLINES = 2
   MXTX = 2
   NTX = 2
   NTXL = NTX
   MXVRTX = 1
   IF (INVERT) THEN
     READ(NR,*) NHRX,NERX,IPM,CMPMT(1:4)
     IF (IPM /= 1) IPM = 0
     WRITE(NW,50) NHRX,NERX,IPM,CMPMT(1:4)
   ELSE
     READ(NR,*) NHRX,NERX,IPM
     IF (IPM /= 1) IPM = 0
     WRITE(NW,51) NHRX,NERX,IPM
   END IF
   IF (NHRX > NERX) CALL WRITE_LOG_FILE (NLG,20,MXERR,2)
   MRXL = NERX
   MRXTX = NHRX + NERX
   NHID = 1
   ALLOCATE (HEADER_ID(1))
   HEADER_ID = 260 + IPM

 CASE DEFAULT
   CALL WRITE_LOG_FILE (NLG,10,MXERR,2)   !  Write fatal error message
 END SELECT

 ALLOCATE (LINE(NLINES),UNITS(NLINES),KNORM(NLINES),SVAZM(NLINES),NRX(NLINES),CMP(NLINES),NCMPL(NLINES), &
           IDH(NLINES),IPLT(NLINES),RXMNT(NLINES),RX_TYPE(NLINES),LNTR(4,NLINES),SDN0(NTXL),SDE0(NTXL),  &
           SDZ0(NTXL),TXLNGTH(NTXL),TXWDTH(NTXL),RXOFF(NTXL),XRXOF(MRXTX,NTXL),YRXOF(MRXTX,NTXL),        &
           ZRXOF(MRXTX,NTXL),NVRTX(MXTX),RXOTX(MXTX),SXDIP(MXTX),SXAZM(MXTX),TXZ(MXTX),SXZ(MXTX),        &
           NRXTX(MXTX),TXCLN(MXTX),TXAZM(MXTX),SXND(MXVRTX,MXTX),SXED(MXVRTX,MXTX),DSTAT(MRXL,NTXL),     &
           RXZ(MRXL,NLINES))

 LINE=0;  UNITS=0;  KNORM = 0;  SVAZM=0.;  NRX=1; CMP=1; RXZ=0.; IPLT=1; RXMNT=1.; RX_TYPE=1
 SDN0=0.D0;  SDE0=0.D0;  SDZ0=0.;  TXLNGTH=1.;  TXWDTH=1.;  RXOFF=0.;  XRXOF=0.;  YRXOF=0.;  ZRXOF=0.
 NVRTX=4; NRXTX=0;  SXDIP=0.;  SXAZM=0.;  TXCLN=0.;  TXAZM=0.;  TXZ=0.;  SXZ=0.;  SXND=0.D0
 SXED=0.D0;  DSTAT=0.; RXOTX=0.;  MXRS = 1;  MQVR = 1; IDH = 0; LNTR=1

! MXRS = maximum number of subnet receivers
!      = 1 for magnetic dipole; = 5 for electric dipole;  = 100 for coincident loop
! MQVR = maximum number of vertices for a receiver
!      = 1 for magnetic dipole; = 2 for electric dipole

 SELECT CASE (SURVEY_TYPE)

 CASE (1)                                         !  General Source-Receiver Option
   SELECT CASE (SOURCE_TYPE)
   CASE (1:2)                                     ! Closed or open loops
     IF (SOURCE_TYPE == 1) WRITE(NW,12)
     IF (SOURCE_TYPE == 2) WRITE(NW,13)

     DO JS = 1,NTX
       READ(NR,*) NVRTX(JS),TXZ(JS)
       WRITE(NW,14) JS,NVRTX(JS)
       DO JV = 1, NVRTX(JS)
         READ(NR,*) SXED(JV,JS),SXND(JV,JS)
         WRITE(NW,'(I5,3F14.2)') JV,SXED(JV,JS),SXND(JV,JS),TXZ(JS)
       END DO
     END DO
     SXZ = -TXZ           ! Convert from elevation to depth

   CASE(3)                                        !  Magnetic dipole source
     NVRTX = 1
     WRITE(NW,15)
     DO JS = 1,NTX
       READ(NR,*) SXED(1,JS),SXND(1,JS),TXZ(JS),TXCLN(JS),TXAZM(JS)
       WRITE(NW,'(I5,3F14.2,2F9.1,F10.1)') JS,SXED(1,JS),SXND(1,JS),TXZ(JS),TXCLN(JS),TXAZM(JS)
       SXAZM(JS) = TXAZM(JS) * D2R
       SXDIP(JS) = TXCLN(JS) * D2R
       IF (ABS (SXDIP(JS)) < 1.E-3) SXDIP(JS) = 0.
       IF (ABS (SXAZM(JS)) < 1.E-3) SXAZM(JS) = 0.
     END DO
     SXZ = -TXZ           ! Convert from elevation to depth

   CASE DEFAULT
     CALL WRITE_LOG_FILE (NLG,8,MXERR,2)
   END SELECT

   ALLOCATE (QD1(MRXL,NLINES,2),QD2(MRXL,NLINES,2),BHDIP(MRXL,NLINES),BHAZM(MRXL,NLINES))

   QD1 = 0.D0; QD2 = 0.D0; BHDIP = 90.; BHAZM = 0.

   RXMNT = 1.
   CMP = 1
   NCMPL = 1
   DO JL = 1,NLINES
     READ(NR,*) LINE(JL),LNTR(1,JL),RX_TYPE(JL),NRX(JL),UNITS(JL)
     WRITE(NW,17) LINE(JL),LNTR(1,JL),RX_TYPE(JL),NRX(JL),UNITS(JL)
     IF (RX_TYPE(JL) /= 1) THEN
       WRITE(NW,92)
       WRITE(*,92)
       STOP
     END IF

     IF (NRX(JL) > MRXL) THEN
       CALL WRITE_LOG_FILE (NLG,12,MXERR,1)
       NRX(J) = MRXL
       WRITE (NLG,33) JL,MRXL
     END IF
     HEADER_ID(JL) = HEADER_ID(JL) + 10*RX_TYPE(JL)

     IF (SOURCE_TYPE == 3 .AND. RX_TYPE(JL) > 1) THEN
       CALL WRITE_LOG_FILE (NLG,6,MXERR,2)
       WRITE(NLG,'(T3,A,I3)') 'Rx selection error for Line',JL
     END IF

     IF (TDFD /= 2 .AND. RX_TYPE(JL) == 3) THEN
       CALL WRITE_LOG_FILE (NLG,30,MXERR,2)
       WRITE(NLG,'(T3,A,I3)') 'Rx selection error for Line',JL
     END IF

     LNTR(2,JL) = LNTR(1,JL)                     !  transmitter index for Line JL
     KTX = LNTR(1,JL)
     NRXTX(KTX) = NRXTX(KTX) + NRX(JL)

     SELECT CASE (RX_TYPE(JL))
     CASE (1)                                         ! Magnetic dipole receiver
       READ(NR,*) CMP(JL),SV_AZM,KNORM(JL),IPLT(JL),IDH(JL),RXMNT(JL)
       CALL VALIDATE_CMP (NLG,MXERR,JL,NLINES,CMP,NCMPL,ISYS)
       WRITE(NW,22) CMP(JL),KNORM(JL),IPLT(JL),IDH(JL),SV_AZM,RXMNT(JL)
       HEADER_ID(JL) = HEADER_ID(JL) + IDH(JL)
       IF (KNORM(JL) > 0) THEN
         IF (UNITS(JL) < 31 .OR. UNITS(JL) > 35) CALL WRITE_LOG_FILE (NLG,15,MXERR,2)
       END IF
       SVAZM(JL) = SV_AZM * D2R

       IF (IDH(JL) == 0) THEN                          ! Surface receiver processing
         WRITE(NW,18)
         DO JR = 1, NRX(JL)
           READ (NR,*) QD1(JR,JL,1),QD2(JR,JL,1),RXZ(JR,JL)    ! RXED,RXND,RXZ
           WRITE(NW,'(I4,3F12.1)') JR,QD1(JR,JL,1),QD2(JR,JL,1),RXZ(JR,JL)
         END DO
       ELSE                ! U-V-A processing
         WRITE(NW,19)
         DO JR = 1, NRX(JL)
           READ (NR,*) QD1(JR,JL,1),QD2(JR,JL,1),RXZ(JR,JL),BHDIP(JR,JL),BHAZM(JR,JL)    ! RXED,RXND,RXZ,BHDIP,BHAZM
           WRITE(NW,'(I4,3F12.1,2F9.1)') JR,QD1(JR,JL,1),QD2(JR,JL,1),RXZ(JR,JL),BHDIP(JR,JL),BHAZM(JR,JL)
         END DO
       END IF

     CASE (2)                                         ! Electric dipole receiver
       IDH(JL) = -1
       MQVR = 2
       MXRS = 5
       WRITE(NW,20)
       DO JR = 1, NRX(JL)
         READ (NR,*) QD1(JR,JL,1),QD2(JR,JL,1),QD1(JR,JL,2),QD2(JR,JL,2),RXZ(JR,JL)
         RXZ(JR,JL) = MIN (RXZ(JR,JL),-MIN_RXED)         ! Electrodes should be subsurface
         WRITE(NW,'(I4,5F13.2)') JR,QD1(JR,JL,1),QD2(JR,JL,1),QD1(JR,JL,2),QD2(JR,JL,2),RXZ(JR,JL)
       END DO

     CASE (3)                                         ! Point E field receiver
       READ(NR,*) CMP(JL),SV_AZM
       IDH(JL) = -1
       CALL VALIDATE_CMP (NLG,MXERR,JL,NLINES,CMP,NCMPL,ISYS)
       WRITE(NW,35) CMP(JL),SV_AZM
       SVAZM(JL) = SV_AZM * D2R
       DO JR = 1, NRX(JL)
         READ (NR,*) QD1(JR,JL,1),QD2(JR,JL,1),RXZ(JR,JL)
         RXZ(JR,JL) = MIN (RXZ(JR,JL),-MIN_RXED)         ! Electrodes should be subsurface
         WRITE(NW,'(I4,3F13.2)') JR,QD1(JR,JL,1),QD2(JR,JL,1),RXZ(JR,JL)
       END DO
     CASE DEFAULT
       CALL WRITE_LOG_FILE (NLG,9,MXERR,2)
     END SELECT
   END DO
   MRXTX = MAXVAL (NRXTX)

   ALLOCATE (RXED(MRXL,NLINES,MQVR),RXND(MRXL,NLINES,MQVR),RXE(MRXL,NLINES,MQVR),RXN(MRXL,NLINES,MQVR))

   RXED(1:MRXL,1:NLINES,1:MQVR) = QD1(1:MRXL,1:NLINES,1:MQVR)
   RXND(1:MRXL,1:NLINES,1:MQVR) = QD2(1:MRXL,1:NLINES,1:MQVR)

   DEALLOCATE (QD1, QD2)

   ALLOCATE (KHID(NLINES))
   NHID = 1
   KHID(1) = HEADER_ID(1)
   HEADER: DO JL = 2, NLINES
     DO J = 1,JL-1
       IF (HEADER_ID(JL) == HEADER_ID(J)) CYCLE HEADER
     END DO
     NHID = NHID + 1
     KHID(NHID) = HEADER_ID(JL)
   END DO HEADER
   IF (ISYS == 2) THEN
     HEADER_ID = 250
     MCMP = 1           ! Sampo output is single component (ABS (Bz/Bx))
   END IF

 CASE (2)              !  Moving Loop Tx - Fixed MD Rx offset

   NTX = 0
   NVRTX = 4
   NRXTX = MRXTX
   DO JS = 1,NTXL                 ! DSTAT(1,JS) was initialised to 0
     READ(NR,*) KTXP,TXLNGTH(JS),TXWDTH(JS),SV_AZM,SDE0(JS),SDN0(JS)
     IF (KTXP > MRXL) THEN
       CALL WRITE_LOG_FILE (NLG,14,MXERR,1)
       KTXP = MRXL
       WRITE (NLG,34) JS,MRXL
     END IF

     READ(NR,*) DSTAT(2:KTXP,JS)
     WRITE(NW,27) JS,KTXP,TXLNGTH(JS),TXWDTH(JS),SDE0(JS),SDN0(JS),SV_AZM,DSTAT(2:KTXP,JS)
     K1 = NTX + 1
     NTX = NTX + KTXP

     DO JR = 1,MRXTX
       JL = JR + (JS-1) * MRXTX       ! Line index
       SVAZM(JL) = SV_AZM * D2R
       NRX(JL) = KTXP
       READ(NR,*) LINE(JL),CMP(JL),XRXOF(JR,JS),YRXOF(JR,JS),ZRXOF(JR,JS),RXMNT(JL),UNITS(JL),KNORM(JL),IPLT(JL)
       CALL VALIDATE_CMP (NLG,MXERR,JL,NLINES,CMP,NCMPL,ISYS)
       WRITE(NW,30) LINE(JL),XRXOF(JR,JS),YRXOF(JR,JS),ZRXOF(JR,JS),CMP(JL),UNITS(JL),KNORM(JL),IPLT(JL),RXMNT(JL)

       LNTR(1,JL) = K1
       LNTR(2,JL) = NTX
       LNTR(3,JL) = JR             ! Rx index at start of Line JL
       LNTR(4,JL) = JR             ! Rx index at end of Line JL
     END DO
   END DO

 CASE (3)   !  Moving MD Tx   - Fixed MD Rx offset
   NVRTX = 1
   NRXTX = MRXTX
   NTX = 0
   DO JS = 1,NTXL                 ! DSTAT(1,JS) was initialised to 0
     READ(NR,*) KTXP,TXCLN(JS),TXAZM(JS),SV_AZM,SDE0(JS),SDN0(JS),SDZ0(JS)
     IF (KTXP > MRXL) THEN
       CALL WRITE_LOG_FILE (NLG,14,MXERR,1)
       KTXP = MRXL
       WRITE (NLG,34) JS,MRXL
     END IF
     READ(NR,*) DSTAT(2:KTXP,JS)
     WRITE(NW,28) JS,KTXP,TXCLN(JS),TXAZM(JS),SDE0(JS),SDN0(JS),SDZ0(JS),SV_AZM,DSTAT(2:KTXP,JS)
     K1 = NTX + 1
     NTX = NTX + KTXP
     SXDIP(K1:NTX) = TXCLN(JS) * D2R
     SXAZM(K1:NTX) = TXAZM(JS) * D2R

     DO JR = 1,MRXTX
       JL = JR + (JS-1) * MRXTX
       SVAZM(JL) = SV_AZM * D2R
       NRX(JL) = KTXP
       READ(NR,*) LINE(JL),CMP(JL),XRXOF(JR,JS),YRXOF(JR,JS),ZRXOF(JR,JS),RXMNT(JL),UNITS(JL),KNORM(JL),IPLT(JL)
       CALL VALIDATE_CMP (NLG,MXERR,JL,NLINES,CMP,NCMPL,ISYS)
       WRITE(NW,30) LINE(JL),XRXOF(JR,JS),YRXOF(JR,JS),ZRXOF(JR,JS),CMP(JL),UNITS(JL),KNORM(JL),IPLT(JL),RXMNT(JL)

       LNTR(1,JL) = K1             ! Tx index at start of Line JL
       LNTR(2,JL) = NTX            ! Tx index at end of Line JL
       LNTR(3,JL) = JR             ! Rx index at start of Line JL
       LNTR(4,JL) = JR             ! Rx index at end of Line JL
     END DO
   END DO

 CASE (4)              !  Coincident Loop
   NVRTX = 4
   RX_TYPE = 4
   NRXTX = 1
   MXRS = 100          ! maximum number of subnet receivers
   IPLT = 3
   NTX = 0
   MCMP = 1
   DO JL = 1,NLINES                 ! DSTAT(1,JL) was initialised to 0
     READ(NR,*) KTXP,TXLNGTH(JL),TXWDTH(JL),SV_AZM,SDE0(JL),SDN0(JL)
     IF (KTXP > MRXL) THEN
       CALL WRITE_LOG_FILE (NLG,14,MXERR,1)
       KTXP = MRXL
       WRITE (NLG,34) JL,MRXL
     END IF
     READ(NR,*) DSTAT(2:KTXP,JL)
     WRITE(NW,27) JL,KTXP,TXLNGTH(JL),TXWDTH(JL),SV_AZM,SDE0(JL),SDN0(JL),DSTAT(2:KTXP,JL)
     K1 = NTX + 1
     NTX = NTX + KTXP
     LNTR(1,JL) = K1
     LNTR(2,JL) = NTX

     SVAZM(JL) = SV_AZM * D2R
     NRX(JL) = KTXP
     READ(NR,*) LINE(JL), UNITS(JL)
     WRITE(NW,29) LINE(JL), UNITS(JL)
     LNTR(3,JL) = 1             ! Rx index at start of Line JL
     LNTR(4,JL) = 1             ! Rx index at end of Line JL

   END DO

 CASE (5)                   !  Downhole MD Tx_Rx probe
   NRXTX = 1
   NVRTX = 1
   ALLOCATE (QD1(1,MXTX,1),QD2(1,MXTX,1),BHDIP(MRXL,NLINES),BHAZM(MRXL,NLINES))
   NTX = 0
   DO JL = 1,NLINES
     READ(NR,*) LINE(JL),SV_AZM,NRX(JL),CMP(JL),RXOFF(JL),RXMNT(JL),UNITS(JL),KNORM(JL),IPLT(JL),IDH(JL)
     CALL VALIDATE_CMP (NLG,MXERR,JL,NLINES,CMP,NCMPL,ISYS)
     WRITE(NW,31) LINE(JL),SV_AZM,NRX(JL),CMP(JL),RXOFF(JL),UNITS(JL),KNORM(JL),IPLT(JL),IDH(JL),RXMNT(JL)
     SVAZM(JL) = SV_AZM * D2R
     K1 = NTX
     NTX = NTX + NRX(JL)
     LNTR(1,JL) = K1 + 1        ! Tx index at start of Line JL
     LNTR(2,JL) = NTX           ! Tx index at end of Line JL
     LNTR(3,JL) = 1             ! Rx index at start of Line JL
     LNTR(4,JL) = 1             ! Rx index at end of Line JL

     WRITE(NW,32)
     DO JR = 1,NRX(JL)
       JS = JR + K1
       READ(NR,*) QD2(1,JS,1),QD1(1,JS,1),TXZ(JS),BHDIP(JR,JL),BHAZM(JR,JL)
       WRITE(NW,'(I5,3F14.2,2F9.1,F10.1)') JR,QD2(1,JS,1),QD1(1,JS,1),TXZ(JS),BHDIP(JR,JL),BHAZM(JR,JL)
       SXZ(JS) = -TXZ(JS)
       SXDIP(JS) = (90. - BHDIP(JR,JL)) * D2R
       SXAZM(JS) = BHAZM(JR,JL) * D2R
       RXOTX(JS) = RXOFF(JL)
     END DO
   END DO
   SXND(1,1:NTX) = QD1(1,1:NTX,1)
   SXED(1,1:NTX) = QD2(1,1:NTX,1)
   DEALLOCATE (QD1,QD2)
 CASE (6)                   !  Magnetotellurics
   MCMP = 4                 !  Components defined as ZXX, ZXY, ZYX, ZYY
   RXMNT = 1.
   NRXTX = MRXTX
   NRX(1) = NERX
   NRX(2) = NHRX
   ALLOCATE (IDHE(NERX),RXED(MRXL,NLINES,1),RXND(MRXL,NLINES,1),RXE(MRXL,NLINES,1),RXN(MRXL,NLINES,1))
   RXED = 0._QL;  RXND = 0._QL; RXE = 0.;  RXN = 0.
   WRITE(NW,53)
   DO J = 1,NHRX
     READ(NR,*) RXED(J,1,1), RXND(J,1,1)   ! positions of H receivers
     WRITE(NW,55) J,RXED(J,1,1), RXND(J,1,1)
   END DO
   WRITE(NW,54)
   DO J = 1,NERX
     READ(NR,*) RXED(J,2,1), RXND(J,2,1), IDHE(J) ! H-E receiver reference + positions of E receivers
     WRITE(NW,55) IDHE(J),RXED(J,2,1), RXED(J,2,1)
     IF (IDHE(J) > NHRX) CALL WRITE_LOG_FILE (NLG,19,MXERR,2)
   END DO
 END SELECT

 IF (SURVEY_TYPE /= 6 .AND. MAXVAL (NRX) > MRXL) THEN
   CALL WRITE_LOG_FILE(NLG,32,MXERR,2)
   DO JL = 1,NLINES
     IF (NRX(JL) < MRXL) CYCLE
     WRITE(NLG,90) JL,NRX(JL),MRXL
   END DO
 END IF

 IF (MAXVAL (KNORM) > 0 .AND. STEP == 0) THEN
   KNORM = 0
   CALL WRITE_LOG_FILE (NLG,13,MXERR,1)
 END IF

 MLINES = NLINES
 IF (MT) MLINES = 1
 ALLOCATE (BFTL(MCHNL,MRXL,MCMP,MLINES),     &
           SXN(MXVRTX,NTX),SXE(MXVRTX,NTX),XRXTX(MRXTX,NTX,MQVR),YRXTX(MRXTX,NTX,MQVR),ZRXTX(MRXTX,NTX), &
           YXZPLT(3,MRXL,NLINES),MTPLT(2,NERX),NCTD(MRXTX,NTX),RXID(MRXTX,NTX),KNORM2(MRXTX,NTX))

 ALLOCATE (RDATA(MCHNL,MRXL,MCMP,MLINES),RWTS(MCHNL,MRXL,MCMP,MLINES))
 RDATA = 0. ; RWTS = 1

 BFTL = 0.
 SXN = 0.; SXE = 0.; XRXTX = 0.;  YRXTX = 0.;  ZRXTX = 0. ; MTPLT = 0._QL
 YXZPLT = 0.D0;  RXID = 1;  KNORM2 = 0

 MD1 = 1; MD2 = 1
 IF (SURVEY_TYPE == 1 .OR. SURVEY_TYPE == 5) THEN
   MD1 = MRXL
   MD2 = NLINES
   ALLOCATE (RXAZM(MD1,MD2),RXDIP(MD1,MD2))
   DO JL = 1,NLINES
     DO JR = 1,NRX(JL)
       RXDIP(JR,JL) = (90. - BHDIP(JR,JL)) * D2R
       RXAZM(JR,JL) = BHAZM(JR,JL) * D2R
     END DO
   END DO
   DEALLOCATE (BHAZM,BHDIP)
 ELSE
   MD1 = 1
   MD2 = 1
   ALLOCATE (RXAZM(MD1,MD2),RXDIP(MD1,MD2))
   RXDIP = 0.
   RXAZM = 0.
 END IF

 IF (TDFD < 2) SWY = SWY * TXMNT
 IF (ISYS /= 6) CURNT = CURNT * TXMNT
 DEALLOCATE (TXCLN,TXAZM)

  1 Format (/, 2x, 78('-'), &
    /, 2x, '| Program:  ', a, &
    /, 2x, '| Version:  ', a, &
    /, 2x, '| Date:     ', a, &
    /, 2x, '|', &
    /, 2x, '| Authors:  ', a, &
    /, 2x, '|           ', a, &
    /, 2x, '| Contact:  ', a, &
    /, 2x, '| Project:  ', a, &
    /, 2x, '| License:  ', a, &
    /, 2x, '|', &
    /, 2x, '| Compiler: ', a, &
    /, 2x, '| Options:  ', a, &
    /, 2x, '|', &
    /, 2x, '| Started:  ', i4.4, '-', i2.2, '-', i2.2, 'T', i2.2, ':', i2.2, ':', i2.2, &
    /, 2x, 78('-'), /)
  
 2 FORMAT (T1,79('-'))
 3 FORMAT(/T3,'TDFD =',I2,';   DO3D =',I3,';   ISYS =',I2,';   PRFL =',I3,';   ISTOP =',I2)
 4 FORMAT(/10X,'+-----------------------------------------+' &
          /10X,'+  Time-Domain Ground System Information  +' &
          /10X,'+-----------------------------------------+' &
         // T3,'STEP =',I2,';   NSX =',I4,';   NCHNL =',I4,';   KRXW =',I2,';   REFTYM =',G12.4,';   OFFTYM =',G12.4)
 5 FORMAT(//T14,'TXON (ms)    Transmitter current (amps)' &
           /T14,'---------    --------------------------'/)
 6 FORMAT(//T10,'Receiver channel origin INPUT is shifted by', F9.3,' ms from signal origin.' &
          //T10,'Receiver Window Specifications (ms - referenced to signal origin)'/ &
            T10,'----------------------------------------------------------------'// &
            T62,'Referenced'/ &
            T8,'Window',T19,'Open',T30,'Close',T42,'Width',T52,'Centre',T64,'Centre'/ &
            T8,'------',T19,'----',T30,'-----',T42,'-----',T52,'------',T64,'------')
 7 FORMAT(/T3,'KHSQ =',I2,4X,'IPPD =',I2,4X,'MIN_FREQ =',G13.4,4X,'MAX_FREQ =',G13.4)
 8 FORMAT(/10X,'+----------------------------------------------+' &
          /10X,'+  Frequency-Domain Ground System Information  +' &
          /10X,'+----------------------------------------------+' &
          //T3,'NFRQ =',I3)
 9 FORMAT(/T12,'Frequency      Transmitter current in amps', &
          /T12,'---------      ---------------------------'/)
 10 FORMAT(//T3,'SURVEY_TYPE =',I2)
 11 FORMAT(/T3,'NLINES =',I3,';   MRXL =',I4,';   NTX =',I3,';   SOURCE_TYPE =',I2,';   MXVRTX =',I2,';   NTRN =',I3)

 12 FORMAT(/T3,'Vertex Locations for Loop Sources' &
           /T3,'---------------------------------')
 13 FORMAT(/T3,'Vertex Locations for Grounded Wire Sources' &
           /T3,'------------------------------------------')
 14 FORMAT(/T3,'Transmitter',I3,' has',I3,' vertices:' &
          //T13,'Easting      Northing      Elevation' &
           /T13,'-------      --------      ---------')
 15 FORMAT(/T3,'Magnetic Dipole Source Specification' &
          //T12,'Easting       Northing      Elevation   TXCLN   Azimuth' &
           /T12,'-------       --------      ---------   -----   -------')
 16 FORMAT(/T3,'WARNING:  Instead of using the default frequency range .001 Hz to 100 MHz' &
           /T3,'          to compute the HALFSPACE time-domain response, Loki will now use' &
           /T3,'          the user-specified range.')
 17 FORMAT(//T3,'Line',I9,';   Tx Index',I3,';   Rx Type =',I2,';   NRX =',I3,';   Units =',I3)
 18 FORMAT(/T11,'Magnetic Dipole Receivers' &
           /T11,'-------------------------' &
          //T10,'Easting     Northing   Elevation' &
           /T10,'-------     --------   ---------')
 19 FORMAT(/T11,'Magnetic Dipole Receivers' &
           /T11,'-------------------------' &
          //T10,'Easting     Northing   Elevation    Dip    Azimuth' &
           /T10,'-------     --------   ---------    ---    -------')
 20 FORMAT(/T12,'Electric Dipole Receiver Electrodes' &
           /T12,'-----------------------------------' &
          //T12,'East 1      North 1       East 2      North 2       Depth' &
           /T12,'------      -------       ------      -------       -----')
 21 FORMAT(/T3,'NLINES =',I3,';   NTX =',I3,';   SOURCE_TYPE =',I2,';   MXVRTX =',I2,';   TXMMT =',G12.4)
 22 FORMAT(/T9,'CMP =',I4,';   KNORM =',I2,';   IPLT =',I2,';   IDH =',I2,';   SV_AZM =',F7.1,';   RXMNT =',G12.4)
 23 FORMAT( T3,'NTXL =',I3,';   MRXL =',I3,';   MRXTX =',I2,';   NTRN =',I3)
 24 FORMAT( T3,'NTXL =',I3,';   MRXL =',I3,';   MRXTX =',I2,';   TXMMT =',G12.4)
 25 FORMAT(/T3,'NLINES =',I3,';   MRXL =',I3,';   IDH =',I2,';   TXMMT =',G12.4)
 26 FORMAT(/T3,'NLINES =',I3,';   MRXL =',I3,';   NTRN =',I3)
 27 FORMAT(/T3,'Moving Loop Tx Line',I3,';   NTX =',I3,';   Tx_length =',F7.1,';   Tx_width =',F7.1, &
           /T3,'--------------------------------------------------------------------------' &
           /T3,'Initial position (E,N) :',2F11.1 /T13,'Line_azimuth :',F6.1/T8,'Station_intervals :',2048F8.1)
 28 FORMAT(/T3,'Moving Magnetic Dipole Tx Line',I3,';   NTX =',I3,';   TXCLN =',F6.1,';   TXAZM =',F6.1,&
          //T3,'Initial position (E,N,Z) :',3F11.2,/T13,'Line_azimuth :',F6.1/T8,'Station_intervals :',2048F8.1)
 29 FORMAT(/T3,'Coincident loop survey - Line',I9,';   UNITS =',I3)
 30 FORMAT(/T3,'X,Y,Z Rx offsets for Line',I9,':',3F7.1 &
           /T5,'CMP =',I4,':  UNITS =',I3,';   KNORM =',I3,';   IPLT =',I2,';   RXMNT =',G12.4)
 31 FORMAT(/T3,'Line',I9,';   SV_AZM =',F6.1,';   NRX =',I3,';   CMP =',I4,';   OFFSET =',F6.1, &
           /T5,'UNITS =',I4,';   KNORM =',I3,';   IPLT =',I2,';   IDH =',I2,';   RXMNT =',G12.4)
 32 FORMAT(/T3,'Magnetic Dipole Transmitter Specification' &
          //T12,'Easting       Northing      Elevation     Dip   Azimuth' &
           /T12,'-------       --------      ---------     ---   -------')
 33 FORMAT(/T3,'NRX(',I2,') has been reduced to',I3)
 34 FORMAT(/T3,'KTXP for Transmitter Line',I3,' has been reduced to',I3)
 35 FORMAT(/T11,'Point E-field Receivers  CMP =',I4,';   SVAZM =',F7.1 &
           /T11,'-------------------------------------------------' &
          //T11,'Easting     Northing      Elevation' &
           /T11,'-------     --------      ---------')
 39 FORMAT(/T12,'Frequency' /T12,'---------'/)
 40 FORMAT(/T3,'UTEM base frequency =',F7.1,' Hz.   NCHNL = ',I3)
 50 FORMAT(/T3,'Magnetotelluric Survey:   NHRX =',I4,',  NERX =',I4,',  IPM =',I2,4X,'CMPMT(1:4) =',4I2)
 51 FORMAT(/T3,'Magnetotelluric Survey:   NHRX =',I4,',  NERX =',I4,',  IPM =',I2)
 53 FORMAT(//T4,'Magnetic Field Receivers'/T4,'------------------------' &
           //T3,'H-Field'                                                &
            /T3,'receiver       East          North'                     &
            /T3,'--------       ----          -----'/)
 54 FORMAT(//T4,'Electric Field Receivers'/T4,'------------------------' &
           //T3,'H-Field'                                                &
            /T4,'Index         East          North'                      &
            /T4,'-----         ----          -----'/)
 55 FORMAT(I6,T13,F11.1,T28,F11.1)
 90 FORMAT(T3,'JL =',I3,' ;  NRX(JL) =',I3,' ;  MRXL =',I3)
 91 FORMAT(//T3,"The MT option is inoperable until the incomplete Green's tensor operator is replaced.")
 92 FORMAT(//T3,"Electric dipole receivers option is inoperable until", &
            /T3,"the incomplete Green's tensor operator is replaced.")
    END SUBROUTINE READ_SYSTEM_AND_SURVEY_DATA

   SUBROUTINE READ_MODEL_DATA

   !-------------------------------------------------------------------------

   !***  Called by MAIN
   !***  Calls SET_CELL

   IMPLICIT NONE
   INTEGER, PARAMETER :: QL = SELECTED_REAL_KIND(12,76)
   INTEGER TOT_NODES,KN,KE,KZ,J1,I,J,K,NEZ,NN1,NN2,NN3,NBN,IX,IY,IZ,DSE, &
           DEE,DSN,DEN,DSZ,DEZ,ILYTH,NNE
   REAL, ALLOCATABLE :: STT(:,:,:)
   REAL ZLOCTMP
   REAL(KIND=QL) ZAIR(50)
   DATA (ZAIR(I),I=1,7)/2000.D0,1000.D0,200.D0,100.D0,50.D0,30.D0,10.D0/

   NEL = 1 ; NNL = 1 ; NER = 1 ; NNR = 1 ; NAIR = 7 ; NZB = 1

   WRITE(NW,1)
   READ(NR,*) NNORTH,NSZ,NEAST,NLITH,KACC,SOLVER,OUTPUT

   IF (SOLVER /= 2) THEN
     SOLVER = 2
     CALL WRITE_LOG_FILE (NLG,17,MXERR,1)
   END IF
   IF (OUTPUT < 0) THEN
     OUTPUT = -10
   ELSE
     OUTPUT = 10
   END IF
   IF (KACC /= 1 .AND. KACC /= 2) KACC = 2

   READ_RES_FILE = .FALSE.
   IF (NLITH < 0) THEN
     READ_RES_FILE = .TRUE.
     NLITH = ABS(NLITH)
   END IF

   WRITE(NW,2) NNORTH,NSZ,NEAST,NLITH,KACC,SOLVER,OUTPUT

!   ALLOCATE (LYTH(NLITH+1,NPROP),LITHL(NLYR),RES(NLYR),RMU(NLYR),RMUD(0:NLYR),REPS(NLYR),SIG0(NLYR),CHRG(NLYR), &
!           CTAU(NLYR),CFREQ(NLYR),THK(NPAR),THKD(NPAR),MPAR(NPAR))
   ALLOCATE (LYTH(NLITH+1,NPROP))

   ! Initialise lithology list.

   LYTH(1:NLITH+1, 1) = 0.    !  domain resistivity indicator
   LYTH(1:NLITH+1, 2) = -1.   !  conductance (SIG_T) indicator (not needed for Loki)
   LYTH(1:NLITH+1, 3) = 1.    !  Relative magnetic permeabilities
   LYTH(1:NLITH+1, 4) = 1.    !  Relative dielectric constants
   LYTH(1:NLITH+1, 5) = 0.    !  Chargeabilities
   LYTH(1:NLITH+1, 6) = 0.    !  CTAUs
   LYTH(1:NLITH+1, 7) = 1.    !  CFREQs

   WRITE(NW,3)
!   WRITE(np,9) NLITH
   DO J = 1, NLITH
     READ (NR,*) LYTH(J,1:NPROP)
     WRITE(NW,'(I4,T8,G12.4,T22,F7.1,F12.3,F11.3,F10.2,G12.3,F8.2)') J,LYTH(J,1:NPROP)
!     WRITE(np,10) J,LYTH(J,1:NPROP)
     IF (LYTH(J,3) < 0.01) LYTH(J,3) = 1.   ! Default RMU
     IF (LYTH(J,4) < 0.01) LYTH(J,4) = 1.   ! Default REPS
     IF (LYTH(J,5) < 1.E-3 .OR. LYTH(J,6) < 1.E-15 .OR. LYTH(J,7) < 1.E-6)  THEN
       LYTH(J,5) = 0   ! default CHRG
       LYTH(J,6) = 0   ! default CTAU
       LYTH(J,7) = 1   ! default CFRQ
     END IF
   END DO

   LYTH(NLITH+1, 1) = 1.E10 !  air resistivity indicator
   LYTH(NLITH+1, 2) = -1.   !  air conductance (SIG_T) indicator
   LYTH(NLITH+1, 3) = 1.    !  air Relative magnetic permeabilities
   LYTH(NLITH+1, 4) = 1.    !  air Relative dielectric constants
   LYTH(NLITH+1, 5) = 0.    !  air Chargeabilities
   LYTH(NLITH+1, 6) = 0.    !  air CTAUs
   LYTH(NLITH+1, 7) = 1.    !  air CFREQs

   TOT_NODES = NEAST * NNORTH * NSZ
   IF (TOT_NODES > 60000)   CALL WRITE_LOG_FILE (NLG,30,MXERR,1)

   NE = NEAST + NEL + NER
   NN = NNORTH + NNL + NNR
   NZ = NSZ + NZB + NAIR

   NPART = (NN-1)*(NE-1)*(NZ-1)      ! Total number of elements.
!   NPAR  = (NN-5)*(NE-5)*(NZ-NAIR-3) ! Number of elements in user mesh area.
   NPAR  = (NN-3)*(NE-3)*(NZ-NAIR-2) ! Number of elements in user mesh area.

   ALLOCATE (MPAR(NPAR))

   !#############################################################
   !                                                            !
   !  CRUCIAL NOTE                                              !
   !  ------------                                              !
   !                                                            !
   !  The user interface will read in location indices I,K,J    !
   !  ordered as NORTH, VERTICAL, EAST                          !
   !                                                            !
   !  SamAir will refer to these as KN, KZ, KE, conforming      !
   !  to the right hand assignment of                           !
   !                                                            !
   !  X = North;   Y = East;    Z = down                        !
   !                                                            !
   !  Nodes locations are indexed in the right hand system; eg, !
   !  the triplet KN,KE,KZ                                      !
   !                                                            !
   !#############################################################

   ALLOCATE (ELOCD(NE,NN,NZ),NLOCD(NE,NN,NZ),ZLOCD(NE,NN,NZ), &
             LITH(NE,NN,NZ),NSURFD(NNORTH,NEAST),ESURFD(NNORTH,NEAST), &
             ZSURFD(NNORTH,NEAST))

   ELOCD = 0.D0 ; NLOCD = 0.D0 ; ZLOCD = 0.D0 ; LITH = 0
   NSURFD = 0.D0 ; ESURFD = 0.D0 ; ZSURFD = 0.D0

!   WRITE (np,6) TOT_NODES,NNORTH,NEAST,NSZ

   IF (DO3D == 2) RETURN

   DO J1 =  1, TOT_NODES

      READ(NR,*) KN,KZ,KE,NLOCD(NEL+KE,NNL+KN,NAIR+KZ),ZLOCTMP,  &
                          ELOCD(NEL+KE,NNL+KN,NAIR+KZ),ILYTH
      ZLOCD(NEL+KE,NNL+KN,NAIR+KZ) = ZLOCTMP
      LITH(NEL+KE,NNL+KN,NAIR+KZ) = ILYTH

!      WRITE(np,7) KN,KZ,KE,NLOCD(NEL+KE,NNL+KN,NAIR+KZ), &
!                   -ZLOCTMP,ELOCD(NEL+KE,NNL+KN,NAIR+KZ),ILYTH

      IF (KZ == 1) THEN   ! Set up surface node array
        NSURFD(KN,KE) = NLOCD(NEL+KE,NNL+KN,NAIR+1)
        ESURFD(KN,KE) = ELOCD(NEL+KE,NNL+KN,NAIR+1)
        ZSURFD(KN,KE) = ZLOCD(NEL+KE,NNL+KN,NAIR+1)
      END IF
   END DO

   ! Extend the mesh and the resistivities to the left, right, top and bottom

   DSE = NEL  + 1 ; DEE = NE - NER
   DSN = NNL  + 1 ; DEN = NN - NNR
   DSZ = NAIR + 1 ; DEZ = NZ - NZB

   ELOCD(DSE-1,1:NN,1:NZ) = ELOCD(DSE,1:NN,1:NZ) - 1000.D0
   ELOCD(DEE+1,1:NN,1:NZ) = ELOCD(DEE,1:NN,1:NZ) + 1000.D0
   NLOCD(DSE-1,1:NN,1:NZ) = NLOCD(DSE,1:NN,1:NZ)
   NLOCD(DEE+1,1:NN,1:NZ) = NLOCD(DEE,1:NN,1:NZ)
   ZLOCD(DSE-1,1:NN,1:NZ) = ZLOCD(DSE,1:NN,1:NZ)
   ZLOCD(DEE+1,1:NN,1:NZ) = ZLOCD(DEE,1:NN,1:NZ)

   ELOCD(1:NE,DSN-1,1:NZ) = ELOCD(1:NE,DSN,1:NZ)
   ELOCD(1:NE,DEN+1,1:NZ) = ELOCD(1:NE,DEN,1:NZ)
   NLOCD(1:NE,DSN-1,1:NZ) = NLOCD(1:NE,DSN,1:NZ) - 1000.D0
   NLOCD(1:NE,DEN+1,1:NZ) = NLOCD(1:NE,DEN,1:NZ) + 1000.D0
   ZLOCD(1:NE,DSN-1,1:NZ) = ZLOCD(1:NE,DSN,1:NZ)
   ZLOCD(1:NE,DEN+1,1:NZ) = ZLOCD(1:NE,DEN,1:NZ)

   ELOCD(1:NE,1:NN,DEZ+1) = ELOCD(1:NE,1:NN,DEZ)
   NLOCD(1:NE,1:NN,DEZ+1) = NLOCD(1:NE,1:NN,DEZ)
   ZLOCD(1:NE,1:NN,DEZ+1) = ZLOCD(1:NE,1:NN,DEZ) - 1000.D0

   DO K = 1, NAIR
     ELOCD(1:NE,1:NN,K) = ELOCD(1:NE,1:NN,DSZ)
     NLOCD(1:NE,1:NN,K) = NLOCD(1:NE,1:NN,DSZ)
     ZLOCD(1:NE,1:NN,K) = ZLOCD(1:NE,1:NN,DSZ) + ZAIR(K)
   END DO

   NCNTRD = 0.5D0 * (MAXVAL (NLOCD) + MINVAL (NLOCD) )      !  Working origin
   ECNTRD = 0.5D0 * (MAXVAL (ELOCD) + MINVAL (ELOCD) )

   IF (ABS(ECNTRD) < 2000.D0) ECNTRD = 0.D0
   IF (ABS(NCNTRD) < 2000.D0) NCNTRD = 0.D0

   ! Convert arrays into computation coordinates (singular precision).

   ALLOCATE (ELOC(NE,NN,NZ),NLOC(NE,NN,NZ),ZLOC(NE,NN,NZ), &
             NSURF(NNORTH,NEAST),ESURF(NNORTH,NEAST),ZSURF(NNORTH,NEAST) )

   ELOC  =  REAL (ELOCD - ECNTRD)
   NLOC  =  REAL (NLOCD - NCNTRD)
   ZLOC  = -REAL (ZLOCD)
   ESURF =  REAL (ESURFD - ECNTRD)
   NSURF =  REAL (NSURFD - NCNTRD)
   ZSURF =  REAL (ZSURFD)

   DEALLOCATE (ELOCD,NLOCD,ZLOCD,NSURFD,ESURFD,ZSURFD )

   LITH(DSE-1,1:NN-1,1:NZ-1) = LITH(DSE  ,1:NN-1,1:NZ-1)
   LITH(DEE  ,1:NN-1,1:NZ-1) = LITH(DEE-1,1:NN-1,1:NZ-1)
   LITH(1:NE-1,DSN-1,1:NZ-1) = LITH(1:NE-1,DSN  ,1:NZ-1)
   LITH(1:NE-1,DEN  ,1:NZ-1) = LITH(1:NE-1,DEN-1,1:NZ-1)
   LITH(1:NE-1,1:NN-1,DEZ)   = LITH(1:NE-1,1:NN-1,DEZ-1)

   DO K = 1, NAIR
     LITH(1:NE-1,1:NN-1,K) = NLITH + 1
   END DO

   IF (.NOT. INVERT) THEN
     OPEN (NMP,FILE = 'Loki.map',STATUS = 'REPLACE')
     ALLOCATE (STT(NE,NN,NZ))
     STT = 0.
     DO J = 1, NN-1
       DO K = 1, NZ-1
         DO I = 1, NE-1
           STT(I,J,K) = LYTH(LITH(I,J,K),1)
           IF (STT(I,J,K) < 0.) THEN
             CALL WRITE_LOG_FILE (NLG,22,MXERR,2)
             WRITE(NLG,4) LITH(I,J,K)
           END IF
         END DO
       END DO
     END DO
     CALL RESMAP (NE,NN,NZ,NAIR,NEL,NER,NNL,NNR,NZB,STT,NLOC,ZLOC)
     DEALLOCATE (STT)
     CLOSE (NMP)
   END IF

   ! Numbering the edges of the elements.

   ALLOCATE (NEDGE(12,(NE-1)*(NN-1)*(NZ-1)))

   NESWAP = .FALSE.

   NNE = NE
   NEZ = NZ*(NNE-1) + NNE*(NZ-1)
   DO IY = 1, NN-1
     DO IZ = 1, NZ-1
       DO IX = 1, NE-1

         NBN = (IY-1)*(NE-1)*(NZ-1) + (IZ-1)*(NE-1) + IX

         NN1 = (NEZ + NNE*NZ) * (IY-1)
         NN2 = NN1 + ((NNE-1) + NNE)* (IZ-1)
         NN3 = NN1 + NEZ + (IZ-1)*NNE

         NEDGE(1, NBN) = NN2 + IX
         NEDGE(2, NBN) = NN2 + (NNE-1) + IX
         NEDGE(3, NBN) = NEDGE(2,NBN) + 1
         NEDGE(4, NBN) = NEDGE(2,NBN) + NNE
         NEDGE(5, NBN) = NN3 + IX
         NEDGE(6, NBN) = NEDGE(5,NBN) + 1
         NEDGE(7, NBN) = NEDGE(5,NBN) + NNE
         NEDGE(8, NBN) = NEDGE(7,NBN) + 1
         NEDGE(9, NBN) = NEDGE(1,NBN) + NEZ + NNE*NZ
         NEDGE(10,NBN) = NEDGE(2,NBN) + NEZ + NNE*NZ
         NEDGE(11,NBN) = NEDGE(3,NBN) + NEZ + NNE*NZ
         NEDGE(12,NBN) = NEDGE(4,NBN) + NEZ + NNE*NZ

       END DO
     END DO
   END DO

   ! Fill the vector of element resistivities.

   ALLOCATE (XPART(NPART)) ; XPART = 0.

   DO IY = 1, NN-1
     DO IZ = 1, NZ-1
       DO IX = 1, NE-1
         NBN = (IY-1)*(NE-1)*(NZ-1) + (IZ-1)*(NE-1) + IX
         XPART(NBN) = LYTH(LITH(IX,IY,IZ),1)
       END DO
     END DO
   END DO

   IF (READ_RES_FILE) THEN
     WRITE(*,8) ; WRITE(NW,8)
     OPEN(NM,FILE='LokiAir.res',STATUS='OLD')
     DO IY = NNL+1, NN-NNR-1
       DO IZ = NAIR+1, NZ-NZB-1
         DO IX = NEL+1, NE-NER-1
           READ(NM,*) S1
           NBN = (IY-1)*(NE-1)*(NZ-1) + (IZ-1)*(NE-1) + IX
           XPART(NBN) = S1
         END DO
       END DO
     END DO
     CLOSE(NM)
   END IF

    1 FORMAT(//T6,'MESH AND LITHOLOGIES DESCRIPTION' &
              /T6,'----------------------------------------')
    2 FORMAT(/T3,'Number of node "slices" along North-South axis =',I3 &
             /T3,'Number of node rows in each slice =',I4              &
             /T3,'Number of node columns in each slice =',I4,          &
             /T3,'NLITH =',I2,4X,'KACC =',I2,4X,'SOLVER =',I2,4X,'OUTPUT =',I4/)
    3 FORMAT(//T27,'LITHOLOGY PROPERTIES'/T27,'--------------------' &
             //T35,'Relative   Relative     Cole-Cole Parameters'    &
              /T9,'Resistivity  Conductance     MU     Dielectric   CHRG    CTAU       CFREQ'/)
    4 FORMAT(/T3,'Lithology',I3,' has a negative resistivity.')
!    6 FORMAT( T1,'/ TOTAL_NODES=',I5.5,' NN=',I3.3,' NE=',I3.3,' NZ=',I3.3, &
!             /T1,'/ JN       JZ       JE     NLOC     ZLOC     ELOC     LITH')
!    7 FORMAT(T1,'/ ',I2,T12,I2,T21,I2,3F9.0,T54,I5)
    8 FORMAT(//T3,'Element resistivities read from file, LokiAir.res.')
!    9 FORMAT( T1,'/ NLITH=',I2.2, &
!            /T1,'/ LITH    RES           Cond        MU         EPS       CHRG    CTAU        CFREQ')
!   10 FORMAT(T1,'/',I4,T8,G12.4,T22,F7.1,F12.3,F11.3,F10.2,G12.3,F8.2)

   END SUBROUTINE READ_MODEL_DATA

   SUBROUTINE READ_PARAMETER_CONTROL
!  ---------------------------------

!***  Called by: MAIN
!***      Calls: WRITE_LOG_FILE

! INTEGER NFIX,CTYPE,PLT_INDX,LYR_INDX,KPAR,J1
 INTEGER NFIX,JP,LITH_INDX,CTYPE,KPAR,J1
 REAL E1,E2,E3,A1,A2


! CHARACTER(LEN=12) PLATE_PRM(9),LYR_PRM(2)
! DATA LYR_PRM /  'Resistivity','Thickness'/
! DATA PLATE_PRM /'Conductance','Depth to top','Plate length','Dip width','CNTR_East', &
!                 'CNTR_North','Dip azimuth','Dip angle','Plunge'/

!  Set degree of constraint on each parameter
!  CXPAR = 0 => parameter is completely free to vary as dictated by inversion step
!        = 1 => parameter is fixed
!        = 2 => parameter is constrained by elasticity.
!        = 3 => parameter bounds are buffered.

 READ(NR,*) NFIX,MAXITS,CNVRG,INVPRT
 MV1PRT = INVPRT
 OUTPRT = INVPRT
 IF (INVPRT < 0 .OR. INVPRT > 3) INVPRT = 1
 IF (CNVRG < 1 .OR. CNVRG > 4) CALL WRITE_LOG_FILE (NLG,205,MXERR,2)
 WRITE(NW,1) NFIX,MAXITS,CNVRG,INVPRT

 PARPCT = 6.
 PCTCNV = 1.
 IF (CNVRG == 2) READ(NR,*) PCTCNV
 WRITE(NW,2) PCTCNV,MAXITS ; WRITE(*,2) PCTCNV,MAXITS
 IF (CNVRG == 4) THEN
   READ(NR,*) PARPCT
   WRITE(NW,3) PARPCT
   IF (PARPCT > 10. .OR. PARPCT < 3.) CALL WRITE_LOG_FILE (NLG,213,MXERR,1)
 END IF


!   ! Set the elasticity and bounds. For all lithologies, set the elasticity
!   ! equal to one. Assign a very small lower bound on the resistivity, and
!   ! a very large upper bound on the resitivity. These bounds are sufficiently
!   ! large that they are not going to be exceeded in any realistic inversion.

!   ALLOCATE (ELAS(NPAR),LBND(NPAR),UBND(NPAR),CXPAR(NPAR))
   ALLOCATE (ELAS(NLITH),LBND(NLITH),UBND(NLITH),CXPAR(NLITH))

   DO JP = 1, NLITH
     CXPAR(JP) = 0       ! All parameters are free to vary.
     ELAS(JP)  = 1.      ! Elasticity equal to one (full update step).
     LBND(JP)  = 1.E-8   ! Ridiculous lower bound on resistivity that won't be exceeded.
     UBND(JP)  = 1.E+8   ! Ridiculous upper bound on resistivity that won't be exceeded.
   END DO

 CXPAR = 0
 IF (NFIX > 0) THEN
   WRITE(NW,4)
   DO JP = 1, NFIX
     E1 =  0.                 ! E1 - Elasticity
     E2 =  1.E-8              ! E2 - Lower bound on resistivity.
     E3 =  1.E+8              ! E3 - Upper bound on resistivity.
     READ(NR,*) CTYPE
     BACKSPACE NR
     SELECT CASE (CTYPE)            ! J1 is a dummy variable
     CASE(1)
!       READ(NR,*) J1,PLT_INDX,KPAR
       READ(NR,*) J1,LITH_INDX,KPAR
       E1 = 0.
     CASE(2)
!       READ(NR,*) J1,PLT_INDX,KPAR,E1
       READ(NR,*) J1,LITH_INDX,KPAR,E1
     CASE(3)
!       READ(NR,*) J1,PLT_INDX,KPAR,E1,E2,E3
       READ(NR,*) J1,LITH_INDX,KPAR,E1,E2,E3
     END SELECT
     IF (KPAR /= 1) KPAR = 1       ! KPAR = 1 corresponds to resistivity. No other options

     E1 = ABS(E1)
     IF (E1 < 0.05) E1 = 0.      ! Hold for elasticities < 0.05
     IF (E1 > 0.95) E1 = 1.      ! Allow full freedom for elasticities > 0.95
     IF (E2 > E3) THEN           ! Switch if LB > UB
       A1 = E3
       E3 = E2
       E2 = A1
     END IF
     A1 = E3 - E2
     A2 = 0.005 * (ABS (E2) + ABS (E3))    ! If bound interval is infinitesimal,
     IF (A1  < A2) E1 = 0.                 ! set elastcicty to 0.

     WRITE(NW,8) LITH_INDX,KPAR,E1,E2,E3

     CXPAR(LITH_INDX) = J1
     ELAS(LITH_INDX)  = E1
     LBND(LITH_INDX)  = E2   ! Lower bound on resistivity.
     UBND(LITH_INDX)  = E3   ! Upper bound on resistivity.

!     ELAS(J1) = E1
!     LBND(J1) = E2
!     UBND(J1) = E3
   END DO
   WRITE(NW,9)
 ELSE
   WRITE(NW,10)
 END IF

  1 FORMAT(//T3,'----------------------------------------------' &
            /T3,'Inversion Controls for Parameters using Loki' &
            /T3,'----------------------------------------------' &
           //T3,'NFIX =',I3,3X,'MAXITS =',I3,3X,'CNVRG =',I2,3X,'INVPRT =',I2)
  2 FORMAT(/T3,'The inversion will finish if the RMS error is less than',F5.1,' percent or if' &
           /T3,'the error can no longer be reduced significantly or after',I3,' iterations.')
  3 FORMAT(T3,'The manual derivative step has been fixed at',F5.1,' percent.')
  4 FORMAT(//T12,'Constrained Parameters'/T12,'----------------------' &
           //T5,'Lithology'                                                        &
            /T5,' Index      Parameter      Elasticity  Lower Bound   Upper Bound' &
            /T5,'---------   ---------      ----------  -----------   -----------')
  8 FORMAT(T7,I2,T20,I1,T31,G12.4,T43,G12.4,T58,G12.4)
  9 FORMAT(/T3,90('-'))
 10 FORMAT(/T3,'All element resistivities will be allowed to vary during inversion.')

 END SUBROUTINE READ_PARAMETER_CONTROL

   SUBROUTINE READ_INVRT_DATA

   !------------------------------------------------------------------------
   !
   !***  Called by MAIN
   !
   !  Set inversion dimensions:
   !
   !  NCHNL = number of time domain channels
   !  MCHNL = total number of readings per station to be inverted(TD or FD)
   !
   !  RDATA & RWTS are data and weights in array form (MCHNL, NSTAT)
   !  XDATA & XWTS are data and weights in column form (MCHNL * NSTAT)
   !  RWTS  & XWTS are now restricted to integer values of 0 or 1 (reject or accep)
   !  Note that data are normalised so that balance weighting is unnecessary.
   !
   !  XMODL contains model results in column form (MCHNL * NSTAT)
   !  NRX = 1 for TD & NFRQ for FD
   !
   !  For TD, RDATA is ordered as vertical components for all delay times
   !  followed by all in-line components, followed by all transverse
   !  components for each station.
   !
   !  For FD data, in-phase data is followed by quadrature data
   !
   !  XDATA are the RDATA stacked into a column, station by station
   !  The same convention applies to XMODL, XWTS & RWTS
   !
   !------------------------------------------------------------------------

   INTEGER ORDER,FIQ,N2B,N2E,N3B,N3E,JP,MAXRX,JI,JQ,J1,J2,J3,JS,JD, &
           N0STAT,N0CHNL,N0PTS,DUNIT
   INTEGER,ALLOCATABLE,DIMENSION(:) :: K0STAT,K0CHNL
   INTEGER,ALLOCATABLE,DIMENSION(:,:) :: KSTAT
   REAL ALINE,BIG
   REAL, ALLOCATABLE,DIMENSION (:) :: QDATA,Q2DATA
   CHARACTER (LEN=1) TCHR
   CHARACTER (LEN=4) QUNIT(13)
   DATA QUNIT / '   V','  mV','  uV','  nV','nT/s','pT/s','  nT','pT', &
                '    ','   %',' ppt',' ppm',' ppb'/
   Logical :: IsComment

   WRITE(NW,1)

   ! Start reading from SamAir.inv on logical unit NRI = 13.

   DO
     READ (NRI,'(A)') TCHR
   IF (.not.(IsComment(tchr))) EXIT
   END DO
   BACKSPACE (NRI)

   ! RECORD 15
   ! ---------

   KCMP = 0 ; ORDER = 0 ; FIQ = 0

   IF (TDFD == 1) THEN
     READ(NRI,*)  KCMP, ORDER
     WRITE(NW,21) KCMP, ORDER
   ELSE IF (TDFD == 2) THEN
     READ(NRI,*)  KCMP, ORDER, FIQ
     WRITE(NW,20) KCMP, ORDER, FIQ
   END IF

   ! Check KCMP, ORDER and FIQ (if appropriate) is valid.

   IF (ORDER /= 1 .AND. ORDER /= 2) THEN
     CALL WRITE_LOG_FILE (NLG,102,MXERR,2)
     STOP
   END IF

   IF (TDFD == 1) THEN
     IF (KCMP /= 1 .AND. KCMP /=   2 .AND. KCMP /=   3 .AND. KCMP /= 13 &
                   .AND. KCMP /=  31 .AND. KCMP /=  23 .AND. KCMP /= 32 &
                   .AND. KCMP /= 123 .AND. KCMP /= 312) THEN
       CALL WRITE_LOG_FILE (NLG,103,MXERR,2)
       STOP
     END IF
   ELSE IF (TDFD == 2) THEN
     IF (KCMP /= 1 .AND. KCMP /=   2 .AND. KCMP /=   3 .AND. KCMP /= 13 &
                   .AND. KCMP /=  31 .AND. KCMP /=  23 .AND. KCMP /= 32 &
                   .AND. KCMP /= 123 .AND. KCMP /= 312 .AND. KCMP /= 41 &
                   .AND. KCMP /=  42 .AND. KCMP /=  43 .AND. KCMP /= 44) THEN
       CALL WRITE_LOG_FILE (NLG,103,MXERR,2)
       STOP
     END IF
     IF (FIQ /= 1 .AND. FIQ /= 2 .AND. FIQ /= 3 .AND. FIQ /= 4) THEN
       CALL WRITE_LOG_FILE (NLG,104,MXERR,2)
       STOP
     END IF
   END IF

   ! Write inversion data header and set MCHNL.

   SELECT CASE (TDFD)
   CASE(1)
     IF (KCMP == 1) THEN
       WRITE(NW,3)     ! X (or U) TD data inversion.
       MCHNL = NCHNL
     ELSE IF (KCMP == 2) THEN
       WRITE(NW,4)     ! Y (or V) TD data inversion.
       MCHNL = NCHNL
     ELSE IF (KCMP == 3) THEN
       WRITE(NW,2)     ! Z (or A) TD data inversion.
       MCHNL = NCHNL
     ELSE IF (KCMP == 13 .OR. KCMP == 31) THEN
       WRITE(NW,5)     ! Joint Z (or A) and X (or U) TD data inversion.
       MCHNL = 2*NCHNL
     ELSE IF (KCMP == 23 .OR. KCMP == 32) THEN
       WRITE(NW,6)     ! Joint Z (or A) and Y (or V) TD data inversion.
       MCHNL = 2*NCHNL
     ELSE IF (KCMP == 123 .OR. KCMP == 312) THEN
       WRITE(NW,7)     ! Joint three-component TD data inversion.
       MCHNL = 3*NCHNL
     END IF
   CASE(2)
     IF (KCMP == 1) THEN
       WRITE(NW,9)     ! X (or U) FD data inversion.
       MCHNL = 2*NFRQ
     ELSE IF (KCMP == 2) THEN
       WRITE(NW,10)    ! Y (or V) FD data inversion.
       MCHNL = 2*NFRQ
     ELSE IF (KCMP == 3) THEN
       WRITE(NW,8)     ! Z (or A) FD data inversion.
       MCHNL = 2*NFRQ
     ELSE IF (KCMP == 13 .OR. KCMP == 31 ) THEN
       WRITE(NW,11)    ! Joint Z (or A) and X (or U) FD data inversion.
       MCHNL = 4*NFRQ
     ELSE IF (KCMP == 23 .OR. KCMP == 32) THEN
       WRITE(NW,12)    ! Joint Z (or A) and Y (or V) FD data inversion.
       MCHNL = 4*NFRQ
     ELSE IF (KCMP == 123 .OR. KCMP == 312) THEN
       WRITE(NW,13)    ! Joint three-component FD data inversion.
       MCHNL = 6*NFRQ
     ELSE IF (KCMP == 41) THEN
       WRITE(NW,14)    ! Zxy data inversion.
       MCHNL = 2*NFRQ
     ELSE IF (KCMP == 42) THEN
       WRITE(NW,15)    ! Zyx data inversion.
       MCHNL = 2*NFRQ
     ELSE IF (KCMP == 43) THEN
       WRITE(NW,16)    ! Joint Zxy and Zyx data inversion.
       MCHNL = 4*NFRQ
     ELSE IF (KCMP == 44) THEN
       WRITE(NW,17)    ! Joint full tensor data inversion.
       MCHNL = 8*NFRQ
     END IF
   END SELECT

   ! RECORD 16
   ! ---------

   IF (UNITS(1)  == 1) DUNIT = 1
   IF (UNITS(1)  == 2) DUNIT = 2
   IF (UNITS(1)  == 3) DUNIT = 3
   IF (UNITS(1)  == 4) DUNIT = 4
   IF (UNITS(1) == 11) DUNIT = 5
   IF (UNITS(1) == 12) DUNIT = 6
   IF (UNITS(1) == 21) DUNIT = 7
   IF (UNITS(1) == 22) DUNIT = 8
   IF (UNITS(1) == 31) DUNIT = 9
   IF (UNITS(1) == 32) DUNIT = 10
   IF (UNITS(1) == 33) DUNIT = 11
   IF (UNITS(1) == 34) DUNIT = 12
   IF (UNITS(1) == 35) DUNIT = 13

   ALLOCATE (DATA_FLOOR(MCHNL)) ; DATA_FLOOR = 0.

   IF (TDFD == 1) THEN
     READ(NRI,*) DATA_FLOOR(1)         ! One data floor is read for all channels and components.
     WRITE(NW,22) ABS (DATA_FLOOR(1)), TRIM(QUNIT(DUNIT))
     DATA_FLOOR = DATA_FLOOR(1)
   ELSE
     READ(NRI,*) DATA_FLOOR(1:2*NFRQ)  ! Data floors are read for each frequency.
     WRITE(NW,23) TRIM (QUNIT(DUNIT))
     DO JP = 1, NFRQ
       WRITE(NW,24) JP,FREQ(JP), ABS(DATA_FLOOR(JP)), ABS(DATA_FLOOR(JP+NFRQ))
     END DO
     IF (KCMP == 13 .OR. KCMP == 31 .OR. KCMP == 23 &      ! Fill in the MCHNL array of data floors.
                    .OR. KCMP == 32 .OR. KCMP == 43) THEN
       DATA_FLOOR(2*NFRQ+1:3*NFRQ) = DATA_FLOOR(NFRQ+1:2*NFRQ)
       DATA_FLOOR(3*NFRQ+1:4*NFRQ) = DATA_FLOOR(NFRQ+1:2*NFRQ)
       DATA_FLOOR(  NFRQ+1:2*NFRQ) = DATA_FLOOR(     1:  NFRQ)
     ELSE IF (KCMP == 123 .OR. KCMP == 312) THEN
       DATA_FLOOR(3*NFRQ+1:4*NFRQ) = DATA_FLOOR(NFRQ+1:2*NFRQ)
       DATA_FLOOR(4*NFRQ+1:5*NFRQ) = DATA_FLOOR(NFRQ+1:2*NFRQ)
       DATA_FLOOR(5*NFRQ+1:6*NFRQ) = DATA_FLOOR(NFRQ+1:2*NFRQ)
       DATA_FLOOR(  NFRQ+1:2*NFRQ) = DATA_FLOOR(     1:  NFRQ)
       DATA_FLOOR(2*NFRQ+1:3*NFRQ) = DATA_FLOOR(     1:  NFRQ)
     ELSE IF (KCMP == 44) THEN
       DATA_FLOOR(4*NFRQ+1:5*NFRQ) = DATA_FLOOR(NFRQ+1:2*NFRQ)
       DATA_FLOOR(5*NFRQ+1:6*NFRQ) = DATA_FLOOR(NFRQ+1:2*NFRQ)
       DATA_FLOOR(6*NFRQ+1:7*NFRQ) = DATA_FLOOR(NFRQ+1:2*NFRQ)
       DATA_FLOOR(7*NFRQ+1:8*NFRQ) = DATA_FLOOR(NFRQ+1:2*NFRQ)
       DATA_FLOOR(  NFRQ+1:2*NFRQ) = DATA_FLOOR(     1:  NFRQ)
       DATA_FLOOR(2*NFRQ+1:3*NFRQ) = DATA_FLOOR(     1:  NFRQ)
       DATA_FLOOR(3*NFRQ+1:4*NFRQ) = DATA_FLOOR(     1:  NFRQ)
     END IF
   END IF

   ! RECORD 17
   ! ---------

   MAXRX = MAXVAL(NRX)

   ALLOCATE (RDATA1(NLINES,MAXRX,MCHNL),RWTS1(NLINES,MAXRX,MCHNL),KSTAT(NLINES,MAXRX))

   RDATA1 = 0. ; RWTS1 = 1 ; KSTAT = 0

   READ(NRI,*)  N0STAT,N0CHNL,N0PTS
   WRITE(NW,25) N0STAT,N0CHNL,N0PTS

   IF (N0STAT > 0) THEN                         ! Stations weights.
     ALLOCATE (K0STAT(N0STAT)) ; K0STAT = 0
     READ(NRI,*)  K0STAT(1:N0STAT)
     WRITE(NW,26) K0STAT(1:N0STAT)
     DO J1 = 1, N0STAT
       J2 = K0STAT(J1)
       J3 = 0
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           J3 = J3 + 1
           IF (J3 == J2) THEN
             RWTS1(K,J,1:MCHNL) = 0
           END IF
         END DO
       END DO
     END DO
     DEALLOCATE (K0STAT)
   END IF

   IF (N0CHNL > 0) THEN                         ! Channel weights.
     ALLOCATE (K0CHNL(N0CHNL)) ; K0CHNL = 0
     READ(NRI,*)  K0CHNL(1:N0CHNL)
     WRITE(NW,27) K0CHNL(1:N0CHNL)
     DO J1 = 1, N0CHNL
       RWTS1(1:NLINES,1:MAXRX,K0CHNL(J1)) = 0
     END DO
     DEALLOCATE (K0CHNL)
   END IF

   IF (N0PTS > 0) THEN                          ! Point weights.
     ALLOCATE (K0STAT(N0PTS),K0CHNL(N0PTS)) ; K0STAT = 0 ; K0CHNL = 0
     READ(NRI,*) (K0CHNL(J1),K0STAT(J1),J1=1,N0PTS)
     WRITE(NW,28)
     DO J1 = 1, N0PTS
       WRITE(NW,'(T3,2I4)') K0CHNL(J1),K0STAT(J1)
       J2 = 0
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           J2 = J2 + 1
           IF (J2 == K0STAT(J1)) THEN
             RWTS1(K,J,K0CHNL(J1)) = 0
           END IF
         END DO
       END DO
     END DO
     DEALLOCATE (K0STAT,K0CHNL)
   END IF

   !======================!
   !      DATA ENTRY      !
   !======================!

   ALLOCATE(QDATA(MCHNL),Q2DATA(MCHNL)) ; QDATA = 0. ; Q2DATA = 0.

   DO K = 1, NLINES
     DO J = 1, NRX(K)

       READ(NRI,*) ALINE,KSTAT(K,J),QDATA(1:MCHNL)
       LINE(K) = FLOOR(ALINE)

       ! Put data in the order of all Z followed by all X followed by all Y,
       ! depending upon which components are present.

       IF (TDFD < 2) THEN ! Time-domain data.

         N2B = NCHNL+1 ; N2E = 2*NCHNL
         N3B = N2E  +1 ; N3E = 3*NCHNL

         IF (KCMP == 1 .OR. KCMP == 2 .OR. KCMP == 3) Q2DATA(1:NCHNL) = QDATA(1:NCHNL)

         SELECT CASE (ORDER)
         CASE (1)
           IF (KCMP == 31 .OR. KCMP == 32) Q2DATA(1:N2E) = QDATA(1:N2E)
           IF (KCMP == 312)                Q2DATA(1:N3E) = QDATA(1:N3E)
           IF (KCMP == 13 .OR. KCMP == 23) THEN
             Q2DATA(1:NCHNL) = QDATA(N2B:N2E)
             Q2DATA(N2B:N2E) = QDATA(1:NCHNL)
           ELSE IF (KCMP == 123) THEN
             Q2DATA(1:NCHNL) = QDATA(N3B:N3E)
             Q2DATA(N2B:N2E) = QDATA(1:NCHNL)
             Q2DATA(N3B:N3E) = QDATA(N2B:N2E)
           END IF
         CASE (2)
           DO JT = 1, NCHNL
             IF (KCMP == 31 .OR. KCMP == 32) THEN
               Q2DATA(JT)       = QDATA(2*JT-1)
               Q2DATA(JT+NCHNL) = QDATA(2*JT)
             ELSE IF (KCMP == 13 .OR. KCMP == 23) THEN
               Q2DATA(JT)       = QDATA(2*JT)
               Q2DATA(JT+NCHNL) = QDATA(2*JT-1)
             ELSE IF (KCMP == 123) THEN
               Q2DATA(JT)         = QDATA (3*JT)
               Q2DATA(JT+NCHNL)   = QDATA (3*JT-2)
               Q2DATA(JT+2*NCHNL) = QDATA (3*JT-1)
             ELSE IF (KCMP == 312) THEN
               Q2DATA(JT)         = QDATA (3*JT-2)
               Q2DATA(JT+NCHNL)   = QDATA (3*JT-1)
               Q2DATA(JT+2*NCHNL) = QDATA (3*JT)
             END IF
           END DO
         END SELECT

       ELSE IF (TDFD == 2) THEN

         ! Store as in-phase then quadrature for all frequencies; in Z then X then Y (as present).

         ! Single component, put in IIQQ format.

         IF (KCMP == 1 .OR. KCMP == 2 .OR. KCMP == 3) THEN
           SELECT CASE(FIQ)
           CASE(1) ! IQIQ
             DO JF = 1,NFRQ
               Q2DATA(JF     ) = QDATA(2*JF-1)
               Q2DATA(JF+NFRQ) = QDATA(2*JF  )
             END DO
           CASE(2) ! QIQI
             DO JF = 1, NFRQ
               Q2DATA(JF     ) = QDATA(2*JF  )
               Q2DATA(JF+NFRQ) = QDATA(2*JF-1)
             END DO
           CASE(3) ! IIQQ
             Q2DATA(1:2*NFRQ) = QDATA(1:2*NFRQ)
           CASE(4) ! QQII
             Q2DATA(     1:  NFRQ) = QDATA(NFRQ+1:2*NFRQ)
             Q2DATA(NFRQ+1:2*NFRQ) = QDATA(     1:  NFRQ)
           END SELECT
         END IF

         SELECT CASE (ORDER)
         CASE (1) ! X1, X2, Y1, Y2, Z1, Z2
           IF (KCMP == 31 .OR. KCMP == 32) THEN  ! Two components, put in IIQQ, KCMP = 31/32 order.
             SELECT CASE (FIQ)
             CASE(1) ! IQIQ
               DO JF = 1, NFRQ
                 Q2DATA(JF       ) = QDATA(JF       )
                 Q2DATA(JF+  NFRQ) = QDATA(JF+2*NFRQ)
                 Q2DATA(JF+2*NFRQ) = QDATA(JF+  NFRQ)
                 Q2DATA(JF+3*NFRQ) = QDATA(JF+3*NFRQ)
               END DO
             CASE(2) ! QIQI
               DO JF = 1, NFRQ
                 Q2DATA(JF       ) = QDATA(JF+  NFRQ)
                 Q2DATA(JF+NFRQ  ) = QDATA(JF+3*NFRQ)
                 Q2DATA(JF+2*NFRQ) = QDATA(JF       )
                 Q2DATA(JF+3*NFRQ) = QDATA(JF+2*NFRQ)
               END DO
             CASE(3) ! IIQQ
               Q2DATA(1:4*NFRQ) = QDATA(1:4*NFRQ)
             CASE(4) ! QQII
               Q2DATA(       1:2*NFRQ) = QDATA(2*NFRQ+1:4*NFRQ)
               Q2DATA(2*NFRQ+1:4*NFRQ) = QDATA(       1:2*NFRQ)
             CASE DEFAULT
               CALL WRITE_LOG_FILE (NLG,214,MXERR,2)
             END SELECT
           ELSE IF (KCMP == 13 .OR. KCMP == 23) THEN
             SELECT CASE (FIQ)
             CASE(1) ! IQIQ
               DO JF = 1, NFRQ
                 Q2DATA(JF       ) = QDATA(JF+2*NFRQ)
                 Q2DATA(JF+  NFRQ) = QDATA(JF       )
                 Q2DATA(JF+2*NFRQ) = QDATA(JF+3*NFRQ)
                 Q2DATA(JF+3*NFRQ) = QDATA(JF+  NFRQ)
               END DO
             CASE(2) ! QIQI
               DO JF = 1, NFRQ
                 Q2DATA(JF       ) = QDATA(JF+3*NFRQ)
                 Q2DATA(JF+  NFRQ) = QDATA(JF+  NFRQ)
                 Q2DATA(JF+2*NFRQ) = QDATA(JF+2*NFRQ)
                 Q2DATA(JF+3*NFRQ) = QDATA(JF       )
               END DO
             CASE(3) ! IIQQ
               Q2DATA(       1:1*NFRQ) = QDATA(  NFRQ+1:2*NFRQ)
               Q2DATA(  NFRQ+1:2*NFRQ) = QDATA(       1:1*NFRQ)
               Q2DATA(2*NFRQ+1:3*NFRQ) = QDATA(3*NFRQ+1:4*NFRQ)
               Q2DATA(3*NFRQ+1:4*NFRQ) = QDATA(2*NFRQ+1:3*NFRQ)
             CASE(4) ! QQII
               Q2DATA(       1:  NFRQ) = QDATA(3*NFRQ+1:4*NFRQ)
               Q2DATA(  NFRQ+1:2*NFRQ) = QDATA(2*NFRQ+1:3*NFRQ)
               Q2DATA(2*NFRQ+1:3*NFRQ) = QDATA(  NFRQ+1:2*NFRQ)
               Q2DATA(3*NFRQ+1:4*NFRQ) = QDATA(       1:  NFRQ)
             CASE DEFAULT
               CALL WRITE_LOG_FILE (NLG,214,MXERR,2)
             END SELECT
           ELSE IF (KCMP == 312) THEN  ! Three components, put in IIQQ, KCMP = 312 order.
             SELECT CASE (FIQ)
             CASE(1) ! IQIQ
               DO JF = 1, NFRQ
                 Q2DATA(JF       ) = QDATA(JF       )
                 Q2DATA(JF+  NFRQ) = QDATA(JF+2*NFRQ)
                 Q2DATA(JF+2*NFRQ) = QDATA(JF+4*NFRQ)
                 Q2DATA(JF+3*NFRQ) = QDATA(JF+  NFRQ)
                 Q2DATA(JF+4*NFRQ) = QDATA(JF+3*NFRQ)
                 Q2DATA(JF+5*NFRQ) = QDATA(JF+5*NFRQ)
               END DO
             CASE(2) ! QIQI
               DO JF = 1, NFRQ
                 Q2DATA(JF       ) = QDATA(JF+  NFRQ)
                 Q2DATA(JF+  NFRQ) = QDATA(JF+3*NFRQ)
                 Q2DATA(JF+2*NFRQ) = QDATA(JF+5*NFRQ)
                 Q2DATA(JF+3*NFRQ) = QDATA(JF       )
                 Q2DATA(JF+4*NFRQ) = QDATA(JF+2*NFRQ)
                 Q2DATA(JF+4*NFRQ) = QDATA(JF+4*NFRQ)
               END DO
             CASE(3) ! IIQQ
               Q2DATA(1:6*NFRQ) = QDATA(1:6*NFRQ)
             CASE(4) ! QQII
               Q2DATA(       1:3*NFRQ) = QDATA(3*NFRQ+1:6*NFRQ)
               Q2DATA(3*NFRQ+1:6*NFRQ) = QDATA(       1:3*NFRQ)
             CASE DEFAULT
               CALL WRITE_LOG_FILE (NLG,214,MXERR,2)
             END SELECT
           ELSE IF (KCMP == 123) THEN  ! Put in KCMP = 312
             SELECT CASE (FIQ)
             CASE(1) ! IQIQ
               DO JF = 1, NFRQ
                 Q2DATA(JF       ) = QDATA(JF+4*NFRQ)
                 Q2DATA(JF+  NFRQ) = QDATA(JF       )
                 Q2DATA(JF+2*NFRQ) = QDATA(JF+2*NFRQ)
                 Q2DATA(JF+3*NFRQ) = QDATA(JF+5*NFRQ)
                 Q2DATA(JF+4*NFRQ) = QDATA(JF+3*NFRQ)
                 Q2DATA(JF+5*NFRQ) = QDATA(JF+  NFRQ)
               END DO
             CASE(2) ! QIQI
               DO JF = 1, NFRQ
                 Q2DATA(JF       ) = QDATA(JF+5*NFRQ)
                 Q2DATA(JF+  NFRQ) = QDATA(JF+4*NFRQ)
                 Q2DATA(JF+2*NFRQ) = QDATA(JF+  NFRQ)
                 Q2DATA(JF+3*NFRQ) = QDATA(JF       )
                 Q2DATA(JF+4*NFRQ) = QDATA(JF+3*NFRQ)
                 Q2DATA(JF+5*NFRQ) = QDATA(JF+2*NFRQ)
               END DO
             CASE(3) ! IIQQ
               Q2DATA(       1:1*NFRQ) = QDATA(2*NFRQ+1:3*NFRQ)
               Q2DATA(  NFRQ+1:2*NFRQ) = QDATA(       1:  NFRQ)
               Q2DATA(2*NFRQ+1:3*NFRQ) = QDATA(  NFRQ+1:2*NFRQ)
               Q2DATA(3*NFRQ+1:4*NFRQ) = QDATA(5*NFRQ+1:6*NFRQ)
               Q2DATA(4*NFRQ+1:5*NFRQ) = QDATA(3*NFRQ+1:4*NFRQ)
               Q2DATA(5*NFRQ+1:6*NFRQ) = QDATA(4*NFRQ+1:5*NFRQ)
             CASE(4) ! QQII
               Q2DATA(       1:  NFRQ) = QDATA(5*NFRQ+1:6*NFRQ)
               Q2DATA(  NFRQ+1:2*NFRQ) = QDATA(3*NFRQ+1:4*NFRQ)
               Q2DATA(2*NFRQ+1:3*NFRQ) = QDATA(4*NFRQ+1:5*NFRQ)
               Q2DATA(3*NFRQ+1:4*NFRQ) = QDATA(2*NFRQ+1:3*NFRQ)
               Q2DATA(4*NFRQ+1:5*NFRQ) = QDATA(       1:  NFRQ)
               Q2DATA(5*NFRQ+1:6*NFRQ) = QDATA(  NFRQ+1:2*NFRQ)
             END SELECT
           END IF
         CASE(2) ! X1, Y1, Z1, X2, Y2, Z2 ...
           IF (KCMP == 31 .OR. KCMP == 32) THEN  ! Two components, put in IIQQ, KCMP = 31/32 order.
             SELECT CASE (FIQ)
             CASE(1) ! IQIQ
               DO JF = 1, NFRQ
                 Q2DATA(JF       ) = QDATA(1+4*NFRQ*(JF-1))
                 Q2DATA(JF+  NFRQ) = QDATA(3+4*NFRQ*(JF-1))
                 Q2DATA(JF+2*NFRQ) = QDATA(2+4*NFRQ*(JF-1))
                 Q2DATA(JF+3*NFRQ) = QDATA(4+4*NFRQ*(JF-1))
               END DO
             CASE(2) ! QIQI
               DO JF = 1, NFRQ
                 Q2DATA(JF       ) = QDATA(2+4*NFRQ*(JF-1))
                 Q2DATA(JF+  NFRQ) = QDATA(4+4*NFRQ*(JF-1))
                 Q2DATA(JF+2*NFRQ) = QDATA(1+4*NFRQ*(JF-1))
                 Q2DATA(JF+3*NFRQ) = QDATA(3+4*NFRQ*(JF-1))
               END DO
             END SELECT
           ELSE IF (KCMP == 13 .OR. KCMP == 23) THEN
             SELECT CASE (FIQ)
             CASE(1) ! IQIQ
               DO JF = 1, NFRQ
                 Q2DATA(JF       ) = QDATA(3+4*NFRQ*(JF-1))
                 Q2DATA(JF+  NFRQ) = QDATA(1+4*NFRQ*(JF-1))
                 Q2DATA(JF+2*NFRQ) = QDATA(4+4*NFRQ*(JF-1))
                 Q2DATA(JF+3*NFRQ) = QDATA(2+4*NFRQ*(JF-1))
               END DO
             CASE(2) ! QIQI
               DO JF = 1, NFRQ
                 Q2DATA(JF       ) = QDATA(4+4*NFRQ*(JF-1))
                 Q2DATA(JF+  NFRQ) = QDATA(2+4*NFRQ*(JF-1))
                 Q2DATA(JF+2*NFRQ) = QDATA(3+4*NFRQ*(JF-1))
                 Q2DATA(JF+3*NFRQ) = QDATA(1+4*NFRQ*(JF-1))
               END DO
             END SELECT
           ELSE IF (KCMP == 132) THEN            ! Three components, put in IIQQ, KCMP = 312 order.
             SELECT CASE (FIQ)
             CASE(1) ! IQIQIQ
               DO JF = 1, NFRQ
                 Q2DATA(JF       ) = QDATA(3+6*NFRQ*(JF-1))
                 Q2DATA(JF+  NFRQ) = QDATA(1+6*NFRQ*(JF-1))
                 Q2DATA(JF+2*NFRQ) = QDATA(5+6*NFRQ*(JF-1))
                 Q2DATA(JF+3*NFRQ) = QDATA(4+6*NFRQ*(JF-1))
                 Q2DATA(JF+4*NFRQ) = QDATA(2+6*NFRQ*(JF-1))
                 Q2DATA(JF+5*NFRQ) = QDATA(6+6*NFRQ*(JF-1))
               END DO
             CASE(2) ! QIQIQI
               DO JF = 1, NFRQ
                 Q2DATA(JF       ) = QDATA(4+6*NFRQ*(JF-1))
                 Q2DATA(JF+  NFRQ) = QDATA(2+6*NFRQ*(JF-1))
                 Q2DATA(JF+2*NFRQ) = QDATA(6+6*NFRQ*(JF-1))
                 Q2DATA(JF+3*NFRQ) = QDATA(3+6*NFRQ*(JF-1))
                 Q2DATA(JF+4*NFRQ) = QDATA(1+6*NFRQ*(JF-1))
                 Q2DATA(JF+5*NFRQ) = QDATA(5+6*NFRQ*(JF-1))
               END DO
             END SELECT
           ELSE IF (KCMP == 312) THEN
             SELECT CASE (FIQ)
             CASE(1) ! IQIQIQ
               DO JF = 1, NFRQ
                 Q2DATA(JF       ) = QDATA(1+6*NFRQ*(JF-1))
                 Q2DATA(JF+  NFRQ) = QDATA(3+6*NFRQ*(JF-1))
                 Q2DATA(JF+2*NFRQ) = QDATA(5+6*NFRQ*(JF-1))
                 Q2DATA(JF+3*NFRQ) = QDATA(2+6*NFRQ*(JF-1))
                 Q2DATA(JF+4*NFRQ) = QDATA(4+6*NFRQ*(JF-1))
                 Q2DATA(JF+5*NFRQ) = QDATA(6+6*NFRQ*(JF-1))
               END DO
             CASE(2) ! QIQIQI
               DO JF = 1, NFRQ
                 Q2DATA(JF       ) = QDATA(2+6*NFRQ*(JF-1))
                 Q2DATA(JF+  NFRQ) = QDATA(4+6*NFRQ*(JF-1))
                 Q2DATA(JF+2*NFRQ) = QDATA(6+6*NFRQ*(JF-1))
                 Q2DATA(JF+3*NFRQ) = QDATA(1+6*NFRQ*(JF-1))
                 Q2DATA(JF+4*NFRQ) = QDATA(3+6*NFRQ*(JF-1))
                 Q2DATA(JF+5*NFRQ) = QDATA(5+6*NFRQ*(JF-1))
               END DO
             END SELECT
           END IF
         END SELECT
       END IF

       RDATA1(K,J,1:MCHNL) = Q2DATA(1:MCHNL)

     END DO
   END DO

   DEALLOCATE (QDATA,Q2DATA)

   ! Write the data and weights in blocked format to make checking easier.

   IF (TDFD < 2) THEN

     ! Write time domain data.
     ! -----------------------

     IF (KCMP == 1 .OR. KCMP == 2 .OR. KCMP == 3) THEN
       IF (KCMP == 1) WRITE(NW,51)  ! Write Z (or A) data.
       IF (KCMP == 2) WRITE(NW,52)  ! Write X (or U) data.
       IF (KCMP == 3) WRITE(NW,53)  ! Write Y (or V) data.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           WRITE(NW,40) LINE(K),KSTAT(K,J),RDATA1(K,J,1:NCHNL)
         END DO
       END DO
     END IF

     IF (KCMP == 13 .OR. KCMP == 31 .OR. KCMP == 23 .OR. KCMP == 32) THEN
       WRITE(NW,51)                                  ! Write Z (or A) data.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           WRITE(NW,40) LINE(K),KSTAT(K,J),RDATA1(K,J,1:NCHNL)
         END DO
       END DO
       IF (KCMP == 13 .OR. KCMP == 31) WRITE(NW,52)  ! Write X (or U) data.
       IF (KCMP == 23 .OR. KCMP == 32) WRITE(NW,53)  ! Write Y (or V) data.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           WRITE(NW,40) LINE(K),KSTAT(K,J),RDATA1(K,J,NCHNL+1:2*NCHNL)
         END DO
       END DO
     END IF

     IF (KCMP == 123 .OR. KCMP == 312) THEN
       WRITE(NW,51)     ! Write Z (or A) data.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           WRITE(NW,40) LINE(K),KSTAT(K,J),RDATA1(K,J,1:NCHNL)
         END DO
       END DO
       WRITE(NW,52)     ! Write X (or U) data.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           WRITE(NW,40) LINE(K),KSTAT(K,J),RDATA1(K,J,NCHNL+1:2*NCHNL)
         END DO
       END DO
       WRITE(NW,53)     ! Write Y (or V) data.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           WRITE(NW,40) LINE(K),KSTAT(K,J),RDATA1(K,J,2*NCHNL+1:3*NCHNL)
         END DO
       END DO
     END IF

     ! Write time domain data weights.
     ! -------------------------------

     DO K = 1, NLINES
       DO J = 1, NRX(K)
         DO I = 1, MCHNL
           IF (ABS(RDATA1(K,J,I)) < DATA_FLOOR(1)) RWTS1(K,J,I) = 0
         END DO
       END DO
     END DO

     IF (KCMP == 1 .OR. KCMP == 2 .OR. KCMP == 3) THEN
       IF (KCMP == 1) WRITE(NW,54)                   ! Write Z (or A) weights.
       IF (KCMP == 2) WRITE(NW,55)                   ! Write X (or U) weights.
       IF (KCMP == 3) WRITE(NW,56)                   ! Write Y (or V) weights.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           WRITE(NW,41) LINE(K),KSTAT(K,J),RWTS1(K,J,1:NCHNL)
         END DO
       END DO
     ELSE IF (KCMP == 13 .OR. KCMP == 31 .OR. KCMP == 23 .OR. KCMP == 32) THEN
       WRITE(NW,54)                                  ! Write Z (or A) weights.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           WRITE(NW,41) LINE(K),KSTAT(K,J),RDATA1(K,J,1:NCHNL)
         END DO
       END DO
       IF (KCMP == 13 .OR. KCMP == 31) WRITE(NW,55)  ! Write X (or U) weights.
       IF (KCMP == 23 .OR. KCMP == 32) WRITE(NW,56)  ! Write Y (or V) weights.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           WRITE(NW,41) LINE(K),KSTAT(K,J),RWTS1(K,J,NCHNL+1:2*NCHNL)
         END DO
       END DO
     ELSE IF (KCMP == 123 .OR. KCMP == 312) THEN
       WRITE(NW,54)                                  ! Write Z (or A) weights.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           WRITE(NW,41) LINE(K),KSTAT(K,J),RWTS1(K,J,1:NCHNL)
         END DO
       END DO
       WRITE(NW,55)                                  ! Write X (or U) weights.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           WRITE(NW,41) LINE(K),KSTAT(K,J),RWTS1(K,J,NCHNL+1:2*NCHNL)
         END DO
       END DO
       WRITE(NW,56)                                  ! Write Y (or V) weights.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           WRITE(NW,41) LINE(K),KSTAT(K,J),RWTS1(K,J,2*NCHNL+1:3*NCHNL)
         END DO
       END DO
     END IF

   ELSE IF (TDFD == 2) THEN

     ! Write frequency domain data.
     ! ----------------------------

     IF (KCMP == 1 .OR. KCMP == 2 .OR. KCMP == 3) THEN
       IF (KCMP == 1) WRITE(NW,61)                   ! Write Z (or A) data.
       IF (KCMP == 2) WRITE(NW,62)                   ! Write X (or U) data.
       IF (KCMP == 3) WRITE(NW,63)                   ! Write Y (or V) data.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           WRITE(NW,40) LINE(K),KSTAT(K,J),RDATA1(K,J,1:2*NFRQ)
         END DO
       END DO
     ELSE IF (KCMP == 13 .OR. KCMP == 31 .OR. KCMP == 23 .OR. KCMP == 32) THEN
       WRITE(NW,63)                                  ! Write Z (or A) data.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           WRITE(NW,40) LINE(K),KSTAT(K,J),RDATA1(K,J,1:NFRQ),RDATA1(K,J,2*NFRQ+1:3*NFRQ)
         END DO
       END DO
       IF (KCMP == 13 .OR. KCMP == 31) WRITE(NW,61)  ! Write X (or U) data.
       IF (KCMP == 23 .OR. KCMP == 32) WRITE(NW,62)  ! Write Y (or V) data.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           WRITE(NW,40) LINE(K),KSTAT(K,J),RDATA1(K,J,NFRQ+1:2*NFRQ),RDATA1(K,J,3*NFRQ+1:4*NFRQ)
         END DO
       END DO
     ELSE IF (KCMP == 123 .OR. KCMP == 312) THEN
       WRITE(NW,61)                                  ! Write Z (or A) data.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           WRITE(NW,40) LINE(K),KSTAT(K,J),RDATA1(K,J,1:NFRQ),RDATA1(K,J,3*NFRQ+1:4*NFRQ)
         END DO
       END DO
       WRITE(NW,62)                                  ! Write X (or U) data.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           WRITE(NW,40) LINE(K),KSTAT(K,J),RDATA1(K,J,NFRQ+1:2*NFRQ),RDATA1(K,J,4*NFRQ+1:5*NFRQ)
         END DO
       END DO
       WRITE(NW,63)                                  ! Write Y (or V) data.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           WRITE(NW,40) LINE(K),KSTAT(K,J),RDATA1(K,J,2*NFRQ+1:3*NFRQ),RDATA1(K,J,5*NFRQ+1:6*NFRQ)
         END DO
       END DO
     ELSE IF (KCMP == 41 .OR. KCMP == 42) THEN
       IF (KCMP == 41) WRITE(NW,72)                  ! Write Zxy impedance data.
       IF (KCMP == 42) WRITE(NW,73)                  ! Write Zyx impedance data.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           WRITE(NW,40) LINE(K),KSTAT(K,J),RDATA1(K,J,1:2*NFRQ)
         END DO
       END DO
     ELSE IF (KCMP == 43) THEN
       WRITE(NW,72)                                  ! Write Zxy impedance data.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           WRITE(NW,40) LINE(K),KSTAT(K,J),RDATA1(K,J,1:NFRQ),RDATA1(K,J,2*NFRQ+1:3*NFRQ)
         END DO
       END DO
       WRITE(NW,73)                                  ! Write Zyx impedance data.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           WRITE(NW,40) LINE(K),KSTAT(K,J),RDATA1(K,J,NFRQ+1:2*NFRQ),RDATA1(K,J,3*NFRQ+1:4*NFRQ)
         END DO
       END DO
     ELSE IF (KCMP == 44) THEN
       WRITE(NW,72)                                  ! Write Zxy impedance data.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           WRITE(NW,40) LINE(K),KSTAT(K,J),RDATA1(K,J,1:NFRQ),RDATA1(K,J,4*NFRQ+1:5*NFRQ)
         END DO
       END DO
       WRITE(NW,73)                                  ! Write Zyx impedance data.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           WRITE(NW,40) LINE(K),KSTAT(K,J),RDATA1(K,J,NFRQ+1:2*NFRQ),RDATA1(K,J,5*NFRQ+1:6*NFRQ)
         END DO
       END DO
       WRITE(NW,71)                                  ! Write Zxx impedance data.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           WRITE(NW,40) LINE(K),KSTAT(K,J),RDATA1(K,J,2*NFRQ+1:3*NFRQ),RDATA1(K,J,6*NFRQ+1:7*NFRQ)
         END DO
       END DO
       WRITE(NW,74)                                  ! Write Zxx impedance data.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           WRITE(NW,40) LINE(K),KSTAT(K,J),RDATA1(K,J,3*NFRQ+1:4*NFRQ),RDATA1(K,J,7*NFRQ+1:8*NFRQ)
         END DO
       END DO
     END IF

     ! Write frequency domain data weights.
     ! ------------------------------------

     IF (KCMP == 1 .OR. KCMP == 2 .OR. KCMP == 3) THEN
       IF (KCMP == 1) WRITE(NW,64)                   ! Write Z (or A) data weights.
       IF (KCMP == 2) WRITE(NW,65)                   ! Write X (or U) data weights.
       IF (KCMP == 3) WRITE(NW,66)                   ! Write Y (or V) data weights.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           DO JF = 1, NFRQ
             JI = JF
             JQ = JF + NFRQ
             IF (ABS(RDATA1(K,J,JI)) < DATA_FLOOR(JI)) RWTS1(K,J,JI) = 0
             IF (ABS(RDATA1(K,J,JQ)) < DATA_FLOOR(JQ)) RWTS1(K,J,JQ) = 0
           END DO
           WRITE(NW,41) LINE(K),KSTAT(K,J),RWTS1(K,J,1:2*NFRQ)
         END DO
       END DO
     ELSE IF (KCMP == 13 .OR. KCMP == 31 .OR. KCMP == 23 .OR. KCMP == 32) THEN
       WRITE(NW,64)                                  ! Write Z (or A) data weights.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           DO JF = 1, NFRQ
             JI = JF
             JQ = JF + 2*NFRQ
             IF (ABS(RDATA1(K,J,JI)) < DATA_FLOOR(JI)) RWTS1(K,J,JI) = 0
             IF (ABS(RDATA1(K,J,JQ)) < DATA_FLOOR(JQ)) RWTS1(K,J,JQ) = 0
           END DO
           WRITE(NW,41) LINE(K),KSTAT(K,J),RWTS1(K,J,1:NFRQ),RWTS1(K,J,2*NFRQ+1:3*NFRQ)
         END DO
       END DO
       IF (KCMP == 13 .OR. KCMP == 31) WRITE(NW,65)  ! Write X (or U) data weights.
       IF (KCMP == 23 .OR. KCMP == 32) WRITE(NW,66)  ! Write Y (or V) data weights.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           DO JF = 1, NFRQ
             JI = JF +   NFRQ
             JQ = JF + 3*NFRQ
             IF (ABS(RDATA1(K,J,JI)) < DATA_FLOOR(JI)) RWTS1(K,J,JI) = 0
             IF (ABS(RDATA1(K,J,JQ)) < DATA_FLOOR(JQ)) RWTS1(K,J,JQ) = 0
           END DO
           WRITE(NW,41) LINE(K),KSTAT(K,J),RWTS1(K,J,NFRQ+1:2*NFRQ),RWTS1(K,J,3*NFRQ+1:4*NFRQ)
         END DO
       END DO
     ELSE IF (KCMP == 123 .OR. KCMP == 312) THEN
       WRITE(NW,64)                                  ! Write Z (or A) data weights.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           DO JF = 1, NFRQ
             JI = JF
             JQ = JF + 3*NFRQ
             IF (ABS(RDATA1(K,J,JI)) < DATA_FLOOR(JI)) RWTS1(K,J,JI) = 0
             IF (ABS(RDATA1(K,J,JQ)) < DATA_FLOOR(JQ)) RWTS1(K,J,JQ) = 0
           END DO
           WRITE(NW,41) LINE(K),KSTAT(K,J),RWTS1(K,J,1:NFRQ),RWTS1(K,J,3*NFRQ+1:4*NFRQ)
         END DO
       END DO
       WRITE(NW,65)                                  ! Write X (or U) data weights.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           DO JF = 1, NFRQ
             JI = JF +   NFRQ
             JQ = JF + 4*NFRQ
             IF (ABS(RDATA1(K,J,JI)) < DATA_FLOOR(JI)) RWTS1(K,J,JI) = 0
             IF (ABS(RDATA1(K,J,JQ)) < DATA_FLOOR(JQ)) RWTS1(K,J,JQ) = 0
           END DO
           WRITE(NW,41) LINE(K),KSTAT(K,J),RWTS1(K,J,NFRQ+1:2*NFRQ),RWTS1(K,J,4*NFRQ+1:5*NFRQ)
         END DO
       END DO
       WRITE(NW,66)                                  ! Write Y (or V) data weights.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           DO JF = 1, NFRQ
             JI = JF + 2*NFRQ
             JQ = JF + 5*NFRQ
             IF (ABS(RDATA1(K,J,JI)) < DATA_FLOOR(JI)) RWTS1(K,J,JI) = 0
             IF (ABS(RDATA1(K,J,JQ)) < DATA_FLOOR(JQ)) RWTS1(K,J,JQ) = 0
           END DO
           WRITE(NW,41) LINE(K),KSTAT(K,J),RWTS1(K,J,2*NFRQ+1:3*NFRQ),RWTS1(K,J,5*NFRQ+1:6*NFRQ)
         END DO
       END DO
     ELSE IF (KCMP == 41 .OR. KCMP == 42) THEN
       IF (KCMP == 41) WRITE(NW,76)                  ! Write Zxy impedance data weights.
       IF (KCMP == 42) WRITE(NW,77)                  ! Write Zyx impedance data weights.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           DO JF = 1, NFRQ
             JI = JF
             JQ = JF + NFRQ
             IF (ABS(RDATA1(K,J,JI)) < DATA_FLOOR(JI)) RWTS1(K,J,JI) = 0
             IF (ABS(RDATA1(K,J,JQ)) < DATA_FLOOR(JQ)) RWTS1(K,J,JQ) = 0
           END DO
           WRITE(NW,41) LINE(K),KSTAT(K,J),RWTS1(K,J,1:2*NFRQ)
         END DO
       END DO
     ELSE IF (KCMP == 43) THEN
       WRITE(NW,76)                                  ! Write Zxy impedance data weights.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           DO JF = 1, NFRQ
             JI = JF
             JQ = JF + 2*NFRQ
             IF (ABS(RDATA1(K,J,JI)) < DATA_FLOOR(JI)) RWTS1(K,J,JI) = 0
             IF (ABS(RDATA1(K,J,JQ)) < DATA_FLOOR(JQ)) RWTS1(K,J,JQ) = 0
           END DO
           WRITE(NW,40) LINE(K),KSTAT(K,J),RWTS1(K,J,1:NFRQ),RWTS1(K,J,2*NFRQ+1:3*NFRQ)
         END DO
       END DO
       WRITE(NW,77)                                  ! Write Zyx impedance data weights.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           DO JF = 1, NFRQ
             JI = JF +   NFRQ
             JQ = JF + 3*NFRQ
             IF (ABS(RDATA1(K,J,JI)) < DATA_FLOOR(JI)) RWTS1(K,J,JI) = 0
             IF (ABS(RDATA1(K,J,JQ)) < DATA_FLOOR(JQ)) RWTS1(K,J,JQ) = 0
           END DO
           WRITE(NW,40) LINE(K),KSTAT(K,J),RWTS1(K,J,NFRQ+1:2*NFRQ),RWTS1(K,J,3*NFRQ+1:4*NFRQ)
         END DO
       END DO
     ELSE IF (KCMP == 44) THEN
       WRITE(NW,76)                                  ! Write Zxy impedance data weights.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           DO JF = 1, NFRQ
             JI = JF
             JQ = JF + 4*NFRQ
             IF (ABS(RDATA1(K,J,JI)) < DATA_FLOOR(JI)) RWTS1(K,J,JI) = 0
             IF (ABS(RDATA1(K,J,JQ)) < DATA_FLOOR(JQ)) RWTS1(K,J,JQ) = 0
           END DO
           WRITE(NW,40) LINE(K),KSTAT(K,J),RWTS1(K,J,1:NFRQ),RWTS1(K,J,4*NFRQ+1:5*NFRQ)
         END DO
       END DO
       WRITE(NW,77)                                  ! Write Zyx impedance data weights.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           DO JF = 1, NFRQ
             JI = JF +   NFRQ
             JQ = JF + 5*NFRQ
             IF (ABS(RDATA1(K,J,JI)) < DATA_FLOOR(JI)) RWTS1(K,J,JI) = 0
             IF (ABS(RDATA1(K,J,JQ)) < DATA_FLOOR(JQ)) RWTS1(K,J,JQ) = 0
           END DO
           WRITE(NW,40) LINE(K),KSTAT(K,J),RWTS1(K,J,NFRQ+1:2*NFRQ),RWTS1(K,J,5*NFRQ+1:6*NFRQ)
         END DO
       END DO
       WRITE(NW,75)                                  ! Write Zxx impedance data weights.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           DO JF = 1, NFRQ
             JI = JF + 2*NFRQ
             JQ = JF + 6*NFRQ
             IF (ABS(RDATA1(K,J,JI)) < DATA_FLOOR(JI)) RWTS1(K,J,JI) = 0
             IF (ABS(RDATA1(K,J,JQ)) < DATA_FLOOR(JQ)) RWTS1(K,J,JQ) = 0
           END DO
           WRITE(NW,40) LINE(K),KSTAT(K,J),RWTS1(K,J,2*NFRQ+1:3*NFRQ),RWTS1(K,J,6*NFRQ+1:7*NFRQ)
         END DO
       END DO
       WRITE(NW,78)                                  ! Write Zxx impedance data weights.
       DO K = 1, NLINES
         DO J = 1, NRX(K)
           DO JF = 1, NFRQ
             JI = JF + 3*NFRQ
             JQ = JF + 7*NFRQ
             IF (ABS(RDATA1(K,J,JI)) < DATA_FLOOR(JI)) RWTS1(K,J,JI) = 0
             IF (ABS(RDATA1(K,J,JQ)) < DATA_FLOOR(JQ)) RWTS1(K,J,JQ) = 0
           END DO
           WRITE(NW,40) LINE(K),KSTAT(K,J),RWTS1(K,J,3*NFRQ+1:4*NFRQ),RWTS1(K,J,7*NFRQ+1:8*NFRQ)
         END DO
       END DO
     END IF
   END IF

   NSTAT = 0
   DO K = 1, NLINES
     DO J = 1, NRX(K)
       NSTAT = NSTAT+1
     END DO
   END DO
   NDATA = NSTAT * MCHNL

   ALLOCATE (XDATA(NDATA),XMODL(NDATA),XWTS(NDATA),VSTAT(NSTAT))

   XDATA = 0. ; XMODL = 0. ; XWTS = 1 ; VSTAT = 0

   ! Construct the vectors of observed data and data weights.

   JS = 0
   DO J1 = 1, NLINES
     DO J2 = 1, NRX(J1)
       JS = JS + 1
       VSTAT(JS) = KSTAT(J1,J2)
       DO J3 = 1, MCHNL
         JD = J3 + (JS-1)*MCHNL
         XDATA(JD) = RDATA1(J1,J2,J3)
         XWTS(JD) = RWTS1(J1,J2,J3)
       END DO
     END DO
   END DO

   ! Construct data normalisation vector.

   ALLOCATE (DNORM(NDATA)) ; DNORM = 1.

   BIG = MAXVAL(ABS(XDATA))
   DO J3 = 1, MCHNL
     S1 = 0.
     DO J1 = 1, NLINES
       DO J2 = 1, NRX(J1)
         S1 = S1 + ABS(RDATA1(J1,J2,J3))
       END DO
     END DO
     S1 = S1/REAL(NSTAT)
     S1 = MAX(S1,DATA_FLOOR(J3))
     IF (S1 < 1.0E-7 * BIG) S1 = 1.0E7 * BIG  ! Eliminate cross-over fluctuations.
     JS = 0
     DO J1 = 1, NLINES
       DO J2 = 1, NRX(J1)
         JS = JS + 1
         JD = J3 + (JS-1)*MCHNL
         DNORM(JD) = S1
       END DO
     END DO
   END DO

    1 FORMAT(//T3,'-----------------------------------'&
              /T3,'     INVERSION DATA FOR LOKI       '&
              /T3,'-----------------------------------')
    2 FORMAT(/T3,'Inversion of Time-Domain Z (or A) Component Data.')
    3 FORMAT(/T3,'Inversion of Time-Domain X (or U) Component Data.')
    4 FORMAT(/T3,'Inversion of Time-Domain Y (or V) Component Data.')
    5 FORMAT(/T3,'Joint Inversion of Time-Domain Z (or A) and X (or U) Component Data.')
    6 FORMAT(/T3,'Joint Inversion of Time-Domain Z (or A) and Y (or V) Component Data.')
    7 FORMAT(/T3,'Joint Inversion of Time-Domain Three Component Data.')
    8 FORMAT(/T3,'Inversion of Frequency-Domain Z (or A) Component Data.')
    9 FORMAT(/T3,'Inversion of Frequency-Domain X (or U) Component Data.')
   10 FORMAT(/T3,'Inversion of Frequency-Domain Y (or V) Component Data.')
   11 FORMAT(/T3,'Joint Inversion of Frequency-Domain Z (or A) and X (or U) Component Data.')
   12 FORMAT(/T3,'Joint Inversion of Freqeucny-Domain Z (or A) and Y (or V) Component Data.')
   13 FORMAT(/T3,'Joint Inversion of Frequency-Domain Three Component Data.')

   14 FORMAT(/T3,'Inversion of Zxy Impedance Data.')
   15 FORMAT(/T3,'Inversion of Zyx Impedance Data.')
   16 FORMAT(/T3,'Joint Inversion of Zxy and Zyx Impedance Data.')
   17 FORMAT(/T3,'Joint Inversion of Full Impedance Tensor Data.')
   20 FORMAT(/T3,'KCMP =',I4,3X,'ORDER =',I2,3X,'FIQ =',I2)
   21 FORMAT(/T3,'KCMP =',I4,3X,'ORDER =',I2)
   22 FORMAT(/T3,'Time-Domain Data Floor =',G12.4,1X,A)
   23 FORMAT(/T8,'Frequency  Data Floors (',A,')'&
             /T8,'-----------------------------' &
            //T8,'Freq     In-phase   Quadrature'/)
   24 FORMAT(I4,F9.0,2G12.4)
   25 FORMAT(/T3,'N0STAT =',I4,3X,'N0CHNL =',I3,3X,'N0PTS =',I4)
   26 FORMAT(/T3,'Data from the following stations will be weighted to zero:'/T3,60I4)
   27 FORMAT(/T3,'Data from the following PCHNLs will be weighted to zero:'/T3,60I4)
   28 FORMAT(/T3,'Data from the following (PCHNL, STAT) pairs will be weighted to zero:')
   40 FORMAT(I9,I7,40G13.4)
   41 FORMAT(I9,I7,40I2)
   51 FORMAT(/T3,'Line    Station    Z (or A) Component Data'&
             /T3,'----    -------    -----------------------')
   52 FORMAT(/T3,'Line    Station    X (or U) Component Data'&
             /T3,'----    -------    -----------------------')
   53 FORMAT(/T3,'Line    Station    Y (or V) Component Data'&
             /T3,'----    -------    -----------------------')
   54 FORMAT(/T3,'Line    Station    Z (or A) Data Weights'&
             /T3,'----    -------    ---------------------')
   55 FORMAT(/T3,'Line    Station    X (or U) Data Weights'&
             /T3,'----    -------    ---------------------')
   56 FORMAT(/T3,'Line    Station    Y (or V) Data Weights'&
             /T3,'----    -------    ---------------------')
   61 FORMAT(/T3,'Line    Station    Z (or A) Component Data; In-phase followed by Quadrature'&
             /T3,'----    -------    --------------------------------------------------------')
   62 FORMAT(/T3,'Line    Station    X (or U) Component Data; In-phase followed by Quadrature'&
             /T3,'----    -------    --------------------------------------------------------')
   63 FORMAT(/T3,'Line    Station    Y (or V) Component Data; In-phase followed by Quadrature'&
             /T3,'----    -------    --------------------------------------------------------')
   64 FORMAT(/T3,'Line    Station    Z (or A) Data Weights; In-phase followed by Quadrature'&
             /T3,'----    -------    ------------------------------------------------------')
   65 FORMAT(/T3,'Line    Station    X (or U) Data Weights; In-phase followed by Quadrature'&
             /T3,'----    -------    ------------------------------------------------------')
   66 FORMAT(/T3,'Line    Station    Y (or V) Data Weights; In-phase followed by Quadrature'&
             /T3,'----    -------    ------------------------------------------------------')
   71 FORMAT(/T3,'Line    Station    Zxx Impedance Data; Real followed by Imaginary'&
             /T3,'----    -------    ----------------------------------------------')
   72 FORMAT(/T3,'Line    Station    Zxy Impedance Data; Real followed by Imaginary'&
             /T3,'----    -------    ----------------------------------------------')
   73 FORMAT(/T3,'Line    Station    Zyx Impedance Data; Real followed by Imaginary'&
             /T3,'----    -------    ----------------------------------------------')
   74 FORMAT(/T3,'Line    Station    Zyy Impedance Data; Real followed by Imaginary'&
             /T3,'----    -------    ----------------------------------------------')
   75 FORMAT(/T3,'Line    Station    Zxx Impedance Data Weights; Real followed by Imaginary'&
             /T3,'----    -------    ------------------------------------------------------')
   76 FORMAT(/T3,'Line    Station    Zxy Impedance Data Weights; Real followed by Imaginary'&
             /T3,'----    -------    ------------------------------------------------------')
   77 FORMAT(/T3,'Line    Station    Zyx Impedance Data Weights; Real followed by Imaginary'&
             /T3,'----    -------    ------------------------------------------------------')
   78 FORMAT(/T3,'Line    Station    Zyy Impedance Data Weights; Real followed by Imaginary'&
             /T3,'----    -------    ------------------------------------------------------')

   END SUBROUTINE READ_INVRT_DATA

   SUBROUTINE RESMAP (NE,NN,NZ,NAIR,NEL,NER,NNL,NNR,NZB,STT,NLOC,ZLOC)

   !-------------------------------------------------------------------------
   !
   !*** Called by: READ_MODEL_DATA
   !
   ! Produces a 3D character map of resistivities.
   !
   !-------------------------------------------------------------------------

   IMPLICIT NONE
   REAL, PARAMETER :: TOL = .01
   INTEGER NRES,NZ,NN,NE,NAIR,I,J,K,KR,NEL,NER,NNL,NNR,NZB
   REAL STT(NE,NN,NZ),RESVAL(52),TST1,NLOC(NE,NN,NZ),ZLOC(NE,NN,NZ)
   CHARACTER (LEN=1) RESCHAR(52),MAP(100,100)
   DATA RESCHAR /'1','2','3','4','5','6','7','8','9','0','A','B','C','D','E','F', &
               'G','H','J','K','L','M','N','P','R','S','T','U','W','X','Y','a', &
               'b','c','d','e','f','g','h','j','k','l','m','n','p','r','s','t', &
               'u','w','x','y'/

   WRITE(NMP,'(/T11,A)') 'RESISTIVITY MAP'
   WRITE(NMP,'(T11,A)')  '---------------'

   NRES = 0
   RESVAL = -999.
   DO J = NNL+1, NN-NNR-1
     WRITE(NMP,'(/T11,A,F9.2/)') 'Y-Cord. = ', NLOC(1,J,NAIR+1)
     MAP = '_'
     DO K = NAIR+1, NZ-NZB-1
       HORIZ_CELL_LOOP: DO I = NEL+1, NE-NER-1
         DO KR = 1,NRES
           TST1 = ABS (STT(I,J,K) - RESVAL(KR))
           IF (TST1 < TOL * RESVAL(KR)) THEN
             MAP(I,K) = RESCHAR(KR)
             CYCLE HORIZ_CELL_LOOP
           END IF
         END DO
         NRES = NRES + 1
         RESVAL(NRES) = STT(I,J,K)
         MAP(I,K) = RESCHAR(NRES)
       END DO HORIZ_CELL_LOOP
       WRITE(NMP,'(T11,F9.2,3X,2048A)') ZLOC(1,J,K),MAP(NEL+1:NE-NER-1,K)
     END DO
   END DO

   WRITE(NMP,1)
   DO KR = 1,NRES
     WRITE(NMP,'(T4,A,T11,G13.2)') RESCHAR(KR),RESVAL(KR)
   END DO

   CLOSE (NMP)

   1 FORMAT(//T3,'RESISTIVITIES DESCRIPTION',  &
             /T3,'-------------------------'/, &
             /T3,'INDEX      RESISTIVITY'/)

   END SUBROUTINE RESMAP

   SUBROUTINE SHOW_AND_TELL
!  ------------------------

! Prints out arrays and model in model-centred coordinates

!*** Called by MAIN

 IMPLICIT NONE
 REAL SXA,SXD

 WRITE (NW,1)
 IF (ABS (ECNTRD) + ABS (NCNTRD) > 1.D0) THEN
   WRITE (NW,2) ECNTRD,NCNTRD
 ELSE
   WRITE(NW,3)
 END IF

 SELECT CASE (SURVEY_TYPE)
 CASE (1)   !  Line with single transmitter and multiple receivers
   DO JL = 1,NLINES
     JS = LNTR(1,JL)
     WRITE(NW,4) LINE(JL),R2D*SVAZM(JL)
     SELECT CASE (SOURCE_TYPE)
     CASE(1:2)                               ! Open or closed loop Tx
       IF (SOURCE_TYPE == 1) WRITE(NW,5)
       IF (SOURCE_TYPE == 2) WRITE(NW,6)
       WRITE(NW,7) JS,NVRTX(JS)
       DO JV = 1, NVRTX(JS)
         WRITE(NW,'(I5,3F11.2)') JV,SXE(JV,JS),SXN(JV,JS),SXZ(JS)
       END DO

     CASE(3)                               ! Magnetic dipole Tx
       WRITE(NW,8)
       WRITE(NW,9)
       WRITE(NW,'(I4,3F11.2,2F10.2,G12.4)') JS,SXE(1,JS),SXN(1,JS),SXZ(JS),R2D*SXDIP(JS),R2D*SXAZM(JS),TXMNT
     END SELECT

     SELECT CASE (RX_TYPE(JL))
     CASE(1)                     !  Magnetic dipole receivers
       WRITE(NW,11) LINE(JL)
       IF (IDH(JL) < 3) THEN               !  Surface position only
         WRITE(NW,10)
         DO JR = 1,NRX(JL)
           WRITE(NW,'(I4,3F11.2,G12.4)') JR,RXE(JR,JL,1),RXN(JR,JL,1),RXZ(JR,JL),RXMNT(JL)
         END DO
       ELSE
         WRITE(NW,9)
         DO JR = 1,NRX(JL)
           WRITE(NW,'(I4,3F11.2,2F7.1,G12.4)') JR,RXE(JR,JL,1),RXN(JR,JL,1),RXZ(JR,JL),RXDIP(JR,JL),RXAZM(JR,JL),RXMNT(JL)
         END DO
       END IF

     CASE(2)                     !  Grounded wire receivers
       WRITE(NW,12) LINE(JL)
       DO JR = 1, NRX(JL)
         WRITE(NW,'(I4,6F11.2)') JR,RXE(JR,JL,1),RXN(JR,JL,1),RXE(JR,JL,2),RXN(JR,JL,2),RXZ(JR,JL)
       END DO

     CASE(3)                     !  Point E-field receivers
       WRITE(NW,61) LINE(JL)
       DO JR = 1,NRX(JL)
         WRITE(NW,'(I4,3F11.2)') JR,RXE(JR,JL,1),RXN(JR,JL,1),RXZ(JR,JL)
       END DO
     END SELECT
   END DO
 CASE (2)   !  Line with moving loop transmitters and one or more fixed offset receivers
   WRITE(NW,13)
   DO JL = 1,NLINES
     JR = 0
     DO JS = LNTR(1,JL), LNTR(2,JL)
       JR = JR + 1
       DO JV = 1,4
         IF (JV == 1) THEN
           IF (JR == 1) THEN
             WRITE(NW,15) LINE(JL),JS,JV,SXE(1,JS),SXN(1,JS),RXE(1,JL,1),RXN(1,JL,1),YXZPLT(1:2,JR,JL)
           ELSE
             WRITE(NW,16) JS,JV,SXE(1,JS),SXN(1,JS),RXE(JR,JL,1),RXN(JR,JL,1),YXZPLT(1:2,JR,JL)
           END IF
         ELSE
           WRITE(NW,17) JV,SXE(JV,JS),SXN(JV,JS)
         END IF
       END DO
     END DO
   END DO

 CASE (3)   !  Line with moving MD transmitters and one or more fixed offset receivers
   WRITE(NW,18)
   DO JL = 1,NLINES
     JR = 0
     DO JS = LNTR(1,JL), LNTR(2,JL)
       JR = JR + 1
       SXD = R2D*SXDIP(JS); SXA = R2D*SXAZM(JS)
       IF (JR == 1) THEN
         WRITE(NW,19) LINE(JL),JS,RXE(1,JL,1),RXN(1,JL,1),RXZ(1,JL),SXE(1,JS),SXN(1,JS),SXZ(JS),SXD,SXA
       ELSE
         WRITE(NW,20) JS,RXE(JR,JL,1),RXN(JR,JL,1),RXZ(JR,JL),SXE(1,JS),SXN(1,JS),SXZ(JS)
       END IF
     END DO
   END DO

 CASE (4)                                 !  Coincident loop lines
   WRITE(NW,14)

   DO JL = 1,NLINES
     JR = 0
     DO JS = LNTR(1,JL), LNTR(2,JL)
       JR = JR + 1
       DO JV = 1,4
         IF (JV == 1) THEN
           IF (JR == 1) THEN
             WRITE(NW,15) LINE(JL),JS,JV,SXE(1,JS),SXN(1,JS),YXZPLT(1:2,JR,JL)
           ELSE
             WRITE(NW,16) JS,JV,SXE(1,JS),SXN(1,JS),YXZPLT(1:2,JR,JL)
           END IF
         ELSE
           WRITE(NW,17) JV,SXE(JV,JS),SXN(JV,JS)
         END IF
       END DO
     END DO
   END DO

 CASE (5)   !  Downhole probe
   WRITE(NW,21)
   DO JL = 1,NLINES
     JR = 0
     DO JS = LNTR(1,JL), LNTR(2,JL)
       JR = JR + 1
       SXD = R2D*SXDIP(JS); SXA = R2D*SXAZM(JS)
       IF (JR == 1) THEN
         WRITE(NW,22) LINE(JL),JS,SXE(1,JS),SXN(1,JS),SXZ(JS),SXD,SXA,RXE(1,JL,1),RXN(1,JL,1),RXZ(1,JL)
       ELSE
         WRITE(NW,23) JS,SXE(1,JS),SXN(1,JS),SXZ(JS),SXD,SXA,RXE(JR,JL,1),RXN(JR,JL,1),RXZ(JR,JL)
       END IF
     END DO
   END DO

 END SELECT

 SELECT CASE (SURVEY_TYPE)
 CASE (1,3,5)
   DO JL = 1,NLINES     ! Plot coordinates
     WRITE(NW,24) LINE(JL)
     DO JR = 1,NRX(JL)
       WRITE(NW,'(I4,2F13.1,F9.1)') JR,YXZPLT(1:3,JR,JL)
     END DO
   END DO
 END SELECT

! Set up pretty output for layered earth.

! WRITE(NW,31)
! DEPTH = 0.
! DO JL = 1, NLYR
!   IF (JL > 1) DEPTH = DEPTH + THK(JL-1)
!   IF(JL == NLYR) THEN
!     WRITE(NW,32) NLYR,DEPTH,RES(NLYR),RMU(NLYR),REPS(NLYR),CHRG(NLYR),CFREQ(NLYR),CTAU(NLYR)
!   ELSE
!     WRITE(NW,33) JL,THK(JL),DEPTH,RES(JL),RMU(JL),REPS(JL),CHRG(JL),CFREQ(JL),CTAU(JL)
!   END IF
! END DO

 IF (DO3D == 0) THEN
   IF (TDFD < 2) WRITE(NW,15)  ! Time-domain option
   IF (TDFD == 2) WRITE(NW,16)  ! Frequency-domain option

 ELSE

!!  Set up comprehensive output for
!!  Set up and print 3D model.  Convert strike angles to radians
!
!   CALL SET_CELLS (NW,DEPTH,NPLT,MXAB,CELLW,PLNGTH,PLWDTH,XCNTR,YCNTR,PLTOP,PLAZM,PLDIP, &
!                   PLUNJ,NA,NB,DA,DB,MXCL2,NCELL2,XCELL,YCELL,ZCELL,ZMIN,ZMAX,SHIFT)
!
!   DO JP = 1,NPLT
!     WRITE(NW,40) JP
!     IF (SHIFT(JP) > 0.001) WRITE(NW,39) JP,SHIFT(JP)
!     WRITE(NW,41) LITHP(JP),SIG_T(JP),CHRGP(JP),CFREQP(JP),CTAUP(JP)
!     WRITE(NW,42) PLTOP(JP),DZM(JP),PLNGTH(JP),XCNTR(JP),DIP(JP),PLWDTH(JP),YCNTR(JP),PLG(JP)
!     WRITE(NW,44) JP,NA(JP),DA(JP),NB(JP),DB(JP)
!   END DO
!
!! Set up and print cell positions after setting up the geometric factors between
!! cells in different plates.  Convert strike angles to radians
!
!   DO JP = 1,NPLT
!     WRITE(NW,25) JP
!     DO JB = 1, NB(JP)
!       DO JA = 1, NA(JP)
!         JAB = JA + ((JB-1) * NA(JP))
!         IF (JA == 1) THEN
!           IF (JB > 1) WRITE(NW,'(A)') '  '
!           WRITE(NW,'(I5,I7,6X,3G13.4)') JB,JA,YCELL(JAB,JP),XCELL(JAB,JP),ZCELL(JAB,JP)
!         ELSE
!           WRITE(NW,'(5X,I7,6X,3G13.4)')    JA,YCELL(JAB,JP),XCELL(JAB,JP),ZCELL(JAB,JP)
!         END IF
!       END DO
!     END DO
!   END DO
   IF (TDFD < 2) WRITE(NW,51)  ! Time-domain option
   IF (TDFD == 2) WRITE(NW,52)  ! Frequency-domain option
 END IF

 WRITE(NW,53)                 ! End of input data description


 1 FORMAT(//T3,'Before computation begins, Loki may transform array and model coordinates'  &
           /T3,'from GPS coordimnates where elevation increases positive upwards to a'     &
           /T3,'body-centred system where depth increases positive downwards.'                &
           /T3,'In this system, the dip of magnetic dipole transmitters and receivers'         &
           /T3,'= 0 for vertical dipoles and 90 for horizontal dipoles.')
 2 FORMAT( /T3,'The new computational horizontal origin is over the centre of the model region.' &
           /T3,'In the original user defined system the new computation origin is located at:'   &
          //T4,'EAST: ',F12.2,';    NORTH: ',F12.2)
 3 FORMAT( /T3,'The computational horizontal origin remains unchanged.')
 4 FORMAT(/T3,'Transformed transmitter and receiver locations for Line',I7 &
           /T3,'Survey aximuth =',F5.0,'degrees clockwise from North.')
 5 FORMAT(/T3,'Transformed Vertex Locations for Loop Sources' &
          /T3,'---------------------------------------------')
 6 FORMAT(/T3,'Transformed Vertex Locations for Grounded Wire Sources' &
          /T3,'------------------------------------------------------')
 7 FORMAT(/T7,'Transmitter',I3,' has',I3,' vertices.'&
        //T10,'Easting   Northing'/)
 8 FORMAT(/T3,'Transformed Dipole Source Specification')
 9 FORMAT(/T9,'Easting   Northing      Depth    Txcln    Azimuth   Moment' &
          /T9,'-------   --------      -----    -----    -------   ------')
 10 FORMAT(/T9,'Easting   Northing      Depth   Moment' &
           /T9,'-------   --------      -----   ------')
 11 FORMAT(//T3,'Transformed Locations for Magnetic Dipole Receivers in Line',I7 &
            /T3,'------------------------------------------------------------------')
 12 FORMAT(//T3,'Transformed Electrode Positions for Electric Dipoles in Line',I7    &
            /T3,'-------------------------------------------------------------------' &
           /T10,'East 1    North 1     East 2    North 2     Depth'/)
 13 FORMAT(/T9,'Transformed Receiver & Transmitter Vertex Coordinates' &
           /T9,'-----------------------------------------------------' &
          //T25,'Transmitter             Receiver               Plot Point' &
           /T5,'Line',T23,'East      North',T47,'East      North',T71,'East      North')
 14 FORMAT(/T9,'Transformed Coincident Loop Vertex Coordinates' &
           /T9,'----------------------------------------------' &
          //T25,'Transmitter',T49,'Plot Point'                  &
           /T5,'Line',T23,'East      North',T47,'East      North')
 15 FORMAT(/I8,2I4,2F11.1,2(2X,2F11.1))
 16 FORMAT(/8X,2I4,2F11.1,2(2X,2F11.1))
 17 FORMAT(12X,I4,2F11.1,2X,2F11.1)
 18 FORMAT(/T17,'Transformed Receiver & Transmitter Dipole Coordinates' &
           /T17,'-----------------------------------------------------' &
         //T23,'Receiver',T58,'Transmitter'/T23,'--------',T58,'-----------'   &
           /T5,'Line          East       North     Depth         East       North      Depth     Incl    Azm',&
           /T5,'----          ----       -----     -----         ----       -----      -----     ----    ---')
 19 FORMAT(/I8,I4,2F12.1,F9.1,2X,2F12.1,F9.1,2X,2F7.0)
 20 FORMAT(/8X,I4,2F12.1,F9.1,2X,2F12.1,F9.1,2X,2F7.0)
 21 FORMAT(/T16,'Transformed Downhole Probe Coordinates & Orientation'         &
           /T16,'----------------------------------------------------'         &
          //T26,'Transmitter',T51,'Borehole',T75,'Receiver'     &
           /T26,'-----------',T51,'--------',T75,'--------'                  &
           /T19,          'East      North     Depth      Dip     Azm       East      North     Depth '   &
           /T5,'Line',T19,'----      -----     -----      ---     ---       ----      -----     -----')
 22 FORMAT(I8,I4,2F11.1,F10.1,1X,2F8.1,2F11.1,F10.1)
 23 FORMAT(8X,I4,2F11.1,F10.1,1X,2F8.1,2F11.1,F10.1)
 24 FORMAT(//T3,'Plot points for receivers on Line',I7, &
           //T13,'East        North     Elev'          &
            /T13,'----        -----     ----' /I4,2F13.1,F9.1)
 51 FORMAT(/T3,'Loki will compute 3D responses for a time-domain system.'/)
 52 FORMAT(/T3,'Loki will compute 3D responses for a frequency-domain system.'/)
 53 FORMAT(/75('-')/T24,'END OF INPUT DATA DESCRIPTION'/75('-'))
 61 FORMAT(//T3,'Transformed Locations for Point E-field Receivers in Line',I7     &
            /T3,'----------------------------------------------------------------' &
           //T9,'Easting   Northing      Depth' &
            /T9,'-------   --------      -----')

   END SUBROUTINE SHOW_AND_TELL

   SUBROUTINE SET_FRQ
!  ------------------

   REAL(KIND=QL), PARAMETER :: MID = 10._QL
   INTEGER J,PPDH,PPDL
   REAL(KIND=QL) QFRQ12,QFRQL,QFRQH,MIN_FREQD, MAX_FREQD
   REAL(KIND=QL), ALLOCATABLE :: FDUM(:)

   ALLOCATE (FDUM(1000))
   IF (TDFD == 1) THEN
     FDUM(1) = 0.1_QL
     PPDL = 3
     PPDH = 6
     MAX_FREQD = 1.0E5_QL
     T0 = MINVAL (TOPN) - SWX(NSX)
     IF (T0 < .28E-3) MAX_FREQD = 1.0E6_QL         ! Extend range to 1 MHz
   ELSE IF (TDFD == 0) THEN
     QFRQ12 = EXP ( LOG (10.D0) / REAL (12._QL) )
     MIN_FREQD = 1.001_QL * REAL (MIN_FREQ,KIND=QL)
     MAX_FREQD = REAL (MAX_FREQ,KIND=QL)
     FDUM(1) = 1000._QL
     DO
       IF (FDUM(1) < MIN_FREQD) EXIT
       FDUM(1) = FDUM(1) / QFRQ12
     END DO

     IF (IPPD == 0) THEN
       PPDL = 3
       PPDH = 6
     ELSE
       PPDL = 3
       IF (IPPD > 3) PPDL = 6
       IF (IPPD > 6) PPDL = 12
       PPDH = PPDL
     END IF
   END IF

   QFRQL = EXP (LOG (10.D0) / REAL (PPDL) )
   QFRQH = EXP (LOG (10.D0) / REAL (PPDH) )
   NFRQ = 1
   DO J = 2,1000
     NFRQ = J
     IF (FDUM(J-1) < MID) THEN
        FDUM(J) = FDUM(J-1) * QFRQL
     ELSE
        FDUM(J) = FDUM(J-1) * QFRQH
     END IF
     IF (FDUM(J) > 0.999 * MAX_FREQD) EXIT
   END DO

   ALLOCATE (FREQ(NFRQ))
   FREQ(1:NFRQ) = REAL (FDUM(1:NFRQ))
   DEALLOCATE (FDUM)

   WRITE(NW,1) FREQ(1),MID,PPDL,MID,FREQ(NFRQ),PPDH,FREQ(1)

 1 FORMAT(/T3,'The frequency-domain results are directly computed from' &
          /T3,G12.4,' Hz to',G12.4,' Hz, using',I3,' points per decade and from' &
          /T3,G12.4,' Hz to',G12.4,' Hz, using',I3,' points per decade.' &
         //T3,'These are used to construct the frequency-domain spectrum from DC to',G12.4,' Hz' &
          /T3,'before transformation to the time domain.  The transformed result is folded over' &
          /I3,' pulses and convolved with the input signal.')

   END SUBROUTINE SET_FRQ

   Subroutine Set_Frq_from_File
!   
   Integer, Parameter :: NCOMP = 3
   Integer :: jf, its
   Integer :: ioer

   Real :: stn1, stn2, cfrq, ifrq
   Real :: bfd(2 * NCOMP)

   Rewind (nd)

!
!   We need to read the number of unique frequencies from the file so that we can allocate the array.
   ioer = 0; cfrq = 0.; nfrq = 0
   Do While (ioer .ne. -1)
        Read (nd, *, IOSTAT = ioer) bfd, stn1, stn2, ifrq, its
        If (ifrq .ne. cfrq) Then
            nfrq = nfrq + 1
            cfrq = ifrq
        End If
   End Do
   Rewind (nd)
!
!  Now we can read the correct frequencies
   Allocate (freq(nfrq))
   ioer = 0; cfrq = 0.; nfrq = 0
   Do While (ioer .ne. -1)
        Read (nd, *, IOSTAT = ioer) bfd, stn1, stn2, ifrq, its
        If (ifrq .ne. cfrq) Then
            nfrq = nfrq + 1
            freq(nfrq) = ifrq
            cfrq = ifrq
        End If
   End Do
   Rewind (nd)

   Write (nw, 10) nfrq
   Do jf = 1, nfrq
     Write (nw, 11) jf, freq(jf)
   End Do
   Write (nw, 12)

10 Format (/, 2x, '', &
           /, 2x, 'Loki will read frequency-domain computations from Loki.frq.' /, &
           /, 2x, 'Time-domain calulations use ', i3, ' frequencies.', /, &
           /, 2x, '   #', 2x, '      Frequency', &
           /, 2x, '----', 2x, '---------------')
11 Format (2x, i4, 2x, en15.6)
12 Format (2x, 64('-'), //)

   End Subroutine Set_Frq_from_File

   SUBROUTINE SET_RHO
!  ------------------

!  Sets up horizontal interpolation array (12 digit precision) for Hankel transforms
!  from 0.1 m to 10 km

!***  Called by READ_LOKI_DATA

   USE KG_Filter_coefficients

   REAL(KIND=QL), ALLOCATABLE :: B(:)
   REAL(KIND=QL) QRHO, RBASE

!  Set the horizontal interpolation grid to conform to filter intervals.

   QRHO = LOG (10._QL) / REAL (NDEC_JN,KIND=QL)
   QRHO = EXP (QRHO)
   RBASE = EXP (REAL (-SHFTJN,KIND=QL))

   ALLOCATE (B(1000))
   B(1) = .1_QL

   DO JR = 1,1000                 !  Get starting point
     IF (RBASE < B(1)) EXIT
     RBASE = RBASE / QRHO
   END DO
   B(1) = RBASE

   DO JR = 2, 10000
     MXRHO = JR
     B(JR) = B(JR-1) * QRHO
     IF (B(JR) > 1.D4) EXIT
   END DO

   ALLOCATE (RHOTRP(MXRHO))
   RHOTRP(1:MXRHO) = REAL ( B(1:MXRHO))
   DEALLOCATE (B)

   END SUBROUTINE SET_RHO

   SUBROUTINE SET_TRP
!  ------------------

!  Sets up interpolation times for FD -> TD transform which use the
!  exact 6 points per decade frequency-domain data plus 6 per decade
!  interpolated values.  These are based on a 12 point per decade
!  cosine filter derived from the Niels Christensen routine FILCOA
!  with OMEGA = .3 PI and shift 0.

!***  Called by: MAIN program
!***       Uses:  MODULE KG_Filter_coefficients

!             Output
!             ------

!        TRP - array of time values for FD -> TD transformations
!      NTYRP - number of values in TRP
!     EXTENT - the latest time for which time-domain output is required.
!      PULSE - time length of one signal pulse
!     NTYPLS - number of TRP values in 1 PULSE


 IMPLICIT NONE
 REAL, PARAMETER :: TWOPI=6.2831853, T0_MIN=0.1E-6
 INTEGER MXTYM,J1
 REAL EXTENT,T0
 REAL,ALLOCATABLE :: QQQ(:)
 REAL(KIND=QL) TBASE,QTYM, TQ

 MXTYM=200
 ALLOCATE (QQQ(MXTYM))
 QQQ = 0.

 QTYM = LOG (10.D0) /12.D0
 QTYM = EXP (QTYM)
 NPULS = 5
 EXTENT = 2.0 * NPULS * PULSE

 T0 = MINVAL (TOPN) - SWX(NSX)
 T0 = MAX (T0, T0_MIN)
 TBASE = 1.D0 / DBLE (TWOPI)
 DO J1 = 1,MXTYM
   IF (TBASE < T0) EXIT
   TBASE = TBASE / QTYM
 END DO

 TQ = TBASE
 QQQ(1) = REAL (TQ)
 DO J1 = 2, MXTYM
   NTYRP = J1
   TQ = TQ * QTYM
   QQQ(J1) = REAL(TQ)
   IF (QQQ(J1) < PULSE) NTYPLS = J1+2
   IF( QQQ(J1) > EXTENT) EXIT
 END DO

 ALLOCATE (TRP(NTYRP))
 TRP(1:NTYRP) = QQQ(1:NTYRP)
 DEALLOCATE (QQQ)

   END SUBROUTINE SET_TRP

   SUBROUTINE WRITE_np_INITIAL
!  ----------------------------
!
!***  Called by: MAIN
!
! Sets up the initial part of the output plotting file for inversion.

! HEADER_ID is used to set the appropriate LINE HEADER(s)
! HEADER_ID = 100 + 10 * I2 + I3 for time domain
!           = 200 + 10 * I2 + I3 for frequency domain
!
! I2 =1 (magnetic dipole RX);  =2 (electric dipole Rx);  =3 (point E-field Rx)
!    =4 (coincident loop);     =5 (Sampo);               =6 (magnetotellurics)
!
! I1 = 0: X, Y, Z output (usually for surface receivers)
! I1 = 1: U, V, A output (for downhole receivers only)
! I1 = 2: W, N, S output (for UTEM downhole receivers only)
!
! In this version
!
!  HEADER_ID = 110 or 210 for SURVEY_TYPE = 2 or 3 (constant Rx offfset for loop or magnetic dipole Tx)
!            = 140 for SURVEY_TYPE = 4 (coincident loop )
!            = 110, 111, 112, 210, 211 or 212 for SURVEY_TYPE = 5 (drillhole probe)
!            = 250 for Sampo
!            = 260 or 261 for magnetotellurics
!            = 120 or 220 for electric dipole receivers (SURVEY_TYPE = 1 only)
!            = 230 for point E-field receivers (SURVEY_TYPE = 1 only)
!
!  HEADER_ID is Line dependent for SURVEY_TYPE = 1


 INTEGER HID,CL,NCL
! CHARACTER(LEN=7) RES_MOD(9), THK_MOD(9)
 CHARACTER(LEN=20) SVTXT,SXTXT,QL,QL0
! CHARACTER(LEN=82) PLT_MOD(9)
! DATA RES_MOD /'  RES_1','  RES_2','  RES_3','  RES_4','  RES_5','  RES_6','  RES_7','  RES_8','  RES_9'/
! DATA THK_MOD /'  THK_1','  THK_2','  THK_3','  THK_4','  THK_5','  THK_6','  THK_7','  THK_8','  THK_9'/
! DATA PLT_MOD /'  SIG_T_1  PLNGTH_1  DIP_WDTH_1  DEPTH_1  EAST_1  NORTH_1  DIP_AZM_1  DIP_1  PLG_1', &
!               '  SIG_T_2  PLNGTH_2  DIP_WDTH_2  DEPTH_2  EAST_2  NORTH_2  DIP_AZM_2  DIP_2  PLG_2', &
!               '  SIG_T_3  PLNGTH_3  DIP_WDTH_3  DEPTH_3  EAST_3  NORTH_3  DIP_AZM_3  DIP_3  PLG_3', &
!               '  SIG_T_4  PLNGTH_4  DIP_WDTH_4  DEPTH_4  EAST_4  NORTH_4  DIP_AZM_4  DIP_4  PLG_4', &
!               '  SIG_T_5  PLNGTH_5  DIP_WDTH_5  DEPTH_5  EAST_5  NORTH_5  DIP_AZM_5  DIP_5  PLG_5', &
!               '  SIG_T_6  PLNGTH_6  DIP_WDTH_6  DEPTH_6  EAST_6  NORTH_6  DIP_AZM_6  DIP_6  PLG_6', &
!               '  SIG_T_7  PLNGTH_7  DIP_WDTH_7  DEPTH_7  EAST_7  NORTH_7  DIP_AZM_7  DIP_7  PLG_7', &
!               '  SIG_T_8  PLNGTH_8  DIP_WDTH_8  DEPTH_8  EAST_8  NORTH_8  DIP_AZM_8  DIP_8  PLG_8', &
!               '  SIG_T_9  PLNGTH_9  DIP_WDTH_9  DEPTH_9  EAST_9  NORTH_9  DIP_AZM_9  DIP_9  PLG_9'/

 WRITE(np,1) FVERS,Trim(PNAME),TRIM(TITLE)
 CALL GET_SURVEY_TEXT (SURVEY_TYPE,SVTXT)
 CALL GET_SOURCE_TEXT (SOURCE_TYPE,SXTXT)

 IF (TDFD < 2) THEN
   WRITE(np,2) TRIM (ADJUSTL (SVTXT)),TRIM (ADJUSTL (SXTXT))
   WRITE(np,3) NCHNL,REFTYM
   WRITE(np,4) TMS(1:NCHNL)
   WRITE(np,5) WTMS(1:NCHNL)
   IF (ISYS == 4) WRITE(np,24)
 ELSE
   WRITE(np,6) TRIM (ADJUSTL (SVTXT)),TRIM (ADJUSTL (SXTXT))
   WRITE(np,7) NFRQ
   WRITE(np,8) FREQ(1:NFRQ)
   IF (ISYS == 2) WRITE(np,22)
   IF (ISYS == 6) THEN
     IF (IPM == 1) THEN
       WRITE(np,26)
     ELSE
       WRITE(np,27)
     END IF
   END IF
 END IF

 IF (.NOT. MT) WRITE(np,9) NLINES

 HID = HEADER_ID(1)
 IF (HID > 240 .OR. HID == 140) THEN  ! Sampo, Coincident loop or MT
   QL = '001'
   CL = 1
   NCL = 1
   CALL WRITE_LINE_HEADER (QL,HID,CL,NCL)
 ELSE
   DO JL = 1, NLINES
     NCL = NCMPL(JL)
     CL = CMP(JL)
     HID = HEADER_ID(JL)
     WRITE(QL0,*) LINE(JL)
     READ(QL0,'(A)') QL           ! Line number
     CALL WRITE_LINE_HEADER (QL,HID,CL,NCL)
   END DO
 END IF

! WRITE(np,10) NLYR,NPLT
! WRITE(np,11) PLT_MOD(1:NPLT),RES_MOD(1:NLYR),THK_MOD(1:NLYR-1)
! WRITE(np,12) MPAR(1:NPAR)
! WRITE(np,12) MPAR(1:10)

  1 FORMAT(T1,'/ ',I4.4,T15,'File version number'/T1,'/ PROGRAM_NAME: ',A/T1,'/ TITLE: ',A)
  2 FORMAT(T1,'/ Time-Domain Survey_Type = ',A,4X,'Source_Type = ',A)
  3 FORMAT(T1,'/ NCH=',I3.3,4X,'REFTYM(ms)=',G12.4)
  4 FORMAT(T1,'/ TIMES(ms)=',T17,100G13.4)
  5 FORMAT(T1,'/ CHNL_WDTH(ms)=',T17,100G13.4)
  6 FORMAT(T1,'/ Frequency-Domain Survey_Type = ',A,4X,'Source_Type = ',A)
  7 FORMAT(T1,'/ NFRQ=',I3.3)
  8 FORMAT(T1,'/ FREQS(Hz) =',100G13.4)
  9 FORMAT(T1,'/ NLINES =',I3)
 22 FORMAT(T1,'/ SYSTEM: SAMPO')
 24 FORMAT(T1,'/ SYSTEM: UTEM')
 26 FORMAT(T1,'/ SYSTEM: MAGNETOTELLURICS:  Phase and Magnitude')
 27 FORMAT(T1,'/ SYSTEM: MAGNETOTELLURICS:  Inphase and Quadrature')

   END SUBROUTINE WRITE_np_INITIAL

   SUBROUTINE WRITE_LINE_HEADER (QL,HID,CL,NCL)
!  -------------------------------------------
!
!    QL  - Line number in character form
!    HID - Header ID defining survey & line character
!    CL  - component control

!  For inversion of magnetic dipole or point electric receiver data, all three components of the
!  data are put into the mv1 file, even if some are given null status.
!  This is not the case for Sampo or MT.

 INTEGER HID,CL,NCL
 CHARACTER(LEN=20) QL
 CHARACTER(LEN=5) CHZ,CHX,CHY,CHU,CHV,CHA,CHS,CHN,CHW,CHE,CHC, &
                  RFZ,RFX,RFY,RFU,RFV,RFA,RFS,RFN,RFW,RFE, &
                  QFZ,QFX,QFY,QFU,QFV,QFA,QFS,QFN,QFW,QFE,SMP
 CHARACTER(LEN=6) ZXXR,ZXXQ,ZXXP,ZXXM,ZXYR,ZXYQ,ZXYP,ZXYM,ZYXR,ZYXQ,ZYXP,ZYXM,ZYYR,ZYYQ,ZYYP,ZYYM

 CHZ = '  CHZ' ;  RFZ = '  RFZ'  ;  QFZ = '  QFZ'    ! Z dipole component
 CHX = '  CHX' ;  RFX = '  RFX'  ;  QFX = '  QFX'    ! X dipole component
 CHY = '  CHY' ;  RFY = '  RFY'  ;  QFY = '  QFY'    ! Y dipole component
 CHU = '  CHU' ;  RFU = '  RFU'  ;  QFU = '  QFU'    ! U dipole component
 CHV = '  CHV' ;  RFV = '  RFV'  ;  QFV = '  QFV'    ! V dipole component
 CHA = '  CHA' ;  RFA = '  RFA'  ;  QFA = '  QFA'    ! A dipole component
 CHS = '  CHS' ;  RFS = '  RFS'  ;  QFS = '  QFS'    ! S dipole component - UTEM
 CHN = '  CHN' ;  RFN = '  RFN'  ;  QFN = '  QFN'    ! N dipole component - UTEM
 CHW = '  CHW' ;  RFW = '  RFW'  ;  QFW = '  QFW'    ! W dipole component - UTEM
 CHC = '  CHC' ;  SMP = '  SMP'                      ! coincident loop (TD); Sampo (FD)
 CHE = '  CHE' ;  RFE = '  RFE'  ;  QFE = '  QFE'    ! electric potential or field
 ZXXR = '  ZXXR' ;  ZXYR = '  ZXYR' ;  ZYXR = '  ZYXR' ;   ZYYR = '  ZYYR'
 ZXXQ = '  ZXXQ' ;  ZXYQ = '  ZXYQ' ;  ZYXQ = '  ZYXQ' ;   ZYYQ = '  ZYYQ'
 ZXXP = '  ZXXP' ;  ZXYP = '  ZXYP' ;  ZYXP = '  ZYXP' ;   ZYYP = '  ZYYP'
 ZXXM = '  ZXXM' ;  ZXYM = '  ZXYM' ;  ZYXM = '  ZYXM' ;   ZYYM = '  ZYYM'

 SELECT CASE (HID)
 CASE (110)            ! TD magnetic dipole receiver : X, Y, Z

   SELECT CASE (CL)
   CASE(1)
     WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHX,JT,JT=1,NCHNL)
   CASE(2)
     WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHY,JT,JT=1,NCHNL)
   CASE(3)
     WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHZ,JT,JT=1,NCHNL)
   CASE(12)
     WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHX,JT,JT=1,NCHNL), (CHY,JT,JT=1,NCHNL)
   CASE(13)
     WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHX,JT,JT=1,NCHNL), (CHZ,JT,JT=1,NCHNL)
   CASE(23)
     WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHY,JT,JT=1,NCHNL), (CHZ,JT,JT=1,NCHNL)
   CASE(123)
     WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHX,JT,JT=1,NCHNL), (CHY,JT,JT=1,NCHNL), (CHZ,JT,JT=1,NCHNL)
   END SELECT

 CASE (210)           ! FD magnetic dipole receiver : X, Y, Z
   SELECT CASE (CL)
   CASE(1)
   WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFX,JF,JF=1,NFRQ), (QFX,JF,JF=1,NFRQ)
   CASE(2)
   WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFY,JF,JF=1,NFRQ), (QFY,JF,JF=1,NFRQ)
   CASE(3)
   WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFZ,JF,JF=1,NFRQ), (QFZ,JF,JF=1,NFRQ)
   CASE(12)
   WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFX,JF,JF=1,NFRQ), (QFX,JF,JF=1,NFRQ),  &
                                              (RFY,JF,JF=1,NFRQ), (QFY,JF,JF=1,NFRQ)
   CASE(13)
   WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFX,JF,JF=1,NFRQ), (QFX,JF,JF=1,NFRQ),  &
                                              (RFZ,JF,JF=1,NFRQ), (QFZ,JF,JF=1,NFRQ)
   CASE(23)
   WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFY,JF,JF=1,NFRQ), (QFY,JF,JF=1,NFRQ),  &
                                              (RFZ,JF,JF=1,NFRQ), (QFZ,JF,JF=1,NFRQ)
   CASE(123)
   WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFX,JF,JF=1,NFRQ), (QFX,JF,JF=1,NFRQ), (RFY,JF,JF=1,NFRQ), &
                                              (QFY,JF,JF=1,NFRQ), (RFZ,JF,JF=1,NFRQ), (QFZ,JF,JF=1,NFRQ)
   END SELECT

 CASE (111)           ! Downhole TD magnetic dipole receiver : U, V, A
   SELECT CASE (CL)
   CASE(1)
     WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHU,JT,JT=1,NCHNL)
   CASE(2)
     WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHV,JT,JT=1,NCHNL)
   CASE(3)
     WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHA,JT,JT=1,NCHNL)
   CASE(12)
     WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHU,JT,JT=1,NCHNL), (CHV,JT,JT=1,NCHNL)
   CASE(13)
     WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHU,JT,JT=1,NCHNL), (CHA,JT,JT=1,NCHNL)
   CASE(23)
     WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHV,JT,JT=1,NCHNL), (CHA,JT,JT=1,NCHNL)
   CASE(123)
     WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHU,JT,JT=1,NCHNL), (CHV,JT,JT=1,NCHNL), (CHA,JT,JT=1,NCHNL)
   END SELECT

 CASE (211)           ! Downhole FD magnetic dipole receiver : U, V, A
   SELECT CASE (CL)
   CASE(1)
   WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFU,JF,JF=1,NFRQ), (QFU,JF,JF=1,NFRQ)
   CASE(2)
   WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFV,JF,JF=1,NFRQ), (QFV,JF,JF=1,NFRQ)
   CASE(3)
   WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFA,JF,JF=1,NFRQ), (QFA,JF,JF=1,NFRQ)
   CASE(12)
   WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFU,JF,JF=1,NFRQ), (QFU,JF,JF=1,NFRQ),  &
                                              (RFV,JF,JF=1,NFRQ), (QFV,JF,JF=1,NFRQ)
   CASE(13)
   WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFU,JF,JF=1,NFRQ), (QFU,JF,JF=1,NFRQ),  &
                                              (RFA,JF,JF=1,NFRQ), (QFA,JF,JF=1,NFRQ)
   CASE(23)
   WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFV,JF,JF=1,NFRQ), (QFV,JF,JF=1,NFRQ),  &
                                              (RFA,JF,JF=1,NFRQ), (QFA,JF,JF=1,NFRQ)
   CASE(123)
   WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFU,JF,JF=1,NFRQ), (QFU,JF,JF=1,NFRQ), (RFV,JF,JF=1,NFRQ), &
                                              (QFV,JF,JF=1,NFRQ), (RFA,JF,JF=1,NFRQ), (QFA,JF,JF=1,NFRQ)
   END SELECT

 CASE (112)           ! Downhole TD magnetic dipole receiver : N, S, W
   SELECT CASE (CL)
   CASE(1)
     WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHN,JT,JT=1,NCHNL)
   CASE(2)
     WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHS,JT,JT=1,NCHNL)
   CASE(3)
     WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHW,JT,JT=1,NCHNL)
   CASE(12)
     WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHN,JT,JT=1,NCHNL), (CHS,JT,JT=1,NCHNL)
   CASE(13)
     WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHN,JT,JT=1,NCHNL), (CHW,JT,JT=1,NCHNL)
   CASE(23)
     WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHS,JT,JT=1,NCHNL), (CHW,JT,JT=1,NCHNL)
   CASE(123)
     WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHN,JT,JT=1,NCHNL), (CHS,JT,JT=1,NCHNL), (CHW,JT,JT=1,NCHNL)
   END SELECT

 CASE (212)          ! Downhole FD magnetic dipole receiver : N, S, W
   SELECT CASE (CL)
   CASE(1)
   WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFN,JF,JF=1,NFRQ), (QFN,JF,JF=1,NFRQ)
   CASE(2)
   WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFS,JF,JF=1,NFRQ), (QFS,JF,JF=1,NFRQ)
   CASE(3)
   WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFW,JF,JF=1,NFRQ), (QFW,JF,JF=1,NFRQ)
   CASE(12)
   WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFN,JF,JF=1,NFRQ), (QFN,JF,JF=1,NFRQ),  &
                                              (RFS,JF,JF=1,NFRQ), (QFS,JF,JF=1,NFRQ)
   CASE(13)
   WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFN,JF,JF=1,NFRQ), (QFN,JF,JF=1,NFRQ),  &
                                              (RFW,JF,JF=1,NFRQ), (QFW,JF,JF=1,NFRQ)
   CASE(23)
   WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFS,JF,JF=1,NFRQ), (QFS,JF,JF=1,NFRQ),  &
                                              (RFW,JF,JF=1,NFRQ), (QFW,JF,JF=1,NFRQ)
   CASE(123)
   WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFN,JF,JF=1,NFRQ), (QFN,JF,JF=1,NFRQ), (RFS,JF,JF=1,NFRQ), &
                                              (QFS,JF,JF=1,NFRQ), (RFW,JF,JF=1,NFRQ), (QFW,JF,JF=1,NFRQ)
   END SELECT

 CASE (140)           ! TD coincident loop
   WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHC,JT,JT=1,NCHNL)

 CASE (120)          ! TD electric dipole receiver
   WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHE,JT,JT=1,NCHNL)

 CASE (220)          ! FD electric dipole receiver
   WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFE,JF,JF=1,NFRQ), (QFE,JF,JF=1,NFRQ)

 CASE (230)          ! FD point E-field receiver
   SELECT CASE (CL)
   CASE(1)
   WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFX,JF,JF=1,NFRQ), (QFX,JF,JF=1,NFRQ)
   CASE(2)
   WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFY,JF,JF=1,NFRQ), (QFY,JF,JF=1,NFRQ)
   CASE(3)
   WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFZ,JF,JF=1,NFRQ), (QFZ,JF,JF=1,NFRQ)
   CASE(12)
   WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFX,JF,JF=1,NFRQ), (QFX,JF,JF=1,NFRQ),  &
                                              (RFY,JF,JF=1,NFRQ), (QFY,JF,JF=1,NFRQ)
   CASE(13)
   WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFX,JF,JF=1,NFRQ), (QFX,JF,JF=1,NFRQ),  &
                                              (RFZ,JF,JF=1,NFRQ), (QFZ,JF,JF=1,NFRQ)
   CASE(23)
   WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFY,JF,JF=1,NFRQ), (QFY,JF,JF=1,NFRQ),  &
                                              (RFZ,JF,JF=1,NFRQ), (QFZ,JF,JF=1,NFRQ)
   CASE(123)
   WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFX,JF,JF=1,NFRQ), (QFX,JF,JF=1,NFRQ), (RFY,JF,JF=1,NFRQ), &
                                              (QFY,JF,JF=1,NFRQ), (RFZ,JF,JF=1,NFRQ), (QFZ,JF,JF=1,NFRQ)
   END SELECT

 CASE (250)            ! FD Sampo
   WRITE(np,1) TRIM (ADJUSTL(QL)), HID, NCL, (SMP,JF,JF=1,NFRQ)

 CASE (260)            ! FD magnetotellurics : Inphase & Quadrature
   WRITE(np,2) TRIM (ADJUSTL(QL)), HID, (ZXXR,JF,JF=1,NFRQ), (ZXXQ,JF,JF=1,NFRQ), (ZXYR,JF,JF=1,NFRQ), (ZXYQ,JF,JF=1,NFRQ), &
                                         (ZYXR,JF,JF=1,NFRQ), (ZYXQ,JF,JF=1,NFRQ), (ZYYR,JF,JF=1,NFRQ), (ZYYQ,JF,JF=1,NFRQ)

 CASE (261)            ! FD magnetotellurics : Phase and Magnitude
   WRITE(np,2) TRIM (ADJUSTL(QL)), HID, (ZXXP,JF,JF=1,NFRQ), (ZXXM,JF,JF=1,NFRQ), (ZXYP,JF,JF=1,NFRQ), (ZXYM,JF,JF=1,NFRQ), &
                                         (ZYXP,JF,JF=1,NFRQ), (ZYXM,JF,JF=1,NFRQ), (ZYYP,JF,JF=1,NFRQ), (ZYYM,JF,JF=1,NFRQ)

 CASE DEFAULT
   CALL WRITE_LOG_FILE (NLG,35,MXERR,2)   !  Write fatal error message
 END SELECT


 1 FORMAT(T1,'/ LINE_HEADER    LINE_ID ',A,4X,'HID:',I4,4X,'NCTD:',I2 /T1,'/ DatIndx  XLOC  YLOC  ZLOC  ',250(A,I3.3))
 2 FORMAT(T1,'/ LINE_HEADER    LINE_ID ',A,4X,'HID:',I4 /T1,'/ StatIndx  XLOC  YLOC  ZLOC  ',250(A,I3.3))

   END SUBROUTINE WRITE_LINE_HEADER
!==================================================================================================

 END MODULE KG_Input_routines

 PROGRAM MAIN
!------------

!*** Calls FDREAD, HSBOSS, HSBOSS_FRQ, LOKI_3D, SET_SWYTD, TDEM_3D
!          TDEM_3D, WRITE_FD, WRITE_TD, WRITE_LOG_FILE

!*** Calls from KG_Input_routines:
!          READ_READ_SYSTEMY_DATA, READ_MODEL_DATA, SET_TRP, SET_FRQ,

 USE KG_Input_routines

 IMPLICIT NONE
 INTEGER QQDT(8),QQHMS(2)
 COMPLEX, PARAMETER :: ZERO=(0.,0.)
 REAL, ALLOCATABLE :: BPRM(:,:)
 REAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: BTD,BTD_SCAT
 COMPLEX, ALLOCATABLE, DIMENSION(:,:,:,:) :: BFD_SCAT,BFD
 Real :: CMP_Start, CMP_Final, CMP_Delta


 OPEN(NR,FILE = 'Loki.cfl',STATUS = 'OLD')
 OPEN(NW,FILE = 'Loki.out',STATUS = 'REPLACE')

 ! CALL DATE_AND_TIME (DATE, TIME, ZONE, QQDT)
 ! QQHMS(1:2) = QQDT(5:6)
 ! WRITE(*,97) QQHMS(1:2),QQDT(3),MONTH(QQDT(2)),QQDT(1)
 ! WRITE(NW,97) QQHMS(1:2),QQDT(3),MONTH(QQDT(2)),QQDT(1)

 CALL CPU_TIME (CMP_START)

 CALL READ_SYSTEM_AND_SURVEY_DATA    ! Set up system & survey variables
 ALLOCATE (BPRM(MRXTX,NTX))        ! Total primary (air) B fields
 BPRM = 1.                           ! B field normalisation

 CALL READ_MODEL_DATA
 CALL SET_RHO              ! Horizontal interpolation array

! Put arrays in body centred system.

 SELECT CASE (SURVEY_TYPE)

 CASE(1)
   CALL SET_SURVEY_1 (NLINES,MRXL,NTX,MXVRTX,NVRTX,MRXTX,NRX,MQVR,RX_TYPE,IPLT,NCNTRD, &
                      ECNTRD,SXND,SXED,SXN,SXE,SXZ,RXND,RXED,RXN,RXE,RXZ,LNTR,XRXTX,   &
                      YRXTX,ZRXTX,RXID,NCTD,YXZPLT,KNORM,KNORM2)
   DEALLOCATE (SXND,SXED,RXND,RXED)

 CASE(2:3)
   NCTD = 3
   CALL SET_SURVEY_2 (NTX,NTXL,MXVRTX,TXLNGTH,TXWDTH,NLINES,MRXL,NRX,DSTAT,SVAZM,SDN0, &
                      SDE0,SDZ0,MRXTX,XRXOF,YRXOF,ZRXOF,NCNTRD,ECNTRD,IPLT,SXN,SXE,SXZ, &
                      RXN,RXE,RXZ,XRXTX,YRXTX,ZRXTX,YXZPLT,KNORM,KNORM2)
 CASE(4)
   NCTD = 1
   RXID = 4
   CALL SET_SURVEY_4 (NTX,NLINES,TXLNGTH,TXWDTH,MRXL,NRX,DSTAT,SVAZM,NCNTRD,ECNTRD, &
                      SDN0,SDE0,SXN,SXE,YXZPLT)

 CASE(5)
   NCTD = 3
   CALL SET_SURVEY_5 (NTX,NLINES,LNTR,SXND,SXED,SXZ,SXDIP,SXAZM,MRXL,RXOTX,NCNTRD,ECNTRD, &
                      IPLT,SXN,SXE,RXN,RXE,RXZ,XRXTX,YRXTX,ZRXTX,YXZPLT,KNORM,KNORM2)

   DEALLOCATE (SXND,SXED)

 CASE(6)
   NCTD = 3
   CALL SET_SURVEY_6 (NCNTRD,ECNTRD,NHRX,NERX,MRXTX,RXND,RXED,XRXTX,YRXTX,ZRXTX,MTPLT,RXID)
   DEALLOCATE (RXND,RXED)

 END SELECT
 IF (SURVEY_TYPE < 6) DEALLOCATE (DSTAT,SDN0,SDE0,XRXOF,YRXOF)
 IF (SOURCE_TYPE == 1) CALL SET_VERTEX_ORDER (NTX,MXVRTX,NVRTX,SXN,SXE)

 IF (MAXVAL (KNORM) > 0) THEN     !  Set up DC primary fields
   IF (SOURCE_TYPE == 1) THEN
      CALL PRMDC_LP (NTX,MXVRTX,NVRTX,SXN,SXE,MRXTX,NRXTX,MQVR,RXID,XRXTX,YRXTX,ZRXTX,KNORM2,BPRM)
   ELSE IF (SOURCE_TYPE == 3) THEN
     CALL PRMDC_MD (NTX,SXN,SXE,SXZ,SXDIP,SXAZM,MRXTX,NRXTX,RXID,XRXTX,YRXTX,ZRXTX,KNORM2,BPRM)
   END IF
 END IF

 CALL SHOW_AND_TELL        ! Set & Print array & model coordinates in body-centred system
 IF (INVERT) THEN
   OPEN(NRI,FILE = 'Loki.inv',STATUS = 'OLD')
   OPEN(np,FILE = 'Loki.mv1',STATUS = 'REPLACE')
   CALL WRITE_np_INITIAL
   CALL READ_PARAMETER_CONTROL
   CALL READ_INVRT_DATA
!   NSTAT = 1
!   IF (MT) THEN
!     CALL PREPARE_INVRT_MT_DATA
!   ELSE
!     CALL PREPARE_INVRT_DATA
!   END IF
   CLOSE (NR)
 ELSE
   OPEN(np,FILE = 'Loki.mf1',STATUS = 'REPLACE')
   CALL WRITE_np_INITIAL
 END IF

 !============================================================================
 !
 ! Error checkpoint
 Select Case (MXERR)
 Case (0)
    Write (*, 90) Trim(PNAME)
 Case (1)
    Write (*, 91) Trim(PNAME), Trim(PNAME), Trim(PNAME)
 Case (2)
    Write (*, 92) Trim(PNAME)
 End Select
!============================================================================
 IF (ISTOP == 1 .or. MXERR .eq. 2) STOP
!============================================================================

!  For time-domain, set up frequencies, interpolation times

 IF (TDFD < 2) THEN   ! Time-Domain
   CALL SET_SWYTD (NSX,SWX,SWY,T0SX)  ! Compute dI/dt at the receiver
   CALL SET_TRP
   If (do3d .eq. 2) Then
        Call Set_Frq_from_File
   Else
        CALL SET_FRQ
   End If
 END IF

! IF (KHSQ == 1) THEN
!   NFRQHS = NFRQ
!   ALLOCATE (FRQHS(NFRQ))
!   FRQHS(1:NFRQ) = FREQ(1:NFRQ)
! ELSE
!   NFRQHS = NF_6PDE
!   ALLOCATE (FRQHS(NFRQHS))
!   FRQHS(1:NFRQHS) = FRQ_6PDE(1:NFRQHS)
! END IF

 IF (INVERT) THEN
!   NLYR = 1
!   NPLT = 1
!   CALL NLSQ2(NPAR,NDATA,XDATA,RDATA,RWTS,PARPCT,CNVRG,PCTCNV,CXPAR,ELAS,LBND,UBND,MAXITS,NW,  &
!              np,INVPRT,TITLE,SURVEY_TYPE,MT,SOURCE_TYPE,NLINES,MLINES,LINE,HEADER_ID,IPLT,   &
!              MTPLT,YXZPLT,MCMP,CMP,MRXL,MCHNL,NCHNL,NFRQ,NFRQHS,TDFD,STEP,FREQ,FRQHS,NSX,SWX, &
!              SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,TOPN,TCLS,NFT,CURNT,ISYS,SVAZM,UNITS,RX_TYPE,   &
!              IDH,IPM,IDHE,MXTX,NTX,SXN,SXE,SXZ,SXDIP,SXAZM,MXVRTX,NVRTX,NRX,LNTR,NRXTX,MRXTX, &
!              RXID,MQVR,MXRS,XRXTX,YRXTX,ZRXTX,BPRM,MD1,MD2,RXAZM,RXDIP,NERX,NHRX,KNORM2,NCTD, &
!              MXRHO,RHOTRP,NLYR,THK,RES,RMUD,REPS,CHRG,CTAU,CFREQ,NCNTRD,ECNTRD,NPLT,SIG_T,    &
!              PLNGTH,PLWDTH,XCNTR,YCNTR,PLTOP,PLAZM,PLDIP,PLUNJ,CELLW,MXAB,CHRGP,CTAUP,CFREQP)

   CALL NLSQ (NW,np,ND,NS,NM,MV1PRT,OUTPRT,MAXITS,CNVRG,PCTCNV,NDATA, &
              XDATA,XMODL,XWTS,NPAR,CXPAR,ELAS,LBND,UBND,NE,NN,NZ,NAIR,NEL, &
              NER,NNL,NNR,NZB,ELOC,NLOC,ZLOC,LITH,LYTH,NLITH,NPROP,XPART, &
              NPART,STEP,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL, &
              TOPN,TCLS,FREQ,NFRQ,KFRQE,NTX,MRXTX,NRXTX,RXID,NCTD, &
              SOURCE_TYPE,NVRTX,MXVRTX,MQVR,SXN,SXE,SXZ,SXDIP,SXAZM,XRXTX, &
              YRXTX,ZRXTX,NFT,NLINES,MRXL,NRX,LNTR,KNORM2,RX_TYPE,UNITS, &
              ISYS,IDH,SVAZM,MD1,MD2,RXAZM,RXDIP,CURNT,BPRM,TDFD,INRM,DNORM, &
              KCMP,MCHNL,LINE,VSTAT,NSTAT,NEDGE,KACC,SURVEY_TYPE,MXRS)
 ELSE
   KPRT = 0                   ! Print model data only
   IF (DO3D > 0) THEN
     ALLOCATE (BFD_SCAT(NFRQ,MRXTX,NTX,3))
     BFD_SCAT = ZERO
   END IF

   NEW_3D_MODEL: IF (DO3D == 1) THEN

!  The computation assumes unit transmitter and receiver dipoles and MKS units
!  Conversion to user specified units and inclusion of transmitter and receiver
!  dipole moments happens in the output routines.

     JCBN = .FALSE.

     CALL LOKI_3D (ND,NS,NW,NFRQ,FREQ,SOURCE_TYPE,NTX,MXVRTX,NVRTX,SXE,SXN,SXZ, &
                   SXDIP,SXAZM,NRXTX,MRXTX,RXID,MQVR,MXRS,XRXTX,YRXTX,ZRXTX,NE,NN,NZ, &
                   ELOC,NLOC,ZLOC,NEDGE,NAIR,NEL,NER,NNL,NNR,NZB,KACC,LITH,LYTH, &
                   NLITH,NPROP,XPART,NPART,NPAR,JCBN,SURVEY_TYPE)

     CLOSE (NLG)

!  End of frequency stepping.
!  Write out the total frequency-domain scattered magnetic fields for each to UNIT ND.

     IF (TDFD < 2 .AND. DO3D == 1) THEN  ! Time-Domain
!       DO JF = 1,NFRQ
!         DO JS = 1, NTX
!           DO JR = 1, NRXTX(JS)
!             WRITE(ND,'(6E16.6,3X,A,G12.4,2I4)') BFD_SCAT(JF,JR,JS,1:3),'frq, jr, js',FREQ(JF),JR,JS
!           END DO
!         END DO
!       END DO
!
!       CLOSE (ND)
     END IF
   END IF NEW_3D_MODEL

   IF (TDFD == 2) THEN      !  Construct frequency-domain output
!     IF (MT) THEN
!       ALLOCATE (BFD(NFRQ,MRXTX,NTX,3))
!
!       IF (DO3D > 0) CALL FDREAD (ND,NFRQ,NTX,MRXTX,NRXTX,NCTD,BFD_SCAT)  !  Read old frequency-domain data
!       CLOSE (ND)
!
!       CALL HSBOSS_MT (NFRQ,FREQ,NLYR,THKD,RES,RMUD,REPS,CHRG,CTAU,CFREQ,NHRX,NERX,MRXTX,BFD)
!
!       IF (DO3D > 0) BFD = BFD + BFD_SCAT  !  Redefine BFD as the total response.
!
!!       RMU1 = RMU(1)
!       RMU1 = 1.
!       CALL SET_OUTPUT_LINES_MT (NFRQ,NERX,MRXTX,IDHE,RMU1,BFD,IPM,BFTL)
!
!       CALL WRITE_MT (NW,np,KTS,NFRQ,FREQ,NERX,TITLE,MTPLT,IPM,BFTL,RDATA,RWTS)
!
!     ELSE  !  Construct the frequency-domain response (except for MT).

       ALLOCATE ( BFD(NFRQ,MRXTX,NTX,3))
       BFD = ZERO

       IF (DO3D > 0) CALL FDREAD (ND,NFRQ,NTX,MRXTX,NRXTX,NCTD,BFD_SCAT)  !  Read old frequency-domain data
       CLOSE (ND)

!        CALL HSBOSS (NFRQ,FREQ,SOURCE_TYPE,NTX,MXVRTX,NVRTX,SXN,SXE,SXZ,SXDIP,SXAZM, &
!                     NRXTX,MRXTX,RXID,MQVR,XRXTX,YRXTX,ZRXTX,MXRHO,RHOTRP,NLYR,THKD, &
!                     RES,RMUD,REPS,CHRG,CTAU,CFREQ,BFD)

!  Redefine BFD as the total response.
!  Reconfigure output from Tx to Line basis.
!  Perform additional output processing where required

!       IF (DO3D > 0) BFD = BFD + BFD_SCAT
       IF (DO3D > 0) BFD = BFD_SCAT

       CALL SET_OUTPUT_LINES_FD (NFRQ,MCHNL,NTX,MRXTX,NRXTX,NLINES,MRXL,MCMP,NRX,LNTR,KNORM2,RX_TYPE, &
                                 UNITS,ISYS,IDH,SVAZM,MD1,MD2,RXAZM,RXDIP,CURNT,BPRM,BFD,BFTL)

       CALL WRITE_FD (NW,np,KTS,NFRQ,MCHNL,NLINES,MRXL,MCMP,NRX,SURVEY_TYPE,LINE,IDH,RX_TYPE,UNITS, &
                      SVAZM,TITLE,ISYS,PRFL,IPLT,YXZPLT,FREQ,HEADER_ID,CMP,BFTL,RDATA,RWTS)

       IF (PRTSEC) THEN
         WRITE(NW,11)
         WRITE(np,12)
         CALL SET_OUTPUT_LINES_FD (NFRQ,MCHNL,NTX,MRXTX,NRXTX,NLINES,MRXL,MCMP,NRX,LNTR,KNORM2,RX_TYPE, &
                                   UNITS,ISYS,IDH,SVAZM,MD1,MD2,RXAZM,RXDIP,CURNT,BPRM,BFD_SCAT,BFTL)

         LINE = LINE + 900000000
         CALL WRITE_FD (NW,np,KTS,NFRQ,MCHNL,NLINES,MRXL,MCMP,NRX,SURVEY_TYPE,LINE,IDH,RX_TYPE,UNITS, &
                        SVAZM,TITLE,ISYS,PRFL,IPLT,YXZPLT,FREQ,HEADER_ID,CMP,BFTL,RDATA,RWTS)

       END IF
!     END IF

   ELSE IF (TDFD < 2) THEN   ! Time-Domain.  Compute BTD_SCAT, the Scattering Response

     ALLOCATE ( BTD(NCHNL,MRXTX,NTX,3), BTD_SCAT(NCHNL,MRXTX,NTX,3))
     BTD = 0.
     BTD_SCAT = 0.

     IF (DO3D > 0) THEN
!       IF (DO3D == 2) THEN
         CALL FDREAD (ND,NFRQ,NTX,MRXTX,NRXTX,NCTD,BFD_SCAT)  !  Read old frequency-domain data
         CLOSE (ND)
!       END IF

       CALL TDEM_3D (STEP,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL,TOPN,TCLS, &
                     FREQ,NFRQ,NTX,MRXTX,NRXTX,RXID,NCTD,BFD_SCAT,BTD_SCAT)

       DEALLOCATE (BFD_SCAT)
     END IF

!  Compute BTD, the layered earth response convolved with the excitation waveform

!     CALL HSBOSS_TD (NFRQHS,FRQHS,STEP,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL, &
!                     TOPN,TCLS,SOURCE_TYPE,NTX,MXVRTX,NVRTX,SXN,SXE,SXZ,SXDIP,SXAZM,   &
!                     NRXTX,RXID,MRXTX,MQVR,XRXTX,YRXTX,ZRXTX,MXRHO,RHOTRP,NLYR,THKD,   &
!                     RES,RMUD,REPS,CHRG,CTAU,CFREQ,NCTD,BTD)

!  Redefine BTD as the total response.
!  Reconfigure output from Tx to Line basis.
!  Perform additional output processing where required

!     IF (DO3D > 0) BTD = BTD + BTD_SCAT
     IF (DO3D > 0) BTD = BTD_SCAT

     CALL SET_OUTPUT_LINES_TD (NCHNL,NTX,MRXTX,NRXTX,NLINES,MRXL,MCMP,NRX,LNTR,KNORM2,RX_TYPE, &
                               UNITS,ISYS,IDH,SVAZM,MD1,MD2,RXAZM,RXDIP,CURNT,BPRM,BTD,BFTL)

!  Write out the results.

     CALL WRITE_TD (NW,np,KTS,NCHNL,NLINES,MRXL,MCMP,NRX,SURVEY_TYPE,LINE,IDH,RX_TYPE,UNITS, &
                    SVAZM,TITLE,ISYS,PRFL,IPLT,YXZPLT,TMS,HEADER_ID,CMP,BFTL,RDATA,RWTS)

     IF (PRTSEC) THEN
       WRITE(NW,11)
       WRITE(np,12)
       CALL SET_OUTPUT_LINES_TD (NCHNL,NTX,MRXTX,NRXTX,NLINES,MRXL,MCMP,NRX,LNTR,KNORM2,RX_TYPE, &
                                 UNITS,ISYS,IDH,SVAZM,MD1,MD2,RXAZM,RXDIP,CURNT,BPRM,BTD_SCAT,BFTL)

       LINE = LINE + 900000000
       CALL WRITE_TD (NW,np,KTS,NCHNL,NLINES,MRXL,MCMP,NRX,SURVEY_TYPE,LINE,IDH,RX_TYPE,UNITS, &
                      SVAZM,TITLE,ISYS,PRFL,IPLT,YXZPLT,TMS,HEADER_ID,CMP,BFTL,RDATA,RWTS)
     END IF

   END IF
 END IF

 CALL CPU_TIME (CMP_Final)
 Call date_and_time(Values = tvals)
 CMP_Delta = CMP_Final - CMP_Start
 Select Case (Invert)
 Case (.True.)
    Write (np, 11) trim(PNAME), 'inversion', tvals(5:7), tvals(3:1:-1), CMP_delta
    Write (nw, 12) trim(PNAME), 'inversion', tvals(5:7), tvals(3:1:-1), CMP_delta
    Write ( *, 12) trim(PNAME), 'inversion', tvals(5:7), tvals(3:1:-1), CMP_delta
 Case (.False.)
    Write (np, 11) trim(PNAME), 'forward model', tvals(5:7), tvals(3:1:-1), CMP_delta
    Write (nw, 12) trim(PNAME), 'forward model', tvals(5:7), tvals(3:1:-1), CMP_delta
    Write ( *, 12) trim(PNAME), 'forward model', tvals(5:7), tvals(3:1:-1), CMP_delta
 End Select

 CLOSE (NW)
 CLOSE (NLG)

 STOP
 ! 11 FORMAT(//T3,'*************************************' &
 !           //T3,'      SCATTERED FIELD OUTPUT'          &
 !           //T3,'*************************************')
 ! 12 FORMAT(T1,'/'/T1,'/ SCATTERED FIELDS'/'/')
!
! Formats
10 Format (/, 2x, 'Frequency-domain calculations finished ...', &
           /, 2x, 'Starting convolution for time-domain calculations ...')
11  Format ('/', / &
            '/ ', a, ' ', a, ' run completed: ', i4.4, '-', i2.2, '-', i2.2, 'T', i2.2, ':', i2.2, ':', i2.2, / &
            '/ Runtime: ', f12.2, ' secs')
12  Format (/, 2x, a, ' ', a, ' run completed: ', i4.4, '-', i2.2, '-', i2.2, 'T', i2.2, ':', i2.2, ':', i2.2, &
		    /, 2x, 'Runtime: ', f12.2, ' seconds', /)
90 Format (/, 2x, 'Completed sanity check on entries in ', a, '.cfl ...', &
           /, 2x, 'Computation begining ...')
91 Format (/, 2x, 'WARNING', &
           /, 2x, a, '.cfl may contain errors. Please check ', a, '.log and ', a, '.out')
92 Format (/, 2x, 'FATAL ERROR', &
           /, 2x, a, '.cfl contains errors. Please correct these before restarting.')

 END PROGRAM MAIN

 REAL FUNCTION COSTRN (WF,YFRQ,NFRQ,T)
!-------------------------------------

!***  Calls CUBVAL
!***  Called by TDEM_3D

! LAST MODIFICATION DATE: October, 2001

! Produces time-domain value at time T by cosine transformation of NFRQ
! frequency-domain values contained in cubic spline array YFRQ.
! Array WF contains the LOG (base e) of the angular frequency values.

! The routine uses filter coefficients derived from the Niels Christensen
! fast Hankel transform routine FILCOA at a spacing of 12 points per decade
! and omega = 0.3.  Various filters were tested using a vertical magnetic
! dipole receiver in a very large circular for which accurate frequency
! and time-domain solutions were programmed.  This particular filter gave
! the overall best accuracy for 1/2 spaces ranging in resistivity from
! .1 to 10,000 ohm-m for times ranging from .01 to 50 msec.


!  K(W,T) = (2/PI) * F(W) * COS(WT) dW

! Letting X = WT, the above becomes
!
!  K(W,T) = (2/PI*T) * F(X/T) * COS(X) dX
!
! From Abramowitz and Stegun, COS(X) = SQRT(X*PI/2) * J(-1/2:X).
! Filter Coefficients are used to represent X**(1/2) * J(-1/2:X)
!
!  COSTRN = SQRT (2/PI) * SUM(i) { WCOS(i) * F [X(i) /T] }

! The accumulation is done using 12 digit precision


 USE KG_Filter_coefficients

 IMPLICIT NONE
 INTEGER, PARAMETER :: NDEC_COS=12, KFLOW=-200, KFHIGH=99
 REAL, PARAMETER :: FAC=.7978846, TOL=1.0E-6
 INTEGER J1,NFRQ
 REAL WF(NFRQ),YFRQ(4,NFRQ),T,YS,CUBVAL,V1
 REAL(KIND=QL) DELTA,Y1,Y,TD,YTYM,VAL

 INTENT (IN) WF,YFRQ,NFRQ,T


 DELTA = LOG (10._QL)/ REAL (NDEC_COS, KIND=QL)
 TD = REAL (T, KIND=QL)
 YTYM = 0.
 Y1 = -LOG (TD) -DELCOS

! Begin right side convolution at weight 0.
! Stop when frequency domain array is exhausted.

 MOVE_HIGH: DO J1 = 0, KFHIGH

   Y = Y1 + J1 * DELTA
   YS = REAL(Y)
   IF (YS > WF(NFRQ)) EXIT MOVE_HIGH
   IF (YS < WF(1)) YS = WF(1)
   V1 = CUBVAL (WF,YFRQ,NFRQ,YS)
   VAL = WCOS(J1) * REAL (V1,KIND=QL)
   YTYM = YTYM + VAL
 END DO MOVE_HIGH

 Y = Y1

! Begin left side convolution at weight -1.
! When log angular frequency is less than WF(3), check convergence.
! Continue left using the fact that impulse B is inversely proportional to
! frequency as freq -> 0; i.e., step response B is constant.

 MOVE_LOW: DO J1 = -1, KFLOW, -1

   Y = Y1 + J1 * DELTA
   YS = REAL(Y)
   IF (YS > WF(NFRQ)) CYCLE
   IF (YS < WF(1)) YS = WF(1)
   V1 = CUBVAL (WF,YFRQ,NFRQ,YS)
   VAL = WCOS(J1) * REAL (V1,KIND=QL)
   YTYM = YTYM + VAL
   IF ((Y < WF(3))) THEN
     IF (ABS (VAL) < TOL * ABS (YTYM)) EXIT MOVE_LOW
   END IF
 END DO MOVE_LOW

 COSTRN = FAC * REAL (YTYM) / T

 END FUNCTION COSTRN

  REAL FUNCTION CUBINT (X_ARRAY, Y_VAL, NVAL, X1, X2)
! ---------------------------------------------------

!  Integrates a function from X1 to X2 using its cubic spline representation.

!***  Called by  TXCNVD, TXCNVL
!***  Calls INTERV.  On exit from INTERV
!
!       MFLAG = -1  : X is to the left of interpolated range
!             =  1  : X is to the right of interpolated range
!             =  0  : X is in the interpolated range

!      NVAL - total number of knots including endpoints.
!
!     X_ARRAY(I), I = 1,NVAL - Location of the X coordinate for each known Y value.
!                              The rightmost data point used to calculate coefficients
!                              is not included.
!
!     Y_VAL(J,I), J = 1,4; I = 1,NVAL
!
!              The coefficients of the cubic spline represent the
!              indefinite integral of F, on the I'th interval, as:
!
!       INTGR [ F(X) ] = Y_VAL(4,I)/24 * H**4  +  Y_VAL(3,I)/6 * H**3  +
!                        Y_VAL(2,I)/2 * H**2  +  Y_VAL(1,I) * H
!
!                          WITH  H = X - X_ARRAY(K)
!
!  This is a modification of the FUNCTION PPVALU in the book
!  "A PRACTICAL GUIDE TO SPLINES"  by C. DE BOOR

!*********************************************************************

  IMPLICIT NONE
  INTEGER I,I1,I2,MFLAG,NVAL
  REAL H,H1,H2,X1,X2,X_ARRAY(NVAL), Y_VAL(4,NVAL)

!  Find the indices I1 and I2 of largest breakpoints to the left of X1
!  and X2 respectively.
!
  CALL INTERV ( X_ARRAY, NVAL-1, X1, I1, MFLAG )
  CALL INTERV ( X_ARRAY, NVAL-1, X2, I2, MFLAG )
  H1 = X1 - X_ARRAY(I1)
  IF (MFLAG == -1) H1 = 0.

  H2 = X2 - X_ARRAY(I2)
  CUBINT = (((Y_VAL(4,I2)*H2/4.0 + Y_VAL(3,I2) )*H2/3.0 + &
              Y_VAL(2,I2) )*H2/2.0 + Y_VAL(1,I2) )*H2 &
         - (((Y_VAL(4,I1)*H1/4.0 + Y_VAL(3,I1) )*H1/3.0 + &
              Y_VAL(2,I1) )*H1/2.0 + Y_VAL(1,I1) )*H1

!  Include integrals over intervening intervals.

  IF (I2 > I1) THEN
    DO I = I1, I2-1
      H = X_ARRAY(I+1) - X_ARRAY(I)
      CUBINT = CUBINT + (((Y_VAL(4,I)*H/4.0 + Y_VAL(3,I) )*H/3.0 + &
                           Y_VAL(2,I) )*H/2.0 + Y_VAL(1,I) )*H
    END DO
  END IF

 END FUNCTION CUBINT

  SUBROUTINE CUBSPL (XVAL, F, N)
! ------------------------------

!***  Called by FOLD_AND_CONVOLVE, READ_SYSTEM_AND_LAYER_DATA, TXCNVD

!  Calculates coefficients for cubic spline interpolation.
!  Call function CUBVAL to evaluate function values after interpolation.
!  Adapted from "A Practical Guide to Splines"  by Carl de Boor.

!             INPUT
!             -----
!
!     N = number of data points. assumed to be at least 4
!
!  (XVAL(I), F(1,I), I=1,...,N) = abscissae and ordinates of the data points.
!                                 XVAL is assumed to be strictly increasing.
!
!          OUTPUT
!          ------
!
!     F(J,I), J=1,...,4; I=1,. N-1 = the polynomial coefficients
!         of the cubic interpolating spline with interior knots (or joints)
!         XVAL(2), ..., XVAL(N-1).
!
!        In the interval: (XVAL(I) - XVAL(I+1)), the spline F is given by:
!
!        F(X) = F(1,I) + H* (F(2,I) + H* (F(3,I) + H* F(4,I)/3.) /2.)
!
!     where H = X - XVAL(I).  FUNCTION  CUBVAL of it s variations may be
!     used to evaluate F or its derivatives from XVAL,C, L = N-1, & K=4.
!------------------------------------------------------------------------

  IMPLICIT NONE
  INTEGER N,I,J,M
  REAL F(4,N),XVAL(N),DIVDF1,DIVDF3,DXVAL,G

!  A tridiagonal linear system for the unknown slopes S(I) of F at
!  XVAL(I), I=1,...,N, is generated and then solved by Gauss elimination,
!  with S(I) ending up in F(2,I), ALL I.
!  F(3,.) AND F(4,.) are used initially for temporary storage.

!  Compute first differences of XVAL sequence and store in F(3,.).
!  Also, compute first divided difference of data and store in F(4,.).

  DO M = 2,N
    F(3,M) = XVAL(M) - XVAL(M-1)
    F(4,M) = (F(1,M) - F(1,M-1)) /F(3,M)
  END DO

!  Not-a-knot condition at left

 F(4,1) = F(3,3)
 F(3,1) = F(3,2) + F(3,3)
 F(2,1) = ((F(3,2) + 2.* F(3,1)) * F(4,2)*F(3,3) + F(3,2)**2 * F(4,3)) /F(3,1)

!  Generate the corresponding equations and
!  perform the forward pass of Gauss elimination, after which the M-TH
!  equation reads    F(4,M)*S(M) + F(3,M)*S(M+1) = F(2,M).

 DO M = 2, N-1
   G = -F(3,M+1) / F(4,M-1)
   F(2,M) = G*F(2,M-1) + 3.* (F(3,M)*F(4,M+1) + F(3,M+1)*F(4,M))
   F(4,M) = G* F(3,M-1) + 2.* (F(3,M) + F(3,M+1))
 END DO

 G = F(3,N-1) + F(3,N)
 F(2,N) = ((F(3,N) + 2.*G) *F(4,N)*F(3,N-1) + F(3,N)**2 *(F(1,N-1) - F(1,N-2)) /F(3,N-1))/G
 G = -G / F(4,N-1)
 F(4,N) = F(3,N-1)


 F(4,N) = G*F(3,N-1) + F(4,N)
 F(2,N) = (G*F(2,N-1) + F(2,N)) /F(4,N)

!  Perform back substitution.

 DO J = N-1, 1, -1
   F(2,J) = (F(2,J) - F(3,J) *F(2,J+1)) /F(4,J)
 END DO

!  Generate cubic coefficients in each interval, i.e., the derivatives at its
!  left endpoint, from value and slope at its endpoints.

 DO I = 2,N
   DXVAL = F(3,I)
   DIVDF1 = (F(1,I) - F(1,I-1)) /DXVAL
   DIVDF3 = F(2,I - 1) + F(2,I) - 2.*DIVDF1
   F(3,I-1) = 2.* (DIVDF1 - F(2,I-1) - DIVDF3) /DXVAL
   F(4,I-1) = (DIVDF3/DXVAL) * (6./DXVAL)
 END DO
 END SUBROUTINE CUBSPL

 REAL FUNCTION CUBVAL (X_ARRAY, Y_VAL, NXVAL, X1)
!------------------------------------------------

!  Evaluates a function at X1 from from its cubic spline representation.

!***  Called by COSTRN, FOLD_AND_CONVOLVE, TXCNVD, TXCNVL
!***  Calls INTERV.  On exit from INTERV
!
!       MFLAG = -1  : X is to the left of interpolated range
!             =  1  : X is to the right of interpolated range
!             =  0  : X is in the interpolated range

!      NXVAL - total number of knots including endpoints.
!
!     X_ARRAY(I), I = 1,NXVAL - location of the knots.  The rightmost data
!                            point used to calculate coefficients is not
!                            included.
!
!     Y_VAL(J,I), J = 1,4; I = 1,NXVAL
!
! The coefficients of the cubic spline on the I'th interval represent F as:
!
!                F(X) = Y_VAL(4,I)/6 * H**3  +  Y_VAL(3,I)/2 * H**2  +
!                       Y_VAL(2,I) * H  +  Y_VAL(1,I)
!
!                          with  H = X - X_ARRAY(I)
!
!  This is a modification of the FUNCTION PPVALU in the book
!  "A PRACTICAL GUIDE TO SPLINES"  by C. DE Boor
!
!             METHOD
!             ------
!
!  The interval index I, appropriate for X, is found through a call to INTERV.
!  The formula for F is evaluated using nested multiplication.
!******************************************************************************

 IMPLICIT NONE
 INTEGER I,MFLAG,NXVAL
 REAL X_ARRAY(NXVAL),Y_VAL(4,NXVAL),X1,H

 INTENT (IN) X_ARRAY, Y_VAL, NXVAL, X1

!  Find index I of largest breakpoint to the left of X1.

 CALL INTERV ( X_ARRAY, NXVAL-1, X1, I, MFLAG )
 H = X1 - X_ARRAY(I)
 IF (MFLAG == -1) H = 0.
 CUBVAL = ((Y_VAL(4,I)*H/3.0 + Y_VAL(3,I) )*0.5*H + Y_VAL(2,I) )*H + Y_VAL(1,I)

 END FUNCTION CUBVAL

 REAL FUNCTION DIST2D (X1,Y1,X2,Y2)
!----------------------------------

! Computes distance RHO between points (X1, Y1) & (X2, Y2)

 REAL X1,Y1,X2,Y2

 DIST2D = SQRT ((X1-X2)**2 + (Y1-Y2)**2)

 END FUNCTION DIST2D

 SUBROUTINE FDREAD (ND,NFRQ,NTX,MRXTX,NRXTX,NCTD,BFD_SCAT)
!--------------------------------------------------------

!  Reads frequency-domain scattered impulse magnetic field data
!  (real & imaginary components) from logical UNIT ND into
!  array BFD_SCAT for conversion to time-domain by SUBROUTINE TDEM_OUT.

!*** Called by MAIN

!            NFRQ - number of frequencies
!             NTX - number of transmitter positions
!           MRXTX - maximum number of receivers for any transmitter
!           NRXTX - number of receivers for each transmitter position
!            NCTD - number of components for each receiver
!   BFD1(I,J,K,L) - Lth component of the complex frequency-domain response at
!                   receiver J, of transmitter K for frequency I.  If NCTD = 1
!                   as is the case for coincident loop, or electric dipole,
!                   then the relevant component is stored in L=1 position.

  IMPLICIT NONE
  INTEGER ND,NFRQ,NTX,MRXTX,NRXTX(NTX),NCTD(MRXTX,NTX),JF,JS,JR,JC,NC,NC2
  REAL A(6)
  COMPLEX BFD_SCAT(NFRQ,MRXTX,NTX,3)

  REWIND(ND)

  DO JF = 1,NFRQ
    DO JS = 1, NTX
      DO JR = 1, NRXTX(JS)
        NC = NCTD(JR,JS)
        NC2 = 2*NC
        READ(ND,*) A(1:NC2)
        DO JC = 1,NC
          BFD_SCAT(JF,JR,JS,JC) = CMPLX (A(2*JC-1), A(2*JC)) * 12.56637E-7
        END DO
      END DO
    END DO
  END DO
 END SUBROUTINE FDREAD

 SUBROUTINE GET_SOURCE_TEXT (J,SXTXT)
!-----------------------------------

 INTEGER J
 CHARACTER(LEN=20) SXTXT

 SELECT CASE (J)
 CASE (1)
   SXTXT = 'CLOSED_LOOP'
 CASE (2)
   SXTXT = 'GROUNDED_OPEN_LOOP'
 CASE (3)
   SXTXT = 'MAGNETIC_DIPOLE'
 CASE (4)
   SXTXT = 'RECTANGULAR_LOOP'
 CASE (5)
   SXTXT = 'PLANE_WAVE'
 END SELECT
 END SUBROUTINE GET_SOURCE_TEXT

 SUBROUTINE GET_SURVEY_TEXT (J,SVTXT)
!-----------------------------------

 INTEGER J
 CHARACTER(LEN=20) SVTXT

 SELECT CASE (J)
 CASE (1)
   SVTXT = 'FIXED_SOURCE'
 CASE (2)
   SVTXT = 'MOVING_LOOP'
 CASE (3)
   SVTXT = 'MOVING_DIPOLE'
 CASE (4)
   SVTXT = 'COINCIDENT_LOOP'
 CASE (5)
   SVTXT = 'DRILL_HOLE_PROBE'
 CASE (6)
   SVTXT = 'MAGNETOTELLURICS'
 END SELECT
 END SUBROUTINE GET_SURVEY_TEXT

 SUBROUTINE GET_UNITS_TEXT (J,UTXT)
!----------------------------------

 INTEGER J
 CHARACTER(LEN=20) UTXT(2)

 SELECT CASE (J)
 CASE (1)
   UTXT(1) = 'volts'
   UTXT(2)  = 'V'
 CASE (2)
   UTXT(1) = 'millivolts'
   UTXT(2) = 'mV'
 CASE (3)
   UTXT(1) = 'microvolts'
   UTXT(2) = 'mu-V'
 CASE (4)
   UTXT(1) = 'nanovolts'
   UTXT(2) = 'nV'
 CASE (11)
   UTXT(1) = 'nanoteslas / sec'
   UTXT(2) = 'nT/s'
 CASE (12)
   UTXT(1) = 'picoteslas / sec'
   UTXT(2) = 'pT/s'
 CASE (21)
   UTXT(1) = 'nanoteslas'
   UTXT(2) = 'nT'
 CASE (22)
   UTXT(1) = 'picoteslas'
   UTXT(2) = 'pT'
 CASE (31)
   UTXT(1) = 'ratio'
   UTXT(2) = 'ratio'
 CASE (32)
   UTXT(1) = 'percent'
   UTXT(2) = 'percent'
 CASE (33)
   UTXT(1) = 'parts per thousand'
   UTXT(2) = 'PPT'
 CASE (34)
   UTXT(1) = 'parts per million'
   UTXT(2) = 'PPM'
 CASE (35)
   UTXT(1) = 'parts per billion'
   UTXT(2) = 'PPB'
 CASE (41)
   UTXT(1) = 'volts per metre'
   UTXT(2) = 'V/m'
 CASE (42)
   UTXT(1) = 'millivolts per metre'
   UTXT(2) = 'mV/m'
 CASE (43)
   UTXT(1) = 'microvolts per metre'
   UTXT(2) = 'mV/m'
 CASE (44)
   UTXT(1) = 'nanovolts per metre'
   UTXT(2) = 'nV/m'
 END SELECT
 END SUBROUTINE GET_UNITS_TEXT

   SUBROUTINE INTERV (XT, LXT, X, LEFT, MFLAG)
!-------------------------------------------

!***   Called by CUBVAL, CUBINT
!       INPUT: XT, LXT, X
!      OUTPUT: LEFT, MFLAG
!
!  XT is an ascending sequence of length LXT.
!  INTERV finds the interval in XT containing the input variable X.
!
!  If XT(1)  <=  X  <=  XT(LXT) then MFLAG = 0 and
!
!  XT (LEFT)  <=  X  <=  XT (LEFT+1)
!
!  --------------------------------------------
!  If  X <  XT(1) then MFLAG = - 1 & LEFT = 1
!  If  X >  XT(lxt) then MFLAG = 1 & LEFT = LXT
!  --------------------------------------------
!
!  from  * A PRACTICAL GUIDE TO SPLINES *  by C. DE BOOR
!
!             METHOD
!             ------
!
!  The program is designed to be efficient in the common situation that
!  it is called repeatedly, with  X  taken from an increasing or decreasing
!  sequence. This will happen, e.g., when a pp function is to be grapged.
!  The first guess for  LEFT  is therefore taken to be the value returned at
!  the previous call and stored in the  L O C A L  variable ILO. A first
!  check ascertains that  ILO < LXT (This is necessary since the present
!  call may have nothing to do with the previous call).
!  Then, if XT(ILO) <= XT(ILO+1),
!  we set  LEFT = ILO  and are done after just three comparisons.
!  Otherwise, we repeatedly double the difference  ISTEP = IHI - ILO
!  while also moving  ILO  AND  IHI  in the direction of  X , until
!                      XT(ILO) <= X < XT(IHI) ,
!  after which we use bisection to get, in addition, ILO+1 = IHI .
!  LEFT = ILO  is then returned.
!******************************************************************************
!******************************************************************************

  IMPLICIT NONE
  INTEGER LEFT,LXT,MFLAG,IHI,ILO,ISTEP,MIDDLE,J1
  REAL X,XT(LXT)
  SAVE ILO

  DATA ILO /1/

!***********************************************************
!  Trivial returns when X is not in the range.

  IF ( (X <= XT(1)) .OR. (LXT <= 1) ) THEN
    MFLAG = -1
    LEFT = 1
    RETURN
  END IF

  IF (X >= XT(LXT)) THEN
    MFLAG = 1
    LEFT = LXT
    RETURN
  END IF

  MFLAG = 0
  IF (ILO >= LXT) ILO = LXT-1
  IHI = ILO + 1

!  Trivial return when X is already in the interval.

  IF ( (X <= XT(IHI)) .AND. (X >= XT(ILO)) ) THEN
    LEFT = ILO
    RETURN
  END IF
!***********************************************************

  IF (X <= XT(ILO)) THEN  ! decrease ILO  to capture X.
    ISTEP = 1
    DO J1 = 1,LXT
      IHI = ILO
      ILO = IHI - ISTEP
      ILO = MAX(1, ILO)
      IF ( (X >= XT(ILO)) .OR. (ILO == 1) ) EXIT
      ISTEP = ISTEP*2
    END DO

  ELSE IF ( X >= XT(IHI)) THEN  ! increase IHI to capture X

    ISTEP = 1
    DO J1 = 1,LXT
      ILO = IHI
      IHI = ILO + ISTEP
      IHI = MIN (IHI,LXT)
      IF ( (X <= XT(IHI)) .OR. (IHI == LXT) ) EXIT
      ISTEP = ISTEP*2
    END DO

  END IF

!  Now XT(ILO) <= X < XT(IHI) . Narrow the interval.

  DO J1 = 1,LXT
    MIDDLE = (ILO + IHI)/2
    IF (MIDDLE == ILO) EXIT
    IF (X < XT(MIDDLE)) THEN
      IHI = MIDDLE
    ELSE
      ILO = MIDDLE
    END IF
  END DO

! Task complete

  LEFT = ILO
  RETURN

 END SUBROUTINE INTERV

 SUBROUTINE PRMDC_LP (NTX,MXVRTX,NVRTX,SXN,SXE,MRXTX,NRXTX,MQVR,RXID,XRXTX,YRXTX,ZRXTX,KNORM2,BPRM)
!--------------------------------------------------------------------------------------------------

! Returns the primary DC magnetic field in Teslas due to polygonal loop lying on the
! surface of a flat earth at z = 0.  It accumulates BPRM segment by segment
! by rotating each segment in turn to lie along the transformed X axis.

!               Input
!               -----
!
!  NTX                    = number of transmitter loops
!  MXVRTX                 = maximum number of vertices
!  NVRTX(JS)              = number of vertices for transmitter JS
!  SXN, SXE (JV,JS)       : northing and easting of vertex JV of Tx JS
!  MRXTX                  = maximum number of receivers per transmitter
!  NRXTX(JS)              = number of receivers for transmitter JS
!  MQVR - maximum number of vertices for all receivers
!  XRXTX, YRXTX (JR,JS,1) : northing and easting of Rx JR of Tx JS
!  ZRXTX(JR,JS)           : depth of Rx JR of Tx JS (positive down)
!  KNORM2(MRXTX,NTX)      : normalisation indicator for receiver JR of transmitter JS
!
!               Output
!               ------
!
!  KNORM = 0 :  BPRM (JR,JS) = 1 Tesla
!  KNORM = 1 :  BPRM (JR,JS) = Total DC Field
!  KNORM = 2 :  BPRM (JR,JS) = Vertical DC Field


 IMPLICIT NONE
 REAL, PARAMETER ::  FAC = 1.0E-7       !  MU0 / 4 PI
 INTEGER NTX,MRXTX,NRXTX(NTX),MQVR,RXID(MRXTX,NTX),MXVRTX,NVRTX(NTX),KNORM2(MRXTX,NTX),JS,JR,JV
 REAL X0,Y0,Z0,X1,Y1,X2,Y2,LNGTH,CJ,SJ,XI,ETA,RHOSQ,LXI,B1, &
      ZRXTX(MRXTX,NTX),BP(4),BPRM(MRXTX,NTX)
 REAL, DIMENSION (MXVRTX,NTX) :: SXN,SXE
 REAL, DIMENSION (MRXTX,NTX,MQVR) :: XRXTX,YRXTX

 BPRM = 1.
 DO JS = 1,NTX
   DO JR = 1,NRXTX(JS)
     IF (RXID(JR,JS) > 1) CYCLE
     BP = 0.
     X0 = XRXTX(JR,JS,1)
     Y0 = YRXTX(JR,JS,1)
     Z0 = ZRXTX(JR,JS)
     DO JV = 1,NVRTX(JS)
       X1 = SXN(JV,JS);  Y1 = SXE(JV,JS)
       IF (JV == NVRTX(JS)) THEN
         X2 = SXN(1,JS);  Y2 = SXE(1,JS)
       ELSE
         X2 = SXN(JV+1,JS);  Y2 = SXE(JV+1,JS)
       END IF

       LNGTH = SQRT ((X2-X1)**2 + (Y2-Y1)**2)
       CJ = (X2 - X1) / LNGTH
       SJ = (Y2 - Y1) / LNGTH
       XI =   (X0 - X1) * CJ + (Y0 - Y1) * SJ   ! Transformed Rx position
       ETA = -(X0 - X1) * SJ + (Y0 - Y1) * CJ
       RHOSQ = ETA**2 + Z0 **2
       LXI = LNGTH - XI
       B1 = (LXI / SQRT (LXI**2 + RHOSQ)) + (XI / SQRT (XI**2 + RHOSQ))
       B1 = FAC * B1 / RHOSQ
       BP(1) = BP(1) + B1 * Z0 * SJ
       BP(2) = BP(2) - B1 * Z0 * CJ
       BP(3) = BP(3) + B1 * ETA
       BP(4) = B1 * SQRT (ETA**2 + Z0**2)
     END DO
     IF (KNORM2(JR,JS) == 1) THEN
       BPRM(JR,JS) = BP(4)                  ! Normalise to TOTAL field
     ELSE IF (KNORM2(JR,JS) == 2) THEN
       BPRM(JR,JS) = ABS (BP(3))            ! Normalise to VERTICAL field
     END IF
   END DO
 END DO
 END  SUBROUTINE PRMDC_LP

 SUBROUTINE PRMDC_MD (NTX,SXN,SXE,SXZ,SXDIP,SXAZM,MRXTX,NRXTX,RXID,XRXTX,YRXTX,ZRXTX,KNORM2,BPRM)
!------------------------------------------------------------------------------------------------

!***  Called by MAIN

! Returns the primary DC magnetic field in Teslas due to magnetic dipole transmitter of
! unit moment at each receiver position.
!
!               Input
!               -----
!
!  NTX                    = number of transmitter loops
!  SXN, SXE, SXZ          : Tx northing, easting and depth
!  SXDIP, SXAZM           : Tx dip and azimuth
!  MRXTX                  = maximum number of receivers per transmitter
!  NRXTX(JS)              = number of receivers for transmitter JS
!  XRXTX, YRXTX (JR,JS,1) : northing and easting of Rx JR of Tx JS
!  ZRXTX(JR,JS)           : depth of Rx JR of Tx JS (positive down)
!  KNORM2(MRXTX,NTX)      : normalisation indicator for receiver JR of transmitter JS
!
!               Output
!               ------
!
!  KNORM = 0 :  BPRM (JR,JS) = 1 Tesla
!  KNORM = 1 :  BPRM (JR,JS) = Total DC Field in Teslas (absolute value)
!  KNORM = 2 :  BPRM (JR,JS) = Axial DC Field in Teslas (absolute value)

 IMPLICIT NONE
 REAL, PARAMETER ::  BFAC = 1.0E-7       !  MU0 / 4 PI
 INTEGER NTX,MRXTX,RXID(MRXTX,NTX),NRXTX(NTX),KNORM2(MRXTX,NTX),JS,JR
 REAL ZRXTX(MRXTX,NTX),BPRM(MRXTX,NTX),SNDP,CSDP,SNAZ,CSAZ,X(3),RSQ,R,ZBAR3
 REAL, DIMENSION (NTX) :: SXZ,SXAZM,SXDIP
 REAL, DIMENSION (1,NTX) :: SXN,SXE
 REAL, DIMENSION (MRXTX,NTX,1) :: XRXTX,YRXTX

 BPRM = 1.
 DO JS = 1,NTX
   SNDP = SIN (SXDIP(JS))
   CSDP = COS (SXDIP(JS))
   SNAZ = SIN (SXAZM(JS))
   CSAZ = COS (SXAZM(JS))

   DO JR = 1,NRXTX(JS)
     IF (RXID(JR,JS) > 1) CYCLE
     X(1) = XRXTX(JR,JS,1) - SXN (1,JS)
     X(2) = YRXTX(JR,JS,1) - SXE (1,JS)
     X(3) = ZRXTX(JR,JS)   - SXZ (JS)

     RSQ = X(1)**2 + X(2)**2 + X(3)**2
     R = SQRT (RSQ)
!  Rotate to coordinate system where new Z axis lies along dipole axis.

     X(1) = X(1) * CSAZ + X(2) * SNAZ
     X(3) = X(3) * CSDP + X(1) * SNDP
     ZBAR3 = 3. * X(3)**2 / RSQ
     IF (KNORM2(JR,JS) == 1) THEN
       BPRM(JR,JS) = SQRT (ZBAR3 + 1.)       ! Normalise to TOTAL field
     ELSE IF (KNORM2(JR,JS) == 2) THEN
       BPRM(JR,JS) = ZBAR3 - 1.               ! Normalise to VERTICAL field
     END IF
     BPRM(JR,JS) = BFAC * BPRM(JR,JS) / R**3
   END DO
 END DO

 END SUBROUTINE PRMDC_MD

 SUBROUTINE SET_TIME_ORDER (NCHNL,TMS,WTMS)
!------------------------------------------

 IMPLICIT NONE
 INTEGER NCHNL,J1,J2
 REAL TMS(NCHNL),WTMS(NCHNL),A1

 DO J1 = 1,NCHNL-1
   DO J2 = J1+1, NCHNL
     IF (TMS(J1) > TMS(J2)) THEN
       A1 = TMS(J1)
       TMS(J1) = TMS(J2)
       TMS(J2) = A1
       A1 = WTMS(J1)
       WTMS(J1) = WTMS(J2)
       WTMS(J2) = A1
     END IF
   END DO
 END DO
 END SUBROUTINE SET_TIME_ORDER

 SUBROUTINE SET_TMSR (NCHNL,TMS)
!-------------------------------

!  Reverses TMS for UTEM output

 IMPLICIT NONE
 INTEGER NCHNL,J,JR
 REAL TMS(NCHNL), TMS1(NCHNL)

 TMS1 = TMS
 DO J = 1,NCHNL
   JR = NCHNL + 1 - J
   TMS(J) = TMS1(JR)
 END DO
 END SUBROUTINE SET_TMSR

 SUBROUTINE SET_VERTEX_ORDER (NTX,MXVRTX,NVRTX,SXN,SXE)
!------------------------------------------------------

! Assumes that the vertices have been entered in order, either clockwise or
! counter-clockwise.  If counter-clockwise, it reorders them.

!           NTX - number of loop transmitters
!        MXVRTX - maximum number of vertices for any loop
!      NVRTX(J) - number of vertices for transmitter J
!      SXE(I,J) = local east coordinate of vertex I for loop position J
!      SXN(I,J) = local coordinate of vertex I for loop position J

 IMPLICIT NONE
 INTEGER NTX,MXVRTX,NVRTX(NTX),NV,KS,KS1,JV,J1,JS
 REAL, DIMENSION (MXVRTX,NTX) :: SXN,SXE
 REAL, DIMENSION (MXVRTX) :: TEMPN,TEMPE
 REAL R,RMAX,XN_CNTR,YE_CNTR,CTH,STH,EMID,XN(MXVRTX),YE(MXVRTX)

 DO JS = 1,NTX
   NV = NVRTX(JS)
   KS = 0
   RMAX = 0.
   DO JV = 1,NV
     J1 = JV + 1
     IF (JV == NV) J1 = 1
     R = (SXN(J1,JS) - SXN(JV,JS))**2 + (SXE(J1,JS) - SXE(JV,JS))**2
     IF (R > RMAX) THEN
       RMAX = R               ! Find longest segment
       KS = JV
     END IF
   END DO

   RMAX = SQRT (RMAX)
   KS1 = KS + 1
   IF (KS1 > NV) KS1 = 1

!  Renumber loop vertices such that order is maintained but the longest segment
!  goes from new verex index NV to 1.  Rotate & translate loop such that vertex
!  NV is at the origin and vetex 1 is north on the axis.

   XN_CNTR = SXN(KS,JS)
   YE_CNTR = SXE(KS,JS)
   CTH = (SXN(KS1,JS) - XN_CNTR) / RMAX
   STH = (SXE(KS1,JS) - YE_CNTR) / RMAX

   XN(NV) = 0.;  YE(NV) = 0.;  XN(1) = RMAX;  YE(1) = 0.
   DO JV = 2, NV - 1
     J1 = JV + KS
     IF (J1 > NV) J1 = J1 - NV
     XN(JV) = CTH * (SXN(J1,JS) - XN_CNTR) + STH * (SXE(J1,JS) - YE_CNTR)
     YE(JV) = CTH * (SXE(J1,JS) - YE_CNTR) - STH * (SXN(J1,JS) - XN_CNTR)
   END DO


   RMAX = 0
   DO JV = 2, NV-1
     R = XN(JV) - XN(JV+1)
     IF (R > RMAX) THEN      ! Find segment with longest north to south extent
       RMAX = R
       EMID = (YE(JV) + YE(JV+1)) /2.
     END IF
   END DO

   IF (EMID < 0 ) THEN
     TEMPN(1:NV) = SXN(1:NV,JS)
     TEMPE(1:NV) = SXE(1:NV,JS)
     DO JV = 1,NV
       SXN(JV,JS) = TEMPN(NV+1-JV)
       SXE(JV,JS) = TEMPE(NV+1-JV)
     END DO
   END IF
 END DO

 END SUBROUTINE SET_VERTEX_ORDER

 SUBROUTINE SET_OUTPUT_LINES_FD (NFRQ,MCHNL,NTX,MRXTX,NRXTX,NLINES,MRXL,MCMP,NRX,LNTR,KNORM2,RX_TYPE, &
                                 UNITS,ISYS,IDH,SVAZM,MD1,MD2,RXAZM,RXDIP,CURNT,BPRM,BFD,BFTL)
!---------------------------------------------------------------------------------------------------

!** Called by MAIN

!   INPUT:  BFD (JF,JRS,JS,JC) : component JC of frequency-domain output for frequency JF
!                                of receiver JRS belonging to transmitter JS
!                                The units are in teslas or teslas / sec.
!                                Components are: north, east, vertical


!  OUTPUT:  BFTL (JF,JRL,JCL,JL) : frequency-domain output for receiver JRL belonging to Line JL.
!                                  The components,JCL, are either X,Y,Z as defined by a surface
!                                  survey or U,V,A, or modified U,V,A for downhole receivers.
!                                  JF = 1:NFRQ => REAL (BFD)
!                                  JF = NFRQ+1 : 2*NFRQ => AIMAG (BFD)

!          BFTL is normalised or not as determined by KNORM2 and BPRM.
!          BFTL units are determined for each line by UNITS.

!          Sampo:  When ISYS = 2, BFTL (*,*,1,*) = the normalised Sampo response

!
!               SURVEY_TYPE = 1
!               ---------------
!   LNTR(1,L) : Tx index for Line L.   LNTR(2,L) = LNTR(1,L)
!   LNTR(3,L) : Rx index  for start of Line L
!   LNTR(4,L) : Rx index for finish of Line L
!
!               SURVEY_TYPE > 1
!               ---------------
!   LNTR(1,L) : Tx index for start of Line L
!   LNTR(2,L) : Tx index for finish of Line L
!   LNTR(3,L) : Rx index for Line L.   LNTR(4,L) = LNTR(3,L)
!
!   The Rx index for variable LNTR is defined with reference to specific
!   transmitters rather than the global number.  Tx indices are global.
!
!   NFRQ         : number of frequencies
!   MCHNL        = NFRQ for Sampo;  = 2*NFRQ otherwise
!   NTX          : total number of transmitters
!   MRXTX        : maximum number of receivers per transmitter position
!   NRXTX        : number of receivers for transmitter position JS
!   NLINES       : number of receiver lines
!   MRXL         : maximum number of receiver positions for any line
!   MCMP         = 1 for Sampo;  = 3 otherwise
!   NRX          : number of receiver positions for Line L
!   KNORM2       : dc B field normalisation for Rx JR of Tx JS
!                : 0 (unnormalised), 1 (by components), 2 (by total field)
!   UNITS        : output units for line L
!   SVAZM        : survey azimuth for Line L
!   ISYS         : If ISYS = 2, use Sampo convention
!   IDH          : (0) surface survey; (1) conventional U,V,A; (2) UTEM S,N,W
!   (MD1,MD2))   : (MRXL, NLINES) for DH receivers; (1,1) otherwise
!   RXAZM,RXDIP  : azimuth & dip of receiver JR of line JL
!   CURNT        : maximum waveform current
!   BPRM         : Axial or total primary field (Teslas/amp) at receiver JR transmitter (JS)

!  YXZPLT(1:3,I,L) - GPS east, GPS north, RL  plot coordinate for Ith receiver of Line L


 INTEGER MCHNL,MCMP,NFRQ,NTX,MRXTX,NRXTX(NTX),NLINES,MRXL,MD1,MD2,LNTR(4,NLINES), &
         KNORM2(MRXTX,NTX),ISYS,JS,JR,JL,JRL,JC,JF1,JF2
 INTEGER, DIMENSION(NLINES) :: NRX,UNITS,RX_TYPE,IDH
 REAL, DIMENSION(NLINES) :: SVAZM
 REAL, DIMENSION(MD1,MD2) :: RXAZM,RXDIP
 REAL CURNT(NFRQ),BPRM(MRXTX,NTX),BFTL(MCHNL,MRXL,MCMP,NLINES),QR(3),QI(3), &
      QXR,QXI,QYR,QYI,CAZ,SAZ,CAZ0,SAZ0,SDP,CDP,A1
 COMPLEX BFD(NFRQ,MRXTX,NTX,3)

!================================================================

 IF (ISYS == 2) THEN  !  Sampo based on absoluute ratio
   UNITS = 31
   DO JL = 1,NLINES
     CAZ0 = COS (SVAZM (JL))
     SAZ0 = SIN (SVAZM (JL))
     IF (ABS (CAZ0) < 1.E-4) CAZ0 = 0.
     IF (ABS (SAZ0) < 1.E-4) SAZ0 = 0.
     JRL = 0
     DO JS = LNTR(1,JL), LNTR(2,JL)
       DO JR = LNTR(3,JL), LNTR(4,JL)
         BFD(1:NFRQ,JR,JS,1) = CAZ0 * BFD(1:NFRQ,JR,JS,1) + SAZ0 * BFD(1:NFRQ,JR,JS,2)
         JRL = JRL + 1
         BFTL(1:MCHNL,JRL,1,JL) = ABS (BFD(1:NFRQ,JR,JS,3) / BFD(1:NFRQ,JR,JS,1))
       END DO
     END DO
   END DO
!**************
   RETURN
!**************
 END IF
!================================================================

 DO JS = 1,NTX
   DO JR = 1,NRXTX(JS)
     DO JC = 1,3
       IF (KNORM2(JR,JS) == 0) THEN
         BFD(1:NFRQ,JR,JS,JC) = BFD(1:NFRQ,JR,JS,JC) * CURNT(1:NFRQ)
       ELSE
         BFD(1:NFRQ,JR,JS,JC) = BFD(1:NFRQ,JR,JS,JC) / BPRM(JR,JS)
       END IF
     END DO
   END DO
 END DO

!  Convert to LINE-based output and apply units

 DO JL = 1,NLINES
    A1 = 1.
    SELECT CASE (UNITS(JL))
    CASE (32)
      A1 = 100.
    CASE (2,33,42)
      A1 = 1000.
    CASE (3,34,43)
      A1 = 1.E6
    CASE (4,11,21,35,44)
      A1 = 1.E9
    CASE (12,22)
      A1 = 1.E12
    END SELECT

   JRL = 0
   DO JS = LNTR(1,JL), LNTR(2,JL)
     DO JR = LNTR(3,JL), LNTR(4,JL)
       JRL = JRL + 1
       DO JC = 1,MCMP
         BFTL(1:NFRQ,JRL,JC,JL) = A1 * REAL (BFD(1:NFRQ,JR,JS,JC))
         BFTL(NFRQ+1:2*NFRQ,JRL,JC,JL) = A1 * AIMAG (BFD(1:NFRQ,JR,JS,JC))
       END DO
     END DO
   END DO

!  For surface dipole surveys, reorient component 1 from north to lie along
!  the Survey X axis and component 2 from east to lie along the Survey Y axis.
!  For downhole surveys apply u,V,A transformation.

   IF (RX_TYPE(JL) > 1) CYCLE  ! Skip if not magnetic dipole receiver

   CAZ0 = COS (SVAZM (JL))
   SAZ0 = SIN (SVAZM (JL))
   IF (ABS (CAZ0) < 1.E-4) CAZ0 = 0.
   IF (ABS (SAZ0) < 1.E-4) SAZ0 = 0.
   DO JR = 1,NRX(JL)
     DO JF1 = 1,NFRQ
       JF2 = JF1 + NFRQ
       QR(1:3) = BFTL(JF1,JR,1:3,JL)
       QI(1:3) = BFTL(JF2,JR,1:3,JL)
       IF (IDH(JL) == 0) THEN    !  Surface survey
         BFTL(JF1,JR,1,JL) =  CAZ0 * QR(1) + SAZ0 * QR(2)    ! X component parallel to survey direction
         BFTL(JF1,JR,2,JL) = -SAZ0 * QR(1) + CAZ0 * QR(2)    ! Y component transverse to survey direction
         BFTL(JF2,JR,1,JL) =  CAZ0 * QI(1) + SAZ0 * QI(2)    ! X component parallel to survey direction
         BFTL(JF2,JR,2,JL) = -SAZ0 * QI(1) + CAZ0 * QI(2)    ! Y component transverse to survey direction
       ELSE
         CAZ = COS (RXAZM(JR,JL))
         SAZ = SIN (RXAZM(JR,JL))
         CDP = COS (RXDIP(JR,JL))
         SDP = SIN (RXDIP(JR,JL))
         IF (ABS (CAZ) < 1.E-4) CAZ = 0.
         IF (ABS (SAZ) < 1.E-4) CAZ = 0.
         IF (ABS (CDP) < 1.E-4) CDP = 0.
         IF (ABS (SDP) < 1.E-4) CDP = 0.
         QXR =  CAZ * QR(1) + SAZ * QR(2)                  ! local horizontal radial component
         QXI =  CAZ * QI(1) + SAZ * QI(2)                  ! local horizontal radial component
         QYR = -SAZ * QR(1) + CAZ * QR(2)                  ! local horizontal transverse component
         QYI = -SAZ * QI(1) + CAZ * QI(2)                  ! local horizontal transverse component
         BFTL(JF1,JR,3,JL) =  CDP * QR(3) + SDP * QXR       ! Axial component
         BFTL(JF2,JR,3,JL) =  CDP * QI(3) + SDP * QXI       ! Axial component

         IF (IDH(JL) == 1) THEN                              ! Conventional U,V,A processing
           BFTL(JF1,JR,1,JL) = -SDP * QR(3) + CDP * QXR        !   local U component
           BFTL(JF2,JR,1,JL) = -SDP * QI(3) + CDP * QXI        !   local U component
           BFTL(JF1,JR,2,JL) = QYR                             !   local V component
           BFTL(JF2,JR,2,JL) = QYI                             !   local V component

         ELSE IF (IDH(JL) == 2) THEN                         ! Utem style U,V,A processing
           QXR =  CAZ0 * QR(1) + SAZ0 * QR(2)                 !   Line-referenced horizontal radial component
           QXI =  CAZ0 * QI(1) + SAZ0 * QI(2)                 !   Line-referenced horizontal radial component
           BFTL(JF1,JR,1,JL) = -SDP * QR(3) + CDP * QXR        !   Line-referenced U component
           BFTL(JF2,JR,1,JL) = -SDP * QI(3) + CDP * QXI        !   Line-referenced U component

           BFTL(JF1,JR,2,JL) = SQRT (SUM (QR(1:3)**2) - BFTL(JF1,JR,1,JL)**2 - BFTL(JF1,JR,3,JL)**2)
           BFTL(JF2,JR,2,JL) = SQRT (SUM (QI(1:3)**2) - BFTL(JF2,JR,1,JL)**2 - BFTL(JF2,JR,3,JL)**2)
         END IF
       END IF
     END DO
   END DO
 END DO

 END SUBROUTINE SET_OUTPUT_LINES_FD

 SUBROUTINE SET_OUTPUT_LINES_FD1 (NFRQ,NTX,MRXTX,NRXTX,NLINES,MRXL,NRX,LNTR,KNORM2,RX_TYPE,UNITS, &
                                  ISYS,IDH,SVAZM,MD1,MD2,RXAZM,RXDIP,CURNT,BPRM,BFD,BFDL)
!-----------------------------------------------------------------------------------------------

!** Called by MAIN

!   INPUT:  BFD (JF,JRS,JS,JC) : component JC of frequency-domain output for frequency JF
!                                of receiver JRS belonging to transmitter JS
!                                The units are in teslas or teslas / sec.
!                                Components are: north, east, vertical


!  OUTPUT:  BFDL (JF,JRL,JL,JCL) : frequency-domain output for receiver JRL belonging to Line JL.
!                                  The components,JCL, are either X,Y,Z as defined by a surface
!                                  survey or U,V,A, or modified U,V,A for downhole receivers.

!          BFDL is normalised or not as determined by KNORM2 and BPRM.
!          BFDL units are determined for each line by UNITS.

!          Sampo:  When ISYS = 2, BFDL (*,*,*,1) = the normalised Sampo response
!                  In this case BFDL (*,*,*,2:3) = 0.

!
!               SURVEY_TYPE = 1
!               ---------------
!   LNTR(1,L) : Tx index for Line L.   LNTR(2,L) = LNTR(1,L)
!   LNTR(3,L) : Rx index  for start of Line L
!   LNTR(4,L) : Rx index for finish of Line L
!
!               SURVEY_TYPE > 1
!               ---------------
!   LNTR(1,L) : Tx index for start of Line L
!   LNTR(2,L) : Tx index for finish of Line L
!   LNTR(3,L) : Rx index for Line L.   LNTR(4,L) = LNTR(3,L)
!
!   The Rx index for variable LNTR is defined with reference to specific
!   transmitters rather than the global number.  Tx indices are global.
!
!   NFRQ         : number of frequencies
!   NTX          : total number of transmitters
!   MRXTX        : maximum number of receivers per transmitter position
!   NRXTX        : number of receivers for transmitter position JS
!   NLINES       : number of receiver lines
!   MRXL         : maximum number of receiver positions for any line
!   NRX          : number of receiver positions for Line L
!   KNORM2       : dc B field normalisation for Rx JR of Tx JS
!                : 0 (unnormalised), 1 (by components), 2 (by total field)
!   UNITS        : output units for line L
!   SVAZM        : survey azimuth for Line L
!   ISYS         : If ISYS = 2, use Sampo convention
!   IDH          : (0) surface survey; (1) conventional U,V,A; (2) UTEM S,N,W
!   (MD1,MD2))   : (MRXL, NLINES) for DH receivers; (1,1) otherwise
!   RXAZM,RXDIP  : azimuth & dip of receiver JR of line JL
!   CURNT        : maximum waveform current
!   BPRM         : Axial or total primary field (Teslas/amp) at receiver JR transmitter (JS)

!  YXZPLT(1:3,I,L) - GPS east, GPS north, RL  plot coordinate for Ith receiver of Line L


 INTEGER NFRQ,NTX,MRXTX,NRXTX(NTX),NLINES,MRXL,MD1,MD2,LNTR(4,NLINES),KNORM2(MRXTX,NTX), &
         ISYS,JS,JR,JL,JRL,JC,JF
 INTEGER, DIMENSION(NLINES) :: NRX,UNITS,RX_TYPE,IDH
 REAL, DIMENSION(NLINES) :: SVAZM
 REAL, DIMENSION(MD1,MD2) :: RXAZM,RXDIP
 REAL CURNT(NFRQ),BPRM(MRXTX,NTX),CAZ,SAZ,CAZ0,SAZ0,SDP,CDP,A1,A2
 COMPLEX BFD(NFRQ,MRXTX,NTX,3),BFDL(NFRQ,MRXL,3,NLINES),Q1(1:3),QX,QY

 DO JS = 1,NTX
   DO JR = 1,NRXTX(JS)
     DO JC = 1,3
       IF (KNORM2(JR,JS) == 0) THEN
         BFD(1:NFRQ,JR,JS,JC) = BFD(1:NFRQ,JR,JS,JC) * CURNT(1:NFRQ)
       ELSE
         BFD(1:NFRQ,JR,JS,JC) = BFD(1:NFRQ,JR,JS,JC) / BPRM(JR,JS)
       END IF
     END DO
   END DO
 END DO

!  Convert to LINE-based output and apply units

 IF (ISYS == 2) UNITS = 31    !  Sampo based on ratio
 DO JL = 1,NLINES
    A1 = 1.
    SELECT CASE (UNITS(JL))
    CASE (32)
      A1 = 100.
    CASE (2,33,42)
      A1 = 1000.
    CASE (3,34,43)
      A1 = 1.E6
    CASE (4,11,21,35,44)
      A1 = 1.E9
    CASE (12,22)
      A1 = 1.E12
    END SELECT

   JRL = 0
   DO JS = LNTR(1,JL), LNTR(2,JL)
     DO JR = LNTR(3,JL), LNTR(4,JL)
       JRL = JRL + 1
       DO JC = 1,3
         BFDL(1:NFRQ,JRL,JC,JL) = A1 * BFD(1:NFRQ,JR,JS,JC)
       END DO
     END DO
   END DO

!  For surface dipole surveys, reorient component 1 from north to lie along
!  the Survey X axis and component 2 from east to lie along the Survey Y axis.
!  For downhole surveys apply u,V,A transformation.

   IF (RX_TYPE(JL) > 1) CYCLE  ! Skip if not magnetic dipole receiver

   CAZ0 = COS (SVAZM (JL))
   SAZ0 = SIN (SVAZM (JL))
   IF (ABS (CAZ0) < 1.E-4) CAZ0 = 0.
   IF (ABS (SAZ0) < 1.E-4) SAZ0 = 0.
   DO JR = 1,NRX(JL)
     DO JF = 1,NFRQ
       Q1(1:3) = BFDL(JF,JR,1:3,JL)
       IF (IDH(JL) == 0) THEN    !  Surface survey
         BFDL(JF,JR,1,JL) =  CAZ0 * Q1(1) + SAZ0 * Q1(2)    ! X component parallel to survey direction
         BFDL(JF,JR,2,JL) = -SAZ0 * Q1(1) + CAZ0 * Q1(2)    ! Y component transverse to survey direction
         IF (ISYS == 2) THEN   ! Sampo
           A2 = ABS (BFDL(JF,JR,3,JL) / BFDL(JF,JR,1,JL))
           BFDL(JF,JR,1,JL) = CMPLX (A2,0.)
           BFDL(JF,JR,2:3,JL) = (0.,0.)
         END IF
       ELSE
         CAZ = COS (RXAZM(JR,JL))
         SAZ = SIN (RXAZM(JR,JL))
         CDP = COS (RXDIP(JR,JL))
         SDP = SIN (RXDIP(JR,JL))
         IF (ABS (CAZ) < 1.E-4) CAZ = 0.
         IF (ABS (SAZ) < 1.E-4) CAZ = 0.
         IF (ABS (CDP) < 1.E-4) CDP = 0.
         IF (ABS (SDP) < 1.E-4) CDP = 0.
         QX =  CAZ * Q1(1) + SAZ * Q1(2)                  ! local horizontal radial component
         QY = -SAZ * Q1(1) + CAZ * Q1(2)                  ! local horizontal transverse component
         BFDL(JF,JR,3,JL) =  CDP * Q1(3) + SDP * QX       ! Axial component

         IF (IDH(JL) == 1) THEN                              ! Conventional U,V,A processing
           BFDL(JF,JR,1,JL) = -SDP * Q1(3) + CDP * QX        !   local U component
           BFDL(JF,JR,2,JL) = QY                             !   local V component

         ELSE IF (IDH(JL) == 2) THEN                         ! Utem style U,V,A processing
           QX =  CAZ0 * Q1(1) + SAZ0 * Q1(2)                 !   Line-referenced horizontal radial component
           BFDL(JF,JR,1,JL) = -SDP * Q1(3) + CDP * QX        !   Line-referenced U component

           BFDL(JF,JR,2,JL) = SQRT (SUM (Q1(1:3)**2) - BFDL(JF,JR,1,JL)**2 - BFDL(JF,JR,3,JL)**2)
         END IF
       END IF
     END DO
   END DO
 END DO

 END SUBROUTINE SET_OUTPUT_LINES_FD1

 SUBROUTINE SET_OUTPUT_LINES_TD (NCHNL,NTX,MRXTX,NRXTX,NLINES,MRXL,MCMP,NRX,LNTR,KNORM2,RX_TYPE, &
                                 UNITS,ISYS,IDH,SVAZM,MD1,MD2,RXAZM,RXDIP,CURNT,BPRM,BTD,BFTL)
!-----------------------------------------------------------------------------------------------

!** Called by MAIN

!   INPUT:  BTD (JT,JRS,JS,JC) : component JC of time-domain output for channel JT
!                                of receiver JRS belonging to transmitter JS
!                                The units are in teslas or teslas / sec.
!                                Components are: north, east, vertical


!  OUTPUT:  BFTL (JT,JRL,JL,JCL) : time domain output for receiver JRL belonging to Line JL.
!                                  The components,JCL, are either X,Y,Z as defined by a surface
!                                  survey or U,V,A, or modified U,V,A for downhole receivers.

!          BFTL is normalised or not as determined by KNORM2 and BPRM.
!          BFTL units are determined for each line by UNITS.
!
!          UTEM: When ISYS = 1, BFTL(JT,*,*,*) is presented in reverse order from the latest
!                (JT = 1) to the earliest time (JT = NCHNL).  In this case the response from
!                JT = 1 is subtracted from all the other channels.
!
!               SURVEY_TYPE = 1
!               ---------------
!   LNTR(1,L) : Tx index for Line L.   LNTR(2,L) = LNTR(1,L)
!   LNTR(3,L) : Rx index  for start of Line L
!   LNTR(4,L) : Rx index for finish of Line L
!
!               SURVEY_TYPE > 1
!               ---------------
!   LNTR(1,L) : Tx index for start of Line L
!   LNTR(2,L) : Tx index for finish of Line L
!   LNTR(3,L) : Rx index for Line L.   LNTR(4,L) = LNTR(3,L)
!
!   The Rx index for variable LNTR is defined with reference to specific
!   transmitters rather than the global number.  Tx indices are global.
!
!   NCHNL        : number of channels
!   NTX          : total number of transmitters
!   MRXTX        : maximum number of receivers per transmitter position
!   NRXTX        : number of receivers for transmitter position JS
!   NLINES       : number of receiver lines
!   MRXL         : maximum number of receiver positions for any line
!   MCMP         = 1 for coincident loop;  = 3 otherwise
!   NRX          : number of receiver positions for Line L
!   KNORM2       : dc B field normalisation for Rx JR of Tx JS
!                : 0 (unnormalised), 1 (by components), 2 (by total field)
!   UNITS        : output units for line L
!   SVAZM        : survey azimuth for Line L
!   ISYS         : If ISYS = 1,use Utem convention
!   IDH          : (0) surface survey; (1) conventional U,V,A; (2)  UTEM S,N,W
!   (MD1,MD2))   : (MRXL, NLINES) for DH receivers; (1,1) otherwise
!   RXAZM,RXDIP  : azimuth & dip of receiver JR of line JL
!   CURNT        : maximum waveform current
!   BPRM         : Axial or total primary field (Teslas/amp) at receiver JR transmitter (JS)

!   YXZPLT(1:3,I,L) - GPS east, GPS north, RL  plot coordinate for Ith receiver of Line L


 INTEGER NCHNL,NTX,MRXTX,NRXTX(NTX),NLINES,MRXL,MCMP,MD1,MD2,LNTR(4,NLINES),KNORM2(MRXTX,NTX), &
         ISYS,JS,JR,JL,JRL,JC,JT
 INTEGER, DIMENSION(NLINES) :: NRX,UNITS,RX_TYPE,IDH
 REAL, DIMENSION(NLINES) :: SVAZM
 REAL, DIMENSION(MD1,MD2) :: RXAZM,RXDIP
 REAL CURNT(1),BPRM(MRXTX,NTX),BTD(NCHNL,MRXTX,NTX,3),BFTL(NCHNL,MRXL,MCMP,NLINES), &
      Q1(1:3),QX,QY,QZ,CAZ,SAZ,CAZ0,SAZ0,SDP,CDP,PHI,QT(NCHNL),ALF(3,3),RHO,A1

 ALF = 0.
 DO JS = 1,NTX
   DO JR = 1,NRXTX(JS)
     IF (KNORM2(JR,JS) == 0) CYCLE
     DO JC = 1,3
       BTD(1:NCHNL,JR,JS,JC) = BTD(1:NCHNL,JR,JS,JC) / (BPRM(JR,JS) * CURNT(1))
     END DO
   END DO
 END DO

!  Convert to LINE-based output and apply units

 DO JL = 1,NLINES
    A1 = 1.
    SELECT CASE (UNITS(JL))
    CASE (32)
      A1 = 100.
    CASE (2,33)
      A1 = 1000.
    CASE (3,34)
      A1 = 1.E6
    CASE (4,11,21,35)
      A1 = 1.E9
    CASE (12,22)
      A1 = 1.E12
    END SELECT

   JRL = 0
   DO JS = LNTR(1,JL), LNTR(2,JL)
     DO JR = LNTR(3,JL), LNTR(4,JL)
       JRL = JRL + 1
       DO JC = 1,MCMP
         BFTL(1:NCHNL,JRL,JC,JL) = A1 * BTD(1:NCHNL,JR,JS,JC)
       END DO
     END DO
   END DO

!  For surface dipole surveys only, reorient component 1 from north to lie along
!  the Survey X axis and component 2 from east to lie along the Survey Y axis.

   IF (RX_TYPE(JL) > 1) CYCLE  ! Skip if not magnetic dipole receiver

   CAZ0 = COS (SVAZM (JL))
   SAZ0 = SIN (SVAZM (JL))
   IF (ABS (CAZ0) < 1.E-4) CAZ0 = 0.
   IF (ABS (SAZ0) < 1.E-4) SAZ0 = 0.
   DO JR = 1,NRX(JL)
     IF (IDH(JL) == 0) THEN    !  Surface survey
       DO JT = 1,NCHNL
         Q1(1:2) = BFTL(JT,JR,1:2,JL)
         BFTL(JT,JR,1,JL) =  CAZ0 * Q1(1) + SAZ0 * Q1(2)
         BFTL(JT,JR,2,JL) = -SAZ0 * Q1(1) + CAZ0 * Q1(2)
       END DO
     ELSE
       CAZ = COS (RXAZM(JR,JL))
       SAZ = SIN (RXAZM(JR,JL))
       CDP = COS (RXDIP(JR,JL))
       SDP = SIN (RXDIP(JR,JL))
       DO JT = 1,NCHNL
         Q1(1:3) = BFTL(JT,JR,1:3,JL)
         QZ = Q1(3)
         IF (IDH(JL) == 1) THEN                           ! Express BFTL in U,V,A
           QX =  CAZ * Q1(1) + SAZ * Q1(2)                 ! local horizontal radial component
           QY = -SAZ * Q1(1) + CAZ * Q1(2)                 ! local horizontal transverse component
           BFTL(JT,JR,3,JL) =  CDP * QZ + SDP * QX         ! Axial component
           BFTL(JT,JR,1,JL) = -SDP * QZ + CDP * QX          !   local U component
           BFTL(JT,JR,2,JL) = QY                            !   local V component

         ELSE IF (IDH(JL) == 2) THEN                 ! Express BFTL in S, N, W
           QX =  CAZ0 * Q1(1) + SAZ0 * Q1(2)         !   X radial component in survey system
           QY = -SAZ0 * Q1(1) + CAZ0 * Q1(2)         !   Y transverse component in survey system
           PHI = RXAZM(JR,JL) - SVAZM(JL)            !   local - survey aximuth

           ALF(3,1) = SDP * COS (PHI)               ! direction cosines for W (axial) component
           ALF(3,2) = SDP * SIN (PHI)
           ALF(3,3) = CDP

           RHO = SQRT (ALF(3,2)**2 + ALF(3,3)**2)
           ALF(2,1) = 0.                             ! direction cosines for N (out-of-section component)
           ALF(2,2) =  ALF(3,3) / RHO
           ALF(2,3) = -ALF(3,2) /RHO

           ALF(1,1) = ALF(2,2) * ALF(3,3) - ALF(2,3) * ALF(3,2)   ! direction cosines for S (in-section component)
           ALF(1,2) = ALF(2,3) * ALF(3,1)
           ALF(1,3) = -ALF(2,2) * ALF(3,1)

           BFTL(JT,JR,1,JL) = ALF(1,1) * QX + ALF(1,2) * QY + ALF(1,3) * QZ  ! S
           BFTL(JT,JR,2,JL) =                 ALF(2,2) * QY + ALF(2,3) * QZ  ! N
           BFTL(JT,JR,3,JL) = ALF(3,1) * QX + ALF(3,2) * QY + ALF(3,3) * QZ  ! W
         END IF
       END DO
     END IF
     IF (ISYS == 4) THEN        !  reverse channels for UTEM & subtract channel 1 response
       DO JC = 1,3
         DO JT = 1,NCHNL
           JRL = NCHNL - JT + 1
           QT(JT) = BFTL(JT,JR,JC,JL)
         END DO
         DO JT = 1,NCHNL
           BFTL(JT,JR,JC,JL) = QT(JT)
           IF (JT > 1) BFTL(JT,JR,JC,JL) = BFTL(JT,JR,JC,JL) - BFTL(1,JR,JC,JL)
         END DO
       END DO
     END IF
   END DO
 END DO

 END SUBROUTINE SET_OUTPUT_LINES_TD

 SUBROUTINE SET_OUTPUT_LINES_TD1 (NCHNL,NTX,MRXTX,NRXTX,NLINES,MRXL,NRX,LNTR,KNORM2,RX_TYPE,UNITS, &
                                  ISYS,IDH,SVAZM,MD1,MD2,RXAZM,RXDIP,CURNT,BPRM,BTD,BTDL)
!-------------------------------------------------------------------------------------------------

!** Called by MAIN

!   INPUT:  BTD (JT,JRS,JS,JC) : component JC of time-domain output for channel JT
!                                of receiver JRS belonging to transmitter JS
!                                The units are in teslas or teslas / sec.
!                                Components are: north, east, vertical


!  OUTPUT:  BTDL (JT,JRL,JL,JCL) : time domain output for receiver JRL belonging to Line JL.
!                                  The components,JCL, are either X,Y,Z as defined by a surface
!                                  survey or U,V,A, or modified U,V,A for downhole receivers.

!          BTDL is normalised or not as determined by KNORM2 and BPRM.
!          BTDL units are determined for each line by UNITS.
!
!          UTEM: When ISYS = 1, BTDL(JT,*,*,*) is presented in reverse order from the latest
!                (JT = 1) to the earliest time (JT = NCHNL).  In this case the response from
!                JT = 1 is subtracted from all the other channels.
!
!               SURVEY_TYPE = 1
!               ---------------
!   LNTR(1,L) : Tx index for Line L.   LNTR(2,L) = LNTR(1,L)
!   LNTR(3,L) : Rx index  for start of Line L
!   LNTR(4,L) : Rx index for finish of Line L
!
!               SURVEY_TYPE > 1
!               ---------------
!   LNTR(1,L) : Tx index for start of Line L
!   LNTR(2,L) : Tx index for finish of Line L
!   LNTR(3,L) : Rx index for Line L.   LNTR(4,L) = LNTR(3,L)
!
!   The Rx index for variable LNTR is defined with reference to specific
!   transmitters rather than the global number.  Tx indices are global.
!
!   NCHNL        : number of channels
!   NTX          : total number of transmitters
!   MRXTX        : maximum number of receivers per transmitter position
!   NRXTX        : number of receivers for transmitter position JS
!   NLINES       : number of receiver lines
!   MRXL         : maximum number of receiver positions for any line
!   NRX          : number of receiver positions for Line L
!   KNORM2       : dc B field normalisation for Rx JR of Tx JS
!                : 0 (unnormalised), 1 (by components), 2 (by total field)
!   UNITS        : output units for line L
!   SVAZM        : survey azimuth for Line L
!   ISYS         : If ISYS = 1,use Utem convention
!   IDH          : (0) surface survey; (1) conventional U,V,A; (2)  UTEM S,N,W
!   (MD1,MD2))   : (MRXL, NLINES) for DH receivers; (1,1) otherwise
!   RXAZM,RXDIP  : azimuth & dip of receiver JR of line JL
!   CURNT        : maximum waveform current
!   BPRM         : Axial or total primary field (Teslas/amp) at receiver JR transmitter (JS)

!   YXZPLT(1:3,I,L) - GPS east, GPS north, RL  plot coordinate for Ith receiver of Line L


 INTEGER NCHNL,NTX,MRXTX,NRXTX(NTX),NLINES,MRXL,MD1,MD2,LNTR(4,NLINES),KNORM2(MRXTX,NTX), &
         ISYS,JS,JR,JL,JRL,JC,JT
 INTEGER, DIMENSION(NLINES) :: NRX,UNITS,RX_TYPE,IDH
 REAL, DIMENSION(NLINES) :: SVAZM
 REAL, DIMENSION(MD1,MD2) :: RXAZM,RXDIP
 REAL CURNT(1),BPRM(MRXTX,NTX),BTD(NCHNL,MRXTX,NTX,3),BTDL(NCHNL,MRXL,3,NLINES), &
      Q1(1:3),QX,QY,QZ,CAZ,SAZ,CAZ0,SAZ0,SDP,CDP,PHI,QT(NCHNL),ALF(3,3),RHO,A1

 ALF = 0.
 DO JS = 1,NTX
   DO JR = 1,NRXTX(JS)
     IF (KNORM2(JR,JS) == 0) CYCLE
     DO JC = 1,3
       BTD(1:NCHNL,JR,JS,JC) = BTD(1:NCHNL,JR,JS,JC) / (BPRM(JR,JS) * CURNT(1))
     END DO
   END DO
 END DO

!  Convert to LINE-based output and apply units

 DO JL = 1,NLINES
    A1 = 1.
    SELECT CASE (UNITS(JL))
    CASE (32)
      A1 = 100.
    CASE (2,33)
      A1 = 1000.
    CASE (3,34)
      A1 = 1.E6
    CASE (4,11,21,35)
      A1 = 1.E9
    CASE (12,22)
      A1 = 1.E12
    END SELECT

   JRL = 0
   DO JS = LNTR(1,JL), LNTR(2,JL)
     DO JR = LNTR(3,JL), LNTR(4,JL)
       JRL = JRL + 1
       DO JC = 1,3
         BTDL(1:NCHNL,JRL,JC,JL) = A1 * BTD(1:NCHNL,JR,JS,JC)
       END DO
     END DO
   END DO

!  For surface dipole surveys only, reorient component 1 from north to lie along
!  the Survey X axis and component 2 from east to lie along the Survey Y axis.

   IF (RX_TYPE(JL) > 1) CYCLE  ! Skip if not magnetic dipole receiver

   CAZ0 = COS (SVAZM (JL))
   SAZ0 = SIN (SVAZM (JL))
   IF (ABS (CAZ0) < 1.E-4) CAZ0 = 0.
   IF (ABS (SAZ0) < 1.E-4) SAZ0 = 0.
   DO JR = 1,NRX(JL)
     IF (IDH(JL) == 0) THEN    !  Surface survey
       DO JT = 1,NCHNL
         Q1(1:2) = BTDL(JT,JR,1:2,JL)
         BTDL(JT,JR,1,JL) =  CAZ0 * Q1(1) + SAZ0 * Q1(2)
         BTDL(JT,JR,2,JL) = -SAZ0 * Q1(1) + CAZ0 * Q1(2)
       END DO
     ELSE
       CAZ = COS (RXAZM(JR,JL))
       SAZ = SIN (RXAZM(JR,JL))
       CDP = COS (RXDIP(JR,JL))
       SDP = SIN (RXDIP(JR,JL))
       DO JT = 1,NCHNL
         Q1(1:3) = BTDL(JT,JR,1:3,JL)
         QZ = Q1(3)
         IF (IDH(JL) == 1) THEN                           ! Express BTDL in U,V,A
           QX =  CAZ * Q1(1) + SAZ * Q1(2)                 ! local horizontal radial component
           QY = -SAZ * Q1(1) + CAZ * Q1(2)                 ! local horizontal transverse component
           BTDL(JT,JR,3,JL) =  CDP * QZ + SDP * QX         ! Axial component
           BTDL(JT,JR,1,JL) = -SDP * QZ + CDP * QX          !   local U component
           BTDL(JT,JR,2,JL) = QY                            !   local V component

         ELSE IF (IDH(JL) == 2) THEN                 ! Express BTDL in S, N, W
           QX =  CAZ0 * Q1(1) + SAZ0 * Q1(2)         !   X radial component in survey system
           QY = -SAZ0 * Q1(1) + CAZ0 * Q1(2)         !   Y transverse component in survey system
           PHI = RXAZM(JR,JL) - SVAZM(JL)            !   local - survey aximuth

           ALF(3,1) = SDP * COS (PHI)               ! direction cosines for W (axial) component
           ALF(3,2) = SDP * SIN (PHI)
           ALF(3,3) = CDP

           RHO = SQRT (ALF(3,2)**2 + ALF(3,3)**2)
           ALF(2,1) = 0.                             ! direction cosines for N (out-of-section component)
           ALF(2,2) =  ALF(3,3) / RHO
           ALF(2,3) = -ALF(3,2) /RHO

           ALF(1,1) = ALF(2,2) * ALF(3,3) - ALF(2,3) * ALF(3,2)   ! direction cosines for S (in-section component)
           ALF(1,2) = ALF(2,3) * ALF(3,1)
           ALF(1,3) = -ALF(2,2) * ALF(3,1)

           BTDL(JT,JR,1,JL) = ALF(1,1) * QX + ALF(1,2) * QY + ALF(1,3) * QZ  ! S
           BTDL(JT,JR,2,JL) =                 ALF(2,2) * QY + ALF(2,3) * QZ  ! N
           BTDL(JT,JR,3,JL) = ALF(3,1) * QX + ALF(3,2) * QY + ALF(3,3) * QZ  ! W
         END IF
       END DO
     END IF
     IF (ISYS == 4) THEN        !  reverse channels for UTEM & subtract channel 1 response
       DO JC = 1,3
         DO JT = 1,NCHNL
           JRL = NCHNL - JT + 1
           QT(JT) = BTDL(JT,JR,JC,JL)
         END DO
         DO JT = 1,NCHNL
           BTDL(JT,JR,JC,JL) = QT(JT)
           IF (JT > 1) BTDL(JT,JR,JC,JL) = BTDL(JT,JR,JC,JL) - BTDL(1,JR,JC,JL)
         END DO
       END DO
     END IF
   END DO
 END DO

 END SUBROUTINE SET_OUTPUT_LINES_TD1

 SUBROUTINE SET_SWYTD (NSX,SWX,SWY,T0SX)
!----------------------------------------

! For time-domain, SET_SWYTD computes dI/dt at the transmitter using
!
!  Computes SWY to be dI(t)/dt
!  The units of SWY are amps / s

!*** Called by MAIN

!             INPUT
!             -----
!
!          NSX - number of source points in waveform
!          SWX - time abscissae for input waveform in seconds
!     SWY(*,3) - waveform in amps * NTRN * transmitter moment
!   SWY(*,1:2) = 0
!
!             OUTPUT
!             ------
!
!     SWY(*,3) = transmitter moment * waveform in amps
!     SWY(*,2) = transmitter moment * delta I(t) in amps
!     SWY(*,1) = transmitter moment * dI(t)/dt   in amps/s
!         T0SX = end of dI/dt on time or start of off-time

 IMPLICIT NONE
 REAL, PARAMETER :: T0_MIN=1.E-7
 INTEGER NSX,JT
 REAL SWX(NSX),SWY(NSX,3),DELT,T0SX

 INTENT (IN) NSX, SWX
 INTENT (INOUT) SWY


! Compute delta I in SWY(*,2) and dI/dt if it exists in SW(*,1).
! Compute the negative derivative so that dI/dt will be positive
! just before signal turn-off.  Store on right node (27.01.00)

 DO JT = 2, NSX
   SWY(JT,2) = SWY(JT-1,3) - SWY(JT,3)
   DELT = SWX(JT) - SWX(JT-1)
   IF (DELT > T0_MIN) THEN
     SWY(JT,1) = SWY(JT,2) / DELT
   END IF
 END DO


 T0SX = SWX(NSX)
 DO JT = NSX-1, 1, -1
   IF ( ABS (SWY(JT,1)) > 1.E-3) EXIT
   T0SX = SWX(JT)
 END DO
 END SUBROUTINE SET_SWYTD

 SUBROUTINE SET_SURVEY_1 (NLINES,MRXL,NTX,MXVRTX,NVRTX,MRXTX,NRX,MQVR,RX_TYPE,IPLT,NCNTRD, &
                          ECNTRD,SXND,SXED,SXN,SXE,SXZ,RXND,RXED,RXN,RXE,RXZ,LNTR,XRXTX,   &
                          YRXTX,ZRXTX,RXID,NCTD,YXZPLT,KNORM,KNORM2)
!----------------------------------------------------------------------------------------
!
!**  Called by MAIN
!
!  FOR SURVEY_TYPE = 1, SET_SURVEY_1
!  transforms transmitter coordinates from a global to local system if necessary.
!  Specifying transmitter and receiver arrays in GPS coordinates can
!  require double precision.  Rather than carry unnecessary higher precision
!  into the computation, arrays are adjusted by ECNTRD, NCNTRD
!
!  Although the user finds it more efficient to specify receivers by line,
!  computational efficiency requires these receivers to be referenced by their
!  transmitters.  Thus SET_SURVEY takes receiver positions and properties
!  associated with a line and references these to the transmitters.
!
!  Line-based arrays are constructed containing  the the global plot coordinates
!  of every Rx-Tx combination used in the survey.
!
!                    Input
!                    -----
!   NLINES      - number of lines
!   NTX         - total number of transmitter positions
!   MXVRTX      - maximum number of transmitter vertices
!   NVRTX(J)    - number of vertices for transmitter J
!   MRXTX       - maximum number of receivers per transmitter
!   MRXL        - maximum number of receivers per line = MRXTX
!   NRX(L)      - the number of receivers in Line L.
!   MQVR        - maximum number of vertices for all receivers
!               = 2 if any receiver is electric dipole, = 1 otherwise
!   RX_TYPE(L)  - Receiver type for Line L
!   IPLT        = 1 : plot response at receiver location
!               = 2 : plot response at transmitter-receiver midpoint
!   KNORM(L)    - norm indicator for line L
!   LNTR(1,L)   - transmitter index associated with Line L.  LNTR(2,L) = LNTR(1,L)
!
!   SXND(K,J)   - global north coordinate of vertex K of Tx J
!   SXED(K,J)   - global east coordinate of vertex K of Tx J
!   SXZ(J)      - depth of Tx J
!   RXND(I,J,K) - global north coordinate of vertex K of Rx I of Line J
!   RXED(I,J,K) - global east coordinate of vertex K of Rx I of Line J
!   RXZ(I,J)    - ground clearance (not depth) of dipole Rx I of Line J
!                 (electric dipole receivers have been rendered horizontal)
!
!      SXND, SXED, RXND, RXED are deallocated before return
!
!      NCNTRD   - North offset of local coordinate system       LOCAL = GLOBAL - CNTRD
!      ECNTRD   - East offset of local coordinate system
!
!                    Output
!                    ------
!  SXN(K,J)     - local north coordinate of vertex K of Tx J
!  SXE(K,J)     - local east coordinate of vertex K of Tx J
!  RXZ(I,J)     - depth of magnetic dipole Rx I of Line J
!  NCTD(I,J)    = 3 for MD Rx; = 1 for electric dipole Rx or coincident loop
!  LNTR(3,L)    - first receiver of Tx LNTR(1,JL) used for Line JL
!  LNTR(4,L)    - last receiver of Tx LNTR(1,JL) used for Line JL
!  XRXTX(I,J,K) - north coordinate of the Kth vertex of the Ith receiver of Tx J
!  YRXTX(I,J,K) - east coordinate of the Kth vertex of the Ith receiver of Tx J
!  ZRXTX(I,J)   - depth of the Ith receiver of transmitter J
!  RXID(I,J)    - receiver type for Rx I for Tx J
!  KNORM2(I,J)  - norm indicator for Rx I for Tx J
!  YXZPLT(1:3,I,L) - GPS east, GPS north, RL  plot coordinate for Ith receiver of Line L

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80)
 INTEGER NLINES,MRXL,NTX,MXVRTX,NVRTX(NTX),MRXTX,MQVR,JL,JS,JR,JRP, &
         JV,KVR,NV,KRXTX(NTX),LNTR(4,NLINES)
 INTEGER, DIMENSION (NLINES) :: RX_TYPE,IPLT,KNORM,NRX
 INTEGER, DIMENSION (MRXTX,NTX) :: KNORM2,NCTD,RXID
 REAL SXZ(NTX),ZRXTX(MRXTX,NTX),A1
 REAL, DIMENSION(MXVRTX,NTX) :: SXN ,SXE
 REAL, DIMENSION(MRXL,NLINES) :: RXZ
 REAL, DIMENSION(MRXL,NLINES,MQVR) :: RXN, RXE
 REAL, DIMENSION(MRXTX,NTX,MQVR) :: XRXTX,YRXTX
 REAL(KIND=QL) ECNTRD,NCNTRD,XSC,YSC,XRC,YRC
 REAL(KIND=QL), DIMENSION(MXVRTX,NTX) :: SXND,SXED
 REAL(KIND=QL), DIMENSION(MRXL,NLINES,MQVR) :: RXND,RXED
 REAL(KIND=QL), DIMENSION(3,MRXL,NLINES) :: YXZPLT

!  MRXL = MRXTX

 NCTD = 1
 SXE = REAL (SXED - ECNTRD)
 SXN = REAL (SXND - NCNTRD)

 XRXTX = 0.;  YRXTX = 0.; ZRXTX = 0.
 YXZPLT = 0.D0

 KRXTX = 0
 RXN = REAL (RXND - NCNTRD)
 RXE = REAL (RXED - ECNTRD)
 RXZ = -RXZ                 ! Convert elevation to depth
 DO JL = 1,NLINES
   KVR = RX_TYPE(JL)                  ! KVR = number of vertices for receiver type RX_TYPE(JL)
   IF (RX_TYPE(JL) == 3) KVR = 1

   JS = LNTR(1,JL)                    ! Identify transmitter for Line JL
   LNTR(3,JL) =  KRXTX(JS) + 1        ! First receiver of Tx JS used for Line JL
   KRXTX(JS) = KRXTX(JS) + NRX(JL)    ! Tx indices of receivers of Line L
   LNTR(4,JL) =  KRXTX(JS)            ! Last receiver of Tx JS used for Line JL
   NV = NVRTX(JS)
   XSC = SUM (SXND(1:NV,JS)) / REAL (NV)
   YSC = SUM (SXED(1:NV,JS)) / REAL (NV)

   DO JR = 1, NRX(JL)
     JRP = JR + LNTR(3,JL) - 1
     IF (RX_TYPE(JL) == 1) THEN
       KNORM2(JRP,JS) = KNORM(JL)
       NCTD(JRP,JS) = 3
     END IF
     RXID(JRP,JS) = RX_TYPE(JL)
     ZRXTX(JRP,JS) = RXZ(JR,JL)
     DO JV = 1,KVR
       XRXTX(JRP,JS,JV) = RXN(JR,JL,JV)
       YRXTX(JRP,JS,JV) = RXE(JR,JL,JV)
       IF (ABS (XRXTX(JRP,JS,JV)) < 1.E-4) XRXTX(JRP,JS,JV) = 0.
       IF (ABS (YRXTX(JRP,JS,JV)) < 1.E-4) YRXTX(JRP,JS,JV) = 0.
     END DO
     XRC = SUM (RXND(JR,JL,1:KVR)) / REAL (KVR)
     YRC = SUM (RXED(JR,JL,1:KVR)) / REAL (KVR)
     IF (IPLT(JL) == 1) THEN
       YXZPLT(1,JR,JL) = YRC
       YXZPLT(2,JR,JL) = XRC
       YXZPLT(3,JR,JL) = -REAL (RXZ(JR,JL),QL)
     ELSE
       YXZPLT(1,JR,JL) = 0.5 * (YRC + YSC)
       YXZPLT(2,JR,JL) = 0.5 * (XRC + XSC)
       A1 = 0.5 * (RXZ(JR,JL) + SXZ(JS))
       YXZPLT(3,JR,JL) = -REAL (A1,QL)           ! Make depth display negative.
     END IF
   END DO
 END DO

 END SUBROUTINE SET_SURVEY_1

 SUBROUTINE SET_SURVEY_2 (NTX,NTXL,MXVRTX,TXLNGTH,TXWDTH,NLINES,MRXL,NRX,DSTAT,SVAZM,SDN0,  &
                          SDE0,SDZ0,MRXTX,XRXOF,YRXOF,ZRXOF,NCNTRD,ECNTRD,IPLT,SXN,SXE,SXZ, &
                          RXN,RXE,RXZ,XRXTX,YRXTX,ZRXTX,YXZPLT,KNORM,KNORM2)
!------------------------------------------------------------------------------------------
!
!**  Called by MAIN
!
!  Sets up survey variables for SURVEY_TYPE = 2 & 3
!  Transmitter and receiver arrays are given in GPS coordinates which
!  require double precision.  Rather than carry unnecessary higher precision
!  into the computation, arrays are adjusted by ECNTRD & NCNTRD
!
!  Arrays containing the coordinates of every receiver belonging to a particular
!  transmitter indexed to that transmitter are constructed.  These are obtained
!  assigning the coordinates of the receivers by group.  This development is
!  only used for Loki at present.  Finally arrays are constructed containing
!  the the plot coordinates of every Rx-Tx combination used in the survey.
!
!                    Input
!                    -----
!        MXVRTX = 4 for loop transmitters;  = 1 for magnetic dipole transmitter
!           NTX - total number of transmitter positions
!          NTXL - number of distinct transmitter lines
!          MRXL - maximum number of receiver and transmitter positions per line
!        NRX(L) - number of transmitter or receiver positions on Line L
!    DSTAT(I,J) - Ith interval between loop positions (I-1, I) on Tx line J.  DSTAT(1,J) = 0
!       SDN0(J) - initial north coordinate for Tx line J
!       SDE0(J) - initial  east coordinate for Tx line J
!       SDZ0(J) - ground clearance of transmitter for Tx line J
!      SVAZM(J) - survey azimuth of Line J in radians
!                  X is along survey aximuth = north if SVAZM = 0
!                  Y is perpendicular to survey aximuth = east if SVAZM = 0
!       TXLNGTH - loop length along Line J
!       TXWDTH) - loop width across Line J
!         MRXTX - number of receivers per transmitter
!    XRXOF(I,J) - offset of receiver I along Tx line J
!    YRXOF(I,J) - offset of receiver I across Tx line J
!    ZRXOF(I,J) - ground clearance of receiver I (above = positive)
!
!          IPLT = 1 : plot response at receiver location
!               = 2 : plot response at transmitter-receiver midpoint
!       NCNTRD  - North offset of local origin      LOCAL = GLOBAL - CNTRD
!       ECNTRD  - East offset of local origin
!
!                     Output
!                     ------
!      SXN(K,J) - local north coordinate of vertex K of Tx J
!      SXE(K,J) - local east coordinate of vertex K of Tx J
!      SXZ(J)   - depth (z is positive down) of Tx J
!
!  XRXTX(I,J,1) - north coordinate of the Ith receiver of transmitter J
!  YRXTX(I,J,1) - east coordinate of the Ith receiver of transmitter J
!    ZRXTX(J,I) - depth of the Jth receiver of transmitter I
!
!  YXZPLT(1:3,I,L) - GPS east, GPS north, RL  plot coordinate for Ith receiver of Line L

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80)
 INTEGER NTX,MRXTX,NLINES,KNORM(NLINES),KNORM2(MRXTX,NTX),NTXL,MRXL,NRX(NLINES),MXVRTX, &
         IPLT(NLINES),JR,JL,JL1,JS,JTL,JRL,JV
 REAL DSTAT(MRXL,NTXL),CPHI,SPHI,SXZ(NTX),SXN(MXVRTX,NTX),SXE(MXVRTX,NTX),ZRXTX(MRXTX,NTX), &
      DRXN(MRXTX),DRXE(MRXTX),SLH,SWH,CLE(4),CLN(4),SEC(MRXL),SNC(MRXL),SVAZM(NLINES)
 REAL, DIMENSION(NTXL) :: TXLNGTH,TXWDTH,SDZ0
 REAL, DIMENSION(MRXL,NLINES) :: RXZ
 REAL, DIMENSION(MRXL,NLINES,1) :: RXN,RXE
 REAL, DIMENSION(MRXTX,NTX,1) :: XRXTX,YRXTX
 REAL, DIMENSION(MRXTX,NTXL)  :: XRXOF,YRXOF,ZRXOF
 REAL(KIND=QL) ECNTRD,NCNTRD,SDN0(NTXL),SDE0(NTXL)
 REAL(KIND=QL), DIMENSION(3,MRXL,NLINES) :: YXZPLT

 ZRXTX = 0.;  YXZPLT = 0.D0

 JS = 0                         ! Transmitter index
 DO JTL = 1,NTXL                ! Transmitter line index
   JL1 = 1 + (JTL-1) * MRXTX    ! First receiver line belonging to Tx line JTL
                                ! It is used to get the survey azimuth to
   CPHI = COS (SVAZM(JL1))      ! generate Tx vertices.
   SPHI = SIN (SVAZM(JL1))
   IF (ABS (CPHI) < 1.E-4) CPHI = 0.
   IF (ABS (SPHI) < 1.E-4) SPHI = 0.
   CLE = 0.;  CLN = 0.
   IF (MXVRTX == 4) THEN                ! Compute N,E corner offsets
     SLH = TXLNGTH(JTL) /2.
     SWH = TXWDTH(JTL) /2.
     CLN(1) =  SLH * CPHI + SWH * SPHI  ! lead left corner
     CLN(2) =  SLH * CPHI - SWH * SPHI  ! lead right corner
     CLN(3) = -SLH * CPHI - SWH * SPHI  ! rear right corner
     CLN(4) = -SLH * CPHI + SWH * SPHI  ! rear right corner
     CLE(1) = -SWH * CPHI + SLH * SPHI
     CLE(2) =  SWH * CPHI + SLH * SPHI
     CLE(3) =  SWH * CPHI - SLH * SPHI
     CLE(4) = -SWH * CPHI - SLH * SPHI
   END IF

   DO JR = 1,MRXTX         ! Compute N,E receiver offset from Tx centre
     DRXN(JR) = XRXOF(JR,JTL) * CPHI - YRXOF(JR,JTL) * SPHI
     DRXE(JR) = XRXOF(JR,JTL) * SPHI + YRXOF(JR,JTL) * CPHI
     IF (ABS (DRXN(JR)) < 1.E-4) DRXN(JR) = 0.
     IF (ABS (DRXE(JR)) < 1.E-4) DRXE(JR) = 0.
   END DO
   DO JRL = 1, NRX(JL1)    ! Transmitter / receiver position index for TX line JTL
     JS = JS + 1           ! Global transmitter index

     SXZ(JS) = -SDZ0(JTL)  ! ground clearance transformed to depth
     IF (JRL == 1) THEN
       SNC(1) = REAL (SDN0(JTL) - NCNTRD)
       SEC(1) = REAL (SDE0(JTL) - ECNTRD)
     ELSE
       SNC(JRL) = SNC(JRL-1) + DSTAT(JRL,JTL) * CPHI
       SEC(JRL) = SEC(JRL-1) + DSTAT(JRL,JTL) * SPHI
     END IF
     DO JV = 1,MXVRTX
       SXN(JV,JS) = SNC(JRL) + CLN(JV)
       SXE(JV,JS) = SEC(JRL) + CLE(JV)
     END DO

     DO JR = 1,MRXTX
       XRXTX(JR,JS,1) = SNC(JRL) + DRXN(JR)
       YRXTX(JR,JS,1) = SEC(JRL) + DRXE(JR)
       ZRXTX(JR,JS) = -(SDZ0(JTL) + ZRXOF(JR,JTL))
       JL = JL1 + JR -1         ! Line Index (JL1 = Line index of first offset Rx)
       RXN(JRL,JL,1) = XRXTX(JR,JS,1)
       RXE(JRL,JL,1) = YRXTX(JR,JS,1)
       RXZ(JRL,JL) =   ZRXTX(JR,JS)

       KNORM2(JR,JS) = KNORM(JL)

       IF (IPLT(JL) == 1) THEN
         YXZPLT(1,JRL,JL) = ECNTRD + REAL (SEC(JRL),QL) + REAL(DRXE(JR),QL)
         YXZPLT(2,JRL,JL) = NCNTRD + REAL (SNC(JRL),QL) + REAL(DRXN(JR),QL)
         YXZPLT(3,JRL,JL) = -RXZ(JRL,JL)
       ELSE
         YXZPLT(1,JRL,JL) = ECNTRD + REAL (SEC(JRL),QL) + 0.5D0 * REAL(DRXE(JR),QL)
         YXZPLT(2,JRL,JL) = NCNTRD + REAL (SNC(JRL),QL) + 0.5D0 * REAL(DRXN(JR),QL)
         YXZPLT(3,JRL,JL) = -(SXZ(JS) +  RXZ(JRL,JL)) / 2.
       END IF
     END DO
   END DO
 END DO
 END SUBROUTINE SET_SURVEY_2

 SUBROUTINE SET_SURVEY_4 (NTX,NLINES,TXLNGTH,TXWDTH,MRXL,NRX,DSTAT,SVAZM,NCNTRD,ECNTRD, &
                          SDN0,SDE0,SXN,SXE,YXZPLT)
!---------------------------------------------------------------------------------------
!
!**  Called by MAIN
!
!  Sets up survey variables for coincident loop, SURVEY_TYPE = 4
!  The transmitter vertices are given in GPS coordinates which require
!  double precision.  Rather than carry unnecessary higher precision into the
!  computation, arrays are adjusted by ECNTRD & NCNTRD
!
!                    Input
!                    -----
!           NTX - total number of transmitter positions
!        NLINES - number of distinct transmitter lines
!          MRXL - maximum number of receiver and transmitter positions per line
!        NRX(J) - number of transmitter or receiver positions on Tx line J
!    DSTAT(I,J) - Ith interval between loop positions (I-1, I) on Tx line J.  DSTAT(1,J) = 0
!       SDN0(J) - initial north coordinate for Tx line J
!       SDE0(J) - initial  east coordinate for Tx line J
!      SVAZM(J) - survey azimuth of Line J in radians
!                  X is along survey aximuth = north if SVAZM = 0
!                  Y is perpendicular to survey aximuth = east if SVAZM = 0
!       TXLNGTH - loop length along Line J
!       TXWDTH) - loop width across Line J
!       NCNTRD  - North offset of local origin      LOCAL = GLOBAL - CNTRD
!       ECNTRD  - East offset of local origin
!
!                     Output
!                     ------
!      SXN(K,J) - local north coordinate of vertex K of Tx J
!      SXE(K,J) - local east coordinate of vertex K of Tx J
!
!  YXZPLT(1:3,I,L) - GPS east, GPS north, RL  plot coordinate for Ith receiver of Line L

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80)
 INTEGER NTX,NLINES,MRXL,NRX(NLINES),JR,JL,JS,JV
 REAL DSTAT(MRXL,NLINES),CPHI,SPHI,SXN(4,NTX),SXE(4,NTX),SLH,SWH,CLE(4),CLN(4),SEC(MRXL),SNC(MRXL),SVAZM(NLINES)
 REAL, DIMENSION(NLINES) :: TXLNGTH,TXWDTH
 REAL(KIND=QL) ECNTRD,NCNTRD,SDN0(NLINES),SDE0(NLINES)
 REAL(KIND=QL), DIMENSION(3,MRXL,NLINES) :: YXZPLT

 YXZPLT = 0.D0
 JS = 0                         ! Transmitter index
 DO JL = 1,NLINES                ! Transmitter line index
   CPHI = COS (SVAZM(JL))
   SPHI = SIN (SVAZM(JL))
   IF (ABS (CPHI) < 1.E-4) CPHI = 0.
   IF (ABS (SPHI) < 1.E-4) SPHI = 0.
   CLE = 0.;  CLN = 0.
   SLH = TXLNGTH(JL) /2.
   SWH = TXWDTH(JL) /2.
   CLN(1) =  SLH * CPHI + SWH * SPHI  ! lead left corner
   CLN(2) =  SLH * CPHI - SWH * SPHI  ! lead right corner
   CLN(3) = -SLH * CPHI - SWH * SPHI  ! rear right corner
   CLN(4) = -SLH * CPHI + SWH * SPHI  ! rear right corner
   CLE(1) = -SWH * CPHI + SLH * SPHI
   CLE(2) =  SWH * CPHI + SLH * SPHI
   CLE(3) =  SWH * CPHI - SLH * SPHI
   CLE(4) = -SWH * CPHI - SLH * SPHI

   DO JR = 1, NRX(JL)   ! Transmitter / receiver position index
     JS = JS + 1           ! Global transmitter index
     IF (JR == 1) THEN
       SNC(1) = REAL (SDN0(JL) - NCNTRD)
       SEC(1) = REAL (SDE0(JL) - ECNTRD)
     ELSE
       SNC(JR) = SNC(JR-1) + DSTAT(JR,JL) * CPHI
       SEC(JR) = SEC(JR-1) + DSTAT(JR,JL) * SPHI
     END IF

     YXZPLT(1,JR,JL) = ECNTRD + REAL (SEC(JR),QL)
     YXZPLT(2,JR,JL) = NCNTRD + REAL (SNC(JR),QL)

     DO JV = 1,4
       SXN(JV,JS) = SNC(JR) + CLN(JV)
       SXE(JV,JS) = SEC(JR) + CLE(JV)
     END DO
   END DO
 END DO

 END SUBROUTINE SET_SURVEY_4

 SUBROUTINE SET_SURVEY_5 (NTX,NLINES,LNTR,SXND,SXED,SXZ,SXDIP,SXAZM,MRXL,RXOTX,NCNTRD,ECNTRD, &
                          IPLT,SXN,SXE,RXN,RXE,RXZ,XRXTX,YRXTX,ZRXTX,YXZPLT,KNORM,KNORM2)
!-------------------------------------------------------------------------------------------
!
!**  Called by MAIN
!
!  Sets up local transmitter coordinates for downhole SURVEY_TYPE = 5
!  Computes receiver position for each transmitter position.
!
!  Arrays containing the coordinates of every receiver belonging to a particular
!  transmitter indexed to that transmitter are constructed.  These are obtained
!  assigning the coordinates of the receivers by group.  This development is
!  only used for Loki at present.  Finally arrays are constructed containing
!  the the plot coordinates of every Rx-Tx combination used in the survey.
!
!                    Input
!                    -----
!           NTX - total number of number of transmitter positions
!     SXND(1,J) - initial north coordinate for Tx J
!     SXED(1,J) - initial  east coordinate for Tx J
!        SXZ(J) - depth (z is positive down) of Tx J
!      SXDIP(J) - dip (radians) of transmitter J (VMD = 0)
!      SXAZM(J) - azimute of transmitter J (north = 0)
!      RXOTX(J) - offset of receiver for Tx J (positive if Rx ABOVE Tx)
!                 (This routine sets RX coordinates positive down using RXOTX(J) as a negative.)
!
!          IPLT = 1 : plot response at receiver location
!               = 2 : plot response at transmitter-receiver midpoint
!       NCNTRD  - North offset of local origin      LOCAL = GLOBAL - CNTRD
!       ECNTRD  - East offset of local origin
!
!                     Output
!                     ------
!      SXN(1,J) - local north coordinate of Tx J
!      SXE(1,J) - local east coordinate of Tx J
!
!  XRXTX(1,J,1) - north coordinate of receiver for transmitter J
!  YRXTX(1,J,1) - east coordinate of receiver for transmitter J
!    ZRXTX(1,J) - depth of receiver for transmitter J
!
!  YXZPLT(1:3,I,L) - GPS east, GPS north, RL  plot coordinate for Ith receiver of Line L

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80)
 INTEGER NLINES,NTX,KNORM(NLINES),KNORM2(1,NTX),IPLT(NLINES),LNTR(4,NLINES),JS,JL,MRXL,JR
 REAL DN,DE,DZ,A1
 REAL, DIMENSION (MRXL,NLINES) :: RXZ
 REAL, DIMENSION (MRXL,NLINES,1) :: RXE,RXN
 REAL, DIMENSION(NTX) :: SXZ,SXDIP,SXAZM,RXOTX
 REAL, DIMENSION(1,NTX) :: SXN,SXE,ZRXTX
 REAL, DIMENSION(1,NTX,1) :: XRXTX,YRXTX
 REAL(KIND=QL) ECNTRD,NCNTRD
 REAL(KIND=QL), DIMENSION(1,NTX) :: SXND,SXED
 REAL(KIND=QL), DIMENSION(3,MRXL,NLINES) :: YXZPLT

 SXN = REAL (SXND - NCNTRD)
 SXE = REAL (SXED - ECNTRD)

 JS = 0
 DO JL = 1,NLINES
   DO JR = LNTR(1,JL), LNTR(2,JL)
     JS = JS + 1

!  Align offset.

     DN = SIN (SXDIP(JS)) * COS (SXAZM(JS)) * RXOTX(JS)
     DE = SIN (SXDIP(JS)) * SIN (SXAZM(JS)) * RXOTX(JS)
     DZ = COS (SXDIP(JS)) * RXOTX(JS)

     XRXTX(1,JS,1) = SXN(1,JS) + DN
     YRXTX(1,JS,1) = SXE(1,JS) + DE
     ZRXTX(1,JS) = SXZ(JS) + DZ

     RXN(JR,JL,1) = XRXTX(1,JS,1)
     RXE(JR,JL,1) = YRXTX(1,JS,1)
     RXZ(JR,JL) = ZRXTX(1,JS)
     KNORM2(1,JS) = KNORM(JL)

     IF (IPLT(JL) == 1) THEN
       A1 = SXZ(JS) + DZ
       YXZPLT(1,JR,JL) = SXED(1,JS) + REAL(DE,QL)
       YXZPLT(2,JR,JL) = SXND(1,JS) + REAL(DN,QL)
       YXZPLT(3,JR,JL) = -REAL(A1,QL)
     ELSE
       A1 = SXZ(JS) + 0.5 * DZ
       YXZPLT(1,JR,JL) = SXED(1,JS) + 0.5D0 * REAL(DE,QL)
       YXZPLT(2,JR,JL) = SXND(1,JS) + 0.5D0 * REAL(DN,QL)
       YXZPLT(3,JR,JL) = -REAL(A1,QL)
     END IF
   END DO
 END DO

 END SUBROUTINE SET_SURVEY_5

 SUBROUTINE SET_SURVEY_6 (NCNTRD,ECNTRD,NHRX,NERX,MRXTX,RXND,RXED,XRXTX,YRXTX,ZRXTX,MTPLT,RXID)
!---------------------------------------------------------------------------------------------
!
!**  Called by MAIN
!
!  FOR THE MAGNETOTELLURICS OPTION - SURVEY_TYPE = 6, SET_SURVEY_6
!  transforms receiver coordinates from a global to local system if necessary.
!  Specifying  receiver arrays in GPS coordinates can require double precision.
!  Rather than carry unnecessary higher precision into the computation, arrays
!  are adjusted by ECNTRD, NCNTRD
!
!  Although the user finds it more efficient to specify receivers by line,
!  computational efficiency requires these receivers to be referenced by their
!  two pseudo transmitters (X & Y polarisations.  SET_SURVEY does this.
!
!  Line-based arrays are constructed containing  the the global plot coordinates
!  of every Rx-Tx combination used in the survey.
!
!                    Input
!                    -----
!
!   NCNTRD      : North offset of local coordinate system       LOCAL = GLOBAL - CNTRD
!   ECNTRD      : East offset of local coordinate system
!   NHRX        : number of magnetic receivers
!   NERX        : number of electric receivers
!   MRXTX       : NHRX + NERX
!   MRXTX       - maximum number of receivers per transmitter
!   RXND(I,J,1) - global north coordinate of Rx I of Line J
!   RXED(I,J,1) - global east coordinate of  Rx I of Line J
!
!                    Output
!                    ------
!  XRXTX(I,J,K) - north coordinate of the Kth vertex of the Ith receiver of Tx J
!  YRXTX(I,J,K) - east coordinate of the Kth vertex of the Ith receiver of Tx J
!  ZRXTX(I,J)   - depth of the Ith receiver of transmitter J
!  MTPLT(1:2,K) - GPS east, and GPS north plot coordinate for Kth E-field receiver
!  RXID(I,J)    - receiver type for Rx I of Polarisation J

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80), NLINES=2, NTX=2
 INTEGER NERX,NHRX,MRXTX,JR,JRP,RXID(MRXTX,NTX)
 REAL ZRXTX(MRXTX,NTX)
 REAL, DIMENSION(NERX,NLINES,1) :: RXN, RXE
 REAL, DIMENSION(MRXTX,2,1) :: XRXTX,YRXTX
 REAL(KIND=QL) ECNTRD,NCNTRD,RXND(NERX,NLINES,1),RXED(NERX,NLINES,1),MTPLT(2,NERX)

!  MRXL = MRXTX

 ZRXTX = 0.01  !  All receivers at 1 cm depth

 RXN = REAL (RXND - NCNTRD)
 RXE = REAL (RXED - ECNTRD)
 DO JR = 1,NHRX             ! magnetic receivers
   RXID(JR,1:2) = 1
   XRXTX(JR,1:NTX,1) = RXN(JR,1,1)
   YRXTX(JR,1:NTX,1) = RXE(JR,1,1)
 END DO

 DO JR = 1, NERX ! electric receivers
   JRP = JR + NHRX
   RXID(JRP,1:2) = 3
   XRXTX(JRP,1:NTX,1) = RXN(JR,2,1)
   YRXTX(JRP,1:NTX,1) = RXE(JR,2,1)
   MTPLT(1,JR) = RXED(JR,2,1)
   MTPLT(2,JR) = RXND(JR,2,1)
 END DO

 END SUBROUTINE SET_SURVEY_6

 SUBROUTINE TDEM_3D (STEP,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL,TOPN, &
                     TCLS,FREQ,NFRQ,NTX,MRXTX,NRXTX,RXID,NCTD,BFD1,BTD1)
!----------------------------------------------------------------------------

!***  Called by MAIN
!***  Calls CUBSPL, COSTRN, FOLD_CONVOLVE

!  Computes BTD1, the time domain response for the input frequency_domain data.
!  It computes voltage (volts) or dB/dt (teslas/sec) if STEP = 0) or magnetic
!  field B (in teslas) if STEP = 1)  by convolving the step response of the
!  earth with the negative time-derivative of the current waveform.
!  For magnetic field, this is averaged across the receiver window.  For db/dt,
!  or voltage, this is differenced across the  receiver window.  The negative
!  dI/dt is used so that current switch off corresponds to positive response.
!
!  On entry, the imaginary component of the frequency-domain data in array BFD1
!  is divided by frequency and then cosine transformed into time-domain step
!  response data out to NPULS bipolar cycles.  For each receiver position for
!  for each transmitter, FOLD_AND_CONVOLVE is called to fold the positive &
!  negative parts of the bipolar current pulse into a half-cycle (length PULSE)
!  decay curve.  This result is convolved with the dI/dt waveform.
!
!        STEP = 1 for normal step response is to be computed
!             = 4 for UTEM pure step response
!         NSX - number of points used to discretise transmitter signal
!         SWX - abscissae (seconds) of current waveform
!         SWY - dI/dt at times SWX
!       NPULS - number of bipolar pulses of length PULSE
!       PULSE - length single on pulse plus off-time
!       NTYRP - number of values in TRP for total signal length: 2 * NPULS *PULSE
!         TRP - array of time values for FD -> TD transformations
!      NTYPLS - number of TRP values in 1 PULSE
!       NCHNL - number of channels
!        TOPN - time at which receiver channel I opens.
!        TCLS - time at which receiver channel I closes.
!        FREQ - array of NFRQ frequencies.
!         NTX - number of transmitter positions.
!       MRXTX - maximum number of receiver positions for any transmitter position
!    NRXTX(J) = number of receivers FOR transmitter J
!   RXID(I,J) = RX_TYPE of receiver I for transmitter J
!   NCTD(I,J) = number of components for receiver I for transmitter J
!             = number of components, = 3 or 1 usually
!   BFD1(I,J,K,L) - Lth component of the complex frequency-domain response at
!                   receiver J, of transmitter K for frequency I.  If NCTD = 1
!                   as is the case for coincident loop, or electric dipole,
!                   then the relevant component is stored in L=1 position

 IMPLICIT NONE
 REAL, PARAMETER :: TWOPI=6.283185
 INTEGER STEP,NSX,NPULS,NTYPLS,NTYRP,NCHNL,NFRQ,NTX,MRXTX,NRXTX(NTX), &
         NCTD(MRXTX,NTX),STEPC,RXID(MRXTX,NTX),JR,JS,JF,JC,JT
 REAL, DIMENSION(:,:), ALLOCATABLE ::  YSCAT,YFRQ
 REAL PULSE,FREQ(NFRQ),WF(NFRQ),SWX(NSX),SWY(NSX,3), &
      COSTRN,T,YCUM(NCHNL),TOPN(NCHNL),TCLS(NCHNL),TRP(NTYRP), &
      BTD1(NCHNL,MRXTX,NTX,3)
 COMPLEX BFD1(NFRQ,MRXTX,NTX,3)

 BTD1 = 0.

 ALLOCATE (YSCAT(4,NTYRP), YFRQ(4,NFRQ) )
 YSCAT=0; YFRQ=0

!  The - sign below is a consequence of using the sine transform for a
!  +iwt sign convention

 WF(1:NFRQ) = ALOG (TWOPI * FREQ(1:NFRQ))

!  For each component at each receiver, compute the time-domain step response
!  by splining the imaginary part of the frequency-domain response, converting
!  it to time-domain step function response and folding the NPULS bipolar decay
!  curve into a combined pulse decay curve of length PULSE.  Convolve this with
!  the TX waveform to produce BTD1, the 'observable" stripped response for the
!  system.

 DO JS = 1,NTX
   DO JR = 1, NRXTX(JS)
     STEPC = STEP
     IF (RXID(JR,JS) == 2) THEN
       STEPC = 3                      ! Electrode E field Transform INTEGRAL { E dl } to TD
     END IF
     DO JC = 1,NCTD(JR,JS)
       DO JF = 1,NFRQ     ! Divide by -iw to set up step response
         IF (STEPC == 3) THEN
           YFRQ(1,JF) = REAL ( BFD1(JF,JR,JS,JC) )
         ELSE
           YFRQ(1,JF) = -AIMAG ( BFD1(JF,JR,JS,JC) ) / (TWOPI * FREQ(JF))
         END IF
       END DO
       CALL CUBSPL (WF,YFRQ,NFRQ)

       YSCAT = 0.

       DO JT = 1, NTYRP   !  Convert to step-function time-domain.
         T = TRP(JT)
         YSCAT(1,JT) = COSTRN (WF,YFRQ,NFRQ,T)
       END DO

       CALL FOLD_AND_CONVOLVE (STEPC,NSX,SWX,SWY,NPULS,PULSE,TRP,NTYRP,NTYPLS, &
                               NCHNL,TOPN,TCLS,YSCAT,YCUM)

       BTD1(1:NCHNL,JR,JS,JC) = YCUM(1:NCHNL)
     END DO
   END DO
 END DO

 DEALLOCATE (YSCAT, YFRQ)

 END SUBROUTINE TDEM_3D

 SUBROUTINE TDEM_3D1 (STEP,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL,TOPN, &
                     TCLS,FREQ,NFRQ,KFRQE,NTX,MRXTX,NRXTX,RXID,NCMP,BFD1,BTD1)
!----------------------------------------------------------------------------------

!***  Called by MAIN
!***  Calls CUBSPL, COSTRN, FOLD_CONVOLVE

!  Computes BTD1, the time domain response for the input frequency_domain data.
!  It computes voltage (volts) or dB/dt (teslas/sec) if STEP = 0) or magnetic
!  field B (in teslas) if STEP = 1)  by convolving the step response of the
!  earth with the negative time-derivative of the current waveform.
!  For magnetic field, this is averaged across the receiver window.  For db/dt,
!  or voltage, this is differenced across the  receiver window.  The negative
!  dI/dt is used so that current switch off corresponds to positive response.
!
!  On entry, the imaginary component of the frequency-domain data in array BFD1
!  is divided by frequency and then cosine transformed into time-domain step
!  response data out to NPULS bipolar cycles.  For each receiver position for
!  for each transmitter, FOLD_AND_CONVOLVE is called to fold the positive &
!  negative parts of the bipolar current pulse into a half-cycle (length PULSE)
!  decay curve.  This result is convolved with the dI/dt waveform.
!
!        STEP = 1 for normal step response is to be computed
!             = 4 for UTEM pure step response
!         NSX - number of points used to discretise transmitter signal
!         SWX - abscissae (seconds) of current waveform
!         SWY - dI/dt at times SWX
!       NPULS - number of bipolar pulses of length PULSE
!       PULSE - length single on pulse plus off-time
!       NTYRP - number of values in TRP for total signal length: 2 * NPULS *PULSE
!         TRP - array of time values for FD -> TD transformations
!      NTYPLS - number of TRP values in 1 PULSE
!       NCHNL - number of channels
!        TOPN - time at which receiver channel I opens.
!        TCLS - time at which receiver channel I closes.
!        FREQ - array of NFRQ frequencies.
!         NTX - number of transmitter positions.
!       MRXTX - maximum number of receiver positions for any transmitter position
!    NRXTX(J) = number of receivers FOR transmitter J
!   RXID(I,J) = RX_TYPE of receiver I for transmitter J
!   NCMP(I,J) = number of components for receiver I for transmitter J
!             = number of components, = 3 or 1 usually
!   BFD1(I,J,K,L) - Lth component of the complex frequency-domain response at
!                   receiver J, of transmitter K for frequency I.  If NCMP = 1
!                   as is the case for coincident loop, or electric dipole,
!                   then the relevant component is stored in L=1 position

 IMPLICIT NONE
 REAL, PARAMETER :: TWOPI=6.283185
 INTEGER STEP,NSX,NPULS,NTYPLS,NTYRP,NCHNL,NFRQ,KFRQE,KFRQ,NTX,MRXTX,NRXTX(NTX), &
         NCMP(MRXTX,NTX),STEPC,RXID(MRXTX,NTX),JR,JS,JF,JC,JT
 REAL, DIMENSION(:,:), ALLOCATABLE ::  YSCAT,YFRQ
 REAL PULSE,FREQ(NFRQ),WF(NFRQ),SWX(NSX),SWY(NSX,3), &
      COSTRN,T,YCUM(NCHNL),TOPN(NCHNL),TCLS(NCHNL),TRP(NTYRP), &
      BTD1(NCHNL,MRXTX,NTX,3)
 COMPLEX BFD1(NFRQ,MRXTX,NTX,3)

 BTD1 = 0.

 ALLOCATE (YSCAT(4,NTYRP), YFRQ(4,NFRQ) )
 YSCAT=0; YFRQ=0

!  The - sign below is a consequence of using the sine transform for a
!  +iwt sign convention

 WF(1:NFRQ) = ALOG (TWOPI * FREQ(1:NFRQ))

!  For each component at each receiver, compute the time-domain step response
!  by splining the imaginary part of the frequency-domain response, converting
!  it to time-domain step function response and folding the NPULS bipolar decay
!  curve into a combined pulse decay curve of length PULSE.  Convolve this with
!  the TX waveform to produce BTD1, the 'observable" stripped response for the
!  system.

 DO JS = 1,NTX
   DO JR = 1, NRXTX(JS)
     STEPC = STEP
     KFRQ = NFRQ
     IF (RXID(JR,JS) == 2) THEN
       STEPC = 3                      ! Electrode E field Transform INTEGRAL { E dl } to TD
       KFRQ = KFRQE
     END IF
     DO JC = 1,NCMP(JR,JS)
       DO JF = 1,KFRQ     ! Divide by -iw to set up step response
         IF (STEPC == 3) THEN
           YFRQ(1,JF) = REAL ( BFD1(JF,JR,JS,JC) )
         ELSE
           YFRQ(1,JF) = -AIMAG ( BFD1(JF,JR,JS,JC) ) / (TWOPI * FREQ(JF))
         END IF
       END DO
       CALL CUBSPL (WF,YFRQ,KFRQ)

       YSCAT = 0.

       DO JT = 1, NTYRP   !  Convert to step-function time-domain.
         T = TRP(JT)
         YSCAT(1,JT) = COSTRN (WF,YFRQ,KFRQ,T)
       END DO

       CALL FOLD_AND_CONVOLVE (STEPC,NSX,SWX,SWY,NPULS,PULSE,TRP,NTYRP,NTYPLS, &
                               NCHNL,TOPN,TCLS,YSCAT,YCUM)

       BTD1(1:NCHNL,JR,JS,JC) = YCUM(1:NCHNL)
     END DO
   END DO
 END DO

 DEALLOCATE (YSCAT, YFRQ)

END SUBROUTINE TDEM_3D1

 SUBROUTINE FOLD_AND_CONVOLVE (STEPC,NSX,SWX,SWY,NPULS,PULSE,TRP,NTYRP,NTYPLS, &
                               NCHNL,TOPN,TCLS,YPLS,YCUM)
!----------------------------------------------------------------------------

!  Computes the "observed" response YCUM by convolving the splined earth
!  response function, YPLS, with the TX waveform.

!***  Called by HSBOSS and TDEM_3D
!***  Calls: CUBVAL, CUBSPL, TXCNVL

!    STEPC = 1 => average convolved response over receiver windows.  This will
!                 be the case when input and desired output are both either
!                 step response or impulse response.
!          = 0 => difference convolved response over receiver windows. This will
!                 be the case when input is step and desired output is impulse.
!          = 3 => electrode voltage convolution with current
!          = 4 for UTEM pure step response
!      NSX - number of points used to discretise transmitter signal
!      SWX - abscissae (seconds) of current waveform
!      SWY - dI/dt at times SWX
!    NPULS - number of bipolar pulses of length PULSE
!    PULSE - length single on pulse plus off-time
!    NTYRP - number of values in TRP for total signal length: 2 * NPULS *PULSE
!      TRP - array of time values for FD -> TD transformations
!   NTYPLS - number of TRP values in 1 PULSE
!    NCHNL - number of channels
!     TOPN - time at which receiver channel I opens.
!     TCLS - time at which receiver channel I closes.
!

 IMPLICIT NONE
 INTEGER JT,NTYRP,NTYPLS,NPULS,NSX,NCHNL,JGL,JP,STEPC,MXCNV
 REAL PULSE,TRP(NTYRP),SWX(NSX),SWY(NSX,3),TOPN(NCHNL),TCLS(NCHNL),T1,T2,WIDTH, &
      HWIDTH,YC1,YC2,TC(3),GLX(3),GLW(3),YPLS(4,NTYRP),X,XP,YCUM(NCHNL),CUBVAL, &
      TXCNVL,TXCNVD,WT,FOLD(NTYRP)
 DATA GLW(1:3) /.5555556, .8888889, .5555556/, GLX(1:3) /-.7745967, 0., .7745967/

!  Accumulate the results of NPULS bipolar cycles by splining the instantaneous
!  response and folding the positive and negative parts of each cycle back
!  into a single pulse.  For on-time systems, compute a correction factor.
!  which will be based on the negative instantaneous response at .1 microsecond.

 CALL CUBSPL (TRP,YPLS,NTYRP)
 FOLD = 0.
 DO JT = 1,NTYPLS
   X = TRP(JT)
   XP = X + PULSE
   FOLD(JT) = CUBVAL (TRP,YPLS,NTYRP,X) - CUBVAL (TRP,YPLS,NTYRP,XP)
   FOLD(JT) = CUBVAL (TRP,YPLS,NTYRP,X) - CUBVAL (TRP,YPLS,NTYRP,XP)

   DO JP = 2, NPULS
     X = XP + PULSE
     XP = X + PULSE
     FOLD(JT) = CUBVAL (TRP,YPLS,NTYRP,X) - CUBVAL (TRP,YPLS,NTYRP,XP) &
              + FOLD(JT)
   END DO
 END DO

 YPLS = 0.
 YPLS(1,1:NTYPLS) = FOLD(1:NTYPLS)
 CALL CUBSPL (TRP,YPLS,NTYPLS)
 YCUM = 0.

 MXCNV = NTYPLS + NSX
 DO JT = 1, NCHNL
   T1 = TOPN(JT)
   T2 = TCLS(JT)
   WIDTH = T2 - T1
   HWIDTH = WIDTH /2.

   IF (STEPC == 0) THEN  ! Differentiate to compute impulse response for step input
     YC1 = TXCNVL (T1,NTYPLS,TRP,YPLS,NSX,SWX,SWY)
     YC2 = TXCNVL (T2,NTYPLS,TRP,YPLS,NSX,SWX,SWY)
     YCUM(JT) = (YC1 - YC2) / WIDTH

   ELSE

! Step response for step input or impulse response for impulse input
! Average the response over receiver windows using 3 point Gaussian integration.

     TC(2) = (TCLS(JT) + TOPN(JT)) /2.
     TC(1) = TC(2) + HWIDTH * GLX(1)
     TC(3) = TC(2) + HWIDTH * GLX(3)

     DO JGL = 1, 3
       T1 = TC(JGL)
       WT = GLW(JGL) / 2.
       IF (STEPC == 1) THEN                                     ! Magnetic dipole or loop receiver
         YC1 = TXCNVL (T1,NTYPLS,TRP,YPLS,NSX,SWX,SWY)
       ELSE IF (STEPC == 4) THEN                                ! pure on-time step
         YC1 = SWY(1,1) * CUBVAL (TRP,YPLS,NTYPLS,T1)
       ELSE IF (STEPC == 3) THEN                                ! Electrode receiver
         YC1 = TXCNVD (MXCNV,T1,NTYPLS,TRP,YPLS,NSX,SWX,SWY,3)  ! Current is in SWY(*,3)
       END IF
       YCUM(JT) = YCUM(JT) + (WT * YC1)
     END DO
   END IF
 END DO

 END SUBROUTINE FOLD_AND_CONVOLVE

 REAL FUNCTION TXCNVD (MXCNV,T,NTYPLS,TRP,YPLS,NSX,SWX,SWY,K1)
!-------------------------------------------------------------
!
!***  Called by FOLD_AND_CONVOLVE
!***  Calls CUBINT, CUBSPL, CUBVAL, LINVAL, TXCMRG

!  Convolves earth response function (ERF) with the source waveform
!  contained in SWY(*,K1) at NSX points to produce the system response
!  of the earth.  In Loki, it is used for electric field computations.
!  The source current function is contained in component K1 = 3
!
!       MXCNV = NTYPLS + NSX
!           T - convolution time in sec measured from the beginning
!               of the source waveform.
!   TRP, YPLS - abscissa & ordinate values of earth response function to
!               be convolved.
!      NTYPLS - number of values in TRP and YPLS
!         SWX - abscissa of time values of source waveform in sec.
!    SWY(*,K1) - source current values
!         NSX - number of points in SWX & in each waveform stored in SWY
!
!  Defining  T1 = MIN {T, signal length,}, the convolution is formally
!  computed as
!
!   TXCNVD (T) = INT (T0 -> T) { YPLS (tau) * SWY (T-tau)  d tau }

!  where T0 = MAX { TRP(1), T - SWX (NSX)}
!
!       ONTIME RESPONSE
!       ---------------
!  For response in the on-time period, ( T < signal length) a correction to
!  account for the response from 0 -> T0 is needed.  Analysis and subsequent
!  numerical experiments confirm that as T -> 0, step dB/dt -> A * T**(-1/2).
!  Thus ERFINT, the integral of YPLS from 0 to TRP(1), is simply
!  2 * TRP(1) * YPLS (TRP(1)) if TRP(1) is chosen sufficiently early.
!  The convolution correction factor is SWY(T) * ERFINT.

!  Alternatively, we can difference the step B field from 0 to TRP(1) which
!  is a lot easier since the step B field at T = 0 is simply the DC field due
!  to a transmitter image buried at z = ALT; i.e., the z+z' term.  In this case,
!  the bigger TRP(1) is, the more accurate the difference in B but this must be
!  sufficiently small so that the change in dI/dt is negligable.  Thus, TRP(1)
!  is chosen to be .1 microsecond.

 IMPLICIT NONE
 INTEGER K1,MXCNV,NTYPLS,NSX,N1,J1,N2,J2,NCNV
 REAL T,TC,T0,TRP(NTYPLS),YPLS(4,NTYPLS),SWX(NSX),SWY(NSX,3),YCNV(4,MXCNV), &
      XCNV(MXCNV),X1(MXCNV),Y1(MXCNV),X2(MXCNV),Y2(MXCNV),CUBVAL,CUBINT,LINVAL

 INTENT (IN) MXCNV,T,NTYPLS,TRP,YPLS,NSX,SWX,SWY

!  Set up X1,Y1, the N1 values of SWX, SWY * YPLS for signal ontime < T.
!  where X1, the conjugate signal time, contains T-SWX values.
!  Set up X2,Y2, the N2 values of TRP, YPLS * SWY for ERF points  <= T.

 TXCNVD = 0.0
 N1 = 0
 DO J1 = NSX, 1, -1
   TC = T - SWX(J1)
   IF (TC < 0.) CYCLE
   N1 = N1 + 1
   X1(N1) = TC
   Y1(N1) = SWY(J1,K1) * CUBVAL (TRP,YPLS,NTYPLS,TC)
 END DO

 T0 = T - SWX(NSX)
 T0 = MAX (T0, TRP(1))/ 1.0001
 N2 = 0
 DO J2 = 1,NTYPLS
   IF ((TRP(J2) > T0) .AND. (TRP(J2) < T)) THEN
     N2 = N2 + 1
     X2(N2) = TRP(J2)
     TC = T - TRP(J2)
     Y2(N2) = YPLS(1,J2) * LINVAL(NSX,SWX,SWY,K1,TC)
   END IF
 END DO

!  Merge the two lists into XCNV, YCNV of length NCNV.
!  Then spline and integrate

!+++++++++++++++++++++++++++++++++
 IF (N1 + N2 < 4) RETURN
!+++++++++++++++++++++++++++++++++

 CALL TXCMRG (MXCNV,X1,Y1,N1,X2,Y2,N2,XCNV,YCNV,NCNV)

!+++++++++++++++++++++++++++++++++
 IF (NCNV < 4) RETURN
!+++++++++++++++++++++++++++++++++

 CALL CUBSPL (XCNV,YCNV,NCNV)
 TXCNVD = CUBINT (XCNV,YCNV,NCNV,T0,T)

 END FUNCTION TXCNVD

 REAL FUNCTION TXCNVL (T,NTYPLS,TRP,YPLS,NSX,SWX,SWY)
!----------------------------------------------------

!***  Called by FOLD_AND_CONVOLVE
!***  Calls CUBINT, CUBVAL

!  Computes the system dB/dt response by convolving the computed dI/dt with
!  the impulse B response of the earth.  For step current drops, system dB/dt
!  is computed asthe product of instantaneous current drop times the
!  earth step dB/dt.

!  This routine assumes that the source waveform is composed of NSX linear
!  segments.  Thus NSX-1 constant dI/dt values are contained in SWY(*,1).

!  The input earth response function (step dB/dt or equivalently, impulse B)
!  must be contained in a splined array of NTYPLS values of time (abscissa) TRP
!  and ordinate YPLS.  System dB/dt is computed by integrating YPLS between
!  the SWX points of constant dI/dt segments.

!              T - convolution time in sec measured from the beginning
!                  of the source waveform.
!      TRP, YPLS - abscissa & ordinate values of earth response function to
!                  be convolved.
!         NTYPLS - number of values in TRP and YPLS
!            SWX - abscissa of time values of source waveform in sec.
!       SWY(*,1) - dI/dt if it exists (0 otherwise)
!       SWY(*,2) - first difference values of source waveform
!                  (-delta I) in amps.
!            NSX - number of points in SWX & 1

 IMPLICIT NONE
 REAL, PARAMETER :: T0_MIN=1.E-7
 INTEGER NTYPLS,NSX,JT
 REAL T,TF,CNV,TB,DELT,SEG,TRP(NTYPLS),YPLS(4,NTYPLS),SWX(NSX),SWY(NSX,3),TEND, &
      CUBINT,CUBVAL
 LOGICAL DER

 TF = T - TRP(1)
 CNV = 0.
 DO JT = 2, NSX
   IF (SWX(JT) < T0_MIN) CYCLE
   IF (SWX(JT-1) > TF) EXIT
   TB = T - MIN (TF, SWX(JT))
   DELT = SWX(JT) - SWX(JT-1)
   DER = .FALSE.
   IF (DELT > T0_MIN) THEN
     TEND = T - SWX(JT-1)
     DER = .TRUE.
   END IF

!  For an instantaneous step drop in current, SEG is YPLS times SWY(*,2),
!  since YPLS is already the dB/dt step response.  Otherwise SEG is the
!  integral of YPLS * constant dI/dt SWY(*,1) since YPLS is also impulse B.

   IF (DER) THEN
     SEG = SWY(JT,1) * CUBINT (TRP,YPLS,NTYPLS,TB,TEND)
   ELSE
     SEG = SWY(JT,2) * CUBVAL (TRP,YPLS,NTYPLS,TB)
   END IF
   CNV = CNV + SEG
 END DO
 TXCNVL = CNV

 END FUNCTION TXCNVL

 REAL FUNCTION LINVAL (NX,XVAL,YVAL,K1,X1)
!-----------------------------------------

!  Evaluates a function at X1 from from its linear representation.

!***  Called by TXCNVD
!
!***  Calls INTERV.  On exit from INTERV
!
!       MFLAG = -1  => X is to the left of interpolated range
!             =  1  => X is to the right of interpolated range
!             =  0  => X is in the interpolated range

!
!     XVAL(1:NX) - location of the abscissa knots.  The rightmost data point
!                  used to calculate coefficients is not included.
!
!  YVAL(1:NX,K1) = function values.
!
!
!     The value is a linear interpolation between the knots.
!
!             METHOD
!             ------
!
!  The interval index I, appropriate for X, is found through a call to INTERV.
!  The formula for F is evaluated using nested multiplication.
!******************************************************************************

 IMPLICIT NONE
 INTEGER K1,I,MFLAG,NX
 REAL XVAL(NX),YVAL(NX,3),X1,H

 INTENT (IN) NX,XVAL,YVAL,X1
!
!  Find index I of largest breakpoint to the left of X1.
!
 CALL INTERV ( XVAL, NX-1, X1, I, MFLAG )

 H = X1 - XVAL(I)
 IF (MFLAG == -1) H = 0.
 LINVAL = YVAL(I,K1) + H * (YVAL(I+1,K1) - YVAL(I,K1)) / (XVAL(I+1) - XVAL(I))

 END FUNCTION LINVAL


 SUBROUTINE TXCMRG (MXCNV,X1,Y1,N1,X2,Y2,N2,XCNV,YCNV,NCNV)
!----------------------------------------------------------

!  Merges two previously sorted list pairs X1, Y1 of length N1 and X2, Y2 of
!  length N2 into list pair XCNV, YCNV of length NCNV into ascending values of
!  XCNV.

!***  Called by TXCNVD

 IMPLICIT NONE
 REAL, PARAMETER :: TOL=1.E-3
 INTEGER MXCNV,N1,N2,NCNV,K1,K2,N,J1
 REAL DELT,TL1,XCNV(MXCNV),X1(MXCNV),Y1(MXCNV),X2(MXCNV),Y2(MXCNV),YCNV(4,MXCNV)
 LOGICAL LIST1, LIST2

 INTENT (IN) MXCNV,X1,Y1,N1,X2,Y2,N2
 INTENT (OUT) XCNV,YCNV,NCNV

 LIST1 = .TRUE.
 LIST2 = .TRUE.
 K1 = 1
 K2 = 1
 N = N1 + N2

 DO J1 = 1, N
   IF (LIST1 .AND. LIST2) THEN
     IF (X1(K1) < X2(K2)) THEN
       XCNV(J1) = X1(K1)
       YCNV(1,J1) = Y1(K1)
       K1 = K1 + 1
       IF (K1 > N1) LIST1 = .FALSE.
     ELSE
       XCNV(J1) = X2(K2)
       YCNV(1,J1) = Y2(K2)
       K2 = K2 + 1
       IF (K2 > N2) LIST2 = .FALSE.
     END IF
   ELSE IF (LIST1) THEN
     XCNV(J1) = X1(K1)
     YCNV(1,J1) = Y1(K1)
     K1 = K1 + 1
     IF (K1 > N1) LIST1 = .FALSE.
   ELSE IF (LIST2) THEN
     XCNV(J1) = X2(K2)
     YCNV(1,J1) = Y2(K2)
     K2 = K2 + 1
     IF (K2 > N2) LIST2 = .FALSE.
   END IF
 END DO

 NCNV = 1      !  Clean up list
 DO J1 = 2, N
   DELT = XCNV(J1) - XCNV(NCNV)
   TL1 = TOL * XCNV(J1)
   IF (DELT > TL1) THEN
     NCNV = NCNV + 1
     XCNV(NCNV) = XCNV(J1)
     YCNV(1,NCNV) = YCNV(1,J1)
   END IF
 END DO

 END SUBROUTINE TXCMRG

 SUBROUTINE VALIDATE_CMP (NLG,MXERR,JL,NLINES,CMP,NCMPL,ISYS)
!------------------------------------------------------------

 IMPLICIT NONE
 INTEGER NLG,MXERR,JL,NLINES,QCMP,ISYS
 INTEGER, DIMENSION(NLINES) :: CMP,NCMPL

 SELECT CASE (CMP(JL))
 CASE (1,2,3)
   QCMP = CMP(JL)
 CASE (12,21)
   QCMP = 12
 CASE (13,31)
   QCMP = 13
 CASE (23,32)
   QCMP = 23
 CASE (123,132,231,213,321,312)
   QCMP = 123
 CASE DEFAULT
   CALL WRITE_LOG_FILE (NLG,24,MXERR,2)
 END SELECT
 CMP(JL) = QCMP
 NCMPL(JL) = 1
 IF (CMP(JL) > 9) NCMPL(JL) = 2
 IF (CMP(JL) > 99) NCMPL(JL) = 3
 IF (ISYS == 2) THEN
   CMP(JL) = 1
   NCMPL(JL) = 1
 END IF

 END SUBROUTINE VALIDATE_CMP

 SUBROUTINE WRITE_TD (NW,np,KPRT,NCHNL,NLINES,MRXL,MCMP,NRX,SURVEY_TYPE,LINE,IDH,RX_TYPE,UNITS, &
                      SVAZM,TITLE,ISYS,PRFL,IPLT,YXZPLT,TMS,HEADER_ID,CMP,BFTL,RDATA,RWTS)
!---------------------------------------------------------------------------------------------

!  Writes time domain output to unit NW
!  If KPRT = 1, write error structure.
!
!*** Called by: MAIN
!***     Calls:
!
!   NW,np          : output unit numbers
!   KPRT            = 0 => write model data only
!                   = 1 write model data plus error structurete
!                   = -1 write inversion data only
!   NLINES          : number of lines
!   MRXL            : maximum number of receivers per line
!   MCMP            = 1 for coincident loop;  = 3 otherwise
!   NRX             : number of receivers per line
!   NCHNL           : number of channels
!   SURVEY_TYPE     = 1: general transmitter with independent receiver arrays
!                   = 2: moving rectangular loop with one or more fixed offset MD receivers
!                   = 3: fixed offset MD - MD surface surface
!                   = 4: coincident Loop
!                   = 5: downhole MD-MD probe
!   LINE(L)         : line number of Line L
!   UNITS(L)        : unit designater of Line L
!   IDH(L)          : surface (0);  local U,V,A (1);  modified U,V,A (2)
!   RXID(L)         : receiver type Line L - mag dipole (1); electric dipole (2)
!   UNITS(L)        : integer uniot designator = see SUBROUTINE GET_UNITS_TEXT
!   SVAZM(L)        : survey aximuth for Line L (radians)
!   PRFL            : 1 for profile output; = 0 for temporal output
!   SVAZM           : survey azimuth (radians) of Line L
!   ISYS            : UTEM output if ISYS = 4
!   IPLT(J,L)       : the response of Rx J of Line L is plotted at 1:Rx;  2:Tx-Rx midpoint;  3:Tx midpoint
!   YXZPLT(1:3,I,L) : GPS east, GPS north, RL  plot coordinate for Ith receiver of Line L
!   TMS             : time midpoints (ms) for NCHNL windows
!   HEADER_ID       : used to relate header to RX_TYPE and plot orientation
!   CMP             : component selection
!   BFTL(I,J,K,L)   : the Kth component measured response at frequency I from receiver J,
!                     of Line L, transmitter K (modelled)
!   RDATA(I,J,K,L)  : the Kth component measured response at frequency I from receiver J,
!                     of Line L, transmitter K (inversion data)
!   RWTS(I,J,K,L)   : inversion weights corresponding to RDATA

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80)
 REAL, PARAMETER :: R2D= 180./3.14159
 INTEGER NW,np,KPRT,IDT,NLINES,MRXL,MCMP,NCHNL,PRFL,SURVEY_TYPE,ISYS,KDEG,NRX1,JL,JR,LC,JT
 INTEGER, DIMENSION(NLINES) :: NRX,CMP,LINE,UNITS,RX_TYPE,HEADER_ID,IDH,IPLT
 INTEGER RWTS(NCHNL,MRXL,MCMP,NLINES)
 REAL, DIMENSION(NCHNL,MRXL,MCMP,NLINES) :: BFTL,RDATA
 REAL SVAZM(NLINES),TMS(NCHNL),YTR(NCHNL,MRXL)
 REAL(KIND=QL) YXZPLT(3,MRXL,NLINES),RXPLT(3,MRXL)
 CHARACTER(LEN=120) TITLE
 CHARACTER(LEN=20) UTXT(2),CTXT(2),PLT_PT(3),QL0,DTYPE(4)
 DATA PLT_PT /'Rx','Tx-Rx_MID','Tx'/
 DATA DTYPE /'model output','data','Model Output','Data'/

 IDT = 1
 IF (KPRT == -1) THEN
   IDT = 2
   BFTL = RDATA
 END IF
 IF (KPRT == 0) THEN             !  No inversion
   WRITE(NW,'(/T3,A)') TITLE
   IF (ISYS == 4) THEN
     WRITE(NW,7)
   ELSE
     IF (SURVEY_TYPE == 1) WRITE(NW,1) TRIM (ADJUSTL (DTYPE(IDT)))
     IF (SURVEY_TYPE == 2) WRITE(NW,2) TRIM (ADJUSTL (DTYPE(IDT)))
     IF (SURVEY_TYPE == 3) WRITE(NW,3) TRIM (ADJUSTL (DTYPE(IDT)))
     IF (SURVEY_TYPE == 4) WRITE(NW,4) TRIM (ADJUSTL (DTYPE(IDT)))
     IF (SURVEY_TYPE == 5) WRITE(NW,5) TRIM (ADJUSTL (DTYPE(IDT)))
     WRITE(NW,6)
   END IF
 END IF

 IDT = 3
 IF (KPRT == -1) IDT = 4
 DO JL = 1,NLINES
   LC = CMP(JL)

   CALL GET_UNITS_TEXT (UNITS(JL),UTXT)
   WRITE(QL0,*) LINE(JL)
   READ(QL0,'(A)') CTXT(1)           ! Line number
   CTXT(2) = PLT_PT(IPLT(JL)+1)
   KDEG = INT (R2D * SVAZM(JL))
   WRITE(np,16) TRIM (ADJUSTL (CTXT(1))),HEADER_ID(JL),KDEG,TRIM(UTXT(2)),TRIM (ADJUSTL (CTXT(2)))
   IF (RX_TYPE(JL) == 2) THEN
     WRITE(NW,12) TRIM (ADJUSTL (CTXT(1))),TRIM(UTXT(1)),TRIM (ADJUSTL (CTXT(2)))
   ELSE
     IF (SURVEY_TYPE == 1) THEN
       WRITE(NW,11) TRIM (ADJUSTL (CTXT(1))),KDEG,TRIM(UTXT(1)),TRIM (ADJUSTL (CTXT(2)))
     ELSE
       WRITE(NW,10) TRIM (ADJUSTL (CTXT(1))),KDEG,TRIM(UTXT(1)),TRIM (ADJUSTL (CTXT(2)))
     END IF
   END IF

   NRX1 = NRX(JL)
   DO JR = 1,NRX1
     RXPLT(1:3,JR) = YXZPLT(1:3,JR,JL)
   END DO

   IF (RX_TYPE(JL) == 1) THEN            ! magnetic dipole output
     DO JR = 1,NRX1
       SELECT CASE (CMP(JL))
       CASE(1)
         WRITE(np,15) JR,YXZPLT(1:3,JR,JL), BFTL(1:NCHNL,JR,1,JL)
       CASE(2)
         WRITE(np,15) JR,YXZPLT(1:3,JR,JL), BFTL(1:NCHNL,JR,2,JL)
       CASE(3)
         WRITE(np,15) JR,YXZPLT(1:3,JR,JL), BFTL(1:NCHNL,JR,3,JL)
       CASE(12)
         WRITE(np,15) JR,YXZPLT(1:3,JR,JL), BFTL(1:NCHNL,JR,1,JL), BFTL(1:NCHNL,JR,2,JL)
       CASE(13)
         WRITE(np,15) JR,YXZPLT(1:3,JR,JL), BFTL(1:NCHNL,JR,1,JL), BFTL(1:NCHNL,JR,3,JL)
       CASE(23)
         WRITE(np,15) JR,YXZPLT(1:3,JR,JL), BFTL(1:NCHNL,JR,2,JL), BFTL(1:NCHNL,JR,3,JL)
       CASE(123)
         WRITE(np,15) JR,YXZPLT(1:3,JR,JL), BFTL(1:NCHNL,JR,1,JL), BFTL(1:NCHNL,JR,2,JL), BFTL(1:NCHNL,JR,3,JL)
       END SELECT
     END DO

     IF (LC == 3 .OR. LC == 13 .OR. LC == 23 .OR. LC == 123) THEN
       SELECT CASE (IDH(JL))               ! Z, A or W component
       CASE (0)
         WRITE(NW,23) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       CASE (1)
         WRITE(NW,26) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       CASE (2)
         WRITE(NW,29) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       END SELECT
       DO JR = 1,NRX1
         YTR(1:NCHNL,JR) = BFTL(1:NCHNL,JR,3,JL)
       END DO
       CALL WRTDP (NW,PRFL,NCHNL,TMS,MRXL,NRX1,RXPLT,YTR)
       IF (KPRT == 1) CALL WRITE_TD_MISFIT (3)
     END IF

     IF (LC == 1 .OR. LC == 13 .OR. LC == 12 .OR. LC == 123) THEN
       SELECT CASE (IDH(JL))               ! X, U or S component
       CASE (0)
         WRITE(NW,21) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       CASE (1)
         WRITE(NW,24) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       CASE (2)
         WRITE(NW,27) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       END SELECT
       DO JR = 1,NRX1
         YTR(1:NCHNL,JR) = BFTL(1:NCHNL,JR,1,JL)
       END DO
       CALL WRTDP (NW,PRFL,NCHNL,TMS,MRXL,NRX1,RXPLT,YTR)
       IF (KPRT == 1) CALL WRITE_TD_MISFIT (1)
     END IF

     IF (LC == 2 .OR. LC == 12 .OR. LC == 23 .OR. LC == 123) THEN
       SELECT CASE (IDH(JL))               ! Y, V or N component
       CASE (0)
         WRITE(NW,22) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       CASE (1)
         WRITE(NW,25) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       CASE (2)
         WRITE(NW,28) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       END SELECT
       DO JR = 1,NRX1
         YTR(1:NCHNL,JR) = BFTL(1:NCHNL,JR,2,JL)
       END DO
       CALL WRTDP (NW,PRFL,NCHNL,TMS,MRXL,NRX1,RXPLT,YTR)
       IF (KPRT == 1) CALL WRITE_TD_MISFIT (2)
     END IF

   ELSE                                  ! Coincident loop or electric dipole output
     WRITE(NW,30) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
     DO JR = 1,NRX1
       YTR(1:NCHNL,JR) = BFTL(1:NCHNL,JR,1,JL)
       WRITE(np,15) JR,YXZPLT(1:3,JR,JL), YTR(1:NCHNL,JR)
     END DO
     CALL WRTDP (NW,PRFL,NCHNL,TMS,MRXL,NRX1,RXPLT,YTR)
     IF (KPRT == 1) CALL WRITE_TD_MISFIT (1)
   END IF
 END DO

  1 FORMAT(/T3,'Time-domain ',A,' for transmitter(s) with independent receiver lines.')
  2 FORMAT(/T3,'Time-domain ',A,' for moving rectangular loop Tx with fixed offset receiver(s).')
  3 FORMAT(/T3,'Time-domain ',A,' for moving dipole Tx with fixed offset receiver(s).')
  4 FORMAT(/T3,'Time-domain ',A,' for rectangular coincident loop survey.')
  5 FORMAT(/T3,'Time-domain ',A,' for downhole Tx-Rx probe.')
  6 FORMAT( T3,'Channels are ordered from earliest time to latest time.')
  7 FORMAT(/T3,'UTEM Survey:  Channels are ordered from latest time to earliest time.' &
           /T3,'Channel 1 response is subtracted from the other channels.')
 10 FORMAT(/T3,'Line ',A,4X,'Survey azimuth =',I4,' degrees',4X,'Units = ',A,4X,'Plot point: ',A9)
 11 FORMAT(/T3,'Line ',A,4X,'Magnetic dipole Rx    Survey azimuth =',I4,' degrees',4X,'Units = ',A,4X,'Plot point: ',A9)
 12 FORMAT(/T3,'Line ',A,4X,'Electric dipole Rx',4X,'Units = ',A,4X,'Plot point: ',A9)
 15 FORMAT(I5,2F12.1,F9.1,150G13.4)
 16 FORMAT(/T3,'Line ',A,4X,'HID:',I4,4X,'SVAZ:',I4,4X,'Units: ',A,4X,'PP: ',A9)
 21 FORMAT(/T10,'X : Radial Component ',A,' for Line ',A &
           /T10,'--------------------------------------------')

 22 FORMAT(/T10,'Y : Tangential Component ',A,' for Line ',A &
           /T10,'------------------------------------------------')
 23 FORMAT(/T10,'Z : Vertical Component ',A,' for Line ',A &
           /T10,'----------------------------------------------')
 24 FORMAT(/T10,'U : Slope Component ',A,' for Line ',A &
           /T10,'-----------------------------------------------')
 25 FORMAT(/T10,'V : Horizontal Component ',A,' for Line ',A &
           /T10,'------------------------------------------------')
 26 FORMAT(/T10,'A : Axial Component ',A,' for Line ',A &
           /T10,'-------------------------------------------')
 27 FORMAT(/T10,'S : In-section Component ',A,' for Line ',A &
           /T10,'------------------------------------------------')
 28 FORMAT(/T10,'N : Out-section Component ',A,' for Line ',A &
           /T10,'-------------------------------------------------')
 29 FORMAT(/T10,'W : Axial Component ',A,'for Line ',A &
           /T10,'------------------------------------------')
 30 FORMAT(/T10,'Coincident Loop ',A,'for Line ',A &
           /T10,'--------------------------------------')

 CONTAINS

   SUBROUTINE WRITE_TD_MISFIT (JC)
!  ------------------------------

   IMPLICIT NONE
   INTEGER JC
   REAL DENOM,VM,VD

   DO JR = 1,NRX1
     DO JT = 1,NCHNL
       YTR(JT,JR) = 0.
       VD = RDATA(JT,JR,JC,JL)
       VM = BFTL(JT,JR,JC,JL)
       DENOM = SQRT( (VM*VM + VD*VD)/2.0)
       IF (DENOM > 0 .AND. RWTS(JT,JR,JC,JL) > 0 ) YTR(JT,JR) = 100. * (VD - VM) / DENOM
     END DO
   END DO

   WRITE(NW,1) JC
   CALL WRTDP (NW,PRFL,NCHNL,TMS,MRXL,NRX1,RXPLT,YTR)

 1 FORMAT(/T10,'Misfit for Component',I2/T10,'----------------------')

   END SUBROUTINE WRITE_TD_MISFIT

 END SUBROUTINE WRITE_TD

 SUBROUTINE WRTDP (NW,PRFL,NCHNL,TMS,MRXL,NRX1,RXPLT,YTR)
!--------------------------------------------------------

!***  Called by: WRITE_TD
!***      CallS: nil

!  Writes time-domain output in profile form.

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80)
 INTEGER NW,PRFL,NCHNL,MRXL,NRX1,JR,JT
 REAL TMS(NCHNL),YTR(NCHNL,MRXL)
 REAL(KIND=QL) RXPLT(3,MRXL)

 CHARACTER(LEN=8) CHN(150)
 DATA CHN(1:150) &
   /' CHNL 1 ',' CHNL 2 ',' CHNL 3 ',' CHNL 4 ',' CHNL 5 ',' CHNL 6 ', &
    ' CHNL 7 ',' CHNL 8 ',' CHNL 9 ','CHNL 10 ','CHNL 11 ','CHNL 12 ', &
    'CHNL 13 ','CHNL 14 ','CHNL 15 ','CHNL 16 ','CHNL 17 ','CHNL 18 ', &
    'CHNL 19 ','CHNL 20 ','CHNL 21 ','CHNL 22 ','CHNL 23 ','CHNL 24 ', &
    'CHNL 25 ','CHNL 26 ','CHNL 27 ','CHNL 28 ','CHNL 29 ','CHNL 30 ', &
    'CHNL 31 ','CHNL 32 ','CHNL 33 ','CHNL 34 ','CHNL 35 ','CHNL 36 ', &
    'CHNL 37 ','CHNL 38 ','CHNL 39 ','CHNL 40 ','CHNL 41 ','CHNL 42 ', &
    'CHNL 43 ','CHNL 44 ','CHNL 45 ','CHNL 46 ','CHNL 47 ','CHNL 48 ', &
    'CHNL 49 ','CHNL 50 ','CHNL 51 ','CHNL 52 ','CHNL 53 ','CHNL 54 ', &
    'CHNL 55 ','CHNL 56 ','CHNL 57 ','CHNL 58 ','CHNL 59 ','CHNL 60 ', &
    'CHNL 61 ','CHNL 62 ','CHNL 63 ','CHNL 64 ','CHNL 65 ','CHNL 66 ', &
    'CHNL 67 ','CHNL 68 ','CHNL 69 ','CHNL 70 ','CHNL 71 ','CHNL 72 ', &
    'CHNL 73 ','CHNL 74 ','CHNL 75 ','CHNL 76 ','CHNL 77 ','CHNL 78 ', &
    'CHNL 79 ','CHNL 80 ','CHNL 81 ','CHNL 82 ','CHNL 83 ','CHNL 84 ', &
    'CHNL 85 ','CHNL 86 ','CHNL 87 ','CHNL 88 ','CHNL 89 ','CHNL 90 ', &
    'CHNL 91 ','CHNL 92 ','CHNL 93 ','CHNL 94 ','CHNL 95 ','CHNL 96 ', &
    'CHNL 97 ','CHNL 98 ','CHNL 99 ','CHNL 100','CHNL 101','CHNL 102', &
    'CHNL 103','CHNL 104','CHNL 105','CHNL 106','CHNL 107','CHNL 108', &
    'CHNL 109','CHNL 110','CHNL 111','CHNL 112','CHNL 113','CHNL 114', &
    'CHNL 115','CHNL 116','CHNL 117','CHNL 118','CHNL 119','CHNL 120', &
    'CHNL 121','CHNL 122','CHNL 123','CHNL 124','CHNL 125','CHNL 126', &
    'CHNL 127','CHNL 128','CHNL 129','CHNL 130','CHNL 131','CHNL 132', &
    'CHNL 133','CHNL 134','CHNL 135','CHNL 136','CHNL 137','CHNL 138', &
    'CHNL 139','CHNL 140','CHNL 141','CHNL 142','CHNL 143','CHNL 144', &
    'CHNL 145','CHNL 146','CHNL 147','CHNL 148','CHNL 149','CHNL 150'/

 IF (PRFL == 0) THEN  ! Forget profiles, write output in temporal form.
   WRITE(NW,4)
   WRITE(NW,5) RXPLT(1,1:NRX1)
   WRITE(NW,6) RXPLT(2,1:NRX1)
   WRITE(NW,7) RXPLT(3,1:NRX1)
   WRITE(NW,'(3X)')

   DO JT = 1,NCHNL
     WRITE(NW,'(I4,F10.3,T18,1024G13.4)') JT,TMS(JT),YTR(JT,1:NRX1)
   END DO
 ELSE

   WRITE(NW,1) CHN(1:NCHNL)
   WRITE(NW,2) TMS(1:NCHNL)
   WRITE(NW,'(3X)')
   DO JR = 1, NRX1
     WRITE(NW,3) JR,RXPLT(1:3,JR),YTR(1:NCHNL,JR)
   END DO
 END IF

 1 FORMAT(/T11,'RECEIVER POSITIONS',6X,1024(:5X,A))
 2 FORMAT(T9,'Easting    Northing     Elev',T37,1024G13.4)
 3 FORMAT(I3,2F12.1,F9.1,1024G13.4)
 4 FORMAT(/T20,'RECEIVER COORDINATES (Top to Bottom): Easting, Northing, Elevation')
 5 FORMAT(/T2,'       Window',T16,1024F13.2)
 6 FORMAT( T2,'      Centres',T16,1024F13.2)
 7 FORMAT( T2,'Chnl   (ms)  ',T16,1024F13.2)

 END SUBROUTINE WRTDP

 SUBROUTINE WRITE_FD (NW,np,KPRT,NFRQ,MCHNL,NLINES,MRXL,MCMP,NRX,SURVEY_TYPE,LINE,IDH,RX_TYPE,UNITS, &
                      SVAZM,TITLE,ISYS,PRFL,IPLT,YXZPLT,FREQ,HEADER_ID,CMP,BFTL,RDATA,RWTS)
!--------------------------------------------------------------------------------------------------

!  Writes frequency-domain output to unit NW
!  If KPRT = 1, write error structure.
!
!*** Called by: MAIN
!***     Calls:
!
!   NW, np         : output unit numbers for .out & .mf1 files
!   KPRT            = 0 => write model data only
!                   = 1 write model data plus error structurete
!                   = -1 write inversion data only
!   NFRQ            : number of frequencies
!   MCHNL           = NFRQ for Sampo;  = 2*NFRQ otherwise
!   NLINES          : number of lines
!   MRXL            : maximum number of receivers per line
!   MCMP            = 1 for Sampo;  = 3 otherwise
!   NRX             : number of receivers per line
!   SURVEY_TYPE     = 1: general transmitter with independent receiver arrays
!                   = 2: moving rectangular loop with one or more fixed offset MD receivers
!                   = 3: fixed offset MD - MD surface surface
!                   = 4: coincident Loop
!                   = 5: downhole MD-MD probe
!   LINE(L)         : line number of Line L
!   UNITS(L)        : unit designater of Line L
!   IDH(L)          : surface (0);  local U,V,A (1);  modified U,V,A (2)
!   RXID(L)         : receiver type Line L - mag dipole (1); electric dipole (2)
!   SVAZM(L)        : survey aximuth for Line L (radians)
!   PRFL            : 1 for profile output; = 0 for frequency output
!   SVAZM           : survey azimuth (radians) of Line L
!   ISYS            : Sampo output if ISYS = 2
!   IPLT(J,L)       : the response of Rx J of Line L is plotted at 1:Rx;  2:Tx-Rx midpoint;  3:Tx midpoint
!   YXZPLT(1:3,I,L) : GPS east, GPS north, RL  plot coordinate for Ith receiver of Line L
!   FREQ            : frequency array
!   HEADER_ID       : used to relate header to RX_TYPE and plot orientation
!   CMP             : component selection
!
!   BFTL(I,J,K,L)   : the Kth component measured response at frequency I from receiver J,
!                     of Line L, transmitter K (modelled)
!   RDATA(I,J,K,L)  : the Kth component measured response at frequency I from receiver J,
!                     of Line L, transmitter K (inversion data)
!   RWTS(I,J,K,L)   : inversion weights corresponding to RDATA

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80)
 REAL, PARAMETER :: R2D= 180./3.14159
 INTEGER NW,np,IDT,MCHNL,MCMP,KPRT,NLINES,MRXL,NFRQ,PRFL,SURVEY_TYPE,ISYS,KDEG,NRX1,JL,JR,IPPM,LC
 INTEGER, DIMENSION(NLINES) :: NRX,CMP,LINE,UNITS,RX_TYPE,HEADER_ID,IDH,IPLT
 INTEGER RWTS(MCHNL,MRXL,MCMP,NLINES)
 REAL, DIMENSION(MCHNL,MRXL,MCMP,NLINES) :: BFTL,RDATA
 REAL SVAZM(NLINES),FREQ(NFRQ),YTR(NFRQ,MRXL)
 REAL(KIND=QL) YXZPLT(3,MRXL,NLINES),RXPLT(3,MRXL)
 LOGICAL SPIT
 CHARACTER(LEN=120) TITLE
 CHARACTER(LEN=20) UTXT(2),QL0,CTXT(2),PLT_PT(3),DTYPE(4)
 DATA PLT_PT /'Rx','Tx-Rx_MID','Tx'/
 DATA DTYPE /'model output','data','Model Output','Data'/

 IDT = 1
 IF (KPRT == -1) THEN
   IDT = 2
   BFTL = RDATA
 END IF
 IF (KPRT == 0) WRITE(NW,'(/T3,A)') TITLE
 IF (ISYS == 4) THEN
   WRITE(NW,7)
 ELSE
   IF (SURVEY_TYPE == 1) WRITE(NW,1) TRIM (ADJUSTL (DTYPE(IDT)))
   IF (SURVEY_TYPE == 2) WRITE(NW,2) TRIM (ADJUSTL (DTYPE(IDT)))
   IF (SURVEY_TYPE == 3) WRITE(NW,3) TRIM (ADJUSTL (DTYPE(IDT)))
   IF (SURVEY_TYPE == 5) WRITE(NW,5) TRIM (ADJUSTL (DTYPE(IDT)))
 END IF

 IDT = 3
 IF (KPRT == -1) IDT = 4

 DO JL = 1,NLINES
   LC = CMP(JL)
   IPPM = 0
   IF (UNITS(JL) > 30 .AND. UNITS(JL) < 39) IPPM = 1  ! Format control
   IF (ISYS == 2) IPPM = 0
   CALL GET_UNITS_TEXT (UNITS(JL),UTXT)
   WRITE(QL0,*) LINE(JL)
   READ(QL0,'(A)') CTXT(1)
   CTXT(2) = PLT_PT(IPLT(JL))
   KDEG = INT (R2D * SVAZM(JL))
   WRITE(np,17) TRIM (ADJUSTL (CTXT(1))),HEADER_ID(JL),KDEG,TRIM(UTXT(2)),TRIM (ADJUSTL (CTXT(2)))

   IF (RX_TYPE(JL) == 2) THEN
     WRITE(NW,12) TRIM (ADJUSTL (CTXT(1))),TRIM(UTXT(1))
   ELSE IF (RX_TYPE(JL) == 3) THEN
     WRITE(NW,13) TRIM (ADJUSTL (CTXT(1))),TRIM(UTXT(1))
   ELSE
     IF (SURVEY_TYPE == 1) THEN
       WRITE(NW,11) TRIM (ADJUSTL (CTXT(1))),KDEG,TRIM(UTXT(1))
     ELSE
       WRITE(NW,10) TRIM (ADJUSTL (CTXT(1))),KDEG,TRIM(UTXT(1))
     END IF
   END IF

   NRX1 = NRX(JL)
   DO JR = 1,NRX1
     RXPLT(1:3,JR) = YXZPLT(1:3,JR,JL)
   END DO

   SPIT = .FALSE.
   IF (RX_TYPE(JL) == 1 .AND. ISYS < 2) SPIT = .TRUE.
   IF (RX_TYPE(JL) == 3) SPIT = .TRUE.
   IF (SPIT) THEN
     DO JR = 1,NRX1
       IF (IPPM == 1) THEN
         SELECT CASE (CMP(JL))
         CASE(1)
           WRITE(np,16) JR,YXZPLT(1:3,JR,JL), BFTL(1:NFRQ,JR,1,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,1,JL)
         CASE(2)
           WRITE(np,16) JR,YXZPLT(1:3,JR,JL), BFTL(1:NFRQ,JR,2,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,2,JL)
         CASE(3)
           WRITE(np,16) JR,YXZPLT(1:3,JR,JL), BFTL(1:NFRQ,JR,3,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,3,JL)
         CASE(12)
           WRITE(np,16) JR,YXZPLT(1:3,JR,JL), BFTL(1:NFRQ,JR,1,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,1,JL), &
                                               BFTL(1:NFRQ,JR,2,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,2,JL)
         CASE(13)
           WRITE(np,16) JR,YXZPLT(1:3,JR,JL), BFTL(1:NFRQ,JR,1,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,1,JL), &
                                               BFTL(1:NFRQ,JR,3,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,3,JL)
         CASE(23)
           WRITE(np,16) JR,YXZPLT(1:3,JR,JL), BFTL(1:NFRQ,JR,2,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,2,JL), &
                                               BFTL(1:NFRQ,JR,3,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,3,JL)
         CASE(123)
           WRITE(np,16) JR,YXZPLT(1:3,JR,JL), BFTL(1:NFRQ,JR,1,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,1,JL), &
                                               BFTL(1:NFRQ,JR,2,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,2,JL), &
                                               BFTL(1:NFRQ,JR,3,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,3,JL)
         END SELECT
       ELSE
         SELECT CASE (CMP(JL))
         CASE(1)
           WRITE(np,15) JR,YXZPLT(1:3,JR,JL), BFTL(1:NFRQ,JR,1,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,1,JL)
         CASE(2)
           WRITE(np,15) JR,YXZPLT(1:3,JR,JL), BFTL(1:NFRQ,JR,2,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,2,JL)
         CASE(3)
           WRITE(np,15) JR,YXZPLT(1:3,JR,JL), BFTL(1:NFRQ,JR,3,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,3,JL)
         CASE(12)
           WRITE(np,15) JR,YXZPLT(1:3,JR,JL), BFTL(1:NFRQ,JR,1,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,1,JL), &
                                               BFTL(1:NFRQ,JR,2,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,2,JL)
         CASE(13)
           WRITE(np,15) JR,YXZPLT(1:3,JR,JL), BFTL(1:NFRQ,JR,1,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,1,JL), &
                                               BFTL(1:NFRQ,JR,3,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,3,JL)
         CASE(23)
           WRITE(np,15) JR,YXZPLT(1:3,JR,JL), BFTL(1:NFRQ,JR,2,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,2,JL), &
                                               BFTL(1:NFRQ,JR,3,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,3,JL)
         CASE(123)
           WRITE(np,15) JR,YXZPLT(1:3,JR,JL), BFTL(1:NFRQ,JR,1,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,1,JL), &
                                               BFTL(1:NFRQ,JR,2,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,2,JL), &
                                               BFTL(1:NFRQ,JR,3,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,3,JL)
         END SELECT
       END IF
     END DO
   END IF

   IF (RX_TYPE(JL) == 1 .AND. ISYS < 2) THEN            ! 3 component magnetic dipole output
     IF (LC == 3 .OR. LC == 13 .OR. LC == 23 .OR. LC == 123) THEN
       SELECT CASE (IDH(JL))               ! Z, A or W Inphase component
       CASE (0)
         WRITE(NW,23) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       CASE (1)
         WRITE(NW,26) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       CASE (2)
         WRITE(NW,29) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       END SELECT
       DO JR = 1,NRX1
         YTR(1:NFRQ,JR) = BFTL(1:NFRQ,JR,3,JL)
       END DO
       CALL WRFDP (NW,PRFL,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)
       IF (KPRT == 1) CALL WRITE_FD_MISFIT (3,1)

       SELECT CASE (IDH(JL))               ! Z, A or W Quadrature component
       CASE (0)
         WRITE(NW,33) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       CASE (1)
         WRITE(NW,36) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       CASE (2)
         WRITE(NW,39) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       END SELECT
       DO JR = 1,NRX1
         YTR(1:NFRQ,JR) = BFTL(NFRQ+1:2*NFRQ,JR,3,JL)
       END DO
       CALL WRFDP (NW,PRFL,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)
       IF (KPRT == 1) CALL WRITE_FD_MISFIT (3,2)
     END IF

     IF (LC == 1 .OR. LC == 13 .OR. LC == 12 .OR. LC == 123) THEN
       SELECT CASE (IDH(JL))               ! X, U or S Inphase component
       CASE (0)
         WRITE(NW,21) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       CASE (1)
         WRITE(NW,24) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       CASE (2)
         WRITE(NW,27) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       END SELECT
       DO JR = 1,NRX1
         YTR(1:NFRQ,JR) = BFTL(1:NFRQ,JR,1,JL)
       END DO
       IF (KPRT == 1) CALL WRITE_FD_MISFIT (1,1)
       CALL WRFDP (NW,PRFL,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)

       SELECT CASE (IDH(JL))               ! X, U or S Quadrature component
       CASE (0)
         WRITE(NW,31) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       CASE (1)
         WRITE(NW,34) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       CASE (2)
         WRITE(NW,37) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       END SELECT
       DO JR = 1,NRX1
         YTR(1:NFRQ,JR) = BFTL(NFRQ+1:2*NFRQ,JR,1,JL)
       END DO
       CALL WRFDP (NW,PRFL,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)
       IF (KPRT == 1) CALL WRITE_FD_MISFIT (1,2)
     END IF

     IF (LC == 2 .OR. LC == 12 .OR. LC == 23 .OR. LC == 123) THEN
       SELECT CASE (IDH(JL))               ! Y, V or N Inphase component
       CASE (0)
         WRITE(NW,22) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       CASE (1)
         WRITE(NW,25) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       CASE (2)
         WRITE(NW,28) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       END SELECT
       DO JR = 1,NRX1
         YTR(1:NFRQ,JR) = BFTL(1:NFRQ,JR,2,JL)
       END DO
       CALL WRFDP (NW,PRFL,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)
       IF (KPRT == 1) CALL WRITE_FD_MISFIT (2,1)

       SELECT CASE (IDH(JL))               ! Y, V or N Quadrature component
       CASE (0)
         WRITE(NW,32) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       CASE (1)
         WRITE(NW,35) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       CASE (2)
         WRITE(NW,38) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       END SELECT
       DO JR = 1,NRX1
         YTR(1:NFRQ,JR) = BFTL(NFRQ+1:2*NFRQ,JR,2,JL)
       END DO
       CALL WRFDP (NW,PRFL,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)
       IF (KPRT == 1) CALL WRITE_FD_MISFIT (2,2)
     END IF

   ELSE IF (RX_TYPE(JL) == 2) THEN                          ! electric dipole output
     DO JR = 1,NRX1
       WRITE(np,15) JR,YXZPLT(1:3,JR,JL), BFTL(1:NFRQ,JR,1,JL), BFTL(NFRQ+1:2*NFRQ,JR,1,JL)
     END DO

     WRITE(NW,20) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
     DO JR = 1,NRX1
       YTR(1:NFRQ,JR) = BFTL(1:NFRQ,JR,1,JL)
     END DO
     CALL WRFDP (NW,PRFL,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)
     IF (KPRT == 1) CALL WRITE_FD_MISFIT (1,1)
     WRITE(NW,30) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
     DO JR = 1,NRX1
       YTR(1:NFRQ,JR) = BFTL(NFRQ+1:2*NFRQ,JR,1,JL)
     END DO
     CALL WRFDP (NW,PRFL,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)
       IF (KPRT == 1) CALL WRITE_FD_MISFIT (1,2)

   ELSE IF (RX_TYPE(JL) == 3) THEN                          ! point electric field
     IF (LC == 1 .OR. LC == 13 .OR. LC == 12 .OR. LC == 123) THEN
       WRITE(NW,41) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       DO JR = 1,NRX1
         YTR(1:NFRQ,JR) = BFTL(1:NFRQ,JR,1,JL)
       END DO
       CALL WRFDP (NW,PRFL,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)
       IF (KPRT == 1) CALL WRITE_FD_MISFIT (1,1)

       WRITE(NW,44) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       DO JR = 1,NRX1
         YTR(1:NFRQ,JR) = BFTL(NFRQ+1:2*NFRQ,JR,1,JL)
       END DO
       CALL WRFDP (NW,PRFL,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)
       IF (KPRT == 1) CALL WRITE_FD_MISFIT (1,2)
     END IF

     IF (LC == 2 .OR. LC == 12 .OR. LC == 23 .OR. LC == 123) THEN
       WRITE(NW,42) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       DO JR = 1,NRX1
         YTR(1:NFRQ,JR) = BFTL(1:NFRQ,JR,2,JL)
       END DO
       CALL WRFDP (NW,PRFL,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)
       IF (KPRT == 1) CALL WRITE_FD_MISFIT (2,1)

       WRITE(NW,45) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       DO JR = 1,NRX1
         YTR(1:NFRQ,JR) = BFTL(NFRQ+1:2*NFRQ,JR,2,JL)
       END DO
       CALL WRFDP (NW,PRFL,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)
       IF (KPRT == 1) CALL WRITE_FD_MISFIT (2,2)
     END IF

     IF (LC == 3 .OR. LC == 13 .OR. LC == 23 .OR. LC == 123) THEN
       WRITE(NW,43) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       DO JR = 1,NRX1
         YTR(1:NFRQ,JR) = BFTL(1:NFRQ,JR,3,JL)
       END DO
       CALL WRFDP (NW,PRFL,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)
       IF (KPRT == 1) CALL WRITE_FD_MISFIT (3,1)

       WRITE(NW,46) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       DO JR = 1,NRX1
         YTR(1:NFRQ,JR) = BFTL(NFRQ+1:2*NFRQ,JR,3,JL)
       END DO
       CALL WRFDP (NW,PRFL,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)
       IF (KPRT == 1) CALL WRITE_FD_MISFIT (3,2)
     END IF

   ELSE IF (ISYS == 2) THEN                                 ! Sampo output
     DO JR = 1,NRX1
       WRITE(np,15) JR,YXZPLT(1:3,JR,JL), BFTL(1:NFRQ,JR,1,JL)
     END DO

     WRITE(NW,40) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
     DO JR = 1,NRX1
       YTR(1:NFRQ,JR) = BFTL(1:NFRQ,JR,1,JL)
     END DO
     CALL WRFDP (NW,PRFL,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)
     IF (KPRT == 1) CALL WRITE_FD_MISFIT (1,3)

   END IF
 END DO

  1 FORMAT(/T3,'Frequency-domain ',A,' for transmitter(s) with independent receiver lines.')
  2 FORMAT(/T3,'Frequency-domain ',A,' for moving rectangular loop Tx with fixed offset receiver(s).')
  3 FORMAT(/T3,'Frequency-domain ',A,' for moving dipole Tx with fixed offset receiver(s).')
  5 FORMAT(/T3,'Frequency-domain ',A,' for downhole Tx-Rx probe.')
  7 FORMAT(/T3,'Sampo ',A,' is the ratio ABS (Bz / Br).')
 10 FORMAT(/T3,'Line ',A,4X,'Survey azimuth =',I4,' degrees',4X,'Units = ',A)
 11 FORMAT(/T3,'Line ',A,4X,'Magnetic dipole Rx    Survey azimuth =',I4,' degrees',4X,'Units = ',A)
 12 FORMAT(/T3,'Line ',A,4X,'Electric dipole Rx',4X,'Units = ',A)
 13 FORMAT(/T3,'Line ',A,4X,'Point electric field',4X,'Units = ',A)
 15 FORMAT(I5,2F12.1,F9.1,540G13.4)
 16 FORMAT(I5,2F12.1,F9.1,540F13.2)
 17 FORMAT(/T3,'Line ',A,4X,'HID:',I4,4X,'SVAZ:',I4,4X,'Units: ',A,4X,'PP: ',A)
 20 FORMAT(//T3,'Inphase ',A,' for Line ',A &
            /T3,'-------------------------------')
 21 FORMAT(//T10,'X Inphase - Radial Component ',A,' for Line ',A &
            /T10,'----------------------------------------------------')
 22 FORMAT(//T10,'Y Inphase - Tangential Component ',A,' for Line ',A &
            /T10,'--------------------------------------------------------')
 23 FORMAT(//T10,'Z Inphase - Vertical Component ',A,' for Line ',A &
            /T10,'------------------------------------------------------')
 24 FORMAT(//T10,'U Inphase - Slope Component ',A,' for Line ',A &
            /T10,'---------------------------------------------------')
 25 FORMAT(//T10,'V Inphase - Horizontal Component ',A,' for Line ',A &
            /T10,'--------------------------------------------------------')
 26 FORMAT(//T10,'A Inphase - Axial Component ',A,' for Line ',A &
            /T10,'-------------------------------------------------------')
 27 FORMAT(//T10,'S Inphase - In-section Component ',A,' for Line ',A &
            /T10,'--------------------------------------------------------')
 28 FORMAT(//T10,'N Inphase - Out-section Component ',A,' for Line ',A &
            /T10,'---------------------------------------------------------')
 29 FORMAT(//T10,'W Inphase - Axial Component ',A,' for Line ',A &
            /T10,'---------------------------------------------------')
 30 FORMAT(//T3,'Quadrature ',A,' for Line ',A &
            /T3,'----------------------------------')
 31 FORMAT(/T10,'X Quadrature - Radial Component ',A,' for Line ',A &
           /T10,'-------------------------------------------------------')
 32 FORMAT(//T10,'Y Quadrature - Tangential Component ',A,' for Line ',A &
           /T10,'------------------------------------------------------------')
 33 FORMAT(//T10,'Z Quadrature - Vertical Component ',A,' for Line ',A &
            /T10,'---------------------------------------------------------')
 34 FORMAT(//T10,'U Quadrature - Slope Component ',A,' for Line ',A &
            /T10,'------------------------------------------------------')
 35 FORMAT(//T10,'V Quadrature - Horizontal Component ',A,' for Line ',A &
            /T10,'-----------------------------------------------------------')
 36 FORMAT(//T10,'A Quadrature - Axial Component ',A,' for Line ',A &
            /T10,'------------------------------------------------------')
 37 FORMAT(//T10,'S Quadrature - In-section Component ',A,' for Line ',A &
            /T10,'-----------------------------------------------------------')
 38 FORMAT(//T10,'N Quadrature - Out-section Component ',A,' for Line ',A &
            /T10,'------------------------------------------------------------')
 39 FORMAT(//T10,'W Quadrature - Axial Component ',A,' for Line ',A &
            /T10,'------------------------------------------------------')
 40 FORMAT(//T3,'Abs (Bz / Bx Ratio  ',A,' for Line ',A &
            /T3,'-------------------------------------------')
 41 FORMAT(//T10,'X Inphase - Component ',A,' for Line ',A &
            /T10,'---------------------------------------------')
 42 FORMAT(//T10,'Y Inphase - Component ',A,' for Line ',A &
            /T10,'---------------------------------------------')
 43 FORMAT(//T10,'Z Inphase - Component ',A,' for Line ',A &
            /T10,'---------------------------------------------')
 44 FORMAT(//T10,'X Quadrature - Component ',A,' for Line ',A &
            /T10,'------------------------------------------------')
 45 FORMAT(//T10,'Y Quadrature - Component ',A,' for Line ',A &
            /T10,'------------------------------------------------')
 46 FORMAT(//T10,'Z Quadrature - Component ',A,' for Line ',A &
            /T10,'------------------------------------------------')

 CONTAINS

   SUBROUTINE WRITE_FD_MISFIT (JC,KC)
!  ----------------------------------

   IMPLICIT NONE
   INTEGER JF,JC,KC,J1,J2
   REAL DENOM,VM,VD

   J1 = 1;  J2 = NFRQ
   IF (KC == 2) THEN
     J1 = NFRQ + 1
     J2 = 2 * NFRQ
   END IF
   DO JR = 1,NRX1
     DO JF = J1,J2
       YTR(JF,JR) = 0.
       VD = RDATA(JF,JR,JC,JL)
       VM = BFTL(JF,JR,JC,JL)
       DENOM = SQRT( (VM*VM + VD*VD)/2.0)
       IF (DENOM > 0 .AND. RWTS(JF,JR,JC,JL) > 0 ) YTR(JF,JR) = 100. * (VD - VM) / DENOM
     END DO
   END DO

   IF (KC == 1) WRITE(NW,1) JC
   IF (KC == 2) WRITE(NW,2) JC
   IF (KC == 3) WRITE(NW,3)
   CALL WRFDP (NW,PRFL,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)

 1 FORMAT(/T10,'Inphase Misfit for Component',I2/T10,'------------------------------')
 2 FORMAT(/T10,'Quadrature Misfit for Component',I2/T10,'---------------------------------')
 3 FORMAT(/T10,'Sampo Misfit'/T10,'------------')

   END SUBROUTINE WRITE_FD_MISFIT

 END SUBROUTINE WRITE_FD

 SUBROUTINE WRFDP (NW,PRFL,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)
!------------------------------------------------------------

!***  Called by WRSLV

!  Writes frequency-domain output in profile form.  IPPM = format control

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80)
 INTEGER NW,PRFL,IPPM,NFRQ,MRXL,NRX1,JR,JF
 REAL FREQ(NFRQ),YTR(NFRQ,NRX1)
 REAL(KIND=QL) RXPLT(3,MRXL)

 IF (PRFL == 0) THEN  ! Forget profiles, write output in spectral form.
   WRITE(NW,4)
   WRITE(NW,5) RXPLT(1,1:NRX1)
   WRITE(NW,6) RXPLT(2,1:NRX1)
   WRITE(NW,7) RXPLT(3,1:NRX1)
   WRITE(NW,'(3X)')

   DO JF = 1,NFRQ
     WRITE(NW,'(I4,G12.4,T18,150G13.4)') JF,FREQ(JF),YTR(JF,1:NRX1)
   END DO

 ELSE
   WRITE(NW,1)
   IF (IPPM == 1) THEN
     WRITE(NW,9) FREQ(1:NFRQ)
     WRITE(NW,'(3X)')
     DO JR = 1, NRX1
       WRITE(NW,8) JR,RXPLT(1:3,JR),YTR(1:NFRQ,JR)
     END DO
   ELSE
     WRITE(NW,2) FREQ(1:NFRQ)
     WRITE(NW,'(3X)')
     DO JR = 1, NRX1
       WRITE(NW,3) JR,RXPLT(1:3,JR),YTR(1:NFRQ,JR)
     END DO
   END IF
 END IF

  1 FORMAT(/T14,'RECEIVER POSITIONS',T47,'FREQUENCIES')
  2 FORMAT(T10,'East        North     Elev',F10.2,100F13.2)
  3 FORMAT(I3,2F12.1,F9.1,300G13.4)
  4 FORMAT(/T20,'RECEIVER COORDINATES (Top to Bottom): Easting, Northing, Elevation')
  5 FORMAT(/T16,150F13.2)
  6 FORMAT(T16,150F13.2)
  7 FORMAT( T6,'Frequency',T16,150F13.4)
  8 FORMAT(I3,2F12.1,F9.1,300F12.1)
  9 FORMAT(T10,'East        North     Elev ',300F12.1)

 END SUBROUTINE WRFDP

  SUBROUTINE WRITE_LOG_FILE (NLG,MSG,MXERR,ERR_LVL)
! -------------------------------------------------

! This subroutine prints out warning and fatal error messages on the LOG file.
!
! NLG = output unit index
! MSG refers to error message index
! ERR_LVL = 1 for warnings;  = 2 for fatal errors
! MXERR = MAX (ERR_LVL)

 INTEGER ERR_LVL, MSG, NLG, MXERR

 IF (MXERR == 0) OPEN (NLG,FILE = 'Loki.log',STATUS = 'REPLACE')

 MXERR = MAX (ERR_LVL,MXERR)
 IF (ERR_LVL == 1) WRITE(NLG,501)
 IF (ERR_LVL == 2) WRITE(NLG,502)

 IF (MSG == 1) WRITE(NLG,1)
 IF (MSG == 2) WRITE(NLG,2)
 IF (MSG == 3) WRITE(NLG,3)
 IF (MSG == 4) WRITE(NLG,4)
 IF (MSG == 5) WRITE(NLG,5)
 IF (MSG == 6) WRITE(NLG,6)
 IF (MSG == 7) WRITE(NLG,7)
 IF (MSG == 8) WRITE(NLG,8)
 IF (MSG == 9) WRITE(NLG,9)
 IF (MSG == 10) WRITE(NLG,10)
 IF (MSG == 11) WRITE(NLG,11)
 IF (MSG == 12) WRITE(NLG,12)
 IF (MSG == 13) WRITE(NLG,13)
 IF (MSG == 14) WRITE(NLG,14)
 IF (MSG == 15) WRITE(NLG,15)
 IF (MSG == 16) WRITE(NLG,16)
 IF (MSG == 17) WRITE(NLG,17)
 IF (MSG == 18) WRITE(NLG,18)
 IF (MSG == 19) WRITE(NLG,19)
 IF (MSG == 20) WRITE(NLG,20)
 IF (MSG == 21) WRITE(NLG,21)
 IF (MSG == 22) WRITE(NLG,22)
 IF (MSG == 23) WRITE(NLG,23)
 IF (MSG == 24) WRITE(NLG,24)
 IF (MSG == 26) WRITE(NLG,26)
 IF (MSG == 27) WRITE(NLG,27)
 IF (MSG == 28) WRITE(NLG,28)
 IF (MSG == 29) WRITE(NLG,29)
 IF (MSG == 30) WRITE(NLG,30)
 IF (MSG == 31) WRITE(NLG,31)
 IF (MSG == 32) WRITE(NLG,32)
 IF (MSG == 35) WRITE(NLG,35)
 IF (MSG == 40) WRITE(NLG,40)
 IF (MSG == 50) WRITE(NLG,50)
 IF (MSG == 51) WRITE(NLG,51)
 IF (MSG == 52) WRITE(NLG,52)
 IF (MSG == 54) WRITE(NLG,54)
 IF (MSG == 55) WRITE(NLG,55)
 IF (MSG == 56) WRITE(NLG,56)
 IF (MSG == 58) WRITE(NLG,58)
 IF (MSG == 60) WRITE(NLG,60)
 IF (MSG == 61) WRITE(NLG,61)
 IF (MSG == 62) WRITE(NLG,62)
 IF (MSG == 100) WRITE(NLG,100)
 IF (MSG == 90) THEN
   WRITE(NLG,90)
   STOP
 END IF
  1 FORMAT(/T3,'The value for TDFD is outside the permitted range.' &
           /T3,'The allowed values are: 1 or 0 for time-domain or 2 for frequency domain.')
  2 FORMAT(/T3,'The allowed values for PRFL are: 1 or 11 for profile mode, 0 or 10 otherwise.' &
           /T3,'PRFL has been set to 1.')
  3 FORMAT(/T3,'The value for DO3D is outside the permitted range of -1, 0, 1, or 2.' &
           /T3,'DO3D has been reset to 1.  A new model will be computed.')
  4 FORMAT(/T3,'The value for STEP is outside the permitted range.' &
           /T3,'The allowed values are: 0 or 1.')
  5 FORMAT(/T3,'The value for KRXW is outside the permitted range.' &
           /T3,'The allowed values are: 1 or 2.')
  6 FORMAT(/T3,'Only magnetic dipole receivers areallowed with magnnetic dipole transmitters')
  7 FORMAT(/T3,'This value for TOPN is outside the permitted range.' &
           /T3,'It must be > 0.')
  8 FORMAT(/T3,'Entry for SOURCE_TYPE is restricted to the values: 1 (loop), 2 (grounded wire) or 3 (magnetic dipole).')
  9 FORMAT(/T3,'RX_TYPE must be 1 (magnetic dipole), 2 (electric dipole), or 3 (point E-field)')
 10 FORMAT(/T3,'SURVEY_TYPE is only allowed values: 1, 2, 3, 4 5 or 6.')
 11 FORMAT(/T3,'NTX > NLINES.  A line must be specified for every transmitter position.')
 12 FORMAT(/T3,'The number of receivers for this line, NRX, has been reduced to MRXL,' &
           /T3,'the maximum set earlier in this control file.')
 13 FORMAT(/T3,'Primary field for dB/dt output is not implemented')
 14 FORMAT(/T3,'The number of transmitter positions for this line, KTXP, has been' &
           /T3,'reduced to MRXL, the maximum set earlier in this control file.')
 15 FORMAT(/T3,'UNITS must = 31 (ratio), 32 (percent), 33 (ppt), 34 (ppm) or 35 (ppb)' &
           /T3,'if normalised response (KNORM > 0) is specified.')
 16 FORMAT(/T3,'For time-domain, ISYS must = 4 for UTEM or 0 otherwise')
 17 FORMAT(/T3,'For frequency-domain, ISYS must = 0, 2 for Sampo or 6 for MT')
 18 FORMAT(/T3,'For UTEM, the number of channels must be 10 or 20.')
 19 FORMAT(/T3,'The magnetic receiver index exceeds the number of magnetic receivers')
 20 FORMAT(/T3,'The magnetotellurics option is not allowed for time-domain applications.')
 21 FORMAT(/T3,'This lithology index is invalid.' &
           /T3,'No resistivity or conductance has been specified.')
 22 FORMAT(/T3,'Layer lithology indices must be an integer between 1 & NLITH')
 23 FORMAT(/T3,'Layer resistivities must be positive.')
 24 FORMAT(/T3,'The value given to CMP is not allowed.' &
           /T3,'CMP values must be 1, 2, 3, 12, 13, 23, or 123')
 26 FORMAT(/T3,'Plate lithology indices must be an integer between 1 & NLITH')
 27 FORMAT(/T3,'Plate conductance must be positive.')
 28 FORMAT(/T3,'Each plate must be contained entirely within the basement.' &
           /T3,'One or more plates have been shifted downwards to comply with this rule.')
 29 FORMAT(/T3,'LOKI requires 0 <= DIP < 180 degrees.')
 30 FORMAT(/T3,'The point E-field receiver is not allowed for time-domain modelling')
 31 FORMAT(/T3,'The number of magnetic receivers must not exceed the number of electric receivers.')
 32 FORMAT(/T3,'The number of receivers NRX on at least one line exceeds MRXL.')
 35 FORMAT(/T3,'Problem with HEADER_ID.  Seek help !')
 40 FORMAT(/T3,'The Sampo option is not defined for time-domain applications.')
 50 FORMAT(/T3,'The value for LINE_CHK does not match LINE(J)')
 51 FORMAT(/T3,'The value for NSTAT does not match NRX(J)')
 52 FORMAT(/T3,'The value given to KMP is not allowed.')
 54 FORMAT(/T3,'The component(s) specified for inversion in CMP' &
           /T3,'are not present in the data as specified by KMP')
 55 FORMAT(/T3,'Conflict between NKMP(J) and KMP(J).' &
           /T3,'There must be at least NKMP data components to invert.')
 56 FORMAT(/T3,'Conflict between NKMP(j) and CMP(J).')
 58 FORMAT(/T3,'FD_ORDER must = 0, 1, or 2')
 60 FORMAT(/T3,'LINE_CHK must = Line number(J)')
 61 FORMAT(/T3,'NSTAT must = NRX(J)')
 62 FORMAT(/T3,'The data for each receiver must be presented in the same order as specified' &
           /T3,'in the receiver specification in Loki.inv')
 90 FORMAT(//T3,'MESSAGE FROM SCAT_MTRX_LU_DCMP: THE MATRIX IS SINGULAR.', &
            /T3,'COMPUTATION HALTED.  SEEK HELP.')
 100 FORMAT(1X)
 501 FORMAT(/T2,'INFORMATION / WARNING'/T2,'---------------------'/)
 502 FORMAT(/T2,'FATAL ERROR'/T2,'----- -----')

 END SUBROUTINE  WRITE_LOG_FILE

 SUBROUTINE SET_RX_SUBNET (NTX,MRXTX,NRXTX,MXRS,NRS,MQVR,RXID,XRXTX,YRXTX,XRS,YRS,WTRS,EDCS,EDSN)
!-----------------------------------------------------------------------------------------------

! Sets up virtual receivers for magnetic dipole, grounded wire and loop receivers.
! All receivers except magnetic dipoles must lie on the surface in this version.

!*** Called by LOKI_3D

! INPUT PARAMETERS:
! ----------------
!
!           NTX - number of transmitters
!         MRXTX - maximum number of receivers per transmitter
!         NRXTX - number of receivers for each transmitter
!           NRS - number of sub-net receivers per receiver
!          MXRS = maximum value NRS
!          MQVR = maximum number of vertices for all receivers (2 or 1)
!          RXID - reciver type: 1 = mag dipole;  2 = grounded dipole
!  XRXTX, YRXTX - north and east coordinates of receiver vertices
!
!  OUTPUT PARAMETERS:
!  -----------------
!
!      XRS, YRS - north and east coordinates of receiver subnet
!          WTRS - weights of receiver subnet
!     EDCS,EDSN - cosine & sine of angle from north made by grounded wire receiver

 IMPLICIT NONE
 INTEGER NTX,MRXTX,MXRS,NRXTX(NTX),NRS(MRXTX,NTX),MQVR,RXID(MRXTX,NTX),JS,JR
 REAL DELX,DELY,DELR,DELR12
 REAL, DIMENSION (MRXTX,NTX) :: EDCS,EDSN
 REAL, DIMENSION (MRXTX,NTX,MQVR) :: XRXTX,YRXTX
 REAL, DIMENSION (MXRS,MRXTX,NTX) :: XRS,YRS,WTRS

 DO JS = 1,NTX
   DO JR = 1, NRXTX(JS)
     IF (RXID(JR,JS) == 2) THEN   !  electric dipole receiver
       NRS(JR,JS) = 5
       XRS(1,JR,JS) = XRXTX(JR,JS,1)
       YRS(1,JR,JS) = YRXTX(JR,JS,1)
       XRS(2,JR,JS) = XRXTX(JR,JS,2)
       YRS(2,JR,JS) = YRXTX(JR,JS,2)
       XRS(3,JR,JS) = 0.5 * (XRS(1,JR,JS) + XRS(2,JR,JS))
       YRS(3,JR,JS) = 0.5 * (YRS(1,JR,JS) + YRS(2,JR,JS))
       XRS(4,JR,JS) = 0.5 * (XRS(1,JR,JS) + XRS(3,JR,JS))
       YRS(4,JR,JS) = 0.5 * (YRS(1,JR,JS) + YRS(3,JR,JS))
       XRS(5,JR,JS) = 0.5 * (XRS(2,JR,JS) + XRS(3,JR,JS))
       YRS(5,JR,JS) = 0.5 * (YRS(2,JR,JS) + YRS(3,JR,JS))

       DELX = XRXTX(JR,JS,2) - XRXTX(JR,JS,1)
       DELY = YRXTX(JR,JS,2) - YRXTX(JR,JS,1)
       DELR = SQRT (DELX**2 + DELY**2)

       EDCS(JR,JS) = DELX / DELR
       EDSN(JR,JS) = DELY / DELR
       DELR12 =  DELR / 12.
       WTRS(1:2,JR,JS) = DELR12
       WTRS(3,JR,JS) = 2.* DELR12
       WTRS(4:5,JR,JS) = 4.* DELR12

     ELSE                              !  magnetic dipole or electric point receiver
       NRS(JR,JS) = 1
       XRS(1,JR,JS) = XRXTX(JR,JS,1)
       YRS(1,JR,JS) = YRXTX(JR,JS,1)
       WTRS(1,JR,JS) = 1.
     END IF
   END DO
 END DO

 END SUBROUTINE SET_RX_SUBNET

 SUBROUTINE SET_RX_SUBNET_CL (NTX,SXN,SXE,MXRS,NRS,XRS,YRS,WTRS)
!---------------------------------------------------------------

!*** Called by LOKI_3D
!*** Calls SET_RX_SUBNET_QUAD

! Sets up virtual receivers for coincident loop sources.
! Four-sided loops are set up as a rectangular array using SET_RX_SUBNET_QUAD
! Uniform weights are used.


! INPUT PARAMETERS:
! ----------------
!
!              NTX - number of coincident loop transmitter positions
!         SXN, SXE - north & east coordinates of loop vertices
!            MXRS - maximum NRS
!
!  OUTPUT PARAMETERS:
!  -----------------
!        NRS(1,JS) - number of receivers for loop JS
!      XRS(*,1,JS) - North coordinates of the NRS magnetic dipole points
!      YRS(*,1,JS) - East coordinates of the NRS magnetic dipole points
!
!     WTRS(*,1,JS) - integration weights associated with magnetic dipole receiver net

 IMPLICIT NONE
 INTEGER, PARAMETER :: MXRS4=200
 INTEGER NTX,MXRS,NRS(1,NTX),NRS4,JS
 REAL SXN1(4),SXE1(4),WTRS1,XRS4(MXRS4),YRS4(MXRS4)
 REAL, DIMENSION (4,NTX) :: SXN,SXE
 REAL, DIMENSION (MXRS,1,NTX) :: XRS,YRS,WTRS

 DO JS = 1,NTX
   SXN1(1:4) = SXN(1:4,JS); SXE1(1:4) = SXE(1:4,JS)
   CALL SET_RX_SUBNET_QUAD (SXN1,SXE1,MXRS4,NRS4,XRS4,YRS4,WTRS1)
   NRS(1,JS) = NRS4; WTRS(1:NRS4,1,JS) = WTRS1
   XRS(1:NRS4,1,JS) = XRS4(1:NRS4); YRS(1:NRS4,1,JS) = YRS4(1:NRS4)
 END DO

 END SUBROUTINE SET_RX_SUBNET_CL

 SUBROUTINE SET_RX_SUBNET_QUAD (SXN1,SXE1,MXRS4,NRS4,XRS4,YRS4,WTRS4)
!--------------------------------------------------------------------

!  Sets up virtual receivers for four sided loops.  Initially based on 20 m intervals
!  it sets a maximum of 10 dipoles in either direction

!*** Called by SET_RX_SUBNET_CL
!*** Calls DIST2D

! INPUT PARAMETERS:
! ----------------
!
!   SXN1, SXE1 - north & east coordinates of loop vertices
!        MXRS4 - maximum number of receivers in rectangular subnet (100)
!
!  OUTPUT PARAMETERS:
!  -----------------
!
!        NRS4 - number of receivers in rectangular subnet
!        XRS4 - North coordinates of the NRS4 magnetic dipole points
!        YRS4 - East coordinates of the NRS4 magnetic dipole points
!       WTRS4 - integration weights associated with magnetic dipole receiver net

 IMPLICIT NONE
 INTEGER, PARAMETER :: MXD = 10  ! maximun number of equivalent dipoles per length
 INTEGER MXRS4,NRS4,J1,J2,ND(2),JR
 REAL SXN1(4),SXE1(4),XCNTR,YCNTR,TOTAL_AREA,AREA,A,B,C,DIST2D,S,SL(4),DELX,DELY, &
      X(MXD,2),Y(MXD,2),X0,Y0,SL_AVG,WTRS4,XRS4(MXRS4),YRS4(MXRS4)

 XCNTR = SUM (SXN1(1:4)) / 4.          ! Compute loop centre coordinates
 YCNTR = SUM (SXE1(1:4)) / 4.          ! Compute loop centre coordinates

 TOTAL_AREA = 0. ; X = 0.;  Y = 0
 DO J1 = 1, 4
   J2 = J1 + 1
   IF (J1 == 4) J2 = 1
   A = DIST2D (SXN1(J1),SXE1(J1), XCNTR, YCNTR )
   B = DIST2D (SXN1(J2),SXE1(J2), XCNTR, YCNTR)
   C = DIST2D (SXN1(J1),SXE1(J1), SXN1(J2),SXE1(J2) )
   SL(J1) = C
   S = (A + B + C) /2.
   AREA = SQRT (S* (S-A)* (S-B)* (S-C))     ! triangle area using Heron's formula
   TOTAL_AREA = TOTAL_AREA + AREA
 END DO

 DO J1 = 1,2
   SL_AVG = 0.5 * (SL(J1) + SL(J1+2))
   ND(J1) = NINT (SL_AVG / 20.)
   ND(J1) = MIN (ND(J1), MXD)
 END DO
 NRS4 = ND(1) * ND(2)
 WTRS4 = TOTAL_AREA / REAL (NRS4)

 DELX = (SXN1(2) - SXN1(1)) / REAL (ND(1))
 DELY = (SXE1(2) - SXE1(1)) / REAL (ND(1))
 X0 = SXN1(1) - 0.5*DELX
 Y0 = SXE1(1) - 0.5*DELY
 DO J1 = 1, ND(1)
   X(J1,1) = X0 + J1 * DELX
   Y(J1,1) = Y0 + J1 * DELY
 END DO

 DELX = (SXN1(3) - SXN1(4)) / REAL (ND(1))
 DELY = (SXE1(3) - SXE1(4)) / REAL (ND(1))
 X0 = SXN1(4) - 0.5*DELX
 Y0 = SXE1(4) - 0.5*DELY
 DO J1 = 1, ND(1)
   X(J1,2) = X0 + J1 * DELX
   Y(J1,2) = Y0 + J1 * DELY
 END DO

 JR = 0
 DO J1 = 1, ND(1)
   DELX = (X(J1,2) - X(J1,1)) / REAL (ND(2))
   DELY = (Y(J1,2) - Y(J1,1)) / REAL (ND(2))
   X0 = X(J1,1) - 0.5* DELX
   Y0 = Y(J1,1) - 0.5* DELY
   DO J2 = 1, ND(2)
     JR = JR + 1
     XRS4(JR) = X0 + J2 * DELX
     YRS4(JR) = Y0 + J2 * DELY
   END DO
 END DO

 END SUBROUTINE SET_RX_SUBNET_QUAD

!===================================================================

!  INVERSION SUBROUTINES

!===================================================================

 SUBROUTINE WRITE_INVMDL (NW,FINAL,ITS,NN,NE,NZ,NPAR,XPAR,IMPORT,NAIR,NEL, &
                          NER,NNL,NNR,NZB)

!----------------------------------------------------------------------------
!
!***  Called by: NLSQ
!
!  Write the inversion output, including importance for the model
!  parameters.
!
!----------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER NW,ITS,NN,NE,NZ,NPAR,I,J,K,NAIR,NEL,NER,NNL,NNR,NZB,NBN,IX,IY,IZ
 REAL, DIMENSION(NPAR) :: XPAR,IMPORT
 REAL RES,IMP
 LOGICAL FINAL

 IF (FINAL) THEN
   WRITE(NW,1) ITS  ! Write final model.
 ELSE
   WRITE(NW,2) ITS  ! Write intermediate model.
 END IF

 IF (FINAL) THEN ! Write final model and parameter importance.
   WRITE(NW,3) ! Write output header.
   DO IY = NNL+1, NN-NNR-1
     DO IZ = NAIR+1, NZ-NZB-1
       DO IX = NEL+1, NE-NER-1
         NBN  = (IY-NNL-1)*(NE-NEL-NER-1)*(NZ-NAIR-NZB-1) + (IZ-NAIR-1)*(NE-NEL-NER-1) + IX-NEL ! User domain
         I = IX - NEL
         J = IY - NNL
         K = IZ - NAIR
         RES = XPAR(NBN)
         IMP = IMPORT(NBN)
         WRITE(NW,4) I,J,K,NBN,RES,IMP
       END DO
     END DO
   END DO
 ELSE IF (.NOT. FINAL) THEN ! Only write intermediate model.
   WRITE(NW,5) ! Write output header.
   DO IY = NNL+1, NN-NNR-1
     DO IZ = NAIR+1, NZ-NZB-1
       DO IX = NEL+1, NE-NER-1
         NBN  = (IY-NNL-1)*(NE-NEL-NER-1)*(NZ-NAIR-NZB-1) + (IZ-NAIR-1)*(NE-NEL-NER-1) + IX-NEL ! User domain
         I = IX - NEL
         J = IY - NNL
         K = IZ - NAIR
         RES = XPAR(NBN)
         WRITE(NW,6) I,J,K,NBN,RES
       END DO
     END DO
   END DO
 END IF

 ! Inversion format.

 1 FORMAT(//T9,'Final Model After',I3,' Iterations' &
           /T9,'================================')
 2 FORMAT(//T9,'Model Description After',I3,' Iterations' &
           /T9,'======================================')
 3 FORMAT(//T9,'------------------------------------------------------------------------'&
           /T9,'  Element    Element    Element    Element   Resistivity    Parameter   '&
           /T9,'  Easting    Northing   Depth      Number      (Ohm-m)      Importance  '&
           /T9,'------------------------------------------------------------------------')
 4 FORMAT(T12,I4,'     ',I4,'     ',I4,'     ',I6,'       ',G12.4,'     ',G12.4)
 5 FORMAT(//T9,'----------------------------------------------------------'&
           /T9,'  Element    Element    Element    Element   Resistivity  '&
           /T9,'  Easting    Northing   Depth      Number      (Ohm-m)    '&
           /T9,'----------------------------------------------------------')
 6 FORMAT(T12,I4,'      ',I4,'      ',I4,'       ',I6,'       ',G12.4)

 END SUBROUTINE WRITE_INVMDL

 SUBROUTINE WRITE_MISFIT (NW,ITSPR,NDATA,NLINES,NRX,MCHNL,TDFD,NFRQ,FREQ, &
                          KCMP,NCHNL,RES,XMODL,XDATA,LINE)

!----------------------------------------------------------------------------
!
!***  Called by NLSQ2
!
!  Writes out the error misfit at any stage of the inversion.
!
!----------------------------------------------------------------------------
!
!     ITSPR > 0 => printout after ITSPR iterations on unit NW
!           < 0 => printout after final iterations on unit NW
!           = 0 => printout initial error structure unit NW
!
!     NSTAT - number of lines
!       NRX - number of recievers per line.
!     NDATA - total number of data points
!     MCHNL - total number of data points per station
!      TDFD = 1: time domain  or 2: frequency-domain
!      FREQ - array of NFRQ frequencies for FD inversion
!     XDATA - 1D array of NDATA measured data points
!     XMODL - 1D array of NDATA computed data points from most recent model
!       RES - symmetric error at each data point
!       CMP = 11 => inversion on horizontal in-line component only
!           = 13 => inversion on vertical component only
!           = 2  => joint inversion on vertical & horizontal in-line components
!           = 3  => inversion on all 3 components
!           = 4 or 42 => inversion on total field
!
!----------------------------------------------------------------------------

 INTEGER NW,ITSPR,NDATA,NLINES,NRX(NLINES),TDFD,NFRQ,MCHNL,NCHNL,NCHN, &
         N1,N2,JL,JR,JT,JD,JS,KCMP,MAXRX,LINE(NLINES)
 REAL FREQ(NFRQ)
 REAL, DIMENSION(NDATA) :: XMODL,XDATA,RES
 REAL, ALLOCATABLE, DIMENSION(:,:,:) :: RDATA,RMODL,RRES
 CHARACTER(LEN=8) CHN(50)
 DATA CHN(1:50) &
   /' CHNL_1 ',' CHNL_2 ',' CHNL_3 ',' CHNL_4 ',' CHNL_5 ',' CHNL_6 ',' CHNL_7 ',' CHNL_8 ',' CHNL_9 ','CHNL_10 ', &
    'CHNL_11 ','CHNL_12 ','CHNL_13 ','CHNL_14 ','CHNL_15 ','CHNL_16 ','CHNL_17 ','CHNL_18 ','CHNL_19 ','CHNL_20 ', &
    'CHNL_21 ','CHNL_22 ','CHNL_23 ','CHNL_24 ','CHNL_25 ','CHNL_26 ','CHNL_27 ','CHNL_28 ','CHNL_29 ','CHNL_30 ', &
    'CHNL_31 ','CHNL_32 ','CHNL_33 ','CHNL_34 ','CHNL_35 ','CHNL_36 ','CHNL_37 ','CHNL_38 ','CHNL_39 ','CHNL_40 ', &
    'CHNL_41 ','CHNL_42 ','CHNL_43 ','CHNL_44 ','CHNL_45 ','CHNL_46 ','CHNL_47 ','CHNL_48 ','CHNL_49 ','CHNL_50 '/

 ! Put data into matrix form.

 MAXRX = MAXVAL(NRX)
 ALLOCATE(RDATA(NLINES,MAXRX,MCHNL),RMODL(NLINES,MAXRX,MCHNL),RRES(NLINES,MAXRX,MCHNL))
 RDATA = 0. ; RMODL = 0. ; RRES = 0.

 CALL CNVRT2_3D

 RRES = 100. * RRES  ! Convert to percent.

 ! Write headers.

 IF (ITSPR == 0) THEN
   WRITE(NW,1)
 ELSE IF (ITSPR > 0) THEN
   WRITE(NW,2) ITSPR
 ELSE IF (ITSPR < 0) THEN
   WRITE(NW,3)
 END IF
 WRITE(NW,4)

 ! Write data.

 IF (TDFD == 1) THEN

   NCHN = MIN (NCHNL,50)

   ! Write Z (or A) component.

   IF (KCMP == 3 .OR. KCMP == 13 .OR. KCMP ==  31 .OR. KCMP ==  23 &
                 .OR. KCMP == 32 .OR. KCMP == 123 .OR. KCMP == 312) THEN
     WRITE(NW,5)
     N1 = 1
     N2 = NCHN
     CALL PRT_TD
   END IF

   ! Write X (or U) component.

   IF (KCMP == 1) THEN
     WRITE(NW,6)
     N1 = 1
     N2 = NCHN
     CALL PRT_TD
   ELSE IF (KCMP == 13 .OR. KCMP == 31 .OR. KCMP == 123 .OR. KCMP == 312) THEN
     WRITE(NW,6)
     N1 = NCHNL + 1
     N2 = 2*NCHNL
     CALL PRT_TD
   END IF

   ! Write Y (or V) component.

   IF (KCMP == 2) THEN
     WRITE(NW,7)
     N1 = 1
     N2 = NCHN
     CALL PRT_TD
   ELSE IF (KCMP == 23 .OR. KCMP == 32 .OR. KCMP == 123 .OR. KCMP == 312) THEN
     WRITE(NW,7)
     N1 = 2*NCHNL + 1
     N2 = 3*NCHNL
     CALL PRT_TD
   END IF

 ELSE

   ! Write Z (or A) component; in-phase then quadrature.

   IF (KCMP == 3 .OR. KCMP == 13 .OR. KCMP ==  31 .OR. KCMP ==  23 &
                 .OR. KCMP == 32 .OR. KCMP == 123 .OR. KCMP == 312) THEN
     WRITE(NW,8)
     N1 = 1
     N2 = NFRQ
     CALL PRT_FD
     WRITE(NW,11)
     N1 = NFRQ + 1
     N2 = 2*NFRQ
     CALL PRT_FD
   END IF

   ! Write X (or U) component; in-phase then quadrature.

   IF (KCMP == 1) THEN
     WRITE(NW,9)
     N1 = 1
     N2 = NFRQ
     CALL PRT_FD
     WRITE(NW,12)
     N1 = NFRQ + 1
     N2 = 2*NFRQ
     CALL PRT_FD
   ELSE IF (KCMP == 13 .OR. KCMP == 31 .OR. KCMP == 123 .OR. KCMP == 312) THEN
     WRITE(NW,9)
     N1 = NFRQ+1
     N2 = 2*NFRQ
     CALL PRT_FD
     WRITE(NW,12)
     N1 = 3*NFRQ + 1
     N2 = 4*NFRQ
     CALL PRT_FD
   END IF

   ! Write Y (or V) component; in-phase then quadrature.

   IF (KCMP == 2) THEN
     WRITE(NW,10)
     N1 = 1
     N2 = NFRQ
     CALL PRT_FD
     WRITE(NW,13)
     N1 = NFRQ + 1
     N2 = 2*NFRQ
     CALL PRT_FD
   ELSE IF (KCMP == 23 .OR. KCMP == 32 .OR. KCMP == 123 .OR. KCMP == 312) THEN
     WRITE(NW,10)
     N1 = 2*NFRQ+1
     N2 = 3*NFRQ
     CALL PRT_FD
     WRITE(NW,13)
     N1 = 5*NFRQ + 1
     N2 = 6*NFRQ
     CALL PRT_FD
   END IF

 END IF

 DEALLOCATE (RDATA,RMODL,RRES)

  1 FORMAT(//T7,'ERROR STRUCTURE OF INITIAL MODEL' &
            /T7,'---------------------------------')
  2 FORMAT(//T7,'ERROR STRUCTURE OF MODEL AFTER',I3,' ITERATIONS' &
            /T7,'--------------------------------------------')
  3 FORMAT(//T7,'ERROR STRUCTURE OF FINAL MODEL' &
            /T7,'------------------------------')
  4 FORMAT(/T3,'For each station:'                              &
           /T3,'----------------'                               &
           /T5,'The first line is the percent symmetric error.' &
           /T5,'The second line is the model response.'         &
           /T5,'The third line is the data.')
  5 FORMAT(//T7,'Z (or A) COMPONENT STRUCTURE' &
            /T7,'----------------------------')
  6 FORMAT(//T7,'X (or U) COMPONENT STRUCTURE' &
            /T7,'----------------------------')
  7 FORMAT(//T7,'Y (or V) COMPONENT STRUCTURE' &
            /T7,'----------------------------')
  8 FORMAT(//T7,'Z (or A) IN-PHASE COMPONENT STRUCTURE' &
            /T7,'-------------------------------------')
  9 FORMAT(//T7,'X (or U) IN-PHASE COMPONENT STRUCTURE' &
            /T7,'-------------------------------------')
 10 FORMAT(//T7,'Y (or V) IN-PHASE COMPONENT STRUCTURE' &
            /T7,'-------------------------------------')
 11 FORMAT(//T7,'Z (or A) QUADRATURE COMPONENT STRUCTURE' &
            /T7,'---------------------------------------')
 12 FORMAT(//T7,'X (or U) QUADRATURE COMPONENT STRUCTURE' &
            /T7,'---------------------------------------')
 13 FORMAT(//T7,'Y (or V) QUADRATURE COMPONENT STRUCTURE' &
            /T7,'---------------------------------------')

 CONTAINS

   SUBROUTINE CNVRT2_3D

     JS = 0
     DO JL = 1, NLINES
       DO JR = 1, NRX(JL)
         JS = JS + 1
         DO JT = 1, MCHNL
           JD = JT + (JS-1)*MCHNL
           RDATA(JL,JR,JT) = XDATA(JD)
           RMODL(JL,JR,JT) = XMODL(JD)
           RRES(JL,JR,JT)  = RES(JD)
         END DO
       END DO
     END DO

   END SUBROUTINE CNVRT2_3D

   SUBROUTINE PRT_TD

     WRITE(NW,'(/9X,30(A:,5X))') CHN(1:NCHN)
     DO JL = 1, NLINES
       DO JR = 1, NRX(JL)
         WRITE(NW,'(/I7,40G13.4)') LINE(JL),RRES(JL,JR,N1:N2)
         WRITE(NW,'( 7X,40G13.4)') RMODL(JL,JR,N1:N2)
         WRITE(NW,'( 7X,40G13.4)') RDATA(JL,JR,N1:N2)
       END DO
     END DO

   END SUBROUTINE PRT_TD

   SUBROUTINE PRT_FD

     WRITE(NW,'(/7X,40F11.0)') FREQ(1:NFRQ)
     DO JL = 1, NLINES
       DO JR = 1, NRX(JL)
         WRITE(NW,'(/I7,40F11.2)') LINE(JL),RRES(JL,JR,N1:N2)
         WRITE(NW,'( 7X,40F11.2)') RMODL(JL,JR,N1:N2)
         WRITE(NW,'( 7X,40F11.2)') RDATA(JL,JR,N1:N2)
       END DO
     END DO

   END SUBROUTINE PRT_FD

 END SUBROUTINE WRITE_MISFIT

!===========================================================================

!===========================================================================

!===========================================================================!
!                                                                           !
! Structure of the inversion subroutines                                    !
! --------------------------------------                                    !
!                                                                           !
! Loki can solve for either the over-determined (NDATA > NPAR) or the     !
! under-determined (NDATA < NPAR) inverse problem. This makes no difference !
! to the way the Jacobian matrix is constructed. It does make a difference  !
! however to the way the Jacobian matrix is decomposed via SVD.             !
!                                                                           !
! The general structure of the subroutines are as follows:                  !
!                                                                           !
! NLSQ               - Nonlinear least squares                              !
!       RESJAC       - Residual vector and Jacobian matrix construction     !
!       ESVD         - Singular value decomposition of the Jacobian matrix  !
!       SOLVE        - Solve for updated model parameters                   !
!       IMPORTANCE   - Singular value analysis for parameter importance     !
!       NOISE_2_SIGR - Compute the noise to signal ratio for final model    !
!                                                                           !
! For Loki inversion, RESJAC computes the residual vector using the       !
! Loki modelling algorithm, and computes the Jacobian matrix using the    !
! domain differentiation method.                                            !
!                                                                           !
!===========================================================================!

 SUBROUTINE LOKI_3D (ND,NS,NW,NFRQ,FREQ,SOURCE_TYPE,NTX,MXVRTX,NVRTX,SXE,SXN, &
                     SXZ,SXDIP,SXAZ,NRXTX,MRXTX,RXID,MQVR,MXRS,XRXTX,YRXTX, &
                     ZRXTX,NE,NN,NZ,ELOC,NLOC,ZLOC,NEDGE,NAIR,NEL,NER, &
                     NNL,NNR,NZB,KACC,LITH,LYTH,NLITH,NPROP,XPART,NPART, &
                     NPAR,JCBN,SURVEY_TYPE)

!---------------------------------------------------------------------------
!
!*** Called by: MAIN, NLSQ
!
! Computes the frequency-domain response for the 3D model.
!
!               INPUT
!               -----
!       FREQ - array of NFRQ frequencies at which the rsponse will be computed.
!SOURCE_TYPE - indicates loop (1), grounded wire (2), magnetic dipole(3) or
!              plane wave sources (4)
!        NTX - number of transmitter positions
!     MXVRTX - maximum number of vertices for any tansmitter
!   NVRTX(J) - number of vertices for tansmitter J
!     MXVRTX - maximum number of vertices for any tansmitter
!   SXE(I,J) - local east coordinate of vertex I for loop position J
!   SXN(I,J) - local coordinate of vertex I for loop position J
!     SXZ(J) - local (z positive down) depth of vertex I for loop position J
!   SXDIP(J) - dip (in radians) of dipole J (eg; vertical = 0, horizontal = 90)
!    SXAZ(J) - azimuth (in radians) of dipole J (north = 0, east = 90)
!   NRXTX(J) - number of receivers for transmitter J
!     MRXTX - maximum number of receivers per transmitter
!  RXID(I,J) - RX_TYPE of receiver I for transmitter J. 1 => MD; 2 => ED; 4=> cdnt loop.
!       MQVR - maximum number of vertices for all receivers
!            - 1 if all sources are magnetic dipoles)
!XRXTX(I,J,K)- north coordinate of the Kth vertex of the Ith receiver of transmitter J
!YRXTX(I,J,K)- east coordinate of the Kth vertex of the Ith receiver of transmitter J
!  ZRXTX(I,J)- depth of the Ith receiver of transmitter J
!         NE - number of user specified nodes from south to north
!         NN - number of user specified nodes from west to east
!         NZ - number of user specified nodes top to bottom
!       ELOC - east coordinates of user specified nodes positive east along Y axis
!       NLOC - east coordinates of user specified nodes positive north along X axis
!       ZLOC - depth coordinates of user specified nodes positive down
!              The top most node(s) have ZLOC defined as ZLOC = 0
!       LITH - lithology-index arrays of the domain
!       LYTH - lithology-list arrays of the domain
!      NLITH - number of distinct lithologial units
!      NPROP - number of lithologies (currently set to 7)
!
!                OUTPUT
!                ------
!
! BFD(JF,JR,JS,JC) - frequency-domain component JC at receiver JR
!                    corresponding to source position JS, frequency JF.
!
! If SURVEY_TYPE = 1,
!   For magnetic dipole receivers, BFD will be the frequency-domain magnetic field.
!     BFD(*,*,*,1:3) contain the NORTH (1),  EAST (2), & VERTICAL(3)
!                 components respectively.
!
!   For loop of electric dipole receivers, BFD(*,*,*,1) will be the frequency-
!                domain voltage and BFD(*,*,*,2:3) = (0,.0.)
!
!
! If SURVEY_TYPE = 2 or 3 or 4, results are stored in BFD(1, 1, 1:NTX, 1:3)
!
! If SURVEY_TYPE = 4, results are stored in BFD(1, 1, 1:NTX, 1)
!    As above, unless the receiver type is a magnetic dipole and CMP = 0,
!    all results will be stored in BFD(1, 1, 1:NTX, 1)
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER, PARAMETER :: MXLIM=33
 LOGICAL JCBN
 INTEGER NFRQ,NE,NN,NZ,NAIR,NEL,NER,NNL,NNR,NZB,NTX,JF,NELEM,SOURCE_TYPE, &
         ND,NS,IFIN,JS,MXVRTX,MQVR,MRXTX,LITH(NE,NN,NZ),NLITH,NPROP, &
         RXID(MRXTX,NTX),KACC,ITER,NPART,NPAR,SURVEY_TYPE,MXRS,NW
 INTEGER NINEDG,NEDGE(12,(NE-1)*(NN-1)*(NZ-1))
 INTEGER, DIMENSION (NTX) :: NVRTX
 INTEGER, DIMENSION (NTX) :: NRXTX
 REAL, PARAMETER :: THRESHOLD = 1.E-10
 REAL LYTH(NLITH+1,NPROP),FREQ(NFRQ),FRQ,XPART(NPART)
 REAL, DIMENSION (NTX) :: SXDIP,SXAZ,SXZ
 REAL, DIMENSION (MXVRTX,NTX) :: SXE, SXN
 REAL, DIMENSION (MRXTX,NTX) :: ZRXTX
 REAL, DIMENSION (MRXTX,NTX,MQVR) :: XRXTX,YRXTX
 REAL, DIMENSION (NE,NN,NZ) :: ELOC, NLOC, ZLOC
 COMPLEX KSQ(NPART)
 REAL, ALLOCATABLE :: SMB1(:,:), ELED(:),NLED(:),ZLED(:)
 REAL, ALLOCATABLE :: DIME(:),DIMN(:),DIMZ(:)
 COMPLEX, ALLOCATABLE :: EEDG(:),F(:),A(:)
 INTEGER, ALLOCATABLE :: IA(:,:),IMMX(:),ICED(:)
 Integer :: dval(8) ! for date_time
 Integer :: itsf, itsj, nl

 REWIND (ND)
 IF (JCBN) THEN
   ! WRITE(*,1)
   REWIND(NS)
 ELSE
   ! WRITE(*,2)
 END IF

 NINEDG = ((NE-1)*NZ + NE*(NZ-1)) * NN + NE*NZ*(NN-1)
 NELEM  = (NE-1)*(NN-1)*(NZ-1)

 OPEN(99,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=1000,STATUS='SCRATCH')

 ALLOCATE (ELED(NINEDG),NLED(NINEDG),ZLED(NINEDG),ICED(NINEDG), &
           DIME(NELEM),DIMN(NELEM),DIMZ(NELEM),SMB1(MXLIM,NINEDG), &
           IA(MXLIM,NINEDG),IMMX(NINEDG))

 SMB1 = 0. ; IA = 0 ; IMMX = 0

 CALL PREP (NE,NN,NZ,NAIR,ELOC,NLOC,ZLOC,NELEM,NINEDG,ELED,NLED,ZLED,DIME, &
            DIMN,DIMZ,ICED)

 CALL BMAT (NE,NN,NZ,ELED,NLED,ZLED,NINEDG,NEDGE,IA,MXLIM,IMMX,NELEM,SMB1, &
            DIME,DIMN,DIMZ)

 ALLOCATE (EEDG(NINEDG),F(NINEDG))

 Write (nw, 10) ! header ...
 Write (nw, 11)
 Write (*, 9)
 Write (nw, 9)
 DO JF = 1, NFRQ

   FRQ = FREQ(JF)
   ! WRITE(*,3) JF,FRQ

   CALL SET_KSQ (LYTH,NLITH,NPROP,FRQ,XPART,KSQ,NPART,NN,NZ,NE,LITH)

   CALL BMAT1 (NE,NN,NZ,NAIR,KSQ,NINEDG,NEDGE,IA,MXLIM,IMMX,NELEM, &
               DIME,DIMN,DIMZ,FRQ,JF,SMB1,NPART)

   DO JS = 1, NTX

     IFIN = (JF-1)*NTX + JS-1
     IFIN = INT (100. * (IFIN) / REAL(NTX*NFRQ))
     ! WRITE(*,4) JS,IFIN

     CALL EPEDG (ELED,NLED,ZLED,ICED,MXVRTX,NVRTX,NTX,SXE,SXN,SXZ,SXDIP,SXAZ,JS, &
                 SOURCE_TYPE,NINEDG,EEDG,FRQ)

     CALL RHS (NE,NN,NZ,NAIR,KSQ,F,NINEDG,NEDGE,EEDG,NELEM,DIME,DIMN, &
               DIMZ,NPART)

     

     CALL CG (F,NINEDG,IA,IMMX,MXLIM,KACC,ITSF,FRQ,THRESHOLD)

     CALL HFIELD (ELOC,NLOC,ZLOC,NE,NN,NZ,NAIR,KSQ,FRQ,NTX, &
                  NRXTX,MRXTX,RXID,MQVR,MXRS,XRXTX,YRXTX,ZRXTX,ND,F,NINEDG, &
                  NEDGE,EEDG,itsf,DIME,DIMN,DIMZ,NELEM,JS,SOURCE_TYPE, &
                  MXVRTX,NVRTX,SXE,SXN,SXZ,NPART,SURVEY_TYPE)

     IF (JCBN) THEN

       ALLOCATE (A(NINEDG)) ; A = (0.,0.)

       CALL RHS_DD (NE,NN,NZ,NAIR,NINEDG,NEDGE,NELEM,DIME,DIMN,DIMZ, &
                    F,EEDG,A,FRQ)

       CALL CG (A,NINEDG,IA,IMMX,MXLIM,KACC,ITSJ,FRQ,THRESHOLD/100)

       CALL DHFIELD (ELOC,NLOC,ZLOC,NE,NN,NZ,NAIR,NEL,NER,NNL,NNR,NZB,KSQ, &
                     FRQ,NTX,NRXTX,MRXTX,RXID,MQVR,MXRS,XRXTX,YRXTX,ZRXTX,NS, &
                     F,A,NINEDG,NEDGE,EEDG,DIME,DIMN,DIMZ,NELEM,JS,MXVRTX, &
                     SXE,SXN,NPART,NPAR,SURVEY_TYPE)

       DEALLOCATE (A)
       !
       ! DWA 2012.08.08: changed output so that frequencies were written to the
       ! *.out file.  These are useful for mesh design, but too much information 
       ! for onscreen dumps so we just write frq / of ??
       Call date_and_time(values = dval)
       Write (*,   8) dval(3:1:-1), dval(5:7), jf, nfrq, js, ntx, itsf, itsj
       Write (nw, 18) dval(3:1:-1), dval(5:7), frq, js, ntx, itsf, itsj
     Else
       CALL date_and_time(values = dval)
       Write (*,  7)  dval(3:1:-1), dval(5:7), jf, nfrq, js, ntx, itsf
       Write (nw, 17) dval(3:1:-1), dval(5:7), frq, js, ntx, itsf

     END IF

   END DO
 END DO

 DEALLOCATE (EEDG,F,SMB1,IA,IMMX,ELED,NLED,ZLED,ICED,DIME,DIMN,DIMZ)

 CLOSE (99)

 1 FORMAT(/T3,'Computing fields and sensitivities:',/T3,' ')
 2 FORMAT(/T3,'Computing fields:',/T3,' ')
 3 FORMAT(/T3,'Computations started for frequency',I3,'  =',G12.4/)
 4 FORMAT(T6,'Transmitter position',I3,':',I8,' percent done')
 7 Format (6x, i2.2, '/', i2.2, '/', i4.4, 2x, i2.2, ':', i2.2, ':', i2.2, &
           5x, i2, '/', i2, 4x, i3, '/', i3, 14x, i4)
17 Format (6x, i2.2, '/', i2.2, '/', i4.4, 2x, i2.2, ':', i2.2, ':', i2.2, &
           5x, en13.4, 4x, i3, '/', i3, 14x, i4)
 8 Format (6x, i2.2, '/', i2.2, '/', i4.4, 2x, i2.2, ':', i2.2, ':', i2.2, &
           5x, i2, '/', i2, 4x, i3, '/', i3, 3x, i4, ' & ', i4, ' (Jac)')
18 Format (6x, i2.2, '/', i2.2, '/', i4.4, 2x, i2.2, ':', i2.2, ':', i2.2, &
           5x, en13.4, 4x, i3, '/', i3, 3x, i4, ' & ', i4, ' (Jac)')
 9 Format (/, 12x, 'Date', 6x, 'Time', 7x, 'Frq', 9x, 'Tx', 5x, 'CG Iterations', &
           /, 12x, '----', 6x, '----', 7x, '---', 9x, '--', 5x, '-------------')
10 Format (/, 80('-'), /)
11 Format (2x, 'Solution convergence data')
 END SUBROUTINE LOKI_3D

!===========================================================================

 SUBROUTINE PREP (NE,NN,NZ,NAIR,ELOC,NLOC,ZLOC,NELEM,NINEDG,ELED,NLED,ZLED, &
                  DIME,DIMN,DIMZ,ICED)

!---------------------------------------------------------------------------
!
!*** Called by: LOKI_3D
!
! Calculates the cell dimension and the coordinates of the edges.
!
!        NE - number of target nodes in X-direction
!        NN - number of target nodes in Y-direction
!        NZ - number of target nodes in Z-direction
!      NAIR - number of target nodes in air in Z-direction
! ELOC,NLOC,-
!      ZLOC - coordinates of the finite element nodes
!     NELEM - total number of elements
!    NINEDG - total number of edges
! ELED,NLED,-
!      ZLED - coordinates of the finite element edges
! DIME,DIMN,-
!      DIMZ - dimension of the elements
!      ICED - the direction of the edges
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER NE,NN,NZ,NAIR,IX,IY,IZ,NINEDG,NBN,NELEM,ICED(NINEDG),IEDG
 REAL ELOC(NE,NN,NZ),NLOC(NE,NN,NZ),ZLOC(NE,NN,NZ),ELED(NINEDG), &
      NLED(NINEDG),ZLED(NINEDG),DIME(NELEM),DIMN(NELEM),DIMZ(NELEM)

 DO IY = 1, NN-1
   DO IZ = 1, NZ-1
     DO IX = 1, NE-1
       NBN  = (IY-1)*(NE-1)*(NZ-1) + (IZ-1)*(NE-1) + IX
       DIME(NBN) = .5 * ABS (ELOC(IX+1,IY,IZ) - ELOC(IX,IY,IZ))
       DIMN(NBN) = .5 * ABS (NLOC(IX,IY+1,IZ) - NLOC(IX,IY,IZ))
       DIMZ(NBN) = .5 * ABS (ZLOC(IX,IY,IZ+1) - ZLOC(IX,IY,IZ))
     END DO
   END DO
 END DO

 ICED = 0
 IEDG = 0
 DO IY = 1, NN
   DO IZ = 1, NZ
     DO IX = 1, NE-1
       IEDG = IEDG + 1
       ELED(IEDG) = (ELOC(IX,IY,IZ) + ELOC(IX+1,IY,IZ)) / 2.
       NLED(IEDG) = (NLOC(IX,IY,IZ) + NLOC(IX+1,IY,IZ)) / 2.
       ZLED(IEDG) = (ZLOC(IX,IY,IZ) + ZLOC(IX+1,IY,IZ)) / 2.
       IF (IZ > NAIR) ICED(IEDG) =  2
     END DO
     IF ( IZ < NZ ) THEN
       DO IX = 1, NE
         IEDG = IEDG + 1
         ELED(IEDG) = (ELOC(IX,IY,IZ) + ELOC(IX,IY,IZ+1)) / 2.
         NLED(IEDG) = (NLOC(IX,IY,IZ) + NLOC(IX,IY,IZ+1)) / 2.
         ZLED(IEDG) = (ZLOC(IX,IY,IZ) + ZLOC(IX,IY,IZ+1)) / 2.
         IF (IZ > NAIR)  ICED(IEDG) =  3
       END DO
     END IF
   END DO
   IF ( IY < NN )  THEN
     DO IZ = 1, NZ
       DO IX = 1, NE
         IEDG = IEDG + 1
         ELED(IEDG) = (ELOC(IX,IY,IZ) + ELOC(IX,IY+1,IZ)) / 2.
         NLED(IEDG) = (NLOC(IX,IY,IZ) + NLOC(IX,IY+1,IZ)) / 2.
         ZLED(IEDG) = (ZLOC(IX,IY,IZ) + ZLOC(IX,IY+1,IZ)) / 2.
         IF (IZ > NAIR)  ICED(IEDG) =  1
       END DO
     END DO
   END IF
 END DO

 END SUBROUTINE PREP

!===========================================================================

 SUBROUTINE BMAT (NE,NN,NZ,ELED,NLED,ZLED,NINEDG,NEDGE,IA,MXLIM,IMMX, &
                  NELEM,SMB1,DIME,DIMN,DIMZ)

!---------------------------------------------------------------------------
!
!*** Called by: LOKI_3D
!*** Calls: EDGELMR, EDMELMI
!
! Set up the global matrix.
!
!        NE - number of target nodes in X-direction
!        NN - number of target nodes in Y-direction
!        NZ - number of target nodes in Z-direction
! ELED,NLED,-
!      ZLED - coordinates of the finite element edges
!    NINEDG - total number of edges
!     NEDGE - array of the edges assignment
!        IA - Indexing pointer
!     MXLIM - Maximum band-with
!      IMMX - Band-with array
!     NELEM - total number of elements
!      SMB1 - part of left-hand side matrix
! DIME,DIMN,-
!      DIMZ - dimension of the cells
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 REAL, ALLOCATABLE :: SMB(:,:)
 INTEGER NE,NN,NZ,NEZ,NCOR,NCOR1,NCOR2,N,M,I,IX,IY,IZ,I5,I6,I7,IN,KIMX,KI, &
         KM,IM,NINEDG,MXLIM,ISHIFT,NBN,IMMX(NINEDG),NELEM,NNE,KI1(12),     &
         NEDGE(12,NELEM),IA(MXLIM,NINEDG),KM1(12),IT(12,12),IXLAST,IYLAST
 REAL ELR(9,12,12),EC(12),NC(12),ZC(12),ELED(NINEDG),NLED(NINEDG),      &
      ZLED(NINEDG),DIME(NELEM),DIMN(NELEM),DIMZ(NELEM),SMB1(MXLIM,NINEDG)
 DATA (IT(I,1),I=1,12)/1,7,7,1,4,4,4,4,1,7,7,1/   & ! (M,N) XX,XZ,XZ,XX,XY,XY,XY,XY,XX,XZ,XZ,XX
      (IT(I,2),I=1,12)/3,9,9,3,6,6,6,6,3,9,9,3/   &
      (IT(I,3),I=1,12)/3,9,9,3,6,6,6,6,3,9,9,3/   &
      (IT(I,4),I=1,12)/1,7,7,1,4,4,4,4,1,7,7,1/   &
      (IT(I,5),I=1,12)/2,8,8,2,5,5,5,5,2,8,8,2/   &
      (IT(I,6),I=1,12)/2,8,8,2,5,5,5,5,2,8,8,2/   &
      (IT(I,7),I=1,12)/2,8,8,2,5,5,5,5,2,8,8,2/   &
      (IT(I,8),I=1,12)/2,8,8,2,5,5,5,5,2,8,8,2/   &
      (IT(I,9),I=1,12)/1,7,7,1,4,4,4,4,1,7,7,1/   &
     (IT(I,10),I=1,12)/3,9,9,3,6,6,6,6,3,9,9,3/   &
     (IT(I,11),I=1,12)/3,9,9,3,6,6,6,6,3,9,9,3/   &
     (IT(I,12),I=1,12)/1,7,7,1,4,4,4,4,1,7,7,1/

 ! Calculating the global system-matrix.

 NNE = NE
 IXLAST = NE - 1
 IYLAST = NN - 1

 NEZ   =    NZ*(NNE-1) + NNE*(NZ-1)
 NCOR  =    NZ*(NNE-1) + NNE*(NZ-1)  +   NNE*NZ
 NCOR1 = 2*(NZ*(NNE-1) + NNE*(NZ-1)) +   NNE*NZ
 NCOR2 = 3*(NZ*(NNE-1) + NNE*(NZ-1)) + 2*NNE*NZ

 ALLOCATE (SMB(NCOR2,NCOR1))

 SMB = 0. ; ISHIFT = 0

 DO IY = 1, IYLAST
   IF (IY > 1) ISHIFT = (IY-2)*NCOR
   DO IZ = 1, NZ-1
     DO IX = 1, IXLAST
       NBN = (IY-1)*IXLAST*(NZ-1) + (IZ-1)*IXLAST + IX
       DO I = 1, 12
         EC(I)  = ELED(NEDGE(I,NBN))
         NC(I)  = NLED(NEDGE(I,NBN))
         ZC(I)  = ZLED(NEDGE(I,NBN))
       END DO
       DO I = 1, 12
         KI1(I) = NEDGE(I,NBN) - (IY-1)*NCOR
         KM1(I) = NEDGE(I,NBN) - ISHIFT
       END DO
       CALL EDGELMR (DIME(NBN),DIMN(NBN),DIMZ(NBN),ELR,EC,NC,ZC)
       DO N = 1, 12
         DO M = 1, 12
           SMB(KM1(M),KI1(N)) = SMB(KM1(M),KI1(N)) + ELR(IT(N,M),N,M)
         END DO
       END DO
     END DO
   END DO
   KIMX = NCOR
   IF (IY == IYLAST) KIMX = NCOR + NEZ
   DO KI = 1, KIMX
     IM = 0
     IN = (IY-1)*NCOR + KI
     DO KM = 1, NCOR2
       IF ( ABS(SMB(KM,KI)) > 0. ) THEN
         IM  = IM + 1
         SMB1(IM,IN) = SMB1(IM,IN) + SMB(KM,KI)
         IA  (IM,IN) = KM + ISHIFT
       END IF
     END DO
     IMMX(IN) = IM
   END DO
   DO I5 = 1, NEZ
     I7 = NCOR + I5
     DO I6 = 1, NCOR2
       SMB(I6,I5) = SMB(I6,I7)
     END DO
   END DO
   SMB(1:NCOR2,NEZ+1:NCOR1) = 0.
   IF (IY > 1)  THEN
     DO I5 = 1, NEZ
       DO I6 = 1, NCOR1
         SMB(I6,I5) = SMB(NCOR+I6,I5)
       END DO
     END DO
     SMB(NCOR1+1:NCOR2,1:NEZ) = 0.
   END IF
 END DO

 DEALLOCATE (SMB)

 END SUBROUTINE BMAT

!===========================================================================

 SUBROUTINE SET_KSQ (LYTH,NLITH,NPROP,FRQ,XPART,KSQ,NPART,NN,NZ,NE,LITH)

!---------------------------------------------------------------------------
!
!*** Called by: LOKI_3D
!
! Computes propagation constants.
!
!   LYTH - lithology arrays of the domain
!  NLITH - number of distinct lithologial units
!    FRQ - frequency
!   XPAR - vector of element resistivities
!    KSQ - vector of element propagation constants
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 REAL,PARAMETER :: EPS0 = 8.854215E-12, MU0 = 12.56637E-7, TWOPI = 6.283185
 COMPLEX,PARAMETER :: ONE=(1.,0.)
 INTEGER JL,NLITH,NPROP,NPART,NN,NZ,NE,LITH(NE,NN,NZ),NBN,IY,IZ,IX, &
         IXLAST,IYLAST
 REAL LYTH(NLITH+1,NPROP),CALF,CTAU,CFREQ,FRQ,XPART(NPART)
 COMPLEX CC,IW,Q,SIGC,KSQ(NPART)

 IW = CMPLX(0.,TWOPI*FRQ)
 KSQ = (0.,0.)

 IXLAST = NE - 1
 IYLAST = NN - 1

 DO IY = 1, IYLAST
   DO IZ = 1, NZ-1
     DO IX = 1, IXLAST
       JL = LITH(IX,IY,IZ)
       CC = ONE
       IF (LYTH(JL,5) > 1.E-4) THEN
         CALF  = 1. - LYTH(JL,5)
         CTAU  = LYTH(JL,6)
         CFREQ = LYTH(JL,7)
         Q  = (IW * CTAU)**CFREQ
         CC = (ONE + Q) / (ONE + CALF*Q)
       END IF
       NBN = (IY-1)*IXLAST*(NZ-1) + (IZ-1)*IXLAST + IX
       IF (XPART(NBN) > 0.) THEN
         SIGC = CC/XPART(NBN) + IW*EPS0*LYTH(JL,4)          ! 1./res (CC) + iw eps
         KSQ(NBN) = IW*MU0*LYTH(JL,3)*SIGC                   ! iw*mu*sigC
       END IF
     END DO
   END DO
 END DO

 END SUBROUTINE SET_KSQ

!===========================================================================

 SUBROUTINE BMAT1 (NE,NN,NZ,NAIR,KSQ,NINEDG,NEDGE,IA,MXLIM,IMMX,NELEM, &
                   DIME,DIMN,DIMZ,FRQ,JF,SMB1,NPART)

!---------------------------------------------------------------------------
!
!*** Called by: LOKI_3D
!*** Calls: EDGELMR, EDMELMI
!
! Set up the global matrix.
!
!        NE - number of target nodes in X-direction
!        NN - number of target nodes in Y-direction
!        NZ - number of target nodes in Z-direction
!      NAIR - number of target nodes in air Z-direction
!      LITH - lithology-index arrays of the domain
!       KSQ - element k^^2 array
!     NLITH - number of distinct lithologial units
!    NINEDG - total number of edges
!     NEDGE - array of the edges assignment
!        IA - Indexing pointer
!     MXLIM - Maximum band-with
!      IMMX - Band-with array
!     NELEM - total number of elements
! DIME,DIMN,-
!      DIMZ - dimension of the cells
!       FRQ - current frequency
!        JF - Loop over frequencies
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 REAL, ALLOCATABLE :: SMB(:,:)
 INTEGER NE,NN,NZ,NEZ,NCOR,NCOR1,NCOR2,N,M,ISM,I,IX,IY,IZ,I5,I6,I7,IN,KIMX, &
         KI,KMM,NINEDG,MXLIM,ISHIFT,NBN,IMMX(NINEDG),NAIR,NELEM,NNE, &
         NEDGE(12,NELEM),IA(MXLIM,NINEDG),KI1(12),KM1(12),IH(12),IXLAST, &
         IYLAST,ISMMAX,JF,NPART
 REAL ELI(9,12,12),ST,DIME(NELEM),DIMN(NELEM),DIMZ(NELEM),FRQ,SMB1(MXLIM,NINEDG)
 COMPLEX KSQ(NPART),FACT,STC,SMB5(MXLIM)
 DATA (IH(I),I=1,12)/1,9,9,1,5,5,5,5,1,9,9,1/

 ! Calculating the global system-matrix.

 NNE = NE
 IXLAST = NE - 1
 IYLAST = NN - 1

 NEZ   =    NZ*(NNE-1) + NNE*(NZ-1)
 NCOR  =    NZ*(NNE-1) + NNE*(NZ-1)  +   NNE*NZ
 NCOR1 = 2*(NZ*(NNE-1) + NNE*(NZ-1)) +   NNE*NZ
 NCOR2 = 3*(NZ*(NNE-1) + NNE*(NZ-1)) + 2*NNE*NZ

 ISMMAX = 1
 DO I = 1, NPART
   IF(ABS(REAL(KSQ(I))) > 1.E-6) ISMMAX = 2
 END DO
 IF (ISMMAX == 1 .AND. JF > 1) RETURN

 ALLOCATE (SMB(NCOR2,NCOR1))

 DO ISM = 1, ISMMAX
   IF (ISM == 1) FACT = (0.,1.)
   IF (ISM == 2) FACT = (1.,0.)
   SMB    = 0.
   SMB5   = (0.,0.)
   ISHIFT = 0
   DO IY = 1, IYLAST
     IF (IY > 1) ISHIFT = (IY-2)*NCOR
     DO IZ = 1, NZ-1
       IF (IZ >= NAIR) THEN
         DO IX = 1, IXLAST
           NBN = (IY-1)*IXLAST*(NZ-1) + (IZ-1)*IXLAST + IX
           STC = KSQ(NBN)  / FRQ
           IF (ISM == 1) ST  = AIMAG(STC)
           IF (ISM == 2) ST  = REAL (STC)
           DO I = 1, 12
             KI1(I) = NEDGE(I,NBN) - (IY-1)*NCOR
             KM1(I) = NEDGE(I,NBN) - ISHIFT
           END DO
           CALL EDGELMI (DIME(NBN),DIMN(NBN),DIMZ(NBN),ELI)
           DO N = 1, 12
             DO M = 1, 12
               SMB(KM1(M),KI1(N)) = SMB(KM1(M),KI1(N)) + ST*ELI(IH(N),N,M)
             END DO
           END DO
         END DO
       END IF
     END DO
     KIMX = NCOR
     IF (IY == IYLAST)  KIMX = NCOR + NEZ
     DO KI = 1, KIMX
       IN = (IY-1)*NCOR + KI
       IF (ISM == 1) THEN
         DO I = 1, IMMX(IN)
           KMM = IA(I,IN) - ISHIFT
           SMB5(I) = CMPLX(SMB1(I,IN),0.) + FACT*SMB(KMM,KI)
         END DO
         WRITE(99,REC=IN) (SMB5(I),I = 1,IMMX(IN))
       ELSE
         READ(99,REC=IN) (SMB5(I),I = 1,IMMX(IN))
         DO I = 1, IMMX(IN)
           KMM = IA(I,IN) - ISHIFT
           SMB5(I) = SMB5(I) + FACT*SMB(KMM,KI)
         END DO
         WRITE(99,REC=IN) (SMB5(I),I = 1,IMMX(IN))
       END IF
     END DO
     DO I5 = 1, NEZ
       I7 = NCOR + I5
       DO I6 = 1, NCOR2
         SMB(I6,I5) = SMB(I6,I7)
       END DO
     END DO
     SMB(1:NCOR2,NEZ+1:NCOR1) = 0.
     IF (IY > 1)  THEN
       DO I5 = 1, NEZ
         DO I6 = 1, NCOR1
           SMB(I6,I5) = SMB(NCOR+I6,I5)
         END DO
       END DO
       SMB(NCOR1+1:NCOR2,1:NEZ) = 0.
     END IF
   END DO
 END DO

 DEALLOCATE (SMB)

 END SUBROUTINE BMAT1

!===========================================================================

 SUBROUTINE EPEDG (ELED,NLED,ZLED,ICED,MXVRTX,NVRTX,NTX,SXE,SXN,SXZ,SXDIP, &
                   SXAZ,JS,SOURCE_TYPE,NINEDG,EEDG,FRQ)

!---------------------------------------------------------------------------
!
!*** Called by: LOKI_3D
!*** Calls:
!
! Compute the primary electric fields.
!
!        ELED - East - coordinates of the finite element edges
!        NLED - Nort - coordinates of the finite element edges
!        ZLED - Z    - coordinates of the finite element edges
!        ICED - the direction of the edges
!      MXVRTX - maximum number of vertices for any tansmitter
!    NVRTX(J) - number of vertices for tansmitter J
!         NTX - number of transmitter positions
!    SXE(I,J) - east coordinate of vertex I for loop position J
!    SXN(I,J) - north coordinate of vertex I for loop position J
!      SXZ(J) - relative level of vertex I for loop position J
!    SXDIP(J) - dip (in degrees) of dipole J
!                (eg; vertical = 0, horizontal = 90)
!     SXAZ(J) - azimuth (in degrees) of dipole J
!                (north = 0, east = 90)
!          JS - loop over transmitters
! SOURCE_TYPE - type of source, e.g. loop, dipole etc.
!      NINEDG - total number of edges
!        EEDG - array of the primary electric field at the edges
!         FRQ - frequency
!
!---------------------------------------------------------------------------

 USE KG_Filter_coefficients
 IMPLICIT NONE
 REAL, PARAMETER :: MU0=12.56637E-7,TWOPI=6.283185,FOURPI=12.56637
 COMPLEX, PARAMETER :: ONE=(1.,0.)
 INTEGER NINEDG,I,J,ICED(NINEDG),JS,NTX,NVRTX(NTX),IEDG,MXVRTX, &
         SOURCE_TYPE,NV,L
 REAL SXDIP(NTX),SXAZ(NTX),ELED(NINEDG),NLED(NINEDG),ZLED(NINEDG),SFAC, &
      DELT,CFAC,XR,YR,ZR,R1,R2,FRQ,SIGB,EP,NP,ZP,CS(MXVRTX),SN(MXVRTX), &
      SFC(MXVRTX),CFC(MXVRTX),SXL(MXVRTX),DX,DY,DZ,RR,FACT,Q1,Q2,Q3,Q4, &
      Q5,Q6,X1,Y1,Z1,X2,Y2,Z2,XA1,XA2,YY1,YY2,F1,F2,F3
 REAL(KIND=QL) VAL0A,VAL2A,VAL0B,VAL2B,ARG1,ARG2
 COMPLEX EEDG(NINEDG),KK,IWMU
 REAL, DIMENSION (MXVRTX,NTX) :: SXE,SXN
 REAL, DIMENSION (NTX) :: SXZ

 DELT = LOG (10.) / 15
 IWMU = (0.,1.)*TWOPI*FRQ*MU0
 SIGB = 1.E-6
 KK   = SQRT(-IWMU*SIGB)

 IF (SOURCE_TYPE == 1 .OR. SOURCE_TYPE == 2 .OR. SOURCE_TYPE == 4) THEN
   NV = NVRTX(JS)
   DO I = 1, NV - 1
     Q1 = SXN(I,JS) ; Q4 = SXN(I+1,JS)
     Q2 = SXE(I,JS) ; Q5 = SXE(I+1,JS)
     Q3 = SXZ(JS)   ; Q6 = SXZ(JS)
     SXL(I) = DIST1(Q1,Q2,Q3,Q4,Q5,Q6)
     RR     = DIST2(Q1,Q2,Q4,Q5)
     SN(I) = (SXE(I+1,JS) - SXE(I,JS)) / RR
     CS(I) = (SXN(I+1,JS) - SXN(I,JS)) / RR
     SFC(I) = RR / SXL(I)
     CFC(I) = (SXZ(JS) - SXZ(JS)) / SXL(I)
   END DO
   IF (SOURCE_TYPE == 1 .OR. SOURCE_TYPE == 4) THEN
     Q1 = SXN(NV,JS) ; Q4 = SXN(1,JS)
     Q2 = SXE(NV,JS) ; Q5 = SXE(1,JS)
     Q3 = SXZ(JS)    ; Q6 = SXZ(JS)
     SXL(NV) = DIST1(Q1,Q2,Q3,Q4,Q5,Q6)
     RR     = DIST2(Q1,Q2,Q4,Q5)
     SN(NV) = (SXE(1,JS) - SXE(NV,JS)) / RR
     CS(NV) = (SXN(1,JS) - SXN(NV,JS)) / RR
     SFC(NV) = RR / SXL(NV)
     CFC(NV) = (SXZ(JS)  - SXZ(JS)) / SXL(NV)
   ELSE  IF (SOURCE_TYPE == 2) THEN
     Q1 = SXN(NV-1,JS) ; Q4 = SXN(NV,JS)
     Q2 = SXE(NV-1,JS) ; Q5 = SXE(NV,JS)
     Q3 = SXZ(JS)      ; Q6 = SXZ(JS)
     SXL(NV) = DIST1(Q1,Q2,Q3,Q4,Q5,Q6)
     RR     = DIST2(Q1,Q2,Q4,Q5)
     SN(NV) = (SXE(NV-1,JS) - SXE(NV,JS)) / RR
     CS(NV) = (SXN(NV-1,JS) - SXN(NV,JS)) / RR
     SFC(NV) = RR / SXL(NV)
     CFC(NV) = (SXZ(JS)     - SXZ(JS)) / SXL(NV)
   END IF
 END IF

 EEDG  = (0.,0.)
 DO IEDG = 1, NINEDG
   IF ( ABS(ICED(IEDG)) > 0 )   THEN
     NP = NLED(IEDG) ; EP = ELED(IEDG) ; ZP = ZLED(IEDG)
     IF (SOURCE_TYPE == 3) THEN
       CFAC = COS(SXDIP(JS))
       SFAC = SIN(SXDIP(JS))
       DZ = CFAC
       DX = SFAC * COS(SXAZ(JS))
       DY = SFAC * SIN(SXAZ(JS))
       Q1 = SXN(1,JS)
       Q2 = SXE(1,JS)
       Q3 = SXZ(JS)
       R1 = DIST1 (NP, EP, ZP, Q1, Q2, Q3)
       XR = (NP - SXN(1,JS)) / R1
       YR = (EP - SXE(1,JS)) / R1
       ZR = (ZP - SXZ(JS)) / R1
       IF (ABS(ICED(IEDG)) == 1) EEDG(IEDG) = EXP(KK*R1)*(ONE+KK*R1) * (YR*DZ-ZR*DY)/(R1*R1)
       IF (ABS(ICED(IEDG)) == 2) EEDG(IEDG) = EXP(KK*R1)*(ONE+KK*R1) * (ZR*DX-XR*DZ)/(R1*R1)
       IF (ABS(ICED(IEDG)) == 3) EEDG(IEDG) = EXP(KK*R1)*(ONE+KK*R1) * (XR*DY-YR*DX)/(R1*R1)
     ELSE IF (SOURCE_TYPE == 1 .OR. SOURCE_TYPE == 4) THEN
       DO J = 1, NV
         FACT = 0.
         IF (ABS(ICED(IEDG)) == 1) FACT = SFC(J)*CS(J)
         IF (ABS(ICED(IEDG)) == 2) FACT = SFC(J)*SN(J)
         IF (ABS(ICED(IEDG)) == 3) FACT = CFC(J)
         DO I = 0, IFIX(SXL(J))
           Q1 = SXN(J,JS) + I*CS(J)
           Q2 = SXE(J,JS) + I*SN(J)
           Q3 = SXZ(JS) + I*CFC(J)
           R1 = DIST1 (NP, EP, ZP, Q1, Q2, Q3)
           EEDG(IEDG) = EEDG(IEDG) - FACT*EXP(KK*R1)/R1
         END DO
       END DO
     ELSE IF (SOURCE_TYPE == 2) THEN
       KK  = SQRT(IWMU*1.E-6)
       DO J = 1, NV-1
         DO I = 1, IFIX(SXL(J))
           Q1 = SXN(J,JS) + I*CS(J)
           Q2 = SXE(J,JS) + I*SN(J)
           Q3 = SXZ(JS)   + I*CFC(J)
           R1 = DIST1 (NP, EP, ZP, Q1, Q2, Q3)
           IF (ABS(ICED(IEDG)) == 1) EEDG(IEDG) = EEDG(IEDG) - SFC(J)*CS(J)*EXP(-KK*R1)/R1
           IF (ABS(ICED(IEDG)) == 2) EEDG(IEDG) = EEDG(IEDG) - SFC(J)*SN(J)*EXP(-KK*R1)/R1
           IF (ABS(ICED(IEDG)) == 3) EEDG(IEDG) = EEDG(IEDG) - CFC(J)      *EXP(-KK*R1)/R1
         END DO
         Q1 = SXN(J,JS)   ; Q2 = SXE(J,JS)   ; Q3 = SXZ(JS) ; R1 = DIST1(NP,EP,ZP,Q1,Q2,Q3)
         Q4 = SXN(J+1,JS) ; Q5 = SXE(J+1,JS) ; Q6 = SXZ(JS) ; R2 = DIST1(NP,EP,ZP,Q4,Q5,Q6)
         X1 = (NP-SXN(J,JS)) ; X2 = (NP-SXN(J+1,JS))
         Y1 = (EP-SXE(J,JS)) ; Y2 = (EP-SXE(J+1,JS))
         Z1 = (ZP-SXZ(JS)) ; Z2 = (ZP-SXZ(JS))
         VAL0A = 0.D0 ; VAL2A = 0.D0 ; VAL0B = 0.D0 ; VAL2B = 0.D0
         YY1 = -LOG (R1) - 0.14314998
         YY2 = -LOG (R2) - 0.14314998
         DO L = -250, 150
           XA1  = EXP(YY1+L*DELT) ; XA2 = EXP(YY2+L*DELT)
           ARG1 = Z1*EXP(-XA1*Z1) ; ARG2 = Z2*EXP(-XA2*Z2)
           VAL0A = VAL0A + WJ1(L)*ARG1 ; VAL2A = VAL2A + WJ0(L)*ARG1
           VAL0B = VAL0B + WJ1(L)*ARG2 ; VAL2B = VAL2B + WJ0(L)*ARG2
         END DO
         F1 = (X1/R1*REAL(VAL0A)/R1 - X2/R2*REAL(VAL0B)/R2)
         F2 = (Y1/R1*REAL(VAL0A)/R1 - Y2/R2*REAL(VAL0B)/R2)
         F3 = (Z1/R1*REAL(VAL2A)/R1 - Z2/R1*REAL(VAL2B)/R2)
         IF (ABS(ICED(IEDG)) == 1) EEDG(IEDG) = EEDG(IEDG) + CMPLX(F1,0.)
         IF (ABS(ICED(IEDG)) == 2) EEDG(IEDG) = EEDG(IEDG) + CMPLX(F2,0.)
         IF (ABS(ICED(IEDG)) == 3) EEDG(IEDG) = EEDG(IEDG) + CMPLX(F3,0.)
       END DO
     END IF
     EEDG(IEDG) = EEDG(IEDG) / FOURPI
   END IF
 END DO

 CONTAINS

   REAL FUNCTION DIST1(X1,Y1,Z1,X2,Y2,Z2)

   !-------------------------------------

   REAL X1,Y1,Z1,X2,Y2,Z2

   DIST1 = SQRT ((X1-X2)**2 + (Y1-Y2)**2 + (Z1-Z2)**2)
   DIST1 = MAX(DIST1, 1.)

   END FUNCTION DIST1

   REAL FUNCTION DIST2(X1,Y1,X2,Y2)

   !-------------------------------

   REAL X1,Y1,X2,Y2

   DIST2 = SQRT ((X1-X2)**2 + (Y1-Y2)**2)
   DIST2 = MAX(DIST2, 1.)

   END FUNCTION DIST2

 END SUBROUTINE EPEDG

!===========================================================================

 SUBROUTINE RHS (NE,NN,NZ,NAIR,KSQ,F,NINEDG,NEDGE,EEDG,NELEM,DIME,DIMN, &
                 DIMZ,NPART)

!---------------------------------------------------------------------------
!
!*** Called by: LOKI_3D
!*** Calls: EDGELMR, EDMELMI
!
! SET UP THE RHS OF SYSTEM MATRIX.
!
!        NE - number of target nodes in X-direction
!        NN - number of target nodes in Y-direction
!        NZ - number of target nodes in Z-direction
!      NAIR - number of target nodes in air Z-direction
!      LITH - lithology-index arrays of the domain
!       KSQ - element k^^2 array
!     NLITH - number of distinct lithologial units
!    NINEDG - total number of edges
!     NEDGE - array of the edges assignment
!     NELEM - total number of elements
! DIME,DIMN,-
!      DIMZ - dimension of the cells
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER NE,NN,NZ,N,M,I,IX,IY,IZ,NINEDG,NBN,NELEM,NEDGE(12,NELEM),IH(12), &
         NED,NAIR,IXLAST,IYLAST,NPART
 REAL ELI(9,12,12),DIME(NELEM),DIMN(NELEM),DIMZ(NELEM)
 COMPLEX EEDG(NINEDG),KSQ(NPART),F(NINEDG),STC
 DATA (IH(I),I=1,12)/1,9,9,1,5,5,5,5,1,9,9,1/

 IXLAST = NE - 1
 IYLAST = NN - 1

 F = (0.,0.)
 DO IY = 1, IYLAST
   DO IZ = 1, NZ-1
     DO IX = 1, IXLAST
       NBN = (IY-1)*IXLAST*(NZ-1) + (IZ-1)*IXLAST + IX
       STC = KSQ(NBN)
       IF (IZ >= NAIR) THEN
         CALL EDGELMI (DIME(NBN),DIMN(NBN),DIMZ(NBN),ELI)
         DO N = 1, 12
           NED = NEDGE(N,NBN)
           DO M = 1, 12
             F(NED) = F(NED) - STC*ELI(IH(N),N,M)*EEDG(NEDGE(M,NBN))
           END DO
         END DO
       END IF
     END DO
   END DO
 END DO

 END SUBROUTINE RHS

!===========================================================================

 SUBROUTINE CG (B,N,IA,IMMX,MXLIM,KACC,ITER,FRQ,THRESHOLD)

!---------------------------------------------------------------------------
!
!*** Called by: LOKI_3D
!
!  Solves the matrix equation using conjugate gradient method.
!
!      B - right-hand side matrix, which will be overwritten by the results.
!      N - number of the unknowns
!     IA - Indexing pointer
!   IMMX - Band-with array
!  MXLIM - Maximum band-with
!   KACC = 1 => TOL = 1.E-6
!        = 2 => TOL = 1.E-7
!   ITER - maximum number of itreration
!    FRQ - frequency
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80)
 INTEGER N,ITMAX,MXLIM,I,J,KACC,ITER
 REAL (KIND=QL) TOL,BNORM,RES,BNORM1,RESMIN
 REAL FCT,RFACT,FRQ,VALUE,SMB5RE,SMB5IM,VALUE1,RFACT1,THRESHOLD
 COMPLEX SMB5(MXLIM),B(N)
 COMPLEX (KIND=QL) :: KSI,ALPHA,BETA,TMP,RHO,DELTA,RHO0
 COMPLEX (KIND=QL), ALLOCATABLE :: R(:),U(:),P(:),X(:),X1(:)
 COMPLEX, ALLOCATABLE :: A(:,:),ADG(:)
 INTEGER IMMX(N),IA(MXLIM,N)

 ALLOCATE (A(MXLIM,N),X(N),R(N),P(N),U(N),ADG(N),X1(N))

 IF (KACC == 1) THEN
   TOL = 1.D-6
   ITMAX = 2000
 ELSE
   TOL = 1.D-7
   ITMAX = 5000
 END IF

 DO I = 1, N
   READ(99,REC=I) (SMB5(J),J=1,IMMX(I))
   DO J = 1, IMMX(I)
     SMB5RE = REAL(SMB5(J))
     SMB5IM = AIMAG(SMB5(J))
     A(J,I) = CMPLX(SMB5RE,FRQ*SMB5IM)
   END DO
 END DO

 ! Define the pre-conditioner.

 ADG = (0.,0.)
 DO I = 1, N
   DO J = 1, IMMX(I)
     IF ((IA(J,I) == I))  ADG(I) = 1. / A(J,I)
   END DO
 END DO

 ! Scaling.

 RFACT  = 1.E20
 RFACT1 = 1.E20
 DO I = 1, N
   VALUE  = ABS(B(I))
   VALUE1 = ABS(ADG(I))
   IF(VALUE  > THRESHOLD .AND. VALUE  < RFACT)  RFACT  = VALUE
   IF(VALUE1 > THRESHOLD .AND. VALUE1 < RFACT1) RFACT1 = VALUE1
 END DO
 FCT = RFACT1*1.E5/(RFACT*FRQ)

 DO I = 1, N
   DO J = 1, IMMX(I)
     A(J,I) = FCT*A(J,I)
   END DO
   B(I) = FCT*B(I)
 END DO

 ADG = (0.,0.)
 DO I = 1, N
   DO J = 1, IMMX(I)
     IF ((IA(J,I) == I))  ADG(I) = 1. / A(J,I)
   END DO
   X(I) = ADG(I)
 END DO
 ADG = SQRT(ADG)

 DO I = 1, N
   DO J = 1, IMMX(I)
     A(J,I) = ADG(I)*A(J,I)*ADG(IA(J,I))
   END DO
 END DO

 ! Calculate the residual.

 BNORM = 0.D0
 DO I = 1, N
   BNORM = BNORM + ABS(B(I))
   R(I)  = ADG(I)*B(I)
 END DO
 BNORM1 = TOL*BNORM

 DO I = 1, N
   TMP  = (0.D0,0.D0)
   DO J = 1, IMMX(I)
     TMP = TMP + A(J,I) * X(IA(J,I))
   END DO
   R(I) = R(I) - TMP
 END DO
 P = R

 X1  = X
 U   = (0.D0,0.D0)
 RHO = (0.D0,0.D0)
 KSI = (0.D0,0.D0)
 DO I = 1, N
   DO J = 1, IMMX(I)
     U(I) = U(I) + A(J,I) * P(IA(J,I))
   END DO
   RHO = RHO + R(I)*R(I)
   KSI = KSI + R(I)*U(I)
 END DO

 BETA = (0.D0,0.D0)                 !   Iterate
 RESMIN = 1.D10
 DO ITER = 1, ITMAX
   ALPHA = RHO/KSI
   DO I = 1, N
     P(I) = R(I) +  BETA*P(I)
     X(I) = X(I) + ALPHA*P(I)
     R(I) = R(I) - ALPHA*U(I)
   END DO

   RHO0 = RHO
   RHO  = (0.D0,0.D0)
   DO I = 1, N
      RHO = RHO + R(I)*R(I)
   END DO
   BETA = RHO / RHO0

   IF (ITER > 40) THEN             !   Test convergence
      RES  = 0.D0
      DO I = 1, N
         RES = RES + ABS(R(I))
      END DO
      IF (RES < RESMIN) THEN
         RESMIN = RES
         X1 = X
      END IF
      IF (RES < BNORM1)  EXIT
   END IF

   DELTA  = (0.D0,0.D0)
   DO I = 1, N
      TMP = (0.D0,0.D0)
      DO J = 1, IMMX(I)
         TMP = TMP + A(J,I) * R(IA(J,I))
      END DO
      DELTA = DELTA + R(I)*TMP
      U(I)  = TMP  + BETA*U(I)
   END DO
   KSI = DELTA - BETA*BETA * KSI

 END DO

!
!   This just clutters the screen ...
! WRITE(*,*) 'CG iter   = ',ITER
! WRITE(*,*) 'Residual  = ',RES
! WRITE(*,*) 'Threshold = ',BNORM1

 DO I = 1, N
   B(I) = CMPLX(X1(I))*ADG(I)
 END DO

 DEALLOCATE (A,R,P,U,ADG,X,X1)

 END SUBROUTINE CG

!===========================================================================

 SUBROUTINE HFIELD (ELOC,NLOC,ZLOC,NE,NN,NZ,NAIR,KSQ,FRQ,NTX,NRXTX,MRXTX, &
                    RXID,MQVR,MXRS,XRXTX,YRXTX,ZRXTX,ND,F,NINEDG,NEDGE,EEDG, &
                    ITER,DIME,DIMN,DIMZ,NELEM,JS,SOURCE_TYPE,MXVRTX,NVRTX, &
                    SXE,SXN,SXZ,NPART,SURVEY_TYPE)

!---------------------------------------------------------------------------
!
!*** Called by: LOKI_3D
!*** Calls:
!
! Compute the secondary electric and magnetic fields.
!
!        ELOC - X - coordinates of the finite element nodes
!        NLOC - Y - coordinates of the finite element nodes
!        ZLOC - Z - coordinates of the finite element nodes
!        NRXG - number of receiver groups
!             - 1 if SURVEY_TYPE > 1
!    NRGTX(J) - number of receiver groups for transmitter J
!             - 1 if SURVEY_TYPE > 1
!  RX_TYPE(I) - for Rx group I: dipole (1), loop (2), grounded electrode (3)
! RGTXID(I,J) - receiver group index for Ith group for transmitter J
!      NRX(J) - the number of receivers in receiver group J.  MRX = MAXVAL (NRX)
!             - 1 if SURVEY_TYPE > 1
!         MRX - maximum number of receivers in any receiver group
!        MQVR - maximum number of vertices for all receivers
!             - 1 if all sources are magnetic dipoles)
! XRXTX(I,J,K)- north coordinate of the Kth vertex of the Ith receiver of transmitter J
! YRXTX(I,J,K)- east coordinate of the Kth vertex of the Ith receiver of transmitter J
!   ZRXTX(I,J)- depth of the Ith receiver of transmitter J
!             - K = 1 only for mag dipole Rx; 1 to 2 for electric dipole Rx; 1 to 4 for loop Rx
!          NE - number of target nodes in X-direction
!          NN - number of target nodes in Y-direction
!          NZ - number of target nodes in Z-direction
!        NAIR - number of target nodes in air in Z-direction
!        LITH - lithology-index arrays of the domain
!         KSQ - element k^^2-array
!       NLITH - number of distinct lithologial units
!         FRQ - frequency
!         NTX - total number of transmitters
!    RX,RY,RZ - receiver coord.
!          ND - save-file unit number
!           F - array of the secondary electric field at the target
!        NFRQ - number of frequencies
!   DIME,DIMN,-
!        DIMZ - dimension of the cells
! SURVEY_TYPE - Types of survey
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 REAL, PARAMETER :: PI=3.141592654
 INTEGER NE,NN,NZ,NELEM,I,J,K,JR,JS,NTX,ND,NINEDG,NAIR,NEDGE(12,(NE-1)*(NN-1)*(NZ-1)), &
         ITER,MRXTX,MQVR,RXID(MRXTX,NTX),NVRTX(NTX),MXVRTX,NV,SOURCE_TYPE, &
         NVLOOP,JJ,NPART,NINTEG,NINTEG_MAX,SURVEY_TYPE,MXRS,NRS(MRXTX,NTX)
 COMPLEX EEDG(NINEDG),F(NINEDG),HX,HY,HZ,KSQ(NPART),HNX,HNY,HNZ,H_TOT, &
         HDUM1,HDUM2
 REAL ELOC(NE,NN,NZ),NLOC(NE,NN,NZ),ZLOC(NE,NN,NZ),X,Y,Z,FRQ,DIME(NELEM), &
      DIMN(NELEM),DIMZ(NELEM),RHO,XFC,YFC,ZFC,RXX,RYY,RZZ,CS(MXVRTX), &
      SN(MXVRTX),SXL1(MXVRTX),SFC(MXVRTX),CFC(MXVRTX),SXL2(MXVRTX), &
      INTEG_WEIGHT(3),R1,R2,FCT
 INTEGER,DIMENSION (NTX) :: NRXTX
 REAL, DIMENSION(MRXTX,NTX,MQVR) :: XRXTX,YRXTX
 REAL,DIMENSION(MXRS,MRXTX,NTX) :: XRS,YRS,WTRS
 REAL,DIMENSION(MRXTX,NTX) :: EDCS,EDSN,ZRXTX
 REAL, DIMENSION (MXVRTX,NTX) :: SXE,SXN
 REAL, DIMENSION (NTX) :: SXZ
 REAL, ALLOCATABLE :: VERTEX(:,:),INTEG_POINTS(:,:)

 XRS = 0 ; YRS = 0 ; WTRS = 1. ; EDCS = 0. ; EDSN = 0.
 IF (SOURCE_TYPE == 4) THEN
    CALL SET_RX_SUBNET_CL (NTX,SXN,SXE,MXRS,NRS,XRS,YRS,WTRS)
 ELSE
   CALL SET_RX_SUBNET (NTX,MRXTX,NRXTX,MXRS,NRS,MQVR,RXID,XRXTX,YRXTX,XRS,YRS,WTRS,EDCS,EDSN)
 END IF

 IF (SOURCE_TYPE == 1 .OR. SOURCE_TYPE == 2 .OR. SOURCE_TYPE == 4) THEN
   NV = NVRTX(JS)
   DO I = 1, NV - 1
     SXL1(I) = SQRT((SXE(I,JS)-SXE(I+1,JS))**2+(SXN(I,JS)-SXN(I+1,JS))**2+(SXZ(JS)-SXZ(JS))**2)
     SXL2(I) = SQRT((SXE(I,JS)-SXE(I+1,JS))**2+(SXN(I,JS)-SXN(I+1,JS))**2)
     SXL1(I) = MAX(SXL1(I),1.) ; SXL2(I) =  MAX(SXL2(I),1.)
     SN(I)  = (SXE(I+1,JS) - SXE(I,JS)) / SXL2(I)
     CS(I)  = (SXN(I+1,JS) - SXN(I,JS)) / SXL2(I)
     SFC(I) =  SXL2(I) / SXL1(I)
     CFC(I) = (SXZ(JS) - SXZ(JS)) / SXL1(I)
   END DO
   IF (SOURCE_TYPE == 1 .OR. SOURCE_TYPE == 4) THEN
     SXL1(NV) = SQRT((SXE(NV,JS)-SXE(1,JS))**2+(SXN(NV,JS)-SXN(1,JS))**2+(SXZ(JS)-SXZ(JS))**2)
     SXL2(NV) = SQRT((SXE(NV,JS)-SXE(1,JS))**2+(SXN(NV,JS)-SXN(1,JS))**2)
     SXL1(NV) = MAX(SXL1(NV),1.) ; SXL2(NV) =  MAX(SXL2(NV),1.)
     SN(NV)  = (SXE(1,JS) - SXE(NV,JS)) / SXL2(NV)
     CS(NV)  = (SXN(1,JS) - SXN(NV,JS)) / SXL2(NV)
     SFC(NV) =  SXL2(NV) / SXL1(NV)
     CFC(NV) = (SXZ(JS) - SXZ(JS)) / SXL1(NV)
   END IF
 END IF

 IF (SOURCE_TYPE == 1) NVLOOP = NV
 IF (SOURCE_TYPE == 2) NVLOOP = NV - 1
 IF (SOURCE_TYPE == 4) NVLOOP = NV

 DO JR = 1, NRXTX(JS)                 ! Loop over all receivers.

   HDUM1 = (0.,0.) ; HDUM2 = (0.,0.)

   IF (SURVEY_TYPE == 1) THEN                      ! Loop
     RXX = XRXTX(JR,JS,1)
     RYY = YRXTX(JR,JS,1)
     RZZ = ZRXTX(JR,JS)
     XFC = 0. ; YFC = 0. ; ZFC = 0.
     IF (SOURCE_TYPE == 1) THEN
       DO J = 1, NVLOOP
         DO I = 0, IFIX(SXL1(J))
           X =  RXX - (SXN(J,JS) + I*CS(J))
           Y =  RYY - (SXE(J,JS) + I*SN(J))
           Z =  RZZ - (SXZ(JS)   + I*CFC(J))
           RHO = MAX((X*X+Y*Y+Z*Z),1.)
           RHO = SQRT(RHO)
           XFC = XFC - (Z*SFC(J)*CS(J)-X*CFC(J))/(RHO*RHO*RHO) / (4.*PI)
           YFC = YFC - (Y*CFC(J)-Z*SFC(J)*SN(J))/(RHO*RHO*RHO) / (4.*PI)
           ZFC = ZFC -  SFC(J)*(X*SN(J)-Y*CS(J))/(RHO*RHO*RHO) / (4.*PI)
           ZFC = ZFC -  (X*SN(J)-Y*CS(J))/(RHO*RHO*RHO) / (4.*PI)
         END DO
       END DO
     ELSE IF (SOURCE_TYPE == 2) THEN
       DO J = 1, NVLOOP
         DO I = 0, IFIX(SXL1(J))
           X =  RXX - (SXN(J,JS) + I*CS(J))
           Y =  RYY - (SXE(J,JS) + I*SN(J))
           Z =  RZZ - (SXZ(JS)   + I*CFC(J))
           RHO = MAX((X*X+Y*Y+Z*Z),1.)
           RHO = SQRT(RHO)
           XFC =  XFC - (Z*SFC(J)*CS(J)-X*CFC(J))/(RHO*RHO*RHO) / (4.*PI)
           YFC =  YFC - (Y*CFC(J)-Z*SFC(J)*SN(J))/(RHO*RHO*RHO) / (4.*PI)
           ZFC =  ZFC -  SFC(J)*(X*SN(J)-Y*CS(J))/(RHO*RHO*RHO) / (4.*PI)
         END DO
         R1 = MAX((RXX-SXN(J,JS))**2+(RYY-SXE(J,JS))**2+(RZZ-SXZ(JS))**2,1.)
         R2 = MAX((RXX-SXN(J+1,JS))**2+(RYY-SXE(J+1,JS))**2+(RZZ-SXZ(JS))**2,1.)
         R1 = SQRT(R1) ; R2 = SQRT(R2) ; FCT = 1.
         XFC =  XFC - FCT * ((RXX-SXN(J,JS))/(R1*R1*R1) - (RXX-SXN(J+1,JS))/(R2*R2*R2))
         YFC =  YFC - FCT * ((RYY-SXE(J,JS))/(R1*R1*R1) - (RYY-SXE(J+1,JS))/(R2*R2*R2))
         ZFC =  ZFC - FCT * ((RZZ-SXZ(JS))/(R1*R1*R1)   - (RZZ-SXZ(JS))/(R2*R2*R2))
       END DO
     ELSE IF (SOURCE_TYPE == 3) THEN
       X =  RXX - SXN(1,JS)
       Y =  RYY - SXE(1,JS)
       Z =  RZZ - SXZ(JS)
       RHO = MAX(SQRT(X*X+Y*Y+Z*Z),0.1)
       XFC = 3.*X*Z/(RHO*RHO*RHO*RHO*RHO)
       YFC = 3.*Y*Z/(RHO*RHO*RHO*RHO*RHO)
       ZFC = 1./(RHO*RHO*RHO) * (3.*Z*Z/(RHO*RHO) - 1.) / (4.*PI)
     END IF

     IF (RXID(JR,JS) == 1) THEN                        ! Magnetic dipole receivers
        CALL HGREEN (ELOC,NLOC,ZLOC,NE,NN,NZ,NAIR,KSQ,F,FRQ,NINEDG,NEDGE, &
                     DIME,DIMN,DIMZ,NELEM,RXX,RYY,RZZ,EEDG,XFC,YFC,ZFC,HX, &
                     HY,HZ,NPART)

        WRITE(ND,'(6E16.7,1X,2F10.2,1X,E16.7,1x,I8)') HY,HX,HZ,RXX,RYY,FRQ,ITER
     ELSE IF (RXID(JR,JS) == 2) THEN                   ! Electric dipole receivers.
        NINTEG_MAX = 5
        NINTEG = 5
        ALLOCATE (VERTEX(3,MQVR),INTEG_POINTS(3,NINTEG_MAX))
        VERTEX(1,1) = YRXTX(JR,JS,1) ; VERTEX(1,2) = YRXTX(JR,JS,2)
        VERTEX(2,1) = XRXTX(JR,JS,1) ; VERTEX(2,2) = XRXTX(JR,JS,2)
        VERTEX(3,1) = ZRXTX(JR,JS)   ; VERTEX(3,2) = ZRXTX(JR,JS)
        INTEG_POINTS(1:3,1) = VERTEX(1:3,1)
        DO J = 2, NINTEG
          INTEG_POINTS(1,J) = INTEG_POINTS(1,J-1) + (VERTEX(1,2)-VERTEX(1,1))/(NINTEG-1)
          INTEG_POINTS(2,J) = INTEG_POINTS(2,J-1) + (VERTEX(2,2)-VERTEX(2,1))/(NINTEG-1)
          INTEG_POINTS(3,J) = INTEG_POINTS(3,J-1) + (VERTEX(3,2)-VERTEX(3,1))/(NINTEG-1)
        END DO
        INTEG_WEIGHT(1:3) = (VERTEX (1:3,2)-VERTEX (1:3,1)) / NINTEG
        HNX = (0.,0.) ; HNY = (0.,0.) ; HNZ = (0.,0.)
        DO K = 1, NINTEG
          RXX = INTEG_POINTS(1,K)
          RYY = INTEG_POINTS(2,K)
          RZZ = INTEG_POINTS(3,K)
          XFC = 0.; YFC = 0. ; ZFC = 0.
          CALL EGREEN (ELOC,NLOC,ZLOC,NE,NN,NZ,NAIR,F,FRQ,NINEDG,NEDGE,DIME, &
                       DIMN,DIMZ,NELEM,RXX,RYY,RZZ,HX,HY,HZ,EEDG,KSQ,XFC,YFC, &
                       ZFC,NPART)
          HNX = HNX + HX * INTEG_WEIGHT(1)
          HNY = HNY + HY * INTEG_WEIGHT(2)
          HNZ = HNZ + HZ * INTEG_WEIGHT(3)
        END DO
        H_TOT = NINTEG * (HNX + HNY + HNZ)
        WRITE(ND,'(6E16.7,1X,2F10.2,1X,E16.7,1x,I8)') H_TOT,HDUM1,HDUM2,RXX,RYY,FRQ,ITER
        DEALLOCATE (VERTEX,INTEG_POINTS)
     END IF
   ELSE IF (SURVEY_TYPE == 2) THEN                      ! Moving rectangular loop Tx with fixed offset Magnetic Dipole receivers
     DO JJ = 1, MQVR
       RXX = XRXTX(JR,JS,JJ)
       RYY = YRXTX(JR,JS,JJ)
       RZZ = ZRXTX(JR,JS)
       XFC = 0. ; YFC = 0. ; ZFC = 0.
       DO J = 1, NVLOOP
         DO I = 0, IFIX(SXL1(J))
           X =  RXX - (SXN(J,JS) + I*CS(J))
           Y =  RYY - (SXE(J,JS) + I*SN(J))
           Z =  RZZ - (SXZ(JS)   + I*CFC(J))
           RHO = MAX((X*X+Y*Y+Z*Z),1.)
           RHO = SQRT(RHO)
           XFC = XFC - (Z*SFC(J)*CS(J)-X*CFC(J))/(RHO*RHO*RHO)  / (4.*PI)
           YFC = YFC - (Y*CFC(J)-Z*SFC(J)*SN(J))/(RHO*RHO*RHO)  / (4.*PI)
           ZFC = ZFC - SFC(J)*(X*SN(J)-Y*CS(J))/(RHO*RHO*RHO) / (4.*PI)
          END DO
       END DO
       CALL HGREEN (ELOC,NLOC,ZLOC,NE,NN,NZ,NAIR,KSQ,F,FRQ,NINEDG,NEDGE, &
                    DIME,DIMN,DIMZ,NELEM,RXX,RYY,RZZ,EEDG,XFC,YFC,ZFC,HX, &
                    HY,HZ,NPART)

       WRITE(ND,'(6E16.7,1X,2F10.2,1X,E16.7,1x,I8)') HY,HX,HZ,RXX,RYY,FRQ,ITER
     END DO
   ELSE IF (SURVEY_TYPE == 3) THEN                      ! Surface Magnetic Dipole-Dipole receivers
     RXX = XRXTX(JR,JS,1)
     RYY = YRXTX(JR,JS,1)
     RZZ = ZRXTX(JR,JS)
     X =  RXX - SXN(1,JS)
     Y =  RYY - SXE(1,JS)
     Z =  RZZ - SXZ(JS)
     RHO = MAX(SQRT(X*X+Y*Y+Z*Z),0.1)
     XFC = 3.*X*Z/(RHO*RHO*RHO*RHO*RHO)
     YFC = 3.*Y*Z/(RHO*RHO*RHO*RHO*RHO)
     ZFC = 1./(RHO*RHO*RHO) * (3.*Z*Z/(RHO*RHO) - 1.) / (4.*PI)
     CALL HGREEN (ELOC,NLOC,ZLOC,NE,NN,NZ,NAIR,KSQ,F,FRQ,NINEDG,NEDGE, &
                  DIME,DIMN,DIMZ,NELEM,RXX,RYY,RZZ,EEDG,XFC,YFC,ZFC,HX, &
                  HY,HZ,NPART)

     WRITE(ND,'(6E16.7,1X,2F10.2,1X,E16.7,1x,I8)') HY,HX,HZ,RXX,RYY,FRQ,ITER
   ELSE IF (SURVEY_TYPE == 4) THEN                      ! Coincident loops
     HNX = (0.,0.) ; HNY = (0.,0.) ; HNZ = (0.,0.)
     DO K = 1, NRS(1,JS)
       RXX = XRS(K,1,JS)
       RYY = YRS(K,1,JS)
       RZZ = ZRXTX(1,JS)
       DO J = 1, 4
         DO I = 0, IFIX(SXL1(J))
           X =  RXX - (SXN(J,JS) + I*CS(J))
           Y =  RYY - (SXE(J,JS) + I*SN(J))
           Z =  RZZ - (SXZ(JS)   + I*CFC(J))
           RHO = MAX((X*X+Y*Y+Z*Z),1.)
           RHO = SQRT(RHO)
           XFC = XFC - (Z*SFC(J)*CS(J)-X*CFC(J))/(RHO*RHO*RHO) / (4.*PI)
           YFC = YFC - (Y*CFC(J)-Z*SFC(J)*SN(J))/(RHO*RHO*RHO) / (4.*PI)
           ZFC = ZFC -  SFC(J)*(X*SN(J)-Y*CS(J))/(RHO*RHO*RHO) / (4.*PI)
           ZFC = ZFC -  (X*SN(J)-Y*CS(J))/(RHO*RHO*RHO) / (4.*PI)
         END DO
       END DO
       CALL HGREEN (ELOC,NLOC,ZLOC,NE,NN,NZ,NAIR,KSQ,F,FRQ,NINEDG,NEDGE, &
                    DIME,DIMN,DIMZ,NELEM,RXX,RYY,RZZ,EEDG,XFC,YFC,ZFC,HX, &
                    HY,HZ,NPART)
       HNX = HNX + HX * WTRS(K,1,JS)
       HNY = HNY + HY * WTRS(K,1,JS)
       HNZ = HNZ + HZ * WTRS(K,1,JS)
     END DO
!     H_TOT = (HNX + HNY + HNZ)
     H_TOT = HNZ
     WRITE(ND,'(6E16.7,1X,2F10.2,1X,E16.7,1x,I8)') H_TOT,HDUM1,HDUM2,RXX,RYY,FRQ,ITER
   ELSE IF (SURVEY_TYPE == 5) THEN                      ! Borehole Magnetic Dipole-Dipole survey
     RXX = XRXTX(JR,JS,1)
     RYY = YRXTX(JR,JS,1)
     RZZ = ZRXTX(JR,JS)
     X =  RXX - SXN(1,JS)
     Y =  RYY - SXE(1,JS)
     Z =  RZZ - SXZ(JS)
     RHO = MAX(SQRT(X*X+Y*Y+Z*Z),0.1)
     XFC = 3.*X*Z/(RHO*RHO*RHO*RHO*RHO)
     YFC = 3.*Y*Z/(RHO*RHO*RHO*RHO*RHO)
     ZFC = 1./(RHO*RHO*RHO) * (3.*Z*Z/(RHO*RHO) - 1.) / (4.*PI)
     CALL HGREEN (ELOC,NLOC,ZLOC,NE,NN,NZ,NAIR,KSQ,F,FRQ,NINEDG,NEDGE, &
                  DIME,DIMN,DIMZ,NELEM,RXX,RYY,RZZ,EEDG,XFC,YFC,ZFC,HX, &
                  HY,HZ,NPART)
     WRITE(ND,'(6E16.7,1X,2F10.2,1X,E16.7,1x,I8)') HY,HX,HZ,RXX,RYY,FRQ,ITER
   ELSE IF (SURVEY_TYPE == 6) THEN                      ! Magnetotelluric

     WRITE(*,*) ' NOT IMPLEMENTED YET !'

   END IF
 END DO

 END SUBROUTINE HFIELD

!===========================================================================

 SUBROUTINE HGREEN (ELOC,NLOC,ZLOC,NE,NN,NZ,NAIR,KSQ,F,FRQ,NINEDG,NEDGE, &
                    DIME,DIMN,DIMZ,NELEM,RXX,RYY,RZZ,EEDG,XFC,YFC,ZFC,HX, &
                    HY,HZ,NPART)

!---------------------------------------------------------------------------
!
!*** Called by: HFIELD
!
! Compute integral equation for magnetic field.
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 REAL, PARAMETER :: MU0=12.56637E-7, TWOPI=6.283185
 INTEGER NE,NN,NZ,NELEM,CX(4),CY(4),CZ(4),L1,L2,L3,I,J,K,IJ,NBN,I1,I2,I3, &
         NINEDG,NAIR,NEDGE(12,(NE-1)*(NN-1)*(NZ-1)),NPART
 COMPLEX EN(3),EEDG(NINEDG),F(NINEDG),HX,HY,HZ,KSQ(NPART),STC,P1,PM,IWMU
 REAL ELOC(NE,NN,NZ),NLOC(NE,NN,NZ),ZLOC(NE,NN,NZ),S(3),WG(3),S1,S2,S3, &
      FRQ,XM(12),YM(12),ZM(12),FCT,X,Y,Z,SHP1,SHP2,W1,W2,W3,SHP3,VOL,RR, &
      DIME(NELEM),DIMN(NELEM),DIMZ(NELEM),XFC,YFC,ZFC,RXX,RYY,RZZ,GR(3,3)
 DATA CX/1,4,9,12/,CY/5,6,7,8/,CZ/2,3,10,11/
 DATA XM/ 0.,-1., 1., 0.,-1., 1.,-1., 1., 0.,-1., 1., 0./,         &
      YM/-1.,-1.,-1.,-1., 0., 0., 0., 0., 1., 1., 1., 1./,         &
      ZM/-1., 0., 0., 1.,-1.,-1., 1., 1.,-1., 0., 0., 1./
 DATA S(1),S(2),S(3)/-0.774596669241483,0.,+0.774596669241483/
 DATA WG(1),WG(2),WG(3)/0.555555555555555,0.888888888888888,0.555555555555555/

 IWMU = (0.,1.)*TWOPI*FRQ*MU0

 HX = (0.,0.) ; HY = (0.,0.) ; HZ = (0.,0.)

 DO J = 1, NN-1
   DO K = 1, NZ-1
     DO I = 1, NE-1
       NBN  = (J-1)*(NE-1)*(NZ-1) + (K-1)*(NE-1) + I
       STC  = KSQ(NBN) / (2.*TWOPI)
       IF ( K > NAIR )   THEN
         VOL = DIMN(NBN) * DIME(NBN) * DIMZ(NBN)
         DO L1 = 1, 3
           DO L2 = 1, 3
             DO L3 = 1, 3

               S1 = S(L1)  ; S2 = S(L2)  ; S3 = S(L3)
               W1 = WG(L1) ; W2 = WG(L2) ; W3 = WG(L3)
               X =  RXX - (NLOC(I,J,K)+(1.+S(L2))*DIMN(NBN))
               Y =  RYY - (ELOC(I,J,K)+(1.+S(L1))*DIME(NBN))
               Z =  RZZ - (ZLOC(I,J,K)+(1.+S(L3))*DIMZ(NBN))

               FCT = W1 * W2 * W3 * VOL

               RR = MAX( SQRT(X*X+Y*Y+Z*Z), 0.01)
               P1 = SQRT(IWMU*1.E-4) * RR
               PM = EXP(-P1) * ((1.,0.)+P1) / (RR*RR*RR)

               GR(1,1) =  0.
               GR(1,2) =  Z    ! XY
               GR(1,3) = -X    ! XZ
               GR(2,1) = -Z    ! YX
               GR(2,2) =  0.
               GR(2,3) =  Y    ! YZ
               GR(3,1) =  X    ! ZX
               GR(3,2) = -Y    ! ZY
               GR(3,3) =  0.

               EN = (0.,0.)
               DO IJ = 1, 4
                 I1 = CX(IJ) ; I2 = CY(IJ) ; I3 = CZ(IJ)
                 SHP1  = (1.+S1*XM(I1))*(1.+S2*YM(I1))*(1.+S3*ZM(I1))/4.
                 SHP2  = (1.+S1*XM(I2))*(1.+S2*YM(I2))*(1.+S3*ZM(I2))/4.
                 SHP3  = (1.+S1*XM(I3))*(1.+S2*YM(I3))*(1.+S3*ZM(I3))/4.
                 EN(1) = EN(1) + SHP1 * (F(NEDGE(I1,NBN)) + EEDG(NEDGE(I1,NBN)))
                 EN(2) = EN(2) + SHP2 * (F(NEDGE(I2,NBN)) + EEDG(NEDGE(I2,NBN)))
                 EN(3) = EN(3) + SHP3 * (F(NEDGE(I3,NBN)) + EEDG(NEDGE(I3,NBN)))
               END DO
               HX = HX - PM*STC*FCT*(GR(1,1)*EN(1) + GR(1,2)*EN(2) + GR(1,3)*EN(3))
               HY = HY - PM*STC*FCT*(GR(2,1)*EN(1) + GR(2,2)*EN(2) + GR(2,3)*EN(3))
               HZ = HZ - PM*STC*FCT*(GR(3,1)*EN(1) + GR(3,2)*EN(2) + GR(3,3)*EN(3))
             END DO
           END DO
         END DO
       END IF
     END DO
   END DO
 END DO

 HX =  CMPLX(XFC,0.) + HX
 HY =  CMPLX(YFC,0.) + HY
 HZ =  CMPLX(ZFC,0.) + HZ

 END SUBROUTINE HGREEN

!===========================================================================

 SUBROUTINE EGREEN (ELOC,NLOC,ZLOC,NE,NN,NZ,NAIR,F,FRQ,NINEDG,NEDGE,DIME, &
                    DIMN,DIMZ,NELEM,RXX,RYY,RZZ,HX,HY,HZ,EEDG,KSQ,XFC,YFC, &
                    ZFC,NPART)

!---------------------------------------------------------------------------
!
!*** Called by: HFIELD
!
! Compute integral equation for the electric field.
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 REAL, PARAMETER :: MU0=12.56637E-7, TWOPI=6.283185, PI=3.141592654
 INTEGER NE,NN,NZ,NELEM,CX(4),CY(4),CZ(4),L1,L2,L3,I,J,K,IJ,NBN,I1,I2,I3, &
         NINEDG,NAIR,NEDGE(12,(NE-1)*(NN-1)*(NZ-1)),NPART
 COMPLEX EN(3),F(NINEDG),HX,HY,HZ,STC,P1,GR(3,3),KSQ1,P3,P4,IWMU,SIGD, &
         EEDG(NINEDG),KSQ(NPART)
 REAL ELOC(NE,NN,NZ),NLOC(NE,NN,NZ),ZLOC(NE,NN,NZ),S(3),WG(3),S1,S2,S3,FRQ, &
      XM(12),YM(12),ZM(12),FCT,X,Y,SHP1,SHP2,SHP3,DIME(NELEM),DIMN(NELEM),  &
      DIMZ(NELEM),VOL,Z,RR,RXX,RYY,RZZ,RR2,W1,W2,W3,XFC,YFC,ZFC,FACT
 DATA CX/1,4,9,12/,CY/5,6,7,8/,CZ/2,3,10,11/
 DATA XM/ 0.,-1., 1., 0.,-1., 1.,-1., 1., 0.,-1., 1., 0./, &
      YM/-1.,-1.,-1.,-1., 0., 0., 0., 0., 1., 1., 1., 1./, &
      ZM/-1., 0., 0., 1.,-1.,-1., 1., 1.,-1., 0., 0., 1./
 DATA S(1),S(2),S(3)/-0.774596669241483,0.,+0.774596669241483/
 DATA WG(1),WG(2),WG(3)/0.555555555555555,0.888888888888888,0.555555555555555/

 IWMU = (0.,1.)*TWOPI*FRQ*MU0

 DO J = 1, NN-1
   IF ( (NLOC(1+NE/2,J,NAIR+1) <= RXX) .AND. (NLOC(1+NE/2,J+1,NAIR+1) > RXX)) EXIT
 END DO
 DO I = 1, NE-1
   IF ( (ELOC(I,1+NN/2,NAIR+1) <= RYY) .AND. (ELOC(I+1,1+NN/2,NAIR+1) > RYY)) EXIT
 END DO
 DO K = 1, NZ-1
   IF ( (ZLOC(1+NE/2,1+NN/2,K) <= RZZ) .AND. (ZLOC(1+NE/2,1+NN/2,K+1) > RZZ)) EXIT
 END DO

 NBN  = (J-1)*(NE-1)*(NZ-1) + (K-1)*(NE-1) + I
 SIGD = KSQ(NBN)/IWMU

 HX = (0.,0.) ; HY = (0.,0.) ; HZ = (0.,0.) ; GR = (0.,0.)
 DO J = 1, NN-1
   DO K = 1, NZ-1
     DO I = 1, NE-1
       NBN  = (J-1)*(NE-1)*(NZ-1) + (K-1)*(NE-1) + I
       IF ( K > NAIR )   THEN
         VOL = DIMN(NBN) * DIME(NBN) * DIMZ(NBN)
         DO L1 = 1, 3
           DO L2 = 1, 3
             DO L3 = 1, 3
               S1 = S(L1)  ; S2 = S(L2)  ; S3 = S(L3)
               W1 = WG(L1) ; W2 = WG(L2) ; W3 = WG(L3)
               X = (RXX - (NLOC(I,J,K) + (1.+S(L1))*DIMN(NBN)))
               Y = (RYY - (ELOC(I,J,K) + (1.+S(L2))*DIME(NBN)))
               Z = (RZZ - (ZLOC(I,J,K) + (1.+S(L3))*DIMZ(NBN)))

               FCT = W1 * W2 * W3 * VOL * 1.E3
               RR2 = MAX( (X*X+Y*Y+Z*Z), .1) ; RR = SQRT(RR2)

               STC  = IWMU * TWOPI
               KSQ1 = IWMU*1.E-6
               P1 =  SQRT(KSQ1) * RR
               P3 =  EXP(-P1)/(RR2*RR) * ((3.,0.) + 3.*P1 + RR2*KSQ1)
               P4 =  EXP(-P1)/(RR2*RR) * ((1.,0.) +    P1 + RR2*KSQ1)

               GR(1,1) = P3*X*X/RR2 - P4   ! XX
               GR(1,2) = P3*X*X/RR2        ! XY
               GR(1,3) = P3*X*X/RR2        ! XZ
               GR(2,1) = P3*Y*Y/RR2        ! YX
               GR(2,2) = P3*Y*Y/RR2 - P4   ! YY
               GR(2,3) = P3*Y*Y/RR2        ! YZ
               GR(3,1) = P3*Z*Z/RR2        ! ZX
               GR(3,2) = P3*Z*Z/RR2        ! ZY
               GR(3,3) = P3*Z*Z/RR2 - P4   ! ZZ

               EN = (0.,0.)
               DO IJ = 1, 4
                 I1 = CX(IJ) ; I2 = CY(IJ) ; I3 = CZ(IJ)
                 SHP1  = (1.+S1*XM(I1))*(1.+S2*YM(I1))*(1.+S3*ZM(I1))/4.
                 SHP2  = (1.+S1*XM(I2))*(1.+S2*YM(I2))*(1.+S3*ZM(I2))/4.
                 SHP3  = (1.+S1*XM(I3))*(1.+S2*YM(I3))*(1.+S3*ZM(I3))/4.
                 EN(1) = EN(1) - SHP1 * (F(NEDGE(I1,NBN)) + EEDG(NEDGE(I1,NBN)))
                 EN(2) = EN(2) - SHP2 * (F(NEDGE(I2,NBN)) + EEDG(NEDGE(I2,NBN)))
                 EN(3) = EN(3) - SHP3 * (F(NEDGE(I3,NBN)) + EEDG(NEDGE(I3,NBN)))
               END DO

               HX = HX - FCT*STC*((GR(1,1)*EN(1) + GR(1,2)*EN(2) + GR(1,3)*EN(3)))
               HY = HY - FCT*STC*((GR(2,1)*EN(1) + GR(2,2)*EN(2) + GR(2,3)*EN(3)))
               HZ = HZ - FCT*STC*((GR(3,1)*EN(1) + GR(3,2)*EN(2) + GR(3,3)*EN(3)))

             END DO
           END DO
         END DO
       END IF
     END DO
   END DO
 END DO

 FACT = 8.E3 * PI / REAL(SIGD)

 HX = HX + CMPLX(FACT*XFC,0.)
 HY = HY + CMPLX(FACT*YFC,0.)
 HZ = HZ + CMPLX(FACT*ZFC,0.)

 END SUBROUTINE EGREEN

!===========================================================================

 SUBROUTINE RHS_DD (NE,NN,NZ,NAIR,NINEDG,NEDGE,NELEM,DIME,DIMN,DIMZ, &
                    F,EEDG,A,FRQ)

!---------------------------------------------------------------------------
!
!*** Calls: EDGELMR, EDMELMI
!*** Called by: LOKI_3D
!
! Set up the DD RHS of the system matrix.
!
!        NE - number of target nodes in X-direction
!        NN - number of target nodes in Y-direction
!        NZ - number of target nodes in Z-direction
!      NAIR - number of target nodes in air Z-direction
!      LITH - lithology-index arrays of the domain
!     NLITH - number of distinct lithologial units
!    NINEDG - total number of edges
!     NEDGE - array of the edges assignment
!     NELEM - total number of elements
! DIME,DIMN,-
!      DIMZ - dimension of the cells
!    NESWAP - indication of node numbering swapping
!     NSTAT - total number of transmitters
!         F - old RHS array of electric fields.
!         G - new RHS of electric fields for DD solution.
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 REAL, PARAMETER :: MU0 = 12.56637E-7, TWOPI = 6.283185
 INTEGER NE,NN,NZ,N,M,I,IX,IY,IZ,NINEDG,NBN,NELEM,NEDGE(12,NELEM), &
         IH(12),NED,NAIR,IXLAST,IYLAST
 REAL    ELI(9,12,12),DIME(NELEM),DIMN(NELEM),DIMZ(NELEM),FRQ
 COMPLEX F(NINEDG),A(NINEDG),EEDG(NINEDG),IWMU
 DATA (IH(I),I=1,12)/1,9,9,1,5,5,5,5,1,9,9,1/

 IWMU = (0.,1.)*TWOPI*FRQ*MU0 * 1.e-9

 IXLAST = NE - 1
 IYLAST = NN - 1

 A = (0.,0.)
 DO IY = 1, IYLAST
   DO IZ = 1, NZ-1
      DO IX = 1, IXLAST
       IF (IZ >= NAIR) THEN
         NBN = (IY-1)*IXLAST*(NZ-1) + (IZ-1)*IXLAST + IX
         CALL EDGELMI (DIME(NBN),DIMN(NBN),DIMZ(NBN),ELI)
         DO N = 1, 12
           NED = NEDGE(N,NBN)
           DO M = 1, 12
             A(NED) = A(NED) - IWMU*ELI(IH(N),N,M)*(F(NEDGE(M,NBN))+EEDG(NEDGE(M,NBN)))
           END DO
         END DO
       END IF
      END DO
   END DO
 END DO

 END SUBROUTINE RHS_DD

!===========================================================================

 SUBROUTINE DHFIELD (ELOC,NLOC,ZLOC,NE,NN,NZ,NAIR,NEL,NER,NNL,NNR,NZB,KSQ, &
                     FRQ,NTX,NRXTX,MRXTX,RXID,MQVR,MXRS,XRXTX,YRXTX,ZRXTX,NS, &
                     F,A,NINEDG,NEDGE,EEDG,DIME,DIMN,DIMZ,NELEM,JS,MXVRTX, &
                     SXE,SXN,NPART,NPAR,SURVEY_TYPE)

!---------------------------------------------------------------------------
!
!*** Called by: LOKI_3D
!*** Calls:
!
! Compute the secondary electric and magnetic fields.
!
!        ELOC - X - coordinates of the finite element nodes
!        NLOC - Y - coordinates of the finite element nodes
!        ZLOC - Z - coordinates of the finite element nodes
!        NRXG - number of receiver groups
!             - 1 if SURVEY_TYPE > 1
!    NRGTX(J) - number of receiver groups for transmitter J
!             - 1 if SURVEY_TYPE > 1
!  RX_TYPE(I) - for Rx group I: dipole (1), loop (2), grounded electrode (3)
! RGTXID(I,J) - receiver group index for Ith group for transmitter J
!      NRX(J) - the number of receivers in receiver group J.  MRX = MAXVAL (NRX)
!             - 1 if SURVEY_TYPE > 1
!         MRX - maximum number of receivers in any receiver group
!        MQVR - maximum number of vertices for all receivers
!             - 1 if all sources are magnetic dipoles)
! XRXTX(I,J,K)- north coordinate of the Kth vertex of the Ith receiver of transmitter J
! YRXTX(I,J,K)- east coordinate of the Kth vertex of the Ith receiver of transmitter J
!   ZRXTX(I,J)- depth of the Ith receiver of transmitter J
!             - K = 1 only for mag dipole Rx; 1 to 2 for electric dipole Rx; 1 to 4 for loop Rx
!          NE - number of target nodes in X-direction
!          NN - number of target nodes in Y-direction
!          NZ - number of target nodes in Z-direction
!        NAIR - number of target nodes in air in Z-direction
!        LITH - lithology-index arrays of the domain
!         KSQ - element k^^2-array
!       NLITH - number of distinct lithologial units
!         FRQ - frequency
!         NTX - total number of transmitters
!    RX,RY,RZ - receiver coord.
!          ND - save-file unit number
!           F - array of the secondary electric field at the target
!        NFRQ - number of frequencies
!   DIME,DIMN,-
!        DIMZ - dimension of the cells
! SURVEY_TYPE - Types of survey
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER NE,NN,NZ,NELEM,J,K,JR,JS,NTX,NS,NINEDG,NAIR,NEL,NER,NNL,NNR, &
         NZB,MRXTX,MQVR,NEDGE(12,(NE-1)*(NN-1)*(NZ-1)),RXID(MRXTX,NTX), &
         MXVRTX,JJ,NPART,NPAR,JP,NINTEG,NINTEG_MAX,MXRS,SURVEY_TYPE, &
         NRS(MRXTX,NTX)
 COMPLEX EEDG(NINEDG),F(NINEDG),A(NINEDG),KSQ(NPART),H_TOT,HDUM1,HDUM2
 REAL ELOC(NE,NN,NZ),NLOC(NE,NN,NZ),ZLOC(NE,NN,NZ),FRQ,DIME(NELEM), &
      DIMN(NELEM),DIMZ(NELEM),RXX,RYY,RZZ,INTEG_WEIGHT(3)
 INTEGER,DIMENSION (NTX) :: NRXTX
 REAL,DIMENSION(MRXTX,NTX,MQVR) :: XRXTX,YRXTX
 REAL,DIMENSION(MXRS,MRXTX,NTX) :: XRS,YRS,WTRS
 REAL,DIMENSION(MRXTX,NTX) :: ZRXTX
 REAL,DIMENSION(MXVRTX,NTX) :: SXE,SXN
 COMPLEX,DIMENSION(NPAR) :: HX,HY,HZ,HNX,HNY,HNZ
 REAL,ALLOCATABLE :: VERTEX(:,:),INTEG_POINTS(:,:)

 XRS = 0 ; YRS = 0 ; WTRS = 1.
 CALL SET_RX_SUBNET_CL (NTX,SXN,SXE,MXRS,NRS,XRS,YRS,WTRS)

 DO JR = 1, NRXTX(JS)                 ! Loop over all receivers.
   HDUM1 = (0.,0.) ; HDUM2 = (0.,0.)
   IF (SURVEY_TYPE == 1) THEN                      ! Loop
     IF (RXID(JR,JS) == 1) THEN                        ! Magnetic dipole receivers
       RXX = XRXTX(JR,JS,1)
       RYY = YRXTX(JR,JS,1)
       RZZ = ZRXTX(JR,JS)
       CALL DHGREEN (ELOC,NLOC,ZLOC,NE,NN,NZ,NAIR,NEL,NER,NNL,NNR,NZB,KSQ, &
                     F,A,FRQ,NINEDG,NEDGE,DIME,DIMN,DIMZ,NELEM,RXX,RYY,RZZ, &
                     EEDG,HX,HY,HZ,NPART,NPAR)
       DO JP = 1, NPAR
         WRITE(NS,'(6E16.7)') HY(JP),HX(JP),HZ(JP)
       END DO
     ELSE IF (RXID(JR,JS) == 2) THEN                    ! Electric dipole receivers.
       NINTEG_MAX = 5
       NINTEG = 5
       ALLOCATE (VERTEX(3,MQVR),INTEG_POINTS(3,NINTEG_MAX))
       VERTEX(1,1) = YRXTX(JR,JS,1) ; VERTEX(1,2) = YRXTX(JR,JS,2)
       VERTEX(2,1) = XRXTX(JR,JS,1) ; VERTEX(2,2) = XRXTX(JR,JS,2)
       VERTEX(3,1) = ZRXTX(JR,JS)   ; VERTEX(3,2) = ZRXTX(JR,JS)
       INTEG_POINTS(1:3,1) = VERTEX(1:3,1)
       DO J = 2, NINTEG
         INTEG_POINTS(1,J) = INTEG_POINTS(1,J-1) + (VERTEX(1,2)-VERTEX(1,1))/(NINTEG-1)
         INTEG_POINTS(2,J) = INTEG_POINTS(2,J-1) + (VERTEX(2,2)-VERTEX(2,1))/(NINTEG-1)
         INTEG_POINTS(3,J) = INTEG_POINTS(3,J-1) + (VERTEX(3,2)-VERTEX(3,1))/(NINTEG-1)
       END DO
       INTEG_WEIGHT(1:3) = (VERTEX (1:3,2)-VERTEX (1:3,1)) / NINTEG
       HNX = (0.,0.) ; HNY = (0.,0.) ; HNZ = (0.,0.)
       DO K = 1, NINTEG
         RXX = INTEG_POINTS(1,K)
         RYY = INTEG_POINTS(2,K)
         RZZ = INTEG_POINTS(3,K)
         CALL DEGREEN (ELOC,NLOC,ZLOC,NE,NN,NZ,NAIR,NEL,NER,NNL,NNR,NZB,F,A, &
                       FRQ,NINEDG,NEDGE,DIME,DIMN,DIMZ,NELEM,RXX,RYY,RZZ,HX, &
                       HY,HZ,EEDG,KSQ,NPART,NPAR)
         DO JP = 1, NPAR
           HNX(JP) = HNX(JP) + HX(JP) * INTEG_WEIGHT(1)
           HNY(JP) = HNY(JP) + HY(JP) * INTEG_WEIGHT(2)
           HNZ(JP) = HNZ(JP) + HZ(JP) * INTEG_WEIGHT(3)
         END DO
       END DO
       DO JP = 1, NPAR
         H_TOT = NINTEG * (HNX(JP) + HNY(JP) + HNZ(JP))
         WRITE(NS,'(6E16.7)') H_TOT,HDUM1,HDUM2
       END DO
       DEALLOCATE (VERTEX,INTEG_POINTS)
     END IF
   ELSE IF (SURVEY_TYPE == 2) THEN                      ! Moving rectangular loop Tx with fixed offset Magnetic Dipole receivers
     DO JJ = 1, MQVR
       RXX = XRXTX(JR,JS,JJ)
       RYY = YRXTX(JR,JS,JJ)
       RZZ = ZRXTX(JR,JS)
       CALL DHGREEN (ELOC,NLOC,ZLOC,NE,NN,NZ,NAIR,NEL,NER,NNL,NNR,NZB,KSQ, &
                     F,A,FRQ,NINEDG,NEDGE,DIME,DIMN,DIMZ,NELEM,RXX,RYY,RZZ, &
                     EEDG,HX,HY,HZ,NPART,NPAR)
       DO JP = 1, NPAR
         WRITE(NS,'(6E16.7)') HY(JP),HX(JP),HZ(JP)
       END DO
     END DO
   ELSE IF (SURVEY_TYPE == 3) THEN                      ! Surface Magnetic Dipole-Dipole receivers
     RXX = XRXTX(JR,JS,1)
     RYY = YRXTX(JR,JS,1)
     RZZ = ZRXTX(JR,JS)
     CALL DHGREEN (ELOC,NLOC,ZLOC,NE,NN,NZ,NAIR,NEL,NER,NNL,NNR,NZB,KSQ, &
                   F,A,FRQ,NINEDG,NEDGE,DIME,DIMN,DIMZ,NELEM,RXX,RYY,RZZ, &
                   EEDG,HX,HY,HZ,NPART,NPAR)
     DO JP = 1, NPAR
       WRITE(NS,'(6E16.7)') HY(JP),HX(JP),HZ(JP)
     END DO
   ELSE IF (SURVEY_TYPE == 4) THEN                      ! Coincident loops
     HNX = (0.,0.) ; HNY = (0.,0.) ; HNZ = (0.,0.)
     DO K = 1, NRS(1,JS)
       RXX = XRS(K,1,JS)
       RYY = YRS(K,1,JS)
       RZZ = ZRXTX(1,JS)
       CALL DHGREEN (ELOC,NLOC,ZLOC,NE,NN,NZ,NAIR,NEL,NER,NNL,NNR,NZB,KSQ, &
                     F,A,FRQ,NINEDG,NEDGE,DIME,DIMN,DIMZ,NELEM,RXX,RYY,RZZ, &
                     EEDG,HX,HY,HZ,NPART,NPAR)
       DO JP = 1, NPAR
         HNX(JP) = HNX(JP) + HX(JP) * WTRS(K,1,JS)
         HNY(JP) = HNY(JP) + HY(JP) * WTRS(K,1,JS)
         HNZ(JP) = HNZ(JP) + HZ(JP) * WTRS(K,1,JS)
       END DO
     END DO
     DO JP = 1, NPAR
!       H_TOT = HNX(JP) + HNY(JP) + HNZ(JP)
       H_TOT = HNZ(JP)
       WRITE(NS,'(6E16.7)') H_TOT,HDUM1,HDUM2
     END DO
     DEALLOCATE (VERTEX,INTEG_POINTS)
   ELSE IF (SURVEY_TYPE == 6) THEN                      ! Magnetotelluric
     WRITE(*,*) ' NOT IMPLEMENTED YET !'
   END IF
 END DO

 END SUBROUTINE DHFIELD

!===========================================================================

 SUBROUTINE DHGREEN (ELOC,NLOC,ZLOC,NE,NN,NZ,NAIR,NEL,NER,NNL,NNR,NZB,KSQ, &
                     F,A,FRQ,NINEDG,NEDGE,DIME,DIMN,DIMZ,NELEM,RXX,RYY,RZZ, &
                     EEDG,HX,HY,HZ,NPART,NPAR)

!---------------------------------------------------------------------------
!
!*** Called by: DHFIELD
!
! Compute integral equation for magnetic field sensitivity.
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 REAL, PARAMETER :: MU0=12.56637E-7, TWOPI=6.283185
 INTEGER NE,NN,NZ,NELEM,CX(4),CY(4),CZ(4),L1,L2,L3,I,J,K,IJ,NBN,JC,I1,I2,I3, &
         NINEDG,NAIR,NEL,NER,NNL,NNR,NZB,NEDGE(12,(NE-1)*(NN-1)*(NZ-1)), &
         NPART,NPAR
 COMPLEX EN(3),EEDG(NINEDG),F(NINEDG),A(NINEDG),KSQ(NPART),STC,KEL,P1,PM, &
         IWMU
 REAL ELOC(NE,NN,NZ),NLOC(NE,NN,NZ),ZLOC(NE,NN,NZ),S(3),WG(3),S1,S2,S3, &
      FRQ,XM(12),YM(12),ZM(12),FCT,X,Y,Z,SHP1,SHP2,W1,W2,W3,SHP3,VOL,RR, &
      DIME(NELEM),DIMN(NELEM),DIMZ(NELEM),RXX,RYY,RZZ,GR(3,3)
 COMPLEX,DIMENSION(NPAR) :: HX,HY,HZ
 DATA CX/1,4,9,12/,CY/5,6,7,8/,CZ/2,3,10,11/
 DATA XM/ 0.,-1., 1., 0.,-1., 1.,-1., 1., 0.,-1., 1., 0./,         &
      YM/-1.,-1.,-1.,-1., 0., 0., 0., 0., 1., 1., 1., 1./,         &
      ZM/-1., 0., 0., 1.,-1.,-1., 1., 1.,-1., 0., 0., 1./
 DATA S(1),S(2),S(3)/-0.774596669241483,0.,+0.774596669241483/
 DATA WG(1),WG(2),WG(3)/0.555555555555555,0.888888888888888,0.555555555555555/

 IWMU = (0.,1.)*TWOPI*FRQ*MU0

 HX = (0.,0.) ; HY = (0.,0.) ; HZ = (0.,0.)

 DO J = NNL+1, NN-NNR-1
   DO K = NAIR+1, NZ-NZB-1
     DO I = NEL+1, NE-NER-1

       JC  = (J-NNL-1)*(NE-NEL-NER-1)*(NZ-NAIR-NZB-1) + (K-NAIR-1)*(NE-NEL-NER-1) + I-NEL ! User domain
       NBN = (J    -1)*(NE        -1)*(NZ         -1) + (K     -1)*(NE        -1) + I     ! Full domain

       STC = KSQ(NBN) / (2.*TWOPI)
       KEL = IWMU / (2.*TWOPI)

       VOL = DIMN(NBN) * DIME(NBN) * DIMZ(NBN)

       DO L1 = 1, 3
         DO L2 = 1, 3
           DO L3 = 1, 3

             S1 = S(L1)  ; S2 = S(L2)  ; S3 = S(L3)
             W1 = WG(L1) ; W2 = WG(L2) ; W3 = WG(L3)
             X =  RXX - (NLOC(I,J,K)+(1.+S(L2))*DIMN(NBN))
             Y =  RYY - (ELOC(I,J,K)+(1.+S(L1))*DIME(NBN))
             Z =  RZZ - (ZLOC(I,J,K)+(1.+S(L3))*DIMZ(NBN))

             FCT = W1 * W2 * W3 * VOL

             RR = MAX( SQRT(X*X+Y*Y+Z*Z), 0.01)
             P1 = SQRT(IWMU*1.E-4) * RR
             PM = EXP(-P1) * ((1.,0.)+P1) / (RR*RR*RR)

             GR(1,1) =  0.
             GR(1,2) =  Z    ! XY
             GR(1,3) = -X    ! XZ
             GR(2,1) = -Z    ! YX
             GR(2,2) =  0.
             GR(2,3) =  Y    ! YZ
             GR(3,1) =  X    ! ZX
             GR(3,2) = -Y    ! ZY
             GR(3,3) =  0.

             EN = (0.,0.)
             DO IJ = 1, 4
               I1 = CX(IJ) ; I2 = CY(IJ) ; I3 = CZ(IJ)
               SHP1  = (1.+S1*XM(I1))*(1.+S2*YM(I1))*(1.+S3*ZM(I1))/4.
               SHP2  = (1.+S1*XM(I2))*(1.+S2*YM(I2))*(1.+S3*ZM(I2))/4.
               SHP3  = (1.+S1*XM(I3))*(1.+S2*YM(I3))*(1.+S3*ZM(I3))/4.
               EN(1) = EN(1) + SHP1 * (F(NEDGE(I1,NBN)) + EEDG(NEDGE(I1,NBN)))
               EN(2) = EN(2) + SHP2 * (F(NEDGE(I2,NBN)) + EEDG(NEDGE(I2,NBN)))
               EN(3) = EN(3) + SHP3 * (F(NEDGE(I3,NBN)) + EEDG(NEDGE(I3,NBN)))
             END DO
             HX(JC) = HX(JC) - PM*FCT*KEL*(GR(1,1)*EN(1) + GR(1,2)*EN(2) + GR(1,3)*EN(3))
             HY(JC) = HY(JC) - PM*FCT*KEL*(GR(2,1)*EN(1) + GR(2,2)*EN(2) + GR(2,3)*EN(3))
             HZ(JC) = HZ(JC) - PM*FCT*KEL*(GR(3,1)*EN(1) + GR(3,2)*EN(2) + GR(3,3)*EN(3))

             EN = (0.,0.)
             DO IJ = 1, 4
               I1 = CX(IJ) ; I2 = CY(IJ) ; I3 = CZ(IJ)
               SHP1  = (1.+S1*XM(I1))*(1.+S2*YM(I1))*(1.+S3*ZM(I1))/4.
               SHP2  = (1.+S1*XM(I2))*(1.+S2*YM(I2))*(1.+S3*ZM(I2))/4.
               SHP3  = (1.+S1*XM(I3))*(1.+S2*YM(I3))*(1.+S3*ZM(I3))/4.
               EN(1) = EN(1) + SHP1 * A(NEDGE(I1,NBN))
               EN(2) = EN(2) + SHP2 * A(NEDGE(I2,NBN))
               EN(3) = EN(3) + SHP3 * A(NEDGE(I3,NBN))
             END DO
             HX(JC) = HX(JC) - PM*FCT*STC*(GR(1,1)*EN(1) + GR(1,2)*EN(2) + GR(1,3)*EN(3))
             HY(JC) = HY(JC) - PM*FCT*STC*(GR(2,1)*EN(1) + GR(2,2)*EN(2) + GR(2,3)*EN(3))
             HZ(JC) = HZ(JC) - PM*FCT*STC*(GR(3,1)*EN(1) + GR(3,2)*EN(2) + GR(3,3)*EN(3))

           END DO
         END DO
       END DO
     END DO
   END DO
 END DO

 END SUBROUTINE DHGREEN

!===========================================================================

 SUBROUTINE DEGREEN (ELOC,NLOC,ZLOC,NE,NN,NZ,NAIR,NEL,NER,NNL,NNR,NZB,F,A, &
                     FRQ,NINEDG,NEDGE,DIME,DIMN,DIMZ,NELEM,RXX,RYY,RZZ,HX, &
                     HY,HZ,EEDG,KSQ,NPART,NPAR)

!---------------------------------------------------------------------------
!
!*** Called by: DHFIELD
!
! Compute integral equation for the electric field sensitivity.
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 REAL, PARAMETER :: MU0=12.56637E-7, TWOPI=6.283185
 INTEGER NE,NN,NZ,NELEM,CX(4),CY(4),CZ(4),L1,L2,L3,I,J,K,IJ,NBN,I1,I2,I3, &
         NINEDG,NAIR,NEL,NER,NNL,NNR,NZB,NEDGE(12,(NE-1)*(NN-1)*(NZ-1)), &
         NPART,NPAR,JC
 COMPLEX EN(3),F(NINEDG),A(NINEDG),STC,KEL,P1,GR(3,3),KSQ1,P3,P4,IWMU, &
         EEDG(NINEDG),KSQ(NPART),SIGD
 REAL ELOC(NE,NN,NZ),NLOC(NE,NN,NZ),ZLOC(NE,NN,NZ),S(3),WG(3),S1,S2,S3,FRQ, &
      XM(12),YM(12),ZM(12),FCT,X,Y,SHP1,SHP2,SHP3,DIME(NELEM),DIMN(NELEM),  &
      DIMZ(NELEM),VOL,Z,RR,RXX,RYY,RZZ,RR2,W1,W2,W3
 COMPLEX,DIMENSION(NPAR) :: HX,HY,HZ
 DATA CX/1,4,9,12/,CY/5,6,7,8/,CZ/2,3,10,11/
 DATA XM/ 0.,-1., 1., 0.,-1., 1.,-1., 1., 0.,-1., 1., 0./, &
      YM/-1.,-1.,-1.,-1., 0., 0., 0., 0., 1., 1., 1., 1./, &
      ZM/-1., 0., 0., 1.,-1.,-1., 1., 1.,-1., 0., 0., 1./
 DATA S(1),S(2),S(3)/-0.774596669241483,0.,+0.774596669241483/
 DATA WG(1),WG(2),WG(3)/0.555555555555555,0.888888888888888,0.555555555555555/

 IWMU = (0.,1.)*TWOPI*FRQ*MU0

 DO J = 1, NN-1
   IF ( (NLOC(1+NE/2,J,NAIR+1) <= RXX) .AND. (NLOC(1+NE/2,J+1,NAIR+1) > RXX)) EXIT
 END DO
 DO I = 1, NE-1
   IF ( (ELOC(I,1+NN/2,NAIR+1) <= RYY) .AND. (ELOC(I+1,1+NN/2,NAIR+1) > RYY)) EXIT
 END DO
 DO K = 1, NZ-1
   IF ( (ZLOC(1+NE/2,1+NN/2,K) <= RZZ) .AND. (ZLOC(1+NE/2,1+NN/2,K+1) > RZZ)) EXIT
 END DO

 NBN  = (J-1)*(NE-1)*(NZ-1) + (K-1)*(NE-1) + I

 SIGD = KSQ(NBN)/IWMU

 HX = (0.,0.) ; HY = (0.,0.) ; HZ = (0.,0.) ; GR = (0.,0.)

 DO J = NNL+1, NN-NNR-1
   DO K = NAIR+1, NZ-NZB-1
     DO I = NEL+1, NE-NER-1

       JC  = (J-NNL-1)*(NE-NEL-NER-1)*(NZ-NAIR-NZB-1) + (K-NAIR-1)*(NE-NEL-NER-1) + I-NEL ! User domain
       NBN = (J    -1)*(NE        -1)*(NZ         -1) + (K     -1)*(NE        -1) + I     ! Full domain

       STC = KSQ(NBN) / (2.*TWOPI)
       KEL = IWMU / (2.*TWOPI)

       VOL = DIMN(NBN) * DIME(NBN) * DIMZ(NBN)

       DO L1 = 1, 3
         DO L2 = 1, 3
           DO L3 = 1, 3
             S1 = S(L1)  ; S2 = S(L2)  ; S3 = S(L3)
             W1 = WG(L1) ; W2 = WG(L2) ; W3 = WG(L3)
             X = (RXX - (NLOC(I,J,K) + (1.+S(L1))*DIMN(NBN)))
             Y = (RYY - (ELOC(I,J,K) + (1.+S(L2))*DIME(NBN)))
             Z = (RZZ - (ZLOC(I,J,K) + (1.+S(L3))*DIMZ(NBN)))

             FCT = W1 * W2 * W3 * VOL * 1.E3
             RR2 = MAX( (X*X+Y*Y+Z*Z), .1) ; RR = SQRT(RR2)

             STC  = IWMU * TWOPI
             KSQ1 = IWMU * 1.E-6
             P1 =  SQRT(KSQ1) * RR
             P3 =  EXP(-P1)/(RR2*RR) * ((3.,0.) + 3.*P1 + RR2*KSQ1)
             P4 =  EXP(-P1)/(RR2*RR) * ((1.,0.) +    P1 + RR2*KSQ1)

             GR(1,1) = P3*X*X/RR2 - P4   ! XX
             GR(1,2) = P3*X*X/RR2        ! XY
             GR(1,3) = P3*X*X/RR2        ! XZ
             GR(2,1) = P3*Y*Y/RR2        ! YX
             GR(2,2) = P3*Y*Y/RR2 - P4   ! YY
             GR(2,3) = P3*Y*Y/RR2        ! YZ
             GR(3,1) = P3*Z*Z/RR2        ! ZX
             GR(3,2) = P3*Z*Z/RR2        ! ZY
             GR(3,3) = P3*Z*Z/RR2 - P4   ! ZZ

             EN = (0.,0.)
             DO IJ = 1, 4
               I1 = CX(IJ) ; I2 = CY(IJ) ; I3 = CZ(IJ)
               SHP1  = (1.+S1*XM(I1))*(1.+S2*YM(I1))*(1.+S3*ZM(I1))/4.
               SHP2  = (1.+S1*XM(I2))*(1.+S2*YM(I2))*(1.+S3*ZM(I2))/4.
               SHP3  = (1.+S1*XM(I3))*(1.+S2*YM(I3))*(1.+S3*ZM(I3))/4.
               EN(1) = EN(1) - SHP1 * (F(NEDGE(I1,NBN)) + EEDG(NEDGE(I1,NBN)))
               EN(2) = EN(2) - SHP2 * (F(NEDGE(I2,NBN)) + EEDG(NEDGE(I2,NBN)))
               EN(3) = EN(3) - SHP3 * (F(NEDGE(I3,NBN)) + EEDG(NEDGE(I3,NBN)))
             END DO

             HX(JC) = HX(JC) - FCT*STC*((GR(1,1)*EN(1) + GR(1,2)*EN(2) + GR(1,3)*EN(3))) / REAL(SIGD)
             HY(JC) = HY(JC) - FCT*STC*((GR(2,1)*EN(1) + GR(2,2)*EN(2) + GR(2,3)*EN(3))) / REAL(SIGD)
             HZ(JC) = HZ(JC) - FCT*STC*((GR(3,1)*EN(1) + GR(3,2)*EN(2) + GR(3,3)*EN(3))) / REAL(SIGD)

             EN = (0.,0.)
             DO IJ = 1, 4
               I1 = CX(IJ) ; I2 = CY(IJ) ; I3 = CZ(IJ)
               SHP1  = (1.+S1*XM(I1))*(1.+S2*YM(I1))*(1.+S3*ZM(I1))/4.
               SHP2  = (1.+S1*XM(I2))*(1.+S2*YM(I2))*(1.+S3*ZM(I2))/4.
               SHP3  = (1.+S1*XM(I3))*(1.+S2*YM(I3))*(1.+S3*ZM(I3))/4.
               EN(1) = EN(1) - SHP1 * A(NEDGE(I1,NBN))
               EN(2) = EN(2) - SHP2 * A(NEDGE(I2,NBN))
               EN(3) = EN(3) - SHP3 * A(NEDGE(I3,NBN))
             END DO

             HX(JC) = HX(JC) - FCT*KEL*((GR(1,1)*EN(1) + GR(1,2)*EN(2) + GR(1,3)*EN(3)))
             HY(JC) = HY(JC) - FCT*KEL*((GR(2,1)*EN(1) + GR(2,2)*EN(2) + GR(2,3)*EN(3)))
             HZ(JC) = HZ(JC) - FCT*KEL*((GR(3,1)*EN(1) + GR(3,2)*EN(2) + GR(3,3)*EN(3)))

           END DO
         END DO
       END DO
     END DO
   END DO
 END DO

 END SUBROUTINE DEGREEN

!===========================================================================

 SUBROUTINE EDGELMR (A,B,C,ELR,EC,NC,ZC)

!---------------------------------------------------------------------------
!
! Called by: BMAT
!
! Calculates the element stiffness matrix.
!
!        A - length of element in X-direction
!        B - length of element in Y-direction
!        C - length of element in Z-direction
!      ELR - real part of the element stiffness matrix
! EC,NC,ZC - coordinate of the edges
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER I,J,IG,JG,KG,IP,KX(4),KY(4),KZ(4),KX1,KY1,KZ1
 REAL A,B,C,WEIGHT,OP,GG(5),WG(5),ELR(9,12,12),R,S,T,VOL,XM(12), &
      YM(12),ZM(12),AA(3,6),DXDR,DYDR,DZDR,DXDS,DYDS,DZDS,DXDT,DYDT, &
      DZDT,EC(12),NC(12),ZC(12),DSRDR(12),DSRDS(12),DSRDT(12), &
      DSSDR(12),DSSDS(12),DSSDT(12),DSTDR(12),DSTDS(12),DSTDT(12), &
      DSXDY(12),DSXDZ(12),DSYDX(12),DSYDZ(12),DSZDX(12),DSZDY(12)
 DATA XM/ 0.,-1., 1., 0.,-1., 1.,-1., 1., 0.,-1., 1., 0./, &
      YM/-1.,-1.,-1.,-1., 0., 0., 0., 0., 1., 1., 1., 1./, &
      ZM/-1., 0., 0., 1.,-1.,-1., 1., 1.,-1., 0., 0., 1./
 DATA KX/1,4,9,12/, KY/5,6,7,8/, KZ/2,3,10,11/
 DATA GG(1),GG(2),GG(3)/-0.774597,0.,+0.774597/
 DATA WG(1),WG(2),WG(3)/0.5555556,0.8888889,0.5555556/

 ELR = 0.
 VOL= 8.*A*B*C
 DO IG = 1, 3
   DO JG = 1, 3
     DO KG = 1, 3
       R = GG(IG) ; S = GG(JG) ; T = GG(KG)
       WEIGHT = WG(IG) * WG(JG) * WG(KG) * VOL
       DSRDR = 0. ; DSRDS = 0. ; DSRDT = 0.
       DSSDR = 0. ; DSSDS = 0. ; DSSDT = 0.
       DSTDR = 0. ; DSTDS = 0. ; DSTDT = 0.
       DSXDY = 0. ; DSYDX = 0. ; DSZDX = 0.
       DSXDZ = 0. ; DSYDZ = 0. ; DSZDY = 0.
       DO I = 1, 4
         DSRDR(KX(I)) = 0.
         DSRDS(KX(I)) = (1.+T*ZM(KX(I)))*YM(KX(I))/ 8.
         DSRDT(KX(I)) = (1.+S*YM(KX(I)))*ZM(KX(I))/ 8.
         DSSDR(KY(I)) = (1.+T*ZM(KY(I)))*XM(KY(I))/ 8.
         DSSDS(KY(I)) = 0.
         DSSDT(KY(I)) = (1.+R*XM(KY(I)))*ZM(KY(I))/ 8.
         DSTDR(KZ(I)) = (1.+S*YM(KZ(I)))*XM(KZ(I))/ 8.
         DSTDS(KZ(I)) = (1.+R*XM(KZ(I)))*YM(KZ(I))/ 8.
         DSTDT(KZ(I)) = 0.
       END DO
       DXDR = 0. ; DYDR = 0.; DZDR = 0.
       DXDS = 0. ; DYDS = 0.; DZDS = 0.
       DXDT = 0. ; DYDT = 0.; DZDT = 0.
       DO I = 1, 4
         KX1 = KX(I) ; KY1 = KY(I) ; KZ1 = KZ(I)
         DXDR = DXDR + (DSRDR(KX1)*EC(KX1)+DSSDR(KY1)*EC(KY1)+DSTDR(KZ1)*EC(KZ1))
         DXDS = DXDS + (DSRDS(KX1)*EC(KX1)+DSSDS(KY1)*EC(KY1)+DSTDS(KZ1)*EC(KZ1))
         DXDT = DXDT + (DSRDT(KX1)*EC(KX1)+DSSDT(KY1)*EC(KY1)+DSTDT(KZ1)*EC(KZ1))
         DYDR = DYDR + (DSRDR(KX1)*NC(KX1)+DSSDR(KY1)*NC(KY1)+DSTDR(KZ1)*NC(KZ1))
         DYDS = DYDS + (DSRDS(KX1)*NC(KX1)+DSSDS(KY1)*NC(KY1)+DSTDS(KZ1)*NC(KZ1))
         DYDT = DYDT + (DSRDT(KX1)*NC(KX1)+DSSDT(KY1)*NC(KY1)+DSTDT(KZ1)*NC(KZ1))
         DZDR = DZDR + (DSRDR(KX1)*ZC(KX1)+DSSDR(KY1)*ZC(KY1)+DSTDR(KZ1)*ZC(KZ1))
         DZDS = DZDS + (DSRDS(KX1)*ZC(KX1)+DSSDS(KY1)*ZC(KY1)+DSTDS(KZ1)*ZC(KZ1))
         DZDT = DZDT + (DSRDT(KX1)*ZC(KX1)+DSSDT(KY1)*ZC(KY1)+DSTDT(KZ1)*ZC(KZ1))
       END DO
       AA(1,1) = DXDR ; AA(1,2) = DYDR ; AA(1,3) = DZDR
       AA(2,1) = DXDS ; AA(2,2) = DYDS ; AA(2,3) = DZDS
       AA(3,1) = DXDT ; AA(3,2) = DYDT ; AA(3,3) = DZDT
       AA(1,4) = 1.   ; AA(1,5) = 0.   ; AA(1,6) = 0.
       AA(2,4) = 0.   ; AA(2,5) = 1.   ; AA(2,6) = 0.
       AA(3,4) = 0.   ; AA(3,5) = 0.   ; AA(3,6) = 1.
       DO IP = 1, 3
         DO I = 1, 3
           IF (I /= IP) THEN
              OP = -AA(I,IP) / AA(IP,IP)
              DO J = IP, 6
                 AA(I,J) = AA(I,J) + OP * AA(IP,J)
              END DO
           END IF
         END DO
       END DO
       DXDR = AA(1,4) / AA(1,1) ; DYDR = AA(1,5) / AA(1,1) ; DZDR = AA(1,6) / AA(1,1)
       DXDS = AA(2,4) / AA(2,2) ; DYDS = AA(2,5) / AA(2,2) ; DZDS = AA(2,6) / AA(2,2)
       DXDT = AA(3,4) / AA(3,3) ; DYDT = AA(3,5) / AA(3,3) ; DZDT = AA(3,6) / AA(3,3)
       DO I = 1, 4
         DSXDY(KX(I)) = (DXDS*DSRDR(KX(I)) + DYDS*DSRDS(KX(I)) + DZDS*DSRDT(KX(I)))
         DSXDZ(KX(I)) = (DXDT*DSRDR(KX(I)) + DYDT*DSRDS(KX(I)) + DZDT*DSRDT(KX(I)))
         DSYDX(KY(I)) = (DXDR*DSSDR(KY(I)) + DYDR*DSSDS(KY(I)) + DZDR*DSSDT(KY(I)))
         DSYDZ(KY(I)) = (DXDT*DSSDR(KY(I)) + DYDT*DSSDS(KY(I)) + DZDT*DSSDT(KY(I)))
         DSZDX(KZ(I)) = (DXDR*DSTDR(KZ(I)) + DYDR*DSTDS(KZ(I)) + DZDR*DSTDT(KZ(I)))
         DSZDY(KZ(I)) = (DXDS*DSTDR(KZ(I)) + DYDS*DSTDS(KZ(I)) + DZDS*DSTDT(KZ(I)))
       END DO
       DO I = 1, 4
         DO J = 1, 4
           ELR(1,KX(I),KX(J)) = ELR(1,KX(I),KX(J)) + 1.1*WEIGHT*(DSXDY(KX(J))*DSXDY(KX(I)) &
                                                               + DSXDZ(KX(I))*DSXDZ(KX(J)))
           ELR(5,KY(I),KY(J)) = ELR(5,KY(I),KY(J)) + 1.1*WEIGHT*(DSYDZ(KY(J))*DSYDZ(KY(I)) &
                                                               + DSYDX(KY(I))*DSYDX(KY(J)))
           ELR(9,KZ(I),KZ(J)) = ELR(9,KZ(I),KZ(J)) + 1.1*WEIGHT*(DSZDY(KZ(J))*DSZDY(KZ(I)) &
                                                               + DSZDX(KZ(I))*DSZDX(KZ(J)))
         END DO
       END DO
       DO I = 1, 12
         DO J = 1, 12
           ELR(2,I,J) = ELR(2,I,J) - WEIGHT * (DSYDX(J)*DSXDY(I))
           ELR(4,I,J) = ELR(4,I,J) - WEIGHT * (DSXDY(J)*DSYDX(I))
           ELR(3,I,J) = ELR(3,I,J) - WEIGHT * (DSZDX(J)*DSXDZ(I))
           ELR(7,I,J) = ELR(7,I,J) - WEIGHT * (DSXDZ(J)*DSZDX(I))
           ELR(6,I,J) = ELR(6,I,J) - WEIGHT * (DSZDY(J)*DSYDZ(I))
           ELR(8,I,J) = ELR(8,I,J) - WEIGHT * (DSYDZ(J)*DSZDY(I))
         END DO
       END DO
     END DO
   END DO
 END DO

 END SUBROUTINE EDGELMR

!===========================================================================

 SUBROUTINE EDGELMI (A,B,C,ELI)

!---------------------------------------------------------------------------
!
!*** Called by: BMAT
!
! Calculates the element stiffness matrix.
!
!    A - length of element in X-direction
!    B - length of element in Y-direction
!    C - length of element in Z-direction
!  ELI - Imaginary part of the element stiffness matrix
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER I,J,IG,JG,KG,KX(4),KY(4),KZ(4)
 REAL A,B,C,WEIGHT,GG(5),WG(5),ELI(9,12,12),R,S,T,VOL,XM(12),YM(12),ZM(12), &
      SR(12),SS(12),ST(12)
 DATA XM/ 0.,-1., 1., 0.,-1., 1.,-1., 1., 0.,-1., 1., 0./, &
      YM/-1.,-1.,-1.,-1., 0., 0., 0., 0., 1., 1., 1., 1./, &
      ZM/-1., 0., 0., 1.,-1.,-1., 1., 1.,-1., 0., 0., 1./
 DATA KX/1,4,9,12/, KY/5,6,7,8/, KZ/2,3,10,11/
 DATA GG(1),GG(2),GG(3)/-0.774597,0.,+0.774597/
 DATA WG(1),WG(2),WG(3)/0.5555556,0.8888889,0.5555556/

 ELI = 0.
 VOL= 8.*A*B*C
 DO IG = 1, 3
   DO JG = 1, 3
     DO KG = 1, 3
       R = GG(IG) ; S = GG(JG) ; T = GG(KG)
       WEIGHT = WG(IG) * WG(JG) * WG(KG) * VOL
       SR = 0. ; SS = 0. ; ST = 0.
       DO I = 1, 4
         SR(KX(I)) = (1.+S*YM(KX(I))) * (1.+T*ZM(KX(I))) / 8.
         SS(KY(I)) = (1.+R*XM(KY(I))) * (1.+T*ZM(KY(I))) / 8.
         ST(KZ(I)) = (1.+R*XM(KZ(I))) * (1.+S*YM(KZ(I))) / 8.
       END DO
       DO I = 1, 4
         DO J = I, 4
           ELI(1,KX(I),KX(J)) = ELI(1,KX(I),KX(J)) + WEIGHT*(SR(KX(J))*SR(KX(I)))
           ELI(5,KY(I),KY(J)) = ELI(5,KY(I),KY(J)) + WEIGHT*(SS(KY(J))*SS(KY(I)))
           ELI(9,KZ(I),KZ(J)) = ELI(9,KZ(I),KZ(J)) + WEIGHT*(ST(KZ(J))*ST(KZ(I)))
         END DO
       END DO
       DO I = 1, 4
         DO J = I, 4
           ELI(1,KX(J),KX(I)) = ELI(1,KX(I),KX(J))
           ELI(5,KY(J),KY(I)) = ELI(5,KY(I),KY(J))
           ELI(9,KZ(J),KZ(I)) = ELI(9,KZ(I),KZ(J))
         END DO
       END DO
     END DO
   END DO
 END DO

 END SUBROUTINE EDGELMI

!===========================================================================!
!                                                                           !
! Structure of the inversion subroutines                                    !
! --------------------------------------                                    !
!                                                                           !
! Loki can solve for either the over-determined (NDATA > NPAR) or the       !
! under-determined (NDATA < NPAR) inverse problem. This makes no difference !
! to the way the Jacobian matrix is constructed. It does make a difference  !
! however to the way the Jacobian matrix is decomposed via SVD.             !
!                                                                           !
! The general structure of the subroutines are as follows:                  !
!                                                                           !
! NLSQ               - Nonlinear least squares                              !
!       RESJAC       - Residual vector and Jacobian matrix construction     !
!       ESVD         - Singular value decomposition of the Jacobian matrix  !
!       SOLVE        - Solve for updated model parameters                   !
!       IMPORTANCE   - Singular value analysis for parameter importance     !
!       NOISE_2_SIGR - Compute the noise to signal ratio for final model    !
!                                                                           !
! For Loki inversion, RESJAC computes the residual vector using the         !
! Loki modelling algorithm, and computes the Jacobian matrix using the      !
! domain-differentiation method.                                            !
!                                                                           !
!===========================================================================!

 SUBROUTINE NLSQ (NW,np,ND,NS,NM,MV1PRT,OUTPRT,MAXITS,CNVRG,PCTCNV,NDATA, &
                  XDATA,XMODL,XWTS,NPAR,CXPAR,ELAS,LBND,UBND,NE,NN,NZ,NAIR, &
                  NEL,NER,NNL,NNR,NZB,ELOC,NLOC,ZLOC,LITH,LYTH,NLITH,NPROP, &
                  XPART,NPART,STEP,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP, &
                  NCHNL,TOPN,TCLS,FREQ,NFRQ,KFRQE,NTX,MRXTX,NRXTX,RXID,NCMP, &
                  SOURCE_TYPE,NVRTX,MXVRTX,MQVR,SXN,SXE,SXZ,SXDIP,SXAZM,XRXTX, &
                  YRXTX,ZRXTX,NFT,NLINES,MRXL,NRX,LNTR,KNORM2,RX_TYPE,UNITS, &
                  ISYS,IDH,SVAZM,MD1,MD2,RXAZM,RXDIP,CURNT,BPRM,TDFD,INRM, &
                  DNORM,KCMP,MCHNL,LINE,VSTAT,NSTAT,NEDGE,KACC,SURVEY_TYPE,MXRS)

!----------------------------------------------------------------------------
!
!*** Called by: MAIN
!*** Calls: RESJAC, ESVD, SOLVE, WRITE_MODEL, WRITE_MISFIT
!
!  Nonlinear least square inversion.
!
!----------------------------------------------------------------------------
!
!  DESCRIPTION:
!
!  Nonlinear least square inversion based on the Gauss-Newton method using
!  second order Marquardt damping of the singular value decomposition of the
!  Jacobian matrix.
!
!  The solution for the update to the model parameters (DELPAR) by the
!  nonlinear least square problem is:
!
!  DELPAR = INV(TRANSPOSE(JAC)*JAC) * TRANSPOSE(JAC) * RES
!
!  where JAC is the Jacobian/Frechet/Sensitivity matrix. The singular value
!  decomposition of JAC is computed:
!
!  JAC = UMAT * SV * TRANSPOSE(VMAT)
!
!  where SV is a diagonal matrix of singular values, and UMAT and VMAT are
!  unitary matrices of eigenvectors. The solution to DELPAR now takes the
!  form:
!
!  DELPAR = VMAT * INV(SV) * TRANSPOSE(UMAT) * RES
!
!  The singular values are then damped using a second order Marquardt damping.
!
!  REFERENCE:
!
!  A.P. Raiche, D.L.B. Jupp, H. Rutter and K. Vozoff, 1985, The joint use of
!  coincident loop transient electromagnetic and Schlumberger sounding to
!  resolve layered structures: Geophysics, vol. 50, pp. 1618-1627.
!
!----------------------------------------------------------------------------
!
!     INPUTS:
!
!   NW     = Output unit number.
!
!   MV1PRT & OUTPRT are print options for the MV1 & OUT files respectively.
!          =  0  No output DURING inversion.  The final model set AFTER inversion,
!                but NOT the final model data, is written to output files.
!
!          =  1  as above plue plus final model data
!
!          =  2  as above plus intermediate model sets after each iteration
!
!          =  3 as above plus intermediate model data after each iteration
!
!   MAXITS = On call, is the maximum permitted iterations. On return, it is the
!            number of iterations performed.
!   CNVRG  = 1 : Converge on predicted decrease.
!          = 2 : Stop when RMS error < PCTCNV.
!   BND    = Estimate of the noise to signal ratio (NSR), used for the singular
!            value damping limit.
!   JAC    = Jacobian (Frechet/Sensitivity) matrix.
!   SV     = Singular values of the Jacobian matrix.
!   UMAT   = Matrix of data eigenvectors of the Jacobian matrix.
!   VMAT   = Matrix of model eigenvectors of the Jacobian matrix.
!   WSP    = Working space.
!   RES    = Residual error vector.
!   SIGMA  = Standard error.
!   RMSER  = RMS error (in percent).
!   NPAR   = Number of model parameters.
!   XPAR   = On call, XPAR contains the LOG of the initial parameter estimates.
!            On exit it contains the final (real) values.
!   NDATA  = Number of data points. For time domain, NCMP * NSTAT * NCHNL or for
!            frequency-domain, 2 * NFRQ * NSTAT.
!   XMODL  = Array of model data points.
!   XDATA  = Array of data points to be inverted.
!   XWTS   = Array of data weights for XDATA
!
!   The rest of the input variables are model specific. Refer to the header of
!   RESJAC for their definitions and/or values.
!
!----------------------------------------------------------------------------

 IMPLICIT NONE

 ! Inversion parameters.

 LOGICAL,PARAMETER :: WITHU = .TRUE., WITHV = .TRUE.
 INTEGER, PARAMETER :: IP = 0
 REAL, PARAMETER :: BND = 0.01, EXPND = 2., RSVT0 = 0.1, ETA = 1.E-7, &
                    TOL=.5E-31
 INTEGER NM,NW,np,NS,ND,MV1PRT,OUTPRT,ITS,CNVRG,NDATA,NPAR,NPART,TDFD, &
         ICNT,MAXITS,MAXITS_INT,INRM,NSV,JP,KLITH,FITS,NBN,NBNT,NLITH, &
         KCMP,MCHNL,SURVEY_TYPE
 LOGICAL JCBN,RESID,FINAL
 REAL PCTCNV,SUMSQ(2),SSQNEW(2),PSERR(2),FNM,GTEST,PDRE,WGTSUM,DELT,RSVT, &
      ZERO,NSR,B1,B2,DRMS,RMSE,MP,GCRIT
 REAL, DIMENSION(NDATA) :: XDATA,XMODL,RES,DNORM
 INTEGER, DIMENSION(NDATA) :: XWTS
 INTEGER, DIMENSION(NPAR) :: MODFIX
 INTEGER, DIMENSION(NLITH) :: CXPAR
 REAL, DIMENSION(NPAR)  :: DELPAR,GXPAR,IMPORT
 REAL, DIMENSION(NPAR)  :: XPAR,MODWGT,MUBND,MLBND
 REAL, DIMENSION(NPART) :: XPART,GXPART
 REAL, DIMENSION(NLITH) :: ELAS,LBND,UBND
 REAL, DIMENSION(NDATA,NPAR) :: JAC
 REAL, ALLOCATABLE, DIMENSION(:) :: SV,WSP,RMSERR,UTRES
 REAL, ALLOCATABLE, DIMENSION(:,:) :: UMAT,VMAT,JACT,UT

 ! Loki parameters.

 INTEGER STEP,NSX,NPULS,NTYPLS,NTYRP,NCHNL,NFRQ,KFRQE,NTX,MRXTX, &
         SOURCE_TYPE,NLINES,MRXL,MD1,MD2,LNTR(4,NLINES),MXVRTX,MQVR, &
         KNORM2(MRXTX,NTX),NFT,NE,NN,NZ,NAIR,NEL,NER,NNL,NNR,NZB,NPROP, &
         IX,IY,IZ,LITH(NE,NN,NZ),NEDGE(12,(NE-1)*(NN-1)*(NZ-1)),ISYS, &
         NSTAT,VSTAT(NSTAT),KACC,MXRS
 REAL PULSE,FREQ(NFRQ),SWX(NSX),SWY(NSX,3),TRP(NTYRP), &
      CURNT(NFT),BPRM(MRXTX,NTX),SVAZM(NLINES),ZRXTX(MRXTX,NTX), &
      LYTH(NLITH+1,NPROP)
 INTEGER,DIMENSION(NLINES) :: NRX,UNITS,RX_TYPE,IDH,LINE
 INTEGER,DIMENSION(NTX) :: NRXTX,NVRTX
 INTEGER,DIMENSION(MRXTX,NTX) :: NCMP,RXID
 REAL,DIMENSION(NCHNL) :: TOPN,TCLS
 REAL,DIMENSION(NTX) :: SXDIP,SXAZM,SXZ
 REAL,DIMENSION(MD1,MD2) :: RXAZM,RXDIP
 REAL,DIMENSION(MXVRTX,NTX) :: SXN,SXE
 REAL,DIMENSION(MRXTX,NTX,MQVR) :: XRXTX,YRXTX
 REAL,DIMENSION(NE,NN,NZ) :: ELOC,NLOC,ZLOC

 IF (TDFD == 2) THEN
   WRITE(NW,1) ; WRITE(*,1)  ! Frequency-domain.
 ELSE
   WRITE(NW,2) ; WRITE(*,2)  ! Time-domain.
 END IF

 WRITE(NW,3) MAXITS ; WRITE(*,3) MAXITS

 IF (INRM == 1) THEN
   WRITE(NW,25); WRITE(*,25) ! Point norm.
 ELSE
   WRITE(NW,26); WRITE(*,26) ! Survey norm.
 END IF

 ! Set up the model parameter, weight and bounds vectors.

! DO IY = NNL+1, NN-NNR-1
!   DO IZ = NAIR+1, NZ-NZB-1
!     DO IX = NEL+1, NE-NER-1
!       NBN  = (IY-NNL-1)*(NE-NEL-NER-1)*(NZ-NAIR-NZB-1) + (IZ-NAIR-1)*(NE-NEL-NER-1) + IX-NEL ! User domain
!       NBNT = (IY    -1)*(NE        -1)*(NZ         -1) + (IZ     -1)*(NE        -1) + IX     ! Full domain


 DO IY = 2, NN-2
   DO IZ = NAIR+1, NZ-2
     DO IX = 2, NE-2
! DO IY = 1, NN-1
!   DO IZ = NAIR+1, NZ-1
!     DO IX = 1, NE-1
       NBN  = (IY-2)*(NE-3)*(NZ-NAIR-2) + (IZ-NAIR-1)*(NE-3) + IX-1 ! User domain
       NBNT = (IY-1)*(NE-1)*(NZ-1)      + (IZ-1)*(NE-1)      + IX     ! Full domain
       KLITH = LITH(IX,IY,IZ)
       XPAR(NBN)   = XPART(NBNT)         ! Resistivity vector.
       MODWGT(NBN) = ELAS(KLITH)         ! Resistivity weights.
       MODFIX(NBN) = CXPAR(KLITH)        ! Vector of integer flags for updating model parameters.
       MUBND(NBN)  = UBND(KLITH)         ! Resistivity upper bound vector.
       MLBND(NBN)  = LBND(KLITH)         ! Resistivity lower bound vector.
     END DO
   END DO
 END DO

 GXPAR(1:NPAR)   = XPAR(1:NPAR)
 GXPART(1:NPART) = XPART(1:NPART)

 ! Determine if the inverse problem is under-determined or over-determined and
 ! allocate appropriately.

 IF (NDATA >= NPAR) THEN
   ALLOCATE (UMAT(NDATA,NPAR),VMAT(NPAR,NPAR),SV(NPAR),WSP(3*NPAR), &
             UTRES(NPAR))
 ELSE
   ALLOCATE (UMAT(NDATA,NDATA),VMAT(NPAR,NDATA),SV(NDATA),WSP(3*NDATA), &
             UTRES(NDATA))
 END IF

 UMAT = 0. ; VMAT = 0. ; SV = 0. ; WSP = 0. ; UTRES = 0.

 ! Preset the threshold parameters and index workspace.

 ZERO = BND*BND
 IF (ETA >= ZERO) ZERO = ETA
 GCRIT = SQRT(ETA)

 ! Set the relative singular value threshold, RSVT. Initialise the eigenvalue
 ! damping at 10 percent, RSVT0 = 0.1.

 RSVT = MAX(BND,RSVT0)

 ! Initialise arrays and logical flags.

 JAC = 0. ; RES = 0. ; IMPORT = 0. ; FINAL = .FALSE.
 WGTSUM = REAL(SUM(XWTS)) ; ITS = 0 ; MAXITS_INT = 3 ; DRMS = 0.
 GTEST = 0. ; SUMSQ = 0. ; SSQNEW = 0.

 ALLOCATE(RMSERR(MAXITS)) ; RMSERR = 0.

 !------------------------------!
 ! START OF MAIN INVERSION LOOP !
 !------------------------------!

 INV_LOOP: DO ITS = 1, MAXITS

   WRITE(*,'(/T3,A,I3)') 'Begin iteration =',ITS

   WRITE(*,45) ! Compute the residual error vector (RES) and Jacobian matrix (JAC).

   JCBN = .TRUE. ; RESID = .TRUE. ; RES = 0. ; JAC = 0. ; SUMSQ = 0.

   CALL RESJAC (ND,NS,NE,NN,NZ,NAIR,NEL,NER,NNL,NNR,NZB,ELOC,NLOC,ZLOC, &
                LITH,LYTH,NLITH,NPROP,XPART,NPART,RESID,JCBN,STEP,NSX,SWX, &
                SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL,TOPN,TCLS,FREQ,NFRQ, &
                KFRQE,NTX,MRXTX,NRXTX,RXID,NCMP,SOURCE_TYPE,MXVRTX,MQVR, &
                NVRTX,SXN,SXE,SXZ,SXDIP,SXAZM,XRXTX,YRXTX,ZRXTX,NFT,NLINES, &
                MRXL,NRX,LNTR,KNORM2,RX_TYPE,UNITS,ISYS,IDH,SVAZM,MD1,MD2, &
                RXAZM,RXDIP,CURNT,BPRM,XMODL,NDATA,KCMP,INRM,DNORM,TDFD, &
                SUMSQ,RES,XDATA,XWTS,MCHNL,XPAR,NPAR,JAC,NEDGE,KACC,     &
                SURVEY_TYPE,MXRS)

   PSERR(1:2) = 100. * SQRT(SUMSQ(1:2)/WGTSUM)
   RMSERR(ITS) = PSERR(INRM)
   FNM = 0.01 * SQRT(SUMSQ(INRM))
   FNM = MAX(FNM,ETA)

   IF (ITS == 1) THEN
     WRITE(NW,4) PSERR(1:2),RSVT ; WRITE(*,4) PSERR(1:2),RSVT
     FITS = 0
     IF (MV1PRT > 1) THEN
       WRITE(np,30) FITS,RMSERR(ITS),RSVT
     END IF
     IF (MV1PRT == 3) CALL WRITE_MDATA(FITS)
     IF (OUTPRT == 3)  &
        CALL WRITE_MISFIT (NW,FITS,NDATA,NLINES,NRX,MCHNL,TDFD,NFRQ,FREQ, &
                          KCMP,NCHNL,RES,XMODL,XDATA,LINE)
   END IF

   IF (CNVRG == 1 .AND. RMSERR(ITS) < 1.) THEN
     WRITE(NW,10) 1.
     FINAL = .TRUE.
   ELSE IF (CNVRG == 2 .AND. RMSERR(ITS) < PCTCNV) THEN
     WRITE(NW,10) PCTCNV
     FINAL = .TRUE.
   END IF

   IF (NDATA >= NPAR) THEN

     !----------------------------------------------!
     ! Solve for over-determined case, NDATA > NPAR !
     !----------------------------------------------!

     WRITE(*,41)
     CALL ESVD (JAC,NDATA,NPAR,IP,WITHU,WITHV,SV,UMAT,VMAT,WSP,ETA,TOL,NW)
     IF ( ABS(SV(1)) < 1.E-30 ) THEN
       WRITE(NW,99) ; WRITE(*,99)
       RETURN
     END IF
     WRITE(*,42)

     ALLOCATE (UT(NPAR,NDATA))
     UT = TRANSPOSE(UMAT)
     UTRES = MATMUL(UT,RES)
     DEALLOCATE (UT)

     ! Solve for the update to the model parameters: DELPAR = VMAT * SV * UTRES
     ! and test for convergence on the predicted decrease. Loop over internal
     ! iterations.

     ICNT_LOOP1: DO ICNT = 0, MAXITS_INT

       IF (FINAL) EXIT ICNT_LOOP1

       WRITE(*,44) ICNT

       CALL SOLVE (NPAR,NPAR,VMAT,SV,UTRES,WSP,RSVT,ZERO,PDRE,NSV,DELPAR)

       CALL UPDATE_MODEL_PARAMETERS

       ! If the predicted residual decrease < 1 percent of RMS error,
       ! terminate iterations. Inversion won't improve.

       DELT = SQRT(PDRE)
       IF (DELT < FNM) THEN
         WRITE(NW,5) ; WRITE(*,5)
         WRITE(NW,6) ITS,PSERR(1:2),RSVT ; WRITE(*,6) ITS,PSERR(1:2),RSVT
         IF (CNVRG == 1 .AND. RMSERR(ITS) > 15.)    WRITE(NW,7)
         IF (CNVRG == 2 .AND. RMSERR(ITS) > PCTCNV) WRITE(NW,8) PCTCNV
         EXIT INV_LOOP
       END IF

       ! Get the error for model with corrected parameters. Test for improvement
       ! (decrease) in residual. If it fails, reduce step and try again. Give up
       ! and return after MAXIT "internal" iterations.

       JCBN = .FALSE. ; RESID = .TRUE. ; RES = 0. ; JAC = 0. ; SSQNEW = 0.

       CALL RESJAC (ND,NS,NE,NN,NZ,NAIR,NEL,NER,NNL,NNR,NZB,ELOC,NLOC,ZLOC, &
                    LITH,LYTH,NLITH,NPROP,GXPART,NPART,RESID,JCBN,STEP,NSX, &
                    SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL,TOPN,TCLS, &
                    FREQ,NFRQ,KFRQE,NTX,MRXTX,NRXTX,RXID,NCMP,SOURCE_TYPE, &
                    MXVRTX,MQVR,NVRTX,SXN,SXE,SXZ,SXDIP,SXAZM,XRXTX,YRXTX, &
                    ZRXTX,NFT,NLINES,MRXL,NRX,LNTR,KNORM2,RX_TYPE,UNITS,ISYS, &
                    IDH,SVAZM,MD1,MD2,RXAZM,RXDIP,CURNT,BPRM,XMODL,NDATA,KCMP, &
                    INRM,DNORM,TDFD,SSQNEW,RES,XDATA,XWTS,MCHNL,GXPAR,NPAR, &
                    JAC,NEDGE,KACC,SURVEY_TYPE,MXRS)

       PSERR(1:2) = 100. * SQRT(SSQNEW(1:2)/WGTSUM)
       RMSE = PSERR(INRM)
       WRITE(*,16) RMSE

       GTEST = SUMSQ(INRM) - SSQNEW(INRM)
       IF (GTEST > GCRIT*PDRE) THEN   ! Error reduced using smaller step.
         IF (ICNT == 0) THEN
           RSVT = RSVT / EXPND        !  Decrease eigenvalue threshold damping
           RSVT = MAX(BND,RSVT)
         END IF
         EXIT ICNT_LOOP1     ! Start next iteration
       END IF

       RSVT = RSVT * EXPND            !  No error decrease. Raise threshold

       IF (ICNT == MAXITS_INT) THEN
         WRITE(NW,9) ICNT             ! No improvement possible. Maybe another starting guess?
         EXIT INV_LOOP
       END IF

     END DO ICNT_LOOP1

   ELSE

     !-----------------------------------------------!
     ! Solve for under-determined case, NDATA < NPAR !
     !-----------------------------------------------!

     ! If NDATA < NPAR, then the Jacobian matrix has more columns than it does have
     ! rows. Doing the S.V.D. on that size matrix leads to expanded U and V matrices
     ! as the null space of the zero singular values are included. We can eliminate
     ! those null spaces by computing the S.V.D. of the transposed Jacobian matrix.
     ! That is, the transpose of the Jacobian matrix has more rows than columns and
     ! the U and V matrices are swapped.

     WRITE(*,41)
     ALLOCATE (JACT(NPAR,NDATA))
     JACT = TRANSPOSE(JAC)
     CALL ESVD (JACT,NPAR,NDATA,IP,WITHU,WITHV,SV,VMAT,UMAT,WSP,ETA,TOL,NW)
     IF ( ABS(SV(1)) < 1.E-30 ) THEN
       WRITE(NW,99) ; WRITE(*,99)
       RETURN
     END IF
     DEALLOCATE(JACT)
     WRITE(*,42)

     ALLOCATE (UT(NDATA,NDATA))
     UT = TRANSPOSE(UMAT)
     UTRES = MATMUL(UT,RES)
     DEALLOCATE (UT)

     ! Solve for the update to the model parameters and test for convergence on
     ! the predicted decrease. Loop over internal iterations.

     ICNT_LOOP2: DO ICNT = 0, MAXITS_INT

       IF (FINAL) EXIT ICNT_LOOP2

       WRITE(*,44) ICNT

       ! Solve for the updated model parameters.

       CALL SOLVE (NPAR,NDATA,VMAT,SV,UTRES,WSP,RSVT,ZERO,PDRE,NSV,DELPAR)

       CALL UPDATE_MODEL_PARAMETERS

       ! If the predicted residual decrease < 1 percent of RMS error, terminate
       ! iterations. Inversion won't improve.

       DELT = SQRT(PDRE)
       IF (DELT < FNM) THEN
         WRITE(NW,5) ; WRITE(*,5)
         WRITE(NW,6) ITS,PSERR(1:2),RSVT ; WRITE(*,6) ITS,PSERR(1:2),RSVT
         IF (CNVRG == 1 .AND. RMSERR(ITS) > 15.)    WRITE(NW,7)
         IF (CNVRG == 2 .AND. RMSERR(ITS) > PCTCNV) WRITE(NW,8) PCTCNV
         EXIT INV_LOOP
       END IF

       ! Get the error for model with corrected parameters. Test for improvement
       ! (decrease) in residual. If it fails, reduce step and try again. Give up
       ! and return after MAXIT "internal" iterations.

       JCBN = .FALSE. ; RESID = .TRUE. ; RES = 0. ; JAC = 0. ; SSQNEW = 0.

       CALL RESJAC (ND,NS,NE,NN,NZ,NAIR,NEL,NER,NNL,NNR,NZB,ELOC,NLOC,ZLOC, &
                    LITH,LYTH,NLITH,NPROP,GXPART,NPART,RESID,JCBN,STEP,NSX, &
                    SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL,TOPN,TCLS, &
                    FREQ,NFRQ,KFRQE,NTX,MRXTX,NRXTX,RXID,NCMP,SOURCE_TYPE, &
                    MXVRTX,MQVR,NVRTX,SXN,SXE,SXZ,SXDIP,SXAZM,XRXTX,YRXTX, &
                    ZRXTX,NFT,NLINES,MRXL,NRX,LNTR,KNORM2,RX_TYPE,UNITS,ISYS, &
                    IDH,SVAZM,MD1,MD2,RXAZM,RXDIP,CURNT,BPRM,XMODL,NDATA,KCMP, &
                    INRM,DNORM,TDFD,SSQNEW,RES,XDATA,XWTS,MCHNL,GXPAR,NPAR,JAC, &
                    NEDGE,KACC,SURVEY_TYPE,MXRS)

       PSERR(1:2) = 100. * SQRT(SSQNEW(1:2)/WGTSUM)
       RMSE = PSERR(INRM)
       WRITE(*,16) RMSE

       GTEST = SUMSQ(INRM) - SSQNEW(INRM)
       IF (GTEST > GCRIT*PDRE) THEN   ! Error reduced using smaller step.
         IF (ICNT == 0) THEN
           RSVT = RSVT / EXPND        ! Decrease eigenvalue threshold damping
           RSVT = MAX(BND,RSVT)
         END IF
         EXIT ICNT_LOOP2     ! Start next iteration
       END IF

       RSVT = RSVT * EXPND            ! No error decrease. Raise threshold

       IF (ICNT == MAXITS_INT) THEN
         WRITE(NW,9) ICNT             ! No improvement possible. Maybe another starting guess?
         EXIT INV_LOOP
       END IF

     END DO ICNT_LOOP2

   END IF ! End UNDER/OVER determined IF construct.

   XPAR(1:NPAR)   = GXPAR(1:NPAR)
   XPART(1:NPART) = GXPART(1:NPART)

   ! Write inversion result to Loki.res.

   OPEN(NM,FILE='Loki.res',STATUS='REPLACE')
   DO JP = 1, NPAR
     WRITE(NM,50) JP,XPAR(JP)
   END DO
   CLOSE(NM)

   ! The error has been reduced so accept the step, write out inversion summary
   ! and test convergence.

   DELT = SQRT(ABS(GTEST))
   SUMSQ (1:2) = SSQNEW(1:2)
   PSERR(1:2) = 100. * SQRT(SUMSQ(1:2)/WGTSUM)
   RMSERR(ITS) = PSERR(INRM)
   FNM = 0.01 * SQRT(SUMSQ(INRM))
   FNM = MAX(FNM,ETA)

   ! Write out the current model and continue iterating up until IT = MAXIT.

   IF (DELT < FNM) THEN
     WRITE(NW,5) ; WRITE(*,5)
     WRITE(NW,6) ITS,PSERR(1:2),RSVT ; WRITE(*,6) ITS,PSERR(1:2),RSVT
     IF (CNVRG == 1 .AND. RMSERR(ITS) > 15.)    WRITE(NW,7)
     IF (CNVRG == 2 .AND. RMSERR(ITS) > PCTCNV) WRITE(NW,8) PCTCNV
     EXIT INV_LOOP
   END IF
   IF (CNVRG == 2 .AND. RMSERR(ITS) < PCTCNV) THEN
     WRITE(NW,10) PCTCNV
     WRITE(NW,6) ITS,PSERR(1:2),RSVT ; WRITE(*,6) ITS,PSERR(1:2),RSVT
     EXIT INV_LOOP
   END IF

   IF (ITS > 3) DRMS = RMSERR(ITS-2) - RMSERR(ITS)
   IF (ITS == MAXITS) THEN
     WRITE(NW,11)
     WRITE(NW,6) ITS,PSERR(1:2),RSVT ; WRITE(*,6) ITS,PSERR(1:2),RSVT
     IF (DRMS > 1.) THEN
       WRITE(NW,14) DRMS
       WRITE(*,14)  DRMS
     END IF
     EXIT INV_LOOP
   END IF

   WRITE(np,30) ITS,RMSERR(ITS),RSVT
   IF (MV1PRT > 1) THEN
     WRITE(np,31) ITS
     WRITE(np,36) XPAR(1:NPAR)
   END IF
   IF (MV1PRT > 2) CALL WRITE_MDATA(ITS)

   WRITE(NW,6) ITS,PSERR(1:2),RSVT ; WRITE(*,6) ITS,PSERR(1:2),RSVT

   IF (OUTPRT > 1) CALL WRITE_INVMDL (NW,FINAL,ITS,NN,NE,NZ,NPAR,XPAR,IMPORT,NAIR, &
                                      NEL,NER,NNL,NNR,NZB)

   IF (OUTPRT == 3 .AND. ITS < MAXITS) &
     CALL WRITE_MISFIT (NW,FITS,NDATA,NLINES,NRX,MCHNL,TDFD,NFRQ,FREQ, &
                        KCMP,NCHNL,RES,XMODL,XDATA,LINE)

   IF (ITS > 10 .AND. DRMS < 1.) THEN
     WRITE(NW,15) ITS ; WRITE(*,15) ITS
     EXIT INV_LOOP
   END IF

 END DO INV_LOOP ! End of main inversion loop. Write the final model and exit.

 !---------------------------------------------------!
 ! Singular value analysis for parameter importance. !
 !---------------------------------------------------!

 IF (NDATA >= NPAR) THEN
   CALL IMPORTANCE (NPAR,NPAR,VMAT,WSP,IMPORT)
 ELSE IF (NPAR > NDATA) THEN
   CALL IMPORTANCE (NPAR,NDATA,VMAT,WSP,IMPORT)
 END IF

 DEALLOCATE(UMAT,VMAT,SV,WSP,UTRES)

 !------------------------------------------------------------!
 ! Compute the noise to signal ratio (NSR) and write to file. !
 !------------------------------------------------------------!

 CALL NOISE_2_SIGR (NPAR,NDATA,XMODL,XDATA,XWTS,NSR)

 WRITE(NW,12)
 WRITE(NW,13) PSERR(1:2),NSR

 IF (INRM == 1) THEN
   WRITE(NW,25); WRITE(*,25)
 ELSE
   WRITE(NW,26); WRITE(*,26)
 END IF

 !----------------------------------------!
 ! Write the final inverse model to file. !
 !----------------------------------------!

 FITS = -1
 WRITE(NW,12)
 WRITE(np,32) ITS,RMSERR(ITS),NSR,RSVT

 FINAL = .TRUE.
 CALL WRITE_INVMDL (NW,FINAL,ITS,NN,NE,NZ,NPAR,XPAR,IMPORT,NAIR, &
                    NEL,NER,NNL,NNR,NZB)

 WRITE(np,35)
 WRITE(np,33) XPAR(1:NPAR)
 WRITE(np,34) IMPORT(1:NPAR)
 IF (MV1PRT > 0) CALL WRITE_MDATA(FITS)

 !---------------------------------!
 ! Write the final misfit to file. !
 !---------------------------------!

 CALL WRITE_MISFIT (NW,FITS,NDATA,NLINES,NRX,MCHNL,TDFD,NFRQ,FREQ, &
                    KCMP,NCHNL,RES,XMODL,XDATA,LINE)

 DEALLOCATE (RMSERR)

 !-------------------!
 ! Print statements. !
 !-------------------!

 1  FORMAT(/T3,'Begin frequency-domain inversion.')
 2  FORMAT(/T3,'Begin time-domain inversion.')
 3  FORMAT(/T3,'Maximum iterations =',I3)
 4  FORMAT(/T3,'Initial Point-norm RMS error  =',F8.2,' percent.' &
           /T3,'Initial Survey-norm RMS error =',F8.2,' percent.' &
           /T3,'Initial Relative SV threshold =',F8.3)
 5  FORMAT(//T3,'Convergence on predicted decrease.')
 6  FORMAT(/I4,' iterations completed.' &
           /T6,'Point-norm RMS error  =',F8.2,' percent.' &
           /T6,'Survey-norm RMS error =',F8.2,' percent.' &
           /T6,'Relative SV threshold =',F8.3)
 7  FORMAT(/T3,'An alternative starting guess might achieve better results.')
 8  FORMAT(/T3,'The inversion was unable to achieve an RMS error' &
           /T3,'within the specified threshold of',F9.2,' percent.' &
           /T3,'An alternative starting guess may achieve better results.')
 9  FORMAT(/T3,'The solution is trapped. ICNT = ',I2/ &
           /T3,'Another starting guess may yield a better result.')
 10 FORMAT(/T3,'Convergence within RMS error threshold of',F7.2,' has been achieved.')
 11 FORMAT(//T3,'Inversion finished after maximum number of iterations.')
 12 FORMAT(//T3,50('='))
 13 FORMAT(/T23,'Point-norm RMS error  =',F8.2,' percent.' &
           /T22,'Survey-norm RMS error =',F8.2,' percent.' &
           /T22,'Noise to signal ratio =',F8.3)
 14 FORMAT(/T3,'------------------------------------------------------------------' &
           /T3,'The reduction in the RMS error during the last two iterations was ' &
           /T3,F5.2,' percent.  A better result may be achieved by using the final' &
           /T3,'model from this inversion as the starting guess for another run.'   &
           /T3,'------------------------------------------------------------------')
 15 FORMAT(/T3,'--------------------------------------------------------------------'  &
           /T3,'Inversion terminated after',I3,' iterations because, the reduction in' &
           /T3,'the RMS error from the last two iterations was less than 1 percent.'   &
           /T3,'--------------------------------------------------------------------')
 16 FORMAT(/T3,'RMS error =',F9.2,' percent.')
 25 FORMAT(//T3,'RMS error reduction based on symmetric point normalisation'&
            /T3,'----------------------------------------------------------')
 26 FORMAT(//T3,'RMS error reduction based on using the Survey norm' &
            /T3,'--------------------------------------------------')
 30 FORMAT(T1,'/'/T1,'/ ITERATION  ',I2.2/T1,'/ PERCENT_RMS_ERROR:',F9.2,3X,'RSVT:',F8.3)
 31 FORMAT(T1,'/'/T1,'/ MODEL_',I2.2)
 36 FORMAT(T1,'/ MODEL ',T15,100G13.4)
 32 FORMAT(T1,'/'/T1,'/ FINAL_ITERATION  ',I2.2/T1,'/ PERCENT_RMS_ERROR:',F8.2,3X,'NSR:',F7.3,3X,'RSVT:',F7.3)
 35 FORMAT(T1,'/')
 33 FORMAT(T1,'/ FINAL_MODEL',T15,100G13.4)
 34 FORMAT(T1,'/ IMPORTANCE ',T15,100G13.4)
 41 FORMAT(/T3,'SVD of the Jacobian matrix commenced.')
 42 FORMAT( T3,'SVD of the Jacobian matrix completed.')
 44 FORMAT(/T3,'Testing model updates for error reduction, internal iteration ',I3,' :')
 45 FORMAT(/T3,'Computing models for primal and sensitivity fields:')
 50 FORMAT(I4,G12.4)
 99 FORMAT(//T3,'Singular value decomposition failure. INVERSION HALTED.')

 CONTAINS

   SUBROUTINE UPDATE_MODEL_PARAMETERS

     DO JP = 1, NPAR
       SELECT CASE (MODFIX(JP))
       CASE(0) ! Free to vary.
         MP = LOG(XPAR(JP)) + DELPAR(JP)
         GXPAR(JP) = EXP(MP)
       CASE(1) ! Fixed at a priori value.
         GXPAR(JP) = XPAR(JP)
       CASE(2) ! Constrained by elasticity.
         MP = LOG(XPAR(JP)) + MODWGT(JP) * DELPAR(JP)
         GXPAR(JP) = EXP(MP)
       CASE(3) ! Constrained by elasticity and bounds.
         B1 = LOG(MLBND(JP))
         B2 = LOG(MUBND(JP))
         MP = LOG(XPAR(JP)) + MODWGT(JP) * DELPAR(JP)
         MP = MIN(MP,B2)
         MP = MAX(MP,B1)
         GXPAR(JP) = EXP(MP)
       END SELECT
     END DO

     DO IY = NNL+1, NN-NNR-1
       DO IZ = NAIR+1, NZ-NZB-1
         DO IX = NEL+1, NE-NER-1
           NBN  = (IY-NNL-1)*(NE-NEL-NER-1)*(NZ-NAIR-NZB-1) + (IZ-NAIR-1)*(NE-NEL-NER-1) + IX-NEL ! User domain
           NBNT = (IY    -1)*(NE        -1)*(NZ         -1) + (IZ     -1)*(NE        -1) + IX     ! Full domain
           GXPART(NBNT) = GXPAR(NBN) ! Resistivity vector.
         END DO
       END DO
     END DO

   END SUBROUTINE UPDATE_MODEL_PARAMETERS

   SUBROUTINE WRITE_MDATA(KTS)

     LOGICAL WL
     INTEGER JS,JL,JR,K0,KTS
     CHARACTER (LEN=80) QL0,QL1

     JS = 0
     DO JL = 1, NLINES
       WL = .TRUE.
       IF (JL > 1 .AND. LINE(JL) == LINE(JL-1)) WL = .FALSE.
       IF (WL) THEN
         IF (KTS < 0) THEN
           WRITE(QL0,2) LINE(JL)
         ELSE
           WRITE(QL0,1) LINE(JL),KTS
         END IF
         READ(QL0,'(A)') QL1
         WRITE(np,3) TRIM (ADJUSTL (QL1))
       END IF
       DO JR = 1, NRX(JL)
         JS = JS + 1
         K0 = (JS-1)*MCHNL
         WRITE(np,4) VSTAT(JS),XMODL(K0+1:K0+MCHNL),100.*RES(K0+1:K0+MCHNL)
       END DO
     END DO

     1 FORMAT(T2,I10,'_I',I2.2)
     2 FORMAT(T2,I10,'_ZFNL')
     3 FORMAT(T2,'Line ',A)
     4 FORMAT(I5,300G13.4)

   END SUBROUTINE WRITE_MDATA

 END SUBROUTINE NLSQ

!============================================================================

 SUBROUTINE RESJAC (ND,NS,NE,NN,NZ,NAIR,NEL,NER,NNL,NNR,NZB,ELOC,NLOC,ZLOC, &
                    LITH,LYTH,NLITH,NPROP,XPART,NPART,RESID,JCBN,STEP,NSX, &
                    SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL,TOPN,TCLS, &
                    FREQ,NFRQ,KFRQE,NTX,MRXTX,NRXTX,RXID,NCMP,SOURCE_TYPE, &
                    MXVRTX,MQVR,NVRTX,SXN,SXE,SXZ,SXDIP,SXAZM,XRXTX,YRXTX,ZRXTX, &
                    NFT,NLINES,MRXL,NRX,LNTR,KNORM2,RX_TYPE,UNITS,ISYS,IDH, &
                    SVAZM,MD1,MD2,RXAZM,RXDIP,CURNT,BPRM,XMODL,NDATA,KCMP, &
                    INRM,DNORM,TDFD,SUMSQ,RES,XDATA,XWTS,MCHNL,XPAR,NPAR, &
                    JAC,NEDGE,KACC,SURVEY_TYPE,MXRS)

!----------------------------------------------------------------------------
!
!*** Called by: NLSQ
!*** Calls: LOKI_3D, FDREAD, TD_FIELD, FD_FIELD, TD_SENS, FD_SENS
!
!  Construct the residual error vector (RES) and sensitivity matrix (JAC).
!
!----------------------------------------------------------------------------
!
!  DESCRIPTION:
!
!  Calls model computation using Loki 3D FEM to compute the residual!
!  error vector and sensitivity matrix as required.
!
!  Convention: Residual error = Observed data - Predicted data
!
!  In NLSQ, DELPAR will thus be added rather than subtracted during updates
!  of the model parameters.
!
!  The Jacobian matrix computed by the domain differentiation method in
!  S_FD_CONSTRUCT is dD/dM. These have to be converted to calibrated
!  units for each system, which is done respectively in the subroutines
!  TD_SENS and FD_SENS.
!
!  The sensitivity matrix is then constructed as the perfect change in
!  response per percent change in model parameter; equivalent to taking
!  the logarithms with respect to both data and model parameters:
!
!    dD/dM * dM/d(lnM) = dD/dM * M
!
!    dD/dM * d(lnD)/dD = dD/dM / D
!
!  so we end up with:
!
!    d(lnD)/d(lnM) = dD/dM * M/D
!
!----------------------------------------------------------------------------

 IMPLICIT NONE
 LOGICAL JCBN,RESID
 INTEGER ND,NS,NW,STEP,NSX,NPULS,NTYPLS,NTYRP,NCHNL,NFRQ,KFRQE,NTX,MRXTX, &
         SOURCE_TYPE,NLINES,MRXL,MD1,MD2,LNTR(4,NLINES),TDFD,KNORM2(MRXTX,NTX), &
         NFT,NE,NN,NZ,NPART,NLITH,NPROP,LITH(NE,NN,NZ),NDATA,INRM, &
         KCMP,ISYS,JD,JP,MCHNL,NPAR,KACC,NAIR,NEL,NER,NNL,NNR,NZB, &
         NEDGE(12,(NE-1)*(NN-1)*(NZ-1)),MXVRTX,MQVR,SURVEY_TYPE,MXRS
 REAL PULSE,FREQ(NFRQ),SWX(NSX),SWY(NSX,3),TRP(NTYRP), &
      CURNT(NFT),BPRM(MRXTX,NTX),SVAZM(NLINES),ZRXTX(MRXTX,NTX), &
      XPART(NPART),LYTH(NLITH+1,NPROP),SUMSQ(2),VD,VM,V1,V2,RES_ERR,DENOM
 INTEGER,DIMENSION(NLINES) :: NRX,UNITS,RX_TYPE,IDH
 INTEGER,DIMENSION(NTX) :: NRXTX,NVRTX
 INTEGER,DIMENSION(MRXTX,NTX) :: NCMP,RXID
 INTEGER,DIMENSION(NDATA) :: XWTS
 REAL,DIMENSION(NPAR) :: XPAR
 REAL,DIMENSION(NDATA) :: XMODL,XDATA,DNORM,RES
 REAL,DIMENSION(NDATA,NPAR) :: JAC
 REAL,DIMENSION(NCHNL) :: TOPN,TCLS
 REAL,DIMENSION(NTX) :: SXDIP,SXAZM,SXZ
 REAL,DIMENSION(MD1,MD2) :: RXAZM,RXDIP
 REAL,DIMENSION(MXVRTX,NTX) :: SXN,SXE
 REAL,DIMENSION(MRXTX,NTX,MQVR) :: XRXTX,YRXTX
 REAL,DIMENSION(NE,NN,NZ) :: ELOC,NLOC,ZLOC
 COMPLEX,ALLOCATABLE,DIMENSION(:,:,:,:) :: BFD
 COMPLEX,ALLOCATABLE,DIMENSION(:,:,:,:,:) :: SFD

 IF (RESID) OPEN(ND,FILE = 'Loki.frq',STATUS = 'REPLACE')
 IF (JCBN)  OPEN(NS,FILE = 'Loki.sty',STATUS = 'REPLACE')

 ! Compute forward model.
 ! ----------------------

 CALL LOKI_3D (ND,NS,NW,NFRQ,FREQ,SOURCE_TYPE,NTX,MXVRTX,NVRTX,SXE,SXN,SXZ, &
               SXDIP,SXAZM,NRXTX,MRXTX,RXID,MQVR,MXRS,XRXTX,YRXTX,ZRXTX,NE,NN,NZ, &
               ELOC,NLOC,ZLOC,NEDGE,NAIR,NEL,NER,NNL,NNR,NZB,KACC,LITH,LYTH, &
               NLITH,NPROP,XPART,NPART,NPAR,JCBN,SURVEY_TYPE)
! CALL LOKI_3D (ND,NS,NFRQ,FREQ,SOURCE_TYPE,NTX,MXVRTX,NVRTX,SXE,SXN, &
!               SXZ,SXDIP,SXAZ,NRXTX,MRXTX,RXID,MQVR,XRXTX,YRXTX, &
!               ZRXTX,NE,NN,NZ,ELOC,NLOC,ZLOC,NEDGE,NAIR,NEL,NER,NNL, &
!               NNR,NZB,KACC,LITH,LYTH,NLITH,NPROP,XPART,NPART,NPAR,JCBN)

 ! Residual error vector construction.
 ! -----------------------------------

 IF (RESID) THEN

   ALLOCATE (BFD(NFRQ,MRXTX,NTX,3)) ; BFD = (0.,0.)

   CALL FDREAD (ND,NFRQ,NTX,MRXTX,NRXTX,NCMP,BFD)

   IF (TDFD == 1) THEN  ! Time-domain fields.

     CALL TD_FIELD (BFD,STEP,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP, &
                    NCHNL,TOPN,TCLS,FREQ,NFRQ,KFRQE,NTX,MRXTX,NRXTX, &
                    RXID,NCMP,NFT,NLINES,MRXL,NRX,LNTR,KNORM2,RX_TYPE, &
                    UNITS,ISYS,IDH,SVAZM,MD1,MD2,RXAZM,RXDIP,CURNT,BPRM, &
                    XMODL,NDATA,MCHNL,KCMP)

   ELSE                 ! Frequency-domain fields.

     CALL FD_FIELD (BFD,NFRQ,NTX,NRXTX,MRXTX,NFT,NLINES,MRXL,NRX,LNTR, &
                    KNORM2,RX_TYPE,UNITS,ISYS,IDH,SVAZM,MD1,MD2,RXAZM, &
                    RXDIP,CURNT,BPRM,XMODL,NDATA,MCHNL,KCMP)

   END IF

   DEALLOCATE (BFD)

   ! Compute the residual error vector and the sum squared scaled symmetric
   ! error; convert to the log of residual errors.

   SUMSQ = 0. ; RES = 0.

   DO JD = 1, NDATA
     VD = XDATA(JD)                        ! Observed data.
     VM = XMODL(JD)                        ! Predicted data.
     RES_ERR = REAL(XWTS(JD)) * (VD - VM)  ! Weighed residual error.
     DENOM = SQRT((VM**2 + VD**2)/2.)
     V1 = RES_ERR / DENOM                  ! Point symmetric RMS error.
     V2 = RES_ERR / DNORM(JD)              ! Survey norm RMS error.
     SUMSQ(1) = SUMSQ(1) + V1**2
     SUMSQ(2) = SUMSQ(2) + V2**2
     IF (INRM == 1) THEN
       RES(JD) = V1
     ELSE
       RES(JD) = V2
     END IF
   END DO

 END IF

 CLOSE (ND)

 ! Sensitivity matrix construction.
 ! --------------------------------

 IF (JCBN) THEN

   ALLOCATE (SFD(NFRQ,MRXTX,NTX,3,NPAR)) ; SFD = (0.,0.)

   CALL SFDREAD (NS,NFRQ,NTX,MRXTX,NRXTX,NCMP,NPAR,SFD)

   IF (TDFD == 1) THEN

     CALL TD_SENS (SFD,STEP,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP, &
                   NCHNL,TOPN,TCLS,FREQ,NFRQ,NTX,MRXTX,NRXTX,RXID,NCMP, &
                   NFT,NLINES,MRXL,NRX,LNTR,KNORM2,RX_TYPE,UNITS,KFRQE, &
                   ISYS,IDH,SVAZM,MD1,MD2,RXAZM,RXDIP,CURNT,BPRM,NDATA, &
                   MCHNL,KCMP,NPAR,JAC)

   ELSE

     CALL FD_SENS (SFD,NFRQ,NTX,MRXTX,NRXTX,MRXL,NFT,NLINES,NRX,LNTR, &
                   KNORM2,RX_TYPE,UNITS,ISYS,IDH,SVAZM,MD1,MD2,RXAZM, &
                   RXDIP,CURNT,BPRM,NDATA,MCHNL,KCMP,NPAR,JAC)

   END IF

   DEALLOCATE (SFD)

   ! Convert to sensitivity wrt resistivity instead of sensitivity wrt
   ! conductivity. Convert to log of the sensitivities wrt resisitivity and
   ! data. Multiply by data weights.

   DO JD = 1, NDATA
     IF (INRM == 1) THEN  ! Normalise by the point symmetric data.
       VD = XDATA(JD)
       VM = XMODL(JD)
       DENOM = SQRT((VM**2 + VD**2)/2.)
     ELSE                 ! Normalise by the survey norm.
       DENOM = DNORM(JD)
     END IF
     DO JP = 1, NPAR
       JAC(JD,JP) = -JAC(JD,JP) * REAL(XWTS(JD)) / (XPAR(JP) * DENOM)
     END DO
   END DO

 END IF

 CLOSE (NS)

 END SUBROUTINE RESJAC

!===========================================================================

 SUBROUTINE SFDREAD (NS,NFRQ,NTX,MRXTX,NRXTX,NCMP,NPAR,SFD)

!---------------------------------------------------------------------------
!
!*** Called by RESJAC
!
!  Reads frequency-domain sensitivities from logical UNIT NS into
!  array SFD.
!
!            NFRQ - number of frequencies
!             NTX - number of transmitter positions
!           MRXTX - maximum number of receivers for any transmitter
!           NRXTX - number of receivers for each transmitter position
!            NCMP - number of components for each receiver
!            NPAR - number of parameters
! BFD1(I,J,K,L,P) - Lth component of the complex frequency-domain response at
!                   receiver J, of transmitter K for frequency I.  If NCMP = 1
!                   as is the case for coincident loop, or electric dipole,
!                   then the relevant component is stored in L=1 position.
!                   P is for the Pth model parameter.
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER NS,NFRQ,NTX,MRXTX,NRXTX(NTX),NCMP(MRXTX,NTX),JF,JS,JR,JC,JP, &
         NC,NC2,NPAR
 REAL A(6)
 COMPLEX SFD(NFRQ,MRXTX,NTX,3,NPAR)

 REWIND (NS)

 DO JF = 1, NFRQ
   DO JS = 1, NTX
     DO JR = 1, NRXTX(JS)
       NC = NCMP(JR,JS)
       NC2 = 2*NC
       DO JP = 1, NPAR
         READ(NS,*) A(1:NC2)
         DO JC = 1, NC
           SFD(JF,JR,JS,JC,JP) = CMPLX (A(2*JC-1),A(2*JC)) * 12.56637E-7
         END DO
       END DO
     END DO
   END DO
 END DO

 END SUBROUTINE SFDREAD

!===========================================================================

 SUBROUTINE TD_FIELD (BFD,STEP,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP, &
                      NCHNL,TOPN,TCLS,FREQ,NFRQ,KFRQE,NTX,MRXTX,NRXTX, &
                      RXID,NCMP,NFT,NLINES,MRXL,NRX,LNTR,KNORM2,RX_TYPE, &
                      UNITS,ISYS,IDH,SVAZM,MD1,MD2,RXAZM,RXDIP,CURNT,BPRM, &
                      XMODL,NDATA,MCHNL,KCMP)

!---------------------------------------------------------------------------
!
!*** Called by: RESJAC
!*** Calls: TDEM_3D, HBOSS_TD, SET_OUTPUT_LINES_TD
!
! Compute the time-domain response for the model.
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER STEP,NSX,NPULS,NTYPLS,NTYRP,NCHNL,NFRQ,KFRQE,NTX,MRXTX, &
         NLINES,MRXL,MD1,MD2,LNTR(4,NLINES),KNORM2(MRXTX,NTX), &
         NFT,NDATA,MCHNL,KCMP,JS,JL,JC,JD,JR,ISYS
 REAL PULSE,FREQ(NFRQ),SWX(NSX),SWY(NSX,3),TRP(NTYRP),CURNT(NFT), &
      BPRM(MRXTX,NTX),SVAZM(NLINES),XMODL(NDATA),YTR(MCHNL)
 INTEGER,DIMENSION(NLINES) :: NRX,UNITS,RX_TYPE,IDH
 INTEGER,DIMENSION(NTX) :: NRXTX
 INTEGER,DIMENSION(MRXTX,NTX) :: NCMP,RXID
 REAL,DIMENSION(NCHNL) :: TOPN,TCLS
 REAL,DIMENSION(MD1,MD2) :: RXAZM,RXDIP
 REAL,ALLOCATABLE,DIMENSION(:,:,:,:) :: BTD,BTDL
 COMPLEX BFD(NFRQ,MRXTX,NTX,3)

 ! Compute BTD, the scattered response convolved with the excitation waveform.

 ALLOCATE (BTD(NCHNL,MRXTX,NTX,3)) ; BTD = 0.

 CALL TDEM_3D1 (STEP,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL,TOPN,TCLS, &
               FREQ,NFRQ,KFRQE,NTX,MRXTX,NRXTX,RXID,NCMP,BFD,BTD)


 ALLOCATE (BTDL(NCHNL,MRXL,NLINES,3)) ; BTDL = 0.

 CALL SET_OUTPUT_LINES_TD1 (NCHNL,NTX,MRXTX,NRXTX,NLINES,MRXL,NRX,LNTR,KNORM2, &
                            RX_TYPE,UNITS,ISYS,IDH,SVAZM,MD1,MD2,RXAZM,RXDIP, &
                           CURNT,BPRM,BTD,BTDL)
! CALL SET_OUTPUT_LINES_TD (NCHNL,NTX,MRXTX,NRXTX,NLINES,MRXL,MCMP,NRX,LNTR,KNORM2,RX_TYPE, &
!                           UNITS,ISYS,IDH,SVAZM,MD1,MD2,RXAZM,RXDIP,CURNT,BPRM,BTD,BFTL)

 DEALLOCATE (BTD)

 JS = 0
 DO JL = 1, NLINES
   IF (RX_TYPE(JL) == 1) THEN            ! 3 component magnetic dipole output
     DO JR = 1, NRX(JL)
       JS = JS + 1
       IF (KCMP == 1) THEN
         YTR(        1:  NCHNL) = BTDL(1:NCHNL,JR,JL,1)
       ELSE IF (KCMP == 2) THEN
         YTR(        1:  NCHNL) = BTDL(1:NCHNL,JR,JL,2)
       ELSE IF (KCMP == 3) THEN
         YTR(        1:  NCHNL) = BTDL(1:NCHNL,JR,JL,3)
       ELSE IF (KCMP == 13 .OR. KCMP == 31) THEN
         YTR(        1:  NCHNL) = BTDL(1:NCHNL,JR,JL,3)
         YTR(  NCHNL+1:2*NCHNL) = BTDL(1:NCHNL,JR,JL,1)
       ELSE IF (KCMP == 23 .OR. KCMP == 32) THEN
         YTR(        1:  NCHNL) = BTDL(1:NCHNL,JR,JL,3)
         YTR(  NCHNL+1:2*NCHNL) = BTDL(1:NCHNL,JR,JL,2)
       ELSE IF (KCMP == 123 .OR. KCMP == 312) THEN
         YTR(        1:  NCHNL) = BTDL(1:NCHNL,JR,JL,3)
         YTR(  NCHNL+1:2*NCHNL) = BTDL(1:NCHNL,JR,JL,1)
         YTR(2*NCHNL+1:3*NCHNL) = BTDL(1:NCHNL,JR,JL,2)
       END IF
       DO JC = 1, MCHNL
         JD = JC + (JS-1)*MCHNL
         XMODL(JD) = YTR(JC)
       END DO
     END DO
   ELSE                                  ! Coincident loop or electric dipole output
     DO JR = 1, NRX(JL)
       JS = JS + 1
       DO JC = 1, NCHNL
         JD = JC + (JS-1)*NCHNL
         XMODL(JD) = BTDL(JC,JR,JL,1)
       END DO
     END DO
   END IF
 END DO

 DEALLOCATE (BTDL)

 END SUBROUTINE TD_FIELD

!===========================================================================

 SUBROUTINE TD_SENS (SFD,STEP,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP, &
                     NCHNL,TOPN,TCLS,FREQ,NFRQ,NTX,MRXTX,NRXTX,RXID,NCMP, &
                     NFT,NLINES,MRXL,NRX,LNTR,KNORM2,RX_TYPE,UNITS,KFRQE, &
                     ISYS,IDH,SVAZM,MD1,MD2,RXAZM,RXDIP,CURNT,BPRM,NDATA, &
                     MCHNL,KCMP,NPAR,JAC)

!---------------------------------------------------------------------------
!
!*** Called by: RESJAC
!*** Calls: SENS_FD2TD, SET_OUTPUT_LINES_STD
!
! Compute the time-domain sensitivities for the model.
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER STEP,NSX,NPULS,NTYPLS,NTYRP,NCHNL,NFRQ,KFRQE,NTX,MRXTX,NLINES, &
         MRXL,MD1,MD2,LNTR(4,NLINES),KNORM2(MRXTX,NTX),NFT,NDATA,MCHNL, &
         KCMP,JS,JL,JC,JD,JR,ISYS,NPAR,JP
 REAL PULSE,FREQ(NFRQ),SWX(NSX),SWY(NSX,3),TRP(NTYRP),CURNT(NFT), &
      BPRM(MRXTX,NTX),SVAZM(NLINES),YTR(MCHNL,NPAR)
 INTEGER,DIMENSION(NLINES) :: NRX,UNITS,RX_TYPE,IDH
 INTEGER,DIMENSION(NTX) :: NRXTX
 INTEGER,DIMENSION(MRXTX,NTX) :: NCMP,RXID
 REAL,DIMENSION(NCHNL) :: TOPN,TCLS
 REAL,DIMENSION(NDATA,NPAR) :: JAC
 REAL,DIMENSION(MD1,MD2) :: RXAZM,RXDIP
 REAL,ALLOCATABLE,DIMENSION(:,:,:,:,:) :: STD,STDL
 COMPLEX SFD(NFRQ,MRXTX,NTX,3,NPAR)

 ALLOCATE (STD(NCHNL,MRXTX,NTX,3,NPAR)) ; STD = 0.

 ! Compute BTD_SCAT, the scattered response convolved with the excitation waveform.

 CALL SENS_FD2TD (STEP,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL,TOPN,TCLS, &
                  FREQ,NFRQ,KFRQE,NTX,MRXTX,NRXTX,RXID,NCMP,SFD,STD,NPAR)

 ! Reconfigure output from Tx to Line basis.

 ALLOCATE (STDL(NCHNL,MRXL,NLINES,3,NPAR)) ; STDL = 0.

 CALL SET_OUTPUT_LINES_STD (NCHNL,NTX,MRXTX,NRXTX,NLINES,MRXL,NRX,LNTR,KNORM2, &
                            RX_TYPE,UNITS,ISYS,IDH,SVAZM,MD1,MD2,RXAZM,RXDIP, &
                            CURNT,BPRM,STD,STDL,NPAR)

 DEALLOCATE (STD)

 JS = 0
 DO JL = 1, NLINES
   IF (RX_TYPE(JL) == 1) THEN            ! 3 component magnetic dipole output
     DO JR = 1, NRX(JL)
       JS = JS + 1
       IF (KCMP == 1) THEN
         DO JP = 1, NPAR
           YTR(        1:  NCHNL,JP) = STDL(1:NCHNL,JR,JL,1,JP)
         END DO
       ELSE IF (KCMP == 2) THEN
         DO JP = 1, NPAR
           YTR(        1:  NCHNL,JP) = STDL(1:NCHNL,JR,JL,2,JP)
         END DO
       ELSE IF (KCMP == 3) THEN
         DO JP = 1, NPAR
           YTR(        1:  NCHNL,JP) = STDL(1:NCHNL,JR,JL,3,JP)
         END DO
       ELSE IF (KCMP == 13 .OR. KCMP == 31) THEN
         DO JP = 1, NPAR
           YTR(        1:  NCHNL,JP) = STDL(1:NCHNL,JR,JL,3,JP)
           YTR(  NCHNL+1:2*NCHNL,JP) = STDL(1:NCHNL,JR,JL,1,JP)
         END DO
       ELSE IF (KCMP == 23 .OR. KCMP == 32) THEN
         DO JP = 1, NPAR
           YTR(        1:  NCHNL,JP) = STDL(1:NCHNL,JR,JL,3,JP)
           YTR(  NCHNL+1:2*NCHNL,JP) = STDL(1:NCHNL,JR,JL,2,JP)
         END DO
       ELSE IF (KCMP == 123 .OR. KCMP == 312) THEN
         DO JP = 1, NPAR
           YTR(        1:  NCHNL,JP) = STDL(1:NCHNL,JR,JL,3,JP)
           YTR(  NCHNL+1:2*NCHNL,JP) = STDL(1:NCHNL,JR,JL,1,JP)
           YTR(2*NCHNL+1:3*NCHNL,JP) = STDL(1:NCHNL,JR,JL,2,JP)
         END DO
       END IF
       DO JC = 1, MCHNL
         JD = JC + (JS-1)*MCHNL
         DO JP = 1, NPAR
           JAC(JD,JP) = YTR(JC,JP)
         END DO
       END DO
     END DO
   ELSE                                  ! Coincident loop or electric dipole output
     DO JR = 1, NRX(JL)
       JS = JS + 1
       DO JC = 1, NCHNL
         JD = JC + (JS-1)*NCHNL
         DO JP = 1, NPAR
           JAC(JD,JP) = STDL(JC,JR,JL,1,JP)
         END DO
       END DO
     END DO
   END IF
 END DO

 DEALLOCATE (STDL)

 END SUBROUTINE TD_SENS

!===========================================================================

 SUBROUTINE SENS_FD2TD (STEP,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL, &
                        TOPN,TCLS,FREQ,NFRQ,KFRQE,NTX,MRXTX,NRXTX,RXID,NCMP, &
                        SFD1,STD1,NPAR)

!---------------------------------------------------------------------------
!
!***  Called by: TD_SENS
!***  Calls CUBSPL, COSTRN, FOLD_CONVOLVE
!
!  Computes STD1, the time domain sensitivity for the input frequency-domain
!  data.  It computes the senstivity of the voltage (volts) or dB/dt
!  (teslas/sec) if STEP = 0) or magnetic field B (in teslas) if STEP = 1)
!  by convolving the step response of the earth with the negative
!  time-derivative of the current waveform. For magnetic sensitivities, this
!  is averaged across the receiver window.  For db/dt, or voltage, this is
!  differenced across the  receiver window.  The negative dI/dt is used so
!  that current switch off corresponds to positive response.
!
!  On entry, the imaginary component of the frequency-domain sensitivity
!  in array SFD1 is divided by frequency and then cosine transformed into
!  time-domain step response sensitivity out to NPULS bipolar cycles.  For
!  each receiver position for for each transmitter, FOLD_AND_CONVOLVE is
!  called to fold the positive and negative parts of the bipolar current
!  pulse into a half-cycle (length PULSE) decay curve.  This result is
!  convolved with the dI/dt waveform.
!
!        STEP = 1 for normal step response is to be computed
!             = 4 for UTEM pure step response
!         NSX - number of points used to discretise transmitter signal
!         SWX - abscissae (seconds) of current waveform
!         SWY - dI/dt at times SWX
!       NPULS - number of bipolar pulses of length PULSE
!       PULSE - length single on pulse plus off-time
!       NTYRP - number of values in TRP for total signal length: 2 * NPULS *PULSE
!         TRP - array of time values for FD -> TD transformations
!      NTYPLS - number of TRP values in 1 PULSE
!       NCHNL - number of channels
!        TOPN - time at which receiver channel I opens.
!        TCLS - time at which receiver channel I closes.
!        FREQ - array of NFRQ frequencies.
!         NTX - number of transmitter positions.
!       MRXTX - maximum number of receiver positions for any transmitter position
!    NRXTX(J) = number of receivers FOR transmitter J
!   RXID(I,J) = RX_TYPE of receiver I for transmitter J
!   NCMP(I,J) = number of components for receiver I for transmitter J
!             = number of components, = 3 or 1 usually
! SFD1(I,J,K,L,P) - Lth component of the complex frequency-domain response at
!                   receiver J, of transmitter K for frequency I.  If NCMP = 1
!                   as is the case for coincident loop, or electric dipole,
!                   then the relevant component is stored in L=1 position, and
!                   Pth model parameter.
!
!---------------------------------------------------------------------------


 IMPLICIT NONE
 REAL, PARAMETER :: TWOPI=6.283185
 INTEGER STEP,NSX,NPULS,NTYPLS,NTYRP,NCHNL,NFRQ,KFRQE,KFRQ,NTX,MRXTX,NPAR, &
         NRXTX(NTX),NCMP(MRXTX,NTX),STEPC,RXID(MRXTX,NTX),JR,JS,JF,JC,JT,JP
 REAL, DIMENSION(:,:), ALLOCATABLE ::  YSCAT,YFRQ
 REAL PULSE,FREQ(NFRQ),WF(NFRQ),SWX(NSX),SWY(NSX,3), &
      COSTRN,T,YCUM(NCHNL),TOPN(NCHNL),TCLS(NCHNL),TRP(NTYRP), &
      STD1(NCHNL,MRXTX,NTX,3,NPAR)
 COMPLEX SFD1(NFRQ,MRXTX,NTX,3,NPAR)

 STD1 = 0.

 ALLOCATE (YSCAT(4,NTYRP),YFRQ(4,NFRQ)) ; YSCAT = 0. ; YFRQ = 0.

 ! The - sign below is a consequence of using the sine transform for a
 ! +iwt sign convention

 WF(1:NFRQ) = LOG (TWOPI * FREQ(1:NFRQ))

 ! For each component at each receiver, compute the time-domain step response
 ! by splining the imaginary part of the frequency-domain response, converting
 ! it to time-domain step function response and folding the NPULS bipolar decay
 ! curve into a combined pulse decay curve of length PULSE.  Convolve this with
 ! the TX waveform to produce STD1, the 'observable" stripped response for the
 ! system.

 DO JS = 1, NTX
   DO JR = 1, NRXTX(JS)
     STEPC = STEP
     KFRQ = NFRQ
     IF (RXID(JR,JS) == 2) THEN
       STEPC = 3                      ! Electrode E field Transform INTEGRAL { E dl } to TD
       KFRQ = KFRQE
     END IF
     DO JC = 1, NCMP(JR,JS)
       DO JP = 1, NPAR
         DO JF = 1, KFRQ     ! Divide by -iw to set up step response
           IF (STEPC == 3) THEN
             YFRQ(1,JF) =   REAL(SFD1(JF,JR,JS,JC,JP))
           ELSE
             YFRQ(1,JF) = -AIMAG(SFD1(JF,JR,JS,JC,JP)) / (TWOPI * FREQ(JF))
           END IF
         END DO
         CALL CUBSPL (WF,YFRQ,KFRQ)
         YSCAT = 0.
         DO JT = 1, NTYRP   !  Convert to step-function time-domain.
           T = TRP(JT)
           YSCAT(1,JT) = COSTRN (WF,YFRQ,KFRQ,T)
         END DO
         CALL FOLD_AND_CONVOLVE (STEPC,NSX,SWX,SWY,NPULS,PULSE,TRP,NTYRP,NTYPLS, &
                                 NCHNL,TOPN,TCLS,YSCAT,YCUM)
         STD1(1:NCHNL,JR,JS,JC,JP) = YCUM(1:NCHNL)
       END DO
     END DO
   END DO
 END DO

 DEALLOCATE (YSCAT, YFRQ)

 END SUBROUTINE SENS_FD2TD

!===========================================================================

 SUBROUTINE SET_OUTPUT_LINES_STD (NCHNL,NTX,MRXTX,NRXTX,NLINES,MRXL,NRX, &
                                  LNTR,KNORM2,RX_TYPE,UNITS,ISYS,IDH,SVAZM, &
                                  MD1,MD2,RXAZM,RXDIP,CURNT,BPRM,STD,STDL, &
                                  NPAR)

!---------------------------------------------------------------------------
!
!*** Called by: RESJAC
!
!  Change time-domain sensitivity from Tx to Line basis.
!
!---------------------------------------------------------------------------

 INTEGER NCHNL,NTX,MRXTX,NRXTX(NTX),NLINES,MRXL,MD1,MD2,LNTR(4,NLINES), &
         KNORM2(MRXTX,NTX),ISYS,JS,JR,JL,JRL,JC,JT,JP,NPAR
 INTEGER, DIMENSION(NLINES) :: NRX,UNITS,RX_TYPE,IDH
 REAL, DIMENSION(NLINES) :: SVAZM
 REAL, DIMENSION(MD1,MD2) :: RXAZM,RXDIP
 REAL CURNT(1),BPRM(MRXTX,NTX),STD(NCHNL,MRXTX,NTX,3,NPAR), &
      STDL(NCHNL,MRXL,NLINES,3,NPAR),A1,Q1(1:3),QX,QY,QZ,CAZ,SAZ,CAZ0, &
      SAZ0,SDP,CDP,PHI,QT(NCHNL),ALF(3,3),RHO

 ALF = 0.
 DO JS = 1, NTX
   DO JR = 1, NRXTX(JS)
     IF (KNORM2(JR,JS) == 0) CYCLE
     DO JC = 1,3
       IF (KNORM2(JR,JS) == 1) THEN  !  Total field normalisation
         DO JP = 1, NPAR
           STD(1:NCHNL,JR,JS,JC,JP) = STD(1:NCHNL,JR,JS,JC,JP) / (BPRM(JR,JS) * CURNT(1))
         END DO
       ELSE IF (KNORM2(JR,JS) == 2) THEN   ! Component normalisation
!         A1 = ABS (BPRM(JC,JR,JS))
!         A2 = 1.E-6 * BPRM(4,JR,JS)
!         IF (A1 < A2) A1 = BPRM(4,JR,JS)
         A1 = BPRM(JR,JS)
         DO JP = 1, NPAR
           STD(1:NCHNL,JR,JS,JC,JP) = STD(1:NCHNL,JR,JS,JC,JP) / (A1 * CURNT(1))
         END DO
       END IF
     END DO
   END DO
 END DO

 ! Convert to LINE-based output and apply units.

 DO JL = 1, NLINES
   A1 = 1.
   SELECT CASE (UNITS(JL))
   CASE (32)
     A1 = 100.
   CASE (2,33)
     A1 = 1000.
   CASE (3,34)
     A1 = 1.E6
   CASE (4,11,21,35)
     A1 = 1.E9
   CASE (12,22)
     A1 = 1.E12
   END SELECT
   JRL = 0
   DO JS = LNTR(1,JL), LNTR(2,JL)
     DO JR = LNTR(3,JL), LNTR(4,JL)
       JRL = JRL + 1
       DO JC = 1, 3
         DO JP = 1, NPAR
           STDL(1:NCHNL,JRL,JL,JC,JP) = A1 * STD(1:NCHNL,JR,JS,JC,JP)
         END DO
       END DO
     END DO
   END DO

   ! For surface dipole surveys only, reorient component 1 from north to lie along
   ! the Survey X axis and component 2 from east to lie along the Survey Y axis.

   IF (RX_TYPE(JL) > 1) CYCLE  ! Skip if not magnetic dipole receiver

   CAZ0 = COS (SVAZM (JL))
   SAZ0 = SIN (SVAZM (JL))
   DO JR = 1,NRX(JL)
     IF (IDH(JL) == 0) THEN    !  Surface survey
       DO JT = 1,NCHNL
         DO JP = 1, NPAR
           Q1(1:2) = STDL(JT,JR,JL,1:2,JP)
           STDL(JT,JR,JL,1,JP) =  CAZ0 * Q1(1) + SAZ0 * Q1(2)
           STDL(JT,JR,JL,2,JP) = -SAZ0 * Q1(1) + CAZ0 * Q1(2)
         END DO
       END DO
     ELSE
       CAZ = COS (RXAZM(JR,JL))
       SAZ = SIN (RXAZM(JR,JL))
       CDP = COS (RXDIP(JR,JL))
       SDP = SIN (RXDIP(JR,JL))
       DO JT = 1,NCHNL
         DO JP = 1, NPAR
           Q1(1:3) = STDL(JT,JR,JL,1:3,JP)
           QZ = Q1(3)
           IF (IDH(JL) == 1) THEN                           ! Express BTDL in U,V,A
             QX =  CAZ * Q1(1) + SAZ * Q1(2)                 ! local horizontal radial component
             QY = -SAZ * Q1(1) + CAZ * Q1(2)                 ! local horizontal transverse component
             STDL(JT,JR,JL,3,JP) =  CDP * QZ + SDP * QX         ! Axial component
             STDL(JT,JR,JL,1,JP) = -SDP * QZ + CDP * QX          !   local U component
             STDL(JT,JR,JL,2,JP) = QY                            !   local V component
           ELSE IF (IDH(JL) == 2) THEN                 ! Express BTDL in S, N, W
             QX =  CAZ0 * Q1(1) + SAZ0 * Q1(2)         !   X radial component in survey system
             QY = -SAZ0 * Q1(1) + CAZ0 * Q1(2)         !   Y transverse component in survey system
             PHI = RXAZM(JR,JL) - SVAZM(JL)            !   local - survey aximuth
             ALF(3,1) = SDP * COS (PHI)               ! direction cosines for W (axial) component
             ALF(3,2) = SDP * SIN (PHI)
             ALF(3,3) = CDP
             RHO = SQRT (ALF(3,2)**2 + ALF(3,3)**2)
             ALF(2,1) = 0.                             ! direction cosines for N (out-of-section component)
             ALF(2,2) =  ALF(3,3) / RHO
             ALF(2,3) = -ALF(3,2) /RHO
             ALF(1,1) = ALF(2,2) * ALF(3,3) - ALF(2,3) * ALF(3,2)   ! direction cosines for S (in-section component)
             ALF(1,2) = ALF(2,3) * ALF(3,1)
             ALF(1,3) = -ALF(2,2) * ALF(3,1)
             STDL(JT,JR,JL,1,JP) = ALF(1,1) * QX + ALF(1,2) * QY + ALF(1,3) * QZ  ! S
             STDL(JT,JR,JL,2,JP) =                 ALF(2,2) * QY + ALF(2,3) * QZ  ! N
             STDL(JT,JR,JL,3,JP) = ALF(3,1) * QX + ALF(3,2) * QY + ALF(3,3) * QZ  ! W
           END IF
         END DO
       END DO
     END IF
     IF (ISYS == 4) THEN        !  reverse channels for UTEM & subtract channel 1 response
       DO JC = 1,3
         DO JP = 1, NPAR
           DO JT = 1,NCHNL
             JRL = NCHNL - JT + 1
             QT(JT) = STDL(JT,JR,JL,JC,JP)
           END DO
           DO JT = 1,NCHNL
             STDL(JT,JR,JL,JC,JP) = QT(JT)
             IF (JT > 1) STDL(JT,JR,JL,JC,JP) = STDL(JT,JR,JL,JC,JP) - STDL(1,JR,JL,JC,JP)
           END DO
         END DO
       END DO
     END IF
   END DO
 END DO

 END SUBROUTINE SET_OUTPUT_LINES_STD

!===========================================================================

 SUBROUTINE FD_FIELD (BFD,NFRQ,NTX,NRXTX,MRXTX,NFT,NLINES,MRXL,NRX,LNTR, &
                      KNORM2,RX_TYPE,UNITS,ISYS,IDH,SVAZM,MD1,MD2,RXAZM, &
                      RXDIP,CURNT,BPRM,XMODL,NDATA,MCHNL,KCMP)

!---------------------------------------------------------------------------
!
!*** Called by: RESJAC
!*** Calls: HSBOSS, SET_OUTPUT_LINES_FD
!
! Compute the freqeucny-domain response for the model.
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER NFRQ,NTX,MRXTX,NLINES,MRXL,MD1,MD2,LNTR(4,NLINES),KNORM2(MRXTX,NTX), &
         NFT,KCMP,NDATA,MCHNL,JL,JS,JR,JC,JD,ISYS
 REAL CURNT(NFT),BPRM(MRXTX,NTX),SVAZM(NLINES),XMODL(NDATA),YTR(MCHNL)
 INTEGER,DIMENSION(NLINES) :: NRX,UNITS,RX_TYPE,IDH
 INTEGER,DIMENSION(NTX) :: NRXTX
 REAL,DIMENSION(MD1,MD2) :: RXAZM,RXDIP
 COMPLEX BFD(NFRQ,MRXTX,NTX,3)
 COMPLEX,ALLOCATABLE,DIMENSION(:,:,:,:) :: BFDL

 ALLOCATE (BFDL(NFRQ,MRXL,NLINES,3)) ; BFDL = (0.,0.)

 CALL SET_OUTPUT_LINES_FD1 (NFRQ,NTX,MRXTX,NRXTX,NLINES,MRXL,NRX,LNTR,KNORM2, &
                            RX_TYPE,UNITS,ISYS,IDH,SVAZM,MD1,MD2,RXAZM,RXDIP, &
                            CURNT,BPRM,BFD,BFDL)

 JS = 0
 DO JL = 1, NLINES
   IF (RX_TYPE(JL) == 1) THEN            ! 3 component magnetic dipole output
     DO JR = 1, NRX(JL)
       JS = JS + 1
       IF (KCMP == 1) THEN
         YTR(       1:  NFRQ) =  REAL(BFDL(1:NFRQ,JR,JL,1))
         YTR(  NFRQ+1:2*NFRQ) = AIMAG(BFDL(1:NFRQ,JR,JL,1))
       ELSE IF (KCMP == 2) THEN
         YTR(       1:  NFRQ) =  REAL(BFDL(1:NFRQ,JR,JL,2))
         YTR(  NFRQ+1:2*NFRQ) = AIMAG(BFDL(1:NFRQ,JR,JL,2))
       ELSE IF (KCMP == 3) THEN
         YTR(       1:  NFRQ) =  REAL(BFDL(1:NFRQ,JR,JL,3))
         YTR(  NFRQ+1:2*NFRQ) = AIMAG(BFDL(1:NFRQ,JR,JL,3))
       ELSE IF (KCMP == 13 .OR. KCMP == 31) THEN
         YTR(       1:  NFRQ) =  REAL(BFDL(1:NFRQ,JR,JL,3))
         YTR(  NFRQ+1:2*NFRQ) =  REAL(BFDL(1:NFRQ,JR,JL,1))
         YTR(2*NFRQ+1:3*NFRQ) = AIMAG(BFDL(1:NFRQ,JR,JL,3))
         YTR(3*NFRQ+1:4*NFRQ) = AIMAG(BFDL(1:NFRQ,JR,JL,1))
       ELSE IF (KCMP == 23 .OR. KCMP == 32) THEN
         YTR(       1:  NFRQ) =  REAL(BFDL(1:NFRQ,JR,JL,3))
         YTR(  NFRQ+1:2*NFRQ) =  REAL(BFDL(1:NFRQ,JR,JL,2))
         YTR(2*NFRQ+1:3*NFRQ) = AIMAG(BFDL(1:NFRQ,JR,JL,3))
         YTR(3*NFRQ+1:4*NFRQ) = AIMAG(BFDL(1:NFRQ,JR,JL,2))
       ELSE IF (KCMP == 123 .OR. KCMP == 312) THEN
         YTR(       1:  NFRQ) =  REAL(BFDL(1:NFRQ,JR,JL,3))
         YTR(  NFRQ+1:2*NFRQ) =  REAL(BFDL(1:NFRQ,JR,JL,1))
         YTR(2*NFRQ+1:3*NFRQ) =  REAL(BFDL(1:NFRQ,JR,JL,2))
         YTR(3*NFRQ+1:4*NFRQ) = AIMAG(BFDL(1:NFRQ,JR,JL,3))
         YTR(4*NFRQ+1:5*NFRQ) = AIMAG(BFDL(1:NFRQ,JR,JL,1))
         YTR(5*NFRQ+1:6*NFRQ) = AIMAG(BFDL(1:NFRQ,JR,JL,2))
       END IF
       DO JC = 1, MCHNL
         JD = JC + (JS-1)*MCHNL
         XMODL(JD) = YTR(JC)
       END DO
     END DO
   ELSE                                  ! Coincident loop, Sampo or electric dipole output
     DO JR = 1, NRX(JL)
       JS = JS + 1
       YTR(     1:  NFRQ) =  REAL(BFDL(1:NFRQ,JR,JL,1))
       YTR(NFRQ+1:2*NFRQ) = AIMAG(BFDL(1:NFRQ,JR,JL,1))
       DO JC = 1, MCHNL
         JD = JC + (JS-1)*MCHNL
         XMODL(JD) = YTR(JC)
       END DO
     END DO
   END IF
 END DO

 DEALLOCATE (BFDL)

 END SUBROUTINE FD_FIELD

!===========================================================================

 SUBROUTINE FD_SENS (SFD,NFRQ,NTX,MRXTX,NRXTX,MRXL,NFT,NLINES,NRX,LNTR, &
                     KNORM2,RX_TYPE,UNITS,ISYS,IDH,SVAZM,MD1,MD2,RXAZM, &
                     RXDIP,CURNT,BPRM,NDATA,MCHNL,KCMP,NPAR,JAC)

!---------------------------------------------------------------------------
!
!*** Called by: RESJAC
!*** Calls: SET_OUTPUT_LINES_SFD
!
! Compute the time-domain sensitivities for the model.
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER NFRQ,NTX,MRXTX,NLINES,MRXL,MD1,MD2,LNTR(4,NLINES), &
         KNORM2(MRXTX,NTX),NFT,KCMP,NDATA,NPAR,MCHNL,JL,JS,JR,JC,JD, &
         JP,ISYS
 REAL CURNT(NFT),BPRM(MRXTX,NTX),SVAZM(NLINES),YTR(MCHNL,NPAR)
 INTEGER,DIMENSION(NLINES) :: NRX,UNITS,RX_TYPE,IDH
 INTEGER,DIMENSION(NTX) :: NRXTX
 REAL,DIMENSION(NDATA,NPAR) :: JAC
 REAL,DIMENSION(MD1,MD2) :: RXAZM,RXDIP
 COMPLEX SFD(NFRQ,MRXTX,NTX,3,NPAR)
 COMPLEX,ALLOCATABLE,DIMENSION(:,:,:,:,:) :: SFDL

 ! Reconfigure output from Tx to Line basis.

 ALLOCATE (SFDL(NFRQ,MRXL,NLINES,3,NPAR)) ; SFDL = (0.,0.)

 CALL SET_OUTPUT_LINES_SFD (NFRQ,NTX,MRXTX,NRXTX,NLINES,MRXL,NRX,LNTR,KNORM2, &
                            RX_TYPE,UNITS,ISYS,IDH,SVAZM,MD1,MD2,RXAZM,RXDIP, &
                            CURNT,BPRM,SFD,SFDL,NPAR)

 ! Construct Jacobian matrix.

 JS = 0
 DO JL = 1, NLINES
   IF (RX_TYPE(JL) == 1) THEN            ! 3 component magnetic dipole output
     DO JR = 1, NRX(JL)
       JS = JS + 1
       IF (KCMP == 1) THEN
         DO JP = 1, NPAR
           YTR(       1:  NFRQ,JP) =  REAL(SFDL(1:NFRQ,JR,JL,1,JP))
           YTR(  NFRQ+1:2*NFRQ,JP) = AIMAG(SFDL(1:NFRQ,JR,JL,1,JP))
         END DO
       ELSE IF (KCMP == 2) THEN
         DO JP = 1, NPAR
           YTR(       1:  NFRQ,JP) =  REAL(SFDL(1:NFRQ,JR,JL,2,JP))
           YTR(  NFRQ+1:2*NFRQ,JP) = AIMAG(SFDL(1:NFRQ,JR,JL,2,JP))
         END DO
       ELSE IF (KCMP == 3) THEN
         DO JP = 1, NPAR
           YTR(       1:  NFRQ,JP) =  REAL(SFDL(1:NFRQ,JR,JL,3,JP))
           YTR(  NFRQ+1:2*NFRQ,JP) = AIMAG(SFDL(1:NFRQ,JR,JL,3,JP))
         END DO
       ELSE IF (KCMP == 13 .OR. KCMP == 31) THEN
         DO JP = 1, NPAR
           YTR(       1:  NFRQ,JP) =  REAL(SFDL(1:NFRQ,JR,JL,3,JP))
           YTR(  NFRQ+1:2*NFRQ,JP) =  REAL(SFDL(1:NFRQ,JR,JL,1,JP))
           YTR(2*NFRQ+1:3*NFRQ,JP) = AIMAG(SFDL(1:NFRQ,JR,JL,3,JP))
           YTR(3*NFRQ+1:4*NFRQ,JP) = AIMAG(SFDL(1:NFRQ,JR,JL,1,JP))
         END DO
       ELSE IF (KCMP == 23 .OR. KCMP == 32) THEN
         DO JP = 1, NPAR
           YTR(       1:  NFRQ,JP) =  REAL(SFDL(1:NFRQ,JR,JL,3,JP))
           YTR(  NFRQ+1:2*NFRQ,JP) =  REAL(SFDL(1:NFRQ,JR,JL,2,JP))
           YTR(2*NFRQ+1:3*NFRQ,JP) = AIMAG(SFDL(1:NFRQ,JR,JL,3,JP))
           YTR(3*NFRQ+1:4*NFRQ,JP) = AIMAG(SFDL(1:NFRQ,JR,JL,2,JP))
         END DO
       ELSE IF (KCMP == 123 .OR. KCMP == 312) THEN
         DO JP = 1, NPAR
           YTR(       1:  NFRQ,JP) =  REAL(SFDL(1:NFRQ,JR,JL,3,JP))
           YTR(  NFRQ+1:2*NFRQ,JP) =  REAL(SFDL(1:NFRQ,JR,JL,1,JP))
           YTR(2*NFRQ+1:3*NFRQ,JP) =  REAL(SFDL(1:NFRQ,JR,JL,2,JP))
           YTR(3*NFRQ+1:4*NFRQ,JP) = AIMAG(SFDL(1:NFRQ,JR,JL,3,JP))
           YTR(4*NFRQ+1:5*NFRQ,JP) = AIMAG(SFDL(1:NFRQ,JR,JL,1,JP))
           YTR(5*NFRQ+1:6*NFRQ,JP) = AIMAG(SFDL(1:NFRQ,JR,JL,2,JP))
         END DO
       END IF
       DO JC = 1, MCHNL
         JD = JC + (JS-1)*MCHNL
         DO JP = 1, NPAR
           JAC(JD,JP) = YTR(JC,JP)
         END DO
       END DO
     END DO
   ELSE                                  ! Coincident loop, Sampo or electric dipole output
     DO JR = 1, NRX(JL)
       JS = JS + 1
       DO JP = 1, NPAR
         YTR(     1:  NFRQ,JP) =  REAL(SFDL(1:NFRQ,JR,JL,1,JP))
         YTR(NFRQ+1:2*NFRQ,JP) = AIMAG(SFDL(1:NFRQ,JR,JL,1,JP))
       END DO
       DO JC = 1, MCHNL
         JD = JC + (JS-1)*MCHNL
         DO JP = 1, NPAR
           JAC(JD,JP) = YTR(JC,JP)
         END DO
       END DO
     END DO
   END IF
 END DO

 DEALLOCATE (SFDL)

 END SUBROUTINE FD_SENS

!===========================================================================

 SUBROUTINE SET_OUTPUT_LINES_SFD (NFRQ,NTX,MRXTX,NRXTX,NLINES,MRXL,NRX, &
                                  LNTR,KNORM2,RX_TYPE,UNITS,ISYS,IDH,SVAZM, &
                                  MD1,MD2,RXAZM,RXDIP,CURNT,BPRM,SFD,SFDL, &
                                  NPAR)

!---------------------------------------------------------------------------
!
!*** Called by: RESJAC
!
!  Change frequency-domain sensitivity from Tx to Line basis.
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER NFRQ,NTX,MRXTX,NRXTX(NTX),NLINES,MRXL,MD1,MD2,LNTR(4,NLINES), &
         KNORM2(MRXTX,NTX),ISYS,JS,JR,JL,JRL,JC,JF,JP,NPAR
 INTEGER, DIMENSION(NLINES) :: NRX,UNITS,RX_TYPE,IDH
 REAL, DIMENSION(NLINES) :: SVAZM
 REAL, DIMENSION(MD1,MD2) :: RXAZM,RXDIP
 REAL CURNT(NFRQ),BPRM(MRXTX,NTX),A1,A2,CAZ,SAZ,CAZ0,SAZ0,SDP,CDP
 COMPLEX SFD(NFRQ,MRXTX,NTX,3,NPAR),SFDL(NFRQ,MRXL,NLINES,3,NPAR), &
         Q1(1:3),QX,QY

 DO JS = 1, NTX
   DO JR = 1, NRXTX(JS)
     DO JC = 1,3
       IF (KNORM2(JR,JS) == 0) THEN
         DO JP = 1, NPAR
           SFD(1:NFRQ,JR,JS,JC,JP) = SFD(1:NFRQ,JR,JS,JC,JP) * CURNT(1:NFRQ)
         END DO
       ELSE IF (KNORM2(JR,JS) == 1) THEN  !  Total field normalisation
         DO JP = 1, NPAR
           SFD(1:NFRQ,JR,JS,JC,JP) = SFD(1:NFRQ,JR,JS,JC,JP) / BPRM(JR,JS)
         END DO
       ELSE IF (KNORM2(JR,JS) == 2) THEN   ! Component normalisation
!         A1 = ABS (BPRM(JC,JR,JS))
!         A2 = 1.E-6 * BPRM(4,JR,JS)
!         IF (A1 < A2) A1 = BPRM(4,JR,JS)
         A1 = BPRM(JR,JS)
         DO JP = 1, NPAR
           SFD(1:NFRQ,JR,JS,JC,JP) = SFD(1:NFRQ,JR,JS,JC,JP) / A1
         END DO
       END IF
     END DO
   END DO
 END DO

 ! Convert to LINE-based output and apply units.

 IF (ISYS == 2) UNITS = 31    !  Sampo based on ratio
 DO JL = 1, NLINES
   A1 = 1.
   SELECT CASE (UNITS(JL))
   CASE (32)
     A1 = 100.
   CASE (2,33)
     A1 = 1000.
   CASE (3,34)
     A1 = 1.E6
   CASE (4,11,21,35)
     A1 = 1.E9
   CASE (12,22)
     A1 = 1.E12
   END SELECT
   JRL = 0
   DO JS = LNTR(1,JL), LNTR(2,JL)
     DO JR = LNTR(3,JL), LNTR(4,JL)
       JRL = JRL + 1
       DO JC = 1, 3
         DO JP = 1, NPAR
           SFDL(1:NFRQ,JRL,JL,JC,JP) = A1 * SFD(1:NFRQ,JR,JS,JC,JP)
         END DO
       END DO
     END DO
   END DO

   ! For surface dipole surveys, reorient component 1 from north to lie along
   ! the Survey X axis and component 2 from east to lie along the Survey Y axis.
   ! For downhole surveys apply u,V,A transformation.

   IF (RX_TYPE(JL) > 1) CYCLE  ! Skip if not magnetic dipole receiver

   CAZ0 = COS (SVAZM (JL))
   SAZ0 = SIN (SVAZM (JL))
   IF (ABS (CAZ0) < 1.E-4) CAZ0 = 0.
   IF (ABS (SAZ0) < 1.E-4) CAZ0 = 0.
   DO JR = 1,NRX(JL)
     DO JF = 1,NFRQ
       DO JP = 1, NPAR
         Q1(1:3) = SFDL(JF,JR,JL,1:3,JP)
         IF (IDH(JL) == 0) THEN  !  Surface survey
           SFDL(JF,JR,JL,1,JP) =  CAZ0 * Q1(1) + SAZ0 * Q1(2)    ! X component parallel to survey direction
           SFDL(JF,JR,JL,2,JP) = -SAZ0 * Q1(1) + CAZ0 * Q1(2)    ! Y component transverse to survey direction
           IF (ISYS == 2) THEN   ! Sampo
             A2 = ABS (SFDL(JF,JR,JL,3,JP) / SFDL(JF,JR,JL,1,JP))
             SFDL(JF,JR,JL,1,JP) = CMPLX (A2,0.)
             SFDL(JF,JR,JL,2:3,JP) = (0.,0.)
           END IF
         ELSE
           CAZ = COS (RXAZM(JR,JL))
           SAZ = SIN (RXAZM(JR,JL))
           CDP = COS (RXDIP(JR,JL))
           SDP = SIN (RXDIP(JR,JL))
           IF (ABS (CAZ) < 1.E-4) CAZ = 0.
           IF (ABS (SAZ) < 1.E-4) CAZ = 0.
           IF (ABS (CDP) < 1.E-4) CDP = 0.
           IF (ABS (SDP) < 1.E-4) CDP = 0.
           QX =  CAZ * Q1(1) + SAZ * Q1(2)                  ! local horizontal radial component
           QY = -SAZ * Q1(1) + CAZ * Q1(2)                  ! local horizontal transverse component
           SFDL(JF,JR,JL,3,JP) =  CDP * Q1(3) + SDP * QX    ! Axial component
           IF (IDH(JL) == 1) THEN                              ! Conventional U,V,A processing
             SFDL(JF,JR,JL,1,JP) = -SDP * Q1(3) + CDP * QX        !   local U component
             SFDL(JF,JR,JL,2,JP) = QY                             !   local V component
           ELSE IF (IDH(JL) == 2) THEN                         ! Utem style U,V,A processing
             QX =  CAZ0 * Q1(1) + SAZ0 * Q1(2)                 !   Line-referenced horizontal radial component
             SFDL(JF,JR,JL,1,JP) = -SDP * Q1(3) + CDP * QX     !   Line-referenced U component
             SFDL(JF,JR,JL,2,JP) = SQRT (SUM (Q1(1:3)**2) - SFDL(JF,JR,JL,1,JP)**2 - SFDL(JF,JR,JL,3,JP)**2)
           END IF
         END IF
       END DO
     END DO
   END DO
 END DO

 END SUBROUTINE SET_OUTPUT_LINES_SFD

!===========================================================================!

 SUBROUTINE ESVD (AJAC,NDATA,NPAR,KP,WITHU,WITHV,SV,UMAT,VMAT,WSP,ETA,TOL,NW)

!----------------------------------------------------------------------------
!
!***  Called by: NLSQ
!***  Calls: DPROD1, WRYT_MESSAGE
!
!  Singular value decomposition.
!
!----------------------------------------------------------------------------
!
!  DESCRIPTION:
!
!  Singular value decomposition based on the Golub-Reinsch algorithm with
!  and option of least squares reduction of RHS vectors for under or over
!  determined problems.
!
!  This routine is based on a program written by P. Businger of Bell Telephone
!  Labs, but incorporates modifications due to R. Underwood (Stanford University)
!  and D.L.B. Jupp.
!
!  REFERENCE:
!
!  G.H. Golub and C. Reinsch, 1970, Singular value decomposition and least
!  squares solutions: Numerical Mathematics, vol. 14, pp. 403-420; ALGOL
!  language features both SVD and minfit with accumulation in double precision.
!
!----------------------------------------------------------------------------
!
!     INPUTS:
!
!   AJAC  = Rectangular matrix to be decomposed.
!   NDATA = Number of rows in AJAC; i.e., number of data points.
!   NPAR  = Number of columns in AJAC; i.e., number of model parameters.
!   KP    = If KP > 0 columns, NPAR+1, ..., NPAR+KP of AJAC contain KP
!           'right hand sides' - these are multiplied by the transpose of
!           UMAT for use in SOLVE (accompanying subroutine).
!   WITHU = Logical control variable governing whether or not UMAT is
!           constructed.
!   WITHV = Logical control variable governing whether or not VMAT is
!           constructed.
!   ETA   = Relative precision of real numbers.
!   TOL   = Smallest representable number divided by ETA.
!
!     OUTPUTS:
!
!   SV    = Vector of the ordered NPAR singular values of AJAC.
!   UMAT  = Matrix of data (LHS) eigenvectors.
!   VMAT  = Matrix of model (RHS) eigenvectors.
!   WSP   = Workspace array of 3*NPAR.
!
!----------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER, PARAMETER :: ITMAX = 30
 INTEGER I,IM1,ITER,J,K,KK,KP,K1,L,LL,L1,MKP,MP1,NDATA,NMK,NPI,NPK,NPL,NPAR,NP, &
         NW,N1,N2
 REAL CS,EPS,ETA,F,FAC,FTEMP,G,H,Q,R,SN,TOL,W,X,Y,Z
 LOGICAL WITHU,WITHV
 REAL AJAC(NDATA,NPAR+KP),UMAT(NDATA,NPAR),VMAT(NPAR,NPAR),SV(NPAR),WSP(3*NPAR), &
      DPROD1
 EXTERNAL DPROD1

 NP = NPAR + KP
 N1 = NPAR + 1
 N2 = NPAR + NPAR
 WSP(N1) = 0.
 K = 1

   ! Householder reduction to upper bidiagonal form.

50 K1 = K + 1
   NPK = NPAR + K1

   ! Row transformation.

   Z = 0.0
   MKP = NDATA - K + 1
   NMK = NPAR - K
   Z = DPROD1(MKP,1,1,Z,AJAC(K,K),AJAC(K,K))
   WSP(K) = 0.0
   IF ( Z > TOL ) THEN
     Z = SQRT(Z)
     WSP(K) = Z
     W = ABS(AJAC(K,K))
     FAC = Z + W
     Q = 1.0
     IF ( AJAC(K,K) < 0.0 ) Q = -1.0
     AJAC(K,K) = Q*FAC
     IF ( K /= NP ) THEN
       FAC = Z*FAC
       DO J = K1,NP
         Q = 0.0
         Q = DPROD1(MKP,1,1,Q,AJAC(K,K),AJAC(K,J))
         Q = Q/FAC
         AJAC(K:NDATA,J) = AJAC(K:NDATA,J) - Q*AJAC(K:NDATA,K)
       END DO

       ! Phase transformation.

       IF ( AJAC(K,K) > 0.0 ) AJAC(K,K1:NP) = -AJAC(K,K1:NP)
     END IF
   END IF

   IF ( K == NPAR ) THEN

     ! End of householder reduction.  Set tolerance for iteration.

     EPS = 0.0
     DO K = 1,NPAR
       NPK = N2 + K
       NPL = NPAR + K
       SV(K) = WSP(K)
       WSP(NPK) = WSP(NPL)
       EPS = MAX(EPS,SV(K) + WSP(NPK))
     END DO
     EPS = EPS*ETA

     ! Set UMAT, and VMAT, to identity and preset pad of zero's
     ! for case NDATA < NPAR.

     IF ( WITHU ) THEN
       UMAT = 0.
       DO J = 1,NPAR
         UMAT(J,J) = 1.0
       END DO
     END IF

     IF ( WITHV ) THEN
       VMAT = 0.
       DO J = 1,NPAR
         VMAT(J,J) = 1.0
       END DO
     END IF

     IF ( NDATA < NPAR .AND. NP > NPAR ) THEN
       MP1 = NDATA + 1
       DO I = MP1,NPAR
         AJAC(I,N1:NP) = 0.
       END DO
     END IF

     ! Main iteration loop on K ... Q-R algorithm due to Francis.

     DO KK = 1,NPAR
       K = N1 - KK
       NPK = N2 + K
       ITER = 0
120    LOOP1: DO LL = 1,K
         L = K + 1 - LL
         NPL = N2 + L
         IF ( ABS(WSP(NPL)) <= EPS ) GO TO 160
         IF ( ABS(SV(L - 1)) <= EPS ) EXIT LOOP1
       END DO LOOP1

       ! Cancellation.

       CS = 0.0
       SN = 1.0
       L1 = L - 1
       LOOP2: DO I = L,K
         NPI = N2 + I
         F = SN*WSP(NPI)
         WSP(NPI) = CS*WSP(NPI)
         IF ( ABS(F) <= EPS ) EXIT LOOP2
         H = SV(I)
         IF ( ABS(F) <= ABS(H) ) THEN
           SN = F/H
           W = SQRT(SN*SN + 1.0)
           SV(I) = H*W
           CS = 1.0/W
           SN = -SN*CS
         ELSE
           CS = H/F
           W = SQRT(CS*CS + 1.0)
           SV(I) = F*W
           SN = -1.0/W
           CS = -CS*SN
         END IF
         IF ( WITHU ) THEN
           DO J = 1,NPAR
             X = UMAT(J,L1)
             Y = UMAT(J,I)
             UMAT(J,L1) = X*CS + Y*SN
             UMAT(J,I) = Y*CS - X*SN
           END DO
         END IF
         IF ( NP/=NPAR ) THEN
           DO J = N1,NP
             Q = AJAC(L1,J)
             R = AJAC(I,J)
             AJAC(L1,J) = Q*CS + R*SN
             AJAC(I,J) = R*CS - Q*SN
           END DO
         END IF
       END DO LOOP2

       ! Test for convergence.

160    W = SV(K)
       IF ( L/=K ) THEN

         ! Test for maximum iterations.

         ITER = ITER + 1
         IF ( ITER <= ITMAX ) THEN

           ! Compute the implicit shift of origin from bottom 2x2 minor.

           X = SV(L)
           Y = SV(K - 1)
           G = WSP(NPK - 1)
           H = WSP(NPK)
           F = (Y - W)*(Y + W) + (G - H)*(G + H)
           FTEMP = 2.0*H*Y
           IF ( ABS(F) > ABS(FTEMP) ) THEN
             F = FTEMP/F
             G = SQRT(F*F + 1.0)
             F = 1.0/F
             G = G*F
           ELSE
             F = F/FTEMP
             G = SQRT(F*F + 1.0)
             IF ( F < 0.0 ) G = -G
           END IF
           F = ((X - W)*(X + W) + (Y/(F + G) - H)*H)/X
           CS = 1.0
           SN = 1.0
           L1 = L + 1

           ! Main loop Q-R transformation for SV(K).

           DO I = L1,K
             IM1 = I - 1
             NPI = N2 + I
             G = WSP(NPI)
             Y = SV(I)
             H = SN*G
             G = CS*G

             ! Givens rotation from the right.

             IF ( ABS(F) <= ABS(H) ) THEN
               CS = F/H
               W = SQRT(CS*CS + 1.0)
               WSP(NPI - 1) = H*W
               SN = 1.0/W
               CS = CS*SN
             ELSE
               SN = H/F
               W = SQRT(SN*SN + 1.0)
               WSP(NPI - 1) = F*W
               CS = 1.0/W
               SN = SN*CS
             END IF
             F = X*CS + G*SN
             G = G*CS - X*SN
             H = Y*SN
             Y = Y*CS
             IF ( WITHV ) THEN
               DO J = 1,NPAR
                 X = VMAT(J,IM1)
                 W = VMAT(J,I)
                 VMAT(J,IM1) = X*CS + W*SN
                 VMAT(J,I) = W*CS - X*SN
               END DO
             END IF

             !  Givens rotation from the left.

             IF ( ABS(F) <= ABS(H) ) THEN
               CS = F/H
               W = SQRT(CS*CS + 1.0)
               SV(IM1) = H*W
               SN = 1.0/W
               CS = CS*SN
             ELSE
               SN = H/F
               W = SQRT(SN*SN + 1.0)
               SV(IM1) = F*W
               CS = 1.0/W
               SN = SN*CS
             END IF
             F = CS*G + SN*Y
             X = CS*Y - SN*G
             IF ( WITHU ) THEN
               DO J = 1,NPAR
                 Y = UMAT(J,IM1)
                 W = UMAT(J,I)
                 UMAT(J,IM1) = Y*CS + W*SN
                 UMAT(J,I) = W*CS - Y*SN
               END DO
             END IF
             IF ( NPAR/=NP ) THEN
               DO J = N1,NP
                 Q = AJAC(IM1,J)
                 R = AJAC(I,J)
                 AJAC(IM1,J) = Q*CS + R*SN
                 AJAC(I,J) = R*CS - Q*SN
               END DO
             END IF
           END DO
           WSP(NPL) = 0.0
           WSP(NPK) = F
           SV(K) = X
           GOTO 120
         ELSE
           WRITE(NW,'(//T3,A,I3)') ' MESSAGE FROM ESVD: Maximum iterations exceeded for singular value:',K
         END IF
       END IF

       ! Convergence ... if singular value negative, make it positive.

       IF ( W < 0.0 ) THEN
         SV(K) = -W
         IF ( WITHV ) VMAT(1:NPAR,K) = -VMAT(1:NPAR,K)
       END IF
     END DO

     ! End of main loop. Order singular values, UMAT, VMAT, and RHS'S.

     DO K = 1,NPAR
       G = -1.0
       J = K
       DO I = K,NPAR
         IF ( SV(I) > G ) THEN
           G = SV(I)
           J = I
         END IF
       END DO
       IF ( J/=K ) THEN
         SV(J) = SV(K)
         SV(K) = G
         IF ( WITHV ) THEN
           DO I = 1,NPAR
             Q = VMAT(I,J)
             VMAT(I,J) = VMAT(I,K)
             VMAT(I,K) = Q
           END DO
         END IF
         IF ( WITHU ) THEN
           DO I = 1,NPAR
             Q = UMAT(I,J)
             UMAT(I,J) = UMAT(I,K)
             UMAT(I,K) = Q
           END DO
         END IF
         IF ( NPAR /= NP ) THEN
           DO I = N1,NP
             Q = AJAC(J,I)
             AJAC(J,I) = AJAC(K,I)
             AJAC(K,I) = Q
           END DO
         END IF
       END IF
     END DO

     ! Update umat with stored Householder transformations.

     IF ( WITHU ) THEN
       DO KK = 1,NPAR
         K = N1 - KK
         IF ( ABS(WSP(K)) > TOL ) THEN
           IF ( K <= NDATA ) THEN
             MKP = NDATA - K + 1
             FAC = ABS(AJAC(K,K))*WSP(K)

             ! Undo the phase.

             IF ( AJAC(K,K) > 0.0 ) UMAT(K,1:NPAR) = -UMAT(K,1:NPAR)
             DO J = 1,NPAR
               Q = 0.0
               Q = DPROD1(MKP,1,1,Q,AJAC(K,K),UMAT(K,J))
               Q = Q/FAC
               UMAT(K:NDATA,J) = UMAT(K:NDATA,J) - Q*AJAC(K:NDATA,K)
             END DO
           END IF
         END IF
       END DO
     END IF

     ! Update VMAT with stored Householder transformations.

     IF ( WITHV ) THEN
       IF ( NPAR >=2 ) THEN
         DO KK = 2,NPAR
           K = N1 - KK
           K1 = K + 1
           NPK = NPAR + K1
           IF ( ABS(WSP(NPK)) > TOL ) THEN
             IF ( K <= NDATA ) THEN
               NMK = NPAR - K
               FAC = ABS(AJAC(K,K1))*WSP(NPK)

               ! Undo the phase.

               IF ( AJAC(K,K1) > 0.0 ) VMAT(K1,1:NPAR) = -VMAT(K1,1:NPAR)
               DO J = 1,NPAR
                 Q = 0.0
                 Q = DPROD1(NMK,NDATA,1,Q,AJAC(K,K1),VMAT(K1,J))
                 Q = Q/FAC
                 VMAT(K1:NPAR,J) = VMAT(K1:NPAR,J) - Q*AJAC(K,K1:NPAR)
               END DO
             END IF
           END IF
         END DO
       END IF
     END IF
     RETURN
   ELSE

     ! Column transformation.

     Z = 0.0
     IF ( K <= NDATA ) Z = DPROD1(NMK,NDATA,NDATA,Z,AJAC(K,K1),AJAC(K,K1))
     WSP(NPK) = 0.0
     IF ( Z > TOL ) THEN
       Z = SQRT(Z)
       WSP(NPK) = Z
       W = ABS(AJAC(K,K1))
       FAC = Z + W
       Q = 1.0
       IF ( AJAC(K,K1) < 0.0 ) Q = -1.0
       AJAC(K,K1) = Q*FAC
       IF ( NDATA > K ) THEN
         FAC = Z*FAC
         DO I = K1,NDATA
           Q = 0.0
           Q = DPROD1(NMK,NDATA,NDATA,Q,AJAC(K,K1),AJAC(I,K1))
           Q = Q/FAC
           AJAC(I,K1:NPAR) = AJAC(I,K1:NPAR) - Q*AJAC(K,K1:NPAR)
         END DO

         ! Phase transformation.

         IF ( AJAC(K,K1) > 0.0 ) AJAC(K1:NDATA,K1) = -AJAC(K1:NDATA,K1)

       END IF
     END IF
     K = K1
     GO TO 50
   END IF

 END SUBROUTINE ESVD

!============================================================================

  REAL FUNCTION DPROD1 (N,N1,N2,A,B,C)

!----------------------------------------------------------------------------
!
!***  Called by ESVD, SOLVE2
!
!     Double precision inner product routine:
!
!     DPROD = A + B * C
!
!         A = scalar
!       B,C = vectors (can be rows or columns of arrays)
!         N = length of vectors
!     N1,N2 = increment for b,c
!           = 1 if col of array
!           = col length (i.e. no. of rows) if row of array
!
!----------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER N,NA,NB,N1,N2,I
 DOUBLE PRECISION Z1,Z2,Z3
 REAL A,B(1),C(1)

 Z1 = A

 IF ( N >= 1 ) THEN
   NA = 1
   NB = 1
   DO I = 1, N
     Z2 = B(NA)
     Z3 = C(NB)
     Z1 = Z1 + Z2*Z3
     NA = NA + N1
     NB = NB + N2
   END DO
 END IF

 DPROD1 = REAL (Z1)

 END FUNCTION DPROD1

!===========================================================================

 SUBROUTINE SOLVE (NMAX,NMIN,VMAT,SV,UTRES,WSP,RSVT,ZERO,PDRE,NSV,DELPAR)

!---------------------------------------------------------------------------
!
!*** Called by: NLSQ
!
! Solves the damped eigenvalue problem, DELPAR = V * (DMPFAC/SV) * UT * RES
! where UT * RES has been computed previously. This is a generic subroutine
! in that it can be called for either over- or under-determined problems
! by appropriate calling.
!
!  NMAX - Maximum dimension; NPAR if over-determined, NPAR if under-determined.
!  NMIN - Minimum dimension; NPAR if over-determined, NDATA if under-determined.
!  VMAT - V matrix; (NPAR,NPAR) dimension if over-determined, (NPAR,NDATA)
!         dimension if under-determined.
!    SV - Singular values; (NPAR) length if over-determined, (NDATA) length if
!         under-determined.
! UTRES - UT * RES; (NPAR) length if over-determined, (NDATA) lenth if
!         under-determined.
!   WSP - Workspace vector; (NPAR) length if over-determined, (NDATA) length
!         if under-determined.
!  RSVT - Relative singular value threshold.
!  PDRE - Predicted decrease in residual error.
!
!---------------------------------------------------------------------------

 INTEGER NMAX,NMIN,NSV,I
 REAL EIGPAR(NMIN),SV(NMIN),DELPAR(NMAX),UTRES(NMIN),WSP(3*NMIN), &
      VMAT(NMAX,NMIN),RSVT,Q,DMPFAC,ZERO,PDRE

 ! Compute the eigenparameters with damping of the singular values.

 NSV = 0 ; EIGPAR = 0. ; WSP(1:2*NMIN) = 0.
 DO I = 1, NMIN
   Q = SV(I) / SV(1)
   IF (Q <= ZERO) CYCLE
   NSV = NSV + 1
   IF (Q < RSVT) THEN
     Q = (Q/RSVT)**4
     DMPFAC = Q/(1. + Q)
   ELSE
     DMPFAC = 1. / (1. + (RSVT/Q)**4)
   END IF
   WSP(I) = UTRES(I)*UTRES(I)       ! Store the error.
   WSP(NMIN+I) = DMPFAC             ! Store the damping factors.
   EIGPAR(I) = (DMPFAC/SV(I))*UTRES(I)  ! Eigenparameter calculation.
 END DO

 ! Calculate change in physical parameters from eigenparameters.

 DELPAR = MATMUL(VMAT,EIGPAR)

 ! Predicted decrease in residual error.

 PDRE = DOT_PRODUCT(WSP(1:NMIN),WSP(NMIN+1:2*NMIN))

 END SUBROUTINE SOLVE

!---------------------------------------------------------------------------

 SUBROUTINE IMPORTANCE (NMAX,NMIN,VMAT,WSP,IMPORT)

!---------------------------------------------------------------------------
!
!*** Called by: NLSQ
!
! The importance is computed as the rotation of the eigenparameters into
! parameter space. This is simply computed as:
!
! IMPORTANCE = SQRT (VT * DMPFAC * DMPFAC * V)
!
! This is a generic subroutine in that it can be called for either over-
! or under-determined problems by appropriate calling.
!
!  NMAX - Maximum dimension; NPAR if over-determined, NPAR if under-determined.
!  NMIN - Minimum dimension; NPAR if over-determined, NDATA if under-determined.
!  VMAT - V matrix; (NPAR,NPAR) dimension if over-determined, (NPAR,NDATA)
!         dimension if under-determined.
!   WSP - Workspace vector; (NPAR) length if over-determined, (NDATA) length
!         if under-determined. Contains the damping factors.
!  IMPORT - Parameter importance; NPAR long, always.
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER NMAX,NMIN,I,J
 REAL VMAT(NMAX,NMIN),WSP(3*NMIN),IMPORT(NMAX),CUM_IMP

 DO I = 1, NMAX
   CUM_IMP = 0.
   DO J= 1, NMIN
     CUM_IMP = CUM_IMP + (VMAT(I,J) * WSP(NMIN+J))**2
   END DO
   IMPORT(I) = SQRT(CUM_IMP)
 END DO

 END SUBROUTINE IMPORTANCE

!============================================================================

 SUBROUTINE NOISE_2_SIGR (NPAR,NDATA,XMODL,XDATA,XWTS,NSR)

!----------------------------------------------------------------------------
!
!***  Called by: NLSQ
!
!  Compute the Noise to Signal Ratio (NSR)
!
!----------------------------------------------------------------------------
!
!  DESCRIPTION:
!
!  Compute the Noise to Signal Ratio (NSR).
!
!  Set up a stitched log representation for model and data voltages.
!  Use a base of 6 orders of magnitude or the data dynamic range, whichever
!  is the lesser. Accumulate the means and variances in double precision.
!
!----------------------------------------------------------------------------
!
!     INPUTS:
!
!   NPAR  = Number of model parameters.
!   NDATA = Number of data points.
!   XMODL = Model data in microvolts.
!   XDATA = Observed data in microvolts.
!   XWTS  = Weights for data points.
!
!     OUTPUTS:
!
!   NSR   = Noise to signal ratio.
!
!----------------------------------------------------------------------------

 IMPLICIT NONE
 REAL, PARAMETER :: TOL = 0.1E-5
 INTEGER NPAR,NDATA,J1,XWTS(NDATA)
 REAL XMODL(NDATA),XDATA(NDATA),YM(NDATA),YD(NDATA),PK,PKM,BASE,ZM,ZD,YBAR, &
      NSR,CUMM,CUMD,CWMD

 BASE = ABS (XDATA(1))
 PK = BASE

 DO J1 = 2, NDATA
   BASE = MIN ( ABS ( XDATA(J1)),BASE )
   PK   = MAX ( ABS ( XDATA(J1)),PK   )
 END DO

 PKM = TOL * PK
 BASE = MAX(PKM,BASE)

 CUMM = 0.
 YM = 0.
 YD = 0.

 DO J1 = 1, NDATA
   IF (REAL(XWTS(J1)) > 1.E-4) THEN
     ZM = ABS (XMODL(J1))
     ZD = ABS (XDATA(J1))
     IF ( ZM > BASE ) THEN
       YM(J1) = LOG (ZM / BASE)
       IF (XMODL(J1) < 0) YM(J1) = -YM(J1)
     END IF
     IF (ZD > BASE) THEN
       YD(J1) = LOG (ZD / BASE)
       IF (XDATA(J1) < 0) YD(J1) = -YD(J1)
     END IF
     CUMM = CUMM + YM(J1)
   END IF
 END DO

 YBAR = CUMM / REAL(NDATA)

 CUMM = 0.
 CUMD = 0.

 DO J1 = 1, NDATA
   CUMM = CUMM + (YM(J1) - YBAR)**2
   CWMD = REAL(XWTS(J1)) * (YM(J1) - YD(J1))
   CUMD = CUMD + CWMD**2
 END DO

 CUMM = CUMM / ABS(REAL(NDATA-1))
 CUMD = CUMD / ABS(REAL(NDATA-NPAR))

 NSR = SQRT (CUMD / CUMM)

 END SUBROUTINE NOISE_2_SIGR

Logical Function IsComment(char)
!-------------------------------
!
!  Function designed to test whether a character is a comment (/ or \)

   Implicit none
   Character (Len = 1) :: char
   ! Logical :: IsComment

   If (char .eq. achar(47)) Then ! /
      IsComment = .True.
   Else If (char .eq. achar(92)) Then  ! \
      IsComment = .True.
   Else
      IsComment = .False.
   End If

   Return

End Function IsComment
!==============================================================================
!
