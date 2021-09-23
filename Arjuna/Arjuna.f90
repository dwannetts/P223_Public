!   PROGRAM Arjuna
!----------------------------------------------------------------------------------------
Module AG_Metadata
!--------------
! 
! Module designed to hold 'constant character options'
    Implicit NONE

    Character (Len = 40), Parameter :: PNAME = 'Arjuna'
    Character (Len = 40), Parameter :: PVERS = '2.1.3'
    Character (Len = 40), Parameter :: PDATE = '02 February, 2021'
    Character (Len = 40), Parameter :: PAUT1 = 'CSIRO Electromagnetic Modelling Group'
    Character (Len = 40), Parameter :: PAUT2 = 'Art Raiche, Fred Sugeng and Glenn Wilson'
    Character (Len = 40), Parameter :: PAUT3 = 'David Annetts (david.annetts@csiro.au)'
    Character (Len = 40), Parameter :: PPROJ = 'AMIRA P223F'
    Character (Len = 80), Parameter :: PRELS = 'GPL-2.0 (Full text at https://opensource.org/licenses/gpl-2.0.php)'

End Module AG_Metadata
!----------------------------------------------------------------------------------------
!
!   2.1.3
!   1.  DWA: Arjuna will now read frequencies from Arjuna.frq in DO3D = 2 mode.
!
!     CHANGES to Version 2.1.0 from from Version 2.0.4
!     ------------------------------------------------
!
!   1. In the previous version, full Arjuna acuracy was obtained only when the
!      receiver was coincident with a mesh node.  Inter-node accuracy has been
!      improved significantly by performing the inter-node interpolation in
!      the spatial domain rather than uin the wave-number domain as was done
!      previously.
!
!   2. An error causing inaccurate and in-loop responses was identified and corrected
!
!   3. Code which caused stack overflows on Linux-based systesm was identified & 
!      rewritten.  Windows systems appear unaffected.
!
!
!********************
!   DESCRIPTION     *
!********************
!
!  Arjuna computes the 3D ground and downhole EM response of a general
!  heterogeneous 2-D structure (cross section).  The cross section lies
!  in the East-West vertical plane.  Lithology is allowed to vary East-
!  West and with depth.  It is constant in the North-South direction.
!  Similarly topography can vary East-West but not North-South.
!  Arjuna is capable of modelling high contrasts accurately.
!
!  The program can be used for inductive sources but not grounded wires.
!  Magnetic dipole sources can be above, on or below the surface.  They
!  can dip but their azimuth is required to lie along the East-West plane,
!  perpendicular to strike.  Dipole receivers can have any dip or azimuth.
!
!  Loop transmitters and receivers must lie on the surface.  They are
!  are modelled as an area integration of magnetic dipoles rather than a
!  line integral of electric dipoles.  Members of the equivalent dipole
!  are given the elevation and dip angle of the ground cells in which
!  they reside, thus allowing loops draped on topography to be modelled
!  accurately.  Transmitter loops can be polygonal but spearated receiver
!  loops must be rectangular.
!
!  Surveys can be defined with one or more groups of receivers assigned
!  to each transmitter position or for transmitter and single loop or
!  magnetic dipole receiver to move at constant offset.
!
!  Time-domain responses are computed by first computing frequency-domain
!  responses over several decades. The frequency-domain response is then
!  extrapolated back to zero frequency.  A cubic spline interpolation of the
!  imaginary component of the magnetic field is used to represent the
!  frequency-domain response over the whole spectrum.  This spline is
!  converted to time domain step-response using a Hankel transform.
!  The response is stacked over several cycles and then convolved with the
!  transmitter system waveform.  On-time and off time channels are permitted.
!
!  When used in time-domain mode, all frequency-domain computations are
!  saved in a file ArjunAir.frq which allows them to be reused for
!  different time-domain systems as long as the model and all geometric
!  factors remain the same.  These additional time-domain computations
!  can be done in less than one percent of the time required by
!  the original computation in most cases.
!
!  The frequency-domain solutions are solved by transformation into a mixed
!  spatial-Fourier domain (kx,y,z,f) where kx is the wavenumber corresponding
!  to the strike direction.  In this domain, an isoparametric finite-element
!  method is used to solve a coupled system of differential equations for the
!  along-strike components of the magnetic and electric fields.  The global
!  matrix equation is solved using the frontal solution method.  The vertical
!  and transverse strike magnetic field components are then computed in this
!  domain.  After obtaining solutions for 21 wavenumber values, the three
!  magnetic field components are then transformed into the (x,y,z) domain.
!
!  It is assumed that data entry will be made using one of two
!  graphical user interface - either EMGUI (Encom) or Maxwell (EMIT).
!  The mesh property specification now based on lithologies.
!
!*********************
!   MESH DESIGN      *
!*********************
!
!  A good conservative starting point for mesh design would be a domain
!  that extends 2 km horizontally and 1 km deep.  The top 4 rows should be
!  about 12 m high with a 6 percent expansion factor used from row 5 to the
!  mesh bottom.  Suggested horizontal cell width is 40 m with 20 m near
!  boundaries where the conductivity taks big jumps.
!  Receivers shoud be at least 300 m from the left and right boundaries.
!------------------------------------------------------------------------
!
!*********************
!   FILE CONVENTIONS *
!*********************
!
!   The input control file, now called Arjuna.cfl is read
!   from logical unit number 3.
!
!   The output data file, now called Arjuna.out is written
!   to logical unit number 4.
!
!   Frequency-domain data, now called Arjuna.frq data is written
!   to logical unit number 7.
!
!   Messages about data or runtime errors are now written in a file
!   called Arjuna.log on logical unit 23.
!
!   The AMIRA format file, now called Arjuna.amx, is written
!   to logical unit number 22.
!
!    UNIT #   UNIT ID      FILE ID           FUNCTION
!    ------   -------      -------           --------
!      3        NR       Arjuna.cfl    Input control file
!      4        NW       Arjuna.out    Time or frequency-domain output data (verbose)
!      7        ND       Arjuna.frq    Frequency-domain data for TDEM reuse
!      22       NA       Arjuna.amx    Output data in extended AMIRA format
!      23       NLG      Arjuna.log    Runtime & data error messages
!
!*************************
!   GEOMETRY CONVENTIONS *
!*************************
!
!   The user enters source coordinates in terms of Easting, Northing & Altitude
!   Altitude is positive upwards.
!
!   The user enters mesh coordinates in terms of Northing, Depth & Easting,
!   Depth is positive downwards.
!
!   Internally, a right-handed coordinate system is used where
!   strike angle is 0 pointing North and positive Eastwards
!   Thus the positive X axis points North.
!   The positive Y axis points East.  Z is positive downwards.
!   This has no effect on data input.
!
!
!** RECORD 1:  TITLE - up to 120 characters
!
!** RECORD 2:  TDFD, PRFL, STEP, ISTOP
!
!      TDFD = 1 compute time domain responses
!           = 2 compute frequency domain responses
!           = -1 over-ride default limits for frequency to time domain conversion
!
!      PRFL = 1  prints response in profile mode.
!                Each column contains the response for one channel
!                (or frequency) for all stations on the profile.
!
!           = 0  prints responses in temporal or frequency mode.
!                Each column contains the responses for one receiver
!                position for either all channels (if TDFD = 1) or
!                for all frequencies (if TDFD = 2).
!
!      STEP = 0 => Compute dB/dt for all magnetic dipole receivers.
!           = 1 => Compute B for all magnetic dipole receivers.
!           = 0 for all other receiver types
!
!     ISTOP = 0  read the input data and run the specified models.
!           = 1  read the input data, print the model description
!                  and STOP so that the model description can be verified.
!                  REMEMBER to change ISTOP to 0 once the models have been
!                  verified.
!   ______________________________________________________________________________________
!
!** (only if TDFD = -1)  RECORD 2.1:  MINFRQ, MAXFRQ, MXFRQE  (only if TDFD = 1)
!
!                Time domain output is computed as a transform of frequency domain results.
!                MINFRQ = frequency below which the step B and impulse E are extrapolated
!                          back to zero as constant.
!                MAXFRQ = cut off frequency for magnetic field computations.
!                MXFRQE = cut off frequency for electric field computations.
!                DEFAULTS:  MINFRQ = 1 Hz for B field; 0.1 Hz for electric field
!                           MAXFRQ = 100 KHz or 1 MHz;  MXFRQE = 4 kHz
!   ---------------------------------------------------------------------------------------
!
!         CONTROL PARAMETERS FOR TIME-DOMAIN SYSTEMS
!         ------------------------------------------
!
!** RECORD 3 (time-domain):  DO3D, NSX, OFFTIME, NCHNL, KRXW
!
!      DO3D = 1  compute new 3D model
!
!           = 2  use previously computed frequency-domain responses
!                contained in PROG-NAME.frq    Setting DO3D = 2 allows
!                the user to test different time-domain systems (with
!                the same model and Tx-Rx geometry) in a tiny fraction
!                of the time required to rerun the whole model.  Under
!                this option, the user can vary the waveform, receiver
!                channels, PRFL mode or switch between B & dB/dt
!
!                Variations in model and system geometry require DO3D = 1
!
!      NSX = number of points needed to describe 1/2 cycle of the transmitter
!            waveform.  A bipolar waveform is assumed.  Thus for a system
!            like Sirotem or EM37, NSX = 4, one point each for the start
!            and end of the two ramps.  For an ideal step turnoff system,
!            NSX = 1
!
!      OFFTIME - time (milliseconds) between end of one pulse and the start of
!                the next pulse (of opposite sign) since a bipolar waveform is
!                assumed.  For systems which have a signal which is always on,
!                OFFTIME = 0.
!
!      NCHNL - number of receiver channels
!
!      KRXW = 1 => receiver channels will be read in terms of start and end
!                  times in ms.
!           = 2 => receiver channels will be read in terms of midpoints and
!                  channel widths.
!         -->   THESE TIMES WILL BE TAKEN FROM THE SIGNAL ORIGIN TIME
!         -->   To reference them to the signal off time  (or any other time)
!         -->   set KRXW = -1 or -2 and enter reference time in RECORD 3.1
!
!    Only if KRXW < 0
!** RECORD 3.1 (time-domain):  REFTYM
!               Time (in ms) from which TMS or TOPN & TCLS are measured.
!               For example, this could be signal off-time or
!                            start of downward ramp.
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
!              (measured from start of signal turn on.)
!
!           SKIP to RECORD 5
!
!
!   For KRXW = 2
!** RECORD 4.2 (time-domain):  (TMS(J), J=1,NCHNL)
!** RECORD 4.3 (time-domain):  (WIDTH(J), J=1,NCHNL)
!
!        TMS(J) -  centre of receiver window J in ms.
!                  measured from start of signal turn on.
!      WIDTH(J) -  width of receiver window J in ms.
!
!           SKIP to RECORD 5
!
!   For KRXW = -1
!** RECORD 4.2 (time-domain):  (TOPN(J), TCLS(J), J=1, NCHNL)
!
!              Start and end times (in ms) of receiver windows.
!              measured from time entered in RECORD 3.1
!
!   For KRXW = -2
!** RECORD 4.2 (time-domain):  (TMS(J), J=1,NCHNL)
!** RECORD 4.3 (time-domain):  (WIDTH(J), J=1,NCHNL)
!
!        TMS(J) -  centre of receiver window J in ms.
!                  measured from time entered in RECORD 3.1
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
!      CURNT(J) - current in amps for Jth frequency
!
!
!         TRANSMITTER - RECEIVER INFORMATION
!         ----------------------------------
!
!** RECORD 5:  SOURCE_TYPE, SURVEY_TYPE, NTX, NRXG
!
!
!      SOURCE_TYPE = 1 => general loop (vertex locations will be specified)
!                  = 2 => grounded wire (path + endpoints will be specified
!                  = 3 => magnetic dipole (location & angle will be specified
!                  = 4 => magnetotellurics
!
!      SURVEY_TYPE = 1 => separate setup of transmitter and receiver arrays.
!
!                  = 2 => single magnetic dipole receiver moving at fixed
!                         offset with a magnetic dipole or loop transmitter
!                         (only for SOURCE_TYPE = 1 or 3)
!
!                  = 3 => central loop survey with magnetic dipole receiver
!                         in the centre of transmitting loop.
!
!                  = 4 => coincident loop survey (time-domain only)
!                         (only for SOURCE_TYPE = 1 & TDFD = 1)
!
!       NTX = number of transmitter positions
!             (loop, magnetic dipole, or electric bipole or dipole)
!
!      NRXG = number of receiver groups.  A receiver group consists of one
!             or more receivers in fixed positions.  All receivers in a group
!             must measure the same thing, either B, dB/dt for a magnetic
!             dipole receivers, or the electric field integrated over electric
!             dipole receiver.
!
!             If SURVEY_TYPE > 1, set NRXG = NTX
!
!============================================================================
!
!  RECORD(S) 6 SPECIFIES THE TRANSMITTER LOCATIONS
!============================================================================
!
!
!** For SOURCE_TYPE = 1 or 2 (closed loop & grounded wire)
!   ======================================================
!
!     For each source position, specify the number of vertices followed by the
!     East, North, & RLs of each vertex.  If the loop is above the earth's
!     surface reference point, RL is positive.
!
!     If SOURCE_TYPE = 1, the program will connect the first vertex
!                         to the last to complete the loop.
!                         DON'T SPECIFY THE SAME VERTEX MORE THAN ONCE.
!
!     If SOURCE_TYPE = 2, the program assumes that the wire is grounded
!                         at the first and last (N_VRTX) vertex.
!
!   SPECIFY_TX: DO FOR J = 1 TO NTX
!** RECORD 6.J.0: N_VRTX - number of vertices
!
!     SPECIFY_VERTICES: DO FOR I = 1 TO N_VRTX
!**   RECORDS 6.J.I: SXE(I,J), SXN(I,J), SXZ(I,J)
!
!       SXE(I,J) = east coordinate of vertex I for loop position J
!       SXN(I,J) = north coordinate of vertex I for loop position J
!       SXZ(I,J) = relative level of vertex I for loop position J
!
!     END SPECIFY_VERTICES FOR TX POSITION J
!
!   END SPECIFY_TX
!
!
!** For SOURCE_TYPE = 3 (dipole source)
!   ===================================
!
!   SPECIFY_TX: DO FOR J = 1 TO NTX
!** RECORD 6.J: SDE(J), SDN(J), SDZ(J), SXDIP(J), SXAZ(J), SXMNT(J)
!
!       SDE(J) = east coordinate of dipole J
!       SDN(J) = north coordinate of dipole J
!       SDZ(J) = relative level of dipole J
!
!-------------------------------------------------------------------------
!     SXDIP(J) = dip (in degrees) of dipole J  (Borehole convention)
!                                              ---------------------
!      SXAZ(J) = azimuth (in degrees) of dipole J
!                (north = 0, east = 90)
!
!------^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!      SXAZ is restricted to 90 or -90 (270) degreesin this version
!------^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!
!         For a dipole whose axis points vertically downwards,
!             SXDIP = 90;  SXAZ = 0
!-------------------------------------------------------------------------
!     SXMNT(J) = moment per unit amp of dipole J
!                (physical area * number of turns)
!
!   END SPECIFY_TX
!
!============================================================================
!
!  RECORD(S) 7 SPECIFIES THE TRANSMITTER LOCATIONS & MEASUREMENT UNITS
!============================================================================
!
!
!   UNITS is an integer which allows the user to choose outut units for
!         dB/dt, B, or grounded wire receivers.
!
!
!
!            RX_TYPE(J)= 1        RX_TYPE(J)= 1        Loop receivers &
!            (mag dipole)         (mag dipole)         grounded electrodes
!            &  STEP = 0          &  STEP = 1          RX_TYPE(J)= 2 or 3
!    UNITS       dB/dt              B                 (integrated E field)
!
!      1      nanoteslas /sec    nanoteslas             volts
!      2      picoteslas /sec    picoteslas             millivolts
!      3      microvolts         femtoteslas            microvolts
!      4      nanovolts                                 nanovolts
!
!  ------------------------------------------------------------------------------
!  IF SURVEY_TYPE = 4 (coincident loop)
!** RECORD 7: UNITS
!
!***********************************************
!   SKIP TO RECORD 10 for model specification
!***********************************************
!
!=============================================================================
!  IF SURVEY_TYPE = 2 or 3 (single, fixed offset magnetic dipole receiver),
!  specify receiver moment & offset.
!
!** RECORD 7: RXOE, RXON, RXOZ, RXFMNT, CMP, UNITS
!
!        RXOE - east offset of receiver from transmitter
!        RXON - north offset of receiver from transmitter
!        RXOZ - vertical offset (positive for receiver above transmitter)
!      RXFMNT - physical area * turns of receiver
!
!          CMP(J) = 0 => compute 3 components (North, East, Vertical)
!          CMP(J) = 1 => compute single component in vertical direction
!
!        ___________________________________________________________________
!        If the source is a magnetic dipole an additional option to write
!        three components in a borehole system is available:
!        ___________________________________________________________________
!
!          CMP(J) = 1 => compute single component in Tx dipole direction
!
!          CMP(J) = 2 => write 3 components in a borehole system (HR, SL, AX)
!                        defined by transmitter azimuth rotation, SXAZ
!                        followed by transmitter dip rotation, SXDIP
!
!              AX (Axial component) lies along the borehole axis, defined by SXDIP.
!                 If the hole is vertical, it contains the vertical component.
!                 If the hole is horizontal, with azimuth = 0, it points north.
!              SL (Slope component perpendicular to AX) in the vertical plane
!                 containing the borehole.
!                 If the hole is vertical, it points North. (12 o'clock)
!                 If the hole is horizontal, it contains the vertical component.
!              HR is the Horizontal component regardless of dip.
!                 If the hole is vertical, it points West. (9 o'clock)
!
!***********************************************
!   SKIP TO RECORD 10 for model specification
!***********************************************
!
!=============================================================================
!
!   RECEIVER GROUP SPECIFICATION FOR SURVEY_TYPE = 1
!   ------------------------------------------------
!
!   Note that electric dipole receivers cannot be in the same group
!   as magnetic dipole receivers.
!
!
!   Enter one RECORD 7 followed by RECORDs 8 for each group of receivers.
!   DO FOR J = 1 TO NRXG
!
!** RECORD 7.J: NRX(J), RX_TYPE(J), CMP(J), UNITS(J)
!
!         NRX(J) = number of receivers for receiver group J
!     RX_TYPE(J) = 1 for magnetic dipole receivers
!                = 2 for finite loop receiver
!                = 3 for electric dipole receivers
!
!          CMP(J) = 0 => compute 3 components (North, East, Vertical)
!          CMP(J) = 1 => compute single component in vertical direction
!
!        ___________________________________________________________________
!        If the receiver is a magnetic dipole, an additional option to write
!        three components in a borehole system is available:
!        ___________________________________________________________________
!
!          CMP(J) = 2 => write 3 components in a borehole system (HR, SL, AX)
!                        defined by receiver azimuth rotation, RXAZ
!                        followed by receiver dip rotation, RXDIP
!
!              AX (Axial component) lies along the borehole axis, defined by SXDIP.
!                 If the hole is vertical, it contains the vertical component.
!                 If the hole is horizontal, with azimuth = 0, it points north.
!
!              SL (Slope component perpendicular to AX) in the vertical plane
!                 containing the borehole.
!                 If the hole is vertical, it points North. (12 o'clock)
!                 If the hole is horizontal, it contains the vertical component.
!
!              HR is the Horizontal component regardless of dip.
!                 If the hole is vertical, it points West. (9 o'clock)
!
!     Enter  RECORD(s) 8 for each receiver in Group J,
!     (DO FOR I = 1 TO NRX(J)
!
!       IF RX_TYPE(J) = 1 (magnetic dipoles), AND CMP(J) < 2,
!                                             --------------
!             enter the east (RXE) & north (RXN) cordinates and RLs (RXZ) of the
!             Ith receiver of receiver group J plus the receiver dipole moment
!             RXMNT, (area * number of turns)
!
!**   RECORD 8.I  RXE(I,J,1), RXN(I,J,1), RXZ(I,J,1), RXMNT(I,J)
!
!    OR IF RX_TYPE(J) = 1 (magnetic dipoles), AND CMP(J) = 2,
!                                             --------------
!             enter the east (RXE) & north (RXN) cordinates and RLs (RXZ) of the
!             Ith receiver of receiver group J plus the receiver dipole moment
!             RXMNT, (area * number of turns)
!             plus RXDIP, RXAZ, the dip and azimuth of each receiver position
!
!**   RECORD 8.I  RXE(I,J,1), RXN(I,J,1), RXZ(I,J,1), RXDIP(I,J), RXAZ(I,J), RXMNT(I,J)
!
!-------------------------------------------------------------------------
!     RXDIP(J) = dip (in degrees) of dipole receiver J  (Borehole convention)
!                                                       ---------------------
!      RXAZ(J) = azimuth (in degrees) of dipole J
!                (north = 0, east = 90)
!
!         For a dipole whose axis is horizontal pointing Northeast,
!             RXDIP = 0;  RXAZ = 45
!
!         For a dipole whose axis points vertically downwards,
!             RXDIP = 90;  RXAZ = 0
!-------------------------------------------------------------------------
!
!     or IF RX_TYPE(J) = 2 (finite loops) enter the east and north
!        coordinates and RLs of four corners.
!
!**   RECORD 8.I.1  RXE(I,J,1), RXN(I,J,1), RXZ(I,J,1)
!**   RECORD 8.I.2  RXE(I,J,2), RXN(I,J,2), RXZ(I,J,2)
!**   RECORD 8.I.3  RXE(I,J,3), RXN(I,J,3), RXZ(I,J,3)
!**   RECORD 8.I.4  RXE(I,J,4), RXN(I,J,4), RXZ(I,J,4)
!
!     or IF RX_TYPE(J) = 3 (electric dipoles) enter the east and north
!        coordinates and RLs of each electrode.
!
!**   RECORD 8.I.1  RXE(I,J,1), RXN(I,J,1), RXZ(I,J,1)
!**   RECORD 8.I.2  RXE(I,J,2), RXN(I,J,2), RXZ(I,J,2)
!
!   SURVEY EVENT SPECIFICATION FOR SURVEY_TYPE = 1
!   ----------------------------------------------
!
!   An event consists of specifying one transmitter position index and
!   one receiver group index.
!
!** RECORD 9.0: NEVENTS
!
!   DO FOR J = 1 TO NEVENTS - one record for each event
!** RECORD 9.J TX_INDEX(J), RXG_INDEX(J)
!
!      TX_INDEX(J) must be a positive integer less than or equal to NTX
!     RXG_INDEX(J) must be a positive integer less than or equal to NRXG
!
!===============================================================================
!
!          LITHOLOGY & STRUCTURE FOR ARJUNA & ARJUNA_AIR
!          =========================================
!
!         SET MESH SIZE, ACCURACY & SOLVER OPTIONS
!         ----------------------------------------
!
!**  RECORD 10:  NZ, NEAST, NLITH, KACC, SOLVER, OUTPUT
!
!                The heterogeneous domain must be divided into
!                a mesh which is defined by the user.
!
!          NZ - number of domain nodes in depth-direction.
!       NEAST - number of domain nodes in East-West direction.
!
!       NLITH - number of distinct lithologial units
!
!        KACC = 1 => standard solver with a balance between runtime & accuracy
!             = 2 => higher accuracy but much slower.   (not implemented for Arjuna)
!        KACC is reset to 1 in this version)
!
!      SOLVER = 1  direct matrix solution
!             = 2  iterative matrix solution            (not implemented for Arjuna)
!      SOLVER is reset to 1 in this version)

!
!        OUTPUT =  10 => If PPM available use PPM and ignore fields in Loki.amx
!        OUTPUT = -10 => Loki.amx file will Ignore PPM data and print field values
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
!                 NODE LOCATIONS:   NZ * NEAST records
!                 -------------------------------------
!
!       Nodes and associated lithologies are read a row at a time
!       (West to East) from the top (surface) to the bottom.
!
!       The lithology of each cell is associated with the top, western node
!       defining that cell.
!
!**  RECORDS 12.(I,J,K):  K, J, ZLOC, ELOC, LITH
!
!      1 <= K <= NZ      is the node index increasing from surface to bottom
!      1 <= J <= NEAST  is the node index increasing from West to East
!
!      ZLOC(J,K) = the relative level of node (J,K) in metres.
!                  For fixed J, it must increase negatively with
!                  increasing K; ie,
!
!      ELOC(J,K) = the east coordinate of node (J,K) in metres.
!                  For fixed J,K, it must increase with increasing I.
!
!
!      Each cell (or mesh element) is defined by its four corner nodes.
!      Each cell is identified by the indices of the node of its top, western corner.
!
!      Thus LITH (J,K) will be the lithology index of the cell
!      defined by nodes:
!
!      (K, J),   (K+1, J), (K, J+1), (K+1, J+1)
!
!      LITH is not read if J = NEAST or if K = NZ
!
!      There are NZ * NEAST records to be read in below, one
!      for each permutation of I, J, K
!
!============================================================================

   MODULE AG_Filter_coefficients
!  --------------------------

  IMPLICIT NONE

  INTEGER, PARAMETER :: JNLO=-250, JNHI=150, NDEC_JN=15, &
                        SNLO=-112, SNHI=85,  NDEC_SN=12
  INTEGER, PARAMETER :: NLO=-214, NHI=85, NDEC=12
  INTEGER J9,LL,IL

  REAL SHFTJN, WJ0(JNLO:JNHI), WJ1(JNLO:JNHI), WSIN(SNLO:SNHI), DELCOS,WCOS(-200:99)
  REAL WCOS1(NLO:NHI), WSIN1(NLO:NHI),WS(266),WC(281),AS(266),AC(281)
  SAVE

!  Filter restored to original Leroi_Air 7 February, 2000 (artificial shift removed)

!  J0 filter coefficients computed from the Niels Christensen program, FILCOA
!  for the following parameters:
!
!   ANY =  0      AMY =  0      NDEC = 15       NLO = -250        NHI =  150
!   IOPT = 1   ISHIFT = 0      OMEGA = .3 PI    EPS = 1.0E-12      SC = 3.257209
!      A = 0.162875             DEL0 = 0.14314998               ERROR =  1.4032E-08

 DATA SHFTJN /0.14314998/
 DATA (WJ0(J9), J9= -250,-161)/ &
  2.86608135867E-18,  3.34160553102E-18,  3.89602601168E-18,  4.54243283439E-18,  5.29608785801E-18,  6.17478510356E-18, &
  7.19927087644E-18,  8.39373359283E-18,  9.78637487555E-18,  1.14100754027E-17,  1.33031712306E-17,  1.55103589191E-17, &
  1.80837508313E-17,  2.10841055215E-17,  2.45822622636E-17,  2.86608135867E-17,  3.34160553102E-17,  3.89602601168E-17, &
  4.54243283439E-17,  5.29608785801E-17,  6.17478510356E-17,  7.19927087644E-17,  8.39373359283E-17,  9.78637487555E-17, &
  1.14100754027E-16,  1.33031712306E-16,  1.55103589191E-16,  1.80837508313E-16,  2.10841055215E-16,  2.45822622636E-16, &
  2.86608135867E-16,  3.34160553102E-16,  3.89602601168E-16,  4.54243283439E-16,  5.29608785801E-16,  6.17478510356E-16, &
  7.19927087644E-16,  8.39373359283E-16,  9.78637487555E-16,  1.14100754027E-15,  1.33031712306E-15,  1.55103589191E-15, &
  1.80837508313E-15,  2.10841055215E-15,  2.45822622636E-15,  2.86608135867E-15,  3.34160553102E-15,  3.89602601168E-15, &
  4.54243283439E-15,  5.29608785801E-15,  6.17478510356E-15,  7.19927087644E-15,  8.39373359283E-15,  9.78637487555E-15, &
  1.14100754027E-14,  1.33031712306E-14,  1.55103589191E-14,  1.80837508313E-14,  2.10841055215E-14,  2.45822622636E-14, &
  2.86608135867E-14,  3.34160553102E-14,  3.89602601168E-14,  4.54243283439E-14,  5.29608785801E-14,  6.17478510356E-14, &
  7.19927087644E-14,  8.39373359283E-14,  9.78637487555E-14,  1.14100754027E-13,  1.33031712306E-13,  1.55103589191E-13, &
  1.80837508313E-13,  2.10841055215E-13,  2.45822622636E-13,  2.86608135867E-13,  3.34160553102E-13,  3.89602601168E-13, &
  4.54243283439E-13,  5.29608785801E-13,  6.17478510356E-13,  7.19927087644E-13,  8.39373359283E-13,  9.78637487555E-13, &
  1.14100754027E-12,  1.33031712306E-12,  1.55103589191E-12,  1.80837508313E-12,  2.10841055215E-12,  2.45822622636E-12/
 DATA (WJ0(J9),J9= -160,-71)/ &
  2.86608135867E-12,  3.34160553102E-12,  3.89602601168E-12,  4.54243283439E-12,  5.29608785801E-12,  6.17478510356E-12, &
  7.19927087644E-12,  8.39373359283E-12,  9.78637487555E-12,  1.14100754027E-11,  1.33031712306E-11,  1.55103589191E-11, &
  1.80837508313E-11,  2.10841055215E-11,  2.45822622636E-11,  2.86608135867E-11,  3.34160553102E-11,  3.89602601168E-11, &
  4.54243283439E-11,  5.29608785801E-11,  6.17478510356E-11,  7.19927087644E-11,  8.39373359283E-11,  9.78637487555E-11, &
  1.14100754027E-10,  1.33031712306E-10,  1.55103589191E-10,  1.80837508313E-10,  2.10841055215E-10,  2.45822622636E-10, &
  2.86608135867E-10,  3.34160553102E-10,  3.89602601168E-10,  4.54243283439E-10,  5.29608785801E-10,  6.17478510356E-10, &
  7.19927087644E-10,  8.39373359283E-10,  9.78637487555E-10,  1.14100754027E-09,  1.33031712306E-09,  1.55103589191E-09, &
  1.80837508313E-09,  2.10841055215E-09,  2.45822622636E-09,  2.86608135867E-09,  3.34160553102E-09,  3.89602601168E-09, &
  4.54243283439E-09,  5.29608785801E-09,  6.17478510356E-09,  7.19927087644E-09,  8.39373359283E-09,  9.78637487555E-09, &
  1.14100754027E-08,  1.33031712306E-08,  1.55103589191E-08,  1.80837508313E-08,  2.10841055215E-08,  2.45822622636E-08, &
  2.86608135867E-08,  3.34160553102E-08,  3.89602601168E-08,  4.54243283439E-08,  5.29608785801E-08,  6.17478510356E-08, &
  7.19927087644E-08,  8.39373359283E-08,  9.78637487555E-08,  1.14100754027E-07,  1.33031712306E-07,  1.55103589191E-07, &
  1.80837508313E-07,  2.10841055215E-07,  2.45822622635E-07,  2.86608135866E-07,  3.34160553102E-07,  3.89602601167E-07, &
  4.54243283438E-07,  5.29608785799E-07,  6.17478510354E-07,  7.19927087640E-07,  8.39373359277E-07,  9.78637487545E-07, &
  1.14100754026E-06,  1.33031712304E-06,  1.55103589187E-06,  1.80837508307E-06,  2.10841055205E-06,  2.45822622620E-06/
 DATA (WJ0(J9),J9= -70,19)/ &
  2.86608135842E-06,  3.34160553063E-06,  3.89602601105E-06,  4.54243283340E-06,  5.29608785643E-06,  6.17478510107E-06, &
  7.19927087248E-06,  8.39373358656E-06,  9.78637486561E-06,  1.14100753870E-05,  1.33031712056E-05,  1.55103588795E-05, &
  1.80837507685E-05,  2.10841054221E-05,  2.45822621060E-05,  2.86608133369E-05,  3.34160549143E-05,  3.89602594894E-05, &
  4.54243273495E-05,  5.29608770041E-05,  6.17478485378E-05,  7.19927048056E-05,  8.39373296541E-05,  9.78637388116E-05, &
  1.14100738267E-04,  1.33031687328E-04,  1.55103549604E-04,  1.80837445571E-04,  2.10840955776E-04,  2.45822465035E-04, &
  2.86607886087E-04,  3.34160157229E-04,  3.89601973751E-04,  4.54242289050E-04,  5.29607209800E-04,  6.17476012564E-04, &
  7.19923128912E-04,  8.39367085119E-04,  9.78627543681E-04,  1.14099178031E-03,  1.33029214523E-03,  1.55099630479E-03, &
  1.80831234191E-03,  2.10831111434E-03,  2.45806862870E-03,  2.86583158466E-03,  3.34120966900E-03,  3.89539861933E-03, &
  4.54143849891E-03,  5.29451197347E-03,  6.17228756167E-03,  7.19531268313E-03,  8.38746058912E-03,  9.77643350230E-03, &
  1.13943208262E-02,  1.32782050079E-02,  1.54707967971E-02,  1.80210634703E-02,  2.09847837166E-02,  2.44249145050E-02, &
  2.84115778193E-02,  3.30213524808E-02,  3.83353639832E-02,  4.44353673090E-02,  5.13965627145E-02,  5.92752031985E-02, &
  6.80880607240E-02,  7.77794366644E-02,  8.81696149649E-02,  9.88766639298E-02,  1.09202052802E-01,  1.17971700371E-01, &
  1.23332521049E-01,  1.22530035854E-01,  1.11753240889E-01,  8.62569960973E-02,  4.11899187108E-02, -2.61456504772E-02, &
 -1.11691705121E-01, -1.97411432453E-01, -2.44254055664E-01, -1.95918893763E-01, -1.49300191739E-02,  2.33634698676E-01, &
  3.13582629541E-01, -4.47760615930E-03, -3.86535797015E-01, -3.87589109967E-03,  4.18653972543E-01, -4.16298788795E-01/
 DATA (WJ0(J9),J9= 20,109)/ &
  2.34448877498E-01, -9.52158343728E-02,  3.09020778713E-02, -8.49535839509E-03,  2.06835506815E-03, -4.67185821059E-04, &
  1.02086153218E-04, -2.20830053233E-05,  4.76413760468E-06, -1.02705545675E-06,  2.21421979164E-07, -4.77750910705E-08, &
  1.03340738634E-08, -2.25102276694E-09,  4.99715357680E-10, -1.16500471179E-10,  3.03986897639E-11, -9.72611811870E-12, &
  3.99994042396E-12, -2.00348565820E-12,  1.11608417099E-12, -6.50767639555E-13,  3.86180817012E-13, -2.30659587418E-13, &
  1.38093695980E-13, -8.27455585993E-14,  4.95961642994E-14, -2.97302965597E-14,  1.78224472343E-14, -1.06841897105E-14, &
  6.40498685290E-15, -3.83968417568E-15,  2.30182896520E-15, -1.37991039489E-15,  8.27234374391E-16, -4.95913890248E-16, &
  2.97292643817E-16, -1.78222228351E-16,  1.06841401468E-16, -6.40497544674E-17,  3.83968128138E-17, -2.30182807939E-17, &
  1.37991004842E-17, -8.27234560136E-18,  4.95913797287E-18, -2.97292590016E-18,  1.78222272891E-18, -1.06841382487E-18, &
  6.40497431324E-19, -3.83968224515E-19,  2.30182767120E-19, -1.37990980321E-19,  8.27234414081E-20, -4.95914134387E-20, &
  2.97292537295E-20, -1.78222241286E-20,  1.06841455108E-20, -6.40497317742E-21,  3.83968156424E-21, -2.30182923671E-21, &
  1.37990955793E-21, -8.27234267383E-22,  4.95914046240E-22, -2.97292739490E-22,  1.78222209690E-22, -1.06841436161E-22, &
  6.40497753124E-23, -3.83968088314E-23,  2.30182784256E-23, -1.37991049701E-23,  8.27234475022E-24, -4.95913958682E-24, &
  2.97292559305E-24, -1.78222330828E-24,  1.06841371450E-24, -6.40497639510E-25,  3.83968184851E-25, -2.30182842033E-25, &
  1.37990966066E-25, -8.27234682962E-26,  4.95914083158E-26, -2.97292634049E-26,  1.78222222810E-26, -1.06841489841E-26, &
  6.40497251344E-27, -3.83968281228E-27,  2.30182702533E-27, -1.37991000702E-27,  8.27234181627E-28, -4.95914207635E-28/
 DATA WJ0(110:150)/ &
  2.97292963477E-28, -1.78222420371E-28,  1.06841425086E-28, -6.40497412376E-29,  3.83968377606E-29, -2.30182957681E-29, &
  1.37991153609E-29, -8.27235098582E-30,  4.95914332316E-30, -2.97292528486E-30,  1.78222312353E-30, -1.06841451903E-30, &
  6.40498122076E-31, -3.83968474142E-31,  2.30183015458E-31, -1.37991188353E-31,  8.27234597206E-32, -4.95914031749E-32, &
  2.97292858145E-32, -1.78222357152E-32,  1.06841478804E-32, -6.40498282844E-33,  3.83968570659E-33, -2.30182876031E-33, &
  1.37991104718E-33, -8.27234805187E-34,  4.95914156225E-34, -2.97292932767E-34,  1.78222401887E-34, -1.06841414093E-34, &
  6.40497895409E-35, -3.83968338099E-35,  2.30182933903E-35, -1.37991139355E-35,  8.27235013127E-36, -4.95914281087E-36, &
  2.97292752582E-36, -1.78222294016E-36,  1.06841440910E-36, -6.40498056176E-37,  3.83968434477E-37/

!  J1 filter coefficients computed from the Niels Christensen program, FILCOA
!  for the following parameters:
!
!   ANY =  1      AMY =  0      NDEC = 15       NLO = -250        NHI =  150
!   IOPT = 1   ISHIFT = 0      OMEGA = .3 PI    EPS = 1.0E-12      SC = 3.257209
!      A = 0.162875             DEL0 = 0.14314998               ERROR =  1.4032E-08

 DATA (WJ1(J9),J9= -250,-161)/ &
  2.67560875879E-35,  3.63710586576E-35,  4.94412310292E-35,  6.72082533724E-35,  9.13599687416E-35,  1.24190757379E-34, &
  1.68819499732E-34,  2.29485865865E-34,  3.11953078380E-34,  4.24055410750E-34,  5.76442432690E-34,  7.83590704850E-34, &
  1.06517903247E-33,  1.44795792522E-33,  1.96829085937E-33,  2.67560875879E-33,  3.63710586576E-33,  4.94412310292E-33, &
  6.72082533724E-33,  9.13599687416E-33,  1.24190757379E-32,  1.68819499732E-32,  2.29485865865E-32,  3.11953078380E-32, &
  4.24055410750E-32,  5.76442432690E-32,  7.83590704850E-32,  1.06517903247E-31,  1.44795792522E-31,  1.96829085937E-31, &
  2.67560875879E-31,  3.63710586576E-31,  4.94412310292E-31,  6.72082533724E-31,  9.13599687416E-31,  1.24190757379E-30, &
  1.68819499732E-30,  2.29485865865E-30,  3.11953078380E-30,  4.24055410750E-30,  5.76442432690E-30,  7.83590704850E-30, &
  1.06517903247E-29,  1.44795792522E-29,  1.96829085937E-29,  2.67560875879E-29,  3.63710586576E-29,  4.94412310292E-29, &
  6.72082533724E-29,  9.13599687416E-29,  1.24190757379E-28,  1.68819499732E-28,  2.29485865865E-28,  3.11953078380E-28, &
  4.24055410750E-28,  5.76442432690E-28,  7.83590704850E-28,  1.06517903247E-27,  1.44795792522E-27,  1.96829085937E-27, &
  2.67560875879E-27,  3.63710586576E-27,  4.94412310292E-27,  6.72082533724E-27,  9.13599687416E-27,  1.24190757379E-26, &
  1.68819499732E-26,  2.29485865865E-26,  3.11953078380E-26,  4.24055410750E-26,  5.76442432690E-26,  7.83590704850E-26, &
  1.06517903247E-25,  1.44795792522E-25,  1.96829085937E-25,  2.67560875879E-25,  3.63710586576E-25,  4.94412310292E-25, &
  6.72082533724E-25,  9.13599687416E-25,  1.24190757379E-24,  1.68819499732E-24,  2.29485865865E-24,  3.11953078380E-24, &
  4.24055410750E-24,  5.76442432690E-24,  7.83590704850E-24,  1.06517903247E-23,  1.44795792522E-23,  1.96829085937E-23/
 DATA (WJ1(J9),J9= -160,-71)/ &
  2.67560875879E-23,  3.63710586576E-23,  4.94412310292E-23,  6.72082533724E-23,  9.13599687416E-23,  1.24190757379E-22, &
  1.68819499732E-22,  2.29485865865E-22,  3.11953078380E-22,  4.24055410750E-22,  5.76442432690E-22,  7.83590704850E-22, &
  1.06517903247E-21,  1.44795792522E-21,  1.96829085937E-21,  2.67560875879E-21,  3.63710586576E-21,  4.94412310292E-21, &
  6.72082533724E-21,  9.13599687416E-21,  1.24190757379E-20,  1.68819499732E-20,  2.29485865865E-20,  3.11953078380E-20, &
  4.24055410750E-20,  5.76442432690E-20,  7.83590704850E-20,  1.06517903247E-19,  1.44795792522E-19,  1.96829085937E-19, &
  2.67560875879E-19,  3.63710586576E-19,  4.94412310292E-19,  6.72082533724E-19,  9.13599687416E-19,  1.24190757379E-18, &
  1.68819499732E-18,  2.29485865865E-18,  3.11953078380E-18,  4.24055410750E-18,  5.76442432690E-18,  7.83590704850E-18, &
  1.06517903247E-17,  1.44795792522E-17,  1.96829085937E-17,  2.67560875879E-17,  3.63710586576E-17,  4.94412310292E-17, &
  6.72082533724E-17,  9.13599687416E-17,  1.24190757379E-16,  1.68819499732E-16,  2.29485865865E-16,  3.11953078380E-16, &
  4.24055410750E-16,  5.76442432690E-16,  7.83590704850E-16,  1.06517903247E-15,  1.44795792522E-15,  1.96829085937E-15, &
  2.67560875879E-15,  3.63710586576E-15,  4.94412310292E-15,  6.72082533724E-15,  9.13599687416E-15,  1.24190757379E-14, &
  1.68819499732E-14,  2.29485865865E-14,  3.11953078380E-14,  4.24055410750E-14,  5.76442432690E-14,  7.83590704849E-14, &
  1.06517903247E-13,  1.44795792522E-13,  1.96829085938E-13,  2.67560875878E-13,  3.63710586577E-13,  4.94412310288E-13, &
  6.72082533728E-13,  9.13599687406E-13,  1.24190757380E-12,  1.68819499729E-12,  2.29485865868E-12,  3.11953078372E-12, &
  4.24055410758E-12,  5.76442432666E-12,  7.83590704871E-12,  1.06517903240E-11,  1.44795792527E-11,  1.96829085917E-11/
 DATA (WJ1(J9),J9= -70,19)/ &
  2.67560875891E-11,  3.63710586515E-11,  4.94412310317E-11,  6.72082533541E-11,  9.13599687462E-11,  1.24190757324E-10, &
  1.68819499736E-10,  2.29485865695E-10,  3.11953078363E-10,  4.24055410221E-10,  5.76442432542E-10,  7.83590703194E-10, &
  1.06517903172E-09,  1.44795791998E-09,  1.96829085611E-09,  2.67560874206E-09,  3.63710585268E-09,  4.94412304898E-09, &
  6.72082528725E-09,  9.13599669890E-09,  1.24190755523E-08,  1.68819493996E-08,  2.29485859113E-08,  3.11953059487E-08, &
  4.24055386543E-08,  5.76442370102E-08,  7.83590618983E-08,  1.06517882412E-07,  1.44795762309E-07,  1.96829016283E-07, &
  2.67560770231E-07,  3.63710352883E-07,  4.94411942636E-07,  6.72081747305E-07,  9.13598412795E-07,  1.24190492063E-06, &
  1.68819059152E-06,  2.29484968860E-06,  3.11951559104E-06,  4.24052372735E-06,  5.76437203602E-06,  7.83580400571E-06, &
  1.06516106220E-05,  1.44792293329E-05,  1.96822917833E-05,  2.67548981332E-05,  3.63689436167E-05,  4.94371845248E-05, &
  6.72010067340E-05,  9.13461935181E-05,  1.24165945005E-04,  1.68772580859E-04,  2.29400955289E-04,  3.11793204874E-04, &
  4.23764974965E-04,  5.75897507579E-04,  7.82597702990E-04,  1.06332133421E-03,  1.44456435715E-03,  1.96195766368E-03, &
  2.66401748131E-03,  3.61551958902E-03,  4.90456094796E-03,  6.64729428357E-03,  9.00112880743E-03,  1.21689223295E-02, &
  1.64231258930E-02,  2.20996958736E-02,  2.96400942278E-02,  3.95385050500E-02,  5.24078149405E-02,  6.87615215337E-02, &
  8.91013723344E-02,  1.13192375541E-01,  1.40192739735E-01,  1.66618485339E-01,  1.87030308669E-01,  1.89612379729E-01, &
  1.61380285157E-01,  8.29859362099E-02, -4.46335736689E-02, -2.01737898138E-01, -2.84006740802E-01, -1.90854624427E-01, &
  1.45861570853E-01,  3.42338340245E-01,  5.72930699760E-02, -4.71068534718E-01,  2.63969067746E-01,  8.25956507901E-02/
 DATA (WJ1(J9),J9= 20,109)/ &
 -2.22236420794E-01,  2.04428998525E-01, -1.44401888321E-01,  9.24618900674E-02, -5.69896615248E-02,  3.45697730305E-02, &
 -2.08227940873E-02,  1.25054653306E-02, -7.50178808640E-03,  4.49828025678E-03, -2.69688071237E-03,  1.61678766116E-03, &
 -9.69249547051E-04,  5.81052166908E-04, -3.48332124427E-04,  2.08819730575E-04, -1.25184162926E-04,  7.50459390809E-05, &
 -4.49888596104E-05,  2.69701130091E-05, -1.61681580285E-05,  9.69255610555E-06, -5.81053473294E-06,  3.48332405883E-06, &
 -2.08819791213E-06,  1.25184175990E-06, -7.50459418954E-07,  4.49888602168E-07, -2.69701131398E-07,  1.61681580566E-07, &
 -9.69255611161E-08,  5.81053473425E-08, -3.48332405911E-08,  2.08819791219E-08, -1.25184175991E-08,  7.50459418957E-09, &
 -4.49888602168E-09,  2.69701131398E-09, -1.61681580566E-09,  9.69255611161E-10, -5.81053473425E-10,  3.48332405911E-10, &
 -2.08819791219E-10,  1.25184175991E-10, -7.50459418957E-11,  4.49888602168E-11, -2.69701131398E-11,  1.61681580566E-11, &
 -9.69255611161E-12,  5.81053473425E-12, -3.48332405911E-12,  2.08819791219E-12, -1.25184175991E-12,  7.50459418957E-13, &
 -4.49888602168E-13,  2.69701131398E-13, -1.61681580566E-13,  9.69255611161E-14, -5.81053473425E-14,  3.48332405911E-14, &
 -2.08819791219E-14,  1.25184175991E-14, -7.50459418957E-15,  4.49888602168E-15, -2.69701131398E-15,  1.61681580566E-15, &
 -9.69255611161E-16,  5.81053473425E-16, -3.48332405911E-16,  2.08819791219E-16, -1.25184175991E-16,  7.50459418957E-17, &
 -4.49888602168E-17,  2.69701131398E-17, -1.61681580566E-17,  9.69255611161E-18, -5.81053473425E-18,  3.48332405911E-18, &
 -2.08819791219E-18,  1.25184175991E-18, -7.50459418957E-19,  4.49888602168E-19, -2.69701131398E-19,  1.61681580566E-19, &
 -9.69255611161E-20,  5.81053473425E-20, -3.48332405911E-20,  2.08819791219E-20, -1.25184175991E-20,  7.50459418957E-21/
 DATA WJ1(110:150)/ &
 -4.49888602168E-21,  2.69701131398E-21, -1.61681580566E-21,  9.69255611161E-22, -5.81053473425E-22,  3.48332405911E-22, &
 -2.08819791219E-22,  1.25184175991E-22, -7.50459418957E-23,  4.49888602168E-23, -2.69701131398E-23,  1.61681580566E-23, &
 -9.69255611161E-24,  5.81053473425E-24, -3.48332405911E-24,  2.08819791219E-24, -1.25184175991E-24,  7.50459418957E-25, &
 -4.49888602168E-25,  2.69701131398E-25, -1.61681580566E-25,  9.69255611161E-26, -5.81053473425E-26,  3.48332405911E-26, &
 -2.08819791219E-26,  1.25184175991E-26, -7.50459418957E-27,  4.49888602168E-27, -2.69701131398E-27,  1.61681580566E-27, &
 -9.69255611161E-28,  5.81053473425E-28, -3.48332405911E-28,  2.08819791219E-28, -1.25184175991E-28,  7.50459418957E-29, &
 -4.49888602168E-29,  2.69701131398E-29, -1.61681580566E-29,  9.69255611161E-30, -5.81053473425E-30/

!  Niels Christensen shifted cosine filter:
!  12 points per decade, OMEGA = .3 PI

  DATA DELCOS /.00632173 /
  DATA (WCOS (J9), J9 = -200, -21)/ &
  3.27764748749E-18,  3.97096058632E-18,  4.81092858166E-18,  5.82857304036E-18,  7.06147744874E-18,  8.55517523993E-18, &
  1.03648314276E-17,  1.25572799515E-17,  1.52134919784E-17,  1.84315663162E-17,  2.23303523839E-17,  2.70538395400E-17, &
  3.27764748749E-17,  3.97096058632E-17,  4.81092858166E-17,  5.82857304036E-17,  7.06147744874E-17,  8.55517523993E-17, &
  1.03648314276E-16,  1.25572799515E-16,  1.52134919784E-16,  1.84315663162E-16,  2.23303523839E-16,  2.70538395400E-16, &
  3.27764748749E-16,  3.97096058632E-16,  4.81092858166E-16,  5.82857304036E-16,  7.06147744874E-16,  8.55517523993E-16, &
  1.03648314276E-15,  1.25572799515E-15,  1.52134919784E-15,  1.84315663162E-15,  2.23303523839E-15,  2.70538395400E-15, &
  3.27764748749E-15,  3.97096058632E-15,  4.81092858166E-15,  5.82857304036E-15,  7.06147744874E-15,  8.55517523993E-15, &
  1.03648314276E-14,  1.25572799515E-14,  1.52134919784E-14,  1.84315663162E-14,  2.23303523839E-14,  2.70538395400E-14, &
  3.27764748749E-14,  3.97096058632E-14,  4.81092858166E-14,  5.82857304036E-14,  7.06147744874E-14,  8.55517523993E-14, &
  1.03648314276E-13,  1.25572799515E-13,  1.52134919784E-13,  1.84315663162E-13,  2.23303523839E-13,  2.70538395400E-13, &
  3.27764748749E-13,  3.97096058632E-13,  4.81092858166E-13,  5.82857304036E-13,  7.06147744874E-13,  8.55517523993E-13, &
  1.03648314276E-12,  1.25572799515E-12,  1.52134919784E-12,  1.84315663162E-12,  2.23303523839E-12,  2.70538395400E-12, &
  3.27764748749E-12,  3.97096058632E-12,  4.81092858166E-12,  5.82857304036E-12,  7.06147744874E-12,  8.55517523993E-12, &
  1.03648314276E-11,  1.25572799515E-11,  1.52134919784E-11,  1.84315663162E-11,  2.23303523839E-11,  2.70538395400E-11, &
  3.27764748749E-11,  3.97096058632E-11,  4.81092858166E-11,  5.82857304036E-11,  7.06147744874E-11,  8.55517523993E-11, &
  1.03648314276E-10,  1.25572799515E-10,  1.52134919784E-10,  1.84315663162E-10,  2.23303523839E-10,  2.70538395400E-10, &
  3.27764748749E-10,  3.97096058632E-10,  4.81092858166E-10,  5.82857304036E-10,  7.06147744874E-10,  8.55517523993E-10, &
  1.03648314276E-09,  1.25572799515E-09,  1.52134919784E-09,  1.84315663162E-09,  2.23303523839E-09,  2.70538395400E-09, &
  3.27764748749E-09,  3.97096058632E-09,  4.81092858166E-09,  5.82857304036E-09,  7.06147744874E-09,  8.55517523993E-09, &
  1.03648314276E-08,  1.25572799515E-08,  1.52134919784E-08,  1.84315663162E-08,  2.23303523839E-08,  2.70538395400E-08, &
  3.27764748749E-08,  3.97096058632E-08,  4.81092858166E-08,  5.82857304036E-08,  7.06147744874E-08,  8.55517523992E-08, &
  1.03648314276E-07,  1.25572799515E-07,  1.52134919784E-07,  1.84315663162E-07,  2.23303523839E-07,  2.70538395400E-07, &
  3.27764748748E-07,  3.97096058631E-07,  4.81092858163E-07,  5.82857304032E-07,  7.06147744866E-07,  8.55517523979E-07, &
  1.03648314273E-06,  1.25572799511E-06,  1.52134919777E-06,  1.84315663149E-06,  2.23303523815E-06,  2.70538395358E-06, &
  3.27764748674E-06,  3.97096058499E-06,  4.81092857928E-06,  5.82857303614E-06,  7.06147744122E-06,  8.55517522657E-06, &
  1.03648314038E-05,  1.25572799093E-05,  1.52134919033E-05,  1.84315661826E-05,  2.23303521464E-05,  2.70538391177E-05, &
  3.27764741237E-05,  3.97096045276E-05,  4.81092834413E-05,  5.82857261799E-05,  7.06147669760E-05,  8.55517390427E-05, &
  1.03648290523E-04,  1.25572757278E-04,  1.52134844670E-04,  1.84315529598E-04,  2.23303286305E-04,  2.70537973035E-04, &
  3.27763997594E-04,  3.97094723005E-04,  4.81090482791E-04,  5.82853080445E-04,  7.06140233231E-04,  8.55504167951E-04, &
  1.03645938870E-03,  1.25568576016E-03,  1.52127408052E-03,  1.84302307509E-03,  2.23279769616E-03,  2.70496162210E-03/

  DATA (WCOS (J9), J9= -20, 99)/ &
  3.27689631886E-03,  3.96962511374E-03,  4.80855324839E-03,  5.82435024343E-03,  7.05396657593E-03,  8.54182367870E-03, &
  1.03410843805E-02,  1.25150721296E-02,  1.51384287367E-02,  1.82981828574E-02,  2.20932012652E-02,  2.66326428704E-02, &
  3.20280504750E-02,  3.83817031757E-02,  4.57529090015E-02,  5.41138165506E-02,  6.32336060872E-02,  7.25429239280E-02, &
  8.07814005943E-02,  8.56648215301E-02,  8.29754131995E-02,  6.61728839009E-02,  2.49099879313E-02, -5.25662370332E-02, &
 -1.77257695902E-01, -3.38275600250E-01, -4.82415902998E-01, -4.55992280486E-01, -7.52812327135E-02,  6.65970979261E-01, &
  8.99170503986E-01, -3.96592370781E-01, -1.38198747238E+00,  1.66395693227E+00, -9.30334922154E-01,  3.30012032268E-01, &
 -8.19311720454E-02,  1.48662188728E-02, -2.13960121462E-03,  2.89777944084E-04, -4.10252655190E-05,  5.96303531789E-06, &
 -8.72916816254E-07,  1.28031659199E-07, -1.87886052472E-08,  2.75763186999E-09, -4.04758530392E-10,  5.94101668614E-11, &
 -8.72020580969E-12,  1.27995006152E-12, -1.87869546474E-13,  2.75750390141E-14, -4.04729332639E-15,  5.94004630834E-16, &
 -8.70764639675E-17,  1.27459963186E-17, -1.82944370627E-18,  2.67836880337E-19, -3.04833935943E-20,  1.64313000801E-21, &
  3.01142825752E-21, -5.21478596825E-22,  1.37002813677E-21, -6.52797182652E-22,  1.40079856288E-22, -1.40667671784E-22, &
  1.70033730143E-23, -2.74453364807E-23,  2.41787117103E-23, -1.78716987481E-23,  4.99883433782E-24, -4.06084044984E-24, &
  2.89670334941E-24, -8.77965537372E-25,  1.21194987045E-25, -1.74181776862E-25,  1.50307641169E-25, -1.09826064382E-25, &
  3.14586965779E-26, -2.51308231025E-26,  1.77594485992E-26, -1.17543940755E-26,  8.42024121640E-28, -1.10510759608E-27, &
  9.31619291992E-28, -6.75339996352E-28,  1.97531071217E-28, -1.55371775135E-28,  1.08953022579E-28, -7.17780762223E-29, &
  2.55398099963E-29, -6.99012347840E-30,  5.76787420019E-30, -4.15016624873E-30,  1.23507827864E-30, -9.59703688264E-31, &
  6.68070421281E-31, -4.37770918800E-31,  1.57257106203E-31, -1.06708053061E-31,  3.57322505765E-32, -2.54887457918E-32, &
  7.72541668811E-33, -5.92277283725E-33,  4.09438835539E-33, -1.32259081936E-33,  1.67919911757E-33, -2.76812163102E-34, &
  2.21131777864E-34,  5.28010221339E-35,  1.03429563330E-34, -7.40916006860E-36,  9.72409086858E-36, -8.19752817047E-36, &
 -2.58911797964E-36, -3.98829026336E-36,  1.78104494324E-37, -3.32579083872E-37,  3.00732538418E-37, -2.24730545742E-37/

! Niels Christensen sine filter computed for the following parameters :
!
!    ANY =  0.50          AMY =  0.50     NDEC_SN = 12         SC = 2.605767
!      A = 0.203593       EPS = 1.00E-12     IOPT = 1       OMEGA = 0.30 PI
! ISHIFT = 0             DEL0 = 0.00000      SNLO = -112     SNHI =  85
!  ERROR = 5.3494E-07

 DATA (WSIN(J9),J9= SNLO,SNHI)/ &
  3.29843470141E-20,  4.84144003903E-20,  7.10626214352E-20,  1.04305663698E-19,  1.53099776785E-19,  2.24719740238E-19, &
  3.29843470141E-19,  4.84144003903E-19,  7.10626214352E-19,  1.04305663698E-18,  1.53099776785E-18,  2.24719740238E-18, &
  3.29843470141E-18,  4.84144003903E-18,  7.10626214352E-18,  1.04305663698E-17,  1.53099776785E-17,  2.24719740238E-17, &
  3.29843470141E-17,  4.84144003903E-17,  7.10626214352E-17,  1.04305663698E-16,  1.53099776785E-16,  2.24719740238E-16, &
  3.29843470141E-16,  4.84144003903E-16,  7.10626214352E-16,  1.04305663698E-15,  1.53099776785E-15,  2.24719740238E-15, &
  3.29843470141E-15,  4.84144003903E-15,  7.10626214351E-15,  1.04305663698E-14,  1.53099776784E-14,  2.24719740238E-14, &
  3.29843470140E-14,  4.84144003905E-14,  7.10626214348E-14,  1.04305663699E-13,  1.53099776783E-13,  2.24719740240E-13, &
  3.29843470135E-13,  4.84144003912E-13,  7.10626214333E-13,  1.04305663701E-12,  1.53099776778E-12,  2.24719740250E-12, &
  3.29843470115E-12,  4.84144003946E-12,  7.10626214259E-12,  1.04305663713E-11,  1.53099776751E-11,  2.24719740291E-11, &
  3.29843470017E-11,  4.84144004090E-11,  7.10626213893E-11,  1.04305663763E-10,  1.53099776614E-10,  2.24719740458E-10, &
  3.29843469501E-10,  4.84144004636E-10,  7.10626211927E-10,  1.04305663935E-09,  1.53099775856E-09,  2.24719740965E-09, &
  3.29843466535E-09,  4.84144005941E-09,  7.10626200153E-09,  1.04305664163E-08,  1.53099771106E-08,  2.24719740669E-08, &
  3.29843447058E-08,  4.84143999692E-08,  7.10626118987E-08,  1.04305659511E-07,  1.53099736752E-07,  2.24719712781E-07, &
  3.29843299537E-07,  4.84143847630E-07,  7.10625477136E-07,  1.04305580801E-06,  1.53099454243E-06,  2.24719318225E-06, &
  3.29842043629E-06,  4.84141911268E-06,  7.10619846782E-06,  1.04304644290E-05,  1.53096912404E-05,  2.24714835740E-05, &
  3.29830502601E-05,  4.84120618278E-05,  7.10567202638E-05,  1.04294586029E-04,  1.53072809860E-04,  2.24667522134E-04, &
  3.29719828517E-04,  4.83898767299E-04,  7.10057855007E-04,  1.04190820377E-03,  1.52838000422E-03,  2.24183176974E-03, &
  3.28636154488E-03,  4.81642274472E-03,  7.05054547250E-03,  1.03141810945E-02,  1.50529568277E-02,  2.19321837975E-02, &
  3.18014238705E-02,  4.59240858792E-02,  6.56510206738E-02,  9.29354313249E-02,  1.28683857774E-01,  1.73917366767E-01, &
  2.23007141217E-01,  2.67404509859E-01,  2.73366070252E-01,  2.06068241098E-01, -1.67534380110E-02, -3.60408605065E-01, &
 -7.33064377532E-01, -4.81827612364E-01,  4.40141743865E-01,  1.32831422063E+00, -1.13994567799E+00, -2.46268092208E-01, &
  9.29146388411E-01, -8.57560517297E-01,  5.73597714499E-01, -3.36064823205E-01,  1.85046743779E-01, -9.90581893402E-02, &
  5.24802391849E-02, -2.77162017159E-02,  1.46248394020E-02, -7.71564221998E-03,  4.07025589629E-03, -2.14707450654E-03, &
  1.13258347863E-03, -5.97437791024E-04,  3.15148309040E-04, -1.66240647777E-04,  8.76918939599E-05, -4.62574484721E-05, &
  2.44007905095E-05, -1.28714098324E-05,  6.78966490764E-06, -3.58154624520E-06,  1.88926459271E-06, -9.96586517910E-07, &
  5.25699095571E-07, -2.77306118553E-07,  1.46278896111E-07, -7.71620747468E-08,  4.07029717719E-08, -2.14708056581E-08, &
  1.13258436802E-08, -5.97437921568E-09,  3.15148328202E-09, -1.66240650589E-09,  8.76918943727E-10, -4.62574485326E-10, &
  2.44007905184E-10, -1.28714098337E-10,  6.78966490783E-11, -3.58154624523E-11,  1.88926459271E-11, -9.96586517911E-12, &
  5.25699095571E-12, -2.77306118553E-12,  1.46278896111E-12, -7.71620747468E-13,  4.07029717719E-13, -2.14708056581E-13, &
  1.13258436802E-13, -5.97437921568E-14,  3.15148328202E-14, -1.66240650589E-14,  8.76918943727E-15, -4.62574485326E-15, &
  2.44007905184E-15, -1.28714098337E-15,  6.78966490783E-16, -3.58154624523E-16,  1.88926459271E-16, -9.96586517911E-17, &
  5.25699095571E-17, -2.77306118553E-17,  1.46278896111E-17, -7.71620747468E-18,  4.07029717719E-18, -2.14708056581E-18, &
  1.13258436802E-18, -5.97437921568E-19,  3.15148328202E-19, -1.66240650589E-19,  8.76918943727E-20, -4.62574485326E-20/

! Niels Christensen shifted cosine and sine filters:
! 12 points per decade, OMEGA = .3 PI, shift = 0

 DATA (WCOS1(LL), LL = NLO, -65)/ &
  2.24719660091E-19,  2.72254083638E-19,  3.29843352502E-19,  3.99614344570E-19,  4.84143831232E-19,  5.86553642292E-19, &
  7.10625960906E-19,  8.60943006577E-19,  1.04305626497E-18,  1.26369151452E-18,  1.53099722181E-18,  1.85484547951E-18, &
  2.24719660091E-18,  2.72254083638E-18,  3.29843352502E-18,  3.99614344570E-18,  4.84143831232E-18,  5.86553642292E-18, &
  7.10625960906E-18,  8.60943006577E-18,  1.04305626497E-17,  1.26369151452E-17,  1.53099722181E-17,  1.85484547951E-17, &
  2.24719660091E-17,  2.72254083638E-17,  3.29843352502E-17,  3.99614344570E-17,  4.84143831232E-17,  5.86553642292E-17, &
  7.10625960906E-17,  8.60943006577E-17,  1.04305626497E-16,  1.26369151452E-16,  1.53099722181E-16,  1.85484547951E-16, &
  2.24719660091E-16,  2.72254083638E-16,  3.29843352502E-16,  3.99614344570E-16,  4.84143831232E-16,  5.86553642292E-16, &
  7.10625960906E-16,  8.60943006577E-16,  1.04305626497E-15,  1.26369151452E-15,  1.53099722181E-15,  1.85484547951E-15, &
  2.24719660091E-15,  2.72254083638E-15,  3.29843352502E-15,  3.99614344570E-15,  4.84143831232E-15,  5.86553642292E-15, &
  7.10625960906E-15,  8.60943006577E-15,  1.04305626497E-14,  1.26369151452E-14,  1.53099722181E-14,  1.85484547951E-14, &
  2.24719660091E-14,  2.72254083638E-14,  3.29843352502E-14,  3.99614344570E-14,  4.84143831232E-14,  5.86553642292E-14, &
  7.10625960906E-14,  8.60943006577E-14,  1.04305626497E-13,  1.26369151452E-13,  1.53099722181E-13,  1.85484547951E-13, &
  2.24719660091E-13,  2.72254083638E-13,  3.29843352502E-13,  3.99614344570E-13,  4.84143831232E-13,  5.86553642292E-13, &
  7.10625960906E-13,  8.60943006577E-13,  1.04305626497E-12,  1.26369151452E-12,  1.53099722181E-12,  1.85484547951E-12, &
  2.24719660091E-12,  2.72254083638E-12,  3.29843352502E-12,  3.99614344570E-12,  4.84143831232E-12,  5.86553642292E-12, &
  7.10625960906E-12,  8.60943006577E-12,  1.04305626497E-11,  1.26369151452E-11,  1.53099722181E-11,  1.85484547951E-11, &
  2.24719660091E-11,  2.72254083638E-11,  3.29843352502E-11,  3.99614344570E-11,  4.84143831232E-11,  5.86553642292E-11, &
  7.10625960906E-11,  8.60943006577E-11,  1.04305626497E-10,  1.26369151452E-10,  1.53099722181E-10,  1.85484547951E-10, &
  2.24719660091E-10,  2.72254083638E-10,  3.29843352502E-10,  3.99614344570E-10,  4.84143831232E-10,  5.86553642292E-10, &
  7.10625960906E-10,  8.60943006577E-10,  1.04305626497E-09,  1.26369151452E-09,  1.53099722181E-09,  1.85484547951E-09, &
  2.24719660091E-09,  2.72254083638E-09,  3.29843352502E-09,  3.99614344570E-09,  4.84143831232E-09,  5.86553642292E-09, &
  7.10625960906E-09,  8.60943006577E-09,  1.04305626497E-08,  1.26369151452E-08,  1.53099722181E-08,  1.85484547951E-08, &
  2.24719660091E-08,  2.72254083638E-08,  3.29843352502E-08,  3.99614344570E-08,  4.84143831232E-08,  5.86553642292E-08, &
  7.10625960906E-08,  8.60943006577E-08,  1.04305626497E-07,  1.26369151452E-07,  1.53099722181E-07,  1.85484547951E-07, &
  2.24719660091E-07,  2.72254083637E-07,  3.29843352501E-07,  3.99614344569E-07,  4.84143831229E-07,  5.86553642287E-07/

 DATA (WCOS1(LL), LL = -64, NHI)/ &
  7.10625960898E-07,  8.60943006563E-07,  1.04305626495E-06,  1.26369151447E-06,  1.53099722174E-06,  1.85484547937E-06, &
  2.24719660067E-06,  2.72254083595E-06,  3.29843352425E-06,  3.99614344434E-06,  4.84143830990E-06,  5.86553641861E-06, &
  7.10625960140E-06,  8.60943005216E-06,  1.04305626255E-05,  1.26369151021E-05,  1.53099721416E-05,  1.85484546590E-05, &
  2.24719657670E-05,  2.72254079333E-05,  3.29843344846E-05,  3.99614330958E-05,  4.84143807024E-05,  5.86553599245E-05, &
  7.10625884355E-05,  8.60942870452E-05,  1.04305602289E-04,  1.26369108405E-04,  1.53099645630E-04,  1.85484411827E-04, &
  2.24719418014E-04,  2.72253653176E-04,  3.29842586984E-04,  3.99612983335E-04,  4.84141410447E-04,  5.86549337704E-04, &
  7.10618305692E-04,  8.60929394349E-04,  1.04303205701E-03,  1.26364846920E-03,  1.53092066949E-03,  1.85470935984E-03, &
  2.24695452222E-03,  2.72211039676E-03,  3.29766801610E-03,  3.99478232651E-03,  4.83901765165E-03,  5.86123250049E-03, &
  7.09860548309E-03,  8.59582190588E-03,  1.04063629744E-02,  1.25938958734E-02,  1.52334795010E-02,  1.84125065675E-02, &
  2.22303045947E-02,  2.67961146198E-02,  3.22217075582E-02,  3.86080352953E-02,  4.60134626346E-02,  5.44036223886E-02, &
  6.35429593408E-02,  7.28386350724E-02,  8.10164405210E-02,  8.57185943809E-02,  8.27059125063E-02,  6.52412472777E-02, &
  2.30534830311E-02, -5.59882081138E-02, -1.81947463227E-01, -3.44153844469E-01, -4.84698754829E-01, -4.51282157764E-01, &
 -5.26307445100E-02,  6.84734610421E-01,  8.91769297901E-01, -4.76739927041E-01, -1.30978689417E+00,  1.68212186387E+00, &
 -1.00628128630E+00,  4.07691417517E-01, -1.37820955847E-01,  4.89713716333E-02, -2.12356283595E-02,  1.05629577712E-02, &
 -5.49082413602E-03,  2.88529446687E-03, -1.52040819919E-03,  8.02064953282E-04, -4.22814103793E-04,  2.23029196843E-04, &
 -1.17647084589E-04,  6.20586923092E-05, -3.27359257860E-05,  1.72681888643E-05, -9.10896420677E-06,  4.80497589323E-06, &
 -2.53462336528E-06,  1.33701307984E-06, -7.05274006868E-07,  3.72031831510E-07, -1.96246681878E-07,  1.03520067066E-07, &
 -5.46068049809E-08,  2.88050735932E-08, -1.51946678624E-08,  8.01518283581E-09, -4.22800659239E-09,  2.23027223602E-09, &
 -1.17646794963E-09,  6.20586497981E-10, -3.27359195463E-10,  1.72681879484E-10, -9.10896407233E-11,  4.80497587349E-11, &
 -2.53462336238E-11,  1.33701307942E-11, -7.05274006806E-12,  3.72031831501E-12, -1.96246681877E-12,  1.03520067066E-12, &
 -5.46068049808E-13,  2.88050735932E-13, -1.51946678624E-13,  8.01518283581E-14, -4.22800659239E-14,  2.23027223602E-14, &
 -1.17646794963E-14,  6.20586497982E-15, -3.27359195463E-15,  1.72681879484E-15, -9.10896407233E-16,  4.80497587349E-16, &
 -2.53462336238E-16,  1.33701307942E-16, -7.05274006806E-17,  3.72031831500E-17, -1.96246681877E-17,  1.03520067066E-17, &
 -5.46068049809E-18,  2.88050735932E-18, -1.51946678624E-18,  8.01518283581E-19, -4.22800659239E-19,  2.23027223602E-19, &
 -1.17646794963E-19,  6.20586497982E-20, -3.27359195463E-20,  1.72681879484E-20, -9.10896407234E-21,  4.80497587349E-21/

 DATA (WSIN1(LL), LL = NLO, -65)/ &
  3.29843470086E-37,  4.84144003957E-37,  7.10626214297E-37,  1.04305663703E-36,  1.53099776779E-36,  2.24719740243E-36, &
  3.29843470135E-36,  4.84144003908E-36,  7.10626214346E-36,  1.04305663698E-35,  1.53099776784E-35,  2.24719740238E-35, &
  3.29843470140E-35,  4.84144003903E-35,  7.10626214351E-35,  1.04305663698E-34,  1.53099776785E-34,  2.24719740238E-34, &
  3.29843470141E-34,  4.84144003903E-34,  7.10626214352E-34,  1.04305663698E-33,  1.53099776785E-33,  2.24719740238E-33, &
  3.29843470141E-33,  4.84144003903E-33,  7.10626214352E-33,  1.04305663698E-32,  1.53099776785E-32,  2.24719740238E-32, &
  3.29843470141E-32,  4.84144003903E-32,  7.10626214352E-32,  1.04305663698E-31,  1.53099776785E-31,  2.24719740238E-31, &
  3.29843470141E-31,  4.84144003903E-31,  7.10626214352E-31,  1.04305663698E-30,  1.53099776785E-30,  2.24719740238E-30, &
  3.29843470141E-30,  4.84144003903E-30,  7.10626214352E-30,  1.04305663698E-29,  1.53099776785E-29,  2.24719740238E-29, &
  3.29843470141E-29,  4.84144003903E-29,  7.10626214352E-29,  1.04305663698E-28,  1.53099776785E-28,  2.24719740238E-28, &
  3.29843470141E-28,  4.84144003903E-28,  7.10626214352E-28,  1.04305663698E-27,  1.53099776785E-27,  2.24719740238E-27, &
  3.29843470141E-27,  4.84144003903E-27,  7.10626214352E-27,  1.04305663698E-26,  1.53099776785E-26,  2.24719740238E-26, &
  3.29843470141E-26,  4.84144003903E-26,  7.10626214352E-26,  1.04305663698E-25,  1.53099776785E-25,  2.24719740238E-25, &
  3.29843470141E-25,  4.84144003903E-25,  7.10626214352E-25,  1.04305663698E-24,  1.53099776785E-24,  2.24719740238E-24, &
  3.29843470141E-24,  4.84144003903E-24,  7.10626214352E-24,  1.04305663698E-23,  1.53099776785E-23,  2.24719740238E-23, &
  3.29843470141E-23,  4.84144003903E-23,  7.10626214352E-23,  1.04305663698E-22,  1.53099776785E-22,  2.24719740238E-22, &
  3.29843470141E-22,  4.84144003903E-22,  7.10626214352E-22,  1.04305663698E-21,  1.53099776785E-21,  2.24719740238E-21, &
  3.29843470141E-21,  4.84144003903E-21,  7.10626214352E-21,  1.04305663698E-20,  1.53099776785E-20,  2.24719740238E-20, &
  3.29843470141E-20,  4.84144003903E-20,  7.10626214352E-20,  1.04305663698E-19,  1.53099776785E-19,  2.24719740238E-19, &
  3.29843470141E-19,  4.84144003903E-19,  7.10626214352E-19,  1.04305663698E-18,  1.53099776785E-18,  2.24719740238E-18, &
  3.29843470141E-18,  4.84144003903E-18,  7.10626214352E-18,  1.04305663698E-17,  1.53099776785E-17,  2.24719740238E-17, &
  3.29843470141E-17,  4.84144003903E-17,  7.10626214352E-17,  1.04305663698E-16,  1.53099776785E-16,  2.24719740238E-16, &
  3.29843470141E-16,  4.84144003903E-16,  7.10626214352E-16,  1.04305663698E-15,  1.53099776785E-15,  2.24719740238E-15, &
  3.29843470141E-15,  4.84144003903E-15,  7.10626214351E-15,  1.04305663698E-14,  1.53099776784E-14,  2.24719740238E-14, &
  3.29843470140E-14,  4.84144003905E-14,  7.10626214348E-14,  1.04305663699E-13,  1.53099776783E-13,  2.24719740240E-13, &
  3.29843470135E-13,  4.84144003912E-13,  7.10626214333E-13,  1.04305663701E-12,  1.53099776778E-12,  2.24719740250E-12/

 DATA (WSIN1(LL), LL = -64 ,NHI)/ &
  3.29843470115E-12,  4.84144003946E-12,  7.10626214259E-12,  1.04305663713E-11,  1.53099776751E-11,  2.24719740291E-11, &
  3.29843470017E-11,  4.84144004090E-11,  7.10626213893E-11,  1.04305663763E-10,  1.53099776614E-10,  2.24719740458E-10, &
  3.29843469501E-10,  4.84144004636E-10,  7.10626211927E-10,  1.04305663935E-09,  1.53099775856E-09,  2.24719740965E-09, &
  3.29843466535E-09,  4.84144005941E-09,  7.10626200153E-09,  1.04305664163E-08,  1.53099771106E-08,  2.24719740669E-08, &
  3.29843447058E-08,  4.84143999692E-08,  7.10626118987E-08,  1.04305659511E-07,  1.53099736752E-07,  2.24719712781E-07, &
  3.29843299537E-07,  4.84143847630E-07,  7.10625477136E-07,  1.04305580801E-06,  1.53099454243E-06,  2.24719318225E-06, &
  3.29842043629E-06,  4.84141911268E-06,  7.10619846782E-06,  1.04304644290E-05,  1.53096912404E-05,  2.24714835740E-05, &
  3.29830502601E-05,  4.84120618278E-05,  7.10567202638E-05,  1.04294586029E-04,  1.53072809860E-04,  2.24667522134E-04, &
  3.29719828517E-04,  4.83898767299E-04,  7.10057855007E-04,  1.04190820377E-03,  1.52838000422E-03,  2.24183176974E-03, &
  3.28636154488E-03,  4.81642274472E-03,  7.05054547250E-03,  1.03141810945E-02,  1.50529568277E-02,  2.19321837975E-02, &
  3.18014238705E-02,  4.59240858792E-02,  6.56510206738E-02,  9.29354313249E-02,  1.28683857774E-01,  1.73917366767E-01, &
  2.23007141217E-01,  2.67404509859E-01,  2.73366070252E-01,  2.06068241098E-01, -1.67534380110E-02, -3.60408605065E-01, &
 -7.33064377532E-01, -4.81827612364E-01,  4.40141743865E-01,  1.32831422063E+00, -1.13994567799E+00, -2.46268092208E-01, &
  9.29146388411E-01, -8.57560517297E-01,  5.73597714499E-01, -3.36064823205E-01,  1.85046743779E-01, -9.90581893402E-02, &
  5.24802391849E-02, -2.77162017159E-02,  1.46248394020E-02, -7.71564221998E-03,  4.07025589629E-03, -2.14707450654E-03, &
  1.13258347863E-03, -5.97437791024E-04,  3.15148309040E-04, -1.66240647777E-04,  8.76918939599E-05, -4.62574484721E-05, &
  2.44007905095E-05, -1.28714098324E-05,  6.78966490764E-06, -3.58154624520E-06,  1.88926459271E-06, -9.96586517910E-07, &
  5.25699095571E-07, -2.77306118553E-07,  1.46278896111E-07, -7.71620747468E-08,  4.07029717719E-08, -2.14708056581E-08, &
  1.13258436802E-08, -5.97437921568E-09,  3.15148328202E-09, -1.66240650589E-09,  8.76918943727E-10, -4.62574485326E-10, &
  2.44007905184E-10, -1.28714098337E-10,  6.78966490783E-11, -3.58154624523E-11,  1.88926459271E-11, -9.96586517911E-12, &
  5.25699095571E-12, -2.77306118553E-12,  1.46278896111E-12, -7.71620747468E-13,  4.07029717719E-13, -2.14708056581E-13, &
  1.13258436802E-13, -5.97437921568E-14,  3.15148328202E-14, -1.66240650589E-14,  8.76918943727E-15, -4.62574485326E-15, &
  2.44007905184E-15, -1.28714098337E-15,  6.78966490783E-16, -3.58154624523E-16,  1.88926459271E-16, -9.96586517911E-17, &
  5.25699095571E-17, -2.77306118553E-17,  1.46278896111E-17, -7.71620747468E-18,  4.07029717719E-18, -2.14708056581E-18, &
  1.13258436802E-18, -5.97437921568E-19,  3.15148328202E-19, -1.66240650589E-19,  8.76918943727E-20, -4.62574485326E-20/

!  Set Anderson sine filter weights
!  --------------------------------

      DATA (WS(J9),J9=1,76)/ &
      -1.1113940E-09,-1.3237246E-12, 1.5091739E-12,-1.6240954E-12, &
       1.7236636E-12,-1.8227727E-12, 1.9255992E-12,-2.0335514E-12, &
       2.1473541E-12,-2.2675549E-12, 2.3946842E-12,-2.5292661E-12, &
       2.6718110E-12,-2.8227693E-12, 2.9825171E-12,-3.1514006E-12, &
       3.3297565E-12,-3.5179095E-12, 3.7163306E-12,-3.9256378E-12, &
       4.1464798E-12,-4.3794552E-12, 4.6252131E-12,-4.8845227E-12, &
       5.1582809E-12,-5.4474462E-12, 5.7530277E-12,-6.0760464E-12, &
       6.4175083E-12,-6.7783691E-12, 7.1595239E-12,-7.5618782E-12, &
       7.9864477E-12,-8.4344110E-12, 8.9072422E-12,-9.4067705E-12, &
       9.9349439E-12,-1.0493731E-11, 1.1084900E-11,-1.1709937E-11, &
       1.2370354E-11,-1.3067414E-11, 1.3802200E-11,-1.4575980E-11, &
       1.5390685E-11,-1.6249313E-11, 1.7155934E-11,-1.8115250E-11, &
       1.9131898E-11,-2.0209795E-11, 2.1352159E-11,-2.2561735E-11, &
       2.3840976E-11,-2.5192263E-11, 2.6618319E-11,-2.8122547E-11, &
       2.9709129E-11,-3.1382870E-11, 3.3149030E-11,-3.5013168E-11, &
       3.6981050E-11,-3.9058553E-11, 4.1251694E-11,-4.3566777E-11, &
       4.6010537E-11,-4.8590396E-11, 5.1314761E-11,-5.4193353E-11, &
       5.7236720E-11,-6.0455911E-11, 6.3861222E-11,-6.7461492E-11, &
       7.1265224E-11,-7.5279775E-11, 7.9512249E-11,-8.3971327E-11/
     DATA (WS(J9),J9=77,152)/ &
      8.8668961E-11,-9.3621900E-11, 9.8851764E-11,-1.0438319E-10,  &
      1.1024087E-10,-1.1644680E-10, 1.2301979E-10,-1.2997646E-10,  &
      1.3733244E-10,-1.4510363E-10, 1.5330772E-10,-1.6196550E-10,  &
      1.7110130E-10,-1.8074257E-10, 1.9091922E-10,-2.0166306E-10,  &
      2.1300756E-10,-2.2498755E-10, 2.3763936E-10,-2.5100098E-10,  &
      2.6511250E-10,-2.8001616E-10, 2.9575691E-10,-3.1238237E-10,  &
      3.2994314E-10,-3.4849209E-10, 3.6808529E-10,-3.8878042E-10,  &
      4.1063982E-10,-4.3372666E-10, 4.5811059E-10,-4.8386049E-10,  &
      5.1105728E-10,-5.3977672E-10, 5.7011632E-10,-6.0215516E-10,  &
      6.3601273E-10,-6.7175964E-10, 7.0955028E-10,-7.4942601E-10,  &
      7.9161025E-10,-8.3606980E-10, 8.8317110E-10,-9.3270330E-10,  &
      9.8533749E-10,-1.0404508E-09, 1.0993731E-09,-1.1605442E-09,  &
      1.2267391E-09,-1.2942905E-09, 1.3691677E-09,-1.4429912E-09,  &
      1.5288164E-09,-1.6077524E-09, 1.7085998E-09,-1.7890471E-09,  &
      1.9129068E-09,-1.9857116E-09, 2.1491608E-09,-2.1926779E-09,  &
      2.4312660E-09,-2.3959044E-09, 2.7872500E-09,-2.5610596E-09,  &
      3.2762318E-09,-2.6082940E-09, 4.0261453E-09,-2.3560563E-09,  &
      5.3176554E-09,-1.3960161E-09, 7.7708747E-09, 1.1853546E-09,  &
      1.2760851E-08, 7.4264707E-09, 2.3342187E-08, 2.1869851E-08/
     DATA (WS(J9),J9=153,228)/ &
      4.6306744E-08, 5.4631686E-08, 9.6763087E-08, 1.2823337E-07,  &
      2.0832812E-07, 2.9280540E-07, 4.5580888E-07, 6.5992437E-07,  &
      1.0056815E-06, 1.4779183E-06, 2.2284335E-06, 3.2994604E-06,  &
      4.9485823E-06, 7.3545473E-06, 1.1001083E-05, 1.6380539E-05,  &
      2.4469550E-05, 3.6469246E-05, 5.4441527E-05, 8.1176726E-05,  &
      1.2113828E-04, 1.8066494E-04, 2.6954609E-04, 4.0202288E-04,  &
      5.9969995E-04, 8.9437312E-04, 1.3338166E-03, 1.9886697E-03,  &
      2.9643943E-03, 4.4168923E-03, 6.5773518E-03, 9.7855105E-03,  &
      1.4539361E-02, 2.1558670E-02, 3.1871864E-02, 4.6903518E-02,  &
      6.8559512E-02, 9.9170152E-02, 1.4120770E-01, 1.9610835E-01,  &
      2.6192603E-01, 3.2743321E-01, 3.6407406E-01, 3.1257559E-01,  &
      9.0460168E-02,-3.6051039E-01,-8.6324760E-01,-8.1178720E-01,  &
      5.2205241E-01, 1.5449873E+00,-1.1817933E+00,-2.6759896E-01,  &
      8.0869203E-01,-6.2757149E-01, 3.4062630E-01,-1.5885304E-01,  &
      7.0472984E-02,-3.1624462E-02, 1.4894068E-02,-7.4821176E-03,  &
      4.0035936E-03,-2.2543784E-03, 1.3160358E-03,-7.8636604E-04,  &
      4.7658745E-04,-2.9125817E-04, 1.7885105E-04,-1.1012416E-04,  &
      6.7910334E-05,-4.1914054E-05, 2.5881544E-05,-1.5985851E-05,  &
      9.8751880E-06,-6.1008526E-06, 3.7692543E-06,-2.3287953E-06/
     DATA (WS(J9),J9=229,266)/  &
      1.4388425E-06,-8.8899353E-07, 5.4926991E-07,-3.3937048E-07,  &
      2.0968284E-07,-1.2955437E-07, 8.0046336E-08,-4.9457371E-08,  &
      3.0557711E-08,-1.8880390E-08, 1.1665454E-08,-7.2076428E-09,  &
      4.4533423E-09,-2.7515696E-09, 1.7001092E-09,-1.0504494E-09,  &
      6.4904567E-10,-4.0102999E-10, 2.4778763E-10,-1.5310321E-10,  &
      9.4600354E-11,-5.8453314E-11, 3.6119400E-11,-2.2320056E-11,  &
      1.3793460E-11,-8.5242656E-12, 5.2675102E-12,-3.2543076E-12,  &
      2.0097689E-12,-1.2405412E-12, 7.6530538E-13,-4.7191929E-13,  &
      2.9084993E-13,-1.7923661E-13, 1.1018948E-13,-6.7885902E-14,  &
      4.2025050E-14,-2.1314731E-14/

!  Set Anderson cosine filter weights
!  ----------------------------------

      DATA (WC(IL),IL=1,76)/  &
      5.1178101E-14, 2.9433849E-14, 2.5492522E-14, 1.9034819E-14,  &
      6.4179780E-14, 1.3085746E-15, 1.1989957E-13,-1.2216234E-14,  &
      1.7534103E-13, 7.9373498E-15, 2.1235658E-13, 7.9981520E-14,  &
      2.3815757E-13, 1.9714260E-13, 2.8920132E-13, 3.4161340E-13,  &
      4.0349917E-13, 5.2203885E-13, 5.9837223E-13, 7.8015306E-13,  &
      8.8911655E-13, 1.1709731E-12, 1.3165595E-12, 1.7578463E-12,  &
      1.9538564E-12, 2.6289768E-12, 2.9167697E-12, 3.9044344E-12,  &
      4.3927341E-12, 5.7526904E-12, 6.6569552E-12, 8.4555678E-12,  &
      1.0063229E-11, 1.2487964E-11, 1.5134682E-11, 1.8501488E-11,  &
      2.2720051E-11, 2.7452598E-11, 3.4025443E-11, 4.0875985E-11,  &
      5.0751668E-11, 6.1094382E-11, 7.5492982E-11, 9.1445759E-11,  &
      1.1227336E-10, 1.3676464E-10, 1.6720269E-10, 2.0423244E-10,  &
      2.4932743E-10, 3.0470661E-10, 3.7198526E-10, 4.5449934E-10,  &
      5.5502537E-10, 6.7793669E-10, 8.2810001E-10, 1.0112626E-09,  &
      1.2354800E-09, 1.5085255E-09, 1.8432253E-09, 2.2503397E-09,  &
      2.7499027E-09, 3.3569525E-09, 4.1025670E-09, 5.0077487E-09,  &
      6.1205950E-09, 7.4703399E-09, 9.1312760E-09, 1.1143911E-08,  &
      1.3622929E-08, 1.6623917E-08, 2.0324094E-08, 2.4798610E-08,  &
      3.0321709E-08, 3.6992986E-08, 4.5237482E-08, 5.5183434E-08/
     DATA (WC(IL),IL=77,152)/                                      &
      6.7491070E-08, 8.2317946E-08, 1.0069271E-07, 1.2279375E-07,  &
      1.5022907E-07, 1.8316969E-07, 2.2413747E-07, 2.7322865E-07,  &
      3.3441046E-07, 4.0756197E-07, 4.9894278E-07, 6.0793233E-07,  &
      7.4443665E-07, 9.0679753E-07, 1.1107379E-06, 1.3525651E-06,  &
      1.6573073E-06, 2.0174273E-06, 2.4728798E-06, 3.0090445E-06,  &
      3.6898816E-06, 4.4879625E-06, 5.5059521E-06, 6.6935820E-06,  &
      8.2160716E-06, 9.9828691E-06, 1.2260527E-05, 1.4888061E-05,  &
      1.8296530E-05, 2.2202672E-05, 2.7305154E-05, 3.3109672E-05,  &
      4.0751046E-05, 4.9372484E-05, 6.0820947E-05, 7.3619571E-05,  &
      9.0780005E-05, 1.0976837E-04, 1.3550409E-04, 1.6365676E-04,  &
      2.0227521E-04, 2.4398338E-04, 3.0197018E-04, 3.6370760E-04,  &
      4.5083748E-04, 5.4213338E-04, 6.7315347E-04, 8.0800951E-04,  &
      1.0051938E-03, 1.2041401E-03, 1.5011708E-03, 1.7942344E-03,  &
      2.2421056E-03, 2.6730676E-03, 3.3490681E-03, 3.9815050E-03,  &
      5.0028666E-03, 5.9285668E-03, 7.4730905E-03, 8.8233510E-03,  &
      1.1160132E-02, 1.3119627E-02, 1.6653199E-02, 1.9472767E-02,  &
      2.4800811E-02, 2.8793704E-02, 3.6762063E-02, 4.2228780E-02,  &
      5.3905163E-02, 6.0804660E-02, 7.7081738E-02, 8.3874501E-02,  &
      1.0377190E-01, 1.0377718E-01, 1.1892208E-01, 9.0437429E-02/
     DATA (WC(IL),IL=153,228)/                                     &
      7.1685138E-02,-3.9473064E-02,-1.5078720E-01,-4.0489859E-01,  &
     -5.6018995E-01,-6.8050388E-01,-1.5094224E-01, 6.6304064E-01,  &
      1.3766748E+00,-8.0373222E-01,-1.0869629E+00, 1.2812892E+00,  &
     -5.0341082E-01,-4.4274455E-02, 2.0913102E-01,-1.9999661E-01,  &
      1.5207664E-01,-1.0920260E-01, 7.8169956E-02,-5.6651561E-02,  &
      4.1611799E-02,-3.0880012E-02, 2.3072559E-02,-1.7311631E-02,  &
      1.3021442E-02,-9.8085025E-03, 7.3943529E-03,-5.5769518E-03,  &
      4.2073164E-03,-3.1745026E-03, 2.3954154E-03,-1.8076122E-03,  &
      1.3640816E-03,-1.0293934E-03, 7.7682952E-04,-5.8623518E-04,  &
      4.4240399E-04,-3.3386183E-04, 2.5195025E-04,-1.9013541E-04,  &
      1.4348659E-04,-1.0828284E-04, 8.1716174E-05,-6.1667509E-05,  &
      4.6537684E-05,-3.5119887E-05, 2.6503388E-05,-2.0000904E-05,  &
      1.5093768E-05,-1.1390572E-05, 8.5959318E-06,-6.4869407E-06,  &
      4.8953713E-06,-3.6942830E-06, 2.7878625E-06,-2.1038241E-06,  &
      1.5875917E-06,-1.1980090E-06, 9.0398030E-07,-6.8208296E-07,  &
      5.1458650E-07,-3.8817581E-07, 2.9272267E-07,-2.2067921E-07,  &
      1.6623514E-07,-1.2514102E-07, 9.4034535E-08,-7.0556837E-08,  &
      5.2741581E-08,-3.9298610E-08, 2.9107255E-08,-2.1413893E-08,  &
      1.5742032E-08,-1.1498608E-08, 8.7561571E-09,-7.2959446E-09/
     DATA (WC(IL),IL=229,281)/                                     &
      6.8816619E-09,-8.9679825E-09, 1.4258275E-08,-1.9564299E-08,  &
      2.0235313E-08,-1.4725545E-08, 5.4632820E-09, 3.5995580E-09,  &
     -9.5287133E-09, 1.1460041E-08,-1.0250532E-08, 7.4641748E-09,  &
     -4.4703465E-09, 2.0499053E-09,-4.4806353E-10,-4.0374336E-10,  &
      7.0321001E-10,-6.7067960E-10, 4.9130404E-10,-2.8840747E-10,  &
      1.2373144E-10,-1.5260443E-11,-4.2027559E-11, 6.1885474E-11,  &
     -5.9273937E-11, 4.6588766E-11,-3.2054182E-11, 1.9831637E-11,  &
     -1.1210098E-11, 5.9567021E-12,-3.2427812E-12, 2.1353868E-12,  &
     -1.8476851E-12, 1.8438474E-12,-1.8362842E-12, 1.7241847E-12,  &
     -1.5161479E-12, 1.2627657E-12,-1.0129176E-12, 7.9578625E-13,  &
     -6.2131435E-13, 4.8745900E-13,-3.8703630E-13, 3.1172547E-13,  &
     -2.5397802E-13, 2.0824130E-13,-1.7123163E-13, 1.4113344E-13,  &
     -1.1687986E-13, 9.7664016E-14,-8.2977176E-14, 7.2515267E-14,  &
     -5.6047478E-14/

!  Base for Anderson sine filter weights
!  ---------------------------------------

      DATA (AS(IL),IL=1,76)/ &
      0.2827488E-16, 0.3453502E-16, 0.4218117E-16, 0.5152020E-16,  &
      0.6292691E-16, 0.7685910E-16, 0.9387592E-16, 0.1146603E-15,  &
      0.1400464E-15, 0.1710531E-15, 0.2089247E-15, 0.2551812E-15,  &
      0.3116790E-15, 0.3806856E-15, 0.4649705E-15, 0.5679162E-15,  &
      0.6936544E-15, 0.8472315E-15, 0.1034811E-14, 0.1263921E-14,  &
      0.1543756E-14, 0.1885548E-14, 0.2303014E-14, 0.2812907E-14,  &
      0.3435693E-14, 0.4196365E-14, 0.5125452E-14, 0.6260241E-14,  &
      0.7646276E-14, 0.9339182E-14, 0.1140690E-13, 0.1393242E-13,  &
      0.1701710E-13, 0.2078473E-13, 0.2538653E-13, 0.3100718E-13,  &
      0.3787225E-13, 0.4625727E-13, 0.5649876E-13, 0.6900774E-13,  &
      0.8428625E-13, 0.1029475E-12, 0.1257403E-12, 0.1535796E-12,  &
      0.1875825E-12, 0.2291138E-12, 0.2798402E-12, 0.3417976E-12,  &
      0.4174725E-12, 0.5099021E-12, 0.6227958E-12, 0.7606845E-12,  &
      0.9291022E-12, 0.1134808E-11, 0.1386058E-11, 0.1692935E-11,  &
      0.2067755E-11, 0.2525562E-11, 0.3084728E-11, 0.3767695E-11,  &
      0.4601874E-11, 0.5620741E-11, 0.6865189E-11, 0.8385161E-11,  &
      0.1024166E-10, 0.1250919E-10, 0.1527876E-10, 0.1866152E-10,  &
      0.2279323E-10, 0.2783971E-10, 0.3400350E-10, 0.4153197E-10,  &
      0.5072727E-10, 0.6195842E-10, 0.7567619E-10, 0.9243110E-10/
     DATA (AS(IL),IL=77,152)/                                      &
      0.1128956E-09, 0.1378910E-09, 0.1684205E-09, 0.2057092E-09,  &
      0.2512538E-09, 0.3068821E-09, 0.3748266E-09, 0.4578143E-09,  &
      0.5591756E-09, 0.6829786E-09, 0.8341920E-09, 0.1018884E-08,  &
      0.1244468E-08, 0.1519997E-08, 0.1856528E-08, 0.2267569E-08,  &
      0.2769615E-08, 0.3382815E-08, 0.4131780E-08, 0.5046568E-08,  &
      0.6163892E-08, 0.7528594E-08, 0.9195445E-08, 0.1123134E-07,  &
      0.1371799E-07, 0.1675519E-07, 0.2046484E-07, 0.2499581E-07,  &
      0.3052996E-07, 0.3728937E-07, 0.4554534E-07, 0.5562921E-07,  &
      0.6794566E-07, 0.8298903E-07, 0.1013630E-06, 0.1238051E-06,  &
      0.1512159E-06, 0.1846955E-06, 0.2255876E-06, 0.2755333E-06,  &
      0.3365371E-06, 0.4110474E-06, 0.5020544E-06, 0.6132106E-06,  &
      0.7489771E-06, 0.9148027E-06, 0.1117343E-05, 0.1364725E-05,  &
      0.1666879E-05, 0.2035931E-05, 0.2486692E-05, 0.3037252E-05,  &
      0.3709708E-05, 0.4531048E-05, 0.5534234E-05, 0.6759529E-05,  &
      0.8256107E-05, 0.1008403E-04, 0.1231666E-04, 0.1504361E-04,  &
      0.1837430E-04, 0.2244243E-04, 0.2741124E-04, 0.3348017E-04,  &
      0.4089277E-04, 0.4994654E-04, 0.6100484E-04, 0.7451148E-04,  &
      0.9100853E-04, 0.1111581E-03, 0.1357688E-03, 0.1658283E-03,  &
      0.2025432E-03, 0.2473868E-03, 0.3021590E-03, 0.3690578E-03/
     DATA (AS(IL),IL=153,228)/                                     &
      0.4507682E-03, 0.5505695E-03, 0.6724671E-03, 0.8213532E-03,  &
      0.1003203E-02, 0.1225315E-02, 0.1496603E-02, 0.1827955E-02,  &
      0.2232670E-02, 0.2726989E-02, 0.3330752E-02, 0.4068189E-02,  &
      0.4968897E-02, 0.6069025E-02, 0.7412724E-02, 0.9053921E-02,  &
      0.1105849E-01, 0.1350686E-01, 0.1649732E-01, 0.2014987E-01,  &
      0.2461111E-01, 0.3006008E-01, 0.3671546E-01, 0.4484437E-01,  &
      0.5477304E-01, 0.6689993E-01, 0.8171177E-01, 0.9980298E-01,  &
      0.1218996E+00, 0.1488885E+00, 0.1818529E+00, 0.2221156E+00,  &
      0.2712926E+00, 0.3313576E+00, 0.4047211E+00, 0.4943274E+00,  &
      0.6037728E+00, 0.7374498E+00, 0.9007232E+00, 0.1100146E+01,  &
      0.1343721E+01, 0.1641225E+01, 0.2004596E+01, 0.2448420E+01,  &
      0.2990507E+01, 0.3652613E+01, 0.4461311E+01, 0.5449058E+01,  &
      0.6655495E+01, 0.8129040E+01, 0.9928831E+01, 0.1212710E+02,  &
      0.1481208E+02, 0.1809151E+02, 0.2209702E+02, 0.2698936E+02,  &
      0.3296488E+02, 0.4026340E+02, 0.4917783E+02, 0.6006593E+02,  &
      0.7336469E+02, 0.8960784E+02, 0.1094473E+03, 0.1336792E+03,  &
      0.1632761E+03, 0.1994259E+03, 0.2435794E+03, 0.2975085E+03,  &
      0.3633777E+03, 0.4438306E+03, 0.5420959E+03, 0.6621174E+03,  &
      0.8087120E+03, 0.9877631E+03, 0.1206457E+04, 0.1473569E+04/
     DATA (AS(IL),IL=229,266)/                                     &
      0.1799822E+04, 0.2198307E+04, 0.2685019E+04, 0.3279489E+04,  &
      0.4005577E+04, 0.4892423E+04, 0.5975619E+04, 0.7298637E+04,  &
      0.8914575E+04, 0.1088829E+05, 0.1329898E+05, 0.1624342E+05,  &
      0.1983975E+05, 0.2423233E+05, 0.2959743E+05, 0.3615039E+05,  &
      0.4415418E+05, 0.5393004E+05, 0.6587030E+05, 0.8045416E+05,  &
      0.9826694E+05, 0.1200235E+06, 0.1465970E+06, 0.1790540E+06,  &
      0.2186971E+06, 0.2671173E+06, 0.3262578E+06, 0.3984921E+06,  &
      0.4867193E+06, 0.5944804E+06, 0.7260999E+06, 0.8868605E+06,  &
      0.1083214E+07, 0.1323040E+07, 0.1615965E+07, 0.1973744E+07,  &
      0.2410737E+07, 0.2944481E+07/

!  Base for Anderson cosine filter weights
!  ---------------------------------------

      DATA (AC(IL),IL=1,76)/ &
      0.8445875E-13, 0.1031582E-12, 0.1259977E-12, 0.1538939E-12,  &
      0.1879664E-12, 0.2295827E-12, 0.2804129E-12, 0.3424971E-12,  &
      0.4183269E-12, 0.5109457E-12, 0.6240705E-12, 0.7622414E-12,  &
      0.9310038E-12, 0.1137131E-11, 0.1388894E-11, 0.1696399E-11,  &
      0.2071987E-11, 0.2530731E-11, 0.3091041E-11, 0.3775407E-11,  &
      0.4611292E-11, 0.5632245E-11, 0.6879239E-11, 0.8402321E-11,  &
      0.1026262E-10, 0.1253479E-10, 0.1531003E-10, 0.1869971E-10,  &
      0.2283988E-10, 0.2789669E-10, 0.3407310E-10, 0.4161697E-10,  &
      0.5083109E-10, 0.6208523E-10, 0.7583107E-10, 0.9262028E-10,  &
      0.1131267E-09, 0.1381732E-09, 0.1687652E-09, 0.2061302E-09,  &
      0.2517680E-09, 0.3075102E-09, 0.3755938E-09, 0.4587513E-09,  &
      0.5603201E-09, 0.6843764E-09, 0.8358993E-09, 0.1020970E-08,  &
      0.1247015E-08, 0.1523108E-08, 0.1860328E-08, 0.2272210E-08,  &
      0.2775283E-08, 0.3389739E-08, 0.4140236E-08, 0.5056896E-08,  &
      0.6176507E-08, 0.7544003E-08, 0.9214266E-08, 0.1125433E-07,  &
      0.1374607E-07, 0.1678949E-07, 0.2050673E-07, 0.2504697E-07,  &
      0.3059244E-07, 0.3736569E-07, 0.4563856E-07, 0.5574306E-07,  &
      0.6808473E-07, 0.8315887E-07, 0.1015705E-06, 0.1240585E-06,  &
      0.1515254E-06, 0.1850735E-06, 0.2260493E-06, 0.2760972E-06/
     DATA (AC(IL),IL=77,152)/                                      &
       0.3372259E-06, 0.4118886E-06, 0.5030819E-06, 0.6144656E-06, &
       0.7505100E-06, 0.9166750E-06, 0.1119629E-05, 0.1367518E-05, &
       0.1670291E-05, 0.2040098E-05, 0.2491781E-05, 0.3043468E-05, &
       0.3717300E-05, 0.4540321E-05, 0.5545561E-05, 0.6773363E-05, &
       0.8273004E-05, 0.1010467E-04, 0.1234187E-04, 0.1507440E-04, &
       0.1841191E-04, 0.2248836E-04, 0.2746734E-04, 0.3354869E-04, &
       0.4097646E-04, 0.5004876E-04, 0.6112969E-04, 0.7466398E-04, &
       0.9119479E-04, 0.1113856E-03, 0.1360466E-03, 0.1661677E-03, &
       0.2029577E-03, 0.2478931E-03, 0.3027774E-03, 0.3698131E-03, &
       0.4516908E-03, 0.5516963E-03, 0.6738434E-03, 0.8230342E-03, &
       0.1005256E-02, 0.1227823E-02, 0.1499666E-02, 0.1831696E-02, &
       0.2237239E-02, 0.2732570E-02, 0.3337568E-02, 0.4076515E-02, &
       0.4979067E-02, 0.6081446E-02, 0.7427895E-02, 0.9072452E-02, &
       0.1108112E-01, 0.1353451E-01, 0.1653109E-01, 0.2019111E-01, &
       0.2466148E-01, 0.3012160E-01, 0.3679061E-01, 0.4493615E-01, &
       0.5488514E-01, 0.6703686E-01, 0.8187900E-01, 0.1000072E+00, &
       0.1221491E+00, 0.1491933E+00, 0.1822251E+00, 0.2225702E+00, &
       0.2718479E+00, 0.3320357E+00, 0.4055493E+00, 0.4953391E+00, &
       0.6050085E+00, 0.7389591E+00, 0.9025667E+00, 0.1102397E+01/
     DATA (AC(IL),IL=153,228)/                                     &
       0.1346471E+01, 0.1644584E+01, 0.2008699E+01, 0.2453431E+01, &
       0.2996627E+01, 0.3660089E+01, 0.4470442E+01, 0.5460211E+01, &
       0.6669116E+01, 0.8145677E+01, 0.9949152E+01, 0.1215192E+02, &
       0.1484239E+02, 0.1812854E+02, 0.2214225E+02, 0.2704460E+02, &
       0.3303235E+02, 0.4034580E+02, 0.4927848E+02, 0.6018887E+02, &
       0.7351485E+02, 0.8979124E+02, 0.1096713E+03, 0.1339528E+03, &
       0.1636103E+03, 0.1998341E+03, 0.2440779E+03, 0.2981174E+03, &
       0.3641214E+03, 0.4447389E+03, 0.5432053E+03, 0.6634725E+03, &
       0.8103672E+03, 0.9897847E+03, 0.1208926E+04, 0.1476585E+04, &
       0.1803505E+04, 0.2202806E+04, 0.2690514E+04, 0.3286201E+04, &
       0.4013775E+04, 0.4902436E+04, 0.5987849E+04, 0.7313575E+04, &
       0.8932820E+04, 0.1091057E+05, 0.1332620E+05, 0.1627666E+05, &
       0.1988036E+05, 0.2428192E+05, 0.2965801E+05, 0.3622438E+05, &
       0.4424455E+05, 0.5404042E+05, 0.6600512E+05, 0.8061883E+05, &
       0.9846806E+05, 0.1202692E+06, 0.1468971E+06, 0.1794205E+06, &
       0.2191447E+06, 0.2676639E+06, 0.3269255E+06, 0.3993077E+06, &
       0.4877155E+06, 0.5956971E+06, 0.7275861E+06, 0.8886756E+06, &
       0.1085431E+07, 0.1325748E+07, 0.1619273E+07, 0.1977784E+07, &
       0.2415671E+07, 0.2950507E+07, 0.3603757E+07, 0.4401639E+07/
      DATA (AC(IL),IL=229,281)/                                    &
       0.5376174E+07, 0.6566474E+07, 0.8020310E+07, 0.9796028E+07, &
       0.1196490E+08, 0.1461396E+08, 0.1784953E+08, 0.2180146E+08, &
       0.2662837E+08, 0.3252396E+08, 0.3972485E+08, 0.4852005E+08, &
       0.5926252E+08, 0.7238340E+08, 0.8840929E+08, 0.1079834E+09, &
       0.1318912E+09, 0.1610922E+09, 0.1967585E+09, 0.2403214E+09, &
       0.2935292E+09, 0.3585173E+09, 0.4378941E+09, 0.5348451E+09, &
       0.6532612E+09, 0.7978950E+09, 0.9745512E+09, 0.1190320E+10, &
       0.1453860E+10, 0.1775748E+10, 0.2168904E+10, 0.2649105E+10, &
       0.3235624E+10, 0.3952000E+10, 0.4826984E+10, 0.5895691E+10, &
       0.7201014E+10, 0.8795338E+10, 0.1074265E+11, 0.1312110E+11, &
       0.1602615E+11, 0.1957438E+11, 0.2390821E+11, 0.2920155E+11, &
       0.3566686E+11, 0.4356360E+11, 0.5320870E+11, 0.6498925E+11, &
       0.7937805E+11, 0.9695257E+11, 0.1184181E+12, 0.1446362E+12, &
       0.1766591E+12/

END MODULE AG_Filter_coefficients

 MODULE AG_Input_routines

! CONTAINS: READ_SYSTEM_DATA, READ_MODEL_DATA, SET_FRQ

 Use iso_Fortran_env
 Use AG_Metadata
 IMPLICIT NONE

! SYSTEM & LITHOLOGY DIMENSIONS
! -----------------------------

 INTEGER, PARAMETER :: NPROP=7,QL = SELECTED_REAL_KIND(12,76)
 REAL, PARAMETER :: PI=3.141592654
 INTEGER NR,NW,ND,NDR,NLG,MSG,MXERR,DO3D,TDFD,STEP,NSX,PRFL,ISTOP,KRXW,NCHNL,NFRQ, &
         SOURCE_TYPE,SURVEY_TYPE,NTX,NRXG,MXVRTX,MQVR,J,JS,JT,JV,JG,JR,NEVENTS,LRX, &
         MRX,NLITH,KACC,SOLVER,OUTPUT,NPULS,NTYRP,NTYPLS,NTXE,KFRQE,NEZ,NPN,CMPDX, &
         QQDT(8), QQHMS(2)
 INTEGER, ALLOCATABLE, DIMENSION(:) :: N_VRTX,UNITS,NRX,RX_TYPE,CMP,NRGTX,NRXTX,TXID,NCMPG
 INTEGER, ALLOCATABLE, DIMENSION(:,:)  :: RGTXID,RXID,NCMP,PRTCMP,LNODS
 REAL T0SX,OFFTYM,REFTYM,PULSE,RXOE,RXON,RXOZ,RXFMNT,MAXFRQ,MINFRQ,MXFRQE
 REAL, ALLOCATABLE, DIMENSION(:) :: TXON,WAVEFORM,CURNT,TRP,TMS,WTMS,TOPN,TCLS,FREQ,SWX,SXMNT, &
       SXDIP,SXAZ
 REAL, ALLOCATABLE, DIMENSION(:,:) :: SWY,SXE,SXN,SXZ,LYTH,RXDIP,RXAZ,RXMNT,BHAZ,BHDIP,COORD
 REAL, ALLOCATABLE, DIMENSION(:,:,:) :: RXE,RXN,RXZ
 REAL(KIND=QL) ECNTRD,NCNTRD,QD
 REAL(KIND=QL), ALLOCATABLE, DIMENSION(:,:)   :: SXED,SXND,SXZD,CLCD
 REAL(KIND=QL), ALLOCATABLE, DIMENSION(:,:,:) :: RXED,RXND,RXZD
 LOGICAL NEW
 LOGICAL, ALLOCATABLE :: BHR(:,:)
 CHARACTER (LEN=120) INP,TITLE
 Logical :: Invert = .False.
 Integer :: tvals(8)

! Specific parameters for Arjuna
! ----------------------------
 INTEGER NE,NZ,NAIR,NEAST,NSZ,NMP
 INTEGER,ALLOCATABLE :: LITH(:,:)
 REAL, ALLOCATABLE, DIMENSION(:)   :: ESURF,ZSURF
 REAL, ALLOCATABLE, DIMENSION(:,:,:) :: XRM,YRM
 REAL, ALLOCATABLE, DIMENSION(:,:) :: ELOC,ZLOC
 REAL(KIND=QL), ALLOCATABLE :: ESURFD(:),ZSURFD(:)
 REAL(KIND=QL), ALLOCATABLE :: ELOCD(:,:),ZLOCD(:,:)
 LOGICAL NESWAP

 CONTAINS

   SUBROUTINE READ_SYSTEM_DATA
!  ---------------------------

!***  Called by MAIN
!***  Calls CUBSPL, CALL CONFIG_ID, WRYT_LOG_FILE

 IMPLICIT NONE
 INTEGER, PARAMETER :: MXTMP = 300,QL = SELECTED_REAL_KIND(12,76)
 INTEGER NEVNT_TX,KS,JS1,JRG,JRP,KRFIN,KRG,NRX1
 INTEGER, ALLOCATABLE, DIMENSION(:) :: TX_INDX, RXG_INDX
 REAL RXOR
 REAL(KIND=QL), ALLOCATABLE, DIMENSION(:,:,:) :: Q2E,Q2N,Q2Z
 LOGICAL REFT

 NR = 3       !  Set INPUT UNIT NUMBER
 NW = 4       !  Set OUTPUT UNIT NUMBER
 ND = 7       !  Set unit to store frequency domain input for reruns.
 NLG = 23     !  Set LOG FILE UNIT NUMBER
 NMP  = 25    !  Set MAP FILE UNIT NUMBER
 T0SX = 100.

 OPEN(NR,FILE = 'Arjuna.cfl',STATUS = 'OLD')
 OPEN(NW,FILE = 'Arjuna.out',STATUS = 'REPLACE')
 OPEN(NLG,FILE = 'Arjuna.log',STATUS = 'REPLACE')

!      Initialise some variables.

 NEW = .TRUE.
 NDR = NR              !  Read from Arjuna.cfl
 MXERR = 0             !  Initialise input error flag
 DO3D = 1
 REFT = .FALSE.
 REFTYM = 0.

!  Reproduce input data with no assignments and rewind file.

 Call Date_and_time(Values = tvals)
 Write (NW, 1) PNAME, PVERS, PDATE, PAUT1, PAUT2, PAUT3, PPROJ, PRELS, &
              compiler_version(), compiler_options(), &
              tvals(1:3), tvals(5:7)
Write ( *, 1) PNAME, PVERS, PDATE, PAUT1, PAUT2, PAUT3, PPROJ, PRELS, &
              compiler_version(), compiler_options(), &
              tvals(1:3), tvals(5:7)

 WRITE(NW,'(T11,A/T11,A/)') 'INPUT DATA', '----------'

 READ(NR,'(A)') TITLE
 WRITE(NW,'(/1X,A)') TITLE

! Read model control & print parameters

 READ(NR,*)  TDFD, PRFL, STEP, ISTOP
 WRITE(NW,3) TDFD, PRFL, STEP, ISTOP

!   TDFD = 1 (or -1) or 2 for TD or FD respectively.
!   PRFL - indicates profile or decay curve output
!  ISTOP - read data and stop if ISTOP = 1
!        - used as a frequency setter later on in this routine.

 MINFRQ = -1.;  MAXFRQ = -1.;  MXFRQE = -1.

 IF (TDFD == -1) THEN
   READ(NR,*)  MINFRQ, MAXFRQ, MXFRQE
   MAXFRQ = MAX (MAXFRQ,MXFRQE)
   WRITE(NW,7) MINFRQ, MAXFRQ, MXFRQE
   TDFD = 1
 END IF

 IF (TDFD /= 1 .AND. TDFD /= 2) CALL WRYT_LOG_FILE (NLG,1,MXERR,3)

 IF (PRFL < 0 .OR. PRFL > 1 ) THEN
   CALL WRYT_LOG_FILE (NLG,2,MXERR,2)
   PRFL = 1
 END IF

 IF (STEP /= 1 .AND. STEP /= 0) CALL WRYT_LOG_FILE (NLG,4,MXERR,3)

 IF (TDFD == 1) THEN                            ! Time-domain parameters
   READ(NR,*) DO3D,NSX,OFFTYM,NCHNL,KRXW
   WRITE(NW,4) DO3D, NSX, NCHNL, KRXW, OFFTYM
   DO3D = ABS(DO3D)

   IF (KRXW < 0) THEN     !  Check for shifted time origin for receiver channels
     REFT = .TRUE.
     KRXW = ABS(KRXW)
   END IF

   IF (DO3D < 2) THEN
     OPEN(ND,FILE = 'Arjuna.frq',STATUS = 'REPLACE')
   ELSE IF (DO3D == 2) THEN
     NEW = .FALSE.
     NDR = ND           ! Data to be read in from Arjuna.frq
     OPEN(ND,FILE = 'Arjuna.frq',STATUS = 'OLD')
   ELSE IF (DO3D > 2) THEN
     DO3D = 1
     CALL WRYT_LOG_FILE (NLG,3,MXERR,2)
   END IF

   IF (KRXW /= 1 .AND. KRXW /= 2) CALL WRYT_LOG_FILE (NLG,5,MXERR,3)

   IF (REFT) READ(NR,*) REFTYM

   ALLOCATE (TXON(NSX), WAVEFORM(NSX),TMS(NCHNL),WTMS(NCHNL),TOPN(NCHNL),TCLS(NCHNL),SWX(NSX),SWY(NSX,3))
   TXON=0.; WAVEFORM=0.; TMS=0.; WTMS=0.; TOPN=0.; TCLS=0; SWX=0; SWY=0

   READ(NR,*) (TXON(J),WAVEFORM(J), J = 1,NSX)  ! Read in source waveform.

   WRITE(NW,5)
   DO J = 1, NSX
     WRITE(NW,'(3X,I4,F13.3,5X,G13.4)') J,TXON(J),WAVEFORM(J)
     SWX(J) = 1.E-3 * TXON(J)
     SWY(J,3) = WAVEFORM(J)
   END DO
   PULSE = 1.E-3 * (OFFTYM + MAXVAL (TXON) )

   IF (KRXW == 1) THEN
     READ(NR,*) (TOPN(J) ,TCLS(J), J = 1,NCHNL)
     TMS = (TOPN + TCLS) / 2.
     WTMS = TCLS - TOPN
   ELSE
     READ(NR,*) TMS(1:NCHNL)
     READ(NR,*) WTMS(1:NCHNL)
     TCLS= TMS + WTMS /2.
     TOPN= TMS - WTMS /2.
   END IF

   IF (REFT) THEN
     WRITE(NW,25) REFTYM
   ELSE
     WRITE(NW,6)
   END IF

   TOPN = TOPN + REFTYM
   TCLS = TCLS + REFTYM
   DO J = 1,NCHNL
     IF (REFT) THEN
       WRITE(NW,'(7X,I4,2F12.3,F11.3,F12.3,F11.3)') J,TOPN(J),TCLS(J),WTMS(J),TMS(J)+REFTYM,TMS(J)
     ELSE
       WRITE(NW,'(7X,I4,2F12.3,F11.3,F12.3)') J,TOPN(J),TCLS(J),WTMS(J),TMS(J)
     END IF
     IF ( TOPN(J) <= 0) CALL WRYT_LOG_FILE (NLG,7,MXERR,3)
   END DO
   TOPN = 1.E-3 * TOPN
   TCLS = 1.E-3 * TCLS
 END IF

 IF (TDFD == 2) THEN          ! Frequency-domain systems
   READ(NR,*)  NFRQ
   WRITE(NW,8) NFRQ
   ALLOCATE( FREQ(NFRQ), CURNT(NFRQ))

! Read & write source waveform.

   WRITE(NW,9)
   DO J = 1, NFRQ
     READ(NR,*) FREQ(J),CURNT(J)
     WRITE(NW,'(3X,I4,F13.0,5X,G13.4)') J,FREQ(J),CURNT(J)
   END DO
   OPEN(ND,FILE = 'Arjuna.frq',STATUS = 'REPLACE')   !For my checking only
 END IF

! Transmitter Information
! -----------------------
! Read in absolute locations in double precision variables and convert to
! REAL*4 body centred coordinates before entering the computation realm.


 READ(NDR,*) SOURCE_TYPE, SURVEY_TYPE, NTX, NRXG
 IF (SURVEY_TYPE > 1) NRXG = NTX
 IF (NEW) WRITE(ND,'(5I5)') SOURCE_TYPE, SURVEY_TYPE, NTX, NRXG

 WRITE(NW,10) SOURCE_TYPE, SURVEY_TYPE, NTX, NRXG

 IF (SOURCE_TYPE < 1 .OR. SOURCE_TYPE > 4) CALL WRYT_LOG_FILE (NLG,8,MXERR,3)
 IF (SOURCE_TYPE == 4) THEN
   WRITE(NW,'(/T3,A/)') 'MT OPTION NOT YET IMPLEMENTED.  NO EXECUTION.'
   WRITE(*,'(/T3,A/)') 'MT OPTION NOT YET IMPLEMENTED.  NO EXECUTION.'
   STOP
 END IF

 IF (SURVEY_TYPE < 1 .OR. SURVEY_TYPE > 4) CALL WRYT_LOG_FILE (NLG,10,MXERR,3)
 IF (SURVEY_TYPE == 1 .AND. NRXG == 0) CALL WRYT_LOG_FILE (NLG,11,MXERR,3)

 ALLOCATE (UNITS(NRXG),NRX(NRXG),RX_TYPE(NRXG),CMP(NRXG),NCMPG(NRXG),PRTCMP(10,NRXG))
 UNITS=0; NRX=1; MRX=1; MQVR=1; RX_TYPE=1; CMP=1

! When SURVEY_TYPE > 1, then the number of events is equal to the number
! and this can be used to set dimensions early.  Otherwise, the transmitter
! related dimensions have to wait until events are counted.

 IF (SURVEY_TYPE > 1) THEN  ! Allocate SURVEY_TYPE = 1 variables for degenerate case
   NTXE = NTX  ! Number of transmitters actually used.
   MRX = 1     ! Maximum number of receivers per group
   LRX = 1     ! Maximum number of receivers per transmitter
   MQVR = 1    ! Dipole receiver has 1 "corner"

   ALLOCATE (NRGTX(NTXE), NRXTX(NTXE), RGTXID(NRXG,NTXE),RXID(LRX,NTXE),BHR(LRX,NTXE),      &
             BHDIP(LRX,NTXE), BHAZ(LRX,NTXE),NCMP(LRX,NTXE),RXDIP(MRX,NRXG),RXAZ(MRX,NRXG), &
             RXED(MRX,NRXG,MQVR),RXND(MRX,NRXG,MQVR),RXZD(MRX,NRXG,MQVR),RXMNT(MRX,NRXG),   &
             RXE(MRX,NRXG,MQVR),RXN(MRX,NRXG,MQVR),RXZ(MRX,NRXG,MQVR))

   NRGTX = 1;  NRXTX = 1; RGTXID = 0; RXID = 1; BHR = .FALSE.; BHAZ = 0.; BHDIP = 0.
   NCMP = 1; RXMNT = 1.; RXN = 0.; RXE = 0.; RXZ = 0.; RXED = 0.D0; RXZD = 0.D0
   RXND = 0.D0; RXAZ = 0; RXDIP = 0.

   DO JS = 1,NTXE
     RGTXID(JS,JS) = JS
   END DO
 END IF

 ALLOCATE (SXED(MXTMP,NTX),SXND(MXTMP,NTX),SXZD(MXTMP,NTX),SXMNT(NTX),SXDIP(NTX),SXAZ(NTX),N_VRTX(NTX))
 N_VRTX = 1; SXMNT = 1; SXAZ = 90; SXDIP = 0; SXED = 0.D0; SXND = 0.D0; SXZD = 0.D0

 IF (SOURCE_TYPE < 3) THEN
   IF (SOURCE_TYPE == 1) WRITE(NW,11)
   IF (SOURCE_TYPE == 2) WRITE(NW,12)
   DO JS = 1,NTX
     READ(NDR,*) N_VRTX(JS)
     IF (NEW) WRITE(ND,'(I5)') N_VRTX(JS)
     WRITE(NW,13) JS,N_VRTX(JS)
     DO JV = 1, N_VRTX(JS)
       READ(NDR,*) SXED(JV,JS),SXND(JV,JS),SXZD(JV,JS)
       WRITE(NW,'(I5,3F14.2)') JV,SXED(JV,JS),SXND(JV,JS),SXZD(JV,JS)
       IF (NEW) WRITE(ND,'(3F14.2)') SXED(JV,JS),SXND(JV,JS),SXZD(JV,JS)
     END DO
   END DO

   MXVRTX = MAXVAL (N_VRTX)

 ELSE IF (SOURCE_TYPE == 3) THEN
   MXVRTX=1
   N_VRTX = 1
   WRITE(NW,14)
   DO JS = 1,NTX
     READ(NDR,*) SXED(1,JS),SXND(1,JS),SXZD(1,JS),SXDIP(JS),SXAZ(JS),SXMNT(JS)

     IF (ABS (ABS (SXDIP(JS)) - 90.) > 0.01) THEN
       IF (ABS (SXAZ(JS) - 270.) < 1.) SXAZ(JS) = -90.
       IF (SXAZ(JS)  > 0. .AND. (SXAZ(JS) - 90.) > 0.001) THEN
         SXAZ(JS) = 90.
         WRITE(NW,90) JS
         WRITE(*,90) JS
       ELSE IF (SXAZ(JS)  < 0. .AND. (SXAZ(JS) + 90.) > 0.001) THEN
         SXAZ(JS) = -90.
         WRITE(NW,90) JS
         WRITE(*,90) JS
       END IF
     END IF

     WRITE(NW,'(I5,3F14.2,2F9.1,F10.1)') JS,SXED(1,JS),SXND(1,JS),SXZD(1,JS),SXDIP(JS),SXAZ(JS),SXMNT(JS)
     IF(NEW) WRITE(ND,'(6F13.2)') SXED(1,JS),SXND(1,JS),SXZD(1,JS),SXDIP(JS),SXAZ(JS),SXMNT(JS)
   END DO
 END IF

!   Receiver Group Specification
!   ----------------------------

 IF (SURVEY_TYPE == 4) THEN      ! Coincident Loop
   IF (SOURCE_TYPE > 1) CALL WRYT_LOG_FILE (NLG,16,MXERR,3)
   IF (TDFD == 2) CALL WRYT_LOG_FILE (NLG,17,MXERR,3)
   READ(NDR,*) UNITS(1)
   WRITE(NW,'(/A,I2)') '  Coincident loop voltage units =',UNITS(1)
   IF (NEW) WRITE(ND,'(I3)') UNITS(1)

 ELSE IF (SURVEY_TYPE == 2 .OR. SURVEY_TYPE == 3) THEN
   READ(NDR,*) RXOE,RXON,RXOZ,RXFMNT,CMP(1),UNITS(1)        ! mag dipole at fixed offfset
   IF (SOURCE_TYPE == 2) CALL WRYT_LOG_FILE (NLG,14,MXERR,3)

   IF (CMP(1) > 2) CMP(1) = 0
   IF (SOURCE_TYPE /= 3 .AND. CMP(1) == 2) CMP(1) = 0
   CMP = CMP(1)

   IF (NEW) WRITE(ND,'(4F13.2,2I5)') RXOE,RXON,RXOZ,RXFMNT,CMP(1),UNITS(1)
   WRITE(NW,16) RXOE,RXON,RXOZ,RXFMNT,CMP(1),UNITS(1)
   RXOR = ABS (RXOE) + ABS (RXON) + ABS (RXOZ)
   IF (SOURCE_TYPE == 3) THEN
     IF (RXOR < 0.01) CALL WRYT_LOG_FILE (NLG,15,MXERR,3)
     IF (SURVEY_TYPE == 3) CALL WRYT_LOG_FILE (NLG,6,MXERR,3)
   END IF
 ELSE IF (SURVEY_TYPE == 1) THEN
   ALLOCATE (RXED(MXTMP,NRXG,4), RXND(MXTMP,NRXG,4), RXZD(MXTMP,NRXG,4), &
             RXDIP(MXTMP,NRXG),  RXAZ(MXTMP,NRXG),   RXMNT(MXTMP,NRXG))
   RXZD = 0.D0; RXED = 0.D0; RXND = 0.D0; RXDIP = 0; RXAZ = 0; RXMNT = 1.

   DO JG = 1,NRXG
     READ(NDR,*) NRX(JG),RX_TYPE(JG), CMP(JG), UNITS(JG)
     IF (RX_TYPE(JG) /= 1) CMP(JG) = 1
     IF (NEW) WRITE(ND,'(4I6)') NRX(JG), RX_TYPE(JG), CMP(JG), UNITS(JG)
     WRITE(NW,17) JG, NRX(JG), RX_TYPE(JG), CMP(JG), UNITS(JG)

     IF (RX_TYPE(JG) == 1) THEN
       IF (CMP(JG) > 2)  CMP(JG) = 0
       IF (CMP(JG) < 2) THEN
         WRITE(NW,18)
         DO JR = 1, NRX(JG)
           READ (NDR,*) RXED(JR,JG,1),RXND(JR,JG,1),RXZD(JR,JG,1), RXMNT(JR,JG)
           IF (NEW) WRITE(ND,'(4F12.2)') RXED(JR,JG,1),RXND(JR,JG,1),RXZD(JR,JG,1), RXMNT(JR,JG)
           WRITE(NW,'(I4,3F13.2,F10.1)') JR,RXED(JR,JG,1),RXND(JR,JG,1),RXZD(JR,JG,1),RXMNT(JR,JG)
         END DO
       ELSE IF (CMP(JG) == 2) THEN
         WRITE(NW,15)
         DO JR = 1, NRX(JG)
           READ (NDR,*) RXED(JR,JG,1),RXND(JR,JG,1),RXZD(JR,JG,1),RXDIP(JR,JG), &
                       RXAZ(JR,JG),RXMNT(JR,JG)
           IF (NEW) WRITE(ND,'(6F12.2)') RXED(JR,JG,1),RXND(JR,JG,1),RXZD(JR,JG,1), &
                                         RXDIP(JR,JG),RXAZ(JR,JG),RXMNT(JR,JG)
           WRITE(NW,'(I4,3F13.2,2F8.1,F12.1)') JR,RXED(JR,JG,1),RXND(JR,JG,1),RXZD(JR,JG,1), &
                                               RXDIP(JR,JG),RXAZ(JR,JG),RXMNT(JR,JG)
         END DO
       END IF
     ELSE IF (RX_TYPE(JG) == 2) THEN
       WRITE(NW,19)
       MQVR = 4
       DO JR = 1, NRX(JG)
         DO JV = 1,4
           READ (NDR,*) RXED(JR,JG,JV),RXND(JR,JG,JV),RXZD(JR,JG,JV)
           IF (NEW) WRITE(ND,'(3F13.2)') RXED(JR,JG,JV),RXND(JR,JG,JV),RXZD(JR,JG,JV)

         END DO
         WRITE(NW,'(/I4,3F13.2)') JR,RXED(JR,JG,1),RXND(JR,JG,1),RXZD(JR,JG,1)
         DO JV = 2,4
           WRITE(NW,'(4X,3F13.2)')  RXED(JR,JG,JV),RXND(JR,JG,JV), RXZD(JR,JG,JV)
         END DO
       END DO

     ELSE IF (RX_TYPE(JG) == 3) THEN
       WRITE(NW,20)
       MQVR = MAX (MQVR,2)
       DO JR = 1, NRX(JG)
         DO JV = 1,2
           READ (NDR,*) RXED(JR,JG,JV),RXND(JR,JG,JV),RXZD(JR,JG,JV)
           IF (NEW) WRITE(ND,'(3F13.2)') RXED(JR,JG,JV),RXND(JR,JG,JV),RXZD(JR,JG,JV)
         END DO
         WRITE(NW,'(I4,6F13.2)') JR,RXED(JR,JG,1),RXND(JR,JG,1),RXZD(JR,JG,1), &
                                      RXED(JR,JG,2),RXND(JR,JG,2),RXZD(JR,JG,2)
       END DO
     END IF
   END DO
   MRX = MAXVAL (NRX)
 END IF

 LRX = 1
 IF (SURVEY_TYPE == 1) THEN
   READ(NDR,*) NEVENTS
   IF (NEW) WRITE(ND,'(I4)') NEVENTS
   WRITE(NW,30) NEVENTS
   IF (NEVENTS < 1) THEN
     CALL WRYT_LOG_FILE (NLG,12,MXERR,3)
   ELSE IF (NEVENTS < NTX) THEN
     CALL WRYT_LOG_FILE (NLG,13,MXERR,1)
   END IF

   ALLOCATE (RXG_INDX(NEVENTS), TX_INDX(NEVENTS))
   DO JS = 1, NEVENTS
     READ(NDR,*) TX_INDX(JS), RXG_INDX(JS)
     IF (NEW) WRITE(ND,'(2I4)') TX_INDX(JS), RXG_INDX(JS)
     WRITE(NW,'(I6,2I8,I7)') JS, TX_INDX(JS), RXG_INDX(JS), NRX(RXG_INDX(JS))
   END DO

!  Set up transmitter indexing system based on the events.
!  First count the number of transmitters used in events for dimensioning purposes.

   NTXE = 0
   TX_CNT: DO JS = 1,NTX   !  Compute the actual number of transmitters in use
     DO J = 1,NEVENTS
       IF (TX_INDX(J) == JS) THEN
         NTXE = NTXE + 1
         CYCLE TX_CNT
       END IF
     END DO
   END DO TX_CNT

   ALLOCATE (NRXTX(NTXE), TXID(NTXE), RGTXID(NRXG,NTXE), NRGTX(NTXE))
   NRXTX = 0; TXID = 0; RGTXID = 0; NRGTX = 0
   KS = 0

   DO JS = 1,NTX   !  Compute the actual number of transmitters in use
     NEVNT_TX = 0

     DO J = 1,NEVENTS
       IF (TX_INDX(J) == JS) THEN
         NEVNT_TX = NEVNT_TX + 1   ! The number of events using transmitter Tx
         IF (NEVNT_TX == 1) THEN   ! Use KS as the new transmitter index of
           KS = KS + 1             !   transmitters that are actually used.
           TXID(KS) = JS           ! Identify new Tx index with old one
         END IF                    ! Don't count transmitters more han once

         NRGTX(KS) = NEVNT_TX                  ! Up date the number of rexeiver groups new Tx index, KS
         RGTXID(NEVNT_TX,KS) = RXG_INDX(J)     ! Identify Rx Group number
         NRXTX(KS) = NRXTX(KS) + NRX (RXG_INDX(J)) ! Update total number of Rx for transmitter index KS
       END IF
     END DO
   END DO
   LRX = MAXVAL (NRXTX)
   DEALLOCATE (RXG_INDX, TX_INDX)

 ELSE
   NCMP = 1
   IF (RX_TYPE(1) == 1) NCMP = 3
   UNITS = UNITS(1)
 END IF

!  Reset dimensions for transmitters

 ALLOCATE (Q2N(MXVRTX,NTXE,2), Q2E(MXVRTX,NTXE,2), Q2Z(MXVRTX,NTXE,2), CLCD(3,NTXE) )
 CLCD = 0.D0

 DO JS1 = 1, NTXE
   JS = JS1
   IF (SURVEY_TYPE == 1) JS = TXID(JS1)
   Q2N(1:MXVRTX,JS1,1) = SXND(1:MXVRTX,JS)
   Q2E(1:MXVRTX,JS1,1) = SXED(1:MXVRTX,JS)
   Q2Z(1:MXVRTX,JS1,1) = SXZD(1:MXVRTX,JS)

   Q2N(1,JS1,2) = SXAZ(JS)
   Q2E(1,JS1,2) = SXDIP(JS)
   Q2Z(1,JS1,2) = SXMNT(JS)

 END DO
 DEALLOCATE (SXND,SXED,SXZD,SXAZ,SXDIP,SXMNT)

 ALLOCATE (SXED(MXVRTX,NTXE), SXND(MXVRTX,NTXE), SXZD(MXVRTX,NTXE), &
           SXE(MXVRTX,NTXE),  SXN(MXVRTX,NTXE),  SXZ(MXVRTX,NTXE),  &
           SXDIP(NTXE),       SXAZ(NTXE),        SXMNT(NTXE))

 SXE =  0.;  SXN  = 0.;  SXZ  = 0.  !  Centred system to be defined later

 DO JS = 1, NTXE
   SXND(1:MXVRTX,JS) = Q2N(1:MXVRTX,JS,1)
   SXED(1:MXVRTX,JS) = Q2E(1:MXVRTX,JS,1)
   SXZD(1:MXVRTX,JS) = Q2Z(1:MXVRTX,JS,1)

   SXAZ(JS)  = REAL (Q2N(1,JS,2) )
   SXDIP(JS) = REAL (Q2E(1,JS,2) )
   SXMNT(JS) = REAL (Q2Z(1,JS,2) )
 END DO
 DEALLOCATE (Q2N, Q2E, Q2Z)

 IF (SURVEY_TYPE == 1) THEN ! Reset receivers

   ALLOCATE (Q2N(MRX,NRXG,MQVR+1), Q2E(MRX,NRXG,MQVR+1), Q2Z(MRX,NRXG,MQVR+1) )

   DO JG = 1, NRXG
     DO JV = 1,MQVR
       Q2N(1:MRX,JG,JV) = RXND(1:MRX,JG,JV)
       Q2E(1:MRX,JG,JV) = RXED(1:MRX,JG,JV)
       Q2Z(1:MRX,JG,JV) = RXZD(1:MRX,JG,JV)
     END DO
     Q2N(1:MRX,JG,MQVR+1) = RXAZ(1:MRX,JG)
     Q2Z(1:MRX,JG,MQVR+1) = RXDIP(1:MRX,JG)
     Q2E(1:MRX,JG,MQVR+1) = RXMNT(1:MRX,JG)
   END DO
   DEALLOCATE (RXND,RXED,RXZD,RXAZ,RXDIP,RXMNT)

   ALLOCATE (RXED(MRX,NRXG,MQVR), RXND(MRX,NRXG,MQVR), RXZD(MRX,NRXG,MQVR), &
              RXE(MRX,NRXG,MQVR),  RXN(MRX,NRXG,MQVR),  RXZ(MRX,NRXG,MQVR), &
              RXDIP(MRX,NRXG),  RXAZ(MRX,NRXG), RXMNT(MRX,NRXG))

   RXE = 0.; RXN = 0.; RXZ = 0.

   DO JG = 1, NRXG
     DO JV = 1,MQVR
       RXND(1:MRX,JG,JV) = Q2N(1:MRX,JG,JV)
       RXED(1:MRX,JG,JV) = Q2E(1:MRX,JG,JV)
       RXZD(1:MRX,JG,JV) = Q2Z(1:MRX,JG,JV)
     END DO
     RXAZ(1:MRX,JG)  = REAL (Q2N(1:MRX,JG,MQVR+1) )
     RXDIP(1:MRX,JG) = REAL (Q2Z(1:MRX,JG,MQVR+1) )
     RXMNT(1:MRX,JG) = REAL (Q2E(1:MRX,JG,MQVR+1) )
   END DO

 ELSE IF (SURVEY_TYPE == 2 .OR. SURVEY_TYPE == 3) THEN

! Convert receiver offsets to absolute positions (REAL*8)

   RXMNT = RXFMNT
   RXAZ = 0
   RXDIP = 0
   IF (SOURCE_TYPE == 1) THEN
     DO JS = 1,NTXE
       JV = N_VRTX(JS)
       QD = SUM (SXED(1:JV,JS)) / REAL (JV,8)
       RXED(1,JS,1) = QD + REAL (RXOE,8)
       QD = SUM (SXND(1:JV,JS)) / REAL (JV,8)
       RXND(1,JS,1) = QD + REAL (RXON,8)
       QD = SUM (SXZD(1:JV,JS)) / REAL(JV,8)
       RXZD(1,JS,1) = QD + REAL (RXOZ,8)
     END DO

   ELSE IF (SOURCE_TYPE == 3) THEN
     RXED(1,1:NTX,1) = SXED(1,1:NTX) + REAL (RXOE,8)
     RXND(1,1:NTX,1) = SXND(1,1:NTX) + REAL (RXON,8)
     RXZD(1,1:NTX,1) = SXZD(1,1:NTX) + REAL (RXOZ,8)
     IF (CMP(1) > 0) THEN
       BHR(1,1:NTXE) = .TRUE.
       RXAZ(1,1:NTX) = SXAZ(1:NTX)
       RXDIP(1,1:NTX) = SXDIP(1:NTX)
       BHAZ(1,1:NTX) = SXAZ(1:NTX) * PI / 180.

!  Convert from borehole convention to computation convention.
       BHDIP(1,1:NTXE) = (90. - SXDIP(1:NTXE)) * PI / 180.
!-------------------------------------------------------
     END IF
   END IF
 END IF

 PRTCMP = 0; NCMPG = 1
 PRTCMP(1,1:NRXG) = 1  ! Put default response into component 1

 DO JG = 1,NRXG
   IF (RX_TYPE(JG) /= 1 .OR. SURVEY_TYPE == 4) CYCLE         !  Only magnetic dipole receivers have alternatives
   IF (CMP(JG) /= 1) NCMPG(JG) = 3

! For unrotated systems the following will result in printing out the
! vertical component first followed by the east and north components.
! For rotated systems, the axial component will be printed out first
! followed by the Slope and Horizontal components.
! Note that for constant offset systems, if the user chooses to print
! only one component, this will be the vertical one for unrotated
! systems and the maximally coupled component for rotated systems.

   PRTCMP(1,JG) = 3   !  Print vertical component first
   PRTCMP(2,JG) = 2   !  followed by east component
   PRTCMP(3,JG) = 1   !  followed by north component

   IF (CMP(JG) == 2) THEN              !  Use Axial, Slope & Horizontal borehole coordinates
     PRTCMP(1,JG) = 6                  !  Print the axial component, first
     PRTCMP(2,JG) = 5                  !  followed by the Slope component
     PRTCMP(3,JG) = 4                  !  followed by the Horizontal component
   END IF
   IF (CMP(JG) == 1 .AND. SOURCE_TYPE == 3) PRTCMP(1,JG) = 6
 END DO

!  For Survey Type 1, assign the receiver type, number of computed components
!  the dip and azimuth of the receivers in a given receiver group to the
!  appropriate receivers belonging to each transmitter

 IF (SURVEY_TYPE == 1) THEN
   ALLOCATE (NCMP(LRX,NTXE), RXID(LRX,NTXE), BHR(LRX,NTXE), BHAZ(LRX,NTXE), BHDIP(LRX,NTXE))
   BHR = .FALSE.; BHDIP = 0.; BHAZ = 0.
   NCMP = 1                             !  Set the number of components for each receiver (1 or 3)

   DO JS = 1, NTXE
     KRFIN = 0
     DO JRG = 1, NRGTX(JS)
       KRG = RGTXID(JRG,JS)    ! Receiver Group index for group JRG for Tx JS
       NRX1 = NRX(KRG)         ! Number of receivers in receiver group KRG
       DO JR = 1,NRX1
         JRP = JR + KRFIN
         RXID(JRP,JS) = RX_TYPE(KRG)
         IF (RX_TYPE(KRG) == 1) NCMP(JRP,JS) = 3

         IF (CMP(KRG) == 2) THEN
           BHR(JRP,JS) = .TRUE.
           BHAZ(JRP,JS) = RXAZ(JR,KRG) * PI / 180.

!  Convert from borehole convention to computation convention.
           BHDIP(JRP,JS) = (90. - RXDIP(JR,KRG)) * PI / 180.
!-------------------------------------------------------
         END IF
       END DO
       KRFIN = KRFIN + NRX1  ! Set index for next group
     END DO                  ! End of Rx groups for transmitter JS
   END DO
 END IF

  1 Format (/, 2x, 78('-'), &
    /, 2x, '| Program:  ', a, &
    /, 2x, '| Version:  ', a, &
    /, 2x, '| Date:     ', a, &
    /, 2x, '|', &
    /, 2x, '| Authors:  ', a, &
    /, 2x, '|           ', a, &
    /, 2x, '| Contact:  ', a, &
    /, 2x, '| Project:  ', a, &
    /, 2x, '|', &
    /, 2x, '| Compiler: ', a, &
    /, 2x, '| Options:  ', a, &
    /, 2x, '|', &
    /, 2x, '| Started:  ', i2.2, '/', i2.2, '/', i4.4, ' at ', i2.2, ':', i2.2, ':', i2.2, &
    /, 2x, 78('-'), /)
 3 FORMAT(/T3,'TDFD =',I2,';   PRFL =',I2,';   STEP =',I2,';   ISTOP =',I2)
 4 FORMAT(/10X,'+-----------------------------------------+' &
          /10X,'+  Time-Domain Ground System Information  +' &
          /10X,'+-----------------------------------------+' &
         // T3,'DO3D =',I3,';   NSX =',I4,';   NCHNL =',I4, &
         /T3,'KRXW =',I2,';    OFFTYM =',G12.4)
 5 FORMAT(//T14,'TXON (ms)    Transmitter current (amps)' &
           /T14,'---------    --------------------------'/)
 6 FORMAT(//T10,'Receiver Window Specifications (ms - referenced to signal origin)'/ &
            T10,'----------------------------------------------------------------'// &
            T8,'Window',T19,'Open',T30,'Close',T42,'Width',T52,'Centre'/ &
            T8,'------',T19,'----',T30,'-----',T42,'-----',T52,'------')
 7 FORMAT(/T3,'MINFRQ =',G12.4,' Hz.  MAXFRQ =',G12.4,' Hz.    MXFRQE =',G12.4,' Hz.')
 8 FORMAT(/10X,'+----------------------------------------------+' &
          /10X,'+  Frequency-Domain Ground System Information  +' &
          /10X,'+----------------------------------------------+' &
          //T3,'NFRQ =',I3)
 9 FORMAT(/T12,'Frequency      Transmitter current in amps', &
          /T12,'---------      ---------------------------'/)
10 FORMAT(//T3,'SOURCE_TYPE =',I2,';   SURVEY_TYPE =',I2,';   NTX =',I3,';   NRXG =',I2/)
11 FORMAT(/T3,'Vertex Locations for Loop Sources' &
          /T3,'---------------------------------')
12 FORMAT(/T3,'Vertex Locations for Grounded Wire Sources' &
          /T3,'------------------------------------------')
13 FORMAT(/T3,'Transmitter',I3,' has',I3,' vertices.' &
         //T13,'Easting      Northing      Elevation' &
          /T13,'-------      --------      ---------')  !^^^^^^^^^^^^^^^^^
14 FORMAT(/T3,'Dipole Source Specification' &
         //T12,'Easting       Northing      Elevation     Dip   Azimuth   Moment' &
          /T12,'-------       --------      ---------     ---   -------   ------')
15 FORMAT(/T11,'Magnetic Dipole Receivers' &
          /T11,'-------------------------' &
         //T11,'Easting     Northing     Elevation   Dip    Azimuth    Moment' &
          /T11,'-------     --------     ---------   ---    -------    ------')
16 FORMAT(/T3,'Fixed Offset for Magnetic Dipole Receiver', &
          /T3,'-----------------------------------------' &
          /T6,'East        North     Vertical   Moment    CMP  UNITS'/4F11.2,2I6)
17 FORMAT(//T3,'Rx Group:',I3,';   NRX =',I3,';   Type =',I2,';   CMP =',I2,';   Units =',I2)
18 FORMAT(/T11,'Magnetic Dipole Receivers' &
          /T11,'-------------------------' &
         //T11,'Easting     Northing      Elevation   Moment' &
          /T11,'-------     --------      ---------   ------')  !^^^^^^^^^^^^^^^^^
19 FORMAT(/T11,'Receiver Loop Vertices' &
          /T11,'----------------------' &
         //T11,'Easting     Northing      Elevation' &
          /T11,'-------     --------      ---------') !^^^^^^^^^^^^^^^^
20 FORMAT(/T12,'Electric Dipole Receiver Electrodes' &
          /T12,'-----------------------------------' &
         //T12,'East 1      North 1     Elevation 1    East 2      North 2     Elevation 2' &
          /T12,'------      -------     -----------    ------      -------     -----------') !^^^^^^^^^^^^
25 FORMAT(/T10,'Receiver channel origin INPUT is shifted by', F9.3,' ms from signal origin.' &
         //T10,'Receiver Window Specifications (ms - referenced to signal origin)'/ &
            T10,'----------------------------------------------------------------'// &
            T62,'Referenced'/ &
            T8,'Window',T19,'Open',T30,'Close',T42,'Width',T52,'Centre',T64,'Centre'/ &
            T8,'------',T19,'----',T30,'-----',T42,'-----',T52,'------',T64,'------')
30 FORMAT(/T5,'This survey is composed of',I3,' events.' &
         //T14,'Tx   Rx Group'/T4,'Event   Index  Index    NRX'/)
90 FORMAT(/T5,'Dipole transmitters must have azimuth = 90 degrees or  -90 degrees.' &
          /T5,'The azimuth of transmitter',I3,' has been changed to',F6.0,' degrees.')
97 FORMAT(//T3,'Arjuna task started at ',I2.2,':',I2.2,' on',I3.2,1X,A,I5//)

   END SUBROUTINE READ_SYSTEM_DATA

   SUBROUTINE READ_MODEL_DATA
!  --------------------------

!***  Called by MAIN
!***  Calls SET_CELL

 IMPLICIT NONE

 INTEGER TOT_NODES, KE, KZ, J1, NEL, NER, NZB, I, J, K,  &
         DSE, DEE, DSZ, DEZ,ILYTH,L
 INTEGER, ALLOCATABLE :: LITHM(:,:)
 REAL ZLOCTMP
 REAL ZAIR(50)
 CHARACTER(LEN=5) MARK
 DATA MARK/'QQQQQ'/
 DATA (ZAIR(I),I=1,11)/+10000,1000,400,300,200,130,80,40,20,10,5/

 NEL = 2 ; NER = 2 ; NAIR = 11 ; NZB = 2

 WRITE(NW,1)
 READ(NDR,*) NSZ, NEAST, NLITH, KACC, SOLVER, OUTPUT
 IF (KACC /= 1) KACC = 1
 IF (SOLVER /= 1) SOLVER = 1
 WRITE(NW,2) NSZ, NEAST, NLITH, KACC, SOLVER, OUTPUT
 IF (NEW) WRITE(ND,'(7I5)') NSZ, NEAST, NLITH, KACC, SOLVER, OUTPUT
 IF (OUTPUT < 0) THEN
   OUTPUT = -10
 ELSE
   OUTPUT = 10
 END IF

 ALLOCATE (LYTH(NLITH+1,NPROP))

!  Initialise lithology list.

 LYTH(1:NLITH+1, 1) = 0.    !  domain resistivity indicator
 LYTH(1:NLITH+1, 2) = -1.   !  conductance (SIG_T) indicator (not needed for loki)
 LYTH(1:NLITH+1, 3) = 1.    !  Relative magnetic permeabilities
 LYTH(1:NLITH+1, 4) = 1.    !  Relative dielectric constants
 LYTH(1:NLITH+1, 5) = 0.    !  Chargeabilities
 LYTH(1:NLITH+1, 6) = 0.    !  CTAUs
 LYTH(1:NLITH+1, 7) = 1.    !  CFREQs

 DO J = 1,NLITH
   READ (NDR,*) LYTH(J,1:NPROP)
   IF (NEW) WRITE(ND,'(7G14.5)') LYTH(J,1:NPROP)
   WRITE(NW,'(I4,T8,G12.4,T22,F7.1,F12.3,F11.3,F10.2,G12.3,F8.2)') J,LYTH(J,1:NPROP)
   IF (LYTH(J,3) < 0.01) LYTH(J,3) = 1.   ! Default RMU
   IF (LYTH(J,4) < 0.01) LYTH(J,4) = 1.   ! Default REPS
   IF (LYTH(J,5) < 1.E-3 .OR. LYTH(J,6) < 1.E-15 .OR. LYTH(J,7) < 1.E-6)  THEN
     LYTH(J,5) = 0   ! default CHRG
     LYTH(J,6) = 0   ! default CTAU
     LYTH(J,7) = 1   ! default CFRQ
   END IF
 END DO

 LYTH(NLITH+1, 1) = 1.E8  !  air resistivity indicator
 LYTH(NLITH+1, 2) = -1.   !  air conductance (SIG_T) indicator
 LYTH(NLITH+1, 3) = 1.    !  air Relative magnetic permeabilities
 LYTH(NLITH+1, 4) = 1.    !  air Relative dielectric constants
 LYTH(NLITH+1, 5) = 0.    !  air Chargeabilities
 LYTH(NLITH+1, 6) = 0.    !  air CTAUs
 LYTH(NLITH+1, 7) = 1.    !  air CFREQs

 TOT_NODES = NEAST * NSZ
 IF (TOT_NODES > 60000)   CALL WRYT_LOG_FILE (NLG,20,MXERR,1)

 NE = NEAST + NEL + NER
 NZ = NSZ + NZB + NAIR
 NEZ = (NE-1) * (NZ-1)
 NPN = (2*NE-1)*(2*NZ-1) - NEZ

!############################################################

!  CRUCIAL NOTE
!  ------------

!  The user interface will read in location indices I,K,J
!  ordered as NORTH, VERTICAL, EAST
!  Arjuna will refer to these as KN, KZ, KE, conforming
!  to the right hand assignment of

!  X = North;   Y = East;    Z = down

!  Nodes locations are indexed in the right hand system; eg,
!  the triplet KN,KE,KZ

!############################################################

 ALLOCATE (ELOCD(NE,NZ), ZLOCD(NE,NZ),LITH(NE,NZ), &
           ESURFD(NEAST),ZSURFD(NEAST), LNODS(NEZ,8),COORD(NPN,2) )

 ELOCD = 0. ; ZLOCD = 0. ; LITH = 0

 IF (DO3D == 2)  RETURN
 DO J1 =  1, TOT_NODES
    READ(NDR,*) KZ,KE,ZLOCTMP,ELOCD(NEL+KE,NAIR+KZ), ILYTH
    ZLOCD(NEL+KE,NAIR+KZ) = ZLOCTMP
    LITH(NEL+KE,NAIR+KZ)  = ILYTH

    IF (KZ == 1) THEN   ! Set up surface node array
      ESURFD(KE) =  ELOCD(NEL+KE,NAIR+1)
      ZSURFD(KE) =  ZLOCD(NEL+KE,NAIR+1)
    END IF
 END DO

! Extend the mesh and the resistivities to the left, right, top and bottom

 DSE = NEL  + 1 ; DEE = NE - NER
 DSZ = NAIR + 1 ; DEZ = NZ - NZB

 ELOCD(DSE-1,1:NZ) = ELOCD(DSE,1:NZ) -  1000.D0
 ELOCD(DSE-2,1:NZ) = ELOCD(DSE,1:NZ) - 11000.D0
 ELOCD(DEE+1,1:NZ) = ELOCD(DEE,1:NZ) +  1000.D0
 ELOCD(DEE+2,1:NZ) = ELOCD(DEE,1:NZ) + 11000.D0
 ZLOCD(DSE-1,1:NZ) = ZLOCD(DSE,1:NZ)
 ZLOCD(DSE-2,1:NZ) = ZLOCD(DSE,1:NZ)
 ZLOCD(DEE+1,1:NZ) = ZLOCD(DEE,1:NZ)
 ZLOCD(DEE+2,1:NZ) = ZLOCD(DEE,1:NZ)

 ELOCD(1:NE,DEZ+1) = ELOCD(1:NE,DEZ)
 ELOCD(1:NE,DEZ+2) = ELOCD(1:NE,DEZ)
 ZLOCD(1:NE,DEZ+1) = ZLOCD(1:NE,DEZ) -  1000.D0
 ZLOCD(1:NE,DEZ+2) = ZLOCD(1:NE,DEZ) - 11000.D0

 DO K = 1, NAIR
    ELOCD(1:NE,K) = ELOCD(1:NE,DSZ)
    ZLOCD(1:NE,K) = ZLOCD(1:NE,DSZ) + ZAIR(K)
 END DO

 ECNTRD = 0.5D0 * (MAXVAL (ELOCD) + MINVAL (ELOCD) )
 NCNTRD = 0.D0

 IF (ABS(ECNTRD) < 2000.D0) ECNTRD = 0.D0

! Convert arrays into computation coordinates (singular precision)

 ALLOCATE (ELOC (NE,NZ),ZLOC(NE,NZ), ESURF(NEAST),ZSURF(NEAST) )

 ELOC  =  REAL (ELOCD - ECNTRD)
 ZLOC  = -REAL (ZLOCD)
 ESURF =  REAL (ESURFD - ECNTRD)
 ZSURF =  REAL (ZSURFD)

 DEALLOCATE (ELOCD, ZLOCD, ESURFD, ZSURFD )

 LITH(DSE-2, 1:NZ-1) = LITH(DSE,   1:NZ-1)
 LITH(DSE-1, 1:NZ-1) = LITH(DSE,   1:NZ-1)
 LITH(DEE,   1:NZ-1) = LITH(DEE-1, 1:NZ-1)
 LITH(DEE+1, 1:NZ-1) = LITH(DEE-1, 1:NZ-1)
 LITH(1:NE-1,DEZ)    = LITH(1:NE-1,DEZ-1)
 LITH(1:NE-1,DEZ+1)  = LITH(1:NE-1,DEZ-1)

 DO K = 1, NAIR
    LITH(1:NE-1,K) = NLITH + 1
 END DO

 ALLOCATE (LITHM(NE,NZ))
 LITHM = 0
 DO K = 1, NZ-1
    DO I = 1, NE-1
       LITHM(I,K) = LITH(I,K)
    END DO
 END DO
!
! not any more ... 
! CALL LITHMAP (NW,NZ,NE,LITHM)
 DEALLOCATE ( LITHM )

!  Element numbering

 L = 0
 K = 1
 DO KE = 1, NE-1
    DO KZ = 1, NZ-1
       L = L + 1
       LNODS(L,1) = K
       LNODS(L,8) = LNODS(L,1) + 1
       LNODS(L,7) = LNODS(L,1) + 2
       LNODS(L,3) = LNODS(L,1) + (2*NZ - 1) + NZ
       LNODS(L,4) = LNODS(L,3) + 1
       LNODS(L,5) = LNODS(L,3) + 2
       K = K + 2
    END DO
    K = K + (NZ+1)
 END DO

 KZ = 0
 DO I = 1, (NE-1)*(NZ-1)
   LNODS(I,2) = LNODS(I,1) + (2*NZ-1) - KZ
   LNODS(I,6) = LNODS(I,2) + 1
   KZ = KZ + 1
   IF (MOD(I,NZ-1) == 0)   KZ = 0
 END DO

! Calculate element coordinates

 L = 1
 DO KE = 1, NE
   DO KZ = 1, NZ
     COORD(L,1) =  ELOC(KE,KZ)
     COORD(L,2) =  ZLOC(KE,KZ)
     L = L + 2
   END DO
   L = L + (NZ-1)
 END DO

 DO K = 1, 2
   DO I = 1, (NE-1)*(NZ-1)
     COORD(LNODS(I,2),K) = (COORD(LNODS(I,1),K) + COORD(LNODS(I,3),K)) / 2.
     COORD(LNODS(I,4),K) = (COORD(LNODS(I,3),K) + COORD(LNODS(I,5),K)) / 2.
     COORD(LNODS(I,6),K) = (COORD(LNODS(I,5),K) + COORD(LNODS(I,7),K)) / 2.
     COORD(LNODS(I,8),K) = (COORD(LNODS(I,7),K) + COORD(LNODS(I,1),K)) / 2.
   END DO
 END DO

! Marking the freq.-save file

 WRITE(ND,'(A5)') MARK

 1 FORMAT(//T6,'MESH AND LITHOLOGIES DESCRIPTION' &
           /T6,'----------------------------------------')
 2 FORMAT(/T3,'Number of node rows =',I4,             &
          /T3,'Number of node columns =',I4,          &
          /T3,'NLITH =',I3,4X,'KACC =',I2,4X,'SOLVER =',I2,4X,'OUTPUT =',I4/)

   END SUBROUTINE READ_MODEL_DATA

    SUBROUTINE LITHMAP (NW,NZ,NE,LITHM)
!   ----------------------------------

! produces a 2D character map of lithologies.
! NE & NZ are the number of cells in the East and depth directions respectively.
! LITH(J,I) = lithology associated with node in column I, row J.

!**** Called by READ_2D_DATA

    IMPLICIT NONE
    INTEGER NW,NZ,NE,I,J,LITHM(NE,NZ)
    CHARACTER (LEN=1) LITHCHAR(52), MAP(NE,NZ)
    DATA LITHCHAR /'1','2','3','4','5','6','7','8','9','0','A','B','C','D','E','F', &
                   'G','H','J','K','L','M','N','P','R','S','T','U','W','X','Y','a', &
                   'b','c','d','e','f','g','h','j','k','l','m','n','p','r','s','t', &
                   'u','w','x','y'/

    WRITE(NW,'(/T15,A)') '2D LITHOLOGY MAP'
    WRITE(NW,'(T15,A)')   '---------------'
    WRITE(NW,'(T10,A/)') '(reduced to rectangular grid)'

    MAP = '_'
    DO I = NAIR+1, NZ-1
      DO J = 1, NE-1
        MAP(J,I) = LITHCHAR(LITHM(J,I))
      END DO
      WRITE(NW,'(2X,4096A)') MAP(1:NE-1,I)
    END DO

   END SUBROUTINE LITHMAP

   SUBROUTINE SHOW_AND_TELL
!  ------------------------

! Prints out arrays and model in model-centred coordinates

!*** Called by MAIN

 IMPLICIT NONE
 INTEGER KRX

 WRITE (NW,1) ECNTRD,NCNTRD

 IF (SOURCE_TYPE < 3) THEN
   IF (SOURCE_TYPE == 1) WRITE(NW,2)
   IF (SOURCE_TYPE == 2) WRITE(NW,3)
   DO JS = 1,NTXE
     WRITE(NW,4) JS,N_VRTX(JS)
     DO JV = 1, N_VRTX(JS)
       WRITE(NW,'(I5,3F11.2)') JV,SXE(JV,JS),SXN(JV,JS),SXZ(JV,JS)
     END DO
   END DO
 ELSE IF (SOURCE_TYPE == 3) THEN
   WRITE(NW,5)
   DO JS = 1,NTXE
     WRITE(NW,'(I4,3F11.2,3F10.2)') JS,SXE(1,JS),SXN(1,JS),SXZ(1,JS),SXDIP(JS),SXAZ(JS),SXMNT(JS)
   END DO
   SXAZ = SXAZ * 3.14159 / 180.
   SXDIP = (90. - SXDIP) * 3.14159 / 180.
 END IF

 IF (SURVEY_TYPE == 1) THEN
   DO JG = 1,NRXG
     KRX = NRX(JG)
     IF (RX_TYPE(JG) == 1) THEN
       WRITE(NW,6) JG
       DO JR = 1, KRX
         WRITE(NW,'(I4,3F11.2,2F7.1,F10.0)') JR,RXE(JR,JG,1),RXN(JR,JG,1),RXZ(JR,JG,1), &
                                    RXDIP(JR,JG),RXAZ(JR,JG),RXMNT(JR,JG)
       END DO
     ELSE IF (RX_TYPE(JG) == 2) THEN
       WRITE(NW,7) JG
       DO JR = 1, KRX
         WRITE(NW,'(/I4,3F11.2)') JR,RXE(JR,JG,1),RXN(JR,JG,1),RXZ(JR,JG,1)
         DO JV = 2,4
           WRITE(NW,'(4X,3F11.2)')  RXE(JR,JG,JV),RXN(JR,JG,JV), RXZ(JR,JG,JV)
         END DO
       END DO
     ELSE IF (RX_TYPE(JG) == 3) THEN
       WRITE(NW,8) JG
       DO JR = 1, KRX
         WRITE(NW,'(I4,6F11.2)') JR,RXE(JR,JG,1),RXN(JR,JG,1),RXZ(JR,JG,1), &
                                    RXE(JR,JG,2),RXN(JR,JG,2),RXZ(JR,JG,2)
       END DO
     END IF
   END DO

   WRITE(NW,30)
   DO JS = 1,NTXE
     WRITE(NW,'(/T3,I3,T17,I3,T30,I3,T39,I3)') TXID(JS),JS,RGTXID(1,JS),NRX(RGTXID(1,JS))
     DO JR = 2,NRGTX(JS)
       WRITE(NW,'(T30,I3,T39,I3)') RGTXID(JR,JS),NRX(RGTXID(JR,JS))
     END DO
   END DO

 ELSE IF (SURVEY_TYPE == 2 .OR. SURVEY_TYPE == 3) THEN
   IF (SURVEY_TYPE == 2) WRITE(NW,9)
   IF (SURVEY_TYPE == 3) WRITE(NW,11)
   DO JG = 1,NTXE
     WRITE(NW,'(I4,3F11.2,2F7.1,F10.0)') JG,RXE(1,JG,1),RXN(1,JG,1),RXZ(1,JG,1), &
                                          RXDIP(1,JG),RXAZ(1,JG),RXMNT(1,JG)
   END DO
 END IF

 WRITE(NW,35)                 ! End of input data description

 1 FORMAT(//T3,'Before computation begins, Arjuna transforms array and model coordinates from' &
           /T3,'"real world" coordimnates where elevation increases positive upwards to a'   &
           /T3,'body- centred system where depth increases positive downwards.  The new ' &
           /T3,'vertical origin is at the air-earth interface.  The new horizontal origin is' &
           /T3,'over the centre of the model region.  In the original user defined system' &
           /T3,'the new computation origin is located at:' &
          //T4,'EAST: ',F12.2,';    NORTH: ',F12.2,';    ELEVATION: ',F8.2)

 2 FORMAT(/T3,'Transformed Vertex Locations for Loop Sources' &
          /T3,'---------------------------------------------')
 3 FORMAT(/T3,'Transformed Vertex Locations for Grounded Wire Sources' &
          /T3,'------------------------------------------------------')
 4 FORMAT(/T7,'Transmitter',I3,' has',I3,' vertices.'&
         //T10,'Easting   Northing     Depth'/)
 5 FORMAT(/T3,'Transformed Dipole Source Specification'// &
         //T9,'Easting   Northing      Depth      Dip    Azimuth   Moment' &
          /T9,'-------   --------      -----      ---    -------   ------')
 6 FORMAT(//T3,'Transformed Locations for Magnetic Dipole Receivers in Rx Group',I3 &
           /T3,'------------------------------------------------------------------' &
           /T9,'Easting   Northing     Depth    Dip    Azimuth   Moment'/)
 7 FORMAT(//T3,'Transformed Vertices for Loop Receivers in Rx Group',I3 &
           /T3,'------------------------------------------------------' &
          /T9,'Easting   Northing     Depth ')
 8 FORMAT(//T3,'Transformed Electrode Positions for Electric Dipoles in Rx Group',I3 &
           /T3,'-------------------------------------------------------------------' &
          /T10,'East 1    North 1    Depth 1     East 2    North 2    Depth 2'/)
 9 FORMAT(//T3,'Transformed Locations for Constant Offset Magnetic Dipole Receivers' &
           /T3,'-------------------------------------------------------------------' &
           /T9,'Easting   Northing     Depth    Dip    Azimuth   Moment'/)
 11 FORMAT(//T3,'Transformed Locations for Central Loop Magnetic Dipole Receivers' &
           /T3,'-----------------------------------------------------------------')
 30 FORMAT(//T7,'Revised transmitter indexing'/T7,'----------------------------' &
           //T5,'Old          New        Receiver  Group' &
            /T3,'Tx Index     Tx Index     Groups     NRX')
 35 FORMAT(/75('-')/T24,'END OF INPUT DATA DESCRIPTION'/75('-'))

   END SUBROUTINE SHOW_AND_TELL

   SUBROUTINE SET_FRQ
!  ------------------

!***  Called by MAIN program

!  For time-domain options for B or dB/dt:
!
!  Numerical experiments where the frequency-domain responses of layered
!  layered 1/2 space models were transformed to time-domain for a wide
!  range of resistivities has indicated that 6 points per decade is
!  adequate frequency discretisation, even at the high end.  Moreover,
!  for perfect frequency-domain data, only 3 frequencies are required
!  from 1 to 10 Hz to maintain an accuracy of better than .1 percent for
!  the models studied.  This means that 28 frequencies are needed for the
!  1Hz to 100kHz range.
!
!  The imaginary component of B divided by iw is extrapolated back to
!  zero frequency using the constant value at freq = 1Hz.
!
!  The need to go to 1 MHz depends upon receiver channels and conductivity.
!  A 10,000 ohm-m 1/2 space requires extended range if the earliest window
!  opening is within .28 ms of signal turn-off.  A 1 ohm-m 1/2 space allows
!  normal range if the first window open time is > .002 ms.
!
!  In a 3D program, this is pretty hard to control because of non-uniform
!  resistivity distribution so the safe option of going to 1 MHz is chosen
!  if the first window opens earlier than .2 ms after signal turn-off.  This
!  requires computation for 34 frequencies.
!
!  This can be over-ridden by setting ISTOP = -1 in which case the program
!  will compute 43 frequency-domain responses at 6 points per decade from
!  .1 Hz to 1MHz.
!
!  For a truly tedious experience, the user can force frequency computations
!  at 12 points per decade from 1 Hz to 1 MHz by entering ISTOP = -2 but this
!  is excessive, especially for a program which takes as long as Arjuna
!  to run.
!
!  Although only 6 frequencies per decade are used, setting these frequencies
!  precisely allows the most accurate use of the filters by COSTRN.

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL = SELECTED_REAL_KIND(12,76)
 INTEGER JF
 REAL(KIND=QL), ALLOCATABLE :: QQ(:)
 REAL(KIND=QL) QFRQ,QFRQ3,QFRQ6,QFRQ12,QQT

 ALLOCATE (QQ(200))

 QQ(1) = 1.D-1
 QFRQ6 = EXP ( LOG (10.D0) /6.D0 )
 QFRQ12 = EXP ( LOG (10.D0) /12.D0 )
 QFRQ3 = EXP ( LOG (10.D0) /3.D0 )
 QFRQ = QFRQ6
 NFRQ = 7

 IF (ISTOP == -2) THEN                  ! Very tedious DEFAULT choice
   QFRQ = QFRQ12
   MAXFRQ = 1.E6
   MXFRQE = MAXFRQ
   DO JF = 2,7
     QQ(JF) = QQ(JF-1) * QFRQ12
   END DO
 ELSE IF (ISTOP == -1) THEN             ! Somewhat tedious DEFAULT choice
   MAXFRQ = 1.E6
   MXFRQE = MAXFRQ
   DO JF = 2,7
     QQ(JF) = QQ(JF-1) * QFRQ6
   END DO

 ELSE
   IF (MINFRQ < 0) THEN                  ! Default range
     MXFRQE = 3162.
     IF (MAXVAL (RX_TYPE) < 3) THEN      ! No E field measurements
       QQ(1) = 1.0D0
       DO JF = 2,4
         QQ(JF) = QQ(JF-1) * QFRQ3       ! 3 points per decade to 10 Hz
       END DO
       DO JF = 5,7
         QQ(JF) = QQ(JF-1) * QFRQ6       ! 6 points per decade after 10 Hz
       END DO
     ELSE                                ! E field starts at 0.1 Hz
       DO JF = 2,7
         QQ(JF) = QQ(JF-1) * QFRQ3       ! 3 points per decade to 10 Hz
       END DO
     END IF
     MAXFRQ = 1.E5
     IF (MINVAL (RX_TYPE) == 3) MAXFRQ = MXFRQE   !  Maximum value for E fields only
   ELSE                                !  USER defined range
     QQ(1) = 10.D0                     !  Find starting point
     DO JF = 1, 36
       IF (QQ(1) < 0.95*MINFRQ) EXIT
       QQ(1) = QQ(1) / QFRQ12
     END DO
     DO JF = 2,7
       QQ(JF) = QQ(JF-1) * QFRQ6       ! 6 points per decade
     END DO
   END IF

   IF (MINVAL (RX_TYPE) == 3) MAXFRQ = MXFRQE   !  Maximum value for E fields only
 END IF

 DO JF = 8,100
   QQT = QQ(JF-1) * QFRQ
   IF (QQT > 1.1 * MAXFRQ) EXIT
   NFRQ = NFRQ + 1
   QQ(JF) = QQT
 END DO

 ALLOCATE (FREQ(NFRQ) )

 DO JF = 1,NFRQ
   FREQ(JF) = REAL (QQ(JF))
   IF (FREQ(JF) < 1.1 * MXFRQE) KFRQE = JF
 END DO
 DEALLOCATE (QQ)

 IF (ISTOP == -2) THEN                  ! Very tedious DEFAULT choice
   WRITE(NW,1)
 ELSE IF (ISTOP == -1) THEN                  ! Very tedious DEFAULT choice
   WRITE(NW,2)
 ELSE IF (MINFRQ < 0) THEN                  ! Default range
   WRITE(NW,3) FREQ(1),FREQ(NFRQ)
 ELSE
   WRITE(NW,4) FREQ(1),FREQ(NFRQ)
 END IF

 IF (NEW) THEN
   WRITE(NW,5)
 ELSE
   WRITE(NW,6)
 END IF

 Write (nw, 7) nfrq
 Do jf = 1, nfrq
     Write (nw, 8) jf, freq(jf)
 End Do

 1 FORMAT(/T3,'The time-domain results will be constructed using frequency-domain' &
          /T3,'output computed at 12 points per decade, from 0.1 Hz to 1 MHz')
 2 FORMAT(/T3,'The time-domain results will be constructed using frequency-domain' &
          /T3,'output computed at 6 points per decade, from 0.1 Hz to 1 MHz')
 3 FORMAT(/T3,'The time-domain results will be constructed using frequency-domain' &
          /T3,'output computed at 3 points per decade, from',F5.1,' Hz to 10 Hz' &
          /T3,'and at 6 points per decade, from 10 Hz to',G12.4,' Hz.')
 4 FORMAT(/T3,'The time-domain results will be constructed from frequency-domain output' &
          /T3,'computed at 6 points per decade, from',F5.1,' Hz to',G12.4,' Hz.')
 5 FORMAT(/T3,'Arjuna will start by computing new frequency-domain responses in the specified ' &
          /T3,'range to construct the spectrum from DC to the appropriate upper limit.')
 6 FORMAT(/T3,'Arjuna will compute time-domain responses from the frequency-domain data' &
          /T3,'contained in file ARJUNA.FRQ' &
         //T3,'These should be at the frequecies listed below.'/)
 7 Format (2x, 'Number of frequencies: ', i2)
 8 Format (2x, i2, 2x, f12.4, ' Hz')

END SUBROUTINE SET_FRQ

Subroutine Set_Frq_from_File
!
   Integer, Parameter :: NCOMP = 3
   Integer :: jf, its
   Integer :: ioer

   Real :: stn1, stn2, stn3, stn4, cfrq, ifrq
   Real :: bfd(2 * NCOMP)

   Character (Len = 05) :: test

   Rewind (nd)

!
!   We need to read the number of unique frequencies from the file so that we can allocate the array.
   ioer = 0; cfrq = 0.; nfrq = 0; test = 'zzzzz'
   Do While (test .ne. 'QQQQQ')
       Read (nd, 13) test
   End Do
   Do While (ioer .ne. -1)
        Read (nd, *, IOSTAT = ioer) bfd, stn1, stn2, stn3, stn4, ifrq
        If (ifrq .ne. cfrq) Then
            nfrq = nfrq + 1
            cfrq = ifrq
        End If
   End Do
   Rewind (nd)
!
!  Now we can read the correct frequencies
   Allocate (freq(nfrq))
   ioer = 0; cfrq = 0.; nfrq = 0; test = 'zzzzz'
   Do While (test .ne. 'QQQQQ')
       Read (nd, 13) test
   End Do
   Do While (ioer .ne. -1)
          Read (nd, *, IOSTAT = ioer) bfd, stn1, stn2, stn3, stn4, ifrq
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
           /, 2x, 'Arjuna will read frequency-domain computations from Arjuna.frq.' /, &
           /, 2x, 'Time-domain calulations use ', i3, ' frequencies.', /, &
           /, 2x, '   #', 2x, '      Frequency', &
           /, 2x, '----', 2x, '---------------')
11 Format (2x, i4, 2x, en15.6)
12 Format (2x, 64('-'), //)
13 Format (a5)

   End Subroutine Set_Frq_from_File


   SUBROUTINE SET_SURVEY
!  ---------------------

!  Arrays are given in real world coordinates which require double precision.
!  Rather than carry unnecessary higher precision into the computation, arrays
!  are adjusted by ECNTRD and NCNTRD

   IMPLICIT NONE
   IF (STEP == 1) RXMNT = 1.
   DO JG = 1,NRXG
     IF (STEP == 0 .AND. UNITS(JG) < 3) RXMNT(1:MRX,JG) = 1
   END DO

   SXE =  REAL (SXED - ECNTRD,4)
   SXN =  REAL (SXND - NCNTRD,4)
   SXZ = -REAL (SXZD,4)

   IF (SURVEY_TYPE /= 4) THEN
     RXE =  REAL (RXED - ECNTRD,4)
     RXN =  REAL (RXND - NCNTRD,4)
     RXZ = -REAL (RXZD,4)

   ELSE               ! Set up coincident loop midpoints in REAL KIND 8
     DO JS = 1,NTXE
       JV = N_VRTX(JS)
       CLCD(1,JS) = SUM (SXND(1:JV,JS)) / REAL (JV,8)
       CLCD(2,JS) = SUM (SXED(1:JV,JS)) / REAL (JV,8)
       CLCD(3,JS) = SUM (SXZD(1:JV,JS)) / REAL (JV,8)
     END DO
   END IF


   END SUBROUTINE SET_SURVEY

   SUBROUTINE SET_TRP
!  ------------------

!  Sets up interpolation times for FD -> TD transform which use the
!  exact 6 points per decade frequency-domain data plus 6 per decade
!  interpolated values.  These are based on a 12 point per decade
!  cosine filter derived from the Niels Christensen routine FILCOA
!  with OMEGA = .3 PI and shift 0.

!***  Called by MAIN program
!***       Uses:  MODULE AG_Filter_coefficients

!             OUTPUT
!             ------

!        TRP - array of time values for FD -> TD transformations
!      NTYRP - number of values in TRP
!     EXTENT - the latest time for which time-domain output is required.
!      PULSE - time length of one signal pulse
!     NTYPLS - number of TRP values in 1 PULSE


 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,76)
 REAL, PARAMETER :: TWOPI=6.2831853
 INTEGER MXTYM,J1
 REAL T0_MIN, EXTENT
 REAL,ALLOCATABLE :: QQQ(:)
 REAL(KIND=QL) TBASE,QTYM, TQ

 MXTYM=200
 ALLOCATE (QQQ(MXTYM))
 QQQ = 0.

 T0_MIN = 1.0 / MAXFRQ

 QTYM = LOG (10.D0) /12.D0
 QTYM = EXP (QTYM)
 NPULS = 5
 EXTENT = 2.0 * NPULS * PULSE

 TBASE = 1.D0 / DBLE (TWOPI)
 DO J1 = 1,MXTYM
   IF (REAL (TBASE) < T0_MIN) EXIT
   TBASE = TBASE / QTYM
 END DO

 TQ = TBASE
 QQQ(1) = REAL (TQ,4)
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

END MODULE AG_Input_routines

  PROGRAM MAIN
!------------

!*** Calls FDREAD, ARJUNA_3D, SET_SOURCE, TDEM_3D
!          TDEM_3D, WRITE_FD, WRITE_TD, WRYT_LOG_FILE

!*** Calls from AG_Input_routines:
!          READ_READ_SYSTEMY_DATA, READ_MODEL_DATA, SET_TRP, SET_FRQ,

 USE AG_Input_routines

 IMPLICIT NONE
 COMPLEX, PARAMETER :: ZERO=(0.,0.)
 REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: BTD
 COMPLEX,DIMENSION(:,:,:,:),ALLOCATABLE :: BFD
 INTEGER KCHNL
 REAL CMP_START, CMP_Final, CMP_Delta, DXPRM(3,3), SXDP1,SXAZ1
 LOGICAL, ALLOCATABLE :: SKIP_TX(:), SKIP_RX(:,:)
 LOGICAL AMX_PPM

 CALL CPU_TIME (CMP_START)

 CALL READ_SYSTEM_DATA  ! Set up airborne system & layered earth model.
 CALL READ_MODEL_DATA
 CLOSE (NR)

! Put arrays in body centred system.  Change order of loop vertices
! to clockwise if they have been entered in counter-clockwise order.

 CALL SET_SURVEY
 IF (SOURCE_TYPE == 1) CALL LP_VERTEX_ORDER (NTXE,MXVRTX,N_VRTX,SXN,SXE,SXZ)
 CALL SHOW_AND_TELL        ! Set & Print array & model coordinates in body-centred system

 IF (SURVEY_TYPE == 1)  DEALLOCATE (TXID)

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

 ALLOCATE (SKIP_TX(NTXE), SKIP_RX(LRX,NTXE) )
 CALL ARJUNA_EXCLUDE (NLG,SOURCE_TYPE,NTXE,MXVRTX,N_VRTX,SXZ,NRXTX,LRX, &
                      RXID,SKIP_TX,SKIP_RX)
 DEALLOCATE (SKIP_TX, SKIP_RX)

!  For time-domain, set up frequencies, interpolation times

 IF (TDFD == 1) THEN   ! Time-Domain
   CALL SET_SOURCE (NSX,SWX,SWY,T0SX)  ! Compute dI/dt at the receiver
   If (DO3D .eq. 1) Then
        Call SET_FRQ
   Else
        Call Set_frq_from_file
   End If
   CALL SET_TRP
   KCHNL = 0
   IF (MINVAL (TOPN) < TRP(1)) THEN
     WRITE(*,1)
     WRITE(NLG,3) MAXFRQ
     DO JT = 1,NCHNL
       IF (TOPN(JT) < SWX(NSX)) KCHNL = KCHNL + 1
       IF (TOPN(JT) < TRP(1)) THEN
         WRITE(NW,2) JT
         WRITE(NLG,2) JT
         WRITE(*,2) JT
       END IF
     END DO
   END IF
   IF (KCHNL > 0) THEN
     WRITE(*,5) KCHNL
     WRITE(NLG,6) KCHNL
     WRITE(NW,6) KCHNL
   END IF
 END IF

 NEW_3D_MODEL: IF (DO3D == 1) THEN

!  The computation assumes unit transmitter and receiver dipoles and MKS units
!  Conversion to user specified units and inclusion of transmitter and receiver
!  dipole moments happens in the output routines.

   CALL ARJUNA_2D (NFRQ,FREQ,SURVEY_TYPE,SOURCE_TYPE,NTXE,MXVRTX,N_VRTX,SXE,SXN,  &
                   SXZ,SXDIP,SXAZ,NRXG,NRGTX,RX_TYPE,RGTXID,NRX,MRX,MQVR,RXE,RXN, &
                   RXZ,NE,NZ,ELOC,ZLOC,ND,NAIR,LITH,LYTH,NLITH,NPROP,NLG,NEZ,NPN, &
                   LNODS,COORD)
   CLOSE (NLG)

!  End of frequency stepping.
!  Write out the total frequency-domain scattered magnetic fields for each to UNIT ND.
!
! For loop or grounded wire receivers, component JC = 1 contains the voltage and
! the other components (JC = 2,3) are zero.

 END IF NEW_3D_MODEL

 ALLOCATE (BFD(NFRQ,LRX,NTXE,3))
 BFD = ZERO

 CALL FDREAD (ND,NFRQ,NTXE,LRX,NRXTX,NCMP,BFD)  !  Read old frequency-domain data
 CLOSE (ND)

 AMX_PPM = .FALSE.
 IF (TDFD == 1) THEN   ! Time-Domain.  Compute BTD
   ALLOCATE ( BTD(NCHNL,LRX,NTXE,3))
   BTD = 0.

   CALL TDEM_3D (STEP,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL,TOPN, &
                 TCLS,FREQ,NFRQ,KFRQE,NTXE,LRX,NRXTX,RXID,NCMP,BFD,BTD)

   DEALLOCATE (BFD)

!  Write out the results.
!  Rotate to borehole system if applicable. (Default rotation is null)

   CALL BH_ROTATE (NCHNL,LRX,NTXE,NRXTX,BHR,BHAZ,BHDIP,BTD)

   CALL WRITE_TD (NW,PRFL,STEP,NCHNL,TMS,SURVEY_TYPE,SOURCE_TYPE,NTXE,SXMNT,NRXG, &
                  NRGTX,RGTXID,NRX,LRX,MRX,UNITS,RX_TYPE,NCMPG,PRTCMP,RXMNT,RXED, &
                  RXND,RXZD,MQVR,CLCD,TITLE,BTD)

   CALL WRITE_TAMX (TITLE,STEP,OUTPUT,SOURCE_TYPE,SURVEY_TYPE,NCHNL,TMS,NTXE, &
                    MXVRTX,N_VRTX,SXE,SXN,SXZ,SXDIP,SXAZ,SXMNT,NRGTX,NRXG,RGTXID,  &
                    NCMPG,PRTCMP,LRX,MRX,NRX,MQVR,RX_TYPE,UNITS,RXED,RXND,RXZD,    &
                    RXDIP,RXAZ,RXMNT,CLCD,BTD)

 ELSE  !  Write out the frequency-domain response.
!         Rotate to borehole system if applicable. (Default rotation is null)

   CALL BHC_ROTATE (NFRQ,LRX,NTXE,NRXTX,BHR,BHAZ,BHDIP,BFD)

! Multiply FD response per amp by transmitter current

   CALL FD_CURNT (NFRQ,LRX,NTXE,CURNT,BFD)

!  PPM normalisation for single offset magnetic dipole survey

   IF (SOURCE_TYPE == 3 .AND. SURVEY_TYPE == 2 .AND. CMP(1) > 0) THEN
     IF (OUTPUT > 0) AMX_PPM = .TRUE.
     CMPDX = 3; SXDP1 = SXDIP(1); SXAZ1 = SXAZ(1)
     IF (CMP(1) == 1) CMPDX = 1
     CALL MD_PRM (NW,SXDP1,SXAZ1,RXON,RXOE,RXOZ,CMPDX,DXPRM)
     CALL WRITE_FD_PPM (NW,NFRQ,FREQ,NTXE,CMPDX,RXED,RXND,RXZD,TITLE,BFD,CURNT,DXPRM)
   END IF

   CALL WRITE_FD (NW,PRFL,STEP,NFRQ,FREQ,SURVEY_TYPE,SOURCE_TYPE,NTXE,SXMNT,NRXG, &
                  NRGTX,RGTXID,LRX,MRX,NRX,UNITS,RX_TYPE,NCMPG,PRTCMP,RXMNT,RXED, &
                  RXND,RXZD,MQVR,TITLE,BFD)

   IF (AMX_PPM) THEN
       OUTPUT = ABS(OUTPUT)
       CALL WRITE_FAMX_PPM (TITLE, STEP, OUTPUT, SOURCE_TYPE, SURVEY_TYPE, NFRQ, FREQ, &
               NTXE, MXVRTX, SXE, SXN, SXZ, SXDIP, SXAZ, SXMNT, NRXG, NCMPG, PRTCMP,   &
               LRX, MRX, MQVR, RXED, RXND, RXZD, RXDIP, RXAZ, RXMNT, BFD,              &
               DXPRM, CURNT, CMPDX)
   ELSE
       OUTPUT = ABS(OUTPUT)
       CALL WRITE_FAMX (TITLE,STEP,OUTPUT,SOURCE_TYPE,SURVEY_TYPE,NFRQ,FREQ,NTXE, &
                        MXVRTX,N_VRTX,SXE,SXN,SXZ,SXDIP,SXAZ,SXMNT,NRGTX,NRXG,RGTXID,  &
                        NCMPG,PRTCMP,LRX,MRX,NRX,MQVR,RX_TYPE,UNITS,RXED,RXND,RXZD,    &
                        RXDIP,RXAZ,RXMNT,BFD)
   END IF

 END IF

 !
 ! complete run time calcs & sign off ...
 call date_and_time(Values = tvals)
 call CPU_time(CMP_final)
 CMP_delta = CMP_final - CMP_start

 Select Case (Invert)
 Case (.True.)
    ! Write (np, 11) trim(PNAME), 'inversion', tvals(5:7), tvals(3:1:-1), CMP_delta
    Write (nw, 12) trim(PNAME), 'inversion', tvals(5:7), tvals(3:1:-1), CMP_delta
    Write ( *, 12) trim(PNAME), 'inversion', tvals(5:7), tvals(3:1:-1), CMP_delta
 Case (.False.)
    ! Write (np, 11) trim(PNAME), 'forward model', tvals(5:7), tvals(3:1:-1), CMP_delta
    Write (nw, 12) trim(PNAME), 'forward model', tvals(5:7), tvals(3:1:-1), CMP_delta
    Write ( *, 12) trim(PNAME), 'forward model', tvals(5:7), tvals(3:1:-1), CMP_delta
 End Select

 STOP

!
! Formats
1 FORMAT(/T3,'No computations will be done for the following channels.' &
          /T3,'See Arjuna.log for explanation.')
 2 FORMAT(/T3,'Channel',I3,' response will not be computed.')
 3 FORMAT(/T3,'The discretisation for the Arjuna mesh precludes the use of frequencies' &
          /T3,'higher than',G12.4,' Hz.'                                               &
          /T3,'This places a limit on how early a delay time can be computed.')
 5 FORMAT(//T15,'WARNING!'//T3,'THERE ARE',I3,' ON-TIME CHANNELS!  Read Arjuna.log for this run!')
 6 FORMAT(//T3,'*****************************************************************************' &
           /T3,'*                                                                           *' &
           /T3,'*  WARNING:  There are',I3,' on-time channels for this Arjuna run.',T79, '*' &
           /T3,'*                                                                           *' &
           /T3,'*  On-time channels that occur during rapid changes in transmitter signal   *' &
           /T3,'*  will be lower than the correct response, often by an order of magnitude  *' &
           /T3,'*  or more.  Reasonable discretisation for Arjuna  precludes the use of     *' &
           /T3,'*  frequencies greater than 100 kHz, that are necessary for the very early  *' &
           /T3,'*  time responses required for convolution when dI/dt is non-zero.          *' &

           /T3,'*                                                                           *' &
           /T3,'*  On-time channels occuring when dI/dt is very small; eg, during the flat  *' &
           /T3,'*  period between ramp on and ramp off will be accurate within the usual    *' &
           /T3,'*  accuracy limits of off-time channels.                                    *' &
           /T3,'*                                                                           *' &
           /T3,'*  When modelling Geotem-Questem type systems, say for example, a 25 Hz     *' &
           /T3,'*  system where the response is essentially zero after 5 ms, it is better   *' &
           /T3,'*  to discretise the first 5 ms of the signal and then explicitly set the   *' &
           /T3,'*  off-time to 15 ms rather than include the small oscillations.            *' &
           /T3,'*                                                                           *' &
           /T3,'*****************************************************************************'/)
10 Format (/, 2x, 'Frequency-domain calculations finished ...', &
           /, 2x, 'Starting convolution for time-domain calculations ...')
11  Format ('/', / &
            '/ ', a, ' ', a, ' run completed: ', i4.4, '-', i2.2, '-', i2.2, 'T', i2.2, ':', i2.2, ':', i2.2, / &
            '/ Runtime: ', f12.2, ' secs')
12  Format (/, 2x, a, ' ', a, ' run completed: ', i4.4, '-', i2.2, '-', i2.2, 'T', i2.2, ':', i2.2, ':', i2.2, &
		    /, 2x, 'Runtime: ', f12.2, ' seconds', /)
90 Format (/, 2x, 'Completed sanity check on wetries in ', a, '.cfl ...', &
           /, 2x, 'Computation begining ...')
91 Format (/, 2x, 'WARNING', &
           /, 2x, a, '.cfl may contain errors. Please check ', a, '.log and ', a, '.out')
92 Format (/, 2x, 'FATAL ERROR', &
           /, 2x, a, '.cfl contains errors. Please correct these before restarting.')


 END PROGRAM MAIN

 SUBROUTINE BH_ROTATE (NCHNL,LRX,NTXE,NRXTX,BHR,DXAZ,DXDIP,BTD)
!-------------------------------------------------------------

!***  Called by MAIN
!***  Calls NIL

! Rotates REAL (time-domain) North, East, Vertical response
! components into Axial, Slope & Horizontal components.

!     The Axial component points in the direction of the borehole axis, defined by DXDIP.
!          If the hole is vertical, it points down.
!          If the hole is horizontal, it points towards DXAZ.
!
!     The Slope component lies perpendicular to the borehole in the vertical plane
!     containing the borehole.
!          If the hole is vertical, it is the North component.   (12 o'clock)
!          If the hole is horizontal, it is the vertical component.
!
!     The Horizontal component lies perpendicular to the azimuth.
!          If the hole is vertical, it points West.  (9 o'clock)
!          If the hole is horizontal, it points towards DXAZ - PI / 2.


!               INPUT
!               -----
!      NCHNL - number of time domain receibver channels
!        LRX - number of receivers for each transmitter position
!       NTXE - number of transmitter positions
!   NRXTX(J) - number of receivers belonging to transmitter J
!        BHR - if false, don't rotate
!       DXAZ - assigned borehole azimuth in radians(= 0 due north)
!      DXDIP - assigned borehole dip in radians (vertical down = 0)
!        BTD - time domain step or impulse response expressed in
!              North, East, and Vertical components
!
!               OUTPUT
!               ------
!      BTD(JT,JR,JS,K) - time domain step or impulse response at time JT,
!                        at receiver, JR, for transmitter, JS, expressed
!                        in borehole components: K = 1, 2, 3

!         K = 1 => Horizontal;  K = 2 => Slope;  K = 3 => Axial
!

 REAL, PARAMETER :: TOL = 0.001
 INTEGER NCHNL, LRX, NTXE, NRXTX(NTXE),JR,JS,JT
 REAL DXAZ(LRX,NTXE), DXDIP(LRX,NTXE), BTD(NCHNL,LRX,NTXE,3), &
      SL,HR,AX,BX,BY,BZ,SPHI,CPHI,STH,CTH
 LOGICAL BHR(LRX,NTXE)

 DO JS = 1,NTXE
   DO JR = 1,NRXTX(JS)
     IF (.NOT. BHR(JR,JS)) CYCLE     !  Do not rotate unless BHR is true.
     CTH = COS (DXDIP(JR,JS))
     STH = SIN (DXDIP(JR,JS))
     CPHI = COS (DXAZ(JR,JS))
     SPHI = SIN (DXAZ(JR,JS))

     DO JT = 1,NCHNL
       BX = BTD(JT,JR,JS,1)
       BY = BTD(JT,JR,JS,2)
       BZ = BTD(JT,JR,JS,3)

       IF (ABS (DXDIP(JR,JS)) < TOL) THEN
         AX = BZ
         SL = BX
         HR = -BY
       ELSE
         AX = CTH * BZ + STH * (CPHI*BX + SPHI*BY)
         SL = STH * BZ - CTH * (CPHI*BX + SPHI*BY)
         HR =        SPHI*BX - CPHI*BY
       END IF
       BTD(JT,JR,JS,1) = HR
       BTD(JT,JR,JS,2) = SL
       BTD(JT,JR,JS,3) = AX
     END DO

   END DO
 END DO
END SUBROUTINE BH_ROTATE

 SUBROUTINE BHC_ROTATE (NFRQ,LRX,NTXE,NRXTX,BHR,DXAZ,DXDIP,BFD)
!--------------------------------------------------------------

!***  Called by MAIN
!***  Calls NIL

! Rotates COMPLEX (frequency-domain) North, East, Vertical response
! components into Axial, Slope & Horizontal components.

!     The Axial component points in the direction of the borehole axis, defined by DXDIP.
!          If the hole is vertical, it points down.
!          If the hole is horizontal, it points towards DXAZ.
!
!     The Slope component lies perpendicular to the borehole in the vertical plane
!     containing the borehole.
!          If the hole is vertical, it points North.   (12 o'clock)
!          If the hole is horizontal, it is the vertical component.
!
!     The Horizontal component lies perpendicular to the azimuth.
!          If the hole is vertical, it points West.  (9 o'clock)
!          If the hole is horizontal, it points towards DXAZ - PI / 2.


!               INPUT
!               -----
!       NFRQ - number of time domain receibver channels
!        LRX - number of receivers for each transmitter position
!       NTXE - number of transmitter positions
!   NRXTX(J) - number of receivers belonging to transmitter J
!        BHR - if false, don't rotate
!       DXAZ - assigned borehole azimuth in radians(= 0 due north)
!      DXDIP - assigned borehole dip in radians(vertical = 0)
!        BFD - frequency domain step or impulse response expressed in
!              North, East, and Vertical components
!
!               OUTPUT
!               ------
!      BFD(JF,JR,JS,K) - frequency domain response at frequency JF, at
!                        receiver, JR, for transmitter, JS, expressed
!                        in borehole components: K = 1, 2, 3

!         K = 1 => Horizontal;  K = 2 => Slope;  K = 3 => Axial
!

 REAL, PARAMETER :: TOL = 0.001
 INTEGER NFRQ, LRX, NTXE, NRXTX(NTXE),JR,JS,JF
 REAL DXAZ(LRX,NTXE), DXDIP(LRX,NTXE),SPHI,CPHI,STH,CTH
 COMPLEX BFD(NFRQ,LRX,NTXE,3), SL,HR,AX,BX,BY,BZ
 LOGICAL BHR(LRX,NTXE)

 DO JS = 1,NTXE
   DO JR = 1,NRXTX(JS)
     IF (.NOT. BHR(JR,JS)) CYCLE     !  Do not rotate unless BHR is true.

     CTH = COS (DXDIP(JR,JS))
     STH = SIN (DXDIP(JR,JS))
     CPHI = COS (DXAZ(JR,JS))
     SPHI = SIN (DXAZ(JR,JS))

     DO JF = 1,NFRQ
       BX = BFD(JF,JR,JS,1)
       BY = BFD(JF,JR,JS,2)
       BZ = BFD(JF,JR,JS,3)

       IF (ABS (DXDIP(JR,JS)) < TOL) THEN
         AX = BZ
         SL = BX
         HR = -BY
       ELSE
         AX = CTH * BZ + STH * (CPHI*BX + SPHI*BY)
         SL = STH * BZ - CTH * (CPHI*BX + SPHI*BY)
         HR =        SPHI*BX - CPHI*BY
       END IF

       BFD(JF,JR,JS,1) = HR
       BFD(JF,JR,JS,2) = SL
       BFD(JF,JR,JS,3) = AX
     END DO

   END DO
 END DO
END SUBROUTINE BHC_ROTATE

 REAL FUNCTION COSTRN (WF,YFRQ,NFRQ,KFRQ,T)
!------------------------------------------

!***  Calls CUBVAL
!***  Called by TDEM_3D

! LAST MODIFICATION DATE: June, 2001

! Produces time-domain value at time T by cosine transformation of NFRQ
! frequency-domain values contained in cubic spline array YFRQ.
! KFRQ is the high frequency cutoff, less than or equal to NFRQ.
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

 USE AG_Filter_coefficients

 IMPLICIT NONE
 INTEGER, PARAMETER :: NDEC_COS=12, KFLOW=-200, KFHIGH=99
 REAL, PARAMETER :: FAC=.7978846, TOL=1.0E-4
 INTEGER J1,NFRQ,KFRQ
 REAL WF(NFRQ),YFRQ(4,NFRQ),T,YTYM,YS,VAL,CUBVAL
 DOUBLE PRECISION DELTA,Y1,Y,TD

 INTENT (IN) WF,YFRQ,NFRQ,T


 DELTA = LOG (10.D0)/ DBLE (NDEC_COS)
 TD = DBLE(T)
 YTYM = 0.
 Y1 = -LOG (TD) - DBLE (DELCOS)

! Begin right side convolution at weight 0.
! Stop when frequency domain array is exhausted.

 MOVE_HIGH: DO J1 = 0, KFHIGH

   Y = Y1 + J1 * DELTA
   YS = REAL(Y)
   IF (YS > WF(KFRQ)) EXIT MOVE_HIGH
   IF (YS < WF(1)) YS = WF(1)
   VAL = WCOS(J1) * CUBVAL (WF,YFRQ,NFRQ,YS)
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
   IF (YS > WF(KFRQ)) CYCLE
   IF (YS < WF(1)) YS = WF(1)
   VAL = WCOS(J1) * CUBVAL (WF,YFRQ,NFRQ,YS)
   YTYM = YTYM + VAL
   IF ((Y < WF(3))) THEN
     IF (ABS (VAL) < TOL * ABS (YTYM)) EXIT MOVE_LOW
   END IF
 END DO MOVE_LOW

 COSTRN = FAC * YTYM / T

END FUNCTION COSTRN

  REAL FUNCTION CUBINT (XKNOT, COEF, KNOT, X1, X2)
! ------------------------------------------------

!  Integrates a function from X1 to X2 using its cubic spline representation.

!***  Called by  TXCNVD, TXCNVL
!***  Calls INTERV.  On exit from INTERV
!
!       MFLAG = -1  => X is to the left of interpolated range
!             =  1  => X is to the right of interpolated range
!             =  0  => X is in the interpolated range

!      KNOT - total number of knots including endpoints.
!
!     XKNOT(I), I = 1,KNOT - Location of the knots.  The rightmost data
!                            point used to calculate coefficients is not
!                            included.
!
!     COEF(J,I), J = 1,4; I = 1,KNOT
!
!              The coefficients of the cubic spline represent the
!              indefinite integral of F, on the I'th interval, as:
!
!       INTGR [ F(X) ] = COEF(4,I)/24 * H**4  +  COEF(3,I)/6 * H**3  +
!                        COEF(2,I)/2 * H**2  +  COEF(1,I) * H
!
!                          WITH  H = X - XKNOT(K)
!
!  This is a modification of the FUNCTION PPVALU in the book
!  "A PRACTICAL GUIDE TO SPLINES"  by C. DE BOOR

!*********************************************************************

  IMPLICIT NONE
  INTEGER I,I1,I2,MFLAG,KNOT
  REAL H,H1,H2,X1,X2,XKNOT(KNOT), COEF(4,KNOT)

!  Find the indices I1 and I2 of largest breakpoints to the left of X1
!  and X2 respectively.
!
  CALL INTERV ( XKNOT, KNOT-1, X1, I1, MFLAG )
  CALL INTERV ( XKNOT, KNOT-1, X2, I2, MFLAG )
  H1 = X1 - XKNOT(I1)
  IF (MFLAG == -1) H1 = 0.

  H2 = X2 - XKNOT(I2)
  CUBINT = (((COEF(4,I2)*H2/4.0 + COEF(3,I2) )*H2/3.0 + &
              COEF(2,I2) )*H2/2.0 + COEF(1,I2) )*H2 &
         - (((COEF(4,I1)*H1/4.0 + COEF(3,I1) )*H1/3.0 + &
              COEF(2,I1) )*H1/2.0 + COEF(1,I1) )*H1

!  Include integrals over intervening intervals.

  IF (I2 > I1) THEN
    DO I = I1, I2-1
      H = XKNOT(I+1) - XKNOT(I)
      CUBINT = CUBINT + (((COEF(4,I)*H/4.0 + COEF(3,I) )*H/3.0 + &
                           COEF(2,I) )*H/2.0 + COEF(1,I) )*H
    END DO
  END IF

END FUNCTION CUBINT

  SUBROUTINE CUBSPL (XNOT, C, N, IBCBEG, IBCEND)
! ----------------------------------------------

!***  Called by FOLD_AND_CONVOLVE, READ_SYSTEM_AND_LAYER_DATA, TXCNVD

!  Calculates coefficients for cubic spline interpolation.
!  Call function CUBVAL to evaluate function values after interpolation.
!  From  * A PRACTICAL GUIDE TO SPLINES *  by Carl de Boor.

!             INPUT
!             -----
!
!     N = number of data points. assumed to be > 1.
!
!  (XNOT(I), C(1,I), I=1,...,N) = abscissae and ordinates of the data points.
!                                 XNOT is assumed to be strictly increasing.
!
!     IBCBEG, IBCEND = boundary condition indicators, and
!     C(2,1), C(2,N) = boundary condition information. Specifically,
!
!     IBCBEG = 0  No boundary condition at XNOT(1) is given.  In this case,
!                 the not-a-knot condition is used, i.e. the jump in the
!                 third derivative across XNOT(2) is forced to zero.  Thus
!                 first and the second cubic polynomial pieces are made to
!                 coincide.
!     IBCBEG = 1  the slope at XNOT(1) is made to equal C(2,1),
!                 supplied by input.
!     IBCBEG = 2  the second derivative at XNOT(1) is made to equal C(2,1),
!                 supplied by input.
!
!     IBCEND = 0, 1, or 2 has analogous meaning concerning the boundary
!                 condition at XNOT(n), with the additional information
!                 taken from C(2,n).
!
!          OUTPUT
!          ------
!
!     C(J,I), J=1,...,4; I=1,...,L (= N-1) = the polynomial coefficients
!         of the cubic interpolating spline with interior knots (or joints)
!         XNOT(2), ..., XNOT(N-1).
!
!        In the interval: (XNOT(I) - XNOT(I+1)), the spline F is given by:
!
!        F(X) = C(1,I) + H* (C(2,I) + H* (C(3,I) + H* C(4,I)/3.) /2.)
!
!     where H = X - XNOT(I).  FUNCTION  *CUBVAL* may be
!     used to evaluate F or its derivatives from XNOT,C, L = N-1,
!     AND K=4.
!******************************************************************************
!******************************************************************************
  IMPLICIT NONE
  INTEGER IBCBEG,IBCEND,N,I,J,L,M
  REAL C(4,N),XNOT(N),DIVDF1,DIVDF3,DXNOT,G

  INTENT (IN) XNOT, N, IBCBEG, IBCEND
  INTENT (INOUT) C
  SAVE

!  A tridiagonal linear system for the unknown slopes S(I) of F at
!  XNOT(I), I=1,...,N, is generated and then solved by Gauss elimination,
!  with S(I) ending up in C(2,I), ALL I.
!  C(3,.) AND C(4,.) are used initially for temporary storage.

!  Compute first differences of XNOT sequence and store in C(3,.).
!  Also, compute first divided difference of data and store in C(4,.).

  L = N - 1
  DO M = 2,N
    C(3,M) = XNOT(M) - XNOT(M-1)
    C(4,M) = (C(1,M) - C(1,M-1)) /C(3,M)
  END DO

!  Construct first equation from the boundary condition, of the form
!  C(4,1)*S(1) + C(3,1)*S(2) = C(2,1)

      IF (IBCBEG < 1) THEN
        IF (N > 2) THEN

!  Not-a-knot condition at left end and N > 2.

          C(4,1) = C(3,3)
          C(3,1) = C(3,2) + C(3,3)
          C(2,1) = ((C(3,2) + 2.* C(3,1)) * C(4,2)*C(3,3) &
                  + C(3,2)**2 * C(4,3)) /C(3,1)
          GOTO 100
        ELSE

!  No condition at left end and N = 2.

          C(4,1) = 1.
          C(3,1) = 1.
          C(2,1) = 2. * C(4,2)
          GOTO 300
        END IF
      ELSE IF (IBCBEG == 1) THEN

!  Slope prescribed at left end.

        C(4,1) = 1.
        C(3,1) = 0.
      ELSE

!  Second derivative prescribed at left end.

        C(4,1) = 2.
        C(3,1) = 1.
        C(2,1) = 3.* C(4,2) - C(3,2) * C(2,1) /2.
      END IF
      IF (N == 2) GOTO 300

!  if there are interior knots, generate the corresponding equations and
!  perform the forward pass of Gauss elimination, after which the M-TH
!  equation reads    C(4,M)*S(M) + C(3,M)*S(M+1) = C(2,M).

  100 DO M = 2,L
        G = -C(3,M+1) / C(4,M-1)
        C(2,M) = G*C(2,M-1) &
                + 3.* (C(3,M)*C(4,M+1) + C(3,M+1)*C(4,M))
        C(4,M) = G* C(3,M-1) + 2.* (C(3,M) + C(3,M+1))
      END DO

!  Construct last equation from the second boundary condition, of the form
!  (-G*C(4,N-1))*S(N-1) + C(4,N)*S(N) = C(2,N)
!  If slope is prescribed at right end, one can go directly to back-
!  substitution, since C array happens to be set up just right for it
!  at this point.

      IF (IBCEND < 1) THEN
        IF ( N /=3 .OR. IBCBEG /=0 ) THEN

!  Not-a-knot and N > 2, and either N > 3 or also not-a-knot at
!  left end point.

          G = C(3,N-1) + C(3,N)
          C(2,N) = ((C(3,N) + 2.*G) *C(4,N)*C(3,N-1) + C(3,N)**2 &
                  *(C(1,N-1) - C(1,N-2)) /C(3,N-1))/G
          G = -G / C(4,N-1)
          C(4,N) = C(3,N-1)
          GOTO 350
        END IF
      ELSE IF (IBCEND == 1) THEN
        GOTO 400
      ELSE
        GOTO 250
      END IF

!  Either (N=3 and not-a-knot also at left) or (N=2 and not not-a-
!  knot at left end point).

  200 C(2,N) = 2. * C(4,N)
      C(4,N) = 1.
      G = -1. / C(4,N-1)
      GOTO 350

!  Second derivative prescribed at right endpoint.

  250 C(2,N) = 3.*C(4,N) + C(3,N)*C(2,N)/2.
      C(4,N) = 2.
      G = -1. / C(4,N-1)
      GOTO 350
  300 IF (IBCEND < 1) THEN
        IF (IBCBEG > 0) GOTO 200

!  Not-a-knot at right endpoint and at left endpoint and N = 2.

        C(2,N) = C(4,N)
        GOTO 400
      ELSE IF (IBCEND == 1) THEN
        GOTO 400
      ELSE
        GOTO 250
      END IF

!  Complete forward pass of Gauss elimination.

  350 C(4,N) = G*C(3,N-1) + C(4,N)
      C(2,N) = (G*C(2,N-1) + C(2,N)) /C(4,N)

!  Perform back substitution.

  400 J = L
  450 C(2,J) = (C(2,J) - C(3,J) *C(2,J+1)) /C(4,J)
      J = J - 1
      IF (J > 0) GOTO 450

!  Generate cubic coefficients in each interval, i.e., the derivatives at its
!  left endpoint, from value and slope at its endpoints.

  DO I = 2,N
    DXNOT = C(3,I)
    DIVDF1 = (C(1,I) - C(1,I-1)) /DXNOT
    DIVDF3 = C(2,I - 1) + C(2,I) - 2.*DIVDF1
    C(3,I-1) = 2.* (DIVDF1 - C(2,I-1) - DIVDF3) /DXNOT
    C(4,I-1) = (DIVDF3/DXNOT) * (6./DXNOT)
  END DO
END SUBROUTINE CUBSPL

  REAL FUNCTION CUBVAL (XKNOT, COEF, KNOT, X1)
! --------------------------------------------

!  Evaluates a function at X1 from from its cubic spline representation.

!***  Called by COSTRN, FOLD_AND_CONVOLVE, TXCNVD, TXCNVL
!***  Calls INTERV.  On exit from INTERV
!
!       MFLAG = -1  => X is to the left of interpolated range
!             =  1  => X is to the right of interpolated range
!             =  0  => X is in the interpolated range

!      KNOT - total number of knots including endpoints.
!
!     XKNOT(I), I = 1,KNOT - location of the knots.  The rightmost data
!                            point used to calculate coefficients is not
!                            included.
!
!     COEF(J,I), J = 1,4; I = 1,KNOT
!
! The coefficients of the cubic spline on the I'th interval represent F as:
!
!                F(X) = COEF(4,I)/6 * H**3  +  COEF(3,I)/2 * H**2  +
!                       COEF(2,I) * H  +  COEF(1,I)
!
!                          with  H = X - XKNOT(I)
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
  INTEGER I,MFLAG,KNOT
  REAL XKNOT(KNOT),COEF(4,KNOT),X1,H

  INTENT (IN) XKNOT, COEF, KNOT, X1
!
!  Find index I of largest breakpoint to the left of X1.
!
  CALL INTERV ( XKNOT, KNOT-1, X1, I, MFLAG )
  H = X1 - XKNOT(I)
  IF (MFLAG == -1) H = 0.

  CUBVAL = ((COEF(4,I)*H/3.0 + COEF(3,I) )*0.5*H + COEF(2,I) )*H + COEF(1,I)

END FUNCTION CUBVAL

 SUBROUTINE FD_CURNT (NFRQ,LRX,NTXE,CURNT,BFD)
!---------------------------------------------

!*** Called by MAIN
!  Multiplies  FD response per amp by transmitter current

 IMPLICIT NONE
 INTEGER JR,JF,JC,JS,NFRQ,LRX,NTXE
 REAL CURNT(NFRQ)
 COMPLEX, DIMENSION (NFRQ,LRX,NTXE,3) :: BFD

 DO JC = 1, 3
   DO JS = 1, NTXE
     DO JR = 1, LRX
       DO JF = 1,NFRQ
         BFD(JF,JR,JS,JC) = CURNT(JF) * BFD(JF,JR,JS,JC)
       END DO
     END DO
   END DO
 END DO

 END SUBROUTINE FD_CURNT

 SUBROUTINE FDREAD (ND,NFRQ,NTXE,LRX,NRXTX,NCMP,BFD)
!---------------------------------------------------

!  Reads frequency-domain scattered impulse magnetic field data
!  (real & imaginary components) from logical UNIT ND into
!  array BFD for conversion to time-domain by SUBROUTINE TDEM_OUT.

!*** Called by MAIN

!            NFRQ - number of frequencies
!            NTXE - number of transmitter positions
!             LRX - maximum number of receivers for any transmitter
!           NRXTX - number of receivers for each transmitter position
!            NCMP - number fo components for each receiver
!   BFD1(I,J,K,L) - Lth component of the complex frequency-domain impulse
!                   response at receiver J, corresponding to transmitter K
!                   for frequency I.  Usually L = 1,2,3 => North, East, & vertical
!                   components respectively.  If NCMP = 1 as is the case for loop,
!                   electric dipole of coupled magnetic dipoles then the relevant
!                   component is stored in L=1 position

  IMPLICIT NONE
  INTEGER ND,NFRQ,NTXE,LRX,NRXTX(NTXE),NCMP(LRX,NTXE),JF,JS,JR,JC,KC,I
  REAL A(6)
  COMPLEX BFD(NFRQ,LRX,NTXE,3)
  CHARACTER(LEN=5) SKIP

  REWIND ND
  DO I = 1, 1000000
     READ (ND,'(A5)') SKIP(1:5)
     IF (SKIP == 'QQQQQ')   EXIT
  END DO
  DO JF = 1,NFRQ
    DO JS = 1, NTXE
      DO JR = 1, NRXTX(JS)
        KC = NCMP(JR,JS)
        KC = 3
        READ(ND,*) A(1:2*KC)
        DO JC = 1,3
          BFD(JF,JR,JS,JC) = CMPLX (A(2*JC-1), A(2*JC)) * 12.56637E-7
        END DO
      END DO
    END DO
  END DO
END SUBROUTINE FDREAD

 SUBROUTINE INTERV (XT, LXT, X, LEFT, MFLAG)
!-------------------------------------------

!***   Called by CUBVAL, CUBINT

!********  Restructured April, 1997

!  from  * A PRACTICAL GUIDE TO SPLINES *  by C. DE BOOR
!  computes  LEFT = MAX( I , 1 <= I <= LXT  .AND.  XT(I) <= X )  .
!
!             INPUT
!             -----
!       XT - a real sequence, of length  LXT, assumed to be non-decreasing.
!      LXT - number of terms in the sequence  XT .
!        X - the point whose location with respect to the sequence XT is
!            to be determined.
!
!             OUTPUT
!             ------
!      LEFT, MFLAG.....are both integers, whose value is:
!
!        1     -1      IF               X <  XT(1)
!        I      0      IF   XT(I)  <= X < XT(I+1)
!       LXT     1      IF  XT(LXT) <= X
!
!        In particular, MFLAG = 0 is the 'usual' case.  MFLAG /= 0
!        indicates that X  lies outside the halfopen interval
!        XT(1) <= Y < XT(LXT) . The asymmetric treatment of the
!        interval is due to the decision to make all pp functions
!        continuous from the right.
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

 SUBROUTINE MD_PRM (NW,SXDP1,SXAZ1,RXON,RXOE,RXOZ,CMPDX,DXPRM)
!-------------------------------------------------------------

!***  Called by MAIN

! Computes three components of the direct DC free space magnetic field B in the
! orthogonal borehole system defined by SXAZ & SXDIP, expressed in Teslas.
! For time-domain applications, multiply it by current and get T.
! Multiply it by dI/dt and get volts per unit area which is the same as T/s.
!
! This version considers only a single probe with fixed offset.
!
!               INPUT
!               -----
!  RXON, RXOE, RXOZ - north, east & depth offsets of dipole receiver
!             CMPDX = 3 => 3 borehole components
!             CMPDX = 1 => axial component only
!
!               OUTPUT
!               ------
!
!   DXPRM(K,1) - Kth component of the direct field in teaslas
!   DXPRM(1:3,2) = absolute value of normalising component
!
!      Components:  K = 1 => Horizontal,  K = 2 => Slope,  K = 3 => Axial
!
!      The Axial component lies along the borehole axis, defined by SXDIP.
!         If the hole is vertical, it contains the vertical component.
!         If the hole is horizontal, with azimuth = 0, it points north.
!      The Slope component lies in the vertical plane containing the borehole.
!         If the hole is vertical, it points North. (12 o'clock)
!         If the hole is horizontal, it contains the vertical component.
!      The Horizontal component is perpendicular to Slope and Axial.
!         If the hole is vertical, it points West. (9 o'clock)

!        Current default OUTPUT:
!          If the magnitude of DXPRM(K,1) > 0.1 Total field
!               DXPRM(K,2) = absolute value of DXPRM(K,1)
!          Otherwise
!               DXPRM(K,2) = Total field

 IMPLICIT NONE
 INTEGER NW,CMPDX
 REAL SXDP1,SXAZ1,RXON,RXOE,RXOZ,SNDP,CSDP,SNAZ,CSAZ,BFAC,FAC3,XRX,YRX, &
      ZRX,R,ZR,TOL,DXPRM(3,2),TOTAL

!  BFAC = MU / (4 * PI) for time domain => output is in Teslas

 BFAC = 1.E-7

 SNDP = SIN (SXDP1)
 CSDP = COS (SXDP1)
 SNAZ = SIN (SXAZ1)
 CSAZ = COS (SXAZ1)

!  Rotate the components into the transmitter dip & azimuth system

 XRX = RXON * CSAZ + RXOE * SNAZ
 YRX = RXOE * CSAZ - RXON * SNAZ
 ZRX = RXOZ * CSDP + XRX * SNDP
 XRX = XRX * CSDP - RXOZ * SNDP

 R = SQRT (ZRX**2 + XRX**2 + YRX**2)
 ZR = ZRX / R
 FAC3 = BFAC / R**3

 DXPRM(1,1) = FAC3 * (3.* ZR * YRX/R)
 DXPRM(2,1) = FAC3 * (3.* ZR * XRX/R)
 DXPRM(3,1) = FAC3 * (3.* ZR * ZR - 1.)
 TOTAL = FAC3 * SQRT (3.* ZR * ZR + 1.)

 TOL = 0.1 * TOTAL
 DXPRM(1:3,2) = ABS (DXPRM(1:3,1))

 IF (CMPDX == 3) THEN
   WRITE(NW,4) 1.E12*DXPRM(3,1),1.E12*DXPRM(2,1),1.E12*DXPRM(1,1),1.E12*TOTAL

   IF (DXPRM(1,2) < TOL) THEN
     DXPRM(1,2) = TOTAL
     WRITE(NW,1)
   END IF
   IF (ABS (DXPRM(2,2)) < TOL) THEN
     DXPRM(2,2) = TOTAL
     WRITE(NW,2)
   END IF
 ELSE
   WRITE(NW,5) 1.E12*DXPRM(3,1),1.E12*TOTAL
 END IF
 IF (ABS (DXPRM(3,2)) < TOL) THEN
   DXPRM(3,2) = TOTAL
   WRITE(NW,3)
 END IF

 1 FORMAT(T3,'The Horizontal component will be normalised by the TOTAL field.')
 2 FORMAT(T3,'The Slope component will be normalised by the TOTAL field.')
 3 FORMAT(T3,'The Axial component will be normalised by the TOTAL field.')
 4 FORMAT(//T7,'PPM OUTPUT for constant offset frequency-domain system' &
           /T7,'------------------------------------------------------' &
          //T7,'DC magnetic field (in Teslas) used for PPM normalisation' &
          //T3,'     Axial field =',G12.4,' picoTeslas.' &
           /T3,'     Slope field =',G12.4,' picoTeslas.' &
           /T3,'Horizontal field =',G12.4,' picoTeslas.' &
           /T3,'  Total DC field =',G12.4,' picoTeslas.' &
          //T3,'All fields will be normalised by the corresponding DC component unless' &
           /T3,'that DC component is less than 10 percent of the total DC field'/)
 5 FORMAT(//T7,'PPM OUTPUT for constant offset frequency-domain system' &
           /T7,'------------------------------------------------------' &
          //T7,'DC magnetic field (in Teslas) used for PPM normalisation' &
          //T3,'     Axial field =',G12.4,' picoTeslas.' &
           /T3,'  Total DC field =',G12.4,' picoTeslas.' &
          //T3,'the Axial fields will be normalised by the Axial DC component.'/)

 END SUBROUTINE MD_PRM

 SUBROUTINE SET_OUTPUT_FACTORS (NRXG,MRX,SURVEY_TYPE,RX_TYPE,RXMNT,STEP,UNITS,OUTTXT,OUTFAC)
!-------------------------------------------------------------------------------------------

!  Determines output conversion factors and text output units for each receiver group.
!  For magnetic dipole and loop receivers, a factor of MU0 is necessary to convert
!  from H to B

!*** Called by: WRITE_TD, WRITE_FD
!***     Calls: nil

 IMPLICIT NONE
 INTEGER NRXG,MRX,SURVEY_TYPE,STEP,JG
 INTEGER, DIMENSION(NRXG) :: RX_TYPE,UNITS
 REAL, DIMENSION(MRX,NRXG) :: OUTFAC,RXMNT
 CHARACTER(LEN=16) OUTTXT(NRXG)

 DO JG = 1,NRXG

   IF (SURVEY_TYPE == 4 .OR. RX_TYPE(JG) > 1) THEN  !  Loop or electrode receivers
     IF (UNITS(JG) == 1) THEN
       OUTFAC(1:MRX,JG) = 1.
       OUTTXT(JG) = 'VOLTS.          '
     ELSE IF (UNITS(JG) == 2) THEN
       OUTFAC(1:MRX,JG) = 1000.
       OUTTXT(JG) = 'MILLIVOLTS.     '
     ELSE IF (UNITS(JG) == 3) THEN
       OUTFAC(1:MRX,JG) = 1.E6
       OUTTXT(JG) = 'MICROVOLTS.     '
     ELSE IF (UNITS(JG) == 4) THEN
       OUTFAC(1:MRX,JG) = 1.E9
       OUTTXT(JG) = 'NANOVOLTS.      '
     END IF

   ELSE IF (STEP == 0 .AND. RX_TYPE(JG) == 1) THEN   ! Measure dB/dt or volts for magnetic dipole receivers

     IF (UNITS(JG) == 1) THEN
       OUTFAC(1:MRX,JG) = 1.E9
       OUTTXT(JG) = 'NANOTESLAS /SEC.'
     ELSE IF (UNITS(JG) == 2) THEN
       OUTFAC(1:MRX,JG) = 1.E12
       OUTTXT(JG) = 'PICOTESLAS /SEC.'
     ELSE IF (UNITS(JG) == 3) THEN
       OUTFAC(1:MRX,JG) = 1.E6 * RXMNT(1:MRX,JG)
       OUTTXT(JG) = 'MICROVOLTS.     '
     ELSE IF (UNITS(JG) == 4) THEN
       OUTFAC(1:MRX,JG) = 1.E9 * RXMNT(1:MRX,JG)
       OUTTXT(JG) = 'NANOVOLTS.      '
     END IF

   ELSE IF (STEP == 1 .AND. RX_TYPE(JG) == 1) THEN

     IF (UNITS(JG) == 1) THEN
       OUTFAC(1:MRX,JG) = 1.E9
       OUTTXT(JG) = 'NANOTESLAS      '
     ELSE IF (UNITS(JG) == 2) THEN
       OUTFAC(1:MRX,JG) = 1.E12
       OUTTXT(JG) = 'PICOTESLAS      '
     ELSE IF (UNITS(JG) == 3) THEN
       OUTFAC(1:MRX,JG) = 1.E15
       OUTTXT(JG) = 'FEMTOTESLAS     '
     END IF

   END IF
 END DO

 END SUBROUTINE SET_OUTPUT_FACTORS

!=================================================
!  Start of extended AMIRA format routines: Arjuna
!=================================================
!
!  SUBROUTINE WRITE_FAMX
!  SUBROUTINE WRITE_FAMX_PPM
!      SUBROUTINE WFAMX_HEADER
!  SUBROUTINE WRITE_TAMX
!      SUBROUTINE WAMX_CLDIST
!      SUBROUTINE WAMX_POS
!      SUBROUTINE WTAMX_HEADER
!  SUBROUTINE SET_OUTPUT_SCALING
!
!  Notes
!  1.  Data are ALWAYS written in PROFILE mode
!  2.  Following the convention of AEM data, quadrature phase data are output before inphase data.
!      In the *.out file, the order is reversed ie. inphase, then quadrature.
!  3.  Blocked-mode ouput (OUTPUT > 12) must be accessed by editing Arjuna.cfl; it cannot be
!      enabled from EMGui
!  4.  FEM dipole-dipole data are written in units of ppm UNLESS OVERRIDDEN (by setting OUTPUT < 0)
!  5.  Contents of Arjuna.amx are determined by the variable OUTPUT as follows
!
!      OUTPUT      Contents
!      ------      ---------
!      10, 11      Total-field only
!      12          Total, scattered & percentage target effect data
!      20, 21      Blocked-mode total-field only
!      22          Blocked-mode total, scattered & percentage target effect data
!
!      OUTPUT is irrelevant for Arjuna & Loki as as neither program computes scattered fields
!

 SUBROUTINE WRITE_FAMX(TITLE, &
                STEP,OUTPUT,SOURCE_TYPE,SURVEY_TYPE, &
                NFRQ,FREQ, &
                NTXE, MXVRTX,N_VRTX, &
                SXE,SXN,SXZ,SXDIP,SXAZ,SXMNT,&
                NRGTX,NRXG,RGTXID,NCMPG,PRTCMP,LRX,MRX,NRX,MQVR,&
                RX_TYPE,UNITS,RXED,RXND,RXZD,RXDIP,RXAZ,RXMNT,&
                BFD)
!---------------------------------------------------------------------------------

!  Calls       wfamx_header
!              set_output_scaling
!              wamx_pos
!              wamx_cldist
!              fem_perc
!  Called by:  main

   IMPLICIT NONE

   INTEGER, PARAMETER :: AMX_MAJOR = 1
   INTEGER, PARAMETER :: AMX_MINOR = 0
   INTEGER, PARAMETER :: NA = 30
   INTEGER, PARAMETER :: MAX_RCT = 3
   INTEGER, PARAMETER :: MAX_CMP = 3
   INTEGER, PARAMETER :: MAX_TYP = 1
   INTEGER, PARAMETER :: MAX_SRC = 4
   INTEGER, PARAMETER :: LBASE = 10000
   INTEGER, PARAMETER :: DP = SELECTED_REAL_KIND(12, 80)

   INTEGER, INTENT (IN) :: STEP, OUTPUT
   INTEGER, INTENT (IN) :: SOURCE_TYPE, SURVEY_TYPE
   INTEGER, INTENT (IN) :: NFRQ
   INTEGER, INTENT (IN) :: NTXE, MXVRTX
   INTEGER, INTENT (IN) :: N_VRTX(NTXE)
   INTEGER, INTENT (IN) :: NRXG, LRX, MRX, MQVR

   INTEGER, INTENT (IN) :: NRGTX(NTXE)
   INTEGER, INTENT (IN) :: NRX(NRXG), UNITS(NRXG), RX_TYPE(NRXG), NCMPG(NRXG)
   INTEGER, INTENT (IN) :: PRTCMP(10, NRXG)

   INTEGER, INTENT (IN) :: RGTXID(NRXG, NTXE)

   INTEGER :: JD, JG, JS, JV, JX, JR, JC, JC1
   INTEGER :: KA, KZ, KT, KG, KC, KV
   INTEGER :: N_CORNR, NLOC, STNS, N_PRFL

   COMPLEX, INTENT (IN) :: BFD(NFRQ, LRX, NTXE, MAX_CMP)

   COMPLEX, ALLOCATABLE :: OUT_TOTL(:,:), OUT_HOLD(:,:)

   REAL, PARAMETER :: RAD2DEG = (180. / 3.14159265)

   REAL (KIND = DP), INTENT (IN) :: RXED(MRX, NRXG, MQVR), RXND(MRX, NRXG, MQVR), RXZD(MRX, NRXG, MQVR)

   REAL (KIND = DP), ALLOCATABLE :: RXPLT(:, :)
   REAL (KIND = DP), ALLOCATABLE :: SLENG(:)

   REAL, INTENT (IN) :: SXE(MXVRTX, NTXE), SXN(MXVRTX, NTXE), SXZ(MXVRTX, NTXE)
   REAL, INTENT (IN) :: RXDIP(MRX, NRXG), RXAZ(MRX, NRXG), RXMNT(MRX, NRXG)
   REAL, INTENT (IN) :: FREQ(NFRQ)
   REAL, INTENT (IN) :: SXDIP(NTXE), SXAZ(NTXE), SXMNT(NTXE)

   REAL :: OUT_SCALE(MRX, NRXG)

   CHARACTER (LEN = 120), INTENT (IN) :: TITLE

   CHARACTER (LEN = 06), PARAMETER :: PROG_NAME = "Arjuna"
   CHARACTER (LEN = 10), PARAMETER :: FILE_NAME = "Arjuna.amx"

   CHARACTER (LEN = 03) :: OUT_DTYPE(MAX_TYP)
   CHARACTER (LEN = 03) :: OUT_RXTYP2(MAX_RCT)
   CHARACTER (LEN = 03) :: OUT_COMP(10)
   CHARACTER (LEN = 04) :: OUT_UNITS(NRXG)
   CHARACTER (LEN = 15) :: OUT_RXTYP1(MAX_RCT)
   CHARACTER (LEN = 15) :: OUT_SURVEY(MAX_SRC)
   CHARACTER (LEN = 16) :: OUT_SOURCE(MAX_SRC + 1)

   DATA OUT_RXTYP2 / "MDP", "FLP", "EDP"/
   DATA OUT_SOURCE / "General_Loop    ", &
                     "Grounded_Wire   ", &
                     "Magnetic_Dipole ", &
                     "Magnetotellurics", &
                     "Coincident-loop "/
   DATA OUT_SURVEY / "Separate_Tx_Rx ", &
                     "Slingram       ", &
                     "Central_loop   ", &
                     "Coincident_loop" /
   DATA OUT_RXTYP1 / "Magnetic_dipole", &
                     "Finite_loop    ", &
                     "Electric_dipole"/
   DATA OUT_COMP   / " N", " E", " Z", &
                     "HR", "SL", "AX", &
                     "ED", "LP", "DZ", " Z"/
   DATA OUT_DTYPE  /"_TF"/

!
!  Enforce profile mode for output ...
   N_PRFL = 1

!
!  Code begins -- open output file & write first standard three lines ...
   OPEN (UNIT = NA, FILE = FILE_NAME, STATUS = "REPLACE")

   WRITE (NA, 1000) TRIM(TITLE)
   WRITE (NA, 1002) AMX_MAJOR, AMX_MINOR, PROG_NAME
   CALL WFAMX_HEADER(NA, OUTPUT, NFRQ)

!
!  Determine output scaling factors & units...
   CALL SET_OUTPUT_SCALING (NRXG, MRX, SURVEY_TYPE, RX_TYPE, RXMNT, STEP, UNITS, OUT_SCALE, OUT_UNITS)

!
!  Write header blocks ...
   WRITE (NA, 1003) N_PRFL, OUTPUT
   WRITE (NA, 1004) OUT_SURVEY(SURVEY_TYPE), OUT_SOURCE(SOURCE_TYPE)

   SELECT CASE (SURVEY_TYPE)
   CASE (1)
     NLOC = MRX
     WRITE (NA, 1005) NTXE
     DO JS = 1, NTXE
       WRITE (NA, 1001)
       IF (SOURCE_TYPE == 3) WRITE (NA, 1006) JS, SXE(1, JS), SXN(1, JS), SXZ(1, JS), &
                                                  SXMNT(JS), RAD2DEG * SXDIP(JS), RAD2DEG * SXAZ(JS)
       IF (SOURCE_TYPE /= 3) WRITE (NA, 1007) JS, N_VRTX(JS), (SXE(JV, JS), SXN(JV, JS), SXZ(JV, JS), JV = 1, N_VRTX(JS))
!
!      Now Rx groups for current transmitter ...
       DO JG = 1, NRGTX(JS)
           WRITE (NA, 1001)
           KG = RGTXID(JG, JS)
           KT = RX_TYPE(KG)
           JX = NRX(KG)

           IF (KT == 1) N_CORNR = 1        ! dipole ...
           IF (KT == 2) N_CORNR = 4        ! loop ...
           IF (KT == 3) N_CORNR = 2        ! bipole ...

           ALLOCATE (RXPLT(3, NRX(KG)))
           ALLOCATE (SLENG(NRX(KG)))
           CALL WAMX_POS(0, MRX, NRXG, MQVR, JX, KG, N_CORNR, RXED, RXND, RXZD, RXPLT, SLENG, STNS)

           WRITE (NA, 1008) JS, JG, OUT_RXTYP1(KT)
           WRITE (NA, 1009) JS, JG, TRIM(OUT_UNITS(JG))
           WRITE (NA, 1010) JS, JG, JX

!
!          Receiver parameters for current group ...
           WRITE (NA, 1001)
           IF (KT == 1) THEN
               DO JR = 1, JX
               WRITE (NA, 1011) JS, JG, JR, RXED(JR, KG, 1), RXND(JR, KG, 1), RXZD(JR, KG, 1), &
                                   RXMNT(JR, KG), RXDIP(JR, KG), RXAZ(JR, KG)
               END DO
           ELSE
               DO JR = 1, JX
               WRITE (NA, 1016) JS, JG, JR, N_CORNR, &
                                   (RXED(JR, KG, KV), RXND(JR, KG, KV), RXZD(JR, KG, KV), KV = 1, N_CORNR)
               END DO
           END IF

           DEALLOCATE (RXPLT, SLENG)

       END DO
     END DO


!
!    Usually, we'd branch for Profile / spectral mode, but for multiple receivers per transmitters, there seems
!    little point.  Instead, write frequencies no matter the mode.
     WRITE (NA, 1001)
     WRITE (NA, 1012) "/FREQSINGLE(Hz)=", FREQ(1: NFRQ)

!
!    We are now(!) in a position to write data ...
     DO JS = 1, NTXE
       KA = 1
       DO JG = 1, NRGTX(JS)

         KG = RGTXID(JG, JS)
         KT = RX_TYPE(KG)
         JX = NRX(KG)
         KZ = (KA + JX) - 1

         IF (KT == 1) N_CORNR = 1        ! dipole ...
         IF (KT == 2) N_CORNR = 4        ! loop ...
         IF (KT == 3) N_CORNR = 2        ! bipole ...

!
!        Set up plotting points ...
         ALLOCATE (RXPLT(3, NRX(KG)))
         ALLOCATE (SLENG(NRX(KG)))
         CALL WAMX_POS(0, MRX, NRXG, MQVR, JX, KG, N_CORNR, RXED, RXND, RXZD, RXPLT, SLENG, STNS)

!
!        Loop over components ...
         DO JC = 1, NCMPG(JG)
           WRITE (NA, 1001)
           IF (KT > 1) THEN
               KC = 1
               JC1 = 7
           ELSE
               JC1 = PRTCMP(JC, KG)
               KC = JC1
               IF (KC > 3) KC = KC - 3
           END IF

!
!          Fill output arrays & calculate percentage-target effect ...
           ALLOCATE (OUT_TOTL(NFRQ, JX), OUT_HOLD(NFRQ, JX))
           OUT_TOTL(1: NFRQ, 1: JX) = BFD(1: NFRQ, KA: KZ, JS, KC)
           DO JR = 1, JX
               OUT_TOTL(1: NFRQ, JR) = SXMNT(JS) * OUT_TOTL(1: NFRQ, JR) * OUT_SCALE(JR, KG)
           END DO

           DO JR = 1, JX

               SELECT CASE (OUTPUT)
               CASE (10, 11, 12)

                   WRITE (NA, 1014, ADVANCE = "NO") (LBASE + 1000 * JS + 10 * JG), JS, JG, JR, &
                                                   SLENG(JR), RXPLT(STNS, JR), RXPLT(1, JR), RXPLT(2, JR), RXPLT(3, JR), &
                                                   OUT_COMP(JC1), OUT_RXTYP2(KT), NFRQ

                   WRITE (NA, 1015, ADVANCE = "NO") AIMAG(OUT_TOTL(1: NFRQ, JR))
                   WRITE (NA, 1015, ADVANCE = "YES") REAL(OUT_TOTL(1: NFRQ, JR))

               CASE (20, 21)

                   WRITE (NA, 1018, ADVANCE = "NO") (LBASE + 1000 * JS + 10 * JG), "_TF", &
                                                   JS, JG, JR, &
                                                   SLENG(JR), RXPLT(STNS, JR), RXPLT(1, JR), RXPLT(2, JR), RXPLT(3, JR), &
                                                   OUT_COMP(JC1), OUT_RXTYP2(KT), NFRQ

                   WRITE (NA, 1015, ADVANCE = "NO") AIMAG(OUT_TOTL(1: NFRQ, JR))
                   WRITE (NA, 1015, ADVANCE = "YES") REAL(OUT_TOTL(1: NFRQ, JR))

               END SELECT

           END DO

           DEALLOCATE (OUT_TOTL, OUT_HOLD)

         END DO

!
!        Special case of output == 21 (blocked mode, all data).  We need this because of the requirement to group
!        all data of a certain type together.  Without the special case, we get the different types printed at the
!        same station and this makes the file quite difficult to process
         IF (OUTPUT == 22) THEN
           DO JD = 1, MAX_TYP
             WRITE (NA, 1001)
             DO JC = 1, NCMPG(JG)
               WRITE (NA, 1001)
               IF (KT > 1) THEN
                 KC = 1
                 JC1 = 7
               ELSE
                 JC1 = PRTCMP(JC, KG)
                 KC = JC1
                 IF (KC > 3) KC = KC - 3
               END IF

!
!              Fill output arrays & calculate percentage-target effect ...
               ALLOCATE (OUT_TOTL(NFRQ, NLOC), OUT_HOLD(NFRQ, NLOC))
               OUT_TOTL(1: NFRQ, 1: NLOC) =  BFD(1: NFRQ, 1: NLOC, 1, KC)
               DO JR = 1, NLOC
                 OUT_TOTL(1: NFRQ, JR) = OUT_TOTL(1: NFRQ, JR) * SXMNT(1) * OUT_SCALE(JR, 1)
               END DO

               IF (JD == 1) OUT_HOLD = OUT_TOTL

               DO JR = 1, JX
                   WRITE (NA, 1018, ADVANCE = "NO") (LBASE + 1000 * JS + 10 * JG), OUT_DTYPE(JD), &
                                                       JS, JG, JR, &
                                                       SLENG(JR), RXPLT(STNS, JR), RXPLT(1, JR), RXPLT(2, JR), RXPLT(3, JR), &
                                                       OUT_COMP(JC1), OUT_RXTYP2(KT), NFRQ

                   WRITE (NA, 1015, ADVANCE = "NO")  AIMAG(OUT_HOLD(1: NFRQ, JR))
                   WRITE (NA, 1015, ADVANCE = "YES")  REAL(OUT_HOLD(1: NFRQ, JR))
               END DO

               DEALLOCATE (OUT_TOTL, OUT_HOLD)

             END DO
           END DO
         END IF

         DEALLOCATE (RXPLT, SLENG)
         KA = KZ + 1

       END DO
     END DO

   CASE DEFAULT
     NLOC = NTXE
     JG = 1; KT = 1

!
!    Write transmitter parameters ....
     WRITE (NA, 1005) NLOC
     WRITE (NA, 1001)
     SELECT CASE (SOURCE_TYPE)
     CASE (3)
       DO JS = 1, NLOC
         WRITE (NA, 1006) JS, SXE(1, JS), SXN(1, JS), SXZ(1, JS), SXMNT(JS), RAD2DEG * SXDIP(JS), RAD2DEG * SXAZ(JS)
       END DO
     CASE DEFAULT
       DO JS = 1, NLOC
         WRITE (NA, 1007) JS, N_VRTX(JS), (SXE(JV, JS), SXN(JV, JS), SXZ(JV, JS), JV = 1, N_VRTX(JS))
       END DO
     END SELECT

!
!    Write receiver parameters ...
     WRITE (NA, 1001)
     WRITE (NA, 1008) 1, JG, OUT_RXTYP1(KT)
     WRITE (NA, 1009) 1, JG, TRIM(OUT_UNITS(JG))
     WRITE (NA, 1010) 1, JG, NLOC

     DO JR = 1, NLOC
       WRITE (NA, 1011) 1, JG, JR, &
                       RXED(1, JR, 1), RXND(1, JR, 1), RXZD(1, JR, 1), &
                       RXMNT(JR, 1), RXDIP(JR, 1), RXAZ(JR, 1)
     END DO

     ALLOCATE (RXPLT(3, NLOC))
     ALLOCATE (SLENG(NLOC))
     CALL WAMX_POS(1, MRX, NRXG, MQVR, NLOC, JG, N_CORNR, RXED, RXND, RXZD, RXPLT, SLENG, STNS)

!
!    Branch for profile / spectral mode ...
     WRITE (NA, 1001)
     WRITE (NA, 1012) "/FREQSINGLE(Hz)=", FREQ(1: NFRQ)

!
!    Completed writing header section; start on data ...
     DO JC = 1, NCMPG(1)
       WRITE (NA, 1001)
       IF (KT > 1) THEN
         KC = 1
         JC1 = 7
       ELSE
         JC1 = PRTCMP(JC, 1)
         KC = JC1
         IF (KC > 3) KC = KC - 3
       END IF

!
!      Fill output arrays
       ALLOCATE (OUT_TOTL(NFRQ, NLOC), OUT_HOLD(NFRQ, NLOC))
       OUT_TOTL(1: NFRQ, 1: NLOC) =  BFD(1: NFRQ, 1: NLOC, 1, KC)
       DO JR = 1, NLOC
         OUT_TOTL(1: NFRQ, JR) = OUT_TOTL(1: NFRQ, JR) * SXMNT(1) * OUT_SCALE(JR, 1)
       END DO

       DO JS = 1, NLOC

           SELECT CASE (OUTPUT)
           CASE (10, 11, 12)

               WRITE (NA, 1014, ADVANCE = "NO") (LBASE + 10 * JG), 1, 1, JS, &
                                               SLENG(JS), RXPLT(STNS, JS), RXPLT(1, JS), RXPLT(2, JS), RXPLT(3, JS), &
                                               OUT_COMP(JC1), OUT_RXTYP2(1), NFRQ

               WRITE (NA, 1015, ADVANCE = "NO") AIMAG(OUT_TOTL(1: NFRQ, JS))
               WRITE (NA, 1015, ADVANCE = "YES") REAL(OUT_TOTL(1: NFRQ, JS))

           CASE (20, 21)

               WRITE (NA, 1018, ADVANCE = "NO") (LBASE + 10 * JG), "_TF", 1, 1, JS, &
                                               SLENG(JS), RXPLT(STNS, JS), RXPLT(1, JS), RXPLT(2, JS), RXPLT(3, JS), &
                                               OUT_COMP(JC1), OUT_RXTYP2(1), NFRQ

               WRITE (NA, 1015, ADVANCE = "NO") AIMAG(OUT_TOTL(1: NFRQ, JS))
               WRITE (NA, 1015, ADVANCE = "YES") REAL(OUT_TOTL(1: NFRQ, JS))

           END SELECT

       END DO

       DEALLOCATE (OUT_TOTL, OUT_HOLD)

     END DO

!
!    Special case of output == 22 (blocked mode, all data).  We need this because of the requirement to group
!    all data of a certain type together.  Without the special case, we get the different types printed at the
!    same station and this makes the file quite difficult to process
     IF (OUTPUT == 22) THEN
       DO JD = 1, MAX_TYP
         WRITE (NA, 1001)
         DO JC = 1, NCMPG(1)
           WRITE (NA, 1001)
           IF (KT > 1) THEN
             KC = 1
             JC1 = 7
           ELSE
             JC1 = PRTCMP(JC, 1)
             KC = JC1
             IF (KC > 3) KC = KC - 3
           END IF

!
!          Fill output arrays & calculate percentage-target effect ...
           ALLOCATE (OUT_TOTL(NFRQ, NLOC), OUT_HOLD(NFRQ, NLOC))
           OUT_TOTL(1: NFRQ, 1: NLOC) =  BFD(1: NFRQ, 1: NLOC, 1, KC)
           DO JR = 1, NLOC
               OUT_TOTL(1: NFRQ, JR) = OUT_TOTL(1: NFRQ, JR) * SXMNT(1) * OUT_SCALE(JR, 1)
           END DO

           IF (JD == 1) OUT_HOLD = OUT_TOTL

           DO JS = 1, NLOC
               WRITE (NA, 1018, ADVANCE = "NO") (LBASE + 10 * JG), OUT_DTYPE(JD), 1, 1, JS, &
                                                   SLENG(JS), RXPLT(STNS, JS), RXPLT(1, JS), RXPLT(2, JS), RXPLT(3, JS), &
                                                   OUT_COMP(JC1), OUT_RXTYP2(1), NFRQ

               WRITE (NA, 1015, ADVANCE = "NO") AIMAG(OUT_HOLD(1: NFRQ, JS))
               WRITE (NA, 1015, ADVANCE = "YES") REAL(OUT_HOLD(1: NFRQ, JS))
           END DO

           DEALLOCATE (OUT_TOTL, OUT_HOLD)

         END DO

       END DO

     END IF

     DEALLOCATE (RXPLT, SLENG)

   END SELECT

   CLOSE (NA)

!  Formats ...
1000 FORMAT (a)
1001 FORMAT ("/")
1002 FORMAT ("AMX_VERSION=", i2.2, ".", i2.2, 2x, "PROGRAM_NAME:", a)
1003 FORMAT ("/", /, "/FILE_TYPE=", i2.2, /, "/FIELDS_FORMAT=", i2.2, /, "/")
1004 FORMAT ("/", /, "/SURVEY_TYPE=", a, /, "/SOURCE_TYPE=", a, /, "/")
1005 FORMAT ("/TX_TOTAL=", i3.3)
1006 FORMAT ("/TX", i3.3, "_COORDS(N,E,Z,M,Dip,Az)=", 6(f13.2, :, ", ", 2x))
1007 FORMAT ("/TX", i3.3, "_COORDS(NVRTxN,E,Z)=", i2.2, ", ", 256(f13.2, ", ", f13.2, ", ", f13.2, :, ", "))
1008 FORMAT ("/TX", i3.3, "_RXGroup", i3.3, "_TYPE=", a)
1009 FORMAT ("/TX", i3.3, "_RXGroup", i3.3, "_UNITS=", 3(a, :, ","))
1010 FORMAT ("/TX", i3.3, "_RXGroup", i3.3, "_NUMBER=", i3.3)
1011 FORMAT ("/TX", i3.3, "_RXGroup", i3.3, "_RXN", i3.3, "_PARAMS(N,E,Z,M,Dip,Az)=", 6(f13.2, :, ", "))
1012 FORMAT (a, 2x, 512(f13.2, :, ", "))
1014 FORMAT (2x, i6, 3(2x, i4), 5(2x, f13.2), 2x, a2, 2x, a3, 2x, i4)
1015 FORMAT (512(:, 2x, e15.6))
1016 FORMAT ("/TX", i3.3, "_RXGroup", i3.3, "_RXN", i3.3, "_PARAMS(nv,Nv,Ev,Zv)=", &
             i2.2, ", ", 4(f13.2, ", ", f13.2, ", ", f13.2, :, ", "))
1018 FORMAT (2x, i6, a3, 3(2x, i4), 5(2x, f13.2), 2x, a2, 2x, a3, 2x, i4)

  END SUBROUTINE WRITE_FAMX
!--------------

 SUBROUTINE WRITE_FAMX_PPM(TITLE, &
               STEP, OUTPUT, SOURCE_TYPE, SURVEY_TYPE, &
               NFRQ, FREQ, &
               NTXE, MXVRTX, SXE, SXN, SXZ, SXDIP, SXAZ, SXMNT, &
               NRXG, NCMPG, PRTCMP, &
               LRX, MRX, MQVR, &
               RXED, RXND, RXZD, RXDIP, RXAZ, RXMNT, &
               BFD, &
               DXPRM, CURNT, CMPDX)
!---------------------------------------------------------------------------------

!  Calls       wfamx_header
!              set_output_scaling
!              wamx_pos
!              wamx_cldist
!              fem_perc
!  Called by:  main

   IMPLICIT NONE

   INTEGER, PARAMETER :: AMX_MAJOR = 1
   INTEGER, PARAMETER :: AMX_MINOR = 0
   INTEGER, PARAMETER :: NA = 30
   INTEGER, PARAMETER :: MAX_RCT = 3
   INTEGER, PARAMETER :: MAX_CMP = 3
   INTEGER, PARAMETER :: MAX_TYP = 1
   INTEGER, PARAMETER :: MAX_SRC = 4
   INTEGER, PARAMETER :: LBASE = 10000
   INTEGER, PARAMETER :: DP = SELECTED_REAL_KIND(12, 80)

   INTEGER, INTENT (IN) :: STEP, OUTPUT
   INTEGER, INTENT (IN) :: SOURCE_TYPE, SURVEY_TYPE
   INTEGER, INTENT (IN) :: NFRQ
   INTEGER, INTENT (IN) :: NTXE, MXVRTX
   INTEGER, INTENT (IN) :: NRXG, LRX, MRX, MQVR

   INTEGER, INTENT (IN) :: NCMPG(NRXG)
   INTEGER, INTENT (IN) :: PRTCMP(10, NRXG)

   INTEGER, INTENT (IN) :: CMPDX

   INTEGER :: JD, JG, JS, JR, JC, JC1, JF
   INTEGER :: KT, KC
   INTEGER :: NLOC, STNS, N_PRFL

   LOGICAL :: CONVERT

   COMPLEX, PARAMETER :: I2PI= (0.,6.283185)  ! 2 * pi * i

   COMPLEX, INTENT (IN) :: BFD(NFRQ, LRX, NTXE, MAX_CMP)

   COMPLEX, ALLOCATABLE :: OUT_TOTL(:,:), OUT_HOLD(:,:)

   REAL, PARAMETER :: RAD2DEG = (180. / 3.14159265)

   REAL (KIND = DP), INTENT (IN) :: RXED(MRX, NRXG, MQVR), RXND(MRX, NRXG, MQVR), RXZD(MRX, NRXG, MQVR)

   REAL (KIND = DP), ALLOCATABLE :: RXPLT(:, :)
   REAL (KIND = DP), ALLOCATABLE :: SLENG(:)

   REAL, INTENT (IN) :: SXE(MXVRTX, NTXE), SXN(MXVRTX, NTXE), SXZ(MXVRTX, NTXE)
   REAL, INTENT (IN) :: RXDIP(MRX, NRXG), RXAZ(MRX, NRXG), RXMNT(MRX, NRXG)
   REAL, INTENT (IN) :: FREQ(NFRQ), CURNT(NFRQ)
   REAL, INTENT (IN) :: SXDIP(NTXE), SXAZ(NTXE), SXMNT(NTXE)
   REAL, INTENT (IN) :: DXPRM(MAX_CMP, MAX_CMP - 1)

   REAL :: TOTAL_F, RTMP, QTMP

   CHARACTER (LEN = 120), INTENT (IN) :: TITLE

   CHARACTER (LEN = 06), PARAMETER :: PROG_NAME = "Arjuna"
   CHARACTER (LEN = 10), PARAMETER :: FILE_NAME = "Arjuna.amx"

   CHARACTER (LEN = 03) :: OUT_DTYPE(MAX_TYP)
   CHARACTER (LEN = 03) :: OUT_RXTYP2(MAX_RCT)
   CHARACTER (LEN = 03) :: OUT_COMP(10)
   CHARACTER (LEN = 04) :: OUT_UNITS(NRXG)
   CHARACTER (LEN = 10) :: OUT_ACTUAL(MAX_CMP + 1)
   CHARACTER (LEN = 11) :: OUT_NORMAL(MAX_CMP)
   CHARACTER (LEN = 15) :: OUT_RXTYP1(MAX_RCT)
   CHARACTER (LEN = 15) :: OUT_SURVEY(MAX_SRC)
   CHARACTER (LEN = 16) :: OUT_SOURCE(MAX_SRC + 1)

   DATA OUT_RXTYP2 / "MDP", "FLP", "EDP"/
   DATA OUT_SOURCE / "General_Loop    ", &
                     "Grounded_Wire   ", &
                     "Magnetic_Dipole ", &
                     "Magnetotellurics", &
                     "Coincident-loop "/
   DATA OUT_SURVEY / "Separate_Tx_Rx ", &
                     "Slingram       ", &
                     "Central_loop   ", &
                     "Coincident_loop" /
   DATA OUT_RXTYP1 / "Magnetic_dipole", &
                     "Finite_loop    ", &
                     "Electric_dipole"/
   DATA OUT_COMP   / " N", " E", " Z", &
                     "HR", "SL", "AX", &
                     "ED", "LP", "DZ", " Z"/
   DATA OUT_DTYPE  /"_TF"/
   DATA OUT_NORMAL /"TOTAL_FIELD", &
                    "UNUSED     ", &
                    "COMPONENT  "/
   DATA OUT_ACTUAL /"HORIZONTAL", &
                    "SLOPE     ", &
                    "AXIAL     ", &
                    "TOTAL     "/

!
!  Enforce profile mode for output ...
   N_PRFL = 1;
   CONVERT = .FALSE.

!
!  This means that data have already been converted to voltages in WRITE_FD.  We need to convert
!  them back to fields so that normalisation works properly ...
   IF (STEP == 0) CONVERT = .TRUE.

!
!  Code begins -- open output file & write first standard three lines ...
   OPEN (UNIT = NA, FILE = FILE_NAME, STATUS = "REPLACE")

   WRITE (NA, 1000) TRIM(TITLE)
   WRITE (NA, 1002) AMX_MAJOR, AMX_MINOR, PROG_NAME
   CALL WFAMX_HEADER(NA, OUTPUT, NFRQ)

!
!  Write header blocks ...
   TOTAL_F = SQRT(DXPRM(1, 1)**2 + DXPRM(2, 1)**2 + DXPRM(3, 1)**2)
   WRITE (NA, 1003) N_PRFL, OUTPUT
   WRITE (NA, 1004) OUT_SURVEY(SURVEY_TYPE), OUT_SOURCE(SOURCE_TYPE)
   WRITE (NA, 1020) OUT_NORMAL(CMPDX)
   WRITE (NA, 1021) 1.e12 * DXPRM(3, 1), 1.e12 * DXPRM(2, 1), 1.e12 * DXPRM(1, 1), 1.e12 * TOTAL_F
   WRITE (NA, 1001)

!
!  Tell the user what we are _actually_ doing.  This is determined by equality of slots 1 & 2
!  in DXPRM ie.  DXPRM(i, 1) == DXPRM(i, 2) => component
!                DXPRM(i, 1) != DXPRM(i, 2) => total field
   IF (CMPDX == 3) THEN
     DO JC = 1, 3
       IF (ABS(DXPRM(JC, 1) - DXPRM(JC, 2)) < TINY(1.0)) THEN
         WRITE (NA, 1022) TRIM(OUT_ACTUAL(JC)), TRIM(OUT_ACTUAL(JC))
       ELSE
         WRITE (NA, 1022) TRIM(OUT_ACTUAL(JC)), TRIM(OUT_ACTUAL(4))
       END IF
     END DO
   END IF
   WRITE (NA, 1001)

!
!  Write units to array ...
   DO JR = 1, MRX
     DO JG = 1, NRXG
       OUT_UNITS(JG) = "ppm"
     END DO
   END DO

   SELECT CASE (SURVEY_TYPE)
   CASE DEFAULT
     NLOC = NTXE
     JG = 1; KT = 1

!
!    Write transmitter parameters ....

     WRITE (NA, 1005) NLOC
     WRITE (NA, 1001)
     DO JS = 1, NLOC
       WRITE (NA, 1006) JS, SXE(1, JS), SXN(1, JS), SXZ(1, JS), SXMNT(JS), RAD2DEG * SXDIP(JS), RAD2DEG * SXAZ(JS)
     END DO

!
!    Write receiver parameters ...
     WRITE (NA, 1001)
     WRITE (NA, 1008) 1, JG, OUT_RXTYP1(KT)
     WRITE (NA, 1009) 1, JG, TRIM(OUT_UNITS(JG))


     DO JR = 1, NLOC
       WRITE (NA, 1011) 1, JG, JR, &
                       RXED(1, JR, 1), RXND(1, JR, 1), RXZD(1, JR, 1), &
                       RXMNT(JR, 1), RXDIP(JR, 1), RXAZ(JR, 1)
     END DO

     ALLOCATE (RXPLT(3, NLOC))
     ALLOCATE (SLENG(NLOC))
     CALL WAMX_POS(1, MRX, NRXG, MQVR, NLOC, JG, 1, RXED, RXND, RXZD, RXPLT, SLENG, STNS)

     WRITE (NA, 1001)
     WRITE (NA, 1012) "/FREQSINGLE(Hz)=", FREQ(1: NFRQ)

!
!    Completed writing header section; start on data ...
     DO JC = 1, NCMPG(1)
       WRITE (NA, 1001)
       IF (KT > 1) THEN
         KC = 1
         JC1 = 7
       ELSE
         JC1 = PRTCMP(JC, 1)
         KC = JC1
         IF (KC > 3) KC = KC - 3
       END IF

!
!      Fill output arrays & calculate percentage-target effect ...
       ALLOCATE (OUT_TOTL(NFRQ, NLOC), OUT_HOLD(NFRQ, NLOC))

       OUT_TOTL(1: NFRQ, 1: NLOC) =  BFD(1: NFRQ, 1: NLOC, 1, KC)
       DO JS = 1, NLOC

            IF (CONVERT) OUT_TOTL(1: NFRQ, JS) = OUT_TOTL(1: NFRQ, JS) / (FREQ(1: NFRQ) * I2PI)

            OUT_TOTL(1: NFRQ, JS) = OUT_TOTL(1: NFRQ, JS) / CURNT(1: NFRQ)
            DO JF = 1, NFRQ
               RTMP = REAL(OUT_TOTL(JF, JS))
               QTMP = AIMAG(OUT_TOTL(JF, JS))
               RTMP = (RTMP - DXPRM(KC, 1)) / DXPRM(KC, 2)
               QTMP =  QTMP / DXPRM(KC, 2)

               OUT_TOTL(JF, JS) = CMPLX(RTMP, QTMP)

            END DO
       END DO
       OUT_TOTL(1: NFRQ, 1: NLOC) = 1.0E6 * OUT_TOTL(1: NFRQ, 1: NLOC)

       DO JS = 1, NLOC

           SELECT CASE (OUTPUT)
           CASE (10, 11, 12)

               WRITE (NA, 1014, ADVANCE = "NO") (LBASE + 10 * JG), 1, 1, JS, &
                                               SLENG(JS), RXPLT(STNS, JS), RXPLT(1, JS), RXPLT(2, JS), RXPLT(3, JS), &
                                               OUT_COMP(JC1), OUT_RXTYP2(1), NFRQ

               WRITE (NA, 1015, ADVANCE = "NO") AIMAG(OUT_TOTL(1: NFRQ, JS))
               WRITE (NA, 1015, ADVANCE = "YES") REAL(OUT_TOTL(1: NFRQ, JS))

           CASE (20, 21)

               WRITE (NA, 1018, ADVANCE = "NO") (LBASE + 10 * JG), "_TF", 1, 1, JS, &
                                               SLENG(JS), RXPLT(STNS, JS), RXPLT(1, JS), RXPLT(2, JS), RXPLT(3, JS), &
                                               OUT_COMP(JC1), OUT_RXTYP2(1), NFRQ

               WRITE (NA, 1015, ADVANCE = "NO") AIMAG(OUT_TOTL(1: NFRQ, JS))
               WRITE (NA, 1015, ADVANCE = "YES") REAL(OUT_TOTL(1: NFRQ, JS))

           END SELECT

       END DO

       DEALLOCATE (OUT_TOTL, OUT_HOLD)

     END DO

!
!    Special case of output == 21 (blocked mode, all data).  We need this because of the requirement to group
!    all data of a certain type together.  Without the special case, we get the different types printed at the
!    same station and this makes the file quite difficult to process
     IF (OUTPUT == 22) THEN
       DO JD = 1, MAX_TYP
         WRITE (NA, 1001)
         DO JC = 1, NCMPG(1)
           WRITE (NA, 1001)
           IF (KT > 1) THEN
             KC = 1
             JC1 = 7
           ELSE
             JC1 = PRTCMP(JC, 1)
             KC = JC1
             IF (KC > 3) KC = KC - 3
           END IF

!
!          Fill output arrays & calculate percentage-target effect ...
           ALLOCATE (OUT_TOTL(NFRQ, NLOC), OUT_HOLD(NFRQ, NLOC))
           OUT_TOTL(1: NFRQ, 1: NLOC) =  BFD(1: NFRQ, 1: NLOC, 1, KC)

           DO JS = 1, NLOC

               IF (CONVERT) OUT_TOTL(1: NFRQ, JS) = OUT_TOTL(1: NFRQ, JS) / (FREQ(1: NFRQ) * I2PI)

               OUT_TOTL(1: NFRQ, JS) = OUT_TOTL(1: NFRQ, JS) / CURNT(1: NFRQ)
               DO JF = 1, NFRQ
                   RTMP = REAL(OUT_TOTL(JF, JS))
                   QTMP = AIMAG(OUT_TOTL(JF, JS))
                   RTMP = RTMP / DXPRM(KC, 2)
                   QTMP = QTMP / DXPRM(KC, 2)

                   OUT_TOTL(JF, JS) = CMPLX(RTMP, QTMP)

               END DO
           END DO
           OUT_TOTL(1: NFRQ, 1: NLOC) = 1.0E6 * OUT_TOTL(1: NFRQ, 1: NLOC)

           IF (JD == 1) OUT_HOLD = OUT_TOTL

           DO JS = 1, NLOC
               WRITE (NA, 1018, ADVANCE = "NO") (LBASE + 10 * JG), OUT_DTYPE(JD), 1, 1, JS, &
                                                   SLENG(JS), RXPLT(STNS, JS), RXPLT(1, JS), RXPLT(2, JS), RXPLT(3, JS), &
                                                   OUT_COMP(JC1), OUT_RXTYP2(1), NFRQ

               WRITE (NA, 1015, ADVANCE = "NO") AIMAG(OUT_HOLD(1: NFRQ, JS))
               WRITE (NA, 1015, ADVANCE = "YES") REAL(OUT_HOLD(1: NFRQ, JS))
           END DO

           DEALLOCATE (OUT_TOTL, OUT_HOLD)

         END DO

       END DO

     END IF

     DEALLOCATE (RXPLT, SLENG)

   END SELECT

   CLOSE (NA)

!  Formats ...
1000 FORMAT (a)
1001 FORMAT ("/")
1002 FORMAT ("AMX_VERSION=", i2.2, ".", i2.2, 2x, "PROGRAM_NAME:", a)
1003 FORMAT ("/", /, "/FILE_TYPE=", i2.2, /, "/FIELDS_FORMAT=", i2.2, /, "/")
1004 FORMAT ("/", /, "/SURVEY_TYPE=", a, /, "/SOURCE_TYPE=", a, /, "/")
1005 FORMAT ("/TX_TOTAL=", i3.3)
1006 FORMAT ("/TX", i3.3, "_COORDS(N,E,Z,M,Dip,Az)=", 6(f13.2, :, ", ", 2x))
1008 FORMAT ("/TX", i3.3, "_RXGroup", i3.3, "_TYPE=", a)
1009 FORMAT ("/TX", i3.3, "_RXGroup", i3.3, "_UNITS=", 3(a, :, ","))
1011 FORMAT ("/TX", i3.3, "_RXGroup", i3.3, "_RXN", i3.3, "_PARAMS(N,E,Z,M,Dip,Az)=", 6(f13.2, :, ", "))
1012 FORMAT (a, 2x, 512(f13.2, :, ", "))
1014 FORMAT (2x, i6, 3(2x, i4), 5(2x, f13.2), 2x, a2, 2x, a3, 2x, i4)
1015 FORMAT (512(:, 2x, e15.6))
1018 FORMAT (2x, i6, a3, 3(2x, i4), 5(2x, f13.2), 2x, a2, 2x, a3, 2x, i4)
1020 FORMAT ("/NORMALISATION=", a)
1021 FORMAT ("/PRIMARY_FIELD_AXIAL(pT)=", en13.4, /, &
             "/PRIMARY_FIELD_SLOPE(pT)=", en13.4, /, &
             "/PRIMARY_FIELD_HORIZONTAL(pT)=", en13.4, /, &
             "/PRIMARY_FIELD_TOTAL(pT)=", en13.4)
1022 FORMAT ("/Normalising ", a, " component by ", a, " primary field")

  END SUBROUTINE WRITE_FAMX_PPM
!--------------


 SUBROUTINE WFAMX_HEADER(NA, OUTPUT, NOUT)
!-----------------------------------------------

!  Sub. writes AMX file column headers for FEM files

!   Calls:      none
!   Called by:  wrt_famx

!  Input
!    na       output unit
!    output   10 => total only (flat)
!             11 => total + scattered + % (flat)
!             20 => total only (blocked)
!             21 => total + scattered + % (blocked)
!    nout     # of data to write

!  Output
!       in file connected to unit na
!
!  Notes
!  1.   general nature of subroutine means that nout may be either nfrq or nstat
!       depending upon the setting of prfl.
!  2.   Data type indicators are appended (or not) depending upon detting of output
!       10, 11 => Append _TF, _TF, _SF, _PT
!       20, 21 => Do not append anything (blocked mode)

   IMPLICIT NONE

   INTEGER, INTENT (IN) :: NA, OUTPUT, NOUT
   INTEGER :: JO

   WRITE (NA, 1000, ADVANCE = "NO")
   WRITE (NA, 1002, ADVANCE = "NO")
   WRITE (NA, 1003, ADVANCE = "NO")
   WRITE (NA, 1005, ADVANCE = "NO") "NFrq"

   SELECT CASE (OUTPUT)
   CASE (10, 11)

       DO JO = 1, NOUT
           WRITE (NA, 1004, ADVANCE = "NO")  "QFrq", JO, "_TF"
       END DO
       DO JO = 1, NOUT - 1
           WRITE (NA, 1004, ADVANCE = "NO")  "IFrq", JO, "_TF"
       END DO
       WRITE (NA, 1004, ADVANCE = "YES")   "IFrq", NOUT, "_TF"

   CASE (12)

       DO JO = 1, NOUT
           WRITE (NA, 1004, ADVANCE = "NO")  "QFrq", JO, "_TF"
       END DO
       DO JO = 1, NOUT
           WRITE (NA, 1004, ADVANCE = "NO")  "IFrq", JO, "_TF"
       END DO
       DO JO = 1, NOUT
           WRITE (NA, 1004, ADVANCE = "NO")  "QFrq", JO, "_SF"
       END DO
       DO JO = 1, NOUT
           WRITE (NA, 1004, ADVANCE = "NO")  "IFrq", JO, "_SF"
       END DO
       DO JO = 1, NOUT
           WRITE (NA, 1004, ADVANCE = "NO")  "QFrq", JO, "_PT"
       END DO
       DO JO = 1, NOUT - 1
           WRITE (NA, 1004, ADVANCE = "NO")  "IFrq", JO, "_PT"
       END DO
       WRITE (NA, 1004, ADVANCE = "YES")   "IFrq", NOUT, "_PT"

   CASE (20, 21)

       DO JO = 1, NOUT
           WRITE (NA, 1004, ADVANCE = "NO")  "QFrq", JO, "  "
       END DO
       DO JO = 1, NOUT - 1
           WRITE (NA, 1004, ADVANCE = "NO")  "IFrq", JO, "  "
       END DO
       WRITE (NA, 1004, ADVANCE = "YES")   "IFrq", NOUT, "   "

   CASE (22)

       DO JO = 1, NOUT
           WRITE (NA, 1004, ADVANCE = "NO")  "QFrq", JO, "  "
       END DO
       DO JO = 1, NOUT - 1
           WRITE (NA, 1004, ADVANCE = "NO")  "IFrq", JO, "  "
       END DO
       WRITE (NA, 1004, ADVANCE = "YES")   "IFrq", NOUT, "   "

   END SELECT

   RETURN

1000 FORMAT ("Line", 2x, "TxGrp", 2x, "RxGrp", 2x, "Fid", 2x)
1002 FORMAT ("Dist", 2x, "Stn", 2x, "North", 2x, "East", 2x, "RxElev", 2x)
1003 FORMAT ("CMP", 2x, "RxType", 2x)
1004 FORMAT (2x, a, i3.3, a)
1005 FORMAT (a5, 2x)

 END SUBROUTINE WFAMX_HEADER
!--------------

 SUBROUTINE WRITE_TAMX(TITLE, &
               STEP, OUTPUT, SOURCE_TYPE, SURVEY_TYPE, &
               NCHNL, TMS, &
               NTXE, MXVRTX, N_VRTX, SXE, SXN, SXZ, SXDIP, SXAZ, SXMNT, &
               NRGTX, NRXG, RGTXID, NCMPG, PRTCMP, &
               LRX, MRX, NRX, MQVR, RX_TYPE, UNITS, &
               RXED, RXND, RXZD, RXDIP, RXAZ, RXMNT, CLCD, &
               BTD)
!----------------------------------------------------------------------------------

!  Calls       wtamx_header
!              set_output_scaling
!              wamx_pos
!              wamx_cldist
!              tem_perc
!  Called by:  main

   IMPLICIT NONE

   INTEGER, PARAMETER :: AMX_MAJOR = 1
   INTEGER, PARAMETER :: AMX_MINOR = 0
   INTEGER, PARAMETER :: NA = 30
   INTEGER, PARAMETER :: MAX_TYP = 1
   INTEGER, PARAMETER :: MAX_RCT = 3
   INTEGER, PARAMETER :: MAX_CMP = 3
   INTEGER, PARAMETER :: MAX_SRC = 4
   INTEGER, PARAMETER :: LBASE = 10000
   INTEGER, PARAMETER :: DP = SELECTED_REAL_KIND(12, 80)

   INTEGER, INTENT (IN) :: STEP, OUTPUT
   INTEGER, INTENT (IN) :: SOURCE_TYPE, SURVEY_TYPE
   INTEGER, INTENT (IN) :: NCHNL
   INTEGER, INTENT (IN) :: NTXE, MXVRTX
   INTEGER, INTENT (IN) :: N_VRTX(NTXE)
   INTEGER, INTENT (IN) :: NRXG, LRX, MRX, MQVR
   INTEGER, INTENT (IN) :: NRGTX(NTXE)
   INTEGER, INTENT (IN) :: NRX(NRXG), UNITS(NRXG), RX_TYPE(NRXG), NCMPG(NRXG)
   INTEGER, INTENT (IN) :: PRTCMP(10, NRXG)
   INTEGER, INTENT (IN) :: RGTXID(NRXG, NTXE)

   INTEGER :: JD, JG, JS, JV, JX, JR, JC, JC1
   INTEGER :: KA, KZ, KT, KG, KC, KV
   INTEGER :: N_CORNR, NLOC, STNS, N_PRFL

   REAL, PARAMETER :: RAD2DEG = (180. / 3.14159265)

   REAL (KIND = DP), INTENT (IN) :: RXED(MRX, NRXG, MQVR), RXND(MRX, NRXG, MQVR), RXZD(MRX, NRXG, MQVR), CLCD(MAX_CMP, NTXE)

   REAL (KIND = DP), ALLOCATABLE :: RXPLT(:, :)
   REAL (KIND = DP), ALLOCATABLE :: SLENG(:)

   REAL, INTENT (IN) :: TMS(NCHNL)
   REAL, INTENT (IN) :: SXE(MXVRTX, NTXE), SXN(MXVRTX, NTXE), SXZ(MXVRTX, NTXE)
   REAL, INTENT (IN) :: RXDIP(MRX, NRXG), RXAZ(MRX, NRXG), RXMNT(MRX, NRXG)
   REAL, INTENT (IN) :: SXDIP(NTXE), SXAZ(NTXE), SXMNT(NTXE)
   REAL, INTENT (IN) :: BTD(NCHNL, LRX, NTXE, MAX_CMP)

   REAL, ALLOCATABLE :: OUT_TOTL(:, :), OUT_HOLD(:, :)

   REAL :: OUT_SCALE(MRX, NRXG)

   CHARACTER (LEN = 06), PARAMETER :: PROG_NAME = "Arjuna"
   CHARACTER (LEN = 10), PARAMETER :: FILE_NAME = "Arjuna.amx"

   CHARACTER (LEN = 120), INTENT (IN) :: TITLE

   CHARACTER (LEN = 03) :: OUT_DTYPE(MAX_TYP)
   CHARACTER (LEN = 03) :: OUT_RXTYP2(MAX_RCT)
   CHARACTER (LEN = 03) :: OUT_COMP(10)
   CHARACTER (LEN = 04) :: OUT_UNITS(NRXG)
   CHARACTER (LEN = 15) :: OUT_RXTYP1(MAX_RCT)
   CHARACTER (LEN = 15) :: OUT_SURVEY(MAX_SRC)
   CHARACTER (LEN = 16) :: OUT_SOURCE(MAX_SRC + 1)

   DATA OUT_RXTYP2 / "MDP", "FLP", "EDP"/
   DATA OUT_SOURCE / "General_Loop    ", &
                   "Grounded_Wire   ", &
                   "Magnetic_Dipole ", &
                   "Magnetotellurics", &
                   "Coincident-loop "/
   DATA OUT_SURVEY / "Separate_Tx_Rx ", &
                   "Slingram       ", &
                   "Central_loop   ", &
                   "Coincident_loop" /
   DATA OUT_RXTYP1 / "Magnetic_dipole", &
                   "Finite_loop    ", &
                   "Electric_dipole"/
   DATA OUT_COMP   / " N", " E", " Z", &
                     "HR", "SL", "AX", &
                     "ED", "LP", " Z", " Z"/
   DATA OUT_DTYPE  /"_TF"/

!
!  End of declarations; code begins ...

!
!  Enfore profile output mode ...
   N_PRFL = 1

!
!  Open file & write std. header ...
   OPEN (UNIT = NA, FILE = FILE_NAME, STATUS = "REPLACE")

   WRITE (NA, 1000) TRIM(TITLE)
   WRITE (NA, 1002) AMX_MAJOR, AMX_MINOR, PROG_NAME
   CALL WTAMX_HEADER(NA, OUTPUT, NCHNL)

!
!  Determine output scaling factors & units...
   CALL SET_OUTPUT_SCALING (NRXG, MRX, SURVEY_TYPE, RX_TYPE, RXMNT, STEP, UNITS, OUT_SCALE, OUT_UNITS)

!
!  Write header blocks ...
   WRITE (NA, 1003) N_PRFL, OUTPUT
   WRITE (NA, 1004) OUT_SURVEY(SURVEY_TYPE), OUT_SOURCE(SOURCE_TYPE)

   SELECT CASE (SURVEY_TYPE)
   CASE (1)
     NLOC = MRX
     WRITE (NA, 1005) NTXE
     DO JS = 1, NTXE
       WRITE (NA, 1001)
       IF (SOURCE_TYPE == 3) WRITE (NA, 1006) JS, SXE(1, JS), SXN(1, JS), SXZ(1, JS), &
                                                  SXMNT(JS), RAD2DEG * SXDIP(JS), RAD2DEG * SXAZ(JS)
       IF (SOURCE_TYPE /= 3) WRITE (NA, 1007) JS, N_VRTX(JS), (SXE(JV, JS), SXN(JV, JS), SXZ(JV, JS), JV = 1, N_VRTX(JS))

!
!      Now Rx groups for current transmitter ...
       DO JG = 1, NRGTX(JS)
         WRITE (NA, 1001)
         KG = RGTXID(JG, JS)
         KT = RX_TYPE(KG)
         JX = NRX(KG)

         IF (KT == 1) N_CORNR = 1        ! dipole ...
         IF (KT == 2) N_CORNR = 4        ! loop ...
         IF (KT == 3) N_CORNR = 2        ! bipole ...

         ALLOCATE (RXPLT(3, NRX(KG)))
         ALLOCATE (SLENG(NRX(KG)))
         CALL WAMX_POS(0, MRX, NRXG, MQVR, JX, KG, N_CORNR, RXED, RXND, RXZD, RXPLT, SLENG, STNS)

         WRITE (NA, 1008) JS, JG, OUT_RXTYP1(KT)
         WRITE (NA, 1009) JS, JG, TRIM(OUT_UNITS(JG))
         WRITE (NA, 1010) JS, JG, JX

!
!        Receiver parameters for current group ...
         WRITE (NA, 1001)
         IF (KT == 1) THEN
           DO JR = 1, JX
             WRITE (NA, 1011) JS, JG, JR, RXED(JR, KG, 1), RXND(JR, KG, 1), RXZD(JR, KG, 1), &
                           RXMNT(JR, KG), RXDIP(JR, KG), RXAZ(JR, KG)
           END DO
         ELSE
           DO JR = 1, JX
             WRITE (NA, 1016) JS, JG, JR, N_CORNR, &
                           (RXED(JR, KG, KV), RXND(JR, KG, KV), RXZD(JR, KG, KV), KV = 1, N_CORNR)
           END DO
         END IF
         DEALLOCATE (RXPLT, SLENG)

       END DO
     END DO

!
!    Usually, we'd branch for Profile / temporal mode, but for multiple receivers per transmitters, there is
!    little point.  Instead, write times no matter the mode.
     WRITE (NA, 1001)
     WRITE (NA, 1012) "/TIMES(ms)=", TMS(1: NCHNL)

!
!    We are now(!) in a position to write data ...
     DO JS = 1, NTXE
       KA = 1
       DO JG = 1, NRGTX(JS)

         KG = RGTXID(JG, JS)
         KT = RX_TYPE(KG)
         JX = NRX(KG)
         KZ = (KA + JX) - 1

         IF (KT == 1) N_CORNR = 1        ! dipole ...
         IF (KT == 2) N_CORNR = 4        ! loop ...
         IF (KT == 3) N_CORNR = 2        ! bipole ...

!
!        Set up plotting points ...
         ALLOCATE (RXPLT(3, NRX(KG)))
         ALLOCATE (SLENG(NRX(KG)))
         CALL WAMX_POS(0, MRX, NRXG, MQVR, JX, KG, N_CORNR, RXED, RXND, RXZD, RXPLT, SLENG, STNS)

!
!        Loop over components ...
         DO JC = 1, NCMPG(JG)
           WRITE (NA, 1001)

!
!          Determine the component to write ...
           SELECT CASE (KT)
           CASE (1)                    ! dipole
               JC1 = PRTCMP(JC, KG)
               KC  = JC1
               IF (KC > 3) KC = KC - 3
           CASE (2)                    ! loop
               KC  = 1
               JC1 = 8
           CASE (3)                    ! bipole
               KC  = 1
               JC1 = 7
           END SELECT

!
!          Fill output arrays & calculate percentage-target effect ...
           ALLOCATE (OUT_TOTL(NCHNL, JX + 1), OUT_HOLD(NCHNL, JX + 1))

           OUT_TOTL(1: NCHNL, 1: JX) = BTD(1: NCHNL, KA: KZ, JS, KC)

           DO JR = 1, JX
             OUT_TOTL(1: NCHNL, JR) = OUT_TOTL(1: NCHNL, JR) * SXMNT(JS) * OUT_SCALE(JR, KG)
           END DO

!
!          Write data ...
           DO JR = 1, JX

               SELECT CASE (OUTPUT)
               CASE (10, 11, 12)

                   WRITE (NA, 1014, ADVANCE = "NO") (LBASE + 1000 * JS + 10 * JG), JS, JG, JR, &
                                                   SLENG(JR), RXPLT(STNS, JR), RXPLT(1, JR), RXPLT(2, JR), RXPLT(3, JR), &
                                                   OUT_COMP(JC1), OUT_RXTYP2(KT), NCHNL

                   WRITE (NA, 1015, ADVANCE = "YES") OUT_TOTL(1: NCHNL, JR)

               CASE (20, 21)

                   WRITE (NA, 1018, ADVANCE = "NO") (LBASE + 1000 * JS + 10 * JG), "_TF", &
                                                   JS, JG, JR, &
                                                   SLENG(JR), RXPLT(STNS, JR), RXPLT(1, JR), RXPLT(2, JR), RXPLT(3, JR), &
                                                   OUT_COMP(JC1), OUT_RXTYP2(KT), NCHNL

                   WRITE (NA, 1015, ADVANCE = "YES") OUT_TOTL(1: NCHNL, JR)

               END SELECT

           END DO

           DEALLOCATE (OUT_TOTL, OUT_HOLD)

         END DO

!
!        Special case of output == 21 (blocked mode, all data).  We need this because of the requirement to group
!        all data of a certain type together.  Without the special case, we get the different types printed at the
!        same station and this makes the file quite difficult to process
         IF (OUTPUT == 22) THEN
           DO JD = 1, MAX_TYP
             WRITE (NA, 1001)
             DO JC = 1, NCMPG(JG)
               WRITE (NA, 1001)
!
!              Determine the component to write ...
               SELECT CASE (KT)
               CASE (1)                    ! dipole
                   JC1 = PRTCMP(JC, KG)
                   KC  = JC1
                   IF (KC > 3) KC = KC - 3
               CASE (2)                    ! loop
                   KC  = 1
                   JC1 = 8
               CASE (3)                    ! bipole
                   KC  = 1
                   JC1 = 7
               END SELECT

!
!              Fill output arrays & calculate percentage-target effect ...
               ALLOCATE (OUT_TOTL(NCHNL, JX), OUT_HOLD(NCHNL, JX))
               OUT_TOTL(1: NCHNL, 1: JX) = SXMNT(JS) * BTD(1: NCHNL, KA: KZ, JS, KC)
               DO JR = 1, NLOC
                 OUT_TOTL(1: NCHNL, JR) = OUT_TOTL(1: NCHNL, JR) * SXMNT(1) * OUT_SCALE(JR, 1)
               END DO

               IF (JD == 1) OUT_HOLD = OUT_TOTL

               DO JR = 1, JX
                   WRITE (NA, 1018, ADVANCE = "NO") (LBASE + 1000 * JS + 10 * JG), OUT_DTYPE(JD), &
                                                   JS, JG, JR, &
                                                   SLENG(JR), RXPLT(STNS, JR), RXPLT(1, JR), RXPLT(2, JR), RXPLT(3, JR), &
                                                   OUT_COMP(JC1), OUT_RXTYP2(KT), NCHNL

                   WRITE (NA, 1015, ADVANCE = "YES")  OUT_HOLD(1: NCHNL, JR)
               END DO

               DEALLOCATE (OUT_TOTL, OUT_HOLD)

             END DO
           END DO
         END IF

         DEALLOCATE (RXPLT, SLENG)
         KA = KZ + 1

       END DO
     END DO


   CASE DEFAULT
     NLOC = NTXE
     JG = 1; KT = 1

!
!    Write transmitter parameters ....
     WRITE (NA, 1005) NLOC
     WRITE (NA, 1001)
     SELECT CASE (SOURCE_TYPE)
     CASE (3)
       DO JS = 1, NTXE
         WRITE (NA, 1006) JS, SXE(1, JS), SXN(1, JS), SXZ(1, JS), SXMNT(JS), RAD2DEG * SXDIP(JS), RAD2DEG * SXAZ(JS)
       END DO
     CASE DEFAULT
       DO JS = 1, NTXE
         WRITE (NA, 1007) JS, N_VRTX(JS), (SXE(JV, JS), SXN(JV, JS), SXZ(JV, JS), JV = 1, N_VRTX(JS))
       END DO
     END SELECT

!
!    Write receiver parameters ...
     WRITE (NA, 1001)
     IF (SURVEY_TYPE /= 4) WRITE (NA, 1008)  1, JG, OUT_RXTYP1(KT)
     IF (SURVEY_TYPE == 4) WRITE (NA, 1008)  1, JG, OUT_SURVEY(4)
     IF ((OUTPUT == 12) .OR. (OUTPUT == 22)) THEN
       WRITE (NA, 1009) 1, JG, TRIM(OUT_UNITS(JG)), TRIM(OUT_UNITS(JG)), "%"
     ELSE
       WRITE (NA, 1009) 1, JG, TRIM(OUT_UNITS(JG))
     END IF
     WRITE (NA, 1010)                        1, JG, NLOC

!
!    Write receiver parameters in header section ...
     ALLOCATE (RXPLT(3, NLOC))
     ALLOCATE (SLENG(NLOC))
     DO JR = 1, NLOC
       SELECT CASE (SURVEY_TYPE)
       CASE (4)

         WRITE (NA, 1020) 1, JG, JR, &
                         CLCD(1, JR), CLCD(2, JR), CLCD(3, JR)
                         RXPLT(1: 3, JR) = CLCD(1: 3, JR)

       CASE DEFAULT        ! 2 OR 3

         WRITE (NA, 1011) 1, JG, JR, &
                         RXED(1, JR, 1), RXND(1, JR, 1), RXZD(1, JR, 1), &
                         RXMNT(JR, 1), RXDIP(JR, 1), RXAZ(JR, 1)

       END SELECT
     END DO

!
!    Determine Rx plotting points ...
     IF (SURVEY_TYPE /= 4) CALL WAMX_POS(1, MRX, NRXG, MQVR, NLOC, JG, N_CORNR, RXED, RXND, RXZD, RXPLT, SLENG, STNS)
     IF (SURVEY_TYPE == 4) CALL WAMX_CLDIST(NLOC, RXPLT, SLENG, STNS)

     WRITE (NA, 1001)
     WRITE (NA, 1012) "/TIMES(ms)=", TMS(1: NCHNL)

!
!    Completed writing header section; start on data ...
     DO JC = 1, NCMPG(1)
       WRITE (NA, 1001)
       IF ((RX_TYPE(1) > 1) .OR. (SURVEY_TYPE == 4)) THEN
         KC = 1
         JC1 = 7
       ELSE
         JC1 = PRTCMP(JC, 1)
         KC = JC1
         IF (KC > 3) KC = KC - 3
       END IF

!
!      Fill output arrays & calculate percentage-target effect ...
       ALLOCATE (OUT_TOTL(NCHNL, NLOC), OUT_HOLD(NCHNL, NLOC))

       OUT_TOTL(1: NCHNL, 1: NLOC) =  BTD(1: NCHNL, 1: NLOC, 1, KC)

       DO JR = 1, NLOC
         OUT_TOTL(1: NCHNL, JR) = OUT_TOTL(1: NCHNL, JR) * SXMNT(1) * OUT_SCALE(JR, 1)
       END DO

       DO JS = 1, NLOC

           SELECT CASE (OUTPUT)
           CASE (10, 11, 12)

               WRITE (NA, 1014, ADVANCE = "NO") LBASE, 1, 1, JS, &
                                               SLENG(JS), RXPLT(STNS, JS), RXPLT(1, JS), RXPLT(2, JS), RXPLT(3, JS), &
                                               OUT_COMP(JC1), OUT_RXTYP2(1), NCHNL

               WRITE (NA, 1015, ADVANCE = "YES") OUT_TOTL(1: NCHNL, JS)

           CASE (20, 21)

               WRITE (NA, 1018, ADVANCE = "NO") (LBASE + 10 * JG), "_TF", 1, 1, JS, &
                                               SLENG(JS), RXPLT(STNS, JS), RXPLT(1, JS), RXPLT(2, JS), RXPLT(3, JS), &
                                               OUT_COMP(JC1), OUT_RXTYP2(1), NCHNL

               WRITE (NA, 1015, ADVANCE = "YES") OUT_TOTL(1: NCHNL, JS)

           END SELECT

       END DO

       DEALLOCATE (OUT_TOTL, OUT_HOLD)

     END DO

!
!    Special case of output == 21 (blocked mode, all data).  We need this because of the requirement to group
!    all data of a certain type together.  Without the special case, we get the different types printed at the
!    same station and this makes the file quite difficult to process
     IF (OUTPUT == 22) THEN
       DO JD = 1, MAX_TYP
         WRITE (NA, 1001)
         DO JC = 1, NCMPG(1)
           WRITE (NA, 1001)
           IF ((RX_TYPE(1) > 1) .OR. (SURVEY_TYPE == 4)) THEN
             KC = 1
             JC1 = 7
           ELSE
             JC1 = PRTCMP(JC, 1)
             KC = JC1
             IF (KC > 3) KC = KC - 3
           END IF

!
!          Fill output arrays & calculate percentage-target effect ...
           ALLOCATE (OUT_TOTL(NCHNL, NLOC), OUT_HOLD(NCHNL, NLOC))

           OUT_TOTL(1: NCHNL, 1: NLOC) =  BTD(1: NCHNL, 1: NLOC, 1, KC)

           DO JR = 1, NLOC
             OUT_TOTL(1: NCHNL, JR) = OUT_TOTL(1: NCHNL, JR) * SXMNT(1) * OUT_SCALE(JR, 1)
           END DO

           IF (JD == 1) OUT_HOLD = OUT_TOTL

           DO JS = 1, NLOC
               WRITE (NA, 1018, ADVANCE = "NO") (LBASE + 10 * JG), OUT_DTYPE(JD), 1, 1, JS, &
                                               SLENG(JS), RXPLT(STNS, JS), RXPLT(1, JS), RXPLT(2, JS), RXPLT(3, JS), &
                                               OUT_COMP(JC1), OUT_RXTYP2(1), NCHNL

               WRITE (NA, 1015, ADVANCE = "YES") OUT_HOLD(1: NCHNL, JS)

           END DO

           DEALLOCATE (OUT_TOTL, OUT_HOLD)

         END DO

       END DO
     END IF

     DEALLOCATE (RXPLT, SLENG)

   END SELECT

   CLOSE (NA)

1000 FORMAT (a)
1001 FORMAT ("/")
1002 FORMAT ("AMX_VERSION=", i2.2, ".", i2.2, 2x, "PROGRAM_NAME:", a)
1003 FORMAT ("/", /, "/FILE_TYPE=", i2.2, /, "/FIELDS_FORMAT=", i2.2, /, "/")
1004 FORMAT ("/", /, "/SURVEY_TYPE=", a, /, "/SOURCE_TYPE=", a, /, "/")
1005 FORMAT ("/TX_TOTAL=", i3.3)
1006 FORMAT ("/TX", i3.3, "_COORDS(N,E,Z,M,Dip,Az)=", 6(f13.2, :, ", ", 2x))
1007 FORMAT ("/TX", i3.3, "_COORDS(NVRTxN,E,Z)=", i2.2, ", ", 256(f13.2, ", ", f13.2, ", ", f13.2, :, ", "))
1008 FORMAT ("/TX", i3.3, "_RXGroup", i3.3, "_TYPE=", a)
1009 FORMAT ("/TX", i3.3, "_RXGroup", i3.3, "_UNITS=", 3(a, :, ","))
1010 FORMAT ("/TX", i3.3, "_RXGroup", i3.3, "_NUMBER=", i3.3)
1011 FORMAT ("/TX", i3.3, "_RXGroup", i3.3, "_RXN", i3.3, "_PARAMS(N,E,Z,M,Dip,Az)=", 6(f13.2, :, ", "))
1012 FORMAT (a, 2x, 512(f13.2, :, ", "))
1014 FORMAT (2x, i6, 3(2x, i4), 5(2x, f13.2), 2x, a2, 2x, a3, 2x, i4)
1015 FORMAT (512(:, 2x, e15.6))
1016 FORMAT ("/TX", i3.3, "_RXGroup", i3.3, "_RXN", i3.3, "_PARAMS(nv,Nv,Ev,Zv)=", &
             i2.2, ", ", 4(f13.2, ", ", f13.2, ", ", f13.2, :, ", "))
1018 FORMAT (2x, i6, a3, 2x, 3(2x, i4), 5(2x, f13.2), 2x, a2, 2x, a3, 2x, i4)
1020 FORMAT ("/TX", i3.3, "_RXGroup", i3.3, "_RXN", i3.3, "_PARAMS(N,E,Z)=", 3(f13.2, :, ", "))

 END SUBROUTINE WRITE_TAMX
!--------------

 SUBROUTINE WAMX_CLDIST(NLOC, RXPLT, SLENG, STNS)
!------------------------------------------------

!  Sub. designed to calculate dominate distance for CL surveys ...

!  Calls:      none
!  Called by:  wrt_tamx

!  Input
!      nloc    # survey locations
!      rxplt   survey coordinates

!  Output
!      stns    dimension of most rapidly varying index in rxplt

!  InOut
!      sleng   distance along survey

   IMPLICIT NONE

   INTEGER, PARAMETER :: MAX_COMP = 3
   INTEGER, PARAMETER :: DP = SELECTED_REAL_KIND(12, 80)

   INTEGER, INTENT (IN) :: NLOC
   INTEGER, INTENT (OUT) :: STNS
   INTEGER :: JR

   REAL (KIND = DP), INTENT (IN) :: RXPLT(MAX_COMP, NLOC)
   REAL (KIND = DP)              :: DIST(MAX_COMP)

   REAL (KIND = DP), INTENT (INOUT) :: SLENG(NLOC)
   REAL (KIND = DP) :: DS


   DIST = MAXVAL(TRANSPOSE(RXPLT), DIM = 1) - MINVAL( TRANSPOSE(RXPLT), DIM = 1)
   IF ((DIST(1) > DIST(2)) .AND. (DIST(1) > DIST(3))) THEN
     STNS = 1                                                    ! northings
   ELSE IF ((DIST(3) > DIST(1)) .AND. (DIST(3) > DIST(2))) THEN
     STNS = 3                                                    ! vertical
   ELSE
     STNS = 2                                                    ! easting by default
   END IF

!
!  Calculate distance along traverse
   SLENG = 0.
   DO JR = 2, NLOC
     DS = (RXPLT(1, JR) - RXPLT(1, JR - 1))**2 + &
          (RXPLT(2, JR) - RXPLT(2, JR - 1))**2 + &
          (RXPLT(3, JR) - RXPLT(3, JR - 1))**2
     SLENG(JR) = SQRT(DS) + SLENG(JR - 1)
   END DO

   RETURN

 END SUBROUTINE WAMX_CLDIST

!--------------

 SUBROUTINE WAMX_POS(MODE, MRX, NRXG, MQVR, JX, JG, N_CORNR, RXED, RXND, RXZD, RXPLT, SLENG, STNS)
!-------------------------------------------------------------------------------------------------

!  Sub. designed to encapsulate details of calculating plotting position for general loops & di(bi)poles

!  Called by: WRT_FAMX, WRT_TAMX
!  Calls    : None

!  Input
!      mode                0 => general loops; 1 => in loop
!      mrx                 max. # rx
!      nrxg                # rx groups
!      mqvr                max. # verticies
!      jx                  # rx in current group
!      jg                  # of current group
!      n_cormr             # corners
!      rxed, rxnd, rxzd    East, North & vertical rx coordinates

!  Output
!      stns    index to use as stns

!  InOut
!      rxplt(In)   allocated array (initialised in THIS routine)
!          (out)   simplified coords for plotting
!      sleng(In)   initialised
!          (out)   length along survey (esp. for DDH work)

   IMPLICIT NONE

   INTEGER, PARAMETER :: MAX_COMP = 3
   INTEGER, PARAMETER :: DP = SELECTED_REAL_KIND(12, 80)

   INTEGER, INTENT (IN)  :: MODE
   INTEGER, INTENT (IN)  :: MRX, NRXG, MQVR
   INTEGER, INTENT (IN)  :: JG, JX, N_CORNR
   INTEGER, INTENT (OUT) :: STNS

   INTEGER :: JR

   REAL (KIND = DP), INTENT (IN)    :: RXED(MRX, NRXG, MQVR)
   REAL (KIND = DP), INTENT (IN)    :: RXND(MRX, NRXG, MQVR)
   REAL (KIND = DP), INTENT (IN)    :: RXZD(MRX, NRXG, MQVR)

   REAL (KIND = DP), INTENT (INOUT) :: RXPLT(MAX_COMP, JX)
   REAL (KIND = DP), INTENT (INOUT) :: SLENG(JX)

   REAL (KIND = DP) :: DIST(MAX_COMP)
   REAL (KIND = DP) :: DS

   RXPLT = 0.

   SELECT CASE (MODE)
   CASE (0)
     DO JR = 1, JX
       RXPLT(1, JR) = SUM(RXND(JR, JG, 1: N_CORNR)) / REAL(N_CORNR, DP)
       RXPLT(2, JR) = SUM(RXED(JR, JG, 1: N_CORNR)) / REAL(N_CORNR, DP)
       RXPLT(3, JR) = SUM(RXZD(JR, JG, 1: N_CORNR)) / REAL(N_CORNR, DP)
     END DO
   CASE (1)
     DO JR = 1, JX
       RXPLT(1, JR) = RXND(1, JR, 1)
       RXPLT(2, JR) = RXED(1, JR, 1)
       RXPLT(3, JR) = RXZD(1, JR, 1)
     END DO
   END SELECT

!
!  Select index to plot as station ...
   DIST = MAXVAL (TRANSPOSE (RXPLT), DIM = 1) - MINVAL (TRANSPOSE (RXPLT), DIM = 1)
   IF ((DIST(1) > DIST(2)) .AND. (DIST(1) > DIST(3))) THEN
     STNS = 1                                                       ! northings
   ELSE IF ((DIST(3) > DIST(1)) .AND. (DIST(3) > DIST(2))) THEN
     STNS = 3                                                       ! vertical
   ELSE
     STNS = 2                                                       ! easting by default
   END IF

!
!  Calculate distance along traverse
   SLENG = 0.
   DO JR = 2, JX
     DS = (RXPLT(1, JR) - RXPLT(1, JR - 1))**2 + &
          (RXPLT(2, JR) - RXPLT(2, JR - 1))**2 + &
          (RXPLT(3, JR) - RXPLT(3, JR - 1))**2
     SLENG(JR) = SQRT(DS) + SLENG(JR - 1)
   END DO

   RETURN

 END SUBROUTINE WAMX_POS
!--------------

 SUBROUTINE WTAMX_HEADER(NA, OUTPUT, NOUT)
!--------------------------------------------

!  Sub. writes AMX file column headers for TEM files

!   Calls:      none
!   Called by:  wrt_famx

!  Input
!    na       output unit
!    output   10 => total only (flat)
!             11 => total + scattered + % (flat)
!             20 => total only (blocked)
!             21 => total + scattered + % (blocked)
!    nout     # of data to write

!  Output
!       in file connected to unit na
!
!  Notes
!  1.   general nature of subroutine means that nout may be either nfrq or nstat
!       depending upon the setting of prfl.
!  2.   Data type indicators are appended (or not) depending upon detting of output
!       10, 11 => Append _TF, _TF, _SF, _PT
!       20, 21 => Do not append anything (blocked mode)

   IMPLICIT NONE

   INTEGER, INTENT (IN) :: NA, OUTPUT, NOUT
   INTEGER :: JO

   WRITE (NA, 1000, ADVANCE = "NO")
   WRITE (NA, 1002, ADVANCE = "NO")
   WRITE (NA, 1003, ADVANCE = "NO")
   WRITE (NA, 1005, ADVANCE = "NO") "NChnl"

   SELECT CASE (OUTPUT)
   CASE (10, 11)

     DO JO = 1, NOUT - 1
       WRITE (NA, 1004, ADVANCE = "NO")  "Chn", JO, "_TF"
     END DO
     WRITE (NA, 1004, ADVANCE = "YES")   "Chn", NOUT, "_TF"

   CASE (12)

     DO JO = 1, NOUT
       WRITE (NA, 1004, ADVANCE = "NO")  "Chn", JO, "_TF"
     END DO
     DO JO = 1, NOUT
       WRITE (NA, 1004, ADVANCE = "NO")  "Chn", JO, "_SF"
     END DO
     DO JO = 1, NOUT - 1
       WRITE (NA, 1004, ADVANCE = "NO")  "Chn", JO, "_PT"
     END DO
     WRITE (NA, 1004, ADVANCE = "YES")   "Chn", NOUT, "_PT"

   CASE (20, 21)

     DO JO = 1, NOUT - 1
       WRITE (NA, 1004, ADVANCE = "NO")  "Chn", JO, "   "
     END DO
     WRITE (NA, 1004, ADVANCE = "YES")   "Chn", NOUT, "   "

   CASE (22)

     DO JO = 1, NOUT - 1
       WRITE (NA, 1004, ADVANCE = "NO")  "Chn", JO, "   "
     END DO
     WRITE (NA, 1004, ADVANCE = "YES")   "Chn", NOUT, "   "

   END SELECT

   RETURN

1000 FORMAT ("Line", 2x, "TxGrp", 2x, "RxGrp", 2x, "Fid", 2x)
1002 FORMAT ("Dist", 2x, "Stn", 2x, "North", 2x, "East", 2x, "RxElev", 2x)
1003 FORMAT ("CMP", 2x, "RxType", 2x)
1004 FORMAT (2x, a, i3.3, a)
1005 FORMAT (a5, 2x)

 END SUBROUTINE WTAMX_HEADER
!--------------

 SUBROUTINE SET_OUTPUT_SCALING (NRXG, MRX, SURVEY_TYPE, RX_TYPE, RXMNT, STEP, UNITS, OUT_SCALE, OUT_UNITS)
!---------------------------------------------------------------------------------------------------------

!  Sub. is a neatened version of set_output_factors.

!  Calls:       none
!  Called by:   wrt_famx
!              wrt_tamx

!  Input
!      nrxg         # receiver groups
!      mrx              max # receivers
!      survey_type      cf. entry notes
!      rx_type      cf. entry notes
!      rxmnt        receiver moments
!      step         step / otherwise
!      units        cf. entry notes

!  Output
!      out_scale   total scalling to be applied to output data
!      out_units   textual SI units for outut

!  Notes
!  1.  sub written to minimise changes in existing code; since AMIRA format is no longer
!      used, the orginial set_output_factors could easily be replaced with this version

   IMPLICIT NONE

   INTEGER, INTENT (IN) :: NRXG, MRX, SURVEY_TYPE, STEP
   INTEGER, INTENT (IN) :: RX_TYPE(NRXG), UNITS(NRXG)
   INTEGER :: JG

   REAL, INTENT (IN)  :: RXMNT(MRX, NRXG)
   REAL, INTENT (OUT) :: OUT_SCALE(MRX, NRXG)

   CHARACTER (LEN = 04), INTENT (OUT) :: OUT_UNITS(NRXG)

   DO JG = 1, NRXG

     IF ((SURVEY_TYPE == 4) .OR. (RX_TYPE(JG) > 1)) THEN

       SELECT CASE (UNITS(JG))
       CASE (1)
         OUT_SCALE(1: MRX, JG) = 1.
         OUT_UNITS(JG) = "V   "
       CASE (2)
         OUT_SCALE(1: MRX, JG) = 1.E3
         OUT_UNITS(JG) = "mV  "
       CASE (3)
         OUT_SCALE(1: MRX, JG) = 1.E6
         OUT_UNITS(JG) = "uV  "
       CASE (4)
         OUT_SCALE(1: MRX, JG) = 1.E9
         OUT_UNITS(JG) = "nV  "
       END SELECT

     ELSE IF ((STEP == 0) .AND. (RX_TYPE(JG) == 1)) THEN

       SELECT CASE (UNITS(JG))
       CASE (1)
         OUT_SCALE(1: MRX, JG) = 1.E9
         OUT_UNITS(JG) = "nT/s"
       CASE (2)
         OUT_SCALE(1: MRX, JG) = 1.E12
         OUT_UNITS(JG) = "pT/s"
       CASE (3)
         OUT_SCALE(1: MRX, JG) = 1.E6 * RXMNT(1: MRX, JG)
         OUT_UNITS(JG) = "uV  "
       CASE (4)
         OUT_SCALE(1: MRX, JG) = 1.E9 * RXMNT(1: MRX, JG)
         OUT_UNITS(JG) = "nV  "

       END SELECT
     ELSE IF ((STEP == 1) .AND. (RX_TYPE(JG) == 1)) THEN

       SELECT CASE (UNITS(JG))
       CASE (1)
         OUT_SCALE(1: MRX, JG) = 1.E9
         OUT_UNITS(JG) = "nT  "
       CASE (2)
         OUT_SCALE(1: MRX, JG) = 1.E12
         OUT_UNITS(JG) = "pT  "
       CASE (3)
         OUT_SCALE(1: MRX, JG) = 1.E15
         OUT_UNITS(JG) = "fT  "
       END SELECT

     END IF

   END DO

   RETURN

 END SUBROUTINE SET_OUTPUT_SCALING

!=================================================
!  End of extended AMIRA format routines: Arjuna
!=================================================

 SUBROUTINE SET_SOURCE (NSX,SWX,SWY,T0SX)
!----------------------------------------

! For time-domain, SET_SOURCE computes dI/dt at the transmitter using
!
! SWY will be in amps / sec * Tx area * NTRN
!  Computes SWY to be dI(t)/dt
!  The units of SWY are amps / s

!*** Called by MAIN

!             INPUT
!             -----
!
!          NSX - number of source points in waveform
!          SWX - time abscissae for input waveform in seconds
!     SWY(*,3) - waveform in amps
!   SWY(*,1:2) = 0
!
!             OUTPUT
!             ------
!
!     SWY(*,2) = delta I(t)  amps
!     SWY(*,1) = dI(t)/dt  amps/s
!         T0SX = latest time at which dI/dt /= 0

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
END SUBROUTINE SET_SOURCE

 SUBROUTINE TDEM_3D (STEP,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL,TOPN, &
                     TCLS,FREQ,NFRQ,KFRQE,NTXE,LRX,NRXTX,RXID,NCMP,BFD1,BTD1)
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
!        STEP = 1 iff step response is to be computed
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
!        NTXE - number of transmitter positions.
!         LRX - maximum number of receiver positions for any transmitter position
!    NRXTX(J) = number of receivers FOR transmitter J
!   RXID(I,J) = RX_TYPE of receiver I for transmitter J
!   NCMP(I,J) = number of components for receiver I for transmitter J
!        NCMP = number of components, = 3 or 1 usually
!   BFD1(I,J,K,L) - Lth component of the complex frequency-domain impulse
!                   response at receiver J, corresponding to transmitter K
!                   for frequency I.  Usually L = 1,2,3 => North, East, & vertical
!                   components respectively.  If NCMP = 1 as is the case for loop,
!                   electric dipole of coupled magnetic dipoles then the relevant
!                   component is stored in L=1 position

 IMPLICIT NONE
 REAL, PARAMETER :: TWOPI=6.283185
 INTEGER STEP,NSX,NPULS,NTYPLS,NTYRP,NCHNL,NFRQ,KFRQE,KFRQ,NTXE,LRX,NRXTX(NTXE), &
         NCMP(LRX,NTXE),STEPC,RXID(LRX,NTXE),JR,JS,JF,JC,JT,KC
 REAL, DIMENSION(:,:), ALLOCATABLE ::  YSCAT,YFRQ
 REAL PULSE,FREQ(NFRQ),WF(NFRQ),SWX(NSX),SWY(NSX,3), &
      COSTRN,T,YCUM(NCHNL),TOPN(NCHNL),TCLS(NCHNL),TRP(NTYRP), &
      BTD1(NCHNL,LRX,NTXE,3)
 COMPLEX BFD1(NFRQ,LRX,NTXE,3)

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

 DO JS = 1,NTXE
   DO JR = 1, NRXTX(JS)
     KC = NCMP(JR,JS)
     STEPC = STEP
     KFRQ = NFRQ
     IF (RXID(JR,JS) == 2) STEPC = 1  ! Transform INTEGRAL { E dl } to TD
     IF (RXID(JR,JS) == 3) THEN
       STEPC = 3                      ! Electrode E field Transform INTEGRAL { E dl } to TD
       KFRQ = KFRQE
     END IF
     DO JC = 1,KC
       DO JF = 1,NFRQ     ! Divide by -iw to set up step response
         IF (STEPC == 3) THEN
           YFRQ(1,JF) = REAL ( BFD1(JF,JR,JS,JC) )
         ELSE
           YFRQ(1,JF) = -AIMAG ( BFD1(JF,JR,JS,JC) ) / (TWOPI * FREQ(JF))
         END IF
       END DO
       CALL CUBSPL (WF,YFRQ,NFRQ,0,0)

       YSCAT = 0.
       DO JT = 1, NTYRP   !  Convert to step-function time-domain.
         T = TRP(JT)
         YSCAT(1,JT) = COSTRN (WF,YFRQ,NFRQ,KFRQ,T)
       END DO

       CALL FOLD_AND_CONVOLVE (STEPC,NSX,SWX,SWY,NPULS,PULSE,TRP,NTYRP,NTYPLS, &
                               NCHNL,TOPN,TCLS,YSCAT,YCUM)

       BTD1(1:NCHNL,JR,JS,JC) = YCUM(1:NCHNL)
     END DO
   END DO
 END DO
 DEALLOCATE (YSCAT, YFRQ)

END SUBROUTINE TDEM_3D

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

 CALL CUBSPL (TRP,YPLS,NTYRP,0,0)
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
 CALL CUBSPL (TRP,YPLS,NTYPLS,0,0)
 YCUM = 0.

 MXCNV = NTYPLS + NSX
 DO JT = 1, NCHNL
   T1 = TOPN(JT)
   IF (T1 < TRP(1)) CYCLE
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
       IF (STEPC == 1) THEN                                    ! Magnetic dipole or loop receiver
         YC1 = TXCNVL (T1,NTYPLS,TRP,YPLS,NSX,SWX,SWY)
       ELSE IF (STEPC == 3) THEN                                ! Electrode receiver
         YC1 = TXCNVD (MXCNV,T1,NTYPLS,TRP,YPLS,NSX,SWX,SWY,3)  ! Current is in SWY(*,3)
       END IF
       YCUM(JT) = YCUM(JT) + (WT * YC1)
     END DO
   END IF
 END DO

END SUBROUTINE FOLD_AND_CONVOLVE

 SUBROUTINE LP_VERTEX_ORDER (NTXE,MXVRTX,N_VRTX,SXN,SXE,SXZ)
!----------------------------------------------------------

! Assumes that the vertices have been entered in order, either clockwise or
! counter-clockwise.  If counter-clockwise, it reorders them.

!          NTXE - number of loop transmitters
!        MXVRTX - maximum number of vertices for any loop
!     N_VRTX(J) - number of vertices for transmitter J
!      SXE(I,J) = local east coordinate of vertex I for loop position J
!      SXN(I,J) = local coordinate of vertex I for loop position J
!      SXZ(I,J) = local (z positive down) depth of vertex I for loop position J

 IMPLICIT NONE
 INTEGER NTXE,MXVRTX,N_VRTX(NTXE),NV,KS,KS1,JV,J1,JS
 REAL, DIMENSION (MXVRTX,NTXE) :: SXN,SXE,SXZ
! REAL, DIMENSION (NTXE) :: TEMPN,TEMPE,TEMPZ,XN,YE
 REAL, DIMENSION (NTXE) :: TEMPN,TEMPE,TEMPZ
 REAL, DIMENSION (MXVRTX) :: XN,YE
 REAL R,RMAX,XN_CNTR,YE_CNTR,CTH,STH,EMID

 DO JS = 1,NTXE
   NV = N_VRTX(JS)
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
     TEMPZ(1:NV) = SXZ(1:NV,JS)
     DO JV = 1,NV
       SXN(JV,JS) = TEMPN(NV+1-JV)
       SXE(JV,JS) = TEMPE(NV+1-JV)
       SXZ(JV,JS) = TEMPZ(NV+1-JV)
     END DO
   END IF
 END DO

 END SUBROUTINE LP_VERTEX_ORDER

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
!            NSX - number of points in SWX & WAVEFORM

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

 REAL FUNCTION TXCNVD (MXCNV,T,NTYPLS,TRP,YPLS,NSX,SWX,SWY,K1)
!-------------------------------------------------------------
!
!***  Called by FOLD_AND_CONVOLVE
!***  Calls CUBINT, CUBSPL, CUBVAL, LINVAL, TXCMRG

!  Convolves earth response function (ERF) with the source waveform
!  contained in SWY(*,K1) at NSX points to produce the system response
!  of the earth.  In Arjuna, it is used for electric field computations.
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

!  Return TXCNVD = 0 if N1 + N2 < 4 or if NCNV < 4

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

 CALL CUBSPL (XCNV,YCNV,NCNV,0,0)
 TXCNVD = CUBINT (XCNV,YCNV,NCNV,T0,T)

END FUNCTION TXCNVD

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

 SUBROUTINE WRITE_TD (NW,PRFL,STEP,NCHNL,TMS,SURVEY_TYPE,SOURCE_TYPE,NTXE,SXMNT,NRXG,  &
                      NRGTX,RGTXID,NRX,LRX,MRX,UNITS,RX_TYPE,NCMPG,PRTCMP,RXMNT,RXED,  &
                      RXND,RXZD,MQVR,CLCD,TITLE,BTD)
!--------------------------------------------------------------------------------------

!  Prints out time domain output
!
!*** Called by: MAIN
!***     Calls: SET_OUTPUT_FACTORS, WRTDP
!
!              NW - output unit number
!            PRFL = 1 for profile output; = 0 for temporal output
!            STEP = 0 for dB/dt output and 1 for B output
!                 = 0 for grounded wire and coincident loop receivers
!             TMS - time midpoints (ms) for NCHNL windows
!     SURVEY_TYPE - indicates seperate Tx-Rx arrays (1), fixed offset (2),
!                   central loop (3), coincident loop (4)
!     SOURCE_TYPE - indicates loop (1), grounded wire (2), magnetic dipole(3) or
!                   plane wave sources (4)
!            NTXE - number of transmitter positions
!        SXMNT(J) - turns * area for magnetic dipole transmitter J
!            NRXG - number of receiver groups
!        NRGTX(J) - number of receiver groups for transmitter J
!     RGTXID(I,J) - receiver group index for Ith group for transmitter J
!          NRX(J) - the number of receivers in receiver group J.  MRX = MAXVAL (NRX)
!             LRX - maximum number of receivers assigned to any transmitter position
!             MRX - maximum number of receivers in any receiver group
!           UNITS - output unit indicator for each receiver group
!         RX_TYPE - receiver type: mag dipole (1); finite loop (2); electric dipole (3)
!      RXMNT(I,J) - receiver moment for receiver I of group J if it is a mag dipole
!     RXED(I,J,K) - "real world" east coordinate of vertex K for receiver I of Rx Group J
!      RXND, RXZD - "real world" north and vertical locations of these vertices.
!            MQVR - Maximum number of vertices for all receiver groups (1, 2 or 4)
!       CLCD(3,J) - North (1), East (2), Vertical (3) coincident loop centre of Tx position J.
!
!    BTD(I,J,K,L) - the Lth component measured response at channel I from receiver J,
!                   transmitter K, component L

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,76)
 INTEGER NW,PRFL,STEP,NCHNL,SURVEY_TYPE,SOURCE_TYPE,NTXE,NRXG,NRGTX(NTXE),RGTXID(NRXG,NTXE), &
         RX_TYPE(NRXG),NCMPG(NRXG),PRTCMP(10,NRXG),UNITS(NRXG),NRX(NRXG),LRX,MRX,MQVR,KRG,   &
         KRGT,NRCNR,NRX1,JR,JRP,NLOC,JC,JC1,JRG,JS,KRFIN,PRTYP,KC
 REAL TMS(NCHNL),OUTFAC(MRX,NRXG),RXMNT(MRX,NRXG),FAC,SXMNT(NTXE)
 REAL, ALLOCATABLE, DIMENSION(:,:) :: YTR
 REAL, DIMENSION(NCHNL,LRX,NTXE,3) :: BTD
 REAL(KIND=QL), DIMENSION (MRX,NRXG,MQVR) :: RXED,RXND,RXZD
 REAL(KIND=QL) CLCD(3,NTXE)
 REAL(KIND=QL), ALLOCATABLE :: RXPLT(:,:)
 CHARACTER(LEN=120) TITLE
 CHARACTER(LEN=16) OUTTXT(NRXG),SXTXT(3)
 CHARACTER(LEN=17) RXTXT(3)

 DATA SXTXT /'loop.           ','grounded wire.  ','magnetic dipole.'/
 DATA RXTXT /'magnetic dipoles.','loops.           ','grounded wires.  '/

 WRITE(NW,'(/T3,A)') TITLE
 CALL SET_OUTPUT_FACTORS (NRXG,MRX,SURVEY_TYPE,RX_TYPE,RXMNT,STEP,UNITS, &
                          OUTTXT,OUTFAC)

 IF (SURVEY_TYPE == 1) THEN
   NLOC = MRX
   ALLOCATE (RXPLT(3,NLOC),YTR(NCHNL,NLOC))
   DO JS = 1, NTXE
     KRFIN = 0
     DO JRG = 1, NRGTX(JS)
       KRG = RGTXID(JRG,JS)  ! Receiver Group index for group JRG for Tx JS
       KRGT = RX_TYPE(KRG)
       WRITE(NW,1) JS, KRG, SXTXT(SOURCE_TYPE), RXTXT(KRGT),OUTTXT(KRG)

       NRX1 = NRX(KRG)      ! Number of receivers in receiver group KRG
       NRCNR = 1            ! set receiver midpoints
       IF (RX_TYPE(KRG) == 2) NRCNR = 4
       IF (RX_TYPE(KRG) == 3) NRCNR = 2
       DO JR = 1,NRX1
         RXPLT(1,JR) = SUM (RXND(JR,KRG,1:NRCNR) ) / REAL (NRCNR,8)
         RXPLT(2,JR) = SUM (RXED(JR,KRG,1:NRCNR) ) / REAL (NRCNR,8)
         RXPLT(3,JR) = SUM (RXZD(JR,KRG,1:NRCNR) ) / REAL (NRCNR,8)
       END DO
       DO JC = 1, NCMPG(KRG)
         IF (KRGT > 1) THEN         ! Loop or wire receiver (single component)
           JC1 = 7                  ! No label on output component
           KC = 1                   ! Output is in first component position
         ELSE
           JC1 = PRTCMP(JC,KRG)     !  Sum is now over specified components
           KC = JC1
           IF (JC1 > 3 ) KC = JC1-3 !  Assign borehole component
         END IF

         YTR = 0
         DO JR = 1,NRX1
           FAC = SXMNT(JS) * OUTFAC(JR,KRG)
           JRP = JR + KRFIN

           YTR(1:NCHNL,JR) = FAC * BTD(1:NCHNL,JRP,JS,KC)
         END DO
         PRTYP = 1
         CALL WRTDP (NW,PRFL,PRTYP,NCHNL,TMS,NLOC,NRX1,JC1,RXPLT,YTR)
       END DO
       KRFIN = KRFIN + NRX1  ! Set index for next group
     END DO                 ! End of Rx groups for transmitter JS
   END DO

 ELSE
   NLOC = NTXE
   ALLOCATE (RXPLT(3,NLOC),YTR(NCHNL,NLOC))
   IF (SURVEY_TYPE == 2) WRITE(NW,2) SXTXT(SOURCE_TYPE), OUTTXT(1)
   IF (SURVEY_TYPE == 3) WRITE(NW,3) OUTTXT(1)
   IF (SURVEY_TYPE == 4) WRITE(NW,4) OUTTXT(1)

   DO JS = 1,NTXE
     IF (SURVEY_TYPE == 2 .OR. SURVEY_TYPE == 3) THEN
       RXPLT(1,JS) = RXND(1,JS,1)
       RXPLT(2,JS) = RXED(1,JS,1)
       RXPLT(3,JS) = RXZD(1,JS,1)
     ELSE IF (SURVEY_TYPE == 4) THEN
       RXPLT(1:3,JS) = CLCD(1:3,JS)
     END IF
   END DO

   DO JC = 1, NCMPG(1)

     IF (RX_TYPE(1) > 1 .OR. SURVEY_TYPE == 4) THEN
       JC1 = 7
       KC = 1
     ELSE
       JC1 = PRTCMP(JC,1)     !  Sum is now over specified components
       KC = JC1
       IF (JC1 > 3 ) KC = JC1-3    !  Assign borehole component  ^^^^^^^^^^^^^6
     END IF

     DO JS = 1,NTXE
       FAC = OUTFAC(1,JS) * SXMNT(JS)
       YTR(1:NCHNL,JS) = FAC * BTD(1:NCHNL,1,JS,KC)
     END DO
     PRTYP = 1
     CALL WRTDP (NW,PRFL,PRTYP,NCHNL,TMS,NLOC,NTXE,JC1,RXPLT,YTR)
   END DO

 END IF

 1 FORMAT(//T3,'Time-domain output for Transmitter Position',I3,' & Receiver Group',I3 &
           /T3,'------------------------------------------------------------------'   &
           /T3,'The source is a ',A                                                  &
           /T3,'The receivers in this group are ',A                                  &
           /T3,'For each receiver position, the output is printed in ',A/)
 2 FORMAT(//T3,'Time-domain output for a single magnetic dipole receiver moving with' &
           /T3,'constant offset with respect to the transmitter.'                    &
           /T3,'The source is a ',A                                                  &
           /T3,'For each receiver position, the output is printed in ',A/)
 3 FORMAT(//T3,'Time-domain output for a central loop survey.' &
           /T3,'For each loop centre position, the output is printed in ',A/)
 4 FORMAT(//T3,'Time-domain output for a coincident loop survey.' &
           /T3,'For each loop centre position, the output is printed in ',A/)

 END SUBROUTINE WRITE_TD

 SUBROUTINE WRTDP (NW,PRFL,PRTYP,NCHNL,TMS,NLOC,NRX1,JC,RXPLT,YTR)
!-----------------------------------------------------------------

!***  Called by: WRITE_TD
!***      CallS: nil

!  Writes time-domain output in profile form.

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,76)
 INTEGER NW,PRFL,PRTYP,NCHNL,NLOC,NRX1,JC,JR,NBLKS,J1,JB,JF,NCOL
 REAL TMS(NCHNL),YTR(NCHNL,NLOC)
 REAL(KIND=QL) RXPLT(3,NLOC)

 CHARACTER(LEN=8) CHN(150)
 CHARACTER(LEN=23) DIRECTION(6)
 CHARACTER(LEN=50) RESPONSE(3)
 DATA DIRECTION /'NORTH COMPONENT        ','EAST COMPONENT         ','VERTICAL COMPONENT     ', &
                 'BH HORIZONTAL COMPONENT','BH SLOPE COMPONENT     ','BH AXIAL COMPONENT     '/
 DATA RESPONSE /'STANDARD OUTPUT - LAYERED EARTH + SCATTERED FIELDS', &
                'SCATTERED FIELD - INDUCED PLUS CURRENT GATHERING  ', &
                'PERCENT TARGET EFFECT                             '/
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
   CALL WRTDT (NW,PRTYP,NCHNL,TMS,NLOC,NRX1,JC,RXPLT,YTR)
   RETURN
 END IF

 IF (JC < 7) THEN
   WRITE(NW,'(/T7,2A)') 'PROFILE OUTPUT:  ',DIRECTION(JC)
 ELSE
   WRITE(NW,'(/T7,2A)') 'PROFILE OUTPUT:  '
 END IF
 WRITE(NW,'(T7,A)') RESPONSE(PRTYP)

 NCOL = MIN (NCHNL, 35)
 NBLKS = 1
 IF (NCHNL > 35) THEN
   NBLKS = NCHNL / NCOL
   IF (MOD (NCHNL,NCOL) > 0) NBLKS = NBLKS + 1
 END IF

 DO J1 = 1,NBLKS
   JB = 1 + (J1-1) * NCOL
   JF = JB + NCOL - 1
   JF = MIN (JF,NCHNL)

   WRITE(NW,1) CHN(JB:JF)
   WRITE(NW,2) TMS(JB:JF)
   WRITE(NW,'(3X)')
   DO JR = 1, NRX1
     WRITE(NW,3) JR,RXPLT(1:3,JR),YTR(JB:JF,JR)
   END DO
 END DO

 1 FORMAT(/T11,'RECEIVER POSITIONS',6X,35(:5X,A))
 2 FORMAT(T8,'Northing     Easting     Elev',T37,35G13.4)
 3 FORMAT(I3,2F12.2,F9.2,35G13.4)

END SUBROUTINE WRTDP

 SUBROUTINE WRTDT (NW,PRTYP,NCHNL,TMS,NLOC,NRX1,JC,RXPLT,YTR)
!------------------------------------------------------------

!***  Called by WRITE_TD
!***  Calls WRSLVP

!  Writes time-domain output in temporal form.

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,76)
 INTEGER NW,PRTYP,NCHNL,NLOC,NRX1,JC,JT,NBLKS,J1,JB,JF,NCOL
 REAL TMS(NCHNL),YTR(NCHNL,NLOC)
 REAL(KIND=QL) RXPLT(3,NLOC)
 CHARACTER(LEN=23) DIRECTION(6)
 CHARACTER(LEN=50) RESPONSE(3)
 DATA DIRECTION /'NORTH COMPONENT        ','EAST COMPONENT         ','VERTICAL COMPONENT     ', &
                 'BH HORIZONTAL COMPONENT','BH SLOPE COMPONENT     ','BH AXIAL COMPONENT     '/
 DATA RESPONSE /'STANDARD OUTPUT - LAYERED EARTH + SCATTERED FIELDS', &
                'SCATTERED FIELD - INDUCED PLUS CURRENT GATHERING  ', &
                'PERCENT TARGET EFFECT                             '/

 IF (JC < 7) THEN
   WRITE(NW,'(/T7,2A)') 'TEMPORAL OUTPUT:  ',DIRECTION(JC)
 ELSE
   WRITE(NW,'(/T7,2A)') 'TEMPORAL OUTPUT:  '
 END IF
 WRITE(NW,'(T7,A)') RESPONSE(PRTYP)
 WRITE(NW,4)

 NCOL = MIN (NRX1, 35)
 NBLKS = 1
 IF (NRX1 > 35) THEN
   NBLKS = NRX1 / NCOL
   IF (MOD (NRX1,NCOL) > 0) NBLKS = NBLKS + 1
 END IF

 DO J1 = 1,NBLKS
   JB = 1 + (J1-1) * NCOL
   JF = JB + NCOL - 1
   JF = MIN (JF,NRX1)
   WRITE(NW,1) RXPLT(1,JB:JF)
   WRITE(NW,2) RXPLT(2,JB:JF)
   WRITE(NW,3) RXPLT(3,JB:JF)
   WRITE(NW,'(3X)')

   DO JT = 1,NCHNL
     WRITE(NW,'(I4,F10.3,T18,35G13.4)') JT,TMS(JT),YTR(JT,JB:JF)
   END DO
 END DO

 1 FORMAT(/T2,'       Window',T16,35F13.2)
 2 FORMAT( T2,'      Centres',T16,35F13.2)
 3 FORMAT( T2,'Chnl   (ms)  ',T16,35F13.2)
 4 FORMAT(/T20,'RECEIVER COORDINATES (Top to Bottom): Northing, Easting, Elevation')

END SUBROUTINE WRTDT

 SUBROUTINE WRITE_FD_PPM (NW,NFRQ,FREQ,NTXE,CMPDX,RXED,RXND,RXZD,TITLE,BFD,CURNT,DXPRM)
!--------------------------------------------------------------------------------------

!  Prints out frequency-domain output in PPM for magnetic dipole-dipole surveys
!
!              NW - output unit number
!            FREQ - frequencies for NFRQ responses
!            NTXE - number of transmitter-receiver positions
!           CMPDX - number of components: 1 or 3
!     RXED(I,J,K) - "real world" east coordinate of vertex K for receiver I of Rx Group J
!    BFD(I,1,K,L) - the Lth component measured response at frequency I from receiver J,
!        CURNT(I) - transmitter current at frequency I
!      DXPRM(I,1) - Ith component of the DC field
!      DXPRM(I,2) - DC field used to normalise Ith component.
!             I,J = 1,3 implies the slopw, horizontal & axial components respectively.

 IMPLICIT NONE
 INTEGER NW,NFRQ,NTXE,CMPDX,JC,KC,JS,JRI
 REAL FREQ(NFRQ),CURNT(NFRQ),RXPLT(3,NTXE),YTR(NFRQ),DXPRM(3,3)
 REAL(KIND=8), DIMENSION (1,NTXE,1) :: RXED,RXND,RXZD
 COMPLEX, DIMENSION(NFRQ,1,NTXE,3) :: BFD
 COMPLEX BFD1(NFRQ)
 CHARACTER(LEN=120) TITLE
 CHARACTER(LEN=29) DIRECTION(3)
 DATA DIRECTION /'HORIZONTAL BOREHOLE COMPONENT', &
                 'SLOPE BOREHOLE COMPONENT     ', &
                 'AXIAL BOREHOLE COMPONENT     '/

 WRITE(NW,'(//T3,A,A)') 'TITLE:  ',TITLE

 DO JS = 1,NTXE
   RXPLT(1,JS) = REAL (RXND(1,JS,1))
   RXPLT(2,JS) = REAL (RXED(1,JS,1))
   RXPLT(3,JS) = REAL (RXZD(1,JS,1))
 END DO
 DO JC = 1, CMPDX
   KC = 4-JC

   DO JRI = 1,2
     YTR = 0
     IF (JRI == 1) WRITE(NW,'(//T3,A,5X,A/T3,A)') 'INPHASE RESPONSE in PPM',DIRECTION(KC), &
                                                  '-----------------------'
     IF (JRI == 2) WRITE(NW,'(//T3,A,5X,A/T3,A)') 'QUADRATURE RESPONSE in PPM',DIRECTION(KC), &
                                                  '--------------------------'
     WRITE(NW,2)
     WRITE(NW,3) FREQ(1:NFRQ)
     WRITE(NW,'(3X)')
     DO JS = 1,NTXE
       BFD1(1:NFRQ) = BFD(1:NFRQ,1,JS,KC) / CURNT(1:NFRQ)
       IF (JRI == 1) THEN
         YTR(1:NFRQ) = 1.E6 *  REAL (BFD1(1:NFRQ)) / DXPRM(KC,2)
       ELSE
         YTR(1:NFRQ) = 1.E6 * AIMAG (BFD1(1:NFRQ)) / DXPRM(KC,2)
       END IF
       WRITE(NW,4) JS,RXPLT(1:3,JS),YTR(1:NFRQ)
     END DO
   END DO
 END DO
 WRITE(NW,1)

 1 FORMAT(//T3,'END OF PPM OUTPUT'/T3,'=================' )
 2 FORMAT(/T11,'RECEIVER POSITIONS',T40,'FREQUENCIES')
 3 FORMAT(T5,'Northing     Easting     Elev',T37,35G13.4)
 4 FORMAT(I3,2F12.2,F9.2,35F13.2)

 END SUBROUTINE WRITE_FD_PPM

 SUBROUTINE WRITE_FD (NW,PRFL,STEP,NFRQ,FREQ,SURVEY_TYPE,SOURCE_TYPE,NTXE,SXMNT,NRXG, &
                      NRGTX,RGTXID,LRX,MRX,NRX,UNITS,RX_TYPE,NCMPG,PRTCMP,RXMNT,RXED,  &
                      RXND,RXZD,MQVR,TITLE,BFD)
!------------------------------------------------------------------------------------

!  Prints out time domain output
!
!              NW - output unit number
!            PRFL = 1 for profile output; = 0 for spectral output
!            STEP = 0 for dB/dt output and 1 for B output
!                   (ignored for grounded wire receivers
!            FREQ - frequencies for NFRQ responses
!     SURVEY_TYPE - indicates seperate Tx-Rx arrays (1), fixed offset (2),
!                   central loop (3)
!     SOURCE_TYPE - indicates loop (1), grounded wire (2), magnetic dipole(3) or
!                   plane wave sources (4)
!            NTXE - number of transmitter positions
!        SXMNT(J) - turns * area for magnetic dipole transmitter J
!            NRXG - number of receiver groups
!        NRGTX(J) - number of receiver groups for transmitter J
!     RGTXID(I,J) - receiver group index for Ith group for transmitter J
!          NRX(J) - the number of receivers in receiver group J.  MRX = MAXVAL (NRX)
!             LRX - maximum number of receivers assigned to any transmitter position
!             MRX - maximum number of receivers in any receiver group
!           UNITS - output unit indicator for each receiver group
!         RX_TYPE - receiver type: mag dipole (1); finite loop (2); electric dipole (3)
!             CMP = 0 or 2 => compute 3 components, = 1 => compute 1 component,
!      RXMNT(I,J) - receiver moment for receiver I of group J if it is a mag dipole
!     RXED(I,J,K) - "real world" east coordinate of vertex K for receiver I of Rx Group J

!      RXND, RXZD - "real world" north and vertical locations of these vertices.
!            MQVR - Maximum number of vertices for all receiver groups (1, 2 or 4)
!
!    BFD(I,J,K,L) - the Lth component measured response at channel I from receiver J,
!                   transmitter K

!    NOTE:  BFD is: B if RX_TYPE = 1  (magnetic dipole)
!                                 Multiply by iw for voltage (STEP = 0)

!                   B integrated over area if RX_TYPE = 2  (loops and coincident loop)
!                                  Multiply by iw

!                   voltage if RX_TYPE = 3  (integrated electric field along grounded wire)

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL = SELECTED_REAL_KIND(12,76)
 COMPLEX, PARAMETER :: I2PI= (0.,6.283185)  ! 2 * pi * i
 INTEGER NW,PRFL,STEP,NFRQ,SURVEY_TYPE,SOURCE_TYPE,NTXE,NRXG,NRGTX(NTXE),RGTXID(NRXG,NTXE),   &
         RX_TYPE(NRXG),NCMPG(NRXG),PRTCMP(10,NRXG),UNITS(NRXG),NRX(NRXG),LRX,MRX,JRI,KC,MQVR, &
         KRG,KRGT,NRCNR,NRX1,JR,JRP,NLOC,JC,JC1,JRG,JS,JF,KRFIN,PRTYP
 REAL FREQ(NFRQ),OUTFAC(MRX,NRXG),RXMNT(MRX,NRXG),FAC,SXMNT(NTXE)
 REAL, ALLOCATABLE, DIMENSION(:,:) :: YTR
 REAL(KIND=QL), DIMENSION (MRX,NRXG,MQVR) :: RXED,RXND,RXZD
 REAL(KIND=QL), ALLOCATABLE :: RXPLT(:,:)
 COMPLEX, DIMENSION(NFRQ,LRX,NTXE,3) :: BFD
 LOGICAL CONVERT
 CHARACTER(LEN=120) TITLE
 CHARACTER(LEN=16) OUTTXT(NRXG),SXTXT(3)
 CHARACTER(LEN=17) RXTXT(3)

 DATA SXTXT /'loop.           ','grounded wire.  ','magnetic dipole.'/
 DATA RXTXT /'magnetic dipoles.','loops.           ','grounded wires.  '/

 WRITE(NW,'(/T3,A)') TITLE
 CALL SET_OUTPUT_FACTORS (NRXG,MRX,SURVEY_TYPE,RX_TYPE,RXMNT,STEP,UNITS, &
                          OUTTXT,OUTFAC)

 IF (SURVEY_TYPE == 1) THEN
   NLOC = MRX
   ALLOCATE (RXPLT(3,NLOC),YTR(NFRQ,NLOC))
   DO JS = 1, NTXE
     KRFIN = 0
     DO JRG = 1, NRGTX(JS)
       KRG = RGTXID(JRG,JS)  ! Receiver Group index for group JRG for Tx JS
       KRGT = RX_TYPE(KRG)   ! Identify RX_TYPE of group KRG
       WRITE(NW,1) JS, KRG, SXTXT(SOURCE_TYPE), RXTXT(KRGT),OUTTXT(KRG)

       NRX1 = NRX(KRG)      ! Number of receivers in receiver group KRG
       NRCNR = 1            ! set receiver midpoints
       IF (KRGT == 2) NRCNR = 4
       IF (KRGT == 3) NRCNR = 2

!  Set up plot points and multiply by iw to convert to voltage if necessary

       DO JR = 1,NRX1
         RXPLT(1,JR) = SUM (RXND(JR,KRG,1:NRCNR) ) / REAL (NRCNR,8)
         RXPLT(2,JR) = SUM (RXED(JR,KRG,1:NRCNR) ) / REAL (NRCNR,8)
         RXPLT(3,JR) = SUM (RXZD(JR,KRG,1:NRCNR) ) / REAL (NRCNR,8)

         CONVERT = .FALSE.
         IF (KRGT == 2) CONVERT = .TRUE.                  ! Loop voltage
         IF (KRGT == 1 .AND. STEP == 0) CONVERT = .TRUE.  ! db/dt for magnetic dipole
         IF (CONVERT) THEN   !  multiply by iw
           JRP = JR + KRFIN
           DO JF = 1,NFRQ
             BFD(JF,JRP,JS,1:3) = FREQ(JF) * I2PI * BFD(JF,JRP,JS,1:3)
           END DO
         END IF

       END DO
       DO JC = 1, NCMPG(KRG)
         IF (RX_TYPE(KRG) > 1) THEN
           JC1 = 7
           KC = 1
         ELSE
           JC1 = PRTCMP(JC,KRG)      !  Sum is now over specified components
           KC = JC1
           IF (JC1 > 3 ) KC = JC1-3  !  Assign borehole component
         END IF

         DO JRI = 1,2
           YTR = 0
           IF (JRI == 1) THEN
             WRITE(NW,'(//T3,A/T3,A)') 'INPHASE RESPONSE','----------------'
             DO JR = 1,NRX1
               FAC = SXMNT(JS) * OUTFAC(JR,KRG)
               JRP = JR + KRFIN
               YTR(1:NFRQ,JR) = FAC * REAL (BFD(1:NFRQ,JRP,JS,KC))
             END DO
           ELSE
             WRITE(NW,'(//T3,A/T3,A)') 'QUADRATURE RESPONSE','-------------------'
             DO JR = 1,NRX1
               FAC = SXMNT(JS) * OUTFAC(JR,KRG)
               JRP = JR + KRFIN
               YTR(1:NFRQ,JR) = FAC * AIMAG (BFD(1:NFRQ,JRP,JS,KC))
             END DO
           END IF
           PRTYP = 1
           CALL WRFDP (NW,PRFL,PRTYP,NFRQ,FREQ,NLOC,NRX1,JC1,RXPLT,YTR)
         END DO
       END DO
       KRFIN = KRFIN + NRX1  ! Set index for next group
     END DO                 ! End of Rx groups for transmitter JS
   END DO

 ELSE
   NLOC = NTXE
   ALLOCATE (RXPLT(3,NLOC),YTR(NFRQ,NLOC))
   IF (SURVEY_TYPE == 2) WRITE(NW,2) SXTXT(SOURCE_TYPE), OUTTXT(1)
   IF (SURVEY_TYPE == 3) WRITE(NW,3) OUTTXT(1)

   DO JS = 1,NTXE
     RXPLT(1,JS) = RXND(1,JS,1)
     RXPLT(2,JS) = RXED(1,JS,1)
     RXPLT(3,JS) = RXZD(1,JS,1)
     IF (STEP == 0) THEN
       DO JF = 1,NFRQ
         BFD(JF,1,JS,1:3) = FREQ(JF) * I2PI * BFD(JF,1,JS,1:3)
       END DO
     END IF
   END DO
   DO JC = 1, NCMPG(1)
     IF (RX_TYPE(1) > 1 .OR. SURVEY_TYPE == 4) THEN
       JC1 = 7
       KC = 1
     ELSE
       JC1 = PRTCMP(JC,1)        !  Sum is now over specified components
       KC = JC1
       IF (JC1 > 3 ) KC = JC1-3  !  Assign borehole component
     END IF

     DO JRI = 1,2
       YTR = 0
       IF (JRI == 1) WRITE(NW,'(//T3,A/T3,A)') 'INPHASE RESPONSE','----------------'
       IF (JRI == 2) WRITE(NW,'(//T3,A/T3,A)') 'QUADRATURE RESPONSE','-------------------'
       DO JS = 1,NTXE
         FAC = OUTFAC(1,JS) * SXMNT(JS)
         IF (JRI == 1) THEN
           YTR(1:NFRQ,JS) = FAC * REAL (BFD(1:NFRQ,1,JS,KC))
         ELSE
           YTR(1:NFRQ,JS) = FAC * AIMAG (BFD(1:NFRQ,1,JS,KC))
         END IF
       END DO

       PRTYP = 1
       CALL WRFDP (NW,PRFL,PRTYP,NFRQ,FREQ,NLOC,NTXE,JC1,RXPLT,YTR)
     END DO
   END DO

 END IF

 1 FORMAT(//T3,'Frequency-domain output for Transmitter Position',I3,' & Receiver Group',I3 &
           /T3,'-----------------------------------------------------------------------'   &
           /T3,'The source is a ',A                             &
           /T3,'The receivers in this group are ',A             &
           /T3,'For each position, the output is printed in ',A)
 2 FORMAT(//T3,'Frequency-domain output for a single magnetic dipole receiver moving with' &
           /T3,'constant offset with respect to the transmitter.'                    &
           /T3,'The source is a ',A                                                  &
           /T3,'For each position, the output is printed in ',A)
 3 FORMAT(//T3,'Frequency-domain output for a central loop survey.' &
           /T3,'For each position, the output is printed in ',A)

 END SUBROUTINE WRITE_FD

 SUBROUTINE WRFDP (NW,PRFL,PRTYP,NFRQ,FREQ,NLOC,NRX1,JC,RXPLT,YTR)
!------------------------------------------------------------------

!***  Called by WRSLV

!  Writes time-domain output in profile form.

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL = SELECTED_REAL_KIND(12,76)
 INTEGER NW,PRFL,PRTYP,NFRQ,NLOC,NRX1,JC,JR,NBLKS,J1,JB,JF,NCOL
 REAL FREQ(NFRQ),YTR(NFRQ,NLOC)
 REAL(KIND=QL) RXPLT(3,NLOC)

 CHARACTER(LEN=23) DIRECTION(6)
 CHARACTER(LEN=50) RESPONSE(3)
 DATA DIRECTION /'NORTH COMPONENT        ','EAST COMPONENT         ','VERTICAL COMPONENT     ', &
                 'BH HORIZONTAL COMPONENT','BH SLOPE COMPONENT     ','BH AXIAL COMPONENT     '/
 DATA RESPONSE /'STANDARD OUTPUT - LAYERED EARTH + SCATTERED FIELDS', &
                'SCATTERED FIELD - INDUCED PLUS CURRENT GATHERING  ', &
                'PERCENT TARGET EFFECT                             '/

 IF (PRFL == 0) THEN  ! Forget profiles, write output in spectral form.
   CALL WRFDS (NW,PRTYP,NFRQ,FREQ,NLOC,NRX1,JC,RXPLT,YTR)
   RETURN
 END IF


 IF (JC < 7) THEN
   WRITE(NW,'(/T7,2A)') 'PROFILE OUTPUT:  ',DIRECTION(JC)
 ELSE
   WRITE(NW,'(/T7,2A)') 'PROFILE OUTPUT:  '
 END IF

 WRITE(NW,'(T7,A)') RESPONSE(PRTYP)

 NCOL = MIN (NFRQ, 35)
 NBLKS = 1
 IF (NFRQ > 35) THEN
   NBLKS = NFRQ / NCOL
   IF (MOD (NFRQ,NCOL) > 0) NBLKS = NBLKS + 1
 END IF

 DO J1 = 1,NBLKS
   JB = 1 + (J1-1) * NCOL
   JF = JB + NCOL - 1
   JF = MIN (JF,NFRQ)

   WRITE(NW,1)
   WRITE(NW,2) FREQ(JB:JF)
   WRITE(NW,'(3X)')
   DO JR = 1, NRX1
     WRITE(NW,3) JR,RXPLT(1:3,JR),YTR(JB:JF,JR)
   END DO
 END DO

 1 FORMAT(/T11,'RECEIVER POSITIONS',T40,'FREQUENCIES')
 2 FORMAT(T5,'Northing     Easting     Elev',T37,35G13.4)
 3 FORMAT(I3,2F12.2,F9.2,35G13.4)

END SUBROUTINE WRFDP

 SUBROUTINE WRFDS (NW,PRTYP,NFRQ,FREQ,NLOC,NRX1,JC,RXPLT,YTR)
!-------------------------------------------------------------

!***  Called by WRITE_FD
!***  Calls WRSLVP

!  Writes time-domain output in spectral form.

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL = SELECTED_REAL_KIND(12,76)
 INTEGER NW,PRTYP,NFRQ,NLOC,NRX1,JC,NBLKS,J1,JB,JE,JF,NCOL
 REAL FREQ(NFRQ),YTR(NFRQ,NLOC)
 REAL(KIND=QL) RXPLT(3,NLOC)
 CHARACTER(LEN=23) DIRECTION(6)
 CHARACTER(LEN=50) RESPONSE(3)
 DATA DIRECTION /'NORTH COMPONENT        ','EAST COMPONENT         ','VERTICAL COMPONENT     ', &
                 'BH HORIZONTAL COMPONENT','BH SLOPE COMPONENT     ','BH AXIAL COMPONENT     '/
 DATA RESPONSE /'STANDARD OUTPUT - LAYERED EARTH + SCATTERED FIELDS', &
                'SCATTERED FIELD - INDUCED PLUS CURRENT GATHERING  ', &
                'PERCENT TARGET EFFECT                             '/

 IF (JC < 7) THEN
   WRITE(NW,'(/T7,2A)') 'SPECTRAL OUTPUT:  ',DIRECTION(JC)
 ELSE
   WRITE(NW,'(/T7,2A)') 'SPECTRAL OUTPUT:  '
 END IF
 WRITE(NW,'(T7,A)') RESPONSE(PRTYP)
 WRITE(NW,4)

 NCOL = MIN (NRX1, 35)
 NBLKS = 1
 IF (NRX1 > 35) THEN
   NBLKS = NRX1 / NCOL
   IF (MOD (NRX1,NCOL) > 0) NBLKS = NBLKS + 1
 END IF

 DO J1 = 1,NBLKS
   JB = 1 + (J1-1) * NCOL
   JE = JB + NCOL - 1
   JE = MIN (JE,NRX1)
   WRITE(NW,1) RXPLT(1,JB:JE)
   WRITE(NW,2) RXPLT(2,JB:JE)
   WRITE(NW,3) RXPLT(3,JB:JE)
   WRITE(NW,'(3X)')

   DO JF = 1,NFRQ
     WRITE(NW,'(I4,G12.4,T18,35G13.4)') JF,FREQ(JF),YTR(JF,JB:JE)
   END DO
 END DO

 1 FORMAT(/T16,35F13.2)
 2 FORMAT(T16,35F13.2)
 3 FORMAT( T6,'Frequency',T16,35F13.2)
 4 FORMAT(/T20,'RECEIVER COORDINATES (Top to Bottom): Northing, Easting, Elevation')

END SUBROUTINE WRFDS

  SUBROUTINE WRYT_LOG_FILE (NLG,MSG,MXERR,ERR_LVL)
! -------------------------------------------------

! This subroutine prints out warning and fatal error messages on the LOG file.
!
! NLG = output unit index
! MSG refers to error message index
! ERR_LVL = 1 for warnings;  = 2 for fatal errors
! MXERR = MAX (ERR_LVL)

 INTEGER ERR_LVL, MSG, NLG, MXERR

 MXERR = MAX (ERR_LVL,MXERR)
 IF (ERR_LVL == 1) WRITE(NLG,101)
 IF (ERR_LVL == 2) WRITE(NLG,102)
 IF (ERR_LVL == 3) WRITE(NLG,103)


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
 IF (MSG == 20) WRITE(NLG,20)

  1 FORMAT(T3,'The value for TDFD is outside the permitted range.' &
          /T3,'The allowed values are: 1 for time-domain or 2 for frequency domain.')
  2 FORMAT(T3,'The allowed values for PRFL are: 1 for profile mode, 0 otherwise.' &
          /T3,'PRFL has been set to 1.')
  3 FORMAT(T3,'The value for DO3D is outside the permitted range of 1 or 2.' &
          /T3,'DO3D has been reset to 1.  A new model will be computed.')
  4 FORMAT(T3,'The value for STEP is outside the permitted range.' &
          /T3,'The allowed values are: 0 or 1.')
  5 FORMAT(T3,'The value for KRXW is outside the permitted range.' &
          /T3,'The allowed values are: 1 or 2.')
  6 FORMAT(T3,'Central loop option is only available for loop sources.')
  7 FORMAT(T3,'This value for TOPN is outside the permitted range.' &
          /T3,'It must be > 0.')
  8 FORMAT(T3,'SOURCE_TYPE is only allowed values: 1, 2, 3, or 4.')
  9 FORMAT(T3,'FLAT_TX is only allowed values of 0 and 1')
 10 FORMAT(T3,'SURVEY_TYPE is only allowed values: 1, 2, 3, or 4.')
 11 FORMAT(T3,'There must be at least one receiver group if SURVEY_TYPE = 1.')
 12 FORMAT(T3,'The magnetotelluric option has not yet been implemented in this code' &
          /T3,'Please choose another source type option')
 13 FORMAT(T3,'Not all of the transmitter positions have been specified by events.' &
          /T3,'Is this intended?')
 14 FORMAT(T3,'The constant offset option is not available for grounded wire sources.')
 15 FORMAT(T3,'The offset between magnetic dipole sources and magnetic dipole recaivers' &
          /T3,'should not be less than 0.01 m')
 16 FORMAT(T3,'The coincident loop option is available only for loop sources.'/)
 17 FORMAT(T3,'The coincident loop option is available only for time-domain'/)
 18 FORMAT(T3,'Lithology indices must be an integer between 1 & NLITH')
 20 FORMAT(T3,'This is a BIG model, over 60,000 nodes.  It will not run quickly.')
 101 FORMAT(/T2,'INFORMATION'/T2,'-----------')
 102 FORMAT(/T2,'WARNING'/T2,'-------')
 103 FORMAT(/T2,'FATAL ERROR'/T2,'----- -----')

END SUBROUTINE  WRYT_LOG_FILE

 SUBROUTINE SET_RX_SUBNET_CL (NTXE,SKIP_TX,MXVRTX,N_VRTX,SXN,SXE,MXRS,NRS,XRS,YRS,WTRS)
!---------------------------------------------------------------------------------------

!*** Called by ARJUNA_2D
!*** Calls SET_RX_TRIANGLE, SET_RX_SUBNET_QUAD

! Sets up virtual receivers for coincident loop sources.
! Four-sided loops are set up as a rectangular array using SET_RX_SUBNET_QUAD
! Uniform weights are used.

! Six-sided loops are divided into 2 four-sided loops and set up
! by 2 calls to SET_RX_SUBNET_QUAD
! Uniform weights are used.

! Otherwise, the loop is divided into N_VRTX triangles defined by two adjacent loop
! vertices and loop centre.  Six receivers per triangle are used with weights
! Triangle_area / 6.  An additional receiver is put at loop centre.

! INPUT PARAMETERS:
! ----------------
!
!             NTXE - number of coincident loop transmitter positions
!          SKIP_TX - No computation performed if true.
!           MXVRTX - maximum number of vertices for any loop
!           N_VRTX - number of vertices for each loop
!         SXN, SXE - north & east coordinates of loop vertices
!            MXRS - maximum NRS
!                                                          _______________________
!                                                          \                     /
!                                                           \   x     x     x   /      .9
!                                                            \                 /
!                                                             \               /
!   NRS =  6 * N_VRTX + 1 rule based on averaging over         \   x    x    /         .6
!           triangles formed by connecting eacH vertex          \           /
!           with the loop cemtre.                                \         /
!                                                                 \   x   /            .3
!                                                                  \     /
!                                                                   \   /
!                                                                     x
!
!  OUTPUT PARAMETERS:
!  -----------------
!          NRS(JS) - number of receivers for loop JS
!      XRS(*,1,JS) - North coordinates of the NRS magnetic dipole points
!      YRS(*,1,JS) - East coordinates of the NRS magnetic dipole points
!
!     WTRS(*,1,JS) - integration weights associated with magnetic dipole receiver net

 IMPLICIT NONE
 INTEGER, PARAMETER :: MXRS4=200
 INTEGER NTXE,MXVRTX,KVRTX,N_VRTX(NTXE),MXRS,NRS(NTXE),NRS4,NRS6,JA,JB,K,JS
 REAL XCNTR,YCNTR,AREA3,TOTAL_AREA,SXN1(4),SXE1(4),WTRS1,XRS4(MXRS4),YRS4(MXRS4),XRS3(6),YRS3(6)
 REAL, DIMENSION (MXVRTX,NTXE) :: SXN,SXE
 REAL, DIMENSION (MXRS,1,NTXE) :: XRS,YRS,WTRS
 LOGICAL SKIP_TX(NTXE)

 DO JS = 1,NTXE
   IF (SKIP_TX(JS) ) CYCLE
   KVRTX = N_VRTX(JS)
   XCNTR = SUM (SXN(1:KVRTX,JS)) / REAL (KVRTX)        ! Compute loop centre coordinates
   YCNTR = SUM (SXE(1:KVRTX,JS)) / REAL (KVRTX)

   IF (KVRTX == 4 .OR. KVRTX == 6) THEN                ! Rectangular or hexagonal loops
     SXN1(1:4) = SXN(1:4,JS); SXE1(1:4) = SXE(1:4,JS)
     CALL SET_RX_SUBNET_QUAD (SXN1,SXE1,MXRS4,NRS4,XRS4,YRS4,WTRS1)
     NRS(JS) = NRS4; WTRS(1:NRS4,1,JS) = WTRS1
     XRS(1:NRS4,1,JS) = XRS4(1:NRS4); YRS(1:NRS4,1,JS) = YRS4(1:NRS4)

     IF (KVRTX == 6) THEN
       SXN1(2:4) = SXN(4:6,JS); SXE1(2:4) = SXE(4:6,JS)
       CALL SET_RX_SUBNET_QUAD (SXN1,SXE1,MXRS4,NRS6,XRS4,YRS4,WTRS1)
       NRS(JS) = NRS(JS) + NRS6
       XRS(NRS4+1:NRS(JS),1,JS) = XRS4(1:NRS6)
       YRS(NRS4+1:NRS(JS),1,JS) = YRS4(1:NRS6)
       WTRS(NRS4+1:NRS(JS),1,JS) = WTRS1
     END IF

   ELSE
     NRS(JS) = 6 * KVRTX + 1

     XRS(1,1,JS) = XCNTR
     YRS(1,1,JS) = YCNTR

     TOTAL_AREA = 0.
     DO JA = 1, KVRTX
       K = 1+ 6*(JA-1)  ! index from previous triangles
       JB = JA + 1
       IF (JA == KVRTX) JB = 1
       SXN1(1) = XCNTR;  SXN1(2) = SXN(JA,JS);  SXN1(3) = SXN(JB,JS)
       SXE1(1) = YCNTR;  SXE1(2) = SXE(JA,JS);  SXE1(3) = SXE(JB,JS)

       CALL SET_RX_TRIANGLE (SXN1,SXE1,XRS3,YRS3,AREA3,WTRS1)
       TOTAL_AREA = TOTAL_AREA + AREA3
       WTRS(K+1:K+6,1,JS) = WTRS1
       XRS(K+1:K+6,1,JS) = XRS3(1:6)
       YRS(K+1:K+6,1,JS) = YRS3(1:6)
     END DO
     WTRS(1,1,JS) = TOTAL_AREA / REAL (NRS(JS))
   END IF

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
   IF (MOD(ND(J1),2) == 0) ND(J1) = ND(J1) + 1
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

 SUBROUTINE SET_RX_TRIANGLE (SXN1,SXE1,XRS3,YRS3,AREA3,WTRS3)
!------------------------------------------------------------

!*** Called by SET_RX_SUBNET_CL
!*** Calls DIST2D

!  Computes area of triangle and establishes 6 equivalent receivers.

!    SXN1, SXE1 - coordinates of the three vertices.  Where the triangle is cut
!                 from a loop, SX*1(2:3) are usually taken as loop vertices and
!                 SX*1(1) is the loop centre.  SX*1(4) is not used.
!    XRS3, YRS3 - are the locations of the six equivalent magnetic dipole receivers.
!          AREA - triangle area
!         WTRS3 = intregration weight = AREA / 6

 REAL SXN1(4),SXE1(4),XRS3(6),YRS3(6),AREA3,WTRS3,DIST2D,A,B,C,S

 A = DIST2D (SXN1(1),SXE1(1), SXN1(2),SXE1(2))
 B = DIST2D (SXN1(2),SXE1(2), SXN1(3),SXE1(3))
 C = DIST2D (SXN1(3),SXE1(3), SXN1(1),SXE1(1))
 S = (A + B + C) /2.
 AREA3 = SQRT (S* (S-A)* (S-B)* (S-C))     ! triangle area using Heron's formula
 WTRS3 = AREA3 / 6.

 XRS3(1) = 0.7 * SXN1(1) + 0.15 * (SXN1(2) + SXN1(3))
 YRS3(1) = 0.7 * SXE1(1) + 0.15 * (SXE1(2) + SXE1(3))

 XRS3(2) = 0.4 * SXN1(1) + 0.6 * (0.25 * SXN1(2) + 0.75 * SXN1(3))
 YRS3(2) = 0.4 * SXE1(1) + 0.6 * (0.25 * SXE1(2) + 0.75 * SXE1(3))
 XRS3(3) = 0.4 * SXN1(1) + 0.6 * (0.25 * SXN1(3) + 0.75 * SXN1(2))
 YRS3(3) = 0.4 * SXE1(1) + 0.6 * (0.25 * SXE1(3) + 0.75 * SXE1(2))

 XRS3(4) = 0.1 * SXN1(1) + 0.45 * (SXN1(2) + SXN1(3))
 YRS3(4) = 0.1 * SXE1(1) + 0.45 * (SXE1(2) + SXE1(3))
 XRS3(5) = 0.1 * SXN1(1) + 0.9 *  (SXN1(2) /6. + SXN1(3) * 5./6.)
 YRS3(5) = 0.1 * SXE1(1) + 0.9 *  (SXE1(2) /6. + SXE1(3) * 5./6.)
 XRS3(6) = 0.1 * SXN1(1) + 0.9 *  (SXN1(3) /6. + SXN1(2) * 5./6.)
 YRS3(6) = 0.1 * SXE1(1) + 0.9 *  (SXE1(3) /6. + SXE1(2) * 5./6.)

 END SUBROUTINE SET_RX_TRIANGLE

 SUBROUTINE ARJUNA_EXCLUDE (NLG,SOURCE_TYPE,NTXE,MXVRTX,N_VRTX,SXZ,NRXTX,LRX, &
                           RXID,SKIP_TX,SKIP_RX)
!---------------------------------------------------------------------------

!  Eliminates transmitter receiver combinations that have yet to be programmed
!  If TRUE, SKIP_TX(JS) eliminates transmitter JS
!  If TRUE, SKIP_RX(JR,JS) eliminates receiver JR of transmitter JS

!*** Called by MAIN

!            NLG - error log file number
!           NTXE - number of transmitter positions
!         MXVRTX = MAXVAL (N_VRTX)
!      N_VRTX(J) - number of vertices for transmitter loop or GW J
!       SXZ(I,J) - depth from surface (+ down)
!       NRXTX(J) - number of receivers for transmitter J
!            LRX - maximum number of receivers for any transmitter
!      RXID(I,J) - RX_TYPE of receiver I for transmitter J


 REAL, PARAMETER :: FTOL = 0.05
 INTEGER NLG,SOURCE_TYPE,LRX,NTXE,RXID(LRX,NTXE),MXVRTX, &
         N_VRTX(NTXE),NRXTX(NTXE),JS,JR,JV
 REAL SXZ(MXVRTX,NTXE)
 LOGICAL SKIP_TX(NTXE),SKIP_RX(LRX,NTXE), LOOP_PRT

 SKIP_TX = .FALSE.
 SKIP_RX = .FALSE.
 LOOP_PRT = .TRUE.

 IF (SOURCE_TYPE == 2) THEN
   WRITE(NLG,2)
   WRITE(*,1)
   STOP
 END IF
 IF (SOURCE_TYPE == 4) THEN
   WRITE(NLG,2)
   WRITE(*,2)
   STOP
 END IF

 IF (SOURCE_TYPE == 3) THEN
   DO JS = 1, NTXE
     DO JR = 1, NRXTX(JS)
       IF (RXID(JR,JS) > 1) THEN   !  Only magnetic dipole receivers allowed
         SKIP_RX(JR,JS) = .TRUE.   !  for mag dipole sources.
         WRITE(NLG,5) JR,JS
         WRITE(*,5) JR,JS
       END IF
     END DO
   END DO
 ELSE IF (SOURCE_TYPE == 1) THEN
   TXLOOP: DO JS = 1,NTXE
     DO JV = 1, N_VRTX(JS) - 1
       IF ( ABS (SXZ(JV,JS) - SXZ(JV+1,JS) ) > FTOL .AND. LOOP_PRT) THEN
         WRITE(NLG,3)           ! Surface Loop sources only are implemented so far
         WRITE(*,3)
         LOOP_PRT = .FALSE.       ! Print warning only once.
       END IF
     END DO
   END DO TXLOOP
 END IF

 1 FORMAT(//T3,'THE ELCTRIC SOURCE OPTION HAS NOT BEEN IMPLEMENTED IN THIS VERSION OF ARJUNA', &
           /T3,'EXECUTION ABORTED')
 2 FORMAT(//T3,'THE MAGNETOTELLURICS OPTION HAS NOT BEEN IMPLEMENTED IN THIS VERSION OF ARJUNA', &
           /T3,'EXECUTION ABORTED')
 3 FORMAT(//T3,'Loop sources are restricted to lie on the surface for this version of ARJUNA.' &
           /T3,'The elevations of all loops are changed so that they lie on the surface.')
 5 FORMAT(//T3,'For magnetic dipole transmitters, only dipole receivers have been implemented in ARJUNA.' &
           /T3,'The response for receiver',I3,' of transmitter',I3,' will not be computed')

END SUBROUTINE ARJUNA_EXCLUDE

 REAL FUNCTION DIST2D (X1,Y1,X2,Y2)
!----------------------------------

! Computes distance RHO between points (X1, Y1) & (X2, Y2)

 REAL X1,Y1,X2,Y2

 DIST2D = SQRT ((X1-X2)**2 + (Y1-Y2)**2)

 END FUNCTION DIST2D

 SUBROUTINE ARJUNA_2D (NFRQ,FREQ,SURVEY_TYPE,SOURCE_TYPE,NTXE,MXVRTX,N_VRTX,SXE,SXN,  &
                       SXZ,SXDIP,SXAZ,NRXG,NRGTX,RX_TYPE,RGTXID,NRX,MRX,MQVR,RXE,RXN, &
                       RXZ,NE,NZ,ELOC,ZLOC,ND,NAIR,LITH,LYTH,NLITH,NPROP,NLG,NEZ,NPN, &
                       LNODS,COORD)
!----------------------------------------------------------------------------------

! Main routine for Arjuna computation.  Hx, Hy, & Hz, the north, east and
! vertical frequency-domain magnetic fields at each receiver position
! are written to unit ND near the end of Subroutine HYEY.

!***  Called by: MAIN
!***      Calls: SHAPEF, FRONT, HYEY

!       FREQ - array of NFRQ frequencies at which the rsponse will be computed.
!SURVEY_TYPE - indicates loop (1), grounded wire (2), magnetic dipole(3) or
!              plane wave sources (4)
!SOURCE_TYPE - indicates loop (1), grounded wire (2), magnetic dipole(3) or
!              plane wave sources (4)
!       NTXE - number of transmitter positions
!     MXVRTX - maximum number of vertices for any tansmitter
!  N_VRTX(J) - number of vertices for tansmitter J
!     MXVRTX - maximum number of vertices for any tansmitter
!   SXE(I,J) - local east coordinate of vertex I for loop position J
!   SXN(I,J) - local coordinate of vertex I for loop position J
!   SXZ(I,J) - local (z positive down) depth of vertex I for loop position J
!   SXDIP(J) - dip (in radians) of dipole J (eg; vertical = 0, horizontal = 90)
!    SXAZ(J) - azimuth (in radians) of dipole J (north = 0, east = 90)
!       NRXG - number of receiver groups
!            - 1 if SURVEY_TYPE > 1
!   NRGTX(J) - number of receiver groups for transmitter J
!            - 1 if SURVEY_TYPE > 1
! RX_TYPE(I) - for Rx group I: dipole (1), loop (2), grounded electrode (3)
!RGTXID(I,J) - receiver group index for Ith group for transmitter J
!     NRX(J) - the number of receivers in receiver group J.  MRX = MAXVAL (NRX)
!            - 1 if SURVEY_TYPE > 1
!        MRX - maximum number of receivers in any receiver group
!       MQVR - maximum number of vertices for all receivers
!            - 1 if all sources are magnetic dipoles)
! RXE(J,I,K) - local east coordinate of the Kth vertex of the
!              Jth receiver of receiver group I
! RXE(J,I,K) - local north coordinate of vertex K of Rx J of Group I
! RXZ(J,I,K) - depth (z is positive down) of vertex K of Rx J of Group I
!            - K = 1 only for mag dipole Rx; 1 to 2 for electric dipole Rx; 1 to 4 for loop Rx

!           NE    - total number of nodes in East direction (user plus boundaries)
!           NZ    - total number of nodes in vertical direction (user plus air
!                   plus bottom boundary
!           NAIR  - number of nodes in the air
!           NEZ   - total number of cells in mesh  (NE-1) * (NZ-1)
!           NPN   - (2*NE-1)*(2*NZ-1) - NEZ
!           LNODS - array of element node numbers.
!           COORD - element coordinate array
!           NPROP - number of lithology properties
!           NLITH - number of lithologies defined for mesh
!           LYTH  - properties array of lithology
!           LITH  - element lithology array
!           NKX   - number of wave numbers
!           KX    - array of wave numbers in North direction
!           BPR   - array for the spline location axis.
!           FREQ  - array of NFRQ frequencies
!       ELOC(J,K) - the east coordinate of node (J,K) in metres.
!       ZLOC(J,K) - the relative level of node (J,K) in metres.
!

 IMPLICIT NONE
 INTEGER, PARAMETER :: KXDEC=5, NKX=21  ! Set the number of kx transform values
 INTEGER, PARAMETER :: MXRS=200
 INTEGER NLG,ND,NFRQ,NT1,NT2,NE,NZ,NAIR,NEZ,NPN,LNODS(NEZ,8),NPM,NTXE,   &
         NLITH,NPROP,LITH(NE,NZ),J,SOURCE_TYPE,SURVEY_TYPE,MRX,MXVRTX, &
         MQVR,NRXG,RGTXID(NRXG,NTXE),NRS(NTXE)
 REAL COORD(NPN,2),ELOC(NE,NZ),ZLOC(NE,NZ),FREQ(NFRQ),KX(NKX),AST, &
      LYTH(NLITH+1,NPROP),SXDIP(NTXE),SXAZ(NTXE)
 REAL   , DIMENSION(:,:,:), ALLOCATABLE :: F1,F2,F3,F4,F5
 INTEGER, DIMENSION (NTXE) :: N_VRTX,NRGTX
 INTEGER, DIMENSION (NRXG) :: NRX,RX_TYPE
 REAL, DIMENSION (MXVRTX,NTXE) :: SXE, SXN, SXZ
 REAL, DIMENSION (MRX,NRXG,MQVR) :: RXE, RXN, RXZ
 REAL, DIMENSION (MXRS,1,NTXE) :: XRS,YRS,WTRS
 LOGICAL SKIP_TX(NTXE)

 SXZ  = -SXZ
 RXZ  = -RXZ
 ZLOC = -ZLOC
 COORD(1:NPN,2) = -COORD(1:NPN,2)

 NT1 = 8       !  Set unit to store wave-domain results.
 NT2 = 9
 OPEN(NT1,STATUS = 'SCRATCH')
 OPEN(NT2,STATUS = 'SCRATCH')

 NPM = (NE-1) * (NZ - NAIR - 1)

! Compute shape functions

 ALLOCATE ( F1(8,8,NEZ),F2(8,8,NEZ),F3(8,8,NEZ),F4(8,8,NEZ),F5(8,8,NEZ) )

 CALL SHAPEF (NLG,COORD,LNODS,NEZ,NPN,NE,NZ,F1,F2,F3,F4,F5)

 KX(1) = 1.E-5               ! Set transform values in kx domain
 AST = 1. / REAL (KXDEC)
 DO J = 2, NKX
   KX(J) = KX(J-1) * 10.**AST
 END DO

 IF (SOURCE_TYPE == 1) THEN
    SKIP_TX(1:NTXE) = .FALSE.
    CALL SET_RX_SUBNET_CL (NTXE,SKIP_TX,MXVRTX,N_VRTX,SXN,SXE,MXRS,NRS,XRS,YRS,WTRS)
 END IF

 CALL FRONT (NE,NZ,NPN,NPM,NEZ,NAIR,SXE,SXZ,MXVRTX,NTXE,LNODS,NFRQ,FREQ,NKX,KX, &
             NPROP,NLITH,LYTH,LITH,NRX,NRXG,F1,F2,F3,F4,F5,NT1,NT2,COORD,SXDIP, &
             SXAZ,SOURCE_TYPE,N_VRTX,RXE,RXN,RXZ,MRX,MQVR,ELOC,ZLOC,MXRS,NRS,   &
             YRS,WTRS,SURVEY_TYPE,NRGTX,RGTXID,RX_TYPE)

 DEALLOCATE (F1,F2,F3,F4,F5)
 CALL HYEY (ND,NT1,NT2,NPN,NEZ,LNODS,COORD,NFRQ,FREQ,NKX,KX,NRXG,NRGTX,RX_TYPE,  &
            RGTXID,NRX,MRX,MQVR,RXE,RXN,RXZ,MXVRTX,NTXE,SXE,SXN,SXZ,NE,NZ,       &
            SOURCE_TYPE,N_VRTX,NLITH,LYTH,LITH,NPROP,ELOC,ZLOC,MXRS,NRS,XRS,YRS, &
            WTRS,SURVEY_TYPE,SXDIP,SXAZ,NAIR)

 CLOSE (NT1)
 CLOSE (NT2)

END SUBROUTINE ARJUNA_2D

 SUBROUTINE EHFLD (NE,NZ,NPN,NPM,NEZ,NAIR,SXE,SXZ,MXVRTX,NTXE,COORD,KX1, &
                   LNODS,F2,F4,F5,SRC1,SRC2,SRC3,SRC4,SRC5,ID,XX,SXDIP,  &
                   SXAZ,SOURCE_TYPE,MXRS,NRS,YRS,WTRS,SNTX,CSTX,N_VRTX)
!-------------------------------------------------------------------------
!   Subroutine EHFLD calculates the halfspace electric and magnetic fields
!   in wave number domain.
!
!**** Called by: SUBROUTINE FRONT
!****     Calls: SUBROUTINE CUBSPL
!****      Uses: MODULE AG_Filter_coefficients
!
! Input:    NE,NZ - numbers of nodes in East and depth direction
!           NPN   - (2*NE-1)*(2*NZ-1) - NEZ
!           NPM   - number of elements not in air
!           NAIR  - number of node rows in the air
!        SXE(I,J) - local east coordinate of vertex I for loop position J
!        SXN(I,J) - local coordinate of vertex I for loop position J
!        SXZ(I,J) - local (z positive down) depth of vertex I for loop position J
!           NTXE  - number of transmitter stations
!           COORD - arrays of element coordinates.
!           KX1   - wave number in North direction
!           LNODS - arrays of element node numbers.
!        F2,F4,F5 - derivative of shape function arrays
!       SXDIP(J) - dip (in degrees) of dipole J
!                  (eg; vertical = 0, horizontal = 90)
!        SXAZ(J) - azimuth (in degrees) of dipole J
!                  (north = 0, east = 90)
!
! Output  : SRC1,SRC2,SRC3 - source fields in wave-domain

!  Three components of electric field are computed in the Kx, Y, Z, domain:
!
!  Ex = North (along strike) component
!  Ey = East (across strike) component
!  Ez = Vertical component.
!
!  In Kx domain, each of these should have a factor:  SFAC = iwu / (4 PI)
!  This is applied in SUBROUTINE FRONT instead of here to speed computation time.

 USE AG_Filter_coefficients

 IMPLICIT NONE
 INTEGER NAIR,NE,NZ,NPN,NPM,NEZ,MXVRTX,NTXE,I,J,K,I1,J1,JS,NN,IA(8),ILM,ILM1, &
         LNODS(NEZ,8),NEMAX(2),ID(NPN),IM,IDD,SOURCE_TYPE,MXRS,NRS(NTXE),     &
         N_VRTX(NTXE)
 REAL, DIMENSION(8,8,NEZ) :: F2,F4,F5
 REAL COORD(NPN,2),YY,ZZ,R2,R3,TRC,TRS,GS(3),WG(3),S1(3,2),D1(3,2),DJX1,DJX2, &
      DJZ1,DJZ2,XX(NLO:NHI),G(NLO:NHI),KX1,SXDIP(NTXE),S2(3,2),D2(3,2),ED1,   &
      ED2,ED3,ED4,ED5,ED6,ED7,ED8,ZCNTR,SINTX,COSTX,SNF,CSF,SXAZ(NTXE),TRS1
 REAL, ALLOCATABLE, DIMENSION (:,:) :: EX,EY,EZ
 REAL, DIMENSION (MXVRTX,NTXE) :: SXE, SXZ
 REAL, DIMENSION (NTXE,8,NPM) :: SRC1,SRC2,SRC3,SRC4,SRC5
 REAL, DIMENSION (MXRS,1,NTXE) :: YRS,WTRS
 REAL, DIMENSION (MXRS,NTXE) :: SNTX,CSTX

 DATA GS(1),GS(2)/-0.577350269189626,+0.577350269189626/
 DATA WG(1),WG(2)/ 1.,1./
! DATA GS(1),GS(2),GS(3)/-0.774596669241483,0.,0.774596669241483/
! DATA WG(1),WG(2),WG(3)/ 0.555555555555555,0.888888888888888,0.555555555555555/

 ALLOCATE (EX(NTXE,NPN), EY(NTXE,NPN), EZ(NTXE,NPN))

 DO I = 1, 2                            ! Line shape funtions
   S1(3,I) = (GS(I)*GS(I) + GS(I))/2.
   S1(2,I) = (1. - GS(I)*GS(I))
   S1(1,I) = (GS(I)*GS(I) - GS(I))/2.
   S2(3,I) = (GS(I)*GS(I) + GS(I))/2.
   S2(2,I) = (1. - GS(I)*GS(I))
   S2(1,I) = (GS(I)*GS(I) - GS(I))/2.

   D1(3,I) = (2*GS(I) + 1.)/2.
   D1(2,I) = -2*GS(I)
   D1(1,I) = (2*GS(I) - 1.)/2.
   D2(3,I) = (2*GS(I) + 1.)/2.
   D2(2,I) = -2*GS(I)
   D2(1,I) = (2*GS(I) - 1.)/2.
 END DO

 G = XX / KX1
 NEMAX(1) = 2*NE - 1
 NEMAX(2) = NE
 NN = 0
 EX = 0. ; EY = 0. ; EZ = 0.
 DO J1 = 2*NAIR+1, 2*NZ-1
   IM = 1
   IF (MOD(J1,2) == 0)  IM = 2
   DO I1 = 1, NEMAX(IM)
     NN  = NN + 1
     IDD = ID(NN)
     DO JS = 1, NTXE
         IF (SOURCE_TYPE == 1) THEN
            ZCNTR = SUM (SXZ(1:N_VRTX(JS),JS)) / REAL(N_VRTX(JS))
            DO K = 1, NRS(JS)
               YY = COORD(IDD,1) - YRS(K,1,JS)
               ZZ = COORD(IDD,2) - ZCNTR
               IF (ABS(YY) > 10.) THEN
                  TRC = 0.; TRS = 0. ; TRS1 = 0.
                  DO I = NLO, NHI
                     R2  = G(I)*G(I)+YY*YY+ZZ*ZZ ; R3 = R2*SQRT(R2)
                     TRS = TRS  + WSIN1(I)/R3 * G(I)
                     TRC = TRC  + WCOS1(I)/R3
                     TRS1= TRS1 + WSIN1(I)/R3/R2 * G(I)
                  END DO
                  EX(JS,IDD) = EX(JS,IDD) + WTRS(K,1,JS) * TRS * CSTX(K,JS)
                  EY(JS,IDD) = EY(JS,IDD) + WTRS(K,1,JS) * TRC * CSTX(K,JS) * YY
                  EZ(JS,IDD) = EZ(JS,IDD) + WTRS(K,1,JS) * TRS * SNTX(K,JS)
               ENDIF
            END DO
         ELSE IF(SOURCE_TYPE == 3) THEN
           YY = COORD(IDD,1) - SXE(1,JS)
           ZZ = COORD(IDD,2) - SXZ(1,JS)
           IF (ABS(YY) > 10.) THEN
             TRC = 0. ; TRS = 0. ; TRS1 = 0.
             DO I = NLO, NHI
                R2 = G(I)*G(I)+YY*YY+ZZ*ZZ ; R3 = R2*SQRT(R2)
                TRS = TRS  + WSIN1(I)/R3 * G(I)
                TRC = TRC  + WCOS1(I)/R3
                TRS1= TRS1 + WSIN1(I)/R3/R2 * G(I)
             END DO
             SINTX = SIN(SXDIP(JS))
             COSTX = COS(SXDIP(JS))
             CSF = 0. ; SNF = 1.
             IF (SXAZ(JS) < 0.) SNF = -1.
             EX(JS,IDD) =  ZZ*TRC*CSF*SINTX + TRS*COSTX
             EY(JS,IDD) =  TRC*(YY*COSTX - ZZ*SINTX*CSF)
             EZ(JS,IDD) =  SINTX*(TRS*CSF + YY*TRC*SNF)
           END IF
         END IF
     END DO
   END DO
 END DO

 SRC1 = 0. ; SRC2 = 0. ; SRC3 = 0. ; SRC4 = 0. ; SRC5 = 0. ; ILM1 = 0
 DO I1 = 1, NE-1
   DO J1 = NAIR+1, NZ-1
     ILM  = (I1-1)*(NZ-1) + J1
     ILM1 = ILM1 + 1
     IA(1:8) = ABS(LNODS(ILM,1:8))
     DO I = 1, 8
       DO J = 1, 8
         DO JS = 1, NTXE
           SRC1(JS,I,ILM1) = SRC1(JS,I,ILM1) + F5(I,J,ILM)*EX(JS,IA(J))
           SRC2(JS,I,ILM1) = SRC2(JS,I,ILM1) + F4(I,J,ILM)*EX(JS,IA(J))
           SRC3(JS,I,ILM1) = SRC3(JS,I,ILM1) + F2(I,J,ILM)*EY(JS,IA(J))
         END DO
       END DO
     END DO

     DO I = 1, 2
       DJX1 = WG(I)*(D1(1,I)*COORD(IA(1),1)+D1(2,I)*COORD(IA(2),1)+D1(3,I)*COORD(IA(3),1))
       DJX2 = WG(I)*(D1(1,I)*COORD(IA(7),1)+D1(2,I)*COORD(IA(6),1)+D1(3,I)*COORD(IA(5),1))
       DJZ1 = WG(I)*(D2(1,I)*COORD(IA(3),2)+D2(2,I)*COORD(IA(4),2)+D2(3,I)*COORD(IA(5),2))
       DJZ2 = WG(I)*(D2(1,I)*COORD(IA(1),2)+D2(2,I)*COORD(IA(8),2)+D2(3,I)*COORD(IA(7),2))
       DO JS = 1, NTXE
         ED1 = -DJX1 * (S1(1,I)*EX(JS,IA(1)) + S1(2,I)*EX(JS,IA(2)) + S1(3,I)*EX(JS,IA(3)))
         ED2 =  DJX2 * (S1(1,I)*EX(JS,IA(7)) + S1(2,I)*EX(JS,IA(6)) + S1(3,I)*EX(JS,IA(5)))
         ED3 = -DJZ1 * (S2(1,I)*EZ(JS,IA(3)) + S2(2,I)*EZ(JS,IA(4)) + S2(3,I)*EZ(JS,IA(5)))
         ED4 =  DJZ2 * (S2(1,I)*EZ(JS,IA(1)) + S2(2,I)*EZ(JS,IA(8)) + S2(3,I)*EZ(JS,IA(7)))
         ED5 = -DJZ1 * (S2(1,I)*EX(JS,IA(3)) + S2(2,I)*EX(JS,IA(4)) + S2(3,I)*EX(JS,IA(5)))
         ED6 =  DJZ2 * (S2(1,I)*EX(JS,IA(1)) + S2(2,I)*EX(JS,IA(8)) + S2(3,I)*EX(JS,IA(7)))
         ED7 = -DJX1 * (S1(1,I)*EZ(JS,IA(1)) + S1(2,I)*EZ(JS,IA(2)) + S1(3,I)*EZ(JS,IA(3)))
         ED8 =  DJX2 * (S1(1,I)*EZ(JS,IA(7)) + S1(2,I)*EZ(JS,IA(6)) + S1(3,I)*EZ(JS,IA(5)))

         SRC4(JS,1,ILM1) = SRC4(JS,1,ILM1) + S1(1,I) * ED1 + S2(1,I) * ED4
         SRC4(JS,2,ILM1) = SRC4(JS,2,ILM1) + S1(2,I) * ED1
         SRC4(JS,3,ILM1) = SRC4(JS,3,ILM1) + S1(3,I) * ED1 + S2(1,I) * ED3
         SRC4(JS,4,ILM1) = SRC4(JS,4,ILM1)                 + S2(2,I) * ED3
         SRC4(JS,5,ILM1) = SRC4(JS,5,ILM1) + S1(3,I) * ED2 + S2(3,I) * ED3
         SRC4(JS,6,ILM1) = SRC4(JS,6,ILM1) + S1(2,I) * ED2
         SRC4(JS,7,ILM1) = SRC4(JS,7,ILM1) + S1(1,I) * ED2 + S2(3,I) * ED4
         SRC4(JS,8,ILM1) = SRC4(JS,8,ILM1)                 + S2(2,I) * ED4

         SRC5(JS,1,ILM1) = SRC5(JS,1,ILM1) + S2(1,I) * ED6 + S1(1,I) * ED7
         SRC5(JS,2,ILM1) = SRC5(JS,2,ILM1)                 + S1(2,I) * ED7
         SRC5(JS,3,ILM1) = SRC5(JS,3,ILM1) + S2(1,I) * ED5 + S1(3,I) * ED7
         SRC5(JS,4,ILM1) = SRC5(JS,4,ILM1) + S2(2,I) * ED5
         SRC5(JS,5,ILM1) = SRC5(JS,5,ILM1) + S2(3,I) * ED5 + S1(3,I) * ED8
         SRC5(JS,6,ILM1) = SRC5(JS,6,ILM1)                 + S1(2,I) * ED8
         SRC5(JS,7,ILM1) = SRC5(JS,7,ILM1) + S2(3,I) * ED6 + S1(1,I) * ED8
         SRC5(JS,8,ILM1) = SRC5(JS,8,ILM1) + S2(2,I) * ED6
       END DO
     END DO

   END DO
 END DO

 DEALLOCATE (EX,EY,EZ)

END SUBROUTINE EHFLD

 SUBROUTINE FRONT (NE,NZ,NPN,NPM,NEZ,NAIR,SXE,SXZ,MXVRTX,NTXE,LNODS,NFRQ,FREQ,NKX,KX, &
                   NPROP,NLITH,LYTH,LITH,NRX,NRXG,F1,F2,F3,F4,F5,NT1,NT2,COORD,SXDIP, &
                   SXAZ,SOURCE_TYPE,N_VRTX,RXE,RXN,RXZ,MRX,MQVR,ELOC,ZLOC,MXRS,NRS,   &
                   YRS,WTRS,SURVEY_TYPE,NRGTX,RGTXID,RX_TYPE)
!---------------------------------------------------------------------------------------
!   Subroutine FRONT assembles the element matrices and solves by the
!   frontal technique the global system of algebraic equations.
!   The source factor SFAC = iwu / (4 PI) is applied near the end
!   of the Kx loop rather than in the source subroutine EHFLD to save
!   computation time.
!
!**** Called by: ARJUNA_2D
!****     Calls: EHFLD
!
! Input   : NE,NZ - number of nodes in East and Depth-direction
!           NEZ   - total number of cells in mesh  (NE-1) * (NZ-1)
!           NAIR  - number of nodes in the air
!           NPN   - (2*NE-1)*(2*NZ-1) - NEZ
!           NPM   - number of elements not in air
!        SXE(I,J) - local east coordinate of vertex I for loop position J
!        SXN(I,J) - local coordinate of vertex I for loop position J
!        SXZ(I,J) - local (z positive down) depth of vertex I for loop position J
!           NTXE  - number of transmitter stations
!           LNODS - array of element node numbers.
!           FREQ  - array of NFRQ frequencies
!           NKX   - number of wave numbers
!           KX    - array of wave numbers in North direction
!           NPROP - number of lithology properties
!           NLITH - number of lithologies defined for mesh
!           LYTH  - properties array of lithology
!           LITH  - element lithology array
!           NRX   - number of Tx-Rx offsets
!           COORD - element coordinate array
!  F1,F2,F3,F4,F5 - Derivative of the shape function arrays
!        SXDIP(J) - dip (in degrees) of dipole J
!                   (eg; vertical = 0, horizontal = 90)

! Output  : HXX   - Wave domain H (north) along strike at Rx locations
!                   written to unit NT

 USE AG_Filter_coefficients

 IMPLICIT NONE

 REAL, PARAMETER :: EPS0=8.854156E-12, MU0=12.56637E-7, PI =3.14159265359
 COMPLEX, PARAMETER :: ONE=(1.0,0.), CI=(0.,1.0)
 INTEGER  MFRON,NPSR,I,J,K,I1,J1,II,ITX,IFRON,JFRON,ILM,NE,NZ,NN,NAIR,NPOS,JA,JB,   &
          MXVRTX,NTXE,NKX,IKX,NRXG,NRX(NRXG),NPN,NEZ,NLOC,ILM1,JR,JS,INODEL,IDEST,  &
          NEMAX(2),LNODS(NEZ,8),LOCEL(16,NEZ),NPM,NFRQ,JF,NSOL,JSOL,  &
          NLITH,NPROP,LITH(NE,NZ),KLITH,KELV1,SOURCE_TYPE,N_VRTX(NTXE),NT1,NT2,   &
          MRX,MQVR,NRS(NTXE),MXRS,SURVEY_TYPE,JRMAX,JG,JG1,JG1MAX,NRGTX(NTXE),NRSR, &
          RGTXID(NRXG,NTXE),LR,RX_TYPE(NRXG)
 INTEGER, ALLOCATABLE :: NLOCA(:,:),ID(:),NACVA(:),NAC(:,:), NDEST(:,:)
 INTEGER, ALLOCATABLE, DIMENSION(:) :: NFUNC,IFRO1,NIKN1
 REAL, DIMENSION(8,8,NEZ) :: F1,F2,F3,F4,F5
 REAL SIGR,KX1SQ,KX(NKX),FRQ,KX1,COORD(NPN,2),FREQ(NFRQ),IFIN,LYTH(NLITH+1,NPROP), &
      OMEGA,MU,EPS,CALF,CTAU,CFRQ,XX(NLO:NHI),WMU,SXDIP(NTXE),RXELOC,RXZLOC,R2,    &
      ELOC(NE,NZ),ZLOC(NE,NZ),XRL(9),YRL(9),SXAZ(NTXE)
 REAL,    DIMENSION (MXVRTX,NTXE) :: SXE, SXZ
 REAL,    DIMENSION (MRX,NRXG,MQVR) :: RXE, RXN, RXZ
 REAL,    ALLOCATABLE, DIMENSION(:,:,:) :: SRC1,SRC2,SRC3,SRC4,SRC5
 REAL, DIMENSION (MXRS,1,NTXE) :: YRS,WTRS
 REAL, DIMENSION (MXRS,NTXE) :: SNTX,CSTX
 COMPLEX, ALLOCATABLE, DIMENSION(:,:) :: RR,EQRH1,EQUAT,VECRV
 COMPLEX, ALLOCATABLE :: SR(:)
 COMPLEX PIVOT,PP,QQ,SS,ER(16,16),CWMU,CKX,HXX(8),TMP,KX2,SIG,SFAC,CC,DEN,EXX(8)
 DOUBLE PRECISION DELTA
 
 Integer :: tim(8)
 
 SAVE

 REWIND (NT1)
 REWIND (NT2)

 MFRON = 2 * (2*NZ+4)
 NPSR  = MFRON*(MFRON+1)/2

 ALLOCATE (NFUNC (MFRON), NACVA (MFRON), NAC (MFRON,NEZ), NLOCA(MFRON,MFRON) )
 ALLOCATE (NDEST(16, NEZ) )
 ALLOCATE (IFRO1 (2*NPN), NIKN1 (2*NPN), ID(NPN) )
 ALLOCATE (SRC1(NTXE,8,NPM), SRC2(NTXE,8,NPM), SRC3(NTXE,8,NPM), SRC4(NTXE,8,NPM), &
           SRC5(NTXE,8,NPM))

! Determine angle of individual dipole inside the loop
  Write (*, 3)
  IF (SOURCE_TYPE == 1) THEN
     DO JS = 1,NTXE
       DO K  = 1, NRS(JS)
          DO I = 1, NE-1
             IF ( (ELOC(I,1) <= YRS(K,1,JS)) .AND. (ELOC(I+1,1) > YRS(K,1,JS)) ) EXIT
          END DO
          ILM = (I-1)*(NZ-1) + NAIR + 1
          R2 = SQRT((COORD(LNODS(ILM,2),2)-COORD(LNODS(ILM,1),2))**2 + &
                    (COORD(LNODS(ILM,2),1)-COORD(LNODS(ILM,1),1))**2)
          SNTX(K,JS) = ABS(COORD(LNODS(ILM,2),2)-COORD(LNODS(ILM,1),2)) / R2
          CSTX(K,JS) = ABS(COORD(LNODS(ILM,2),1)-COORD(LNODS(ILM,1),1)) / R2
       END DO
     END DO
  END IF

! Coordinates for cos- and sin-transform.

 DELTA = LOG (10.D0) / REAL (NDEC,8)
 DO I = NLO, NHI
    XX(I) = SNGL(EXP(I*DELTA))
 END DO

!                                  Assign "FRONT-NODES" Book-Keeping
 NFUNC(1) = 0
 DO I = 1, MFRON-1
   NFUNC(I+1) = NFUNC(I) + I
 END DO

 DO I = 1, MFRON
    DO J = 1, MFRON
       IF (J >= I)  NLOCA(I,J) = NFUNC(J) + I
       IF (J <  I)  NLOCA(I,J) = NFUNC(I) + J
    END DO
 END DO

!                                  Assign negative values to "FRONT-NODES"

 DO I = 1, (2*NE-1)*(2*NZ-1) - (NE-1)*(NZ-1)
   DO ILM = 1, (NE-1)*(NZ-1)
     DO NN = 1, 8
       IF (LNODS(ILM,NN) == I)  THEN
         I1 = ILM
         J1 = NN
       END IF
      END DO
   END DO
   LNODS(I1,J1) = -I
 END DO
!                                  Assign variable node numbering.
 DO I = 1, NE-1
   DO J = 1, NZ-1
     ILM = (I-1)*(NZ-1) + J
     DO NN = 1, 8
       LOCEL(2*NN  ,ILM) = 2*LNODS(ILM,NN)
       LOCEL(2*NN-1,ILM) = 2*LNODS(ILM,NN) - 1
       IF (LNODS(ILM,NN) < 0)  LOCEL(2*NN-1,ILM) = 2*LNODS(ILM,NN) + 1
     END DO
   END DO
 END DO

 NEMAX(1) = 2*NE - 1
 NEMAX(2) = NE
 NN = 1
 DO J = 2*NAIR+1, 2*NZ-1
   ID(NN) = J
   J1 = 1
   IF (MOD(J,2) == 0)  J1 = 2
   DO I = 1, NEMAX(J1)
     IF (J1 == 1) THEN
       IF ( MOD(I,2) == 0 ) ID(NN+1) = ID(NN) + NZ     + J/2
       IF ( MOD(I,2) == 1 ) ID(NN+1) = ID(NN) + 2*NZ-1 - J/2
     ELSE
       ID(NN+1) = ID(NN) + (2*NZ-1) + NZ
     END IF
     NN = NN + 1
   END DO
 END DO

! Frontal solution method book keeping

     KELV1 = 0
     NACVA = 0
     DO I1 = 1, NE-1
       DO J1 = 1, NZ-1
         ILM  = (I1-1)*(NZ-1) + J1
         ILOOP: DO I = 1, 16
           INODEL = ABS(LOCEL(I,ILM))
           DO IFRON = 1, MFRON
             IF ( INODEL == NACVA(IFRON) ) THEN
               NDEST(I,ILM) = IFRON
               CYCLE ILOOP
             END IF
           END DO
           DO IFRON = 1, MFRON
             IF (NACVA(IFRON) == 0 ) THEN
               NDEST(I,ILM) = IFRON
               NACVA(IFRON) = INODEL
               CYCLE ILOOP
             END IF
           END DO
         END DO ILOOP
         NAC(1:MFRON,ILM) = NACVA(1:MFRON)

         DO K = 1, 16
           IF (LOCEL(K,ILM) < 0)  THEN
             DO IFRON = 1, MFRON
               IF (NACVA(IFRON) == -LOCEL(K,ILM)) THEN
                 KELV1 = KELV1 + 1
                 NIKN1(KELV1) = -LOCEL(K,ILM)
                 IFRO1(KELV1) =  IFRON
                 NACVA(IFRON) = 0
               END IF
             END DO
           END IF
         END DO
       END DO
     END DO

! Solve for every Kx-value and frequencies

 NSOL = NKX * NFRQ
 KX_LOOP: DO IKX = 1, NKX
   CALL DATE_AND_TIME (Values = tim)
   KX1   = KX(IKX)
   WRITE(*,1) IKX, NKX, tim(5:7), tim(3), tim(2), tim(1)
   KX1SQ = KX1 * KX1
   KX2  = CMPLX (KX1SQ, 0.)
   CKX  = CMPLX (0., KX1)

   CALL EHFLD (NE,NZ,NPN,NPM,NEZ,NAIR,SXE,SXZ,MXVRTX,NTXE,COORD,KX1, &
               LNODS,F2,F4,F5,SRC1,SRC2,SRC3,SRC4,SRC5,ID,XX,SXDIP,  &
               SXAZ,SOURCE_TYPE,MXRS,NRS,YRS,WTRS,SNTX,CSTX,N_VRTX)

   ALLOCATE (EQRH1(NTXE,2*NPN),VECRV(NTXE,2*NPN),EQUAT(MFRON,2*NPN), &
             RR(NTXE,2*NPN),SR(NPSR))

   EQUAT = (0.,0.)
   FREQUENCY_LOOP: DO JF = 1,NFRQ
     FRQ = FREQ(JF)
     OMEGA = 2. * PI * FRQ
     JSOL = NFRQ * (IKX-1) + JF - 1
     IFIN = 100. * JSOL / REAL (NSOL)

! Initialize the frontal solution arrays

     EQRH1 = (0.,0.)
     RR    = (0.,0.)
     SR    = (0.,0.)

     KELV1 = 0
     ILM1  = 0
     DO I1 = 1, NE-1
       DO J1 = 1, NZ-1
         ILM  = (I1-1)*(NZ-1) + J1
         KLITH = LITH(I1,J1)
         SIGR = 1./ LYTH(KLITH,1)
         MU = MU0 * LYTH(KLITH,3)
         EPS = EPS0 * LYTH(KLITH,4)
         CALF = 1. - LYTH(KLITH,5)
         CTAU = LYTH(KLITH,6)
         CFRQ = LYTH(KLITH,7)

         SIG = CMPLX (SIGR, 0.)       !  sigr + iw eps0
         CC = (CI * OMEGA * CTAU)**CFRQ
         SIG = SIGR * (ONE + CC) / (ONE + CALF*CC)
         SIG = SIG + CI * OMEGA * EPS
         WMU = OMEGA * MU        !  wu
         CWMU = CI * WMU         !  iwu
         SFAC = CWMU / (4.*PI)   !  iwu / 4 PI

         DEN = KX2 + CWMU*SIG
         PP  = -CWMU / DEN
         QQ  = -CKX  / DEN
         SS  = -SIG  / DEN

! Calculate the Left-hand side of the element system matrix

         DO I = 1, 8
           DO J = 1, 8
             ER(2*I-1,2*J-1) =   PP*F1(I,J,ILM) - CWMU*F2(I,J,ILM)
             ER(2*I  ,2*J-1) =  -QQ* F3(I,J,ILM)
             ER(2*I  ,2*J  ) =   SS*F1(I,J,ILM) -  SIG*F2(I,J,ILM)
             ER(2*I-1,2*J  ) =   QQ* F3(I,J,ILM)
           END DO
         END DO

         DO I = 1, 16
           DO J = I, 16
             NLOC = NLOCA(NDEST(I,ILM),NDEST(J,ILM))
             SR(NLOC) = SR(NLOC) + ER(I,J)
           END DO
         END DO

! Calculate the Right-hand side of the element system matrix

         IF (J1 > NAIR)   THEN
           ILM1 = ILM1 + 1
           DO I = 1, 8
             IDEST = NDEST(2*I,ILM)
             DO ITX = 1, NTXE
                 RR(ITX,IDEST-1) = RR(ITX,IDEST-1) + SFAC*SIG/KX1* CI*PP*(SRC1(ITX,I,ILM1)+SRC4(ITX,I,ILM1))
                 RR(ITX,IDEST  ) = RR(ITX,IDEST  ) + SFAC*SIG/KX1*(CI*QQ*(SRC5(ITX,I,ILM1)+SRC2(ITX,I,ILM1)) &
                                                                  + CMPLX(SRC3(ITX,I,ILM1),0.))
             END DO
           END DO
         END IF

! Solve the equations using frontal solution method

         DO K = 1, 16
           IF (LOCEL(K,ILM) < 0)  THEN
             DO IFRON = 1, MFRON
               IF (NAC(IFRON,ILM) == -LOCEL(K,ILM))  THEN
                 KELV1 = KELV1 + 1
                 DO JFRON = 1, MFRON
                   J = NLOCA(IFRON,JFRON)
                   EQUAT(JFRON,KELV1) = SR(J) ; SR(J) = (0.,0.)
                 END DO
                 EQRH1(1:NTXE,KELV1) = RR(1:NTXE,IFRON) ; RR(1:NTXE,IFRON) = (0.,0.)

                 PIVOT = EQUAT(IFRON,KELV1)
                 EQUAT(IFRON,KELV1) = (0.,0.)
                 DO I = 1, MFRON
                   TMP = EQUAT(I,KELV1) / PIVOT
                   DO J = 1, I
                     II = NFUNC(I) + J
                     SR(II) = SR(II) - TMP*EQUAT(J,KELV1)
                   END DO
                   RR(1:NTXE,I) = RR(1:NTXE,I) -TMP*EQRH1(1:NTXE,KELV1)
                 END DO
                 EQUAT(IFRON,KELV1) = PIVOT
               END IF
             END DO
           END IF
         END DO
       END DO
     END DO

!  Backsubtitution using frontal solution method

     VECRV = (0.,0.)
     DO K = KELV1, 1, -1
       IFRON = IFRO1(K)
       PIVOT = EQUAT(IFRON,K)
       EQUAT(IFRON,K) = (0.,0.)
       DO ITX = 1, NTXE
          TMP = (0.,0.)
          DO J = 1, MFRON
             TMP = TMP + VECRV(ITX,J)*EQUAT(J,K)
          END DO
          VECRV(ITX,IFRON) = (EQRH1(ITX,K)-TMP) / PIVOT
          RR(ITX,NIKN1(K)) = VECRV(ITX,IFRON)
       END DO
     END DO

     IF (SURVEY_TYPE == 1 .OR. SURVEY_TYPE == 2) THEN
        DO ITX = 1, NTXE
           IF (SURVEY_TYPE == 1) JG1MAX = NRGTX(ITX)
           IF (SURVEY_TYPE == 2) JG1MAX = 1
           DO JG1 = 1, JG1MAX
              IF (SURVEY_TYPE == 1) JG = RGTXID(JG1,ITX)
              IF (SURVEY_TYPE == 2) JG = 1
              DO JR  = 1, NRX(JG)
                 IF (RX_TYPE(JG) == 1) THEN                        ! Magnetic dipole receivers
                    NRSR = 1
                 ELSE IF (RX_TYPE(JG) == 2) THEN                   ! Finite Loop receivers
                    NRSR = 9
                    XRL(1) = SUM (RXN(JR,JG,1:4)) / 4.
                    YRL(1) = SUM (RXE(JR,JG,1:4)) / 4.
                    DO JA = 1,4
                      XRL(JA+1) = (1.-SQRT(0.6)) * XRL(1) + SQRT(0.6) * RXN(JR,JG,JA)
                      YRL(JA+1) = (1.-SQRT(0.6)) * YRL(1) + SQRT(0.6) * RXE(JR,JG,JA)
                    END DO
                    DO JA = 2,5
                      JB = JA + 1
                      IF (JA == 5) JB = 2
                      XRL(JA+4) = (XRL(JA) + XRL(JB)) / 2.
                      YRL(JA+4) = (YRL(JA) + YRL(JB)) / 2.
                    END DO
                 ENDIF
                 DO LR = 1, NRSR
                    IF (RX_TYPE(JG) == 1) THEN
                       IF (SURVEY_TYPE == 1) THEN
                          RXELOC = RXE(JR,JG,1)
                          RXZLOC = RXZ(JR,JG,1)
                       ELSE IF (SURVEY_TYPE == 2) THEN
                          RXELOC = RXE(JR,ITX,1)
                          RXZLOC = RXZ(JR,ITX,1)
                       END IF
                    ELSE IF (RX_TYPE(JG) == 2) THEN
                       RXELOC = YRL(LR)
                       RXZLOC = 0.
                    ENDIF
                    DO I = 1, NE-1
                       IF ( (ELOC(I,1) <= RXELOC) .AND. (ELOC(I+1,1) > RXELOC)) EXIT
                    END DO
                    DO J = 1, NZ-1
                       IF ( (ZLOC(I,J+1) <= RXZLOC) .AND. (ZLOC(I,J) > RXZLOC)) EXIT
                    END DO
                    ILM = (I-1)*(NZ-1) + J
                    DO J = 1, 8
                      NPOS = ABS ( LNODS(ILM,J))
                      HXX(J) = RR(ITX,2*NPOS-1)
                      EXX(J) = RR(ITX,2*NPOS)
                    END DO
                    WRITE(NT1,8) (HXX(J),J=1,8), ikx,jf,itx,jr
                    WRITE(NT2,8) (EXX(J),J=1,8), ikx,jf,itx,jr
                 END DO
              END DO
           END DO
        END DO
     END IF
     IF (SURVEY_TYPE == 3 .OR. SURVEY_TYPE == 4) THEN
        DO ITX = 1, NTXE
           IF (SURVEY_TYPE == 3) JRMAX = 1
           IF (SURVEY_TYPE == 4) JRMAX = NRS(ITX)
           DO JR = 1, JRMAX
             IF (SURVEY_TYPE == 3) THEN                            ! Central    loops
                RXELOC = SUM(SXE(1:N_VRTX(ITX),ITX))/N_VRTX(ITX)
                DO I = 1, NE-1
                   IF ( (ELOC(I,1) <= RXELOC) .AND. (ELOC(I+1,1) > RXELOC)) EXIT
                END DO
                RXZLOC = ZLOC(I,NAIR+1)
             ELSE IF (SURVEY_TYPE == 4) THEN                       ! Coincident loops
                RXELOC = YRS(JR,1,ITX)
                DO I = 1, NE-1
                   IF ( (ELOC(I,1) <= RXELOC) .AND. (ELOC(I+1,1) > RXELOC)) EXIT
                END DO
                RXZLOC = ZLOC(I,NAIR+1)
             END IF
             DO I = 1, NE-1
                IF ( (ELOC(I,1) <=  RXELOC) .AND. (ELOC(I+1,1) >  RXELOC)) EXIT
             END DO
             DO J = 1, NZ-1
                IF ( (ZLOC(I,J+1) <= RXZLOC) .AND. (ZLOC(I,J) > RXZLOC)) EXIT
             END DO
             ILM = (I-1)*(NZ-1) + J
             DO J = 1, 8
               NPOS = ABS ( LNODS(ILM,J))
               HXX(J) = RR(ITX,2*NPOS-1)
             END DO
             WRITE(NT1,8) (HXX(J),J=1,8), ikx,jf,itx,jr
           END DO
        END DO
     END IF

   END DO FREQUENCY_LOOP
   DEALLOCATE ( RR, EQRH1, VECRV, EQUAT, SR )

 END DO KX_LOOP

 DO ILM = 1, (NE-1)*(NZ-1)
   LNODS(ILM,1:8) = ABS(LNODS(ILM,1:8))
 END DO

 DEALLOCATE ( NFUNC, NACVA, NAC, IFRO1, NIKN1, SRC1, SRC2, SRC3, SRC4, SRC5, ID )
 DEALLOCATE ( NDEST )

 1 FORMAT(4X,'Solving for wavenumber ',I3,' of ',I3, &
             ' at ', 2(i2.2, ':'), i2.2, ' on ', i2.2, '/', i2.2, '/', i4)
 2 FORMAT(6X,'Frequency',I3,' =',G12.4,F8.1,' percent done.')
 3 Format (/, 2x)
 8 FORMAT(16E14.6,'  !',4i5)

END SUBROUTINE FRONT

 SUBROUTINE HYEY (ND,NT1,NT2,NPN,NEZ,LNODS,COORD,NFRQ,FREQ,NKX,KX,NRXG,NRGTX,   &
                  RX_TYPE,RGTXID,NRX,MRX,MQVR,RXE,RXN,RXZ,MXVRTX,NTXE,SXE,SXN,  &
                  SXZ,NE,NZ,SOURCE_TYPE,N_VRTX,NLITH,LYTH,LITH,NPROP,ELOC,ZLOC, &
                  MXRS,NRS,XRS,YRS,WTRS,SURVEY_TYPE,SXDIP,SXAZ,NAIR)
!--------------------------------------------------------------------------------
!
! Purpose:  Subroutine HYEY calculates the inverse Fourier tranform
!           of Hx,Hy,Hz components from (X,Ky,Z) domain to the (X,Y,Z)
!           domain at the receiver positions and writes them to unit ND.
!
!**** Called by: ARJUNA_2D
!****      Uses: MODULE AG_Filter_coefficients
!
! Input:   ND      - I/O unit for writing out frequency domain magnetic fields
!          NT      - unit number for storing wave-domain results.
!          NPN     - (2*NE-1)*(2*NZ-1) - NEZ
!          NEZ     - total number of cells in mesh  (NE-1) * (NZ-1)
!          LNODS   - arrays of element node numbers.
!          COORD   - arrays of element coordinates.
!          FREQ    - array of NFRQ frequencies
!          NKX     - numbers of kx-values
!          KX      - wave number in North direction
!          NRX     - number of Tx-Rx offsets (= NFRQ for FD)
!       RXE(J,I,K) - local east coordinate of the Kth vertex of the
!                    Jth receiver of receiver group I
!       RXE(J,I,K) - local north coordinate of vertex K of Rx J of Group I
!       RXZ(J,I,K) - depth (z is positive down) of vertex K of Rx J of Group I
!                  - K = 1 only for mag dipole Rx; 1 to 2 for electric dipole Rx; 1 to 4 for loop Rx
!         SXE(I,J) - local east coordinate of vertex I for loop position J
!         SXN(I,J) - local coordinate of vertex I for loop position J
!         SXZ(I,J) - local (z positive down) depth of vertex I for loop position J
!           NTXE   - number of transmitter stations

 USE AG_Filter_coefficients

 IMPLICIT NONE
 REAL, PARAMETER :: X_OFFSET=.001, PI=3.141592654
 REAL, PARAMETER :: EPS0=8.854156E-12, MU0=12.56637E-7
 INTEGER, PARAMETER :: MAXRG=20 , RXGP=9
 COMPLEX, PARAMETER :: ONE=(1.0,0.), CI=(0.,1.0)
 INTEGER ND,NT1,NT2,NE,NZ,NPN,NEZ,NRXG,NTXE,NKX,NFRQ,ITX,IKX,I,J,I1,J1,JF,JR,LR, &
         LNODS(NEZ,8),NRGTX(NTXE),RGTXID(NRXG,NTXE),MRX,MXVRTX,MQVR,SOURCE_TYPE, &
         N_VRTX(NTXE),NV,SYMFAC,NLITH,LITH(NE,NZ),KLITH,JA,JB,NRSR,JG,JG1,ILM, &
         NPROP,IELEM,MXRS,NRS(NTXE),SURVEY_TYPE,JRMAX,NAIR,JG1MAX
 REAL SIGR,LYTH(NLITH+1,NPROP),OMEGA,EPS,CALF,CTAU,CFRQ,WMU
 REAL X1A,XS,CXR(4,NKX),CXI(4,NKX),CYR(4,NKX),CYI(4,NKX),CZR(4,NKX),CZI(4,NKX),   &
      KX(NKX),DPDX(8),DPDZ(8),SHP(8),COORD(NPN,2),HXRA,BRI(16),G(NLO:NHI),CUBVAL, &
      HXIA,HYRS,HYIS,HZRS,HZIS,FRQ,S,T,X,Y,Z,RHO,XFC,YFC,ZFC,ZLOC(NE,NZ),R3,    &
      FREQ(NFRQ),CRI(16),XX(NLO:NHI),ELX(8),ELZ(8),XRL(9),YRL(9),WRL(9),TRC,TRS,  &
      SN(MXVRTX),CS(MXVRTX),SL(MXVRTX),XCNTR,ZCNTR,PA,PB,SNTX,CSTX,SXDIP(NTXE),   &
      ELOC(NE,NZ),RXNLOC,RXELOC,RXZLOC,DIST2D,HTOTR,HTOTI,SNF,CSF,SXAZ(NTXE),YCNTR
 REAL C1(4,NKX),C2(4,NKX),C3(4,NKX),C4(4,NKX),C5(4,NKX),C6(4,NKX),H1(8),H2(8),H3(8),H4(8),H5(8),H6(8)
 COMPLEX QQ,SS,DEN,HXX,HYY,HZZ,CC,CWMU,SIG,SFAC,EXPR,EYPR,HDUM1,HDUM2
 INTEGER, DIMENSION (NRXG) :: NRX,RX_TYPE
 REAL,    DIMENSION (MXVRTX,NTXE) :: SXE, SXN, SXZ
 REAL,    DIMENSION (MRX,NRXG,MQVR) :: RXE, RXN, RXZ
 REAL,    DIMENSION (MXRS,1,NTXE) :: XRS,YRS,WTRS
 REAL,    ALLOCATABLE :: HXRR(:,:,:,:,:,:,:), HXRI(:,:,:,:,:,:,:), &
                         EXRR(:,:,:,:,:,:,:), EXRI(:,:,:,:,:,:,:)
 REAL,    ALLOCATABLE :: HXRRC(:,:,:,:,:), HXRIC(:,:,:,:,:)
 DOUBLE PRECISION DELTA

! Coordinates for cos- and sin-transform.

 DELTA = LOG (10.D0) / REAL (NDEC,8)
 DO I = NLO, NHI
    XX(I) = SNGL(EXP(I*DELTA))
 END DO

 IF (SURVEY_TYPE == 1 .OR. SURVEY_TYPE == 2) THEN
     ALLOCATE (HXRR(8,NFRQ,NKX,MRX,NTXE,MAXRG,RXGP),HXRI(8,NFRQ,NKX,MRX,NTXE,MAXRG,RXGP))
     ALLOCATE (EXRR(8,NFRQ,NKX,MRX,NTXE,MAXRG,RXGP),EXRI(8,NFRQ,NKX,MRX,NTXE,MAXRG,RXGP))
     REWIND (NT1)
     REWIND (NT2)
     DO IKX = 1, NKX
       DO JF = 1, NFRQ
         DO ITX = 1, NTXE
               IF (SURVEY_TYPE == 1) JG1MAX = NRGTX(ITX)
               IF (SURVEY_TYPE == 2) JG1MAX = 1
               DO JG1 = 1, JG1MAX
                  IF (SURVEY_TYPE == 1) JG = RGTXID(JG1,ITX)
                  IF (SURVEY_TYPE == 2) JG = 1
                  DO JR  = 1, NRX(JG)
                     IF (RX_TYPE(JG) == 1) NRSR = 1
                     IF (RX_TYPE(JG) == 2) NRSR = 9
                     DO LR = 1, NRSR
                        READ (NT1,*) BRI(1:16)
                        READ (NT2,*) CRI(1:16)
                        DO J1 = 1,8
                           HXRR(J1,JF,IKX,JR,ITX,JG1,LR) = BRI(2*J1-1)
                           HXRI(J1,JF,IKX,JR,ITX,JG1,LR) = BRI(2*J1)
                           EXRR(J1,JF,IKX,JR,ITX,JG1,LR) = CRI(2*J1-1)
                           EXRI(J1,JF,IKX,JR,ITX,JG1,LR) = CRI(2*J1)
                        END DO
                     END DO
                  END DO
               END DO
         END DO
       END DO
     END DO
 END IF
 IF (SURVEY_TYPE == 3 .OR. SURVEY_TYPE == 4) THEN
    ALLOCATE (HXRRC(8,NFRQ,NKX,100,NTXE),HXRIC(8,NFRQ,NKX,100,NTXE))
    REWIND (NT1)
    DO IKX = 1, NKX
       DO JF = 1, NFRQ
         DO ITX = 1, NTXE
           IF (SURVEY_TYPE == 3) JRMAX = 1
           IF (SURVEY_TYPE == 4) JRMAX = NRS(ITX)
           DO JR = 1, JRMAX
              READ (NT1,*) BRI(1:16)
              DO J1 = 1,8
                 HXRRC(J1,JF,IKX,JR,ITX) = BRI(2*J1-1)
                 HXRIC(J1,JF,IKX,JR,ITX) = BRI(2*J1)
              END DO
           END DO
         END DO
       END DO
    END DO
 END IF

 FREQUENCY_LOOP: DO JF = 1, NFRQ
   FRQ = FREQ(JF)
   OMEGA = 2. * PI * FRQ

   TX_LOOP: DO ITX = 1, NTXE
     IF (SURVEY_TYPE == 1 .OR. SURVEY_TYPE == 2) THEN
        IF (SURVEY_TYPE == 1) JG1MAX = NRGTX(ITX)
        IF (SURVEY_TYPE == 2) JG1MAX = 1
        DO JG1 = 1, JG1MAX
           IF (SURVEY_TYPE == 1) JG = RGTXID(JG1,ITX)
           IF (SURVEY_TYPE == 2) JG = 1
  RX_LOOP: DO JR  = 1, NRX(JG)
              IF (RX_TYPE(JG) == 1) THEN                        ! Magnetic dipole receivers
                 NRSR = 1
              ELSE IF (RX_TYPE(JG) == 2) THEN                   ! Finite Loop receivers
                 NRSR = 9
                 PA = 0.5* (DIST2D (RXN(JR,JG,1), RXE(JR,JG,1),  &
                                    RXN(JR,JG,2), RXE(JR,JG,2))  &
                           +DIST2D (RXN(JR,JG,3), RXE(JR,JG,3),  &
                                    RXN(JR,JG,4), RXE(JR,JG,4)))
                 PB = 0.5* (DIST2D (RXN(JR,JG,1), RXE(JR,JG,1),  &
                                    RXN(JR,JG,4), RXE(JR,JG,4))  &
                           +DIST2D (RXN(JR,JG,3), RXE(JR,JG,3),  &
                                    RXN(JR,JG,2), RXE(JR,JG,2)) )
                 XRL(1) = SUM (RXN(JR,JG,1:4)) / 4.
                 YRL(1) = SUM (RXE(JR,JG,1:4)) / 4.
                 WRL(1) = PA * PB * 16. / 81.
                 DO JA = 1,4
                   XRL(JA+1) = (1.-SQRT(.6))*XRL(1) + SQRT(.6)*RXN(JR,JG,JA)
                   YRL(JA+1) = (1.-SQRT(.6))*YRL(1) + SQRT(.6)*RXE(JR,JG,JA)
                   WRL(JA+1) = PA * PB * 25. / 324.
                 END DO
                 DO JA = 2,5
                   JB = JA + 1
                   IF (JA == 5) JB = 2
                   XRL(JA+4) = (XRL(JA) + XRL(JB)) / 2.
                   YRL(JA+4) = (YRL(JA) + YRL(JB)) / 2.
                   WRL(JA+4) = PA * PB * 10. / 81.
                 END DO
                 HDUM1 = (0.,0.) ; HDUM2 = (0.,0.) ; HTOTR = 0. ; HTOTI = 0.
              ENDIF
              DO LR = 1, NRSR
                 IF (RX_TYPE(JG) == 1) THEN
                    IF (SURVEY_TYPE == 1) THEN
                       RXNLOC = RXN(JR,JG,1)
                       RXELOC = RXE(JR,JG,1)
                       RXZLOC = RXZ(JR,JG,1)
                       YCNTR  = SUM(SXN(1:N_VRTX(ITX),ITX))/REAL(N_VRTX(ITX))
                    ELSE IF (SURVEY_TYPE == 2) THEN
                       RXNLOC = RXN(JR,ITX,1)
                       RXELOC = RXE(JR,ITX,1)
                       RXZLOC = RXZ(JR,ITX,1)
                       YCNTR  = SXN(1,ITX)
                    END IF
                 ELSE IF (RX_TYPE(JG) == 2) THEN
                    RXNLOC = XRL(LR)
                    RXELOC = YRL(LR)
                    RXZLOC = RXZ(JR,JG,1)
                    YCNTR  = SUM(SXN(1:N_VRTX(ITX),ITX))/REAL(N_VRTX(ITX))
                 ENDIF
                 DO I = 1, NE-1
                    IF ( (ELOC(I,1)  <=  RXELOC) .AND. (ELOC(I+1,1) >  RXELOC)) EXIT
                 END DO
                 DO J = 1, NZ-1
                    IF ( (ZLOC(I,J+1) <= RXZLOC) .AND. (ZLOC(I,J)   >  RXZLOC)) EXIT
                 END DO
                 IELEM = (I-1)*(NZ-1) + J
                 KLITH = LITH(I,J)
                 XFC = 0. ; YFC = 0. ; ZFC = 0.
                 IF (SOURCE_TYPE == 1) THEN
                   NV = N_VRTX(ITX)
                   DO I = 1, NV - 1
                      SL(I) = DIST2D(SXE(I,ITX),SXN(I,ITX),SXE(I+1,ITX),SXN(I+1,ITX))
                      SN(I) = (SXE(I+1,ITX) - SXE(I,ITX)) / SL(I)
                      CS(I) = (SXN(I+1,ITX) - SXN(I,ITX)) / SL(I)
                   END DO
                   SL(NV) =  DIST2D(SXE(NV,ITX), SXN(NV,ITX), SXE(1,ITX), SXN(1,ITX))
                   SN(NV) =  (SXE(1,ITX) - SXE(NV,ITX)) / SL(NV)
                   CS(NV) =  (SXN(1,ITX) - SXN(NV,ITX)) / SL(NV)
                   DO J = 1, NV
                      DO I = 0, IFIX(SL(J))
                         X =  RXNLOC - (SXN(J,ITX) + I*CS(J))
                         Y =  RXELOC - (SXE(J,ITX) + I*SN(J))
                         Z =  RXZLOC
                         RHO = MAX((X*X+Y*Y+Z*Z),.1) ; RHO = SQRT(RHO)
                         XFC = XFC -  Z*SN(J)          / (4.*PI*RHO*RHO*RHO)
                         YFC = YFC +  Z*CS(J)          / (4.*PI*RHO*RHO*RHO)
                         ZFC = ZFC - (X*SN(J)-Y*CS(J)) / (4.*PI*RHO*RHO*RHO)
                       END DO
                   END DO
                 ELSE IF(SOURCE_TYPE == 3) THEN
                    X =  RXNLOC - SXN(1,ITX)
                    Y =  RXELOC - SXE(1,ITX)
                    Z =  RXZLOC - SXZ(1,ITX)
                    RHO = MAX((X*X+Y*Y+Z*Z),.1) ; RHO = SQRT(RHO)
                    SNTX = SIN (SXDIP(ITX))
                    CSTX = COS (SXDIP(ITX))
                    SNF = 0. ; CSF = 1.
                    IF (SXAZ(ITX) < 0.) CSF = -1.
                    RHO = X*X+Y*Y+Z*Z ; RHO = SQRT(RHO)
                    XFC =   1./(4.*PI*RHO*RHO*RHO) * ( 3.*X*Z/(RHO*RHO)    *CSTX &
                                                   + SNTX*((3.*X*X/(RHO*RHO)-1.)*CSF - (3.*X*Y/(RHO*RHO)*SNF)))
                    YFC =  -1./(4.*PI*RHO*RHO*RHO) * ( 3.*Y*Z/(RHO*RHO)    *CSTX &
                                                   + SNTX*(3.*X*Y*CSF/(RHO*RHO)-(3.*Y*Y/(RHO*RHO)-1.)*SNF))
                    ZFC =   1./(4.*PI*RHO*RHO*RHO) * ((3.*Z*Z/(RHO*RHO)-1.)*CSTX &
                                                   + 3.*Z*SNTX*(X*CSF-Y*SNF)/(RHO*RHO))
                 END IF

                 SYMFAC = 1
                 IF ((RXNLOC-YCNTR) < 0) SYMFAC = -1
                 X1A = ABS (RXNLOC-YCNTR)
                 X1A = MAX (X1A, X_OFFSET)
                 ELX(1:8) =  COORD(LNODS(IELEM,1:8),1)
                 ELZ(1:8) =  COORD(LNODS(IELEM,1:8),2)
                 XCNTR = SUM (ELX(1:8)) / 8.
                 ZCNTR = SUM (ELZ(1:8)) / 8.
                 PA = .25 * (DIST2D(ELX(1),ELZ(1),ELX(3),ELZ(3)) &
                          +  DIST2D(ELX(5),ELZ(5),ELX(7),ELZ(7)))
                 PB = .25 * (DIST2D(ELX(1),ELZ(1),ELX(7),ELZ(7)) &
                         +  DIST2D(ELX(5),ELZ(5),ELX(3),ELZ(3)))
                 S =  (RXELOC - XCNTR) / ABS(PA)
                 T =  (RXZLOC - ZCNTR) / ABS(PB)
!                 write(*,'(2I5,6f10.4)') itx,jr,s,t,RXE(JR,ITX,1),ELX(1),RXZ(JR,ITX,1),ELZ(1)

                 CALL SHAPE (ELX,ELZ,S,T,SHP,DPDX,DPDZ)

                 CXR = 0. ; CXI = 0. ; CYR = 0. ; CYI = 0. ; CZR = 0. ; CZI = 0.
                 DO IKX = 1, NKX
                   HXX = (0.,0.) ; HYY = (0.,0.) ; HZZ = (0.,0.)
                   SIGR = 1./ LYTH(KLITH,1)
                   IF (SIGR < 1.E-7) THEN
                      DO I = 1, 8                                     ! Ground Case
                        HXX = HXX +             SHP(I)*CMPLX(HXRR(I,JF,IKX,JR,ITX,JG,LR),HXRI(I,JF,IKX,JR,ITX,JG,LR))
                        HYY = HYY + CI/KX(IKX)*DPDX(I)*CMPLX(HXRR(I,JF,IKX,JR,ITX,JG,LR),HXRI(I,JF,IKX,JR,ITX,JG,LR))
                        HZZ = HZZ + CI/KX(IKX)*DPDZ(I)*CMPLX(HXRR(I,JF,IKX,JR,ITX,JG,LR),HXRI(I,JF,IKX,JR,ITX,JG,LR))
                      END DO
                   ELSE                                               ! Down Hole Case
                      G = XX / KX(IKX)
                      EPS = EPS0 * LYTH(KLITH,4)
                      CALF = 1. - LYTH(KLITH,5)
                      CTAU = LYTH(KLITH,6)
                      CFRQ = LYTH(KLITH,7)
                      SIG = CMPLX (SIGR, 0.)       !  sigr + iw eps0
                      CC = (CI * OMEGA * CTAU)**CFRQ
                      SIG = SIGR * (ONE + CC) / (ONE + CALF*CC)
                      SIG = SIG + CI * OMEGA * EPS
                      WMU = OMEGA * MU0        !  wu
                      CWMU = CI * WMU          !  iwu
                      SFAC = CWMU / (4.*PI)    !  iwu / 4 PI
                      DEN = CMPLX(KX(IKX)*KX(IKX),0.) + CWMU*SIG
                      QQ  = CI*KX(IKX) / DEN
                      SS  = SIG  / DEN
                      IF (SOURCE_TYPE == 1) THEN
                         EXPR = (0.,0.) ; EYPR = (0.,0.)
                         DO I = 1, NRS(ITX)
                            Y  =  RXELOC - YRS(I,1,ITX)
                            Z  =  RXZLOC
                            TRC = 0. ; TRS = 0.
                            DO J = NLO, NHI
                               RHO = G(J)*G(J)+Y*Y+Z*Z ; R3 = RHO*SQRT(RHO)
                               TRS = TRS + WSIN1(J)/R3 * G(J)
                               TRC = TRC + WCOS1(J)/R3
                            END DO
                            DO I1 = 1, NE-1
                               IF ( (ELOC(I1,1) <= YRS(I,1,ITX)) .AND. &
                                    (ELOC(I1+1,1) > YRS(I,1,ITX)) ) EXIT
                            END DO
                            ILM = (I1-1)*(NZ-1) + NAIR + 1
                            RHO = SQRT((COORD(LNODS(ILM,2),2)-COORD(LNODS(ILM,1),2))**2 + &
                                       (COORD(LNODS(ILM,2),1)-COORD(LNODS(ILM,1),1))**2)
                            SNTX = ABS(COORD(LNODS(ILM,2),2)-COORD(LNODS(ILM,1),2)) / RHO
                            CSTX = ABS(COORD(LNODS(ILM,2),1)-COORD(LNODS(ILM,1),1)) / RHO
                            EXPR = EXPR + WTRS(I,1,ITX) * SFAC*CMPLX(0.,TRS)*CSTX/KX(IKX)
                            EYPR = EYPR + WTRS(I,1,ITX) * SFAC*CMPLX(TRC,0.)*CSTX*Y/KX(IKX)
                         END DO
                         HZZ = QQ * SIG * EXPR
                      ELSE IF(SOURCE_TYPE == 3) THEN
                         Y =  RXELOC - SXE(1,ITX)
                         Z =  RXZLOC - SXZ(1,ITX)
                         TRC = 0. ; TRS = 0.
                         DO J = NLO, NHI
                            RHO = G(J)*G(J)+Y*Y+Z*Z ; R3 = RHO*SQRT(RHO)
                            TRS = TRS + WSIN1(J)/R3 * G(J)
                            TRC = TRC + WCOS1(J)/R3
                         END DO
                         SNTX = SIN (SXDIP(ITX))
                         CSTX = COS (SXDIP(ITX))
                         CSF = 0. ; SNF = 1.
                         IF (SXAZ(ITX) < 0.) SNF = -1.

                         EXPR = SFAC * (Z*CMPLX(TRC,0.)*CSF*SNTX + CMPLX(0.,TRS)*CSTX)  / KX(IKX)
                         EYPR = SFAC * (CMPLX(TRC,0.)*(Y*CSTX - Z*SNTX*CSF))            / KX(IKX)
                         HZZ =  QQ * SIG * EXPR
                      END IF

                      DO I = 1, 8
                         HXX = HXX - SHP(I) * CMPLX(HXRR(I,JF,IKX,JR,ITX,JG,1),HXRI(I,JF,IKX,JR,ITX,JG,1))
                         HYY = HYY +  QQ * DPDX(I)*CMPLX(HXRR(I,JF,IKX,JR,ITX,JG,1),HXRI(I,JF,IKX,JR,ITX,JG,1)) &
                                   -  SS * DPDZ(I)*CMPLX(EXRR(I,JF,IKX,JR,ITX,JG,1),EXRI(I,JF,IKX,JR,ITX,JG,1))
                         HZZ = HZZ + (SS * DPDX(I)*CMPLX(EXRR(I,JF,IKX,JR,ITX,JG,1),EXRI(I,JF,IKX,JR,ITX,JG,1)) &
                                   +  QQ * DPDZ(I)*CMPLX(HXRR(I,JF,IKX,JR,ITX,JG,1),HXRI(I,JF,IKX,JR,ITX,JG,1)))
                      END DO
                   END IF
                   CXR(1,IKX) =  -REAL(HXX)
                   CXI(1,IKX) = -AIMAG(HXX)
                   CYR(1,IKX) =  -REAL(HYY)
                   CYI(1,IKX) = -AIMAG(HYY)
                   CZR(1,IKX) =   REAL(HZZ)
                   CZI(1,IKX) =  AIMAG(HZZ)
                 END DO
                 CALL CUBSPL (KX,CXR,NKX,1,1) ; CALL CUBSPL (KX,CXI,NKX,1,1)
                 CALL CUBSPL (KX,CYR,NKX,1,1) ; CALL CUBSPL (KX,CYI,NKX,1,1)
                 CALL CUBSPL (KX,CZR,NKX,1,1) ; CALL CUBSPL (KX,CZI,NKX,1,1)

                 HXRA = 0. ; HXIA = 0. ; HYRS = 0. ; HYIS = 0. ; HZRS = 0. ; HZIS = 0.
                 DO I = NLO, NHI
                   XS = XX(I) / X1A
                   IF ( (XS >= KX(1)) .AND. (XS <= KX(NKX)) ) THEN
                     HXRA = HXRA + CUBVAL(KX, CXI, NKX, XS) * WSIN1(I)
                     HXIA = HXIA + CUBVAL(KX, CXR, NKX, XS) * WSIN1(I)
                     HYRS = HYRS + CUBVAL(KX, CYR, NKX, XS) * WCOS1(I)
                     HYIS = HYIS + CUBVAL(KX, CYI, NKX, XS) * WCOS1(I)
                     HZRS = HZRS + CUBVAL(KX, CZR, NKX, XS) * WCOS1(I)
                     HZIS = HZIS + CUBVAL(KX, CZI, NKX, XS) * WCOS1(I)
                   END IF
                 END DO
                 HYRS =  HYRS / X1A           + YFC
                 HYIS =  HYIS / X1A
                 HZRS =  HZRS / X1A           + ZFC
                 HZIS =  HZIS / X1A
                 HXRA =  HXRA / X1A * SYMFAC  + XFC
                 HXIA =  HXIA / X1A * SYMFAC
                 IF (RX_TYPE(JG) == 1) THEN
                    WRITE(ND,'(6E16.7,1X,4F10.1,e16.7)') HXRA,HXIA,HYRS,HYIS,HZRS,HZIS, &
                                         RXELOC,RXZLOC,SXE(1,ITX),SXZ(1,ITX),FRQ
                 ELSE IF (RX_TYPE(JG) == 2) THEN
                    HTOTR =  HTOTR + WRL(LR)*(HXRA+HYRS+HZRS)
                    HTOTI =  HTOTI + WRL(LR)*(HXIA+HYIS+HZIS)
                 END IF
              END DO
              IF (RX_TYPE(JG) == 2) THEN
                 WRITE(ND,'(6E16.7,1X,4F10.1,e16.7)') HTOTR,HTOTI,HDUM1,HDUM2,RXELOC,RXZLOC, &
                                                SXE(1,ITX),SXZ(1,ITX),FRQ
              END IF
           END DO RX_LOOP
        END DO
     ELSE IF (SURVEY_TYPE == 3 .OR. SURVEY_TYPE == 4) THEN
        HDUM1 = (0.,0.) ; HDUM2 = (0.,0.) ; HTOTR = 0. ; HTOTI = 0.
        IF (SURVEY_TYPE == 3) JRMAX = 1
        IF (SURVEY_TYPE == 4) JRMAX = NRS(ITX)
        YCNTR = SUM (SXN(1:N_VRTX(ITX),ITX)) / REAL(N_VRTX(ITX))
        DO JR = 1, JRMAX
           IF (SURVEY_TYPE == 3) THEN                            ! Central    loops
              RXNLOC = SUM(SXN(1:N_VRTX(ITX),ITX))/N_VRTX(ITX)
              RXELOC = SUM(SXE(1:N_VRTX(ITX),ITX))/N_VRTX(ITX)
           ELSE IF (SURVEY_TYPE == 4) THEN                       ! Coincident loops
              RXNLOC = XRS(JR,1,ITX)
              RXELOC = YRS(JR,1,ITX)
           END IF
           DO I = 1, NE-1
              IF ((ELOC(I,NAIR+1) <= RXELOC) .AND. (ELOC(I+1,NAIR+1) > RXELOC)) EXIT
           END DO
           IELEM  = (I-1)*(NZ-1) + NAIR
           SIG = CMPLX (1./ LYTH(LITH(I,NAIR),1), 0.)
           RXZLOC = ZLOC(I,NAIR+1)

           XFC = 0. ; YFC = 0. ; ZFC = 0.
           NV = N_VRTX(ITX)
           DO I = 1, NV - 1
              SL(I) = DIST2D(SXE(I,ITX),SXN(I,ITX),SXE(I+1,ITX),SXN(I+1,ITX))
              SN(I) = (SXE(I+1,ITX) - SXE(I,ITX)) / SL(I)
              CS(I) = (SXN(I+1,ITX) - SXN(I,ITX)) / SL(I)
           END DO
           SL(NV) = DIST2D(SXE(NV,ITX), SXN(NV,ITX), SXE(1,ITX), SXN(1,ITX))
           SN(NV) = (SXE(1,ITX) - SXE(NV,ITX)) / SL(NV)
           CS(NV) = (SXN(1,ITX) - SXN(NV,ITX)) / SL(NV)
           DO J = 1, NV
              DO I = 0, IFIX(SL(J))
                 X = RXNLOC - (SXN(J,ITX) + I*CS(J))
                 Y = RXELOC - (SXE(J,ITX) + I*SN(J))
                 Z = 0.
                 RHO = MAX((X*X+Y*Y+Z*Z),.1) ; RHO = SQRT(RHO)
                 XFC = XFC -  Z*SN(J)          / (4.*PI*RHO*RHO*RHO)
                 YFC = YFC +  Z*CS(J)          / (4.*PI*RHO*RHO*RHO)
                 ZFC = ZFC - (X*SN(J)-Y*CS(J)) / (4.*PI*RHO*RHO*RHO)
               END DO
           END DO

           X1A = ABS (RXNLOC-YCNTR)
           X1A = MAX (X1A, X_OFFSET)
           ELX(1:8) = COORD(LNODS(IELEM,1:8),1)
           ELZ(1:8) = COORD(LNODS(IELEM,1:8),2)
           XCNTR = SUM (ELX(1:8)) / 8.
           ZCNTR = SUM (ELZ(1:8)) / 8.
           PA = .25 * (DIST2D(ELX(1),ELZ(1),ELX(3),ELZ(3))+DIST2D(ELX(5),ELZ(5),ELX(7),ELZ(7)))
           PB = .25 * (DIST2D(ELX(1),ELZ(1),ELX(7),ELZ(7))+DIST2D(ELX(5),ELZ(5),ELX(3),ELZ(3)))
           S = (RXELOC - XCNTR) / ABS(PA)
           T = (RXZLOC - ZCNTR) / ABS(PB)
!           write(*,'(2I5,6f10.4)') itx,jr,s,t,RXELOC,ELX(1),RXZLOC,ELZ(1)

           CALL SHAPE (ELX,ELZ,S,T,SHP,DPDX,DPDZ)

           H1 = 0. ; H2 = 0. ; H3 = 0. ; H4 = 0. ; H5 = 0. ; H6 = 0.
           DO I = 1, 8
             C1 = 0. ; C2 = 0. ; C3 = 0. ; C4 = 0. ; C5 = 0. ; C6 = 0.
             DO IKX = 1, NKX
               QQ  = CI*KX(IKX) / (CMPLX(KX(IKX)*KX(IKX),0.)+CI*OMEGA*MU0*SIG)
               HXX =            CMPLX(HXRRC(I,JF,IKX,JR,ITX),HXRIC(I,JF,IKX,JR,ITX))
!               HYY = QQ * CMPLX(HXRRC(I,JF,IKX,JR,ITX),HXRIC(I,JF,IKX,JR,ITX))
!               HZZ = QQ * CMPLX(HXRRC(I,JF,IKX,JR,ITX),HXRIC(I,JF,IKX,JR,ITX))
               HYY = CI/KX(IKX) * CMPLX(HXRRC(I,JF,IKX,JR,ITX),HXRIC(I,JF,IKX,JR,ITX))
               HZZ = CI/KX(IKX) * CMPLX(HXRRC(I,JF,IKX,JR,ITX),HXRIC(I,JF,IKX,JR,ITX))
               C1(1,IKX) =  -REAL(HXX)
               C2(1,IKX) = -AIMAG(HXX)
               C3(1,IKX) = -REAL(HYY)
               C4(1,IKX) = -AIMAG(HYY)
               C5(1,IKX) =  REAL(HZZ)
               C6(1,IKX) =  AIMAG(HZZ)
             END DO
             CALL CUBSPL (KX,C1,NKX,1,1) ; CALL CUBSPL (KX,C2,NKX,1,1)
             CALL CUBSPL (KX,C3,NKX,1,1) ; CALL CUBSPL (KX,C4,NKX,1,1)
             CALL CUBSPL (KX,C5,NKX,1,1) ; CALL CUBSPL (KX,C6,NKX,1,1)
             DO I1 = NLO, NHI
               XS = XX(I1) / X1A
               IF ( (XS >= KX(1)) .AND. (XS <= KX(NKX)) ) THEN
                 H1(I) = H1(I) + CUBVAL(KX, C2, NKX, XS) * WSIN1(I1) / X1A
                 H2(I) = H2(I) + CUBVAL(KX, C1, NKX, XS) * WSIN1(I1) / X1A
                 H3(I) = H3(I) + CUBVAL(KX, C3, NKX, XS) * WCOS1(I1) / X1A
                 H4(I) = H4(I) + CUBVAL(KX, C4, NKX, XS) * WCOS1(I1) / X1A
                 H5(I) = H5(I) + CUBVAL(KX, C5, NKX, XS) * WCOS1(I1) / X1A
                 H6(I) = H6(I) + CUBVAL(KX, C6, NKX, XS) * WCOS1(I1) / X1A
               END IF
             END DO
           END DO
           HXRA = 0. ; HXIA = 0. ; HYRS = 0. ; HYIS = 0. ; HZRS = 0. ; HZIS = 0.
           DO I = 1, 8
              HXRA = HXRA + SHP(I)  * H1(I)
              HXIA = HXIA + SHP(I)  * H2(I)
              HYRS = HYRS + DPDX(I) * H3(I)
              HYIS = HYIS + DPDX(I) * H4(I)
              HZRS = HZRS + DPDZ(I) * H5(I)
              HZIS = HZIS + DPDZ(I) * H6(I)
           END DO
           HXRA = HXRA + XFC
           HXIA = HXIA
           HYRS = HYRS + YFC
           HYIS = HYIS
           HZRS = HZRS + ZFC
           HZIS = HZIS
           IF (SURVEY_TYPE == 3) THEN
              WRITE(ND,'(6E16.7,1X,4F10.1,e16.7)') HXRA,HXIA,HYRS,HYIS,HZRS,HZIS, &
                                    RXELOC,RXZLOC,SXE(1,ITX),SXZ(1,ITX),FRQ
           ELSE IF (SURVEY_TYPE == 4) THEN
              HTOTR = HTOTR + WTRS(JR,1,ITX)*(HXRA+HYRS+HZRS)
!              HTOTI = HTOTI + WTRS(JR,1,ITX)*(HXIA+HYIS+HZIS)
              HTOTI = HTOTI + WTRS(JR,1,ITX)*HZIS
           ENDIF
        END DO
        IF (SURVEY_TYPE == 4) THEN
           WRITE(ND,'(6E16.7,1X,4F10.1,e16.7)') HTOTR,HTOTI,HDUM1,HDUM2,RXELOC,RXZLOC, &
                                          SXE(1,ITX),SXZ(1,ITX),FRQ
        END IF
     END IF
   END DO TX_LOOP
 END DO FREQUENCY_LOOP

 IF (SURVEY_TYPE == 1 .OR. SURVEY_TYPE == 2) THEN
    DEALLOCATE ( HXRR, HXRI, EXRR, EXRI )
 ELSE IF (SURVEY_TYPE == 3 .OR. SURVEY_TYPE == 4) THEN
    DEALLOCATE ( HXRRC, HXRIC)
 END IF

END SUBROUTINE HYEY

 SUBROUTINE SHAPEF (NLG,COORD,LNODS,NEZ,NPN,NE,NZ,F1,F2,F3,F4,F5)
!-----------------------------------------------------------------

! Purpose : Subroutine SHAPEF calculates the isoparametric shape functions.

!**** Called by: ARJUNA_2D

!  Input:   NLG - log fle unit number
!           COORD - element coordinates array
!           LNODS - element node number array
!           NEZ   - maximum allowable number of elements
!           NPN   - dimension of coord
!           NE,NZ - number of nodes in X- and Z-direction

! Output  : F1,F2,F3,F4,F5 - Derivative of the shape function arrays


 IMPLICIT NONE

 INTEGER NLG,NE,NZ,NEZ,NPN,I,J,I1,J1,IG,JG,ILM,LNODS(NEZ,8)
 REAL COORD(NPN,2),GS(3),GT(3),WG(3),S,T,DET,WEIGHT,DXDS,DZDS,DXDT,DZDT
 REAL, DIMENSION(8) :: SHP,DPDS,DPDT,DPDX,DPDZ,S0,T0
 REAL, DIMENSION(8,8,NEZ) :: F1,F2,F3,F4,F5
 DATA S0/-1., 0.,+1.,+1.,+1., 0.,-1.,-1./
 DATA T0/+1.,+1.,+1., 0.,-1.,-1.,-1., 0./
 DATA GS(1),GS(2),GS(3)/-0.774596669241483,0.,+0.774596669241483/
 DATA GT(1),GT(2),GT(3)/+0.774596669241483,0.,-0.774596669241483/
 DATA WG(1),WG(2),WG(3)/0.555555555555555,0.888888888888888,0.555555555555555/

 INTENT (IN)  NLG,COORD,LNODS,NEZ,NPN,NE,NZ
 INTENT (OUT) F1,F2,F3,F4,F5

 F1 = 0. ; F2 = 0. ; F3 = 0. ; F4 = 0. ; F5 = 0.
 DO I1 = 1, NE-1
   DO J1 = 1, NZ-1
     ILM = (I1-1)*(NZ-1) + J1

     DO IG = 1, 3
       DO JG = 1, 3
         S = GS(IG) ; T = GT(JG)
         WEIGHT = WG(IG) * WG(JG)

         SHP(1) = .25 *(1.+S0(1)*S)*(1.+T0(1)*T)*(S0(1)*S+T0(1)*T-1.)
         SHP(3) = .25 *(1.+S0(3)*S)*(1.+T0(3)*T)*(S0(3)*S+T0(3)*T-1.)
         SHP(5) = .25 *(1.+S0(5)*S)*(1.+T0(5)*T)*(S0(5)*S+T0(5)*T-1.)
         SHP(7) = .25 *(1.+S0(7)*S)*(1.+T0(7)*T)*(S0(7)*S+T0(7)*T-1.)
         SHP(2) = .50 * (1. - S*S) * (1. + T0(2)*T)
         SHP(4) = .50 * (1. - T*T) * (1. + S0(4)*S)
         SHP(6) = .50 * (1. - S*S) * (1. + T0(6)*T)
         SHP(8) = .50 * (1. - T*T) * (1. + S0(8)*S)

         DPDS(1) = .25 * (1. + T0(1)*T) * (2.*S + S0(1)*T0(1)*T)
         DPDS(3) = .25 * (1. + T0(3)*T) * (2.*S + S0(3)*T0(3)*T)
         DPDS(5) = .25 * (1. + T0(5)*T) * (2.*S + S0(5)*T0(5)*T)
         DPDS(7) = .25 * (1. + T0(7)*T) * (2.*S + S0(7)*T0(7)*T)
         DPDS(2) =  -S * (1. + T0(2)*T)
         DPDS(4) = .50 * (1. - T*T) * S0(4)
         DPDS(6) =  -S * (1. + T0(6)*T)
         DPDS(8) = .50 * (1. - T*T) * S0(8)

         DPDT(1) = .25 * (1. + S0(1)*S) * (2.*T + T0(1)*S0(1)*S)
         DPDT(3) = .25 * (1. + S0(3)*S) * (2.*T + T0(3)*S0(3)*S)
         DPDT(5) = .25 * (1. + S0(5)*S) * (2.*T + T0(5)*S0(5)*S)
         DPDT(7) = .25 * (1. + S0(7)*S) * (2.*T + T0(7)*S0(7)*S)
         DPDT(2) = .50 * (1. - S*S) * T0(2)
         DPDT(4) =  -T * (1. + S0(4)*S)
         DPDT(6) = .50 * (1. - S*S) * T0(6)
         DPDT(8) =  -T * (1. + S0(8)*S)

         DXDS = 0. ; DZDS = 0. ; DXDT = 0. ; DZDT = 0.
         DO I = 1, 8
            DXDS = DXDS + DPDS(I) * COORD(ABS(LNODS(ILM,I)),1)
            DXDT = DXDT + DPDT(I) * COORD(ABS(LNODS(ILM,I)),1)
            DZDS = DZDS + DPDS(I) * COORD(ABS(LNODS(ILM,I)),2)
            DZDT = DZDT + DPDT(I) * COORD(ABS(LNODS(ILM,I)),2)
         END DO
         DET = DXDS*DZDT - DZDS*DXDT

         IF (ABS(DET) < 1.E-10)  THEN
            WRITE(*,1) ILM,(COORD(ILM,J),J=1,2)
            WRITE(NLG,1) ILM,(COORD(ILM,J),J=1,2)
            STOP
         END IF

         DPDX(1:8) = ( DZDT*DPDS(1:8) - DZDS*DPDT(1:8)) / DET
         DPDZ(1:8) = (-DXDT*DPDS(1:8) + DXDS*DPDT(1:8)) / DET

         DO I = 1, 8
            DO J = 1, 8
               F1(I,J,ILM) = F1(I,J,ILM) + WEIGHT*DET*( DPDX(I)*DPDX(J)+DPDZ(I)*DPDZ(J))
               F2(I,J,ILM) = F2(I,J,ILM) + WEIGHT*DET*  SHP(I) * SHP(J)
               F3(I,J,ILM) = F3(I,J,ILM) + WEIGHT*DET*(-DPDX(I)*DPDZ(J)+DPDZ(I)*DPDX(J))
               F4(I,J,ILM) = F4(I,J,ILM) + WEIGHT*DET*  SHP(I) * DPDX(J)
               F5(I,J,ILM) = F5(I,J,ILM) + WEIGHT*DET*  SHP(I) * DPDZ(J)
           END DO
         END DO
       END DO
     END DO
   END DO
 END DO

1 FORMAT(//T3,'MESH ERROR, DET=0. AT ELEMEN',I8,'X=',E12.4,'Z=',E12.4 &
          /T3,'EXECUTION HALTED')

END SUBROUTINE SHAPEF

 SUBROUTINE SHAPE (ELX,ELZ,S,T,SHP,DPDX,DPDZ)
!--------------------------------------------

! Purpose : Subroutine SHAPE calculates the isoparametric shape functions.

!  Input:   ELX,ELZ - element coordinates array

! Output  : Derivative of the shape function arrays


 IMPLICIT NONE

 INTEGER I
 REAL  S,T,DET,DXDS,DZDS,DXDT,DZDT,ELX(8),ELZ(8)
 REAL, DIMENSION(8) :: SHP,DPDS,DPDT,DPDX,DPDZ,S0,T0
 DATA S0/-1., 0.,+1.,+1.,+1., 0.,-1.,-1./
 DATA T0/+1.,+1.,+1., 0.,-1.,-1.,-1., 0./


 SHP(1) = .25 *(1.+S0(1)*S)*(1.+T0(1)*T)*(S0(1)*S+T0(1)*T-1.)
 SHP(3) = .25 *(1.+S0(3)*S)*(1.+T0(3)*T)*(S0(3)*S+T0(3)*T-1.)
 SHP(5) = .25 *(1.+S0(5)*S)*(1.+T0(5)*T)*(S0(5)*S+T0(5)*T-1.)
 SHP(7) = .25 *(1.+S0(7)*S)*(1.+T0(7)*T)*(S0(7)*S+T0(7)*T-1.)
 SHP(2) = .50 * (1. - S*S) * (1. + T0(2)*T)
 SHP(4) = .50 * (1. - T*T) * (1. + S0(4)*S)
 SHP(6) = .50 * (1. - S*S) * (1. + T0(6)*T)
 SHP(8) = .50 * (1. - T*T) * (1. + S0(8)*S)

 DPDS(1) = .25 * (1. + T0(1)*T) * (2.*S + S0(1)*T0(1)*T)
 DPDS(3) = .25 * (1. + T0(3)*T) * (2.*S + S0(3)*T0(3)*T)
 DPDS(5) = .25 * (1. + T0(5)*T) * (2.*S + S0(5)*T0(5)*T)
 DPDS(7) = .25 * (1. + T0(7)*T) * (2.*S + S0(7)*T0(7)*T)
 DPDS(2) =  -S * (1. + T0(2)*T)
 DPDS(4) = .50 * (1. - T*T) * S0(4)
 DPDS(6) =  -S * (1. + T0(6)*T)
 DPDS(8) = .50 * (1. - T*T) * S0(8)

 DPDT(1) = .25 * (1. + S0(1)*S) * (2.*T + T0(1)*S0(1)*S)
 DPDT(3) = .25 * (1. + S0(3)*S) * (2.*T + T0(3)*S0(3)*S)
 DPDT(5) = .25 * (1. + S0(5)*S) * (2.*T + T0(5)*S0(5)*S)
 DPDT(7) = .25 * (1. + S0(7)*S) * (2.*T + T0(7)*S0(7)*S)
 DPDT(2) = .50 * (1. - S*S) * T0(2)
 DPDT(4) =  -T * (1. + S0(4)*S)
 DPDT(6) = .50 * (1. - S*S) * T0(6)
 DPDT(8) =  -T * (1. + S0(8)*S)

 DXDS = 0. ; DZDS = 0. ; DXDT = 0. ; DZDT = 0.
 DO I = 1, 8
    DXDS = DXDS + DPDS(I) * ELX(I)
    DXDT = DXDT + DPDT(I) * ELX(I)
    DZDS = DZDS + DPDS(I) * ELZ(I)
    DZDT = DZDT + DPDT(I) * ELZ(I)
 END DO
 DET = DXDS*DZDT - DZDS*DXDT

 DPDX(1:8) = ( DZDT*DPDS(1:8) - DZDS*DPDT(1:8)) / DET
 DPDZ(1:8) = (-DXDT*DPDS(1:8) + DXDS*DPDT(1:8)) / DET


END SUBROUTINE SHAPE

