!   PROGRAM Beowulf
!------------------------------------------------------------------------------------------------------
 Module BG_Metadata
!--------------
! 
! Module designed to hold 'constant character options'
    Implicit NONE

    Character (Len = 40), Parameter :: PNAME = 'Beowulf'
    Character (Len = 40), Parameter :: PVERS = '4.7.2'
    Character (Len = 40), Parameter :: PDATE = '02 February, 2021'
    Character (Len = 40), Parameter :: PAUT1 = 'CSIRO Electromagnetic Modelling Group'
    Character (Len = 40), Parameter :: PAUT2 = 'Art Raiche, Fred Sugeng and Glenn Wilson'
    Character (Len = 40), Parameter :: PAUT3 = 'David Annetts (david.annetts@csiro.au)'
    Character (Len = 40), Parameter :: PPROJ = 'AMIRA P223F'
    Character (Len = 80), Parameter :: PRELS = 'GPL-2.0 (Full text at https://opensource.org/licenses/gpl-2.0.php)'

End Module BG_Metadata
!------------------------------------------------------------------------------------------------------
!
!            2007 P223F Software Contact:
!
!            Dr. Art Raiche                  phone: +61(02) 9498 8392
!            P.O. Box 3022                  mobile: 0422 489 577
!            Lindfield West, NSW            e-mail: art.raiche@optusnet.com.au
!            Australia 2070
!
!
!   Important note: the original P223 release of Beowulf in 2010 used a six-element
!                   lithology table.  This was completely incompatible with other 
!                   P223 codes.  The seven-element lithology table that is used
!                   by other P223 codes has been restored for this release to 
!                   maintain compatibility with other P223 codes. (DWA)
!
!   20170704 DWA: turned on RXMNT
!
! 1.0.7
!   1.  DWA: modified screen output during run for clarity & brevity
!
! 1.0.5
!   1.  DWA: reinstated SVD analysis
!   2.  DWA: output field weighting to MV1 & OUT
!
! 1.0.6
!   1.  DWA reinstated rxmnt
!
!--------------------------------------------------------------------------------------------------
!================
!   DESCRIPTION |
!================
!
!  Beowulf is a 1D layered earth inversion program that can be used to invert
!  time or frequency-domain data from almost any extant ground, downhole or
!  bore hole controlled source system.  For 1D MT inversion, or, to generate 
!  lauered earth controlled source data, use Leroi and set NPLT = 0.  
!
!  Beowulf can be used for magnetic dipole, closed loop or dipole transmitters.  
!  It can be used for magnetic dipole, closed loop or dipole receivers.  
!  The vertices of closed and open loop transmitters and receivers must have 
!  the same elevation (or depth) but they and magnetic dipole transmitters 
!  can be on, above or below the surface in any layer including basement.
!
!  There are basically two source-receiver specification modes:
!  independently specified transmitter and receiver groupings or
!  fixed offset surveys.

!  All transmitters in a survey must be of the same type: either, open or
!  closed loops or magnetic dipoles.
!
!  Regardless of transmitter type, all types of receivers can be used in the same
!  survey as long as each group has only one type of receiver.
!
!====================
!   FILE CONVENTIONS
!====================
!
!   INPUT FILES:
!   -----------
!
!   The input control file, named Beowulf.cfl is read
!   from logical unit number NR = 3.
!
!   For inversion the data to be inverted must be contained in
!   Beowulf.inv, read from logical unit NRI = 13
!
!
!   VERBOSE-OUTPUT FILES:
!   --------------------
!
!   The Beowulf.out file is written to logical unit NW = 4
!
!   Messages about data or runtime errors are written in a file
!   called Beowulf.log on logical unit NLG = 9
!
!
!   OUTPUT FILES FOR PLOTTING:
!   --------------------------
!
!   Terse inversion output for plotting is written to
!   logical unit NI = 14
!
!   NMD = 15 contains a very terse file containing resistivities & thicknesses
!            as a function of position.  The resistivities for all layers,
!            starting from the top to bottom are written out followed by
!            the thicknesses starting from the top to bottom.
!
!
!    UNIT #   UNIT ID      FILE ID        Function
!    ------   -------      -------        --------
!       3       NR       Beowulf.cfl    Input control file
!       4       NW       Beowulf.out    Verbose output data
!       9       NLG      Beowulf.log    Data error messages
!      13       NRI      Beowulf.inv    Data to be inverted
!      14       NP      Beowulf.mv1    Inversion plotting output
!      15       NR1      Beowulf.res    Resistivities + layer depths & thicknesses
!
!----------------------------------------------------------------
!
!   Survey Geometry Conventions
!   --------------------------
!
!   Except for magnetotellurics, there are basically two options: fixed loop
!   with one or more receiver groups and generalised slingram: one or more
!   magnetic dipole receivers moving at fixed offset(s) with respect to a
!   moving transmitter (loop or magnetic dipole).  Transmitter and receiver
!   coordinates are specified in terms of Easting, Northing and RLs (relative
!   level) that increases negatively downwards.
!
!   For generalised slingram, there is an option to specify only the
!   coordinates of the first transmitter position and then the others are
!   determined by loop interval and survey azimuth.
!
!   The survey azimuth is defined as 0 degrees pointing north and positive
!   clockwise.  For surface lines, the X (radial) component will point along
!   the survey azimuth.  The Y (tangential) component will be the horizontal
!   transverse component.  The Z component is vertical.
!
!   For downhole surveys, the U-V-A system will be used.  A is the axial
!   component, V (horizontal) is the horizontal component perpendicular to the
!   receiver azimuth and U (slope) is the component (perpendicular to the
!   Axial component) in the vertical plane determined by the receiver azimuth.
!   For a vertical hole (DIP = 0) U points along the survey azimuth.
!
!------------------------------------------------------------------------
!
!   Time-domain waveform sign convention
!   ------------------------------------
!
!  Beowulf assumes that the specified transmitter current will start at 0 or
!  some low value, and rise to a positive maximum before going to zero or
!  oscillating about zero with small magnitudes.  In this case, dI/dt will
!  be computed using the negative I so that the early off-time response is
!  positive for vertical fields.
!

!**********************************
!     DESCRIPTION OF DATA RECORDS *
!**********************************
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
!** RECORD 2:  TDFD, ISYS, ISTOP
!
!      TDFD = 1 => time-domain modelling - STANDARD OPTION
!           = 2 => frequency-domain modelling
!
!      ISYS = 0 : Standard output processing
!           = 2 : Sampo processing => ABS (Bz/Bx) = vertical / radial
!           = 4 : Utem processing
!
!     ISTOP = 0  read the input data and run the specified models.
!           = 1  read the input data, print the model description
!                  and STOP so that the model description can be verified.
!                  REMEMBER to change ISTOP to 0 once the models have been
!                  verified.
!
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
!      CURNT(J) - current in amps for Jth frequency.
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
!                  = 6 : MAGNETOTELLURICS
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
!  DEFINITION OF PLOT CONVENTIONS FOR ALL SURVEY OPTIONS:
!  -----------------------------------------------------
!
!    Define CMP(J), the component selection for magnetic dipole and point electric receivers.
!    For inversion, this will define the data components of Line J that are to be inverted.
!    For modelling, these will govern output for Line J.
!
!    For coincident loop or electric dipole receivers or Sampo, Beowulf sets CMP = 1
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
!          For vertical coaxial dipole surveys,
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
!     RX_TYPE(J) = 1 for magnetic dipole receivers -
!
!                = 2 for electric dipole receivers
!                    not allowed if SOURCE_TYPE = 3 (magnetic dipole transmitters)
!
!                = 3 (frequency domain only)  for point electric field measurements.
!                    not allowed if SOURCE_TYPE = 3 (magnetic dipole transmitters)
!
!         This program requires that receiver electrodes be buried at least 1 mm below surface.
!         This is forced by parameter MIN_RXED IN SUBROUTINE READ_SYSTEM_AND_SURVEY_DATA
!
!       UNITS(J) - defined above at end of RECORD 5 description
!
!
!   Enter 8.J.2 only for magnetic dipole & point electric receivers: (RX_TYPE = 1)
!   Skip 8.J.2 for electric dipole receivers: (RX_TYPE = 2)
!   ---------------------------------------------------------
!** RECORD 8.J.2: (RX_TYPE = 1 only) CMP(J), SV_AZM(J),KNORM(J), IPLT(J), IDH(J), RXMNT(J)
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
!     This version of Beowulf requires that electric dipole receivers be horizontal.
!     Thus only one depth is required. RXZ must be < or = 0.
!     The output is voltage computed as the integral of the electric field.
!     Enter the easting (RXE), northing (RXN) for each electrode followed by receiver depth.
!
!**   RECORD 9.J.I  RXE(I,J,1), RXN(I,J,1), RXE(I,J,2), RXN(I,J,2), RXZ(I,J)
!
!     For point electric field output:
!     -------------------------------
!
!     This is used for for measurements down a hole referenced to a single point.
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
!** RECORD 7.J  NRX(J),TXLNGTH(J),TXWDTH(J),SV_AZM(J),SDE(1,J),SDN(1,J),SDZ0(J)
!
!            NRX(J) : number of transmitter positions on Line J
!        TXLNGTH(J) : length of survey loop along Line J
!         TXWDTH(J) : width of survey loop perpendicular to Line J
!         SV_AZM(J) : azimuth of Tx Line J in degrees. North = 0, East = 90
!                       X direction (radial) is defined positive along SV_AZM
!                       Y direction (tangential) is positive along SV_AZM + 90
!          SDE(1,J) : east coordinate of first transmitter loop centre for Line J
!          SDN(1,J) : north coordinate of first transmitter loop centre for Line J
!           SDZ0(J) : Tx Loop ground clearance for Line J
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
!** RECORD 7.J  NRX(J),TXCLN(J),TXAZM(J),SV_AZM(J),SDE(1,J),SDN(1,J),SDZ0(J)
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
!=============================================================================
!
!          Lithology & structure for Beowulf
!          =================================
!
!** RECORD 10:  NLAYER, NLITH
!
!      NLAYER - number of layers including basement.
!
!       NLITH - number of layer plus plate lithologies.  Any number of
!               lithologies may be defined.  Be careful not to use
!               layer lithologies for plates and vice versa
!
!          DEFINE LITHOLOGIES
!          ------------------
!
!** RECORD 11.1: RES(1), SIGT(1), RMU(1), REPS(1), CHRG(1), CTAU(1), CFREQ(1)
!** RECORD 11.2: RES(2), SIGT(2), RMU(2), REPS(2), CHRG(2), CTAU(2), CFREQ(2)
!     .
!
!** RECORD 11.N: RES(N), SIGT(N), RMU(N), REPS(N), CHRG(N), CTAU(N), CFREQ(N)
!
!           N = NLITH
!      RES(I) - layer resistivity
!     SIGT(I) - thin plate conductance (not used) 
!      RMU(I) - relative layer magnetic permeability for LITH_INDEX(I)
!     REPS(I) - relative layer dielectric constant (permittivity for LITH_INDEX(I)
!     CHRG(I) - Cole-Cole layer chargeability for LITH_INDEX(I)
!     CTAU(I) - Cole-Cole layer time constant for LITH_INDEX(I)
!    CFREQ(I) - Cole-Cole layer frequency constant for LITH_INDEX(I)
!
!    Default values:  RMU = 1   SIGT = -1     REPS = 1   CHRG = 0   CTAU = 0   CFREQ = 1
!
!    The default means no magnetic permeability contrast (MU = 4 PI * 10^(-7))
!                      no dielectric constant contrast  (EPSILON = 8.854215E-12)
!                      and no IP effects (no Cole-Cole)
!
!
!          LAYERED EARTH STRUCTURE
!          -----------------------
!
!   (Don't enter 12.1 for a uniform half-space)
!** RECORD 12.1: LITH(1), THK(1)
!** RECORD 12.2: LITH(2), THK(2)
!
!** RECORD 12.NLAYER: LITH (NLAYER)
!
!      LITH(J) = integer which assigns the resistivity and other
!                physical properties from the list of RECORD 10
!                to layer J.
!
!         THK  = thickness of layers above basement
!
!___________________________________________
!
!     END OF DATA ENTRY IF NO INVERSION
!___________________________________________
!========================================================
!========================================================
!
!             INVERSION CONTROL
!             =================
!
!    The inversion will run until one of the following occurs:
!
!    1. The maximum number of iterations, specified by MAXITS in
!       RECORD 16 has been completed.  This shouldn't happen
!       since the suggested value is MAXITS = 90
!
!    2. The RMS error is below that specified by PCTCNV.
!       Default = 0.1 percent
!
!    3. The inversion is unable to reduce the error any further.
!
!    4. The predicted error decrease goes to zero
!
!    5. After 10 iterations, the combined error reduction of the previous
!       two iterations is less than 1 percent.
!
!    In view of these criteria, it is best not to set MAXITS too low.  During
!    developmental testing, the number of iterations required for convergence
!    rarely exceeded 25.  Often 10 iterations were insufficient.
!
!    MAXITS = 90 will allow the inversion to work to its full capability
!    and is suggested unless the user is in a hurry.
!
!    Beowulf computes sensitivities using a numerical derivative.  The
!    default uses a sequence of 5 and 3 percent step to compute this.
!    Beowulf allows the user to specify a different derivative step through
!    the RECORD 16 variable, CNVRG.
!
!
!** RECORD 16: NFIX, MAXITS, CNVRG, INVPRT
!
!     NFIX   - the number of parameters that will be constrained.  If all
!              parameters are free to vary without restriction, NFIX = 0
!
!               If NFIX > 0, NFIX records describing the constraint must be
!               entered as RECORDS 17
!
!     MAXITS - Maximum number of iterations.  Suggested value: MAXITS = 90.
!
!     CNVRG = 1  => Iterations will proceed using the default stopping criteria.
!                   The default derivative step regime will be used.
!
!           = 2  => The inversion will stop if the RMS (root mean square) error is
!                   reduced to below a user-specified percent (PCTCNV in RECORD 16.1).
!                   The default derivative step regime will be used.
!
!           = 10 => Iterations will proceed using the default stoppingg criteria.
!                   A user-specified derivative regime will be specified in RECORD 16.2
!
!           = 20 => The inversion will stop if the RMS (root mean square) error is
!                   reduced to below a user-specified percent (PCTCNV in RECORD 16.1).
!                   A user-specified derivative step is used. (PCTPAR in RECORD 16.2)
!
!     INVPRT =  1  The model results and global RMS fitting error are written to
!                  Beowulf.out and Beowulf.mv1 after each iteration.
!                  Model data and error structure are not written to these files.
!
!            =  2  as above plus final model data and error structure
!            =  3  as above plus model data and error structure after each iteration
!
!     ------------------
!     only if CNVRG = 2 or 20
!**   RECORD 16.1: PCTCNV - terminate inversion if SQRT {SUMSQ / NDATA } < PCTCNV
!     -------------------
!
!        SUMSQ - sum of the squares of the weighted symmetric error
!                 SERR(J) at each data point.
!
!        WSUM - sum of weights for all data points
!             = NDATA if all data points are equally weighted.
!
!                    W(J) * ABS ( M(J) - D(J) )
!        SERR(J) =   --------------------------
!                    (ABS (M(J)) + ABS (D(J)) ) / 2.
!
!        M(J), D(J) & W(J) are the model value, data value & weight
!        respectively at CHANNEL J.
!
!     ----------------------------------------------------------------------
!     only if CNVRG = 10 or 20  (non-default step regime)
!**   RECORD 16.2: NDSTP - number of difference steps to be used.
!
!**   RECORD 16.3: KPCT (1:NDSTP) - percent changes in parameter values when
!                                   computing sensitivity matrix.
!     ----------------------------------------------------------------------
!
!     In theory, the sensitivity matrix, for each iteration would be computed using
!     the analytic derivatives of the model with respect to each unfixed parameter.
!     In practice, a numerical derivative sceme is faster and more stable.  In Beowulf,
!     this is done by computing models where each unfixed parameter in turn is
!     increased by a specified percentage and then subtracting the original model
!     data from each result.
!
!     Since the inversion is a non-linear process, the exact derivative is not
!     necessarily the most efficient choice.  The inversion path (but not always the
!     result) will be guided by the size of the difference step.  The default step
!     regime in Beowulf initially increasing each parameter by 5 percent and then when
!     the error can no longer be significantly be reduced, testing at 3, and then
!     8 percent for further error reduction.
!
!     If CNVRG is set to 10 or 20, the user can shange this by specifying the number
!     and value of the difference steps to be tested duing the inversion.  The first
!     KPCT value will be used until the error can no longer be reduced and then the
!     next valueswill be used similarly.  Thus the time required by the inversion
!     will be increased according to how many KPCT values are specified.
!
!     At least 1 value is required (NDSTP = 1)  The suggested maximum value of
!     NDSTP is 3.  The values for KPCT should be in the interval from 2 to 15.
!     A KPCT value can be used more than once but it would be silly to use the same
!     values sequentially.
!
!     Example (Default scheme)
!     2         ! NDSTP
!     5 3       ! KPCT (1:NDSTP)
!
!     Example (constant derivative step of 5 percent)
!     1         ! NDSTP
!     5         ! KPCT (1:NDSTP)
!     ______________________________________________________________
!
!     only if NFIX > 0  (NFIX records)
!
!**   RECORD 17: CTYPE, LYR_INDX, KPAR, ELAS(KPAR), LBND(KPAR), UBND(KPAR)
!     _____________________________________________________________________
!
!     CTYPE = 1 : parameter is fixed to initial model value
!                 only LYR_INDX & KPAR need be specified.
!                 ELAS(KPAR), LBND(KPAR), UBND(KPAR) are not read in
!
!           = 2 : frictional restraint
!                 only LYR_INDX, KPAR & ELAS(KPAR)need be specified.
!                 LBND(KPAR), UBND(KPAR) are not read in
!
!           = 3 : Buffered boundaries
!                 LYR_INDX, KPAR, ELAS(KPAR), LBND(KPAR), UBND(KPAR) must be specified.
!
!     LYR_INDX - layer for which a parameter KPAR (resistivity or thickness)
!                will be constrained.
!
!     KPAR = 1 => layer resistivity
!          = 2 => layer thickness
!
!     ELAS(KPAR) - elasticity  of parameter KPAR  (0 < ELAS < 1)
!           If ELAS > 0.95, it is set to ELAS = 1
!           If ELAS < 0.05, it is set to ELAS = 0
!
!     LBND(KPAR) - lower bound of parameter KPAR
!     UBND(KPAR) - upper bound of parameter KPAR
!
!        -------------------------------
!    After each iteration, the inversion will compute a proposed step change
!    for each parameter.  Call this STEP(KPAR)
!
!     For CTYPE = 1, the allowed STEP change = 0
!
!     For CTYPE = 2 - restrained step
!        The maximum allowed parameter change will be ELAS * STEP(KPAR)
!        ELAS serves like a frictional restraint or rubber band preventing a
!        parameter from making the full change suggested by the inversion
!        process.  It is used when a parameter value is thought to be known
!        but allows more latitude than a hard fix.
!
!     For CTYPE = 3 - buffered bounds
!        Suppose the B1(KPAR) is the bound in the direction proposed by STEP(KPAR)
!        Define D1 as the distance between the current parameter value and B1
!        The maximum allowed step for the current iteration S1 = ELAS * D1
!        The actual parameter change will be the minimum of S1 & STEP(KPAR)
!
!   EXAMPLE:  Suppose there were two plates and it was desired to fix the rotation
!             angle of each plate plus constrain the resistivity of the overburden
!             and host to between 800 and 1220 ohm-m and let the OB thickness vary
!             with some retardation.
!
!             In that case, NFIX = 5 and the 5 entries in RECORD 17 would read
!
!             1 1 9
!             1 2 9
!             3 -1 1 .9  800 1200   ! max step = 90 percent of distance to boundary.
!             3 -2 1 .9  800 1200   ! max step = 90 percent of distance to boundary.
!             2 -1 2  0.6           ! actual step = 60 percent of proposed step
!
!=======================================
!                                      =
!     END OF ENTRIES FOR Beowulf.cfl   =
!         (Logical unit NR = 3)        =
!                                      =
!                                      =
!=======================================
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!=================================================================================!
!=                                                                               =!
!=      INVERSION DATA SPECIFICATION                                             =!
!=                                                                               =!
!=                                                                               =!
!=      BEGIN DESCRIPTON FOR Beowulf.inv (Logical unit NRI = 13)                 =!
!=      ------------------------------------------------------                   =!
!=                                                                               =!
!=      Any number of comment lines can be inserted at the beginning of the      =!
!=      .inv file by using either / or \ as the first character of the line.     =!
!=                                                                               =!
!=      Beowulf will start reading at the first line not containing / or \       =!
!=      as the first character.  Thereafter, any line beginning with  / or \     =!
!=      will cause an error and an end to execution.                              =!
!=                                                                               =!
!_________________________________________________________________________________!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!=================================================================================!
!
!             DATA DESCRIPTION & WEIGHTING  - read from Beowulf.inv
!             ============================
!
!  Beowulf.inv allows comment lines only at the start of the file.
!  These must have either \ or / in the first column.
!
!  In what follows, the term channels is used generically to refer to either time
!  or frequency-domain data
!  Each data record must contain the responses for all the channels for all the
!  spatial components specified.
!
!  The records must be arranged in blocks, one for each Line in the oder that
!  the Lines have been specified.  Each block J must have NRX(J) records where
!  NRX(J) = number of receivers on Line J. These records must be in sequential
!  order corresponding to the order in which they were specified in Beowulf.cfl
!
!  Each data record must be grouped component by component.  For example, if, in
!  RECORD 2, KMP(J) = 312, then all the Z-component data channels must be followed
!  by all the X-component data channels followed by all the Y-component data channels.
!
!  Beowulf requires that all lines specified in Beowulf.cfl will be inverted and
!  that data will be presented in the same order as the line specification.
!  All components specified by CMP in Beowulf.cfl must be present in the data.
!  For example, if CMP(J) = 13, then KMP(J) in RECORD 2 must be some permutation
!  of 13 or 123
!
!**  RECORD 1: FD_ORDER
!
!      FD_ORDER = 0 for all time domain data OR
!                   for frequency-domain data where absolute values rather than
!                   inphase and qaudrature are used; eg Sampo
!
!               = 1 : Each datum will be entered as an Inphase, Quadrature pair
!
!               = 2 : All the inphase data for one component is followed by the
!                     quadrature data for that component followed by the similarly
!                     grouped inphase and quadrature data for any other components.
!
!  OUTPUT CONVENTION:  During and after inversion, survey and model data will be ordered using
!                      FD_ORDER = 0 or 1
!
!   ---------------  FOR EACH LINE J:
!  |                 ----------------
!  |
!  | **  RECORD 2: LINE_CHK, NSTATL, KMP(J)
!  |
!  |         LINE_CHK - Line number for Line J, (error checking purposes)
!  |                    It must match Line(J) in Beowulf.cfl to enable the inversion.
!  |
!  |         NSTATL     - Number of stations for Line J, (error checking purposes)
!  |                      It must match NRX(J) in Beowulf.cfl to enable the inversion.
!  |
!  |         KMP(J) is NOT required for coincident loop surveys, Sampo (ISYS = 2),
!  |         or lines using electric dipole receivers (RX_TYPE = 2)
!  |         In these cases, they are all set to 1.
!  |
!  |         KMP(J) IS required for magnetic dipole receivers (RX_TYPE = 1) (except for Sampo)
!  |         KMP governs which data components are present in Beowulf.inv
!  |         and the order in which they appear
!  |
!  |
!  |     (ONLY IF RX_TYPE (J) = 1 - magnetic dipole receivers
!  |        In what follows: 1 = X for surface surveys, U for downhole surveys or S for downhole Utem
!  |                         2 = Y for surface surveys, V for downhole surveys or N for downhole Utem
!  |                         3 = Z for surface surveys, A for downhole surveys or W for downhole Utem
!  |
!  |        To keep the explanation below simple, for downhole surveys, substitute U or S for X
!  |        V or N for Y and A or W for Z
!  |
!  |        For vertical coxial surveys, the X component is read => KMP = 1
!  |        For vertical coplanar surveys (broadside), the Y component is read => KMP = 2
!  |        For horizontal coplanar surveys, the Z component is read => KMP = 3
!  |
!  |        KMP =   1 : Beowulf.inv will contain only X data
!  |            =   2 : Beowulf.inv will contain only Y data
!  |            =   3 : Beowulf.inv will contain only Z data
!  |            =  13 : X data followed by Z data will be included in Beowulf.inv.
!  |            =  31 : Z data followed by X data will be included in Beowulf.inv.
!  |            =  23 : Y data followed by Z data will be included in Beowulf.inv.
!  |            =  32 : Z data followed by Y data will be included in Beowulf.inv.
!  |            = 123 : X data followed by Y data followed by Z data will be included in Beowulf.inv.
!  |            = 312 : Z data followed by X data followed by Y data will be included in Beowulf.inv.
!  |
!  |        KMP = 12, 21, 132, 213, 231, 321 are similarly defined.
!  |
!  |
!  |     DATA_FLOORS (data rejection)
!  |     -----------
!  |       Any data value whose ABSOLUTE MAGNITUDE is less than DATA_FLOOR will be
!  |       weighted to zero.
!  |
!  |  ==>  To ensure that all data is used, set DATA_FLOOR < 0
!  |
!  |  ==>  To exclude any data point, set its value to zero and specify DATA_FLOOR > 0
!  |       This convention replaces identifying points by channel and station; ie,
!  |       N0STAT & N0PTS have been eliminated.
!  |
!  |   ---------------------------------------
!  |     For Time Domain Only
!  | **  RECORD 3: DATA_FLOOR(J)
!  |
!  |   ---------------------------------------
!  |     For Frequency Domain Only
!  |     This allows individual frequency domain data to be weighted to zero by
!  |     using a data floor higher than any possible value.
!  |
!  |     If the absolute value is used; eg Sampo, (FD_ORDER = 0), MCHNL = NFRQ
!  |
!  |     If FD_ORDER > 0, MCHNL = 2*NFRQ
!  |     Enter all NFRQ inphase data floors followed by all NFRQ quadrature data floors
!  |
!  | **  RECORD 3.I: DATA_FLOOR_I (I,J) I = 1 to MCHNL)
!  |
!  |
!  |              DATA ENTRY   (read from Beowulf.inv)
!  |              ==========
!  |
!  |     DO for each receiver J of each line K
!  | **  RECORD 4.J: KSTAT(I,J), DATA(I,J,K), K = 1 to NPCHNL
!  |
!  |        KSTAT(I,J) = station index of receiver I of Line J (sequential from 1 to NRX(J)
!  |       DATA(I,J,K) = consists of all the data (channels & components) associated
!  |                     with KSTAT(I,J) of Line J, in the format specified by KMP(J)
!  |                     and ORDER(J) in RECORD 2.
!  |
!  |      NPCHNL is the total number of data entries for a given receiver
!  |      It equals the number of channels (frequencies) * number of spatial components.
!  |      When both inphase and quadrature data are present, it is double the above.
!  |
!  |
!  |____________  END OF DATA ENTRY FOR LINE J
!
!==========================================================
!                                                         =
!     END DATA ENTRY DESCRIPTIOM                          =
!                                                         =
!**********************************************************
!
!============================================================================

 MODULE BG_Filter_coefficients
!--------------------------

 IMPLICIT NONE

 INTEGER, PARAMETER :: JNLO=-250, JNHI=150, NDEC_JN=15, QL=SELECTED_REAL_KIND(p = 18)
 INTEGER J9
 REAL(KIND=QL) WJ0(JNLO:JNHI), WJ1(JNLO:JNHI), WCOS(-200:99), DELCOS, SHFTJN
 SAVE


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

 END MODULE BG_Filter_coefficients

 MODULE BG_Frequency_select
!-----------------------

 IMPLICIT NONE
 INTEGER, PARAMETER :: NF_MD2=46, NF_6PDE=67
 REAL FRQ_MD2(NF_MD2), FRQ_6PDE(NF_6PDE)
 SAVE

!  FRQ_MD2 has 3 frequencies / decade from 0.001 to 1 Hz &
!              6 frequencies / decade from 1.0 to 1MHz

 DATA FRQ_MD2(1:NF_MD2)/ &
  0.10000000E-02, 0.21544347E-02, 0.46415888E-02, 0.10000000E-01, 0.21544347E-01, 0.46415888E-01, &
  0.10000000E+00, 0.21544347E+00, 0.46415888E+00, 0.10000000E+01, 0.14677993E+01, 0.21544347E+01, &
  0.31622777E+01, 0.46415888E+01, 0.68129207E+01, 0.10000000E+02, 0.14677993E+02, 0.21544347E+02, &
  0.31622777E+02, 0.46415888E+02, 0.68129207E+02, 0.10000000E+03, 0.14677993E+03, 0.21544347E+03, &
  0.31622777E+03, 0.46415888E+03, 0.68129207E+03, 0.10000000E+04, 0.14677993E+04, 0.21544347E+04, &
  0.31622777E+04, 0.46415888E+04, 0.68129207E+04, 0.10000000E+05, 0.14677993E+05, 0.21544347E+05, &
  0.31622777E+05, 0.46415888E+05, 0.68129207E+05, 0.10000000E+06, 0.14677993E+06, 0.21544347E+06, &
  0.31622777E+06, 0.46415888E+06, 0.68129207E+06, 0.10000000E+07/

!  FRQ_6PDE has 6 frequencies / decade from 0.001 to 100 MHz

 DATA FRQ_6PDE(1:NF_6PDE)/ &
  0.10000000E-02, 0.14677993E-02, 0.21544347E-02, 0.31622777E-02, 0.46415888E-02, 0.68129207E-02, &
  0.10000000E-01, 0.14677993E-01, 0.21544347E-01, 0.31622777E-01, 0.46415888E-01, 0.68129207E-01, &
  0.10000000E+00, 0.14677993E+00, 0.21544347E+00, 0.31622777E+00, 0.46415888E+00, 0.68129207E+00, &
  0.10000000E+01, 0.14677993E+01, 0.21544347E+01, 0.31622777E+01, 0.46415888E+01, 0.68129207E+01, &
  0.10000000E+02, 0.14677993E+02, 0.21544347E+02, 0.31622777E+02, 0.46415888E+02, 0.68129207E+02, &
  0.10000000E+03, 0.14677993E+03, 0.21544347E+03, 0.31622777E+03, 0.46415888E+03, 0.68129207E+03, &
  0.10000000E+04, 0.14677993E+04, 0.21544347E+04, 0.31622777E+04, 0.46415888E+04, 0.68129207E+04, &
  0.10000000E+05, 0.14677993E+05, 0.21544347E+05, 0.31622777E+05, 0.46415888E+05, 0.68129207E+05, &
  0.10000000E+06, 0.14677993E+06, 0.21544347E+06, 0.31622777E+06, 0.46415888E+06, 0.68129207E+06, &
  0.10000000E+07, 0.14677993E+07, 0.21544347E+07, 0.31622777E+07, 0.46415888E+07, 0.68129207E+07, &
  0.10000000E+08, 0.14677993E+08, 0.21544347E+08, 0.31622777E+08, 0.46415888E+08, 0.68129207E+08, &
  0.10000000E+09/

 END MODULE BG_Frequency_select

 MODULE BG_Input_routines
!--------------------------

!** CONTAINS: READ_SYSTEM_DATA, READ_MODEL_DATA, SET_FRQ

 Use iso_Fortran_env
 Use BG_Metadata
 IMPLICIT NONE

! SYSTEM & LITHOLOGY DIMENSIONS
! -----------------------------

 INTEGER, PARAMETER :: NPROP=7, QL=SELECTED_REAL_KIND(p = 18)
 Integer, Parameter :: FVERS = 100
 REAL, PARAMETER :: PI=3.141592654, PI2=PI/2., R2D=180./PI,  D2R=PI/180.
 INTEGER NR,NW,ND,NLG,NRI,NP,MD1,MSG,MXERR,TDFD,STEP,NSX,ISTOP,KRXW, &
         MCHNL,NCHNL,NFRQ,NFT,MCMP,SOURCE_TYPE,SURVEY_TYPE,   &
         NLINES,MLINES,NTX,MXVRTX,MQVR,ISYS,KTX,K1,MXTX,NTXL,J,JS,JT,JF,JV,JR,JC,JRL,  &
         MRXTX,MRXL,NLITH,NPULS,NTYRP,NTYPLS,NPPD,NR1,MD2,NHID,NSTAT
 INTEGER, ALLOCATABLE, DIMENSION(:) :: LINE,IPLT,IDH,NVRTX,UNITS,KNORM,NRX,RX_TYPE,CMP,NRGTX,NRXTX, &
                                       HEADER_ID,KHID
 INTEGER, ALLOCATABLE, DIMENSION(:,:)  :: KRGTX,RXID,KNORM2,NCTD,LNTR
 REAL T0,T0SX,OFFTYM,REFTYM,PULSE,RXFMNT,TXFREQ,TXMNT,ZMAX,ZMIN,SV_AZM
 REAL, ALLOCATABLE, DIMENSION(:) :: TXON,WAVEFORM,CURNT,TRP,TMS,WTMS,TOPN,TCLS,FREQ,SWX,TXZ,SXZ,SVAZM,SDZ0, &
                                    TXLNGTH,TXWDTH,TXCLN,TXAZM,SXDIP,SXAZM,RHOTRP,RXMNT,RXOFF,RXOTX
 REAL, ALLOCATABLE, DIMENSION(:,:) :: SWY,LYTH,SXE,SXN,ZRXTX,RXDIP,RXAZM,BHDIP,BHAZM,RXZ,XRXOF,YRXOF,ZRXOF,DSTAT
 REAL, ALLOCATABLE, DIMENSION(:,:,:) :: XRXTX,YRXTX,RXE,RXN
 REAL(KIND=QL) ECNTRD,NCNTRD,QD,QFRQ1,QFRQ2,FQQ
 REAL(KIND=QL), ALLOCATABLE :: SDN0(:),SDE0(:)
 REAL(KIND=QL), ALLOCATABLE, DIMENSION(:,:)   :: SXED,SXND,CLCD
 REAL(KIND=QL), ALLOCATABLE, DIMENSION(:,:,:) :: YXZPLT,RXED,RXND
 CHARACTER (LEN=120) INP,TITLE
 ! CHARACTER(LEN=60) PVC,LTXT
 ! CHARACTER(LEN=10) TIME,DATE,ZONE
 ! CHARACTER(LEN=3) MONTH(12)
 DATA NR,NW,ND,NLG,NRI,NP,NR1 /3,4,7,9,13,14,15/
 ! DATA MONTH /'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'/
 ! DATA PVC, FVN /'Beowulf - Version 1.0.5   11 January, 2016',100/
 ! DATA LTXT     /'----------------------------------------'/

! Inversion specific parameters
! -----------------------------
 INTEGER NDATA,KCHNL,MAXITS,CNVRG,INVPRT
 INTEGER, ALLOCATABLE :: RWTS(:,:,:,:)
 INTEGER, ALLOCATABLE, DIMENSION(:) :: CXPAR,NCMPL,KMP
 REAL PCTCNV,PARPCT
 REAL, ALLOCATABLE, DIMENSION(:) :: XDATA,QDATA,XMODL,UBND,LBND,ELAS
 REAL, ALLOCATABLE :: RDATA(:,:,:,:)
 LOGICAL, ALLOCATABLE :: SINGLE(:)
 Integer :: tvals(8)

! Specific parameters for Beowulf
! ------------------------------
 INTEGER NLYR,JL,JP,MXAB,BEG,FIN,JA,JB,JAB,NAB,NAJ,NBJ,JP2,JAB2, &
         NAB2,MXRHO,NRMGT,MXCL2,NPAR,NDSTP
 INTEGER,ALLOCATABLE, DIMENSION(:) :: KPCT
 REAL, ALLOCATABLE, DIMENSION(:) :: RES,RES0,THK,THK0,RMU,REPS,CHRG,CTAU,CFREQ,MPAR
 REAL(KIND=QL),ALLOCATABLE, DIMENSION(:) :: RMUD,THKD

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
!      RXID(I,J) - RX_TYPE of receiver I for transmitter J. 1 => MD; 2 => ED; 4=> cdnt loop.
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
 INTEGER NTRN,KTXP,IDH5, ISTAT
 REAL A1
 REAL(KIND=QL), ALLOCATABLE, DIMENSION(:,:,:) :: QD1,QD2

!      Initialise some variables.

 T0SX = 100.
 MXERR = 0             !  Initialise input error flag
 REFTYM = 0.
 MRXTX = 1
 MXVRTX = 4
 SV_AZM = 0.
 MCMP = 3               !  Number of possible spatial components

!  Reproduce input data with no assignments and rewind file.
call Date_and_Time(Values = tvals)
Write (NW, 1) PNAME, PVERS, PDATE, PAUT1, PAUT2, PAUT3, PPROJ, PRELS, &
              compiler_version(), compiler_options(), &
              tvals(1:3), tvals(5:7)
Write ( *, 1) PNAME, PVERS, PDATE, PAUT1, PAUT2, PAUT3, PPROJ, PRELS, &
              compiler_version(), compiler_options(), &
              tvals(1:3), tvals(5:7)

 ! WRITE(NW,1) PNAME,LTXT
 ! WRITE(*,1) PNAME,LTXT
! WRITE(NW,'(T11,A/T11,A/)') 'INPUT DATA', '----------'
! REFLECT_DATA: DO J = 1,200
!   READ(NR,'(A)',END = 100) INP
!   WRITE(NW,'(1X,A)') INP
! END DO REFLECT_DATA

 100 REWIND NR
     WRITE(NW,2)

 READ(NR,'(A)') TITLE

! Read model control & print parameters

 READ(NR,*)  TDFD, ISYS, ISTOP
 WRITE(NW,3) TDFD, ISYS, ISTOP

!   TDFD = 1 or 2 for standard TD or FD respectively.
!  ISTOP - read data and stop if ISTOP = 1

 IF (TDFD == 1) THEN
   IF (ISYS == 2) THEN
     CALL WRITE_LOG_FILE (NLG,40,MXERR,2)
   ELSE IF (ISYS /= 0 .AND. ISYS /= 4) THEN
     CALL WRITE_LOG_FILE (NLG,16,MXERR,2)
   END IF
 ELSE
   IF (ISYS /= 0 .AND. ISYS /= 2) CALL WRITE_LOG_FILE (NLG,17,MXERR,2)
 END IF

 STEP = 2
 IF (TDFD == 1) THEN
   NFT = 1                                      ! Used for dimensioning similar TD & FD arrays
   ALLOCATE (CURNT(NFT),FREQ(1))

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
   STEP = 2
   READ(NR,*)  NFRQ
   WRITE(NW,8) NFRQ
   NFT = NFRQ                        ! Used for dimensioning similar TD & FD arrays
   MCHNL = 2*NFRQ
   IF (ISYS == 2) MCHNL = NFRQ
   ALLOCATE( FREQ(NFRQ), CURNT(NFT),SWX(1),SWY(1,1),TRP(1),TOPN(1),TCLS(1))

! Read & write source waveform.

   WRITE(NW,9)
   DO J = 1, NFRQ
     READ(NR,*) FREQ(J),CURNT(J)
     WRITE(NW,'(3X,I4,G13.4,5X,G13.4)') J,FREQ(J),CURNT(J)
   END DO
 END IF


! Survey Information
! ------------------
! Read in absolute locations in double precision variables and convert to
! REAL body centred coordinates before entering the computation realm.

 NTRN = 1; TXMNT = 1; NTXL = 1

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
   READ(NR,*) NTXL, MRXL, MRXTX, NTRN, ISTAT
   TXMNT = REAL (NTRN)
   WRITE(NW,23) NTXL, MRXL, MRXTX, NTRN, ISTAT
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
   READ(NR,*)   NTXL, MRXL, MRXTX, TXMNT, ISTAT
   WRITE(NW,24) NTXL, MRXL, MRXTX, TXMNT, ISTAT
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
   READ(NR,*) NLINES, MRXL, NTRN, ISTAT
   NTXL = NLINES
   TXMNT = REAL (NTRN)
   WRITE(NW,26) NLINES, MRXL, NTRN, ISTAT
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
 SXED=0.D0;  DSTAT=0.; RXOTX=0.;  MQVR = 1; IDH = 0; LNTR=1

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
     READ(NR,*) KTXP,TXLNGTH(JS),TXWDTH(JS),SV_AZM,SDE0(JS),SDN0(JS),SDZ0(JS)
     IF (KTXP > MRXL) THEN
       CALL WRITE_LOG_FILE (NLG,14,MXERR,1)
       KTXP = MRXL
       WRITE (NLG,34) JS,MRXL
     END IF

     Select Case (ISTAT)
     Case (1)
        Read (nr, *) dstat(2:ktxp, js)
     Case (2)
        Read (nr, *) (sde0(jt), sdn0(jt), jt = 2, ktxp)
     End Select

     WRITE(NW,27) JS,KTXP,TXLNGTH(JS),TXWDTH(JS),SDE0(JS),SDN0(JS),SDZ0(JS),SV_AZM,DSTAT(2:KTXP,JS)
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
   IF (ISYS == 2) THEN
     HEADER_ID = 250
     MCMP = 1           ! Sampo output is single component (ABS (Bz/Bx))
   END IF

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
   IF (ISYS == 2) THEN
     HEADER_ID = 250
     MCMP = 1           ! Sampo output is single component (ABS (Bz/Bx))
   END IF

 CASE (4)              !  Coincident Loop
   NVRTX = 4
   RX_TYPE = 4
   NRXTX = 1
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
 END SELECT

 IF (SURVEY_TYPE /= 6 .AND. MAXVAL (NRX) > MRXL) THEN
   CALL WRITE_LOG_FILE(NLG,32,MXERR,2)
   DO JL = 1,NLINES
     IF (NRX(JL) < MRXL) CYCLE
     WRITE(NLG,90) JL,NRX(JL),MRXL
   END DO
 END IF

 NSTAT = 0
 DO JL = 1, NLINES
   DO JR = 1, NRX(JL)
     NSTAT = NSTAT + 1
   END DO
 END DO

 IF (MAXVAL (KNORM) > 0 .AND. STEP == 0) THEN
   KNORM = 0
   CALL WRITE_LOG_FILE (NLG,13,MXERR,1)
 END IF

 MLINES = NLINES
 ALLOCATE (RDATA(MCHNL,MRXL,MCMP,MLINES),RWTS(MCHNL,MRXL,MCMP,MLINES),      &
           SXN(MXVRTX,NTX),SXE(MXVRTX,NTX),XRXTX(MRXTX,NTX,MQVR),YRXTX(MRXTX,NTX,MQVR),ZRXTX(MRXTX,NTX), &
           YXZPLT(3,MRXL,NLINES),NCTD(MRXTX,NTX),RXID(MRXTX,NTX),KNORM2(MRXTX,NTX))

 RDATA = 0.
 RWTS = 1
 SXN = 0.; SXE = 0.; XRXTX = 0.;  YRXTX = 0.;  ZRXTX = 0.
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

 IF (TDFD == 1) SWY = SWY * TXMNT
 CURNT = CURNT * TXMNT
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
 3 FORMAT(/T3,'TDFD =',I2,';   ISYS =',I2,';   ISTOP =',I2)
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
           /T3,'Initial position (E,N,Z) :',3F11.2 /T13,'Line_azimuth :',F6.1/T8,'Station_intervals :',60(2x, f11.2))
 28 FORMAT(/T3,'Moving Magnetic Dipole Tx Line',I3,';   NTX =',I3,';   TXCLN =',F6.1,';   TXAZM =',F6.1,&
          //T3,'Initial position (E,N,Z) :',3F11.2,/T13,'Line_azimuth :',F6.1/T8,'Station_intervals :',60(2x, F11.2))
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
 40 FORMAT(/T3,'UTEM base frequency =',F7.1,' Hz.   NCHNL = ',I3)
 90 FORMAT(T3,'JL =',I3,' ;  NRX(JL) =',I3,' ;  MRXL =',I3)

    END SUBROUTINE READ_SYSTEM_AND_SURVEY_DATA

   SUBROUTINE READ_MODEL_DATA
!  --------------------------

!***  Called by MAIN

 IMPLICIT NONE
 INTEGER, ALLOCATABLE :: LITHL(:)

 ECNTRD = 0.D0; NCNTRD = 0.D0
 IF (SURVEY_TYPE == 1) THEN
   ECNTRD = MAXVAL (RXED) - MINVAL (RXED)
   NCNTRD = MAXVAL (RXND) - MINVAL (RXND)
   IF (ABS (ECNTRD) < 2.D3) ECNTRD = 0.D0
   IF (ABS (NCNTRD) < 2.D3) NCNTRD = 0.D0
 END IF

!  Layered Model Specification
!  ---------------------------

 READ(NR,*)  NLYR, NLITH
 WRITE(NW,1) NLYR, NLITH
 NPAR = 2*NLYR-1

 ALLOCATE (LYTH(NLITH,NPROP),LITHL(NLYR),RES(NLYR),RMU(NLYR),RMUD(0:NLYR),REPS(NLYR),CHRG(NLYR), &
           CTAU(NLYR),CFREQ(NLYR),THK(NLYR),THKD(NLYR),MPAR(NPAR),RES0(NLYR),THK0(NLYR))

 THK=1.E5; RES=0; CHRG=0; CTAU=0; CFREQ=1; RMU=1; RMUD=1._QL
 REPS=1; LITHL=0; MPAR=0.

!  Initialise lithology list.

 LYTH(1:NLITH, 1) = -1.   !  blank resistivity indicator
 LYTH(1:NLITH, 2) = -1.   !  conductance 
 LYTH(1:NLITH, 3) = 1.    !  Relative magnetic permeabilities
 LYTH(1:NLITH, 4) = 1.    !  Relative dielectric constants
 LYTH(1:NLITH, 5) = 0.    !  Chargeabilities
 LYTH(1:NLITH, 6) = 0.    !  CTAUs
 LYTH(1:NLITH, 7) = 1.    !  CFREQs

 WRITE(NW,2)
 DO J = 1,NLITH
   READ (NR,*) LYTH(J,1:NPROP)
   ! WRITE(NW,'(I4,T8,G12.4,T8,G12.4,T22,F7.1,F12.3,F11.3,F10.2,G12.3,F8.2)') J,LYTH(J,1:NPROP)
   Write (nw, 4) j, lyth(j, 1:nprop)
   IF (LYTH(J,1) < 0 .AND. LYTH(J,2) < 0) CALL WRITE_LOG_FILE (NLG,21,MXERR,2)

   IF (LYTH(J,3) < 0.01) LYTH(J,3) = 1.   ! Default RMU
   IF (LYTH(J,4) < 0.01) LYTH(J,4) = 1.   ! Default REPS


   IF (LYTH(J,5) < 1.E-3 .OR. LYTH(J,6) < 1.E-15 .OR. LYTH(J,7) < 1.E-6)  THEN
     LYTH(J,5) = 0   ! default CHRG
     LYTH(J,6) = 0   ! default CTAU
     LYTH(J,7) = 1   ! default CFRQ
   END IF

 END DO

 WRITE(NW,3)
 Write (nw, 5)
 IF (NLYR > 1) THEN
   DO J = 1, NLYR-1
     READ (NR,*) LITHL(J), THK(J)
     WRITE(NW,'(2I4,F7.1,T19,A)') J, LITHL(J), THK(J),'J, LITHL(J), THK(J)'
   END DO
 END IF
 READ(NR,*) LITHL(NLYR)
 WRITE(NW,'(2I4,T22,A)') NLYR,LITHL(NLYR),'Basement Lithology'

 DO JL = 1, NLYR
   J = LITHL(JL)

   IF (J < 1 .OR. J > NLITH) THEN
     WRITE(NW,'(T3,A,I2,A,I4)') 'LITHL(',JL,') =',J
     CALL WRITE_LOG_FILE (NLG,22,MXERR,2)
   END IF

   RES(JL)  =  LYTH(J,1)
   IF ( RES(JL) < 0) CALL WRITE_LOG_FILE (NLG,23,MXERR,2)

   RMU(JL)   = LYTH(J,3)
   RMUD(JL)   = REAL (RMU(JL),KIND=QL)
   REPS(JL)  = LYTH(J,4)
   CHRG(JL)  = LYTH(J,5)
   CTAU(JL)  = LYTH(J,6)
   CFREQ(JL) = LYTH(J,7)
 END DO
 THKD = REAL (THK,KIND=QL)
 RES0 = RES
 THK0 = THK


 1 FORMAT(//T3,'NLAYER =',I3,';   NLITH =',I3)
 2 FORMAT(/, 2x, 'Lithology properties', &
          /, 2x, '--------------------', &
          /, 8x, 'Resistivity', 3x, 'Conductance', 10x, 'R-Mu', 9x, 'R-Eps', &
             3x, 'C-C Charge.', 7x, 'C-C Tau', 5x, 'C-C Freq.')
 3 FORMAT( /T3,'Initial Layered earth', &
           /T3,'---------------------')
 4 Format (2x, i3, 7(2x, f12.5))
 5 Format (8x, 'Lith.', 5x, 'Thickness')
 6 Format (2x, i3, 4x, i4, 2x, f12.5)

   END SUBROUTINE READ_MODEL_DATA

   SUBROUTINE READ_PARAMETER_CONTROL
!  ---------------------------------

!***  Called by: MAIN
!***      Calls: WRITE_LOG_FILE

 INTEGER NFIX,CTYPE,LYR_INDX,KPAR,J1
 REAL E1,E2,E3,A1,A2
 CHARACTER(LEN=12) LYR_PRM(2)
 DATA LYR_PRM /  'Resistivity','Thickness'/

!  Set degree of constraint on each parameter
!  CXPAR = 0 => parameter is completely free to vary as dictated by inversion step
!        = 1 => parameter is fixed
!        = 2 => parameter is constrained by elasticity.
!        = 3 => parameter bounds are buffered.

 READ(NR,*) NFIX,MAXITS,CNVRG,INVPRT
 IF (INVPRT < 0 .OR. INVPRT > 3) INVPRT = 1
 WRITE(NW,1) NFIX,MAXITS,CNVRG,INVPRT
 SELECT CASE (CNVRG)
 CASE (1)
   PCTCNV = 0.1
   NDSTP = 2
   ALLOCATE (KPCT(3))
   KPCT(1) = 5 ; KPCT(2) = 3
 CASE (2)
   READ(NR,*) PCTCNV
   NDSTP = 2
   ALLOCATE (KPCT(3))
   KPCT(1) = 5 ; KPCT(2) = 3
 CASE (10)
   PCTCNV = 0.1
   READ(NR,*) NDSTP
   ALLOCATE (KPCT(NDSTP))
   READ(NR,*) KPCT(1:NDSTP)
 CASE (20)
   READ(NR,*) PCTCNV
   READ(NR,*) NDSTP
   ALLOCATE (KPCT(NDSTP))
   READ(NR,*) KPCT(1:NDSTP)
 CASE DEFAULT
   CALL WRITE_LOG_FILE (NLG,63,MXERR,2)
 END SELECT
 WRITE(NW,2) PCTCNV,MAXITS !; WRITE(*,2) PCTCNV,MAXITS
 WRITE(NW,3) NDSTP,KPCT(1:NDSTP)

 ALLOCATE (ELAS(NPAR),LBND(NPAR),UBND(NPAR),CXPAR(NPAR))
 CXPAR = 0
 ELAS = 1.
 LBND =  1.E-15
 UBND =  1.E15
 IF (NFIX > 0) THEN
   WRITE(NW,4)
   DO JP = 1, NFIX
     E1 = 1.
     E2 = 1.E-15
     E3 = 1.E15
     READ(NR,*) CTYPE
     BACKSPACE NR
     SELECT CASE (CTYPE)            ! J1 is a dummy variable
     CASE(1)
       READ(NR,*) J1,LYR_INDX,KPAR
       E1 = 0.
     CASE(2)
       READ(NR,*) J1,LYR_INDX,KPAR,E1
     CASE(3)
       READ(NR,*) J1,LYR_INDX,KPAR,E1,E2,E3
     END SELECT

     IF (LYR_INDX < 1 .OR. LYR_INDX > NLYR) THEN
       CALL WRITE_LOG_FILE (NLG,210,MXERR,1)
       WRITE(NLG,20) JP
       IF (KPAR < 1 .OR. KPAR > 2) THEN
         CALL WRITE_LOG_FILE (NLG,211,MXERR,1)
         WRITE(NLG,20) JP
       END IF
     END IF

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
     IF (A1  < A2) E1 = 0.                 ! set elasticIty to 0.

     J1 = LYR_INDX                     !  Resistivity of layer LYR_INDX
     IF (KPAR == 2) J1 = J1 + NLYR     !  Thickness of layer LYR_INDX
     CXPAR (J1) = CTYPE
     IF (E1 < 0.05) CXPAR(J1) = 1
     IF (CTYPE == 3) THEN
       WRITE(NW,5) J1,LYR_INDX,KPAR,LYR_PRM(KPAR),E1,E2,E3,CXPAR(J1)
     ELSE
       WRITE(NW,6) J1,LYR_INDX,KPAR,LYR_PRM(KPAR),E1,CXPAR(J1)
     END IF
     ELAS(J1) = E1
     LBND(J1) = E2
     UBND(J1) = E3
   END DO
   WRITE(NW,9)
 ELSE
   WRITE(NW,10)
 END IF

  1 FORMAT(//T3,'-----------------------------------------------------' &
            /T3,'Inversion Controls for Layer Parameters using Beowulf' &
            /T3,'-----------------------------------------------------' &
           //T3,'NFIX =',I3,3X,'MAXITS =',I3,3X,'CNVRG =',I2,3X,'INVPRT =',I2)
  2 FORMAT(/T3,'The inversion will finish if the RMS error is less than',F5.1,& 
            ' percent or for some other as yet undisclosed reason.' &
           /T3,'A maximum of',I3,' iterations will be allowed.')
  3 FORMAT(T3,'The inversion sequence will use',I2,' numerical derivative steps' &
          /T3,'Values in percent:',10I3)
  4 FORMAT(//T12,'Constrained Parameters'/T12,'----------------------' &
           //T5,'Global  Layer  Parameter'                                                        &
            /T5,'Index   Index   Index      Parameter      Elasticity  Lower Bound   Upper Bound   CTYPE' &
            /T5,'------  -----  ---------   ---------      ----------  -----------   -----------   -----')
  5 FORMAT(I8,I7,I8,T32,A,T50,F5.2,G15.4,G14.4,T88,I2)
  6 FORMAT(I8,I7,I8,T32,A,T50,F5.2,T88,I2)
  9 FORMAT(/T3,90('-'))
 10 FORMAT(/T3,'All parameters will be allowed to vary during inversion')
 20 FORMAT(T3,'Constraint',I2,' ignored')

 END SUBROUTINE READ_PARAMETER_CONTROL

 SUBROUTINE PREPARE_INVERT_DATA
!-----------------------------

 INTEGER MCHNL,LINE_CHK,FD_ORDER,NSTATL,MD,NDT,JC,JL,JR,JF,J0,J1,J2,JRD,NCL,KPC(4)
 INTEGER, DIMENSION(NLINES):: KMP,NKMP
 REAL, ALLOCATABLE,DIMENSION (:) :: DATA_FLOOR,Q2DATA
 LOGICAL KMP1
 CHARACTER (LEN=1) TCHR
 CHARACTER(LEN=20) CTXT(3),QL0
 Logical :: IsComment

! ----------------------------------------------------------------
! Set inversion dimensions:
!
!  NCHNL = number of time domain channels
!  MCHNL = NCHNL for Time domain or NFRQ for frequency domain
!  MCHNL = 2*NFRQ only if both inphase and quadrature data are included
!
!  RDATA & RWTS are data and weights in array form (MCHNL, NSTAT)
!  XDATA is the  data in column form  (MCHNL * NSTAT)
!  RWTS  is now restricted to integer values of 0 or 1 (reject or accept)
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
!  XDATA is RDATA stacked into a 1D array excluding all data weighted to zero.
! ----------------------------------------------------------------


!  Start reading from Beowulf.inv on UNIT NRI = 13
!  First skip over alL comment lines

 ALLOCATE (SINGLE(NLINES))
 SINGLE = .FALSE.
 KMP1 = .FALSE.
 IF (SURVEY_TYPE == 4 .OR. ISYS == 2) THEN
   KMP1 = .TRUE.                             ! Coincident loop or Sampo
   SINGLE = .TRUE.                           ! Single component is put into X position in data
 END IF

 DO
   READ (NRI,'(A)') TCHR
   IF (.not.(IsComment(tchr))) EXIT
 END DO
 BACKSPACE (NRI)

 READ(NRI,*) FD_ORDER
 IF (TDFD == 2) THEN
   MCHNL = 2*NFRQ
   IF (FD_ORDER == 0) MCHNL = NFRQ
 ELSE
   FD_ORDER = 0
   MCHNL = NCHNL
 END IF
 WRITE(NW,7) FD_ORDER
 IF (FD_ORDER /= 0 .AND. FD_ORDER /= 1 .AND. FD_ORDER /= 2) THEN
   CALL WRITE_LOG_FILE(NLG,58,MXERR,2)
   WRITE(NLG,'(T3,A,I2)') 'FD_ORDER = ',FD_ORDER
 END IF

 ALLOCATE (QDATA(MCHNL*3),Q2DATA(MCHNL*3),DATA_FLOOR(MCHNL))

 DATA_FLOOR = 0.
 KMP = 1
 NKMP = 1
 DO JL = 1,NLINES
   WRITE(QL0,*) LINE(JL)
   READ(QL0,'(A)') CTXT(1)           ! Line number
   WRITE(NW,1) TRIM(ADJUSTL(CTXT(1)))

   IF (ISYS == 2) THEN
     READ(NRI,*) LINE_CHK,NSTATL
     WRITE(NW,2) KMP(JL), CMP(JL)
     CALL VALIDATE_LINE (NLG,MXERR,LINE_CHK,NSTATL,JL,NLINES,LINE,NRX)

   ELSE IF (RX_TYPE(JL) == 1) THEN
     READ(NRI,*) LINE_CHK,NSTATL,KMP(JL)
     CALL VALIDATE_CMP (NLG,MXERR,JL,NLINES,CMP,NCMPL,ISYS)
     WRITE(NW,2) KMP(JL), CMP(JL)
     CALL VALIDATE_LINE (NLG,MXERR,LINE_CHK,NSTATL,JL,NLINES,LINE,NRX)
     CALL VALIDATE_KMP (NLG,MXERR,JL,NLINES,CMP,KMP)
   ELSE
     READ(NRI,*) LINE_CHK,NSTATL
     WRITE(NW,2) KMP(JL), CMP(JL)
     CALL VALIDATE_LINE (NLG,MXERR,LINE_CHK,NSTATL,JL,NLINES,LINE,NRX)
   END IF
   IF (KMP(JL) > 9) NKMP(JL) = 2
   IF (KMP(JL) > 99) NKMP(JL) = 3

   IF (NSTATL /= NRX(JL)) THEN
     CALL WRITE_LOG_FILE(NLG,100,MXERR,2)
     WRITE(NLG,10) TRIM (ADJUSTL (CTXT(1)))
     WRITE(NLG,13) JL,NRX(J),NSTATL
   END IF

   IF (TDFD == 1) THEN
     READ(NRI,*) DATA_FLOOR(1)
     WRITE(NW,3) DATA_FLOOR(1)
     DATA_FLOOR(2:MCHNL) = DATA_FLOOR(1)
   ELSE IF (TDFD == 2) THEN
     READ(NRI,*) DATA_FLOOR(1:MCHNL)

     IF (FD_ORDER /= 0) THEN
       WRITE(NW,4)
       DO JF = 1,NFRQ
         WRITE(NW,6) JF,FREQ(JF),DATA_FLOOR (JF), DATA_FLOOR(JF+NFRQ)
       END DO
     ELSE
       WRITE(NW,5)
       DO JF = 1,NFRQ
         WRITE(NW,6) JF,FREQ(JF),DATA_FLOOR (JF)
       END DO
     END IF
   END IF

!======================
!      DATA ENTRY
!======================

!  If there is more than one spatial component, arrange the data such that all
!  the data for one component is followed by all the data for the next.
!  When both inphase and quadrature data are present, arrange the data for each
!  component such that all the inphase data are followed by all the quadrature
!  data for that component.
!  For multi-component data where the data is presented as inphase and quadrature
!  duples, the data is temporarily arranged such that all the inphase data for all
!  components is followed by all the quadrature data for all components.

   IF (RX_TYPE(JL) == 2) SINGLE(JL) = .TRUE.
   MD = NKMP(JL) * MCHNL
   QDATA = 0.;  Q2DATA = 0.
   DO JR = 1,NRX(JL)
     READ(NRI,*) JRD,QDATA(1:MD)
     IF (JRD /= JR) CALL WRITE_LOG_FILE (NLG,62,MXERR,2)
     IF (SINGLE(JL)) THEN
       KPC = 1
       IF (FD_ORDER == 1) THEN
         DO JF = 1,NFRQ
           Q2DATA(JF) = QDATA(2*JF-1)     ! arrange such that all inphase followed
           Q2DATA(JF+NFRQ) = QDATA(2*JF)  ! by all quadrature data
         END DO
         QDATA(1:MCHNL) = Q2DATA(1:MCHNL)
       END IF
       RDATA (1:MCHNL,JR,1,JL) = QDATA(1:MCHNL)
       IF (.NOT. KMP1) RWTS(1:MCHNL,JR,2:3,JL) = 0
       CYCLE
     END IF

!    Rearrange multi-component data if necessary.
!    If FD_ORDER = 1, arrange such that the inphase data for each spatial
!    component, is followed by all the quadrature data for that component.

     IF (FD_ORDER == 1) THEN
       DO JC = 1,NKMP(JL)
         J0 = NFRQ * (2*JC-2)
         DO JF = 1,NFRQ
           Q2DATA(J0+JF) = QDATA(J0+2*JF-1)
           Q2DATA(J0+JF+NFRQ) = QDATA(J0+2*JF)
         END DO
       END DO
       QDATA(1:MD) = Q2DATA(1:MD)
     END IF

     DO JC = 1, NKMP(JL)
       J1 = (JC -1) * MCHNL + 1
       J2 = JC * MCHNL
       RDATA(1:MCHNL,JR,JC,JL) = QDATA(J1:J2)
     END DO

!  Arrange the multi-component data such that the order is X, Y, Z
!  and that these are in the positions 1, 2, 3 respectively in RDATA

     SELECT CASE (KMP(JL))
     CASE (1)
       RDATA(1:MCHNL,JR,2:3,JL) = 0.
     CASE (2)
       RDATA(1:MCHNL,JR,2,JL) = RDATA(1:MCHNL,JR,1,JL)
       RDATA(1:MCHNL,JR,1,JL) = 0.
       RDATA(1:MCHNL,JR,3,JL) = 0.
     CASE (3)
       RDATA(1:MCHNL,JR,3,JL) = RDATA(1:MCHNL,JR,1,JL)
       RDATA(1:MCHNL,JR,1:2,JL) = 0.
     CASE (12)
       RDATA(1:MCHNL,JR,3,JL) = 0.
     CASE (13)
       RDATA(1:MCHNL,JR,3,JL) = RDATA(1:MCHNL,JR,2,JL)
       RDATA(1:MCHNL,JR,2,JL) = 0.
     CASE (23)
       RDATA(1:MCHNL,JR,3,JL) = RDATA(1:MCHNL,JR,2,JL)
       RDATA(1:MCHNL,JR,2,JL) = RDATA(1:MCHNL,JR,1,JL)
       RDATA(1:MCHNL,JR,1,JL) = 0.
     CASE (32)
       RDATA(1:MCHNL,JR,3,JL) = RDATA(1:MCHNL,JR,1,JL)
       RDATA(1:MCHNL,JR,1,JL) = 0.
     CASE (31)
       RDATA(1:MCHNL,JR,3,JL) = RDATA(1:MCHNL,JR,1,JL)
       RDATA(1:MCHNL,JR,1,JL) = RDATA(1:MCHNL,JR,2,JL)
       RDATA(1:MCHNL,JR,2,JL) = 0.
     CASE (21)
       QDATA(1:MCHNL) = RDATA(1:MCHNL,JR,1,JL)
       RDATA(1:MCHNL,JR,1,JL) = RDATA(1:MCHNL,JR,2,JL)
       RDATA(1:MCHNL,JR,2,JL) = QDATA(1:MCHNL)
       RDATA(1:MCHNL,JR,3,JL) = 0.
     CASE (213)
       QDATA(1:MCHNL) = RDATA(1:MCHNL,JR,1,JL)
       RDATA(1:MCHNL,JR,1,JL) = RDATA(1:MCHNL,JR,2,JL)
       RDATA(1:MCHNL,JR,2,JL) = QDATA(1:MCHNL)
     CASE(321)
       QDATA(1:MCHNL) = RDATA(1:MCHNL,JR,1,JL)
       RDATA(1:MCHNL,JR,1,JL) = RDATA(1:MCHNL,JR,3,JL)
       RDATA(1:MCHNL,JR,3,JL) = QDATA(1:MCHNL)
     CASE (132)
       QDATA(1:MCHNL) = RDATA(1:MCHNL,JR,3,JL)
       RDATA(1:MCHNL,JR,3,JL) = RDATA(1:MCHNL,JR,2,JL)
       RDATA(1:MCHNL,JR,2,JL) = QDATA(1:MCHNL)
     CASE (231)
       QDATA(1:MCHNL) = RDATA(1:MCHNL,JR,3,JL)
       RDATA(1:MCHNL,JR,3,JL) = RDATA(1:MCHNL,JR,2,JL)
       RDATA(1:MCHNL,JR,2,JL) = RDATA(1:MCHNL,JR,1,JL)
       RDATA(1:MCHNL,JR,1,JL) = QDATA(1:MCHNL)
     CASE(312)
       QDATA(1:MCHNL) = RDATA(1:MCHNL,JR,1,JL)
       RDATA(1:MCHNL,JR,1,JL) = RDATA(1:MCHNL,JR,2,JL)
       RDATA(1:MCHNL,JR,2,JL) = RDATA(1:MCHNL,JR,3,JL)
       RDATA(1:MCHNL,JR,3,JL) = QDATA(1:MCHNL)
     END SELECT

     KPC = -9
     SELECT CASE (CMP(JL))                ! Set RWTS = 0 for unused data components.
     CASE (1)                             ! when data includes all three components
       RWTS(1:MCHNL,JR,2:3,JL) = 0
       KPC(1) = 1
     CASE (2)
       RWTS(1:MCHNL,JR,1,JL) = 0
       RWTS(1:MCHNL,JR,3,JL) = 0
       KPC(1) = 2
     CASE (3)
       RWTS(1:MCHNL,JR,1:2,JL) = 0
       KPC(1) = 3
     CASE (12)
       RWTS(1:MCHNL,JR,3,JL) = 0
       KPC(1) = 1
       KPC(2) = 2
     CASE (13)
       RWTS(1:MCHNL,JR,2,JL) = 0
       KPC(1) = 1
       KPC(2) = 3
     CASE (23)
       RWTS(1:MCHNL,JR,1,JL) = 0
       KPC(1) = 2
       KPC(2) = 3
     CASE (123)
       KPC(1) = 1
       KPC(2) = 2
       KPC(3) = 3
     END SELECT
   END DO

   NCL = NCMPL(JL)
   DO JR = 1,NRX(JL)
     DO J1 = 1,NCL
       JC = KPC(J1)
       DO JF = 1,MCHNL
         IF (ABS (RDATA(JF,JR,JC,JL)) < DATA_FLOOR(JF)) RWTS(JF,JR,JC,JL) = 0
         RDATA(JF,JR,JC,JL) = RDATA(JF,JR,JC,JL) * RWTS(JF,JR,JC,JL)
       END DO
     END DO
   END DO

 END DO
 DEALLOCATE (QDATA,Q2DATA)

 WRITE(NW,14)
 IF (TDFD == 1) THEN
   TMS = 0.5 * (TOPN + TCLS)
   CALL WRITE_RDATA_TD (NW,NCHNL,NLINES,MRXL,MCMP,NRX,SURVEY_TYPE,LINE,IDH, &
                        RX_TYPE,UNITS,SVAZM,IPLT,YXZPLT,TMS,CMP,RDATA)
 ELSE
   CALL WRITE_RDATA_FD (NW,NFRQ,MCHNL,NLINES,MRXL,MCMP,NRX,SURVEY_TYPE,LINE,IDH, &
                        RX_TYPE,UNITS,SVAZM,ISYS,IPLT,YXZPLT,FREQ,CMP,RDATA)
 END IF

 NDT = MCHNL * 3
 ALLOCATE (QDATA(NDT))

  1 FORMAT(/T3,'Inversion controls and data for Line ',A)
  2 FORMAT( T3,'KMP =',I4,4X,'CMP =',I4)
  3 FORMAT( T3,'Time-Domain Data Floor =',G12.4,1X,A)
  4 FORMAT(/T8,'Frequency Domain Data Floors'/T8,'----------------------------' &
          //T8,'Freq     In-phase   Quadrature'/)
  5 FORMAT(/T8,'Frequency Domain Data Floors'/T8,'----------------------------' &
          //T8,'Freq     Floor'/)
  6 FORMAT(I4,F9.0,2G12.4)
  7 FORMAT(/T3,'FD_ORDER =',I2)
 10 FORMAT( T3,'Problem with Line ',A)
 13 FORMAT(T3,'NRX(',I2,') =',I2,4X,'NSTATL =',I2)
 14 FORMAT(//T3,'SURVEY DATA'/T3,'-----------'/)
   END SUBROUTINE PREPARE_INVERT_DATA

   SUBROUTINE SHOW_AND_TELL
!  ------------------------

! Prints out arrays and model in model-centred coordinates

!*** Called by MAIN

 IMPLICIT NONE
 REAL SXA,SXD,DEPTH

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

 WRITE(NW,30)
 DEPTH = 0.
 DO JL = 1, NLYR
   IF (JL > 1) DEPTH = DEPTH + THK(JL-1)
   IF(JL == NLYR) THEN
     WRITE(NW,32) NLYR,DEPTH,RES(NLYR),RMU(NLYR),REPS(NLYR),CHRG(NLYR),CFREQ(NLYR),CTAU(NLYR)
   ELSE
     WRITE(NW,33) JL,THK(JL),DEPTH,RES(JL),RMU(JL),REPS(JL),CHRG(JL),CFREQ(JL),CTAU(JL)
   END IF
 END DO

 IF (TDFD == 1) WRITE(NW,15)  ! Time-domain option
 IF (TDFD == 2) WRITE(NW,16)  ! Frequency-domain option

 WRITE(NW,53)                 ! End of input data description


 1 FORMAT(//T3,'Before computation begins, Beowulf may transform array and model coordinates'  &
           /T3,'from GPS coordinates where elevation increases positive upwards to a'     &
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
 30 FORMAT(//T11,'+------------------------------------------+'  &
            /T11,'+  Initial Layered Earth Model Parameters  +'  &
            /T11,'+------------------------------------------+'  &
           //T2,'                   Depth' &
            /T2,'Layer  Thickness   to Top    Resistivity   MU-R   EPS-R   CHRG    CFREQ    CTAU' &
            /T2,'-----  ---------   ------    -----------   ----   -----   ----    -----    ----')
 32 FORMAT(I4,11X,F11.1,G15.4,2F7.2,2F8.2,G13.2)
 33 FORMAT(I4,   2F11.1,G15.4,2F7.2,2F8.2,G13.2)
 53 FORMAT(/75('-')/T24,'END OF INPUT DATA DESCRIPTION'/75('-'))
 61 FORMAT(//T3,'Transformed Locations for Point E-field Receivers in Line',I7     &
            /T3,'----------------------------------------------------------------' &
           //T9,'Easting   Northing      Depth' &
            /T9,'-------   --------      -----')

   END SUBROUTINE SHOW_AND_TELL

   SUBROUTINE SET_RHO
!  ------------------


!  Sets up horizontal interpolation array (12 digit precision) for Hankel transforms
!  from 0.1 m to 10 km

!***  Called by READ_BEOWULF_DATA

   USE BG_Filter_coefficients
   INTEGER JRV,NRVR
   REAL HSMX,R1
   REAL(KIND=QL), ALLOCATABLE :: B(:)
   REAL(KIND=QL) QRHO, RBASE

   HSMX = 0.
   DO JS = 1,NTX
     DO JV = 1,NVRTX(JS)
       IF (SOURCE_TYPE == 4) THEN
         DO JRV = JV+1, NVRTX(JS)
           R1 = (SXN(JV,JS) - SXN(JRV,JS))**2 + (SXE(JV,JS) - SXE(JRV,JS))**2
           HSMX = MAX (HSMX,R1)
         END DO
       ELSE
         DO JR = 1, NRXTX(JS)
           NRVR = 1
           IF (RXID(JR,JS) == 2) NRVR = 2
           DO JRV = 1, NRVR
             R1 = (XRXTX(JR,JS,JRV) - SXN(JV,JS))**2 + (YRXTX(JR,JS,JRV) - SXE(JV,JS))**2
             HSMX = MAX (R1, HSMX)
           END DO
         END DO
       END IF
     END DO
   END DO
   HSMX = SQRT (HSMX)

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
     IF (REAL (B(JR)) > HSMX) EXIT
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
!***       Uses:  MODULE BG_Filter_coefficients

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

   SUBROUTINE WRITE_NP_INITIAL
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
!            = 120 or 220 for electric dipole receivers (SURVEY_TYPE = 1 only)
!            = 230 for point E-field receivers (SURVEY_TYPE = 1 only)
!
!  HEADER_ID is Line dependent for SURVEY_TYPE = 1


 INTEGER HID,CL,NCL
 CHARACTER(LEN=7) RES_MOD(20), THK_MOD(20)
 CHARACTER(LEN=20) SVTXT,SXTXT,QL,QL0
 DATA RES_MOD /'  RES_1','  RES_2','  RES_3','  RES_4','  RES_5','  RES_6','  RES_7','  RES_8','  RES_9',' RES_10', &
               ' RES_11',' RES_12',' RES_13',' RES_14',' RES_15',' RES_16',' RES_17',' RES_18',' RES_19',' RES_20'/
 DATA THK_MOD /'  THK_1','  THK_2','  THK_3','  THK_4','  THK_5','  THK_6','  THK_7','  THK_8','  THK_9',' THK_10', &
               ' THK_11',' THK_12',' THK_13',' THK_14',' THK_15',' THK_16',' THK_17',' THK_18',' THK_19',' THK_20'/

 WRITE(NP,1) FVERS,trim(PNAME),TRIM(TITLE)
 WRITE(NR1,1) FVERS,PNAME,TRIM(TITLE)
 WRITE(NR1,20)
 CALL GET_SURVEY_TEXT (SURVEY_TYPE,SVTXT)
 CALL GET_SOURCE_TEXT (SOURCE_TYPE,SXTXT)

 IF (TDFD == 1) THEN
   WRITE(NP,2) TRIM (ADJUSTL (SVTXT)),TRIM (ADJUSTL (SXTXT))
   WRITE(NP,3) NCHNL,REFTYM
   WRITE(NP,4) TMS(1:NCHNL)
   WRITE(NP,5) WTMS(1:NCHNL)
   IF (ISYS == 4) WRITE(NP,24)
 ELSE
   WRITE(NP,6) TRIM (ADJUSTL (SVTXT)),TRIM (ADJUSTL (SXTXT))
   WRITE(NP,7) NFRQ
   WRITE(NP,8) FREQ(1:NFRQ)
   IF (ISYS == 2) WRITE(NP,22)
 END IF

 WRITE(NP,9) NSTAT,NLINES

 HID = HEADER_ID(1)
 IF (HID > 240 .OR. HID == 140) THEN  ! Sampo, Coincident loop
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

 WRITE(NP,10) NLYR
 WRITE(NP,11) RES_MOD(1:NLYR),THK_MOD(1:NLYR-1)

  1 FORMAT(T1,'/ ',I4.4,T15,'File version number'/T1,'/ PROGRAM_NAME: ',A/T1,'/ TITLE: ',A)
  2 FORMAT(T1,'/ Time-Domain Survey_Type = ',A,4X,'Source_Type = ',A)
  3 FORMAT(T1,'/ NCH=',I3.3,4X,'REFTYM(ms)=',G12.4)
  4 FORMAT(T1,'/ TIMES(ms)=',T17,100G13.4)
  5 FORMAT(T1,'/ CHNL_WDTH(ms)=',T17,100G13.4)
  6 FORMAT(T1,'/ Frequency-Domain Survey_Type = ',A,4X,'Source_Type = ',A)
  7 FORMAT(T1,'/ NFRQ=',I3.3)
  8 FORMAT(T1,'/ FREQS(Hz) =',100G13.4)
  9 FORMAT(T1,'/ NSTAT =',I4,3X,'NLINES =',I3)
 10 FORMAT(T1,'/ LAYERS=',I2.2)
 11 FORMAT(T1,'/ MODEL_HEADER'/T1,'/ ',120A)
 20 FORMAT(T1,'/ Station   Line     East       North     Elev   ITS   ERROR    RES(1:NLYR)  DEPTH(1:NLYR-1)  THK(1:NLYR-1)')
 22 FORMAT(T1,'/ SYSTEM: SAMPO')
 24 FORMAT(T1,'/ SYSTEM: UTEM')

   END SUBROUTINE WRITE_NP_INITIAL

   SUBROUTINE WRITE_LINE_HEADER (QL,HID,CL,NCL)
!  -------------------------------------------
!
!    QL  - Line number in character form
!    HID - Header ID defining survey & line character
!    CL  - component control

!  For inversion of magnetic dipole or point electric receiver data, all three components of the
!  data are put into the mv1 file, even if some are given null status.
!  This is not the case for Sampo

 INTEGER HID,CL,NCL
 CHARACTER(LEN=20) QL
 CHARACTER(LEN=5) CHZ,CHX,CHY,CHU,CHV,CHA,CHS,CHN,CHW,CHE,CHC, &
                  RFZ,RFX,RFY,RFU,RFV,RFA,RFS,RFN,RFW,RFE, &
                  QFZ,QFX,QFY,QFU,QFV,QFA,QFS,QFN,QFW,QFE,SMP

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

 SELECT CASE (HID)
 CASE (110)            ! TD magnetic dipole receiver : X, Y, Z

   SELECT CASE (CL)
   CASE(1)
     WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHX,JT,JT=1,NCHNL)
   CASE(2)
     WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHY,JT,JT=1,NCHNL)
   CASE(3)
     WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHZ,JT,JT=1,NCHNL)
   CASE(12)
     WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHX,JT,JT=1,NCHNL), (CHY,JT,JT=1,NCHNL)
   CASE(13)
     WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHX,JT,JT=1,NCHNL), (CHZ,JT,JT=1,NCHNL)
   CASE(23)
     WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHY,JT,JT=1,NCHNL), (CHZ,JT,JT=1,NCHNL)
   CASE(123)
     WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHX,JT,JT=1,NCHNL), (CHY,JT,JT=1,NCHNL), (CHZ,JT,JT=1,NCHNL)
   END SELECT

 CASE (210)           ! FD magnetic dipole receiver : X, Y, Z
   SELECT CASE (CL)
   CASE(1)
   WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFX,JF,JF=1,NFRQ), (QFX,JF,JF=1,NFRQ)
   CASE(2)
   WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFY,JF,JF=1,NFRQ), (QFY,JF,JF=1,NFRQ)
   CASE(3)
   WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFZ,JF,JF=1,NFRQ), (QFZ,JF,JF=1,NFRQ)
   CASE(12)
   WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFX,JF,JF=1,NFRQ), (QFX,JF,JF=1,NFRQ),  &
                                              (RFY,JF,JF=1,NFRQ), (QFY,JF,JF=1,NFRQ)
   CASE(13)
   WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFX,JF,JF=1,NFRQ), (QFX,JF,JF=1,NFRQ),  &
                                              (RFZ,JF,JF=1,NFRQ), (QFZ,JF,JF=1,NFRQ)
   CASE(23)
   WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFY,JF,JF=1,NFRQ), (QFY,JF,JF=1,NFRQ),  &
                                              (RFZ,JF,JF=1,NFRQ), (QFZ,JF,JF=1,NFRQ)
   CASE(123)
   WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFX,JF,JF=1,NFRQ), (QFX,JF,JF=1,NFRQ), (RFY,JF,JF=1,NFRQ), &
                                              (QFY,JF,JF=1,NFRQ), (RFZ,JF,JF=1,NFRQ), (QFZ,JF,JF=1,NFRQ)
   END SELECT

 CASE (111)           ! Downhole TD magnetic dipole receiver : U, V, A
   SELECT CASE (CL)
   CASE(1)
     WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHU,JT,JT=1,NCHNL)
   CASE(2)
     WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHV,JT,JT=1,NCHNL)
   CASE(3)
     WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHA,JT,JT=1,NCHNL)
   CASE(12)
     WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHU,JT,JT=1,NCHNL), (CHV,JT,JT=1,NCHNL)
   CASE(13)
     WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHU,JT,JT=1,NCHNL), (CHA,JT,JT=1,NCHNL)
   CASE(23)
     WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHV,JT,JT=1,NCHNL), (CHA,JT,JT=1,NCHNL)
   CASE(123)
     WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHU,JT,JT=1,NCHNL), (CHV,JT,JT=1,NCHNL), (CHA,JT,JT=1,NCHNL)
   END SELECT

 CASE (211)           ! Downhole FD magnetic dipole receiver : U, V, A
   SELECT CASE (CL)
   CASE(1)
   WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFU,JF,JF=1,NFRQ), (QFU,JF,JF=1,NFRQ)
   CASE(2)
   WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFV,JF,JF=1,NFRQ), (QFV,JF,JF=1,NFRQ)
   CASE(3)
   WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFA,JF,JF=1,NFRQ), (QFA,JF,JF=1,NFRQ)
   CASE(12)
   WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFU,JF,JF=1,NFRQ), (QFU,JF,JF=1,NFRQ),  &
                                              (RFV,JF,JF=1,NFRQ), (QFV,JF,JF=1,NFRQ)
   CASE(13)
   WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFU,JF,JF=1,NFRQ), (QFU,JF,JF=1,NFRQ),  &
                                              (RFA,JF,JF=1,NFRQ), (QFA,JF,JF=1,NFRQ)
   CASE(23)
   WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFV,JF,JF=1,NFRQ), (QFV,JF,JF=1,NFRQ),  &
                                              (RFA,JF,JF=1,NFRQ), (QFA,JF,JF=1,NFRQ)
   CASE(123)
   WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFU,JF,JF=1,NFRQ), (QFU,JF,JF=1,NFRQ), (RFV,JF,JF=1,NFRQ), &
                                              (QFV,JF,JF=1,NFRQ), (RFA,JF,JF=1,NFRQ), (QFA,JF,JF=1,NFRQ)
   END SELECT

 CASE (112)           ! Downhole TD magnetic dipole receiver : N, S, W
   SELECT CASE (CL)
   CASE(1)
     WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHN,JT,JT=1,NCHNL)
   CASE(2)
     WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHS,JT,JT=1,NCHNL)
   CASE(3)
     WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHW,JT,JT=1,NCHNL)
   CASE(12)
     WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHN,JT,JT=1,NCHNL), (CHS,JT,JT=1,NCHNL)
   CASE(13)
     WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHN,JT,JT=1,NCHNL), (CHW,JT,JT=1,NCHNL)
   CASE(23)
     WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHS,JT,JT=1,NCHNL), (CHW,JT,JT=1,NCHNL)
   CASE(123)
     WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHN,JT,JT=1,NCHNL), (CHS,JT,JT=1,NCHNL), (CHW,JT,JT=1,NCHNL)
   END SELECT

 CASE (212)          ! Downhole FD magnetic dipole receiver : N, S, W
   SELECT CASE (CL)
   CASE(1)
   WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFN,JF,JF=1,NFRQ), (QFN,JF,JF=1,NFRQ)
   CASE(2)
   WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFS,JF,JF=1,NFRQ), (QFS,JF,JF=1,NFRQ)
   CASE(3)
   WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFW,JF,JF=1,NFRQ), (QFW,JF,JF=1,NFRQ)
   CASE(12)
   WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFN,JF,JF=1,NFRQ), (QFN,JF,JF=1,NFRQ),  &
                                              (RFS,JF,JF=1,NFRQ), (QFS,JF,JF=1,NFRQ)
   CASE(13)
   WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFN,JF,JF=1,NFRQ), (QFN,JF,JF=1,NFRQ),  &
                                              (RFW,JF,JF=1,NFRQ), (QFW,JF,JF=1,NFRQ)
   CASE(23)
   WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFS,JF,JF=1,NFRQ), (QFS,JF,JF=1,NFRQ),  &
                                              (RFW,JF,JF=1,NFRQ), (QFW,JF,JF=1,NFRQ)
   CASE(123)
   WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFN,JF,JF=1,NFRQ), (QFN,JF,JF=1,NFRQ), (RFS,JF,JF=1,NFRQ), &
                                              (QFS,JF,JF=1,NFRQ), (RFW,JF,JF=1,NFRQ), (QFW,JF,JF=1,NFRQ)
   END SELECT

 CASE (140)           ! TD coincident loop
   WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHC,JT,JT=1,NCHNL)

 CASE (120)          ! TD electric dipole receiver
   WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHE,JT,JT=1,NCHNL)

 CASE (220)          ! FD electric dipole receiver
   WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFE,JF,JF=1,NFRQ), (QFE,JF,JF=1,NFRQ)

 CASE (230)          ! FD point E-field receiver
   SELECT CASE (CL)
   CASE(1)
   WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFX,JF,JF=1,NFRQ), (QFX,JF,JF=1,NFRQ)
   CASE(2)
   WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFY,JF,JF=1,NFRQ), (QFY,JF,JF=1,NFRQ)
   CASE(3)
   WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFZ,JF,JF=1,NFRQ), (QFZ,JF,JF=1,NFRQ)
   CASE(12)
   WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFX,JF,JF=1,NFRQ), (QFX,JF,JF=1,NFRQ),  &
                                              (RFY,JF,JF=1,NFRQ), (QFY,JF,JF=1,NFRQ)
   CASE(13)
   WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFX,JF,JF=1,NFRQ), (QFX,JF,JF=1,NFRQ),  &
                                              (RFZ,JF,JF=1,NFRQ), (QFZ,JF,JF=1,NFRQ)
   CASE(23)
   WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFY,JF,JF=1,NFRQ), (QFY,JF,JF=1,NFRQ),  &
                                              (RFZ,JF,JF=1,NFRQ), (QFZ,JF,JF=1,NFRQ)
   CASE(123)
   WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFX,JF,JF=1,NFRQ), (QFX,JF,JF=1,NFRQ), (RFY,JF,JF=1,NFRQ), &
                                              (QFY,JF,JF=1,NFRQ), (RFZ,JF,JF=1,NFRQ), (QFZ,JF,JF=1,NFRQ)
   END SELECT

 CASE (250)            ! FD Sampo
   WRITE(NP,1) TRIM (ADJUSTL(QL)), HID, NCL, (SMP,JF,JF=1,NFRQ)

 CASE DEFAULT
   CALL WRITE_LOG_FILE (NLG,35,MXERR,2)   !  Write fatal error message
 END SELECT


 1 FORMAT(T1,'/ LINE_HEADER    STATION    LINE_ID ',A,4X,'HID:',I4,4X,'NCTD:',I2 /T1,'/ Data_Type  EAST  NORTH  ELEV  ',250(A,I3.3))

   END SUBROUTINE WRITE_LINE_HEADER
!==================================================================================================

 END MODULE BG_Input_routines

 PROGRAM MAIN
!------------

!*** Calls FDREAD, HSBOSS, HSBOSS_FRQ, SET_SWYTD, TDEM_3D, WRITE_LOG_FILE

!*** Calls from BG_Input_routines:
!          READ_READ_SYSTEMY_DATA, READ_MODEL_DATA, SET_TRP, SET_FRQ,

 USE BG_Input_routines
 USE BG_Frequency_select

 IMPLICIT NONE
 INTEGER QQDT(8),QQHMS(2),NFRQHS,JSTAT
 REAL PCTFIN
 REAL, ALLOCATABLE :: FRQHS(:), BPRM(:,:)
 REAL CMP_Start, CMP_Final, CMP_Delta


 OPEN(NR,FILE = 'Beowulf.cfl',STATUS = 'OLD')
 OPEN(NW,FILE = 'Beowulf.out',STATUS = 'REPLACE')

 ! CALL DATE_AND_TIME (DATE, TIME, ZONE, QQDT)
 ! QQHMS(1:2) = QQDT(5:6)
 ! WRITE(*,97) QQHMS(1:2),QQDT(3),MONTH(QQDT(2)),QQDT(1)
 ! WRITE(NW,97) QQHMS(1:2),QQDT(3),MONTH(QQDT(2)),QQDT(1)

 CALL CPU_TIME (CMP_Start)

 CALL READ_SYSTEM_AND_SURVEY_DATA    ! Set up system & survey variables

 ALLOCATE (BPRM(MRXTX,NTX))        ! Total primary (air) B fields
 BPRM = 1.                           ! B field normalisation

 CALL READ_MODEL_DATA

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

 END SELECT
 DEALLOCATE (DSTAT,SDN0,SDE0,XRXOF,YRXOF)
 IF (SOURCE_TYPE == 1) CALL SET_VERTEX_ORDER (NTX,MXVRTX,NVRTX,SXN,SXE)

 CALL SET_RHO                        ! Horizontal interpolation array

 IF (MAXVAL (KNORM) > 0) THEN     !  Set up DC primary fields
   IF (SOURCE_TYPE == 1) THEN
      CALL PRMDC_LP (NTX,MXVRTX,NVRTX,SXN,SXE,MRXTX,NRXTX,MQVR,RXID,XRXTX,YRXTX,ZRXTX,KNORM2,BPRM)
   ELSE IF (SOURCE_TYPE == 3) THEN
     CALL PRMDC_MD (NTX,SXN,SXE,SXZ,SXDIP,SXAZM,MRXTX,NRXTX,RXID,XRXTX,YRXTX,ZRXTX,KNORM2,BPRM)
   END IF
 END IF
 Write (nw, 14) bprm

 CALL SHOW_AND_TELL        ! Set & Print array & model coordinates in body-centred system
 OPEN(NRI,FILE = 'Beowulf.inv',STATUS = 'OLD')
 OPEN(NP,FILE = 'Beowulf.mv1',STATUS = 'REPLACE')
 OPEN(NR1,FILE = 'Beowulf.res',STATUS = 'REPLACE')
 CALL WRITE_NP_INITIAL
 CALL READ_PARAMETER_CONTROL
 CALL PREPARE_INVERT_DATA
 CLOSE (NR)

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

 IF (TDFD == 1) THEN   ! Time-Domain
   CALL SET_SWYTD (NSX,SWX,SWY,T0SX)  ! Compute dI/dt at the receiver
   CALL SET_TRP
   NFRQHS = NF_6PDE
   ALLOCATE (FRQHS(NFRQHS))
   FRQHS(1:NFRQHS) = FRQ_6PDE(1:NFRQHS)
 Else
   Allocate (frqhs(1))
 END IF
 WRITE(NW,1) TRIM (ADJUSTL (TITLE))!;  WRITE(*,1)  TRIM (ADJUSTL (TITLE))

 JSTAT = 0
 DO JL = 1,NLINES
   JRL = 0
   DO JS = LNTR(1,JL),LNTR(2,JL)
     DO JR = LNTR(3,JL),LNTR(4,JL)
       JSTAT = JSTAT + 1
       JRL = JRL + 1
       NDATA = 0
       DO JC = 1,MCMP
         DO JF = 1,MCHNL
           IF (RWTS(JF,JRL,JC,JL) > 0) THEN
             NDATA = NDATA + 1
             QDATA(NDATA) = RDATA(JF,JRL,JC,JL)
           END IF
         END DO
       END DO
       ALLOCATE (XDATA(NDATA),XMODL(NDATA))
       XDATA(1:NDATA) = QDATA(1:NDATA)

       RES = RES0  ! Reset starting model
       THK = THK0
       IF (NDATA < NPAR) THEN
         CALL WRITE_LOG_FILE (NLG,100,MXERR,1)
         ! WRITE(*,10) JRL,LINE(JL),NDATA,NPAR
         WRITE(NLG,10) JRL,JL,NDATA,NPAR
         WRITE(NW,10) JRL,JL,NDATA,NPAR
         CYCLE
       END IF

       CALL NLSQ2 (JL,JRL,JSTAT,JS,JR,NPAR,NDATA,XDATA,RDATA,RWTS,PCTCNV,NDSTP,KPCT,CXPAR,ELAS,LBND,UBND, &
                   MAXITS,INVPRT,NW,NP,SURVEY_TYPE,SOURCE_TYPE,NLINES,MLINES,LINE,HEADER_ID, &
                   IPLT,YXZPLT,MCMP,CMP,MRXL,MCHNL,NCHNL,NFRQ,NFRQHS,TDFD,STEP,FREQ,FRQHS,NSX,SWX,  &
                   SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,TOPN,TCLS,NFT,CURNT,ISYS,SVAZM,UNITS,RX_TYPE,   &
                   IDH,MXTX,NTX,SXN,SXE,SXZ,SXDIP,SXAZM,MXVRTX,NVRTX,NRXTX,MRXTX,RXID,MQVR,     &
                   XRXTX,YRXTX,ZRXTX,BPRM,MD1,MD2,RXAZM,RXDIP,KNORM2,NCTD,MXRHO,RHOTRP,NLYR,THK,    &
                   RES,RMUD,REPS,CHRG,CTAU,CFREQ,NR1, &
                   rxmnt, NSTAT)
       PCTFIN = 100 * REAL (JSTAT) / REAL (NSTAT)
       ! WRITE(*,2) PCTFIN,JSTAT,NSTAT
       DEALLOCATE (XDATA,XMODL)
     END DO
   END DO
 END DO

 CALL CPU_TIME (CMP_Final)
 Call date_and_time(Values = tvals)
 CMP_Delta = CMP_Final - CMP_Start
 Write (np, 11) trim(PNAME), 'inversion', tvals(1:3), tvals(5:7), CMP_delta
 Write (nw, 12) trim(PNAME), 'inversion', tvals(1:3), tvals(5:7), CMP_delta
 Write ( *, 12) trim(PNAME), 'inversion', tvals(1:3), tvals(5:7), CMP_delta

 CLOSE (NW)
 CLOSE (NLG)

 STOP
  1 FORMAT(//T2,'=========================================================================' &
            /T3,'BEGIN INVERSION - TITLE = ',A &
            /T3,'---------------')
  2 FORMAT(/T3,'---------------------------------------------------------------'   &
           /T3,F5.1,' percent completed:',I5,' of',I5,' inversions have finished.' &
           /T3,'---------------------------------------------------------------')
 10 FORMAT(T3,'For Beowulf, the number pf parameters cannot exceed the number of data points.' &
          /T3,'For station',I4,' of Line',I6,', NDATA = ',I6,';  NPAR =',I4 &
          /T3,'The inversion will ignore this station and proceed to the next one.')
11  Format ('/', / &
            '/ ', a, ' ', a, ' run completed at ', i4.4, '-', i2.2, '-', i2.2, 'T', i2.2, ':', i2.2, ':', i2.2, / &
            '/ Runtime: ', f12.2, ' secs')
12  Format (/, 2x, a, ' ', a, ' run completed at ', i4.4, '-', i2.2, '-', i2.2, 'T', i2.2, ':', i2.2, ':', i2.2, &
            /, 2x, 'Runtime: ', f12.2, ' seconds', /)
14  Format (/, 2x, '-------------------'&
            /, 2x, 'Primary fields (T): ', 1024(2x, en12.4))
90 Format (/, 2x, 'Completed sanity check on entries in ', a, '.cfl ...', &
           /, 2x, 'Computation begining ...', /)
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


 USE BG_Filter_coefficients

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

 SUBROUTINE CDCUBVAL (X_ARRAY, FUN_R, FUN_I, NVAL, X1, CD2)
!-----------------------------------------------------------

!  Uses method of CUBSPL to create complex double precision CD2 at X1 from two
!  real splined functions

!***  Called by
!***  Calls INTERV.  On exit from INTERV
!
!******************************************************************************

  IMPLICIT NONE
  INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
  INTEGER I,MFLAG,NVAL
  REAL X_ARRAY(NVAL),FUN_R(4,NVAL),FUN_I(4,NVAL),X1R
  REAL(KIND=QL) X1,H,CR,CI,A(4),B(4)
  COMPLEX(KIND=QL) CD2

!  Find index I of largest breakpoint to the left of X1.

  X1R = REAL (X1)
  CALL INTERV ( X_ARRAY, NVAL-1, X1R, I, MFLAG )
  H = X1 - REAL (X_ARRAY(I),KIND=QL)
  IF (MFLAG == -1) H = 0.D0
  A(1:4) = REAL (FUN_R(1:4,I),KIND=QL)
  B(1:4) = REAL (FUN_I(1:4,I),KIND=QL)
  CR = ((A(4)*H/3.D0 + A(3))*0.5D0*H + A(2))*H + A(1)
  CI = ((B(4)*H/3.D0 + B(3))*0.5D0*H + B(2))*H + B(1)
  CD2 = CMPLX (CR,CI,KIND=QL)

 END SUBROUTINE CDCUBVAL

 REAL FUNCTION DIST2D (X1,Y1,X2,Y2)
!----------------------------------

! Computes distance RHO between points (X1, Y1) & (X2, Y2)

 REAL X1,Y1,X2,Y2

 DIST2D = SQRT ((X1-X2)**2 + (Y1-Y2)**2)

 END FUNCTION DIST2D

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

 SUBROUTINE SET_OUTPUT_LINES_FD (JL,JRL,JS,JR,NFRQ,MCHNL,NTX,MRXTX,NLINES,MRXL,MCMP,KNORM2,UNITS, &
                                 RX_TYPE,ISYS,IDH,SVAZM,MD1,MD2,RXAZM,RXDIP,CURNT,BPRM,BFD,BFTL, &
                                 RXMNT)
!-----------------------------------------------------------------------------------------------

!** Called by MAIN

!  The Beowulf version operates on one receiver per call.
!  JRL is the line-reference receiver corresponding to the Tx JS -Rx JR

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

!   NFRQ         : number of frequencies
!   MCHNL        = NFRQ for Sampo;  = 2*NFRQ otherwise
!   NTX          : total number of transmitters
!   MRXTX        : maximum number of receivers per transmitter position
!   NLINES       : number of receiver lines
!   MRXL         : maximum number of receiver positions for any line
!   MCMP         = 1 for Sampo;  = 3 otherwise
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


 INTEGER MCHNL,MCMP,NFRQ,NTX,MRXTX,NLINES,MRXL,MD1,MD2, &
         KNORM2(MRXTX,NTX),ISYS,JS,JR,JL,JRL,JC,JF1,JF2
 INTEGER, DIMENSION(NLINES) :: UNITS,RX_TYPE,IDH
 REAL, DIMENSION(NLINES) :: SVAZM
 REAL, DIMENSION(MD1,MD2) :: RXAZM,RXDIP
 REAL CURNT(NFRQ),BPRM(MRXTX,NTX),BFTL(MCHNL,MRXL,MCMP,NLINES),QR(3),QI(3), &
      QXR,QXI,QYR,QYI,CAZ,SAZ,CAZ0,SAZ0,SDP,CDP,A1
 COMPLEX BFD(NFRQ,MRXTX,NTX,3)

 Real :: RXMNT(NLINES)

!================================================================

 IF (ISYS == 2) THEN  !  Sampo based on absoluute ratio
   UNITS = 31
   CAZ0 = COS (SVAZM (JL))
   SAZ0 = SIN (SVAZM (JL))
   IF (ABS (CAZ0) < 1.E-4) CAZ0 = 0.
   IF (ABS (SAZ0) < 1.E-4) SAZ0 = 0.
   BFD(1:NFRQ,JR,JS,1) = CAZ0 * BFD(1:NFRQ,JR,JS,1) + SAZ0 * BFD(1:NFRQ,JR,JS,2)
   BFTL(1:MCHNL,JRL,1,JL) = ABS (BFD(1:NFRQ,JR,JS,3) / BFD(1:NFRQ,JR,JS,1))
!**************
   RETURN
!**************
 END IF
!================================================================

 DO JC = 1,3
   IF (KNORM2(JR,JS) == 0) THEN
     BFD(1:NFRQ,JR,JS,JC) = BFD(1:NFRQ,JR,JS,JC) * CURNT(1:NFRQ)
   ELSE
     BFD(1:NFRQ,JR,JS,JC) = BFD(1:NFRQ,JR,JS,JC) / BPRM(JR,JS)
   END IF
 END DO

!  Convert to LINE-based output and apply units

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
  a1 = a1 * rxmnt(jl)       ! 20170704 DWA addition

 DO JC = 1,MCMP
   BFTL(1:NFRQ,JRL,JC,JL) = A1 * REAL (BFD(1:NFRQ,JR,JS,JC))
   BFTL(NFRQ+1:2*NFRQ,JRL,JC,JL) = A1 * AIMAG (BFD(1:NFRQ,JR,JS,JC))
 END DO

!  For surface dipole surveys, reorient component 1 from north to lie along
!  the Survey X axis and component 2 from east to lie along the Survey Y axis.
!  For downhole surveys apply u,V,A transformation.

 IF (RX_TYPE(JL) > 1) RETURN  ! Skip if not magnetic dipole receiver

 CAZ0 = COS (SVAZM (JL))
 SAZ0 = SIN (SVAZM (JL))
 IF (ABS (CAZ0) < 1.E-4) CAZ0 = 0.
 IF (ABS (SAZ0) < 1.E-4) SAZ0 = 0.
 DO JF1 = 1,NFRQ
   JF2 = JF1 + NFRQ
   QR(1:3) = BFTL(JF1,JRL,1:3,JL)
   QI(1:3) = BFTL(JF2,JRL,1:3,JL)
   IF (IDH(JL) == 0) THEN    !  Surface survey
     BFTL(JF1,JRL,1,JL) =  CAZ0 * QR(1) + SAZ0 * QR(2)    ! X component parallel to survey direction
     BFTL(JF1,JRL,2,JL) = -SAZ0 * QR(1) + CAZ0 * QR(2)    ! Y component transverse to survey direction
     BFTL(JF2,JRL,1,JL) =  CAZ0 * QI(1) + SAZ0 * QI(2)    ! X component parallel to survey direction
     BFTL(JF2,JRL,2,JL) = -SAZ0 * QI(1) + CAZ0 * QI(2)    ! Y component transverse to survey direction
   ELSE
     CAZ = COS (RXAZM(JRL,JL))
     SAZ = SIN (RXAZM(JRL,JL))
     CDP = COS (RXDIP(JRL,JL))
     SDP = SIN (RXDIP(JRL,JL))
     IF (ABS (CAZ) < 1.E-4) CAZ = 0.
     IF (ABS (SAZ) < 1.E-4) CAZ = 0.
     IF (ABS (CDP) < 1.E-4) CDP = 0.
     IF (ABS (SDP) < 1.E-4) CDP = 0.
     QXR =  CAZ * QR(1) + SAZ * QR(2)                  ! local horizontal radial component
     QXI =  CAZ * QI(1) + SAZ * QI(2)                  ! local horizontal radial component
     QYR = -SAZ * QR(1) + CAZ * QR(2)                  ! local horizontal transverse component
     QYI = -SAZ * QI(1) + CAZ * QI(2)                  ! local horizontal transverse component
     BFTL(JF1,JRL,3,JL) =  CDP * QR(3) + SDP * QXR       ! Axial component
     BFTL(JF2,JRL,3,JL) =  CDP * QI(3) + SDP * QXI       ! Axial component

     IF (IDH(JL) == 1) THEN                              ! Conventional U,V,A processing
       BFTL(JF1,JRL,1,JL) = -SDP * QR(3) + CDP * QXR        !   local U component
       BFTL(JF2,JRL,1,JL) = -SDP * QI(3) + CDP * QXI        !   local U component
       BFTL(JF1,JRL,2,JL) = QYR                             !   local V component
       BFTL(JF2,JRL,2,JL) = QYI                             !   local V component

     ELSE IF (IDH(JL) == 2) THEN                         ! Utem style U,V,A processing
       QXR =  CAZ0 * QR(1) + SAZ0 * QR(2)                 !   Line-referenced horizontal radial component
       QXI =  CAZ0 * QI(1) + SAZ0 * QI(2)                 !   Line-referenced horizontal radial component
       BFTL(JF1,JRL,1,JL) = -SDP * QR(3) + CDP * QXR        !   Line-referenced U component
       BFTL(JF2,JRL,1,JL) = -SDP * QI(3) + CDP * QXI        !   Line-referenced U component

       BFTL(JF1,JRL,2,JL) = SQRT (SUM (QR(1:3)**2) - BFTL(JF1,JRL,1,JL)**2 - BFTL(JF1,JRL,3,JL)**2)
       BFTL(JF2,JRL,2,JL) = SQRT (SUM (QI(1:3)**2) - BFTL(JF2,JRL,1,JL)**2 - BFTL(JF2,JRL,3,JL)**2)
     END IF
   END IF
 END DO

 END SUBROUTINE SET_OUTPUT_LINES_FD

 SUBROUTINE SET_OUTPUT_LINES_TD (JL,JRL,JS,JR,NCHNL,NTX,MRXTX,NLINES,MRXL,MCMP,KNORM2,RX_TYPE, &
                                 UNITS,ISYS,IDH,SVAZM,MD1,MD2,RXAZM,RXDIP,CURNT,BPRM,BTD,BFTL, &
                                 rxmnt)
!---------------------------------------------------------------------------------------------

!** Called by MAIN
!
!  The Beowulf version operates on one receiver per call.
!  JRL is the line-reference receiver corresponding to the Tx JS -Rx JR
!
!   INPUT:  BTD (JT,JR,JS,JC) : component JC of time-domain output for channel JT
!                               of receiver JR belonging to transmitter JS
!                               The units are in teslas or teslas / sec.
!                               Components are: north, east, vertical


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


 INTEGER NCHNL,NTX,MRXTX,NLINES,MRXL,MCMP,MD1,MD2,KNORM2(MRXTX,NTX), &
         ISYS,JS,JR,JL,JRL,JC,JT,JTR
 INTEGER, DIMENSION(NLINES) :: UNITS,RX_TYPE,IDH
 REAL, DIMENSION(NLINES) :: SVAZM
 REAL, DIMENSION(MD1,MD2) :: RXAZM,RXDIP
 REAL CURNT(1),BPRM(MRXTX,NTX),BTD(NCHNL,MRXTX,NTX,3),BFTL(NCHNL,MRXL,MCMP,NLINES), &
      Q1(1:3),QX,QY,QZ,CAZ,SAZ,CAZ0,SAZ0,SDP,CDP,PHI,QT(NCHNL),ALF(3,3),RHO,A1
 Real :: rxmnt(nlines)

 ALF = 0.
 IF (KNORM2(JR,JS) /= 0) THEN
   DO JC = 1,3
     BTD(1:NCHNL,JR,JS,JC) = BTD(1:NCHNL,JR,JS,JC) / (BPRM(JR,JS) * CURNT(1))
   END DO
 END IF

!  Convert to LINE-based output and apply units

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
 a1 = a1 * rxmnt(jl)        ! 20170704 DWA addition

 DO JC = 1,MCMP
   BFTL(1:NCHNL,JRL,JC,JL) = A1 * BTD(1:NCHNL,JR,JS,JC)
 END DO

!For surface dipole surveys only, reorient component 1 from north to lie along
!the Survey X axis and component 2 from east to lie along the Survey Y axis.

 IF (RX_TYPE(JL) > 1) RETURN  ! Skip if not magnetic dipole receiver

 CAZ0 = COS (SVAZM (JL))
 SAZ0 = SIN (SVAZM (JL))
 IF (ABS (CAZ0) < 1.E-4) CAZ0 = 0.
 IF (ABS (SAZ0) < 1.E-4) SAZ0 = 0.
 IF (IDH(JL) == 0) THEN    !  Surface survey
   DO JT = 1,NCHNL
     Q1(1:2) = BFTL(JT,JRL,1:2,JL)
     BFTL(JT,JRL,1,JL) =  CAZ0 * Q1(1) + SAZ0 * Q1(2)
     BFTL(JT,JRL,2,JL) = -SAZ0 * Q1(1) + CAZ0 * Q1(2)
   END DO
 ELSE
   CAZ = COS (RXAZM(JRL,JL))
   SAZ = SIN (RXAZM(JRL,JL))
   CDP = COS (RXDIP(JRL,JL))
   SDP = SIN (RXDIP(JRL,JL))
   DO JT = 1,NCHNL
     Q1(1:3) = BFTL(JT,JRL,1:3,JL)
     QZ = Q1(3)
     IF (IDH(JL) == 1) THEN                           ! Express BFTL in U,V,A
       QX =  CAZ * Q1(1) + SAZ * Q1(2)                 ! local horizontal radial component
       QY = -SAZ * Q1(1) + CAZ * Q1(2)                 ! local horizontal transverse component
       BFTL(JT,JRL,3,JL) =  CDP * QZ + SDP * QX         ! Axial component
       BFTL(JT,JRL,1,JL) = -SDP * QZ + CDP * QX          !   local U component
       BFTL(JT,JRL,2,JL) = QY                            !   local V component

     ELSE IF (IDH(JL) == 2) THEN                 ! Express BFTL in S, N, W
       QX =  CAZ0 * Q1(1) + SAZ0 * Q1(2)         !   X radial component in survey system
       QY = -SAZ0 * Q1(1) + CAZ0 * Q1(2)         !   Y transverse component in survey system
       PHI = RXAZM(JRL,JL) - SVAZM(JL)            !   local - survey aximuth

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

       BFTL(JT,JRL,1,JL) = ALF(1,1) * QX + ALF(1,2) * QY + ALF(1,3) * QZ  ! S
       BFTL(JT,JRL,2,JL) =                 ALF(2,2) * QY + ALF(2,3) * QZ  ! N
       BFTL(JT,JRL,3,JL) = ALF(3,1) * QX + ALF(3,2) * QY + ALF(3,3) * QZ  ! W
     END IF
   END DO
 END IF
 IF (ISYS == 4) THEN        !  reverse channels for UTEM & subtract channel 1 response
   DO JC = 1,3
     DO JT = 1,NCHNL
       JTR = NCHNL - JT + 1
       QT(JT) = BFTL(JTR,JRL,JC,JL)
     END DO
     DO JT = 1,NCHNL
       BFTL(JT,JRL,JC,JL) = QT(JT)
       IF (JT > 1) BFTL(JT,JRL,JC,JL) = BFTL(JT,JRL,JC,JL) - BFTL(1,JRL,JC,JL)
     END DO
   END DO
 END IF

 END SUBROUTINE SET_OUTPUT_LINES_TD

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
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
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
!  assigning the coordinates of the receivers by group.  Finally arrays are
!  constructed containing the the plot coordinates of every Rx-Tx combination
!  used in the survey.
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
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
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
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
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
!  assigning the coordinates of the receivers by group.
!  Finally arrays are constructed containing
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
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
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

 SUBROUTINE SET_KFG (I1,NLYR,SXLYR,RXLYR,KFG)
!--------------------------------------------

!  Determines the value of KFG needed for layer coefficient computation
!  I1 = 1 for magnetic source or 2 for electric source.

 IMPLICIT NONE
 INTEGER I1,I2,I3,SXLYR,RXLYR,NLYR,KFG

 I2 = 1; I3 = 1
 IF (SXLYR == 0) I2 = 0
 IF (RXLYR == 0) I3 = 0
 IF (SXLYR == NLYR) I2 = 3
 IF (RXLYR == NLYR) I3 = 3
 IF (I2 == 1 .AND. I3 == 1) THEN
   IF (RXLYR > SXLYR) I3 = 2
   IF (RXLYR < SXLYR) I2 = 2
 END IF
 KFG = 100*I1 + 10*I2 + I3

 END SUBROUTINE SET_KFG

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
   ! FOLD(JT) = CUBVAL (TRP,YPLS,NTYRP,X) - CUBVAL (TRP,YPLS,NTYRP,XP)
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
!  of the earth.  In Beowulf, it is used for electric field computations.
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

 SUBROUTINE VALIDATE_KMP (NLG,MXERR,J,NLINES,CMP,KMP)
!-----------------------------------------------------

 IMPLICIT NONE
 INTEGER NLG,MXERR,J,NLINES,QCMP
 INTEGER, DIMENSION(NLINES) :: CMP,KMP

 SELECT CASE (KMP(J))
 CASE (1)
   QCMP = 1
 CASE(2)
   QCMP = 2
 CASE(3)
   QCMP = 3
 CASE (12,21)
   QCMP = 12
 CASE (13,31)
   QCMP = 13
 CASE (23,32)
   QCMP = 23
 CASE (123,132,231,213,321,312)
   QCMP = 123
 CASE DEFAULT
   CALL WRITE_LOG_FILE (NLG,52,MXERR,2)
 END SELECT

 SELECT CASE (CMP(J))
 CASE(1)
   IF (QCMP /= 1 .AND. QCMP /= 12 .AND. QCMP /= 13 .AND. QCMP /= 123) THEN
     CALL WRITE_LOG_FILE (NLG,54,MXERR,2)
     WRITE(NLG,1) J,J
   END IF
 CASE(2)
   IF (QCMP /= 2 .AND. QCMP /= 12 .AND. QCMP /= 23 .AND. QCMP /= 123) THEN
     CALL WRITE_LOG_FILE (NLG,54,MXERR,2)
     WRITE(NLG,2) J,J
   END IF
 CASE(3)
   IF (QCMP /= 3 .AND. QCMP /= 13 .AND. QCMP /= 23 .AND. QCMP /= 123) THEN
     CALL WRITE_LOG_FILE (NLG,54,MXERR,2)
     WRITE(NLG,3) J,J
   END IF
 CASE(12)
   IF (QCMP /= 12 .AND. QCMP /= 123) THEN
     CALL WRITE_LOG_FILE (NLG,54,MXERR,2)
     WRITE(NLG,4) J,J
   END IF
 CASE(13)
   IF (QCMP /= 13 .AND. QCMP /= 123) THEN
     CALL WRITE_LOG_FILE (NLG,54,MXERR,2)
     WRITE(NLG,5) J,J
   END IF
 CASE(23)
   IF (QCMP /= 23 .AND. QCMP /= 123) THEN
     CALL WRITE_LOG_FILE (NLG,54,MXERR,2)
     WRITE(NLG,6) J,J
   END IF
 CASE(123)
   IF (QCMP /= 123) THEN
     CALL WRITE_LOG_FILE (NLG,54,MXERR,2)
     WRITE(NLG,7) J,J
   END IF
 END SELECT

 1 FORMAT(T3,'If CMP(',I2,') = 1, KMP(',I2,') must be 1, 12, 21, 13, 31, or a permutation of 123')
 2 FORMAT(T3,'If CMP(',I2,') = 2, KMP(',I2,') must be 2, 12, 21, 23, 32, or a permutation of 123')
 3 FORMAT(T3,'If CMP(',I2,') = 3, KMP(',I2,') must be 3, 13, 31, 23, 32, or a permutation of 123')
 4 FORMAT(T3,'If CMP(',I2,') = 12, KMP(',I2,') must be 12, 21 or a permutation of 123')
 5 FORMAT(T3,'If CMP(',I2,') = 13, KMP(',I2,') must be 13, 31 or a permutation of 123')
 6 FORMAT(T3,'If CMP(',I2,') = 23, KMP(',I2,') must be 23, 32 or a permutation of 123')
 7 FORMAT(T3,'If CMP(',I2,') = 123, KMP(',I2,') must be a permutation of 123')

 END SUBROUTINE VALIDATE_KMP

 SUBROUTINE VALIDATE_LINE (NLG,MXERR,LINE_CHK,NSTATL,JL,NLINES,LINE,NRX)
!----------------------------------------------------------------------

!*** Call WRITE_LOG_FILE
!*** Called by PREPARE_INVERT_DATA

! Validates Line number LINE_CHK aagainst Line(J).
! Validates NSTATL against NRX, the number of stations on Line J

 IMPLICIT NONE
 INTEGER NLG,NLINES,LINE_CHK,NSTATL,JL,LINE(NLINES),NRX(NLINES),MXERR
 CHARACTER(LEN=20) CTXT(2),QL0

 IF (LINE_CHK /= LINE(JL)) THEN
   CALL WRITE_LOG_FILE (NLG,50,MXERR,2)
   WRITE(QL0,*) LINE(JL)
   READ(QL0,'(A)') CTXT(1)
   WRITE(QL0,*) LINE_CHK
   READ(QL0,'(A)') CTXT(2)
   WRITE(NLG,90) JL,CTXT(1:2)
 END IF

 IF (NSTATL /= NRX(JL)) THEN
   CALL WRITE_LOG_FILE (NLG,51,MXERR,2)
   WRITE(NLG,91) JL,NRX(JL),NSTATL
 END IF

 90 FORMAT(T3,'LINE(',I2,') =',A,4X,'LINE_CHK =',A)
 91 FORMAT(T3,'NRX(',I2,') =',I4,4X,'NSTATL =',I4)

 END SUBROUTINE VALIDATE_LINE

 SUBROUTINE WRITE_INVMDL (NW,FINAL,RMSERR,THING,ITS,NLYR,NPAR,XPAR,RES,THK,DEPTH,IMPORT)
!---------------------------------------------------------------------------------------

!*** Called by: NLSQ2

! Transforms XPAR to user parameters and writes intermediate and final models
!
!  NW     : output unit
!  FINAL  : true if final model, false if intermediate model.
!  ITS    : print model after iteration ITS
!  THING  : Noise to signal ratio if FINAL; RSVT otherwise
!  NLYR   : number of layers
!  NPAR   = 2*NLYR-1
!  XPAR   : transformed parameter array

 IMPLICIT NONE
 INTEGER NW,NLYR,NPAR,ITS,JL,JL1
 REAL, DIMENSION(NLYR) :: RES,THK,DEPTH,CND
 REAL, DIMENSION(NPAR) :: XPAR,IMPORT
 REAL RMSERR,THING
 LOGICAL FINAL

 THK = 1.E5
 DEPTH = 0.
 CND = 0.

 IF (FINAL) THEN
   WRITE(NW,2) ITS,RMSERR
   ! WRITE(*,2) ITS,RMSERR
 ELSE
   WRITE(NW,1) ITS,RMSERR,THING
   ! WRITE(*,1) ITS,RMSERR,THING
 END IF

 RES(1:NLYR) = EXP (XPAR(1:NLYR))
 DO JL = 1,NLYR - 1
   THK(JL) = EXP (XPAR(NLYR+JL))
   DEPTH(JL) = SUM (THK(1:JL))
   CND(JL) = THK(JL) / RES(JL)
 END DO

 IF (NLYR > 1) THEN
   IF (FINAL) THEN
     WRITE(NW,13); !WRITE(*,13)
     DO JL = 1,NLYR-1
       JL1 = JL + NLYR
       WRITE(NW,15) JL,RES(JL),DEPTH(JL),THK(JL),CND(JL),IMPORT(JL),IMPORT(JL1)
       ! WRITE(*,15)  JL,RES(JL),DEPTH(JL),THK(JL),CND(JL),IMPORT(JL),IMPORT(JL1)
     END DO
     WRITE(NW,16) RES(NLYR),IMPORT(NLYR); 
     ! WRITE(*,16) RES(NLYR),IMPORT(NLYR)
   ELSE
     WRITE(NW,3); !WRITE(*,3)
     DO JL = 1,NLYR-1
       WRITE(NW,5) JL,RES(JL),DEPTH(JL),THK(JL),CND(JL)
       ! WRITE(*,5)  JL,RES(JL),DEPTH(JL),THK(JL),CND(JL)
     END DO
     WRITE(NW,6) RES(NLYR); !WRITE(*,6)  RES(NLYR)
   END IF
 ELSE
   WRITE(NW,4) RES(1)
   ! WRITE(*,4) RES(1)
 END IF
 WRITE(NW,20); !WRITE(*,20)

  1 FORMAT(/T3,'Model Description After',I3,' Iterations:  RMS error =',F7.2,'  RSVT =',F7.3)
  2 FORMAT(/T3,'Final Model After',I3,' Iterations:  RMS error =',F7.2)
  3 FORMAT(/T3,'Layer  Resistivity  Depth  Thickness  Conductance' &
           /T3,'-----  -----------  -----  ---------  -----------')
  4 FORMAT(/T3,'Host resistivity =',G15.4)

  5 FORMAT(I5,G15.4,F7.1,F9.1,F12.3)
  6 FORMAT(T5,'B',G15.4)
 13 FORMAT(/T3,'Layer  Resistivity  Depth  Thickness  Conductance   ResImport   ThkImport' &
           /T3,'-----  -----------  -----  ---------  -----------   ---------   ---------')
 15 FORMAT(I5,G15.4,F7.1,F9.1,F12.3,T55,F6.2,F12.2)
 16 FORMAT(T5,'B',G15.4,T66,F7.2)
 20 FORMAT(2X)

 END SUBROUTINE WRITE_INVMDL

 SUBROUTINE WRITE_INVDATA_TD (JL,JRL,NW,NP,NCHNL,NLINES,MRXL,MCMP,SURVEY_TYPE,LINE,IDH,RX_TYPE, &
                              YXZPLT,CMP,BFTL,RDATA,RWTS)
!---------------------------------------------------------------------------------------------

!  Writes time domain output to unit NW
!
!*** Called by: MAIN
!***     Calls:
!
!   NW,NP          : output unit numbers
!   NLINES          : number of lines
!   MRXL            : maximum number of receivers per line
!   MCMP            = 1 for coincident loop;  = 3 otherwise
!   NCHNL           : number of channels
!   SURVEY_TYPE     = 1: general transmitter with independent receiver arrays
!                   = 2: moving rectangular loop with one or more fixed offset MD receivers
!                   = 3: fixed offset MD - MD surface surface
!                   = 4: coincident Loop
!                   = 5: downhole MD-MD probe
!   LINE(L)         : line number of Line L
!   IDH(L)          : surface (0);  local U,V,A (1);  modified U,V,A (2)
!   RXID(L)         : receiver type Line L - mag dipole (1); electric dipole (2)
!   SVAZM           : survey azimuth (radians) of Line L
!   YXZPLT(1:3,I,L) : GPS east, GPS north, RL  plot coordinate for Ith receiver of Line L
!   CMP             : component selection
!   BFTL(I,J,K,L)   : the Kth component measured response at frequency I from receiver J,
!                     of Line L, transmitter K (modelled)
!   RDATA(I,J,K,L)  : the Kth component measured response at frequency I from receiver J,
!                     of Line L, transmitter K (inversion data)
!   RWTS(I,J,K,L)   : inversion weights corresponding to RDATA

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 INTEGER NW,NP,NLINES,MRXL,MCMP,NCHNL,SURVEY_TYPE,JL,JRL,LC
 INTEGER, DIMENSION(NLINES) :: CMP,LINE,RX_TYPE,IDH
 INTEGER RWTS(NCHNL,MRXL,MCMP,NLINES)
 REAL, DIMENSION(NCHNL,MRXL,MCMP,NLINES) :: BFTL,RDATA
 REAL YTR(NCHNL)
 REAL(KIND=QL) YXZPLT(3,MRXL,NLINES)
 CHARACTER(LEN=20) CLINE,QL0
 CHARACTER(LEN=8) CHN(150)
 DATA CHN(1:150) &
   /'CHNL 1  ','CHNL 2  ','CHNL 3  ','CHNL 4  ','CHNL 5  ','CHNL 6  ', &
    'CHNL 7  ','CHNL 8  ','CHNL 9  ','CHNL 10 ','CHNL 11 ','CHNL 12 ', &
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

 LC = CMP(JL)
 WRITE(QL0,*) LINE(JL)
 READ(QL0,'(A)') CLINE           ! Line number

 ! Print *, JRL

 SELECT CASE (CMP(JL))
 CASE(1)
   Write(NP,27) JRL, 'W', YXZPLT(1:3,JRL,JL), RWTS(1:NCHNL, JRL, 1, JL)
   WRITE(NP,21) JRL, 'F', YXZPLT(1:3,JRL,JL), RDATA(1:NCHNL,JRL,1,JL)
   WRITE(NP,22) JRL, 'M', YXZPLT(1:3,JRL,JL), BFTL(1:NCHNL,JRL,1,JL)
 CASE(2)                       
   Write(NP,27) JRL, 'W', YXZPLT(1:3,JRL,JL), RWTS(1:NCHNL, JRL, 2, JL)
   WRITE(NP,21) JRL, 'F', YXZPLT(1:3,JRL,JL), RDATA(1:NCHNL,JRL,2,JL)
   WRITE(NP,22) JRL, 'M', YXZPLT(1:3,JRL,JL), BFTL(1:NCHNL,JRL,2,JL)
 CASE(3)                       
   Write(NP,27) JRL, 'W', YXZPLT(1:3,JRL,JL), RWTS(1:NCHNL, JRL, 3, JL)
   WRITE(NP,21) JRL, 'F', YXZPLT(1:3,JRL,JL), RDATA(1:NCHNL,JRL,3,JL)
   WRITE(NP,22) JRL, 'M', YXZPLT(1:3,JRL,JL), BFTL(1:NCHNL,JRL,3,JL)
 CASE(12)                      
   Write(NP,27) JRL, 'W', YXZPLT(1:3,JRL,JL), RWTS(1:NCHNL, JRL, 1, JL), RWTS(1:NCHNL, JRL, 2, JL)
   WRITE(NP,21) JRL, 'F', YXZPLT(1:3,JRL,JL), RDATA(1:NCHNL,JRL,1,JL), RDATA(1:NCHNL,JRL,2,JL)
   WRITE(NP,22) JRL, 'M', YXZPLT(1:3,JRL,JL), BFTL(1:NCHNL,JRL,1,JL),  BFTL(1:NCHNL,JRL,2,JL)
 CASE(13)                      
   Write(NP,27) JRL, 'W', YXZPLT(1:3,JRL,JL), RWTS(1:NCHNL, JRL, 1, JL), RWTS(1:NCHNL, JRL, 3, JL)
   WRITE(NP,21) JRL, 'F', YXZPLT(1:3,JRL,JL), RDATA(1:NCHNL,JRL,1,JL), RDATA(1:NCHNL,JRL,3,JL)
   WRITE(NP,22) JRL, 'M', YXZPLT(1:3,JRL,JL), BFTL(1:NCHNL,JRL,1,JL),  BFTL(1:NCHNL,JRL,3,JL)
 CASE(23)                      
   Write(NP,27) JRL, 'W', YXZPLT(1:3,JRL,JL), RWTS(1:NCHNL, JRL, 2, JL), RWTS(1:NCHNL, JRL, 3, JL)
   WRITE(NP,21) JRL, 'F', YXZPLT(1:3,JRL,JL), RDATA(1:NCHNL,JRL,2,JL), RDATA(1:NCHNL,JRL,3,JL)
   WRITE(NP,22) JRL, 'M', YXZPLT(1:3,JRL,JL), BFTL(1:NCHNL,JRL,2,JL),  BFTL(1:NCHNL,JRL,3,JL)
 CASE(123)                     
   Write(NP,27) JRL, 'W', YXZPLT(1:3,JRL,JL), RWTS(1:NCHNL, JRL, 1, JL), RWTS(1:NCHNL, JRL, 2, JL), RWTS(1:NCHNL, JRL, 3, JL)
   WRITE(NP,21) JRL, 'F', YXZPLT(1:3,JRL,JL), RDATA(1:NCHNL,JRL,1,JL), RDATA(1:NCHNL,JRL,2,JL), RDATA(1:NCHNL,JRL,3,JL)
   WRITE(NP,22) JRL, 'M', YXZPLT(1:3,JRL,JL), BFTL(1:NCHNL,JRL,1,JL),  BFTL(1:NCHNL,JRL,2,JL),  BFTL(1:NCHNL,JRL,3,JL)
 END SELECT

 IF (RX_TYPE(JL) == 1) THEN      ! magnetic dipole or electric monopole output
   IF (LC == 3 .OR. LC == 13 .OR. LC == 23 .OR. LC == 123) THEN
     SELECT CASE (IDH(JL))               ! Z, A or W component
     CASE (0)
       WRITE(NW,3) JRL,TRIM (ADJUSTL (CLINE))
     CASE (1)
       WRITE(NW,6) JRL,TRIM (ADJUSTL (CLINE))
     CASE (2)
       WRITE(NW,9) JRL,TRIM (ADJUSTL (CLINE))
     END SELECT
     WRITE(NW,16) CHN(1:NCHNL)
     Write(nw, 28) rwts(1:nchnl, jrl, 3, jl)
     WRITE(NW,23) RDATA(1:NCHNL,JRL,3,JL)
     WRITE(NW,24) BFTL(1:NCHNL,JRL,3,JL)
     CALL MISFIT_TD(3)
     WRITE(NW,25) YTR(1:NCHNL)
   END IF

   IF (LC == 1 .OR. LC == 13 .OR. LC == 12 .OR. LC == 123) THEN
     SELECT CASE (IDH(JL))               ! X, U or S component
     CASE (0)
       WRITE(NW,1) JRL,TRIM (ADJUSTL (CLINE))
     CASE (1)
       WRITE(NW,4) JRL,TRIM (ADJUSTL (CLINE))
     CASE (2)
       WRITE(NW,7) JRL,TRIM (ADJUSTL (CLINE))
     END SELECT
     WRITE(NW,16) CHN(1:NCHNL)
     Write(nw, 28) rwts(1:nchnl, jrl, 1, jl)
     WRITE(NW,23) RDATA(1:NCHNL,JRL,1,JL)
     WRITE(NW,24) BFTL(1:NCHNL,JRL,1,JL)
     CALL MISFIT_TD(1)
     WRITE(NW,25) YTR(1:NCHNL)
   END IF

   IF (LC == 2 .OR. LC == 12 .OR. LC == 23 .OR. LC == 123) THEN
     SELECT CASE (IDH(JL))               ! Y, V or N component
     CASE (0)
       WRITE(NW,2) JRL,TRIM (ADJUSTL (CLINE))
     CASE (1)
       WRITE(NW,5) JRL,TRIM (ADJUSTL (CLINE))
     CASE (2)
       WRITE(NW,8) JRL,TRIM (ADJUSTL (CLINE))
     END SELECT
     WRITE(NW,16) CHN(1:NCHNL)
     Write(nw, 28) rwts(1:nchnl, jrl, 2, jl)
     WRITE(NW,23) RDATA(1:NCHNL,JRL,2,JL)
     WRITE(NW,24) BFTL(1:NCHNL,JRL,2,JL)
     CALL MISFIT_TD(2)
     WRITE(NW,25) YTR(1:NCHNL)
   END IF

 ELSE IF (SURVEY_TYPE == 4) THEN         ! Coincident loop output
   WRITE(NW,10) JRL,TRIM (ADJUSTL (CLINE))
   WRITE(NW,16) CHN(1:NCHNL)
   Write(nw, 28) rwts(1:nchnl, jrl, 1, jl)
   WRITE(NW,23) RDATA(1:NCHNL,JRL,1,JL)
   WRITE(NW,24) BFTL(1:NCHNL,JRL,1,JL)
   CALL MISFIT_TD(1)
   WRITE(NW,25) YTR(1:NCHNL)
 ELSE IF (RX_TYPE(JL) == 2) THEN        ! electric dipole output
   WRITE(NW,11) JRL,TRIM (ADJUSTL (CLINE))
   WRITE(NW,16) CHN(1:NCHNL)
   Write(nw, 28) rwts(1:nchnl, jrl, 1, jl)
   WRITE(NW,23) RDATA(1:NCHNL,JRL,1,JL)
   WRITE(NW,24) BFTL(1:NCHNL,JRL,1,JL)
   CALL MISFIT_TD(1)
   WRITE(NW,25) YTR(1:NCHNL)
 END IF

  1 FORMAT(/T10,'X : Radial Component data for Station',I4,' of Line ',A, &
           /T10,'-------------------------------------------------------')
  2 FORMAT(/T10,'Y : Tangential Component data for Station',I4,' of Line ',A, &
           /T10,'-----------------------------------------------------------')
  3 FORMAT(/T10,'Z : Vertical Component data for Station',I4,' of Line ',A, &
           /T10,'---------------------------------------------------------')
  4 FORMAT(/T10,'U : Slope Component data for Station',I4,' of Line ',A, &
           /T10,'------------------------------------------------------')
  5 FORMAT(/T10,'V : Horizontal Component data for Station',I4,' of Line ',A, &
           /T10,'-----------------------------------------------------------')
  6 FORMAT(/T10,'A : Axial Component data for Station',I4,' of Line ',A, &
           /T10,'------------------------------------------------------')
  7 FORMAT(/T10,'S : In-section Component data for Station',I4,' of Line ',A, &
           /T10,'-----------------------------------------------------------')
  8 FORMAT(/T10,'N : Out-section Component data for Station',I4,' of Line ',A, &
           /T10,'------------------------------------------------------------')
  9 FORMAT(/T10,'W : Axial Component data for Station',I4,' of Line ',A, &
           /T10,'------------------------------------------------------')
 10 FORMAT(/T10,'Coincident Loop data for Station',I4,' of Line ',A, &
           /T10,'--------------------------------------------------')
 11 FORMAT(/T10,'Electric Dipole data for Station',I4,' of Line ',A &
           /T10,'--------------------------------------------------')
 16 FORMAT(/T20,150(:A,5X))
 21 FORMAT(2x, i4, 2x, a1, 3(2x, f12.2), 1024(e15.6, 2x))
 22 FORMAT(2x, i4, 2x, a1, 3(2x, f12.2), 1024(e15.6, 2x))
 23 FORMAT(T3, 'Field Response:', 1024(2x, e15.6))
 24 FORMAT(T3, 'Model Response:', 1024(2x, e15.6))
 28 FORMAT(/, &
           T3, ' Field weights:', 1024(2x, i15))
 25 FORMAT(T3, 'Misfit (%):', 4x, 1024(2x, e15.6))
 26 Format(/)
 27 FORMAT(2x, i4, 2x, a1, 3(2x, f12.2), 1024(i15, 2x))


 CONTAINS

   SUBROUTINE MISFIT_TD (JC)
!  -------------------------

   IMPLICIT NONE
   INTEGER JC,JT
   REAL DENOM,VM,VD

   YTR = 0
   DO JT = 1,NCHNL
     IF (RWTS(JT,JRL,JC,JL) == 0) CYCLE
     VD = RDATA(JT,JRL,JC,JL)
     VM = BFTL(JT,JRL,JC,JL)
     DENOM = SQRT( (VM*VM + VD*VD)/2.0)
     IF (DENOM > 0) YTR(JT) = 100. * (VD - VM) / DENOM
   END DO
   END SUBROUTINE MISFIT_TD

 END SUBROUTINE WRITE_INVDATA_TD

 SUBROUTINE WRITE_RDATA_TD (NW,NCHNL,NLINES,MRXL,MCMP,NRX,SURVEY_TYPE,LINE,IDH, &
                            RX_TYPE,UNITS,SVAZM,IPLT,YXZPLT,TMS,CMP,RDATA)
!-----------------------------------------------------------------------------

!  Writes time domain output to unit NW
!
!*** Called by: MAIN
!***     Calls:
!
!   NW,             : output unit numbers
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
!   SVAZM           : survey azimuth (radians) of Line L
!   IPLT(J,L)       : the response of Rx J of Line L is plotted at 1:Rx;  2:Tx-Rx midpoint;  3:Tx midpoint
!   YXZPLT(1:3,I,L) : GPS east, GPS north, RL  plot coordinate for Ith receiver of Line L
!   TMS             : time midpoints (ms) for NCHNL windows
!   CMP             : component selection
!   RDATA(I,J,K,L)  : the Kth component measured response at frequency I from receiver J,
!                     of Line L, transmitter K (inversion data)

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 REAL, PARAMETER :: R2D= 180./3.14159
 INTEGER NW,NLINES,MRXL,MCMP,NCHNL,SURVEY_TYPE,KDEG,NRX1,JL,JR,LC
 INTEGER, DIMENSION(NLINES) :: NRX,CMP,LINE,UNITS,RX_TYPE,IDH,IPLT
 REAL, DIMENSION(NCHNL,MRXL,MCMP,NLINES) :: RDATA
 REAL SVAZM(NLINES),TMS(NCHNL),YTR(NCHNL,MRXL)
 REAL(KIND=QL) YXZPLT(3,MRXL,NLINES),RXPLT(3,MRXL)
 CHARACTER(LEN=20) UTXT(2),CTXT(2),PLT_PT(3),QL0
 DATA PLT_PT /'Rx','Tx-Rx_MID','Tx'/

 DO JL = 1,NLINES
   LC = CMP(JL)

   CALL GET_UNITS_TEXT (UNITS(JL),UTXT)
   WRITE(QL0,*) LINE(JL)
   READ(QL0,'(A)') CTXT(1)           ! Line number
   CTXT(2) = PLT_PT(IPLT(JL))
   KDEG = INT (R2D * SVAZM(JL))
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

     IF (LC == 3 .OR. LC == 13 .OR. LC == 23 .OR. LC == 123) THEN
       SELECT CASE (IDH(JL))               ! Z, A or W component
       CASE (0)
         WRITE(NW,23) TRIM (ADJUSTL (CTXT(1)))
       CASE (1)       
         WRITE(NW,26) TRIM (ADJUSTL (CTXT(1)))
       CASE (2)       
         WRITE(NW,29) TRIM (ADJUSTL (CTXT(1)))
       END SELECT
       DO JR = 1,NRX1
         YTR(1:NCHNL,JR) = RDATA(1:NCHNL,JR,3,JL)
       END DO
       CALL WRTDP (NW,NCHNL,TMS,MRXL,NRX1,RXPLT,YTR)
     END IF

     IF (LC == 1 .OR. LC == 13 .OR. LC == 12 .OR. LC == 123) THEN
       SELECT CASE (IDH(JL))               ! X, U or S component
       CASE (0)
         WRITE(NW,21) TRIM (ADJUSTL (CTXT(1)))
       CASE (1)       
         WRITE(NW,24) TRIM (ADJUSTL (CTXT(1)))
       CASE (2)       
         WRITE(NW,27) TRIM (ADJUSTL (CTXT(1)))
       END SELECT
       DO JR = 1,NRX1
         YTR(1:NCHNL,JR) = RDATA(1:NCHNL,JR,1,JL)
       END DO
       CALL WRTDP (NW,NCHNL,TMS,MRXL,NRX1,RXPLT,YTR)
     END IF

     IF (LC == 2 .OR. LC == 12 .OR. LC == 23 .OR. LC == 123) THEN
       SELECT CASE (IDH(JL))               ! Y, V or N component
       CASE (0)
         WRITE(NW,22) TRIM (ADJUSTL (CTXT(1)))
       CASE (1)       
         WRITE(NW,25) TRIM (ADJUSTL (CTXT(1)))
       CASE (2)       
         WRITE(NW,28) TRIM (ADJUSTL (CTXT(1)))
       END SELECT
       DO JR = 1,NRX1
         YTR(1:NCHNL,JR) = RDATA(1:NCHNL,JR,2,JL)
       END DO
       CALL WRTDP (NW,NCHNL,TMS,MRXL,NRX1,RXPLT,YTR)
     END IF

   ELSE                                  
     IF (SURVEY_TYPE == 4) WRITE(NW,30) TRIM (ADJUSTL (CTXT(1))) ! Coincident loop
     IF (RX_TYPE(JL) == 2) WRITE(NW,31) TRIM (ADJUSTL (CTXT(1))) ! Electric dipole output
     DO JR = 1,NRX1
       YTR(1:NCHNL,JR) = RDATA(1:NCHNL,JR,1,JL)
     END DO
     CALL WRTDP (NW,NCHNL,TMS,MRXL,NRX1,RXPLT,YTR)
   END IF
 END DO

 10 FORMAT(/T3,'Line ',A,4X,'Survey azimuth =',I4,' degrees',4X,'Units = ',A,4X,'Plot point: ',A)
 11 FORMAT(/T3,'Line ',A,4X,'Magnetic dipole Rx    Survey azimuth =',I4,' degrees',4X,'Units = ',A,4X,'Plot point: ',A)
 12 FORMAT(/T3,'Line ',A,4X,'Electric dipole Rx',4X,'Units = ',A,4X,'Plot point: ',A)
 21 FORMAT(/T10,'X : Radial Component Survey Data for Line ',A &
           /T10,'---------------------------------------------')

 22 FORMAT(/T10,'Y : Tangential Component Survey Data for Line ',A &          
           /T10,'-------------------------------------------------')
 23 FORMAT(/T10,'Z : Vertical Component Survey Data for Line ',A &
           /T10,'-----------------------------------------------')
 24 FORMAT(/T10,'U : Slope Component Survey Data for Line ',A &
           /T10,'------------------------------------------------')
 25 FORMAT(/T10,'V : Horizontal Component Survey Data for Line ',A &
           /T10,'-------------------------------------------------')
 26 FORMAT(/T10,'A : Axial Component Survey Data for Line ',A &
           /T10,'--------------------------------------------')
 27 FORMAT(/T10,'S : In-section Component Survey Data for Line ',A &
           /T10,'-------------------------------------------------')
 28 FORMAT(/T10,'N : Out-section Component Survey Data for Line ',A &
           /T10,'-------------------------------------------------')
 29 FORMAT(/T10,'W : Axial Component Survey Data for Line ',A &
           /T10,'--------------------------------=-----------')
 30 FORMAT(/T10,'Coincident Loop Survey Data for Line ',A &
           /T10,'---------------------------------------')
 31 FORMAT(/T10,'Electric Dipole Loop Survey Data for Line ',A &
           /T10,'-----------------------------------------')

 END SUBROUTINE WRITE_RDATA_TD

 SUBROUTINE WRTDP (NW,NCHNL,TMS,MRXL,NRX1,RXPLT,YTR)
!---------------------------------------------------

!***  Called by: WRITE_TD
!***      CallS: nil

!  Writes time-domain output in profile form.

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 INTEGER NW,NCHNL,MRXL,NRX1,JR
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

 WRITE(NW,1) CHN(1:NCHNL)
 WRITE(NW,2) TMS(1:NCHNL)
 WRITE(NW,'(3X)')
 DO JR = 1, NRX1
   WRITE(NW,3) JR,RXPLT(1:3,JR),YTR(1:NCHNL,JR)
 END DO

 1 FORMAT(/T11,'RECEIVER POSITIONS',6X,1024(:5X,A))
 2 FORMAT(T9,'Easting    Northing     Elev',T37,1024G13.4)
 3 FORMAT(I3,2F12.1,F9.1,1024G13.4)

 END SUBROUTINE WRTDP

 SUBROUTINE WRITE_RDATA_FD (NW,NFRQ,MCHNL,NLINES,MRXL,MCMP,NRX,SURVEY_TYPE,LINE,IDH, &
                            RX_TYPE,UNITS,SVAZM,ISYS,IPLT,YXZPLT,FREQ,CMP,RDATA)
!----------------------------------------------------------------------------------

!  Writes frequency-domain output to unit NW
!
!*** Called by: MAIN
!***     Calls:
!
!   NW               : output unit numbers for .out 
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
!   SVAZM           : survey azimuth (radians) of Line L
!   ISYS            : Sampo output if ISYS = 2
!   IPLT(J,L)       : the response of Rx J of Line L is plotted at 1:Rx;  2:Tx-Rx midpoint;  3:Tx midpoint
!   YXZPLT(1:3,I,L) : GPS east, GPS north, RL  plot coordinate for Ith receiver of Line L
!   FREQ            : frequency array
!   CMP             : component selection
!
!   RDATA(I,J,K,L)  : the Kth component measured response at frequency I from receiver J,
!                     of Line L, transmitter K (inversion data)

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 REAL, PARAMETER :: R2D= 180./3.14159
 INTEGER NW,MCHNL,MCMP,NLINES,MRXL,NFRQ,SURVEY_TYPE,ISYS,KDEG,NRX1,JL,JR,IPPM,LC
 INTEGER, DIMENSION(NLINES) :: NRX,CMP,LINE,UNITS,RX_TYPE,IDH,IPLT
 REAL RDATA(MCHNL,MRXL,MCMP,NLINES)
 REAL SVAZM(NLINES),FREQ(NFRQ),YTR(NFRQ,MRXL)
 REAL(KIND=QL) YXZPLT(3,MRXL,NLINES),RXPLT(3,MRXL)
 CHARACTER(LEN=20) UTXT(2),QL0,CTXT(2),PLT_PT(3)
 DATA PLT_PT /'Rx','Tx-Rx_MID','Tx'/

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

   IF (RX_TYPE(JL) == 1 .AND. ISYS < 2) THEN            ! 3 component magnetic dipole output
     IF (LC == 3 .OR. LC == 13 .OR. LC == 23 .OR. LC == 123) THEN
       SELECT CASE (IDH(JL))               ! Z or A Inphase component
       CASE (0)
         WRITE(NW,23) TRIM (ADJUSTL (CTXT(1)))
       CASE (1)       
         WRITE(NW,26) TRIM (ADJUSTL (CTXT(1)))
       END SELECT
       DO JR = 1,NRX1
         YTR(1:NFRQ,JR) = RDATA(1:NFRQ,JR,3,JL)
       END DO
       CALL WRFDP (NW,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)

       SELECT CASE (IDH(JL))               ! Z or A Quadrature component
       CASE (0)
         WRITE(NW,33) TRIM (ADJUSTL (CTXT(1)))
       CASE (1)       
         WRITE(NW,36) TRIM (ADJUSTL (CTXT(1)))
       END SELECT
       DO JR = 1,NRX1
         YTR(1:NFRQ,JR) = RDATA(NFRQ+1:2*NFRQ,JR,3,JL)
       END DO
       CALL WRFDP (NW,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)
     END IF

     IF (LC == 1 .OR. LC == 13 .OR. LC == 12 .OR. LC == 123) THEN
       SELECT CASE (IDH(JL))               ! X or U Inphase component
       CASE (0)
         WRITE(NW,21) TRIM (ADJUSTL (CTXT(1)))
       CASE (1)       
         WRITE(NW,24) TRIM (ADJUSTL (CTXT(1)))
       END SELECT
       DO JR = 1,NRX1
         YTR(1:NFRQ,JR) = RDATA(1:NFRQ,JR,1,JL)
       END DO
       CALL WRFDP (NW,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)

       SELECT CASE (IDH(JL))               ! X or U Quadrature component
       CASE (0)
         WRITE(NW,31) TRIM (ADJUSTL (CTXT(1)))
       CASE (1)       
         WRITE(NW,34) TRIM (ADJUSTL (CTXT(1)))
       END SELECT
       DO JR = 1,NRX1
         YTR(1:NFRQ,JR) = RDATA(NFRQ+1:2*NFRQ,JR,1,JL)
       END DO
       CALL WRFDP (NW,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)
     END IF

     IF (LC == 2 .OR. LC == 12 .OR. LC == 23 .OR. LC == 123) THEN
       SELECT CASE (IDH(JL))               ! Y or V Inphase component
       CASE (0)
         WRITE(NW,22) TRIM (ADJUSTL (CTXT(1)))
       CASE (1)       
         WRITE(NW,25) TRIM (ADJUSTL (CTXT(1)))
       END SELECT
       DO JR = 1,NRX1
         YTR(1:NFRQ,JR) = RDATA(1:NFRQ,JR,2,JL)
       END DO
       CALL WRFDP (NW,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)

       SELECT CASE (IDH(JL))               ! Y or V Quadrature component
       CASE (0)
         WRITE(NW,32) TRIM (ADJUSTL (CTXT(1)))
       CASE (1)       
         WRITE(NW,35) TRIM (ADJUSTL (CTXT(1)))
       END SELECT
       DO JR = 1,NRX1
         YTR(1:NFRQ,JR) = RDATA(NFRQ+1:2*NFRQ,JR,2,JL)
       END DO
       CALL WRFDP (NW,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)
     END IF

   ELSE IF (RX_TYPE(JL) == 2) THEN                          ! electric dipole output

     WRITE(NW,20) TRIM (ADJUSTL (CTXT(1)))
     DO JR = 1,NRX1
       YTR(1:NFRQ,JR) = RDATA(1:NFRQ,JR,1,JL)
     END DO
     CALL WRFDP (NW,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)
     WRITE(NW,30) TRIM (ADJUSTL (CTXT(1)))
     DO JR = 1,NRX1
       YTR(1:NFRQ,JR) = RDATA(NFRQ+1:2*NFRQ,JR,1,JL)
     END DO
     CALL WRFDP (NW,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)

   ELSE IF (ISYS == 2) THEN                                 ! Sampo output

     WRITE(NW,40) TRIM (ADJUSTL (CTXT(1)))
     DO JR = 1,NRX1
       YTR(1:NFRQ,JR) = RDATA(1:NFRQ,JR,1,JL)
     END DO
     CALL WRFDP (NW,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)

   END IF
 END DO

 10 FORMAT(/T3,'Line ',A,4X,'Survey azimuth =',I4,' degrees',4X,'Units = ',A)
 11 FORMAT(/T3,'Line ',A,4X,'Magnetic dipole Rx    Survey azimuth =',I4,' degrees',4X,'Units = ',A)
 12 FORMAT(/T3,'Line ',A,4X,'Electric dipole Rx',4X,'Units = ',A)
 13 FORMAT(/T3,'Line ',A,4X,'Point electric field',4X,'Units = ',A)
 20 FORMAT(//T3,'Inphase ',A,' for Line ',A &
            /T3,'-------------------------------')
 21 FORMAT(//T10,'X Inphase - Radial Component Survey Data for Line ',A &            
            /T10,'----------------------------------------------------')
 22 FORMAT(//T10,'Y Inphase - Tangential Component Survey Data for Line ',A &
            /T10,'--------------------------------------------------------')
 23 FORMAT(//T10,'Z Inphase - Vertical Component Survey Data for Line ',A &
            /T10,'------------------------------------------------------')
 24 FORMAT(//T10,'U Inphase - Slope Component Survey Data for Line ',A &
            /T10,'---------------------------------------------------')
 25 FORMAT(//T10,'V Inphase - Horizontal Component Survey Data for Line ',A &
            /T10,'--------------------------------------------------------')
 26 FORMAT(//T10,'A Inphase - Axial Component Survey Data for Line ',A &
            /T10,'-------------------------------------------------------')
 30 FORMAT(//T3,'Quadrature Survey Data for Line ',A &
            /T3,'----------------------------------')
 31 FORMAT(/T10,'X Quadrature - Radial Component Survey Data for Line ',A &
           /T10,'-------------------------------------------------------')
 32 FORMAT(//T10,'Y Quadrature - Tangential Component Survey Data for Line ',A &
           /T10,'------------------------------------------------------------')
 33 FORMAT(//T10,'Z Quadrature - Vertical Component Survey Data for Line ',A &
            /T10,'---------------------------------------------------------')
 34 FORMAT(//T10,'U Quadrature - Slope Component Survey Data for Line ',A &
            /T10,'------------------------------------------------------')
 35 FORMAT(//T10,'V Quadrature - Horizontal Component Survey Data for Line ',A &
            /T10,'-----------------------------------------------------------')
 36 FORMAT(//T10,'A Quadrature - Axial Component Survey Data for Line ',A &
            /T10,'------------------------------------------------------')
 40 FORMAT(//T3,'Abs (Bz / Bx Ratio  Survey Data for Line ',A &
            /T3,'-------------------------------------------')

 END SUBROUTINE WRITE_RDATA_FD

 SUBROUTINE WRFDP (NW,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)
!-------------------------------------------------------

!***  Called by WRSLV

!  Writes frequency-domain output in profile form.  IPPM = format control

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 INTEGER NW,IPPM,NFRQ,MRXL,NRX1,JR
 REAL FREQ(NFRQ),YTR(NFRQ,NRX1)
 REAL(KIND=QL) RXPLT(3,MRXL)

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

  1 FORMAT(/T14,'RECEIVER POSITIONS',T47,'FREQUENCIES')
  2 FORMAT(T10,'East        North     Elev',F10.2,100F13.2)
  3 FORMAT(I3,2F12.1,F9.1,300G13.4)
  8 FORMAT(I3,2F12.1,F9.1,300F12.1)
  9 FORMAT(T10,'East        North     Elev ',300F12.1)

 END SUBROUTINE WRFDP

 SUBROUTINE WRITE_INVDATA_FD (JL,JRL,NW,NP,NFRQ,MCHNL,NLINES,MRXL,MCMP,LINE,RX_TYPE, &
                              IDH,UNITS,ISYS,YXZPLT,FREQ,CMP,BFTL,RDATA,RWTS)
!-----------------------------------------------------------------------------------

!  Writes frequency-domain output to unit NW
!
!*** Called by: MAIN
!***     Calls:
!
!   NW, NP         : output unit number for Beowulf.out
!   NFRQ            : number of frequencies
!   MCHNL           = NFRQ for Sampo;  = 2*NFRQ otherwise
!   NLINES          : number of lines
!   MRXL            : maximum number of receivers per line
!   MCMP            = 1 for Sampo;  = 3 otherwise
!   LINE(L)         : line number of Line L
!   UNITS(L)        : unit designater of Line L
!   IDH(L)          : surface (0);  local U,V,A (1)
!   RXID(L)         : receiver type Line L - mag dipole (1); electric dipole (2)
!   ISYS            : Sampo output if ISYS = 2
!   YXZPLT(1:3,I,L) : GPS east, GPS north, RL  plot coordinate for Ith receiver of Line L
!   FREQ            : frequency array
!   CMP             : component selection
!
!   BFTL(I,J,K,L)   : the Kth component measured response at frequency I from receiver J,
!                     of Line L, transmitter K (modelled)
!   RDATA(I,J,K,L)  : the Kth component measured response at frequency I from receiver J,
!                     of Line L, transmitter K (inversion data)
!   RWTS(I,J,K,L)   : inversion weights corresponding to RDATA

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 INTEGER NW,NP,MCHNL,MCMP,NLINES,MRXL,NFRQ,ISYS,JL,JRL,IPPM,LC,KF1,KF2
 INTEGER, DIMENSION(NLINES) :: CMP,LINE,UNITS,RX_TYPE,IDH
 INTEGER RWTS(MCHNL,MRXL,MCMP,NLINES)
 REAL, DIMENSION(MCHNL,MRXL,MCMP,NLINES) :: BFTL,RDATA
 REAL FREQ(NFRQ),YTR(NFRQ,3),PERR(MCHNL)
 REAL(KIND=QL) YXZPLT(3,MRXL,NLINES)
 CHARACTER(LEN=20) QL0,CLINE

 LC = CMP(JL)
 IPPM = 0
 IF (UNITS(JL) > 30 .AND. UNITS(JL) < 39) IPPM = 1  ! Format control
 IF (ISYS == 2) IPPM = 0
 WRITE(QL0,*) LINE(JL)
 READ(QL0,'(A)') CLINE

 IF (IPPM == 1) THEN
   SELECT CASE (CMP(JL))
   CASE(1)
     WRITE(NP,1) YXZPLT(1:3,JRL,JL), RDATA(1:MCHNL,JRL,1,JL)
     WRITE(NP,2) YXZPLT(1:3,JRL,JL),  BFTL(1:MCHNL,JRL,1,JL)
   CASE(2)        
     WRITE(NP,1) YXZPLT(1:3,JRL,JL), RDATA(1:MCHNL,JRL,2,JL)
     WRITE(NP,2) YXZPLT(1:3,JRL,JL),  BFTL(1:MCHNL,JRL,2,JL)
   CASE(3)        
     WRITE(NP,1) YXZPLT(1:3,JRL,JL), RDATA(1:MCHNL,JRL,3,JL)
     WRITE(NP,2) YXZPLT(1:3,JRL,JL),  BFTL(1:MCHNL,JRL,3,JL)
   CASE(12)       
     WRITE(NP,1) YXZPLT(1:3,JRL,JL), RDATA(1:MCHNL,JRL,1,JL), RDATA(1:MCHNL,JRL,2,JL)
     WRITE(NP,2) YXZPLT(1:3,JRL,JL),  BFTL(1:MCHNL,JRL,1,JL),  BFTL(1:MCHNL,JRL,2,JL)
   CASE(13)       
     WRITE(NP,1) YXZPLT(1:3,JRL,JL), RDATA(1:MCHNL,JRL,1,JL), RDATA(1:MCHNL,JRL,3,JL)
     WRITE(NP,2) YXZPLT(1:3,JRL,JL),  BFTL(1:MCHNL,JRL,1,JL),  BFTL(1:MCHNL,JRL,3,JL)
   CASE(23)       
     WRITE(NP,1) YXZPLT(1:3,JRL,JL), RDATA(1:MCHNL,JRL,2,JL), RDATA(1:MCHNL,JRL,3,JL)
     WRITE(NP,2) YXZPLT(1:3,JRL,JL),  BFTL(1:MCHNL,JRL,2,JL),  BFTL(1:MCHNL,JRL,3,JL)
   CASE(123)      
     WRITE(NP,1) YXZPLT(1:3,JRL,JL), RDATA(1:MCHNL,JRL,1,JL), RDATA(1:MCHNL,JRL,2,JL), RDATA(1:MCHNL,JRL,3,JL)
     WRITE(NP,2) YXZPLT(1:3,JRL,JL),  BFTL(1:MCHNL,JRL,1,JL),  BFTL(1:MCHNL,JRL,2,JL),  BFTL(1:MCHNL,JRL,3,JL)
   END SELECT
 ELSE
   SELECT CASE (CMP(JL))
   CASE(1)
     WRITE(NP,11) YXZPLT(1:3,JRL,JL), RDATA(1:MCHNL,JRL,1,JL)
     WRITE(NP,12) YXZPLT(1:3,JRL,JL),  BFTL(1:MCHNL,JRL,1,JL)
   CASE(2)        
     WRITE(NP,11) YXZPLT(1:3,JRL,JL), RDATA(1:MCHNL,JRL,2,JL)
     WRITE(NP,12) YXZPLT(1:3,JRL,JL),  BFTL(1:MCHNL,JRL,2,JL)
   CASE(3)        
     WRITE(NP,11) YXZPLT(1:3,JRL,JL), RDATA(1:MCHNL,JRL,3,JL)
     WRITE(NP,12) YXZPLT(1:3,JRL,JL),  BFTL(1:MCHNL,JRL,3,JL)
   CASE(12)       
     WRITE(NP,11) YXZPLT(1:3,JRL,JL), RDATA(1:MCHNL,JRL,1,JL), RDATA(1:MCHNL,JRL,2,JL)
     WRITE(NP,12) YXZPLT(1:3,JRL,JL),  BFTL(1:MCHNL,JRL,1,JL),  BFTL(1:MCHNL,JRL,2,JL)
   CASE(13)       
     WRITE(NP,11) YXZPLT(1:3,JRL,JL), RDATA(1:MCHNL,JRL,1,JL), RDATA(1:MCHNL,JRL,3,JL)
     WRITE(NP,12) YXZPLT(1:3,JRL,JL),  BFTL(1:MCHNL,JRL,1,JL),  BFTL(1:MCHNL,JRL,3,JL)
   CASE(23)       
     WRITE(NP,11) YXZPLT(1:3,JRL,JL), RDATA(1:MCHNL,JRL,2,JL), RDATA(1:MCHNL,JRL,3,JL)
     WRITE(NP,12) YXZPLT(1:3,JRL,JL),  BFTL(1:MCHNL,JRL,2,JL),  BFTL(1:MCHNL,JRL,3,JL)
   CASE(123)      
     WRITE(NP,11) YXZPLT(1:3,JRL,JL), RDATA(1:MCHNL,JRL,1,JL), RDATA(1:MCHNL,JRL,2,JL), RDATA(1:MCHNL,JRL,3,JL)
     WRITE(NP,12) YXZPLT(1:3,JRL,JL),  BFTL(1:MCHNL,JRL,1,JL),  BFTL(1:MCHNL,JRL,2,JL),  BFTL(1:MCHNL,JRL,3,JL)
   END SELECT
 END IF

 KF1 = 1;  KF2 = NFRQ
 IF (ISYS /= 2) THEN
   KF1 = NFRQ+1;  KF2 = 2*NFRQ
 END IF

 IF (ISYS == 2) THEN                                 ! Sampo output
   CALL MISFIT_FD(1)
   WRITE(NW,40) JRL,TRIM (ADJUSTL (CLINE))
   WRITE(NW,6) FREQ(1:NFRQ)
   YTR(1:MCHNL,1) = RDATA(1:MCHNL,JRL,1,JL)
   YTR(1:MCHNL,2) =  BFTL(1:MCHNL,JRL,1,JL)
   YTR(1:MCHNL,3) =  PERR(1:MCHNL)
   CALL WRFDP 

 ELSE IF (RX_TYPE(JL) == 2) THEN                          ! electric dipole output
   CALL MISFIT_FD(1)
   WRITE(NW,20) JRL,TRIM (ADJUSTL (CLINE))
   WRITE(NW,6) FREQ(1:NFRQ)
   YTR(1:NFRQ,1) = RDATA(1:NFRQ,JRL,1,JL)
   YTR(1:NFRQ,2) =  BFTL(1:NFRQ,JRL,1,JL)
   YTR(1:NFRQ,3) =  PERR(1:NFRQ)
   CALL WRFDP 

   WRITE(NW,30) JRL,TRIM (ADJUSTL (CLINE))
   WRITE(NW,6) FREQ(1:NFRQ)
   YTR(1:NFRQ,1) = RDATA(KF1:KF2,JRL,1,JL)
   YTR(1:NFRQ,2) =  BFTL(KF1:KF2,JRL,1,JL)
   YTR(1:NFRQ,3) =  PERR(KF1:KF2)
   CALL WRFDP 

 ELSE IF (RX_TYPE(JL) == 1) THEN                 ! 3 component magnetic dipole output
   IF (LC == 3 .OR. LC == 13 .OR. LC == 23 .OR. LC == 123) THEN
     CALL MISFIT_FD(3)
     SELECT CASE (IDH(JL))               ! Z or A Inphase component
     CASE (0)
       WRITE(NW,23) JRL,TRIM (ADJUSTL (CLINE))
     CASE (1)
       WRITE(NW,26) JRL,TRIM (ADJUSTL (CLINE))
     END SELECT
     WRITE(NW,6) FREQ(1:NFRQ)
     YTR(1:NFRQ,1) = RDATA(1:NFRQ,JRL,3,JL)
     YTR(1:NFRQ,2) =  BFTL(1:NFRQ,JRL,3,JL)
     YTR(1:NFRQ,3) =  PERR(1:NFRQ)
     CALL WRFDP 

     SELECT CASE (IDH(JL))               ! Z or A Quadrature component
     CASE (0)
       WRITE(NW,33) JRL,TRIM (ADJUSTL (CLINE))
     CASE (1)
       WRITE(NW,36) JRL,TRIM (ADJUSTL (CLINE))
     END SELECT
     WRITE(NW,6) FREQ(1:NFRQ)
     YTR(1:NFRQ,1) = RDATA(KF1:KF2,JRL,3,JL)
     YTR(1:NFRQ,2) =  BFTL(KF1:KF2,JRL,3,JL)
     YTR(1:NFRQ,3) =  PERR(KF1:KF2)
   CALL WRFDP 
   END IF

   IF (LC == 1 .OR. LC == 13 .OR. LC == 12 .OR. LC == 123) THEN
     CALL MISFIT_FD(1)
     SELECT CASE (IDH(JL))               ! X or U Inphase component
     CASE (0)
       WRITE(NW,21) JRL,TRIM (ADJUSTL (CLINE))
     CASE (1)
       WRITE(NW,24) JRL,TRIM (ADJUSTL (CLINE))
     END SELECT
     WRITE(NW,6) FREQ(1:NFRQ)
     YTR(1:NFRQ,1) = RDATA(1:NFRQ,JRL,1,JL)
     YTR(1:NFRQ,2) =  BFTL(1:NFRQ,JRL,1,JL)
     YTR(1:NFRQ,3) =  PERR(1:NFRQ)
     CALL WRFDP 

     SELECT CASE (IDH(JL))               ! X, or U Quadrature component
     CASE (0)
       WRITE(NW,31) JRL,TRIM (ADJUSTL (CLINE))
     CASE (1)
       WRITE(NW,34) JRL,TRIM (ADJUSTL (CLINE))
     END SELECT
     WRITE(NW,6) FREQ(1:NFRQ)
     YTR(1:NFRQ,1) = RDATA(KF1:KF2,JRL,1,JL)
     YTR(1:NFRQ,2) =  BFTL(KF1:KF2,JRL,1,JL)
     YTR(1:NFRQ,3) =  PERR(KF1:KF2)
     CALL WRFDP 

   END IF

   IF (LC == 2 .OR. LC == 12 .OR. LC == 23 .OR. LC == 123) THEN
     CALL MISFIT_FD(2)
     SELECT CASE (IDH(JL))               ! Y or V Inphase component
     CASE (0)
       WRITE(NW,22) JRL,TRIM (ADJUSTL (CLINE))
     CASE (1)
       WRITE(NW,25) JRL,TRIM (ADJUSTL (CLINE))
     END SELECT
     WRITE(NW,6) FREQ(1:NFRQ)
     YTR(1:NFRQ,1) = RDATA(1:NFRQ,JRL,2,JL)
     YTR(1:NFRQ,2) =  BFTL(1:NFRQ,JRL,2,JL)
     YTR(1:NFRQ,3) =  PERR(1:NFRQ)
     CALL WRFDP 

     SELECT CASE (IDH(JL))               ! Y, V Quadrature component
     CASE (0)
       WRITE(NW,32) JRL,TRIM (ADJUSTL (CLINE))
     CASE (1)
       WRITE(NW,35) JRL,TRIM (ADJUSTL (CLINE))
     END SELECT
     WRITE(NW,6) FREQ(1:NFRQ)
     YTR(1:NFRQ,1) = RDATA(KF1:KF2,JRL,2,JL)
     YTR(1:NFRQ,2) =  BFTL(KF1:KF2,JRL,2,JL)
     YTR(1:NFRQ,3) =  PERR(KF1:KF2)
     CALL WRFDP 
   END IF

 END IF

  1 FORMAT(/T3,'SURVEY_DATA',T15,2F12.1,F9.1,500F13.4)
  2 FORMAT(T3, 'MODEL_DATA', T15,2F12.1,F9.1,500F13.4)
  6 FORMAT(T3,'Frequencies',F9.1,300F13.1)
 11 FORMAT(/T3,'SURVEY_DATA',T15,2F12.1,F9.1,500G13.4)
 12 FORMAT(T3, 'MODEL_DATA', T15,2F12.1,F9.1,500G13.4)
 20 FORMAT(//T3,'Inphase data for Station',I4,' of Line ',A &
            /T3,'------------------------------------------')
 21 FORMAT(//T10,'X Inphase (Radial) Component data for Station',I4,' of Line ',A &
            /T10,'---------------------------------------------------------------')
 22 FORMAT(//T10,'Y Inphase (Tangential) Component data for Station',I4,' of Line ',A &
            /T10,'-------------------------------------------------------------------')
 23 FORMAT(//T10,'Z Inphase (Vertical) Component data for Station',I4,' of Line ',A &
            /T10,'-----------------------------------------------------------------')
 24 FORMAT(//T10,'U Inphase (Slope) Component data for Station',I4,' of Line ',A &
            /T10,'--------------------------------------------------------------')
 25 FORMAT(//T10,'V Inphase (Horizontal) Component data for Station',I4,' of Line ',A &
            /T10,'-------------------------------------------------------------------')
 26 FORMAT(//T10,'A Inphase (Axial) Component data for Station',I4,' of Line ',A &
            /T10,'--------------------------------------------------------------')
 30 FORMAT(//T3,'Quadrature data for Station',I4,' of Line ',A &
            /T3,'---------------------------------------------')
 31 FORMAT(//T10,'X Quadrature (Radial) Component data for Station',I4,' of Line ',A &
            /T10,'-------------------------------------------------------')
 32 FORMAT(//T10,'Y Quadrature (Tangential) Component data for Station',I4,' of Line ',A &
            /T10,'------------------------------------------------------------')
 33 FORMAT(//T10,'Z Quadrature (Vertical) Component data for Station',I4,' of Line ',A &
            /T10,'---------------------------------------------------------')
 34 FORMAT(//T10,'U Quadrature (Slope) Component data for Station',I4,' of Line ',A &
            /T10,'------------------------------------------------------')
 35 FORMAT(//T10,'V Quadrature (Horizontal) Component data for Station',I4,' of Line ',A &
            /T10,'-----------------------------------------------------------')
 36 FORMAT(//T10,'A Quadrature (Axial) Component data for Station',I4,' of Line ',A &
            /T10,'------------------------------------------------------')
 40 FORMAT(//T3,'Abs (Bz / Bx Ratio data for Station',I4,' of Line ',A &
            /T3,'-------------------------------------------')

  CONTAINS

   SUBROUTINE MISFIT_FD (JC)
!  -------------------------

   IMPLICIT NONE
   INTEGER JC,JF
   REAL DENOM,VM,VD

   PERR = 0
   DO JF = 1,MCHNL
     IF (RWTS(JF,JRL,JC,JL) == 0) CYCLE
     VD = RDATA(JF,JRL,JC,JL)
     VM = BFTL(JF,JRL,JC,JL)
     DENOM = SQRT( (VM*VM + VD*VD)/2.0)
     IF (DENOM > 0) PERR(JF) = 100. * (VD - VM) / DENOM
   END DO
   END SUBROUTINE MISFIT_FD


   SUBROUTINE WRFDP
!  ----------------

!  Writes frequency-domain output in profile form.  IPPM = format control

   IF (IPPM == 1) THEN
     WRITE(NW,1) YTR(1:NFRQ,1)
     WRITE(NW,2) YTR(1:NFRQ,2)
   ELSE
     WRITE(NW,3) YTR(1:NFRQ,1)
     WRITE(NW,4) YTR(1:NFRQ,2)
   END IF
   WRITE(NW,5) YTR(1:NFRQ,3)

  1 FORMAT(/T3,'Survey data:',T15,500F13.1)
  2 FORMAT(T3, 'Model data:', T15,500F13.1)
  3 FORMAT(/T3,'Survey data:',T15,500G13.4)
  4 FORMAT(T3, 'Model data:', T15,500G13.4)
  5 FORMAT(T3,'Misfit (%):',F9.1,500F13.1)

   END SUBROUTINE WRFDP

 END SUBROUTINE WRITE_INVDATA_FD

  SUBROUTINE WRITE_LOG_FILE (NLG,MSG,MXERR,ERR_LVL)
! -------------------------------------------------

! This subroutine prints out warning and fatal error messages on the LOG file.
!
! NLG = output unit index
! MSG refers to error message index
! ERR_LVL = 1 for warnings;  = 2 for fatal errors
! MXERR = MAX (ERR_LVL)

 INTEGER ERR_LVL, MSG, NLG, MXERR

 IF (MXERR == 0) OPEN (NLG,FILE = 'Beowulf.log',STATUS = 'REPLACE')

 MXERR = MAX (ERR_LVL,MXERR)
 IF (ERR_LVL == 1) WRITE(NLG,501)
 IF (ERR_LVL == 2) WRITE(NLG,502)

 IF (MSG == 1) WRITE(NLG,1)
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
 IF (MSG == 63) WRITE(NLG,63)
 IF (MSG == 100) WRITE(NLG,100)
 IF (MSG == 90) THEN
   WRITE(NLG,90)
   STOP
 END IF
  1 FORMAT(/T3,'The value for TDFD is outside the permitted range.' &
           /T3,'The allowed values are: 1 or 0 for time-domain or 2 for frequency domain.')
  4 FORMAT(/T3,'The value for STEP is outside the permitted range.' &
           /T3,'The allowed values are: 0 or 1.')
  5 FORMAT(/T3,'The value for KRXW is outside the permitted range.' &
           /T3,'The allowed values are: 1 or 2.')
  6 FORMAT(/T3,'Only magnetic dipole receivers areallowed with magnnetic dipole transmitters')
  7 FORMAT(/T3,'This value for TOPN is outside the permitted range.' &
           /T3,'It must be > 0.')
  8 FORMAT(/T3,'Entry for SOURCE_TYPE is restricted to the values: 1 (loop), 2 (grounded wire) or 3 (magnetic dipole).')
  9 FORMAT(/T3,'RX_TYPE must be 1 (magnetic dipole) or 2 (electric dipole)')
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
 17 FORMAT(/T3,'For frequency-domain, ISYS must = 0 or 2 for Sampo')
 18 FORMAT(/T3,'For UTEM, the number of channels must be 10 or 20.')
 19 FORMAT(/T3,'The magnetic receiver index exceeds the number of magnetic receivers')
 20 FORMAT(/T3,'The magnetotellurics option is not allowed for time-domain applications.')
 21 FORMAT(/T3,'This lithology index is invalid.' &
           /T3,'No resistivity or conductance has been specified.')
 22 FORMAT(/T3,'Layer lithology indices must be an integer between 1 & NLITH')
 23 FORMAT(/T3,'Layer resistivities must be positive.')
 24 FORMAT(/T3,'The value given to CMP is not allowed.' &
           /T3,'CMP values must be 1, 2, 3, 12, 13, 23, or 123')
 30 FORMAT(/T3,'The point E-field receiver is not allowed for time-domain modelling')
 31 FORMAT(/T3,'The number of magnetic receivers must not exceed the number of electric receivers.')
 32 FORMAT(/T3,'The number of receivers NRX on at least one line exceeds MRXL.')
 35 FORMAT(/T3,'Problem with HEADER_ID.  Seek help !')
 40 FORMAT(/T3,'The Sampo option is not defined for time-domain applications.')
 50 FORMAT(/T3,'The value for LINE_CHK does not match LINE(J)')
 51 FORMAT(/T3,'The value for NSTATL does not match NRX(J)')
 52 FORMAT(/T3,'The value given to KMP is not allowed.')
 54 FORMAT(/T3,'The component(s) specified for inversion in CMP' &
           /T3,'are not present in the data as specified by KMP')
 55 FORMAT(/T3,'Conflict between NKMP(J) and KMP(J).' &
           /T3,'There must be at least NKMP data components to invert.')
 56 FORMAT(/T3,'Conflict between NKMP(j) and CMP(J).')
 58 FORMAT(/T3,'FD_ORDER must = 0, 1, or 2')
 60 FORMAT(/T3,'LINE_CHK must = Line number(J)')
 61 FORMAT(/T3,'NSTATL must = NRX(J)')
 62 FORMAT(/T3,'The data for each receiver must be presented in the same order as specified' &
           /T3,'in the receiver specification in Beowulf.inv')
 63 FORMAT(/T3,'CNVRG must = 1, 2, 10 or 20')
 90 FORMAT(//T3,'MESSAGE FROM SCAT_MTRX_LU_DCMP: THE MATRIX IS SINGULAR.', &
            /T3,'COMPUTATION HALTED.  SEEK HELP.')
 100 FORMAT(1X)
 501 FORMAT(/T2,'INFORMATION / WARNING'/T2,'---------------------'/)
 502 FORMAT(/T2,'FATAL ERROR'/T2,'----- -----')

 END SUBROUTINE  WRITE_LOG_FILE

!==========================================================================================
!*****************************************
!
!       LAYERED HALFSPACE ROUTINES
!       --------------------------
!*****************************************


   SUBROUTINE HSBOSS_TD (NFRQ,FREQ,STEP,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL,  &
                         TOPN,TCLS,SOURCE_TYPE,NTX,MXVRTX,NVRTX,SXN,SXE,SXZ,SXDIP,SXAZM, &
                         NRXTX,RXID,MRXTX,MQVR,XRXTX,YRXTX,ZRXTX,MXRHO,RHOTRP,NLYR,THKD, &
                         RES,RMUD,REPS,CHRG,CTAU,CFREQ,NCTD,JS,JR,BTD)
!----------------------------------------------------------------------------------------

!  Computes BTD, the time-domain layered earth response convolved with the
!  excitation waveform and the receiver channels per unit receiver area.
!  For impulse response, it computes dB/dt in T / s which is the same as
!  volts per unit area.

!  For step response, it computes B in Teslas.
!
!  This version computes individually per call, the response for receiver JR of transmitter JS.

!***  Called by MAIN

!***  Calls HSBOSS

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!  SIGN CONVENTION:
!  ----------------
!  The normal layered earth field coordinate system used in this
!  subroutine has X (JC=1) positive north, Y (JC=2) positive east
!  and Z (JC=3) positive down.
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
!                             Input
!                             -----
!         STEP = 1 for normal step response is to be computed
!              = 4 for UTEM pure step response
!         STEP = 1 iff step response is to be computed
!          NSX - number of points used to discretise transmitter signal
!          SWX - abscissae (seconds) of current waveform
!          SWY - dI/dt * Tx moment & nanotesla conversion at times SWX
!        NPULS - number of bipolar pulses of length PULSE
!        PULSE - length for single on pulse plus off-time
!       NTYPLS - number of TRP values in 1 PULSE
!        NTYRP - number of values in TRP for total signal length: 2 * NPULS *PULSE
!          TRP - array of time values for FD -> TD transformations
!        NCHNL - number of channels
!         TOPN - time at which receiver channel I opens.
!         TCLS - time at which receiver channel I closes.
!  SOURCE_TYPE = 1 : general loop
!              = 2 : grounded wire
!              = 3 : magnetic dipole
!              = 4 : coincident loop
!          NTX - number of transmitter positions
!       MXVRTX - maximum number of vertices for any transmitter
!     NVRTX(J) - number of vertices for transmitter J
!     SXE(I,J) = local east coordinate of vertex I for loop position J
!     SXN(I,J) = local coordinate of vertex I for loop position J
!     SXZ(J)   = z positive down depth of mag dipole Tx J
!     SXDIP(J) = dip (in radians) of dipole J (eg; vertical = 0, horizontal = 90)
!      SXAZM(J) = azimuth (in radians) of dipole J (north = 0, east = 90)
!        MRXTX - maximum number of receivers per transmitter
!     NRXTX(I) - number of receivers for transmitter I
!    RXID(I,J) - RX_TYPE of receiver I for transmitter J
!         MQVR - maximum number of vertices for all receivers (= 1 if all sources are magnetic dipoles)
! XRXTX(J,I,K) - north coordinate of the Kth vertex of the Jth receiver of transmitter I
! YRXTX(J,I,K) - east coordinate of the Kth vertex of the Jth receiver of transmitter I
!   ZRXTX(J,I) - depth of the Jth receiver of transmitter I
!         NLYR - number of layers
!          RES - layer resistivities
!         RMUD - mu(i) / mu(0)
!         REPS - array of relative dislectric constants
!         THKD - layer thicknesses
!         CHRG - chargeability
!         CTAU - array of layer relaxation times (sec).
!        CFREQ - array of layer frequency parameters.
!    NCTD(I,J) = number of components for receiver I for transmitter J
!
!                             Output
!                             ------
!  BTD(JT,JR,JS,I) - the Ith component of the layered earth response at
!                    time JT, receiver JR, station JS.
!           I = 1,2,3 = north, east and vertical componsnts respectively

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 REAL, PARAMETER :: TWOPI=6.283185307
 INTEGER NFRQ,STEP,STEPC,NSX,NPULS,NTYPLS,NTYRP,NCHNL,SOURCE_TYPE,NTX,MXVRTX,MXRHO,MRXTX, &
         NVRTX(NTX),NRXTX(NTX),NCTD(MRXTX,NTX),JC,JT,JF,JR,JS,MQVR,NLYR,RXID(MRXTX,NTX)
 REAL FREQ(NFRQ),WF(NFRQ),YFRQ(4,NFRQ),YPRM(4,NTYRP),YCUM(NCHNL),SWX(NSX),PULSE, &
      SWY(NSX,3),TRP(NTYRP),T,RHOTRP(MXRHO),ZRXTX(MRXTX,NTX),BTD(NCHNL,MRXTX,NTX,3),COSTRN
 REAL, DIMENSION (NLYR) :: RES,REPS,CTAU,CFREQ,CHRG
 REAL, DIMENSION (NCHNL) :: TOPN,TCLS
 REAL, DIMENSION (NTX) :: SXDIP,SXAZM,SXZ
 REAL, DIMENSION (MXVRTX,NTX) :: SXN,SXE
 REAL, DIMENSION(MRXTX,NTX,MQVR) :: XRXTX,YRXTX
 REAL(KIND=QL) THKD(NLYR),RMUD(0:NLYR)
 COMPLEX BFD(NFRQ,MRXTX,NTX,3)

 BTD = 0.
 WF(1:NFRQ) = ALOG (TWOPI * FREQ(1:NFRQ))

 CALL HSBOSS (NFRQ,FREQ,SOURCE_TYPE,NTX,MXVRTX,NVRTX,SXN,SXE,SXZ,SXDIP,SXAZM, &
              NRXTX,MRXTX,RXID,MQVR,XRXTX,YRXTX,ZRXTX,MXRHO,RHOTRP,NLYR,THKD, &
              RES,RMUD,REPS,CHRG,CTAU,CFREQ,JS,JR,BFD)
 STEPC = STEP
 IF (RXID(JR,JS) == 2) THEN
   STEPC = 3                      ! Electrode E field Transform INTEGRAL { E dl } to TD
 END IF
 DO JC = 1,NCTD(JR,JS)
   DO JF = 1,NFRQ     ! Divide by -iw to set up step response
     IF (STEPC == 3) THEN
       YFRQ(1,JF) = REAL ( BFD(JF,JR,JS,JC) )
     ELSE
       YFRQ(1,JF) = -AIMAG ( BFD(JF,JR,JS,JC) ) / (TWOPI * FREQ(JF))
     END IF
   END DO
   CALL CUBSPL (WF,YFRQ,NFRQ)

   YPRM = 0.
   DO JT = 1, NTYRP   !  Convert to step-function time-domain.
     T = TRP(JT)
     YPRM(1,JT) = COSTRN (WF,YFRQ,NFRQ,T)
   END DO

   CALL FOLD_AND_CONVOLVE (STEPC,NSX,SWX,SWY,NPULS,PULSE,TRP,NTYRP,NTYPLS, &
                           NCHNL,TOPN,TCLS,YPRM,YCUM)

   BTD(1:NCHNL,JR,JS,JC) = YCUM(1:NCHNL)
 END DO
 END SUBROUTINE HSBOSS_TD

 SUBROUTINE HSBOSS (NFRQ,FREQ,SOURCE_TYPE,NTX,MXVRTX,NVRTX,SXN,SXE,SXZ,SXDIP,SXAZM, &
                    NRXTX,MRXTX,RXID,MQVR,XRXTX,YRXTX,ZRXTX,MXRHO,RHOTRP,NLYR,THKD,  &
                    RES,RMUD,REPS,CHRG,CTAU,CFREQ,JS,JR,BFD)
!---------------------------------------------------------------------------------------

!***  Called by MAIN
!***  Calls SXMDB

!  Computes BFD, the frequency-domain layered earth response.
!  Magnetic fields are in Teslas per unit amp.  Electrode response is in volts.
!
! For magnetic dipole receivers, BFD(JF,JR,JS,1:3) contains the
! 1: north, 2: east & 3:vertical components for frequency JF, transmitter JS, receiver JR.
!
! The response for electric dipoles and loop receivers is contained in BFD(JF,JR,JS,1),
! BFD(JF,JR,JS,2:3) is set to zero.
!
! This version computes individually per call, the response for receiver JR of transmitter JS.
!
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!  SIGN CONVENTION:
!  ----------------
!  The normal layered earth field coordinate system used in this
!  subroutine has X (JC=1) positive north, Y (JC=2) positive east
!  and Z (JC=3) positive down.
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
!                             Input
!                             -----
!         NFRQ - number of frequencies
!         FREQ - array of frequencies
!  SOURCE_TYPE = 1 : general loop
!              = 2 : grounded wire
!              = 3 : magnetic dipole
!              = 4 : coincident loop
!              = 6 : magnetotellurics
!          NTX - number of transmitter positions
!       MXVRTX - maximum number of vertices for any transmitter
!     NVRTX(J) - number of vertices for transmitter J
!     SXE(I,J) = local east coordinate of vertex I for loop position J
!     SXN(I,J) = local coordinate of vertex I for loop position J
!     SXZ(J)   = depth of Tx J
!     SXDIP(J) = dip (in radians) of dipole J (eg; vertical = 0, horizontal = 90)
!     SXAZM(J) = azimuth (in radians) of dipole J (north = 0, east = 90)
!     NRXTX(I) - number of receivers for transmitter I
!        MRXTX - maximum number of receivers per transmitter
!    RXID(I,J) - RX_TYPE of receiver I for transmitter J. 1 => MD; 2 => ED
!         MQVR - maximum number of vertices for all receivers (= 1 if all sources are magnetic dipoles)
! XRXTX(J,I,K) - north coordinate of the Kth vertex of the Jth receiver of transmitter I
! YRXTX(J,I,K) - east coordinate of the Kth vertex of the Jth receiver of transmitter I
!   ZRXTX(J,I) - depth of the Jth receiver of transmitter I
!              - K = 1 only for mag dipole Rx; 1 to 2 for electric dipole Rx; 1 to 4 for loop Rx
!         NLYR - number of layers
!          RES - layer resistivities
!         RMUD - mu(i) / mu(0)
!         REPS - array of relative dislectric constants
!         THKD - layer thicknesses
!         CHRG - chargeability array
!         CTAU - array of layer relaxation times (sec).
!        CFREQ - array of layer frequency parameters.
!
!                             Output
!                             ------
!  BFD(JF,JR,JS,I) - the Ith component of the layered earth response at
!                    frequency JF, receiver JR, station JS.
!           I = 1,2,3 = north, east and vertical componsnts respectively

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 INTEGER NFRQ,SOURCE_TYPE,NTX,MXVRTX,RXLYR,SXLYR,NVRTX(NTX),MRXTX,MXRHO,JS,JR, &
         JZ,NRXTX(NTX),RXID(MRXTX,NTX),MQVR,NLYR,NVRL
 REAL FREQ(NFRQ),SXN1,SXE1,SXDP1,SXAZM1,XRX,YRX,RHOTRP(MXRHO),ZRXTX(MRXTX,NTX)
 REAL, DIMENSION (NLYR) :: RES,REPS,CTAU,CFREQ,CHRG
 REAL, DIMENSION (NTX) :: SXDIP,SXAZM,SXZ
 REAL, DIMENSION (MXVRTX,NTX) :: SXN,SXE
 REAL, DIMENSION (MXVRTX) :: SXNL,SXEL
 REAL, DIMENSION(MRXTX,NTX,MQVR) :: XRXTX,YRXTX
 COMPLEX BFD(NFRQ,MRXTX,NTX,3)
 REAL(KIND=QL) RMUD(0:NLYR),THKD(NLYR),DPTHL(NLYR),ZS,ZR
 LOGICAL MDRX,EDRX,EPRX

 BFD = (0.,0.)
 DPTHL = 0._QL
 DO JZ = 2, NLYR
   DPTHL(JZ) = DPTHL(JZ-1) + THKD(JZ-1)
 END DO

 SELECT CASE (SOURCE_TYPE)
 CASE (1:2)                 ! Closed and Open Loops
   MDRX = .FALSE.
   EDRX = .FALSE.
   EPRX = .FALSE.
   ZS = REAL (SXZ(JS),QL)
   SXLYR = 0                ! Identify layer containing loop or GW source
   DO JZ = NLYR,1,-1
     IF (ZS > DPTHL(JZ)) THEN
       SXLYR = JZ
       EXIT
     END IF
   END DO
   NVRL = NVRTX(JS)
   SXNL(1:NVRL) = SXN(1:NVRL,JS)
   SXEL(1:NVRL) = SXE(1:NVRL,JS)

   IF (RXID(JR,JS) == 1) MDRX = .TRUE.          !    Magnetic dipole receivers
   IF (RXID(JR,JS) == 2) EDRX = .TRUE.          !    Electric dipole receivers
   IF (RXID(JR,JS) == 3) EPRX = .TRUE.          !    Electric field at a point

   IF (MDRX) CALL HSLPB (NFRQ,FREQ,SOURCE_TYPE,NTX,JS,SXLYR,NVRL,SXNL,SXEL,ZS,MRXTX,NRXTX,MQVR,RXID,XRXTX, &
                         YRXTX,ZRXTX,MXRHO,RHOTRP,NLYR,THKD,DPTHL,RES,RMUD,REPS,CHRG,CTAU,CFREQ,BFD)

   IF (EPRX) CALL HSLPE (NFRQ,FREQ,SOURCE_TYPE,NTX,JS,SXLYR,NVRL,SXNL,SXEL,ZS,MRXTX,NRXTX,MQVR,RXID,XRXTX, &
                         YRXTX,ZRXTX,MXRHO,RHOTRP,NLYR,THKD,DPTHL,RES,RMUD,REPS,CHRG,CTAU,CFREQ,BFD)

   IF (EDRX) CALL HSLPED (NFRQ,FREQ,SOURCE_TYPE,NTX,JS,SXLYR,NVRL,SXNL,SXEL,ZS,MRXTX,NRXTX,MQVR,RXID,XRXTX, &
                          YRXTX,ZRXTX,MXRHO,RHOTRP,NLYR,THKD,DPTHL,RES,RMUD,REPS,CHRG,CTAU,CFREQ,BFD)

 CASE (3)                    ! Magnetic dipole sources
   SXN1 = SXN(1,JS)
   SXE1 = SXE(1,JS)
   ZS = REAL (SXZ(JS),QL)
   SXDP1 = SXDIP(JS)
   SXAZM1 = SXAZM(JS)

   SXLYR = 0                ! Identify layer containing dipole source
   DO JZ = NLYR,1,-1
     IF (ZS > DPTHL(JZ)) THEN
       SXLYR = JZ
       EXIT
     END IF
   END DO

   XRX = XRXTX(JR,JS,1) - SXN1
   YRX = YRXTX(JR,JS,1) - SXE1  ! Compute receiver offsets.
   ZR = REAL (ZRXTX(JR,JS),QL)
   RXLYR = 0                    ! Set layer number and reference depth for receiver JR
   DO JZ = NLYR,1,-1
     IF (ZR > DPTHL(JZ)) THEN
       RXLYR = JZ
       EXIT
     END IF
   END DO
   CALL HSMDB (SXLYR,RXLYR,NFRQ,FREQ,ZS,SXDP1,SXAZM1,ZR,XRX,YRX,NLYR,THKD,DPTHL, &
               RES,RMUD,REPS,CHRG,CTAU,CFREQ,JS,JR,NTX,MRXTX,BFD)

 CASE (4)                     ! Coincident loop option
   ZS = REAL (SXZ(JS),QL)
   SXLYR = 0                ! Identify layer containing dipole source
   DO JZ = NLYR,1,-1
     IF (ZS > DPTHL(JZ)) THEN
       SXLYR = JZ
       EXIT
     END IF
   END DO

   NVRL = 4
   SXNL(1:4) = SXN(1:4,JS)
   SXEL(1:4) = SXE(1:4,JS)
   CALL HS_CDNT (NFRQ,FREQ,SXNL,SXEL,MXRHO,RHOTRP,NLYR,THKD,RES, &
                 RMUD,REPS,CHRG,CTAU,CFREQ,JS,NTX,BFD)

 END SELECT

 END SUBROUTINE HSBOSS

 SUBROUTINE COLRES_1D (FRQ,NLYR,RES,REPS,RMUD,CHRG,CTAU,CFREQ,SIGL,KSQL)
!-----------------------------------------------------------------------

! Computes KSQL & SIGL for each layer for each frequency.

!          Input
!          -----
!     FRQ - frequency
!    NLYR - number of layers
!     RES - layer resistivities
!    REPS - array of relative dielectric constants
!    RMUD - mu(i) / mu(0)
!    CHRG - chargeability array
!    CTAU - array of layer relaxation times (sec).
!   CFREQ - array of layer frequency parameters.
!           curently set to 5 cm.
!
!          Output for receiver in layer i
!          ------
!
!    SIGL - full wave complex conductivity
!    KSQL - full wave propagation constant


 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 REAL, PARAMETER :: TWOPI=6.2831853, MU0=12.56637E-7, EPS0=8.854156E-12
 COMPLEX, PARAMETER :: ONE=(1.,0.)
 INTEGER NLYR,J
 REAL FRQ
 REAL, DIMENSION(NLYR) :: RES,CHRG,CALF,CTAU,CFREQ,REPS
 REAL(KIND=QL) RMUD(0:NLYR)
 COMPLEX A1,IW,P
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL

 CALF = 1. - CHRG
 IW = TWOPI * CMPLX (0.,FRQ)
 DO J = 1,NLYR
   A1 = CMPLX (1./RES(J), 0.)
   P = (0.,0.)
   IF (CFREQ(J) > 1.E-6) P = (IW * CTAU(J) )**CFREQ(J)
   A1 = A1 * (ONE + P) / (ONE + CALF(J)*P)
   A1 = A1 + IW * EPS0 * REPS(J)  !  Add in displacement term
   SIGL(J) = CMPLX (A1,KIND=QL)
   A1 = IW * MU0* A1
   KSQL(J) = RMUD(J) * CMPLX (A1,KIND=QL)
 END DO

 END SUBROUTINE COLRES_1D

 SUBROUTINE HS_CDNT (NFRQ,FREQ,SXNL,SXEL,MXRHO,RHOTRP,NLYR,THKD,RES, &
                     RMUD,REPS,CHRG,CTAU,CFREQ,JS,NTX,BFD)
!-------------------------------------------------------------------

!***  Called by HSBOSS
!***  Calls COLRES_1D, HSLPLP_HNK, CUBSPL, CDCUBVAL

!  Valid for any number of layers for coincident loops which
!  must lie flat on the surface.

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!  SIGN CONVENTION:
!  ----------------
!  The normal layered earth field coordinate system used in this
!  subroutine has X (JC=1) positive north, Y (JC=2) positive east
!  and Z (JC=3) positive down.
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

!                             Input
!                             -----
!         FREQ - array of NFRQ frequencies
!   SXEL, SXNL - east-north coordinates for each vertex of Loop JS
!       RHOTRP - interpolation array of dimension MXRHO
!         NLYR - number of layers
!          RES - layer resistivities
!         RMUD - mu(i) / mu(0)   (i = 1:NLYR)
!         REPS - array of relative dislectric constants
!         THKD - layer thicknesses
!         CHRG - C-C chargeability
!         CTAU - C-C time constant in seconds
!        CFREQ - C-C frequency constant
!           JS - index of total of NTX transmitters
!
!                             Output
!                             ------
!  BFD(JF,1,JS,1)   - the layered earth response of flat coincident loop
!                     frequency JF, station JS.
!  BFD(JF,1,JS,2:3) = 0.

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18), NDIP0=5, MXDIP=100
 REAL, PARAMETER :: DIPL0 = 5.
 COMPLEX(KIND=QL), PARAMETER :: ZERO=(0._QL,0._QL)
 INTEGER NFRQ,MXRHO,NLYR,NTX,JF,JS,JX,JY,JR,JW,JD, &
         NDIP(4),NDPLP,NDPX,NDPY
 REAL FREQ(NFRQ),FRQ,SXNL(4),SXEL(4),RHOTRP(MXRHO),X1,XLOC,XSL,YSL,DPX,DPY,WTMD,XWRX2,WYRL(4),DIPL(4)
 REAL, DIMENSION(NLYR) :: RES,REPS,CTAU,CFREQ,CHRG
 REAL, DIMENSION(4,MXRHO) :: HL1R,HL1I
 REAL(KIND=QL) RMUD(0:NLYR),THKD(NLYR),YD,R1,DIPLD
 REAL, ALLOCATABLE :: YLOC(:),XRXMD(:),YRXMD(:),YWRX(:,:),RXRHO(:,:,:)
 COMPLEX HLYR(MXRHO),BFD(NFRQ,1,NTX,3)
 COMPLEX(KIND=QL) SIGL(NLYR),KSQL(NLYR),CDS1,BZR,BZG,BZV

!  This routine will produce B for unit TX moment so VFAC = RMUD(Txlyr) * MU0 / (4 * PI)
!  In this routine, the source is on the surface so RMUD(Txlyr) = 1

!  The G potential contributions are integrated around the loop.

 CALL HS_CDNT_SET_RX (SXNL,SXEL,NDPX,NDPY,XSL,YSL,DPX,DPY)

!  The coincident loop has now been adjusted to a rectangular loop at vertices
!  (0,0), (XSL,0), (XSL,YSL), (0,YSL)


 NDPLP = NDPX * NDPY
 WTMD = XSL * YSL / REAL(NDPLP)
 ALLOCATE (YLOC(NDPY),YWRX(NDPLP,4),RXRHO(NDPLP,MXDIP,4),XRXMD(NDPLP),YRXMD(NDPLP))

 DO JY = 1,NDPY
   YLOC(JY) = (JY - 0.5) * DPY
 END DO

 DO JX = 1,NDPX
   XLOC = (JX - 0.5) * DPX
   DO JY = 1,NDPY
     JR = JY + (JX-1) * NDPY
     XRXMD(JR) = XLOC
     YRXMD(JR) = YLOC(JY)
   END DO
 END DO

 WYRL(1) = XSL; WYRL(2) = YSL

 DO JW = 1,2
   NDIP(JW) = CEILING (WYRL(JW) / DIPL0)  ! 5 m initial dipole length
   NDIP(JW) = MAX (NDIP(JW), NDIP0)       ! At least 5 dipoles per segment
   NDIP(JW) = MIN (NDIP(JW), MXDIP)
   DIPL(JW) = WYRL(JW) / REAL (NDIP(JW))
   NDIP(JW+2) = NDIP(JW)
   DIPL(JW+2) = DIPL(JW)
 END DO

 DO JR = 1,NDPLP
   YWRX(JR,1) = YRXMD(JR)
   YWRX(JR,2) = XSL - XRXMD(JR)
   YWRX(JR,3) = YSL - YRXMD(JR)
   YWRX(JR,4) = XRXMD(JR)

   DO JW = 1,4
     DO JD = 1,NDIP(JW)
       X1 = (JD - 0.5) * DIPL(JW)
       IF (JW == 1 .OR. JW == 3) THEN
         XWRX2 = (XRXMD(JR) - X1)**2
       ELSE
         XWRX2 = (YRXMD(JR) - X1)**2
       END IF
       RXRHO(JR,JD,JW) =   SQRT (YWRX(JR,JW)**2   + XWRX2)
     END DO
   END DO
 END DO

 DO JF = 1,NFRQ
   HLYR = (0.,0.)
   FRQ = FREQ(JF)
   CALL COLRES_1D (FRQ,NLYR,RES,REPS,RMUD,CHRG,CTAU,CFREQ,SIGL,KSQL)

   CALL HSLPLP_HNK (MXRHO,RHOTRP,NLYR,KSQL,RMUD,THKD,HLYR)
   HL1R(1,1:MXRHO) = REAL (HLYR(1:MXRHO))
   CALL CUBSPL (RHOTRP,HL1R,MXRHO)
   HL1I(1,1:MXRHO) = AIMAG (HLYR(1:MXRHO))
   CALL CUBSPL (RHOTRP,HL1I,MXRHO)

!  Compute the contribution to the magnetic fields from the vertical magnetic (Gz)
!  potential due to a horizontal electric dipole.  For closed loops, the horizontal
!  fields are a simple integral of a J0 term around the loop.

   BZR = ZERO
   DO JR = 1,NDPLP               ! Sum over internal dipole receivers
     BZG = ZERO
     DO JW = 1,4             ! Sum over each wire segment of transmitting loop.
       BZV = ZERO
       YD = REAL (YWRX(JR,JW),KIND=QL)
       DIPLD = REAL (DIPL(JW),KIND=QL)
       DO JD = 1, NDIP(JW)                             ! Sum over each electric dipole
         R1 = REAL (RXRHO(JR,JD,JW), KIND=QL)       ! of each wire segment.
         CALL CDCUBVAL (RHOTRP,HL1R,HL1I,MXRHO,R1,CDS1)
         BZV = BZV + DIPLD * CDS1 * YD/R1              ! Accumulate Bx for all dipole segments
       END DO                                          ! for wire JW.
       BZG = BZG + BZV                                 ! Accumulate magnetic field for all segments
     END DO                                            ! for internal receiver

     BZR = BZR + BZG                                  ! Integrate over internal Rx for each loop
   END DO
   BFD(JF,1,JS,1) = WTMD * CMPLX (BZR)
   BFD(JF,1,JS,2:3) = (0.,0.)
 END DO     !  Next frequency

 DEALLOCATE (YLOC,YWRX,RXRHO,XRXMD,YRXMD)
 END SUBROUTINE HS_CDNT

 SUBROUTINE HS_CDNT_SET_RX (SXNL,SXEL,NDPX,NDPY,XSL,YSL,DPX,DPY)
!---------------------------------------------------------------
!
!  For layered half space computations:
!  If the coincident is not rectangular replace it with a rectangle of the same
!  area and diagonal whose length is equal to that of the maximum inter-vertex
!  distance of the original loop.
!
! INPUT PARAMETERS:
! ----------------
!
!    SXNL,SXEL - north & east coordinates of coincident loop vertices

! OUTPUT PARAMETERS:
! -----------------
!
!    XCLP,YCLP - north & east coordinates of equivalent retngular coincident loop vertices
!    NDPX,NDPY - numbers of magnetic dipole receivers in each direction.
!    DPX,DPY - magnetic dipole spatial intervals in each direction.
!
 IMPLICIT NONE
 INTEGER, PARAMETER :: DELMD=20, MXDP=1000, MNDP1=3
 INTEGER NDPX,NDPY,NDPRX
 REAL SXNL(4),SXEL(4),XSL,YSL,DPX,DPY,ALPHA,DIST2D

 XSL    = 0.5* (DIST2D (SXNL(1), SXEL(1), SXNL(2), SXEL(2)) + &
                DIST2D (SXNL(3), SXEL(3), SXNL(4), SXEL(4)) )
 YSL    = 0.5* (DIST2D (SXNL(1), SXEL(1), SXNL(4), SXEL(4)) + &
                DIST2D (SXNL(3), SXEL(3), SXNL(2), SXEL(2)) )

 NDPX = CEILING (XSL / DELMD)
 NDPY = CEILING (YSL / DELMD)

 NDPX = MAX (NDPX,MNDP1)
 NDPY = MAX (NDPY,MNDP1)
 NDPRX = NDPX * NDPY
 IF (NDPRX > MXDP) THEN
   ALPHA = SQRT (REAL (MXDP) / REAL(NDPRX) )
   NDPX = CEILING (ALPHA * NDPX)
   NDPY = CEILING (ALPHA * NDPY)
 END IF
 DPX = XSL / REAL (NDPX)
 DPY = YSL / REAL (NDPY)

 END SUBROUTINE HS_CDNT_SET_RX

 SUBROUTINE HSLPLP_HNK (MXRHO,RHOTRP,NLYR,KSQL,RMUD,THKD,HLYR)
!------------------------------------------------------------

!***  Calls HS_JMP, HSLPLP_KER
!***  Called by HSLPLP

!  VALID FOR ANY NUMBER OF LAYERS
!  LOOP TRANSMITTER FLAT ON EARTH SURFACE
!  MAGNETIC DIPOLE RECEIVER IN AIR OR AT ANY DEPTH IN ANY LAYER

!  Uses flow through Hankel transform to compute transform integrals HLYRD(MXRHO)
!  which are used to compute vertical and horizontal frequency-domain magnetic
!  field components at the RX from pseudo X oriented  HED source.  The Hankel
!  uses a 15 points per decade filter coefficient set derived from Christensen's
!  FLTGEN program.

!  RHOTRP - array of MXRHO logrithmically spaced (15 / decade) horizontal distances
!           at same spacing as lambda values for Hankel transform.
!     KER - stores kernel values from HSLPLP_KER

!  NLYR,KSQL,RMUD,THKD
!  are described in HSLPLP
!
!    OUTPUT is HLYR(1:MXRHO,)  forward model components

 USE BG_Filter_coefficients


 IMPLICIT NONE
 REAL(KIND=QL), PARAMETER :: VFAC0=1.0D-7
 INTEGER MXRHO,NLYR,K,L,JR,K1,KBOT,KMAX,KMIN,LMAX,LMIN
 REAL(KIND=QL) DELTA,Y,Y1,LMBDA,THKD(NLYR),RMUD(0:NLYR),RHOD
 REAL RHOTRP(MXRHO)
 COMPLEX HLYR(MXRHO)
 COMPLEX(KIND=QL) KSQL(NLYR),HLYRD(MXRHO),KER(JNLO-MXRHO:JNHI)
 LOGICAL JUMP

 DELTA = LOG (10.D0)/ DBLE (NDEC_JN)
 HLYRD = (0._QL, 0._QL)
 KER = (0._QL, 0._QL)

!  Set up KER for JR = 1 corresponding to minimum value of RHO.  This will
!  compute most of the needed kernel range from the high end.  Note that
!  the filter is defined between JNLO < L < JNHI

 JR = 1
 RHOD = REAL (RHOTRP(1),KIND=QL)
 Y1 = -LOG (RHOD) - SHFTJN

 DO L = -50, JNHI             ! Start at L = -50 to pick up low values.
   LMAX = L                   ! Maximum filter index used
   K = L + 1 - JR             ! K is the kernel index.
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
   CALL HSLPLP_KER (K,JR,L,LMBDA,NLYR,KSQL,RMUD,THKD,MXRHO,KER,HLYRD,JUMP)
   IF (JUMP .AND. L > -40) EXIT
 END DO

 JUMP = .FALSE.           ! Finish off the low end for RHOTRP(1)
 DO L = -51, JNLO, -1
   LMIN = L                   ! Maximum filter index used
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
   K = L + 1 - JR             ! Compute the kernel index.
   CALL HSLPLP_KER (K,JR,L,LMBDA,NLYR,KSQL,RMUD,THKD,MXRHO,KER,HLYRD,JUMP)
   IF (JUMP .AND. L < -60) EXIT
 END DO

 KMIN = LMIN + 1 - JR  !  Define the range of kernel values
 KMAX = LMAX + 1 - JR  !  used for RHOTRP(1)

! Complete definition of kernel values by evaluating transform of
! maximum RHO = RHOTRP (MXRHO)

 JR = MXRHO
 RHOD = REAL (RHOTRP(MXRHO),KIND=QL)
 Y1 = -LOG (RHOD) - SHFTJN
 KBOT = JNLO + 1 - JR
 K1 = MAX (KBOT,KMIN)

 DO K = K1, KMAX          ! Compute EHR for maximum RHO using previously
   L = K - 1 + JR         ! computed kernel values.
   HLYRD(JR) = HLYRD(JR) + KER(K) * WJ1(L)
 END DO

 IF (K1 > KBOT) THEN    !  Add low end kernel values until convergence.
   DO K = K1-1, KBOT, -1
     KMIN = K
     L = K - 1 + JR
     Y = Y1 + DBLE (L) * DELTA
     LMBDA = EXP (Y)
     CALL HSLPLP_KER (K,JR,L,LMBDA,NLYR,KSQL,RMUD,THKD,MXRHO,KER,HLYRD,JUMP)
     IF (JUMP) EXIT
   END DO
 END IF

 DO JR = 2, MXRHO-1          !  Compute transforms for all other RHO values
   DO K = KMIN, KMAX         !  using previously computed kernel values.
     L = K - 1 + JR
     HLYRD(JR) = HLYRD(JR) + KER(K) * WJ1(L)
   END DO
 END DO

 DO JR = 1,MXRHO
   RHOD = REAL (RHOTRP(JR),KIND=QL)
   HLYRD(JR) = VFAC0 * HLYRD(JR) / RHOD
 END DO
 HLYR = CMPLX (HLYRD)

 END SUBROUTINE HSLPLP_HNK

 SUBROUTINE HSLPLP_KER (K,JR,L,LMBDA,NLYR,KSQL,RMUD,THKD,MXRHO,KER,HLYRD,JUMP)
!----------------------------------------------------------------------------

!***  Called by HSLPLP__HNK

!  Computes the G potential kernels  for a flat surface loop transmitter
!  and a vertical component pseudo-receiver on the surface
!
!          Input
!          -----
!    MXRHO - number of logrithmically horizontal distances
!       K - kernel index
!      JR - RHO index
!       L - filter index
!   LMBDA = Hankel transform variable
!    NLYR - number of layers
!    KSQL - array of proagation constants
!    RMUD - mu(i) / mu(0)
!    THKD - layer thicknesses
!
!          Output for receiver in layer i
!          ------
!
!      KER(MXRHO) - kernel values for transform
!           JUMP - logical convergence indicator
!

 USE BG_Filter_coefficients

 IMPLICIT NONE
 REAL(KIND=QL), PARAMETER :: EXP_TOL=80.D0, TOL=1.D-6, TOL2=1.D-35
 COMPLEX(KIND=QL), PARAMETER :: ONE=(1._QL,0._QL),ZERO=(0._QL,0._QL)
 INTEGER MXRHO,K,JR,L,NLYR,J
 REAL(KIND=QL) THKD(NLYR),LMBDA,RMUD(0:NLYR),RMUSQ(NLYR),QR,QI
 COMPLEX(KIND=QL) LMBSQ,S0,T0,F0,XP1,FW,KER(JNLO-MXRHO:JNHI),HLYRD(MXRHO)
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: KSQL,S,XP,T,F
 LOGICAL JUMP

 XP = ZERO; T = ZERO

 S0 = CMPLX (LMBDA, 0._QL, KIND=QL)
 LMBSQ = CMPLX (LMBDA * LMBDA, 0._QL, KIND=QL)
 DO J = NLYR, 1, -1
   RMUSQ(J) = RMUD(J) * RMUD(J)
   S(J) = SQRT (KSQL(J) + LMBSQ)

   IF (J == NLYR) CYCLE
   T(J)= ( (RMUSQ(J+1) - RMUSQ(J)) * LMBSQ + RMUSQ(J+1)* KSQL(J) - RMUSQ(J)* KSQL(J+1) )  &
                                           / (RMUD(J+1)*   S(J) +  RMUD(J)*   S(J+1) )**2
   XP1 = 2* S(J) * THKD(J)
   IF ( REAL (XP1) < EXP_TOL) XP(J) = EXP (-XP1)
 END DO

 T0 = ( (RMUSQ(1) - 1._QL) * LMBSQ - KSQL(1) ) / ( RMUD(1)*S0 + S(1) )**2
 F = ZERO
 DO J = NLYR - 1, 1, -1
   F(J) = XP(J) * (T(J) + F(J+1) )/ (ONE + T(J) * F(J+1))
 END DO
 F0 = (T0 + F(1) )/ (ONE + T0 * F(1))

! Assume pseudo vertical receiver is 3 cm above ground.

 KER(K) = (F0 + ONE) * LMBDA * EXP (-0.03 * LMBDA)

!  Accumulate Hankel transform integrals & check convergence

 FW = WJ1(L) * KER(K)
 HLYRD(JR) = HLYRD(JR) + FW

 JUMP = .TRUE.
 QR = ABS (REAL  (HLYRD(JR), KIND=QL) )
 QI = ABS (AIMAG (HLYRD(JR) ) )
 IF (QR > TOL2 .AND. ABS (REAL  (FW)) > TOL * QR) JUMP = .FALSE.
 IF (QI > TOL2 .AND. ABS (AIMAG (FW)) > TOL * QI) JUMP = .FALSE.

 END SUBROUTINE HSLPLP_KER

 SUBROUTINE HSLPB (NFRQ,FREQ,SOURCE_TYPE,NTX,JS,SXLYR,NVRL,SXNL,SXEL,ZS,MRXTX,NRXTX,MQVR,RXID,XRXTX, &
                   YRXTX,ZRXTX,MXRHO,RHOTRP,NLYR,THKD,DPTHL,RES,RMUD,REPS,CHRG,CTAU,CFREQ,BFD)
!--------------------------------------------------------------------------------------------------

!***  Called by HSBOSS
!***  Calls COLRES_1D, HSCLB_HNK, HSGWB_HNK, CUBSPL, CDCUBVAL

!  Valid for any number of layers
!  Computes the frequency-domain layered earth B field for a flat-lying loop
!  or grounded wire source.
!  Closed Loop can be in air or at any depth in any layer
!  Open loop must have electrodes below earth surface.
!  Magnetic dipole receiver in air or at any depth in any layer


!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!  SIGN CONVENTION:
!  ----------------
!  The normal layered earth field coordinate system used in this
!  subroutine has X (JC=1) positive north, Y (JC=2) positive east
!  and Z (JC=3) positive down.
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

!                             Input
!                             -----
!         FREQ - array of NFRQ frequencies
!  SOURCE_TYPE - 1 => closed loop;  2 => open loop (grounded wire)
!          NTX - number of transmitters
!        SXLYR - layer containing transmitter JS
!         NVRL - number of vertices for loop JS
!         SXNL - north coordinates for vertices for loop JS
!         SXEL - east coordinates for vertices for loop JS
!           ZS - depth from surface for flat lying loop JS
!        MRXTX - maximum number of receivers per transmitter
!     NRXTX(I) - number of receivers for transmitter I
!         MQVR - maximum number of vertices for all receivers
!    RXID(I,J) - RX_TYPE of receiver I for transmitter J. 1 => MD; 2 => ED
! XRXTX(J,I,K) - north coordinate of the Kth vertex of the Jth receiver of transmitter I
! YRXTX(J,I,K) - east coordinate of the Kth vertex of the Jth receiver of transmitter I
!   ZRXTX(J,I) - depth of the Jth receiver of transmitter I
!       RHOTRP - MXRHO size interpolation array for horizontal distances
!         NLYR - number of layers
!         THKD - layer thicknesses
!        DPTHL - depth to top of layer
!          RES - layer resistivities
!         RMUD - mu(i) / mu(0)   (i = 0:NLYR)
!         REPS - array of relative dislectric constants
!         CHRG - C-C chargeability
!         CTAU - C-C time constant in seconds
!        CFREQ - C-C frequency constant
!
!                             Output
!                             ------
!  BFD(JF,JR,JS,I) - the Ith component of the layered earth response at
!                    frequency JF, receiver JR, station JS.
!           I = 1,2,3 = north, east and vertical componsnts respectively

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18), NDIP0=5, MXDIP=100
 REAL, PARAMETER :: DIPL0 = 5.
 COMPLEX(KIND=QL), PARAMETER :: ZERO=(0._QL,0._QL)
 INTEGER NTX,MRXTX,NRXTX(NTX),MQVR,RXID(MRXTX,NTX),SXLYR,RXLYR,NFRQ,MXRHO,SOURCE_TYPE,NVRL,NVRLS,NLYR, &
         J1,JS,JR,JF,JV,JV1,JD,NDIP(NVRL),I1,KFG,GAM
 REAL FREQ(NFRQ),FRQ,ZRXTX(MRXTX,NTX),RXVRT,SXNL(NVRL),SXEL(NVRL),XRX,YRX,RHOTRP(MXRHO),X1,XWRX(NVRL,2,MRXTX), &
      BRFD(3),BIFD(3),BRFDM,BIFDM,A1,A2
 REAL, DIMENSION(MRXTX,NTX,MQVR) :: XRXTX,YRXTX
 REAL, DIMENSION(NLYR) :: RES,REPS,CTAU,CFREQ,CHRG
 REAL, DIMENSION(NVRL) :: CPHI,SPHI,WYRL,DIPL
 REAL, DIMENSION(NVRL,MRXTX) :: YWRX
 REAL, DIMENSION(MXDIP,NVRL,MRXTX) :: XRXD,RXRHO
 REAL, DIMENSION(4,MXRHO) :: HL1R,HL2R,HL3R,HL1I,HL2I,HL3I
 REAL(KIND=QL) ZR,ZS,ZPRV,RMUD(0:NLYR),THKD(NLYR),DPTHL(NLYR),YD,X1D,X2D,RHO1,RHO2,DIPLD,CPHID,SPHID
 COMPLEX HLYR(MXRHO,3),BFD(NFRQ,MRXTX,NTX,3)
 COMPLEX(KIND=QL) SIGL(NLYR),KSQL(NLYR),CDS1,CDS2,BX,BY,BZ,BX0,BY0,BFDD(3)

!  The fields have two parts, a non-conservative (path-dependent) integration over all closed
!  segments and a conservative (path-independent) part which is integrated on a direct path
!  between electrodes.  Obviously this gives a null contribution for closed loops.

!  All parts have the commpn multiplier VFAC = RMUD(Txlyr) * MU0 / (4 * PI) = 1.D-7 * RMUD(Txlyr)

!  Loop contributions are evaluated segment by segment (open or closed) in an initial
!  coordinate system whose X component lies in the direction of the segment.  The local
!  field copmponents are rotated back into the survey soordinate system.

 BX0 = ZERO;  BY0 = ZERO    !  Open segment contributions

 NVRLS = NVRL
 IF (SOURCE_TYPE == 2) NVRLS = NVRL -1

 DO JV = 1, NVRL
   JV1 = JV + 1             ! Wire JV goes from vertex JV to vertex JV1
   IF (JV == NVRL) JV1 = 1

! Length & orientation of Wire JV between vertices JV & JV1

   WYRL(JV) = SQRT ( (SXNL(JV) - SXNL(JV1))**2 + (SXEL(JV) - SXEL(JV1))**2)
   CPHI(JV) = (SXNL(JV1) - SXNL(JV) ) / WYRL(JV)
   SPHI(JV) = (SXEL(JV1) - SXEL(JV) ) / WYRL(JV)

! Divide each wire into segment lengths of 5 m with a minimum of 5 segments per wire.

   NDIP(JV) = CEILING (WYRL(JV) / DIPL0)  ! 5 m initial dipole length
   NDIP(JV) = MAX (NDIP(JV), NDIP0)   ! At least 5 dipoles per segment
   NDIP(JV) = MIN (NDIP(JV), MXDIP)
   DIPL(JV) = WYRL(JV) / REAL (NDIP(JV))

   DO JR = 1,NRXTX(JS)
     IF (RXID(JR,JS) /= 1) CYCLE     ! This subroutine is only used for magnetic dipole receivers
     XRX = XRXTX(JR,JS,1)
     YRX = YRXTX(JR,JS,1)

! Distances from first vertex of wire segment JV to receiver  (Y = constant in new system)
     XWRX(JV,1,JR) = (XRX - SXNL(JV)) *  CPHI(JV) + (YRX-SXEL(JV)) * SPHI(JV)
     XWRX(JV,2,JR) = (XRX - SXNL(JV1)) * CPHI(JV) + (YRX-SXEL(JV1)) * SPHI(JV)

     YWRX(JV,JR) = -(XRX - SXNL(JV)) * SPHI(JV) + (YRX-SXEL(JV)) * CPHI(JV)

!  Compute distamces from each dipole centre to receiver in coordinate system
!  where wire runs along the new X (north) axis.

     DO JD = 1,NDIP(JV)
       X1 = (JD - 0.5) * DIPL(JV)
       XRXD(JD,JV,JR) =  XWRX(JV,1,JR) - X1
       RXRHO(JD,JV,JR) = SQRT (XRXD(JD,JV,JR)**2 + YWRX(JV,JR)**2)
       RXRHO(JD,JV,JR) = MAX (RXRHO(JD,JV,JR), 0.1)
     END DO
   END DO
 END DO

!  Interpolation array for Hankel integrals

 DO JF = 1,NFRQ
   FRQ = FREQ(JF)
   CALL COLRES_1D (FRQ,NLYR,RES,REPS,RMUD,CHRG,CTAU,CFREQ,SIGL,KSQL)

   ZPRV = -9.D4
   I1 = 2                              ! electric source
   DO JR = 1, NRXTX(JS)                ! Loop over receivers
     IF (RXID(JR,JS) /= 1) CYCLE       ! This subroutine is only used for magnetic dipole receivers
     BFDD = ZERO
     ZR = REAL (ZRXTX(JR,JS),QL)
     RXVRT = REAL (ABS (ZPRV - ZR))
     IF (RXVRT > .01_QL) THEN
       HLYR = (0.,0.)
       ZPRV = ZR
       RXLYR = 0
       DO J1 = NLYR,1,-1
         IF (ZR > DPTHL(J1)) THEN
           RXLYR = J1
           EXIT
         END IF
       END DO
       CALL SET_KFG (I1,NLYR,SXLYR,RXLYR,KFG)
       GAM = 0
       IF (RXLYR == SXLYR) THEN       !  Set whole space term
         IF (ZR > ZS) GAM = -1
         IF (ZR < ZS) GAM =  1
         IF (ABS (ZR - ZS) < 0.01_QL) GAM = 0
       END IF

       IF (SOURCE_TYPE == 2) THEN
         CALL HSGWB_HNK (MXRHO,RHOTRP,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,KFG,GAM,SXLYR,RXLYR,HLYR)
       ELSE
         CALL HSCLB_HNK (MXRHO,RHOTRP,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,KFG,GAM,SXLYR,RXLYR,HLYR)
       END IF

       HL1R(1,1:MXRHO) = REAL (HLYR(1:MXRHO,1))   ! For integration of non-conservative components over connected segments
       CALL CUBSPL (RHOTRP,HL1R,MXRHO)
       HL1I(1,1:MXRHO) = AIMAG (HLYR(1:MXRHO,1))
       CALL CUBSPL (RHOTRP,HL1I,MXRHO)
       HL2R(1,1:MXRHO) = REAL (HLYR(1:MXRHO,2))
       CALL CUBSPL (RHOTRP,HL2R,MXRHO)
       HL2I(1,1:MXRHO) = AIMAG (HLYR(1:MXRHO,2))
       CALL CUBSPL (RHOTRP,HL2I,MXRHO)
     END IF

!  Integrate non-conservative (path dependent) components around all closed segments

     BX = ZERO
     DO JV = 1,NVRLS
       YD = REAL (YWRX(JV,JR),KIND=QL)
       DIPLD = REAL (DIPL(JV),KIND=QL)
       CPHID = REAL (CPHI(JV),KIND=QL)
       SPHID = REAL (SPHI(JV),KIND=QL)
       BZ = ZERO; BY = ZERO
       DO JD = 1, NDIP(JV)                     !  Integrate over wire JV for By and Bz fields
         RHO1 = REAL (RXRHO(JD,JV,JR), KIND=QL)
         CALL CDCUBVAL (RHOTRP,HL1R,HL1I,MXRHO,RHO1,CDS1)
         BY = BY + CDS1
         CALL CDCUBVAL (RHOTRP,HL2R,HL2I,MXRHO,RHO1,CDS2)
         BZ = BZ + YD * CDS2
       END DO

! Rotate these back to the North, East, Z system.

       BY = DIPLD * BY
       BZ = DIPLD * BZ

       BFDD(1) = BFDD(1) + BX * CPHID - BY * SPHID
       BFDD(2) = BFDD(2) + BY * CPHID + BX * SPHID
       BFDD(3) = BFDD(3) + BZ

     END DO   !  Next wire

     IF (SOURCE_TYPE == 2) THEN
       HL3R(1,1:MXRHO) = REAL (HLYR(1:MXRHO,3))
       CALL CUBSPL (RHOTRP,HL3R,MXRHO)
       HL3I(1,1:MXRHO) = AIMAG (HLYR(1:MXRHO,3))
       CALL CUBSPL (RHOTRP,HL3I,MXRHO)

!  Integrate conservative term over open side for GW source.
!  The open loop path is positive from vertex 1 to vertex NVRL, the reverse of
!  the closed loop path being positive from vertex NVRL to vertex 1.

       CPHI(NVRL) = (SXNL(NVRL) - SXNL(1) ) / WYRL(NVRL)
       SPHI(NVRL) = (SXEL(NVRL) - SXEL(1) ) / WYRL(NVRL)
       CPHID = REAL (CPHI(NVRL),KIND=QL)
       SPHID = REAL (SPHI(NVRL),KIND=QL)
       XWRX(NVRL,1,JR) =  (XRX - SXNL(1)) * CPHI(NVRL) + (YRX-SXEL(1)) * SPHI(NVRL)
       XWRX(NVRL,2,JR) =  (XRX - SXNL(NVRL)) * CPHI(NVRL) + (YRX-SXEL(NVRL)) * SPHI(NVRL)
       YWRX(NVRL,JR) = -(XRX - SXNL(1)) * SPHI(NVRL) + (YRX-SXEL(1)) * CPHI(NVRL)

       X1D = REAL (XWRX(NVRL,1,JR), KIND=QL)   ! for open segment.
       X2D = REAL (XWRX(NVRL,2,JR), KIND=QL)
       YD = REAL (YWRX(NVRL,JR),KIND=QL)
       RHO1 = SQRT (X1D**2 + YD**2)
       RHO2 = SQRT (X2D**2 + YD**2)

       CALL CDCUBVAL (RHOTRP,HL3R,HL3I,MXRHO,RHO1,CDS1)
       CALL CDCUBVAL (RHOTRP,HL3R,HL3I,MXRHO,RHO2,CDS2)
       BX0 = YD * (CDS2 - CDS1)
       BY0 = X1D * CDS1 - X2D * CDS2

       BFDD(1) = BFDD(1) + BX0 * CPHID - BY0 * SPHID
       BFDD(2) = BFDD(2) + BY0 * CPHID + BX0 * SPHID
     END IF

     BFD(JF,JR,JS,1:3) = CMPLX (BFDD(1:3))
     BRFD(1:3) =  REAL (BFD(JF,JR,JS,1:3) )
     BIFD(1:3) = AIMAG (BFD(JF,JR,JS,1:3) )
     BRFDM = MAXVAL (ABS (BRFD) )
     BIFDM = MAXVAL (ABS (BIFD) )
     DO JD = 1,3
       A1 = 0.;  A2 = 0.
       IF (ABS (BRFD(JD)) > 1.E-8 * BRFDM) A1 = BRFD(JD)
       IF (ABS (BIFD(JD)) > 1.E-8 * BIFDM) A2 = BIFD(JD)
       BFD(JF,JR,JS,JD) = CMPLX (A1,A2)
     END DO
   END DO     !  Next receiver
 END DO     !  Next frequency

END SUBROUTINE HSLPB

 SUBROUTINE HSCLB_HNK (MXRHO,RHOTRP,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,KFG,GAM,SXLYR,RXLYR,HLYR)
!-----------------------------------------------------------------------------------------------

!***  Calls HSCLB_KER
!***  Called by HSLPB


!  Uses flow through Hankel transform to compute electric dipole response integrals
!  HLYR(MXRHO,1:NKR) used to compute the path-dependent frequency-domain magnetic
!  field components at the RX from pseudo X oriented  HED source.  The Hankel transform
!  uses a 15 points per decade filter coefficient set derived from Christensen's
!  FLTGEN program.

!          Input
!          -----
!    MXRHO - number of logrithmically horizontal distances
!  RHOTRP - array of MXRHO logrithmically spaced (15 / decade) horizontal distances
!           at same spacing as lambda values for Hankel transform.
!       L - filter index
!      ZS - depth of flat-lying loop
!      ZR - receiver depth
!    NLYR - number of layers
!    THKD - layer thicknesses
!   DPTHL - depth to top of layers (DPTHL = depth to basement
!    RMUD - mu(i) / mu(0)
!    SIGL - complex layer conductivities
!    KSQL - complex layer propagation constants
!     KFG - Tx-Rx indicator for coefficient generation
!   SXLYR - layer containing transmitter
!   RXLYR - layer containing receiver

!    OUTPUT is HLYR(1:MXRHO,1:3)

 USE BG_Filter_coefficients

 IMPLICIT NONE
 INTEGER, PARAMETER :: NKR=2
 REAL(KIND=QL), PARAMETER :: VFAC0=1.0D-7
 INTEGER KFG,MXRHO,NLYR,GAM,RXLYR,SXLYR,K,L,JR,K1,KBOT,KMAX,KMIN,LMAX,LMIN
 REAL RHOTRP(MXRHO)
 REAL(KIND=QL) ZS,ZR,DELTA,Y,Y1,LMBDA,RMUD(0:NLYR),RHOD
 REAL(KIND=QL), DIMENSION (NLYR) :: THKD,DPTHL
 COMPLEX HLYR(MXRHO,3)
 COMPLEX(KIND=QL) HLYRD(MXRHO,3),KER(JNLO-MXRHO:JNHI,NKR)
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL
 LOGICAL JUMP

 DELTA = LOG (10.D0)/ DBLE (NDEC_JN)
 HLYRD = (0._QL, 0._QL)
 KER = (0._QL, 0._QL)

!  Set up KER for JR = 1 corresponding to minimum value of RHO.  This will
!  compute most of the needed kernel range from the high end.  Note that
!  the filter is defined between JNLO < L < JNHI

 JR = 1
 RHOD = REAL (RHOTRP(1),KIND=QL)
 Y1 = -LOG (RHOD) - SHFTJN

 DO L = -50, JNHI             ! Start at L = -50 to pick up low values.
   LMAX = L                   ! Maximum filter index used
   K = L + 1 - JR             ! K is the kernel index.
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
    CALL HSCLB_KER (MXRHO,K,JR,L,LMBDA,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                    KFG,GAM,SXLYR,RXLYR,NKR,KER,HLYRD,JUMP)
   IF (JUMP .AND. L > -40) EXIT
 END DO

 JUMP = .FALSE.           ! Finish off the low end for RHOTRP(1)
 DO L = -51, JNLO, -1
   LMIN = L                   ! Maximum filter index used
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
   K = L + 1 - JR             ! Compute the kernel index.
   CALL HSCLB_KER (MXRHO,K,JR,L,LMBDA,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                   KFG,GAM,SXLYR,RXLYR,NKR,KER,HLYRD,JUMP)
   IF (JUMP .AND. L < -60) EXIT
 END DO

 KMIN = LMIN + 1 - JR  !  Define the range of kernel values
 KMAX = LMAX + 1 - JR  !  used for RHOTRP(1)

! Complete definition of kernel values by evaluating transform of
! maximum RHO = RHOTRP (MXRHO)

 JR = MXRHO
 RHOD = REAL (RHOTRP(MXRHO),KIND=QL)
 Y1 = -LOG (RHOD) - SHFTJN
 KBOT = JNLO + 1 - JR
 K1 = MAX (KBOT,KMIN)

 DO K = K1, KMAX          ! Compute EHR for maximum RHO using previously
   L = K - 1 + JR         ! computed kernel values.
   HLYRD(JR,1) = HLYRD(JR,1) + KER(K,1) * WJ0(L)
   HLYRD(JR,2) = HLYRD(JR,2) + KER(K,2) * WJ1(L)
 END DO

 IF (K1 > KBOT) THEN    !  Add low end kernel values until convergence.
   DO K = K1-1, KBOT, -1
     KMIN = K
     L = K - 1 + JR
     Y = Y1 + DBLE (L) * DELTA
     LMBDA = EXP (Y)
     CALL HSCLB_KER (MXRHO,K,JR,L,LMBDA,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                     KFG,GAM,SXLYR,RXLYR,NKR,KER,HLYRD,JUMP)
     IF (JUMP) EXIT
   END DO
 END IF

 DO JR = 2, MXRHO-1          !  Compute transforms for all other RHO values
   DO K = KMIN, KMAX         !  using previously computed kernel values.
     L = K - 1 + JR
     HLYRD(JR,1) = HLYRD(JR,1) + KER(K,1) * WJ0(L)
     HLYRD(JR,2) = HLYRD(JR,2) + KER(K,2) * WJ1(L)
   END DO
 END DO

 DO JR = 1,MXRHO
   RHOD = REAL (RHOTRP(JR),KIND=QL)
   HLYRD(JR,2) = HLYRD(JR,2) / RHOD
   HLYRD(JR,1:2) = VFAC0 * RMUD(RXLYR) * HLYRD(JR,1:2) / RHOD
 END DO
 HLYR = CMPLX (HLYRD)

END SUBROUTINE HSCLB_HNK

 SUBROUTINE HSCLB_KER (MXRHO,K,JR,L,LMBDA,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                       KFG,GAM,SXLYR,RXLYR,NKR,KER,HLYRD,JUMP)
!----------------------------------------------------------------------------

!***  Called by HSLPM0__HNK

!  Closed loop kernels and dipole response integrals for the path-dependent response from a
!  flat closed-loop transmitter and a surface or downhole magnetic dipole receiver.
!  Correction of long-standing error: GAM is necessary in KER(K,1)
!
!          Input
!          -----
!    MXRHO - number of logrithmically horizontal distances
!       K - kernel index
!      JR - RHO index
!       L - filter index
!   LMBDA = Hankel transform variable
!      ZS - depth of flat-lying loop
!      ZR - receiver depth
!    NLYR - number of layers
!    THKD - layer thicknesses
!   DPTHL - depth to top of layers (DPTHL = depth to basement
!    RMUD - mu(i) / mu(0)
!    SIGL - complex layer conductivities
!    KSQL - complex layer propagation constants
!     KFG - Tx-Rx indicator for coefficient generation
!   SXLYR - layer containing transmitter
!   RXLYR - layer containing receiver
!     NKR - kernel dimension
!
!          Output for receiver in layer i
!          ------
!
!    KER(MXRHO,NKR) - kernel values for transform
!            HLYRD - dipole response integral
!             JUMP - logical convergence indicator

 USE BG_Filter_coefficients

 IMPLICIT NONE
 REAL(KIND=QL), PARAMETER :: TOL=1.D-6, TOL2=1.D-35
 INTEGER MXRHO,K,JR,L,NLYR,KFG,GAM,SXLYR,RXLYR,NKR,JINT
 REAL(KIND=QL) LMBDA,LMBDA2,ZR,ZS,RMUD(0:NLYR),QR,QI
 REAL(KIND=QL), DIMENSION (NLYR) :: THKD,DPTHL
 COMPLEX(KIND=QL) S(0:NLYR),XI_V,F_V,F_H,ETA_V,G_V,G_H,SL,SM,XPDIR,FW(NKR), &
                  FACV,KER(JNLO-MXRHO:JNHI,NKR),HLYRD(MXRHO,3)
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL
 LOGICAL JUMP

 CALL EDSX_COEF (KFG,RXLYR,SXLYR,LMBDA,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                 ZS,S,XI_V,F_V,F_H,ETA_V,G_V,G_H)

 LMBDA2 = LMBDA**2
 SL = S(RXLYR)
 SM = S(SXLYR)
 FACV = RMUD(SXLYR) * SL / (RMUD(RXLYR) * SM)
 XPDIR = (0._QL,0._QL)
 IF (SXLYR == RXLYR) XPDIR = EXP (-SL * ABS ( ZR-ZS))

 IF (RXLYR < NLYR) XI_V =  XI_V * EXP (SL * (ZR - DPTHL(RXLYR+1)))
 IF (RXLYR > 0)   ETA_V = ETA_V * EXP (SL * (DPTHL(RXLYR) - ZR))

 KER(K,1) = FACV * (XI_V - ETA_V + GAM * XPDIR) * LMBDA     !  Local Y component
 KER(K,2) = (XI_V + ETA_V + XPDIR) * LMBDA2 / S(SXLYR)      !  Vertical component

!  Accumulate Hankel transform integrals & check convergence

 FW(1) = WJ0(L) * KER(K,1)
 FW(2) = WJ1(L) * KER(K,2)
 HLYRD(JR,1:NKR) = HLYRD(JR,1:NKR) + FW(1:NKR)

 JUMP = .TRUE.
 DO JINT = 1,NKR
   QR = ABS (REAL  (HLYRD(JR,JINT), KIND=QL) )
   QI = ABS (AIMAG (HLYRD(JR,JINT) ) )
   IF (QR > TOL2 .AND. ABS (REAL  (FW(JINT))) > TOL * QR) JUMP = .FALSE.
   IF (QI > TOL2 .AND. ABS (AIMAG (FW(JINT))) > TOL * QI) JUMP = .FALSE.
 END DO

END SUBROUTINE HSCLB_KER

 SUBROUTINE HSGWB_HNK (MXRHO,RHOTRP,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,KFG,GAM,SXLYR,RXLYR,HLYR)
!------------------------------------------------------------------------------------------------

!***  Calls HSGWB_KER
!***  Called by HSLPB


!  Uses flow through Hankel transform to compute electric dipole response integrals
!  HLYR(MXRHO,1:NKR) used to compute the closed and openloop frequency-domain magnetic
!  field components at the RX from pseudo X oriented  HED source.  The Hankel transform
!  uses a 15 points per decade filter coefficient set derived from Christensen's
!  FLTGEN program.

!          Input
!          -----
!    MXRHO - number of logrithmically horizontal distances
!  RHOTRP - array of MXRHO logrithmically spaced (15 / decade) horizontal distances
!           at same spacing as lambda values for Hankel transform.
!       L - filter index
!      ZS - depth of flat-lying loop
!      ZR - receiver depth
!    NLYR - number of layers
!    THKD - layer thicknesses
!   DPTHL - depth to top of layers (DPTHL = depth to basement
!    RMUD - mu(i) / mu(0)
!    SIGL - complex layer conductivities
!    KSQL - complex layer propagation constants
!     KFG - Tx-Rx indicator for coefficient generation
!   SXLYR - layer containing transmitter
!   RXLYR - layer containing receiver

!    OUTPUT is HLYR(1:MXRHO,1:3)

 USE BG_Filter_coefficients

 IMPLICIT NONE
 INTEGER, PARAMETER :: NKR=3
 REAL(KIND=QL), PARAMETER :: VFAC0=1.0D-7
 INTEGER KFG,MXRHO,NLYR,GAM,RXLYR,SXLYR,K,L,JR,K1,KBOT,KMAX,KMIN,LMAX,LMIN
 REAL RHOTRP(MXRHO)
 REAL(KIND=QL) ZS,ZR,DELTA,Y,Y1,LMBDA,RMUD(0:NLYR),RHOD
 REAL(KIND=QL), DIMENSION (NLYR) :: THKD,DPTHL
 COMPLEX HLYR(MXRHO,3)
 COMPLEX(KIND=QL) HLYRD(MXRHO,3),KER(JNLO-MXRHO:JNHI,NKR)
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL
 LOGICAL JUMP

 DELTA = LOG (10.D0)/ DBLE (NDEC_JN)
 HLYRD = (0._QL, 0._QL)
 KER = (0._QL, 0._QL)

!  Set up KER for JR = 1 corresponding to minimum value of RHO.  This will
!  compute most of the needed kernel range from the high end.  Note that
!  the filter is defined between JNLO < L < JNHI

 JR = 1
 RHOD = REAL (RHOTRP(1),KIND=QL)
 Y1 = -LOG (RHOD) - SHFTJN

 DO L = -50, JNHI             ! Start at L = -50 to pick up low values.
   LMAX = L                   ! Maximum filter index used
   K = L + 1 - JR             ! K is the kernel index.
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
    CALL HSGWB_KER (MXRHO,K,JR,L,LMBDA,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                    KFG,GAM,SXLYR,RXLYR,NKR,KER,HLYRD,JUMP)
   IF (JUMP .AND. L > -40) EXIT
 END DO

 JUMP = .FALSE.           ! Finish off the low end for RHOTRP(1)
 DO L = -51, JNLO, -1
   LMIN = L                   ! Maximum filter index used
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
   K = L + 1 - JR             ! Compute the kernel index.
   CALL HSGWB_KER (MXRHO,K,JR,L,LMBDA,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                   KFG,GAM,SXLYR,RXLYR,NKR,KER,HLYRD,JUMP)
   IF (JUMP .AND. L < -60) EXIT
 END DO

 KMIN = LMIN + 1 - JR  !  Define the range of kernel values
 KMAX = LMAX + 1 - JR  !  used for RHOTRP(1)

! Complete definition of kernel values by evaluating transform of
! maximum RHO = RHOTRP (MXRHO)

 JR = MXRHO
 RHOD = REAL (RHOTRP(MXRHO),KIND=QL)
 Y1 = -LOG (RHOD) - SHFTJN
 KBOT = JNLO + 1 - JR
 K1 = MAX (KBOT,KMIN)

 DO K = K1, KMAX          ! Compute EHR for maximum RHO using previously
   L = K - 1 + JR         ! computed kernel values.
   HLYRD(JR,1)   = HLYRD(JR,1) + WJ0(L) * KER(K,1)
   HLYRD(JR,2:3) = HLYRD(JR,2) + WJ1(L) * KER(K,2:3)
 END DO

 IF (K1 > KBOT) THEN    !  Add low end kernel values until convergence.
   DO K = K1-1, KBOT, -1
     KMIN = K
     L = K - 1 + JR
     Y = Y1 + DBLE (L) * DELTA
     LMBDA = EXP (Y)
     CALL HSGWB_KER (MXRHO,K,JR,L,LMBDA,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                     KFG,GAM,SXLYR,RXLYR,NKR,KER,HLYRD,JUMP)
     IF (JUMP) EXIT
   END DO
 END IF

 DO JR = 2, MXRHO-1          !  Compute transforms for all other RHO values
   DO K = KMIN, KMAX         !  using previously computed kernel values.
     L = K - 1 + JR
     HLYRD(JR,1)   = HLYRD(JR,1) + WJ0(L) * KER(K,1)
     HLYRD(JR,2:3) = HLYRD(JR,2) + WJ1(L) * KER(K,2:3)
   END DO
 END DO

 DO JR = 1,MXRHO
   RHOD = REAL (RHOTRP(JR),KIND=QL)
   HLYRD(JR,2:3) = HLYRD(JR,2:3) / RHOD
   HLYRD(JR,1:3) = VFAC0 * RMUD(RXLYR) * HLYRD(JR,1:3) / RHOD
 END DO
 HLYR = CMPLX (HLYRD)

END SUBROUTINE HSGWB_HNK

 SUBROUTINE HSGWB_KER (MXRHO,K,JR,L,LMBDA,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                       KFG,GAM,SXLYR,RXLYR,NKR,KER,HLYRD,JUMP)
!----------------------------------------------------------------------------

!***  Called by HSLPM0__HNK
!***  Calls EDSX_COEF

!  Kernels and magnetic dipole response integrals for response from aclosed or grounded
!  loop transmitter and a surface or downhole magnetic dipole receiver.
!
!          Input
!          -----
!    MXRHO - number of logrithmically horizontal distances
!       K - kernel index
!      JR - RHO index
!       L - filter index
!   LMBDA = Hankel transform variable
!      ZS - depth of flat-lying loop
!      ZR - receiver depth
!    NLYR - number of layers
!    THKD - layer thicknesses
!   DPTHL - depth to top of layers (DPTHL = depth to basement
!    RMUD - mu(i) / mu(0)
!    SIGL - complex layer conductivities
!    KSQL - complex layer propagation constants
!     KFG - Tx-Rx indicator for coefficient generation
!   SXLYR - layer containing transmitter
!   RXLYR - layer containing receiver
!     NKR - kernel dimension
!
!          Output for receiver in layer i
!          ------
!
!    KER(MXRHO,NKR) - kernel values for transform
!            HLYRD - dipole response integral
!             JUMP - logical convergence indicator

 USE BG_Filter_coefficients

 IMPLICIT NONE
 REAL(KIND=QL), PARAMETER :: TOL=1.D-6, TOL2=1.D-35
 INTEGER MXRHO,K,JR,L,NLYR,KFG,GAM,RXLYR,SXLYR,NKR,JINT
 REAL(KIND=QL) LMBDA,LMBDA2,ZR,ZS,RMUD(0:NLYR),QR,QI
 REAL(KIND=QL), DIMENSION (NLYR) :: THKD,DPTHL
 COMPLEX(KIND=QL) S(0:NLYR),XI_V,F_V,F_H,ETA_V,G_V,G_H,XP,XPDIR,SL,SM,FW(NKR), &
                  FACV,KER(JNLO-MXRHO:JNHI,NKR),HLYRD(MXRHO,NKR)
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL
 LOGICAL JUMP

 CALL EDSX_COEF (KFG,RXLYR,SXLYR,LMBDA,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                 ZS,S,XI_V,F_V,F_H,ETA_V,G_V,G_H)

 LMBDA2 = LMBDA**2
 SL = S(RXLYR)
 SM = S(SXLYR)
 XPDIR = (0._QL,0._QL)
 IF (SXLYR == RXLYR) XPDIR = EXP (-SL * ABS ( ZR-ZS))

 IF (RXLYR < NLYR) THEN
   XP = EXP (SL * (ZR - DPTHL(RXLYR+1)))
   F_H  = XP * F_H
   XI_V = XP * XI_V
 END IF
 IF (RXLYR > 0) THEN
   XP = EXP (SL * (DPTHL(RXLYR) - ZR))
   G_H   = XP * G_H
   ETA_V = XP * ETA_V
 END IF
 FACV = RMUD(SXLYR) * SL / (RMUD(RXLYR) * SM)
 KER(K,1) = FACV * (XI_V - ETA_V + GAM * XPDIR) * LMBDA   !  Local Y component
 KER(K,2) = (XI_V + ETA_V + XPDIR) * LMBDA2 / S(SXLYR)    !  Vertical component
 KER(K,3) = F_H + G_H + FACV * (ETA_V - XI_V)

!  Accumulate Hankel transform integrals & check convergence

 FW(1)   = WJ0(L) * KER(K,1)
 FW(2:3) = WJ1(L) * KER(K,2:3)
 HLYRD(JR,1:NKR) = HLYRD(JR,1:NKR) + FW(1:NKR)

 JUMP = .TRUE.
 DO JINT = 1,NKR
   QR = ABS (REAL  (HLYRD(JR,JINT), KIND=QL) )
   QI = ABS (AIMAG (HLYRD(JR,JINT) ) )
   IF (QR > TOL2 .AND. ABS (REAL  (FW(JINT))) > TOL * QR) JUMP = .FALSE.
   IF (QI > TOL2 .AND. ABS (AIMAG (FW(JINT))) > TOL * QI) JUMP = .FALSE.
 END DO

END SUBROUTINE HSGWB_KER

 SUBROUTINE HSLPE (NFRQ,FREQ,SOURCE_TYPE,NTX,JS,SXLYR,NVRL,SXNL,SXEL,ZS,MRXTX,NRXTX,MQVR,RXID,XRXTX, &
                   YRXTX,ZRXTX,MXRHO,RHOTRP,NLYR,THKD,DPTHL,RES,RMUD,REPS,CHRG,CTAU,CFREQ,BFD)
!--------------------------------------------------------------------------------------------------

!***  Called by HSBOSS
!***  Calls COLRES_1D, HSCLE_HNK, HSGWE_HNK, CUBSPL, CDCUBVAL

!  Computes the three component, frequency-domain layered earth ELECTRIC FIELD
!  at any depth in any layer for a flat-lying loop source or grounded wire source.
!  Closed Loop can be in air or at any depth in any layer
!  Open loop must have electrodes below earth surface.
!  Valid for any number of layers

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!  SIGN CONVENTION:
!  ----------------
!  The normal layered earth field coordinate system used in this
!  subroutine has X (JC=1) positive north, Y (JC=2) positive east
!  and Z (JC=3) positive down.
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

!                               Input
!                               -----
!         FREQ - array of NFRQ frequencies
!  SOURCE_TYPE - 1 => closed loop;  2 => open loop (grounded wire)
!          NTX - number of transmitters
!        SXLYR - layer containing transmitter JS
!         NVRL - number of vertices for loop JS
!         SXNL - north coordinates for vertices for loop JS
!         SXEL - east coordinates for vertices for loop JS
!           ZS - depth from surface for flat lying loop JS
!        MRXTX - maximum number of receivers per transmitter
!     NRXTX(I) - number of receivers for transmitter I
!         MQVR - maximum number of vertices for all receivers
!    RXID(I,J) - RX_TYPE of receiver I for transmitter J. 1 => MD; 2 => ED
! XRXTX(J,I,K) - north coordinate of the Kth vertex of the Jth receiver of transmitter I
! YRXTX(J,I,K) - east coordinate of the Kth vertex of the Jth receiver of transmitter I
!   ZRXTX(J,I) - depth of the Jth receiver of transmitter I
!       RHOTRP - MXRHO size interpolation array for horizontal distances
!         NLYR - number of layers
!         THKD - layer thicknesses
!        DPTHL - depth to top of layer
!          RES - layer resistivities
!         RMUD - mu(i) / mu(0)   (i = 0:NLYR)
!         REPS - array of relative dislectric constants
!         CHRG - C-C chargeability
!         CTAU - C-C time constant in seconds
!        CFREQ - C-C frequency constant
!
!                             Output
!                             ------
!  BFD(JF,JR,JS,1:3) = the layered earth voltage for frequency JF, receiver JR, station JS.

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18), NDIP0=5, MXDIP=100
 REAL, PARAMETER :: DIPL0 = 5.
 COMPLEX (KIND=QL), PARAMETER :: ZERO=(0._QL, 0._QL)
 INTEGER NTX,MRXTX,NRXTX(NTX),MQVR,RXID(MRXTX,NTX),SXLYR,RXLYR,NFRQ,MXRHO,SOURCE_TYPE,NVRL,NVRLS,NLYR, &
         J1,JF,JS,JR,JV,JV1,JD,NDIP(NVRL),KFG,I1
 REAL FREQ(NFRQ),FRQ,ZRXTX(MRXTX,NTX),RXVRT,RHOTRP(MXRHO),SXNL(NVRL),SXEL(NVRL),X1,XWRX(NVRL,2,MRXTX),YWRX(NVRL,MRXTX)
 REAL, DIMENSION(MRXTX,NTX,MQVR) :: XRXTX,YRXTX
 REAL, DIMENSION(NLYR) :: RES,REPS,CTAU,CFREQ,CHRG
 REAL, DIMENSION(NVRL) :: CPHI,SPHI,WYRL,DIPL
 REAL, DIMENSION(MXDIP,NVRL,MRXTX) :: XRXD,RXRHO
 REAL, DIMENSION(4,MXRHO) :: HL1R,HL2R,HL3R,HL1I,HL2I,HL3I
 REAL, DIMENSION(MRXTX) :: XR,YR
 REAL(KIND=QL) ZR,ZPRV,ZS,RMUD(0:NLYR),THKD(NLYR),DPTHL(NLYR),YD,X1D,X2D,RHO1,RHO2,DIPLD,CPHID,SPHID
 COMPLEX HLYR(MXRHO,3),BFD(NFRQ,MRXTX,NTX,3)
 COMPLEX(KIND=QL) CDS1,CDS2,EX1,EY1,EX2,EY2,EZ2,KSQL(NLYR),SIGL(NLYR)

!  The fields have two parts, a non-conservative (path-dependent) integration over all closed
!  segments and a conservative (path-independent) part which is integrated on a direct path
!  between electrodes.  Obviously this gives a null contribution for closed loops.

!  All parts have the common multiplier 1 / (4 * PI * SIGL (RXLYR))

!  Loop contributions are evaluated segment by segment (open or closed) in an initial
!  coordinate system whose X component lies in the direction of the segment.  The local
!  field copmponents are rotated back into the survey soordinate system.
!
!  There is no vertical E field for a flat closed loop but there is one for a flat open loop.
!  The vertical E field for the open loop will be computed if the value of the integer parameter

!  Voltage is computed as uniform integration of electric field over receiver length.

 XR = 0.;  YR = 0.

 DO JR = 1, NRXTX(JS)                ! Loop over receivers
   IF (RXID(JR,JS) /= 3 ) CYCLE       ! This subroutine is only used for the point electric field.
   XR(JR) = XRXTX(JR,JS,1)
   YR(JR) = YRXTX(JR,JS,1)
 END DO

!  The initial computations occur in a coordinate system whose X component
!  lies in the direction of transmitter tilt.  In other words we rotate
!  the system by SXAZM.
!  For loop sources, the Gzx potential contributions are integrated around the loop.
!  For grouned wire sources, the Fzx potential is path independent so it is
!  integrated across the open side instead of around all the wire segments.

 NVRLS = NVRL
 IF (SOURCE_TYPE == 2) NVRLS = NVRL -1

 DO JV = 1, NVRL
   JV1 = JV + 1             ! Wire JV goes from vertex JV to vertex JV1
   IF (JV == NVRL) JV1 = 1

! Length & orientation of Wire JV between vertices JV & JV1

   WYRL(JV) = SQRT ( (SXNL(JV) - SXNL(JV1))**2 + (SXEL(JV) - SXEL(JV1))**2)
   CPHI(JV) = (SXNL(JV1) - SXNL(JV) ) / WYRL(JV)
   SPHI(JV) = (SXEL(JV1) - SXEL(JV) ) / WYRL(JV)
   IF (SOURCE_TYPE == 2 .AND. JV == NVRL) THEN
     CPHI(NVRL) = (SXNL(NVRL) - SXNL(1) ) / WYRL(NVRL)
     SPHI(NVRL) = (SXEL(NVRL) - SXEL(1) ) / WYRL(NVRL)
   END IF

! Divide each source wire into segment lengths of 5 m with a minimum of 5 segments per wire.

   NDIP(JV) = CEILING (WYRL(JV) / DIPL0)  ! 5 m initial dipole length
   NDIP(JV) = MAX (NDIP(JV), NDIP0)       ! At least 5 dipoles per segment
   NDIP(JV) = MIN (NDIP(JV), MXDIP)
   DIPL(JV) = WYRL(JV) / REAL (NDIP(JV))

! Distances from both vertices of wire segment JV to each point of
! electric dipole receiver  (Y = constant in new system)

   DO JR = 1, NRXTX(JS)              ! Loop over receivers
     IF (RXID(JR,JS) /= 3) CYCLE     ! This subroutine is only used for electric dipole receivers

     XWRX(JV,1,JR) = (XR(JR) - SXNL(JV))  * CPHI(JV) + (YR(JR) - SXEL(JV))  * SPHI(JV)
     XWRX(JV,2,JR) = (XR(JR) - SXNL(JV1)) * CPHI(JV) + (YR(JR) - SXEL(JV1)) * SPHI(JV)
     YWRX(JV,JR) =  -(XR(JR) - SXNL(JV)) * SPHI(JV) + (YR(JR) - SXEL(JV))  * CPHI(JV)
     IF (SOURCE_TYPE == 2 .AND. JV == NVRL) THEN
       XWRX(NVRL,1,JR) = (XR(JR) - SXNL(1))    * CPHI(NVRL) + (YR(JR) - SXEL(1))    * SPHI(NVRL)
       XWRX(NVRL,2,JR) = (XR(JR) - SXNL(NVRL)) * CPHI(NVRL) + (YR(JR) - SXEL(NVRL)) * SPHI(NVRL)
       YWRX(NVRL,JR) =  -(XR(JR) - SXNL(1))    * SPHI(NVRL) + (YR(JR) - SXEL(1))    * CPHI(NVRL)
     END IF

!  Compute distamces from each dipole centre to each receiver point in coordinate system
!  where wire runs along the new X (north) axis.

     DO JD = 1,NDIP(JV)
       X1 = (JD - 0.5) * DIPL(JV)
       XRXD(JD,JV,JR) =  XWRX(JV,1,JR) - X1
       RXRHO(JD,JV,JR) = SQRT (XRXD(JD,JV,JR)**2 + YWRX(JV,JR)**2)
     END DO
   END DO
 END DO

 DO JF = 1,NFRQ
   FRQ = FREQ(JF)
   CALL COLRES_1D (FRQ,NLYR,RES,REPS,RMUD,CHRG,CTAU,CFREQ,SIGL,KSQL)

   ZPRV = -9.D4
   I1 = 2                              ! electric source
   DO JR = 1, NRXTX(JS)                ! Loop over receivers
     IF (RXID(JR,JS) /= 3) CYCLE       ! This subroutine is only used for electric dipole receivers
     ZR = REAL (ZRXTX(JR,JS),QL)
     ZR = MAX (0.001D0,ZR)               ! Electrodes must be in contact with earth
     RXVRT = REAL (ABS (ZPRV - ZR))
     IF (RXVRT > .01_QL) THEN
      HLYR = (0.,0.)
       ZPRV = ZR
       RXLYR = 1
       DO J1 = NLYR,2,-1
         IF (ZR > DPTHL(J1)) THEN
           RXLYR = J1
           EXIT
         END IF
       END DO
       CALL SET_KFG (I1,NLYR,SXLYR,RXLYR,KFG)

!  Interpolation array for Hankel integrals

       CALL HSLPE_HNK (MXRHO,RHOTRP,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,KFG,SXLYR,RXLYR,HLYR)

       HL1R(1,1:MXRHO) = REAL (HLYR(1:MXRHO,1))
       CALL CUBSPL (RHOTRP,HL1R,MXRHO)
       HL1I(1,1:MXRHO) = AIMAG (HLYR(1:MXRHO,1))
       CALL CUBSPL (RHOTRP,HL1I,MXRHO)

       IF (SOURCE_TYPE == 2) THEN                      ! Include conservative terms for open loop source.
         HL2R(1,1:MXRHO) = REAL (HLYR(1:MXRHO,2))
         CALL CUBSPL (RHOTRP,HL2R,MXRHO)
         HL2I(1,1:MXRHO) = AIMAG (HLYR(1:MXRHO,2))
         CALL CUBSPL (RHOTRP,HL2I,MXRHO)

         HL3R(1,1:MXRHO) = REAL (HLYR(1:MXRHO,3))
         CALL CUBSPL (RHOTRP,HL3R,MXRHO)
         HL3I(1,1:MXRHO) = AIMAG (HLYR(1:MXRHO,3))
         CALL CUBSPL (RHOTRP,HL3I,MXRHO)
       END IF
     END IF

     EX2 = ZERO;  EY2 = ZERO
     DO JV = 1,NVRLS                ! Sum the contribution for each Tx loop segment
       YD = REAL (YWRX(JV,JR),KIND=QL)
       DIPLD = REAL (DIPL(JV),KIND=QL)
       CPHID = REAL (CPHI(JV),KIND=QL)
       SPHID = REAL (SPHI(JV),KIND=QL)
       EX1 = ZERO
       DO JD = 1, NDIP(JV)          !  Integrate over Tx segment
         X1D = REAL (XRXD(JD,JV,JR), KIND=QL)
         RHO1 = REAL (RXRHO(JD,JV,JR), KIND=QL)
         CALL CDCUBVAL (RHOTRP,HL1R,HL1I,MXRHO,RHO1,CDS1)
         EX1 = EX1 + CDS1
       END DO
       EX1 = DIPLD * EX1       ! Ex for Tx loop segment JV in rotated system

! Rotate these back to the survey system & add to
! sum from previous wire segnment.

       EX2 = EX2 + EX1 * CPHID  ! Fields in survey system
       EY2 = EY2 + EX1 * SPHID
     END DO                                !  Next wire segment

     BFD(JF,JR,JS,1) =  CMPLX (EX2)
     BFD(JF,JR,JS,2) =  CMPLX (EY2)
     BFD(JF,JR,JS,3) =  (0.,0.)
   END DO                                      ! next integration point


   IF (SOURCE_TYPE == 2) THEN       ! Include conservative terms for open segment.
     DO JR = 1, NRXTX(JS)               !  Sum over dipole receivers
       IF (RXID(JR,JS) /= 3) CYCLE     ! This subroutine is only used for electric dipole receivers
       YD = REAL (YWRX(NVRL,JR),KIND=QL)
       X1D = REAL (XWRX(NVRL,1,JR), KIND=QL)   ! for open loop segment.
       X2D = REAL (XWRX(NVRL,2,JR), KIND=QL)
       RHO1 = SQRT (X1D**2 + YD**2)
       RHO2 = SQRT (X2D**2 + YD**2)
       CALL CDCUBVAL (RHOTRP,HL2R,HL2I,MXRHO,RHO1,CDS1)
       CALL CDCUBVAL (RHOTRP,HL2R,HL2I,MXRHO,RHO2,CDS2)
       CPHID = REAL (CPHI(NVRL),KIND=QL)
       SPHID = REAL (SPHI(NVRL),KIND=QL)

       EX1 = X2D * CDS2 - X1D * CDS1    ! Fields in rotated system
       EY1 = YD * (CDS2 - CDS1)
       EX2 = EX1 * CPHID - EY1 * SPHID                    ! Fields in survey system
       EY2 = EY1 * CPHID + EX1 * SPHID

       CALL CDCUBVAL (RHOTRP,HL3R,HL3I,MXRHO,RHO1,CDS1)
       CALL CDCUBVAL (RHOTRP,HL3R,HL3I,MXRHO,RHO2,CDS2)
       EZ2 = CDS2 - CDS1

       BFD(JF,JR,JS,1) = BFD(JF,JR,JS,1) + CMPLX (EX2)  ! add charge & induction contributions
       BFD(JF,JR,JS,2) = BFD(JF,JR,JS,2) + CMPLX (EY2)
       BFD(JF,JR,JS,3) = BFD(JF,JR,JS,3) + CMPLX (EZ2)
     END DO
   END IF
 END DO     !  Next frequency

 END SUBROUTINE HSLPE

 SUBROUTINE HSLPE_HNK (MXRHO,RHOTRP,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,KFG,SXLYR,RXLYR,HLYR)
!--------------------------------------------------------------------------------------------

!***  Calls HS_JMP, HSLPE_KER
!***  Called by HSLPE

!  Uses flow through Hankel transform to compute electric dipole response integrals
!  HLYR(MXRHO,1:3) used to compute the closed and openloop frequency-domain electric
!  field components at the RX from pseudo X oriented  HED source.  The Hankel transform
!  uses a 15 points per decade filter coefficient set derived from Christensen's
!  FLTGEN program.

!          Input
!          -----
!    MXRHO - number of logrithmically horizontal distances
!  RHOTRP - array of MXRHO logrithmically spaced (15 / decade) horizontal distances
!           at same spacing as lambda values for Hankel transform.
!       L - filter index
!      ZS - depth of flat-lying loop
!      ZR - receiver depth
!    NLYR - number of layers
!    THKD - layer thicknesses
!   DPTHL - depth to top of layers (DPTHL = depth to basement
!    RMUD - mu(i) / mu(0)
!    SIGL - complex layer conductivities
!    KSQL - complex layer propagation constants
!     KFG - Tx-Rx indicator for coefficient generation
!   SXLYR - layer containing transmitter
!   RXLYR - layer containing receiver

!    OUTPUT is HLYR(1:MXRHO,1:3)

 USE BG_Filter_coefficients

 IMPLICIT NONE
 REAL(KIND=QL), PARAMETER :: FOURPI = 12.56637061_QL
 INTEGER KFG,GAM,MXRHO,NLYR,RXLYR,SXLYR,K,L,JR,K1,KBOT,KMAX,KMIN,LMAX,LMIN
 REAL RHOTRP(MXRHO)
 REAL(KIND=QL) ZS,ZR,DELTA,Y,Y1,LMBDA,RMUD(0:NLYR),RHOD
 REAL(KIND=QL), DIMENSION (NLYR) :: THKD,DPTHL
 COMPLEX HLYR(MXRHO,3)
 COMPLEX(KIND=QL) HLYRD(MXRHO,3),KER(JNLO-MXRHO:JNHI,3)
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL
 LOGICAL JUMP

 DELTA = LOG (10.D0)/ DBLE (NDEC_JN)
 HLYRD = (0._QL, 0._QL)
 KER = (0._QL, 0._QL)
 GAM = 0
 IF (RXLYR == SXLYR) THEN       !  Set whole space term
   IF (ZR > ZS) GAM = -1
   IF (ZR < ZS) GAM =  1
   IF (ABS (ZR - ZS) < 0.01_QL) GAM = 0
 END IF

!  Set up KER for JR = 1 corresponding to minimum value of RHO.  This will
!  compute most of the needed kernel range from the high end.  Note that
!  the filter is defined between JNLO < L < JNHI

 JR = 1
 RHOD = REAL (RHOTRP(1),KIND=QL)
 Y1 = -LOG (RHOD) - SHFTJN

 DO L = -50, JNHI             ! Start at L = -50 to pick up low values.
   LMAX = L                   ! Maximum filter index used
   K = L + 1 - JR             ! K is the kernel index.
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
   CALL HSLPE_KER (MXRHO,K,JR,L,LMBDA,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,KFG,GAM,SXLYR,RXLYR,KER,HLYRD,JUMP)
   IF (JUMP .AND. L > -40) EXIT
 END DO

 JUMP = .FALSE.           ! Finish off the low end for RHOTRP(1)
 DO L = -51, JNLO, -1
   LMIN = L                   ! Maximum filter index used                            r4cs
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
   K = L + 1 - JR             ! Compute the kernel index.
   CALL HSLPE_KER (MXRHO,K,JR,L,LMBDA,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,KFG,GAM,SXLYR,RXLYR,KER,HLYRD,JUMP)
   IF (JUMP .AND. L < -60) EXIT
 END DO

 KMIN = LMIN + 1 - JR  !  Define the range of kernel values
 KMAX = LMAX + 1 - JR  !  used for RHOTRP(1)

! Complete definition of kernel values by evaluating transform of
! maximum RHO = RHOTRP (MXRHO)

 JR = MXRHO
 RHOD = REAL (RHOTRP(MXRHO),KIND=QL)
 Y1 = -LOG (RHOD) - SHFTJN
 KBOT = JNLO + 1 - JR
 K1 = MAX (KBOT,KMIN)

 DO K = K1, KMAX          ! Compute EHR for maximum RHO using previously
   L = K - 1 + JR         ! computed kernel values.
   HLYRD(JR,1) = HLYRD(JR,1) + KER(K,1) * WJ0(L)
   HLYRD(JR,2) = HLYRD(JR,2) + KER(K,2) * WJ1(L)
   HLYRD(JR,3) = HLYRD(JR,3) + KER(K,3) * WJ0(L)
 END DO

 IF (K1 > KBOT) THEN    !  Add low end kernel values until convergence.
   DO K = K1-1, KBOT, -1
     KMIN = K
     L = K - 1 + JR
     Y = Y1 + DBLE (L) * DELTA
     LMBDA = EXP (Y)
     CALL HSLPE_KER (MXRHO,K,JR,L,LMBDA,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,KFG,GAM,SXLYR,RXLYR,KER,HLYRD,JUMP)
     IF (JUMP) EXIT
   END DO
 END IF

 DO JR = 2, MXRHO-1          !  Compute transforms for all other RHO values
   DO K = KMIN, KMAX         !  using previously computed kernel values.
     L = K - 1 + JR
     HLYRD(JR,1) = HLYRD(JR,1) + KER(K,1) * WJ0(L)
     HLYRD(JR,2) = HLYRD(JR,2) + KER(K,2) * WJ1(L)
     HLYRD(JR,3) = HLYRD(JR,3) + KER(K,3) * WJ0(L)
   END DO
 END DO

 DO JR = 1,MXRHO
   RHOD = REAL (RHOTRP(JR),KIND=QL)
   HLYRD(JR,1:3) =  HLYRD(JR,1:3) / (FOURPI * SIGL(RXLYR) * RHOD)
   HLYRD(JR,2) = HLYRD(JR,2) / RHOD
 END DO
 HLYR = CMPLX (HLYRD)

 END SUBROUTINE HSLPE_HNK

 SUBROUTINE HSLPE_KER (MXRHO,K,JR,L,LMBDA,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                       KFG,GAM,SXLYR,RXLYR,KER,HLYRD,JUMP)
!----------------------------------------------------------------------------

!***  Called by HSLPE__HNK
!***  Calls EDSX_COEF

!  Kernels and magnetic dipole response integrals for response from a closed or grounded
!  loop transmitter and a surface or downhole horizontal electric dipole receiver.
!
!          Input
!          -----
!    MXRHO - number of logrithmically horizontal distances
!       K - kernel index
!      JR - RHO index
!       L - filter index
!   LMBDA = Hankel transform variable
!      ZS - depth of flat-lying loop
!      ZR - receiver depth
!    NLYR - number of layers
!    THKD - layer thicknesses
!   DPTHL - depth to top of layers (DPTHL = depth to basement
!    RMUD - mu(i) / mu(0)
!    SIGL - complex layer conductivities
!    KSQL - complex layer propagation constants
!     KFG - Tx-Rx indicator for coefficient generation
!   SXLYR - layer containing transmitter
!   RXLYR - layer containing receiver
!
!          Output for receiver in layer L
!          ------
!
!    KER(MXRHO,3) - kernel values for transform
!            HLYRD - dipole response integral
!             JUMP - logical convergence indicator

 USE BG_Filter_coefficients

 IMPLICIT NONE
 REAL(KIND=QL), PARAMETER :: TOL=1.D-6, TOL2=1.D-35
 INTEGER MXRHO,K,JR,L,NLYR,KFG,GAM,RXLYR,SXLYR,J1
 REAL(KIND=QL) LMBDA,ZR,ZS,RMUD(0:NLYR),QR,QI
 REAL(KIND=QL), DIMENSION (NLYR) :: THKD,DPTHL
 COMPLEX(KIND=QL) S(0:NLYR),SL,XI_V,F_V,F_H,ETA_V,G_V,G_H,XP,XPDIR,FACV,KV,KS,FW(3), &
                  KER(JNLO-MXRHO:JNHI,3),HLYRD(MXRHO,3)
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL
 LOGICAL JUMP

 CALL EDSX_COEF (KFG,RXLYR,SXLYR,LMBDA,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                 ZS,S,XI_V,F_V,F_H,ETA_V,G_V,G_H)
 SL = S(RXLYR)
 FACV = RMUD(SXLYR) * KSQL(RXLYR) / (RMUD(RXLYR) * S(SXLYR))

 IF (RXLYR < NLYR) THEN
   XP = EXP (SL * (ZR - DPTHL(RXLYR+1)))
   F_H  = XP * F_H
   XI_V = XP * XI_V
 END IF
 XP = EXP (SL * (DPTHL(RXLYR) - ZR))
 XPDIR = (0._QL, 0._QL)
 IF (SXLYR == RXLYR) XPDIR = EXP (-SL * ABS (ZR-ZS))

 G_H   = XP * G_H
 ETA_V = XP * ETA_V

 KV = FACV * (XI_V + ETA_V + XPDIR)
 KS = (F_H - G_H + XPDIR) * SL
 KER(K,1) = -KV * LMBDA
 KER(K,2) = KS - KV
 KER(K,3) = -(F_H + G_H + GAM * XPDIR) * LMBDA   ! Vertical component

!  Accumulate Hankel transform integrals & check convergence

 FW(1) = WJ0(L) * KER(K,1)
 FW(2) = WJ1(L) * KER(K,2)
 FW(3) = WJ0(L) * KER(K,3)

 HLYRD(JR,1:3) = HLYRD(JR,1:3) + FW(1:3)

 JUMP = .TRUE.
 DO J1 = 1,3
   QR = ABS (REAL  (HLYRD(JR,J1), KIND=QL) )
   QI = ABS (AIMAG (HLYRD(JR,J1) ) )
   IF (QR > TOL2 .AND. ABS (REAL  (FW(J1))) > TOL * QR) JUMP = .FALSE.
   IF (QI > TOL2 .AND. ABS (AIMAG (FW(J1))) > TOL * QI) JUMP = .FALSE.
 END DO

 END SUBROUTINE HSLPE_KER

 SUBROUTINE HSLPED (NFRQ,FREQ,SOURCE_TYPE,NTX,JS,SXLYR,NVRL,SXNL,SXEL,ZS,MRXTX,NRXTX,MQVR,RXID,XRXTX, &
                    YRXTX,ZRXTX,MXRHO,RHOTRP,NLYR,THKD,DPTHL,RES,RMUD,REPS,CHRG,CTAU,CFREQ,BFD)
!---------------------------------------------------------------------------------------------------

!***  Called by HSBOSS
!***  Calls COLRES_1D, HSCLE_HNK, HSGWE_HNK, CUBSPL, CDCUBVAL

!  Computes the frequency-domain layered earth voltage across a HORIZONTAL ELECTRIC DIPOLE
!  for a flat-lying loop or grounded wire source.
!  Closed Loop can be in air or at any depth in any layer
!  Open loop must have electrodes below earth surface.
!  Horizontal electric dipole receiver in at any depth in any layer
!  Valid for any number of layers

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!  SIGN CONVENTION:
!  ----------------
!  The normal layered earth field coordinate system used in this
!  subroutine has X (JC=1) positive north, Y (JC=2) positive east
!  and Z (JC=3) positive down.
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

!                               Input
!                               -----
!         FREQ - array of NFRQ frequencies
!  SOURCE_TYPE - 1 => closed loop;  2 => open loop (grounded wire)
!          NTX - number of transmitters
!        SXLYR - layer containing transmitter JS
!         NVRL - number of vertices for loop JS
!         SXNL - north coordinates for vertices for loop JS
!         SXEL - east coordinates for vertices for loop JS
!           ZS - depth from surface for flat lying loop JS
!        MRXTX - maximum number of receivers per transmitter
!     NRXTX(I) - number of receivers for transmitter I
!         MQVR - maximum number of vertices for all receivers
!    RXID(I,J) - RX_TYPE of receiver I for transmitter J. 1 => MD; 2 => ED
! XRXTX(J,I,K) - north coordinate of the Kth vertex of the Jth receiver of transmitter I
! YRXTX(J,I,K) - east coordinate of the Kth vertex of the Jth receiver of transmitter I
!   ZRXTX(J,I) - depth of the Jth receiver of transmitter I
!       RHOTRP - MXRHO size interpolation array for horizontal distances
!         NLYR - number of layers
!         THKD - layer thicknesses
!        DPTHL - depth to top of layer
!          RES - layer resistivities
!         RMUD - mu(i) / mu(0)   (i = 0:NLYR)
!         REPS - array of relative dislectric constants
!         CHRG - C-C chargeability
!         CTAU - C-C time constant in seconds
!        CFREQ - C-C frequency constant
!
!                             Output
!                             ------
!  BFD(JF,JR,JS,1)   - the layered earth voltage for frequency JF, receiver JR, station JS.
!  BFD(JF,JR,JS,2:3) = 0.

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18), NDIP0=5, MXDIP=100
 REAL, PARAMETER :: DIPL0 = 5.
 COMPLEX (KIND=QL), PARAMETER :: ZERO=(0._QL, 0._QL)
 INTEGER NTX,MRXTX,NRXTX(NTX),MQVR,RXID(MRXTX,NTX),SXLYR,RXLYR,NFRQ,MXRHO,SOURCE_TYPE,NVRL,NVRLS,NLYR, &
         J1,JF,JS,JR,JG,JV,JV1,JD,NDIP(NVRL),KFG,I1
 REAL FREQ(NFRQ),FRQ,ZRXTX(MRXTX,NTX),RXVRT,RHOTRP(MXRHO),SXNL(NVRL),SXEL(NVRL),X1,CSRX(MRXTX),SNRX(MRXTX), &
      WRXD(5,MRXTX),DELR,DELX,DELY,XWRX(NVRL,2,5,MRXTX),YWRX(NVRL,5,MRXTX)
 REAL, DIMENSION(MRXTX,NTX,MQVR) :: XRXTX,YRXTX
 REAL, DIMENSION(NLYR) :: RES,REPS,CTAU,CFREQ,CHRG
 REAL, DIMENSION(NVRL) :: CPHI,SPHI,WYRL,DIPL
 REAL, DIMENSION(MXDIP,NVRL,5,MRXTX) :: XRXD,RXRHO
 REAL, DIMENSION(4,MXRHO) :: HL1R,HL2R,HL1I,HL2I
 REAL, DIMENSION(5,MRXTX) :: XRXED,YRXED
 REAL(KIND=QL) ZR,ZPRV,ZS,RMUD(0:NLYR),THKD(NLYR),DPTHL(NLYR),YD,X1D,X2D,RHO1,RHO2,DIPLD,CPHID,SPHID
 COMPLEX HLYR(MXRHO,3),BFD(NFRQ,MRXTX,NTX,3)
 COMPLEX(KIND=QL) CDS1,CDS2,EX1,EY1,EX2,EY2,EXL,EYL,EDIP,KSQL(NLYR),SIGL(NLYR)

!  The fields have two parts, a non-conservative (path-dependent) integration over all closed
!  segments and a conservative (path-independent) part which is integrated on a direct path
!  between electrodes.  Obviously this gives a null contribution for closed loops.

!  All parts have the common multiplier 1 / (4 * PI * SIGL (RXLYR))

!  Loop contributions are evaluated segment by segment (open or closed) in an initial
!  coordinate system whose X component lies in the direction of the segment.  The local
!  field copmponents are rotated back into the survey soordinate system.
!  The voltage for the electric dipole receivers will be computed by integrating
!  the electric field between two electrodes using 5 point Simpson integration.
!
!  This version assumes flat receivers so no vertical E field is computed. (NKR = 2)
!  There is no vertical E field for a flat closed loop but there is one for a flat open loop.
!  The vertical E field for the open loop will be computed if the value of the integer parameter
!  NKR is changed from 2 to 3, but that also requires changing the integration path over the
!  segments to account for vertical extent.

!  Voltage is computed as uniform integration of electric field over receiver length.

 XRXED = 0.;  YRXED = 0.; WRXD = 0.

 DO JR = 1, NRXTX(JS)                ! Loop over receivers
   IF (RXID(JR,JS) /= 2) CYCLE       ! This subroutine is only used for electric dipole receivers
   XRXED(1,JR) = XRXTX(JR,JS,1)
   XRXED(5,JR) = XRXTX(JR,JS,2)
   YRXED(1,JR) = YRXTX(JR,JS,1)
   YRXED(5,JR) = YRXTX(JR,JS,2)

   DELX = (XRXED(5,JR) - XRXED(1,JR) ) / 4.
   DELY = (YRXED(5,JR) - YRXED(1,JR) ) / 4.
   DELR = SQRT (DELX**2 + DELY**2)                  ! Rx dipole length
   CSRX(JR) = DELX / DELR                           ! cos of angle Rx dipole makes w/ north
   SNRX(JR) = DELY / DELR

   DO JG = 2,4
     XRXED(JG,JR) = XRXED(JG-1,JR) + DELX
     YRXED(JG,JR) = YRXED(JG-1,JR) + DELY
   END DO

   WRXD(1,JR) = DELR /3.
   WRXD(2,JR) = 4. * WRXD(1,JR)
   WRXD(3,JR) = 2. * WRXD(1,JR)
   WRXD(4,JR) = WRXD(2,JR)
   WRXD(5,JR) = WRXD(1,JR)
 END DO

!  The initial computations occur in a coordinate system whose X component
!  lies in the direction of transmitter tilt.  In other words we rotate
!  the system by SXAZM.
!  For loop sources, the Gzx potential contributions are integrated around the loop.
!  For grouned wire sources, the Fzx potential is path independent so it is
!  integrated across the open side instead of around all the wire segments.

 NVRLS = NVRL
 IF (SOURCE_TYPE == 2) NVRLS = NVRL -1

 DO JV = 1, NVRL
   JV1 = JV + 1             ! Wire JV goes from vertex JV to vertex JV1
   IF (JV == NVRL) JV1 = 1

! Length & orientation of Wire JV between vertices JV & JV1

   WYRL(JV) = SQRT ( (SXNL(JV) - SXNL(JV1))**2 + (SXEL(JV) - SXEL(JV1))**2)
   CPHI(JV) = (SXNL(JV1) - SXNL(JV) ) / WYRL(JV)
   SPHI(JV) = (SXEL(JV1) - SXEL(JV) ) / WYRL(JV)
   IF (SOURCE_TYPE == 2 .AND. JV == NVRL) THEN
     CPHI(NVRL) = (SXNL(NVRL) - SXNL(1) ) / WYRL(NVRL)
     SPHI(NVRL) = (SXEL(NVRL) - SXEL(1) ) / WYRL(NVRL)
   END IF

! Divide each source wire into segment lengths of 5 m with a minimum of 5 segments per wire.

   NDIP(JV) = CEILING (WYRL(JV) / DIPL0)  ! 5 m initial dipole length
   NDIP(JV) = MAX (NDIP(JV), NDIP0)       ! At least 5 dipoles per segment
   NDIP(JV) = MIN (NDIP(JV), MXDIP)
   DIPL(JV) = WYRL(JV) / REAL (NDIP(JV))

! Distances from both vertices of wire segment JV to each point of
! electric dipole receiver  (Y = constant in new system)

   DO JR = 1, NRXTX(JS)              ! Loop over receivers
     IF (RXID(JR,JS) /= 2) CYCLE     ! This subroutine is only used for electric dipole receivers
     DO JG = 1,5                     ! Sum over Simpson integration points

       XWRX(JV,1,JG,JR) =  (XRXED(JG,JR) - SXNL(JV)) * CPHI(JV) + (YRXED(JG,JR) - SXEL(JV)) * SPHI(JV)
       XWRX(JV,2,JG,JR) =  (XRXED(JG,JR) - SXNL(JV1)) * CPHI(JV) + (YRXED(JG,JR) - SXEL(JV1)) * SPHI(JV)
       YWRX(JV,JG,JR) = -(XRXED(JG,JR) - SXNL(JV)) * SPHI(JV) + (YRXED(JG,JR) - SXEL(JV)) * CPHI(JV)
       IF (SOURCE_TYPE == 2 .AND. JV == NVRL) THEN
         XWRX(NVRL,1,JG,JR) = (XRXED(JG,JR) - SXNL(1))    * CPHI(NVRL) + (YRXED(JG,JR) - SXEL(1))    * SPHI(NVRL)
         XWRX(NVRL,2,JG,JR) = (XRXED(JG,JR) - SXNL(NVRL)) * CPHI(NVRL) + (YRXED(JG,JR) - SXEL(NVRL)) * SPHI(NVRL)
         YWRX(NVRL,JG,JR) =  -(XRXED(JG,JR) - SXNL(1))    * SPHI(NVRL) + (YRXED(JG,JR) - SXEL(1))    * CPHI(NVRL)
       END IF

!  Compute distamces from each dipole centre to each receiver point in coordinate system
!  where wire runs along the new X (north) axis.

       DO JD = 1,NDIP(JV)
         X1 = (JD - 0.5) * DIPL(JV)
         XRXD(JD,JV,JG,JR) =  XWRX(JV,1,JG,JR) - X1
         RXRHO(JD,JV,JG,JR) = SQRT (XRXD(JD,JV,JG,JR)**2 + YWRX(JV,JG,JR)**2)
       END DO
     END DO
   END DO
 END DO

 EDIP = ZERO
 DO JF = 1,NFRQ
   FRQ = FREQ(JF)
   CALL COLRES_1D (FRQ,NLYR,RES,REPS,RMUD,CHRG,CTAU,CFREQ,SIGL,KSQL)

   ZPRV = -9.D4
   I1 = 2                               ! electric source
   DO JR = 1, NRXTX(JS)                ! Loop over receivers
     IF (RXID(JR,JS) /= 2) CYCLE       ! This subroutine is only used for electric dipole receivers
     ZR = REAL (ZRXTX(JR,JS),QL)
     ZR = MAX (0.001D0,ZR)               ! Electrodes must be in contact with earth
     RXVRT = REAL (ABS (ZPRV - ZR))
     IF (RXVRT > .01_QL) THEN
      HLYR = (0.,0.)
       ZPRV = ZR
       RXLYR = 1
       DO J1 = NLYR,2,-1
         IF (ZR > DPTHL(J1)) THEN
           RXLYR = J1
           EXIT
         END IF
       END DO
       CALL SET_KFG (I1,NLYR,SXLYR,RXLYR,KFG)

!  Interpolation array for Hankel integrals

       CALL HSLPE_HNK (MXRHO,RHOTRP,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,KFG,SXLYR,RXLYR,HLYR)

       HL1R(1,1:MXRHO) = REAL (HLYR(1:MXRHO,1))
       CALL CUBSPL (RHOTRP,HL1R,MXRHO)
       HL1I(1,1:MXRHO) = AIMAG (HLYR(1:MXRHO,1))
       CALL CUBSPL (RHOTRP,HL1I,MXRHO)

       IF (SOURCE_TYPE == 2) THEN                      ! Include conservative terms for open loop source.
         HL2R(1,1:MXRHO) = REAL (HLYR(1:MXRHO,2))
         CALL CUBSPL (RHOTRP,HL2R,MXRHO)
         HL2I(1,1:MXRHO) = AIMAG (HLYR(1:MXRHO,2))
         CALL CUBSPL (RHOTRP,HL2I,MXRHO)
       END IF

!      HL3R(1,1:MXRHO) = REAL (HLYR(1:MXRHO,3))
!      CALL CUBSPL (RHOTRP,HL3R,MXRHO)
!      HL3I(1,1:MXRHO) = AIMAG (HLYR(1:MXRHO,3))
!      CALL CUBSPL (RHOTRP,HL3I,MXRHO)
     END IF

     EXL = ZERO;  EYL = ZERO
     DO JG = 1,5                      ! Integrate over the 5 receiver segments
       EX2 = ZERO;  EY2 = ZERO
       DO JV = 1,NVRLS                ! Sum the contribution for each Tx loop segment
         YD = REAL (YWRX(JV,JG,JR),KIND=QL)
         DIPLD = REAL (DIPL(JV),KIND=QL)
         CPHID = REAL (CPHI(JV),KIND=QL)
         SPHID = REAL (SPHI(JV),KIND=QL)
         EX1 = ZERO
         DO JD = 1, NDIP(JV)          !  Integrate over Tx segment
           X1D = REAL (XRXD(JD,JV,JG,JR), KIND=QL)
           RHO1 = REAL (RXRHO(JD,JV,JG,JR), KIND=QL)
           CALL CDCUBVAL (RHOTRP,HL1R,HL1I,MXRHO,RHO1,CDS1)
           EX1 = EX1 + CDS1
         END DO
         EX1 = DIPLD * EX1       ! Ex for Tx loop segment JV in rotated system

! Rotate these back to the survey system & add to
! sum from previous wire segnment.

         EX2 = EX2 + EX1 * CPHID  ! Fields in survey system
         EY2 = EY2 + EX1 * SPHID
       END DO                                !  Next wire segment

       EXL = EXL + WRXD(JG,JR) * EX2         ! Simpson integration over Rx dipole
       EYL = EYL + WRXD(JG,JR) * EY2
     END DO                                      ! next integration point
     EDIP = CSRX(JR) * EXL + SNRX(JR) * EYL    ! resolve components along Rx dipole
     BFD(JF,JR,JS,1) =  CMPLX (EDIP)
     BFD(JF,JR,JS,2:3) =  (0.,0.)
   END DO                                      ! next integration point

   IF (SOURCE_TYPE == 2) THEN       ! Include conservative terms for open segment.
     DO JR = 1, NRXTX(JS)               !  Sum over dipole receivers
       IF (RXID(JR,JS) /= 2) CYCLE     ! This subroutine is only used for electric dipole receivers
       EXL = ZERO;  EYL = ZERO
       DO JG = 1,5                  !  Sum over Simpson integration points
         YD = REAL (YWRX(NVRL,JG,JR),KIND=QL)
         X1D = REAL (XWRX(NVRL,1,JG,JR), KIND=QL)   ! for open loop segment.
         X2D = REAL (XWRX(NVRL,2,JG,JR), KIND=QL)
         RHO1 = SQRT (X1D**2 + YD**2)
         RHO2 = SQRT (X2D**2 + YD**2)
         CALL CDCUBVAL (RHOTRP,HL2R,HL2I,MXRHO,RHO1,CDS1)
         CALL CDCUBVAL (RHOTRP,HL2R,HL2I,MXRHO,RHO2,CDS2)
         CPHID = REAL (CPHI(NVRL),KIND=QL)
         SPHID = REAL (SPHI(NVRL),KIND=QL)

         EX1 = X2D * CDS2 - X1D * CDS1    ! Fields in rotated system
         EY1 = YD * (CDS2 - CDS1)
         EX2 = EX1 * CPHID - EY1 * SPHID                    ! Fields in survey system
         EY2 = EY1 * CPHID + EX1 * SPHID

         EXL = EXL + WRXD(JG,JR) * EX2                      ! Simpson integration
         EYL = EYL + WRXD(JG,JR) * EY2

!        CALL CDCUBVAL (RHOTRP,HL3R,HL3I,MXRHO,RHO1,CDS1)
!        CALL CDCUBVAL (RHOTRP,HL3R,HL3I,MXRHO,RHO2,CDS2)
!        EZL = EXL + WRXD(JG,JR) * (CDS2 - CDS1)

       END DO
       EDIP = CSRX(JR) * EXL + SNRX(JR) * EYL              ! resolve components along dipole
       BFD(JF,JR,JS,1) = BFD(JF,JR,JS,1) + CMPLX (EDIP)  ! add charge & induction contributions
     END DO
   END IF
 END DO     !  Next frequency

 END SUBROUTINE HSLPED

 SUBROUTINE HSMDB (SXLYR,RXLYR,NFRQ,FREQ,ZS,SXDP1,SXAZM1,ZR,XRX,YRX,NLYR,THKD,DPTHL, &
                   RES,RMUD,REPS,CHRG,CTAU,CFREQ,JS,JR,NTX,MRXTX,BFD)
!----------------------------------------------------------------------------------

!***  Called by HSBOSS
!***  Calls HSMDB_HNK

!  Computes the frequency-domain layered earth magnetic field in RXLYR due to a
!  magnetic dipole transmitter (unit area/moment) in SXLYR.
!  SXLYR & RXLYR can have any value from 0 to NLYR

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!  SIGN CONVENTION:
!  ----------------
!  The normal layered earth field coordinate system used in this
!  subroutine has X (JC=1) positive north, Y (JC=2) positive east
!  and Z (JC=3) positive down.
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

!                             Input
!                             -----
!        SXLYR - layer containing transmitter
!        RXLYR - layer containing receiver
!         FREQ - array of NFRQ frequencies
!           ZS - depth of dipole transmitter (negative above earth)
!        SXDP1 - dip of transmitter (radians)
!       SXAZM1 - azimuth of transmitter (radians)
!           ZR - receiver depth
!          XRX - north receiver offset from transmitter
!          YRX - east receiver offset from transmitter
!         NLYR - number of layers
!         THKD - layer thicknesses
!        DPTHL - depth to top of layer
!          RES - layer resistivities
!         RMUD - mu(i) / mu(0)   (i = 0:NLYR)
!         REPS - array of relative dislectric constants
!         CHRG - C-C chargeability
!         CTAU - C-C time constant in seconds
!        CFREQ - C-C frequency constant
!
!          NTX - number of transmitters
!        MRXTX - maximum number of receivers per transmitter
!
!                             Output
!                             ------
!  BFD(JF,JR,JS,I) - the Ith component of the layered earth response at
!                    frequency JF, receiver JR, transmitter JS.
!           I = 1,2,3 = north, east and vertical componsnts respectively

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 COMPLEX(KIND=QL), PARAMETER :: ZERO=(0._QL, 0._QL), ONE=(1._QL, 0._QL), TWO=(2._QL, 0._QL)
 INTEGER NFRQ,MRXTX,NTX,NLYR,KFG,RXLYR,SXLYR,JF,JS,JR,NINTG,I1
 REAL FREQ(NFRQ),FRQ,SXDP1,SXAZM1,XRX,YRX,RHO,XRP,YRP
 REAL, DIMENSION(NLYR) :: RES,REPS,CTAU,CFREQ,CHRG
 REAL(KIND=QL) ZS,ZR,RHOD,XRPD,YRPD,XBAR,YBAR,XBARSQ,CAZD,SAZD,CDPD,SDPD, &
               RMUD(0:NLYR),THKD(NLYR),DPTHL(NLYR)
 COMPLEX(KIND=QL) SIGL(NLYR),KSQL(NLYR),HLYR(6),BZZ,BXX,BZX,BXZ,BYX,BYZ,BX,BY,BZ,BZZD,BXXD, &
                  BZXD,BXZD,BYXD,BYZD,BFDD(NFRQ,MRXTX,NTX,3)
 COMPLEX BFD(NFRQ,MRXTX,NTX,3)

!  This routine will produce B for unit TX moment so VFAC0 = mu0 / (4 * PI)

!  The initial computations occur in a coordinate system whose X component
!  lies in the direction of transmitter tilt.  In other words we rotate
!  the system by SXAZM

 I1 = 1
 CALL SET_KFG (I1,NLYR,SXLYR,RXLYR,KFG)

 NINTG = 5
 IF (RXLYR > 0 .AND. SXLYR > 0) NINTG = 6

 HLYR = ZERO

 BZZD = ZERO
 BZXD = ZERO
 BXXD = ZERO
 BXZD = ZERO
 BYXD = ZERO
 BYZD = ZERO

 XRP = XRX * COS (SXAZM1) + YRX * SIN (SXAZM1)
 YRP = YRX * COS (SXAZM1) - XRX * SIN (SXAZM1)
 XRPD = REAL (XRP, KIND=QL)
 YRPD = REAL (YRP, KIND=QL)

 RHO = SQRT(XRX**2 + YRX**2)
 RHOD = REAL (RHO, KIND=QL)

 IF (RHOD > 1.D-2) THEN
   XBAR = XRPD / RHOD
   YBAR = YRPD / RHOD
 ELSE
   RHOD = 0.01_QL
   XBAR = 0._QL
   YBAR = 0._QL
 END IF
 XBARSQ = XBAR**2

 DO JF = 1,NFRQ
   FRQ = FREQ(JF)
   CALL COLRES_1D (FRQ,NLYR,RES,REPS,RMUD,CHRG,CTAU,CFREQ,SIGL,KSQL)

   IF (RXLYR == SXLYR) CALL B_DIRECT

   CALL HSMDB_HNK (KFG,SXLYR,RXLYR,NLYR,THKD,DPTHL,SIGL,KSQL,RMUD,ZS,ZR,RHOD,NINTG,HLYR)

   BZZ = BZZD + HLYR(1)
   BXZ = BXZD - XBAR * HLYR(2)
   BYZ = BYZD - YBAR * HLYR(2)
   BZX = BZXD - XBAR * HLYR(3)
   BXX = BXXD - (ONE - TWO* XBARSQ) * HLYR(4) - XBARSQ * HLYR(5)
   BYX = BYXD + XBAR*YBAR * (TWO * HLYR(4) - HLYR(5))

   IF (NINTG == 6) BXX = BXX - HLYR(6)  ! add HED terms

   CDPD = REAL (COS (SXDP1),KIND=QL)
   SDPD = REAL (SIN (SXDP1),KIND=QL)
   CAZD = REAL (COS (SXAZM1),KIND=QL)
   SAZD = REAL (SIN (SXAZM1),KIND=QL)

   BX = BXZ * CDPD + BXX * SDPD
   BY = BYZ * CDPD + BYX * SDPD
   BZ = BZZ * CDPD + BZX * SDPD

! Rotate these from the system where X points to the Tx azimuth
! to the North, East, Z system.  Retain 12 digit accuracy.

   BFDD(JF,JR,JS,1) = BX * CAZD - BY * SAZD
   BFDD(JF,JR,JS,2) = BY * CAZD + BX * SAZD
   BFDD(JF,JR,JS,3) = BZ

   BFD(JF,JR,JS,1:3) = CMPLX (BFDD(JF,JR,JS,1:3) ) ! Convert to standard precision
 END DO

  CONTAINS

   SUBROUTINE B_DIRECT
!  --------------------

! If both transmitter and receiver are in layer RXLYR, B_DIRECT computes the direct
! magnetic fields in a whole space with the geoelectric properties of layer RXLYR.

   IMPLICIT NONE
   COMPLEX(KIND=QL), PARAMETER :: THREE=(3._QL, 0._QL)
   REAL(KIND=QL) RSQ,RR,R3,XRD,YRD,ZRD
   COMPLEX(KIND=QL) Q,QSQ,FAC,B0,B1

   RSQ = RHOD**2 + (ZS - ZR)**2
   RR = SQRT (RSQ)
   R3 = RR * RSQ
   XRD = XRP / RR
   YRD = YRP / RR
   ZRD = (ZR - ZS) / RR
   IF (RXLYR == 0) THEN
     FAC = (1.0D-7, 0.D0) / (R3)
     BZZD = FAC * (THREE*ZRD**2 - ONE)
     BXZD = THREE * FAC * XRD * ZRD
     BYZD = THREE * FAC * YRD * ZRD
     BXXD = FAC * (THREE*XRD**2 - ONE)
     BZXD = THREE * FAC * ZRD * XRD
     BYXD = THREE * FAC * YRD * XRD
   ELSE
     QSQ = RSQ * KSQL(RXLYR)
     Q = SQRT (QSQ)
     FAC = 1.0D-7 * RMUD(SXLYR) * EXP (-Q) / (R3)
     B0 = FAC * (ONE + Q + QSQ)
     B1 = FAC * (3.*(ONE + Q) + QSQ)
     BZZD = B1 * ZRD**2  - B0
     BXXD = B1 * XRD**2  - B0
     BZXD = B1 * XRD * ZRD
     BYXD = B1 * YRD * XRD
     BXZD = BZXD
     BYZD = B1 * YRD * ZRD
   END IF
   END SUBROUTINE B_DIRECT

 END SUBROUTINE HSMDB

 SUBROUTINE HSMDB_HNK (KFG,SXLYR,RXLYR,NLYR,THKD,DPTHL,SIGL,KSQL,RMUD,ZS,ZR,RHOD,NINTG,HLYR)
!-------------------------------------------------------------------------------------------

!***  Calls HSMDB_KER, HSMD0B_KER, HSMDNB_KER
!***  Called by HSMDB

!  Computes transform integrals HLYR (1:NINTG) which are used to compute vertical
!  and horizontal frequency-domain magnetic field components at the RX from
!  VMD and HMD sources.  It evaluates the Hankel transform integral using a
!  15 points per decade filter coefficient set derived from Christensen's
!  FLTGEN program.

!    RHOD - horizontal TX -> RX distance.
!     KER - stores kernel values from HSMDB_KER

!  NLYR,SIGL,KSQL,RMUD,RXLYR,SXLYR,ZS,ZR
!  are described in HSMDB
!
!    OUTPUT is HLYR(1:NINTG)  forward model components

 USE BG_Filter_coefficients

 IMPLICIT NONE
 REAL(KIND=QL), PARAMETER :: VFAC0=1.0D-7
 INTEGER NLYR,KFG,RXLYR,SXLYR,NINTG,L
 REAL(KIND=QL) RMUD(0:NLYR),THKD(NLYR),DPTHL(NLYR),DEL_JN,RHO_JN,Y,LMBDA,RHOD,ZS,ZR
 COMPLEX(KIND=QL) SIGL(NLYR),KSQL(NLYR),HLYR(NINTG)
 LOGICAL JUMP

 DEL_JN = LOG (10.D0)/ DBLE (NDEC_JN)
 RHO_JN = -LOG (RHOD) - SHFTJN

 HLYR = (0._QL, 0._QL)
 DO L = -50, JNHI             ! Start at L = -50 to pick up low values.
   Y = RHO_JN + DBLE(L) * DEL_JN
   LMBDA = EXP (Y)
   CALL HSMDB_KER (L,LMBDA,NLYR,THKD,DPTHL,SIGL,KSQL,RMUD,KFG,RXLYR,SXLYR,ZS,ZR,RHOD,NINTG,HLYR,JUMP)
   IF (JUMP .AND. L > -40) EXIT
 END DO

 JUMP = .FALSE.           ! Finish off the low end for RHOTRP(1)
 DO L = -51, JNLO, -1
   Y = RHO_JN + DBLE (L) * DEL_JN
   LMBDA = EXP (Y)
   CALL HSMDB_KER (L,LMBDA,NLYR,THKD,DPTHL,SIGL,KSQL,RMUD,KFG,RXLYR,SXLYR,ZS,ZR,RHOD,NINTG,HLYR,JUMP)
   IF (JUMP .AND. L < -60) EXIT
 END DO

 HLYR = VFAC0 * HLYR / RHOD

 END SUBROUTINE HSMDB_HNK

 SUBROUTINE HSMDB_KER (L,LMBDA,NLYR,THKD,DPTHL,SIGL,KSQL,RMUD,KFG,RXLYR,SXLYR,ZS,ZR,RHOD,NINTG,HLYR,JUMP)
!--------------------------------------------------------------------------------------------------------

!***  Called by HSMDB_HNK
!***  Calls PROPAGATE

!  Computes the frequency-domain layered-earth response kernel QFD for subsurface magnetic
!  dipole transmitters above basement for magnetic dipole receivers in any layer.
!________________________________________________________________________________
!  IMPORTANT NOTE: DPTHL(J) is the depth from the surface to the TOP of layer J.
!---------------------------------------------------------------------------------
!          Input
!          -----
!       L - Hankel filter index
!   LMBDA = Hankel transform variable
!    NLYR - number of layers
!     RES - layer resistivities
!    REPS - array of relative dielectric constants
!    RMUD - mu(i) / mu(0)
!    THKD - thickness of layer J
!   DPTHL - depth to TOP of layer J
!   RXLYR - layer containing receiver
!   SXLYR - layer containing transmitter
!      ZS - depth of dipole transmitter (negative above earth)
!      ZR - receiver depth relative to surface
!    RHOD - horizontal receiver - transmitter separation
!   NINTG - number of integrals
!
!          Output for receiver in layer i
!          ------
!  HLYR(1:NINTG) updated frequency-Hankel domain integrals for either frequency or
!  time-domain responses

 USE BG_Filter_coefficients
 IMPLICIT NONE
 REAL(KIND=QL), PARAMETER :: TOL=1.D-6, TOL2=1.D-35
 COMPLEX(KIND=QL), PARAMETER :: ZERO=(0._QL,0._QL)
 INTEGER L,NLYR,KFG,RXLYR,SXLYR,J,NINTG
 REAL(KIND=QL) RMUD(0:NLYR),LMBDA,LMBDA2,LMBDA3,ZS,ZR,RHOD,QR,QI,RM,RL
 REAL(KIND=QL), DIMENSION (NLYR) :: THKD,DPTHL
 COMPLEX(KIND=QL) XI_V,XI_H,F_V,ETA_V,ETA_H,G_V,QFD(5),XP1,SL,SM,KSQL(NLYR), &
                  SIGL(NLYR),S(0:NLYR),FW(NINTG),HLYR(NINTG)
 LOGICAL JUMP

 CALL MDSX_COEF (KFG,RXLYR,SXLYR,LMBDA,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                 ZS,S,XI_V,XI_H,F_V,ETA_V,ETA_H,G_V)

 SL = S(RXLYR)
 SM = S(SXLYR)
 RL = RMUD(RXLYR)
 RM = RMUD(SXLYR)
 LMBDA2 = LMBDA**2
 LMBDA3 = LMBDA**3
 XP1 = ZERO
 IF (RXLYR < NLYR) THEN
   XP1 = EXP (SL * (ZR - DPTHL(RXLYR+1)))
   F_V  = XP1 * F_V
   XI_V = XP1 * XI_V
   XI_H = XP1 * XI_H
 END IF
 IF (RXLYR > 0) THEN
   XP1 = EXP (SL * (DPTHL(RXLYR) - ZR))
   G_V   = XP1 * G_V
   ETA_V = XP1 * ETA_V
   ETA_H = XP1 * ETA_H
 END IF

!  Accumulate Hankel transform integrals & check convergence

 QFD(1) = RM * (XI_V + ETA_V) * LMBDA3 / SM          ! Bzz
 QFD(2) = RM * (XI_V - ETA_V) * LMBDA2 * SL / SM     ! Bxz and Byz
 QFD(3) = RM * (XI_H + ETA_H) * LMBDA2               ! Bzx and Bzy
 QFD(4) = RM * (XI_H - ETA_H) * SL
 FW(1) = WJ0(L) * QFD(1)
 FW(2) = WJ1(L) * QFD(2)
 FW(3) = WJ1(L) * QFD(3)

 IF (NINTG == 6) THEN                       ! F potential terms for Bxx, Byy, and Bxy
   QFD(5) = RL * (F_V + G_V) * KSQL(SXLYR) / SM
   QFD(4) = QFD(4) - QFD(5)
   FW(6) = WJ0(L) * QFD(5) * LMBDA
 END IF
 FW(4) = WJ1(L) * QFD(4) / RHOD
 FW(5) = WJ0(L) * QFD(4) * LMBDA

 HLYR(1:NINTG) = HLYR(1:NINTG) + FW(1:NINTG)

 JUMP = .TRUE.
 DO J = 1,NINTG
   QR = ABS (REAL  (HLYR(J) ) )
   QI = ABS (AIMAG (HLYR(J) ) )
   IF (QR > TOL2 .AND. ABS (REAL  (FW(J))) > TOL * QR) JUMP = .FALSE.
   IF (QI > TOL2 .AND. ABS (AIMAG (FW(J))) > TOL * QI) JUMP = .FALSE.
 END DO

 END SUBROUTINE HSMDB_KER

 SUBROUTINE EDSX_COEF (KFG,RXLYR,SXLYR,LMBDA,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                       ZS,S,XI_V,F_V,F_H,ETA_V,G_V,G_H)
!---------------------------------------------------------------------------

!***  Called by
!***  Calls PROPAGATE

!  For an arbitrarily oriented electric dipole source at ZS in layer SXLYR of a
!  halfspace with NLYR-1 layers above basement, EDSX_COEF computes the coefficients
!  for the vertical and horizontal F (electric) and G (magnetic) potentials
!  These coefficients can be used to compute magnetic and electric fields.
!
!          Input
!          -----
!  KFG indicates source type and layer locations of source and receiver: air, basement or intermediate
!
!  KFG = IJK : I = type; J = Tx layer; K = Rx layer
!
!        I = 2 => electric dipole source
!
!        J = 0 => Transmitter in air
!        J = 3 => Transmitter in basement
!
!        K = 0 => Receiver in air
!        K = 3 => Receiver in basement
!
!        When BOTH tranamitter nor receiver are in an intermediate layer:
!
!        JK = 11 => Transmitter and recever are in the same layer
!        JK = 12 => Transmitter layer is above receiver layer
!        JK = 21 => Transmitter layer is below receiver layer
!
!   RXLYR - layer containing receiver
!   SXLYR - layer containing transmitter
!   LMBDA - Hankel transform variable
!    NLYR - number of layers
!    THKD - thickness of layer J
!   DPTHL - depth to TOP of layer J
!    RMUD - mu(i) / mu(0)
!    SIGL - complex conductivity including Cole-Cole + dimagnetic terms
!    KSQL - iwu * SIGL
!      ZS - depth of dipole transmitter (negative above earth)
!
!          Output
!          ------
!             S - Layer property variable in Hankel space
!   XI_V, ETA_V - coefficients for the vertical magnetic Schelkunoff potentials.
!
!   F_V,  G_V   - coefficients for the vertical electric Schelkunoff potentials.
!   F_H,  G_H   - coefficients for the horizontal electric Schelkunoff potential
!
!  P (F_V, F_H, XI_V)  are all used in the generic form:  P * EXP (S(RXLYR) * (ZR - DPTHL(RXLYR+1)))
!  Q (G_V, G_H, ETA_V) are all used in the generic form:  Q * EXP (S(RXLYR) * (DPTHL(RXLYR) - ZR))
!                               where ZR = receiver depth relative to surface

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 COMPLEX(KIND=QL), PARAMETER :: ONE=(1._QL,0._QL),ZERO=(0._QL,0._QL)
 INTEGER NLYR,KFG,RXLYR,SXLYR,J
 REAL(KIND=QL) RMUD(0:NLYR),LMBDA,ZS
 REAL(KIND=QL), DIMENSION (NLYR) ::  THKD,DPTHL,RMUSQ
 COMPLEX(KIND=QL) SM,SL,VLG,VMG,AMG,VLF,VMF,AMF,XPA,XP1,XP2,XQ1,XQ2,DENOMF,DENOMG, &
                  PRJG,PRJF,XI_V,F_V,F_H,ETA_V,G_V,G_H,XI_VBAR, &
                  F_VBAR,F_HBAR,ETA_VBAR,G_VBAR,G_HBAR,LMBSQ,VACHI
 COMPLEX(KIND=QL), DIMENSION (0:NLYR) :: S,T,R,AF,AG,VF,VG,CHI
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL

 S = ZERO; T = ZERO; R = ZERO; AF = ZERO; AG = ZERO; VF = ZERO; VG = ZERO; CHI = ONE
 F_V = ZERO;  F_H = ZERO;  XI_V  = ZERO;
 G_V = ZERO;  G_H = ZERO;  ETA_V = ZERO;


! VF & VG are the downward propagators for the F & G potentials respectively.
! AF & AG are the upward propagators.
! T & R are the magnetic and magnetic reflection coefficients


 LMBSQ = CMPLX (LMBDA * LMBDA, 0._QL, KIND=QL)

 S(0) = CMPLX (LMBDA, 0._QL,KIND=QL)
 DO J = 1,NLYR
   S(J) = SQRT (KSQL(J) + LMBSQ)
   RMUSQ(J) = RMUD(J) * RMUD(J)
 END DO
 T(0) = ( (RMUSQ(1) - 1._QL) * LMBSQ - KSQL(1) ) / ( RMUD(1)*S(0) + S(1) )**2
 R(0) = ONE
 DO J = 1,NLYR-1
   T(J) = (RMUSQ(J+1) - RMUSQ(J)) * LMBSQ + (RMUSQ(J+1) * KSQL(J) - RMUSQ(J) * KSQL(J+1)) &
        / (RMUD(J+1) * S(J) + RMUD(J) * S(J+1))**2
   R(J) = (SIGL(J+1) * S(J) - SIGL(J) * S(J+1)) / (SIGL(J+1) * S(J) + SIGL(J) * S(J+1))
   CHI(J) = EXP (-2._QL * S(J) * THKD(J))
 END DO

 SM = S(SXLYR)
 SL = S(RXLYR)

 SELECT CASE (KFG)

 CASE (200)                                  ! ED Tx in Air - Rx in Air
   CALL PROPAGATE (0)
   XI_V = VMG * EXP (SM * ZS)

 CASE (201)                                  ! ED Tx in Air - Rx in Layer
   XPA = SM * ZS
   DO J = 1, RXLYR-1
     XPA = XPA - S(J) * THKD(J)
   END DO
   PRJG = EXP (XPA)
   PRJF = PRJG
   CALL PROPAGATE (0)
   DO J = 0, RXLYR-1
     PRJG = PRJG * (ONE + VG(J)) / (ONE + VG(J+1) * CHI(J+1))
     PRJF = PRJF * (ONE + VF(J)) / (ONE + VF(J+1) * CHI(J+1))
   END DO
   ETA_V = PRJG
   G_V = PRJF
   G_H = -G_V
   XP1 = EXP (-SL * THKD(RXLYR))
   XI_V  = VLG * XP1 * ETA_V
   F_V  = VLF * XP1 * G_V
   F_H = -F_V

 CASE (203)                                  ! ED Tx in Air - Rx in Basement
   XPA = SM * ZS
   DO J = 1, RXLYR-1
     XPA = XPA - S(J) * THKD(J)
   END DO
   PRJG = EXP (XPA)
   PRJF = PRJG
   CALL PROPAGATE (0)
   DO J = 0, RXLYR-1
     PRJG = PRJG * (ONE + VG(J)) / (ONE + VG(J+1) * CHI(J+1))
     PRJF = PRJF * (ONE + VF(J)) / (ONE + VF(J+1) * CHI(J+1))
   END DO
   ETA_V = PRJG
   G_V = PRJF
   G_H = -G_V

 CASE (210)                                   ! ED Tx in Layer - Rx in Air
   XP1 = EXP (SM * (DPTHL(SXLYR) - ZS))
   XP2 = EXP (SM * (ZS - DPTHL(SXLYR+1) - THKD(SXLYR)))
   CALL PROPAGATE (1)
   XI_VBAR = (XP1 + VMG * XP2) / DENOMG

   XPA = ZERO
   DO J = RXLYR+1, SXLYR-1
     XPA = XPA - S(J) * THKD(J)
   END DO
   PRJG = EXP (XPA)

   DO J = RXLYR, SXLYR-1
     PRJG = PRJG * (ONE + AG(J+1)) / (ONE + AG(J) * CHI(J))
   END DO
   XI_V = PRJG * XI_VBAR

 CASE (230)                              ! ED Tx in basement - Rx in Air
   XPA = ZERO
   DO J = RXLYR+1, NLYR-1
     XPA = XPA - S(J) * THKD(J)
   END DO
   PRJG = EXP (XPA)

   CALL PROPAGATE (2)
   DO J = RXLYR, SXLYR-1
     PRJG = PRJG * (ONE + AG(J+1)) / (ONE + AG(J) * CHI(J))
   END DO
   XI_V = PRJG * EXP (SM * (DPTHL(SXLYR) - ZS))

 CASE (211)                                  ! ED Tx in Rx Layer
   XP1 = EXP (SM * (DPTHL(SXLYR) - THKD(SXLYR) - ZS))
   XP2 = EXP (SM * (ZS - DPTHL(SXLYR+1)))
   XQ1 = EXP (SM * (DPTHL(SXLYR) - ZS))
   XQ2 = EXP (SM * (ZS - DPTHL(SXLYR+1) - THKD(SXLYR)))

   CALL PROPAGATE (1)
   F_V  = VMF * (AMF * XP1 + XP2) / DENOMF
   F_H  = VMF * (AMF * XP1 - XP2) / DENOMF
   XI_V = VMG * (AMG * XP1 + XP2) / DENOMG

   G_V   = AMF * (XQ1 + VMF * XQ2) / DENOMF
   G_H   = AMF * (XQ1 - VMF * XQ2) / DENOMF
   ETA_V = AMG * (XQ1 + VMG * XQ2) / DENOMG

 CASE (212)                                  ! ED Tx Layer above Rx Layer
   XQ1 = EXP (SM * (DPTHL(SXLYR) - ZS - THKD(SXLYR)))
   XQ2 = EXP (SM * (ZS - DPTHL(SXLYR+1)))
   CALL PROPAGATE (1)
   G_VBAR   = (AMF * XQ1 + XQ2) / DENOMF
   G_HBAR   = (AMF * XQ1 - XQ2) / DENOMF
   ETA_VBAR = (AMG * XQ1 + XQ2) / DENOMG

   XPA = ZERO
   DO J = SXLYR+1, RXLYR-1
     XPA = XPA - S(J) * THKD(J)
   END DO
   PRJG = EXP (XPA)
   PRJF = PRJG
   DO J = SXLYR, RXLYR-1
     PRJG = PRJG * (ONE + VG(J)) / (ONE + VG(J+1) * CHI(J+1))
     PRJF = PRJF * (ONE + VF(J)) / (ONE + VF(J+1) * CHI(J+1))
   END DO
   G_V   = PRJF * G_VBAR
   G_H   = PRJF * G_HBAR
   ETA_V = PRJG * ETA_VBAR
   XP1   = EXP (-SL * THKD(RXLYR))

   F_V   = VLF * XP1 * G_V
   F_H   = VLF * XP1 * G_H
   XI_V  = VLG * XP1 * ETA_V

 CASE (213)                                  ! ED Tx in Layer - Rx in Basement
   XQ1 = EXP (SM * (DPTHL(SXLYR) - ZS - THKD(SXLYR)))
   XQ2 = EXP (SM * (ZS - DPTHL(SXLYR+1)))
   CALL PROPAGATE (1)
   ETA_VBAR = (AMG * XQ1 + XQ2) / DENOMG
   G_VBAR   = (AMF * XQ1 + XQ2) / DENOMF
   G_HBAR   = (AMF * XQ1 - XQ2) / DENOMF

   XPA = ZERO
   DO J = SXLYR+1, RXLYR-1
     XPA = XPA - S(J) * THKD(J)
   END DO
   PRJG = EXP (XPA)
   PRJF = PRJG
   DO J = SXLYR, RXLYR-1
     PRJG = PRJG * (ONE + VG(J)) / (ONE + VG(J+1) * CHI(J+1))
     PRJF = PRJF * (ONE + VF(J)) / (ONE + VF(J+1) * CHI(J+1))
   END DO
   G_V   = PRJF * G_VBAR
   G_H   = PRJF * G_HBAR
   ETA_V = PRJG * ETA_VBAR

 CASE (221)                                  ! ED Tx Layer below Rx Layer
   XP1 = EXP (SM * (DPTHL(SXLYR) - ZS))
   XP2 = EXP (SM * (ZS - DPTHL(SXLYR+1) - THKD(SXLYR)))
   CALL PROPAGATE (1)
   F_VBAR  = (XP1 + VMF * XP2) / DENOMF
   F_HBAR  = (XP1 - VMF * XP2) / DENOMF
   XI_VBAR = (XP1 + VMG * XP2) / DENOMG

   XPA = ZERO
   DO J = RXLYR+1, SXLYR-1
     XPA = XPA - S(J) * THKD(J)
   END DO
   PRJG = EXP (XPA)
   PRJF = PRJG

   DO J = RXLYR, SXLYR-1
     PRJG = PRJG * (ONE + AG(J+1)) / (ONE + AG(J) * CHI(J))
     PRJF = PRJF * (ONE + AF(J+1)) / (ONE + AF(J) * CHI(J))
   END DO
   F_V  = PRJF * F_VBAR
   F_H  = PRJF * F_HBAR
   XI_V = PRJG * XI_VBAR

   XQ1 = EXP (-SL * THKD(RXLYR))
   G_V   = AF(RXLYR) * XQ1 * F_V
   G_H   = AF(RXLYR) * XQ1 * F_H
   ETA_V = AG(RXLYR) * XQ1 * XI_V

 CASE (231)                                  ! ED Tx in basement - Rx in Layer
   XPA = ZERO
   DO J = RXLYR+1, NLYR-1
     XPA = XPA - S(J) * THKD(J)
   END DO
   PRJG = EXP (XPA)
   PRJF = PRJG

   CALL PROPAGATE (2)
   DO J = RXLYR, NLYR-1
     PRJG = PRJG * (ONE + AG(J+1)) / (ONE + AG(J) * CHI(J))
     PRJF = PRJF * (ONE + AF(J+1)) / (ONE + AF(J) * CHI(J))
   END DO

   XP1 = EXP (SM * (DPTHL(SXLYR) - ZS))
   F_V  = PRJF * XP1
   F_H  = F_V
   XI_V = PRJG * XP1

   XQ1 = EXP (-SL * THKD(RXLYR))
   G_V   = AF(RXLYR) * XQ1 * F_V
   G_H   = G_V
   ETA_V = AG(RXLYR) * XQ1 * XI_V

 CASE (233)                                 ! ED Tx in basement - Rx in Basement
   XQ1 = EXP (SM * (DPTHL(SXLYR) - ZS))
   CALL PROPAGATE (2)
   ETA_V = AMG * XQ1
   G_V   = AMF * XQ1
   G_H   = G_V
 END SELECT

 CONTAINS

   SUBROUTINE PROPAGATE (LSX)
!  --------------------------

   INTEGER LSX

   SELECT CASE (LSX)
   CASE (0)                  !  SXLYR = 0 - Build downward source propagator only
     VG(NLYR-1) = T(NLYR-1)
     VF(NLYR-1) = R(NLYR-1)
     DO J = NLYR-2, 0, -1
       VACHI = VG(J+1) * CHI(J+1)
       VG(J) = (VACHI + T(J)) / (ONE + VACHI* T(J))
       VACHI = VF(J+1) * CHI(J+1)
       VF(J) = (VACHI + R(J)) / (ONE + VACHI* R(J))
     END DO
     VLG = VG(RXLYR)
     VMG = VG(0)
     VLF = VF(RXLYR)
     VMF = VF(0)

   CASE (2)                 ! SXLYR = NLYR - Build upward source propagator only
     AG(1) = -T(0)
     AF(1) = -ONE
     DO J = 2, NLYR
       VACHI = AG(J-1) * CHI(J-1)
       AG(J) = (VACHI - T(J-1)) / (ONE - VACHI* T(J-1))
       VACHI = AF(J-1) * CHI(J-1)
       AF(J) = (VACHI - R(J-1)) / (ONE - VACHI* R(J-1))
     END DO
     AMG = AG(NLYR)
     AMF = AF(NLYR)

   CASE (1)                     ! Build downward and upward source propagators
     VG(NLYR-1) = T(NLYR-1)
     VF(NLYR-1) = R(NLYR-1)
     DO J = NLYR-2, 0, -1
       VACHI = VG(J+1) * CHI(J+1)
       VG(J) = (VACHI + T(J)) / (ONE + VACHI* T(J))
       VACHI = VF(J+1) * CHI(J+1)
       VF(J) = (VACHI + R(J)) / (ONE + VACHI* R(J))
     END DO

     AG(1) = -T(0)
     AF(1) = -ONE
     DO J = 2, NLYR
       VACHI = AG(J-1) * CHI(J-1)
       AG(J) = (VACHI - T(J-1)) / (ONE - VACHI* T(J-1))
       VACHI = AF(J-1) * CHI(J-1)
       AF(J) = (VACHI - R(J-1)) / (ONE - VACHI* R(J-1))
     END DO
     VLG = VG(RXLYR)
     VLF = VF(RXLYR)
     VMG = VG(SXLYR)
     VMF = VF(SXLYR)
     AMG = AG(SXLYR)
     AMF = AF(SXLYR)
     DENOMG = ONE - VMG * AMG * CHI(SXLYR)
     DENOMF = ONE - VMF * AMF * CHI(SXLYR)

   END SELECT

   END SUBROUTINE PROPAGATE

 END SUBROUTINE EDSX_COEF

 SUBROUTINE MDSX_COEF (KFG,RXLYR,SXLYR,LMBDA,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                       ZS,S,XI_V,XI_H,F_V,ETA_V,ETA_H,G_V)
!---------------------------------------------------------------------------

!***  Called by
!***  Calls PROPAGATE

!  For an arbitrarily oriented magnetic dipole source at ZS in layer SXLYR of a
!  halfspace with NLYR-1 layers above basement, MDSX_COEF computes the coefficients
!  for the vertical and horizontal F (electric) and G (magnetic) potentials
!  These coefficients can be used to compute magnetic and electric fields.
!
!          Input
!          -----
!  KFG indicates source type and layer locations of source and receiver: air, basement or intermediate
!
!  KFG = IJK : I = type; J = Tx layer; K = Rx layer
!
!        I = 1 => magnetic dipole source
!
!        J = 0 => Transmitter in air
!        J = 3 => Transmitter in basement
!
!        K = 0 => Receiver in air
!        K = 3 => Receiver in basement
!
!        When BOTH tranamitter nor receiver are in an intermediate layer:
!
!        JK = 11 => Transmitter and recever are in the same layer
!        JK = 12 => Transmitter layer is above receiver layer
!        JK = 21 => Transmitter layer is below receiver layer
!
!   RXLYR - layer containing receiver
!   SXLYR - layer containing transmitter
!   LMBDA - Hankel transform variable
!    NLYR - number of layers
!    THKD - thickness of layer J
!   DPTHL - depth to TOP of layer J
!    RMUD - mu(i) / mu(0)
!    SIGL - complex conductivity including Cole-Cole + dimagnetic terms
!    KSQL - iwu * SIGL
!      ZS - depth of dipole transmitter (negative above earth)
!
!          Output
!          ------
!             S - Layer property variable in Hankel space
!   XI_V, ETA_V - coefficients for the vertical magnetic Schelkunoff potentials.
!   XI_H, ETA_H - coefficients for the horizontal magnetic Schelkunoff potential
!
!   F_V,  G_V   - coefficients for the vertical electric Schelkunoff potentials.
!
!  P (F_V, XI_V,  XI_H)  are all used in the generic form:  P * EXP (S(RXLYR) * (ZR - DPTHL(RXLYR+1)))
!  Q (G_V, ETA_V, ETA_H) are all used in the generic form:  Q * EXP (S(RXLYR) * (DPTHL(RXLYR) - ZR))
!                               where ZR = receiver depth relative to surface

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 COMPLEX(KIND=QL), PARAMETER :: ONE=(1._QL,0._QL),ZERO=(0._QL,0._QL)
 INTEGER NLYR,KFG,RXLYR,SXLYR,J
 REAL(KIND=QL) RMUD(0:NLYR),LMBDA,ZS
 REAL(KIND=QL), DIMENSION (NLYR) ::  THKD,DPTHL,RMUSQ
 COMPLEX(KIND=QL) SM,SL,VLG,VMG,AMG,VLF,VMF,AMF,XPA,XP1,XP2,XQ1,XQ2,DENOMF,DENOMG, &
                  PRJG,PRJF,XI_V,XI_H,F_V,ETA_V,ETA_H,G_V,XI_VBAR,XI_HBAR, &
                  F_VBAR,ETA_VBAR,ETA_HBAR,G_VBAR,LMBSQ,VACHI
 COMPLEX(KIND=QL), DIMENSION (0:NLYR) :: S,T,R,AF,AG,VF,VG,CHI
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL

 S = ZERO; T = ZERO; R = ZERO; AF = ZERO; AG = ZERO; VF = ZERO; VG = ZERO; CHI = ONE
 F_V = ZERO;  XI_V  = ZERO;  XI_H  = ZERO
 G_V = ZERO;  ETA_V = ZERO;  ETA_H = ZERO


! VF & VG are the downward propagators for the F & G potentials respectively.
! AF & AG are the upward propagators.
! T & R are the magnetic and magnetic reflection coefficients


 LMBSQ = CMPLX (LMBDA * LMBDA, 0._QL, KIND=QL)
 S(0) = CMPLX (LMBDA, 0._QL,KIND=QL)
 DO J = 1,NLYR
   S(J) = SQRT (KSQL(J) + LMBSQ)
   RMUSQ(J) = RMUD(J) * RMUD(J)
 END DO
 T(0) = ( (RMUSQ(1) - 1._QL) * LMBSQ - KSQL(1) ) / ( RMUD(1)*S(0) + S(1) )**2
 R(0) = ONE
 DO J = 1,NLYR-1
   T(J) = (RMUSQ(J+1) - RMUSQ(J)) * LMBSQ + (RMUSQ(J+1) * KSQL(J) - RMUSQ(J) * KSQL(J+1)) &
        / (RMUD(J+1) * S(J) + RMUD(J) * S(J+1))**2
   R(J) = (SIGL(J+1) * S(J) - SIGL(J) * S(J+1)) / (SIGL(J+1) * S(J) + SIGL(J) * S(J+1))
   CHI(J) = EXP (-2._QL * S(J) * THKD(J))
 END DO

 SM = S(SXLYR)
 SL = S(RXLYR)


 SELECT CASE (KFG)
 CASE (100)                                  ! MD Tx in Air - Rx in Air
   CALL PROPAGATE (0)
   XI_V = VMG * EXP (SM * ZS)
   XI_H = -XI_V

 CASE (101)                                  ! MD Tx in air - Rx in Layer
   XPA = ZERO
   DO J = 1, RXLYR-1
     XPA = XPA - S(J) * THKD(J)
   END DO
   PRJG = EXP (XPA)
   CALL PROPAGATE (0)
   DO J = 0, RXLYR-1
     PRJG = PRJG * (ONE + VG(J)) / (ONE + VG(J+1) * CHI(J+1))
   END DO
   ETA_V = PRJG * EXP (SM * ZS)
   XI_V  = VLG * EXP (-SL * THKD(RXLYR)) * ETA_V
   XI_H  = -XI_V
   ETA_H = -ETA_V

 CASE (103)                                  ! MD Tx in Air - Rx in Basement
   XPA = ZERO
   DO J = 1, RXLYR-1
     XPA = XPA - S(J) * THKD(J)
   END DO
   CALL PROPAGATE (0)
   PRJG = EXP (XPA)
   DO J = 0, RXLYR-1
     PRJG = PRJG * (ONE + VG(J)) / (ONE + VG(J+1) * CHI(J+1))
   END DO
   ETA_V = PRJG * EXP (SM * ZS)
   ETA_H = -ETA_V

 CASE (110)                                  ! MD Tx in Layer - Rx in Air
   XP1 = EXP (SM * (DPTHL(SXLYR) - ZS))
   XP2 = EXP (SM * (ZS - DPTHL(SXLYR+1) - THKD(SXLYR)))
   CALL PROPAGATE (1)
   XI_VBAR = (XP1 + VMG * XP2) / DENOMG
   XI_HBAR = (XP1 - VMG * XP2) / DENOMG

   XPA = ZERO
   DO J = RXLYR+1, SXLYR-1
     XPA = XPA - S(J) * THKD(J)
   END DO
   PRJG = EXP (XPA)

   DO J = RXLYR, SXLYR-1
     PRJG = PRJG * (ONE + AG(J+1)) / (ONE + AG(J) * CHI(J))
   END DO
   XI_V = PRJG * XI_VBAR
   XI_H = PRJG * XI_HBAR

 CASE (130)                              ! MD Tx in basement - Rx in Air
   XPA = ZERO
   DO J = RXLYR+1, NLYR-1
     XPA = XPA - S(J) * THKD(J)
   END DO
   PRJG = EXP (XPA)

   CALL PROPAGATE (2)
   DO J = RXLYR, SXLYR-1
     PRJG = PRJG * (ONE + AG(J+1)) / (ONE + AG(J) * CHI(J))
   END DO
   XI_V = PRJG * EXP (SM * (DPTHL(SXLYR) - ZS))
   XI_H = XI_V

 CASE (111)                                  ! MD Tx in Rx Layer
   XP1 = EXP (SM * (DPTHL(SXLYR) - THKD(SXLYR) - ZS))
   XP2 = EXP (SM * (ZS - DPTHL(SXLYR+1)))
   XQ1 = EXP (SM * (DPTHL(SXLYR) - ZS))
   XQ2 = EXP (SM * (ZS - DPTHL(SXLYR+1) - THKD(SXLYR)))

   CALL PROPAGATE (1)
   XI_V = VMG * (AMG * XP1 + XP2) / DENOMG
   XI_H = VMG * (AMG * XP1 - XP2) / DENOMG
   F_V  = VMF * (AMF * XP1 + XP2) / DENOMF

   ETA_V = AMG * (XQ1 + VMG * XQ2) / DENOMG
   ETA_H = AMG * (XQ1 - VMG * XQ2) / DENOMG
   G_V   = AMF * (XQ1 + VMF * XQ2) / DENOMF

 CASE (112)                                  ! MD Tx Layer above Rx Layer
   XQ1 = EXP (SM * (DPTHL(SXLYR) - ZS - THKD(SXLYR)))
   XQ2 = EXP (SM * (ZS - DPTHL(SXLYR+1)))
   CALL PROPAGATE (1)
   ETA_VBAR = (AMG * XQ1 + XQ2) / DENOMG
   ETA_HBAR = (AMG * XQ1 - XQ2) / DENOMG
   G_VBAR   = (AMF * XQ1 + XQ2) / DENOMF

   XPA = ZERO
   DO J = SXLYR+1, RXLYR-1
     XPA = XPA - S(J) * THKD(J)
   END DO
   PRJG = EXP (XPA)
   PRJF = PRJG
   DO J = SXLYR, RXLYR-1
     PRJG = PRJG * (ONE + VG(J)) / (ONE + VG(J+1) * CHI(J+1))
     PRJF = PRJF * (ONE + VF(J)) / (ONE + VF(J+1) * CHI(J+1))
   END DO
   ETA_V = PRJG * ETA_VBAR
   ETA_H = PRJG * ETA_HBAR
   G_V   = PRJF * G_VBAR
   XP1   = EXP (-SL * THKD(RXLYR))
   XI_V  = VLG * XP1 * ETA_V
   XI_H  = VLG * XP1 * ETA_H
   F_V   = VLF * XP1 * G_V

 CASE (113)                                  ! MD Tx in Layer - Rx in Basement
   XQ1 = EXP (SM * (DPTHL(SXLYR) - ZS - THKD(SXLYR)))
   XQ2 = EXP (SM * (ZS - DPTHL(SXLYR+1)))
   CALL PROPAGATE (1)
   ETA_VBAR = (AMG * XQ1 + XQ2) / DENOMG
   ETA_HBAR = (AMG * XQ1 - XQ2) / DENOMG
   G_VBAR   = (AMF * XQ1 + XQ2) / DENOMF

   XPA = ZERO
   DO J = SXLYR+1, RXLYR-1
     XPA = XPA - S(J) * THKD(J)
   END DO
   PRJG = EXP (XPA)
   PRJF = PRJG
   DO J = SXLYR, RXLYR-1
     PRJG = PRJG * (ONE + VG(J)) / (ONE + VG(J+1) * CHI(J+1))
     PRJF = PRJF * (ONE + VF(J)) / (ONE + VF(J+1) * CHI(J+1))
   END DO
   ETA_V = PRJG * ETA_VBAR
   ETA_H = PRJG * ETA_HBAR
   G_V   = PRJF * G_VBAR

 CASE (121)                                  ! MD Tx Layer below Rx Layer
   XP1 = EXP (SM * (DPTHL(SXLYR) - ZS))
   XP2 = EXP (SM * (ZS - DPTHL(SXLYR+1) - THKD(SXLYR)))
   CALL PROPAGATE (1)
   XI_VBAR = (XP1 + VMG * XP2) / DENOMG
   XI_HBAR = (XP1 - VMG * XP2) / DENOMG
   F_VBAR  = (XP1 + VMF * XP2) / DENOMF

   XPA = ZERO
   DO J = RXLYR+1, SXLYR-1
     XPA = XPA - S(J) * THKD(J)
   END DO
   PRJG = EXP (XPA)
   PRJF = PRJG

   DO J = RXLYR, SXLYR-1
     PRJG = PRJG * (ONE + AG(J+1)) / (ONE + AG(J) * CHI(J))
     PRJF = PRJF * (ONE + AF(J+1)) / (ONE + AF(J) * CHI(J))
   END DO
   XI_V = PRJG * XI_VBAR
   XI_H = PRJG * XI_HBAR
   F_V  = PRJF * F_VBAR

   XQ1 = EXP (-SL * THKD(RXLYR))
   ETA_V = AG(RXLYR) * XQ1 * XI_V
   ETA_H = AG(RXLYR) * XQ1 * XI_H
   G_V   = AF(RXLYR) * XQ1 * F_V

 CASE (131)                                  ! MD Tx in basement - Rx in Layer
   XPA = ZERO
   DO J = RXLYR+1, NLYR-1
     XPA = XPA - S(J) * THKD(J)
   END DO
   PRJG = EXP (XPA)
   PRJF = PRJG

   CALL PROPAGATE (2)
   DO J = RXLYR, NLYR-1
     PRJG = PRJG * (ONE + AG(J+1)) / (ONE + AG(J) * CHI(J))
     PRJF = PRJF * (ONE + AF(J+1)) / (ONE + AF(J) * CHI(J))
   END DO
   XP1 = EXP (SM * (DPTHL(SXLYR) - ZS))
   F_V  = PRJF * XP1
   XI_V = PRJG * XP1
   XI_H = XI_V

   XQ1 = EXP (-SL * THKD(RXLYR))
   G_V   = AF(RXLYR) * XQ1 * F_V
   ETA_V = AG(RXLYR) * XQ1 * XI_V
   ETA_H = ETA_V

 CASE (133)                              ! MD Tx in basement - Rx in Basement
   XQ1 = EXP (SM * (DPTHL(SXLYR) - ZS))
   CALL PROPAGATE (2)
   ETA_V = AMG * XQ1
   G_V   = AMF * XQ1
   ETA_H = ETA_V
 END SELECT

 CONTAINS

   SUBROUTINE PROPAGATE (LSX)
!  --------------------------

   INTEGER LSX

   SELECT CASE (LSX)
   CASE (0)                  !  SXLYR = 0 - Build downward source propagator only
     VG(NLYR-1) = T(NLYR-1)
     VF(NLYR-1) = R(NLYR-1)
     DO J = NLYR-2, 0, -1
       VACHI = VG(J+1) * CHI(J+1)
       VG(J) = (VACHI + T(J)) / (ONE + VACHI* T(J))
       VACHI = VF(J+1) * CHI(J+1)
       VF(J) = (VACHI + R(J)) / (ONE + VACHI* R(J))
     END DO
     VLG = VG(RXLYR)
     VMG = VG(0)

   CASE (2)                 ! SXLYR = NLYR - Build upward source propagator only
     AG(1) = -T(0)
     AF(1) = -ONE
     DO J = 2, NLYR
       VACHI = AG(J-1) * CHI(J-1)
       AG(J) = (VACHI - T(J-1)) / (ONE - VACHI* T(J-1))
       VACHI = AF(J-1) * CHI(J-1)
       AF(J) = (VACHI - R(J-1)) / (ONE - VACHI* R(J-1))
     END DO
     AMG = AG(NLYR)
     AMF = AF(NLYR)

   CASE (1)                     ! Build downward and upward source propagators
     VG(NLYR-1) = T(NLYR-1)
     VF(NLYR-1) = R(NLYR-1)
     DO J = NLYR-2, 0, -1
       VACHI = VG(J+1) * CHI(J+1)
       VG(J) = (VACHI + T(J)) / (ONE + VACHI* T(J))
       VACHI = VF(J+1) * CHI(J+1)
       VF(J) = (VACHI + R(J)) / (ONE + VACHI* R(J))
     END DO

     AG(1) = -T(0)
     AF(1) = -ONE
     DO J = 2, NLYR
       VACHI = AG(J-1) * CHI(J-1)
       AG(J) = (VACHI - T(J-1)) / (ONE - VACHI* T(J-1))
       VACHI = AF(J-1) * CHI(J-1)
       AF(J) = (VACHI - R(J-1)) / (ONE - VACHI* R(J-1))
     END DO
     VLG = VG(RXLYR)
     VLF = VF(RXLYR)
     VMG = VG(SXLYR)
     VMF = VF(SXLYR)
     AMG = AG(SXLYR)
     AMF = AF(SXLYR)
     DENOMG = ONE - VMG * AMG * CHI(SXLYR)
     DENOMF = ONE - VMF * AMF * CHI(SXLYR)

   END SELECT

   END SUBROUTINE PROPAGATE

 END SUBROUTINE MDSX_COEF

!========================================================

!  INVERSION SUBROUTINES

!========================================================

 SUBROUTINE NLSQ2 (JL,JRL,JSTAT,JS,JR,NPAR,NDATA,XDATA,RDATA,RWTS,PCTCNV,NDSTP,KPCT,CXPAR,ELAS,LBND,UBND, &
                   MAXITS,INVPRT,NW,NP,SURVEY_TYPE,SOURCE_TYPE,NLINES,MLINES,LINE,HEADER_ID, &
                   IPLT,YXZPLT,MCMP,CMP,MRXL,MCHNL,NCHNL,NFRQ,NFRQHS,TDFD,STEP,FREQ,FRQHS,NSX,SWX,  &
                   SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,TOPN,TCLS,NFT,CURNT,ISYS,SVAZM,UNITS,RX_TYPE,   &
                   IDH,MXTX,NTX,SXN,SXE,SXZ,SXDIP,SXAZM,MXVRTX,NVRTX,NRXTX,MRXTX,RXID,MQVR,     &
                   XRXTX,YRXTX,ZRXTX,BPRM,MD1,MD2,RXAZM,RXDIP,KNORM2,NCTD,MXRHO,RHOTRP,NLYR,THK,    &
                   RES,RMUD,REPS,CHRG,CTAU,CFREQ,NR1, &
                   rxmnt, NSTAT)
!--------------------------------------------------------------------------------------------------

!***  Called by: MAIN
!***      Calls: CNVRT_BOUNDS, CNVRT2_XPAR, ESVD, FORJAC, INDEX_MPAR,
!                NOISE_2_SIGR, SOLVE2, WRITE_INVMDL, WRITE_DATA,
!                WRITE_TD, WRITE_FD
!
!  Inversion for receiver JR of transmitter JS corresponding to station JRL of line JL.
!
!  SVD non-linear least square inversion using a modified
!  Gauss-Newton-Marquardt method as described in Jupp & Vozoff, 1975,
!  Stable iterative methods for the inversion of geophysical data,
!  Geophys. J. R. Astr. Soc.,42
!
!  New convention: Dec, 2003: VERR is now VD - VM
!  Thus DELPAR is now added rather than subtracted during updates.
!
!  2007: XWTS is no longer used.  Instead, XDATA contains only those data not
!        weighted to zero.
!
!=========================================================================

!***  Called by: MAIN
!***      Calls: ESVD, FORJAC, WRITE_MODEL, WRITE_MISFIT, SOLVE2
!
!             Inversion Input Variables
!             -------------------------
!
!
!  NPAR        : number of parameters; = 2*NLYR -1
!  NDATA       = the number of data points to be inverted excluding all those weighted to 0
!              = the number of rows of the Jacobian matrix
!  XDATA       : 1D array of length NDATA containing data to be inverted.
!  RDATA       : 4D array containing all user entered data to be inverted.
!                entries weighted to 0 have been set to 0
!  RWTS        : 4D array containing weights for all user entered data
!  PCTCNV      : manually set convergence threshold
!  NDSTP       : number of manual derivative steps
!  KPCT        : percent changes for derivative steps
!  CXPAR       = 0 => parameter is completely free to vary as dictated by inversion step
!              = 1 => parameter is fixed
!              = 2 => parameter is constrained by elasticity.
!              = 3 => parameter bounds are buffered.
!  ELAS        - [0 to 1] fraction of proposed step actually taken.
!  LBND, UBND  - lower & upper bound of parameter
!  MAXITS      - maximum permitted iterations
!  NW          - verbose output unit number
!  NP         = plot file unit number
!  INVPRT      = 0 No output DURING inversion.  The final model AFTER inversion,
!                  but NOT the final model data, is written to output files.
!              = 1 as above plue plus final model data
!              = 2 as above plus intermediate model sets after each iteration
!              = 3 as above plus intermediate model data after each iteration
!  SURVEY_TYPE = 1 : general survey
!              = 2 : moving rectangular loop Tx with fixed offset MD Rx
!              = 3 : magnetic dipole Tx with fixed offset MD Rx
!              = 4 : coincident loop
!              = 5 : borehole MD Tx with constant offset Rx
!              = 6 : magnetotellurics
!  SOURCE_TYPE = 1 : general loop
!              = 2 : grounded wire
!              = 3 : magnetic dipole
!              = 4 : coincident loop
!              = 5 : plane wave
!
!  XMODL       - Array of model data points.
!  XPAR        - On call, XPAR contains the log (initial parameter estimates).
!                On exit it contains the final values.
!
!             System Input Variables
!             ----------------------
!
!     TDFD   = 1 for TD; 2 for FD
!     CMP    - (time-domain only) component to be inverted
!               11: in-line; 13: vertical; 2: joint vertical & in-line
!               3 component; 4 total field
!
!     KNRM   - dimension of NORM: = 3 for time-domain,  = NFRQ for frequency-domain
!     NORM   - PPM conversion
!     FREQ   - array of NFRQ frequencies
!     STEP   = 1 iff step response is to be computed
!     NSX    - number of points used to discretise transmitter signal
!     SWX    - abscissae (seconds) of current waveform
!     SWY    - dI/dt * Tx moment & nanotesla conversion at times SWX
!     NTYRP  - number of values in TRP for total signal length: 2 * NPULS *PULSE
!     TRP    - array of time values for FD -> TD transformations
!     NTYPLS - number of TRP values in 1 PULSE
!     NCHNL  = number of time domain channels
!     MCHNL  = total number of readings per station to be inverted(TD or FD)
!            = NCHNL for time-domain when CMP = 11, 13, 4, 42, 43
!            = 2* NCHNL for time-domain when CMP = 2
!            = 3* NCHNL for time-domain when CMP = 3
!
!            = 2 * NFRQ for frequency-domain
!     TOPN   - time at which receiver channel I opens.
!     TCLS   - time at which receiver channel I closes.
!     FREQ   = array of NFRQ frequencies
!     NRXST  - dimension for receiver offset & transmitter tilt
!            = NFRQ in FD;  = NSTATL in TD
!     ZRX    - vertical receiver offset for each frequency;   below = positive
!     XRX    - in-line receiver offset for each frequency;    behind = positive
!     YRX    - transverse receiver offset for each frequency; left = positive.
!
!             Model Description Input Variables
!             ---------------------------------
!
!     NLYR   - number of layers (1 or 2)
!     THK    - array of layer thicknesses
!     RES    - array of layer resistivities
!     RMU    - mu(i) / mu(0)
!     REPS   - relative dielectric constant
!
!     CALF, CTAU & CFREQ are the layered earth Cole-Cole parameters.
!
!         Other Stuff
!         -----------
!
!     A        -  Jacobian matrix
!     WSP      - working space.
!     VERR     - error vector
!     SUMSQ    - sum squared scaled error (point norm)
!     RMSER    - RMS error (in percent)
!     BND      - estimate of noise to signal ratio, used for damping limit.
!     WSP      - working space.
!     SV, UMAT, and VMAT are the decomposition of A.  SV contains eigenvalues.

!      The model set consists of the model parameters, parameter importance,
!      standard error and RSVT, the relative singular value threshold.
!
!      The model data (distinct from survey data) is defined as the fields or
!      ppm values for each channel or frequency for each station; ie the
!      model response for a given model.
!

 IMPLICIT NONE
 INTEGER, PARAMETER :: IP=1, QL=SELECTED_REAL_KIND(p = 18), MAXITS_INT=5
 REAL, PARAMETER :: BND=0.01, EXPND=2., RSVT0=0.1, ETA=1.E-7, TOL=.5E-31,GCRIT=1.E-4
 INTEGER JL,JRL,JSTAT,JS,JR,NPAR,NDATA,MAXITS,ITS,PITS,ICNT,NW,NP,INVPRT,CXPAR(NPAR),NSV,NLINES,MLINES, &
         MRXL,MCMP,MCHNL,NCHNL,NFRQ,NFRQHS,NFT,TDFD,STEP,NSX,NPULS,NTYPLS,NTYRP,SURVEY_TYPE,  &
         ISYS,SOURCE_TYPE,MXTX,NTX,MXVRTX,NVRTX(MXTX),MRXTX,NRXTX(MXTX),RXID(MRXTX,NTX),MQVR, &
         MD1,MD2,MXRHO,KNORM2(MRXTX,NTX),JP,NCTD(MRXTX,NTX),NR1,KDEG
 INTEGER, DIMENSION(NLINES) :: RX_TYPE,UNITS,IDH,CMP,LINE,HEADER_ID,IPLT
 INTEGER RWTS(MCHNL,MRXL,MCMP,MLINES)
 REAL PCTCNV,A(NDATA,NPAR+1),UMAT(NDATA,NPAR),VMAT(NPAR,NPAR),WSP(3*NPAR),DELT,DELTOLD, &
      RSVT,ZERO,FNM,GTEST,SQPDRE,PDRE,SSQNEW,SUMSQ,RMSERR,ERR1P,B1,B2,B3,B4,BMID
 REAL, DIMENSION(NDATA) :: VERR,XMODL,XDATA
 REAL, DIMENSION(NPAR) :: SV,XPAR,MPAR,DELPAR,GXPAR,ELAS,LBND,UBND,XLBND,XUBND
 REAL, DIMENSION(MCHNL,MRXL,MCMP,NLINES) :: RMODL,RDATA
 REAL, DIMENSION(MD1,MD2) :: RXAZM,RXDIP
 REAL FREQ(NFRQ),FRQHS(NFRQHS),SWX(NSX),SWY(NSX,3),PULSE,TRP(NTYRP),TOPN(NCHNL),TCLS(NCHNL),   &
      CURNT(NFT),XRXTX(MRXTX,NTX,MQVR),YRXTX(MRXTX,NTX,MQVR),ZRXTX(MRXTX,NTX),SXN(MXVRTX,NTX), &
      SXE(MXVRTX,NTX),SXZ(MXTX),SXDIP(MXTX),SXAZM(MXTX),RHOTRP(MXRHO),BPRM(MRXTX,NTX),         &
      SVAZM(NLINES),FNEW,FOLD
 REAL(KIND=QL) YXZPLT(3,MRXL,NLINES)
 LOGICAL,PARAMETER :: WITHU=.TRUE., WITHV=.TRUE.
 LOGICAL JCBN,FINAL,MOVE
 CHARACTER(LEN=20) UTXT(2),PLT_PT(3),CPLT,CLINE,QL0
 REAL :: DMPFAC(NPAR)
 Real :: a_orig(ndata, npar)
 Integer :: jd, ji
 Real :: rxmnt(nlines)
 Integer:: tvals(8)
 Real :: rmsini
 Integer :: NSTAT

! Model Dependent Variables

 INTEGER NLYR,NL1,IPAR,NDSTP,KPCT(NDSTP)
 REAL PFAC(NDSTP),PARFAC,IMPORT(NPAR)
 REAL(KIND=QL) RMUD(0:NLYR)
 REAL, DIMENSION(NLYR) :: RES,REPS,CHRG,CTAU,CFREQ,THK,DEPTH
 DATA PLT_PT /'Rx','Tx-Rx_MID','Tx'/

!  Preset threshold parameters and index workspace, but return if problem
!  size in error.

 ZERO =BND * BND
 IF (ZERO < ETA) ZERO = ETA

 A = 0.      !  Initialise arrays
 VERR = 0.
 FINAL = .FALSE.

 ITS = 0
 RSVT = MAX (BND, RSVT0)   ! Initialise eigenvalue damping at 10 percent
 WRITE(QL0,*) LINE(JL)
 READ(QL0,'(A)') CLINE           ! Line number

 WRITE(NW,1) JRL,TRIM(ADJUSTL(CLINE)),NDATA,MAXITS,KPCT(1)
 ! WRITE(*,1)  JRL,TRIM(ADJUSTL(CLINE)),NDATA,MAXITS,KPCT(1)
 KDEG = INT (SVAZM(JL) * 180. /3.14159)
 CPLT = PLT_PT(IPLT(JL))
 CALL GET_UNITS_TEXT (UNITS(JL),UTXT)
 WRITE(NP,18) JSTAT,TRIM (ADJUSTL (CLINE)),HEADER_ID(JL),KDEG,TRIM(UTXT(2)),TRIM (ADJUSTL (CPLT))

!----------------------------------------------------------------------
!  Start of main loop.  Call FORJAC to get error and Jacobian.
!  First, tranform physical model into transformed parameters.
!  Call ESVD to find the S.V.D. of the Jacobian.
!----------------------------------------------------------------------

 CALL CNVRT2_XPAR (NLYR,NPAR,RES,THK,XPAR)
 CALL CNVRT_BOUNDS (NLYR,NPAR,LBND,UBND,XLBND,XUBND)

 IPAR = 1
 PFAC = 0.01 * KPCT

 ITER_LOOP: DO ITS = 1, MAXITS
   PITS = ITS
   PARFAC = PFAC(IPAR)
   JCBN = .TRUE.
   CALL FORJAC (JL,JRL,JS,JR,NDATA,NPAR,NLINES,MLINES,MCMP,MRXL,MCHNL,NCHNL,NFRQ,NFRQHS,TDFD, &
                STEP,FREQ,FRQHS,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,TOPN,TCLS,NFT,CURNT, &
                ISYS,SVAZM,UNITS,RX_TYPE,SOURCE_TYPE,IDH,MXTX,NTX,MXVRTX,NVRTX,SXN,SXE,SXZ,   &
                SXDIP,SXAZM,NRXTX,MRXTX,RXID,MQVR,XRXTX,YRXTX,ZRXTX,MD1,MD2,RXAZM,RXDIP,NCTD, &
                KNORM2,MXRHO,RHOTRP,NLYR,RMUD,REPS,CHRG,CTAU,CFREQ,PARFAC,XPAR,BPRM,XDATA,    &
                RWTS,RMODL,XMODL,SUMSQ,JCBN,A,VERR,NW, &
                rxmnt)

   RMSERR = 100. * SQRT (SUMSQ / REAL (NDATA))
   FNM = 0.01 * SQRT (SUMSQ)
   FNM = MAX (FNM, ETA)
   MPAR(1:NPAR) = EXP (XPAR(1:NPAR))
   IF (ITS == 1) THEN
     rmsini = RMSERR
     WRITE(NP,28) RMSERR,RSVT
     WRITE(NP,29) MPAR(1:NPAR)
     WRITE(NW,4) RMSERR,RSVT; !WRITE(*,4) RMSERR,RSVT
     IF (INVPRT == 3) THEN
       WRITE(NW,9)
       IF (TDFD == 1) THEN
         CALL WRITE_INVDATA_TD (JL,JRL,NW,NP,NCHNL,NLINES,MRXL,MCMP,SURVEY_TYPE, &
                                LINE,IDH,RX_TYPE,YXZPLT,CMP,RMODL,RDATA,RWTS)
       ELSE
         CALL WRITE_INVDATA_FD (JL,JRL,NW,NP,NFRQ,MCHNL,NLINES,MRXL,MCMP,LINE,RX_TYPE, &
                                IDH,UNITS,ISYS,YXZPLT,FREQ,CMP,RMODL,RDATA,RWTS)
       END IF
     END IF
   END IF

!  Load the error vector into the NPAR+1 column of A.  On return from ESVD,
!  this column will contain the transformed error vector; i.e.,
!  VERR = U * VERR

   Do jd = 1, ndata
    Do jp = 1, npar
        a_orig(jd, jp) = a(jd, jp)
    End Do
   End Do
   A(1:NDATA,NPAR+1) = VERR(1:NDATA)
   CALL ESVD (A,NDATA,NPAR,IP,WITHU,WITHV,SV,UMAT,VMAT,WSP,ETA,TOL,NW)

   IF (ABS (SV(1)) < 1.E-30) THEN
     WRITE(NW,99); WRITE(*,99)
     RETURN
   END IF

   VERR(1:NDATA) = A(1:NDATA,NPAR+1)

!  Solve for the correction vector, and test for convergence on
!  predicted decrease.  Loop over internal iterations.

   ICNT_LOOP: DO ICNT = 0, MAXITS_INT
     CALL SOLVE2 (NPAR,RSVT,ZERO,VMAT,VERR,SV,NSV,DELPAR,WSP,SQPDRE)
     PDRE = SQRT (SQPDRE)
     IF (PDRE < FNM) THEN

!  Change KPCT or terminate if predicted residual decrease < 1 percent of RMS error

       IF (IPAR < NDSTP) THEN
         WRITE(NW,17) KPCT(IPAR); !WRITE(*,17) KPCT(IPAR)
         IPAR = IPAR + 1
         WRITE(NW,21) ;  !WRITE(*,21)
         IF (INVPRT > 2) THEN
           WRITE(NW,3) ITS,ICNT,RSVT,RMSERR
           ! WRITE(*,3) ITS,ICNT,RSVT,RMSERR
           Write (nw, *) VMAT
         END IF
         WRITE(NW,16) KPCT(IPAR) ; !WRITE(*,16) KPCT(IPAR)
         RSVT = MIN (RSVT, 0.1)
         CYCLE ITER_LOOP
       ELSE
         WRITE(NW,21) ;  !WRITE(*,21)
         WRITE(NW,15) ;  !WRITE(*,15)
         IF (INVPRT > 2)  THEN
           WRITE(NW,3) ITS,ICNT,RSVT,RMSERR
           ! WRITE(*,3) ITS,ICNT,RSVT,RMSERR
         END IF
!*******************************************
         EXIT ITER_LOOP
!*******************************************
       END IF
     END IF
     DO JP = 1,NPAR                            ! Update parameters
       SELECT CASE (CXPAR(JP))
       CASE (0)                                ! No constraints
         GXPAR(JP) = XPAR(JP) + DELPAR(JP)
       CASE (1)                                ! Fixed parameter
         GXPAR(JP) = XPAR(JP)
       CASE (2)
         GXPAR(JP) = XPAR(JP) + ELAS(JP) * DELPAR(JP)
       CASE (3)
         B1 = XLBND(JP)
         B2 = XUBND(JP)
         BMID = 0.5* (B1 + B2)
         IF (XPAR(JP) < B1 .OR. XPAR(JP) > B2 ) XPAR(JP) = BMID
         B3 = XPAR(JP) + ELAS(JP) * (B2 - XPAR(JP))
         B4 = XPAR(JP) + ELAS(JP) * (B1 - XPAR(JP))
         GXPAR(JP) = XPAR(JP) + DELPAR(JP)
         GXPAR(JP) = MIN (GXPAR(JP), B4)
         GXPAR(JP) = MAX (GXPAR(JP), B3)
       END SELECT
     END DO

!  Get the error for model with corrected parameters.  Test for improvement (decrease) in
!  residual on ratio of actual to computed decrease in error.  If it fails, reduce step
!  and try again.  Give up and return after MAXITS_INT "internal" iterations.

     JCBN = .FALSE.
     CALL FORJAC (JL,JRL,JS,JR,NDATA,NPAR,NLINES,MLINES,MCMP,MRXL,MCHNL,NCHNL,NFRQ,NFRQHS,TDFD, &
                  STEP,FREQ,FRQHS,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,TOPN,TCLS,NFT,CURNT, &
                  ISYS,SVAZM,UNITS,RX_TYPE,SOURCE_TYPE,IDH,MXTX,NTX,MXVRTX,NVRTX,SXN,SXE,SXZ,   &
                  SXDIP,SXAZM,NRXTX,MRXTX,RXID,MQVR,XRXTX,YRXTX,ZRXTX,MD1,MD2,RXAZM,RXDIP,NCTD, &
                  KNORM2,MXRHO,RHOTRP,NLYR,RMUD,REPS,CHRG,CTAU,CFREQ,PARFAC,GXPAR,BPRM,XDATA,   &
                  RWTS,RMODL,XMODL,SSQNEW,JCBN,A,VERR,NW, &
                  rxmnt)

     ERR1P = 100. * SQRT (SSQNEW / REAL (NDATA))
     IF (INVPRT > 2) WRITE(NW,2) ICNT,RSVT,ERR1P

!  Test if the reduction in squared error (GTEST) > 1 percent of predicted decrease

     GTEST = SUMSQ - SSQNEW
     IF (GTEST > GCRIT*SQPDRE) THEN   !  Error reduced using smaller step
       IF (ICNT == 0) THEN
         RSVT = RSVT / EXPND        !  Decrease eigenvalue threshold damping
         RSVT = MAX (BND, RSVT)     !  if this happens on the first pass
       END IF
!*******************************************
       EXIT ICNT_LOOP               !  Start next outer iteration
!*******************************************
     END IF
     RSVT = RSVT * EXPND            !  No error decrease. Raise threshold
     IF (ICNT == MAXITS_INT .OR. RSVT > 10.)  THEN
       IF (IPAR < NDSTP) THEN
         WRITE(NW,17) KPCT(IPAR)
         IF (INVPRT > 2) THEN
           WRITE(NW,22) ICNT, RSVT
           ! WRITE(*,22) ICNT, RSVT
         END IF
         IPAR = IPAR + 1
         WRITE(NW,16) KPCT(IPAR) ; !WRITE(*,16) KPCT(IPAR)
         RSVT = MIN (RSVT, 0.1)
         CYCLE ITER_LOOP
       ELSE
         WRITE(NW,22) ICNT, RSVT ; !WRITE(*,22) ICNT, RSVT
         WRITE(NW,15) ;  !WRITE(*,15)
!*******************************************
         EXIT ITER_LOOP
!*******************************************
       END IF
     END IF
   END DO ICNT_LOOP

!  Error reduced.  Accept step, write out summary.
!  Test MAXITS, and reduction with respect to PDRE, PCTCNV.

   XPAR(1:NPAR) = GXPAR(1:NPAR)
   FOLD = 100. * SQRT (SUMSQ / REAL (NDATA))
   FNEW = 100. * SQRT (SSQNEW / REAL (NDATA))
   MOVE = .FALSE.
   DELT = FOLD - FNEW
   IF (ITS > 10) THEN
     MOVE = .TRUE.
     IF (DELT + DELTOLD < 0.5) THEN
       WRITE(NW,14);  !WRITE(*,14)
     ELSE IF (DELT < 0.01* FOLD) THEN
       WRITE(NW,23); !WRITE(*,23)
     ELSE
       MOVE = .FALSE.
     END IF
   END IF
   SUMSQ = SSQNEW
   RMSERR = FNEW
   DELTOLD = DELT

!  If the predicted residual decrease < 1 percent of RMS error for two succesive
!  terminate inversion.
!  Else, write out the current model and continue iterating up until ITS = MAXITS.

   IF (MOVE) THEN
     IF (IPAR < NDSTP) THEN
       WRITE(NW,17) KPCT(IPAR)
       IPAR = IPAR + 1
       WRITE(NW,16) KPCT(IPAR) ; !WRITE(*,16) KPCT(IPAR)
       RSVT = MIN (RSVT, 0.1)
       CYCLE ITER_LOOP
     ELSE
       WRITE(NW,6) ITS,RMSERR,RSVT; !WRITE(*,6) ITS,RMSERR,RSVT
       WRITE(NW,15) ; !WRITE(*,15)
!*******************************************
       EXIT ITER_LOOP
!*******************************************
     END IF
   END IF
   IF (RMSERR < PCTCNV) THEN
     WRITE(NW,20) PCTCNV
     WRITE(NW,6) ITS,RMSERR,RSVT; !WRITE(*,6) ITS,RMSERR,RSVT
!*******************************************
     EXIT ITER_LOOP
!*******************************************
   END IF
   IF (ITS == MAXITS) THEN
     WRITE(NW,24)
     WRITE(NW,6) ITS,RMSERR,RSVT; !WRITE(*,6) ITS,RMSERR,RSVT
!*******************************************
     EXIT ITER_LOOP
!*******************************************
   END IF

   WRITE(NP,30) ITS,RMSERR,RSVT
   MPAR(1:NPAR) = EXP (XPAR(1:NPAR))
   WRITE(NP,31) ITS,MPAR(1:NPAR)
   CALL WRITE_INVMDL (NW,FINAL,RMSERR,RSVT,ITS,NLYR,NPAR,XPAR,RES,THK,DEPTH,IMPORT)
   IF (INVPRT ==3 ) THEN
     WRITE(NW,10) ITS
     IF (TDFD == 1) THEN
       CALL WRITE_INVDATA_TD (JL,JRL,NW,NP,NCHNL,NLINES,MRXL,MCMP,SURVEY_TYPE, &
                              LINE,IDH,RX_TYPE,YXZPLT,CMP,RMODL,RDATA,RWTS)
     ELSE
       CALL WRITE_INVDATA_FD (JL,JRL,NW,NP,NFRQ,MCHNL,NLINES,MRXL,MCMP,LINE,RX_TYPE, &
                              IDH,UNITS,ISYS,YXZPLT,FREQ,CMP,RMODL,RDATA,RWTS)
     END IF
   END IF
 

   CLOSE (NP)                                                            ! Flush Buffer
   OPEN(NP,FILE = 'Beowulf.mv1',STATUS = 'OLD', POSITION = 'APPEND')
 END DO ITER_LOOP   !  END OF MAIN LOOP.  Write final model and exit.

 IF (RMSERR > 15.) WRITE(NW,7)

 CALL PARAMETER_SENSITIVITY (NPAR,VMAT,SV,BND,IMPORT, DMPFAC)
 CALL WRITE_STATW (NW,NPAR,NLYR,VMAT, DMPFAC, 100.* BND)

 WRITE(NW,12)
 FINAL = .TRUE.
 CALL WRITE_INVMDL (NW,FINAL,RMSERR,RSVT,ITS,NLYR,NPAR,XPAR,RES,THK,DEPTH,IMPORT)
 WRITE(NP,32) ITS,RMSERR,RSVT
 MPAR(1:NPAR) = EXP (XPAR(1:NPAR))
 WRITE(NP,33) MPAR(1:NPAR)
!
!   eigenparameter analysis ...
 Write (NP, 46)
 Write (NP, 40) a_orig
 Write (NP, 42) dmpfac
 Write (NP, 34) import( 1: npar)
 Write (NP, 41) sv
 Write (NP, 36) vmat
 Write (NP, 38) umat
 IF (INVPRT > 1) THEN
   WRITE(NW,11) YXZPLT(1:3,JRL,JL)
   IF (TDFD == 1) THEN
     CALL WRITE_INVDATA_TD (JL,JRL,NW,NP,NCHNL,NLINES,MRXL,MCMP,SURVEY_TYPE, &
                            LINE,IDH,RX_TYPE,YXZPLT,CMP,RMODL,RDATA,RWTS)
  ELSE
    CALL WRITE_INVDATA_FD (JL,JRL,NW,NP,NFRQ,MCHNL,NLINES,MRXL,MCMP,LINE,RX_TYPE, &
                           IDH,UNITS,ISYS,YXZPLT,FREQ,CMP,RMODL,RDATA,RWTS)
   END IF
 END IF
 NL1 = NLYR-1
 WRITE(NR1,35) JSTAT,LINE(JL),YXZPLT(1:3,JRL,JL),PITS,RMSERR,RES(1:NLYR),DEPTH(1:NL1),THK(1:NL1)
 !
 ! report to screen
 Call date_and_time(Values = tvals)
 Write (*, 45) tvals(1:3), tvals(5:7), jstat, nstat, rmsini, RMSERR, pits

  1 FORMAT(//T3,'---------------------------------------------------------------' &
            /T3,'Begin Inversion for Station',I3,' of Line ',A,'.  NDATA =',I3    &
            /T3,'Maximum iterations =',I3,3X,'Derivative step =',I3,' percent.')
  2 FORMAT(/T11,'ICNT:',I3,3X,'RSVT:',F8.3,3X,'Test RMS error =',F9.2,' percent.')
  3 FORMAT(/T3,'ITS:',I3,3X,'ICNT:',I3,3X,'RSVT',F8.3,3X,'RMSERR =',F9.2)
  4 FORMAT(/T3,' 0 iterations completed:  RMS error =',F8.2,' percent.  RSVT =',F8.3)
  6 FORMAT(/I4,' Iterations completed:  RMS error =',F8.2,' percent.  RSVT =',F8.3)
  7 FORMAT(/T3,'An alternative starting guess might achieve better results.')
  9 FORMAT(/T3,'Initial Data and Misfit')
 10 FORMAT(/T3,'Data and Misfit prior to Iteration',I3)
 11 FORMAT(/T3,'Data and Misfit Final - (East, North, Elevation) = ',3(2x, g13.4, : ','))
 12 FORMAT(//T3,50('='))
 14 FORMAT(/T3,'Error reduction after last 2 iterations < 0.5 percent.')
 15 FORMAT(T3,'Inversion terminated')
 16 FORMAT(T3,'Test derivative step =',I3,' percent.')
 17 FORMAT(T3,'No further error reduction can occur using a',I3,' percent derivative step.')
 18 FORMAT(/T3,'Station',I4,4X,'Line ',A,4X,'HID:',I4,4X,'SVAZ:',I4,4X,'Units: ',A,4X,'PP: ',A)
 20 FORMAT(/T3,'Convergence within RMS error threshold of',F7.2,' has been achieved.')
 21 FORMAT(/T3,'Predicted residual decrease < 1 percent of RMS error.')
 22 FORMAT(/T3,'No significant error reduction during internal iterations.' &
           /T3,'ICNT =',I3,';  RSVT =',G13.4)
 23 FORMAT(/T3,'Error reduction < 1 percent of RMS error.')
 24 FORMAT(//T3,'Inversion finished after maximum number of iterations')
 28 FORMAT(T1,'/'/T1,'/ ITERATION  00'/T1,'/ PERCENT_RMS_ERROR:',F9.2,3X,'RSVT:',F8.3)
 29 FORMAT(T1,'/'/T1,'/ MODEL_00',84F13.2)
 30 FORMAT(T1,'/'/T1,'/ ITERATION  ',I2.2/T1,'/ PERCENT_RMS_ERROR:',F9.2,3X,'RSVT:',F8.3)
 31 FORMAT(T1,'/'/T1,'/ MODEL_',I2.2,84F13.2)
 32 FORMAT(T1,'/'/T1,'/ FINAL_ITERATION  ',I2.2/T1,'/ FINAL_PERCENT_RMS_ERROR:',F8.2,3X,'RSVT:',F7.3)
 33 FORMAT(T1 '/ FINAL_MODEL: ', 5x, 8192(2x, e15.8))
 34 FORMAT(T1,'/ Importance: ', 5x, 8192(2x, f9.5))
 35 FORMAT(I7,I9,3F14.4,I6,F8.2,60G13.4)
 42 Format (t1, '/ Damping_Factors: ', 1024(2x, f9.5))
 36 Format (t1, '/ V-matrix: ', 7x, 2048(2x, f9.5))
 37 Format (t1, '/ ', /, &
            t1, '/ ', 2014('^'))
 38 Format (t1, '/ U-matrix: ', 7x, 8192(2x, f9.5))
 40 Format (t1, '/ Jacobian: ', 8x, 8192(2x, e15.8))
 41 Format (t1, '/ Singular_values: ', 1024(2x, f9.5))
 45 Format (2x, i4.4, '-', i2.2, '-', i2.2, 'T', i2.2, ':', i2.2, ':', i2.2, &
            ': Station ', i4, ' of ', i4, &
            ' initial error = ', f6.2, '%; final error = ', f6.2, '%; ', i2, ' iterations')
 46 Format (t1, '/')
 99 FORMAT(//T3,'Singular value decomposition failure.  INVERSION HALTED')

END SUBROUTINE NLSQ2

 SUBROUTINE FORJAC (JL,JRL,JS,JR,NDATA,NPAR,NLINES,MLINES,MCMP,MRXL,MCHNL,NCHNL,NFRQ,NFRQHS,TDFD, &
                    STEP,FREQ,FRQHS,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,TOPN,TCLS,NFT,CURNT, &
                    ISYS,SVAZM,UNITS,RX_TYPE,SOURCE_TYPE,IDH,MXTX,NTX,MXVRTX,NVRTX,SXN,SXE,SXZ,   &
                    SXDIP,SXAZM,NRXTX,MRXTX,RXID,MQVR,XRXTX,YRXTX,ZRXTX,MD1,MD2,RXAZM,RXDIP,NCTD, &
                    KNORM2,MXRHO,RHOTRP,NLYR,RMUD,REPS,CHRG,CTAU,CFREQ,PARFAC,XPAR,BPRM,XDATA,    &
                    RWTS,RMODL,XMODL,SUMSQ,JCBN,A,VERR,NW, &
                    rxmnt)
!-------------------------------------------------------------------------------------------

!  Sets up and implements layered earth computation for receiver JR of transmitter JS corresponding
!  to station JRL of line JL.  It also calculates the Jacobian and error vector if required.
!  New convention: Dec, 2003: VERR is now VD - VM
!  Thus DELPAR is now added rather than subtracted during updates.
!
!***  Called by: NLSQ2
!***      Calls: GET_FWD_MODL, CNVRT2_MPAR
!
!             Output
!             ------
!
!       RMODL : 4D model response ((NCHNL,MRXL,MCMP,MLINE)
!       XMODL : 1D model response containing all response not weighted to 0 by RWTS
!       SUMSQ : sum squared scaled error
!       JCBN  : true if Jacobian required
!       A     : a large array which carries the Jacobian out of this routine
!       VERR  : scaled error
!
!             General Inversion Input Variables
!             ---------------------------------
!
!       NDATA : number of data points not weighted to zero either explicitly,
!               through CMP,or using data floors
!       RWTS  : 4D weights (0 or 1) applied to RMODL (0 or 1) to create XMODL
!       XDATA : inversion data in 1D form containing all data not weighted to 0
!       NPAR  : number of parameters to be inverted
!       XPAR  : array of transformed model parameters
!
!             System Input Variables
!             ----------------------
!
!        TDFD = 2 for FD; 1 for TD; 0 for user controlled FD -> TD comversion
!        FREQ : array of NFRQ frequencies for which 3D response is computed
!      FREQHS : array of NFRQHS frequencies for halfspace time domain conversion
!       NCHNL : number of time domain channels
!       MCHNL = total number of readings per station to be inverted (TD or FD)
!             = NCHNL * number of components for time domain
!             = 2* NFRQ * number of components for frequency domain
!        STEP = 1 iff step response is to be computed
!         NSX : number of points used to discretise transmitter signal
!         SWX : abscissae (seconds) of current waveform
!         SWY : dI/dt * Tx moment & nanotesla conversion at times SWX
!         TRP : array of NTYRP time values for FD -> TD transformations
!       NPULS : number of bipolar pulses of length PULSE
!      NTYPLS : number of TRP values in 1 PULSE
!        TOPN : time at which receiver channel I opens.
!        TCLS : time at which receiver channel I closes.
!         NFT = 1 for time domain; = NFRQ for frequency domain (used for CURNT)
!       CURNT : current
!        ISYS = 2 for Sampo;  4 for UTEM;  0 otherwise
!
!             Survey Input Variables
!             ----------------------


!        NLINES : number of lines of data
!          MCMP = 1 for Sampo and coincident loop, 3 otherwise
!          MRXL : maximum number for receivers per line
!         SVAZM : survey azimuth for each line
!         UNITS : units for each line
!       RX_TYPE : receiver type for each line(1 or 2)
!   SOURCE_TYPE : closed loop, open loop (grounded ends) magnetic dipole, or plane wave
!           IDH : identifies standard or Utem downhole processing where reauired
!           NTX : number of distinct transmitter positions (max = MXTX)
!         NVRTX : number of vertices for source loop (maximum = MXVRTX)
!      SXN, SXE : north and east coordinates of loop vertices or magnetic dipole Tx
!           SXZ : source depth
!  SXDIP, SXAZM : dipole Tx dip and azimuth
!         NRXTX : number of receivers per transmitter (maximum = MRXTX)
!          RXID : type of receiver for a given transmitter
!          MQVR : maximum number of vertices for all receivers (1 or 2)
!  XRXTX(I,J,K) : north coordinate of the Kth vertex of the Ith receiver of transmitter J
!  YRXTX(I,J,K) : east coordinate of the Kth vertex of the Ith receiver of transmitter J
!    ZRXTX(I,J) : depth of the Ith receiver of transmitter J
!      MD1, MD2 : (MRXL, NLINES) for DH receivers; (1,1) otherwise
!  RXDIP, RXAZM : magnetic dipole receiver dip and azimuth
!        KNORM2 : normalisation indicator for receiver JR of transmitter JS
!         NCTD = 3 for magnetic dipole and point electric receivers
!               = 1 for electric dipole and coincident loop receivers
!
!             Model Description Input Variables
!             ---------------------------------
!
!      RHOTRP : Array (size MXRHO) for horizontal spatial interpolation
!        NLYR : number of layers
!        RMUD : enhanced precision mu(i) / mu(0)
!        REPS : relative dielectric constant
!        CHRG, CTAU & CFREQ are the layered earth Cole-Cole parameters.
!
!          PHYSICAL PARAMETER 0RDERING
!          ---------------------------
!      1:NLYR           : layer resistivities
!      NLYR + 1:NLYR-1  : layer thicknesses
!
!
!          TRANSFORMED PARAMETERS FOR INVERSION
!          ------------------------------------
!
!   RES & THK are represented logarithmically


 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 INTEGER NDATA,NPAR,NLINES,MCMP,MRXL,MCHNL,NCHNL,NFRQ,NFRQHS,NFT,TDFD,STEP,NSX,NPULS,NTYPLS,   &
         NTYRP,ISYS,SOURCE_TYPE,MXTX,NTX,MXVRTX,NVRTX(MXTX),NRXTX(MXTX),MRXTX,NCTD(MRXTX,NTX), &
         RXID(MRXTX,NTX),MQVR,MD1,MD2,KNORM2(MRXTX,NTX),MXRHO,MLINES,NW,LP,J1,JL,JRL,JS,JR,    &
         JC,JD,JF
 INTEGER, DIMENSION(NLINES) :: RX_TYPE,UNITS,IDH
 INTEGER RWTS(MCHNL,MRXL,MCMP,MLINES)
 REAL FREQ(NFRQ),FRQHS(NFRQHS),SWX(NSX),SWY(NSX,3),PULSE,TRP(NTYRP),TOPN(NCHNL),TCLS(NCHNL),   &
      CURNT(NFT),XRXTX(MRXTX,NTX,MQVR),YRXTX(MRXTX,NTX,MQVR),ZRXTX(MRXTX,NTX),SXN(MXVRTX,NTX), &
      SXE(MXVRTX,NTX),SXZ(MXTX),SXDIP(MXTX),SXAZM(MXTX),RHOTRP(MXRHO),XPAR(NPAR),SUMSQ,  &
      A(NDATA,NPAR+1),BTD(NCHNL,MRXTX,NTX,3),BPRM(MRXTX,NTX),      &
      SVAZM(NLINES),VM,VJ,VD,DENOM,V1,DELTA,XP0,PARFAC
 REAL, DIMENSION(MCHNL,MRXL,MCMP,MLINES) :: BFTL,RMODL
 REAL, DIMENSION(MD1,MD2) :: RXAZM,RXDIP
 REAL, DIMENSION(NDATA) :: XMODL,XMODL0,XDATA,VERR
 COMPLEX, DIMENSION(NFRQ,MRXTX,NTX,3) :: BFD
 LOGICAL JCBN

! Model dependent variables

 INTEGER NLYR
 REAL, DIMENSION(NLYR) :: THK,RES,REPS,CHRG,CTAU,CFREQ
 REAL(KIND=QL) RMUD(0:NLYR)

 Real :: rxmnt(nlines)

! Compute initial model & compute error

 CALL CNVRT2_MPAR (NLYR,NPAR,RES,THK,XPAR)

 LP = 0;
 CALL GET_FWD_MODL(LP,PARFAC)
 XMODL0 = XMODL
 RMODL(1:MCHNL,JRL,1:MCMP,JL) = BFTL(1:MCHNL,JRL,1:MCMP,JL)

 SUMSQ = 0.
 DO JD = 1, NDATA
   VERR(JD) = 0.
   VD = XDATA(JD)
   VM = XMODL0(JD)
   DENOM = SQRT( (VM*VM + VD*VD)/2.0)
   V1 = (VD-VM) / DENOM        !
   SUMSQ = SUMSQ + V1**2
   VERR(JD) = V1
 END DO

!  Initialise and then compute the Jacobian as the derivative of log(volts) wrt
!  log(parameter) for a three percent step.  Skip over held parameters

 IF (JCBN) THEN
   A = 0.
   DO J1 = 1,NLYR
     LP = J1
     XP0 = RES(J1)
     RES(J1) = (1. + PARFAC) * RES(J1)
     CALL GET_FWD_MODL(LP,PARFAC)
     RES(J1) = XP0
   END DO
   DO J1 = 1,NLYR-1
     LP = NLYR + J1
     XP0 = THK(J1)
     THK(J1) = (1. + PARFAC) * THK(J1)
     CALL GET_FWD_MODL(LP,PARFAC)
     THK(J1) = XP0
   END DO
 END IF

 CONTAINS

   SUBROUTINE GET_FWD_MODL(LP,PARFAC)
!  ----------------------------------

!***  Called by: FORJAC
!***      Calls: HSBOSS_TD, HSBOSS_FD

!  If LP = 0, performs model computation using existing parameters
!  If LP > 0, performs model computation where parameter LP is multiplied by
!             PARFAC and Jacobian column LP isconstructed.

 INTEGER LP
 REAL PARFAC
 REAL(KIND=QL) THKD(NLYR)

 THKD = REAL (THK,KIND=QL)

       !  Construct frequency-domain output
 IF (TDFD == 2) THEN  !  Construct the frequency-domain response

   CALL HSBOSS (NFRQ,FREQ,SOURCE_TYPE,NTX,MXVRTX,NVRTX,SXN,SXE,SXZ,SXDIP,SXAZM, &
                NRXTX,MRXTX,RXID,MQVR,XRXTX,YRXTX,ZRXTX,MXRHO,RHOTRP,NLYR,THKD, &
                RES,RMUD,REPS,CHRG,CTAU,CFREQ,JS,JR,BFD)

!  Redefine BFD as the total response.
!  Reconfigure output from Tx to Line basis.

   CALL SET_OUTPUT_LINES_FD (JL,JRL,JS,JR,NFRQ,MCHNL,NTX,MRXTX,NLINES,MRXL,MCMP,KNORM2,UNITS, &
                             RX_TYPE,ISYS,IDH,SVAZM,MD1,MD2,RXAZM,RXDIP,CURNT,BPRM,BFD,BFTL, &
                             RXMNT)

 ELSE IF (TDFD == 1) THEN   ! Time-Domain.


!  Compute BTD, the layered earth response convolved with the excitation waveform

   CALL HSBOSS_TD (NFRQHS,FRQHS,STEP,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL, &
                   TOPN,TCLS,SOURCE_TYPE,NTX,MXVRTX,NVRTX,SXN,SXE,SXZ,SXDIP,SXAZM,   &
                   NRXTX,RXID,MRXTX,MQVR,XRXTX,YRXTX,ZRXTX,MXRHO,RHOTRP,NLYR,THKD,   &
                   RES,RMUD,REPS,CHRG,CTAU,CFREQ,NCTD,JS,JR,BTD)

!  Redefine BTD as the total response.
!  Reconfigure output from Tx to Line basis.

   CALL SET_OUTPUT_LINES_TD (JL,JRL,JS,JR,NCHNL,NTX,MRXTX,NLINES,MRXL,MCMP,KNORM2,RX_TYPE, &
                             UNITS,ISYS,IDH,SVAZM,MD1,MD2,RXAZM,RXDIP,CURNT,BPRM,BTD,BFTL, &
                             RXMNT)
 END IF

 JD = 0
 DO JC = 1,MCMP
   DO JF = 1,MCHNL
     IF (RWTS(JF,JRL,JC,JL) > 0) THEN
       JD = JD + 1
       XMODL(JD) = BFTL(JF,JRL,JC,JL)
     END IF
   END DO
 END DO
 IF (JD /= NDATA) THEN
   WRITE(*,1) JD,NDATA
   WRITE(NW,1) JD,NDATA
   STOP
 END IF

 IF (LP > 0) THEN
   DO JD = 1, NDATA
     VM = XMODL0(JD)
     VJ = XMODL(JD)
     DELTA = (VJ - VM)
     DENOM = SQRT( (VM*VM + VJ*VJ)/2.0)
     IF (ABS(DELTA) > 1.E-8 * DENOM) A(JD,LP) = DELTA / (PARFAC * DENOM)
   END DO
 END IF

1 FORMAT(/T1,'********************************************', &
         /T3,'Problem in SUBROUINE FORJAC: JD /= NDATA',     &
         /T3,'JD =',I5,4X,'NDATA =',I5,                      &
         /T3,'EXECUTION HALTED !',                           &
         /T1,'********************************************')
   END SUBROUTINE GET_FWD_MODL

 END SUBROUTINE FORJAC

 SUBROUTINE CNVRT2_MPAR (NLYR,NPAR,RES,THK,XPAR)
!-----------------------------------------------

!*** Called by: NLSQ2

!  Converts XPAR to model parameters
!
!      NLYR : number of layers
!      NPAR : 2*NLYR-1
!       RES : layer resistivities
!       THK : layer thicknesses
!      XPAR : transformed parameters


! The parameters are ordered by layer resistivities followed by thicknesses.
!
!      1 : NLYR : resistivities
!      NLYR + J  - thickness oflayer J
!
!
!          TRANSFORMED PARAMETERS FOR INVERSION
!          ------------------------------------
!
!   RES & THK are represented logarithmically

 INTEGER  NLYR,NPAR,JL
 REAL XPAR(NPAR),RES(NLYR),THK(NLYR)

 RES(1:NLYR) = EXP (XPAR(1:NLYR))
 DO JL = 1,NLYR-1
   THK(JL) = EXP (XPAR(NLYR+JL))
 END DO
 THK(NLYR) = 1.E5

 END SUBROUTINE CNVRT2_MPAR

 SUBROUTINE CNVRT2_XPAR (NLYR,NPAR,RES,THK,XPAR)
!-----------------------------------------------

!*** Called by: NLSQ2

!  Converts from model parameters to inversion parameters

!      NLYR : number of layers
!      NPAR : 2*NLYR-1
!       RES : layer resistivities
!       THK : layer thicknesses
!      XPAR : transformed parameters


! The parameters are ordered:

!      1 : NLYR : resistivities
!      NLYR + J  - thickness oflayer J
!
!
!          TRANSFORMED PARAMETERS FOR INVERSION
!          ------------------------------------
!
!   RES & THK are represented logarithmically

 INTEGER NLYR,NPAR,JL
 REAL RES(NLYR),THK(NLYR),XPAR(NPAR)

 XPAR(1:NLYR) = LOG (RES(1:NLYR))
 DO JL = 1,NLYR-1
   XPAR(NLYR+JL) = LOG (THK(JL))
 END DO

 END SUBROUTINE CNVRT2_XPAR

 SUBROUTINE CNVRT_BOUNDS (NLYR,NPAR,LBND,UBND,XLBND,XUBND)
!---------------------------------------------=-----------

!*** Called by NLSQ2

! Converts the LBND & UBND specified by the user into the form used
! by the inversion, XLBND & XUBND.

! Bounds are only applied to plate or layer parameters for CXPAR = 3

!  INPUT:  NLYR,NPAR,LBND,UBND,CXPAR
! OUTPUT:  XLBND,XUBND

 INTEGER NLYR,NPAR,J1
 REAL,DIMENSION(NPAR) :: LBND,UBND,XLBND,XUBND

 DO J1 = 1, 2*NLYR - 1
   XLBND(J1) = LOG (LBND(J1))
   XUBND(J1) = LOG (UBND(J1))
 END DO

END SUBROUTINE CNVRT_BOUNDS

 SUBROUTINE ESVD (AJAC,NDATA,NPAR,KP,WITHU,WITHV,SV,UMAT,VMAT,WSP,ETA,TOL,NW)
!----------------------------------------------------------------------------

!***  Called by: NLSQ2
!***      Calls: DPROD1, WRITE_MESSAGE
!
!==ESVD.spg  processed by SPAG 4.51A  at 17:56 on 17 Mar 1995
!
!  Singular value decomposition based on Golub's method with an option of
!  least squares reduction of RHS vectors for under or over determined
!  problems
!
!  REFERENCE:  G.H. Golub & C. Reinsch,
!             'Singular value decomposition and least squares solutions'
!              Num. Math.14,403-420(1970) ... (algol language) features both
!              SVD and minfit with accumulation in double precision
!
!     This routine is based on a program written by P. Businger
!     of Bell Telephone Lab., but incorporates a few modifications
!     due to R. Underwood of Stanford University, and D.L.B. Jupp
!==============================================================================

!          AJAC -  NDATA*(N+KP) array containing matrix to be decomposed
!         NDATA -  Number of data channels to be inverted
!          NPAR -  Number of parameters to be inverted.
!            KP -  If KP > 0 columns NPAR+1, ... ,NPAR+KP of AJAC contain KP
!                  'right hand sides'. these are multiplied by the transpose
!                  of UMAT for use in SOLVE2 (accompanying routine)
!   WITHU,WITHV -  logical control variables governing whether or not UMAT
!                  and VMAT respectively are constructed
!            SV -  on return SV(1) ... SV(N) contain the ordered singular
!                  values of AJAC. (SV(1) > SV(2) >  ...  > SV(NPAR))
!     UMAT,VMAT -  on return contain data and parameter space eigenvectors
!                  of AJAC. (AJAC =UMAT*SV*VMATtr)  depending upon the truth
!                  of WITHU and WITHV
!           WSP -  a workspace array of length 3*NPAR
!     ETA,TOL are machine dependent constants
!     ETA is the relative precision of real numbers
!     TOL is the smallest representable number divided by ETA
!
!     EXTERNAL REFERENCES ... DPROD1
!==================================================================

 IMPLICIT NONE
 INTEGER, PARAMETER :: ITMAX=30
 INTEGER I,IM1,ITER,J,K,KK,KP,K1,L,LL,L1,MKP,MP1,NDATA,NMK,NPI,NPK,NPL,NPAR,NP, &
         NW,N1,N2
 REAL CS,EPS,ETA,F,FAC,FTEMP,G,H,Q,R,SN,TOL,W,X,Y,Z
 LOGICAL WITHU,WITHV
 REAL AJAC(NDATA,NPAR+1),UMAT(NDATA,NPAR),VMAT(NPAR,NPAR),SV(NPAR),WSP(3*NPAR),DPROD1
 EXTERNAL DPROD1

 NP = NPAR + KP
 N1 = NPAR + 1
 N2 = NPAR + NPAR
 WSP(N1) = 0.
 K = 1

!  Householder reduction to upper bidiagonal form

50 K1 = K + 1
   NPK = NPAR + K1

!  Row transformation

   Z = 0.0
   MKP = NDATA - K + 1
   NMK = NPAR - K
   ! Z = DPROD1(MKP,1,1,Z,AJAC(K,K),AJAC(K,K))
   z = ajac(k, k) * ajac(k, k)
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
         ! Q = DPROD1(MKP,1,1,Q,AJAC(K,K),AJAC(K,J))
         q = ajac(k, k) * ajac(k, j)
         Q = Q/FAC
         AJAC(K:NDATA,J) = AJAC(K:NDATA,J) - Q*AJAC(K:NDATA,K)
       END DO

!  Phase transformation
       IF ( AJAC(K,K) > 0.0 ) AJAC(K,K1:NP) = -AJAC(K,K1:NP)
     END IF
   END IF

   IF ( K == NPAR ) THEN

!  End of householder reduction.  Set tolerance for iteration.

     EPS = 0.0
     DO K = 1,NPAR
       NPK = N2 + K
       NPL = NPAR + K
       SV(K) = WSP(K)
       WSP(NPK) = WSP(NPL)
       EPS = MAX(EPS,SV(K) + WSP(NPK))
     END DO
     EPS = EPS*ETA

!  Set UMAT, and VMAT, to identity and preset pad of zero's
!  for case NDATA < NPAR.

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

!  Main iteration loop on K ... Q-R algorithm due to Francis

     DO KK = 1,NPAR
       K = N1 - KK
       NPK = N2 + K
       ITER = 0
120    LOOP1: DO LL = 1,K
         L = K + 1 - LL
         NPL = N2 + L
         IF ( ABS(WSP(NPL)) <= EPS ) GOTO 160
         IF ( ABS(SV(L - 1)) <= EPS ) EXIT LOOP1
       END DO LOOP1

!  Cancellation

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

!  Test for convergence

160    W = SV(K)
       IF ( L/=K ) THEN

!     TEST FOR MAX ITERATIONS

         ITER = ITER + 1
         IF ( ITER <= ITMAX ) THEN

!  Compute the implicit shift of origin from bottom 2x2 minor

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

!  Main loop Q-R transformation for SV(K)

           DO I = L1,K
             IM1 = I - 1
             NPI = N2 + I
             G = WSP(NPI)
             Y = SV(I)
             H = SN*G
             G = CS*G

!  Givens rotation from the right

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

!  Givens rotation from the left

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
           WRITE(NW,'(//T3,A,I3)') ' MESSAGE FROM ESVD: Maximum iterations exceeded for singular value.',K
         END IF
       END IF

!  Convergence ... if singular value negative, make it positive

       IF ( W < 0.0 ) THEN
         SV(K) = -W
         IF ( WITHV ) VMAT(1:NPAR,K) = -VMAT(1:NPAR,K)
       END IF
     END DO

!  End of main loop.  Order singular values, UMAT, VMAT, and RHS'S.

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

!  Update umat with stored Householder transformations

     IF ( WITHU ) THEN
       DO KK = 1,NPAR
         K = N1 - KK
         IF ( ABS(WSP(K)) > TOL ) THEN
           IF ( K <= NDATA ) THEN
             MKP = NDATA - K + 1
             FAC = ABS(AJAC(K,K))*WSP(K)

!  Undo the phase
             IF ( AJAC(K,K) > 0.0 ) UMAT(K,1:NPAR) = -UMAT(K,1:NPAR)
             DO J = 1,NPAR
               Q = 0.0
               ! Q = DPROD1(MKP,1,1,Q,AJAC(K,K),UMAT(K,J))
               q = ajac(k, k) * umat(k, j)
               Q = Q/FAC
               UMAT(K:NDATA,J) = UMAT(K:NDATA,J) - Q*AJAC(K:NDATA,K)
             END DO
           END IF
         END IF
       END DO
     END IF

!  Update VMAT with stored householder transformations

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

!  Undo the phase

               IF ( AJAC(K,K1) > 0.0 ) VMAT(K1,1:NPAR) = -VMAT(K1,1:NPAR)
               DO J = 1,NPAR
                 Q = 0.0
                 ! Q = DPROD1(NMK,NDATA,1,Q,AJAC(K,K1),VMAT(K1,J))
                 q = ajac(k, k1) * vmat(k1, j)
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

!  Column transformation

     Z = 0.0
     ! IF ( K <= NDATA ) Z = DPROD1(NMK,NDATA,NDATA,Z,AJAC(K,K1),AJAC(K,K1))
     If (k .le. ndata) z = ajac(k, k1) * ajac(k, k1)
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
           ! Q = DPROD1(NMK,NDATA,NDATA,Q,AJAC(K,K1),AJAC(I,K1))
           q = ajac(k, k1) * ajac(i, k1)
           Q = Q/FAC
           AJAC(I,K1:NPAR) = AJAC(I,K1:NPAR) - Q*AJAC(K,K1:NPAR)
         END DO

!  Phase transformation
         IF ( AJAC(K,K1) > 0.0 ) AJAC(K1:NDATA,K1) = -AJAC(K1:NDATA,K1)
       END IF
     END IF
     K = K1
     GOTO 50
   END IF

END SUBROUTINE ESVD

 SUBROUTINE SOLVE2 (NPAR,RSVT,ZERO,VMAT,R,SV,NSV,DELPAR,WSP,SQPDRE)
!-----------------------------------------------------------------

!*** Called by: NLSQ2
!***     Calls: DPROD1

!  Calculates the new parameter changes by taking the product of
!  the transformed error vector times the damping factors (second order
!  Marquardt damping) times the inverse singular value matrix.
!  It also calculates the predicted error decrease.
!
!           Input
!           -----
!
!   NPAR :  the number of unknown parameters
!   RSVT :  the relative singular value threshold for the current iteration.
!   ZERO :  rejection level for relative singular values; i.e., truncation
!           instead of damping for RSV less than zero.  in NLSQ2, ZERO is
!           set to the square of the minimum allowed value of RSVT.
!   VMAT :  is the (NPAR * NPAR) V matrix of parameter space eigenvectors
!           output by ESVD.
!   R    :  the transformed error vector output by ESVD.  It is the product
!           of the transposed U matrix times the observed error vector.
!   SV   :  the (ordered) array of NPAR singular values returned by ESVD.
!           SV(1) > SV(2) > SV(3) etc.

!           Output
!           ------
!
!   NSV    : the returned number of singular values used in the solution.
!   DELPAR : the returned solution ( parameter changes)
!   WSP    : WSP (1:NPAR) contains the squared r vector
!            WSP (NPAR: 2*NPAR) contains the damping factors.
!   SQPDRE : the returned predicted decrease in residual squared error
!

 INTEGER NSV,I,NPAR
 REAL DPROD1,EIGPAR(NPAR),R(NPAR),SV(NPAR),DELPAR(NPAR),WSP(3*NPAR), &
      VMAT(NPAR,NPAR),Q,DMPFAC,SQPDRE,RSVT,ZERO
 EXTERNAL DPROD1

 INTENT (IN) NPAR,RSVT,ZERO,VMAT,R,SV
 INTENT (OUT) NSV,DELPAR,WSP,SQPDRE

 NSV = 0
 DO I = 1,NPAR
   WSP(I) = 0.
   WSP(NPAR+I) = 0.
   EIGPAR(I) = 0.
   Q = SV(I) / SV(1)
   IF (Q <= ZERO) CYCLE
   NSV = NSV + 1
   IF (Q < RSVT) THEN
     Q = (Q/RSVT)**4
     DMPFAC = Q/(1.0 + Q)
   ELSE
     DMPFAC = 1.0/(1.0 + (RSVT/Q)**4)
   END IF

!  Eigenparameter calculation.  store the damping factors in WSP(NPAR+I)

   EIGPAR(I) = R(I) * (DMPFAC / SV(I))
   WSP(NPAR+I) = DMPFAC
 END DO

!  Calculate change in physical parameters from eigenparameters.

 DO I = 1,NPAR
   ! DELPAR(I) = DPROD1 (NPAR,NPAR,1,0.0,VMAT(I,1),EIGPAR)
   delpar(i) = vmat(i, 1) * eigpar(i)
   WSP(I) = R(I) * R(I)
 END DO
 ! SQPDRE = DPROD1(NPAR,1,1,0.0,WSP(1),WSP(NPAR+1))
 sqpdre = wsp(1) * wsp(npar + 1)

END SUBROUTINE SOLVE2

 SUBROUTINE PARAMETER_SENSITIVITY (NPAR,VMAT,SV,BND,IMPORT, DMPFAC)
!----------------------------------------------------------

!***  Called by NLSQ2

!  Compute IMPORTANCE = parameter sensitivity for each parameter.  This is simply
!  an RMS rotation of the damping factors rotated into physical parameter space

!***  Called by: NLSQ2

!     NPAR - number of parameters
!     VMAT - the NPAR * NPAR matrix from SVD of Jacobian
!       SV - ordered singular values
!      BND - set minimum singular value threshold
!   IMPORT - importance  (damping factors rotated into physical parameter space.


 IMPLICIT NONE
 REAL, PARAMETER :: ETA=1.E-7
 INTEGER NPAR,J1,J2
 REAL VMAT(NPAR,NPAR),BND,SVNR,SVNR4,CUM,EP4,EPSQ
 REAL, DIMENSION(NPAR) :: SV,IMPORT,DMPFAC

!  Normalise the singular values and set the damping factors for the
!  second order Marquardt method.
!  The threshold level is set at that used in NLSQ2

 DMPFAC = 0.0
 EPSQ = BND * BND
 EPSQ = MAX (EPSQ, ETA)
 EP4 = EPSQ * EPSQ

 DO J1 = 1, NPAR
   SVNR = SV(J1) / SV(1)
   SVNR = MAX (SVNR, ETA)
   SVNR4 = SVNR**4
   IF (SVNR > EPSQ) DMPFAC(J1) = SVNR4 / (SVNR4 + EP4)
 END DO

 DO J1 = 1, NPAR
   CUM = 0.0
   DO J2= 1, NPAR
     CUM = CUM + (VMAT(J1,J2) * DMPFAC(J2))**2
   END DO
   IMPORT(J1) = SQRT (CUM)
 END DO

END SUBROUTINE PARAMETER_SENSITIVITY

 REAL FUNCTION DPROD1 (N,N1,N2,A,B,C)
!------------------------------------

!***  Called by: ESVD, SOLVE2

!     Double precision inner product routine
!     DPROD = A + B * C

!         A = scalar
!       B,C = vectors (can be rows or columns of arrays)
!         N = length of vectors
!     N1,N2 = increment for b,c
!           = 1 if col of array
!           = col length (i.e. no. of rows) if row of array

!  DPROD must be declared external by any routine using it because there is
!  a standard intrinsic function with the same name.
!  If omitted compilation warnings result.

 IMPLICIT NONE
 INTEGER N,NA,NB,N1,N2,I
 DOUBLE PRECISION Z1,Z2,Z3
 REAL A,B(1), C(1)

 INTENT (IN) N,N1,N2,A,B,C

 Z1=A
 IF (N >= 1) THEN
   NA=1
   NB=1
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

SUBROUTINE WRITE_STATW (NW,NPAR,NLYR,VMAT,DMPFAC,BDD)
!---------------------------------------------------------

!  Write out the V matrix and damping factors

!***  CALLED BY STATS

!     VMAT - V matrix
!   DMPFAC - damping factors
!      BDD - relative singular value threshold
!     NLYR - number of layers
!     NPAR - number of parameters

 IMPLICIT NONE
 INTEGER NW,NPAR,NLYR !,IWHZ !,NLM1 !,K1,K2,K3,K4,I,J
 REAL VMAT(NPAR,NPAR),DMPFAC(NPAR),BDD
 Integer :: jp, pp

!
!   just dump everything on a line ...
Write (nw, 20) bdd
Write (nw, 50) (jp, jp = 1, npar)
Do jp = 1, NLYR
    Write (nw, 51) jp, (vmat(jp, pp), pp = 1, npar)
End Do   
Do jp = nlyr + 1, npar
    Write (nw, 52) jp-nlyr, (vmat(jp, pp), pp = 1, npar)
End Do 
Write (nw, 54) (dmpfac(jp), jp = 1, npar)

 20 Format (/, 2x 'Parameter-space eigenvectors & damping factors @ ', f8.4, ' % level', /)
 50 Format (14x, 1024(8x, 'EP_', i3.3: ))
 51 Format (2x, 'Log(Rho_', i3.3, ')', 1024(6x, f8.5))
 52 Format (2x, '  Log(k_', i3.3, ')', 1024(6x, f8.5))
 54 Format (7x, 'Damping', 1024(6x, f8.5))

END SUBROUTINE WRITE_STATW

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
