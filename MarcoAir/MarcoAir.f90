!   PROGRAM MarcoAir
!------------------------------------------------------------------------------------
Module MA_Metadata
!--------------
! 
! Module designed to hold 'constant character options'
    Implicit NONE

    Character (Len = 40), Parameter :: PNAME = 'MarcoAir'
    Character (Len = 40), Parameter :: PVERS = '3.0.1'
    Character (Len = 40), Parameter :: PDATE = '08 February, 2021'
    Character (Len = 40), Parameter :: PAUT1 = 'CSIRO Electromagnetic Modelling Group'
    Character (Len = 40), Parameter :: PAUT2 = 'Zonghou Xiong & Art Raiche'
    Character (Len = 40), Parameter :: PAUT3 = 'David Annetts (david.annetts@csiro.au)'
    Character (Len = 40), Parameter :: PPROJ = 'AMIRA P223D'
    Character (Len = 80), Parameter :: PRELS = 'GPL-2.0 (Full text at https://opensource.org/licenses/gpl-2.0.php)'

End Module MA_Metadata
!------------------------------------------------------------------------------------
!
!	3.0.0
!	1.	Removed symmetry checks. These were critical in when memory was limited. However, in
!		2020, they prove more trouble than they are worth when the major limiting factor
!		is CPU speed and memory is O(GB) not O(kB).
!
!  Essentially there is no change in functionality from the P223D final version.
!  except for a few minor refinements.  The data input format has been revised 
!  conform to the general P223F AEM format
!  MarcoAir 2.8.2 has a better option for user specified spectra for 
!  time-domain modelling. 
!  The prescriptive header has been removed to reflect the end of embargo. 
!
!================
!   DESCRIPTION |
!================
!
!  Marco_Air computes the airborne EM response (dB/dt or B) for any
!  arbitrary structure which can be composed of multiple rectilinear
!  prisms oriented parallel to the three component axes contained in a
!  multi-layered host.  Cole-Cole resistivities may be specified for
!  target and layers.
!
!  The airborne system is modelled as an airborne dipole transmitter
!  loop with one or more receivers.  Three components are computed:
!  vertical, horizontal in-line, and horizontal perpendicular.  The
!  dipole can be either vertical, horizontal, or tilted at any angle
!  in the flight path of the plane.  The response can be computed in
!  time domain or frequency-domain.
!
!  Marco_Air uses multiple prismatic blocks to simulate arbitrary
!  structures.  The conductivity in each prism can be arbitrary or
!  constant.  The program also allows symmetric structures with two
!  symmetry planes, the (XOZ) and (YOZ) planes, with the shape and
!  conductivity of the structure in one quadrant being arbitrary.
!  Symmetrical considerations greatly saves both computation time
!  and storage requirements.
!
!  This program uses a system iterative method for solving the matrix
!  equation,  thus it allows a large number of cells to be handled for
!  large and complex structures.  The division of substructures are
!  done automatically by the program itself if the number of cells for the
!  structure or for one quadrant of a symmetric structure exceeds defined
!  limits for a single scattering matrix.  There is in fact no limit on
!  the total number of cells to be computed by this program.  If the
!  structure is discretised into equal sized cells that fit in a uniform
!  grid, the program will invoke spatial symmetry reductions to speed up
!  the computation for the Green's functions.  The spatial symmetry
!  reductions also enable fast re-computation of all matrix elements,
!  which in conjunction with the method of system iteration allows tens,
!  or even hundreds of thousands of cells.  Thus there is no limit on the
!  size of the models to be computed,  as long as there is available
!  computer time.
!
!  This current version of Marco_Air is however restricted to conductivity
!  contrasts of up to about 1000 to 1 between the 3D target and its
!  immediate host.
!
!   Geometry Conventions
!   --------------------
!
!   The user enters coordinates in terms of Easting, Northing and depth.
!   Depth is positive downwards.
!
!
!   Internally the positive X axis points North.
!   The positive Y axis will point East.  Z is positive downwards.
!   This has no effect on data input.
!
!*****************************************
!   TIME-DOMAIN WAVEFORM SIGN CONVENTION *
!*****************************************
!
!  If the the user specifies waveform excitation as either the transmitter
!  current or primary B at the receiver, the program assumes that current or
!  magnetic field will start at 0 or some low value, and rise to a positive
!  maximum before going to zero or oscillating about zero with small
!  magnitudes.  In this case, dI/dt wull be computed using the negative I or B
!  so that the early off-time response is positive for vertical fields.

!  If the user specifies the excitation waveform as dB/dt at the receiver,
!  the program assumes that the response will rise to a positive maximum
!  followed by a negative maximum before oscillating about zero with smaller
!  amplitudes.  In this case dI/dt is derived by reversing the sign of the
!  input primary dB/dt and dividing by the geimetric coupling factor.  Again,
!  this procedure is designed so that the early off-time response is positive
!  for vertical fields.
!
!*********************
!      Systems
!*********************
!
!  MarcoAir can be used to model any existing AEM system in frequency or
!  time-domain mode.  Currently the transmitter is modelled as a magnetic
!  dipole whose axis is in the vertical plane along the flight path.  Three
!  coponents of either B or dB/dt are computed.  For co-axial time-domain HEM
!  systems normalisation assumes a horizontal circular loop.
!  In time-domain, transmitter  output can be specified either as current or
!  as dB/dt or B from a calibration run.
!  A variety of flight path specifications are available.
!
!   Time-domain waveform sign convention
!   ------------------------------------
!
!  If the user specifies waveform excitation as either the transmitter
!  current or primary B at the receiver, the program assumes that current or
!  magnetic field will start at 0 or some low value, and rise to a positive
!  maximum before going to zero or oscillating about zero with small
!  magnitudes.  In this case, dI/dt will be computed using the negative I or B
!  so that the early off-time response is positive for vertical fields.
!
!  If the user specifies the excitation waveform as dB/dt at the receiver,
!  the program assumes that the response will rise to a positive maximum
!  followed by a negative maximum before oscillating about zero with smaller
!  amplitudes.  In this case dI/dt is derived by reversing the sign of the
!  input primary dB/dt and dividing by the geometric coupling factor.  Again,
!  this procedure is designed so that the early off-time response is positive
!  for vertical fields.
!
!*********************
!   FILE CONVENTIONS
!*********************
!
!   INPUT FILES:
!   -----------
!
!   The input control file, named MarcoAir.cfl is read
!   from logical unit number NR = 3.
!
!
!   INPUT-OUTPUT FILES:
!   ------------------
!
!   Frequency-domain output data, for reuse in
!   the time-domain restart option, is written to and read from
!   MarcoAir.frq, on logical unit ND = 7
!
!
!   VERBOSE-OUTPUT FILES:
!   --------------------
!
!   The MarcoAir.out is written to logical unit NW = 4
!
!   Messages about data or runtime errors are written in a file
!   called MarcoAir.log on logical unit NLG = 9
!
!
!   OUTPUT FILES FOR PLOTTING:
!   --------------------------
!
!   The AMIRA format file, MarcoAir.amx has been replacedby MarcoAir.mf1
!
!
!    UNIT #   UNIT ID      FILE ID      Function
!    ------   -------      -------      --------
!       3       NR       MarcoAir.cfl   Input control file
!       4       NW       MarcoAir.out   Verbose output data
!       7       ND       MarcoAir.frq   F-D data for T-D reuse
!       9       NLG      MarcoAir.log   Data error messages
!      14       np      MarcoAir.mf1   Model output   
!
!****************************************************
!     DESCRIPTION OF DATA RECORDS for MARCOAIR.CFL
!****************************************************
!
!  All records are in list directed (free) format except for
!  the TITLE in RECORD 1.
!
!    NOTE:  All distances and locations are to be specified in metres.
!    ----   All angles are to be specified in degrees.
!
!      In plan view, the coordinate system used for input and output
!      is (East, North, Depth).   Depth is positive downwards.
!
!
!**  RECORD 1:  TITLE - up to 120 characters
!
!**  RECORD 2:  TDFD, DO3D, PRFL, ISTOP
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
!            from 1 to 10 Hz to maintain an accuracy of better than .1 percent for
!            the models studied.  This means that 28 frequencies are needed for the
!            1Hz to 100kHz range.
!           
!            This is over-ridden by setting TDFD = 0 in which case the user needs to specify
!            the minimum and maximum frequencies plus points per decade in RECORD 2.1
!  
!  
!      DO3D < 0  INVERSION  In this case, NPLATE = 0 is not allowed.
!      -------------------------------------------------------------
!        
!           = -1 : the fitting error and sensitivity matrix are normalised
!                  point by point by the averaged field and model data for
!                  each survey & channel. This was the only inversion 
!                  option for versions prior to 5.4.0.  
!                  This point normalisation option is reported as: 
!                  symmetric point norm or P-norm rms error.
!        
!           = -2 : for each channel or frequency, the fitting error and 
!                  sensitivity matrix are normalised by the L1 norm of 
!                  all survey data for that channel.  
!                  Reported as: survey norm or S-norm rms error
!        
!      DO3D =  1, 2 or 0 for FORWARD MODELLING
!
!      DO3D =  1  computes response of 3-D heterogeneities and prints
!                 voltages as measured by receivers.
!
!           =  2  (time-domain only)
!                 instead of computing the frequency-domain responses
!                 (95-99 percent of MarcoAir usual computation time) use
!                 previously computed frequency-domain responses contained
!                 in file MarcoAir.frq to calculate 3-D time-domain responses.
!
!           =  0  compute layered earth model only
!
!       PRFL =  1 prints response in profile mode using the default units and
!                 line tag.  Each column contains the response at all stations
!                 for a channel or frequency.
!
!            =  2 prints responses in temporal or frequency mode, using the
!                 default units and line tag.  Each column contains the
!                 responses for a single position for all frequencies (TDFD=2)
!                 or delay times (TDFD=1).
!
!       ISTOP = 0  read the input data and run the specified models.
!             = 1  read the input data, print the model description
!                  and STOP so that the model description can be verified.
!
!            REMEMBER to change ISTOP to 0 once the models have been verified.
!
!              ________________________________________________
!              NOTE ON DO3D for TIME-DOMAIN forward modelling
!              ------------------------------------------------
!
!    Each time MarcoAir is run with DO3D = 1, a file called MarcoAir.frq 
!    is created on logical unit ND = 7.  MarcoAir reads this file to 
!    produce the time-domain responses.
!  
!    Setting DO3D = 2 allows the user to test different time-domain
!    systems (with the same model and Tx-Rx geometry of course) in a
!    tiny fraction of the time required to rerun the whole model.
!  _____________________________________________________________________
!
!  RECORDS 2.1, 3, 4, 5, 6, 7 & 8 for TIME-DOMAIN AEM SYSTEM INFORMATION  (ISW > 0)
!  ---------------------------------------------------------------------
!
!------------------------------------------------------------------------------------------
!    only if TDFD = 0
!**  RECORD 2.1:  MIN_FREQ, MAX_FREQ, IPPD
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
!      one can achieve accurate results in less time - especially for inversion.
!
!------------------------------------------------------------------------------------------
!  
!   The user specifies the waveform for 1/2 cycle ONLY.  The program
!   will then fill in the other half as the negative of the first half.
!   Any non-negative off-time value can be specified.
!
!**  RECORD 3:  ISW, NSX, STEP, UNITS, NCHNL, KRXW, OFFTIME
!
!      ISW =  1 =>  RECORD 4 will contain a transmitter current waveform in amps
!      ISW = -1 =>  Aerotem triangular current input with on-time channels
!
!      ISW =  4 =>  Step B system where the Tx current waveform is a
!                   bipolar square wave (no off-time) whose frequency
!                   will be read in RECORD 4.  Output is B in femtoteslas.
!
!                   For ISW = 4, set OFFTIME = 0  and NSX = 1
!
!                   Towed Bird ONLY options
!                   ------------------------
!       ISW = 10  => RECORD 4 will contain the horizontal IN-LINE dB/dt receiver
!                    calibration waveform in dB/dt UNITS
!
!       ISW = 11 =>  RECORD 4 will contain the horizontal IN-LINE B receiver
!                    calibration waveform in B UNITS.
!
!       ISW = 30 =>  RECORD 4 will contain the VERTICAL dB/dt receiver
!                    calibration waveform in dB/dt UNITS
!
!       ISW = 31 =>  RECORD 4 will contain the VERTICAL B receiver
!                    calibration waveform in B UNITS.
!
!                   Central loop ONLY options
!                   -------------------------
!       ISW = 130 =>  RECORD 4 will contain the VERTICAL dB/dt receiver
!                     calibration waveform in dB/dt UNITS
!       ISW = 131 =>  RECORD 4 will contain the VERTICAL B receiver
!                     calibration waveform in B UNITS.
!
!            Geotem / Questem Stripping Option
!            ---------------------------------
!
!            Geotem & Questem data are processed by using an algorithm to strip
!            primary field and bird motion from the output waveform.  Users
!            have the option to apply this algorithm to MarcoAir model output.
!            This can be done by specifying ISW as a negative integer for
!            receiver waveform options.
!
!            In other words, setting ISW = -10, -11, -30 or -31 instead of
!            10, 11, 30 or 31 respectively will produce stripped output.
!
!
!      STEP = O =>  Compute dB/dt in nanovolts per unit area  (nV/m^2)
!                    (same as nanoteslas per second (nT/s) )
!
!           = 1 =>  Compute B field in picoteslas (pT)
!
!      UNITS apply to  - waveform input except for ISW = 1
!                      - un-normalised forward time-domain model output
!                      - un-normalised time-domain data to be inverted
!
!                    STEP=0   STEP=1
!                    dB/dt      B
!                    -----    ------
!      UNITS = 1     nT/s       nT
!              2     pT/s       pT
!              3     fT/s       fT
!
!      NCHNL - number of receiver channels
!
!      NSX - number of digitised points in waveform including endpoints
!
!      KRXW = 1 => receiver channels will be read in terms of start and end
!                  times in ms.
!      KRXW = 2 => receiver channels will be read in terms of midpoints and
!                  channel widths.
!
!      OFFTIME - time (milliseconds) between end of one pulse and the start of
!                the next pulse (of opposite sign) since a bipolar waveform is
!                assumed.  For systems which have a signal which is always on,
!                OFFTIME = 0.
!
!
!**  RECORD 4  for ISW /= 4:  (TXON(J), WAVEFORM(J), J = 1,NSX)
!              -------------
!
!      TXON(J) = digitised time (in milliseconds)
!                In most cases, TXON(1) = 0, TXON(NSX) = pulse on-time
!
!      WAVEFORM(J) = transmitter current (in amps) at time TXON(J) if ISW = 1
!                  = vertical dB/dt receiver waveform if ISW = 30 or 130
!                  = vertical B receiver waveform if ISW = 31 or 131
!                  = horizontal in-line dB/dt receiver waveform if ISW = 10
!                  = horizontal in-line B receiver waveform if ISW = 11
!
!**  RECORD 4  for ISW = 4 ONLY:  FREQ, TXAMPL
!              ----------------
!      FREQ = frequency of bipolar square wave
!      TXAMPL = peak to peak current amplitude
!
!
!         In RECORDS 5 (& 6), the receiver windows can be specified in terms of
!         start & end times if KRXW = 1; or centres & widths if KRXW = 2.
!
!   If KRXW = 1, Specify start and end times (in ms) of receiver windows.
!                   (measured from start of signal turn on.)
!   ---------------------------------------------------------------------
!**  RECORD 5:  (TOPN(J), TCLS(J), J=1, NCHNL)
!
!         No RECORD 6 if KRXW = 2!  Go to RECORD 7
!   ---------------------------------------------------------------------
!
!   If KRXW = 2,  Specify centres and widths of receiver windows.
!   ------------------------------------------------------------
!**  RECORD 5:  (TMS(J), J=1,NCHNL) - centre of receiver window gates in ms.
!                                     measured from start of signal turn on.
!
!**  RECORD 6:  (WIDTH(J), J=1,NCHNL)  -  width of Rx window gates in ms.
!
!
!**  RECORD 7:  TXCLN, CMP, KPPM
!
!         TXCLN - angle in degrees that transmitter dipole axis makes
!                 with the vertical for level flight.
!                 TXDIP = 0 for a horizontal loop in level flight.
!                 TXCLN > 0 if the front of the loop is above the rear of the loop
!
!
!    INVERSION:
!    ---------
!         CMP = 11 => invert on horizontal in-line component only
!             = 13 => invert on vertical component only
!             = 2  => joint inversion on vertical & horizontal in-line components
!             = 3  => invert on all 3 components
!             = 4  => invert on total field
!
!    FORWARD MODELLING
!    -----------------
!       CMP = 11 => print horizontal in-line component only
!           = 13 => print vertical component only
!           = 2  => print vertical & horizontal in-line components
!           = 3  => print all three components
!
!      KPPM = 0  => No normalisation (automatic if ISW = 4)
!
!      KPPM > 0 => normalisation based on maximum dI/dt response for dB/dt output.
!                  In this case, an entry for KPPF in RECORD 7.1 is necessary.
!                  ---------------------------------------------------------------
!
!      KPPM = 1   => All components are normalised to in-line primary field
!      KPPM = 3   => All components are normalised to vertical primary field
!      KPPM = 123 => Vertical component normalised to the vertical primary &
!                    In-line component to the in-line primary
!                    Transverse component to the transverse primary
!                     (For null coupling, total field is used.)
!
!      KPPM = 4 =>   all components are normalised to total primary field
!
!      -------------------------
!      only if KPPM > 0
!**    RECORD 7.1:  NPPF
!      -------------------------
!
!           NPPF = 1 => normalisation is in percent (parts per hundred)
!                = 2 => normalisation in parts per thousand
!                = 3 => normalisation in parts per million
!                = 4 => normalisation in parts per billion
!
!      -------------------------
!      only if ISW = 1 or ISW > 100
!**    RECORD 7.2:  TXAREA, NTRN
!      -------------------------
!
!      TXAREA - transmitter loop area (sq. metres)
!        NTRN - number of transmitter loop turns
!
!      TXAREA & NTRN need be specified ONLY for ISW = 1 or ISW > 100,
!                    because the program ignores them for all other ISW options.
!
!**  RECORDS 8: ZRX0, XRX0, YRX0
!
!      ZRX0 - initial vertical offset of RX J;              below = positive
!      XRX0 - initial in-line horizontal offset of RX J;    behind = positive
!      YRX0 - initial transverse horizontal offset of RX J; left = positive.
!
!             These are the high altitude calibration offsets.
!             If SURVEY < 3 in RECORD 9, these offsets are used for each station
!
!
!     DATA ENTRY CONTINUES WITH FLIGHT PATH SPECIFICATION IN RECORD 9
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  RECORDS 3 & 4 for FREQUENCY-DOMAIN AEM SYSTEM INFORMATION
!  -------------------------------------------------------------
!
!**  RECORD 3:  NFREQ, CMP, NPPF
!
!      NFREQ - number of frequencies
!
!    INVERSION:   data must be in normalised form either in PPM, PPB, PPT or percent
!    ---------    Invert on single component response in direction of
!                 transmitter orientation; eg co-planar or co-axial.
!
!      CMP = 1 => Transmitter dipole axis azimuth is oriented along flight path
!                 Transmitter dipole axis inclinations are specified in RECORD 5
!
!      CMP = -1 for VCPB (vertical co-planar broadside array)
!               Transmitter dipole axis azimuth is 90 degrees to flight line
!               Transmitter dipole axis inclination is fixed at 90 degrees
!               regardless of RECORD 5
!
!
!    FORWARD MODELLING
!    -----------------
!      CMP = 1 or -1 => compute single component response in direction of
!                       transmitter orientation; eg co-planar or co-axial.
!                       Use CMP = -1 only for VCBB array
!          = 2 => compute vertical and horizontal in-line response.
!          = 3 => compute 3 component response.
!
!           If CMP = 1, output is in normalised units specified by NPPF
!           If CMP > 1, output is in picoTeslas
!
!
!     NPPF = 1 => normalisation is in percent (parts per hundred)
!          = 2 => normalisation in parts per thousand
!          = 3 => normalisation in parts per million
!          = 4 => normalisation in parts per billion
!
!
!**  RECORDS 4: (FREQ(J), ZRX(J), XRX(J), YRX(J), TXCLN(J), J = 1, NFRQ)
!            (enter a separate record for each of the NFRQ offsets in metres
!
!      FREQ(J) - frequency in Hz
!
!      TXCLN(J) - inclination angle in degrees that transmitter dipole axis makes
!                 with the vertical.
!                 TXCLN = 0 for a horizontal loop in level flight.
!                 TXCLN is positive if the front of the loop is
!                       above the rear of the loop
!
!      If CMP = -1, the default TXCLN = 90.  In this case, entries for
!      TXCLN in RECORD 4 are ignored and needn't be specified.
!
!      ZRX(J) - vertical offset of RX J;              below = positive
!      XRX(J) - in-line horizontal offset of RX J;    behind = positive
!      YRX(J) - transverse horizontal offset of RX J; left = positive.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!             Flight Path Information
!             -----------------------
!
!  ======================================================================
!
!**  RECORD 9.0:  NSTAT, SURVEY, BAROMTRC, LINE_TAG
!
!        NSTAT - total number of transmitter data positions for all lines
!
!       SURVEY = 1 constant altitude, course & Tx-Rx geometry
!              = 2 variable altitude & course but constant Tx-Rx geometry
!              = 3 variable altitude, course, transmitter pitch
!                          and receiver offset (time-domain only)
!
!     BAROMTRC = 0 => altitudes are ground clearance in metres.
!              = 1 => altitudes are barometric; ie, metres above sea level.
!
!     LINE_TAG = 0 => default line number (1000) will be used for all stations
!              = 1 => a line number must be specified for each station
!
!-------------------------------------------------------------------------------
!   ONLY IF SURVEY = 1    Constant course and altitude
!   ------------------
!
!    IF LINE_TAG = 0
!**  RECORD 9.1:  EAST(1), NORTH(1), ALT(1), BEARING, DSTAT
!
!    IF LINE_TAG = 1
!**  RECORD 9.1:  LINE(1), EAST(1), NORTH(1), ALT(1), BEARING, DSTAT
!
!      LINE(1) - integer line number
!      EAST(1) - Initial East coordinate of transmitter
!     NORTH(1) - Initial North coordinate of transmitter
! ---------------------------------------------------------------------------------
!         In frequency domain, these are interpreted as the Tx-Rx midpoint.
!         The Tx position for all frequencies are based on the offset for 
!         the first frequency.  The Rx position for each frequency is computed 
!         using the offset for that frequency relative to the Tx position for 
!         frequency 1.
! ---------------------------------------------------------------------------------
!
!       ALT(1) - Altitude of transmitter
!      BEARING - Flight path angle with respect to North (in degrees)
!                Due North = 0;  due East = 90.
!        DSTAT - Distance (metres) between successive transmitter positions.
!
!-------------------------------------------------------------------------------
!
!   IF SURVEY > 1    Variable course and altitude  (NSTAT records to follow)
!   -------------
!
!        Enter  RECORD 9.J,  J = 1,NSTAT  - ie one for each station
!
!   If SURVEY = 2  and  LINE_TAG = 0
!   ---------------------------------
!**  RECORD 9.J:  EAST(J), NORTH(J), ALT(J)
!
!
!   If SURVEY = 3  and  LINE_TAG = 0    (Time-domain only)
!   ---------------------------------
!**  RECORD 9.J:  EAST(J), NORTH(J), ALT(J), TXCLN(J), ZRX(J), XRX(J), YRX(J)
!
!
!   If SURVEY = 2  and  LINE_TAG = 1
!   ---------------------------------
!**  RECORD 9.J:  LINE(J), EAST(J), NORTH(J), ALT(J)
!
!
!   If SURVEY = 3  and  LINE_TAG = 1    (Time-domain only)
!   ---------------------------------
!**  RECORD 9.J:  LINE(J), EAST(J), NORTH(J), ALT(J), TXCLN(J), ZRX(J), XRX(J), YRX(J)
!
!
!      LINE(J) - integer line number
!      EAST(J) - East coordinate of transmitter at station J
!     NORTH(J) - North coordinate of transmitter at station J
! ---------------------------------------------------------------------------------
!         In frequency domain, these are interpreted as the Tx-Rx midpoint.
!         The Tx position for all frequencies are based on the offset for 
!         the first frequency.  The Rx position for each frequency is computed 
!         using the offset for that frequency relative to the Tx position for 
!         frequency 1.
! ---------------------------------------------------------------------------------
!
!       ALT(J) - Altitude of transmitter at station J
!
!       ZRX(J) - Vertical offset of receiver at station J
!                below = positive
!       XRX(J) - In-line horizontal offset of receiver at station J
!                behind = positive
!       YRX(J) - Transverse horizontal offset of receiver at station J
!                left = positive.

!
!          LITHOLOGY & STRUCTURE FOR MARCO & MARCOAIR
!          ===========================================
!
!** RECORD 10:  NLAYER, NPRISM, NLITH, GND_LVL
!
!      NLAYER - number of layers including basement.
!               NLAYER can be any integer > 0.
!
!      NPRISM - number of 3D prisms in the layered earth structure
!             - a negative value of NPRISM will bypass symmetry checks.  This is 
!               useful for models containing single targets which are offset from the 
!               origin. Symmetry checks would shift such plates to a new origin.
!
!       NLITH - number of layer plus plate lithologies.  Any number of
!               lithologies may be defined.  Be careful not to use
!               layer lithologies for plates and vice versa
!
!     GND_LVL - Relative level of flat surface (m)
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
!      RES(I) - layer resistivity
!    SIG_T(I) - Conductance (conductivity-thickness product)
!      RMU(I) - relative layer magnetic permeability for LITH_INDEX(I)
!     REPS(I) - relative layer dielectric constant (permittivity for LITH_INDEX(I)
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
!
!     NOTE:  For plates, RMU must = 1;   REPS must = 1
!
!
!          LAYERED EARTH STRUCTURE
!          -----------------------
!
!   (Don't enter 12.1 for a uniform half-space)
!** RECORD 12.1: LITH(1), THICK
!
!** RECORD 12.NLAYER: LITH (NLAYER)
!
!      LITH(J) = integer which assigns the resistivity and other
!                physical properties from the list of RECORD 11
!                to layer J.
!
!       THICK  = thickness of overburden if present
!
!                *********************************************
!                !   DATA ENTRY IS FINISHED IF DO3D = 0,     !
!                *********************************************
!
!===============================================================================
!
!===============================================================================
!
!          INPUT DATA FOR THE 3D TARGET STRUCTURE
!          --------------------------------------
!
!  Marco_Air can build up geometrically complex models as an agglomeration
!  of rectangular bricks whose boundaries are parallel to the three
!  coordinate axes.  The bricks can touch or be separated by any distance.
!  Thus a model could consist of a complex structure composed of abutting
!  rectangular bricks plus another distant structure.  The input parameter
!  NPRISM in RECORD 10 specifies the number of bricks needed to build a
!  model.
!
!  COMPUTATION TIME CAN BE CUT BY AS MUCH AS A FACTOR OF 10 IF THE MODEL IS
!  SYMMETRIC WITH RESPECT TO BOTH THE BOTH NORTH-SOUTH AND EAST-WEST AXES.
!
!  Marco_Air checks for spatial symmetry before computation.  If it finds it
!  then run times are much decreased.  One example of the required symmetry
!  is illustrated below.  In order for the symmetry option to be invoked,
!  prisms 1, 4, 5, and 7 must all be of the same size, depth to top, and
!  lithology.  Prism 2 must have the same lithology, depth to top and size
!  as prism 6.
!
!  If there is only one prism, the summetry option is automatically invoked.
!
!  NOTE: The symmetry is necessary for the MODEL ONLY and is NOT required
!        for transmitter or receiver positions relative to the model.
!
!
!
!                                NORTH
!
!                                  ^
!                                  |
!                                  |
!     xxxxxxxxxxxxxxxx      xxxxxxx|xxxxxxx     xxxxxxxxxxxxxxxx
!     x              x      x      |      x     x              x
!     x      4.      x      x      |  2.  x     x     1.       x
!     x              x      xxxxxxx|xxxxxxx     x              x
!     xxxxxxxxxxxxxxxx             |            xxxxxxxxxxxxxxxx
!                                  |
!                             xxxxx|xxxxx
!                             x    | 3. x
!   --------------------------x----|----x---------------------------->  EAST
!                             x    |    x
!                             xxxxx|xxxxx
!                                  |
!     xxxxxxxxxxxxxxxx             |            xxxxxxxxxxxxxxxx
!     x              x      xxxxxxx|xxxxxxx     x              x
!     x       5.     x      x      |  6.  x     x     7.       x
!     x              x      x      |      x     x              x
!     xxxxxxxxxxxxxxxx      xxxxxxx|xxxxxxx     xxxxxxxxxxxxxxxx
!                                  |
!                                  |
!
!
!     DISCRETISATION AND COMPUTATION CONTROL
!     --------------------------------------
!
!** RECORD 13:  KACC, SOLVER, OUTPUT
!
!        KACC = 2:  standard accuracy
!             = 3:  enhanced accuracy
!
!      SOLVER = 1:  direct solver          (best choice if model isn't too big)
!             = 2:  block iterative solver
!
!
!      OUTPUT = 10:  standard output of east, north and vertical components
!      OUTPUT = 11:    plus PERCENT TARGET EFFECT and scattered fields
!
!===============================================================================
!
!  RECORDS 14, 15 & 16 are repeated sequentially for each prism (brick)
!
!  DO FOR J = 1, NPRISM
!
!** RECORD 14:  LITH(J), PRISM_EAST(J), PRISM_NORTH(J), PRISM_TOP(J)
!
!            LITH(J) = integer which assigns the resistivity and other
!                      physical properties from the list of RECORD 11
!                      to PRISM J.  LITH(J) must be between 1 and NLITH
!     PRISM_EAST(J)  = East coordinate of the centre of the J'th prism.
!     PRISM_NORTH(J) = North coordinate of the centre of the J'th prism.
!
!
!  |     ****************************************************************************
!  |     ****************************************************************************
!  |                 MAJOR CHANGE TO SIGN & DEFINITION OF PRSM_TOP
!  |
!  |             PRSM_TOP(J) = RL of the top of the J'th prism
!  |
!  |                Therefore PRSM_TOP now increases NEGATIVELY downwards
!  |     ****************************************************************************
!  |     ****************************************************************************
!
!** RECORD 15:  PRSM_SIZE_EW(J), PRSM_SIZE_NS(J), PRSM_SIZE_Z(J)
!
!      PRSM_SIZE_EW(J) = length of the J'th prism in the East-West direction.
!      PRSM_SIZE_NS(J) = length of the J'th prism in the North-South direction.
!      PRSM_SIZE_Z(J)  = depth extent of the J'th prism from top to bottom
!
!     DISCRETISATION
!     --------------
!
!** RECORD 16:  CELL_SIZE_EW(J), CELL_SIZE_NS(J), CELL_SIZE_Z(J)
!
!          Each prism is divided into a number of cells.  Ideally these are
!          equi-dimensional but this is not a requirement.  The program will
!          adjust these dimensions to fit an integer number of cells into
!          each prism.
!
!          CELL_SIZE_EW, CELL_SIZE_NS, CELL_SIZE_Z are the cell lengths in the
!          east-west; north-south & depth directions respectively.  (metres)
!
!
!   NOTE:  For exploratory data purposes, cells can have dimensions of
!   ====   40 metres.  However, accuracy for high contrast models may
!          require cells with dimensions of 5 x 5 x 5 m.  Models with
!          this level discretisation can take quite a long time to run.
!
!   END DO
!
!**  If NPRISM > 1,  repeat records 14, 15 and 16 for each prism.
!
!
!#####################
!  END OF DATA ENTRY #
!#####################
!
!============================================================================

   MODULE MA_Filter_coefficients
!  --------------------------

  IMPLICIT NONE

  INTEGER, PARAMETER :: JNLO=-250, JNHI=150, NDEC_JN=15
  INTEGER J9
  REAL SHFTJN, WJ0(JNLO:JNHI), WJ1(JNLO:JNHI)
  SAVE

!  Filter restored to original MarcoAir 7 February, 2000 (artificial shift removed)

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

END MODULE MA_Filter_coefficients

 MODULE MA_Filter_coefficients_QL
!------------------

!  These are based on using extended precision.  Not yet used for 3D computatiuon

 IMPLICIT NONE

 INTEGER, PARAMETER :: JNLO=-250, JNHI=150, NDEC_JN=15, QL=SELECTED_REAL_KIND(12,80)
 INTEGER J9
 REAL(KIND=QL) WJ0(JNLO:JNHI), WJ1(JNLO:JNHI), WCOS(-200:99), DELCOS, SHFTJN
 SAVE

!  Filter restored to original MarcoAir 7 February, 2000 (artificial shift removed)

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
  1.6889499732D-18,  2.29485865865D-18,  3.11953078380D-18,  4.24055410750D-18,  5.76442432690D-18,  7.83590704850D-18, &
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

END MODULE MA_Filter_coefficients_QL

 MODULE MA_Frequency_select
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

!  FRQ_6PDE has 6 frequencies / decade from 0.001 to 1 MHz

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

 END MODULE MA_Frequency_select

 MODULE MA_Input_routines

! CONTAINS: READ_SYSTEM_AND_SURVEY, READ_MODEL, READ_INVRSION_CNTRL, , READ_INVRSION_DATA, SET_FRQ
 Use iso_Fortran_env
 Use MA_Metadata
 IMPLICIT NONE

! General Airborne & Layered Earth Dimensions

 INTEGER, PARAMETER :: NPROP=7, QL=SELECTED_REAL_KIND(12,80)
 REAL, PARAMETER :: T0_MIN=1.E-7, PI=3.141592654, PI2=PI/2., DATA_TOL=1.E-24, TURN=PI/10.
 INTEGER NR,NW,ND,NDR,NLG,NRI,np,KS,TDFD,STEP,DO3D,ISW,PRFL,NCHNL,KPPM,ISTOP,NLITH,  &
         NSX,NSX1,JF,JT,JS,JL,JR,NFRQ,NTYRP,NSTAT,NLYR,NRX,NRXST,NTRN,MTXRX,NPULS,    &
         NTYPLS,SURVEY,CMP,KRXW,MSG,MXERR,J,GSTRP,ASTRP,IUNITS,NPPF,QQDT(8),QQHMS(2), &
         MCHNL,NDATA,NPAR,MV1PRT,OUTPRT,CNVRG,MAXITS,A2C,FVN,BAROMTRC,LINE_TAG,IPPD
 INTEGER,ALLOCATABLE,DIMENSION(:) :: LITH,KFIX,CXPAR,XWTS,LINE,CFG1
 INTEGER,ALLOCATABLE,DIMENSION(:,:) :: RWTS
 INTEGER,DIMENSION(:),ALLOCATABLE :: Z_INDX,NSTATL,LITHL,LITHP,ID_LITH
 INTEGER,DIMENSION(:,:),ALLOCATABLE :: TMP_INDX,TX_INDX
 REAL MIN_FREQ,MAX_FREQ,TXAMPL,TXFREQ,PULSE,PKCUR,OFFTYM,ALF,DELT,ALT1,GND_LVL,TXCLN0,XRX0, &
      YRX0,ZRX0,CSF,SNF,DSTAT,PRM_TD(3),BFFAC,PPFAC,TXAREA,T0,PCTCNV,ALINE,RDUM(6)
 REAL,DIMENSION(:),ALLOCATABLE :: FREQ,WAVEFORM,TXON,SWX,TMS,WTMS,TOPN,TCLS,SZ,SX,SY,ZRX, &
                                  XRX,YRX,BEARING,FANGLE,THK,RES,RMU,REPS,CHRG,TRP,CALF,  &
                                  CTAU,CFREQ,TXCLN,TXDEG,PRM_FD,XDATA,XMODL,UBN,DEPTH,   &
                                  LBND,ELAS,MPAR
 REAL,DIMENSION(:,:),ALLOCATABLE :: SWY,RX,RY,RZ,LYTH,RDATA,PBLK
 REAL(KIND=QL) QFRQ1,QFRQ2,FQQ,D_EAST,D_NORTH,EAST1,NORTH1,ECNTRD,NCNTRD
 REAL(KIND=QL), ALLOCATABLE :: SXD(:),SYD(:),RXD(:,:),RYD(:,:)
 LOGICAL PRTSEC,NEWF,TXA90
 LOGICAL, ALLOCATABLE :: SAME_TX(:)
 CHARACTER (LEN=3), ALLOCATABLE :: CONFIG(:)
 CHARACTER (LEN=1) TCHR
 CHARACTER (LEN=120) INP,TITLE
 CHARACTER(LEN=60) PVC, LTXT
 CHARACTER(LEN=10) TIME,DATE,ZONE
 CHARACTER(LEN=3) MONTH(12)
 CHARACTER(LEN=4) QUNIT,BUNIT,PUNIT
 Integer :: tvals(8)
 DATA MONTH /'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'/
 DATA PVC, FVN /'MarcoAir - Version 2.8.4   2 September 2007',280/
 DATA LTXT     /'-------------------------------------------'/

 INTEGER :: KSYMM,NTX
 INTEGER, DIMENSION (:), ALLOCATABLE :: NCELL_EW,NCELL_NS,NCELL_Z
 REAL TN,TE
 REAL, DIMENSION (:), ALLOCATABLE :: PRSM_RES,RMUP,REPSP,PRSM_CHRG,PRSM_TAU,PRSM_CFR, &
             PRISM_ZMID,PRISM_EAST,PRISM_NORTH,PRSM_SIZE_EW,PRSM_SIZE_NS,PRSM_SIZE_Z
 SAVE

! Marco Specific Dimensions

 INTEGER NPRISM,MXB,JP,MXAB,NBLK,BEG,FIN,JA,JB,JAB,NAB,NAJ,NBJ, &
         JP2,JAB2,NAB2,MXRHO,NRMGT,KACC,SOLVER,KP,KP1,KP2
 INTEGER,ALLOCATABLE :: NA(:),NB(:)
 REAL CELLW,DAH,TMPL,PL2,X0,Y0,Z0,XCTMP,YCTMP,R1,R2,R3,R4,PARPCT
 REAL,DIMENSION(:),ALLOCATABLE :: SIG_T,CHRGP,CALFP,CTAUP,CFREQP,PLTOP,XCNTR,YCNTR,PLNGTH, &
                                  PLWDTH,AZM,PLAZM,DIP,PLDIP,ROT,PLROT,ESTRT,NSTRT,E_END,  &
                                  N_END,ZEND,DA,DB
 REAL,DIMENSION(:,:),ALLOCATABLE :: XCELL,YCELL,ZCELL
 REAL,DIMENSION(:,:,:),ALLOCATABLE :: PCNR
 REAL(KIND=QL), ALLOCATABLE, DIMENSION(:) :: XCNTRD,YCNTRD

 CONTAINS

   SUBROUTINE READ_SYSTEM_AND_SURVEY
!  -----------------------=---------

!***  Called by: MAIN
!***      Calls: CALL CONFIG_ID, WRITE_LOG_FILE

 REAL BFFACS(6),PPFACS(4),RHO,DELX,DELY,A1,A2
 CHARACTER(LEN=4) PUNITS(4),BUNITS(6)
 CHARACTER (LEN=19) WVFRM(3)
 DATA WVFRM /'Transmitter current','Vertical receiver  ','Horizontal receiver'/
 DATA PUNITS /'pct ','ppt ','ppm ','ppb '/
 DATA BUNITS /'nT/s','pT/s','fT/s','nT  ','pT  ','fT  '/
 DATA BFFACS /1.,1000.,1.E6,1.,1000.,1.E6/
 DATA PPFACS /1.E2, 1.E3, 1.E6,1.E9/

 NR =  3     !  Input unit number for MarcoAir.cfl
 NW =  4     !  Output unit number for MarcoAir.out
 ND =  7     !  Input/Output unit number for MarcoAir.frq
 NLG = 9     !  Log file unit number for MarcoAir.log
 np = 14    !  Output unit number for MarcoAir.mf1

 OPEN(NR,FILE = 'MarcoAir.cfl',STATUS = 'OLD')
 OPEN(NW,FILE = 'MarcoAir.out',STATUS = 'REPLACE')

!      Initialise some variables.

 MXERR = 0             !  Initialise input error flag
 NCHNL = 1             !  Initialise dimensions
 NSX = 1
 NPPF = 3              !  ppm default
 IUNITS = 5            !  pT default
 GSTRP = 0
 ASTRP = 0
 TXAREA = 1.
 PRM_TD = 0.
 NEWF = .FALSE.
 NDR = NR              !  Read from MarcoAir.cfl
 PRTSEC = .FALSE.      !  Don't print scattered fields
 TXA90 = .FALSE.      !  Don't print scattered fields

!  Reproduce input data with no assignments and rewind file.

 Call Date_and_time(Values = tvals)
 Write (NW, 1) PNAME, PVERS, PDATE, PAUT1, PAUT2, PAUT3, PPROJ, PRELS, &
              compiler_version(), compiler_options(), &
              tvals(1:3), tvals(5:7)
 Write ( *, 1) PNAME, PVERS, PDATE, PAUT1, PAUT2, PAUT3, PPROJ, PRELS, &
              compiler_version(), compiler_options(), &
              tvals(1:3), tvals(5:7)

 WRITE(NW,'(T11,A/T11,A/)') 'INPUT DATA', '----------'
 REFLECT_DATA: DO JF = 1,10000
   READ(NR,'(A)',END = 100) INP
   WRITE(NW,'(1X,A)') INP
 END DO REFLECT_DATA

 100 REWIND NR
     ! WRITE(NW,2)

 READ(NR,'(A)') TITLE
 WRITE(NW,'(/1X,A)') TRIM (TITLE)

! Read model control & print parameters

 READ(NR,*)  TDFD, DO3D, PRFL, ISTOP
 WRITE(NW,3) TDFD, DO3D, PRFL, ISTOP

 IF (PRFL == 0 .OR. PRFL == 2 .OR. PRFL == 10) THEN
   PRFL = 2
 ELSE
   PRFL = 1
 END IF

!   TDFD = 0, 1 or 2 for user controlled TD, standard TD or FD respectively.
!   DO3D = 2 => use old FD data from MarcoAir.frq.
!        = 1 => compute new  plate model.
!        = 0 => compute layered 1/2 space model only.
!   PRFL - indicates profile or decay curve output
!  ISTOP - read data and stop if ISTOP = 1
!        - used as a frequency setter later on in this routine.

 IF (TDFD < 0 .OR. TDFD > 2) CALL WRITE_LOG_FILE (NLG,1,MXERR,2)

 IF (DO3D > 2 ) THEN
   CALL WRITE_LOG_FILE (NLG,2,MXERR,1)
   DO3D = 1
 ELSE IF (TDFD == 2 .AND. DO3D == 2) THEN
   CALL WRITE_LOG_FILE (NLG,3,MXERR,1)
   DO3D = 1
 END IF

 IF (TDFD < 2) THEN
   IF (DO3D == 1) THEN
     NEWF = .TRUE.
     OPEN(ND,FILE = 'MarcoAir.frq',STATUS = 'REPLACE')
   ELSE IF (DO3D == 2) THEN
     NDR = ND           ! Data to be read in from MarcoAir.frq
     OPEN(ND,FILE = 'MarcoAir.frq',STATUS = 'OLD')
   END IF

! Transmitter system information
! ------------------------------

   IF (TDFD == 0) THEN
     READ(NR,*) MIN_FREQ,MAX_FREQ,IPPD
     WRITE(NW,20) IPPD,MIN_FREQ,MAX_FREQ
   END IF

   NRX = 1
   READ(NR,*)   ISW, NSX1, STEP, IUNITS, NCHNL, KRXW, OFFTYM
   WRITE(NW,4) ISW, NSX1, STEP, IUNITS, NCHNL, KRXW, OFFTYM
   IF (IUNITS < 1 .OR. IUNITS > 3) THEN
      CALL WRITE_LOG_FILE (NLG,19,MXERR,1)
   END IF
   IF (STEP == 0 .AND. IUNITS > 3) IUNITS = 1           ! Default
   IF (STEP == 1 .AND. IUNITS < 4) IUNITS = IUNITS + 3
   IF (ISW == 4)  THEN
     IUNITS = 6                         ! Default
     IF (STEP == 0) IUNITS = 3          ! Change from fT to fT/s
   END IF
   BUNIT = BUNITS(IUNITS)
   QUNIT = BUNIT
   NPULS = 5
   IF (ISW == -1) THEN
     ASTRP = 1
     ISW = 1
   ELSE IF (ISW == 4 .OR. ISW == 40) THEN
     NSX = 1
     OFFTYM = 0.
     IF (ISW == 4) NPULS = 1
     ISW = 4
   ELSE IF (ISW == -10 .OR. ISW == -30) THEN
     IF (STEP == 0) THEN
       GSTRP = 1
       WRITE(NW,41)
     ELSE
       WRITE(NW,42)
     END IF
   ELSE IF (ISW == -11 .OR. ISW == -31) THEN
     WRITE(NW,43)
   ELSE IF (ISW == -1 .OR. ISW == -4) THEN
     WRITE(NW,44)
   END IF
   ISW = ABS (ISW)

   IF (ISW /= 1 .AND. ISW /= 10 .AND. ISW /=11 .AND. ISW /= 30 .AND. ISW /= 31 &
                .AND. ISW /= 4 .AND. ISW /= 130 .AND. ISW /= 131 ) THEN
     CALL WRITE_LOG_FILE (NLG,4,MXERR,2)
   ELSE IF (ISW == 4) THEN
     ALLOCATE (SWX(NSX),SWY(NSX,3))
     IF (STEP == 1) WRITE(NW,5) TRIM (BUNIT)
     IF (STEP == 0) WRITE(NW,6) TRIM (BUNIT)
   ELSE
     IF (STEP == 0) WRITE(NW,9)  TRIM (BUNIT)
     IF (STEP == 1) WRITE(NW,10) TRIM (BUNIT)
     IF (STEP /= 1 .AND. STEP /= 0) CALL WRITE_LOG_FILE (NLG,5,MXERR,2)
   END IF

   IF (KRXW /= 1 .AND. KRXW /= 2) CALL WRITE_LOG_FILE (NLG,6,MXERR,2)

   ALLOCATE (TXON(NSX1+1), WAVEFORM(NSX1+1),TMS(NCHNL),WTMS(NCHNL), &
             TOPN(NCHNL),TCLS(NCHNL))

   TXON = 0.; WAVEFORM = 0.; TMS = 0.
   WTMS = 0.; TOPN = 0.; TCLS = 0.

   IF (ISW == 4) THEN    ! Step B response for full duty cycle rectangular pulse
     READ (NR,*) TXFREQ, TXAMPL
     SWX(1) = 0.
     PULSE = .5 / TXFREQ
     SWY(1,1) = TXAMPL
     IF (NPULS == 1) THEN
       WRITE(NW,11) TXAMPL
     ELSE
       WRITE(NW,12) TXAMPL, TXFREQ, 1000. * PULSE
     END IF
   ELSE
     IF (ISW ==   1) THEN
       WRITE(NW,13) WVFRM(1),'amps'
     ELSE IF (ISW == 10 .OR. ISW == 11) THEN ! In-line component
       WRITE(NW,13) WVFRM(3),BUNIT
     ELSE IF (ISW > 11) THEN                 ! Vertical comonent
       WRITE(NW,13) WVFRM(2),BUNIT
     END IF

     READ(NR,*) (TXON(J),WAVEFORM(J), J = 1,NSX1)  ! Read in source waveform.
     NSX = NSX1
     IF (TXON(1) > 1000. * T0_MIN) THEN !  Fill in 0 point if not in original data
       NSX = NSX1 + 1
       DO JT = NSX,2,-1
         TXON(JT) = TXON(JT-1)
         WAVEFORM(JT) = WAVEFORM(JT-1)
       END DO
       TXON(1) = 0.
       WAVEFORM(1) = 0.
     END IF

     DO J = 1, NSX
       WRITE(NW,'(3X,I4,F13.3,5X,G13.4)') J,TXON(J),WAVEFORM(J)
     END DO

     ALLOCATE (SWX(NSX),SWY(NSX,3))
     SWX(1:NSX) = 1.E-3 * TXON(1:NSX)
     PULSE = 1.E-3 * (OFFTYM + TXON(NSX))
   END IF
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
   WRITE(NW,14)

   DO JT = 1,NCHNL
     WRITE(NW,'(8X,I3,2F12.3,F11.3,F12.3)') JT,TOPN(JT),TCLS(JT),WTMS(JT),TMS(JT)
     IF ( TOPN(JT) <= 0) CALL WRITE_LOG_FILE (NLG,7,MXERR,2)
   END DO
   TOPN = 1.E-3 * TOPN
   TCLS = 1.E-3 * TCLS

! End of time-domain system input data.  If a new frequency-domain
! step data set is to be created, save the rest of the input data
! to unit number, ND.  If pre-existing frequency-domain data is to
! be re-worked, read from NDR.

! Read in Tx area, turns and the number of receivers

   READ(NR,*) TXCLN0,CMP,KPPM
   IF (KPPM > 0) THEN
     READ(NR,*) NPPF
     IF (NPPF < 1 .OR. NPPF > 4) NPPF = 3
     KPPM = ABS (KPPM)
   END IF
   PUNIT = PUNITS(NPPF)
   BFFAC = BFFACS(IUNITS)    !  Field unit conversion
   PPFAC = PPFACS(NPPF)      !  Normalisation conversion
   IF (KPPM > 0) QUNIT = PUNIT

   IF (CMP /= 11 .AND. CMP /= 13 .AND. CMP/= 2 .AND. CMP /= 3) THEN
     CMP = 3
     CALL WRITE_LOG_FILE (NLG,9,MXERR,1)
   END IF
   IF (KPPM /= 0 .AND. KPPM /= 1 .AND. KPPM /= 3 .AND. KPPM /= 123 &
                 .AND. KPPM /= 4) THEN
     KPPM = 123
     CALL WRITE_LOG_FILE (NLG,10,MXERR,2)
   END IF

! Normalisation isn't defined for step output and dB/dt waveform calibration
! or impulse output and B calibration
! It isn't used for the pure rectangular step waveform.

   IF ((STEP == 1) .AND. (ISW == 10 .OR. ISW == 30 .OR. ISW == 130)) THEN
     STEP = 0
     CALL WRITE_LOG_FILE (NLG,11,MXERR,1)
   ELSE IF ((STEP == 0) .AND. (ISW == 11 .OR. ISW == 31 .OR. ISW == 131)) THEN
     STEP = 1
     CALL WRITE_LOG_FILE (NLG,12,MXERR,1)
   END IF

   IF (ISW == 4) KPPM = 0
   IF (KPPM == 123) THEN
     IF (CMP == 11) KPPM = 1
     IF (CMP == 13) KPPM = 3
   END IF
   WRITE(NW,15) CMP,KPPM,TXCLN0

   IF (ISW == 1) THEN
     READ(NR,*) TXAREA,NTRN
     WRITE(NW,16) NINT(TXAREA),NTRN
   ELSE IF (ISW > 100) THEN        !  ISW > 100 => central loop system
     READ(NR,*) TXAREA
     WRITE(NW,17) NINT(TXAREA)
   END IF

   IF (ISW == 1) WAVEFORM = WAVEFORM * NTRN * TXAREA

   READ(NDR,*) ZRX0, XRX0, YRX0
   IF (NEWF) WRITE(ND,'(3F10.2)')  ZRX0, XRX0, YRX0
   WRITE(NW,18) ZRX0, XRX0, YRX0
   RHO = ABS (XRX0) + ABS (YRX0)
   IF (RHO < 1. .AND. KPPM > 0) KPPM = 3

 ELSE IF (TDFD == 2) THEN                ! Frequency-domain systems
  NCHNL = 1;  NSX = 1;  NTYRP=1
  ALLOCATE (SWX(NSX),SWY(NSX,3),TRP(NTYRP),WAVEFORM(NSX),TMS(NCHNL),WTMS(NCHNL),TOPN(NCHNL),TCLS(NCHNL))
  SWX=0.;  SWY=0.;  TRP=0.; WAVEFORM=0.; TMS=0.; WTMS=0.; TOPN=0.; TCLS=0. 

   WRITE(NW,7)
   IF (IUNITS < 4) IUNITS = IUNITS + 3   ! Convert to B.  Default was 5
   BUNIT = BUNITS(IUNITS)
   READ(NR,*)  NFRQ, CMP, NPPF
   IF (NPPF < 1 .OR. NPPF > 4) NPPF = 3

   IF (CMP == -1) THEN
     TXA90 = .TRUE.
     CMP = 1
     WRITE(NW,8)
     IF (NPPF < 1 .OR. NPPF > 4) NPPF = 3
   END IF
   IF (CMP < 1 .OR. CMP > 3) CALL WRITE_LOG_FILE (NLG,13,MXERR,1)

   PUNIT = PUNITS(NPPF)
   QUNIT = PUNIT
   IF (CMP > 1) QUNIT = BUNIT
   PPFAC = PPFACS(NPPF)
   BFFAC = BFFACS(IUNITS)
   WRITE(NW,19) NFRQ,CMP,NPPF,QUNIT

   NRX = NFRQ
   NRXST = NFRQ

   ALLOCATE (PRM_FD(NFRQ),FREQ(NFRQ),ZRX(NFRQ),XRX(NFRQ),YRX(NFRQ),TXCLN(NFRQ),TXDEG(NFRQ),CONFIG(NFRQ),CFG1(NFRQ))
   ZRX = 0.;  XRX = 0.;  YRX = 0.;  FREQ = 0;  TXCLN = 0.;  PRM_FD = 0.

   DO JF = 1,NFRQ
     IF (TXA90) THEN
       TXCLN(JF) = 90.
       READ(NR,*) FREQ(JF),ZRX(JF),XRX(JF),YRX(JF)
     ELSE
       READ(NR,*) FREQ(JF),ZRX(JF),XRX(JF),YRX(JF),TXCLN(JF)
     END IF
   END DO

   CALL CONFIG_ID (NFRQ,TXCLN,TXA90,XRX,YRX,ZRX,CONFIG,CFG1)

   A1 = 0.
   IF (TXA90) A1 = 90.
   DO JF = 1,NFRQ
     IF (CMP< 2) THEN
       WRITE(NW,21) JF,FREQ(JF),TXCLN(JF),A1,ZRX(JF),XRX(JF),YRX(JF),CONFIG(JF)
     ELSE
       WRITE(NW,21) JF,FREQ(JF),TXCLN(JF),A1,ZRX(JF),XRX(JF),YRX(JF)
     END IF
   END DO
 END IF                     !  End frequency-domain specifics

! Flight path details for forward modelling only.  Convert FANGLE & TXCLN to radians

   READ(NDR,*)  NSTAT,SURVEY,BAROMTRC,LINE_TAG
   WRITE(NW,22) NSTAT,SURVEY,BAROMTRC,LINE_TAG
   IF (NEWF) WRITE(ND,'(3I4)') NSTAT,SURVEY,BAROMTRC,LINE_TAG
   IF (NSTAT < 2) CALL WRITE_LOG_FILE (NLG,15,MXERR,2)
   IF (SURVEY <1 .OR. SURVEY > 3) THEN
     CALL WRITE_LOG_FILE (NLG,16,MXERR,2)
   ELSE IF (TDFD == 2 .AND. ABS (SURVEY) > 2) THEN
     CALL WRITE_LOG_FILE (NLG,16,MXERR,2)
   END IF

!  NRXST is used to dimension the transmitter-receiver offsets and transmitter
!  orientation.  For time-domain systems, the offset is constant with frequency
!  but can vary with station => NRXST = NSTAT

!  With frequency-domain systems, the offset is constant along the survey but
!  can vary with frequency => NRXST = NFRQ

!  NRX is used to dimension absolute receiver locations as a function of
!  frequency, so NRX = NFRQ for frequency-domain systems but
!  NRX = 1 for time-domain systems

   IF (TDFD < 2) THEN
     NRXST = NSTAT
     ALLOCATE (ZRX(NSTAT),XRX(NSTAT),YRX(NSTAT),TXCLN(NSTAT),TXDEG(NSTAT))
     TXCLN = TXCLN0; ZRX = ZRX0; XRX = XRX0; YRX = YRX0
   END IF

   ALLOCATE (LINE(NSTAT),SX(NSTAT),SY(NSTAT),SZ(NSTAT),FANGLE(NSTAT),BEARING(NSTAT),SXD(NSTAT),SYD(NSTAT), &
             RX(NSTAT,NRX),RY(NSTAT,NRX),RZ(NSTAT,NRX),RXD(NSTAT,NRX),RYD(NSTAT,NRX),SAME_TX(NSTAT))

   LINE = 1000; SX=0.; SY=0.; SZ = 0.; FANGLE = 0.; BEARING=0.
   SXD = 0.; SYD = 0.; RX=0.; RY=0.; RZ = 0.; RXD=0.; RYD=0.;

   IF (SURVEY == 1) THEN
     IF (LINE_TAG == 1) THEN
       READ(NDR,*) ALINE,SYD(1),SXD(1),SZ(1),BEARING(1),DSTAT
       LINE(1) = FLOOR (ALINE)
       IF (NEWF) WRITE(ND,'(I10,5F10.2)') LINE(1),SYD(1),SXD(1),SZ(1),BEARING(1),DSTAT
     ELSE
       READ(NDR,*) SYD(1),SXD(1),SZ(1),BEARING(1),DSTAT
       IF (NEWF) WRITE(ND,'(5F10.2)') SYD(1),SXD(1),SZ(1),BEARING(1),DSTAT
     END IF

     IF (NEWF) WRITE(ND,'(5F10.2)') SYD(1),SXD(1),SZ(1),BEARING(1),DSTAT
     WRITE(NW,24) BEARING(1)
     FANGLE(1:NSTAT) = BEARING(1) * PI / 180.
     LINE(2:NSTAT) = LINE(1)
     SZ(2:NSTAT) = SZ(1)

     DO JS = 2, NSTAT
       SXD(JS) = SXD(JS-1) + REAL (COS (FANGLE(1)) * DSTAT,8)
       SYD(JS) = SYD(JS-1) + REAL (SIN (FANGLE(1)) * DSTAT,8)
     END DO
   ELSE
     DO JS = 1, NSTAT
       IF (ABS (SURVEY) == 2) THEN
         IF (LINE_TAG == 1) THEN
           READ(NDR,*) ALINE,SYD(JS),SXD(JS),SZ(JS)
           LINE(JS) = FLOOR (ALINE)
           IF (NEWF) WRITE(ND,'(I10,3F14.2)') LINE(JS),SYD(JS),SXD(JS),SZ(JS)
         ELSE
           READ(NDR,*) SYD(JS),SXD(JS),SZ(JS)
           IF (NEWF) WRITE(ND,'(3F14.2)') SYD(JS),SXD(JS),SZ(JS)
         END IF
       ELSE IF (ABS (SURVEY) == 3) THEN
         IF (LINE_TAG == 1) THEN
           READ(NDR,*) ALINE,SYD(JS),SXD(JS),SZ(JS),TXCLN(JS),ZRX(JS),XRX(JS),YRX(JS)
           LINE(JS) = FLOOR (ALINE)
           IF (NEWF) WRITE(ND,'(I10,7F14.2)') LINE(JS),SYD(JS),SXD(JS),SZ(JS),TXCLN(JS),ZRX(JS),XRX(JS),YRX(JS)
         ELSE
           READ(NDR,*) SYD(JS), SXD(JS), SZ(JS), TXCLN(JS), ZRX(JS), XRX(JS), YRX(JS)
           IF (NEWF) WRITE(ND,'(7F14.2)') SYD(JS), SXD(JS), SZ(JS), TXCLN(JS), ZRX(JS), XRX(JS), YRX(JS)
         END IF
       END IF
       IF (JS > 1) THEN
         DELX = REAL (SXD(JS) - SXD(JS-1))
         DELY = REAL (SYD(JS) - SYD(JS-1))
         RHO = SQRT (DELX**2 + DELY**2)
         IF (RHO > 0.01) FANGLE(JS) = ATAN2 (DELY,DELX)
       END IF
     END DO
   END IF

   IF (ABS (SURVEY) > 1) THEN   !  Bearing correction for new line
     FANGLE(1) = FANGLE(2)
     DO JS = 2,NSTAT-2
       IF (ABS (FANGLE(JS+1) - FANGLE(JS)) > TURN) FANGLE(JS+1) = FANGLE(JS+2)
     END DO
   END IF
   BEARING = FANGLE * 180. / PI

   TXDEG = TXCLN
   TXCLN = TXCLN * PI / 180.

   SAME_TX = .FALSE.
   IF (TDFD < 2) THEN
     WRITE(NW,25) NSTAT
     DO  JS = 1, NSTAT
       WRITE(NW,26) LINE(JS),JS,SYD(JS),SXD(JS),SZ(JS),BEARING(JS),TXCLN(JS),ZRX(JS),XRX(JS),YRX(JS)
       IF (JS > 1) THEN
         A1 = ABS (SZ(JS) - SZ(JS-1))
         A2 = ABS (TXCLN(JS) - TXCLN(JS-1))
         IF (A1 < 0.1 .AND. A2 < 0.01) SAME_TX(JS) = .TRUE.
       END IF
     END DO
   ELSE
     WRITE(NW,27) NSTAT
     DO  JS = 1, NSTAT
       WRITE(NW,28) LINE(JS),JS,SYD(JS),SXD(JS),SZ(JS),BEARING(JS)
       IF (JS > 1) THEN
         A1 = ABS (SZ(JS) - SZ(JS-1))
!         A2 = ABS (TXCLN(JS) - TXCLN(JS-1))
!         IF (A1 < 0.1 .AND. A2 < 0.01) SAME_TX(JS) = .TRUE.
         IF (A1 < 0.1) SAME_TX(JS) = .TRUE.
       END IF
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
 2 FORMAT (T1,79('-'))
 3 FORMAT(/T3,'TDFD =',I3,3X,'DO3D =',I3,3X,'PRFL =',I3,4X,'ISTOP =',I2)
 4 FORMAT(/T3,'ISW =',I4,T15,'NSX =',I4,T27,'STEP =',I2,T39,'UNITS =',I4,T52,'NCHNL =',I4, &
         /T3,'KRXW =',I3,T15,'OFFTYM =',G12.4)
 5 FORMAT(//T10,'+-------------------------------------------' &
            /T10,'+   Airborne System Information'              &
            /T10,'+   100 Percent Duty Cycle STEP Response'     &
            /T10,'+   for Rectangular Waveform'                 &
            /T10,'+   B output will be in ',A                   &
            /T10,'+-------------------------------------------'/)
 6 FORMAT(//T10,'+-------------------------------------------' &
            /T10,'+   Airborne System Information'              &
            /T10,'+   100 Percent Duty Cycle Response'          &
            /T10,'+   for Rectangular Waveform'                 &
            /T10,'+   dB/dt output will be in ',A               &
            /T10,'+-------------------------------------------')
 7 FORMAT(/10X,'+------------------------------------------------+' &
          /10X,'+  Frequency-Domain Airborne System Information  +' &
          /10X,'+------------------------------------------------+')
 8 FORMAT(/T3,'System orientation = vertical coplanar broadside')
 9 FORMAT(//T10,'+----------------------------------------------' &
           /T10,'+    Time-Domain AEM Impulse System Input Data ' &
           /T10,'+          dB/dt output will be in ',A           &
           /T10,'+----------------------------------------------')
 10 FORMAT(//T10,'+----------------------------------------------' &
            /T10,'+    Time-Domain AEM Step System Input Data    ' &
            /T10,'+        B output will be in ',A                 &
            /T10,'+----------------------------------------------')
 11 FORMAT(/T6,'Peak Current =',F6.1,' amps.' &
          /T6,'Single pulse response to step current turn-off.')
 12 FORMAT(/T6,'Peak Current =',F6.1,' amps.' &
           /T6,'Step B System Frequency =',F6.1,' Hz.' &
           /T6,'Pulse On-Time =',F6.1,' ms.'/)

 13 FORMAT(//T27,A/T12,'TXON (ms)      waveform in ',A &
                  /T12,'---------      -----------------'/)
 14 FORMAT(/T10,'Receiver Window Specifications (ms)'/ &
            T10,'-----------------------------------'// &
            T8,'Window',T19,'Open',T31,'Close',T42,'Width',T53,'Centre'/ &
            T8,'------',T19,'----',T31,'-----',T42,'-----',T53,'------')
 15 FORMAT(/T3,'CMP =',I3,4X,'KPPM =',I4 &
           /T3,'Inclination angle of transmitter in level flight =',F5.1,' degrees (front up)')
 16 FORMAT(/T3,'Tx area =',I8,' m^2;    NTRN =',I2)
 17 FORMAT(/T3,'Tx area =',I8)
 18 FORMAT(/T3,'Initial Rx offset relative to Tx:',F7.1,' Below,',F7.1,' Behind,',F6.1,' Left')
 19 FORMAT(/T3,'NFRQ =',I3,';  CMP =',I2,';  NPPF =',I2 &
           /T3,'Data will be expressed as ',A &
          //T3,'Frequencies, Tx Angles and Receiver Offset(s)' &
          //T6,'Frequency  TXCLN  TXAZM   ZRX   XRX   YRX   CONFIG' &
           /T6,'---------  -----  -----   ---   ---   ---   ------')
 20 FORMAT(/T3,'IPPD =',I2,4X,'MIN_FREQ =',G13.4,4X,'MAX_FREQ =',G13.4)
 21 FORMAT(I3,F9.0,F8.0,F7.0,F7.1,2F6.1,T51,A)
 22 FORMAT(//T3,'NSTAT =',I4,3X,'SURVEY =',I2,3X,'BAROMTRC =',I2,3X,'LINE_TAG =',I2)
 24 FORMAT(T3,'The flight path follows an angle of',F5.0,' degrees East of North.')
 25 FORMAT(/T7,I3,' transmitter positions along the flight path' &
          //T6,'Line   Stat     East       North       Alt      Bearing    Pitch   ZRX    XRX      YRX' &
           /T6,'----   ----     ----       -----       ---      -------    -----   ---    ---      ---'/)
 26 FORMAT(T1,I9,I6,2F12.1,2F10.1,F9.1,3F8.1)
 27 FORMAT(/T7,I3,' transmitter positions along the flight path' &
          //T6,'Line   Stat       East        North       Alt     Bearing' &
           /T6,'----   ----       ----        -----       ---     -------')
 28 FORMAT(T1,I9,I6,2X,2F12.1,2F10.1)
 41 FORMAT(/T3,'Geotem / Questem stripping algorithm will be applied to computations.')
 42 FORMAT(/T3,'Geotem / Questem stripping algorithm is not applied for B field output.')
 43 FORMAT(/T3,'Geotem / Questem stripping algorithm is not applied for B field input.')
 44 FORMAT(/T3,'Geotem / Questem stripping algorithm is not applied for ISW = 1 or 4.')
 97 FORMAT(//T3,'MarcoAir task started at ',I2.2,':',I2.2,' on',I3.2,1X,A,I5//)

   END SUBROUTINE READ_SYSTEM_AND_SURVEY

   SUBROUTINE READ_MODEL
!  ---------------------

!***  Called by: MAIN
!***      Calls: WRITE_LOG_FILE

 IMPLICIT NONE
 REAL, PARAMETER :: PTOL = 1.0  !  Minimum prism dimension, and overlap tolerasnce
 INTEGER OUTPUT,MXLITH,NPR1,NPR2,NP11,JP,J,PR_LYR,IDUMV,KEW,KNS 
 INTEGER, ALLOCATABLE :: LBLK(:,:)
 REAL CELL_SIZE_EW,CELL_SIZE_NS,CELL_SIZE_Z,A,ESTRT,EFIN,NSTRT,NFIN, &
      BOT,BOTP,TOP,TOPL,DUMV(9),PRISM_TOP,DUMVZ
 REAL(KIND=QL) DUMVE,DUMVN


!  Layered Model Specification
!  ---------------------------

 READ(NDR,*) NLYR, NPRISM, NLITH, GND_LVL

 ! 
 ! DWA 20200130: New option to bypass symmetry check
 If (NPRISM .lt. 0) Then
 	KSYMM = 0
 	NPRISM = Abs(NPRISM)
 Else 
 	KSYMM = 1
  End If
 IF (DO3D == 0) NPRISM = 0
 IF (NEWF) WRITE(ND,'(3I6,G12.4,A)') NLYR, NPRISM, NLITH, GND_LVL,'!  NLYR, NPRISM, NLITH, GND_LVL'
 WRITE(NW,1) NLYR, NPRISM, NLITH, GND_LVL
 IF (NPRISM < 0 .OR. NPRISM > 9) CALL WRITE_LOG_FILE (NLG,50,MXERR,2)
 IF (NPRISM == 0) DO3D = 0
 NPAR = 9*NPRISM + 2*NLYR-1

 MXLITH = MAX(NLYR,NPRISM)

 ALLOCATE (LYTH(NLITH,NPROP),LITH(MXLITH),THK(NLYR),RES(NLYR),CHRG(NLYR),CALF(NLYR), &
           CTAU(NLYR),CFREQ(NLYR),RMU(NLYR),REPS(NLYR),MPAR(NPAR))

 THK=0; RES=0; CHRG=0; CALF=1; CTAU=0; CFREQ=1; RMU=1
 REPS=1; LITH=0

 ALLOCATE (DEPTH(NLYR),LITHL(NLYR),ID_LITH(NLITH))

 DEPTH=0.; LITHL=0; ID_LITH=0

!  Initialise lithology list.

 LYTH(1:NLITH, 1) = -1.   !  blank resistivity indicator
 LYTH(1:NLITH, 2) = -1.   !  blank conductance (SIG_T) indicator
 LYTH(1:NLITH, 3) = 1.    !  Relative magnetic permeabilities
 LYTH(1:NLITH, 4) = 1.    !  Relative dielectric constants
 LYTH(1:NLITH, 5) = 0.    !  Chargeabilities
 LYTH(1:NLITH, 6) = 0.    !  CTAUs
 LYTH(1:NLITH, 7) = 1.    !  CFREQs

 WRITE(NW,2)
 DO J = 1,NLITH
   READ (NDR,*) LYTH(J,1:NPROP)
   IF (NEWF) WRITE(ND,'(7G14.5)') LYTH(J,1:NPROP)
   WRITE(NW,'(I4,T8,G12.4,T22,F7.1,F12.3,F11.3,F10.2,G12.3,F8.2)') J,LYTH(J,1:NPROP)
   IF (LYTH(J,1) < 0 .AND. LYTH(J,2) < 0) CALL WRITE_LOG_FILE (NLG,53,MXERR,2)

   IF (LYTH(J,3) < 0.01) LYTH(J,3) = 1.   ! Default RMU
   IF (LYTH(J,4) < 0.01) LYTH(J,4) = 1.   ! Default REPS


   IF (LYTH(J,5) < 1.E-3 .OR. LYTH(J,6) < 1.E-15 .OR. LYTH(J,7) < 1.E-6)  THEN
     LYTH(J,5) = 0   ! default CHRG
     LYTH(J,6) = 0   ! default CTAU
     LYTH(J,7) = 1   ! default CFRQ
   END IF

 END DO

 CALL LYTH_CHK   !  Eliminate duplicate lithologies.

 DEPTH(NLYR) = 1.E6

 WRITE(NW,3)
 IF (NLYR > 1) THEN
   DO J = 1, NLYR-1
     READ (NDR,*) LITH(J), THK(J)
     IF (NEWF) WRITE(ND,'(I5,G13.5)') LITH(J), THK(J)
     IF (JL == 1) THEN
       DEPTH(1) = THK(1)
     ELSE
       DEPTH(J) = THK(J) + DEPTH(J-1)
     END IF
     WRITE(NW,'(2I4,F7.1,T19,A)') J, LITH(J), THK(J),'J, LITH(J), THK(J)'
   END DO
 END IF
 READ(NDR,*) LITH(NLYR)
 IF (NEWF) WRITE(ND,'(I5)') LITH(NLYR)
 WRITE(NW,'(2I4,T22,A)') NLYR,LITH(NLYR),'Basement Lithology'

 DO JL = 1, NLYR

   J = LITH(JL)

   IF (J < 1 .OR. J > NLITH) THEN
     WRITE(NW,'(T3,A,I2,A,I4)') 'LITH(',JL,') =',J
     CALL WRITE_LOG_FILE (NLG,54,MXERR,2)
   END IF

   RES(JL)  =  LYTH(J,1)
   IF ( RES(JL) < 0) CALL WRITE_LOG_FILE (NLG,55,MXERR,2)

   RMU(JL)  =  LYTH(J,3)
   REPS(JL) =  LYTH(J,4)
   CHRG(JL) =  LYTH(J,5)
   CTAU(JL) =  LYTH(J,6)
   CFREQ(JL) = LYTH(J,7)

   CALF(JL) = 1. - CHRG(JL)

 END DO

!*************************
!*************************

 IF (DO3D == 0) RETURN

!*************************
!*************************


!  Start reading prism variables if DO3D > 0

 READ (NDR,*) KACC,SOLVER,OUTPUT
 IF (NEWF) WRITE(ND,'(2I4)') KACC,SOLVER,OUTPUT
 WRITE(NW,4)  NPRISM, KACC, SOLVER,OUTPUT
 IF (OUTPUT > 10) PRTSEC = .TRUE.
 IF (SOLVER /= 1 .AND. SOLVER /= 2) THEN
   SOLVER = 1
   CALL WRITE_LOG_FILE (NLG,17,MXERR,1)
 END IF

 IF (KACC < 1 .OR. KACC > 3) KACC = 2
 IF (KACC == 1) WRITE(NW,5)

 NP11 = 11 * NPRISM
 ALLOCATE (LBLK(NP11,2), PBLK(NP11,9))
 LBLK=0; PBLK=0

! Initially, prism variabes are read into a pre-dummy array before
! being transferred to LBLK & PBLK.
! LBLK & PBLK are dummy arrays used to modify prism structure.
! Checks need to be made for prisms which cross layer boundaries
! & whether 2 fold symmetry is pressent.
! After this is done, the value of NPRISM is determined and the variables
! can be given their proper names.

 LBLK = 0
 PBLK = 0
 NPR1 = 0

!  For distant origins, it is necessary to read horizontal coordinates into
!  REAL(KIND=QL) variables DUMVE, DUMVN to maintain precision.  If either has
!  an absolute value greater than 2000, the origin is shifted to above the
!  first prism.  The depths to the top of the prisms are now read in as RLs
!  (negative down so they need to be converted to positive depths below
!  surface to minimise changes to MARCO_3D.


 ECNTRD = 0.D0; NCNTRD = 0.D0
 PR1: DO J = 1, NPRISM

!               LITHP(J),   PRISM_EAST(J), PRISM_NORTH(J), PRISM_TOP
   READ (NDR,*) IDUMV,      DUMVE,         DUMVN,          DUMVZ
   IF (J == 1) THEN
     IF (ABS (DUMVN) > 2.D3) NCNTRD = DUMVN
     IF (ABS (DUMVE) > 2.D3) ECNTRD = DUMVE

   END IF
   DUMV(1) = REAL (DUMVE - ECNTRD)
   DUMV(2) = REAL (DUMVN - NCNTRD)
   DUMV(3) = GND_LVL - DUMVZ

   IF (NEWF) WRITE(ND,'(I4,3G14.6)') IDUMV,DUMVE,DUMVN,DUMVZ
   IF (DUMV(3) < 0) THEN
     DUMV(3) = 0
     WRITE(NW,6) J
   END IF

   IF (IDUMV < 1 .OR. IDUMV > NLITH) THEN
     WRITE(NW,'(T3,A,I2,A,I4)') 'LITHP(',J,') =',IDUMV
     CALL WRITE_LOG_FILE (NLG,16,MXERR,2)
   END IF

   IDUMV = ID_LITH(IDUMV)  !  Assign common lithological index
   WRITE(NW,'(/T3,A,2I4,T56,3F12.1)') 'J, LITHP, PRISM_EAST, PRISM_NORTH, PRISM_TOP:', &
                                   J, IDUMV,DUMVE,DUMVN,DUMVZ

!               PRSM_SIZE_EW(J), PRSM_SIZE_NS(J), PRSM_SIZE_Z(J)
   READ (NDR,*) DUMV(4),         DUMV(5),         DUMV(6)
   IF (NEWF) WRITE(ND,'(3G14.6)') DUMV(4:6)
   WRITE(NW,'(T6,A,T56,3F12.1)') 'PRSM_SIZE_EW, PRSM_SIZE_NS, PRSM_SIZE_Z', DUMV(4:6)

!               CELL_SIZE_EW(J), CELL_SIZE_NS(J), CELL_SIZE_Z(J)
   READ (NDR,*) DUMV(7),         DUMV(8),         DUMV(9)
   DUMV(7) = MIN (DUMV(7), DUMV(4))
   DUMV(8) = MIN (DUMV(8), DUMV(5))
   DUMV(9) = MIN (DUMV(9), DUMV(6))

   IF (NEWF) WRITE(ND,'(3G14.6)') DUMV(7:9)
   WRITE(NW,'(T6,A,T56,3F12.1)') 'CELL_SIZE_EW, CELL_SIZE_NS, CELL_SIZE_Z', DUMV(7:9)

!  Eliminate PRISMS whose minimum dimension < 1 m.  If NLYR = 1, eliminate
!  prisms with the same lithology as that of the half-space.

   IF (MINVAL (DUMV(4:6)) < PTOL) THEN
      WRITE(NW,10) PTOL
      CYCLE PR1
   END IF

   IF (NLYR == 1 .AND. IDUMV == LITHL(1) ) THEN
      WRITE(NW,11)
      CYCLE PR1
   END IF

   NPR1 = NPR1 + 1
   LBLK(NPR1,1) = IDUMV
   PBLK(NPR1,1:9) = DUMV(1:9)
 END DO PR1
 NPRISM = NPR1

 IF (NPRISM < 1) THEN
   DO3D = 0
   WRITE(NW,12)
   RETURN
 END IF

! If any prisms cross layer boundaries, create new prisms.  Eliminate
! prisms which have the same lithology as the layer which contains them.

 IF (NLYR > 1) THEN

   NPR2 = 0
   PR2: DO JP = 1, NPR1
     BOTP = PBLK(JP,3) + PBLK(JP,6)
     LYR1: DO JL = 1, NLYR
       IF (PBLK(JP,3) + PTOL < DEPTH(JL)) THEN
         PR_LYR = JL   !  The top of prism J and at least
         EXIT LYR1     !  0.1 m of it are in layer JL
       END IF
     END DO LYR1

     TOPL = 0
     LYR2: DO JL = PR_LYR, NLYR
       IF (LBLK(JP,1) /= LITHL(JL)) THEN
         IF (JL > 1) TOPL = DEPTH(JL-1)
         TOP = MAX (TOPL, PBLK(JP,3))
         BOT = MIN (DEPTH(JL), BOTP)
         IF (BOT < TOP + PTOL) CYCLE LYR2
         NPR2 = NPR2 + 1
         LBLK(NPR2+NPR1,1) = LBLK(JP,1)
         PBLK(NPR2+NPR1,3) = TOP
         PBLK(NPR2+NPR1,6) = BOT - TOP
         PBLK(NPR2+NPR1,1:2) = PBLK(JP,1:2)
         PBLK(NPR2+NPR1,4:5) = PBLK(JP,4:5)
         PBLK(NPR2+NPR1,7:9) = PBLK(JP,7:9)

         IF (BOTP < DEPTH(JL) + PTOL) CYCLE PR2
       END IF
     END DO LYR2

   END DO PR2

   IF (NPR2 /= NPRISM) WRITE(NW,7)
   NPRISM = NPR2
   DO J = 1, NPRISM   !  Move new block structure to the top
     PBLK(J, 1:9) = PBLK(NPR1+J, 1:9)
     LBLK(J, 1) = LBLK(NPR1+J, 1)
   END DO
 END IF

 PBLK(NPRISM+1:NP11, 1:6) = 0.
 LBLK(NPRISM+1:NP11, 1) = 0
 LBLK(1:NP11, 2) = 0

 If (KSYMM .ne. 0) Then
   CALL CHK_SYMMETRY (NPRISM,NP11,LBLK,PBLK,TE,TN,KSYMM)
 End If
 IF (KSYMM == 1) WRITE(NW,13) TE,TN

 ALLOCATE (LITHP(NPRISM),PRISM_EAST(NPRISM),PRISM_NORTH(NPRISM),PRSM_SIZE_EW(NPRISM),  &
           PRSM_SIZE_NS(NPRISM),PRSM_SIZE_Z(NPRISM),NCELL_EW(NPRISM),NCELL_NS(NPRISM), &
           NCELL_Z(NPRISM),PRISM_ZMID(NPRISM),PRSM_RES(NPRISM),RMUP(NPRISM),           &
           REPSP(NPRISM),PRSM_CHRG(NPRISM),PRSM_TAU(NPRISM),PRSM_CFR(NPRISM))

 LITHP=0; PRISM_EAST=0; PRISM_NORTH=0; PRSM_SIZE_EW=0; PRSM_SIZE_NS=0; PRSM_SIZE_Z=0
 NCELL_EW=0; NCELL_NS=0; NCELL_Z=0; PRISM_ZMID=0; PRSM_RES=0; RMUP=1; REPSP=1
 PRSM_CHRG=0; PRSM_TAU=0; PRSM_CFR=0

 DO J = 1, NPRISM
   JL = LBLK(J,1)

   LITHP(J) = JL

   PRSM_RES(J)  = LYTH(JL,1)
   RMUP(J)      = LYTH(JL,3)
   REPSP(J)     = LYTH(JL,4)
   PRSM_CHRG(J) = LYTH(JL,5)
   PRSM_TAU(J)  = LYTH(JL,6)
   PRSM_CFR(J)  = LYTH(JL,7)

   PRISM_EAST(J)   = PBLK(J,1)
   PRISM_NORTH(J)  = PBLK(J,2)
   PRISM_TOP       = PBLK(J,3)
   PRSM_SIZE_EW(J) = PBLK(J,4)
   PRSM_SIZE_NS(J) = PBLK(J,5)
   PRSM_SIZE_Z(J)  = PBLK(J,6)
   CELL_SIZE_EW    = PBLK(J,7)
   CELL_SIZE_NS    = PBLK(J,8)
   CELL_SIZE_Z     = PBLK(J,9)

!  Construct the number of cells in each dimension

   NCELL_EW(J) = INT (PRSM_SIZE_EW(J) / CELL_SIZE_EW)
   IF (NCELL_EW(J) < 1 ) NCELL_EW(J) = 1
   A = 1.2 * NCELL_EW(J) * PRSM_SIZE_EW(J)
   IF (A < PRSM_SIZE_EW(J)) NCELL_EW(J) = NCELL_EW(J) + 1
   CELL_SIZE_EW =  PRSM_SIZE_EW(J) / REAL (NCELL_EW(J))

   NCELL_NS(J) = INT (PRSM_SIZE_NS(J) / CELL_SIZE_NS)
   IF (NCELL_NS(J) < 1 ) NCELL_NS(J) = 1
   A = 1.2 * NCELL_NS(J) * PRSM_SIZE_NS(J)
   IF (A < PRSM_SIZE_NS(J)) NCELL_NS(J) = NCELL_NS(J) + 1
   CELL_SIZE_NS = PRSM_SIZE_NS(J) / REAL (NCELL_NS(J))

   NCELL_Z(J) = INT (PRSM_SIZE_Z(J) / CELL_SIZE_Z)
   IF (NCELL_Z(J) < 1 ) NCELL_Z(J) = 1
   A = 1.2 * NCELL_Z(J) * PRSM_SIZE_Z(J)
   IF (A < PRSM_SIZE_Z(J)) NCELL_Z(J) = NCELL_Z(J) + 1
   CELL_SIZE_Z = PRSM_SIZE_Z(J) / REAL (NCELL_Z(J))
   PRISM_ZMID(J)= PRISM_TOP + .5 * PRSM_SIZE_Z(J)

   ESTRT = PRISM_EAST(J) - 0.5 * PRSM_SIZE_EW(J)
   EFIN  = PRISM_EAST(J) + 0.5 * PRSM_SIZE_EW(J)
   KEW = NCELL_EW(J)

   NSTRT = PRISM_NORTH(J) - 0.5 * PRSM_SIZE_NS(J)
   NFIN  = PRISM_NORTH(J) + 0.5 * PRSM_SIZE_NS(J)
   KNS = NCELL_NS(J)

   BOT = PRISM_TOP + PRSM_SIZE_Z(J)

   WRITE(NW,14) J
   WRITE(NW,15) J,ESTRT,EFIN, NSTRT,NFIN,PRISM_TOP,BOT,J,KEW,CELL_SIZE_EW,KNS, &
                CELL_SIZE_NS,NCELL_Z(J),CELL_SIZE_Z,J,PRSM_RES(J)
   WRITE(NW,16) RMUP(J), REPSP(J), PRSM_CHRG(J), PRSM_CFR(J), PRSM_TAU(J)

 END DO
 DEALLOCATE (PBLK,LBLK)

  1 FORMAT(//T3,'NLAYER =',I3,';   NPLATE =',I3,';   NLITH =',I3,';   GND_LVL =',F8.2)
  2 FORMAT(//T27,'LITHOLOGY PROPERTIES'/T27,'--------------------' &
           //T35,'Relative   Relative     Cole-Cole Parameters'    &
            /T9,'Resistivity  Conductance     MU     Dielectric   CHRG    CTAU       CFREQ'/)
  3 FORMAT(//T3,'LAYERED EARTH INPUT DATA'/T3,'------------------------'/)
  4 FORMAT(//T3,'INITIAL PARAMETERS FOR',I3,' PRISMS'/T3,39('-')          &
            /T3,'KACC =',I2,4X,'SOLVER =',I2,4X,'OUTPUT =',I3 )
  5 FORMAT(/T3,'Input Data for Plate',I3/T3,'-----------------------')
  6 FORMAT(/T3,'MARCO_AIR DOES NOT WORK FOR OUTCROPPING PRISMS.  The top of PRISM',I3, &
           /T3,'has been dropped to the air-earth interface.  A small cell size of' &
           /T3,'around 5 m (or less) would be advisable for a prism at the surface.'/)
  7 FORMAT(/T3,'Prism structire has changed due either prisms crossing layer boundaries' &
           /T3,'or no contrast in at least 1 layer.')
 10 FORMAT(/T3,'The minimum dimension allowed for a prism is',F5.1 &
           /T3,'This prism will be deleted.')
 11 FORMAT(/T3,'This prism has no geoelectric contrast with the half-space')
 12 FORMAT(/T3,'None of the prisms have a geoelectric contrast with the half-space')
 13 FORMAT(/T3,'The prism structure has two-fold symmetry about' &
           /T3,'East-North coordinates:',2G12.4 &
          //T3,'It has been reduced to an equivalent single quadrant structure' &
           /T3,'to allow faster computation using a group symmetry option.')
 14 FORMAT (//T10,'Description of PRISM',I3/T10,'-----------------------'/)
 15 FORMAT(T3,'PRISM',I3,' extends:' &
          /T12,'from',F7.1,'  to',F7.1,T34,' - West to East' &
          /T12,'from',F7.1,'  to',F7.1,T34,' - South to North' &
          /T12,'from',F7.1,'  to',F7.1,T34,' - Below Air-Earth Interface' &
          //T3,'PRISM',i3,' is discretised into:' &
          /T12,I3,' cells of length',F7.1,' m.  East-West', &
          /T12,I3,' cells of length',F7.1,' m.  North-South', &
          /T12,I3,' cells of length',F7.1,' m.  Vertically' &
          //T3,'PRISM',I3,' resistivity =',G12.4,' ohm-m')
 16 FORMAT(T12,'Relative MU =',F7.2,4X,'Relative EPSILON =',F7.2/ &
           T14,'PRSM_CHRG =',F7.2,T36,'PRSM_CFR =',F6.2,4X,'PRSM_TAU =',G12.4)

   END SUBROUTINE READ_MODEL

   SUBROUTINE SHOW_MODEL
!  ---------------------

! Prints the 3D model in revised coordinates plus layered earth host.

!*** Called by MAIN

 IMPLICIT NONE
 WRITE(NW,1)

! Set up pretty output for layered earth.

 DO JL = 1, NLYR
   IF(JL == NLYR) THEN
     WRITE(NW,2) NLYR,RES(NLYR),RMU(NLYR),REPS(NLYR),CHRG(NLYR),CTAU(NLYR),CFREQ(NLYR)
   ELSE
     WRITE(NW,3) JL,THK(JL),RES(JL),RMU(JL),REPS(JL),CHRG(JL),CTAU(JL),CFREQ(JL)
   END IF
 END DO

 IF (DO3D > 0) THEN
   IF (TDFD < 2) WRITE(NW,21)  ! Time-domain option
   IF (TDFD == 2) WRITE(NW,22)  ! Frequency-domain option
   IF (KACC == 1) WRITE(NW,23)
   IF (KACC > 1) WRITE(NW,24) KACC
 ELSE
   IF (TDFD < 2) WRITE(NW,25)  ! Time-domain option
   IF (TDFD == 2) WRITE(NW,26)  ! Frequency-domain option
 END IF

 WRITE(NW,35)                 ! End of input data description


  1 FORMAT(//T11,'+----------------------------------+'  &
            /T11,'+  Layered Earth Model Parameters  +'  &
            /T11,'+----------------------------------+'  &
           //T2,'Layer  Thickness  Resistivity   MU-R   EPS-R   CHRG    CTAU      CFREQ' &
            /T2,'-----  ---------  -----------   ----   -----   ----    ----      -----')
  2 FORMAT(I4,11X,  G15.4,2F7.2,F8.2,G11.2,F7.2)
  3 FORMAT(I4,F11.1,G15.4,2F7.2,F8.2,G11.2,F7.2)
 21 FORMAT(/T3,'Marco_Air will compute 3D responses for a time-domain AEM system')
 22 FORMAT(/T3,'Marco_Air will compute 3D responses for a frequency-domain AEM system')
 23 FORMAT(/T3,'using a fast APPROXIMATE 3D solution. !')
 24 FORMAT( T3,'using accuracy level: ',I3)
 25 FORMAT(/T3,'Marco_Air will compute layered earth responses for a', &
           /T3,'frequency-domain AEM system.')
 26 FORMAT(/T3,'Marco_Air will compute layered earth responses for a', &
           /T3,'time-domain AEM system.')
 35 FORMAT(/75('-')/T24,'END OF INPUT DATA DESCRIPTION'/75('-'))

   END SUBROUTINE  SHOW_MODEL

 SUBROUTINE LYTH_CHK
!-------------------

!***  Called by SUBROUTINE READ_MODEL_DATA
!  Checks lithology data and identifies duplicates
!  Checks for Cole-Cole identification.

 INTEGER  JL1
 REAL  A1,A2

 DO JL = 1,NLITH
   ID_LITH(JL) = JL
   LYTH(JL,1) = ABS (LYTH(JL,1) )
   IF (LYTH(JL,3) < 0) LYTH(JL,2) = 1
   IF (LYTH(JL,4) < 0) LYTH(JL,3) = 1
   IF (LYTH(JL,5) < 1.E-3 .OR. LYTH(JL,6) < 1.E-15 .OR. LYTH(JL,7) < 1.E-6)  THEN
     LYTH(JL,5) = 0
     LYTH(JL,6) = 0
     LYTH(JL,7) = 1.
   END IF
 END DO

 DO JL = 1, NLITH-1
   INNER: DO JL1 = JL+1, NLITH
     DO JP = 1, NPROP
       A1 = ABS (LYTH(JL,JP) - LYTH(JL1, JP))
       A2 = MIN (LYTH(JL,JP), LYTH(JL1, JP))
       IF (A1 > 1.0E-3 * A2) CYCLE INNER
     END DO
     ID_LITH(JL1) = ID_LITH(JL)
     WRITE(NW,3) JL1,JL
   END DO INNER
 END DO
 WRITE(NW,1)
 DO JL = 1, NLITH
   WRITE(NW,2) JL,LYTH(JL,1:NPROP)
 END DO


 1 FORMAT(//T27,'LITHOLOGY PROPERTIES'/T27,'--------------------' &
          //T35,'Relative   Relative     Cole-Cole Parameters'    &
           /T9,'Resistivity  Conductance     MU     Dielectric   CHRG    CTAU       CFREQ'/)
 2 FORMAT(I4,T8,G12.4,T22,F7.1,F12.3,F11.3,F10.2,G12.3,F8.2)
 3 FORMAT(/T3,'Lithologies',I3,' and',I3,' are identical.')

END SUBROUTINE LYTH_CHK

   SUBROUTINE RESET_ORIGIN
!  -----------------------

!***  Called by: MAIN

! Set up body centred origin. Shift origin by (D_NORTH, D_EAST)

   IF (NPRISM > 0) THEN
!     D_EAST = YCNTRD(1)
!     D_NORTH = XCNTRD(1)
     D_EAST  = ECNTRD
     D_NORTH = NCNTRD
     IF (MAX (ABS(D_EAST),ABS(D_EAST)) < 2000._QL) THEN
       D_NORTH = 0._QL
       D_EAST = 0._QL
     END IF
   END IF
!   XCNTR = REAL (XCNTRD - D_NORTH,4)   !  Set up old Marco world
!   YCNTR = REAL (YCNTRD - D_EAST,4)

   END SUBROUTINE RESET_ORIGIN

   SUBROUTINE SET_FRQ
!  ------------------

   REAL(KIND=QL), PARAMETER :: MID = 10._QL
   INTEGER J,PPDH,PPDL
   REAL(KIND=QL) QFRQ12,QFRQL,QFRQH,MIN_FREQD, MAX_FREQD
   REAL(KIND=QL), ALLOCATABLE :: FDUM(:)

   ALLOCATE (FDUM(1000))
   IF (TDFD == 1) THEN
     FDUM(1) = 1.0_QL
     PPDL = 3
     PPDH = 6
     MAX_FREQD = 1.0E5_QL
     T0 = MINVAL (TOPN) - SWX(NSX)
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

   WRITE(NW,1) FREQ(1),MID,PPDL,MID,FREQ(NFRQ),PPDH,FREQ(NFRQ)

 1 FORMAT(/T3,'The frequency-domain results are directly computed from' &
          /T3,G12.4,' Hz to',G12.4,' Hz, using',I3,' points per decade and from' &
          /T3,G12.4,' Hz to',G12.4,' Hz, using',I3,' points per decade.' &
         //T3,'These are used to construct the frequency-domain spectrum from DC to',G12.4,' Hz' &
          /T3,'before transformation to the time domain.')

   END SUBROUTINE SET_FRQ

   SUBROUTINE SET_SURVEY
!  ---------------------

!***  Called by: MAIN

!  Arrays are given in GPS coordinates which require double precision.
!  Rather than carry unnecessary higher precision into the computation, arrays
!  are adjusted by D_EAST and D_NORTH.  Computes and prints array positions in
!  body centred coordinates.

   IMPLICIT NONE
   LOGICAL SHIFTH,BLAB

   BLAB = .FALSE.
   SHIFTH = .FALSE.
   IF (ABS (D_NORTH) > 2.0D3) SHIFTH = .TRUE.
   IF (ABS (D_EAST) > 2.0D3) SHIFTH = .TRUE.

   IF (BLAB) WRITE (NW,1)
   IF (BAROMTRC == 1 ) THEN
     SZ = SZ - GND_LVL        ! Change barometric altitude to ground clearance
     IF (BLAB .AND. ABS (GND_LVL) > 0.01) WRITE(NW,2) GND_LVL
   END IF
   IF (BLAB .AND. SHIFTH) WRITE (NW,3) D_EAST,D_NORTH

!  Compute receiver coordinates in both body centred and GPS systems.

   DO JS = 1,NSTAT
     CSF = COS (FANGLE(JS))
     SNF = SIN (FANGLE(JS))

     IF (TDFD < 2) THEN
       SX(JS) = REAL (SXD(JS) - D_NORTH)      ! Body centred coordinates
       SY(JS) = REAL (SYD(JS) - D_EAST)
       RX(JS,1) = SX(JS) - XRX(JS) * CSF + YRX(JS) * SNF
       RY(JS,1) = SY(JS) - YRX(JS) * CSF - XRX(JS) * SNF 
       RZ(JS,1) = SZ(JS) - ZRX(JS)
     ELSE
       DO JF = 1,NFRQ
         SX(JS) = REAL (SXD(JS) - D_NORTH) + 0.5 * (XRX(1) * CSF - YRX(1) * SNF)
         SY(JS) = REAL (SYD(JS) - D_EAST ) + 0.5 * (YRX(1) * CSF + XRX(1) * SNF)
         RX(JS,JF) = SX(JS) - XRX(JF) * CSF + YRX(JF) * SNF 
         RY(JS,JF) = SY(JS) - YRX(JF) * CSF - XRX(JF) * SNF 
         RZ(JS,JF) = SZ(JS) - ZRX(JF)
       END DO
    END IF
   END DO
   RXD = REAL (RX,KIND=QL) + D_NORTH
   RYD = REAL (RY,KIND=QL) + D_EAST
   SXD = REAL (SX,KIND=QL) + D_NORTH !  SXD & SYD were midpoints in Freq domain
   SYD = REAL (SY,KIND=QL) + D_EAST  !  Now they are Tx positions based on XRX(1), YRX(1)

   IF (BLAB) THEN
     WRITE(NW,4)
     DO JS = 1,NSTAT
       WRITE(NW,5) JS,SY(JS),SX(JS),SZ(JS),RY(JS,1),RX(JS,1),RZ(JS,1)
     END DO
   END IF

 1 FORMAT(//T3,'Before computation begins, array and model coordinates are transformed from' &
           /T3,'GPS coordinates to a local coordinate system where depth increases' &
           /T3,'positive downwards with the vertical origin at the air-earth interface.')
 2 FORMAT(/T3,'Barometric altitude will be changed to ground clearance.' &
          /T3,'The vertical origin is shifted down by',F6.1)
 3 FORMAT(/T3,'When either horizontal coordinate magnitude exceeds 2km, the origin is shifted'  &
          /T3,'horizontally to lie above the reference point of the first plate.'               &
          /T3,'This shift preserves precision without the need for higher precision arithmetic' &
         //T3,'The new computation origin is offset from the GPS origin as follows:'   &
         //T3,'East by: ',F11.1,';    North by: ',F11.1)
 4 FORMAT(/T4,'The revised transmitter and receiver positions are:'               &
         //T21,'TRANSMITTER                   RECEIVER'                           &

         //T7,'Station    East    North   Altitude    East    North    Altitude ' &
          /T7,'-------    ----    -----   --------    ----    -----    -------- ')
 5 FORMAT(I10,3X,3F9.0,1X,3F9.0)

   END SUBROUTINE SET_SURVEY

   SUBROUTINE SET_TRP
!  ------------------

!***  Called by: MAIN

!  Sets up interpolation times for FD -> TD transform which use the
!  exact 6 points per decade frequency-domain data plus 6 per decade
!  interpolated values.  These are based on a 12 point per decade
!  cosine filter derived from the Niels Christensen routine FILCOA
!  with OMEGA = .3 PI and shift 0.

!             OUTPUT
!             ------

!        TRP - array of time values for FD -> TD transformations
!      NTYRP - number of values in TRP
!     EXTENT - the latest time for which time-domain output is required.
!      PULSE - time length of one signal pulse
!     NTYPLS - number of TRP values in 1 PULSE


 REAL, PARAMETER :: TWOPI=6.2831853
 INTEGER MXTYM,J1
 REAL T0,EXTENT
 REAL,ALLOCATABLE :: QQQ(:)
 REAL(KIND=QL) TBASE,QTYM, TQ

 MXTYM=200
 ALLOCATE (QQQ(MXTYM))
 QQQ = 0.

 QTYM = LOG (10.D0) /12.D0
 QTYM = EXP (QTYM)
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
!  ---------------------------

!***  Called by: MAIN

! Sets up the initial part of the output plotting file for inversion.

 INTEGER NCMP
 CHARACTER(LEN=5) CHZ,CHX,CHY,CHT
 CHARACTER(LEN=6),DIMENSION(NFRQ) :: QFRQ,IFRQ
 CHARACTER(LEN=7) RES_MOD(9), THK_MOD(9)
 CHARACTER(LEN=82) PLT_MOD(9)
 DATA RES_MOD /'  RES_1','  RES_2','  RES_3','  RES_4','  RES_5','  RES_6','  RES_7','  RES_8','  RES_9'/
 DATA THK_MOD /'  THK_1','  THK_2','  THK_3','  THK_4','  THK_5','  THK_6','  THK_7','  THK_8','  THK_9'/
 DATA PLT_MOD /'  SIG_T_1  DEPTH_1  PLNGTH_1  DIP_WDTH_1  EAST_1  NORTH_1  DIP_AZM_1  DIP_1  ROT_1', &
               '  SIG_T_2  DEPTH_2  PLNGTH_2  DIP_WDTH_2  EAST_2  NORTH_2  DIP_AZM_2  DIP_2  ROT_2', &
               '  SIG_T_3  DEPTH_3  PLNGTH_3  DIP_WDTH_3  EAST_3  NORTH_3  DIP_AZM_3  DIP_3  ROT_3', &
               '  SIG_T_4  DEPTH_4  PLNGTH_4  DIP_WDTH_4  EAST_4  NORTH_4  DIP_AZM_4  DIP_4  ROT_4', &
               '  SIG_T_5  DEPTH_5  PLNGTH_5  DIP_WDTH_5  EAST_5  NORTH_5  DIP_AZM_5  DIP_5  ROT_5', &
               '  SIG_T_6  DEPTH_6  PLNGTH_6  DIP_WDTH_6  EAST_6  NORTH_6  DIP_AZM_6  DIP_6  ROT_6', &
               '  SIG_T_7  DEPTH_7  PLNGTH_7  DIP_WDTH_7  EAST_7  NORTH_7  DIP_AZM_7  DIP_7  ROT_7', &
               '  SIG_T_8  DEPTH_8  PLNGTH_8  DIP_WDTH_8  EAST_8  NORTH_8  DIP_AZM_8  DIP_8  ROT_8', &
               '  SIG_T_9  DEPTH_9  PLNGTH_9  DIP_WDTH_9  EAST_9  NORTH_9  DIP_AZM_9  DIP_9  ROT_9'/

 WRITE(np,1) FVN,PVC,TRIM(TITLE)
 IF (TDFD < 2) THEN
   NCMP = CMP
   IF (CMP > 10) NCMP = 1
   CHZ = '  CHZ'
   CHX = '  CHX'
   CHY = '  CHY'
   CHT = '  CHT'

   WRITE(np,3) TRIM (QUNIT),NSTAT,NCHNL,NCMP
   WRITE(np,4) TMS(1:NCHNL)
   WRITE(np,5) WTMS(1:NCHNL)
   WRITE(np,6)
   WRITE(np,8) NLYR,NPRISM
   IF (CMP ==11) WRITE(np,7) (CHX,JT,JT=1,NCHNL)
   IF (CMP ==13) WRITE(np,7) (CHZ,JT,JT=1,NCHNL)
   IF (CMP ==2)  WRITE(np,7) (CHZ,JT,JT=1,NCHNL), (CHX,JT,JT=1,NCHNL)
   IF (CMP ==3)  WRITE(np,7) (CHZ,JT,JT=1,NCHNL), (CHX,JT,JT=1,NCHNL), (CHY,JT,JT=1,NCHNL)
   IF (CMP ==4)  WRITE(np,7) (CHT,JT,JT=1,NCHNL)
 ELSE
   DO JF = 1,NFRQ
     QFRQ(JF) = '  Q'//CONFIG(JF)
     IFRQ(JF) = '  I'//CONFIG(JF)
   END DO

   WRITE(np,13) TRIM (QUNIT),NSTAT,NFRQ
   WRITE(np,14) FREQ(1:NFRQ)
   WRITE(np,16)
   WRITE(np,17) (IFRQ(JF),JF,JF=1,NFRQ),(QFRQ(JF),JF,JF=1,NFRQ)
   WRITE(np,8) NLYR,NPRISM
 END IF
 WRITE(np,20) PLT_MOD(1:NPRISM),RES_MOD(1:NLYR),THK_MOD(1:NLYR-1)

!   WRITE(np,22) MPAR(1:NPAR)

  1 FORMAT(T1,'/ ',I4.4,T15,'File version number'/T1,'/ PROGRAM_NAME=',A/T1,'/ TITLE: ',A)
  3 FORMAT(T1,'/ UNITS=',A,3X,'NSTAT=',I3.3,3X,'NCH=',I3.3,3X,'NCMP=',I1)
  4 FORMAT(T1,'/ TIMES(ms)=',T17,100G13.4)
  5 FORMAT(T1,'/ CHNL_WDTH(ms)=',T17,100G13.4)
  6 FORMAT(T1,'/ SURVEY=TD_AEM  PP=RX_POS')
  7 FORMAT(T1,'/ LINE_HEADER'/T1,'/ DatIndx  EastTx  NorthTx  AltTx  Txcln  EastRx  NorthRx  AltRx',350(A,I3.3))
  8 FORMAT(T1,'/ LAYERS=',I2.2/T1,'/ PLATES=',I2.2)
 13 FORMAT(T1,'/ UNITS=',A,3X,'NSTAT=',I3.3,3X,'NFRQ=',I2.2,3X,'NCMP=1')
 14 FORMAT(T1,'/ FREQS(Hz) =',60F13.2)
 16 FORMAT(T1,'/ SURVEY=FD_AEM  PP=RX_POS')
 17 FORMAT(T1,'/ LINE_HEADER'/T1,'/ DatIndx  EastTx  NorthTx  AltTx  EastRx  NorthRx  AltRx  ',100(A,I2.2))
 20 FORMAT(T1,'/ MODEL_HEADER'/T1,'/ ',12A)

   END SUBROUTINE WRITE_np_INITIAL

END MODULE MA_Input_routines

 PROGRAM MAIN
!------------

!*** Calls DCPRM_FD, DCPRM_TD, FDREAD, HSBOSS_TD, HSBOSS_FD, MARCO_3D, NLSQ2,
!          SET_NORM_TD, SET_SOURCE, TDEM_3D, WRITE_FD, WRITE_TD, WRITE_MODEL,
!          WRITE_LOG_FILE

!*** Calls from MA_Input_routines:
!          READ_SYSTEM_AND_SURVEY, READ_MODEL, READ_INVRT_CNTRL_AND_DATA
!          RESET_ORIGIN, SET_CELLS, SET_FRQ, SET_SURVEY, SET_TRP, WRITE_np_INITIAL

 USE MA_Input_routines

 IMPLICIT NONE
 INTEGER IDER, KNRM
 REAL, ALLOCATABLE, DIMENSION(:) :: NORM
 REAL, ALLOCATABLE, DIMENSION(:,:,:) :: BTD,BTD_SCAT
 COMPLEX,ALLOCATABLE, DIMENSION(:,:,:) :: BFD_SCAT,BFD
 REAL CMP_START, CMP_Final, CMP_Delta
 LOGICAL WRT_np

 CALL CPU_TIME (CMP_START)
 CALL READ_SYSTEM_AND_SURVEY
 CALL READ_MODEL

 OPEN(np,FILE = 'MarcoAir.mf1',STATUS = 'REPLACE')
 CALL RESET_ORIGIN

 IF (DO3D /= 0) THEN
   IF (TDFD < 2) CALL SET_FRQ    ! Set up frequencis for time-domain work.
!   KP = 0
!   CALL SET_CELLS (KP,NPRISM,NLYR,MXAB,MXB,CELLW,THK,PLNGTH,PLWDTH,NA,NB,DA,DB, &
!                   XCNTR,YCNTR,PLTOP,PLAZM,PLDIP,PLROT,XCELL,YCELL,ZCELL,PCNR)
 END IF

 CALL SET_SURVEY
 WRT_np = .TRUE.
 IF (TDFD == 2 .AND. CMP > 1)  WRT_np = .FALSE.
 IF (WRT_np) CALL WRITE_np_INITIAL

 CALL SHOW_MODEL
 
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

 IF (TDFD < 2) THEN   ! Time-Domain

! For time-domain, set up frequencies, interpolation times
! For time-domain, call SET_SOURCE to compute dI/dt at the transmitter using
! the DC coupling if waveform at the receiver has been specified.  Then
! (for time-domain) convert PRM_TD to the peak primary dB/dt in NT if
! impulse response is output or B in pT for step response.
! SWY will be in amps/s  * Tx area * NTRN

! IDER = 0 => that current derivative must be computed: ISW = 1, 11 or 31
!      = 1 => that current derivative has specified through voltage
!             calibration: ISW = 10 or 30
!      = 4 => ISW = 4  (pure rectangular pulse)

   IDER = 0
   IF (ISW == 10 .OR. ISW == 30 .OR. ISW == 130) IDER = 1
   IF (ISW == 4) IDER = 4
   CALL SET_TRP
   KNRM = 3
   ALLOCATE (NORM(KNRM))
   IF (ISW ==4) THEN
     NORM = 1.E6
   ELSE
     CALL DCPRM_TD (XRX0,YRX0,ZRX0,TXCLN0,TXAREA,PRM_TD)
     CALL SET_SOURCE (STEP,ISW,BFFAC,WAVEFORM,NSX,SWX,SWY,PRM_TD)
   END IF
 ELSE
   KNRM = NFRQ
   ALLOCATE (NORM(KNRM))
   CALL DCPRM_FD (NFRQ,XRX,YRX,ZRX,TXCLN,TXA90,PRM_FD,PPFAC,NORM)
 END IF

   CLOSE (NR)
   ALLOCATE (BFD_SCAT(NFRQ,NSTAT,3))
   BFD_SCAT = (0.,0.)

   NEW_3D_MODEL: IF (DO3D == 1) THEN

   CALL MARCO_3D (NLG,TDFD,NFRQ,FREQ,NLYR,THK,RES,CHRG,CTAU,CFREQ,REPS,RMU,SOLVER, &
                  KACC,KSYMM,NPRISM,NCELL_NS,NCELL_EW,NCELL_Z,PRSM_SIZE_NS,        &
                  PRSM_SIZE_EW,PRSM_SIZE_Z,PRISM_NORTH,PRISM_EAST,PRISM_ZMID,      &
                  PRSM_RES,PRSM_CHRG,PRSM_TAU,PRSM_CFR,REPSP,RMUP,NRXST,NSTAT,SX,    &
                  SY,SZ,TXCLN,FANGLE,NRX,RX,RY,RZ,BFD_SCAT)

!  End of frequency stepping.
!  Write out the total frequency-domain scattered magnetic fields for each to UNIT ND.

     IF (TDFD < 2) THEN  ! Time-Domain
       DO JF = 1,NFRQ
         DO JS= 1,NSTAT
           WRITE(ND,'(6E16.6)') BFD_SCAT(JF,JS,1:3)
         END DO
       END DO

       CLOSE (ND)
       WRITE(*, 10)
     END IF
   END IF NEW_3D_MODEL

   IF (TDFD < 2) THEN   ! Time-Domain.  Compute BTD_SCAT, the Scattering Response
     ALLOCATE (BTD_SCAT(NCHNL,NSTAT,3),BTD(NCHNL,NSTAT,3))
     BTD_SCAT = 0.;  BTD = 0.

     IF (DO3D > 0) THEN
       IF (DO3D == 2) CALL FDREAD (ND,NFRQ,NSTAT,BFD_SCAT)  !  Read old frequency-domain data
       CLOSE (ND)

       CALL TDEM_3D (STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL, &
                     TOPN,TCLS,FREQ,NFRQ,FANGLE,NSTAT,BFD_SCAT,GSTRP,ASTRP,BTD_SCAT)

     END IF

!  Compute BTD, the layered earth response convolved with the excitation waveform
!  as dB/dt in nT/s if STEP = 0;  or as B in pT if STEP = 1

     CALL HSBOSS_TD (STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL,TOPN, &
                     TCLS,TXCLN,NSTAT,SAME_TX,SZ,ZRX,XRX,YRX,NLYR,RES,REPS,RMU,THK, &
                     CALF,CTAU,CFREQ,GSTRP,ASTRP,BTD)

     BTD = BTD + BTD_SCAT    !  Redefine BTD as the total response,

!  Write out the results.

     CALL WRITE_TD (NW,np,TITLE,NSTAT,LINE,SXD,SYD,SZ,TXDEG,RXD,RYD,RZ,XRX,YRX,ZRX,NCHNL, &
                    TMS,PRFL,QUNIT,BUNIT,PPFAC,BFFAC,PRTSEC,PRM_TD,CMP,KPPM,BTD_SCAT,BTD)

   ELSE  !  Construct the frequency-domain response.

!     BFD_SCAT = - BFD_SCAT
     ALLOCATE (BFD(NFRQ,NSTAT,3))
     BFD = (0.,0.)

!      The - sign above is a consequence of using the sine transform for a +iwt
!      sign convention.  It is thus consistant with the convention used for the
!      layered half space and in TDEM for time-domain scattered fields.
!      The check is that the response is consistant between a large flat plate
!      in a uniform half-space when compared with the equivalent 3-laye space.

     CALL HSBOSS_FD (NFRQ,FREQ,TXCLN,TXA90,NSTAT,SAME_TX,SZ,ZRX,XRX, &
                     YRX,NLYR,RES,REPS,RMU,THK,CALF,CTAU,CFREQ,BFD)

     BFD = BFD + BFD_SCAT  !  Redefine BFD as the total response,
     CALL WRITE_FD (NW,np,TITLE,NSTAT,LINE,TXCLN,TXA90,SXD,SYD,SZ,RXD,RYD,RZ,CONFIG,NFRQ, &
                    FREQ,PRFL,QUNIT,PPFAC,BFFAC,PRTSEC,PRM_FD,CMP,BFD_SCAT,BFD)
   END IF

  !
 ! complete run time calcs & sign off ...
 call date_and_time(Values = tvals)
 call CPU_time(CMP_final)
 CMP_delta = CMP_final - CMP_start

 Write (np, 11) trim(PNAME), 'forward model', tvals(5:7), tvals(3:1:-1), CMP_delta
 Write (nw, 12) trim(PNAME), 'forward model', tvals(5:7), tvals(3:1:-1), CMP_delta
 Write ( *, 12) trim(PNAME), 'forward model', tvals(5:7), tvals(3:1:-1), CMP_delta
 
 CLOSE (NW)
 STOP

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

 SUBROUTINE CONFIG_ID (NFRQ,TXCLN,TXA90,XRX,YRX,ZRX,CONFIG,CFG1)
!---------------------------------------------------------------

!***  Called by: READ_SYSTEM_AND_SURVEY

!  Returns CONFIG = HCP, VCA, VCP, VCB, HCA or '   ' 
!            CFG1 =  1    2    3    4    5       0
!
!        NFRQ - number of frequencies
!       TXCLN - transmitter inclination in degrees
!       TXA90 - true for vertical co-planar briadside array
!         ZRX - vertical receiver offset for each frequency;   below = positive
!         XRX - in-line receiver offset for each frequency;    behind = positive
!         YRX - transverse receiver offset for each frequency; left = positive.

 INTEGER NFRQ,JF,TXO,CFG1(NFRQ)
 REAL XABS,YABS,ZABS,RABS
 REAL, DIMENSION (NFRQ) :: TXCLN,XRX,YRX,ZRX
 LOGICAL TXA90
 CHARACTER (LEN=3) CONFIG(NFRQ), KCMPC(0:5)
 DATA KCMPC / '   ','HCP','VCA','VCP','VCB','HCA'/

 IF (TXA90) THEN
   CONFIG = 'VCB'
   CFG1 = 4
   RETURN
 END IF

 CFG1 = 0
 DO JF = 1,NFRQ
   XABS = ABS (XRX(JF))
   YABS = ABS (YRX(JF))
   ZABS = ABS (ZRX(JF))
   RABS = SQRT (XABS**2 + YABS**2)
   TXO = -1
   IF (ABS (ABS (TXCLN(JF)) - 90.) < 0.1) TXO = 90
   IF (ABS (ABS (TXCLN(JF)) - 0.)  < 0.1) TXO = 0
   IF (ZABS < 0.1 .AND. RABS > 1.) THEN
     IF (TXO == 0)  CFG1(JF) = 1
     IF (TXO == 90) THEN
       IF (XABS < 0.01) CFG1(JF) = 3
       IF (YABS < 0.01) CFG1(JF) = 2
     END IF
   ELSE IF (ZABS > 1. .AND. RABS > 0.1) THEN
     CFG1(JF) = 5
   END IF
   CONFIG(JF) = KCMPC(CFG1(JF))
 END DO

END  SUBROUTINE CONFIG_ID

 REAL FUNCTION COSTRN (WF,YFRQ,NFRQ,KFRQ,T)
!------------------------------------------

!***  Called by: HSBOSS_TD, TDEM_3D
!***      Calls: CUBVAL

! LAST MODIFICATION DATE: October, 2001

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

! The accumulation is done using 12 digit precision


 USE MA_Filter_coefficients_QL

 IMPLICIT NONE
 INTEGER, PARAMETER :: NDEC_COS=12, KFLOW=-200, KFHIGH=99
 REAL, PARAMETER :: FAC=.7978846, TOL=1.0E-6
 INTEGER J1,NFRQ,KFRQ
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
   IF (YS > WF(KFRQ)) EXIT MOVE_HIGH
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
   IF (YS > WF(KFRQ)) CYCLE
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

  REAL FUNCTION CUBDER (XKNOT, COEF, KNOT, X1)
! --------------------------------------------

!***  Called by: FOLD_AND_CONVOLVE
!***      Calls: INTERV.  On exit from INTERV

!  Evaluates the first derivative of a function from its cubic spline
!  interpolation.

!       MFLAG = -1  => X is to the left of interpolated range
!             =  1  => X is to the right of interpolated range
!             =  0  => X is in the interpolated range

!         KNOT - total number of knots including endpoints.
!     XKNOT(I), I = 1,KNOT - location of the knots.  The rightmost data
!                            point used to calculate coefficients is not
!                            used.
!     COEF(J,I), J = 1,4; I = 1,KNOT = Jth derivative at H = 0
!                                      where  H = X - XKNOT(I)
!******************************************************************************
!******************************************************************************

  IMPLICIT NONE
  INTEGER I,MFLAG,KNOT
  REAL XKNOT(KNOT), COEF(4,KNOT), X1, H

!  Find index i of largest breakpoint to the left of X1.

  CALL INTERV ( XKNOT, KNOT-1, X1, I, MFLAG )
  H = X1 - XKNOT(I)
  IF (MFLAG == -1) H = 0.

  CUBDER = (COEF(4,I)*H/2. + COEF(3,I) ) *H + COEF(2,I)

END FUNCTION CUBDER

  REAL FUNCTION CUBINT (XKNOT, COEF, KNOT, X1, X2)
! ------------------------------------------------
!
!***  Called by:  EGT_BOSS TXCNVD, TXCNVL
!***      Calls: INTERV.  On exit from INTERV

!  Integrates a function from X1 to X2 using its cubic spline representation.

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

!***  Called by: EGT_CSPL, FOLD_AND_CONVOLVE, HSBOSS_TD, INTER_EGT_CSPL, MGTBS,
!                PRM_BOSS, TDEM_3D, TQSTRIP, TXCNVD,
!

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

!***  Called by: COSTRN, EGT_BOSS, FOLD_AND_CONVOLVE, INTER_EGT_BOSS,
!                MGTV1, PRM_BOSS, TXCNVD, TXCNVL
!
!***      Calls: INTERV.

!  On exit from INTERV,
!  Evaluates a function at X1 from from its cubic spline representation.

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

 SUBROUTINE DCPRM_FD (NFRQ,XRX,YRX,ZRX,TXCLN,TXA90,PRM_FD,PPFAC,NORM)
!--------------------------------------------------------------------

!***  Called by: MAIN

!  In frequency-domain, it computes the maximally coupled component of B at each
!  receiver location for each frequency assuming unit dipoles and current
!  transmitters are co-oriented.
!
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!  SIGN CONVENTION:
!  ----------------
!  The normal layered earth field coordinate system used in this
!  subroutine has X (JC=1) positive along the flight path, Y (JC=2)
!  positive to starboard, and Z (JC=3) positive down.
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
!                               INPUT
!                               -----
!       NFRQ - number of frequencies
!        ZRX - vertical offset of RX relative to transmitter   (below = + ).
!        XRX - in-line offset of RX relative to transmitter    (behind = + ).
!        YRX - transverse offset of RX relative to transmitter (left = + ).
!
!       PPFAC = 100  => parts per hundred (percent) pct
!            or 1000 => parts per thousand (ppt)
!            or 1.e6 => parts per million (ppm)
!            or 1.e9 => parts per billion (ppb)
!
!
!                                 OUTPUT (frequency domain)
!                                 -------------------------
!     PRM_FD(1:NFRQ)  = primary B (nT) per unit dipole moment at each frequency.
!       NORM(1:NFRQ)  = PPM normalisation factor for fields expresed in nT

 IMPLICIT NONE
 INTEGER NFRQ,JF
 REAL SNTX,CSTX,XBD,YBD,ZBD,RBRD,RSQ,RSQ1,BFAC,FAC,FACZX,INLINE,VERT,PPFAC
 REAL, DIMENSION(NFRQ) :: TXCLN,XRX,YRX,ZRX,PRM_FD,NORM
 LOGICAL COPLANAR,TXA90

!  BFAC = 1.0E9 * MU / (4 * PI)  ! NANOTESLAS

 PRM_FD = 0.
 BFAC = 100.

 DO JF = 1,NFRQ
   SNTX = SIN (TXCLN(JF))
   CSTX = COS (TXCLN(JF))


   XBD = -XRX(JF)  !  XRX is defined as positive behind the TX.
   YBD = YRX(JF)
   RBRD = SQRT (XBD**2 + YBD**2)
   ZBD = ZRX(JF)
   RSQ = ZBD**2 + RBRD**2
   RSQ1 = SQRT (RSQ)
   COPLANAR = .FALSE.
   IF (ABS (SNTX) < .01) COPLANAR = .TRUE.
   IF (TXA90) COPLANAR = .TRUE.
   IF (COPLANAR) THEN
     PRM_FD(JF) = -BFAC / RSQ1**3
   ELSE
     FAC = BFAC / RSQ1**5
     FACZX = 3. * FAC * XBD * ZBD
     VERT = FAC * CSTX * (3.* ZBD**2 - RSQ) + SNTX * FACZX
     INLINE = FAC * SNTX * (3.* XBD**2 - RSQ) + CSTX * FACZX
     PRM_FD(JF) = CSTX * VERT + SNTX * INLINE
   END IF
   NORM(JF) = PPFAC / ABS (PRM_FD(JF))
 END DO

END SUBROUTINE DCPRM_FD

 SUBROUTINE DCPRM_TD (XRX0,YRX0,ZRX0,TXCLN0,TXAREA,PRM_TD)
!---------------------------------------------------------

!***  Called by: READ_INVERSION_CONTROL

! For time-domain, PRM_TD is the 3 component Tx-Rx dc coupling factor
! per unit dipole moment, expressed in NANOTESLAS per unit amp
! Multiply it by dI/dt and get nanovolts per m^2 which is the
! same as nT/s.  Multiply it by current and get nT.
!
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!  SIGN CONVENTION:
!  ----------------
!  The normal layered earth field coordinate system used in this
!  subroutine has X (JC=1) positive along the flight path, Y (JC=2)
!  positive to starboard, and Z (JC=3) positive down.
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
!                               INPUT
!                               -----
!      TXCLN0 - angle in degrees that the transmitting dipole makes with vertical
!              (climb = positive for VMD transmitter)
!      TXAREA - transmitter area in sq. metres
!      ZRX0, XRX0 & YRX0 are the initial vertical, in-line and transverse offsets
!                        of the receiver relative to transmitter
!                        below = + ;  behind = + ;  left = +
!
!                                 OUTPUT (time domain)
!                                 --------------------
!     PRM_TD = primary field coupling factor for B in nT (unit TX moment)
!     PRM_TD(1) = in-line B
!     PRM_TD(2) = transverse B
!     PRM_TD(3) = vertical B for station

 IMPLICIT NONE
 REAL, PARAMETER :: PI=3.141592654
 REAL SNTX,CSTX,XBD,YBD,ZBD,RBRD,RSQ,RSQ1,BFAC,FAC,FACZX,TXAREA, &
      TXCLN0,THETA,XRX0,YRX0,ZRX0,INLINE,VERT,TRANS,PRM_TD(3)


!  BFAC = 1.0E9 * MU / (4 * PI) for time domain  ! NANOTESLAS

 PRM_TD = 0.

!----------------------------------------------------------------------------
! In-loop time-domain HEM

 IF (TXAREA > 1.) THEN
   RBRD = ABS (XRX0) + ABS (YRX0)
   IF (RBRD < 1.) THEN
     ZBD = SQRT (ZRX0**2 + TXAREA / PI)
     PRM_TD(3) = 200. / ZBD**3       ! 1.0E9 * MU / (2 * PI) = 200. (nT)
     RETURN
   END IF
 END IF
!----------------------------------------------------------------------------

 BFAC = 100.
 THETA = TXCLN0 * PI / 180.
 SNTX = SIN (THETA)
 CSTX = COS (THETA)

 XBD = -XRX0  !  XRX is defined as positive behind the TX.
 YBD = YRX0
 RBRD = SQRT (XBD**2 + YBD**2)
 ZBD = ZRX0
 RSQ =  ZBD**2 + RBRD**2
 RSQ1 = SQRT (RSQ)
 FAC = BFAC / RSQ1**5
 FACZX = 3. * FAC * XBD * ZBD
 VERT = FAC * CSTX * (3.* ZBD**2 - RSQ) + SNTX * FACZX
 INLINE = FAC * SNTX * (3.* XBD**2 - RSQ) + CSTX * FACZX
 TRANS = 3.* FAC * YBD * ((CSTX * ZBD) + (SNTX * XBD))
 PRM_TD(1) = INLINE
 PRM_TD(2) = TRANS
 PRM_TD(3) = VERT

END SUBROUTINE DCPRM_TD

 SUBROUTINE FDREAD (ND,NFRQ,NSTAT,BFD_SCAT)
!------------------------------------------

!*** Called by: MAIN

!  Reads frequency-domain scattered impulse magnetic field data
!  (real & imaginary components) from logical UNIT ND into
!  array BFD_SCAT for conversion to time-domain by  TDEM_OUT.

!          NFRQ - number of frequencies
!         NSTAT - number of transmitter positions
!  BFD_SCAT(I,J,K) - Kth component of the complex frequency-domain impulse
!                      response magnetic field (H) at transmitter J
!                      for frequency I.  (nT)

  IMPLICIT NONE
  INTEGER ND,NFRQ,NSTAT,JF,JS,JC
  REAL A(6)
  COMPLEX BFD_SCAT(NFRQ,NSTAT,3)

  INTENT (IN) ND,NFRQ,NSTAT
  INTENT (INOUT) BFD_SCAT

  DO JF = 1,NFRQ
    DO JS = 1,NSTAT
      READ(ND,*) A(1:6)
      DO JC = 1,3
        BFD_SCAT(JF,JS,JC) = CMPLX (A(2*JC-1), A(2*JC))
      END DO
    END DO
  END DO
END SUBROUTINE FDREAD

 SUBROUTINE HSBOSS_TD (STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL,TOPN, &
                       TCLS,TXCLN,NSTAT,SAME_TX,SZ,ZRX,XRX,YRX,NLYR,RES,REPS,RMU,THK, &
                       CALF,CTAU,CFREQ,GSTRP,ASTRP,BTD)
!------------------------------------------------------------------------------------

!***  Called by: MAIN, GET_FWD_MODL
!***      Calls: COSTRN, CUBSPL HSMD_FD, FOLD_AND_CONVOLVE

!  Computes BTD, the time-domain layered earth response convolved with the
!  excitation waveform and the receiver channels per unit receiver area.
!  For impulse response, it computes dB/dt in nT / s which is the same as
!  nanovolts per unit area.

!  For step response, it computes B in nanoteslas.

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!  SIGN CONVENTION:
!  ----------------
!  The normal layered earth field coordinate system used in this
!  subroutine has X (JC=1) positive along the flight path, Y (JC=2)
!  positive to starboard, and Z (JC=3) positive down.
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

!                             INPUT
!                             -----
!       STEP = 1 iff step response is to be computed
!       IDER = 1 if source waveform was dB/dt; = 0 if amps pr B
!        NSX - number of points used to discretise transmitter signal
!        SWX - abscissae (seconds) of current waveform
!        SWY - dI/dt * Tx moment & nanotesla conversion at times SWX
!      NPULS - number of bipolar pulses of length PULSE
!      PULSE - length of half-cycle on pulse plus off-time
!      NTYRP - number of values in TRP for total signal length: 2 * NPULS *PULSE
!        TRP - array of time values for FD -> TD transformations
!     NTYPLS - number of TRP values in 1 PULSE
!      NCHNL - number of channels
!       TOPN - time at which receiver channel I opens.
!       TCLS - time at which receiver channel I closes.
!      TXCLN - angle in radians that TX dipole makes with vertical (climb = +)
!      NSTAT - number of stations in survey line.
!    SAME_TX - used to avoid repeat computations
!         SZ - array of transmitter altitudes
!        ZRX - vertical offset of RX at each station from transmitter  (below = +)
!        XRX - in-line horizontal offset of RX at each station J;      (behind = +)
!        YRX - transverse offset of RX at each station J               (left = +)
!       NLYR - number of layers
!        RES - array of layer resistivities
!       REPS - relative dielectric constant
!        RMU - mu(i) / mu(0)
!        THK - array of layer thicknesses
!     CALF, CTAU, CFREQ are the layered earth Cole-Cole parameters.
!
!                             OUTPUT
!                             ------
!     BTD(JT,JS,1) - the in-line component of the layered earth response at
!                    time JT, station JS.
!     BTD(JT,JS,2) - the horizontal transverse component
!     BTD(JT,JS,3) - the vertical component

 USE MA_Frequency_select

 IMPLICIT NONE
 INTEGER, PARAMETER :: NFRQ=NF_6PDE, NRXF=1, QL=SELECTED_REAL_KIND(12,80)
 REAL, PARAMETER :: TWOPI=6.283185307
 INTEGER STEP,IDER,NSX,NPULS,NTYPLS,NTYRP,NCHNL,NSTAT,NLYR,TDFD,GSTRP,ASTRP, &
         JS,JF,JT,JC
 REAL SWX(NSX),SWY(NSX,3),PULSE,TRP(NTYRP),SZ(NSTAT),ALT,T,YPRM(4,NTYRP),   &
      YCUM(NCHNL),YFRQ(4,NFRQ),FREQ(NFRQ),WF(NFRQ),COSTRN,BTD(NCHNL,NSTAT,3)
 REAL, DIMENSION(NCHNL) :: TOPN,TCLS
 REAL, DIMENSION(NLYR) :: RES,REPS,THK,CTAU,CFREQ,CALF,RMU
 REAL, DIMENSION(NSTAT) :: TXCLN,ZRX,XRX,YRX
 REAL(KIND=QL), DIMENSION(NRXF) :: XRXD,YRXD,ZRXD,TXCLND
 COMPLEX(KIND=QL) BFDD(NFRQ,3)
 COMPLEX BFD
 LOGICAL TXA90,SAME_TX(NSTAT)

 INTENT (IN) STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,NCHNL,TOPN,TCLS,SZ, &
             TXCLN,NSTAT,ZRX,XRX,YRX,NLYR,THK,RMU,CALF,CTAU,CFREQ
 INTENT (OUT) BTD
 INTENT (INOUT) TRP

 BTD = 0.
 TDFD = 1
 TXA90 = .FALSE.

 FREQ(1:NFRQ) = FRQ_6PDE(1:NFRQ)
 WF(1:NFRQ) = LOG (TWOPI * FREQ(1:NFRQ))

 DO JS = 1, NSTAT
   IF (SAME_TX(JS)) THEN
     DO JC = 1,3
       BTD(1:NCHNL,JS,JC) = BTD(1:NCHNL,JS-1,JC)
     END DO
   ELSE
     TXCLND(1) = REAL (TXCLN(JS), KIND=QL)
     ALT = SZ(JS)
     ZRXD(1) = REAL (ZRX(JS), KIND=QL)
     XRXD(1) = REAL (XRX(JS), KIND=QL)
     YRXD(1) = REAL (YRX(JS), KIND=QL)

     CALL HSMD_FD (NFRQ,FREQ,ALT,NRXF,TXCLND,TXA90,ZRXD,XRXD,YRXD,NLYR, &
                   THK,RES,REPS,RMU,CALF,CTAU,CFREQ,TDFD,BFDD)

!    Compute BTD, the 'observed' layered earth response by folding the BLEXT,
!    the extended response over NPULS bipolar cycles into 1 PULSE and then
!    convolving this with the TX waveform.  It is during the convolution that we
!    shift from teslas to nanoteslas or nT/s.

     YFRQ = 0.
     DO JC = 1,3
       DO JF = 1,NFRQ
         BFD = CMPLX (BFDD(JF,JC) )
         YFRQ(1,JF) = AIMAG (BFD) / (TWOPI * FREQ(JF) )
       END DO
       CALL CUBSPL (WF,YFRQ,NFRQ,0,0)

       YPRM = 0.
       DO JT = 1, NTYRP   !  Convert to step-function time-domain.
         T = TRP(JT)
         YPRM(1,JT) = COSTRN (WF,YFRQ,NFRQ,NFRQ,T)
       END DO
       CALL FOLD_AND_CONVOLVE (STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,TRP,NTYPLS, &
                               NTYRP,NCHNL,TOPN,TCLS,YPRM,GSTRP,ASTRP,YCUM)

       BTD(1:NCHNL,JS,JC) = YCUM(1:NCHNL)
     END DO
   END IF
 END DO

END SUBROUTINE HSBOSS_TD

 SUBROUTINE HSBOSS_FD (NFRQ,FREQ,TXCLN,TXA90,NSTAT,SAME_TX,SZ,ZRX,XRX, &
                       YRX,NLYR,RES,REPS,RMU,THK,CALF,CTAU,CFREQ,BFD)
!----------------------------------------------------------------------

!  Computes the frequency-domain layered earth H field for a dipole of
!  unit moment and current.

!***  Called by: MAIN, GET_FWD_MODL
!***      Calls: HSMD_FD

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!  SIGN CONVENTION:
!  ----------------
!  The normal layered earth field coordinate system used in this
!  subroutine has X (JC=1) positive along the flight path, Y (JC=2)
!  positive to starboard, and Z (JC=3) positive down.
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

!                             INPUT
!                             -----
!      FREQ - array of NFRQ frequencies
!     TXCLN - angle in radians that TX dipole makes with vertical (climb = +)
!     TXA90 - true for vertical co-planar briadside array
!     NSTAT - number of stations in survey line.
!   SAME_TX- used to reduce redundant computations
!        SZ - array of transmitter altitudes
!       ZRX - vertical offset of each receiver from transmitter  (below = +)
!       XRX - in-line horizontal offset of RX J;                 (behind = +)
!       YRX - transverse horizontal offset of RX J               (left = +)
!      NLYR - number of layers
!       RES - layer resistivities
!      REPS - array of relative dislectric constants
!      RMUX - mu(i) / mu(0)
!       THK - array of layer thicknesses
!     CALF, CTAU, CFREQ are the layered earth Cole-Cole parameters.
!
!                             OUTPUT
!                             ------
!     BFD(JF,JS,1) - the in-line component of the layered earth response at
!                    time JT, station JS. (nT)
!     BFD(JF,JS,2) - the transverse component
!     BFD(JF,JS,3) - the vertical component

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80)
 COMPLEX, PARAMETER :: ZERO=(0.,0.)
 INTEGER NFRQ,NSTAT,NLYR,JS,JC,TDFD,NRXF
 REAL SZ(NSTAT),ALT
 REAL, DIMENSION (NFRQ) :: FREQ,TXCLN,ZRX,XRX,YRX
 REAL, DIMENSION(NLYR) :: RES,RMU,REPS,THK,CTAU,CFREQ,CALF
 REAL(KIND=QL), DIMENSION(NFRQ) :: XRXD,YRXD,ZRXD,TXCLND
 COMPLEX(KIND=QL) BFDD(NFRQ,3)
 COMPLEX BFD(NFRQ,NSTAT,3)
 LOGICAL TXA90,SAME_TX(NSTAT)

!  Compute layered earth fields BLE_LYR at first station for each different altitude.

 TDFD = 2
 NRXF = NFRQ
 ZRXD(1:NFRQ) = REAL (ZRX(1:NFRQ), KIND=QL)
 XRXD(1:NFRQ) = REAL (XRX(1:NFRQ), KIND=QL)
 YRXD(1:NFRQ) = REAL (YRX(1:NFRQ), KIND=QL)
 TXCLND(1:NFRQ) = REAL (TXCLN(1:NFRQ), KIND=QL)

 BFD = ZERO
 IF (TXA90) BFD = 4.* ZERO     ! dummy statement

 DO JS = 1, NSTAT
   IF (SAME_TX(JS)) THEN
     DO JC = 1,3
       BFD(1:NFRQ,JS,JC) = BFD(1:NFRQ,JS-1,JC)
     END DO
   ELSE

     ALT = SZ(JS)
     CALL HSMD_FD (NFRQ,FREQ,ALT,NRXF,TXCLND,TXA90,ZRXD,XRXD,YRXD,NLYR, &
                   THK,RES,REPS,RMU,CALF,CTAU,CFREQ,TDFD,BFDD)

     BFD(1:NFRQ,JS,1:3) = CMPLX (BFDD(1:NFRQ,1:3))
   END IF
 END DO

END SUBROUTINE HSBOSS_FD

 SUBROUTINE HSMD_FD (NFRQ,FREQ,ALT,NRXF,TXCLND,TXA90,ZRXD,XRXD,YRXD,NLYR, &
                     THK,RES,REPS,RMU,CALF,CTAU,CFREQ,TDFD,BFDD)
!------------------------------------------------------------------------

!***  Called by: HSBOSS_TD, HSBOSS_FD
!***      Calls: HSMD_HNK

!  Computes the frequency-domain layered earth magnetic field for a dipole of
!  unit moment and current.

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!  SIGN CONVENTION:
!  ----------------
!  The normal layered earth field coordinate system used in this
!  subroutine has X (JC=1) positive along the flight path, Y (JC=2)
!  positive to starboard, and Z (JC=3) positive down.
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

!                             INPUT
!                             -----
!      FREQ - array of NFRQ frequencies
!       ALT - transmitter altitude above earth
!      NRXF - receiver offset dimension = NFRQ in frequency-domain and 1 in time-domain
!    TXCLND - angle in radians (QL) that TX dipole makes with vertical (climb = +)
!     TXA90 - true for vertical co-planar briadside array
!      ZRXD - vertical receiver offset (QL) from transmitter, (below = +)
!      XRXD - in-line horizontal offset (QL)  of RX J;        (behind = +)
!      YRXD - transverse horizontal offset (QL) of RX J       (left = +)
!      NLYR - number of layers
!       THK - array of layer thicknesses
!       RES - array of layer resistivities
!      REPS - array of relative dielectric constants for each layer
!       RMU - array of relative magnetic permeabilities for each layer
!     CALF, CTAU, CFREQ are the layered earth Cole-Cole parameters.
!     TDFD  = 1 for time domain;  = 2 for frequency-domain
!
!                             OUTPUT
!                             ------
!   BFDD(1:NFRQ,3) - vertical magnetic field in nT
!   BFDD(1:NFRQ,1) - in-line magnetic field in nT
!   BFDD(1:NFRQ,2) - transverse magnetic field in nT


 IMPLICIT NONE

 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80)
 REAL, PARAMETER :: TWOPI=6.2831853, C_LIGHT = 2.99793E8
 COMPLEX(KIND=QL), PARAMETER :: ZERO=(0._QL,0._QL)
 INTEGER NRXF,NFRQ,NLYR,ICOLE(NLYR),JF,JL,TDFD,JQ
 REAL W,RES(NLYR),FREQ(NFRQ),ALT
 REAL(KIND=QL) SIG0(NLYR),THKD(NLYR),RMUX(NLYR),SNTX,CSTX,ZRFD,RHOD,XBRQ,XBRQ2,YBRQ
 REAL(KIND=QL), DIMENSION(NRXF) :: TXCLND,ZRXD,XRXD,YRXD
 REAL, DIMENSION(NLYR) :: THK,REPS,RMU,CTAU,CFREQ,CALF
 COMPLEX(KIND=QL) IW,DISPD,HLYR(3), BFDD(NFRQ,3),VERT,INLINE,TRANS
 LOGICAL TXA90

 ICOLE = 0
 DO JL = 1,NLYR
   IF (CFREQ(JL) > 1.E-3 .AND. CTAU(JL) > 1.E-12) THEN
     ICOLE(JL) = 1
   END IF
 END DO

! Set extended precision variables

 SIG0(1:NLYR) = REAL (1. / RES(1:NLYR), KIND=QL)
 THKD = 0._QL
 THKD(1:NLYR-1) = REAL (THK(1:NLYR-1), KIND=QL)
 RMUX = REAL (RMU, KIND=QL)

!  Compute layered earth fields BLE_LYR at first station for each different altitude.

 BFDD = ZERO

 JQ = 1
 DO JF = 1,NFRQ
   IF (TDFD == 2) JQ = JF
   ZRFD = REAL (2.* ALT, KIND=QL) - ZRXD(JQ)  ! Reflected distance from TX to ground to RX
   RHOD = SQRT (XRXD(JQ)**2 + YRXD(JQ)**2)
   IF (RHOD > .01_QL) THEN
     XBRQ = -REAL (XRXD(JQ)) / RHOD  !  XRXD is defined + in negative direction
     YBRQ =  REAL (YRXD(JQ)) / RHOD
   ELSE
     RHOD = .01_QL
     XBRQ =  0._QL
     YBRQ =  0._QL
   END IF
   XBRQ2 = XBRQ**2

   W = TWOPI * FREQ(JF)
   IW = CMPLX (0.D0, W, KIND=QL)
   DISPD = (IW / C_LIGHT)**2
   CALL HSMD_HNK (IW,NLYR,THKD,DISPD,SIG0,REPS,RMUX,ICOLE,CALF,CTAU,CFREQ,ZRFD,RHOD,HLYR)

   IF (TXA90) THEN
     BFDD(JF,2) = HLYR(3)

   ELSE

     SNTX = SIN (TXCLND(JQ))
     CSTX = COS (TXCLND(JQ))

     VERT = (CSTX * HLYR(1)) + (XBRQ * SNTX * HLYR(2))

     INLINE = SNTX * ( (1._QL - 2._QL*XBRQ2) * HLYR(3) + XBRQ2 * HLYR(1)) &
                   - CSTX * XBRQ * HLYR(2)

     TRANS = SNTX * XBRQ * YBRQ * (HLYR(1) - 2._QL* HLYR(3)) &
                         - CSTX * YBRQ * HLYR(2)
     BFDD(JF,3) = VERT
     BFDD(JF,1) = INLINE
     BFDD(JF,2) = TRANS

   END IF
 END DO

END SUBROUTINE HSMD_FD

 SUBROUTINE HSMD_HNK (IW,NLYR,THKD,DISPD,SIG0,REPS,RMUX,ICOLE,CALF,CTAU,CFREQ,ZRFD,RHOD,HLYR)
!-------------------------------------------------------------------------------------------

!***  Called by: HSMD_FD
!***      Calls: HS_JMP, HSMD_KER

!  VALID FOR ANY NUMBER OF LAYERS
!  Magnetic dipole transmitter & receiver above or on earth surface

!  Computes transform integrals HLYR(3) which are used to compute vertical
!  and horizontal frequency-domain magnetic field components at the RX from
!  VMD and HMD sources.  It evaluates the Hankel transform integral using a
!  15 points per decade filter coefficient set derived from Christensen's
!  FLTGEN program.

!      IW - iw  angular frequency *(0.,1.)
!    RHOD - horizontal TX -> RX distance.
!     KER - stores kernel values from HSMD_KER

!  NLYR,SIG0,REPS,RMUX,THK,ICOLE,CALF,CTAU,CFREQ,ZRFD
!  are described in HSMD_FD
!
!    OUTPUT is HLYR(1:3)  forward model components

 USE MA_Filter_coefficients_QL

 IMPLICIT NONE
 REAL(KIND=QL), PARAMETER :: VFAC0=100._QL
 INTEGER NLYR,I,ICOLE(NLYR)
 REAL, DIMENSION(NLYR) :: REPS,CTAU,CFREQ,CALF
 REAL(KIND=QL) DEL_JN,RHO_JN,Y,LMBDA,RHOD,ZRFD,SIG0(NLYR),THKD(NLYR),RMUX(NLYR)
 COMPLEX(KIND=QL) IW,HLYR(3),DISPD,QFD
 LOGICAL JUMP

 DEL_JN = LOG (10.D0)/ DBLE (NDEC_JN)
 RHO_JN = -LOG (RHOD) - SHFTJN

 HLYR = (0._QL, 0._QL)

 DO I = -50, JNHI             ! Start at I = -50 to pick up low values.
   Y = RHO_JN + DBLE(I) * DEL_JN
   LMBDA = EXP (Y)
   CALL HSMD_KER (IW,LMBDA,NLYR,THKD,DISPD,SIG0,REPS,RMUX,ICOLE,CALF,CTAU,CFREQ, &
                  ZRFD,QFD)
   CALL HS_FRQ_JMP
   IF (JUMP .AND. I > -40) EXIT
 END DO

 JUMP = .FALSE.           ! Finish off the low end for RHOTRP(1)
 DO I = -51, JNLO, -1
   Y = RHO_JN + DBLE (I) * DEL_JN
   LMBDA = EXP (Y)
   CALL HSMD_KER (IW,LMBDA,NLYR,THKD,DISPD,SIG0,REPS,RMUX,ICOLE,CALF,CTAU,CFREQ, &
                  ZRFD,QFD)

   CALL HS_FRQ_JMP
   IF (JUMP .AND. I < -60) EXIT
 END DO

 HLYR = VFAC0 * HLYR / RHOD

  CONTAINS

   SUBROUTINE HS_FRQ_JMP
!  ---------------------

!***  Called by: HSMD_HNK

!  Accumulates function calls for the Hankel transformation &
!  checks convergence.

     REAL(KIND=QL), PARAMETER :: TOL=1.D-6, TOL2=1.D-35
     INTEGER JINT
     REAL(KIND=QL) QR,QI
     COMPLEX(KIND=QL) FW(3)

     FW(1) = WJ0(I) * QFD * LMBDA
     FW(2) = WJ1(I) * QFD * LMBDA
     FW(3) = WJ1(I) * QFD / RHOD

     HLYR = HLYR + FW

     JUMP = .TRUE.
     DO JINT = 1,3
       QR = ABS (REAL  (HLYR(JINT), KIND=QL) )
       QI = ABS (AIMAG (HLYR(JINT) ) )
       IF (QR > TOL2 .AND. ABS (REAL  (FW(JINT))) > TOL * QR) JUMP = .FALSE.
       IF (QI > TOL2 .AND. ABS (AIMAG (FW(JINT))) > TOL * QI) JUMP = .FALSE.
     END DO

   END SUBROUTINE HS_FRQ_JMP

END SUBROUTINE HSMD_HNK

 SUBROUTINE HSMD_KER (IW,LMBDA,NLYR,THKD,DISPD,SIG0,REPS,RMUX,ICOLE,CALF,CTAU, &
                      CFREQ,ZRFD,QFD)
!-----------------------------------------------------------------------------

!***  Called by: HSMD_SNTR

!  Kernel for dipole transmitter and receiver above earth.
!
!          Input
!          -----
!      IW - iw = complex angular frequency
!   LMBDA = Hankel transform variable
!    NLYR - number of layers
!    SIG0 = real conductivities ( 1. / RES)
!    REPS - array of relative dielectric constants
!    RMUX - mu(i) / mu(0)
!    THKD - layer thicknesses
!   ICOLE - C-C layer indicator array. (0 for pure real, 1 for C-C layer)
!    CALF - complementary chargeability array; ie., CALF(I) = 1.0 - CHRG(I)
!    CTAU - array of layer relaxation times (sec).
!   CFREQ - array of layer frequency parameters.
!    ZRFD - reflected distance from transmitter to earth to receiver
!
!          Output
!          ------
!  QFD  forward model kernel
!

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80)
 REAL(KIND=QL), PARAMETER :: EPS0=8.854156D-12, MU0=12.56637D-7, EXP_TOL=80.D0
 COMPLEX(KIND=QL), PARAMETER :: ONE=(1._QL,0._QL),ZERO=(0._QL,0._QL)
 INTEGER NLYR,ICOLE(NLYR),J
 REAL, DIMENSION(NLYR) :: CALF,CTAU,CFREQ,REPS
 REAL(KIND=QL) XP0,SIG0(NLYR),THKD(NLYR),LMBDA,ZRFD,RMUX(NLYR),RMUSQ(NLYR)
 COMPLEX(KIND=QL) DISPD,S0,T0,IW,P,LMBSQ,EP,SIGL(NLYR),T(NLYR),QFD
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: F,XP,KSQ,S,E

 KSQ = ZERO;   E = ZERO; XP = ZERO; T = ZERO

 LMBSQ = CMPLX (LMBDA * LMBDA, 0._QL, KIND=QL)
 S0 = SQRT (LMBSQ - DISPD)
 XP0 = -LMBDA * EXP (-LMBDA * ZRFD)

 SIGL(1:NLYR) = CMPLX (SIG0(1:NLYR), 0._QL, KIND=QL)
 DO J = NLYR, 1, -1
   RMUSQ(J) = RMUX(J) * RMUX(J)
   P = (IW * CTAU(J) )**CFREQ(J)
   P = ICOLE(J) * P
   SIGL(J) = SIGL(J) * (ONE + P) / (ONE + CALF(J)*P)
   SIGL(J) = SIGL(J) + IW * EPS0 * REPS(J)  !  Add in displacement term
   KSQ(J) = IW * MU0* RMUX(J) * SIGL(J)
   S(J) = SQRT (KSQ(J) + LMBSQ)

   IF (J == NLYR) CYCLE

   EP = 2.D0* S(J) * THKD(J)
   IF ( REAL (EP) < EXP_TOL) THEN
     E(J) = EXP (-EP)
   END IF
   T(J)= ( (RMUSQ(J+1) - RMUSQ(J)) * LMBSQ + RMUSQ(J+1)* KSQ(J) - RMUSQ(J)* KSQ(J+1) )  &
                                           / (RMUX(J+1)*   S(J) +  RMUX(J)*   S(J+1) )**2
 END DO

 T0 = ( (RMUSQ(1) - 1._QL) * LMBSQ - KSQ(1) ) / ( RMUX(1)*S0 + S(1) )**2
 XP(1:NLYR-1) = E(1:NLYR-1)
 F = ZERO
 DO J = NLYR - 1, 1, -1
   F(J) = XP(J) * (T(J) + F(J+1) ) / (ONE + T(J) * F(J+1))
 END DO

 QFD = XP0 * (T0 + F(1)) / (ONE + T0 * F(1))

END SUBROUTINE HSMD_KER

 SUBROUTINE INTERV (XT, LXT, X, LEFT, MFLAG)
!-------------------------------------------

!***   Called by: CUBVAL, CUBINT, CUBDER, LINVAL

!---  Restructured April, 1997

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

 REAL FUNCTION LINVAL (NX,XVAL,YVAL,X1,IDER)
!-------------------------------------------

!***  Called by: TXCNVD
!***      Calls: INTERV

!  Evaluates a function at X1 from from its linear representation.
!
!           On exit from INTERV
!
!       MFLAG = -1  => X is to the left of interpolated range
!             =  1  => X is to the right of interpolated range
!             =  0  => X is in the interpolated range

!
!     XVAL(1:NX) - location of the abscissa knots.  The rightmost data point
!                  used to calculate coefficients is not included.
!
!     YVAL(1:NX,1) = function values.
!     YVAL(1:NX,2)   may be populated but aren't used.
!
!     If IDER = 0, the value in the interval is that of the leftmost knot.
!                  because the derivative has been computed using two knot
!                  values and stored at the left node.
!
!     If IDER = 1, the value is a linear interpolation between the knots.
!
!             METHOD
!             ------
!
!  The interval index I, appropriate for X, is found through a call to INTERV.
!  The formula for F is evaluated using nested multiplication.
!******************************************************************************

 IMPLICIT NONE
 INTEGER I,MFLAG,NX,IDER
 REAL XVAL(NX),YVAL(NX,3),X1,H

 INTENT (IN) NX,XVAL,YVAL,X1,IDER
!
!  Find index I of largest breakpoint to the left of X1.
!
 CALL INTERV ( XVAL, NX-1, X1, I, MFLAG )

 IF (IDER == 0) THEN      !  Computed derivative values stored at right node (26.01.00)
   LINVAL = YVAL(I+1,1)
 ELSE
   H = X1 - XVAL(I)
   IF (MFLAG == -1) H = 0.

   LINVAL = YVAL(I,1) + H * (YVAL(I+1,1) - YVAL(I,1)) / (XVAL(I+1) - XVAL(I))
 END IF

END FUNCTION LINVAL

 SUBROUTINE SET_SOURCE (STEP,ISW,BFFAC,WAVEFORM,NSX,SWX,SWY,PRM_TD)
!------------------------------------------------------------------

! For time-domain, SET_SOURCE computes dI/dt at the transmitter using
! the DC coupling if waveform at the receiver has been specified.  Then
! (for time-domain) it converts PRM_TD to the peak primary dB/dt in nT/s
! if impulse response is required or B in nT for step response.
!
! SWY will be in amps / sec * Tx area * NTRN
!  Computes SWY to be TXMNT * dI(t)/dt & PKSX, the peak response.
!  The units of SWY are amps * m^2 / s

!*** Called by: MAIN

!             INPUT
!             -----
!
!       STEP = 0 for impulse response (dB/dt)
!            = 1 for step response (B)
!        ISW - waveform indicator
!      BUNIT & BFAC are set in SUBROUTINE READ_SYSTEM_SETUP
!      BUNIT can have values nT/s, nT, pT, pT/s, fT or fT/s
!      BFFAC is the conversion factor needed to achieve this from nT or nT/s
!            = 1, 1000, or 1E6
!   WAVEFORM - amps * TXMNT at the transmitter if ISW = 1
!            - vertical dB/dt if ISW = 30
!            - vertical B if ISW = 31
!            - horizontal dB/dt if ISW = 10
!            - horizontal B if ISW = 11
!        NSX - number of source points in waveform
!        SWX - time abscissae for input waveform in seconds
!
!   PRM_TD(I) = Ith component of B in nT per unit dipole moment
!               at the receiver
!
!        I = 1, 2 & 3 are the in-line, transverse & vertical components.
!
!             OUTPUT
!             ------
!
!        SWY - TRXMNT * dI(t)/dt  amps/s * m^2
!     PRM_TD - peak primary dB/dt in nT/s if STEP = 0 or
!              peak primary B (in nT if STEP = 1)

 IMPLICIT NONE
 INTEGER ISW,STEP,NSX,JT
 REAL BFFAC,SWX(NSX),WAVEFORM(NSX),SWY(NSX,3),PKSX,DELT,COUPLING,PRM_TD(3)

 IF (SWX(2) - SWX(1) < 0.5E-7) SWX(2) = SWX(1) + 0.5E-7
 IF (SWX(NSX) - SWX(NSX-1) < 0.5E-7) SWX(NSX-1) = SWX(NSX) - 0.5E-7

!  Remove the receiver coupling if the receiver voltage or magnetic field is
!  to be used to get dI/dt.  Ensure that the input waveform is converted to
!  nT or nT/s

 SWY = 0.
 IF (ISW /=1) WAVEFORM = WAVEFORM / BFFAC
 SWY(1:NSX,3) = WAVEFORM(1:NSX)  !  Store original waveform for Geotem stripping.

! Remove receiver coupling factor if ISW isn't given in amps.
! Compensate 1.E9 for the fact that PRM_TD is in nT

 COUPLING = 1.
 IF (ISW == 30 .OR. ISW == 31 .OR. ISW == 130 .OR. ISW == 131) THEN
   COUPLING = PRM_TD(3) !  Waveform derived from vertical measurement
 ELSE IF (ISW == 10 .OR. ISW == 11) THEN
   COUPLING = PRM_TD(1) ! Waveform derived from in-line measurement
 END IF
 COUPLING = ABS (COUPLING)

! The current is obtained by dividing the waveform by the coupling.  At this
! stage, the coupling, PRM_TD, is expressed in nT for unit dipole moment.
! The waveform is in nT/s or nT so coupling meeds to be multiplied
! by BFFAC to also be in UNITS thus get the current in amps and SWY in amps / s.

 IF (ISW /= 1) WAVEFORM = WAVEFORM / COUPLING  ! TXMNT * dI/dt or I

!  Compute the source waveform for the program from the input data.
!  This is dI/dt * the Tx-Rx moment.

 IF (ISW == 30 .OR. ISW == 10 .OR. ISW == 130) THEN
   IF (STEP == 0) THEN
     SWY(1:NSX,1) = -WAVEFORM(1:NSX)  ! Reverse negatibe derivative at signal end
   ELSE
     SWY(1:NSX,1) = WAVEFORM(1:NSX)
   END IF
 ELSE

! Compute delta I in SWY(*,2) and dI/dt if it exists in SW(*,1).
! Compute the negative derivative so that dI/dt will be positive
! just before signal turn-off.  Store on right node (27.01.00)

   DO JT = 2, NSX
     SWY(JT,2) = WAVEFORM(JT-1) - WAVEFORM(JT)
     DELT = SWX(JT) - SWX(JT-1)
     IF (DELT > 1.0E-7) THEN
       SWY(JT,1) = SWY(JT,2) / DELT
     END IF
   END DO
 END IF

 PKSX = 0  ! Compute peak I or dI/dt for step normalisation.

 IF (STEP == 0) THEN
   PKSX = MAXVAL (ABS (SWY(1:NSX-1,1)) )   ! Compute peak dI/dt.
 ELSE IF (STEP == 1) THEN
   PKSX = MAXVAL (ABS (WAVEFORM) )
 END IF

 PRM_TD = PKSX * PRM_TD

END SUBROUTINE SET_SOURCE

 SUBROUTINE TDEM_3D (STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL, &
                     TOPN,TCLS,FREQ,NFRQ,FANGLE,NSTAT,BFD_SCAT,GSTRP,ASTRP,BTD_SCAT)

!***  Called by: MAIN, GET_FWD_MODL
!***      Calls: CUBSPL, COSTRN, FOLD_CONVOLVE

!  Computes BTD_SCAT, the time domain response for the 3D part of the model
!  (scattered field) as db/dt in (nT/s if STEP is false) or magnetic field B
!  (in nanoteslas if STEP is true) by convolving the step b response of the
!  earth with the negative time-derivative of the current waveform.  Questo
!  waveform contains the Txarea * NTRN.  For magnetic field, this averaged
!  across the receiver window.  For db/dt, this is differenced across the
!  receiver window.  The negative dI/dt is used so that current switch off
!  corresponds to positive response.
!
!  On entry, the scattered field frequency-domain step H data in array BFD_SCAT
!  is rotated to the aircraft system and the imaginary component is converted
!  to time-domain step b(t) data out to NPULS bipolar cycles.  For each
!  component in-line, transverse,and vertical, and for each transmitter-
!  receiver position, FOLD_AND_CONVOLVE is called to fold the positive &
!  negative parts of the bipolar current pulse into a half-cycle (length PULSE)
!  decay curve.  This result is convolved with the dI/dt waveform.
!
!
!                      INPUT
!                      -----
!
!     STEP = 1 iff step response is to be computed
!     IDER = 1 if source waveform was dB/dt; = 0 if amps or B
!      NSX - number of points used to discretise transmitter signal
!      SWX - abscissae (seconds) of current waveform
!      SWY - dI/dt at times SWX
!    NPULS - number of bipolar pulses of length PULSE
!    PULSE - length of half-cycle on pulse plus off-time
!    NTYRP - number of values in TRP for total signal length: 2 * NPULS *PULSE
!      TRP - array of time values for FD -> TD transformations
!   NTYPLS - number of TRP values in 1 PULSE
!    NCHNL - number of channels
!     TOPN - time at which receiver channel I opens.
!     TCLS - time at which receiver channel I closes.
!     FREQ - array of NFRQ frequencies.
!    NSTAT - number of transmitter positions.
!      NRX - number of actual receivers (not receiver positions)
!  BFD_SCAT(I,J,K) - Kth component of the complex frequency-domain impulse
!                    response magnetic field (H) at receiver L, transmitter J
!                    for frequency I - due to a unit dipole transmitter,
!                    oriented at angle TXCLN.  (nT)
!                    K = 1,2,3 => in-line, transverse & vertical  components respectively.
!
!                      OUTPUT
!                      ------
!
!  BTD_SCAT(JT,JS,1) - the in-line component of the layered earth response at time JT, station JS.
!  BTD_SCAT(JT,JS,2) - the horizontal transverse component
!  BTD_SCAT(JT,JS,3) - the vertical component

 IMPLICIT NONE
 REAL, PARAMETER :: TWOPI=6.283185
 INTEGER GSTRP,ASTRP,IDER,STEP,NSX,NPULS,NTYPLS,NTYRP,NCHNL,NFRQ,NSTAT, &
         JS,JF,JC,JT
 REAL, DIMENSION(:,:), ALLOCATABLE ::  YSCAT,YFRQ,BFD_QUAD
 REAL PULSE,FANGLE(NSTAT),CSF,SNF,FREQ(NFRQ),T0_MIN,WF(NFRQ),SWX(NSX),SWY(NSX,3),COSTRN,T,YCUM(NCHNL), &
      TOPN(NCHNL),TCLS(NCHNL),TRP(NTYRP),BTD_SCAT(NCHNL,NSTAT,3),OMEGA(NFRQ)
 COMPLEX BFD_SCAT(NFRQ,NSTAT,3)

 INTENT (IN) STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL, &
             TOPN,TCLS,FREQ,NFRQ,NSTAT,BFD_SCAT
 INTENT (INOUT) BTD_SCAT

 T0_MIN = 0.1 / MAXVAL (FREQ)
 BTD_SCAT = 0.

 ALLOCATE (YSCAT(4,NTYRP), YFRQ(4,NFRQ), BFD_QUAD(NFRQ,3) )
 YSCAT=0; YFRQ=0; BFD_QUAD=0;

!  For all frequencies and receiver positions, rotate the fields from
!  X, Y, Z orientation to in-line, transverse, and vertical components.
!  For dead North flight line, in-line is positive North and transverse
!  is positive East.  Spline the result as a function of log (w) (omega).
!  Convert to step response and apply the microvolt conversion factor.

!  step dB/dt (nT/s) = H * 1.0E9 * MU * dI/dt * TXMMNT / (I * 2*PI * FREQ)
!  Originally VFAC was = 1.0E9 * MU * TX MOMENT / (2*PI);
!  From 9/01, input is B in nT and 1/(2 Pi) is now absorbed in OMEGA
!  If STEP = 1, output is in nanoteslas consistent with HSBOSS.
!  In this case, conversion to pT or fT occurs in WRITE_TD.

 OMEGA(1:NFRQ) = TWOPI * FREQ(1:NFRQ)
 WF = LOG (OMEGA)
 OMEGA = -OMEGA     !  division by -iw for step response

 DO JS = 1,NSTAT         ! station loop
   DO JF = 1, NFRQ
     BFD_QUAD(JF,1:3) = AIMAG ( BFD_SCAT(JF,JS,1:3) )  / OMEGA(JF)
   END DO

   CSF = COS (FANGLE(JS))
   SNF = SIN (FANGLE(JS))
   DO JF = 1, NFRQ
     BFD_QUAD(JF,1) = CSF * BFD_QUAD(JF,1) + SNF * BFD_QUAD(JF,2)
     BFD_QUAD(JF,2) = CSF * BFD_QUAD(JF,2) - SNF * BFD_QUAD(JF,1)
   END DO

!  Above: Conversion from impulse to step current turn-off.
!  For each component at each receiver station, compute the SCATTERED response
!  by splining the imaginary part of the frequency-domain response, converting
!  it to time-domain step function response and folding the NPULS bipolar decay
!  curve into a combined pulse decay curve of length PULSE.  Convolve this with
!  the TX waveform to produce BTD_SCAT, the 'observable" stripped response for the
!  system.

   DO JC = 1,3             ! component loop
     YFRQ(1,1:NFRQ) = BFD_QUAD(1:NFRQ,JC)
     CALL CUBSPL (WF,YFRQ,NFRQ,0,0)

     YSCAT = 0.
     DO JT = 1, NTYRP   !  Convert to step-function time-domain.
       T = TRP(JT)
       IF (T < T0_MIN) CYCLE
       YSCAT(1,JT) = COSTRN (WF,YFRQ,NFRQ,NFRQ,T)
     END DO
     CALL FOLD_AND_CONVOLVE (STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,TRP,NTYPLS, &
                             NTYRP,NCHNL,TOPN,TCLS,YSCAT,GSTRP,ASTRP,YCUM)

     BTD_SCAT(1:NCHNL,JS,JC) = YCUM(1:NCHNL)
   END DO
 END DO
 DEALLOCATE (BFD_QUAD, YSCAT, YFRQ)

END SUBROUTINE TDEM_3D

 SUBROUTINE FOLD_AND_CONVOLVE (STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,TRP,NTYPLS, &
                               NTYRP,NCHNL,TOPN,TCLS,YPLS,GSTRP,ASTRP,YCUM)
!-------------------------------------------------------------------------------

!  Computes the "observed" response YCUM by convolving the splined earth
!  response function, YPLS, with the TX waveform.

!***  Called by: HSBOSS_TD, TDEM3D
!***      Calls: CUBDER, CUBVAL, CUBSPL, TXCNVD, TXCNVL, TQSTRIP

!     IDER - derivative indicator
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
!    GSTRP = 1 if Geotem / Questem stripping is to be applied

 IMPLICIT NONE
 INTEGER JT,NTYRP,NTYPLS,NPULS,IDER,STEP,NSX,NCHNL,JGL,JP,GSTRP,ASTRP,MXCNV,IPL
 REAL PULSE,TRP(NTYRP),SWX(NSX),SWY(NSX,3),TOPN(NCHNL),TCLS(NCHNL),T1,T2,WIDTH, &
      TF,TFH,HWIDTH,YC1,TC(3),GLX(3),GLW(3),YPLS(4,NTYRP),X,XP,YCUM(NCHNL),     &
      CUBVAL,CUBDER,TXCNVL,TXCNVD,WT,FOLD(NTYPLS),YCNV(4,NSX)
 DATA GLW(1:3) /.5555556, .8888889, .5555556/, GLX(1:3) /-.7745967, 0., .7745967/

 INTENT (IN)  IDER,NSX,SWX,SWY,NPULS,PULSE,TRP,NTYPLS,NTYRP,NCHNL,TOPN,TCLS,GSTRP
 INTENT (INOUT) YPLS
 INTENT (OUT) YCUM

!  Accumulate the results of NPULS bipolar cycles by splining the instantaneous
!  response and folding the positive and negative parts of each cycle back
!  into a single pulse.

 CALL CUBSPL (TRP,YPLS,NTYRP,0,0)
 FOLD = 0.
 IPL = 1
 IF (NPULS == 1) IPL = 0

 IF (STEP == 1) THEN
   DO JT = 1,NTYPLS
     X = TRP(JT)
     XP = X + PULSE
     FOLD(JT) = CUBVAL (TRP,YPLS,NTYRP,X) - IPL* CUBVAL (TRP,YPLS,NTYRP,XP)
     DO JP = 2, NPULS
       X = XP + PULSE
       XP = X + PULSE
       FOLD(JT) = CUBVAL (TRP,YPLS,NTYRP,X) - CUBVAL (TRP,YPLS,NTYRP,XP) &
                + FOLD(JT)
     END DO
   END DO
 ELSE
   DO JT = 1,NTYPLS
     X = TRP(JT)
     XP = X + PULSE
     FOLD(JT) = IPL* CUBDER (TRP,YPLS,NTYRP,XP) - CUBDER (TRP,YPLS,NTYRP,X)
     DO JP = 2, NPULS
       X = XP + PULSE
       XP = X + PULSE
       FOLD(JT) = CUBDER (TRP,YPLS,NTYRP,XP) - CUBDER (TRP,YPLS,NTYRP,X) &
                + FOLD(JT)
     END DO
   END DO
 END IF

 YPLS = 0.
 YPLS(1,1:NTYPLS) = FOLD(1:NTYPLS)
 CALL CUBSPL (TRP,YPLS,NTYPLS,0,0)
 YCUM = 0.

!  Begin convolution.  If Geotem / Questem primary field stripping is required
!  the convolution must be done for all points in the waveform.
!  Otherwise, convolve only for those points needed in the windows.

!  The layered earth field is in IMPULSE form if dB/dt is desired
!  or in STEP form if B is to be computed.

 MXCNV = NTYPLS + NSX
 TF = SWX(NSX)
 TFH = 0.5 * TF

 IF (GSTRP == 1) CALL TQSTRIP (IDER,NTYPLS,TRP,YPLS,NSX,SWX,SWY,YCNV)
 DO JT = 1, NCHNL
   T1 = TOPN(JT)
   T2 = TCLS(JT)
   WIDTH = T2 - T1
   HWIDTH = WIDTH /2.

! Step response for step input or impulse response response for impulse input
! Average the response over receiver windows using 3 point Gaussian integration.

   TC(2) = (TCLS(JT) + TOPN(JT)) /2.
   TC(1) = TC(2) + HWIDTH * GLX(1)
   TC(3) = TC(2) + HWIDTH * GLX(3)

   DO JGL = 1, 3
     T1 = TC(JGL)
     WT = GLW(JGL) / 2.
     YC1 = 0.

     IF (GSTRP == 1 .AND. T1 < TF) THEN
       YC1 = CUBVAL (SWX,YCNV,NSX,T1)
     ELSE IF (IDER == 0) THEN   ! Waveform input as I or B field (derived dI/dt)
       YC1 = TXCNVL (T1,NTYPLS,TRP,YPLS,NSX,SWX,SWY)
       IF (ASTRP == 1 .AND. T1 < TF) THEN   ! Asymmetric stripping
         T2 = T1 - TFH
         YC1 = YC1 + TXCNVL (T2,NTYPLS,TRP,YPLS,NSX,SWX,SWY)
       END IF

     ELSE IF (IDER == 1) THEN        ! Waveform input as voltage (known dI/dt)
       YC1 = TXCNVD (MXCNV,T1,NTYPLS,TRP,YPLS,NSX,SWX,SWY)
     ELSE IF (IDER == 4) THEN                      ! pure on-time step
       YC1 = SWY(1,1) * CUBVAL (TRP,YPLS,NTYPLS,T1)
     END IF
     YCUM(JT) = YCUM(JT) + (WT * YC1)
   END DO
 END DO

END SUBROUTINE FOLD_AND_CONVOLVE

 SUBROUTINE TQSTRIP (IDER,NTYPLS,TRP,YPLS,NSX,SWX,SWY,YCNV)
!----------------------------------------------------------
!
!***  Called by: FOLD_AND_CONVOLVE
!***      Calls: CUBSPL, TXCNVD, TXCNVL

!  A stripped down version of TXCNVD is used to convolve the earth response
!  with the receiver waveform for for every point on that waveform.
!  The Geotem / Questem correlation is used to strip remnant primary field.
!  The result is sent back for binning into NCHNL receiver windows.
!
!   TRP, YPLS - abscissa & ordinate values of earth response function to
!               be convolved.
!        IDER - derivative indicator
!      NTYPLS - number of values in TRP and YPLS
!         SWX - abscissa of time values of source waveform in sec.
!         SWY - dI/dt values derived from receiver dB/dt. + raw waveform.
!         NSX - number of points in SWX & in each waveform stored in SWY
!        YCNV - the stripped convolved waveform
!
!  Defining  T1 = MIN {T, signal length,}, the convolution is formally
!  computed as
!
!   TXCNVD (T) = INT (T0 -> T) { YPLS (tau) * SWY (T-tau)  d tau }

!  where T0 = MAX { TRP(1), T - SWX (NSX)}
!

 IMPLICIT NONE
 REAL, PARAMETER :: T0_MIN=1.E-7
 INTEGER IDER,NTYPLS,NSX,JT,MXCNV
 REAL T,TRP(NTYPLS),YPLS(4,NTYPLS),SWX(NSX),SWY(NSX,3),YCNV(4,NSX),TXCNVL, &
      TXCNVD,A1,B1,ALPHA

 INTENT (IN) IDER,NTYPLS,TRP,YPLS,NSX,SWX,SWY
 INTENT (OUT) YCNV

 A1 = 0.;  B1 = 0.; YCNV = 0.
 MXCNV = NTYPLS + NSX

 DO JT = 2,NSX              !  Convolve NSW points using the derived waveform
   T = SWX(JT)
   IF (T < T0_MIN) CYCLE
   IF (IDER == 0) THEN     ! Waveform input as I or B field (derived dI/dt)
     YCNV(1,JT) = TXCNVL (T,NTYPLS,TRP,YPLS,NSX,SWX,SWY)
   ELSE                    ! Waveform input as voltage (known dI/dt)
     YCNV(1,JT) = TXCNVD (MXCNV,T,NTYPLS,TRP,YPLS,NSX,SWX,SWY)
   END IF

   A1 = A1 + YCNV(1,JT) * SWY(JT,3)   !  Compute correlation
   B1 = B1 + SWY(JT,3) * SWY(JT,3)
 END DO

 ALPHA = A1 / B1
 DO JT = 1,NSX
   YCNV(1,JT) = YCNV(1,JT) - ALPHA * SWY(JT,3)
 END DO

 CALL CUBSPL (SWX,YCNV,NSX,0,0)

END SUBROUTINE TQSTRIP

 REAL FUNCTION TXCNVD (MXCNV,T,NTYPLS,TRP,YPLS,NSX,SWX,SWY)
!----------------------------------------------------------
!
!***  Called by: FOLD_AND_CONVOLVE, TQSTRIP
!***      Calls: CUBINT, CUBSPL, CUBVAL, LINVAL, TXCMRG

!  Convolves impulse B (step dB/dt) earth response function (ERF) with the
!  specified derivative of the source waveform at NSX points to produce
!  the system dB/dt response of the earth.
!
!       MXCNV = NTYPLS + NSX
!           T - convolution time in sec measured from the beginning
!               of the source waveform.
!   TRP, YPLS - abscissa & ordinate values of earth response function to
!               be convolved.
!      NTYPLS - number of values in TRP and YPLS
!         SWX - abscissa of time values of source waveform in sec.
!         SWY - dI/dt values derived from receiver dB/dt.
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
 INTEGER MXCNV,NTYPLS,NSX,N1,J1,N2,J2,NCNV
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
   Y1(N1) = SWY(J1,1) * CUBVAL (TRP,YPLS,NTYPLS,TC)
 END DO

 T0 = T - SWX(NSX)
 T0 = MAX (T0, TRP(1))/ 1.0001

 N2 = 0
 DO J2 = 1,NTYPLS
   IF ((TRP(J2) > T0) .AND. (TRP(J2) < T)) THEN
     N2 = N2 + 1
     X2(N2) = TRP(J2)
     TC = T - TRP(J2)
     Y2(N2) = YPLS(1,J2) * LINVAL(NSX,SWX,SWY,TC,1)
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

 REAL FUNCTION TXCNVL (T,NTYPLS,TRP,YPLS,NSX,SWX,SWY)
!----------------------------------------------------

!***  Called by: FOLD_AND_CONVOLVE, TQSTRIP
!***      Calls: CUBINT, CUBVAL

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

 SUBROUTINE TXCMRG (MXCNV,X1,Y1,N1,X2,Y2,N2,XCNV,YCNV,NCNV)
!----------------------------------------------------------

!***  Called by: TXCNVD

!  Merges two previously sorted list pairs X1, Y1 of length N1 and X2, Y2 of
!  length N2 into list pair XCNV, YCNV of length NCNV into ascending values of
!  XCNV.

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

  SUBROUTINE WRITE_LOG_FILE (NLG,MSG,MXERR,ERR_LVL)
! -------------------------------------------------

!***  Called by: READ_SYSTEM_AND_SURVEY, READ_MODEL, READ_INVRT_CNTRL_AND_DATA

! This subroutine prints out warning and fatal error messages on the LOG file.
!
! NLG = output unit index
! MSG refers to error message index
! ERR_LVL = 1 for warnings;  = 2 for fatal errors
! MXERR = MAX (ERR_LVL)

 INTEGER ERR_LVL, MSG, NLG, MXERR

 IF (MXERR == 0) OPEN (NLG,FILE = 'MarcoAir.log',STATUS = 'REPLACE')

 MXERR = MAX (ERR_LVL,MXERR)
 IF (ERR_LVL == 1) WRITE(NLG,501)
 IF (ERR_LVL == 2) WRITE(NLG,502)


 IF (MSG ==   1) WRITE(NLG,1)
 IF (MSG ==   2) WRITE(NLG,2)
 IF (MSG ==   3) WRITE(NLG,3)
 IF (MSG ==   4) WRITE(NLG,4)
 IF (MSG ==   5) WRITE(NLG,5)
 IF (MSG ==   6) WRITE(NLG,6)
 IF (MSG ==   7) WRITE(NLG,7)
 IF (MSG ==   8) WRITE(NLG,8)
 IF (MSG ==   9) WRITE(NLG,9)
 IF (MSG ==  10) WRITE(NLG,10)
 IF (MSG ==  11) WRITE(NLG,11)
 IF (MSG ==  12) WRITE(NLG,12)
 IF (MSG ==  13) WRITE(NLG,13)
 IF (MSG ==  14) WRITE(NLG,14)
 IF (MSG ==  15) WRITE(NLG,15)
 IF (MSG ==  16) WRITE(NLG,16)
 IF (MSG ==  17) WRITE(NLG,17)
 IF (MSG ==  18) WRITE(NLG,18)
 IF (MSG ==  19) WRITE(NLG,19)

 IF (MSG ==  50) WRITE(NLG,50)
 IF (MSG ==  51) WRITE(NLG,51)
 IF (MSG ==  53) WRITE(NLG,53)
 IF (MSG ==  54) WRITE(NLG,54)
 IF (MSG ==  55) WRITE(NLG,55)
 IF (MSG ==  56) WRITE(NLG,56)
 IF (MSG ==  57) WRITE(NLG,57)
 IF (MSG ==  58) WRITE(NLG,58)
 IF (MSG ==  59) WRITE(NLG,59)

  1 FORMAT(/T3,'The value for TDFD is outside the permitted range.' &
           /T3,'The allowed values are: 0 & 1 for time-domain or 2 for frequency domain.'/)
  2 FORMAT(/T3,'The value for DO3D is outside the permitted range.' &
           /T3,'It has been reset to 1 for forward modelling'/)
  3 FORMAT(/T3,'The restart option is not available for frequency domain.' &
           /T3,'DO3D has been reset to 1')
  4 FORMAT(/T3,'The value for ISW is outside the permitted range.')
  5 FORMAT(/T3,'The value for STEP is outside the permitted range.' &
           /T3,'The allowed values are: 0 or 1.'/)
  6 FORMAT(/T3,'The value for KRXW is outside the permitted range.' &
           /T3,'The allowed values are: 1 or 2.'/)
  7 FORMAT(/T3,'This value for TOPN is outside the permitted range.' &
           /T3,'It must be > 0.'/)
  8 FORMAT(/T3,'For inversion CMP must be 11, 13, 2, 3 or 4.')
  9 FORMAT(/T3,'CMP must be 11, 13, 2 or 3.  It has been reset to 3')
 10 FORMAT(/T3,'KPPM is outside allowed range.  It must be 0, 1, 3, 123, or 4.')
 11 FORMAT(/T3,'Input calibration and output must have the same units' &
           /T3,'Output will be in dB/dt units')
 12 FORMAT(/T3,'Input calibration and output must have the same units' &
           /T3,'Output will be in B units')
 13 FORMAT(//T3,'CMP must = 1, 2, or 3 for frequency domain modelling' &
            /T3,'CMP has been set to 1')
 14 FORMAT(//T3,'Frequency-domain inversion is based on the maximally coupled component only.' &
            /T3,'CMP must = 1 or -1')
 15 FORMAT(/T3,'MarcoAir requires a minimum of 2 stations in a survey')
 16 FORMAT(/T3,'SURVEY must be either 1, 2, 3, -1, -2, or -3 for time-domain.' &
           /T3,'SURVEY = 3 or -3 cannot be used for frequency-domain because'  &
           /T3,'Tx-Rx offset must be constant as a function of position')
 17 FORMAT(/T3,'IUNITS must be 1, 2 or 3')
 18 FORMAT(/T3,'CMP has been set to 1 for the vertical coplanar broadside option.')
 19 FORMAT(/T3,'IUNITS must be 1, 2 or 3.  It has been reset to the default')

!  Model Messages

 50 FORMAT(/T3,'MarcoAir inversion is intended for models of 1 to 9 plates.'         &
           /T3,'You have specified either too many or a negative number of plates.' &
           /T3,'MarcoAir does not wish to perform such tasks.')
 51 FORMAT(/T3,'MarcoAir inversion is intended for models with 1 or more plates.' &
           /T3,'You have set NPLATE = 0.   For layered earth inversion, use Airbeo.')
 53 FORMAT(/T3,'A lithology must have a positive first component (resistivity) if it is to be' &
           /T3,'applied to a layer.  It must have a positive second component (conductance)'   &
           /T3,'if it is to be aplied to a plate.  You have incorrectly specified both    '    &
           /T3,'conductance and resistivity as negative for one of the lithologies')
 54 FORMAT(/T3,'LITH must be an integer between 1 & NLITH')
 55 FORMAT(/T3,'Layer resistivities must be positive.')
 56 FORMAT(/T3,'This version of MarcoAir does not allow the plate to extend into' &
           /T3,'the air or overburden')
 57 FORMAT(/T3,'Plate conductance must be positive.')
 58 FORMAT(/T3,'Negative DIP or DIP > 180 degrees is not allowed.')
 59 FORMAT(/T3,'ROT must be in the interval -90 degrees to + 90 degrees.')

 501 FORMAT(/T2,'WARNING'/T2,'-------'/)
 502 FORMAT(/T2,'FATAL ERROR'/T2,'----- -----'/)

END SUBROUTINE  WRITE_LOG_FILE

 SUBROUTINE WRITE_FD (NW,np,TITLE,NSTAT,LINE,TXCLN,TXA90,SXD,SYD,SZ,RXD,RYD,RZ,CONFIG,NFRQ, &
                      FREQ,PRFL,QUNIT,PPFAC,BFFAC,PRTSEC,PRM_FD,CMP,BFD_SCAT,BFD)
!--------------------------------------------------------------------------------------

!***  Called by: MAIN
!***      Calls: WRSLV_FD

!  Prints the results of TEM computations.
!
!                NW - output unit number
!               np - unit number for mf1 file
!             NSTAT - Number of stations
!              LINE - Line number
!         TXCLN(JF) - transmitter orientation at frequency JF (in radians)
!             TXA90 - true for vertical co-planar briadside array
!            PRTSEC - true iff scattered fields are to be printed
!              PRFL = 1 => profile output
!                   = 2 => spectral output
!             QUNIT = text units for B, dB/dt or normalisation
!             BFFAC = numeric factor for nT, nT/s, pT, fT, pT/s or fT/s output
!             PPFAC = numeric factor for output in pct, ppt, ppm or ppb
!              NFRQ - number of frequencies.
!              FREQ - array of frequencies.
!               CMP = 1: write single component response only in direction of transmitter
!                        orientation in normalised units
!                   = 2: write un-normalised vertical and horizontal in-line response. (pT)
!                   = 3: write un-normalised 3 component response. (pT)
!
!             JF, below refers to receiver location varying with frequency
!             since the offset can vary with frequency
!
!         RXD(I,JF) - North coordinates for receiver JF at station I
!         RYD(I,JF) - East coordinates for receiver JF at station I
!          RZ(I,JF) - altitude of receiver JF at station I
!
!        PRM_FD(JF) - DC (nT) response along transmitting dipole direction
!                     for Tx-Rx configuration for frequency JF.
!
!       BFD(JF,JS,JC) - layered + scattered earth magnetic field for frequency JF,
!                       source position JS, component JC (nT) in aircraft components
!  BFD_SCAT(JF,JS,JC) - scattered field in aircraft components
!

 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80)
 INTEGER NW,np,NSTAT,LINE(NSTAT),NFRQ,CMP,PRFL,TXD(NFRQ),JF,JS
 REAL FREQ(NFRQ),RZ(NSTAT,NFRQ),SZ(NSTAT),MPZ1(NSTAT),PRM_FD(NFRQ),PRM4,NORM(NFRQ), &
      YTR(NFRQ,NSTAT),TXCLN(NFRQ),PPFAC,BFFAC
 REAL(KIND=QL) RXD(NSTAT,NFRQ),RYD(NSTAT,NFRQ),MXD(NSTAT),MYD(NSTAT),SXD(NSTAT),SYD(NSTAT)
 COMPLEX BFD_SCAT(NFRQ,NSTAT,3),BFD(NFRQ,NSTAT,3),BFD1(NFRQ,NSTAT,4),BFD1SC(NFRQ,NSTAT,4)
 LOGICAL TXA90,PRTSEC,WL
 CHARACTER (LEN=120) TITLE
 CHARACTER (LEN=11) QI,QL0,QL1
 CHARACTER (LEN=9) QR
 CHARACTER (LEN=10) CMP2
 CHARACTER (LEN=3) CONFIG(NFRQ)
 CHARACTER (LEN=4) QUNIT
 CHARACTER (LEN=8) CMP3
 CHARACTER (LEN=7) CMP1
 DATA CMP1,CMP2,CMP3 /'IN-LINE','TRANSVERSE','VERTICAL'/
 DATA QR,QI /'IN-PHASE ','QUADRATURE '/

! This routine assumes a single Tx-Rx separation for each frequency.
! Put all the results into  BFD1 & BFD1_SCAT.

!   BFD1(JF,JS,1:3) will contain the in-line(1), transverse(2) and
!                   vertical fields(3) respectively (in pT)
!                   for frequency JF and station JS
!   BFD1(JF,JS,4) will contain the maximally coupled field (ppm)
!                 normalised to the parallel primary component.
!   BFD1SC(JF,JS,1:4) contains the scattered fields

! Print results at position of first receiver if more than one
! separation is used.

 MXD(1:NSTAT) = (RXD(1:NSTAT,1) + SXD(1:NSTAT) ) /2.
 MYD(1:NSTAT) = (RYD(1:NSTAT,1) + SYD(1:NSTAT) ) /2.
 MPZ1(1:NSTAT) =  (RZ(1:NSTAT,1)  + SZ(1:NSTAT) )  / 2.
 BFD1 = (0.,0.);  BFD1SC = (0.,0.)
 IF (CMP == 1) WRITE(NW,10)
 DO JF = 1, NFRQ
   PRM4 = BFFAC * ABS (PRM_FD(JF))                   ! coupled primary in nT, pT or fT
   NORM(JF) = PPFAC / PRM4                         ! for pct, ppt, ppm or ppb output

   IF (CMP == 1) WRITE(NW,'(I6,T17,G12.4,T34,g12.4)')  JF,PRM4,NORM(JF)

   BFD1(JF,1:NSTAT,1:3)   = BFFAC * BFD(JF,1:NSTAT,1:3)       ! total field in nT, pT or fT
   BFD1SC(JF,1:NSTAT,1:3) = BFFAC * BFD_SCAT(JF,1:NSTAT,1:3)  ! scattered field in nT, pT or fT
 END DO

! Normalise them as indicated in input data file.
! For CMP = 1, compute component along Tx direction

 TXD(1:NFRQ) = NINT ( 180. * TXCLN(1:NFRQ) / 3.1416)
 WRITE(NW,1)
 IF (CMP > 1) WRITE(NW,8)
 IF (CMP > 2) WRITE(NW,9)

 DO JF = 1, NFRQ

!  maximally coupled response
   IF (TXA90) THEN
     BFD1(JF,1:NSTAT,4) =  BFD1(JF,1:NSTAT,2)
     BFD1SC(JF,1:NSTAT,4) =  BFD1SC(JF,1:NSTAT,2)
   ELSE
     BFD1(JF,1:NSTAT,4) =  BFD1(JF,1:NSTAT,1) * SIN (TXCLN(JF)) &
                        +  BFD1(JF,1:NSTAT,3) * COS (TXCLN(JF))
     BFD1SC(JF,1:NSTAT,4) =  BFD1SC(JF,1:NSTAT,1) * SIN (TXCLN(JF)) &
                          +  BFD1SC(JF,1:NSTAT,3) * COS (TXCLN(JF))
   END IF

   BFD1(JF,1:NSTAT,4) = NORM(JF) * BFD1(JF,1:NSTAT,4)
   BFD1SC(JF,1:NSTAT,4) = NORM(JF) * BFD1SC(JF,1:NSTAT,4)
 END DO

 IF (CMP == 1) THEN
   WRITE(NW,15) TRIM (TITLE)
   WRITE(NW,3)

   WRITE(NW,4) QR,TRIM (QUNIT)
   YTR(1:NFRQ,1:NSTAT) = REAL (BFD1(1:NFRQ,1:NSTAT,4))
   CALL WRSLV_FD (NW,PRFL,MXD,MYD,MPZ1,NSTAT,NFRQ,FREQ,TXD,CMP,CONFIG,YTR)

   WRITE(NW,4) QI,TRIM (QUNIT)
   YTR(1:NFRQ,1:NSTAT) = AIMAG (BFD1(1:NFRQ,1:NSTAT,4))
   CALL WRSLV_FD (NW,PRFL,MXD,MYD,MPZ1,NSTAT,NFRQ,FREQ,TXD,CMP,CONFIG,YTR)

   IF (PRTSEC) THEN   ! Scattered field for maximally coupled component
     WRITE(NW,6) QR,TRIM (QUNIT)
     YTR(1:NFRQ,1:NSTAT) = REAL (BFD1SC(1:NFRQ,1:NSTAT,4))
     CALL WRSLV_FD (NW,PRFL,MXD,MYD,MPZ1,NSTAT,NFRQ,FREQ,TXD,CMP,CONFIG,YTR)

     WRITE(NW,6) QI,TRIM (QUNIT)
     YTR(1:NFRQ,1:NSTAT) = AIMAG (BFD1SC(1:NFRQ,1:NSTAT,4))
     CALL WRSLV_FD (NW,PRFL,MXD,MYD,MPZ1,NSTAT,NFRQ,FREQ,TXD,CMP,CONFIG,YTR)
   END IF
 END IF

 IF (CMP > 1) THEN
   WRITE(NW,'(/3X,A)') TRIM (TITLE)     !  Vertical component
   WRITE(NW,14) QR,CMP3,QUNIT
   YTR(1:NFRQ,1:NSTAT) = REAL(BFD1(1:NFRQ,1:NSTAT,3))
   CALL WRSLV_FD (NW,PRFL,MXD,MYD,MPZ1,NSTAT,NFRQ,FREQ,TXD,CMP,CONFIG,YTR)

   WRITE(NW,14) QI,CMP3,QUNIT
   YTR(1:NFRQ,1:NSTAT) = AIMAG(BFD1(1:NFRQ,1:NSTAT,3))
   CALL WRSLV_FD (NW,PRFL,MXD,MYD,MPZ1,NSTAT,NFRQ,FREQ,TXD,CMP,CONFIG,YTR)

   IF (PRTSEC) THEN      ! Scattered field for vertical component
     WRITE(NW,16) QR,CMP3,QUNIT
     YTR(1:NFRQ,1:NSTAT) = REAL (BFD1SC(1:NFRQ,1:NSTAT,3))
     CALL WRSLV_FD (NW,PRFL,MXD,MYD,MPZ1,NSTAT,NFRQ,FREQ,TXD,CMP,CONFIG,YTR)

     WRITE(NW,16) QI,CMP3,QUNIT
     YTR(1:NFRQ,1:NSTAT) = AIMAG (BFD1SC(1:NFRQ,1:NSTAT,3))
     CALL WRSLV_FD (NW,PRFL,MXD,MYD,MPZ1,NSTAT,NFRQ,FREQ,TXD,CMP,CONFIG,YTR)
   END IF

   WRITE(NW,'(//3X,A)') TRIM (TITLE)     !  In-line component
   WRITE(NW,14) QR,CMP1,QUNIT
   YTR(1:NFRQ,1:NSTAT) = REAL (BFD1(1:NFRQ,1:NSTAT,1))
   CALL WRSLV_FD (NW,PRFL,MXD,MYD,MPZ1,NSTAT,NFRQ,FREQ,TXD,CMP,CONFIG,YTR)

   WRITE(NW,14) QI,CMP1,QUNIT
   YTR(1:NFRQ,1:NSTAT) = AIMAG (BFD1(1:NFRQ,1:NSTAT,1))
   CALL WRSLV_FD (NW,PRFL,MXD,MYD,MPZ1,NSTAT,NFRQ,FREQ,TXD,CMP,CONFIG,YTR)

   IF (PRTSEC) THEN      ! Scattered field for in-line component
     WRITE(NW,16) QR,CMP1,QUNIT
     YTR(1:NFRQ,1:NSTAT) = REAL (BFD1SC(1:NFRQ,1:NSTAT,1))
     CALL WRSLV_FD (NW,PRFL,MXD,MYD,MPZ1,NSTAT,NFRQ,FREQ,TXD,CMP,CONFIG,YTR)

     WRITE(NW,16) QI,CMP1,QUNIT
     YTR(1:NFRQ,1:NSTAT) = AIMAG (BFD1SC(1:NFRQ,1:NSTAT,1))
     CALL WRSLV_FD (NW,PRFL,MXD,MYD,MPZ1,NSTAT,NFRQ,FREQ,TXD,CMP,CONFIG,YTR)
   END IF
 END IF

 IF (CMP == 3) THEN
   WRITE(NW,'(//3X,A)') TRIM (TITLE)     ! Transverse component
   YTR(1:NFRQ,1:NSTAT) = REAL(BFD1(1:NFRQ,1:NSTAT,2))
   WRITE(NW,14) QR,CMP2,QUNIT
   CALL WRSLV_FD (NW,PRFL,MXD,MYD,MPZ1,NSTAT,NFRQ,FREQ,TXD,CMP,CONFIG,YTR)

   YTR(1:NFRQ,1:NSTAT) = AIMAG(BFD1(1:NFRQ,1:NSTAT,2))
   WRITE(NW,14) QI,CMP2,QUNIT
   CALL WRSLV_FD (NW,PRFL,MXD,MYD,MPZ1,NSTAT,NFRQ,FREQ,TXD,CMP,CONFIG,YTR)

   IF (PRTSEC) THEN      ! Scattered field for transverse component
     WRITE(NW,16) QR,CMP2,QUNIT
     YTR(1:NFRQ,1:NSTAT) = REAL (BFD1SC(1:NFRQ,1:NSTAT,2))
     CALL WRSLV_FD (NW,PRFL,MXD,MYD,MPZ1,NSTAT,NFRQ,FREQ,TXD,CMP,CONFIG,YTR)

     WRITE(NW,16) QI,CMP2,QUNIT
     YTR(1:NFRQ,1:NSTAT) = AIMAG (BFD1SC(1:NFRQ,1:NSTAT,2))
     CALL WRSLV_FD (NW,PRFL,MXD,MYD,MPZ1,NSTAT,NFRQ,FREQ,TXD,CMP,CONFIG,YTR)
   END IF
 END IF

!  Finish writing MarcoAir.mf1

 IF (CMP > 1) RETURN
 DO JS = 1,NSTAT
   WL = .TRUE.
   IF (JS > 1 .AND. LINE(JS) == LINE(JS-1)) WL = .FALSE.

   IF (WL) THEN
     WRITE(QL0,'(I10)') LINE(JS)
     READ(QL0,'(A)') QL1
     WRITE(np,'(2A)') 'Line ',TRIM (ADJUSTL (QL1))
   END IF

   WRITE(np,20) JS,SYD(JS),SXD(JS),SZ(JS),RYD(JS,1),RXD(JS,1),RZ(JS,1), &
                 REAL (BFD1(1:NFRQ,JS,4)), AIMAG (BFD1(1:NFRQ,JS,4))
 END DO

 1 FORMAT(/T3,'FREQUENCY-DOMAIN MarcoAir OUTPUT' /T3,33('-'))
 3 FORMAT(//T3,'SINGLE COMPONENT RESPONSE ALONG TRANSMITTER DIPOLE DIRECTION')
 4 FORMAT(//T10,A,'COMPONENT - ',A)
 6 FORMAT(//T10,'SCATTERED ',A,' COMPONENT - ',A)
 8 FORMAT(/T3,'The IN-LINE component is defined as the horizontal component along' &
          /T3,'the flight path.  It is positive in the forward flight direction.')
 9 FORMAT(/T3,'The TRANSVERSE component is the horizontal component',&
          /T3,'perpendicular to the flight path.'/)
 10 FORMAT(/T4,'Frequency    Coupled Primary   Normalisation' &
           /T4,'Index        Field (pT)        Factor'        &
           /T4,'---------    ---------------   ------------')
 14 FORMAT(/T10,2A,' COMPONENT - ',A)
 15 FORMAT(//T2,'TITLE:  ',A/T2,'-----')
 16 FORMAT(/T10,'SCATTERED ',2A,' COMPONENT - ',A)
 20 FORMAT(I5,2F12.1,F8.1,2F12.1,F8.1,50G13.4)

END SUBROUTINE WRITE_FD

 SUBROUTINE WRSLV_FD (NW,PRFL,MXD,MYD,MPZ1,NSTAT,NFRQ,FREQ,TXD,CMP,CONFIG,YTR)
!------------------------------------------------------------------------------

!***  Called by: WRITE_FD
!***      Calls: (conditional) WRSLVS_FD

!  Writes frequency-domain output in profile form
!  if PRFL = 1  or in spectral output if PRFL = 2

!         TXD(JF) - transmitter orientation at frequency JF (in degrees)
!      YTR(JF,JS) - field at station JS for frequency JF.

!    All other variables defined in SUBROUTINE WRITE_FD

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80)
 INTEGER NW,NFRQ,PRFL,CMP,NSTAT,TXD(NFRQ),KRX,KRY,KRZ,JS
 REAL MPZ1(NSTAT),FREQ(NFRQ),YTR(NFRQ,NSTAT)
 REAL(KIND=QL) MXD(NSTAT),MYD(NSTAT)
 CHARACTER(LEN=3) CONFIG(NFRQ)

 IF (PRFL == 2) THEN
   CALL WRSLVS_FD (NW,MXD,MYD,MPZ1,NSTAT,NFRQ,FREQ,TXD,CMP,YTR)
 ELSE
   IF (CMP == 1) THEN
     IF (MAXVAL (FREQ) < 100000.) THEN
       WRITE(NW,1) (FREQ(1:NFRQ))
     ELSE
       WRITE(NW,2) (FREQ(1:NFRQ))
     END IF
   ELSE
     WRITE(NW,5) (FREQ(1:NFRQ))
   END IF

   IF (CMP == 1) WRITE(NW,4) CONFIG(1:NFRQ)
   WRITE(NW,'(3X)')
   DO JS = 1, NSTAT
     KRX = NINT (MXD(JS))
     KRY = NINT (MYD(JS))
     KRZ = NINT (MPZ1(JS))
     IF (CMP == 1) THEN
       WRITE(NW,3) JS,KRY,KRX,KRZ,YTR(1:NFRQ,JS)
     ELSE
       WRITE(NW,6) JS,KRY,KRX,KRZ,YTR(1:NFRQ,JS)
     END IF
   END DO

 END IF

 1 FORMAT(/T10,'EAST     NORTH    ALT',F10.0,40F12.0)
 2 FORMAT(/T10,'EAST     NORTH    ALT',40G11.3)
 3 FORMAT(I3,2I10,I7,40F12.2)
 4 FORMAT(T37,30(A,9X))
 5 FORMAT(/T10,'EAST     NORTH    ALT',30G14.4)
 6 FORMAT(I3,2I10,I7,40G14.4)

END SUBROUTINE WRSLV_FD

 SUBROUTINE WRSLVS_FD (NW,MXD,MYD,MPZ1,NSTAT,NFRQ,FREQ,TXD,CMP,YTR)
!-------------------------------------------------------------------

!***  Called by: WRSLV_FD

!  Writes frequency-domain output in spectral form

!         TXD(JF) - transmitter orientation at frequency JF (in degrees)
!      YTR(JF,JS) - field at station JS for frequency JF.

!    All other variables defined in SUBROUTINE WRITE_FD

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80), NCOL=40
 INTEGER NW,NFRQ,CMP,NSTAT,TXD(NFRQ),NBLKS,J1,J2,JB,JF
 REAL MPZ1(NSTAT),FREQ(NFRQ),YTR(NFRQ,NSTAT)
 REAL(KIND=QL) MXD(NSTAT),MYD(NSTAT)

 NBLKS = NSTAT / NCOL
 IF (MOD (NSTAT,NCOL) > 0) NBLKS = NBLKS + 1

 DO J1 = 1,NBLKS
   JF = J1 * NCOL
   JB = JF - NCOL + 1
   JF = MIN (JF,NSTAT)

   WRITE(NW,'(/T14,A,F9.0,39F13.0)') 'Receiver  Z',MPZ1(JB:JF)
   WRITE(NW,'(T13,A,F9.0,39F13.0)') 'Positions  E',MYD(JB:JF)
   WRITE(NW,'(T24,A,F9.0,39F13.0)') 'N',MXD(JB:JF)
   WRITE(NW,'(T7,A)') 'Freq      TXCLN'
   WRITE(NW,'(3X)')

   DO J2 = 1, NFRQ
     IF (CMP == 1) THEN
       WRITE(NW,'(I3,G13.5,I4,3X,40F13.2)') J2,FREQ(J2), TXD(J2), YTR(J2,JB:JF)  ! PPM
     ELSE
       WRITE(NW,'(I3,G13.5,I4,3X,40G13.4)') J2,FREQ(J2), TXD(J2), YTR(J2,JB:JF)  ! pT
     END IF
   END DO
   WRITE(NW,1)
 END DO

 1 FORMAT (85('-')/)

END SUBROUTINE WRSLVS_FD

 SUBROUTINE WRITE_TD (NW,np,TITLE,NSTAT,LINE,SXD,SYD,SZ,TXDEG,RXD,RYD,RZ,XRX,YRX,ZRX,NCHNL, &
                      TMS,PRFL,QUNIT,BUNIT,PPFAC,BFFAC,PRTSEC,PRM_TD,CMP,KPPM,BTD_SCAT,BTD)
!----------------------------------------------------------------------------------------------

!***  Called by: MAIN
!***      Calls: WRSLV
!
!  Prints the results of TEM computations.
!
!                NW - output unit number
!          LINE_TAG - Line ID up to 20 alphanumeric characters
!             NSTAT - Number of stations
!            SZ(JS) - transmitter altitude at station JS
!           SXD(JS) - North coordinate of transmitter at station JS
!           SYD(JS) - East)coordinate of transmitter at station JS
!          RXD(I,1) - North coordinates for receiver at station I
!          RYD(I,1) - East coordinates for receiver at station I
!           RZ(I,1) - altitude of receiver J at station I
!          XRX (JS) - distance receiver is behind the transmitter at station JS
!          ZRX (JS) - distance receiver is below the transmitter at station JS
!          YRX (JS) - left offset of receiver at station JS
!             NCHNL - number of channels.
!               TMS - array of times at channel midpoints
!              PRFL = 1 for orofile output;  = 2 for decay output.
!             QUNIT = text units for B, dB/dt or normalisation
!             BFFAC = numeric factor for nT, nT/s, pT, fT, pT/s or fT/s output
!             PPFAC = numeric factor for output in pct, ppt, ppm or ppb
!            PRTSEC - true iff scattered fields are to be printed
!       PRM_TD(1:3) - (1) in-line; (2) transverse; (3) vertical primary field
!                     nT/s for impulse, nT for step.
!            CMP = 11 => print horizontal in-line component only
!                = 13 => print vertical component only
!                =  2 => print vertical & horizontal in-line components
!                =  3 => print all three components
!
!           KPPM = 0   => no PPM normalisation
!           KPPM = 1   => all components are normalised to in-line primary field
!           KPPM = 3   => all components are normalised to vertical primary field
!           KPPM = 4   => all components are normalised to total primary field
!           KPPM = 123 => vertical component normalised to the vertical primary &
!                         in-line component to the in-line primary
!
!       BTD(JT,JS,JC) - total field for channel JT, source position JS,component JC
!  BTD_SCAT(JT,JS,JC) - scattered field: units = QUNIT
!          JC = 1 => in-line component; 2 => transverse component;  3 => vertical component

 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80)
 REAL, PARAMETER ::  TOL=1.E-3
 INTEGER NW,np,PRFL,NSTAT,LINE(NSTAT),NCHNL,MCHNL,CMP,KPPM,JC,JS
 REAL TMS(NCHNL),RZ(NSTAT,1),PRM_TD(3),XBD,ZBD,XRN,NORM(4), &
      PPFAC,BFFAC,YTR(NCHNL,NSTAT),XQ(4)
 REAL, DIMENSION(NSTAT) :: SZ,XRX,YRX,ZRX,TXDEG
 REAL, DIMENSION(NCHNL,NSTAT,3) :: BTD,BTD_SCAT
 REAL, ALLOCATABLE :: QDATA(:)
 REAL(KIND=QL) SXD(NSTAT),SYD(NSTAT),RXD(NSTAT,1),RYD(NSTAT,1)
 LOGICAL WL,PRTSEC,PRTYBD,PRTX,PRTY,PRTZ
 CHARACTER (LEN=120) TITLE
 CHARACTER (LEN=25) XLOC(2),XLC
 CHARACTER (LEN=10) CMP2,QL0,QL1
 CHARACTER (LEN=8) CMP3
 CHARACTER (LEN=7) CMP1
 CHARACTER (LEN=5) YLC,ZLOC(2),ZLC
 CHARACTER (LEN=4) QUNIT,BUNIT

 DATA ZLOC(1:2) /'below','above'/
 DATA XLOC(1:2) /'behind the transmitter.  ','ahead of the transmitter.'/
 DATA CMP1,CMP2,CMP3 /'IN-LINE','TRANSVERSE','VERTICAL'/

!  Set up receiver locations and write TITLE.

! Normalisation isn't defined for step output and dB/dt waveform calibration
! It isn't used for the pure rectangular step waveform.

 PRTX = .TRUE.
 PRTY = .TRUE.
 PRTZ = .TRUE.
 IF (CMP /= 3) PRTY = .FALSE.
 IF (CMP == 11) PRTZ = .FALSE.
 IF (CMP == 13) PRTX = .FALSE.

 NORM = 1.
 IF (KPPM == 0) THEN                 !  Compute fields in requied units
   BTD =      BFFAC * BTD
   BTD_SCAT = BFFAC * BTD_SCAT

 ELSE IF (KPPM > 0) THEN            !  Compute normalised response
   NORM(1) = ABS (PRM_TD(1))
   NORM(2) = ABS (PRM_TD(2))
   NORM(3) = ABS (PRM_TD(3))
   NORM(4) = SQRT (NORM(1)**2 + NORM(2)**2 + NORM(3)**2)

   DO JC = 1,3
     XRN = NORM(JC) / NORM (4)
     IF (XRN < TOL) NORM(JC) = NORM(4)
   END DO

   IF (KPPM == 4) NORM(1:3) = NORM(4)                   ! TOTAL primary normalisation
   IF (KPPM == 1 .OR. KPPM == 3) NORM(1:3) = NORM(KPPM) ! vertical or in-line normalisation

   DO JC = 1,3
     BTD(1:NCHNL,1:NSTAT,JC) =      PPFAC * BTD(1:NCHNL,1:NSTAT,JC)      / NORM(JC)
     BTD_SCAT(1:NCHNL,1:NSTAT,JC) = PPFAC * BTD_SCAT(1:NCHNL,1:NSTAT,JC) / NORM(JC)
   END DO
 END IF

 XBD = ABS (XRX(1) )
 ZBD = ABS (ZRX(1) )
 ZLC = ZLOC(1)
 XLC = XLOC(1)
 IF (ZRX(1) < 0) ZLC = ZLOC(2)
 IF (XRX(1) < 0) XLC = XLOC(2)

 WRITE(NW,1)
 IF (CMP == 3) WRITE(NW,41)
 PRTYBD = .FALSE.
 IF (ABS (YRX(1)) > .5) THEN
   PRTYBD = .TRUE.
   YLC = 'left'
   IF (YRX(1) < 0.) YLC = 'right'
 END IF

 XQ(1:3) = BFFAC * PRM_TD(1:3)
 XQ(4) = BFFAC * NORM(4)
 IF (KPPM > 0) THEN   !  Print out primary fields
   WRITE(NW,9)  XQ(3),BUNIT, XQ(1),BUNIT,XQ(2),BUNIT,XQ(4),BUNIT

   IF (KPPM == 1) WRITE(NW,11)
   IF (KPPM == 3) WRITE(NW,13)
   IF (KPPM == 4) WRITE(NW,14)
   IF (KPPM == 123) WRITE(NW,12)
 END IF

 WRITE(NW,3) ZBD,ZLC,XBD,XLC
 IF (PRTYBD) WRITE(NW,2) ABS (YRX(1) ),YLC

 IF (PRFL == 2) THEN
   WRITE(NW,4)
   WRITE(NW,5) TRIM (QUNIT)
   WRITE(NW,6)
 END IF

 IF (PRTZ) THEN  !  Total vertical component
   WRITE(NW,15) TRIM (TITLE)
   WRITE(NW,7) CMP3,TRIM (QUNIT)
   YTR(1:NCHNL,1:NSTAT) = BTD(1:NCHNL,1:NSTAT,3)
   CALL WRSLV (NW,PRFL,RZ,RXD,RYD,NSTAT,SZ,SXD,SYD,NCHNL,TMS,YTR)
 END IF

 IF (PRTX) THEN   !  Total in-line component
   WRITE(NW,7) CMP1,TRIM (QUNIT)
   YTR(1:NCHNL,1:NSTAT) = BTD(1:NCHNL,1:NSTAT,1)
   CALL WRSLV (NW,PRFL,RZ,RXD,RYD,NSTAT,SZ,SXD,SYD,NCHNL,TMS,YTR)
 END IF

 IF (PRTY) THEN            !  Total transverse component
   WRITE(NW,7) CMP2,TRIM (QUNIT)
   YTR(1:NCHNL,1:NSTAT) = BTD(1:NCHNL,1:NSTAT,2)
   CALL WRSLV (NW,PRFL,RZ,RXD,RYD,NSTAT,SZ,SXD,SYD,NCHNL,TMS,YTR)
 END IF

!  Write out scattered voltages stripped of layered 1/2 space responses
!  if requested
!  ====================================================================

 IF (PRTSEC) THEN
   WRITE(NW,30) TRIM (QUNIT)

   IF (PRTZ) THEN    !  Scattered vertical component
   WRITE(NW,15) TRIM (TITLE)
     WRITE(NW,31) CMP3,TRIM (QUNIT)
     YTR(1:NCHNL,1:NSTAT) = BTD_SCAT(1:NCHNL,1:NSTAT,3)
     CALL WRSLV (NW,PRFL,RZ,RXD,RYD,NSTAT,SZ,SXD,SYD,NCHNL,TMS,YTR)
   END IF

   IF (PRTX) THEN    !  Scattered in-line component
     WRITE(NW,31) CMP1,TRIM (QUNIT)
     YTR(1:NCHNL,1:NSTAT) = BTD_SCAT(1:NCHNL,1:NSTAT,1)
     CALL WRSLV (NW,PRFL,RZ,RXD,RYD,NSTAT,SZ,SXD,SYD,NCHNL,TMS,YTR)
   END IF

   IF (PRTY) THEN    !  Scattered transverse component
     WRITE(NW,31) CMP2,TRIM (QUNIT)
     YTR(1:NCHNL,1:NSTAT) = BTD_SCAT(1:NCHNL,1:NSTAT,2)
     CALL WRSLV (NW,PRFL,RZ,RXD,RYD,NSTAT,SZ,SXD,SYD,NCHNL,TMS,YTR)
   END IF
 END IF    ! End scattered field printout

!  Finish writing MarcoAir.mf1

 MCHNL = NCHNL
 IF (CMP < 4) MCHNL = CMP * NCHNL
 ALLOCATE (QDATA(MCHNL))

 DO JS = 1,NSTAT
   IF (CMP /= 11) THEN
     QDATA(1:NCHNL) = BTD(1:NCHNL,JS,3)
     IF (CMP < 4) QDATA(NCHNL+1:2*NCHNL) = BTD(1:NCHNL,JS,1)
     IF (CMP == 3) QDATA(2*NCHNL+1:3*NCHNL) = BTD(1:NCHNL,JS,2)
   ELSE
     QDATA(1:NCHNL) = BTD(1:NCHNL,JS,1)
   END IF

   WL = .TRUE.
   IF (JS > 1 .AND. LINE(JS) == LINE(JS-1)) WL = .FALSE.
   IF (WL) THEN
     WRITE(QL0,'(I10)') LINE(JS)
     READ(QL0,'(A)') QL1
     WRITE(np,'(2A)') 'Line ',TRIM (ADJUSTL (QL1))
   END IF
   WRITE(np,20) JS,SYD(JS),SXD(JS),SZ(JS),TXDEG(JS),RYD(JS,1),RXD(JS,1),RZ(JS,1),QDATA(1:MCHNL)

 END DO

 1 FORMAT(//T3,'TIME-DOMAIN MarcoAir OUTPUT'/T3,28('-') &
          //T3,'The IN-LINE component is defined as the horizontal component along' &
           /T3,'the flight path.  It is positive in the forward flight direction.')
 2 FORMAT  (T3,'It is ',F6.1,' metres to ',A)
 3 FORMAT(/T3,'The receiver is',F6.1,' metres ',A,' and',F6.1,' metres ',A)
 4 FORMAT(/T57,'Altitude' &
          /T3,'The first 3 rows of columns 3-8 are the transmitter',T57,'East' &
          /T57,'North   coordinates.' &
         //T57,'Altitude' &
          /T12,'below which are the corresponding receiver',T57,'East' &
          /T57,'North   coordinates.' &
         //T3,'Underneath each receiver (Altitude, East, North) triplet,')
 5 FORMAT(T3,'the output is printed in ',A)
 6 FORMAT(T3,'Channel times are in milliseconds from the start of the transmitter pulse', &
         /T3,'to the centre of the receiver window.')
 7 FORMAT(/T11,A,' COMPONENT - ',A/)
 9 FORMAT(/T20,'  Vertical primary =',G12.4,1X,A &
          /T20,'   In-line primary =',G12.4,1X,A &
          /T20,'Transverse primary =',G12.4,1X,A &
          /T20,'     Total primary =',G12.4,1X,A)
 11 FORMAT(//T3,'Each component is normalised to the in-line primary field')
 12 FORMAT(/T3,'Each component is normalised to its corresponding primary field')
 13 FORMAT(/T3,'Each component is normalised to the vertical primary field')
 14 FORMAT(/T3,'Each component is normalised to the total primary field')
 15 FORMAT(//T2,'TITLE:  ',A/T2,'-----')
 20 FORMAT(I5,2F12.1,F8.1,F6.1,2F12.1,F8.1, 1024e15.6)
 30 FORMAT (/5X,'SCATTERED RESPONSE -  UNITS = ',A/T6,33('-') &
            /7X,'The layered earth response is stripped from each component'/)
 31 FORMAT(10X,'SCATTERED ',A,' COMPONENT - ',A/)
 41 FORMAT(/T3,'The TRANSVERSE component is the horizontal component along' &
           /T3,'perpendicular to the flight path.')

END SUBROUTINE WRITE_TD

 SUBROUTINE WRSLV (NW,PRFL,RZ,RXD,RYD,NSTAT,SZ,SXD,SYD,NCHNL,TMS,YTR)
!--------------------------------------------------------------------

!***  Called by: WRITE_TD
!***      Calls: WRSLVP

!  Writes time-domain output in temporal form for receiver

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80), NCOL=30
 INTEGER NW,PRFL,NSTAT,NCHNL,NBLKS,J1,JB,JF,JT
 REAL RZ(NSTAT,1),SZ(NSTAT),TMS(NCHNL),YTR(NCHNL,NSTAT)
 REAL(KIND=QL) SXD(NSTAT),SYD(NSTAT),RXD(NSTAT,1),RYD(NSTAT,1)

 IF (PRFL == 1) THEN
   CALL WRSLVP (NW,SXD,SYD,SZ,NSTAT,NCHNL,TMS,YTR)
 ELSE

   NBLKS = NSTAT / NCOL
   IF (MOD (NSTAT,NCOL) > 0) NBLKS = NBLKS + 1

   DO J1 = 1,NBLKS
     JF = J1 * NCOL
     JB = JF - NCOL + 1
     JF = MIN (JF,NSTAT)

     WRITE(NW,'(/T15,A,F10.0,29F13.0)') 'Z',SZ(JB:JF)
     WRITE(NW,'(T2,A,F10.0,29F13.0)') 'Transmitter  E',SYD(JB:JF)
     WRITE(NW,'(T2,A,F10.0,29F13.0)') 'Positions    N',SXD(JB:JF)
     WRITE(NW,'(/T2,A,F10.0,29F13.0)') 'Rx Positions Z',RZ(JB:JF,1)
     WRITE(NW,'(T15,A,F10.0,29F13.0)') 'E',RYD(JB:JF,1)
     WRITE(NW,'(T2,A,F10.0,29F13.0)') 'CHNL   TIME  N',RXD(JB:JF,1)
     WRITE(NW,'(3X)')

     DO JT = 1, NCHNL
       WRITE(NW,'(I3,F9.3,3X,30G13.4)') JT,TMS(JT),YTR(JT,JB:JF)
     END DO
     WRITE(NW,1)
   END DO
 END IF

 1 FORMAT (85('-')/)

END SUBROUTINE WRSLV

 SUBROUTINE WRSLVP (NW,SXD,SYD,SZ,NSTAT,NCHNL,TMS,YTR)
!-----------------------------------------------------

!***  Called by: WRSLV

!  Writes time-domain output in profile form for receiver
 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80), NCOL=30
 INTEGER NW,NSTAT,NCHNL,NBLKS,J1,JB,JF,KRX,KRY,KRZ,JS
 REAL SZ(NSTAT,1),TMS(NCHNL),YTR(NCHNL,NSTAT)
 REAL(KIND=QL) SXD(NSTAT,1),SYD(NSTAT,1)
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

 NBLKS = NCHNL / NCOL
 IF (MOD (NCHNL,NCOL) > 0) NBLKS = NBLKS + 1

 DO J1 = 1,NBLKS
   JF = J1 * NCOL
   JB = JF - NCOL + 1
   JF = MIN (JF,NCHNL)

   WRITE(NW,1) CHN(JB:JF)
   WRITE(NW,2) TMS(JB:JF)
   WRITE(NW,'(3X)')
   DO JS = 1, NSTAT
     KRX = 0
     KRY = 0
     KRZ = NINT (SZ(JS,1))
     KRX = NINT (SXD(JS,1))
     KRY = NINT (SYD(JS,1))
     WRITE(NW,3) JS,KRY,KRX,KRZ,YTR(JB:JF,JS)
   END DO
   WRITE(NW,5)
 END DO

 1 FORMAT(T10,'TRANSMITTER POSITION',T35,30(A:,5X))
 2 FORMAT(T10,'EAST     NORTH    ALT',F10.3,29F13.3)
 3 FORMAT(I3,2I10,I7,1X,30G13.4)
 5 FORMAT (85('-')/)

END SUBROUTINE WRSLVP

!==================================================================================
!  ROUTINES SPECIFIC FOR MARCO_AIR
!  -------------------------------

 SUBROUTINE CHK_SYMMETRY (NPRISM,NP11,LBLK,PBLK,TE,TN,KSYMM)
!-----------------------------------------------------------

!  Created for MARCO_AIR 2.0, November, 1999

!  Checks to see if the structure of NPRISM prisms is symmetric about some
!  point (TE,TN) in the horizontal plane.  If so, KSYMM is set to 1, the
!  coordinate system is shifted to (TE,TN) Otherwise, KSYMM = 0

!  TE & TN are the east & north coordinates of the geometric centre projected
!  onto the 2D horizontal plane.  NPRNE is the number of prisms wholly or
!  partially within the NE quadrant in the (TE,TN) centred system.

!    ENTRY
!    -----
!
!   PBLK(J,1) = PRISM_EAST, east coordinate of PRISM J
!   PBLK(J,2) = PRISM_NORTH
!   PBLK(J,3) = PRISM_TOP, depth (from air-earth interface) to top of PRISM J
!   PBLK(J,4) = PRISM_SIZE_EW, for PRISM J
!   PBLK(J,5) = PRISM_SIZE_NS, for PRISM J
!   PBLK(J,6) = PRISM_SIZE_Z, vertical dimension of PRISM J
!   PBLK(J,7) = CELL_SIZE_EW, for PRISM J
!   PBLK(J,8) = CELL_SIZE_NS, for PRISM J
!   PBLK(J,9) = CELL_SIZE_Z, vertical dimension of PRISM J

!   LBLK(J,1) = lithology index PRISM J
!   NP11 = 11 * original value of NPRISM

!  On EXIT, if the system is symmetrical, NPRISM is redefined to be NPRNE
!  and prism sizes adjusted to their size and location in NE quadrant
!  in accordance with the KSYMM = 1 description in pre 2.0 versions.
!  The new PRISM structure is shifted to be in the first NPRNE rows of
!  PBLK & LBLK.

!  If the system is not symmetric, no change is made.

 REAL, PARAMETER :: EWNS= 0.1  ! Spatial tolerance = 0.1 m
 INTEGER NPRISM,NP11,LBLK(NP11,2),KSYMM,NPRNE,NQ,N4SYM,N2SYMEW,N2SYMNS,N1SYM, &
         J,J1,JS,KSTRT,KFIN,MATCH,MATCH3,JN
 REAL PBLK(NP11,9),TE,TN,E_CHK(5),N_CHK(5),QE,QN,QS,QW,TMPE,TMPN
 INTENT (IN) NP11
 INTENT (OUT) TE,TN,KSYMM
 INTENT (INOUT) NPRISM,LBLK,PBLK


!  For the trivial case of 1 prism, restructure to fit MARCO_3D and RETURN.

 KSYMM = 1
 IF (NPRISM == 1) THEN
   TE = PBLK(1,1)
   TN = PBLK(1,2)
   PBLK(1,4) = PBLK(1,4) / 2.  !  Reduce size to 1/4 of original
   PBLK(1,5) = PBLK(1,5) / 2.
   PBLK(1,1) = PBLK(1,4) / 2.  !  New east and north coordinates are
   PBLK(1,2) = PBLK(1,5) / 2.  !  the centre of the reduced PRISM

!===========================
   RETURN
!===========================

 END IF

!  Compute TE & TN

 TN = 0
 TE = 0
 DO J = 1, NPRISM
   TE = TE + PBLK(J,1)
   TN = TN + PBLK(J,2)
 END DO
 TN = TN / REAL (NPRISM)
 TE = TE / REAL (NPRISM)

!  Transform into model centre coordinates and look for trivial symmetry violation.
!  Reorder so that the first NPRNE prisms are in the NE quadrant.

 NPRNE = 0     ! number of prisms partially or wholly within NE quadrant
 NQ = 0        ! NPRISM - NPRNE
 N4SYM = 0     ! number of prisms centred on origin
 N2SYMEW = 0   ! number of prisms centred on E-W axis only
 N2SYMNS = 0   ! number of prisms centred on N-S axis only
 N1SYM = 0     ! number of prisms entirely in upper right quadrant (URHQ)

 SL1: DO J = 1, NPRISM
   TMPE = PBLK(J,1) - TE
   TMPN = PBLK(J,2) - TN

!  If the prism straddles N-S axis but isn't centred on it, or
!  if the prism straddles E-W axis but isn't centred on it, the model
!  isn't symmetric.  In this case, EXIT SL1

   QE = TMPE + PBLK(J,4) / 2.    ! Prism size E-W
   QW = TMPE - PBLK(J,4) / 2.
   IF (QE * QW < -EWNS .AND. ABS (TMPE) > EWNS) KSYMM = 0
   IF (KSYMM == 0) EXIT SL1

   QN = TMPN + PBLK(J,5) / 2.    ! Prism size N-S
   QS = TMPN - PBLK(J,5) / 2.
   IF (QN * QS < -EWNS .AND. ABS (TMPN) > EWNS) KSYMM = 0
   IF (KSYMM == 0) EXIT SL1

   IF (TMPE > -EWNS .AND. TMPN > -EWNS) THEN
     NPRNE = NPRNE + 1           ! centre is in or on boundary of NE quadrant
     JN = NPRISM + NPRNE
     IF (TMPE < EWNS) THEN       ! centre is on upper N-S axis
       IF (TMPN < EWNS) THEN     ! centre is at origin
         N4SYM = N4SYM + 1
         LBLK(JN,2) = 4
       ELSE                      ! centre is on upper N-S axis but not at origin
         N2SYMEW = N2SYMEW + 1       ! East-West symmetry for this block
         LBLK(JN,2) = 21         ! Need to find match south of E-W axis
       END IF
     ELSE IF (TMPN < EWNS) THEN  ! centre is on right E-W axis but not at origin
       N2SYMNS = N2SYMNS + 1       ! East-West symmetry for this block
       LBLK(JN,2) = 22           ! Need to find match west of N-S axis
     ELSE
       N1SYM = N1SYM + 1         ! centre is entirely in NE quadrant
       LBLK(JN,2) = 1            ! Need to find matches in other 3 quadrants
     END IF
   ELSE
     NQ = NQ + 1                 ! no part of the prism is in the NE quadrant
     JN = 2*NPRISM + 1 - NQ      ! fill in from 2*MPRISM upwards
   END IF

   LBLK(JN,1) = LBLK(J,1)
   PBLK(JN,1) = TMPE
   PBLK(JN,2) = TMPN
   PBLK(JN,3:9) = PBLK(J,3:9)
 END DO SL1

 IF (N4SYM + 2* (N2SYMNS + N2SYMEW) + 4* N1SYM /= NPRISM) KSYMM = 0

!===========================
 IF (KSYMM == 0) RETURN
!===========================

! Look for matches

 KSTRT = NPRISM + NPRNE + 1
 KFIN = 2*NPRISM

 SL3: DO J = NPRISM + 1, NPRISM + NPRNE
   E_CHK(1) = 0.          ! Find matching prism south of E-W axis
   N_CHK(1) = -PBLK(J,2)

   E_CHK(2) = -PBLK(J,1)  ! Find matching prism west of N-S axis
   N_CHK(2) = 0.

   E_CHK(3) =  PBLK(J,1)  ! Find matching prism in SE quadrant
   N_CHK(3) = -PBLK(J,2)

   E_CHK(4) = -PBLK(J,1)  ! Find matching prism in SW quadrant
   N_CHK(4) = -PBLK(J,2)

   E_CHK(5) = -PBLK(J,1)  ! Find matching prism in NE quadrant
   N_CHK(5) =  PBLK(J,2)

   MATCH = 1
   IF (LBLK(J,2) == 4) CYCLE SL3 ! prism centred on origin

   IF (LBLK(J,2) == 21) THEN     ! look for EW symmetric prism
     DO J1 = KSTRT, KFIN         ! centred on negative NS axis
       CALL CHK_SYM_PROP (J,J1,N_CHK(1),E_CHK(1),MATCH)
       IF (MATCH == 1) CYCLE SL3
     END DO
     IF (MATCH == 0) EXIT SL3

   ELSE IF (LBLK(J,2) == 22) THEN  ! look for NS symmetric prism
     DO J1 = KSTRT, KFIN           ! centred on negative EW axis
       CALL CHK_SYM_PROP (J,J1,N_CHK(2),E_CHK(2),MATCH)
       IF (MATCH == 1) CYCLE SL3
     END DO
     IF (MATCH == 0) EXIT SL3

   ELSE IF (LBLK(J,2) == 1) THEN
     MATCH3 = 0
     SL4: DO JS = 3,5           ! Find matches in other three quadrants
       DO J1 = KSTRT, KFIN
         CALL CHK_SYM_PROP (J,J1,N_CHK(JS),E_CHK(JS),MATCH)
         IF (MATCH == 1) THEN
           MATCH3 = MATCH3 + 1  ! Success in quadrant I = JS - 2
           CYCLE SL4
         END IF
       END DO
       IF (MATCH == 0) EXIT SL3
     END DO SL4
   END IF
 END DO SL3

 KSYMM = MATCH
 IF (KSYMM == 1) THEN  !  Set up NPRNE prisms as stand alone
   DO J = 1, NPRNE     !  Relocate to top of matrix
     JS = NPRISM + J
     LBLK(J,1:2) = LBLK(JS,1:2)
     PBLK(J,3) = PBLK(JS,3)
     PBLK(J,6:9) = PBLK(JS,6:9)

     IF (LBLK(JS,2) == 4) THEN      !  Reduce block currently centred at origin
       PBLK(J,4) = PBLK(JS,4) / 2.  !  Reduce size to 1/4 of original
       PBLK(J,5) = PBLK(JS,5) / 2.
       PBLK(J,1) = PBLK(J,4) / 2.   !  Relocate reduced block centre to NE quadrant
       PBLK(J,2) = PBLK(J,5) / 2.   !

     ELSE IF (LBLK(JS,2) == 21) THEN !  Reduce block currently in NE & NW quadrants
       PBLK(J,4) = PBLK(JS,4) / 2.   !  Reduce size to 1/2 of original
       PBLK(J,5) = PBLK(JS,5)
       PBLK(J,1) = PBLK(J,4) / 2.    !  Relocate reduced block centre to NE quadrant
       PBLK(J,2) = PBLK(JS,2)        !

     ELSE IF (LBLK(JS,2) == 22) THEN !  Reduce block currently in NE & SE quadrants
       PBLK(J,4) = PBLK(JS,4)
       PBLK(J,5) = PBLK(JS,5) / 2.   !  Reduce size to 1/2 of original
       PBLK(J,1) = PBLK(JS,1)
       PBLK(J,2) = PBLK(J,5) / 2.    !  Relocate reduced block centre to NE quadrant

     ELSE IF (LBLK(JS,2) == 1) THEN
       PBLK(J,1:5) = PBLK(JS,1:5)
     END IF
   END DO
   NPRISM = NPRNE
 END IF

 CONTAINS

   SUBROUTINE CHK_SYM_PROP (L1,L2,N1,E1,MATCH)
!  -------------------------------------------

!  Compares locations, depth to top and dimensions of prisms L1 * L2.
!  If all agree within tolerance EWNS, MATCH = 1
!  If one property is outside the tolerance, MATCH = 0

   INTEGER L1,L2,MATCH,JD
   REAL DIFF1,N1,E1

   MATCH = 1
   IF (LBLK(L1,1) /= LBLK(L2,1) ) MATCH = 0

   DIFF1 = ABS (E1 - PBLK(L2,1))
   IF (DIFF1 > EWNS) MATCH = 0

   DIFF1 = ABS (N1 - PBLK(L2,2))
   IF (DIFF1 > EWNS) MATCH = 0

   DO JD = 3,9
     DIFF1 = ABS (PBLK(L1,JD) - PBLK(L2,JD))
     IF (DIFF1 > EWNS) MATCH = 0
   END DO

   END SUBROUTINE CHK_SYM_PROP

 END SUBROUTINE CHK_SYMMETRY


 SUBROUTINE SKINS_3D (NLYR,THK,KSQ_LYR,PLTOP0,TOO_DEEP)
!------------------------------------------------------

!  SKINS_3D computes PLTOP_SD, the depth to the shallowest target block
!  in skin depths, from PLTOP0, the minimum target depth, in metres.
!  THK_SD is an array of the thicknesses of each layer normalised to the skin
!  depth of that layer.  Thus, if, for example, the target is layer 4,
!  PLTOP_SD is the sum of THK_SD(1:3) + the distance from the bottom of layer 3
!  normalised to the skin depth of layer 4.

!  SKIN_TOL is the minimum integer skin depths such that the 3D response
!  is less than 10**(-ACC_3D) * the half space response.
!  If the depth to the shallowest target is more than SKIN_TOL skin depths,
!  then the response from the target is completely buried by the half-space
!  response.

!  In this case, TOO_DEEP is true and the 3D part of the computation is
!  stopped for all higher frequencies in the MAIN PROGRAM.


!  NLYR = number of layers;  NSHT = number of sheets
!  THK = thickness of layers.
!  KSQ_LYR = the layered earth propagation constants.
!  PLTOP0 = minimum depth to top of sheets in m.

!
!  ** This routine is slightly modified from the version in MarcoAir.
!     In order to minimise array data passing, some arrays are defined
!     as fixed-size array with a maximal number of layer of 200.
!
!

!***  Called by MARCO_3D

 IMPLICIT NONE
 INTEGER, PARAMETER :: ACC_3D=4
 INTEGER NLYR,SKIN_TOL,JL
! REAL PLTOP0,PLTOP_SD,THK(NLYR),THK_SD(NLYR),RK(NLYR),DEPTHL(NLYR),DEPTH_SD(NLYR)
 REAL PLTOP0,PLTOP_SD,THK(NLYR),THK_SD(200),RK(200),DEPTHL(200),DEPTH_SD(200)
 COMPLEX KSQ_LYR(NLYR)
 LOGICAL TOO_DEEP

 INTENT (IN) NLYR,THK,KSQ_LYR,PLTOP0
 INTENT (OUT) TOO_DEEP

 SKIN_TOL = CEILING (ACC_3D * LOG(10.) )

!  Compute RK, the inverse skin depth and THK_SD, thickness of layers
!  normalised to skin depths.  Compute depths to the bottom of each layer
!  in metres (DEPTHL) and skin depths (DEPTH_SD)

 RK(1:NLYR) = REAL (SQRT (KSQ_LYR(1:NLYR)))
 DEPTHL(NLYR) = 1.E6
 DEPTH_SD(NLYR) = 1.E6

 IF (NLYR == 1) THEN
   PLTOP_SD = PLTOP0 * RK(1)
 ELSE
   THK_SD(1:NLYR-1) = THK(1:NLYR-1) * RK(1:NLYR-1)
   DEPTHL(1) = THK(1)
   DEPTH_SD(1) = THK(1)

   IF (NLYR > 2) THEN
     DO JL = 2,NLYR-1
       DEPTHL(JL) =   DEPTHL(JL-1) +   THK(JL)
       DEPTH_SD(JL) = DEPTH_SD(JL-1) + THK_SD(JL)
     END DO
   END IF

   IF (PLTOP0 < THK(1)) THEN
     PLTOP_SD = PLTOP0 * RK(1)
   ELSE
     PLTOP_SD = THK_SD(1)
     DO JL = 2, NLYR
       IF (PLTOP0 < DEPTHL(JL)) THEN
         PLTOP_SD = PLTOP_SD + RK(JL) * (PLTOP0 - DEPTHL(JL-1) )
       ELSE
         PLTOP_SD = PLTOP_SD + THK_SD(JL)
       END IF
     END DO
   END IF
   DEPTH_SD(1:NLYR-1) = SUM (THK_SD(1:NLYR-1))
 END IF

 TOO_DEEP = .FALSE.
 IF (PLTOP_SD > SKIN_TOL) TOO_DEEP = .TRUE.

END SUBROUTINE SKINS_3D

 SUBROUTINE MARCO_3D (NLG,TDFD,NFRQ,FREQ,NLAYER,LRYTH,RES_EARTH,CHRG_EARTH,TAU_EARTH, &
                      FRQC_EARTH,REPSL,RMUL,SOLVER,KACC,KSYMM,TRGT_BLCK,BLCK_NX,      &
                      BLCK_NY,BLCK_NZ,BLCK_LX,BLCK_LY,BLCK_LZ,BLCK_CX,BLCK_CY,        &
                      BLCK_CZ,RSB,CHRSB,TAUSB,CFRSB,REPSP,RMUP,NRXST,NTX,SX,SY,SZ,    &
                      TXCLN,FANGLE,NRX,RX,RY,RZ,BFD_SCAT)
!-----------------------------------------------------------------------------------

!***  Called from MAIN
!***  Calls: INIT_3D_INPUT_TEST, INIT_CMPLX_CD_1D, INIT_CMPLX_CD_3D, INIT_COMPUTATION
!            INIT_REF_CELL_DIM, INIT_RHOMAX, INIT_SUPER_BLOCK, INIT_ZLVLS,
!            INIT_ZLVLSCS, MAIN_MATRICES MTRX, MAIN_PRM_AT_CELL, MAIN_PRM_AT_RCV,
!            MAIN_SCAT_EH_CS, MAIN_SOLVER, MAIN_SUPER_GRID

! BFD_SCAT is the scattered field

!**** This is an airborne module of the program Marco in subroutine form.
!     This works only for magnetic dipoles with arbitrary orientations.
!     Program portions in Marco that are not relevant to this excitation
!     are stripped as much as possible with minimal impact on the
!     compatibility of the routines with those in original Marco.  Some
!     routine bear the same name as their counterparts in Marco but are
!     specialised for magnetic dipole excitations and thus should not be
!     confused with those in Marco.
!
!     Arrays are dynamically allocated where permitted.  Thus the memory
!     requirement is kept minimal.  There is a parameter controlling the
!     size of the scattering (sub-) matrix to be allowed for matrix inversion,
!     parameter SUB_CELL_MAX,  see below.  Although this parameter does not
!     restrict the total number of cells to be allowed for the modelling
!     procedure,  which is in fact unrestricted by computer memory because of
!     the use of the method of system iteration,  it does, however, affect
!     the efficiency of airborne computations as there are many transmitters
!     involved.   SUB_CELL_MAX controls the maximal number of cells in
!     one substructure whose scattering matrix is factorised by standard
!     matrix factorization.  This can then be reused for many transmitters.
!     Therefore,  it is advisable to use fewer cells than allowed by
!     SUB_CELL_MAX for non-symmetric structures and 4xSUB_CELL_MAX for
!     symmetric structures.  Structures with two plane-symmetries are
!     computed with group symmetry reduction and computations are reduced
!     to one quarter only,  thus allowing 4 times the number of cells
!     to be treated by direct matrix factorization.  Input data for
!     symmetric models are required for one quarter only.
!
!     SUB_CELL_MAX=500 limits the memory requirement for the scattering
!     matrix to be less than 8x(3x500)**2 = 18 M-bytes.  Total memory
!     requirements for the computation are in general a few more M-bytes.
!
! ***  Parameter SUB_CELL_MAX is now disabled in favour of SOLVER.  It is
!      now determined by SOLVER and the total number of cells of the model
!
!
!  Inpute parameters:
!
!     NLG:      Integer,  output unit.
!     NFRQ:    Integer,  number of frequencies.
!     FREQ:    Real FREQ(NFRQ), array for the frequencies.
!     NLAYER:  Integer,  number of layers of the earth
!     SIG_1D:  Complex SIG_1D(NLAYER,NFRQ), complex conductivities of
!              the layers for all the frequencies.
!     LRYTH:   Real LRYTH(MLAYER), the thickness of the layers.
!     REPSL:   Real, relative dielectric constants of the layers.
!     RMUL:    Real, relative permeability of the layers.
!     SOLVER:
!     KACC:    Integer,  accuracy level for the computation of the scattered
!              fields.
!              CHOOSE 2 OR 3 FOR ACCURATE COMPUTATION.
!              KACC = 1 invokes a fast solver that uses a fast approximation
!              for the solution of the matrix equation.
!     KSYMM:   Integer,  parameter indicating if two-plane symmetry
!              is present in the 3D model.
!              KSYMM=1:  model with two plane-symmetries;
!                   =0:  non-symmetric model.
!     TRGT_BLCK: Integer,  number of input blocks to be divided into
!                substructures.
!     BLCK_NX: Integer BLCK_NX(TRGT_BLCK),  number of cells in the
!              x-direction of blocks.
!     BLCK_NY: Integer BLCK_NY(TRGT_BLCK), number of cells in the
!              y-direction of blocks.
!     BLCK_NZ: Integer BLCK_NZ(TRGT_BLCK),  number of cells in the
!              z-direction of blocks.
!     BLCK_LX: Real BLCK_LX(TRGT_BLCK),  dimensions of the blocks in the
!              x-direction.
!     BLCK_LY: Real BLCK_LY(TRGT_BLCK),  dimensions of the blocks in the
!              y-direction.
!     BLCK_LZ: Real BLCK_LZ(TRGT_BLCK),  dimensions of the blocks in the
!              z-direction.
!     BLCK_CX: Real BLCK_CX(TRGT_BLCK),  x-coordinates of the block centres.
!     BLCK_CY: Real BLCK_CY(TRGT_BLCK),  y-coordinates of the block centres.
!     BLCK_CZ: Real BLCK_CZ(TRGT_BLCK),  z-coordinates of the block centres.
!     RSB:     Real RSB(TRGT_BLCK),  resistivities of the blocks.
!     CHRSB:   Real CHRSB(TRGT_BLCK),  chargeability of the blocks (if it
!              is polarisable).  Cole-Cole models are used to presents
!              polarisable models.
!     TAUSB:   Real TAUSB(TRGT_BLCK),  time constants of the blocks.
!     CFRSB:   Real CFRSB(TRGT_BLCK),  frequency constants of the blocks.
!     REPSP:   Real REPSP(TRGT_BLCK), relative dielectric constants of the blocks.
!     RMUP:    Real RMUP(TRGT_BLCK), relative permeability of the blocks.
!     NTX:     Integer, number of transmitters.
!     SX, SY, SZ: Real SX(NTX), SY(NTX), SZ(NTX),  x, y, and z-coordinates
!              of the transmitters, respectively.
!     TXCLN(NRXST):   Real, magnetic dipole dip angle for each frequency.
!     FANGLE:  Real, azimuthal angle of the flight path,  which is also the
!              azimuthal angle of the magnetic dipole transmitters.
!     NRX:     Integer, number of receivers.
!     RX, RY, RZ: Real RX(NTX,NRX),RY(NTX,NRX),RZ(NTX,NRX), receivers sites
!              for all transmitters.
!
!  Output parameters:
!
!     BFD_SCAT:  Complex BFD_SCAT(NFRQ,NTX,3) scattered magnetic field of the
!                3D target.  (returned in nT)
!
!
 IMPLICIT NONE
!
! ---- Parameter SUB_CELL_MAX is the maximal number of cells to be allowed
!      in a substructure.  The parameter defines the maximal size of the
!      scattering matrix.  Note that the total number of cells to be modelled
!      is independent of SUB_CELL_MAX.  In fact,  there is no limit on the
!      total number of cells this program can handle as long as there is
!      enough disk space.
!
! ***  Parameter SUB_CELL_MAX is now disabled in favour of SOLVER.  It is
!      now determined by SOLVER and the total number of cells of the model
!  Parameters for the 1D earths MLAYER==NLAYER.  MLAYER is used because of original design
!   -- AJ is the current amplitude for electric sources or the dipole
!      moment for magnetic sources.  It must be given for each frequency.

 INTEGER, PARAMETER :: MZGRID=300, KCOND=0, KCLMN=0, KSFT=0
 INTEGER, ALLOCATABLE, DIMENSION(:) :: N_RX,NXI,NYI,NZI,NXJ,NYJ,NZJ,NCELLI,NCELLJ, &
                                       NCT,NET,KCELL,SUB_BLOCK,IND
 INTEGER, ALLOCATABLE, DIMENSION(:,:) :: NX,NY,NZ,NCELL
 INTEGER TDFD,NFRQ,NRXST,NLAYER,MLAYER,NTX,NRX,NLG,I,IFRQ,J,JS,COLE_COLE,KSMR,KUTCRP,NBODY,  &
         NCRD,NCTT,NEQ,NBMAX,NTX_FD,NXS,NYS,NOBCS,NSRCS,NZOB,NZSR,NZS,SUB_CELL_MAX,KSYMM, &
         TRGT_BLCK,KACC,SOLVER,NMAX,NSMR,MBODY,M_RX,NHFILM,NSUBCM,NXMAX,NYMAX,NZMAX
 INTEGER, DIMENSION (TRGT_BLCK) :: BLCK_NX,BLCK_NY,BLCK_NZ

! BFAC = 1.E9 * mu0
 REAL, PARAMETER :: BLMIN=0., BFAC=400.*3.14159
 REAL, ALLOCATABLE, DIMENSION(:) :: RMU,ZBND,TX_CRDXI,TX_CRDYI,TX_CRDZI,AJ,RRG,RRG3,  &
                                    CLMN,BLXI,BLYI,BLZI,X1,Y1,Z1,VV,GAI
 REAL, ALLOCATABLE, DIMENSION(:,:) :: RX_X,RX_Y,RX_Z,TX_CRDX,TX_CRDY,TX_CRDZ,MD_ANGLE,ZBG, &
                                      RBC,CHRBC,TAUBC,CFRBC,RMUB,REPSB,BLX,BLY,BLZ,XBL,YBL, &
                                      ZBL,XCELLI,YCELLI,ZCELLI,XCELLJ,YCELLJ,ZCELLJ
 REAL, ALLOCATABLE, DIMENSION (:,:,:) :: XCELL,YCELL,ZCELL
 REAL FREQ(NFRQ),CLMN2,FRQ,RMAX1,RMAX2,RMIN1,RMIN2,SBX,SBY,SBZ,SCX,SCY,SCZ,XSB, &
      YSB,ZSB,TXCLN(NRXST),ZOBG(MZGRID),ZSRG(2,MZGRID),ZOBCS(MZGRID),ZSRCS(2,MZGRID),      &
      ALMAX,DMIN,RHOMAX,RHOMIN,ZTOP,CSF,SNF
 REAL, DIMENSION(NLAYER) :: LRYTH,RES_EARTH,CHRG_EARTH,TAU_EARTH,FRQC_EARTH,REPSL,RMUL
 REAL, DIMENSION(NTX) :: FANGLE,SX,SY,SZ
 REAL, DIMENSION(NTX,NRX) :: RX,RY,RZ
 REAL, DIMENSION (TRGT_BLCK) :: BLCK_LX,BLCK_LY,BLCK_LZ,BLCK_CX,BLCK_CY,BLCK_CZ,   &
                                RSB,CHRSB,TAUSB,CFRSB,REPSP,RMUP

 COMPLEX, ALLOCATABLE, DIMENSION(:) :: CDH,KKH,EN,EMT,EJGS,EJGS2,EAX,EAY,EAZ,HAX,HAY,HAZ
 COMPLEX, ALLOCATABLE, DIMENSION(:,:) :: ESX,ESY,ESZ,HSX,HSY,HSZ,GRHO03,CDB,GA,ENT,JST,GSB1,GSB2
 COMPLEX, ALLOCATABLE, DIMENSION(:,:,:) :: GRHO0,GRHF3
 COMPLEX, ALLOCATABLE, DIMENSION(:,:,:,:) :: GRHF
 COMPLEX BFD_SCAT(NFRQ,NTX,3),YLINE,YPERP
 LOGICAL TOO_DEEP
 Integer :: tvals(8)
 Real :: prc

!
! --- Arrays storing the tabulated Hankel integrals.  RRG contains the lateral
!     grid abscissas

!  --- MZGRID is the maximal number of the vertical z-levels of the sources
!      and receivers.   300 should be quite big since it corresponds to an
!      array size of over 600 M-bytes for the array GRHF.
!
!      The arrays ZOBG etc. may also be made allocatable so that the parameter
!      MZGRID is eliminated.  However,  unless the routine INIT_ZLVLS is made an
!      internal routine of the main program this may affect the beauty of the
!      code since determining the z-levels is quite lengthy.


! KSFT=0 => no outcropping for airborne models
!**** Parameters for the 3D body  *******************

! ---- Parameter NSMR controls the size of the array containing
!   all the non-identical matrix elements for spatial symmetry
!   reductions.  Note that NSRM is defined differently from the
!   old version SYSEM since the first subscript of GRHF is changed.
!
!   In case NZTMAX has to be enlarged to allow either more cells
!   in the vertical direction or to allow more downhole receivers,
!   but the array GSB1 and GSB2 is big enough for the problem,
!   the definition of NSMR may changed in order to minimise the
!   memory requirement.  The memory requirement will otherwise
!   increases with NZTMAX quadratically.
!
!
 ALLOCATE (AJ(NFRQ))
!
 AJ=1.
 BFD_SCAT = (0.,0.)

!     COLE_COLE: Integer,  parameter indicating if the model is polarisable.
!                COLE_COLE = 0:  non-polarisable;  and
!                          = 1:  polarisable.

 COLE_COLE = 0
 IF (MAXVAL (CHRG_EARTH) > 1.E-3 .OR. MAXVAL (CHRSB) > 1.E-3) COLE_COLE = 1

! --- Open files RESST and CONDT for storing the resistivity and the
!    complex conductivity of the cells.  These two units must be
!    opened for computation since cell conductivities are needed.

 OPEN (61,FILE='RESST',FORM='UNFORMATTED',STATUS='UNKNOWN')
 OPEN (62,FILE='CONDT',FORM='UNFORMATTED',STATUS='UNKNOWN')

 CALL TX_RX_CONVERTER

! ---- Set parameters for the high or low frequency version (i.e.,
!      whether displacement currents should be included)
!
!      ALMAX if for the truncation of the kernels functions.  It
!      is to be passed into routine ONE_D_0_2_INFTY.
!
!      DMIN is the geometric resolution limit.
!
!      RHOMIN and RHOMAX determine the minimal and maximal dimensions the
!      code can handle,  which is determined by the filters and required
!      size of the array GRHF.  They must agree with the design of
!      routine HFIL (the array dimensions in that routine).
!
!      NHFILM is the total number of grid for the spatial Hankel transforms
!      designed in routine HFIL which uses 15-per-decade filters in the low
!      frequency version and 50-per-decade filters in the high frequency
!      version.
!
!      The value of ALMAX depends on the highest frequency
!
       ALMAX=20.; DMIN=.1;  RHOMIN=0.1
!
!  -- Allocate working array for the electrode coordinates of the loops
!
      ALLOCATE(TX_CRDXI(NCRD),TX_CRDYI(NCRD),TX_CRDZI(NCRD))
!
!  --- Divide the block(s) into substructures if necessary
!
!
!      Parameter NBMAX for the maximal number of block in each substructure
!      is necessary to control the number of blocks in a substructure.

      NBMAX=3

      CALL STRUCTURING

!  --    Allocate 3D parametric arrays
!
      NXMAX=MAXVAL(NX)
      NYMAX=MAXVAL(NY)
      NZMAX=MAXVAL(NZ)
!  -- Reset NBMAX
      NBMAX=MAXVAL(SUB_BLOCK)
      NSUBCM=MAXVAL(NCT)
      MBODY=NBODY         ! MBODY normally have identical functions as NBODY but
                          ! some routines use both

      ALLOCATE(XCELL(NXMAX,NBMAX,NBODY),YCELL(NYMAX,NBMAX,NBODY),          &
               ZCELL(NZMAX,NBMAX,NBODY),BLXI(NBMAX),BLYI(NBMAX),           &
               BLZI(NBMAX),XCELLI(NXMAX,NBMAX),YCELLI(NYMAX,NBMAX),        &
               ZCELLI(NZMAX,NBMAX),XCELLJ(NXMAX,NBMAX),                    &
               YCELLJ(NYMAX,NBMAX),ZCELLJ(NZMAX,NBMAX),X1(NXMAX),          &
               Y1(NYMAX),Z1(NZMAX),NXI(NBMAX),NYI(NBMAX),NZI(NBMAX),       &
               NXJ(NBMAX),NYJ(NBMAX),NZJ(NBMAX),NCELLI(NBMAX),             &
               NCELLJ(NBMAX),NET(NBODY),KCELL(NBODY),CLMN(NBODY))


      MLAYER=NLAYER
      ALLOCATE (ZBND(0:MLAYER),CDH(0:MLAYER),KKH(0:MLAYER),    &
                RMU(0:MLAYER))
!
! ---- Note that after the routine INIT_COMPUTATION is executed,
!    arrays BLX,  BLY,  and BLZ contains the dimensions
!    of the cells instead of the blocks.
!
 CALL INIT_COMPUTATION (DMIN,MBODY,NBMAX,NXMAX,NYMAX,NZMAX,MLAYER,ZBND,   &
                        LRYTH,KSYMM,NBODY,SUB_BLOCK,NX,NY,NZ,BLX,BLY, &
                        BLZ,XBL,YBL,ZBL,X1,Y1,Z1,NCT,NET,NCELL,NCTT,NEQ,  &
                        XCELL,YCELL,ZCELL,KCELL)
!
! ---- Determine the reference length for numerical integration
!    of the Green's functions as required by the accuracy levels
!    determined by the parameter KACC.

 CALL INIT_REF_CELL_DIM(MBODY,NBMAX,NBODY,SUB_BLOCK,BLX,BLY,BLZ,DMIN,KCLMN,CLMN)
!
! ---- Determine if the spatial symmetry reduction can be used
!
      CALL INIT_SUPER_BLOCK (NLG,MBODY,NBMAX,NSMR,KSYMM,DMIN,NXMAX,      &
                             NYMAX,NZMAX,NBODY,SUB_BLOCK,BLX,BLY,BLZ,   &
                             NX,NY,NZ,XBL,YBL,ZBL,XCELL,YCELL,ZCELL,    &
                             SBX,SBY,SBZ,XSB,YSB,ZSB,SCX,SCY,SCZ,NXS,   &
                             NYS,NZS,KSMR)
!
! --- For approximate solutions it is suggested to use spatial
!   symmetry reduction.  Issue warnings if not so.
!
      IF (KACC == 1) WRITE (NLG,1030)
!
      CALL INIT_ZLVLSCS(MZGRID,NTX_FD,M_RX,N_RX,NCRD,TX_CRDZ,RX_Z,        &
                        DMIN,NSRCS,ZSRCS,NOBCS,ZOBCS)
      CALL INIT_ZLVLS(NLG,MBODY,NBMAX,NZMAX,KSMR,NBODY,SUB_BLOCK,         &
                      NZS,NZ,SBZ,SCZ,ZSB,ZCELL,BLZ,                       &
                      MZGRID,DMIN,NZSR,ZSRG,NZOB,ZOBG)
!
      CALL INIT_RHOMAX(MBODY,NBMAX,NXMAX,NYMAX,KSYMM,                     &
                       NBODY,SUB_BLOCK,NX,NY,XCELL,YCELL,BLX,BLY,NTX_FD,  &
                       NCRD,TX_CRDX,TX_CRDY,N_RX,RX_X,RX_Y,               &
                       M_RX,RHOMIN,RMAX1,RMIN1,RMAX2,RMIN2)
!
! --- Test input data after all initializations are done
!
      CALL INIT_3D_INPUT_TEST (NLG,DMIN,TRGT_BLCK,BLCK_LX,              &
                               BLCK_LY,BLCK_LZ,BLCK_CX,BLCK_CY,BLCK_CZ)
!
! ---- Allocate arrays for field components
!
      ALLOCATE(ESX(M_RX,NTX_FD),ESY(M_RX,NTX_FD),ESZ(M_RX,NTX_FD),      &
               HSX(M_RX,NTX_FD),HSY(M_RX,NTX_FD),HSZ(M_RX,NTX_FD))
      ALLOCATE(EAX(M_RX),EAY(M_RX),EAZ(M_RX),HAX(M_RX),HAY(M_RX),HAZ(M_RX))
      ALLOCATE(CDB(NSUBCM,NBMAX))
!
!***********  Allocate and deallocate arrays GA, GRHF, GSB1 and GSB2 !!!!!!!
!
      RHOMAX=MAX(RMAX1,RMAX2) ! NHFILM=301;RHOMIN=0.1; RHOMAX=100000.; NHFILM=91
!
! --- Determine the number grid nodes in the rho-direction for the Hankel tables.
!     Note those values must strictly be determined by the sampling density
!     values of the filters used in routine HFILL
!
      NHFILM=INT(ALOG(RHOMAX)/(ALOG(10.)/15.))+15+5
      ALLOCATE(ZBG(2,NSRCS))
      ALLOCATE(RRG(NHFILM))
      ALLOCATE(RRG3(NHFILM))
      ALLOCATE(GRHF3(11,NHFILM,NZSR),GRHO03(4,NZSR))
!
! **** de- re- allocate!!
!
      ALLOCATE(GRHF(11,NHFILM,max(NZSR,nsrcs),max(NZOB,nobcs)),        &
               GRHO0(4,       max(NZSR,nsrcs),max(NZOB,nobcs)))
!
!
      NMAX=3*NSUBCM
      ALLOCATE(GA(NMAX,NMAX),EN(NMAX),EMT(NMAX),ENT(NMAX,NBODY),          &
               JST(NMAX,NBODY),EJGS(4*NMAX*NBODY),EJGS2(4*NMAX*NBODY),    &
               VV(NMAX),GAI(NMAX),IND(NMAX))
!  --- Take the maximal value of nsmr (=9*NXS*NYS*NZS*(NZS+1)/2) and
!      the size of the array for the Hankel tables
!         NSMR=MAX(11*NHFILM*NZSR*NZOB,NSMR)
! *** NSMR does not have to be linked to the array GRHF
      IF (KSMR==1) THEN
         ALLOCATE(GSB1(NSMR/3,3),GSB2(NSMR/3,3))
      ELSE
         NSMR=0
         ALLOCATE(GSB1(0,3),GSB2(0,3))  ! just to avoid error for some compilers!
      END IF
!
! ---- Open files to store the scattering matrices
!
!--- Files Green0 through 8 store the scattering matrices.
!
!    For CSAMT problems unit 31 through 38 store the block
!    matrices  computed by group theoretical diagonalization.
!    The original scattering matrix is block diagonalised into
!    four block matrices piece by piece.   Those four block
!    matrices are then further partitioned into submatrices
!    for the use of system iteration.   Units 32 through 35
!    store the diagonal submatrices of the four block matrices
!    obtained by group reduction. Units 31 through 34 store
!    the factored results.  Unit 35 through 38 store the
!    interaction submatrices, or the off-diagonal submatrices.
!    Note that unit 35 first store the diagonal submatrices
!    temporarily.  After factorization it's contents are move
!    to unit 34, and contents in units 34, 33,  and 32 are
!    move to units 33, 32, and 31.
!
!    Units 41 through 44 stores the four sets of transformed
!    incident fields used by the group reduction.  They
!    correspond to the incident fields for the four block
!    matrices
!    Units 45 through 48 stores the four sets of transformed
!    scattering currents
!
      IF (KSYMM==1.OR.NBODY>1) THEN
         OPEN (31,FILE='GREEN1',FORM='UNFORMATTED',STATUS='UNKNOWN')
         OPEN (32,FILE='GREEN2',FORM='UNFORMATTED',STATUS='UNKNOWN')
      END IF
      IF (KSYMM == 1) THEN
         OPEN (33,FILE='GREEN3',FORM='UNFORMATTED',STATUS='UNKNOWN')
         OPEN (34,FILE='GREEN4',FORM='UNFORMATTED',STATUS='UNKNOWN')
         OPEN (35,FILE='GREEN5',FORM='UNFORMATTED',STATUS='UNKNOWN')
         OPEN (36,FILE='GREEN6',FORM='UNFORMATTED',STATUS='UNKNOWN')
         OPEN (37,FILE='GREEN7',FORM='UNFORMATTED',STATUS='UNKNOWN')
         OPEN (38,FILE='GREEN8',FORM='UNFORMATTED',STATUS='UNKNOWN')
         OPEN (41,FILE='UEI1',FORM='UNFORMATTED',STATUS='UNKNOWN')
         OPEN (42,FILE='UEI2',FORM='UNFORMATTED',STATUS='UNKNOWN')
         OPEN (43,FILE='UEI3',FORM='UNFORMATTED',STATUS='UNKNOWN')
         OPEN (44,FILE='UEI4',FORM='UNFORMATTED',STATUS='UNKNOWN')
         OPEN (45,FILE='UJS1',FORM='UNFORMATTED',STATUS='UNKNOWN')
         OPEN (46,FILE='UJS2',FORM='UNFORMATTED',STATUS='UNKNOWN')
         OPEN (47,FILE='UJS3',FORM='UNFORMATTED',STATUS='UNKNOWN')
         OPEN (48,FILE='UJS4',FORM='UNFORMATTED',STATUS='UNKNOWN')
      END IF
!
! ---- File INDEX1 through INDEX4 in units 63 through 66 store the
!    array IND used in solution of the matrix equations
!
      OPEN (63,FILE='INDEX1',FORM='UNFORMATTED',STATUS='UNKNOWN')
      OPEN (64,FILE='INDEX2',FORM='UNFORMATTED',STATUS='UNKNOWN')
      OPEN (65,FILE='INDEX3',FORM='UNFORMATTED',STATUS='UNKNOWN')
      OPEN (66,FILE='INDEX4',FORM='UNFORMATTED',STATUS='UNKNOWN')
!
! ---- Eics and Jscs store the electric fields and scattering
!    current in the structure due to different excitation sites.
!    Note that for symmetric structures 41 through 44 instead of
!    10 will be used
!
      IF (KSYMM==0) OPEN (10,FILE='EICS',FORM='UNFORMATTED',STATUS='UNKNOWN')
      OPEN (20,FILE='JSCS',FORM='UNFORMATTED',STATUS='UNKNOWN')
!
!-- Units 11 and 12 store the values of the normal and scattered
!   electric and magnetic fields at each frequency to be used in printing
!   the frequency domain results in the file MARCOAIR.REF
!
!
! ------ Begin the frequency loop -------------------------------------
!
! Compute frequency domain solutions.
!
	Write (*, 9)
      Frequency_loop: DO IFRQ=1,NFRQ
!
         FRQ=FREQ(IFRQ)
         prc = 100.*(IFRQ-1.)/nfrq
!
         Call date_and_time(Values = tvals)
         Write (*, 10) tvals(3:1:-1), tvals(5:7), ifrq, nfrq, freq(ifrq), prc
!
!  -- For frequency domain, MD_ANGLE is frequency dependent. 
!  -- For time-domain, MD_ANGLE is station dependent. 
!     NRXST = either NSTAT (time domain) or NFRQ (frequency domain)

         DO J=1,NTX
           IF (TDFD < 2) MD_ANGLE(1,J) = TXCLN(J)
           IF (TDFD == 2) MD_ANGLE(1,J) = TXCLN(IFRQ)
           MD_ANGLE(2,J) = FANGLE(J)
         END DO
!
!---- Compute the complex conductivities of the earths and the cells.
!
         CALL INIT_CMPLX_CD_1D(FRQ,COLE_COLE,NLAYER,RES_EARTH,CHRG_EARTH, &
                               TAU_EARTH,FRQC_EARTH,REPSL,RMUL,CDH,KKH,RMU)
         CALL INIT_CMPLX_CD_3D(FRQ,COLE_COLE,NBMAX,NBODY,SUB_BLOCK,NCELL,       &
                               BLZ,ZBND,NLAYER,RMU,NSUBCM,CDB,RBC,CHRBC,        &
                               TAUBC,CFRBC,REPSB,RMUB)

! --- Skip the frequency if the target is deeper than 10 skin-depths

         ZTOP=100000.
         DO I=1,TRGT_BLCK
            IF (ZTOP>(BLCK_CZ(I)-.5*BLCK_LZ(I))) ZTOP=BLCK_CZ(I)-.5*BLCK_LZ(I)
         END DO
         CALL SKINS_3D (NLAYER,LRYTH,KKH,ZTOP,TOO_DEEP)
         IF (TOO_DEEP) CYCLE

!-- No outcrops in forming the scattering matrix

         KUTCRP=0

! ---- calculate the incident fields at cell centres

         CALL MAIN_PRM_AT_CELL (FRQ,MLAYER,ZBND,LRYTH,KKH,CDH,RMU,KSYMM,MBODY,   &
                               NBMAX,NSUBCM,NMAX,NXMAX,NYMAX,NZMAX,NTX_FD,MD_ANGLE,  &
                               NCRD,TX_CRDX,TX_CRDY,TX_CRDZ,TX_CRDXI,TX_CRDYI,       &
                               TX_CRDZI,NBODY,SUB_BLOCK,NET,NEQ,NX,NY,NZ,NCELL,      &
                               XCELL,YCELL,ZCELL,NXI,NYI,NZI,NCELLI,XCELLI,YCELLI,   &
                               ZCELLI,CDB,EN,EMT,ENT,EJGS,EJGS2,RHOMIN,RMAX1,NZOB,   &
                               ZOBG,NSRCS,ZSRCS,ZBG,AJ(IFRQ),DMIN,NHFILM,ALMAX,      &
                               BLMIN,GRHF,RRG,GRHO0)

         IF (KSMR==1) THEN

! ---- Compute the non-indentical elements of the scattering matrix
!    for use by spatial symmetry reductions.
!
!    Only the value of the clmn of one substructure is valid.
!
            CLMN2=CLMN(1)
!
            CALL MAIN_SUPER_GRID(GSB1,CLMN2,SBX,SBY,SBZ,NXS,NYS,       &
                                 NZS,XSB,YSB,ZSB,NSMR,FRQ,MLAYER,ZBND,       &
                                 LRYTH,KKH,CDH,RMU,NZOB,ZOBG,NZSR,ZSRG,  &
                                 DMIN,NHFILM,RHOMIN,RMAX2,RRG,RRG3,GRHF,     &
                                 GRHF3,GRHO0,GRHO03,ALMAX,KSFT,KCLMN,BLMIN,  &
                                 KACC,KUTCRP)
!
! -- Move the results from the array gsb1 to the array gsb2
!    since the array ga will be used later
!
            DO J=1,3
               DO I=1,3*NXS*NYS*NZS*(NZS+1)/2
                  GSB2(I,J)=GSB1(I,J)
               END DO
            END DO
!
         END IF
!
! --- Form the scattering matrices
!
         CALL MAIN_MATRICES(NLG,KSMR,CLMN,KSYMM,KCOND,GA,GSB2,IND,      &
                            VV,GAI,EN,CDB,MBODY,NBMAX,NXMAX,NYMAX,   &
                            NZMAX,NSUBCM,NBODY,SUB_BLOCK,NET,NX,NY,NZ,   &
                            NCELL,BLX,BLY,BLZ,XCELL,YCELL,ZCELL,KCELL,   &
                            NEQ,NMAX,NXI,NYI,NZI,NCELLI,BLXI,BLYI,       &
                            BLZI,XCELLI,YCELLI,ZCELLI,NXJ,NYJ,NZJ,       &
                            NCELLJ,XCELLJ,YCELLJ,ZCELLJ,SBX,SBY,SBZ,     &
                            NXS,NYS,NZS,ZSB,NSMR,FRQ,MLAYER,ZBND,        &
                            LRYTH,KKH,RMU,CDH,NZOB,ZOBG,NZSR,    &
                            ZSRG,DMIN,NHFILM,RHOMIN,RMAX2,RRG,RRG3,GRHF, &
                            GRHF3,GRHO0,GRHO03,ALMAX,KSFT,KCLMN,BLMIN,   &
                            KACC,KUTCRP)
!
! --- Solving the matrix equations for the scattering currents
!   in the 3D structure using the system iteration method
!
         CALL MAIN_SOLVER(NLG,GA,IND,EMT,EN,ENT,NET,JST,EJGS,EJGS2,       &
                          KACC,KSMR,KSYMM,NTX_FD,   &
                          MBODY,NBMAX,NXMAX,NYMAX,          &
                          NZMAX,NMAX,NBODY,SUB_BLOCK,NX,NY,NZ,NCELL,     &
                          NCT,KCELL,XCELL,YCELL,ZCELL,BLX,BLY,BLZ,       &
                          NXI,NYI,NZI,NCELLI,XCELLI,YCELLI,ZCELLI,       &
                          BLXI,BLYI,BLZI,NXJ,NYJ,NZJ,NCELLJ,XCELLJ,      &
                          YCELLJ,ZCELLJ,SBX,SBY,SBZ,NXS,NYS,NZS,ZSB,     &
                          NSMR,GSB2,NEQ)
!
! --- Compute scattered E and H fields at receiver sites.
!
         CALL MAIN_SCAT_EH_CS(KACC,KSYMM,MBODY,                         &
                              NBMAX,NBODY,SUB_BLOCK,NCELL,              &
                              NET,EMT,JST,EJGS,NMAX,NTX_FD,N_RX,        &
                              NHFILM,FRQ,MLAYER,ZBND,LRYTH,KKH,     &
                              BLMIN,DMIN,RHOMIN,RMAX1,NOBCS,            &
                              ZOBCS,NZSR,ZSRG,ALMAX,RRG,RRG3,GRHF,      &
                              GRHF3,GRHO0,GRHO03,RX_X,RX_Y,RX_Z,        &
                              NXMAX,NYMAX,NZMAX,NEQ,NX,NY,NZ,XCELL,     &
                              YCELL,ZCELL,BLX,BLY,BLZ,RMU,CDH,KSFT,     &
                              CLMN,M_RX,EAX,EAY,EAZ,                    &
                              HAX,HAY,HAZ,ESX,ESY,ESZ,HSX,HSY,HSZ)
!
!
!
!********** Start a new frequency loop ***********
!
         IF (NFRQ/=1) THEN
!
            IF (KSYMM==1.OR.NBODY>1) THEN
               REWIND (31)
               REWIND (32)
            END IF
            IF (KSYMM==1) THEN
               REWIND (33)
               REWIND (34)
               REWIND (35)
               REWIND (36)
               REWIND (37)
               REWIND (38)
               REWIND (41)
               REWIND (42)
               REWIND (43)
               REWIND (44)
               REWIND (45)
               REWIND (46)
               REWIND (47)
               REWIND (48)
            END IF
            IF (KSYMM==0) REWIND (10)
            REWIND (20)
            DO I=1,6
               REWIND (60+I)
            END DO
         END IF
!
!    Leroi and Marco have different coordinate systems.  The x and
!    y components are switched in the following.  Note the sign
!    reversals
!
!    The sequence of the do loops is kept identical to that
!    of Leroi.
!
!    Return BFD_SCAT in nanoteslas
!
         DO JS = 1,NTX_FD
           IF (TDFD < 2) THEN 
             BFD_SCAT(IFRQ,JS,1) = BFAC * HSX(1,JS)
             BFD_SCAT(IFRQ,JS,2) = BFAC * HSY(1,JS)
             BFD_SCAT(IFRQ,JS,3) = BFAC * HSZ(1,JS)
           ELSE IF (TDFD == 2) THEN
             BFD_SCAT(IFRQ,JS,1) = BFAC * HSX(IFRQ,JS)
             BFD_SCAT(IFRQ,JS,2) = BFAC * HSY(IFRQ,JS)
             BFD_SCAT(IFRQ,JS,3) = BFAC * HSZ(IFRQ,JS)

             CSF = COS (FANGLE(JS))
             SNF = SIN (FANGLE(JS))
             YLINE =  CSF * BFD_SCAT(IFRQ,JS,1) + SNF * BFD_SCAT(IFRQ,JS,2)
             YPERP =  CSF * BFD_SCAT(IFRQ,JS,2) - SNF * BFD_SCAT(IFRQ,JS,1)

!   NOTE The - sign below is a consequence of using the sine transform for a +iwt
!        sign convention.  It is thus consistant with the convention used for the
!        layered half space and in TDEM for time-domain scattered fields.
!        The check is that over a layer, the coplanar and coaxial response for a
!        Dighem configuration should be positive, at least at low frequencies.

              BFD_SCAT(IFRQ,JS,1) = -YLINE
              BFD_SCAT(IFRQ,JS,2) = -YPERP
              BFD_SCAT(IFRQ,JS,3) = -BFD_SCAT(IFRQ,JS,3)
           END IF
         END DO

      END DO Frequency_loop
!
! --- Close units 10, 20,  41 through 48 and units 61 through 66
!   in order to reduce the total number of files being opened.
!   The units 31 through 38 storing the scattering matrix may
!   be too large to be closed during the executation and thus
!   will not closed during the executaion.
!
!  !! Unit 31 through 38 must be closed since the total number
!     of files is too large for some computers like Silicon
!     Graphics workstations
!
      IF (KSYMM==0) CLOSE (10,STATUS='DELETE')
      CLOSE (20,STATUS='DELETE')
      DO I=1,6
         CLOSE (UNIT=60+I,STATUS='DELETE')
      END DO
      IF (KSYMM==1) THEN
         DO I=1,8
            CLOSE (UNIT=40+I,STATUS='DELETE')
         END DO
      END IF
      DO I=1,8
         CLOSE (UNIT=30+I,STATUS='DELETE')
      END DO
!
! ----- Print out results -----
!
! --- Rewind files storing the field components
!
!
      DEALLOCATE(GA,ENT,JST,EJGS,EJGS2,GSB1,GSB2)
      DEALLOCATE(ESX,ESY,ESZ,HSX,HSY,HSZ)
      ALLOCATE(ESX(NFRQ,M_RX),ESY(NFRQ,M_RX),ESZ(NFRQ,M_RX),          &
               HSX(NFRQ,M_RX),HSY(NFRQ,M_RX),HSZ(NFRQ,M_RX))
!
!  --- Deallocate all the arrays before leaving this routine to avoid problems
!      with some compilers
!
      DEALLOCATE (AJ,CDH,KKH,ZBND,N_RX,RX_X,RX_Y,RX_Z,TX_CRDX,      &
                  TX_CRDY,TX_CRDZ,MD_ANGLE,TX_CRDXI,TX_CRDYI,TX_CRDZI,      &
                  RRG,RRG3,GRHF,GRHO0,GRHF3,GRHO03,ZBG,                     &
                  ESX,ESY,ESZ,HSX,HSY,HSZ,CDB,EAX,EAY,                      &
                  EAZ,HAX,HAY,HAZ)
!  -- Some of the arrays have been deallocated
      DEALLOCATE(EN,EMT,VV,GAI,IND)
      DEALLOCATE(XCELL,YCELL,ZCELL,BLXI,BLYI,BLZI,XCELLI,YCELLI,        &
                 ZCELLI,XCELLJ,YCELLJ,ZCELLJ,X1,Y1,Z1,NXI,NYI,NZI,      &
                 NXJ,NYJ,NZJ,NCELLI,NCELLJ,NET,KCELL,CLMN)
!   -- These are allocated in routine STRUCTURING
      DEALLOCATE(NX,NY,NZ,NCT,NCELL,BLX,BLY,BLZ,XBL,YBL,ZBL,RBC,        &
                    CHRBC,TAUBC,CFRBC,RMUB,REPSB,SUB_BLOCK)
 1030 FORMAT (/T3,'It is suggested to use spatial symmetry reduction to compute approximate' &
              /T3,'solutions by discretizing the body into equal size cells in order to'    &
              /T3,'achieve maximal efficiency!')
 9  Format (/, 2x, 6x, 'Date', 2x, 4x, 'Time', 16x, 'Frequency', 8x, '%')
 10 Format (2x, i2.2, '/', i2.2, '/', i4.4, 2x, i2.2, ':', i2.2, ':', i2.2, 2x, i2, ' of ', i2, ' = ', en12.3, ' ', f8.2)
!  END subroutine MARCOAIR_3D
!
   CONTAINS
!
      SUBROUTINE TX_RX_CONVERTER
!
      INTEGER :: II
!
! --- Transmitter parameters
!
      NTX_FD=NTX
      NCRD=1
!
      ALLOCATE(TX_CRDX(NCRD,NTX_FD),TX_CRDY(NCRD,NTX_FD),               &
               TX_CRDZ(NCRD,NTX_FD),MD_ANGLE(2,NTX_FD))
!
      DO J=1,NTX
         TX_CRDX(1,J)=SX(J)
         TX_CRDY(1,J)=SY(J)
!  -- SZ is altitude (positive) instead of coordinate
         TX_CRDZ(1,J)=-SZ(J)
      END DO
!
! --- Receiver parameters
!
      ALLOCATE(N_RX(NTX_FD))
      N_RX=NRX
!
      M_RX=MAXVAL(N_RX)
      ALLOCATE(RX_X(M_RX,NTX_FD),RX_Y(M_RX,NTX_FD),RX_Z(M_RX,NTX_FD))
!
!  -- Note that the index order of main program parameters RX, RY, and RZ
!     are different from the Marco parameters RX_X etc.
!
      DO II=1,NTX
         DO I=1,N_RX(II)
            RX_X(I,II)=RX(II,I)
            RX_Y(I,II)=RY(II,I)
!  -- RZ is altitude (positive) instead of coordinate
            RX_Z(I,II)=-RZ(II,I)
         END DO
      END DO
!
      END SUBROUTINE TX_RX_CONVERTER
!
      SUBROUTINE STRUCTURING
!
!**** Divide block(s) into substructures
!
!   Input blocks are divided into substructures for the use of the
!   system iteration.  Blocks are halved in the direction of maximal
!   cell numbers until the total number of cells within each
!   substructure is reduced to a quarter of NSUBCM.
!
!Input parameters:
!
!  NSUBCM:  Integer(*4),  maximal number of cells in a substructure.
!  MBODY:   Integer,  maximal number of substructures allowed in the
!           program.
!  NBMAX:   Integer,  maximal number of blocks allowed in a
!           substructure.  It is the maximum of SUB_BLOCK used for
!           array definitions.
!  TRGT_BLCK: Integer,  number of input blocks to be divided into
!           substructures.
!  BLCK_NX:    Integer BLCK_NX(TRGT_BLCK),  number of cells in the x-direction
!           of blocks.
!  BLCK_NY:    Integer BLCK_NY(TRGT_BLCK),  number of cells in the y-direction
!           of blocks.
!  BLCK_NZ:    Integer BLCK_NZ(TRGT_BLCK),  number of cells in the z-direction
!           of blocks.
!  BLCK_LX:    Real BLCK_LX(TRGT_BLCK),  dimensions of the blocks in the
!           x-direction.
!  BLCK_LY:    Real BLCK_LY(TRGT_BLCK),  dimensions of the blocks in the
!           y-direction.
!  BLCK_LZ:    Real BLCK_LZ(TRGT_BLCK),  dimensions of the blocks in the
!           z-direction.
!  BLCK_CX:    Real BLCK_CX(TRGT_BLCK),  x-coordinates of the block centres.
!  BLCK_CY:    Real BLCK_CY(TRGT_BLCK),  y-coordinates of the block centres.
!  BLCK_CZ:    Real BLCK_CZ(TRGT_BLCK),  z-coordinates of the block centres.
!  RSB:     Real RSB(TRGT_BLCK),  resistivities of the blocks.
!  CHRSB:   Real CHRSB(TRGT_BLCK),  chargeability of the blocks (if it
!           is polarisable).  Cole-Cole models are used to presents
!           polarisable models.
!  TAUSB:   Real TAUSB(TRGT_BLCK),  time constants of the blocks.
!  CFRSB:   Real CFRSB(TRGT_BLCK),  frequency constants of the blocks.
!  REPSP:   Real REPSP(TRGT_BLCK), relative dielectric constants of the blocks.
!  RMUP:    Real RMUP(TRGT_BLCK), relative permeability of the blocks.
!  KACC:    Integer,  accuracy level for the computation of the scattered
!           fields.  See documentation file "MARCO.DOC" for detail.
!  COLE_COLE:     Integer,  parameter controlling whether the model is polarisable.
!           COLE_COLE = 0:  non-polarisable;  and
!               = 1:  polarisable.
!
!
!Output parameters:
!
!  NBODY:   Integer,  number of substructures.
!  SUB_BLOCK:  Integer SUB_BLOCK(MBODY),  number of blocks in each substructure.
!  NCT:     Integer NCT(MBODY),  number of cells in each substructure.
!  NCELL:   Integer NCELL(NBMAX,MBODY),  number of cells in each block
!           of the substructures.
!  NX:      Integer NX(NBMAX,MBODY),  numbers of cells in the x-direction
!           in each block of the substructures.
!  NY:      Integer NY(NBMAX,MBODY),  numbers of cells in the y-direction
!           in each block of the substructures.
!  NZ:      Integer NZ(NBMAX,MBODY),  numbers of cells in the z-direction
!           in each block of the substructures.
!  BLX:     Real BLX(NBMAX,MBODY),  dimensions of each blocks of the
!           substructures in the x-direction.
!  BLY:     Real BLY(NBMAX,MBODY),  dimensions of each blocks of the
!           substructures in the y-direction.
!  BLZ:     Real BLZ(NBMAX,MBODY),  dimensions of each blocks of the
!           substructures in the z-direction.
!  XBL:     Real XBL(NBMAX,MBODY),  x-coordinates of each blocks in the
!           substructures.
!  YBL:     Real YBL(NBMAX,MBODY),  y-coordinates of each blocks in the
!           substructures.
!  ZBL:     Real ZBL(NBMAX,MBODY),  z-coordinates of each blocks in the
!           substructures.
!  RBC:     Real RBC(NBMAX,MBODY),  resistivities of each blocks of the
!           substructures.
!  CHRBC:   Real CHRBC(NBMAX,MBODY),  chargeabilities of each blocks of
!           the substructures.
!  TAUBC:   Real TAUBC(NBMAX,MBODY),  time constants of each blocks of
!           the substructures.
!  CFRBC:   Real CFRBC(NBMAX,MBODY),  frequency constants of each blocks of
!           the substructures.
!  REPSB:   Real REPSB(NBMAX,NBODY), relative dielectric constants for the blocks of the substructures.
!  RMUB:    Real RMUB(NBMAX,NBODY), relative permeability for the blocks of the substructures.
!
!Note that the word "block" means differently for the input and output.
!In the input it is the blocks that are read from the data input file
!"MARCO.DAT".  In the output it is the blocks for computation purpose.
!
!
      IMPLICIT NONE
!
      INTEGER I,IB,II,IMAX,PARTITION,N1,N2,NCLIM,NCMAX,NM,NBODY1,NCTT3
!
!  -- Determine the total number of cells
!
      NCTT=0
      DO II=1,TRGT_BLCK
         NCTT=NCTT+BLCK_NX(II)*BLCK_NY(II)*BLCK_NZ(II)
      END DO
      NCTT3 = INT(8*(3*NCTT)**2 / 1000000)

!  -- Determine the value of SUB_CELL_MAX according to SOLVER

      IF (SOLVER==1) THEN
         SUB_CELL_MAX=NCTT
         ! IF (NCTT > 1000) THEN
         !    WRITE(*,1) NCTT3
         !    WRITE(NLG,1) NCTT3
         ! END IF
      ELSE
!  -- For the iterative solver use a maximum of 300 cells
         SUB_CELL_MAX=MIN(NCTT/2,300)
      END IF
!
!
! ======== DIVIDE THE STRUCTURE BLOCKS INTO SUBSTRUCTURES =======
!
!-- If the total number of the substructure cells does not exceed the
!   designed limits or if accurate solution is needed
!
!*** This criterion may be changed !!
!
      IF (NCTT<=SUB_CELL_MAX.AND.TRGT_BLCK<=NBMAX.AND.KACC>1) THEN
!********************************************************************
!*                                                                  *
         NBODY=1
!
         ALLOCATE(BLX(TRGT_BLCK,1),BLY(TRGT_BLCK,1),BLZ(TRGT_BLCK,1),      &
                  XBL(TRGT_BLCK,1),YBL(TRGT_BLCK,1),ZBL(TRGT_BLCK,1),      &
                  NX(TRGT_BLCK,1),NY(TRGT_BLCK,1),NZ(TRGT_BLCK,1),         &
                  NCT(1),NCELL(TRGT_BLCK,1),                               &
                  RBC(TRGT_BLCK,1),CHRBC(TRGT_BLCK,1),                     &
                  TAUBC(TRGT_BLCK,1),CFRBC(TRGT_BLCK,1),                   &
                  REPSB(TRGT_BLCK,1),RMUB(TRGT_BLCK,1),                   &
                  SUB_BLOCK(1))
!
         SUB_BLOCK(1)=TRGT_BLCK
         NCT(1)=0
         DO I=1,SUB_BLOCK(1)
            BLX(I,1)=BLCK_LX(I)
            BLY(I,1)=BLCK_LY(I)
            BLZ(I,1)=BLCK_LZ(I)
            XBL(I,1)=BLCK_CX(I)
            YBL(I,1)=BLCK_CY(I)
            ZBL(I,1)=BLCK_CZ(I)
            NX(I,1)=BLCK_NX(I)
            NY(I,1)=BLCK_NY(I)
            NZ(I,1)=BLCK_NZ(I)
            NCELL(I,1)=NX(I,1)*NY(I,1)*NZ(I,1)
            NCT(1)=NCT(1)+NCELL(I,1)
            RBC(I,1)=RSB(I)
            IF (COLE_COLE==1) THEN
               CHRBC(I,1)=CHRSB(I)
               TAUBC(I,1)=TAUSB(I)
               CFRBC(I,1)=CFRSB(I)
               REPSB(I,1)=REPSP(I)
            END IF
            REPSB(I,1)=REPSP(I)
            RMUB(I,1) =RMUP(I)
         END DO
!*                                                                  *
!********************************************************************
      ELSE
!********************************************************************
!*                                                                  *
!
! -- Determine the number of substructures according to number of
!    cells in the substructures
!
!    NBODY1 is used as a working parameter that is the maximal number of
!    substructures for the fast solver with KACC=1
!
         NBODY1=MAX(NCTT/10+1,TRGT_BLCK)
         ALLOCATE(NX(1,NBODY1),NY(1,NBODY1),NZ(1,NBODY1),NCT(NBODY1),   &
                  NCELL(1,NBODY1))
!
         NBODY=TRGT_BLCK
!
         DO IB=1,NBODY
            NX(1,IB)=BLCK_NX(IB)
            NY(1,IB)=BLCK_NY(IB)
            NZ(1,IB)=BLCK_NZ(IB)
         END DO
!
! --- Break the block that has the maximal number of cells in the
!     direction of maximal cell numbers.
!
         DO PARTITION=1,100000
!
            DO IB=1,NBODY
               NCT(IB)=0
               NCELL(1,IB)=NX(1,IB)*NY(1,IB)*NZ(1,IB)
               NCT(IB)=NCT(IB)+NCELL(1,IB)
            END DO
!
            NCMAX=0
            DO IB=1,NBODY
               IF (NCMAX<NCT(IB)) THEN
                  NCMAX=NCT(IB)
                  IMAX=IB
               END IF
            END DO
!
!--- No further division necessary if the maximal number of
!    cells in a substructure is less than SUB_CELL_MAX/8 or if
!    the maximal number of substructures is reached
!
!    In case an approximate solution is sought,  the body
!    is divided into maximal number of substructures defined by NCLIM
!
            IF (KACC==1) THEN
               NCLIM=10
            ELSE
               NCLIM=SUB_CELL_MAX/8
            END IF
!
!      --   NBODY1 is the maximum for the number of substructures
            IF (NCMAX<=NCLIM.OR.NBODY==NBODY1) THEN
               EXIT
            END IF
!
! -- Increase the number of substructures by 1
!
            NBODY=NBODY+1
!
! -- Move the blocks
!
            DO IB=NBODY,IMAX+2,-1
               NX(1,IB)=NX(1,IB-1)
               NY(1,IB)=NY(1,IB-1)
               NZ(1,IB)=NZ(1,IB-1)
            END DO
!
! -- divide the block imax into two
!
            NM=NX(1,IMAX)
            II=1
            IF (NM<NY(1,IMAX)) THEN
               NM=NY(1,IMAX)
               II=2
            END IF
            IF (NM<NZ(1,IMAX)) THEN
               NM=NZ(1,IMAX)
               II=3
            END IF
            N1=NM/2
            N2=NM-N1
!
            IF (II==1) THEN
               NX(1,IMAX+1)=N2
               NY(1,IMAX+1)=NY(1,IMAX)
               NZ(1,IMAX+1)=NZ(1,IMAX)
               NX(1,IMAX)=N1
            END IF
!
            IF (II==2) THEN
               NX(1,IMAX+1)=NX(1,IMAX)
               NY(1,IMAX+1)=N2
               NZ(1,IMAX+1)=NZ(1,IMAX)
               NY(1,IMAX)=N1
            END IF
!
            IF (II==3) THEN
               NX(1,IMAX+1)=NX(1,IMAX)
               NY(1,IMAX+1)=NY(1,IMAX)
               NZ(1,IMAX+1)=N2
               NZ(1,IMAX)=N1
            END IF
!
         END DO
!
! --- Now that the exact number of substructures is determined, reallocate
!     the arrays NX etc. and complete the restructuring.
!
         DEALLOCATE(NX,NY,NZ,NCT,NCELL)
         ALLOCATE(NX(1,NBODY),NY(1,NBODY),NZ(1,NBODY),NCT(NBODY),         &
                  NCELL(1,NBODY),BLX(1,NBODY),BLY(1,NBODY),BLZ(1,NBODY),  &
                  XBL(1,NBODY),YBL(1,NBODY),ZBL(1,NBODY),                 &
                  RBC(1,NBODY),CHRBC(1,NBODY),                            &
                  TAUBC(1,NBODY),CFRBC(1,NBODY),                          &
                  REPSB(1,NBODY),RMUB(1,NBODY),                           &
                  SUB_BLOCK(NBODY))
         NBODY1=NBODY
!
! --  Now divide the blocks into substructures as determined above.
!     Note that the following rules are identical the above.
!
         NBODY=TRGT_BLCK
!
! -- SUB_BLOCK for all substructures set to be one.
!
         DO IB=1,NBODY1
            SUB_BLOCK(IB)=1
         END DO
!
         NCT=0
         DO IB=1,NBODY
            BLX(1,IB)=BLCK_LX(IB)
            BLY(1,IB)=BLCK_LY(IB)
            BLZ(1,IB)=BLCK_LZ(IB)
            XBL(1,IB)=BLCK_CX(IB)
            YBL(1,IB)=BLCK_CY(IB)
            ZBL(1,IB)=BLCK_CZ(IB)
            NX(1,IB)=BLCK_NX(IB)
            NY(1,IB)=BLCK_NY(IB)
            NZ(1,IB)=BLCK_NZ(IB)
            NCELL(1,IB)=NX(1,IB)*NY(1,IB)*NZ(1,IB)
            NCT(IB)=NCT(IB)+NCELL(1,IB)
            RBC(1,IB)=RSB(IB)
            IF (COLE_COLE==1) THEN
               CHRBC(1,IB)=CHRSB(IB)
               TAUBC(1,IB)=TAUSB(IB)
               CFRBC(1,IB)=CFRSB(IB)
            END IF
            REPSB(1,IB)=REPSP(IB)
            RMUB(1,IB)=RMUP(IB)
         END DO
!
! --- Break the block that has the maximal number of cells in the
!     direction of maximal cell numbers.
!
         DO PARTITION=1,100000
!
            DO IB=1,NBODY
               NCT(IB)=0
               NCELL(1,IB)=NX(1,IB)*NY(1,IB)*NZ(1,IB)
               NCT(IB)=NCT(IB)+NCELL(1,IB)
            END DO
!
            NCMAX=0
            DO IB=1,NBODY
               IF (NCMAX<NCT(IB)) THEN
                  NCMAX=NCT(IB)
                  IMAX=IB
               END IF
            END DO
!
!--- No further division necessary if the maximal number of
!    cells in a substructure is less than SUB_CELL_MAX/8 or if
!    the maximal number of substructures is reached
!
!    In case an approximate solution is sought,  the body
!    is divided into maximal number of substructures defined by NCLIM
!
            IF (KACC==1) THEN
               NCLIM=10
            ELSE
               NCLIM=SUB_CELL_MAX/8
            END IF
!
            IF (NCMAX<=NCLIM.OR.NBODY==NBODY1) THEN
               EXIT
            END IF
!
! -- Increase the number of substructures by 1
!
            NBODY=NBODY+1
!
! -- Move the blocks
!
            DO IB=NBODY,IMAX+2,-1
               BLX(1,IB)=BLX(1,IB-1)
               BLY(1,IB)=BLY(1,IB-1)
               BLZ(1,IB)=BLZ(1,IB-1)
               XBL(1,IB)=XBL(1,IB-1)
               YBL(1,IB)=YBL(1,IB-1)
               ZBL(1,IB)=ZBL(1,IB-1)
               NX(1,IB)=NX(1,IB-1)
               NY(1,IB)=NY(1,IB-1)
               NZ(1,IB)=NZ(1,IB-1)
               RBC(1,IB)=RBC(1,IB-1)
               IF (COLE_COLE==1) THEN
                  CHRBC(1,IB)=CHRBC(1,IB-1)
                  TAUBC(1,IB)=TAUBC(1,IB-1)
                  CFRBC(1,IB)=CFRBC(1,IB-1)
               END IF
               REPSB(1,IB)=REPSB(1,IB-1)
               RMUB(1,IB)=RMUB(1,IB-1)
            END DO
!
! -- divide the block imax into two
!
            NM=NX(1,IMAX)
            II=1
            IF (NM<NY(1,IMAX)) THEN
               NM=NY(1,IMAX)
               II=2
            END IF
            IF (NM<NZ(1,IMAX)) THEN
               NM=NZ(1,IMAX)
               II=3
            END IF
            N1=NM/2
            N2=NM-N1
!
            IF (II==1) THEN
               BLX(1,IMAX+1)=BLX(1,IMAX)*N2/NM
               BLY(1,IMAX+1)=BLY(1,IMAX)
               BLZ(1,IMAX+1)=BLZ(1,IMAX)
               XBL(1,IMAX+1)=XBL(1,IMAX)-.5*BLX(1,IMAX)+.5*BLX(1,IMAX+1)
               YBL(1,IMAX+1)=YBL(1,IMAX)
               ZBL(1,IMAX+1)=ZBL(1,IMAX)
               NX(1,IMAX+1)=N2
               NY(1,IMAX+1)=NY(1,IMAX)
               NZ(1,IMAX+1)=NZ(1,IMAX)
               BLX(1,IMAX)=BLX(1,IMAX)*N1/NM
               XBL(1,IMAX)=XBL(1,IMAX+1)+.5*(BLX(1,IMAX)+BLX(1,IMAX+1))
               NX(1,IMAX)=N1
            END IF
!
            IF (II==2) THEN
               BLX(1,IMAX+1)=BLX(1,IMAX)
               BLY(1,IMAX+1)=BLY(1,IMAX)*N2/NM
               BLZ(1,IMAX+1)=BLZ(1,IMAX)
               XBL(1,IMAX+1)=XBL(1,IMAX)
               YBL(1,IMAX+1)=YBL(1,IMAX)-.5*BLY(1,IMAX)+.5*BLY(1,IMAX+1)
               ZBL(1,IMAX+1)=ZBL(1,IMAX)
               NX(1,IMAX+1)=NX(1,IMAX)
               NY(1,IMAX+1)=N2
               NZ(1,IMAX+1)=NZ(1,IMAX)
               BLY(1,IMAX)=BLY(1,IMAX)*N1/NM
               YBL(1,IMAX)=YBL(1,IMAX+1)+.5*(BLY(1,IMAX)+BLY(1,IMAX+1))
               NY(1,IMAX)=N1
            END IF
!
            IF (II==3) THEN
               BLX(1,IMAX+1)=BLX(1,IMAX)
               BLY(1,IMAX+1)=BLY(1,IMAX)
               BLZ(1,IMAX+1)=BLZ(1,IMAX)*N2/NM
               XBL(1,IMAX+1)=XBL(1,IMAX)
               YBL(1,IMAX+1)=YBL(1,IMAX)
               ZBL(1,IMAX+1)=ZBL(1,IMAX)-.5*BLZ(1,IMAX)+.5*BLZ(1,IMAX+1)
               NX(1,IMAX+1)=NX(1,IMAX)
               NY(1,IMAX+1)=NY(1,IMAX)
               NZ(1,IMAX+1)=N2
               BLZ(1,IMAX)=BLZ(1,IMAX)*N1/NM
               ZBL(1,IMAX)=ZBL(1,IMAX+1)+.5*(BLZ(1,IMAX)+BLZ(1,IMAX+1))
               NZ(1,IMAX)=N1
            END IF
!
            RBC(1,IMAX+1)=RBC(1,IMAX)
            IF (COLE_COLE==1) THEN
               CHRBC(1,IMAX+1)=CHRBC(1,IMAX)
               TAUBC(1,IMAX+1)=TAUBC(1,IMAX)
               CFRBC(1,IMAX+1)=CFRBC(1,IMAX)
            END IF
            REPSB(1,IMAX+1)=REPSB(1,IMAX)
            RMUB(1,IMAX+1)=RMUB(1,IMAX)

         END DO

! -- Check is NBODY = NBODY1.  They must be identical, or something must
!    be wrong in determining NBODY and/or NBODY1.

         IF (NBODY1/=NBODY) THEN
            WRITE(NLG,2)
            STOP
         END IF

!********************************************************************
      END IF

! --- Substructure information can be printed out as in program Sysem
!     if necessary.  Check file sysem.f, line 1594 ff.

  1 FORMAT(/T3,'WARNING: You need at least',I6,' megabytes of memory to run this job'/)
  2 FORMAT(/T3,'Error in routine STRUCTURING: NBODY1 /= NBODY.' &
           /T3,'Program aborted.  Check prism input in file MarcoAir.LOG'/)

      END SUBROUTINE STRUCTURING

   END SUBROUTINE MARCO_3D

!
 SUBROUTINE INIT_CMPLX_CD_1D (FRQ,COLE_COLE,NLAYER,RES_EARTH,CHRG_EARTH,TAU_EARTH, &
                              FRQC_EARTH,REPSL,RMUL,CDH,KKH,RMU)
!---------------------------------------------------------------------------------

!**** Compute the complex conductivities of the earths and the cells
!
! Input parameters:
!
!  REPSL:  Real REPSL(NLAYER), relative dielectric constants of the layers.
!
!  RMUL:   Real RMUL(NLAYER), relative permeability of the layers.
!
! Output parameters:
!
!  CDH:    Complex CDH(0:NLAYER),  complex conductivities of
!          the layers in the lateral direction including the
!          air (layer 0).
!  KKH:    Complex(*8) KKH(0:NLAYER),  square ( **2) of the complex wave
!          number of the layers (in the lateral direction) including
!          the air (layer number 0).
!  RMU:    Real RMU(0:NLAYER), relative permeability of the layers including the air
!
   IMPLICIT NONE
!
   REAL, PARAMETER :: TOL=1.E-6, PI=3.141592654, EPSL0=8.854156E-12
   COMPLEX, PARAMETER :: ONE = (1.,0.)
   REAL OMEGA,FRQ,MU0
   INTEGER I,COLE_COLE,NLAYER
   COMPLEX SIG,P,CALF,WMU,CDH(0:NLAYER),KKH(0:NLAYER)
   REAL RES_EARTH(NLAYER),CHRG_EARTH(NLAYER),TAU_EARTH(NLAYER),                       &
        FRQC_EARTH(NLAYER),REPSL(NLAYER),RMUL(NLAYER),RMU(0:NLAYER)
!
   OMEGA=2.*PI*FRQ
   MU0=PI*4.E-7
   WMU=CMPLX(0.,MU0*OMEGA)
   CDH(0)=CMPLX(0.,EPSL0*OMEGA)
!
   DO I=1,NLAYER
!
      SIG=CMPLX(1./RES_EARTH(I),0.)
      CDH(I)=SIG+REPSL(I)*CDH(0)
!
      IF (COLE_COLE == 1 .AND. TAU_EARTH(I) > TOL .AND. FRQC_EARTH(I) > TOL) THEN
!
!  Add Cole-Cole models to the conductivities and permittivities
!
         P=CMPLX(0.,OMEGA*TAU_EARTH(I))**FRQC_EARTH(I)
         CALF=CMPLX((1.-CHRG_EARTH(I)),0.)
         CDH(I)=SIG*(ONE+P)/(ONE+CALF*P)+REPSL(I)*CDH(0)
      END IF
!
   END DO
!
   KKH(0)=WMU*CDH(0)
   RMU(0)=1.
   DO I=1,NLAYER
      KKH(I)=WMU*CDH(I)*RMUL(I)
      RMU(I)=RMUL(I)
   END DO
!
   END SUBROUTINE INIT_CMPLX_CD_1D
!
!
 SUBROUTINE INIT_CMPLX_CD_3D (FRQ,COLE_COLE,NBMAX,NBODY,SUB_BLOCK,NCELL,BLZ,ZBND,       &
                              NLAYER,RMU,NSUBCM,CDB,RBC,CHRBC,TAUBC,CFRBC,REPSB,RMUB)
!-----------------------------------------------------------------------------------
!**** Compute the complex conductivities of the earths and the cells
!
!   Note that units 61 and 62 are used to read and store cells
!   conductivity parameters.   The reading sequence must be consistent
!   with the writing sequence in routine READATA.
!   No need ot check outcrop since this is fixed in READ3D
!
!Input parameters:
!
!  FRQ:    Real,  the frequency.
!  COLE_COLE:    Integer,  parameter controlling whether the model (including
!          the earth) is polarizable.  If so,  Cole-Cole models are used
!          to represent the complex resistivities of the model.
!  NBMAX:   Integer,  maximal number of blocks allowed in a
!           substructure.  It is the maximum of SUB_BLOCK used for
!           array definitions.
!  NBODY:   Integer,  number of substructures.
!  SUB_BLOCK:  Integer SUB_BLOCK(NBODY),  number of blocks in each substructure.
!  NCELL:   Integer NCELL(NBMAX,NBODY),  number of cells in each block
!           of the substructures.
!  NSUBCM:  Integer,  maximal number of cells in a substructure.
!  CDB:    COMPLEX CDB(NSUBCM,NBMAX),  working array for the
!          complex conductivities of the cells in a substructure.
!          Its values are all stored on the disk in unit 62
!  RBC:     Real RBC(NBMAX,NBODY),  resistivities of each blocks of the
!           substructures.
!  CHRBC:   Real CHRBC(NBMAX,NBODY),  chargeabilities of each blocks of
!           the substructures.
!  TAUBC:   Real TAUBC(NBMAX,NBODY),  time constants of each blocks of
!           the substructures.
!  CFRBC:   Real CFRBC(NBMAX,NBODY),  frequency constants of each blocks of
!           the substructures.
!
!  REPSB:   Real REPSB(NBMAX,NBODY), relative dielectric constants of the blocks.
!
!  RMUB:    Real RMUB(NBMAX,NBODY), relative permeability of the blocks.
!
!  ZBND:    layer boundaries
!  BLZ:     block centre cordinates in z.   NOTE that a block must be restricted within
!           a single layer.  Otherwise cell z-coordinated must be used in this routine
!  RMU:     layer permeability
!
! Output parameters:
!
!  Complex conductivities of the 3D cells are stored in unit
!  62 on the disk.
!
!**** Called by:  main
!**** Calls:      none

 IMPLICIT NONE

 REAL, PARAMETER :: TOL=1.E-6, PI=3.141592654, EPSL0=8.854156E-12
 COMPLEX, PARAMETER :: ONE = (1.,0.)
 INTEGER I,IB,J,COLE_COLE,NBMAX,NBODY,NSUBCM,NOB,IOB,NLAYER
 COMPLEX CDB(NSUBCM,NBMAX)
 INTEGER SUB_BLOCK(NBODY),NCELL(NBMAX,NBODY)
 REAL RBC(NBMAX,NBODY),CHRBC(NBMAX,NBODY),TAUBC(NBMAX,NBODY),           &
      CFRBC(NBMAX,NBODY),REPSB(NBMAX,NBODY),RMUB(NBMAX,NBODY),          &
      BLZ(NBMAX,NBODY),ZBND(0:NLAYER),RMU(0:NLAYER)

 REAL FRQ,OMEGA
 COMPLEX P,SIG,CALF

!  Assign 1D conductivities and add displacement current term to the air

 OMEGA=2.*PI*FRQ

 DO IB=1,NBODY
   DO J=1,SUB_BLOCK(IB)
     SIG=CMPLX(1./RBC(J,IB),0.)
     IF (COLE_COLE == 1 .AND. TAUBC(J,IB) > TOL .AND.  &
!  Add Cole-Cole models to the conductivities and permittivities
       CFRBC(J,IB) > TOL) THEN
       P = CMPLX (0., OMEGA * TAUBC(J,IB))**CFRBC(J,IB)
       CALF = CMPLX ((1. - CHRBC(J,IB)), 0.)
       SIG = SIG * (ONE + P)/ (ONE + CALF*P)
     END IF

!  Store the cell conductivity in array CDB for writing into file CONDT (unit 62)

     DO I=1,NCELL(J,IB)
       CDB(I,J)=SIG+CMPLX(0.,REPSB(J,IB)*EPSL0*OMEGA)
     END DO
   END DO
!
!  Multiply relative permeabilities normalised by those of the layers.
! *** Block centre coordinate is used instead of cell centres!  This simplifies the code
!     at the cost of restricting the blocks to be within every single layer.

   NOB=0
   DO J=1,SUB_BLOCK(IB)
     LP1: DO I=1,NCELL(J,IB)
       DO IOB=NLAYER-1,0,-1
         IF (BLZ(J,IB) >= ZBND(IOB)) THEN
           NOB=IOB+1
           CDB(I,J)=CDB(I,J)*RMUB(J,IB)/RMU(NOB)
           CYCLE LP1
         END IF
       END DO
     END DO LP1
   END DO
! --- Store the cell conductivity to file CONDT (unit 62)

   WRITE (62) ((CDB(I,J),I=1,NCELL(J,IB)),J=1,SUB_BLOCK(IB))

 END DO

 REWIND (61)
 REWIND (62)

 END SUBROUTINE INIT_CMPLX_CD_3D


!===== Subroutines for intializations
!
 SUBROUTINE INIT_COMPUTATION (DMIN,MBODY,NBMAX,NXMAX,NYMAX,NZMAX,MLAYER,ZBND,  &
                              LRYTH,KSYMM,NBODY,SUB_BLOCK,NX,NY,NZ,BLX,BLY,    &
                              BLZ,XBL,YBL,ZBL,X1,Y1,Z1,NCT,NET,NCELL,NCTT,NEQ, &
                              XCELL,YCELL,ZCELL,KCELL)
!------------------------------------------------------------------------------
!**** Initial computations
!
!   Set input parameters to those suitable for computation,
!   discretise the blocks into cell,  etc.
!
!   Note that after this routine is executed,  the arrays BLX,  BLY,
!   and BLZ become the dimensions of the cells
!
!
! Input parameters:
!
!  DMIN:    Real,  minimal dimension value that is considered to be
!           zero.  DMIN=0.1 for the low frequency module and DMIN=0.001
!           for the high frequency module.
!  MBODY:   Integer(*4),  maximal number of substructures allowed in
!           the program.
!  NBMAX:   Integer,  maximal number of blocks allowed in a
!           substructure.  It is the maximum of SUB_BLOCK used for
!           array definitions.
!  NXMAX:   Integer,  maximal number of cells in the x-direction
!           in a block.
!  NYMAX:   Integer,  maximal number of cells in the y-direction
!           in a block.
!  NZMAX:   Integer,  maximal number of cells in the z-direction
!           in a block.
!  ZBND:    Real ZBND(0:MLAYER), the z-coordinates of the layer
!           boundaries with the air-earth interface being always
!           0 (zbnd(0)=0).
!  LRYTH:   Real LRYTH(MLAYER), the thickness of the layers.
!  KSYMM:   Integer,  controlling parameter for the two-plane symmetry
!           for the 3D model.  KSYMM=1:  symmetric model;  =0:  non-sym-
!           metric model.
!  NBODY:   Integer,  number of substructures.
!  SUB_BLOCK:   Integer SUB_BLOCK(MBODY),  number of blocks in each substructure.
!  NX:      Integer NX(NBMAX,MBODY),  numbers of cells in the x-direction
!           in each block of the substructures.
!  NY:      Integer NY(NBMAX,MBODY),  numbers of cells in the y-direction
!           in each block of the substructures.
!  NZ:      Integer NZ(NBMAX,MBODY),  numbers of cells in the z-direction
!           in each block of the substructures.
!  BLX:     Real BLX(NBMAX,MBODY),  dimensions of each blocks of the
!           substructures in the x-direction.
!  BLY:     Real BLY(NBMAX,MBODY),  dimensions of each blocks of the
!           substructures in the y-direction.
!  BLZ:     Real BLZ(NBMAX,MBODY),  dimensions of each blocks of the
!           substructures in the z-direction.
!  XBL:     Real XBL(NBMAX,MBODY),  x-coordinates of each blocks in the
!           substructures.
!  YBL:     Real YBL(NBMAX,MBODY),  y-coordinates of each blocks in the
!           substructures.
!  ZBL:     Real ZBL(NBMAX,MBODY),  z-coordinates of each blocks in the
!           substructures.
!  X1, Y1, Z1: Real X1(NXMAX), Y1(NYMAX), Z1(NZMAX),  working arrays for
!           centre coordinates of the cells.
!
!
! Output parameters:
!
!  ZBND:    Real ZBND(0:MLAYER), the z-coordinates of the layer
!           boundaries with the air-earth interface being always
!           0 (zbnd(0)=0).
!  NCT:     Integer NCT(MBODY),  number of cells in each substructure.
!  NET:     Integer NET(MBODY),  number of unknowns in each substructure.
!           NET=3*NCT.
!  NCELL:   Integer NCELL(NBMAX,MBODY),  number of cells in each block
!           of the substructures.
!  NCT:     Integer,  total number of cells for the whole structure.
!  NEQ:     Integer,  total number of cells in one quadrant of a symmetric
!           model.
!  XCELL:   Real XCELL(NXMAX,NBMAX,MBODY),  x-coordinates of the cells.
!  YCELL:   Real YCELL(NYMAX,NBMAX,MBODY),  y-coordinates of the cells.
!  ZCELL:   Real ZCELL(NZMAX,NBMAX,MBODY),  z-coordinates of the cells.
!  KCELL:   Integer KCELL(MBODY), controlling parameter whether all cells
!           in each substructure are equal-sized.
!  BLX:     Real BLX(NBMAX,MBODY),  dimensions of each cells of the
!           substructures in the x-direction.
!  BLY:     Real BLY(NBMAX,MBODY),  dimensions of each cells of the
!           substructures in the y-direction.
!  BLZ:     Real BLZ(NBMAX,MBODY),  dimensions of each cells of the
!           substructures in the z-direction.
!
!**** Called by:  main
!**** Calls:      none


 IMPLICIT NONE
 INTEGER I,IB,J,KSYMM,L,MBODY,MLAYER,NBMAX,NBODY,NCTT,NEQ, NXMAX,NYMAX,NZMAX, &
         NX(NBMAX,MBODY),NY(NBMAX,MBODY),NZ(NBMAX,MBODY),NCELL(NBMAX,MBODY), &
         NCT(MBODY),NET(MBODY),KCELL(MBODY),SUB_BLOCK(MBODY)
 REAL DMIN,LRYTH(MLAYER),ZBND(0:MLAYER),BLX(NBMAX,MBODY),BLY(NBMAX,MBODY), &
      BLZ(NBMAX,MBODY),XBL(NBMAX,MBODY),YBL(NBMAX,MBODY),ZBL(NBMAX,MBODY),               &
      XCELL(NXMAX,NBMAX,MBODY),YCELL(NYMAX,NBMAX,MBODY),ZCELL(NZMAX,NBMAX,MBODY),        &
      X1(NXMAX),Y1(NYMAX),Z1(NZMAX)

!-- The air is isotropic in conductivity

! Construct layer boundaries coordinates from layer thickness
!
 ZBND(0)=0.
 DO I=1,MLAYER-1
   ZBND(I)=ZBND(I-1)+LRYTH(I)
 END DO

 NCTT=0
 DO IB=1,NBODY
    NCT(IB)=0
    DO I=1,SUB_BLOCK(IB)
       NCELL(I,IB)=NX(I,IB)*NY(I,IB)*NZ(I,IB)
       NCT(IB)=NCT(IB)+NCELL(I,IB)
    END DO
!
    NET(IB)=3*NCT(IB)
    NCTT=NCTT+NCT(IB)
 END DO

! ---- NEQ is the total number of cells in a quarter used by
!    group theoretical reduction

 IF (KSYMM==1) THEN
    NEQ=0
    DO IB=1,NBODY
       NEQ=NEQ+NET(IB)
    END DO
 END IF

! ----- Dividing the blocks into cells
! ----- Note BLX BLY and BLZ become now the dimensions of the cells

 DO IB=1,NBODY
    DO I=1,SUB_BLOCK(IB)
       CALL INIT_DISCRETIZATION(BLX(I,IB),BLY(I,IB),BLZ(I,IB),NX(I,IB), &
                                NY(I,IB),NZ(I,IB),XBL(I,IB),YBL(I,IB),  &
                                ZBL(I,IB),X1,Y1,Z1)
       DO J=1,NX(I,IB)
          XCELL(J,I,IB)=X1(J)
       END DO
       DO J=1,NY(I,IB)
          YCELL(J,I,IB)=Y1(J)
       END DO
       DO J=1,NZ(I,IB)
          ZCELL(J,I,IB)=Z1(J)
       END DO
    END DO
 END DO


! --- Determine if all the cells in a substructure have the same size.
!   KCELL(ib)=1: yes; KCELL(ib)=0: no.
 DO IB=1,NBODY
    KCELL(IB)=1
    DO L=2,SUB_BLOCK(IB)
       IF (ABS(BLX(L,IB)-BLX(1,IB))>DMIN.OR.  &
           ABS(BLY(L,IB)-BLY(1,IB))>DMIN.OR.  &
           ABS(BLZ(L,IB)-BLZ(1,IB))>DMIN) KCELL(IB)=0
    END DO
 END DO

 END SUBROUTINE INIT_COMPUTATION


 SUBROUTINE INIT_3D_INPUT_TEST (NLG,DMIN,TRGT_BLCK,BLCK_LX,BLCK_LY,BLCK_LZ, &
                                BLCK_CX,BLCK_CY,BLCK_CZ)
!---------------------------------------------------------------------------

!**** Test 3D data
!
!Input parameters:
!
!
!Other parameters for this routine are the same as those in routine
!READATA.
!
!**** Called by:  INIT_INPUT_TEST
!
!**** Calls:      none

 IMPLICIT NONE

 INTEGER I,II,J,JJ,KK,TRGT_BLCK,NLG
 REAL DMIN,XI,XJ,YI,YJ,ZI,ZJ,BLCK_LX(TRGT_BLCK),BLCK_LY(TRGT_BLCK),   &
      BLCK_LZ(TRGT_BLCK),BLCK_CX(TRGT_BLCK),BLCK_CY(TRGT_BLCK),BLCK_CZ(TRGT_BLCK)


! ----- Test if the blocks overlap each other
 DO I=1,TRGT_BLCK
   DO J=1,TRGT_BLCK

     IF (I/=J) THEN

       XJ=.5*BLCK_LX(J)-DMIN
       YJ=.5*BLCK_LY(J)-DMIN
       ZJ=.5*BLCK_LZ(J)-DMIN
       DO II=1,5
         DO JJ=1,5
           DO KK=1,5
             XI=BLCK_CX(I)-.5*BLCK_LX(I)+BLCK_LX(I)*(II-1.)/4.
             YI=BLCK_CY(I)-.5*BLCK_LY(I)+BLCK_LY(I)*(JJ-1.)/4.
             ZI=BLCK_CZ(I)-.5*BLCK_LZ(I)+BLCK_LZ(I)*(KK-1.)/4.
             IF (XI > BLCK_CX(J)-XJ .AND. XI < BLCK_CX(J)+XJ .AND.  &
                 YI > BLCK_CY(J)-YJ .AND. YI < BLCK_CY(J)+YJ .AND.  &
                 ZI > BLCK_CZ(J)-ZJ .AND. ZI < BLCK_CZ(J)+ZJ) THEN
                WRITE(NLG,1) I,J
                STOP
             END IF
            END DO
          END DO
        END DO
     END IF

   END DO
 END DO
 ! WRITE (*,2)

 1 FORMAT(/T3,'Block #',I3,' overlaps with block #',I3,'  This is verboten!' &
          /T3,'Please check input data and correct the problem to let execution occur.'/)
 2 FORMAT(/T3,'Input data passed elementary tests.  Computation begins ...'/)

 END SUBROUTINE INIT_3D_INPUT_TEST


 SUBROUTINE INIT_DISCRETIZATION (BLX,BLY,BLZ,NX,NY,NZ,XCD,YCD,ZCD,X,Y,Z)
!-----------------------------------------------------------------------
!*****  Discretise a prism into equal-size cells
!
 IMPLICIT NONE

 REAL BLX,BLY,BLZ,X,XCD,Y,YCD,Z,ZCD
 INTEGER I,N1,NX,NY,NZ
 DIMENSION X(NX),Y(NY),Z(NZ)
 BLX=BLX/FLOAT(NX)
 BLY=BLY/FLOAT(NY)
 BLZ=BLZ/FLOAT(NZ)
 IF (ABS(AMOD(FLOAT(NX),2.))<.2) THEN
    N1=NX/2
    DO I=1,NX
       X(I)=BLX*(FLOAT(I)-FLOAT(N1))-BLX/2.+XCD
    END DO
 ELSE
    N1=NX/2+1
    DO I=1,NX
       X(I)=BLX*(I-N1)+XCD
    END DO
 END IF
 IF (ABS(AMOD(FLOAT(NY),2.))<.2) THEN
    N1=NY/2
    DO I=1,NY
       Y(I)=BLY*(FLOAT(I)-FLOAT(N1)-.5)+YCD
    END DO
 ELSE
    N1=NY/2+1
    DO I=1,NY
       Y(I)=BLY*(FLOAT(I)-FLOAT(N1))+YCD
    END DO
 END IF
 IF (ABS(AMOD(FLOAT(NZ),2.))<.2) THEN
    N1=NZ/2
    DO I=1,NZ
       Z(I)=BLZ*(FLOAT(I)-FLOAT(N1)-.5)+ZCD
    END DO
 ELSE
    N1=NZ/2+1
    DO I=1,NZ
       Z(I)=BLZ*(FLOAT(I)-FLOAT(N1))+ZCD
    END DO
 END IF
 RETURN
 END SUBROUTINE INIT_DISCRETIZATION


 SUBROUTINE INIT_REF_CELL_DIM (MBODY,NBMAX,NBODY,SUB_BLOCK,BLX,BLY,BLZ,DMIN,KCLMN,CLMN)
!-------------------------------------------------------------------------------------
!
!   Determines the reference length for numerical integration
!   of the Green's functions as required by the accuracy levels
!   determined by the parameter KACC.
!
!   If clmn(ib)=0., it is then replaced by the default value which
!   is chosen as follows. The code finds at first two cells with
!   maximal diagonal lengths. Note that in each block the cells
!   have the same size. Let the dimensions of one of these two cells,
!   blx1, bly1, and blz1 be arranged in an increasing order, say,
!   blx1<=bly1<=blz1, then the mediate length of the cell is bly1.
!   The mediate length of the other cell can be found in the same
!   way. The value of clmn(ib) is then the average of those two
!   mediate lengths.
!
!   Note that clmn takes its value for each substructure.
!
!
!Input parameters:
!
!  MBODY:   Integer(*4),  maximal number of substructures allowed in
!           the program.
!  NBMAX:   Integer,  maximal number of blocks allowed in a
!           substructure.  It is the maximum of SUB_BLOCK used for
!           array definitions.
!  NBODY:   Integer,  number of substructures.
!  SUB_BLOCK:  Integer SUB_BLOCK(MBODY),  number of blocks in each substructure.
!  BLX:     Real BLX(NBMAX,MBODY),  dimensions of each blocks of the
!           substructures in the x-direction.
!  BLY:     Real BLY(NBMAX,MBODY),  dimensions of each blocks of the
!           substructures in the y-direction.
!  BLZ:     Real BLZ(NBMAX,MBODY),  dimensions of each blocks of the
!           substructures in the z-direction.
!  DMIN:    Real,  minimal dimension value that is considered to be
!           zero.  DMIN=0.1 for the low frequency module and DMIN=0.001
!           for the high frequency module.
!  KCLMN:   Integer,  parameter controlling whether values for CLMN
!           need to be input or to be set automatically by this routine.
!
!Output parameters:
!
!  CLMN:    Real CLMN(MBODY),  the reference lengths in each substructure.
!
!
!**** Called by:  main
!
!**** Calls:      none
!
!
 IMPLICIT NONE

 REAL AA1,AA2,CL1,CLMN1,CLMN2,CLXM,CLYM,CLZM,DMIN
 INTEGER IB,KCLMN,L,LL,LM,LMN,LN,MBODY,NBMAX,NBODY
 REAL BLX(NBMAX,MBODY),BLY(NBMAX,MBODY),BLZ(NBMAX,MBODY),CLMN(MBODY)
 INTEGER SUB_BLOCK(MBODY)

 DO IB=1,NBODY

   IF (KCLMN==0) CLMN(IB)=0.

! ---- cl1 is the diagonal length of a cell
   CL1=SQRT(BLX(1,IB)**2+BLY(1,IB)**2+BLZ(1,IB)**2)
   LMN=1
   CLXM=BLX(1,IB)
   CLYM=BLY(1,IB)
   CLZM=BLZ(1,IB)
   DO L=2,SUB_BLOCK(IB)
     AA1=SQRT(BLX(L,IB)**2+BLY(L,IB)**2+BLZ(L,IB)**2)
     IF (CL1<AA1) THEN
       CL1=AA1
       CLXM=BLX(L,IB)
       CLYM=BLY(L,IB)
       CLZM=BLZ(L,IB)
       LMN=L
     END IF
   END DO

   LL=1

 50  AA1=AMAX1(CLXM,CLYM,CLZM)
   AA2=MIN(CLXM,CLYM,CLZM)

   IF (ABS(CLXM-AA1)<=DMIN) LM=1
   IF (ABS(CLYM-AA1)<=DMIN) LM=2
   IF (ABS(CLZM-AA1)<=DMIN) LM=3
   IF (ABS(CLXM-AA2)<=DMIN) LN=1
   IF (ABS(CLYM-AA2)<=DMIN) LN=2
   IF (ABS(CLZM-AA2)<=DMIN) LN=3

   IF (LM==2) THEN

     IF (LN==2) THEN
       CLMN1=CLYM
     ELSE IF (LN==3) THEN
       CLMN1=CLXM
     ELSE
       CLMN1=CLZM
     END IF
   ELSE IF (LM==3) THEN

     IF (LN==2) THEN
       CLMN1=CLXM
     ELSE IF (LN==3) THEN
       CLMN1=CLZM
     ELSE
       CLMN1=CLYM
     END IF

   ELSE IF (LN==2) THEN
     CLMN1=CLZM
   ELSE IF (LN==3) THEN
     CLMN1=CLYM
   ELSE
     CLMN1=CLXM
   END IF

   IF (SUB_BLOCK(IB)/=1) THEN
     IF (LL==1) THEN  !  Determine the second mediate length
       CL1=0.
       DO L=1,SUB_BLOCK(IB)
         IF (L/=LMN) THEN
           AA1=SQRT(BLX(L,IB)**2+BLY(L,IB)**2+BLZ(L,IB)**2)
           IF (CL1<AA1) THEN
             CL1=AA1
             CLXM=BLX(L,IB)
             CLYM=BLY(L,IB)
             CLZM=BLZ(L,IB)
           END IF
         END IF
       END DO

       CLMN2=CLMN1
       LL=2
       GOTO 50

     ELSE
       CLMN1=(CLMN1+CLMN2)/2.

     END IF
   END IF
   IF (CLMN(IB)<DMIN) CLMN(IB)=CLMN1

 END DO

 END SUBROUTINE INIT_REF_CELL_DIM

      SUBROUTINE INIT_SUPER_BLOCK(NLG,MBODY,NBMAX,NSMR,KSYMM,DMIN,NXMAX,NYMAX, &
                                  NZMAX,NBODY,SUB_BLOCK,BLX,BLY,BLZ,NX,NY,NZ, &
                                  XBL,YBL,ZBL,XCELL,YCELL,ZCELL,SBX,SBY,SBZ,  &
                                  XSB,YSB,ZSB,SCX,SCY,SCZ,NXS,NYS,NZS,KSMR)
!
!**** Determine the super block for the spatial symmetry reductions
!
!   This routine find the super block that contains all the
!   structural blocks of the 3D body.  All disrectised cells
!   must coincide with the cells of the super block to facilitate
!   the spatial symmetry reduction.
!
!   Note that unlike in routine INIT_COMPUTATION, the arrays BLX,  BLY,
!   and BLZ containes the dimensions of the cells instead of the
!   blocks.
!
!Input parameters:
!
!  MBODY:   Integer(*4), the maximal number of substructures allowed
!           in the program.
!  NBMAX:   Integer,  the maximal number of blocks allowed in a
!           substructure.  It is the maximum of SUB_BLOCK used for
!           array definitions.
!  NSMR:    Integer,  the maximal number of non-identical elements
!           for the scattering matrix which is determined by the
!           parameters NZSR, NZOB and NHFILM of the code.  See the corresponding
!           parameter list in the main program for detail.
!  KSYMM:   Integer,  controlling parameter for the two-plane symmetry
!           for the 3D model.  KSYMM=1:  symmetric model;  =0:  non-sym-
!           metric model.
!  DMIN:    Real,  minimal dimension value that is considered to be
!           zero.  DMIN=0.1 for the low frequency module and DMIN=0.001
!           for the high frequency module.
!  NXMAX:   Integer,  maximal number of cells in the x-direction
!           in a block.
!  NYMAX:   Integer,  maximal number of cells in the y-direction
!           in a block.
!  NZMAX:   Integer,  maximal number of cells in the z-direction
!           in a block.
!  NBODY:   Integer,  number of substructures.
!  SUB_BLOCK:  Integer SUB_BLOCK(MBODY),  number of blocks in each substructure.
!  BLX:     Real BLX(NBMAX,MBODY),  dimensions of each cells of the
!           substructures in the x-direction.
!  BLY:     Real BLY(NBMAX,MBODY),  dimensions of each cells of the
!           substructures in the y-direction.
!  BLZ:     Real BLZ(NBMAX,MBODY),  dimensions of each cells of the
!           substructures in the z-direction.
!  NX:      Integer NX(NBMAX,MBODY),  numbers of cells in the x-direction
!           in each block of the substructures.
!  NY:      Integer NY(NBMAX,MBODY),  numbers of cells in the y-direction
!           in each block of the substructures.
!  NZ:      Integer NZ(NBMAX,MBODY),  numbers of cells in the z-direction
!           in each block of the substructures.
!  XBL:     Real XBL(NBMAX,MBODY),  x-coordinates of each blocks in the
!           substructures.
!  YBL:     Real YBL(NBMAX,MBODY),  y-coordinates of each blocks in the
!           substructures.
!  ZBL:     Real ZBL(NBMAX,MBODY),  z-coordinates of each blocks in the
!           substructures.
!  XCELL:   Real XCELL(NXMAX,NBMAX,MBODY),  x-coordinates of the cells.
!  YCELL:   Real YCELL(NYMAX,NBMAX,MBODY),  y-coordinates of the cells.
!  ZCELL:   Real ZCELL(NZMAX,NBMAX,MBODY),  z-coordinates of the cells.
!
!
!Output parameters:
!
!  SBX:     Real,  dimension of the super block in the x-direction.
!  SBY:     Real,  dimension of the super block in the y-direction.
!  SBZ:     Real,  dimension of the super block in the z-direction.
!  XSB:     Real,  x-coordinate of the superblock.
!  YSB:     Real,  y-coordinate of the superblock.
!  ZSB:     Real,  z-coordinate of the superblock.
!  SCX:     Real,  dimension the cells of the super block in the
!           x-direction.
!  SCY:     Real,  dimension the cells of the super block in the
!           y-direction.
!  SCZ:     Real,  dimension the cells of the super block in the
!           z-direction.
!  NXS:     Integer,  number of cells in the x-direction of the superblock.
!  NYS:     Integer,  number of cells in the y-direction of the superblock.
!  NZS:     Integer,  number of cells in the z-direction of the superblock.
!  KSMR:    Integer,  controlling parameter whether spatial symmetry can
!           be used or not.  KSMR=1:  yes;  =0:  no.
!
!**** Called by:  main
!
!**** Calls:      none
!
!
      IMPLICIT NONE
!
      REAL DMIN,SBX,SBX1,SBX2,SBY,SBY1,SBY2,SBZ,SBZ1,SBZ2,SCX,SCY,SCZ,   &
           XI,XSB,YI,YSB,ZI,ZSB
      INTEGER I,IB,II,J,JB,JJ,K,KCELLT,KK,KSMR,KSYMM,L,MBODY,NBMAX,      &
              NBODY,NSMR,NXMAX,NXS,NYMAX,NYS,NLG
      INTEGER NZMAX,NZS
      INTEGER SUB_BLOCK(MBODY),NX(NBMAX,MBODY),NY(NBMAX,MBODY),NZ(NBMAX,MBODY)
      REAL BLX(NBMAX,MBODY),BLY(NBMAX,MBODY),BLZ(NBMAX,MBODY),           &
           XBL(NBMAX,MBODY),YBL(NBMAX,MBODY),ZBL(NBMAX,MBODY),           &
           XCELL(NXMAX,NBMAX,MBODY),YCELL(NYMAX,NBMAX,MBODY),            &
           ZCELL(NZMAX,NBMAX,MBODY)
! ---- Determine if the spatial symmetry reduction can be used
!
! --- Determine if all the cells have the same size.
!   KCELLT=1: yes; =0: no
!
      KCELLT=1
      DO IB=1,NBODY
         DO L=1,SUB_BLOCK(IB)
            IF (IB/=1.OR.L/=1) THEN
               IF (ABS(BLX(L,IB)-BLX(1,1))>DMIN.OR.  &
                   ABS(BLY(L,IB)-BLY(1,1))>DMIN.OR.  &
                   ABS(BLZ(L,IB)-BLZ(1,1))>DMIN) THEN
                  KCELLT=0
                  GOTO 100
               END IF
            END IF
         END DO
      END DO
!
! --- If all the cells have the same size (kcellt=1), great saving
!   can be achieved by spatial symmetry reduction in forming the
!   scattering matrix.
!
!
!-- Find the dimension of the super block.
!   Note that the parameters blx etc. are the cell dimensions now
!  xbl etc. are still the coordinates of the blocks.

    SBX1=10000000.
      SBY1=10000000.
      SBZ1=10000000.
      SBX2=-10000000.
      SBY2=-10000000.
      SBZ2=-10000000.
!
      DO IB=1,NBODY
         DO L=1,SUB_BLOCK(IB)
            SBX1=MIN(SBX1,XBL(L,IB)-.5*BLX(L,IB)*NX(L,IB))
            SBX2=AMAX1(SBX2,XBL(L,IB)+.5*BLX(L,IB)*NX(L,IB))
            SBY1=MIN(SBY1,YBL(L,IB)-.5*BLY(L,IB)*NY(L,IB))
            SBY2=AMAX1(SBY2,YBL(L,IB)+.5*BLY(L,IB)*NY(L,IB))
            SBZ1=MIN(SBZ1,ZBL(L,IB)-.5*BLZ(L,IB)*NZ(L,IB))
            SBZ2=AMAX1(SBZ2,ZBL(L,IB)+.5*BLZ(L,IB)*NZ(L,IB))
         END DO
      END DO
!
      SBX=SBX2-SBX1
      SBY=SBY2-SBY1
      SBZ=SBZ2-SBZ1
!
!-- For symmetric structures the super block need to include
!   the block that are not input.  The super block must also
!   include the full region covered by the SBX2 instead of
!   SBX2-SBX1

      IF (KSYMM==1) THEN
         SBX=2.*SBX2
         SBY=2.*SBY2
      END IF
!
!-- Determine the centre coordinates of the super block
!
      IF (KSYMM==0) THEN
         XSB=SBX2-.5*SBX
         YSB=SBY2-.5*SBY
      ELSE
         XSB=0.
         YSB=0.
      END IF
      ZSB=SBZ2-.5*SBZ
!
!-- Determine the number of cells of the super block
!
      NXS=INT(SBX/BLX(1,1)+.2)
      NYS=INT(SBY/BLY(1,1)+.2)
      NZS=INT(SBZ/BLZ(1,1)+.2)
!
!-- Check if all cells coincide with the cells of the super block.
!   If so, the symmetry reduction can be used, and the parameter
!   KSMR is set to be 1.  Otherwise, the symmetry reduction cannot
!   be used even if kcellt=1, and KSMR is assigned a 0.
!
!   This may be done by comparing the dimensions of a cell of the
!   super block and the dimensions of a cell of the original body.
!   But such a check may fail if a block in between does not coincide
!   with the super block cells.
!
!   Note the blocks in other three quadrants for a symmetric
!   structure need not be tested since the super block automatically
!   contain them by reflection.
!
!-- Compute the cell dimension of the super block
!
      SCX=SBX/NXS
      SCY=SBY/NYS
      SCZ=SBZ/NZS
!
      DO IB=1,NBODY
         DO L=1,SUB_BLOCK(IB)
!
            DO I=1,NX(L,IB)
               DO J=1,NY(L,IB)
                  DO K=1,NZ(L,IB)
                     DO II=1,NXS
                        DO JJ=1,NYS
                           DO KK=1,NZS
                              XI=XSB-.5*SBX+.5*SCX+(II-1)*SCX
                              YI=YSB-.5*SBY+.5*SCY+(JJ-1)*SCY
                              ZI=ZSB-.5*SBZ+.5*SCZ+(KK-1)*SCZ
                              IF (ABS(XI-XCELL(I,L,IB))<DMIN.AND.  &
                                  ABS(YI-YCELL(J,L,IB))<DMIN.AND.  &
                                  ABS(ZI-ZCELL(K,L,IB))<DMIN) THEN
                              GOTO 5
                              END IF
!
                           END DO
                        END DO
                     END DO
!
!-- If any cell of the original body fails to identify a cell
!   from the super block, ksmr=0
!
                     KSMR=0
!
                     WRITE (*,1010) XCELL(I,L,IB),YCELL(I,L,IB),ZCELL(I,L,IB)
                     WRITE (NLG,1010) XCELL(I,L,IB),YCELL(I,L,IB),ZCELL(I,L,IB)
!
!-- Partial use of the spatial symmetry may be possible for
!   substructures with one block
!
                     IF (KSYMM==0) THEN
                        DO JB=1,NBODY
                           IF (SUB_BLOCK(JB)==1) THEN
                              WRITE (*,1020) JB
                              WRITE (NLG,1020) JB
                           END IF
                        END DO
                     END IF
!
                     GOTO 100
!
    5             END DO
               END DO
            END DO
         END DO
      END DO
!
!-- Now that all cells passed the test, namely, all cells have
!   successfully identified a cell from the super block, symmetry
!   reduction can be used
!
      KSMR=1

  100 IF (KCELLT==0) KSMR=0

      IF (KSMR==1) NSMR=9*NXS*NYS*NZS*(NZS+1)/2
!
     RETURN
 1010 FORMAT (/T3,'The spatial symmetry reduction cannot be used for the whole body' &
              /T3,'because the cell with the coordinates:',3F14.5                    &
              /T3,'does not coincide with any cell of the super block.  You will need to '     &
              /T3,'re-discretise the body in order to use the spatial symmetry reduction.')
 1020 FORMAT (/T3,'Partial use of spatial symmetry reduction for substructure #',I4)
      END
!
!**** End of INIT_SUPER_BLOCK
!
!
 SUBROUTINE INIT_RHOMAX (MBODY,NBMAX,NXMAX,NYMAX,KSYMM,NBODY,SUB_BLOCK,NX,NY, &
                         XCELL,YCELL,BLX,BLY,NTX_FD,NCRD,TX_CRDX,TX_CRDY,  &
                         N_RX,RX_X,RX_Y,M_RX,RHOMIN,RMAX1,RMIN1,RMAX2,RMIN2)
!
!**** Determine the maximal dimension of the model geometry in the
!     lateral directions
!
! Input parameters:
!
!  MBODY:   Integer,  maximal number of substructures allowed in the
!           program.
!  NBMAX:   Integer,  maximal number of blocks allowed in a
!           substructure.  It is the maximum of SUB_BLOCK used for
!           array definitions.
!  NXMAX:   Integer,  maximal number of cells in the x-direction
!           in a block.
!  NYMAX:   Integer,  maximal number of cells in the y-direction
!           in a block.
!  KSYMM:   Integer,  controlling parameter for the two-plane symmetry
!           for the 3D model.  KSYMM=1:  symmetric model;  =0:  non-sym-
!           metric model.
!  NBODY:   Integer,  number of substructures.
!  SUB_BLOCK:  Integer SUB_BLOCK(MBODY),  number of blocks in each substructure.
!  NX:      Integer NX(NBMAX,MBODY),  numbers of cells in the x-direction
!           in each block of the substructures.
!  NY:      Integer NY(NBMAX,MBODY),  numbers of cells in the y-direction
!           in each block of the substructures.
!  XCELL:   Real XCELL(NXMAX,NBMAX,MBODY),  x-coordinates of the cells.
!  YCELL:   Real YCELL(NYMAX,NBMAX,MBODY),  y-coordinates of the cells.
!  BLX:     Real BLX(NBMAX,MBODY),  dimensions of each cells of the
!           substructures in the x-direction.
!  BLY:     Real BLY(NBMAX,MBODY),  dimensions of each cells of the
!           substructures in the y-direction.
!  NTX_FD:    Integer,  number of excitations.
!  NCRD:    Integer,  number of electrodes or number of different coordinates
!           for the source configuration per excitation.
!  RX_X,RX_Y:  Real RX_X(M_RX,NTX_FD), RX_Y(M_RX,NTX_FD),
!           receiver sites coordinates in the
!           x, and y-directions the receivers of each excitation.
!  N_RX:    Integer N_RX(NTX_FD),  number of receivers for each excitation.
!  M_RX:    Integer,  maximal number of receivers for controlled source
!           problems.
!  RHOMIN:  Real,  minimum of the lateral grid (rho) which determines
!           the minimum of rho in the Hankel integrals.  RHOMIN=0.1 for
!           the low frequency module and RHOMIN=0.01 for the high
!           frequency module.
!  RMAX1:   Real,  the maximum of the geometric dimensions of the model
!           in the lateral direction.
!  RMIN1:   Real,  the minimum of the geometric dimensions of the model
!           in the lateral direction.
!  RMAX2:   Real,  the maximum of the geometric dimensions of the model.
!  RMIN2:   Real,  the minimum of the geometric dimensions of the model.
!
!
!Output parameter:
!
!
!**** Called by:  main
!
!**** Calls:      none

 IMPLICIT NONE

 INTEGER I,IB,IB2,II,J,JJ,KSYMM,L,LL,LSYM,MBODY,M_RX,NBMAX,NBODY,NCRD, &
         JX,NN,NSYM,NTX_FD,NXMAX,NYMAX,NX(NBMAX,MBODY),NY(NBMAX,MBODY),        &
         SUB_BLOCK(MBODY),N_RX(NTX_FD)
 REAL RHOMIN,RMAX,RMAX1,RMAX2,RMIN1,RMIN2,RR1,RR2,RR3,RR4,RR5,SX,SY, &
      BLX(NBMAX,MBODY),BLY(NBMAX,MBODY),XCELL(NXMAX,NBMAX,MBODY),    &
      YCELL(NYMAX,NBMAX,MBODY),TX_CRDX(NCRD,NTX_FD),TX_CRDY(NCRD,NTX_FD),       &
      RX_X(M_RX,NTX_FD),RX_Y(M_RX,NTX_FD)

!
! ---- Determine the maximal dimensions of the structure in
!    the lateral direction (the range of Rho).
!    The minimum should always be rhomin
!
 RMAX=0.
 RR1=0.
 DO IB=1,NBODY
   DO L=1,SUB_BLOCK(IB)
     DO I=1,NX(L,IB)
       DO J=1,NY(L,IB)
         DO IB2=1,NBODY
           DO LL=1,SUB_BLOCK(IB2)
             DO II=1,NX(LL,IB2)
               DO JJ=1,NY(LL,IB2)
                 IF (IB/=IB2.OR.L/=LL.OR.I/=II.OR.J/=JJ) THEN
                    RR1=SQRT((XCELL(I,L,IB)-XCELL(II,LL,IB2))    &
                      **2+(YCELL(J,L,IB)-YCELL(JJ,LL,IB2))**2)
                 END IF
                 RR2=SQRT((ABS(XCELL(I,L,IB)-XCELL(II,LL,IB2))   &
                     +.5*BLX(LL,IB2))**2+(ABS(YCELL(J,L,IB)-     &
                     YCELL(JJ,LL,IB2)))**2)
                 RR3=SQRT((ABS(XCELL(I,L,IB)-XCELL(II,LL,IB2))   &
                     -.5*BLX(LL,IB2))**2+(ABS(YCELL(J,L,IB)-     &
                     YCELL(JJ,LL,IB2)))**2)
                 RR4=SQRT((ABS(XCELL(I,L,IB)-XCELL(II,LL,IB2)))  &
                    **2+(ABS(YCELL(J,L,IB)-YCELL(JJ,LL,IB2))    &
                     +.5*BLY(LL,IB2))**2)
                 RR5=SQRT((ABS(XCELL(I,L,IB)-XCELL(II,LL,IB2)))  &
                     **2+(ABS(YCELL(J,L,IB)-YCELL(JJ,LL,IB2))    &
                     -.5*BLY(LL,IB2))**2)
                 RMAX=MAX(RR1,RR2,RR3,RR4,RR5,RMAX)
                 IF (KSYMM==1) THEN
                   RR2=SQRT((ABS(XCELL(I,L,IB)+XCELL(II,LL,IB2))   &
                       +.5*BLX(LL,IB2))**2+(ABS(YCELL(J,L,IB)+     &
                       YCELL(JJ,LL,IB2))+.5*BLY(LL,IB2))**2)
                   IF (RR2>RMAX) RMAX=RR2
                 END IF
               END DO
             END DO
           END DO
         END DO
       END DO
     END DO
   END DO
 END DO

 RMIN2=RHOMIN
 RMAX2=RMAX

! --- Determine the maximal source-receiver and target-receiver
!     distances
!
! -- Determine the maximal source-receiver distance
!
 RMAX=0.
 DO I=1,NTX_FD
   DO JJ=1,N_RX(I)
!
! -- Note that circular loops are checked by their maximal and
!    minimal wire locations.
!
     DO J=1,NCRD
       RR1=SQRT((RX_X(JJ,I)-TX_CRDX(J,I))**2+             &
                (RX_Y(JJ,I)-TX_CRDY(J,I))**2)
       IF (RR1>RMAX) RMAX=RR1
     END DO
   END DO
 END DO

 RMAX1=RMAX

!
!-- Determine maximal source-target distance
!
 RMAX=0.
 DO IB=1,NBODY
   DO L=1,SUB_BLOCK(IB)
     DO I=1,NX(L,IB)
       DO J=1,NY(L,IB)
         IF (KSYMM==0) THEN
           NSYM=1
         ELSE
           NSYM=4
         END IF
         DO LSYM=1,NSYM
           IF (LSYM==1) THEN
             SX=1.
             SY=1.
           END IF
           IF (LSYM==2) THEN
             SX=-1.
             SY=1.
           END IF
           IF (LSYM==3) THEN
             SX=1.
             SY=-1.
           END IF
           IF (LSYM==4) THEN
             SX=-1.
             SY=-1.
           END IF
           DO JX=1,NTX_FD
             NN=NCRD
             DO JJ=1,NN
               RR1=SQRT((TX_CRDX(JJ,JX)-XCELL(I,L,IB))**2+      &
                        (TX_CRDY(JJ,JX)-YCELL(J,L,IB))**2)
               RR2=SQRT((ABS(TX_CRDX(JJ,JX)-SX*XCELL(I,L,IB))   &
                   +.5*BLX(L,IB))**2+(ABS(TX_CRDY(JJ,JX)-       &
                   SY*YCELL(J,L,IB)))**2)
               RR3=SQRT((ABS(TX_CRDX(JJ,JX)-SX*XCELL(I,L,IB))   &
                   -.5*BLX(L,IB))**2+(ABS(TX_CRDY(JJ,JX)-       &
                   SY*YCELL(J,L,IB)))**2)
               RR4=SQRT((ABS(TX_CRDX(JJ,JX)-SX*XCELL(I,L,IB)))  &
                   **2+(ABS(TX_CRDY(JJ,JX)-SY*YCELL(J,L,IB))    &
                   +.5*BLY(L,IB))**2)
               RR5=SQRT((ABS(TX_CRDX(JJ,JX)-SX*XCELL(I,L,IB)))  &
                   **2+(ABS(TX_CRDY(JJ,JX)-SY*YCELL(J,L,IB))    &
                   -.5*BLY(L,IB))**2)
               RMAX=MAX(RR1,RR2,RR3,RR4,RR5,RMAX)
             END DO
           END DO
         END DO
       END DO
     END DO
   END DO
 END DO

      RMAX1=AMAX1(RMAX1,RMAX)
!
!-- Determine maximal target-receiver distance
!
 RMAX=0.
 DO JX=1,NTX_FD
   DO JJ=1,N_RX(JX)
     DO IB=1,NBODY
       DO L=1,SUB_BLOCK(IB)
         DO I=1,NX(L,IB)
           DO J=1,NY(L,IB)
             IF (KSYMM==0) THEN
             NSYM=1
             ELSE
             NSYM=4
             END IF
             DO LSYM=1,NSYM
             IF (LSYM==1) THEN
             SX=1.
             SY=1.
             END IF
             IF (LSYM==2) THEN
             SX=-1.
             SY=1.
             END IF
             IF (LSYM==3) THEN
             SX=1.
             SY=-1.
             END IF
             IF (LSYM==4) THEN
             SX=-1.
             SY=-1.
             END IF
             RR1=SQRT((RX_X(JJ,JX)-XCELL(I,L,IB))**2+      &
                      (RX_Y(JJ,JX)-YCELL(J,L,IB))**2)
             RR2=SQRT((ABS(RX_X(JJ,JX)-SX*XCELL(I,L,IB))   &
                 +.5*BLX(L,IB))**2+(ABS(RX_Y(JJ,JX)-       &
                 SY*YCELL(J,L,IB)))**2)
             RR3=SQRT((ABS(RX_X(JJ,JX)-SX*XCELL(I,L,IB))   &
                 -.5*BLX(L,IB))**2+(ABS(RX_Y(JJ,JX)-       &
                 SY*YCELL(J,L,IB)))**2)
             RR4=SQRT((ABS(RX_X(JJ,JX)-SX*XCELL(I,L,IB)))  &
                 **2+(ABS(RX_Y(JJ,JX)-SY*YCELL(J,L,IB))    &
                 +.5*BLY(L,IB))**2)
             RR5=SQRT((ABS(RX_X(JJ,JX)-SX*XCELL(I,L,IB)))  &
                 **2+(ABS(RX_Y(JJ,JX)-SY*YCELL(J,L,IB))    &
                 -.5*BLY(L,IB))**2)
             RMAX=MAX(RR1,RR2,RR3,RR4,RR5,RMAX)
             END DO
           END DO
         END DO
       END DO
     END DO
   END DO
 END DO

 RMAX1=AMAX1(RMAX1,RMAX)
 RMIN1=RHOMIN  ! Always set RMIN1=RHOMIN

! --- RMIN1 can sometimes be zero,  as in the case of 1D downhole
!     computation.

      IF (RMAX1<RHOMIN) RMAX1=RHOMIN

! --- In order to avoid unaccounted errors,  increase the values of
!     RMAX1 and RMAX2

      RMAX1=1.2*RMAX1
      RMAX2=1.1*RMAX2


  END SUBROUTINE INIT_RHOMAX
!
!
   SUBROUTINE INIT_ZLVLSCS(MZGRID,NTX_FD,M_RX,N_RX,NCRD,TX_CRDZ,RX_Z, &
                           DMIN,NZSR,ZSRG,NZOB,ZOBG)
!
!**** Determine the z and z'-levels in the vertical direction
!
!     The program automatically finds all z and z'-levels for the
!     3D body and for the sources and receivers.  Hankel integrals
!     are computed at those levels for various rho (in the lateral
!     direction) for interpolations for computing the (integrated)
!     Green's tensors.
!
!  Input parameters:
!
!    MZRGID:  Integer,  dimension parameter for the arrays ZOBG and
!             ZSRG.  MZGRID must be much larger than NZTMAX since
!             these two arrays also works as working arrays.
!    NTX_FD:   Integer,  number of  excitations.
!    M_RX:
!    N_RX:    Integer N_RX(NTX_FD),  number of receiver sites.
!    NCRD:
!    TX_CRDZ:    TX_CRDZ(NCRD,NTX_FD), z-coordinates of electrodes or the centre
!             z-coordinates of a current loop or a magnetic dipole for all the
!             excitations.
!    RX_Z:    Real RX_Z(M_RX,NTX_FD),  receiver sites coordinates in the
!             z-direction for the receiver sites.
!    DMIN:
!
!  Output parameters:
!
!     NZOB:   Integer, number of z-levels for the excitation sources.
!     NZSR:   Integer,  number of z-levels for the excitation sources.
!     ZOBG:  Real ZOBG(MZGRID),  the coordinates of the nzob z-levels
!             for the excitation sources.
!     ZSRG:   Real ZSRG(MZGRID), the coordinates of the nzsr z-levels
!             for the excitation sources.
!
!
!**** Called by:  main
!
!**** Calls:      none
!
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) ::  MZGRID,NTX_FD,NCRD,M_RX
      INTEGER, INTENT(IN) ::  N_RX(NTX_FD)
      INTEGER, INTENT(OUT) :: NZOB,NZSR
      REAL, INTENT(IN) :: DMIN,RX_Z(M_RX,NTX_FD),TX_CRDZ(NCRD,NTX_FD)
      REAL, INTENT(OUT) :: ZOBG(MZGRID),ZSRG(2,MZGRID)
!
      INTEGER ::  I,J,II,JJ
!
! --- ZWORK is an allocatable working array for sorting
!
      REAL, ALLOCATABLE :: ZWORK(:),ZWORK2(:,:)
!
! --- Determine the z-levels for the receivers
!
!     Firstly all different z-levels are found.  They are then
!     re-arranged later.
!
      ZOBG(1)=RX_Z(1,1)
      JJ=1
      DO J=1,NTX_FD
         DO I=1,N_RX(J)
            DO II=1,JJ
               IF (ABS(RX_Z(I,J)-ZOBG(II))<=DMIN) GOTO 2
            END DO
            JJ=JJ+1
            ZOBG(JJ)=RX_Z(I,J)
   2     CONTINUE
      END DO
      END DO
      NZOB=JJ
!
!  -- Arrange the elements of ZOBG in an increasing order.
!
!     This part is a bit tricky,  but it's organised as follows:
!     The Order_loop finds the (next) minimal ZOBG(:) and put them
!     in a increasing order;  the Minimization_loop identifies the
!     element that is smaller than the current one;  and the Skip_loop
!     skips those that have already been identified which are always
!     <= the current one.
!
!  -- Allocate the working array
!
      ALLOCATE(ZWORK(NZOB+1))
!
      ZWORK(1)=ZOBG(1)
      Order_loop:  DO I=1,NZOB
         Minimization_loop:  DO J=1,NZOB
            IF (ZOBG(J)<ZWORK(I)) THEN
               Skip_loop:  DO JJ=1,I-1
                  IF (ZOBG(J)<=ZWORK(JJ)) CYCLE Minimization_loop
               END DO Skip_loop
               ZWORK(I)=ZOBG(J)
            END IF
         END DO Minimization_loop
         ZWORK(I+1)=1.E20
      END DO Order_loop
!
      ZOBG(1:NZOB)=ZWORK(1:NZOB)
!
!  -- Deallocate the working array
!
      DEALLOCATE(ZWORK)
!
! --- Determine the z_prime levels for different excitations
!     in controlled source cases.
!
!     The technique used here is exactly the same as in the above
!
      ZSRG(1,1)=TX_CRDZ(1,1)
      ZSRG(2,1)=TX_CRDZ(1,1)
      JJ=1

      LP1: DO J=1,NTX_FD

         DO I=1,JJ
           IF (ABS(TX_CRDZ(1,J)-ZSRG(1,I))<=DMIN) CYCLE LP1
         END DO
         JJ=JJ+1
         ZSRG(1,JJ)=TX_CRDZ(1,J)
         ZSRG(2,JJ)=TX_CRDZ(1,J)
      END DO LP1
!
      NZSR=JJ
!
!  -- Allocate the working array
!
      ALLOCATE(ZWORK2(2,NZSR+1))
!
!  -- Arrange the elements of ZSRG in an increasing order according
!     to ZSRG(1,*).
!
      ZWORK2(1:2,1)=ZSRG(1:2,1)
      Order_loop2:  DO I=1,NZSR
         Minimization_loop2:  DO J=1,NZSR
            IF (ZSRG(1,J)<ZWORK2(1,I)) THEN
            Skip_loop2:  DO JJ=1,I-1
                  IF (ZSRG(1,J)<ZWORK2(1,JJ) .OR.                 &
                      (ABS(ZSRG(1,J)-ZWORK2(1,JJ))<DMIN.AND.      &
                       ABS(ZSRG(2,J)-ZWORK2(2,JJ))<DMIN))    THEN
                     Cycle Minimization_loop2
                  END IF
               END DO Skip_loop2
               ZWORK2(1:2,I)=ZSRG(1:2,J)
            END IF
         END DO Minimization_loop2
         ZWORK2(1,I+1)=1.E20
      END DO Order_loop2
!
      ZSRG(1:2,1:NZSR)=ZWORK2(1:2,1:NZSR)
!
      DEALLOCATE(ZWORK2)
!
      RETURN
!
   END SUBROUTINE INIT_ZLVLSCS
!
!
   SUBROUTINE INIT_ZLVLS(NLG,MBODY,NBMAX,NZMAX,KSMR,NBODY,SUB_BLOCK,       &
                         NZS,NZ,SBZ,SCZ,ZSB,ZCELL,BLZ,                    &
                         MZGRID,DMIN,NZSR,ZSRG,NZOB,ZOBG)
!
!**** Determine the z and z'-levels in the vertical direction
!
!     The program automatically finds all z and z'-levels for the
!     3D body and for the sources and receivers.  Hankel integrals
!     are computed at those levels for various rho (in the lateral
!     direction) for interpolations for computing the (integrated)
!     Green's tensors.
!
!  Input parameters:
!
!  MBODY:   Integer,  maximal number of substructures allowed in the
!           program.
!  NBMAX:   Integer,  maximal number of blocks allowed in a
!           substructure.  It is the maximum of SUB_BLOCK used for
!           array definitions.
!  NZMAX:   Integer,  maximal number of cells in the z-direction
!           in a block.
!  KSMR:    Integer,  controlling parameter whether spatial symmetry can
!           be used or not.  KSMR=1:  yes;  =0:  no.
!  NBODY:   Integer,  number of substructures.
!  SUB_BLOCK:  Integer SUB_BLOCK(MBODY),  number of blocks in each substructure.
!  NZS:     Integer,  number of cells in the z-direction of the superblock.
!  NZ:      Integer NZ(NBMAX,MBODY),  numbers of cells in the z-direction
!           in each block of the substructures.
!  SBZ:     Real,  dimension of the super block in the z-direction.
!  SCZ:     Real,  dimension the cells of the super block in the
!           z-direction.
!  ZSB:     Real,  z-ccordinate of the superblock.
!  ZCELL:   Real ZCELL(NZMAX,NBMAX,MBODY),  z-coordinates of the cells.
!  BLZ:     Real BLZ(NBMAX,MBODY),  dimensions of each cells of the
!           substructures in the z-direction.
!  MZRGID:  Integer,  dimension parameter for the arrays ZOBG and
!           ZSRG.  MZGRID must be much larger than NZTMAX since
!           these two arrays also works as working arrays.
!  DMIN:
!
!  Output parameters:
!
!     NZOB:   Integer, number of z-levels for the cells of the 3D bodies.
!     NZSR:   Integer,  number of z-levels for the 3D bodies.
!     ZOBG:   Real ZOBG(MZGRID),  the coordinates of the nzob z-levels
!             for the 3D bodies.
!     ZSRG:   Real ZSRG(MZGRID), the coordinates of the nzsr z-levels
!             for the cells of the 3D bodies.
!
!**** Called by:  main
!
!**** Calls:      none
!
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: KSMR,MBODY,MZGRID,NBMAX,NBODY,NZMAX,NZS,NLG,    &
                             SUB_BLOCK(MBODY),NZ(NBMAX,MBODY)
      REAL,    INTENT(IN) :: DMIN,SBZ,SCZ,ZSB,BLZ(NBMAX,MBODY),             &
                             ZCELL(NZMAX,NBMAX,MBODY)
      INTEGER, INTENT(OUT) :: NZOB,NZSR
      REAL,    INTENT(OUT) :: ZOBG(MZGRID),ZSRG(2,MZGRID)
!
      INTEGER :: I,II,J,JJ,IB
!
! --- ZWORK is an allocatable working array for sorting
!
      REAL, ALLOCATABLE :: ZWORK(:),ZWORK2(:,:)
!
! --- Life is easy with spatially symmetric models
!
      IF (KSMR==1) THEN
         NZOB=NZS
         NZSR=NZS
!
         DO I=1,NZS
            ZOBG(I)=ZSB-.5*SBZ+.5*SCZ+(I-1)*SCZ
            ZSRG(1,I)=ZSB-.5*SBZ+(I-1)*SCZ
            ZSRG(2,I)=ZSB-.5*SBZ+I*SCZ
         END DO
         RETURN
      END IF
!
! ----- Determine the z-levels for storing the values of the
!     Green's functions (the Hankel transforms) in the z-direction
!
!     Note that the z and z'-levels are different with and
!     without spatial symmetry reductions if there more than
!     two separated blocks.
!
!     Firstly all different z-levels are found.  They are then
!     re-arranged later.
!
      JJ=1
      ZOBG(1)=ZCELL(1,1,1)
      DO IB=1,NBODY
         DO I=1,SUB_BLOCK(IB)
            DO J=1,NZ(I,IB)
               DO II=1,JJ
                  IF (ABS(ZCELL(J,I,IB)-ZOBG(II))<=DMIN) GOTO 2
               END DO
               JJ=JJ+1
!--- The maximal value of JJ is MZGRID
               IF (JJ>MZGRID) THEN
                  WRITE (*,1010)
                  WRITE (NLG,1010)
                  STOP   &
       'Program aborted. Please check the error messages in MarcoAir.LOG'
               END IF
               ZOBG(JJ)=ZCELL(J,I,IB)
   2           CONTINUE
            END DO
         END DO
      END DO
      NZOB=JJ
!  -- Arrange the elements of ZOBG in an increasing order.
!
!     This part is a bit tricky,  but it's organised as follows:
!     The Order_loop finds the (next) minimal ZOBG(:) and put them
!     in a increasing order;  the Minimization_loop identifies the
!     element that is smaller than the current one;  and the Skip_loop
!     skips those that have already been identified which are always
!     <= the current one.
!
!  -- Allocate the working array
!
      ALLOCATE(ZWORK(NZOB+1))
!
      ZWORK(1)=ZOBG(1)
      Order_loop:  DO I=1,NZOB
         Minimization_loop:  DO J=1,NZOB
            IF (ZOBG(J)<ZWORK(I)) THEN
               Skip_loop:  DO JJ=1,I-1
                  IF (ZOBG(J)<=ZWORK(JJ)) CYCLE Minimization_loop
               END DO Skip_loop
               ZWORK(I)=ZOBG(J)
            END IF
         END DO Minimization_loop
         ZWORK(I+1)=1.E20
      END DO Order_loop
!
      ZOBG(1:NZOB)=ZWORK(1:NZOB)
!
!  -- Deallocate the working array
!
      DEALLOCATE(ZWORK)
!
!
! ---  Determine the z'-levels for storing the values of the
!      Green's functions integrated in the z'-direction
!
!     The technique used here is exactly the same as in the above
!
!
      JJ=1
      ZSRG(1,JJ)=ZCELL(1,1,1)-.5*BLZ(1,1)
      ZSRG(2,JJ)=ZCELL(1,1,1)+.5*BLZ(1,1)
      DO IB=1,NBODY
         DO I=1,SUB_BLOCK(IB)
            DO J=1,NZ(I,IB)
               DO II=1,JJ
                  IF (ABS(ZSRG(1,II)-ZCELL(J,I,IB)-.5*BLZ(I,IB))<=DMIN.AND. &
                      ABS(ZSRG(2,II)-ZCELL(J,I,IB)+.5*BLZ(I,IB))<=DMIN) GOTO 4
               END DO
               JJ=JJ+1
               ZSRG(1,JJ)=ZCELL(J,I,IB)-.5*BLZ(I,IB)
               ZSRG(2,JJ)=ZCELL(J,I,IB)+.5*BLZ(I,IB)
   4           CONTINUE
            END DO
         END DO
      END DO
      NZSR=JJ
!
!  -- Allocate the working array
!
      ALLOCATE(ZWORK2(2,NZSR+1))
      zwork2=0.
!
!  -- Arrange the elements of ZSRG in an increasing order according
!     to ZSRG(1,*).
!
      ZWORK2(1:2,1)=ZSRG(1:2,1)
      Order_loop2:  DO I=1,NZSR
         Minimization_loop2:  DO J=1,NZSR
            IF (ZSRG(1,J)<ZWORK2(1,I)) THEN
            Skip_loop2:  DO JJ=1,I-1
                  IF (ZSRG(1,J)<ZWORK2(1,JJ) .OR.                 &
                      (ABS(ZSRG(1,J)-ZWORK2(1,JJ))<DMIN.AND.      &
                       ABS(ZSRG(2,J)-ZWORK2(2,JJ))<DMIN))    THEN
                     Cycle Minimization_loop2
                  END IF
               END DO Skip_loop2
               ZWORK2(1:2,I)=ZSRG(1:2,J)
            END IF
         END DO Minimization_loop2
         ZWORK2(1,I+1)=1.E20
      END DO Order_loop2
!
      ZSRG(1:2,1:NZSR)=ZWORK2(1:2,1:NZSR)
!
      DEALLOCATE(ZWORK2)
!
      RETURN
!
 1010 FORMAT (/' The sum of all the cell numbers in the vertical',          &
               'direction of all blocks exceed designed limit.',           &
             'Please either change the value of the parameter MZGRID or',  &
             'try to use less blocks or reduce the cells in the vertical', &
             'direction.')
!
   END SUBROUTINE INIT_ZLVLS
!
!
!===== Subroutines called directly by the main program -- major structuring
!      of the program
!
      SUBROUTINE MAIN_PRM_AT_CELL(FRQ,MLAYER,ZBND,LRYTH,KKH,CDH,   &
                                  RMU,KSYMM,MBODY,          &
                                  NBMAX,NSUBCM,NMAX,NXMAX,NYMAX,NZMAX,    &
                                  NTX_FD,MD_ANGLE,NCRD,TX_CRDX,    &
                                  TX_CRDY,TX_CRDZ,TX_CRDXI,TX_CRDYI,  &
                                  TX_CRDZI,NBODY,SUB_BLOCK,               &
                                  NET,NEQ,NX,NY,NZ,NCELL,XCELL,YCELL,     &
                                  ZCELL,NXI,NYI,NZI,NCELLI,XCELLI,YCELLI, &
                                  ZCELLI,CDB,EN,EMT,ENT,EJGS,EJGS2,RHOMIN,&
                                  RHOMAX,NZOB,ZOBG,NZSR,ZSRG,ZBG,AJ, &
                                  DMIN,NHFILM,ALMAX,BLMIN,GRHF,RRG,GRHO0)
!
!**** calculate the incident fields at cell centres
!
!   Results (electric fields at cell centres) for non-symmetric
!   models are stored on the disk in unit 10 if there is more than
!   one excitation and/or more than one substructure.  Results for
!   for symmetric models are always stored in unit 20 on the disk.
!   Access to those units must be in the same sequence as
!   used here in this routine.
!
!
!Input parameters:
!
!  FRQ:     Real,  the frequency.
!  MLAYER:  Integer,  number of layers of the earth.
!  ZBND:    Real ZBND(0:MLAYER), the z-coordinates of the layer
!           boundaries with the air-earth interface being always
!           0 (zbnd(0)=0).
!  LRYTH:   Real LRYTH(MLAYER), the thickness of the layers.
!  KKH:    Complex(*8) KKH(0:MLAYER),  square ( **2) of the complex wave
!          number of the layers (in the lateral direction) including
!          the air (layer number 0).
!  CDH:    Complex CDH(0:MLAYER),  complex conductivities of
!          the layers in the lateral direction including the
!          air (layer 0).
!  RMU:    relative permeability
!  The above parameters FRQ etc. are to be passed into subroutines.
!  KSYMM:   Integer,  controlling parameter for the two-plane symmetry
!           for the 3D model.  KSYMM=1:  symmetric model;  =0:  non-sym-
!           metric model.
!  MBODY:   Integer,  maximal number of substructures allowed in the
!           program.
!  NBMAX:   Integer,  maximal number of blocks allowed in a
!           substructure.  It is the maximum of SUB_BLOCK used for
!           array definitions.
!  NSUBCM:  Integer,  maximal number of cells in a substructure.
!  NMAX:    Integer,  maximal number of unknowns in a substructure.
!           NMAX=3*NSUBCM.
!  NXMAX:   Integer,  maximal number of cells in the x-direction
!           in a block.
!  NYMAX:   Integer,  maximal number of cells in the y-direction
!           in a block.
!  NZMAX:   Integer,  maximal number of cells in the z-direction
!           in a block.
!  NTX_FD:    Integer,  number of excitations.
!  MD_ANGLE:  Real MD_ANGLE(2,NTX_FD),  orientations of arbitrary magnetic
!           dipoles for for each excitation.  For each excitation
!           the two elements of MD_ANGLE(1:2,J) contain the dip angle
!           MD_ANGLE(1,J) and the azimuthal angle MD_ANGLE(2,J) of the Jth
!           magnetic dipole.  The dip angle is the angle from the axis down
!           to the earth (note that this is the internal positive z-axis
!           which differs from the input-output z-axis that points upwards).
!           The azimuthal angle is the angle from the North,  the y-axis.
!           Thus for a vertical magnetic dipole MD_ANGLE(1,J)=PI for
!           pointing up air and MD_ANGLE(1,J) =0 for pointing down.  A
!           horizontal magnetic dipole pointing North will have MD_ANGLE(1,J) = PI/2
!           and MD_ANGLE(2,J)=0,  and a horizontal magnetic dipole
!           pointing East will have MD_ANGLE(1,J)=90 and MD_ANGLE(2,J)=PI/2.
!  NCRD:    Integer,  number of electrodes or number of different coordinates
!           for the source configuration per excitation.
!  TX_CRDX, TX_CRDY, TX_CRDZ: Real TX_CRDX(NCRD,NTX_FD), TX_CRDY(NCRD,NTX_FD),
!  TX_CRDZ(NCRD,NTX_FD):
!           coordinates of a magnetic dipole for all the excitations.
!  TX_CRDXI, TX_CRDYI, TX_CRDZI: Real TX_CRDXI(NCRD), TX_CRDYI(NCRD),
!  TX_CRDZI(NCRD):  the x, y, and z-coordinates of the sources for one
!           excitation.  They are used as working arrays.
!  NBODY:   Integer,  number of substructures.
!  SUB_BLOCK:  Integer SUB_BLOCK(MBODY),  number of blocks in each substructure.
!  NET:     Integer NET(MBODY),  number of unknowns in each substructure.
!  NEQ:     Integer,  total number of cells in one quadrant of a symmetric
!           model.
!  NX:      Integer NX(NBMAX,MBODY),  numbers of cells in the x-direction
!           in each block of the substructures.
!  NY:      Integer NY(NBMAX,MBODY),  numbers of cells in the y-direction
!           in each block of the substructures.
!  NZ:      Integer NZ(NBMAX,MBODY),  numbers of cells in the z-direction
!           in each block of the substructures.
!  NCELL:   Integer NCELL(NBMAX,MBODY),  number of cells in each block
!           of the substructures.
!  XCELL:   Real XCELL(NXMAX,NBMAX,MBODY),  x-coordinates of the cells.
!  YCELL:   Real YCELL(NYMAX,NBMAX,MBODY),  y-coordinates of the cells.
!  ZCELL:   Real ZCELL(NZMAX,NBMAX,MBODY),  z-coordinates of the cells.
!  NXI, NYI, NZI, NCELLI, XCELLI, YCELLI, and ZCELLI:   working arrays
!           corresponding to NX, NY, NZ, NCELL, XCELL, YCELL, and
!           ZCELL for each substructure.
!  CDB:     COMPLEX CDB(NSUBCM,NBMAX),  working array for the
!           complex conductivities of the cells in a substructure.
!           Its values are all stored on the disk in unit 62.
!  EN:      Complex EN(NMAX),  the incident electric fields in a
!           substructure. It is used as a working array.
!  DMIN:    Real,  minimal dimension value that is considered to be
!           zero.  DMIN=0.1 for the low frequency module and DMIN=0.001
!           for the high frequency module.
!
!  Parameters RHOMIN, RHOMAX, NZOB, ZOBG, NZSR, ZSRG, ZBG,
!  AJ, NHFILM, HF, HF1, BLMIN, GRHF, RRG, ALMAX, and GRHO0 are to be passed
!  into routine ONE_D_HF_TABLE.  Check ONE_D_HF_TABLE for descriptions
!
!Output parameter:
!
!
!  ENT:    Complex ENT(NMAX),  the incident electric fields in a
!          substructure multiplied by the conductivity difference
!          between the target and the host.  ENT won't be used in
!          this version.
!  EJGS:   Complex EJGS(4*NMAX*MBODY),  the incident electric fields
!          for all cells of a symmetric model with KSYMM=1.  Note
!          that the input data contain only the model in the first
!          quadrant,  but the actual computation is done for all the
!          body.
!  EJGS2:  Complex EJGS2(4*NMAX*MBODY),  the transformed incident
!          fields for the four block matrices of group reductions.
!
!
!**** Called by:  main
!
!**** Calls:      ONE_D_HF_TABLE,  EN_PRM,  EN_PRM_CS,  EN_PRM_GS,  and
!                 MTRX_UNITARY
!
!
      IMPLICIT NONE
!
      REAL AJ,ALMAX,BLMIN,DMIN,FRQ,RHOMAX,RHOMIN,U
      INTEGER I,IB,IEXCI,II,IQ,IR,J,JCOL,JJ,KEYG,KSYMM,LPC,LPR,NCRDI
      INTEGER LQC,LQR,MBODY,MLAYER,SUB_BLOCKI,NBMAX,NBODY,NCB1,NCRD,          &
              NEQ,NETI,NHFILM,NRG,NMAX,NSUBCM,NTX_FD,NXMAX,NYMAX
      INTEGER NZMAX,NZOB,NZSR
      COMPLEX EN(NMAX),EMT(NMAX),ENT(NMAX,NBODY),EJGS(4*NMAX*MBODY),          &
              EJGS2(4*NMAX*MBODY),CDH(0:MLAYER),KKH(0:MLAYER),                &
              CDB(NSUBCM,NBMAX),GRHF(11,NHFILM,NZSR,NZOB),GRHO0(4,NZSR,NZOB)
      REAL  ZBND(0:MLAYER),LRYTH(MLAYER),RMU(0:MLAYER)
      REAL  ZOBG(NZOB),ZSRG(2,NZSR),ZBG(2,NZSR),RRG(NHFILM)
      REAL XCELL(NXMAX,NBMAX,MBODY),YCELL(NYMAX,NBMAX,MBODY),                 &
           ZCELL(NZMAX,NBMAX,MBODY),XCELLI(NXMAX,NBMAX),                      &
           YCELLI(NYMAX,NBMAX),ZCELLI(NZMAX,NBMAX),                           &
           TX_CRDX(NCRD,NTX_FD),TX_CRDY(NCRD,NTX_FD),             &
           TX_CRDZ(NCRD,NTX_FD),TX_CRDXI(NCRD),TX_CRDYI(NCRD),TX_CRDZI(NCRD)
      INTEGER NX(NBMAX,MBODY),NY(NBMAX,MBODY),NZ(NBMAX,MBODY),                &
              NCELL(NBMAX,MBODY),NET(MBODY),SUB_BLOCK(MBODY),                 &
              NXI(NBMAX),NYI(NBMAX),NZI(NBMAX),NCELLI(NBMAX)
      REAL :: MD_ANGLE(2,NTX_FD)
      REAL :: ANGLES(2)
!
      KEYG=1
!
!
! ---- Compute the grid values for the interpolation of the
!    Hankel transforms in integrating current sources.
!
      CALL ONE_D_HF_TABLE(NHFILM,MLAYER,ZBND,LRYTH,KKH,RMU,        &
                  BLMIN,KEYG,DMIN,RHOMIN,RHOMAX,NZOB,ZOBG,     &
                  NZSR,ZSRG,ZBG,ALMAX,GRHF,RRG,NRG,GRHO0)
!
      DO IEXCI=1,NTX_FD
!
!-- Determine the orientation of the magnetic dipoles
!
         ANGLES(1:2)=MD_ANGLE(1:2,IEXCI)

         NCRDI=NCRD
         DO I=1,NCRDI

            TX_CRDXI(I)=TX_CRDX(I,IEXCI)
            TX_CRDYI(I)=TX_CRDY(I,IEXCI)
            TX_CRDZI(I)=TX_CRDZ(I,IEXCI)
         END DO
!
         IF (KSYMM==0) THEN
! *********************************
! *                               *
            DO IB=1,NBODY
               SUB_BLOCKI=SUB_BLOCK(IB)
               NETI=NET(IB)
               DO I=1,SUB_BLOCKI
                  NXI(I)=NX(I,IB)
                  NYI(I)=NY(I,IB)
                  NZI(I)=NZ(I,IB)
                  NCELLI(I)=NCELL(I,IB)
                  DO J=1,NXI(I)
                     XCELLI(J,I)=XCELL(J,I,IB)
                  END DO
                  DO J=1,NYI(I)
                     YCELLI(J,I)=YCELL(J,I,IB)
                  END DO
                  DO J=1,NZI(I)
                     ZCELLI(J,I)=ZCELL(J,I,IB)
                  END DO
               END DO
!
               CALL EN_PRM_CS(NETI,SUB_BLOCKI,NSUBCM,NXMAX,NYMAX,NZMAX, &
                          NXI,NYI,NZI,NCELLI,XCELLI,YCELLI,ZCELLI,CDB,  &
                          NCRDI,TX_CRDXI,TX_CRDYI,TX_CRDZI,     &
                          EN,EMT,FRQ,MLAYER,ZBND,KKH,CDH,RMU,      &
                          ANGLES,KEYG,AJ,NZOB,ZOBG,NZSR,ZSRG,      &
                          RHOMIN,NHFILM,RRG,NRG,GRHF,GRHO0)
!
               DO J=1,NETI
                  ENT(J,IB)=EN(J)
               END DO
!
            END DO
!
! --- Store ent in case of multiple excitations
!
            IF (NTX_FD>1) THEN
               DO IB=1,NBODY
                  WRITE (10) (ENT(I,IB),I=1,NET(IB))
               END DO
            END IF
! *                               *
! *********************************
         END IF
!
         IF (KSYMM==1) THEN
! *********************************
! *                               *
            CALL EN_PRM_GS(NEQ,NBODY,SUB_BLOCK,NBMAX,NXMAX,NYMAX,NZMAX,NET, &
                       NX,NY,NZ,NCELL,XCELL,YCELL,ZCELL,NCRDI,      &
                       TX_CRDXI,TX_CRDYI,TX_CRDZI,EJGS,FRQ,MLAYER,     &
                       ZBND,KKH,CDH,RMU,ANGLES,KEYG,AJ,NZOB,ZOBG,      &
                       NZSR,ZSRG,RHOMIN,NHFILM,RRG,NRG,GRHF,GRHO0)
!
! --- The arrays EJGS and EJGS2 are very big.  They share the
!   memory with GA and GHF2 etc.  The unitary transforms
!   require both EJGS and EJGS2. But the array GHF2 storing
!   the grid values of the Hankel transforms is still in use
!   for computing the incident fields.  Thus EJGS must be
!   stored for now.
!
            WRITE (20) (EJGS(I),I=1,4*NEQ)
! *                               *
! *********************************
         END IF
!
      END DO
!
! --- Transform the incident field by the unitary matrix for
!   symmetrical model under controlled source excitations
!
      IF (KSYMM==1) THEN
!
         REWIND (20)
!
         DO IEXCI=1,NTX_FD
!
            READ (20) (EJGS(I),I=1,4*NEQ)
!
            DO LPR=1,NEQ/3
               DO LQR=1,4
                  DO II=1,3
                     IR=NEQ*(LQR-1)+3*(LPR-1)+II
                     EJGS2(IR)=(0.,0.)
                     DO LPC=1,NEQ/3
                        DO LQC=1,4
                           DO JJ=1,3
!
!--- Note that the entries of the unitary matrix for
!    lpr .ne. lpc and ii .ne. jj are zero
!
                              IF (LPR==LPC.AND.II==JJ) THEN
                              CALL MTRX_UNITARY(LPR,LQR,II,LPC,LQC,JJ,U)
!--- PMAX=NEQ/3.  JCOL is not computed in MTRX_UNITARY
                              JCOL=12*(LPC-1)+3*(LQC-1)+JJ
                              EJGS2(IR)=EJGS2(IR)+U*EJGS(JCOL)
                              END IF
                           END DO
                        END DO
                     END DO
                  END DO
               END DO
            END DO
!
! ---  Store EJGS2 in four files which correspond to the four
!    block matrices
!
!    The do loop IB is necessary because the incident fields
!    will be read so during system iteration later on
!
            NCB1=0
            DO IB=1,NBODY
               IF (IB>1) NCB1=NCB1+NET(IB-1)
               DO IQ=1,4
                  WRITE (40+IQ) (EJGS2(NCB1+I+NEQ*(IQ-1)),I=1,NET(IB))
               END DO
            END DO
!
         END DO
!
      END IF
!
      IF (KSYMM==1) THEN
         REWIND (20)
         DO IQ=1,4
            REWIND (40+IQ)
         END DO
      ELSE IF (NTX_FD>1) THEN
         REWIND (10)
      END IF
!
      RETURN
      END
!
!**** End of MAIN_PRM_AT_CELL
!
!
      SUBROUTINE MAIN_SUPER_GRID(GSB1,CLMN2,SBX,SBY,SBZ,NXS,NYS,       &
                                 NZS,XSB,YSB,ZSB,NSMR,FRQ,MLAYER,ZBND,       &
                                 LRYTH,KKH,CDH,RMU,NZOB,ZOBG,NZSR,ZSRG,  &
                                 DMIN,NHFILM,RHOMIN,RHOMAX,RRG,RRG3,GRHF,    &
                                 GRHF3,GRHO0,GRHO03,ALMAX,KSFT,KCLMN,        &
                                 BLMIN,KACC,KUTCRP)
!
!**** Compute the nonidentical elements of the scattering matrix for the
!   use of spatial symmetry reduction.  This must be an independent
!   subroutine called directly by the main program because of memory
!   management.  See comments about the definition of the arrays
!   GSB1 and GSB2 in the main program for detail.
!
!
!  SBX,SBY,SBZ,NXS,NYS,NZS,XSB,YSB,ZSB,NSMR,GSB1 are to be passed
!  into routine GSB
!
!
!
!**** Called by:  main
!
!**** Calls:      THR_D_HF_TABLE,  GSB,
!
!
      IMPLICIT NONE
!
      REAL ALMAX,BLMIN,CLMN2,DMIN,FRQ,RHOMAX,RHOMIN,                        &
           SBX,SBY,SBZ,XSB,YSB,ZSB
      INTEGER KACC,KCLMN,KEYG,KSFT,KUTCRP,MLAYER,NHFILM,  &
              NRG,NRG3,NSMR,NXS,NYS,NZOB,NZS,NZSR
      COMPLEX GSB1(NSMR/3,3),GRHF(11,NHFILM,NZSR,NZOB),                     &
              GRHF3(11,NHFILM,NZSR),GRHO0(4,NZSR,NZOB),                     &
              GRHO03(4,NZSR),KKH(0:MLAYER),CDH(0:MLAYER)
      REAL ZOBG(NZOB),ZSRG(2,NZSR),RRG(NHFILM),RRG3(NHFILM),                &
           LRYTH(MLAYER),ZBND(0:MLAYER),RMU(0:MLAYER)
!
! ---- Compute the grid values for the interpolation of the
!    Hankel transforms
!
      KEYG=1
!
      CALL THR_D_HF_TABLE(NHFILM,MLAYER,ZBND,LRYTH,KKH,RMU,           &
                          KSFT,BLMIN,KEYG,DMIN,RHOMIN,RHOMAX,NZOB,ZOBG,   &
                          NZSR,ZSRG,ALMAX,RRG,NRG,RRG3,NRG3,GRHF,GRHF3,   &
                          GRHO0,GRHO03)
!
! ---- Compute the scattering matrix elements of the super block
!    used in spatial symmetry reduction.
!
      CALL MTRX_SPSGSB(SBX,SBY,SBZ,NXS,NYS,NZS,XSB,YSB,ZSB,NSMR,GSB1,FRQ,     &
                       MLAYER,ZBND,CDH,RMU,NZOB,ZOBG,NZSR,ZSRG,RHOMIN,    &
                       DMIN,NHFILM,RRG,NRG,GRHF,RRG3,NRG3,GRHF3,GRHO0,GRHO03, &
                       CLMN2,KCLMN,BLMIN,KACC,KUTCRP)
!
      RETURN
      END
!
!**** End of MAIN_SUPER_GRID
!
!
      SUBROUTINE MAIN_MATRICES(NLG,KSMR,CLMN,KSYMM,KCOND,GA,GSB2,IND,VV,GAI,EN,                &
                               CDB,MBODY,NBMAX,NXMAX,NYMAX,NZMAX,NSUBCM,         &
                               NBODY,SUB_BLOCK,NET,NX,NY,NZ,NCELL,BLX,BLY,       &
                               BLZ,XCELL,YCELL,ZCELL,KCELL,NEQ,NMAX,NXI,NYI,     &
                               NZI,NCELLI,BLXI,BLYI,BLZI,XCELLI,YCELLI,          &
                               ZCELLI,NXJ,NYJ,NZJ,NCELLJ,XCELLJ,YCELLJ,          &
                               ZCELLJ,SBX,SBY,SBZ,NXS,NYS,NZS,ZSB,NSMR,FRQ,      &
                               MLAYER,ZBND,LRYTH,KKH,RMU,CDH,NZOB,ZOBG,  &
                               NZSR,ZSRG,DMIN,NHFILM,RHOMIN,RHOMAX,RRG,RRG3,     &
                               GRHF,GRHF3,GRHO0,GRHO03,ALMAX,KSFT,KCLMN,         &
                               BLMIN,KACC,KUTCRP)
!
!**** Form the scattering matrices
!
!
!
!  Parameters NHFILM, LRYTH,
!                  KSFT,BLMIN,DMIN,RHOMIN,RHOMAX,
!                 NZOB,ZOBG,NZSR,ZSRG,ALMAX,
!                 RRG,NRG,RRG3,NRG3,GRHF,GRHF3,GRHO0,GRHO03
!  are to be passed into routine THR_D_HF_TABLE,  check THR_D_HF_TABLE for
!  their descriptions.
!
!  SBX,SBY,SBZ,NXS,NYS,NZS,ZSB,NSMR,GSB1 are to be passed
!  into routine GSB
!
!  commonly:
!  FRQ,MLAYER,ZBND,KKH,BLMIN,NZOB,ZOBG,NZSR,ZSRG,
!  RRG,NRG,DMIN,RHOMIN,NHFILM,GRHF,GRHF3,GRHO0,GRHO03
!
!
!**** Called by:  main
!
!**** Calls:  THR_D_HF_TABLE,  GSB,  MTRX_SPS,  MTRX_SPS1B,  MTRX_1MS,  MTRX_1M,
!             MTRX_SLV_FACTOR,  MTRX_SPSS,  MTRX_3MS,  MTRX_3M,  MTRX_GS,
!             MTRX_SPSM,  MTRX_M,  MTRX_GS,  MTRX_SPSSM,  MTRX_MS,
!
!
      IMPLICIT NONE
!
      REAL AEM,ALMAX,ANORM1,ANORM3,BLMIN,CLMN2,COND1,COND3,DMIN,FRQ,          &
           RHOMAX,RHOMIN,SBX,SBY,SBZ,ZSB
      INTEGER I,IB,II,IQ,J,JB,JJ,KACC,KCLMN,KCOND,   &
              KEYG,KK,KS,KSFT,KSMR,KSYMM,NLG
      INTEGER KUTCRP,LL,MBODY,MLAYER,N2,SUB_BLOCKI,SUB_BLOCKJ,NBMAX,NBODY,    &
              NCB1,NEQ,NETI,NETJ,NHFILM,NMAX,NRG,NRG3,NSMR,NSUBCM
      INTEGER NXMAX,NXS,NYMAX,NYS,NZMAX,NZOB,NZS,NZSR
      COMPLEX GA(NMAX,NMAX),GSB2(NSMR/3,3)
!
      REAL  VV(NMAX),GAI(NMAX)
      INTEGER IND(NMAX)
      COMPLEX EN(NMAX)
      INTEGER NX(NBMAX,MBODY),NY(NBMAX,MBODY),NZ(NBMAX,MBODY),          &
              NCELL(NBMAX,MBODY),                                       &
              NXI(NBMAX),NYI(NBMAX),NZI(NBMAX),NXJ(NBMAX),              &
              NYJ(NBMAX),NZJ(NBMAX),NCELLI(NBMAX),NCELLJ(NBMAX),        &
              NET(MBODY),KCELL(MBODY),SUB_BLOCK(MBODY)
      REAL BLX(NBMAX,MBODY),BLY(NBMAX,MBODY),BLZ(NBMAX,MBODY),          &
           XCELL(NXMAX,NBMAX,MBODY),YCELL(NYMAX,NBMAX,MBODY),           &
           ZCELL(NZMAX,NBMAX,MBODY)
      REAL BLXI(NBMAX),BLYI(NBMAX),BLZI(NBMAX),                         &
           XCELLI(NXMAX,NBMAX),YCELLI(NYMAX,NBMAX),                     &
           ZCELLI(NZMAX,NBMAX),XCELLJ(NXMAX,NBMAX),                     &
           YCELLJ(NYMAX,NBMAX),ZCELLJ(NZMAX,NBMAX)
      COMPLEX GRHF(11,NHFILM,NZSR,NZOB),GRHF3(11,NHFILM,NZSR),          &
              GRHO0(4,NZSR,NZOB),GRHO03(4,NZSR),                        &
              KKH(0:MLAYER),CDH(0:MLAYER)
      COMPLEX CDB(NSUBCM,NBMAX)
      REAL ZOBG(NZOB),ZSRG(2,NZSR),RRG(NHFILM),RRG3(NHFILM),            &
           LRYTH(MLAYER),ZBND(0:MLAYER),RMU(0:MLAYER)
      REAL CLMN(NBODY)
!
      complex, allocatable :: g12(:,:)

      allocate (g12(nmax,12))
!
! ---- Compute the grid values for the interpolation of the
!    Hankel transforms if KSMR=0
!
      IF (KSMR==0) THEN
!
         KEYG=1
!
         CALL THR_D_HF_TABLE(NHFILM,MLAYER,ZBND,LRYTH,KKH,RMU,         &
                             KSFT,BLMIN,KEYG,DMIN,RHOMIN,RHOMAX,NZOB,ZOBG, &
                             NZSR,ZSRG,ALMAX,RRG,NRG,RRG3,NRG3,GRHF,GRHF3, &
                             GRHO0,GRHO03)
!
      END IF
!
! --- compute the diagonal submatrices
!
      DO IB=1,NBODY
         SUB_BLOCKI=SUB_BLOCK(IB)
         NETI=NET(IB)
         DO I=1,SUB_BLOCKI
            NXI(I)=NX(I,IB)
            NYI(I)=NY(I,IB)
            NZI(I)=NZ(I,IB)
            NCELLI(I)=NCELL(I,IB)
            BLXI(I)=BLX(I,IB)
            BLYI(I)=BLY(I,IB)
            BLZI(I)=BLZ(I,IB)
            DO J=1,NXI(I)
               XCELLI(J,I)=XCELL(J,I,IB)
            END DO
            DO J=1,NYI(I)
               YCELLI(J,I)=YCELL(J,I,IB)
            END DO
            DO J=1,NZI(I)
               ZCELLI(J,I)=ZCELL(J,I,IB)
            END DO
         END DO
! --- Read cell conductivity from file CONDT (unit 62)
         READ (62) ((CDB(J,I),J=1,NCELL(I,IB)),I=1,SUB_BLOCK(IB))
!
         CLMN2=CLMN(IB)
!
!        If the scattering matrix is symmetric, KS = 1
         KS=0
         IF (KCELL(IB)==1) KS=1
!
         IF (KSYMM==1) THEN
!
! -------- Symmetric structure (two plane symmetries) with
!        arbitrary excitations (CSAMT).
!
!        Block diagonalization using group theory.
!        Note that all the four block matrices obtained
!        by block diagonalization are further partitioned
!        into block submatrices to be used by system
!        iteration.  Here the diagonal submatrices for
!        each of the four block matrices are computed.
!
!        In order to save memory as much as possible,
!        only one column for each of the four block
!        matrices is computed at a time which is
!        then stored.
!
            NCB1=0
            DO LL=1,SUB_BLOCKI
               IF (LL>1) NCB1=NCB1+NCELLI(LL-1)
               DO II=1,NXI(LL)
                  DO JJ=1,NYI(LL)
                     DO KK=1,NZI(LL)
!
                        N2=(II-1)*NYI(LL)*NZI(LL)+(JJ-1)*NZI(LL)+KK+NCB1
!
                        CALL MTRX_GS(KSMR,NSMR,NXS,NYS,NZS,SBX,SBY,SBZ,ZSB,     &
                                   KS,1,12,N2,BLXI(LL),BLYI(LL),BLZI(LL),       &
                                   XCELLI(II,LL),YCELLI(JJ,LL),ZCELLI(KK,LL),   &
                                   NMAX,NXMAX,NYMAX,NZMAX,NXI,NYI,NZI,NCELLI,   &
                                   NSUBCM,SUB_BLOCKI,XCELLI,YCELLI,ZCELLI,      &
                                   CDB,G12,GSB2,FRQ,MLAYER,ZBND,RMU,CDH, &
                                   NZOB,ZOBG,NZSR,ZSRG,RHOMIN,DMIN,NHFILM,      &
                                   RRG,NRG,GRHF,RRG3,NRG3,GRHF3,GRHO0,GRHO03,   &
                                   CLMN2,KCLMN,BLMIN,KACC,KUTCRP)
!
! --- Store the four submatrices
!
                        DO IQ=1,4
                           DO J=1,3
                              WRITE (31+IQ) (G12(I,3*(IQ-1)+J),I=1,NETI)
                   !           WRITE (31+IQ) (GA(I,3*(IQ-1)+J),I=1,NETI)
                           END DO
                        END DO
!
                     END DO
                  END DO
               END DO
            END DO
!
         ELSE
!
! -------- General, non-symmetric structure
!
            IF (KS/=1) THEN
! ----  Non-symmetric scattering matrix
               CALL MTRX_1M(NMAX,NXMAX,NYMAX,NZMAX,NXI,NYI,NZI,      &
                          NCELLI,NSUBCM,SUB_BLOCKI,XCELLI,YCELLI,ZCELLI,  &
                          BLXI,BLYI,BLZI,CDB,GA,FRQ,MLAYER,ZBND,RMU,CDH,  &
                          NZOB,ZOBG,NZSR,ZSRG,RHOMIN,DMIN,        &
                          NHFILM,RRG,NRG,GRHF,RRG3,NRG3,GRHF3,GRHO0,      &
                          GRHO03,CLMN2,KCLMN,BLMIN,KACC,KUTCRP)
! ----  Symmetric scattering matrix
            ELSE IF (KSMR==1) THEN
               CALL MTRX_SPS(NSMR,MLAYER,ZBND,CDH,SBX,SBY,SBZ,NXS,NYS,NZS, &
                          ZSB,NMAX,NXMAX,NYMAX,NZMAX,NXI,NYI,NZI,NCELLI,      &
                          NSUBCM,SUB_BLOCKI,XCELLI,YCELLI,ZCELLI,CDB,GSB2,GA)
            ELSE IF (SUB_BLOCKI==1) THEN
!--- Use the spatial symmetry reduction for a single block
!    substructure if it cannot be use for the whole structure
               CALL MTRX_SPS1B(NMAX,NXMAX,NYMAX,NZMAX,NXI,NYI,NZI,NSUBCM,&
                          SUB_BLOCKI,XCELLI,YCELLI,ZCELLI,BLXI,BLYI,BLZI,   &
                          CDB,GA,FRQ,MLAYER,ZBND,RMU,CDH,NZOB,ZOBG, &
                          NZSR,ZSRG,RHOMIN,DMIN,NHFILM,RRG,NRG,GRHF,RRG3,   &
                          NRG3,GRHF3,GRHO0,GRHO03,CLMN2,KCLMN,BLMIN,KACC,   &
                          KUTCRP)
            ELSE
               CALL MTRX_1MS(NMAX,NETI,NXMAX,NYMAX,NZMAX,NXI,NYI,NZI,NCELLI,   &
                             NSUBCM,SUB_BLOCKI,XCELLI,YCELLI,ZCELLI,BLXI,      &
                             BLYI,BLZI,CDB,GA,FRQ,MLAYER,ZBND,RMU,CDH, &
                             NZOB,ZOBG,NZSR,ZSRG,RHOMIN,DMIN,NHFILM,RRG,NRG,   &
                             GRHF,RRG3,NRG3,GRHF3,GRHO0,GRHO03,CLMN2,KCLMN,    &
                             BLMIN,KACC,KUTCRP)
            END IF
!
! ---- Factor the matrix with condition number computation.
!    EN works here as a working array only
!
            CALL MTRX_SLV_FACTOR(NLG,GA,NMAX,NETI,IND,VV,EN,GAI,KS,KCOND,     &
                                 ANORM1,ANORM3,COND1,COND3,AEM)
!
!--- Store ind in file index1 in unit 63
!
            WRITE (63) (IND(I),I=1,NETI)
!
            IF (KCOND==1) THEN
               WRITE (NLG,1050) IB,ANORM1,ANORM3
               WRITE (NLG,1060) COND1,COND3,AEM
            END IF
!
! --- Store the factored matrix in unit 31
!   The do loop is used to reduced the stacks for execution
!
!   Note that if nbody = 1 the array ga needs not to be stored
!
            IF (NBODY>1) THEN
               DO I=1,NETI
                  WRITE (31) (GA(J,I),J=1,NETI)
               END DO
!
            END IF
         END IF
!
      END DO
!
      IF (KSYMM==1) THEN
         DO IQ=1,4
            REWIND (31+IQ)
         END DO
      ELSE
         IF (KSYMM==1.OR.NBODY>1) REWIND (31)
      END IF
!
      IF (KSYMM/=0) THEN
!
! --- Read the submatrices in units 32, 33, 34, and 35 and
!   factorise them and store them in unit 31, 32, 33, and 34
!
         DO IQ=1,4
!
            IF (KCOND==1) WRITE (NLG,1070) IQ
!
            DO IB=1,NBODY
               NETI=NET(IB)
!
!              If the block matrix is symmetric, KS = 1
               KS=0
               IF (KCELL(IB)==1) KS=1
!
               DO I=1,NETI
                  READ (31+IQ) (GA(J,I),J=1,NETI)
               END DO
!
! ---- Determine the lower triangles of the block matrices
!    for symmetric matrices
!
               IF (KS==1) THEN
                  DO I=1,NETI
                     DO J=1,NETI
                        IF (J<I) GA(J,I)=GA(I,J)
                     END DO
                  END DO
               END IF
!
! ---- Factor the matrix with condition number computation.
!    EN works here as a working array only
!
               CALL MTRX_SLV_FACTOR(NLG,GA,NMAX,NETI,IND,VV,EN,GAI,KS,KCOND,    &
                                    ANORM1,ANORM3,COND1,COND3,AEM)
!
!--- Store ind in files index1, index2, index3, or index4
!    in units 63, 64, 65, or 66 according to the value of IQ
!
               WRITE (62+IQ) (IND(I),I=1,NETI)
!
               IF (KCOND==1) THEN
                  IF (NBODY==1) WRITE (NLG,1080) ANORM1,ANORM3
                  IF (NBODY>1) WRITE (NLG,1090) IB,ANORM1,ANORM3
                  WRITE (NLG,1100) COND1,COND3,AEM
               END IF

! --- Store the factored matrix in unit 31, 32, 33, and 34

               DO I=1,NETI
                  WRITE (30+IQ) (GA(J,I),J=1,NETI)
               END DO
!
            END DO
!
            REWIND (31+IQ)
!
         END DO
!
         DO IQ=1,4
            REWIND (30+IQ)
         END DO
      END IF
!
      REWIND (62)
!
      DO IQ=1,4
         REWIND (62+IQ)
      END DO
!
! ---------- Form the mutual-coupling, or off-diagonal matrices.
!          Note that the interaction matrices between two
!          substructures, ib and jb, are assumed to be
!          non-symmetric, i.e., the reciprocity do not hold
!          true due to different cell sizes in general cases.
!
!---- For efficiency in array references in interpolation,
!     jb is designed to be the out-loop here
!
!
!---- In case of spatial symmetry reductions,  the mutual
!     coupling matrices need not be formed here.  They will
!     be computed in each iteration in order to save both
!     the disk storage requirement and the disk access time.
!     For symmetric bodies with group reduction the computation
!     time needed for complete the block diagonalizations is
!     too much to be economical.  Thus the mutual interaction
!     matrices will be always stored unless the total disk
!     storage requirements exceed ca. 200 M-bytes, i.e., about
!     800 cells per quadrant.
!
!
      IF (.NOT.(NBODY==1.OR.KSMR==1.AND.KSYMM==0.OR.  &
          KSMR==1.AND.KSYMM==1.AND.NEQ/3>800)) THEN
!
         DO JB=1,NBODY
!
            SUB_BLOCKJ=SUB_BLOCK(JB)
            NETJ=NET(JB)
            DO I=1,SUB_BLOCKJ
               NXJ(I)=NX(I,JB)
               NYJ(I)=NY(I,JB)
               NZJ(I)=NZ(I,JB)
               NCELLJ(I)=NCELL(I,JB)
               DO J=1,NXJ(I)
                  XCELLJ(J,I)=XCELL(J,I,JB)
               END DO
               DO J=1,NYJ(I)
                  YCELLJ(J,I)=YCELL(J,I,JB)
               END DO
               DO J=1,NZJ(I)
                  ZCELLJ(J,I)=ZCELL(J,I,JB)
               END DO
            END DO
!
            DO IB=1,NBODY
!
               SUB_BLOCKI=SUB_BLOCK(IB)
               NETI=NET(IB)
               DO I=1,SUB_BLOCKI
                  NXI(I)=NX(I,IB)
                  NYI(I)=NY(I,IB)
                  NZI(I)=NZ(I,IB)
                  NCELLI(I)=NCELL(I,IB)
                  BLXI(I)=BLX(I,IB)
                  BLYI(I)=BLY(I,IB)
                  BLZI(I)=BLZ(I,IB)
                  DO J=1,NXI(I)
                     XCELLI(J,I)=XCELL(J,I,IB)
                  END DO
                  DO J=1,NYI(I)
                     YCELLI(J,I)=YCELL(J,I,IB)
                  END DO
                  DO J=1,NZI(I)
                     ZCELLI(J,I)=ZCELL(J,I,IB)
                  END DO
               END DO
!
               CLMN2=CLMN(IB)
!
               IF (IB/=JB) THEN
!
                  IF (KSYMM==1) THEN
!
! ---- Group reduction for controlled source problems
!
                     NCB1=0
                     DO LL=1,SUB_BLOCKI
                        IF (LL>1) NCB1=NCB1+NCELLI(LL-1)
                        DO II=1,NXI(LL)
                           DO JJ=1,NYI(LL)
                              DO KK=1,NZI(LL)
!
                              N2=(II-1)*NYI(LL)*NZI(LL)+(JJ-1)*NZI(LL)   &
                                 +KK+NCB1
!
! ---   cdb is not used and is thus arbitrary
!
                              CALL MTRX_GS(KSMR,NSMR,NXS,NYS,NZS,SBX,SBY,SBZ,   &
                                         ZSB,0,0,12,N2,BLXI(LL),BLYI(LL),       &
                                         BLZI(LL),XCELLI(II,LL),YCELLI(JJ,LL),  &
                                         ZCELLI(KK,LL),NMAX,NXMAX,NYMAX,NZMAX,  &
                                         NXJ,NYJ,NZJ,NCELLJ,NSUBCM,SUB_BLOCKJ,  &
                                         XCELLJ,YCELLJ,ZCELLJ,CDB,G12,GSB2,FRQ,  &
                                         MLAYER,ZBND,RMU,CDH,NZOB,ZOBG, &
                                         NZSR,ZSRG,RHOMIN,DMIN,NHFILM,RRG,NRG,  &
                                         GRHF,RRG3,NRG3,GRHF3,GRHO0,GRHO03,     &
                                         CLMN2,KCLMN,BLMIN,KACC,KUTCRP)
!
! --- Store the four submatrices
!
!   Note that the submatrices as computed by the routines
!   MTRX_SPSM, MTRX_M, MTRX_SPSSM, MTRX_MS, MTRX_SPSSM, and MTRX_MS
!   have their sign reversed as compared with the results
!   here.  Thus -GA is stored in order to use the same
!   algorithm for system iteration
!
                              DO IQ=1,4
                              DO J=1,3
                              WRITE (34+IQ) (-G12(I,3*(IQ-1)+J),I=1,NETJ)
                      !        WRITE (34+IQ) (-GA(I,3*(IQ-1)+J),I=1,NETJ)
                              END DO
                              END DO
!
                              END DO
                           END DO
                        END DO
!
                     END DO
!
                  ELSE
!
                     IF (KSMR==1) THEN
                        CALL MTRX_SPSM(SBX,SBY,SBZ,NXS,NYS,NZS,ZSB,NMAX,     &
                           NXMAX,NYMAX,NZMAX,NXJ,NYJ,NZJ,NCELLJ,SUB_BLOCKJ,  &
                           XCELLJ,YCELLJ,ZCELLJ,NXI,NYI,NZI,NCELLI,          &
                           SUB_BLOCKI,XCELLI,YCELLI,ZCELLI,GA,NSMR,GSB2)
                     ELSE
                        CALL MTRX_M(NMAX,NXMAX,NYMAX,NZMAX,NXJ,NYJ,    &
                           NZJ,NCELLJ,SUB_BLOCKJ,XCELLJ,YCELLJ,ZCELLJ,NXI,  &
                           NYI,NZI,NCELLI,SUB_BLOCKI,XCELLI,YCELLI,ZCELLI,  &
                           BLXI,BLYI,BLZI,GA,FRQ,MLAYER,ZBND,RMU,CDH,   &
                           NZOB,ZOBG,NZSR,ZSRG,RHOMIN,DMIN,                 &
                           NHFILM,RRG,NRG,GRHF,RRG3,NRG3,GRHF3,GRHO0,       &
                           GRHO03,CLMN2,KCLMN,BLMIN,KACC,KUTCRP)
                     END IF
!
! ---- Store the interaction matrices in GREEN2
!
                     DO I=1,NETI
                        WRITE (32) (GA(J,I),J=1,NETJ)
!
                     END DO
                  END IF
               END IF
!
            END DO
         END DO
!
         IF (KSYMM==1) THEN
            DO IQ=1,4
               REWIND (34+IQ)
            END DO
         ELSE
            REWIND (32)
            IF (KSYMM==1) REWIND (34)
         END IF
      END IF
!
      deallocate (g12)

      RETURN
 1050 FORMAT (/' Maximal column and maximal row norms of the ',           &
              'scattering matrix',/'  for substructure #',I3,':',16X,    &
              2E15.7)
 1060 FORMAT (' Condition numbers after these two norms:',2E15.7,         &
              /' Maximal diagonal element of the matrix:',E16.7)
 1070 FORMAT (/' Norms and condition numbers of the ',                    &
              'submatrices for quadrant',I2)
 1080 FORMAT (/'    Maximal column and row norms:           ',2E15.7)
 1090 FORMAT ('  Substructure #',I3,':',                                 &
              /'    Maximal column and row norms:           ',2E15.7)
 1100 FORMAT ('    Condition numbers after these two norms:',2E15.7,     &
              /'    Maximal diagonal element of the matrix:',E16.7)
      END
!
!**** End of MAIN_MATRICES
!
!
      SUBROUTINE MAIN_SOLVER(NLG,GA,IND,EMT,EN,ENT,NET,JST,EJGS,EJGS2,        &
                             KACC,KSMR,KSYMM,NTX_FD,    &
                             MBODY,NBMAX,NXMAX,NYMAX,NZMAX,     &
                             NMAX,NBODY,SUB_BLOCK,NX,NY,NZ,NCELL,NCT,KCELL,  &
                             XCELL,YCELL,ZCELL,BLX,BLY,BLZ,NXI,NYI,NZI,      &
                             NCELLI,XCELLI,YCELLI,ZCELLI,BLXI,BLYI,BLZI,     &
                             NXJ,NYJ,NZJ,NCELLJ,XCELLJ,YCELLJ,ZCELLJ,        &
                             SBX,SBY,SBZ,NXS,NYS,NZS,ZSB,NSMR,GSB2,NEQ)
!
!--- NITMAX is the maximal number of iteration allowed in the
!    program.  It is also the maximal length of the array ERR.
!
      IMPLICIT NONE
!
      REAL EPS,FRACCN,SBX,SBY,SBZ,TJS,U,ZSB
      INTEGER I,IB,IEXCI,II,IQ,IROW,J,JB,JC,JJ,KACC,KSMR,KSYMM,LPC,NLG
      INTEGER LPR,LQC,LQR,MBODY,SUB_BLOCKI,SUB_BLOCKJ,NBMAX,NBODY,NCB1,NCMAX,&
              NEQ,NERR,NETI,NETJ,NITER,NITM,NITMAX,NMAX,NQ
      INTEGER NSMR,NUNIT,NTX_FD,NXMAX,NXS,NYMAX,NYS,NZMAX,NZS
      PARAMETER (NITMAX=500)
!
      REAL ERR(NITMAX)
!
      COMPLEX GA(NMAX,NMAX)

!**** Only one of them
      COMPLEX GSB2(NSMR/3,3)
!   COMPLEX GSB1(NSMR/3,3),GSB2(NSMR/3,3)

      INTEGER IND(NMAX)
      COMPLEX EN(NMAX),EMT(NMAX),ENT(NMAX,MBODY),JST(NMAX,MBODY),   &
              EJGS(4*NMAX*MBODY),EJGS2(4*NMAX*MBODY)
      INTEGER NX(NBMAX,MBODY),NY(NBMAX,MBODY),NZ(NBMAX,MBODY),      &
              NCELL(NBMAX,MBODY),                                   &
              NXI(NBMAX),NYI(NBMAX),NZI(NBMAX),NXJ(NBMAX),          &
              NYJ(NBMAX),NZJ(NBMAX),NCELLI(NBMAX),NCELLJ(NBMAX),    &
              NCT(MBODY),NET(MBODY),KCELL(MBODY),SUB_BLOCK(MBODY)
      REAL BLX(NBMAX,MBODY),BLY(NBMAX,MBODY),BLZ(NBMAX,MBODY),      &
           XCELL(NXMAX,NBMAX,MBODY),YCELL(NYMAX,NBMAX,MBODY),       &
           ZCELL(NZMAX,NBMAX,MBODY)
      REAL BLXI(NBMAX),BLYI(NBMAX),BLZI(NBMAX),                     &
           XCELLI(NXMAX,NBMAX),YCELLI(NYMAX,NBMAX),                 &
           ZCELLI(NZMAX,NBMAX),XCELLJ(NXMAX,NBMAX),                 &
           YCELLJ(NYMAX,NBMAX),ZCELLJ(NZMAX,NBMAX)
!
! --- Solving the matrix equations for the scattering currents
!   in the 3D structure using the system iteration method
!
!-- The maximal number of iterations is determined by the
!   number of substructures and the total number of cells.
!   500 cells as reference
!
      NCMAX=0
      DO IB=1,NBODY
         NCMAX=NCMAX+NCT(IB)
      END DO
      FRACCN=FLOAT(NCMAX)/500.
!
      NITM=2*NBODY**2
      IF (NITM<60) NITM=60
      NITM=INT(NITM*(1.+FRACCN))
      IF (NITM>NITMAX) NITM=NITMAX
!
!-- KACC=1 corresponds to approximate solutions with an
!   accuracy of 20%.  NITLM will also be limited to be 3
!   for the fast solver
!
      IF (KACC==1) THEN
         EPS=2.E-01
         NITM=3
      ELSE IF (KACC>=2.AND.KACC<=4) THEN
         EPS=1.E-04
      ELSE
         EPS=1.E-05
      END IF
!
      IF (KSYMM==1) THEN
         NQ=4
      ELSE
         NQ=1
      END IF
!
!--- Start the loop for the four quadrants in case of
!    group reduction
!
      DO IQ=1,NQ
!
! ---- Start the loop for multiple excitations
!
         DO IEXCI=1,NTX_FD
!
            IF (NQ/=1) THEN
! -- Read ent from unit 41, 42, 43, or 44 for the corresponding
!    quadrants
               DO IB=1,NBODY
                  READ (40+IQ) (EMT(I),I=1,NET(IB))
                  DO J=1,NET(IB)
                     ENT(J,IB)=EMT(J)
                  END DO
               END DO
! -- Read ent from unit 10 for multiple excitations
            ELSE IF (NTX_FD>1) THEN
               DO IB=1,NBODY
                  READ (10) (EMT(I),I=1,NET(IB))
                  DO J=1,NET(IB)
                     ENT(J,IB)=EMT(J)
                  END DO
               END DO
            END IF
!
! ---- Assign jst initial values (zero)
!
            DO IB=1,NBODY
               DO I=1,NET(IB)
                  JST(I,IB)=(0.,0.)
               END DO
            END DO
!
            NITER=1
   10       ERR(NITER)=0.
            TJS=0.
            NERR=0
!
! ---- Loop 4500 is for the solutions of the scattering currents
!    in the substructures (# JB)
!
            DO JB=1,NBODY
               NETJ=NET(JB)
!    LET=NETJ*(NETJ+1)/2
               NERR=NERR+NETJ
!
               DO I=1,NETJ
                  EMT(I)=(0.,0.)
               END DO
!
! ---- Computing mutual interactions, namely, the scattered fields
!    of all other substructure to substructures jb
!
!--- The mutual interaction matrices will be either read from
!    the disk or computed with pre-computed values using the
!    spatial symmetry property of the Green's functions.
!    See the comments before the line labelled 5800 for more
!    explanations
!
               DO IB=1,NBODY
!
                  IF (IB/=JB) THEN
                     NETI=NET(IB)
!
                     IF (KSMR==1.AND.KSYMM==0.OR.  &
                         KSMR==1.AND.KSYMM==1.AND.  &
                         NEQ/3>800) THEN
!
! ---- Re-compute the matrices using spatial symmetries
!
! -- The following lines are similar to those for forming the
!    mutual-coupling matrices earlier in the program,  with
!    slight differences in some subroutines.
!
! -- The following lines for jb may be moved outside the
!    ib loop.
!
                        SUB_BLOCKJ=SUB_BLOCK(JB)
                        NETJ=NET(JB)
                        DO I=1,SUB_BLOCKJ
                           NXJ(I)=NX(I,JB)
                           NYJ(I)=NY(I,JB)
                           NZJ(I)=NZ(I,JB)
                           NCELLJ(I)=NCELL(I,JB)
                           DO J=1,NXJ(I)
                           XCELLJ(J,I)=XCELL(J,I,JB)
                           END DO
                           DO J=1,NYJ(I)
                           YCELLJ(J,I)=YCELL(J,I,JB)
                           END DO
                           DO J=1,NZJ(I)
                           ZCELLJ(J,I)=ZCELL(J,I,JB)
                           END DO
                        END DO
!
                        SUB_BLOCKI=SUB_BLOCK(IB)
                        NETI=NET(IB)
                        DO I=1,SUB_BLOCKI
                           NXI(I)=NX(I,IB)
                           NYI(I)=NY(I,IB)
                           NZI(I)=NZ(I,IB)
                           NCELLI(I)=NCELL(I,IB)
                           BLXI(I)=BLX(I,IB)
                           BLYI(I)=BLY(I,IB)
                           BLZI(I)=BLZ(I,IB)
                           DO J=1,NXI(I)
                           XCELLI(J,I)=XCELL(J,I,IB)
                           END DO
                           DO J=1,NYI(I)
                           YCELLI(J,I)=YCELL(J,I,IB)
                           END DO
                           DO J=1,NZI(I)
                           ZCELLI(J,I)=ZCELL(J,I,IB)
                           END DO
                        END DO
!
                        IF (KSYMM==1) THEN
!
! ---- Group reduction for controlled source problems
!
!
                           CALL MTRX_SMRGS(NXS,NYS,NZS,SBX,SBY,SBZ,ZSB,  &
                              NMAX,NXMAX,NYMAX,NZMAX,NETI,SUB_BLOCKI,    &
                              NCELLI,XCELLI,YCELLI,ZCELLI,NXI,NYI,       &
                              NZI,NETJ,SUB_BLOCKJ,NCELLJ,NXJ,NYJ,NZJ,    &
                              XCELLJ,YCELLJ,ZCELLJ,IQ,GA,NSMR,GSB2)
                        ELSE
!
                           CALL MTRX_SPSM(SBX,SBY,SBZ,NXS,NYS,NZS,ZSB,    &
                              NMAX,NXMAX,NYMAX,NZMAX,NXJ,NYJ,        &
                              NZJ,NCELLJ,SUB_BLOCKJ,XCELLJ,YCELLJ,ZCELLJ, &
                              NXI,NYI,NZI,NCELLI,SUB_BLOCKI,XCELLI,       &
                              YCELLI,ZCELLI,GA,NSMR,GSB2)
                        END IF
                     ELSE
!
! ---- Read the interaction matrices from GREEN2, GREEN4
!    or GREEN5 through GREEN8
!
                        IF (KSYMM==1) THEN
                           NUNIT=34+IQ
                        ELSE IF (KSYMM==0) THEN
                           NUNIT=32
                        END IF
                        DO I=1,NETI
                           READ (NUNIT) (GA(J,I),J=1,NETJ)
!
                        END DO
                     END IF
!
!-- Compute the mutual interactions
!
                     DO I=1,NETI
                        DO J=1,NETJ
                           EMT(J)=EMT(J)+GA(J,I)*JST(I,IB)
                        END DO
                     END DO
                  END IF
               END DO
!
               DO J=1,NETJ
                  EN(J)=ENT(J,JB)+EMT(J)
               END DO
!
!--- Read the factored matrices from GREEN1 or GREEN3
!    for MT problems,  or from GREEN1, GREEN2, GREEN3,
!    and GREEN4 for CSAMT problems with group reduction
!
!    Note that if nbody=1 and ksymm=0 ga is not stored
!
               IF (KSYMM/=0.OR.NBODY/=1) THEN
!
                  IF (KSYMM==1) THEN
!
!-- Read the factored matrices for each quadrant in
!   case of group reduction
!
                     DO I=1,NETJ
                        READ (30+IQ) (GA(J,I),J=1,NETJ)
                     END DO
!
                  ELSE IF (KSYMM==0) THEN
                     DO I=1,NETJ
                        READ (31) (GA(J,I),J=1,NETJ)
                     END DO
                  END IF
               END IF
!
!-- Read the array ind from the files index1 though index4
!   in unit 63 through 66
!
               IF (KSYMM==1) THEN
                  READ (62+IQ) (IND(J),J=1,NETJ)
               ELSE
                  READ (63) (IND(J),J=1,NETJ)
               END IF
!
               IF (KCELL(JB)==1) THEN
                  CALL MTRX_SLV_CSISL(GA,NMAX,NETJ,IND,EN)
               ELSE
                  CALL MTRX_SLV_LUBKSB(GA,EN,NMAX,NETJ,IND)
               END IF
!
! ----- Determine the accuracy of iteration and store the
!     evaluated scattering currents
!
               DO J=1,NETJ
!
!--- The following error criteria some times yield pure
!    convergence if jst is negligible but have large
!    relative error.
!      ERR(NITER)=ERR(NITER)+((ABS(EN(J))-ABS(JST(J,JB)))/
!   +           (ABS(EN(J))+ABS(JST(J,JB))+1.E-30))**2
                  ERR(NITER)=ERR(NITER)   &
                     +(ABS(ABS(EN(J))-ABS(JST(J,JB))))**2
                  TJS=TJS+(ABS(EN(J)))**2
                  JST(J,JB)=EN(J)
               END DO
!
            END DO
!
!    ERR(NITER)=SQRT(ERR(NITER)/NERR)
!
! --- 1.E-30 is added to TJS to avoid overflow in case TJS is zero.
!   TJS may be zero for some quadrants if a vertical dipole is
!   located on some symmetry axes.
!
            ERR(NITER)=SQRT(ERR(NITER)/(TJS+1.E-30))
!
! --- Rewind all the files regardless convergency or not because
!   there may be multi-excitations.

            IF (KSYMM==1) THEN
               REWIND (30+IQ)
               REWIND (34+IQ)
            ELSE IF (KSYMM==0) THEN
               IF (NBODY>1) REWIND (31)
               IF (NBODY>1) REWIND (32)
            END IF
            DO I=1,4
               REWIND (62+I)
            END DO

            IF (ERR(NITER)<=EPS.OR.NITER>=NITM.OR.NBODY==1) THEN

               IF (NITER>=NITM) THEN
                  IF (KSYMM==1) THEN
                     WRITE (NLG,1010) NITER,IQ,ERR(NITER)
                  ELSE
                     WRITE (NLG,1020) NITER,ERR(NITER)
                  END IF
               END IF

! --- Store jst in case of multiple excitation
!   or group reduction
!   Note that kpol=1 for controlled source problems

               IF (KSYMM==1) THEN
                  DO IB=1,NBODY
                     WRITE (44+IQ) (JST(I,IB),I=1,NET(IB))
                  END DO
               ELSE IF (NTX_FD>1) THEN
                  DO IB=1,NBODY
                     WRITE (20) (JST(I,IB),I=1,NET(IB))
                  END DO
               END IF
            ELSE
               NITER=NITER+1
               IF (KSYMM==0.AND.NBODY==1) THEN
               END IF
               GOTO 10
            END IF
!
         END DO
!
         IF (KSYMM==0.AND.NTX_FD>1) REWIND (20)
         IF (KSYMM==1) REWIND (44+IQ)
!
      END DO
!
      IF (KSYMM/=0) THEN
!
! --- Read the transformed scattering currents from units
!   45 through 48, transform them back to real scattering
!   currents, and then store them in unit 20
!
         DO IEXCI=1,NTX_FD
!
            DO IQ=1,4
               NCB1=0
               DO IB=1,NBODY
                  IF (IB>1) NCB1=NCB1+NET(IB-1)
                  READ (44+IQ) (EMT(I),I=1,NET(IB))
                  DO J=1,NET(IB)
                     EJGS(J+NCB1+NEQ*(IQ-1))=EMT(J)
                  END DO
               END DO
            END DO
!
! --- Back transformation of the scattering currents by
!   the transposed unitary matrix
!
            DO LPC=1,NEQ/3
               DO LQC=1,4
                  DO JJ=1,3
                     JC=12*(LPC-1)+3*(LQC-1)+JJ
                     EJGS2(JC)=(0.,0.)
                     DO LPR=1,NEQ/3
                        DO LQR=1,4
                           DO II=1,3
!
!--- Note that the entries of the unitary matrix for
!    lpr .ne. lpc and ii .ne. jj are zero
!
                              IF (LPR==LPC.AND.II==JJ) THEN
                              CALL MTRX_UNITARY(LPR,LQR,II,LPC,LQC,JJ,U)
!-- PMAX=NEQ/3,  IROW is not computed in MTRX_UNITARY
                              IROW=NEQ*(LQR-1)+3*(LPR-1)+II
                              EJGS2(JC)=EJGS2(JC)+U*EJGS(IROW)
                              END IF
                           END DO
                        END DO
                     END DO
                  END DO
               END DO
            END DO
!
! ---  Store EJGS2 in unit 20
!
            WRITE (20) (EJGS2(I),I=1,4*NEQ)
!
         END DO
!
         REWIND (20)
      END IF
!

      RETURN
 1010 FORMAT (' Warning: Iteration did not converge in ',I4,  &
              ' iterations! for quadrant #',I2,  &
              /' Accuracy of the solution:',E10.3)
 1020 FORMAT (' Warning: Iteration did not converge in ',I4,  &
              ' iterations!',/' Accuracy of the solution:',E10.3)
      END
!
!**** End of MAIN_SOLVER
!
!
      SUBROUTINE MAIN_SCAT_EH_CS(KACC,KSYMM,MBODY,NBMAX,                       &
                                 NBODY,SUB_BLOCK,NCELL,NET,EMT,                &
                                 JST,EJGS,NMAX,NTX_FD,N_RX,NHFILM,FRQ,MLAYER,  &
                                 ZBND,LRYTH,KKH,BLMIN,DMIN,RHOMIN,         &
                                 RHOMAX,NZOB,ZOBG,NZSR,ZSRG,ALMAX,RRG,RRG3,    &
                                 GRHF,GRHF3,GRHO0,GRHO03,RX_X,RX_Y,RX_Z,       &
                                 NXMAX,NYMAX,NZMAX,NEQ,NX,NY,NZ,XCELL,YCELL,   &
                                 ZCELL,BLX,BLY,BLZ,RMU,CDH,KSFT,CLMN,M_RX,     &
                                 EAX,EAY,EAZ,HAX,HAY,HAZ,                      &
                                 ESX,ESY,ESZ,HSX,HSY,HSZ)
!
!**** Calculate EM fields at receiver sites
!
!
!   EAX etc are for one excitation whilst ESX are for all excitations
!
!
      IMPLICIT NONE
!
      REAL ALMAX,BLMIN,DMIN,FRQ,RHOMAX,RHOMIN
      INTEGER I,IB,IEXCI,J,KACC,KACC1,KEYG,KSFT,                           &
              KSYMM,MBODY,MLAYER,M_RX,NBMAX,NBODY,NEQ,NHFILM
      INTEGER NMAX,NRG,NRG3,NTX_FD,NXMAX,NYMAX,NZMAX,NZOB,NZSR
      COMPLEX EMT(NMAX),JST(NMAX,MBODY),EJGS(4*NMAX*MBODY),                &
              GRHF(11,NHFILM,NZSR,NZOB),                                   &
              GRHF3(11,NHFILM,NZSR),GRHO0(4,NZSR,NZOB),GRHO03(4,NZSR),     &
              KKH(0:MLAYER),CDH(0:MLAYER),                                 &
              EAX(M_RX),EAY(M_RX),EAZ(M_RX),                               &
              HAX(M_RX),HAY(M_RX),HAZ(M_RX),                               &
              ESX(M_RX,NTX_FD),ESY(M_RX,NTX_FD),ESZ(M_RX,NTX_FD),          &
              HSX(M_RX,NTX_FD),HSY(M_RX,NTX_FD),HSZ(M_RX,NTX_FD)
      REAL ZOBG(NZOB),ZSRG(2,NZSR),RRG(NHFILM),RRG3(NHFILM),               &
           LRYTH(MLAYER),ZBND(0:MLAYER),RMU(0:MLAYER)
      REAL BLX(NBMAX,MBODY),BLY(NBMAX,MBODY),BLZ(NBMAX,MBODY),             &
           XCELL(NXMAX,NBMAX,MBODY),YCELL(NYMAX,NBMAX,MBODY),              &
           ZCELL(NZMAX,NBMAX,MBODY)
      REAL RX_X(M_RX,NTX_FD),RX_Y(M_RX,NTX_FD),RX_Z(M_RX,NTX_FD)
      INTEGER N_RX(NTX_FD)
      INTEGER NX(NBMAX,MBODY),NY(NBMAX,MBODY),NZ(NBMAX,MBODY),             &
              NCELL(NBMAX,MBODY),NET(MBODY),SUB_BLOCK(MBODY)
      REAL CLMN(NBODY)
      INTEGER, PARAMETER :: KCLMN=0  ! always use automatic choice of clmn
!
! --- Reduce the accuracy level for computing scattered responses
!   to 3 if it is above 3.  High accuracy levels are necessary
!   for accurate computation of the matrix elements which are
!   also affordable if spatial symmetry reductions are used.
!
      IF (KACC>3) THEN
         KACC1=3
      ELSE
         KACC1=KACC
      END IF
!
! ---- Compute the grid values for the interpolation of the
!    Hankel transforms
!
      KEYG=2
!
      CALL THR_D_HF_TABLE(NHFILM,MLAYER,ZBND,LRYTH,KKH,RMU,           &
                          KSFT,BLMIN,KEYG,DMIN,RHOMIN,RHOMAX,NZOB,ZOBG,   &
                          NZSR,ZSRG,ALMAX,RRG,NRG,RRG3,NRG3,GRHF,GRHF3,   &
                          GRHO0,GRHO03)
!
      DO IEXCI=1,NTX_FD
!
! ---- Read jst from unit 20 for multiple excitations
!    or ejgs from 20 for controlled source problems
!    with two-plane symmetries (group reduction)
!
         IF (NTX_FD>1.AND.KSYMM==0) THEN
            DO IB=1,NBODY
               READ (20) (EMT(I),I=1,NET(IB))
               DO J=1,NET(IB)
                  JST(J,IB)=EMT(J)
               END DO
            END DO
         END IF
!
         IF (KSYMM==1) READ (20) (EJGS(I),I=1,4*NEQ)
!
!-- For loop-loop configurations only one receiver per excitation
!
         IF (KSYMM==0) CALL SCAT_EH_CS(NBMAX,NMAX,NXMAX,NYMAX,NZMAX,NBODY,  &
             SUB_BLOCK,N_RX,RX_X,RX_Y,RX_Z,NX,NY,NZ,NCELL,XCELL,YCELL,      &
             ZCELL,BLX,BLY,BLZ,JST,IEXCI,CLMN,M_RX,NTX_FD,                  &
             EAX,EAY,EAZ,HAX,HAY,HAZ,FRQ,MLAYER,ZBND,RMU,                   &
             CDH,NZOB,ZOBG,NZSR,ZSRG,RHOMIN,DMIN,NHFILM,                &
             RRG,NRG,GRHF,RRG3,NRG3,GRHF3,GRHO0,GRHO03,KCLMN,BLMIN,         &
             KACC1)
         IF (KSYMM==1) CALL SCAT_EH_CSGS(NBMAX,NEQ,NXMAX,NYMAX,NZMAX,NBODY, &
             SUB_BLOCK,NET,N_RX,RX_X,RX_Y,RX_Z,NX,NY,NZ,NCELL,XCELL,        &
             YCELL,ZCELL,BLX,BLY,BLZ,EJGS,IEXCI,CLMN,                       &
             M_RX,NTX_FD,EAX,EAY,EAZ,HAX,HAY,HAZ,FRQ,                       &
             MLAYER,ZBND,RMU,CDH,NZOB,ZOBG,NZSR,ZSRG,                   &
             RHOMIN,DMIN,NHFILM,RRG,NRG,GRHF,RRG3,NRG3,GRHF3,GRHO0,         &
             GRHO03,KCLMN,BLMIN,KACC1)
!
         DO I=1,N_RX(IEXCI)
            ESX(I,IEXCI)=EAX(I)
            ESY(I,IEXCI)=EAY(I)
            ESZ(I,IEXCI)=EAZ(I)
            HSX(I,IEXCI)=HAX(I)
            HSY(I,IEXCI)=HAY(I)
            HSZ(I,IEXCI)=HAZ(I)
         END DO
!
      END DO
!
      RETURN
      END
!
!**** End of MAIN_SCAT_EH_CS
!
!
      SUBROUTINE MTRX_1M(NMAX,NXMAX,NYMAX,NZMAX,NX,NY,NZ,NCELL,    &
                         NSUBCM,SUB_BLOCK,X,Y,Z,CLX,CLY,CLZ,CDB,G,   &
                         FRQ,MLAYER,ZBND,RMU,CDH,NZOB,ZOBG,  &
                         NZSR,ZSRG,RHOMIN,DMIN,NHFILM,RRG,           &
                         NRG,GRHF,RRG3,NRG3,GRHF3,GRHO0,GRHO03,      &
                         CLMN,KCLMN,BLMIN,KACC,KUTCRP)
!
!**** SET UP THE MATRIX FOR A STRUCTURE WITH DIFFERENT CELL
!   SIZES OR A STRUCTURE EMBBEDED IN AN ANISOTROPIC LAYER
!   WITH THE BODY CROSSING A LAYER BOUNDARY
!
!   Note the matrix is not symmetrical
!
!   The difference between smtrx1 (in MT3D) and smtrx1m is
!   that the matrix elements in smtrx1 are multiplied by the
!   conductivity differences
!
!   The parameter kself controls if the computation for the
!   integration of the primary part of the Green's function
!   for self-cells should be repeated. As the dimensions of
!   the cells within a block is the same, this integration
!   need to be done only once (kself=1) for cells located in
!   the same layer.   The results are stored in a common
!   block in geprm for use for all other cells in this block
!   (kself=0)
!
!Input parameters:
!
!
!
!  All other parameters FRQ, MLAYER, ZBND,  CDH, NZOB, ZOBG,
!  NZSR, ZSRG, RHOMIN, DMIN, NHFILM, RRG, NRG, GRHF,
!  GRHF3, GRHO0, GRHO03, CLMN, KCLMN, BLMIN, KACC, and KUTCRP are to
!  be passed into routine THR_D_GREEN.  Check THR_D_GREEN for their
!  descriptions.
!
!
!Output parameters:
!
!  G:
!
!
!**** Called by: MAIN_MATRICES
!
!**** Calls:     THR_D_GREEN
!
!
      IMPLICIT NONE
!
      REAL BLMIN,CLMN,CLX1,CLY1,CLZ1,DMIN,FRQ,RHOMIN
      INTEGER I,II,IOB,ISR,J,JJ,K,KACC,KCLMN,KEYG,KK,KSELF,KUTCRP,L,LL,   &
              MLAYER,N1,N2,SUB_BLOCK
      INTEGER NCB,NCB1,NHFILM,NMAX,NN,NN1,NN2,NOB,NOB1,NRG,NRG3,NSR,      &
              NSUBCM,NXMAX,NYMAX,NZMAX,NZOB,NZSR
      COMPLEX G(NMAX,NMAX),EXX,EXY,EXZ,EYX,EYY,EYZ,EZX,EZY,EZZ,              &
              CDH(0:MLAYER),CDB(NSUBCM,SUB_BLOCK),DELTH,                          &
              GRHF(11,NHFILM,NZSR,NZOB),GRHO0(4,NZSR,NZOB),               &
              GRHF3(11,NHFILM,NZSR),GRHO03(4,NZSR)
      REAL :: RMU(0:MLAYER)
      INTEGER NX(SUB_BLOCK),NY(SUB_BLOCK),NZ(SUB_BLOCK),NCELL(SUB_BLOCK)
      REAL  X(NXMAX,SUB_BLOCK),Y(NYMAX,SUB_BLOCK),Z(NZMAX,SUB_BLOCK),     &
            CLX(SUB_BLOCK),CLY(SUB_BLOCK),CLZ(SUB_BLOCK),                 &
            ZBND(0:MLAYER),ZOBG(NZOB),ZSRG(2,NZSR),RRG(NRG),RRG3(NRG3),PSTION(8)
      COMPLEX ECOMP(9),HCOMP(9)
      EQUIVALENCE (ECOMP(1),EXX),(ECOMP(2),EYX),(ECOMP(3),EZX),   &
                  (ECOMP(4),EXY),(ECOMP(5),EYY),(ECOMP(6),EZY),   &
                  (ECOMP(7),EXZ),(ECOMP(8),EYZ),(ECOMP(9),EZZ)
!
      KEYG=1
!
      NCB=0
      DO L=1,SUB_BLOCK
         IF (L>1) NCB=NCB+NCELL(L-1)
         DO K=1,NZ(L)
            DO IOB=MLAYER-1,0,-1
               IF (Z(K,L)>=ZBND(IOB)) THEN
                  NOB=IOB+1
                  GOTO 20
               END IF
            END DO
            NOB=0
   20       IF (L==1.AND.K==1) NOB1=NOB
            DO I=1,NX(L)
               DO J=1,NY(L)
                  NN=(I-1)*NY(L)*NZ(L)+(J-1)*NZ(L)+K
                  N1=NN+NCB
                  NN1=(N1-1)*3+1
                  DELTH=CDB(NN,L)-CDH(NOB)
!
                  NCB1=0
                  DO LL=1,SUB_BLOCK
                     IF (LL>1) NCB1=NCB1+NCELL(LL-1)
                     DO II=1,NX(LL)
                        DO JJ=1,NY(LL)
                           DO KK=1,NZ(LL)
                              N2=(II-1)*NY(LL)*NZ(LL)+(JJ-1)*NZ(LL)   &
                                 +KK+NCB1
                              NN2=(N2-1)*3+1
!
                              DO ISR=MLAYER-1,0,-1
                              IF (Z(KK,LL)>=ZBND(ISR)) THEN
                              NSR=ISR+1
                              GOTO 22
                              END IF
                              END DO
                              NSR=0
!
! ---- KSELF controls if the calculation for the self-cells
!    should be repeated
   22                         IF (I==1.AND.J==1.AND.K==1.OR.  &
                                 NOB1/=NOB.OR.NOB1/=NSR) THEN
                              KSELF=1
                              ELSE
                              KSELF=0
                              END IF
!
                              CLX1=CLX(LL)
                              CLY1=CLY(LL)
                              CLZ1=CLZ(LL)
!
                              PSTION(1)=X(I,L)
                              PSTION(2)=Y(J,L)
                              PSTION(3)=Z(K,L)
                              PSTION(4)=X(II,LL)
                              PSTION(5)=Y(JJ,LL)
                              PSTION(6)=Z(KK,LL)
                              CALL THR_D_GREEN(KEYG,PSTION,FRQ,MLAYER,ZBND,  &
                                 CDH,RMU,NZOB,ZOBG,NZSR,ZSRG,            &
                                 RHOMIN,DMIN,NHFILM,RRG,NRG,GRHF,RRG3,       &
                                 NRG3,GRHF3,GRHO0,GRHO03,CLX1,CLY1,CLZ1,     &
                                 CLMN,KCLMN,BLMIN,KACC,KUTCRP,KSELF,         &
                                 ECOMP,HCOMP)
!
                              IF (N1==N2) THEN
                              G(NN1,NN2)=1./DELTH-EXX
                              G(NN1+1,NN2+1)=1./DELTH-EYY
                              G(NN1+2,NN2+2)=1./DELTH-EZZ
                              ELSE
                              G(NN1,NN2)=-EXX
                              G(NN1+1,NN2+1)=-EYY
                              G(NN1+2,NN2+2)=-EZZ
                              END IF
                              G(NN1+1,NN2)=-EYX
                              G(NN1+2,NN2)=-EZX
                              G(NN1,NN2+1)=-EXY
                              G(NN1+2,NN2+1)=-EZY
                              G(NN1,NN2+2)=-EXZ
                              G(NN1+1,NN2+2)=-EYZ
                           END DO
                        END DO
                     END DO
                  END DO
               END DO
            END DO
         END DO
      END DO
      RETURN
      END
!
!**** End of MTRX_1M
!
!
      SUBROUTINE MTRX_1MS(NMAX,N,NXMAX,NYMAX,NZMAX,NX,NY,NZ,NCELL,    &
                          NSUBCM,SUB_BLOCK,X,Y,Z,CLX,CLY,CLZ,CDB,G,   &
                          FRQ,MLAYER,ZBND,RMU,CDH,NZOB,ZOBG,  &
                          NZSR,ZSRG,RHOMIN,DMIN,NHFILM,               &
                          RRG,NRG,GRHF,RRG3,NRG3,GRHF3,GRHO0,GRHO03,  &
                          CLMN,KCLMN,BLMIN,KACC,KUTCRP)
!
!**** SET UP THE MATRIX FOR A STRUCTURE WITH EQUAL CELL
!   SIZES FOR A STRUCTURE EMBBEDED IN AN ISOPTROPIC EARTN
!   OR IN A SINGLE LAYER OF AN ANISOTROPIC EARTH
!
!   Note the matrix is symmetrical and is stored in a doubly
!   subscripted array
!
!   smtrx1ms is essentially the same as smtrx1m except
!   that smtrx1ms save a half computer time due to symmetry.
!   smtrx1ms and smtrx differ only in their way of storing
!   the matrices
!
!
!Input parameters:
!
!
!
!  All other parameters FRQ, MLAYER, ZBND,  CDH,  NZOB, ZOBG,
!  NZSR, ZSRG, RHOMIN, DMIN, NHFILM, RRG, NRG, GRHF,
!  GRHF3, GRHO0, GRHO03, CLMN, KCLMN, BLMIN, KACC, and KUTCRP are to
!  be passed into routine THR_D_GREEN.  Check THR_D_GREEN for their
!  descriptions.
!
!
!Output parameters:
!
!  G:
!
!
!**** Called by: MAIN_MATRICES
!
!**** Calls:     THR_D_GREEN
!
!
      IMPLICIT NONE
!
      REAL BLMIN,CLMN,CLX1,CLY1,CLZ1,DMIN,FRQ,RHOMIN
      INTEGER I,II,IOB,ISR,J,JJ,K,KACC,KCLMN,KEYG,KK,KSELF,KUTCRP,L,LL,   &
              MLAYER,N,N1,N2,SUB_BLOCK
      INTEGER NCB,NCB1,NHFILM,NMAX,NN,NN1,NN2,NOB,NOB1,NRG,NRG3,NSR,      &
              NSUBCM,NXMAX,NYMAX,NZMAX,NZOB,NZSR
      COMPLEX G(NMAX,NMAX),EXX,EXY,EXZ,EYX,EYY,EYZ,EZX,EZY,EZZ,              &
              CDH(0:MLAYER),CDB(NSUBCM,SUB_BLOCK),DELTH,                          &
              GRHF(11,NHFILM,NZSR,NZOB),GRHO0(4,NZSR,NZOB),               &
              GRHF3(11,NHFILM,NZSR),GRHO03(4,NZSR)
      REAL :: RMU(0:MLAYER)
      INTEGER NX(SUB_BLOCK),NY(SUB_BLOCK),NZ(SUB_BLOCK),NCELL(SUB_BLOCK)
      REAL  X(NXMAX,SUB_BLOCK),Y(NYMAX,SUB_BLOCK),Z(NZMAX,SUB_BLOCK),     &
            CLX(SUB_BLOCK),CLY(SUB_BLOCK),CLZ(SUB_BLOCK),                 &
            ZBND(0:MLAYER),ZOBG(NZOB),                      &
            ZSRG(2,NZSR),RRG(NRG),RRG3(NRG3),PSTION(8)
      COMPLEX ECOMP(9),HCOMP(9)
      EQUIVALENCE (ECOMP(1),EXX),(ECOMP(2),EYX),(ECOMP(3),EZX),   &
                  (ECOMP(4),EXY),(ECOMP(5),EYY),(ECOMP(6),EZY),   &
                  (ECOMP(7),EXZ),(ECOMP(8),EYZ),(ECOMP(9),EZZ)
!
      KEYG=1
!
      NCB=0
      DO L=1,SUB_BLOCK
         IF (L>1) NCB=NCB+NCELL(L-1)
         DO K=1,NZ(L)
            DO IOB=MLAYER-1,0,-1
               IF (Z(K,L)>=ZBND(IOB)) THEN
                  NOB=IOB+1
                  GOTO 20
               END IF
            END DO
            NOB=0
   20       IF (L==1.AND.K==1) NOB1=NOB
            DO I=1,NX(L)
               DO J=1,NY(L)
                  NN=(I-1)*NY(L)*NZ(L)+(J-1)*NZ(L)+K
                  N1=NN+NCB
                  NN1=(N1-1)*3+1
                  DELTH=CDB(NN,L)-CDH(NOB)
!
                  NCB1=0
                  DO LL=1,SUB_BLOCK
                     IF (LL>1) NCB1=NCB1+NCELL(LL-1)
                     DO II=1,NX(LL)
                        DO JJ=1,NY(LL)
                           DO KK=1,NZ(LL)
                              N2=(II-1)*NY(LL)*NZ(LL)+(JJ-1)*NZ(LL)   &
                                 +KK+NCB1
                              NN2=(N2-1)*3+1
!
                              IF (N1<=N2) THEN
!
                              DO ISR=MLAYER-1,0,-1
                              IF (Z(KK,LL)>=ZBND(ISR)) THEN
                              NSR=ISR+1
                              GOTO 22
                              END IF
                              END DO
                              NSR=0
!
! ---- KSELF controls if the calculation for the self-cells
!    should be repeated
   22                         IF (I==1.AND.J==1.AND.K==1.OR.  &
                                 NOB1/=NOB.OR.NOB1/=NSR) THEN
                              KSELF=1
                              ELSE
                              KSELF=0
                              END IF
!
                              CLX1=CLX(LL)
                              CLY1=CLY(LL)
                              CLZ1=CLZ(LL)
!
                              PSTION(1)=X(I,L)
                              PSTION(2)=Y(J,L)
                              PSTION(3)=Z(K,L)
                              PSTION(4)=X(II,LL)
                              PSTION(5)=Y(JJ,LL)
                              PSTION(6)=Z(KK,LL)
                              CALL THR_D_GREEN(KEYG,PSTION,FRQ,MLAYER,ZBND,  &
                                 CDH,RMU,NZOB,ZOBG,NZSR,ZSRG,            &
                                 RHOMIN,DMIN,NHFILM,RRG,NRG,GRHF,RRG3,       &
                                 NRG3,GRHF3,GRHO0,GRHO03,CLX1,CLY1,CLZ1,     &
                                 CLMN,KCLMN,BLMIN,KACC,KUTCRP,KSELF,         &
                                 ECOMP,HCOMP)
!
                              IF (N1==N2) THEN
                              G(NN1,NN2)=1./DELTH-EXX
                              G(NN1+1,NN2+1)=1./DELTH-EYY
                              G(NN1+2,NN2+2)=1./DELTH-EZZ
                              ELSE
                              G(NN1,NN2)=-EXX
                              G(NN1+1,NN2+1)=-EYY
                              G(NN1+2,NN2+2)=-EZZ
                              END IF
                              G(NN1+1,NN2)=-EYX
                              G(NN1+2,NN2)=-EZX
                              G(NN1,NN2+1)=-EXY
                              G(NN1+2,NN2+1)=-EZY
                              G(NN1,NN2+2)=-EXZ
                              G(NN1+1,NN2+2)=-EYZ
                              END IF
                           END DO
                        END DO
                     END DO
                  END DO
               END DO
            END DO
         END DO
      END DO
      DO I=1,N
         DO J=1,N
            IF (I>J) G(I,J)=G(J,I)
         END DO
      END DO
      RETURN
      END
!
!**** End of MTRX_1MS
!
!
      SUBROUTINE MTRX_M(NMAX,NXMAX,NYMAX,NZMAX,NX1,NY1,NZ1,NCELL1,      &
                        SUB_BLOCK1,X1,Y1,Z1,NX2,NY2,NZ2,NCELL2,           &
                        SUB_BLOCK2,X2,Y2,Z2,CLX2,CLY2,CLZ2,G,FRQ,MLAYER,  &
                        ZBND,RMU,CDH,NZOB,ZOBG,NZSR,ZSRG,RHOMIN,DMIN, &
                        NHFILM,RRG,NRG,GRHF,RRG3,NRG3,GRHF3,GRHO0,        &
                        GRHO03,CLMN,KCLMN,BLMIN,KACC,KUTCRP)
!
!**** Form mutual-coupling matrices between substructures
!
      IMPLICIT NONE
!
      REAL BLMIN,CLMN,CLX,CLY,CLZ,DMIN,FRQ,RHOMIN
      INTEGER I,II,J,JJ,K,KACC,KCLMN,KEYG,KK,KSELF,KUTCRP,L,LL,MLAYER,  &
              N1,N2,SUB_BLOCK1,SUB_BLOCK2,NCB
      INTEGER NCB1,NHFILM,NMAX,NN,NN1,NN2,NRG,NRG3,NXMAX,NYMAX,NZMAX,     &
              NZOB,NZSR
      COMPLEX G(NMAX,NMAX),EXX,EXY,EXZ,EYX,EYY,EYZ,EZX,EZY,EZZ,              &
              GRHF(11,NHFILM,NZSR,NZOB),GRHO0(4,NZSR,NZOB),               &
              GRHF3(11,NHFILM,NZSR),GRHO03(4,NZSR)
      INTEGER NX1(SUB_BLOCK1),NY1(SUB_BLOCK1),NZ1(SUB_BLOCK1),            &
              NCELL1(SUB_BLOCK1),NX2(SUB_BLOCK2),NY2(SUB_BLOCK2),         &
              NZ2(SUB_BLOCK2),NCELL2(SUB_BLOCK2)
      REAL  X1(NXMAX,SUB_BLOCK1),Y1(NYMAX,SUB_BLOCK1),Z1(NZMAX,SUB_BLOCK1),  &
            X2(NXMAX,SUB_BLOCK2),Y2(NYMAX,SUB_BLOCK2),Z2(NZMAX,SUB_BLOCK2),  &
            CLX2(SUB_BLOCK2),CLY2(SUB_BLOCK2),CLZ2(SUB_BLOCK2),              &
            ZBND(0:MLAYER),ZOBG(NZOB),                         &
            ZSRG(2,NZSR),RRG(NRG),RRG3(NRG3),PSTION(8)
      COMPLEX CDH(0:MLAYER),ECOMP(9),HCOMP(9)
      REAL :: RMU(0:MLAYER)
      EQUIVALENCE (ECOMP(1),EXX),(ECOMP(2),EYX),(ECOMP(3),EZX),   &
                  (ECOMP(4),EXY),(ECOMP(5),EYY),(ECOMP(6),EZY),   &
                  (ECOMP(7),EXZ),(ECOMP(8),EYZ),(ECOMP(9),EZZ)
!
      KEYG=1
!
      NCB=0
      DO L=1,SUB_BLOCK1
         IF (L>1) NCB=NCB+NCELL1(L-1)
! ---- Note that here k is the out-loop which may save some time in
!    array reference in the interpolation
         DO K=1,NZ1(L)
            DO I=1,NX1(L)
               DO J=1,NY1(L)
                  NN=(I-1)*NY1(L)*NZ1(L)+(J-1)*NZ1(L)+K
                  N1=NN+NCB
                  NN1=(N1-1)*3+1
                  NCB1=0
                  DO LL=1,SUB_BLOCK2
                     IF (LL>1) NCB1=NCB1+NCELL2(LL-1)
                     DO II=1,NX2(LL)
                        DO JJ=1,NY2(LL)
                           DO KK=1,NZ2(LL)
                              N2=(II-1)*NY2(LL)*NZ2(LL)+(JJ-1)*NZ2(LL) +   &
                                  KK+NCB1
                              NN2=(N2-1)*3+1
                              CLX=CLX2(LL)
                              CLY=CLY2(LL)
                              CLZ=CLZ2(LL)
!
                              PSTION(1)=X1(I,L)
                              PSTION(2)=Y1(J,L)
                              PSTION(3)=Z1(K,L)
                              PSTION(4)=X2(II,LL)
                              PSTION(5)=Y2(JJ,LL)
                              PSTION(6)=Z2(KK,LL)
                              CALL THR_D_GREEN(KEYG,PSTION,FRQ,MLAYER,ZBND,  &
                                 CDH,RMU,NZOB,ZOBG,NZSR,ZSRG,            &
                                 RHOMIN,DMIN,NHFILM,RRG,NRG,GRHF,RRG3,       &
                                 NRG3,GRHF3,GRHO0,GRHO03,CLX,CLY,CLZ,        &
                                 CLMN,KCLMN,BLMIN,KACC,KUTCRP,KSELF,         &
                                 ECOMP,HCOMP)
!
                              G(NN1,NN2)=EXX
                              G(NN1+1,NN2)=EYX
                              G(NN1+2,NN2)=EZX
                              G(NN1,NN2+1)=EXY
                              G(NN1+1,NN2+1)=EYY
                              G(NN1+2,NN2+1)=EZY
                              G(NN1,NN2+2)=EXZ
                              G(NN1+1,NN2+2)=EYZ
                              G(NN1+2,NN2+2)=EZZ
                           END DO
                        END DO
                     END DO
                  END DO
               END DO
            END DO
         END DO
      END DO
      RETURN
      END
!
!
      SUBROUTINE MTRX_SPSGSB(SBX,SBY,SBZ,NXS,NYS,NZS,XSB,YSB,ZSB,NSMR,G,      &
                             FRQ,MLAYER,ZBND,CDH,RMU,NZOB,ZOBG,NZSR,      &
                             ZSRG,RHOMIN,DMIN,NHFILM,RRG,NRG,GRHF,RRG3,NRG3,  &
                             GRHF3,GRHO0,GRHO03,CLMN,KCLMN,BLMIN,KACC,KUTCRP)
!
!**** Compute the non-identical elements of the scattering matrix
!   for the super block discretised into equal-size cells
!
!   Cell conductivities are not considered in this routine.
!
!   The size of the array G is determined in the main program.
!   It has the same size as the array grhf which stores the
!   grid value of the hankel transforms.
!
!   The parameter kself controls if the computation for the
!   integration of the primary part of the Green's function
!   for self-cells should be repeated. As the dimensions of
!   the cells within a block is the same, this integration
!   need to be done only once (kself=1) for cells located in
!   the same layer.
!
! Input parameters
!
!  SBX:     Real,  dimension of the super block in the x-direction.
!  SBY:     Real,  dimension of the super block in the y-direction.
!  SBZ:     Real,  dimension of the super block in the z-direction.
!  NXS:     Integer,  number of cells in the x-direction of the superblock.
!  NYS:     Integer,  number of cells in the y-direction of the superblock.
!  NZS:     Integer,  number of cells in the z-direction of the superblock.
!  XSB:     Real,  x-ccordinate of the superblock.
!  YSB:     Real,  y-ccordinate of the superblock.
!  ZSB:     Real,  z-ccordinate of the superblock.
!  NSMR:    Integer,  the maximal number of non-identical elements
!           for the scattering matrix which is determined by the
!           parameters NZSR, NZOB, and NHFILM of the code.  See the corresponding
!           parameter list in the main program for detail.
!
!  All other parameters FRQ, MLAYER, ZBND,  CDH, NZOB, ZOBG,
!  NZSR, ZSRG,  RHOMIN, DMIN, NHFILM, RRG, NRG, GRHF,
!  GRHF3, GRHO0, GRHO03, CLMN, KCLMN, BLMIN, KACC, and KUTCRP are to
!  be passed into routine THR_D_GREEN.  Check THR_D_GREEN for their
!  descriptions.
!
!
!Output parameters:
!
!  G:       Complex G(NSMR/3,3),  the non-identical matrix entries
!
!
!**** Called by:  MAIN_MATRICES
!
!**** Calls:      THR_D_GREEN
!
!
      IMPLICIT NONE
!
      REAL BLMIN,CLMN,FRQ,RHOMIN,SBX,SBY,SBZ,SCX,SCY,SCZ,X1,XI,XSB,Y1,   &
           YI,YSB,ZI,ZJ,ZSB
      INTEGER I,IOB,ISR,J,K,KACC,KCLMN,KEYG,KK,KSELF,KUTCRP,MLAYER,      &
              NHFILM,NN,NN1,NOB,NOB1,NRG,NRG3,NSMR
      INTEGER NSR,NXS,NYS,NZOB,NZS,NZSR
!
      COMPLEX G(NSMR/3,3)
!-- Parameter HCOMP is only used for consistence with routine THR_D_GREEN.
      COMPLEX ECOMP(9),HCOMP(9),EXX,EYX,EZX,EXY,EYY,EZY,EXZ,EYZ,EZZ,       &
              CDH(0:MLAYER),GRHF(11,NHFILM,NZSR,NZOB),GRHO0(4,NZSR,NZOB),  &
              GRHF3(11,NHFILM,NZSR),GRHO03(4,NZSR)
      REAL ZBND(0:MLAYER),RMU(0:MLAYER),ZOBG(NZOB),ZSRG(2,NZSR), &
           RRG(NHFILM),RRG3(NRG3),PSTION(8),DMIN
      EQUIVALENCE (ECOMP(1),EXX),(ECOMP(2),EYX),(ECOMP(3),EZX),   &
                  (ECOMP(4),EXY),(ECOMP(5),EYY),(ECOMP(6),EZY),   &
                  (ECOMP(7),EXZ),(ECOMP(8),EYZ),(ECOMP(9),EZZ)
!
      KEYG=1
!
! --- Compute the cell dimension of the super block
!
      SCX=SBX/NXS
      SCY=SBY/NYS
      SCZ=SBZ/NZS
!
! --- Set the x- and y-coordinates of the cells at the lower left
!   corner
!
      X1=XSB-.5*SBX+.5*SCX
      Y1=YSB-.5*SBY+.5*SCY
!
      DO KK=1,NZS
         ZJ=ZSB-.5*SBZ+.5*SCZ+(KK-1)*SCZ
         DO IOB=MLAYER-1,0,-1
            IF (ZJ>=ZBND(IOB)) THEN
               NOB=IOB+1
               GOTO 50
            END IF
         END DO
         NOB=0
   50    IF (KK==1) NOB1=NOB
!
         DO K=1,NZS
!
! --- Reciprocity
!
!   Note that unlike in the routines MTRX_SPS1 etc. here the KK loop
!   is the field cell loop
!
            IF (KK<=K) THEN
!
               ZI=ZSB-.5*SBZ+.5*SCZ+(K-1)*SCZ
               DO ISR=MLAYER-1,0,-1
                  IF (ZI>=ZBND(ISR)) THEN
                     NSR=ISR+1
                     GOTO 60
                  END IF
               END DO
               NSR=0
!
! --- Note that the order of the array G in this routine is
!   different from that of the scattering matrices in other
!   routines which form the scattering matrices.  This is
!   due to the consideration of the reciprocity available for
!   different z-levels (the KK loop).  Also the loop for the
!   vertical direction (the K loop) is moved ahead of the
!   other two loops
!
   60          DO J=1,NYS
                  DO I=1,NXS
!
                     NN=(K-KK)*NXS*NYS+(J-1)*NXS+I
                     NN1=(3*NXS*NYS*NZS+3*NXS*NYS*NZS-3*NXS*NYS*(KK-2))  &
                         *(KK-1)/2+(NN-1)*3+1
!
! ---- KSELF controls if the calculation for the self-cells
!    should be repeated
!
                     IF (I==1.AND.J==1.AND.K==1.OR.NOB1/=NOB.OR.  &
                         NOB1/=NSR) THEN
                        KSELF=1
                     ELSE
                        KSELF=0
                     END IF
!
                     XI=XSB-.5*SBX+.5*SCX+(I-1)*SCX
                     YI=YSB-.5*SBY+.5*SCY+(J-1)*SCY
!
                     PSTION(1)=X1
                     PSTION(2)=Y1
                     PSTION(3)=ZJ
                     PSTION(4)=XI
                     PSTION(5)=YI
                     PSTION(6)=ZI
                     CALL THR_D_GREEN(KEYG,PSTION,FRQ,MLAYER,ZBND,CDH,RMU, &
                          NZOB,ZOBG,NZSR,ZSRG,RHOMIN,DMIN,NHFILM,              &
                          RRG,NRG,GRHF,RRG3,NRG3,GRHF3,GRHO0,GRHO03,SCX,       &
                          SCY,SCZ,CLMN,KCLMN,BLMIN,KACC,KUTCRP,KSELF,          &
                          ECOMP,HCOMP)
!
                     G(NN1,1)=-EXX
                     G(NN1+1,1)=-EYX
                     G(NN1+2,1)=-EZX
                     G(NN1,2)=-EXY
                     G(NN1+1,2)=-EYY
                     G(NN1+2,2)=-EZY
                     G(NN1,3)=-EXZ
                     G(NN1+1,3)=-EYZ
                     G(NN1+2,3)=-EZZ
                  END DO
               END DO
            END IF
!
         END DO
      END DO
      END
!
!**** End of GSB
!
!
      SUBROUTINE MTRX_SPS(NSMR,MLAYER,ZBND,CDH,SBX,SBY,SBZ,NXS,NYS,  &
                          NZS,ZSB,NMAX,NXMAX,NYMAX,NZMAX,NX,NY,NZ,NCELL,   &
                          NSUBCM,SUB_BLOCK,X,Y,Z,CDB,GSB2,G)
!
!****  Form the scattering matrix using spatial symmetry reduction
!    for diagonal submatrices or a general matrix of a
!    non-symmetric structure
!
!    The non-identical elements are pre-computed by the routine
!    GSB and are stored in the array gsb2.  Routine GSB must be called
!    before this routine.
!
!    Note that this routine works per substructure.  Thus,  all
!    structure parameters like NX etc. are those for one substructure.
!
!Input parameters
!
!  NSMR:    Integer,  the maximal number of non-identical elements
!           for the scattering matrix which is determined by the
!           parameters NZSR, NZOB and NHFILM of the code.  See the corresponding
!           parameter list in the main program for detail.
!  MLAYER:  Integer, the number of layers including the air.
!  ZBND:    Real ZBND(0:MLAYER), the z-coordinates of the layer
!           boundaries with the air-earth interface being always
!           0 (zbnd(0)=0).
!  CDH:     Complex CDH(0:MLAYER),  complex conductivities of
!           the layers in the lateral direction including the
!           air (layer 0).
!  SBX:     Real,  dimension of the super block in the x-direction.
!  SBY:     Real,  dimension of the super block in the y-direction.
!  SBZ:     Real,  dimension of the super block in the z-direction.
!  NXS:     Integer,  number of cells in the x-direction of the superblock.
!  NYS:     Integer,  number of cells in the y-direction of the superblock.
!  NZS:     Integer,  number of cells in the z-direction of the superblock.
!  ZSB:     Real,  z-ccordinate of the superblock.
!  NMAX:     Integer,  maximal number of unknowns in a substructure.
!            NMAX=3*NSUBCM.
!  NXMAX:   Integer,  maximal number of cells in the x-direction
!           in a block.
!  NYMAX:   Integer,  maximal number of cells in the y-direction
!           in a block.
!  NZMAX:   Integer,  maximal number of cells in the z-direction
!           in a block.
!  NX:      Integer NX(SUB_BLOCK), number of cells in the substructure
!           in the x-direction.
!  NY:      Integer NY(SUB_BLOCK), number of cells in the substructure
!           in the y-direction.
!  NZ:      Integer NZ(SUB_BLOCK), number of cells in the substructure
!           in the z-direction.
!  NCELL:   Integer NCELL(SUB_BLOCK),  number of cell in each block
!           of the substructure.
!  NSUBCM:  Integer,  maximal number of cells in a substructure.
!  SUB_BLOCK:  Integer,  number of blocks in the substructure.
!  X:       Real X(NXMAX,SUB_BLOCK), the x-coordinates of the cells
!           of the blocks in the substructure.
!  Y:       Real Y(NXMAX,SUB_BLOCK), the y-coordinates of the cells
!           of the blocks in the substructure.
!  Z:       Real Z(NXMAX,SUB_BLOCK), the z-coordinates of the cells
!           of the blocks in the substructure.
!  CDB:     COMPLEX CDB(NSUBCM,NBMAX),  working array for the
!           complex conductivities of the cells in a substructure.
!           Its values are all stored on the disk in unit 62.
!  GSB2:    Complex GSB2(NSMR/3,3),  the non-identical matrix entries
!
!
!Output parameters:
!
!  G:       Complex G(NMAX,NMAX),  the scattering matrix or the
!           diagonal submatrix
!
!**** Called by:  MAIN_MATRICES
!
!**** Calls:      none
!
!
 IMPLICIT NONE

 INTEGER I,II,IOB,IX,J,JJ,JY,K,KK,KKZ,KZ,L,LL,MLAYER,MM,MM2,MX,MY,N1,NCB,NCB1, &
         N2,SUB_BLOCK,NMAX,NN,NN1,NN2,NOB,NSMR,NSUBCM,NXMAX,NXS,NYMAX,NYS,NZS, &
         NZMAX,NX(SUB_BLOCK),NY(SUB_BLOCK),NZ(SUB_BLOCK),NCELL(SUB_BLOCK)
 REAL DX,DY,SBX,SBY,SBZ,SCX,SCY,SCZ,ZSB, X(NXMAX,SUB_BLOCK),Y(NYMAX,SUB_BLOCK), &
      Z(NZMAX,SUB_BLOCK), ZBND(0:MLAYER)
 COMPLEX G(NMAX,NMAX),GSB2(NSMR/3,3),CDH(0:MLAYER),DELTH,  &
         CDB(NSUBCM,SUB_BLOCK)

      SCX=SBX/NXS
      SCY=SBY/NYS
      SCZ=SBZ/NZS

      NCB=0
      DO L=1,SUB_BLOCK
         IF (L>1) NCB=NCB+NCELL(L-1)
!
         DO I=1,NX(L)
            DO J=1,NY(L)
               DO K=1,NZ(L)
!
                  NN=(I-1)*NY(L)*NZ(L)+(J-1)*NZ(L)+K
                  N1=NN+NCB
                  NN1=(N1-1)*3+1
!
! --- Locate the z-level of the field cell in the super block
!
                  KZ=INT((Z(K,L)+.5*SCZ-(ZSB-.5*SBZ))/SCZ+.2)
!
                  NCB1=0
                  DO LL=1,SUB_BLOCK
                     IF (LL>1) NCB1=NCB1+NCELL(LL-1)
                     DO II=1,NX(LL)
                        DO JJ=1,NY(LL)
                           DO KK=1,NZ(LL)
!
! --- Locate the z'-level of the source cell in the super block
!
                              KKZ=INT((Z(KK,LL)+.5*SCZ-(ZSB-.5*SBZ))  &
                                 /SCZ+.2)
!
! --- Reciprocity to be considered later
!
                              IF (KZ<=KKZ) THEN
!
                              N2=(II-1)*NY(LL)*NZ(LL)+(JJ-1)*NZ(LL)  &
                                 +KK+NCB1
                              NN2=(N2-1)*3+1
!
! --- Identify the relative cell location in the super blocks
!
                              DX=(X(II,LL)-X(I,L))/SCX
! -- For stability
                              IF (DX>=0) IX=INT(DX+.2)+1
                              IF (DX<0) IX=INT(DX-.2)-1
                              IF (IX==-1) IX=1
!
                              DY=(Y(JJ,LL)-Y(J,L))/SCY
! -- For stability
                              IF (DY>=0) JY=INT(DY+.2)+1
                              IF (DY<0) JY=INT(DY-.2)-1
                              IF (JY==-1) JY=1
!
! --- Determine the values of the scattering matrix using the
!   symmetry properties of the Green's functions
!
                              IF (IX>0.AND.JY>0) THEN
!
                              MX=IX
                              MY=JY
!
! --- Note that the order of the array GSB2 determined in the
!   routine GSB is different from that of the scattering matrix
!   formed here in this routine.  Check the routine GSB for
!   more explanations.
!
!   Note here KZ controls the field cell levels
!
                              MM=(KKZ-KZ)*NXS*NYS+(MY-1)*NXS+MX
                              MM2=(3*NXS*NYS*NZS+3*NXS*NYS*NZS-  &
                                 3*NXS*NYS*(KZ-2))*(KZ-1)/2+(MM-1)*3+1
!
                              G(NN1,NN2)=GSB2(MM2,1)
                              G(NN1+1,NN2)=GSB2(MM2+1,1)
                              G(NN1+2,NN2)=GSB2(MM2+2,1)
                              G(NN1,NN2+1)=GSB2(MM2,1+1)
                              G(NN1+1,NN2+1)=GSB2(MM2+1,1+1)
                              G(NN1+2,NN2+1)=GSB2(MM2+2,1+1)
                              G(NN1,NN2+2)=GSB2(MM2,1+2)
                              G(NN1+1,NN2+2)=GSB2(MM2+1,1+2)
                              G(NN1+2,NN2+2)=GSB2(MM2+2,1+2)
!
                              END IF
!
                              IF (IX<0.AND.JY>0) THEN
!
                              MX=-IX
                              MY=JY
!
                              MM=(KKZ-KZ)*NXS*NYS+(MY-1)*NXS+MX
                              MM2=(3*NXS*NYS*NZS+3*NXS*NYS*NZS-  &
                                 3*NXS*NYS*(KZ-2))*(KZ-1)/2+(MM-1)*3+1
!
                              G(NN1,NN2)=GSB2(MM2,1)
                              G(NN1+1,NN2)=-GSB2(MM2+1,1)
                              G(NN1+2,NN2)=-GSB2(MM2+2,1)
                              G(NN1,NN2+1)=-GSB2(MM2,1+1)
                              G(NN1+1,NN2+1)=GSB2(MM2+1,1+1)
                              G(NN1+2,NN2+1)=GSB2(MM2+2,1+1)
                              G(NN1,NN2+2)=-GSB2(MM2,1+2)
                              G(NN1+1,NN2+2)=GSB2(MM2+1,1+2)
                              G(NN1+2,NN2+2)=GSB2(MM2+2,1+2)
!
                              END IF
!
                              IF (IX<0.AND.JY<0) THEN
!
                              MX=-IX
                              MY=-JY
!
                              MM=(KKZ-KZ)*NXS*NYS+(MY-1)*NXS+MX
                              MM2=(3*NXS*NYS*NZS+3*NXS*NYS*NZS-  &
                                 3*NXS*NYS*(KZ-2))*(KZ-1)/2+(MM-1)*3+1
!
                              G(NN1,NN2)=GSB2(MM2,1)
                              G(NN1+1,NN2)=GSB2(MM2+1,1)
                              G(NN1+2,NN2)=-GSB2(MM2+2,1)
                              G(NN1,NN2+1)=GSB2(MM2,1+1)
                              G(NN1+1,NN2+1)=GSB2(MM2+1,1+1)
                              G(NN1+2,NN2+1)=-GSB2(MM2+2,1+1)
                              G(NN1,NN2+2)=-GSB2(MM2,1+2)
                              G(NN1+1,NN2+2)=-GSB2(MM2+1,1+2)
                              G(NN1+2,NN2+2)=GSB2(MM2+2,1+2)
!
                              END IF
!
                              IF (IX>0.AND.JY<0) THEN
!
                              MX=IX
                              MY=-JY
!
                              MM=(KKZ-KZ)*NXS*NYS+(MY-1)*NXS+MX
                              MM2=(3*NXS*NYS*NZS+3*NXS*NYS*NZS-  &
                                 3*NXS*NYS*(KZ-2))*(KZ-1)/2+(MM-1)*3+1
!
                              G(NN1,NN2)=GSB2(MM2,1)
                              G(NN1+1,NN2)=-GSB2(MM2+1,1)
                              G(NN1+2,NN2)=GSB2(MM2+2,1)
                              G(NN1,NN2+1)=-GSB2(MM2,1+1)
                              G(NN1+1,NN2+1)=GSB2(MM2+1,1+1)
                              G(NN1+2,NN2+1)=-GSB2(MM2+2,1+1)
                              G(NN1,NN2+2)=GSB2(MM2,1+2)
                              G(NN1+1,NN2+2)=-GSB2(MM2+1,1+2)
                              G(NN1+2,NN2+2)=GSB2(MM2+2,1+2)
!
                              END IF
                              END IF
!
                           END DO
                        END DO
                     END DO
                  END DO
               END DO
            END DO
         END DO
      END DO
!
! --- Construct the entries of KKZ > KZ using the reciprocity
!
      NCB=0
      DO L=1,SUB_BLOCK
         IF (L>1) NCB=NCB+NCELL(L-1)
!
         DO I=1,NX(L)
            DO J=1,NY(L)
               DO K=1,NZ(L)
!
                  NN=(I-1)*NY(L)*NZ(L)+(J-1)*NZ(L)+K
                  N1=NN+NCB
                  NN1=(N1-1)*3+1
!
                  KZ=INT((Z(K,L)+.5*SCZ-(ZSB-.5*SBZ))/SCZ+.2)
!
                  NCB1=0
                  DO LL=1,SUB_BLOCK
                     IF (LL>1) NCB1=NCB1+NCELL(LL-1)
                     DO II=1,NX(LL)
                        DO JJ=1,NY(LL)
                           DO KK=1,NZ(LL)
!
                              KKZ=INT((Z(KK,LL)+.5*SCZ-(ZSB-.5*SBZ))/SCZ+.2)
!
                              IF (KZ>KKZ) THEN
!
                              N2=(II-1)*NY(LL)*NZ(LL)+(JJ-1)*NZ(LL)+KK+NCB1
                              NN2=(N2-1)*3+1
!
                              G(NN1,NN2)=G(NN2,NN1)
                              G(NN1+1,NN2)=G(NN2,NN1+1)
                              G(NN1+2,NN2)=G(NN2,NN1+2)
                              G(NN1,NN2+1)=G(NN2+1,NN1)
                              G(NN1+1,NN2+1)=G(NN2+1,NN1+1)
                              G(NN1+2,NN2+1)=G(NN2+1,NN1+2)
                              G(NN1,NN2+2)=G(NN2+2,NN1)
                              G(NN1+1,NN2+2)=G(NN2+2,NN1+1)
                              G(NN1+2,NN2+2)=G(NN2+2,NN1+2)
                              END IF
!
                           END DO
                        END DO
                     END DO
                  END DO
               END DO
            END DO
         END DO
      END DO
!
! --- Add the term of cell conductivity to the celf cells
!
      NCB=0
      DO L=1,SUB_BLOCK
         IF (L>1) NCB=NCB+NCELL(L-1)
!
         DO K=1,NZ(L)
            DO IOB=MLAYER-1,0,-1
               IF (Z(K,L)>=ZBND(IOB)) THEN
                  NOB=IOB+1
                  GOTO 20
               END IF
            END DO
            NOB=0
!
   20       DO I=1,NX(L)
               DO J=1,NY(L)
!
                  NN=(I-1)*NY(L)*NZ(L)+(J-1)*NZ(L)+K
                  N1=NN+NCB
                  NN1=(N1-1)*3+1
!
                  DELTH=CDB(NN,L)-CDH(NOB)
!
                  G(NN1,NN1)=1./DELTH+G(NN1,NN1)
                  G(NN1+1,NN1+1)=1./DELTH+G(NN1+1,NN1+1)
                  G(NN1+2,NN1+2)=1./DELTH+G(NN1+2,NN1+2)
!
               END DO
            END DO
         END DO
      END DO
!
      RETURN
      END
!
!**** End of MTRX_SPS
!
!
      SUBROUTINE MTRX_SPSM(SBX,SBY,SBZ,NXS,NYS,NZS,ZSB,NMAX,NXMAX,NYMAX,  &
                           NZMAX,NX1,NY1,NZ1,NCELL1,SUB_BLOCK1,X1,Y1,Z1,    &
                           NX2,NY2,NZ2,NCELL2,SUB_BLOCK2,X2,Y2,Z2,G,NSMR,GSB2)
!
!**** Form the mutual-coupling between substructures
!   using spatial symmetry reduction for
!   non-symmetric structures
!
!   The non-identical elements are pre-computed by the routine
!   GSB and are stored in the array gsb2
!
!
      IMPLICIT NONE
!
      REAL DX,DY,SBX,SBY,SBZ,SCX,SCY,SCZ,ZSB
      INTEGER I,II,IX,J,JJ,JY,K,KK,KKZ,KZ,L,LL,MM,MM1,MM2,MM3,MX,MY,N1
      INTEGER N12,N13,N2,N22,N23,SUB_BLOCK1,SUB_BLOCK2,NCB,NCB1,NMAX,NN,   &
              NN1,NN2,NSMR,NXMAX,NXS,NYMAX,NYS,NZMAX,NZS
!
      COMPLEX GSB2(NSMR/3,3),G(NMAX,NMAX)
      INTEGER NX1(SUB_BLOCK1),NY1(SUB_BLOCK1),NZ1(SUB_BLOCK1),             &
              NCELL1(SUB_BLOCK1),NX2(SUB_BLOCK2),NY2(SUB_BLOCK2),          &
              NZ2(SUB_BLOCK2),NCELL2(SUB_BLOCK2)
      REAL  X1(NXMAX,SUB_BLOCK1),Y1(NYMAX,SUB_BLOCK1),Z1(NZMAX,SUB_BLOCK1), &
            X2(NXMAX,SUB_BLOCK2),Y2(NYMAX,SUB_BLOCK2),Z2(NZMAX,SUB_BLOCK2)
!
      SCX=SBX/NXS
      SCY=SBY/NYS
      SCZ=SBZ/NZS
!
      NCB=0
      DO L=1,SUB_BLOCK1
         IF (L>1) NCB=NCB+NCELL1(L-1)
! ---- Note that here k is the out-loop which may save some time in
!    array reference in identifying elements from GSB2
         DO K=1,NZ1(L)
!
! --- Locate the z-level of the field cell in the super block
!
            KZ=INT((Z1(K,L)+.5*SCZ-(ZSB-.5*SBZ))/SCZ+.2)
!
            DO I=1,NX1(L)
               DO J=1,NY1(L)
                  NN=((I-1)*NY1(L)+(J-1))*NZ1(L)+K
                  N1=NN+NCB
                  NN1=(N1-1)*3+1
                  N12=NN1+1
                  N13=NN1+2
!
                  NCB1=0
                  DO LL=1,SUB_BLOCK2
                     IF (LL>1) NCB1=NCB1+NCELL2(LL-1)
                     DO KK=1,NZ2(LL)
!
! --- Locate the z'-level of the source cell in the super block
!
                        KKZ=INT((Z2(KK,LL)+.5*SCZ-(ZSB-.5*SBZ))/SCZ+.2)
!
                        DO II=1,NX2(LL)
                           DO JJ=1,NY2(LL)
                              N2=((II-1)*NY2(LL)+(JJ-1))*NZ2(LL)+KK+NCB1
                              NN2=(N2-1)*3+1
                              N22=NN2+1
                              N23=NN2+2
!
! --- Identify the relative cell location in the super blocks
!
!   Note that reciprocity is considered here
!
                              IF (KZ<=KKZ) THEN
                              DX=(X2(II,LL)-X1(I,L))/SCX
                              ELSE
                              DX=(X1(I,L)-X2(II,LL))/SCX
                              END IF
                              IF (DX>=0) IX=INT(DX+.2)+1
                              IF (DX<0) IX=INT(DX-.2)-1
                              IF (IX==-1) IX=1
!
                              IF (KZ<=KKZ) THEN
                              DY=(Y2(JJ,LL)-Y1(J,L))/SCY
                              ELSE
                              DY=(Y1(J,L)-Y2(JJ,LL))/SCY
                              END IF
                              IF (DY>=0) JY=INT(DY+.2)+1
                              IF (DY<0) JY=INT(DY-.2)-1
                              IF (JY==-1) JY=1
!
! --- Determine the values of the scattering matrix using the
!   symmetry properties of the Green's functions
!
!      IF(KZ.LE.KKZ) THEN
!           MZ=KKZ
!         ELSE
!           MZ=KZ
!       END IF
!
                              IF (IX>0.AND.JY>0) THEN
!
                              MX=IX
                              MY=JY
!
! --- Note that the order of the array GSB2 determined in the
!   routine GSB is different from that of the scattering matrix
!   formed here in this routine.  Check the routine GSB for
!   more explanations.
!
!   Note that if KZ.LE.KKZ then KZ controls the field cell levels.
!   otherwise KZZ controls the field cell levels..
!
                              IF (KZ<=KKZ) THEN
                              MM=((KKZ-KZ)*NYS+(MY-1))*NXS+MX
                              MM1=(3*NXS*NYS*(2*NZS-(KZ-2)))*(KZ-1)  &
                                 /2+(MM-1)*3+1
                              ELSE
                              MM=((KZ-KKZ)*NYS+(MY-1))*NXS+MX
                              MM1=(3*NXS*NYS*(2*NZS-(KKZ-2)))*(KKZ-1)  &
                                 /2+(MM-1)*3+1
                              END IF
                              MM2=MM1+1
                              MM3=MM1+2
!
                              IF (KZ>KKZ) THEN
!
!--- Use the reciprocity theorem
!
                              G(NN1,NN2)=-GSB2(MM1,1)
                              G(N12,NN2)=-GSB2(MM1,2)
                              G(N13,NN2)=-GSB2(MM1,3)
                              G(NN1,N22)=-GSB2(MM2,1)
                              G(N12,N22)=-GSB2(MM2,2)
                              G(N13,N22)=-GSB2(MM2,3)
                              G(NN1,N23)=-GSB2(MM3,1)
                              G(N12,N23)=-GSB2(MM3,2)
                              G(N13,N23)=-GSB2(MM3,3)
                              ELSE
!
! ---  The sign of G is reversed as compared to those in
!      the routine MTRX_SPSS
!
                              G(NN1,NN2)=-GSB2(MM1,1)
                              G(N12,NN2)=-GSB2(MM2,1)
                              G(N13,NN2)=-GSB2(MM3,1)
                              G(NN1,N22)=-GSB2(MM1,2)
                              G(N12,N22)=-GSB2(MM2,2)
                              G(N13,N22)=-GSB2(MM3,2)
                              G(NN1,N23)=-GSB2(MM1,3)
                              G(N12,N23)=-GSB2(MM2,3)
                              G(N13,N23)=-GSB2(MM3,3)
                              END IF
                              CYCLE
!
                              END IF
!
                              IF (IX<0.AND.JY>0) THEN
!
                              MX=-IX
                              MY=JY
!
!   Note that if KZ.LE.KKZ then KZ controls the field cell levels.
!   otherwise KZZ controls the field cell levels..
!
                              IF (KZ<=KKZ) THEN
                              MM=((KKZ-KZ)*NYS+(MY-1))*NXS+MX
                              MM1=(3*NXS*NYS*(2*NZS-(KZ-2)))*(KZ-1)  &
                                 /2+(MM-1)*3+1
                              ELSE
                              MM=((KZ-KKZ)*NYS+(MY-1))*NXS+MX
                              MM1=(3*NXS*NYS*(2*NZS-(KKZ-2)))*(KKZ-1)  &
                                 /2+(MM-1)*3+1
                              END IF
                              MM2=MM1+1
                              MM3=MM1+2
!
                              IF (KZ>KKZ) THEN
!
!--- Use the reciprocity theorem
!
                              G(NN1,NN2)=-GSB2(MM1,1)
                              G(N12,NN2)=GSB2(MM1,2)
                              G(N13,NN2)=GSB2(MM1,3)
                              G(NN1,N22)=GSB2(MM2,1)
                              G(N12,N22)=-GSB2(MM2,2)
                              G(N13,N22)=-GSB2(MM2,3)
                              G(NN1,N23)=GSB2(MM3,1)
                              G(N12,N23)=-GSB2(MM3,2)
                              G(N13,N23)=-GSB2(MM3,3)
                              ELSE
!
                              G(NN1,NN2)=-GSB2(MM1,1)
                              G(N12,NN2)=GSB2(MM2,1)
                              G(N13,NN2)=GSB2(MM3,1)
                              G(NN1,N22)=GSB2(MM1,2)
                              G(N12,N22)=-GSB2(MM2,2)
                              G(N13,N22)=-GSB2(MM3,2)
                              G(NN1,N23)=GSB2(MM1,3)
                              G(N12,N23)=-GSB2(MM2,3)
                              G(N13,N23)=-GSB2(MM3,3)
                              END IF
                              CYCLE
!
                              END IF
!
                              IF (IX<0.AND.JY<0) THEN
!
                              MX=-IX
                              MY=-JY
!
!   Note that if KZ.LE.KKZ then KZ controls the field cell levels.
!   otherwise KZZ controls the field cell levels..
!
                              IF (KZ<=KKZ) THEN
                              MM=((KKZ-KZ)*NYS+(MY-1))*NXS+MX
                              MM1=(3*NXS*NYS*(2*NZS-(KZ-2)))*(KZ-1)  &
                                 /2+(MM-1)*3+1
                              ELSE
                              MM=((KZ-KKZ)*NYS+(MY-1))*NXS+MX
                              MM1=(3*NXS*NYS*(2*NZS-(KKZ-2)))*(KKZ-1)  &
                                 /2+(MM-1)*3+1
                              END IF
                              MM2=MM1+1
                              MM3=MM1+2
!
                              IF (KZ>KKZ) THEN
!
!--- Use the reciprocity theorem
!
                              G(NN1,NN2)=-GSB2(MM1,1)
                              G(N12,NN2)=-GSB2(MM1,2)
                              G(N13,NN2)=GSB2(MM1,3)
                              G(NN1,N22)=-GSB2(MM2,1)
                              G(N12,N22)=-GSB2(MM2,2)
                              G(N13,N22)=GSB2(MM2,3)
                              G(NN1,N23)=GSB2(MM3,1)
                              G(N12,N23)=GSB2(MM3,2)
                              G(N13,N23)=-GSB2(MM3,3)
                              ELSE
!
                              G(NN1,NN2)=-GSB2(MM1,1)
                              G(N12,NN2)=-GSB2(MM2,1)
                              G(N13,NN2)=GSB2(MM3,1)
                              G(NN1,N22)=-GSB2(MM1,2)
                              G(N12,N22)=-GSB2(MM2,2)
                              G(N13,N22)=GSB2(MM3,2)
                              G(NN1,N23)=GSB2(MM1,3)
                              G(N12,N23)=GSB2(MM2,3)
                              G(N13,N23)=-GSB2(MM3,3)
                              END IF
                              CYCLE
!
                              END IF
!
                              IF (IX>0.AND.JY<0) THEN
!
                              MX=IX
                              MY=-JY
!
!   Note that if KZ.LE.KKZ then KZ controls the field cell levels.
!   otherwise KZZ controls the field cell levels..
!
                              IF (KZ<=KKZ) THEN
                              MM=((KKZ-KZ)*NYS+(MY-1))*NXS+MX
                              MM1=(3*NXS*NYS*(2*NZS-(KZ-2)))*(KZ-1)  &
                                 /2+(MM-1)*3+1
                              ELSE
                              MM=((KZ-KKZ)*NYS+(MY-1))*NXS+MX
                              MM1=(3*NXS*NYS*(2*NZS-(KKZ-2)))*(KKZ-1)  &
                                 /2+(MM-1)*3+1
                              END IF
                              MM2=MM1+1
                              MM3=MM1+2
!
                              IF (KZ>KKZ) THEN
!
!--- Use the reciprocity theorem
!
                              G(NN1,NN2)=-GSB2(MM1,1)
                              G(N12,NN2)=GSB2(MM1,2)
                              G(N13,NN2)=-GSB2(MM1,3)
                              G(NN1,N22)=GSB2(MM2,1)
                              G(N12,N22)=-GSB2(MM2,2)
                              G(N13,N22)=GSB2(MM2,3)
                              G(NN1,N23)=-GSB2(MM3,1)
                              G(N12,N23)=GSB2(MM3,2)
                              G(N13,N23)=-GSB2(MM3,3)
                              ELSE
!
                              G(NN1,NN2)=-GSB2(MM1,1)
                              G(N12,NN2)=GSB2(MM2,1)
                              G(N13,NN2)=-GSB2(MM3,1)
                              G(NN1,N22)=GSB2(MM1,2)
                              G(N12,N22)=-GSB2(MM2,2)
                              G(N13,N22)=GSB2(MM3,2)
                              G(NN1,N23)=-GSB2(MM1,3)
                              G(N12,N23)=GSB2(MM2,3)
                              G(N13,N23)=-GSB2(MM3,3)
                              END IF
!
                              END IF
!
                           END DO
                        END DO
                     END DO
                  END DO
               END DO
            END DO
         END DO
      END DO
!
      RETURN
      END
!
!
      SUBROUTINE MTRX_SPS1B(NMAX,NXMAX,NYMAX,NZMAX,NX,NY,NZ,           &
                            NSUBCM,SUB_BLOCK,X,Y,Z,CLX,CLY,CLZ,CDB,G,    &
                            FRQ,MLAYER,ZBND,RMU,CDH,NZOB,ZOBG,   &
                            NZSR,ZSRG,RHOMIN,DMIN,NHFILM,RRG,NRG,GRHF,    &
                            RRG3,NRG3,GRHF3,GRHO0,GRHO03,CLMN,KCLMN,     &
                            BLMIN,KACC,KUTCRP)
!
!**** Form the scattering matrix of a single block structure using
!   spatial symmetry reductions
!
!   This routine is for one single block only.  This may be
!   useful if the whole structure is not divided into equal-
!   size cells but there is a substructure or more that
!   consists of only one block.  Note that the design of this
!   routine is completely different from the design of other
!   routines like MTRX_SPS and MTRX_SPSS.  Here no extra memory is
!   required.
!
!   The parameter kself controls if the computation for the
!   integration of the primary part of the Green's function
!   for self-cells should be repeated. As the dimensions of
!   the cells within a block is the same, this integration
!   need to be done only once (kself=1) for cells located in
!   the same layer.   The results are stored in a common
!   block in geprm for use for all other cells in this block
!   (kself=0)
!
!Input parameters:
!
!
!
!  All other parameters FRQ, MLAYER, ZBND,  CDH,  NZOB, ZOBG,
!  NZSR, ZSRG, RHOMIN, DMIN, NHFILM, RRG, NRG, GRHF,
!  GRHF3, GRHO0, GRHO03, CLMN, KCLMN, BLMIN, KACC, and KUTCRP are to
!  be passed into routine THR_D_GREEN.  Check THR_D_GREEN for their
!  descriptions.
!
!
!Output parameters:
!
!  G:
!
!
!**** Called by: MAIN_MATRICES
!
!**** Calls:     THR_D_GREEN
!
!
      IMPLICIT NONE
!
      REAL BLMIN,CLMN,CLX1,CLY1,CLZ1,DMIN,DX,DY,FRQ,RHOMIN
      INTEGER I,II,IOB,ISR,IX,J,JJ,JY,K,KACC,KCLMN,KEYG,KK,KSELF,KUTCRP,   &
              L,LL,MLAYER,MM,MM1
      INTEGER MM2,MX,MY,MZ,N1,N2,SUB_BLOCK,NCB,NCB1,NHFILM,NMAX,NN,NN1,  &
              NN2,NOB,NOB1,NRG,NRG3,NSR
      INTEGER NSUBCM,NXMAX,NYMAX,NZMAX,NZOB,NZSR
!
      COMPLEX G(NMAX,NMAX),EXX,EXY,EXZ,EYX,EYY,EYZ,EZX,EZY,EZZ,               &
              CDH(0:MLAYER),CDB(NSUBCM,SUB_BLOCK),DELTH,                                &
              GRHF(11,NHFILM,NZSR,NZOB),GRHO0(4,NZSR,NZOB),                &
              GRHF3(11,NHFILM,NZSR),GRHO03(4,NZSR)
      REAL :: RMU(0:MLAYER)
      INTEGER NX(SUB_BLOCK),NY(SUB_BLOCK),NZ(SUB_BLOCK)
      REAL  X(NXMAX,SUB_BLOCK),Y(NYMAX,SUB_BLOCK),Z(NZMAX,SUB_BLOCK),      &
            CLX(SUB_BLOCK),CLY(SUB_BLOCK),CLZ(SUB_BLOCK),                  &
            ZBND(0:MLAYER),ZOBG(NZOB),                       &
            ZSRG(2,NZSR),RRG(NRG),RRG3(NRG3),PSTION(8)
      COMPLEX ECOMP(9),HCOMP(9)
      EQUIVALENCE (ECOMP(1),EXX),(ECOMP(2),EYX),(ECOMP(3),EZX),   &
                  (ECOMP(4),EXY),(ECOMP(5),EYY),(ECOMP(6),EZY),   &
                  (ECOMP(7),EXZ),(ECOMP(8),EYZ),(ECOMP(9),EZZ)
!
      KEYG=1
!
! ******  SUB_BLOCK=1 FOR THE TIME BEING!!
      IF (SUB_BLOCK>1) THEN
         STOP 'SUB_BLOCK>1 !!'
      END IF
!
      L=1
      NCB=0
      DO K=1,NZ(L)
         DO IOB=MLAYER-1,0,-1
            IF (Z(K,L)>=ZBND(IOB)) THEN
               NOB=IOB+1
               GOTO 50
            END IF
         END DO
         NOB=0
   50    IF (L==1.AND.K==1) NOB1=NOB
!
         LL=1
         NCB1=0
         DO KK=1,NZ(LL)
!
! --- RECIPROCITY CONSIDERATIONS
!
            IF (KK<=K) THEN
!
               DO ISR=MLAYER-1,0,-1
                  IF (Z(KK,LL)>=ZBND(ISR)) THEN
                     NSR=ISR+1
                     GOTO 60
                  END IF
               END DO
               NSR=0
!
   60          DO I=1,NX(L)
                  DO J=1,NY(L)
                     NN=(I-1)*NY(L)*NZ(L)+(J-1)*NZ(L)+K
                     N1=NN+NCB
                     NN1=(N1-1)*3+1
!
! ---- KSELF CONTROLS IF THE CALCULATION FOR THE SELF-CELLS
!    SHOULD BE REPEATED
!
! THIS PART MAY BE DISABLED FOR TEST PUPPOSE IN ORDER TO MATCH
! THE THEORETICAL REDUCTION MTRX_SLV_FACTOR
!
! ---- KSELF controls if the calculation for the self-cells
!    should be repeated
                     IF (I==1.AND.J==1.AND.K==1.OR.NOB1/=NOB.OR.  &
                         NOB1/=NSR) THEN
                        KSELF=1
                     ELSE
                        KSELF=0
                     END IF
!
                     DO II=1,NX(LL)
                        DO JJ=1,NY(LL)
!
                           N2=(II-1)*NY(LL)*NZ(LL)+(JJ-1)*NZ(LL)+KK+NCB1
                           NN2=(N2-1)*3+1
                           CLX1=CLX(LL)
                           CLY1=CLY(LL)
                           CLZ1=CLZ(LL)
!
                           IF (I==1.AND.J==1) THEN
!
                              IF (II==1.AND.JJ==1) MM1=NN1
!
                              PSTION(1)=X(I,L)
                              PSTION(2)=Y(J,L)
                              PSTION(3)=Z(K,L)
                              PSTION(4)=X(II,LL)
                              PSTION(5)=Y(JJ,LL)
                              PSTION(6)=Z(KK,LL)
                              CALL THR_D_GREEN(KEYG,PSTION,FRQ,MLAYER,ZBND,  &
                                 CDH,RMU,NZOB,ZOBG,NZSR,ZSRG,            &
                                 RHOMIN,DMIN,NHFILM,RRG,NRG,GRHF,RRG3,       &
                                 NRG3,GRHF3,GRHO0,GRHO03,CLX1,CLY1,CLZ1,     &
                                 CLMN,KCLMN,BLMIN,KACC,KUTCRP,KSELF,         &
                                 ECOMP,HCOMP)
!
                              G(NN1,NN2)=-EXX
                              G(NN1+1,NN2+1)=-EYY
                              G(NN1+2,NN2+2)=-EZZ
                              G(NN1+1,NN2)=-EYX
                              G(NN1+2,NN2)=-EZX
                              G(NN1,NN2+1)=-EXY
                              G(NN1+2,NN2+1)=-EZY
                              G(NN1,NN2+2)=-EXZ
                              G(NN1+1,NN2+2)=-EYZ
!
                              CYCLE
!
                           END IF
!
                           DX=(X(II,LL)-X(I,L))/CLX1
!     FOR STABILITY
                           IF (DX>=0) IX=INT(DX+.2)+1
                           IF (DX<0) IX=INT(DX-.2)-1
                           IF (IX==-1) IX=1
!
                           DY=(Y(JJ,LL)-Y(J,L))/CLY1
!     FOR STABILITY
                           IF (DY>=0) JY=INT(DY+.2)+1
                           IF (DY<0) JY=INT(DY-.2)-1
                           IF (JY==-1) JY=1
!
                           IF (IX>0.AND.JY>0) THEN
!
                              MX=IX
                              MY=JY
                              MZ=KK
!
                              MM=(MX-1)*NY(L)*NZ(L)+(MY-1)*NZ(L)+MZ
                              MM2=(MM-1)*3+1
!
                              G(NN1,NN2)=G(MM1,MM2)
                              G(NN1+1,NN2)=G(MM1+1,MM2)
                              G(NN1+2,NN2)=G(MM1+2,MM2)
                              G(NN1,NN2+1)=G(MM1,MM2+1)
                              G(NN1+1,NN2+1)=G(MM1+1,MM2+1)
                              G(NN1+2,NN2+1)=G(MM1+2,MM2+1)
                              G(NN1,NN2+2)=G(MM1,MM2+2)
                              G(NN1+1,NN2+2)=G(MM1+1,MM2+2)
                              G(NN1+2,NN2+2)=G(MM1+2,MM2+2)
!
                           END IF
!
                           IF (IX<0.AND.JY>0) THEN
!
                              MX=-IX
                              MY=JY
!              NOT K !!
                              MZ=KK
!
                              MM=(MX-1)*NY(L)*NZ(L)+(MY-1)*NZ(L)+MZ
                              MM2=(MM-1)*3+1
!
                              G(NN1,NN2)=G(MM1,MM2)
                              G(NN1+1,NN2)=-G(MM1+1,MM2)
                              G(NN1+2,NN2)=-G(MM1+2,MM2)
                              G(NN1,NN2+1)=-G(MM1,MM2+1)
                              G(NN1+1,NN2+1)=G(MM1+1,MM2+1)
                              G(NN1+2,NN2+1)=G(MM1+2,MM2+1)
                              G(NN1,NN2+2)=-G(MM1,MM2+2)
                              G(NN1+1,NN2+2)=G(MM1+1,MM2+2)
                              G(NN1+2,NN2+2)=G(MM1+2,MM2+2)
!
                           END IF
!
                           IF (IX<0.AND.JY<0) THEN
!
                              MX=-IX
                              MY=-JY
!              NOT K !!
                              MZ=KK
!
                              MM=(MX-1)*NY(L)*NZ(L)+(MY-1)*NZ(L)+MZ
                              MM2=(MM-1)*3+1
!
                              G(NN1,NN2)=G(MM1,MM2)
                              G(NN1+1,NN2)=G(MM1+1,MM2)
                              G(NN1+2,NN2)=-G(MM1+2,MM2)
                              G(NN1,NN2+1)=G(MM1,MM2+1)
                              G(NN1+1,NN2+1)=G(MM1+1,MM2+1)
                              G(NN1+2,NN2+1)=-G(MM1+2,MM2+1)
                              G(NN1,NN2+2)=-G(MM1,MM2+2)
                              G(NN1+1,NN2+2)=-G(MM1+1,MM2+2)
                              G(NN1+2,NN2+2)=G(MM1+2,MM2+2)
!
                           END IF
!
                           IF (IX>0.AND.JY<0) THEN
!
                              MX=IX
                              MY=-JY
!              NOT K !!
                              MZ=KK
!
                              MM=(MX-1)*NY(L)*NZ(L)+(MY-1)*NZ(L)+MZ
                              MM2=(MM-1)*3+1

                              G(NN1,NN2)=G(MM1,MM2)
                              G(NN1+1,NN2)=-G(MM1+1,MM2)
                              G(NN1+2,NN2)=G(MM1+2,MM2)
                              G(NN1,NN2+1)=-G(MM1,MM2+1)
                              G(NN1+1,NN2+1)=G(MM1+1,MM2+1)
                              G(NN1+2,NN2+1)=-G(MM1+2,MM2+1)
                              G(NN1,NN2+2)=G(MM1,MM2+2)
                              G(NN1+1,NN2+2)=-G(MM1+1,MM2+2)
                              G(NN1+2,NN2+2)=G(MM1+2,MM2+2)
!
                           END IF
!
                        END DO
                     END DO
                  END DO
               END DO
            END IF
!
         END DO
      END DO
!
! ---  RECIPROCITY
!
      L=1
      LL=1
      NCB=0
      NCB1=0
!
      DO K=1,NZ(L)
!
         DO KK=1,NZ(LL)
!
            IF (KK>K) THEN
!
               DO I=1,NX(L)
                  DO J=1,NY(L)
                     NN=(I-1)*NY(L)*NZ(L)+(J-1)*NZ(L)+K
                     N1=NN+NCB
                     NN1=(N1-1)*3+1
!
                     DO II=1,NX(LL)
                        DO JJ=1,NY(LL)
                           N2=(II-1)*NY(LL)*NZ(LL)+(JJ-1)*NZ(LL)+KK+NCB1
                           NN2=(N2-1)*3+1
!
                           G(NN1,NN2)=G(NN2,NN1)
                           G(NN1+1,NN2)=G(NN2,NN1+1)
                           G(NN1+2,NN2)=G(NN2,NN1+2)
                           G(NN1,NN2+1)=G(NN2+1,NN1)
                           G(NN1+1,NN2+1)=G(NN2+1,NN1+1)
                           G(NN1+2,NN2+1)=G(NN2+1,NN1+2)
                           G(NN1,NN2+2)=G(NN2+2,NN1)
                           G(NN1+1,NN2+2)=G(NN2+2,NN1+1)
                           G(NN1+2,NN2+2)=G(NN2+2,NN1+2)
!
                        END DO
                     END DO
                  END DO
               END DO
            END IF
!
         END DO
      END DO

! --- Add the term of cell conductivity to the celf cells
!
      NCB=0
      L=1
!
      DO K=1,NZ(L)
         DO IOB=MLAYER-1,0,-1
            IF (Z(K,L)>=ZBND(IOB)) THEN
               NOB=IOB+1
               GOTO 100
            END IF
         END DO
         NOB=0
!
  100    DO I=1,NX(L)
            DO J=1,NY(L)
!
               NN=(I-1)*NY(L)*NZ(L)+(J-1)*NZ(L)+K
               N1=NN+NCB
               NN1=(N1-1)*3+1
!
               DELTH=CDB(NN,L)-CDH(NOB)
!
               G(NN1,NN1)=1./DELTH+G(NN1,NN1)
               G(NN1+1,NN1+1)=1./DELTH+G(NN1+1,NN1+1)
               G(NN1+2,NN1+2)=1./DELTH+G(NN1+2,NN1+2)
!
            END DO
         END DO
      END DO
!
      RETURN
      END
!
!**** End of MTRX_SPS1B
!
!
      SUBROUTINE MTRX_GS(KSMR,NSMR,NXS,NYS,NZS,SBX,SBY,SBZ,ZSB,          &
                         KS,KDIAG,N,N2,CLX,CLY,CLZ,XSR,YSR,ZSR,          &
                         NMAX,NXMAX,NYMAX,NZMAX,NX,NY,NZ,NCELL,          &
                         NSUBCM,SUB_BLOCK,X,Y,Z,CDB,G,GSB2,FRQ,          &
                         MLAYER,ZBND,RMU,CDH,NZOB,ZOBG,NZSR,     &
                         ZSRG,RHOMIN,DMIN,NHFILM,RRG,NRG,GRHF,           &
                         RRG3,NRG3,GRHF3,GRHO0,GRHO03,CLMN,KCLMN,        &
                         BLMIN,KACC,KUTCRP)
!
!****  Compute the column matrices of the submatrices of the four
!  block matrices obtained by group theoretical reduction
!
!  Three columns for each field cell in the four block matrices
!  are computed at a time
!
!  The parameter KDIAG controls whether the diagonal or off-
!  diagonal submatrices are computed. KDIAG=1: diagonal;
!  =0, off-diagonal.
!
!  The parameter KS controls if the matrices are symmetric.
!  If so, only half of the elements will be computed.  The rests
!  will be determined in the main program.  In case of system
!  iteration is applied, however, the symmetry of the block
!  matrices will not used in forming the matrices except for
!  the diagonal submatrices.  The use of symmetry will save
!  some computer time for the block diagonalization.  The
!  use of symmetry in the actual computation of the Green's
!  functions has been considered in the spatial symmetry
!  reduction already.  The use of symmetry here will save
!  the total computation time by about 10 percent.
!
!  The parameter kself controling the computation for the
!  integration of the primary part of the Green's function
!  for self-cells as in other routines like smtrx1m is dropped
!  since one loop is move to the main program.
!
!
      IMPLICIT NONE
!
      REAL BLMIN,CLMN,CLX,CLY,CLZ,DMIN,DX,DY,FRQ,RHOMIN,SBX,SBY,SBZ,SCX,  &
           SCY,SCZ,XOB1,XSR,XSR1,YOB1
      REAL YSR,YSR1,ZSB,ZSR
      INTEGER I,II,II1,II2,II3,IOB,IX,J,JJ,JJ1,JJ2,JJ3,JY,K,KACC,KCLMN,   &
              KDIAG,KEYG,KKZ,KS
      INTEGER KSELF,KSMR,KUTCRP,KZ,L,LQ,LQC,LQR,MLAYER,MM,MM1,MM2,MM3,    &
              MX,MY,N,N1,N2,SUB_BLOCK,NCB
      INTEGER NHFILM,NMAX,NN,NN1,NOB,NQ,NQC,NQR,NRG,NRG3,NSMR,NSUBCM,     &
              NXMAX,NXS,NYMAX,NYS,NZMAX,NZOB,NZS,NZSR
!
      COMPLEX GSB2(NSMR/3,3)
!
      COMPLEX G(NMAX,N),EXX,EXY,EXZ,EYX,EYY,EYZ,EZX,EZY,EZZ,              &
              CDH(0:MLAYER),CDB(NSUBCM,SUB_BLOCK),DELTH,                          &
              GE(12,12),AA(3,3),GRHF(11,NHFILM,NZSR,NZOB),GRHO0(4,NZSR,NZOB),               &
              GRHF3(11,NHFILM,NZSR),GRHO03(4,NZSR)
      INTEGER NX(SUB_BLOCK),NY(SUB_BLOCK),NZ(SUB_BLOCK),NCELL(SUB_BLOCK)
      REAL  X(NXMAX,SUB_BLOCK),Y(NYMAX,SUB_BLOCK),Z(NZMAX,SUB_BLOCK),     &
            BX(4),BY(4),ZBND(0:MLAYER),UR(3),UC(3,4),       &
            ZOBG(NZOB),ZSRG(2,NZSR),RRG(NRG),RRG3(NRG3),PSTION(8),RMU(0:MLAYER)
      COMPLEX ECOMP(9),HCOMP(9)
      EQUIVALENCE (ECOMP(1),EXX),(ECOMP(2),EYX),(ECOMP(3),EZX),   &
                  (ECOMP(4),EXY),(ECOMP(5),EYY),(ECOMP(6),EZY),   &
                  (ECOMP(7),EXZ),(ECOMP(8),EYZ),(ECOMP(9),EZZ)
!
      DATA BX /-1., 1.,1.,-1./
      DATA BY /-1.,-1.,1., 1./
!
      KEYG=1
!
! --- Compute the cell dimension of the super block
!
      IF (KSMR==1) THEN
         SCX=SBX/NXS
         SCY=SBY/NYS
         SCZ=SBZ/NZS
      END IF
!
! --- KSELF set to be 1
!
      KSELF=1
!
      DO I=1,N
         DO J=1,NMAX
            G(J,I)=(0.,0.)
         END DO
      END DO
!
      IF (KSMR==1) THEN
!
! --- Locate the z'-level of the source cell in the super block
!
!   Note that the source cells are the out loop here.  Thus
!   the design of the program is slightly different from
!   the designs in routines MTRX_SPSS etc.
!
         KKZ=INT((ZSR+.5*SCZ-(ZSB-.5*SBZ))/SCZ+.2)
!
      END IF
!
      NCB=0
      DO L=1,SUB_BLOCK
         IF (L>1) NCB=NCB+NCELL(L-1)
         DO K=1,NZ(L)
!
            IF (KSMR==1) THEN
!
! --- Locate the z-level of the field cell in the super block
!
!   Note that the source cells are the out loop here.  Thus
!   the design of the program is slightly different from
!   the designs in routines MTRX_SPSS etc.
!
               KZ=INT((Z(K,L)+.5*SCZ-(ZSB-.5*SBZ))/SCZ+.2)
!
            END IF
!
            IF (KDIAG==1) THEN
               DO IOB=MLAYER-1,0,-1
                  IF (Z(K,L)>=ZBND(IOB)) THEN
                     NOB=IOB+1
                     GOTO 20
                  END IF
               END DO
               NOB=0
            END IF
!
   20       DO I=1,NX(L)
               DO J=1,NY(L)
                  NN=((I-1)*NY(L)+(J-1))*NZ(L)+K
                  N1=NN+NCB
                  NN1=(N1-1)*3
!
! --- For symmetric matrices only the upper triangles need to
!   be computed
!
                  IF (KS/=1.OR.N1>=N2) THEN
!
                     IF (KDIAG==1.AND.N1==N2) THEN
                        DELTH=CDB(NN,L)-CDH(NOB)
                     END IF
!
! --- Compute the elements for the 4 quarters
!
                     DO LQC=1,4
                        JJ=3*(LQC-1)
                        JJ1=JJ+1
                        JJ2=JJ+2
                        JJ3=JJ+3
                        IF (BX(LQC)>0.) THEN
                           XSR1=XSR
                        ELSE
                           XSR1=-XSR
                        END IF
                        IF (BY(LQC)>0.) THEN
                           YSR1=YSR
                        ELSE
                           YSR1=-YSR
                        END IF
!
                        DO LQR=1,4
                           II=3*(LQR-1)
                           II1=II+1
                           II2=II+2
                           II3=II+3
                           IF (BX(LQR)>0.) THEN
                              XOB1=X(I,L)
                           ELSE
                              XOB1=-X(I,L)
                           END IF
                           IF (BY(LQR)>0.) THEN
                              YOB1=Y(J,L)
                           ELSE
                              YOB1=-Y(J,L)
                           END IF
!
                           IF (LQC==1) THEN
!
                              IF (KSMR==0) THEN
!
                              PSTION(1)=XOB1
                              PSTION(2)=YOB1
                              PSTION(3)=Z(K,L)
                              PSTION(4)=XSR1
                              PSTION(5)=YSR1
                              PSTION(6)=ZSR
                              CALL THR_D_GREEN(KEYG,PSTION,FRQ,MLAYER,ZBND,  &
                                 CDH,RMU,NZOB,ZOBG,NZSR,ZSRG,            &
                                 RHOMIN,DMIN,NHFILM,RRG,NRG,GRHF,RRG3,       &
                                 NRG3,GRHF3,GRHO0,GRHO03,CLX,CLY,CLZ,        &
                                 CLMN,KCLMN,BLMIN,KACC,KUTCRP,KSELF,         &
                                 ECOMP,HCOMP)
!
                              GE(II1,JJ1)=-EXX
                              GE(II2,JJ1)=-EYX
                              GE(II3,JJ1)=-EZX
                              GE(II1,JJ2)=-EXY
                              GE(II2,JJ2)=-EYY
                              GE(II3,JJ2)=-EZY
                              GE(II1,JJ3)=-EXZ
                              GE(II2,JJ3)=-EYZ
                              GE(II3,JJ3)=-EZZ
!
                              CYCLE
!
                              END IF
!
! --- Evaluate the Green's functions by means of spatial symmetry
!   using pre-computed results
!
! --- Identify the relative cell location in the super blocks
!
!   Note that reciprocity is considered here
!
                              IF (KZ<=KKZ) THEN
                              DX=(XSR1-XOB1)/SCX
                              ELSE
                              DX=(XOB1-XSR1)/SCX
                              END IF
                              IF (DX>=0) IX=INT(DX+.2)+1
                              IF (DX<0) IX=INT(DX-.2)-1
                              IF (IX==-1) IX=1
!
                              IF (KZ<=KKZ) THEN
                              DY=(YSR1-YOB1)/SCY
                              ELSE
                              DY=(YOB1-YSR1)/SCY
                              END IF
                              IF (DY>=0) JY=INT(DY+.2)+1
                              IF (DY<0) JY=INT(DY-.2)-1
                              IF (JY==-1) JY=1
!
! --- Determine the values of the scattering matrix using the
!   symmetry properties of the Green's functions
!
!      IF(KZ.LE.KKZ) THEN
!           MZ=KKZ
!         ELSE
!           MZ=KZ
!       END IF
!
                              IF (IX>0.AND.JY>0) THEN
!
                              MX=IX
                              MY=JY
!
! --- Note that the order of the array GSB2 determined in the
!   routine GSB is different from that of the scattering matrix
!   formed here in this routine.  Check the routine GSB for
!   more explanations.
!
!   Note that if KZ.LE.KKZ then KZ controls the field cell levels.
!   otherwise KZZ controls the field cell levels..
!
                              IF (KZ<=KKZ) THEN
                              MM=((KKZ-KZ)*NYS+(MY-1))*NXS+MX
                              MM1=(3*NXS*NYS*(2*NZS-(KZ-2)))*(KZ-1)  &
                                 /2+(MM-1)*3+1
                              ELSE
                              MM=((KZ-KKZ)*NYS+(MY-1))*NXS+MX
                              MM1=(3*NXS*NYS*(2*NZS-(KKZ-2)))*(KKZ-1)  &
                                 /2+(MM-1)*3+1
                              END IF
                              MM2=MM1+1
                              MM3=MM1+2
!
                              IF (KZ>KKZ) THEN
!
!--- Use the reciprocity theorem
!
                              GE(II1,JJ1)=GSB2(MM1,1)
                              GE(II2,JJ1)=GSB2(MM1,2)
                              GE(II3,JJ1)=GSB2(MM1,3)
                              GE(II1,JJ2)=GSB2(MM2,1)
                              GE(II2,JJ2)=GSB2(MM2,2)
                              GE(II3,JJ2)=GSB2(MM2,3)
                              GE(II1,JJ3)=GSB2(MM3,1)
                              GE(II2,JJ3)=GSB2(MM3,2)
                              GE(II3,JJ3)=GSB2(MM3,3)
                              ELSE
!
                              GE(II1,JJ1)=GSB2(MM1,1)
                              GE(II2,JJ1)=GSB2(MM2,1)
                              GE(II3,JJ1)=GSB2(MM3,1)
                              GE(II1,JJ2)=GSB2(MM1,2)
                              GE(II2,JJ2)=GSB2(MM2,2)
                              GE(II3,JJ2)=GSB2(MM3,2)
                              GE(II1,JJ3)=GSB2(MM1,3)
                              GE(II2,JJ3)=GSB2(MM2,3)
                              GE(II3,JJ3)=GSB2(MM3,3)
                              END IF
                              CYCLE
!
                              END IF
!
                              IF (IX<0.AND.JY>0) THEN
!
                              MX=-IX
                              MY=JY
!
!   Note that if KZ.LE.KKZ then KZ controls the field cell levels.
!   otherwise KZZ controls the field cell levels..
!
                              IF (KZ<=KKZ) THEN
                              MM=((KKZ-KZ)*NYS+(MY-1))*NXS+MX
                              MM1=(3*NXS*NYS*(2*NZS-(KZ-2)))*(KZ-1)  &
                                 /2+(MM-1)*3+1
                              ELSE
                              MM=((KZ-KKZ)*NYS+(MY-1))*NXS+MX
                              MM1=(3*NXS*NYS*(2*NZS-(KKZ-2)))*(KKZ-1)  &
                                 /2+(MM-1)*3+1
                              END IF
                              MM2=MM1+1
                              MM3=MM1+2
!
                              IF (KZ>KKZ) THEN
!
!--- Use the reciprocity theorem
!
                              GE(II1,JJ1)=GSB2(MM1,1)
                              GE(II2,JJ1)=-GSB2(MM1,2)
                              GE(II3,JJ1)=-GSB2(MM1,3)
                              GE(II1,JJ2)=-GSB2(MM2,1)
                              GE(II2,JJ2)=GSB2(MM2,2)
                              GE(II3,JJ2)=GSB2(MM2,3)
                              GE(II1,JJ3)=-GSB2(MM3,1)
                              GE(II2,JJ3)=GSB2(MM3,2)
                              GE(II3,JJ3)=GSB2(MM3,3)
                              ELSE
!
                              GE(II1,JJ1)=GSB2(MM1,1)
                              GE(II2,JJ1)=-GSB2(MM2,1)
                              GE(II3,JJ1)=-GSB2(MM3,1)
                              GE(II1,JJ2)=-GSB2(MM1,2)
                              GE(II2,JJ2)=GSB2(MM2,2)
                              GE(II3,JJ2)=GSB2(MM3,2)
                              GE(II1,JJ3)=-GSB2(MM1,3)
                              GE(II2,JJ3)=GSB2(MM2,3)
                              GE(II3,JJ3)=GSB2(MM3,3)
                              END IF
                              CYCLE
!
                              END IF
!
                              IF (IX<0.AND.JY<0) THEN
!
                              MX=-IX
                              MY=-JY
!
!   Note that if KZ.LE.KKZ then KZ controls the field cell levels.
!   otherwise KZZ controls the field cell levels..
!
                              IF (KZ<=KKZ) THEN
                              MM=((KKZ-KZ)*NYS+(MY-1))*NXS+MX
                              MM1=(3*NXS*NYS*(2*NZS-(KZ-2)))*(KZ-1)  &
                                 /2+(MM-1)*3+1
                              ELSE
                              MM=((KZ-KKZ)*NYS+(MY-1))*NXS+MX
                              MM1=(3*NXS*NYS*(2*NZS-(KKZ-2)))*(KKZ-1)  &
                                 /2+(MM-1)*3+1
                              END IF
                              MM2=MM1+1
                              MM3=MM1+2
!
                              IF (KZ>KKZ) THEN
!
!--- Use the reciprocity theorem
!
                              GE(II1,JJ1)=GSB2(MM1,1)
                              GE(II2,JJ1)=GSB2(MM1,2)
                              GE(II3,JJ1)=-GSB2(MM1,3)
                              GE(II1,JJ2)=GSB2(MM2,1)
                              GE(II2,JJ2)=GSB2(MM2,2)
                              GE(II3,JJ2)=-GSB2(MM2,3)
                              GE(II1,JJ3)=-GSB2(MM3,1)
                              GE(II2,JJ3)=-GSB2(MM3,2)
                              GE(II3,JJ3)=GSB2(MM3,3)
                              ELSE
!
                              GE(II1,JJ1)=GSB2(MM1,1)
                              GE(II2,JJ1)=GSB2(MM2,1)
                              GE(II3,JJ1)=-GSB2(MM3,1)
                              GE(II1,JJ2)=GSB2(MM1,2)
                              GE(II2,JJ2)=GSB2(MM2,2)
                              GE(II3,JJ2)=-GSB2(MM3,2)
                              GE(II1,JJ3)=-GSB2(MM1,3)
                              GE(II2,JJ3)=-GSB2(MM2,3)
                              GE(II3,JJ3)=GSB2(MM3,3)
                              END IF
                              CYCLE
!
                              END IF
!
                              IF (IX>0.AND.JY<0) THEN
!
                              MX=IX
                              MY=-JY
!
!   Note that if KZ.LE.KKZ then KZ controls the field cell levels.
!   otherwise KZZ controls the field cell levels..
!
                              IF (KZ<=KKZ) THEN
                              MM=((KKZ-KZ)*NYS+(MY-1))*NXS+MX
                              MM1=(3*NXS*NYS*(2*NZS-(KZ-2)))*(KZ-1)  &
                                 /2+(MM-1)*3+1
                              ELSE
                              MM=((KZ-KKZ)*NYS+(MY-1))*NXS+MX
                              MM1=(3*NXS*NYS*(2*NZS-(KKZ-2)))*(KKZ-1)  &
                                 /2+(MM-1)*3+1
                              END IF
                              MM2=MM1+1
                              MM3=MM1+2
!
                              IF (KZ>KKZ) THEN
!
!--- Use the reciprocity theorem
!
                              GE(II1,JJ1)=GSB2(MM1,1)
                              GE(II2,JJ1)=-GSB2(MM1,2)
                              GE(II3,JJ1)=GSB2(MM1,3)
                              GE(II1,JJ2)=-GSB2(MM2,1)
                              GE(II2,JJ2)=GSB2(MM2,2)
                              GE(II3,JJ2)=-GSB2(MM2,3)
                              GE(II1,JJ3)=GSB2(MM3,1)
                              GE(II2,JJ3)=-GSB2(MM3,2)
                              GE(II3,JJ3)=GSB2(MM3,3)
                              ELSE
!
                              GE(II1,JJ1)=GSB2(MM1,1)
                              GE(II2,JJ1)=-GSB2(MM2,1)
                              GE(II3,JJ1)=GSB2(MM3,1)
                              GE(II1,JJ2)=-GSB2(MM1,2)
                              GE(II2,JJ2)=GSB2(MM2,2)
                              GE(II3,JJ2)=-GSB2(MM3,2)
                              GE(II1,JJ3)=GSB2(MM1,3)
                              GE(II2,JJ3)=-GSB2(MM2,3)
                              GE(II3,JJ3)=GSB2(MM3,3)
                              END IF
                              CYCLE
!
                              END IF
                           END IF
!
! --- Compute the elements of GE for LQC.NE.1 by
!   symmetry relations of the Green's functions.
!   In the following the spatial symmetry is used to
!   determine the Green's functions.  In some cases
!   the reciprocity thereom may also be used.  Note
!   that the reciprocity thereom is not always true
!   here unless the media are isotropic or the four
!   field cells and the four source cells locate in
!   a single anisotropic layer. For this reason the
!   use of the reciprocity thereom is avoid here.
!
!   The following parameters r(-i)(-j) etc. refer to
!   thos in Table 1 of Xiong and Tripp (1992)
!   (spatial symmetry reduction).
!
                           IF (LQC==1) THEN
                           ELSE IF (LQC==3) THEN
!
                              IF (LQR==2) THEN
!
! --- Equivalent to the results of r(-i)(-j).
!
!   May also use reciprocity.
!   II=3, JJ=6
                              GE(II1,JJ1)=GE(10,1)
                              GE(II2,JJ1)=GE(11,1)
                              GE(II3,JJ1)=-GE(12,1)
                              GE(II1,JJ2)=GE(10,2)
                              GE(II2,JJ2)=GE(11,2)
                              GE(II3,JJ2)=-GE(12,2)
                              GE(II1,JJ3)=-GE(10,3)
                              GE(II2,JJ3)=-GE(11,3)
!
                              GE(II3,JJ3)=GE(12,3)
                              ELSE IF (LQR==3) THEN
!
! --- Equivalent to the results of r(-i)(-j).
!
!   Note this is not a self-cell unless N1=N2.
!
!   II=JJ=6
                              GE(II1,JJ1)=GE(1,1)
                              GE(II2,JJ1)=GE(2,1)
                              GE(II3,JJ1)=-GE(3,1)
                              GE(II1,JJ2)=GE(1,2)
                              GE(II2,JJ2)=GE(2,2)
                              GE(II3,JJ2)=-GE(3,2)
                              GE(II1,JJ3)=-GE(1,3)
                              GE(II2,JJ3)=-GE(2,3)
!
                              GE(II3,JJ3)=GE(3,3)
                              ELSE IF (LQR==4) THEN
!
! --- Equivalent to the results of r(-i)(-j).
!
!   II=9, JJ=6.
                              GE(II1,JJ1)=GE(4,1)
                              GE(II2,JJ1)=GE(5,1)
                              GE(II3,JJ1)=-GE(6,1)
                              GE(II1,JJ2)=GE(4,2)
                              GE(II2,JJ2)=GE(5,2)
                              GE(II3,JJ2)=-GE(6,2)
                              GE(II1,JJ3)=-GE(4,3)
                              GE(II2,JJ3)=-GE(5,3)
!
                              GE(II3,JJ3)=GE(6,3)
                              ELSE
!
! --- Equivalent to the results of r(-i)(-j).
!   II=0, JJ=6
                              GE(II1,JJ1)=GE(7,1)
                              GE(II2,JJ1)=GE(8,1)
                              GE(II3,JJ1)=-GE(9,1)
                              GE(II1,JJ2)=GE(7,2)
                              GE(II2,JJ2)=GE(8,2)
                              GE(II3,JJ2)=-GE(9,2)
                              GE(II1,JJ3)=-GE(7,3)
                              GE(II2,JJ3)=-GE(8,3)
!
                              GE(II3,JJ3)=GE(9,3)
                              END IF
                           ELSE IF (LQC==4) THEN
!
                              IF (LQR==2) THEN
!
! --- Equivalent to  ri(-j) or the combination of
!   r(-i)j and r(-i)(-j).
!
!   II=3, JJ=9
                              GE(II1,JJ1)=GE(7,1)
                              GE(II2,JJ1)=-GE(8,1)
                              GE(II3,JJ1)=GE(9,1)
                              GE(II1,JJ2)=-GE(7,2)
                              GE(II2,JJ2)=GE(8,2)
                              GE(II3,JJ2)=-GE(9,2)
                              GE(II1,JJ3)=GE(7,3)
                              GE(II2,JJ3)=-GE(8,3)
!
                              GE(II3,JJ3)=GE(9,3)
                              ELSE IF (LQR==3) THEN
!
! --- Equivalent to the results of ri(-j).
!
!   II=6, JJ=9
                              GE(II1,JJ1)=GE(4,1)
                              GE(II2,JJ1)=-GE(5,1)
                              GE(II3,JJ1)=GE(6,1)
                              GE(II1,JJ2)=-GE(4,2)
                              GE(II2,JJ2)=GE(5,2)
                              GE(II3,JJ2)=-GE(6,2)
                              GE(II1,JJ3)=GE(4,3)
                              GE(II2,JJ3)=-GE(5,3)
!
                              GE(II3,JJ3)=GE(6,3)
                              ELSE IF (LQR==4) THEN
!
! --- Equivalent to the results of ri(-j).
!
!   Note this is not a self-cell unless N1=N2.
!
!   II=JJ=9
                              GE(II1,JJ1)=GE(1,1)
                              GE(II2,JJ1)=-GE(2,1)
                              GE(II3,JJ1)=GE(3,1)
                              GE(II1,JJ2)=-GE(1,2)
                              GE(II2,JJ2)=GE(2,2)
                              GE(II3,JJ2)=-GE(3,2)
                              GE(II1,JJ3)=GE(1,3)
                              GE(II2,JJ3)=-GE(2,3)
                              GE(II3,JJ3)=GE(3,3)
                              ELSE
!
! --- Equivalent to the results of ri(-j).
!
!   II=0, JJ=9.
                              GE(II1,JJ1)=GE(10,1)
                              GE(II2,JJ1)=-GE(11,1)
                              GE(II3,JJ1)=GE(12,1)
                              GE(II1,JJ2)=-GE(10,2)
                              GE(II2,JJ2)=GE(11,2)
                              GE(II3,JJ2)=-GE(12,2)
                              GE(II1,JJ3)=GE(10,3)
                              GE(II2,JJ3)=-GE(11,3)
!
                              GE(II3,JJ3)=GE(12,3)
                              END IF
!
                           ELSE IF (LQR==2) THEN
!
! --- Equivalent to the results of r(-i)j.
!
!   Note this is not a self-cell unless N1=N2.
!
!   II=JJ=3
                              GE(II1,JJ1)=GE(1,1)
                              GE(II2,JJ1)=-GE(2,1)
                              GE(II3,JJ1)=-GE(3,1)
                              GE(II1,JJ2)=-GE(1,2)
                              GE(II2,JJ2)=GE(2,2)
                              GE(II3,JJ2)=GE(3,2)
                              GE(II1,JJ3)=-GE(1,3)
                              GE(II2,JJ3)=GE(2,3)
!
                              GE(II3,JJ3)=GE(3,3)
                           ELSE IF (LQR==3) THEN
!
! --- Equivalent to the results of r(-i)j.
!
!   II=6, JJ=3
                              GE(II1,JJ1)=GE(10,1)
                              GE(II2,JJ1)=-GE(11,1)
                              GE(II3,JJ1)=-GE(12,1)
                              GE(II1,JJ2)=-GE(10,2)
                              GE(II2,JJ2)=GE(11,2)
                              GE(II3,JJ2)=GE(12,2)
                              GE(II1,JJ3)=-GE(10,3)
                              GE(II2,JJ3)=GE(11,3)
!
                              GE(II3,JJ3)=GE(12,3)
                           ELSE IF (LQR==4) THEN
!
! --- Equivalent to r(-i)j or the combination of
!   ri(-j) and r(-i)(-j).
!   II=9, JJ=3
                              GE(II1,JJ1)=GE(7,1)
                              GE(II2,JJ1)=-GE(8,1)
                              GE(II3,JJ1)=-GE(9,1)
                              GE(II1,JJ2)=-GE(7,2)
                              GE(II2,JJ2)=GE(8,2)
                              GE(II3,JJ2)=GE(9,2)
                              GE(II1,JJ3)=-GE(7,3)
                              GE(II2,JJ3)=GE(8,3)
!
                              GE(II3,JJ3)=GE(9,3)
                           ELSE
!
! --- Equivalent to the results of r(-i)j.
!
!   In the following 9 lines II=0, JJ=3
                              GE(II1,JJ1)=GE(4,1)
                              GE(II2,JJ1)=-GE(5,1)
                              GE(II3,JJ1)=-GE(6,1)
                              GE(II1,JJ2)=-GE(4,2)
                              GE(II2,JJ2)=GE(5,2)
                              GE(II3,JJ2)=GE(6,2)
                              GE(II1,JJ3)=-GE(4,3)
                              GE(II2,JJ3)=GE(5,3)
!
                              GE(II3,JJ3)=GE(6,3)
                           END IF
!
                        END DO
                     END DO
!
! --- Add the conductivity term
!
                     IF (N1==N2.AND.KDIAG==1) THEN
                        DO LQR=1,4
                           II=3*(LQR-1)
                           GE(II1,II1)=1./DELTH+GE(II1,II1)
                           GE(II2,II2)=1./DELTH+GE(II2,II2)
                           GE(II3,II3)=1./DELTH+GE(II3,II3)
                        END DO
                     END IF
!
! --- Transform the above elements of the scattering matrix
!   by the unitary matrix, U*Z*UT
!
!   Note that the total number of cells in one quarter and
!   the cell number as required by the parameters PMAX, PR
!   and PC in the routine unitary are not important here
!   as long as PR=PC, since the parameters IROW and JCOL
!   are of no use in the routine
!
!-- The loop LQ controls the contributions to the four
!   block matrices.  To test the validity of the block
!   diagonalization as well as the code, i.e., the off
!   diagonal matrices are zero, just change the loop LQ
!   into two loops, as
!     DO 28 LQ1=1,4
!     DO 28 LQ2=1,4  ,
!   call MTRX_UNITARY in the loop LQR like
!     CALL MTRX_UNITARY (1,LQ1,II,1,LQR,II,UR(II)) ,
!   and call MTRX_UNITARY in the loop LQC like
!     CALL MTRX_UNITARY (1,LQ2,JJ,1,LQR,JJ,UC(JJ)) ,
!   then the following sum should be zero for LQ1.NE.LQ2
!
                     DO LQ=1,4
                        NQ=3*(LQ-1)
!
                        DO II=1,3
                           DO JJ=1,3
                              AA(JJ,II)=(0.,0.)
                           END DO
                        END DO
!
                        DO LQR=1,4
                           NQR=3*(LQR-1)
                           DO II=1,3
! --               PMAX=1
                              CALL MTRX_UNITARY(1,LQ,II,1,LQR,II,UR(II))
                           END DO
                           DO LQC=1,4
                              NQC=3*(LQC-1)
                              IF (LQR==1) THEN
                              DO JJ=1,3
! --                 PMAX=1
                              CALL MTRX_UNITARY(1,LQ,JJ,1,LQC,JJ,UC(JJ,LQC))
                              END DO
                              END IF
!
                              DO JJ=1,3
                              DO II=1,3
!
! --- The following may be simply designed as
!
!      G(NN1+II,NQ+JJ)=G(NN1+II,NQ+JJ)+
!   +                  UR(II)*GE(NQR+II,NQC+JJ)*UC(JJ,LQC)
!
!   The following designed is intended to reduce computation
!   time by avoiding multipications.  Not that the values of
!   UR and UC are either +.5 or -.5
!
                              IF (UR(II)<0) THEN
                              IF (UC(JJ,LQC)>=0) THEN
                              GOTO 22
                              END IF
                              ELSE IF (UC(JJ,LQC)<=0) THEN
                              GOTO 22
                              END IF
!
                              AA(II,JJ)=AA(II,JJ)+GE(NQR+II,NQC+JJ)
                              CYCLE
   22                         AA(II,JJ)=AA(II,JJ)-GE(NQR+II,NQC+JJ)
!
                              END DO
                              END DO
                           END DO
                        END DO
!
                        DO JJ=1,3
                           DO II=1,3
                              G(NN1+II,NQ+JJ)=.25*AA(II,JJ)
                           END DO
                        END DO
!
                     END DO
                  END IF
!
               END DO
            END DO
         END DO
      END DO
!
      RETURN
      END
!
!**** End of MTRX_GS
!
!
      SUBROUTINE MTRX_SMRGS(NXS,NYS,NZS,SBX,SBY,SBZ,ZSB,NMAX,NXMAX,     &
                            NYMAX,NZMAX,NETI,SUB_BLOCKI,NCELLI,XCELLI,  &
                            YCELLI,ZCELLI,NXI,NYI,NZI,NET,SUB_BLOCK,    &
                            NCELL,NX,NY,NZ,X,Y,Z,LQ,G,NSMR,GSB2)
!
!**** Compute the submatrices of one of the four block matrices
!   obtained by group theoretical reduction using spatial
!   symmetry relations
!
!   This routine differs from smtrxgs in that the full matrix
!   is computed for one quadrant.   And the signs of the matrix
!   elements are reversed here in this routine,  instead of in
!   the main program
!
      IMPLICIT NONE
!
      REAL DX,DY,SBX,SBY,SBZ,SCX,SCY,SCZ,XOB1,XSR1,YOB1,YSR1,ZSB
      INTEGER I,II,II1,II2,II3,IL,IX,J,JJ,JJ1,JJ2,JJ3,JL,JY,K,KK,KKZ,KZ,   &
              L,LL,LQ,LQC,LQR,MM,MM1,MM2,MM3,MX,MY,N1,N2,SUB_BLOCK,        &
              SUB_BLOCKI,NCB,NCBI,NET,NETI,NMAX,NN,NN1
      INTEGER NN2,NQC,NQR,NSMR,NXMAX,NXS,NYMAX,NYS,NZMAX,NZS
!
      COMPLEX GSB2(NSMR/3,3)
      COMPLEX G(NMAX,NETI),GE(12,12),AA(3,3)
      INTEGER NX(SUB_BLOCK),NY(SUB_BLOCK),NZ(SUB_BLOCK),NCELL(SUB_BLOCK),   &
              NXI(SUB_BLOCKI),NYI(SUB_BLOCKI),NZI(SUB_BLOCKI),              &
              NCELLI(SUB_BLOCKI)
      REAL  X(NXMAX,SUB_BLOCK),Y(NYMAX,SUB_BLOCK),Z(NZMAX,SUB_BLOCK),       &
            XCELLI(NXMAX,SUB_BLOCKI),YCELLI(NYMAX,SUB_BLOCKI),              &
            ZCELLI(NZMAX,SUB_BLOCKI),                                       &
            BX(4),BY(4),UR(3),UC(3,4)
!
      DATA BX /-1., 1.,1.,-1./
      DATA BY /-1.,-1.,1., 1./
!
! --- Compute the cell dimension of the super block
!
      SCX=SBX/NXS
      SCY=SBY/NYS
      SCZ=SBZ/NZS
!
      DO I=1,NETI
         DO J=1,NET
            G(J,I)=(0.,0.)
         END DO
      END DO
!
      NCBI=0
      DO LL=1,SUB_BLOCKI
         IF (LL>1) NCBI=NCBI+NCELLI(LL-1)
         DO IL=1,NXI(LL)
            DO JL=1,NYI(LL)
               DO KK=1,NZI(LL)
!
                  N2=((IL-1)*NYI(LL)+(JL-1))*NZI(LL)+KK+NCBI
                  NN2=(N2-1)*3
!
! --- Locate the z'-level of the source cell in the super block
!
!   Note that the source cells are the out loop here.  Thus
!   the design of the program is slightly different from
!   the designs in routines MTRX_SPSS etc.
!
                  KKZ=INT((ZCELLI(KK,LL)+.5*SCZ-(ZSB-.5*SBZ))/SCZ+.2)
!
                  NCB=0
                  DO L=1,SUB_BLOCK
                     IF (L>1) NCB=NCB+NCELL(L-1)
                     DO K=1,NZ(L)
!
! --- Locate the z-level of the field cell in the super block
!
!   Note that the source cells are the out loop here.  Thus
!   the design of the program is slightly different from
!   the designs in routines MTRX_SPSS etc.
!
                        KZ=INT((Z(K,L)+.5*SCZ-(ZSB-.5*SBZ))/SCZ+.2)
!
                        DO I=1,NX(L)
                           DO J=1,NY(L)
                              NN=((I-1)*NY(L)+(J-1))*NZ(L)+K
                              N1=NN+NCB
                              NN1=(N1-1)*3
!
! --- Compute the elements for the 4 quarters
!
                              DO LQC=1,4
                              JJ=3*(LQC-1)
                              JJ1=JJ+1
                              JJ2=JJ+2
                              JJ3=JJ+3
                              IF (BX(LQC)>0.) THEN
                              XSR1=XCELLI(IL,LL)
                              ELSE
                              XSR1=-XCELLI(IL,LL)
                              END IF
                              IF (BY(LQC)>0.) THEN
                              YSR1=YCELLI(JL,LL)
                              ELSE
                              YSR1=-YCELLI(JL,LL)
                              END IF
!
                              DO LQR=1,4
                              II=3*(LQR-1)
                              II1=II+1
                              II2=II+2
                              II3=II+3
                              IF (BX(LQR)>0.) THEN
                              XOB1=X(I,L)
                              ELSE
                              XOB1=-X(I,L)
                              END IF
                              IF (BY(LQR)>0.) THEN
                              YOB1=Y(J,L)
                              ELSE
                              YOB1=-Y(J,L)
                              END IF
!
                              IF (LQC==1) THEN
!
! --- Evaluate the Green's functions by means of spatial symmetry
!   using pre-computed results
!
! --- Identify the relative cell location in the super blocks
!
!   Note that reciprocity is considered here
!
                              IF (KZ<=KKZ) THEN
                              DX=(XSR1-XOB1)/SCX
                              ELSE
                              DX=(XOB1-XSR1)/SCX
                              END IF
                              IF (DX>=0) IX=INT(DX+.2)+1
                              IF (DX<0) IX=INT(DX-.2)-1
                              IF (IX==-1) IX=1
!
                              IF (KZ<=KKZ) THEN
                              DY=(YSR1-YOB1)/SCY
                              ELSE
                              DY=(YOB1-YSR1)/SCY
                              END IF
                              IF (DY>=0) JY=INT(DY+.2)+1
                              IF (DY<0) JY=INT(DY-.2)-1
                              IF (JY==-1) JY=1
!
! --- Determine the values of the scattering matrix using the
!   symmetry properties of the Green's functions
!
!      IF(KZ.LE.KKZ) THEN
!           MZ=KKZ
!         ELSE
!           MZ=KZ
!       END IF
!
                              IF (IX>0.AND.JY>0) THEN
!
                              MX=IX
                              MY=JY
!
! --- Note that the order of the array GSB2 determined in the
!   routine GSB is different from that of the scattering matrix
!   formed here in this routine.  Check the routine GSB for
!   more explanations.
!
!   Note that if KZ.LE.KKZ then KZ controls the field cell levels.
!   otherwise KZZ controls the field cell levels..
!
                              IF (KZ<=KKZ) THEN
                              MM=((KKZ-KZ)*NYS+(MY-1))*NXS+MX
                              MM1=(3*NXS*NYS*(2*NZS-(KZ-2)))*(KZ-1)  &
                                 /2+(MM-1)*3+1
                              ELSE
                              MM=((KZ-KKZ)*NYS+(MY-1))*NXS+MX
                              MM1=(3*NXS*NYS*(2*NZS-(KKZ-2)))*(KKZ-1)  &
                                 /2+(MM-1)*3+1
                              END IF
                              MM2=MM1+1
                              MM3=MM1+2
!
                              IF (KZ>KKZ) THEN
!
!--- Use the reciprocity theorem
!
                              GE(II1,JJ1)=GSB2(MM1,1)
                              GE(II2,JJ1)=GSB2(MM1,2)
                              GE(II3,JJ1)=GSB2(MM1,3)
                              GE(II1,JJ2)=GSB2(MM2,1)
                              GE(II2,JJ2)=GSB2(MM2,2)
                              GE(II3,JJ2)=GSB2(MM2,3)
                              GE(II1,JJ3)=GSB2(MM3,1)
                              GE(II2,JJ3)=GSB2(MM3,2)
                              GE(II3,JJ3)=GSB2(MM3,3)
                              ELSE
!
                              GE(II1,JJ1)=GSB2(MM1,1)
                              GE(II2,JJ1)=GSB2(MM2,1)
                              GE(II3,JJ1)=GSB2(MM3,1)
                              GE(II1,JJ2)=GSB2(MM1,2)
                              GE(II2,JJ2)=GSB2(MM2,2)
                              GE(II3,JJ2)=GSB2(MM3,2)
                              GE(II1,JJ3)=GSB2(MM1,3)
                              GE(II2,JJ3)=GSB2(MM2,3)
                              GE(II3,JJ3)=GSB2(MM3,3)
                              END IF
                              CYCLE
!
                              END IF
!
                              IF (IX<0.AND.JY>0) THEN
!
                              MX=-IX
                              MY=JY
!
!   Note that if KZ.LE.KKZ then KZ controls the field cell levels.
!   otherwise KZZ controls the field cell levels..
!
                              IF (KZ<=KKZ) THEN
                              MM=((KKZ-KZ)*NYS+(MY-1))*NXS+MX
                              MM1=(3*NXS*NYS*(2*NZS-(KZ-2)))*(KZ-1)  &
                                 /2+(MM-1)*3+1
                              ELSE
                              MM=((KZ-KKZ)*NYS+(MY-1))*NXS+MX
                              MM1=(3*NXS*NYS*(2*NZS-(KKZ-2)))*(KKZ-1)  &
                                 /2+(MM-1)*3+1
                              END IF
                              MM2=MM1+1
                              MM3=MM1+2
!
                              IF (KZ>KKZ) THEN
!
!--- Use the reciprocity theorem
!
                              GE(II1,JJ1)=GSB2(MM1,1)
                              GE(II2,JJ1)=-GSB2(MM1,2)
                              GE(II3,JJ1)=-GSB2(MM1,3)
                              GE(II1,JJ2)=-GSB2(MM2,1)
                              GE(II2,JJ2)=GSB2(MM2,2)
                              GE(II3,JJ2)=GSB2(MM2,3)
                              GE(II1,JJ3)=-GSB2(MM3,1)
                              GE(II2,JJ3)=GSB2(MM3,2)
                              GE(II3,JJ3)=GSB2(MM3,3)
                              ELSE
!
                              GE(II1,JJ1)=GSB2(MM1,1)
                              GE(II2,JJ1)=-GSB2(MM2,1)
                              GE(II3,JJ1)=-GSB2(MM3,1)
                              GE(II1,JJ2)=-GSB2(MM1,2)
                              GE(II2,JJ2)=GSB2(MM2,2)
                              GE(II3,JJ2)=GSB2(MM3,2)
                              GE(II1,JJ3)=-GSB2(MM1,3)
                              GE(II2,JJ3)=GSB2(MM2,3)
                              GE(II3,JJ3)=GSB2(MM3,3)
                              END IF
                              CYCLE
!
                              END IF
!
                              IF (IX<0.AND.JY<0) THEN
!
                              MX=-IX
                              MY=-JY
!
!   Note that if KZ.LE.KKZ then KZ controls the field cell levels.
!   otherwise KZZ controls the field cell levels..
!
                              IF (KZ<=KKZ) THEN
                              MM=((KKZ-KZ)*NYS+(MY-1))*NXS+MX
                              MM1=(3*NXS*NYS*(2*NZS-(KZ-2)))*(KZ-1)  &
                                 /2+(MM-1)*3+1
                              ELSE
                              MM=((KZ-KKZ)*NYS+(MY-1))*NXS+MX
                              MM1=(3*NXS*NYS*(2*NZS-(KKZ-2)))*(KKZ-1)  &
                                 /2+(MM-1)*3+1
                              END IF
                              MM2=MM1+1
                              MM3=MM1+2
!
                              IF (KZ>KKZ) THEN
!
!--- Use the reciprocity theorem
!
                              GE(II1,JJ1)=GSB2(MM1,1)
                              GE(II2,JJ1)=GSB2(MM1,2)
                              GE(II3,JJ1)=-GSB2(MM1,3)
                              GE(II1,JJ2)=GSB2(MM2,1)
                              GE(II2,JJ2)=GSB2(MM2,2)
                              GE(II3,JJ2)=-GSB2(MM2,3)
                              GE(II1,JJ3)=-GSB2(MM3,1)
                              GE(II2,JJ3)=-GSB2(MM3,2)
                              GE(II3,JJ3)=GSB2(MM3,3)
                              ELSE
!
                              GE(II1,JJ1)=GSB2(MM1,1)
                              GE(II2,JJ1)=GSB2(MM2,1)
                              GE(II3,JJ1)=-GSB2(MM3,1)
                              GE(II1,JJ2)=GSB2(MM1,2)
                              GE(II2,JJ2)=GSB2(MM2,2)
                              GE(II3,JJ2)=-GSB2(MM3,2)
                              GE(II1,JJ3)=-GSB2(MM1,3)
                              GE(II2,JJ3)=-GSB2(MM2,3)
                              GE(II3,JJ3)=GSB2(MM3,3)
                              END IF
                              CYCLE
!
                              END IF
!
                              IF (IX>0.AND.JY<0) THEN
!
                              MX=IX
                              MY=-JY
!
!   Note that if KZ.LE.KKZ then KZ controls the field cell levels.
!   otherwise KZZ controls the field cell levels..
!
                              IF (KZ<=KKZ) THEN
                              MM=((KKZ-KZ)*NYS+(MY-1))*NXS+MX
                              MM1=(3*NXS*NYS*(2*NZS-(KZ-2)))*(KZ-1)  &
                                 /2+(MM-1)*3+1
                              ELSE
                              MM=((KZ-KKZ)*NYS+(MY-1))*NXS+MX
                              MM1=(3*NXS*NYS*(2*NZS-(KKZ-2)))*(KKZ-1)  &
                                 /2+(MM-1)*3+1
                              END IF
                              MM2=MM1+1
                              MM3=MM1+2
!
                              IF (KZ>KKZ) THEN
!
!--- Use the reciprocity theorem
!
                              GE(II1,JJ1)=GSB2(MM1,1)
                              GE(II2,JJ1)=-GSB2(MM1,2)
                              GE(II3,JJ1)=GSB2(MM1,3)
                              GE(II1,JJ2)=-GSB2(MM2,1)
                              GE(II2,JJ2)=GSB2(MM2,2)
                              GE(II3,JJ2)=-GSB2(MM2,3)
                              GE(II1,JJ3)=GSB2(MM3,1)
                              GE(II2,JJ3)=-GSB2(MM3,2)
                              GE(II3,JJ3)=GSB2(MM3,3)
                              ELSE
!
                              GE(II1,JJ1)=GSB2(MM1,1)
                              GE(II2,JJ1)=-GSB2(MM2,1)
                              GE(II3,JJ1)=GSB2(MM3,1)
                              GE(II1,JJ2)=-GSB2(MM1,2)
                              GE(II2,JJ2)=GSB2(MM2,2)
                              GE(II3,JJ2)=-GSB2(MM3,2)
                              GE(II1,JJ3)=GSB2(MM1,3)
                              GE(II2,JJ3)=-GSB2(MM2,3)
                              GE(II3,JJ3)=GSB2(MM3,3)
                              END IF
                              CYCLE
!
                              END IF
                              END IF
!
! --- Compute the elements of GE for LQC.NE.1 by
!   symmetry relations of the Green's functions.
!   In the following the spatial symmetry is used to
!   determine the Green's functions.  In some cases
!   the reciprocity thereom may also be used.  Note
!   that the reciprocity thereom is not always true
!   here unless the media are isotropic or the four
!   field cells and the four source cells locate in
!   a single anisotropic layer. For this reason the
!   use of the reciprocity thereom is avoid here.
!
!   The following parameters r(-i)(-j) etc. refer to
!   thos in Table 1 of Xiong and Tripp (1992)
!   (spatial symmetry reduction).
!
                              IF (LQC==1) THEN
                              ELSE IF (LQC==3) THEN
!
                              IF (LQR==2) THEN
!
! --- Equivalent to the results of r(-i)(-j).
!
!   May also use reciprocity.
!   II=3, JJ=6
                              GE(II1,JJ1)=GE(10,1)
                              GE(II2,JJ1)=GE(11,1)
                              GE(II3,JJ1)=-GE(12,1)
                              GE(II1,JJ2)=GE(10,2)
                              GE(II2,JJ2)=GE(11,2)
                              GE(II3,JJ2)=-GE(12,2)
                              GE(II1,JJ3)=-GE(10,3)
                              GE(II2,JJ3)=-GE(11,3)
!
                              GE(II3,JJ3)=GE(12,3)
                              ELSE IF (LQR==3) THEN
!
! --- Equivalent to the results of r(-i)(-j).
!
!   Note this is not a self-cell unless N1=N2.
!
!   II=JJ=6
                              GE(II1,JJ1)=GE(1,1)
                              GE(II2,JJ1)=GE(2,1)
                              GE(II3,JJ1)=-GE(3,1)
                              GE(II1,JJ2)=GE(1,2)
                              GE(II2,JJ2)=GE(2,2)
                              GE(II3,JJ2)=-GE(3,2)
                              GE(II1,JJ3)=-GE(1,3)
                              GE(II2,JJ3)=-GE(2,3)
!
                              GE(II3,JJ3)=GE(3,3)
                              ELSE IF (LQR==4) THEN
!
! --- Equivalent to the results of r(-i)(-j).
!
!   II=9, JJ=6.
                              GE(II1,JJ1)=GE(4,1)
                              GE(II2,JJ1)=GE(5,1)
                              GE(II3,JJ1)=-GE(6,1)
                              GE(II1,JJ2)=GE(4,2)
                              GE(II2,JJ2)=GE(5,2)
                              GE(II3,JJ2)=-GE(6,2)
                              GE(II1,JJ3)=-GE(4,3)
                              GE(II2,JJ3)=-GE(5,3)
!
                              GE(II3,JJ3)=GE(6,3)
                              ELSE
!
! --- Equivalent to the results of r(-i)(-j).
!   II=0, JJ=6
                              GE(II1,JJ1)=GE(7,1)
                              GE(II2,JJ1)=GE(8,1)
                              GE(II3,JJ1)=-GE(9,1)
                              GE(II1,JJ2)=GE(7,2)
                              GE(II2,JJ2)=GE(8,2)
                              GE(II3,JJ2)=-GE(9,2)
                              GE(II1,JJ3)=-GE(7,3)
                              GE(II2,JJ3)=-GE(8,3)
!
                              GE(II3,JJ3)=GE(9,3)
                              END IF
                              ELSE IF (LQC==4) THEN
!
                              IF (LQR==2) THEN
!
! --- Equivalent to  ri(-j) or the combination of
!   r(-i)j and r(-i)(-j).
!
!   II=3, JJ=9
                              GE(II1,JJ1)=GE(7,1)
                              GE(II2,JJ1)=-GE(8,1)
                              GE(II3,JJ1)=GE(9,1)
                              GE(II1,JJ2)=-GE(7,2)
                              GE(II2,JJ2)=GE(8,2)
                              GE(II3,JJ2)=-GE(9,2)
                              GE(II1,JJ3)=GE(7,3)
                              GE(II2,JJ3)=-GE(8,3)
!
                              GE(II3,JJ3)=GE(9,3)
                              ELSE IF (LQR==3) THEN
!
! --- Equivalent to the results of ri(-j).
!
!   II=6, JJ=9
                              GE(II1,JJ1)=GE(4,1)
                              GE(II2,JJ1)=-GE(5,1)
                              GE(II3,JJ1)=GE(6,1)
                              GE(II1,JJ2)=-GE(4,2)
                              GE(II2,JJ2)=GE(5,2)
                              GE(II3,JJ2)=-GE(6,2)
                              GE(II1,JJ3)=GE(4,3)
                              GE(II2,JJ3)=-GE(5,3)
!
                              GE(II3,JJ3)=GE(6,3)
                              ELSE IF (LQR==4) THEN
!
! --- Equivalent to the results of ri(-j).
!
!   Note this is not a self-cell unless N1=N2.
!
!   II=JJ=9
                              GE(II1,JJ1)=GE(1,1)
                              GE(II2,JJ1)=-GE(2,1)
                              GE(II3,JJ1)=GE(3,1)
                              GE(II1,JJ2)=-GE(1,2)
                              GE(II2,JJ2)=GE(2,2)
                              GE(II3,JJ2)=-GE(3,2)
                              GE(II1,JJ3)=GE(1,3)
                              GE(II2,JJ3)=-GE(2,3)
                              GE(II3,JJ3)=GE(3,3)
                              ELSE
!
! --- Equivalent to the results of ri(-j).
!
!   II=0, JJ=9.
                              GE(II1,JJ1)=GE(10,1)
                              GE(II2,JJ1)=-GE(11,1)
                              GE(II3,JJ1)=GE(12,1)
                              GE(II1,JJ2)=-GE(10,2)
                              GE(II2,JJ2)=GE(11,2)
                              GE(II3,JJ2)=-GE(12,2)
                              GE(II1,JJ3)=GE(10,3)
                              GE(II2,JJ3)=-GE(11,3)
!
                              GE(II3,JJ3)=GE(12,3)
                              END IF
!
                              ELSE IF (LQR==2) THEN
!
! --- Equivalent to the results of r(-i)j.
!
!   Note this is not a self-cell unless N1=N2.
!
!   II=JJ=3
                              GE(II1,JJ1)=GE(1,1)
                              GE(II2,JJ1)=-GE(2,1)
                              GE(II3,JJ1)=-GE(3,1)
                              GE(II1,JJ2)=-GE(1,2)
                              GE(II2,JJ2)=GE(2,2)
                              GE(II3,JJ2)=GE(3,2)
                              GE(II1,JJ3)=-GE(1,3)
                              GE(II2,JJ3)=GE(2,3)
!
                              GE(II3,JJ3)=GE(3,3)
                              ELSE IF (LQR==3) THEN
!
! --- Equivalent to the results of r(-i)j.
!
!   II=6, JJ=3
                              GE(II1,JJ1)=GE(10,1)
                              GE(II2,JJ1)=-GE(11,1)
                              GE(II3,JJ1)=-GE(12,1)
                              GE(II1,JJ2)=-GE(10,2)
                              GE(II2,JJ2)=GE(11,2)
                              GE(II3,JJ2)=GE(12,2)
                              GE(II1,JJ3)=-GE(10,3)
                              GE(II2,JJ3)=GE(11,3)
!
                              GE(II3,JJ3)=GE(12,3)
                              ELSE IF (LQR==4) THEN
!
! --- Equivalent to r(-i)j or the combination of
!   ri(-j) and r(-i)(-j).
!   II=9, JJ=3
                              GE(II1,JJ1)=GE(7,1)
                              GE(II2,JJ1)=-GE(8,1)
                              GE(II3,JJ1)=-GE(9,1)
                              GE(II1,JJ2)=-GE(7,2)
                              GE(II2,JJ2)=GE(8,2)
                              GE(II3,JJ2)=GE(9,2)
                              GE(II1,JJ3)=-GE(7,3)
                              GE(II2,JJ3)=GE(8,3)
!
                              GE(II3,JJ3)=GE(9,3)
                              ELSE
!
! --- Equivalent to the results of r(-i)j.
!
!   In the following 9 lines II=0, JJ=3
                              GE(II1,JJ1)=GE(4,1)
                              GE(II2,JJ1)=-GE(5,1)
                              GE(II3,JJ1)=-GE(6,1)
                              GE(II1,JJ2)=-GE(4,2)
                              GE(II2,JJ2)=GE(5,2)
                              GE(II3,JJ2)=GE(6,2)
                              GE(II1,JJ3)=-GE(4,3)
                              GE(II2,JJ3)=GE(5,3)
!
                              GE(II3,JJ3)=GE(6,3)
                              END IF
!
                              END DO
                              END DO
!
! --- Transform the above elements of the scattering matrix
!   by the unitary matrix, U*Z*UT
!
!   Note that the total number of cells in one quarter and
!   the cell number as required by the parameters PMAX, PR
!   and PC in the routine unitary are not important here
!   as long as PR=PC, since the parameters IROW and JCOL
!   are of no use in the routine
!
!-- The parameter LQ controls the contributions to the four
!   block matrices.  LQ need be passed into this routine.
!
                              DO II=1,3
                              DO JJ=1,3
                              AA(JJ,II)=(0.,0.)
                              END DO
                              END DO
!
                              DO LQR=1,4
                              NQR=3*(LQR-1)
                              DO II=1,3
! --               PMAX=1
                              CALL MTRX_UNITARY(1,LQ,II,1,LQR,II,UR(II))
                              END DO
                              DO LQC=1,4
                              NQC=3*(LQC-1)
                              IF (LQR==1) THEN
                              DO JJ=1,3
! --                 PMAX=1
                              CALL MTRX_UNITARY(1,LQ,JJ,1,LQC,JJ,UC(JJ,LQC))
                              END DO
                              END IF
!
                              DO JJ=1,3
                              DO II=1,3
!
! --- Not that the values of UR and UC are either +.5 or -.5
!
                              IF (UR(II)<0) THEN
                              IF (UC(JJ,LQC)>=0) THEN
                              GOTO 2
                              END IF
                              ELSE IF (UC(JJ,LQC)<=0) THEN
                              GOTO 2
                              END IF
!
                              AA(II,JJ)=AA(II,JJ)+GE(NQR+II,NQC+JJ)
                              CYCLE
    2                         AA(II,JJ)=AA(II,JJ)-GE(NQR+II,NQC+JJ)
!
                              END DO
                              END DO
                              END DO
                              END DO
!
! ---  The sign of G is reversed
!
                              DO JJ=1,3
                              DO II=1,3
                              G(NN1+II,NN2+JJ)=-.25*AA(II,JJ)
                              END DO
                              END DO
!
                           END DO
                        END DO
                     END DO
                  END DO
               END DO
            END DO
         END DO
      END DO
!
      RETURN
      END
!
!
! This routine factors a square matrix by either LU or UDUT
! decomposition. The condition numbers of the matrix calculated
! by the maxmal column norm and maximal row norm will be
! returned if kcond=1. KSYM controls if whether the matrix is
! symmetric or not. This routine also returns the maxmal column
! and maximal row norms of the original matrix, anorm1 and
! anorm3, which would be useful for analysing the accuracy of
! the oringinal matrix (A) computed with different accuracy
! levels. The condition numbers may serve the same
! purpose better since they are functions of the matrix norms
! as well as the condition numbers themselves.  The routine
! also returns the maximal diagonal element of the matrix which
! should also be the maximal element of the matrix
!
      SUBROUTINE MTRX_SLV_FACTOR(NLG,A,NMAX,N,IND,V,C1,AI,KSYM,         &
                                 KCOND,ANORM1,ANORM3,COND1,COND3,AEM)
!
      IMPLICIT NONE
!
      INTEGER NLG
      REAL AEM,AINRM1,AINRM3,ANORM1,ANORM3,COND1,COND3,DD
      INTEGER I,INFO,J,JJ,KCOND,KSYM,N,NMAX
      COMPLEX A(NMAX,N),C1(N)
      INTEGER IND(N)
      REAL  V(N),AI(N)
      DOUBLE PRECISION AM
!
! --- Compute the norms of A
!
      IF (KCOND==1) THEN
!
! --- Maximal column norm (l_1 norm):
         ANORM1=0.
         AEM=0.
         DO J=1,N
!
! ---   Compute the maximal diagonal element
!
            AEM=AMAX1(AEM,ABS(A(J,J)))
!
            AM=0.D0
            DO I=1,N
               AM=AM+ABS(A(I,J))
            END DO
            ANORM1=AMAX1(ANORM1,SNGL(AM))
         END DO
!
! --- Maximal row norm (l_infinity norm):
         ANORM3=0.
         DO I=1,N
            AM=0.D0
            DO J=1,N
               AM=AM+ABS(A(I,J))
            END DO
            ANORM3=AMAX1(ANORM3,SNGL(AM))
         END DO
      END IF
!
! ---- Performing UDUT or LU decompistion
!
      IF (KSYM==1) THEN
         CALL MTRX_SLV_CSIFA(A,NMAX,N,IND,INFO)
      ELSE
         CALL MTRX_SLV_LUDCMP(NLG,A,NMAX,N,IND,V,DD)
      END IF
!
! ----  Compute the norm of inverse A
!
      IF (KCOND/=0) THEN
         AINRM1=0.
         DO I=1,N
            AI(I)=0.
         END DO
!
         DO J=1,N
!
            DO JJ=1,N
               C1(JJ)=(0.,0.)
            END DO
            C1(J)=(1.,0.)
!
! --- Compute the column of the inverse of A
            IF (KSYM==1) THEN
               CALL MTRX_SLV_CSISL(A,NMAX,N,IND,C1)
            ELSE
               CALL MTRX_SLV_LUBKSB(A,C1,NMAX,N,IND)
            END IF
!
! ---- Calculate the maximal column norm
            AM=0.D0
            DO I=1,N
               AM=AM+ABS(C1(I))
            END DO
            AINRM1=AMAX1(AINRM1,SNGL(AM))
!
! ---- Calculate the row norms
            DO I=1,N
               AI(I)=AI(I)+ABS(C1(I))
            END DO
         END DO
!
! --- Compute the maximal row norm of inverse A
         AINRM3=0.
         DO I=1,N
            AINRM3=AMAX1(AINRM3,AI(I))
         END DO
!
! Determine the condition number
!
         COND1=ANORM1*AINRM1
         COND3=ANORM3*AINRM3
      END IF
      RETURN
      END
!
!
!           LU matrix decomposition
!  (Source: Press et al., Numerical recipes)
!
      SUBROUTINE MTRX_SLV_LUDCMP(NLG,A,NP,N,INDEX,VV,D)
!
      IMPLICIT NONE
!
      INTEGER NLG
      REAL AAMAX,D,DUM,TINY
      INTEGER I,IMAX,J,K,N,NP
      COMPLEX A(NP,NP),SUM,T
      REAL VV(N)
      INTEGER INDEX(N)
      PARAMETER (TINY=1.E-30)
      D=1.
      DO I=1,N
         AAMAX=0.
         DO J=1,N
            IF (ABS(A(I,J))>AAMAX) AAMAX=ABS(A(I,J))
         END DO
         IF (ABS(AAMAX)<TINY) THEN
            WRITE (NLG,'(A)') '  Singular matrix'
            STOP
         END IF
         VV(I)=1./AAMAX
      END DO
      DO J=1,N
         DO I=1,J-1
            SUM=A(I,J)
            DO K=1,I-1
               SUM=SUM-A(I,K)*A(K,J)
            END DO
            A(I,J)=SUM
         END DO
         AAMAX=0.
         DO I=J,N
            SUM=A(I,J)
            DO K=1,J-1
               SUM=SUM-A(I,K)*A(K,J)
            END DO
            A(I,J)=SUM
            DUM=VV(I)*ABS(SUM)
            IF (DUM>=AAMAX) THEN
               IMAX=I
               AAMAX=DUM
            END IF
         END DO
         IF (J/=IMAX) THEN
            DO K=1,N
               T=A(IMAX,K)
               A(IMAX,K)=A(J,K)
               A(J,K)=T
            END DO
            D=-D
            VV(IMAX)=VV(J)
         END IF
         INDEX(J)=IMAX
         IF (ABS(A(J,J))<TINY) A(J,J)=cmplx(TINY,0.)
         IF (J/=N) THEN
            T=1./A(J,J)
            DO I=J+1,N
               A(I,J)=A(I,J)*T
            END DO
         END IF
      END DO
      RETURN
      END
!
!
!  Solving linear equations by LU decomposition: backsubstitution
!  (Source: Press et al., Numerical recipes)
!
      SUBROUTINE MTRX_SLV_LUBKSB(A,B,NP,N,INDEX)
!
      IMPLICIT NONE
!
      INTEGER I,II,J,LL,N,NP
      REAL TINY
      COMPLEX A(NP,NP),B(N),SUM
      INTEGER INDEX(N)
      PARAMETER (TINY=1.E-30)
      II=0
      DO I=1,N
         LL=INDEX(I)
         SUM=B(LL)
         B(LL)=B(I)
         IF (II/=0) THEN
            DO J=II,I-1
               SUM=SUM-A(I,J)*B(J)
            END DO
         ELSE IF (ABS(SUM)>TINY) THEN
            II=I
         END IF
         B(I)=SUM
      END DO
      DO I=N,1,-1
         SUM=B(I)
         IF (I<N) THEN
            DO J=I+1,N
               SUM=SUM-A(I,J)*B(J)
            END DO
         END IF
         B(I)=SUM/A(I,I)
      END DO
      RETURN
      END
!
!
!   UDUT DECOMPSTION OF A SYMMETRICAL INDEFINITE COMPLEX MATRIX
!
!  (Source: Dongarra et al., Linpack user' guide)
!
      SUBROUTINE MTRX_SLV_CSIFA(A,LDA,N,KPVT,INFO)
!
      IMPLICIT NONE
!
      REAL ABSAKK,ALPHA,COLMAX,ROWMAX,TINY
      INTEGER IMAX,IMAXP1,INFO,ISAMAX,J,JJ,JMAX,K,KM1,KM2,KSTEP,LDA,N
      COMPLEX  A(LDA,N),T,MULK,MULKM1,AK,AKM1,DENOM,BK,BKM1
      INTEGER KPVT(N)
      LOGICAL SWAP
      PARAMETER (TINY=1.E-30)
!
      ALPHA=(1.+SQRT(17.))/8.
      INFO=0
!
      K=N
!
  100 IF (K/=0) THEN
         IF (K>1) THEN
!
            KM1=K-1
            ABSAKK=ABS(A(K,K))
!
            IMAX=ISAMAX(K-1,A(1,K),1)
            COLMAX=ABS(A(IMAX,K))
            IF (ABSAKK<ALPHA*COLMAX) THEN
!
               ROWMAX=0.
               IMAXP1=IMAX+1
               DO J=IMAXP1,K
                  ROWMAX=AMAX1(ROWMAX,ABS(A(IMAX,J)))
               END DO
               IF (IMAX/=1) THEN
                  JMAX=ISAMAX(IMAX-1,A(1,IMAX),1)
                  ROWMAX=AMAX1(ROWMAX,ABS(A(JMAX,IMAX)))
               END IF
               IF (ABS(A(IMAX,IMAX))>=ALPHA*ROWMAX) THEN
                  KSTEP=1
                  SWAP=.TRUE.
               ELSE IF (ABSAKK<ALPHA*COLMAX*(COLMAX/ROWMAX)) THEN
                  KSTEP=2
                  SWAP=IMAX/=KM1
               ELSE
                  KSTEP=1
                  SWAP=.FALSE.
               END IF
            ELSE
               KSTEP=1
               SWAP=.FALSE.
            END IF
            IF (ABS(AMAX1(ABSAKK,COLMAX))<=TINY) THEN
!
               KPVT(K)=K
               INFO=K
            ELSE IF (KSTEP==2) THEN
!
               IF (SWAP) THEN
!
                  CALL MTRX_SLV_SSWAP(IMAX,A(1,IMAX),1,A(1,K-1),1)
                  DO JJ=IMAX,KM1
                     J=KM1+IMAX-JJ
                     T=A(J,K-1)
                     A(J,K-1)=A(IMAX,J)
                     A(IMAX,J)=T
                  END DO
                  T=A(K-1,K)
                  A(K-1,K)=A(IMAX,K)
                  A(IMAX,K)=T
               END IF
!
               KM2=K-2
               IF (KM2/=0) THEN
                  AK=A(K,K)/A(K-1,K)
                  AKM1=A(K-1,K-1)/A(K-1,K)
                  DENOM=(1.,0.)-AK*AKM1
                  DO JJ=1,KM2
                     J=KM1-JJ
                     BK=A(J,K)/A(K-1,K)
                     BKM1=A(J,K-1)/A(K-1,K)
                     MULK=(AKM1*BK-BKM1)/DENOM
                     MULKM1=(AK*BKM1-BK)/DENOM
                     T=MULK
                     CALL MTRX_SLV_SAXPY(J,T,A(1,K),1,A(1,J),1)
                     T=MULKM1
                     CALL MTRX_SLV_SAXPY(J,T,A(1,K-1),1,A(1,J),1)
                     A(J,K)=MULK
                     A(J,K-1)=MULKM1
                  END DO
               END IF
!
               KPVT(K)=1-K
               IF (SWAP) KPVT(K)=-IMAX
               KPVT(K-1)=KPVT(K)
            ELSE
!
               IF (SWAP) THEN
!
                  CALL MTRX_SLV_SSWAP(IMAX,A(1,IMAX),1,A(1,K),1)
                  DO JJ=IMAX,K
                     J=K+IMAX-JJ
                     T=A(J,K)
                     A(J,K)=A(IMAX,J)
                     A(IMAX,J)=T
                  END DO
               END IF
!
               DO JJ=1,KM1
                  J=K-JJ
                  MULK=-A(J,K)/A(K,K)
                  T=MULK
                  CALL MTRX_SLV_SAXPY(J,T,A(1,K),1,A(1,J),1)
                  A(J,K)=MULK
               END DO
!
               KPVT(K)=K
               IF (SWAP) KPVT(K)=IMAX
            END IF
            K=K-KSTEP
            GOTO 100
         ELSE
            KPVT(1)=1
!
            IF (ABS(A(1,1))<TINY) INFO=1
         END IF
      END IF
      RETURN
      END
!
!
! SOLVING LINEAR EQUATION BY BACKSUBSTITUTE USING UDUT DECOMPSTION
!
      SUBROUTINE MTRX_SLV_CSISL(A,LDA,N,KPVT,B)
!
      IMPLICIT NONE
!
      INTEGER K,KP,LDA,N
      COMPLEX A(LDA,N),B(N),TEMP,AK,AKM1,BK,BKM1,DENOM,SDOT
      INTEGER KPVT(N)
!
      K=N
  100 IF (K==0) THEN
!
         K=1
  150    IF (K<=N) THEN
            IF (KPVT(K)<0) THEN
!
               IF (K/=1) THEN
!
                  B(K)=B(K)+SDOT(K-1,A(1,K),1,B(1),1)
                  B(K+1)=B(K+1)+SDOT(K-1,A(1,K+1),1,B(1),1)
                  KP=IABS(KPVT(K))
                  IF (KP/=K) THEN
!
                     TEMP=B(K)
                     B(K)=B(KP)
                     B(KP)=TEMP
                  END IF
               END IF
               K=K+2
            ELSE
!
               IF (K/=1) THEN
!
                  B(K)=B(K)+SDOT(K-1,A(1,K),1,B(1),1)
                  KP=KPVT(K)
                  IF (KP/=K) THEN
!
                     TEMP=B(K)
                     B(K)=B(KP)
                     B(KP)=TEMP
                  END IF
               END IF
               K=K+1
            END IF
            GOTO 150
         END IF
      ELSE
         IF (KPVT(K)<0) THEN
!
            IF (K/=2) THEN
               KP=IABS(KPVT(K))
               IF (KP/=K-1) THEN
!
                  TEMP=B(K-1)
                  B(K-1)=B(KP)
                  B(KP)=TEMP
               END IF
!
               CALL MTRX_SLV_SAXPY(K-2,B(K),A(1,K),1,B(1),1)
               CALL MTRX_SLV_SAXPY(K-2,B(K-1),A(1,K-1),1,B(1),1)
            END IF
!
            AK=A(K,K)/A(K-1,K)
            AKM1=A(K-1,K-1)/A(K-1,K)
            BK=B(K)/A(K-1,K)
            BKM1=B(K-1)/A(K-1,K)
            DENOM=AK*AKM1-(1.,0.)
            B(K)=(AKM1*BK-BKM1)/DENOM
            B(K-1)=(AK*BKM1-BK)/DENOM
            K=K-2
         ELSE
!
            IF (K/=1) THEN
               KP=KPVT(K)
               IF (KP/=K) THEN
!
                  TEMP=B(K)
                  B(K)=B(KP)
                  B(KP)=TEMP
               END IF
!
               CALL MTRX_SLV_SAXPY(K-1,B(K),A(1,K),1,B(1),1)
            END IF
!
            B(K)=B(K)/A(K,K)
            K=K-1
         END IF
         GOTO 100
      END IF
      RETURN
      END
!
!
!    BLA LISTINGS OF LINPACK
!
!
! ISAMAX
!
      INTEGER FUNCTION ISAMAX(N,SX,INCX)
      INTEGER I,INCX,IX,N
      REAL SMAX
      COMPLEX SX(N)
!
      ISAMAX=0
      IF (N<1) THEN
         RETURN
      END IF
      ISAMAX=1
      IF (N==1) THEN
         RETURN
      END IF
      IF (INCX/=1) THEN
!
         IX=1
         SMAX=ABS(SX(1))
         IX=IX+INCX
         DO I=2,N
            IF (ABS(SX(IX))>SMAX) THEN
               ISAMAX=I
               SMAX=ABS(SX(IX))
            END IF
            IX=IX+INCX
         END DO
         RETURN
      END IF
!
      SMAX=ABS(SX(1))
      DO I=2,N
         IF (ABS(SX(I))>SMAX) THEN
            ISAMAX=I
            SMAX=ABS(SX(I))
         END IF
      END DO
      RETURN
      END
!
!
      SUBROUTINE MTRX_SLV_SAXPY(N,SA,SX,INCX,SY,INCY)
!
!**** SAXPY
!
      IMPLICIT NONE
!
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
      REAL TINY
       COMPLEX SX(N),SY(N),SA
       PARAMETER (TINY=1.E-30)
!
      IF (N<=0) THEN
         RETURN
      END IF
      IF (ABS(SA)<TINY) THEN
         RETURN
      END IF
      IF (INCX==1.AND.INCY==1) THEN
!
         M=MOD(N,4)
         IF (M/=0) THEN
            DO I=1,M
               SY(I)=SY(I)+SA*SX(I)
            END DO
            IF (N<4) THEN
               RETURN
            END IF
         END IF
      ELSE
!
         IX=1
         IY=1
         IF (INCX<0) IX=(-N+1)*INCX+1
         IF (INCY<0) IY=(-N+1)*INCY+1
         DO I=1,N
            SY(IY)=SY(IY)+SA*SX(IX)
            IX=IX+INCX
            IY=IY+INCY
         END DO
         RETURN
      END IF
      MP1=M+1
      DO I=MP1,N,4
         SY(I)=SY(I)+SA*SX(I)
         SY(I+1)=SY(I+1)+SA*SX(I+1)
         SY(I+2)=SY(I+2)+SA*SX(I+2)
         SY(I+3)=SY(I+3)+SA*SX(I+3)
      END DO
      RETURN
      END
!
! SDOT
!
      COMPLEX FUNCTION SDOT(N,SX,INCX,SY,INCY)
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
      COMPLEX SX(N),SY(N),STEMP
!
      STEMP=(0.,0.)
      SDOT=(0.,0.)
      IF (N==0) THEN
         RETURN
      END IF
      IF (INCX==1.AND.INCY==1) THEN
!
         M=MOD(N,5)
         IF (M/=0) THEN
            DO I=1,M
               STEMP=STEMP+SX(I)*SY(I)
            END DO
            IF (N<5) THEN
               GOTO 100
            END IF
         END IF
         MP1=M+1
         DO I=MP1,N,5
            STEMP=STEMP+SX(I)*SY(I)+SX(I+1)*SY(I+1)+SX(I+2)*SY(I+2)  &
                  +SX(I+3)*SY(I+3)+SX(I+4)*SY(I+4)
         END DO
      ELSE
!
         IX=1
         IY=1
         IF (INCX<0) IX=(-N+1)*INCX+1
         IF (INCY<0) IY=(-N+1)*INCY+1
         DO I=1,N
            STEMP=STEMP+SX(IX)*SY(IY)
            IX=IX+INCX
            IY=IY+INCY
         END DO
         SDOT=STEMP
         RETURN
      END IF
  100 SDOT=STEMP
      RETURN
      END
!
! MTRX_SLV_SSWAP
!
      SUBROUTINE MTRX_SLV_SSWAP(N,SX,INCX,SY,INCY)
!
      IMPLICIT NONE
!
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
      COMPLEX SX(N),SY(N),STEMP
!
      IF (N<=0) THEN
         RETURN
      END IF
      IF (INCX==1.AND.INCY==1) THEN
!
         M=MOD(N,3)
         IF (M/=0) THEN
            DO I=1,M
               STEMP=SX(I)
               SX(I)=SY(I)
               SY(I)=STEMP
            END DO
            IF (N<3) THEN
               RETURN
            END IF
         END IF
      ELSE
!
         IX=1
         IY=1
         IF (INCX<0) IX=(-N+1)*INCX+1
         IF (INCY<0) IY=(-N+1)*INCY+1
         DO I=1,N
            STEMP=SX(IX)
            SX(IX)=SY(IY)
            SY(IY)=STEMP
            IX=IX+INCX
            IY=IY+INCY
         END DO
         RETURN
      END IF
      MP1=M+1
      DO I=MP1,N,3
         STEMP=SX(I)
         SX(I)=SY(I)
         SY(I)=STEMP
         STEMP=SX(I+1)
         SX(I+1)=SY(I+1)
         SY(I+1)=STEMP
         STEMP=SX(I+2)
         SX(I+2)=SY(I+2)
         SY(I+2)=STEMP
      END DO
      RETURN
      END
!
!
      SUBROUTINE MTRX_UNITARY (PR,QR,I,PC,QC,J,U)
!
!****  Unitary matrix that block diagonalise the scattering matrix
!
!   The routine compute the values of the unitary matrix according
!   to the position of the cell PR (for rows) and PC (for columns)
!   in sequential order in the first quadrant, the quadrant numbers
!   QR (for the rows) and QC (for the columns), and the component
!   numbers I (for the rows) and J (for the columns).  It returns
!   value of the matrix element U corresponding to the matrix row
!   number IROW and matrix column number JCOL (not computed for
!   efficience.  They are determined in the calling routines)
!   The total number of the cells in a quadrant, PMAX will be needed
!   to compute IROW and JCOL.
!
      IMPLICIT NONE
!
      INTEGER PR,PC,QR,QC,I,J
      REAL U
!
!    IF ( QR.GT.4 .OR.
!   +     QC.GT.4 .OR.
!   +      I.GT.3 .OR.
!   +      J.GT.3 )    STOP 'Error: parameters out of bound!'
!
! --- IROW and JCOL are computed where needed.  Therefore, in order
!   to save computation time, they are not computed here.
!
!    IROW=3*PMAX*(QR-1)+3*(PR-1)+I
!    JCOL=12*(PC-1)+3*(QC-1)+J
!
      IF (I/=J.OR.PR/=PC) THEN
         U=0.
         RETURN
      END IF
!
! --- Form I, I1, I2, and I3 according to QC.
!   Note that I=J
!
      IF (QC==2) THEN
         IF (I==2.OR.I==3) THEN
            U=.5
         ELSE
            U=-.5
         END IF
      ELSE IF (QC==3) THEN
!
         IF (I==3) THEN
            U=.5
         ELSE
            U=-.5
         END IF
      ELSE IF (QC==4) THEN
!
         IF (I==2) THEN
            U=-.5
         ELSE
            U=.5
         END IF
      ELSE
!
         U=.5
      END IF
!
      IF (QR==2) THEN
!
         IF (QC==2) THEN
            U=-U
            RETURN
         ELSE IF (QC==3) THEN
            RETURN
         ELSE IF (QC==4) THEN
            U=-U
            RETURN
         ELSE
            RETURN
         END IF
      ELSE IF (QR==3) THEN
!
         IF (QC==2) THEN
            RETURN
         ELSE IF (QC==3) THEN
            U=-U
            RETURN
         ELSE IF (QC==4) THEN
            U=-U
            RETURN
         ELSE
            RETURN
         END IF
      ELSE IF (QR==4) THEN
!
         IF (QC==2) THEN
            U=-U
            RETURN
         ELSE IF (QC==3) THEN
            U=-U
            RETURN
         ELSE IF (QC/=4) THEN
            RETURN
         END IF
      ELSE
!
         RETURN
      END IF
      RETURN
!
      END
!
!
      SUBROUTINE EN_PRM_CS(NET,SUB_BLOCK,NSUBCM,NXMAX,NYMAX,            &
                           NZMAX,NX,NY,NZ,NCELL,XCELL,YCELL,ZCELL,      &
                           CDB,NCRD,TX_CRDX,TX_CRDY,TX_CRDZ,    &
                           EN,ECD,FRQ,MLAYER,ZBND,KKH,CDH,RMU,      &
                           ANGLES,KEYG,AJ,NZOB,ZOBG,NZSR,ZSRG,     &
                           RHOMIN,NHFILM,RRG,NRG,GRHF,GRHO0)
!
!**** Form the right hand side of the matrix equation,  or the
!   incident fields at cell centres for controlled source problems.
!
!   Note that this routine computes the incident fields in
!   one substructure only,  and thus all the parameters like
!   NX,  XCELL,  etc are defined for one substructure only.
!   (They are represented by NXI,  XCELLI etc. in the main program)
!
!   Parameters  KEYG,  AJ, NZOB, ZOBG, NZSR, ZSRG,
!   RHOMIN, NHFILM, RRG, NRG, GRHF, and GRHO0 are to be
!   passed into routine ONE_D_SOURCE.
!
!
      IMPLICIT NONE
!
      REAL AJ,FRQ,RHOMIN
      INTEGER I,IOB,J,K,KEYG,L,MLAYER,SUB_BLOCK,NEB,NET,   &
              NHFILM,NN,NN1,NOBSV,NRG,NSUBCM,NXMAX,NCRD
      INTEGER NYMAX,NZMAX,NZOB,NZSR
!
      COMPLEX EN(NET),ECD(NET),EX,EY,EZ,HX,HY,HZ,DELTCD,                 &
              CDH(0:MLAYER),KKH(0:MLAYER),CDB(NSUBCM,SUB_BLOCK)
      REAL    RMU(0:MLAYER)
      COMPLEX EHFLD(6)
      INTEGER NX(SUB_BLOCK),NY(SUB_BLOCK),NZ(SUB_BLOCK),NCELL(SUB_BLOCK)
      REAL  XCELL(NXMAX,SUB_BLOCK),YCELL(NYMAX,SUB_BLOCK),               &
            ZCELL(NZMAX,SUB_BLOCK),ZBND(0:MLAYER),                       &
            TX_CRDX(NCRD),TX_CRDY(NCRD),TX_CRDZ(NCRD),RECVR(3)
      REAL  ZOBG(NZOB),ZSRG(2,NZSR),RRG(NRG)
      REAL :: ANGLES(2)
      COMPLEX GRHF(11,NHFILM,NZSR,NZOB),GRHO0(4,NZSR,NZOB)
      EQUIVALENCE (EHFLD(1),EX),(EHFLD(2),EY),(EHFLD(3),EZ),   &
                  (EHFLD(4),HX),(EHFLD(5),HY),(EHFLD(6),HZ)
!
      NEB=0
      DO L=1,SUB_BLOCK
         IF (L>1) NEB=NEB+3*NCELL(L-1)
         DO K=1,NZ(L)
            DO IOB=MLAYER-1,0,-1
               IF (ZCELL(K,L)>=ZBND(IOB)) THEN
                  NOBSV=IOB+1
                  GOTO 20
               END IF
            END DO
            NOBSV=0
   20       DO I=1,NX(L)
               DO J=1,NY(L)
                  NN=(I-1)*NY(L)*NZ(L)+(J-1)*NZ(L)+K
                  NN1=(NN-1)*3+1+NEB
!
                  RECVR(1)=XCELL(I,L)
                  RECVR(2)=YCELL(J,L)
                  RECVR(3)=ZCELL(K,L)
!
                  CALL ONE_D_SOURCE(ANGLES,RECVR,NCRD,TX_CRDX,    &
                                    TX_CRDY,TX_CRDZ,KEYG,FRQ,    &
                                    MLAYER,ZBND,AJ,KKH,CDH,RMU,NZOB,ZOBG, &
                                    NZSR,ZSRG,RHOMIN,NHFILM,RRG,NRG,GRHF, &
                                    GRHO0,EHFLD)
!
                  EN(NN1)=EX
                  EN(NN1+1)=EY
                  EN(NN1+2)=EZ
                  DELTCD=CDB(NN,L)-CDH(NOBSV)
                  ECD(NN1)=EX*DELTCD
                  ECD(NN1+1)=EY*DELTCD
                  ECD(NN1+2)=EZ*DELTCD
!
               END DO
            END DO
         END DO
      END DO
      RETURN
      END
!
!**** End of EN_PRM_CS
!
!
      SUBROUTINE EN_PRM_GS(NEQ,NBODY,SUB_BLOCK,NBMAX,NXMAX,NYMAX,NZMAX,   &
                           NET,NX,NY,NZ,NCELL,XCELL,YCELL,ZCELL,          &
                           NCRD,TX_CRDX,TX_CRDY,TX_CRDZ,EN,   &
                           FRQ,MLAYER,ZBND,KKH,CDH,RMU,ANGLES,KEYG,       &
                           AJ,NZOB,ZOBG,NZSR,ZSRG,                   &
                           RHOMIN,NHFILM,RRG,NRG,GRHF,GRHO0)
!
!**** Form the right hand side of the matrix equation,  or the
!   incident fields at cell centres for controlled source problems.
!
!   Note that this routine computes the incident fields in
!   the whole structure (unlike routine EN_PRM_CS!),  and thus all the
!   parameters like NX,  XCELL,  etc are defined for the whole
!   structure as in the main program.
!
!   Note that the array EN is arranged in an order as described
!   by Tripp and Tripp & Hohmann, i.e., the elements go first
!   by quarters
!
!   Parameters  KEYG, AJ, NZOB, ZOBG, NZSR, ZSRG,
!   RHOMIN, NHFILM, RRG, NRG, GRHF, and GRHO0 are to be
!   passed into routine ONE_D_SOURCE.
!
!
      IMPLICIT NONE
!
      REAL AJ,BX,BY,FRQ,RHOMIN
      INTEGER I,IB,IQ,J,K,KEYG,L,MLAYER,NBMAX,NBODY,NCB,NEQ,  &
              NHFILM,NN,NN1,NP,NRG,NCRD,NSUB,NXMAX,NYMAX,NZMAX,NZOB,NZSR
!
      INTEGER NX(NBMAX,NBODY),NY(NBMAX,NBODY),NZ(NBMAX,NBODY),             &
              NCELL(NBMAX,NBODY),NET(NBODY),SUB_BLOCK(NBODY)
      REAL  XCELL(NXMAX,NBMAX,NBODY),YCELL(NYMAX,NBMAX,NBODY),             &
            ZCELL(NZMAX,NBMAX,NBODY),ZBND(0:MLAYER),                       &
            TX_CRDX(NCRD),TX_CRDY(NCRD),TX_CRDZ(NCRD),RECVR(3)
      REAL :: ANGLES(2),ZOBG(NZOB),ZSRG(2,NZSR),RRG(NRG),RMU(0:MLAYER)
      COMPLEX EN(NEQ*4),EX,EY,EZ,HX,HY,HZ,CDH(0:MLAYER),KKH(0:MLAYER)
      COMPLEX EHFLD(6),GRHF(11,NHFILM,NZSR,NZOB),GRHO0(4,NZSR,NZOB)
      EQUIVALENCE (EHFLD(1),EX),(EHFLD(2),EY),(EHFLD(3),EZ),               &
                  (EHFLD(4),HX),(EHFLD(5),HY),(EHFLD(6),HZ)
!
      NSUB=0
      DO IB=1,NBODY
         IF (IB>1) NSUB=NSUB+NET(IB-1)/3
!
         NCB=0
         DO L=1,SUB_BLOCK(IB)
            IF (L>1) NCB=NCB+NCELL(L-1,IB)
            DO K=1,NZ(L,IB)
               DO I=1,NX(L,IB)
                  DO J=1,NY(L,IB)
                     NN=(I-1)*NY(L,IB)*NZ(L,IB)+(J-1)*NZ(L,IB)+K
!
                     NP=NSUB+NCB+NN
!
! --- Loop for the four quarters
                     DO IQ=1,4
!
! --- See the formula for jcol in routine unitary
                        NN1=12*(NP-1)+3*(IQ-1)+1
!
                        IF (IQ==1) THEN
                           BX=-1.
                           BY=-1.
                        END IF
                        IF (IQ==2) THEN
                           BX=1.
                           BY=-1.
                        END IF
                        IF (IQ==3) THEN
                           BX=1.
                           BY=1.
                        END IF
                        IF (IQ==4) THEN
                           BX=-1.
                           BY=1.
                        END IF
!
                        RECVR(1)=BX*XCELL(I,L,IB)
                        RECVR(2)=BY*YCELL(J,L,IB)
                        RECVR(3)=ZCELL(K,L,IB)
!
                        CALL ONE_D_SOURCE(ANGLES,RECVR,NCRD,TX_CRDX,  &
                                          TX_CRDY, TX_CRDZ,KEYG,FRQ, &
                                          MLAYER,ZBND,AJ,KKH,CDH,RMU,NZOB,    &
                                          ZOBG,NZSR,ZSRG,RHOMIN,NHFILM,RRG,   &
                                          NRG,GRHF,GRHO0,EHFLD)
!
                        EN(NN1)=EX
                        EN(NN1+1)=EY
                        EN(NN1+2)=EZ
!
                     END DO
                  END DO
               END DO
            END DO
         END DO
      END DO
!
      RETURN
      END
!
!**** End of EN_PRM_GS
!
!
   SUBROUTINE ONE_D_KERNEL(KEYG,KEMD,MLAYER,ZBND,LRYTH,KKH,RMU,     &
                           KPRM,KRHO,KITG,KCHRG,NOB,NSR,                &
                           ZOB,ZSR,ZSRH,ZSRL,LUMBDA,FKN)
!
!****  KERNELS  OF  THE  HANKEL  TRANSFORMS  IN  THE  GREEN'S  FUNCTIONS
!      FOR ELECTROMAGNETIC FIELDS IN A STRATIFIED ANISOTROPIC EARTH
!      (INTEGRATED ANALYTICALLY IN THE VERTICAL DIRECTION IF KITG=1)
!
!    NOTE THAT THE PRIMARY CONTRIBUTION IS INCLUDED IF KPRM=1 AND
!    IS EXCLUDED IF KPRM=0. THE FIGURE AND EQUATION NUMBERS REFERED
!    HERE ARE THOSE IN XIONG, 1989, GEOPHYSICS, 1643-1646
!
!
! --- The input parameters:
!
!      KEYG:    Interger(*4), parameter controlling the output kernels
!               for electric dipoles.
!               KEYG=1:  compute the kernels for electric Green's
!                        tensors as required by the scattering matrix.
!                        Only the first six elements in the output array
!                        FKNS are used which are for the general electric
!                        Green's tensor when the current and charge
!                        termss are computed together as required by old
!                        versions of the 3D code SYSEM et al.
!                    =2: kernels for electric and magnetic field without
!                        separating the current and charge terms;
!                    =3: compute the kernels for current and charge terms
!                        of electric Green's tensors as required by the
!                        scattering matrix; and
!                    =4: compute the scattered fields due to cells with
!                        the current and the charge terms separated for the
!                        electric field.
!      KEMD:    Interger, controls if electric or magnetic
!               dipoles are to be computed.
!               KEMD=1:  electric dipoles;  and
!                   =2:  magnetic dipoles.
!      MLAYER:  Integer(*4), the number of layers including the air.
!      ZBND:    Real ZBND(0:MLAYER), the z-coordinates of the layer
!               boundaries with the air-earth interface being always
!               0 (zbnd(0)=0).
!      LRYTH:   Real LRYTH(MLAYER), the thickness of the layers.
!      KKH:     Complex KKH(0:MLAYER), the wave numbers of the layers
!               (the air as layer 0) defined as
!                 i*w*mu*(horizontal complex conductivity)
!               where the horizontal complex conductivity includes
!               i*epsilon*w in its imaginary part.
!      RMU:     relateive permeabilities of the layers.
!               Note that the current version of this kernel routine does not
!               allow sources in layers with mu/=1.  Only one recurrence
!               relation is modified to take into account mu/=1.  Further
!               verification may be necessary.
!      KPRM:    Integer, controls whether the primary parts due to
!               the whole space should be included or excluded.
!               KPRM=1:  include the primary terms so the kernels
!                        are complete for all contributions; and
!               KPRM=0:  exclude the primary terms so the kernels
!                        are secondary that are due to the layers.
!      KRHO:    Integer, flag for the value of rho
!               (=sqrt(x-x')**2+(y-y')**2).
!               KRHO=0:  rho=0;  and
!               KRHO=1:  rho>0
!               The kernels are different for rho=0 where the Hankel
!               transforms reduce to plaine infinite integrals.
!      KITG:    Integer, controls whether the kernels should be
!               integrated analytically in the z'-direction.
!               KITG=1:  integrate the kernels from ZSRL to ZSRH; and
!               KITG=0:  ordinary kernels (EM fields from sources to
!               receivers due to dipoles).
!      KCHRG:   Integer,  controls whether the charge terms need be
!               removed from the kernels.  For loop sources, the charge
!               terms in the electric fields do not exist but cannot be
!               canceled exactly in the numerical procedures.  Thus
!               they need to removed directly from the kernels.
!               If KCHRG=1,  the charge terms in the electric field
!               will be removed from the kernels.  The parameter
!               KCHRG is controlled in the routine ONE_D_HF_TABLE and THR_D_HF_TABLE.
!      NOB:     Integer,  the layer number for ZOB.
!      NSR:     Integer,  the layer number of ZSR, ZSRH and ZSRL (they
!               must be in the same layer).
!      ZOB:     Real(*4), the receiver z-coordinate.
!      ZSR:     Real, the source z-coordinate.  Also the mid-point
!               of ZSRH and ZSRL.
!      ZSRL,ZSRH: Real,  the lower the upper limits of the integration.
!               Note that ZSRL and ZSRH both should be either less than
!               or equal to, or greater than or equal to ZOB.
!               ZSR, ZSRH and ZSR must be in the same layer.
!      LUMBDA:   Real, the integration variable, lumbda,  in the Hankel
!               transforms.  The A in LUMBDA is used to make the
!               parameter an implicit real one.
!
! --- The output is the array FKNS
!
!      FKN:    Defined as complex FKN(11).  The contents of the
!              11 values are the kernels of the Hankel transforms
!              for the Green's tensors which are strictly related
!              to those in the routine ONE_D_GREEN and THR_D_GREEN.  The actual
!              kernels are determined by the input parameter KEYG.
!              Check comments on KEYG for more details.
!
! --- Further remarks:
!
!    The parameter EMAX represents the maximal argument of the exponential
!    functions. The exponential functions in some cases tend to infinity as
!    z and z' goes far away from each other, but their contributions to the
!    field values are less and less.   Therefore, for the sake of numerical
!    accuracy these exponential functions are set to zero if their arguments
!    exceed EMAX.  The value of EMAX depends on the accuracy of the numerical
!    Hankel tranforms as well as on the computer system used.  EMAX can be
!    chosen using reciprocity tests.
!
!    The numerical Hankel transforms used in this program yield an ac-
!    curacy of about 10**(-7).   With an EMAX ranging from 5 to 25
!    identical results (up to at least 5 figures) were observed for some
!    tests on an IBM-3090. However, the numerical integration for rho=0
!    (routine simpinf and simpsn) sometimes would not converge for
!    EMAX=20 or so.  Further tests show that EMAX must be at least greater
!    than 5 in order to reach a reasonable accuracy.  Discrepancies may still
!    be visible for EMAX < 20.
!
!    While the use of the control parameter EMAX is adequate for double
!    precision computations,  it fails to yield reasonable accuracy for
!    single precision operations.  This turns out to be caused be the
!    wave penetration factors,  which are computed as FCTA, FCTB, etc.
!    Thus they are set to be zero if penetration is weak.   Parameter
!    PENETRATION controls this threshold.
!
!    There are two recurrence formula for the computation of EM fields
!    in layered earths.  One is the Wait recursion equation and the
!    other is the Knight-Raiche recursion equation.  They both are
!    built in this routine.  Choose proper values for the parameter
!    KWAIT in the parameter list to switch between them.  The Knight-Raiche
!    recursion can be found in Geophysics, Vol. 47,  p 47-50,  1982.
!    See also Schmucker and Weidelt,  Lecture notes on electromagnetic
!    induction in the earth,  Aarhus,  1975.
!
!****    CALLED by:  hfill, hfilh, simpsn
!
!****    CALLS    :  none
!
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) ::  KCHRG,KEMD,KITG,KPRM,KRHO,MLAYER,NOB,NSR,KEYG
      REAL, INTENT(IN) :: LUMBDA,ZOB,ZSR,ZSRH,ZSRL,LRYTH(MLAYER),  &
                          RMU(0:MLAYER),ZBND(0:MLAYER)
      COMPLEX, INTENT(IN) :: KKH(0:MLAYER)
!  INTENT seem to cause problems on some compilers      COMPLEX, INTENT(OUT) :: FKN(11)
      COMPLEX :: FKN(11)
!
! --- Note the parameter LAYERM must have a value greater than or
!     equal to that in the main program
!
      INTEGER, PARAMETER :: LAYERM=200
!
      REAL    :: RMUMN(0:LAYERM),RMUPL(0:LAYERM)
      COMPLEX :: ZMN(LAYERM),ZPL(LAYERM),YMN(LAYERM),                &
                 YPL(LAYERM),NUMN(0:LAYERM),NUPL(0:LAYERM),          &
                 GMMN(0:LAYERM),GMPL(0:LAYERM),UMN(0:LAYERM),        &
                 UPL(0:LAYERM),VMN(0:LAYERM),VPL(0:LAYERM),          &
                 TANH1,RMN,RPL,XMN,XPL,U0,V0,NU0,GM0,UI,VI,    &
                 A1,B1,C1,D1,P1,Q1,A2,B2,C2,D2,P2,Q2,                &
                 AE1,BE1,CE1,DE1,PE1,QE1,                            &
                 AE2,BE2,CE2,DE2,PE2,QE2,                            &
                 FCTA,FCTB,FCTC,FCTD,FCTP,FCTQ,                      &
                 EXPA,EXPB,EXPC,EXPD,EXPP,EXPQ,                      &
                 CAB,CCD,CPQ,CAB1,CCD1,CPQ1,DG,                      &
                 PAB,PCD,PPQ,PAB1,PCD1,PPQ1,DCAB,DCAB1,DCCD,DCCD1,   &
                 EA1,EA2,EB1,EB2,EC1,EC2,ED1,ED2,EP1,EP2,EQ1,EQ2,    &
!
!  --- The following three lines are for the computation of the
!      fields in the source layer using Knight and Raiche's recurrrence
!      formula.
!
                 KMN(0:LAYERM),KPL(0:LAYERM),RRU,RRUPL0,RRUMN0,      &
                 SMN0,SPL0,FMN(0:LAYERM),FPL(0:LAYERM)
!
! --- Parameter KWAIT determines whether Wait's recurssion or
!     Knight and Raiche's recursion formula are to be used to compute the
!     kernel coefficients in the source layer.
!       KWAIT=1:  Wait recursion; =0:  Knight-Raiche recursion.
!
      INTEGER, PARAMETER :: KWAIT=0
!
      INTEGER ::  L,LMN,LPL,MPLUS,NMINUS,NOB1
!
! --- For double precision computations PENETRATION should be at least 1.E-12
!
      REAL, PARAMETER :: EMAX=10.,PENETRATION=1.E-6
!
! --- PSQRT is the parameter controlling the accuracy of the binomial
!     expansion for the computation of SQRT.  This depends on the
!     precision of computation.
!
!     For double precision computation use
!
!      REAL, PARAMETER :: PSQRT=100.
!
!     For single precision computation use
!
      REAL, PARAMETER :: PSQRT=10.
!
! --- No analytical integration of the kernels if the primary
!     contributions need to be abstracted with source and receiver
!     being in different layers.
!
      IF (KITG==1.AND.KPRM==0.AND.NSR/=NOB) THEN
         WRITE (*,*) 'Routine kernel not ready for kitg=1 and ',   &
                   'kprm=0 and nsr.ne.nob!'
         STOP 'Program aborted.'
      END IF
!
! --- Transform the actual layer sequence into the sequence numbered
!     downwards and upperwards with source in layer number 0 as shown
!     in Fig. 1 (of Xiong, GEOPHYSICS); and calculate the impedances
!     ZMN and ZPL.
!
!     It is assumed that there is at least one layer boundary in the
!     space.
!
!  -- MMINUS:  number of the -layers
!     MPLUS:   number of the +layers
!
      NMINUS=NSR
      MPLUS=MLAYER-NSR
!
!  -- Layer parameters
!
      DO L=0,NMINUS
         LMN=NSR-L
         KMN(L)=CSQRT(KKH(LMN))
         RMUMN(L)=RMU(LMN)
         IF (LUMBDA>PSQRT*ABS(KMN(L))) THEN
            CAB=(KMN(L)/LUMBDA)**2
            UMN(L)=LUMBDA*((1.,0.)+CAB*((.5,0.)+CAB*(-(.125,0.)+        &
                           CAB*((.0625,0.)+CAB*(-(.0390625,0.)+         &
                           CAB*((.02734375,0.)+CAB*(-(.0205078125,0.)+  &
                           CAB*(.01611328,0.))))))))
         ELSE IF (PSQRT*LUMBDA<ABS(KMN(L))) THEN
            CAB=(LUMBDA/KMN(L))**2
            UMN(L)=KMN(L)*((1.,0.)+CAB*((.5,0.)+CAB*(-(.125,0.)+        &
                           CAB*((.0625,0.)+CAB*(-(.0390625,0.)+         &
                           CAB*((.02734375,0.)+CAB*(-(.0205078125,0.)+  &
                           CAB*(.01611328,0.))))))))
         ELSE
            UMN(L)=CSQRT(CMPLX(LUMBDA**2,0.)+KKH(LMN))
         END IF
         VMN(L)=UMN(L)
         NUMN(L)=1./UMN(L)
         GMMN(L)=KKH(LMN)/VMN(L)
      END DO
!
      IF (MPLUS>0) THEN
         KPL(0)=KMN(0)
         UPL(0)=UMN(0)
         VPL(0)=VMN(0)
         NUPL(0)=NUMN(0)
         GMPL(0)=GMMN(0)
      END IF
!
      DO L=1,MPLUS
         LPL=NSR+L
         KPL(L)=CSQRT(KKH(LPL))
         RMUPL(L)=RMU(LPL)
         IF (LUMBDA>PSQRT*ABS(KPL(L))) THEN
            CAB=(KPL(L)/LUMBDA)**2
            UPL(L)=LUMBDA*((1.,0.)+CAB*((.5,0.)+CAB*(-(.125,0.)+        &
                           CAB*((.0625,0.)+CAB*(-(.0390625,0.)+         &
                           CAB*((.02734375,0.)+CAB*(-(.0205078125,0.)+  &
                           CAB*(.01611328,0.))))))))
         ELSE IF (PSQRT*LUMBDA<ABS(KPL(L))) THEN
            CAB=(LUMBDA/KPL(L))**2
            UPL(L)=KPL(L)*((1.,0.)+CAB*((.5,0.)+CAB*(-(.125,0.)+        &
                           CAB*((.0625,0.)+CAB*(-(.0390625,0.)+         &
                           CAB*((.02734375,0.)+CAB*(-(.0205078125,0.)+  &
                           CAB*(.01611328,0.))))))))
         ELSE
            UPL(L)=CSQRT(CMPLX(LUMBDA**2,0.)+KKH(LPL))
         END IF
         VPL(L)=UPL(L)
         NUPL(L)=1./UPL(L)
         GMPL(L)=KKH(LPL)/VPL(L)
      END DO
!
!  -- Recursion
!
      IF (NMINUS/=0) THEN
!
         ZMN(NMINUS)=-NUMN(NMINUS)
         YMN(NMINUS)=-GMMN(NMINUS)
         IF (KWAIT==0) THEN
            FMN(NMINUS)=(0.,0.)
         END IF
!
         DO L=NMINUS-1,1,-1
            LMN=NSR-L
            CAB=-2.*UMN(L)*LRYTH(LMN)
            IF (REAL(CAB)>-80.) THEN
               EA1=EXP(CAB)
            ELSE
               EA1=(0.,0.)
            END IF
            TANH1=((1.,0.)-EA1)/((1.,0.)+EA1)
!
!  --- Note that the impedances Z and Y must be computed for
!      propagation of fields in other layers even if KWAIT=0.
!
            ZMN(L)=NUMN(L)*(ZMN(L+1)-NUMN(L)*TANH1)   &
                 /(NUMN(L)-ZMN(L+1)*TANH1)
            YMN(L)=GMMN(L)*(YMN(L+1)-GMMN(L)*TANH1)   &
                 /(GMMN(L)-YMN(L+1)*TANH1)
!
            IF (KWAIT==0) THEN
!   --- RRU is the Knight-Raiche R with both the numerator and the
!       denominator being multiplied by (u_i+u_{i+1)) for better accuracy
!               RRU=(KMN(L)**2-KMN(L+1)**2)/(UMN(L)+UMN(L+1))**2
!               RRU=(KMN(L)-KMN(L+1))*(KMN(L)+KMN(L+1))/(UMN(L)+UMN(L+1))**2
!    ** Relative mu is added to the recurrence relation (computing RRU is the ONLY parameter
!       mu is taken into account in this version)
               RRU=(RMUMN(L+1)*UMN(L)-RMUMN(L)*UMN(L+1))/(RMUMN(L+1)*UMN(L)+RMUMN(L)*UMN(L+1))
               FMN(L)=EA1*(RRU+FMN(L+1))/((1.,0.)+RRU*FMN(L+1))
            END IF
!
         END DO
!
      END IF
!
      IF (MPLUS/=0) THEN
!
         ZPL(MPLUS)=NUPL(MPLUS)
         YPL(MPLUS)=GMPL(MPLUS)
         IF (KWAIT==0) THEN
            FPL(MPLUS)=(0.,0.)
         END IF
!
         DO L=MPLUS-1,1,-1
            LPL=NSR+L
            CAB=-2.*UPL(L)*LRYTH(LPL)
            IF(REAL(CAB)>-80.) THEN
               EA1=EXP(CAB)
            ELSE
               EA1=(0.,0.)
            END IF
            TANH1=((1.,0.)-EA1)/((1.,0.)+EA1)
            ZPL(L)=NUPL(L)*(ZPL(L+1)+NUPL(L)*TANH1)   &
                 /(NUPL(L)+ZPL(L+1)*TANH1)
            YPL(L)=GMPL(L)*(YPL(L+1)+GMPL(L)*TANH1)   &
                 /(GMPL(L)+YPL(L+1)*TANH1)
!
            IF (KWAIT==0) THEN
!   --- RRU is the Knight-Raiche R with both the numerator and the
!       denominator being multiplied by (u_i+u_{i+1)) for better accuracy
!               RRU=(KPL(L)**2-KPL(L+1)**2)/(UPL(L)+UPL(L+1))**2
!               RRU=(KPL(L)-KPL(L+1))*(KPL(L)+KPL(L+1))/(UPL(L)+UPL(L+1))**2
!    ** Relative mu is added to the recurrence relation (computing RRU is the ONLY parameter
!       mu is taken into account in this version)
               RRU=(RMUPL(L+1)*UPL(L)-RMUPL(L)*UPL(L+1))/(RMUPL(L+1)*UPL(L)+RMUPL(L)*UPL(L+1))
               FPL(L)=EA1*(RRU+FPL(L+1))/((1.,0.)+RRU*FPL(L+1))
            END IF
!
         END DO
!
      END IF
!
! --- The coefficients A0,B0,C0.,P0, and Q0 in the source layer
!     are separated into A1*EXP(AE1)+A2*EXP(AE2),... +Q2*EXP(QE2),
!     respectively, in which "1" represents the terms with EXP(+U0 Z')
!     and "2" represents those with EXP(-U0 Z). But the two exponetial
!     functions (EXP(+U0 Z') etc.) are not included here.
!
!     Note: the radiation conditions are automatically considered here
!           and the calculation is kept to the minimum for every special
!           case possible.
!
      U0=UMN(0)
      V0=VMN(0)
      NU0=NUMN(0)
      GM0=GMMN(0)
!
!  -- If the source is located neither in the upper-most nor in the
!     lower-most layer
!
      IF (NMINUS/=0.AND.MPLUS/=0) THEN
!
         IF (KWAIT==1) THEN
            RMN=(ZMN(1)+NU0)/(ZMN(1)-NU0)
            RPL=(ZPL(1)+NU0)/(ZPL(1)-NU0)
         ELSE
!            RRUMN0=(KMN(0)**2-KMN(1)**2)/(UMN(0)+UMN(1))**2
            RRUMN0=(KMN(0)-KMN(1))*(KMN(0)+KMN(1))/(UMN(0)+UMN(1))**2
            SMN0=(RRUMN0+FMN(1))/((1.,0.)+RRUMN0*FMN(1))
            RMN=SMN0
!            RRUPL0=(KPL(0)**2-KPL(1)**2)/(UPL(0)+UPL(1))**2
            RRUPL0=(KPL(0)-KPL(1))*(KPL(0)+KPL(1))/(UPL(0)+UPL(1))**2
            SPL0=(RRUPL0+FPL(1))/((1.,0.)+RRUPL0*FPL(1))
            RPL=(1.,0.)/SPL0
         END IF
         XMN=(YMN(1)+GM0)/(YMN(1)-GM0)
         XPL=(YPL(1)+GM0)/(YPL(1)-GM0)
         CAB=-2.*U0*LRYTH(NSR)
         IF (REAL(CAB)>-80.) THEN
            EA1=EXP(CAB)
         ELSE
            EA1=(0.,0.)
         END IF
         EA2=RPL-RMN*EA1
         A1=LUMBDA/U0/EA2
         A2=A1*RMN
         B1=A2
         B2=B1*RPL
         EA2=XPL-XMN*EA1
         C1=-1./LUMBDA/EA2
         C2=-C1*XMN
         D1=-C2
         D2=-D1*XPL
         P1=LUMBDA/V0/EA2
         P2=P1*XMN
         Q1=P2
         Q2=Q1*XPL
         AE1=-2.*U0*ZBND(NSR)
         AE2=-2.*U0*LRYTH(NSR)
         BE1=AE2
         BE2=2.*U0*ZBND(NSR-1)
         CE1=AE1
         CE2=AE2
         DE2=BE2
         DE1=CE2
         PE1=CE1
         PE2=CE2
         QE1=PE2
         QE2=DE2
!
      END IF
!
!  -- If the source sits in the upper-most layer and the receiver
!     everywhere but the lower-most layer
!
!     Note that the other components that are not calculated here
!     are zero and are ignored accordingly in the other parts of
!     this routine.
!
      IF (NMINUS==0.AND.NOB/=MLAYER) THEN
         IF (KWAIT==1) THEN
            RPL=(ZPL(1)+NU0)/(ZPL(1)-NU0)
         ELSE
!            RRUPL0=(KPL(0)**2-KPL(1)**2)/(UPL(0)+UPL(1))**2
            RRUPL0=(KPL(0)-KPL(1))*(KPL(0)+KPL(1))/(UPL(0)+UPL(1))**2
            SPL0=(RRUPL0+FPL(1))/((1.,0.)+RRUPL0*FPL(1))
            RPL=(1.,0.)/SPL0
         END IF
         XPL=(YPL(1)+GM0)/(YPL(1)-GM0)
         A1=LUMBDA/U0/RPL
         C1=-1./LUMBDA/XPL
         P1=LUMBDA/V0/XPL
         AE1=-2.*U0*ZBND(NSR)
         CE1=-2.*V0*ZBND(NSR)
         PE1=CE1
      END IF
!
!  -- If the source sits in the lower-most layer and the receiver
!     everywhere but the upper-most layer
!
      IF (MPLUS==0.AND.NOB/=0) THEN
         IF (KWAIT==1) THEN
            RMN=(ZMN(1)+NU0)/(ZMN(1)-NU0)
         ELSE
!            RRUMN0=(KMN(0)**2-KMN(1)**2)/(UMN(0)+UMN(1))**2
            RRUMN0=(KMN(0)-KMN(1))*(KMN(0)+KMN(1))/(UMN(0)+UMN(1))**2
            SMN0=(RRUMN0+FMN(1))/((1.,0.)+RRUMN0*FMN(1))
            RMN=SMN0
         END IF
         XMN=(YMN(1)+GM0)/(YMN(1)-GM0)
         B2=LUMBDA/U0*RMN
         D2=XMN/LUMBDA
         Q2=LUMBDA/V0*XMN
         BE2=2.*U0*ZBND(NSR-1)
         DE2=2.*V0*ZBND(NSR-1)
         QE2=DE2
      END IF
!
! --- Separate the primary contributions according to the plus
!     and minus region after equations (20) to (27).
!
!  -- Here also the cases when the receiver is in the upper-most and
!     source is in the lower-most layer and vice versa are considered.
!
      IF (ZOB>=ZSR) THEN
!
         IF (KPRM==0.AND.NSR==NOB) THEN
!
!--- Note that the primary contributions can be omitted only if ZOB
!    and ZSR are in the same layer. Otherwise, they must be subs-
!    tracted from the final results calculated later (eq. (34) and
!    (35) etc.).
!
            B1=(0.,0.)
            D1=(0.,0.)
            Q1=(0.,0.)
         ELSE IF (NMINUS==0.OR.MPLUS==0) THEN
            B1=LUMBDA/U0
            D1=-(1.,0.)/LUMBDA
            Q1=LUMBDA/V0
         ELSE
            IF (REAL(BE1).GT.-80.) THEN
               EA1=EXP(BE1)
            ELSE
               EA1=(0.,0.)
            END IF
            B1=B1*EA1+LUMBDA/U0
            D1=D1*EA1-(1.,0.)/LUMBDA
            Q1=Q1*EA1+LUMBDA/V0
         END IF
         BE1=(0.,0.)
         DE1=(0.,0.)
         QE1=(0.,0.)
!
      END IF
!
      IF (ZOB<ZSR) THEN
!
         IF (KPRM==0.AND.NSR==NOB) THEN
            A2=(0.,0.)
            C2=(0.,0.)
            P2=(0.,0.)
         ELSE IF (NMINUS==0.OR.MPLUS==0) THEN
            A2=LUMBDA/U0
            C2=(1.,0.)/LUMBDA
            P2=LUMBDA/V0
         ELSE
            IF (REAL(AE2).GT.-80.) THEN
               EA1=EXP(AE2)
            ELSE
               EA1=(0.,0.)
            END IF
            A2=A2*EA1+LUMBDA/U0
            C2=C2*EA1+(1.,0.)/LUMBDA
            P2=P2*EA1+LUMBDA/V0
         END IF
         AE2=(0.,0.)
         CE2=(0.,0.)
         PE2=(0.,0.)
!
      END IF
!
! --- Calculate the coefficients in other layers
!
!     Propagations of the coefficients a, b according to
!     equations (34) and (35).
!
      NOB1=NOB-NSR
!
      IF (NOB/=NSR) THEN
!
         FCTA=(1.,0.)
         FCTB=(1.,0.)
         FCTC=(1.,0.)
         FCTD=(1.,0.)
         EXPA=(0.,0.)
         EXPC=(0.,0.)
!
         IF (NOB1>=0) THEN
!
            DO L=1,NOB1
               IF (ABS(ZPL(L)-NUPL(L-1))>1.E-35) THEN
                  IF (ABS(ZPL(L)-NUPL(L))<PENETRATION*ABS(NUPL(L))) THEN
                     FCTA=(0.,0.)
                  ELSE
                     FCTA=FCTA*(ZPL(L)-NUPL(L))/(ZPL(L)-NUPL(L-1))
                  END IF
               END IF
               FCTB=FCTB*(ZPL(L)+NUPL(L))/(ZPL(L)+NUPL(L-1))
               IF (ABS(YPL(L)-GMPL(L-1))>1.E-35) THEN
                  IF (ABS(YPL(L)-GMPL(L))<PENETRATION*ABS(GMPL(L))) THEN
                     FCTC=(0.,0.)
                  ELSE
                     FCTC=FCTC*(YPL(L)-GMPL(L))/(YPL(L)-GMPL(L-1))
                  END IF
               END IF
               FCTD=FCTD*(YPL(L)+GMPL(L))/(YPL(L)+GMPL(L-1))
               EXPA=EXPA+(UPL(L-1)-UPL(L))*ZBND(NSR+L-1)
            END DO
!
         END IF
!
         IF (NOB1<0) THEN
!
            DO L=1,-NOB1
               FCTA=FCTA*(ZMN(L)-NUMN(L))/(ZMN(L)-NUMN(L-1))
               IF (ABS(ZMN(L)+NUMN(L-1))>1.E-35) THEN
                  IF (ABS(ZMN(L)+NUMN(L))<PENETRATION*ABS(NUMN(L))) THEN
                     FCTB=(0.,0.)
                  ELSE
                     FCTB=FCTB*(ZMN(L)+NUMN(L))/(ZMN(L)+NUMN(L-1))
                  END IF
               END IF
               FCTC=FCTC*(YMN(L)-GMMN(L))/(YMN(L)-GMMN(L-1))
               IF (ABS(YMN(L)+GMMN(L-1))>1.E-35) THEN
                  IF (ABS(YMN(L)+GMMN(L))<PENETRATION*ABS(GMMN(L))) THEN
                     FCTD=(0.,0.)
                  ELSE
                    FCTD=FCTD*(YMN(L)+GMMN(L))/(YMN(L)+GMMN(L-1))
                  END IF
               END IF
               EXPA=EXPA+(UMN(L-1)-UMN(L))*ZBND(NSR-L)
            END DO
!
         END IF
!
         EXPC=EXPA
         FCTP=FCTC
         FCTQ=FCTD
         EXPB=-EXPA
         EXPD=-EXPC
         EXPP=EXPC
         EXPQ=EXPD
!
         IF (.NOT.(NSR==0.AND.NOB==MLAYER.OR.NSR==MLAYER.AND.NOB==0))   &
           THEN
!
            IF (NMINUS==0) THEN
               A1=A1*FCTA
               C1=C1*FCTC
               P1=P1*FCTP
               AE1=AE1+EXPA
               CE1=CE1+EXPC
               PE1=PE1+EXPP
            ELSE IF (MPLUS==0) THEN
               B2=B2*FCTB
               D2=D2*FCTD
               Q2=Q2*FCTQ
               BE2=BE2+EXPB
               DE2=DE2+EXPD
               QE2=QE2+EXPQ
            ELSE
               A1=A1*FCTA
               A2=A2*FCTA
               C1=C1*FCTC
               C2=C2*FCTC
               P1=P1*FCTP
               P2=P2*FCTP
               B1=B1*FCTB
               B2=B2*FCTB
               D1=D1*FCTD
               D2=D2*FCTD
               Q1=Q1*FCTQ
               Q2=Q2*FCTQ
               AE1=AE1+EXPA
               AE2=AE2+EXPA
               CE1=CE1+EXPC
               CE2=CE2+EXPC
               PE1=PE1+EXPP
               PE2=PE2+EXPP
               BE1=BE1+EXPB
               BE2=BE2+EXPB
               DE1=DE1+EXPD
               DE2=DE2+EXPD
               QE1=QE1+EXPQ
               QE2=QE2+EXPQ
!
            END IF
!
         END IF
!
         IF (NMINUS==0.OR.MPLUS==0) THEN
!
            IF (ZOB>=ZSR) THEN
               B1=B1*FCTB
               D1=D1*FCTD
               Q1=Q1*FCTQ
               BE1=BE1+EXPB
               DE1=DE1+EXPD
               QE1=QE1+EXPQ
            ELSE
               A2=A2*FCTA
               C2=C2*FCTC
               P2=P2*FCTP
               AE2=AE2+EXPA
               CE2=CE2+EXPC
               PE2=PE2+EXPP
            END IF
!
         END IF
!
      END IF
!
! --- Calculate the kernels of the Hankel transforms: the coefficients
!     with the expoentials of z and z'
!
      IF (NOB1>0) THEN
         UI=UPL(NOB1)
         VI=VPL(NOB1)
      ELSE
         UI=UMN(-NOB1)
         VI=VMN(-NOB1)
      END IF
!
!  -- If the source is in the upper-most layer
!
!     Due to the complexity of the if blocks,  each block level is
!     marked by *****, _____, -----, or .....
!
      IF (NMINUS==0) THEN
!   **********************************
!   *                                *
!    -- If the receiver is not in the lower-most layer
!
         IF (NOB/=MLAYER) THEN
!   ______________________________
!   !                            !
            IF (KITG==0) THEN
!    ...........................
!    .                         .
               EXPA=AE1+U0*ZSR+UI*ZOB
               IF (REAL(EXPA)>-80..AND.REAL(EXPA)<=EMAX) THEN
                  CAB=EXP(EXPA)
                  EA1=A1*CAB
                  EC1=C1*CAB
                  EP1=P1*CAB
               ELSE
                  EA1=(0.,0.)
                  EC1=(0.,0.)
                  EP1=(0.,0.)
               END IF
!    .                         .
!    ...........................
            END IF
!
            IF (KITG==1) THEN
!    ...........................
!    .                         .
               EXPA=AE1+U0*ZSRH+UI*ZOB
               IF (REAL(EXPA)>-80..AND.REAL(EXPA)<=EMAX) THEN
                  CAB=EXP(EXPA)/U0
                  EA1=A1*CAB
                  EC1=C1*CAB
                  EP1=P1*CAB
               ELSE
                  EA1=(0.,0.)
                  EC1=(0.,0.)
                  EP1=(0.,0.)
               END IF
!
               EXPA=AE1+U0*ZSRL+UI*ZOB
               IF (REAL(EXPA)>-80..AND.REAL(EXPA)<=EMAX) THEN
                  CAB=EXP(EXPA)/U0
                  EA1=EA1-A1*CAB
                  EC1=EC1-C1*CAB
                  EP1=EP1-P1*CAB
               END IF
!     .                        .
!     ..........................
            END IF
!     !                              !
!     ________________________________
         END IF
!
!    -- If the receiver is in the lower-most layer
!
         IF (NOB==MLAYER) THEN
!   ______________________________
!   !                            !
            EA1=(0.,0.)
            EC1=(0.,0.)
            EP1=(0.,0.)
!   !                            !
!   ______________________________
         END IF
!   *                                *
!   **********************************
      END IF
!
!  -- If the source is in the lower-most layer
!
      IF (MPLUS==0) THEN
!   ***********************************
!   *                                 *
         IF (NOB/=0) THEN
!   _________________________________
!   !                               !
            IF (KITG==0) THEN
!    ...........................
!    .                         .
               EXPB=BE2-U0*ZSR-UI*ZOB
               IF (REAL(EXPB)>-80..AND.REAL(EXPB)<=EMAX) THEN
                  CAB=EXP(EXPB)
                  EB2=B2*CAB
                  ED2=D2*CAB
                  EQ2=Q2*CAB
               ELSE
                  EB2=(0.,0.)
                  ED2=(0.,0.)
                  EQ2=(0.,0.)
               END IF
!    .                           .
!    .............................
            END IF
!
            IF (KITG==1) THEN
!    .............................
!    .                           .
               EXPB=BE2-U0*ZSRH-UI*ZOB
               IF (REAL(EXPB)>-80..AND.REAL(EXPB)<=EMAX) THEN
                  CAB=-EXP(EXPB)/U0
                  EB2=B2*CAB
                  ED2=D2*CAB
                  EQ2=Q2*CAB
               ELSE
                  EB2=(0.,0.)
                  ED2=(0.,0.)
                  EQ2=(0.,0.)
               END IF
!
               EXPB=BE2-U0*ZSRL-UI*ZOB
               IF (REAL(EXPB)>-80..AND.REAL(EXPB)<=EMAX) THEN
                  CAB=-EXP(EXPB)/U0
                  EB2=EB2-B2*CAB
                  ED2=ED2-D2*CAB
                  EQ2=EQ2-Q2*CAB
               END IF
!    .                            .
!    ..............................
            END IF
!    !                                  !
!    ____________________________________
         END IF
!
         IF (NOB==0) THEN
!    ____________________________________
!    !                                  !
            EB2=(0.,0.)
            ED2=(0.,0.)
            EQ2=(0.,0.)
!    !                                  !
!    ____________________________________
         END IF
!   *                                      *
!   ****************************************
      END IF
!
!  -- For sources either in the upper-most or the lower-most layers
!
      IF (NMINUS==0.OR.MPLUS==0) THEN
!   ****************************************
!   *                                      *
         IF (ZOB<ZSR) THEN
!   _____________________________________
!   !                                   !
            IF (KPRM/=0.OR.NSR/=NOB) THEN
!   ----------------------------------
!   |                                |
!
               IF (KITG==0) THEN
!
                  EXPA=AE2-U0*ZSR+UI*ZOB
                  IF (REAL(EXPA)>-80..AND.REAL(EXPA)<=EMAX) THEN
                     CAB=EXP(EXPA)
                     EA2=A2*CAB
                     EC2=C2*CAB
                     EP2=P2*CAB
                  ELSE
                     EA2=(0.,0.)
                     EC2=(0.,0.)
                     EP2=(0.,0.)
                  END IF
               END IF
!
               IF (KITG==1) THEN
                  EXPA=AE2-U0*ZSRH+UI*ZOB
                  IF (REAL(EXPA)>-80..AND.REAL(EXPA)<=EMAX) THEN
                     CAB=-EXP(EXPA)/U0
                     EA2=A2*CAB
                     EC2=C2*CAB
                     EP2=P2*CAB
                  ELSE
                     EA2=(0.,0.)
                     EC2=(0.,0.)
                     EP2=(0.,0.)
                  END IF
!
                  EXPA=AE2-U0*ZSRL+UI*ZOB
                  IF (REAL(EXPA)>-80..AND.REAL(EXPA)<=EMAX) THEN
                     CAB=-EXP(EXPA)/U0
                     EA2=EA2-A2*CAB
                     EC2=EC2-C2*CAB
                     EP2=EP2-P2*CAB
                  END IF
               END IF
!   |                                |
!   ----------------------------------
            END IF
!
            IF (KPRM==0.AND.NSR==NOB) THEN
!   ----------------------------------
!   |                                |
               EA2=(0.,0.)
               EC2=(0.,0.)
               EP2=(0.,0.)
!   |                                |
!   ----------------------------------
            END IF
!   !                                  !
!   ____________________________________
         END IF
!
         IF (ZOB>=ZSR) THEN
!   ____________________________________
!   !                                  !
            IF (KPRM/=0.OR.NSR/=NOB) THEN
!   ---------------------------------
!   |                               |
!
               IF (KITG==0) THEN
!
                  EXPB=BE1+U0*ZSR-UI*ZOB
                  IF (REAL(EXPB)>-80..AND.REAL(EXPB)<=EMAX) THEN
                     CAB=EXP(EXPB)
                     EB1=B1*CAB
                     ED1=D1*CAB
                     EQ1=Q1*CAB
                  ELSE
                     EB1=(0.,0.)
                     ED1=(0.,0.)
                     EQ1=(0.,0.)
                  END IF
!
               END IF
!
               IF (KITG==1) THEN
!
                  EXPB=BE1+U0*ZSRH-UI*ZOB
                  IF (REAL(EXPB)>-80..AND.REAL(EXPB)<=EMAX) THEN
                     CAB=EXP(EXPB)/U0
                     EB1=B1*CAB
                     ED1=D1*CAB
                     EQ1=Q1*CAB
                  ELSE
                     EB1=(0.,0.)
                     ED1=(0.,0.)
                     EQ1=(0.,0.)
                  END IF
!
                  EXPB=BE1+U0*ZSRL-UI*ZOB
                  IF (REAL(EXPB)>-80..AND.REAL(EXPB)<=EMAX) THEN
                     CAB=EXP(EXPB)/U0
                     EB1=EB1-B1*CAB
                     ED1=ED1-D1*CAB
                     EQ1=EQ1-Q1*CAB
                  END IF
!
               END IF
!   |                                |
!   ----------------------------------
            END IF
!
            IF (KPRM==0.AND.NSR==NOB) THEN
!   ----------------------------------
!   |                                |
               EB1=(0.,0.)
               ED1=(0.,0.)
               EQ1=(0.,0.)
!   |                                |
!   ----------------------------------
            END IF
!   !                                !
!   __________________________________
         END IF
!   *                                      *
!   ****************************************
      END IF
!
!  -- For sources neither in the upper-most nor the lower-most layers
!
      IF (NMINUS/=0.AND.MPLUS/=0) THEN
!   ****************************************
!   *                                      *
         IF (NOB/=MLAYER) THEN
!   __________________________________
!   !                                !
            IF (KITG==0) THEN
!   ...........................
!   .                         .
               EXPA=AE1+U0*ZSR+UI*ZOB
               IF (REAL(EXPA)>-80..AND.REAL(EXPA)<=EMAX) THEN
                  EP1=EXP(EXPA)
                  EA1=A1*EP1
                  EC1=C1*EP1
                  EP1=P1*EP1
               ELSE
                  EA1=(0.,0.)
                  EC1=(0.,0.)
                  EP1=(0.,0.)
               END IF
               EXPA=AE2-U0*ZSR+UI*ZOB
               IF (REAL(EXPA)>-80..AND.REAL(EXPA)<=EMAX) THEN
                  EP2=EXP(EXPA)
                  EA2=A2*EP2
                  EC2=C2*EP2
                  EP2=P2*EP2
               ELSE
                  EA2=(0.,0.)
                  EC2=(0.,0.)
                  EP2=(0.,0.)
               END IF
!   .                         .
!   ...........................
            END IF
!
            IF (KITG==1) THEN
!   ...........................
!   .                         .
               EXPA=AE1+U0*ZSRH+UI*ZOB
               IF (REAL(EXPA)>-80..AND.REAL(EXPA)<=EMAX) THEN
                  CAB=EXP(EXPA)/U0
                  EA1=A1*CAB
                  EC1=C1*CAB
                  EP1=P1*CAB
               ELSE
                  EA1=(0.,0.)
                  EC1=(0.,0.)
                  EP1=(0.,0.)
               END IF
               EXPA=AE2-U0*ZSRH+UI*ZOB
               IF (REAL(EXPA)>-80..AND.REAL(EXPA)<=EMAX) THEN
                  CAB=-EXP(EXPA)/U0
                  EA2=A2*CAB
                  EC2=C2*CAB
                  EP2=P2*CAB
               ELSE
                  EA2=(0.,0.)
                  EC2=(0.,0.)
                  EP2=(0.,0.)
               END IF
!
               EXPA=AE1+U0*ZSRL+UI*ZOB
               IF (REAL(EXPA)>-80..AND.REAL(EXPA)<=EMAX) THEN
                  CAB=EXP(EXPA)/U0
                  EA1=EA1-A1*CAB
                  EC1=EC1-C1*CAB
                  EP1=EP1-P1*CAB
               ELSE
                  EA1=EA1-(0.,0.)
                  EC1=EC1-(0.,0.)
                  EP1=EP1-(0.,0.)
               END IF
               EXPA=AE2-U0*ZSRL+UI*ZOB
               IF (REAL(EXPA)>-80..AND.REAL(EXPA)<=EMAX) THEN
                  CAB=-EXP(EXPA)/U0
                  EA2=EA2-A2*CAB
                  EC2=EC2-C2*CAB
                  EP2=EP2-P2*CAB
               ELSE
                  EA2=EA2-(0.,0.)
                  EC2=EC2-(0.,0.)
                  EP2=EP2-(0.,0.)
               END IF
!   .                         .
!   ...........................
            END IF
!   !                                !
!   __________________________________
         END IF
!
         IF (NOB==MLAYER) THEN
!   __________________________________
!   !                                !
            EA1=(0.,0.)
            EC1=(0.,0.)
            EP1=(0.,0.)
            EA2=(0.,0.)
            EC2=(0.,0.)
            EP2=(0.,0.)
!   !                                !
!   __________________________________
         END IF
!
         IF (NOB/=0) THEN
!   __________________________________
!   !                                !
!
            IF (KITG==0) THEN
!    ...........................
!    .                         .
               EXPB=BE1+U0*ZSR-UI*ZOB
               IF (REAL(EXPB)>-80..AND.REAL(EXPB)<=EMAX) THEN
                  CAB=EXP(EXPB)
                  EB1=B1*CAB
                  ED1=D1*CAB
                  EQ1=Q1*CAB
               ELSE
                  EB1=(0.,0.)
                  ED1=(0.,0.)
                  EQ1=(0.,0.)
               END IF
               EXPB=BE2-U0*ZSR-UI*ZOB
               IF (REAL(EXPB)>-80..AND.REAL(EXPB)<=EMAX) THEN
                  CAB=EXP(EXPB)
                  EB2=B2*CAB
                  ED2=D2*CAB
                  EQ2=Q2*CAB
               ELSE
                  EB2=(0.,0.)
                  ED2=(0.,0.)
                  EQ2=(0.,0.)
               END IF
!    .                          .
!    ............................
            END IF
!
            IF (KITG==1) THEN
!    ............................
!    .                          .
               EXPB=BE1+U0*ZSRH-UI*ZOB
               IF (REAL(EXPB)>-80..AND.REAL(EXPB)<=EMAX) THEN
                  CAB=EXP(EXPB)/U0
                  EB1=B1*CAB
                  ED1=D1*CAB
                  EQ1=Q1*CAB
               ELSE
                  EB1=(0.,0.)
                  ED1=(0.,0.)
                  EQ1=(0.,0.)
               END IF
               EXPB=BE2-U0*ZSRH-UI*ZOB
               IF (REAL(EXPB)>-80..AND.REAL(EXPB)<=EMAX) THEN
                  CAB=-EXP(EXPB)/U0
                  EB2=B2*CAB
                  ED2=D2*CAB
                  EQ2=Q2*CAB
               ELSE
                  EB2=(0.,0.)
                  ED2=(0.,0.)
                  EQ2=(0.,0.)
               END IF
!
               EXPB=BE1+U0*ZSRL-UI*ZOB
               IF (REAL(EXPB)>-80..AND.REAL(EXPB)<=EMAX) THEN
                  CAB=EXP(EXPB)/U0
                  EB1=EB1-B1*CAB
                  ED1=ED1-D1*CAB
                  EQ1=EQ1-Q1*CAB
               ELSE
                  EB1=EB1-(0.,0.)
                  ED1=ED1-(0.,0.)
                  EQ1=EQ1-(0.,0.)
               END IF
               EXPB=BE2-U0*ZSRL-UI*ZOB
               IF (REAL(EXPB)>-80..AND.REAL(EXPB)<=EMAX) THEN
                  CAB=-EXP(EXPB)/U0
                  EB2=EB2-B2*CAB
                  ED2=ED2-D2*CAB
                  EQ2=EQ2-Q2*CAB
               ELSE
                  EB2=EB2-(0.,0.)
                  ED2=ED2-(0.,0.)
                  EQ2=EQ2-(0.,0.)
               END IF
!    .                          .
!    ............................
            END IF
!   !                                !
!   __________________________________
         END IF
!
         IF (NOB==0) THEN
!   __________________________________
!   !                                !
            EB1=(0.,0.)
            ED1=(0.,0.)
            EQ1=(0.,0.)
            EB2=(0.,0.)
            ED2=(0.,0.)
            EQ2=(0.,0.)
!   !                                !
!   __________________________________
         END IF
!   *                                      *
!   ****************************************
      END IF
!
! --- Calculate the kernel functions of the Hankel transforms
!     in terms of CAB etc. where CAB is defined as
!         CAB =a*exp(uz)+b*exp(-uz) and
!         CAB1=a*exp(uz)-b*exp(-uz).
!     The exponential functions exp(+-uz') are contained in a and b.
!
      IF (NMINUS==0) THEN
         CAB=EA1
         CAB1=EA1
         CCD=EC1
         CCD1=EC1
         CPQ=EP1
         CPQ1=EP1
!    --   Magnetic dipole kernels
         IF (KEMD==2) THEN
            DCAB =U0*EA1
            DCAB1=U0*EA1
            DCCD =U0*EC1
            DCCD1=U0*EC1
         END IF
      ELSE IF (MPLUS==0) THEN
         CAB=EB2
         CAB1=-EB2
         CCD=ED2
         CCD1=-ED2
         CPQ=EQ2
         CPQ1=-EQ2
!
         IF (KEMD==2) THEN
            DCAB =-U0*EB2
            DCAB1= U0*EB2
            DCCD =-U0*ED2
            DCCD1= U0*ED2
         END IF
      ELSE
         CAB=EA1+EA2+EB1+EB2
         CAB1=EA1+EA2-EB1-EB2
         CCD=EC1+EC2+ED1+ED2
         CCD1=EC1+EC2-ED1-ED2
         CPQ=EP1+EP2+EQ1+EQ2
         CPQ1=EP1+EP2-EQ1-EQ2
         IF (KEMD==2) THEN
            DCAB =U0*(EA1-EA2+EB1-EB2)
            DCAB1=U0*(EA1-EA2-EB1+EB2)
            DCCD =U0*(EC1-EC2+ED1-ED2)
            DCCD1=U0*(EC1-EC2-ED1+ED2)
         END IF
      END IF
      IF (NMINUS==0.OR.MPLUS==0) THEN
         IF (ZOB>=ZSR) THEN
            CAB=CAB+EB1
            CAB1=CAB1-EB1
            CCD=CCD+ED1
            CCD1=CCD1-ED1
            CPQ=CPQ+EQ1
            CPQ1=CPQ1-EQ1
            IF (KEMD==2) THEN
               DCAB =DCAB +U0*EB1
               DCAB1=DCAB1-U0*EB1
               DCCD =DCCD +U0*ED1
               DCCD1=DCCD1-U0*ED1
            END IF
         ELSE
            CAB=CAB+EA2
            CAB1=CAB1+EA2
            CCD=CCD+EC2
            CCD1=CCD1+EC2
            CPQ=CPQ+EP2
            CPQ1=CPQ1+EP2
            IF (KEMD==2) THEN
               DCAB =DCAB -U0*EA2
               DCAB1=DCAB1-U0*EA2
               DCCD =DCCD -U0*EC2
               DCCD1=DCCD1-U0*EC2
            END IF
         END IF
      END IF
!
!  ---- Now finally,  the kernel functions FKN.  Note
!       that they are corrected for various purposes
!
!  --- Electric dipoles
!
      IF (KEMD==1) THEN
!   **************************************
!   *                                    *
!
         IF (KPRM==1.OR.NSR==NOB) THEN
!   ___________________________________
!   !                                 !
!
!  ---  If rho=0,  only the first 4 element of fkn are needed.
!
            IF (KRHO==0) THEN
!   ---------------------------------
!   |                               |
!
!  --- Remove the charge terms from the kernels if KCHRG=1
!
!      Exx, Eyy and Ezz have different factors before the
!      integral before and after the charge term is removed.
!      This is taken into account here.
!
               IF (KEYG==1.OR.KEYG==2) THEN
                  IF (KCHRG==0) THEN
                     FKN(1)=.5*(KKH(NOB)*CAB+LUMBDA**2*VI*CCD1)
                     FKN(2)=LUMBDA**2*CPQ
                  ELSE
                     FKN(1)=KKH(NOB)*CAB
                     FKN(2)=-KKH(NOB)*CPQ
                  END IF
               END IF
               IF (KEYG==2) FKN(3)=.5*(UI*CAB1+LUMBDA**2*CCD)

            END IF

!  --- The general case with rho .ne. 0
!
            IF (KRHO/=0) THEN
!   ---------------------------------
!   |                               |
!
!  --- Corrections to the kernels for impoved filtering accuracy
!
!      FKN(2) and FKN(5) tends to 2*LUMBDA**2 as LUMBDA goes to
!      infinity for z=z'=0.  Since FKN(5) is related to z components
!      of the electric field which are not measured at the earth's
!      surface,  it is not corrected here.  For z' and z < 0, most
!      kernels for the electric field goes to LUMBDA**2 as LUMBDA
!      goes to infinity.  Since no electric field are measured in
!      such cases,  they will not be corrected here.
!
!      FKN(10) goes to LUMBDA as LUMBDA goest to infinity (CAB goes
!      to 1).  FKN(10) is related to Hzx and Hzy.  It is also
!      corrected on the earth's surface.
!
!      The filters used in this program (the filter for functions
!      with opening angle of analycity of pi/4) yield accurate
!      results even for kernels without correction.  However,
!      corrected kernel may allow the use of filters with much
!      higher accuracy (such as those for functions with opening
!      angle of analycity of pi/2 which decays not as fast as
!      the filter for pi/4)
!
!      The corrections must also be done in the routines computing
!      the electric fields,  routine ONE_D_GREEN.   Since z and z' will
!      never be zero in the routine THR_D_GREEN,  no corrections are
!      needed there.
!
               IF (KEYG==1.OR.KEYG==2) THEN
!
                  FKN(1)=CAB
                  IF (KCHRG==0) THEN
                     FKN(2)=LUMBDA**2*CCD1*VI-KKH(NOB)*CAB
!   -- Correct the kernels
                     IF (ABS(ZOB)<.001.AND.ABS(ZSR)<.001) FKN(2)=FKN(2)   &
                       -CMPLX(2.*LUMBDA**2,0.)
                     FKN(3)=LUMBDA**2*CPQ
                  ELSE
!   -- Remove the charge terms
                     FKN(2)=(0.,0.)
!  --- Note that the factor before the integral for Gzz after
!      removing the charge term is also different.  In order to
!      simplify routine ONE_D_GREEN,  this factor is considered here.
                     FKN(3)=-KKH(NOB)*CPQ
                  END IF
!
                  IF (KCHRG==0) THEN
                     FKN(4)=LUMBDA*CCD1*VI-KKH(NOB)/LUMBDA*CAB
                     FKN(5)=LUMBDA**3*CCD
                     FKN(6)=LUMBDA*CPQ1*VI
                  ELSE
                     FKN(4)=(0.,0.)
                     FKN(5)=-KKH(NOB)*(LUMBDA*CCD-UI/LUMBDA*CAB1)
                     FKN(6)=(0.,0.)
                  END IF
!
               END IF
!
               IF (KEYG==2) THEN
                  FKN(7)=LUMBDA**2*CCD-UI*CAB1
                  FKN(8)=UI*CAB1
                  FKN(9)=LUMBDA*CCD-UI/LUMBDA*CAB1
               END IF
!
!  --- Corrections on cab for sources and receivers in the air
!      and on the air-earth interface.
!      Here 1.E-3 is regarded as zero.
!
               IF (KEYG==2) THEN
                  IF ((ZSR<0..AND.ZOB<0.).AND.   &
                    (ABS(ZSR)>=1.E-3.OR.ABS(ZOB)>=1.E-3)) THEN
                     EA1=-U0*ABS(ZOB-ZSR)
                     EA2=U0*(ZSR+ZOB)
                     IF (REAL(EA1)>-80.) THEN
                        EB1=EXP(EA1)
                     ELSE
                        EB1=(0.,0.)
                     END IF
                     IF (REAL(EA2)>-80.) THEN
                        EB2=EXP(EA2)
                     ELSE
                        EB2=(0.,0.)
                     END IF
                     FKN(10)=LUMBDA*(CAB-LUMBDA/U0*(EB1-EB2))
!                     FKN(10)=LUMBDA*(CAB-LUMBDA/U0*(EXP(-U0*ABS(ZOB-ZSR))   &
!                           -EXP(U0*(ZSR+ZOB))))
                  ELSE IF (ABS(ZSR)<1.E-3.AND.ABS(ZOB)<1.E-3) THEN
                     FKN(10)=LUMBDA*(CAB-(1.,0.))
                  ELSE
                     FKN(10)=LUMBDA*CAB
                  END IF
                  FKN(11)=LUMBDA*CPQ
               END IF
!   |                               |
!   ---------------------------------
            END IF
!   !                                 !
!   ___________________________________
         END IF
!
! ---- Note that the primary parts are now substracted from the
!      final results if Z' and Z are not in the same layer
!
         IF (KPRM==0.AND.NSR/=NOB) THEN
!   ___________________________________
!   !                                 !
            IF (ZOB>=ZSR) THEN
!   ---------------------------------
!   |                               |
               EXPA=EXP(U0*ZSR-U0*ZOB)
               EB1=LUMBDA/U0*EXPA
               ED1=-1./LUMBDA*EXPA
               EQ1=LUMBDA/V0*EXPA
               PAB=EB1
               PAB1=-EB1
               PCD=ED1
               PCD1=-ED1
               PPQ=EQ1
               PPQ1=-EQ1
!   |                               |
!   ---------------------------------
            END IF
!
            IF (ZOB<ZSR) THEN
!   ---------------------------------
!   |                               |
               EXPA=EXP(-U0*ZSR+U0*ZOB)
               EA2=LUMBDA/U0*EXPA
               EC2=1./LUMBDA*EXPA
               EP2=LUMBDA/V0*EXPA
               PAB=EA2
               PAB1=EA2
               PCD=EC2
               PCD1=EC2
               PPQ=EP2
               PPQ1=EP2
!   |                               |
!   ---------------------------------
            END IF
!
            IF (KRHO==0) THEN
!   ---------------------------------
!   |                               |
!
               IF (KEYG==1.OR.KEYG==2) THEN
                  IF (KCHRG==0) THEN
                     FKN(1)   &
                      =.5*(KKH(NOB)*CAB-KKH(NSR)*PAB+LUMBDA**2*(VI*   &
                      CCD1-V0*PCD1))
                     FKN(2)=LUMBDA**2*(CPQ-PPQ)
                  ELSE
                     FKN(1)=KKH(NOB)*(CAB-PAB)
                     FKN(2)=-KKH(NOB)*(CPQ-PPQ)
                  END IF
               END IF
               IF (KEYG==2) FKN(3) =.5*(UI*CAB1-U0*PAB1+LUMBDA**2*(CCD-PCD))
!
            END IF
!
            IF (KRHO/=0) THEN
!   ---------------------------------
!   |                               |
!  --- Remove the charge terms from the kernels if KCHRG=1
!
!  !! This will never happen since KCHRG is set to zero in
!     routine THR_D_HF_TABLE where KPRM can be 0 for separating the
!     primary and the secondary Green's functions.
!
               IF (KEYG==1.OR.KEYG==2) THEN
!
                  FKN(1)=CAB-PAB
                  IF (KCHRG==0) THEN
                     FKN(2)=LUMBDA**2*(CCD1*VI-PCD1*V0)   &
                          -(KKH(NOB)*CAB-KKH(NSR)*PAB)
                     FKN(3)=LUMBDA**2*(CPQ-PPQ)
                  ELSE
                     FKN(2)=(0.,0.)
                     FKN(3)=-KKH(NOB)*(CPQ-PPQ)
                  END IF
!
                  IF (KCHRG==0) THEN
                     FKN(4)=LUMBDA*(CCD1*VI-PCD1*V0)   &
                          -(KKH(NOB)*CAB-KKH(NSR)*PAB)/LUMBDA
                     FKN(5)=LUMBDA**3*(CCD-PCD)
                     FKN(6)=LUMBDA*(CPQ1*VI-PPQ1*V0)
                  ELSE
                     FKN(4)=(0.,0.)
                     FKN(5)=-KKH(NOB)*(LUMBDA*CCD-UI/LUMBDA*CAB1)+KKH(NSR)   &
                          *(LUMBDA*PCD-U0/LUMBDA*PAB1)
                     FKN(6)=(0.,0.)
                  END IF
!
               END IF
!
               IF (KEYG==2) THEN
                  FKN(7)=LUMBDA**2*(CCD-PCD)-(UI*CAB1-U0*PAB1)
                  FKN(8)=UI*CAB1-U0*PAB1
                  FKN(9)=LUMBDA*(CCD-PCD)-(UI*CAB1-U0*PAB1)/LUMBDA
               END IF
!
               IF (KEYG==2) THEN
                  FKN(10)=LUMBDA*(CAB-PAB)
                  FKN(11)=LUMBDA*(CPQ-PPQ)
               END IF
!   |                               |
!   ---------------------------------
            END IF
!   !                                 !
!   ___________________________________
         END IF
!   *                                    *
!   **************************************
      END IF
!
!  --- Magnetic dipoles
!
      IF (KEMD==2) THEN
!   **************************************
!   *                                    *
!
!   -- Kernel for magnetic dipoles do not separate primary and
!      secondary parts if source and receiver are not in the
!      same layer.  However,  this can be readily modified by
!      following the computations above for the electric dipoles
!      (extra computation of PAB etc.).
!
         IF (KPRM==0.AND.NSR/=NOB) THEN
            WRITE (*,1010)
!            WRITE (4,1010)
            STOP 'Program aborted.  Check error messages in MarcoAir.LOG'
         END IF
!
         IF (KRHO/=0) THEN
!   ___________________________________
!   !                                 !
!
!   -- In order to make the output compatible with the electric
!      dipoles cases so that routine HFIL needs not
!      be changed,  here the output FKN are order in the same way
!      as for the electric dipoles, namely, FKN(1-3) and FKN(7-8)
!      are for the J0 transforms and the rest are for the J1
!      transforms.  Since only 8 distinctive kernels exist for
!      the magnetic dipoles,  some elements of FKN are not used.
!      Note that in Exx, Eyx, etc. of the following comments the
!      first indices are for the field compoments directions and
!      the second indices are for the source orientations.
!
!      On the earth's surface CAB and CAB1 goes to 1, DCAB and
!      DCAB1 goes to LUMBDA as LUMBDA goes to infinity.
!
!      Note that all kernels for both the electric fields and the
!      magnetic fields are computed although sometimes only one
!      set of them are required.  The following computations are
!      trivial in CPU times as compared to that needed by the other
!      parts of this routine.
!
            DG=DCCD-DCAB1*UI/LUMBDA/LUMBDA
!
            FKN(1)=VI*(CPQ1+DCCD1)*LUMBDA*LUMBDA-KKH(NOB)*DCAB
            FKN(2)=DCAB
            FKN(3)=UI*DCAB1
            FKN(4)=VI*(CPQ1+DCCD1)*LUMBDA-KKH(NOB)*DCAB/LUMBDA
            FKN(5)=(CPQ+DCCD)*LUMBDA*LUMBDA*LUMBDA
            FKN(6)=LUMBDA*CAB
            FKN(7)=(DG+CPQ)*LUMBDA*LUMBDA
            IF (KPRM==1.AND.ABS(ZSR-ZOB)<1.E-4) THEN
               FKN(8)=(CAB-(1.,0.))*LUMBDA*LUMBDA
            ELSE
               FKN(8)=CAB*LUMBDA*LUMBDA
            END IF
            FKN(9)=LUMBDA*(DG+CPQ)
            FKN(10)=LUMBDA*DCAB
            FKN(11)=LUMBDA*UI*CAB1
!   !                                 !
!   ___________________________________
         END IF
!
         IF (KRHO==0) THEN
!   ___________________________________
!   !                                 !
            DG=DCCD-UI/LUMBDA**2*DCAB1
!
            FKN(1)=.5*DCAB*KKH(NOB)+.5*                                    &
                   (VI*(CPQ1+DCCD1)*LUMBDA*LUMBDA) ! Eyx and Exy
            FKN(2)=UI*DCAB1+.5*LUMBDA**2*(DG+CPQ)  ! Hxx and Hyy
            FKN(3)=CAB*LUMBDA*LUMBDA                           ! Hzz
!   !                                 !
!   ___________________________________
         END IF
!   *                                    *
!   **************************************
      END IF
!
      RETURN
!
 1010 FORMAT (' Error: kernels for magnetic dipoles do not ',   &
            'separate primary and secondary ',/' parts if ',   &
            'source and receiver are not in the same layer.',   &
            /' Modify routine ONE_D_KERNEL if you want to.')
!
   END SUBROUTINE ONE_D_KERNEL
!
!
   SUBROUTINE ONE_D_0_2_INFTY(KEYG,KEMD,MLAYER,ZBND,LRYTH,KKH,RMU,KPRM, &
                              KITG,KCHRG,NOB,NSR,ZOB,ZSRH,ZSRL,ALMAX,KEH,S)
!
!****   Numerical integration for the case rho = 0
!
!       The integration range is from 0 to infinity. This routine determines
!       the upper integral limit automatically by double the integration
!       interval untill convergence is reached.
!
!    Input parameters:
!
!       KEH : =1, 2, 3, or 4 represents the number of the kernel values
!              for the field components. For magnetic dipoles,  the
!              first one is for the E-fields, and the last two are for
!              the H-fields.   KEH depends on the values of KEYG and KEMD.
!
!              For electric dipoles,  KEMD=1:
!
!                KEYG=1:  KEH=2,  the two elements for the general electric
!                         Green's function;
!                KEYG=2:  KEH=3,  the first two are for the E-fields,
!                         and the third one is for the H-fields;
!                KEYG=3:  KEH=3,  the three elements for the current and
!                         charge terms of the electric Green's function.
!                         in the case of KEYG=1.  The last one is for the
!                         magnetic field (as the third element as in the
!                         case of KEYG=2);  and
!                KEYG=4:  KEH=4,  the first three elements are those as
!                         in the case of KEYG=3.  The last one is for the
!                         magnetic field (as the third element as in the
!                         case of KEYG=2).
!
!              For magnetic dipoles,  KEMD=2:
!
!                KEYG=1:  KEH=1;  and
!                KEYG=3:  KEH=2.
!
!              KEH is defined in routines THR_D_HF_TABLE and ONE_D_HF_TABLE
!              (parameter NS)
!
!       Parameters KEMD, MLAYER, ZBND, LRYTH,  KKH,  KPRM,
!       KITG, KCHRG, NOB,  NSR and ALMAX are to be passed to routine
!       kernel.  See routine kernel for their descriptions.
!
!    Output parameters:
!
!       S:  Complex S(4).  Kernels for electric or magnetic fields in case
!           rho=0 (source and receiver above one another).   See the above
!           description for KEH.
!
!****  CALLED by:  gridhf, gridcs
!
!****  CALLS    :  simpsn
!
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: KEYG,KCHRG,KEH,KEMD,KITG,KPRM,           &
                             MLAYER,NOB,NSR
      COMPLEX, INTENT(IN) :: KKH(0:MLAYER)
      REAL, INTENT(IN) :: LRYTH(MLAYER),ZBND(0:MLAYER),       &
                          ALMAX,ZOB,ZSRH,ZSRL,RMU(0:MLAYER)
      COMPLEX, INTENT(OUT) :: S(4)
!
      REAL :: A0,A1,B1,T
      INTEGER :: I,J,N,NINT,KRHO
      COMPLEX :: S0(4)
!
! --- EPS is the desired relative accuracy of numerical intergration.
!
      REAL, PARAMETER :: EPS=1.E-4
!
!  -- Parameter KRHO is required by routine ONE_D_KERNEL.
!     KRHO=0 means rho is zero
!
      KRHO=0
!
      A0=1.E-8
      A1=A0
      B1=1.E-4
      T=B1-A0
      DO I=1,KEH
         S(I)=(0.,0.)
      END DO
      CALL ONE_D_SIMPSON(KEYG,KEMD,MLAYER,ZBND,LRYTH,KKH,RMU,KPRM,   &
                         KRHO,KITG,KCHRG,NOB,NSR,ZOB,ZSRH,ZSRL,            &
                         A1,B1,EPS,8,N,KEH,S0,S)
      DO J=1,KEH
         S(J)=S0(J)
      END DO
      NINT=N
  100 A1=B1
      B1=A1+T*2.
      IF (B1>ALMAX) THEN
!       WRITE (4,*)' Warning: Numerical integration did not converge.',
!    &            ' Upper limit tended to infinity.',
!    &            ' Terminated automatically.'
!       DO 2 I=1,KEH
! 2     WRITE (4,*)' Accuracy of integration: ',
!    &             ABS(S0(I))/(1.E-30+ABS(S(I)))
!
!******  Return
!
         RETURN
      END IF
      CALL ONE_D_SIMPSON(KEYG,KEMD,MLAYER,ZBND,LRYTH,KKH,RMU,KPRM,   &
                         KRHO,KITG,KCHRG,NOB,NSR,ZOB,ZSRH,ZSRL,            &
                         A1,B1,EPS,8,N,KEH,S0,S)
      DO J=1,KEH
         S(J)=S(J)+S0(J)
      END DO
      NINT=NINT+N
      T=B1-A0
      DO I=1,KEH
         IF (ABS(S0(I))>(1.E-30+ABS(S(I)))*EPS) THEN
            GOTO 100
         END IF
      END DO
!
      RETURN
!
   END SUBROUTINE ONE_D_0_2_INFTY
!
!
 SUBROUTINE ONE_D_SIMPSON (KEYG,KEMD,MLAYER,ZBND,LRYTH,KKH,RMU,KPRM,KRHO,KITG,   &
                           KCHRG,NOB,NSR,ZOB,ZSRH,ZSRL,A,B,EPS,K,N,KEH,S,ST)
!------------------------------------------------------------------------------

!****   NUMERICAL INTEGRATION USING ONE_D_SIMPCSSON'S RULE
!       FOR A LIMITED ROI_INTERVAL [A, B]
!
!    Input parameters:
!
!      A:    Real.  Lower limit of the interval of integration.
!      B:    Real.  Upper limit of the interval of integration.
!      EPS:  Real.  Desired relative accuracy of the numerical
!            integration.
!      K:    Integer.  Miminal number of steps for the prevention
!            of false convergence.
!      ST:   Complex.  The accumulative sum of the integrals in
!            all previous intervals.  It is used to check if the
!            results for the interval [A, B] is significant for
!            accurate computation.
!
!      Parameter KEH is defined in routine ONE_D_0_2_INFTY.  See detailed
!      description in that routine.
!
!      Parameters KEMD, MLAYER, ZBND, LRYTH,  KKH,  KPRM,
!      KRHO, KITG, KCHRG, NOB,  NSR,  and FKN are to be passed
!      to routine kernel.  See routine kernel for their descriptions.
!
!    Output parameters:
!
!      N:    Integer. Number of steps used for convergence.
!
!            Note that the parameter NLIM controls the maximal
!            number of steps in the integration.  In actual modeling
!            problems the numerical integration here occasionally
!            do not converge,  no matter how large NLIM is.
!            However, this have little effects on the final results.
!            But the computation can be greatly affected by NLIM.
!            NLIM is preasigned in the parameter list.
!
!      S:    Complex S(5).  Results of the integration.  Check
!            routine ONE_D_0_2_INFTY for detailed description.
!
!
!    Note that there are multiple returns and gotos due to the complexity
!    of convergence tests.  They are marked by five stars like *****.
!
!
!****   CALLED by:  simpinf
!
!****   CALLS    :  kernel
!
 IMPLICIT NONE


 INTEGER, INTENT(IN) :: KCHRG,KEH,KEMD,KITG,KPRM,KRHO,MLAYER,NOB,NSR,KEYG
 COMPLEX, INTENT(IN) :: ST(3)
 COMPLEX, INTENT(OUT) :: S(4)
 COMPLEX :: S0(4),T1(4),T2(4),FKN(11)
 COMPLEX, INTENT(IN) :: KKH(0:MLAYER)
 REAL, INTENT(IN) :: LRYTH(MLAYER),ZBND(0:MLAYER),RMU(0:MLAYER)
 INTEGER, INTENT(OUT) :: N
 REAL, INTENT(IN) :: A,B,EPS,ZOB,ZSRH,ZSRL
 REAL :: H,X,C
 INTEGER ::  I,J,K
 INTEGER, PARAMETER :: NLIM=1024

 DO I=1,KEH
    S0(I)=(0.,0.)
 END DO

 N = 1
 C = ABS (A) + ABS (B)
 H= .5 * (B - A)

 CALL ONE_D_KERNEL (KEYG,KEMD,MLAYER,ZBND,LRYTH,KKH,RMU,KPRM,KRHO,KITG, &
                    KCHRG,NOB,NSR,ZOB,(ZSRH+ZSRL)/2.,ZSRH,ZSRL,A,FKN)

 T1(1:KEH) = H * FKN(1:KEH)

 CALL ONE_D_KERNEL (KEYG,KEMD,MLAYER,ZBND,LRYTH,KKH,RMU,KPRM,KRHO,KITG, &
                    KCHRG,NOB,NSR,ZOB,(ZSRH+ZSRL)/2.,ZSRH,ZSRL,B,FKN)

 DO J=1,KEH
   T1(J) = T1(J) + H* FKN(J)
 END DO

!***** Branch entry point for GOTO 10

  100 X=A-H
      DO J=1,KEH
         T2(J)=.5*T1(J)
      END DO
      DO I=1,N
         X=X+2.*H
         CALL ONE_D_KERNEL(KEYG,KEMD,MLAYER,ZBND,LRYTH,KKH,RMU,KPRM, &
                           KRHO,KITG,KCHRG,NOB,NSR,ZOB,(ZSRH+ZSRL)/2.,     &
                           ZSRH,ZSRL,X,FKN)
         DO J=1,KEH
            T2(J)=T2(J)+H*FKN(J)
         END DO
      END DO
      DO J=1,KEH
         S(J)=.333333333*(4.*T2(J)-T1(J))
      END DO
!
! ---- K is used to prevent false convergence
!
      IF (N>K) THEN
!-------------------------------
!|                             |
!
! ---- If convrged within the interval [a,b], then return
!
         DO I = 1, KEH
           IF (ABS (S(I) - S0(I)) > (1.E-30 + ABS (S(I)))*EPS) EXIT
           IF (I == KEH) RETURN
         END DO
!
! ---- In case inconvergence within the interval [a,b], still
!      return if the absolut values of the integrals for this
!      interval are less than eps times the integrals over the
!      whole interval (0,b] or if step number is over NLIM.
!
!***** Branch entry point for GOTO 30
!
         DO I = 1, KEH
           IF (ABS (S(I)) > ABS (ST(I)) * EPS) EXIT
           IF (I == KEH) RETURN
         END DO
!
!***** Branch entry point for GOTO 40
!
         IF (N>=NLIM) THEN
            DO I=1,KEH
               S0(I)=(S(I)-S0(I))/(ABS(S(I))+ABS(ST(I))+1.E-30)
            END DO
!
! ---- Warning will be issued if the total accuracy (relative)
!      is worse than EPS
!
            DO I=1,KEH
               IF (ABS(S0(I))>EPS) THEN
!          WRITE (4,*) 'Warning: numerical integration ',
!    &                'did not converge!'
!          WRITE (4,*) ' Accuracy of integration:',
!    &                (ABS(S0(II)),II=1,KEH)
!
!***** Return
!
                  RETURN
               END IF
            END DO
!
!***** Return
!
            RETURN
         END IF
!
!|                             |
!-------------------------------
      END IF
!
      N=N+N
      H=.5*H
      DO J=1,KEH
         S0(J)=S(J)
         T1(J)=T2(J)
      END DO
!
!***** Restart of a new iteration if the step length is still
!      distinguishable by the machine.
!
      IF (ABS(C+H-C)>1.E-37) THEN
         GOTO 100
      END IF
!
      WRITE (4,*) ' Warning: numerical integration step ',      &
                  ' tends to zero. Terminated automatically',   &
                  'A, B and N =',A,B,N
!
      RETURN
!
   END SUBROUTINE ONE_D_SIMPSON


   SUBROUTINE ONE_D_SOURCE (ANGLES,RECVR,NCRD,TX_CRDX,TX_CRDY,   &
                            TX_CRDZ,KEYG,FRQ,MLAYER,ZBND,AJ,    &
                            KKH,CDH,RMU,NZOB,ZOBG,NZSR,ZSRG,RHOMIN,      &
                            NHFILM,RRG,NRG,GRHF,GRHO0,EHFLD)
!
!****  Electromagnetic fields due to electric or magnetic sources
!
!      Fields are obtained by the integration of dipole sources
!
!
!   Input parameters:
!
!     ANGLES: Real ANGLES(2),  orientations of the magnetic dipoles
!             The two elements of MD_ANGLE(1:2,J) contain the dip angle
!             MD_ANGLE(1,J) and the azimuthal angle MD_ANGLE(2,J) of the Jth
!             magnetic dipole.  The dip angle is the angle from the axis down
!             to the earth (note that this is the internal positive z-axis
!             which differs from the input-output z-axis that points upwards).
!             The azimuthal angle is the angle from the North,  the y-axis.
!             Thus for a vertical magnetic dipole MD_ANGLE(1,J)=PI for
!             pointing up air and MD_ANGLE(1,J) =0 for pointing down.  A
!             horizontal magnetic dipole pointing North will have MD_ANGLE(1,J)=PI/2
!             and MD_ANGLE(2,J)=0,  and a horizontal magnetic dipole
!             pointing East will have MD_ANGLE(1,J)=90 and MD_ANGLE(2,J)=PI/2.
!     RECVR: Real RECVR(3),  receiver coordinates in the x, y,
!             and z-directions,  respectively.
!     NCRD:
!     TX_CRDX, TX_CRDY, TX_CRDZ: Real TX_CRDX(NCRD), TX_CRDY(NCRD),
!     TX_CRDZ(NCRD):   the first elements of TX_CRDX etc.
!                      contain the coordinates of the centre of the
!                      magnetic dipole.
!     KEYG:   Integer, mode of computation controlling whether mangetic
!             fields are computed.
!             KEYG=1:  Computing electric fields only.  This is used
!                      when the exciting electric fields are computed
!                      as the right hand side of the matric equation
!                      for 3D modeling;  and
!                 =2:  All components of the electric and magnetic fields
!                      are computed.
!     AJ:     Real,  current amplitude for electric sources or
!             dipole moment for magnetic sources.
!
!     Parameters FRQ, MLAYER, ZBND, KKH, CDH, NZOB, ZOBG, NZSR, ZSRG,
!     RHOMIN, NHFILM, RRG, NRG, GRHF, and GRHO0 are to be passed
!     into routine ONE_D_GREEN.  See ONE_D_GREEN for detailed descriptions.
!
!
!   Output parameters:
!
!     EHFLD:  Complex EHFLD(6),  components of the electric and magnetic
!             fields in the x, y, and z-directions,  respectively.
!
!
!****  CALLED by:  main, eincs, eincsgs
!
!****  CALLS    :  line, simp, green
!
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: NZOB,NZSR,NHFILM,NRG,MLAYER,KEYG,NCRD
      REAL, INTENT(IN) :: RECVR(3),TX_CRDX(NCRD),TX_CRDY(NCRD),TX_CRDZ(NCRD), &
                          ANGLES(2)
      COMPLEX, INTENT(OUT) :: EHFLD(6)
!
      INTEGER ::  KEMD
      REAL :: XOB,XSR,YOB,YSR,ZOB,ZSR,ZSR1,ZSR2,XSR2,YSR2
      REAL :: PSTION(8),PX,PY,PZ,DIP,AZIM
      COMPLEX :: EX,EY,EZ,HX,HY,HZ,                             &
                 EXX,EYX,EZX,EXY,EYY,EZY,EXZ,EYZ,EZZ,                   &
                 HXX,HYX,HZX,HXY,HYY,HZY,HXZ,HYZ,HZZ,ECOMP(9),HCOMP(9)
      EQUIVALENCE (PSTION(1),XOB),(PSTION(2),YOB),(PSTION(3),ZOB),      &
                  (PSTION(4),XSR),(PSTION(5),YSR),(PSTION(6),ZSR),      &
!      --  The last two elements of PSIION are either zsr1 and zsr2 for
!          a vertical bipole or xsr2 and ysr2 for an electric bipole.
                  (PSTION(7),ZSR1,XSR2),(PSTION(8),ZSR2,YSR2),          &
                  (ECOMP(1),EXX),(ECOMP(2),EYX),(ECOMP(3),EZX),         &
                  (ECOMP(4),EXY),(ECOMP(5),EYY),(ECOMP(6),EZY),         &
                  (ECOMP(7),EXZ),(ECOMP(8),EYZ),(ECOMP(9),EZZ),         &
                  (HCOMP(1),HXX),(HCOMP(2),HYX),(HCOMP(3),HZX),         &
                  (HCOMP(4),HXY),(HCOMP(5),HYY),(HCOMP(6),HZY),         &
                  (HCOMP(7),HXZ),(HCOMP(8),HYZ),(HCOMP(9),HZZ)
!
! --- Parameters used solely for data passing
!
      REAL, INTENT(IN)    :: RMU(0:MLAYER)
      COMPLEX, INTENT(IN) :: KKH(0:MLAYER),CDH(0:MLAYER),                  &
                             GRHF(11,NHFILM,NZSR,NZOB),GRHO0(4,NZSR,NZOB)
      REAL, INTENT(IN) :: ZBND(0:MLAYER),ZOBG(NZOB),ZSRG(2,NZSR),          &
                          FRQ,RHOMIN,RRG(NRG),AJ
      XOB=RECVR(1)
      YOB=RECVR(2)
      ZOB=RECVR(3)
!
!  -- Determine the accuracy of the numerical integration according
!     to the accuracy level.
!
      KEMD=2
      ZSR1=TX_CRDZ(1)
      ZSR2=ZSR1

!  -- Arbitrary magnetic dipoles
!
!     Note that the first elements of the array crdx, crdy, and
!     crdz are the dipole coordinates
!
!     Zsr1 and zsr2 have been determined earlier
!

      XSR=TX_CRDX(1)
      YSR=TX_CRDY(1)
      ZSR=TX_CRDZ(1)
      CALL ONE_D_GREEN(KEMD,KEYG,PSTION,FRQ,MLAYER,ZBND,KKH,CDH,RMU,  &
                       NZOB,ZOBG,NZSR,ZSRG,RHOMIN,NHFILM,RRG,NRG,GRHF,        &
                       GRHO0,ECOMP,HCOMP)

!  --- Note that the coordinate systems for I/O and for computations are
!      different.  It is important to define the dip angle as the angle from
!      the axis pointing down and the azimuthal angle as the angle from the
!      North.

      DIP = ANGLES(1)
      AZIM = ANGLES(2)

      PX=SIN(DIP)*COS(AZIM)*AJ  !    Multiplied by dipole moment
      PY=SIN(DIP)*SIN(AZIM)*AJ
      PZ=COS(DIP)*AJ
!
      EX=PX*EXX+PY*EXY+PZ*EXZ
      EY=PX*EYX+PY*EYY+PZ*EYZ
      EZ=PX*EZX+PY*EZY+PZ*EZZ
      IF (KEYG==2) THEN
         HX=PX*HXX+PY*HXY+PZ*HXZ
         HY=PX*HYX+PY*HYY+PZ*HYZ
         HZ=PX*HZX+PY*HZY+PZ*HZZ
      END IF

      EHFLD(1)=EX
      EHFLD(2)=EY
      EHFLD(3)=EZ
      IF (KEYG==2) THEN
         EHFLD(4)=HX
         EHFLD(5)=HY
         EHFLD(6)=HZ
      END IF


 END SUBROUTINE ONE_D_SOURCE


 SUBROUTINE ONE_D_GREEN (KEMD,KEYG,PSTION,FRQ,MLAYER,ZBND,KKH,CDH,RMU,NZOB,ZOBG, &
                         NZSR,ZSRG,RHOMIN,NHFILM,RRG,NRG,GRHF,GRHO0,ECOMP,HCOMP)
!------------------------------------------------------------------------------------
!****   Electromagnetic fields of a dipole source in
!       layered anisotropic earths
!
!
!   Input parameters:
!
!     KEMD:   Integer,  switch for electric or magnetic dipoles.
!             KEMD=1:  electric dipoles;  and
!                 =2:  magnetic dipoles.
!     KEYG:   Integer,  mode of computation controlling whether mangetic
!             fields are computed.
!             KEYG=1:  Computing electric fields only.  This is used
!                      when the exciting electric fields are computed
!                      as the right hand side of the matric equation
!                      for 3D modeling;  and
!                 =2:  All components of the electric and magnetic fields
!                      are computed.
!     PSTION: Real PSTION(8),  the positions of the source and the
!             receiver plus the lower and upper z-coordinates the kernels
!             are integrated analytically.  The first 3 elements of the
!             array is for the x, y, and z-coordinates of the receiver,
!             which are followed by the x, y, and z-coordinates of the
!             source.  The last two elements are the lower and upper
!             z-coordinates of the integral interval.  If the kernels
!             are not integrated,  the last two elements must be identical
!             to the source z-coordinate,  i.e.,  the last three elements
!             of PSTION must be the same if the kernels are not to be
!             integrated in the vertical direction.
!     FRQ:    Real(*4), frequency.
!     MLAYER: Integer, number of layers excluding the air.
!     ZBND:   Real ZBND(0:MLAYER), coordinates of the layer
!             boundaries with the air earth interface being zbnd(0)=0.
!     KKH:    Complex(*8) KKH(0:MLAYER),  square ( **2) of the complex wave
!             number of the layers including the air (layer number 0).
!     CDH:    Complex CDH(0:MLAYER),  complex conductivities of
!             the layers in the lateral direction including the
!             air (layer 0).
!     NZOB:   Integer, number of z-levels. Determined in the main program.
!     ZOBG:   Real ZOBG(NZOB), the nzob z-levels coordinates.
!     NZSR:   Integer,  number of z'-levels.
!     ZSRG:   Real ZSRG(NZSR),  the nzsr z'-levels.
!     RHOMIN: Real,  minimum of the lateral grid (rho) which determines
!             the minimum of rho in the Hankel integrals.  RHOMIN=0.1 for
!             the low frequency module and RHOMIN=0.01 for the high
!             frequency module.
!     NHFILM: Integer,  maximal number of grids in the lateral direction
!             (rho) for all the Hankel integrals.
!     NRG:    Integer,  number of grids in the lateral direction (rho).
!     RRG:    Real RRG(NRG),  the NRG grid points in rho.
!     GRHF:   Complex GRHF(11,NHFILM,NZSR,NZOB), tabulated values of
!             the Hankel integrals.  To be passed to the interpolation
!             routines.
!     GRHO0:  Complex GRHO0(4,NZSR,NZOB), tabulated values of the
!             Hankel integrals for rho=0 (source and receiver have the
!             same x and y coordinates).
!
!   Output parameters:
!
!     ECOMP:  Complex ECOMP(9),  the nine components of the electric
!             field tensor,  Exx, Eyx, Ezx, Exy, ..., and Ezz.   See the
!             EQUIVALENCE statement.   Note that in parameters Exy etc.
!             the first index "x" denotes the direction of the field and
!             the second index "y" denotes the direction of the source.
!     HCOMP:  Complex HCOMP(9),  the nine components for the magnetic
!             field tensor which is arranged in the same way as ECOMP.
!
!
!     Note that the Green's functions are not separated into primary and
!     secondary parts as in routine THR_D_GREEN.   Whether the kernels are
!     integrated analytically in the z'-directions or not is controlled
!     in the routine ONE_D_HF_TABLE.  The calling routine ONE_D_LINE must
!     correctly determine this.
!
!     For loop sources, i.e., ksrc=4, 5, or 6, the charge terms in
!     the electric fields need be canceled in the program since
!     they cannot be canceled numerically. This is considered in
!     kernel function, routine kernel, as well as in the routines
!     gridhf and gridcs.   However,  the components Ezz and Exx
!     for rho=0 have different factors before and after the charge
!     terms are removed.  Those factors are numerically more
!     advantagous to be considered here than to be considered in
!     the kernel functions.
!
!     Note that the Hankel integrals for the Green's functions must
!     computed before this routine is called.  The integrals are first
!     tabulated by routine ONE_D_HF_TABLE and then interpolated by routines
!     INTERPO_PLPOL etc.
!
!****  CALLED by:  line, circle, csource
!
!****  CALLS    :  intpl15, intplpol,  and intplrat (never active
!                  unless krat is changed to 1)

 IMPLICIT NONE

 INTEGER, INTENT(IN) :: KEMD,KEYG,NZOB,NZSR,NHFILM,NRG,MLAYER
 REAL, INTENT(IN) :: RMU(0:MLAYER)
 COMPLEX, INTENT(IN) :: KKH(0:MLAYER),CDH(0:MLAYER),                  &
                        GRHF(11,NHFILM,NZSR,NZOB),GRHO0(4,NZSR,NZOB)
 REAL, INTENT(IN) :: ZBND(0:MLAYER),ZOBG(NZOB),ZSRG(2,NZSR),FRQ,      &
                     RHOMIN,PSTION(8)
 COMPLEX, INTENT(OUT) :: ECOMP(9),HCOMP(9)

 REAL :: CRRTN,RHO,RHO1,RR0,RR1
 REAL :: C1,XOB,YOB,ZOB,XSR,YSR,ZSR,RRG(NRG)
 INTEGER :: NF,I,II,NS,NOB,IOB,ISR
 COMPLEX :: EXX,EYX,EZX,EXY,EYY,EZY,EXZ,EYZ,EZZ,   &
            HXX,HYX,HZX,HXY,HYY,HZY,HXZ,HYZ,HZZ,   &
            S(4),HCRR,HF(11),WMU,WMU0
 LOGICAL, PARAMETER :: PLPOL=.FALSE.

!  --- Routine INTERPO_PLPOL may used for computing normal fields at
!      high frequencies.  It offers higer accuracy for large rho (> 10 m),
!      but its run time is large.
!
!      RHO(XR,YR,XI,YI)=SQRT((XR-XI)**2+(YR-YI)**2)

 C1 =1. /4. /3.1415926
 WMU0=CMPLX(0.,78.9568352E-7*FRQ)

! --- Extract parameter values for XOB etc. from PSTION

 XOB=PSTION(1)
 YOB=PSTION(2)
 ZOB=PSTION(3)
 XSR=PSTION(4)
 YSR=PSTION(5)
 ZSR=PSTION(6)

 DO I = MLAYER-1, 0, -1
   IF (ZOB >= ZBND(I)) THEN
      NOB = I + 1
      EXIT
   END IF
   IF (I == 0) NOB = 0
 END DO


 WMU=WMU0*RMU(NOB)

! ---- Determine the level of zob in the grid array zobg.
!      Note that there is only one z'-level which is
!      therefore ignored.

 DO I = 1, NZOB
   IF (ABS (ZOB-ZOBG(I)) < .01) THEN
     IOB=I
     EXIT
   END IF
   IF (I == NZOB)  WRITE (4,*) 'Warning: failed to choose a z-level for zob=',ZOB
 END DO


! ---- Determine the level of zsr in the grid array zsrg
!      Note that the z'-levels in the zsr-direction are
!      determined by the full lengths of the electrode span.
!
!      Note that in case of horizontal current bipoles or loops
!      zsr1 and zsr2 take the value of the z-coordinates of
!      any electrode.
!
      DO I=1,NZSR
         IF (ABS (ZSR-ZSRG(1,I)) < .01) THEN
            ISR=I
            GOTO 300
         END IF
      END DO

  300 IF (KEYG==1) THEN
         NF=6
      ELSE
         NF=11
      END IF
!
!  --- Note that both electric and magnetic dipoles have 6 non-zero
!      kernels for the electric fields,  #1 through 6,  and 5 non-zero
!      kernels for magnetic fields,  # 7 through 11.
!      For rho=0,  electric dipoles have 2 non-zero electric kernels
!      and one magnetic kernel, and magnetic dipoles have only one
!      non-zero kernel for the electric fields.
!
!      The kernels for the magnetic dipoles are arranged in such an
!      order that is compatible with the kernels for the electric
!      fields in using all the subroutines,  especially routine
!      HFIL.
!
      IF (KEYG==2) NS=3
      IF (KEYG==1) THEN
         IF (KEMD==1) THEN
            NS=2
         ELSE
            NS=1
         END IF
      END IF
!
      RHO1=RHO(XOB,YOB,XSR,YSR)
!
      IF (RHO1>=RHOMIN) THEN
!
!  --- Interpolate the hankel integrals
!
!  --- Routine INTERPO_PLPOL may used for computing normal fields at
!      high frequencies.  It offers higer accuracy for large rho (> 10 m),
!      but its run time is large.
!
         IF (PLPOL) THEN
            CALL INTERPO_PLPOL(GRHF,RRG,NRG,NHFILM,NZSR,NZOB,ALOG(RHO1),   &
                              IOB,ISR,HF,NF)
         ELSE
            CALL INTERPO_LG6(GRHF,RRG,NRG,NHFILM,NZSR,NZOB,ALOG(RHO1),     &
                             IOB,ISR,HF,NF)
         END IF
!
!  --- HF are now divided by rho
!
         DO II=1,NF
            HF(II)=HF(II)/RHO1
         END DO
!
      END IF
!
! ---- Electric dipoles
!
      IF (KEMD==1) THEN
!   *****************************************
!   *                                       *
         IF (RHO1>=RHOMIN) THEN
!   --------------------------------------
!   |                                    |
!
!  --- The kernel related to HF(2) tends to 2*LUMBDA**2 as
!      LUMBDA goes to infinity for z=z'=0.  The corresponding
!      kernel has been corrected in the function ONE_D_KERNEL.
!      This affect Exx, Eyx, Eyy and Exy only.  Other components
!      and most components of the electric field in the air
!      have similar behaviors but are not corrected since they
!      are not measured.   The hankel filters used in this program
!      do yield accurate results for all cases even without
!      correction.   The corrections done here can hopefully
!      boost the accuracy of the code.  It is also possible to
!      use better filters which decay slower but have much higher
!      accuracy.
!
!      Note that the charge terms which also involves HF(2)
!      are set to zero in ONE_D_KERNEL for loop type sources in order
!      to councel the charge terms accurately
!
            IF (ABS(ZOB)<.001.AND.ABS(ZSR)<.001) THEN
              CRRTN=2./RHO1**3
               EXX=-C1*WMU*HF(1)-C1/CDH(NOB)                           &
                   *(((XOB-XSR)/RHO1)**2*(HF(2)-CMPLX(CRRTN,0.))                 &
                   +(1.-2.*((XOB-XSR)/RHO1)**2)/RHO1*HF(4))
               EYX=-C1/CDH(NOB)*(XOB-XSR)*(YOB-YSR)                    &
                   /RHO1**2*(HF(2)-CMPLX(CRRTN,0.)-2.*HF(4)/RHO1)
               EYY=-C1*WMU*HF(1)-C1/CDH(NOB)                           &
                   *(((YOB-YSR)/RHO1)**2*(HF(2)-CMPLX(CRRTN,0.))                 &
                   +(1.-2.*((YOB-YSR)/RHO1)**2)/RHO1*HF(4))
            ELSE
               EXX=-C1*WMU*HF(1)-C1/CDH(NOB)*(((XOB-XSR)/RHO1)**2*HF(2)+  &
                   (1.-2.*((XOB-XSR)/RHO1)**2)/RHO1*HF(4))
               EYX=-C1/CDH(NOB)*(XOB-XSR)*(YOB-YSR)/RHO1**2*              &
                   (HF(2)-2.*HF(4)/RHO1)
               EYY=-C1*WMU*HF(1)-C1/CDH(NOB)*(((YOB-YSR)/RHO1)**2*HF(2)+  &
                   (1.-2.*((YOB-YSR)/RHO1)**2)/RHO1*HF(4))
            END IF
            EXY=EYX
            EZX=-C1/CDH(NOB)*(XOB-XSR)/RHO1*HF(5)
            EZY=-C1/CDH(NOB)*(YOB-YSR)/RHO1*HF(5)
            EXZ=-C1/CDH(NOB)*(XOB-XSR)/RHO1*HF(6)
            EYZ=-C1/CDH(NOB)*(YOB-YSR)/RHO1*HF(6)
!  --- Ezz has different factors before the integral before and
!      after the charge term is removed.  This has been taken into
!      account in routine ONE_D_KERNEL
            EZZ=C1/CDH(NOB)*HF(3)
!
!  --- Compute magnetic fields as well if keyg=2
!
            IF (KEYG==2) THEN
!
               HXX=-C1*(XOB-XSR)*(YOB-YSR)/RHO1**2*(HF(7)-2./RHO1*HF(9))
               HYX= C1*HF(8)+C1*((XOB-XSR)/RHO1)**2*HF(7)                &
                   +C1*(1.-2.*((XOB-XSR)/RHO1)**2)/RHO1*HF(9)
               HXY=-C1*HF(8)-C1*((YOB-YSR)/RHO1)**2*HF(7)                &
                   -C1*(1.-2.*((YOB-YSR)/RHO1)**2)/RHO1*HF(9)
               HYY=-HXX
!
!  --- Corrections for hxz and hyz for sources and receivers in the air.
!      This is necessary for high frequencies (>100 kHz).
!
!      These corrections need be done for all components.  Here only
!      hxz and hyz are done for the time being.
!
!      Corrections are also made for hxz and hyz for sources and
!      receivers on the earth's surface.
!
               IF (ZOB<.0.AND.ZSR<.0.AND.                      &
                 (ABS(ZOB)>=.001.OR.ABS(ZSR)>=.001)) THEN
                  RR0=SQRT(RHO1**2+(ZSR-ZOB)**2)
                  RR1=SQRT(RHO1**2+(ZSR+ZOB)**2)
                  HCRR=RHO1/RR0**3*EXP(-CSQRT(KKH(0))*RR0)    &
                       *((1.,0.)+CSQRT(KKH(0))*RR0)                 &
                       -RHO1/RR1**3*EXP(-CSQRT(KKH(0))*RR1)   &
                       *((1.,0.)+CSQRT(KKH(0))*RR1)
               ELSE IF (ABS(ZOB)<.001.AND.ABS(ZSR)<.001) THEN
                  HCRR=CMPLX(1./RHO1**2,0.)
               ELSE
                  HCRR=(0.,0.)
               END IF
!
               HZX=C1*(YOB-YSR)/RHO1*(HF(10)+HCRR)
               HZY=-C1*(XOB-XSR)/RHO1*(HF(10)+HCRR)
!
               HXZ=-C1*(YOB-YSR)/RHO1*HF(11)
               HYZ=C1*(XOB-XSR)/RHO1*HF(11)
               HZZ=(0.,0.)
!
            END IF
!
!   |                                    |
!   --------------------------------------
         END IF
!
         IF (RHO1<RHOMIN) THEN
!   --------------------------------------
!   |                                    |
!
            DO II=1,NS
               S(II)=GRHO0(II,ISR,IOB)
            END DO
!
            S(1)=-C1/CDH(NOB)*S(1)
            EXX=S(1)
            EYY=S(1)
            EZZ=C1/CDH(NOB)*S(2)
            EYX=(0.,0.)
            EZX=(0.,0.)
            EXY=(0.,0.)
            EZY=(0.,0.)
            EXZ=(0.,0.)
            EYZ=(0.,0.)
!
            IF (KEYG==2) THEN
               HYX=C1*S(3)
               HXY=-C1*S(3)
               HXX=(0.,0.)
               HZX=(0.,0.)
               HYY=(0.,0.)
               HZY=(0.,0.)
               HXZ=(0.,0.)
               HYZ=(0.,0.)
               HZZ=(0.,0.)
            END IF
!
!   |                                    |
!   --------------------------------------
         END IF
!
!   *                                       *
!   *****************************************
      END IF
!
      IF (KEMD==2) THEN
!   *****************************************
!   *                                       *
!
!     Corrections necessary for almost all components!
!     Here only Hzz is corrected for the time being.
!
         IF (RHO1>=RHOMIN) THEN
!   --------------------------------------
!   |                                    |
!
            EXX=C1/CDH(NOB)*(XOB-XSR)*(YOB-YSR)/RHO1**2*(HF(1)-2./RHO1*HF(4))
            EYX=C1*WMU*HF(2)+C1/CDH(NOB)*((YOB-YSR)/RHO1)**2*HF(1)+          &
                C1/CDH(NOB)*((XOB-XSR)**2-(YOB-YSR)**2)/RHO1/RHO1/RHO1*HF(4)
            EZX=C1/CDH(NOB)*(YOB-YSR)/RHO1*HF(5)
            EXY=-C1*WMU*HF(2)-C1/CDH(NOB)*((XOB-XSR)/RHO1)**2*HF(1)-         &
                C1/CDH(NOB)*((YOB-YSR)**2-(XOB-XSR)**2)/RHO1/RHO1/RHO1*HF(4)
            EYY=-EXX
            EZY=-C1/CDH(NOB)*(XOB-XSR)/RHO1*HF(5)
!
            EXZ=C1*WMU*(YOB-YSR)/RHO1*HF(6)
            EYZ=-C1*WMU*(XOB-XSR)/RHO1*HF(6)
            EZZ=(0.,0.)
!
            IF (KEYG==2) THEN
               HXX=C1*(HF(3)                                               &
                 +((YOB-YSR)**2*HF(7)+((XOB-XSR)**2-(YOB-YSR)**2)          &
                 /RHO1*HF(9))/RHO1**2)
               HYX=C1*(XOB-XSR)*(YOB-YSR)/RHO1**2*(-HF(7)+2.*HF(9)/RHO1)
               HZX=C1*(XOB-XSR)/RHO1*HF(10)
               HXY=HYX
               HYY=C1*(HF(3)                                               &
                  +((XOB-XSR)**2*HF(7)+((YOB-YSR)**2-(XOB-XSR)**2)         &
                 /RHO1*HF(9))/RHO1**2)
               HZY=C1*(YOB-YSR)/RHO1*HF(10)
               HXZ=-C1*(XOB-XSR)/RHO1*HF(11)
               HYZ=-C1*(YOB-YSR)/RHO1*HF(11)
               IF (ABS(ZOB-ZSR)<1.E-4) THEN
                  HCRR=-CMPLX(1./RHO1**3,0.)
               ELSE
                  HCRR=(0.,0.)
               END IF
               HZZ=C1*(HF(8)+HCRR)
            END IF
!
!   |                                    |
!   --------------------------------------
         END IF
!
         IF (RHO1<RHOMIN) THEN
!   --------------------------------------
!   |                                    |
            DO II=1,NS
               S(II)=GRHO0(II,ISR,IOB)
            END DO
!
            EYX=C1/CDH(NOB)*S(1)
            EXY=-EYX
            EXX=(0.,0.)
            EZX=(0.,0.)
            EYY=(0.,0.)
            EZY=(0.,0.)
            EXZ=(0.,0.)
            EYZ=(0.,0.)
            EZZ=(0.,0.)
!
            IF (KEYG==2) THEN
               HXX=C1*S(2)
               HYY=HXX
               HZZ=C1*S(3)
               HYX=(0.,0.)
               HZX=(0.,0.)
               HXY=(0.,0.)
               HZY=(0.,0.)
               HXZ=(0.,0.)
               HYZ=(0.,0.)
            END IF
!
!   |                                    |
!   --------------------------------------
         END IF
!
!   *                                       *
!   *****************************************
      END IF
!
! --- Assign the components to the arrays ECOMP and HCOMP
!     for data passing
!
      ECOMP(1)=EXX
      ECOMP(2)=EYX
      ECOMP(3)=EZX
      ECOMP(4)=EXY
      ECOMP(5)=EYY
      ECOMP(6)=EZY
      ECOMP(7)=EXZ
      ECOMP(8)=EYZ
      ECOMP(9)=EZZ
      IF (KEYG==2) THEN
         HCOMP(1)=HXX
         HCOMP(2)=HYX
         HCOMP(3)=HZX
         HCOMP(4)=HXY
         HCOMP(5)=HYY
         HCOMP(6)=HZY
         HCOMP(7)=HXZ
         HCOMP(8)=HYZ
         HCOMP(9)=HZZ
      END IF

   END SUBROUTINE ONE_D_GREEN
!
   REAL FUNCTION RHO(XR,YR,XI,YI)
      IMPLICIT NONE
      REAL XR,YR,XI,YI
      RHO=SQRT((XR-XI)**2+(YR-YI)**2)
   END


   SUBROUTINE ONE_D_HF_TABLE(NHFILM,MLAYER,ZBND,LRYTH,KKH,RMU,  &
                             BLMIN,KEYG,DMIN,RHOMIN,RHOMAX,NZOB,    &
                             ZOBG,NZSR,ZSRG,ZBG,ALMAX,GRHF,RRG,NRG,GRHO0)
!
!****  Compute tabulated Hankel integrals of the Green's functions
!      on a three-demensional grids in the rho (the lateral) direction,
!      z, and z'-directions (or the z and z'-levels).  Note that
!      Hankel integrals for many kernels associated with the components
!      of the tensor Green's functions are computed simultaneously,
!      thus the array GRHF is four-dimensional.  Those tabulated values
!      are interpolated to compute actual Green's functions or field
!      components.
!
!      This routine is similar to THR_D_HF_TABLE but is used for
!      computing electromagnetic fields of controlled
!      sources only,  ie., the 1D responses of electric or
!      magnetic sources.
!
!
!   Input parameters:
!
!     NHFILM: Integer,  maximal number of grids in the lateral direction
!             (rho) for all the Hankel integrals.
!     MLAYER: Integer, number of layers excluding the air.
!     ZBND:   Real ZBND(0:MLAYER), coordinates of the layer
!             boundaries with the air earth interface being zbnd(0)=0.
!     LRYTH:  Real LRYTH(MLAYER),  thickness of each layer (the air is not
!             included in LRYTH).
!     KKH:    Complex(*8) KKH(0:MLAYER),  square ( **2) of the complex wave
!             number of the layers including the air (layer number 0).
!     BLMIN:  Real,  small interval to be cut from a vertical current bipole
!             that passes the centre of a cell.  It is used to avoid
!             singularity.  Note that it has a different function from the
!             BLMIN in routine THR_D_HF_TABLE.
!     KEYG:   Integer.  Mode of computation controlling whether mangetic
!             fields are to be computed.
!             KEYG=1:  Computing electric fields only.  This is used
!                      when elements of the scattering impedance
!                      matrix are computed;  and
!                 =2:  All components of the electric and magnetic fields
!                      are computed as in the computation of secondary
!                      E- and H-fields.
!     DMIN:   Real,  minmum of dimension for numerical treatments of cells.
!             DMIN=0.1 for the low frequency module and DMIN=0.001 for the
!             high frequency module.
!     RHOMIN: Real,  minimal dimension in the lateral direction (rho).
!     RHOMAX: Real,  maxiaml dimension in the lateral direction (rho).
!     NZOB:   Integer, number of z-levels. Determined in the main program.
!     ZOBG:   Real ZOBG(NZOB), the nzob z-levels coordinates.
!     NZSR:   Integer,  number of z'-levels.
!     ZSRG:   Real ZSRG(2,NZSR),  the nzsr z'-levels.
!     ZBG:    Real ZBG(2,NZSR),  working array for the nzsr z'-levels.
!
!   Output parameters:
!
!     NRG:    Integer,  number of grids in the lateral direction (rho).
!     RRG:    Real RRG(NRG),  the NRG grid points in rho.
!     GRHF:   Complex GRHF(11,NHFILM,NZSR,NZOB), tabulated values of
!             the Hankel integrals.  To be passed to the interpolation
!             routines.
!     GRHO0:  Complex GRHO0(4,NZSR,NZOB), tabulated values of the
!             Hankel integrals for rho=0 (source and receiver have the
!             same x and y coordinates).
!
!     Parameter ALMAX is to be passed into routine ONE_D_KERNEL.  See
!     ONE_D_KERNEL for its description.
!
!
!   Remarks:
!
!        The grid points in the rho (or R) direction are set
!        up automatically by the Hankel transform subroutine
!        on a logrithmical scale.   Note that the interpolated
!        values in the rho-direction must be divided by rho
!        to obtain the value of the Hankel transform.
!
!        The grid values of the Hankel transform in the z and z'
!        directions are stored at exactly those z and z'-levels
!        (ZOBG and ZSRG) which are determined by discretization.
!        Hence no interpolations in the z and z'-direction are
!        necessary.
!
!        The Hankel transforms for vertical current bipoles are
!        integrated analytically in the z'-directrion.  Results
!        are stored in the preassigned z'-levels (zsrgrd) with
!        both upper and lower limits for integration. This analytical
!        integration increases the accuracy and efficiency of the
!        algorithm substantially.  But this design restricts that
!        the vertical bipole sources must not be inclined.  Otherwise,
!        some routines must be rewritten.
!
!        The results will be integrated analytically in the routine
!        kernel for vertical bipole sources. This analytical
!        integration increases the accuracy and efficiency
!        substantially. Yet this design restricts that the
!        vertical bipole source must not be inclined. Otherwise,
!        some routines must be rewritten.
!
!        It is assumed that there is only one z'-level for
!        the source current. This is clear for horizontal bipoles
!        or loops. For vertical current bipoles there must be
!        fixed end points.
!
!
!**** Called by:  MAIN_PRM_AT_RCV
!
!**** Calls:      ONE_D_0_2_INFTY,  HFIL
!
!
      IMPLICIT NONE
!
      REAL, INTENT(IN) :: ALMAX,BLMIN,DMIN,RHOMAX,RHOMIN
      INTEGER, INTENT(IN) :: KEYG,MLAYER,NHFILM,NZOB,NZSR
      INTEGER, INTENT(INOUT) :: NRG
      REAL, INTENT(IN) :: ZOBG(NZOB),ZSRG(2,NZSR),                           &
                          LRYTH(MLAYER),ZBND(0:MLAYER),RMU(0:MLAYER)
      REAL, INTENT(INOUT) :: ZBG(2,NZSR),RRG(NHFILM)
      COMPLEX, INTENT(IN) :: KKH(0:MLAYER)
      COMPLEX, INTENT(INOUT) :: GRHF(11,NHFILM,NZSR,NZOB),GRHO0(4,NZSR,NZOB)
      INTEGER :: KITG,KPRM,KCHRG,KEMD,NF,NS,NOB,NSR,I,II,INTER,J,J3,JJ
      COMPLEX :: S(4),S1(4)
!
!  -- HF and HF1 are allocatable working arrays for the kernels of the
!     Hankel transforms.
!
      COMPLEX, ALLOCATABLE :: HF(:,:),HF1(:,:)
!
      ALLOCATE(HF(11,NHFILM),HF1(11,NHFILM))
!
!
!
! --- The parameter KCHRG controls the charge terms in the
!     kernels of Hankel transform.  For loop sources, i.e.,
!     ksrc=0 and 4, the charge terms in the electric fields
!     need be canceled in the program since they cannot be
!     canceled numerically.
!
      KEMD=2
      KCHRG=0
      KITG=0
!
! --- The primary and secondary parts of the Green's functions
!     are computed together.
!
      KPRM=1
!
      IF (KEYG==1) THEN
         NF=6
      ELSE
         NF=11
      END IF
!
      IF (KEYG==2) NS=3
      IF (KEYG==1) THEN
         IF (KEMD==1) THEN
            NS=2
         ELSE
            NS=1
         END IF
      END IF
!
      Receiver_z_level_loop:  DO I=1,NZOB
!
         DO II=MLAYER-1,0,-1
            IF (ZOBG(I)>=ZBND(II)) THEN
               NOB=II+1
               GOTO 50
            END IF
         END DO
         NOB=0
!
      50    CONTINUE
         Source_z_level_loop:  DO J=1,NZSR
!
            DO II=1,NF
               DO J3=1,NHFILM
                  GRHF(II,J3,J,I)=(0.,0.)
               END DO
            END DO
            DO J3=1,NS
               GRHO0(J3,J,I)=(0.,0.)
            END DO

            INTER=1
            ZBG(1,1)=ZSRG(1,J)
            ZBG(2,1)=ZSRG(2,J)

            DO JJ=1,INTER

               DO II=MLAYER-1,0,-1
                  IF ((ZBG(1,JJ)+ZBG(2,JJ))/2.>=ZBND(II)) THEN
                     NSR=II+1
                     GOTO 90
                  END IF
               END DO
               NSR=0
!
! ---- If zobg(i) lies in the integral interval [zbg(1,*),zbg(2,*)]
!      the intergral will be cut into two parts for vertical
!      currents.
!
!      Note that zbg(2,*) > zbg(1,*)
!
! ----- Note the grid points in the Rho-direction are generated
!       by the program HFIL
!
   90          IF (ZOBG(I) > ZBG(2,JJ).OR. ZOBG(I) <ZBG(1,JJ)) THEN
                  CALL HFILL(KEYG,KEMD,MLAYER,ZBND,LRYTH,KKH,RMU,KPRM,   &
                            KITG,KCHRG,NOB,NSR,RHOMIN,RHOMAX,                &
                            ZOBG(I),ZBG(2,JJ),ZBG(1,JJ),NRG,RRG,NHFILM,HF)
               ELSE
                  CALL HFILL(KEYG,KEMD,MLAYER,ZBND,LRYTH,KKH,RMU,KPRM,   &
                            KITG,KCHRG,NOB,NSR,RHOMIN,RHOMAX,                &
                            ZOBG(I),ZBG(2,JJ),ZOBG(I),NRG,RRG,NHFILM,HF)
                  CALL HFILL(KEYG,KEMD,MLAYER,ZBND,LRYTH,KKH,RMU,KPRM,   &
                            KITG,KCHRG,NOB,NSR,RHOMIN,RHOMAX,                &
                            ZOBG(I),ZOBG(I),ZBG(1,JJ),NRG,RRG,NHFILM,HF1)
               END IF
!
               DO II=1,NF
                  DO J3=1,NRG
                     GRHF(II,J3,J,I)=GRHF(II,J3,J,I)+HF(II,J3)
                  END DO
               END DO
!
! --  In order to avoid the case where the transmitter and
!     the receiver coincide with each other which may happen
!     due to discretizations for a body under vertical bipole
!     source excitation, namely, the centre of a cell lies
!     on the source line,  the electrodes are positioned
!     slightly above or below the receiver (cell centre)
!     in such cases.
!
               IF (ABS(ZOBG(I)-ZBG(2,JJ))>=DMIN.OR.            &
                   ABS(ZOBG(I)-ZBG(1,JJ))>=DMIN)     THEN
!
                  IF (ZOBG(I) > ZBG(2,JJ) .OR. ZOBG(I) < ZBG(1,JJ)) THEN
                     CALL ONE_D_0_2_INFTY(KEYG,KEMD,MLAYER,ZBND,LRYTH,KKH,RMU, &
                                      KPRM,KITG,KCHRG,NOB,NSR,ZOBG(I),   &
                                      ZBG(2,JJ),ZBG(1,JJ),ALMAX,NS,S)
                  ELSE IF (ABS (ZOBG(I) - ZBG(1,JJ)) < DMIN) THEN
                     CALL ONE_D_0_2_INFTY(KEYG,KEMD,MLAYER,ZBND,LRYTH,KKH,RMU, &
                                      KPRM,KITG,KCHRG,NOB,NSR,ZOBG(I),   &
                                      ZBG(2,JJ),ZOBG(I)+2.*BLMIN,ALMAX,NS,S)
                     S1=(0.,0.)
                  ELSE IF (ABS (ZOBG(I) - ZBG(2,JJ)) < DMIN) THEN
                     CALL ONE_D_0_2_INFTY(KEYG,KEMD,MLAYER,ZBND,LRYTH,KKH,RMU, &
                                      KPRM,KITG,KCHRG,NOB,NSR,ZOBG(I),   &
                                      ZOBG(I)-2.*BLMIN,ZBG(1,JJ),ALMAX,NS,S1)
                     S=(0.,0.)
                  ELSE
                     CALL ONE_D_0_2_INFTY(KEYG,KEMD,MLAYER,ZBND,LRYTH,KKH,RMU, &
                                      KPRM,KITG,KCHRG,NOB,NSR,ZOBG(I),   &
                                      ZBG(2,JJ),ZOBG(I)+2.*BLMIN,ALMAX,NS,S)
                     CALL ONE_D_0_2_INFTY(KEYG,KEMD,MLAYER,ZBND,LRYTH,KKH,RMU, &
                                      KPRM,KITG,KCHRG,NOB,NSR,ZOBG(I),   &
                                      ZOBG(I)-2.*BLMIN,ZBG(1,JJ),ALMAX,NS,S1)
                  END IF
!
                  DO J3=1,NS
                     IF (ZOBG(I) > ZBG(2,JJ) .OR. ZOBG(I) < ZBG(1,JJ)) THEN
                        GRHO0(J3,J,I)=GRHO0(J3,J,I)+S(J3)
                     ELSE
                        GRHO0(J3,J,I)=GRHO0(J3,J,I)+S(J3)+S1(J3)
                     END IF
!
                  END DO
               END IF
!
            END DO
!
         END DO Source_z_level_loop
!
      END DO Receiver_z_level_loop
!
      DO J3=1,NRG
         RRG(J3)=ALOG(RRG(J3))
      END DO
!
      DEALLOCATE(HF,HF1)
!
      RETURN
!
   END SUBROUTINE ONE_D_HF_TABLE
!
!
      SUBROUTINE INTERPO_LG(GRHF,A,L,NHFILM,NZSR,NZOB,X,IOB,ISR,F,NF)
!
!**** INTERPOLATION OF THE HANKEL INTEGRALS
!
!    One-dimensional three-point Lagrangian interpolations
!    in the Rho-direction.
!
! Input parameters:
!
!   GRHF:   Complex GRHF(11,NHFILM,NZSR,NZOB), tabulated values of
!           the Hankel integrals.
!   L:      Integer,  number of grid points.
!   A:      Real A(L),  array for grid points.
!   NHFILM: Integer,  maximal number of grids in the lateral direction
!           (rho) for all the Hankel integrals.
!   X:      Real,  interpolation point.
!   IOB:    Integer,  number for receiver z-level.  It is used to
!           specify the third array element of GRHF.
!   ISR:    Integer,  number for receiver z'-level.  It is used to
!           specify the fourth array element of GRHF.
!   NF:     Integer,  number of functions to be interpolated.
!
! Output parameter:
!
!   F:      Complex F(NF),  interpolated NF function values.
!
!
!    This routine is similar to the routine intpl4 a
!    three-point rule is used.
!
!
!**** Called by:  THR_D_GREEN
!
!**** Calls:      none
!
!
      IMPLICIT NONE
!
      REAL A1,A2,A3,X
      INTEGER I,I1,II,IOB,ISR,L,L1,NF,NHFILM,NN,NZSR,NZOB
      REAL A(L),U(3)
      COMPLEX GRHF(11,NHFILM,NZSR,NZOB),F(NF)
!
! --- Sorting
!
      L1=L-2
      DO I=1,L1
         IF (X<=A(I+1)) THEN
            GOTO 100
         END IF
      END DO
      I=L-2
  100 IF (I/=1.AND.X-A(I)<A(I+1)-X) I=I-1
!
! --- Compute interpolant coefficients
!
      A1=A(I)
      A2=A(I+1)
      A3=A(I+2)
      U(1)=(X-A2)*(X-A3)/((A1-A2)*(A1-A3))
      U(2)=(X-A1)*(X-A3)/((A2-A1)*(A2-A3))
      U(3)=(X-A1)*(X-A2)/((A3-A1)*(A3-A2))
!
      DO NN=1,NF
         F(NN)=(.0,.0)
      END DO
!
      DO II=1,3
         I1=I+II-1
!
         DO NN=1,NF
            F(NN)=F(NN)+U(II)*GRHF(NN,I1,ISR,IOB)
         END DO
      END DO
      RETURN
      END
!
!**** End of INTERPO_LG
!
!
      SUBROUTINE INTERPO_LG1(GRHF3,A,L,NHFILM,NZSR,X,ISR,F,NF)
!
!****   INTERPOLATION OF THE GREEN'S FUNCTIONS FOR
!     OUTCROPPING CELLS.
!
!    One-dimensional three-point Lagrangian interpolations
!    in the Rho-direction.
!
! Input parameters:
!
!   GRHF3:  Complex GRHF3(11,NHFILM,NZSR), tabulated values of
!           the Hankel integrals for an outcropping cell.
!   L:      Integer,  number of grid points.
!   A:      Real A(L),  array for grid points.
!   NHFILM: Integer,  maximal number of grids in the lateral direction
!           (rho) for all the Hankel integrals.
!   X:      Real,  interpolation point.
!   ISR:    Integer,  number for receiver z'-level.  It is used to
!           specify the fourth array element of GRHF.
!   NF:     Integer,  number of functions to be interpolated.
!
! Output parameter:
!
!   F:      Complex F(NF),  interpolated NF function values.
!
!
!    This routine is similar to the routine intpl4 except that a
!    five-point rule is used and that there is no z-level.
!
!
!**** Called by:  THR_D_GREEN
!
!**** Calls:      none
!
!
      IMPLICIT NONE
!
      REAL A1,A2,A3,X
      INTEGER I,I1,II,ISR,L,L1,NF,NHFILM,NN,NZSR
      REAL A(L),U(3)
      COMPLEX GRHF3(11,NHFILM,NZSR),F(NF)
!
! --- Sorting
!
      L1=L-2
      DO I=1,L1
         IF (X<=A(I+1)) THEN
            GOTO 100
         END IF
      END DO
      I=L-2
  100 IF (I/=1.AND.X-A(I)<A(I+1)-X) I=I-1
!
! --- Computing interpolant coefficients
!
      A1=A(I)
      A2=A(I+1)
      A3=A(I+2)
      U(1)=(X-A2)*(X-A3)/((A1-A2)*(A1-A3))
      U(2)=(X-A1)*(X-A3)/((A2-A1)*(A2-A3))
      U(3)=(X-A1)*(X-A2)/((A3-A1)*(A3-A2))
!
      DO NN=1,NF
         F(NN)=(.0,.0)
      END DO
!
      DO II=1,3
         I1=I+II-1
         DO NN=1,NF
            F(NN)=F(NN)+U(II)*GRHF3(NN,I1,ISR)
         END DO
      END DO
!
      RETURN
      END
!
!**** End of INTERPO_LG1
!
!
      SUBROUTINE INTERPO_LG4(GRHF,A,L,NHFILM,NZSR,NZOB,X,IOB,ISR,F,NF)
!
!**** INTERPOLATION OF THE HANKEL INTEGRALS
!
!    Five-point Lagrangian interpolations
!    in the Rho-direction.
!    Note the abscissas are equally spaced
!
! Input parameters:
!
!   GRHF:   Complex GRHF(11,NHFILM,NZSR,NZOB), tabulated values of
!           the Hankel integrals.
!   L:      Integer,  number of grid points.
!   A:      Real A(L),  array for grid points.
!   NHFILM: Integer,  maximal number of grids in the lateral direction
!           (rho) for all the Hankel integrals.
!   X:      Real,  interpolation point.
!   IOB:    Integer,  number for receiver z-level.  It is used to
!           specify the third array element of GRHF.
!   ISR:    Integer,  number for receiver z'-level.  It is used to
!           specify the fourth array element of GRHF.
!   NF:     Integer,  number of functions to be interpolated.
!
! Output parameter:
!
!   F:      Complex F(NF),  interpolated NF function values.
!
!
!    This routine is similar to the routine intpl15 a
!    five-point rule is used.
!
!
!**** Called by:  THR_D_GREEN
!
!**** Calls:      none
!
!
      IMPLICIT NONE
!
      INTEGER I,I1,ICN2,II,IOB,ISR,L,NF,NHFILM,NN,NZSR,NZOB
      REAL P,P1,P2,PP,X
      REAL A(L),U(5)
      COMPLEX GRHF(11,NHFILM,NZSR,NZOB),F(NF)
!
! --- Sorting
!
      IF (X<=A(1)) THEN
         I=1
      ELSE IF (X>=A(L)) THEN
         I=L
      ELSE
         DO I=1,L-1
            IF (X>=A(I).AND.X<=A(I+1)) THEN
               EXIT
            END IF
         END DO
      END IF
!
      IF (I<=3) THEN
         ICN2=1
      ELSE IF (I>=L-2) THEN
         ICN2=L-4
      ELSE
         ICN2=I-2
      END IF
!
! --- Compute interpolant coefficients
!
      P=(X-A(ICN2+2))/(A(2)-A(1))
!
      PP=P*P
      P1=(PP-1.)*P/24.
      P2=P*(PP-4.)/6.
      U(1)=P1*(P-2.)
      U(2)=-(P-1.)*P2
      U(3)=(PP-1.)*(PP-4.)/4.
      U(4)=-(P+1.)*P2
      U(5)=P1*(P+2.)
!
      DO NN=1,NF
         F(NN)=(.0,.0)
      END DO
!
      DO II=1,5
         I1=ICN2+II-1
         DO NN=1,NF
            F(NN)=F(NN)+U(II)*GRHF(NN,I1,ISR,IOB)
         END DO
      END DO
!
      RETURN
      END
!
!**** End of INTERPO_LG4
!
!
      SUBROUTINE INTERPO_LG5(GRHF3,A,L,NHFILM,NZSR,X,ISR,F,NF)
!
!****   INTERPOLATION OF THE HANKEL INTEGRALS FOR
!     OUTCROPPING CELLS.
!
!    Five-point Lagrangian interpolations
!    in the Rho-direction.
!    Note the abscissas are equally spaced
!
!
! Input parameters:
!
!   GRHF3:  Complex GRHF3(11,NHFILM,NZSR), tabulated values of
!           the Hankel integrals for an outcropping cell.
!   L:      Integer,  number of grid points.
!   A:      Real A(L),  array for grid points.
!   NHFILM: Integer,  maximal number of grids in the lateral direction
!           (rho) for all the Hankel integrals.
!   X:      Real,  interpolation point.
!   ISR:    Integer,  number for receiver z'-level.  It is used to
!           specify the fourth array element of GRHF.
!   NF:     Integer,  number of functions to be interpolated.
!
! Output parameter:
!
!   F:      Complex F(NF),  interpolated NF function values.
!
!
!    This routine is similar to the routine intpl except that a
!    five-point rule is used.
!
!
!**** Called by:  THR_D_GREEN
!
!**** Calls:      none
!
!
      IMPLICIT NONE
!
      INTEGER I,I1,ICN2,II,ISR,L,NF,NHFILM,NN,NZSR
      REAL P,P1,P2,PP,X
      REAL A(L),U(5)
      COMPLEX GRHF3(11,NHFILM,NZSR),F(NF)
!
      IF (X<=A(1)) THEN
         I=1
         GOTO 100
      END IF
      IF (X>=A(L)) THEN
         I=L
         GOTO 100
      END IF
      DO I=1,L-1
         IF (X>=A(I).AND.X<=A(I+1)) THEN
            EXIT
         END IF
      END DO
!
  100 IF (I<=3) THEN
         ICN2=1
      ELSE IF (I>=L-2) THEN
         ICN2=L-4
      ELSE
         ICN2=I-2
      END IF
      P=(X-A(ICN2+2))/(A(2)-A(1))
!
      PP=P*P
      P1=(PP-1.)*P/24.
      P2=P*(PP-4.)/6.
      U(1)=P1*(P-2.)
      U(2)=-(P-1.)*P2
      U(3)=(PP-1.)*(PP-4.)/4.
      U(4)=-(P+1.)*P2
      U(5)=P1*(P+2.)
!
      DO NN=1,NF
         F(NN)=(.0,.0)
      END DO
!
      DO II=1,5
         I1=ICN2+II-1
         DO NN=1,NF
            F(NN)=F(NN)+U(II)*GRHF3(NN,I1,ISR)
         END DO
      END DO
!
      RETURN
      END
!
!**** End of INTERPO_LG5
!
!
      SUBROUTINE INTERPO_LG6(GRHF,A,L,NHFILM,NZSR,NZOB,X,IOB,ISR,F,NF)
!
!****    INTERPOLATION OF THE HANKEL INTEGRALS
!
!    Six-point Lagrangian interpolations
!    in the Rho-direction.
!    Note the abscissas are equally spaced
!
! Input parameters:
!
!   GRHF:   Complex GRHF(11,NHFILM,NZSR,NZOB), tabulated values of
!           the Hankel integrals.
!   L:      Integer,  number of grid points.
!   A:      Real A(L),  array for grid points.
!   NHFILM: Integer,  maximal number of grids in the lateral direction
!           (rho) for all the Hankel integrals.
!   X:      Real,  interpolation point.
!   IOB:    Integer,  number for receiver z-level.  It is used to
!           specify the third array element of GRHF.
!   ISR:    Integer,  number for receiver z'-level.  It is used to
!           specify the fourth array element of GRHF.
!   NF:     Integer,  number of functions to be interpolated.
!
! Output parameter:
!
!   F:      Complex F(NF),  interpolated NF function values.
!
!
!    This routine is similar to the routine intpl4 a
!    six-point rule is used.
!
!
!**** Called by:  ONE_D_GREEN
!
!**** Calls:      none
!
!
      IMPLICIT NONE
!
      INTEGER I,I1,ICN2,II,IOB,ISR,L,NF,NHFILM,NN,NZSR,NZOB
      REAL P,P1,P2,P3,PP1,PP4,X
      REAL A(L),U(6)
      COMPLEX GRHF(11,NHFILM,NZSR,NZOB),F(NF)
!
! --- Sorting
!
      IF (X<=A(1)) THEN
         I=1
      ELSE IF (X>=A(L)) THEN
         I=L
      ELSE
         DO I=1,L-1
            IF (X>=A(I).AND.X<=A(I+1)) THEN
               EXIT
            END IF
         END DO
      END IF
!
      IF (I<=3) THEN
         ICN2=1
      ELSE IF (I>=L-3) THEN
         ICN2=L-5
      ELSE
         ICN2=I-2
      END IF
!
! --- Compute interpolant coefficients
!
      P=(X-A(ICN2+2))/(A(2)-A(1))
!
      PP1=(P-1.)*(P+1.)
      PP4=(P-2.)*(P+2.)
      P1=PP1*P/120.
      P2=P*(P-3.)/24.
      P3=(P-3.)*PP4/12.
      U(1)=-P1*(P-2.)*(P-3.)
      U(2)=P2*(P-1.)*PP4
      U(3)=-P3*PP1
      U(4)=P3*P*(P+1.)
      U(5)=-P2*PP1*(P+2.)
      U(6)=P1*PP4
!
      DO NN=1,NF
         F(NN)=(.0,.0)
      END DO
!
      DO II=1,6
         I1=ICN2+II-1
         DO NN=1,NF
            F(NN)=F(NN)+U(II)*GRHF(NN,I1,ISR,IOB)
         END DO
      END DO
!
      RETURN
      END
!
!**** End of INTERPO_LG6
!
!
   SUBROUTINE INTERPO_PLPOL(GRHF,A,L,NHFILM,NZSR,NZOB,X,IOB,ISR,F,NF)
!
!****    INTERPOLATION OF THE HANKEL TRANSFORMS
!
!      Eight-point polynomial or rational interpolations
!      in the Rho-direction.
!
!
!   Input parameters:
!
!     GRHF:   Complex GRHF(11,NHFILM,NZSR,NZOB), tabulated values of
!             the Hankel integrals.
!     L:      Integer,  number of grid points.
!     A:      Real A(L),  array for grid points.
!     NHFILM: Integer,  maximal number of grids in the lateral direction
!             (rho) for all the Hankel integrals.
!     X:      Real,  interpolation point.
!     IOB:    Integer,  number for receiver z-level.  It is used to
!             specify the third array element of GRHF.
!     ISR:    Integer,  number for receiver z'-level.  It is used to
!             specify the fourth array element of GRHF.
!     NF:     Integer,  number of functions to be interpolated.
!
!   Output parameter:
!
!     F:      Complex F(NF),  interpolated NF function values.
!
!
!      This routine takes much more computation time than the
!      Lagrangian interpolations because the interpolation is
!      done recursively for each component of the array F.
!
!
!**** Called by:  ONE_D_GREEN
!
!**** Calls:      none
!
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: IOB,ISR,L,NF,NHFILM,NZSR,NZOB
      REAL, INTENT(IN) :: A(L)
      COMPLEX, INTENT(OUT) :: F(NF)
!
      INTEGER :: I,I1,ICN2,II,NN
      REAL :: X
      INTEGER, PARAMETER :: NINT=8, NINTM=10
      COMPLEX :: DY,YA(NINTM)
!
      REAL :: XA(NINT)
      COMPLEX, INTENT(IN) :: GRHF(11,NHFILM,NZSR,NZOB)
!
      IF (X<=A(1)) THEN
         I=1
      ELSE IF (X>=A(L)) THEN
         I=L
      ELSE
         DO I=1,L-1
            IF (X>=A(I).AND.X<=A(I+1)) THEN
               EXIT
            END IF
         END DO
      END IF
!
      IF (I<=4) THEN
         ICN2=1
      ELSE IF (I>=L-4) THEN
         ICN2=L-7
      ELSE
         ICN2=I-3
      END IF
!
      DO II=1,NINT
         I1=ICN2+II-1
         XA(II)=A(I1)
      END DO
!
! --- May need to use a two dimensional array for YA
!     in order to save time.
!
      DO NN=1,NF
!
         DO II=1,NINT
            I1=ICN2+II-1
            YA(II)=GRHF(NN,I1,ISR,IOB)
         END DO
!
         CALL INTERPO_POLINT(XA,YA,NINT,X,F(NN),DY)
!
      END DO
!
      RETURN
!
   END SUBROUTINE INTERPO_PLPOL
!
!
      SUBROUTINE INTERPO_POLINT(XA,YA,N,X,Y,DY)
!
!**** Polynomial interpolation
!
!   modified from Press et al., Numerical recipes, p.82
!
!
      IMPLICIT NONE
!
      REAL DIF,DIFT,HO,HP,TINY,X
      INTEGER I,M,N,NMAX,NS
!
      PARAMETER (NMAX=10,TINY=1.E-30)
      REAL XA(N)
      COMPLEX YA(N),Y,DY,C(NMAX),D(NMAX),W,DEN
      NS=1
      DIF=ABS(X-XA(1))
      DO I=1,N
         DIFT=ABS(X-XA(I))
         IF (DIFT<DIF) THEN
            NS=I
            DIF=DIFT
         END IF
         C(I)=YA(I)
         D(I)=YA(I)
      END DO
      Y=YA(NS)
      NS=NS-1
      DO M=1,N-1
         DO I=1,N-M
            HO=XA(I)-X
            HP=XA(I+M)-X
            W=C(I+1)-D(I)
            DEN=CMPLX(HO-HP,0.)
            IF (ABS(DEN)<TINY) THEN
               STOP 'Error in routine INTERPO_POLINT: two identical XA!'
            END IF
            DEN=W/DEN
            D(I)=HP*DEN
            C(I)=HO*DEN
         END DO
         IF (2*NS<N-M) THEN
            DY=C(NS+1)
         ELSE
            DY=D(NS)
            NS=NS-1
         END IF
         Y=Y+DY
      END DO
      RETURN
      END
!
!
   SUBROUTINE THR_D_HF_TABLE(NHFILM,MLAYER,ZBND,LRYTH,KKH,RMU,          &
                             KSFT,BLMIN,KEYG,DMIN,RHOMIN,RHOMAX,NZOB,ZOBG,  &
                             NZSR,ZSRG,ALMAX,RRG,NRG,RRG3,NRG3,GRHF,GRHF3,  &
                             GRHO0,GRHO03)
!
!****  Compute tabulated Hankel integrals of the Green's functions
!    on a three-demensional grids in the rho (the lateral) direction,
!    z, and z'-directions (or the z and z'-levels).  Note that
!    Hankel integrals for many kernels associated with the components
!    of the tensor Green's functions are computed simultaneously,
!    thus the array GRHF is four-dimensional.  Those tabulated values
!    are interpolated to compute actual Green's functions or field
!    components.
!
!
! Input parameters:
!
!   NHFILM: Integer,  maximal number of grids in the lateral direction
!           (rho) for all the Hankel integrals.
!   MLAYER: Integer, number of layers excluding the air.
!   ZBND:   Real ZBND(0:MLAYER), coordinates of the layer
!           boundaries with the air earth interface being zbnd(0)=0.
!   LRYTH:  Real LRYTH(MLAYER),  thickness of each layer (the air is not
!           included in LRYTH).
!   KKH:    Complex(*8) KKH(0:MLAYER),  square ( **2) of the complex wave
!           number of the layers including the air (layer number 0).
!   KSFT:   Integer,  parameter controlling whethter there is an outcropping
!           cell.  KSFT=1: yes; and KSFT=0:, no.
!   BLMIN:  Real,  radius and height of a cylinder to be removed
!           from computation for the treatment of singularity
!           arising in the computation of magnetic fields due to
!           an outcropping cell.  See description for KUTCRP
!           in routine THR_D_GREEN for more details.
!   KEYG:   Integer.  Mode of computation controlling whether mangetic
!           fields are to be computed.
!           KEYG=1:  Computing electric fields only.  This is used
!                    when elements of the scattering impedance
!                    matrix are computed;  and
!               =2:  All components of the electric and magnetic fields
!                    are computed as in the computation of secondary
!                    E- and H-fields.
!   DMIN:   Real,  minmum of dimension for numerical treatments of cells.
!           DMIN=0.1 for the low frequency module and DMIN=0.001 for the
!           high frequency module.
!   RHOMIN: Real,  minimal dimension in the lateral direction (rho).
!   RHOMAX: Real,  maxiaml dimension in the lateral direction (rho).
!   NZOB:   Integer, number of z-levels. Determined in the main program.
!   ZOBG:   Real ZOBG(NZOB), the nzob z-levels coordinates.
!   NZSR:   Integer,  number of z'-levels.
!   ZSRG:   Real ZSRG(NZSR),  the nzsr z'-levels.
!
!   Parameter ALMAX is to be passed into routine ONE_D_KERNEL.  See
!   ONE_D_KERNEL for its description.
!
! Output parameters:
!
!   NRG:    Integer,  number of grids in the lateral direction (rho).
!   RRG:    Real RRG(NRG),  the NRG grid points in rho.
!   NRG3:   Integer,  number of grids in the lateral direction (rho) for
!           the Hankel integrals for outcropping cells, namely, for the
!           array GRHF3 (see following description).
!   RRG3:   Real RRG3(NRG3),  the NRG3 grid points in rho for GRHF3.
!   GRHF:   Complex GRHF(11,NHFILM,NZSR,NZOB), tabulated values of
!           the Hankel integrals.  To be passed to the interpolation
!           routines.
!   GRHF3:  Complex GRHF3(11,NHFILM,NZSR), tabulated values of
!           the Hankel integrals for outcropping cells.  To be passed
!           to the interpolation routines.  It is integrated from blmin
!           to zsrg(2,*) for receivers at the earth's surface.
!   GRHO0:  Complex GRHO0(4,NZSR), tabulated values of the
!           Hankel integrals for rho=0 (source and receiver have the
!           same x and y coordinates).
!   GRHO03:  Complex GRHO03(4,NZSR,NZOB), tabulated values of the
!           Hankel integrals for outcropping cells for rho=0 (source
!           and receiver have the same x and y coordinates).
!
!
! Remarks:
!
!      The grid points in the rho (or R) direction are set
!      up automatically by the Hankel transform subroutine
!      on a logrithmical scale.   Note that the interpolated
!      values in the rho-direction must be divided by rho
!      to obtain the value of the Hankel transform.
!
!      The grid values of the Hankel transform in the z and z'
!      directions are stored at exactly those z and z'-levels
!      (ZOBG and ZSRG) which are determined by discretization.
!      Hence no interpolations in the z and z'-direction are
!      necessary.
!
!      All the Hankel transforms are integrated analytically
!      in the z'-directrion. Results are stored in the preassigned
!      z'-levels (ZSRG) with both upper and lower limits for
!      integration.
!
!      If KSFT=1 and KEYG=2, the array grhf3 and grho3 will
!      be compute. These two arrays are used to deal with the
!      singularities of the secondary magnetic fields at the
!      earth's surface (z=z'=0).
!
!
!**** Called by:  main
!
!**** Calls:      ONE_D_0_2_INFTY,  HFILL
!
!
      IMPLICIT NONE
!
      REAL, INTENT(IN) :: ALMAX,BLMIN,DMIN,RHOMAX,RHOMIN
      INTEGER, INTENT(IN) :: KEYG,KSFT,MLAYER,NHFILM,NZOB,NZSR
      INTEGER, INTENT(INOUT) :: NRG,NRG3
      REAL, INTENT(IN) :: ZOBG(NZOB),ZSRG(2,NZSR),                            &
                          LRYTH(MLAYER),ZBND(0:MLAYER),RMU(0:MLAYER)
      REAL, INTENT(INOUT) :: RRG(NHFILM),RRG3(NHFILM)
      COMPLEX, INTENT(IN) :: KKH(0:MLAYER)
      COMPLEX, INTENT(INOUT) :: GRHF(11,NHFILM,NZSR,NZOB),GRHO0(4,NZSR,NZOB), &
                                GRHF3(11,NHFILM,NZSR),GRHO03(4,NZSR)
      INTEGER :: KITG,KPRM,KCHRG,KEMD,NF,NOB,NSR,I,II,J,JJ
      REAL    :: ZSR1
      COMPLEX :: S(4),S1(4)
!
!  -- HF and HF1 are allocatable working arrays for the kernels of the
!     Hankel transforms.
!
      COMPLEX, ALLOCATABLE :: HF(:,:),HF1(:,:)
!
      ALLOCATE(HF(11,NHFILM),HF1(11,NHFILM))
!
! --- KEMD control if electric or magnetic dipoles are to be
!   computed.  Here for the Green's tensors within the body,
!   only electric dipoles are considered.
!
      KEMD=1
!
! --- The parameter KCHRG controls the charge terms in the
!   kernels of Hankel transform.
!
      KCHRG=0
!
      KITG=1
!
      IF (KEYG==1) THEN
         NF=6
      ELSE
         NF=11
      END IF
!
      DO I=1,NZOB
!
         DO II=MLAYER-1,0,-1
            IF (ZOBG(I)>=ZBND(II)) THEN
               NOB=II+1
               GOTO 50
            END IF
         END DO
         NOB=0
!
   50    DO J=1,NZSR
!
            DO II=MLAYER-1,0,-1
               IF ((ZSRG(1,J)+ZSRG(2,J))/2.>=ZBND(II)) THEN
                  NSR=II+1
                  GOTO 60
               END IF
            END DO
            NSR=0


   60       IF (KEYG==1.AND.NOB==NSR) THEN
               KPRM=0
            ELSE
               KPRM=1
            END IF
!
! ---- If zobg(i) lies in the integral interval (zsrg(1,j)-zsrg(2,j)),
!    the intergral will be cut into two parts.
!
!    Note that zsrg(2,j) > zsrg(1,j).
!
! ----- Note the grid points in the Rho-direction are generated
!     by the program HFILL
!
            IF (ZOBG(I)>ZSRG(2,J).OR.ZOBG(I)<ZSRG(1,J)) THEN
               CALL HFILL(KEYG,KEMD,MLAYER,ZBND,LRYTH,KKH,RMU,KPRM,       &
                         KITG,KCHRG,NOB,NSR,RHOMIN,RHOMAX,ZOBG(I),            &
                         ZSRG(2,J),ZSRG(1,J),NRG,RRG,NHFILM,HF)
            ELSE
               CALL HFILL(KEYG,KEMD,MLAYER,ZBND,LRYTH,KKH,RMU,KPRM,KITG,  &
                         KCHRG,NOB,NSR,RHOMIN,RHOMAX,ZOBG(I),ZSRG(2,J),       &
                         ZOBG(I),NRG,RRG,NHFILM,HF)
               CALL HFILL(KEYG,KEMD,MLAYER,ZBND,LRYTH,KKH,RMU,KPRM,KITG,  &
                         KCHRG,NOB,NSR,RHOMIN,RHOMAX,ZOBG(I),ZOBG(I),         &
                         ZSRG(1,J),NRG,RRG,NHFILM,HF1)
            END IF
!
            IF (ZOBG(I)>=ZSRG(1,J).AND.ZOBG(I)<=ZSRG(2,J)) THEN
!
!---- This may also happen for keyg=2 for cells just below
!     the earth's surface
               DO JJ=1,NRG
                  DO II=1,NF
                     GRHF(II,JJ,J,I)=HF(II,JJ)+HF1(II,JJ)
                  END DO
               END DO
            ELSE
               DO JJ=1,NRG
                  DO II=1,NF
                     GRHF(II,JJ,J,I)=HF(II,JJ)
                  END DO
               END DO
            END IF
!
! ----- To avoid singularity in case zobg(i) and zsrg(1,j) = 0
!     the lower limit for integration in the z'-direction is
!     increased from 0 to blmin/2.
!     Note that for outcropping structures the electric fields
!     on the top of blocks at the earth's surface are determined
!     from the scattering currents directly.
!
!     Note that the two elements for self-cells are not
!     computed.
!
            IF (KEYG/=2.OR.ABS(ZOBG(I))>=DMIN.OR.ABS(ZSRG(1,J))>=DMIN)  THEN
!----------------------------------------------------
!|                                                  |
! -- Outcroppers are computed later
!
               IF (ZOBG(I)>ZSRG(2,J).OR.ZOBG(I)<ZSRG(1,J)) THEN
                  CALL ONE_D_0_2_INFTY(KEYG,KEMD,MLAYER,ZBND,LRYTH,KKH,RMU, &
                                   KPRM,KITG,KCHRG,NOB,NSR,ZOBG(I),   &
                                   ZSRG(2,J),ZSRG(1,J),ALMAX,KEYG+1,S)
!
! -- The following two blocks are used to avoid singularity
!    if the receivers are located on or near a layer boundary
!    within the 3D structure, or in the case when the receiver
!    is located on the top or bottom of a cell and yet in a
!    different layer as the cell
!
               ELSE IF (ABS(ZOBG(I)-ZSRG(2,J))<DMIN.AND.NOB/=NSR) THEN
                  CALL ONE_D_0_2_INFTY(KEYG,KEMD,MLAYER,ZBND,LRYTH,KKH,RMU,  &
                                   KPRM,KITG,KCHRG,NOB,NSR,ZOBG(I),    &
                                   ZSRG(2,J)-BLMIN,ZSRG(1,J),ALMAX,KEYG+1,S1)
                  DO JJ=1,KEYG+1
                     S(JJ)=(0.,0.)
                  END DO
               ELSE IF (ABS(ZOBG(I)-ZSRG(1,J))<DMIN.AND.NOB/=NSR) THEN
                  CALL ONE_D_0_2_INFTY(KEYG,KEMD,MLAYER,ZBND,LRYTH,KKH,RMU,  &
                                   KPRM,KITG,KCHRG,NOB,NSR,ZOBG(I),    &
                                   ZSRG(2,J),ZSRG(1,J)+BLMIN,ALMAX,KEYG+1,S)
                  DO JJ=1,KEYG+1
                     S1(JJ)=(0.,0.)
                  END DO
               ELSE
                  CALL ONE_D_0_2_INFTY(KEYG,KEMD,MLAYER,ZBND,LRYTH,KKH,RMU,  &
                                   KPRM,KITG,KCHRG,NOB,NSR,ZOBG(I),    &
                                   ZSRG(2,J),ZOBG(I),ALMAX,KEYG+1,S)
                  CALL ONE_D_0_2_INFTY(KEYG,KEMD,MLAYER,ZBND,LRYTH,KKH,RMU,  &
                                   KPRM,KITG,KCHRG,NOB,NSR,ZOBG(I),    &
                                   ZOBG(I),ZSRG(1,J),ALMAX,KEYG+1,S1)
               END IF
!
            END IF
!|                                                  |
!----------------------------------------------------
!
            DO JJ=1,KEYG+1
               IF (ZOBG(I)>ZSRG(2,J).OR.ZOBG(I)<ZSRG(1,J)) THEN
                  GRHO0(JJ,J,I)=S(JJ)
               ELSE
                  GRHO0(JJ,J,I)=S(JJ)+S1(JJ)
               END IF
            END DO
!
         END DO
      END DO
!
      DO JJ=1,NRG
         RRG(JJ)=ALOG(RRG(JJ))
      END DO
!
      IF (KEYG==2.AND.KSFT==1) THEN
!**************************************************
!*                                                *
!
! ---- For outcropping structures a cylindrical volume with radius
!    of blmin and height of blmin near the observer will be
!    removed in the integration of the magnetic Green's functions.
!
!   Note here the receiver and source are in the same layer.
!   The surface of the earth is always considered to be layer # 1
!   which is the layer of the outcropping cells. Notice the .ge.
!   in the lines determining layer sequences throught the program.
!
!   kprm changed to be 1.   See the comments at the beginning of
!   the routine THR_D_GREEN
!
!    KPRM=0
!
         KPRM=1
!
! --- The array grhf3 adn grho3 are formed for every z'-level,
!   but ... (see below)
!
         DO I=1,NZSR
!
            ZSR1=(ZSRG(1,I)+ZSRG(2,I))/2.
!
            DO II=MLAYER-1,0,-1
               IF (ZSR1>=ZBND(II)) THEN
                  NSR=II+1
                  GOTO 80
               END IF
            END DO
            NSR=0
   80       NOB=NSR
!
! --- Compute GRHF3 for outcropping cells.
!
            IF (ZSRG(1,I)<=.1)                                         &
               CALL HFILL(KEYG,KEMD,MLAYER,ZBND,LRYTH,KKH,RMU,   &
                         KPRM,KITG,KCHRG,NOB,NSR,.1,BLMIN,0.,         &
                         ZSRG(2,I),BLMIN,NRG3,RRG3,NHFILM,HF)
!
            DO II=1,NF
               DO JJ=1,NRG3
                  GRHF3(II,JJ,I)=HF(II,JJ)
               END DO
            END DO
!
            IF (ZSRG(1,I)<=.1) THEN
               CALL ONE_D_0_2_INFTY(KEYG,KEMD,MLAYER,ZBND,LRYTH,   &
                                    KKH,RMU,KPRM,KITG,KCHRG,NOB,NSR, &
                                    0.,ZSRG(2,I),BLMIN,ALMAX,3,S)
               GRHO03(1,I)=S(1)
               GRHO03(2,I)=S(2)
               GRHO03(4,I)=S(3)
            END IF
!
         END DO
!
         DO JJ=1,NRG3
            RRG3(JJ)=ALOG(RRG3(JJ))
         END DO
!
!*                                                *
!**************************************************
      END IF
!
      DEALLOCATE(HF,HF1)
!
   END SUBROUTINE THR_D_HF_TABLE
!
!
      SUBROUTINE THR_D_GREEN(KEYG,PSTION,FRQ,MLAYER,ZBND,CDH,RMU,NZOB,ZOBG,  &
                             NZSR,ZSRG,RHOMIN,DMIN,NHFILM,RRG,NRG,GRHF,RRG3,     &
                             NRG3,GRHF3,GRHO0,GRHO03,CLX,CLY,CLZ,CLMN,KCLMN,     &
                             BLMIN,KACC,KUTCRP,KSELF,ECOMP,HCOMP)
!
!****   Integration of the tensor green's functions over a
!     prismatic cell
!
!
! Input parameters:
!
!   KEYG:   Integer.  Mode of computation controlling whether mangetic
!           fields are to be computed.
!           KEYG=1:  Computing electric fields only.  This is used
!                    when elements of the scattering impedance
!                    matrix are computed;  and
!               =2:  All components of the electric and magnetic fields
!                    are computed as in the computation of secondary
!                    E- and H-fields.
!   PSTION: Real PSTION(8),  the positions of the source and the
!           receiver plus the lower and upper z-coordinates the kernels
!           are integrated analytically.  The first 3 elements of the
!           array is for the x, y, and z-coordinates of the receiver,
!           which are followed by the x, y, and z-coordinates of the
!           source.  The last two elements are the lower and upper
!           z-coordinates of the integral interval.  If the kernels
!           are not integrated,  the last two elements must be identical
!           to the source z-coordinate,  i.e.,  the last three elements
!           of PSTION must be the same if the kernels are not to be
!           integrated in the vertical direction.
!   FRQ:    Real(*4), frequency.
!   MLAYER: Integer, number of layers excluding the air.
!   ZBND:   Real ZBND(0:MLAYER), coordinates of the layer
!           boundaries with the air earth interface being zbnd(0)=0.
!   CDH:    Complex CDH(0:MLAYER),  complex conductivities of
!           the layers in the lateral direction including the
!           air (layer 0).
!   RMU:    relateive permeabilities of the layers
!   NZOB:   Integer, number of z-levels. Determined in the main program.
!   ZOBG:   Real ZOBG(NZOB), the nzob z-levels coordinates.
!   NZSR:   Integer,  number of z'-levels.
!   ZSRG:   Real ZSRG(NZSR),  the nzsr z'-levels.
!   RHOMIN: Real,  minimum of the lateral grid (rho) which determines
!           the minimum of rho in the Hankel integrals.  RHOMIN=0.1 for
!           the low frequency module and RHOMIN=0.01 for the high
!           frequency module.
!   DMIN:   Real,  minmum of dimension for numerical treatments of cells.
!           DMIN=0.1 for the low frequency module and DMIN=0.001 for the
!           high frequency module.
!   NHFILM: Integer,  maximal number of grids in the lateral direction
!           (rho) for all the Hankel integrals.
!   NRG:    Integer,  actual number of grids in the lateral direction
!           (rho) for the arrays RRG and GRHF.
!   RRG:    Real RRG(NRG),  the NRG grid points in rho.
!   GRHF:   Complex GRHF(11,NHFILM,NZSR,NZOB), tabulated values of
!           the Hankel integrals.  To be passed to the interpolation
!           routines.
!   NRG3:   Integer,  number of actual grids in the lateral direction
!           (rho) for the arrays RRG3 and GRHF3.
!   RRG3:   Real RRG3(NRG),  the NRG3 grid points in rho.
!   GRHF3:  Complex GRHF3(11,NHFILM,NZSR), tabulated values of
!           the Hankel integrals for outcropping cells.  To be passed
!           to the interpolation routines.
!   GRHO0:  Complex GRHO0(4,NZSR,NZOB), tabulated values of the
!           Hankel integrals for rho=0 (source and receiver have the
!           same x and y coordinates).
!   GRHO03:  Complex GRHO03(4,NZSR,NZOB), tabulated values of the
!           Hankel integrals for outcropping cells for rho=0 (source
!           and receiver have the same x and y coordinates).
!   CLX:    Real,  dimension of the cell in the x-direction.
!   CLY:    Real,  dimension of the cell in the y-direction.
!   CLZ:    Real,  dimension of the cell in the z-direction.
!   KCLMN:  Integer,  controlling parameter for the choice of values
!           for the parameter CLMN.  KCLMN=0:  automatic;  and
!           KCLMN=1:  input.  See the following description for
!           CLMN for details.
!   CLMN:   Real,  medium dimension for which the number steps
!           in the numerical integration is defined.  The parameter
!           CLMN is a global parameter. For the numerical
!           integrations of self-cells and most directly adjacent
!           cells,  a parameter CLREF is found by the minimal cell
!           dimensions of that cell if keyg=1.  Otherwise the
!           global value of CLMN is used.  This allows automatic
!           control of the accuracy of integration for most cases
!           while minimizing the computation time.  Since the
!           numbers of steps for the volume and the surface
!           integrations over the cell for certain accuracy are
!           mostly determined by the geometric angle of the
!           receiver with respect to cell deimensions,  the
!           minimal cell dimension may be the best reference
!           length in forming the matrix.   But this reference
!           length may increase computation unnecessarily for
!           computing the secondary fields as well as for the
!           matrix entries of far separeted cells if the body is
!           discretised into greatly non-equal sizes of cells,
!           such as very big cells for some part and very thin
!           plate-like cells for other parts.  Therefore,  a
!           global parameter is necessary.  For keyg=1, however,
!           the global clmn may be used if KCLMN=1 which may
!           add more flexibility in dealing with complex structures
!           as well as for manipulating the numerical integrations.
!           Clmn may be input or may have defaulted values.   See
!           documention on the input parameters KCLMN and CLMN
!           in "MARCO.DOC" for details.
!   BLMIN:  Real,  radius and height of a cylinder to be removed
!           from computation for the treatment of singularity
!           arising in the computation of magnetic fields due to
!           an outcropping cell.  See description for KUTCRP
!           in the following for more details.
!   KACC:   Integer,  accuracy level of computation.  KACC=1, 2
!           3, 4, or 5.  KACC controls the accuracy of the
!           numerical integration of the Green's functions
!           with increasing number of steps as KACC increases.
!           The higher the level,  the more accurate at the
!           cost of computation time.  A 2 or 3 is usually
!           adequate.  For high contrast models us 3 or 4.
!           KACC=5 should be used only for serious convergence
!           tests purpose.  See documentation "MARCO.DOC" for
!           more details.
!   KUTCRP: Integer,  parameter controlling whether the receiver
!           is located just above a outcroping cell,  which is
!           determined in the routines emrspm and emrspms.  If so
!           (koutcrp=1),  electric fields won't be computed by
!           routines THR_D_GREEN and geprm.  Instead, the electric
!           fields at the top of a outcroping cell is determined
!           by the scattering current directly in emrspm and emrspms.
!           Magnetic fields are integrated by excluding a small
!           volume (a cylinder with radius and height of BLMIN).
!           This parameter was first introduced for MT problems
!           and has been adapted for downhole receivers of
!           controlled source problems as well as for MT receivers
!           at different z planes.  Thus, koutcrop=1 does not
!           necessarily mean the body outcrops, it may also mean
!           that the receiver is on the surfaces of or within the
!           body.
!           Note that koutcrop=0 when forming the scattering matrix
!           (keyg=1).
!   KSELF:  Integer,  parameter controlling whether computations
!           for self-cells should be done.  It is to be passed to
!           routine THR_D_GEPRM.  See THR_D_GEPRM for detail.
!
!
! Output parameters:
!
!   ECOMP:  Complex ECOMP(9),  the nine components of the electric
!           field tensor,  Exx, Eyx, Ezx, Exy, ..., and Ezz.  Note
!           that in parameters Exy etc. the first index "x" denotes
!           the direction of the field and the second index "y"
!           denotes the direction of the source.
!   HCOMP:  Complex HCOMP(9),  the nine componenets for the magnetic
!           field tensor which is arranged in the same way as ECOMP.
!
!
!   Note that the Green's functions are not separated into primary and
!   secondary parts as in routine THR_D_GREEN.   Whether the kernels are
!   integrated analytically in the z'-directions or not is controlled
!   in the routine ONE_D_HF_TABLE.  The calling routine ONE_D_LINE must
!   correctly determine this.
!
!   For loop sources, i.e., ksrc=4, 5, or 6, the charge terms in
!   the electric fields need be canceled in the program since
!   they cannot be canceled numerically. This is considered in
!   kernel function, routine kernel, as well as in the routines
!   gridhf and gridcs.   However,  the components Ezz and Exx
!   for rho=0 have different factors before and after the charge
!   terms are removed.  Those factors are numerically more
!   advantagous to be considered here than to be considered in
!   the kernel functions.
!
!   Note that the Hankel integrals for the Green's functions must
!   computed before this routine is called.  The integrals are first
!   tabulated by routine THR_D_HF_TABLE and then interpolated by routines
!   INTERPO_LG4 etc.
!
!
! --- Remarks:
!
!     In parameters like EXY etc. the first index "x" denotes the
!     direction of the field and the second index "y" denotes
!     the direction of the source.
!
!     Cells are not allowed to intersect any layer boundary if
!     numerical integrations of the Green's functions are applied,
!     for otherwise the implementation of the numerical
!     integrations will be much more complicated and possiblly
!     inaccurate.
!
!     Kernels are integrated analytically in the z'-directions.
!     This has been done in the routine THR_D_HF_TABLE with KITG=1.
!     Thus the numerical integrations here are,  except for
!     self-cells, surface integration only.
!
!     The numbers of steps for the primary and secondary parts,
!     or the total Green's functions if not separated,  have
!     been thoroughly tested for both keyg=1 and keyg=2.
!
!     If keyg=1, the primary and secondary parts of the Green's
!     functions are separated if z and z' are in the same layer.
!     however, if keyg=2, they are not separated unless the
!     receiver is on or within the cell.
!
!
!****  CALLED by:
!
!****  CALLS    :  geprm, ghprm, vxyz, intpl, intpl1, intpl4 and intpl5
!
!
      IMPLICIT NONE
!
      REAL BLMIN,C1,C2,CLDST,CLMN,CLNGTH,CLREF,CLX,CLXS,CLY,CLYS,CLZ,CM,  &
           CMX,CMY,CX,CY,DIST,FRQ,RCD
      REAL RHO,RHO1,RHOMIN,SCL,X1,XOB,XSR,XSRS,Y1,YOB,YSR,    &
           YSRS,ZOB,ZSR
      INTEGER I,II,IOB,ISR,ISUB,J,JSUB,KACC,KCLMN,KEYG,KHF,KPRM,KRCD,     &
              KSELF,KUTCRP,MCX,MCY,MINT,MLAYER,MSX
      INTEGER MSY,MX,MXINT,MY,MYINT,MZINT,NF,NHFILM,NOB,NRG,NRG3,NSR,     &
              NZOB,NZSR
!
      COMPLEX ECOMP(9),HCOMP(9),                              &
              EXX,EYX,EZX,EXY,EYY,EZY,EXZ,EYZ,EZZ,            &
              HXX,HYX,HZX,HXY,HYY,HZY,HXZ,HYZ,HZZ,            &
              GAX,HF(11),WMU,CDH(0:MLAYER),S(4),WMU0,         &
              GRHF(11,NHFILM,NZSR,NZOB),GRHO0(4,NZSR,NZOB),   &
              GRHF3(11,NHFILM,NZSR),GRHO03(4,NZSR)
      REAL ZBND(0:MLAYER),RMU(0:MLAYER),ZOBG(NZOB),   &
           ZSRG(2,NZSR),RRG(NRG),RRG3(NRG3),PSTION(6),DMIN
!
      C2=1./4./3.1415926
      WMU0=CMPLX(0.,78.9568352E-7*FRQ)
!
! --- Extract parameter values for XOB etc. from PSTION
!
      XOB=PSTION(1)
      YOB=PSTION(2)
      ZOB=PSTION(3)
      XSR=PSTION(4)
      YSR=PSTION(5)
      ZSR=PSTION(6)
!
! ---- Set the number of steps for the numerical integration according
!    to the accuracy levels. Note that the numbers of steps are assumed
!    to be equal in both of the three dimensions at first. The actual
!    values are then determined later on by the ratios of the lengths
!    of a specific cell in either dimension with the value of the
!    parameter CLMN and by the distance of the cell to the receiver
!    point.
!
      IF (KACC==1) THEN
         MINT=2
      ELSE IF (KACC==2) THEN
         MINT=4
      ELSE IF (KACC==3) THEN
         MINT=6
      ELSE IF (KACC==4) THEN
         MINT=8
      ELSE
         MINT=12
      END IF
!
! ---- Determine the actual number of steps in each dimensions
!
      MXINT=MINT
      MYINT=MINT
      MZINT=MINT
!
! ---- Determine the reference length of the cell dimensions
!    to which the above-asigned numbers of steps are defined.
!    If kclmn=0 and keyg=1, choose the minimal length of the
!    cell dimensions for self-cells and most directly adjacent
!    cells.  Otherwise, use the global value.
!
      CLREF=MIN(CLX,CLY,CLZ)+DMIN
      IF (KCLMN==0.AND.KEYG==1.AND.ABS(XOB-XSR)<CLREF.AND.ABS(YOB-YSR)  &
          <CLREF.AND.ABS(ZOB-ZSR)<CLREF) THEN
         CLREF=MIN(CLX,CLY,CLZ)
      ELSE
         CLREF=CLMN
      END IF
!
      IF (CLX>CLREF) MXINT=INT(FLOAT(MXINT)*(CLX/CLREF))
      IF (CLY>CLREF) MYINT=INT(FLOAT(MYINT)*(CLY/CLREF))
      IF (CLZ>CLREF) MZINT=INT(FLOAT(MZINT)*(CLZ/CLREF))
!
! --- Determine the layer numbers the cell and the receiver
!   are located.
!   Note that each cell is confined within a single layer.
!
      DO I=MLAYER-1,0,-1
         IF (ZOB>=ZBND(I)) THEN
            NOB=I+1
            GOTO 100
         END IF
      END DO
      NOB=0
  100 DO I=MLAYER-1,0,-1
         IF (ZSR>=ZBND(I)) THEN
            NSR=I+1
            GOTO 200
         END IF
      END DO
      NSR=0
!
! ---- Determine the level of zob in the grid array zobg
!
  200 DO I=1,NZOB
         IF (ABS(ZOB-ZOBG(I))<DMIN) THEN
            IOB=I
            GOTO 300
         END IF
      END DO
!
      WRITE (4,*) 'Warning: failed to choose a z-level for zob=',ZOB
!
! ---- Determine the level of zsr in the grid array zsrg
!    Note that the z'-levels in the zsr-direction are
!    determined by the full lengths of the cells
!
  300 DO I=1,NZSR
         IF (ABS(ZSR-CLZ/2.-ZSRG(1,I))<DMIN.AND.  &
             ABS(ZSR+CLZ/2.-ZSRG(2,I))<DMIN) THEN
            ISR=I
            GOTO 400
         END IF
      END DO
!
      WRITE (4,*) 'Warning: failed to choose a z_prime-level',  &
                  ' for zsr=',ZSR
!
!
! ---- Determine if the Green's functions are to be computed
!    separately as primary and secondary parts. Note this has
!    been taken into account already in the routine gridhf
!    where the gridding is formed.
!
!    Note that the meaning of KPRM is as follows:
!
!       KPRM = 0: the primary fields (the whole space terms) are cal-
!                 culated by the subroutines THR_D_GEPRM and THR_D_GHPRM; and
!            = 1: the primary fields are calculated together with the
!                 secondary terms by means of Hankel transforms
!
!    It is adviced however, not to calculate the whole space and secon-
!    dary parts separately if the observation points (ZOB or z') is in
!    the air and the source (ZSR) is in the earth, because the conducti-
!    vity of the air approaches zero and only simple precision is used
!    here in the calculation and so the electrical fields resulted may
!    be inaccurate.
!
!
  400 WMU=WMU0*RMU(NOB)
!
      IF (KEYG==1.AND.NOB==NSR) THEN
         KPRM=0
      ELSE
         KPRM=1
      END IF
!
! ---  Further reductions
!
! -- Reduce the number of steps for the secondary terms.  Tests
!    have shown that with a reduction factor of about 1.5 the
!    numerica integrations of the secondary parts may match the
!    accuracy of the primary parts.  Note the secondary parts
!    cost far more computation time for one single value (one step)
!    than the priamry parts.
!
!    This reduction is only valid if when the primary parts are
!    separated from the computation here
!
!  !! May sometimes affects the accuracy !!
!
!    IF(KPRM.EQ.0) THEN
!      MXINT=MXINT*.7
!      MYINT=MYINT*.7
!    END IF
!
! ----- The numbers of steps determined above are for the self-cells
!     only, i.e., for |r-r'|=0.  Each cell will be divided into
!     many sub-cells in order to reduce unnecessary computations
!     for cells having great aspect ratios.  The numbers of steps
!     for the numerical integration is then determined by the
!     distance of the subcells to the receiver point.
!
!     Note that the subcells for the primary parts are determined
!     similarly in the corresponding routines.
!
      IF (KPRM==0) THEN
!
! Calculating the primary terms integrated over the cell
!
! --- Note that the E-field of a outcropping block is computed by
!   the scattering current directly
!
         CALL THR_D_GEPRM(PSTION,KACC,KSELF,CLREF,MXINT,MYINT,MZINT,CLX, &
                          CLY,CLZ,FRQ,CDH(NSR),RMU(NSR),DMIN,GAX,ECOMP)
         EXX=ECOMP(1)
         EYX=ECOMP(2)
         EZX=ECOMP(3)
         EXY=ECOMP(4)
         EYY=ECOMP(5)
         EZY=ECOMP(6)
         EXZ=ECOMP(7)
         EYZ=ECOMP(8)
         EZZ=ECOMP(9)
         EXX=GAX+CDH(NSR)/CDH(NOB)*(EXX-GAX)
         EYX=CDH(NSR)/CDH(NOB)*EYX
         EYY=GAX+CDH(NSR)/CDH(NOB)*(EYY-GAX)
         EXY=EYX
         EZX=CDH(NSR)/CDH(NOB)*EZX
         EZY=CDH(NSR)/CDH(NOB)*EZY
         EXZ=CDH(NSR)/CDH(NOB)*EXZ
         EYZ=CDH(NSR)/CDH(NOB)*EYZ
         EZZ=CDH(NSR)/CDH(NOB)*EZZ
      END IF
!
      IF (KPRM==1) THEN
         EXX=(0.,0.)
         EYX=(0.,0.)
         EZX=(0.,0.)
         EXY=(0.,0.)
         EYY=(0.,0.)
         EZY=(0.,0.)
         EXZ=(0.,0.)
         EYZ=(0.,0.)
         EZZ=(0.,0.)
         IF (KEYG==2) THEN
            HXX=(0.,0.)
            HYX=(0.,0.)
            HZX=(0.,0.)
            HXY=(0.,0.)
            HYY=(0.,0.)
            HZY=(0.,0.)
            HXZ=(0.,0.)
            HYZ=(0.,0.)
            HZZ=(0.,0.)
         END IF
      END IF
!
! -----------  Compute the secondary terms  --------------
!
! ---- Divide the cell into subcells
!
!    Every subcell has a length of either 2*CLREF or 3*CLREF
!    in either direction, according to kacc.   For higher
!    kacc it is more efficient to have smaller subcell size.
!    But small subcell may increase waste for low kacc.
!
!    Note that the cell is not divided in the vertical direction
!    since the Hankel transforms and intergrated analytically
!    in the z-direction.
!
      IF (KACC<=3) THEN
         SCL=3.*CLREF
      ELSE
         SCL=2.*CLREF
      END IF
      MSX=INT(CLX/SCL)
      MSY=INT(CLY/SCL)
      IF (MSX<1) MSX=1
      IF (MSY<1) MSY=1
      MCX=MXINT/MSX
      MCY=MYINT/MSY

      CLXS=CLX/MSX
      CLYS=CLY/MSY
!
      DO ISUB=1,MSX
         IF (MSX==1) THEN
            XSRS=XSR
         ELSE
            XSRS=XSR+CLXS*(ISUB-1)-CLX/2.+.5*CLXS
         END IF
         DO JSUB=1,MSY
            IF (MSY==1) THEN
               YSRS=YSR
            ELSE
               YSRS=YSR+CLYS*(JSUB-1)-CLY/2.+.5*CLYS
            END IF
!
!-- Determine the distance of the subcell to the receiver point
!   with respect to the dimension of the subcell.
!
!   Note that unlike in the routine geprm where only the dimensions
!   of the surfaces are compared with the distance, here it is the
!   subcell diagonal length that is compared with the distance
!   because the x-y surface used in the integration here is for
!   both the bottom and the top of a cell in the z'-direction.
!
            DIST=SQRT((RHO(XOB,YOB,XSRS,YSRS))**2+(ZOB-ZSR)**2)
            CLNGTH=SQRT(CLXS**2+CLYS**2+CLZ**2)
!
! --- Determine the numbers of steps according to the distance
!   between the source and the receiver
!
!-- Determine the location of the receiver close to the cell.
!   This is necessary for computing the EM fields at receiver
!   sites which are close to the cell.  Note that outcropping
!   cells has been considered already elsewhere.
!
            IF (KEYG==2.AND.KUTCRP==0) THEN
!
               IF (DIST<=CLNGTH) THEN
                  CM=MIN(CLXS,CLYS,CLZ)
                  KRCD=5
                  DO I=1,4
                     IF (I==1) THEN
                        RCD=.05*CM
                     ELSE IF (I==2) THEN
                        RCD=.1*CM
                     ELSE IF (I==3) THEN
                        RCD=.2*CM
                     ELSE
                        RCD=.5*CM
                     END IF
                     IF (XOB>=XSRS-.5*CLXS-RCD.AND.XOB<=XSRS+.5*CLXS+  &
                         RCD.AND.YOB>=YSRS-.5*CLYS-RCD.AND.  &
                         YOB<=YSRS+.5*CLYS+RCD.AND.ZOB>=ZSR-.5*CLZ-  &
                         RCD.AND.ZOB<=ZSR+.5*CLZ+RCD) THEN
                        KRCD=I
                        EXIT
                     END IF
                  END DO
               END IF
!
! ---- Determine the numbers of steps according to DIST and KRCD
!
               IF (DIST<=CLNGTH) THEN
                  IF (KRCD==1) THEN
                     MX=8*MCX
                     MY=8*MCY
                  END IF
                  IF (KRCD==2) THEN
                     MX=5*MCX
                     MY=5*MCY
                  END IF
                  IF (KRCD==3) THEN
                     MX=3*MCX
                     MY=3*MCY
                  END IF
                  IF (KRCD==4) THEN
                     MX=MCX*2
                     MY=MCY*2
                  END IF
                  IF (KRCD==5) THEN
                     MX=MCX*3/2
                     MY=MCY*3/2
                  END IF
               ELSE IF (DIST<=3.*CLNGTH) THEN
                  MX=MCX
                  MY=MCY
               ELSE
                  CLDST=SQRT(2.*CLNGTH/DIST)
                  MX=INT(FLOAT(MCX)*CLDST)
                  MY=INT(FLOAT(MCY)*CLDST)
               END IF
!
            END IF
!
! --- The following rules for the numbers of steps have been tested
!   thoroughly.
!
!   The introduction of the parameter clref garantees the accuracy
!   of the self-cells.   Self-cells are treatly more accurately in
!   order to ensure accuracies since on the other hand the primary
!   parts need to be computed only once for a block.
!
!   It seems that the accuracy of the numerical integrations for
!   cells from 0 to about 3 clengths has more effects on the
!   final results than other cells.
!
!   The rule for cldst may be changed as sqrt(clength/dist) to
!   save a little bit computation time.  There seems to be no
!   significant difference which one to use.
!
!   The decay patterns of the step numbers have been tested with
!   clength/dist, sqrt(clength/dist), and (clength/dist)**.3333.
!   It seems that the second one yields adequate accuracy while
!   minimizing the computation time.
!
!-- The following rules are also used by outcropping cells
!   in computing the responses at receiver sites
!
            IF (KEYG/=2.OR.KUTCRP/=0) THEN
!
               IF (DIST<=1.*CLNGTH) THEN
                  MX=MCX*3/2
                  MY=MCY*3/2
! --  Test has shown that the above rule is insufficient for the
!   magentic fields of outcropping cells.  Mx and My are thus
!   increased for outcropping cells.
                  IF (KEYG==2.AND.KUTCRP==1) THEN
                     MX=MX*4
                     MY=MY*4
                  END IF
               ELSE IF (DIST<=3.*CLNGTH) THEN
                  MX=MCX
                  MY=MCY
               ELSE
                  CLDST=SQRT(2.*CLNGTH/DIST)
                  MX=INT(FLOAT(MCX)*CLDST)
                  MY=INT(FLOAT(MCY)*CLDST)
               END IF
!
            END IF
!
            IF (MX<1) MX=1
            IF (MY<1) MY=1
!
!-- A step number of 3 may not be more accuarate than the 2
!   used for the four-point approach to surface integrations
!
            IF (MX==3) MX=2
            IF (MY==3) MY=2
!
            IF (MX>1) CMX=MX-1.
            IF (MY>1) CMY=MY-1.
!
!-- The four-point approach to the surface integration is
!   preserved from the previous work of Xiong et al (1986)
!
            DO I=1,MX
               IF (MX==1) THEN
                  X1=XSRS
                  CX=1.
               ELSE IF (MX==2) THEN
                  X1=XSRS+2.*(I-1.5)*.288675*CLXS
                  CX=.5
               ELSE
                  X1=XSRS-.5*CLXS+CLXS/CMX*(I-1.)
                  CX=1./CMX
                  IF (I==1.OR.I==MX) CX=.5*CX
               END IF
               DO J=1,MY
                  IF (MY==1) THEN
                     Y1=YSRS
                     CY=1.
                  ELSE IF (MY==2) THEN
                     Y1=YSRS+2.*(J-1.5)*.288675*CLYS
                     CY=.5
                  ELSE
                     Y1=YSRS-.5*CLYS+CLYS/CMY*(J-1.)
                     CY=1./CMY
                     IF (J==1.OR.J==MY) CY=.5*CY
                  END IF
!
                  C1=C2*CX*CY*CLXS*CLYS
!
                  RHO1=RHO(XOB,YOB,X1,Y1)
!
! ----- KHF together with keyg controls which gridding to be chosen
!     KHF=2: ordinary grid values; and
!        =1: a cylinder of radius blmin is excluded for treatment
!            of the singularities. See routine gridhf
!
!     Note this need only be done for outcropping cells.
!     for receivers on or within a cells deep in the earth
!     a cylinder of diameter blmin and height clz is excluded.
!     Here for outcropping cells the height of the cylinder is
!     blmin and the diameter is 2*blmin.   See
!     comments at the beginning of this routine.
!
                  IF (KEYG==2.AND.ABS(ZOB)<DMIN.AND.ABS(ZSR-.5*CLZ)  &
                      <DMIN.AND.RHO1<BLMIN) THEN
                     KHF=1
                  ELSE
                     KHF=2
                  END IF
!
! ----- If Rho < RHOMIN it should be assumed to be zero because the
!     Hankel transforms by digital linear filter are limited for
!     Rho >= RHOMIN
!
                  IF (RHO1>=RHOMIN) THEN
!****************************************************
!*                                                  *
!
                     IF (KEYG==1) THEN
                        NF=6
                     ELSE
                        NF=11
                     END IF
!
! --- khf controls which gridding to be chosen
!
!   Note three-point interpolation rules are used for
!   kacc <= 2 and five-point interpolation rules are
!   used for kacc>=3
!
                     IF (KHF==1) THEN
                        IF (KACC<=2)                                       &
                            CALL INTERPO_LG1(GRHF3,RRG3,NRG3,NHFILM,NZSR,  &
                                      ALOG(RHO1),ISR,HF,NF)
                        IF (KACC>=3)                                       &
                            CALL INTERPO_LG5(GRHF3,RRG3,NRG3,NHFILM,NZSR,  &
                                      ALOG(RHO1),ISR,HF,NF)
                     ELSE
                        IF (KACC<=2)                                       &
                            CALL INTERPO_LG(GRHF,RRG,NRG,NHFILM,NZSR,NZOB, &
                                     ALOG(RHO1),IOB,ISR,HF,NF)
                        IF (KACC>=3)                                        &
                            CALL INTERPO_LG4(GRHF,RRG,NRG,NHFILM,NZSR,NZOB, &
                                      ALOG(RHO1),IOB,ISR,HF,NF)
                     END IF
!
! ----- HF are now divided by rho
!
                     DO II=1,NF
                        HF(II)=HF(II)/RHO1
                     END DO
!
                     IF (KUTCRP==0) THEN
!
!-- Note that the correction done to HF(2) in the kernel
!   function, routine ONE_D_KERNEL,  will not affect this routine
!   since z and z' will never be zero.
!
                        EXX=EXX-C1*WMU*HF(1)-C1/CDH(NOB)               &
                            *(((XOB-X1)/RHO1)**2*HF(2)                 &
                            +(1.-2.*((XOB-X1)/RHO1)**2)/RHO1*HF(4))
                        EYX=EYX-C1/CDH(NOB)*(XOB-X1)*(YOB-Y1)          &
                            /RHO1**2*(HF(2)-2.*HF(4)/RHO1)
                        EYY=EYY-C1*WMU*HF(1)-C1/CDH(NOB)               &
                            *(((YOB-Y1)/RHO1)**2*HF(2)                 &
                            +(1.-2.*((YOB-Y1)/RHO1)**2)/RHO1*HF(4))
                        EXY=EYX
                        EZX=EZX-C1/CDH(NOB)*(XOB-X1)/RHO1*HF(5)
                        EZY=EZY-C1/CDH(NOB)*(YOB-Y1)/RHO1*HF(5)
                        EXZ=EXZ-C1/CDH(NOB)*(XOB-X1)/RHO1*HF(6)
                        EYZ=EYZ-C1/CDH(NOB)*(YOB-Y1)/RHO1*HF(6)
                        EZZ=EZZ+C1/CDH(NOB)*HF(3)
!
                     END IF
!
! ---  Compute magnetic fields if KEYG=2
!
!    A cylinder of diameter blmin is removed if the receiver
!    is located on or within the cell
!
!    Note that outcropping cells has been considered by the
!    parameter KHF
!
                     IF (KEYG==2.AND.                                 &
                         .NOT.(KUTCRP==1.AND.ABS(ZOB)>=.1.AND.        &
                         ABS(ZSR-.5*CLZ)>=.1.AND.RHO1<=.5*BLMIN)) THEN
!
                        HXX=-C1*(XOB-X1)*(YOB-Y1)                     &
                            /RHO1**2*(HF(7)-2./RHO1*HF(9))+HXX
                        HYX=C1*HF(8)+C1*((XOB-X1)/RHO1)**2*HF(7)      &
                            +C1*(1.-2.*((XOB-X1)/RHO1)**2)/RHO1*HF(9) &
                            +HYX
                        HXY=-C1*HF(8)-C1*((YOB-Y1)/RHO1)**2*HF(7)     &
                            -C1*(1.-2.*((YOB-Y1)/RHO1)**2)/RHO1*HF(9) &
                            +HXY
                        HYY=-HXX
                        HZX=C1*(YOB-Y1)/RHO1*HF(10)+HZX
                        HZY=-C1*(XOB-X1)/RHO1*HF(10)+HZY
                        HXZ=-C1*(YOB-Y1)/RHO1*HF(11)+HXZ
                        HYZ=C1*(XOB-X1)/RHO1*HF(11)+HYZ
                        HZZ=(0.,0.)
                     END IF
!
!*                                                  *
!****************************************************
                  END IF
!
                  IF (RHO1<RHOMIN) THEN
!****************************************************
!*                                                  *
!
                     DO II=1,KEYG+1
                        IF (KEYG==2.AND.KHF==1) THEN
                           S(II)=GRHO03(II,ISR)
                        ELSE
                           S(II)=GRHO0(II,ISR,IOB)
                        END IF
                     END DO
!
                     IF (KUTCRP==0) THEN
                        S(1)=-C1/CDH(NOB)*S(1)
                        EXX=EXX+S(1)
                        EYY=EYY+S(1)
                        EZZ=EZZ-C1/CDH(NOB)*S(2)
                     END IF
!
                     IF (KEYG/=1.AND.KUTCRP/=1) THEN
                        HYX=C1*S(3)+HYX
                        HXY=-C1*S(3)+HXY
                     END IF
!
!*                                                  *
!****************************************************
                  END IF
!
               END DO
            END DO
         END DO
      END DO
!
!
! --- Assign the componenets to the arrays ECOMP and HCOMP
!   for data passing
!
      ECOMP(1)=EXX
      ECOMP(2)=EYX
      ECOMP(3)=EZX
      ECOMP(4)=EXY
      ECOMP(5)=EYY
      ECOMP(6)=EZY
      ECOMP(7)=EXZ
      ECOMP(8)=EYZ
      ECOMP(9)=EZZ
      IF (KEYG==2) THEN
         HCOMP(1)=HXX
         HCOMP(2)=HYX
         HCOMP(3)=HZX
         HCOMP(4)=HXY
         HCOMP(5)=HYY
         HCOMP(6)=HZY
         HCOMP(7)=HXZ
         HCOMP(8)=HYZ
         HCOMP(9)=HZZ
      END IF
!
      RETURN
!
      END
!
!**** End of THR_D_GREEN
!
!
      SUBROUTINE THR_D_VXYZ(CLX,CLY,CLZ,XSR,YSR,ZSR,XX,YY,ZZ)
!
!**** Six-point rule for numerical integration of a prismatic cube
!
! Input parameters:
!
!   CLX,CLY,CLZ:  Real,  dimension of the cell in the x, y,
!                 and z direction;  and
!   XSR,YSR,ZSR:  Real,  centre coordinates of the cell in
!                 the x, y and z-direction.
!
! Output parameters:
!
!   XX, YY, ZZ:   Real XX(6), YY(6), ZZ(6),  the six sampling
!                 points for numerical volume integration.
!
!**** Called by:  THR_D_GEPRM, THR_D_GHPRM
!
!**** Calls:      None
!
!
      IMPLICIT NONE
!
      INTEGER I
      REAL SIGN
      REAL  CLX,CLY,CLZ,XSR,YSR,ZSR,XX(6),YY(6),ZZ(6)
!
      DO I=1,4
         XX(2+I)=XSR
         ZZ(I)=ZSR
      END DO
      DO I=1,2
         YY(I)=YSR
         YY(4+I)=YSR
         SIGN=2.*(I-1.5)
         XX(I)=XSR+SIGN*CLX/2.
         YY(2+I)=YSR+SIGN*CLY/2.
         ZZ(4+I)=ZSR+SIGN*CLZ/2.
      END DO
!
      RETURN
      END
!
!**** End of THR_D_VXYZ
!
!
      SUBROUTINE THR_D_GEPRM(PSTION,KACC,KSELF,CLREF,MXINT,MYINT,MZINT,   &
                             CLX,CLY,CLZ,FRQ,CDH,RMU,DMIN,GAX,ECOMP)
!
!**** Integration of the primary e-fields of the tensor green's function
!
!
! Input parameters:
!
!   KACC:   Integer,  accuracy level of computation.  KACC=1, 2
!           3, 4, or 5.  KACC controls the accuracy of the
!           numerical integration of the Green's functions
!           with increasing number of steps as KACC increases.
!           The higher the level,  the more accurate at the
!           cost of computation time.  A 2 or 3 is usually
!           adequate.  For high contrast models us 3 or 4.
!           KACC=5 should be used only for serious convergence
!           tests purpose.  See documentation "MARCO.DOC" for
!           more details.
!   KSELF:  Integer,  parameter controlling whether computations
!           for self-cells should be done.  This parameter is
!           determined in routines forming the scattering matrix.
!           Primary contributions of self-cells (fields due to
!           the cell at it's own cell centre) are the same for
!           cells with the same dimensions in the same layer (
!           being treated as a whole space).  Since self-cells
!           are expensive to compute,  computing them only once
!           helps to reduce computation time.   Note that cells
!           must be arranged in certain order in order to avoid
!           fetching the wrong values for self-cells with different
!           dimensions.  Field values for a self-cell are stored
!           in the parameters EXX1,  EYX1, ..., EZZ1, and GAX1.
!           routine THR_D_GEPRM.  See THR_D_GEPRM for detail.
!   CLREF:  Real,  minimal dimension of the cell in consideration.
!           In routine THR_D_GREEN it is used as the reference
!           dimension for which the number of steps for numerical
!           integration is defined.  Actual number of steps in each
!           dimension depends on the ratio of that dimension to
!           CLREF.  Here and in the computation of the secondary
!           parts of the Green's tensors in routine THR_D_GREEN
!           CLREF is used as a reference to further dividing a
!           cell into subcells for more efficient and accurate
!           treatments of the numerical integrations.
!   MXINT:  Integer,  number of steps in the x-direction.
!   MYINT:  Integer,  number of steps in the y-direction.
!   MZINT:  Integer,  number of steps in the z-direction.
!   CLX:    Real,  dimension of the cell in the x-direction.
!   CLY:    Real,  dimension of the cell in the y-direction.
!   CLZ:    Real,  dimension of the cell in the z-direction.
!   FRQ:    Real(*4), frequency.
!   CDH:    Complex CDH(0:MLAYER),  complex conductivities of
!           the layers including the air (layer 0).
!   DMIN:   Real,  minmum of dimension for numerical treatments of cells.
!           DMIN=0.1 for the low frequency module and DMIN=0.001 for the
!           high frequency module.
!
! Output parameters:
!
!   GAX:    Complex, the primay current term due to a selff-cell.
!   ECOMP:  Complex ECOMP(9),  the nine components of the electric
!           field tensor,  Exx, Eyx, Ezx, Exy, ..., and Ezz.  Note
!           that in parameters Exy etc. the first index "x" denotes
!           the direction of the field and the second index "y"
!           denotes the direction of the source.
!
!****  CALLED by: THR_D_GREEN
!
!****  CALLS    : THR_D_GASELF,  THR_D_VXYZ
!
!
      IMPLICIT NONE
!
      REAL B1,B2,B3,CLDST,CLNGTH,CLXS,CLYS,CLZS,CMX,CMY,CMZ,CX,CY,CZ,DIST, &
           R,R1,RR,RS,SCL,T2,T3,T4,X1,XSRS,Y1,               &
           YSRS,Z1,ZSRS
      INTEGER I,II,ISUB,J,J3,JJ,JSUB,KACC,KINT,KSELF,KSUB,L,MCX,MCY,MCZ,   &
              MM1,MM2,MM3,MSX,MSY,MSZ,MX,MXINT,MY,MYINT,MZ,MZINT
      REAL  DMIN,XOB,YOB,ZOB,XSR,YSR,ZSR,PSTION(6),          &
            CLX,CLY,CLZ,FRQ,CLREF,XX(6),YY(6),ZZ(6),RMU
      COMPLEX CDH,K,WMU,CG,G1,G2,G3,C2,C1,                &
              GX,GY,GZ,GZX,GZY,GAX,GAX1,D1,D2,D3,ECOMP(9),   &
              EXX,EXY,EXZ,EYX,EYY,EYZ,EZX,EZY,EZZ,           &
              EXX1,EXY1,EXZ1,EYX1,EYY1,EYZ1,EZX1,EZY1,EZZ1
      SAVE EXX1,EYX1,EZX1,EXY1,EYY1,EZY1,EXZ1,EYZ1,EZZ1,GAX1
!
!      R(XR,YR,ZR,XI,YI,ZI)=SQRT((XR-XI)**2+(YR-YI)**2+(ZR-ZI)**2)
!      RR(XR,YR,XI,YI)=SQRT((XR-XI)**2+(YR-YI)**2)

      WMU=CMPLX(.0,78.9568352E-7*FRQ)*RMU
      K=CSQRT(WMU*CDH)
!
! --- Extract parameter values for XOB etc. from PSTION
!
      XOB=PSTION(1)
      YOB=PSTION(2)
      ZOB=PSTION(3)
      XSR=PSTION(4)
      YSR=PSTION(5)
      ZSR=PSTION(6)
!
      RS=R(XOB,YOB,ZOB,XSR,YSR,ZSR)
!
! ----- Compute a self-cell unless kself=1
!
      IF (KSELF/=1.AND.RS<DMIN) THEN
         EXX=EXX1
         EYX=EYX1
         EZX=EZX1
         EXY=EXY1
         EYY=EYY1
         EZY=EZY1
         EXZ=EXZ1
         EYZ=EYZ1
         EZZ=EZZ1
         GAX=GAX1
!
!**** Goto the end of the routine for return
!
         GOTO 100
!
      END IF
!
! ---- Divide the cell into subcells
!
!    Every subcell has a length of either 2*CLREF or 3*CLREF
!    in either direction, according to kacc.   For higher
!    kacc it is more efficient to have smaller subcell size.
!    But small subcell may increase waste for low kacc.
!
      IF (KACC<=3) THEN
         SCL=3.*CLREF
      ELSE
         SCL=2.*CLREF
      END IF
      MSX=INT(CLX/SCL)
      MSY=INT(CLY/SCL)
      MSZ=INT(CLZ/SCL)
!
!--- For the treatment of the singularity in the current term for
!    self-cells the subdivisions of cells should be in odd
!    numbers (the centre subcell is approximated by a sphere or
!    spheroid).   This may also increase the accuracy of the
!    surface integrations for the charge terms a little bit.
!
      MSX=(MSX/2)*2+1
      MSY=(MSY/2)*2+1
      MSZ=(MSZ/2)*2+1
!
      IF (MSX<1) MSX=1
      IF (MSY<1) MSY=1
      IF (MSZ<1) MSZ=1
      MCX=MXINT/MSX
      MCY=MYINT/MSY
      MCZ=MZINT/MSZ
      CLXS=CLX/MSX
      CLYS=CLY/MSY
      CLZS=CLZ/MSZ
!
! ---- Compute the current terms by volumn integrations
!
      GX=(.0,.0)
      GZ=(.0,.0)
      GZX=(.0,.0)
      GZY=(.0,.0)
!
      DO ISUB=1,MSX
         IF (MSX==1) THEN
            XSRS=XSR
         ELSE
            XSRS=XSR+CLXS*(ISUB-1)-CLX/2.+.5*CLXS
         END IF
         DO JSUB=1,MSY
            IF (MSY==1) THEN
               YSRS=YSR
            ELSE
               YSRS=YSR+CLYS*(JSUB-1)-CLY/2.+.5*CLYS
            END IF
            DO KSUB=1,MSZ
               IF (MSZ==1) THEN
                  ZSRS=ZSR
               ELSE
                  ZSRS=ZSR+CLZS*(KSUB-1)-CLZ/2.+.5*CLZS
               END IF
!
!-- Determine the distance of the subcell to the receiver point
!   with respect to the dimension of the subcell.
!
               DIST=SQRT((RR(XOB,YOB,XSRS,YSRS))**2+(ZOB-ZSRS)**2)
               CLNGTH=SQRT(CLXS**2+CLYS**2+CLZS**2)
!
               IF (DIST<DMIN) THEN
!*****************************************************
!*                                                   *
!
!--- If DIST < DMIN it should be assumed to be zero for the sake
!    of numerical accuracy.
!
                  CALL THR_D_GASELF(CLXS,CLYS,CLZS,MCX,MCY,MCZ,CDH,K,  &
                                    DMIN,G1,G2)
                  GX=GX+G1
                  GZ=GZ+G2
!*                                                   *
!*****************************************************
               END IF
!
               IF (DIST>=DMIN) THEN
!
!*****************************************************
!*                                                   *
!
!-- Determine the numbers of steps according to the distance
!   between the source and the receiver.
!   Self-cells are considered in the above line.
!
!   See the comments on the choices of step numbers in the
!   routine THR_D_GREEN.
!
                  IF (DIST<=1.*CLNGTH) THEN
                     MX=MCX*3/2
                     MY=MCY*3/2
                     MZ=MCZ*3/2
                  ELSE IF (DIST<=3.*CLNGTH) THEN
                     MX=MCX
                     MY=MCY
                     MZ=MCZ
                  ELSE
                     CLDST=SQRT(2.*CLNGTH/DIST)
                     MX=INT(FLOAT(MCX)*CLDST)
                     MY=INT(FLOAT(MCY)*CLDST)
                     MZ=INT(FLOAT(MCZ)*CLDST)
                  END IF
!
!--- The current terms are more smooth than the charge terms
!    for low frequencies.  Hence the number of steps for the
!    numerical integrations can be reduced.
!
!    Tests have shown that the current terms can greatly
!    affect the final accuracies for frequencies above
!    100 Hz or so.   But for lower frequencies the following
!    reduction of MX etc. seems reasonable.
!
!--- Tests done by CRA, Australia indicate that the following
!    reduction in MX etc. introduce a few percent of errors
!    in the magnetic fields for frequency at 100 Hz, and that
!    the results show a jump at 100 Hz in a magnified scale.
!    Further tests showed that even the reduction for frequency
!    lower than 1 Hz there are appreciable instability at
!    later times in time domain solutions.  Thus the following
!    reduction in MX etc. is elimilated.
!
!       IF(FRQ.LT.1.) THEN
!         MX=MX*2/3
!         MY=MY*2/3
!         MZ=MZ*2/3
!       END IF
!
                  IF (MX<1) MX=1
                  IF (MY<1) MY=1
                  IF (MZ<1) MZ=1
!
                  IF (MX>1) CMX=MX-1.
                  IF (MY>1) CMY=MY-1.
                  IF (MZ>1) CMZ=MZ-1.
!
                  C2=-WMU*CLXS*CLYS*CLZS
!
! --- The parameter kint controls if the six-point rule or the
!   multi-step trapezoidal rule should be used.
!   The six-point approach to the volume integration is perserved
!   from the previous work of Xiong et al (1986)
!
!   Step numbers less than or equal to 3 in either dimension
!   may not be more accuarate than the 6-point approach
!
                  IF (MX<=3.AND.MY<=3.AND.MZ<=3.AND.MX>1.AND.MY>1.AND.  &
                      MZ>1) THEN
                     KINT=1
                  ELSE
                     KINT=2
                  END IF
!
                  IF (KINT==1) THEN
                     CALL THR_D_VXYZ(CLXS,CLYS,CLZS,XSRS,YSRS,ZSRS,XX,YY,ZZ)
                     MM1=1
                     MM2=1
                     MM3=6
                  ELSE
                     MM1=MX
                     MM2=MY
                     MM3=MZ
                  END IF
!
                  DO I=1,MM1
                     IF (KINT==2.AND.MM1==1) THEN
                        X1=XSRS
                        CX=1.
                     END IF
                     IF (KINT==2.AND.MM1/=1) THEN
                        X1=XSRS-.5*CLXS+CLXS/CMX*(I-1.)
                        CX=1./CMX
                        IF (I==1.OR.I==MX) CX=.5*CX
                     END IF
                     DO J=1,MM2
                        IF (KINT==2.AND.MM2==1) THEN
                           Y1=YSRS
                           CY=1.
                        END IF
                        IF (KINT==2.AND.MM2/=1) THEN
                           Y1=YSRS-.5*CLYS+CLYS/CMY*(J-1.)
                           CY=1./CMY
                           IF (J==1.OR.J==MY) CY=.5*CY
                        END IF
                        DO J3=1,MM3
                           IF (KINT==2.AND.MM3==1) THEN
                              Z1=ZSRS
                              CZ=1.
                           ELSE IF (KINT==2.AND.MM3/=1) THEN
                              Z1=ZSRS-.5*CLZS+CLZS/CMZ*(J3-1.)
                              CZ=1./CMZ
                              IF (J3==1.OR.J3==MZ) CZ=.5*CZ
                           ELSE
                              X1=XX(J3)
                              Y1=YY(J3)
                              Z1=ZZ(J3)
                           END IF
!
                           IF (KINT==1) THEN
                              C1=C2/6.
                           ELSE
                              C1=C2*CX*CY*CZ
                           END IF
!
                           R1=R(XOB,YOB,ZOB,X1,Y1,Z1)
!
                           G1=.0795774771*EXP(-K*R1)/R1
                           GX=GX+G1*C1
                           GZ=GX
                        END DO
                     END DO
                  END DO
!
!*                                                   *
!*****************************************************
               END IF
!
            END DO
         END DO
      END DO
!
      GY=GX
      GZ=GX
      GAX=GX
!
! ---- Compute the charge terms by surface integrations
!
      C2=.0795774771/CDH
      T2=.288675*CLXS
      T3=.288675*CLYS
      T4=.288675*CLZS
!
      DO L=1,3,2
!
         D1=(.0,.0)
         D2=(.0,.0)
         D3=(.0,.0)
         X1=XSR-(FLOAT(L)-2.)*CLX*.5
!
!-- Note that the coordinates in the x-direction are fixed
!   and cells are divided only in the y and z directions
!
         DO JSUB=1,MSY
            IF (MSY==1) THEN
               YSRS=YSR
            ELSE
               YSRS=YSR+CLYS*(JSUB-1)-CLY/2.+.5*CLYS
            END IF
            DO KSUB=1,MSZ
               IF (MSZ==1) THEN
                  ZSRS=ZSR
               ELSE
                  ZSRS=ZSR+CLZS*(KSUB-1)-CLZ/2.+.5*CLZS
               END IF
!
!-- Determine the distance of the subcell surface over which the
!   integration is to be done to the receiver point with
!   respect to the dimension of the subcell surface.
!
               DIST=SQRT((RR(XOB,YOB,X1,YSRS))**2+(ZOB-ZSRS)**2)
               CLNGTH=SQRT(CLYS**2+CLZS**2)
!
!-- Determine the numbers of steps according to the distance
!   between the source and the receiver
!
!   See the comments on the choices of step numbers in the
!   routine THR_D_GREEN.
!
!   Since the distance of the actual surface of integration
!   to the receiver point is used, DIST may never be zero in
!   forming the scattering matrix.
!
               IF (DIST<=1.*CLNGTH) THEN
                  MY=MCY*3/2
                  MZ=MCZ*3/2
               ELSE IF (DIST<=3.*CLNGTH) THEN
                  MY=MCY
                  MZ=MCZ
               ELSE
                  CLDST=SQRT(2.*CLNGTH/DIST)
                  MY=INT(FLOAT(MCY)*CLDST)
                  MZ=INT(FLOAT(MCZ)*CLDST)
               END IF
!
               IF (MY<1) MY=1
               IF (MZ<1) MZ=1
!
!-- A step number of 3 may not be more accuarate than the 2
!   used for the four-point approach to surface integrations
!
               IF (MY==3) MY=2
               IF (MZ==3) MZ=2
!
               IF (MY>1) CMY=MY-1.
               IF (MZ>1) CMZ=MZ-1.
!
!-- The four-point approach to the surface integration is preserved
!   from Xiong et al (1986)
!
               DO II=1,MY
                  IF (MY==1) THEN
                     Y1=YSRS
                     CY=1.
                  ELSE IF (MY==2) THEN
                     Y1=YSRS+2.*(II-1.5)*T3
                     CY=.5
                  ELSE
                     Y1=YSRS-.5*CLYS+CLYS/CMY*(II-1.)
                     CY=1./CMY
                     IF (II==1.OR.II==MY) CY=.5*CY
                  END IF
                  DO JJ=1,MZ
                     IF (MZ==1) THEN
                        Z1=ZSRS
                        CZ=1.
                     ELSE IF (MZ==2) THEN
                        Z1=ZSRS+2.*(JJ-1.5)*T4
                        CZ=.5
                     ELSE
                        Z1=ZSRS-.5*CLZS+CLZS/CMZ*(JJ-1.)
                        CZ=1./CMZ
                        IF (JJ==1.OR.JJ==MZ) CZ=.5*CZ
                     END IF
                     C1=C2*CY*CZ*CLYS*CLZS
!
                     R1=R(XOB,YOB,ZOB,X1,Y1,Z1)
!
                     CG=K*R1
                     G3=C1*((1.,0.)+CG)*EXP(-CG)/R1/R1/R1
                     B1=XOB-X1
                     D1=D1+B1*G3
                     B2=YOB-Y1
                     D2=D2+B2*G3
                     B3=ZOB-Z1
                     D3=D3+B3*G3
                  END DO
               END DO
            END DO
         END DO
         IF (L==1) THEN
            EXX=D1
            EXY=D2
            EXZ=D3
         ELSE
            EXX=EXX-D1
            EXY=EXY-D2
            EXZ=EXZ-D3
         END IF
!
      END DO
!
      DO L=1,3,2
!
         D1=(.0,.0)
         D2=(.0,.0)
         D3=(.0,.0)
         Y1=YSR-(FLOAT(L)-2.)*CLY*.5
!
!-- Note that the coordinates in the y-direction are fixed
!   and cells are divided only in the x and z directions
!
         DO ISUB=1,MSX
            IF (MSX==1) THEN
               XSRS=XSR
            ELSE
               XSRS=XSR+CLXS*(ISUB-1)-CLX/2.+.5*CLXS
            END IF
            DO KSUB=1,MSZ
               IF (MSZ==1) THEN
                  ZSRS=ZSR
               ELSE
                  ZSRS=ZSR+CLZS*(KSUB-1)-CLZ/2.+.5*CLZS
               END IF
!
!-- Determine the distance of the subcell surface over which the
!   integration is to be done to the receiver point with
!   respect to the dimension of the subcell surface.
!
               DIST=SQRT((RR(XOB,YOB,XSRS,Y1))**2+(ZOB-ZSRS)**2)
               CLNGTH=SQRT(CLXS**2+CLZS**2)
!
!-- Determine the numbers of steps according to the distance
!   between the source and the receiver
!
               IF (DIST<=1.*CLNGTH) THEN
                  MX=MCX*3/2
                  MZ=MCZ*3/2
               ELSE IF (DIST<=3.*CLNGTH) THEN
                  MX=MCX
                  MZ=MCZ
               ELSE
                  CLDST=SQRT(2.*CLNGTH/DIST)
                  MX=INT(FLOAT(MCX)*CLDST)
                  MZ=INT(FLOAT(MCZ)*CLDST)
               END IF
!
               IF (MX<1) MX=1
               IF (MZ<1) MZ=1
!
               IF (MX==3) MX=2
               IF (MZ==3) MZ=2
!
               IF (MX>1) CMX=MX-1.
               IF (MZ>1) CMZ=MZ-1.
!
               DO II=1,MX
                  IF (MX==1) THEN
                     X1=XSRS
                     CX=1.
                     CY=1.
                  ELSE IF (MX==2) THEN
                     X1=XSRS+2.*(II-1.5)*T2
                     CX=.5
                  ELSE
                     X1=XSRS-.5*CLXS+CLXS/CMX*(II-1.)
                     CX=1./CMX
                     IF (II==1.OR.II==MX) CX=.5*CX
                  END IF
                  DO JJ=1,MZ
                     IF (MZ==1) THEN
                        Z1=ZSRS
                        CZ=1.
                     ELSE IF (MZ==2) THEN
                        Z1=ZSRS+2.*(JJ-1.5)*T4
                        CZ=.5
                     ELSE
                        Z1=ZSRS-.5*CLZS+CLZS/CMZ*(JJ-1.)
                        CZ=1./CMZ
                        IF (JJ==1.OR.JJ==MZ) CZ=.5*CZ
                     END IF
                     C1=C2*CX*CZ*CLXS*CLZS
!
                     R1=R(XOB,YOB,ZOB,X1,Y1,Z1)
!
                     CG=K*R1
                     G3=C1*((1.,0.)+CG)*EXP(-CG)/R1/R1/R1
                     B1=XOB-X1
                     D1=D1+B1*G3
                     B2=YOB-Y1
                     D2=D2+B2*G3
                     B3=ZOB-Z1
                     D3=D3+B3*G3
                  END DO
               END DO
            END DO
         END DO
         IF (L==1) THEN
            EYX=D1
            EYY=D2
            EYZ=D3
         ELSE
            EYX=EYX-D1
            EYY=EYY-D2
            EYZ=EYZ-D3
         END IF
!
      END DO
!
      DO L=1,3,2
         D1=(.0,.0)
         D2=(.0,.0)
         D3=(.0,.0)
         Z1=ZSR-(FLOAT(L)-2.)*CLZ*.5
!
!-- Note that the coordinates in the z-direction are fixed
!   and cells are divided only in the x and y directions
!
         DO ISUB=1,MSX
            IF (MSX==1) THEN
               XSRS=XSR
            ELSE
               XSRS=XSR+CLXS*(ISUB-1)-CLX/2.+.5*CLXS
            END IF
            DO JSUB=1,MSY
               IF (MSY==1) THEN
                  YSRS=YSR
               ELSE
                  YSRS=YSR+CLYS*(JSUB-1)-CLY/2.+.5*CLYS
               END IF
!
!-- Determine the distance of the subcell surface over which the
!   integration is to be done to the receiver point with
!   respect to the dimension of the subcell surface.
!
               DIST=SQRT((RR(XOB,YOB,XSRS,YSRS))**2+(ZOB-Z1)**2)
               CLNGTH=SQRT(CLXS**2+CLYS**2)
!
!-- Determine the numbers of steps according to the distance
!   between the source and the receiver
!
               IF (DIST<=1.*CLNGTH) THEN
                  MX=MCX*3/2
                  MY=MCY*3/2
               ELSE IF (DIST<=3.*CLNGTH) THEN
                  MX=MCX
                  MY=MCY
               ELSE
                  CLDST=SQRT(2.*CLNGTH/DIST)
                  MX=INT(FLOAT(MCX)*CLDST)
                  MY=INT(FLOAT(MCY)*CLDST)
               END IF
!
               IF (MX<1) MX=1
               IF (MY<1) MY=1
!
               IF (MX==3) MX=2
               IF (MY==3) MY=2
!
               IF (MX>1) CMX=MX-1.
               IF (MY>1) CMY=MY-1.
!
               DO II=1,MX
                  IF (MX==1) THEN
                     X1=XSRS
                     CX=1.
                  ELSE IF (MX==2) THEN
                     X1=XSRS+2.*(II-1.5)*T2
                     CX=.5
                  ELSE
                     X1=XSRS-.5*CLXS+CLXS/CMX*(II-1.)
                     CX=1./CMX
                     IF (II==1.OR.II==MX) CX=.5*CX
                  END IF
                  DO JJ=1,MY
                     IF (MY==1) THEN
                        Y1=YSRS
                        CY=1.
                     ELSE IF (MY==2) THEN
                        Y1=YSRS+2.*(JJ-1.5)*T3
                        CY=.5
                     ELSE
                        Y1=YSRS-.5*CLYS+CLYS/CMY*(JJ-1.)
                        CY=1./CMY
                        IF (JJ==1.OR.JJ==MY) CY=.5*CY
                     END IF
                     C1=C2*CX*CY*CLXS*CLYS
!
                     R1=R(XOB,YOB,ZOB,X1,Y1,Z1)
!
                     CG=K*R1
                     G3=C1*((1.,0.)+CG)*EXP(-CG)/R1/R1/R1
                     B1=XOB-X1
                     D1=D1+B1*G3
                     B2=YOB-Y1
                     D2=D2+B2*G3
                     B3=ZOB-Z1
                     D3=D3+B3*G3
                  END DO
               END DO
            END DO
         END DO
         IF (L==1) THEN
            EZX=D1
            EZY=D2
            EZZ=D3
         ELSE
            EZX=EZX-D1
            EZY=EZY-D2
            EZZ=EZZ-D3
         END IF
!
      END DO
!
      EXX=EXX+GX
      EYY=EYY+GY
      EZX=EZX+GZX
      EZY=EZY+GZY
      EZZ=EZZ+GZ
!
! ----- Store the values for a self cell upon computation
!
      IF (KSELF==1.AND.RS<DMIN) THEN
         EXX1=EXX
         EYX1=EYX
         EZX1=EZX
         EXY1=EXY
         EYY1=EYY
         EZY1=EZY
         EXZ1=EXZ
         EYZ1=EYZ
         EZZ1=EZZ
         GAX1=GAX
      END IF
!
! --- Assign the componenets to the arrays ECOMP and HCOMP
!   for data passing
!
!**** Entry point for goto 500
!
  100 ECOMP(1)=EXX
      ECOMP(2)=EYX
      ECOMP(3)=EZX
      ECOMP(4)=EXY
      ECOMP(5)=EYY
      ECOMP(6)=EZY
      ECOMP(7)=EXZ
      ECOMP(8)=EYZ
      ECOMP(9)=EZZ
!
      RETURN
!
      END
!
!**** End of THR_D_GEPRM
!
      REAL FUNCTION R(XR,YR,ZR,XI,YI,ZI)
         IMPLICIT NONE
         REAL XR,XI,YR,YI,ZR,ZI
         R=SQRT((XR-XI)**2+(YR-YI)**2+(ZR-ZI)**2)
      END
      REAL FUNCTION RR(XR,YR,XI,YI)
         IMPLICIT NONE
         REAL XR,XI,YR,YI
         RR=SQRT((XR-XI)**2+(YR-YI)**2)
      END
!
!
 SUBROUTINE THR_D_GASELF(CLX,CLY,CLZ,MXINT,MYINT,MZINT,CDH,K,DMIN,GX,GZ)
!----------------------------------------------------------------------
!
!****  Integration of the current term for a self-cell
!
!   The prism cell is divided into 27 subcells with the one
!   in the centre being a cube for the approximation by a sphere.
!   Of the 27 subcells either 18 or 24 have zero size.
!
!
! Input parameters:
!
!   CLX:    Real,  dimension of the cell in the x-direction.
!   CLY:    Real,  dimension of the cell in the y-direction.
!   CLZ:    Real,  dimension of the cell in the z-direction.
!   MXINT:  Integer,  number of steps in the x-direction.
!   MYINT:  Integer,  number of steps in the y-direction.
!   MZINT:  Integer,  number of steps in the z-direction.
!   CDH:    Complex CDH(0:MLAYER),  complex conductivities of
!           the layers including the air (layer 0).
!   K:      Complex,  wave number of the layer which is considered as
!           a whole space for primary parts.
!   DMIN:   Real,  minmum of dimension for numerical treatments of cells.
!           DMIN=0.1 for the low frequency module and DMIN=0.001 for the
!           high frequency module.
!
! Output parameters:
!
!   GX:     Complex, the x- (and y-) component of the primay current
!           term due to the selff-cell.
!   GZ:     Complex, the z- component of the primay current term due
!           to the self-cell.  It differs from GX if the medium is
!           anisotropic.
!
!
!**** Called by:  THR_D_GEPRM
!
!**** Calls:      THR_D_G12
!
!
      IMPLICIT NONE
!
      REAL CLX,CLY,CLZ,CMX,CMY,CMZ,CX,CY,CZ,R,R1,SL,X1,Y1,Z1
      INTEGER I,II,J,JJ,L,LL,MX,MXINT,MY,MYINT,MZ,MZINT
      COMPLEX GX,GZ,G1,K,CDH,C1,C2
      REAL  DMIN,X(3),Y(3),Z(3),XL(3),YL(3),ZL(3)
!
!      R(XR,YR,ZR,XI,YI,ZI)=SQRT((XR-XI)**2+(YR-YI)**2+(ZR-ZI)**2)
!
      SL=MIN(CLX,CLY,CLZ)
      XL(1)=(CLX-SL)/2.
      XL(2)=SL
      XL(3)=(CLX-SL)/2.
      YL(1)=(CLY-SL)/2.
      YL(2)=SL
      YL(3)=(CLY-SL)/2.
      ZL(1)=(CLZ-SL)/2.
      ZL(2)=SL
      ZL(3)=(CLZ-SL)/2.
      X(1)=-(SL/2.+XL(1)/2.)
      X(2)=0.
      X(3)=(SL/2.+XL(3)/2.)
      Y(1)=-(SL/2.+YL(1)/2.)
      Y(2)=0.
      Y(3)=(SL/2.+YL(3)/2.)
      Z(1)=-(SL/2.+ZL(1)/2.)
      Z(2)=0.
      Z(3)=(SL/2.+ZL(3)/2.)

      GX=(.0,.0)
      DO I=1,3
         DO J=1,3
            DO L=1,3
!
!**** Skip the centre cell and cells with a zero dimension
!
               IF (.NOT.(I==2.AND.J==2.AND.L==2.OR.ABS(XL(I))<=DMIN.OR.  &
                   ABS(YL(J))<=DMIN.OR.ABS(ZL(L))<=DMIN)) THEN
!
                  C2=-K**2/CDH*XL(I)*YL(J)*ZL(L)
!
                  MX=INT(FLOAT(MXINT)*(XL(I)/CLX))
                  IF (MX<1) MX=1
                  MY=INT(FLOAT(MYINT)*(YL(J)/CLY))
                  IF (MY<1) MY=1
                  MZ=INT(FLOAT(MZINT)*(ZL(L)/CLZ))
                  IF (MZ<1) MZ=1
!
                  IF (MX>1) THEN
                     CMX=MX-1.
                  ELSE
                     CMX=1.
                  END IF
                  IF (MY>1) THEN
                     CMY=MY-1.
                  ELSE
                     CMY=1.
                  END IF
                  IF (MZ>1) THEN
                     CMZ=MZ-1.
                  ELSE
                     CMZ=1.
                  END IF
!
                  DO II=1,MX
                     X1=X(I)-.5*XL(I)+XL(I)/CMX*(II-1.)
                     IF (MX>1) THEN
                        CX=1./CMX
                     ELSE
                        CX=1.
                     END IF
                     IF (II==1.OR.II==MX) CX=.5*CX
                     IF (MX==1) CX=1.
                     DO JJ=1,MY
                        Y1=Y(J)-.5*YL(J)+YL(J)/CMY*(JJ-1.)
                        IF (MY>1) THEN
                           CY=1./CMY
                        ELSE
                           CY=1.
                        END IF
                        IF (JJ==1.OR.JJ==MY) CY=.5*CY
                        IF (MY==1) CY=1.
                        DO LL=1,MZ
                           Z1=Z(L)-.5*ZL(L)+ZL(L)/CMZ*(LL-1.)
                           IF (MZ>1) THEN
                              CZ=1./CMZ
                           ELSE
                              CZ=1.
                           END IF
                           IF (LL==1.OR.LL==MZ) CZ=.5*CZ
                           IF (MZ==1) CZ=1.
                           C1=C2*CX*CY*CZ
!
                           R1=R(0.,0.,0.,X1,Y1,Z1)
                           G1=.0795774771*EXP(-K*R1)/R1
                           GX=GX+G1*C1
!
                        END DO
                     END DO
                  END DO
               END IF
!
            END DO
         END DO
      END DO
      CALL THR_D_G12(SL,SL,SL,CDH,K,G1)
      GX=GX+G1
      GZ=GX
!
      END
!
!**** End of THR_D_GASELF
!
!
      SUBROUTINE THR_D_G12(CLX,CLY,CLZ,CDH,K,G1)
!
!**** Approximate the volumn integration of the primary current term
!   due to a self-cell by the analytical solution for a sphere.
!
!
! Input parameters:
!
!   CLX:    Real,  dimension of the cell in the x-direction.
!   CLY:    Real,  dimension of the cell in the y-direction.
!   CLZ:    Real,  dimension of the cell in the z-direction.
!   CDH:    Complex CDH(0:MLAYER),  complex conductivities of
!           the layers including the air (layer 0).
!   K:      Complex,  wave number of the layer which is considered as
!           a whole space for primary parts.
!
! Output parameters:
!
!   G1:     Complex, the x, y & z component of the primay current
!           term due to the selff-cell approximated by a sphere.
!
!**** Called by:  THR_D_GASELF
!
!**** Calls:      None
!
!
      IMPLICIT NONE
!
      REAL AV,CLX,CLY,CLZ,VOLUME
      COMPLEX G1,K,CDH
      VOLUME=CLX*CLY*CLZ
      AV=.6207505*VOLUME**.33333333
      G1= (EXP(-K*AV)*(K*AV+(1.,0.))-(1.,0.))/CDH

      END
!
!**** End of THR_D_G12
!
!
      SUBROUTINE SCAT_EH_CSGS(NBMAX,NEQ,NXMAX,NYMAX,NZMAX,NBODY,SUB_BLOCK,    &
                              NET,N_RX,RX_X,RX_Y,RX_Z,NX,NY,NZ,NCELL,X,Y,     &
                              Z,CLX,CLY,CLZ,JS,IEXCI,CLMN,M_RX,               &
                              NTX_FD,EAX,EAY,EAZ,HAX,HAY,HAZ,                 &
                              FRQ,MLAYER,ZBND,RMU,CDH,NZOB,ZOBG,NZSR,     &
                              ZSRG,RHOMIN,DMIN,NHFILM,RRG,NRG,GRHF,RRG3,      &
                              NRG3,GRHF3,GRHO0,GRHO03,KCLMN,BLMIN,KACC)
!
!**** Calculate the scattered fields at receiver sites
!   due to symmetric structures
!
!   This routine is similar to the routine SCAT_EH_CS. The numbering
!   of the array JS is completely different.
!
!   Note if the receiver is located within a cell, the electric field
!   is then calculated by the scattering current directly.
!
!   Note the controlling parameter koutcrp was originally designed for
!   outcropping structures so that it bears this named.
!
!   The array clmn should have the size of mbody. Since mbody
!   may be changed in the main program, here it is defined as 30.
!
!
      IMPLICIT NONE
!
      REAL BLMIN,BX,BY,CLMN2,CLX1,CLY1,CLZ1,DMIN,FRQ,RHOMIN
      INTEGER I,IB,IEXCI,II,IQ,J,K,KACC,KCLMN,KEYG,KSELF,KUTCRP,L,      &
              MLAYER,M_RX,NBMAX,NBODY,NEQ
      INTEGER NHFILM,NJ,NN,NN1,NRG,NRG3,NSUB,NTX_FD,NXMAX,              &
              NYMAX,NZMAX,NZOB,NZSR
!
      COMPLEX JS(4*NEQ),EX,EY,EZ,HX,HY,HZ,                              &
              EXX,EYX,EZX,EXY,EYY,EZY,EXZ,EYZ,EZZ,                      &
              HXX,HYX,HZX,HXY,HYY,HZY,HXZ,HYZ,HZZ,                      &
              EAX(M_RX),EAY(M_RX),EAZ(M_RX),                            &
              HAX(M_RX),HAY(M_RX),HAZ(M_RX),                            &
              CDH(0:MLAYER),                                            &
              GRHF(11,NHFILM,NZSR,NZOB),GRHO0(4,NZSR,NZOB),             &
              GRHF3(11,NHFILM,NZSR),GRHO03(4,NZSR)
      INTEGER NX(NBMAX,NBODY),NY(NBMAX,NBODY),NZ(NBMAX,NBODY),          &
              NCELL(NBMAX,NBODY),SUB_BLOCK(NBODY),NET(NBODY)
      REAL  X(NXMAX,NBMAX,NBODY),Y(NYMAX,NBMAX,NBODY),                  &
            Z(NZMAX,NBMAX,NBODY),CLMN(NBODY),                           &
            CLX(NBMAX,NBODY),CLY(NBMAX,NBODY),CLZ(NBMAX,NBODY),         &
            RX_X(M_RX,NTX_FD),RX_Y(M_RX,NTX_FD),RX_Z(M_RX,NTX_FD),      &
            ZBND(0:MLAYER),RMU(0:MLAYER),                 &
            ZOBG(NZOB),ZSRG(2,NZSR),RRG(NRG),RRG3(NRG3),PSTION(6)
      INTEGER N_RX(NTX_FD)
      COMPLEX ECOMP(9),HCOMP(9)
      EQUIVALENCE (ECOMP(1),EXX),(ECOMP(2),EYX),(ECOMP(3),EZX),   &
                  (ECOMP(4),EXY),(ECOMP(5),EYY),(ECOMP(6),EZY),   &
                  (ECOMP(7),EXZ),(ECOMP(8),EYZ),(ECOMP(9),EZZ),   &
                  (HCOMP(1),HXX),(HCOMP(2),HYX),(HCOMP(3),HZX),   &
                  (HCOMP(4),HXY),(HCOMP(5),HYY),(HCOMP(6),HZY),   &
                  (HCOMP(7),HXZ),(HCOMP(8),HYZ),(HCOMP(9),HZZ)
!
      KSELF=0
      KEYG=2
      KUTCRP=0
!
      DO II=1,N_RX(IEXCI)
!
         EX=(0.,0.)
         EY=(0.,0.)
         EZ=(0.,0.)
         HX=(0.,0.)
         HY=(0.,0.)
         HZ=(0.,0.)
!
         NSUB=0
         DO IB=1,NBODY
            IF (IB>1) NSUB=NSUB+NET(IB-1)/3
!
            CLMN2=CLMN(IB)
!
            NJ=0
            DO L=1,SUB_BLOCK(IB)
               IF (L>1) NJ=NJ+NCELL(L-1,IB)
               DO I=1,NX(L,IB)
                  DO J=1,NY(L,IB)
                     DO K=1,NZ(L,IB)
                        DO IQ=1,4
!
                           IF (IQ==1) THEN
                              BX=-1.
                              BY=-1.
                           END IF
                           IF (IQ==2) THEN
                              BX=1.
                              BY=-1.
                           END IF
                           IF (IQ==3) THEN
                              BX=1.
                              BY=1.
                           END IF
                           IF (IQ==4) THEN
                              BX=-1.
                              BY=1.
                           END IF
!
                           NN1=(I-1)*NY(L,IB)*NZ(L,IB)+(J-1)*NZ(L,IB)+K
                           NN=12*(NN1+NJ+NSUB-1)+3*(IQ-1)+1
                           CLX1=CLX(L,IB)
                           CLY1=CLY(L,IB)
                           CLZ1=CLZ(L,IB)
!
                           PSTION(1)=RX_X(II,IEXCI)
                           PSTION(2)=RX_Y(II,IEXCI)
                           PSTION(3)=RX_Z(II,IEXCI)
                           PSTION(4)=BX*X(I,L,IB)
                           PSTION(5)=BY*Y(J,L,IB)
                           PSTION(6)=Z(K,L,IB)
                           CALL THR_D_GREEN(KEYG,PSTION,FRQ,MLAYER,ZBND,CDH,  &
                              RMU,NZOB,ZOBG,NZSR,ZSRG,RHOMIN,             &
                              DMIN,NHFILM,RRG,NRG,GRHF,RRG3,NRG3,GRHF3,       &
                              GRHO0,GRHO03,CLX1,CLY1,CLZ1,CLMN2,KCLMN,        &
                              BLMIN,KACC,KUTCRP,KSELF,ECOMP,HCOMP)
!
                           EX=EX+(JS(NN)*EXX+JS(NN+1)*EXY+JS(NN+2)*EXZ)
                           EY=EY+(JS(NN)*EYX+JS(NN+1)*EYY+JS(NN+2)*EYZ)
                           EZ=EZ+(JS(NN)*EZX+JS(NN+1)*EZY+JS(NN+2)*EZZ)
                           HX=HX+(JS(NN)*HXX+JS(NN+1)*HXY+JS(NN+2)*HXZ)
                           HY=HY+(JS(NN)*HYX+JS(NN+1)*HYY+JS(NN+2)*HYZ)
                           HZ=HZ+(JS(NN)*HZX+JS(NN+1)*HZY+JS(NN+2)*HZZ)
                        END DO
                     END DO
                  END DO
               END DO
            END DO
         END DO
!
         EAX(II)=EX
         EAY(II)=EY
         EAZ(II)=EZ
         HAX(II)=HX
         HAY(II)=HY
         HAZ(II)=HZ
      END DO
!
      RETURN
      END
!
!
      SUBROUTINE SCAT_EH_CS(NBMAX,NMAX,NXMAX,NYMAX,NZMAX,NBODY,SUB_BLOCK,    &
                            N_RX,RX_X,RX_Y,RX_Z,NX,NY,NZ,NCELL,X,Y,Z,CLX,    &
                            CLY,CLZ,JS,IEXCI,CLMN,M_RX,NTX_FD,               &
                            EAX,EAY,EAZ,HAX,HAY,HAZ,FRQ,MLAYER,              &
                            ZBND,RMU,CDH,NZOB,ZOBG,NZSR,ZSRG,RHOMIN,     &
                            DMIN,NHFILM,RRG,NRG,GRHF,RRG3,NRG3,GRHF3,GRHO0,  &
                            GRHO03,KCLMN,BLMIN,KACC)
!
!**** Calculate scattered fields at receiver sites
!
!   Note if the receiver is located within a cell, the electric field
!   is then calculated by the scattering current directly.
!
!   Note the controlling parameter koutcrp was originally designed for
!   outcropping structures so that it bears this named.
!
!   The array clmn should have the size of mbody. Since mbody
!   may be changed in the main program, here it is defined as 30.
!
!   Parameters  FRQ,MLAYER,ZBND,
!             CDH,NZOB,ZOBG,NZSR,ZSRG,
!             RHOMIN,DMIN,NHFILM,RRG,GRHF,GRHF3,GRHO0,
!             GRHO03,KCLMN,BLMIN,
!             KACC,  are to be passed to routine THR_D_GREEN
!
!
      IMPLICIT NONE
!
      REAL BLMIN,CLMN2,CLX1,CLY1,CLZ1,DMIN,FRQ,RHOMIN
      INTEGER I,IB,IEXCI,II,J,K,KACC,KCLMN,KEYG,KSELF,KUTCRP,L,MLAYER,   &
              M_RX,NBMAX,NBODY,NHFILM
      INTEGER NJS,NMAX,NN,NN1,NRG,NRG3,NTX_FD,NXMAX,NYMAX,               &
              NZMAX,NZOB,NZSR
!
      COMPLEX JS(NMAX,NBODY),EX,EY,EZ,HX,HY,HZ,                          &
              EXX,EYX,EZX,EXY,EYY,EZY,EXZ,EYZ,EZZ,                       &
              HXX,HYX,HZX,HXY,HYY,HZY,HXZ,HYZ,HZZ,                       &
              EAX(M_RX),EAY(M_RX),EAZ(M_RX),                             &
              HAX(M_RX),HAY(M_RX),HAZ(M_RX),                             &
              CDH(0:MLAYER),                                             &
              GRHF(11,NHFILM,NZSR,NZOB),GRHO0(4,NZSR,NZOB),              &
              GRHF3(11,NHFILM,NZSR),GRHO03(4,NZSR)
      INTEGER NX(NBMAX,NBODY),NY(NBMAX,NBODY),NZ(NBMAX,NBODY),           &
              NCELL(NBMAX,NBODY),SUB_BLOCK(NBODY)
      REAL  X(NXMAX,NBMAX,NBODY),Y(NYMAX,NBMAX,NBODY),                   &
            Z(NZMAX,NBMAX,NBODY),CLMN(NBODY),                            &
            CLX(NBMAX,NBODY),CLY(NBMAX,NBODY),CLZ(NBMAX,NBODY),          &
            RX_X(M_RX,NTX_FD),RX_Y(M_RX,NTX_FD),RX_Z(M_RX,NTX_FD),       &
            ZBND(0:MLAYER),ZOBG(NZOB),ZSRG(2,NZSR),        &
            RRG(NRG),RRG3(NRG3),PSTION(6),RMU(0:MLAYER)
      INTEGER N_RX(NTX_FD)
      COMPLEX ECOMP(9),HCOMP(9)
      EQUIVALENCE (ECOMP(1),EXX),(ECOMP(2),EYX),(ECOMP(3),EZX),   &
                  (ECOMP(4),EXY),(ECOMP(5),EYY),(ECOMP(6),EZY),   &
                  (ECOMP(7),EXZ),(ECOMP(8),EYZ),(ECOMP(9),EZZ),   &
                  (HCOMP(1),HXX),(HCOMP(2),HYX),(HCOMP(3),HZX),   &
                  (HCOMP(4),HXY),(HCOMP(5),HYY),(HCOMP(6),HZY),   &
                  (HCOMP(7),HXZ),(HCOMP(8),HYZ),(HCOMP(9),HZZ)
!
      KSELF=0
      KEYG=2
      KUTCRP=0
!
      DO II=1,N_RX(IEXCI)
!
         EX=(0.,0.)
         EY=(0.,0.)
         EZ=(0.,0.)
         HX=(0.,0.)
         HY=(0.,0.)
         HZ=(0.,0.)
!
         DO IB=1,NBODY
!
            CLMN2=CLMN(IB)
!
            NJS=0
            DO L=1,SUB_BLOCK(IB)
               IF (L>1) NJS=NJS+3*NCELL(L-1,IB)
               DO I=1,NX(L,IB)
                  DO J=1,NY(L,IB)
                     DO K=1,NZ(L,IB)
                        NN1=(I-1)*NY(L,IB)*NZ(L,IB)+(J-1)*NZ(L,IB)+K
                        NN=(NN1-1)*3+1+NJS
                        CLX1=CLX(L,IB)
                        CLY1=CLY(L,IB)
                        CLZ1=CLZ(L,IB)
!
                        PSTION(1)=RX_X(II,IEXCI)
                        PSTION(2)=RX_Y(II,IEXCI)
                        PSTION(3)=RX_Z(II,IEXCI)
                        PSTION(4)=X(I,L,IB)
                        PSTION(5)=Y(J,L,IB)
                        PSTION(6)=Z(K,L,IB)
                        CALL THR_D_GREEN(KEYG,PSTION,FRQ,MLAYER,ZBND,CDH,RMU, &
                           NZOB,ZOBG,NZSR,ZSRG,RHOMIN,DMIN,               &
                           NHFILM,RRG,NRG,GRHF,RRG3,NRG3,GRHF3,GRHO0,         &
                           GRHO03,CLX1,CLY1,CLZ1,CLMN2,KCLMN,BLMIN,KACC,      &
                           KUTCRP,KSELF,ECOMP,HCOMP)
!
                        EX=EX+(JS(NN,IB)*EXX+JS(NN+1,IB)*EXY+JS(NN+2,IB)  &
                           *EXZ)
                        EY=EY+(JS(NN,IB)*EYX+JS(NN+1,IB)*EYY+JS(NN+2,IB)  &
                           *EYZ)
                        EZ=EZ+(JS(NN,IB)*EZX+JS(NN+1,IB)*EZY+JS(NN+2,IB)  &
                           *EZZ)
                        HX=HX+(JS(NN,IB)*HXX+JS(NN+1,IB)*HXY+JS(NN+2,IB)  &
                           *HXZ)
                        HY=HY+(JS(NN,IB)*HYX+JS(NN+1,IB)*HYY+JS(NN+2,IB)  &
                           *HYZ)
                        HZ=HZ+(JS(NN,IB)*HZX+JS(NN+1,IB)*HZY+JS(NN+2,IB)  &
                           *HZZ)
                     END DO
                  END DO
               END DO
            END DO
         END DO
!
         EAX(II)=EX
         EAY(II)=EY
         EAZ(II)=EZ
         HAX(II)=HX
         HAY(II)=HY
         HAZ(II)=HZ
      END DO

      RETURN
      END

 SUBROUTINE HFILL (KEYG,KEMD,MLAYER,ZBND,LRYTH,KKH,RMU,KPRM,KITG,KCHRG,   &
                   NOB,NSR,RLO,RHI,ZOB,ZSRH,ZSRL,NRG,RRG,NHFILM,HF)
!----------------------------------------------------------------------

!     HANKEL TRANSFORMATIONS BY DIGITAL LINEAR FILTERING
!      for computing normal responses
!
!      The filters used here yield an accuracy of about 0.4E-8.
!
!      This routine is based on a filtering routine provided by
!      N. Christensen.  Though the routine has been restructured,
!      it still retains the basic original designs of Christensen
!      because of their efficience.
!
!   Input parameters:
!
!      RLO:  real (*4), minimum of rho.
!      RHI:  real     , maximum of rho.
!
!      The parameters KEMD, MLAYER, ZBND, LRYTH,  KKH,  KPRM,
!      KITG, KCHRG, NOB, NSR, ZOB,  ZSRH and ZSRL are to be passed
!      to routine kernel.  See routine kernel for their descriptions.
!
!
!   Output parameters:
!
!      NRG:  integer (*4), number of data point returned.
!      RRG:  real RRG(nhfil), the discrete values of rho on
!            logrithmic scale on which results of the Hankel
!            transforms are returned.
!      HF:   complex(11,nhfil), the nhfil columns of the
!            11 components of the Hankel transforms of the
!            Green's tensors.  Note that the values contained
!            in HF must be divided by rho in order to get the
!            actual Hankel transforms.
!
!****  CALLED by:  gridhf, gridcs
!
!****  CALLS    :  kernel

 USE MA_Filter_coefficients


 IMPLICIT NONE
 REAL, PARAMETER :: EPS=1.E-9, TOL1=1.E-28
 INTEGER, INTENT(IN) :: KEYG,KEMD,MLAYER,KPRM,KITG,KCHRG,NOB,NSR,NHFILM
 INTEGER, INTENT(OUT) :: NRG
 REAL, INTENT(IN) :: RLO,RHI
 REAL, INTENT(INOUT) :: RRG(NHFILM)
 COMPLEX, INTENT(INOUT) :: HF(11,NHFILM)

!  -- NC used to be defined as nc(nhfilm),  It now becomes fixed since
!     arrasy hf etc are defined in the calling routine to make array lengths
!     consistent.  91 should cover 100 km.

 INTEGER ::  NC(91),INR(491),NLO,NHI,NF,I0G,IGSH,NLIM,KRHO,I,IG,IJ,J,K
 REAL :: DEL,R1,E,R,X
 COMPLEX :: FC0(5,491),FC1(6,491),                     &
            SUM0(5),SUM1(6),SDEL0(5),SDEL1(6),FKN(11)


!  -- Parameters to be passed to the routine kernel

 COMPLEX, INTENT(IN) :: KKH(0:MLAYER)
 REAL, INTENT(IN) :: LRYTH(MLAYER),ZBND(0:MLAYER),ZOB,ZSRH,ZSRL,RMU(0:MLAYER)

!  -- Parameter KRHO is required by routine ONE_D_KERNEL.
!     KRHO=1 means rho (or R in this routine) .ne. zero
!  -- Inititalizations
!     NLO, NHI, NRG, R1,  and E are determined by DEL,  RLO,  and RHI
!
!  -- Initialise FKN to avoid compiler errors for some compilers
!
      FKN=(0.,0.)
!
 KRHO=1

 DEL = LOG (10.) /15.
 NLO=INT (LOG (RLO)/DEL + 100.) - 101
 IF (NLO < -15) NLO = -15
 NHI=INT (LOG (RHI) / DEL + 100.) - 98
 IF (NHI > 75) NHI = 75
 NRG = NHI - NLO + 1
 R1 = EXP (REAL (NLO) * DEL)
 E = EXP (DEL)

!  --  NG=491 is the dimension of the arrays INR, FC0,  and FC1

 NLIM=60; NF=401; I0G=326; INR=-1

 R=R1/E
 IGSH=INT(ALOG(R)/DEL+100.5)-100

 R_loop:  DO K = 1 ,NRG !  This loop computes the discreted values of
   R=R*E                !  the Hankel integrals at discreted points of
   IGSH=IGSH+1                              !  R on a logarithmic scale determined by DEL.
   X =  E/R* EXP(-.14314998)                !  All transforms are returned simultaneously.
   SUM0 = (0.,0.);  SUM1 = (0.,0.)          ! --- The evaluation of IGSH has been moved to the top of the do loop
                                            !     since the value of R may not be very accurate if it is large.
   LC_LP:  DO I=1,NF                    ! IGSH=INT(ALOG(R)/DEL+100.5)-100
     IJ = - I + 1                       ! Calculation of convolution, arg. of kernel to zero
     IF (IJ < JNLO) EXIT                ! Check for limits to convolution, arg. of kernel to zero.
     X = X / E                          ! Exit if the IJ runs out of the filters range
     IG = I0G - IGSH - I + 1
      IF (INR(IG) < 0) THEN
        CALL ONE_D_KERNEL (KEYG,KEMD,MLAYER,ZBND,LRYTH,KKH,RMU, &
                  KPRM,KRHO,KITG,KCHRG,NOB,NSR,ZOB,(ZSRH+ZSRL)/2.,   &
                  ZSRH,ZSRL,X,FKN)
        FC0(1:3,IG) = FKN(1:3)
        FC0(4:5,IG) = FKN(7:8)
        FC1(1:3,IG) = FKN(4:6)
        FC1(4:6,IG) = FKN(9:11)
        INR(IG)=1
      END IF

      DO J=1,5
        SDEL0(J) = WJ0(IJ)*FC0(J,IG)
        SDEL1(J) = WJ1(IJ)*FC1(J,IG)
        SUM0(J) = SUM0(J)+SDEL0(J)
        SUM1(J) = SUM1(J)+SDEL1(J)
      END DO
      SDEL1(6) = WJ1(IJ) * FC1(6,IG)
      SUM1(6) = SUM1(6) + SDEL1(6)

      IF (I > NLIM) THEN
        DO J = 1, 5
          IF (ABS (SDEL0(J)) > (TOL1 + ABS (SUM0(J))) * EPS) CYCLE LC_LP
          IF (ABS (SDEL1(J)) > (TOL1 + ABS (SUM1(J))) * EPS) CYCLE LC_LP
        END DO
        IF (ABS (SDEL1(6)) > (TOL1 + ABS (SUM1(6))) * EPS) CYCLE LC_LP

        EXIT  ! lower convergence

      END IF

    END DO LC_LP

    NC(K) = I
    X = 1. /R * EXP(-.14314998)

    UC_LP:  DO I=1,NF

      IF (I > JNHI) EXIT  ! exahausted filter

      X = X * E
      IG = I0G - IGSH + I
      IF (INR(IG) < 0) THEN
        CALL ONE_D_KERNEL(KEYG,KEMD,MLAYER,ZBND,LRYTH,KKH,RMU,KPRM,KRHO,KITG,  &
                         KCHRG,NOB,NSR,ZOB,(ZSRH+ZSRL)/2.,ZSRH,ZSRL,X,FKN)
        FC0(1:3,IG) = FKN(1:3)
        FC0(4:5,IG) = FKN(7:8)
        FC1(1:3,IG) = FKN(4:6)
        FC1(4:6,IG) = FKN(9:11)
        INR(IG)=1
      END IF

      DO J=1,5
        SDEL0(J) = WJ0(I)*FC0(J,IG)
        SDEL1(J) = WJ1(I)*FC1(J,IG)
        SUM0(J) = SUM0(J)+SDEL0(J)
        SUM1(J) = SUM1(J)+SDEL1(J)
      END DO

      SDEL1(6) = WJ1(I) * FC1(6,IG)
      SUM1(6) = SUM1(6) + SDEL1(6)

      DO J = 1, 5
        IF (ABS(SDEL0(J)) > (TOL1 + ABS (SUM0(J))) * EPS) CYCLE UC_LP
        IF (ABS(SDEL1(J)) > (TOL1 + ABS (SUM1(J))) * EPS) CYCLE UC_LP
      END DO

      IF (ABS(SDEL1(6)) > (TOL1 + ABS (SUM1(6))) * EPS) CYCLE UC_LP

      EXIT  ! upper convergence

    END DO UC_LP

    NC(K)=NC(K)+I

    HF(1:3,K)=SUM0(1:3)
    HF(7:8,K)=SUM0(4:5)
    HF(4:6,K)=SUM1(1:3)
    HF(9:11,K)=SUM1(4:6)

    RRG(K)=R

  END DO R_loop
 END SUBROUTINE HFILL

