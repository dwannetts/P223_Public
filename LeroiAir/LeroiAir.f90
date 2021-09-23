!   PROGRAM LeroiAir
!-----------------------------------------------------------------------------------------
Module LA_Metadata
!--------------
!
! Module designed to hold 'constant character options'
    Implicit NONE

    Character (Len = 40), Parameter :: PNAME = 'LeroiAir'
    Character (Len = 40), Parameter :: PVERS = '5.5.2'
    Character (Len = 40), Parameter :: PDATE = '08 February, 2021'
    Character (Len = 40), Parameter :: PAUT1 = 'CSIRO Electromagnetic Modelling Group'
    Character (Len = 40), Parameter :: PAUT2 = 'Art Raiche, Fred Sugeng and Glenn Wilson'
    Character (Len = 40), Parameter :: PAUT3 = 'David Annetts (david.annetts@csiro.au)'
    Character (Len = 40), Parameter :: PPROJ = 'AMIRA P223F'
    Character (Len = 80), Parameter :: PRELS = 'GPL-2.0 (Full text at https://opensource.org/licenses/gpl-2.0.php)'

End Module LA_Metadata
!-----------------------------------------------------------------------------------------
!
!
!            Develped by:  CSIRO Electromagnetic Modelling Group
!                          Art Raiche, Fred Sugeng & Glenn Wilson
!
!                    For:  AMIRA project P223F
!
! Changes
! 5.5.4
! 1.	Added model to mf1 file
! 5.5.3
! 1.    Move to  standardised versioning & output via Module LA_Metadata
!
!================
!   DESCRIPTION |
!================
!
!  LeroiAir can be used for inversion or modelling
!
!*************************
!      THE MODEL
!*************************
!
!  LeroiAir is used for modelling the response of one or more fully interacting
!  3D thin plate targets in a horizontally layered host.  At present,
!  the target(s) are restricted to be in the basement of an N-layered host.
!F
!  In theory, the term "thin plate" means zero thickness, ie, a conductance
!  of zero volume imposed upon the host region.  This implies that all the
!  induced currents lie in the planes defined by the plates.  In practice, the
!  thin plate algorithm will be accurate when the target is electrically thin;
!  ie, when its thickness is a fraction of the incident wavelength.
!
!  A 5 m thick, dike of high conductance will not be a thin target at high
!  frequencies or equivalently early delay times.  A 40 m thick moderately
!  conducting dyke can be regarded as electrically thin, especially at lower
!  frequencies.  For time-domain modelling or inversion, the range of
!  applicability of LeroiAir will be extended due to the fact that conductive
!  hosts filter out high frequencies.
!
!  The reference point of each plate is defined as the midpoint of its top
!  edge.  In the case of a flat lying plate, the midpoint of the western edge
!  is defined as the reference point.  Each plate is defined by 9 parameters:
!  conductance, the east & north coordinates of the reference point and its
!  relative level, the strike length and width of the plate and its strike
!  azimuth, dip and rotation.
!
!
!   Geometry Conventions
!   --------------------
!
!   Transmitter and receiver coordinates are specified in terms of Easting,
!   Northing and RLs (relative level) which increases negatively downwards.
!
!   Initially, each plate starts as a north-south oriented horizontal plate.
!   The designated edge is defined as the western edge.  The plate reference
!   point is the midpoint of this edge.  The user enters the coordinates of
!   this point in terms of Easting, Northing and RL.
!
!   The plate is oriented by first applying a DIP using the designated edge as
!   the hinge.  ( 0 <= DIP < 180 )  A flat-lying plate has DIP = 0.
!
!   lastly, AZM, the plate strike angle or azimuth is applied as a rotation
!   about the vertical axis passing through the plate reference point.  AZM
!   refers to the angle that the designated edge makes with the NORTH axis.
!      ( -180 <= AZM <= 180 )
!
!   DIP is positive clockwise down when facing in the strike direction.
!   Flat lying plates have a dip of 0 degrees.
!   Vertical plates have a dip of 90 degrees.
!   0 <=  DIP  < 180 degrees
!
!
!                INTERNALLY
!                ----------
!
!         The positive X axis points NORTH.
!         The positive Y axis will point EAST.
!         Z is positive downwards.
!
!   This has no effect on data input.
!
!------------------------------------------------------------------------
!*********************
!      Systems
!*********************
!
!  LeroiAir can be used to model any existing AEM system in frequency or time-
!  domain mode.  Currently the transmitter is modelled as a magnetic dipole
!  whose axis is in the vertical plane along the flight path except for the
!  new vertical coplanar broadside option.  The other exception to this is
!  for co-axial time-domain HEM where the system normalisation assumes a
!  horizontal lop of finite radius.
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
!   The input control file, named LeroiAir.cfl is read
!   from logical unit number NR = 3.
!
!   For inversion the data to be inverted must be contained in
!   LeroiAir.inv, read from logical unit NRI = 13
!
!
!   INPUT-OUTPUT FILES:
!   ------------------
!
!   For modelling only, frequency-domain output data, for reuse in
!   the time-domain restart option, is written to and read from
!   LeroiAir.frq, on logical unit ND = 7
!
!
!   VERBOSE-OUTPUT FILES:
!   --------------------
!
!   The LeroiAir.out is written to logical unit NW = 4
!
!   Messages about data or runtime errors are written in a file
!   called LeroiAir.log on logical unit NLG = 9
!
!
!   OUTPUT FILES FOR PLOTTING:
!   --------------------------
!
!   The AMIRA format file, LeroiAir.amx has been replacedby LeroiAir.mf1
!   (modelling only)
!
!   Terse inversion output for plotting is written to
!   logical unit NI = 14
!
!
!    UNIT #   UNIT ID      FILE ID      Function                 Invert    Model
!    ------   -------      -------      --------                 ------   -------
!       3       NR       LeroiAir.cfl   Input control file          x       x
!       4       NW       LeroiAir.out   Verbose output data         x       x
!       7       ND       LeroiAir.frq   F-D data for T-D reuse              x
!       9       NLG      LeroiAir.log   Data error messages         x       x
!      13       NRI      LeroiAir.inv   Data to be inverted         x
!      14       NP      LeroiAir.mv1   Inversion plotting output   x
!      14       NP      LeroiAir.mf1   Model output                        x
!
!------------------------------------------------------------------------
!
!****************************************************
!     DESCRIPTION OF DATA RECORDS for LEROIAIR.CFL
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
!         The default spectrum for the 1D host computation extends 0.001 Hz. to 100 MHz.
!
!         The default spectrum for the 3D part is between 1 KHz and 100 KHz. at 6 PPD
!         (points per decade) above 10 Hz and 3 PPD below.
!         The upper frequency limit is based on skin depth in a discretised target
!
!         The time domain transformation first splines the 3D data over the explicit
!         spectrum and than extrapolates to DC.  A fast Hankel transform is used to
!         compute the raw time-domain response out to 5 full cycles to account for
!         previous pulses in conductive environments.  That is then folded back into
!         the single pulse response and convolved with the input signal.
!
!         Numerical experiments have indicated that 6 points per decade is adequate
!         frequency discretisation, even at the high end.  Moreover, for perfect
!         frequency-domain data, only 3 frequencies are required from 1 to 10 Hz to
!         maintain an accuracy of better than .1 percent for the range of models studied.
!         This means that 28 frequencies are needed for the 1Hz to 100kHz range.
!
!         In many cases one could achieve the same accuracy with fewer 3D frequency-domain
!         computations.  The default spectrum can be over-ridden by setting TDFD = 0 in
!         which case the user needs to specify the minimum and maximum frequencies plus
!         points per decade in RECORD 2.1
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
!      DO3D =  1, 2 or 0 for MODELLING
!
!      DO3D =  1  computes response of 3-D heterogeneities and prints
!                 voltages as measured by receivers.
!
!           =  2  (time-domain only)
!                 instead of computing the frequency-domain responses
!                 (95-99 percent of LeroiAir usual computation time) use
!                 previously computed frequency-domain responses contained
!                 in file LeroiAir.frq to calculate 3-D time-domain responses.
!
!           =  0  compute layered earth model only
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
!       ISTOP = 0  read the input data and run the specified models.
!             = 1  read the input data, print the model description
!                  and STOP so that the model description can be verified.
!
!            REMEMBER to change ISTOP to 0 once the models have been verified.
!
!              ________________________________________________
!              NOTE ON DO3D for TIME-DOMAIN modelling
!              ------------------------------------------------
!
!    Each time LeroiAir is run with DO3D = 1, a file called LeroiAir.frq
!    is created on logical unit ND = 7.  LeroiAir reads this file to
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
!           IPPD = 0 : 3 points per decade will be used between MIN_FREQ and 10 KHz
!                      6 points per decade will be used between 10 KHz and MAX_FREQ
!                =  3 :  3 points per decade will be used between MIN_FREQ and MAX_FREQ
!                =  6 :  6 points per decade will be used between MIN_FREQ and MAX_FREQ
!                = 12 : 12 points per decade will be used between MIN_FREQ and MAX_FREQ
!
!      The frequency range is chosen as a compromise between accuracy and computation time.
!      The default TDFD = 1 is overly generous and often by using a smaller frequency set,
!      one can achieve accurate results in less time.
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
!            have the option to apply this algorithm to LeroiAir model output.
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
!                      - un-normalised time-domain model output
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
!    MODELLING
!    ---------
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
!    MODELLING
!    ---------
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
!           only for inversion - NPASS should be set to 0 or 1
!           NPASS > 1 will the cause program to ignore the next NPASS records
!
!**  RECORD 9.0: NPASS
!                      GO TO RECORD 10
!----------------------------------------------------------------------
!======================================================================
!
!    RECORDS 9 FOR MODELLING (DO3D = 0, 1, or 2)
!    -------------------------------------------
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
!          LITHOLOGY & STRUCTURE FOR LEROI & LEROIAIR
!          ===========================================
!
!** RECORD 10:  NLAYER, NPLATE, NLITH, GND_LVL
!
!      NLAYER - number of layers including basement.
!               NLAYER can be any integer > 0.
!
!      NPLATE - number of thin plates in the basement.
!               A maximum of 9 plates is permitted.
!               For inversion, NPLATE must be greater than 0
!               For modelling, NPLATE = 0 is permitted.
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
!          INPUT DATA FOR THE 3D TARGET STRUCTURE
!          --------------------------------------
!
!     DISCRETISATION AND COMPUTATION CONTROL
!     --------------------------------------
!
!** RECORD 13:  CELLW
!
!        CELLW - dimension of target cells for all plates.
!
!   LeroiAir will divide each target plate into a minimum of 2 cells
!   along strike and 2 cells down dip.  If a plate is 525 m. along strike
!   and 100 m. down dip and CELLW has been set to 100 m., then the program
!   will model the target as 6 by 2 cells, each of dimension 87.5 by 50 m.
!
!   The input value for CELLW represents a trade-off between speed and accuracy.
!   Decreasing CELLW can substantially increase run times but will produce
!   better accuracy.  Initially, try setting CELLW = 40.  Then try CELLW = 20.
!   In some cases, CELLW = 40 will give fast and reasonable results,  However,
!   in many cases, especially for strongly interacting multiple plates, a finer
!   discretisation (eg; CELLW = 20) may be required for sufficient accuracy,
!

!
!             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!                 The term PRE or plate reference edge refers to the south edge.
!     ======>     of a pre-oriented horizontal plate square with respect to the
!                 North and East. The PRP (plate reference point) is the midpoint
!                 of the PRE
!
!     ======>     All rotations are constructed using the PRE as a pivot point.
!
!             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!  RECORDS 14 & 15 are repeated sequentially for each plate
!
!  ___ READ FOR J = 1, NPLATE
!  |
!  | *** RECORD 14:  LITH(J), CNTR_East(J), CNTR_North(J), PLTOP(J)
!  |
!  |         LITH(J) = integer which assigns the conductance and other
!  |                   physical properties from the list of RECORD 12
!  |                   Make sure that a plate rather than a layer
!  |                   lithology is specified.
!  |
!  |
!  |        CNTR_East(J), CNTR_North(J) are the east and north coordinates of
!  |        the PLATE REFERENCE POINT of plate J before strike angle
!  |        rotation.
!  |
!  |        PLTOP = RL of designated edge of plate J.
!  |                Therefore PLTOP now increases NEGATIVELY downwards
!  |
!  |
!  |          TARGET SIZES & ORIENTATIONS
!  |          ---------------------------
!  | *** RECORD 15:  PLNGTH(J), DPWDTH(J), DZM(J), DIP(J)
!  |
!  |             PLNGTH - length of plate J.  If PLUNGE = 0, PLNGTH = strike length.
!  |             DPWDTH - width of plate J along dip
!  |                    = depth extent if the plate is vertical.
!  |
!  |                DZM - dip azimuth, (angle plate normal makes with north)
!  |                      in degrees east of north for plate J.
!  |                      If DIP = 0, DZM = 0.  (0 <= DZM <= 180).
!  |
!  |                DIP - dip angle (in degrees) of plate J.  ( 0 =< DIP < 180 )
!  |
!  |   Initially, each plate starts as a west-east oriented horizontal plate.
!  |   The PRE (plate reference edge) is defined as the southern edge.  The PRP,
!  |   is the midpoint of this edge.  The user enters the coordinates of this
!  |   point in terms of Easting, Northing and RL.  The PRP is held fixed for
!  |   all rotations.
!  |
!  |   All rotation axes pass through the PRP.  the plate is oriented by
!  |   a dip rotation about the PRE followed by an azimuth rotation about 
!  |   the vertical axis.
!  |
!  |_____   END READ OVER NPLATE
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
!       since the sugested value is MAXITS = 90
!
!    2. The RMS error is below that specified by PCTCNV.
!       Default = 1 percent
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
!    LeroiAir computes sensitivities using a numerical derivative.  The
!    default uses a 6 percent step to compute this until either the RSVT
!    (relative singular value threshold) drops from 0.1 to 0.01 or until
!    6 iterations have been completed.  At this point a 3 percent step is
!    tested and the step that yields the lowest error is selected for the
!    remainder of the inversion.  LeroiAir allows the user to reverse the
!    default or to specify a fixed derivative step through the RECORD 16
!    variable, CNVRG.
!
!
!** RECORD 16: MAXITS, CNVRG, NFIX, MV1PRT, OUTPRT
!
!      MAXITS - Maximum number of iterations.  Suggested value: MAXITS = 90.
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
!             entered as RECORDS 17
!
!      MV1PRT refers to the output print level in Airbeo.mv1
!      OUTPRT refers to the output print level in Airbeo.OUT
!
!            =  0  No output DURING inversion.  The final model set AFTER inversion,
!                  but NOT the final model data, is written to output files.
!
!            =  1  as above plus final model data
!
!            =  2  as above plus intermediate model sets after each iteration
!
!            =  3 as above plus intermediate model data after each iteration
!
!     ------------------
!     only if CNVRG = 2
!**   RECORD 16.1: PCTCNV - terminate inversion if SQRT {SUMSQ / WSUM } <n PCTCNV
!     -------------------
!
!        SUMSQ - sum of the squares of the weighted error SERR(J) at each data point.
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
!     ------------------
!     only if CNVRG = 3
!**   RECORD 16.1: PARPCT - numerical derivative step in percent
!     -------------------
!
!     LeroiAir inversion computes the sensitivity matrix using numerical derivatives.
!     Initially the derivative is computed by increasing each parameter by 6 percent.
!     When RSVT (relative singular value threshold) decreases to 0.01, a step of 3
!     percent is tested for superior error reduction.
!
!     Setting PARFAC eliminates this test, maintaining a constant value throughout
!     the inversion.  Suggested values for PARFAC are: 3, 5, or 8.
!
!     ______________________________________________________________
!
!     only if NFIX > 0  (NFIX records)
!
!**   RECORD 17: CTYPE, PLT_INDX, KPAR, ELAS(KPAR), LBND(KPAR), UBND(KPAR)
!     _____________________________________________________________________
!
!     CTYPE = 1 : parameter is fixed to initial model value
!                 only PLT_INDX & KPAR need be specified.
!                 ELAS(KPAR), LBND(KPAR), UBND(KPAR) are not read in
!
!           = 2 : frictional restraint
!                 only PLT_INDX, KPAR & ELAS(KPAR)need be specified.
!                 LBND(KPAR), UBND(KPAR) are not read in
!
!           = 3 : Buffered boundaries
!                 PLT_INDX, KPAR, ELAS(KPAR), LBND(KPAR), UBND(KPAR) must be specified.
!
!     PLT_INDX - plate for which a parameter will be constrained.
!               = 0 for layered earth parameters: host & OB resistivities & OB thickness
!
!     KPAR - parameter index for constrained parameter
!
!     For PLT_INDX > 0   (PLATES)
!     ----------------
!     KPAR = 1 => SIG_T  - conductance of plate JP
!          = 2 => PLTOP  - depth of reference point of plate JP
!          = 3 => PLNGTH - strike length of plate JP
!          = 4 => PLWDTH - dip width of plate JP
!          = 5 => YCNTR  - east coordinate of reference point of plate JP
!          = 6 => XCNTR  - north coordinate of reference point of plate JP
!          = 7 => PLAZM  - dip azimuth of plate JP
!          = 8 => PLDIP  - dip angle of plate JP
!
!     For PLT_INDX < 0   (LAYERS)
!     ----------------
!
!     PLT_INDX = -1     => top layer
!              = -NLYR  => host
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
!             In that case, NFIX = 5 and the 4 entries in RECORD 17 would read
!
!             1 1 9
!             1 2 9
!             3 -1 1 .9  800 1200   ! max step = 90 percent of distance to boundary.
!             3 -2 1 .9  800 1200   ! max step = 90 percent of distance to boundary.
!             2 -1 2  0.6           ! actual step = 60 percent of proposed step
!
!=========================================
!                                        =
!     END OF ENTRIES FOR LeroiAir.cfl    =
!         (Logical unit NR = 3)          =
!                                        =
!     BEGIN DESCRIPTON FOR LeroiAir.inv  =
!         (Logical unit NRI = 13)        =
!                                        =
!=========================================
!
!      Any number of comment lines can be inserted at the beginning of the
!      .inv file by using either / or \ as the first character of the line.
!
!      LeroiAir will start reading at the first line not containing / or \
!      as the first character.  Thereafter, any line beginning with  / or \
!      will cause an eror and an end to execution.
!
!
!             DATA DESCRIPTION & WEIGHTING  - read from LeroiAir.inv
!             ============================
!
!    In what follows, the channel reference term PCHNL refers to each
!    component for each frequency or time-domain channel.
!
!    NPCHNL = the number of PCHNLs.
!    For example if there are 2 components of 10 CHANNEL time-domain data, NPCHNL = 20
!    For the 5 frequency DIGHEM system NPCHNL = 10 corresponding
!    to 5 in-phase and 5 quadrature data
!
!         Frequency-domain:
!         ----------------
!           Channels are odered from the lowest to the highest frequency.
!
!              PCHNL = 1 to NFRQ for the in-phase and
!                    = NFRQ+1 to 2*NFRQ for the quadrature.
!
!         Time-domain:
!         -----------
!            Regardless of the order in which the data is read in, for weighting
!            PCHNL = 1 to NCHNL refer to
!                               Vertical response if CMP = 13, 2 or 3
!                               In-Line response of CMP = 11
!                               Transverse response of CMP = 12
!
!            PCHNL = NCHNL+1 to 2*NCHNL refer to in-line response If CMP = 2
!            PCHNL = 2*NCHNL+1 to 3*NCHNL refer to transverse response of CMP = 3
!
!
!**  RECORD 1: NSTAT, SURVEY, BAROMTRC, KCMP, ORDER
!
!      NSTAT - number of stations to be read in from LeroiAir.inv
!
!        SURVEY = 2 variable altitude & course but constant Tx-Rx geometry
!        SURVEY = 3 variable altitude, course, transmitter pitch
!                          and receiver offset (time-domain only)
!
!      BAROMTRC = 0 => altitudes are ground clearance in metres.
!               = 1 => altitudes are barometric; ie, metres above sea level.
!
!      (time-domain)
!      KCMP = 1   : only X (in-line component) data will be read in
!           = 3   : only Z (vertical component) data will be read in
!           = 13  : X data followed by Z data
!           = 31  : Z data followed by X data
!           = 123 : X data followed by Y data followed by Z data
!           = 312 : Z data followed by X data followed by Y data
!
!      (time-domain)
!      ORDER = 1 the first data component for all times is followed
!                by the next data component for all times in the order
!                specified by KCMP.  (for example X1 X2 Y1 Y2 Z1 Z2)
!
!            = 2 all of the data components for each channel are followed by
!                all of the data components for the next channel in the order
!                specified by KCMP   (for example X1 Y1 Z1 X2 Y2 Z2)
!
!
!      (frequency-domain)
!      KCMP(1:NFRQ) : specify components for each frequency.  These must be in the
!                     order as the frequencies in LeroiAir.cfl
!
!                  1 : HCP - horizontal coplanar
!                  2 : VCA - vertical coaxial
!                  3 : VCP - vertical coplanar
!                  4 : VCB - vertical coplanar broadside
!
!     Dighem example:       KCMP(1:5) = 1 2 2 1 1  for 5 frequencies starting from the lowest.
!     GTK wingtip example:  KCMP(1:2) = 3 3        for 2 frequencies starting from the lowest.
!
!      (frequency-domain)
!      ORDER = 1122 : all inphase data will be followed by all quadrature data
!            = 1212 : data consists of paired inphase and quadrature for each frequency
!            = 2211 : all quadrature data will be followed by all inphase data
!            = 2121 : data consists of paired quadrature and inphase for each frequency
!
!**  RECORD 2: DATA_FLOOR
!
!      Any data value whose absolute magnitude is less than DATA_FLOOR will be
!      weighted to zero.
!
!      For TIME-DOMAIN only one value is required.
!
!      For FREQUENCY-DOMAIN, 2 * NFRQ values must be entered.
!
!        The first NFRQ values refer to inphase measurements for all frequencies
!        followed by NFRQ values for all quadrature measurements.
!        These must be read in order from the lowest to the highest frequency
!
!
!**  RECORD 3: N0STAT, N0CHNL, N0PTS
!
!      NSTAT - number of stations to be read in from LeroiAir.inv
!
!      N0STAT - number of stations for which all the data will be weighted to zero
!      N0CHNL - number of PCHNLs for which all the data will be weighted to zero
!      N0PTS  - number of data points not covered by K0STAT & K0CHNL which will be
!               weighted to zero
!
!     ------------------
!     only if N0STAT > 0
!**   RECORD 4:  K0STAT(1:N0STAT) - indices of stations for which all data
!     ------------------               will be weighted to 0
!
!     ------------------
!     only if N0CHNL > 0
!**   RECORD 5:  K0CHNL(1:N0CHNL) - indices of PCHNLs for which all data
!     ------------------               will be weighted to 0
!
!     ------------------
!     only if N0PTS > 0
!**   RECORD 6:  (J0CH(I),J0ST(I)), I = 1,N0PTS)
!     ------------------
!         PCHNL and station indices of individual points to be weighted to 0.
!         using the above PCHNL ordering convention
!
!
!             DATA ENTRY   (read from LeroiAir.inv)
!             ==========
!
!      NSTAT records -
!
!    for SURVEY = 2 or -2
!**  RECORD 7.J: LINE(J), EAST(J), NORTH(J), ALT(J), DATA(1:NPCHNL, J)
!
!    for SURVEY = 3 or -3 (time domain)
!**  RECORD 7.J: LINE(J), EAST(J), NORTH(J), ALT(J), TXCLN(J), ZRX(J), XRX(J), YRX(J), DATA(1:NPCHNL, J)
!                                       ZRX(J), XRX(J),
!        DATA(I,J) = datum of PCHNL I at station J
!          Note that unnormalised B data is in pT and unnormalised dB/dt data is in nT/s
!          unless otherwise specified in RECORD 2.2
!
!        For frequency-domain inversion or if KPPM > 0, normalised data are expressed
!        in ppm unless otherwise specified using NPPF /= 3.
!
!
!        ZRX(J), XRX(J) = vertical & inline offsets at station J
!
!==============================
!  END DATA ENTRY DESCRIPTIOM |
!==============================
!
!============================================================================


   MODULE LA_Filter_coefficients
!  --------------------------

  IMPLICIT NONE

  INTEGER, PARAMETER :: JNLO=-250, JNHI=150, NDEC_JN=15
  INTEGER J9
  REAL SHFTJN, WJ0(JNLO:JNHI), WJ1(JNLO:JNHI)
  SAVE

!  Filter restored to original LeroiAir 7 February, 2000 (artificial shift removed)

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

END MODULE LA_Filter_coefficients

 MODULE LA_Filter_coefficients_QL
!------------------

!  These are based on using extended precision.  Not yet used for 3D computatiuon

 IMPLICIT NONE

 INTEGER, PARAMETER :: JNLO=-250, JNHI=150, NDEC_JN=15, QL=SELECTED_REAL_KIND(p = 18)
 INTEGER J9
 REAL(KIND=QL) WJ0(JNLO:JNHI), WJ1(JNLO:JNHI), WCOS(-200:99), DELCOS, SHFTJN
 SAVE

!  Filter restored to original LeroiAir 7 February, 2000 (artificial shift removed)

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

END MODULE LA_Filter_coefficients_QL

 MODULE LA_Frequency_select
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

 END MODULE LA_Frequency_select

 MODULE LA_Input_routines

! CONTAINS: READ_SYSTEM_AND_SURVEY, READ_MODEL, READ_INVRSION_CNTRL, , READ_INVRSION_DATA, SET_FRQ


 Use LA_Metadata
 Use iso_Fortran_env
 IMPLICIT NONE

! General Airborne & Layered Earth Dimensions

 INTEGER, PARAMETER :: NPROP=7, QL=SELECTED_REAL_KIND(p = 18)
 Integer, Parameter :: FVERS = 500
 REAL, PARAMETER :: T0_MIN=1.E-7, PI=3.141592654, PI2=PI/2., DATA_TOL=1.E-24, TURN=PI/10.
 INTEGER NR,NW,ND,NDR,NLG,NRI,NP,KS,TDFD,STEP,DO3D,ISW,PRFL,NCHNL,KPPM,ISTOP,NLITH, &
         NSX,NSX1,JF,JT,JS,JL,JR,NFRQ,NTYRP,NSTAT,NLYR,NRX,NRXST,NTRN,MTXRX,NPULS,   &
         NTYPLS,SURVEY,CMP,KRXW,MSG,MXERR,J,GSTRP,ASTRP,IUNITS,NPPF,QQDT(8),QQHMS(2), &
         MCHNL,NDATA,INRM,NPAR,MV1PRT,OUTPRT,CNVRG,MAXITS,A2C,BAROMTRC,LINE_TAG, &
         KLO,KHI,KMD,KQ(5),PPD1,PPD2
 INTEGER,ALLOCATABLE,DIMENSION(:) :: LITH,KFIX,CXPAR,XWTS,LINE,CFG1
 INTEGER,ALLOCATABLE,DIMENSION(:,:) :: RWTS
 REAL TXAMPL,TXFREQ,PULSE,PKCUR,OFFTYM,ALF,DELT,ALT1,GND_LVL,TXCLN0,XRX0,YRX0,ZRX0,CSF,SNF, &
      DSTAT,PRM_TD(3),BFFAC,PPFAC,TXAREA,T0,PCTCNV,ALINE,RDUM(6)
 REAL,DIMENSION(:),ALLOCATABLE :: FREQ,WAVEFORM,TXON,SWX,TMS,WTMS,TOPN,TCLS,SZ,SX,SY,ZRX, &
                                  XRX,YRX,BEARING,FANGLE,THK,RES,RMU,REPS,CHRG,TRP,CALF,  &
                                  CTAU,CFREQ,TXCLN,TXDEG,PRM_FD,XDATA,XMODL,DATA_FLOOR,   &
                                  DNORM,UBND,LBND,ELAS,MPAR
 REAL,DIMENSION(:,:),ALLOCATABLE :: SWY,RX,RY,RZ,LYTH,RDATA
 REAL(KIND=QL) QFRQ1,QFRQ2,FQQ,D_EAST,D_NORTH,EAST1,NORTH1
 REAL(KIND=QL), ALLOCATABLE :: SXD(:),SYD(:),RXD(:,:),RYD(:,:)
 LOGICAL PRTSEC,INVERT,NEWF,TXA90
 LOGICAL, ALLOCATABLE :: SAME_TX(:)
 CHARACTER (LEN=3), ALLOCATABLE :: CONFIG(:)
 CHARACTER (LEN=1) TCHR
 CHARACTER (LEN=120) INP,TITLE
 CHARACTER(LEN=60) LTXT
 CHARACTER(LEN=4) QUNIT,BUNIT,PUNIT
 Integer :: tvals(8)
 DATA LTXT     /'-------------------------------------------'/
! Parameters for the 3D target

! Leroi Specific Dimensions

 INTEGER NPLT,MXB,JP,MXAB,MXABT,NBLK,BEG,FIN,JA,JB,JAB,NAB,NAJ,NBJ, &
         JP2,JAB2,NAB2,MXRHO,NRMGT,KP,KP1
 INTEGER,ALLOCATABLE :: NA(:),NB(:)
 REAL CELLW,DAH,TMPL,PL2,X0,Y0,Z0,XCTMP,YCTMP,R1,R2,R3,R4,PARPCT
 REAL,DIMENSION(:),ALLOCATABLE :: SIG_T,CHRGP,CALFP,CTAUP,CFREQP,PLTOP,XCNTR,YCNTR,PLNGTH, &
                                  PLWDTH,AZM,PLAZM,DIP,PLDIP,PLG,PLUNJ,ESTRT,NSTRT,E_END,  &
                                  N_END,ZEND,DA,DB
 REAL,DIMENSION(:,:),ALLOCATABLE :: XCELL,YCELL,ZCELL
 REAL,DIMENSION(:,:,:),ALLOCATABLE :: PCNR
 REAL(KIND=QL), ALLOCATABLE, DIMENSION(:) :: XCNTRD,YCNTRD

 CONTAINS

   SUBROUTINE READ_SYSTEM_AND_SURVEY
!  -----------------------=---------

!***  Called by: MAIN
!***      Calls: CALL CONFIG_ID, WRITE_LOG_FILE

!  If DO3D = -1 or -2, data to be inverted is read in this routine

 REAL BFFACS(6),PPFACS(4),RHO,DELX,DELY,A1,A2
 CHARACTER(LEN=4) PUNITS(4),BUNITS(6)
 CHARACTER (LEN=19) WVFRM(3)
 DATA WVFRM /'Transmitter current','Vertical receiver  ','Horizontal receiver'/
 DATA PUNITS /'pct ','ppt ','ppm ','ppb '/
 DATA BUNITS /'nT/s','pT/s','fT/s','nT  ','pT  ','fT  '/
 DATA BFFACS /1.,1000.,1.E6,1.,1000.,1.E6/
 DATA PPFACS /1.E2, 1.E3, 1.E6,1.E9/

 NR =  3     !  Input unit number for LeroiAir.cfl
 NW =  4     !  Output unit number for LeroiAir.out
 ND =  7     !  Input/Output unit number for LeroiAir.frq
 NLG = 9     !  Log file unit number for LeroiAir.log
 NRI = 13    !  Inversion data for LeroiAir.inv
 NP = 14    !  Output unit number for LeroiAir.mv1 (inversion)
             !  or LeroiAir.mf1 (model)

 OPEN(NR,FILE = 'LeroiAir.cfl',STATUS = 'OLD')
 OPEN(NW,FILE = 'LeroiAir.out',STATUS = 'REPLACE')

 CALL DATE_AND_TIME (Values = tvals)
 QQHMS(1:2) = QQDT(5:6)
 ! WRITE(*,97)  QQHMS(1:2),QQDT(3),MONTH(QQDT(2)),QQDT(1)
 ! WRITE(NW,97) QQHMS(1:2),QQDT(3),MONTH(QQDT(2)),QQDT(1)

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
 INVERT = .FALSE.
 NEWF = .FALSE.
 NDR = NR              !  Read from LeroiAir.cfl
 PRTSEC = .FALSE.      !  Don't print scattered fields
 TXA90 = .FALSE.      !  Don't print scattered fields

!  Reproduce input data with no assignments and rewind file.

Write (NW, 1) PNAME, PVERS, PDATE, PAUT1, PAUT2, PAUT3, PPROJ, PRELS, &
              compiler_version(), compiler_options(), &
              tvals(1:3), tvals(5:7)
Write ( *, 1) PNAME, PVERS, PDATE, PAUT1, PAUT2, PAUT3, PPROJ, PRELS, &
              compiler_version(), compiler_options(), &
              tvals(1:3), tvals(5:7)
! WRITE(NW,'(T11,A/T11,A/)') 'INPUT DATA', '----------'
! REFLECT_DATA: DO JF = 1,10000
!   READ(NR,'(A)',END = 100) INP
!   WRITE(NW,'(1X,A)') INP
! END DO REFLECT_DATA

 ! 100 REWIND NR
 ! WRITE(NW,2)

 READ(NR,'(A)') TITLE
 WRITE(NW,'(/1X,A)') TRIM (TITLE)

! Read model control & print parameters

 READ(NR,*)  TDFD, DO3D, PRFL, ISTOP
 WRITE(NW,3) TDFD, DO3D, PRFL, ISTOP
 IF (DO3D > 0) THEN
   IF (PRFL > 9) THEN
     PRTSEC = .TRUE.
     PRFL = PRFL - 10
   END IF
   IF (PRFL /=0) PRFL = 1
 ELSE IF (DO3D < 0) THEN
   INVERT = .TRUE.
 END IF

!   TDFD = 0, 1 or 2 for user controlled TD, standard TD or FD respectively.
!   DO3D = 2 => use old FD data from LeroiAir.frq.
!        = 1 => compute new  plate model.
!        = 0 => compute layered 1/2 space model only.
!   PRFL - indicates profile or decay curve output
!  ISTOP - read data and stop if ISTOP = 1

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
     OPEN(ND,FILE = 'LeroiAir.frq',STATUS = 'REPLACE')
   ELSE IF (DO3D == 2) THEN
     NDR = ND           ! Data to be read in from LeroiAir.frq
     OPEN(ND,FILE = 'LeroiAir.frq',STATUS = 'OLD')
   END IF

! Transmitter system information
! ------------------------------

   IF (TDFD == 0) THEN
     READ(NR,*) KQ(1:5)
     DO J = 4,5
       IF (KQ(J) <= 3) THEN
         KQ(J) = 3
       ELSE IF (KQ(J) >= 12) THEN
         KQ(J) = 12
       ELSE 
         KQ(J) = 6
       END IF
     END DO
     PPD2 = MAXVAL (KQ(4:5))
     PPD1 = MINVAL (KQ(4:5))

     KLO = MINVAL (KQ(1:3))
     KHI = MAXVAL (KQ(1:3))
     KMD = MINVAL (KQ(2:3))
     WRITE(NW,20) KLO,KHI,KMD,PPD1,PPD2
   END IF

   NRX = 1
   READ(NR,*)   ISW, NSX1, STEP, IUNITS, NCHNL, KRXW, OFFTYM
   WRITE(NW,4) ISW, NSX1, STEP, IUNITS, NCHNL, KRXW, OFFTYM
   IF (IUNITS < 1 .OR. IUNITS > 3) THEN
     IF (INVERT) THEN
       CALL WRITE_LOG_FILE (NLG,17,MXERR,2)
     ELSE
       CALL WRITE_LOG_FILE (NLG,19,MXERR,1)
     END IF
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
       WRITE(NW,'(3X,I6,f13.5,5X,e12.4)') J,TXON(J),WAVEFORM(J)
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
     WRITE(NW,'(8X,I3,4F12.4)') JT,TOPN(JT),TCLS(JT),WTMS(JT),TMS(JT)
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

   IF (INVERT) THEN
     IF (CMP /= 11 .AND. CMP /= 13 .AND. CMP/= 2 .AND. CMP /= 3 .AND. &
         CMP/= 4) CALL WRITE_LOG_FILE (NLG,8,MXERR,2)

   ELSE
     IF (CMP /= 11 .AND. CMP /= 13 .AND. CMP/= 2 .AND. CMP /= 3) THEN
       CMP = 3
       CALL WRITE_LOG_FILE (NLG,9,MXERR,1)
     END IF
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

   IF (INVERT .AND. CMP /= 1) CALL WRITE_LOG_FILE (NLG,14,MXERR,2)
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

! Flight path details for modelling only.  Convert FANGLE & TXCLN to radians

 IF (INVERT) THEN
   READ(NR,*) NSTAT
   IF (NSTAT > 1) THEN
     DO JF = 1,NSTAT
       READ(NR,'(A)') INP
     END DO
   END IF
 ELSE
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

! Read in course for modelling
! Read in course + data to be inverted for inversion

             ! Modelling only

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
         IF (A1 < 0.1) SAME_TX(JS) = .TRUE.
       END IF
     END DO
   END IF
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
    /, 2x, '| License:  ', a, &
    /, 2x, '|', &
    /, 2x, '| Compiler: ', a, &
    /, 2x, '| Options:  ', a, &
    /, 2x, '|', &
    /, 2x, '| Started:  ', i4.4, '-', i2.2, '-', i2.2, 'T', i2.2, ':', i2.2, ':', i2.2, &
    /, 2x, 78('-'), /)
 ! 2 FORMAT (T1,79('-'))
 3 FORMAT(/T3,'TDFD =',I3,3X,'DO3D =',I3,3X,'PRFL =',I3,4X,'ISTOP =',I2)
 4 FORMAT(/T3,'ISW =',I4,T15,'NSX =',i6,T27,'STEP =',I2,T39,'UNITS =',I4,T52,'NCHNL =',I4, &
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
 20 FORMAT(T3,'KLO =',I3,T15,'KHI =',I3,T27,'KMD =',I3,T40,'PPD1 =',I2,3X,'PPD2 =',I2)
 21 FORMAT(I3,F9.0,F8.0,F7.0,F7.1,2F6.1,T51,A)
 22 FORMAT(//T3,'NSTAT =',I10,3X,'SURVEY =',I2,3X,'BAROMTRC =',I2,3X,'LINE_TAG =',I2)
 24 FORMAT(T3,'The flight path follows an angle of',F5.0,' degrees East of North.')
 25 FORMAT(/T7,I3,' transmitter positions along the flight path' &
          //T6,'Line   Stat     East       North       Alt      Bearing    Pitch   ZRX    XRX      YRX' &
           /T6,'----   ----     ----       -----       ---      -------    -----   ---    ---      ---'/)
 26 FORMAT(T1,I10,I6,2F12.1,2F10.1,F9.1,3F8.1)
 27 FORMAT(/T7,I3,' transmitter positions along the flight path' &
          //T6,'Line   Stat       East        North       Alt     Bearing' &
           /T6,'----   ----       ----        -----       ---     -------')
 28 FORMAT(T1,I9,I6,2X,2F12.1,2F10.1)
 41 FORMAT(/T3,'Geotem / Questem stripping algorithm will be applied to computations.')
 42 FORMAT(/T3,'Geotem / Questem stripping algorithm is not applied for B field output.')
 43 FORMAT(/T3,'Geotem / Questem stripping algorithm is not applied for B field input.')
 44 FORMAT(/T3,'Geotem / Questem stripping algorithm is not applied for ISW = 1 or 4.')
 ! 97 FORMAT(//T3,'LeroiAir task started at ',I2.2,':',I2.2,' on',I3.2,1X,A,I5//)

   END SUBROUTINE READ_SYSTEM_AND_SURVEY

   SUBROUTINE READ_MODEL
!  ---------------------

!***  Called by: MAIN
!***      Calls: WRITE_LOG_FILE

 IMPLICIT NONE
 REAL, PARAMETER ::  CELLW_MIN=0.01
 Real, Parameter ::  CELLW_MAX=2000.    ! was 200 ...
 INTEGER MXABT,MXLITH
 REAL TMPL,PLATE_LIMIT,THICK


!  Layered Model Specification
!  ---------------------------

 READ(NDR,*) NLYR, NPLT, NLITH, GND_LVL
 IF (DO3D == 0) NPLT = 0
 IF (NEWF) WRITE(ND,'(3I6,G12.4,A)') NLYR, NPLT, NLITH, GND_LVL,'!  NLYR, NPLT, NLITH, GND_LVL'
 WRITE(NW,1) NLYR, NPLT, NLITH, GND_LVL
 IF (NPLT < 0 .OR. NPLT > 9) CALL WRITE_LOG_FILE (NLG,50,MXERR,2)
 IF (INVERT .AND. NPLT == 0) CALL WRITE_LOG_FILE (NLG,51,MXERR,2)
 IF (NPLT == 0) DO3D = 0
 NPAR = 9*NPLT + 2*NLYR-1

 MXLITH = MAX(NLYR,NPLT)

 ALLOCATE (LYTH(NLITH,NPROP),LITH(MXLITH),THK(NLYR),RES(NLYR),CHRG(NLYR),CALF(NLYR), &
           CTAU(NLYR),CFREQ(NLYR),RMU(NLYR),REPS(NLYR),MPAR(NPAR))

 THK=0; RES=0; CHRG=0; CALF=1; CTAU=0; CFREQ=1; RMU=1
 REPS=1; LITH=0

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

 WRITE(NW,3)
 IF (NLYR > 1) THEN
   DO J = 1, NLYR-1
     READ (NDR,*) LITH(J), THK(J)
     IF (NEWF) WRITE(ND,'(I5,G13.5)') LITH(J), THK(J)
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


!  Start reading plate variables if DO3D > 0

 ALLOCATE (SIG_T(NPLT),CHRGP(NPLT),CALFP(NPLT),CTAUP(NPLT),CFREQP(NPLT),PLTOP(NPLT), &
           XCNTR(NPLT),YCNTR(NPLT),XCNTRD(NPLT),YCNTRD(NPLT),PLNGTH(NPLT),DIP(NPLT), &
           PLDIP(NPLT),PLWDTH(NPLT),AZM(NPLT),PLAZM(NPLT),PLG(NPLT),PLUNJ(NPLT),     &
           NA(NPLT),NB(NPLT),DA(NPLT),DB(NPLT),PCNR(3,4,NPLT))

 SIG_T=-1.; CHRGP=0; CALFP=1; CTAUP=0; CFREQP=1; PLTOP=0; XCNTR=0
 YCNTR=0; PLNGTH=0; PLWDTH=0; AZM=0; DIP=0
 MXAB=4

 READ(NDR,*) CELLW
 WRITE(NW,4) CELLW

 CELLW = MIN (CELLW_MAX, CELLW)
 CELLW = MAX (CELLW_MIN, CELLW)
 IF (NEWF) WRITE(ND,'(F7.2,3I4)')  CELLW

 THICK = SUM (THK(1:NLYR-1))
 PLG = 0.
 DO JP = 1, NPLT
   WRITE(NW,5) JP 

! To maintain historical continuity, the input data description calls for the
! plate locator CNTR_East, CNTR_North read in as the east, north coordinates.
! Substitute YCNTR and XCNTR as the east, north coordinates in order to use
! a right-handed system with Z downwards.

   READ(NDR,*) LITH(JP),YCNTRD(JP),XCNTRD(JP),PLTOP(JP)
   WRITE(NW,'(2I4,2F13.2,F11.2,T49,A)') JP,LITH(JP),YCNTRD(JP),XCNTRD(JP),PLTOP(JP),'JP, LITH,  YCNTRD, XCNTRD,  PLTOP'

   J = LITH(JP)

   PLATE_LIMIT = GND_LVL
   IF (NLYR > 1) PLATE_LIMIT = GND_LVL - THICK
   IF (PLTOP(JP) > PLATE_LIMIT) THEN
     PLTOP(JP) = PLATE_LIMIT
     CALL WRITE_LOG_FILE (NLG,56,MXERR,2)
   END IF

   IF (NEWF) WRITE(ND,'(I4,3G14.5)') LITH(JP),YCNTRD(JP),XCNTRD(JP),PLTOP(JP)
!   READ(NDR,*) PLNGTH(JP), PLWDTH(JP), AZM(JP), DIP(JP), PLG(JP)
   READ(NDR,*) PLNGTH(JP), PLWDTH(JP), AZM(JP), DIP(JP)
   IF (NEWF) WRITE(ND,'(5G13.5)') PLNGTH(JP),PLWDTH(JP),AZM(JP),DIP(JP), PLG(JP)
   WRITE(NW,'(T3,2F9.1,3F8.1,3X,T49,A)') PLNGTH(JP),PLWDTH(JP),AZM(JP),DIP(JP),PLG(JP), &
                                 'PLNGTH, PLWDTH, AZM, DIP, PLG'

   IF (DIP(JP) <   0. .OR. DIP(JP) > 180.) CALL WRITE_LOG_FILE (NLG,58,MXERR,2)
   IF (PLG(JP) < -90. .OR. PLG(JP) >  90.) CALL WRITE_LOG_FILE (NLG,59,MXERR,2)
   KP = 9* (JP-1)
   MPAR(KP+2) = PLTOP(JP)
   MPAR(KP+3) = PLNGTH(JP)
   MPAR(KP+4) = PLWDTH(JP)
   MPAR(KP+5) = REAL (YCNTRD(JP))
   MPAR(KP+6) = REAL (XCNTRD(JP))
   MPAR(KP+7) = AZM(JP)
   MPAR(KP+8) = DIP(JP)
   MPAR(KP+9) = PLG(JP)
 END DO
 PLTOP = GND_LVL - PLTOP    ! Convert PLTOP from RL to depth below surface
 PLDIP = DIP * PI / 180.    ! Convert angles from degrees to radians
! PLAZM = AZM * PI / 180.
 PLAZM = (AZM-90.) * PI / 180.  ! PLASM  will be used as the old strike azimuth
 PLUNJ = PLG * PI / 180.

 IF (NPLT > 0) THEN
   D_EAST = YCNTRD(1)         ! Set up body centred origin. Shift origin by (D_NORTH, D_EAST)
   D_NORTH = XCNTRD(1)
   IF (MAX (ABS(D_EAST),ABS(D_EAST)) < 2000._QL) THEN
     D_NORTH = 0._QL
     D_EAST = 0._QL
   END IF
 END IF

 KP = 9*NPLT
 KP1 = KP + NLYR
 MPAR(KP+1:KP1)  = RES(1:NLYR)
 DO JP = 1,NLYR-1
   MPAR(KP1+JP) = THK(JP)
 END DO

 DO JP = 1, NPLT
   J = LITH(JP)
   IF (J < 1 .OR. J > NLITH) THEN
     WRITE(NW,'(T3,A,I2,A,I4)') 'LITH(',JP,') =',J
     CALL WRITE_LOG_FILE (NLG,54,MXERR,2)
   END IF

   SIG_T(JP) = LYTH(J,2)
   IF (SIG_T(JP) < 0) CALL WRITE_LOG_FILE (NLG,57,MXERR,2)
   KP = 9* (JP-1)
   MPAR(KP+1) = SIG_T(JP)

   CHRGP(JP) =  LYTH(J,5)
   CTAUP(JP) =  LYTH(J,6)
   CFREQP(JP) = LYTH(J,7)
   CALFP(JP) = 1. - CHRGP(JP)

   TMPL = MIN (PLNGTH(JP), PLWDTH(JP)) / 2.
   TMPL = MIN (TMPL, CELLW) + .01
   NA(JP) = INT (PLNGTH(JP) / TMPL) + 1
   NB(JP) = INT (PLWDTH(JP) / TMPL) + 1
   NA(JP) = MAX (2,NA(JP))
   NB(JP) = MAX (2,NB(JP))
   DA(JP) = PLNGTH(JP) / REAL (NA(JP),4)
   DB(JP) = PLWDTH(JP) / REAL (NB(JP),4)
   MXABT = NA(JP) * NB(JP)
   MXAB = MAX (MXAB, MXABT)
 END DO

 MXB = MAX (2, MAXVAL (NB))
 IF (INVERT) THEN
   MXB = CEILING (1.5 * MXB)
   MXAB = CEILING (1.5 * MXAB)
 END IF

 ALLOCATE (XCELL(MXAB,NPLT),YCELL(MXAB,NPLT),ZCELL(MXB,NPLT))

 XCELL=0; YCELL=0; ZCELL=0

  1 FORMAT(//T3,'NLAYER =',I3,';   NPLATE =',I3,';   NLITH =',I3,';   GND_LVL =',F8.2)
  2 FORMAT(//T27,'LITHOLOGY PROPERTIES'/T27,'--------------------' &
           //T35,'Relative   Relative     Cole-Cole Parameters'    &
            /T9,'Resistivity  Conductance     MU     Dielectric   CHRG    CTAU       CFREQ'/)
  3 FORMAT(//T3,'LAYERED EARTH INPUT DATA'/T3,'------------------------'/)
  4 FORMAT(//T3,'CELLW =',F7.2)
  5 FORMAT(/T3,'Input Data for Plate',I3/T3,'-----------------------')

   END SUBROUTINE READ_MODEL

   SUBROUTINE READ_INVRT_CNTRL_AND_DATA
!  ------------------------------------

!***  Called by: MAIN
!***      Calls: WRITE_LOG_FILE

 INTEGER NFIX,ORDER,MDCHNL,NDCMP,N0STAT,N0CHNL,N2B,N2E,N3B,N3E, &
         CTYPE,PLT_INDX,LYR_INDX,N0PTS,KPAR,JP1,J1
 INTEGER, ALLOCATABLE, DIMENSION(:) :: KCMP,K0STAT,K0CHNL
 REAL TDATA,E1,E2,E3,A1,A2,DELX,DELY,RHO
 REAL, ALLOCATABLE,DIMENSION (:) :: QDATA,Q2DATA
 CHARACTER (LEN=1) TCHR
 CHARACTER (LEN=3) KCMPC(0:5)
 CHARACTER(LEN=12) PLATE_PRM(9),LYR_PRM(2)

 Logical :: IsComment

 DATA KCMPC / '   ','HCP','VCP','VCA','VCB','HCA'/
 DATA LYR_PRM /  'Resistivity',' Thickness'/
 DATA PLATE_PRM /'Conductance','Depth to top','Plate length','Dip width','CNTR_East', &
                 'CNTR_North','Dip azimuth','Dip angle',' Plunge'/
! ----------------------------------------------------------------
! Set inversion dimensions:
!
!  NCHNL = number of time domain channels
!  MCHNL = total number of readings per station to be inverted(TD or FD)
!        = NCHNL for time-domain when CMP = 11, 13, 4, 42, 43
!        = 2* NCHNL for time-domain when CMP = 2
!        = 3* NCHNL for time-domain when CMP = 3
!
!        = 2 * NFRQ for frequency-domain
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
! ----------------------------------------------------------------

!  Set degree of constraint on each parameter
!  CXPAR = 0 => parameter is completely free to vary as dictated by inversion step
!        = 1 => parameter is fixed
!        = 2 => parameter is constrained by elasticity.
!        = 3 => parameter bounds are buffered.

 NPAR = 9*NPLT + 2*NLYR-1
 IF (TDFD < 2) THEN
   MCHNL = NCHNL
   IF (CMP == 2) MCHNL = 2*NCHNL
   IF (CMP == 3) MCHNL = 3*NCHNL
 ELSE
   MCHNL = 2*NFRQ
 END IF
 ALLOCATE (ELAS(NPAR),LBND(NPAR),UBND(NPAR),CXPAR(NPAR))
 CXPAR = 0
 ALLOCATE (DATA_FLOOR(MCHNL))
 IF (TDFD < 2) ALLOCATE (KCMP(1))
 IF (TDFD == 2) ALLOCATE (KCMP(NFRQ))

 DATA_FLOOR = 0.
 E1 = 1. ; ELAS = 1.
 E2 = 1. ; LBND = 1.
 E3 = 1. ; UBND = 1.

 WRITE(NW,1)
 IF (TDFD < 2) THEN
   IF (CMP == 13) WRITE(NW,2)
   IF (CMP == 11) WRITE(NW,3)
   IF (CMP == 2)  WRITE(NW,4)
   IF (CMP == 3)  WRITE(NW,5)
   IF (KPPM == 0) THEN
     WRITE(NW,6) TRIM (QUNIT)
   ELSE
     WRITE(NW,7) TRIM (QUNIT)
   END IF
 ELSE IF (TDFD == 2) THEN
   WRITE(NW,8)
   WRITE(NW,7) TRIM (QUNIT)
 END IF
 WRITE(NW,9) NPAR, MCHNL

 READ(NR,*) RDUM(1),CNVRG,NFIX,MV1PRT,OUTPRT
 IF (OUTPRT < 2) OUTPRT = 2
 MAXITS = CEILING (RDUM(1))
 WRITE(NW,10) MAXITS,CNVRG,NFIX,MV1PRT,OUTPRT
 IF (MV1PRT < 0 .OR. MV1PRT > 3) MV1PRT = 1
 IF (OUTPRT < 0 .OR. OUTPRT > 3) OUTPRT = 1
 IF (CNVRG < 1 .OR. CNVRG > 4) CALL WRITE_LOG_FILE (NLG,205,MXERR,2)

 PARPCT = 6.
 PCTCNV = 0.1
 IF (CNVRG == 2) READ(NR,*) PCTCNV
 WRITE(NW,15) PCTCNV,MAXITS ; WRITE(*,15) PCTCNV,MAXITS
 IF (CNVRG == 4) THEN
   READ(NR,*) PARPCT
   WRITE(NW,16) PARPCT
   IF (PARPCT > 10. .OR. PARPCT < 3.) CALL WRITE_LOG_FILE (NLG,213,MXERR,1)
 END IF

 IF (NFIX > 0) THEN
   WRITE(NW,17)
   DO JP = 1, NFIX
     READ(NR,*) CTYPE
     BACKSPACE NR
     SELECT CASE (CTYPE)            ! J1 is a dummy variable
     CASE(1)
       READ(NR,*) J1,PLT_INDX,KPAR
       E1 = 0.
     CASE(2)
       READ(NR,*) J1,PLT_INDX,KPAR,E1
     CASE(3)
       READ(NR,*) J1,PLT_INDX,KPAR,E1,E2,E3
     END SELECT


     IF (PLT_INDX > NPLT) THEN
       CALL WRITE_LOG_FILE (NLG,202,MXERR,1)
       WRITE(NLG,200) JP
       CYCLE
     ELSE IF (PLT_INDX > 0) THEN
       IF (KPAR < 1 .OR. KPAR > 9) THEN
         CALL WRITE_LOG_FILE (NLG,203,MXERR,1)
         WRITE(NLG,200) JP
         CYCLE
       END IF
     ELSE IF (PLT_INDX < -NLYR) THEN
       CALL WRITE_LOG_FILE (NLG,210,MXERR,1)
       WRITE(NLG,200) JP
       CYCLE
     ELSE IF (PLT_INDX < 0) THEN
       IF (KPAR < 1 .OR. KPAR > 2) THEN
         CALL WRITE_LOG_FILE (NLG,211,MXERR,1)
         WRITE(NLG,200) JP
         CYCLE
       END IF
     ELSE IF (PLT_INDX == 0) THEN
       CALL WRITE_LOG_FILE (NLG,212,MXERR,1)
       WRITE(NLG,200) JP
       CYCLE
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
     A2 = 0.005 * (ABS (E2) + ABS (E3))
     IF (A1  < A2) E1 = 0.

     IF (PLT_INDX < 0) THEN
       LYR_INDX = ABS (PLT_INDX)
       JP1 = 9*NPLT + LYR_INDX             !  Resistivity of layer LYR_INDX
       IF (KPAR == 2) JP1 = JP1 + NLYR     !  Thickness of layer LYR_INDX
       CXPAR (JP1) = CTYPE
       IF (E1 < 0.05) CXPAR(JP1) = 1
       IF (CTYPE == 3) THEN
         WRITE(NW,29) JP1,LYR_INDX,KPAR,LYR_PRM(KPAR),E1,E2,E3,CXPAR(JP1)
       ELSE
         WRITE(NW,30) JP1,LYR_INDX,KPAR,LYR_PRM(KPAR),E1,CXPAR(JP1)
       END IF
     ELSE
       JP1 = 9* (PLT_INDX - 1) + KPAR
       CXPAR (JP1) = CTYPE
       IF (E1 < 0.05) CXPAR(JP1) = 1
       IF (CTYPE == 3) THEN
         WRITE(NW,18) JP1,PLT_INDX,KPAR,PLATE_PRM(KPAR),E1,E2,E3,CXPAR(JP1)
       ELSE
         WRITE(NW,31) JP1,PLT_INDX,KPAR,PLATE_PRM(KPAR),E1,CXPAR(JP1)
       END IF
     END IF
     ELAS(JP1) = E1
     LBND(JP1) = E2
     UBND(JP1) = E3
   END DO
   WRITE(NW,20)
 ELSE
   WRITE(NW,19)
 END IF
 DO JP = 1,NPLT
   JP1 = 9*JP
   CXPAR(9*JP) = 1      ! plate plunge held at zero for this version.
   IF (CXPAR(JP1) == 0) THEN  ! Restrict PLG between -PI/2 and PI/2
     CXPAR(JP1) = 3
     ELAS(JP1) = 1.
     LBND(JP1) = -PI2
     UBND(JP1) =  PI2
   ELSE
     ELAS(JP1) = -ABS (ELAS(JP1))
     LBND(JP1) = MAX (LBND(JP1), -PI2)
     UBND(JP1) = MIN (UBND(JP1),  PI2)
   END IF

   JP1 = 9*JP-1
   IF (CXPAR(JP1) == 0) THEN  ! Restrict DIP between 0 and PI
     CXPAR(JP1) = 3
     ELAS(JP1) = 1.
     LBND(JP1) = 0.
     UBND(JP1) = PI
   ELSE
     ELAS(JP1) = -ABS (ELAS(JP1))
     LBND(JP1) = MAX (LBND(JP1), 0.)
     UBND(JP1) = MIN (UBND(JP1), PI)
   END IF
 END DO

!  Start reading from LeroiAir.inv on UNIT NRI = 13
!  First skip over al comment lines

 DO
   READ (NRI,'(A)') TCHR
   IF (.not.(IsComment(tchr))) EXIT
 END DO
 BACKSPACE (NRI)

 IF (TDFD < 2) THEN
   READ(NRI,*)  NSTAT, SURVEY, BAROMTRC, KCMP(1), ORDER
   WRITE(NW,21) NSTAT, SURVEY, BAROMTRC, KCMP(1), ORDER
   SELECT CASE (CMP)
   CASE (11)
     IF (KCMP(1) == 3) CALL WRITE_LOG_FILE (NLG,220,MXERR,2)
   CASE (13)
     IF (KCMP(1) == 1) CALL WRITE_LOG_FILE (NLG,221,MXERR,2)
   CASE (2)
     IF (KCMP(1) == 1) CALL WRITE_LOG_FILE (NLG,222,MXERR,2)
     IF (KCMP(1) == 3) CALL WRITE_LOG_FILE (NLG,223,MXERR,2)
   CASE (3)
     IF (KCMP(1) == 1) CALL WRITE_LOG_FILE (NLG,224,MXERR,2)
     IF (KCMP(1) == 3) CALL WRITE_LOG_FILE (NLG,225,MXERR,2)
     IF (KCMP(1) > 4 .AND. KCMP(1) < 100) CALL WRITE_LOG_FILE (NLG,226,MXERR,2)
   END SELECT
 ELSE IF (TDFD == 2) THEN
   READ(NRI,*)  NSTAT, SURVEY, BAROMTRC, KCMP(1:NFRQ), ORDER
   WRITE(NW,32) NSTAT, SURVEY, BAROMTRC, ORDER
   IF (MAXVAL (FREQ) < 1.E5) THEN
     WRITE(NW,33) (J,KCMPC(KCMP(J)),FREQ(J),J = 1,NFRQ)
   ELSE
     WRITE(NW,34) (J,KCMPC(KCMP(J)),FREQ(J),J = 1,NFRQ)
   END IF
   DO JF = 1,NFRQ
     IF (CFG1(JF) /= KCMP(JF)) THEN
       CALL WRITE_LOG_FILE (NLG,215,MXERR,2)
       WRITE(NLG,45) KCMP(KCMP(1:NFRQ))
       WRITE(NLG,46) KCMPC(KCMP(1:NFRQ))
       WRITE(NLG,47) CFG1(1:NFRQ)
       WRITE(NLG,46) KCMPC(CFG1(1:NFRQ))
       EXIT
     END IF
   END DO
 END IF

 IF (ABS (SURVEY) == 1) CALL WRITE_LOG_FILE (NLG,206,MXERR,2)
 IF (NSTAT < 2) CALL WRITE_LOG_FILE (NLG,15,MXERR,2)

!        SET SYSTEM DIMENSIONS
!        ---------------------
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

 ALLOCATE (LINE(NSTAT),SX(NSTAT),SY(NSTAT),SZ(NSTAT),FANGLE(NSTAT),BEARING(NSTAT),SxD(NSTAT),SYD(NSTAT), &
           RX(NSTAT,NRX),RY(NSTAT,NRX),RZ(NSTAT,NRX),RXD(NSTAT,NRX),RYD(NSTAT,NRX),SAME_TX(NSTAT))

 SX=0.; SY=0.; RX=0.; RY=0.; RZ = 0.
 RXD=0.; RYD=0.; FANGLE=0.

 IF (TDFD < 2) THEN
   IF (KCMP(1) /= 1 .AND. KCMP(1) /= 3 .AND. KCMP(1) /=13 .AND. KCMP(1) /=31 .AND. KCMP(1) /=123 &
                 .AND. KCMP(1) /=321) CALL WRITE_LOG_FILE (NLG,209,MXERR,2)
   IF (KCMP(1) < 100 .AND. CMP == 3) THEN
     MXERR = 2
     WRITE(NLG,101) KCMP(1)
   ELSE IF (KCMP(1) < 13) THEN
     IF (CMP == 2) THEN
       MXERR = 2
       WRITE(NLG,102)  KCMP(1)
     ELSE IF (CMP == 13 .AND. KCMP(1) /=3) THEN
       MXERR = 2
       WRITE(NLG,103) KCMP(1)
     ELSE IF (CMP == 11 .AND. KCMP(1) /=1) THEN
       MXERR = 2
       WRITE(NLG,104) KCMP(1)
     END IF
   END IF
   READ(NRI,*) DATA_FLOOR(1)
   WRITE(NW,22) ABS (DATA_FLOOR(1)),TRIM (QUNIT)
   DATA_FLOOR(1:MCHNL) = DATA_FLOOR(1)
   NDCMP = 3
   IF (KCMP(1) < 100) NDCMP = 2
   IF (KCMP(1) < 13) NDCMP = 1
   MDCHNL = NDCMP * NCHNL

 ELSE IF (TDFD == 2) THEN
   MDCHNL = MCHNL
   READ(NRI,*) DATA_FLOOR(1:2*NFRQ)

   WRITE(NW,23) TRIM (QUNIT)
   DO JP = 1,NFRQ
     WRITE(NW,24) JP,FREQ(JP), ABS (DATA_FLOOR(JP)), ABS (DATA_FLOOR(JP+NFRQ))
   END DO
 END IF

 NDATA = MCHNL * NSTAT
 ALLOCATE (RDATA(MCHNL,NSTAT),RWTS(MCHNL,NSTAT),XDATA(NDATA),XWTS(NDATA),XMODL(NDATA),DNORM(NDATA), &
           QDATA(MDCHNL),Q2DATA(MDCHNL))
 XDATA = 0.; RDATA = 0.; DNORM = 0; XWTS = 1;  RWTS = 1

 INRM = 1
 IF (DO3D == -2) INRM = 2

 READ(NRI,*)   N0STAT, N0CHNL, N0PTS
 WRITE(NW,25) N0STAT, N0CHNL, N0PTS

 IF (N0STAT > 0) THEN
   ALLOCATE (K0STAT(N0STAT))
   READ(NRI,*) K0STAT(1:N0STAT)
   WRITE(NW,26) K0STAT(1:N0STAT)
   DO J1 = 1, N0STAT
     RWTS(1:MCHNL,K0STAT(J1)) = 0
   END DO
   DEALLOCATE (K0STAT)
 END IF
 IF (N0CHNL > 0) THEN
   ALLOCATE (K0CHNL(N0CHNL))
   READ(NRI,*) K0CHNL(1:N0CHNL)
   WRITE(NW,27) K0CHNL(1:N0CHNL)
   DO J1 = 1, N0CHNL
     RWTS(K0CHNL(J1),1:NSTAT) = 0
   END DO
   DEALLOCATE (K0CHNL)
 END IF

 IF (N0PTS > 0) THEN
   ALLOCATE (K0STAT(N0PTS),K0CHNL(N0PTS))
   READ(NRI,*) (K0CHNL(J1),K0STAT(J1), J1=1,N0PTS)
   WRITE(NW,28)
   DO J1 = 1, N0PTS
     WRITE(NW,'(T3,2I4)') K0CHNL(J1),K0STAT(J1)
     RWTS(K0CHNL(J1),K0STAT(J1)) = 0
   END DO
   DEALLOCATE (K0STAT,K0CHNL)
 END IF

!======================
!      DATA ENTRY
!======================

 SAME_TX = .FALSE.
 DO JS = 1,NSTAT
   IF (ABS (SURVEY) == 2) THEN
     READ(NRI,*) ALINE,SYD(JS), SXD(JS), SZ(JS), QDATA(1:MDCHNL)
     LINE(JS) = FLOOR (ALINE)
   ELSE IF (ABS (SURVEY) == 3) THEN
     READ(NRI,*) ALINE,SYD(JS), SXD(JS), SZ(JS), TXCLN(JS), ZRX(JS), XRX(JS), YRX(JS), QDATA(1:MDCHNL)
     LINE(JS) = FLOOR (ALINE)
   END IF

   IF (JS > 1) THEN
     DELX = REAL (SXD(JS) - SXD(JS-1))
     DELY = REAL (SYD(JS) - SYD(JS-1))
     RHO = SQRT (DELX**2 + DELY**2)
     IF (RHO > 0.01) FANGLE(JS) = ATAN2 (DELY,DELX)
     A1 = ABS (SZ(JS) - SZ(JS-1))
     IF (A1 < 0.1) SAME_TX(JS) = .TRUE.
     IF (TDFD < 2) THEN
       A2 = ABS (TXCLN(JS) - TXCLN(JS-1))
       IF (A2 > 0.01) SAME_TX(JS) = .FALSE.
     END IF
   END IF


!  Put data in the order of all Z followed by all X followed by all Y
!  depending upon which components are present.

   IF (TDFD < 2) THEN
     N2B = NCHNL+1
     N2E = 2*NCHNL
     N3B = N2E + 1
     N3E = 3*NCHNL

     IF (KCMP(1) == 1 .OR. KCMP(1) == 3) Q2DATA(1:NCHNL) =  QDATA(1:NCHNL)
     IF (ORDER == 1) THEN
       IF (KCMP(1) == 31 .OR. KCMP(1) == 312) Q2DATA(1:N2E)   = QDATA(1:N2E)
       IF (KCMP(1) == 312)                 Q2DATA(N3B:N3E) = QDATA(N3B:N3E)
       IF (KCMP(1) == 13) THEN
         Q2DATA(1:NCHNL) = QDATA(N2B:N2E)
         Q2DATA(N2B:N2E) = QDATA(1:NCHNL)
       ELSE IF (KCMP(1) == 123) THEN
         Q2DATA(1:NCHNL) = QDATA(N3B:N3E)
         Q2DATA(N2B:N2E) = QDATA(1:NCHNL)
         Q2DATA(N3B:N3E) = QDATA(N2B:N2E)
       END IF
     ELSE IF (ORDER == 2) THEN
       DO JT = 1,NCHNL
         IF (KCMP(1) == 31) THEN
           Q2DATA(JT)       = QDATA(2*JT-1)
           Q2DATA(JT+NCHNL) = QDATA(2*JT)
         ELSE IF (KCMP(1) == 13) THEN
           Q2DATA(JT)       = QDATA(2*JT)
           Q2DATA(JT+NCHNL) = QDATA(2*JT-1)
         ELSE IF (KCMP(1) == 123) THEN
           Q2DATA(JT)         = QDATA (3*JT)
           Q2DATA(JT+NCHNL)   = QDATA (3*JT-2)
           Q2DATA(JT+2*NCHNL) = QDATA (3*JT-1)
         ELSE IF (KCMP(1) == 312) THEN
           Q2DATA(JT)         = QDATA (3*JT-2)
           Q2DATA(JT+NCHNL)   = QDATA (3*JT-1)
           Q2DATA(JT+2*NCHNL) = QDATA (3*JT)
         END IF
       END DO
     ELSE
       CALL WRITE_LOG_FILE (NLG,208,MXERR,2)
     END IF
     IF (CMP == 13) THEN
       RDATA(1:NCHNL,JS) = Q2DATA(1:NCHNL)
     ELSE IF (CMP == 11) THEN
       IF (NDCMP == 1) RDATA(1:NCHNL,JS) = Q2DATA(1:NCHNL)
       IF (NDCMP > 1)  RDATA(1:NCHNL,JS) = Q2DATA(N2B:N2E)
     ELSE IF (CMP == 2 .OR. CMP == 3) THEN
       RDATA(1:MCHNL,JS) = Q2DATA(1:MCHNL)
     ELSE IF (CMP == 4) THEN
       DO JT = 1,NCHNL
         TDATA = Q2DATA(JT)**2
         IF (NDCMP > 1)  TDATA = TDATA + Q2DATA(JT+NCHNL)**2
         IF (NDCMP == 3) TDATA = TDATA + Q2DATA(JT+2*NCHNL)**2
         RDATA(JT,JS) = SQRT (TDATA)
       END DO
     END IF
   ELSE IF (TDFD == 2) THEN      ! Store all in-phase data first and then all quadature data
     SELECT CASE (ORDER)
     CASE(1122)
       RDATA(1:2*NFRQ,JS) = QDATA(1:2*NFRQ)
     CASE(1212)
       DO JF = 1,NFRQ
         RDATA(JF,JS) = QDATA(2*JF-1)
         RDATA(JF+NFRQ,JS) = QDATA(2*JF)
       END DO
     CASE(2211)
       RDATA(1:NFRQ,JS) = QDATA(NFRQ+1:2*NFRQ)
       RDATA(NFRQ+1:2*NFRQ,JS) = QDATA(1:NFRQ)
     CASE(2121)
       DO JF = 1,NFRQ
         RDATA(JF,JS) = QDATA(2*JF)
         RDATA(JF+NFRQ,JS) = QDATA(2*JF-1)
       END DO
     CASE DEFAULT
       CALL WRITE_LOG_FILE (NLG,214,MXERR,2)
     END SELECT
   END IF
 END DO

! Frequency-domain TXDEG & TXCLN already established in READ_SYSTEM_AND_SURVEY
! for both modelling and inversion.

 TXDEG = TXCLN
 TXCLN = TXCLN * PI / 180.

 FANGLE(1) = FANGLE(2)   ! Bearing correction for start of new lines
 DO JS = 2,NSTAT-2
   IF (ABS (FANGLE(JS+1) - FANGLE(JS)) > TURN) FANGLE(JS+1) = FANGLE(JS+2)
 END DO
 BEARING = FANGLE * 180. / PI

 DEALLOCATE (QDATA,Q2DATA)

! Write the data and weights in blocked format to make checking easier.

 IF (TDFD < 2) THEN
   WRITE(NW,43)
   DO JS = 1,NSTAT
     WRITE(NW,40) LINE(JS),JS,BEARING(JS),SYD(JS),SXD(JS),SZ(JS),RDATA(1:NCHNL,JS) ! First component
   END DO
   IF (CMP == 2 .OR. CMP == 3) THEN
     N2B = NCHNL+1;  N2E = 2*NCHNL

     WRITE(NW,41)
     DO JS = 1,NSTAT
       WRITE(NW,40) LINE(JS),JS,BEARING(JS),SYD(JS),SXD(JS),SZ(JS),RDATA(N2B:N2E,JS) ! TD in-line data
     END DO
   END IF

   IF (CMP == 3) THEN
     N3B = 2*NCHNL+1;  N3E = 3*NCHNL
     WRITE(NW,42)
     DO JS = 1,NSTAT
       WRITE(NW,40) LINE(JS),JS,BEARING(JS),SYD(JS),SXD(JS),SZ(JS),RDATA(N3B:N3E,JS) ! TD transverse data
     END DO
   END IF

!  Weights

   WRITE(NW,22) ABS (DATA_FLOOR(1)),TRIM (QUNIT)
   DO JS = 1,NSTAT
     DO JT = 1,MCHNL
       IF (ABS(RDATA(JT,JS)) < DATA_FLOOR(1)) RWTS(JT,JS) = 0
     END DO
   END DO

   WRITE(NW,53)
   DO JS = 1,NSTAT
     WRITE(NW,50) JS,RWTS(1:NCHNL,JS)                 ! solo or vertical weights
   END DO

   IF (CMP == 2 .OR. CMP == 3) THEN
     N2B = NCHNL+1;  N2E = 2*NCHNL
     WRITE(NW,51)
     DO JS = 1,NSTAT
       WRITE(NW,50) JS,RWTS(N2B:N2E,JS)               ! TD in-line weights
     END DO
   END IF

   IF (CMP == 3) THEN
     N3B = 2*NCHNL+1;  N3E = 3*NCHNL
     WRITE(NW,52)
     DO JS = 1,NSTAT
       WRITE(NW,50) JS,RWTS(N3B:N3E,JS)               ! TD transverse weights
     END DO
   END IF
 ELSE IF (TDFD == 2) THEN
   WRITE(NW,60)
   DO JS = 1,NSTAT
     WRITE(NW,40) LINE(JS),JS,BEARING(JS),SYD(JS),SXD(JS),SZ(JS),RDATA(1:2*NFRQ,JS)
   END DO
   WRITE(NW,61)
   DO JS = 1,NSTAT
     DO JF = 1,NFRQ
       J1 = JF + NFRQ
       IF (ABS (RDATA(JF,JS)) < DATA_FLOOR(JF)) RWTS(JF,JS) = 0
       IF (ABS (RDATA(J1,JS)) < DATA_FLOOR(J1)) RWTS(J1,JS) = 0
     END DO
     WRITE(NW,50) JS,RWTS(1:2*NFRQ,JS)
   END DO
 END IF

  1 FORMAT(//T3,'-------------------------------------------------------------' &
            /T3,'Inversion Controls & Data for Plate Parameters using LeroiAir' &
            /T3,'-------------------------------------------------------------')
  2 FORMAT(/T3,'Inversion of Time-Domain Vertical Component Data')
  3 FORMAT(/T3,'Inversion of Time-Domain In-Line Component Data')
  4 FORMAT(/T3,'Joint Inversion of Time-Domain Vertical & In-Line Component Data')
  5 FORMAT(/T3,'Joint Inversion of Time-Domain Three Component Data')
  6 FORMAT(/T3,'The data to be inverted is expressed as ',A)
  7 FORMAT(/T3,'The data to be inverted has been normalised to ',A)
  8 FORMAT(/T3,'Inversion of Frequency-Domain Data')
  9 FORMAT(T3,'NPAR =',I3,3X,'MCHNL =',I3)
 10 FORMAT(T3,'MAXITS =',I3,3X,'CNVRG =',I2,3X,'NFIX =',I3,3X,'MV1PRT =',I2,3X,'OUTPRT =',I2)
 15 FORMAT(/T3,'The inversion will finish if the RMS error is less than',F5.1,' percent or if' &
           /T3,'the error can no longer be reduced significantly or after',I3,' iterations.')
 16 FORMAT(T3,'The manual derivative step has been fixed at',F5.1,' percent.')
 17 FORMAT(//T12,'Constrained Parameters'/T12,'----------------------' &
           //T5,'Global  Plate  Layer  Parameter'                                                        &
            /T5,'Index   Index  Index   Index      Parameter      Elasticity  Lower Bound   Upper Bound   CTYPE' &
            /T5,'------  -----  -----  ---------   ---------      ----------  -----------   -----------   -----')
 18 FORMAT(T7,I2,T14,I2,T30,I1,T38,A,T58,F6.2,T66,G12.4,T80,G12.4,T94,I3)
 19 FORMAT(/T3,'All plate and host parameters will be allowed to vary during inversion')
 20 FORMAT(/T3,90('-'))
 21 FORMAT(//T3,'NSTAT =',I4,3X,'SURVEY =',I2,3X,'BAROMTRC =',I2,3X,'KCMP =',I4,3X,'ORDER =',I2)
 22 FORMAT(/T3,'Time-Domain Data Floor =',G12.4,1X,A)
 23 FORMAT(/T8,'Frequency Data Floors (',A,')'/T8,'----------------------------' &
          //T8,'Freq     In-phase   Quadrature'/)
 24 FORMAT(I4,F9.0,2G12.4)
 25 FORMAT(/T3,'N0STAT =',I4,3X,'N0CHNL =',I3,3X,'N0PTS =',I4)
 26 FORMAT(/T3,'Data from the following stations will be weighted to zero:'/T3,60I4)
 27 FORMAT(/T3,'Data from the following PCHNLs will be weighted to zero:'/T3,60I4)
 28 FORMAT(/T3,'Data from the following (PCHNL, STAT) pairs will be weighted to zero:')
 29 FORMAT(T7,I2,T21,I2,T30,I1,T38,A,T58,F6.2,T66,G12.4,T80,G12.4,T94,I3)
 30 FORMAT(T7,I2,T21,I2,T30,I1,T38,A,T58,F6.2,T94,I3)
 31 FORMAT(T7,I2,T14,I2,T30,I1,T38,A,T58,F6.2,T94,I3)
 32 FORMAT(//T3,'NSTAT =',I4,3X,'SURVEY =',I2,3X,'BAROMTRC =',I2,3X,'ORDER = ',I4)
 33 FORMAT(/T3,'Inversion components:',20(I5,': ',A,F8.1))
 34 FORMAT(/T3,'Inversion components:',20(I5,': ',A,F10.1))
 40 FORMAT(I9,I7,F11.0,2F12.1,F9.1,300G13.4)
 41 FORMAT(//T6,'Line   Station   Bearing     East       North     Alt',T68,'Horizontal In-line Component Data' &
            /T6,'----   -------   -------     ----       -----     ---',T68,'----------------------------------')
 42 FORMAT(//T6,'Line   Station   Bearing     East       North     Alt',T68,'Horizontal Transverse Component Data' &
            /T6,'----   -------   -------     ----       -----     ---',T68,'------------------------------------')
 43 FORMAT(//T6,'Line   Station   Bearing     East       North     Alt',T68,'Vertical Component Data' &
            /T6,'----   -------   -------     ----       -----     ---',T68,'-----------------------')
 45 FORMAT(/T3,'Components from LeroiAir.inv:',I4,20I5)
 46 FORMAT(/T32,20(2X,A))
 47 FORMAT(/T3,'Components from LeroiAir.cfl:',I4,20I5)
 50 FORMAT(T1,I10,T13,300I2)
 51 FORMAT(/T3,'Station   Horizontal In-line Weights' &
           /T3,'-------   --------------------------')
 52 FORMAT(/T3,'Station   Horizontal Transverse Weights' &
           /T3,'-------   -----------------------------')
 53 FORMAT(/T3,'Station   Vertical Weights' &
           /T3,'-------   ----------------')
 60 FORMAT(/T6,'Line   Station   Bearing    East       North       Alt',T68, &
               'In-phase Data followed by Quadrature Data' &
           /T6,'----   -------   -------    ----       -----       ---',T68, &
               '----------------------------------------')
 61 FORMAT(/T3,'Station   In-phase Weights followed by Quadrature Weights'&
            /T3,'-------   ----------------------------------------------')
 101 FORMAT(T3,'Three cpomponent inversion has been specified (CMP = 3)' &
           /T3,'but three components are not read in: KCMP = ',I4)
 102 FORMAT(T3,'Two component inversion has been specified (CMP = 2)' &
           /T3,'but two components are not read in: KCMP = ',I4)
 103 FORMAT(T3,'Vertical component inversion has been specified (CMP = 13)' &
           /T3,'but this component is not read in: KCMP = ',I4)
 104 FORMAT(T3,'In-line component inversion has been specified (CMP = 11)' &
           /T3,'but this component is not read in: KCMP = ',I4)
 200 FORMAT(T3,'Constraint',I2,' ignored')

   END SUBROUTINE READ_INVRT_CNTRL_AND_DATA

   SUBROUTINE RESET_ORIGIN
!  -----------------------

!***  Called by: MAIN

! Set up body centred origin. Shift origin by (D_NORTH, D_EAST)

   IF (NPLT > 0) THEN
     D_EAST = YCNTRD(1)
     D_NORTH = XCNTRD(1)
     IF (MAX (ABS(D_EAST),ABS(D_EAST)) < 2000._QL) THEN
       D_NORTH = 0._QL
       D_EAST = 0._QL
     END IF
     XCNTR = REAL (XCNTRD - D_NORTH,4)   !  Set up old Leroi world
     YCNTR = REAL (YCNTRD - D_EAST,4)    ! 20170908 DWA: moved this under the If block. Otherwise, these arrays are not allocated and we throw an error
   END IF
   

   END SUBROUTINE RESET_ORIGIN

   SUBROUTINE SET_FRQ
!  ------------------

!***  Called by: MAIN

!  For time-domain options:
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
!  The need to go to 1 MHz depends upon receiver channels and conductivity.
!  A 10,000 ohm-m 1/2 space requires extended range if the earliest window
!  opening is within .28 ms of signal turn-off.  A 1 ohm-m 1/2 space allows
!  normal range if the first window open time is > .002 ms.
!
!  In a 3D program, this is pretty hard to control because of non-uniform
!  resistivity distribution so the safe option of going to 1 MHz is chosen
!  if the first window opens earlier than .28 ms after signal turn-off.  This
!  requires computation for 34 frequencies.
!
!  This can be over-ridden by setting TDFD = 0 in which case the user needs to 
!  specify five integers: KLO, KHI, KMD, PPD1, PPD2
!  
!    KLO - Lowest frequency expressed as an integer power of 10 
!          For example setting LOW = -1, 0 or 1 would mean that the lowest 
!          frequency would be either 0.1, 1.0, or 10.0 Hz repectively.
!
!    KHI - Highest frequency expressed as an integer power of 10 
!          For example setting HIGH = 5 or 6 would mean that the highest 
!          frequency would be either 0.1 or 1.0 MHz repectively.
!  
!    KMD - Frequencies below MID are spaced at PPD1 points per decade
!          Frequencies above MID are spaced at PPD2 points per decade
!  
!    The allowed values for both PPD1 and PPD2 are 3, 6 or 12.
!    If PPD1 < 3, it is changed to 3
!    If 3 < PPD1 < 6, it is changed to 6
!    If PPD1 > 6, it is set to 12
!  
!  Thus, depending upon the first channel time, for the default case of TDFD = 1,
!
!  KLO = 0,  KHI = 5,  KMD = 10,  PPD1 = 3 and PPD2 = 6 => NFRQ = 28   or
!  KLO = 0,  KHI = 6,  KMD = 10,  PPD1 = 3 and PPD2 = 6 => NFRQ = 34


 REAL RLO,RHI,RMD
 REAL(KIND=QL) QLO,QHI,QMD
 REAL(KIND=QL), ALLOCATABLE :: FDUM(:)

 ALLOCATE (FDUM(1000))
 IF (TDFD == 1) THEN                 ! Standard option
   KLO = 0
   KHI = 5
   KMD = 1
   PPD1 = 3
   PPD2 = 6
   T0 = MINVAL (TOPN) - SWX(NSX)
   IF (T0 < .28E-3) KHI = 6         ! Extend range to 1 MHz
 END IF

 RLO = 10.**KLO
 RHI = 10.**KHI
 RMD = 10.**KMD
 QLO = REAL (RLO,QL)
 QHI = REAL (RHI,QL)
 QMD = REAL (RMD,QL)
 QFRQ1 = EXP ( LOG (10.D0) / REAL (PPD1,QL) )
 QFRQ2 = EXP ( LOG (10.D0) / REAL (PPD2,QL) )


 IF (DO3D < 2) THEN
   IF (KMD == KLO) THEN
     WRITE(NW,1) RMD,RHI,PPD2,RHI,NPULS
   ELSE IF (KMD == KHI) THEN
     WRITE(NW,1) RLO,RMD,PPD1,RMD,NPULS
   ELSE
     WRITE(NW,2) RLO,RMD,PPD1,RMD,RHI,PPD2,RHI,NPULS
   END IF
 ELSE
   WRITE(NW,3)
 END IF

 FDUM(1) = QLO
 NFRQ = 1
 FQQ = REAL (FDUM(1),KIND=QL)

 DO J = 2,1000
   IF (FDUM(J-1) * QFRQ1 < QMD) THEN
     FQQ = FDUM(J-1) * QFRQ1
   ELSE
     FQQ = FDUM(J-1) * QFRQ2
   END IF
   IF (FQQ > 1.001 * QHI) EXIT
   NFRQ = J
   FDUM(J) = FQQ
 END DO

 ALLOCATE (FREQ(NFRQ))
 FREQ(1:NFRQ) = REAL (FDUM(1:NFRQ))
 DEALLOCATE (FDUM)

 1 FORMAT(/T3,'The frequency-domain results are directly computed from' &
          /T3,G12.4,' Hz to',G12.4,' Hz, using',I3,' points per decade.' &
         //T3,'These are used to construct the frequency-domain spectrum from DC to',G12.4,' Hz' &
          /T3,'before transformation to the time domain.  The transformed result is folded over' &
          /I3,' pulses and convolved with the input signal.')
 2 FORMAT(/T3,'The frequency-domain results are directly computed from' &
          /T3,G12.4,' Hz to',G12.4,' Hz, using',I3,' points per decade and from' &
          /T3,G12.4,' Hz to',G12.4,' Hz, using',I3,' points per decade.' &
         //T3,'These are used to construct the frequency-domain spectrum from DC to',G12.4,' Hz' &
          /T3,'before transformation to the time domain.  The transformed result is folded over' &
          /I3,' pulses and convolved with the input signal.')
 3 FORMAT(/T3,'LeroiAir will compute time-domain responses from the' &
          /T3,'frequency-domain data contained in file LeroiAir.frq')

   END SUBROUTINE SET_FRQ

   SUBROUTINE SET_SURVEY
!  ---------------------

!***  Called by: MAIN

!  Arrays are given in real world coordinates which require double precision.
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

!  Compute receiver coordinates in both body centred and real world systems.

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
           /T3,'"real world" coordinates to a local coordinate system where depth increases' &
           /T3,'positive downwards with the vertical origin at the air-earth interface.')
 2 FORMAT(/T3,'Barometric altitude will be changed to ground clearance.' &
          /T3,'The vertical origin is shifted down by',F6.1)
 3 FORMAT(/T3,'When either horizontal coordinate magnitude exceeds 2km, the origin is shifted'  &
          /T3,'horizontally to lie above the reference point of the first plate.'               &
          /T3,'This shift preserves precision without the need for higher precision arithmetic' &
         //T3,'The new computation origin is offset from the "real world" origin as follows:'   &
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

   SUBROUTINE WRITE_NP_INITIAL
!  ---------------------------

!***  Called by: MAIN

! Sets up the initial part of the output plotting file for inversion.

 INTEGER NCMP
 REAL VERR(MCHNL)
 LOGICAL WL
 CHARACTER(LEN=5) CHZ,CHX,CHY,CHT,WEZ,WEX,WEY,WET,WEQ,WEI
 CHARACTER(LEN=6),DIMENSION(NFRQ) :: QFRQ,IFRQ
 CHARACTER(LEN=7) RES_MOD(9), THK_MOD(9)
 CHARACTER(LEN=85) PLT_MOD(9), QL0,QL1
 DATA RES_MOD /'  RES_1','  RES_2','  RES_3','  RES_4','  RES_5','  RES_6','  RES_7','  RES_8','  RES_9'/
 DATA THK_MOD /'  THK_1','  THK_2','  THK_3','  THK_4','  THK_5','  THK_6','  THK_7','  THK_8','  THK_9'/
 DATA PLT_MOD /'  SIG_T_1  DEPTH_1  PLNGTH_1  DIP_WDTH_1  EAST_1  NORTH_1  DIP_AZM_1  DIP_1  PLUNGE_1', &
               '  SIG_T_2  DEPTH_2  PLNGTH_2  DIP_WDTH_2  EAST_2  NORTH_2  DIP_AZM_2  DIP_2  PLUNGE_2', &
               '  SIG_T_3  DEPTH_3  PLNGTH_3  DIP_WDTH_3  EAST_3  NORTH_3  DIP_AZM_3  DIP_3  PLUNGE_3', &
               '  SIG_T_4  DEPTH_4  PLNGTH_4  DIP_WDTH_4  EAST_4  NORTH_4  DIP_AZM_4  DIP_4  PLUNGE_4', &
               '  SIG_T_5  DEPTH_5  PLNGTH_5  DIP_WDTH_5  EAST_5  NORTH_5  DIP_AZM_5  DIP_5  PLUNGE_5', &
               '  SIG_T_6  DEPTH_6  PLNGTH_6  DIP_WDTH_6  EAST_6  NORTH_6  DIP_AZM_6  DIP_6  PLUNGE_6', &
               '  SIG_T_7  DEPTH_7  PLNGTH_7  DIP_WDTH_7  EAST_7  NORTH_7  DIP_AZM_7  DIP_7  PLUNGE_7', &
               '  SIG_T_8  DEPTH_8  PLNGTH_8  DIP_WDTH_8  EAST_8  NORTH_8  DIP_AZM_8  DIP_8  PLUNGE_8', &
               '  SIG_T_9  DEPTH_9  PLNGTH_9  DIP_WDTH_9  EAST_9  NORTH_9  DIP_AZM_9  DIP_9  PLUNGE_9'/

 WRITE(NP,1) FVERS,trim(PNAME),TRIM(TITLE)
 IF (TDFD < 2) THEN
   NCMP = CMP
   IF (CMP > 10) NCMP = 1
   CHZ = '  CHZ';  WEZ = '  WEZ'
   CHX = '  CHX';  WEX = '  WEX'
   CHY = '  CHY';  WEY = '  WEY'
   CHT = '  CHT';  WET = '  WET'

   WRITE(NP,3) TRIM (QUNIT),NSTAT,NCHNL,NCMP
   WRITE(NP,4) TMS(1:NCHNL)
   WRITE(NP,5) WTMS(1:NCHNL)
   WRITE(NP,6)
   ! WRITE(NP,8) NLYR,NPLT
   IF (INVERT) THEN
     IF (CMP ==11) WRITE(NP,7) (CHX,JT,JT=1,NCHNL), (WEX,JT,JT=1,NCHNL)
     IF (CMP ==13) WRITE(NP,7) (CHZ,JT,JT=1,NCHNL), (WEZ,JT,JT=1,NCHNL)
     IF (CMP ==2)  WRITE(NP,7) (CHZ,JT,JT=1,NCHNL), (CHX,JT,JT=1,NCHNL), &
                                (WEZ,JT,JT=1,NCHNL), (WEX,JT,JT=1,NCHNL)
     IF (CMP ==3)  WRITE(NP,7) (CHZ,JT,JT=1,NCHNL), (CHX,JT,JT=1,NCHNL), (CHY,JT,JT=1,NCHNL), &
                                (WEZ,JT,JT=1,NCHNL), (WEX,JT,JT=1,NCHNL), (WEY,JT,JT=1,NCHNL)
     IF (CMP ==4)  WRITE(NP,7) (CHT,JT,JT=1,NCHNL), (WET,JT,JT=1,NCHNL)
   ELSE
     IF (CMP ==11) WRITE(NP,7) (CHX,JT,JT=1,NCHNL)
     IF (CMP ==13) WRITE(NP,7) (CHZ,JT,JT=1,NCHNL)
     IF (CMP ==2)  WRITE(NP,7) (CHZ,JT,JT=1,NCHNL), (CHX,JT,JT=1,NCHNL)
     IF (CMP ==3)  WRITE(NP,7) (CHZ,JT,JT=1,NCHNL), (CHX,JT,JT=1,NCHNL), (CHY,JT,JT=1,NCHNL)
     IF (CMP ==4)  WRITE(NP,7) (CHT,JT,JT=1,NCHNL)
   END IF
 ELSE
   WEQ = '  WEQ'
   WEI = '  WEI'
   DO JF = 1,NFRQ
     QFRQ(JF) = '  Q'//CONFIG(JF)
     IFRQ(JF) = '  I'//CONFIG(JF)
   END DO

   WRITE(NP,13) TRIM (QUNIT),NSTAT,NFRQ
   WRITE(NP,14) FREQ(1:NFRQ)
   WRITE(NP,16)
   IF (INVERT) THEN
     WRITE(NP,17) (IFRQ(JF),JF,JF=1,NFRQ),(QFRQ(JF),JF,JF=1,NFRQ), &
                   (WEI,JF,JF=1,NFRQ),(WEQ,JF,JF=1,NFRQ)
   ELSE
     WRITE(NP,17) (IFRQ(JF),JF,JF=1,NFRQ),(QFRQ(JF),JF,JF=1,NFRQ)
   END IF
   ! WRITE(NP,8) NLYR,NPLT
 END IF

 !
 ! write model
 Write (np, 8) nlith, nlyr, nplt
 Write (np, 27) 
 Do jl = 1, nlith
    Write (np, 23) jl, lyth(jl, 1:7)
 End Do
 Write (np, 30); Write (np, 28)
 Do jl = 1, nlyr - 1
    Write (np, 24) jl, res(jl), thk(jl), thk(jl)/res(jl)
 End Do
 Write (np, 26) nlyr, res(nlyr)
 Write (np, 30); Write (np, 29)
 Do jl = 1, nplt
    Write (np, 25) jl, LITH(jl),YCNTRD(jl),XCNTRD(jl),PLTOP(jl), PLNGTH(jl),PLWDTH(jl),AZM(jl),DIP(jl), PLG(jl)
 End Do
 Write (np, 30)
 ! Write (np, *) '/ Background: ', res(1:nlyr - 1), thk(1:nlyr - 1), res(nlyr)
 ! Write (np, *) '/ Target:     ', mpar(1:nplt)
 ! WRITE(NP,20) PLT_MOD(1:NPLT),RES_MOD(1:NLYR),THK_MOD(1:NLYR-1)

 IF (INVERT) THEN
   WRITE(NP,'(T1,A)') '/ SURVEY DATA'

   VERR = 0.
   DO JS = 1,NSTAT
     WL = .TRUE.
     IF (JS > 1 .AND. LINE(JS) == LINE(JS-1)) WL = .FALSE.
     IF (WL)  THEN
       WRITE(QL0,*) LINE(JS)
       READ(QL0,'(A)') QL1
       WRITE(NP,11) TRIM (ADJUSTL (QL1))
     END IF
     IF (TDFD < 2) THEN
       WRITE(NP,9) JS,SYD(JS),SXD(JS),SZ(JS),TXDEG(JS),RYD(JS,1),RXD(JS,1),RZ(JS,1), &
                    RDATA(1:MCHNL,JS),VERR(1:MCHNL)
     ELSE
       WRITE(NP,10) JS,SYD(JS),SXD(JS),SZ(JS),RYD(JS,1),RXD(JS,1),RZ(JS,1), &
                     RDATA(1:MCHNL,JS),VERR(1:MCHNL)
     END IF
   END DO
 ! ELSE
 !   WRITE(NP,22) lyth(1:nlyr, 1), THK(1:nlyr-1)  ! 20170908 DWA: was MPAR ... which is undefined if we are in layered-earth mode.
 END IF

  1 FORMAT(T1,'/ ',I4.4,T15,'File version number'/T1,'/ PROGRAM_NAME=',A/T1,'/ TITLE: ',A)
  3 FORMAT(T1,'/ UNITS=',A,3X,'NSTAT=',I3.3,3X,'NCH=',I3.3,3X,'NCMP=',I1)
  4 FORMAT(T1,'/ TIMES(ms)=',T17,8192G13.4)
  5 FORMAT(T1,'/ CHNL_WDTH(ms)=',T17,8192G13.4)
  6 FORMAT(T1,'/ SURVEY=TD_AEM  PP=RX_POS')
  7 FORMAT(T1,'/ LINE_HEADER'/T1,'/ DatIndx  EastTx  NorthTx  AltTx  Txcln  EastRx  NorthRx  AltRx',8192(A,I3.3))
  ! 8 FORMAT(T1,'/', /, '/ Lithologies=',I2.2 /, '/ Layers=',I2.2/T1,'/ Targets=',I2.2)
  8 Format('/', /, &
           '/ Model', /, &
           '/ -----', /, &
           '/ Lithologies: ', i4, /, &
           '/ Layers:      ', i4, /, &
           '/ Targets:     ', i4)
  9 FORMAT(T1,I5,2F12.1,F8.1,F6.1,2F12.1,F8.1,8192en16.6:)
 10 FORMAT(T1,I5,2F12.1,F8.1,2F12.1,F8.1,8192en16.6:)
 11 FORMAT(T2,'Line ',A)
 13 FORMAT(T1,'/ UNITS=',A,3X,'NSTAT=',I3.3,3X,'NFRQ=',I2.2,3X,'NCMP=1')
 14 FORMAT(T1,'/ FREQS(Hz) =',60G13.4)
 16 FORMAT(T1,'/ SURVEY=FD_AEM  PP=RX_POS')
 17 FORMAT(T1,'/ LINE_HEADER'/T1,'/ DatIndx  EastTx  NorthTx  AltTx  EastRx  NorthRx  AltRx  ',8192(A,I2.2))
 ! 20 FORMAT(T1,'/ MODEL_HEADER'/T1,'/ ',12A)
 ! 22 FORMAT(T1,'/'/T1,'/ MODEL_00', 1024e13.4)
 23 Format ('/ Lithology: ', i2, 7(2x, en13.4))
 24 Format ('/ Layer:     ', i2, 3(2x, en13.4))
 26 Format ('/ Base.:     ', i2, 2(2x, en13.4))
 25 Format ('/ Target:    ', i2, 4x, i2, 8(2x, en13.4))
 27 Format ('/             #', 4x, 'Resistivity', 9x, 'SigmaT', 9x, 'Rel.mu', 8x, 'Rel.eps', 8x, &
    'Charge.', 12x, 'Tau', 10x, 'Freq.')
 28 Format ('/             #', 4x, 'Resistivity', 6x, 'Thickness', 4x, 'Conductance')
 29 Format ('/             #', 1x, 'Lith.', 11x, 'East', 10x, 'North', 10x, 'Depth', 9x, &
                                   'Length', 10x, 'Width', 9x, 'Strike', 12x, 'Dip', 9x, 'Plunge')
 30 Format ('/')

   END SUBROUTINE WRITE_NP_INITIAL

END MODULE LA_Input_routines

 PROGRAM MAIN
!------------

!*** Calls DCPRM_FD, DCPRM_TD, FDREAD, HSBOSS_TD, HSBOSS_FD, LEROI_3D, NLSQ2,
!          SET_NORM_TD, SET_SOURCE, TDEM_3D, WRITE_FD, WRITE_TD, WRITE_MODEL,
!          WRITE_LOG_FILE

!*** Calls from LA_Input_routines:
!          READ_SYSTEM_AND_SURVEY, READ_MODEL, READ_INVRT_CNTRL_AND_DATA
!          RESET_ORIGIN, SET_CELLS, SET_FRQ, SET_SURVEY, SET_TRP, WRITE_NP_INITIAL

 USE LA_Input_routines

 IMPLICIT NONE
 INTEGER IDER, KPR,KNRM,JD
 REAL, ALLOCATABLE, DIMENSION(:) :: NORM
 REAL, ALLOCATABLE, DIMENSION(:,:,:) :: BTD,BTD_SCAT
 COMPLEX,ALLOCATABLE, DIMENSION(:,:,:) :: BFD_SCAT,BFD
 REAL CMP_Start, CMP_Final, CMP_Delta, S1, BIG
 LOGICAL INV_FAIL, WRT_NP

 CALL CPU_TIME (CMP_Start)
 CALL READ_SYSTEM_AND_SURVEY
 CALL READ_MODEL

 IF (INVERT) THEN
   OPEN(NRI,FILE = 'LeroiAir.inv',STATUS = 'OLD')
   OPEN(NP,FILE = 'LeroiAir.mv1',STATUS = 'REPLACE')
   CALL READ_INVRT_CNTRL_AND_DATA
 ELSE
   OPEN(NP,FILE = 'LeroiAir.mf1',STATUS = 'REPLACE')
 END IF
 CALL RESET_ORIGIN

 IF (DO3D /= 0) THEN
   IF (TDFD < 2) CALL SET_FRQ    ! Set up frequencis for time-domain work.
   KP = 0
   CALL SET_CELLS (KP,NPLT,NLYR,MXAB,MXB,CELLW,THK,PLNGTH,PLWDTH,NA,NB,DA,DB, &
                   XCNTR,YCNTR,PLTOP,PLAZM,PLDIP,PLUNJ,XCELL,YCELL,ZCELL,PCNR)
 END IF

 CALL SET_SURVEY
 WRT_NP = .TRUE.
 IF (TDFD == 2 .AND. CMP > 1)  WRT_NP = .FALSE.
 IF (WRT_NP) CALL WRITE_NP_INITIAL
 KPR = 3
 CALL WRITE_MODEL (NW,KPR,INVERT,NLYR,RES,THK,CHRG,CTAU,CFREQ,RMU,REPS,NPLT,MXAB,    &
                   MXB,SIG_T,CHRGP,CTAUP,CFREQP,PLNGTH,PLWDTH,D_NORTH,D_EAST,GND_LVL, &
                   XCNTR,YCNTR,PLTOP,PLAZM,PLDIP,PLUNJ,PCNR,NA,NB,XCELL,YCELL,ZCELL)

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

     CALL SET_NORM_TD (NW,BUNIT,BFFAC,KPPM,PUNIT,PPFAC,PRM_TD,NORM)

     IF (INVERT) CALL SET_NORM_TD (NW,BUNIT,BFFAC,KPPM,PUNIT,PPFAC,PRM_TD,NORM)
   END IF
 ELSE
   KNRM = NFRQ
   ALLOCATE (NORM(KNRM))
   CALL DCPRM_FD (NFRQ,XRX,YRX,ZRX,TXCLN,TXA90,PRM_FD,PPFAC,NORM)
 END IF

!============================================================================
 IF (INVERT) THEN    ! Construct the observed data and data weight vectors.

   DO JS = 1,NSTAT
     DO JT = 1,MCHNL
       JD = JT + (JS-1) * MCHNL
       XDATA(JD) = RDATA(JT,JS)
       XWTS(JD) = RWTS(JT,JS)
     END DO
   END DO

   BIG = MAXVAL (ABS (XDATA) )
   DO JT = 1,MCHNL
     S1 = 0.
     DO JS = 1, NSTAT
       S1 = S1 + ABS (RDATA(JT,JS))
     END DO
     S1 = S1 / REAL (NSTAT)
     S1 = MAX (S1, DATA_FLOOR(JT))
     IF (S1 < 1.0E-7 * BIG) S1 = 1.0E7 * BIG  ! Eliminate cross-over fluctuations

     DO JS = 1, NSTAT
       JD = JT + (JS-1) * MCHNL
       DNORM(JD) = S1
     END DO
   END DO

   CALL NLSQ2 (NW,NP,MV1PRT,OUTPRT,MAXITS,CNVRG,PCTCNV,NDATA,DNORM,XDATA,XMODL,XWTS,NPAR,   &
               CXPAR,ELAS,LBND,UBND,TDFD,CMP,INRM,KNRM,NORM,STEP,IDER,NSX,SWX,SWY,NTYRP,TRP, &
               NPULS,PULSE,NTYPLS,MCHNL,NCHNL,TOPN,TCLS,GSTRP,ASTRP,NFRQ,FREQ,TXCLN,TXA90,   &
               NRX,NRXST,XRX,YRX,ZRX,NSTAT,SX,SY,SZ,FANGLE,SAME_TX,RX,RY,RZ,D_NORTH,D_EAST,  &
               GND_LVL,TITLE,LINE,INV_FAIL,                                                  &
               NLYR,RES,THK,RMU,REPS,CALF,CTAU,CFREQ,NPLT,MXB,MXAB,CELLW,SIG_T,XCNTR,        &
               YCNTR,PLTOP,PLNGTH,PLWDTH,PLAZM,PLDIP,PLUNJ,CALFP,CTAUP,CFREQP,PARPCT)

!===========================================================================================

 ELSE         ! MODELLLING OPTION

   CLOSE (NR)
   ALLOCATE (BFD_SCAT(NFRQ,NSTAT,3))
   BFD_SCAT = (0.,0.)

   NEW_3D_MODEL: IF (DO3D == 1) THEN

     CALL LEROI_3D (TDFD,NFRQ,FREQ,NLYR,THK,RES,RMU,REPS,CALF,CTAU,CFREQ,NPLT,MXAB,  &
                    MXB,NB,NA,DA,DB,XCELL,YCELL,ZCELL,PLTOP,PLWDTH,PLNGTH,XCNTR,YCNTR, &
                    PLAZM,PLDIP,SIG_T,CALFP,CTAUP,CFREQP,NSTAT,FANGLE,SX,SY,SZ,TXCLN,  &
                    TXA90,SAME_TX,NRX,NRXST,RX,RY,RZ,INVERT,BFD_SCAT,NW)


!  End of frequency stepping.
!  Write out the total frequency-domain scattered magnetic fields for each to UNIT ND.

     IF (TDFD < 2) THEN  ! Time-Domain
       DO JF = 1,NFRQ
         DO JS= 1,NSTAT
           WRITE(ND,'(6E16.6)') BFD_SCAT(JF,JS,1:3)
         END DO
       END DO

       CLOSE (ND)
       Write (*, 10)
     END IF
   END IF NEW_3D_MODEL

   IF (TDFD < 2) THEN   ! Time-Domain.  Compute BTD_SCAT, the Scattering Response
     ALLOCATE (BTD_SCAT(NCHNL,NSTAT,3),BTD(NCHNL,NSTAT,3))
     BTD_SCAT = 0.;  BTD = 0.

     IF (DO3D > 0) THEN
       IF (DO3D == 2) CALL FDREAD (ND,NFRQ,NSTAT,BFD_SCAT)  !  Read old frequency-domain data
       CLOSE (ND)

       CALL TDEM_3D (STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL, &
                     TOPN,TCLS,FREQ,NFRQ,NSTAT,BFD_SCAT,GSTRP,ASTRP,BTD_SCAT)

     END IF

!  Compute BTD, the layered earth response convolved with the excitation waveform
!  as dB/dt in nT/s if STEP = 0;  or as B in pT if STEP = 1

     CALL HSBOSS_TD (STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL,TOPN, &
                     TCLS,TXCLN,NSTAT,SAME_TX,SZ,ZRX,XRX,YRX,NLYR,RES,REPS,RMU,THK, &
                     CALF,CTAU,CFREQ,GSTRP,ASTRP,BTD)

     BTD = BTD + BTD_SCAT    !  Redefine BTD as the total response,

!  Write out the results.

     CALL WRITE_TD (NW,NP,TITLE,NSTAT,LINE,SXD,SYD,SZ,TXDEG,RXD,RYD,RZ,XRX,YRX,ZRX,NCHNL, &
                    TMS,PRFL,QUNIT,BUNIT,PPFAC,BFFAC,PRTSEC,PRM_TD,CMP,KPPM,BTD_SCAT,BTD)

   ELSE  !  Construct the frequency-domain response.

     BFD_SCAT = - BFD_SCAT
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
     CALL WRITE_FD (NW,NP,TITLE,NSTAT,LINE,TXCLN,TXA90,SXD,SYD,SZ,RXD,RYD,RZ,CONFIG,NFRQ, &
                    FREQ,PRFL,QUNIT,PPFAC,BFFAC,PRTSEC,PRM_FD,CMP,BFD_SCAT,BFD)
   END IF
 END IF

 !
 ! complete run time calcs & sign off ...
 call date_and_time(Values = tvals)
 call CPU_time(CMP_final)
 CMP_delta = CMP_final - CMP_start

 Select Case (Invert)
 Case (.True.)
    Write (np, 11) trim(PNAME), 'inversion', tvals(1:3), tvals(5:7), CMP_delta
    Write (nw, 12) trim(PNAME), 'inversion', tvals(1:3), tvals(5:7), CMP_delta
    Write ( *, 12) trim(PNAME), 'inversion', tvals(1:3), tvals(5:7), CMP_delta
 Case (.False.)
    Write (np, 11) trim(PNAME), 'forward model', tvals(1:3), tvals(5:7), CMP_delta
    Write (nw, 12) trim(PNAME), 'forward model', tvals(1:3), tvals(5:7), CMP_delta
    Write ( *, 12) trim(PNAME), 'forward model', tvals(1:3), tvals(5:7), CMP_delta
 End Select

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


 USE LA_Filter_coefficients_QL

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

 SUBROUTINE FIX_AZM (NPLT,PLAZM)
!------------------------------

!***  Called by: CNVRT2_MPAR, CNVRT2_XPAR

! Puts plate azimuth (PLAZM), into appropriate range


 REAL, PARAMETER :: PI=3.141592654, TWOPI=6.283185307
 INTEGER NPLT,JP
 REAL PLAZM(NPLT)

 DO JP = 1,NPLT
  PLAZM(JP) = MOD (PLAZM(JP), TWOPI)
  IF (PLAZM(JP) >  TWOPI) PLAZM(JP) = PLAZM(JP) - TWOPI
  IF (PLAZM(JP) < -PI) PLAZM(JP) = PLAZM(JP) + TWOPI
 END DO

 END SUBROUTINE FIX_AZM

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

 USE LA_Frequency_select

 IMPLICIT NONE
 INTEGER, PARAMETER :: NFRQ=NF_6PDE, NRXF=1, QL=SELECTED_REAL_KIND(p = 18)
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
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
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

 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 REAL, PARAMETER :: TWOPI=6.2831853, C_LIGHT = 2.99793E8
 COMPLEX(KIND=QL), PARAMETER :: ZERO=(0._QL,0._QL)
 INTEGER NRXF,NFRQ,NLYR,ICOLE(NLYR),JF,JL,TDFD,JQ
 REAL W,RES(NLYR),FREQ(NFRQ),ALT
 REAL(KIND=QL) SIG0(NLYR),THKD(NLYR),RMUX(NLYR),SNTX,CSTX,ZRFD,RHOD,XBRQ,XBRQ2,YBRQ
 REAL(KIND=QL), DIMENSION(NRXF) :: TXCLND,ZRXD,XRXD,YRXD
 REAL, DIMENSION(NLYR) :: THK,REPS,RMU,CTAU,CFREQ,CALF
 COMPLEX(KIND=QL) IW,DISPD,HLYR(3), BFDD(NFRQ,3),VERT,INLINE,TRANS
 LOGICAL TXA90
 Logical :: WRITE_FRQ = .True.

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
 
 If(WRITE_FRQ) Then
    Open(Unit = 66, File = 'LeroiAir.flayer', Status = 'Unknown')
    Do jf = 1, nfrq
        Write (66, 1) freq(jf), bfdd(jf, 1), bfdd(jf, 2), bfdd(jf, 3)
    End Do
    Close(66)
 End If
 
1   Format (en13.4, 6(2x, en15.6)) 

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
!    OUTPUT is HLYR(1:3)  model components

 USE LA_Filter_coefficients_QL

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
!  QFD  model kernel
!

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
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

 SUBROUTINE SET_NORM_TD (NW,BUNIT,BFFAC,KPPM,PUNIT,PPFAC,PRM_TD,NORM)
!--------------------------------------------------------------------

!*** Called by: MAIN
!
!   LeroiAir computes all fields in nT or nT/s.  In order to match field data for
!   inversion, it computes factors (NORM) to convert computed data into pT, pT/s,
!   fT, fT/s, pct, ppt, ppm, ppb as required.
!
!             Input
!             -----
!      BUNIT, BFAC, PUNIT & PPFAC are set in SUBROUTINE READ_SYSTEM_SETUP
!      BUNIT can have values nT/s, nT, pT, pT/s, fT or fT/s
!      BFFAC is the conversion factor needed to achieve this from nT or nT/s
!            = 1, 1000, or 1E6
!
!
!
!      KPPM = 0   => No PPM normalisation (automatic if ISW = 4)
!      KPPM = 1   => All components are normalised to in-line primary field
!      KPPM = 3   => All components are normalised to vertical primary field
!      KPPM = 123 => Vertical component normalised to the vertical primary &
!      KPPM = 4   => All components are normalised to total primary field
!
!      For KPPM > 0:
!        PUNIT can have values pct, ppt, ppm or ppb; eg,
!           parts per hundred (percent)
!           parts per thousand
!           parts per million
!           parts per billion
!
!      PPFAC = 1e2, 1e3, 1e6 or 1e9 is the conversion factor to achieve this
!
!   PRM_TD(I) - peak primary dB/dt in nT/s if STEP = 0 or
!               peak primary B (in nT if STEP = 1)
!               I = 1, 2 & 3 are the in-line, transverse & vertical components.
!
!             Output
!             ------
!
!       NORM - Factor to convert time-domain response in nT or nT/s into relevant units.

 IMPLICIT NONE
 INTEGER NW,KPPM,J
 REAL PRM_TD(3),NORM(3),PTD(4),PPFAC,BFFAC,XQ(4)
 LOGICAL NO(3)
 CHARACTER (LEN=4) BUNIT,PUNIT

 NO = .FALSE.
 PTD(1:3) = ABS (PRM_TD(1:3))
 PTD(4) = SQRT (PTD(1)**2 + PTD(2)**2 + PTD(3)**2)
 XQ(1:3) = BFFAC * PRM_TD(1:3)
 XQ(4) = BFFAC * PTD(4)
 DO J = 1,3
   IF (PTD(J) < 1.E-3*PTD(4) ) THEN
     PTD(J) = PTD(4)
     NO(J) = .TRUE.
   END IF
 END DO

 WRITE(NW,5) PUNIT, BUNIT, XQ(3),XQ(1),XQ(2),XQ(4)

!  Primary fields, PRM_TD, are computed in nT, NORM, the normalisation.  If the
!  field data are not in nT, NORM, must be converted to pT or fT as required.

 IF (KPPM == 0) THEN
   NORM = BFFAC
 ELSE IF (KPPM == 1) THEN
   NORM = PPFAC / PTD(1)
   IF (NO(1)) THEN
     WRITE(NW,4) PUNIT,NORM(1)
   ELSE
     WRITE(NW,1) PUNIT,NORM(1)
   END IF
 ELSE IF (KPPM == 3) THEN
   NORM = PPFAC / PTD(3)
   IF (NO(3)) THEN
     WRITE(NW,4) PUNIT,NORM(3)
   ELSE
     WRITE(NW,3) PUNIT,NORM(3)
   END IF
 ELSE IF (KPPM == 4) THEN
   NORM = PPFAC / PTD(4)
   WRITE(NW,4) BUNIT,NORM(1)
 ELSE IF (KPPM == 123) THEN
   NORM(1:3) = PPFAC / PTD(1:3)
   WRITE(NW,2) NORM(1:3)
 END IF

 1 FORMAT(/T11,'Each component is normalised to the in-line primary field' &
          /T26,'In-line ',A,' norm =',G12.4)
 2 FORMAT(/T11,'Each component is normalised to its corresponding primary field' &
          /T23,'   In-line norm =',G12.4 &
          /T23,'Transverse norm =',G12.4 &
          /T23,'  Vertical norm =',G12.4)
 3 FORMAT(/T11,'Each component is normalised to the vertical primary field' &
          /T25,'Vertical ',A,' norm =',G12.4)
 4 FORMAT(/T3,T11,'Each component is normalised to the total primary field' &
          /T28,'Total ',A,' norm =',G12.4)
 5 FORMAT(//T31,'Normalisation in ',A/T31,'Primary field units are ',A &
          //T20,'  Vertical primary =',G12.4 &
           /T20,'   In-line primary =',G12.4 &
           /T20,'Transverse primary =',G12.4 &
           /T20,'     Total primary =',G12.4)

 END SUBROUTINE SET_NORM_TD

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
                     TOPN,TCLS,FREQ,NFRQ,NSTAT,BFD_SCAT,GSTRP,ASTRP,BTD_SCAT)
!----------------------------------------------------------------------------

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
 REAL PULSE,FREQ(NFRQ),T0_MIN,WF(NFRQ),SWX(NSX),SWY(NSX,3),COSTRN,T,YCUM(NCHNL), &
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

 IF (MXERR == 0) OPEN (NLG,FILE = 'LeroiAir.log',STATUS = 'REPLACE')

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

 IF (MSG == 200) WRITE(NLG,200)
 IF (MSG == 201) WRITE(NLG,201)
 IF (MSG == 202) WRITE(NLG,202)
 IF (MSG == 203) WRITE(NLG,203)
 IF (MSG == 204) WRITE(NLG,204)
 IF (MSG == 205) WRITE(NLG,205)
 IF (MSG == 206) WRITE(NLG,206)
 IF (MSG == 207) WRITE(NLG,207)
 IF (MSG == 208) WRITE(NLG,208)
 IF (MSG == 209) WRITE(NLG,209)
 IF (MSG == 210) WRITE(NLG,210)
 IF (MSG == 211) WRITE(NLG,211)
 IF (MSG == 212) WRITE(NLG,212)
 IF (MSG == 213) WRITE(NLG,213)
 IF (MSG == 214) WRITE(NLG,214)
 IF (MSG == 215) WRITE(NLG,215)
 IF (MSG == 220) WRITE(NLG,220)
 IF (MSG == 221) WRITE(NLG,221)
 IF (MSG == 222) WRITE(NLG,222)
 IF (MSG == 223) WRITE(NLG,223)
 IF (MSG == 224) WRITE(NLG,224)
 IF (MSG == 225) WRITE(NLG,225)
 IF (MSG == 226) WRITE(NLG,226)

  1 FORMAT(/T3,'The value for TDFD is outside the permitted range.' &
           /T3,'The allowed values are: 0 & 1 for time-domain or 2 for frequency domain.'/)
  2 FORMAT(/T3,'The value for DO3D is outside the permitted range.' &
           /T3,'It has been reset to 1 for modelling'/)
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
 15 FORMAT(/T3,'LeroiAir requires a minimum of 2 stations in a survey')
 16 FORMAT(/T3,'SURVEY must be either 1, 2, 3, -1, -2, or -3 for time-domain.' &
           /T3,'SURVEY = 3 or -3 cannot be used for frequency-domain because'  &
           /T3,'Tx-Rx offset must be constant as a function of position')
 17 FORMAT(/T3,'IUNITS must be 1, 2 or 3')
 18 FORMAT(/T3,'CMP has been set to 1 for the vertical coplanar broadside option.')
 19 FORMAT(/T3,'IUNITS must be 1, 2 or 3.  It has been reset to the default')

!  Model Messages

 50 FORMAT(/T3,'LeroiAir inversion is intended for models of 1 to 9 plates.'         &
           /T3,'You have specified either too many or a negative number of plates.' &
           /T3,'LeroiAir does not wish to perform such tasks.')
 51 FORMAT(/T3,'LeroiAir inversion is intended for models with 1 or more plates.' &
           /T3,'You have set NPLATE = 0.   For layered earth inversion, use Airbeo.')
 53 FORMAT(/T3,'A lithology must have a positive first component (resistivity) if it is to be' &
           /T3,'applied to a layer.  It must have a positive second component (conductance)'   &
           /T3,'if it is to be aplied to a plate.  You have incorrectly specified both    '    &
           /T3,'conductance and resistivity as negative for one of the lithologies')
 54 FORMAT(/T3,'LITH must be an integer between 1 & NLITH')
 55 FORMAT(/T3,'Layer resistivities must be positive.')
 56 FORMAT(/T3,'This version of LeroiAir does not allow the plate to extend into' &
           /T3,'the air or overburden')
 57 FORMAT(/T3,'Plate conductance must be positive.')
 58 FORMAT(/T3,'Negative DIP or DIP > 180 degrees is not allowed.')
 59 FORMAT(/T3,'PLG must be in the interval -90 degrees to + 90 degrees.')

! Inversion messages
! -------------------

 200 FORMAT(2X)
 201 FORMAT(/T3,'This version of LeroiAir does not invert for AEM system parameters.')
 202 FORMAT(/T3,'PLT_INDX cannot exceed the number of plates.')
 203 FORMAT(/T3,'If PLT_INDX > 0, KPAR must be an integer in the range 1 to 9.')
 204 FORMAT(/T3,'The maximum number of iterations has been reduced to 20.')
 205 FORMAT(/T3,'CNVRG must be 1, 2, 3 or 4')
 206 FORMAT(/T3,'For inversion, aircraft positions must be entered for every station', &
            /T3,'The automatic course option, SURVEY = 1 is not allowed.', &
            /T3,'SURVEY must be either 2, 3, -2, or -3.'/)
 207 FORMAT(/T3,'KCMP must = 12 or 21 for Frequency-domain inversion.')
 208 FORMAT(/T3,'ORDER must = 1 or 2.')
 209 FORMAT(/T3,'KCMP is restricted to the values: 1, 3, 13, 31, 123, 312')
 210 FORMAT(/T3,'LYR_INDX cannot exceed the number of layers.')
 211 FORMAT(/T3,'If PLT_INDX < 0 (layers), KPAR must be 1 for resistivity or 2 for thickness.')
 212 FORMAT(/T3,'PLT_INDX must be > 0 for plates or < 0 for layers)')
 213 FORMAT(/T3,'It is wise to choose a manual derivative step between 3 & 10 percent.' &
            /T3,'Otherwise the sensitivity matrix may yield silly results.')
 214 FORMAT(/T3,'Order must be 1122 (for IIQQ),  1212 (for IQIQ),  2211 (for QQII),  2121 (for QIQI)')
 215 FORMAT(/T3,'There is a component discrepency between the cfl and inv files.')
 220 FORMAT(/T3,'X component inversion was requested but LeroiAir.inv contains only Z component data.')
 221 FORMAT(/T3,'Z component inversion was requested but LeroiAir.inv contains only X component data.')
 222 FORMAT(/T3,'X & Z component inversion was requested but LeroiAir.inv contains only X component data.')
 223 FORMAT(/T3,'X & Z component inversion was requested but LeroiAir.inv contains only Z component data.')
 224 FORMAT(/T3,'3 component inversion was requested but LeroiAir.inv contains only X component data.')
 225 FORMAT(/T3,'3 component inversion was requested but LeroiAir.inv contains only Z component data.')
 226 FORMAT(/T3,'3 component inversion was requested but LeroiAir.inv contains no Y component data.')
 501 FORMAT(/T2,'WARNING'/T2,'-------'/)
 502 FORMAT(/T2,'FATAL ERROR'/T2,'----- -----'/)

END SUBROUTINE  WRITE_LOG_FILE

 SUBROUTINE WRITE_FD (NW,NP,TITLE,NSTAT,LINE,TXCLN,TXA90,SXD,SYD,SZ,RXD,RYD,RZ,CONFIG,NFRQ, &
                      FREQ,PRFL,QUNIT,PPFAC,BFFAC,PRTSEC,PRM_FD,CMP,BFD_SCAT,BFD)
!--------------------------------------------------------------------------------------

!***  Called by: MAIN
!***      Calls: WRSLV_FD

!  Prints the results of TEM computations.
!
!                NW - output unit number
!               NP - unit number for mf1 file
!             NSTAT - Number of stations
!              LINE - Line number
!         TXCLN(JF) - transmitter orientation at frequency JF (in radians)
!             TXA90 - true for vertical co-planar briadside array
!            PRTSEC - true iff scattered fields are to be printed
!              PRFL = 1 => profile output
!                   = 0 => spectral output
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

 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 INTEGER NW,NP,NSTAT,LINE(NSTAT),NFRQ,CMP,PRFL,TXD(NFRQ),JF,JS
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

!  Finish writing LeroiAir.mf1

 IF (CMP > 1) RETURN
 DO JS = 1,NSTAT
   WL = .TRUE.
   IF (JS > 1 .AND. LINE(JS) == LINE(JS-1)) WL = .FALSE.

   IF (WL) THEN
     WRITE(QL0,'(I10)') LINE(JS)
     READ(QL0,'(A)') QL1
     WRITE(NP,'(2A)') 'Line ',TRIM (ADJUSTL (QL1))
   END IF

   WRITE(NP,20) JS,SYD(JS),SXD(JS),SZ(JS),RYD(JS,1),RXD(JS,1),RZ(JS,1), &
                 REAL (BFD1(1:NFRQ,JS,4)), AIMAG (BFD1(1:NFRQ,JS,4))
 END DO

 1 FORMAT(/T3,'FREQUENCY-DOMAIN LeroiAir OUTPUT' /T3,33('-'))
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
!  if PRFL = 1  or in spectral output if PRFL = 0

!         TXD(JF) - transmitter orientation at frequency JF (in degrees)
!      YTR(JF,JS) - field at station JS for frequency JF.

!    All other variables defined in SUBROUTINE WRITE_FD

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 INTEGER NW,NFRQ,PRFL,CMP,NSTAT,TXD(NFRQ),KRX,KRY,KRZ,JS
 REAL MPZ1(NSTAT),FREQ(NFRQ),YTR(NFRQ,NSTAT)
 REAL(KIND=QL) MXD(NSTAT),MYD(NSTAT)
 CHARACTER(LEN=3) CONFIG(NFRQ)

 IF (PRFL == 0) THEN
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
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18), NCOL=40
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

 SUBROUTINE WRITE_TD (NW,NP,TITLE,NSTAT,LINE,SXD,SYD,SZ,TXDEG,RXD,RYD,RZ,XRX,YRX,ZRX,NCHNL, &
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
!              PRFL = 1 for orofile output;  = 0 for decay output.
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

 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 REAL, PARAMETER ::  TOL=1.E-3
 INTEGER NW,NP,PRFL,NSTAT,LINE(NSTAT),NCHNL,MCHNL,CMP,KPPM,JC,JS
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

 NORM = 1.
 PRTX = .TRUE.
 PRTY = .TRUE.
 PRTZ = .TRUE.
 IF (CMP /= 3) PRTY = .FALSE.
 IF (CMP == 11) PRTZ = .FALSE.
 IF (CMP == 13) PRTX = .FALSE.

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

 IF (PRFL == 0) THEN
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

!  Finish writing LeroiAir.mf1

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
     WRITE(NP,'(2A)') 'Line ',TRIM (ADJUSTL (QL1))
   END IF
   WRITE(NP,20) JS,SYD(JS),SXD(JS),SZ(JS),TXDEG(JS),RYD(JS,1),RXD(JS,1),RZ(JS,1),QDATA(1:MCHNL)

 END DO

 1 FORMAT(//T3,'TIME-DOMAIN LeroiAir OUTPUT'/T3,28('-') &
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
! 20 FORMAT(I5,2F12.1,F8.1,F6.1,2F12.1,F8.1,8192G13.4)
 20 FORMAT(I10,2F12.1,F8.1,F6.1,2F12.1,F8.1,8192en16.6)
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
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18), NCOL=30
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
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18), NCOL=30
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
!  ROUTINES SPECIFIC FOR LEROIAIR
!  -------------------------------

 SUBROUTINE LEROI_3D (TDFD,NFRQ,FREQ,NLYR,THK,RES,RMU,REPS,CALF,CTAU,CFREQ,NPLT,MXAB,  &
                      MXB,NB,NA,DA,DB,XCELL,YCELL,ZCELL,PLTOP,PLWDTH,PLNGTH,XCNTR,YCNTR, &
                      PLAZM,PLDIP,SIG_T,CALFP,CTAUP,CFREQP,NSTAT,FANGLE,SX,SY,SZ,TXCLN,  &
                      TXA90,SAME_TX,NRX,NRXST,RX,RY,RZ,INVERT,BFD_SCAT,NW)
!-----------------------------------------------------------------------------------
!
!***  Called by: MAIN, GET_FWD_MODL
!***      Calls: COLRES, PRM_BOSS, SCAT_MAG, SCAT_MTRX-BOSS, SET_MGT, SET_NCELL2, SET_RHO,

! Main routine for LeroiAir Computation.
! Note that from 25.11.03 BTD_SCAT changes from: north & east components to
!                --------          aircraft in-line & transverse components
!
!             OUTPUT
!             ------
!
!   BFD_SCAT(JF,JS,JC) - scattered frequency-domain magnetic field in nT for
!                        frequency, JF, station, JS, component JC
!
!                from SCAT_MAG  JC = 1 => north;   = 2 => east;       = 3 => vertical
!                from LEROI_3D  JC = 1 => in-line; = 2 => transverse; = 3 => vertical
!
!             INPUT
!             -----
!
!      TDFD - 1=> time-domain;  2 => frequency-domain
!      FREQ - array of NFRQ frequencies
!      NLYR - number of layers (1 or 2)
!       THK - layer thicknesses
!       RES - array of layer resistivities
!       RMU - mu(i) / mu(0)
!      REPS - relative dielectric constant
!      CALF, CTAU, CFREQ are the layered earth Cole-Cole parameters.
!
!      NPLT - number of thin plates
!     PLAZM - strike angle - 0 => due north
!     PLDIP - 0 => horizontal,  90 => vertical
!     XCNTR, YCNTR, PLTOP: north, east & depth coordinates of plate reference point
!                           midpoint of top edge (left edge for flat-lying plate
!     PLNGTH, PLWDTH: strike length and length down dip
!     SIG_T, CALFP, CTAUP, CFREQP: conductance, C-C parameters for each plate

!        NB,NA - number of cells down dip and along strike for each plate.
!        DB,DA - down-dip and along-strike dimensions for these cells
!         MXAB - Number of cells in biggest plate
!          MXB - maximum number of cells down dip
!   XCELL(k,*) - north coordinate of centre of cell k
!   YCELL(k,*) - east coordinate of centre of cell k
!   ZCELL(i,*) - depth of cell centre in row i relative to surface.
!
!        NSTAT - number of transmitter positions.
!       FANGLE - flight path angle in radians. (North = 0; East = PI/2)
!   SX, SY, SZ - north, east and altitude (re gnd level) of transmitter
!       TXCLN  - transmitter inclination (> 0 => nose up)
!        TXA90 - true for vertical co-planar briadside array
!      SAME_TX - used to reduce redundant computations

!        NRXST = TXCLN dimension: = NSTAT for time domain; = NFRQ for frequency domain
!          NRX = receiver offset dimension: = 1 for time domain; = NFRQ for frequency domain
!  RX,RY,RZ(JS,JR) - north, east and vertical receiver positions for station JS, offset JR


 IMPLICIT NONE
 INTEGER, PARAMETER :: NCPTS=3     !  Number of points for MGT gaussian integration
 COMPLEX, PARAMETER :: ZERO=(0.,0.)
 INTEGER NW,TDFD,NFRQ,NLYR,NPLT,MXAB,MXB,MXRHO,NRPRM,NREGT,NRMGT,MXCL2,NSTAT,NRX, &
         IFIN,JF,JS,JQ,NA(NPLT),NB(NPLT),NCELL2(0:NPLT),MXGS,NRXST
 REAL FRQ,FREQ(NFRQ),THK(NLYR),MGTMX,XCELL(MXAB,NPLT),YCELL(MXAB,NPLT),ZCELL(MXB,NPLT), &
      XRM(NSTAT,NRX,NPLT),YRM(NSTAT,NRX,NPLT),TXCLN(NRXST),CSF,SNF
 REAL, DIMENSION(NLYR) :: RES,RMU,REPS,CALF,CTAU,CFREQ
 REAL, DIMENSION(NPLT) :: SIG_T,CALFP,CTAUP,CFREQP,PLTOP,PLWDTH,XCNTR,YCNTR, &
                          PLNGTH,PLAZM,PLDIP,DA,DB,WXZY
 REAL, DIMENSION(NSTAT) :: SX,SY,SZ,FANGLE
 REAL, DIMENSION(NSTAT,NRX) :: RX,RY,RZ
 REAL, DIMENSION(:), ALLOCATABLE :: B,RHOTRP
 REAL, DIMENSION(:,:), ALLOCATABLE :: XGS,YGS,ZGS
 LOGICAL SAME_TX(NSTAT),INVERT,TXA90
 COMPLEX SIGL(NLYR),KSQ_LYR(NLYR),SIGT(NPLT),KSQ_SHT(NPLT),E_PRYM(2,MXAB,NSTAT,NPLT), &
         J_SCAT(2,MXAB,NSTAT,NPLT),XLINE,YTRNS,BFD_SCAT(NFRQ,NSTAT,3)
 LOGICAL DCMP_FAIL
 Integer :: tvals(8)
 Real 	:: fprog

 INTENT (OUT) BFD_SCAT

! Set up horizontal distance descriptors and arrays for
! primary field, & electric & magnetic Green's functions.

! Set up Gausian integration for magnetic field computation

!**************************
 IF (NPLT < 1) RETURN
!**************************

 MXGS = NCPTS * MAX (MAXVAL(NA), MAXVAL(NB))
 ALLOCATE (XGS(MXGS,NPLT),YGS(MXGS,NPLT),ZGS(MXGS,NPLT))

 CALL SET_MGT (NSTAT,NRX,NPLT,NCPTS,NA,NB,DA,DB,PLTOP,PLWDTH,PLNGTH,PLDIP, &
               PLAZM,XCNTR,YCNTR,RX,RY,XRM,YRM,WXZY,MXGS,XGS,YGS,ZGS,MGTMX)

 CALL SET_NCELL2 (NPLT,NA,NB,NCELL2,MXCL2)

 ALLOCATE (B(1000) )
 CALL SET_RHO (MXAB,NPLT,NSTAT,MGTMX,NA,NB,DA,PLNGTH,XCELL,YCELL,SX,SY, &
               NRMGT,NRPRM,NREGT,MXRHO,B)
 ALLOCATE (RHOTRP(MXRHO))
 RHOTRP(1:MXRHO) = B(1:MXRHO)
 DEALLOCATE (B)

 BFD_SCAT = ZERO

 IF (.NOT. INVERT) WRITE(*,1) NFRQ
 FREQUENCY_LOOP: DO JF = 1,NFRQ

   Call Date_and_Time(Values = tvals)
   ifin = nint(100. * (JF-1) / REAL (NFRQ))
   fprog = 100. * (JF-1) / REAL (NFRQ)
   FRQ = FREQ(JF)
   IF (.NOT. INVERT) WRITE(*,2) tvals(1:3), tvals(5:7), JF, NFRQ, FRQ,fprog

! Set up complex conductivities.  Then compute primary fields, Green's tensors,
! scattering matrices and scattering currents for each plate individually.

   CALL COLRES (FRQ,NLYR,NPLT,RES,REPS,CALF,CTAU,CFREQ,SIG_T,CALFP,CTAUP,CFREQP, &
                SIGL,RMU,KSQ_LYR,SIGT,KSQ_SHT)

!  Compute the layered earth electric fields on the target, E_PRYM.

   E_PRYM = ZERO
   JQ =1
   IF (TDFD == 2) JQ = JF            ! Tx-Rx offset index

   CALL PRM_BOSS (TDFD,NRXST,TXCLN,TXA90,JF,FRQ,NSTAT,SAME_TX,SX,SY,SZ,FANGLE, &
                  NLYR,RMU,KSQ_LYR,THK,NPLT,PLAZM,PLDIP,MXB,MXAB,NB,NA,XCELL,  &
                  YCELL,ZCELL,NRPRM,RHOTRP,E_PRYM)

!  Set up scattering matrix SCAT_MTRX as an LU decomposition.


   CALL SCAT_MTRX_BOSS (NPLT,NLYR,MXAB,MXB,NCELL2,MXCL2,NA,NB,DA,DB,KSQ_SHT,SIGT, &
                        SIGL,RMU,KSQ_LYR,THK,PLTOP,PLDIP,PLAZM,XCELL,YCELL,ZCELL, &
                        NREGT,RHOTRP,NSTAT,E_PRYM,J_SCAT,DCMP_FAIL)
   IF (DCMP_FAIL) THEN
     WRITE(NW,3)
     WRITE(*,3)
     CALL WRITE_FAILED_MODEL (NW,NLYR,RES,THK,CALF,CTAU,CFREQ,RMU,REPS,NPLT,PLNGTH,PLWDTH, &
                              SIG_T,CALFP,CTAUP,CFREQP,XCNTR,YCNTR,PLTOP,PLAZM,PLDIP)
     STOP
   END IF

!  Compute BFD_SCAT, the scattered frequency-domain magnetic fields.

   CALL SCAT_MAG (JQ,JF,NFRQ,NPLT,MXAB,NA,NB,PLDIP,PLAZM,NLYR,RMU,KSQ_LYR,THK, &
                  NSTAT,SAME_TX,NRX,XRM,YRM,RZ,NRMGT,RHOTRP,NCPTS,MXGS,XGS,YGS,  &
                  ZGS,WXZY,J_SCAT,BFD_SCAT)

 END DO FREQUENCY_LOOP

! BFD_SCAT(*,*,JC) = the north, east, vertical components for JC = 1, 2, 3 respectively.
! Rotate fields by FANGLE into aircraft system so that
! BFD_SCAT(*,*,JC) = the in-line, transverse, vertical components for JC = 1, 2, 3 respectively.

 DO JS = 1,NSTAT
   CSF = COS (FANGLE(JS))
   SNF = SIN (FANGLE(JS))
   DO JF = 1,NFRQ
     XLINE = BFD_SCAT(JF,JS,1) * CSF + BFD_SCAT(JF,JS,2) * SNF
     YTRNS = BFD_SCAT(JF,JS,2) * CSF - BFD_SCAT(JF,JS,1) * SNF
     BFD_SCAT(JF,JS,1) = XLINE
     BFD_SCAT(JF,JS,2) = YTRNS
   END DO
 END DO

 DEALLOCATE (XGS,YGS,ZGS,RHOTRP)

 1 FORMAT(/T3,'A maximum of',i4,' 3D frequency-domain responses will be computed ....'/)
 2 FORMAT(T3, i4.4, '-', i2.2, '-', i2.2, 'T', i2.2, ':', i2.2, ':', i2.2, &
                   ': Frequency ',i4, ' of ', i4, '  =',1x, en10.2,' Hz; ', f5.2,' % complete')
 3 FORMAT (//T3,'An evil spirit has entered SCAT_MTRX_LU_DCMP causing the matrix to be singular.', &
            /T3,'The model leading to this crash is Described in LeroiAir.out.' &
            /T3,'COMPUTATION HALTED.  SEEK HELP.  (david.annetts@csiro.au)')
 END SUBROUTINE LEROI_3D

 SUBROUTINE COLRES (FRQ,NLYR,NPLT,RES,REPS,CALF,CTAU,CFREQ,SIG_T,CALFP,CTAUP, &
                    CFREQP,SIGL,RMU,KSQ_LYR,SIGT,KSQ_SHT)
!---------------------------------------------------------------------------

!  Computes SIGL, the complex conductivities of layers and SIGT, the complex
!  conductances of plates, at frequency FRQ using the layered earth Cole-Cole
!  parameters, CALF, CTAU, CFREQ, the plate Cole-Cole parameters: CALFP,
!  CTAUP, CFREQP,
!  the NLYR real layer resistivities, RES, and the real conductance, SIG_T,
!  SIGL and SIGT include displacement currents.

!  KSQ_LYR = iwu * SIGL = the layered earth propagation constants.
!  KSQ_SHT = iwu * SIGT = the propagation constants for plates.

!***  Called by: LEROI_3D

 IMPLICIT NONE
 REAL, PARAMETER :: TWOPI=6.2831853, MU0=12.56637E-7, EPS0=8.854156E-12
 COMPLEX, PARAMETER :: ONE=(1.,0.), CI=(0.,1.)
 INTEGER J,NLYR,NPLT
 REAL, DIMENSION(NLYR) :: RES,RMU,REPS,CALF,CTAU,CFREQ
 REAL OMEGA,MU,EPS,FRQ,SIG_T(NPLT),CALFP(NPLT),CTAUP(NPLT),CFREQP(NPLT)
 COMPLEX SIGT(NPLT),KSQ_SHT(NPLT),SIGL(NLYR),KSQ_LYR(NLYR),P

 INTENT (IN) FRQ,NLYR,NPLT,RMU,RES,REPS,CALF,CTAU,CFREQ,SIG_T,CALFP,CTAUP,CFREQP
 INTENT (OUT) SIGT,KSQ_SHT,SIGL,KSQ_LYR

 OMEGA = TWOPI * FRQ

 SIGL = CMPLX ((1. / RES), 0.)
 SIGT = CMPLX (SIG_T, 0.)

! Compute complex conductivity using Cole-Cole parameters if appropriate

 DO J = 1,NLYR                ! Layers
   P = (CI * OMEGA * CTAU(J) )**CFREQ(J)	! this line triggers a benign FPE most of the time: 0^1.0
   SIGL(J) = SIGL(J) * (ONE + P) / (ONE + CALF(J)*P)
   MU = MU0 * RMU(J)
   EPS = EPS0 * REPS(J)
   SIGL(J) = SIGL(J) + CI * OMEGA * EPS  !  Add in displacement term
   KSQ_LYR(J) = CI * OMEGA * MU * SIGL(J)
 END DO

 DO J = 1,NPLT                ! Plates
   P = (CI * OMEGA * CTAUP(J) )**CFREQP(J)
   SIGT(J) = SIGT(J) * (ONE + P) / (ONE + CALFP(J)*P)
 END DO
 KSQ_SHT = CI * OMEGA * MU0 * RMU(NLYR) * SIGT

END SUBROUTINE COLRES

 SUBROUTINE EGT_BOSS (NLYR,NAL,NBL,DAL,DBL,SIGL,RMU,KSQ_LYR,THK,PLTOPL,CDIP, &
                      SDIP,NREGT,RHOTRP,SAA,SBA,SBB,HAA,HAB,HBA,HBB)
! ----------------------------------------------------------------------------

!***  Called by: SCAT_MTRX_BOSS
!***      Calls: EGTDIR, EGT_CSPL, CUBINT, CUBSPL

!  Computes the integrals over the "receiver" cell of the adjoint Green's tensor
!  elements for single plates contained in a uniform half-space or entirely in
!  the bottom layer of a two layer half-space.  The direct portion is computed
!  separately from the reflected/transmitted scattered terms.

!  In what follows, the GTE have the units of electric field divided by a
!  factor (-iwu) because this factor is explicitly included as a multiplier
!  outside the integral over area when solving the integral equation.
!
!  NAL & NBL are the number of cells along strike and down dip respectively.
!  DAL & DBL are the corresponding lengths of the cells.
!  SIGL contains the complex layer conductivities.
!  KSQ_LYR = iwu * SIGL
!  THK are the layer thicknesses.
!  PLTOP is the distance to the top of the plate and (CDIP, SDIP) refer
!  to the dip.
!  A vertical body has these as (0,1) and a flat lying plate as (1,0)
!  HBBT * HABT are temporary storage variables.
!
!    OUTPUT:
!  The integrated Green's functions are initially computed in the form:
!
!  GAA = SAA + HAA /KSQ_BAS;  GAB = SAB + HAB /KSQ_BAS;
!                             GBA = SAB + HBA /KSQ_BAS  &
!  GBB = SBB + HBB /KSQ_BAS   where  KSQ_BAS :=  i * omega * mu / res(NLYR).
!
!  SAA, SAB=SBA, & SBB are the induced (divergence free) part of the GTE,
!  corresponding to currents enclosed within the plate.
!  HAA, HAB, HBA, & HBB are the curl-free or current channelling part
!  of the GTE which are closed outside the plate.
!
!  Normally one would expect the Green's tensor integral in the form:
!
!  ESi(r0) = SUM (over j) INT {Gij(r0, r) * Ej(r) } d3 r
!
!  where Gij(r0, r) is the ith component of the receiver field at ro due to jth
!  component of a source at r.  However, using reciprocity, one could just as
!  easily use the adjoint form as
!
!  ESi(r0) = SUM (over j) Ej(r) * INT {Gji(r, r0) * } d3 r
!
!  which in essence uses transpose of the Green's tensor integrated over the
!  receiver cell.  Since the derivatives of the original GTE were taken wrt r
!  and the integral is now over r, this allows one to use analytic forms
!  for part of the GTE integrals over the receiver cell.

!  In what follows, the GTE are stored in their transposed form to enable
!  consistency in SCAT_MTRX_BUILD with the interplate GTE which are used in
!  the original form.
!
!  Thus, SAB(KB,JB,JA) is the electric field in the ETA (down dip)) direction,
!  integrated over receiver cell (JB,JA) due to a XI-oriented (along-strike)
!  electric dipole at source cell (KB.1).  SAA is the XI electric field
!  at (JB,JA) due to a XI-oriented dipole.  SBB is the ETA electric field caused
!  by an ETA-oriented electric dipole.  Similarly for HAA, HAB, HBA, & HBB.
!
!  These are used by the subroutine, SCAT_MTRX_BUILD, which builds the global
!  matrix,  The solution vector is expressed as the curl and gradient of two
!  potential functions in the form CURL (*C* PSI) + KSQ_BAS * GRAD (PHI) where
!  *C* is a unit vector perpendicular to the plate surface and
!  KSQ_BAS = KSQ_LYR (basement)

!  Weidelt has shown that the surface integrals of HIJ * CURL (*C* PSI) are
!  identically zero which removes the 1 / KSQ_LYR term from the formulation.
!  Thus, to facilitate computation, fact, SUBROUTINE SCAT_MTRX_BUILD uses HIJ
!  only in the form HIJ + SIJ* KSQ_BAS, which is why this routine produces
!  HIJ(new) = HIJ(old) + SIJ * KSQ_BAS at the end.  Alternatively expressed,
!  SCAT_MTRX_BUILD is composed of (HIJ(old) + SIJ * KSQ_BAS) * (PHI terms)
!  added to SIJ * (PSI terms).
!
!  In what follows, XI is the coordinate along strike which is defined as a
!  clockwise rotation (East of North) from the X (North) axis).
!  ETA is the coordinate down dip.  ETA = X*CDIP + Z*SDIP.
!  The "source cell" has cell numbers (KB,1) in the (ETA,XI) direction.
!  The "receiver cell" has cell numbers (JB,JA) in the (ETA,XI) direction.
!  The GTE (Green's tensor elements) are integrated over the "receiver" cell
!  because the transposed form of the Green's tensor is used.
!
! -------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER, PARAMETER :: NGL=5
 REAL, PARAMETER :: TOL=1.0E-3
 COMPLEX, PARAMETER :: ZERO=(0.,0.), TWO=(2.,0.)
 INTEGER NAL,NBL,NREGT,NLYR,J1,KB,JB,JA,LB,NSPL,NXI,NRHO
 REAL, DIMENSION(4,NREGT) :: GR1,GR2,GR3,GR4,GR5,GI1,GI2,GI3,GI4,GI5
 REAL, DIMENSION(NGL) :: GLX,GLW,WGHT
 REAL ETA(MAX(NBL+1,NGL)),RHOTRP(NREGT),XI(NREGT),THK(NLYR),RKBASE,DAL,DBL,CDIP, &
      SDIP,PLTOPL,LENGTH,WIDTH,RHOMAX,FBB2,FBB3,ETAS,ZTR,YTR,XS,XF,CUBVAL, &
      CUBINT,HBBR,HBBI,HABR,HABI,ETAR,SAAR,SAAI,SBBR,SBBI,HAAR,HAAI,SBAR, &
      SBAI,HBAR,HBAI,YR2F,YI2F,YR2S,YI2S,YR3F,YI3F,YR3S,YI3S,YR4F,YI4F, &
      YR4S,YI4S,YR5F,YI5F,YR5S,YI5S,XF_RHO,RMU(NLYR),THICK
 COMPLEX, DIMENSION(NBL,NBL,NAL) :: SAA,SBA,SBB,HAA,HAB,HBA,HBB
 COMPLEX, DIMENSION(NBL+1,NAL) :: HBBT,HABT
 COMPLEX AAS,AAH,ABH,BBH,SIGL(NLYR),KSQ_LYR(NLYR),KBASE,TMP1,TMP2
 LOGICAL ZCMP,XCMP

 INTENT (IN) NLYR,NAL,NBL,DAL,DBL,SIGL,RMU,KSQ_LYR,THK,PLTOPL,CDIP,SDIP
 INTENT (OUT) SAA,SBA,SBB,HAA,HAB,HBA,HBB

 DATA GLX / -.9061798, -.5384693,    0.,    .5384693, .9061798/
 DATA GLW /  .2369269,  .4786287, .5688888, .4786287, .2369269/

! Set up propagation constants, integration weights, and interpolation range.

 ZCMP = .TRUE.
 XCMP = .TRUE.
 IF (ABS (CDIP) < TOL) XCMP = .FALSE.
 IF (ABS (SDIP) < TOL) ZCMP = .FALSE.

 THICK = SUM (THK(1:NLYR-1))
 LENGTH = NAL * DAL
 WIDTH = NBL * DBL
 RHOMAX = SQRT (LENGTH**2 + WIDTH**2 )
 KBASE = CSQRT (KSQ_LYR(NLYR))
 RKBASE = REAL (KBASE)
 IF (RKBASE < 0) KBASE = -KBASE

! Set interpolation range in XI direction and weights for Gauss-Legendre
! integration in ETA direction.  NDXI is the XI index of the right-hand
! boundary of the indexed cell.  Use density of 1/5 modified skin depth.

 WGHT = GLW * DBL /2.  ! Set weight array

 NXI = 5
 DO J1 = 5, NREGT
   NRHO = J1
   IF (RHOTRP(J1) < LENGTH) NXI = J1 + 1
   IF (RHOTRP(J1) > RHOMAX) EXIT
 END DO
 XI(1:NXI) = RHOTRP(1:NXI)

! Initialise the GTE by calling in the direct component.
! HBA picks up the direct component from HAB further down.

 DO KB = 1, NBL       ! Loop over source cell rows
   DO JB = KB, NBL    ! Loop over receiver cell rows
     DO JA = 1,NAL    ! Loop over receiver cell columns

       CALL EGTDIR (KB,JB,JA,KBASE,DAL,DBL,AAS,AAH,ABH,BBH)
       SAA(KB,JB,JA) = AAS
       SAA(JB,KB,JA) = AAS
       SBB(KB,JB,JA) = AAS
       SBB(JB,KB,JA) = AAS
       SBA(KB,JB,JA) = ZERO
       SBA(JB,KB,JA) = ZERO
       HAA(KB,JB,JA) = AAH
       HAA(JB,KB,JA) = AAH
       HAB(KB,JB,JA) = ABH
       HAB(JB,KB,JA) = -ABH
       HBA(KB,JB,JA) = ZERO
       HBA(JB,KB,JA) = ZERO
       HBB(KB,JB,JA) = BBH
       HBB(JB,KB,JA) = BBH
     END DO
   END DO
 END DO

! Set up interpolants for five integrals as a function of XI (strike).
! Use Gauss-Legendre integration along ETA (dip).

 FBB2 = CDIP**2
 FBB3 = 1. - 2.* FBB2
 KB_DOWN_DIP: DO KB = 1, NBL  ! source cell row loop
   ETAS = (KB - 0.5) * DBL
   NSPL = 3
   HABT = ZERO
   HBBT = ZERO
   LB_DOWN_HORIZ_CELL_BND: DO LB = 1, NBL+1

     ETA(LB) = (LB-1) * DBL
     ZTR = (ETA(LB) + ETAS) *SDIP + 2.* (PLTOPL - THICK)
     YTR = (ETA(LB) - ETAS) *CDIP

     GR1 = 0; GR2 = 0; GR3 = 0; GR4 = 0; GR5 = 0
     GI1 = 0; GI2 = 0; GI3 = 0; GI4 = 0; GI5 = 0

     CALL EGT_CSPL (NREGT,NRHO,RHOTRP,YTR,NXI,XI,NLYR,SIGL,RMU,KSQ_LYR,THK,ZTR, &
                    GR1,GR2,GR3,GR4,GR5,GI1,GI2,GI3,GI4,GI5,NSPL)

! JA = 1.  Use symmetry wrt XI.

     XS = 0.
     XF = DAL /2.
     TMP1 = ZERO
     TMP2 = ZERO
     IF (XCMP) THEN
       HBBR = CUBINT (XI,GR4,NXI,XS,XF)
       HBBI = CUBINT (XI,GI4,NXI,XS,XF)
       TMP1 = -CDIP * YTR * CMPLX (HBBR, HBBI)
     END IF
     IF (ZCMP) THEN
       HBBR = CUBINT (XI,GR5,NXI,XS,XF)
       HBBI = CUBINT (XI,GI5,NXI,XS,XF)
       TMP2 = SDIP * CMPLX (HBBR, HBBI)
     END IF
     HBBT(LB,1) = TWO* (TMP1 + TMP2)   ! END OF JA = 1

     XF_RHO = SQRT (XF**2 + YTR**2)
     YR3F = CUBVAL(RHOTRP,GR3,NRHO,XF_RHO)
     YI3F = CUBVAL(RHOTRP,GI3,NRHO,XF_RHO)
     DO JA = 2,NAL
       XS = XF
       XF = XS + DAL
       XF_RHO = SQRT (XF**2 + YTR**2)

       YR3S = YR3F
       YI3S = YI3F
       YR3F = CUBVAL(RHOTRP,GR3,NRHO,XF_RHO)
       YI3F = CUBVAL(RHOTRP,GI3,NRHO,XF_RHO)

       HABR = YR3F - YR3S
       HABI = YI3F - YI3S
       HABT(LB,JA) = CMPLX (HABR, HABI)
       TMP1 = ZERO
       TMP2 = ZERO
       IF (XCMP) THEN
         HBBR = CUBINT (XI,GR4,NXI,XS,XF)
         HBBI = CUBINT (XI,GI4,NXI,XS,XF)
         TMP1 = -CDIP * YTR * CMPLX (HBBR, HBBI)
       END IF
       IF (ZCMP) THEN
         HBBR = CUBINT (XI,GR5,NXI,XS,XF)
         HBBI = CUBINT (XI,GI5,NXI,XS,XF)
         TMP2 = SDIP * CMPLX (HBBR, HBBI)
       END IF
       HBBT(LB,JA) = TMP1 + TMP2
     END DO
   END DO LB_DOWN_HORIZ_CELL_BND

   NSPL = 1
   JB_NBL_LOOP: DO JB = 1, NBL
     ETAR = (JB - 0.5) * DBL
     J1_NGL_LOOP: DO J1 = 1, NGL
       GR1 = 0; GR2 = 0; GR3 = 0; GR4 = 0; GR5 = 0
       GI1 = 0; GI2 = 0; GI3 = 0; GI4 = 0; GI5 = 0
       ETA(J1) = GLX(J1)*DBL/2. + ETAR
       ZTR = (ETA(J1) + ETAS) *SDIP + 2.* (PLTOPL - THICK)
       YTR = (ETA(J1) - ETAS) *CDIP

       CALL EGT_CSPL (NREGT,NRHO,RHOTRP,YTR,NXI,XI,NLYR,SIGL,RMU,KSQ_LYR,THK, &
                      ZTR,GR1,GR2,GR3,GR4,GR5,GI1,GI2,GI3,GI4,GI5,NSPL)

! JA = 1.  Use symmetry wrt XI.

       XS = 0.
       XF = DAL / 2.
       XF_RHO = SQRT (XF**2 + YTR**2)
       YR2F = CUBVAL(RHOTRP,GR2,NRHO,XF_RHO)
       YI2F = CUBVAL(RHOTRP,GI2,NRHO,XF_RHO)
       SAAR = 2.* (CUBINT (XI,GR1,NXI,XS,XF) - YR2F * XF)
       SAAI = 2.* (CUBINT (XI,GI1,NXI,XS,XF) - YI2F * XF)
       SAA(KB,JB,1) = SAA(KB,JB,1) + WGHT(J1)* CMPLX (SAAR,SAAI)
       SBBR = 2.* (FBB3* CUBINT (XI,GR3,NXI,XS,XF) + FBB2* YR2F * XF)
       SBBI = 2.* (FBB3* CUBINT (XI,GI3,NXI,XS,XF) + FBB2* YI2F * XF)
       SBB(KB,JB,1) = SBB(KB,JB,1) + WGHT(J1)* CMPLX (SBBR,SBBI)

       YR2F = CUBVAL(RHOTRP,GR2,NRHO,XF_RHO)
       YI2F = CUBVAL(RHOTRP,GI2,NRHO,XF_RHO)
       YR4F = CUBVAL(RHOTRP,GR4,NRHO,XF_RHO)
       YI4F = CUBVAL(RHOTRP,GI4,NRHO,XF_RHO)
       YR5F = CUBVAL(RHOTRP,GR5,NRHO,XF_RHO)
       YI5F = CUBVAL(RHOTRP,GI5,NRHO,XF_RHO)

       HAAR = 2.* YR4F * XF
       HAAI = 2.* YI4F * XF
       HAA(KB,JB,1) = HAA(KB,JB,1) - WGHT(J1)* CMPLX (HAAR,HAAI)  ! END OF JA = 1

       JA_NAL_LOOP: DO JA = 2, NAL
         XS = XF
         XF = XS + DAL
         XF_RHO = SQRT (XF**2 + YTR**2)

         YR2S = YR2F
         YI2S = YI2F
         YR4S = YR4F
         YI4S = YI4F
         YR5S = YR5F
         YI5S = YI5F

         YR2F = CUBVAL(RHOTRP,GR2,NRHO,XF_RHO)
         YI2F = CUBVAL(RHOTRP,GI2,NRHO,XF_RHO)
         YR4F = CUBVAL(RHOTRP,GR4,NRHO,XF_RHO)
         YI4F = CUBVAL(RHOTRP,GI4,NRHO,XF_RHO)
         YR5F = CUBVAL(RHOTRP,GR5,NRHO,XF_RHO)
         YI5F = CUBVAL(RHOTRP,GI5,NRHO,XF_RHO)

         SAAR = CUBINT (XI,GR1,NXI,XS,XF) - YR2F * XF + YR2S * XS
         SAAI = CUBINT (XI,GI1,NXI,XS,XF) - YI2F * XF + YI2S * XS
         SAA(KB,JB,JA) = SAA(KB,JB,JA) + WGHT(J1) * CMPLX (SAAR,SAAI)
         SBBR = FBB3* CUBINT (XI,GR3,NXI,XS,XF) + FBB2* (YR2F * XF &
              -  YR2S * XS)
         SBBI = FBB3* CUBINT (XI,GI3,NXI,XS,XF) + FBB2* (YI2F * XF &
              -  YI2S * XS)
         SBB(KB,JB,JA) = SBB(KB,JB,JA) + WGHT(J1) * CMPLX (SBBR,SBBI)
         IF (XCMP) THEN
           SBAR = YR2F - YR2S
           SBAI = YI2F - YI2S
           SBA(KB,JB,JA) = SBA(KB,JB,JA) - CDIP*YTR* WGHT(J1)* CMPLX(SBAR,SBAI)
         END IF

         HAAR = YR4F * XF  -  YR4S * XS
         HAAI = YI4F * XF  -  YI4S * XS
         HAA(KB,JB,JA) = HAA(KB,JB,JA) - WGHT(J1) * CMPLX (HAAR,HAAI)
         IF (ZCMP) THEN
           HBAR = YR5F - YR5S
           HBAI = YI5F - YI5S
           HBA(KB,JB,JA) = HBA(KB,JB,JA) + 2.*SDIP * WGHT(J1) * CMPLX (HBAR,HBAI)
         END IF
       END DO JA_NAL_LOOP
     END DO J1_NGL_LOOP

     DO JA = 1, NAL
       HBB(KB,JB,JA) = HBB(KB,JB,JA) + HBBT(JB+1,JA) - HBBT(JB,JA)
       HAB(KB,JB,JA) = HAB(KB,JB,JA) + HABT(JB+1,JA) - HABT(JB,JA)
       HBA(KB,JB,JA) = HBA(KB,JB,JA) + HAB(KB,JB,JA)
     END DO

   END DO JB_NBL_LOOP
 END DO KB_DOWN_DIP

! SET UP S-H SUM AND STORE IT IN H.

 HAA = HAA + SAA * KSQ_LYR(NLYR)
 HAB = HAB + SBA * KSQ_LYR(NLYR)
 HBA = HBA + SBA * KSQ_LYR(NLYR)
 HBB = HBB + SBB * KSQ_LYR(NLYR)

END SUBROUTINE EGT_BOSS

 SUBROUTINE EGTDIR (KB,JB,JA,KBASE,DAL,DBL,AAS,AAH,ABH,BBH)
!-------------------------------------------------------

!***  Called by: EGT_BOSS

!  Computes the integral of the direct (or full space) Green's
!  tensor elements over the receiver cell (JB,JA) for a tangential
!  unit source dipole in cell (KB,1).  In the code below, "a" refers
!  to the strike direction (X axis) and "b" refers to the width
!  direction ( Y cos(beta) + Z sin(beta) ).  However, for the internals
!  of the whole space GF computation, "b" may be taken in the Z
!  direction with no loss of generality.
!
!  (A,B) is the distance from the "source dipole" to the centre of the
!  receiver cell.
!
!  C2 :=  KBASE**2 = i * omega * mu / cres(basement)
!  DAL and DBL are the cell dimensions in the a and b directions respectively.
!
!  The integrated Green's functions are expressed as:
! GAA = AAS + AAH/C2;   GAB = ABH / C2;   GBA = GAB;   GBB = AAS + BBH/C2
!
!     Defining Q  as EXP (-KBASE * R) / (4*PI * R), then
! AAS = INT {Q} da db       ABH = -Q
! AAH = -INT {dQ/da} db     BBH = -INT {dQ/db} da

 IMPLICIT NONE
 COMPLEX, PARAMETER :: ZERO=(0.,0.), ONE=(1.,0.), FRPIC=(12.56637, 0.)
 INTEGER, PARAMETER :: NGL=5
 INTEGER JB,KB,JA,J1,J2
 REAL A,B,DAL,DBL,DAH,DBH,XL,XR,ZT,ZB,ASR,RR
 REAL, DIMENSION (NGL) :: GLX,WGL,X,Z,WGX,WGZ
 COMPLEX KBASE,QB,AAS,AAH,BBH,ABH
 LOGICAL PARTS

 INTENT (IN) KB,JB,JA,KBASE,DAL,DBL
 INTENT (OUT) AAS,AAH,ABH,BBH

 DATA GLX / -.9061798, -.5384693, 0., .5384693, .9061798/
 DATA WGL / .2369269, .4786287, .5688888, .4786287, .2369269/

! Set inter-cell distance and Gauss-Legendre integration points and
! weights.  For the sake of exposition, regarding B as in the Z direction,
! B is positive (negative) if 'source" is above (below) receiver cell.
! XL and XR will refer to the left and right cell boundaries
! and ZT and ZB to the top and bottom boundaries respectively.

 A = DAL* (JA-1)
 B = DBL* (JB-KB)
 DAH = DAL / 2.
 DBH = DBL / 2.
 XL = A - DAH
 XR = A + DAH
 ZT = B - DBH
 ZB = B + DBH

 X = A + GLX * DAH
 Z = B + GLX * DBH
 WGX = WGL * DAH
 WGZ = WGL * DBH

! Zero out functions.  Evaluate ABH analytically.  If same cell or nearest
! neighbours, integrate AAH and BBH by parts and subtract 1/R part from
! AAS.  Then integrate remainder.  Otherwise, integrate full functions.
! Z is positive downwards so integrals go from top to bottom
! and from left to right of receiver cell.

 AAS = ZERO; AAH = ZERO; BBH = ZERO
 IF ((JA == 1) .OR. (JB == KB)) THEN
   ABH = ZERO
 ELSE
   ABH = Q (KBASE,XL,ZB) + Q (KBASE,XR,ZT) - Q (KBASE,XR,ZB) - Q (KBASE,XL,ZT)
 END IF

 PARTS = .FALSE.
 IF ( (ABS (JB-KB) < 2) .AND. (JA < 3)) PARTS = .TRUE.
 IF (PARTS) THEN
   AAH = AAHP1 (KBASE,XL,ZT) + AAHP1 (KBASE,XR,ZB) &
      - AAHP1 (KBASE,XR,ZT) - AAHP1 (KBASE,XL,ZB )
   BBH = AAHP1 (KBASE,ZT,XL) + AAHP1 (KBASE,ZB,XR) &
      - AAHP1 (KBASE,ZB,XL) - AAHP1 (KBASE,ZT,XR )
   ASR = RINT (XL,ZT) + RINT (XR,ZB) - RINT (XR,ZT) - RINT (XL,ZB)
   AAS = CMPLX (ASR, 0.)
   DO J1 = 1, NGL
     AAH = AAH + WGZ(J1) * ( AAHP2 (KBASE,XR,Z(J1)) - &
                            AAHP2 (KBASE,XL,Z(J1)) )
     BBH = BBH + WGX(J1) * ( AAHP2 (KBASE,ZB,X(J1)) - &
                            AAHP2 (KBASE,ZT,X(J1)) )
     DO J2 = 1, NGL
       RR = R (X(J1),Z(J2) )
       QB = - KBASE
       IF (RR > 1.0E-10) QB = (EXP (-KBASE*RR) -ONE) /RR
       AAS = AAS + QB * WGX(J1) * WGZ(J2)
     END DO
   END DO

 ELSE

   DO J1 = 1, NGL
     AAH = AAH + WGZ(J1) * ( DQDX (KBASE,XR,Z(J1)) - DQDX (KBASE,XL,Z(J1)) )
     BBH = BBH + WGX(J1) * ( DQDX (KBASE,ZB,X(J1)) - DQDX (KBASE,ZT,X(J1)) )
     DO J2 = 1, NGL
       AAS = AAS + WGX(J1) * WGZ(J2) * Q (KBASE,X(J1),Z(J2))
     END DO
   END DO
 END IF

 AAS = AAS/ FRPIC;  AAH = AAH/ FRPIC;  ABH = ABH/ FRPIC;  BBH = BBH/ FRPIC

! Define in-line whole space functions.  RINT is the integral of 1/R.

 CONTAINS

   REAL FUNCTION R(X1,Z1)
!  ----------------------

   REAL X1,Z1
   R = SQRT (X1*X1 + Z1*Z1)
   END FUNCTION R

   REAL FUNCTION RINT (X1,Z1)
!  --------------------------

   REAL X1,Z1,RQ
   RQ = SQRT (X1*X1 + Z1*Z1)
   RINT = Z1* LOG (RQ + X1) + X1*LOG (RQ + Z1)
   END FUNCTION RINT

   COMPLEX FUNCTION AAHP1 (KBASE,X1,Z1)
!  ------------------------------------

   REAL X1,Z1,RQ
   COMPLEX KBASE

   RQ = SQRT (X1*X1 + Z1*Z1)
   AAHP1 = Z1 * EXP (-KBASE*RQ) / (X1*RQ)

   END FUNCTION AAHP1

   COMPLEX FUNCTION AAHP2 (KBASE,X1,Z1)
!  ------------------------------------

   REAL X1,Z1,RQ
   COMPLEX KBASE

   RQ = SQRT (X1*X1 + Z1*Z1)
   AAHP2 = KBASE * EXP (-KBASE*RQ) / X1

   END FUNCTION AAHP2

   COMPLEX FUNCTION DQDX (KBASE,X1,Z1)
!  -----------------------------------

   REAL X1,Z1,RQ
   COMPLEX KBASE

   RQ = SQRT (X1*X1 + Z1*Z1)
   DQDX  = (ONE + KBASE*RQ) * EXP (-KBASE*RQ)  * X1 /RQ**3

   END FUNCTION DQDX

   COMPLEX FUNCTION Q (KBASE,X1,Z1)
!  --------------------------------

   REAL X1,Z1,RQ
   COMPLEX KBASE

   RQ = SQRT (X1*X1 + Z1*Z1)
   Q = EXP (-KBASE*RQ) / RQ

   END FUNCTION Q

END SUBROUTINE EGTDIR

 SUBROUTINE EGT_CSPL (NREGT,NRHO,RHOTRP,YTR,NXI,XI,NLYR,SIGL,RMU,KSQ_LYR,THK, &
                      ZTR,GR1,GR2,GR3,GR4,GR5,GI1,GI2,GI3,GI4,GI5,NSPL)
!------------------------------------------------------------------------------

!***  Called by: EGT_BOSS
!***      Calls: EGTHNK, CUBSPL, CUBVAL

! Sets up cubic spline representations, GRj & GIj (j = NSPL,5), for the five
! Hankel integrals needed to compute the electric Green's tensor elements
! as a function of RHOTRP.

!   RHOTRP - interpolation array of rho values
!     NRHO - number of interpolation points needed for EGT
!     NLYR - number of layers
!     SIGL - complex conductivity of layers
!  KSQ_LYR - propagation constants
!      THK = layer thicknesses
!      ZTR = the reflected distance between "source" & "receiver"
!            points off top of basement.

 IMPLICIT NONE
 INTEGER JR,NREGT,NRHO,NLYR,NSPL,NXI
 REAL, DIMENSION (4,NREGT) :: GR1,GR2,GR3,GR4,GR5,GI1,GI2,GI3,GI4,GI5
 REAL RHOTRP(NRHO),XI(NXI),ZTR,YTR,THK(NLYR),RHO,TMP1(NXI),TMP2(NXI),TMP3(NXI), &
      TMP4(NXI),CUBVAL,RMU(NLYR)
 COMPLEX KSQ_LYR(NLYR),SIGL(NLYR),EHRI(NRHO,5)

 INTENT (IN) NRHO,RHOTRP,NXI,XI,ZTR,YTR,NLYR,SIGL,RMU,KSQ_LYR,THK,NSPL
 INTENT (OUT) GR1,GR2,GR3,GR4,GR5,GI1,GI2,GI3,GI4,GI5

 GR1 = 0; GR2 = 0; GR3 = 0; GR4 = 0; GR5 = 0
 GI1 = 0; GI2 = 0; GI3 = 0; GI4 = 0; GI5 = 0

 CALL EGTHNK (NRHO,RHOTRP,NLYR,SIGL,RMU,KSQ_LYR,THK,ZTR,EHRI,NSPL)

 IF (NSPL == 1) THEN
   DO JR = 1, NRHO
     GR1(1,JR) =  REAL (EHRI(JR,1))
     GR2(1,JR) =  REAL (EHRI(JR,2))
     GI1(1,JR) = AIMAG (EHRI(JR,1))
     GI2(1,JR) = AIMAG (EHRI(JR,2))
   END DO

   CALL CUBSPL (RHOTRP,GR1,NRHO,0,0)
   CALL CUBSPL (RHOTRP,GI1,NRHO,0,0)
   CALL CUBSPL (RHOTRP,GR2,NRHO,0,0)
   CALL CUBSPL (RHOTRP,GI2,NRHO,0,0)
 END IF

 DO JR = 1, NRHO
   GR3(1,JR) =  REAL (EHRI(JR,3))
   GR4(1,JR) =  REAL (EHRI(JR,4))
   GR5(1,JR) =  REAL (EHRI(JR,5))
   GI3(1,JR) = AIMAG (EHRI(JR,3))
   GI4(1,JR) = AIMAG (EHRI(JR,4))
   GI5(1,JR) = AIMAG (EHRI(JR,5))
 END DO

 CALL CUBSPL (RHOTRP,GR3,NRHO,0,0)
 CALL CUBSPL (RHOTRP,GI3,NRHO,0,0)
 CALL CUBSPL (RHOTRP,GR4,NRHO,0,0)
 CALL CUBSPL (RHOTRP,GI4,NRHO,0,0)
 CALL CUBSPL (RHOTRP,GR5,NRHO,0,0)
 CALL CUBSPL (RHOTRP,GI5,NRHO,0,0)

 IF (NSPL == 3) THEN   !  G*4 and G*5 are accessed from CUBINT so they
                       !  need to be re-splined in terms of XI
   DO JR = 1,NXI
     RHO = SQRT (XI(JR)**2 + YTR**2)
     TMP1(JR) = CUBVAL(RHOTRP,GR4,NRHO,RHO)
     TMP2(JR) = CUBVAL(RHOTRP,GI4,NRHO,RHO)
     TMP3(JR) = CUBVAL(RHOTRP,GR5,NRHO,RHO)
     TMP4(JR) = CUBVAL(RHOTRP,GI5,NRHO,RHO)
   END DO
   GR4 = 0.; GR5 = 0.; GI4 = 0.; GI5 = 0.
   GR4(1,1:NXI) = TMP1(1:NXI)
   GI4(1,1:NXI) = TMP2(1:NXI)
   GR5(1,1:NXI) = TMP3(1:NXI)
   GI5(1,1:NXI) = TMP4(1:NXI)

   CALL CUBSPL (XI,GR4,NXI,0,0)
   CALL CUBSPL (XI,GI4,NXI,0,0)
   CALL CUBSPL (XI,GR5,NXI,0,0)
   CALL CUBSPL (XI,GI5,NXI,0,0)
 END IF

 IF (NSPL == 1) THEN   !  G*1 and G*3 are accessed from CUBINT so they
                       !  need to be re-splined in terms of XI
   DO JR = 1,NXI
     RHO = SQRT (XI(JR)**2 + YTR**2)
     TMP1(JR) = CUBVAL(RHOTRP,GR1,NRHO,RHO)
     TMP2(JR) = CUBVAL(RHOTRP,GI1,NRHO,RHO)
     TMP3(JR) = CUBVAL(RHOTRP,GR3,NRHO,RHO)
     TMP4(JR) = CUBVAL(RHOTRP,GI3,NRHO,RHO)
   END DO
   GR1 = 0.; GR3 = 0.; GI1 = 0.; GI3 = 0.
   GR1(1,1:NXI) = TMP1(1:NXI)
   GI1(1,1:NXI) = TMP2(1:NXI)
   GR3(1,1:NXI) = TMP3(1:NXI)
   GI3(1,1:NXI) = TMP4(1:NXI)

   CALL CUBSPL (XI,GR1,NXI,0,0)
   CALL CUBSPL (XI,GI1,NXI,0,0)
   CALL CUBSPL (XI,GR3,NXI,0,0)
   CALL CUBSPL (XI,GI3,NXI,0,0)
 END IF

END SUBROUTINE EGT_CSPL

 SUBROUTINE EGTHNK (NRHO,RHOTRP,NLYR,SIGL,RMU,KSQ_LYR,THK,ZTR,EHRI,NSPL)
!-------------------------------------------------------------------------

!***  Called by: EGT_CSPL
!***      Calls: EGTVAL

! Sets up the five integrals EHRI (JR,J1), J1 = 1,5, needed to compute the
! electric Green's tensor elements in EGT_BOSS.  It uses the flow through
! Hankel transform method to compute values at RHOTRP(JR), (15 point per decade
! spacing).  It uses a 15 point per decade filter coefficient set derived from
! Christensen's program, FLTGEN.

!   RHOTRP - interpolation array of rho values
!     NRHO - number of interpolation points needed for EGT
!     NLYR - number of layers
!     SIGL - complex conductivity of layers
!  KSQ_LYR - propagation constants
!      THK = layer thicknesses
!      ZTR = the reflected distance between "source" & "receiver"
!            points off top of basement.
!     EHRI - inverse Hankel transform (J = 1,5)
!     NSPL - compute EHRI(J) unless J < NSPL

 USE LA_Filter_coefficients

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 REAL, PARAMETER :: FOURPI=12.56637
 INTEGER NLYR,NSPL,NRHO,L,JR,LMAX,LMIN,K,KMAX,KMIN,KBOT,K1
 REAL LMBDA,RHOTRP(NRHO),DIST,ZTR,THK(NLYR),RMU(NLYR)
 REAL(KIND=QL) DELTA,Y1,Y,RD
 COMPLEX KSQ_LYR(NLYR),SIGL(NLYR),KER(JNLO-NRHO:JNHI,5),EHRI(NRHO,5)
 LOGICAL JUMP

 INTENT(IN) NRHO,RHOTRP,NLYR,SIGL,RMU,KSQ_LYR,THK,ZTR,NSPL
 INTENT(OUT) EHRI

 EHRI = (0.,0.)
 KER = (0.,0.)
 DELTA = LOG (10.D0)/ DBLE (NDEC_JN)

! Set up KER for JR = 1 corresponding to minimum value of RHO.  This will
! compute most of the needed kernel range from the high end.  Note that
! the filter is defined between JNLO < L < JNHI

 JR = 1
 RD = DBLE (RHOTRP(1) )
 Y1 = -LOG (RD) - DBLE (JR-1) * DELTA - DBLE (SHFTJN)

 DO L = -50, JNHI             ! Start at L = -50 to pick up low values.
   LMAX = L                   ! Maximum filter index used
   K = L + 1 - JR             ! K is the kernel index.
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (REAL(Y))
   CALL EGTVAL (NRHO,K,JR,L,LMBDA,NSPL,NLYR,SIGL,RMU,KSQ_LYR,THK,ZTR,KER,EHRI,JUMP)
   IF (JUMP) EXIT
 END DO

 Y = Y1                       ! Finish off the low end for RHOTRP(1)
 DO L = -51, JNLO, -1
   LMIN = L                   ! Minimum filter index used
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (REAL(Y))
   K = L + 1 - JR             ! Compute the kernel index.
   CALL EGTVAL (NRHO,K,JR,L,LMBDA,NSPL,NLYR,SIGL,RMU,KSQ_LYR,THK,ZTR,KER,EHRI,JUMP)
   IF (JUMP) EXIT
 END DO

 KMIN = LMIN + 1 - JR  !  Define the range of kernel values
 KMAX = LMAX + 1 - JR  !  used for RHOTRP(1)

!Complete definition of kernel values by evaluating transform of
!maximum RHO = RHOTRP (NRHO)

 JR = NRHO
 Y1 = -LOG (RD) - DBLE (JR-1) * DELTA - DBLE (SHFTJN)
 KBOT = JNLO + 1 - JR
 K1 = MAX (KBOT,KMIN)

 DO K = K1, KMAX          ! Compute EHR for maximum RHO using previously
   L = K - 1 + JR         ! computed kernel values.
   IF (L > JNHI .OR. L < JNLO) CYCLE
   EHRI(JR,1) = EHRI(JR,1) + KER(K,1) * WJ0(L)
   EHRI(JR,2) = EHRI(JR,2) + KER(K,2) * WJ1(L)
   EHRI(JR,3) = EHRI(JR,3) + KER(K,3) * WJ0(L)
   EHRI(JR,4) = EHRI(JR,4) + KER(K,4) * WJ1(L)
   EHRI(JR,5) = EHRI(JR,5) + KER(K,5) * WJ0(L)
 END DO

 IF (K1 > KBOT) THEN    !  Add low end kernel values until convergence.
   DO K = K1-1, KBOT, -1
     KMIN = K
     L = K - 1 + JR
     Y = Y1 + DBLE (L) * DELTA
     LMBDA = EXP (REAL(Y))
     CALL EGTVAL (NRHO,K,JR,L,LMBDA,NSPL,NLYR,SIGL,RMU,KSQ_LYR,THK,ZTR,KER,EHRI,JUMP)
     IF (JUMP) EXIT
   END DO
 END IF

 DO JR = 2, NRHO-1          !  Compute transforms for all other RHO values
   DO K = KMIN, KMAX         !  using previously computed kernel values.
     L = K - 1 + JR
     IF (L > JNHI .OR. L < JNLO) CYCLE
     EHRI(JR,1) = EHRI(JR,1) + KER(K,1) * WJ0(L)
     EHRI(JR,2) = EHRI(JR,2) + KER(K,2) * WJ1(L)
     EHRI(JR,3) = EHRI(JR,3) + KER(K,3) * WJ0(L)
     EHRI(JR,4) = EHRI(JR,4) + KER(K,4) * WJ1(L)
     EHRI(JR,5) = EHRI(JR,5) + KER(K,5) * WJ0(L)
   END DO
 END DO

 DO JR = 1,NRHO
   EHRI(JR,2) = EHRI(JR,2) / RHOTRP(JR)
   EHRI(JR,4) = EHRI(JR,4) / RHOTRP(JR)
   DIST = FOURPI * RHOTRP(JR)
   EHRI(JR,1:5) = EHRI(JR,1:5) / DIST
 END DO

END SUBROUTINE EGTHNK

 SUBROUTINE EGTVAL (NRHO,K,JR,L,LMBDA,NSPL,NLYR,SIGL,RMU,KSQ_LYR,THK,ZTR, &
                    KER,EHRI,JUMP)
!--------------------------------------------------------------------------

!***  Called by: EGTHNK

! Accumulates the integrals for EHRI(J), the five inverse Hankel transforms
! needed for evaluation of Green's tensor elements unless J < NSPL

!     NRHO- number of points in 15 point / decade array
!        K - filter kernel index
!       JR - RHO interpolation index
!        L - filter point index
!    LMBDA - Hankel transform variable
!     NLYR - number of layers
!     SIGL - complex resistivity of layers
!  KSQ_LYR - propagation constants
!      THK = layer thicknesses
!      ZTR = (ZS - THICK) + (ZR - THICK) = the reflected distance between
!                 "source" & "receiver" points off top of basement.
!  EHRI(J) - accumulated complex integrals for inverse Hankel transform (J = 1,5)
!     JUMP - keep integrating if false.

 USE LA_Filter_coefficients

 IMPLICIT NONE
 REAL, PARAMETER :: TOL=1.E-5
 COMPLEX, PARAMETER :: ZERO = (0.,0.), ONE = (1.,0.)
 INTEGER NRHO,K,JR,L,NLYR,J,J1,NSPL
 REAL ZTR,LMBDA,EHR,EHI,AR,AI,ALF,RMU(NLYR),THK(NLYR)
 COMPLEX LMBSQ,EHRI(NRHO,5),KER(JNLO-NRHO:JNHI,5),GN,ETAN,TXP,TMP(5)
 COMPLEX, DIMENSION(NLYR) :: SIGL,KSQ_LYR,X,BR,BT
 COMPLEX, DIMENSION(0:NLYR) :: S,R,T
 LOGICAL TOO_BIG,JUMP

 INTENT (IN) NRHO,K,JR,L,LMBDA,NSPL,NLYR,SIGL,RMU,KSQ_LYR,THK,ZTR
 INTENT (INOUT) EHRI,JUMP,KER

 X = ONE ;  T = ZERO ;  R = ZERO

 LMBSQ = CMPLX (LMBDA * LMBDA, 0.)

 S(0) = CMPLX (LMBDA, 0.)
 S(1) = SQRT (LMBSQ + KSQ_LYR(1))
 S(NLYR) = SQRT (LMBSQ + KSQ_LYR(NLYR))
 TXP = EXP (-S(NLYR) * ZTR) / S(NLYR)
 T(0) = ((RMU(1)**2 -1.) * LMBSQ - KSQ_LYR(1)) / (RMU(1) * S(0) + S(1))**2
 R(0) = ONE

 DO J = NLYR-1,1,-1
   S(J) = SQRT (LMBSQ + KSQ_LYR(J))
   ALF = RMU(J+1)/RMU(J)
   T(J) = ((ALF**2 -1.) * S(J)**2 + KSQ_LYR(J) - KSQ_LYR(J+1)) / (ALF * S(J) + S(J+1))**2
   R(J) = (SIGL(J+1)*S(J) - SIGL(J)*S(J+1)) / (SIGL(J+1)*S(J) + SIGL(J)*S(J+1))
   X(J) = EXP (-2.* S(J) * THK(J))
 END DO

 BR(1) = -R(0) * X(1)
 BT(1) = -T(0) * X(1)
 DO J = 1, NLYR-1
   BR(J+1) = X(J+1) * (BR(J) - R(J)) / (ONE - BR(J) * R(J))
   BT(J+1) = X(J+1) * (BT(J) - T(J)) / (ONE - BT(J) * T(J))
 END DO

 ETAN = BT(NLYR) * TXP
 GN =   BR(NLYR) * TXP

 KER(K,1) = S(0) * ETAN
 KER(K,2) = ETAN +GN
 KER(K,3) = S(0) * GN
 KER(K,4) = LMBSQ * GN
 KER(K,5) = S(0) * S(NLYR) * GN

 TMP(1) = WJ0(L) * KER(K,1)
 TMP(2) = WJ1(L) * KER(K,2)
 TMP(3) = WJ0(L) * KER(K,3)
 TMP(4) = WJ1(L) * KER(K,4)
 TMP(5) = WJ0(L) * KER(K,5)

 JUMP = .TRUE.
 DO J1 = NSPL,5
   EHRI(JR,J1) = EHRI(JR,J1) + TMP(J1)
   AR = ABS (REAL (TMP(J1)))
   AI = ABS (AIMAG (TMP(J1)))
   EHR = ABS (REAL (EHRI(JR,J1)))
   EHI = ABS (AIMAG (EHRI(JR,J1)))
   TOO_BIG = .FALSE.
   IF (AR > TOL* EHR) TOO_BIG = .TRUE.
   IF (AI > TOL* EHI) TOO_BIG = .TRUE.
   IF (TOO_BIG) JUMP = .FALSE.
 END DO
END SUBROUTINE EGTVAL

 SUBROUTINE INTER_EGT_BOSS (JPS,JPR,MXAB,MXB,NPLT,NLYR,NA,NB,THK,RMU,KSQ_LYR, &
                            SIGL,PLDIP,PLAZM,XCELL,YCELL,ZCELL,NREGT,RHOTRP,    &
                            HAAI,HABI,HBAI,HBBI,SAAI,SABI,SBAI,SBBI)
!--------------------------------------------------------------------------------

!***  Called by: SCAT_MTRX_BOSS
!***      Calls: CUBVAL, INTER_EGT_DIR, INTER_EGT_CSPL, INTER_ROTATE_EGT

!  Computes the Green's tensor elements which give the response at plate JPR
!  of electric currents in plate JPS.  In what follows, the GTE have the
!  units of electric field divided by a factor (-iwu) because this factor is
!  explicitly included as a multiplier outside the integral over area when
!  solving the integral equation.
!
!  This formulation differs from that of EGT_BOSS in two ways.  Firstly, the
!  area integration is done in the calling routine rather than here.
!  Secondly, a different breakup of GIJ = SIJ + HIJ / KSQ is used.
!
!  Two conventions of EGT_BOSS are used.  Firstly, HIJ is redefined to be
!  HIJ = HIJ' + KSQ * SIJ is output where HIJ' is the curl free part and
!  SIJ is the divergence-free part.  Also, the GTE are computed in the
!  adjoint form.  See definitions below.
!  See EGT_BOSS or SCAT_MTRX_BOSS for further discussion of this.

!  NA & NB are the number of cells along strike and down dip respectively.
!  SIG contains the complex layer conductivities.
!  KSQ_LYR = iwu * SIG
!  THK = layer thicknesses
!
!
!    OUTPUT:
!   The adjoint Green's functions are expressed as:
!
!  HAAI (JCS,JCR) - relate the along strike component of the electric field at
!  SAAI (JCS,JCR)   cell JCR of plate JPR due to the along strike component
!                   of the scattering current of cell JCS of plate JPS
!
!  HABI (JCS,JCR) - relate the down dip component of the electric field at
!  SABI (JCS,JCR)   cell JCR of plate JPR due to the along strike component
!                   of the scattering current of cell JCS of plate JPS
!
!  HBAI (JCS,JCR) - relate the along strike component of the electric field at
!  SBAI (JCS,JCR)   cell JCR of plate JPR due to the down dip component
!                   of the scattering current of cell JCS of plate JPS
!
!  HBBI (JCS,JCR) - relate the down dip component of the electric field at
!  SBBI (JCS,JCR)   cell JCR of plate JPR due to the down dip component
!                   of the scattering current of cell JCS of plate JPS
!
! -------------------------------------------------------------------------

 IMPLICIT NONE
 COMPLEX, PARAMETER :: ZERO=(0.,0.)
 INTEGER JPS,JPR,MXB,MXAB,NPLT,NLYR,NA(NPLT),NB(NPLT),NREGT,JCS,JCR, &
         JB1,JB2,JA1,JA2
 REAL XD,YD,ZD,ZTR,RHO,COSD,SIND,THK(NLYR),XCELL(MXAB,NPLT),YCELL(MXAB,NPLT), &
      ZCELL(MXB,NPLT),CD2,SD2,CD1,SD1,CA2,SA2,CA1,SA1,RHOTRP(NREGT),QQR, &
      QQI,CUBVAL,RMU(NLYR),THICK
 REAL, DIMENSION (NPLT) :: PLDIP,PLAZM
 REAL, DIMENSION (4,NREGT):: GR1,GR2,GR3,GR4,GR5,GR6,GR7,GI1,GI2,GI3, &
                             GI4,GI5,GI6,GI7
 COMPLEX KSQ_LYR(NLYR),SIGL(NLYR),KBASE,KSQ2,Q1,Q2,Q3,Q4,Q5,Q6,Q7
 COMPLEX SXX,SXY,SXZ,SYX,SYY,SYZ,SZX,SZY,SZZ,HXX,HXY,HXZ,HYX,HYY,HYZ,HZX,HZY, &
         HZZ,GAA,GAB,GBA,GBB,SXXD,HXXD,HYYD,HZZD,HXYD,HYZD,HZXD
 COMPLEX, DIMENSION(MXAB,MXAB) :: HAAI,HABI,HBAI,HBBI,SAAI,SABI,SBAI,SBBI

 INTENT (IN) JPS,JPR,MXAB,NPLT,NLYR,NA,NB,SIGL,RMU,KSQ_LYR,THK,PLDIP,PLAZM
 INTENT (OUT) HAAI,HABI,HBAI,HBBI,SAAI,SABI,SBAI,SBBI

 CA1 = COS (PLAZM(JPS)); CA2 = COS (PLAZM(JPR)); SA1 = SIN (PLAZM(JPS)); SA2 = SIN (PLAZM(JPR))
 CD1 = COS (PLDIP(JPS)); CD2 = COS (PLDIP(JPR)); SD1 = SIN (PLDIP(JPS)); SD2 = SIN (PLDIP(JPR))

 THICK = SUM (THK(1:NLYR-1))
 KSQ2 = KSQ_LYR(NLYR)
 KBASE = CSQRT (KSQ2)
 IF (REAL (KBASE) < 0) KBASE = -KBASE

 HAAI = ZERO;  HBAI = ZERO;  HABI = ZERO;  HBBI = ZERO
 SAAI = ZERO;  SBAI = ZERO;  SABI = ZERO;  SBBI = ZERO

 DO JB1 = 1,NB(JPS)                             ! Step over source plate depths.
   DO JB2 = 1,NB(JPR)                           ! Step over receiver plate depths.
     ZD = ZCELL(JB2,JPR) - ZCELL(JB1,JPS)
     ZTR = ZCELL(JB2,JPR) + ZCELL(JB1,JPS) - 2. * THICK

! Set up splines for integrals for indirect terms

     CALL INTER_EGT_CSPL (NREGT,RHOTRP,NLYR,SIGL,RMU,KSQ_LYR,THK,ZTR,GR1,GR2, &
                          GR3,GR4,GR5,GR6,GR7,GI1,GI2,GI3,GI4,GI5,GI6,GI7)

     DO JA1 = 1,NA(JPS)
       JCS = JA1 + (JB1-1) * NA(JPS)
       DO JA2 = 1,NA(JPR)
         JCR = JA2 + (JB2-1) * NA(JPR)
         XD = XCELL(JCR,JPR) - XCELL(JCS,JPS)
         YD = YCELL(JCR,JPR) - YCELL(JCS,JPS)
         RHO = SQRT (XD**2 + YD**2)
         COSD = 0.;  SIND = 0.
         IF (RHO > 1.E-6) THEN
           COSD = XD / RHO
           SIND = YD / RHO
         END IF

! Get direct terms for each cell pair.

         CALL INTER_EGT_DIR (XD,YD,ZD,KBASE,KSQ2,SXXD,HXXD,HYYD,HZZD,HXYD, &
                             HYZD,HZXD)

! Evaluate the integrals and reflected terms for each cell pair

         QQR = CUBVAL(RHOTRP,GR1,NREGT,RHO)
         QQI = CUBVAL(RHOTRP,GI1,NREGT,RHO)
         Q1 = CMPLX (QQR,QQI)
         QQR = CUBVAL(RHOTRP,GR2,NREGT,RHO)
         QQI = CUBVAL(RHOTRP,GI2,NREGT,RHO)
         Q2 = CMPLX (QQR,QQI)
         QQR = CUBVAL(RHOTRP,GR3,NREGT,RHO)
         QQI = CUBVAL(RHOTRP,GI3,NREGT,RHO)
         Q3 = CMPLX (QQR,QQI)
         QQR = CUBVAL(RHOTRP,GR4,NREGT,RHO)
         QQI = CUBVAL(RHOTRP,GI4,NREGT,RHO)
         Q4 = CMPLX (QQR,QQI)
         QQR = CUBVAL(RHOTRP,GR5,NREGT,RHO)
         QQI = CUBVAL(RHOTRP,GI5,NREGT,RHO)
         Q5 = CMPLX (QQR,QQI)
         QQR = CUBVAL(RHOTRP,GR6,NREGT,RHO)
         QQI = CUBVAL(RHOTRP,GI6,NREGT,RHO)
         Q6 = CMPLX (QQR,QQI)
         QQR = CUBVAL(RHOTRP,GR7,NREGT,RHO)
         QQI = CUBVAL(RHOTRP,GI7,NREGT,RHO)
         Q7 = CMPLX (QQR,QQI)

! Compute the divergence free indirect terms, add direct terms and
! rotate them into the plate system.

         SXX = SXXD + Q4 * (2.*COSD**2 - 1.)  +  Q1 * SIND**2
         SXY = COSD * SIND * (2 * Q4 - Q1)
         SXZ = ZERO
         SYX = SXY
         SYY = SXXD + Q4 * (2.*SIND**2 - 1.)  +  Q1 * COSD**2
         SYZ = ZERO
         SZX = -COSD * Q6
         SZY = -SIND * Q6
         SZZ = SXXD + Q2

         CALL INTER_ROTATE_EGT (CA1,CA2,SA1,SA2,CD1,CD2,SD1,SD2,SXX,SXY,SXZ, &
                                SYX,SYY,SYZ,SZX,SZY,SZZ,GAA,GAB,GBA,GBB)

         SAAI(JCS,JCR) = GAA  !  The divergence free GTE are stored in
         SBAI(JCS,JCR) = GAB  !  adjoint form.
         SABI(JCS,JCR) = GBA
         SBBI(JCS,JCR) = GBB

! Compute the curl free indirect terms,  add direct terms and
! rotate them into the plate system.

         HXX = HXXD + KSQ2 * SXX + Q5 * (2.*COSD**2 - 1.) - Q3 * COSD**2
         HYY = HYYD + KSQ2 * SYY + Q5 * (2.*SIND**2 - 1.) - Q3 * SIND**2
         HZZ = HZZD + KSQ2 * SZZ - Q3
         HXY = HXYD + KSQ2 * SXY + COSD * SIND * (2 * Q5 - Q3)
         HYX = HXY
         HZX = HZXD + COSD * Q7
         HXZ = HZXD - COSD * Q7
         HZY = HYZD + SIND * Q7
         HYZ = HYZD - SIND * Q7

         CALL INTER_ROTATE_EGT (CA1,CA2,SA1,SA2,CD1,CD2,SD1,SD2,HXX,HXY,HXZ, &
                                HYX,HYY,HYZ,HZX,HZY,HZZ,GAA,GAB,GBA,GBB)

         HAAI(JCS,JCR) = GAA   !  The total GTE are stored in adjoint form.
         HBAI(JCS,JCR) = GAB
         HABI(JCS,JCR) = GBA
         HBBI(JCS,JCR) = GBB

       END DO   ! receiver plate columns
     END DO     ! source plate columns
   END DO       ! receiver plate rows
 END DO         ! source plate rows

END SUBROUTINE INTER_EGT_BOSS

 SUBROUTINE INTER_EGT_DIR (XD,YD,ZD,KBASE,KSQ2,SXXD,HXXD,HYYD,HZZD,HXYD,HYZD,HZXD)
!---------------------------------------------------------------------------------

!***  Called by: INTER_EGT_BOSS

!   Computes the direct (or full space) Green's tensor elements:
!
!  XD, YD, ZD - receiver coordinates relative to source dipole
!        KSQ2 - i omega mu sigma (basement)
!       KBASE - sqrt (KSQ2)
!
!   LeroiAir uses an integral equation formulation where an explicit factor (-iwu)
!   appears outside the integral.  Thus the Green's tensor elements, GIJ are in
!   units of electric field divided by this factor.  Moreover, these modified
!   GIJ can be expressed as the sum SIJ + HIJ / KSQ2, where SIJ and HIJ are the
!   divergence free and curl free parts respectively.  This is done for reasons
!   explained in EGT_BOSS and SCAT_MTRX_BUILD.
!   This subroutine returns SIJ and HIJ.  It does NOT return HIJ in the form
!   HIJ:= KSQ2 * GIJ = KSQ2 * SIJ + HIJ
!
!   In the direct case, SXX = SYY = SZZ.  SIJ (I/=J) = 0.
!   Also like the indirect case HXY = HYX, but unlike the indirect case,
!   HZX = HXZ and HZY = HYZ
!
!   These are symmetric wrt exchange of direction or TX-RX  point swap.
!
 IMPLICIT NONE
 COMPLEX, PARAMETER :: ONE=(1.,0.)
 REAL XD,YD,ZD,R,RSQ
 COMPLEX KBASE,KSQ2,K2R,FAC1,FAC2,BASE,SXXD,HXXD,HYYD,HZZD,HXYD,HYZD,HZXD

 INTENT (IN) XD,YD,ZD,KBASE,KSQ2
 INTENT (OUT) SXXD,HXXD,HYYD,HZZD,HXYD,HYZD,HZXD

 RSQ = XD**2 + YD**2 + ZD**2
 R = SQRT (RSQ)
 K2R = KBASE * R

 SXXD = EXP (-K2R) / (4. * 3.141592654 * R)

 BASE = SXXD / RSQ
 FAC1 = ONE + K2R
 FAC2 = BASE * (3.*FAC1 + KSQ2*RSQ) /RSQ
 FAC1 = FAC1 * BASE

 HXXD = FAC1 - FAC2 * XD**2
 HYYD = FAC1 - FAC2 * YD**2
 HZZD = FAC1 - FAC2 * ZD**2

 HXYD = -FAC2 * XD * YD
 HYZD = -FAC2 * YD * ZD
 HZXD = -FAC2 * XD * ZD

END SUBROUTINE INTER_EGT_DIR

 SUBROUTINE INTER_EGT_CSPL (NREGT,RHOTRP,NLYR,SIGL,RMU,KSQ_LYR,THK,ZTR,GR1,GR2, &
                             GR3,GR4,GR5,GR6,GR7,GI1,GI2,GI3,GI4,GI5,GI6,GI7)
!--------------------------------------------------------------------------------

!***  Called by: INTER_EGT_BOSS
!***      Calls: INTER_EGT_HNK, CUBSPL

!  Sets up cubic spline representations, GRj & GIj (j = 1,7), for the seven
!  Hankel integrals needed to compute the electric Green's tensor elements
!  as a function of RHO.

!    RHOTRP - interpolation array of rho values
!     NREGT - number of interpolation points needed for EGT
!      NLYR - number of layers
!      SIGL - complex conductivity of layers
!   KSQ_LYR - propagation constants
!       THK = layer thicknesses
!       ZTR = the reflected distance between "source" & "receiver"
!             points off bottom of overburden.

 IMPLICIT NONE
 INTEGER JR,NREGT,NLYR
 REAL, DIMENSION (4,NREGT) :: GR1,GR2,GR3,GR4,GR5,GR6,GR7,GI1,GI2,GI3,GI4, &
                              GI5,GI6,GI7
 REAL RHOTRP(NREGT),ZTR,THK(NLYR),RMU(NLYR)
 COMPLEX KSQ_LYR(NLYR),SIGL(NLYR),EHRI(NREGT,7)

 INTENT (IN) NREGT,RHOTRP,ZTR,NLYR,SIGL,RMU,KSQ_LYR,THK
 INTENT (OUT) GR1,GR2,GR3,GR4,GR5,GI1,GI2,GI3,GI4,GI5

 GR1 = 0; GR2 = 0; GR3 = 0; GR4 = 0; GR5 = 0; GR6 = 0; GR7 = 0
 GI1 = 0; GI2 = 0; GI3 = 0; GI4 = 0; GI5 = 0; GI6 = 0; GI7 = 0

 CALL INTER_EGT_HNK (NREGT,RHOTRP,NLYR,SIGL,RMU,KSQ_LYR,THK,ZTR,EHRI)

 DO JR = 1, NREGT
   GR1(1,JR) =  REAL (EHRI(JR,1))
   GR2(1,JR) =  REAL (EHRI(JR,2))
   GR3(1,JR) =  REAL (EHRI(JR,3))
   GR4(1,JR) =  REAL (EHRI(JR,4))
   GR5(1,JR) =  REAL (EHRI(JR,5))
   GR6(1,JR) =  REAL (EHRI(JR,6))
   GR7(1,JR) =  REAL (EHRI(JR,7))
   GI1(1,JR) = AIMAG (EHRI(JR,1))
   GI2(1,JR) = AIMAG (EHRI(JR,2))
   GI3(1,JR) = AIMAG (EHRI(JR,3))
   GI4(1,JR) = AIMAG (EHRI(JR,4))
   GI5(1,JR) = AIMAG (EHRI(JR,5))
   GI6(1,JR) = AIMAG (EHRI(JR,6))
   GI7(1,JR) = AIMAG (EHRI(JR,7))
 END DO

 CALL CUBSPL (RHOTRP,GR1,NREGT,0,0)
 CALL CUBSPL (RHOTRP,GI1,NREGT,0,0)
 CALL CUBSPL (RHOTRP,GR2,NREGT,0,0)
 CALL CUBSPL (RHOTRP,GI2,NREGT,0,0)
 CALL CUBSPL (RHOTRP,GR3,NREGT,0,0)
 CALL CUBSPL (RHOTRP,GI3,NREGT,0,0)
 CALL CUBSPL (RHOTRP,GR4,NREGT,0,0)
 CALL CUBSPL (RHOTRP,GI4,NREGT,0,0)
 CALL CUBSPL (RHOTRP,GR5,NREGT,0,0)
 CALL CUBSPL (RHOTRP,GI5,NREGT,0,0)
 CALL CUBSPL (RHOTRP,GR6,NREGT,0,0)
 CALL CUBSPL (RHOTRP,GI6,NREGT,0,0)
 CALL CUBSPL (RHOTRP,GR7,NREGT,0,0)
 CALL CUBSPL (RHOTRP,GI7,NREGT,0,0)

END SUBROUTINE INTER_EGT_CSPL

 SUBROUTINE INTER_EGT_HNK (NREGT,RHOTRP,NLYR,SIGL,RMU,KSQ_LYR,THK,ZTR,EHRI)
!--------------------------------------------------------------------------

!***  Called by: INTER_EGT_CSPL
!***      Calls: INTER_EGT_VAL

!  Sets up the seven integrals EHRI (JR,J1), J1 = 1,7, needed to compute the
!  electric Green's tensor elements in INTER_EGT_BOSS.  It uses the flow through
!  Hankel transform method to compute values at RHOTRP(JR), (15 point per decade
!  spacing).  It uses a 15 point per decade filter coefficient set derived from
!  Christensen's program, FLTGEN.

!    RHOTRP - interpolation array of rho values
!     NREGT - number of interpolation points needed for EGT
!      NLYR - number of layers
!      SIGL - complex conductivity of layers
!   KSQ_LYR - propagation constants
!       THK = layer thicknesses
!       ZTR = the reflected distance between "source" & "receiver"
!             points off bottom of overburden.
!      EHRI - inverse Hankel transform (J = 1,7)

  USE LA_Filter_coefficients

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 REAL, PARAMETER :: FOURPI=12.56637
 INTEGER NLYR,NREGT,L,JR,LMAX,LMIN,K,KMAX,KMIN,KBOT,K1
 REAL LMBDA,RHOTRP(NREGT),DIST,ZTR,THK(NLYR),RMU(NLYR)
 REAL(KIND=QL) DELTA,Y1,Y,RD
 COMPLEX KSQ_LYR(NLYR),SIGL(NLYR),KER(JNLO-NREGT:JNHI,7),EHRI(NREGT,7)
 LOGICAL JUMP

 INTENT(IN) NREGT,RHOTRP,NLYR,SIGL,RMU,KSQ_LYR,THK,ZTR
 INTENT(OUT) EHRI

 EHRI = (0.,0.)
 KER = (0.,0.)
 DELTA = LOG (10.D0)/ DBLE (NDEC_JN)

!  Set up KER for JR = 1 corresponding to minimum value of RHO.  This will
!  compute most of the needed kernel range from the high end.  Note that
!  the filter is defined between JNLO < L < JNHI

 JR = 1
 RD = DBLE (RHOTRP(1) )
 Y1 = -LOG (RD) - DBLE (JR-1) * DELTA - DBLE (SHFTJN)

 DO L = -50, JNHI             ! Start at L = -50 to pick up low values.
   LMAX = L                   ! Maximum filter index used
   K = L + 1 - JR             ! K is the kernel index.
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (REAL(Y))
   CALL INTER_EGT_VAL (NREGT,K,JR,L,LMBDA,NLYR,SIGL,RMU,KSQ_LYR,THK,ZTR, &
                       KER,EHRI,JUMP)
   IF (JUMP) EXIT
 END DO

 Y = Y1                       ! Finish off the low end for RHOTRP(1)
 DO L = -51, JNLO, -1
   LMIN = L                   ! Minimum filter index used
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (REAL(Y))
   K = L + 1 - JR             ! Compute the kernel index.
   CALL INTER_EGT_VAL (NREGT,K,JR,L,LMBDA,NLYR,SIGL,RMU,KSQ_LYR,THK,ZTR, &
                       KER,EHRI,JUMP)
   IF (JUMP) EXIT
 END DO

 KMIN = LMIN + 1 - JR  !  Define the range of kernel values
 KMAX = LMAX + 1 - JR  !  used for RHOTRP(1)

! Complete definition of kernel values by evaluating transform of
! maximum RHO = RHOTRP (NREGT)

 JR = NREGT
 Y1 = -LOG (RD) - DBLE (JR-1) * DELTA - DBLE (SHFTJN)
 KBOT = JNLO + 1 - JR
 K1 = MAX (KBOT,KMIN)

 DO K = K1, KMAX          ! Compute EHR for maximum RHO using previously
   L = K - 1 + JR         ! computed kernel values.
   IF (L > JNHI .OR. L < JNLO) CYCLE
   EHRI(JR,1:3) = EHRI(JR,1:3) + KER(K,1:3) * WJ0(L)
   EHRI(JR,4:7) = EHRI(JR,4:7) + KER(K,4:7) * WJ1(L)
 END DO

 IF (K1 > KBOT) THEN    !  Add low end kernel values until convergence.
   DO K = K1-1, KBOT, -1
     KMIN = K
     L = K - 1 + JR
     Y = Y1 + DBLE (L) * DELTA
     LMBDA = EXP (REAL(Y))
     CALL INTER_EGT_VAL (NREGT,K,JR,L,LMBDA,NLYR,SIGL,RMU,KSQ_LYR,THK,ZTR, &
                         KER,EHRI,JUMP)
     IF (JUMP) EXIT
   END DO
 END IF

 DO JR = 2, NREGT-1          !  Compute transforms for all other RHO values
   DO K = KMIN, KMAX         !  using previously computed kernel values.
     L = K - 1 + JR
     IF (L > JNHI .OR. L < JNLO) CYCLE
     EHRI(JR,1:3) = EHRI(JR,1:3) + KER(K,1:3) * WJ0(L)
     EHRI(JR,4:7) = EHRI(JR,4:7) + KER(K,4:7) * WJ1(L)
   END DO
 END DO

 DO JR = 1,NREGT
   EHRI(JR,4:5) = EHRI(JR,4:5) / RHOTRP(JR)
   DIST = FOURPI * RHOTRP(JR)
   EHRI(JR,1:7) = EHRI(JR,1:7) / DIST
 END DO

END SUBROUTINE INTER_EGT_HNK

 SUBROUTINE INTER_EGT_VAL (NREGT,K,JR,L,LMBDA,NLYR,SIGL,RMU,KSQ_LYR,THK,ZTR, &
                            KER,EHRI,JUMP)
!-----------------------------------------------------------------------------

!***  Called by: INTER_EGT_HNK

!  Accumulates the integrals for EHRI(J), the seven inverse Hankel transforms
!  needed for evaluation of Green's tensor elements.

!     NREGT - number of points in 15 point / decade array
!         K - filter kernel index
!        JR - RHO interpolation index
!         L - filter point index
!     LMBDA - Hankel transform variable
!      NLYR - number of layers
!      SIGL - complex resistivity of layers
!   KSQ_LYR - propagation constants
!       THK = layer thicknesses
!       ZTR = (ZS - THICK) + (ZR - THICK) = the reflected distance between
!                  "source" & "receiver" points off top of basement
!   EHRI(J) - accumulated complex integrals for inverse Hankel transform (J = 1,5)
!      JUMP - keep integrating if false.

  USE LA_Filter_coefficients

 IMPLICIT NONE
 REAL, PARAMETER :: TOL=1.E-5
 COMPLEX, PARAMETER :: ZERO = (0., 0.), ONE = (1., 0.)
 INTEGER NREGT,K,JR,L,NLYR,J,J1
 REAL ALF,THK(NLYR),ZTR,LMBDA,EHR,EHI,AR,AI,RMU(NLYR)
 COMPLEX EHRI(NREGT,7),KER(JNLO-NREGT:JNHI,7),LMBSQ,GN,ETAN,TXP,TMP(7)
 COMPLEX, DIMENSION(NLYR) :: SIGL,KSQ_LYR,X,BR,BT
 COMPLEX, DIMENSION(0:NLYR) :: S,R,T
 LOGICAL TOO_BIG,JUMP

 INTENT (IN) NREGT,K,JR,L,LMBDA,NLYR,SIGL,RMU,KSQ_LYR,THK,ZTR
 INTENT (INOUT) EHRI,JUMP,KER

 X = ONE ;  T = ZERO ;  R = ZERO

 LMBSQ = CMPLX (LMBDA * LMBDA, 0.)

 S(0) = CMPLX (LMBDA, 0.)
 S(1) = SQRT (LMBSQ + KSQ_LYR(1))
 S(NLYR) = SQRT (LMBSQ + KSQ_LYR(NLYR))
 TXP = EXP (-S(NLYR) * ZTR)
 T(0) = ((RMU(1)**2 -1.) * LMBSQ - KSQ_LYR(1)) / (RMU(1) * S(0) + S(1))**2
 R(0) = ONE

 DO J = NLYR-1,1,-1
   S(J) = SQRT (LMBSQ + KSQ_LYR(J))
   ALF = RMU(J+1)/RMU(J)
   T(J) = ((ALF**2 -1.) * S(J)**2 + KSQ_LYR(J) - KSQ_LYR(J+1)) / (ALF * S(J) + S(J+1))**2
   R(J) = (SIGL(J+1)*S(J) - SIGL(J)*S(J+1)) / (SIGL(J+1)*S(J) + SIGL(J)*S(J+1))
   X(J) = EXP (-2.* S(J) * THK(J))
 END DO
 BR(1) = -R(0) * X(1)
 BT(1) = -T(0) * X(1)
 DO J = 1, NLYR-1
   BR(J+1) = X(J+1) * (BR(J) - R(J)) / (ONE - BR(J) * R(J))
   BT(J+1) = X(J+1) * (BT(J) - T(J)) / (ONE - BT(J) * T(J))
 END DO
 ETAN = BT(NLYR) * TXP
 GN =   BR(NLYR) * TXP

 KER(K,1) = ETAN * S(0) / S(NLYR)
 KER(K,2) = GN * S(0) / S(NLYR)
 KER(K,3) = GN * S(0) * S(NLYR)
 KER(K,4) = ETAN / S(NLYR)
 KER(K,5) = GN * S(NLYR)
 KER(K,6) = GN
 KER(K,7) = GN * LMBSQ

 TMP(1:3) = WJ0(L) * KER(K,1:3)
 TMP(4:7) = WJ1(L) * KER(K,4:7)

 JUMP = .TRUE.
 DO J1 = 1,7
   EHRI(JR,J1) = EHRI(JR,J1) + TMP(J1)
   AR = ABS (REAL (TMP(J1)))
   AI = ABS (AIMAG (TMP(J1)))
   EHR = ABS (REAL (EHRI(JR,J1)))
   EHI = ABS (AIMAG (EHRI(JR,J1)))
   TOO_BIG = .FALSE.
   IF (AR > TOL* EHR) TOO_BIG = .TRUE.
   IF (AI > TOL* EHI) TOO_BIG = .TRUE.
   IF (TOO_BIG) JUMP = .FALSE.
 END DO
END SUBROUTINE INTER_EGT_VAL

 SUBROUTINE INTER_ROTATE_EGT (CA1,CA2,SA1,SA2,CD1,CD2,SD1,SD2,GXX,GXY,GXZ, &
                              GYX,GYY,GYZ,GZX,GZY,GZZ,GAA,GAB,GBA,GBB)
!---------------------------------------------------------------------------

!  Rotates the Green's tensor elements from the X.Y,Z coordinate system to the
!  "receiver" plate system, A,B where A is along strike and B is down dip.

!***  Called by: INTER_EGT_BOSS

!  CA1 & SA1 are the cos and sin of the strike angle of the "source" plate 1
!  CA2 & SA2 are the cos and sin of the strike angle of the "receiver" plate 2
!  CD1 & SD1 are the cos and sin of the dip angle of the "source" plate 1
!  CD2 & SD2 are the cos and sin of the dip angle of the "receiver" plate 2
!
!  GAA is the along strike field at plate 2 due to an along strike current on plate 1.
!  GAB is the along strike field at plate 2 due to a down dip current on plate 1.
!  GBA is the down dip field at plate 2 due to an along strike current on plate 1.
!  GBB is the down dip field at plate 2 due to a down dip current on plate 1.

!  GXY is the X field at plate 2 due to a Y current on plate 1.  Note that
!  GIJ(2,1) = GJI(1,2) in the X,Y,Z system implies the same in the A,B system

 IMPLICIT NONE
 REAL CA1,CA2,SA1,SA2,CD1,CD2,SD1,SD2
 COMPLEX GAA,GAB,GBA,GBB,GXX,GYY,GZZ,GXY,GYX,GYZ,GZY,GZX,GXZ

 INTENT (IN)  CA1,CA2,SA1,SA2,CD1,CD2,SD1,SD2,GXX,GYY,GZZ,GXY,GYX,GYZ,GZY,GZX,GXZ
 INTENT (OUT) GAA,GAB,GBA,GBB

! The commented equations below are a generic representation of the rotation.
! In practice we use the following:  HXY = HYX,  HXZ = -


 GAA =        CA2* ( GXX*CA1 + GXY*SA1) + SA2* ( GYX*CA1 + GYY*SA1)

 GAB = CD1 * (CA2* (-GXX*SA1 + GXY*CA1) + SA2* (-GYX*SA1 + GYY*CA1)) &
     + SD1 * (GXZ*CA2 + GYZ*SA2)

 GBA = CD2 * (CA1* (-GXX*SA2 + GYX*CA2) + SA1* (-GXY*SA2 + GYY*CA2)) &
     + SD2 * (GZX*CA1 + GZY*SA1)

 GBB = CD1*CD2 * (SA1*SA2*GXX - SA2*CA1*GXY - SA1*CA2*GYX + CA2*CA1*GYY) &
     + CD2*SD1 * (-SA2*GXZ + CA2*GYZ) &
     + CD1*SD2 * (-SA1*GZX + CA1*GZY) +  SD1*SD2 * GZZ

END SUBROUTINE INTER_ROTATE_EGT

 SUBROUTINE MGT_BOSS (JQ,JP,NPLT,MXAB,NA,NB,PLDIP,NLYR,RMU,KSQ_LYR,THK,NSTAT,SAME_TX,  &
                      NRX,XRM,YRM,RZ,NRMGT,RHOTRP,NCPTS,MXGS,XGS,YGS,ZGS,WXZY,HA,HB)
!------------------------------------------------------------------------------------

!***  Called by: SCAT_MAG
!***      Calls: MGTBS, MGTV1

!  Computes the three components of the magnetic field at all receiver
!  positions produced by the induced along strike and down dip tangential
!  electric dipoles at centres of all cells of plate JP.
!  The coordinate system is centred on the centre point of the top edge of
!  the plate.  The XI axis is defined as along strike and the positive ETA axis
!  points down dip.  (It has Z and Y components.)  Positive Z points down.

!  This will be done by integrating the appropriate combinations of the
!  magnetic Green's tensor elements over the area of the plate (XI, ETA).
!  Uniformly spaced integration rule is used.
!  Thus there are NCPTS lines per cell running in the XI direction.
!  For each line of different ETA value, five basic elements are computes at
!  several knots, interpolated and combined into different magnetic Green's
!  tensor elements.  These are integrated along XI for each cell.

!               INPUT
!               -----
!        JQ - Tx-Rx offset index
!      NPLT - Number of plates (index = JP)
!      MXAB - Number of cells in biggest plate
!    NA, NB - number of cells along strike and down dip respectively
!     PLDIP - dip angle.
!      NLYR - number of layers including overburden.
!   KSQ_LYR - propagation constants for layers
!       THK = layer thicknesses
!     NSTAT - number of stations on profile
!   SAME_TX - used to avoid repeat computations
!       NRX - number of physical receivers (RXs)
!   RZ(I,J) - altitude of receiver I at station J
!  XRM, YRM - transformed  receiver positions wrt plate centre for plate JP
!    RHOTRP - horizontal interpolation array for MGT
!     NRMGT - number of points in RHOTRP
!       XGS - location of integration points along strike
!  YGS, ZGS - coordinates for integration down dip
!      MXGS - maximum of Gaussian integration points
!     NCPTS - number of integration points per dimension per cell.
!      WXZY - total integration weight for each point for plate JP.

!               OUTPUT
!               ------

!    HA(I,JCELL,JS) is the I-th component of the magnetic field at
!                   station JS which would be produced by a unitary
!                   electric dipole in the strike direction in the
!                   centre of plate cell number JCELL.

!    HB(I,JCELL,JS) is the I-th component of the magnetic field at
!                    station JS which would be produced by a unitary
!                    electric dipole in the dip direction in the centre
!                    of plate cell number JCELL.
!
!                    I = 1   is the along strike component
!                    I = 2   is the across strike horizontal component
!                    I = 3   is the vertical component

!  HA & HB are thus combinations of magnetic Green's tensor elements
!  integrated over the area of individual cells.

 IMPLICIT NONE
 COMPLEX, PARAMETER :: ZERO=(0.,0.)
 INTEGER JQ,JP,NPLT,MXAB,MXGS,NSTAT,NRX,NLYR,NCPTS,NRMGT,JA,JB,JGA,JGB,KDGS, &
         JS,JCELL,KSGS
 INTEGER, DIMENSION (NPLT) :: NA,NB
 REAL RHOTRP(NRMGT),RMU(NLYR),THK(NLYR),CDIP,ALT,XD,YD,RHO,XBAR,YBAR,ZS,YS, &
      WXZYL,RZ(NSTAT,NRX)
 REAL, DIMENSION (NPLT) :: PLDIP,WXZY
 REAL, DIMENSION (NSTAT,NRX,NPLT) :: XRM,YRM
 REAL, DIMENSION (4,NRMGT) :: QB1R,QB2R,QB3R,QB1I,QB2I,QB3I
 REAL, DIMENSION (MXGS,NPLT) :: XGS,YGS,ZGS
 COMPLEX HA(MXAB,3,NSTAT),HB(MXAB,3,NSTAT),KBASE,KSQ_LYR(NLYR), &
         MXX,MXY,MYX,MYY,MZX,MZY,Q1,Q2,Q3
 LOGICAL SAME_TX(NSTAT)


 KBASE = CSQRT (KSQ_LYR(NLYR))
 IF (REAL (KBASE) < 0.) KBASE = -KBASE

 CDIP = COS (PLDIP(JP))
 WXZYL = WXZY(JP)
 HA = ZERO
 HB = ZERO

!  Step down dip cell by cell, and within each cell, one integration
!  line at a time.  Then step through receivers.  Set up the base integrals,
!  QB*j for each line - receiver height combination.
!  If next receiver has the same height, retain base integrals.

 LOOP_JB_NB: DO JB = 1,NB(JP)
   LOOP_JGB_GS: DO JGB = 1,NCPTS
     KDGS = JGB + (JB-1) * NCPTS
     YS = YGS(KDGS,JP)
     ZS = ZGS(KDGS,JP)
     STN_LOOP: DO JS = 1,NSTAT
       ALT = RZ(JS,JQ)
       IF (.NOT. SAME_TX(JS)) &
         CALL MGTBS (NRMGT,RHOTRP,NLYR,RMU,KSQ_LYR,THK,ALT, &
                     ZS,QB1R,QB1I,QB2R,QB2I,QB3R,QB3I)

!  Step along strike cell by cell, and within each cell, Gauss point by point.
!  WXZYL = total integration weight for each point for plate JP.

       LOOP_JA_NA: DO JA = 1, NA(JP)
         JCELL = JA + ((JB-1) * NA(JP))

         LOOP_JGA_GS: DO JGA = 1,NCPTS
           KSGS = JGA + (JA-1) * NCPTS
           XD = XRM(JS,JQ,JP) - XGS(KSGS,JP)
           YD = YRM(JS,JQ,JP) - YS

!  MGTV1 interpolates the values of real integrals QB*j onto complex
!  values Qj at the integration points.

           RHO = SQRT (XD**2 + YD**2)
           RHO = MAX (RHO, RHOTRP(1))
           CALL MGTV1 (NRMGT,RHOTRP,QB1R,QB1I,QB2R,QB2I,QB3R,QB3I,RHO,Q1,Q2,Q3)
           XBAR = 0.
           YBAR = 0.
           IF (RHO > 1.E-6) THEN
             XBAR = XD / RHO
             YBAR = YD / RHO
           END IF
           MXX =  XBAR * YBAR * (2.* Q2 - Q1)
           MYY = -MXX
           MXY = Q2* (2.*YBAR**2 -1.) + Q1* (1. - YBAR**2)
           MYX = Q2* (1.- 2.*XBAR**2) - Q1* (1. - XBAR**2)
           MZX =  YBAR * Q3
           MZY = -XBAR * Q3

!  Integration by adding up weighted values

           HA(JCELL,1,JS) = HA(JCELL,1,JS) + (WXZYL * MXX)
           HA(JCELL,2,JS) = HA(JCELL,2,JS) + (WXZYL * MYX)
           HA(JCELL,3,JS) = HA(JCELL,3,JS) + (WXZYL * MZX)

           HB(JCELL,1,JS) = HB(JCELL,1,JS) + (WXZYL * CDIP * MXY)
           HB(JCELL,2,JS) = HB(JCELL,2,JS) + (WXZYL * CDIP * MYY)
           HB(JCELL,3,JS) = HB(JCELL,3,JS) + (WXZYL * CDIP * MZY)

         END DO LOOP_JGA_GS
       END DO LOOP_JA_NA
     END DO STN_LOOP
   END DO LOOP_JGB_GS
 END DO LOOP_JB_NB
END SUBROUTINE MGT_BOSS

 SUBROUTINE MGTBS (NRMGT,RHOTRP,NLYR,RMU,KSQ_LYR,THK,ALT,ZS,QB1R,QB1I,QB2R,QB2I, &
                   QB3R,QB3I)
!---------------------------------------------------------------------------------

!  Computes QBjR, and QBjI (j = 1,3) the real and imaginary parts of the
!  base Hankel transform integrals for specified ALT and ZS for the full
!  range of horizontal distances between receivers and points in the plate.
!  These are splined and used to compute magnetic Green's tensor elements.

!***  Called by: MGT_BOSS
!***      Calls: MGTHNK, CUBSPL

!                  INPUT
!                  -----
!         NRMGT - number of points in RHOTRP needed for MGT computation
!        RHOTRP - abscissa array for interpolation in horizontal direction
!          NLYR - number of layers including overburden.
!       KSQ_LYR - i * omega * mu * conductivity for each layer
!           THK = layer thicknesses
!           ALT - receiver altitude
!            ZS - depth of induced "source point" in plate

 IMPLICIT NONE
 INTEGER NRMGT,NLYR,JR
 REAL ALT,ZS,THK(NLYR),RHOTRP(NRMGT),RMU(NLYR)
 REAL, DIMENSION (4,NRMGT) :: QB1R,QB2R,QB3R,QB1I,QB2I,QB3I
 COMPLEX KSQ_LYR(NLYR),MHRI(NRMGT,3)

 INTENT (IN) NRMGT,RHOTRP,NLYR,RMU,KSQ_LYR,THK,ALT,ZS
 INTENT (OUT) QB1R,QB1I,QB2R,QB2I,QB3R,QB3I

 QB1R = 0.; QB2R = 0.; QB3R = 0.
 QB1I = 0.; QB2I = 0.; QB3I = 0.

 CALL MGTHNK (NRMGT,RHOTRP,NLYR,RMU,KSQ_LYR,THK,ALT,ZS,MHRI)
 DO JR = 1,NRMGT

   QB1R(1,JR) = REAL (MHRI(JR,1))
   QB2R(1,JR) = REAL (MHRI(JR,2))
   QB3R(1,JR) = REAL (MHRI(JR,3))
   QB1I(1,JR) = AIMAG (MHRI(JR,1))
   QB2I(1,JR) = AIMAG (MHRI(JR,2))
   QB3I(1,JR) = AIMAG (MHRI(JR,3))

 END DO

 CALL CUBSPL (RHOTRP,QB1R,NRMGT,0,0)
 CALL CUBSPL (RHOTRP,QB1I,NRMGT,0,0)

 CALL CUBSPL (RHOTRP,QB2R,NRMGT,0,0)
 CALL CUBSPL (RHOTRP,QB2I,NRMGT,0,0)

 CALL CUBSPL (RHOTRP,QB3R,NRMGT,0,0)
 CALL CUBSPL (RHOTRP,QB3I,NRMGT,0,0)

END SUBROUTINE MGTBS

 SUBROUTINE MGTHNK (NRMGT,RHOTRP,NLYR,RMU,KSQ_LYR,THK,ALT,ZS,MHRI)
!-------------------------------------------------------------------

!***  Called by: MGTBS
!***      Calls: MGTVAL

!  Sets up the three integrals MHRI (JR,J1), J1 = 1,3, needed to compute the
!  magnetic Green's tensor elements in MGT_BOSS.  It uses the flow through
!  Hankel transform method to compute values at RHOTRP(JR), (15 point per decade
!  spacing).  It uses a 15 point per decade filter coefficient set derived from
!  Christensen's program, FLTGEN.

!              INPUT
!              -----
!    RHOTRP - interpolation array of rho values
!     NRMGT - number of interpolation points needed for MGT
!      NLYR - number of layers
!   KSQ_LYR - propagation constants
!       THK = layer thicknesses
!       ALT - receiver altitude
!        ZS - depth of induced "source point" in plate

!  This subroutine returns MHRI for a magnetic field in nT
!  It uses VFAC = 1.e9 * mu0 / (4 Pi) = 1.e9 * 1.e-7 * 4*Pi / (4 Pi) = 100

 USE LA_Filter_coefficients

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 REAL, PARAMETER :: VFAC=100.
 INTEGER NLYR,NRMGT,L,JR,LMAX,LMIN,K,KMAX,KMIN,KBOT,K1
 REAL LMBDA,THK(NLYR),ALT,ZS,RHOTRP(NRMGT),RMU(NLYR)
 REAL(KIND=QL) DELTA,Y1,Y,RD
 COMPLEX KSQ_LYR(NLYR),KER(JNLO-NRMGT:JNHI,2),MHRI(NRMGT,3)
 LOGICAL JUMP

 INTENT (IN) NRMGT,RHOTRP,NLYR,RMU,KSQ_LYR,THK,ALT,ZS
 INTENT (OUT) MHRI

!  Initialise variables

 MHRI = (0.,0.)
 KER = (0.,0.)
 DELTA = LOG (10.D0)/ DBLE (NDEC_JN)

!  Set up KER for JR = 1 corresponding to minimum value of RHO.  This will
!  compute most of the needed kernel range from the high end.  Note that
!  the filter is defined between JNLO < L < JNHI

 JR = 1
 RD = DBLE (RHOTRP(1) )
 Y1 = -LOG (RD) - DBLE (JR-1) * DELTA - DBLE (SHFTJN)

 DO L = -50, JNHI             ! Start at L = -50 to pick up low values.
   LMAX = L                   ! Maximum filter index used
   K = L + 1 - JR             ! K is the kernel index.
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (REAL(Y))
   CALL MGTVAL (NRMGT,K,JR,L,LMBDA,NLYR,RMU,KSQ_LYR,THK,ALT,ZS,KER,MHRI,JUMP)

   IF (JUMP) EXIT
 END DO

 Y = Y1                       ! Finish off the low end for RHOTRP(1)
 DO L = -51, JNLO, -1
   LMIN = L                   ! Minimum filter index used
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (REAL(Y))
   K = L + 1 - JR             ! Compute the kernel index.
   CALL MGTVAL (NRMGT,K,JR,L,LMBDA,NLYR,RMU,KSQ_LYR,THK,ALT,ZS,KER,MHRI,JUMP)
   IF (JUMP) EXIT
 END DO

 KMIN = LMIN + 1 - JR  !  Define the range of kernel values
 KMAX = LMAX + 1 - JR  !  used for RHOTRP(1)

! Complete definition of kernel values by evaluating transform of
! maximum RHO = RHOTRP (NRMGT)

 JR = NRMGT
 Y1 = -LOG (RD) - DBLE (JR-1) * DELTA - DBLE (SHFTJN)
 KBOT = JNLO + 1 - JR
 K1 = MAX (KBOT,KMIN)

 DO K = K1, KMAX          ! Compute EHR for maximum RHO using previously
   L = K - 1 + JR         ! computed kernel values.
   IF (L > JNHI .OR. L < JNLO) CYCLE
   MHRI(JR,1) = MHRI(JR,1) + KER(K,1) * WJ0(L)
   MHRI(JR,2) = MHRI(JR,2) + KER(K,2) * WJ1(L)
   MHRI(JR,3) = MHRI(JR,3) - KER(K,1) * WJ1(L)
 END DO

 IF (K1 > KBOT) THEN    !  Add low end kernel values until convergence.
   DO K = K1-1, KBOT, -1
     KMIN = K
     L = K - 1 + JR
     Y = Y1 + DBLE (L) * DELTA
     LMBDA = EXP (REAL(Y))
     CALL MGTVAL (NRMGT,K,JR,L,LMBDA,NLYR,RMU,KSQ_LYR,THK,ALT,ZS,KER,MHRI,JUMP)
     IF (JUMP) EXIT
   END DO
 END IF

 DO JR = 2, NRMGT-1          !  Compute transforms for all other RHO values
   DO K = KMIN, KMAX         !  using previously computed kernel values.
     L = K - 1 + JR
     IF (L > JNHI .OR. L < JNLO) CYCLE
     MHRI(JR,1) = MHRI(JR,1) + KER(K,1) * WJ0(L)
     MHRI(JR,2) = MHRI(JR,2) + KER(K,2) * WJ1(L)
     MHRI(JR,3) = MHRI(JR,3) - KER(K,1) * WJ1(L)
   END DO
 END DO

 DO JR = 1,NRMGT
   MHRI(JR,2) = MHRI(JR,2) / RHOTRP(JR)
   MHRI(JR,1:3) = VFAC * MHRI(JR,1:3) / RHOTRP(JR)
 END DO

END SUBROUTINE MGTHNK

 SUBROUTINE MGTVAL (NRMGT,K,JR,L,LMBDA,NLYR,RMU,KSQ_LYR,THK,ALT,ZS,KER,MHRI,JUMP)
!----------------------------------------------------------------------------------

!***  Called by: MGTHNK

!  Accumulates the integrals MHRI(J) (J=1,3) for 3 inverse Hankel transforms
!  needed for evaluation of magnetic Green's tensor elements.

!     NRMGT - number of points in 15 point / decade array
!         K - filter kernel index
!        JR - RHO interpolation index
!         L - filter point index
!     LMBDA - Hankel transform variable
!      NLYR - number of layers
!   KSQ_LYR - propagation constants
!       THK = layer thicknesses
!       ALT - receiver altitude
!        ZS - depth of induced "source point" in plate

!   MHRI(J) - accumulated integrals for real and imaginary parts
!             of inverse Hankel transform (J = 1,3)
!      JUMP - keep integrating if false.

 USE LA_Filter_coefficients

 IMPLICIT NONE
 REAL, PARAMETER :: TOL=1.E-5
 COMPLEX, PARAMETER :: ZERO=(0.,0.), ONE=(1.,0.)
 INTEGER NRMGT,K,JR,L,NLYR,J1,J
 REAL THICK,ALF,LMBDA,MHR,MHI,AR,AI,THK(NLYR),ALT,ZS,RMU(NLYR)
 COMPLEX THKS,DENOM,P0,TXP,TMP(3),KER(JNLO-NRMGT:JNHI,2),MHRI(NRMGT,3),LMBSQ,XSUM
 COMPLEX, DIMENSION(NLYR) :: KSQ_LYR,X,BX
 COMPLEX, DIMENSION(0:NLYR) :: S,T,TP1
 LOGICAL TOO_BIG,JUMP

 INTENT (IN) NRMGT,K,JR,L,LMBDA,NLYR,RMU,KSQ_LYR,THK,ALT,ZS
 INTENT (INOUT) MHRI,JUMP,KER

 X = ONE ;  T = ZERO ;  TP1 = ZERO

 THICK = SUM (THK(1:NLYR-1))
 LMBSQ = CMPLX (LMBDA * LMBDA, 0.)

 S(0) = CMPLX (LMBDA, 0.)
 S(1) = SQRT (LMBSQ + KSQ_LYR(1))
 S(NLYR) = SQRT (LMBSQ + KSQ_LYR(NLYR))
 T(0) = ((RMU(1)**2 -1.) * LMBSQ - KSQ_LYR(1)) / (RMU(1) * S(0) + S(1))**2
 TP1(0) = 2.* RMU(1)* S(0) / (RMU(1) * S(0) + S(1))
 XSUM = S(0) * ALT + S(NLYR) * (ZS - THICK)

 DO J = NLYR-1,1,-1
   S(J) = SQRT (LMBSQ + KSQ_LYR(J))
   ALF = RMU(J+1)/RMU(J)
   DENOM = ALF * S(J) + S(J+1)
   T(J) = ((ALF**2 - 1.) * S(J)**2 + KSQ_LYR(J) - KSQ_LYR(J+1)) / DENOM**2
   TP1(J) =  2. * ALF * S(J) / DENOM
   THKS = S(J) * THK(J)
   X(J) = EXP (-2.* THKS)
   XSUM = XSUM + THKS
 END DO
 TXP = EXP (-XSUM)

 BX(1) = -T(0) * X(1)
 DO J = 1, NLYR-2
   BX(J+1) = X(J+1) * (BX(J) - T(J)) / (ONE - BX(J) * T(J))
 END DO

 P0 = TXP * TP1(0)
 DO J = 1,NLYR-1
   P0 = P0 * TP1(J) / (ONE - T(J)*BX(J))
 END DO

 KER(K,1) = -P0 * S(0)
 KER(K,2) = -P0


 TMP(1) =  KER(K,1) * WJ0(L)
 TMP(2) =  KER(K,2) * WJ1(L)
 TMP(3) = -KER(K,1) * WJ1(L)

!  Accumulate 3 integrals simultaneously until convergence of all.

 JUMP = .TRUE.
 DO J1 = 1, 3
   MHRI(JR,J1) = MHRI(JR,J1) + TMP(J1)
   AR = ABS (REAL (TMP(J1)))
   AI = ABS (AIMAG (TMP(J1)))
   MHR = ABS (REAL (MHRI(JR,J1)))
   MHI = ABS (AIMAG (MHRI(JR,J1)))
   TOO_BIG = .FALSE.
   IF (AR > TOL* MHR) TOO_BIG = .TRUE.
   IF (AI > TOL* MHI) TOO_BIG = .TRUE.
   IF (TOO_BIG) JUMP = .FALSE.
 END DO

END SUBROUTINE MGTVAL

  SUBROUTINE MGTV1 (NRMGT,RHOTRP,QB1R,QB1I,QB2R,QB2I,QB3R,QB3I,RHO,Q1,Q2,Q3)
! --------------------------------------------------------------------------

!***  Called by: MGT_BOSS
!***      Calls: CUBVAL

!  Sets up complex values Q1 - Q3 at RHO through calls to splined functions
!  QBjR and QBjI, j = 1,3.

 IMPLICIT NONE
 INTEGER NRMGT
 REAL RHOTRP(NRMGT),RHO,CUBVAL,QR1,QR2,QR3,QI1,QI2,QI3
 REAL, DIMENSION (4,NRMGT) :: QB1R,QB2R,QB3R,QB1I,QB2I,QB3I
 COMPLEX Q1,Q2,Q3

 INTENT (IN) NRMGT,RHOTRP,QB1R,QB1I,QB2R,QB2I,QB3R,QB3I,RHO
 INTENT (OUT) Q1,Q2,Q3

 QR1 = CUBVAL (RHOTRP,QB1R,NRMGT,RHO)
 QI1 = CUBVAL (RHOTRP,QB1I,NRMGT,RHO)
 Q1 = CMPLX (QR1, QI1)

 QR2 = CUBVAL (RHOTRP,QB2R,NRMGT,RHO)
 QI2 = CUBVAL (RHOTRP,QB2I,NRMGT,RHO)
 Q2 = CMPLX (QR2, QI2)

 QR3 = CUBVAL (RHOTRP,QB3R,NRMGT,RHO)
 QI3 = CUBVAL (RHOTRP,QB3I,NRMGT,RHO)
 Q3 = CMPLX (QR3, QI3)

END SUBROUTINE MGTV1

 SUBROUTINE PRM_BOSS (TDFD,NRXST,TXCLN,TXA90,JF,FRQ,NSTAT,SAME_TX,SX,SY,SZ,FANGLE, &
                      NLYR,RMU,KSQ_LYR,THK,NPLT,PLAZM,PLDIP,MXB,MXAB,NB,NA,XCELL,  &
                      YCELL,ZCELL,NRPRM,RHOTRP,E_PRYM)
! --------------------------------------------------------------------------------

!***  Called by: LEROI_3D
!***      Calls: PRMHNK, CUBSPL, CUBVAL

!  For a unit airborne dipole source of dip TXCLN, and azimuth, FANGLE, HSBOSS
!  computes the tangential components of the primary electric field, E_PRYM,
!  in NPLT plates lying in a uniform half-space or in the basement of a host
!  beneath NLYR horizontal layers.  RES contains the resistivities of
!  the overburden and basement.  The target (plate) is discretised along
!  strike into NAL cells of length DA and down dip into NBL cells of width DB.
!
!  For a vertical plate along the X axis, the cells are numbered along strike
!  from the South to the North, first along the top row and then in the same
!  direction along lower rows.  The last cell would be in the bottom
!  North corner.  In plan view, the X and Y components of E_PRYM are positive
!  to the North and East respectively.
!
!      TDFD = 1 for time domain;   = 2 for frequency domain
!     NRXST = NSTAT for time domain;   = NFRQ for frequency domain
!     TXCLN - TX inclination angle in the vertical plane along flight path
!     TXA90 - true for vertical co-planar briadside array
!        JF - frequency index
!       FRQ - frequency
!   SAME_TX - used to eliminate repeat computations
!     NSTAT - number of flight line transmitter positions
!     SX,SY - North, East coordinates for transmitter station
!        SZ - source altitude
!    FANGLE - flight path angle in radians. (north = 0; east = PI/2)
!      NLYR - number of layers including basement.
!   KSQ_LYR - iwu * SIG for all layers
!       THK - layer thicknesses
!      NPLT - number of plates
!     PLAZM - strike angle in radians
!     PLDIP - dip angle (PI/2 for vertical plate)
!       MXB - maximum number of cells down dip
!      MXAB - maximum number of cells in one plate
!    NA, NB - number of cells along strike & down dip respectively for each plate
! XCELL(k,*) - north coordinate of centre of cell k
! YCELL(k,*) - east coordinate of centre of cell k
!     NRPRM - number of horizontal interpolation points needed for primary field.
!    RHOTRP - 15 points per decade interpolation array
!   ZCELL(i,*) - depth of cell centre in row i relative to surface.
!  E_PRYM(1,*) - complex primary electric fields parallel to strike.
!  E_PRYM(2,*) - complex primary electric fields perpendicular to strike.
!
!    The first NAB components of E_PRYM are the fields along strike and
!    the second group of NA*NB components are along dip.  Thus for a strike
!    along the X axis, the first NA*NB components would be the X component
!    of the primary electric field and the second NA*NB components
!    would be the Y component multiplied by the cosine of the dip angle.
! -------------------------------------------------------------------------

 USE LA_Filter_coefficients
 IMPLICIT NONE
 REAL, PARAMETER :: MU0=12.56637E-7, PI2= 1.570796
 INTEGER TDFD,MXB,MXAB,NPLT,JP,JS,JF,NLYR,NBL,NAL,NSTAT,JB,JA,JAB,NRPRM,NA(NPLT), &
         NB(NPLT),NRXST,NINTG,JQ
 REAL FRQ,TXCLF,TXCLN(NRXST),CSTX,SNTX,SX(NSTAT),SY(NSTAT),SZ(NSTAT),FANGLE(NSTAT), &
      CSF,SNF,ALT,THK(NLYR),CUBVAL,PLAZM(NPLT),PLDIP(NPLT),XCELL(MXAB,NPLT),YCELL(MXAB,NPLT), &
      ZCELL(MXB,NPLT),RHO,YR,RHOTRP(NRPRM),SSTRL,CSTRL,YR2,YR3,YI,YI2,YI3,XB2,XBAR,XBAR0, &
      YBAR,YBAR0,RMU(NLYR),DELT
 REAL, DIMENSION (4,NRPRM) :: QR1,QR2,QR3,QI1,QI2,QI3
 COMPLEX EFAC,KSQ_LYR(NLYR),EX,EY,E_PRYM(2,MXAB,NSTAT,NPLT),QQ(3)
 COMPLEX, DIMENSION(:,:,:), ALLOCATABLE :: FXX,KER
 LOGICAL TXA90,SAME_TX(NSTAT)

 INTENT (OUT) E_PRYM

!  Set up transmitter sine and cosine

 NINTG = 1
 DELT = 0.
 IF (TXA90) DELT = PI2  ! For VCPB array rotate transmitter azimuth by PI/ 2

 ALLOCATE (FXX(NRPRM,3,MXB), KER(JNLO-NRPRM:JNHI,2,MXB) )
 FXX=(0.,0.); KER=(0.,0.)

 EFAC = (0.,1.) * FRQ * MU0 / 2.   ! iwu / 4 PI u0 used because source is in air.
 E_PRYM = (0.,0.)

 PLATE_LOOP: DO JP = 1, NPLT

   NAL = NA(JP)  ! Set up local dimensions for one-time plate operations
   NBL = NB(JP)

!  Set up array FXX which contains values of the F-potential as a function of
!  of horizontal distance and depth.  Step through different altitude levels.

   STN_LOOP: DO JS = 1,NSTAT

     CSF = COS (FANGLE(JS) + DELT)
     SNF = SIN (FANGLE(JS) + DELT)
     SNTX = 0.
     CSTX = 1.
     JQ = JF
     IF (TDFD < 2) JQ = JS
     TXCLF = TXCLN(JQ)
     IF (ABS (TXCLF) .GT. 1.E-4) THEN
       NINTG = 3
       SNTX = SIN (TXCLF)
       CSTX = COS (TXCLF)
     END IF

     ALT = SZ(JS)
     IF (.NOT. SAME_TX(JS)) &
       CALL PRMHNK (NRPRM,RHOTRP,NPLT,JP,MXB,NBL,ZCELL,ALT,NLYR,THK,RMU,KSQ_LYR, &
                    NINTG,KER,FXX)

!  Step through depths, splining FXX for each depth as a function of RHO, the
!  horizontal distance.  Add up contributions for each dipole for each
!  transmitter.

     DEPTH_STEP: DO JB = 1, NBL
       QR1(1,1:NRPRM) = REAL (FXX(1:NRPRM,1,JB))
       QI1(1,1:NRPRM) = AIMAG (FXX(1:NRPRM,1,JB))
       CALL CUBSPL (RHOTRP,QR1,NRPRM,0,0)
       CALL CUBSPL (RHOTRP,QI1,NRPRM,0,0)
       IF (NINTG == 3) THEN
         QR2(1,1:NRPRM) = REAL (FXX(1:NRPRM,2,JB))
         QI2(1,1:NRPRM) = AIMAG (FXX(1:NRPRM,2,JB))
         QR3(1,1:NRPRM) = REAL (FXX(1:NRPRM,3,JB))
         QI3(1,1:NRPRM) = AIMAG (FXX(1:NRPRM,3,JB))
         CALL CUBSPL (RHOTRP,QR2,NRPRM,0,0)
         CALL CUBSPL (RHOTRP,QI2,NRPRM,0,0)
         CALL CUBSPL (RHOTRP,QR3,NRPRM,0,0)
         CALL CUBSPL (RHOTRP,QI3,NRPRM,0,0)
       END IF

       STRYK_STEP: DO JA = 1, NAL
         JAB = JA + (JB-1)*NAL
         EX = (0.,0.)
         EY = (0.,0.)
         QQ = (0.,0.)
         RHO = SQRT ( (XCELL(JAB,JP) - SX(JS) )**2 + (YCELL(JAB,JP) - SY(JS) )**2)

!  Compute components in system with x along flight path centred at
!  transmitter position.

         IF (RHO > 0.) THEN
           XBAR0 = (XCELL(JAB,JP) - SX(JS)) / RHO
           YBAR0 = (YCELL(JAB,JP) - SY(JS)) / RHO
           XBAR = XBAR0 * CSF + YBAR0 * SNF
           YBAR = YBAR0 * CSF - XBAR0 * SNF
           XB2 = XBAR**2
           YR = CUBVAL (RHOTRP,QR1,NRPRM,RHO)
           YI = CUBVAL (RHOTRP,QI1,NRPRM,RHO)
           QQ(1) = CMPLX (YR, YI)
           EX =  CSTX * QQ(1) * YBAR
           EY = -CSTX * QQ(1) * XBAR
           IF (NINTG == 3) THEN
             YR2 = CUBVAL (RHOTRP,QR2,NRPRM,RHO)
             YI2 = CUBVAL (RHOTRP,QI2,NRPRM,RHO)
             YR3 = CUBVAL (RHOTRP,QR3,NRPRM,RHO)
             YI3 = CUBVAL (RHOTRP,QI3,NRPRM,RHO)
             QQ(2) = CMPLX (YR2, YI2)
             QQ(3) = CMPLX (YR3, YI3)
             EX = EX + SNTX * XBAR * YBAR * (2.*QQ(3) - QQ(2))
             EY = EY + SNTX * ((1. - 2.*XB2) * QQ(3) + XB2 * QQ(2))
           END IF
           IF (TXA90) THEN
              QQ(1) = EX     ! Kluge to allow horizontal in-line dipole Tx results
              QQ(2) = EY     ! to be used for horizontal transverse dipole Tx
              EX = -QQ(2)
              EY =  QQ(1)
           END IF

!  Resolve the field components parallel and perpendicular to strike
!  Apportion X component along the strike direction
!  Return for cases where energy is attenuated due to depth and reset the
!  value of NB for all higher frequencies.

           CSTRL = COS (PLAZM(JP) - FANGLE(JS))
           SSTRL = SIN (PLAZM(JP) - FANGLE(JS))
           E_PRYM(1,JAB,JS,JP) = EFAC * (EX*CSTRL + EY*SSTRL)
           E_PRYM(2,JAB,JS,JP) = EFAC * (EY*CSTRL - EX*SSTRL) * COS (PLDIP(JP))
         END IF
       END DO STRYK_STEP
     END DO DEPTH_STEP
   END DO STN_LOOP
 END DO PLATE_LOOP

 DEALLOCATE (FXX,KER)

END SUBROUTINE PRM_BOSS

 SUBROUTINE PRMHNK (NRPRM,RHOTRP,NPLT,JP,MXB,NBL,ZCELL,ALT,NLYR,THK,RMU,KSQ_LYR, &
                    NINTG,KER,FXX)
!----------------------------------------------------------------------------------

!***  Called by: PRM_BOSS
!***      Calls: PRMVAL

!  Sets up values of F-potential for VMD and HMD sources as a function of
!  horizontal distance at discrete depths.  Using the flow through Hankel
!  transform method, it evaluates the Hankel transform integral using a 15 point
!  per decade filter coefficient set derived from Christensen's FLTGEN program.
!  See PRM_BOSS for variable definitions not appearing below.

!       NRPRM - number of horizontal interpolation points
!      RHOTRP - 15 points per decade interpolation array
!        NPLT - number of plates
!        JP - plate index
!         MXB - maximum number of cells down dip
!         NBL - number of cells down dip for plate JP
!  ZCELL(i,*) - depth of cell centre in row i relative to surface.
!        NLYR - number of layers including basement.
!         THK - layer thicknesses
!     KSQ_LYR - iwu * SIG for all layers
!       NINTG - number of integrals needed.
!         FXX - complex F potential array

 USE LA_Filter_coefficients

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 COMPLEX, PARAMETER :: ZERO=(0.,0.)
 INTEGER L,LMAX,LMIN,K,KMAX,KMIN,KBOT,K1,JR,JP,MXB,NBL,NPLT,NLYR,JB,NRPRM,NINTG
 REAL ALT,THK(NLYR),LMBDA,ZCELL(MXB,NPLT),RHOTRP(NRPRM),RMU(NLYR)
 COMPLEX KSQ_LYR(NLYR),FXX(NRPRM,3,MXB),KER(JNLO-NRPRM:JNHI,2,MXB)
 REAL(KIND=QL) DELTA,Y1,Y,RD
 LOGICAL JUMP

 INTENT (IN) JP,MXB,NBL,ALT,NLYR,NPLT,RMU,KSQ_LYR,ZCELL,THK,RHOTRP,NRPRM,NINTG
 INTENT (OUT) FXX

 KER = ZERO
 FXX = ZERO

 DELTA = LOG (10.D0)/ DBLE (NDEC_JN)

! Set up KER for JR = 1 corresponding to minimum value of RHO.  This will
! compute most of the needed kernel range from the high end.  Note that
! the filter is defined between JNLO < L < JNHI

 JR = 1
 RD = DBLE (RHOTRP(1) )
 Y1 = -LOG (RD) - DBLE (JR-1) * DELTA - DBLE (SHFTJN)

 DO L = -50, JNHI             ! Start at L = -50 to pick up low values.
   LMAX = L                   ! Maximum filter index used
   K = L + 1 - JR             ! K is the kernel index.
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (REAL(Y))
   CALL PRMVAL (NRPRM,K,JR,L,LMBDA,NPLT,JP,MXB,NBL,ZCELL,ALT,NLYR,THK, &
                RMU,KSQ_LYR,NINTG,KER,FXX,JUMP)
     IF (JUMP) EXIT
   END DO

 Y = Y1                       ! Finish off the low end for RHOTRP(1)
 DO L = -51, JNLO, -1
   LMIN = L                   ! Minimum filter index used
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (REAL(Y))
   K = L + 1 - JR             ! Compute the kernel index.
   CALL PRMVAL (NRPRM,K,JR,L,LMBDA,NPLT,JP,MXB,NBL,ZCELL,ALT,NLYR,THK, &
                RMU,KSQ_LYR,NINTG,KER,FXX,JUMP)
   IF (JUMP) EXIT
 END DO

 KMIN = LMIN + 1 - JR  !  Define the range of kernel values
 KMAX = LMAX + 1 - JR  !  used for RHOTRP(1)

! Complete definition of kernel values by evaluating transform of
! maximum RHO = RHOTRP (NRPRM)

 JR = NRPRM
 Y1 = -LOG (RD) - DBLE (JR-1) * DELTA - DBLE (SHFTJN)
 KBOT = JNLO + 1 - JR
 K1 = MAX (KBOT,KMIN)

 DO K = K1, KMAX          ! Compute EHR for maximum RHO using previously
   L = K - 1 + JR         ! computed kernel values.

   IF (L > JNHI .OR. L < JNLO) CYCLE
   FXX(JR,1,1:NBL) =  FXX(JR,1,1:NBL) + KER(K,1,1:NBL) * WJ1(L)
   FXX(JR,2,1:NBL) =  FXX(JR,2,1:NBL) + KER(K,1,1:NBL) * WJ0(L)
   FXX(JR,3,1:NBL) =  FXX(JR,3,1:NBL) + KER(K,2,1:NBL) * WJ1(L)
 END DO

 IF (K1 > KBOT) THEN    !  Add low end kernel values until convergence.
   DO K = K1-1, KBOT, -1
     KMIN = K
     L = K - 1 + JR
     Y = Y1 + DBLE (L) * DELTA
     LMBDA = EXP (REAL(Y))
     CALL PRMVAL (NRPRM,K,JR,L,LMBDA,NPLT,JP,MXB,NBL,ZCELL,ALT,NLYR,THK, &
                  RMU,KSQ_LYR,NINTG,KER,FXX,JUMP)
     IF (JUMP) EXIT
   END DO
 END IF

 DO JR = 2, NRPRM-1          !  Compute transforms for all other RHO values
   DO K = KMIN, KMAX         !  using previously computed kernel values.
     L = K - 1 + JR
     IF (L > JNHI .OR. L < JNLO) CYCLE
     FXX(JR,1,1:NBL) =  FXX(JR,1,1:NBL) + KER(K,1,1:NBL) * WJ1(L)
     FXX(JR,2,1:NBL) =  FXX(JR,2,1:NBL) + KER(K,1,1:NBL) * WJ0(L)
     FXX(JR,3,1:NBL) =  FXX(JR,3,1:NBL) + KER(K,2,1:NBL) * WJ1(L)
   END DO
 END DO

 DO JB = 1,NBL
   DO L = 1,NINTG
     FXX(1:NRPRM,L,JB) = FXX(1:NRPRM,L,JB) / RHOTRP(1:NRPRM)
   END DO
   IF (NINTG == 3) FXX(1:NRPRM,3,JB) = FXX(1:NRPRM,3,JB) / RHOTRP(1:NRPRM)
 END DO

END SUBROUTINE PRMHNK

 SUBROUTINE PRMVAL (NRPRM,K,JR,L,LMBDA,NPLT,JP,MXB,NBL,ZCELL,ALT,NLYR,THK, &
                    RMU,KSQ_LYR,NINTG,KER,FXX,JUMP)
!----------------------------------------------------------------------------

!***  Called by: PRMHNK

!  Accumulates the integrals at each depth point for array FXX

!     NRPRM - number of points in 15 point / decade array
!         K - filter kernel index
!        JR - RHO interpolation index
!         L - filter point index
!     LMBDA - Hankel transform variable
!        JP - plate index
!        NPLT - number of plates
!         MXB - maximum number of depths
!         NBL - locally fixed number of depths
!        NLYR - number of layers
!     KSQ_LYR - layer propagation constants
!         THK - layer thicknesses
!       ZCELL - depth to cell centres
!         FXX - accumulated F potential array
!        JUMP - keep integrating if false.

 USE LA_Filter_coefficients
 IMPLICIT NONE
 REAL, PARAMETER :: TOL=1.E-5
 COMPLEX, PARAMETER :: ONE=(1.,0.), ZERO=(0.,0.)
 INTEGER J,JR,L,K,JP,MXB,NBL,NPLT,NLYR,JZ,NRPRM,NINTG,JINT
 REAL AR,AI,FXXR,FXXI,ZCR,THK(NLYR),THICK,LMBDA,ZCELL(MXB,NPLT),ALT,RMU(NLYR),ALF
 COMPLEX, DIMENSION(NLYR) :: KSQ_LYR,X
 COMPLEX, DIMENSION(0:NLYR) :: T,B,TP1,S
 COMPLEX KER(JNLO-NRPRM:JNHI,2,MXB),FXX(NRPRM,3,MXB),LMBSQ,THKS,XSUM, &
         PRJ,DENOM,AJ1,ETA,TXP,TMP(3,MXB)
 LOGICAL TOO_BIG,JUMP

 INTENT (IN) L,JR,JP,MXB,NBL,NLYR,NPLT,LMBDA,RMU,KSQ_LYR,ZCELL,THK,ALT,NINTG
 INTENT (INOUT) FXX

! Compute the kernels for the J0 integration.  Eliminate underflows
! due to exponentials.

 X = ONE ;  T = ZERO ;  TP1 = ZERO
 LMBSQ = CMPLX (LMBDA * LMBDA, 0.)

 THICK = SUM (THK(1:NLYR-1) )
 TMP = ZERO
 S(0) = CMPLX (LMBDA, 0.)
 S(1) = SQRT (LMBSQ + KSQ_LYR(1))
 S(NLYR) = SQRT (LMBSQ + KSQ_LYR(NLYR))
 T(0) = ((RMU(1)**2 - 1.) * LMBSQ - KSQ_LYR(1)) / (RMU(1) * S(0) + S(1))**2
 TP1(0) = 2.* RMU(1)* S(0) / (RMU(1) * S(0) + S(1))
 XSUM = S(0) * ALT

 DO J = NLYR-1,1,-1
   S(J) = SQRT (LMBSQ + KSQ_LYR(J))
   ALF = RMU(J+1)/RMU(J)
   DENOM = ALF * S(J) + S(J+1)
   T(J) = ((ALF**2 - 1.) * S(J)**2 + KSQ_LYR(J) - KSQ_LYR(J+1)) / DENOM**2
   TP1(J) =  2. * ALF * S(J) / DENOM
   THKS = S(J) * THK(J)
   X(J) = EXP (-2.* THKS)
   XSUM = XSUM + THKS
 END DO

 PRJ = TP1(NLYR-1)
 IF (NLYR > 1) THEN
   B(NLYR-1) = T(NLYR-1)
   DO J = NLYR-2,0,-1
     AJ1 = B(J+1) * X(J+1)
     DENOM = ONE + AJ1 * T(J)
     B(J) = (T(J) + AJ1) / DENOM
     PRJ = PRJ * TP1(J) / DENOM
   END DO
 END IF
 ETA = PRJ

 DO JZ = 1, NBL
   ZCR = ZCELL(JZ,JP) - THICK
   TXP = S(NLYR) * ZCR + XSUM
   TXP = EXP (-TXP)
   KER(K,2,JZ) = ETA * TXP
   KER(K,1,JZ) = KER(K,2,JZ) * LMBDA
 END DO

 JUMP = .TRUE.
 IF (NBL >= 1) THEN
   TMP(1,1:NBL) = KER(K,1,1:NBL) * WJ1(L)
   TMP(2,1:NBL) = KER(K,1,1:NBL) * WJ0(L)
   TMP(3,1:NBL) = KER(K,2,1:NBL) * WJ1(L)

   FXX(JR,1:3,1:NBL) = FXX(JR,1:3,1:NBL) + TMP(1:3,1:NBL)
   DO JZ = 1, NBL
     DO JINT = 1,NINTG
       AR = ABS ( REAL (TMP(JINT,JZ)) )
       AI = ABS (AIMAG (TMP(JINT,JZ)) )
       TOO_BIG = .FALSE.
       FXXR = ABS ( REAL (FXX(JR,JINT,JZ)) )
       FXXI = ABS (AIMAG (FXX(JR,JINT,JZ)) )
       IF (AR > TOL* FXXR) TOO_BIG = .TRUE.
       IF (AI > TOL* FXXI) TOO_BIG = .TRUE.
       IF (TOO_BIG) JUMP = .FALSE.
     END DO
   END DO
 END IF
END SUBROUTINE PRMVAL

 SUBROUTINE SCAT_MTRX_BOSS (NPLT,NLYR,MXAB,MXB,NCELL2,MXCL2,NA,NB,DA,DB,KSQ_SHT,SIGT,  &
                            SIGL,RMU,KSQ_LYR,THK,PLTOP,PLDIP,PLAZM,XCELL,YCELL,ZCELL, &
                            NREGT,RHOTRP,NSTAT,E_PRYM,J_SCAT,DCMP_FAIL)
!-------------------------------------------------------------------------------------

!***  Called by: LEROI_3D
!***      Calls: EGT_BOSS, INTER_EGT_BOSS, SCAT_MTRX_LU_DCMP, SCAT_CRNT

!  Sets up the global multi-plate system matrix SCAT_MTRX from the Green's
!  tensor elements.  Using Weidelt's formalism to construct the system matrix,
!  SCAT_MTRX_LU_DCMP is called to replace SCAT_MTRX by its LU decomposition.
!  Finally, SCAT_CRNT is called to yield the scattering currents on each plate.

!  In what follows, the GTE have the units of electric field divided by a
!  factor (-iwu) because this factor is explicitly included as a multiplier
!  outside the integral over area when solving the integral equation.
!
!  These modified integrated Green's functions are initially computed in the form:
!
!  GAA = SAA + HAA /KSQ_BAS;  GAB = SAB + HAB /KSQ_BAS;
!                             GBA = SAB + HBA /KSQ_BAS  &
!  GBB = SBB + HBB /KSQ_BAS   where  KSQ_BAS :=  i * omega * mu / RES(NLYR).
!
!  SAA, SAB=SBA, & SBB are the induced (divergence free) part of the GTE,
!  corresponding to currents enclosed within the plate.
!  HAA, HAB, HBA, & HBB were originally the curl-free or current channelling
!  part of the GTE.
!
!  SAA(KS,KR,IR) is the electric field in the XI (strike) direction, integrated
!  over receiver cell (IR,KR) due to a XI-oriented electric dipole at cell (1,KS).
!  Similarly, SAB(KS,KR,IR) is the electric field in the ETA direction caused
!  by the same dipole.  SBB is the ETA electric field caused by an
!  ETA-oriented electric dipole.  Similarly for HAA, HAB, HBA, & HBB.
!
!  The solution vector is expressed as the curl and gradient of two
!  potential functions in the form CURL (*C* PSI) + KSQ_BAS * GRAD (PHI) where
!  *C* is a unit vector perpendicular to the plate surface.  The actual form of
!  the solution vector consists of the values of the PSI and PHI potentials on
!  the corners of the plate cells.  Thus in building the global matrix,
!  SCAT_MTRX_BUILD has to account for the curl and grad operations as well as
!  including the Green's tensor elements/
!
!  The big advantage of Weidelt's formulation is that the 1 / KSQ_LYR term can be
!  removed since the surface integrals of HIJ * CURL (*C* PSI) are identically
!  zero.  SUBROUTINE SCAT_MTRX_BUILD thus uses the redefined
!  HIJ - HIJ + SIJ* KSQ_BAS input from EGT_BOSS.
!
!  J_SCAT (J1,JCL,JS,JP) is the current in the direction J1 in the centre of
!                        cell JCL of plate JP at station JS.
!
!  E_PRYM (J1,JCL,JS,JP) is the primary field in the direction J1 in the centre of
!                        cell JCL of plate JP at station JS.
!
!          J1 = 1 => along strike;  = 2 => down dip
!
!
!         OUTPUT: J_SCAT
!
!         INPUT
!         -----
!
!           NPLT - number of plates in basement
!           NLYR - number of layers (1 or 2)
!  NA(J) & NB(J) - number of cells along strike and down dip for each plate
!  DA(J) & DB(J) - dimension of cells along strike and down dip for each plate
!           MXAB - maximum number of cells in a single plate
!      NCELL2(J) - 2 * the number of cells in plates 1 to J
!          MXCL2 - 2 * total number of cells in al plates = NCELL2(NPLT)
!       SIGT(J) - complex conductivity thickness product of plate J
!     KSQ_SHT(J) - iwu * SIGT
!         SIGLJ) - complex conductivity thickness product of layer J
!     KSQ_LYR(J) - iwu * SIGL
!            THK - layer thicknesses
!          PLTOP - depth to top of each plate
!          PLDIP - dip angle for each plate
!         RHOTRP - interpolation array for electric Green's functions
!          NREGT - maximum index for RHOTRP
!          NSTAT - number of transmitter locations
!         E_PRYM - primary electric field incident of each plate.
!
!         INTERNAL COMPUTATIONS
!         ---------------------
!  SAA, SBA, SBB - complex Green's kernels from EGT_BOSS for vortex currents
!                  (closed in the plate)
!  HAA, HAB, HBA - complex Green's kernels from EGT_BOSS for current gathering
!          & HBB   (currents closed outside the plate) + the SIJ * BAS
!      SCAT_MTRX - LU decomposed form of system matrix
!           INDX - pivoting vector information

 IMPLICIT NONE
 COMPLEX, PARAMETER :: ZERO=(0.,0.)
 INTEGER NSTAT,NLYR,NPLT,MXCL2,MCELL2,NA(NPLT),NB(NPLT),NCELL2(0:NPLT),MXAB,MXA, &
         MXB,NAL,NBL,IPHI(4),IPSI(4),IDX(4),INDX(MXCL2),KB,JB,KA,JA,JA_KA,&
         ISIG,L,NREGT,JP,JPS,JPR,JSX,JRX,JSXRX,KAB,KABG,JAB
 REAL AD(4),BD(4),GA,GB,DAL,DBL,THK(NLYR),PLTOPL,CDIP,SDIP,RHOTRP(NREGT), &
      XCELL(MXAB,NPLT),YCELL(MXAB,NPLT),ZCELL(MXB,NPLT),AREA,RMU(NLYR)
 REAL, DIMENSION (NPLT) :: DA,DB,PLTOP,PLDIP,PLAZM
 COMPLEX SCAT_MTRX(MXCL2,MXCL2),E_PRYM(2,MXAB,NSTAT,NPLT),J_SCAT(2,MXAB,NSTAT,NPLT), &
         AAS,ABSS,BAS,BBS,AAH,ABH,BAH,BBH,APSI(4),BPSI(4),APHI(4),BPHI(4), &
         KSQ_BAS,SIGT(NPLT),KSQ_SHT(NPLT),SIGL(NLYR),KSQ_LYR(NLYR)
 COMPLEX,DIMENSION(:,:,:),ALLOCATABLE :: SAA,SBA,SBB,HAA,HAB,HBA,HBB
 COMPLEX,DIMENSION(:,:),ALLOCATABLE :: HAAI,HABI,HBAI,HBBI,SAAI,SABI,SBAI,SBBI
 LOGICAL DCMP_FAIL

 INTENT (IN) NPLT,NLYR,MXAB,NCELL2,MXCL2,NA,NB,DA,DB,SIGT,KSQ_SHT,SIGL,RMU,KSQ_LYR, &
             THK,PLTOP,PLDIP,PLAZM,XCELL,YCELL,ZCELL,NREGT,RHOTRP,NSTAT,E_PRYM
 INTENT (OUT) J_SCAT

 KSQ_BAS = KSQ_LYR(NLYR)
 MXA = MAXVAL (NA)

 SCAT_MTRX = ZERO

!Set up the single plate - host interactions ; ie the block diagonal
!part of the scattering matrix.

 PLATE_LOOP: DO JP = 1,NPLT

   NAL = NA(JP)  ! Set up local dimensions for single plate operations
   NBL = NB(JP)
   DAL = DA(JP)
   DBL = DB(JP)
   CDIP = COS (PLDIP(JP))
   SDIP = SIN (PLDIP(JP))
   PLTOPL = PLTOP(JP)
   MCELL2 = NCELL2(JP-1)    ! Index to start each plate

! Compute electric Green's tensor integrals,

   ALLOCATE (SAA(NBL,NBL,NAL),SBA(NBL,NBL,NAL),SBB(NBL,NBL,NAL), &
             HAA(NBL,NBL,NAL),HAB(NBL,NBL,NAL),HBA(NBL,NBL,NAL), &
             HBB(NBL,NBL,NAL))

   SAA=ZERO; SBA=ZERO; SBB=ZERO; HAA=ZERO; HAB=ZERO; HBA=ZERO; HBB=ZERO;

   CALL EGT_BOSS (NLYR,NAL,NBL,DAL,DBL,SIGL,RMU,KSQ_LYR,THK,PLTOPL,CDIP,SDIP, &
                  NREGT,RHOTRP,SAA,SBA,SBB,HAA,HAB,HBA,HBB)

   GA = .5 / DAL   ! part of the CURL and GRAD
   GB = .5 / DBL   ! numerical differentiations

   AD(1) = -GA    !  This is the stencil corresponding to the identity
   AD(2) = +GA    !  matrix acting on the psi and phi values keeping in
   AD(3) = -GA    !  mind that we are inverting
   AD(4) = +GA    !                                     [PHI]
   BD(1) = -GB    !                             {I + G} [   ] = Ep
   BD(2) = -GB    !                                     [PSI}
   BD(3) = +GB    !
   BD(4) = +GB    !

! Construction of the system of equations.  JB and JA are the row and column
! indices of the "receiver" cells

   LOOP_JB_NBL: DO JB = 1,NBL                   !  each cell has local numbers
     LOOP_JA_NAL: DO JA = 1,NAL
       IDX = 1                                  !   1  2
       IF ( JB==1 .OR. JA==1 ) IDX(1) = 0       !   3  4
       IF ( JB==1 .OR. JA==NAL ) IDX(2) = 0     !         IDX is a stencil used
       IF ( JB==NBL .OR. JA==1 ) IDX(3) = 0     ! to set PSI values to zero on
       IF ( JB==NBL .OR. JA==NAL ) IDX(4) = 0   ! all edge nodes of plate

! For each cell, assign global node numbers for PHI and PSI values
! at cell corners: 1, 2, 3, & 4.  PSI values take up the first
! (NA-1) * (NB-1) solution vector places since PSI = 0 on all plate edges.
! PHI values take up the next (NA+1) * (NB + 1) - 2 places for a total
! length of 2 * NA * NB.

       IPSI(1) = MCELL2 + (JB - 2) * (NAL - 1) + JA - 1
       IPSI(2) = IPSI(1) + 1
       IPSI(3) = IPSI(1) + NAL - 1
       IPSI(4) = IPSI(3) + 1
       IPHI(1) = MCELL2 + (NAL - 1) * (NBL - 1) + (JB - 1) * (NAL + 1) + JA
       IPHI(2) = IPHI(1) + 1
       IPHI(3) = IPHI(2) + NAL
       IPHI(4) = IPHI(3) + 1

! Sum Green's tensor "source" contributions.  KB and KA are the
! row and column indices of the "source" cells

       LOOP_KA_NAL: DO KA = 1,NAL
         JA_KA = 1 + IABS(JA - KA)
         ISIG = ISIGN(1,JA - KA)
         LOOP_KB_NBL: DO KB = 1,NBL
           KAB = MCELL2   + 2* ((KB-1) *NAL + KA)

! SAB, HAB, & HBA change sign if the XI (source cell) > XI (receiver cell);
! ie, EGT_BOSS assumes the source to the left of the receiver.

           AAS = SAA(KB,JB,JA_KA)
           BAS = SBA(KB,JB,JA_KA) * ISIG
           BBS = SBB(KB,JB,JA_KA)
           AAH = HAA(KB,JB,JA_KA)
           ABH = HAB(KB,JB,JA_KA) * ISIG
           BAH = HBA(KB,JB,JA_KA) * ISIG
           BBH = HBB(KB,JB,JA_KA)

           APSI(1) = KSQ_SHT(JP) * (- GB*AAS + GA*BAS)   ! cell corner PSI contribution
           APSI(2) = KSQ_SHT(JP) * (- GB*AAS - GA*BAS)   ! to along strike field
           APSI(3) = -APSI(2)
           APSI(4) = -APSI(1)
           BPSI(1) = KSQ_SHT(JP) * (-GB*BAS + GA*BBS)    ! cell corner PSI contribution
           BPSI(2) = KSQ_SHT(JP) * (-GB*BAS - GA*BBS)    ! to down dip field
           BPSI(3) = -BPSI(2)
           BPSI(4) = -BPSI(1)
           APHI(1) = KSQ_SHT(JP) * (-GA*AAH - GB*ABH)    ! cell corner PHI contribution
           APHI(2) = KSQ_SHT(JP) * (+GA*AAH - GB*ABH)    ! to along strike field
           APHI(3) = -APHI(2)
           APHI(4) = -APHI(1)
           BPHI(1) = KSQ_SHT(JP) * (-GA*BAH - GB*BBH)    ! cell corner PHI contribution
           BPHI(2) = KSQ_SHT(JP) * (+GA*BAH - GB*BBH)    ! to down dip field
           BPHI(3) = -BPHI(2)
           BPHI(4) = -BPHI(1)
           DO L = 1,4
             IF (IPHI(L) <= NCELL2(JP) ) THEN
               SCAT_MTRX(KAB-1, IPHI(L)) = SCAT_MTRX(KAB-1, IPHI(L)) + APHI(L)
               SCAT_MTRX(KAB,   IPHI(L)) = SCAT_MTRX(KAB,   IPHI(L)) + BPHI(L)
             END IF
             IF (IDX(L) /= 0) THEN
               SCAT_MTRX(KAB-1, IPSI(L)) = SCAT_MTRX(KAB-1, IPSI(L)) + APSI(L)
               SCAT_MTRX(KAB,   IPSI(L)) = SCAT_MTRX(KAB,   IPSI(L)) + BPSI(L)
             END IF
           END DO
           IF ((JB == KB) .AND. (JA == KA) ) THEN   ! add in identity matrix
             DO L = 1,4
               IF (IPHI(L) <= NCELL2(JP) ) THEN
                 SCAT_MTRX(KAB-1,IPHI(L)) = SCAT_MTRX(KAB-1,IPHI(L)) + CMPLX (AD(L), 0.) * KSQ_BAS
                 SCAT_MTRX(KAB,  IPHI(L)) = SCAT_MTRX(KAB,  IPHI(L)) + CMPLX (BD(L), 0.) * KSQ_BAS
               END IF
               IF ( IDX(L) /= 0 ) THEN
                 SCAT_MTRX(KAB-1,IPSI(L)) = SCAT_MTRX(KAB-1,IPSI(L)) + CMPLX (BD(L), 0.)
                 SCAT_MTRX(KAB,  IPSI(L)) = SCAT_MTRX(KAB,  IPSI(L)) - CMPLX (AD(L), 0.)
               END IF
             END DO
           END IF
         END DO LOOP_KB_NBL
       END DO LOOP_KA_NAL
     END DO LOOP_JA_NAL
   END DO LOOP_JB_NBL
   DEALLOCATE (SAA,SBA,SBB,HAA,HAB,HBA,HBB)

 END DO PLATE_LOOP

! Set up the plate-plate interactions; ie. the off diagonal part of the
! scattering matrix.

 MULTI_SHT: IF (NPLT > 1) THEN
   ALLOCATE (HAAI(MXAB,MXAB),HABI(MXAB,MXAB),HBAI(MXAB,MXAB),HBBI(MXAB,MXAB), &
             SAAI(MXAB,MXAB),SABI(MXAB,MXAB),SBAI(MXAB,MXAB),SBBI(MXAB,MXAB))

   HAAI=ZERO; HABI=ZERO; HBAI=ZERO; HBBI=ZERO; SAAI=ZERO; SABI=ZERO; SBAI=ZERO; SBBI=ZERO

   TX_SHT: DO JPS = 1, NPLT-1
     RX_SHT: DO JPR = JPS+1, NPLT

       CALL INTER_EGT_BOSS (JPS,JPR,MXAB,MXB,NPLT,NLYR,NA,NB,THK,RMU,KSQ_LYR, &
                            SIGL,PLDIP,PLAZM,XCELL,YCELL,ZCELL,NREGT,RHOTRP,    &
                            HAAI,HABI,HBAI,HBBI,SAAI,SABI,SBAI,SBBI)

! Set up matrix for dipoles on plate JPS producing fields on plate JPR.
! Then use symmetry to set the adjoint block.

       JSXRX_L: DO JSXRX = 1,2
         IF (JSXRX ==1) THEN
           JRX = JPR
           JSX = JPS
         ELSE
           JRX = JPS
           JSX = JPR
         END IF
         MCELL2 = NCELL2(JRX-1)
         NBL = NB(JRX)
         NAL = NA(JRX)
         GA = .5 / DA(JRX)
         GB = .5 / DB(JRX)
         AREA = DA(JRX) * DB(JRX)

         JB_L: DO JB = 1,NBL      ! Step over rows and columns of receiver plate
           JA_L: DO JA = 1,NAL
             IDX = 1                                      !   1  2
             IF ( JB==1 .OR. JA==1 )     IDX(1) = 0       !   3  4
             IF ( JB==1 .OR. JA==NAL )   IDX(2) = 0       !         IDX is a stencil used
             IF ( JB==NBL .OR. JA==1 )   IDX(3) = 0       ! to set PSI values to zero on
             IF ( JB==NBL .OR. JA==NAL ) IDX(4) = 0       ! all edge nodes of plate

             IPSI(1) = MCELL2 + (JB - 2) * (NAL - 1) + JA - 1
             IPSI(2) = IPSI(1) + 1
             IPSI(3) = IPSI(1) + NAL - 1
             IPSI(4) = IPSI(3) + 1
             IPHI(1) = MCELL2 + (NAL - 1) * (NBL - 1) + (JB - 1) * (NAL + 1) + JA
             IPHI(2) = IPHI(1) + 1
             IPHI(3) = IPHI(2) + NAL
             IPHI(4) = IPHI(3) + 1

! Sum Green's tensor "source" contributions.  KB and KA are the
! row and column indices of the "source" cells

             JAB = JA + (JB-1) * NAL          !  "receiver" cell index
             KAB_L: DO KAB = 1, NA(JSX) * NB(JSX)    !  "source" cell index
               KABG = NCELL2(JSX-1) + 2* KAB
               IF (JSXRX == 1) THEN
                 AAS = SAAI(KAB,JAB) * AREA
                 ABSS = SABI(KAB,JAB) * AREA
                 BAS = SBAI(KAB,JAB) * AREA
                 BBS = SBBI(KAB,JAB) * AREA
                 AAH = HAAI(KAB,JAB) * AREA
                 ABH = HABI(KAB,JAB) * AREA
                 BAH = HBAI(KAB,JAB) * AREA
                 BBH = HBBI(KAB,JAB) * AREA
               ELSE
                 AAS = SAAI(JAB,KAB) * AREA
                 ABSS = SBAI(JAB,KAB) * AREA
                 BAS = SABI(JAB,KAB) * AREA
                 BBS = SBBI(JAB,KAB) * AREA
                 AAH = HAAI(JAB,KAB) * AREA
                 ABH = HBAI(JAB,KAB) * AREA
                 BAH = HABI(JAB,KAB) * AREA
                 BBH = HBBI(JAB,KAB) * AREA
               END IF

               APSI(1) = KSQ_SHT(JRX) * (- GB*AAS + GA*ABSS)   ! cell corner PSI contribution
               APSI(2) = KSQ_SHT(JRX) * (- GB*AAS - GA*ABSS)   ! to along strike field
               APSI(3) = -APSI(2)
               APSI(4) = -APSI(1)
               BPSI(1) = KSQ_SHT(JRX) * (-GB*BAS + GA*BBS)    ! cell corner PSI contribution
               BPSI(2) = KSQ_SHT(JRX) * (-GB*BAS - GA*BBS)    ! to down dip field
               BPSI(3) = -BPSI(2)
               BPSI(4) = -BPSI(1)
               APHI(1) = KSQ_SHT(JRX) * (-GA*AAH - GB*ABH)    ! cell corner PHI contribution
               APHI(2) = KSQ_SHT(JRX) * (+GA*AAH - GB*ABH)    ! to along strike field
               APHI(3) = -APHI(2)
               APHI(4) = -APHI(1)
               BPHI(1) = KSQ_SHT(JRX) * (-GA*BAH - GB*BBH)    ! cell corner PHI contribution
               BPHI(2) = KSQ_SHT(JRX) * (+GA*BAH - GB*BBH)    ! to down dip field
               BPHI(3) = -BPHI(2)
               BPHI(4) = -BPHI(1)

               DO L = 1,4
                 IF (IPHI(L) <= NCELL2(JRX) ) THEN
                   SCAT_MTRX(KABG-1, IPHI(L)) = SCAT_MTRX(KABG-1, IPHI(L)) + APHI(L)
                   SCAT_MTRX(KABG,   IPHI(L)) = SCAT_MTRX(KABG,   IPHI(L)) + BPHI(L)
                 END IF
                 IF (IDX(L) /= 0) THEN
                   SCAT_MTRX(KABG-1, IPSI(L)) = SCAT_MTRX(KABG-1, IPSI(L)) + APSI(L)
                   SCAT_MTRX(KABG,   IPSI(L)) = SCAT_MTRX(KABG,   IPSI(L)) + BPSI(L)
                 END IF
               END DO

             END DO KAB_L
           END DO JA_L
         END DO JB_L
       END DO JSXRX_L
     END DO RX_SHT
   END DO TX_SHT
   DEALLOCATE (HAAI,HABI,HBAI,HBBI,SAAI,SABI,SBAI,SBBI)
 END IF MULTI_SHT

! Do LU decomposition and then compute scattering currents, J_SCAT
! for all transmitter positions

 CALL SCAT_MTRX_LU_DCMP (MXCL2,SCAT_MTRX,INDX,DCMP_FAIL)

 CALL SCAT_CRNT (NPLT,NSTAT,MXAB,NCELL2,MXCL2,MXA,MXB,NA,NB,DA,DB, &
                 KSQ_BAS,SIGT,INDX,SCAT_MTRX,E_PRYM,J_SCAT)

END SUBROUTINE SCAT_MTRX_BOSS

 SUBROUTINE SCAT_MTRX_LU_DCMP (MXCL2,SCAT_MTRX,INDX,DCMP_FAIL)
!-------------------------------------------------------------

!***  Called by: SCAT_MTRX_BOSS

!  LU decomposition of a complex matrix SCAT_MTRX.  On exit, SCAT_MTRX is
!  overwritten by its LU decomposition,  The 1's on the diagonal are assumed by
!  the solution subroutine SCAT_CRNT.  MXCL2 is the dimension of the
!  matrix, SCAT_MTRX.  SCAT_MTRX_LU_DCMP uses partial pivoting to
!  improve the conditioning.  The pivoting information is stored in INDX.

 IMPLICIT NONE
 INTEGER MXCL2,INDX(MXCL2),ILL,I,J,IDXPIV,KP,K
 REAL SCALE(MXCL2),ROWNRM,BIG,SIZE
 COMPLEX SCAT_MTRX(MXCL2,MXCL2),EM
 LOGICAL DCMP_FAIL

 INTENT (IN) MXCL2
 INTENT (OUT) INDX
 INTENT (INOUT) SCAT_MTRX

! Initialisation of INDX and SCALE

 DCMP_FAIL = .FALSE.
 ILL = 0
 DO I = 1,MXCL2
   INDX(I) = I
   ROWNRM = 0.
   DO J = 1,MXCL2
     IF (ROWNRM < ABS (SCAT_MTRX(I,J)) ) ROWNRM = ABS (SCAT_MTRX(I,J))
   END DO
   IF (ROWNRM > 0.) THEN
     SCALE(I) = 1. / ROWNRM
   ELSE
     ILL = 1
     SCALE(I) = 0.
   END IF
 END DO

! Gauss elimination with partial pivoting


 LOOP_K_MXCL2: DO K = 1, MXCL2 - 1
   BIG = 0.
   DO I = K,MXCL2
     SIZE = ABS (SCAT_MTRX(INDX(I),K)) * SCALE(INDX(I))
     IF (SIZE > BIG) THEN
       BIG = SIZE
       IDXPIV = I
     END IF
   END DO
   IF (ABS (BIG) > 0.) THEN
     IF (IDXPIV /= K) THEN
       J = INDX(K)
       INDX(K) = INDX(IDXPIV)
       INDX(IDXPIV) = J
     END IF
     KP = INDX(K)
     DO I = K + 1,MXCL2
       EM = -SCAT_MTRX(INDX(I),K) / SCAT_MTRX(KP,K)
       SCAT_MTRX(INDX(I),K) = -EM
       DO J = K + 1,MXCL2
         SCAT_MTRX(INDX(I),J) = SCAT_MTRX(INDX(I),J) + EM*SCAT_MTRX(KP,J)
       END DO
     END DO
   ELSE
     ILL = 2
   END IF
 END DO LOOP_K_MXCL2

 IF (ABS ( SCAT_MTRX(INDX(MXCL2),MXCL2) ) < 1.E-34) ILL = 2
 IF (ILL /= 0) DCMP_FAIL = .TRUE.

END SUBROUTINE SCAT_MTRX_LU_DCMP

 SUBROUTINE SCAT_CRNT (NPLT,NSTAT,MXAB,NCELL2,MXCL2,MXA,MXB,NA,NB,DA,DB, &
                       KSQ_BAS,SIGT,INDX,SCAT_MTRX,E_PRYM,J_SCAT)
!------------------------------------------------------------------------

!***  Called by: SCAT_MTRX_BOSS
!
!  Computes the scattering currents, J_SCAT for all transmitter positions,
!  by applying the LU decomposed matrix SCAT_MTRX to the incident layered
!  earth primary electric fields, E_PRYM.

!  For E := either E_PRYM or J_SCAT -

!  E (1,JCELL,JS,JP) is the field (current) in the strike direction in the
!                       centre of plate cell number JCELL of plate JP due
!                       to transmitter JS
!  E (2,JCELL,JS,JP) is the field (current) in the down-dip direction in the
!                       centre of plate cell number JCELL of plate JP due
!                       to transmitter JS
!
!        NPLT - number of plates
!       NSTAT - number of transmitter positions on profile line.
!         MXA - number of cells along strike of longest plate
!         MXB - maximum number of cells down dip of widest plate
!        MXAB - the number of cells in the biggest plate (not necessarily MXA * MXB)
!   NCELL2(J) - 2 * the number of cells in plates 1 to J
!       MXCL2 - 2 * total number of cells for all plates
!     NA & NB - number of cells along strike and down dip for plate
!     DA & DB - cell dimensions along strike and down dip respectively
!    SIGT(J) - complex conductivity thickness product of plate J
!  KSQ_BAS(J) - iwu / RES(NLYR)
!        INDX - pivoting vector information from SCAT_MTRX_LU_DCMP

 IMPLICIT NONE
 COMPLEX, PARAMETER :: ZERO=(0.,0.)
 INTEGER JP,NPLT,JS,NSTAT,JA,JB,JAB,J,J1,J2,MXAB,MXA,MXB,NAL,NA(NPLT), &
         NBL,NB(NPLT),IBACK,I,MXCL2,INDX(MXCL2),NCELL2(0:NPLT),MCELL2
 REAL GA,GB,DA(NPLT),DB(NPLT)
 COMPLEX SCAT_MTRX(MXCL2,MXCL2),E_PRYM(2,MXAB,NSTAT,NPLT),J_SCAT(2,MXAB,NSTAT,NPLT), &
         X(MXCL2),B(MXCL2),PHI(MXB+1,MXA+1),PSI(MXB+1,MXA+1),KSQ_BAS, &
         SIGT(NPLT),ES_TMP,SUM

 INTENT (IN) NPLT,NSTAT,MXAB,NCELL2,MXCL2,MXA,MXB,NA,NB,DA,DB,KSQ_BAS, &
             SIGT,INDX,SCAT_MTRX,E_PRYM
 INTENT (OUT) J_SCAT

 TX_LOOP: DO JS = 1,NSTAT
   DO JP = 1,NPLT
     MCELL2 = NCELL2(JP-1)
     DO JAB = 1, NA(JP) * NB(JP)
       B(MCELL2 + 2*JAB -1) = E_PRYM(1,JAB,JS,JP)
       B(MCELL2 + 2*JAB) =    E_PRYM(2,JAB,JS,JP)
     END DO
   END DO

   X(1) = B(INDX(1))
   DO J1 = 2,MXCL2
     SUM = ZERO
     DO J2 = 1, J1-1
       SUM = SUM + SCAT_MTRX(INDX(J1),J2)*X(J2)
     END DO
     X(J1) = B(INDX(J1)) - SUM
   END DO

   X(MXCL2) = X(MXCL2)/SCAT_MTRX(INDX(MXCL2),MXCL2)
   DO IBACK = 2,MXCL2
     I = MXCL2 + 1 - IBACK
     SUM = ZERO
     DO J = I + 1,MXCL2
       SUM = SUM + SCAT_MTRX(INDX(I),J)*X(J)
     END DO
     X(I) = (X(I) - SUM)/SCAT_MTRX(INDX(I),I)
   END DO

   PLATE_LOOP: DO JP = 1,NPLT
     GA = .5 / DA(JP)
     GB = .5 / DB(JP)
     NAL = NA(JP)
     NBL = NB(JP)
     MCELL2 = NCELL2(JP-1)

     PSI = ZERO
     PHI = ZERO

     DO JB = 2,NBL
       DO JA = 2,NAL
         JAB = (JB-2)* (NAL-1) + JA - 1
         PSI(JB,JA) = X(MCELL2 + JAB)
       END DO
     END DO

     DO JB = 1, NBL+1
       DO JA = 1, NAL+1
         JAB = (JB-1) * (NAL+1) + JA + (NAL-1) * (NBL-1)
         IF (JAB <= (2*NAL*NBL) ) PHI(JB,JA) = X(MCELL2 + JAB)
       END DO
     END DO

     PHI(NBL+1,NAL) = ZERO
     PHI(NBL+1,NAL + 1) = ZERO
     DO JB = 1,NBL
       DO JA = 1,NAL
         JAB = (JB - 1)*NAL + JA
         ES_TMP = KSQ_BAS* (PHI(JB,JA+1) + PHI(JB+1,JA+1) - PHI(JB,JA) - PHI(JB+1,JA)) *GA &
                         + (PSI(JB+1,JA) + PSI(JB+1,JA+1) - PSI(JB,JA) - PSI(JB,JA+1)) *GB

         J_SCAT(1,JAB,JS,JP) = SIGT(JP) * ES_TMP

         ES_TMP = KSQ_BAS* (PHI(JB+1,JA) + PHI(JB+1,JA+1) - PHI(JB,JA) - PHI(JB,JA+1)) *GB &
                         - (PSI(JB,JA+1) + PSI(JB+1,JA+1) - PSI(JB,JA) - PSI(JB+1,JA)) *GA
         J_SCAT(2,JAB,JS,JP) = SIGT(JP) * ES_TMP
       END DO
     END DO
   END DO PLATE_LOOP

 END DO TX_LOOP

END SUBROUTINE SCAT_CRNT

 SUBROUTINE SCAT_MAG (JQ,JF,NFRQ,NPLT,MXAB,NA,NB,PLDIP,PLAZM,NLYR,RMU,KSQ_LYR,THK,  &
                      NSTAT,SAME_TX,NRX,XRM,YRM,RZ,NRMGT,RHOTRP,NCPTS,MXGS,XGS,YGS, &
                      ZGS,WXZY,J_SCAT,BFD_SCAT)
!---------------------------------------------------------------------------------

!***  Called by: LEROI_3D
!***      Calls: MGT_BOSS
!
!  Computes BFD_SCAT(JF,JS,JC), scattered frequency-domain magnetic field in nT
!  for frequency, JF, station JS, component JC (1,2,3 => (north, east, vertical)
!  J_SCAT are the scattered currents.
!
!  NFRQ, NPLT, NLYR, NSTAT, NRX: number of frequencies, plates, layers, stations
!                                and receiver offsets respectively
!           JQ - Tx-Rx offset index
!           JF - frequency index
!         MXAB - Number of cells in biggest plate
!        NB,NA - number of cells down dip and along strike for each plate.
! PLAZM, PLDIP - dip and strike angle for each plate
!          THK - layer thicknesses
! RMU, KSQ_LYR - relative permeability and propagation constant for each layer
!      SAME_TX - used to reduce redundant computations
!     XRM, YRM - transformed  receiver positions wrt plate centre for plate JP
!       RHOTRP - horizontal interpolation array for MGT
!        NRMGT - number of points in RHOTRP
!          XGS - location of integration points along strike
!     YGS, ZGS - coordinates for integration down dip
!         MXGS - maximum of Gaussian integration points
!        NCPTS - number of integration points per dimension per cell.
!         WXZY - total integration weight for each point for plate JP.

!  J_SCAT (J1,JCL,JS,JP) is the current in the direction J1 in the centre of
!                        cell JCL of plate JP at station JS.
!          J1 = 1 => along strike;  = 2 => down dip

 IMPLICIT NONE
 COMPLEX, PARAMETER :: ZERO=(0.,0.)
 INTEGER JQ,JF,NFRQ,JP,NPLT,MXAB,MXGS,NSTAT,NRX,NLYR,NCPTS,NRMGT,JS,J1,JCL
 INTEGER, DIMENSION (NPLT) :: NA,NB
 REAL RHOTRP(NRMGT),THK(NLYR),RZ(NSTAT,NRX),RMU(NLYR),CSTR,SSTR
 REAL, DIMENSION (NPLT) :: PLDIP,PLAZM,WXZY
 REAL, DIMENSION (NSTAT,NRX,NPLT) :: XRM,YRM
 REAL, DIMENSION (MXGS,NPLT) :: XGS,YGS,ZGS
 COMPLEX HA(MXAB,3,NSTAT),HB(MXAB,3,NSTAT),KSQ_LYR(NLYR),TMP(3),TMPR(3), &
         J_SCAT(2,MXAB,NSTAT,NPLT),BFD_SCAT(NFRQ,NSTAT,3)
 LOGICAL SAME_TX(NSTAT)

 INTENT (INOUT) BFD_SCAT

!  Compute the magnetic Green's tensor integrals for each plate and combine
!  them with the scattering currents and sum.


 PLATE_LOOP: DO JP = 1,NPLT

   CSTR = COS (PLAZM(JP))
   SSTR = SIN (PLAZM(JP))
   CALL MGT_BOSS (JQ,JP,NPLT,MXAB,NA,NB,PLDIP,NLYR,RMU,KSQ_LYR,THK,NSTAT,SAME_TX, &
                  NRX,XRM,YRM,RZ,NRMGT,RHOTRP,NCPTS,MXGS,XGS,YGS,ZGS,WXZY,HA,HB)

   TX_LOOP: DO JS = 1,NSTAT

!  Multiply the magnetic Green's tensor elements times the strike and
!  downdip currents.  J1 = 1,2,3 represents the along strike, horizontal
!  cross strike and vertical components respectively.  This has to be rotated
!  so that J1 = 1,2,3 represent the North, East and vertical component
!  respectively. Strike angle is defined as positive, clockwise from North.

     TMP = ZERO; TMPR = ZERO
     DO J1 = 1,3
       DO JCL = 1, NA(JP) * NB(JP)
         TMP(J1) = TMP(J1) + J_SCAT(1,JCL,JS,JP) * HA(JCL,J1,JS) &
                           + J_SCAT(2,JCL,JS,JP) * HB(JCL,J1,JS)
       END DO
     END DO

!  Rotate the components back into the user specified coordinate system.

     TMPR(1) = TMP(1) * CSTR - TMP(2) * SSTR
     TMPR(2) = TMP(1) * SSTR + TMP(2) * CSTR
     TMPR(3) = TMP(3)
     BFD_SCAT(JF,JS,1:3) = BFD_SCAT(JF,JS,1:3) + TMPR(1:3)
   END DO TX_LOOP
 END DO PLATE_LOOP

END SUBROUTINE SCAT_MAG

 SUBROUTINE SET_CELLS (KP,NPLT,NLYR,MXAB,MXB,CELLW,THK,PLNGTH,PLWDTH,NA,NB,DA,DB, &
                       XCNTR,YCNTR,PLTOP,PLAZM,PLDIP,PLUNJ,XCELL,YCELL,ZCELL,PCNR)
!----------------------------_-----------------------------------------------------

!*** Called by: MAIN, FORJAC

!   KP = 0 converts the location and dimension of all plates into cells.
!          all plates extending into OB are pushed down into host
!
!   1 <= KP <= NPLT  converts the location and dimension of plate KP into cells.
!                    This would be used during the Jacobian process when plates
!                    are varied individually.  It wouldn't go into the OB because
!                    location can only increase downwards in this case.
!
!
!             OTHER INPUT
!             -----------
!          KP = 0 => push plate down into host and leave OB as is.
!             = 1 => Leave the plate where it is but decrease OB thickness &
!          KP = (1, 2, or 3) => shift cells (north, east, down) by DELQ
!        NPLT = number of thin plates
!        NLYR = number of layers
!         THK = layer thicknesses
!        MXAB - maximum number of cells per plate
!         MXB - maximum number of cell rows down dip per plate
!       CELLW = cell discretisation dimension
!      PLNGTH = plate strike lengths
!      PLWDTH = plate widths along dip
!       XCNTR = north coordinates of plate reference points
!       YCNTR = east coordinates of plate reference points
!       PLAZM = plate azimuths in radians (0 = north)
!       PLDIP = plate dip angle in radians
!
!               INPUT/OUTPUT
!               -----------
!           THK = layer thicknesses
!        NA, NB = Number of cells along strike and down dip respectively
!        DA, DB = Length of cells along strike and down dip respectively
!         PLTOP = depth (+) from ground surface to reference point on top edge of plate
!   XCELL(k,JP) - X (north) coordinate of centre of cell k of plate JP.
!   YCELL(k,JP) - Y (east) coordinate of centre of cell k of plate JP.
!   ZCELL(i,JP) - depth of cell centre in row i of plate nrelative to surface.
!  PCNR(I,J,JP) = Ith component (x,y,z) of Jth corner (1 to 4) of plate JP


 INTEGER NPLT,NLYR,MXAB,MXB,MXA,NA(NPLT),NB(NPLT),KP,JP,JA,JB,JAB,KPF,KPS
 REAL THICK,THK(NLYR),CELLW,TMPL,SDIP,CDIP,CSTR,SSTR,PL2,X0,Y0,Z0,XCTMP,YCTMP,HSTR,PLWC, &
      XCELL(MXAB,NPLT),YCELL(MXAB,NPLT),ZCELL(MXB,NPLT),PCNR(3,4,NPLT)
 REAL, DIMENSION(NPLT) :: XCNTR,YCNTR,PLTOP,PLNGTH,PLWDTH,PLAZM,PLDIP,PLUNJ,DA,DB

 PLUNJ = 0.

 IF (KP > NPLT) KP = 0
 IF (KP > 0) THEN
   KPS = KP
   KPF = KP
 ELSE
   KPS = 1
   KPF = NPLT
 END IF

!  Plates must be at least 1 metre below surface.  If necessary, adjust
!  PLTOP so that all plates are in the basement.

 THICK = SUM (THK(1:NLYR-1))

 PLTOP = MAX (PLTOP,1.0)
 PLTOP = MAX (PLTOP, THICK)

 DO JP = KPS, KPF
   CDIP = COS (PLDIP(JP))
   SDIP = SIN (PLDIP(JP))
   IF (ABS (CDIP) < .02) CDIP = 0.
   IF (ABS (SDIP) < .02) SDIP = 0.

   CSTR = COS (PLAZM(JP))
   SSTR = SIN (PLAZM(JP))
   IF (ABS (CSTR) < 1.0E-3) CSTR = 0.
   IF (ABS (SSTR) < 1.0E-3) SSTR = 0.

   TMPL = MIN (PLNGTH(JP), PLWDTH(JP)) / 2.
   TMPL = MIN (TMPL, CELLW) + .01
   NB(JP) = CEILING (PLWDTH(JP) / TMPL)
   NB(JP) = MAX (2,NB(JP))
   NB(JP) = MIN (MXB, NB(JP))

   MXA = MXAB / NB(JP)
   NA(JP) = CEILING (PLNGTH(JP) / TMPL)
   NA(JP) = MAX (2,NA(JP))
   NA(JP) = MIN (MXA, NA(JP))

   DA(JP) = PLNGTH(JP) / REAL (NA(JP),4)
   DB(JP) = PLWDTH(JP) / REAL (NB(JP),4)
   PL2 = (NA(JP) + 1) * DA(JP) / 2.
   X0 = - PL2
   Y0 = -CDIP * DB(JP) /2.
   Z0 = PLTOP(JP) - SDIP * DB(JP) /2.

   DO JB = 1, NB(JP)
     ZCELL(JB,JP) = Z0 + JB* DB(JP) *SDIP
     DO JA = 1, NA(JP)
       JAB = JA + ((JB-1) * NA(JP))
       XCTMP = X0 + JA *DA(JP)
       YCTMP = Y0 + JB * DB(JP) * CDIP
       XCELL(JAB,JP) = XCNTR(JP) + (XCTMP * CSTR) - (YCTMP * SSTR)
       YCELL(JAB,JP) = YCNTR(JP) + (YCTMP * CSTR) + (XCTMP * SSTR)
     END DO
   END DO
   HSTR = PLNGTH(JP) /2.
   PLWC = PLWDTH(JP) * CDIP

   PCNR(3,1:2,JP) = PLTOP(JP)                        ! Z for top corners
   PCNR(3,3:4,JP) = PLTOP(JP) + PLWDTH(JP) * SDIP    ! Z for bottom corner

   PCNR(1,1,JP) = XCNTR(JP) - HSTR * CSTR     ! X,Y for top corners
   PCNR(2,1,JP) = YCNTR(JP) - HSTR * SSTR
   PCNR(1,2,JP) = XCNTR(JP) + HSTR * CSTR
   PCNR(2,2,JP) = YCNTR(JP) + HSTR * SSTR

   PCNR(1,3,JP) = PCNR(1,1,JP) - PLWC * SSTR  ! X,Y for bottom corners
   PCNR(2,3,JP) = PCNR(2,1,JP) + PLWC * CSTR
   PCNR(1,4,JP) = PCNR(1,2,JP) - PLWC * SSTR
   PCNR(2,4,JP) = PCNR(2,2,JP) + PLWC * CSTR

 END DO

 END SUBROUTINE SET_CELLS

 SUBROUTINE SHIFT_CELLS (JP,NPLT,MXB,NB,DELZ,ZCELL)
!--------------------------------------------------

!***  Called by: FORJAC

!   Shifts the cells in plate JP down by DELZ, north by by DELX, east by by DELY
!
!             OTHER INPUT
!             -----------
!        NPLT = number of thin plates
!         MXB - maximum number of cell rows down dip per plate
!
!               INPUT/OUTPUT
!               -----------
!            NB = Number of cells down dip
!   ZCELL(i,JP) - depth of cell centre in row i of plate nrelative to surface.


 INTEGER NPLT,MXB,NB(NPLT),JP
 REAL DELZ,ZCELL(MXB,NPLT)

 ZCELL(1:NB(JP),JP) = ZCELL(1:NB(JP),JP) + DELZ

 END SUBROUTINE SHIFT_CELLS

 SUBROUTINE SET_MGT (NSTAT,NRX,NPLT,NCPTS,NA,NB,DA,DB,PLTOP,PLWDTH,PLNGTH,PLDIP, &
                     PLAZM,XCNTR,YCNTR,RX,RY,XRM,YRM,WXZY,MXGS,XGS,YGS,ZGS,MGTMX)
!--------------------------------------------------------------------------------

!  Computes the receiver coordinates (XRM,YRM) relative to each plate in a
!  coordinate system where the strike direction of each plate lies along the
!  new XI axis in turn.  In plan view, if the new XI axis points to the right,
!  then the new Y axis points down.  In section view, Z is positive downwards.
!  SET_MGT sets the grid for interpolation of magnetic Green's tensor elements,
!  plus the grid for uniform integration.
!  NCPTS is the number of integration points per dimension per cell.
!  For other variable definitions, see list at end of
!  MODULE LA_Input_routines
!
!***  Called by: LEROI_3D
!
!         NSTAT - number of source positions
!           NRX - number of receivers
!          NPLT - number of plates or plates
!        DA, DB -  cell dimensions along strike and down dip
!        PLWDTH - down dip plate dimension
!        PLNGTH - plate strike length
!         PLAZM - strike angle
!         PLDIP - dip angle
!  XCNTR, YCNTR - (North, East) plate centre coordinates
!        RX, RY - (North, East) receiver coordinates
!         NCPTS - Number of points per cell pe dimension for Gaussian integration
!
!                  OUTPUT
!                  ------
!    XRM, YRM - transformed  receiver positions relative to each plate
!        MXGS - dimension for Gaussian point arrays
!         XGS - coordinates for integration along strike for each plate
!    YGS, ZGS - coordinates for integration down dip for each plate
!        WXZY - total integration weight for each point for plate JP

 INTEGER J1,J2,JP,JR,JS,MXGS,JG,NSTAT,NRX,NPLT,NCPTS
 INTEGER, DIMENSION(NPLT) :: NA,NB
 REAL YBOT,STR2,STEP,ETAR,ETA,C1,C2,C3,C4,XIC,X0,Y0,MGTMX,CDIP,SDIP,CSTR,SSTR
 REAL, DIMENSION (NSTAT,NRX) :: RX,RY
 REAL, DIMENSION (NSTAT,NRX,NPLT) :: XRM,YRM
 REAL, DIMENSION(NPLT) :: WXZY,DA,DB,PLTOP,PLWDTH,PLNGTH,PLDIP,PLAZM,XCNTR,YCNTR
 REAL, DIMENSION(MXGS,NPLT) :: XGS,YGS,ZGS
 LOGICAL DIPZ

 INTENT(IN) NSTAT,NRX,NPLT,NCPTS,NA,NB,DA,DB,PLTOP,PLWDTH,PLNGTH,PLDIP,PLAZM, &
            MXGS,XCNTR,YCNTR,RX,RY
 INTENT(OUT) XRM,YRM,WXZY,XGS,YGS,ZGS,MGTMX

!  Reference the receiver positions to new origin at (XCNTR, YCNTR)
!  Rotate coordinate system so that the new X (or XI) axis lies along the
!  plate which then runs from XI = -PLNGTH / 2. to +PLNGTH / 2.
!  Also, get maximum and minimum distances between receivers and plate
!  corners.

 XRM=0; YRM=0; WXZY=0; XGS=0; YGS=0; ZGS=0; MGTMX=0
 LOOP_OVER_PLATES: DO JP = 1,NPLT
   CDIP = COS (PLDIP(JP))
   SDIP = SIN (PLDIP(JP))
   CSTR = COS (PLAZM(JP))
   SSTR = SIN (PLAZM(JP))
   WXZY(JP) = DA(JP) * DB(JP) / REAL (NCPTS**2)   ! Integration weights
   YBOT = PLWDTH(JP) * CDIP
   STR2 = PLNGTH(JP) / 2.
   DIPZ = .FALSE.
   IF (ABS (SDIP) > .01) DIPZ = .TRUE.

! Set the Z and Y coordinates (ZGS and YGS) for integration down dip.
! Eta is the downdip coordinate.

   STEP = DB(JP) / REAL(NCPTS)
   DO J1 = 1, NB(JP)
     ETAR = (J1 - 1) * DB(JP) - STEP/2.
     DO J2 = 1,NCPTS
       JG = (NCPTS * (J1-1)) + J2
       ETA = J2 * STEP + ETAR
       ZGS(JG,JP) = PLTOP(JP)
       IF (DIPZ) ZGS(JG,JP) = PLTOP(JP) + (ETA * SDIP)
       YGS(JG,JP) = ETA * CDIP
     END DO
   END DO

! Set the XI coordinates for integration along strike.

   STEP = DA(JP) / REAL(NCPTS)
   DO J1 = 1,NA(JP)
     XIC = (J1 - 1) * DA(JP) - STEP/2. - (PLNGTH(JP) /2.)
     DO J2 = 1,NCPTS
       JG = (NCPTS * (J1-1)) + J2
       XGS(JG,JP) = J2 * STEP + XIC
     END DO
   END DO

   DO JS = 1,NSTAT     !  Find maximum distance to receivers for each plate.
     DO JR = 1,NRX
       X0 = RX(JS,JR) - XCNTR(JP)
       Y0 = RY(JS,JR) - YCNTR(JP)
       XRM(JS,JR,JP) =  X0 * CSTR + Y0 * SSTR
       YRM(JS,JR,JP) = -X0 * SSTR + Y0 * CSTR
       C1 = (XRM(JS,JR,JP) + STR2)**2 +  YRM(JS,JR,JP)**2
       C2 = (XRM(JS,JR,JP) - STR2)**2 +  YRM(JS,JR,JP)**2
       C3 = (XRM(JS,JR,JP) + STR2)**2 + (YRM(JS,JR,JP) - YBOT)**2
       C4 = (XRM(JS,JR,JP) - STR2)**2 + (YRM(JS,JR,JP) - YBOT)**2
       MGTMX = MAX (MGTMX, C1, C2, C3, C4)
     END DO
   END DO
   MGTMX = SQRT (MGTMX)
 END DO LOOP_OVER_PLATES

END SUBROUTINE SET_MGT

 SUBROUTINE SET_NCELL2 (NPLT,NA,NB,NCELL2,MXCL2)
!-----------------------------------------------

!*** Called by: LEROI_3D

!  Computes
!      NCELL2(J) = 2 * the number of cells in plates 1 to J
!          MXCL2 = 2 * total number of cells in al plates = NCELL2(NPLT)
!
!  Where
!           NPLT = number of plates
!   NA(J), NB(J) = number of cells along strike and down dip for plate J

 INTEGER NPLT,MXCL2,JP,NA(NPLT),NB(NPLT),NCELL2(0:NPLT)

 NCELL2(0) = 0
 DO JP = 1,NPLT
   NCELL2(JP) = NCELL2(JP-1) + 2* NA(JP) * NB(JP)
 END DO
 MXCL2 = NCELL2(NPLT)

 END SUBROUTINE SET_NCELL2

 SUBROUTINE SET_RHO (MXAB,NPLT,NSTAT,MGTMX,NA,NB,DA,PLNGTH,XCELL,YCELL,SX,SY, &
                     NRMGT,NRPRM,NREGT,MXRHO,B)
!------------------------------------------------------------------------------

! Sets up horizontal interpolation array for use in PRM_BOSS, MGT_BOSS and MGT_BOSS

!*** Called by: LEROI_3D

!    INPUT
!    -----
!
!        MXAB - maximum number of cells per plate
!        NPLT - number of plates or plates
!       NSTAT - number of stations
!       MGTMX - maximum cell receiver separation (from SET_MGT)
!  XCELL(k,n) - X (north) coordinate of centre of cell k of plate n.
!  YCELL(k,n) - Y (east) coordinate of centre of cell k of plate n.
!      SX, SY - arrays of transmitter (North, East) coordinates
!      NA, NB - the number of cells along strike and down dip respectively for each plate
!      DA, DB - cell dimensions along strike and down dip respectively for each plate
!      PLNGTH - strike lngth of each plate

!    OUTPUT
!    ------
!
!  MXRHO - maximum required number of horizontal interpolation at 15 points / decade
!      B - horizontal interpolation point array
!  NREGT - maximum dimension of B for electric Green's functions
!  NRMGT - maximum dimension of B for magnetic Green's functions
!  NRPRM - maximum dimension of B for primary field at targets

 USE LA_Filter_coefficients  ! get NDEC_JN & SHFT_JN from here to ensure consistancy

 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 INTEGER MXAB,JP,NPLT,NAB,JAB,JP2,JAB2,NSTAT,JS,JR,NAB2,NA(NPLT),NB(NPLT),NRMGT, &
         NRPRM,NREGT,MXRHO
 REAL PRMX,EGTMX,EGTSQ,MGTMX,BIGRHO,R1,B(1000),XCELL(MXAB,NPLT),YCELL(MXAB,NPLT), &
      PLNGTH(NPLT),DA(NPLT),SX(NSTAT),SY(NSTAT)
 REAL(KIND=QL) QRHO, RBASE

 INTENT (IN) MXAB,NPLT,MGTMX,NA,NB,DA,XCELL,YCELL,PLNGTH,NSTAT,SX,SY
 INTENT (OUT) NRMGT,NRPRM,NREGT,MXRHO,B

 B = 0.
 PRMX = 0.                ! initialise maximum source - cell separation
 EGTMX = MAXVAL (PLNGTH)  ! initialise maximum inter-plate cell separation
 EGTSQ = EGTMX**2

 DO JP = 1, NPLT      !  Sum over plates
   NAB = NA(JP) * NB(JP)
   DO JAB = 1, NAB
     DO JS = 1,NSTAT
       R1 = SQRT( (SX(JS) - XCELL(JAB,JP))**2 + (SY(JS) - YCELL(JAB,JP))**2)
       PRMX = MAX (R1, PRMX)
     END DO
     IF ( NPLT > 1 .AND. JP < NPLT ) THEN
       DO JP2 = JP+1,NPLT
         NAB2 = NA(JP2) * NB(JP2)
         DO JAB2 = 1, NAB2
           R1 = (XCELL(JAB,JP) - XCELL(JAB2,JP2))**2 + &
                (YCELL(JAB,JP) - YCELL(JAB2,JP2))**2
           EGTSQ = MAX (EGTSQ,R1)
         END DO
       END DO
     END IF
   END DO
 END DO
 EGTMX = SQRT (EGTSQ) + .5 * MAXVAL(DA)
 BIGRHO = MAX (PRMX,MGTMX,EGTMX)     !  Use MGTMX from SET_MGT

!  Set the horizontal interpolation grid to conform to filter intervals.

 QRHO = LOG(10.D0) / DBLE (NDEC_JN)
 QRHO = EXP (QRHO)
 RBASE = EXP (DBLE (-SHFTJN))

 B=0.
 B(1) = .1

 DO JR = 1,1000                 !  Get starting point
   IF (RBASE < B(1)) EXIT
   RBASE = RBASE / QRHO
 END DO
 B(1) = REAL (RBASE)

 NRPRM = 1;  NRMGT = 1; NREGT = 1; MXRHO = 1
 DO JR = 2, 1000
   MXRHO = JR
   RBASE = RBASE * QRHO
   B(JR) = REAL (RBASE)
   IF (B(JR) < MGTMX) NRMGT = JR + 1
   IF (B(JR) < PRMX)  NRPRM = JR + 1
   IF (B(JR) < EGTMX) NREGT = JR + 1
   IF (B(JR) > BIGRHO) EXIT
 END DO

END SUBROUTINE SET_RHO

 SUBROUTINE WRITE_MODEL (NW,KPR,INVERT,NLYR,RES,THK,CHRG,CTAU,CFREQ,RMU,REPS,NPLT,MXAB,    &
                         MXB,SIG_T,CHRGP,CTAUP,CFREQP,PLNGTH,PLWDTH,D_NORTH,D_EAST,GND_LVL, &
                         XCNTR,YCNTR,PLTOP,PLAZM,PLDIP,PLUNJ,PCNR,NA,NB,XCELL,YCELL,ZCELL)
!-----------------------------------------------------------------------------------------

!***  Called by: MAIN

!      NW - output unit
!     KPR = 1 print PLATE info & host  -  negative KPR implies inversion
!         = 2 plus corners
!         = 3 plus cells
!    NLYR - number of layers
!     RES - array of layer resistivities
!    REPS - relative dielectric constant
!     RMU - mu(i) / mu(0)
!     THK - layer thicknesses
!   CHRG -  chargeability
!    CTAU - layer time constants
!   CFREQ - layer frequency constants
! D_NORTH - north offset from real world to model coordinate
!  D_EAST - east offset from real world to model coordinate
! GND_LVL - ground level
!    NPLT - Number of plates (index = JP)
!    MXAB - Number of cells in biggest plate
!  NA, NB - number of cells along strike and down dip respectively
!   SIG_T = plate conductances
!   XCNTR = north coordinates of plate reference points
!   YCNTR =  east coordinates of plate reference points
!   PLTOP = depth to reference point on top edge of plate
!  PLNGTH = plate strike lengths
!  PLWDTH = plate widths along dip
!   PLAZM = plate azimuths in radians (0 = north)
!   PLDIP = plate dip angle in radians
!    PCNR - plate coprner coordinates
!   XCELL - X (north) coordinate of centre of cells (Leroi system)
!   YCELL - Y (east) coordinate of centre of cells  (Leroi system)
!   ZCELL - depth of cell centres                   (Leroi system)

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 REAL, PARAMETER :: PI=3.141592654
 INTEGER NW,KPR,NLYR,NPLT,MXAB,MXB,J,JA,JB,JAB,JP
 INTEGER, DIMENSION(NPLT) :: NA,NB,PLATE_IP
 REAL, DIMENSION(NPLT) :: SIG_T,CHRGP,CTAUP,CFREQP,PLNGTH,PLWDTH,XCNTR,YCNTR, &
                          PLTOP,PLAZM,PLDIP,PLUNJ
 REAL, DIMENSION(NLYR) :: THK,RES,CTAU,CFREQ,RMU,REPS,CHRG
 REAL(KIND=QL) D_NORTH, D_EAST, NCNTRD, ECNTRD
 REAL GND_LVL,ZP,PCNR(3,4,NPLT),XCELL(MXAB,NPLT),YCELL(MXAB,NPLT), &
      ZCELL(MXB,NPLT),AZM,DIP,PLG
 LOGICAL FULL, INVERT

 FULL = .FALSE.
 IF (INVERT) THEN
   WRITE(NW,1)
 ELSE
   WRITE(NW,2)
 END IF
 IF (MAXVAL (CHRG) > 1.E-4) FULL = .TRUE.
 IF (MAXVAL (RMU) >  1.0001) FULL = .TRUE.
 IF (MAXVAL (REPS) > 1.0001) FULL = .TRUE.
 IF (FULL) THEN
   IF (NLYR > 1) THEN
     WRITE(NW,4)
     DO J = 1,NLYR-1
       WRITE(NW,8) J,THK(J),RES(J),RMU(J),REPS(J),CHRG(J),CTAU(J),CFREQ(J)
     END DO
     J = NLYR
     WRITE(NW,9) J,RES(J),RMU(J),REPS(J),CHRG(J),CTAU(J),CFREQ(J)
   ELSE
     WRITE(NW,6)
     J = NLYR
     WRITE(NW,19) RES(J),RMU(J),REPS(J),CHRG(J),CTAU(J),CFREQ(J)
   END IF
 ELSE
   IF (NLYR > 1) THEN
     WRITE(NW,5)
     DO J = 1,NLYR-1
       WRITE(NW,8) J,THK(J),RES(J)
     END DO
     J = NLYR
     WRITE(NW,9) J,RES(J)
   ELSE
     WRITE(NW,7) RES(1)
   END IF
 END IF

 PLATE_IP = 1
 DO JP = 1,NPLT
   IF (CHRGP(JP) < 0.01)   PLATE_IP = 0
   IF (CFREQP(JP) < 0.01)  PLATE_IP = 0
   IF (CTAUP(JP) < 0.1E-6) PLATE_IP = 0
 END DO

 IF (MAXVAL (PLATE_IP) > 0) THEN
   WRITE(NW,20)
   IF (INVERT) THEN
     WRITE(NW,22)
   ELSE
     WRITE(NW,21)
   END IF
   WRITE(NW,23)

   DO JP = 1,NPLT
     WRITE(NW,24) JP,CHRGP(JP),CFREQP(JP),CTAUP(JP)
   END DO
 END IF

 IF (NPLT > 0) WRITE(NW,10)
 DO JP = 1,NPLT

!   AZM = PLAZM(JP) * 180. / PI
   AZM = PLAZM(JP) * 180. / PI + 90.  ! PLAZM is still the strike azimuth
   DIP = PLDIP(JP) * 180. / PI
   PLG = PLUNJ(JP) * 180. / PI

   NCNTRD = REAL (XCNTR(JP),QL) + D_NORTH
   ECNTRD = REAL (YCNTR(JP),QL) + D_EAST
   ZP = GND_LVL - PLTOP(JP)
   WRITE(NW,11) JP,SIG_T(JP),ZP,ECNTRD,NCNTRD,PLNGTH(JP),PLWDTH(JP),AZM,DIP,PLG
 END DO

 IF (KPR > 1) THEN
   WRITE(NW,16)
   DO JP = 1,NPLT
     AZM = PLAZM(JP) * 180. / PI
     AZM = AZM + 90.                      ! PLAZM is still the strike azimuth
     DIP = PLDIP(JP) * 180. / PI
     PLG = PLUNJ(JP) * 180. / PI
     NCNTRD = REAL (XCNTR(JP),QL)
     ECNTRD = REAL (YCNTR(JP),QL)
     ZP = PLTOP(JP)
     WRITE(NW,11) JP,SIG_T(JP),ZP,ECNTRD,NCNTRD,PLNGTH(JP),PLWDTH(JP),AZM,DIP,PLG
   END DO

   WRITE(NW,12)
   DO JP = 1,NPLT
     WRITE(NW,13) JP,PCNR(1,1:4,JP),PCNR(2,1:4,JP),PCNR(3,1:4,JP)
   END DO
 END IF

 IF (KPR > 2) THEN
   DO JP = 1,NPLT
     WRITE(NW,15) JP
     DO JB = 1, NB(JP)
       DO JA = 1, NA(JP)
         JAB = JA + ((JB-1) * NA(JP))
         IF (JA == 1) THEN
           IF (JB > 1) WRITE(NW,'(A)') '  '
           WRITE(NW,'(I5,I7,6X,3G13.4)') JB,JA,YCELL(JAB,JP),XCELL(JAB,JP),ZCELL(JB,JP)
         ELSE
           WRITE(NW,'(5X,I7,6X,3G13.4)')    JA,YCELL(JAB,JP),XCELL(JAB,JP),ZCELL(JB,JP)
         END IF
       END DO
     END DO
   END DO
 END IF

 1 FORMAT(//T9,'Initial Model Description Before Inversion' &
           /T9,'==========================================')
 2 FORMAT(//T9,'Model Description' /T9,'=================')
 4 FORMAT(/T11,'+----------------------------------+'  &
          /T11,'+  Layered Earth Model Parameters  +'  &
          /T11,'+----------------------------------+'  &
          //T2,'Layer  Thickness  Resistivity   MU-R   EPS-R   CHRG    CTAU      CFREQ' &
           /T2,'-----  ---------  -----------   ----   -----   ----    ----      -----')
 5 FORMAT(/T11,'+----------------------------------+'  &
          /T11,'+  Layered Earth Model Parameters  +'  &
          /T11,'+----------------------------------+'  &
          //T2,'Layer  Thickness  Resistivity' &
           /T2,'-----  ---------  -----------')
 6 FORMAT(/T11,'+----------------------------------+'  &
          /T11,'+  Layered Earth Model Parameters  +'  &
          /T11,'+----------------------------------+'  &
          //T2,'Resistivity   MU-R   EPS-R   CHRG    CTAU      CFREQ' &
           /T2,'-----------   ----   -----   ----    ----      -----')
 7 FORMAT(/T3,'Host resistivity =',G15.4)
 8 FORMAT(I4,F11.1,G15.4,2F7.2,F8.2,G11.2,F7.2)
 9 FORMAT(I4,11X,  G15.4,2F7.2,F8.2,G11.2,F7.2)

 10 FORMAT(//T21,'+-------------------------------+' &
            /T21,'+    Plate Model Parameters     +' &
            /T21,'+-------------------------------+' &
           //T14,'        ____ Plate Reference Point ____                     ______  Angles  ________'  &
            /T14,'       |                               |                   |                        |' &
            /T14,'         Depth      Centre      Centre    Strike    Plate     Dip' &
            /T14,'SIG_T     RL         East       North     Length    Width   Azimuth   Dip   Plunge' &
            /T14,'-----    -----      ------      ------    ------    -----   -------   ---   ------')
 11 FORMAT(T3,'Plate',I2,':',F7.0,2F11.1,F12.1,F10.0,F9.0,F9.0,2F7.0)
 12 FORMAT(//T21,'+-------------------------------+' &
            /T21,'+       Plate Corners           +' &
            /T21,'+   (Transformed Coordinates)   +' &
            /T21,'+-------------------------------+' &
           //T28,'1           2           3           4')
 13 FORMAT(/T3,'Plate',I3,T12,'North:',4F12.1 &
                         /T13, 'East:',4F12.1 &
                         /T12,'Depth:',4F12.1)
 15 FORMAT (//11X,'+-------------------------------------------+'       &
             /11X,'+ Target Cell Centre Locations for Plate',I2,T56,'+' &
             /11X,'+    (Transformed Coordinate System)        +'       &
             /11X,'+-------------------------------------------+'       &
            //'   Dip   Strike                      '                 &
             /'  Index  Index        (East)      (North)       Depth' &
             /'  -----  ------       ------      -------       -----')
 16 FORMAT(//T21,'+-------------------------------+' &
            /T21,'+    Plate Model Parameters     +' &
            /T21,'+   (Transformed Coordinates)   +' &
            /T21,'+-------------------------------+' &
           //T14,'        ____ Plate Reference Point ____'  &
            /T14,'       |                               |' &
           //T14,'         Depth                              Dimensions            Angles' &
            /T14,'         below      Centre      Centre    Strike    Plate     Dip' &
            /T14,'SIG_T    Surface     East       North     Length    Width   Azimuth   Dip   Plunge' &
            /T14,'-----    -------    ------      ------    ------    -----   -------   ---   ------')
 19 FORMAT(G12.4,2F7.2,F8.2,G11.2,F7.2)
 20 FORMAT(//T17,'COLE-COLE PARAMETERS FOR PLATES')
 21 FORMAT(  T17,'-------------------------------')
 22 FORMAT(T15,'These do NOT change during inversion' &
          /T15,'------------------------------------')
 23 FORMAT(/T14,'CHRG   CFREQ      CTAU' &
           /T14,'----   -----      ----')
 24 FORMAT(T3,'Plate',I2,':',F7.2,F8.2,G13.3)

 END SUBROUTINE WRITE_MODEL

 SUBROUTINE WRITE_FAILED_MODEL (NW,NLYR,RES,THK,CALF,CTAU,CFREQ,RMU,REPS,NPLT,PLNGTH,PLWDTH, &
                                SIG_T,CALFP,CTAUP,CFREQP,XCNTR,YCNTR,PLTOP,PLAZM,PLDIP)
!--------------------------------------------------------------------------------------------

!***  Called by: MAIN

!      NW - output unit
!    NLYR - number of layers
!     RES - array of layer resistivities
!    REPS - relative dielectric constant
!     RMU - mu(i) / mu(0)
!     THK - layer thicknesses
!   CHRG -  chargeability
!    CTAU - layer time constants
!   CFREQ - layer frequency constants
!    NPLT - Number of plates (index = JP)
!   SIG_T = plate conductances
!   XCNTR = north coordinates of plate reference points
!   YCNTR =  east coordinates of plate reference points
!   PLTOP = depth to reference point on top edge of plate
!  PLNGTH = plate strike lengths
!  PLWDTH = plate widths along dip
!   PLAZM = plate azimuths in radians (0 = north)
!   PLDIP = plate dip angle in radians

 IMPLICIT NONE
 REAL, PARAMETER :: PI=3.141592654
 INTEGER NW,NLYR,NPLT,J,JP
 INTEGER, DIMENSION(NPLT) :: PLATE_IP
 REAL, DIMENSION(NPLT) :: SIG_T,CHRGP,CALFP,CTAUP,CFREQP,PLNGTH,PLWDTH,XCNTR,YCNTR, &
                          PLTOP,PLAZM,PLDIP,PLUNJ
 REAL, DIMENSION(NLYR) :: THK,RES,CTAU,CFREQ,RMU,REPS,CHRG,CALF
 REAL AZM,DIP,PLG

 CHRG =  1. - CALF
 CHRGP = 1. - CALFP
 PLUNJ = 0.
 WRITE(NW,1)
 IF (NLYR > 1) THEN
   WRITE(NW,4)
   DO J = 1,NLYR-1
     WRITE(NW,8) J,THK(J),RES(J),RMU(J),REPS(J),CHRG(J),CTAU(J),CFREQ(J)
   END DO
   J = NLYR
   WRITE(NW,9) J,RES(J),RMU(J),REPS(J),CHRG(J),CTAU(J),CFREQ(J)
 ELSE
   WRITE(NW,6)
   J = NLYR
   WRITE(NW,19) RES(J),RMU(J),REPS(J),CHRG(J),CTAU(J),CFREQ(J)
 END IF

 PLATE_IP = 1
 DO JP = 1,NPLT
   IF (CHRGP(JP) < 0.01)   PLATE_IP = 0
   IF (CFREQP(JP) < 0.01)  PLATE_IP = 0
   IF (CTAUP(JP) < 0.1E-6) PLATE_IP = 0
 END DO

 IF (MAXVAL (PLATE_IP) > 0) THEN
   WRITE(NW,20)
   DO JP = 1,NPLT
     WRITE(NW,24) JP,CHRGP(JP),CFREQP(JP),CTAUP(JP)
   END DO
 END IF

 IF (NPLT > 0) WRITE(NW,10)
 DO JP = 1,NPLT

!   AZM = PLAZM(JP) * 180. / PI
   AZM = PLAZM(JP) * 180. / PI + 90.  ! PLAZM is still the strike azimuth
   DIP = PLDIP(JP) * 180. / PI
   PLG = PLUNJ(JP) * 180. / PI
   WRITE(NW,11) JP,SIG_T(JP),PLTOP(JP),XCNTR,YCNTR,PLNGTH(JP),PLWDTH(JP),AZM,DIP,PLG
 END DO

 1 FORMAT(//T9,'MODEL CAUSING SINGULAR MATRIX' &
           /T9,'  (Transformed coordinates)'  &
           /T9,'=============================')
 4 FORMAT(/T11,'+----------------------------------+'  &
          /T11,'+  Layered Earth Model Parameters  +'  &
          /T11,'+----------------------------------+'  &
          //T2,'Layer  Thickness  Resistivity   MU-R   EPS-R   CHRG    CTAU      CFREQ' &
           /T2,'-----  ---------  -----------   ----   -----   ----    ----      -----')
 6 FORMAT(/T11,'+----------------------------------+'  &
          /T11,'+  Layered Earth Model Parameters  +'  &
          /T11,'+----------------------------------+'  &
          //T2,'Resistivity   MU-R   EPS-R   CHRG    CTAU      CFREQ' &
           /T2,'-----------   ----   -----   ----    ----      -----')
 8 FORMAT(I4,F11.1,G15.4,2F7.2,F8.2,G11.2,F7.2)
 9 FORMAT(I4,11X,  G15.4,2F7.2,F8.2,G11.2,F7.2)

 10 FORMAT(//T21,'+-------------------------------+' &
            /T21,'+    Plate Model Parameters     +' &
            /T21,'+-------------------------------+' &
           //T14,'        ____ Plate Reference Point ____                     ______  Angles  ________'  &
            /T14,'       |                               |                   |                        |' &
            /T14,'                    Centre      Centre    Strike    Plate     Dip' &
            /T14,'SIG_T    Depth       East       North     Length    Width   Azimuth   Dip  Plunge' &
            /T14,'-----    -----      ------      ------    ------    -----   -------   ---  ------')
 11 FORMAT(T3,'Plate',I2,':',F7.0,2F11.1,F12.1,F10.0,F9.0,F9.0,2F7.0)
 19 FORMAT(G12.4,2F7.2,F8.2,G11.2,F7.2)
 20 FORMAT(//T17,'COLE-COLE PARAMETERS FOR PLATES' &
            /T17,'-------------------------------' &
            /T14,'CHRG   CFREQ      CTAU'          &
            /T14,'----   -----      ----')
 24 FORMAT(T3,'Plate',I2,':',F7.2,F8.2,G13.3)

 END SUBROUTINE WRITE_FAILED_MODEL

!==================================================================================
!  ROUTINES SPECIFIC FOR INVERSION
!  -------------------------------

 SUBROUTINE CNVRT_BOUNDS (NPLT,NLYR,NPAR,LBND,UBND,XYNORM,CXPAR,XLBND,XUBND)
!---------------------------------------------------------------------------

!*** Called by NLSQ2

! Converts the LBND & UBND specified by the user into the form used
! by the inversion, XLBND & XUBND.

! Bounds are only applied to plate or layer parameters for CXPAR = 3

!  INPUT:  NPLT,NLYR,NPAR,LBND,UBND,XYNORM,CXPAR
! OUTPUT:  XLBND,XUBND

 REAL, PARAMETER :: PI=3.141592654
 INTEGER NPLT,NLYR,NL1,NPAR,CXPAR(NPAR),JP,JP1,J1
 REAL XYNORM
 REAL,DIMENSION(NPAR) :: LBND,UBND,XLBND,XUBND

 DO JP = 1,NPLT
   DO J1 = 1,9
     JP1 = J1 + 9* (JP-1)
     IF (CXPAR(JP1) == 3) THEN
       IF (J1 < 5) THEN
         XLBND(JP1) = LOG (LBND(JP1))
         XUBND(JP1) = LOG (UBND(JP1))
       ELSE IF (J1 == 5 .OR. J1 == 6) THEN
         XLBND(JP1) = LBND(JP1) / XYNORM
         XUBND(JP1) = UBND(JP1) / XYNORM
       ELSE IF (J1 > 6) THEN
         XLBND(JP1) = LBND(JP1) / PI
         XUBND(JP1) = UBND(JP1) / PI
       END IF
     END IF
   END DO
 END DO

 NL1 = 2*NLYR - 1
 DO J1 = 1, NL1
   JP1 = J1 + 9*NPLT
   IF (CXPAR(JP1) == 3) THEN
     XLBND(JP1) = LOG(LBND(JP1))
     XUBND(JP1) = LOG(UBND(JP1))
   END IF
 END DO

END SUBROUTINE CNVRT_BOUNDS

 SUBROUTINE CNVRT2_MPAR (NLYR,NPLT,NPAR,RES,THK,SIG_T,PLTOP,PLNGTH,PLWDTH, &
                         XCNTR,YCNTR,XYNORM,PLAZM,PLDIP,PLUNJ,XPAR)
!-------------------------------------------------------------------------

!*** Called by: NLSQ2
!***     Calls: FIX_AZM

!  Converts inversion parameters to model parameters
!
!      NLYR = number of layers = 1 or 2
!      NPLT = number of thin plates
!      NPAR =  2*NLYR-1 + 9*NPLT
!       RES = layer resistivities
!       THK = layer thicknesses
!     SIG_T = plate conductances
!     PLTOP = depth to reference point on top edge of plate
!    PLNGTH = plate strike lengths
!    PLWDTH = plate widths along dip
!     YCNTR =  east coordinates of plate reference points
!     XCNTR = north coordinates of plate reference points
!     PLAZM = plate azimuths in radians (0 = north)
!     PLDIP = plate dip angle in radians
!      XPAR = transformed parameters


! The parameters are ordered in groups of 9 corresponding to the parameters
! for each plate - followed by host resistivity, overburden resistivity &
! thickness.
!
!     Define KP = 9* (JP-1) where JP = plate index.
!
!  Then KPAR, the parameter index is DEFINED / ordered as
!
!     KP + 1 - SIG_T  = conductance of plate JP
!        + 2 - PLTOP  = depth of reference point of plate JP
!        + 3 - PLNGTH = strike length of plate JP
!        + 4 - PLWDTH = dip width of plate JP
!        + 5 - YCNTR  = east coordinate of reference point of plate JP
!        + 6 - XCNTR  = north coordinate of reference point of plate JP
!        + 7 - PLAZM  = dip azimuth of plate JP
!        + 8 - PLDIP  = dip angle of plate JP
!        + 9 - PLUNJ  = plunge angle of plate JP
!
!      9*NPLT + 1         - LAYER 1 resistivity (host resistivity if NLYR = 1)
!      9*NPLT + J         - resistivity of layer J
!      9*NPLT + NLYR + J  - thickness oflayer J
!
!
!          TRANSFORMED PARAMETERS FOR INVERSION
!          ------------------------------------
!
!   SIG_T, PLTOP, PLWDTH, PLNGTH, RES & THK are represented logarithmically
!
!   XCNTR * YCNTR are normalised to PLTOP
!
!   PLAZM, PLDIP & PLUNJ are normalised to PI

 REAL, PARAMETER :: PI=3.141592654
 INTEGER  NLYR,NPLT,NPAR,KP,KP1,JP
 REAL, DIMENSION(NPLT) :: SIG_T,XCNTR,YCNTR,PLTOP,PLNGTH,PLWDTH,PLAZM,PLDIP,PLUNJ
 REAL XPAR(NPAR),RES(NLYR),THK(NLYR),XYNORM

 KP = 9*NPLT
 KP1 = KP + NLYR
 RES(1:NLYR) = EXP (XPAR(KP+1:KP1))
 DO JP = 1,NLYR-1
   THK(JP) = EXP (XPAR(KP1+JP))
 END DO

 DO JP = 1,NPLT
   KP = 9*(JP-1)
   SIG_T(JP)  = EXP (XPAR(KP+1))
   PLTOP(JP)  = EXP (XPAR(KP+2))
   PLNGTH(JP) = EXP (XPAR(KP+3))
   PLWDTH(JP) = EXP (XPAR(KP+4))
   YCNTR(JP) = XYNORM * XPAR(KP+5)
   XCNTR(JP) = XYNORM * XPAR(KP+6)
   PLAZM(JP) = PI * XPAR(KP+7)
   PLDIP(JP) = PI * XPAR(KP+8)
   PLUNJ(JP) = PI * XPAR(KP+9)
 END DO

 CALL FIX_AZM (NPLT,PLAZM)

 END SUBROUTINE CNVRT2_MPAR

 SUBROUTINE CNVRT2_XPAR (NLYR,NPLT,NPAR,RES,THK,SIG_T,PLTOP,PLNGTH,PLWDTH,XCNTR, &
                         YCNTR,XYNORM,PLAZM,PLDIP,PLUNJ,XPAR)
!---------------------------------------------------------------------------------

!*** Called by: NLSQ2
!***     Calls: FIX_AZM

!  Converts from model parameters to inversion parameters

!      NLYR = number of layers = 1 or 2
!      NPLT = number of thin plates
!      NPAR =  2*NLYR-1 + 9*NPLT
!       RES = layer resistivities
!       THK = layer thicknesses
!     SIG_T = plate conductances
!     XCNTR = north coordinates of plate reference points
!     YCNTR =  east coordinates of plate reference points
!     PLTOP = depth to reference point on top edge of plate
!    PLNGTH = plate strike lengths
!    PLWDTH = plate widths along dip
!     PLAZM = plate azimuths in radians (0 = north)
!     PLDIP = plate dip angle in radians
!      XPAR = transformed parameters


! The parameters are ordered:

!     KP = 9* (JP-1) where JP = plate index
!
!     KP + 1 - SIG_T = conductance of plate JP
!        + 2 - PLTOP = depth of reference point of plate JP
!        + 3 - PLNGTH = strike length of plate JP
!        + 4 - PLWDTH = plate width of plate JP
!        + 5 - YCNTR = east coordinate of reference point of plate JP
!        + 6 - XCNTR = north coordinate of reference point of plate JP
!        + 7 - PLAZM = strike angle of plate JP
!        + 8 - PLDIP = dip angle of plate JP
!
!      9*NPLT + 1         - LAYER 1 resistivity (host resistivity if NLYR = 1)
!      9*NPLT + J         - resistivity of layer J
!      9*NPLT + NLYR + J  - thickness oflayer J
!
!
!          TRANSFORMED PARAMETERS FOR INVERSION
!          ------------------------------------
!
!   SIG_T, PLTOP, PLWDTH, PLNGTH, RES & THK are represented logarithmically
!
!   XCNTR * YCNTR are normalised to PLTOP
!
!   PLAZM, PLDIP & PLUNJ are normalised to PI

 REAL, PARAMETER :: PI=3.141592654
 INTEGER NLYR,NPLT,NPAR,KP,KP1,JP
 REAL RES(NLYR),THK(NLYR),XYNORM,XPAR(NPAR)
 REAL,DIMENSION(NPLT) :: SIG_T,XCNTR,YCNTR,PLTOP,PLNGTH,PLWDTH,PLAZM,PLDIP,PLUNJ

 KP = 9*NPLT
 KP1 = KP + NLYR
 XPAR(KP+1:KP1) = LOG (RES(1:NLYR))
 DO JP = 1,NLYR-1
   XPAR(KP1+JP) = LOG (THK(JP))
 END DO

 CALL FIX_AZM (NPLT,PLAZM)
 DO JP = 1,NPLT
   KP = 9*(JP-1)
   XPAR(KP+1) = LOG (SIG_T(JP))
   XPAR(KP+2) = LOG (PLTOP(JP))
   XPAR(KP+3) = LOG (PLNGTH(JP))
   XPAR(KP+4) = LOG (PLWDTH(JP))
   XPAR(KP+5) = YCNTR(JP) / XYNORM
   XPAR(KP+6) = XCNTR(JP) / XYNORM
   XPAR(KP+7) = PLAZM(JP) / PI
   XPAR(KP+8) = PLDIP(JP) / PI
   XPAR(KP+9) = PLUNJ(JP) / PI
 END DO

 END SUBROUTINE CNVRT2_XPAR

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
 REAL A,B(*), C(*)

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
               Q = DPROD1(MKP,1,1,Q,AJAC(K,K),UMAT(K,J))
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

!  Column transformation

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

!  Phase transformation
         IF ( AJAC(K,K1) > 0.0 ) AJAC(K1:NDATA,K1) = -AJAC(K1:NDATA,K1)
       END IF
     END IF
     K = K1
     GOTO 50
   END IF

END SUBROUTINE ESVD

 SUBROUTINE FORJAC (NDATA,INRM,DNORM,XDATA,XMODL,XWTS,NPAR,XPAR,CXPAR,SUMSQ,JCBN,A,VERR,TDFD, &
                    CMP,STEP,IDER,NSX,SWX,SWY,NTYRP,TRP,NPULS,PULSE,NTYPLS,MCHNL,NCHNL,TOPN,  &
                    TCLS,GSTRP,ASTRP,NFRQ,FREQ,KNRM,NORM,TXCLN,TXA90,NRX,XRX,YRX,ZRX,NRXST,   &
                    NSTAT,SX,SY,SZ,FANGLE,SAME_TX,RX,RY,RZ,NLYR,RMU,REPS,CALF,CTAU,CFREQ,     &
                    XYNORM,PARFAC,NPLT,CELLW,MXAB,MXB,CALFP,CTAUP,CFREQP,NW)
!----------------------------------------------------------------------------------------------

!  Sets up and calls for model computation.
!  It also calculates the Jacobian and error vector if required
!  New convention: Dec, 2003: VERR is now VD - VM
!  Thus DELPAR is now added rather than subtracted during updates.

!*** Called by: NLSQ2
!***     Calls: SET_CELLS, SHIFT_CELLS, GET_FWD_MODL, CNVRT2_MPAR

!             General Inversion Input Variables
!             ---------------------------------
!
!       NDATA - dimension of vector to be inverted: = NCMP * NCHNL or 2*NFRQ.
!       DNORM(NDATA) : L1 norm of data for each channel or frequency, averaged over the survey
!        INRM = 1 : point symmetric norm used for fitting error and sensitivity matrix
!        INRM = 2 : survey norm used for fitting error and sensitivity matrix
!       XDATA - data to be inverted in user-specified units
!       XMODL - model data in user-specified units
!        XWTS - weights for XDATA  (0 or 1)
!        NPAR - number of parameters to be inverted (nominally 2*NLYR - 1)
!        XPAR - array of transformed model parameters
!       CXPAR = 0 => parameter is completely free to vary as dictated by inversion step
!             = 1 => parameter is fixed
!             = 2 => parameter is constrained by elasticity.
!             = 3 => parameter bounds are buffered.
!
!             General Inversion Output Variables
!             ----------------------------------
!
!       SUMSQ(1) - sum squared scaled error for point norm
!       SUMSQ(2) - sum squared scaled error for survey norm
!       JCBN    - true if Jacobian required
!       A       - a large array which carries the Jacobian out of this routine
!       VERR(J) - scaled error in channel J
!
!             AEM System Input Variables
!             --------------------------
!
!        TDFD = 1 for TD; 2 for FD
!         CMP - (time-domain only) component to be inverted
!               11: in-line; 13: vertical; 2: joint vertical & in-line
!               3 component; 4 total field
!
!        KNRM - dimension of NORM: = 3 for time-domain,  = NFRQ for frequency-domain
!        NORM - PPM conversion
!        FREQ - array of NFRQ frequencies
!        STEP = 1 iff step response is to be computed
!        IDER = 1 if source waveform was dB/dt; = 0 if amps pr B
!         NSX - number of points used to discretise transmitter signal
!         SWX - abscissae (seconds) of current waveform
!         SWY - dI/dt * Tx moment & nanotesla conversion at times SWX
!       NTYRP - number of values in TRP for total signal length: 2 * NPULS *PULSE
!         TRP - array of time values for FD -> TD transformations
!      NTYPLS - number of TRP values in 1 PULSE
!       NCHNL = number of time domain channels
!       MCHNL = total number of readings per station to be inverted(TD or FD)
!             = NCHNL for time-domain when CMP = 11, 13, 4, 42, 43
!             = 2* NCHNL for time-domain when CMP = 2
!             = 3* NCHNL for time-domain when CMP = 3
!
!             = 2 * NFRQ for frequency-domain
!        TOPN - time at which receiver channel I opens.
!        TCLS - time at which receiver channel I closes.
!       GSTRP = 1 => apply Questem-Geotem stripping algorithm
!       ASTRP = 1 => apply Aerotem stripping algorithm
!        FREQ = array of NFRQ frequencies
!       TXCLN - angle in radians that TX dipole makes with vertical (climb = +)
!       TXA90 - true for vertical co-planar briadside array
!         NRX - number of receivers = NFRQ in FD; = 1 in TD
!       NRXST - dimension for receiver offset & transmitter tilt
!             = NFRQ in FD;  = NSTAT in TD
!         ZRX - vertical receiver offset for each frequency;   below = positive
!         XRX - in-line receiver offset for each frequency;    behind = positive
!         YRX - transverse receiver offset for each frequency; left = positive.
!
!             AEM Survey Input Variables
!             --------------------------
!
!       NSTAT - number of stations in survey line.
!      FANGLE - flight path angle in radians. (North = 0; East = PI/2)
!     SAME_TX - used to reduce redundant computations
!       SX, SY, SZ: north, east and altitude (re gnd level) of transmitter
!       RX, RY, RZ: north, east and altitude (re gnd level) of receiver(s)
!
!             Model Description Input Variables
!             ---------------------------------
!
!        NLYR - number of layers (1 or 2)
!         THK - layer thicknesses
!         RMU - mu(i) / mu(0)
!        REPS - relative dielectric constant
!        CALF, CTAU & CFREQ are the layered earth Cole-Cole parameters.
!
!      XYNORM - survey dependent normalisation for plate X-Y location.
!        NPLT - Number of plates
!       CELLW - maximum cell dimension
!        MXAB - Number of cells in biggest plate
!         MXB - Maximim number of cell rows
!        CALFP, CTAUP & CFREQP are the plate Cole-Cole parameters.
!
!
!          PHYSICAL PARAMETER 0RDERING
!          ---------------------------
!
!     KP = 9* (JP-1) where JP = plate index
!
!     KP + 1 - SIG_T = conductance of plate JP
!        + 2 - PLTOP = depth of reference point of plate JP
!        + 3 - PLNGTH = strike length of plate JP
!        + 4 - PLWDTH = plate width of plate JP
!        + 5 - YCNTR = east coordinate of reference point of plate JP
!        + 6 - XCNTR = north coordinate of reference point of plate JP
!        + 7 - PLAZM = strike angle of plate JP
!        + 8 - PLDIP = dip angle of plate JP
!        + 9 - PLUNJ = plunge angle of plate JP
!
!      9*NPLT + 1 - host resistivity
!      9*NPLT + 2 - overburden resistivity (only if NLYR = 2)
!      9*NPLT + 3 - overburden thickness   (only if NLYR = 2)
!
!
!          TRANSFORMED PARAMETERS FOR INVERSION
!          ------------------------------------
!
!   SIG_T, PLTOP, PLWDTH, PLNGTH, RES & THK are represented logarithmically
!
!   XCNTR * YCNTR are normalised to PLTOP
!
!   PLAZM, PLDIP & PLUNJ are normalised to PI


 IMPLICIT NONE
 REAL, PARAMETER :: PI=3.141592654
 INTEGER NDATA,NPAR,NPLT,NLYR,CXPAR(NPAR),XWTS(NDATA),NCHNL,MCHNL,TDFD,CMP,INRM,KNRM, &
         NFRQ,MXAB,MXB,NA(NPLT),NB(NPLT),NSTAT,NRX,NRXST,STEP,IDER,NSX,NPULS,NTYPLS,  &
         NTYRP,GSTRP,ASTRP,JS,JD,JP,JL,LP,JF,JT,KS1,KS2,KP,K0,NW
 REAL SUMSQ(2),A(NDATA,NPAR+1),XPAR(NPAR),NORM(KNRM),FREQ(NFRQ),CELLW,SWX(NSX),SWY(NSX,3), &
      PULSE,TOPN(NCHNL),TCLS(NCHNL),TRP(NTYRP),X2,X3,VM,VJ,VD,DENOM,V1,V2,DELTA,XYNORM,    &
      DELXY,DELZ,DELPHI,PCNR(3,4,NPLT),XP0,PARFAC,DNORM(NDATA)
 REAL, DIMENSION(NDATA) ::  XMODL,XMODL0,XDATA,VERR
 REAL, DIMENSION(NRXST) :: TXCLN,XRX,YRX,ZRX
 REAL, DIMENSION(NSTAT) :: SX,SY,SZ,FANGLE
 REAL, DIMENSION(NSTAT,NRX) :: RX,RY,RZ
 REAL, DIMENSION(NLYR) :: THK,RES,REPS,RMU,CALF,CTAU,CFREQ
 REAL, DIMENSION(NPLT) :: SIG_T,CALFP,CTAUP,CFREQP,PLTOP,PLWDTH,XCNTR,YCNTR,PLNGTH, &
                          PLAZM,PLDIP,PLUNJ,DA,DB
 REAL, DIMENSION(MXAB,NPLT) :: XCELL,YCELL,ZCELL
 REAL, DIMENSION(NCHNL,NSTAT,3) :: BTD,BTD_SCAT
 COMPLEX XBFD
 COMPLEX, DIMENSION(NFRQ,NSTAT,3) :: BFD,BFD_SCAT
 LOGICAL JCBN,TXA90,SAME_TX(NSTAT)

! Compute initial model & compute error

 CALL CNVRT2_MPAR (NLYR,NPLT,NPAR,RES,THK,SIG_T,PLTOP,PLNGTH,PLWDTH, &
                   XCNTR,YCNTR,XYNORM,PLAZM,PLDIP,PLUNJ,XPAR)
 PLUNJ = 0.

 KP = 0
 CALL SET_CELLS (KP,NPLT,NLYR,MXAB,MXB,CELLW,THK,PLNGTH,PLWDTH,NA,NB,DA,DB, &
                 XCNTR,YCNTR,PLTOP,PLAZM,PLDIP,PLUNJ,XCELL,YCELL,ZCELL,PCNR)

 LP = 0;
 CALL GET_FWD_MODL(LP,PARFAC)
 XMODL0 = XMODL

 SUMSQ = 0.
 DO JD = 1, NDATA
   VERR(JD) = 0.
   IF( XWTS(JD) > 0) THEN
     VD = XDATA(JD)
     VM = XMODL0(JD)
     DENOM = SQRT( (VM*VM + VD*VD)/2.0)
     V1 = XWTS(JD) * (VD-VM) / DENOM        ! Point norm error
     V2 = XWTS(JD) * (VD-VM) / DNORM(JD)    ! Survey norm error
     SUMSQ(1) = SUMSQ(1) + V1**2
     SUMSQ(2) = SUMSQ(2) + V2**2
     IF (INRM == 1) THEN
       VERR(JD) = V1
     ELSE
       VERR(JD) = V2
     END IF
   END IF
 END DO

!  Initialise and then compute the Jacobian as the derivative of log(volts) wrt
!  log(parameter) for a three percent step.  Skip over held parameters

 IF (JCBN) THEN
   A = 0.
   DELXY = PARFAC * XYNORM
   DELPHI = PARFAC * PI
   DO JP = 1,NPLT
     WRITE(*,'(/T3,A,I3/)') 'Compute sensitivities for plate',JP
     KP = JP
     K0 = 9*(JP-1)
     LP = K0 + 1
     IF (CXPAR(LP) /= 1) THEN
       WRITE(*,'(T3,A)') 'Vary plate conductance'
       XP0 = SIG_T(JP)
       SIG_T(JP) = (1. + PARFAC) * SIG_T(JP)
       CALL GET_FWD_MODL(LP,PARFAC)
       SIG_T(jp) = XP0
     END IF
     LP = K0 + 2
     IF (CXPAR(LP) /= 1) THEN
       WRITE(*,'(T3,A)') 'Vary plate depth'
       XP0 = PLTOP(JP)
       DELZ = PARFAC * PLTOP(JP)
       PLTOP(JP) = PLTOP(JP) + DELZ
       CALL SHIFT_CELLS (JP,NPLT,MXB,NB,DELZ,ZCELL)
       CALL GET_FWD_MODL(LP,PARFAC)
       PLTOP(JP) = XP0
     END IF
     LP = K0 + 3
     IF (CXPAR(LP) /= 1) THEN
       WRITE(*,'(T3,A)') 'Vary plate length'
       XP0 = PLNGTH(JP)
       PLNGTH(JP) = (1. + PARFAC) * PLNGTH(JP)
       CALL SET_CELLS (KP,NPLT,NLYR,MXAB,MXB,CELLW,THK,PLNGTH,PLWDTH,NA,NB,DA,DB, &
                       XCNTR,YCNTR,PLTOP,PLAZM,PLDIP,PLUNJ,XCELL,YCELL,ZCELL,PCNR)
       CALL GET_FWD_MODL(LP,PARFAC)
       PLNGTH(JP) = XP0
     END IF
     LP = K0 + 4
     IF (CXPAR(LP) /= 1) THEN
       WRITE(*,'(T3,A)') 'Vary dip width'
       XP0 = PLWDTH(JP)
       PLWDTH(JP) = (1. + PARFAC) * PLWDTH(JP)
       CALL SET_CELLS (KP,NPLT,NLYR,MXAB,MXB,CELLW,THK,PLNGTH,PLWDTH,NA,NB,DA,DB, &
                       XCNTR,YCNTR,PLTOP,PLAZM,PLDIP,PLUNJ,XCELL,YCELL,ZCELL,PCNR)
       CALL GET_FWD_MODL(LP,PARFAC)
       PLWDTH(JP) = XP0
     END IF
     LP = K0 + 5
     IF (CXPAR(LP) /= 1) THEN
       WRITE(*,'(T3,A)') 'Vary PRP east coordinate'
       XP0 = YCNTR(JP)
       YCNTR(JP) = YCNTR(JP) + DELXY
       CALL SET_CELLS (KP,NPLT,NLYR,MXAB,MXB,CELLW,THK,PLNGTH,PLWDTH,NA,NB,DA,DB, &
                       XCNTR,YCNTR,PLTOP,PLAZM,PLDIP,PLUNJ,XCELL,YCELL,ZCELL,PCNR)
       CALL GET_FWD_MODL(LP,PARFAC)
       YCNTR(JP) = XP0
     END IF
     LP = K0 + 6
     IF (CXPAR(LP) /= 1) THEN
       WRITE(*,'(T3,A)') 'Vary PRP north coordinate'
       XP0 = XCNTR(JP)
       XCNTR(JP) = XCNTR(JP) + DELXY
       CALL SET_CELLS (KP,NPLT,NLYR,MXAB,MXB,CELLW,THK,PLNGTH,PLWDTH,NA,NB,DA,DB, &
                       XCNTR,YCNTR,PLTOP,PLAZM,PLDIP,PLUNJ,XCELL,YCELL,ZCELL,PCNR)
       CALL GET_FWD_MODL(LP,PARFAC)
       XCNTR(JP) = XP0
     END IF
     LP = K0 + 7
     IF (CXPAR(LP) /= 1) THEN
       WRITE(*,'(T3,A)') 'Vary dip azimuth'
       XP0 = PLAZM(JP)
       PLAZM(JP) = PLAZM(JP) + DELPHI
       CALL SET_CELLS (KP,NPLT,NLYR,MXAB,MXB,CELLW,THK,PLNGTH,PLWDTH,NA,NB,DA,DB, &
                       XCNTR,YCNTR,PLTOP,PLAZM,PLDIP,PLUNJ,XCELL,YCELL,ZCELL,PCNR)
       CALL GET_FWD_MODL(LP,PARFAC)
       PLAZM(JP) = XP0
     END IF
     LP = K0 + 8
     IF (CXPAR(LP) /= 1) THEN
       WRITE(*,'(T3,A)') 'Vary dip'
       XP0 = PLDIP(JP)
       PLDIP(JP) = PLDIP(JP) + DELPHI
       CALL SET_CELLS (KP,NPLT,NLYR,MXAB,MXB,CELLW,THK,PLNGTH,PLWDTH,NA,NB,DA,DB, &
                       XCNTR,YCNTR,PLTOP,PLAZM,PLDIP,PLUNJ,XCELL,YCELL,ZCELL,PCNR)
       CALL GET_FWD_MODL(LP,PARFAC)
       PLDIP(JP) = XP0
     END IF
     LP = K0 + 9
     IF (CXPAR(LP) /= 1) THEN
       WRITE(*,'(T3,A)') 'Vary plunge'
       XP0 = PLUNJ(JP)
       PLUNJ(JP) = PLUNJ(JP) + DELPHI
       CALL SET_CELLS (KP,NPLT,NLYR,MXAB,MXB,CELLW,THK,PLNGTH,PLWDTH,NA,NB,DA,DB, &
                       XCNTR,YCNTR,PLTOP,PLAZM,PLDIP,PLUNJ,XCELL,YCELL,ZCELL,PCNR)
       CALL GET_FWD_MODL(LP,PARFAC)
       PLUNJ(JP) = XP0
     END IF

     CALL SET_CELLS (KP,NPLT,NLYR,MXAB,MXB,CELLW,THK,PLNGTH,PLWDTH,NA,NB,DA,DB, &
                     XCNTR,YCNTR,PLTOP,PLAZM,PLDIP,PLUNJ,XCELL,YCELL,ZCELL,PCNR)
   END DO

   K0 = 9*NPLT
   WRITE(*,'(2X)')
   DO JL = 1,NLYR

     LP = K0 + JL
     IF (CXPAR(LP) /= 1) THEN
       IF (JL == NLYR) THEN
         WRITE(*,'(T3,A,I2)') 'Vary resistivity of basement'
       ELSE
         WRITE(*,'(T3,A,I2)') 'Vary resistivity of layer',JL
       END IF
       XP0 = RES(JL)
       RES(JL) = (1. + PARFAC) * RES(JL)
       CALL GET_FWD_MODL(LP,PARFAC)
       RES(JL) = XP0
     END IF
   END DO
   WRITE(*,'(2X)')
   DO JL = 1,NLYR-1
     LP = K0 + NLYR + JL
     IF (CXPAR(LP) /= 1) THEN
       WRITE(*,'(T3,A,I2)') 'Vary thickness of layer',JL
       XP0 = THK(JL)
       THK(JL) = (1. + PARFAC) * THK(JL)
       CALL SET_CELLS (KP,NPLT,NLYR,MXAB,MXB,CELLW,THK,PLNGTH,PLWDTH,NA,NB,DA,DB, &
                       XCNTR,YCNTR,PLTOP,PLAZM,PLDIP,PLUNJ,XCELL,YCELL,ZCELL,PCNR)
       CALL GET_FWD_MODL(LP,PARFAC)
       THK(JL) = XP0
     END IF
   END DO
 END IF

 CONTAINS

   SUBROUTINE GET_FWD_MODL(LP,PARFAC)
!  ----------------------------------

!***  Called by: FORJAC
!***      Calls: LEROI_3D, TEM_3D, HSBOSS_TD, HSBOSS_FD

!  If LP = 0, performs model computation using existing parameters
!  If LP > 0, performs model computation where parameter LP is multiplied by
!             PARFAC and Jacobian column LP isconstructed.

 INTEGER LP
 REAL PARFAC,CSTX(NRXST),SNTX(NRXST)
 LOGICAL INVERT
 DATA INVERT /.TRUE./

 CALL LEROI_3D (TDFD,NFRQ,FREQ,NLYR,THK,RES,RMU,REPS,CALF,CTAU,CFREQ,NPLT,MXAB,  &
                MXB,NB,NA,DA,DB,XCELL,YCELL,ZCELL,PLTOP,PLWDTH,PLNGTH,XCNTR,YCNTR, &
                PLAZM,PLDIP,SIG_T,CALFP,CTAUP,CFREQP,NSTAT,FANGLE,SX,SY,SZ,TXCLN,  &
                TXA90,SAME_TX,NRX,NRXST,RX,RY,RZ,INVERT,BFD_SCAT,NW)


 IF (TDFD < 2) THEN

!       BTD(JT,JS,JC) - total field for channel JT, source position JS,component JC
!  BTD_SCAT(JT,JS,JC) - scattered field: units = nT or nT/s for STEP = 1 or 0 respectively.
!          JC = 1 => in-line component; 2 => transverse component;  3 => vertical component



   CALL TDEM_3D (STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL, &
                 TOPN,TCLS,FREQ,NFRQ,NSTAT,BFD_SCAT,GSTRP,ASTRP,BTD_SCAT)


   CALL HSBOSS_TD (STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL,TOPN, &
                   TCLS,TXCLN,NSTAT,SAME_TX,SZ,ZRX,XRX,YRX,NLYR,RES,REPS,RMU,THK, &
                   CALF,CTAU,CFREQ,GSTRP,ASTRP,BTD)

   BTD = BTD + BTD_SCAT

   IF (CMP == 13 .OR. CMP == 2 .OR. CMP == 3) THEN
     DO JS = 1,NSTAT
       KS1 = (JS - 1) * MCHNL + 1
       KS2 = KS1 + NCHNL -1
       XMODL(KS1:KS2) = NORM(3) * BTD(1:NCHNL,JS,3)
     END DO
     IF (CMP == 2 .OR. CMP == 3) THEN
       DO JS = 1,NSTAT
         KS1 = (JS - 1) * MCHNL + NCHNL + 1
         KS2 = KS1 + NCHNL -1
         XMODL(KS1:KS2) = NORM(1) * BTD(1:NCHNL,JS,1)
       END DO
       IF (CMP == 3) THEN
         DO JS = 1,NSTAT
           KS1 = (JS - 1) * MCHNL + 2*NCHNL + 1
           KS2 = KS1 + NCHNL -1
           XMODL(KS1:KS2) = NORM(2) * BTD(1:NCHNL,JS,2)
         END DO
       END IF
     END IF
   ELSE IF (CMP == 11) THEN
     DO JS = 1,NSTAT
       KS1 = (JS - 1) * MCHNL + 1
       KS2 = KS1 + NCHNL -1
       XMODL(KS1:KS2) = NORM(1) * BTD(1:NCHNL,JS,1)
     END DO
   ELSE IF (CMP == 4 .OR. CMP == 42) THEN
     DO JS = 1,NSTAT
       KS1 = (JS - 1) * MCHNL
       DO JT = 1,NCHNL
         X2 = BTD(JT,JS,1)**2 + BTD(JT,JS,3)**2
         X3 = X2 + BTD(JT,JS,2)**2
         IF (CMP == 42) THEN
           XMODL(JT + KS1) = SQRT (X2)
         ELSE
           XMODL(JT + KS1) = SQRT (X3)
         END IF
       END DO
     END DO
   END IF

 ELSE

   CALL HSBOSS_FD (NFRQ,FREQ,TXCLN,TXA90,NSTAT,SAME_TX,SZ,ZRX,XRX, &
                   YRX,NLYR,RES,REPS,RMU,THK,CALF,CTAU,CFREQ,BFD)

   BFD = BFD - BFD_SCAT  !  Redefine BFD as the total response,

!      The "-" sign is a consequence of using the sine transform for a +iwt
!      sign convention.  It is thus consistant with the convention used for the
!      layered half space and in TDEM for time-domain scattered fields.

!  Store maximally coupled components in XMODL

   CSTX(1:NFRQ) = COS (TXCLN(1:NFRQ))
   SNTX(1:NFRQ) = SIN (TXCLN(1:NFRQ))

   DO JS = 1,NSTAT
     KS1 = (JS - 1) * 2*NFRQ
     DO JF = 1,NFRQ
       IF (TXA90) THEN
         XBFD = NORM(JF) * BFD(JF,JS,2)
       ELSE
         XBFD = NORM(JF) * (BFD(JF,JS,1) * SNTX(JF) + BFD(JF,JS,3) * CSTX(JF))
       END IF
       JD = JF + KS1
       XMODL(JD)        =  REAL (XBFD)
       XMODL(JD + NFRQ) = AIMAG (XBFD)
     END DO
   END DO
 END IF

 IF (LP > 0) THEN
   DO JD = 1, NDATA
     VM = XMODL0(JD)
     VJ = XMODL(JD)
     DELTA = XWTS(JD) * (VJ - VM)
     IF (INRM == 1) THEN
       DENOM = SQRT ((VM**2 + VJ**2)/ 2.)
       IF (ABS(DELTA) > 1.E-8 * DENOM) A(JD,LP) = DELTA / (PARFAC * DENOM)
     ELSE IF (INRM == 2) THEN
       IF (ABS(DELTA) > 1.E-8 * DNORM(JD)) A(JD,LP) = DELTA / (PARFAC * DNORM(JD))
     END IF
   END DO
 END IF

   END SUBROUTINE GET_FWD_MODL

 END SUBROUTINE FORJAC

 SUBROUTINE NLSQ2 (NW,NP,MV1PRT,OUTPRT,MAXITS,CNVRG,PCTCNV,NDATA,DNORM,XDATA,XMODL,XWTS,NPAR,   &
                   CXPAR,ELAS,LBND,UBND,TDFD,CMP,INRM,KNRM,NORM,STEP,IDER,NSX,SWX,SWY,NTYRP,TRP, &
                   NPULS,PULSE,NTYPLS,MCHNL,NCHNL,TOPN,TCLS,GSTRP,ASTRP,NFRQ,FREQ,TXCLN,TXA90,   &
                   NRX,NRXST,XRX,YRX,ZRX,NSTAT,SX,SY,SZ,FANGLE,SAME_TX,RX,RY,RZ,D_NORTH,D_EAST,  &
                   GND_LVL,TITLE,LINE,INV_FAIL,                                                  &
                   NLYR,RES,THK,RMU,REPS,CALF,CTAU,CFREQ,NPLT,MXB,MXAB,CELLW,SIG_T,XCNTR,        &
                   YCNTR,PLTOP,PLNGTH,PLWDTH,PLAZM,PLDIP,PLUNJ,CALFP,CTAUP,CFREQP,PARPCT)
!------------------------------------------------------------------------------------------------

!***  Called by: MAIN
!***      Calls: CHK_PLTOP, CNVRT_BOUNDS, CNVRT2_XPAR, ESVD, FORJAC, INDEX_MPAR,
!                NOISE_2_SIGR, PARAMETER_SENSITIVITY, SOLVE2, WRITE_INVMDL,
!                WRITE_MDATA, WRITE_MISFIT

!  SVD non-linear least square inversion using a modified
!  Gauss-Newton-Marquardt method as described in Jupp & Vozoff, 1975,
!  Stable iterative methods for the inversion of geophysical data,
!  Geophys. J. R. Astr. Soc.,42

!  New convergence criteria based on pre-specified RMS threshold
!  added December, 1998
!  New convention: Dec, 2003: VERR is now VD - VM
!  Thus DELPAR is now added rather than subtracted during updates.
!=========================================================================

!***  Called by: MAIN
!***      Calls: ESVD, FORJAC, WRITE_MODEL, WRITE_MISFIT, SOLVE2
!
!             General Inversion Input Variables
!             ---------------------------------
!
!     NW             - verbose output unit number
!     NP            = plot file unit number
!     MV1PRT & OUTPRT are print options for the MV1 & OUT files respectively.
!                    =  0  No output DURING inversion.  The final model set AFTER inversion,
!                          but NOT the final model data, is written to output files.
!                   
!                    =  1  as above plue plus final model data
!                   
!                    =  2  as above plus intermediate model sets after each iteration
!                   
!                    =  3 as above plus intermediate model data after each iteration
!
!     MAXITS         - maximum permitted iterations
!     CNVRG          = 1 => converge on predicted decreaseetc
!                    = 2=> stop when RMS error < PCTCNV
!                    = 3=> PARFAC starts at 0.03 and tests 0.06
!                    = 4=> user will set PARFAC = 100. * PCTPAR
!     NDATA          = NCMP * NSTAT * NCHNL or 2 * NFRQ * NSTAT
!                    = the number of rows of the Jacobian matrix
!     DNORM(1:NDATA) : L1 norm of data for each channel or frequency, averaged over the survey
!     INRM           = 1 : point symmetric norm used for fitting error and sensitivity matrix
!                    = 2 : survey norm used for fitting error and sensitivity matrix
!     XDATA          - Array of survey data points to be inverted.
!     XMODL          - Array of model data points.
!     XWTS           - Array of weights for XDATA (integer 0 or 1)
!     NPAR           = 2*NLYR -1 + NPLT * 9 = number of parameters
!     XPAR           - On call, XPAR contains the log (initial parameter estimates).
!                      On exit it contains the final values.
!     CXPAR          = 0 => parameter is completely free to vary as dictated by inversion step
!                    = 1 => parameter is fixed
!                    = 2 => parameter is constrained by elasticity.
!                    = 3 => parameter bounds are buffered.
!     ELAS           - [0 to 1] fraction of proposed step actually taken.
!     LBND, UBND     - lower & upper bound of parameter
!     INV_FAIL       = 1 for inversion algorithm failure
!
!             AEM System Input Variables
!             --------------------------
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
!     IDER   = 1 if source waveform was dB/dt; = 0 if amps pr B
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
!     GSTRP  = 1 => apply Questem-Geotem stripping algorithm
!     ASTRP  = 1 => apply Aerotem stripping algorithm
!     FREQ   = array of NFRQ frequencies
!     TXCLN  - angle in radians that TX dipole makes with vertical (climb = +)
!     TXA90  - true for vertical co-planar briadside array
!     NRX    - number of receivers = NFRQ in FD; = 1 in TD
!     NRXST  - dimension for receiver offset & transmitter tilt
!            = NFRQ in FD;  = NSTAT in TD
!     ZRX    - vertical receiver offset for each frequency;   below = positive
!     XRX    - in-line receiver offset for each frequency;    behind = positive
!     YRX    - transverse receiver offset for each frequency; left = positive.
!
!             AEM Survey Input Variables
!             --------------------------
!
!     NSTAT      - number of stations in survey line.
!     FANGLE     - flight path angle in radians. (North = 0; East = PI/2)
!     SAME_TX    - used to reduce redundant computations
!     SX, SY, SZ : north, east and altitude (re gnd level) of transmitter
!     RX, RY, RZ : north, east and altitude (re gnd level) of receiver(s)
!
!     D_NORTH    - north offset from real world to model coordinate
!     D_EAST     - east offset from real world to model coordinate
!     GND_LVL    - ground level
!     LINE       - line number
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
!     NPLT   - Number of plates
!     SIG_T  = plate conductances
!     XCNTR  = north coordinates of plate reference points
!     YCNTR  =  east coordinates of plate reference points
!     PLTOP  = depth to reference point on top edge of plate
!     PLNGTH = plate strike lengths
!     PLWDTH = plate widths along dip
!     PLAZM  = plate azimuths in radians (0 = north)
!     PLDIP  = plate dip angle in radians
!     PLUNJ  = plate plunge angle in radians
!     CELLW  - maximum cell dimension
!     MXAB   - Number of cells in biggest plate
!     MXB    - Maximim number of cell rows
!     CALF, CTAU & CFREQ are the layered earth Cole-Cole parameters.
!     CALFP, CTAUP & CFREQP are the plate Cole-Cole parameters.
!
!         Other Stuff
!         -----------
!
!     A        -  Jacobian matrix
!     WSP      - working space.
!     VERR     - error vector
!     SUMSQ(1) - sum squared scaled error for point norm
!     SUMSQ(2) - sum squared scaled error for survey norm
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
 INTEGER, PARAMETER :: IP=1, QL=SELECTED_REAL_KIND(p = 18)
 REAL, PARAMETER :: BND=0.01, EXPND=2., RSVT0=0.1, ETA=1.E-7, TOL=.5E-31, ERRP08 = 15.
 INTEGER NW,NP,MV1PRT,OUTPRT,ITS,CNVRG,NDATA,NPAR,CXPAR(NPAR),XWTS(NDATA),TDFD,CMP,KNRM, &
         INRM,STEP,IDER,NSX,NPULS,NTYPLS,NTYRP,NCHNL,MCHNL,GSTRP,ASTRP,NFRQ,NSTAT,NRXST,  &
         NRX,ICNT,MAXITS,NSV,FITS,JP,K0,LINE(NSTAT),ITSR,ITSF,ITSNW
 REAL PCTCNV,A(NDATA,NPAR+1),UMAT(NDATA,NPAR),VMAT(NPAR,NPAR),WSP(3*NPAR),NSR,CELLW,  &
      NORM(KNRM),SWX(NSX),SWY(NSX,3),TRP(NTYRP),PULSE,FREQ(NFRQ),GND_LVL,ZERO,XYNORM, &
      DELT,RSVT,FNM,GCRIT,GTEST,PDRE,SSQNEW(2),SUMSQ(2),PSERR(2),ERR1,ERR1P,WGTSUM,   &
      B1,B2,BMID,DRMS,PARFAC,PFAC(2),PARPCT,DNORM(NDATA),DUM1
 REAL, ALLOCATABLE :: RMSERR(:)
 REAL, DIMENSION(NRXST) :: TXCLN,XRX,YRX,ZRX
 REAL, DIMENSION(NSTAT,NRX) :: RX,RY,RZ
 REAL, DIMENSION(NPAR) :: SV,XPAR,MPAR,DELPAR,GXPAR,GXPAR1,IMPORT,ELAS,LBND,UBND,XLBND,XUBND
 REAL, DIMENSION(NCHNL) :: TOPN,TCLS
 REAL, DIMENSION(NDATA) :: VERR,XMODL,XDATA
 REAL, DIMENSION(NSTAT) :: SX,SY,SZ,FANGLE,SZ0,RZ0
 REAL(KIND=QL) D_NORTH,D_EAST
 REAL(KIND=QL), DIMENSION(NSTAT) :: SXD,SYD,RXD,RYD
 LOGICAL,PARAMETER :: WITHU=.TRUE., WITHV=.TRUE.
 LOGICAL JCBN,TXA90,SAME_TX(NSTAT),INV_FAIL,FINAL
 CHARACTER (LEN=11) PSNRM(2)
 CHARACTER (LEN=120) CVAR,TITLE
 DATA PSNRM /'Point-norm','Survey-norm'/

! Model Specific
 INTEGER NLYR,NPLT,MXAB,MXB
 Integer :: jd
 REAL, DIMENSION(NLYR) :: THK,RES,REPS,RMU,CALF,CTAU,CFREQ
 REAL, DIMENSION(NPLT) :: SIG_T,PLTOP,PLWDTH,XCNTR,YCNTR,PLNGTH,PLAZM,PLDIP,PLUNJ, &
                          CALFP,CTAUP,CFREQP

!  Preset threshold parameters and index workspace, but return if problem
!  size in error.

 ALLOCATE (RMSERR(MAXITS))
 ZERO =BND * BND
 IF (ZERO < ETA) ZERO = ETA
 GCRIT = SQRT(ETA)

 A = 0.      !  Initialise arrays
 VERR = 0.
 IMPORT = 0.
 INV_FAIL = .FALSE.
 FINAL = .FALSE.

 WGTSUM = REAL (SUM (XWTS))
 ITS = 0
 RSVT = MAX (BND, RSVT0)   ! Initialise eigenvalue damping at 10 percent
 WRITE(NW,'(/T3,A)') TRIM (TITLE)
 WRITE(*,'(/T3,A)') TRIM (TITLE)
 IF (INRM == 1) THEN
   WRITE(NW,26); WRITE(*,26)
 ELSE IF (INRM == 2) THEN
   WRITE(NW,25); WRITE(*,25)
 END IF
 IF (TDFD == 2) THEN
   WRITE(NW,1);  WRITE(*,1)
 ELSE
   IF (CMP == 11) CVAR = 'in-line component'
   IF (CMP == 13) CVAR = 'vertical component'
   IF (CMP == 2)  CVAR = 'joint vertical and in-line components'
   IF (CMP == 3)  CVAR = 'joint vertical, in-line and transverse components'
   IF (CMP == 4)  CVAR = 'total field using all 3 components'

   WRITE(NW,2) TRIM (CVAR); WRITE(*,2) TRIM(CVAR)
 END IF
 WRITE(NW,3) MAXITS;  WRITE(*,3) MAXITS

!----------------------------------------------------------------------
!  Start of main loop.  Call FORJAC to get error and Jacobian.
!  First, tranform physical model into transformed parameters.
!  Call ESVD to find the S.V.D. of the Jacobian.
!----------------------------------------------------------------------

 XYNORM = (SUM (SZ) + SUM(RZ) )/ REAL (NSTAT)
 CALL CNVRT2_XPAR (NLYR,NPLT,NPAR,RES,THK,SIG_T,PLTOP,PLNGTH,PLWDTH, &
                   XCNTR,YCNTR,XYNORM,PLAZM,PLDIP,PLUNJ,XPAR)
 CALL CNVRT_BOUNDS (NPLT,NLYR,NPAR,LBND,UBND,XYNORM,CXPAR,XLBND,XUBND)

 DRMS = 0.
 ITSR = 0
 IF (CNVRG == 4) THEN         ! Derivartive step fixed at initial PARFAC
   PARFAC = 0.01 * PARPCT
   PFAC = PARFAC
   ITSF = 0                   ! No PARFAC tests permitted
   WRITE(NW,20) PARFAC ; WRITE(*, 20) PARFAC
 ELSE
   ITSF = MAXITS
   PFAC(1) = 0.06
   PFAC(2) = 0.03
   IF (CNVRG == 3) THEN
     PFAC(1) = 0.03
     PFAC(2) = 0.06
   END IF

   PARFAC = PFAC(1)
   WRITE(NW,21) PARFAC ; WRITE(*, 21) PARFAC
 END IF

 ITER_LOOP: DO ITS = 1, MAXITS

   ITSNW = ITS
   IF (ITSF == 1) ITSNW = ITS - 1  ! Account for testing PARFAC - no jump in actual iteration
   WRITE(*,'(/T3,A,I3)') 'Begin iteration',ITSNW

   JCBN = .TRUE.
   CALL FORJAC (NDATA,INRM,DNORM,XDATA,XMODL,XWTS,NPAR,XPAR,CXPAR,SUMSQ,JCBN,A,VERR,TDFD, &
                CMP,STEP,IDER,NSX,SWX,SWY,NTYRP,TRP,NPULS,PULSE,NTYPLS,MCHNL,NCHNL,TOPN,  &
                TCLS,GSTRP,ASTRP,NFRQ,FREQ,KNRM,NORM,TXCLN,TXA90,NRX,XRX,YRX,ZRX,NRXST,   &
                NSTAT,SX,SY,SZ,FANGLE,SAME_TX,RX,RY,RZ,NLYR,RMU,REPS,CALF,CTAU,CFREQ,     &
                XYNORM,PARFAC,NPLT,CELLW,MXAB,MXB,CALFP,CTAUP,CFREQP,NW)

   PSERR(1:2) = 100. * SQRT (SUMSQ(1:2) / WGTSUM)
   RMSERR(ITS) = PSERR(INRM)
   IF (ITS == 1) THEN
     FITS = 0
     CALL INDEX_MPAR
     IF (MV1PRT > 1) THEN
       WRITE(NP,30) FITS,RMSERR(ITS),RSVT
       WRITE(NP,31) FITS,MPAR(1:NPAR)
     END IF
     IF (OUTPRT > 1) WRITE(NW,4) TRIM (PSNRM(INRM)),RMSERR(ITS),RSVT
     IF (MV1PRT == 3) CALL WRITE_MDATA(FITS)
     IF (OUTPRT == 3)  &
       CALL WRITE_MISFIT (NW,FITS,NSTAT,NDATA,MCHNL,TDFD,NFRQ,FREQ,CMP,NCHNL,VERR,XMODL,XDATA)
     WRITE(*,4) TRIM (PSNRM(INRM)),RMSERR(ITS),RSVT
   END IF
   FNM = 0.01 * SQRT (SUMSQ(INRM))
   FNM = MAX (FNM, ETA)

!  Load the error vector into the NPAR+1 column of A.  On return from ESVD,
!  this column will contain the transformed error vector; i.e.,
!  VERR = U * VERR

   A(1:NDATA,NPAR+1) = VERR(1:NDATA)
   WRITE(*,'(/T3,A)') 'Compute SVD'
   CALL ESVD (A,NDATA,NPAR,IP,WITHU,WITHV,SV,UMAT,VMAT,WSP,ETA,TOL,NW)

   IF (ABS (SV(1)) < 1.E-30) THEN
     INV_FAIL = .TRUE.
     WRITE(NW,99); WRITE(*,99)
     RETURN
   END IF

   VERR(1:NDATA) = A(1:NDATA,NPAR+1)

!  Solve for the correction vector, and test for convergence on
!  predicted decrease.  Loop over internal iterations.

   WRITE(*,'(/T3,A/)') 'Solve for new parameters & test convergence.'
   ICNT_LOOP: DO ICNT = 0, MAXITS
     CALL SOLVE2 (NPAR,NSV,RSVT,ZERO,VMAT,VERR,SV,DELPAR,WSP,PDRE)
     DELT = SQRT (PDRE)

! If the predicted residual decrease < 1 percent of RMS error,
! terminate iterations.  Inversion won't improve.

     IF (DELT < FNM) THEN
       WRITE(NW,5);  WRITE(*,5)
       WRITE(NW,6) ITSNW,TRIM (PSNRM(INRM)),RMSERR(ITS),RSVT
       WRITE(*,6) ITSNW,TRIM (PSNRM(INRM)),RMSERR(ITS),RSVT
       IF (PARFAC < 0.07 .AND. RMSERR(ITS) > ERRP08) THEN
         PARFAC = 0.08
         WRITE(NW,19) PARFAC; WRITE(*,19) PARFAC
         CYCLE ITER_LOOP
       END IF
       IF (CNVRG == 1 .AND. RMSERR(ITS) > 15.) WRITE(NW,7)
       IF (CNVRG == 2 .AND. RMSERR(ITS) > PCTCNV) WRITE(NW,8) PCTCNV
       EXIT ITER_LOOP
     END IF

     DO JP = 1,NPAR
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
         B2 = XPAR(JP) + ELAS(JP) * (B2 - XPAR(JP))
         B1 = XPAR(JP) + ELAS(JP) * (B1 - XPAR(JP))
         GXPAR(JP) = XPAR(JP) + DELPAR(JP)
         GXPAR(JP) = MIN (GXPAR(JP), B2)
         GXPAR(JP) = MAX (GXPAR(JP), B1)
       END SELECT
     END DO
     CALL CHK_PLTOP

!  Get the error for model with corrected parameters.
!  Test for improvement (decrease) in residual with Goldstein condition
!  on ratio of actual to computed decrease in error.  If it fails, reduce
!  step and try again.  Give up and return after MAXITS. "internal" iterations.

     JCBN = .FALSE.
     CALL FORJAC (NDATA,INRM,DNORM,XDATA,XMODL,XWTS,NPAR,GXPAR,CXPAR,SSQNEW,JCBN,A,VERR,   &
                  TDFD,CMP,STEP,IDER,NSX,SWX,SWY,NTYRP,TRP,NPULS,PULSE,NTYPLS,MCHNL,NCHNL, &
                  TOPN,TCLS,GSTRP,ASTRP,NFRQ,FREQ,KNRM,NORM,TXCLN,TXA90,NRX,XRX,YRX,ZRX,   &
                  NRXST,NSTAT,SX,SY,SZ,FANGLE,SAME_TX,RX,RY,RZ,NLYR,RMU,REPS,CALF,CTAU,    &
                  CFREQ,XYNORM,PARFAC,NPLT,CELLW,MXAB,MXB,CALFP,CTAUP,CFREQP,NW)

     IF (ITS < ITSF) THEN
       IF (ITS > 5 .OR. RSVT < 0.012) THEN
         GXPAR1(1:NPAR) = GXPAR(1:NPAR)
         ERR1 = SSQNEW(INRM)
         ERR1P = 100. * SQRT (SSQNEW(INRM) / WGTSUM)
         DUM1 = 100. * SQRT (SSQNEW(3-INRM) / WGTSUM)
         WRITE(NW,16) ITSNW,PARFAC,ERR1P ; WRITE(*, 16) ITSNW,PARFAC,ERR1P
         WRITE(NW,17) PFAC(2) ; WRITE(*, 17) PFAC(2)
         PARFAC = PFAC(2)
         ITSF = 1
         ITSR = ITS + 1
         CYCLE ITER_LOOP
       END IF
     END IF
     IF (ITS == ITSR) THEN
       ERR1P = 100. * SQRT (SSQNEW(INRM) / WGTSUM)
       WRITE(NW,16) ITSNW,PARFAC,ERR1P;  WRITE(*, 16) ITSNW,PARFAC,ERR1P
       ITSR = 1
       IF (SSQNEW(INRM) > ERR1) THEN
         SSQNEW(INRM) = ERR1
         SSQNEW(3-INRM) = DUM1
         PARFAC = PFAC(1)
         GXPAR(1:NPAR) = GXPAR1(1:NPAR)
       END IF
       WRITE(NW,18) PARFAC ;  WRITE(*, 18) PARFAC
     END IF

     GTEST = SUMSQ(INRM) - SSQNEW(INRM)

     IF (GTEST > GCRIT*PDRE) THEN   !  Error reduced using smaller step
       IF (ICNT == 0) THEN
         RSVT = RSVT / EXPND        !  Decrease eigenvalue threshold damping
         RSVT = MAX (BND, RSVT)
       END IF
       EXIT ICNT_LOOP               !  Start next iteration
     END IF
     RSVT = RSVT * EXPND            !  No error decrease. Raise threshold
     IF (ICNT == MAXITS)  THEN
       WRITE(NW,9) ICNT             !  No improvement possible.  Maybe another starting guess?
         IF (PARFAC < 0.07 .AND. RMSERR(ITS) > ERRP08) THEN
         PARFAC = 0.08
         WRITE(NW,19) PARFAC; WRITE(*,19) PARFAC
         CYCLE ITER_LOOP
       END IF
       EXIT ITER_LOOP
     END IF
   END DO ICNT_LOOP

!  Error reduced.  Accept step, write out summary, and test convergence.

   XPAR(1:NPAR) = GXPAR(1:NPAR)
   DELT = SQRT(GTEST)
   SUMSQ = SSQNEW
   PSERR(1:2) = 100. * SQRT (SUMSQ(1:2) / WGTSUM)
   RMSERR(ITS) = PSERR(INRM)
   FNM = 0.01 * SQRT(SUMSQ(INRM))
   FNM = MAX( FNM, ETA)

!  If the predicted residual decrease < 1 percent of RMS error,
!  If the error decrease from the new iteration < 1 percent of the previous error,
!  claim convergence and terminate iterations because inversion won't improve.

!  Else, write out the current model and continue iterating up until ITS = MAXITS.

   IF (DELT < FNM) THEN
     WRITE(NW,5);  WRITE(*,5)
     WRITE(NW,6) ITSNW,TRIM (PSNRM(INRM)),RMSERR(ITS),RSVT 
     WRITE(*,6) ITSNW,TRIM (PSNRM(INRM)),RMSERR(ITS),RSVT
     IF (PARFAC < 0.07 .AND. RMSERR(ITS) > ERRP08) THEN
       PARFAC = 0.08
       WRITE(NW,19) PARFAC; WRITE(*,19) PARFAC
       CYCLE ITER_LOOP
     END IF
     IF (CNVRG == 1 .AND. RMSERR(ITS) > 15.) WRITE(NW,7)
     IF (CNVRG == 2 .AND. RMSERR(ITS) > PCTCNV) WRITE(NW,8) PCTCNV
     EXIT ITER_LOOP

   END IF
   IF (RMSERR(ITS) < PCTCNV) THEN
     WRITE(NW,10) PCTCNV
     WRITE(NW,6) ITSNW,TRIM (PSNRM(INRM)),RMSERR(ITS),RSVT
     WRITE(*,6) ITSNW,TRIM (PSNRM(INRM)),RMSERR(ITS),RSVT
     EXIT ITER_LOOP
   END IF
   IF (ITS > 3) DRMS = RMSERR(ITS-2) - RMSERR(ITS)
   IF (ITS == MAXITS) THEN
     WRITE(NW,11)
     WRITE(NW,6) ITSNW,TRIM (PSNRM(INRM)),RMSERR(ITS),RSVT 
     WRITE(*,6) ITSNW,TRIM (PSNRM(INRM)),RMSERR(ITS),RSVT
     IF (DRMS > 1.) THEN
       WRITE(NW,14) DRMS; WRITE(*,14) DRMS
     END IF
     EXIT ITER_LOOP
   END IF

   CALL INDEX_MPAR
   WRITE(NP,30) ITSNW,RMSERR(ITS),RSVT
   IF (MV1PRT > 1) WRITE(NP,31) ITSNW,MPAR(1:NPAR)
   IF (MV1PRT > 2) CALL WRITE_MDATA(ITSNW)

   WRITE(*,6) ITSNW,TRIM (PSNRM(INRM)),RMSERR(ITS),RSVT
   IF (OUTPRT > 0) WRITE(NW,6) ITSNW,TRIM (PSNRM(INRM)),RMSERR(ITS),RSVT
   IF (OUTPRT > 1) CALL WRITE_INVMDL (NW,FINAL,ITSNW,NLYR,NPLT,NPAR,CXPAR,XPAR,XYNORM, &
                                      IMPORT,D_NORTH,D_EAST,GND_LVL)
   IF( OUTPRT == 3 .AND. ITS < MAXITS)  &
       CALL WRITE_MISFIT (NW,ITS,NSTAT,NDATA,MCHNL,TDFD,NFRQ,FREQ,CMP,NCHNL,VERR,XMODL,XDATA)

   IF (ITS > 10 .AND. DRMS < 1.) THEN
     WRITE(NW,15) ITSNW; WRITE(*,15) ITSNW
     IF (PARFAC < 0.07 .AND. RMSERR(ITS) > ERRP08) THEN
       PARFAC = 0.08
       WRITE(NW,19) PARFAC; WRITE(*,19) PARFAC
       CYCLE ITER_LOOP
     END IF
     EXIT ITER_LOOP
   END IF
   CLOSE (NP)                                                            ! Flush Buffer
   OPEN(NP,FILE = 'LeroiAir.mv1',STATUS = 'OLD', POSITION = 'APPEND')
 END DO ITER_LOOP   !  END OF MAIN LOOP.  Write final model and exit.

 CALL PARAMETER_SENSITIVITY (NPAR,VMAT,SV,BND,IMPORT)
 CALL NOISE_2_SIGR (NPAR,NDATA,XMODL,XDATA,XWTS,NSR)

 WRITE(NW,12)
 FITS = -1
 WRITE(NP,32) ITSNW,RMSERR(ITS),NSR,RSVT
 CALL INDEX_MPAR
 WRITE(NP,33) MPAR(1:NPAR)
 WRITE(NP,34) IMPORT(1:NPAR)
 Do jd = 1, NDATA
     Write (NP, 35) jd, a(jd, 1:npar)
 End Do
 IF (MV1PRT > 0) CALL WRITE_MDATA(FITS)


 FINAL = .TRUE.
 CALL WRITE_INVMDL (NW,FINAL,ITSNW,NLYR,NPLT,NPAR,CXPAR,XPAR,XYNORM, &
                   IMPORT,D_NORTH,D_EAST,GND_LVL)


 WRITE(NW,13) TRIM (PSNRM(INRM)),RMSERR(ITS),NSR
 IF (INRM == 2) THEN
   WRITE(NW,25); WRITE(*,25)
 ELSE
   WRITE(NW,26); WRITE(*,26)
 END IF

 FITS = -1
 SXD = REAL (SX,QL) + D_NORTH
 SYD = REAL (SY,QL) + D_EAST
 SZ0 = SZ + GND_LVL
 IF (OUTPRT > 0) &
 CALL WRITE_MISFIT (NW,FITS,NSTAT,NDATA,MCHNL,TDFD,NFRQ,FREQ,CMP,NCHNL,VERR,XMODL,XDATA)

 1 FORMAT(/T3,'Begin frequency-domain inversion')
 2 FORMAT(/T3,'Begin time-domain inversion on ',A)
 3 FORMAT(/T3,'Maximum iterations =',I3)
 4 FORMAT(/T3,'Initial ',A,' error =',F8.2,' percent.' &
          /T3,'Initial Relative SV threshold =',F8.3)
 5 FORMAT(//T3,'Convergence on predicted decrease')
 6 FORMAT(/I4,' Iterations completed.' &
          /T6,A,' rms error =',F8.2,' percent.' &
          /T6,'Relative SV threshold =',F8.3)
 7 FORMAT(/T3,'An alternative starting guess might achieve better results.')
 8 FORMAT(/T3,'The inversion was unable to achieve an RMS error' &
          /T3,'within the specified threshold of',F9.2,' percent.' &
          /T3,'An alternative starting guess may achieve better results.')
 9 FORMAT(/T3,'The solution is trapped.  ICNT = ',I2/ &
          /T3,'Another starting guess may yield a better result.')
 10 FORMAT(/T3,'Convergence within RMS error threshold of',F7.2,' has been achieved.')
 11 FORMAT(//T3,'Inversion finished after maximum number of iterations')
 12 FORMAT(//T3,50('='))
 13 FORMAT(/T23,A,' rms error =',F8.2,' percent.' &
           /T22,'Noise to signal ratio =',F8.3)
 14 FORMAT(/T3,'------------------------------------------------------------------' &
           /T3,'The reduction in the RMS error during the last two iterations was ' &
           /T3,F5.2,' percent.  A better result may be achieved by using the final' &
           /T3,'model from this inversion as the starting guess for another run.'   &
           /T3,'__________________________________________________________________')
 15 FORMAT(/T3,'--------------------------------------------------------------------'  &
           /T3,'Inversion terminated after',I3,' iterations because, the reduction in' &
           /T3,'the RMS error from the last two iterations was less than 1 percent.'   &
           /T3,'____________________________________________________________________')
 16 FORMAT(/T3,'ITS =',I3,' : For PARFAC =',F5.2,', RMS error =',G12.4)
 17 FORMAT(/T3,'Test PARFAC =',F5.2)
 18 FORMAT(/T3,'PARFAC = ',F5.2, ' will be used for all remaining iterations.')
 19 FORMAT(/T3,'Test for improved convergence using PARFAC =',F5.2)
 20 FORMAT(/T3,'PARFAC is fixed at',F5.2)
 21 FORMAT(/T3,'Inversion starts with PARFAC =',F5.2)
 25 FORMAT(//T3,'RMS error reduction based on using the Survey norm' &
            /T3,'--------------------------------------------------')
 26 FORMAT(//T3,'RMS error reduction based on symmetric point normalisation'&
            /T3,'----------------------------------------------------------')
 30 FORMAT(T1,'/'/T1,'/ ITERATION  ',I2.2/T1,'/ PERCENT_RMS_ERROR:',F9.2,3X,'RSVT:',F8.3)
 31 FORMAT(T1,'/'/T1,'/ MODEL_',I2.2,84F13.2)
 32 FORMAT(T1,'/'/T1,'/ FINAL_ITERATION  ',I2.2/T1,'/ FINAL_PERCENT_RMS_ERROR:',F8.2,3X,'NSR:',F7.3,3X,'RSVT:',F7.3)
 33 FORMAT(/T1,'/ FINAL_MODEL',84F13.2)
 34 FORMAT(T1,'/ IMPORTANCE:  ',2X,84F6.2)
 35 Format ('/ Jacobian Row ', i6.6, ' : ', 8192(2x, en15.6))

 99 FORMAT(//T3,'Singular value decomposition failure.  INVERSION HALTED')

 CONTAINS

   SUBROUTINE CHK_PLTOP
!  --------------------

!***  Called by NLSQ2

! Ensures that plates don't leave the basement without permission.

 INTEGER JP,KP,KPLTOP
 REAL THICK,PT

 THICK = 0.
 DO JP = 1,NLYR-1
   KP = 9 * NPLT + NLYR + JP
   THICK = EXP (GXPAR(KP)) + THICK  ! Cumulative layer thickness
 END DO

 DO JP = 1, NPLT
   KPLTOP = 9* (JP-1) + 2
   PT = EXP (GXPAR(KPLTOP))
   IF (PT < THICK) PT = THICK 
   GXPAR(KPLTOP) = LOG (PT)
 END DO

   END SUBROUTINE CHK_PLTOP

   SUBROUTINE INDEX_MPAR
!  ---------------------

!***  Called by NLSQ2

 INTEGER KP
 DO JP = 1,NPLT
   KP = 9* (JP-1)
   MPAR(KP+1) = EXP (XPAR(KP+1))               ! SIG_T(JP)
   MPAR(KP+2) = GND_LVL - EXP (XPAR(KP+2))     ! PLTOP(JP) in RL
   MPAR(KP+3) = EXP (XPAR(KP+3))               ! PLNGTH(JP)
   MPAR(KP+4) = EXP (XPAR(KP+4))               ! PLWDTH(JP)
   MPAR(KP+5) = XYNORM * XPAR(KP+5) + REAL (D_EAST)   ! YCNTR(JP)
   MPAR(KP+6) = XYNORM * XPAR(KP+6) + REAL (D_NORTH)  ! XCNTR(JP)
   MPAR(KP+7) = 90. + 180. * XPAR(KP+7)               ! AZM(JP) in degrees = PLAZM + 90
   MPAR(KP+8) = 180. * XPAR(KP+8)              ! DIP(JP) in degrees
   MPAR(KP+9) = 180. * XPAR(KP+9)              ! PLG(JP) in degrees
 END DO
 KP = 9* NPLT
 DO JP = KP+1, NPAR                           ! Layer paameters
   MPAR(JP) = EXP (XPAR(JP))
 END DO

   END SUBROUTINE INDEX_MPAR

   SUBROUTINE WRITE_MDATA(KTS)
!  --------------------------

!***  Called by NLSQ2

 REAL, PARAMETER :: RAD2DEG=180./3.141592654
 REAL, DIMENSION(NRXST) :: TXDEG
 INTEGER JS,KTS
 LOGICAL WL
 CHARACTER (LEN=80) QL0,QL1

 TXDEG = RAD2DEG * TXCLN
 SXD = REAL (SX,QL) + D_NORTH
 SYD = REAL (SY,QL) + D_EAST
 SZ0 = SZ + GND_LVL

 DO JS = 1,NSTAT
   RXD(JS) = REAL (RX(JS,1),QL) + D_NORTH
   RYD(JS) = REAL (RY(JS,1),QL) + D_EAST
   RZ0(JS) = RZ(JS,1) + GND_LVL
   K0 = (JS-1) * MCHNL
   WL = .TRUE.
   IF (JS > 1 .AND. LINE(JS) == LINE(JS-1)) WL = .FALSE.
   IF (WL) THEN
     IF (KTS < 0) THEN
       WRITE(QL0,2) LINE(JS)
     ELSE
       WRITE(QL0,1) LINE(JS),KTS
     END IF
     READ(QL0,'(A)') QL1
     WRITE(NP,3) TRIM (ADJUSTL (QL1))
   END IF
   IF (TDFD < 2) THEN
     WRITE(NP,4) JS,SYD(JS),SXD(JS),SZ0(JS),TXDEG(JS),RYD(JS),RXD(JS),RZ0(JS), &
                  XMODL(K0+1:K0+MCHNL),100.*VERR(K0+1:K0+MCHNL)
   ELSE
     WRITE(NP,5) JS,SYD(JS),SXD(JS),SZ0(JS),RYD(JS),RXD(JS),RZ0(JS), &
                  XMODL(K0+1:K0+MCHNL),100.*VERR(K0+1:K0+MCHNL)
  END IF
 END DO

 1 FORMAT(T2,I10,'_I',I2.2)
 2 FORMAT(T2,I10,'_ZFNL')
 3 FORMAT(T2,'Line ',A)
 4 FORMAT(I5,2F12.1,F8.1,F6.1,2F12.1,F8.1,8192en16.6:)
 5 FORMAT(I5,2F12.1,F8.1,2F12.1,F8.1,8192en16.6:)

   END SUBROUTINE WRITE_MDATA

END SUBROUTINE NLSQ2


 SUBROUTINE NOISE_2_SIGR (NPAR,NDATA,XMODL,XDATA,XWTS,NSR)
!--------------------------------------------------------

!***  Called by NLSQ2

!  Calculates and prints the noise to signal ratio.

!***  Called by: MAIN

!      NPAR - the number of free variables
!     NDATA - the number of data values
!     XMODL - model data in microvolts
!     XDATA - data to be inverted in microvolts
!      XWTS - weights for data points (intger 0 or 1)
!       NSR - noise to signal ratio

 IMPLICIT NONE
 INTEGER NPAR,NDATA,XWTS(NDATA),J1
 REAL, PARAMETER :: TOL=.1E-5
 REAL XMODL(NDATA),XDATA(NDATA),YM(NDATA),YD(NDATA),PK,PKM,BASE,ZM, &
      ZD,YBAR,NSR,CUMM,CUMD,CWMD

 INTENT (IN) NPAR,NDATA,XMODL,XDATA,XWTS
 INTENT (OUT) NSR


!  Set up stitched log representation for model and data voltages.
!  Use a base of 6 orders of magnitude or the data dynamic range,
!  whichever is the lesser.
!  Accumulate means and variances in double precision.

 BASE = ABS (XDATA(1))
 PK = BASE
 DO J1 = 2, NDATA
   BASE = MIN (ABS (XDATA(J1)), BASE)
   PK = MAX (ABS (XDATA(J1)), PK)
 END DO
 PKM = TOL * PK
 BASE = MAX (PKM, BASE)

 CUMM = 0.
 YM = 0.
 YD = 0.
 DO J1 = 1, NDATA
   IF (XWTS(J1) > 0) THEN
     ZM = ABS (XMODL(J1))
     ZD = ABS (XDATA(J1))
     IF (ZM > BASE) THEN
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

 YBAR = CUMM / NDATA

 CUMM = 0.
 CUMD = 0.
 DO J1 = 1, NDATA
   CUMM = CUMM + (YM(J1) - YBAR)**2
   CWMD = XWTS(J1) * (YM(J1) - YD(J1))
   CUMD = CUMD + CWMD**2
 END DO

 CUMM = CUMM / (NDATA - 1)
 CUMD = CUMD / (NDATA - NPAR)

 NSR = SQRT (CUMD / CUMM)

END SUBROUTINE NOISE_2_SIGR

 SUBROUTINE PARAMETER_SENSITIVITY (NPAR,VMAT,SV,BND,IMPORT)
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

 SUBROUTINE SOLVE2 (NPAR,NSV,RSVT,ZERO,VMAT,R,SV,DELPAR,WSP,PDRE)
!----------------------------------------------------------------

!  Calculates the new parameter changes by taking the product of
!  the transformed error vector times the damping factors (second order
!  Marquardt damping) times the inverse singular value matrix.
!  It also calculates the predicted error decrease.

!*** Called by: NLSQ2
!***     Calls: DPROD1

!     NPAR -  the number of unknown parameters
!    * NSV -  the returned number of singular values used in the solution.
!     RSVT -  the relative singular value threshold for the current iteration.
!     ZERO -  rejection level for relative singular values; i.e., truncation
!             instead of damping for RSV less than zero.  in NLSQ2, ZERO is
!             set to the square of the minimum allowed value of RSVT.
!     VMAT -  is the (NPAR * NPAR) V matrix of parameter space eigenvectors
!             output by ESVD.
!        R -  the transformed error vector output by ESVD.  It is the product
!             of the transposed U matrix times the observed error vector.
!       SV -  the (ordered) array of NPAR singular values returned by ESVD.
!             SV(1) > SV(2) > SV(3) etc.
!  *DELPAR -  the returned solution ( parameter changes)
!   *  WSP -  WSP (1:NPAR) contains the squared r vector
!             WSP (NPAR: 2*NPAR) contains the damping factors.
!    *PDRE -  the returned predicted decrease in residual squared error

!     * INDICATES VALUE RETURNED BY SOLVE2

 INTEGER NSV,I,NPAR
 REAL DPROD1,EIGPAR(NPAR),R(NPAR),SV(NPAR),DELPAR(NPAR),WSP(3*NPAR), &
      VMAT(NPAR,NPAR),Q,DMPFAC,PDRE,RSVT,ZERO
 EXTERNAL DPROD1

 INTENT (IN) NPAR,RSVT,ZERO,VMAT,R,SV
 INTENT (OUT) NSV,DELPAR,WSP,PDRE

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
   DELPAR(I) = DPROD1 (NPAR,NPAR,1,0.0,VMAT(I,1),EIGPAR)
   WSP(I) = R(I) * R(I)
 END DO
 PDRE = DPROD1(NPAR,1,1,0.0,WSP(1),WSP(NPAR+1))

END SUBROUTINE SOLVE2

 SUBROUTINE WRITE_INVMDL (NW,FINAL,ITS,NLYR,NPLT,NPAR,CXPAR,XPAR,XYNORM, &
                         IMPORT,D_NORTH,D_EAST,GND_LVL)
!--------------------------------------------------------------------------

!*** Called by: NLSQ2

! Simplified version of WRITE_MODEL for inversion output
! It includes importance for the final model.
!
!      NW - output unit
!   FINAL - true if final model, false if intermediate model.
!     ITS - print model after iteration ITS
!    NLYR - number of layers
!    NPLT - Number of plates (index = JP)
!    NPAR = 9*NPLT + 2*NLYR-1plate conductances
!   CXPAR = 0 => parameter is completely free to vary as dictated by inversion step
!         = 1 => parameter is fixed
!         = 2 => parameter is constrained by elasticity.
!         = 3 => parameter bounds are buffered.
!    XPAR = transformed parameter array
!  IMPORT = importance
! D_NORTH = north offset between model coordinates and real world coordinates.
!  D_EAST = east offset between model coordinates and real world coordinates.
!
!    XPAR(K0+1)  <->  SIG_T
!    XPAR(K0+2)  <->  PLTOP
!    XPAR(K0+3)  <->  PLNGTH
!    XPAR(K0+4)  <->  PLWDTH
!    XPAR(K0+5)  <->  YCNTR
!    XPAR(K0+6)  <->  XCNTR
!    XPAR(K0+7)  <->  PLAZM
!    XPAR(K0+8)  <->  PLDIP
!    XPAR(K0+9)  <->  PLUNJ

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 INTEGER NW,NLYR,NPLT,NPAR,CXPAR(NPAR),ITS,ANG(3,3),K0,JP,JP1,J,K1,KP,KP1,JL
 REAL PLTE(9,3),ZP(3),GND_LVL,IMPRT(9),XYNORM,A
 REAL, DIMENSION(NLYR) :: RES, THK,IMP_RES,IMP_THK
 REAL, DIMENSION(NPAR) :: XPAR,IMPORT
 REAL(KIND=QL) DPLTE(2,3),D_NORTH,D_EAST
 LOGICAL FINAL

 THK = 0.;  IMP_THK = 0.;  IMPRT = 0. ;  PLTE = 0. ; DPLTE = 0.

IF (FINAL) THEN
   WRITE(NW,10) ITS
   WRITE(*,10) ITS
 ELSE
   WRITE(NW,1) ITS
   WRITE(*,1) ITS
 END IF

 KP = 9*NPLT
 KP1 = KP + NLYR
 RES(1:NLYR)   = EXP (XPAR(KP+1:KP1))
 IMP_RES(1:NLYR)   = IMPORT(KP+1:KP1)
 DO JP = 1,NLYR - 1
   THK(JP) = EXP (XPAR(KP1+JP))
   IMP_THK(JP) = IMPORT(KP1+JP)
 END DO
 IF (NLYR > 1) THEN
   WRITE(NW,2,ADVANCE='NO') ('Lyr',JL,JL = 1,NLYR-1);
   WRITE(NW,'(7X,A)') 'Basement'
   WRITE(*,2) ('Lyr',JL,JL = 1,NLYR)
 ELSE
   WRITE(NW,'(T21,A)') 'Basement'; WRITE(*,'(T21,A)') 'Basement'
 END IF
 WRITE(NW,3) ('-----',JL = 1,NLYR);  WRITE(*,3) ('-----',JL = 1,NLYR)

 IF (FINAL) THEN
   IMP_RES(1:NLYR)   = IMPORT(KP+1:KP1)
   DO JP = 1,NLYR - 1
     IMP_THK(JP) = IMPORT(KP1+JP)
   END DO

   WRITE(NW,4) RES(1:NLYR);   WRITE(*,4) RES(1:NLYR)
   WRITE(NW,6) IMP_RES(1:NLYR);   WRITE(*,6) IMP_RES(1:NLYR)
   WRITE(NW,14); WRITE(*,14)
   IF (NLYR > 1) THEN
     WRITE(NW,5) THK(1:NLYR-1); WRITE(*,5) THK(1:NLYR-1)
     WRITE(NW,6) IMP_THK(1:NLYR-1); WRITE(*,6) IMP_THK(1:NLYR-1)
   END IF
 ELSE
   WRITE(NW,4) RES(1:NLYR);   WRITE(*,4) RES(1:NLYR)
   IF (NLYR > 1) THEN
     WRITE(NW,5) THK(1:NLYR-1)
     WRITE(*,5) THK(1:NLYR-1)
   END IF
 END IF

 WRITE(NW,11)
 WRITE(*,11)

 DO JP = 1,NPLT
   K0 = 9*(JP-1)
   PLTE = 0.
   DO J = 1,4
     JP1 = K0 + J            ! SIG_T, PLTOP, PLNGTH, PLWDTH
     PLTE(J,1) = EXP (XPAR(K0+J))
     PLTE(J,2:3) = PLTE(J,1)
     IF (FINAL .AND. CXPAR(JP1) /= 1) THEN
       IMPRT(J) = IMPORT(K0+J)
     END IF
   END DO

   DO J = 5,6                             ! ECNTR, NCNTR
     JP1 = K0 + J            ! SIG_T, PLTOP, PLNGTH, PLWDTH
     PLTE(J,1) = XYNORM * XPAR(K0+J)
     PLTE(J,2:3) = PLTE(J,1)
     IF (FINAL .AND. CXPAR(JP1) /= 1) THEN
       IMPRT(J) = IMPORT(K0+J)
     END IF
   END DO

   DO J = 7,9                             ! Azimuth, dip & plunge in degrees
     JP1 = K0 + J            ! SIG_T, PLTOP, PLNGTH, PLWDTH
     K1 = J - 6
     A = 180. * XPAR(K0+J)           ! XPAR * PI * (180 / PI)
     IF (J == 7) A = A + 90.         ! Convert from plate azimuth to dip azimuth
     ANG(K1,1) = INT (A)
     ANG(K1,2:3) = ANG(K1,1)
     IF (FINAL .AND. CXPAR(JP1) /= 1) THEN
       IMPRT(J) = IMPORT(K0+J)
     END IF
   END DO

   ANG(3,1:3) = 0                       ! Zero out plunge for this version
   IMPRT(9) = 0.

!  Parameter order: SIG_T, PLTOP, PLNGTH, PLWDTH, ECNTTR, NCNTR,  PLAZM, PLDIP, PLUNJ
!    Writing order: SIG_T, PLTOP, ECNTTR, NCNTR,  PLNGTH, PLWDTH, PLAZM, PLDIP, PLUNJ

   ZP(1:3) = GND_LVL - PLTE(2,1:3)
   DPLTE(1,1:3) = REAL (PLTE(5,1:3),QL) + D_EAST
   DPLTE(2,1:3) = REAL (PLTE(6,1:3),QL) + D_NORTH
   WRITE(*,7)  JP,PLTE(1,1),ZP(1),DPLTE(1:2,1),PLTE(3:4,1),ANG(1:3,1)
   IF (FINAL) THEN
     WRITE(NW,12) JP,PLTE(1,1),ZP(1),DPLTE(1:2,1),PLTE(3:4,1),ANG(1:3,1), &
                     IMPRT(1:2),IMPRT(5:6),IMPRT(3:4),IMPRT(7:9)
   ELSE
     WRITE(NW,7) JP,PLTE(1,1),ZP(1),DPLTE(1:2,1),PLTE(3:4,1),ANG(1:3,1)
   END IF
 END DO

 1 FORMAT(//T9,'Model Description After',I3,' Iterations' &
           /T9,'====================================='/)
 2 FORMAT(T13,20(8X,A,I2:))
 3 FORMAT(T13,20(8X,A))
 4 FORMAT(T5,'Resistivity:',20G13.4)
 5 FORMAT(T5,'  Thickness:',20G13.4)
 6 FORMAT(T5,' Importance:',F9.2,20F13.2)
 7 FORMAT(T3,'Plate',I2,':',F7.0,F10.1,2F12.1,F10.1,F9.1,I9,2I7)
 10 FORMAT(//T9,'Final Model After',I3,' Iterations' &
            /T9,'==============================='//)
 11 FORMAT(//T14,'        ____ Plate Reference Point ____                     ______  Angles  ________'  &
            /T14,'       |                               |                   |                        |' &
            /T14,'         Depth      Centre      Centre    Strike    Plate     Dip' &
            /T14,'SIG_T     RL         East       North     Length    Width   Azimuth   Dip   Plunge' &
            /T14,'-----    -----      ------      ------    ------    -----   -------   ---   ------')
 12 FORMAT(//T3,'Plate',I2,':',F7.0,F10.1,2F12.1,F10.1,F9.1,I9,2I7,    &
           //T3,'Import :',    F7.2,F10.2,2F12.2,F10.2,F9.2,F9.2,2F7.2)
 14 FORMAT(2X)

 END SUBROUTINE WRITE_INVMDL

 SUBROUTINE WRITE_MISFIT (NW,ITSPR,NSTAT,NDATA,MCHNL,TDFD,NFRQ,FREQ,CMP,NCHNL,VERR,XMODL,XDATA)
!---------------------------------------------------------------------------------------------

!***  Called by: NLSQ2

!  Writes out the error misfit at any stage of the inversion
!
!     ITSPR > 0 => printout after ITSPR iterations on unit NW
!           < 0 => printout after final iterations on unit NW
!           = 0 => printout initial error structure unit NW
!
!     NSTAT - number of stations points
!     NDATA - total number of data points
!     MCHNL - total number of data points per station
!      TDFD = 1: time domain  or 2: frequency-domain
!      FREQ - array of NFRQ frequencies for FD inversion
!     XDATA - 1D array of NDATA measured data points
!     XMODL - 1D array of NDATA computed data points from most recent model
!      VERR - error at each data point
!       CMP = 11 => inversion on horizontal in-line component only
!           = 13 => inversion on vertical component only
!           = 2  => joint inversion on vertical & horizontal in-line components
!           = 3  => inversion on all 3 components
!           = 4 or 42 => inversion on total field
!

 INTEGER NW,ITSPR,NDATA,NSTAT,TDFD,NFRQ,CMP,MCHNL,NCHNL,NCHN,N1,N2,JS,JT,JD
 REAL FREQ(NFRQ)
 REAL, DIMENSION(NDATA) :: XMODL,XDATA,VERR
 REAL, DIMENSION(MCHNL,NSTAT) :: RMODL,RDATA,RVERR
 CHARACTER(LEN=8) CHN(50)
 DATA CHN(1:50) &
   /' CHNL_1 ',' CHNL_2 ',' CHNL_3 ',' CHNL_4 ',' CHNL_5 ',' CHNL_6 ',' CHNL_7 ',' CHNL_8 ',' CHNL_9 ','CHNL_10 ', &
    'CHNL_11 ','CHNL_12 ','CHNL_13 ','CHNL_14 ','CHNL_15 ','CHNL_16 ','CHNL_17 ','CHNL_18 ','CHNL_19 ','CHNL_20 ', &
    'CHNL_21 ','CHNL_22 ','CHNL_23 ','CHNL_24 ','CHNL_25 ','CHNL_26 ','CHNL_27 ','CHNL_28 ','CHNL_29 ','CHNL_30 ', &
    'CHNL_31 ','CHNL_32 ','CHNL_33 ','CHNL_34 ','CHNL_35 ','CHNL_36 ','CHNL_37 ','CHNL_38 ','CHNL_39 ','CHNL_40 ', &
    'CHNL_41 ','CHNL_42 ','CHNL_43 ','CHNL_44 ','CHNL_45 ','CHNL_46 ','CHNL_47 ','CHNL_48 ','CHNL_49 ','CHNL_50 '/

!  Put data into matrix form

 CALL CNVRT2_2D

 RVERR = 100. * RVERR  !  Convert to percent

 IF (ITSPR == 0) THEN
   WRITE(NW,1)
 ELSE IF (ITSPR > 0) THEN
   WRITE(NW,2) ITSPR
 ELSE IF (ITSPR < 0) THEN
   WRITE(NW,3)
 END IF
 WRITE(NW,4)

 IF (TDFD < 2) THEN
   NCHN = MIN (NCHNL,50)
   IF (CMP == 13 .OR. CMP == 2 .OR. CMP == 3) THEN
     WRITE(NW,13)
     N1 = 1
     N2 = NCHN
     CALL PRT_TD
   END IF

   IF (CMP == 11 .OR. CMP == 2 .OR. CMP == 3) THEN
     WRITE(NW,11)
     N1 = 1
     N2 = NCHN
     IF (CMP == 2 .OR. CMP == 3) THEN
       N1 = NCHNL + 1
       N2 = N1-1 + NCHN
     END IF
     CALL PRT_TD
   END IF

   IF (CMP == 3) THEN
     WRITE(NW,12)
     N1 = 2*NCHNL + 1
     N2 = N1-1 + NCHN
     CALL PRT_TD
   END IF

   IF (CMP == 4 .OR. CMP == 42) THEN
     WRITE(NW,14)
     N1 = 1
     N2 = NCHN
     CALL PRT_TD
   END IF

 ELSE
   WRITE(NW,5)
   N1 = 1
   N2 = NFRQ
   CALL PRT_FD
   WRITE(NW,6)
   N1 = NFRQ + 1
   N2 = 2*NFRQ
   CALL PRT_FD
 END IF

 1 FORMAT(//T7,'ERROR STRUCTURE OF INITIAL MODEL' &
           /T7,'---------------------------------')
 2 FORMAT(//T7,'ERROR STRUCTURE OF MODEL AFTER',I3,' ITERATIONS' &
           /T7,'--------------------------------------------')
 3 FORMAT(//T7,'ERROR STRUCTURE OF FINAL MODEL' &
           /T7,'------------------------------')
 4 FORMAT(/T3,'For each station:'                              &
          /T3,'----------------'                               &
          /T5,'The first line is the percent rms error.' &
          /T5,'The second line is the model response.'         &
          /T5,'The third line is the data.')
 5 FORMAT(//T7,'IN-PHASE COMPONENT STRUCTURE' &
           /T7,'----------------------------')
 6 FORMAT(//T7,'QUADRATURE COMPONENT STRUCTURE' &
           /T7,'------------------------------')
 11 FORMAT(//T7,'IN-LINE COMPONENT STRUCTURE' &
            /T7,'---------------------------')
 12 FORMAT(//T7,'TRANSVERSE COMPONENT STRUCTURE' &
            /T7,'------------------------------')
 13 FORMAT(//T7,'VERTICAL COMPONENT STRUCTURE' &
            /T7,'----------------------------')
 14 FORMAT(//T7,'TOTAL COMPONENT STRUCTURE' &
            /T7,'-------------------------')

 CONTAINS

   SUBROUTINE CNVRT2_2D
!  --------------------

!*** Called by: WRITE_MISFIT

     DO JS = 1,NSTAT
       DO JT = 1,MCHNL
         JD = JT + (JS-1) * MCHNL
         RDATA(JT,JS) = XDATA(JD)
         RMODL(JT,JS) = XMODL(JD)
         RVERR(JT,JS) = VERR(JD)
       END DO
     END DO
   END SUBROUTINE CNVRT2_2D

   SUBROUTINE PRT_TD
!  -----------------

!***  Called by: WRITE_MISFIT

     WRITE(NW,'(/6X,30(A:,5X))') CHN(1:NCHN)
     DO JS = 1,NSTAT
       WRITE(NW,'(/I4,50G13.4)') JS,RVERR(N1:N2,JS)
       WRITE(NW,'( 4X,50G13.4)') RMODL(N1:N2,JS)
       WRITE(NW,'( 4X,50G13.4)') RDATA(N1:N2,JS)
     END DO
   END SUBROUTINE PRT_TD


   SUBROUTINE PRT_FD
!  -----------------

!***  Called by: WRITE_MISFIT

     WRITE(NW,'(/4X,40F11.0)') FREQ(1:NFRQ)
     DO JS = 1,NSTAT
       WRITE(NW,'(/I4,40F11.2)') JS,RVERR(N1:N2,JS)
       WRITE(NW,'( 4X,40F11.2)') RMODL(N1:N2,JS)
       WRITE(NW,'( 4X,40F11.2)') RDATA(N1:N2,JS)
     END DO
   END SUBROUTINE PRT_FD

 END SUBROUTINE WRITE_MISFIT

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
