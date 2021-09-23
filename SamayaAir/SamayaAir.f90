Module SA_Metadata
!--------------
!
! Module designed to hold 'constant character options'
    Implicit NONE

    Character (Len = 40), Parameter :: PNAME = 'SamayaAir'
    Character (Len = 40), Parameter :: PVERS = '2.2.6'
    Character (Len = 40), Parameter :: PDATE = '09 February, 2021'
    Character (Len = 40), Parameter :: PAUT1 = 'CSIRO Electromagnetic Modelling Group'
    Character (Len = 40), Parameter :: PAUT2 = 'Art Raiche, Fred Sugeng and Glenn Wilson'
    Character (Len = 40), Parameter :: PAUT3 = 'David Annetts (david.annetts@csiro.au)'
    Character (Len = 40), Parameter :: PPROJ = 'AMIRA P223F'
    Character (Len = 80), Parameter :: PRELS = 'GPL-2.0 (Full text at https://opensource.org/licenses/gpl-2.0.php)'

End Module SA_Metadata

!   PROGRAM SamAir    Version 2.2.6    20 July, 2017     Language: ANSI Standard Fortran 95
!   ======= ======    =============    =============     ----------------------------------
!
!
!            Developed by:  CSIRO Electromagnetic Modelling Group
!                           Fred Sugeng, Glenn Wilson & Art Raiche
!
!                     For:  AMIRA project P223F
!
!===========================================================================!
!                                                                           !
!                             Program Description                           !
!                                                                           !
!===========================================================================!
!
!   The Model consists of a rectangular 3D prism embedded in a uniform halfspace.
!   The prism interior can contain any arbitrary, geoelectrically heterogenous 
!   3D structure including topography and unconformaties.  At the boundaries, 
!   its top surface should be continuous with the halfspace surface, but in the 
!   interior it can extend above and below that surface.
!
!   The prism interior is subdivided by the user into hexahedral voxels that can 
!   be distorted to suit the structures to be modelled.  It is unwise to use more
!   than 1000 voxels or to let the prism horizontal area exceed 1 square km.
!
!   For efficient computation, the prism volume should restricted to encompass 
!   only the heterogeneous region.
!   
!   Both the prism and host can have frequency-dependent complex conductivities 
!   including IP effects (modelled using Cole-Cole parameters) and variable 
!   permeability and dielectric permittivity. 
!   
!   The preferred graphical interface for data entry is Maxwell.
!   The mesh property specification is based on lithologies.
!
!   Modelling
!   ---------
!
!   SamAir models the airborne EM response (dB/dt or B) of the above model using
!   the compact finite element method (CFEM).  In the current version, this 
!   consists of using an edge-finite-element method with linear elemenst in a 
!   volume containing the prism and integral-equation boundary conditions on all 
!   six surfaces of that volume.
!   
!   Regardless of the system, the base computation is for the frequency-domain
!   magnetic induction (B).  For frequency-domain systems, this is normalised 
!   to the appropriate DC magnetic field.  For time domain, computations are 
!   performed for 28 logrithmically-spaced frequencies: 3 between 1 to 10 Hz
!   and then 6 per decade from 10 Hz to 100 kHz.  These are then splined and 
!   extrapolated back to DC before a specially designed Hankel filter is used to 
!   compute time-domain step response out to several pulse lengths.  These are 
!   then folded back to a single pulse which is then convolved with the system 
!   and integrated over the receiver channels either as B or dB/dt.
!
!   Inversion
!   ---------
!
!   SamAir is is capable of inverting the 3D airborne EM response measured
!   from frequency-domain systems to recover a generally heterogeneous 3D
!   structure in a limited 3D domain embedded in an otherwise uniform half-
!   space. Inversion is based on the nonlinear least square iterative inversion
!   based on the Gauss-Newton method with damping of the singular value spectrum
!   of the Jacobian matrix; the damped eigenparameter algorithm. The Jacobian
!   matrix is constructed using the reciprocity method.
!
!
!===========================================================================!
!                                                                           !
!                                  Systems                                  !
!                                                                           !
!===========================================================================!
!
!
!   SamAir should be capable of modelling almost any time or frequency-domain 
!   AEM system the user can reasonably conceive.  In practice, it has been well 
!   tested for a variety of AEM systems including: Spectrem, Tempest, Geotem, 
!   Aerotem, Vtem, Hoistem, Dighem, wingtip frequency-domain systems, and above 
!   surface slingram configurations.
!   
!   The transmitter is modelled as a magnetic dipole whose axis is in the 
!   vertical plane along the flight path except for the vertical coplanar 
!   broadside option.  The other exception to this is for co-axial time-domain 
!   HEM where the system normalisation assumes a horizontal loop of finite radius.
!   In time-domain, transmitter  output can be specified either as current or
!   as dB/dt or B from a calibration run.
!
!   A variety of flight path specifications are available.
!
!
!===========================================================================!
!                                                                           !
!                     Time-Domain Waveform Sign Convention                  !
!                                                                           !
!===========================================================================!
!
!
!   If the user specifies waveform excitation as either the transmitter
!   current or primary B at the receiver, the program assumes that current or
!   magnetic field will start at 0 or some low value, and rise to a positive
!   maximum before going to zero or oscillating about zero with small
!   magnitudes.  In this case, dI/dt will be computed using the negative I or B
!   so that the early off-time response is positive for vertical fields.
!
!   If the user specifies the excitation waveform as dB/dt at the receiver,
!   the program assumes that the response will rise to a positive maximum
!   followed by a negative maximum before oscillating about zero with smaller
!   amplitudes.  In this case dI/dt is derived by reversing the sign of the
!   input primary dB/dt and dividing by the geometric coupling factor.  Again,
!   this procedure is designed so that the early off-time response is positive
!   for vertical fields.
!
!
!   NOTE: This version of SamAir is based on computing the TOTAL field
!         in the finite-element routines.  There is no half-space
!         distributed source and BTD_SCAT is identically BTD and
!         BFD_SCAT is identically BFD.
!
!         This total field approach is faster and testing has confirmed
!         accuracy and stability.
!
!
!===========================================================================!
!                                                                           !
!                             File Conventions                              !
!                                                                           !
!===========================================================================!
!
!
!   The input control file, now called SamayaAir.cfl is read from logical
!   unit number 3.
!
!   For inversion, the data to be inverted must be contained in SamayaAir.inv
!   from the logial unit number 13. The format of SamayaAir.inv is exactly
!   identical to LeroiAir.inv, Airbeo.inv and ArjunAir.inv to enable ease
!   of portability between inversion programs.
!
!   Frequency-domain data stored in SamayaAir.frq data is written
!   to logical unit number 7.
!
!   Verbose output data file for both forward modelling and inversion
!   is stored in SamayaAir.out, written to logical unit number 4.
!
!   Messages about data or runtime errors are now written in a file
!   called SamayaAir.log on logical unit 9.
!
!   For plotting, the output data file for forward modelling, now called
!   SamayaAir.mf1, is written to logical unit 14.
!
!   For imaging, the output data file for inverse modelling, called
!   SamayaAir.mv1, is written to logical unit 14.
!
!
!     UNIT #   UNIT ID      FILE ID           FUNCTION
!     ------   -------      -------           --------
!       3        NR       SamayaAir.cfl    INPUT CONTROL FILE
!       13       NRI      SamayaAir.inv    INVERSION CONTROLS AND DATA FILE
!       4        NW       SamayaAir.out    VERBOSE FORWARD MODELLING OUTPUT
!       4        NW       SamayaAir.out    VERBOSE INVERSE MODELLING OUTPUT
!       7        ND       SamayaAir.frq    FREQUENCY-DOMAIN DATA FOR TD EM REUSE
!       9        NLG      SamayaAir.log    RUNTIME AND DATA ERROR MESSAGES
!       10       NM       SamayaAir.res    ELEMENT RESISTIVITY
!       14       NWI      SamayaAir.mf1    OUTPUT DATA FROM FORWARD MODELLING
!       14       NWI      SamayaAir.mv1    OUTPUT DATA FROM INVERSE MODELLING
!
!
!===========================================================================!
!                                                                           !
!              Description of data records for SAMAIR.CFL                   !
!                                                                           !
!===========================================================================!
!
!
!  All records are in list directed (free) format except for the TITLE
!  in RECORD 1.
!
!    NOTE:  All distances and locations are to be specified in metres.
!    ----   All angles are to be specified in degrees.
!
!           In plan view, the coordinate system used for input and output
!           is (East, North, Depth). Depth is positive downwards.
!
!
!**  RECORD 1:  TITLE - up to 120 characters.
!
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
!      DO3D =  1, 2 or 0 for MODELLING
!
!      DO3D =  1  computes response of 3-D heterogeneities and prints
!                 voltages as measured by receivers.
!
!           =  2  (time-domain only)
!                 instead of computing the frequency-domain responses
!                 (95-99 percent of SamAir usual computation time) use
!                 previously computed frequency-domain responses contained
!                 in file SamayaAir.frq to calculate 3-D time-domain responses.
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
!              NOTE ON DO3D for TIME-DOMAIN modelling
!              ------------------------------------------------
!
!    Each time SamAir is run with DO3D = 1, a file called SamayaAir.frq 
!    is created on logical unit ND = 7.  SamAir reads this file to 
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
!            have the option to apply this algorithm to SamAir model output.
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
!----------------------------------------------------------------------------
!
!             Flight Path Information
!             -----------------------
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
!
!-------------------------------------------------------------------------------
!
!
!   ONLY IF SURVEY = 1    Constant course and altitude
!   ------------------
!
!    IF LINE_TAG = 0
!**  RECORD 9.1:  EAST(1), NORTH(1), ALT(1), BEARING, DSTAT   
!
!    IF LINE_TAG = 1
!**  RECORD 9.1:  LINE(1), EAST(1), NORTH(1), ALT(1), BEARING, DSTAT   
!
!      EAST(1) - Initial East coordinate of transmitter
!     NORTH(1) - Initial North coordinate of transmitter
!       ALT(1) - Altitude of transmitter
!      BEARING - Flight path angle with respect to North (in degrees)
!                Due North = 0;  due East = 90.
!        DSTAT - Distance (metres) between successive transmitter positions.
!
!
!-------------------------------------------------------------------------------
!
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
!      EAST(J) - East coordinate of transmitter at station J
!     NORTH(J) - North coordinate of transmitter at station J 
!       ALT(J) - Altitude of transmitter at station J 
!
!       ZRX(J) - Vertical offset of receiver at station J
!                below = positive
!       XRX(J) - In-line horizontal offset of receiver at station J
!                behind = positive
!       YRX(J) - Transverse horizontal offset of receiver at station J
!                Starboard = positive.
!
!===============================================================================
!
!
!          LITHOLOGY & STRUCTURE FOR SamAir
!          ================================
!
!
!** RECORD 10:  NLAYER, NLITH
!
!      NLAYER - number of layers including basement.
!               In this version, NLAYER must equal 1.
!
!       NLITH - number of layers plus mesh lithologies. Any number of
!               lithologies may be defined.
!
!             If NLITH is negative, ArjunAir reads the element resistivities
!             from ArjunAir.res (logical unit NM) and internally over-writes
!             the resistivity defined by each element's lithology with those
!             read from ArjunAir.res.
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
!    SIG_T(I) - Conductance (conductivity-thickness product)  (NOT USED)
!               A value for SIG_T must be entered even though it isn't used.
!               It is necessary because of the form of the data base in EMGUI
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
!     NOTE:  A value for SIG_T must be entered even though it isn't used.
!            It is necessary because of the form of the data base in EMGUI
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
!
!         SET MESH SIZE, ACCURACY & SOLVER OPTIONS
!         ----------------------------------------
!
!**  RECORD 13:  NNORTH, NZ, NEAST, KACC, SOLVER, OUTPUT
!
!                The heterogeneous domain must be divided into
!                a mesh which is defined by the user.
!
!      NNORTH - number of domain nodes in North-South direction.
!          NZ - number of domain nodes in depth-direction.
!       NEAST - number of domain nodes in East-West direction.
!
!        KACC = 1 => standard direct solver
!             = 2 => higher accuracy but much slower.
!
!      SOLVER = 1  direct matrix solution
!             = 2  iterative matrix solution
!
!      OUTPUT = 10:  standard output of east, north and vertical components
!                    There are no other options at present.
!
!         This version is restricted to SOLVER = 1
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
!          NODE LOCATIONS:   NNORTH * NZ * NEAST records
!          ---------------------------------------------
!
!       The ordering below is best pictured by taking one 2D sheet at a time
!       along the North-South axis from South to North.  For each sheet,
!       nodes and associated lithologies are read a row at a time
!       (West to East) from the top (surface) to the bottom.
!
!       The lithology of each hexahedral cell is associated with the top,
!       southern, western node defining that cell.
!
!**  RECORDS 14.(I,J,K):  I, K, J, NLOC, ZLOC, ELOC, LITH
!
!      1 <= I <= NNORTH is the node index increasing from South to North
!      1 <= K <= NZ      is the node index increasing from surface to bottom
!      1 <= J <= NEAST  is the node index increasing from West to East
!
!      NLOC(J,I,K) = the north coordinate of node (J,I,K) in metres.
!                    For fixed I,K, it must increase with increasing I.
!
!      ZLOC(J,I,K) = the relative level of node (J,I,K) in metres.
!                    For fixed I,J, it must increase negatively with
!                    increasing K.
!
!      ELOC(J,I,K) = the east coordinate of node (J,I,K) in metres.
!                    For fixed J,K, it must increase with increasing J.
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
!                        INVERSION INPUTS FOR SAMAIR
!                        ---------------------------
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
!      MV1PRT refers to the output print level in SamayaAir.mv1.
!
!      OUTPRT refers to the output print level in SamayaAir.out.
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
!   END OF SAMAIR.CFL
!
!
!===========================================================================!
!
!               Description of data records for SAMAIR.INV
!               ==========================================
!
!   Note:
!   -----
!
!   The format of SamayaAir.inv is identical to ArjunAir, LeroiAir.inv and
!   Airbeo.inv to enable ease of portability between inversion programs.
!
!   Any number of comment lines can be inserted ONLY at the beginning of
!   SamayaAir.inv. These must be designated by making the first character
!   either / or \ as the first characters of the line.
!
!   Blank lines or any other line whose first character is NOT / or \
!   signifies the start of data records.
!
!
!---------------------------------------------------------------------------!
!
!
!   DATA WEIGHTING
!   ==============
!
!   In what follows, the channel reference term PCHNL refers to each
!   component for each frequency or time-domain channel.
!
!   NPCHNL = the number of PCHNLs.
!
!   For example if there are 2 components of 10 CHANNEL time-domain data,
!   NPCHNL = 20. For the 5 frequency DIGHEM system NPCHNL = 10 corresponding
!   to 5 in-phase and 5 quadrature data
!
!     Frequency-domain:
!     -----------------
!
!     Channels are odered from the lowest to the highest frequency.
!
!     PCHNL = 1 to NFRQ for the in-phase and
!           = NFRQ+1 to 2*NFRQ for the quadrature.
!
!     Time-domain:
!     ------------
!
!     Regardless of the order in which the data is read in, for weigting
!     PCHNL = 1 to NCHNL refer to
!                  Vertical response if CMP = 13, 2 or 3
!                  In-Line reaponse of CMP = 11
!                  Transverse reaponse of CMP = 12
!           = NCHNL+1 to 2*NCHNL refer to in-line reaponse If CMP = 2
!           = 2*NCHNL+1 to 3*NCHNL refer to transverse reaponse of CMP = 3
!
!
!**  RECORD 18: NSTAT, SURVEY, BAROMTRC, KCMP, ORDER
!
!      NSTAT - number of stations to be read in from SamayaAir.inv.
!
!      SURVEY = 2 => variable altitude and course but constant Tx-Rx geometry.
!      SURVEY = 3 => variable altitude, course, transmitter pitch and receiver
!                    offset (time-domain only).
!
!      BAROMTRC = 0 => altitudes are ground clearance in metres.
!               = 1 => altitudes are barometric; ie, metres above sea level.
!
!      Frequency-domain:
!
!      KCMP(1:NFRQ) : specify components for each frequency. These must be in the
!                     order as the frequencies in SamayaAir.cfl
!
!                  1 : HCP - horizontal coplanar
!                  2 : VCA - vertical coaxial
!                  3 : VCP - vertical coplanar
!                  4 : VCB - vertical coplanar broadside
!
!      Dighem example:       KCMP(1:5) = 1 2 2 1 1  for 5 frequencies starting from the lowest.
!      GTK wingtip example:  KCMP(1:2) = 3 3        for 2 frequencies starting from the lowest.
!
!      Frequency-domain:
!
!      ORDER = 1122 : all inphase data will be followed by all quadrature data
!            = 1212 : data consists of paired inphase and quadrature for each frequency
!            = 2211 : all quadrature data will be followed by all inphase data
!            = 2121 : data consists of paired quadrature and inphase for each frequency
!
!
!**  RECORD 19: DATA_FLOOR
!
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
!**  RECORD 20: N0STAT, N0CHNL, N0PTS
!
!      NSTAT - Number of stations to be read in from SamayaAir.inv
!
!      N0STAT - Number of stations for which all the data will be weighted to zero.
!
!      N0CHNL - Number of PCHNLs for which all the data will be weighted to zero.
!
!      N0PTS  - Number of data points not covered by K0STAT and K0CHNL which will be
!               weighted to zero.
!
!
!    Only if N0STAT > 0:
!    -------------------
!
!**  RECORD 20.1:  K0STAT(1:K0STAT) - Indices of stations for which all data
!                                     will be weighted to 0.
!
!
!    Only if N0CHNL > 0:
!    -------------------
!
!**  RECORD 20.2:  K0CHNL(1:N0CHNL) - Indices of PCHNLs for which all data
!                                     will be weighted to 0.
!
!
!    Only if N0PTS > 0:
!    ------------------
!
!**  RECORD 20.3:  (J0CH(I),J0ST(I)), I = 1,N0PTS)
!
!      PCHNL and station indices of individual points to be weighted to 0
!      using the above PCHNL ordering convention.
!
!
!   DATA ENTRY
!   ==========
!
!
!   NSTAT records:
!
!
!   For SURVEY = 2 or -2:
!   ---------------------
!
!**  RECORD 21.J: EAST(J), NORTH(J), ALT(J), DATA(1:NPCHNL,J)
!
!
!   For SURVEY = 3 or -3 (time domain):
!   -----------------------------------
!
!**  RECORD 21.J: EAST(J), NORTH(J), ALT(J), TXCLN(J), ZRX(J), XRX(J), YRX(J), DATA(1:NPCHNL,J),
!                                       ZRX(J), XRX(J)
!
!      DATA(I,J) = datum of PCHNL I at station J
!
!      Note:
!      -----
!
!      Unnormalised B data is in pT and unnormalised dB/dt data is in nT/s
!      unless oherwise specified in RECORD 2.2.
!
!      For frequency-domain inversion or if KPPM > 0, normalised data are
!      expressed in ppm unless otherwise specified using NPPF /= 3.
!
!      ZRX(J), XRX(J) = Vertical and inline offsets at station J.
!
!
!   END OF SAMAIR.INV
!
!
!===========================================================================!
!
!
!  There are no sample control files in this documentation because of
!  their length. Refer to the example control files supplied with this
!  release of SamayaAir.
!
!
!===========================================================================!
!                                                                           !
!                              P223F Modules                                !
!                                                                           !
!===========================================================================!

 MODULE SA_Filter_Coefficients_q

 !---------------------------------------------------------------------------

 !  Niels Christensen shifted cosine filter:
 !  12 points per decade, OMEGA = .3 PI
 !  These are based on using extended precision.

 IMPLICIT NONE

 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 INTEGER, PARAMETER :: JNLO=-250, JNHI=150, NDEC_JN=15, &
                       SNLO=-112, SNHI=85,  NDEC_SN=12
 INTEGER J9
 REAL(KIND=QL) WCOS(-200:99), DELCOS
 REAL WSIN(SNLO:SNHI)
 REAL SHFTJN, WJ0(JNLO:JNHI), WJ1(JNLO:JNHI)

 SAVE

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

 END MODULE SA_Filter_Coefficients_q

 !===========================================================================

 MODULE SA_Frequency_Table

 !---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER, PARAMETER :: NF_MD2=46, NF_6PDE=67
 REAL FRQ_MD2(NF_MD2), FRQ_6PDE(NF_6PDE)
 SAVE

 ! FRQ_MD2 has 3 frequencies / decade from 0.001 to 1 Hz &
 !             6 frequencies / decade from 1.0 to 1MHz

 DATA FRQ_MD2(1:NF_MD2)/ &
  0.10000000E-02, 0.21544347E-02, 0.46415888E-02, 0.10000000E-01, 0.21544347E-01, 0.46415888E-01, &
  0.10000000E+00, 0.21544347E+00, 0.46415888E+00, 0.10000000E+01, 0.14677993E+01, 0.21544347E+01, &
  0.31622777E+01, 0.46415888E+01, 0.68129207E+01, 0.10000000E+02, 0.14677993E+02, 0.21544347E+02, &
  0.31622777E+02, 0.46415888E+02, 0.68129207E+02, 0.10000000E+03, 0.14677993E+03, 0.21544347E+03, &
  0.31622777E+03, 0.46415888E+03, 0.68129207E+03, 0.10000000E+04, 0.14677993E+04, 0.21544347E+04, &
  0.31622777E+04, 0.46415888E+04, 0.68129207E+04, 0.10000000E+05, 0.14677993E+05, 0.21544347E+05, &
  0.31622777E+05, 0.46415888E+05, 0.68129207E+05, 0.10000000E+06, 0.14677993E+06, 0.21544347E+06, &
  0.31622777E+06, 0.46415888E+06, 0.68129207E+06, 0.10000000E+07/

 ! FRQ_6PDE has 6 frequencies / decade from 0.001 to 1 MHz

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

 END MODULE SA_Frequency_Table

 !===========================================================================

 MODULE SA_Input_Routines

 ! Contains READ_SYSTEM_AND_SURVEY, READ_MODEL_DATA, SET_FRQ, SET_TRP,
 !          RESMAP, WRITE_np_HEADER, WRITE_np_SYS_DATA, READ_INVRT_CNTRL,
 !          READ_INVRT_DATA.

 !---------------------------------------------------------------------------

 Use iso_Fortran_env
 Use SA_Metadata
 IMPLICIT NONE

 INTEGER, PARAMETER :: QL = SELECTED_REAL_KIND(p = 18), NPROP = 7, NCPTS = 3
 REAL, PARAMETER :: T0_MIN = 1.E-7, PI = 3.141592654, PI2=PI/2., DATA_TOL = 1.E-24, & 
                    TURN = PI/10.
 REAL, PARAMETER :: EPSMU0=1.112646E-17     ! MU0 * EPS0 = 1/ C^2
 COMPLEX, PARAMETER :: ZERO=(0.,0.)
 INTEGER NM,NR,NRI,NS,NW,np,ND,NMP,NDR,NLG,FVN,TDFD,STEP,DO3D,ISW,PRFL,NCHNL,KPPM, &
         ISTOP,NFRQ,JF,NTXD,NLITH,NSX,NSX1,JT,NSTAT,NSTAT1,JS,NTRN,NRX,NRXST,JR,JP, &
         NLYR,JL,MTXRX,NPULS,NTYRP,NTYPLS,SURVEY,BAROMTRC,LINE_TAG,MTXL,CMP,KRXW,   &
         MSG,MXERR,J,GSTRP,ASTRP,IUNITS,NPPF,QQDT(8),QQHMS(2),NE,NN,NZ,NSE,NSN,NSZ, &
         KACC,SOLVER,LEVEL,NCOMP,NTX,OUTPRT,MV1PRT,CNVRG,MAXITS,NDATA,NPART,NPAR,   &
         MCHNL,JD,NPASS,K,I,NBN,NBNT,KLITH,INRM,IPPD,NEL,NER,NNL,NNR,NZT,NZB
 INTEGER, ALLOCATABLE,DIMENSION(:) :: LITHL,KFIX,CXPAR,LINE,XWTS,CFG1
 INTEGER, ALLOCATABLE, DIMENSION(:,:) :: RWTS
 INTEGER, ALLOCATABLE, DIMENSION(:,:,:) :: LITH
 INTEGER,DIMENSION(:),ALLOCATABLE :: ICOLE,Z_INDX,NSTATL,LITHP
 REAL MIN_FREQ,MAX_FREQ,TXAMPL,TXFREQ,PULSE,PKCUR,OFFTYM,ALF,DELT,ALT1,CSF,SNF,TXAREA, &
      TXCLN0,XRX0,YRX0,ZRX0,T0,DSTAT,PRM_TD(3),BFFAC,PPFAC,THICK,PCTCNV,ALINE,S1,BIG
 REAL,DIMENSION(:),ALLOCATABLE :: FREQ,WAVEFORM,TXON,SWX,TMS,WTMS,TOPN,TCLS,SZ,SX,SY,ZRX, &
                                  XRX,YRX,BEARING,FANGLE,SZ1,SX1,SY1,FANGLE1,TXCLN, &
                                  TXDEG,TRP,PRM_FD,NORM,XDATA,XMODL,UBND,LBND,ELAS,MPAR,XPART, &
                                  DNORM,DATA_FLOOR
 REAL,DIMENSION(:),ALLOCATABLE :: THK,RES,RMU,REPS,CNDMU,EPSMU,CHRG,CALF,CTAU,CFREQ
 REAL,DIMENSION(:,:),ALLOCATABLE :: SWY,RX,RY,RZ,LYTH,ESURF,NSURF,ZSURF,RDATA
 REAL,DIMENSION(:,:,:),ALLOCATABLE :: XRM,YRM,ELOC,NLOC,ZLOC,PRM_DC
 REAL(KIND=QL) QFRQ1,QFRQ2,FQQ,ECNTRD,NCNTRD,EAST1,NORTH1,RXR,RYR,RZR,D_EAST,D_NORTH
 REAL(KIND=QL), ALLOCATABLE :: SXD(:),SYD(:),RXD(:,:),RYD(:,:)
 REAL(KIND=QL), ALLOCATABLE :: ESURFD(:,:),NSURFD(:,:),ZSURFD(:,:)
 REAL(KIND=QL), ALLOCATABLE :: ELOCD(:,:,:),NLOCD(:,:,:),ZLOCD(:,:,:)
 LOGICAL NEWF,TXA90,COLE,NEW,INVERT,JCBN,RESID,READ_RES_FILE
 LOGICAL, ALLOCATABLE :: FIXPRM(:)
 CHARACTER(LEN=3), ALLOCATABLE :: CONFIG(:)
 CHARACTER(LEN=120) INP,TITLE
 CHARACTER(LEN=60) PVC,LTXT
 CHARACTER(LEN=10) TIME,DATE,ZONE
 CHARACTER(LEN=3) MONTH(12)
 CHARACTER(LEN=4) QUNIT,BUNIT,PUNIT
 Integer :: tvals(8)
 DATA MONTH /'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'/
 DATA PVC, FVN /'SamAir - Version 2.2.5   22 September 2007',100/
 DATA LTXT     /'------------------------------------------'/

 SAVE

 CONTAINS ! READ_SYSTEM_AND_SURVEY
          ! READ_MODEL_DATA
          ! SET_FRQ
          ! SET_TRP
          ! RESMAP
          ! WRITE_np_HEADER
          ! WRITE_np_SYS_DATA
          ! READ_INVRT_CNTRL
          ! READ_INVRT_DATA

   !-------------------------------------------------------------------------
   ! MODULE: SA_Input_Routines !
   !-------------------------------!

   SUBROUTINE READ_SYSTEM_AND_SURVEY

   !-------------------------------------------------------------------------

   !***  Called by: MAIN
   !***      Calls: CALL CONFIG_ID, WRITE_LOG_FILE


   REAL BFFACS(6),PPFACS(4),RHO,DELX,DELY,A1
   CHARACTER(LEN=4) PUNITS(4),BUNITS(6)
   CHARACTER (LEN=19) WVFRM(3)
   DATA WVFRM /'Transmitter current','Vertical receiver  ','Horizontal receiver'/
   DATA PUNITS /'pct ','ppt ','ppm ','ppb '/
   DATA BUNITS /'nT/s','pT/s','fT/s','nT  ','pT  ','fT  '/
   DATA BFFACS /1.,1000.,1.E6,1.,1000.,1.E6/
   DATA PPFACS /1.E2, 1.E3, 1.E6,1.E9/

   NR =  3     !  Input unit number for SamayaAir.cfl
   NW =  4     !  Output unit number for SamayaAir.out
   ND =  7     !  Input/Output unit number for SamayaAir.frq
   NS =  8     !  Output unit number for SamayaAir.sty
   NLG = 9     !  Log file unit number for SamayaAir.log
   NM = 10     !  Unit number for SamayaAir.res.
   NRI = 13    !  Inversion data unit number for SamayaAir.inv.
   np = 14    !  Output unit number for SamayaAir.mf1
   NMP = 25    !  Set MAP FILE UNIT NUMBER

   OPEN(NR,FILE = 'SamayaAir.cfl',STATUS = 'OLD')
   OPEN(NW,FILE = 'SamayaAir.out',STATUS = 'REPLACE')
   OPEN(NMP,FILE = 'SamayaAir.map',STATUS = 'REPLACE')

   ! Initialise some variables.

   MXERR = 0             !  Initialise input error flag
   NCHNL = 1             !  Initialise dimensions
   NPPF = 3              !  ppm default
   IUNITS = 5            !  pT default
   GSTRP = 0
   ASTRP = 0
   TXAREA = 1.
   PRM_TD = 0.
   NEWF = .FALSE.
   INVERT = .FALSE.
   JCBN = .FALSE.
   RESID = .TRUE.
   NDR = NR             !  Read from SamayaAir.cfl
   TXA90 = .FALSE.      !  Don't print scattered fields

   ! Reproduce input data with no assignments and rewind file.

   Call date_and_time(Values = tvals)
   Write (NW, 1) PNAME, PVERS, PDATE, PAUT1, PAUT2, PAUT3, PPROJ, PRELS, &
              compiler_version(), compiler_options(), &
              tvals(1:3), tvals(5:7)
   Write ( *, 1) PNAME, PVERS, PDATE, PAUT1, PAUT2, PAUT3, PPROJ, PRELS, &
              compiler_version(), compiler_options(), &
              tvals(1:3), tvals(5:7)
   WRITE(NW,'(/T11,A/T11,A/)') 'INPUT DATA', '----------'

   ! REFLECT_DATA: DO JF = 1,10000
   !   READ(NR,'(A)',END = 100) INP
   !   WRITE(NW,'(1X,A)') INP
   ! END DO REFLECT_DATA
   ! 100 REWIND NR
   !     WRITE(NW,2)

   READ(NR,'(A)') TITLE
   WRITE(NW,'(/1X,A)') TRIM (TITLE)

   ! Read model control & print parameters.

   READ(NR,*)  TDFD, DO3D, PRFL, ISTOP
   WRITE(NW,3) TDFD, DO3D, PRFL, ISTOP
   IF (DO3D < 0) INVERT = .TRUE.

   IF (PRFL == 0 .OR. PRFL == 2 .OR. PRFL == 10) THEN
     PRFL = 2
   ELSE
     PRFL = 1
   END IF

   !   TDFD = 0, 1 or 2 for user controlled TD, standard TD or FD respectively.
   !   DO3D = 2 => use old FD data from SamayaAir.frq.
   !        = 1 => compute new  plate model.
   !   PRFL - indicates profile or decay curve output
   !  ISTOP - read data and stop if ISTOP = 1
   !        - used as a frequency setter later on in this routine.

   IF (TDFD < 0 .OR. TDFD > 2) CALL WRITE_LOG_FILE (NLG,1,MXERR,2)

   IF (TDFD == 2 .AND. DO3D == 2) THEN
     CALL WRITE_LOG_FILE (NLG,3,MXERR,1)
     DO3D = 1
   END IF

   IF (TDFD < 2) THEN
     IF (DO3D == 1) THEN
       NEWF = .TRUE.
       OPEN(ND,FILE = 'SamayaAir.frq',STATUS = 'REPLACE')
     ELSE IF (DO3D == 2) THEN
       OPEN(ND,FILE = 'SamayaAir.frq',STATUS = 'OLD')
     END IF

     ! Transmitter system information.

     IF (TDFD == 0) THEN
       READ(NR,*) MIN_FREQ,MAX_FREQ,IPPD
       WRITE(NW,20) IPPD,MIN_FREQ,MAX_FREQ
     END IF

     NTXD = 1
      NRX = 1
     READ(NR,*)  ISW, NSX1, STEP, IUNITS, NCHNL, KRXW, OFFTYM
     WRITE(NW,4) ISW, NSX1, STEP, IUNITS, NCHNL, KRXW, OFFTYM
     IF (IUNITS < 1 .OR. IUNITS > 3) CALL WRITE_LOG_FILE (NLG,19,MXERR,1)
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

     ! Read in Tx area, turns and the number of receivers.

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
     ! or impulse output and B calibration. It isn't used for the pure rectangular
     ! step waveform.

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
     !IF (NEWF) WRITE(ND,'(3F10.2)')  ZRX0, XRX0, YRX0
     WRITE(NW,18) ZRX0, XRX0, YRX0
     RHO = ABS (XRX0) + ABS (YRX0)
     IF (RHO < 1. .AND. KPPM > 0) KPPM = 3

   ELSE IF (TDFD == 2) THEN                ! Frequency-domain systems

     OPEN(ND,FILE = 'SamayaAir.frq',STATUS = 'REPLACE')

     ! Allocate TD arrays to unit dimension and null quantity.

     NCHNL = 1;  NSX = 1

     ALLOCATE (SWX(1),SWY(1,3),TRP(1),TXON(1),WAVEFORM(1),TMS(1),WTMS(1), &
               TOPN(1),TCLS(1))

     SWX=0.; SWY=0.; TRP=0.; TXON=0.; WAVEFORM=0.; TMS=0.; WTMS=0.; TOPN=0.; TCLS=0. 

     WRITE(NW,7)
     IF (IUNITS < 4) IUNITS = IUNITS + 3   ! Convert to B.  Default was 5

     BUNIT = BUNITS(IUNITS)

     READ(NR,*)  NFRQ, CMP, NPPF

     NTXD = NFRQ

     IF (NPPF < 1 .OR. NPPF > 4) NPPF = 3

     IF (CMP == -1) THEN
       TXA90 = .TRUE.
       CMP = 1
       WRITE(NW,8)
       IF (NPPF < 1 .OR. NPPF > 4) NPPF = 3
     END IF
     IF (CMP < 1 .OR. CMP > 3) THEN
       CALL WRITE_LOG_FILE (NLG,13,MXERR,1)
       CMP = 1
     END IF

     PUNIT = PUNITS(NPPF)
     QUNIT = PUNIT
     IF (CMP > 1) QUNIT = BUNIT
     PPFAC = PPFACS(NPPF)
     BFFAC = BFFACS(IUNITS)
     WRITE(NW,19) NFRQ,CMP,NPPF,QUNIT

     NRX = NFRQ
     NRXST = NFRQ

     ALLOCATE (PRM_FD(NFRQ),FREQ(NFRQ),ZRX(NFRQ),XRX(NFRQ),YRX(NFRQ), & 
               TXCLN(NFRQ),TXDEG(NFRQ),CONFIG(NFRQ),CFG1(NFRQ))

     ZRX = 0.;  XRX = 0.;  YRX = 0.;  FREQ = 0;  TXCLN = 0.;  PRM_FD = 0.

     DO JF = 1,NFRQ
       IF (TXA90) THEN
         TXCLN(JF) = 90.
         READ(NR,*) FREQ(JF),ZRX(JF),XRX(JF),YRX(JF)
       ELSE
         READ(NR,*) FREQ(JF),ZRX(JF),XRX(JF),YRX(JF),TXCLN(JF)
       END IF
     END DO

     CONFIG = '   '
     IF (CMP == 1) CALL CONFIG_ID (NFRQ,TXCLN,TXA90,XRX,YRX,ZRX,CONFIG,CFG1)

     A1 = 0.
     IF (TXA90) A1 = 90.
     DO JF = 1,NFRQ
       IF (CMP < 2) THEN
         WRITE(NW,21) JF,FREQ(JF),TXCLN(JF),A1,ZRX(JF),XRX(JF),YRX(JF),CONFIG(JF)
       ELSE
         WRITE(NW,21) JF,FREQ(JF),TXCLN(JF),A1,ZRX(JF),XRX(JF),YRX(JF)
       END IF
     END DO
   END IF                     ! End frequency-domain specifics.

   ! RECORD 9.

   IF (INVERT) THEN

     READ(NR,*) NSTAT  
     IF (NSTAT > 1) THEN
       DO JF = 1,NSTAT
         READ(NR,'(A)') INP
       END DO
     END IF

   ELSE

     ! Flight path details for forward modelling only. Convert FANGLE & TXCLN to radians.

     READ(NDR,*)  NSTAT,SURVEY,BAROMTRC,LINE_TAG
     SURVEY = ABS (SURVEY)
     WRITE(NW,22) NSTAT,SURVEY,BAROMTRC,LINE_TAG
     IF (NSTAT < 2) CALL WRITE_LOG_FILE (NLG,15,MXERR,2)
     IF (SURVEY <1 .OR. SURVEY > 3) THEN
       CALL WRITE_LOG_FILE (NLG,16,MXERR,2)
     ELSE IF (TDFD == 2 .AND. SURVEY > 2) THEN
       CALL WRITE_LOG_FILE (NLG,16,MXERR,2)
     END IF

     ! NRXST is used to dimension the transmitter-receiver offsets and transmitter
     ! orientation.  For time-domain systems, the offset is constant with frequency
     ! but can vary with station => NRXST = NSTAT

     ! With frequency-domain systems, the offset is constant along the survey but
     ! can vary with frequency => NRXST = NFRQ

     ! NRX is used to dimension absolute receiver locations as a function of
     ! frequency, so NRX = NFRQ for frequency-domain systems but
     ! NRX = 1 for time-domain systems

     IF (TDFD < 2) THEN
       NRXST = NSTAT
       ALLOCATE (ZRX(NSTAT),XRX(NSTAT),YRX(NSTAT),TXCLN(NSTAT),TXDEG(NSTAT))
       TXCLN = TXCLN0; ZRX = ZRX0; XRX = XRX0; YRX = YRX0
     END IF

     ALLOCATE (LINE(NSTAT),SX(NSTAT),SY(NSTAT),SZ(NSTAT),FANGLE(NSTAT), &
               BEARING(NSTAT),SXD(NSTAT),SYD(NSTAT),RX(NSTAT,NRX),RY(NSTAT,NRX),      &
               RZ(NSTAT,NRX),RXD(NSTAT,NRX),RYD(NSTAT,NRX))

     LINE = 1000; SX=0.; SY=0.; SZ = 0.; FANGLE = 0.; BEARING=0.
     SXD = 0.; SYD = 0.; RX=0.; RY=0.; RZ = 0.; RXD=0.; RYD=0.; 

     ! Read in course for forward modelling.
     ! Read in course + data to be inverted for inversion.

     IF (SURVEY == 1) THEN
       IF (LINE_TAG == 1) THEN
         IF (DO3D == 1) READ(NDR,*) LINE(1),SYD(1),SXD(1),SZ(1),BEARING(1),DSTAT
         IF (DO3D == 2) READ(NDR,*) LINE(1),SYD(1),SXD(1),SZ(1),BEARING(1),DSTAT
       ELSE
         IF (DO3D == 1) READ(NDR,*) SYD(1),SXD(1),SZ(1),BEARING(1),DSTAT
         IF (DO3D == 2) READ(NDR,*) SYD(1),SXD(1),SZ(1),BEARING(1),DSTAT
       END IF

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
             IF (DO3D == 1) READ(NDR,*) LINE(JS),SYD(JS),SXD(JS),SZ(JS)
             IF (DO3D == 2) READ(NDR,*) LINE(JS),SYD(JS),SXD(JS),SZ(JS)
           ELSE
             IF (DO3D == 1) READ(NDR,*) SYD(JS),SXD(JS),SZ(JS)
             IF (DO3D == 2) READ(NDR,*) SYD(JS),SXD(JS),SZ(JS)
           END IF
         ELSE IF (ABS (SURVEY) == 3) THEN
           IF (TDFD < 2) THEN
              IF (LINE_TAG == 1) THEN
                IF (DO3D == 1) READ(NDR,*) LINE(JS),SYD(JS),SXD(JS),SZ(JS),TXCLN(JS),ZRX(JS),XRX(JS),YRX(JS)
                IF (DO3D == 2) READ(NDR,*) LINE(JS),SYD(JS),SXD(JS),SZ(JS),TXCLN(JS),ZRX(JS),XRX(JS),YRX(JS)
              ELSE
                IF (DO3D == 1) READ(NDR,*) SYD(JS), SXD(JS), SZ(JS), TXCLN(JS), ZRX(JS), XRX(JS), YRX(JS)
                IF (DO3D == 2) READ(NDR,*) SYD(JS), SXD(JS), SZ(JS), TXCLN(JS), ZRX(JS), XRX(JS), YRX(JS)
              END IF
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

     IF (ABS (SURVEY) > 1) THEN   ! Bearing correction for new line.
       FANGLE(1) = FANGLE(2)
       DO JS = 2,NSTAT-2
         IF (ABS (FANGLE(JS+1) - FANGLE(JS)) > TURN) FANGLE(JS+1) = FANGLE(JS+2)
       END DO
     END IF

     BEARING = FANGLE * 180. / PI

     TXDEG = TXCLN
     TXCLN = TXCLN * PI / 180.

     IF (TDFD < 2) THEN
       WRITE(NW,25) NSTAT
       DO JS = 1, NSTAT
         WRITE(NW,26) LINE(JS),JS,SYD(JS),SXD(JS),SZ(JS),BEARING(JS),TXCLN(JS),ZRX(JS),XRX(JS),YRX(JS)
       END DO
     ELSE
       WRITE(NW,27) NSTAT
       DO JS = 1, NSTAT
         WRITE(NW,28) LINE(JS),JS,SYD(JS),SXD(JS),SZ(JS),BEARING(JS)
       END DO
     END IF

     ! Writing to save file for restart survey info.

     IF (DO3D == 2) THEN
       IF (TDFD < 2) THEN
         IF (SURVEY == 1) THEN
           IF (LINE_TAG == 1) THEN
             CSF = COS (FANGLE(1))
             SNF = SIN (FANGLE(1))
             RXD(JS,1) = SXD(1) - REAL(XRX(1)*CSF - YRX(1)*SNF)
             RYD(JS,1) = SYD(1) - REAL(XRX(1)*SNF + YRX(1)*CSF)
             RZ(JS,1) = SZ(1) - REAL(ZRX(1))
           ELSE
             CSF = COS (FANGLE(1))
             SNF = SIN (FANGLE(1))
             RXD(JS,1) = SXD(1) - REAL(XRX(1)*CSF - YRX(1)*SNF)
             RYD(JS,1) = SYD(1) - REAL(XRX(1)*SNF + YRX(1)*CSF)
             RZ(JS,1) = SZ(1) - REAL(ZRX(1))
           END IF
         ELSE
           DO JS = 1, NSTAT
             CSF = COS (FANGLE(JS))
             SNF = SIN (FANGLE(JS))
             IF (ABS (SURVEY) == 2) THEN
               IF (LINE_TAG == 1) THEN
                 RXD(JS,1) = SXD(JS) - REAL(XRX(JS)*CSF - YRX(JS)*SNF)
                 RYD(JS,1) = SYD(JS) - REAL(XRX(JS)*SNF + YRX(JS)*CSF)
                 RZ(JS,1) = SZ(JS)  - REAL(ZRX(JS))
               ELSE
                 RXD(JS,1) = SXD(JS) - REAL(XRX(JS)*CSF - YRX(JS)*SNF)
                 RYD(JS,1) = SYD(JS) - REAL(XRX(JS)*SNF + YRX(JS)*CSF)
                 RZ(JS,1) = SZ(JS)  - REAL(ZRX(JS))
               END IF
             ELSE IF (ABS (SURVEY) == 3) THEN
               IF (LINE_TAG == 1) THEN
                 XRX(JS) = REAL(SXD(JS)) - REAL(XRX(JS)*CSF - YRX(JS)*SNF)
                 YRX(JS) = REAL(SYD(JS)) - REAL(XRX(JS)*SNF + YRX(JS)*CSF)
                 ZRX(JS) = SZ(JS)  - REAL(ZRX(JS))
               ELSE
                 XRX(JS) = REAL(SXD(JS)) - REAL(XRX(JS)*CSF - YRX(JS)*SNF)
                 YRX(JS) = REAL(SYD(JS)) - REAL(XRX(JS)*SNF + YRX(JS)*CSF)
                 ZRX(JS) = SZ(JS)  - REAL(ZRX(JS))
               END IF
             END IF
           END DO
         END IF
       END IF
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
    /, 2x, '| License:  ', a, &
    /, 2x, '|', &
    /, 2x, '| Compiler: ', a, &
    /, 2x, '| Options:  ', a, &
    /, 2x, '|', &
    /, 2x, '| Started:  ', i4.4, '-', i2.2, '-', i2.2, 'T', i2.2, ':', i2.2, ':', i2.2, &
    /, 2x, 78('-'), /)

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
 18 FORMAT(/T3,'Initial Rx offset relative to Tx:',F7.1,' Below,',F7.1,' Behind,',F6.1,' Starboard')
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
 97 FORMAT(//T3,'SamAir task started at ',I2.2,':',I2.2,' on',I3.2,1X,A,I5//)

   END SUBROUTINE READ_SYSTEM_AND_SURVEY

   !-------------------------------------------------------------------------
   ! MODULE: SA_Input_Routines !
   !-------------------------------!

   SUBROUTINE READ_MODEL_DATA

   !-------------------------------------------------------------------------

   !  ***  Called by MAIN

   !    Read in the nodes in double precision.
   !    Remember to change the vertical sign.
   !
   !    Define ECNTRD & NCNTRD as the mesh midpoint
   !    if ABS (ECNTRD) < 2.D3   set ECNTRD = 0.D0
   !    if ABS (NCNTRD) < 2.D3   set NCNTRD = 0.D0
   !
   !     lith is set up as lith (ie, in iz)
   !     USE is a bad variable name.

   IMPLICIT NONE
   INTEGER TOT_NODES, KN, KE, KZ, J1, I, J, K,  &
           OUTPUT,ILYTH,JZAIR,LITH_SPEC
   REAL, ALLOCATABLE :: STT(:,:,:)
   REAL(KIND=QL) BNDSIZE 

   LITH_SPEC = 2

   ! Layered model specification.

   READ(NDR,*) NLYR, NLITH

   READ_RES_FILE = .FALSE.
   IF (NLITH < 0) THEN
     READ_RES_FILE = .TRUE.
     NLITH = ABS(NLITH)
   END IF

   IF (NLYR > 1) CALL WRITE_LOG_FILE (NLG,51,MXERR,2)
   IF (NLYR > 2) CALL WRITE_LOG_FILE (NLG,52,MXERR,2)

   ALLOCATE (THK(NLYR),RES(NLYR),CHRG(NLYR),CALF(NLYR),CTAU(NLYR),CFREQ(NLYR), &
             ICOLE(NLYR),RMU(NLYR),REPS(NLYR),LITHL(NLYR),EPSMU(NLYR),CNDMU(NLYR), &
             LYTH(NLITH+1,NPROP))
   THK=0; RES=0; CHRG=0; CALF=0; CTAU=0; CFREQ=0; ICOLE=0; RMU=0; REPS=0; LITHL=0; EPSMU=0; CNDMU=0

   !  Initialise lithology list.

   LYTH(1:NLITH+1, 1) = -1.   !  blank resistivity indicator
   LYTH(1:NLITH+1, 2) = -1.   !  blank conductance (SIG_T) indicator
   LYTH(1:NLITH+1, 3) = 1.    !  Relative magnetic permeabilities
   LYTH(1:NLITH+1, 4) = 1.    !  Relative dielectric constants
   LYTH(1:NLITH+1, 5) = 0.    !  Chargeabilities
   LYTH(1:NLITH+1, 6) = 0.    !  CTAUs
   LYTH(1:NLITH+1, 7) = 1.    !  CFREQs

   WRITE(NW,3) 
   WRITE(np,8) NLITH
   DO J = 1,NLITH
     READ (NDR,*) LYTH(J,1:NPROP)
     WRITE(NW,'(I4,T8,G12.4,T22,F7.1,F12.3,F11.3,F10.2,G12.3,F8.2)') J,LYTH(J,1:NPROP)
     WRITE(np,9) J,LYTH(J,1:NPROP)
     IF (LYTH(J,1) < 0 .AND. LYTH(J,2) < 0) CALL WRITE_LOG_FILE (NLG,26,MXERR,2)
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

   THICK = 0.
   WRITE(NW,5)
   IF (NLYR > 1) THEN
     DO J = 1, NLYR-1
       READ (NDR,*) LITHL(J), THK(J)
     END DO
     THICK = THK(1)
   END IF
   READ(NDR,*) LITHL(NLYR)

   DO JL = 1, NLYR
     RES(JL)  =  LYTH(LITHL(JL),1)
     RMU(JL)  =  LYTH(LITHL(JL),3)
     REPS(JL) =  LYTH(LITHL(JL),4)
     CHRG(JL) =  LYTH(LITHL(JL),5)
     CTAU(JL) =  LYTH(LITHL(JL),6)
     CFREQ(JL) = LYTH(LITHL(JL),7)
     CALF(JL) = 1. - CHRG(JL)
     CNDMU(JL) = 4.E-7 * PI * RMU(JL) / RES(JL)     !  Set layer constants.
     EPSMU(JL) = EPSMU0 * RMU(JL) * REPS(JL)
   END DO

   WRITE(NW,30)
   IF (LITH_SPEC == 2) THEN
     WRITE(NW,31)
     DO JL = 1, NLYR
       IF(JL == NLYR) THEN
         WRITE(NW,32) NLYR,RES(NLYR),RMU(NLYR),REPS(NLYR),CHRG(NLYR),CTAU(NLYR),CFREQ(NLYR)
       ELSE
         WRITE(NW,33) JL,THK(JL),RES(JL),RMU(JL),REPS(JL),CHRG(JL),CTAU(JL),CFREQ(JL)
       END IF
     END DO
   ELSE
     WRITE(NW,34)
     DO  JL = 1, NLYR
       IF (JL < NLYR) THEN
         WRITE(NW,35) JL,THK(JL),RES(JL)
       ELSE
         WRITE(NW,36) NLYR,RES(NLYR)
       END IF
     END DO
   END IF

   ! Start defining the domain.

   WRITE(NW,1)
   READ(NDR,*) NSN, NSZ, NSE, KACC, SOLVER, OUTPUT
   WRITE(NW,2) NSN, NSZ, NSE, KACC, SOLVER
   IF (OUTPUT /= 10)  OUTPUT = 10

   IF (KACC /= 1) THEN
     KACC = 1
     CALL WRITE_LOG_FILE (NLG,20,MXERR,1)
   END IF
   IF (SOLVER /= 1) THEN
     SOLVER = 1
   END IF

   TOT_NODES = NSE * NSN * NSZ
   IF (TOT_NODES > 2000)   CALL WRITE_LOG_FILE (NLG,23,MXERR,1)

   NEL = 1 ; NER = 1 ; NNL = 1 ; NNR = 1 ; NZT = 1 ; NZB = 1

   NE = NEL + NSE + NER
   NN = NNL + NSN + NNR
   NZ = NZT + NSZ + NZB

   NPART = (NN        -1)*(NE        -1)*(NZ        -1)
   NPAR  = (NN-NNL-NNR-1)*(NE-NEL-NER-1)*(NZ-NZT-NZB-1)

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

   ALLOCATE (ELOCD(NE,NN,NZ),NLOCD(NE,NN,NZ),ZLOCD(NE,NN,NZ),LITH(NE,NN,NZ), &
             NSURFD(NSN,NSE),ESURFD(NSN,NSE),ZSURFD(NSN,NSE))

   ELOCD = 0.D0 ; NLOCD = 0.D0 ; ZLOCD = 0.D0 ; LITH = 0
   NSURFD = 0.D0 ; ESURFD = 0.D0 ; ZSURFD = 0.D0

   WRITE (np,6) TOT_NODES,NSN,NSE,NSZ

   IF (DO3D == 2)  RETURN
   LITH = 0
   DO J1 =  1, TOT_NODES

      READ(NDR,*) KN,KZ,KE,NLOCD(KE+1,KN+1,KZ+1), ZLOCD(KE+1,KN+1,KZ+1), &
                           ELOCD(KE+1,KN+1,KZ+1), ILYTH

      LITH(KE+1,KN+1,KZ+1) = ILYTH

      WRITE(np,7) KN,KZ,KE,NLOCD(KE+1,KN+1,KZ+1), -ZLOCD(KE+1,KN+1,KZ+1), &
                   ELOCD(KE+1,KN+1,KZ+1),LITH(KE+1,KN+1,KZ+1)

      IF (KZ == 1) THEN   ! Set up surface node array
        NSURFD(KN,KE) =  NLOCD(KE+1,KN+1,2)
        ESURFD(KN,KE) =  ELOCD(KE+1,KN+1,2)
        ZSURFD(KN,KE) =  ZLOCD(KE+1,KN+1,2)
      END IF
   END DO

   ! Extend the mesh the left, right, top and bottom

   BNDSIZE = 20.D0

   ELOCD(1   ,1:NN,1:NZ) = ELOCD(2   ,1:NN,1:NZ) - BNDSIZE
   ELOCD(NE  ,1:NN,1:NZ) = ELOCD(NE-1,1:NN,1:NZ) + BNDSIZE
   NLOCD(1   ,1:NN,1:NZ) = NLOCD(2   ,1:NN,1:NZ)
   NLOCD(NE  ,1:NN,1:NZ) = NLOCD(NE-1,1:NN,1:NZ)
   ZLOCD(1   ,1:NN,1:NZ) = ZLOCD(2   ,1:NN,1:NZ)
   ZLOCD(NE  ,1:NN,1:NZ) = ZLOCD(NE-1,1:NN,1:NZ)
   DO K = 1, NZ-1
      DO J = 1, NN-1
         LITH(1    ,J, K) = LITHL(NLYR)
         LITH(NE-1 ,J, K) = LITHL(NLYR)
      END DO
   END DO

   ELOCD(1:NE,1   ,1:NZ) = ELOCD(1:NE,2   ,1:NZ)
   ELOCD(1:NE,NN  ,1:NZ) = ELOCD(1:NE,NN-1,1:NZ)
   NLOCD(1:NE,1   ,1:NZ) = NLOCD(1:NE,2   ,1:NZ) - BNDSIZE
   NLOCD(1:NE,NN  ,1:NZ) = NLOCD(1:NE,NN-1,1:NZ) + BNDSIZE
   ZLOCD(1:NE,1   ,1:NZ) = ZLOCD(1:NE,2   ,1:NZ)
   ZLOCD(1:NE,NN  ,1:NZ) = ZLOCD(1:NE,NN-1,1:NZ)
   DO K = 1, NZ-1
      DO I = 1, NE-1
         LITH(I, 1    ,K) = LITHL(NLYR)
         LITH(I, NN-1 ,K) = LITHL(NLYR)
      END DO
   END DO

   ELOCD(1:NE,1:NN,1)    = ELOCD(1:NE,1:NN,2)
   ELOCD(1:NE,1:NN,NZ)   = ELOCD(1:NE,1:NN,NZ-1)
   NLOCD(1:NE,1:NN,1)    = NLOCD(1:NE,1:NN,2)
   NLOCD(1:NE,1:NN,NZ)   = NLOCD(1:NE,1:NN,NZ-1)
   ZLOCD(1:NE,1:NN,1)    = ZLOCD(1:NE,1:NN,2)    +  20.D0
   ZLOCD(1:NE,1:NN,NZ)   = ZLOCD(1:NE,1:NN,NZ-1) -  BNDSIZE
   DO J = 1, NN-1
      DO I = 1, NE-1
         LITH(I, J, 1)    = LITHL(NLYR)
         LITH(I, J, NZ-1) = LITHL(NLYR)
      END DO
   END DO

   NCNTRD = 0.5D0 * (MAXVAL (NLOCD) + MINVAL (NLOCD) )      !  Working origin
   ECNTRD = 0.5D0 * (MAXVAL (ELOCD) + MINVAL (ELOCD) )
   IF (ABS(ECNTRD) < 2500.D0) ECNTRD = 0.D0
   IF (ABS(NCNTRD) < 2500.D0) NCNTRD = 0.D0

   ! Convert arrays into computation coordinates (singular precision).

   ALLOCATE (ELOC (NE,NN,NZ),NLOC(NE,NN,NZ),ZLOC(NE,NN,NZ), &
             NSURF(NSN,NSE),ESURF(NSN,NSE),ZSURF(NSN,NSE) )

   ELOC  =  REAL (ELOCD - ECNTRD)
   NLOC  =  REAL (NLOCD - NCNTRD)
   ZLOC  = -REAL (ZLOCD)
   ESURF =  REAL (ESURFD - ECNTRD)
   NSURF =  REAL (NSURFD - NCNTRD)
   ZSURF =  REAL (ZSURFD)

   CALL SET_SURVEY (NLG,NW,NSTAT,FANGLE,SXD,SYD,SX,SY,SZ,BAROMTRC,NRX,XRX,YRX,ZRX, &
                    RX,RY,RZ,RXD,RYD,ECNTRD,NCNTRD,NSN,NSE,NSURF,ESURF,ZSURF,JZAIR,TDFD)

   ZLOC = -REAL (ZLOCD)

   DEALLOCATE (ELOCD, NLOCD, ZLOCD, NSURFD, ESURFD, ZSURFD )

   IF (.NOT. INVERT) THEN      ! Generate SamayaAir.map file.
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
     CALL RESMAP (NE,NN,NZ,STT,NLOC,ZLOC)
     DEALLOCATE (STT)
   END IF

   ALLOCATE(XPART(NPART)) ; XPART = 0.

   IF (DO3D < 2) THEN
     DO J = 1, NN-1
       DO K = 1, NZ-1
         DO I = 1, NE-1
           NBN = (J-1)*(NZ-1)*(NE-1) + (K-1)*(NE-1) + I
           KLITH = LITH(I,J,K)
           XPART(NBN) = LYTH(KLITH,1)    ! Resistivity vector.
         END DO
       END DO
     END DO
   END IF

   IF (READ_RES_FILE) THEN
     WRITE(*,29) ; WRITE(NW,29) 
     OPEN(NM,FILE='SamayaAir.res',STATUS='OLD')
     DO J = NNL+1, NN-NNR-1
       DO K = NZT+1, NZ-NZB-1
         DO I = NEL+1, NE-NER-1
           NBNT = (J-1)*(NZ-1)*(NE-1) + (K-1)*(NE-1) + I       ! Total domain.
           READ(NM,*) JP, S1
           XPART(NBNT) = S1
         END DO
       END DO
     END DO
     CLOSE(NM)
   END IF

    1 FORMAT(//T6,'LITHOLOGY AND MESH DESCRIPTION' &
              /T6,'------------------------------')
    2 FORMAT(/T3,'Number of node "slices" along North-South axis =',I3 &
             /T3,'Number of node rows in each slice =',I4              &
             /T3,'Number of node columns in each slice =',I4,          &
             /T3,'KACC =',I2,4X,'SOLVER =',I2)
    3 FORMAT(//T27,'LITHOLOGY PROPERTIES'/T27,'--------------------' &
             //T35,'Relative   Relative     Cole-Cole Parameters'    &
              /T9,'Resistivity  Conductance     MU     Dielectric   CHRG    CTAU       CFREQ'/)
    4 FORMAT(/T3,'Lithology',I3,' has a negative resistivity.')
    5 FORMAT(//T3,'LAYERED EARTH INPUT DATA'/T3,'------------------------'/)
    6 FORMAT( T1,'/ TOTAL_NODES=',I5.5,' NN=',I3.3,' NE=',I3.3,' NZ=',I3.3, &
             /T1,'/ JN       JZ       JE     NLOC     ZLOC     ELOC     LITH')
    7 FORMAT(T1,'/ ',I2,T12,I2,T21,I2,3F9.0,T54,I5)
    8 FORMAT( T1,'/ NLITH=',I2.2, &
             /T1,'/ LITH    RES           Cond        MU         EPS       CHRG    CTAU        CFREQ')
    9 FORMAT(T1,'/',I4,T8,G12.4,T22,F7.1,F12.3,F11.3,F10.2,G12.3,F8.2)
   29 FORMAT(//T3,'Element resistivities read from file, SamayaAir.res.')
   30 FORMAT(//10X,'+----------------------------------+'/ &
                 10X,'+  Layered Earth Model Parameters  +'/ &
                 10X,'+----------------------------------+'/)
   31 FORMAT(T2,'Layer  Thickness  Resistivity   MU-R   EPS-R   CHRG    CTAU      CFREQ' &
            /T2,'-----  ---------  -----------   ----   -----   ----    ----      -----')
   32 FORMAT(I4,11X,  G15.4,2F7.2,F8.2,G11.2,F7.2)
   33 FORMAT(I4,F11.1,G15.4,2F7.2,F8.2,G11.2,F7.2)
   34 FORMAT(10X, ' Layer    Thickness   Resistivity'/ &
             10X, ' -----    ---------   -----------')
   35 FORMAT(10X,I4,4X,2G13.4)
   36 FORMAT(10X,I4,17X,G13.4)

   END SUBROUTINE READ_MODEL_DATA

   !-------------------------------------------------------------------------
   ! MODULE: SA_Input_Routines !
   !-------------------------------!

   SUBROUTINE SET_FRQ
!  ------------------

   REAL(KIND=QL), PARAMETER :: MID = 10._QL
   INTEGER J,PPDH,PPDL
   REAL(KIND=QL) QFRQ12,QFRQL,QFRQH,MIN_FREQD, MAX_FREQD
   REAL(KIND=QL), ALLOCATABLE :: FDUM(:)

   ALLOCATE (FDUM(1000))
   IF (TDFD == 1) THEN
     FDUM(1) = 1._QL
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

   !-------------------------------------------------------------------------
   ! MODULE: SA_Input_Routines !
   !-------------------------------!

   SUBROUTINE SET_TRP

   !-------------------------------------------------------------------------
   !
   !  ***  Called by: MAIN
   !  ***       Uses:  MODULE FILTER_COEFFICIENTS
   !
   !    Sets up interpolation times for FD -> TD transform which use the
   !    exact 6 points per decade frequency-domain data plus 6 per decade
   !    interpolated values.  These are based on a 12 point per decade
   !    cosine filter derived from the Niels Christensen routine FILCOA
   !    with OMEGA = .3 PI and shift 0.
   !
   !               OUTPUT
   !               ------
   !
   !          TRP - array of time values for FD -> TD transformations
   !        NTYRP - number of values in TRP
   !       EXTENT - the latest time for which time-domain output is required.
   !        PULSE - time length of one signal pulse
   !       NTYPLS - number of TRP values in 1 PULSE
   !
   !-------------------------------------------------------------------------

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

   !-------------------------------------------------------------------------
   ! MODULE: SA_Input_Routines !
   !-------------------------------!

   SUBROUTINE RESMAP (NE,NN,NZ,STT,NLOC,ZLOC)

   !-------------------------------------------------------------------------
   !
   !**** Called by READ_2D_DATA
   !
   ! Produces a 2D character map of resistivities.
   !
   ! NE & NZ are the number of cells in the East and depth directions
   ! respectively.
   !
   ! RES(J,I) = resistivity associated with node in column I, row J.
   !
   !-------------------------------------------------------------------------

   IMPLICIT NONE
   REAL, PARAMETER :: TOL=.01
   INTEGER NRES,NZ,NN,NE,I,J,K,KR
   REAL STT(NE,NN,NZ), RESVAL(52), TST1, NLOC(NE,NN,NZ), ZLOC(NE,NN,NZ)
   CHARACTER (LEN=1) RESCHAR(52), MAP(100,100)
   DATA RESCHAR /'1','2','3','4','5','6','7','8','9','0','A','B','C','D','E','F', &
               'G','H','J','K','L','M','N','P','R','S','T','U','W','X','Y','a', &
               'b','c','d','e','f','g','h','j','k','l','m','n','p','r','s','t', &
               'u','w','x','y'/

   WRITE(NMP,'(/T11,A)') 'RESISTIVITY MAP'
   WRITE(NMP,'(T11,A)')  '---------------'

   NRES = 0
   RESVAL = -999.
   DO J = 2, NN-2
     WRITE(NMP,'(/T11,A,F9.2/)') 'Y-Cord. = ', NLOC(1,J,2)
     MAP = '_'
     DO K = 2, NZ-2
       HORIZ_CELL_LOOP: DO I = 2, NE-2
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
       WRITE(NMP,'(T11,F9.2,3X,120A)') -ZLOC(1,J,K),MAP(2:NE-2,K)
     END DO
   END DO

   WRITE(NMP,1)
   DO KR = 1,NRES
     WRITE(NMP,'(T4,A,T11,G13.2)') RESCHAR(KR),RESVAL(KR)
     IF (RESVAL(KR) < 0 ) CALL WRITE_LOG_FILE (NLG,22,MXERR,2)
   END DO

   CLOSE (NMP)

   1 FORMAT(//T3,'RESISTIVITIES DESCRIPTION',  &
             /T3,'-------------------------'/, &
             /T3,'INDEX      RESISTIVITY'/)

   END SUBROUTINE RESMAP

   !-------------------------------------------------------------------------
   ! MODULE: SA_Input_Routines !
   !-------------------------------!

   SUBROUTINE WRITE_np_HEADER

   WRITE(np,1) FVN,PVC,TRIM(TITLE)

   1 FORMAT(T1,'/ ',I4.4,T15,'File version number'/T1,'/ PROGRAM_NAME=',A/T1,'/ TITLE: ',A)

   END SUBROUTINE WRITE_np_HEADER

   !-------------------------------------------------------------------------
   ! MODULE: SA_Input_Routines !
   !-------------------------------!

   SUBROUTINE WRITE_np_SYS_DATA

   !------------------------------------------------------------------------
   !
   !***  Called by: MAIN
   !
   ! Sets up the initial part of the output plotting file for inversion.
   !
   !------------------------------------------------------------------------

   INTEGER NCMP
   REAL VERR(MCHNL)
   LOGICAL WL
   CHARACTER(LEN=5) CHZ,CHX,CHY,CHT,WEZ,WEX,WEY,WET,WEQ,WEI
   CHARACTER(LEN=6),DIMENSION(NFRQ) :: QFRQ,IFRQ
   CHARACTER(LEN=82) QL0,QL1

   IF (TDFD < 2) THEN
     NCMP = CMP
     IF (CMP > 10) NCMP = 1
     CHZ = '  CHZ';  WEZ = '  WEZ'
     CHX = '  CHX';  WEX = '  WEX'
     CHY = '  CHY';  WEY = '  WEY'
     CHT = '  CHT';  WET = '  WET'

     WRITE(np,3) TRIM (QUNIT),NSTAT,NCHNL,NCMP
     WRITE(np,4) TMS(1:NCHNL)
     WRITE(np,5) WTMS(1:NCHNL)
     WRITE(np,6)
       IF (INVERT) THEN
       IF (CMP ==11) WRITE(np,7) (CHX,JT,JT=1,NCHNL), (WEX,JT,JT=1,NCHNL)
       IF (CMP ==13) WRITE(np,7) (CHZ,JT,JT=1,NCHNL), (WEZ,JT,JT=1,NCHNL)
       IF (CMP ==2)  WRITE(np,7) (CHZ,JT,JT=1,NCHNL), (CHX,JT,JT=1,NCHNL), &
                                  (WEZ,JT,JT=1,NCHNL), (WEX,JT,JT=1,NCHNL)
       IF (CMP ==3)  WRITE(np,7) (CHZ,JT,JT=1,NCHNL), (CHX,JT,JT=1,NCHNL), (CHY,JT,JT=1,NCHNL), &
                                  (WEZ,JT,JT=1,NCHNL), (WEX,JT,JT=1,NCHNL), (WEY,JT,JT=1,NCHNL)
       IF (CMP ==4)  WRITE(np,7) (CHT,JT,JT=1,NCHNL), (WET,JT,JT=1,NCHNL)
     ELSE
       IF (CMP ==11) WRITE(np,7) (CHX,JT,JT=1,NCHNL)
       IF (CMP ==13) WRITE(np,7) (CHZ,JT,JT=1,NCHNL)
       IF (CMP ==2)  WRITE(np,7) (CHZ,JT,JT=1,NCHNL), (CHX,JT,JT=1,NCHNL)
       IF (CMP ==3)  WRITE(np,7) (CHZ,JT,JT=1,NCHNL), (CHX,JT,JT=1,NCHNL), (CHY,JT,JT=1,NCHNL)
       IF (CMP ==4)  WRITE(np,7) (CHT,JT,JT=1,NCHNL)
     END IF
   ELSE
     WEQ = '  WEQ'
     WEI = '  WEI'
     DO JF = 1,NFRQ
       QFRQ(JF) = '  Q'//CONFIG(JF)
       IFRQ(JF) = '  I'//CONFIG(JF)
     END DO
     WRITE(np,13) TRIM (QUNIT),NSTAT,NFRQ
     WRITE(np,14) FREQ(1:NFRQ)
     WRITE(np,16)
     IF (INVERT) THEN
       WRITE(np,17) (IFRQ(JF),JF,JF=1,NFRQ),(QFRQ(JF),JF,JF=1,NFRQ), &
                     (WEI,JF,JF=1,NFRQ),(WEQ,JF,JF=1,NFRQ)
     ELSE
       WRITE(np,17) (IFRQ(JF),JF,JF=1,NFRQ),(QFRQ(JF),JF,JF=1,NFRQ)
     END IF
   END IF

   IF (INVERT) THEN
     WRITE(np,'(T1,A)') '/ SURVEY DATA'
     VERR = 0.
     DO JS = 1,NSTAT
       WL = .TRUE.
       IF (JS > 1 .AND. LINE(JS) == LINE(JS-1)) WL = .FALSE.
       IF (WL)  THEN
         WRITE(QL0,*) LINE(JS)
         READ(QL0,'(A)') QL1
         WRITE(np,11) TRIM (ADJUSTL (QL1))
       END IF
       IF (TDFD < 2) THEN
         WRITE(np,9) JS,SYD(JS),SXD(JS),SZ(JS),TXDEG(JS),RYD(JS,1),RXD(JS,1),RZ(JS,1), &
                      RDATA(1:MCHNL,JS),VERR(1:MCHNL)
       ELSE
         WRITE(np,10) JS,SYD(JS),SXD(JS),SZ(JS),RYD(JS,1),RXD(JS,1),RZ(JS,1), &
                       RDATA(1:MCHNL,JS),VERR(1:MCHNL)
       END IF
     END DO
   END IF

    3 FORMAT(T1,'/ UNITS=',A,3X,'NSTAT=',I3.3,3X,'NCH=',I3.3,3X,'NCMP=',I1)
    4 FORMAT(T1,'/ TIMES(ms)=',T17,100G13.4)
    5 FORMAT(T1,'/ CHNL_WDTH(ms)=',T17,100G13.4)
    6 FORMAT(T1,'/ SURVEY=TD_AEM  PP=RX_POS')
    7 FORMAT(T1,'/ LINE_HEADER'/T1,'/ DatIndx  EastTx  NorthTx  AltTx  Txcln  EastRx  NorthRx  AltRx',350(A,I3.3))
    9 FORMAT(T1,I5,2F12.1,F8.1,F6.1,2F12.1,F8.1,350G13.4:)
   10 FORMAT(T1,I5,2F12.1,F8.1,2F12.1,F8.1,100G13.4)
   11 FORMAT(T2,'Line ',A)
   13 FORMAT(T1,'/ UNITS=',A,3X,'NSTAT=',I3.3,3X,'NFRQ=',I2.2,3X,'NCMP=1')
   14 FORMAT(T1,'/ FREQS(Hz) =',60F13.2)
   16 FORMAT(T1,'/ SURVEY=FD_AEM  PP=RX_POS')
   17 FORMAT(T1,'/ LINE_HEADER'/T1,'/ DatIndx  EastTx  NorthTx  AltTx  EastRx  NorthRx  AltRx  ',100(A,I2.2))

   END SUBROUTINE WRITE_np_SYS_DATA

   !-------------------------------------------------------------------------
   ! MODULE: SA_Input_Routines !
   !-------------------------------!

   SUBROUTINE READ_INVRT_CNTRL

   !------------------------------------------------------------------------
   !
   !***  Called by MAIN
   !
   !  Set inversion controls.
   !
   !------------------------------------------------------------------------

   INTEGER NFIX,JP,LITH_INDX,CTYPE,KPAR,J1
   REAL E1,E2,E3,A1,A2,RDUM

   ! RECORD 13
   ! ---------

   READ(NR,*) RDUM,CNVRG,NFIX,MV1PRT,OUTPRT

   MAXITS = FLOOR(RDUM)

   WRITE(NW,10) MAXITS,CNVRG,NFIX,MV1PRT,OUTPRT
   IF (MV1PRT < 0 .OR. MV1PRT > 3) MV1PRT = 1
   IF (OUTPRT < 0 .OR. OUTPRT > 3) OUTPRT = 1

   IF (CNVRG /= 1 .AND. CNVRG /= 2) THEN
     CNVRG = 1
     CALL WRITE_LOG_FILE (NLG,205,MXERR,1)
   END IF

   ! RECORD 13.1
   ! -----------

   PCTCNV = 1.

   IF (CNVRG == 2) READ(NR,*) PCTCNV

   WRITE(NW,15) PCTCNV,MAXITS 
   WRITE(*,15)  PCTCNV,MAXITS

   ! Set the elasticity and bounds. For all lithologies, set the elasticity
   ! equal to one. Assign a very small lower bound on the resistivity, and
   ! a very large upper bound on the resitivity. These bounds are sufficiently
   ! large that they are not going to be exceeded in any realistic inversion.

   ALLOCATE (ELAS(NLITH),LBND(NLITH),UBND(NLITH),CXPAR(NLITH))

   DO JP = 1, NLITH
     CXPAR(JP) = 0       ! All parameters are free to vary.
     ELAS(JP)  = 1.      ! Elasticity equal to one (full update step).
     LBND(JP)  = 1.E-8   ! Ridiculous lower bound on resistivity that won't be exceeded.
     UBND(JP)  = 1.E+8   ! Ridiculous upper bound on resistivity that won't be exceeded.
   END DO

   ! RECORD 14
   ! ---------

   IF (NFIX > 0) THEN
     WRITE(NW,17)
     DO JP = 1, NFIX

       ! E1 - Elasticity
       ! E2 - Lower bound on resistivity.
       ! E3 - Upper bound on resistivity.

       ! Default values:

       E1 = 1.
       E2 = 1.E-8
       E3 = 1.E+8

       READ(NR,*) CTYPE
       BACKSPACE NR
       SELECT CASE (CTYPE)
       CASE (1)
         READ(NR,*) J1,LITH_INDX,KPAR
         E1 = 0.
       CASE (2)
         READ(NR,*) J1,LITH_INDX,KPAR,E1
       CASE (3)
         READ(NR,*) J1,LITH_INDX,KPAR,E1,E2,E3
       END SELECT

       IF (KPAR /= 1) KPAR = 1       ! KPAR = 1 corresponds to resistivity. No other options
                                     ! are currently available.

       IF (ABS(E1) < 0.05) E1 = 0.   ! Hold for elasticities < 0.05.
       IF (ABS(E1) > 0.95) E1 = 1.   ! Allow ful freedom for elasticities > 0.95.

       IF (E2 > E3) THEN             ! Switch if LB > UB.
         A1 = E3
         E3 = E2
         E2 = A1
       END IF
       A1 = E3 - E2
       A2 = 0.005 * (ABS(E2) + ABS(E3))
       IF (A1 < A2) E1 = 0.

       WRITE(NW,18) LITH_INDX,KPAR,E1,E2,E3

       CXPAR(LITH_INDX) = J1
       ELAS(LITH_INDX)  = E1
       LBND(LITH_INDX)  = E2   ! Lower bound on resistivity.
       UBND(LITH_INDX)  = E3   ! Upper bound on resistivity.

     END DO
     WRITE(NW,20)
   ELSE
     WRITE(NW,19)
   END IF

   10 FORMAT(T3,'MAXITS =',I3,3X,'CNVRG =',I2,3X,'NFIX =',I3,3X,'MV1PRT =',I2,3X,'OUTPRT =',I2)
   15 FORMAT(/T3,'The inversion will finish if the RMS error is less than',F5.1,' percent or if' &
             /T3,'the error can no longer be reduced significantly or after',I3,' iterations.')
   17 FORMAT(//T12,'Constrained Parameters'/T12,'----------------------' &
             //T5,'Lithology'                                                        &
              /T5,' Index      Parameter      Elasticity  Lower Bound   Upper Bound' &
              /T5,'---------   ---------      ----------  -----------   -----------')
   18 FORMAT(T7,I2,T20,I1,T31,G12.4,T43,G12.4,T58,G12.4)
   19 FORMAT(/T3,'All element resistivities will be allowed to vary during inversion.')
   20 FORMAT(/T3,70('-'))

   END SUBROUTINE READ_INVRT_CNTRL

   !-------------------------------------------------------------------------
   ! MODULE: SA_Input_Routines !
   !-------------------------------!

   SUBROUTINE READ_INVRT_DATA

   !------------------------------------------------------------------------
   !
   !***  Called by MAIN
   !
   !  Set inversion dimensions:
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
   !
   !------------------------------------------------------------------------

   INTEGER ORDER,MDCHNL,NDCMP,N0STAT,N0CHNL,N2B,N2E,N3B,N3E, &
           N0PTS,JP,J1
   INTEGER, ALLOCATABLE, DIMENSION(:) :: KCMP,K0STAT,K0CHNL
   REAL TDATA,DELX,DELY,RHO
   REAL, ALLOCATABLE,DIMENSION (:) :: QDATA,Q2DATA
   CHARACTER (LEN=1) TCHR
   CHARACTER (LEN=3) KCMPC(0:5)
   DATA KCMPC / '   ','HCP','VCP','VCA','VCB','HCA'/

   IF (TDFD < 2) THEN
     IF (CMP == 1) MCHNL = 1*NCHNL  ! 1 component data.
     IF (CMP == 2) MCHNL = 2*NCHNL  ! 2 component data.
     IF (CMP == 3) MCHNL = 3*NCHNL  ! 3 component data.
   ELSE
     MCHNL = 2*NFRQ                
   END IF

   WRITE(NW,1)
   IF (TDFD < 2) THEN
     IF (CMP == 13) THEN 
       WRITE(NW,2) ! Inversion of vertical component TD data.
       MCHNL = 1*NCHNL
     ELSE IF (CMP == 11) THEN 
       WRITE(NW,3) ! Inversion of inline component TD data.
       MCHNL = 1*NCHNL
     ELSE IF (CMP == 2) THEN 
       WRITE(NW,4) ! Joint inversion of vertical and inline components of TD data.
       MCHNL = 2*NCHNL
     ELSE IF (CMP == 3) THEN
       WRITE(NW,5)   ! Joint inversion of 3 components of TD data.
       MCHNL = 3*NCHNL
     END IF
     IF (KPPM == 0) THEN
       WRITE(NW,6) TRIM (QUNIT)
     ELSE
       WRITE(NW,7) TRIM (QUNIT)
     END IF
   ELSE IF (TDFD == 2) THEN
     WRITE(NW,8)
     WRITE(NW,7) TRIM (QUNIT)
   END IF

   IF (TDFD < 2) ALLOCATE (DATA_FLOOR(MCHNL),KCMP(1))
   IF (TDFD == 2) ALLOCATE (DATA_FLOOR(MCHNL),KCMP(NFRQ))

   DATA_FLOOR = 0.

   ! Start reading from SamayaAir.inv on UNIT NRI = 13. First, skip over
   ! all comment lines.
 	DO
     READ (NRI,'(A)') TCHR
     IF (.not.(IsComment(tchr))) EXIT
   END DO
   BACKSPACE (NRI)                                                          

   ! RECORD 15
   ! ---------

   IF (TDFD < 2) THEN
     READ(NRI,*)  NSTAT, SURVEY, BAROMTRC, KCMP(1), ORDER
     WRITE(NW,21) NSTAT, SURVEY, BAROMTRC, KCMP(1), ORDER
     SELECT CASE (CMP)
     CASE (11)
       IF (KCMP(1) == 3) CALL WRITE_LOG_FILE (NLG,202,MXERR,2)
     CASE (13)
       IF (KCMP(1) == 1) CALL WRITE_LOG_FILE (NLG,203,MXERR,2)
     CASE (2)
       IF (KCMP(1) == 1) CALL WRITE_LOG_FILE (NLG,210,MXERR,2)
       IF (KCMP(1) == 3) CALL WRITE_LOG_FILE (NLG,211,MXERR,2)
     CASE (3)
       IF (KCMP(1) == 1) CALL WRITE_LOG_FILE (NLG,212,MXERR,2)
       IF (KCMP(1) == 3) CALL WRITE_LOG_FILE (NLG,213,MXERR,2)
       IF (KCMP(1) > 4 .AND. KCMP(1) < 100) CALL WRITE_LOG_FILE (NLG,214,MXERR,2)
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

   !  SET SYSTEM DIMENSIONS
   !  ---------------------

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

   ALLOCATE (LINE(NSTAT),SX(NSTAT),SY(NSTAT),SZ(NSTAT),FANGLE(NSTAT),BEARING(NSTAT), &
             SXD(NSTAT),SYD(NSTAT),RX(NSTAT,NRX),RY(NSTAT,NRX), &
             RZ(NSTAT,NRX),RXD(NSTAT,NRX),RYD(NSTAT,NRX))

   SX=0.; SY=0.; RX=0.; RY=0.; RZ = 0.; RXD=0.; RYD=0.; FANGLE=0.

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

     DATA_FLOOR = DATA_FLOOR(1)

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
   ALLOCATE (RDATA(MCHNL,NSTAT),RWTS(MCHNL,NSTAT),XDATA(NDATA),XWTS(NDATA),XMODL(NDATA), &
             QDATA(MDCHNL),Q2DATA(MDCHNL))
   XDATA = 0.; RDATA = 0.
   XWTS = 1;  RWTS = 1

   READ(NRI,*)  N0STAT, N0CHNL, N0PTS
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

   !=======================
   !      DATA ENTRY      !
   !=======================

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
     END IF

     ! Put data in the order of all Z followed by all X followed by all Y
     ! depending upon which components are present.

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
   ! for both forward modelling and inversion.

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

     ! Data weights

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

   ! Construct the observed data and data weight vectors.

   DO JS = 1, NSTAT
     DO JT = 1, MCHNL
       JD = JT + (JS-1) * MCHNL
       XDATA(JD) = RDATA(JT,JS)
       XWTS(JD) = RWTS(JT,JS)
     END DO
   END DO

   ! Construct data normalisation vector.

   INRM = 1
   IF (DO3D == -2) INRM = 2

   ALLOCATE (DNORM(NDATA)) ; DNORM = 1.

   BIG = MAXVAL(ABS(XDATA))
   DO JT = 1, MCHNL  
     S1 = 0.
     DO JS = 1, NSTAT
       S1 = S1 + ABS(RDATA(JT,JS))
     END DO
     S1 = S1/REAL(NSTAT)
     S1 = MAX(S1,DATA_FLOOR(JT))
     IF (S1 < 1.0E-7 * BIG) S1 = 1.0E7 * BIG  ! Eliminate cross-over fluctuations.
     DO JS = 1, NSTAT
       JD = JT + (JS-1) * MCHNL
       DNORM(JD) = S1
     END DO
   END DO

    1 FORMAT(//T3,'------------------------------------' &
              /T3,'Inversion Controls & Data for SamAir' &
              /T3,'------------------------------------')
    2 FORMAT(/T3,'Inversion of Time-Domain Vertical Component Data.')
    3 FORMAT(/T3,'Inversion of Time-Domain In-Line Component Data.')
    4 FORMAT(/T3,'Joint Inversion of Time-Domain Vertical & In-Line Component Data.')
    5 FORMAT(/T3,'Joint Inversion of Time-Domain Three Component Data.')
    6 FORMAT(/T3,'The data to be inverted is expressed as ',A,'.')
    7 FORMAT(/T3,'The data to be inverted has been normalised to ',A,'.')
    8 FORMAT(/T3,'Inversion of Frequency-Domain Data.')
   21 FORMAT(//T3,'NSTAT =',I4,3X,'SURVEY =',I2,3X,'BAROMTRC =',I2,3X,'KCMP =',I4,3X,'ORDER =',I2)
   22 FORMAT(/T3,'Time-Domain Data Floor =',G12.4,1X,A)
   23 FORMAT(/T8,'Frequency  Data Floors (',A,')'/T8,'-----------------------------' &
            //T8,'Freq     In-phase   Quadrature'/)
   24 FORMAT(I4,F9.0,2G12.4)
   25 FORMAT(/T3,'N0STAT =',I4,3X,'N0CHNL =',I3,3X,'N0PTS =',I4)
   26 FORMAT(/T3,'Data from the following stations will be weighted to zero:'/T3,60I4)
   27 FORMAT(/T3,'Data from the following PCHNLs will be weighted to zero:'/T3,60I4)
   28 FORMAT(/T3,'Data from the following (PCHNL, STAT) pairs will be weighted to zero:')
   32 FORMAT(//T3,'NSTAT =',I4,3X,'SURVEY =',I2,3X,'BAROMTRC =',I2,3X,'ORDER = ',I4)
   33 FORMAT(/T3,'Inversion components:',20(I5,': ',A,F8.1))
   34 FORMAT(/T3,'Inversion components:',20(I5,': ',A,F10.1))

   45 FORMAT(/T3,'Components from SamayaAir.inv:',I4,20I5)
   46 FORMAT(/T32,20(2X,A))
   47 FORMAT(/T3,'Components from SamayaAir.cfl:',I4,20I5)
   40 FORMAT(I9,I7,F11.0,2F12.1,F9.1,30G13.4)
   41 FORMAT(//T6,'Line   Station   Bearing     East       North     Alt',T68,'Horizontal In-line Component Data' &
              /T6,'----   -------   -------     ----       -----     ---',T68,'----------------------------------')
   42 FORMAT(//T6,'Line   Station   Bearing     East       North     Alt',T68,'Horizontal Transverse Component Data' &
              /T6,'----   -------   -------     ----       -----     ---',T68,'------------------------------------')
   43 FORMAT(//T6,'Line   Station   Bearing     East       North     Alt',T68,'Vertical Component Data' &
              /T6,'----   -------   -------     ----       -----     ---',T68,'-----------------------')
   50 FORMAT(T1,I10,T13,30I2)
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
              /T3,'-------  -----------------------------------------------')
   101 FORMAT(T3,'Three cpomponent inversion has been specified (CMP = 3)' &
             /T3,'but three components are not read in: KCMP = ',I4)
   102 FORMAT(T3,'Two component inversion has been specified (CMP = 2)' &
             /T3,'but two components are not read in: KCMP = ',I4)
   103 FORMAT(T3,'Vertical component inversion has been specified (CMP = 13)' &
             /T3,'but this component is not read in: KCMP = ',I4)
   104 FORMAT(T3,'In-line component inversion has been specified (CMP = 11)' &
             /T3,'but this component is not read in: KCMP = ',I4)

   END SUBROUTINE READ_INVRT_DATA

!---------------------------------------------------------------------------

   !==================================================================================================

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

 END MODULE SA_Input_Routines

!===========================================================================

 PROGRAM MAIN

!---------------------------------------------------------------------------
!
!*** Calls FDREAD, LOKI_3D, SET_SOURCE, TDEM_3D, TXDRCT
!          TDEM_3D, WRITE_FD, WRITE_TD, WRITE_LOG_FILE
!
!*** Calls from SA_Input_Routines:
!          READ_SYSTEM_AND_DATA, READ_MODEL_DATA, SET_TRP, SET_FRQ,
!
!===========================================================================

 USE SA_Input_Routines
 IMPLICIT NONE
 INTEGER IDER,KNRM,IPR
 LOGICAL WRT_np
 REAL, DIMENSION(:,:,:), ALLOCATABLE :: BTD
 COMPLEX,DIMENSION(:,:,:), ALLOCATABLE :: BFD
 REAL CMP_begin, CMP_final, CMP_delta

 CALL CPU_TIME (CMP_begin)

 CALL READ_SYSTEM_AND_SURVEY

 !-----------------------------------------------

 IF (INVERT) THEN
   OPEN(NRI,FILE = 'SamayaAir.inv',STATUS = 'OLD')
   OPEN(np,FILE = 'SamayaAir.mv1',STATUS = 'REPLACE')
   OPEN(NS,FILE  = 'SamayaAir.sty',STATUS = 'REPLACE')
 ELSE 
   OPEN(np,FILE = 'SamayaAir.mf1',STATUS = 'REPLACE')
 END IF

 IF (INVERT) CALL READ_INVRT_DATA

 CALL WRITE_np_HEADER

 CALL READ_MODEL_DATA

 IF (INVERT) CALL READ_INVRT_CNTRL

 WRT_np = .TRUE.
 IF (TDFD == 2 .AND. CMP > 1) WRT_np = .FALSE.
 IF (WRT_np) CALL WRITE_np_SYS_DATA

 !-----------------------------------------------

 WRITE(NW,1)

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

 !-----------------------------------------------

 IF (TDFD < 2) THEN   ! Time-Domain.

   ! For time-domain, set up frequencies and interpolation times.
   !
   ! SET_TRP sets array TRP, the times at which the pre-stack responses
   !         will be computed.
   ! SET_FRQ sets the array of frequencies necessary to compute responses
   !         at times TRP
   !
   ! If a current, voltage or B field has been specified (eg, not Tempest)
   ! SET_SOURCE  computes dI/dt at the transmitter using the DC coupling
   ! (PRM_DC) if waveform at the receiver has been specified.  Then the
   ! waveform is used to convert PRM_DC to the peak primary dB/dt in nT if
   ! impulse response is output or B in pT for step response.
   ! SWY will be in amps/s * Tx area * NTRN
   !
   ! IDER = 0 => that current derivative must be computed: ISW = 1, 11 or 31
   !      = 1 => that current derivative has specified through voltage
   !             calibration: ISW = 10 or 30
   !      = 4 => ISW = 4  (pure rectangular pulse)

   IDER = 0

   IF (ISW == 10 .OR. ISW == 30 .OR. ISW == 130) IDER = 1
   IF (ISW == 4) IDER = 4

   CALL SET_TRP

   CALL SET_FRQ

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

 !-----------------------------------------------

 IF (INVERT) THEN

   ! Allocate arrays that are passed but not used.

   IF (TDFD < 2) THEN
     ALLOCATE (PRM_FD(NFRQ)) ; PRM_FD = 0.
   END IF

   ! Call nonlinear least squares subroutine.

   CALL NLSQ (NM,ND,NW,np,NS,MV1PRT,OUTPRT,MAXITS,CNVRG,PCTCNV,NDATA, &
              XDATA,XMODL,XWTS,NPAR,CXPAR,ELAS,LBND,UBND,TDFD,CMP, &
              STEP,IDER,FANGLE,NSX,SWX,SWY,NTYRP,TRP,NPULS,PULSE,NTYPLS, &
              MCHNL,NCHNL,TOPN,TCLS,GSTRP,ASTRP,NFRQ,FREQ,TXCLN,TXA90, &
              NRX,NRXST,NSTAT,SX,SY,SZ,RX,RY,RZ,NN,NE,NZ,NLITH,NPROP, & 
              LITH,LITHL,LYTH,SOLVER,NLYR,THK,ELOC,NLOC,ZLOC,KNRM,NORM, & 
              BFFAC,PPFAC,KPPM,PRM_FD,PRM_TD,SXD,SYD,RXD,RYD,LINE,NTXD, & 
              NPART,XPART,DNORM,INRM,NEL,NER,NNL,NNR,NZT,NZB)

 ELSE

   CLOSE (NR)

   IF (DO3D == 1) THEN

     NTX = NSTAT
     IPR = 1
     CALL SAMAYA_3D (NFRQ,FREQ,SOLVER,NE,NN,NZ,ELOC,NLOC,ZLOC,NTXD,TXCLN, &
                     NSTAT,NTX,SX,SY,SZ,FANGLE,NRX,NCOMP,RX,RY,RZ,ND,NS, &
                     LITH,LYTH,NLITH,NPROP,NLYR,LITHL,THK,JCBN,TDFD,CMP, &
                     NPART,XPART,NEL,NER,NNL,NNR,NZT,NZB,IPR)

   END IF

   ALLOCATE (BFD(NFRQ,NSTAT,3))

   BFD = ZERO

   CALL FDREAD (ND,NFRQ,NSTAT,BFD)     !  Read frequency-domain data

   CLOSE (ND)

   IF (TDFD < 2) THEN   ! Time-Domain. Compute BTD, the TOTAL response.

     ALLOCATE (BTD(NCHNL,NSTAT,3))

     BTD = 0.

     CALL TDEM_3D (STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL, &
                   TOPN,TCLS,FREQ,NFRQ,NSTAT,BFD,GSTRP,ASTRP,BTD)

     DEALLOCATE (BFD)

     ! Write out the results.

     CALL WRITE_TD (NW,np,TITLE,NSTAT,LINE,SXD,SYD,SZ,TXDEG,RXD,RYD,RZ,XRX,YRX,ZRX, &
                    NCHNL,TMS,PRFL,QUNIT,BUNIT,PPFAC,BFFAC,PRM_TD,CMP,KPPM,BTD)

   ELSE ! Construct the frequency-domain response.

     BFD = -BFD

     ! The - sign above is a consequence of using the sine transform for a +iwt
     ! sign convention.  It is thus consistant with the convention used for the
     ! layered half space and in TDEM for time-domain scattered fields.
     ! The check is that the response is consistant between a large flat plate
     ! in a uniform half-space when compared with the equivalent 3-laye space.

     CALL WRITE_FD (NW,np,TITLE,NSTAT,LINE,TXCLN,TXA90,SXD,SYD,SZ,RXD,RYD,RZ,CONFIG, &
                    NFRQ,FREQ,PRFL,QUNIT,PPFAC,BFFAC,PRM_FD,CMP,BFD)

   END IF 

 END IF

 ! Calculate and write the computation time.
 !
 ! complete run time calcs & sign off ...
 call date_and_time(Values = tvals)
 call CPU_time(CMP_final)
 CMP_delta = CMP_final - CMP_begin

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
 CLOSE (np)
 CLOSE (NLG)

 ! WRITE(*,4) ! SamAir has finished!

 STOP

 1 FORMAT(/75('-')/T24,'End of SamAir control file description'/75('-'))
 2 FORMAT(//T3,'The SamAir control file, SamayaAir.cfl, contains fatal errors.' &
           //T3,'These errors are described in the file, SamayaAir.log' &
            /T3,'If all else fails, please refer to SamAir documentation.'  &
           //T3,'Execution will NOT occur until the data in SamayaAir.cfl are corrected!')
 3 FORMAT(//T3,'SamayaAir.log contains comments or adjustment information for this run.' &
            /T3,'It is advisable to read them.')
 4 FORMAT (//T3,'SamAir has finished!'//)
11  Format ('/', / &
            '/ ', a, ' ', a, ' run completed at ', i4.4, '-', i2.2, '-', i2.2, 'T', i2.2, ':', i2.2, ':', i2.2, / &
            '/ Runtime: ', f12.2, ' secs')
12  Format (/, 2x, a, ' ', a, ' run completed at ', i4.4, '-', i2.2, '-', i2.2, 'T', i2.2, ':', i2.2, ':', i2.2, &
		    /, 2x, 'Runtime: ', f12.2, ' seconds', /)
14 FORMAT (I5, 3(2x, F12.3),100(2x, G13.4))
90 Format (/, 2x, 'Completed sanity check on entries in ', a, '.cfl ...', &
           /, 2x, 'Computation begining ...', /)
91 FORMAT(T1,'/'/'/ Airbeo forward model completed at ',I2.2,':',I2.2,' on',I3.2,1X,A,I5 &
          /T1,'/ Computation time = ',F10.2,' seconds.')
92 FORMAT(T1,'/'/'/ Airbeo inversion completed at ',I2.2,':',I2.2,' on',I3.2,1X,A,I5 &
          /T1,'/ Computation time = ',F10.2,' seconds.')
98 FORMAT(//T3,'Airbeo forward model completed at ',I2.2,':',I2.2,' on',I3.2,1X,A,I5 &
           //T3,'Computation time = ',F10.2,' seconds.'//)
99 FORMAT(//T3,'Airbeo inversion completed at ',I2.2,':',I2.2,' on',I3.2,1X,A,I5 &
           //T3,'Computation time = ',F10.2,' seconds.'//)

 END PROGRAM MAIN

!===========================================================================

 SUBROUTINE SET_SURVEY (NLG,NW,NSTAT,FANGLE,SXD,SYD,SX,SY,SZ,BAROMTRC,NRX, & 
                        XRX,YRX,ZRX,RX,RY,RZ,RXD,RYD,ECNTRD,NCNTRD,NSN,NSE, & 
                        NSURF,ESURF,ZSURF,JZAIR,TDFD)

!---------------------------------------------------------------------------
!
!*** Called by MAIN
!*** Calls FIND_TX_RX
!
!  Arrays are nor given in real world coordinates which require double precision.
!  Rather than carry unnecessary higher precision into the computation, arrays
!  are adjusted by ECNTRD and NCNTRD.  Computes and prints array positions in
!  body centred coordinates & barometric altitudes.  If any transmitter of receiver
!  position is outside the mesh or subsurface, SET_SURVEY will cause the program to
!  halt with appropriate messages.
!
!             NW - output unit number for .OUT output files
!            NLG - output unit number for errror messages
!          NSTAT - number of stations
!       SXD, SYD - (north, east) coordinates of transmitter in real world coordinates
!         SX, SY - (north, east) coordinates of transmitter in mesh-centred coordinates
!             SZ - transmitter altitude (always barometric on exit)
!       BAROMTRC = 0 => altitudes are ground clearance in metres.
!                = 1 => altitudes are barometric; ie, metres above sea level.
!            NRX - number of receivers per transmitter position
!            XRX - in-line receiver offset.  XRX is positive behind the transmitter
!            YRX - transverse receiver offset.  YRX is positive starboard
!            ZRX - vertical receiver offset.  ZRX is positive below the transmitter
!     RX, RY, RZ - (north, east) body centred coordinates and barometric elevation of receivers
!       RXD, RYD - (north, east) coordinates receiver in real world system
! NCNTRD, ECNTRD - (north, east) real world coordinates of origin of body centred system
!   NSURF, ESURF - (north, east) body centred coordinates of mesh surface
!       NSN, NSE - number oF mesh nodes north-south and east-west respectively
!          ZSURF - height of mesh surface in RLs (positive above sea level, negative below.
!          JZAIR - parameter for defining air layer mesh,
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 INTEGER NW,NLG,NSTAT,BAROMTRC,NRX,NSN,NSE,JS,JR,JZAIR,TDFD
 REAL SX(NSTAT),SY(NSTAT),SZ(NSTAT),FANGLE(NSTAT),XRX(NRX),YRX(NRX),ZRX(NRX), &
      RX(NSTAT,NRX),RY(NSTAT,NRX),RZ(NSTAT,NRX),NSURF(NSN,NSE),ESURF(NSN,NSE), &
      ZSURF(NSN,NSE),CSF,SNF,QE,QN,ALT,CLEAR,CLTX(NSTAT),CLRX(NSTAT,NRX)
 REAL(KIND=QL) SXD(NSTAT),SYD(NSTAT),RXD(NSTAT,NRX),RYD(NSTAT,NRX),ECNTRD,NCNTRD
 LOGICAL KILL,CRASH,MATCH,BAR

 KILL = .FALSE.

 ! Compute receiver coordinates in both body centred and real world systems.

 DO JS = 1,NSTAT

   CSF = COS(FANGLE(JS))
   SNF = SIN(FANGLE(JS))

   IF (TDFD < 2) THEN
     SX(JS) = REAL((SXD(JS) - NCNTRD),4)      ! Body centred coordinates
     SY(JS) = REAL((SYD(JS) - ECNTRD),4)      ! Body centred coordinates
     RX(JS,1) = SX(JS) - XRX(1) * CSF + YRX(1) * SNF
     RY(JS,1) = SY(JS) - YRX(1) * CSF - XRX(1) * SNF 
     RZ(JS,1) = SZ(JS) - ZRX(1)
   ELSE
     SX(JS) = REAL (SXD(JS) - NCNTRD) + 0.5 * (XRX(1) * CSF - YRX(1) * SNF)
     SY(JS) = REAL (SYD(JS) - ECNTRD) + 0.5 * (YRX(1) * CSF + XRX(1) * SNF)
     DO JR = 1, NRX
       RX(JS,JR) = SX(JS) - XRX(JR) * CSF + YRX(JR) * SNF
       RY(JS,JR) = SY(JS) - YRX(JR) * CSF - XRX(JR) * SNF
       RZ(JS,JR) = SZ(JS) - ZRX(JR)
       RXD(JS,JR) = RX(JS,JR) + SXD(JS)
     END DO
   END IF

   QN = SX(JS)
   QE = SY(JS)
   ALT = SZ(JS)
   BAR = .TRUE.
   IF (BAROMTRC == 0) BAR = .FALSE.
   CALL FIND_TX_RX (NSN,NSE,NSURF,ESURF,ZSURF,QN,QE,ALT,BAR,MATCH,CLEAR,CRASH)
   SZ(JS) = ALT  ! Converted to barometric altitude when BAROMTRC == 0
   CLTX(JS) = -999.
   IF (MATCH) CLTX(JS) = CLEAR
   BAR = .TRUE.

   IF (CRASH .OR. .NOT. MATCH) THEN
     KILL = .TRUE.
     IF (.NOT. MATCH) WRITE (NLG,1) JS
     IF (CRASH) WRITE (NLG,2) JS,SY(JS),SX(JS)
   END IF

   DO JR = 1,NRX
     QN =  RX(JS,JR)
     QE =  RY(JS,JR)
     ALT = RZ(JS,JR)
     CALL FIND_TX_RX (NSN,NSE,NSURF,ESURF,ZSURF,QN,QE,ALT,BAR,MATCH,CLEAR,CRASH)
     CLRX(JS,JR) = -999.
     IF (MATCH) CLRX(JS,JR) = CLEAR

     IF (CRASH .OR. .NOT. MATCH) THEN
       KILL = .TRUE.
       IF (.NOT. MATCH) WRITE (NLG,3) JR,JS,RY(JS,JR),RX(JS,JR)
       IF (CRASH) WRITE (NLG,4) JR,JS
     END IF

   END DO
 END DO

 RXD = REAL (RX,8) + NCNTRD
 RYD = REAL (RY,8) + ECNTRD
 SXD = REAL (SX,KIND=QL) + NCNTRD !  SXD & SYD were midpoints in Freq domain
 SYD = REAL (SY,KIND=QL) + ECNTRD  !  Now they are Tx positions based on XRX(1), YRX(1)

 WRITE (NW,8) ECNTRD,NCNTRD

 DO JS = 1,NSTAT
   WRITE(NW,9) JS,SY(JS),SX(JS),SZ(JS),RY(JS,1),RX(JS,1),RZ(JS,1),CLTX(JS),CLRX(JS,1)
 END DO

 IF (KILL) THEN
   WRITE (*,5)
   WRITE (NW,5)
   STOP
 END IF

 JZAIR = 0
 DO JS = 1, NSTAT
    DO JR = 1,NRX
       IF((ABS(CLTX(JS)) < 1.) .OR. (ABS(CLRX(JS,JR)) < 1.)) JZAIR = 1
    END DO
 END DO

 1 FORMAT(/T3,'Transmitter position',I3,' at East, North BODY CENTRED coordinates:'  &
          /T3,'(',F12.2,',',F12.2,')  is outside the mesh boundary')
 2 FORMAT(/T3,'Transmitter position',I3,' is below ground level')
 3 FORMAT(/T3,'Receiver',I3,' (transmitter position',I3,') at East, North BODY CENTRED coordinates:', &
          /T3,'(',F12.2,',',F12.2,')  is outside the mesh boundary')
 4 FORMAT(/T3,'Receiver',I3,' of transmitter position',I3,' is below ground level')
 5 FORMAT(/T3,'This program requires all transmitter & receiver positions to be above surface' &
          /T3,'and horizontally within the mesh area.  There are errors in the flight path'    &
          /T3,'specification with some positions outside the constraints'                      &
         //T3,'SEE LOG FILE FOR FURTHER DETAILS.    EXECUTION HALTED.')
 8 FORMAT(//T3,'Before computation begins, array and model coordinates are transformed from'   &
           /T3,'"real world" coordimnates to a body- centred system where the new origin is'   &
           /T3,'over the centre of the model region where depth is positive downwards.'        &
           /T3,'In the original coordinate system the new computation origin is located at:'   &
           /T5,'EAST: ',F12.2,';    NORTH: ',F12.2,                                            &
          //T3,'Altitudes expressed as ground clearance are converted to barometric altitude.' &
           /T4,'The revised transmitter and receiver positions are:'                           &
          //T20,'TRANSMITTER',T48,'RECEIVER 1'/T69,'Tx Ground   Rx 1 Ground'                  &
           /T4,'Station    East    North   Altitude    East    North   Altitude  Clearance   Clearance' &
           /T4,'-------    ----    -----   --------    ----    -----   --------  ---------   --------- ')
 9 FORMAT(I7,3X,3F9.0,1X,3F9.0,F10.0,F11.0)

 END SUBROUTINE SET_SURVEY

!===========================================================================

 SUBROUTINE FIND_TX_RX (NSN,NSE,NSURF,ESURF,ZSURF,QN,QE,ALT,BAR,MATCH, & 
                        CLEAR,CRASH)

!---------------------------------------------------------------------------
!
!***  Called by SET_SURVEY
!
!  NSURF, ESURF & ZSURF are the north, east & relative levels (metres above sea
!  level) of the surface layer of nodes.  ZSURF is POSITIVE above sea level and
!  NEGATIVE below.
!
!  On entry, QN, QE, ALT are the coordinates of either a transmitter or receiver
!  position.  BAR is false if ALT is expressed in terms of ground clearance.  In
!  this case, FIND_TX_RX computes the surface height at QN, QE.  ALT is returned
!  as barometric altitude.  CRASH is returned as false unless the initial value
!  of ALT was negative.
!
!  BAR is true if ALT is entered as barometric altitude.  In this case, FIND_TX_RX
!  computes the surface height at QN, QE and then the ground clearance.
!  If the ground clearance is negative CRASH is returned as TRUE, implying
!  that the transmitter or receiver is subsurface.
!
!  FIND_TX_RX checks to see if the transmitter or receiver is over the mesh
!  (MATCH = TRUE) or outside it (MATCH = FALSE)
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 REAL, PARAMETER :: TOL=0.001
 Real, Parameter :: SURVEY_RANGE = 3000
 INTEGER NSN,NSE,KN,KE
 REAL QN,QE,ALT,NSURF(NSN,NSE),ESURF(NSN,NSE),ZSURF(NSN,NSE),CLEAR,W1,W2,W3,W4, &
      XMIN,YMIN,XMAX,YMAX,ZAVG
 LOGICAL MATCH,CRASH,BAR

 MATCH = .FALSE.
 CRASH = .FALSE.
 IF (.NOT. BAR .AND. ALT < 0) THEN
   CRASH = .TRUE.
   RETURN      !*=*=*=*=*=*=*=*=*=*=*=*=*=*
 END IF

 SURFACE: DO KN = 1,NSN-1
   DO KE = 1,NSE-1
     XMIN = QN - MIN (NSURF(KN,KE), NSURF(KN,KE+1)) + TOL + SURVEY_RANGE
     YMIN = QE - MIN (ESURF(KN,KE), ESURF(KN+1,KE)) + TOL + SURVEY_RANGE
     XMAX = MAX (NSURF(KN+1,KE), NSURF(KN+1,KE+1)) - QN + TOL + SURVEY_RANGE
     YMAX = MAX (ESURF(KN,KE+1), ESURF(KN+1,KE+1)) - QE + TOL + SURVEY_RANGE
     IF (XMIN > 0 .AND. XMAX > 0 .AND. YMIN > 0 .AND. YMAX > 0) THEN
       MATCH = .TRUE.                                                ! QN & QE are over the mesh

       ! Compute ZAVG, the surface height under QN,QE using linear
       ! horizontal distance weights.

       W1 = SQRT ( (QN - NSURF(KN,KE))**2     + (QE - ESURF(KN,KE))**2)
       W1 = 1./ MAX (W1,TOL)
       W2 = SQRT ( (QN - NSURF(KN+1,KE))**2   + (QE - ESURF(KN+1,KE))**2)
       W2 = 1./ MAX (W2,TOL)
       W3 = SQRT ( (QN - NSURF(KN,KE+1))**2   + (QE - ESURF(KN,KE+1))**2)
       W3 = 1./ MAX (W3,TOL)
       W4 = SQRT ( (QN - NSURF(KN+1,KE+1))**2 + (QE - ESURF(KN+1,KE+1))**2)
       W4 = 1./ MAX (W4,TOL)

       ZAVG = (W1* ZSURF(KN,KE) + W2* ZSURF(KN+1,KE) + W3* ZSURF(KN,KE+1) &
            + W4* ZSURF(KN+1,KE+1)) / (W1 + W2 + W3 + W4) - ZSURF(KN,KE) 

       ! Then compute barymetric altitude as the SUM of ALT1 (ground clearance) and ZAVG
       ! since is negative above sea level

       IF (.NOT. BAR) ALT = ALT + ZAVG  ! Convert from ground clearance to barometric altitude.
       CLEAR = ALT - ZAVG
       IF (CLEAR < 0) CRASH = .TRUE.

       EXIT SURFACE

     END IF
   END DO
 END DO SURFACE

 END SUBROUTINE FIND_TX_RX

!============================================================================

 SUBROUTINE CONFIG_ID (NFRQ,TXCLN,TXA90,XRX,YRX,ZRX,CONFIG,CFG1)

!----------------------------------------------------------------------------
!
!***  Called by READ_SYSTEM_DATA
!
!  Returns CONFIG = HCP, VCA, VCP, VCB, HCA or '   ' 
!            CFG1 =  1    2    3    4    5       0
!
!        NFRQ - number of frequencies
!       TXCLN - transmitter inclination in degrees
!       TXA90 - true for vertical co-planar briadside array
!         ZRX - vertical receiver offset for each frequency; below = positive
!         XRX - in-line receiver offset for each frequency;  behind = positive
!         YRX - transverse receiver offset for each frequency; left = positive.
!
!----------------------------------------------------------------------------

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

 END SUBROUTINE CONFIG_ID

!===========================================================================

 SUBROUTINE FDREAD (ND,NFRQ,NSTAT,BFD)

!---------------------------------------------------------------------------
!
!  Reads frequency-domain scattered impulse magnetic field (H) data
!  (real & imaginary components) from logical UNIT ND into
!  B in nT for array BFD for conversion to time-domain by  TDEM_OUT.
!
!*** Called by MAIN, RESJAC
!
!             NFRQ - number of frequencies
!              NRX - number of receivers per frequency
!            NSTAT - number of transmitter positions
! BFD(I,J,K) - Kth component of the complex frequency-domain impulse response
!                 magnetic field (H) at transmitter J, for frequency I.
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER ND,NFRQ,NSTAT,JF,JS,JC
 REAL A(6)
 COMPLEX BFD(NFRQ,NSTAT,3)

 ! FAC = 400. * 3.141592654   ! 1.e9 * mu0.

 REWIND(ND)

 DO JF = 1, NFRQ
   DO JS = 1,NSTAT
     READ(ND,*) A(1:6)
     DO JC = 1,3
       BFD(JF,JS,JC) = CMPLX (A(2*JC-1), A(2*JC))
     END DO
   END DO
 END DO

 END SUBROUTINE FDREAD

!===========================================================================

 SUBROUTINE DCPRM_FD (NFRQ,XRX,YRX,ZRX,TXCLN,TXA90,PRM_FD,PPFAC,NORM)

!---------------------------------------------------------------------------
!
!***  Called by: MAIN
!
!  In frequency-domain, it computes the maximally coupled component of B at each
!  receiver location for each frequency assuming unit dipoles and current
!  transmitters are co-oriented.
!
!  SIGN CONVENTION:
!  ----------------
!  The normal layered earth field coordinate system used in this
!  subroutine has X (JC=1) positive along the flight path, Y (JC=2)
!  positive to starboard, and Z (JC=3) positive down.
!
!                               INPUT
!                               -----
!       NFRQ - number of frequencies
!        ZRX - vertical offset of RX relative to transmitter (below = + ).
!        XRX - in-line offset of RX relative to transmitter (behind = + ).
!        YRX - transverse offset of RX relative to transmitter (starboard = + ).
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
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER NFRQ,JF
 REAL SNTX,CSTX,XBD,YBD,ZBD,RBRD,RSQ,RSQ1,BFAC,FAC,FACZX,INLINE,VERT,PPFAC
 REAL, DIMENSION(NFRQ) :: TXCLN,XRX,YRX,ZRX,PRM_FD,NORM
 LOGICAL COPLANAR,TXA90

 ! BFAC = 1.0E9 * MU / (4 * PI)  ! NANOTESLAS

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

!===========================================================================

 SUBROUTINE DCPRM_TD (XRX0,YRX0,ZRX0,TXCLN0,TXAREA,PRM_TD)

!---------------------------------------------------------------------------
!
!***  Called by: READ_INVERSION_CONTROL
!
! For time-domain, PRM_TD is the 3 component Tx-Rx dc coupling factor
! per unit dipole moment, expressed in NANOTESLAS per unit amp
! Multiply it by dI/dt and get nanovolts per m^2 which is the
! same as nT/s.  Multiply it by current and get nT.
!
!  SIGN CONVENTION:
!  ----------------
!  The normal layered earth field coordinate system used in this
!  subroutine has X (JC=1) positive along the flight path, Y (JC=2)
!  positive to starboard, and Z (JC=3) positive down.
!
!                               INPUT
!                               -----
!      TXCLN0 - angle in degrees that the transmitting dipole makes with vertical
!              (climb = positive for VMD transmitter)
!      TXAREA - transmitter area in sq. metres
!      ZRX0, XRX0 & YRX0 are the initial vertical, in-line and transverse offsets
!                        of the receiver relative to transmitter
!                        below = + ;  behind = + ;  starboard = +
!
!                                 OUTPUT (time domain)
!                                 --------------------
!     PRM_TD = primary field coupling factor for B in nT (unit TX moment)
!     PRM_TD(1) = in-line B
!     PRM_TD(2) = transverse B
!     PRM_TD(3) = vertical B for station
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 REAL, PARAMETER :: PI=3.141592654
 REAL SNTX,CSTX,XBD,YBD,ZBD,RBRD,RSQ,RSQ1,BFAC,FAC,FACZX,TXAREA, &
      TXCLN0,THETA,XRX0,YRX0,ZRX0,INLINE,VERT,TRANS,PRM_TD(3)

 ! BFAC = 1.0E9 * MU / (4 * PI) for time domain  ! NANOTESLAS

 PRM_TD = 0.

 !-----------------------------------------------

 ! In-loop time-domain HEM

 IF (TXAREA > 1.) THEN
   RBRD = ABS (XRX0) + ABS (YRX0)
   IF (RBRD < 1.) THEN
     ZBD = SQRT (ZRX0**2 + TXAREA / PI)
     PRM_TD(3) = 200. / ZBD**3       ! 1.0E9 * MU / (2 * PI) = 200. (nT)
     RETURN
   END IF
 END IF

 !-----------------------------------------------

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

!===========================================================================

 SUBROUTINE SET_SOURCE (STEP,ISW,BFFAC,WAVEFORM,NSX,SWX,SWY,PRM_TD)

!---------------------------------------------------------------------------
!
! For time-domain, SET_SOURCE computes dI/dt at the transmitter using
! the DC coupling if waveform at the receiver has been specified.  Then
! (for time-domain) it converts PRM_TD to the peak primary dB/dt in nT/s
! if impulse response is required or B in nT for step response.
!
! SWY will be in amps / sec * Tx area * NTRN
!  Computes SWY to be TXMNT * dI(t)/dt & PKSX, the peak response.
!  The units of SWY are amps * m^2 / s
!
!*** Called by: MAIN
!
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
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER ISW,STEP,NSX,JT
 REAL BFFAC,SWX(NSX),WAVEFORM(NSX),SWY(NSX,3),PKSX,DELT,COUPLING,PRM_TD(3)

 IF (SWX(2) - SWX(1) < 0.5E-7) SWX(2) = SWX(1) + 0.5E-7
 IF (SWX(NSX) - SWX(NSX-1) < 0.5E-7) SWX(NSX-1) = SWX(NSX) - 0.5E-7

 ! Remove the receiver coupling if the receiver voltage or magnetic field is
 ! to be used to get dI/dt.  Ensure that the input waveform is converted to
 ! nT or nT/s.

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

 ! Compute the source waveform for the program from the input data.
 ! This is dI/dt * the Tx-Rx moment.

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

!===========================================================================

 SUBROUTINE TDEM_3D (STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL, &
                     TOPN,TCLS,FREQ,NFRQ,NSTAT,BFD,GSTRP,ASTRP,BTD)

!---------------------------------------------------------------------------
!
!***  Called by: MAIN, GET_FWD_MODL
!***      Calls: CUBSPL, COSTRN, FOLD_CONVOLVE
!
!  Computes BTD, the time domain response for the 3D part of the model
!  (scattered field) as db/dt in (nT/s if STEP is false) or magnetic field B
!  (in nanoteslas if STEP is true) by convolving the step b response of the
!  earth with the negative time-derivative of the current waveform.  Questo
!  waveform contains the Txarea * NTRN.  For magnetic field, this averaged
!  across the receiver window.  For db/dt, this is differenced across the
!  receiver window.  The negative dI/dt is used so that current switch off
!  corresponds to positive response.
!
!  On entry, the scattered field frequency-domain step H data in array BFD
!  is rotated to the aircraft system and the imaginary component is converted
!  to time-domain step b(t) data out to NPULS bipolar cycles.  For each
!  component in-line, transverse,and vertical, and for each transmitter-
!  receiver position, FOLD_AND_CONVOLVE is called to fold the positive &
!  negative parts of the bipolar current pulse into a half-cycle (length PULSE)
!  decay curve.  This result is convolved with the dI/dt waveform.
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
!  BFD(I,J,K) - Kth component of the complex frequency-domain impulse
!                    response magnetic field (H) at receiver L, transmitter J
!                    for frequency I - due to a unit dipole transmitter,
!                    oriented at angle TXCLN.  (nT)
!                    K = 1,2,3 => in-line, transverse & vertical  components respectively.
!
!                      OUTPUT
!                      ------
!
!  BTD(JT,JS,1) - the in-line component of the layered earth response at time JT, station JS.
!  BTD(JT,JS,2) - the horizontal transverse component
!  BTD(JT,JS,3) - the vertical component
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 REAL, PARAMETER :: TWOPI=6.283185
 INTEGER GSTRP,ASTRP,IDER,STEP,NSX,NPULS,NTYPLS,NTYRP,NCHNL,NFRQ,NSTAT, &
         JS,JF,JC,JT
 REAL, DIMENSION(:,:), ALLOCATABLE ::  YSCAT,YFRQ,BFD_QUAD
 REAL PULSE,FREQ(NFRQ),T0_MIN,WF(NFRQ),SWX(NSX),SWY(NSX,3),COSTRN,T,YCUM(NCHNL), &
      TOPN(NCHNL),TCLS(NCHNL),TRP(NTYRP),BTD(NCHNL,NSTAT,3),OMEGA(NFRQ)
 COMPLEX BFD(NFRQ,NSTAT,3)

 T0_MIN = 0.1 / MAXVAL (FREQ)

 BTD = 0.

 ALLOCATE (YSCAT(4,NTYRP), YFRQ(4,NFRQ), BFD_QUAD(NFRQ,3) )
 YSCAT=0; YFRQ=0; BFD_QUAD=0;

 ! For all frequencies and receiver positions, rotate the fields from
 ! X, Y, Z orientation to in-line, transverse, and vertical components.
 ! For dead North flight line, in-line is positive North and transverse
 ! is positive East.  Spline the result as a function of log (w) (omega).
 ! Convert to step response and apply the microvolt conversion factor.

 ! step dB/dt (nT/s) = H * 1.0E9 * MU * dI/dt * TXMMNT / (I * 2*PI * FREQ)
 ! Originally VFAC was = 1.0E9 * MU * TX MOMENT / (2*PI);
 ! From 9/01, input is B in nT and 1/(2 Pi) is now absorbed in OMEGA
 ! If STEP = 1, output is in nanoteslas consistent with HSBOSS.
 ! In this case, conversion to pT or fT occurs in WRITE_TD.

 OMEGA(1:NFRQ) = TWOPI * FREQ(1:NFRQ)
 WF = LOG (OMEGA)
 OMEGA = -OMEGA     !  division by -iw for step response

 DO JS = 1,NSTAT         ! station loop
   DO JF = 1, NFRQ
     BFD_QUAD(JF,1:3) = AIMAG ( BFD(JF,JS,1:3) )  / OMEGA(JF)
   END DO

   ! Above: Conversion from impulse to step current turn-off.
   ! For each component at each receiver station, compute the SCATTERED response
   ! by splining the imaginary part of the frequency-domain response, converting
   ! it to time-domain step function response and folding the NPULS bipolar decay
   ! curve into a combined pulse decay curve of length PULSE.  Convolve this with
   ! the TX waveform to produce BTD, the 'observable" stripped response for the
   ! system.

   DO JC = 1,3          ! Component loop.
     YFRQ(1,1:NFRQ) = BFD_QUAD(1:NFRQ,JC)
     CALL CUBSPL (WF,YFRQ,NFRQ,0,0)

     YSCAT = 0.
     DO JT = 1, NTYRP   ! Convert to step-function time-domain.
       T = TRP(JT)
       IF (T < T0_MIN) CYCLE
       YSCAT(1,JT) = COSTRN (WF,YFRQ,NFRQ,NFRQ,T)
     END DO
     CALL FOLD_AND_CONVOLVE (STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,TRP,NTYPLS, &
                             NTYRP,NCHNL,TOPN,TCLS,YSCAT,GSTRP,ASTRP,YCUM)

     BTD(1:NCHNL,JS,JC) = YCUM(1:NCHNL)
   END DO
 END DO

 DEALLOCATE (BFD_QUAD, YSCAT, YFRQ)

 END SUBROUTINE TDEM_3D

!============================================================================

 REAL FUNCTION COSTRN (WF,YFRQ,NFRQ,KFRQ,T)

!----------------------------------------------------------------------------
!
!***  Calls CUBVAL
!***  Called by HSBOSS_TD
!
! LAST MODIFICATION DATE: October, 2001
!
! Produces time-domain value at time T by cosine transformation of NFRQ
! frequency-domain values contained in cubic spline array YFRQ.
! KFRQ is the high frequency cutoff, less than or equal to NFRQ.
! Array WF contains the LOG (base e) of the angular frequency values.
!
! The routine uses filter coefficients derived from the Niels Christensen
! fast Hankel transform routine FILCOA at a spacing of 12 points per decade
! and omega = 0.3.  Various filters were tested using a vertical magnetic
! dipole receiver in a very large circular for which accurate frequency
! and time-domain solutions were programmed.  This particular filter gave
! the overall best accuracy for 1/2 spaces ranging in resistivity from
! .1 to 10,000 ohm-m for times ranging from .01 to 50 msec.
!
!  K(W,T) = (2/PI) * F(W) * COS(WT) dW
!
! Letting X = WT, the above becomes
!
!  K(W,T) = (2/PI*T) * F(X/T) * COS(X) dX
!
! From Abramowitz and Stegun, COS(X) = SQRT(X*PI/2) * J(-1/2:X).
! Filter Coefficients are used to represent X**(1/2) * J(-1/2:X)
!
!  COSTRN = SQRT (2/PI) * SUM(i) { WCOS(i) * F [X(i) /T] }
!
! The accumulation is done using 12 digit precision
!
!----------------------------------------------------------------------------

 USE SA_Filter_Coefficients_q
 IMPLICIT NONE
 INTEGER, PARAMETER :: NDEC_COS=12, KFLOW=-200, KFHIGH=99
 REAL, PARAMETER :: FAC=.7978846, TOL=1.0E-6
 INTEGER J1,NFRQ,KFRQ
 REAL WF(NFRQ),YFRQ(4,NFRQ),T,YS,CUBVAL,V1
 REAL(KIND=QL) DELTA,Y1,Y,TD,YTYM,VAL

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

!===========================================================================

 SUBROUTINE FOLD_AND_CONVOLVE (STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,TRP,NTYPLS, &
                               NTYRP,NCHNL,TOPN,TCLS,YPLS,GSTRP,ASTRP,YCUM)

!---------------------------------------------------------------------------
!
!  Computes the "observed" response YCUM by convolving the splined earth
!  response function, YPLS, with the TX waveform.
!
!***  Called by HSBOSS_TD, TDEM3D
!***  Calls: CUBVAL, CUBSPL, TXCNVD, TXCNVL, TQSTRIP
!
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
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER JT,NTYRP,NTYPLS,NPULS,IDER,STEP,NSX,NCHNL,JGL,JP,GSTRP,ASTRP,MXCNV,IPL
 REAL PULSE,TRP(NTYRP),SWX(NSX),SWY(NSX,3),TOPN(NCHNL),TCLS(NCHNL),T1,T2,WIDTH, &
      TF,TFH,HWIDTH,YC1,TC(3),GLX(3),GLW(3),YPLS(4,NTYRP),X,XP,YCUM(NCHNL),     &
      CUBVAL,CUBDER,TXCNVL,TXCNVD,WT,FOLD(NTYPLS),YCNV(4,NSX)
 DATA GLW(1:3) /.5555556, .8888889, .5555556/, GLX(1:3) /-.7745967, 0., .7745967/

 ! Accumulate the results of NPULS bipolar cycles by splining the instantaneous
 ! response and folding the positive and negative parts of each cycle back
 ! into a single pulse.

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

 ! Begin convolution.  If Geotem / Questem primary field stripping is required
 ! the convolution must be done for all points in the waveform.
 ! Otherwise, convolve only for those points needed in the windows.

 ! The layered earth field is in IMPULSE form if dB/dt is desired
 ! or in STEP form if B is to be computed.

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

!===========================================================================

 SUBROUTINE TQSTRIP (IDER,NTYPLS,TRP,YPLS,NSX,SWX,SWY,YCNV)

!---------------------------------------------------------------------------
!
!***  Called by FOLD_AND_CONVOLVE
!***  Calls CUBINT, CUBSPL, CUBVAL
!
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
!
!  where T0 = MAX { TRP(1), T - SWX (NSX)}
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 REAL, PARAMETER :: T0_MIN=1.E-7
 INTEGER IDER,NTYPLS,NSX,JT,MXCNV
 REAL T,TRP(NTYPLS),YPLS(4,NTYPLS),SWX(NSX),SWY(NSX,3),YCNV(4,NSX),TXCNVL, &
      TXCNVD,A1,B1,ALPHA

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

!===========================================================================

 REAL FUNCTION TXCNVD (MXCNV,T,NTYPLS,TRP,YPLS,NSX,SWX,SWY)

!---------------------------------------------------------------------------
!
!***  Called by FOLD_AND_CONVOLVE
!***  Calls CUBINT, CUBSPL, CUBVAL, LINVAL, TXCMRG
!
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
!
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
!
!  Alternatively, we can difference the step B field from 0 to TRP(1) which
!  is a lot easier since the step B field at T = 0 is simply the DC field due
!  to a transmitter image buried at z = ALT; i.e., the z+z' term.  In this case,
!  the bigger TRP(1) is, the more accurate the difference in B but this must be
!  sufficiently small so that the change in dI/dt is negligable.  Thus, TRP(1)
!  is chosen to be .1 microsecond.
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER MXCNV,NTYPLS,NSX,N1,J1,N2,J2,NCNV
 REAL T,TC,T0,TRP(NTYPLS),YPLS(4,NTYPLS),SWX(NSX),SWY(NSX,3),YCNV(4,MXCNV), &
      XCNV(MXCNV),X1(MXCNV),Y1(MXCNV),X2(MXCNV),Y2(MXCNV),CUBVAL,CUBINT,LINVAL

 ! Set up X1,Y1, the N1 values of SWX, SWY * YPLS for signal ontime < T.
 ! where X1, the conjugate signal time, contains T-SWX values.
 ! Set up X2,Y2, the N2 values of TRP, YPLS * SWY for ERF points  <= T.

 ! Return TXCNVD = 0 if N1 + N2 < 4 or if NCNV < 4

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

 ! Merge the two lists into XCNV, YCNV of length NCNV.
 ! Then spline and integrate

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

!===========================================================================

 REAL FUNCTION TXCNVL (T,NTYPLS,TRP,YPLS,NSX,SWX,SWY)

!---------------------------------------------------------------------------
!
!***  Called by FOLD_AND_CONVOLVE
!***  Calls CUBINT, CUBVAL
!
!  Computes the system dB/dt response by convolving the computed dI/dt with
!  the impulse B response of the earth.  For step current drops, system dB/dt
!  is computed asthe product of instantaneous current drop times the
!  earth step dB/dt.
!
!  This routine assumes that the source waveform is composed of NSX linear
!  segments.  Thus NSX-1 constant dI/dt values are contained in SWY(*,1).
!
!  The input earth response function (step dB/dt or equivalently, impulse B)
!  must be contained in a splined array of NTYPLS values of time (abscissa) TRP
!  and ordinate YPLS.  System dB/dt is computed by integrating YPLS between
!  the SWX points of constant dI/dt segments.
!
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
!
!---------------------------------------------------------------------------

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

   ! For an instantaneous step drop in current, SEG is YPLS times SWY(*,2),
   ! since YPLS is already the dB/dt step response. Otherwise SEG is the
   ! integral of YPLS * constant dI/dt SWY(*,1) since YPLS is also impulse B.

   IF (DER) THEN
     SEG = SWY(JT,1) * CUBINT (TRP,YPLS,NTYPLS,TB,TEND)
   ELSE
     SEG = SWY(JT,2) * CUBVAL (TRP,YPLS,NTYPLS,TB)
   END IF
   CNV = CNV + SEG
 END DO
 TXCNVL = CNV

 END FUNCTION TXCNVL

!===========================================================================

 SUBROUTINE TXCMRG (MXCNV,X1,Y1,N1,X2,Y2,N2,XCNV,YCNV,NCNV)

!---------------------------------------------------------------------------
!
!  Merges two previously sorted list pairs X1, Y1 of length N1 and X2, Y2 of
!  length N2 into list pair XCNV, YCNV of length NCNV into ascending values of
!  XCNV.
!
!***  Called by TXCNVD
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 REAL, PARAMETER :: TOL=1.E-3
 INTEGER MXCNV,N1,N2,NCNV,K1,K2,N,J1
 REAL DELT,TL1,XCNV(MXCNV),X1(MXCNV),Y1(MXCNV),X2(MXCNV),Y2(MXCNV),YCNV(4,MXCNV)
 LOGICAL LIST1, LIST2

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

 NCNV = 1      !  Clean up list.
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

!===========================================================================

  REAL FUNCTION CUBDER (XKNOT, COEF, KNOT, X1)

!---------------------------------------------------------------------------
!
!  Evaluates the first derivative of a function from its cubic spline
!  interpolation.
!
!***  Called by TDSET
!***  Calls INTERV.  On exit from INTERV
!
!       MFLAG = -1  => X is to the left of interpolated range
!             =  1  => X is to the right of interpolated range
!             =  0  => X is in the interpolated range
!
!         KNOT - total number of knots including endpoints.
!     XKNOT(I), I = 1,KNOT - location of the knots.  The rightmost data
!                            point used to calculate coefficients is not
!                            used.
!     COEF(J,I), J = 1,4; I = 1,KNOT = Jth derivative at H = 0
!                                      where  H = X - XKNOT(I)
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER I,MFLAG,KNOT
 REAL XKNOT(KNOT), COEF(4,KNOT), X1, H

 ! Find index i of largest breakpoint to the left of X1.

 CALL INTERV ( XKNOT, KNOT-1, X1, I, MFLAG )
 H = X1 - XKNOT(I)
 IF (MFLAG == -1) H = 0.

 CUBDER = (COEF(4,I)*H/2. + COEF(3,I) ) *H + COEF(2,I)

 END FUNCTION CUBDER

!===========================================================================

 REAL FUNCTION CUBINT (XKNOT, COEF, KNOT, X1, X2)

!---------------------------------------------------------------------------
!
!  Integrates a function from X1 to X2 using its cubic spline representation.
!
!***  Called by  TXCNVD, TXCNVL
!***  Calls INTERV.  On exit from INTERV
!
!       MFLAG = -1  => X is to the left of interpolated range
!             =  1  => X is to the right of interpolated range
!             =  0  => X is in the interpolated range
!
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
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER I,I1,I2,MFLAG,KNOT
 REAL H,H1,H2,X1,X2,XKNOT(KNOT), COEF(4,KNOT)

 ! Find the indices I1 and I2 of largest breakpoints to the left of X1
 ! and X2 respectively.

 CALL INTERV ( XKNOT, KNOT-1, X1, I1, MFLAG )
 CALL INTERV ( XKNOT, KNOT-1, X2, I2, MFLAG )
 H1 = X1 - XKNOT(I1)
 IF (MFLAG == -1) H1 = 0.

 H2 = X2 - XKNOT(I2)
 CUBINT = (((COEF(4,I2)*H2/4.0 + COEF(3,I2) )*H2/3.0 + &
             COEF(2,I2) )*H2/2.0 + COEF(1,I2) )*H2 &
        - (((COEF(4,I1)*H1/4.0 + COEF(3,I1) )*H1/3.0 + &
             COEF(2,I1) )*H1/2.0 + COEF(1,I1) )*H1

 ! Include integrals over intervening intervals.

 IF (I2 > I1) THEN
   DO I = I1, I2-1
     H = XKNOT(I+1) - XKNOT(I)
     CUBINT = CUBINT + (((COEF(4,I)*H/4.0 + COEF(3,I) )*H/3.0 + &
                          COEF(2,I) )*H/2.0 + COEF(1,I) )*H
   END DO
 END IF

 END FUNCTION CUBINT

!===========================================================================

 SUBROUTINE CUBSPL (XNOT, C, N, IBCBEG, IBCEND)

!---------------------------------------------------------------------------
!
!***  Called by FOLD_AND_CONVOLVE, READ_SYSTEM_DATA, TXCNVD
!
!  Calculates coefficients for cubic spline interpolation.
!  Call function CUBVAL to evaluate function values after interpolation.
!  From  * A PRACTICAL GUIDE TO SPLINES *  by Carl de Boor.
!
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
!
!------------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER IBCBEG,IBCEND,N,I,J,L,M
 REAL C(4,N),XNOT(N),DIVDF1,DIVDF3,DXNOT,G

 SAVE

 ! A tridiagonal linear system for the unknown slopes S(I) of F at
 ! XNOT(I), I=1,...,N, is generated and then solved by Gauss elimination,
 ! with S(I) ending up in C(2,I), ALL I.
 ! C(3,.) AND C(4,.) are used initially for temporary storage.

 ! Compute first differences of XNOT sequence and store in C(3,.).
 ! Also, compute first divided difference of data and store in C(4,.).

 L = N - 1
 DO M = 2,N
   C(3,M) = XNOT(M) - XNOT(M-1)
   C(4,M) = (C(1,M) - C(1,M-1)) /C(3,M)
 END DO

 ! Construct first equation from the boundary condition, of the form
 ! C(4,1)*S(1) + C(3,1)*S(2) = C(2,1)

 IF (IBCBEG < 1) THEN
   IF (N > 2) THEN

     ! Not-a-knot condition at left end and N > 2.

     C(4,1) = C(3,3)
     C(3,1) = C(3,2) + C(3,3)
     C(2,1) = ((C(3,2) + 2.* C(3,1)) * C(4,2)*C(3,3) &
             + C(3,2)**2 * C(4,3)) /C(3,1)
     GOTO 100
   ELSE

     ! No condition at left end and N = 2.

     C(4,1) = 1.
     C(3,1) = 1.
     C(2,1) = 2. * C(4,2)
     GOTO 300
   END IF
 ELSE IF (IBCBEG == 1) THEN

   ! Slope prescribed at left end.

   C(4,1) = 1.
   C(3,1) = 0.

 ELSE

   ! Second derivative prescribed at left end.

   C(4,1) = 2.
   C(3,1) = 1.
   C(2,1) = 3.* C(4,2) - C(3,2) * C(2,1) /2.
 END IF

 IF (N == 2) GOTO 300

 ! If there are interior knots, generate the corresponding equations and
 ! perform the forward pass of Gauss elimination, after which the M-TH
 ! equation reads    C(4,M)*S(M) + C(3,M)*S(M+1) = C(2,M).

 100 DO M = 2,L
       G = -C(3,M+1) / C(4,M-1)
       C(2,M) = G*C(2,M-1) &
               + 3.* (C(3,M)*C(4,M+1) + C(3,M+1)*C(4,M))
       C(4,M) = G* C(3,M-1) + 2.* (C(3,M) + C(3,M+1))
     END DO

 ! Construct last equation from the second boundary condition, of the form
 ! (-G*C(4,N-1))*S(N-1) + C(4,N)*S(N) = C(2,N).
 ! If slope is prescribed at right end, one can go directly to back-
 ! substitution, since C array happens to be set up just right for it
 ! at this point.

 IF (IBCEND < 1) THEN
   IF ( N /=3 .OR. IBCBEG /=0 ) THEN

     ! Not-a-knot and N > 2, and either N > 3 or also not-a-knot at
     ! left end point.

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

 ! Either (N=3 and not-a-knot also at left) or (N=2 and not not-a-
 ! knot at left end point).

 200 C(2,N) = 2. * C(4,N)
     C(4,N) = 1.
     G = -1. / C(4,N-1)
     GOTO 350

 ! Second derivative prescribed at right endpoint.

 250 C(2,N) = 3.*C(4,N) + C(3,N)*C(2,N)/2.
     C(4,N) = 2.
     G = -1. / C(4,N-1)
     GOTO 350
 300 IF (IBCEND < 1) THEN
       IF (IBCBEG > 0) GOTO 200

       ! Not-a-knot at right endpoint and at left endpoint and N = 2.

       C(2,N) = C(4,N)
       GOTO 400
     ELSE IF (IBCEND == 1) THEN
       GOTO 400
     ELSE
       GOTO 250
     END IF

 ! Complete forward pass of Gauss elimination.

 350 C(4,N) = G*C(3,N-1) + C(4,N)
     C(2,N) = (G*C(2,N-1) + C(2,N)) /C(4,N)

 ! Perform back substitution.

 400 J = L
 450 C(2,J) = (C(2,J) - C(3,J) *C(2,J+1)) /C(4,J)
     J = J - 1
     IF (J > 0) GOTO 450

 ! Generate cubic coefficients in each interval, i.e., the derivatives at its
 ! left endpoint, from value and slope at its endpoints.

 DO I = 2,N
   DXNOT = C(3,I)
   DIVDF1 = (C(1,I) - C(1,I-1)) /DXNOT
   DIVDF3 = C(2,I - 1) + C(2,I) - 2.*DIVDF1
   C(3,I-1) = 2.* (DIVDF1 - C(2,I-1) - DIVDF3) /DXNOT
   C(4,I-1) = (DIVDF3/DXNOT) * (6./DXNOT)
 END DO

 END SUBROUTINE CUBSPL

!===========================================================================

  REAL FUNCTION CUBVAL (XKNOT, COEF, KNOT, X1)

!---------------------------------------------------------------------------
!
!  Evaluates a function at X1 from from its cubic spline representation.
!
!***  Called by COSTRN, FOLD_AND_CONVOLVE, TXCNVD, TXCNVL
!***  Calls INTERV.  On exit from INTERV
!
!       MFLAG = -1  => X is to the left of interpolated range
!             =  1  => X is to the right of interpolated range
!             =  0  => X is in the interpolated range
!
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
!
!----------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER I,MFLAG,KNOT
 REAL XKNOT(KNOT),COEF(4,KNOT),X1,H

 ! Find index I of largest breakpoint to the left of X1.

 CALL INTERV ( XKNOT, KNOT-1, X1, I, MFLAG )
 H = X1 - XKNOT(I)
 IF (MFLAG == -1) H = 0.

 CUBVAL = ((COEF(4,I)*H/3.0 + COEF(3,I) )*0.5*H + COEF(2,I) )*H + COEF(1,I)

 END FUNCTION CUBVAL

!===========================================================================

 SUBROUTINE INTERV (XT,LXT,X,LEFT,MFLAG)

!---------------------------------------------------------------------------
!
!***   Called by CUBVAL, CUBINT
!
!********  Restructured April, 1997
!
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
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER LEFT,LXT,MFLAG,IHI,ILO,ISTEP,MIDDLE,J1
 REAL X,XT(LXT)
 SAVE ILO
 DATA ILO /1/

 ! Trivial returns when X is not in the range.

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

 ! Trivial return when X is already in the interval.

 IF ( (X <= XT(IHI)) .AND. (X >= XT(ILO)) ) THEN
   LEFT = ILO
   RETURN
 END IF

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

 ! Now XT(ILO) <= X < XT(IHI) . Narrow the interval.

 DO J1 = 1,LXT
   MIDDLE = (ILO + IHI)/2
   IF (MIDDLE == ILO) EXIT
   IF (X < XT(MIDDLE)) THEN
     IHI = MIDDLE
   ELSE
     ILO = MIDDLE
   END IF
 END DO

 ! Task complete.

 LEFT = ILO
 RETURN

 END SUBROUTINE INTERV

!===========================================================================

 REAL FUNCTION LINVAL (NX,XVAL,YVAL,X1,IDER)

!---------------------------------------------------------------------------
!
!  Evaluates a function at X1 from from its linear representation.
!
!***  Called by TXCNVD
!
!***  Calls INTERV.  On exit from INTERV
!
!       MFLAG = -1  => X is to the left of interpolated range
!             =  1  => X is to the right of interpolated range
!             =  0  => X is in the interpolated range
!
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
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER I,MFLAG,NX,IDER
 REAL XVAL(NX),YVAL(NX,3),X1,H

 ! Find index I of largest breakpoint to the left of X1.

 CALL INTERV ( XVAL, NX-1, X1, I, MFLAG )

 IF (IDER == 0) THEN      !  Computed derivative values stored at right node (26.01.00)
   LINVAL = YVAL(I+1,1)
 ELSE
   H = X1 - XVAL(I)
   IF (MFLAG == -1) H = 0.

   LINVAL = YVAL(I,1) + H * (YVAL(I+1,1) - YVAL(I,1)) / (XVAL(I+1) - XVAL(I))
 END IF

 END FUNCTION LINVAL

!==========================================================================!
!                                                                          !
!                      SamAir output subroutines.                          !
!                                                                          !
!==========================================================================!

  SUBROUTINE WRITE_LOG_FILE (NLG,MSG,MXERR,ERR_LVL)

!---------------------------------------------------------------------------
!
! This subroutine prints out warning and fatal error messages on the LOG file.
!
! NLG = output unit index
! MSG refers to error message index
! ERR_LVL = 1 for warnings;  = 2 for fatal errors
! MXERR = MAX (ERR_LVL)
!
!---------------------------------------------------------------------------

 INTEGER ERR_LVL, MSG, NLG, MXERR

 IF (MXERR == 0) OPEN (NLG,FILE = 'SamayaAir.log',STATUS = 'REPLACE')

 MXERR = MAX (ERR_LVL,MXERR)
 IF (ERR_LVL == 1) WRITE(NLG,101)
 IF (ERR_LVL == 2) WRITE(NLG,102)

 IF (MSG == 1) WRITE(NLG,1)
 IF (MSG == 2) WRITE(NLG,2)
 IF (MSG == 3) WRITE(NLG,3)
 IF (MSG == 4) WRITE(NLG,4)
 IF (MSG == 5) WRITE(NLG,5)
 IF (MSG == 6) WRITE(NLG,6)
 IF (MSG == 7) WRITE(NLG,7)
 IF (MSG == 9) WRITE(NLG,9)
 IF (MSG == 10) WRITE(NLG,10)
 IF (MSG == 11) WRITE(NLG,11)
 IF (MSG == 12) WRITE(NLG,12)
 IF (MSG == 13) WRITE(NLG,13)
 IF (MSG == 15) WRITE(NLG,15)
 IF (MSG == 16) WRITE(NLG,16)
 IF (MSG == 17) WRITE(NLG,17)
 IF (MSG == 18) WRITE(NLG,18)
 IF (MSG == 19) WRITE(NLG,19)
 IF (MSG == 20) WRITE(NLG,20)
 IF (MSG == 21) WRITE(NLG,21)
 IF (MSG == 22) WRITE(NLG,22)
 IF (MSG == 23) WRITE(NLG,23)
 IF (MSG == 25) WRITE(NLG,25)
 IF (MSG == 26) WRITE(NLG,26)
 IF (MSG == 51) WRITE(NLG,51)
 IF (MSG == 52) WRITE(NLG,52)
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

 IF (MSG == -99) WRITE(NLG,'(/)')

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
  9 FORMAT( T3,'CMP must be 11, 13, 2 or 3.  It has been reset to 3')
 10 FORMAT(/T3,'KPPM is outside allowed range.  It must be 0, 1, 3, 123, or 4.')
 11 FORMAT(/T3,'Input calibration and output must have the same units' &
           /T3,'Output will be in dB/dt units')
 12 FORMAT(/T3,'Input calibration and output must have the same units' &
           /T3,'Output will be in B units')
 13 FORMAT(//T3,'CMP must = 1, 2, or 3 for frequency domain modelling' &
            /T3,'CMP has been set to 1')
 15 FORMAT(/T3,'SamAir requires a minimum of 2 stations in a survey')
 16 FORMAT(/T3,'SURVEY must be either 1, 2 or 3 for time-domain.' &
           /T3,'SURVEY = 3 cannot be used for frequency-domain because'  &
           /T3,'Tx-Rx offset must be constant as a function of position')
 17 FORMAT(/T3,'IUNITS must be 1, 2 or 3')
 18 FORMAT(/T3,'CMP has been set to 1 for the vertical coplanar broadside option.')
 19 FORMAT(/T3,'IUNITS must be 1, 2 or 3.  It has been reset to the default')
 20 FORMAT( T3,'KACC must = 1.  It has been reset to 2')
 21 FORMAT( T3,'This version is limited to the direct solver option, SOLVER = 1')
 22 FORMAT( T3,'Resistivities must be positive.'/)
 23 FORMAT( T3,'This warning is generated, because you are using more than 2000 cells')
 25 FORMAT(/T6,'All transmitter positions are invalid for one or more', &
           /T6,'of the following reasons:' &
          //T3,'1. The transmitter is underground' &
           /T3,'2. The receiver is underground' &
           /T3,'3. The transmitter is not over the defined mesh position')
 26 FORMAT( T3,'Resistivity and permeability are negative.')
 51 FORMAT(/T3,'NLAYER must = 1 for the current Version.')
 52 FORMAT(/T3,'NLAYER must = 1 or 2 for the computation.')
 101 FORMAT(/T2,'WARNING'/T2,'-------'/)
 102 FORMAT(/T2,'FATAL ERROR'/T2,'----- -----'/)
 201 FORMAT(/T3,'This version of SamAir does not invert for AEM system parameters.')
 202 FORMAT(/T3,'X component inversion was requested but SamayaAir.inv contains only Z component data.')
 203 FORMAT(/T3,'Z component inversion was requested but SamayaAir.inv contains only X component data.')
 204 FORMAT(/T3,'The maximum number of iterations has been reduced to 20.')
 205 FORMAT(/T3,'CNVRG must be 1 or 2.  It has been reset to 1.')
 206 FORMAT(/T3,'For inversion, aircraft positions must be entered for every station', &
            /T3,'The automatic course option, SURVEY = 1 is not allowed.', &
            /T3,'SURVEY must be either 2, 3, -2, or -3.'/)
 207 FORMAT(/T3,'KCMP must = 12 or 21 for Frequency-domain inversion.')
 208 FORMAT(/T3,'ORDER must = 1 or 2.')
 209 FORMAT(/T3,'KCMP is restricted to the values: 1, 3, 13, 31, 123, 312')
 210 FORMAT(/T3,'X & Z component inversion was requested but SamayaAir.inv contains only X component data.')
 211 FORMAT(/T3,'X & Z component inversion was requested but SamayaAir.inv contains only Z component data.')
 212 FORMAT(/T3,'3 component inversion was requested but SamayaAir.inv contains only X component data.')
 213 FORMAT(/T3,'3 component inversion was requested but SamayaAir.inv contains only Z component data.')
 214 FORMAT(/T3,'3 component inversion was requested but SamayaAir.inv contains no Y component data.')
 215 FORMAT(/T3,'There is a component discrepency between the cfl and inv files.')
 220 FORMAT(/T3,'Order must be 1122 (for IIQQ),  1212 (for IQIQ),  2211 (for QQII),  2121 (for QIQI)')

 END SUBROUTINE  WRITE_LOG_FILE

!===========================================================================

 SUBROUTINE WRITE_FD (NW,np,TITLE,NSTAT,LINE,TXCLN,TXA90,SXD,SYD,SZ,RXD, &
                      RYD,RZ,CONFIG,NFRQ,FREQ,PRFL,QUNIT,PPFAC,BFFAC, &
                      PRM_FD,CMP,BFD)

!---------------------------------------------------------------------------
!
!***  Called by: MAIN
!***      Calls: WRSLV_FD
!
!  Prints the results of TEM computations.
!
!                NW - output unit number
!               np - unit number for mf1 file
!             NSTAT - Number of stations
!              LINE - Line number
!         TXCLN(JF) - transmitter orientation at frequency JF (in radians)
!             TXA90 - true for vertical co-planar briadside array
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
!
!---------------------------------------------------------------------------

 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 INTEGER NW,np,NSTAT,LINE(NSTAT),NFRQ,CMP,PRFL,TXD(NFRQ),JF,JS
 REAL FREQ(NFRQ),RZ(NSTAT,NFRQ),SZ(NSTAT),MPZ1(NSTAT),PRM_FD(NFRQ),PRM4,NORM(NFRQ), &
      YTR(NFRQ,NSTAT),TXCLN(NFRQ),PPFAC,BFFAC
 REAL(KIND=QL) RXD(NSTAT,NFRQ),RYD(NSTAT,NFRQ),MXD(NSTAT),MYD(NSTAT),SXD(NSTAT),SYD(NSTAT)
 COMPLEX BFD(NFRQ,NSTAT,3),BFD1(NFRQ,NSTAT,4)
 LOGICAL TXA90,WL
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
 ! Put all the results into  BFD1

 !   BFD1(JF,JS,1:3) will contain the in-line(1), transverse(2) and
 !                   vertical fields(3) respectively (in pT)
 !                   for frequency JF and station JS
 !   BFD1(JF,JS,4) will contain the maximally coupled field (ppm)
 !                 normalised to the parallel primary component.

 ! Print results at Tx-Rx midpoint

 MXD(1:NSTAT) = (RXD(1:NSTAT,1) + SXD(1:NSTAT) ) / 2.
 MYD(1:NSTAT) = (RYD(1:NSTAT,1) + SYD(1:NSTAT) ) / 2.
 MPZ1(1:NSTAT) = (RZ(1:NSTAT,1) + SZ(1:NSTAT) )  / 2.

 BFD1 = (0.,0.)
 IF (CMP == 1) WRITE(NW,10)
 DO JF = 1, NFRQ
   PRM4 = BFFAC * ABS (PRM_FD(JF))                   ! coupled primary in nT, pT or fT
   NORM(JF) = PPFAC / PRM4                         ! for pct, ppt, ppm or ppb output

   IF (CMP == 1) WRITE(NW,'(I6,T17,G12.4,T34,g12.4)')  JF,PRM4,NORM(JF)

   BFD1(JF,1:NSTAT,1:3)   = BFFAC * BFD(JF,1:NSTAT,1:3)       ! total field in nT, pT or fT
 END DO

 ! Normalise them as indicated in input data file.
 ! For CMP = 1, compute component along Tx direction

 TXD(1:NFRQ) = NINT ( 180. * TXCLN(1:NFRQ) / 3.1416)
 WRITE(NW,1)
 IF (CMP > 1) WRITE(NW,8)
 IF (CMP > 2) WRITE(NW,9)

 DO JF = 1, NFRQ

   ! maximally coupled response
   IF (TXA90) THEN
     BFD1(JF,1:NSTAT,4) =  BFD1(JF,1:NSTAT,2)
   ELSE
     BFD1(JF,1:NSTAT,4) =  BFD1(JF,1:NSTAT,1) * SIN (TXCLN(JF)) &
                        +  BFD1(JF,1:NSTAT,3) * COS (TXCLN(JF))
   END IF
   BFD1(JF,1:NSTAT,4) = NORM(JF) * BFD1(JF,1:NSTAT,4)
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
 END IF

 IF (CMP > 1) THEN
   WRITE(NW,'(/3X,A)') TRIM (TITLE)     !  Vertical component
   WRITE(NW,14) QR,CMP3,QUNIT
   YTR(1:NFRQ,1:NSTAT) = REAL(BFD1(1:NFRQ,1:NSTAT,3))
   CALL WRSLV_FD (NW,PRFL,MXD,MYD,MPZ1,NSTAT,NFRQ,FREQ,TXD,CMP,CONFIG,YTR)

   WRITE(NW,14) QI,CMP3,QUNIT
   YTR(1:NFRQ,1:NSTAT) = AIMAG(BFD1(1:NFRQ,1:NSTAT,3))
   CALL WRSLV_FD (NW,PRFL,MXD,MYD,MPZ1,NSTAT,NFRQ,FREQ,TXD,CMP,CONFIG,YTR)

   WRITE(NW,'(//3X,A)') TRIM (TITLE)     !  In-line component
   WRITE(NW,14) QR,CMP1,QUNIT
   YTR(1:NFRQ,1:NSTAT) = REAL (BFD1(1:NFRQ,1:NSTAT,1))
   CALL WRSLV_FD (NW,PRFL,MXD,MYD,MPZ1,NSTAT,NFRQ,FREQ,TXD,CMP,CONFIG,YTR)

   WRITE(NW,14) QI,CMP1,QUNIT
   YTR(1:NFRQ,1:NSTAT) = AIMAG (BFD1(1:NFRQ,1:NSTAT,1))
   CALL WRSLV_FD (NW,PRFL,MXD,MYD,MPZ1,NSTAT,NFRQ,FREQ,TXD,CMP,CONFIG,YTR)
 END IF

 IF (CMP == 3) THEN
   WRITE(NW,'(//3X,A)') TRIM (TITLE)     ! Transverse component
   YTR(1:NFRQ,1:NSTAT) = REAL(BFD1(1:NFRQ,1:NSTAT,2))
   WRITE(NW,14) QR,CMP2,QUNIT
   CALL WRSLV_FD (NW,PRFL,MXD,MYD,MPZ1,NSTAT,NFRQ,FREQ,TXD,CMP,CONFIG,YTR)

   YTR(1:NFRQ,1:NSTAT) = AIMAG(BFD1(1:NFRQ,1:NSTAT,2))
   WRITE(NW,14) QI,CMP2,QUNIT
   CALL WRSLV_FD (NW,PRFL,MXD,MYD,MPZ1,NSTAT,NFRQ,FREQ,TXD,CMP,CONFIG,YTR)
 END IF

 ! Finish writing SamayaAir.mf1

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

 1 FORMAT(/T3,'FREQUENCY-DOMAIN SamAir OUTPUT' /T3,33('-'))
 3 FORMAT(//T3,'SINGLE COMPONENT RESPONSE ALONG TRANSMITTER DIPOLE DIRECTION')
 4 FORMAT(//T10,A,'COMPONENT - ',A)
 8 FORMAT(/T3,'The IN-LINE component is defined as the horizontal component along' &
          /T3,'the flight path.  It is positive in the forward flight direction.')
 9 FORMAT(/T3,'The TRANSVERSE component is the horizontal component',&
          /T3,'perpendicular to the flight path.'/)
 10 FORMAT(/T4,'Frequency    Coupled Primary   Normalisation' &
           /T4,'Index        Field (pT)        Factor'        &
           /T4,'---------    ---------------   ------------')
 14 FORMAT(/T10,2A,' COMPONENT - ',A)
 15 FORMAT(//T2,'TITLE:  ',A/T2,'-----')
 20 FORMAT(I5,2F12.1,F8.1,2F12.1,F8.1,50G13.4)

 END SUBROUTINE WRITE_FD

!===========================================================================

 SUBROUTINE WRSLV_FD (NW,PRFL,RXD1,RYD1,RZ1,NSTAT,NFRQ,FREQ,TXD,CMP,CONFIG,YTR)

!---------------------------------------------------------------------------
!
!***  Called by: WRITE_FD
!***      Calls: (conditional) WRSLVS_FD
!
!  Writes frequency-domain output in profile form
!  if PRFL = 1  or in spectral output if PRFL = 2
!
!         TXD(JF) - transmitter orientation at frequency JF (in degrees)
!      YTR(JF,JS) - field at station JS for frequency JF.
!
!    All other variables defined in SUBROUTINE WRITE_FD
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 INTEGER NW,NFRQ,PRFL,CMP,NSTAT,TXD(NFRQ),KRX,KRY,KRZ,JS
 REAL RZ1(NSTAT),FREQ(NFRQ),YTR(NFRQ,NSTAT)
 REAL(KIND=QL) RXD1(NSTAT),RYD1(NSTAT)
 CHARACTER(LEN=3) CONFIG(NFRQ)

 IF (PRFL == 2) THEN
   CALL WRSLVS_FD (NW,RXD1,RYD1,RZ1,NSTAT,NFRQ,FREQ,TXD,CMP,YTR)
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
     KRX = NINT (RXD1(JS))
     KRY = NINT (RYD1(JS))
     KRZ = NINT (RZ1(JS))
     IF (CMP == 1) THEN
       WRITE(NW,3) JS,KRY,KRX,KRZ,YTR(1:NFRQ,JS)
     ELSE
       WRITE(NW,6) JS,KRY,KRX,KRZ,YTR(1:NFRQ,JS)
     END IF
   END DO

 END IF

 1 FORMAT(/4X,'RECEIVER POSITION '/T10,'EAST     NORTH    ALT',F10.0,40F12.0)
 2 FORMAT(/4X,'RECEIVER POSITION '/T10,'EAST     NORTH    ALT',40G11.3)
 3 FORMAT(I3,2I10,I7,40F12.2)
 4 FORMAT(T37,30(A,9X))
 5 FORMAT(/4X,'RECEIVER POSITION '/T10,'EAST     NORTH    ALT',30G14.4)
 6 FORMAT(I3,2I10,I7,40G14.4)

 END SUBROUTINE WRSLV_FD

!===========================================================================

 SUBROUTINE WRSLVS_FD (NW,RXD1,RYD1,RZ1,NSTAT,NFRQ,FREQ,TXD,CMP,YTR)

!---------------------------------------------------------------------------
!
!***  Called by: WRSLV_FD
!
!  Writes frequency-domain output in spectral form
!
!         TXD(JF) - transmitter orientation at frequency JF (in degrees)
!      YTR(JF,JS) - field at station JS for frequency JF.
!
!    All other variables defined in SUBROUTINE WRITE_FD
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18), NCOL=40
 INTEGER NW,NFRQ,CMP,NSTAT,TXD(NFRQ),NBLKS,J1,J2,JB,JF
 REAL RZ1(NSTAT),FREQ(NFRQ),YTR(NFRQ,NSTAT)
 REAL(KIND=QL) RXD1(NSTAT),RYD1(NSTAT)

 NBLKS = NSTAT / NCOL
 IF (MOD (NSTAT,NCOL) > 0) NBLKS = NBLKS + 1

 DO J1 = 1,NBLKS
   JF = J1 * NCOL
   JB = JF - NCOL + 1
   JF = MIN (JF,NSTAT)

   WRITE(NW,'(/T14,A,F9.0,39F13.0)') 'Receiver  Z',RZ1(JB:JF)
   WRITE(NW,'(T13,A,F9.0,39F13.0)') 'Positions  E',RYD1(JB:JF)
   WRITE(NW,'(T24,A,F9.0,39F13.0)') 'N',RXD1(JB:JF)
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

!===========================================================================

 SUBROUTINE WRITE_TD (NW,np,TITLE,NSTAT,LINE,SXD,SYD,SZ,TXDEG,RXD,RYD,RZ, & 
                      XRX,YRX,ZRX,NCHNL,TMS,PRFL,QUNIT,BUNIT,PPFAC,BFFAC, &
                      PRM_TD,CMP,KPPM,BTD)

!---------------------------------------------------------------------------
!
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
!          JC = 1 => in-line component; 2 => transverse component;  3 => vertical component
!
!---------------------------------------------------------------------------

 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 REAL, PARAMETER ::  TOL=1.E-3
 INTEGER NW,np,PRFL,NSTAT,LINE(NSTAT),NCHNL,MCHNL,CMP,KPPM,JC,JS
 REAL TMS(NCHNL),RZ(NSTAT,1),PRM_TD(3),XBD,ZBD,XRN,NORM(4), &
      PPFAC,BFFAC,YTR(NCHNL,NSTAT),XQ(4)
 REAL, DIMENSION(NSTAT) :: SZ,XRX,YRX,ZRX,TXDEG
 REAL, DIMENSION(NCHNL,NSTAT,3) :: BTD
 REAL, ALLOCATABLE :: QDATA(:)
 REAL(KIND=QL) SXD(NSTAT),SYD(NSTAT),RXD(NSTAT,1),RYD(NSTAT,1)
 LOGICAL WL,PRTYBD,PRTX,PRTY,PRTZ
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

 ! Set up receiver locations and write TITLE.

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
   BTD = BFFAC * BTD
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
     BTD(1:NCHNL,1:NSTAT,JC) = PPFAC * BTD(1:NCHNL,1:NSTAT,JC) / NORM(JC)
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

 ! Finish writing SamayaAir.mf1.

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

 1 FORMAT(//T3,'TIME-DOMAIN SamAir OUTPUT'/T3,28('-') &
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
 20 FORMAT(I5,2F12.1,F8.1,F6.1,2F12.1,F8.1,200G13.4)
 41 FORMAT(/T3,'The TRANSVERSE component is the horizontal component along' &
           /T3,'perpendicular to the flight path.')

 END SUBROUTINE WRITE_TD

!===========================================================================

 SUBROUTINE WRSLV (NW,PRFL,RZ,RXD,RYD,NSTAT,SZ,SXD,SYD,NCHNL,TMS,YTR)

!---------------------------------------------------------------------------
!
!***  Called by: WRITE_TD
!***      Calls: WRSLVP
!
!  Writes time-domain output in temporal form for receiver
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18), NCOL=30
 INTEGER NW,PRFL,NSTAT,NCHNL,NBLKS,J1,JB,JF,JT
 REAL RZ(NSTAT,1),SZ(NSTAT),TMS(NCHNL),YTR(NCHNL,NSTAT)
 REAL(KIND=QL) SXD(NSTAT),SYD(NSTAT),RXD(NSTAT,1),RYD(NSTAT,1)

 IF (PRFL == 1) THEN
   CALL WRSLVP (NW,RXD,RYD,RZ,NSTAT,NCHNL,TMS,YTR)
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

!===========================================================================

 SUBROUTINE WRSLVP (NW,RXD,RYD,RZ,NSTAT,NCHNL,TMS,YTR)

!---------------------------------------------------------------------------
!
!***  Called by: WRSLV
!
!  Writes time-domain output in profile form for receiver
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18), NCOL=30
 INTEGER NW,NSTAT,NCHNL,NBLKS,J1,JB,JF,KRX,KRY,KRZ,JS
 REAL RZ(NSTAT,1),TMS(NCHNL),YTR(NCHNL,NSTAT)
 REAL(KIND=QL) RXD(NSTAT,1),RYD(NSTAT,1)
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
     KRZ = NINT (RZ(JS,1))
     KRX = NINT (RXD(JS,1))
     KRY = NINT (RYD(JS,1))
     WRITE(NW,3) JS,KRY,KRX,KRZ,YTR(JB:JF,JS)
   END DO
   WRITE(NW,5)
 END DO

 1 FORMAT(T12,'RECEIVER POSITION',T35,30(A:,5X))
 2 FORMAT(T10,'EAST     NORTH    ALT',F10.3,29F13.3)
 3 FORMAT(I3,2I10,I7,1X,30G13.4)
 5 FORMAT (85('-')/)

 END SUBROUTINE WRSLVP

!============================================================================

 SUBROUTINE WRITE_INVMDL (NW,FINAL,ITS,NN,NE,NZ,NPAR,XPAR,IMPORT,NEL,NER, &
                          NNL,NNR,NZT,NZB)

!----------------------------------------------------------------------------
!
!***  Called by: NLSQ
!
!  Write the inversion output, including importance for the model
!  parameters.
!
!----------------------------------------------------------------------------
!
!     INPUTS:
!
!   NW     = Output unit.
!   NWI    = Output unit.
!   FINAL  = Logical flag; TRUE if final model, FALSE if intermediate model.
!   ITS    = Print model after iteration ITS.
!   NCELL  = Number of cells in inverse model.
!   XPAR   = Transformed parameter array.
!   IMPORT = Model parameter importance.
!
!----------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER NW,ITS,JC1,I1,J1,K1,NN,NE,NZ,NPAR,I2,J2,K2,NEL,NER,NNL,NNR,NZT,NZB
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
   DO K1 = NZT+1, NZ-NZB-1
     DO J1 = NNL+1, NN-NNR-1
       DO I1 = NEL+1, NE-NER-1
         JC1 = (J1-NNL-1)*(NZ-NZT-NZB-1)*(NE-NEL-NER-1) + (K1-NZT-1)*(NE-NEL-NER-1) + (I1-NEL)
         RES = XPAR(JC1)
         IMP = IMPORT(JC1)
         I2 = I1 - 1
         J2 = J1 - 1
         K2 = K1 - 1
         WRITE(NW,4) I2,J2,K2,JC1,RES,IMP
       END DO
     END DO
   END DO             
 ELSE IF (.NOT. FINAL) THEN ! Only write intermediate model.
   WRITE(NW,5) ! Write output header.
   DO K1 = NZT+1, NZ-NZB-1
     DO J1 = NNL+1, NN-NNR-1
       DO I1 = NEL+1, NE-NER-1
         JC1 = (J1-NNL-1)*(NZ-NZT-NZB-1)*(NE-NEL-NER-1) + (K1-NZT-1)*(NE-NEL-NER-1) + (I1-NEL)
         RES = XPAR(JC1)
         I2 = I1 - 1
         J2 = J1 - 1
         K2 = K1 - 1
         WRITE(NW,6) I2,J2,K2,JC1,RES
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
 4 FORMAT(T12,I4,'     ',I4,'     ',I4,'     ',I4,'       ',G12.4,'     ',G12.4)
 5 FORMAT(//T9,'----------------------------------------------------------'&
           /T9,'  Element    Element    Element    Element   Resistivity  '&
           /T9,'  Easting    Northing   Depth      Number      (Ohm-m)    '&
           /T9,'----------------------------------------------------------')
 6 FORMAT(T12,I4,'      ',I4,'      ',I4,'       ',I4,'       ',G12.4)

 END SUBROUTINE WRITE_INVMDL

!============================================================================

 SUBROUTINE WRITE_MISFIT (NW,ITSPR,NSTAT,NDATA,MCHNL,TDFD,NFRQ,FREQ,CMP, & 
                          NCHNL,VERR,XMODL,XDATA)

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
!     NSTAT - number of stations points
!     NDATA - total number of data points
!     MCHNL - total number of data points per station
!      TDFD = 1: time domain  or 2: frequency-domain
!      FREQ - array of NFRQ frequencies for FD inversion
!     XDATA - 1D array of NDATA measured data points
!     XMODL - 1D array of NDATA computed data points from most recent model
!      VERR - symmetric error at each data point
!       CMP = 11 => inversion on horizontal in-line component only
!           = 13 => inversion on vertical component only
!           = 2  => joint inversion on vertical & horizontal in-line components
!           = 3  => inversion on all 3 components
!           = 4 or 42 => inversion on total field
!
!----------------------------------------------------------------------------

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

 ! Put data into matrix form.

 CALL CNVRT2_2D

 RVERR = 100. * RVERR  ! Convert to percent

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
           /T5,'The first line is the percent symmetric error.' &
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

     WRITE(NW,'(/6X,30(A:,5X))') CHN(1:NCHN)
     DO JS = 1,NSTAT
       WRITE(NW,'(/I4,50G13.4)') JS,RVERR(N1:N2,JS)
       WRITE(NW,'( 4X,50G13.4)') RMODL(N1:N2,JS)
       WRITE(NW,'( 4X,50G13.4)') RDATA(N1:N2,JS)
     END DO

   END SUBROUTINE PRT_TD

   SUBROUTINE PRT_FD

     WRITE(NW,'(/4X,40F11.0)') FREQ(1:NFRQ)
     DO JS = 1,NSTAT
       WRITE(NW,'(/I4,40F11.2)') JS,RVERR(N1:N2,JS)
       WRITE(NW,'( 4X,40F11.2)') RMODL(N1:N2,JS)
       WRITE(NW,'( 4X,40F11.2)') RDATA(N1:N2,JS)
     END DO

   END SUBROUTINE PRT_FD

 END SUBROUTINE WRITE_MISFIT

!==========================================================================!
!                                                                          !
!                  SamAir forward modelling subroutines.                   !
!                                                                          !
!==========================================================================!

 SUBROUTINE SAMAYA_3D (NFRQ,FREQ,SOLVER,NE,NN,NZ,ELOC,NLOC,ZLOC,NTXD,TXCLN, &
                       NSTAT,NTX,SX,SY,SZ,FANGLE,NRX,NCOMP,RX,RY,RZ,ND,NS,  &
                       LITH,LYTH,NLITH,NPROP,NLYR,LITHL,THK,JCBN,TDFD,CMP,  &
                       NPART,XPART,NEL,NER,NNL,NNR,NZT,NZB,IPR)

!-------------------------------------------------------------------------
!
!*** Called from MAIN
!*** Calls
!
! Computes the frequency-domain response for the 3D model
!
!               INPUT
!               -----
!       FREQ - array of NFRQ frequencies at which the rsponse will be computed.
!     SOLVER - integer variable to vary solution methodology
!         NE - number of user specified nodes from south to north
!         NN - number of user specified nodes from west to east
!         NZ - number of user specified nodes top to bottom
!       ELOC - east coordinates of user specified nodes positive east along Y axis
!       NLOC - east coordinates of user specified nodes positive north along X axis
!       ZLOC - depth coordinates of user specified nodes positive down
!              The top most node(s) have ZLOC defined as ZLOC = 0
!       NTXD - number of transmiter orietations.
!            = 1 in time-domain and NFRQ in frequency-domain
!      TXCLN - receiver orientation (nose up = positive)  level flight = 0
!      NSTAT - number of transmitter positions
!         SX - north transmitter coordinate
!         SY - east transmitter coordinate
!         SZ - altitude above topmost user node
!     FANGLE - flight direction in radians, clockwise from north
!        NRX - number of receivers
!  RX(JS,JR) - north coordinate of receiver JR for station JS (usually JR = 1)
!         RY - east coordinate of receiver JR at station JS
!         RZ - altitude of receiver JR at station JS above topmost user-defined node
!       LITH - lithology-index arrays of the domain
!       LYTH - lithology-list arrays of the domain
!      NLITH - number of distinct lithologial units
!      NPROP - number of lithologies (currently set to 7)
!
!               OUTPUT
!               ------
!  HFD(I,J,K,L) - Kth component of the complex frequency-domain impulse
!                 response magnetic field (H) at transmitter J, receiver L
!                 for frequency I.
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 REAL, PARAMETER :: MU0 = 12.56637E-7, TWOPI = 6.283185, EPS0 = 8.854156E-12
 COMPLEX, PARAMETER :: ONE=(1.,0.)
 LOGICAL JCBN, INVERT
 INTEGER NFRQ,SOLVER,NE,NN,NZ,NTXD,NSTAT,NTX,NRX,JF,NELEM,ND,NS,ITX,NLITH, &
         NPROP,LITH(NE,NN,NZ),IFIN,NLYR,ICOLE(2),J,JL,LITHL(NLYR),NINEDG,  &
         IEDG,INTN,IBND,TDFD,NPART,CMP,NCOMP,JR,NEL,NER,NNL,NNR,NZT,NZB,IPR
 REAL LYTH(NLITH+1,NPROP),FREQ(NFRQ),FRQ,TXCLN(NTXD),TXCLN1,XPART(NPART)
 COMPLEX KSQ(NLITH+1),SIGC(NLITH+1),KSQR(2),P,SIGL(2),SIGCV(NPART)
 REAL, DIMENSION (NE,NN,NZ) :: ELOC,NLOC,ZLOC
 REAL, DIMENSION (NSTAT) :: SX,SY,SZ,FANGLE
 REAL, DIMENSION (NSTAT,NRX) :: RX,RY,RZ
 REAL, DIMENSION (NLYR) :: THK,CTAU,CFREQ,CALF,RMU,REPS,RES,CHRG
 INTEGER, ALLOCATABLE :: ICED(:),NUME(:),GBL(:),NEDGE(:,:)
 REAL, ALLOCATABLE, DIMENSION(:) :: ELED,NLED,ZLED,DIME,DIMN,DIMZ,SX1,SY1,SZ1, &
                                    TXANGLE,FANGLE1
 REAL, ALLOCATABLE, DIMENSION(:,:) :: GBN
 COMPLEX, ALLOCATABLE :: F(:,:),SMB(:,:)
 Integer :: tvals(8)  
 Real :: pfin  

 SOLVER = 2
 NINEDG = ((NE-1)*NZ + NE*(NZ-1)) * NN + NE*NZ*(NN-1)
 NELEM  = (NE-1)*(NN-1)*(NZ-1)

 ALLOCATE (DIME(NELEM),DIMN(NELEM),DIMZ(NELEM),ELED(NINEDG),NLED(NINEDG), &
           ZLED(NINEDG),ICED(NINEDG),NUME(NINEDG),GBN(3,NINEDG),GBL(NINEDG), &
           NEDGE(12,(NE-1)*(NN-1)*(NZ-1)))

 CALL PREP (NE,NN,NZ,ELOC,NLOC,ZLOC,NELEM,NINEDG,ELED,NLED,ZLED, &
            DIME,DIMN,DIMZ,ICED,IEDG,IBND,INTN,GBN,GBL,NUME,NEDGE)

 DO JL = 1, NLYR
   RES(JL)  =  LYTH(LITHL(JL),1)
   RMU(JL)  =  LYTH(LITHL(JL),3)
   REPS(JL) =  LYTH(LITHL(JL),4)
   CHRG(JL) =  LYTH(LITHL(JL),5)
   CTAU(JL) =  LYTH(LITHL(JL),6)
   CFREQ(JL) = LYTH(LITHL(JL),7)
   CALF(JL) = 1. - CHRG(JL)
 END DO

 ICOLE = 0

 DO JL = 1,NLYR
   IF (CFREQ(JL) > 1.E-3 .AND. CTAU(JL) > 1.E-12) THEN
     ICOLE(JL) = 1
   END IF
 END DO

 SIGL(1) = CMPLX(1./RES(NLYR))
 SIGL(2) = CMPLX(1./RES(NLYR))

 OPEN(97,FORM='UNFORMATTED',STATUS='SCRATCH')
 OPEN(99,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=1000,STATUS='SCRATCH')

 ALLOCATE (SX1(NTX),SY1(NTX),SZ1(NTX),TXANGLE(NTX),FANGLE1(NTX))
 SX1 = 0. ; SY1 = 0. ; SZ1 = 0. ; TXANGLE = 0. ; FANGLE1 = 0.

 ! IF (JCBN) THEN
 !   WRITE(*,1)
 ! ELSE
 !   WRITE(*,2)
 ! END IF

 REWIND(ND) 
 REWIND(NS)

 TXCLN1 = TXCLN(1)

 ! Write (*, 5)
 FRQ_LOOP: DO JF = 1, NFRQ
   FRQ = FREQ(JF)
   IFIN = 100 * (JF-1) / NFRQ
   pfin = 100. * (jf - 1)/nfrq
   Call date_and_time(values = tvals)
   IF (IPR == 1) THEN
   	  If (JCBN) Then
        WRITE(*,3) tvals(1:3), tvals(5:7), jf, nfrq, frq, pFIN, 'Fields & sensitivities'
      ELSE
        WRITE(*,3) tvals(1:3), tvals(5:7), jf, nfrq, frq, pFIN, 'Fields only'
      End If
   END IF

   CALL SET_KSQ (LYTH,NLITH,NPROP,FRQ,KSQ,SIGC)

   CALL SET_SIGCV (LYTH,NLITH,NPROP,NPART,NE,NN,NZ,FRQ,SIGCV,LITH,XPART)

   DO J = NLYR, 1, -1
     P = ICOLE(J) * ((0.,1.)*TWOPI*FRQ*CTAU(J))**CFREQ(J)
     SIGL(J) = SIGL(J)*(ONE+P)/(ONE+CALF(J)*P) + (0.,1.)*TWOPI*FRQ*EPS0*REPS(J)       !  Add in displacement term
     KSQR(J) = (0.,1.)*TWOPI*FRQ*MU0*RMU(J)*SIGL(J)
   END DO

   IF(ABS(THK(1)) < 1.E-4) THEN
     SIGL(2) = SIGL(1)
     KSQR(2) = KSQR(1)
   END IF

   JR = 1
   IF (NTXD > 1) JR = JF
   TXCLN1 = TXCLN(JR)

   CALL SET_TX_INC_POS (JCBN,TDFD,NSTAT,NCOMP,NRX,JR,CMP,SX,SY,SZ, & 
                        RX,RY,RZ,FANGLE,TXCLN1,SX1,SY1,SZ1,FANGLE1, &
                        TXANGLE,NTX)

   ALLOCATE (F(INTN,NTX))

   CALL EPEDGE (SX1,SY1,SZ1,NTX,TXANGLE,FANGLE1,FRQ,NLYR,SIGL,KSQR,THK, &
                ITX,NE,NN,NZ,ELOC,NLOC,ZLOC,F,RMU,NINEDG,INTN,ELED, & 
                NLED,ZLED,NUME,ICED,NEDGE)

   ALLOCATE (SMB(INTN,INTN)) ; SMB = (0.,0.) 

   CALL BMATE (NE,NN,NZ,NINEDG,NEDGE,ELED,NLED,ZLED,FRQ,SMB,ELOC,NLOC, &
               ZLOC,NUME,INTN,IBND,SIGL,NLYR,KSQR,THK,RMU,GBL,GBN,NPART, &
               SIGCV,NEL,NER,NNL,NNR,NZT,NZB)

   CALL MATINV (SMB,F,INTN,NTX)

   DEALLOCATE (SMB)

   CALL HFIELD (ELOC,NLOC,ZLOC,NE,NN,NZ,LITH,NLITH,FRQ,NSTAT,NTX, & 
                RX,RY,RZ,NRX,ND,F,DIME,DIMN,DIMZ,NELEM,TXCLN1,FANGLE, &
                SX,SY,SZ,NLYR,RMU,THK,KSQR,INTN,NINEDG,NUME,NEDGE, &
                NPART,SIGCV,LYTH,NPROP,NEL,NER,NNL,NNR,NZT,NZB)

   IF (JCBN) CALL SENS_FD_CONSTRUCT (NS,NSTAT,NTX,NCOMP,INTN,NINEDG,NN,NZ,NE, &
                                     NUME,NEDGE,NELEM,DIME,DIMN,DIMZ,FRQ,F, &
                                     NEL,NER,NNL,NNR,NZT,NZB)

   DEALLOCATE (F)

 END DO FRQ_LOOP

 DEALLOCATE (ELED,NLED,ZLED,DIME,DIMN,DIMZ,NUME,ICED,SX1,SY1,SZ1,TXANGLE,FANGLE1, &
             GBL,GBN,NEDGE)

! CLOSE (97)
! CLOSE (99)

 ! 1 FORMAT(/T3,'Computing fields and sensitivities:',/T3,' ')
 ! 2 FORMAT(/T3,'Computing fields:',/T3,' ')
 ! 3 FORMAT(T3,'Frequency',I3,' =',G12.4,' ; ',I8,' percent done')
 ! 3   Format (6x, i2.2, '/', i2.2, '/', i4.4, 2x, i2.2, ':', i2.2, ':', i2.2, &
 !           5x, en13.4, 15x, i4)
 3 Format (2x, i4.4, '-', i2.2, '-', i2.2, 'T', i2.2, ':', i2.2, ':', i2.2, &
 	       ': Frequency ',i4, ' of ', i4, '  =',1x, en10.2,' Hz; ', f5.2,' % complete (', a, ')')
 ! 4 FORMAT(T3,'Frequency',I3,' =',G12.4)
 ! 5 Format (/, 12x, 'Date', 6x, 'Time', 15x, 'Frq', 9x, '% complete', &
          ! /, 12x, '----', 6x, '----', 15x, '---', 9x, '----------')

 END SUBROUTINE SAMAYA_3D

!===========================================================================

 SUBROUTINE PREP (NE,NN,NZ,ELOC,NLOC,ZLOC,NELEM,NINEDG,ELED,NLED,ZLED, &
                  DIME,DIMN,DIMZ,ICED,IEDG,IBND,INTN,GBN,GBL,NUME,NEDGE)

!---------------------------------------------------------------------------
!
!*** Called by SAMAYA_3D
!
! Calculates the cell dimension and the coordinates of the edges.
!
!        NE - number of target nodes in X-direction
!        NN - number of target nodes in Y-direction
!        NZ - number of target nodes in Z-direction
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
 INTEGER NE,NN,NZ,I,IX,IY,IZ,NINEDG,NELEM,ICED(NINEDG),IEDG,IBND,INTN, &
         GBL(NINEDG),NUME(NINEDG),NEDGE(12,(NE-1)*(NN-1)*(NZ-1)),NN1,NN2, &
         NN3,NBN,NEZ
 REAL ELOC(NE,NN,NZ),NLOC(NE,NN,NZ),ZLOC(NE,NN,NZ),ELED(NINEDG),NLED(NINEDG), &
      ZLED(NINEDG),DIME(NELEM),DIMN(NELEM),DIMZ(NELEM),GBN(3,NINEDG)

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

 ! Numbering the edges of the elements.

 NEZ = NZ*(NE-1) + NE*(NZ-1)
 DO IY = 1, NN-1
   DO IZ = 1, NZ-1
     DO IX = 1, NE-1
       NBN = (IY-1)*(NE-1)*(NZ-1) + (IZ-1)*(NE-1) + IX
       NN1 = (NEZ + NE*NZ) * (IY-1)
       NN2 = NN1 + ((NE-1) + NE)* (IZ-1)
       NN3 = NN1 + NEZ + (IZ-1)*NE
       NEDGE(1, NBN) = NN2 + IX
       NEDGE(2, NBN) = NN2 + (NE-1) + IX
       NEDGE(3, NBN) = NEDGE(2,NBN) + 1
       NEDGE(4, NBN) = NEDGE(2,NBN) + NE
       NEDGE(5, NBN) = NN3 + IX
       NEDGE(6, NBN) = NEDGE(5,NBN) + 1
       NEDGE(7, NBN) = NEDGE(5,NBN) + NE
       NEDGE(8, NBN) = NEDGE(7,NBN) + 1
       NEDGE(9, NBN) = NEDGE(1,NBN) + NEZ + NE*NZ
       NEDGE(10,NBN) = NEDGE(2,NBN) + NEZ + NE*NZ
       NEDGE(11,NBN) = NEDGE(3,NBN) + NEZ + NE*NZ
       NEDGE(12,NBN) = NEDGE(4,NBN) + NEZ + NE*NZ
     END DO
   END DO
 END DO

 IEDG = 0
 INTN = 0
 IBND = 0
 NUME = 0

 DO IZ = 1, NZ
   DO IX = 1, NE-1
     IEDG = IEDG + 1
     ICED(IEDG) = 1
     ELED(IEDG) = (ELOC(IX,1,IZ) + ELOC(IX+1,1,IZ)) / 2.
     NLED(IEDG) = (NLOC(IX,1,IZ) + NLOC(IX+1,1,IZ)) / 2.
     ZLED(IEDG) = (ZLOC(IX,1,IZ) + ZLOC(IX+1,1,IZ)) / 2.
     IBND = IBND + 1
     NUME(IEDG) = -IBND
     GBN(1,IBND) = ELED(IEDG)
     GBN(2,IBND) = NLED(IEDG)
     GBN(3,IBND) = ZLED(IEDG)
   END DO
   IF (IZ < NZ) THEN
     DO IX = 1, NE
       IEDG = IEDG + 1
       ICED(IEDG) = 3
       ELED(IEDG) = (ELOC(IX,1,IZ) + ELOC(IX,1,IZ+1)) / 2.
       NLED(IEDG) = (NLOC(IX,1,IZ) + NLOC(IX,1,IZ+1)) / 2.
       ZLED(IEDG) = (ZLOC(IX,1,IZ) + ZLOC(IX,1,IZ+1)) / 2.
       IBND = IBND + 1
       NUME(IEDG) = -IBND
       GBN(1,IBND) = ELED(IEDG)
       GBN(2,IBND) = NLED(IEDG)
       GBN(3,IBND) = ZLED(IEDG)
     END DO
   END IF
 END DO

 DO IZ = 1, NZ
   DO IX = 1, NE
     IEDG = IEDG + 1
     ICED(IEDG) = 2
     ELED(IEDG) = (ELOC(IX,1,IZ) + ELOC(IX,2,IZ)) / 2.
     NLED(IEDG) = (NLOC(IX,1,IZ) + NLOC(IX,2,IZ)) / 2.
     ZLED(IEDG) = (ZLOC(IX,1,IZ) + ZLOC(IX,2,IZ)) / 2.
     IF ((IX ==  1) .OR. (IX == NE) .OR. (IZ == 1) .OR. (IZ == NZ)) THEN
       IBND = IBND + 1
       NUME(IEDG) = -IBND
       GBN(1,IBND) = ELED(IEDG)
       GBN(2,IBND) = NLED(IEDG)
       GBN(3,IBND) = ZLED(IEDG)
     ELSE
       INTN = INTN + 1
       NUME(IEDG) = INTN
     END IF
   END DO
 END DO

 DO IY = 2, NN-1
   DO IZ = 1, NZ
     DO IX = 1, NE-1
       IEDG = IEDG + 1
       ICED(IEDG) = 1
       ELED(IEDG) = (ELOC(IX,IY,IZ) + ELOC(IX+1,IY,IZ)) / 2.
       NLED(IEDG) = (NLOC(IX,IY,IZ) + NLOC(IX+1,IY,IZ)) / 2.
       ZLED(IEDG) = (ZLOC(IX,IY,IZ) + ZLOC(IX+1,IY,IZ)) / 2.
       IF ((IZ == 1) .OR. (IZ == NZ)) THEN
         IBND = IBND + 1
         NUME(IEDG) = -IBND
         GBN(1,IBND) = ELED(IEDG)
         GBN(2,IBND) = NLED(IEDG)
         GBN(3,IBND) = ZLED(IEDG)
       ELSE
         INTN = INTN + 1
         NUME(IEDG) = INTN
       END IF
     END DO
     IF (IZ < NZ) THEN
       DO IX = 1, NE
         IEDG = IEDG + 1
         ICED(IEDG) = 3
         ELED(IEDG) = (ELOC(IX,IY,IZ) + ELOC(IX,IY,IZ+1)) / 2.
         NLED(IEDG) = (NLOC(IX,IY,IZ) + NLOC(IX,IY,IZ+1)) / 2.
         ZLED(IEDG) = (ZLOC(IX,IY,IZ) + ZLOC(IX,IY,IZ+1)) / 2.
         IF ((IX ==  1) .OR. (IX == NE)) THEN
           IBND = IBND + 1
           NUME(IEDG) = -IBND
           GBN(1,IBND) = ELED(IEDG)
           GBN(2,IBND) = NLED(IEDG)
           GBN(3,IBND) = ZLED(IEDG)
         ELSE
           INTN = INTN + 1
           NUME(IEDG) = INTN
         END IF
       END DO
     END IF
   END DO
   IF (IY < (NN-1))  THEN
     DO IZ = 1, NZ
       DO IX = 1, NE
         IEDG = IEDG + 1
         ICED(IEDG) = 2
         ELED(IEDG) = (ELOC(IX,IY,IZ) + ELOC(IX,IY+1,IZ)) / 2.
         NLED(IEDG) = (NLOC(IX,IY,IZ) + NLOC(IX,IY+1,IZ)) / 2.
         ZLED(IEDG) = (ZLOC(IX,IY,IZ) + ZLOC(IX,IY+1,IZ)) / 2.
         IF ((IX ==  1) .OR. (IX == NE) .OR. (IZ == 1) .OR. (IZ == NZ)) THEN
           IBND = IBND + 1
           NUME(IEDG) = -IBND
           GBN(1,IBND) = ELED(IEDG)
           GBN(2,IBND) = NLED(IEDG)
           GBN(3,IBND) = ZLED(IEDG)
         ELSE
           INTN = INTN + 1
           NUME(IEDG) = INTN
         END IF
       END DO
     END DO
   END IF
 END DO

 DO IZ = 1, NZ
   DO IX = 1, NE
     IEDG = IEDG + 1
     ICED(IEDG) = 2
     ELED(IEDG) = (ELOC(IX,NN-1,IZ) + ELOC(IX,NN,IZ)) / 2.
     NLED(IEDG) = (NLOC(IX,NN-1,IZ) + NLOC(IX,NN,IZ)) / 2.
     ZLED(IEDG) = (ZLOC(IX,NN-1,IZ) + ZLOC(IX,NN,IZ)) / 2.
     IF ((IX ==  1) .OR. (IX == NE) .OR. (IZ == 1) .OR. (IZ == NZ)) THEN
       IBND = IBND + 1
       NUME(IEDG) = -IBND
       GBN(1,IBND) = ELED(IEDG)
       GBN(2,IBND) = NLED(IEDG)
       GBN(3,IBND) = ZLED(IEDG)
     ELSE
       INTN = INTN + 1
       NUME(IEDG) = INTN
     END IF
   END DO
 END DO

 DO IZ = 1, NZ
   DO IX = 1, NE-1
     IEDG = IEDG + 1
     ICED(IEDG) = 1
     ELED(IEDG) = (ELOC(IX,NN,IZ) + ELOC(IX+1,NN,IZ)) / 2.
     NLED(IEDG) = (NLOC(IX,NN,IZ) + NLOC(IX+1,NN,IZ)) / 2.
     ZLED(IEDG) = (ZLOC(IX,NN,IZ) + ZLOC(IX+1,NN,IZ)) / 2.
     IBND = IBND + 1
     NUME(IEDG) = -IBND
     GBN(1,IBND) = ELED(IEDG)
     GBN(2,IBND) = NLED(IEDG)
     GBN(3,IBND) = ZLED(IEDG)
   END DO
   IF ( IZ < NZ ) THEN
     DO IX = 1, NE
       IEDG = IEDG + 1
       ICED(IEDG) = 3
       ELED(IEDG) = (ELOC(IX,NN,IZ) + ELOC(IX,NN,IZ+1)) / 2.
       NLED(IEDG) = (NLOC(IX,NN,IZ) + NLOC(IX,NN,IZ+1)) / 2.
       ZLED(IEDG) = (ZLOC(IX,NN,IZ) + ZLOC(IX,NN,IZ+1)) / 2.
       IBND = IBND + 1
       NUME(IEDG) = -IBND
       GBN(1,IBND) = ELED(IEDG)
       GBN(2,IBND) = NLED(IEDG)
       GBN(3,IBND) = ZLED(IEDG)
     END DO
   END IF
 END DO

 GBL = 1
 DO I = 1, IBND
   IF(GBN(3,I) <  0.) GBL(I) = 5
 END DO

 END SUBROUTINE PREP

!===========================================================================

 SUBROUTINE SET_KSQ (LYTH,NLITH,NPROP,FRQ,KSQ,SIGC)

!---------------------------------------------------------------------------
!
!*** Called by SAMAYA_3D
!
! Computes propagation constants.
!
!   LYTH - lithology arrays of the domain
!  NLITH - number of distinct lithologial units
!    FRQ - frequency
!    KSQ - element k^^2 array
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 REAL, PARAMETER :: EPS0 = 8.854215E-12, MU0=12.56637E-7, TWOPI=6.283185
 COMPLEX, PARAMETER :: ONE=(1.,0.)
 INTEGER JL,NLITH,NPROP
 REAL  LYTH(NLITH+1,NPROP),CALF,CTAU,CFREQ,FRQ
 COMPLEX CC,IW,Q,SIGC(NLITH+1),KSQ(NLITH+1)

 IW = CMPLX(0.,TWOPI*FRQ)
 KSQ = (0.,0.)
 DO JL = 1, NLITH+1
   CC = ONE
   IF (LYTH(JL,5) > 1.E-4) THEN
     CALF  = 1. - LYTH(JL,5)
     CTAU  = LYTH(JL,6)
     CFREQ = LYTH(JL,7)
     Q  = (IW * CTAU)**CFREQ
     CC = (ONE + Q) / (ONE + CALF*Q)
   END IF
   IF (LYTH(JL,1) > 0.) THEN
     SIGC(JL) = CC/LYTH(JL,1)
     IF (LYTH(JL,4) > 1.) SIGC(JL) = SIGC(JL) + IW*EPS0*LYTH(JL,4) ! 1./res (CC) + iw eps
     KSQ(JL) = IW*MU0*LYTH(JL,3)*SIGC(JL)                          ! iw*mu*sigC
   END IF
 END DO

 END SUBROUTINE SET_KSQ

!===========================================================================

 SUBROUTINE SET_SIGCV (LYTH,NLITH,NPROP,NPART,NE,NN,NZ,FRQ,SIGCV,LITH,XPART)

!---------------------------------------------------------------------------
!
!*** Called by SAMAYA_3D
!
! Set the vector of the complex conductivity for all elements.
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 REAL, PARAMETER :: EPS0 = 8.854215E-12, TWOPI=6.283185
 COMPLEX, PARAMETER :: ONE=(1.,0.)
 INTEGER NLITH,NPROP,NPART,NBN,NE,NN,NZ,I,J,K,KLITH,LITH(NE,NN,NZ)
 REAL  LYTH(NLITH+1,NPROP),CALF,CTAU,CFREQ,FRQ,XPART(NPART)
 COMPLEX CC,IW,Q,SIGCV(NPART)

 IW = CMPLX(0.,TWOPI*FRQ)
 SIGCV = (0.,0.)
 DO J = 1, NN-1
   DO K = 1, NZ-1
     DO I = 1, NE-1
       NBN = (J-1)*(NZ-1)*(NE-1) + (K-1)*(NE-1) + I
       KLITH = LITH(I,J,K)
       CC = ONE
       IF (LYTH(KLITH,5) > 1.E-4) THEN
         CALF  = 1. - LYTH(KLITH,5)
         CTAU  = LYTH(KLITH,6)
         CFREQ = LYTH(KLITH,7)
         Q  = (IW * CTAU)**CFREQ
         CC = (ONE + Q) / (ONE + CALF*Q)
       END IF
       IF (XPART(NBN) > 0.) THEN
         SIGCV(NBN) = CC / XPART(NBN)
         IF (LYTH(KLITH,4) > 1.) SIGCV(NBN) = SIGCV(NBN) + IW*EPS0*LYTH(KLITH,4) 
       END IF
     END DO
   END DO
 END DO

 END SUBROUTINE SET_SIGCV

!===========================================================================

 SUBROUTINE SET_TX_INC_POS (JCBN,TDFD,NSTAT,NCOMP,NRX,JR,CMP,SX,SY,SZ, & 
                            RX,RY,RZ,FANGLE,TXCLN1,SX1,SY1,SZ1,FANGLE1, &
                            TXANGLE,NTX)

!---------------------------------------------------------------------------
!
!*** Called by SAMAYA_3D
!
!  Set up the vectors of the Tx positions and angles for the primal and
!  adjoint sources.
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 LOGICAL JCBN
 REAL, PARAMETER :: PI = 3.14159265359
 INTEGER NRX,NTX,NSTAT,NCOMP,CMP,TDFD,JTX,JS,JC,JR
 REAL TXCLN1
 REAL, DIMENSION(NSTAT) :: SX,SY,SZ,FANGLE
 REAL, DIMENSION(NSTAT,NRX) :: RX,RY,RZ
 REAL, DIMENSION(NTX) :: SX1,SY1,SZ1,FANGLE1,TXANGLE

 ! Set up the vectors of the Tx positions for the primal and adjoint problems.

 IF (JCBN) THEN             ! Include adjoint Tx positions.
   DO JS = 1, NSTAT         ! Up to NSTAT are the primal Tx positions.
     SX1(JS) = SX(JS)       ! Primal Tx position in X.
     SY1(JS) = SY(JS)       ! Primal Tx position in Y.
     SZ1(JS) = SZ(JS)       ! Primal Tx position in Z.
   END DO
   DO JC = 1, NCOMP         ! After NSTAT, they are the adjoint Tx positions,
     DO JS = 1, NSTAT       ! which are the Rx positions from the primal problem.
       JTX = JS + JC*NSTAT    
       SX1(JTX) = RX(JS,JR)  ! Adjoint Tx position in X.
       SY1(JTX) = RY(JS,JR)  ! Adjoint Tx position in Y.
       SZ1(JTX) = RZ(JS,JR)  ! Adjoint Tx position in Z.
     END DO
   END DO
 ELSE                       ! Primal problem only; no adjoint fields.
   DO JS = 1, NSTAT
     SX1(JS) = SX(JS)       ! Primal Tx position in X.
     SY1(JS) = SY(JS)       ! Primal Tx position in Y.
     SZ1(JS) = SZ(JS)       ! Primal Tx position in Z.
   END DO
 END IF

 ! Set up the vector of the Tx pitch for the primal and adjoint problems.

 IF (JCBN) THEN                   ! Include adjoint Tx positions.
   DO JS = 1, NSTAT               ! Up to NSTAT are the primal Tx positions.
     TXANGLE(JS) = TXCLN1         ! Primal Tx pitch for TD system.
     FANGLE1(JS) = FANGLE(JS)
   END DO
   DO JC = 1, NCOMP               ! After NSTAT, they are the adjoint Tx positions,
     DO JS = 1, NSTAT             ! which are the Rx positions from the primal problem.
       JTX = JS + JC*NSTAT
       IF (TDFD < 2) THEN
         IF (CMP == 11) THEN                ! SINGLE COMPONENT INVERSION
           TXANGLE(JTX) = 90. * PI / 180.      ! Pitch = 90 degrees for in-line.
           FANGLE1(JTX) = FANGLE(JS)
         ELSE IF (CMP == 13) THEN 
           TXANGLE(JTX) = 0.                   ! Pitch =  0 degrees for vertical.
           FANGLE1(JTX) = FANGLE(JS)
         ELSE IF (CMP == 2) THEN            ! TWO COMPONENT INVERSION
           IF (JC == 1) THEN                ! Vertical first, then in-line.
             TXANGLE(JTX) = 0.                 ! Pitch =  0 degrees for vertical.
             FANGLE1(JTX) = FANGLE(JS)      
           ELSE IF (JC == 2) THEN           
             TXANGLE(JTX) = 90. * PI / 180.    ! Pitch = 90 degrees for in-line.
             FANGLE1(JTX) = FANGLE(JS)
           END IF
         ELSE IF (CMP == 3) THEN            ! THREE COMPONENT INVERSION
           IF (JC == 1) THEN                ! Vertical first, then in-line, then transverse.
             TXANGLE(JTX) = 0.                 ! Pitch =  0 degrees for vertical.
             FANGLE1(JTX) = FANGLE(JS)      
           ELSE IF (JC == 2) THEN           
             TXANGLE(JTX) = 90. * PI / 180.    ! Pitch = 90 degrees for in-line.
             FANGLE1(JTX) = FANGLE(JS)      
           ELSE IF (JC == 3) THEN           
             TXANGLE(JTX) = 90. * PI / 180.    ! Pitch = 90 degrees for transverse.
             FANGLE1(JTX) = FANGLE(JS) - PI/2. ! FANGLE1 = FANGLE - 90 degrees.
           END IF
         END IF
       ELSE IF (TDFD == 2) THEN
         TXANGLE(JTX) = TXCLN1       ! Angle for maximally coupled component = Angle of primal Tx.
         FANGLE1(JTX) = FANGLE(JS)
       END IF
     END DO
   END DO
 ELSE                             ! Primal problem only; no adjoint fields.
   DO JS = 1, NSTAT
     TXANGLE(JS) = TXCLN1      ! Primal Tx pitch for TD system.
     FANGLE1(JS) = FANGLE(JS)
   END DO
 END IF

 END SUBROUTINE SET_TX_INC_POS 

!===========================================================================

 SUBROUTINE EPEDGE (SX1,SY1,SZ1,NTX,TXCLN,FANGLE1,FRQ,NLYR,SIGL,KSQR,THK, &
                    ITX,NE,NN,NZ,ELOC,NLOC,ZLOC,F,RMU,NINEDG,INTN,ELED, & 
                    NLED,ZLED,NUME,ICED,NEDGE)

!---------------------------------------------------------------------------
!
!*** Called by SAMAYA_3D
!*** Calls BASEP, EDGELM, SET_RHO
!
!  Computes the primary electric fields.
!
!  SX,SY,SZ - arrays of X-,Y- and Z-transmitter coordinates
!     NSTAT - total number of transmitters
!     TXCLN - receiver orientation (nose up = positive)  level flight = 0
!    FANGLE - flight direction in radians, clockwise from north
!       FRQ - frequency
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 REAL   , ALLOCATABLE :: STDF(:,:,:),ZUNIQ(:)
 COMPLEX, ALLOCATABLE :: EE(:,:)
 REAL, PARAMETER :: MU0 = 12.56637E-7, TWOPI = 6.283185
 INTEGER IEDG,ITX,NTX,NBN,NE,NN,NZ,I,J,IX,IY,IZ,N,M,KI,KM,NLYR,MR,IBASE, &
         IZMAX,IFOUND,NINEDG,INTN,NUME(NINEDG),ICED(NINEDG),IT(12,12), &
         NEDGE(12,(NE-1)*(NN-1)*(NZ-1))
 REAL    ELI(9,12,12),ELR(9,12,12),THK(NLYR),SX1(NTX),SY1(NTX),SZ1(NTX),XR,YR, &
         TXCLN(NTX),BPR(100),FANGLE1(NTX),R2,R,FRQ,NP,EP,ZP,RMU(NLYR),ZLOG, &
         SNTX(NTX),CSTX(NTX),CSF,SNF,ELED(NINEDG),NLED(NINEDG),ZLED(NINEDG),EC(12), &
         NC(12),ZC(12),XBAR,YBAR,XB2,ELOC(NE,NN,NZ),NLOC(NE,NN,NZ),ZLOC(NE,NN,NZ), &
         DMX,DMY,DMZ
 COMPLEX SIGL(2),EH(3),IWMU,H(9),SIGB,F(INTN,NTX),KSQR(2),EEX,EEY
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

 IWMU = (0.,1.)*TWOPI*FRQ*MU0

 ALLOCATE (ZUNIQ(1000))
 IBASE = 0 ; ZUNIQ = 0.
 DO IEDG = 1, NINEDG
   IF (NUME(IEDG) > 0) THEN
     ZP = ZLED(IEDG)
     ZLOG = ABS(ZP)
     IFOUND = 0              ! Check ZP in the index.
     IF (IBASE > 0)   THEN
       DO I = IBASE, 1, -1
         IF (ABS (ZLOG-ZUNIQ(I)) < 1.) THEN
           IFOUND = 1 ; EXIT
         END IF
       END DO
     END IF
     IF (IFOUND == 0) THEN   ! New entry.
       IBASE = IBASE + 1
       IZMAX = IBASE
       ZUNIQ(IBASE) = ZLOG
     END IF
   END IF
 END DO

 DEALLOCATE (ZUNIQ)

 IZMAX = 2*IZMAX

 DO ITX = 1, NTX
   SNTX(ITX) = 0.
   CSTX(ITX) = 1.
   IF ( ABS(TXCLN(ITX)) > 1.E-4 ) THEN
     SNTX(ITX) = SIN(TXCLN(ITX))
     CSTX(ITX) = COS(TXCLN(ITX))
   END IF
 END DO

 CALL SET_RHO(MR,BPR)

 ALLOCATE (STDF(24,MR,IZMAX),ZUNIQ(IZMAX),EE(INTN,NTX))

 EE = (0.,0.)

 DO ITX = 1, NTX
   SNF = 0.
   CSF = 1.
   IF (ABS (FANGLE1(ITX)) > 1.E-4) THEN
     CSF = COS (FANGLE1(ITX))
     SNF = SIN (FANGLE1(ITX))
   END IF
   STDF  = 0. ; IBASE = 0 ; ZUNIQ = 0.
   DO IEDG = 1, NINEDG
     IF (NUME(IEDG) > 0) THEN
       NP = NLED(IEDG) ; EP = ELED(IEDG) ; ZP = ZLED(IEDG)
       R2 = MAX((EP-SY1(ITX))**2 + (NP-SX1(ITX))**2,.1) ; R = SQRT(R2) 
       XR = (NP - SX1(ITX)) / R
       YR = (EP - SY1(ITX)) / R
       XBAR = XR*CSF + YR*SNF
       YBAR = YR*CSF - XR*SNF
       XB2  = XBAR**2
       CALL BASEP (STDF,BPR,R,MR,ZUNIQ,IZMAX,IBASE,ZP,SZ1(ITX),EH, &
                   NLYR,KSQR,THK(1),RMU)
       EEX = SNTX(ITX) * ((1. - 2.*XB2)*EH(3) + XB2*EH(2)) - CSTX(ITX)*XBAR*EH(1) 
       EEY = SNTX(ITX) * XBAR*YBAR * (2.*EH(3) - EH(2))    + CSTX(ITX)*YBAR*EH(1) 
       IF (ABS(ICED(IEDG)) == 1) EE(NUME(IEDG),ITX) = EEX*CSF + EEY*SNF
       IF (ABS(ICED(IEDG)) == 2) EE(NUME(IEDG),ITX) = EEY*CSF - EEX*SNF
       IF (ABS(ICED(IEDG)) == 3) EE(NUME(IEDG),ITX) = CSTX(ITX)*EH(2) + SNTX(ITX)*XBAR*EH(1) 
     END IF
   END DO
 END DO

 DEALLOCATE (STDF,ZUNIQ)

 F = (0.,0.)

 DO IY = 1, NN-1
   DO IZ = 1, NZ-1
     DO IX = 1, NE-1
       NBN = (IY-1)*(NE-1)*(NZ-1) + (IZ-1)*(NE-1) + IX
       IF (ZLOC(IX,IY,IZ) <  THK(1)) SIGB = SIGL(1)
       IF (ZLOC(IX,IY,IZ) >= THK(1)) SIGB = SIGL(NLYR)
       DMX = .5 * ABS (ELOC(IX+1,IY,IZ) - ELOC(IX,IY,IZ))
       DMY = .5 * ABS (NLOC(IX,IY+1,IZ) - NLOC(IX,IY,IZ))
       DMZ = .5 * ABS (ZLOC(IX,IY,IZ+1) - ZLOC(IX,IY,IZ))
       DO I = 1, 12
         EC(I)  = ELED(NEDGE(I,NBN))
         NC(I)  = NLED(NEDGE(I,NBN))
         ZC(I)  = ZLED(NEDGE(I,NBN))
       END DO
       CALL EDGELM (DMX,DMY,DMZ,ELR,ELI,EC,NC,ZC)
       DO N = 1, 12
         KI = NUME(NEDGE(N,NBN))
         IF (KI > 0) THEN
           DO M = 1, 12
             KM = NUME(NEDGE(M,NBN))
             IF (KM > 0)  THEN
               DO J = 1, 9
                 H(J) = CMPLX(ELR(J,N,M),0.) + IWMU*SIGB*CMPLX(ELI(J,N,M),0.)
               END DO
               F(KI,1:NTX) = F(KI,1:NTX) + H(IT(N,M))*EE(KM,1:NTX)
             END IF
           END DO
         END IF
       END DO
     END DO
   END DO
 END DO

 DEALLOCATE (EE)

 END SUBROUTINE EPEDGE

!===========================================================================

 SUBROUTINE BMATE (NE,NN,NZ,NINEDG,NEDGE,ELED,NLED,ZLED,FRQ,SMB,ELOC,NLOC, &
                   ZLOC,NUME,INTN,IBND,SIGL,NLYR,KSQR,THK,RMU,GBL,GBN,NPART, &
                   SIGCV,NEL,NER,NNL,NNR,NZT,NZB)

!---------------------------------------------------------------------------
!
!*** Called by SAMAYA_3D
!*** Calls BASE5, EDGELM, SET_RHO
!
! Set up the global matrix.
!
!   NE - number of target nodes in X-direction
!   NN - number of target nodes in Y-direction
!   NZ - number of target nodes in Z-direction
! LITH - lithology-index arrays of the domain
!   IA - Indexing pointer
!  FRQ - current frequency
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 REAL, PARAMETER :: FOURPI=12.56637
 COMPLEX, PARAMETER :: ONE=(1.,0.)
 INTEGER NE,NN,NZ,N,M,I,IX,IY,IZ,IZMAX,IFOUND,KI,KM,NBN,LAY,IT(12,12), &
         IBASE(5),MR,L1,L2,L3,J,K,L,IBND,IX1,IY1,IZ1,INTN,NBN1,NINEDG, &
         NUME(NINEDG),GBL(NINEDG),NEDGE(12,(NE-1)*(NN-1)*(NZ-1)),NLYR,NPART,  &
         NEL,NER,NNL,NNR,NZT,NZB
 REAL ELED(NINEDG),NLED(NINEDG),ZLED(NINEDG),XC0,YC0,ZC0,ZPC,ELI(9,12,12), &
      FRQ,EC(12),NC(12),ZC(12),ELR(9,12,12),ELOC(NE,NN,NZ),NLOC(NE,NN,NZ), &
      ZLOC(NE,NN,NZ),BPR(100),ZR,DMX,DMY,DMZ,DMX1,DMY1,DMZ1,GBN(3,NINEDG), &
      AJB,XM(12),YM(12),ZM(12),S2(2),W2(2),R,R2,WG,X,Y,Z,ZP,XR,YR,SH,ZLOG, &
      RR2,RR,XR2,YR2
 COMPLEX STC,SMB(INTN,INTN),DSIG,SIGB,SIGCV(NPART),P1,P2,P3,P4,GC(9,12),C(6), &
         IWMU,FF(9),H(9),G1(9),G2(9),KSQR(2),SIGL(2)
 REAL, DIMENSION (NLYR) :: RMU,THK
 REAL   , ALLOCATABLE :: STDF(:,:,:),ZUNIQ(:,:),ZUNIQ1(:) 
 COMPLEX, ALLOCATABLE :: GS(:,:,:)
 DATA XM/ 0.,-1., 1., 0.,-1., 1.,-1., 1., 0.,-1., 1., 0./, &
      YM/-1.,-1.,-1.,-1., 0., 0., 0., 0., 1., 1., 1., 1./, &
      ZM/-1., 0., 0., 1.,-1.,-1., 1., 1.,-1., 0., 0., 1./
      DATA S2/-0.5773502691,0.5773502691/
      DATA W2/ 1.,1./
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

 IWMU = CMPLX(0.,FRQ*7.895683E-6)

 ALLOCATE (ZUNIQ1(1000)) ; ZUNIQ1 = 0. ; IBASE = 1 

 DO KM = 1, IBND
   LAY = GBL(KM)
   DO IY1 = NNL+1, NN-NNR-1
     DO IZ1 = NZT+1, NZ-NZB-1
       DO IX1 = NEL+1, NE-NER-1
          DMZ1 = .5*ABS(ZLOC(IX1,IY1,IZ1+1) - ZLOC(IX1,IY1,IZ1))
          DO L1 = 1, 2
            DO L2 = 1, 2
              DO L3 = 1, 2
                ZP =  ZLOC(IX1,IY1,IZ1) + DMZ1 - S2(L3)*DMZ1 
                IF (LAY == 1) ZLOG = ABS(GBN(3,KM)+ZP)
                IF (LAY == 5) ZLOG = ABS(GBN(3,KM)-ZP)
                IFOUND = 0                              ! Check ZP in the index.
                DO I = 1, IBASE(1)
                  IF (ABS (ZLOG-ZUNIQ1(I)) < 1.) THEN
                    IFOUND = 1 ; EXIT
                  END IF
                END DO
                IF (IFOUND == 0) THEN                    ! New entry.
                  IBASE(1) = IBASE(1) + 1
                  IZMAX  = IBASE(1)
                  ZUNIQ1(IBASE(1)) = ZLOG
                END IF
              END DO
            END DO
          END DO
       END DO
     END DO
   END DO
 END DO

 DEALLOCATE (ZUNIQ1)

 IZMAX = 2*IZMAX

 CALL SET_RHO(MR,BPR)

 ALLOCATE (GS(9,INTN,IBND),STDF(40,MR,IZMAX),ZUNIQ(5,IZMAX))
 GS = (0.,0.) ; STDF  = 0. ; IBASE = 1 ; ZUNIQ = 0. ; 

 DO KM = 1, IBND
   IF (GBN(3,KM) <  0.    ) SIGB = CMPLX(1.E-8,0.)
   IF (GBN(3,KM) <  THK(1)) SIGB = SIGL(1)
   IF (GBN(3,KM) >= THK(1)) SIGB = SIGL(NLYR)
   LAY = GBL(KM)
   DO IY1 = NNL+1, NN-NNR-1
     DO IZ1 = NZT+1, NZ-NZB-1
       DO IX1 = NEL+1, NE-NER-1
         NBN1 = (IY1-1)*(NE-1)*(NZ-1) + (IZ1-1)*(NE-1) + IX1
         STC = SIGCV(NBN1)
         DMX1 = .5 * ABS (ELOC(IX1+1,IY1  ,IZ1)   - ELOC(IX1,IY1,IZ1))
         DMY1 = .5 * ABS (NLOC(IX1  ,IY1+1,IZ1)   - NLOC(IX1,IY1,IZ1))
         DMZ1 = .5 * ABS (ZLOC(IX1  ,IY1  ,IZ1+1) - ZLOC(IX1,IY1,IZ1))
         AJB = DMX1 * DMY1 * DMZ1
         IF (ZLOC(IX1,IY1,IZ1) <  0.) DSIG = STC - SIGL(1)
         IF (ZLOC(IX1,IY1,IZ1) >= 0.) DSIG = STC - SIGL(NLYR)
         XC0  = GBN(1,KM) - (ELOC(IX1,IY1,IZ1) + DMX1)
         YC0  = GBN(2,KM) - (NLOC(IX1,IY1,IZ1) + DMY1)
         ZC0  = GBN(3,KM) - (ZLOC(IX1,IY1,IZ1) + DMZ1)
         ZPC  =              ZLOC(IX1,IY1,IZ1) + DMZ1
         GC = (0.,0.)
         DO L1 = 1, 2
           DO L2 = 1, 2
             DO L3 = 1, 2
               X  =   XC0 - S2(L1)*DMX1
               Y  =  (YC0 - S2(L2)*DMY1)
               Z  =   ZC0 - S2(L3)*DMZ1
               ZP =   ZPC - S2(L3)*DMZ1 
               ZR =   GBN(3,KM)
               WG = W2(L1) * W2(L2) * W2(L3)
               G1 = (0.,0.)
               IF (LAY == 1 .OR. LAY == 4) THEN
                 RR2= X*X+Y*Y+Z*Z ; RR = SQRT(RR2)
                 P1 = (0.,1.)*RR*SQRT(-KSQR(NLYR))
                 P2 = EXP(-P1) / (RR*RR2*RR2) / FOURPI
                 P3 = P2/SIGB * ( 3.*(ONE+P1) +     RR2*KSQR(NLYR))
                 P4 = P2/SIGB * (RR2*(ONE+P1) + RR2*RR2*KSQR(NLYR)) 
                 G1 = (0.,0.)
                 G1(1) = X*X*P3 - P4 
                 G1(2) = X*Y*P3      
                 G1(3) = X*Z*P3      
                 G1(4) = X*Y*P3
                 G1(5) = Y*Y*P3 - P4 
                 G1(6) = Y*Z*P3      
                 G1(7) = Z*X*P3      
                 G1(8) = Z*Y*P3      
                 G1(9) = Z*Z*P3 - P4 
               END IF
               R2 = MAX(X*X+Y*Y,.1) ; R = SQRT(R2)
               XR = X/R ; YR = Y/R ; XR2 = XR*XR ; YR2 = YR*YR
               G2 = (0.,0.)
               CALL BASE5 (STDF,BPR,R,MR,ZUNIQ,IZMAX,IBASE,ZP,ZR,C, &
                           NLYR,LAY,SIGL,KSQR,THK(1),RMU)
               G2(1) =  (2*XR2-1.)*C(4)/R + C(1) - XR2*C(2)
               G2(2) =  XR*YR*(2*C(4)/R - (C(2)+C(1)))
               G2(3) = -XR*C(5)                            
               G2(4) =  XR*YR*(2*C(4)/R - (C(2)+C(1)))
               G2(5) =  (2*YR2-1.)*C(4)/R + C(1) - YR2*C(2)
               G2(6) = -YR*C(5)                            
               G2(7) =  XR*C(5)                            
               G2(8) =  YR*C(5)                            
               G2(9) = -C(3)
               DO J = 1, 12
                 SH = (1.+S2(L1)*XM(J))*(1.+S2(L2)*YM(J))*(1.+S2(L3)*ZM(J))/8.
                 DO I = 1, 9
                   GC(I,J) = GC(I,J) + WG * SH * (G1(I) - G2(I))
                 END DO
               END DO
             END DO
           END DO
         END DO
         DO J = 1, 12
           L = NUME(NEDGE(J,NBN1))
           DO I = 1, 9
             GS(I,L,KM) = GS(I,L,KM) + AJB * DSIG * GC(I,J)
           END DO
         END DO
       END DO
     END DO
   END DO
 END DO

 DEALLOCATE (STDF,ZUNIQ)

 ! Calculate the global system-matrix.

 SMB  = (0.,0.)
 DO IY = 1, NN-1
   DO IZ = 1, NZ-1
     DO IX = 1, NE-1
       NBN = (IY-1)*(NE-1)*(NZ-1) + (IZ-1)*(NE-1) + IX
       IF (ZLOC(IX,IY,IZ) <  THK(1)) SIGB = SIGL(1)
       IF (ZLOC(IX,IY,IZ) >= THK(1)) SIGB = SIGL(NLYR)
       STC = 2.*SIGCV(NBN)
       DMX = .5 * ABS (ELOC(IX+1,IY,IZ) - ELOC(IX,IY,IZ))
       DMY = .5 * ABS (NLOC(IX,IY+1,IZ) - NLOC(IX,IY,IZ))
       DMZ = .5 * ABS (ZLOC(IX,IY,IZ+1) - ZLOC(IX,IY,IZ))
       DO I = 1, 12
         EC(I)  = ELED(NEDGE(I,NBN))
         NC(I)  = NLED(NEDGE(I,NBN))
         ZC(I)  = ZLED(NEDGE(I,NBN))
       END DO
       CALL EDGELM (DMX,DMY,DMZ,ELR,ELI,EC,NC,ZC)
       DO N = 1, 12
         KI = NUME(NEDGE(N,NBN))
         IF (KI > 0) THEN
           DO M = 1, 12
             KM = NUME(NEDGE(M,NBN))
             H(1:9) = IWMU*SIGB*ELI(1:9,N,M)
             IF (KM < 0) THEN
               KM = -KM
               DO K = 1, INTN
                 DO J = 1, 3
                   FF(J)   = H(1)*GS(J,K,KM) + H(2)*GS(J+3,K,KM) + H(3)*GS(J+6,K,KM)
                   FF(J+3) = H(4)*GS(J,K,KM) + H(5)*GS(J+3,K,KM) + H(6)*GS(J+6,K,KM)
                   FF(J+6) = H(7)*GS(J,K,KM) + H(8)*GS(J+3,K,KM) + H(9)*GS(J+6,K,KM)
                 END DO
                 SMB(K,KI) = SMB(K,KI) + FF(IT(N,M)) 
               END DO
             ELSE 
               SMB(KM,KI) = SMB(KM,KI) + (CMPLX(ELR(IT(N,M),N,M),0.) + IWMU*STC*CMPLX(ELI(IT(N,M),N,M),0.))
             END IF
           END DO
         END IF
       END DO
     END DO
   END DO
 END DO

 DEALLOCATE (GS)

 END SUBROUTINE BMATE

!===========================================================================

 SUBROUTINE EDGELM (A,B,C,ELR,ELI,EC,NC,ZC)

!---------------------------------------------------------------------------
!
!*** Called by BMATE
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
 REAL A,B,C,WEIGHT,OP,GG(5),WG(5),ELR(9,12,12),R,S,T,VOL,XM(12),     &
      YM(12),ZM(12),AA(3,6),DXDR,DYDR,DZDR,DXDS,DYDS,DZDS,DXDT,DYDT, &
      DZDT,EC(12),NC(12),ZC(12),DSRDR(12),DSRDS(12),DSRDT(12),       &
      DSSDR(12),DSSDS(12),DSSDT(12),DSTDR(12),DSTDS(12),DSTDT(12),   &
      DSXDY(12),DSXDZ(12),DSYDX(12),DSYDZ(12),DSZDX(12),DSZDY(12),   &
      SR(12),SS(12),ST(12),ELI(9,12,12)
 DATA XM/ 0.,-1., 1., 0.,-1., 1.,-1., 1., 0.,-1., 1., 0./,              &
      YM/-1.,-1.,-1.,-1., 0., 0., 0., 0., 1., 1., 1., 1./,              &
      ZM/-1., 0., 0., 1.,-1.,-1., 1., 1.,-1., 0., 0., 1./
 DATA KX/1,4,9,12/, KY/5,6,7,8/, KZ/2,3,10,11/
 DATA GG(1),GG(2),GG(3)/-0.774597,0.,+0.774597/
 DATA WG(1),WG(2),WG(3)/0.5555556,0.8888889,0.5555556/

 ELI = 0.
 ELR = 0.
 VOL= 8.*A*B*C

 DO IG = 1, 3
   DO JG = 1, 3
     DO KG = 1, 3

       R = GG(IG) ; S = GG(JG) ; T = GG(KG)
       WEIGHT = WG(IG) * WG(JG) * WG(KG) * VOL
       SR = 0. ; SS = 0. ; ST = 0.
       DSRDR = 0. ; DSRDS = 0. ; DSRDT = 0.
       DSSDR = 0. ; DSSDS = 0. ; DSSDT = 0.
       DSTDR = 0. ; DSTDS = 0. ; DSTDT = 0.
       DSXDY = 0. ; DSYDX = 0. ; DSZDX = 0.
       DSXDZ = 0. ; DSYDZ = 0. ; DSZDY = 0.

       DO I = 1, 4
         SR(KX(I)) = (1.+S*YM(KX(I))) * (1.+T*ZM(KX(I))) / 8.
         SS(KY(I)) = (1.+R*XM(KY(I))) * (1.+T*ZM(KY(I))) / 8.
         ST(KZ(I)) = (1.+R*XM(KZ(I))) * (1.+S*YM(KZ(I))) / 8.
       END DO

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
           ELR(1,KX(I),KX(J)) = ELR(1,KX(I),KX(J)) + WEIGHT*(DSXDY(KX(J))*DSXDY(KX(I)) &
                                                       + DSXDZ(KX(I))*DSXDZ(KX(J)))
           ELR(5,KY(I),KY(J)) = ELR(5,KY(I),KY(J)) + WEIGHT*(DSYDZ(KY(J))*DSYDZ(KY(I)) &
                                                       + DSYDX(KY(I))*DSYDX(KY(J)))
           ELR(9,KZ(I),KZ(J)) = ELR(9,KZ(I),KZ(J)) + WEIGHT*(DSZDY(KZ(J))*DSZDY(KZ(I)) &
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

       DO I = 1, 4
         DO J = 1, 4
           ELI(1,KX(I),KX(J)) = ELI(1,KX(I),KX(J)) + WEIGHT*(SR(KX(J))*SR(KX(I)))
           ELI(5,KY(I),KY(J)) = ELI(5,KY(I),KY(J)) + WEIGHT*(SS(KY(J))*SS(KY(I)))
           ELI(9,KZ(I),KZ(J)) = ELI(9,KZ(I),KZ(J)) + WEIGHT*(ST(KZ(J))*ST(KZ(I)))
         END DO
       END DO

     END DO
   END DO
 END DO

 END SUBROUTINE EDGELM

!===========================================================================

 SUBROUTINE MATINV (A,B,N,NTX)

!---------------------------------------------------------------------------
!
!*** Called by: SAMAYA_3D
!
! Solves the matrix equation using Gauss's elimination process.
!
! INPUT
!
! A - left-hand side matrix
! B - right-hand side matrix, which will be overwritten by the results.
! N - dimension of the matrix
! NSTAT - total number of the transmitters
!
! OUTPUT
!
! B - electric fields at the target.
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER I,J,K,IJ,IR,JMIN,N,NTX
 COMPLEX A(N,N),B(N,NTX),PROD,ADIAG

 DO I = 1, N
   IF (I > 1)  THEN
     DO IR = 2, N
       PROD = (0.,0.)
       JMIN = MIN(I,IR)
       DO J = 1, JMIN - 1
         PROD = PROD + A(IR,J) * A(J,I)
       END DO
       A(IR,I) = A(IR,I) - PROD 
     END DO
     DO K = 1, NTX
       PROD = (0.,0.)
       DO J = 1, I-1
         PROD = PROD + B(J,K) * A(J,I)
       END DO
       B(I,K) = B(I,K) - PROD 
     END DO
   END IF
   ADIAG = 1./ A(I,I)
   DO IJ = I+1, N
     A(IJ,I) = A(IJ,I) * ADIAG
   END DO
   B(I,1:NTX) = B(I,1:NTX) * ADIAG
 END DO

 DO I = N-1, 1, -1     ! Back substitution.
   DO J = N, I+1, -1
     B(I,1:NTX) = B(I,1:NTX) -  B(J,1:NTX) * A(J,I)
   END DO
 END DO

 END SUBROUTINE MATINV

!===========================================================================

 SUBROUTINE HFIELD (ELOC,NLOC,ZLOC,NE,NN,NZ,LITH,NLITH,FRQ,NSTAT,NTX, & 
                    RX,RY,RZ,NRX,ND,F,DIME,DIMN,DIMZ,NELEM,TXCLN1,FANGLE, &
                    SX,SY,SZ,NLYR,RMU,THK,KSQR,INTN,NINEDG,NUME,NEDGE, &
                    NPART,SIGCV,LYTH,NPROP,NEL,NER,NNL,NNR,NZT,NZB)

!---------------------------------------------------------------------------
!
!*** Called by SAMAYA_3D
!*** Calls BASEH, BGTHNK, SET_RHO
!
! Compute the magnetic fields at the recievers.
!
!      ELOC - X - coordinates of the finite element nodes
!      NLOC - Y - coordinates of the finite element nodes
!      ZLOC - Z - coordinates of the finite element nodes
!        NE - number of target nodes in X-direction
!        NN - number of target nodes in Y-direction
!        NZ - number of target nodes in Z-direction
!      LITH - lithology-index arrays of the domain
!       KSQ - element k^^2 array
!     NLITH - number of distinct lithologial units
!       FRQ - frequency
!     NSTAT - total number of transmitters
!  RX,RY,RZ - receiver coord.
!       NRX - total number of receivers
!        ND - save-file unit number
!         F - array of the secondary electric field at the target
!      NFRQ - number of frequencies
! DIME,DIMN,-
!      DIMZ - dimension of the cells
!       ITX - Index of transmitter loop
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 REAL, PARAMETER :: FOURPI = 12.56637, PI = 3.14159265359, MU0 = 12.56637E-7
 INTEGER L1,L2,L3,I1,I2,I3,I,J,K,IR,NBN,NSTAT,NTX,NE,NN,NZ,NRX,NELEM,NLYR, &
         ITX,IJ,LITH(NE,NN,NZ),NLITH,ND,MR,IBASE,IZMAX,IFOUND,INTN,NINEDG, &
         CX(4),CY(4),CZ(4),NUME(NINEDG),NEDGE(12,(NE-1)*(NN-1)*(NZ-1)), &
         HNOD(8),NPART,NPROP,NEL,NER,NNL,NNR,NZT,NZB
 COMPLEX EN(3),F(INTN,NTX),HX,HY,HZ,STC,C(3),G(3,3),HX1,HY1,EPX,EPY, &
         SIGCV(NPART),IW,KEL
 REAL ELOC(NE,NN,NZ),NLOC(NE,NN,NZ),ZLOC(NE,NN,NZ),S(5),WG(5),XM(12),YM(12), &
      ZM(12),FCT,X,Y,VOL,DIME(NELEM),DIMN(NELEM),DIMZ(NELEM),FRQ,S1,S2,S3, &
      W1,W2,W3,FAC,ZP,ZR,TXCLN1,SNTX,CSTX,ZRFD,ZLOG,R2,R,BPR(100),CSF,SNF, &
      XRX,YRX,SHP1,SHP2,SHP3,LYTH(NLITH+1,NPROP)
 REAL, DIMENSION (NSTAT) :: SX,SY,SZ,FANGLE
 REAL, DIMENSION (NSTAT,NRX) :: RX,RY,RZ
 REAL, DIMENSION(NLYR) :: THK,RMU
 COMPLEX, DIMENSION (NSTAT,NRX) :: HXP,HYP,HZP
 COMPLEX, DIMENSION (2) :: KSQR
 REAL, ALLOCATABLE :: STDF(:,:,:),ZUNIQ(:)
 DATA XM/ 0.,-1., 1., 0.,-1., 1.,-1., 1., 0.,-1., 1., 0./, &
      YM/-1.,-1.,-1.,-1., 0., 0., 0., 0., 1., 1., 1., 1./, &
      ZM/-1., 0., 0., 1.,-1.,-1., 1., 1.,-1., 0., 0., 1./
 DATA S(1),S(2),S(3)/-0.774597,0.,+0.774597/
 DATA WG(1),WG(2),WG(3)/0.5555556,0.8888889,0.5555556/
 DATA CX/1,4,9,12/,CY/5,6,7,8/,CZ/2,3,10,11/

 FAC = 400. * PI ! 1.e9 * mu0
 IW = (0.,1.) * 2. * PI * FRQ

 HXP = (0.,0.) ; HYP = (0.,0.) ; HZP = (0.,0.)

 SNTX = SIN(TXCLN1) ; CSTX = COS(TXCLN1)

 DO ITX = 1, NSTAT
   DO IR = 1, NRX      
     CSF = COS (FANGLE(ITX))
     SNF = SIN (FANGLE(ITX))
     XRX = (SX(ITX)-RX(ITX,IR))*CSF + (SY(ITX)-RY(ITX,IR))*SNF
     YRX = (SX(ITX)-RX(ITX,IR))*SNF - (SY(ITX)-RY(ITX,IR))*CSF
     R2 = MAX((XRX**2+YRX**2),.1) ; R = SQRT(R2) 
     X   = -XRX / R
     Y   =  YRX / R
     ZRFD= RZ(ITX,IR) + SZ(ITX)
     CALL BGTHNK (R,NLYR,RMU,KSQR,THK(1),ZRFD,C)
     EPX  = (SNTX*X*Y*(C(1) - 2.*C(2)/R)          + CSTX*Y*C(3))
     EPY  = (SNTX*((1.-2.*X*X)*C(2)/R + X*X*C(1)) - CSTX*X*C(3))
     HXP(ITX,IR) = EPX*CSF + EPY*SNF
     HYP(ITX,IR) = EPY*CSF - EPX*SNF
     HZP(ITX,IR) = (CSTX*C(1) + SNTX*X*C(3))
   END DO
 END DO

 ALLOCATE (ZUNIQ(10000)) 

 DO ITX = 1, NSTAT
   IBASE  = 1 ; ZUNIQ  = 0.
   DO IR = 1, 1      
     DO J = NNL+1, NN-NNR-1
       DO K = NZT+1, NZ-NZB-1
         DO I = NEL+1, NE-NER-1
           NBN = (J-1)*(NE-1)*(NZ-1) + (K-1)*(NE-1) + I
           DO L1 = 1, 3
             DO L2 = 1, 3
               DO L3 = 1, 3
                 ZP =   (ZLOC(I,J,K)+(1.-S(L3))*DIMZ(NBN))
                 ZLOG = ABS(ZP+RZ(ITX,IR))
                 IFOUND = 0                              ! Check ZP in the index.
                 DO IJ = 1, IBASE
                   IF (ABS (ZLOG-ZUNIQ(IJ)) < 1.) THEN
                     IFOUND = 1 ; EXIT
                   END IF
                 END DO
                 IF (IFOUND == 0) THEN                   ! New entry.
                   IBASE = IBASE + 1
                   IZMAX = IBASE
                   ZUNIQ(IBASE) = ZLOG
                 END IF
               END DO
             END DO
           END DO
         END DO
       END DO
     END DO
   END DO
 END DO

 DEALLOCATE (ZUNIQ)

 IZMAX = 2*IZMAX

 CALL SET_RHO(MR,BPR)

 ALLOCATE (STDF(24,MR,IZMAX),ZUNIQ(IZMAX))

 DO ITX = 1, NSTAT
   STDF = 0. ; IBASE = 1 ; ZUNIQ = 0.
   DO IR = 1, 1      
     HX = (0.,0.) ; HY = (0.,0.) ; HZ = (0.,0.)
     DO J = NNL+1, NN-NNR-1
       DO K = NZT+1, NZ-NZB-1
         DO I = NEL+1, NE-NER-1

           NBN = (J-1)*(NE-1)*(NZ-1) + (K-1)*(NE-1) + I
           KEL = IW*MU0*LYTH(LITH(I,J,K),3)*SIGCV(NBN)
           STC = KEL - KSQR(2)
           VOL = DIMN(NBN) * DIME(NBN) * DIMZ(NBN)

           HNOD(1) = (J-1)*NE*NZ + (K-1)*NE  + I
           HNOD(2) = HNOD(1) + 1
           HNOD(3) = HNOD(1) + NE
           HNOD(4) = HNOD(3) + 1
           HNOD(5) = HNOD(1) + NE*NZ
           HNOD(6) = HNOD(5) + 1
           HNOD(7) = HNOD(5) + NE
           HNOD(8) = HNOD(7) + 1

           DO L1 = 1, 3
             DO L2 = 1, 3
               DO L3 = 1, 3

                 S1 = S(L1)  ; S2 = S(L2)  ; S3 = S(L3)
                 W1 = WG(L1) ; W2 = WG(L2) ; W3 = WG(L3)

                 X  =  (RX(ITX,IR) - (NLOC(I,J,K)+(1.-S1)*DIMN(NBN)))
                 Y  =  (RY(ITX,IR) - (ELOC(I,J,K)+(1.-S2)*DIME(NBN)))
                 ZR =   RZ(ITX,IR)
                 ZP =                (ZLOC(I,J,K)+(1.-S3)*DIMZ(NBN))
                 FCT = W1*W2*W3*VOL
                 R2  = MAX(X*X+Y*Y,.1) ; R = SQRT(R2)

                 CALL BASEH (STDF,BPR,R,MR,ZUNIQ,IZMAX,IBASE,ZP,ZR,C,NLYR,KSQR,THK(1),RMU)

                 G(1,1) =  X*Y/R2*(2*C(2)/R - C(1))
                 G(1,2) =  (2.*X*X/R2-1.)*C(2)/R - X*X/R2*C(1) + C(1) 
                 G(1,3) =  (0.,0.)
                 G(2,1) =  (1.-2.*Y*Y/R2)*C(2)/R + Y*Y/R2*C(1) - C(1) 
                 G(2,2) = -G(1,1)
                 G(2,3) =  (0.,0.)
                 G(3,1) = -X/R*C(3)
                 G(3,2) =  Y/R*C(3)
                 G(3,3) =  (0.,0.)

                 EN = (0.,0.)
                 DO IJ = 1, 4
                    I1 = CX(IJ) ; I2 = CY(IJ) ; I3 = CZ(IJ)
                    SHP1  = (1.+S1*XM(I1))*(1.+S2*YM(I1))*(1.+S3*ZM(I1)) / 8
                    SHP2  = (1.+S1*XM(I2))*(1.+S2*YM(I2))*(1.+S3*ZM(I2)) / 8.
                    SHP3  = (1.+S1*XM(I3))*(1.+S2*YM(I3))*(1.+S3*ZM(I3)) / 8.
                    EN(1) = EN(1) + SHP1 * F(NUME(NEDGE(I1,NBN)),ITX)
                    EN(2) = EN(2) + SHP2 * F(NUME(NEDGE(I2,NBN)),ITX)
                    EN(3) = EN(3) + SHP3 * F(NUME(NEDGE(I3,NBN)),ITX)
                 END DO

                 HX = HX + FCT*STC*(G(1,1)*EN(1)+G(1,2)*EN(2)+G(1,3)*EN(3)) 
                 HY = HY + FCT*STC*(G(2,1)*EN(1)+G(2,2)*EN(2)+G(2,3)*EN(3)) 
                 HZ = HZ + FCT*STC*(G(3,1)*EN(1)+G(3,2)*EN(2)+G(3,3)*EN(3)) 

               END DO
             END DO
           END DO
         END DO
       END DO
     END DO

     HX1 = FAC * (HX + HXP(ITX,IR))/ FOURPI 
     HY1 = FAC * (HY + HYP(ITX,IR))/ FOURPI 
     HZ  = FAC * (HZ + HZP(ITX,IR))/ FOURPI 

     CSF = COS (FANGLE(ITX))
     SNF = SIN (FANGLE(ITX))
     HX  = CSF * HX1 - SNF * HY1
     HY  = CSF * HY1 + SNF * HX1

     WRITE(ND,'(6E16.7,3(1X,F8.2),F16.6)') HY,HX,HZ,RX(ITX,IR),RY(ITX,IR),RZ(ITX,IR),FRQ

   END DO
 END DO

 DEALLOCATE (STDF,ZUNIQ)

 END SUBROUTINE HFIELD

!===========================================================================

 SUBROUTINE SENS_FD_CONSTRUCT (NS,NSTAT,NTX,NCOMP,INTN,NINEDG,NN,NZ,NE, &
                               NUME,NEDGE,NELEM,DIME,DIMN,DIMZ,FRQ,F, & 
                               NEL,NER,NNL,NNR,NZT,NZB)

!---------------------------------------------------------------------------
!
!*** Called by SAMAYA_3D
!
! Compute the magnetic field sensitivity.
!
!      ELOC - X - coordinates of the finite element nodes
!      NLOC - Y - coordinates of the finite element nodes
!      ZLOC - Z - coordinates of the finite element nodes
!        NE - number of target nodes in X-direction
!        NN - number of target nodes in Y-direction
!        NZ - number of target nodes in Z-direction
!       FRQ - frequency
!     NSTAT - number of primal transmitters
!       NTX - number of primal & adjoint transmitters
!     NCOMP - number of receiver components
!        NS - save-file unit number for sensitivities
!         F - array of the total electric fields in the target
! DIME,DIMN,-
!      DIMZ - dimension of the cells
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 REAL, PARAMETER :: PI = 3.14159265359, MU0 = 12.56637E-7
 COMPLEX, PARAMETER :: CI = (0.,1.)
 INTEGER L1,L2,L3,I1,I2,I3,I,J,K,NBN,NSTAT,NTX,NE,NN,NZ,NELEM,ITX,JTX, &
         IRX,NCOMP,IJ,NS,INTN,NINEDG,CX(4),CY(4),CZ(4),NUME(NINEDG), &
         NEDGE(12,(NE-1)*(NN-1)*(NZ-1)),HNOD(8),NEL,NER,NNL,NNR,NZT,NZB
 REAL S(5),WG(5),XM(12),YM(12),ZM(12),FCT,FAC_NT,VOL,DIME(NELEM), &
      DIMN(NELEM),DIMZ(NELEM),FRQ,S1,S2,S3,W1,W2,W3,SHP1,SHP2,SHP3
 COMPLEX EA(3),EP(3),F(INTN,NTX),IWMU,INT_E
 DATA XM/ 0.,-1., 1., 0.,-1., 1.,-1., 1., 0.,-1., 1., 0./, &
      YM/-1.,-1.,-1.,-1., 0., 0., 0., 0., 1., 1., 1., 1./, &
      ZM/-1., 0., 0., 1.,-1.,-1., 1., 1.,-1., 0., 0., 1./
 DATA S(1),S(2),S(3)/-0.774597,0.,+0.774597/
 DATA WG(1),WG(2),WG(3)/0.5555556,0.8888889,0.5555556/
 DATA CX/1,4,9,12/, CY/5,6,7,8/, CZ/2,3,10,11/

 FAC_NT = 400.*PI          ! Factor for magnetic fields from T to nT.
 IWMU = 2.*PI*FRQ*MU0*CI

 DO ITX = 1, NSTAT
   DO IRX = 1, NCOMP
     JTX = ITX + IRX*NSTAT
     DO J = NNL+1, NN-NNR-1
       DO K = NZT+1, NZ-NZB-1
         DO I = NEL+1, NE-NER-1

           NBN = (J-1)*(NE-1)*(NZ-1) + (K-1)*(NE-1) + I

           VOL = DIMN(NBN) * DIME(NBN) * DIMZ(NBN)

           HNOD(1) = (J-1)*NE*NZ + (K-1)*NE  + I
           HNOD(2) = HNOD(1) + 1
           HNOD(3) = HNOD(1) + NE
           HNOD(4) = HNOD(3) + 1
           HNOD(5) = HNOD(1) + NE*NZ
           HNOD(6) = HNOD(5) + 1
           HNOD(7) = HNOD(5) + NE
           HNOD(8) = HNOD(7) + 1

           ! Interpolate the electric fields from the edges to the Gaussian
           ! integration points, compute the dot product of the primal and adjoint
           ! fields, and volume integrate using Gaussian integration.

           INT_E = (0.,0.)

           DO L1 = 1, 3
             DO L2 = 1, 3
               DO L3 = 1, 3
                 S1 = S(L1)  ; S2 = S(L2)  ; S3 = S(L3)
                 W1 = WG(L1) ; W2 = WG(L2) ; W3 = WG(L3)
                 FCT = W1*W2*W3*VOL
                 EP = (0.,0.) ; EA = (0.,0.)
                 DO IJ = 1, 4
                   I1 = CX(IJ) ; I2 = CY(IJ) ; I3 = CZ(IJ)
                   SHP1  = (1.+S1*XM(I1))*(1.+S2*YM(I1))*(1.+S3*ZM(I1)) / 8
                   SHP2  = (1.+S1*XM(I2))*(1.+S2*YM(I2))*(1.+S3*ZM(I2)) / 8.
                   SHP3  = (1.+S1*XM(I3))*(1.+S2*YM(I3))*(1.+S3*ZM(I3)) / 8.
                   EP(1) = EP(1) + SHP1 * IWMU * F(NUME(NEDGE(I1,NBN)),ITX)
                   EP(2) = EP(2) + SHP2 * IWMU * F(NUME(NEDGE(I2,NBN)),ITX)
                   EP(3) = EP(3) + SHP3 * IWMU * F(NUME(NEDGE(I3,NBN)),ITX)
                   EA(1) = EA(1) - SHP1 * F(NUME(NEDGE(I1,NBN)),JTX)
                   EA(2) = EA(2) - SHP2 * F(NUME(NEDGE(I2,NBN)),JTX)
                   EA(3) = EA(3) - SHP3 * F(NUME(NEDGE(I3,NBN)),JTX)
                 END DO
                 INT_E = INT_E + FCT * (EP(1)*EA(1) + EP(2)*EA(2) + EP(3)*EA(3))
               END DO
             END DO
           END DO

           INT_E = FAC_NT * INT_E  

           WRITE (NS,'(2E16.7)') INT_E

         END DO
       END DO
     END DO
   END DO
 END DO

 END SUBROUTINE SENS_FD_CONSTRUCT

!===========================================================================

 SUBROUTINE SET_RHO (MR,BPR)

!---------------------------------------------------------------------------
!
!*** Called by BMATE, EPEDGE, HFIELD
!
!  Sets up horizontal interpolation array (12 digit precision) for Hankel
!  transforms from 0.1 m to 10 km
!
!---------------------------------------------------------------------------

 USE SA_Filter_Coefficients_q
 REAL(KIND=QL), ALLOCATABLE :: B(:)
 REAL(KIND=QL) QRHO, RBASE
 INTEGER JR,MR
 REAL BPR(100)

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
   B(JR) = B(JR-1) * QRHO
   IF (B(JR) > 1.D4) EXIT
 END DO
 MR = JR

 BPR(1:MR) = REAL (B(1:MR))
 DEALLOCATE (B)

 END SUBROUTINE SET_RHO

!===========================================================================

 SUBROUTINE BASE5 (GEH,BPR,RR,MR,ZUNIQ,IZMAX,IBASE,ZP,Z,C,NLYR,LAY,SIGL, & 
                   KSQR,THICK,RMU)

!---------------------------------------------------------------------------
!
!*** Called by: BMATE
!*** Calls CUBSPL, EGTHNK
!
! Computes the secondary dyadic convolution integrals.
!
!    FUN - external function
!    GEH - array for the indexing scheme
!    BPR - array of splines
!     RR - distance between source and observation point
!     MR - total number of spline-nodes
!  ZUNIQ - array of diff. z-locations  
!  IBASE - number for the indexing scheme
!      C - coefficients of Green's tensor kernel
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 INTEGER K,L,NLYR,IBASE(5),J1(5),J2(5),IFOUND,IZPOS,LAY,J1A, &
         J2A,IMR,MR,IZMAX,JR
 REAL THICK,ZLOG,BR,RR,Z,ZP,FR1,FR2,BPR(100),CF(4),CF1(4),ZUNIQ(5,IZMAX), &
      GEH(40,MR,IZMAX)
 REAL, DIMENSION (NLYR) :: RMU
 REAL, DIMENSION (4,MR) :: GR1,GI1,GR2,GI2,GR3,GI3,GR4,GI4,GR5,GI5
 COMPLEX SIGL(2),KSQR(2),C(5)
 COMPLEX (KIND=QL) EHRI(MR,6)
 DATA J1/0,8,16,24,32/, J2/4,12,20,28,36/

 ZLOG = 0.
 IF (LAY == 1) ZLOG = ABS(Z+ZP)
 IF ((LAY == 5) .AND. ((Z-ZP) > 0.)) ZLOG = LOG(Z-ZP)

 IFOUND = 0                               ! Check ZP in the index.
 DO IZPOS = 1, IBASE(LAY)
    IF (ABS (ZLOG-ZUNIQ(LAY,IZPOS)) < 1.) THEN
       IFOUND = 1 ; EXIT
    END IF
 END DO

 IF (IFOUND == 0) THEN                    ! New entry.

   IBASE(LAY) = IBASE(LAY) + 1
   IZPOS = IBASE(LAY)
   ZUNIQ(LAY,IZPOS) = ZLOG

   IF (LAY == 1 .OR. LAY == 5) THEN     ! Source in layer 2, boundary in layer 2 or in air.
     GR1 = 0; GR2 = 0; GR3 = 0; GR4 = 0; GR5 = 0
     GI1 = 0; GI2 = 0; GI3 = 0; GI4 = 0; GI5 = 0

     CALL EGTHNK (MR,BPR,NLYR,LAY,SIGL,RMU,KSQR,THICK,ZP,Z,EHRI)

     DO JR = 1, MR
       GR1(1,JR) = REAL (REAL (EHRI(JR,1)),4)
       GI1(1,JR) = REAL (AIMAG(EHRI(JR,1)),4)
       GR2(1,JR) = REAL (REAL (EHRI(JR,2)),4)
       GI2(1,JR) = REAL (AIMAG(EHRI(JR,2)),4)
       GR3(1,JR) = REAL (REAL (EHRI(JR,3)),4)
       GI3(1,JR) = REAL (AIMAG(EHRI(JR,3)),4)
       GR4(1,JR) = REAL (REAL (EHRI(JR,4)),4)
       GI4(1,JR) = REAL (AIMAG(EHRI(JR,4)),4)
       GR5(1,JR) = REAL (REAL (EHRI(JR,5)),4)
       GI5(1,JR) = REAL (AIMAG(EHRI(JR,5)),4)
     END DO

     CALL CUBSPL (BPR,GR1,MR,0,0)
     CALL CUBSPL (BPR,GI1,MR,0,0)
     CALL CUBSPL (BPR,GR2,MR,0,0)
     CALL CUBSPL (BPR,GI2,MR,0,0)
     CALL CUBSPL (BPR,GR3,MR,0,0)
     CALL CUBSPL (BPR,GI3,MR,0,0)
     CALL CUBSPL (BPR,GR4,MR,0,0)
     CALL CUBSPL (BPR,GI4,MR,0,0)
     CALL CUBSPL (BPR,GR5,MR,0,0)
     CALL CUBSPL (BPR,GI5,MR,0,0)

     GEH( 1: 4, 1:MR, IBASE(LAY)) = GR1(1:4, 1:MR)
     GEH( 5: 8, 1:MR, IBASE(LAY)) = GI1(1:4, 1:MR)
     GEH( 9:12, 1:MR, IBASE(LAY)) = GR2(1:4, 1:MR)
     GEH(13:16, 1:MR, IBASE(LAY)) = GI2(1:4, 1:MR)
     GEH(17:20, 1:MR, IBASE(LAY)) = GR3(1:4, 1:MR)
     GEH(21:24, 1:MR, IBASE(LAY)) = GI3(1:4, 1:MR)
     GEH(25:28, 1:MR, IBASE(LAY)) = GR4(1:4, 1:MR)
     GEH(29:32, 1:MR, IBASE(LAY)) = GI4(1:4, 1:MR)
     GEH(33:36, 1:MR, IBASE(LAY)) = GR5(1:4, 1:MR)
     GEH(37:40, 1:MR, IBASE(LAY)) = GI5(1:4, 1:MR)

   ELSE IF (LAY == 3) THEN             ! Source in layer 1, boundary in layer 2.
     WRITE(*,*) ' NOT IMPLEMENTED YET'
   ELSE IF (LAY == 4) THEN             ! Source in layer 1, boundary in layer 1.
     WRITE(*,*) ' NOT IMPLEMENTED YET'
   ELSE IF (LAY == 2) THEN             ! Source in layer 2, boundary in layer 1.
     WRITE(*,*) ' NOT IMPLEMENTED YET'
   END IF

 END IF

 DO IMR = 1, MR                       ! READ THE VALUES      
   IF (BPR(IMR) > RR)   THEN
     BR = RR - BPR(IMR-1)
     DO L = 1, 5
       DO K = 1, 4
         J1A = J1(L)+K
         J2A = J2(L)+K
         CF (K) = GEH (J1A,IMR-1,IZPOS)
         CF1(K) = GEH (J2A,IMR-1,IZPOS)
       END DO
       FR1 = ((CF (4)*BR/3 + CF (3))*BR/2 + CF (2))*BR + CF (1)
       FR2 = ((CF1(4)*BR/3 + CF1(3))*BR/2 + CF1(2))*BR + CF1(1)
       C(L)= CMPLX(FR1,FR2)
     END DO
     RETURN
   END IF
 END DO

 END SUBROUTINE BASE5

!===========================================================================

 SUBROUTINE EGTHNK (NRHO,RHOTRP,NLYR,LAY,SIGL,RMU,KSQ_LYR,THICK,ZP,ZR,EHRI)

!---------------------------------------------------------------------------
!
!*** Called by BASE5
!*** Calls EGTVAL
!
! Sets up the five integrals EHRI (JR,J1), J1 = 1,6, needed to compute the
! electric Green's tensor elements. It uses the flow through Hankel transform
! method to compute values at RHOTRP(JR), (15 point per decade spacing).
! It uses a 15 point per decade filter coefficient set derived from
! Christensen's program, FLTGEN.
!
!   RHOTRP - interpolation array of rho values
!     NRHO - number of interpolation points needed for EGT
!     NLYR - number of layers
!     SIGL - complex conductivity of layers
!  KSQ_LYR - propagation constants
!    THICK - thickness of overburden
!      ZTR = the reflected distance between "source" & "receiver"
!            points off bottom of overburden.
!     EHRI - inverse Hankel transform (J = 1,6)
!
!---------------------------------------------------------------------------

 USE SA_Filter_Coefficients_q
 IMPLICIT NONE
 REAL(KIND=QL), PARAMETER :: FOURPI=12.56637_QL
 COMPLEX(KIND=QL), PARAMETER :: ZERO=(0._QL, 0._QL)
 INTEGER NLYR,NRHO,L,JR,LMAX,LMIN,K,KMAX,KMIN,KBOT,K1,LAY
 REAL RHOTRP(NRHO),ZP,ZR,THICK,RMU(NLYR)
 REAL(KIND=QL) DELTA,Y1,Y,RD,RMUD(NLYR),THKD,LMBDA,RHOD,ZPD,ZRD
 COMPLEX KSQ_LYR(NLYR),SIGL(NLYR)
 COMPLEX(KIND=QL) KSQLD(NLYR),SIGLD(NLYR),KER(JNLO-NRHO:JNHI,6),EHRI(NRHO,6)
 LOGICAL JUMP

 RMUD = REAL (RMU,QL)
 THKD = REAL (THICK,QL)
 KSQLD = CMPLX (KSQ_LYR,KIND=QL)
 SIGLD = CMPLX (SIGL,KIND=QL)
 ZPD = REAL (ZP,QL)
 ZRD = REAL (ZR,QL)

 EHRI = ZERO
 KER = ZERO
 DELTA = LOG (10._QL)/ REAL (NDEC_JN,QL)

 ! Set up KER for JR = 1 corresponding to minimum value of RHO.  This will
 ! compute most of the needed kernel range from the high end.  Note that
 ! the filter is defined between JNLO < L < JNHI

 JR = 1
 RD = REAL (RHOTRP(1),QL)
 Y1 = -LOG (RD) - DBLE (JR-1) * DELTA -  SHFTJN

 DO L = -50, JNHI             ! Start at L = -50 to pick up low values.
   LMAX = L                   ! Maximum filter index used
   K = L + 1 - JR             ! K is the kernel index.
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
   IF(LAY == 1) CALL EGTVAL (NRHO,K,JR,L,LMBDA,NLYR,SIGLD,RMUD,KSQLD,THKD,ZPD,ZRD,KER,EHRI,JUMP)
   IF(LAY == 5) CALL EGTVAL5(NRHO,K,JR,L,LMBDA,NLYR,SIGLD,KSQLD,THKD,ZPD,ZRD,KER,EHRI,JUMP)
   IF(JUMP) EXIT
 END DO

 Y = Y1                       ! Finish off the low end for RHOTRP(1)
 DO L = -51, JNLO, -1
   LMIN = L                   ! Minimum filter index used
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
   K = L + 1 - JR             ! Compute the kernel index.
   IF(LAY == 1) CALL EGTVAL (NRHO,K,JR,L,LMBDA,NLYR,SIGLD,RMUD,KSQLD,THKD,ZPD,ZRD,KER,EHRI,JUMP)
   IF(LAY == 5) CALL EGTVAL5(NRHO,K,JR,L,LMBDA,NLYR,SIGLD,KSQLD,THKD,ZPD,ZRD,KER,EHRI,JUMP)
   IF(JUMP) EXIT
 END DO

 KMIN = LMIN + 1 - JR  !  Define the range of kernel values
 KMAX = LMAX + 1 - JR  !  used for RHOTRP(1)

 ! Complete definition of kernel values by evaluating transform of
 ! maximum RHO = RHOTRP (NRHO)

 JR = NRHO
 Y1 = -LOG (RD) - DBLE (JR-1) * DELTA - SHFTJN
 KBOT = JNLO + 1 - JR
 K1 = MAX (KBOT,KMIN)

 DO K = K1, KMAX          ! Compute EHR for maximum RHO using previously
   L = K - 1 + JR         ! computed kernel values.
   IF (L > JNHI .OR. L < JNLO) CYCLE
     EHRI(JR,1:3) = EHRI(JR,1:3) + KER(K,1:3) * WJ0(L)
     EHRI(JR,4:5) = EHRI(JR,4:5) + KER(K,4:5) * WJ1(L)
 END DO

 IF (K1 > KBOT) THEN    !  Add low end kernel values until convergence.
   DO K = K1-1, KBOT, -1
     KMIN = K
     L = K - 1 + JR
     Y = Y1 + DBLE (L) * DELTA
     LMBDA = EXP (Y)
     IF(LAY == 1) CALL EGTVAL (NRHO,K,JR,L,LMBDA,NLYR,SIGLD,RMUD,KSQLD,THKD,ZPD,ZRD,KER,EHRI,JUMP)
     IF(LAY == 5) CALL EGTVAL5(NRHO,K,JR,L,LMBDA,NLYR,SIGLD,KSQLD,THKD,ZPD,ZRD,KER,EHRI,JUMP)
     IF (JUMP) EXIT
   END DO
 END IF

 DO JR = 2, NRHO-1          !  Compute transforms for all other RHO values
   DO K = KMIN, KMAX        !  using previously computed kernel values.
     L = K - 1 + JR
     IF (L > JNHI .OR. L < JNLO) CYCLE
     EHRI(JR,1:3) = EHRI(JR,1:3) + KER(K,1:3) * WJ0(L)
     EHRI(JR,4:5) = EHRI(JR,4:5) + KER(K,4:5) * WJ1(L)
   END DO
 END DO

 DO JR = 1,NRHO
   RHOD = REAL (RHOTRP(JR),KIND=QL)
   EHRI(JR,1:5) = EHRI(JR,1:5) / (FOURPI * RHOD)
 END DO

 END SUBROUTINE EGTHNK

!===========================================================================

 SUBROUTINE EGTVAL (NRHO,K,JR,L,LMBDA,NLYR,SIGLD,RMUD,KSQLD,THKD,ZPD,ZRD, &
                    KER,EHRI,JUMP)

!---------------------------------------------------------------------------
!
!*** Called by EGTHNK
!
! Accumulates the integrals for EHRI(J), the five inverse Hankel transforms
! needed for evaluation of Green's tensor elements
!
!     NRHO - number of points in 15 point / decade array
!        K - filter kernel index
!       JR - RHO interpolation index
!        L - filter point index
!    LMBDA - Hankel transform variable
!     NLYR - number of layers
!    SIGLD - complex resistivity of layers
!    KSQLD - propagation constants
!     THKD - thickness of overburden
!  EHRI(J) - accumulated complex integrals for inverse Hankel transform (J = 1,6)
!     JUMP - keep integrating if false.
!
!---------------------------------------------------------------------------

 USE SA_Filter_Coefficients_q
 IMPLICIT NONE
 REAL, PARAMETER :: TOL=1.E-6
 COMPLEX(KIND=QL), PARAMETER :: ONE=(1._QL,0._QL)
 INTEGER NRHO,K,JR,L,NLYR,J1
 REAL (KIND=QL)THKD,ZPD,ZRD,LMBDA,EHR,EHI,AR,AI,RMUD(NLYR),RMU1S,RMU2,RMU2S
 COMPLEX (KIND=QL) LMBSQ,EHRI(NRHO,6),KER(JNLO-NRHO:JNHI,6),SIGLD(NLYR),KSQLD(NLYR), &
                   S0,S1,S2,T0,T1,R1,G2,ETA2,TXP1,TXP2,TMP(6),B2
 LOGICAL TOO_BIG,JUMP

 RMU1S = RMUD(1) * RMUD(1)
 LMBSQ = CMPLX (LMBDA * LMBDA, 0.)
 S0 = CMPLX (LMBDA, 0.)
 S1 = SQRT (LMBSQ + KSQLD(1))
 S2 = SQRT (LMBSQ + KSQLD(NLYR))
 T0 = ((RMU1S - 1._QL)*LMBSQ  - KSQLD(1)) / (RMUD(1)*S0 + S1)**2
 IF (ZPD <  0._QL) TXP2 = EXP ( S0*ZPD-S2*ZRD)
 IF (ZPD >= 0._QL) TXP2 = EXP (-S2*ZPD-S2*ZRD)
 IF (NLYR == 1) THEN
   ETA2 = -T0 * TXP2
   G2 = -TXP2
 ELSE
   RMU2 = RMUD(2) / RMUD(1)
   RMU2S = RMU2*RMU2
   T1 = (KSQLD(1) - KSQLD(2) + (RMU2S - 1._QL)*S1**2 ) / (RMU2 * S1 + S2)**2
   R1 = (SIGLD(2)*S1 - SIGLD(1)*S2) / (SIGLD(2)*S1 + SIGLD(1)*S2)
   TXP1 = EXP (-2.* THKD *S1)
   G2 = -TXP2 * (R1 + TXP1) / (ONE + R1*TXP1)
   ETA2 = -TXP2 * (T1 + T0*TXP1) / (ONE + T0*T1*TXP1)
 END IF

 B2 = G2*S2 - ETA2*KSQLD(NLYR)/S2

 KER(K,2) =      S0*B2 / SIGLD(NLYR)
 KER(K,4) =         B2 / SIGLD(NLYR) 
 KER(K,1) = -LMBDA* ETA2*KSQLD(NLYR)/S2 /SIGLD(NLYR) 
 KER(K,5) =  LMBSQ* G2                  /SIGLD(NLYR) 
 KER(K,3) =  LMBDA* G2*S0/S2            /SIGLD(NLYR) 

 TMP(1:3) = WJ0(L) * KER(K,1:3)
 TMP(4:5) = WJ1(L) * KER(K,4:5)

 JUMP = .TRUE.
 DO J1 = 1,5
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

!===========================================================================

 SUBROUTINE EGTVAL5 (NRHO,K,JR,L,LMBDA,NLYR,SIGLD,KSQLD,THKD,ZPD,ZRD, &
                    KER,EHRI,JUMP)

!---------------------------------------------------------------------------
!
!***  Called by EGTHNK
!
! Accumulates the integrals for EHRI(J), the five inverse Hankel transforms
! needed for evaluation of Green's tensor elements
!
!     NRHO - number of points in 15 point / decade array
!        K - filter kernel index
!       JR - RHO interpolation index
!        L - filter point index
!    LMBDA - Hankel transform variable
!     NLYR - number of layers
!    SIGLD - complex resistivity of layers
!    KSQLD - propagation constants
!     THKD - thickness of overburden
!  EHRI(J) - accumulated complex integrals for inverse Hankel transform
!     JUMP - keep integrating if false.
!
!---------------------------------------------------------------------------

 USE SA_Filter_Coefficients_q
 IMPLICIT NONE
 REAL, PARAMETER :: TOL=1.E-6
 COMPLEX(KIND=QL), PARAMETER :: ONE=(1._QL,0._QL), TWO=(2._QL,0._QL)
 INTEGER NRHO,K,JR,L,NLYR,J1
 REAL (KIND=QL)THKD,ZPD,ZRD,LMBDA,EHR,EHI,AR,AI
 COMPLEX (KIND=QL) LMBSQ,EHRI(NRHO,6),KER(JNLO-NRHO:JNHI,6),SIGLD(NLYR),KSQLD(NLYR), &
                   S0,S1,S2,TXP,TMP(6),XX,YY,R0L,R1L,R1S,R1T,F0,P0,Q2,A0
 LOGICAL TOO_BIG,JUMP

 LMBSQ = CMPLX (LMBDA*LMBDA, 0.)
 S0  = CMPLX (LMBDA, 0.)
 S1  = SQRT (LMBSQ + KSQLD(1))
 S2  = SQRT (LMBSQ + KSQLD(NLYR))

 IF (ZPD <  0._QL) TXP = EXP(S0*ZRD + S0*ZPD)
 IF (ZPD >= 0._QL) TXP = EXP(S0*ZRD - S1*ZPD)

 R0L = (S1-S0) / (S1+S0)

 IF (NLYR == 1) THEN
    F0 =  TWO*S1/LMBDA
    P0 =  ONE + R0L
    A0 = -TWO*S0*S1*P0 + S1*(S0-S1)*P0
 ELSE
    XX  = EXP(-TWO*S1*THKD)
    YY  = EXP(-TWO*S1*(THKD-ZPD))
    R1L = (S2-S1) / (S2+S1)
    R1S = S0*(SIGLD(2)-SIGLD(1)) / (S2*SIGLD(1)+S1*SIGLD(2))
    R1T = (S2*SIGLD(1)-S1*SIGLD(2)) / (S2*SIGLD(1)+S1*SIGLD(2))
    F0 =  TWO*S1/S0 *(ONE-R1T*YY)/(ONE-R1T*XX)
    P0 = (ONE+R0L)*(ONE-R1L*YY)/(ONE+R0L*R1L*XX)
    Q2 = (ONE-R1L) *(YY+R0L*XX)/(ONE+R0L*R1L*XX)
    A0 = TWO*S1*(R1S*Q2-P0)/(ONE-R1T*XX) + (S1-S0)*P0
 END IF

 KER(K,2) =  LMBDA* A0/S1         /SIGLD(1) * TXP
 KER(K,4) =         A0/S1         /SIGLD(1) * TXP
 KER(K,1) = -LMBDA* P0/S1*KSQLD(1)/SIGLD(1) * TXP
 KER(K,5) =  LMBSQ* A0/S1         /SIGLD(1) * TXP
 KER(K,3) =  LMBDA* F0*S0/S1      /SIGLD(1) * TXP

 TMP(1:3) = WJ0(L) * KER(K,1:3)
 TMP(4:5) = WJ1(L) * KER(K,4:5)

 JUMP = .TRUE.
 DO J1 = 1,5
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

 END SUBROUTINE EGTVAL5

!===========================================================================

 SUBROUTINE HGTHNK (NRHO,RHOTRP,NLYR,RMU,KSQ_LYR,THICK,ZP,ZR,EHRI)

!---------------------------------------------------------------------------
!
!***  Called by EGT_CSPL
!***  Calls EGTVAL
!
! Sets up the five integrals EHRI (JR,J1), J1 = 1,6, needed to compute the
! electric Green's tensor elements in EGT_BOSS.  It uses the flow through
! Hankel transform method to compute values at RHOTRP(JR), (15 point per decade
! spacing).  It uses a 15 point per decade filter coefficient set derived from
! Christensen's program, FLTGEN.
!
!   RHOTRP - interpolation array of rho values
!     NRHO - number of interpolation points needed for EGT
!     NLYR - number of layers
!  KSQ_LYR - propagation constants
!    THICK - thickness of overburden
!      ZTR = the reflected distance between "source" & "receiver"
!            points off bottom of overburden.
!     EHRI - inverse Hankel transform (J = 1,6)
!
!---------------------------------------------------------------------------

 USE SA_Filter_Coefficients_q
 IMPLICIT NONE
 COMPLEX(KIND=QL), PARAMETER :: ZERO=(0._QL, 0._QL)
 INTEGER NLYR,NRHO,L,JR,LMAX,LMIN,K,KMAX,KMIN,KBOT,K1
 REAL RHOTRP(NRHO),THICK,RMU(NLYR),ZP,ZR
 REAL(KIND=QL) DELTA,Y1,Y,RD,RMUD(NLYR),THKD,LMBDA,ZPD,ZRD,RHOD
 COMPLEX KSQ_LYR(NLYR)
 COMPLEX(KIND=QL) KSQLD(NLYR),KER(JNLO-NRHO:JNHI,6),EHRI(NRHO,3)
 LOGICAL JUMP

 RMUD = REAL (RMU,QL)
 THKD = REAL (THICK,QL)
 KSQLD = CMPLX (KSQ_LYR,KIND=QL)
 ZPD = REAL (ZP,QL)
 ZRD = REAL (ZR,QL)

 EHRI = ZERO
 KER = ZERO
 DELTA = LOG (10._QL)/ REAL (NDEC_JN,QL)

 ! Set up KER for JR = 1 corresponding to minimum value of RHO.  This will
 ! compute most of the needed kernel range from the high end.  Note that
 ! the filter is defined between JNLO < L < JNHI

 JR = 1
 RD = REAL (RHOTRP(1),QL)
 Y1 = -LOG (RD) - DBLE (JR-1) * DELTA -  SHFTJN

 DO L = -50, JNHI             ! Start at L = -50 to pick up low values.
   LMAX = L                   ! Maximum filter index used
   K = L + 1 - JR             ! K is the kernel index.
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
   CALL HGTVAL (NRHO,K,JR,L,LMBDA,NLYR,RMUD,KSQLD,THKD,ZPD,ZRD,KER,EHRI,JUMP)
   IF (JUMP) EXIT
 END DO

 Y = Y1                       ! Finish off the low end for RHOTRP(1)
 DO L = -51, JNLO, -1
   LMIN = L                   ! Minimum filter index used
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
   K = L + 1 - JR             ! Compute the kernel index.
   CALL HGTVAL (NRHO,K,JR,L,LMBDA,NLYR,RMUD,KSQLD,THKD,ZPD,ZRD,KER,EHRI,JUMP)
   IF (JUMP) EXIT
 END DO

 KMIN = LMIN + 1 - JR  ! Define the range of kernel values
 KMAX = LMAX + 1 - JR  ! used for RHOTRP(1)

 ! Complete definition of kernel values by evaluating transform of
 ! maximum RHO = RHOTRP (NRHO)

 JR = NRHO
 Y1 = -LOG (RD) - DBLE (JR-1) * DELTA - SHFTJN
 KBOT = JNLO + 1 - JR
 K1 = MAX (KBOT,KMIN)

 DO K = K1, KMAX          ! Compute EHR for maximum RHO using previously
   L = K - 1 + JR         ! computed kernel values.
   IF (L > JNHI .OR. L < JNLO) CYCLE
     EHRI(JR,1) = EHRI(JR,1) + KER(K,1) * WJ0(L)
     EHRI(JR,2) = EHRI(JR,2) + KER(K,2) * WJ1(L)
     EHRI(JR,3) = EHRI(JR,3) + KER(K,3) * WJ1(L)
 END DO

 IF (K1 > KBOT) THEN    ! Add low end kernel values until convergence.
   DO K = K1-1, KBOT, -1
     KMIN = K
     L = K - 1 + JR
     Y = Y1 + DBLE (L) * DELTA
     LMBDA = EXP (Y)
     CALL HGTVAL (NRHO,K,JR,L,LMBDA,NLYR,RMUD,KSQLD,THKD,ZPD,ZRD,KER,EHRI,JUMP)
     IF (JUMP) EXIT
   END DO
 END IF

 DO JR = 2, NRHO-1          ! Compute transforms for all other RHO values
   DO K = KMIN, KMAX        ! using previously computed kernel values.
     L = K - 1 + JR
     IF (L > JNHI .OR. L < JNLO) CYCLE
     EHRI(JR,1) = EHRI(JR,1) + KER(K,1) * WJ0(L)
     EHRI(JR,2) = EHRI(JR,2) + KER(K,2) * WJ1(L)
     EHRI(JR,3) = EHRI(JR,3) + KER(K,3) * WJ1(L)
   END DO
 END DO

 DO JR = 1,NRHO
   RHOD = REAL (RHOTRP(JR),KIND=QL)
   EHRI(JR,1:3) = EHRI(JR,1:3) / RHOD
 END DO

 END SUBROUTINE HGTHNK

!===========================================================================

 SUBROUTINE HGTVAL (NRHO,K,JR,L,LMBDA,NLYR,RMUD,KSQLD,THKD,ZPD,ZRD,KER, &
                    EHRI,JUMP)

!---------------------------------------------------------------------------
!
!*** Called by EGTHNK
!
! Accumulates the integrals for EHRI(J), the five inverse Hankel transforms
! needed for evaluation of Green's tensor elements
!
!     NRHO - number of points in 15 point / decade array
!        K - filter kernel index
!       JR - RHO interpolation index
!        L - filter point index
!    LMBDA - Hankel transform variable
!     NLYR - number of layers
!    KSQLD - propagation constants
!     THKD - thickness of overburden
!  EHRI(J) - accumulated complex integrals for inverse Hankel transform (J = 1,6)
!     JUMP - keep integrating if false.
!
!---------------------------------------------------------------------------

 USE SA_Filter_Coefficients_q
 IMPLICIT NONE
 REAL, PARAMETER :: TOL=1.E-6
 COMPLEX(KIND=QL), PARAMETER :: ONE=(1._QL,0._QL)
 INTEGER NRHO,K,JR,L,NLYR,J1
 REAL (KIND=QL)THKD,ZPD,ZRD,LMBDA,EHR,EHI,AR,AI,RMUD(NLYR),RMU1S,RMU2,RMU2S
 COMPLEX (KIND=QL) LMBSQ,EHRI(NRHO,3),KER(JNLO-NRHO:JNHI,3),KSQLD(NLYR), &
                   S0,S1,S2,T0,T1,G2,TXP1,TMP(3)
 LOGICAL TOO_BIG,JUMP

 RMU1S = RMUD(1) * RMUD(1)
 LMBSQ = CMPLX (LMBDA * LMBDA, 0.)
 S0 = CMPLX (LMBDA, 0.)
 S1 = SQRT (LMBSQ + KSQLD(1))
 T0 = ((RMU1S - 1._QL)*LMBSQ  - KSQLD(1)) / (RMUD(1)*S0 + S1)**2

 IF (ABS(THKD) < 1.E-4_QL)  THEN
    IF (ZPD <  0._QL) G2 = (ONE+T0) * EXP( S0*ZPD-S0*ZRD)
    IF (ZPD >= 0._QL) G2 = (ONE+T0) * EXP(-S1*ZPD-S0*ZRD)
 ELSE
    S2 = SQRT (LMBSQ + KSQLD(NLYR))
    RMU2 = RMUD(2) / RMUD(1)
    RMU2S = RMU2*RMU2
    T1 = (KSQLD(1) - KSQLD(2) + (RMU2S - 1._QL)*S1**2 ) / (RMU2 * S1 + S2)**2
    TXP1 = EXP (-2.* THKD *S1)
    IF (ZPD < THKD)  THEN
       G2 = (ONE+T0)*(ONE-T1*EXP(-2*S1*(THKD-ZPD)))/(ONE-T0*T1*TXP1)*EXP(-S1*ZPD-S0*ZRD)
    ELSE
       G2 = (ONE+T0)*(ONE-T1)/(ONE-T0*T1*TXP1) * EXP(-S2*ZPD+(S2-S1)*THKD-S0*ZRD)
    ENDIF
 END IF

 KER(K,1) = G2 * LMBDA 
 KER(K,2) = G2 
 KER(K,3) = G2 * LMBDA

 TMP(1) =  WJ0(L) * KER(K,1)
 TMP(2) =  WJ1(L) * KER(K,2)
 TMP(3) =  WJ1(L) * KER(K,3)

 JUMP = .TRUE.
 DO J1 = 1,3
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

 END SUBROUTINE HGTVAL

!===========================================================================

 SUBROUTINE BASEH (GEH,BPR,RR,MR,ZUNIQ,IZMAX,IBASE,ZP,ZR,C,NLYR,KSQR, &
                   THICK,RMU)

!---------------------------------------------------------------------------
!
!*** Called by HFIELD
!*** Calls CUBSPL, HGTHNK
!
! Computes the secondary dyadic convolution integrals.
!
!   FUN - external function
!   GEH - array for the indexing scheme
!   BPR - array of splines
!    RR - distance between source and observation point
!    MR - total number of spline-nodes
! ZUNIQ - array of diff. z-locations  
! IBASE - number for the indexing scheme
!     C - coefficients of Green's tensor kernel
!
!---------------------------------------------------------------------------

 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 INTEGER K,L,MR,NLYR,IBASE,J1(3),J2(3),IFOUND,IZPOS,J1A,J2A,JR,IMR,IZMAX
 REAL THICK,ZLOG,ZP,ZR,BR,FR1,FR2,RR
 REAL, DIMENSION (NLYR) :: RMU
 REAL, DIMENSION (4,MR) :: GR1,GI1,GR2,GI2,GR3,GI3
 COMPLEX KSQR(2),C(3)
 COMPLEX (KIND=QL) EHRI(MR,3)
 REAL BPR(100),CF(4),CF1(4),ZUNIQ(IZMAX),GEH(24,MR,IZMAX)
 DATA J1/0,8,16/, J2/4,12,20/

 ZLOG = ABS(ZP+ZR)
 IFOUND = 0                               ! Check ZP in the index.
 IF (IBASE > 0)   THEN
   DO IZPOS = 1, IBASE
     IF (ABS (ZLOG-ZUNIQ(IZPOS)) < 1.) THEN
       IFOUND = 1 ; EXIT
     END IF
   END DO
 END IF

 IF (IFOUND == 0) THEN                    ! New entry.
   IBASE = IBASE + 1
   ZUNIQ(IBASE) = ZLOG
   IZPOS = IBASE
   GR1 = 0; GR2 = 0; GR3 = 0
   GI1 = 0; GI2 = 0; GI3 = 0
   CALL HGTHNK (MR,BPR,NLYR,RMU,KSQR,THICK,ZP,ZR,EHRI)
   DO JR = 1, MR
      GR1(1,JR) = REAL (REAL (EHRI(JR,1)),4)
      GI1(1,JR) = REAL (AIMAG(EHRI(JR,1)),4)
      GR2(1,JR) = REAL (REAL (EHRI(JR,2)),4)
      GI2(1,JR) = REAL (AIMAG(EHRI(JR,2)),4)
      GR3(1,JR) = REAL (REAL (EHRI(JR,3)),4)
      GI3(1,JR) = REAL (AIMAG(EHRI(JR,3)),4)
   END DO
   CALL CUBSPL (BPR,GR1,MR,0,0)
   CALL CUBSPL (BPR,GI1,MR,0,0)
   CALL CUBSPL (BPR,GR2,MR,0,0)
   CALL CUBSPL (BPR,GI2,MR,0,0)
   CALL CUBSPL (BPR,GR3,MR,0,0)
   CALL CUBSPL (BPR,GI3,MR,0,0)
   GEH( 1: 4, 1:MR, IBASE) = GR1(1:4, 1:MR)
   GEH( 5: 8, 1:MR, IBASE) = GI1(1:4, 1:MR)
   GEH( 9:12, 1:MR, IBASE) = GR2(1:4, 1:MR)
   GEH(13:16, 1:MR, IBASE) = GI2(1:4, 1:MR)
   GEH(17:20, 1:MR, IBASE) = GR3(1:4, 1:MR)
   GEH(21:24, 1:MR, IBASE) = GI3(1:4, 1:MR)
 END IF

 DO IMR = 1, MR                       ! Read the values.
   IF (BPR(IMR) > RR)   THEN
     BR = RR - BPR(IMR-1)
     DO L = 1, 3
       DO K = 1, 4
         J1A = J1(L)+K
         J2A = J2(L)+K
         CF (K) = GEH (J1A,IMR-1,IZPOS)
         CF1(K) = GEH (J2A,IMR-1,IZPOS)
       END DO
       FR1 = ((CF (4)*BR/3 + CF (3))*BR/2 + CF (2))*BR + CF (1)
       FR2 = ((CF1(4)*BR/3 + CF1(3))*BR/2 + CF1(2))*BR + CF1(1)
       C(L)= CMPLX(FR1,FR2)
     END DO
     RETURN
   END IF
 END DO

 END SUBROUTINE BASEH

!===========================================================================

 SUBROUTINE BGTHNK (RHOTRP,NLYR,RMU,KSQ_LYR,THICK,ZRF,C)

!---------------------------------------------------------------------------
!
!*** Called by HFIELD
!
!   RHOTRP - interpolation array of rho values
!     NLYR - number of layers
!     SIGL - complex conductivity of layers
!  KSQ_LYR - propagation constants
!    THICK - thickness of overburden
!      ZTR = the reflected distance between "source" & "receiver"
!            points off bottom of overburden.
!     EHRI - inverse Hankel transform
!
!---------------------------------------------------------------------------

 USE SA_Filter_Coefficients_q
 IMPLICIT NONE
 COMPLEX(KIND=QL), PARAMETER :: ZERO=(0._QL, 0._QL)
 INTEGER NLYR,L,K
 REAL RHOTRP,THICK,RMU(NLYR),ZRF
 REAL(KIND=QL) DELTA,Y1,Y,RD,RMUD(NLYR),THKD,LMBDA,ZRFD,RHOD
 COMPLEX KSQ_LYR(NLYR),C(3)
 COMPLEX(KIND=QL) KSQLD(NLYR),KER(JNLO:JNHI,3),EHRI(3)

 RMUD = REAL (RMU,QL)
 THKD = REAL (THICK,QL)
 KSQLD = CMPLX (KSQ_LYR,KIND=QL)
 ZRFD= REAL (ZRF,QL)

 EHRI = ZERO
 KER  = ZERO
 DELTA = LOG (10._QL)/ REAL (NDEC_JN,QL)

 ! Set up KER corresponding to minimum value of RHO.  This will
 ! compute most of the needed kernel range from the high end.  Note that
 ! the filter is defined between JNLO < L < JNHI

 RD = REAL (RHOTRP,QL)
 Y1 = -LOG (RD) - SHFTJN
 DO L = -50, JNHI             ! Start at L = -50 to pick up low values.
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
   CALL BGTVAL (L,LMBDA,NLYR,RMUD,KSQLD,THKD,ZRFD,KER)
 END DO
 Y = Y1                       ! Finish off the low end for RHOTRP(1)
 DO L = -51, JNLO, -1
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
   CALL BGTVAL (L,LMBDA,NLYR,RMUD,KSQLD,THKD,ZRFD,KER)
 END DO
 DO K = JNLO, JNHI
    EHRI(1) = EHRI(1) + KER(K,1) * WJ0(K)
    EHRI(2) = EHRI(2) + KER(K,2) * WJ1(K)
    EHRI(3) = EHRI(3) + KER(K,3) * WJ1(K)
 END DO
 RHOD = REAL (RHOTRP,KIND=QL)
 EHRI(1:3) = EHRI(1:3) / RHOD
 C = CMPLX(EHRI)

 END SUBROUTINE BGTHNK

!===========================================================================

 SUBROUTINE BGTVAL (L,LMBDA,NLYR,RMUD,KSQLD,THKD,ZRFD,KER)

!---------------------------------------------------------------------------
!
!***  Called by BGTHNK
!
! Accumulates the integrals for EHRI, the five inverse Hankel transforms
! needed for evaluation of Green's tensor elements
!
!        L - filter kernel index
!    LMBDA - Hankel transform variable
!     NLYR - number of layers
!    KSQLD - propagation constants
!     THKD - thickness of overburden
!  EHRI(J) - accumulated complex integrals for inverse Hankel transform
!     JUMP - keep integrating if false.
!
!---------------------------------------------------------------------------

 USE SA_Filter_Coefficients_q
 IMPLICIT NONE
 COMPLEX(KIND=QL), PARAMETER :: ONE=(1._QL,0._QL)
 INTEGER L,NLYR,J
 REAL (KIND=QL)THKD,ZRFD,LMBDA,RMUD(NLYR),RMUSQ(NLYR)
 COMPLEX (KIND=QL) LMBSQ,KER(JNLO:JNHI,3),KSQLD(NLYR), &
                   S0,T0,G2,SS(NLYR),E1(NLYR),T(NLYR),F1(NLYR)

 LMBSQ = CMPLX (LMBDA * LMBDA, 0.)
 S0 = CMPLX (LMBDA, 0.)
 DO J = NLYR, 1, -1
   RMUSQ(J) = RMUD(J) * RMUD(J)
   SS(J) = SQRT(LMBSQ+KSQLD(J))
   IF (J == NLYR) CYCLE
   E1(J) = EXP(-2.D0*SS(J)*THKD)
   T(J) = ((RMUSQ(J+1)-RMUSQ(J))*LMBSQ + RMUSQ(J+1)*KSQLD(J) - RMUSQ(J)* KSQLD(J+1))  &
         / (RMUD(J+1)*SS(J) + RMUD(J)*SS(J+1))**2
 END DO
 T0 = ((RMUSQ(1)-1._QL)*LMBSQ - KSQLD(1) ) / (RMUD(1)*S0 + SS(1))**2
 IF (THKD < 0.1_QL )  THEN
    G2 = LMBDA*EXP(-LMBDA*ZRFD) * T0
 ELSE
    F1 = (0._QL,0._QL)
    DO J = NLYR-1, 1, -1
       F1(J) = E1(J) * (T(J)+F1(J+1) ) / (ONE+T(J)*F1(J+1))
    END DO
    G2 = LMBDA*EXP(-LMBDA*ZRFD) * (T0+F1(1))/(ONE+T0*F1(1))
 END IF
 KER(L,1) = G2 * LMBDA 
 KER(L,2) = G2 
 KER(L,3) = G2 * LMBDA

 END SUBROUTINE BGTVAL

!===========================================================================

 SUBROUTINE BASEP (GEH,BPR,RR,MR,ZUNIQ,IZMAX,IBASE,ZP,ZR,C,NLYR,KSQR, & 
                   THICK,RMU)

!---------------------------------------------------------------------------
!
!*** Called by EPEDGE
!*** Calls CUBSPL, PGTHNK
!
!  Compute the secondary dyadic convolution integrals.
!
!    FUN - external function
!    GEH - array for the indexing scheme
!    BPR - array of splines
!     RR - distance between source and observation point
!     MR - total number of spline-nodes
!  ZUNIQ - array of diff. z-locations  
!  IBASE - number for the indexing scheme
!      C - coefficients of Green's tensor kernel
!
!---------------------------------------------------------------------------

 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 INTEGER K,L,MR,IMR,NLYR,IBASE,IFOUND,IZPOS,JR,IZMAX,J1(3),J2(3),J1A,J2A
 REAL THICK,ZLOG,ZP,ZR,BR,FR1,FR2,RR
 REAL, DIMENSION (NLYR) :: RMU
 REAL, DIMENSION (4,MR) :: GR1,GI1,GR2,GI2,GR3,GI3
 COMPLEX KSQR(2),C(3)
 COMPLEX (KIND=QL) EHRI(MR,3)
 REAL BPR(100),CF(4),CF1(4),ZUNIQ(IZMAX),GEH(24,MR,IZMAX)
 DATA J1/0,8,16/, J2/4,12,20/

 ZLOG = ABS(ZP)
 IFOUND = 0                               ! Check ZP in the index.
 IF (IBASE > 0)   THEN
    DO IZPOS = IBASE, 1, -1
       IF (ABS (ZLOG-ZUNIQ(IZPOS)) < 1.) THEN
          IFOUND = 1 ; EXIT
       END IF
    END DO
 ENDIF
 IF (IFOUND == 0) THEN                    ! New entry.
   IBASE = IBASE + 1
   ZUNIQ(IBASE) = ZLOG
   IZPOS = IBASE

   GR1 = 0; GR2 = 0; GR3 = 0
   GI1 = 0; GI2 = 0; GI3 = 0
   CALL PGTHNK (MR,BPR,NLYR,RMU,KSQR,THICK,ZP,ZR,EHRI)
   DO JR = 1, MR
     GR1(1,JR) = REAL (REAL (EHRI(JR,1)),4)
     GI1(1,JR) = REAL (AIMAG(EHRI(JR,1)),4)
     GR2(1,JR) = REAL (REAL (EHRI(JR,2)),4)
     GI2(1,JR) = REAL (AIMAG(EHRI(JR,2)),4)
     GR3(1,JR) = REAL (REAL (EHRI(JR,3)),4)
     GI3(1,JR) = REAL (AIMAG(EHRI(JR,3)),4)
   END DO
   CALL CUBSPL (BPR,GR1,MR,0,0)
   CALL CUBSPL (BPR,GI1,MR,0,0)
   CALL CUBSPL (BPR,GR2,MR,0,0)
   CALL CUBSPL (BPR,GI2,MR,0,0)
   CALL CUBSPL (BPR,GR3,MR,0,0)
   CALL CUBSPL (BPR,GI3,MR,0,0)
   GEH( 1: 4, 1:MR, IBASE) = GR1(1:4, 1:MR)
   GEH( 5: 8, 1:MR, IBASE) = GI1(1:4, 1:MR)
   GEH( 9:12, 1:MR, IBASE) = GR2(1:4, 1:MR)
   GEH(13:16, 1:MR, IBASE) = GI2(1:4, 1:MR)
   GEH(17:20, 1:MR, IBASE) = GR3(1:4, 1:MR)
   GEH(21:24, 1:MR, IBASE) = GI3(1:4, 1:MR)
 END IF
 DO IMR = 1, MR                           ! Read the values.
   IF (BPR(IMR) > RR)   THEN
     BR = RR - BPR(IMR-1)
     DO L = 1, 3
       DO K = 1, 4
         J1A = J1(L)+K
         J2A = J2(L)+K
         CF (K) = GEH (J1A,IMR-1,IZPOS)
         CF1(K) = GEH (J2A,IMR-1,IZPOS)
       END DO
       FR1 = ((CF (4)*BR/3 + CF (3))*BR/2 + CF (2))*BR + CF (1)
       FR2 = ((CF1(4)*BR/3 + CF1(3))*BR/2 + CF1(2))*BR + CF1(1)
       C(L) = CMPLX(FR1,FR2)
     END DO
     EXIT
   END IF
 END DO

 END SUBROUTINE BASEP

!===========================================================================

 SUBROUTINE PGTHNK (NRHO,RHOTRP,NLYR,RMU,KSQ_LYR,THICK,ZP,ZS,EHRI)

!---------------------------------------------------------------------------
!
!***  Called by BASEP
!***  Calls PGTVAL
!
! Sets up the five integrals EHRI, needed to compute the electric Green's
! tensor elements in EFIELD.  It uses the flow through Hankel transform
! method to compute values at RHOTRP(JR), (15 point per decade spacing).
! It uses a 15 point per decade filter coefficient set derived from
! Christensen's program, FLTGEN.
!
!   RHOTRP - interpolation array of rho values
!     NRHO - number of interpolation points needed for EGT
!     NLYR - number of layers
!  KSQ_LYR - propagation constants
!    THICK - thickness of overburden
!      ZTR = the reflected distance between "source" & "receiver"
!            points off bottom of overburden.
!     EHRI - inverse Hankel transform
!
!---------------------------------------------------------------------------

 USE SA_Filter_Coefficients_q
 IMPLICIT NONE
 REAL(KIND=QL), PARAMETER :: FOURPI=12.56637_QL
 COMPLEX(KIND=QL), PARAMETER :: ZERO=(0._QL, 0._QL)
 INTEGER NLYR,NRHO,L,JR,LMAX,LMIN,K,KMAX,KMIN,KBOT,K1
 REAL RHOTRP(NRHO),THICK,RMU(NLYR),ZP,ZS
 REAL(KIND=QL) DELTA,Y1,Y,RD,RMUD(NLYR),THKD,LMBDA,ZPD,ZSD,RHOD
 COMPLEX KSQ_LYR(NLYR)
 COMPLEX(KIND=QL) KSQLD(NLYR),KER(JNLO-NRHO:JNHI,3),EHRI(NRHO,3)
 LOGICAL JUMP

 RMUD = REAL (RMU,QL)
 THKD = REAL (THICK,QL)
 KSQLD = CMPLX (KSQ_LYR,KIND=QL)
 ZPD = REAL (ZP,QL)
 ZSD = REAL (ZS,QL)

 EHRI = ZERO
 KER = ZERO
 DELTA = LOG (10._QL)/ REAL (NDEC_JN,QL)

 ! Set up KER for JR = 1 corresponding to minimum value of RHO.  This will
 ! compute most of the needed kernel range from the high end.  Note that
 ! the filter is defined between JNLO < L < JNHI

 JR = 1
 RD = REAL (RHOTRP(1),QL)
 Y1 = -LOG (RD) - DBLE (JR-1) * DELTA -  SHFTJN

 DO L = -50, JNHI             ! Start at L = -50 to pick up low values.
   LMAX = L                   ! Maximum filter index used
   K = L + 1 - JR             ! K is the kernel index.
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
   CALL PGTVAL (NRHO,K,JR,L,LMBDA,NLYR,RMUD,KSQLD,THKD,ZPD,ZSD,KER,EHRI,JUMP)
   IF (JUMP) EXIT
 END DO

 Y = Y1                       ! Finish off the low end for RHOTRP(1)
 DO L = -51, JNLO, -1
   LMIN = L                   ! Minimum filter index used
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
   K = L + 1 - JR             ! Compute the kernel index.
   CALL PGTVAL (NRHO,K,JR,L,LMBDA,NLYR,RMUD,KSQLD,THKD,ZPD,ZSD,KER,EHRI,JUMP)
   IF (JUMP) EXIT
 END DO

 KMIN = LMIN + 1 - JR  !  Define the range of kernel values
 KMAX = LMAX + 1 - JR  !  used for RHOTRP(1)

 ! Complete definition of kernel values by evaluating transform of
 ! maximum RHO = RHOTRP (NRHO)

 JR = NRHO
 Y1 = -LOG (RD) - DBLE (JR-1) * DELTA - SHFTJN
 KBOT = JNLO + 1 - JR
 K1 = MAX (KBOT,KMIN)

 DO K = K1, KMAX          ! Compute EHR for maximum RHO using previously
   L = K - 1 + JR         ! computed kernel values.
   IF (L > JNHI .OR. L < JNLO) CYCLE
     EHRI(JR,1) = EHRI(JR,1) + KER(K,1) * WJ1(L)
     EHRI(JR,2) = EHRI(JR,2) + KER(K,2) * WJ0(L)
     EHRI(JR,3) = EHRI(JR,3) + KER(K,3) * WJ1(L)
 END DO

 IF (K1 > KBOT) THEN    !  Add low end kernel values until convergence.
   DO K = K1-1, KBOT, -1
     KMIN = K
     L = K - 1 + JR
     Y = Y1 + DBLE (L) * DELTA
     LMBDA = EXP (Y)
     CALL PGTVAL (NRHO,K,JR,L,LMBDA,NLYR,RMUD,KSQLD,THKD,ZPD,ZSD,KER,EHRI,JUMP)
     IF (JUMP) EXIT
   END DO
 END IF

 DO JR = 2, NRHO-1          !  Compute transforms for all other RHO values
   DO K = KMIN, KMAX         !  using previously computed kernel values.
     L = K - 1 + JR
     IF (L > JNHI .OR. L < JNLO) CYCLE
     EHRI(JR,1) = EHRI(JR,1) + KER(K,1) * WJ1(L)
     EHRI(JR,2) = EHRI(JR,2) + KER(K,2) * WJ0(L)
     EHRI(JR,3) = EHRI(JR,3) + KER(K,3) * WJ1(L)
   END DO
 END DO

 DO JR = 1,NRHO
   RHOD = REAL (RHOTRP(JR),KIND=QL)
   EHRI(JR,3) = EHRI(JR,3) / RHOD
   EHRI(JR,1:3) = EHRI(JR,1:3) / (FOURPI * RHOD)
 END DO

 END SUBROUTINE PGTHNK

!===========================================================================

 SUBROUTINE PGTVAL (NRHO,K,JR,L,LMBDA,NLYR,RMUD,KSQLD,THKD,ZPD,ZSD, &
                    KER,EHRI,JUMP)

!---------------------------------------------------------------------------
!
!***  Called by PGTHNK
!
! Accumulates the integrals for EHRI, the five inverse Hankel transforms
! needed for evaluation of Green's tensor elements
!
!     NRHO - number of points in 15 point / decade array
!        K - filter kernel index
!       JR - RHO interpolation index
!        L - filter point index
!    LMBDA - Hankel transform variable
!     NLYR - number of layers
!    KSQLD - propagation constants
!     THKD - thickness of overburden
!     EHRI - accumulated complex integrals for inverse Hankel transform
!     JUMP - keep integrating if false.
!
!---------------------------------------------------------------------------

 USE SA_Filter_Coefficients_q
 IMPLICIT NONE
 REAL, PARAMETER :: TOL=1.E-6
 COMPLEX(KIND=QL), PARAMETER :: ONE=(1._QL,0._QL)
 INTEGER NRHO,K,JR,L,NLYR,J1
 REAL (KIND=QL)THKD,ZPD,ZSD,LMBDA,EHR,EHI,AR,AI,RMUD(NLYR),RMU1S, &
       RMU2,RMU2S
 COMPLEX (KIND=QL) LMBSQ,EHRI(NRHO,3),KER(JNLO-NRHO:JNHI,3),KSQLD(NLYR), &
                   S0,S1,S2,T0,T1,G2,TXP1,TMP(3),FAC
 LOGICAL TOO_BIG,JUMP

 RMU1S = RMUD(1) * RMUD(1)
 LMBSQ = CMPLX (LMBDA * LMBDA, 0.)
 S0 = CMPLX (LMBDA, 0.)
 S1 = SQRT (LMBSQ + KSQLD(1))
 S2 = SQRT (LMBSQ + KSQLD(NLYR))
 T0 = ((RMU1S - 1._QL)*LMBSQ  - KSQLD(1)) / (RMUD(1)*S0 + S1)**2

 IF (ABS(THKD) < 1.E-4_QL)  THEN
   IF (ZPD <  0._QL) G2 = EXP( S0*ZPD-S0*ZSD)*(ONE+T0)
   IF (ZPD >= 0._QL) G2 = EXP(-S1*ZPD-S0*ZSD)*(ONE+T0) 
 ELSE
   RMU2 = RMUD(2) / RMUD(1)
   RMU2S = RMU2*RMU2
   T1 = (KSQLD(1) - KSQLD(2) + (RMU2S - 1._QL)*S1**2 ) / (RMU2 * S1 + S2)**2
   TXP1 = EXP (-2.* THKD *S1)
   FAC= EXP(-S0*ZSD)*(ONE+T0)/(ONE+T0*T1*TXP1) 
   IF (ZPD < THKD)  THEN
     G2 = FAC * T1*EXP(-S1*(2.*THKD-ZPD)) + EXP(-S1*ZPD)
   ELSE
     G2 = FAC * (ONE+T1) * EXP(-S2*(ZPD-THKD)-S1*THKD)  
   END IF
 END IF

 KER(K,1) = G2 * LMBDA 
 KER(K,2) = G2 * LMBDA 
 KER(K,3) = G2 

 TMP(1) =  WJ1(L) * KER(K,1)
 TMP(2) =  WJ0(L) * KER(K,2)
 TMP(3) =  WJ1(L) * KER(K,3)

 JUMP = .TRUE.
 DO J1 = 1,3
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

 END SUBROUTINE PGTVAL

!===========================================================================!
!                                                                           !
! Structure of the inversion subroutines                                    !
! --------------------------------------                                    !
!                                                                           !
! SamAir can solve for either the over-determined (NDATA > NPAR) or the     !
! under-determined (NDATA < NPAR) inverse problem. This makes no difference !
! to the way the Jacobian matrix is constructed. It does make a difference  !
! however to the way the Jacobian matrix is decomposed via S.V.D.           !
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
! For SamAir inversion, RESJAC computes the residual vector using the       !
! SamAir forward modelling algorithm, and computes the Jacobian matrix      !
! using the adjoint operator method.                                        !
!                                                                           !
!===========================================================================!

 SUBROUTINE NLSQ (NM,ND,NW,np,NS,MV1PRT,OUTPRT,MAXITS,CNVRG,PCTCNV,NDATA, &
                  XDATA,XMODL,XWTS,NPAR,CXPAR,ELAS,LBND,UBND,TDFD,CMP, &
                  STEP,IDER,FANGLE,NSX,SWX,SWY,NTYRP,TRP,NPULS,PULSE,NTYPLS, &
                  MCHNL,NCHNL,TOPN,TCLS,GSTRP,ASTRP,NFRQ,FREQ,TXCLN,TXA90, &
                  NRX,NRXST,NSTAT,SX,SY,SZ,RX,RY,RZ,NN,NE,NZ,NLITH,NPROP, &
                  LITH,LITHL,LYTH,SOLVER,NLYR,THK,ELOC,NLOC,ZLOC,KNRM,NORM, &
                  BFFAC,PPFAC,KPPM,PRM_FD,PRM_TD,SXD,SYD,RXD,RYD,LINE,NTXD, &
                  NPART,XPART,DNORM,INRM,NEL,NER,NNL,NNR,NZT,NZB)

!----------------------------------------------------------------------------
!
!*** Called by: MAIN
!*** Calls: RESJAC, ESVD, SOLVE, WRYT_MODEL, WRYT_MISFIT
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
 CHARACTER (LEN = 120) CVAR
 LOGICAL,PARAMETER :: WITHU = .TRUE., WITHV = .TRUE.
 INTEGER, PARAMETER :: IP = 0, QL = SELECTED_REAL_KIND(p = 18)
 REAL, PARAMETER :: BND = 0.01, EXPND = 2., RSVT0 = 0.1, ETA = 1.E-7, &
                    TOL=.5E-31
 INTEGER NM,NW,np,NS,ND,MV1PRT,OUTPRT,ITS,CNVRG,NDATA,NPAR,TDFD,CMP,STEP, &
         IDER,NSX,NPULS,NTYPLS,NTYRP,NCHNL,MCHNL,GSTRP,ASTRP,NFRQ,NSTAT,NRXST, &
         NRX,ICNT,MAXITS,NSV,JP,KLITH,NLITH,CXPAR(NLITH),MODFIX(NPAR),NN,NE,NZ, & 
         LITH(NE,NN,NZ),NPROP,I,J,K,NCOMP,KNRM,KPPM,FITS,K0,LINE(NSTAT),NBN, &
         NBNT,NTXD,NLYR,SOLVER,LITHL(NLYR),NPART,MAXITS_INT,INRM,NEL,NER,NNL, &
         NNR,NZT,NZB
 LOGICAL JCBN,RESID,FINAL,TXA90
 REAL PCTCNV,SUMSQ(2),SSQNEW(2),PSERR(2),FNM,GTEST,PDRE,WGTSUM,PULSE,DELT,RSVT, & 
      ZERO,NSR,SWX(NSX),SWY(NSX,3),TRP(NTYRP),FREQ(NFRQ),B1,B2,LYTH(NLITH+1,NPROP), & 
      NORM(KNRM),PPFAC,BFFAC,PRM_TD(3),PRM_FD(NFRQ),DRMS,RMSE,MP,THK(NLYR),GCRIT
 REAL, DIMENSION(NDATA) :: XDATA,XMODL,RES,DNORM
 INTEGER, DIMENSION(NDATA) :: XWTS
 REAL, DIMENSION(NPAR)  :: XPAR,DELPAR,GXPAR,IMPORT,MODWGT,MUBND,MLBND
 REAL, DIMENSION(NPART) :: XPART,GXPART
 REAL, DIMENSION(NCHNL) :: TOPN,TCLS
 REAL, DIMENSION(NLITH) :: ELAS,LBND,UBND
 REAL, DIMENSION(NSTAT) :: SX,SY,SZ,SZ0,RZ0,FANGLE
 REAL, DIMENSION(NRXST) :: TXCLN
 REAL, DIMENSION(NDATA,NPAR) :: JAC
 REAL, DIMENSION(NSTAT,NRX) :: RX,RY,RZ
 REAL, DIMENSION(NE,NN,NZ) :: ELOC,NLOC,ZLOC
 REAL, ALLOCATABLE, DIMENSION(:) :: SV,WSP,RMSERR,UTRES
 REAL, ALLOCATABLE, DIMENSION(:,:) :: UMAT,VMAT,JACT,UT
 REAL(KIND=QL), DIMENSION(NSTAT) :: SXD,SYD
 REAL(KIND=QL), DIMENSION(NSTAT,NRX) :: RXD,RYD
 CHARACTER(LEN=11) CERR(2)
 DATA CERR /'Point-norm ', 'Survey-norm'/

 IF (TDFD == 2) THEN
   WRITE(NW,1) ; WRITE(*,1)
 ELSE
   IF (CMP == 11) CVAR = 'In-line component'
   IF (CMP == 13) CVAR = 'Vertical component'
   IF (CMP == 2)  CVAR = 'Joint vertical and in-line components'
   IF (CMP == 3)  CVAR = 'Joint vertical, in-line and transverse components'
   WRITE(NW,2) TRIM (CVAR) ; WRITE(*,2) TRIM (CVAR)
 END IF
 WRITE(NW,3) MAXITS ; WRITE(*,3) MAXITS

 ! Determine the number of Rx components. This will determine how many
 ! NSTAT models will have to be computed for sensitivity computation.

 IF (TDFD < 2) THEN 
   IF (CMP == 11) NCOMP = 1  ! X
   IF (CMP == 13) NCOMP = 1  ! Z
   IF (CMP ==  2) NCOMP = 2  ! X, Z
   IF (CMP ==  3) NCOMP = 3  ! Z, X, Y
 ELSE IF (TDFD == 2) THEN
   NCOMP = 1
 END IF

 IF (INRM == 1) THEN
   WRITE(NW,25); WRITE(*,25)
 ELSE 
   WRITE(NW,26); WRITE(*,26)
 END IF

 ! Set up the model parameter, weight and bounds vectors.

 DO J = NNL+1, NN-NNR-1
   DO K = NZT+1, NZ-NZB-1
     DO I = NEL+1, NE-NER-1
       NBN  = (J-NNL-1)*(NZ-NZT-NZB-1)*(NE-NEL-NER-1) + (K-NZT-1)*(NE-NEL-NER-1) + (I-NEL)   ! User domain.
       NBNT = (J    -1)*(NZ        -1)*(NE        -1) + (K    -1)*(NE        -1) +  I        ! Total domain.
       KLITH = LITH(I,J,K)
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
 WGTSUM = REAL(SUM(XWTS)) ; ITS = 0 ; MAXITS_INT = 5 ; DRMS = 0.

 ALLOCATE(RMSERR(MAXITS)) ; RMSERR = 0. 

 !------------------------------!
 ! START OF MAIN INVERSION LOOP !
 !------------------------------!

 INV_LOOP: DO ITS = 1, MAXITS

   WRITE(*,'(/T3,A,I3)') 'Begin iteration =',ITS

   WRITE(*,45) ! Compute the residual error vector (RES) and Jacobian matrix (JAC).

   JCBN = .TRUE. ; RESID = .TRUE. ; RES = 0. ; JAC = 0. ; SUMSQ = 0. 

   CALL RESJAC (ND,NDATA,XDATA,XMODL,XWTS,NPAR,XPAR,SUMSQ,INRM,JCBN,JAC, &
                RES,TDFD,STEP,IDER,NSX,SWX,SWY,NTYRP,TRP,NPULS,PULSE, &
                NTYPLS,NCHNL,TOPN,TCLS,GSTRP,ASTRP,NFRQ,FREQ,TXCLN,CMP, &
                TXA90,NRX,NRXST,NSTAT,SX,SY,SZ,FANGLE,RX,RY,RZ,LITH,LYTH, &
                NLITH,NPROP,NLYR,LITHL,THK,NS,SOLVER,NE,NN,NZ,ELOC,NLOC, &
                ZLOC,NTXD,NCOMP,KNRM,NORM,RESID,BFFAC,PPFAC,KPPM,PRM_FD, &
                PRM_TD,NPART,XPART,DNORM,NEL,NER,NNL,NNR,NZT,NZB)

   PSERR(1:2) = 100. * SQRT(SUMSQ(1:2)/WGTSUM)
   RMSERR(ITS) = PSERR(INRM)
   FNM = 0.01 * SQRT(SUMSQ(INRM))
   FNM = MAX(FNM,ETA)

   IF (ITS == 1) THEN
     WRITE(NW,4) TRIM (CERR(INRM)),PSERR(INRM),RSVT ; WRITE(*,4) TRIM (CERR(INRM)),PSERR(INRM),RSVT
     FITS = 0
     IF (MV1PRT > 1) THEN
       WRITE(np,30) FITS,RMSERR(ITS),RSVT
     END IF
     IF (MV1PRT == 3) CALL WRITE_MDATA(FITS)
     IF (OUTPRT == 3)  &
       CALL WRITE_MISFIT (NW,FITS,NSTAT,NDATA,MCHNL,TDFD,NFRQ,FREQ,CMP,NCHNL, & 
                          RES,XMODL,XDATA)
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
         WRITE(NW,6) ITS,TRIM (CERR(INRM)),PSERR(INRM) ; WRITE(*,6) ITS,TRIM (CERR(INRM)),PSERR(INRM)
         IF (CNVRG == 1 .AND. RMSERR(ITS) > 15.)    WRITE(NW,7)
         IF (CNVRG == 2 .AND. RMSERR(ITS) > PCTCNV) WRITE(NW,8) PCTCNV
         EXIT INV_LOOP
       END IF

       ! Get the error for model with corrected parameters. Test for improvement
       ! (decrease) in residual. If it fails, reduce step and try again. Give up
       ! and return after MAXIT "internal" iterations.

       JCBN = .FALSE. ; RESID = .TRUE. ; RES = 0. ; JAC = 0. ; SSQNEW = 0. 

       CALL RESJAC (ND,NDATA,XDATA,XMODL,XWTS,NPAR,GXPAR,SSQNEW,INRM,JCBN,JAC, &
                    RES,TDFD,STEP,IDER,NSX,SWX,SWY,NTYRP,TRP,NPULS,PULSE, &
                    NTYPLS,NCHNL,TOPN,TCLS,GSTRP,ASTRP,NFRQ,FREQ,TXCLN,CMP, &
                    TXA90,NRX,NRXST,NSTAT,SX,SY,SZ,FANGLE,RX,RY,RZ,LITH,LYTH, &
                    NLITH,NPROP,NLYR,LITHL,THK,NS,SOLVER,NE,NN,NZ,ELOC,NLOC, &
                    ZLOC,NTXD,NCOMP,KNRM,NORM,RESID,BFFAC,PPFAC,KPPM,PRM_FD, &
                    PRM_TD,NPART,GXPART,DNORM,NEL,NER,NNL,NNR,NZT,NZB)

       PSERR(1:2) = 100. * SQRT(SSQNEW(1:2)/WGTSUM)
       RMSE = PSERR(INRM)
       IF (ICNT == 0) THEN
         WRITE(*,16)  ITS,ICNT,RSVT,RMSE
         WRITE(NW,16) ITS,ICNT,RSVT,RMSE
       ELSE
         WRITE(*,17)  ICNT,RSVT,RMSE
         WRITE(NW,17) ICNT,RSVT,RMSE
       END IF

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
         WRITE(NW,6) ITS,TRIM (CERR(INRM)),PSERR(INRM),RSVT ; WRITE(*,6) ITS,TRIM (CERR(INRM)),PSERR(INRM),RSVT
         IF (CNVRG == 1 .AND. RMSERR(ITS) > 15.)    WRITE(NW,7)
         IF (CNVRG == 2 .AND. RMSERR(ITS) > PCTCNV) WRITE(NW,8) PCTCNV
         EXIT INV_LOOP
       END IF

       ! Get the error for model with corrected parameters. Test for improvement
       ! (decrease) in residual. If it fails, reduce step and try again. Give up
       ! and return after MAXIT "internal" iterations.

       JCBN = .FALSE. ; RESID = .TRUE. ; RES = 0. ; JAC = 0. ; SSQNEW = 0. 

       CALL RESJAC (ND,NDATA,XDATA,XMODL,XWTS,NPAR,GXPAR,SSQNEW,INRM,JCBN,JAC, &
                    RES,TDFD,STEP,IDER,NSX,SWX,SWY,NTYRP,TRP,NPULS,PULSE, &
                    NTYPLS,NCHNL,TOPN,TCLS,GSTRP,ASTRP,NFRQ,FREQ,TXCLN,CMP, &
                    TXA90,NRX,NRXST,NSTAT,SX,SY,SZ,FANGLE,RX,RY,RZ,LITH,LYTH, &
                    NLITH,NPROP,NLYR,LITHL,THK,NS,SOLVER,NE,NN,NZ,ELOC,NLOC, &
                    ZLOC,NTXD,NCOMP,KNRM,NORM,RESID,BFFAC,PPFAC,KPPM,PRM_FD, &
                    PRM_TD,NPART,GXPART,DNORM,NEL,NER,NNL,NNR,NZT,NZB)

       PSERR(1:2) = 100. * SQRT(SSQNEW(1:2)/WGTSUM)
       RMSE = PSERR(INRM)
       IF (ICNT == 0) THEN
         WRITE(*,16)  ITS,ICNT,RSVT,RMSE
         WRITE(NW,16) ITS,ICNT,RSVT,RMSE
       ELSE
         WRITE(*,17)  ICNT,RSVT,RMSE
         WRITE(NW,17) ICNT,RSVT,RMSE
       END IF

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

   ! Write inversion result to SamayaAir.res

   OPEN(NM,FILE='SamayaAir.res',STATUS='REPLACE')
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
     WRITE(NW,6) ITS,TRIM (CERR(INRM)),PSERR(INRM),RSVT ; WRITE(*,6) ITS,TRIM (CERR(INRM)),PSERR(INRM),RSVT
     IF (CNVRG == 1 .AND. RMSERR(ITS) > 15.)    WRITE(NW,7)
     IF (CNVRG == 2 .AND. RMSERR(ITS) > PCTCNV) WRITE(NW,8) PCTCNV
     EXIT INV_LOOP
   END IF
   IF (CNVRG == 2 .AND. RMSERR(ITS) < PCTCNV) THEN
     WRITE(NW,10) PCTCNV
     WRITE(NW,6) ITS,TRIM (CERR(INRM)),PSERR(INRM),RSVT ; WRITE(*,6) ITS,TRIM (CERR(INRM)),PSERR(INRM),RSVT
     EXIT INV_LOOP
   END IF

   IF (ITS > 3) DRMS = RMSERR(ITS-2) - RMSERR(ITS) 
   IF (ITS == MAXITS) THEN
     WRITE(NW,11)
     WRITE(NW,6) ITS,TRIM (CERR(INRM)),PSERR(INRM),RSVT ; WRITE(*,6) ITS,TRIM (CERR(INRM)),PSERR(INRM),RSVT
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

   WRITE(NW,6) ITS,TRIM (CERR(INRM)),PSERR(INRM),RSVT ; WRITE(*,6) ITS,TRIM (CERR(INRM)),PSERR(INRM),RSVT

   IF (OUTPRT > 1) CALL WRITE_INVMDL (NW,FINAL,ITS,NN,NE,NZ,NPAR,XPAR,IMPORT, &
                                      NEL,NER,NNL,NNR,NZT,NZB)

   IF (OUTPRT == 3 .AND. ITS < MAXITS) &
     CALL WRITE_MISFIT (NW,ITS,NSTAT,NDATA,MCHNL,TDFD,NFRQ,FREQ,CMP, & 
                          NCHNL,RES,XMODL,XDATA)

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
 WRITE(NW,13) TRIM (CERR(INRM)),PSERR(INRM),NSR

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
 CALL WRITE_INVMDL (NW,FINAL,ITS,NN,NE,NZ,NPAR,XPAR,IMPORT,NEL,NER,NNL,NNR, & 
                    NZT,NZB)

 WRITE(np,35)
 WRITE(np,33) XPAR(1:NPAR) 
 WRITE(np,34) IMPORT(1:NPAR)
 IF (MV1PRT > 0) CALL WRITE_MDATA(FITS)

 !---------------------------------!
 ! Write the final misfit to file. !
 !---------------------------------!

 CALL WRITE_MISFIT (NW,ITS,NSTAT,NDATA,MCHNL,TDFD,NFRQ,FREQ,CMP,NCHNL, & 
                    RES,XMODL,XDATA)

 DEALLOCATE (RMSERR)

 !-------------------!
 ! Print statements. !
 !-------------------!

 1  FORMAT(/T3,'Begin frequency-domain inversion.')
 2  FORMAT(/T3,'Begin time-domain inversion on ',A)
 3  FORMAT(/T3,'Maximum iterations =',I3)
 4  FORMAT(/T3,'Initial ',A,' RMS error  =',F8.2,' percent.' &
           /T3,'Initial Relative SV threshold =',F8.3)
 5  FORMAT(//T3,'Convergence on predicted decrease.')
 6  FORMAT(/I4,' iterations completed.' &
           /T6, A,' RMS error  =',F8.2,' percent.' &
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
 13 FORMAT(/T23,A,' RMS error  =',F8.2,' percent.' &
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
 16 FORMAT(/T3,'Iteration:',I3,T19,'ICNT:',I3,3X,'RSVT:',F8.3,3X,'Test RMS error =',F9.2,' percent.')
 17 FORMAT(/T3, T19,'ICNT:',I3,3X,'RSVT:',F8.3,3X,'Test RMS error =',F9.2,' percent.')
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
 45 FORMAT(/T3,'Computing forward models for primal and adjoint fields:')
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

     DO J = NNL+1, NN-NNR-1
       DO K = NZT+1, NZ-NZB-1
         DO I = NEL+1, NE-NER-1
           NBN  = (J-NNL-1)*(NZ-NZT-NZB-1)*(NE-NEL-NER-1) + (K-NZT-1)*(NE-NEL-NER-1) + (I-NEL)   ! User domain.
           NBNT = (J    -1)*(NZ        -1)*(NE        -1) + (K    -1)*(NE        -1) +  I        ! Total domain.
           GXPART(NBNT) = GXPAR(NBN)                           ! Resistivity vector.
         END DO
       END DO
     END DO

   END SUBROUTINE UPDATE_MODEL_PARAMETERS

   SUBROUTINE WRITE_MDATA(KTS)

     REAL, PARAMETER :: RAD2DEG=180./3.141592654
     REAL, DIMENSION(NRXST) :: TXDEG
     INTEGER JS,KTS
     LOGICAL WL
     CHARACTER (LEN=80) QL0,QL1

     TXDEG = RAD2DEG * TXCLN
     SZ0 = SZ 

     DO JS = 1,NSTAT
       RZ0(JS) = RZ(JS,1) 
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
       WRITE(np,3) TRIM (ADJUSTL (QL1))
       END IF
       IF (TDFD < 2) THEN
         WRITE(np,4) JS,SYD(JS),SXD(JS),SZ0(JS),TXDEG(JS),RYD(JS,1),RXD(JS,1),RZ0(JS), &
                      XMODL(K0+1:K0+MCHNL),100.*RES(K0+1:K0+MCHNL)
       ELSE
         WRITE(np,5) JS,SYD(JS),SXD(JS),SZ0(JS),RYD(JS,1),RXD(JS,1),RZ0(JS), &
                      XMODL(K0+1:K0+MCHNL),100.*RES(K0+1:K0+MCHNL)
       END IF
     END DO

     1 FORMAT(T2,I10,'_I',I2.2)
     2 FORMAT(T2,I10,'_ZFNL')
     3 FORMAT(T2,'Line ',A)
     4 FORMAT(I5,2F12.1,F8.1,F6.1,2F12.1,F8.1,300G13.4)
     5 FORMAT(I5,2F12.1,F8.1,2F12.1,F8.1,300G13.4)

   END SUBROUTINE WRITE_MDATA

 END SUBROUTINE NLSQ       

!============================================================================

 SUBROUTINE RESJAC (ND,NDATA,XDATA,XMODL,XWTS,NPAR,XPAR,SUMSQ,INRM,JCBN,JAC, &
                    RES,TDFD,STEP,IDER,NSX,SWX,SWY,NTYRP,TRP,NPULS,PULSE, &
                    NTYPLS,NCHNL,TOPN,TCLS,GSTRP,ASTRP,NFRQ,FREQ,TXCLN,CMP, &
                    TXA90,NRX,NRXST,NSTAT,SX,SY,SZ,FANGLE,RX,RY,RZ,LITH,LYTH, &
                    NLITH,NPROP,NLYR,LITHL,THK,NS,SOLVER,NE,NN,NZ,ELOC,NLOC, &
                    ZLOC,NTXD,NCOMP,KNRM,NORM,RESID,BFFAC,PPFAC,KPPM,PRM_FD, &
                    PRM_TD,NPART,XPART,DNORM,NEL,NER,NNL,NNR,NZT,NZB)

!----------------------------------------------------------------------------
!
!  Construct the residual error vector (RES) and Jacobian matrix (JAC).
!
!----------------------------------------------------------------------------
!
!  DESCRIPTION:
!
!  Convention: Residual error = Observed data - Predicted data
!
!  In NLSQ, DELPAR will thus be added rather than subtracted during updates
!  of the model parameters.
!
!  The sensitivities computed by the reciprocity method in S_FD_CONSTRUCT
!  are the dD/dM with units of nTm/S. These have to be converted to
!  calibrated units for each system, which is done respectively in the
!  subroutines TD_SENS and FD_SENS.
!
!  The Jacobian matrix is then converted to log base. This means that
!  by the chain rule, we get:
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
 INTEGER NS,NDATA,NPAR,TDFD,STEP,IDER,NSX,NPULS,NTYPLS,NTYRP,NCHNL,GSTRP,ASTRP,NFRQ,NSTAT, & 
         NRXST,NRX,NLITH,NE,NN,NZ,LITH(NE,NN,NZ),NPROP,JD,JP,KPPM,NCOMP,KNRM,NTX,ND,NTXD,  &
         SOLVER,NLYR,LITHL(NLYR),CMP,NPART,INRM,NEL,NER,NNL,NNR,NZT,NZB,IPR
 LOGICAL JCBN,RESID,TXA90
 REAL SUMSQ(2),PULSE,SWX(NSX),SWY(NSX,3),TRP(NTYRP),FREQ(NFRQ),LYTH(NLITH+1,NPROP), & 
      BFFAC,PPFAC,VD,VM,RES_ERR,PRM_FD(NFRQ),PRM_TD(3),NORM(KNRM),THK(NLYR), & 
      XPART(NPART),DENOM,V1,V2
 REAL, DIMENSION(NE,NN,NZ) :: ELOC,NLOC,ZLOC
 REAL, DIMENSION(NDATA) :: XDATA,XMODL,RES,DNORM
 INTEGER, DIMENSION(NDATA) :: XWTS
 REAL, DIMENSION(NPAR)  :: XPAR
 REAL, DIMENSION(NCHNL) :: TOPN,TCLS
 REAL, DIMENSION(NSTAT) :: SX,SY,SZ,FANGLE
 REAL, DIMENSION(NRXST) :: TXCLN
 REAL, DIMENSION(NDATA,NPAR) :: JAC
 REAL, DIMENSION(NSTAT,NRX) :: RX,RY,RZ
 COMPLEX, ALLOCATABLE :: BFD(:,:,:),SFD(:,:,:,:)

 IF (RESID) OPEN(ND,FILE = 'SamayaAir.frq',STATUS = 'REPLACE')
 IF (JCBN)  OPEN(NS,FILE = 'SamayaAir.sty',STATUS = 'REPLACE')

 ! COMPUTE FORWARD MODEL FOR PRIMAL AND/OR ADJOINT PROBLEMS
 ! --------------------------------------------------------

 IF (JCBN) THEN 
   NTX = NSTAT * (1+NCOMP)
 ELSE 
   NTX = NSTAT
 END IF
 IPR = 0
 CALL SAMAYA_3D (NFRQ,FREQ,SOLVER,NE,NN,NZ,ELOC,NLOC,ZLOC,NTXD,TXCLN, &
                 NSTAT,NTX,SX,SY,SZ,FANGLE,NRX,NCOMP,RX,RY,RZ,ND,NS, &
                 LITH,LYTH,NLITH,NPROP,NLYR,LITHL,THK,JCBN,TDFD,CMP, &
                 NPART,XPART,NEL,NER,NNL,NNR,NZT,NZB,IPR)

 ! RESIDUAL ERROR VECTOR CONSTRUCTION
 ! ----------------------------------

 IF (RESID) THEN 

   ALLOCATE (BFD(NFRQ,NSTAT,3))

   BFD = (0.,0.) 

   CALL FDREAD (ND,NFRQ,NSTAT,BFD) 

   IF (TDFD < 2) THEN ! Compute XMODL from BTD.

     CALL TD_FIELD (STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP, &
                    NCHNL,TOPN,TCLS,FREQ,NFRQ,NSTAT,BFD,GSTRP,ASTRP, &
                    XMODL,NCOMP,PRM_TD,BFFAC,PPFAC,CMP,KPPM,NDATA, &
                    KNRM,NORM)

   ELSE IF (TDFD == 2) THEN ! Compute XMODL from BFD.

     CALL FD_FIELD (XMODL,NDATA,NSTAT,NFRQ,BFD,PRM_FD,BFFAC,PPFAC, & 
                    TXCLN,NRXST,TXA90,KNRM,NORM)

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

 ! JACOBIAN MATRIX CONSTRUCTION
 ! ----------------------------

 IF (JCBN) THEN

   ALLOCATE (SFD(NFRQ,NSTAT,NCOMP,NPAR)) ; SFD = (0.,0.)

   CALL SFDREAD (NS,NFRQ,NSTAT,NCOMP,NPAR,SFD)

   JAC = 0.

   IF (TDFD < 2) THEN ! Construct the time-domain Jacobian matrix.

     CALL TD_SENS (STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP, &
                   NCHNL,TOPN,TCLS,FREQ,NFRQ,NSTAT,SFD,JAC,NPAR,NDATA, & 
                   NCOMP,PRM_TD,BFFAC,PPFAC,CMP,KPPM,KNRM,NORM,ASTRP,GSTRP)

   ELSE IF (TDFD == 2) THEN ! Construct the frequency-domain Jacobian matrix.

     CALL FD_SENS (JAC,NDATA,NPAR,NSTAT,NFRQ,SFD,PRM_FD,BFFAC,PPFAC, &
                   NCOMP,KNRM,NORM)

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

 END SUBROUTINE RESJAC

!===========================================================================

 SUBROUTINE SFDREAD (NS,NFRQ,NSTAT,NCOMP,NPAR,SFD)

!---------------------------------------------------------------------------
!
!  Reads frequency-domain sensitivity data (real and imaginary components)
!  from logical UNIT NS into array SFD.
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER NS,NFRQ,NSTAT,NCOMP,NPAR,JF,JS,JC,JP
 REAL A,B
 COMPLEX SFD(NFRQ,NSTAT,NCOMP,NPAR)

 REWIND(NS)

 DO JF = 1, NFRQ
   DO JS = 1, NSTAT
     DO JC = 1, NCOMP
       DO JP = 1, NPAR
         READ(NS,*) A,B
         SFD(JF,JS,JC,JP) = CMPLX(A,B)
       END DO
     END DO
   END DO
 END DO

 END SUBROUTINE SFDREAD

!===========================================================================

 SUBROUTINE TD_FIELD (STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,  &
                      NCHNL,TOPN,TCLS,FREQ,NFRQ,NSTAT,BFD,GSTRP,ASTRP,     &
                      XMODL,NCOMP,PRM_TD,BFFAC,PPFAC,CMP,KPPM,NDATA, &
                      KNRM,NORM)

!---------------------------------------------------------------------------
!
!*** Called by RESJAC
!
!  Compute and normalise the predicted time-domain data vector, XMODL.
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER NDATA,NCHNL,STEP,IDER,NSX,NPULS,NTYPLS,NTYRP,NFRQ,GSTRP,ASTRP, &
         KPPM,JS,JC,JT,JD,NCOMP,CMP,NSTAT,KNRM
 REAL XMODL(NDATA),BTD(NCHNL,NSTAT,3),TOPN(NCHNL),TCLS(NCHNL),FREQ(NFRQ), &
      NORM(KNRM),PRM_TD(3),BFFAC,PPFAC,VM,TRP(NTYRP),SWX(NSX),SWY(NSX,3),PULSE
 COMPLEX BFD(NFRQ,NSTAT,3)

 ! Compute BTD, the time-domain field response via cosine transform and
 ! convolution with the system waveform.

 CALL TDEM_3D (STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL, &
               TOPN,TCLS,FREQ,NFRQ,NSTAT,BFD,GSTRP,ASTRP,BTD)

 ! Normalise BTD.

 IF (KPPM == 0) THEN          ! Compute fields in requied units.
   BTD = BFFAC * BTD
 ELSE IF (KPPM > 0) THEN      ! Compute normalised response.
   NORM(1) = ABS (PRM_TD(1))
   NORM(2) = ABS (PRM_TD(2))
   NORM(3) = ABS (PRM_TD(3))
   IF (KPPM == 1 .OR. KPPM == 3) NORM(1:3) = NORM(KPPM) ! Vertical or in-line normalisation
   DO JC = 1,3
     BTD(1:NCHNL,1:NSTAT,JC) = PPFAC * BTD(1:NCHNL,1:NSTAT,JC) / NORM(JC)
   END DO
 END IF

 DO JS = 1, NSTAT
   DO JC = 1, NCOMP
     DO JT = 1, NCHNL
       IF (CMP == 11) THEN        ! SINGLE COMPONENT INVERSION
         VM = BTD(JT,JS,1)        ! Inline component of predicted data.
       ELSE IF (CMP == 13) THEN
         VM = BTD(JT,JS,3)        ! Vertical component of predicted data.
       ELSE IF (CMP == 2) THEN    ! TWO COMPONENT INVERSION
         IF (JC == 1) THEN
           VM = BTD(JT,JS,3)      ! Vertical component of predicted data.
         ELSE IF (JC == 2) THEN
           VM = BTD(JT,JS,1)      ! In-line component of predicted data.
         END IF
       ELSE IF (CMP == 3) THEN    ! THREE COMPONENT INVERSION
         IF (JC == 1) THEN        ! Vertical component of predicted data.
           VM = BTD(JT,JS,3)
         ELSE IF (JC == 2) THEN   ! In-line component of predicted data.
           VM = BTD(JT,JS,1)
         ELSE IF (JC == 3) THEN   ! Transverse component of predicted data.
           VM = BTD(JT,JS,2)
         END IF
       END IF
       JD = JT + (JC-1)*NCHNL + (JS-1)*NCOMP*NCHNL
       XMODL(JD) = VM 
     END DO
   END DO
 END DO

 END SUBROUTINE TD_FIELD

!===========================================================================

 SUBROUTINE TD_SENS (STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP, &
                     NCHNL,TOPN,TCLS,FREQ,NFRQ,NSTAT,SFD,JAC,NPAR,NDATA, &
                     NCOMP,PRM_TD,BFFAC,PPFAC,CMP,KPPM,KNRM,NORM,ASTRP,GSTRP)

!---------------------------------------------------------------------------
!
!*** Called by RESJAC
!
!  Compute and normalise the time-domain Jacobian matrix, JAC.
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER NDATA,NCHNL,STEP,IDER,NSX,NPULS,NTYPLS,NTYRP,NFRQ,GSTRP,ASTRP, &
         KPPM,JS,JC,JC1,JT,JD,JP,NCOMP,CMP,NSTAT,NPAR,KNRM
 REAL STD(NCHNL,NSTAT,NCOMP,NPAR),TOPN(NCHNL),TCLS(NCHNL),FREQ(NFRQ), & 
      PRM_TD(3),BFFAC,PPFAC,JAC(NDATA,NPAR),TRP(NTYRP),PULSE,SWX(NSX), & 
      SWY(NSX,3),NORM(KNRM)
 COMPLEX SFD(NFRQ,NSTAT,NCOMP,NPAR)

 ! Transform complex frequency-domain sensitivities SFD to real time-domain
 ! sensitivites STD:

 CALL SENS_FD2TD (STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP, &
                  NCHNL,TOPN,TCLS,FREQ,NFRQ,NSTAT,NCOMP,NPAR,SFD,STD, &
                  ASTRP,GSTRP)

 ! Normalise STD:

 IF (KPPM == 0) THEN            ! Compute sensitivities in requied units.
   STD = BFFAC * STD
 ELSE IF (KPPM > 0) THEN        ! Compute normalised response.
   NORM(1) = ABS (PRM_TD(1))
   NORM(2) = ABS (PRM_TD(2))
   NORM(3) = ABS (PRM_TD(3))
   IF (KPPM == 1 .OR. KPPM == 3) NORM(1:3) = NORM(KPPM) ! Vertical or in-line normalisation
   DO JC = 1, NCOMP
     IF (CMP == 11) THEN
       JC1 = 1                  ! In-line normalisation.
     ELSE IF (CMP == 13) THEN
       JC1 = 3                  ! Vertical normalisation.
     ELSE IF (CMP == 2) THEN
       IF (JC == 1) JC1 = 3     ! Vertical normalisation.
       IF (JC == 2) JC1 = 1     ! In-line normalisation.
     ELSE IF (CMP == 3) THEN
       IF (JC == 1) JC1 = 3     ! Vertical normalisation.
       IF (JC == 2) JC1 = 1     ! In-line normalisation.
       IF (JC == 3) JC1 = 2     ! Transverse normalisation.
     END IF
     STD(1:NCHNL,1:NSTAT,JC,1:NPAR) = PPFAC * STD(1:NCHNL,1:NSTAT,JC,1:NPAR) / NORM(JC1)
   END DO
 END IF

 DO JS = 1, NSTAT 
   DO JC = 1, NCOMP 
     DO JT = 1, NCHNL 
       JD = JT + (JC-1)*NCHNL + (JS-1)*NCOMP*NCHNL
       DO JP = 1, NPAR
         JAC(JD,JP) = STD(JT,JS,JC,JP)
       END DO 
     END DO
   END DO
 END DO

 END SUBROUTINE TD_SENS

!============================================================================

 SUBROUTINE SENS_FD2TD (STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP, &
                        NCHNL,TOPN,TCLS,FREQ,NFRQ,NSTAT,NCOMP,NPAR,SFD,STD, &
                        ASTRP,GSTRP)

!----------------------------------------------------------------------------
!
!***  Called by MAIN
!***  Calls CUBSPL, COSTRN, FOLD_CONVOLVE
!
!  Transform the unit dipole frequency-domain sensitivity values to system
!  time-domain sensitivities.
!
!----------------------------------------------------------------------------
!
!  DESCRIPTION:
!
!  Computes STD, the time domain sensitivity for the 3D part of the model
!  as S(t) in d(dB/dt)/dm or dB/dm by convolving the step sensitivity
!  with the negative time-derivative of the current waveform. The Questo
!  waveform contains the TX_AREA*NTRN. For dB/dm, this averaged across
!  the receiver window. For d(dB/dt)/dm, this is differenced across the
!  receiver window. The negative dI/dt is used so that current switch off
!  corresponds to positive response.
!
!  On entry, the imaginary component of the frequency-domain sensitivity
!  data is converted (via cosine transform) to time-domain step sensitivity
!  out to NPULS bipolar cycles. For each reciever component at each transmitter
!  position, FOLD_AND_CONVOLVE is called to fold the positive and negative
!  parts of the bipolar current pulse into a half-cycle (length PULSE) decay
!  curve. This result is convolved with the dI/dt waveform.
!
!----------------------------------------------------------------------------
!
!  INPUTS/OUTPUTS:
!
!  As per TDEM_3D.
!
!----------------------------------------------------------------------------

 IMPLICIT NONE
 REAL, PARAMETER :: TWOPI = 6.283185
 INTEGER GSTRP,ASTRP,IDER,STEP,NSX,NPULS,NTYPLS,NTYRP,NCHNL,NFRQ,NSTAT, &
         JS,JF,JC,JT,NCOMP,JP,NPAR
 REAL, DIMENSION(:,:), ALLOCATABLE :: YSCAT,YFRQ,SFD_QUAD
 REAL PULSE,FREQ(NFRQ),T0_MIN,WF(NFRQ),SWX(NSX),SWY(NSX,3),COSTRN,T, & 
      YCUM(NCHNL),TOPN(NCHNL),TCLS(NCHNL),TRP(NTYRP),OMEGA(NFRQ),    &
      STD(NCHNL,NSTAT,NCOMP,NPAR) 
 COMPLEX SFD(NFRQ,NSTAT,NCOMP,NPAR)  

 T0_MIN = 0.1 / MAXVAL (FREQ)
 STD = 0.

 ALLOCATE (YSCAT(4,NTYRP),YFRQ(4,NFRQ),SFD_QUAD(NFRQ,NCOMP))
 YSCAT = 0. ; YFRQ = 0. ; SFD_QUAD = 0.

 ! step dB/dt (nT/s) = H * 1.0E9 * MU * dI/dt * TXMMNT / (I * 2*PI * FREQ)
 ! Originally VFAC was = 1.0E9 * MU * TX MOMENT / (2*PI);
 ! From 9/01, input is B in nT and 1/(2 Pi) is now absorbed in OMEGA
 ! If STEP = 1, output is in nanoteslas consistent with HXEX_INV.
 ! In this case, conversion to pT or fT occurs in TD_SENS.

 OMEGA(1:NFRQ) = TWOPI * FREQ(1:NFRQ)
 WF = LOG(OMEGA)
 OMEGA = -OMEGA     ! Division by -iw for step response.

 DO JP = 1, NPAR
   DO JS = 1, NSTAT    ! Station loop.
     DO JC = 1, NCOMP
       DO JF = 1, NFRQ
         SFD_QUAD(JF,JC) = AIMAG(SFD(JF,JS,JC,JP)) / OMEGA(JF)
       END DO
     END DO

     ! Above: Conversion from impulse to step current turn-off.
     ! For each component at each receiver station, compute the sensitivity
     ! by splining the imaginary part of the frequency-domain sensitivity,
     ! converting it to time-domain step function response and folding the
     ! NPULS bipolar decay curve into a combined pulse decay curve of length
     ! PULSE. Convolve this with the TX waveform to produce STD, the
     ! "observable" sensitivity for the time-domain system.

     DO JC = 1, NCOMP          

       YFRQ(1,1:NFRQ) = SFD_QUAD(1:NFRQ,JC)

       CALL CUBSPL (WF,YFRQ,NFRQ,0,0)

       YSCAT = 0.
       DO JT = 1, NTYRP   ! Transform to time-domain step response.
         T = TRP(JT)
         IF (T < T0_MIN) CYCLE
         YSCAT(1,JT) = COSTRN (WF,YFRQ,NFRQ,NFRQ,T)
       END DO

       CALL FOLD_AND_CONVOLVE (STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,TRP,NTYPLS, &
                               NTYRP,NCHNL,TOPN,TCLS,YSCAT,GSTRP,ASTRP,YCUM)

       STD(1:NCHNL,JS,JC,JP) = YCUM(1:NCHNL)

     END DO
   END DO
 END DO

 DEALLOCATE (YSCAT,YFRQ,SFD_QUAD)

 END SUBROUTINE SENS_FD2TD

!============================================================================

 SUBROUTINE FD_FIELD (XMODL,NDATA,NSTAT,NFRQ,BFD,PRM_FD,BFFAC,PPFAC, & 
                      TXCLN,NRXST,TXA90,KNRM,NORM)

!----------------------------------------------------------------------------

 IMPLICIT NONE
 LOGICAL TXA90
 INTEGER NDATA,NSTAT,NFRQ,JD1,JD2,JS,JF,NRXST,KNRM
 REAL VM1,VM2,PRM_FD(NFRQ),TXCLN(NRXST),BFFAC,PPFAC,PRM4,NORM(KNRM),XMODL(NDATA)
 COMPLEX BFD(NFRQ,NSTAT,3),BFD1(NFRQ,NSTAT)

 ! The -ve sign above is a consequence of using the sine transform for a +iwt
 ! sign convention.  It is thus consistant with the convention used for the
 ! layered half space and in TDEM for time-domain scattered fields.

 DO JF = 1, NFRQ
   PRM4 = BFFAC * ABS (PRM_FD(JF))                     ! Coupled primary in nT, pT or fT.
   NORM(JF) = PPFAC / PRM4                             ! For pct, ppt, ppm or ppb output.
   BFD(JF,1:NSTAT,1:3) = -BFFAC * BFD(JF,1:NSTAT,1:3)  ! Total field in nT, pT or fT
 END DO

 ! Maximally coupled response.

 BFD1 = CMPLX(0.,0.)
 DO JF = 1, NFRQ
   IF (TXA90) THEN
     BFD1(JF,1:NSTAT) = BFD(JF,1:NSTAT,2)
   ELSE
     BFD1(JF,1:NSTAT) =   BFD(JF,1:NSTAT,1) * SIN(TXCLN(JF)) & 
                        + BFD(JF,1:NSTAT,3) * COS(TXCLN(JF))
   END IF
   BFD1(JF,1:NSTAT) = NORM(JF) * BFD1(JF,1:NSTAT)
 END DO

 XMODL = 0.
 DO JS = 1, NSTAT
   DO JF = 1, NFRQ
     JD1 = JF + 2*(JS-1)*NFRQ          
     JD2 = JF + 2*(JS-1)*NFRQ + NFRQ
     VM1 = REAL (BFD1(JF,JS))
     VM2 = AIMAG(BFD1(JF,JS))
     XMODL(JD1) = VM1
     XMODL(JD2) = VM2
   END DO
 END DO

 END SUBROUTINE FD_FIELD

!============================================================================

 SUBROUTINE FD_SENS (JAC,NDATA,NPAR,NSTAT,NFRQ,SFD,PRM_FD,BFFAC,PPFAC, &
                     NCOMP,KNRM,NORM)

!----------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER NDATA,NPAR,NSTAT,NFRQ,JD1,JD2,JS,JF,JP,NCOMP,KNRM
 REAL JAC(NDATA,NPAR),PRM_FD(NFRQ),BFFAC,PPFAC,PRM4,NORM(KNRM)
 COMPLEX SFD(NFRQ,NSTAT,NCOMP,NPAR)

 ! The -ve sign above is a consequence of using the sine transform for a +iwt
 ! sign convention.  It is thus consistant with the convention used for the
 ! layered half space and in TDEM for time-domain scattered fields.

 DO JF = 1, NFRQ
   PRM4 = BFFAC * ABS (PRM_FD(JF))    ! Coupled primary in nT, pT or fT.
   NORM(JF) = PPFAC / PRM4            ! For pct, ppt, ppm or ppb output.
   SFD(JF,1:NSTAT,1,1:NPAR) = -BFFAC * SFD(JF,1:NSTAT,1,1:NPAR) 
 END DO

 DO JS = 1, NSTAT
   DO JF = 1, NFRQ
     JD1 = JF + 2*(JS-1)*NFRQ
     JD2 = JF + 2*(JS-1)*NFRQ + NFRQ
     DO JP = 1, NPAR
       JAC(JD1,JP) = NORM(JF) * REAL (SFD(JF,JS,1,JP))
       JAC(JD2,JP) = NORM(JF) * AIMAG(SFD(JF,JS,1,JP))
     END DO
   END DO
 END DO

 END SUBROUTINE FD_SENS

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

!============================================!
!                                            !
!           SamAir Version History           !
!                                            !
!============================================!
!
!    CHANGE: 2.2.5 from 2.2.0 (Time-domain only)
!    ------------------------
!
!    1.  Inversion has been implemented and tested.
!
!    2.  A new option to allow the user to control the spectrum selection for time-domain 
!        computation can be accessed by setting TDFD = 0.  Then in a new RECORD 2.1, the
!        user specifies the range and density of frequencies to be used.  In fact, two 
!        ranges can be specified to allow a less dense sampling at lower frequencies.
!        In all cases, the program extrapolates the lower frequency response to DC before 
!        using the Hankel filter to transform to time domain.
!
!    3. Time-domain computation using the default (recommended) spectrum is more efficient
!
!    4. Mesh coordinates written to MF1 file. Mesh lithologies written to both
!       MV1 and MF1 files.
!
!
!  CHANGE: 2.2.0 from 2.1.0      I/O Formats are unchanged.
!  ------------------------
!
!    SamAir 2.2.0 is capable of modelling low to very high contrasts (100,000 : 1)
!    The previous release (2.1.0) was restricted to contrasts of less than 1000 : 1
!    This has been accomplished through the use of edge elements where the three
!    scalar node-based basis and test functions are replaced by single vector
!    edge-based basis and test functions.  
!
!  CHANGES: 2.1.0 from 2.0.4
!  -------------------------
!
!    1. During inversion, the element resistivities are written to SamayaAir.res
!       after each successful iteration.  If NLITH is negative, SamAir will
!       replace the element resistivities defined by the respective lithologies
!       with those contained in SamayaAir.res.
!
!    2. For each channel or frequency, the default fitting error (DO3D = -1)
!       normalises the misfit error and sensitivity matrix for each survey point
!       by the rms average of the model and survey data values at that point.
!       This is variously referred to as the symmetric point norm or P-norm.
!
!       A new option, activated by setting DO3D = -2, normalises the misfit
!       error and sensitivity matrix for each channel by the average of the
!       absolute value of the field data for all survey points for that
!       channel.  This places increased emphasis on fitting the peak anomaly
!       compared with the other data features.
!
!       Experiments with perfect model data to date indicate that the P-norm
!       recovered model features somewhat more accurately than was the case
!       with the S-norm.  However, the S-norm required between 30 to 50 percent
!       fewer iterations to converge.  There were a few cases where the S-norm
!       did a better job of model recovery than the P-norm.
!
!       SamAir writes both the P-norm misfit and S-norm misfit to the screen
!       and SamayaAir.out files to enable comparison of the two inversion
!       options when the true model is not known.
!
!    3. Both time-domain and frequency-domain inversion are enabled.
!
!    4. Additional convergence criterion: After the 10th iteration, the inversion
!       will stop if two successive iterations lower the RMS error by less than
!       1%. It is best to set MAXITS, the maximum number of iterations, to 90 so as
!       to not unduly restrict the inversion. It rare for ITS, the actual number
!       of iterations, to exceed 20 before convergence of some description is
!       achieved.
!
!    5. For inversion, the constraint scheme offers three options for each 
!       constrained parameter. Elasticities are always positive (RECORD 16.2).
!
!       Fixed      CTYPE = 1  => Neither elasticity nor bounds are required.
!       
!       Restraint  CTYPE = 2  => Elasticity (valued 0 to 1) is specified but
!                                no bounds are required.
!       
!       Buffered   CTYPE = 3  => Elasticity (valued 0 to 1) and upper and
!                                lower bounds are required.
!
!    6. The definitions for KCMP and ORDER have changed for frequency-domain
!       inversion to allow ArjunAir.inv files to be displayed more easily.
!
!    7. np Frequency format changed from G13.4 to F13.2.
!
!
!  CHANGE: 2.0.4 from 1.0.5
!  ------------------------
!
!  1. Inversion features now enabled for frequency-domain systems.
!     Time-domain inversion has been disabled.
!
!  2. Surveys for frequency-domain systems are now defined by the Tx-Rx midpoint
!     instead of the Tx position.
!
!  3. For option, SURVEY = 3: Specification of variable aircraft PITCH has
!     been replaced by specification of variable transmitter inclination, TXCLN.
!
!
!  CHANGE: 1.0.4 from 1.0.5
!  ------------------------
!
!  1. Accuracy problem for the different flight direction (East --> West
!     and South --> North) over symmetrical target has been corrected.
!
!  2. An error in calculation of vertical co-planar dipole was corrected.
