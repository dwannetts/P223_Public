!   PROGRAM Airbeo
!------------------------------------------------------------------------------------------------------
Module BA_Metadata
!--------------
!
! Module designed to hold 'constant character options'
    Implicit NONE

    Character (Len = 40), Parameter :: PNAME = 'Airbeo'
    Character (Len = 40), Parameter :: PVERS = '4.7.6'
    Character (Len = 40), Parameter :: PDATE = '06 June, 2021'
    Character (Len = 40), Parameter :: PAUT1 = 'CSIRO Electromagnetic Modelling Group'
    Character (Len = 40), Parameter :: PAUT2 = 'Art Raiche, Fred Sugeng and Glenn Wilson'
	Character (Len = 40), Parameter :: PAUT3 = 'David Annetts (david.annetts@csiro.au)'
    Character (Len = 40), Parameter :: PPROJ = 'AMIRA P223F'
    Character (Len = 60), Parameter :: PRELS = 'GPL-2 (https://opensource.org/licenses/gpl-2.0.php)'

End Module BA_Metadata

!------------------------------------------------------------------------------------------------------
!
!            2007 Software Contact (Sydney Australia):  Dr. Art Raiche
!
!                     phone: (02) 9498 8392
!                    mobile: 0422 489 577
!                    e-mail: art.raiche@optusnet.com.au
!
!     CHANGES: 4.7.4 from 4.7.3 (DWA)
!     -------------------------
!	 1. changed SELECTED_REAL_KIND(12, 80) to SELECTED_REAL_KIND(p = 18)
!	 2. modified inversion progress to omit model parameters
!	 3. abbreviated screen output
!
!
!     CHANGES: 4.7.3 from 4.7.2 (DWA)
!     -------------------------
!    1. Restored variable weighting.  Previous versions have binary weighting.  Can be accessed
!       by setting NSTAT < 0 in *.inv file.  Correct operation requires that weights be set to
!       standard deviations.
!
!    1. The 7-element lithology that was used in P223E has been reinstated.  This 
!       change was made for completeness so that all programs use the same lithology
!
!     CHANGES: 4.7.2 from 4.7.1 (DWA)
!     -------------------------
!    1. Summary statistics, in the form of the V-matrix, have been reinstated as standard.
!    2. U and V matricies are written to the .MV1 & .OUT files as standard
!    3. eigenparameter damping factors are written to the .MV1 & .OUT files as standard
!    4. minor formatting in both .OUT & .MV1 files for improved readability & processing
!
!
!     CHANGES: 4.7.1 from 4.7.0 (DWA)
!     -------------------------
!    1. The 7-element lithology that was used in P223E has been reinstated.  This 
!       change was made for completeness so that all programs use the same lithology
!
!     1. NP Frequency format changed from G13.4 to F13.2
!
!     CHANGES: 4.7.0 from 4.6.6 (DWA)
!     -------------------------
!     1. NP Frequency format changed from G13.4 to F13.2
!     2. New output file, Airbeo.mdl contains only resistivities plus
!        layer depths and thicknesses as a function of position
!
!
!     CHANGES: 4.6.6 from 4.5.5
!     -------------------------
!
!     1. For option, SURVEY = 3, specification of variable aircraft PITCH has been
!        replaced by specification of variable transmitter inclination, TXCLN
!
!     2. Additional error checking for inversion components
!
!
!     CHANGES: 4.6.5 from 4.5.4
!     -------------------------
!
!     1. The definitions for KCMP and ORDER have changed for frequency-domain
!        inversion to allow Airbeo.inv files to be displayed more easily.
!        Refer to RECORD 1 in the description for LeroiAir.inv after
!        RECORD 17 of Airbeo.cfl
!
!     2. The constraint scheme was modified to offer three options for each
!         constrained parameter.  Elasticities are always positve. (RECORD 17)
!
!         - fixed :     CTYPE = 1  neither elasticity nor bounds are required.
!         - restraint : CTYPE = 2  elasticity (value 0 to 1) is specified but
!                                  no bounds are required.
!         - buffered :  CTYPE = 3  elasticity (value 0 to 1) & bounds are required.
!
!     3. The QLYR = 2, depth to base inversion has been disabled due to poor
!        inversion performance and layer cross-over errors.
!
!     4. Additional convergence criterion: After the 10th iteration, inversion stops
!        if two successive iterations lower RMS error by < 1 percent.
!        It is best to set MAXITS, the maximum number of iterations, = 90 so as not
!        to unduly restrict the inversion procedure.  It is rare for ITS, the actual
!        number of iterations, to exceed 20 before convergence of some sort.
!
!     5. New station selection option using negative N0STAT to ignore all but
!        N0STAT stations for inversion. (RECORD 20)
!
!     6. Separate INVPRT options for Airbeo.out and Airbeo.mv1.  (RECORD 16)
!
!     7. The option to invert for system parameters has been removed.  Rather
!        than making system poarameter inversion a feature of every AEM program,
!        a separate program, Raven.f90 will be introduced at a later date.
!
!     8. Frequency-domain stations are now defined by Tx-Rx midpoint
!
!     9. Undefined variable problem rectified.
!
!================
!   DESCRIPTION |
!================
!
!  Airbeo is an AEM inversion program which fits a layered-earth model to
!  frequency or time-domain data.  The flight line is descretised into
!  NSTAT stations.  A separate layered earth model computed for each station.
!
!  In frequency domain, only the maximally coupled component is fit.  This
!  can be mixed horizontal coplanar, vertical co-axial or vertical co-planar
!  or a new option, vertical coplanar broadside.  The data are assumed to be
!  normalised; ie, ppm, ppb ppt or percent.
!
!  In time domain, the user can invert for the vertical (Z), component only,
!  the horizontal in-line (X) componentant only, both components or the total
!  component.  These can be expressed as dB/dt, magnetic field, or normalised
!  data.
!
!  The NLYR model consists of NLYR-1 layers over basement.  The user can
!  express the model in terms of layer thicknesses or depth to the base of
!  each layer.  The latter is best used when depth to basement is important.
!
!  The layer lithologies are composed of resistivities, relative dielectric
!  constants, relative magnetic permeabilities and the Cole-Cole parameters.
!
!  This version of Airbeo inverts for resistivities & either layer thicknesses
!  or depth to basement of each layer.  Lithology components other than
!  resistivity do not vary during inversion.
!
!*********************
!      Systems
!*********************
!
!  Airbeo can be used to model any existing AEM system in frequency or time-
!  domain mode.  Currently the transmitter is modelled as a magnetic dipole
!  whose axis is in the vertical plane along the flight path except for the
!  new vertical coplanar broadside option.  The other exception to this is
!  for co-axial time-domain HEM where the system normalisation assumes a
!  horizontal loop of finite radius.
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
!   The input control file, named Airbeo.cfl is read
!   from logical unit number NR = 3.
!
!   For inversion the data to be inverted must be contained in
!   Airbeo.inv, read from logical unit NRI = 13
!
!
!   VERBOSE-OUTPUT FILES:
!   --------------------
!
!   The Airbeo.out is written to logical unit NW = 4
!
!   Messages about data or runtime errors are written in a file
!   called Airbeo.log on logical unit NLG = 9
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
!    UNIT #   UNIT ID      FILE ID      Function
!    ------   -------      -------      --------
!       3       NR       Airbeo.cfl   Input control file
!       4       NW       Airbeo.out   Verbose output data
!       9       NLG      Airbeo.log   Data error messages
!      13       NRI      Airbeo.inv   Data to be inverted
!      14       NP       Airbeo.mv1   Tabular inversion output
!      15       MD1      Airbeo.mdl   Resistivities + layer depths & thicknesses
!
!-------------------------------------------------------------------------------
!
!****************************************************
!     DESCRIPTION OF DATA RECORDS for AIRBEO.CFL
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
!**  RECORD 1:    TITLE - up to 120 characters
!
!
!             MODEL CONTROL, UNITS & PRINT PARAMETERS
!             ---------------------------------------
!
!**  RECORD 2:  TDFD, DO1D, PRFL, ISTOP
!
!
!      TDFD = 1 => time-domain modelling
!           = 2 => frequency-domain modelling
!
!      DO1D = 1 or -1  Inversion using the same starting model for each station
!           = 2 or -2  Inversion using the final model each station as the
!                      model for the following station. (Care required.)
!           = 0 =>  Forward modelling only
!
!
!      PRFL =  1 prints response in profile mode using the default units and
!                line tag.  Each column contains the response at all stations
!                for a channel or frequency.
!
!           =  0 prints responses in temporal or frequency mode, using the
!                default units and line tag.  Each column contains the
!                responses for a single position for all frequencies (TDFD=2)
!                or delay times (TDFD=1).
!
!           = -1 same as PRFL = 1 except for new feature activation
!
!     ISTOP = 0  read the input data and run the specified models.
!           = 1  read the input data, print the model description
!                and STOP so that the model description can be verified.
!
!            REMEMBER to change ISTOP to 0 once the models have been verified.
!
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
!      ZRX(J) - vertical Rx offset : positive if Rx is below Tx
!      XRX(J) - in-line Rx offset :  positive if Rx is behind Tx
!      YRX(J) - transverse offset :  positive if Rx is left of Tx
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
!     DATA ENTRY CONTINUES WITH FLIGHT PATH SPECIFICATION IN RECORD 9
!
!  _____________________________________________________________________
!
!  RECORDS 3, 4, & 5 for TIME-DOMAIN AEM SYSTEM INFORMATION  (ISW > 0)
!  ---------------------------------------------------------------------
!
!   The user will only specify the waveform for 1/2 cycle.  The program
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
!            have the option to apply this algorithm to Airbeo model output.
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
!      ZRX0 - initial vertical Rx offset : positive if Rx is below Tx
!      XRX0 - initial in-line Rx offset :  positive if Rx is behind Tx
!      YRX0 - initial transverse offset :  positive if Rx is left of Tx
!
!             These are the high altitude calibration offsets.
!             If SURVEY < 3 in RECORD 9, these offsets are used for each station
!
!             Flight Path Information
!             -----------------------
!
!  ======================================================================
!  RECORD 9.1 and 9.2 are used only for forward modelling, DO1D = 0
!
!  For inversion, make a single entry for RECORD 9.0 and go on to RECORD 10
!
!========================================================================
!----------------------------------------------------------------------
!           only for inversion - NPASS should be set to 0 or 1
!           NPASS > 1 will the cause program to ignore the next NPASS records
!
!**  RECORD 9.0: NPASS
!                      GO TO RECORD 10
!----------------------------------------------------------------------
!======================================================================
!
!    RECORDS 9 FOR FORWARD MODELLING (DO1D = 0)
!    ------------------------------------------
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
!      LINE(1) - integer line number
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
!                positive if Rx is below Tx
!       XRX(J) - In-line offset of receiver at station J
!                positive if Rx is behing Tx
!       YRX(J) - Transverse offset of receiver at station J
!                positive if Rx is left of Tx

!
!          LITHOLOGY & STRUCTURE FOR AIRBEO
!          ================================
!
!** RECORD 10:  NLYR, QLYR, NLITH, GND_LVL
!
!          NLYR - number of layers including basement.
!
!          QLYR = 1 => structure specified using thickness of each layer.
!
!         NLITH - number of layer lithologies.
!                 Any number of lithologies may be defined.
!
!       GND_LVL - Relative level of flat surface (m)
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
!               (not used but must be entered)
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
!          LAYERED EARTH STRUCTURE
!          -----------------------
!
!** RECORD 12.1: LITH(1), THK(1)
!
!** RECORD 12.J: LITH(J), THK(J)
!
!** RECORD 12.NLYR: LITH (NLYR) - basement lithology
!
!      LITH(J) = integer which assigns the resistivity and other
!                physical properties from the list of RECORD 11
!                to layer J.
!
!      THK(J) = thickness of layer J
!
!___________________________________________
!
!     END OF DATA ENTRY IF NO INVERSION
!___________________________________________
!========================================================
!
!             INVERSION CONTROL
!             =================
!
!    The inversion will run until one of the following occurs:
!
!    1. The maximum number of iterations, specified by MAXITS in
!       RECORD 16 has been completed.  Hopefully this won't
!       happen since the sugested value is MAXITS = 90
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
!    rarely exceeded 25.  Often 10 iterations were sufficient.
!
!    MAXITS = 90 will allow the inversion to work to its full capability
!    and is suggested unless the user is in a hurry.
!
!** RECORD 16: MAXITS, CNVRG, NFIX, MV1PRT, OUTPRT
!
!      MAXITS - Upper limit on the maximum number of iterations
!               It is unusual not to achieve convergence of some sort before
!               20 iterations.  The suggested value for MAXITS is 90.
!               Use a low value of MAXITS to limit of change the inversion.
!
!      CNVRG = 1 => iterations will proceed unless the error can no
!                   longer be reduced.
!
!            = 2 => iterations will not proceed any further if the
!                   RMS (root mean square) error is less than a user
!                   specified percent (PCTCNV in RECORD 16.1).
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
!     ______________________________________________________________
!
!     only if NFIX > 0  (NFIX records)
!
!**   RECORD 17: CTYPE, LYR_INDX, KPAR, ELAS, LBND, UBND
!     ______________________________________________________________
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
!     LYR_INDX = constained layer number - increasing with depth
!     LYR_INDX = 1 or -1 refers to the top layer
!     LYR_INDX = NLYR or -NLYR refers to the basement.
!
!     The fact that LYR_INDX can be positive or negative is to allow
!     consistency with LeroiAir where plate indices are posiive and
!     layer indices are negative.  Positive indices are mor eintuitive,
!     especially for a layered earth program.
!
!     KPAR = 1 => layer resistivity
!            2 => layer thickness
!
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
!
!=========================================
!                                        =
!     END OF ENTRIES FOR Airbeo.cfl      =
!         (Logical unit NR = 3)          =
!                                        =
!     BEGIN DESCRIPTON FOR Airbeo.inv    =
!         (Logical unit NRI = 13)        =
!                                        =
!=========================================
!
!      Any number of comment lines can be inserted at the beginning of the
!      .inv file by using either / or \ as the first character of the line.
!
!      Airbeo will start reading at the first line not containing / or \
!      as the first character.  Thereafter, any line beginning with  / or \
!      will cause an eror and an end to execution.
!
!
!             DATA DESCRIPTION & WEIGHTING  - read from Airbeo.inv
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
!      N0STAT - number of stations for which all the data will be weighted to zero
!      N0CHNL - number of PCHNLs for which all the data will be weighted to zero
!      N0PTS  - number of data points not covered by K0STAT & K0CHNL which will be
!               weighted to zero
!
!     ------------------
!     only if N0STAT > 0
!**   RECORD 4:  K0STAT(1:K0STAT) - indices of stations for which all data
!     ------------------               will be weighted to 0
!
!     ------------------
!     only if N0STAT < 0
!**   RECORD 4:  K0STAT(1:K0STAT) - indices of stations for which all data
!     ------------------               will NOT be weighted to 0
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
!             DATA ENTRY   (read from Airbeo.inv)
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
!        TXCLN(J) = transmitter inclination
!
!==============================
!  END DATA ENTRY DESCRIPTIOM |
!==============================
!
!============================================================================


   MODULE BA_Filter_coefficients
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

END MODULE BA_Filter_coefficients

 MODULE BA_Filter_coefficients_QL
!------------------

!  These are based on using extended precision.  Not yet used for 3D computatiuon

 IMPLICIT NONE

 INTEGER, PARAMETER :: JNLO=-250, JNHI=150, NDEC_JN=15, QL=SELECTED_REAL_KIND(p=18)
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

END MODULE BA_Filter_coefficients_QL

 MODULE BA_Frequency_select
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

 END MODULE BA_Frequency_select

 MODULE BA_Input_routines

! CONTAINS: READ_SYSTEM_AND_SURVEY, READ_MODEL, READ_INVRSION_CNTRL, , READ_INVRSION_DATA

 Use iso_Fortran_env
 Use BA_Metadata

 IMPLICIT NONE

! General Airborne & Layered Earth Dimensions

 INTEGER, PARAMETER :: NPROP=7, QL=SELECTED_REAL_KIND(p = 18)
 Integer, Parameter :: FVN=450
 REAL, PARAMETER :: T0_MIN=1.E-7, PI=3.141592654, DATA_TOL=1.E-24, TURN=PI/10.
 INTEGER NR,NW,ND,NLG,NRI,NP,MD1,KS,TDFD,STEP,DO1D,ISW,PRFL,NCHNL,KPPM,ISTOP,NLITH,NSX,  &
         NSX1,JF,JT,JS,JL,JR,NFRQ,NTYRP,NSTAT,NLYR,NRX,NRXST,NTRN,MTXRX,NPULS,NTYPLS, &
         SURVEY,CMP,KRXW,MSG,MXERR,J,GSTRP,ASTRP,IUNITS,NPPF,QQDT(8),QQHMS(2),NDATA,  &
         NPAR,OUTPRT,MV1PRT,CNVRG,MAXITS,A2C,BAROMTRC,LINE_TAG
 INTEGER,ALLOCATABLE,DIMENSION(:) :: LITH,KFIX,CXPAR,LINE,CFG1
 INTEGER,ALLOCATABLE,DIMENSION(:,:) :: RWTS
 REAL TXAMPL,TXFREQ,PULSE,PKCUR,OFFTYM,ALF,DELT,ALT1,GND_LVL,TXCLN0,XRX0,YRX0,ZRX0,CSF,SNF, &
      DSTAT,PRM_TD(3),BFFAC,PPFAC,TXAREA,CMIN,CMAX,T0,QCND,PCTCNV,ALINE
 REAL,DIMENSION(:),ALLOCATABLE :: FREQ,WAVEFORM,TXON,SWX,TMS,WTMS,TOPN,TCLS,SZ,SX,SY,ZRX,  &
                                  XRX,YRX,BEARING,FANGLE,DEPTH,THK,RES,RMU,REPS,CHRG, &
                                  TRP,CALF,CTAU,CFREQ,TXCLN,TXDEG,PRM_FD,XDATA,      &
                                  XMODL,UBND,LBND,ELAS,MPAR,RES0,THK0,xwts
 REAL,DIMENSION(:,:),ALLOCATABLE :: SWY,RX,RY,RZ,LYTH,RDATA
 REAL(KIND=QL) QFRQ,QFRQ6,QFRQ12,FQQ,EAST1,NORTH1
 REAL(KIND=QL), ALLOCATABLE :: SXD(:),SYD(:),RXD(:,:),RYD(:,:)
 LOGICAL INVERT,TXA90
 CHARACTER (LEN=3), ALLOCATABLE :: CONFIG(:)
 CHARACTER (LEN=1) TCHR
 CHARACTER (LEN=120) INP,TITLE
 CHARACTER(LEN=60) LTXT
 CHARACTER(LEN=4) QUNIT,BUNIT,PUNIT
 Integer :: tvals(8)

! Parameters for the 3D target

 CONTAINS

   SUBROUTINE READ_SYSTEM_AND_SURVEY
!  -----------------------=---------

!***  Called by: MAIN
!***      Calls: CALL CONFIG_ID, WRITE_LOG_FILE

!  If DO1D > 0, data to be inverted is read in this routine

 REAL BFFACS(6),PPFACS(4),RHO,DELX,DELY,A1
 CHARACTER(LEN=4) PUNITS(4),BUNITS(6)
 CHARACTER (LEN=19) WVFRM(3)
 DATA WVFRM /'Transmitter current','Vertical receiver  ','Horizontal receiver'/
 DATA PUNITS /'pct ','ppt ','ppm ','ppb '/
 DATA BUNITS /'nT/s','pT/s','fT/s','nT  ','pT  ','fT  '/
 DATA BFFACS /1.,1000.,1.E6,1.,1000.,1.E6/
 DATA PPFACS /1.E2, 1.E3, 1.E6,1.E9/

 NR =  3     !  Input unit number for Airbeo.cfl
 NW =  4     !  Output unit number for Airbeo.out
 NLG = 9     !  Log file unit number for Airbeo.log
 NRI = 13    !  Inversion data for Airbeo.inv
 NP = 14    !  Output unit number for Airbeo.mv1 (inversion) or Airbeo.mf1 (forward model)
 MD1 = 15    !  Output unit number for Airbeo.mdl

 OPEN(NR,FILE = 'Airbeo.cfl',STATUS = 'OLD')
 OPEN(NW,FILE = 'Airbeo.out',STATUS = 'REPLACE')

!      Initialise some variables.

 MXERR = 0             !  Initialise input error flag
 NCHNL = 1             !  Initialise dimensions
 NPPF = 3              !  ppm default
 IUNITS = 5            !  pT default
 GSTRP = 0
 ASTRP = 0
 TXAREA = 1.
 PRM_TD = 0.
 INVERT = .FALSE.
 TXA90 = .FALSE.       !  Don't print scattered fields

!  Reproduce input data with no assignments and rewind file.
 Call date_and_time(Values = tvals)
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

 READ(NR,'(A)') TITLE
 WRITE(NW,'(/1X,A)') TRIM (TITLE)

! Read model control & print parameters

 READ(NR,*)  TDFD, DO1D, PRFL, ISTOP
 DO1D = ABS (DO1D)
 WRITE(NW,3) TDFD, DO1D, PRFL, ISTOP
 IF (DO1D > 0) INVERT = .TRUE.

 IF (PRFL == 0 .OR. PRFL == 2 .OR. PRFL == 10) THEN
   PRFL = 2
 ELSE
   PRFL = 1
 END IF

!   TDFD = 1 or 2 for TD or FD respectively.
!   DO1D = 1 => use seed model for all inversins
!        = 2 => use result of station J for seed for station J+1
!        = 0 => compute layered 1/2 space model only.
!   PRFL - indicates profile or decay curve output
!  ISTOP - read data and stop if ISTOP = 1
!        - used as a frequency setter later on in this routine.

 IF (TDFD /= 1 .AND. TDFD /= 2) CALL WRITE_LOG_FILE (NLG,1,MXERR,2)
 IF (DO1D > 2 ) DO1D = 1

 IF (TDFD == 1) THEN

! Transmitter system information
! ------------------------------

   NRX = 1; NFRQ = 1
   READ(NR,*)   ISW, NSX1, STEP, IUNITS, NCHNL, KRXW, OFFTYM
   WRITE(NW,4)  ISW, NSX1, OFFTYM, NCHNL, KRXW, STEP, IUNITS
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
             TOPN(NCHNL),TCLS(NCHNL),FREQ(1))

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

   READ(NR,*) ZRX0, XRX0, YRX0
   WRITE(NW,18) ZRX0, XRX0, YRX0
   RHO = ABS (XRX0) + ABS (YRX0)
   IF (RHO < 1. .AND. KPPM > 0) KPPM = 3

 ELSE IF (TDFD == 2) THEN                ! Frequency-domain systems

   NCHNL = 1;  NSX = 1; NTYRP = 1
   ALLOCATE (SWX(1),SWY(1,3),TRP(1),TXON(1),WAVEFORM(1),TMS(1),WTMS(1),TOPN(1),TCLS(1))
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

! Flight path details for forward modelling only.  Convert FANGLE & TXCLN to radians

 IF (INVERT) THEN
   READ(NR,*) NSTAT
   IF (NSTAT > 1) THEN
     DO JF = 1,NSTAT
       READ(NR,'(A)') INP
     END DO
   END IF
 ELSE
   READ(NR,*)  NSTAT,SURVEY,BAROMTRC,LINE_TAG
   WRITE(NW,22) NSTAT,SURVEY,BAROMTRC,LINE_TAG
   SURVEY = ABS(SURVEY)
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

   IF (TDFD == 1) THEN
     NRXST = NSTAT
     ALLOCATE (ZRX(NSTAT),XRX(NSTAT),YRX(NSTAT),TXCLN(NSTAT),TXDEG(NSTAT))
     TXCLN = TXCLN0; ZRX = ZRX0; XRX = XRX0; YRX = YRX0
   END IF

   ALLOCATE (LINE(NSTAT),SX(NSTAT),SY(NSTAT),SZ(NSTAT),FANGLE(NSTAT),BEARING(NSTAT),SXD(NSTAT),SYD(NSTAT), &
             RX(NSTAT,NRX),RY(NSTAT,NRX),RZ(NSTAT,NRX),RXD(NSTAT,NRX),RYD(NSTAT,NRX))

   LINE = 1000; SX=0.; SY=0.; SZ = 0.; FANGLE = 0.; BEARING=0.
   SXD = 0.; SYD = 0.; RX=0.; RY=0.; RZ = 0.; RXD=0.; RYD=0.;

! Read in course for forward modelling
! Read in course + data to be inverted for inversion

             ! Forward modelling only

   IF (SURVEY == 1) THEN
     IF (LINE_TAG == 1) THEN
       READ(NR,*) ALINE,SYD(1),SXD(1),SZ(1),BEARING(1),DSTAT
       LINE(1) = FLOOR (ALINE)
     ELSE
       READ(NR,*) SYD(1),SXD(1),SZ(1),BEARING(1),DSTAT
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
           READ(NR,*) ALINE,SYD(JS),SXD(JS),SZ(JS)
           LINE(JS) = FLOOR (ALINE)
         ELSE
           READ(NR,*) SYD(JS),SXD(JS),SZ(JS)
         END IF
       ELSE IF (ABS (SURVEY) == 3) THEN
         IF (LINE_TAG == 1) THEN
           READ(NR,*) ALINE,SYD(JS),SXD(JS),SZ(JS),TXCLN(JS),ZRX(JS),XRX(JS),YRX(JS)
           LINE(JS) = FLOOR (ALINE)
         ELSE
           READ(NR,*) SYD(JS), SXD(JS), SZ(JS), TXCLN(JS), ZRX(JS), XRX(JS), YRX(JS)
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

   IF (TDFD == 1) THEN
     WRITE(NW,25) NSTAT
     DO  JS = 1, NSTAT
       WRITE(NW,26) LINE(JS),JS,SYD(JS),SXD(JS),SZ(JS),BEARING(JS),TXCLN(JS),ZRX(JS),XRX(JS),YRX(JS)
     END DO
   ELSE
     WRITE(NW,27) NSTAT
     DO  JS = 1, NSTAT
       WRITE(NW,28) LINE(JS),JS,SYD(JS),SXD(JS),SZ(JS),BEARING(JS)
     END DO
   END IF
 END IF

! ==================================================================================
!
! Formats .....
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

 2 FORMAT (2x ,78('-'))
 3 FORMAT(/T3,'TDFD =',I3,3X,'DO1D =',I3,3X,'PRFL =',I3,4X,'ISTOP =',I2)
 ! 4 FORMAT(T3,'ISW =',I4,T15,'NSX =',I4,T27,'STEP =',I3,T39, &
 ! 	     /t3, 'UNITS =',I2,T52,'NCHNL =',I4, &
 !         /T3,'KRXW =',I3,T15,'OFFTYM =',G12.4)
 4 Format (/, 2x, 'ISW     = ', i4, &
           /, 2x, 'NSX     = ', i4, &
           /, 2x, 'Offtime = ', f12.4, ' ms', &
           /, 2x, 'NCHNL   = ', i4, &
           /, 2x, 'KRXW    = ', i4, &
           /, 2x, 'Step    = ', i4, &
           /, 2x, 'Units   = ', i4)
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

   END SUBROUTINE READ_SYSTEM_AND_SURVEY

   SUBROUTINE READ_MODEL
!  ---------------------

!***  Called by: MAIN
!***      Calls: WRITE_LOG_FILE

 IMPLICIT NONE
 INTEGER QLYR

!  Layered Model Specification
!  ---------------------------

 READ(NR,*)  NLYR, QLYR, NLITH, GND_LVL
 WRITE(NW,1) NLYR, NLITH, GND_LVL
 IF (QLYR /= 1) THEN
   QLYR = 1
   CALL WRITE_LOG_FILE (NLG,50,MXERR,1)
 END IF
 NPAR = 2*NLYR-1

 ALLOCATE (LYTH(NLITH,NPROP),LITH(NLYR),DEPTH(NLYR-1),THK(NLYR-1),RES(NLYR),THK0(NLYR-1),RES0(NLYR), &
           CHRG(NLYR),CALF(NLYR),CTAU(NLYR),CFREQ(NLYR),RMU(NLYR),REPS(NLYR),MPAR(NPAR))

 DEPTH=0.; THK=0; RES=0; CHRG=0; CALF=1; CTAU=0; CFREQ=1; RMU=1
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
   READ (NR,*) LYTH(J,1:NPROP)
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
     READ (NR,*) LITH(J), THK(J)
     WRITE(NW,'(2I4,F7.1,T19,A)') J, LITH(J), THK(J),'J, LITH(J), THK(J)'
   END DO
 END IF
 READ(NR,*) LITH(NLYR)
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
 RES0 = RES
 THK0 = THK
 MPAR(1:NLYR) = RES(1:NLYR)
 MPAR(NLYR+1:NPAR) = THK(1:NLYR-1)

  1 FORMAT(//T3,'NLAYER =',I3,';   NLITH =',I3,';   GND_LVL =',F8.2)
  2 FORMAT(//T27,'LITHOLOGY PROPERTIES'/T27,'--------------------' &
           //T35,'Relative   Relative     Cole-Cole Parameters'    &
            /T9,'Resistivity  Conductance     MU     Dielectric   CHRG    CTAU       CFREQ'/)
  3 FORMAT(//T3,'LAYERED EARTH INPUT DATA'/T3,'------------------------'/)

   END SUBROUTINE READ_MODEL


   SUBROUTINE READ_INVRT_CNTRL_AND_DATA
!  ------------------------------------

!***  Called by: MAIN
!***      Calls: WRITE_LOG_FILE

 INTEGER NFIX,ORDER,MDCHNL,NDCMP,N0STAT,N0CHNL,N2B,N2E,N3B,N3E, &
         CTYPE,LYR_INDX,KPAR,NSTA,N0PTS,JP,JP1,J1
 INTEGER, ALLOCATABLE, DIMENSION(:) :: KCMP,K0STAT,K0CHNL
 Real, Allocatable :: R0CHNL(:)
 REAL TDATA,E1,E2,E3,A1,A2,DELX,DELY,RHO
 REAL, ALLOCATABLE,DIMENSION (:) :: QDATA,Q2DATA,DATA_FLOOR
 CHARACTER (LEN=1) TCHR
 CHARACTER (LEN=3) KCMPC(0:5)
 CHARACTER(LEN=11) LYR_PRM(2)
 DATA KCMPC / '   ','HCP','VCP','VCA','VCB','HCA'/
 Logical :: NON_ZERO_WEIGHTS
 Logical :: IsComment
! ----------------------------------------------------------------
! Set inversion dimensions:
!
!  NCHNL = number of time domain channels
!  NDATA = total number of readings per station to be inverted(TD or FD)
!        = NCHNL for time-domain when CMP = 11, 13, 4, 42, 43
!        = 2* NCHNL for time-domain when CMP = 2
!        = 3* NCHNL for time-domain when CMP = 3
!
!        = 2 * NFRQ for frequency-domain
!
!  RDATA & RWTS are data and weights in array form (NDATA, NSTAT)
!  XDATA & XWTS are data and weights in individual columns of RDATA & RWTS
!  RWTS  & XWTS are now restricted to integer values of 0 or 1 (reject or accep)
!  Note that data are normalised so that balance weighting is unnecessary.
!
!  XMODL contains model results in column form (NDATA)
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


 LYR_PRM(1) =  'Resistivity'
 LYR_PRM(2) =  ' Thickness '

 IF (TDFD == 1) THEN
   NDATA = NCHNL
   IF (CMP == 2) NDATA = 2*NCHNL
   IF (CMP == 3) NDATA = 3*NCHNL
 ELSE
   NDATA = 2*NFRQ
 END IF
 ALLOCATE (ELAS(NPAR),LBND(NPAR),UBND(NPAR),CXPAR(NPAR))
 CXPAR = 0
 IF (TDFD == 1) ALLOCATE (DATA_FLOOR(1),KCMP(1))
 IF (TDFD == 2) ALLOCATE (DATA_FLOOR(2*NFRQ),KCMP(NFRQ))

 DATA_FLOOR = 0.
 E1 = 1. ; ELAS = 1.
 E2 = 1. ; LBND = 1.
 E3 = 1. ; UBND = 1.

 WRITE(NW,1)
 IF (TDFD == 1) THEN
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
 IF (DO1D == 1) WRITE(NW,31)
 IF (DO1D == 2) WRITE(NW,32)
 WRITE(NW,9) NPAR, NDATA

 READ(NR,*) MAXITS,CNVRG,NFIX,MV1PRT,OUTPRT
 WRITE(NW,10) MAXITS,CNVRG,NFIX,MV1PRT,OUTPRT
 IF (MV1PRT < 0 .OR. MV1PRT > 3) MV1PRT = 1
 IF (OUTPRT < 0 .OR. OUTPRT > 3) OUTPRT = 1
 IF (CNVRG /= 1 .AND. CNVRG /=2) THEN
   CNVRG = 1
   CALL WRITE_LOG_FILE (NLG,205,MXERR,1)
 END IF

 IF (CNVRG == 2) THEN
   READ(NR,*) PCTCNV
   WRITE(NW,15) PCTCNV
 ELSE
   WRITE(NW,16) MAXITS
 END IF

 IF (NFIX > 0) THEN
   WRITE(NW,17)
   DO JP = 1, NFIX
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
     E1 = ABS(E1)
     IF (E1 < 0.05) E1 = 0.      ! Hold for elasticities < 0.05
     IF (E1 > 0.95) E1 = 1.      ! Allow full freedom for elasticities > 0.95

     LYR_INDX = ABS (LYR_INDX)
     IF (KPAR /= 1 .AND. KPAR /= 2) CYCLE
     JP1 = LYR_INDX
     IF (KPAR == 2) JP1 = NLYR + LYR_INDX
     IF (JP1 > NPAR) CYCLE

     IF (ABS (E1) > 0.95) E1 = 1.      ! Allow fuLl freedom for elasticities > 0.95
     IF (E2 > E3) THEN                 ! Switch if LB > UB
       A1 = E3
       E3 = E2
       E2 = A1
     END IF
     A1 = E3 - E2
     A2 = 0.005 * (ABS (E2) + ABS (E3))
     IF (A1  < A2) E1 = 0.

     CXPAR(JP1) = CTYPE
     IF (ABS (E1) < 0.05) THEN      ! Hold for elasticities < 0.05
       E1 = 0.
       CXPAR(JP1) = 1
     END IF
     WRITE(NW,18) JP1,LYR_INDX,KPAR,LYR_PRM(KPAR),E1,E2,E3,CXPAR(JP1)

     ELAS(JP1) = E1
     LBND(JP1) = E2
     UBND(JP1) = E3
   END DO
   WRITE(NW,20)
 ELSE
   WRITE(NW,19)
 END IF

!  Start reading from Airbeo.inv on UNIT NRI = 13
!  First skip over al comment lines

 NON_ZERO_WEIGHTS = .False.
  DO
   READ (NRI,'(A)') TCHR
   IF (.not.(IsComment(tchr))) EXIT
 END DO
 BACKSPACE (NRI)

 IF (TDFD == 1) THEN
   READ(NRI,*)  NSTAT, SURVEY, BAROMTRC, KCMP(1), ORDER

   If (NSTAT .lt. 0) Then 
    NON_ZERO_WEIGHTS = .True.
    Write (nw, 70)
   End If
   NSTAT = ABS(NSTAT)
   
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
   WRITE(NW,33) NSTAT, SURVEY, BAROMTRC, ORDER
   IF (MAXVAL (FREQ) < 1.E5) THEN
     WRITE(NW,34) (J,KCMPC(KCMP(J)),FREQ(J),J = 1,NFRQ)
   ELSE
     WRITE(NW,35) (J,KCMPC(KCMP(J)),FREQ(J),J = 1,NFRQ)
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

 IF (TDFD == 1) THEN
   NRXST = NSTAT
   ALLOCATE (ZRX(NSTAT),XRX(NSTAT),YRX(NSTAT),TXCLN(NSTAT),TXDEG(NSTAT))
   TXCLN = TXCLN0; ZRX = ZRX0; XRX = XRX0; YRX = YRX0
 END IF

 ALLOCATE (LINE(NSTAT),SX(NSTAT),SY(NSTAT),SZ(NSTAT),FANGLE(NSTAT),BEARING(NSTAT),SXD(NSTAT),SYD(NSTAT), &
           RX(NSTAT,NRX),RY(NSTAT,NRX),RZ(NSTAT,NRX),RXD(NSTAT,NRX),RYD(NSTAT,NRX))

 SX=0.; SY=0.; RX=0.; RY=0.; RZ = 0.
 RXD=0.; RYD=0.; FANGLE=0.

 IF (TDFD == 1) THEN
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
   NDCMP = 3
   IF (KCMP(1) < 100) NDCMP = 2
   IF (KCMP(1) < 13) NDCMP = 1
   MDCHNL = NDCMP * NCHNL

 ELSE IF (TDFD == 2) THEN
   MDCHNL = NDATA
   READ(NRI,*) DATA_FLOOR(1:2*NFRQ)

   WRITE(NW,23) TRIM (QUNIT)
   DO JP = 1,NFRQ
     WRITE(NW,24) JP,FREQ(JP), ABS (DATA_FLOOR(JP)), ABS (DATA_FLOOR(JP+NFRQ))
   END DO
 END IF

! Invert one station at a time
 ALLOCATE (RDATA(NDATA,NSTAT),RWTS(NDATA,NSTAT),XDATA(NDATA),XWTS(NDATA),XMODL(NDATA), &
           QDATA(MDCHNL),Q2DATA(MDCHNL))
 XDATA = 0.; RDATA = 0.
 XWTS = 1;  RWTS = 1

 READ(NRI,*)   N0STAT, N0CHNL, N0PTS
 WRITE(NW,25) N0STAT, N0CHNL, N0PTS

 IF (N0STAT /= 0) THEN
   NSTA = ABS (N0STAT)
   ALLOCATE (K0STAT(NSTA))
   READ(NRI,*) K0STAT(1:NSTA)
   IF (N0STAT > 0) THEN
     WRITE(NW,26) K0STAT(1:NSTA)
     DO J1 = 1, NSTA
       RWTS(1:NDATA,K0STAT(J1)) = 0
     END DO
   ELSE
     RWTS = 0
     WRITE(NW,29) K0STAT(1:NSTA)
     DO J1 = 1, NSTA
       RWTS(1:NDATA,K0STAT(J1)) = 1
     END DO
   END IF
   DEALLOCATE (K0STAT)
 END IF

 IF (N0CHNL > 0) THEN
   If (NON_ZERO_WEIGHTS) Then
       Allocate (R0CHNL(N0CHNL))
       READ(NRI,*)  R0CHNL(1:N0CHNL)
       Print *, R0CHNL
       Do j1 = 1, N0CHNL
         RWTS(j1, 1:NSTAT) = R0CHNL(j1)
       End Do
       Deallocate (R0CHNL)
   Else       
       ALLOCATE (K0CHNL(N0CHNL))
       READ(NRI,*)  K0CHNL(1:N0CHNL)
       WRITE(NW,27) K0CHNL(1:N0CHNL)
       DO J1 = 1, N0CHNL
         RWTS(K0CHNL(J1),1:NSTAT) = 0
       END DO
       DEALLOCATE (K0CHNL)
   End If
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


!  Put data in the order of all Z followed by all X followed by all Y
!  depending upon which components are present.

   IF (TDFD == 1) THEN
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
       RDATA(1:NDATA,JS) = Q2DATA(1:NDATA)
     ELSE IF (CMP == 4) THEN
       DO JT = 1,NCHNL
         TDATA = Q2DATA(JT)**2
         IF (NDCMP > 1) TDATA = TDATA + Q2DATA(JT+NCHNL)**2
         IF (NDCMP ==3) TDATA = TDATA + Q2DATA(JT+2*NCHNL)**2
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
       CALL WRITE_LOG_FILE (NLG,220,MXERR,2)
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

 IF (TDFD == 1) THEN
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
     DO JT = 1,NDATA
       IF (ABS(RDATA(JT,JS)) < DATA_FLOOR(1)) RWTS(JT,JS) = 0
     END DO
   END DO


   WRITE(NW,53)
   DO JS = 1,NSTAT
     WRITE(NW,50) JS,RWTS(1:NCHNL,JS)               ! Solo or vertical weights
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
     WRITE(NW,50) JS, RWTS(1:2*NFRQ,JS)
   END DO
 END IF

  1 FORMAT(//T3,'-------------------------' &
            /T3,'Inversion Controls & Data' &
            /T3,'-------------------------')
  2 FORMAT(/T3,'Inversion of Time-Domain Vertical Component Data')
  3 FORMAT(/T3,'Inversion of Time-Domain In-Line Component Data')
  4 FORMAT(/T3,'Joint Inversion of Time-Domain Vertical & In-Line Component Data')
  5 FORMAT(/T3,'Joint Inversion of Time-Domain Three Component Data')
  6 FORMAT(T3,'The data to be inverted is expressed as ',A)
  7 FORMAT(T3,'The data to be inverted has been normalised to ',A)
  8 FORMAT(/T3,'Inversion of Frequency-Domain Data')
  9 FORMAT(/T3,'NPAR =',I3,3X,'MCHNL =',I3)
 10 FORMAT(T3,'MAXITS =',I3,3X,'CNVRG =',I2,3X,'NFIX =',I3,3X,'MV1PRT =',I2,3X,'OUTPRT =',I2)
 15 FORMAT( T3,'Airbeo will finish when either the RMS error is less than',F6.1,' percent' &
           /T3,'or after',I3,' iterations, whichever comes first.')
 16 FORMAT( T3,'Airbeo will run until the error can no longer be reduced or after',I3,' iterations,' &
           /T3,'whichever comes first.')
 17 FORMAT(//T12,'Constrained Parameters'/T12,'----------------------' &
           //T5,'Global  Layer  Parameter'                                                        &
            /T5,'Index   Index   Index      Parameter      Elasticity  Lower Bound   Upper Bound   CXPAR' &
            /T5,'------  -----  ---------   ---------      ----------  -----------   -----------   -----')
 18 FORMAT(T7,I2,T14,I2,T23,I1,T31,A,T50,F5.2,T59,G12.4,T73,G12.4,T87,I3)
 19 FORMAT(/T3,'All model parameters will be allowed to vary during inversion')
 20 FORMAT(/T3,90('-'))
 21 FORMAT(/T3,'NSTAT =',I5,3X,'SURVEY =',I2,3X,'BAROMTRC =',I2,3X,'KCMP =',I4,3X,'ORDER =',I2)
 22 FORMAT(/T3,'Time-Domain Data Floor =',G12.4,1X,A)
 23 FORMAT(/T8,'Frequency Data Floors (',A,')'/T8,'----------------------------' &
          //T8,'Freq     In-phase   Quadrature'/)
 24 FORMAT(I4,F9.0,2G12.4)
 25 FORMAT(/T3,'N0STAT =',I4,3X,'N0CHNL =',I3,3X,'N0PTS =',I4)
 26 FORMAT(/T3,'The data from the following stations will not be inverted:'/T3,60I4)
 27 FORMAT(/T3,'Data from the following PCHNLs will be weighted to zero:'/T3,60I4)
 28 FORMAT(/T3,'Data from the following (PCHNL, STAT) pairs will be weighted to zero:')
 29 FORMAT(/T3,'Only data from the following stations will be inverted:'/T3,60I4)
 31 FORMAT(/T3,'The same starting model will be used for all inversions')
 32 FORMAT(/T3,'The starting model for all inversions after the first will be the', &
           /T3,'final model from the previous inversion.')
 33 FORMAT(//T3,'NSTAT =',I5,3X,'SURVEY =',I2,3X,'BAROMTRC =',I2,3X,'ORDER = ',I4)
 34 FORMAT(/T3,'Inversion components:',20(I5,': ',A,F8.1))
 35 FORMAT(/T3,'Inversion components:',20(I5,': ',A,F10.1))
 40 FORMAT(I9,I7,F11.0,2F12.1,F9.1,300G13.4)
 41 FORMAT(//T6,'Line   Station   Bearing     East       North     Alt',T68,'Horizontal In-line Component Data' &
            /T6,'----   -------   -------     ----       -----     ---',T68,'----------------------------------')
 42 FORMAT(//T6,'Line   Station   Bearing     East       North     Alt',T68,'Horizontal Transverse Component Data' &
            /T6,'----   -------   -------     ----       -----     ---',T68,'------------------------------------')
 43 FORMAT(//T6,'Line   Station   Bearing     East       North     Alt',T68,'Vertical Component Data' &
            /T6,'----   -------   -------     ----       -----     ---',T68,'-----------------------')
 45 FORMAT(/T3,'Components from Airbeo.inv:',I4,20I5)
 46 FORMAT(/T32,20(2X,A))
 47 FORMAT(/T3,'Components from Airbeo.cfl:',I4,20I5)
!50 FORMAT(T1,I10,T13,300I2)
 50 Format (t1, i10, t13, 300(2x, i2))
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
 70 Format (/, 2x, 'Weights are NON ZERO ....', /)
 101 FORMAT(T3,'Three component inversion has been specified (CMP = 3)' &
           /T3,'but three components are not read in: KCMP = ',I4)
 102 FORMAT(T3,'Two component inversion has been specified (CMP = 2)' &
           /T3,'but two components are not read in: KCMP = ',I4)
 103 FORMAT(T3,'Vertical component inversion has been specified (CMP = 13)' &
           /T3,'but this component is not read in: KCMP = ',I4)
 104 FORMAT(T3,'In-line component inversion has been specified (CMP = 11)' &
           /T3,'but this component is not read in: KCMP = ',I4)

   END SUBROUTINE READ_INVRT_CNTRL_AND_DATA

   SUBROUTINE SET_SURVEY
!  ---------------------

!***  Called by: MAIN


   IMPLICIT NONE

   IF (BAROMTRC == 1 ) THEN
     SZ = SZ - GND_LVL        ! Change barometric altitude to ground clearance
     IF ( ABS (GND_LVL) > 0.01) WRITE(NW,2) GND_LVL
   END IF

!  Compute receiver coordinates in both body centred and real world systems.

   DO JS = 1,NSTAT
     CSF = COS (FANGLE(JS))
     SNF = SIN (FANGLE(JS))

     IF (TDFD == 1) THEN
       SX(JS) = REAL (SXD(JS))
       SY(JS) = REAL (SYD(JS))
       RX(JS,1) = SX(JS) - XRX(JS) * CSF + YRX(JS) * SNF
       RY(JS,1) = SY(JS) - YRX(JS) * CSF - XRX(JS) * SNF
       RZ(JS,1) = SZ(JS) - ZRX(JS)
     ELSE
       SX(JS) = REAL (SXD(JS)) + 0.5 * (XRX(1) * CSF - YRX(1) * SNF)
       SY(JS) = REAL (SYD(JS)) + 0.5 * (YRX(1) * CSF + XRX(1) * SNF)
       DO JF = 1,NFRQ
         RX(JS,JF) = SX(JS) - XRX(JF) * CSF + YRX(JF) * SNF
         RY(JS,JF) = SY(JS) - YRX(JF) * CSF - XRX(JF) * SNF
         RZ(JS,JF) = SZ(JS) - ZRX(JF)
       END DO
    END IF
   END DO
   RXD = REAL (RX,KIND=QL)
   RYD = REAL (RY,KIND=QL)
   SXD = REAL (SX,KIND=QL)  !  SXD & SYD were midpoints in Freq domain
   SYD = REAL (SY,KIND=QL)  !  Now they are Tx positions based on XRX(1), YRX(1)


 2 FORMAT(/T3,'Barometric altitude will be changed to ground clearance.' &
          /T3,'The vertical origin is shifted down by',F6.1)

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

 INTEGER NCMP,J1
 CHARACTER(LEN=5) CHZ,CHX,CHY,CHT,WEZ,WEX,WEY,WET,WEQ,WEI
 CHARACTER(LEN=6),DIMENSION(NFRQ) :: QFRQ,IFRQ

 WRITE(NP,1) FVN,trim(PNAME),TRIM(TITLE)
 If (INVERT) Then
	 WRITE(MD1,1) FVN,PNAME,TRIM(TITLE)
	 WRITE(MD1,10)
 End If
 IF (TDFD == 1) THEN
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
   WRITE(NP,8) NLYR
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
   WRITE(NP,8) NLYR
 END IF

 WRITE(NP,20)
 WRITE(NP,9,ADVANCE='NO')
 WRITE(NP,'(30(A,I2.2))') ('  RES_',J1,J1=1,NLYR), (' THICK_',J1,J1=1,NLYR-1)

 ! IF (.NOT. INVERT) WRITE(NP,22) MPAR(1:NPAR)
 WRITE(NP,22) MPAR(1:NPAR)      ! generally usefull to have initial model in MV1

  1 FORMAT(T1,'/ ',I4.4,T15,'File version number'/T1,'/ PROGRAM_NAME=',A/T1,'/ TITLE: ',A)
  3 FORMAT(T1,'/ UNITS=',A,3X,'NSTAT=',I5,3X,'NCH=',I3.3,3X,'NCMP=',I1)
  4 FORMAT(T1,'/ TIMES(ms)=',T17,2014(2x, f8.4))
  5 FORMAT(T1,'/ CHNL_WDTH(ms)=',T17, 2014(2x, f8.4))
  6 FORMAT(T1,'/ SURVEY=TD_AEM  PP=RX_POS')
  7 FORMAT(T1,'/ LINE_HEADER:     Station  EastTx  NorthTx  AltTx  Txcln  EastRx  NorthRx  AltRx',1024(A,I2.2))
  8 FORMAT(T1,'/ LAYERS=',I2.2)
  9 FORMAT(T1,'/')
 10 FORMAT(T1,'/ Station  EastTx  NorthTx  RES(1:NLYR)  DEPTH(1:NLYR-1)  THK(1:NLYR-1)')
 13 FORMAT(T1,'/ UNITS=',A,3X,'NSTAT=',I5,3X,'NFRQ=',I2.2,3X,'NCMP=1')
 14 FORMAT(T1,'/ FREQS(Hz) =',60F13.2)
 16 FORMAT(T1,'/ SURVEY=FD_AEM  PP=RX_POS')
 17 FORMAT(T1,'/ LINE_HEADER:     Station  EastTx  NorthTx  AltTx  EastRx  NorthRx  AltRx  ',1024(A,I2.2))
 20 FORMAT(T1,'/ MODEL_HEADER: ', 3x, 2048(4x, a, i2.2))
 22 FORMAT(T1,'/'/ T1,'/ INITIAL_MODEL: ',84G13.4)
 23 Format(t1, '/', /, t1, '/ ', 2048('-'))

   END SUBROUTINE WRITE_NP_INITIAL

END MODULE BA_Input_routines

 PROGRAM MAIN
!------------

!*** Calls DCPRM_FD, DCPRM_TD, HSBOSS_TD, HSBOSS_FD, NLSQ2,
!          SET_NORM_TD, SET_SOURCE, WRITE_FD, WRITE_TD, WRITE_MODEL,
!          WRITE_LOG_FILE

!*** Calls from BA_Input_routines:
!          READ_SYSTEM_AND_SURVEY, READ_MODEL, READ_INVRT_CNTRL_AND_DATA
!          SET_SURVEY, SET_TRP, WRITE_NP_INITIAL

 Use iso_Fortran_env
 USE BA_Input_routines

 IMPLICIT NONE
 INTEGER IDER, KNRM, MPRNT
 REAL, ALLOCATABLE, DIMENSION(:) :: NORM
 REAL, ALLOCATABLE, DIMENSION(:,:,:) :: BTD
 COMPLEX, ALLOCATABLE, DIMENSION(:,:,:) :: BFD
 REAL CMP_begin, CMP_final, CMP_delta
 LOGICAL WRT_NP

 CALL CPU_TIME (CMP_begin)
 CALL READ_SYSTEM_AND_SURVEY
 CALL READ_MODEL

 IF (INVERT) THEN
   OPEN(NRI,FILE = 'Airbeo.inv',STATUS = 'OLD')
   OPEN(NP,FILE = 'Airbeo.mv1',STATUS = 'REPLACE')
   OPEN(MD1,FILE = 'Airbeo.mdl',STATUS = 'REPLACE')
   CALL READ_INVRT_CNTRL_AND_DATA
 ELSE
   MPRNT = 100
   JS = 0
   CALL WRITE_MODEL (NW,MPRNT,JS,NLYR,THK,RES,CHRG,CTAU,CFREQ,RMU,REPS)
   OPEN(NP,FILE = 'Airbeo.mf1',STATUS = 'REPLACE')
 END IF

 CALL SET_SURVEY
 WRT_NP = .TRUE.
 IF (TDFD == 2 .AND. CMP > 1)  WRT_NP = .FALSE.
 IF (WRT_NP) CALL WRITE_NP_INITIAL

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

 IF (TDFD == 1) THEN   ! Time-Domain

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
     CALL SET_SOURCE (nw, STEP,ISW,BFFAC,WAVEFORM,NSX,SWX,SWY,PRM_TD)

     IF (INVERT) CALL SET_NORM_TD (NW,BUNIT,BFFAC,KPPM,PUNIT,PPFAC,PRM_TD,NORM)
   END IF
 ELSE
   KNRM = NFRQ
   ALLOCATE (NORM(KNRM))
   CALL DCPRM_FD (NFRQ,XRX,YRX,ZRX,TXCLN,TXA90,PRM_FD,PPFAC,NORM)
 END IF

!============================================================================
 IF (INVERT) THEN

   DO JS = 1,NSTAT
     IF (DO1D == 1) THEN
       RES = RES0
       THK = THK0
     END IF
     DO JT = 1,NDATA
       XDATA(JT) = RDATA(JT,JS)
       XWTS(JT) = RWTS(JT,JS)
     END DO
!      IF (MAXVAL (XWTS) < 1) THEN
!        WRITE(NW,10) JS; WRITE(*,10) JS
!        CYCLE
!      END IF
     CALL NLSQ2 (JS,NW,NP,NLG,MV1PRT,OUTPRT,MAXITS,CNVRG,PCTCNV,NDATA,XDATA,XMODL, &
                 XWTS,NPAR,CXPAR,ELAS,LBND,UBND,TDFD,CMP,KNRM,NORM,STEP,IDER,NSX,   &
                 SWX,SWY,NTYRP,TRP,NPULS,PULSE,NTYPLS,NCHNL,TOPN,TCLS,GSTRP,ASTRP,  &
                 NFRQ,FREQ,TXCLN,TXA90,NRX,NRXST,XRX,YRX,ZRX,NSTAT,SX,SY,SZ,RX,RY,  &
                 RZ,GND_LVL,TITLE,LINE,NLYR,THK,RES,RMU,REPS,CALF,CTAU,CFREQ,MPAR)

     RES(1:NLYR) = MPAR(1:NLYR)
     THK(1:NLYR-1) = MPAR(NLYR+1:NPAR)
     CALL CNVRT2_DEPTH (NLYR,THK,DEPTH)
     WRITE(MD1, 14) js,SYD(JS),SXD(JS), sz(js), RES(1:NLYR),DEPTH(1:NLYR-1),THK(1:NLYR-1)

   END DO
!===================================================================================

 ELSE         ! FORWARD MODEL OPTION

   CLOSE (NR)

   IF (TDFD == 1) THEN   ! Time-Domain.
     ALLOCATE (BTD(NCHNL,NSTAT,3))
     BTD = 0.

!  Compute BTD, the layered earth response convolved with the excitation waveform
!  as dB/dt in nT/s if STEP = 0;  or as B in pT if STEP = 1

     DO JS = 1,NSTAT
       CALL HSBOSS_TD (JS,STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL, &
                       TOPN,TCLS,TXCLN,NSTAT,SZ,ZRX,XRX,YRX,NLYR,RES,REPS,RMU,THK,  &
                       CALF,CTAU,CFREQ,GSTRP,ASTRP,BTD)
     END DO

!  Write out the results.

     CALL WRITE_TD (NW,NP,TITLE,NSTAT,LINE,SXD,SYD,SZ,TXDEG,RXD,RYD,RZ,XRX,YRX,ZRX,NCHNL, &
                    TMS,PRFL,QUNIT,BUNIT,PPFAC,BFFAC,PRM_TD,CMP,KPPM,BTD)

   ELSE  !  Construct the frequency-domain response.

     ALLOCATE (BFD(NFRQ,NSTAT,3))
     BFD = (0.,0.)

     DO JS = 1,NSTAT
       CALL HSBOSS_FD (JS,NFRQ,FREQ,TXCLN,TXA90,NSTAT,SZ,ZRX,XRX,YRX,NLYR, &
                       RES,REPS,RMU,THK,CALF,CTAU,CFREQ,BFD)
     END DO
     CALL WRITE_FD (NW,NP,TITLE,NSTAT,LINE,TXCLN,TXA90,SXD,SYD,SZ,RXD,RYD,RZ,CONFIG,NFRQ, &
                    FREQ,PRFL,QUNIT,PPFAC,BFFAC,PRM_FD,CMP,BFD)
   END IF
 END IF

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

 CLOSE (NW); Close (MD1); Close (np)
 STOP

10 FORMAT (//T14,'==============================' &
            /T15,'No inversion for station',I4    &
            /T14,'==============================')
11  Format ('/', / &
            '/ ', a, ' ', a, ' run completed at ', i4.4, '-', i2.2, '-', i2.2, 'T', i2.2, ':', i2.2, ':', i2.2, / &
            '/ Runtime: ', f12.2, ' secs')
12  Format (/, 2x, a, ' ', a, ' run completed at ', i4.4, '-', i2.2, '-', i2.2, 'T', i2.2, ':', i2.2, ':', i2.2, &
		    /, 2x, 'Runtime: ', f12.2, ' seconds', /)
14 FORMAT (I5, 3(2x, F12.3),100(2x, G13.4))
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


 USE BA_Filter_coefficients_QL

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
!        ZRX - vertical offset of RX relative to transmitter (below = + ).
!        XRX - in-line offset of RX relative to transmitter (behind = + ).
!        YRX - transverse offset of RX relative to transmitter (port = + ).
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
!                        below = + ;  behind = + ;  port = +
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

 SUBROUTINE HSBOSS_TD (JS,STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL, &
                       TOPN,TCLS,TXCLN,NSTAT,SZ,ZRX,XRX,YRX,NLYR,RES,REPS,RMU,THK,  &
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
!         JS = station reference
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
!         SZ - array of transmitter altitudes
!        ZRX - vertical offset of RX at each station from transmitter  (below = +)
!        XRX - in-line horizontal offset of RX at each stationJ;  (behind = +)
!        YRX - transverse offset of RX at each stationJ (port = +)
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

 USE BA_Frequency_select

 IMPLICIT NONE
 INTEGER, PARAMETER :: NFRQ=NF_6PDE, NRXF=1, QL=SELECTED_REAL_KIND(p=18)
 REAL, PARAMETER :: TWOPI=6.283185307
 INTEGER STEP,IDER,NSX,NPULS,NTYPLS,NTYRP,NCHNL,NSTAT,NLYR,TDFD,GSTRP,ASTRP, &
         JS,JF,JT,JC
 REAL SWX(NSX),SWY(NSX,3),PULSE,TRP(NTYRP),SZ(NSTAT),ALT,T,YPRM(4,NTYRP),   &
      YCUM(NCHNL),YFRQ(4,NFRQ),FREQ(NFRQ),WF(NFRQ),COSTRN,BTD(NCHNL,NSTAT,3)
 REAL, DIMENSION(NCHNL) :: TOPN,TCLS
 REAL, DIMENSION(NLYR) :: RES,REPS,CTAU,CFREQ,CALF,RMU
 REAL, DIMENSION(NLYR-1) :: THK
 REAL, DIMENSION(NSTAT) :: TXCLN,ZRX,XRX,YRX
 REAL(KIND=QL), DIMENSION(NRXF) :: XRXD,YRXD,ZRXD,TXCLND
 COMPLEX(KIND=QL) BFDD(NFRQ,3)
 COMPLEX BFD
 LOGICAL TXA90

 TDFD = 1
 TXA90 = .FALSE.

 FREQ(1:NFRQ) = FRQ_6PDE(1:NFRQ)
 WF(1:NFRQ) = LOG (TWOPI * FREQ(1:NFRQ))

 TXCLND(1) = REAL (TXCLN(JS), KIND=QL)
 ALT = SZ(JS)
 ZRXD(1) = REAL (ZRX(JS), KIND=QL)
 XRXD(1) = REAL (XRX(JS), KIND=QL)
 YRXD(1) = REAL (YRX(JS), KIND=QL)

 CALL HSMD_FD (NFRQ,FREQ,ALT,NRXF,TXCLND,TXA90,ZRXD,XRXD,YRXD,NLYR, &
               THK,RES,REPS,RMU,CALF,CTAU,CFREQ,TDFD,BFDD)

!  Compute BTD, the 'observed' layered earth response by folding the BLEXT,
!  the extended response over NPULS bipolar cycles into 1 PULSE and then
!  convolving this with the TX waveform.  It is during the convolution that we
!  shift from teslas to nanoteslas or nT/s.

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

END SUBROUTINE HSBOSS_TD

 SUBROUTINE HSBOSS_FD (JS,NFRQ,FREQ,TXCLN,TXA90,NSTAT,SZ,ZRX,XRX,YRX,NLYR, &
                       RES,REPS,RMU,THK,CALF,CTAU,CFREQ,BFD)
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
!        JS = station reference
!      FREQ - array of NFRQ frequencies
!     TXCLN - angle in radians that TX dipole makes with vertical (climb = +)
!     TXA90 - true for vertical co-planar briadside array
!     NSTAT - number of stations in survey line.
!        SZ - array of transmitter altitudes
!       ZRX - vertical offset of each receiver from transmitter  (below = +)
!       XRX - in-line horizontal offset of RX J;  (behind = +)
!       YRX - transverse horizontal offset of RX J (port = +)
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
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p=18)
 INTEGER NFRQ,NSTAT,NLYR,JS,TDFD,NRXF
 REAL SZ(NSTAT),ALT
 REAL, DIMENSION (NFRQ) :: FREQ,TXCLN,ZRX,XRX,YRX
 REAL, DIMENSION(NLYR) :: RES,RMU,REPS,CTAU,CFREQ,CALF
 REAL, DIMENSION(NLYR-1) :: THK
 REAL(KIND=QL), DIMENSION(NFRQ) :: XRXD,YRXD,ZRXD,TXCLND
 COMPLEX(KIND=QL) BFDD(NFRQ,3)
 COMPLEX BFD(NFRQ,NSTAT,3)
 LOGICAL TXA90

!  Compute layered earth fields BLE_LYR at first station for each different altitude.

 TDFD = 2
 NRXF = NFRQ
 ZRXD(1:NFRQ) = REAL (ZRX(1:NFRQ), KIND=QL)
 XRXD(1:NFRQ) = REAL (XRX(1:NFRQ), KIND=QL)
 YRXD(1:NFRQ) = REAL (YRX(1:NFRQ), KIND=QL)
 TXCLND(1:NFRQ) = REAL (TXCLN(1:NFRQ), KIND=QL)

 ALT = SZ(JS)
 CALL HSMD_FD (NFRQ,FREQ,ALT,NRXF,TXCLND,TXA90,ZRXD,XRXD,YRXD,NLYR, &
               THK,RES,REPS,RMU,CALF,CTAU,CFREQ,TDFD,BFDD)

 BFD(1:NFRQ,JS,1:3) = CMPLX (BFDD(1:NFRQ,1:3))

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
!      XRXD - in-line horizontal offset (QL)  of RX J;  (behind = +)
!      YRXD - transverse horizontal offset (QL) of RX J (port = +)
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

 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p=18)
 REAL, PARAMETER :: TWOPI=6.2831853, C_LIGHT = 2.99793E8
 COMPLEX(KIND=QL), PARAMETER :: ZERO=(0._QL,0._QL)
 INTEGER NRXF,NFRQ,NLYR,ICOLE(NLYR),JF,JL,TDFD,JQ
 REAL W,RES(NLYR),FREQ(NFRQ),ALT
 REAL(KIND=QL) SIG0(NLYR),THKD(NLYR-1),RMUX(NLYR),SNTX,CSTX,ZRFD,RHOD,XBRQ,XBRQ2,YBRQ
 REAL(KIND=QL), DIMENSION(NRXF) :: TXCLND,ZRXD,XRXD,YRXD
 REAL, DIMENSION(NLYR) :: REPS,RMU,CTAU,CFREQ,CALF
 REAL, DIMENSION(NLYR-1) :: THK
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

 USE BA_Filter_coefficients_QL

 IMPLICIT NONE
 REAL(KIND=QL), PARAMETER :: VFAC0=100._QL
 INTEGER NLYR,I,ICOLE(NLYR)
 REAL, DIMENSION(NLYR) :: REPS,CTAU,CFREQ,CALF
 REAL(KIND=QL) DEL_JN,RHO_JN,Y,LMBDA,RHOD,ZRFD,SIG0(NLYR),THKD(NLYR-1),RMUX(NLYR)
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
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p=18)
 REAL(KIND=QL), PARAMETER :: EPS0=8.854156D-12, MU0=12.56637D-7, EXP_TOL=80.D0
 COMPLEX(KIND=QL), PARAMETER :: ONE=(1._QL,0._QL),ZERO=(0._QL,0._QL)
 INTEGER NLYR,ICOLE(NLYR),J
 REAL, DIMENSION(NLYR) :: CALF,CTAU,CFREQ,REPS
 REAL(KIND=QL) XP0,SIG0(NLYR),THKD(NLYR-1),LMBDA,ZRFD,RMUX(NLYR),RMUSQ(NLYR)
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
!   Airbeo computes all fields in nT or nT/s.  In order to match field data for
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

 IF (KPPM > 0) WRITE(NW,5) PUNIT, BUNIT, XQ(3),XQ(1),XQ(2),XQ(4)

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

 SUBROUTINE SET_SOURCE (nw, STEP,ISW,BFFAC,WAVEFORM,NSX,SWX,SWY,PRM_TD)
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
 INTEGER ISW,STEP,NSX,JT, nw
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

 Write (nw, 10) prm_td, coupling
 ! Write (nw, 12)
 ! Do jt = 1, NSX
 ! 	Write (nw, 11) jt, swx(jt), swy(jt, 1:3)
 ! End Do

! formats 
10	Format (//, 2x, 'Primary field (X, Y, Z): ', 3en16.6, /, &
			 /, 2x, 'Coupling: ', en15.6)
11	Format (2x, i6, 4(2x, en15.6))
12  Format (//, 7x, '#', 11x, 'Time', 10x, 'MdI/dt', 10x, 'Delta_I', 12x, 'dI/dt', &
		     /, 7x, '-', 11x, '----', 10x, '------', 10x, '-------', 12x, '------')

END SUBROUTINE SET_SOURCE

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
! NLG = Airbeo.log unit index
! MSG refers to error message index
! ERR_LVL = 1 for warnings;  = 2 for fatal errors
! MXERR = MAX (ERR_LVL)

 INTEGER ERR_LVL, MSG, NLG, MXERR

 IF (MXERR == 0) OPEN (NLG,FILE = 'Airbeo.log',STATUS = 'REPLACE')

 MXERR = MAX (ERR_LVL,MXERR)
 IF (ERR_LVL == 1) WRITE(NLG,501)
 IF (ERR_LVL == 2) WRITE(NLG,502)
 IF (MSG == 100) WRITE(NLG,'(2X)')

 IF (MSG ==   1) WRITE(NLG,1)
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
 IF (MSG ==  16) WRITE(NLG,16)
 IF (MSG ==  17) WRITE(NLG,17)
 IF (MSG ==  19) WRITE(NLG,19)

 IF (MSG ==  50) WRITE(NLG,50)
 IF (MSG ==  53) WRITE(NLG,53)
 IF (MSG ==  54) WRITE(NLG,54)
 IF (MSG ==  55) WRITE(NLG,55)

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

  1 FORMAT(/T3,'The value for TDFD is outside the permitted range.' &
           /T3,'The allowed values are: 1 for time-domain or 2 for frequency domain.'/)
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
 16 FORMAT(/T3,'SURVEY must be either 1, 2, or 3 for time-domain.' &
           /T3,'SURVEY = 3 cannot be used for frequency-domain because'  &
           /T3,'Tx-Rx offset must be constant as a function of position')
 17 FORMAT(/T3,'IUNITS must be 1, 2 or 3')
 19 FORMAT(/T3,'IUNITS must be 1, 2 or 3.  It has been reset to the default')

!  Model Messages

 50 FORMAT(/T3,'QLYR must = 1 (invert on layer thickness)')
 53 FORMAT(/T3,'A lithology must have a positive first component (resistivity) if it is to be' &
           /T3,'applied to a layer.')
 54 FORMAT(/T3,'LITH must be an integer between 1 & NLITH')
 55 FORMAT(/T3,'Layer resistivities must be positive.')

! Inversion messages
! -------------------

 201 FORMAT(/T3,'This version of Airbeo does not invert for AEM system parameters.')
 202 FORMAT(/T3,'X component inversion was requested but Airbeo.inv contains only Z component data.')
 203 FORMAT(/T3,'Z component inversion was requested but Airbeo.inv contains only X component data.')
 204 FORMAT(/T3,'The maximum number of iterations has been reduced to 20.')
 205 FORMAT(/T3,'CNVRG must be 1 or 2.  It has been reset to 1.')
 206 FORMAT(/T3,'For inversion, aircraft positions must be entered for every station', &
            /T3,'The automatic course option, SURVEY = 1 is not allowed.', &
            /T3,'SURVEY must be either 2, 3, -2, or -3.'/)
 207 FORMAT(/T3,'KCMP must = 12 or 21 for Frequency-domain inversion.')
 208 FORMAT(/T3,'ORDER must = 1 or 2.')
 209 FORMAT(/T3,'KCMP is restricted to the values: 1, 3, 13, 31, 123, 312')
 210 FORMAT(/T3,'X & Z component inversion was requested but Airbeo.inv contains only X component data.')
 211 FORMAT(/T3,'X & Z component inversion was requested but Airbeo.inv contains only Z component data.')
 212 FORMAT(/T3,'3 component inversion was requested but Airbeo.inv contains only X component data.')
 213 FORMAT(/T3,'3 component inversion was requested but Airbeo.inv contains only Z component data.')
 214 FORMAT(/T3,'3 component inversion was requested but Airbeo.inv contains no Y component data.')
 215 FORMAT(/T3,'There is a component discrepency between the cfl and inv files.')
 220 FORMAT(/T3,'Order must be 1122 (for IIQQ),  1212 (for IQIQ),  2211 (for QQII),  2121 (for QIQI)')

 501 FORMAT(/T2,'WARNING'/T2,'-------')
 502 FORMAT(/T2,'FATAL ERROR'/T2,'----- -----')

END SUBROUTINE  WRITE_LOG_FILE

 SUBROUTINE WRITE_FD (NW,NP,TITLE,NSTAT,LINE,TXCLN,TXA90,SXD,SYD,SZ,RXD,RYD,RZ,CONFIG,NFRQ, &
                      FREQ,PRFL,QUNIT,PPFAC,BFFAC,PRM_FD,CMP,BFD)
!------------------------------------------------------------------------------------------

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

 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 INTEGER NW,NP,NSTAT,LINE(NSTAT),NFRQ,CMP,PRFL,TXD(NFRQ),JF,JS
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
!   BFD1SC(JF,JS,1:4) contains the scattered fields

! Print results at Tx-Rx midpoint

 MXD(1:NSTAT) = (RXD(1:NSTAT,1) + SXD(1:NSTAT) ) /2.
 MYD(1:NSTAT) = (RYD(1:NSTAT,1) + SYD(1:NSTAT) ) /2.
 MPZ1(1:NSTAT) =  (RZ(1:NSTAT,1)  + SZ(1:NSTAT) )  / 2.
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

!  maximally coupled response
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

!  Finish writing Airbeo.mf1

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

 1 FORMAT(//T3,'FREQUENCY-DOMAIN Airbeo OUTPUT' /T3,33('-'))
 3 FORMAT(/T3,'SINGLE COMPONENT RESPONSE ALONG TRANSMITTER DIPOLE DIRECTION')
 4 FORMAT(//T10,A,'COMPONENT - ',A)
 8 FORMAT(/T3,'The IN-LINE component is defined as the horizontal component along' &
          /T3,'the flight path.  It is positive in the forward flight direction.')
 9 FORMAT(/T3,'The TRANSVERSE component is defined as the horizontal component',&
          /T3,'perpendicular to the flight path.'/)
 10 FORMAT(//T4,'Frequency    Coupled Primary   Normalisation' &
            /T4,'Index        Field (pT)        Factor'        &
            /T4,'---------    ---------------   ------------')
 14 FORMAT(/T10,2A,' COMPONENT - ',A)
 15 FORMAT(/T3,'TITLE:  ',A/T3,'-----')
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
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
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
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p=18), NCOL=40
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
                      TMS,PRFL,QUNIT,BUNIT,PPFAC,BFFAC,PRM_TD,CMP,KPPM,BTD)
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
!          YRX (JS) - port offset of distance receiver at station JS
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

 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 REAL, PARAMETER ::  TOL=1.E-3
 INTEGER NW,NP,PRFL,NSTAT,LINE(NSTAT),NCHNL,MCHNL,CMP,KPPM,JC,JS
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
 CHARACTER (LEN=5) ZLOC(2),ZLC,YLC
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

 NORM(1) = ABS (PRM_TD(1))
 NORM(2) = ABS (PRM_TD(2))
 NORM(3) = ABS (PRM_TD(3))
 NORM(4) = SQRT (NORM(1)**2 + NORM(2)**2 + NORM(3)**2)
 IF (KPPM == 0) THEN                 !  Compute fields in requied units
   BTD =      BFFAC * BTD
 ELSE IF (KPPM > 0) THEN            !  Compute normalised response
   DO JC = 1,3
     XRN = NORM(JC) / NORM (4)
     IF (XRN < TOL) NORM(JC) = NORM(4)
   END DO

   IF (KPPM == 4) NORM(1:3) = NORM(4)                   ! TOTAL primary normalisation
   IF (KPPM == 1 .OR. KPPM == 3) NORM(1:3) = NORM(KPPM) ! vertical or in-line normalisation

   DO JC = 1,3
     BTD(1:NCHNL,1:NSTAT,JC) =      PPFAC * BTD(1:NCHNL,1:NSTAT,JC)      / NORM(JC)
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

!  Finish writing Airbeo.mf1

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

 1 FORMAT(//T3,'TIME-DOMAIN Airbeo OUTPUT'/T3,28('-') &
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
 41 FORMAT(/T3,'The TRANSVERSE component is the horizontal component' &
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

 SUBROUTINE WRITE_MODEL (NW,MPRNT,JS,NLYR,THK,RES,CHRG,CTAU,CFREQ,RMU,REPS)
!-------------------------------------------------------------------------------

!***  Called by: MAIN, NLSQ2

!      NW - output unit
!   MPRNT = 100 - forward model
!         =  0 - initial model before inversion
!         =  ITS - model after ITS iteations
!         = -ITS - final model after ITS iterations
!    NLYR - number of layers
!     THK - layer thicknesses
!     RES - array of layer resistivities
!    REPS - relative dielectric constant
!     RMU - mu(i) / mu(0)
!    CHRG -  chargeability
!    CTAU - layer time constants
!   CFREQ - layer frequency constants

 IMPLICIT NONE
 INTEGER NW,MPRNT,NLYR,J,JS
 REAL, DIMENSION(NLYR) :: RES,CTAU,CFREQ,RMU,REPS,CHRG
 REAL, DIMENSION(NLYR-1) :: THK,DEPTH,CND
 LOGICAL FULL

 IF (MPRNT == 100) THEN
   WRITE(NW,1)
 ELSE
   ! IF (MPRNT == 0) WRITE(*,2)  JS
   ! IF (MPRNT > 0) WRITE(NW,3) MPRNT,JS
   ! IF (MPRNT < 0) WRITE(NW,4) ABS (MPRNT),JS
 END IF

 IF (NLYR > 1) CALL CNVRT2_DEPTH (NLYR,THK,DEPTH)
 FULL = .FALSE.
 DO J = 1,NLYR-1
   CND(J) = THK(J) / RES(J)
 END DO

 IF (MAXVAL (CHRG) > 1.E-4) FULL = .TRUE.
 IF (MAXVAL (RMU) >  1.0001) FULL = .TRUE.
 IF (MAXVAL (REPS) > 1.0001) FULL = .TRUE.
 IF (FULL) THEN
   IF (NLYR > 1) THEN
     WRITE(NW,5)
     DO J = 1,NLYR-1
       WRITE(NW,9) J,RES(J),DEPTH(J),THK(J),CND(J),RMU(J),REPS(J),CHRG(J),CTAU(J),CFREQ(J)
       ! IF (MPRNT < 100) WRITE(*,9)  J,RES(J),DEPTH(J),THK(J),CND(J)
     END DO
     J = NLYR
     WRITE(NW,10) J,RES(J),RMU(J),REPS(J),CHRG(J),CTAU(J),CFREQ(J)
     ! IF (MPRNT < 100) WRITE(*,10)  J,RES(J)
   ELSE
     WRITE(NW,7)
     J = NLYR
     WRITE(NW,19) RES(J),RMU(J),REPS(J),CHRG(J),CTAU(J),CFREQ(J)
   END IF
 ELSE
   IF (NLYR > 1) THEN
     WRITE(NW,6)
     DO J = 1,NLYR-1
       WRITE(NW,9) J,RES(J),DEPTH(J),THK(J),CND(J)
       ! IF (MPRNT < 100) WRITE(*,9)  J,RES(J),DEPTH(J),THK(J),CND(J)
     END DO
     J = NLYR
     WRITE(NW,10) J,RES(J)
     ! IF (MPRNT < 100) WRITE(*,10)  J,RES(J)
   ELSE
     WRITE(NW,8) RES(1)
   END IF
 END IF

 1 FORMAT(//T9,'Model Description' /T9,'=================')
 2 FORMAT(//T9,'Initial Model Before Inversion for Station',I8, &
           /T9,'----------------------------------------------')
 3 FORMAT(//T9,'Model after',I3,' Iterations for Station',I8, &
           /T9,'-----------------------------------------')
 4 FORMAT(//T9,'Final Model after',I3,' Iterations for Station',I8, &
           /T9,'===============================================')
 5 FORMAT(/T2,'Layer  Resistivity  Depth  Thickness  Conductance   MU-R   EPS-R   CHRG    CTAU      CFREQ' &
          /T2,'-----  -----------  -----  ---------  -----------   ----   -----   ----    ----      -----')
 6 FORMAT(/T2,'Layer  Resistivity  Depth  Thickness  Conductance' &
          /T2,'-----  -----------  -----  ---------  -----------')
 7 FORMAT(/T2,'Resistivity   MU-R   EPS-R   CHRG    CTAU      CFREQ' &
          /T2,'-----------   ----   -----   ----    ----      -----')
 8 FORMAT(/T3,'Host resistivity =',G15.4)
  9 FORMAT(I4,G15.4,F7.1,F9.1,T39,G12.4,2F7.2,F8.2,G11.2,F7.2)
 10 FORMAT(I4,G15.4,28X,2F7.2,F8.2,G11.2,F7.2)
 19 FORMAT(G12.4,2F7.2,F8.2,G11.2,F7.2)

 END SUBROUTINE WRITE_MODEL

!==================================================================================
!  ROUTINES SPECIFIC FOR INVERSION
!  -------------------------------

 SUBROUTINE CNVRT_BOUNDS (NPAR,LBND,UBND,CXPAR,XLBND,XUBND)
!----------------------------------------------------------

!*** Called by NLSQ2

! Converts the LBND & UBND specified by the user into the form used
! by the inversion, XLBND & XUBND.

! Bounds are only applied to layer parameters for CXPAR = 3

!  INPUT:  NPAR,LBND,UBND,CXPAR
! OUTPUT:  XLBND,XUBND

 INTEGER NPAR,CXPAR(NPAR),J1
 REAL,DIMENSION(NPAR) :: LBND,UBND,XLBND,XUBND

 DO J1 = 1, NPAR
   IF (CXPAR(J1) == 3) THEN
     XLBND(J1) = LOG(LBND(J1))
     XUBND(J1) = LOG(UBND(J1))
   END IF
 END DO

END SUBROUTINE CNVRT_BOUNDS

 SUBROUTINE CNVRT2_DEPTH (NLYR,THK,DEPTH)
!----------------------------------------

!*** Called by: WRITE_MODEL, MAIN

!   NLYR - number of layers
!    THK - array of layer thicknessess (input)
!  DEPTH  THK - array of layer depths  (output)

 INTEGER J1,NLYR
 REAL, DIMENSION(NLYR-1) :: THK, DEPTH

 DEPTH(1) = THK(1)
 DO J1 = 2,NLYR-1
   DEPTH(J1) = DEPTH(J1-1) + THK(J1)
 END DO

 END SUBROUTINE CNVRT2_DEPTH

 SUBROUTINE CNVRT2_MPAR (NPAR,NLYR,XPAR,RES,THK)
!-----------------------------------------------

!*** Called by: NLSQ2

!  Converts inversion parameters to model parameters
!
!      NLYR = number of layers = 1 or 2
!      NPAR =  2*NLYR-1
!       RES = layer resistivities
!       THK - layer thicknesses
!      XPAR = transformed parameters

 INTEGER NPAR,NLYR
 REAL XPAR(NPAR),RES(NLYR),THK(NLYR-1)

 RES(1:NLYR) = EXP (XPAR(1:NLYR))
 THK(1:NLYR-1)  = EXP (XPAR(NLYR+1:NPAR))

 END SUBROUTINE CNVRT2_MPAR

 SUBROUTINE CNVRT2_XPAR (NPAR,NLYR,RES,THK,XPAR)
!-----------------------------------------------

!*** Called by: NLSQ2

!  Converts from model parameters to inversion parameters

!      NLYR = number of layers = 1 or 2
!      NPAR =  2*NLYR-1
!       RES = layer resistivities
!       THK - layer thicknesses
!      XPAR = transformed parameters

 INTEGER NLYR,NPAR
 REAL XPAR(NPAR),RES(NLYR),THK(NLYR-1)

 XPAR(1:NLYR) = LOG (RES(1:NLYR))
 XPAR(NLYR+1:NPAR) = LOG (THK(1:NLYR-1))

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

 SUBROUTINE FORJAC (NDATA,XDATA,XMODL,XWTS,NPAR,XPAR,CXPAR,SUMSQ,JCBN,A,VERR,TDFD,CMP,  &
                    STEP,IDER,NSX,SWX,SWY,NTYRP,TRP,NPULS,PULSE,NTYPLS,NCHNL,TOPN,TCLS, &
                    GSTRP,ASTRP,NFRQ,FREQ,KNRM,NORM,JS,TXCLN,TXA90,XRX,YRX,ZRX,NRXST,   &
                    NSTAT,SZ,NLYR,RMU,REPS,CALF,CTAU,CFREQ)
!--------------------------------------------------------------------------------------

!  Sets up and calls for forward model computation.
!  It also calculates the Jacobian and error vector if required
!  New convention: Dec, 2003: VERR is now VD - VM
!  Thus DELPAR is now added rather than subtracted during updates.

!*** Called by: NLSQ2
!***     Calls: SET_CELLS, SHIFT_CELLS, GET_FWD_MODL, CNVRT2_MPAR

!             General Inversion Input Variables
!             ---------------------------------
!
!          JS - station index
!       NDATA - dimension of vector to be inverted: = NCMP * NCHNL or 2*NFRQ.
!       XDATA - data to be inverted in user-specified units
!       XMODL - model data in user-specified units
!        XWTS - weights for XDATA  (0 or 1)
!        NPAR - number of parameters to be inverted (nominally 2*NLYR - 1)
!        XPAR - array of transformed model parameters
!       CXPAR = 0 => parameter is completely free to vary as dictated by inversion step
!             = 1 => parameter is fixed
!             = 2 => parameter is constrained by elasticity.
!             = 3 => parameter bounds are buffered.
!       SUMSQ - sum squared scaled symmetric error
!        JCBN - true if Jacobian required
!           A - a large array which carries the Jacobian out of this routine
!     VERR(J) - scaled symmetric error in channel J
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
!        TOPN - time at which receiver channel I opens.
!        TCLS - time at which receiver channel I closes.
!       GSTRP = 1 => apply Questem-Geotem stripping algorithm
!       ASTRP = 1 => apply Aerotem stripping algorithm
!        FREQ = array of NFRQ frequencies
!       TXCLN - angle in radians that TX dipole makes with vertical (climb = +)
!       TXA90 - true for vertical co-planar briadside array
!       NRXST - dimension for receiver offset & transmitter tilt
!             = NFRQ in FD;  = NSTAT in TD
!         ZRX - vertical receiver offset for each frequency; below = positive
!         XRX - in-line receiver offset for each frequency;  behind = positive
!         YRX - transverse receiver offset for each frequency; Port = positive.
!
!             AEM Survey Input Variables
!             --------------------------
!
!       NSTAT - number of stations in survey line.
!          SZ - altitude (re gnd level) of transmitter
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
!
!          PHYSICAL PARAMETER 0RDERING
!          ---------------------------
!
!          1 : NLYR      - resistivities
!          NLYR+1 : NPAR - thicknesses
!
!
!          TRANSFORMED PARAMETERS FOR INVERSION  - Logarithms
!          ------------------------------------


 IMPLICIT NONE
 INTEGER NDATA,NPAR,NLYR,CXPAR(NPAR),NCHNL,TDFD,CMP,KNRM,NFRQ, &
         NSTAT,NRXST,STEP,IDER,NSX,NPULS,NTYPLS,NTYRP,GSTRP,ASTRP,JS,JD,JP,JP1, &
         JF,JT,NC1,NC2,NC3
 REAL SUMSQ,A(NDATA,NPAR+1),XPAR(NPAR),SZ(NSTAT),NORM(KNRM),FREQ(NFRQ),SWX(NSX), &
      SWY(NSX,3),PULSE,TOPN(NCHNL),TCLS(NCHNL),TRP(NTYRP),X2,X3,VM,VJ,VD,DENOM,DELTA,   &
      XP0,PARFAC
 REAL :: XWTS(NDATA)
 REAL, DIMENSION(NDATA) ::  XMODL,XMODL0,XDATA,VERR
 REAL, DIMENSION(NRXST) :: TXCLN,XRX,YRX,ZRX
 REAL, DIMENSION(NLYR) :: RES,REPS,RMU,CALF,CTAU,CFREQ
 REAL, DIMENSION(NLYR-1) :: THK
 REAL, DIMENSION(NCHNL,NSTAT,3) :: BTD
 COMPLEX XBFD
 COMPLEX, DIMENSION(NFRQ,NSTAT,3) :: BFD
 LOGICAL JCBN,TXA90

! Compute initial forward model & compute error

 CALL CNVRT2_MPAR (NPAR,NLYR,XPAR,RES,THK)

 NC1 = NCHNL
 NC2 = 2*NCHNL
 NC3 = 3*NCHNL
 JP1 = 0
 PARFAC = 1.0
 CALL GET_FWD_MODL(JP1,PARFAC)
 XMODL0 = XMODL

 SUMSQ = 0.
 DO JD = 1, NDATA
   VERR(JD) = 0.
   IF( XWTS(JD) > 0) THEN
     VD = XDATA(JD)
     VM = XMODL0(JD)
     DENOM = SQRT( (VM*VM + VD*VD)/2.0)
     VERR(JD) = XWTS(JD) * (VD-VM) / DENOM
     SUMSQ = SUMSQ + VERR(JD)**2
   END IF
 END DO

!  Initialise and then compute the Jacobian as the derivative of log(volts) wrt
!  log(parameter) for a three percent step.  Skip over held parameters

 IF (JCBN) THEN
   A = 0.
   PARFAC = 0.03

   DO JP1 = 1,NLYR                  ! Vary resistivities.
     IF (CXPAR(JP1) /= 1) THEN
       XP0 = RES(JP1)
       RES(JP1) = (1. + PARFAC) * RES(JP1)
       CALL GET_FWD_MODL(JP1,PARFAC)
       RES(JP1) = XP0
     END IF
   END DO
   DO JP = 1,NLYR-1
     JP1 = NLYR + JP                  ! Vary layer dimesnsion.
     IF (CXPAR(JP1) /= 1) THEN
       XP0 = THK(JP)
       THK(JP) = (1. + PARFAC) * THK(JP)
       CALL GET_FWD_MODL(JP1,PARFAC)
       THK(JP) = XP0
     END IF
   END DO
 END IF

 CONTAINS

   SUBROUTINE GET_FWD_MODL (JP1,PARFAC)
!  ------------------------------------

!***  Called by: FORJAC
!***      Calls: TEM_3D, HSBOSS_TD, HSBOSS_FD

!  If JP1 = 0, performs forward model computation using existing parameters
!  If JP1 > 0, performs forward model computation where parameter JP1 is multiplied by
!             PARFAC and Jacobian column JP1 isconstructed.

 INTEGER JP1
 REAL PARFAC,CSTX(NRXST),SNTX(NRXST)

 IF (TDFD == 1) THEN

   CALL HSBOSS_TD (JS,STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL, &
                   TOPN,TCLS,TXCLN,NSTAT,SZ,ZRX,XRX,YRX,NLYR,RES,REPS,RMU,THK,  &
                   CALF,CTAU,CFREQ,GSTRP,ASTRP,BTD)

   IF (CMP == 13 .OR. CMP == 2 .OR. CMP == 3) &
       XMODL(1:NC1) = NORM(3) * BTD(1:NCHNL,JS,3)

   IF (CMP == 2 .OR. CMP == 3) &
         XMODL(NC1+1 : NC2) = NORM(1) * BTD(1:NCHNL,JS,1)

   IF (CMP == 3) XMODL(NC2+1 : NC3) = NORM(2) * BTD(1:NCHNL,JS,2)

   IF (CMP == 11) XMODL(1:NC1) = NORM(1) * BTD(1:NCHNL,JS,1)

   IF (CMP == 4 .OR. CMP == 42) THEN
     DO JT = 1,NCHNL
       X2 = BTD(JT,JS,1)**2 + BTD(JT,JS,3)**2
       X3 = X2 + BTD(JT,JS,2)**2
       IF (CMP == 42) THEN
         XMODL(JT) = SQRT (X2)
       ELSE
         XMODL(JT) = SQRT (X3)
       END IF
     END DO
   END IF

 ELSE

   CALL HSBOSS_FD (JS,NFRQ,FREQ,TXCLN,TXA90,NSTAT,SZ,ZRX,XRX,YRX, &
                   NLYR,RES,REPS,RMU,THK,CALF,CTAU,CFREQ,BFD)

!  Store maximally coupled components in XMODL

   CSTX(1:NFRQ) = COS (TXCLN(1:NFRQ))
   SNTX(1:NFRQ) = SIN (TXCLN(1:NFRQ))

   DO JF = 1,NFRQ
     IF (TXA90) THEN
       XBFD = NORM(JF) * BFD(JF,JS,2)
     ELSE
       XBFD = NORM(JF) * (BFD(JF,JS,1) * SNTX(JF) + BFD(JF,JS,3) * CSTX(JF))
     END IF
     XMODL(JF)        =  REAL (XBFD)
     XMODL(JF + NFRQ) = AIMAG (XBFD)
   END DO
 END IF

 IF (JP1 > 0) THEN
   DO JD = 1, NDATA
     VM = XMODL0(JD)
     VJ = XMODL(JD)
     DELTA = XWTS(JD) * (VJ - VM)
     DENOM = SQRT ((VM**2 + VJ**2)/ 2.)
     IF (ABS(DELTA) > 1.E-8 * DENOM) A(JD,JP1) = DELTA / (PARFAC * DENOM)
   END DO
 END IF

   END SUBROUTINE GET_FWD_MODL
 END SUBROUTINE FORJAC

 SUBROUTINE NLSQ2 (JS,NW,NP,NLG,MV1PRT,OUTPRT,MAXITS,CNVRG,PCTCNV,NDATA,XDATA,XMODL, &
                   XWTS,NPAR,CXPAR,ELAS,LBND,UBND,TDFD,CMP,KNRM,NORM,STEP,IDER,NSX,   &
                   SWX,SWY,NTYRP,TRP,NPULS,PULSE,NTYPLS,NCHNL,TOPN,TCLS,GSTRP,ASTRP,  &
                   NFRQ,FREQ,TXCLN,TXA90,NRX,NRXST,XRX,YRX,ZRX,NSTAT,SX,SY,SZ,RX,RY,  &
                   RZ,GND_LVL,TITLE,LINE,NLYR,THK,RES,RMU,REPS,CALF,CTAU,CFREQ,MPAR)
!---------------------------------------------------------------------------------------

!***  Called by: MAIN
!***      Calls: CNVRT_BOUNDS, CNVRT2_XPAR, ESVD, FORJAC, INDEX_MPAR,
!                NOISE_2_SIGR, PARAMETER_SENSITIVITY, SOLVE2, WRITE_MODEL,
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
!
!   Output: MPAR:  NLYR resistivities followed by NLYR-1 layer thicknesses
!
!
!
!             General Inversion Input Variables
!             ---------------------------------
!
!              JS - station index
!              NW - verbose output unit number
!             NP = plot file unit number
!             NLG = output unit for Airbeo.log
!          MV1PRT & OUTPRT are print options for the MV1 & OUT files respectively.
!                 =  0  No output DURING inversion.  The final model set AFTER inversion,
!                       but NOT the final model data, is written to output files.
!
!                 =  1  as above plue plus final model data
!
!                 =  2  as above plus intermediate model sets after each iteration
!
!                 =  3 as above plus intermediate model data after each iteration
!
!          MAXITS - maximum permitted iterations.
!           CNVRG = 1 => converge on predicted decrease etc
!                 = 2=> stop when RMS error < PCTCNV
!           NDATA = NCMP NCHNL or 2 * NFRQ
!           NDATA = total number of readings per station to be inverted(TD or FD)
!                 = the number of rows of the Jacobian matrix
!                 = NCHNL for time-domain when CMP = 11, 13, 4, 42, 43
!                 = 2* NCHNL for time-domain when CMP = 2
!                 = 3* NCHNL for time-domain when CMP = 3
!
!                 = 2 * NFRQ for frequency-domain
!           XDATA - Array of survey data points to be inverted.
!           XMODL - Array of model data points.
!            XWTS - Array of weights for XDATA (integer 0 or 1)
!           NPAR = 2*NLYR -1 = number of parameters
!           XPAR - On call, XPAR contains the log (initial parameter estimates).
!                  On exit it contains the final values.
!          CXPAR = 0 => parameter is completely free to vary as dictated by inversion step
!                = 1 => parameter is fixed
!                = 2 => parameter is constrained by elasticity.
!                = 3 => parameter bounds are buffered.
!           ELAS - [0 to 1] fraction of proposed step actually taken.
!     LBND, UBND - lower & upper bound of parameter
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
!         ZRX - vertical receiver offset for each frequency; below = positive
!         XRX - in-line receiver offset for each frequency;  behind = positive
!         YRX - transverse receiver offset for each frequency; Port = positive.
!
!             AEM Survey Input Variables
!             --------------------------
!
!       NSTAT - number of stations in survey line.
!       SX, SY, SZ: north, east and altitude (re gnd level) of transmitter
!       RX, RY, RZ: north, east and altitude (re gnd level) of receiver(s)
!
!     GND_LVL - ground level
!        LINE - line number
!
!             Model Description Input Variables
!             ---------------------------------
!
!        NLYR - number of layers (1 or 2)
!         THK - layer thicknesses
!         RES - array of layer resistivities
!         RMU - mu(i) / mu(0)
!        REPS - relative dielectric constant
!        CALF, CTAU & CFREQ are the layered earth Cole-Cole parameters.
!
!              Other Stuff
!              -----------
!
!              A -  Jacobian matrix
!            WSP - working space.
!           VERR - error vector
!         RMSERR - RMS error (in percent)
!            BND - estimate of noise to signal ratio, used for damping limit.
!            WSP - working space.
!     SV, UMAT, and VMAT are the decomposition of A.  SV contains eigenvalues.

!      The model set consists of the model parameters, parameter importance,
!      standard error and RSVT, the relative singular value threshold.
!
!      The model data (distinct from survey data) is defined as the fields or
!      ppm values for each channel or frequency for each station; ie the
!      forward model response for a given model.
!

 IMPLICIT NONE
 INTEGER, PARAMETER :: IP=1, QL=SELECTED_REAL_KIND(p=18)
 REAL, PARAMETER :: BND=0.01, EXPND=2., RSVT0=0.1, ETA=1.E-7, TOL=.5E-31, RAD2DEG=180./3.141592654
 INTEGER JS,NW,NP,NLG,MV1PRT,OUTPRT,ITS,CNVRG,NDATA,NPAR,CXPAR(NPAR), &
         TDFD,CMP,STEP,IDER,NSX,NPULS,NTYPLS,NTYRP,NCHNL,GSTRP,ASTRP,NFRQ,NSTAT,   &
         KNRM,NRXST,NRX,ICNT,MAXITS,NSV,FITS,JP,LINE(NSTAT),MPRNT,J1,MXERR
 REAL PCTCNV,A(NDATA,NPAR+1),UMAT(NDATA,NPAR),VMAT(NPAR,NPAR),WSP(3*NPAR),NSR,DRMS, &
      RMSERR,NORM(KNRM),SWX(NSX),SWY(NSX,3),TRP(NTYRP),PULSE,FREQ(NFRQ),GND_LVL,    &
      ZERO,DELT,RSVT,FNM,GCRIT,GTEST,PDRE,SSQNEW,SUMSQ,WGTSUM,B1,B2,BMID
 REAL :: XWTS(NDATA)
 REAL, ALLOCATABLE :: RMS1(:)
 REAL, DIMENSION(NRXST) :: TXCLN,TXDEG,XRX,YRX,ZRX
 REAL, DIMENSION(NSTAT,NRX) :: RX,RY,RZ
 REAL, DIMENSION(NPAR) :: SV,XPAR,MPAR,DELPAR,GXPAR,IMPORT,ELAS,LBND,UBND,XLBND,XUBND
 REAL, DIMENSION(NPAR) :: DMPFAC
 REAL, DIMENSION(NCHNL) :: TOPN,TCLS
 REAL, DIMENSION(NDATA) :: VERR,XMODL,XDATA,VERR_FILL
 REAL, DIMENSION(NSTAT) :: SX,SY,SZ,SZ0,RZ0
 REAL(KIND=QL), DIMENSION(NSTAT) ::SXD,SYD,RXD,RYD
 LOGICAL,PARAMETER :: WITHU=.TRUE., WITHV=.TRUE.
 LOGICAL JCBN,TXA90
 CHARACTER (LEN=120) CVAR,TITLE
 CHARACTER (LEN=80) QL0,QL1

! Model Specific
 INTEGER NLYR
 REAL THK(NLYR-1)
 REAL, DIMENSION(NLYR) :: RES,REPS,RMU,CHRG,CALF,CTAU,CFREQ

 Integer :: pi, pj
 Real :: CRAO(npar)
 Real :: df, stderr
 Real (Kind = 8) :: tempc
 Real :: a_orig(ndata, npar)

!  Preset threshold parameters and index workspace, but return if problem
!  size in error.  First write initial model

 TXDEG = RAD2DEG * TXCLN
 SXD = REAL (SX,QL)
 SYD = REAL (SY,QL)
 SZ0 = SZ + GND_LVL

 RXD(JS) = REAL (RX(JS,1),QL)
 RYD(JS) = REAL (RY(JS,1),QL)
 RZ0(JS) = RZ(JS,1) + GND_LVL

 VERR_FILL = 0.
 IF (MV1PRT == 3) CALL WRITE_XDATA

 MPRNT = 0
 CHRG = 1. - CALF
 CALL WRITE_MODEL (NW,MPRNT,JS,NLYR,THK,RES,CHRG,CTAU,CFREQ,RMU,REPS)
 WRITE(NP, 46) js, MPAR(1:NPAR)

 ZERO = BND * BND
 IF (ZERO < ETA) ZERO = ETA
 GCRIT = SQRT(ETA)

 A = 0.      !  Initialise arrays
 VERR = 0.
 IMPORT = 0.

 WGTSUM = REAL (SUM (XWTS))
 ITS = 0
 RSVT = MAX (BND, RSVT0)   ! Initialise eigenvalue damping at 10 percent
 WRITE(NW,'(/T3,A)') TRIM (TITLE)
 !WRITE(*,'(/T3,A)') TRIM (TITLE)
 IF (TDFD == 2) THEN
   WRITE(NW,1) JS ;  !WRITE(*,1) JS
 ELSE
   IF (CMP == 11) CVAR = 'in-line component'
   IF (CMP == 13) CVAR = 'vertical component'
   IF (CMP == 2)  CVAR = 'joint vertical and in-line components'
   IF (CMP == 3)  CVAR = 'joint vertical, in-line and transverse components'
   IF (CMP == 4)  CVAR = 'total field using all 3 components'

   WRITE(NW,2) TRIM (CVAR),JS; !WRITE(*,2) TRIM(CVAR),JS
 END IF
 WRITE(NW,3) MAXITS;  !WRITE(*,3) MAXITS

!----------------------------------------------------------------------
!  Start of main loop.  Call FORJAC to get error and Jacobian.
!  First, tranform physical model into transformed parameters.
!  Call ESVD to find the S.V.D. of the Jacobian.
!----------------------------------------------------------------------

 CALL CNVRT2_XPAR (NPAR,NLYR,RES,THK,XPAR)
 CALL CNVRT_BOUNDS (NPAR,LBND,UBND,CXPAR,XLBND,XUBND)

 ALLOCATE (RMS1(MAXITS))
 DRMS = 0.
 ITER_LOOP: DO ITS = 1, MAXITS

   JCBN = .TRUE.
   CALL FORJAC (NDATA,XDATA,XMODL,XWTS,NPAR,XPAR,CXPAR,SUMSQ,JCBN,A,VERR,TDFD,CMP,  &
                STEP,IDER,NSX,SWX,SWY,NTYRP,TRP,NPULS,PULSE,NTYPLS,NCHNL,TOPN,TCLS, &
                GSTRP,ASTRP,NFRQ,FREQ,KNRM,NORM,JS,TXCLN,TXA90,XRX,YRX,ZRX,NRXST,   &
                NSTAT,SZ,NLYR,RMU,REPS,CALF,CTAU,CFREQ)

   RMSERR = 100. * SQRT (SUMSQ / WGTSUM)
   stderr = Sqrt(sumsq/Abs(ndata-npar))
   RMS1(ITS) = RMSERR
   IF (ITS == 1) THEN
     FITS = 0
     MPAR(1:NPAR) = EXP (XPAR(1:NPAR))
     IF (MV1PRT > 1) THEN
       WRITE(NP,30) FITS,JS,RMSERR,RSVT
       WRITE(NP,31) FITS,MPAR(1:NPAR)
     END IF
     IF (OUTPRT > 1) WRITE(NW,4) RMSERR,RSVT
     IF (MV1PRT == 3) CALL WRITE_MDATA(FITS)
     IF (OUTPRT == 3) CALL WRITE_MISFIT (NW,FITS,NDATA,TDFD,NFRQ,FREQ,CMP,NCHNL,VERR,XMODL,XDATA)
     ! WRITE(*,4) RMSERR,RSVT
   END IF
   FNM = 0.01 * SQRT (SUMSQ)
   FNM = MAX (FNM, ETA)

!  Load the error vector into the NPAR+1 column of A.  On return from ESVD,
!  this column will contain the transformed error vector; i.e.,
!  VERR = U * VERR

   a_orig = a(1:ndata, 1:npar) ! save this for later ...
   A(1:NDATA,NPAR+1) = VERR(1:NDATA)
   CALL ESVD (A,NDATA,NPAR,IP,WITHU,WITHV,SV,UMAT,VMAT,WSP,ETA,TOL,NW)

   IF (ABS (SV(1)) < 1.E-30) THEN
     CALL WRITE_LOG_FILE (NLG,100,MXERR,2)
     Write (NP, 98)
     WRITE(*,99) JS
     WRITE(NLG,99) JS
     RETURN
   END IF

   VERR(1:NDATA) = A(1:NDATA,NPAR+1)


!  Solve for the correction vector, and test for convergence on
!  predicted decrease.  Loop over internal iterations.

   ICNT_LOOP: DO ICNT = 0, MAXITS

     CALL SOLVE2 (NPAR,RSVT,ZERO,VMAT,VERR,SV,NSV,DELPAR,WSP,PDRE)
     DELT = SQRT (PDRE)

! If the predicted residual decrease < 1 percent of RMS error,
! terminate iterations.  Inversion won't improve.

     IF (DELT < FNM) THEN
       WRITE(NW,5) ITS;  !WRITE(*,5) ITS
       WRITE(NW,7) RMSERR,RSVT; !WRITE(*,7) RMSERR,RSVT
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

!  Get the error for model with corrected parameters.
!  Test for improvement (decrease) in residual with Goldstein condition
!  on ratio of actual to computed decrease in error.  If it fails, reduce
!  step and try again.  Give up and return after MAXITS. "internal" iterations.

     JCBN = .True.

     CALL FORJAC (NDATA,XDATA,XMODL,XWTS,NPAR,GXPAR,CXPAR,SSQNEW,JCBN,A,VERR,TDFD,CMP, &
                  STEP,IDER,NSX,SWX,SWY,NTYRP,TRP,NPULS,PULSE,NTYPLS,NCHNL,TOPN,TCLS,  &
                  GSTRP,ASTRP,NFRQ,FREQ,KNRM,NORM,JS,TXCLN,TXA90,XRX,YRX,ZRX,NRXST,    &
                  NSTAT,SZ,NLYR,RMU,REPS,CALF,CTAU,CFREQ)

     GTEST = SUMSQ - SSQNEW

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
       EXIT ITER_LOOP
     END IF
   END DO ICNT_LOOP


!  Error reduced.  Accept step, write out summary, and test convergence.

   XPAR(1:NPAR) = GXPAR(1:NPAR)
   DELT = SQRT(GTEST)
   SUMSQ = SSQNEW
   RMSERR = 100. * SQRT (SUMSQ / WGTSUM)
   FNM = 0.01 * SQRT(SUMSQ)
   FNM = MAX( FNM, ETA)

!  If the predicted residual decrease < 1 percent of RMS error,
!  If the error decrease from the new iteration < 1 percent of the previous error,
!  claim convergence and terminate iterations because inversion won't improve.

!  Else, write out the current model and continue iterating up until ITS = MAXITS.

   IF (DELT < FNM) THEN
     WRITE(NW,5) ITS;  !WRITE(*,5) ITS
     WRITE(NW,7) RMSERR,RSVT; !WRITE(*,7) RMSERR,RSVT
     EXIT ITER_LOOP
   END IF
   IF (CNVRG == 2 .AND. RMSERR < PCTCNV) THEN
     WRITE(NW,10) PCTCNV,ITS; !WRITE(*,10) PCTCNV,ITS
     WRITE(NW,7) RMSERR,RSVT; !WRITE(*,7) RMSERR,RSVT
     EXIT ITER_LOOP
   END IF
   IF (ITS == MAXITS) THEN
     WRITE(NW,11) ITS; !WRITE(*,11) ITS
     WRITE(NW,7) RMSERR,RSVT; !WRITE(*,7) RMSERR,RSVT
     EXIT ITER_LOOP
   END IF

   MPAR(1:NPAR) = EXP (XPAR(1:NPAR))
   IF (MV1PRT > 1) THEN
     WRITE(NP,30) ITS,JS,RMSERR,RSVT
     WRITE(NP,31) ITS,MPAR(1:NPAR)
   END IF
   IF (MV1PRT > 2) CALL WRITE_MDATA(ITS)

!   WRITE(*,6) ITS,RMSERR,RSVT
   IF (OUTPRT > 1) THEN
     RES(1:NLYR) = MPAR(1:NLYR)
     THK(1:NLYR-1) = MPAR(NLYR+1:NPAR)
     CALL WRITE_MODEL (NW,ITS,JS,NLYR,THK,RES,CHRG,CTAU,CFREQ,RMU,REPS)
     WRITE(NW,'(2X)'); !WRITE(*,'(2X)')
     WRITE(NW,7) RMSERR,RSVT; !WRITE(*,7) RMSERR,RSVT

     IF (OUTPRT == 3 .AND. ITS < MAXITS)  &
       CALL WRITE_MISFIT (NW,ITS,NDATA,TDFD,NFRQ,FREQ,CMP,NCHNL,VERR,XMODL,XDATA)
   END IF
   RMS1(ITS) = RMSERR
   IF (ITS > 3) DRMS = RMS1(ITS-2) - RMS1(ITS)
   IF (ITS > 10 .AND. DRMS < 1.) THEN
     WRITE(NW,14) ITS; !WRITE(*,14) ITS
     EXIT ITER_LOOP
   END IF
 END DO ITER_LOOP   !  END OF MAIN LOOP.  Write final model and exit.
 ! DEALLOCATE (RMS1)

 CALL PARAMETER_SENSITIVITY (NPAR,VMAT,SV,BND,IMPORT,DMPFAC)
 CALL NOISE_2_SIGR (NPAR,NDATA,XMODL,XDATA,XWTS,NSR)

 WRITE(NW,12)
 WRITE(NP,32) js, ITS, RMSERR, 100. * stderr, RSVT
 MPAR(1:NPAR) = EXP (XPAR(1:NPAR))
 WRITE(NP,33) MPAR(1:NPAR)
 WRITE(NP,34) IMPORT(1:NPAR)

 ! write eigenparameter stuff ...
 Write (nw, 42) ndata, npar, Size(a), Size(a_orig)
 CALL WRITE_STATW (NW,NPAR,NLYR,VMAT,DMPFAC,100. * BND)
 Write (NP, 40) reshape(a_orig(1:ndata, 1:npar), (/(ndata*npar), 1/))
 Write (NP, 35) dmpfac
 ! Write (NP, 41) sv
 ! Write (NP, 36) reshape(vmat, (/(npar*npar), 1/))
 ! Write (NP, 38) reshape(umat, (/(ndata*ndata), 1/))
 
 ! model data
 IF (MV1PRT == 1 .OR. MV1PRT == 2) CALL WRITE_XDATA
 IF (MV1PRT > 0) CALL WRITE_MDATA(-1)

 MPRNT = -ITS
 RES(1:NLYR) = MPAR(1:NLYR)
 THK(1:NLYR-1) = MPAR(NLYR+1:NPAR)
 CALL WRITE_MODEL (NW,MPRNT,JS,NLYR,THK,RES,CHRG,CTAU,CFREQ,RMU,REPS)
 WRITE(NW,21)
 WRITE(NW,22,ADVANCE='NO') 'RES_01',('    RES_',J1,J1=2,NLYR)
 WRITE(NW,23) ('    THK_',J1,J1=1,NLYR-1)
 WRITE(NW,'(F8.2,1024F10.2)') IMPORT(1),(IMPORT(J1),J1=2,2*NLYR-1)

! calculate Cramer-Rao bounds before calculating parameter error bounds
 DO pI = 1, NPAR
   TEMPC = 0.0D0
   DO pJ = 1, NPAR
     DF = MAX (1.0E-15, SV(pJ))
     TEMPC = TEMPC + (DBLE (VMAT(pI,pJ)) / DBLE (DF))**2
   END DO
   CRAO(pI) = SQRT (SNGL (TEMPC))
 END DO
 Write (NP, 39) crao
 Call sbnd(NW,0,NPAR,NLYR,RES,THK,ELAS,stderr,CRAO,IMPORT)

 WRITE(NW,13) RMSERR,NSR

 FITS = -1
 SXD = REAL (SX,QL)
 SYD = REAL (SY,QL)
 SZ0 = SZ + GND_LVL
 IF (OUTPRT > 0) CALL WRITE_MISFIT (NW,FITS,NDATA,TDFD,NFRQ,FREQ,CMP,NCHNL,VERR,XMODL,XDATA)

 Write (NP, 37)

 Write (*, 45) js, nstat, rms1(1), rms1(its), its
 DEALLOCATE (RMS1)

 1 FORMAT(/T3,'Begin frequency-domain inversion for station ',I8)
 2 FORMAT(/T3,'Begin time-domain inversion on ',A,' for station ',I8)
 3 FORMAT( T3,'Maximum iterations = ',I3)
 4 FORMAT(/T3,'Initial symmetric root mean square error =',F8.2,' percent.' &
          /T3,'Initial RSVT (Relative Singular Value Threshold) =',F8.3)
 5 FORMAT(//T3,'Convergence on predicted decrease after',I3,' iterations.')
 6 FORMAT(/I4,' Iterations completed.  Symmetric RMS error =',F8.2,' percent.'/T44,'RSVT =',F8.3)
 7 FORMAT(T3,'Symmetric RMS error =',F8.2,' percent.',3X,'RSVT =',F7.3)
 9 FORMAT(/T3,'The solution is trapped.  ICNT = ',I2 &
          /T3,'Another starting guess may yield a better result.')
 10 FORMAT(/T3,'Convergence within RMS error threshold of',F7.2,' after',I3,' iterations.')
 11 FORMAT(/T3,'Inversion finished after the maximum',I3,' Iterations')
 12 FORMAT(/T3,50('='))
 13 FORMAT(/T23,'Symmetric RMS error =',F9.2,' percent.' &
           /T21,'Noise to signal ratio =',F9.3)
 14 FORMAT(/T3,'--------------------------------------------------------------------'  &
           /T3,'Inversion terminated after',I3,' iterations because, the reduction in' &
           /T3,'the RMS error from the last two iterations was less than 1 percent.'   &
           /T3,'____________________________________________________________________')
 21 FORMAT(/T3,'Parameter Importance'/T3,'--------------------')
 22 FORMAT(T3,A,1024(A,I2.2))
 23 FORMAT(1X,1024(A,I2.2))
 30 FORMAT(T1, '/'/T1,'/ ITERATION  ',I2.2,4X,'Station',I4/T1,'/ PERCENT_RMS_ERROR:',F9.2,3X,'RSVT:',F8.3)
 31 FORMAT(T1, '/ MODEL_',I2.2,1024G13.4)
 32 FORMAT(   t1, '/', &
           /, t1, '/ Station:          ', 2x, i4, &
           /, t1, '/ FINAL_ITERATION:  ', 2x, i4, &
           /, t1, '/ FINAL_PERCENT_RMS_ERROR: ', f9.5, &
           /, t1, '/ FINAL_Standard_error:    ', f9.5, &
           /, t1, '/ FINAL_RSVT:       ', f9.5, &
           /, t1, '/ ')
 33 Format (t1, '/ FINAL_MODEL:',5x, 1024(2x, e13.5))
 34 Format (t1, '/ ', &
            t1, '/ Importance: ',5x,1024(2x, f9.5))
 35 Format (t1, '/ Damping_Factors: ', 1024(2x, f9.5))
 36 Format (t1, '/ V-matrix: ', 7x, 2048(2x, f9.5))
 37 Format (t1, '/ ', 2014('^'))
 38 Format (t1, '/ U-matrix: ', 7x, 8192(2x, f9.5))
 39 Format (t1, '/ Cramer-Rao: ', 7x, 2048(2x, e11.4))
 40 Format (t1, '/ Jacobian: ', 7x, 8192(2x, e13.6))
 41 Format (t1, '/ Singular_values: ', 1024(2x, f9.5))
 42 Format (/, 2x, 'Eigenparameter information', &
            /, 2x, '--------------------------', &
            /, 2x, '      NData: ', i4, &
 			/, 2x, 'NParameters: ', i4, &
 			/, 2x, '    Size(a): ', i4, &
 			/, 2x, '  Size(a_o): ', i4)
45 Format (2x, 'Station ', i6, ' of ', i6, ': Initial error = ', f8.2, '%; Final error = ', f8.2, '%;', i3, ' iterations')
46 Format (t1, '/', &
        /, t1, 'Station:                ', 2x, i4, &
        /, t1, '/ INITIAL_MODEL:',5x, 1024(2x, e13.5))

 99 Format (/T3,'INVERSION HALTED for station',I4,' due to singular value decomposition failure.')
 98 Format (/, t1, '/ SVD_FAILURE: try restarting with alternate input parameters')

 CONTAINS

   SUBROUTINE WRITE_MDATA(KTS)
!  --------------------------

!***  Called by NLSQ2

 INTEGER KTS

 IF (KTS == -1) THEN      ! Write Final Model Data
   WRITE(QL0,1) LINE(JS)
   READ(QL0,'(A)') QL1
   WRITE(NP,7)
   WRITE(NP,4) TRIM (ADJUSTL (QL1)),JS
 ELSE IF (KTS == -2) THEN      ! Write Failure
   WRITE(QL0,2) LINE(JS)
   READ(QL0,'(A)') QL1
   WRITE(NP,7)
   WRITE(NP,4) TRIM (ADJUSTL (QL1)),JS
 ELSE                          ! Write Model Data after iteration KTS
   WRITE(QL0,3) LINE(JS),KTS
   READ(QL0,'(A)') QL1
   WRITE(NP,4) TRIM (ADJUSTL (QL1)),JS
 END IF

 IF (TDFD == 1) THEN
   WRITE(NP,5) JS,SYD(JS),SXD(JS),SZ0(JS),TXDEG(JS),RYD(JS),RXD(JS),RZ0(JS), &
                XMODL(1:NDATA),100.*VERR(1:NDATA)
 ELSE
   WRITE(NP,6) JS,SYD(JS),SXD(JS),SZ0(JS),RYD(JS),RXD(JS),RZ0(JS), &
                XMODL(1:NDATA),100.*VERR(1:NDATA)
 END IF

 1 FORMAT(T2,I10,'_ZFNL')
 2 FORMAT(T2,I10,'_Failed_Inversion')
 3 FORMAT(T2,I10,'_I',I2.2)
 4 FORMAT(T2,'Line ',A,4X,'Station',I4)
 5 FORMAT(T1,I5,2F12.1,F8.1,F6.1,2F12.1,F8.1,350G13.4:)
 6 FORMAT(T1,I5,2F12.1,F8.1,2F12.1,F8.1,300G13.4)
 7 FORMAT(T1,'/')

   END SUBROUTINE WRITE_MDATA

   SUBROUTINE WRITE_XDATA
!  ----------------------

!***  Called by NLSQ2

! Writes Survey Data

 WRITE(QL0,1) LINE(JS)
 READ(QL0,'(A)') QL1
 WRITE(NP,7)
 WRITE(NP,2) TRIM (ADJUSTL (QL1)),JS

 IF (TDFD == 1) THEN
   WRITE(NP,3) JS,SYD(JS),SXD(JS),SZ0(JS),TXDEG(JS),RYD(JS),RXD(JS),RZ0(JS), &
                XDATA(1:NDATA),VERR_FILL(1:NDATA)
 ELSE
   WRITE(NP,4) JS,SYD(JS),SXD(JS),SZ0(JS),RYD(JS),RXD(JS),RZ0(JS), &
                XDATA(1:NDATA),VERR_FILL(1:NDATA)
 END IF

 1 FORMAT(T2,I10,'_Survey_Data')
 2 FORMAT(T2,'Line ',A,4X,'Station',I4)
 3 FORMAT(T1,I5,2F12.1,F8.1,F6.1,2F12.1,F8.1,350G13.4:)
 4 FORMAT(T1,I5,2F12.1,F8.1,2F12.1,F8.1,300G13.4)
 7 FORMAT(T1,'/')

   END SUBROUTINE WRITE_XDATA

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
 INTEGER NPAR,NDATA,J1
 REAL, PARAMETER :: TOL=.1E-5
 REAL XMODL(NDATA),XDATA(NDATA),YM(NDATA),YD(NDATA),PK,PKM,BASE,ZM, &
      ZD,YBAR,NSR,CUMM,CUMD,CWMD
 Real :: XWTS(NDATA)

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

 SUBROUTINE PARAMETER_SENSITIVITY (NPAR,VMAT,SV,BND,IMPORT,DMPFAC)
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

 SUBROUTINE SOLVE2 (NPAR,RSVT,ZERO,VMAT,R,SV,NSV,DELPAR,WSP,PDRE)
!----------------------------------------------------------------

!  Calculates the new parameter changes by taking the product of
!  the transformed error vector times the damping factors (second order
!  Marquardt damping) times the inverse singular value matrix.
!  It also calculates the predicted error decrease.

!*** Called by: NLSQ2
!***     Calls: DPROD1

!       Input
!       -----
!     NPAR -  the number of unknown parameters
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
!
!       Output
!       ------
!
!     NSV -  the returned number of singular values used in the solution.
!  DELPAR -  the returned solution ( parameter changes)
!     WSP -  WSP (1:NPAR) contains the squared r vector
!             WSP (NPAR: 2*NPAR) contains the damping factors.
!    PDRE -  the returned predicted decrease in residual squared error

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

 SUBROUTINE WRITE_MISFIT (NW,ITSPR,NDATA,TDFD,NFRQ,FREQ,CMP,NCHNL,VERR,XMODL,XDATA)
!---------------------------------------------------------------------------------

!***  Called by: NLSQ2

!  Writes out the error misfit at any stage of the inversion
!
!     ITSPR > 0 => printout after ITSPR iterations on unit NW
!           < 0 => printout after final iterations on unit NW
!           = 0 => printout initial error structure unit NW
!
!     NDATA - total number of data points
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

 INTEGER NW,ITSPR,NDATA,TDFD,NFRQ,CMP,NCHNL,NCHN,N1,N2
 REAL FREQ(NFRQ)
 REAL, DIMENSION(NDATA) :: XMODL,XDATA,VERR
 CHARACTER(LEN=8) CHN(50)
 DATA CHN(1:50) &
   /' CHNL_1 ',' CHNL_2 ',' CHNL_3 ',' CHNL_4 ',' CHNL_5 ',' CHNL_6 ',' CHNL_7 ',' CHNL_8 ',' CHNL_9 ','CHNL_10 ', &
    'CHNL_11 ','CHNL_12 ','CHNL_13 ','CHNL_14 ','CHNL_15 ','CHNL_16 ','CHNL_17 ','CHNL_18 ','CHNL_19 ','CHNL_20 ', &
    'CHNL_21 ','CHNL_22 ','CHNL_23 ','CHNL_24 ','CHNL_25 ','CHNL_26 ','CHNL_27 ','CHNL_28 ','CHNL_29 ','CHNL_30 ', &
    'CHNL_31 ','CHNL_32 ','CHNL_33 ','CHNL_34 ','CHNL_35 ','CHNL_36 ','CHNL_37 ','CHNL_38 ','CHNL_39 ','CHNL_40 ', &
    'CHNL_41 ','CHNL_42 ','CHNL_43 ','CHNL_44 ','CHNL_45 ','CHNL_46 ','CHNL_47 ','CHNL_48 ','CHNL_49 ','CHNL_50 '/

!  Put data into matrix form

 IF (ITSPR == 0) THEN
   WRITE(NW,1)
 ELSE IF (ITSPR > 0) THEN
   WRITE(NW,2) ITSPR
 ELSE IF (ITSPR < 0) THEN
   WRITE(NW,3)
 END IF
 WRITE(NW,4)

 IF (TDFD == 1) THEN
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

   SUBROUTINE PRT_TD
!  -----------------

!***  Called by: WRITE_MISFIT

     WRITE(NW,'(/6X,30(A:,5X))') CHN(1:NCHN)
     WRITE(NW,'(/50G13.4)') 100. * VERR(N1:N2)
     WRITE(NW,'( 50G13.4)') XMODL(N1:N2)
     WRITE(NW,'( 50G13.4)') XDATA(N1:N2)
   END SUBROUTINE PRT_TD


   SUBROUTINE PRT_FD
!  -----------------

!***  Called by: WRITE_MISFIT

     WRITE(NW,'(/40F11.0)') FREQ(1:NFRQ)
     WRITE(NW,'(/40F11.2)') 100. * VERR(N1:N2)
     WRITE(NW,'( 40F11.2)') XMODL(N1:N2)
     WRITE(NW,'( 40F11.2)') XDATA(N1:N2)
   END SUBROUTINE PRT_FD

 END SUBROUTINE WRITE_MISFIT

!   ============================================================================================
!   Stats routines from Airbeo 4.0.1
!   
!   We use these for error analysis which seems to have fallen by the wayside in later versions

! SUBROUTINE STATS (NW,NPAR,NDATA,NLYR,A,UMAT,VMAT,SV,WSP,VERR,BND,IWHZ,CRAO,IMPORT,INV_FAIL)
! !-------------------------------------------------------------------------------------------

! !  Singular value analysis of the Jacobian for parameter sensitivity
! !  and 'importance'.   See Jupp and Vozoff 'Stable iterative methods for
! !  the inversion of geophysical data' Geophys.J.R. Astr.Soc.,42(1975)

! !***  Called by: AIRBEO
! !***  Calls:     ESVD, PRESS, WRITE_STATW, WRITE_MESSAGE

! !        A - NDATA * NPAR Jacobian matrix
! !      WSP - workspace
! !     NPAR - number of parameters
! !    NDATA - number of data values
! !     NLYR - number of layers
! !     VERR - error vector to be passed to PRESS
! !      BND - set minimum singular value threshold
! !    SIGMA - standard error
! !     CRAO - Cramer-Rao multipliers
! !   IMPORT - importance  (damping factors rotated into physical
! !             parameter space.
! !       NW - output unit number

! !     PRINT OPTIONS -
! !  IWHZ > 0  => Do layer thickness analysis
! !  IWHZ < 0  => Do depth to base analysis

! !      IPR = ABS (IWWHZ)

! !  IPR = 1  -  Print error bounds and APRE
! !  IPR = 2  -  plus damping, and V-matrix
! !  IPR = 3  -  plus singular values, and the Cramer-Rao multipliers

!  IMPLICIT NONE
!  INTEGER NW,NPAR,NDATA,NLYR,KP,IWHZ,IPR,I,J,KVAR(4),IFAIL
!  REAL, PARAMETER :: ETA=1.E-7, TOL=.5E-31
!  REAL A(NDATA,NPAR+1),UMAT(NDATA,NPAR),VMAT(NPAR,NPAR),WSP(3*NPAR),SV(NPAR), &
!       VERR(NDATA),IMPORT(NPAR),CRAO(NPAR),DMPFAC(NPAR),BND,SVNR,SVNR4, &
!       APRE,EFFPAR,TEMPI,DF,BDD,EP4,EPSQ,ZVAR(2*NPAR+4)
!  DOUBLE PRECISION TEMPC
!  LOGICAL WITHU,WITHV,INV_FAIL
!  CHARACTER (LEN=120) CVAR

! !  Undamped mode for error bounds.
! !  Calculate the singular value decomposition followed by the APRE, Average
! !  Predicted Residual Error.

!  KVAR = 0
!  ZVAR = 0
!  CVAR(1:120) = '*'
!  KP = 0
!  WITHU = .TRUE.
!  WITHV = .TRUE.

!  CALL ESVD (A,NDATA,NPAR,KP,WITHU,WITHV,SV,UMAT,VMAT,WSP,ETA,TOL,NW)
!  IF (ABS (SV(1)) < 1.E-30) THEN
!    INV_FAIL = .TRUE.
!    RETURN
!  END IF

!  CALL PRESS (NDATA,NPAR,UMAT,VERR,APRE,IFAIL)
!  KVAR(1) = IFAIL
!  ZVAR(1) = APRE
!  CALL WRITE_MESSAGE (NW,43,KVAR,NPAR,ZVAR,CVAR)

!  KVAR(1) = 2
!  IF (IWHZ > 0) KVAR(1) = 1
!  CALL WRITE_MESSAGE (NW,45,KVAR,NPAR,ZVAR,CVAR)
!  IPR = ABS (IWHZ)

! !  Normalise the singular values and set the damping factors for the
! !  second order Marquardt method.
! !  The threshold level is set at that used in NLSQ2

!  EPSQ = BND * BND
!  EPSQ = MAX( EPSQ, ETA)
!  EP4 = EPSQ * EPSQ
!  BDD = 100. * BND
!  EFFPAR = 0.0

!  DO J = 1, NPAR
!    SVNR = SV(J) / SV(1)
!    SVNR = MAX (SVNR, ETA)
!    SVNR4 = SVNR**4
!    DMPFAC(J) = 0.0
!    IF (SVNR > EPSQ) DMPFAC(J) = SVNR4 / (SVNR4 + EP4)
!    EFFPAR = EFFPAR +  DMPFAC(J)
!  END DO
!  ZVAR(1) = EFFPAR
!  CALL WRITE_MESSAGE (NW,47,KVAR,NPAR,ZVAR,CVAR)

! !  Write out the parameter space eigen vectors (V matrix) and damping
! !  factors.

!  IF (IPR > 1) CALL WRITE_STATW (NW,NPAR,NLYR,0,VMAT,DMPFAC,BDD)

! !  Calculate the importance and the undamped Cramer-Rao multipliers.
! !  These are simply the damping factors and the inverse eigenvalues
! !  rotated from eigenparameter space to physical parameter space.
! !  the C-R multipliers are used by SBND to calculate error
! !  bounds.

!  DO I = 1, NPAR
!    TEMPI = 0.0
!    TEMPC = 0.0D0
!    DO J = 1, NPAR
!      TEMPI =TEMPI + (VMAT(I,J) * DMPFAC(J) )**2
!      DF = MAX (1.0E-15, SV(J))
!      TEMPC = TEMPC + (DBLE (VMAT(I,J)) / DBLE (DF))**2
!    END DO
!    IMPORT(I) = SQRT (TEMPI)
!    CRAO(I) = SQRT (SNGL (TEMPC))
!  END DO

!  IF (IPR > 2) THEN
!    ZVAR(1:NPAR) = SV(1:NPAR)
!    ZVAR(NPAR+1: 2*NPAR) = CRAO(1:NPAR)
!    CALL WRITE_MESSAGE (NW,48,KVAR,NPAR,ZVAR,CVAR)
!  END IF

! END SUBROUTINE STATS

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
 50 Format (14x, 1024(8x, 'EP_', i3.3))
 51 Format (2x, 'Log(Rho_', i3.3, ')', 1024(6x, f8.5))
 52 Format (2x, '  Log(k_', i3.3, ')', 1024(6x, f8.5))
 54 Format (7x, 'Damping', 1024(6x, f8.5))

END SUBROUTINE WRITE_STATW

SUBROUTINE PRESS (NDATA,NPAR,UMAT,VERR,APRE,IFAIL)
!--------------------------------------------------

!  Calculates & returns APRE, the Average Predicted Residual Error

!***  Called by: STATS

!        NW - output unit number
!     NDATA = no. of rows in Jacobian = number of data values
!      NPAR = no. of parameters
!      UMAT - U MATRIX (data space eigenvectors)
!      VERR - the weighted error vector
!     IFAIL = 0 if APRE is defined, 1 otherwise

 IMPLICIT NONE
 REAL, PARAMETER :: TOL = 1.E-30
 INTEGER NDATA,NPAR,J,NC,I,IFAIL
 REAL UMAT(NDATA,NPAR),VERR(NDATA),PRSS,ERRSQ,USUMC,APRE

 INTENT (IN) NDATA,NPAR,UMAT,VERR
 INTENT (OUT) IFAIL,APRE


!  Calculate PRSS, predicted residual error, sum of squares.

 PRSS = 0.
 APRE = -9999.
 NC = NDATA
 CHNL_LOOP: DO I = 1, NDATA
   ERRSQ = VERR(I)**2
   IF (ERRSQ < .1E-10) THEN
     NC = NC - 1
     CYCLE CHNL_LOOP
   END IF

   USUMC = 1.
   DO J = 1, NPAR
     USUMC = USUMC - UMAT(I,J)**2
   END DO

   IF (USUMC < TOL) EXIT CHNL_LOOP

   PRSS = PRSS + (ERRSQ / USUMC)
 END DO CHNL_LOOP

 IFAIL = 1
 IF (NC > 0 .AND. USUMC > TOL) THEN
   APRE = 100. * SQRT (PRSS / REAL (NC))
   IFAIL = 0
 END IF

END SUBROUTINE PRESS

SUBROUTINE SBND (NW,IWHZ,NPAR,NLYR,RES,THK,ELAS,SIGMA,CRAO,IMPORT)
!------------------------------------------------------------------------

!  Error bounds for resistivities and either thicknesses or depths to bottom
!  of layers, based upon the Cramer-Rao multipliers.

!***  Called by: MAIN
!***  Calls:     WRITE_MESSAGE

!        NW - Output unit number
!      IWHZ - Switch for thickness or depth to base analysis.
!      NLYR = Number of layers
!       RES = Resistivities for final model
!       THK = Thicknesses for final model
!     DEPTH = Depths to base for final model
!      HOLD - Logical variable determining whether or not a parameter
!             is fixed
!     SIGMA - Standard error
!      CRAO - Cramer-Rao multipliers
!    IMPORT - Importance  (damping factors rotated into physical
!             parameter space.

 IMPLICIT NONE
 INTEGER NW,IWHZ,NLYR,I,KVAR(4),NPAR
 REAL RES(NLYR),THK(NLYR),DEPTH(NLYR),IMPORT(NPAR),CRAO(NPAR),T1,T, &
      W1,W2,SIGMA,ZVAR(2*NPAR+4)
 LOGICAL HOLD(2*NLYR-1)
 CHARACTER (LEN=120) CVAR
 Real :: elas(npar)
 Integer :: jp

 ! calulate logical held parameters ...
 Do jp = 1, npar
    If (elas(jp) .lt. .05) Then
        hold(jp) = .true.
    ELSE
        hold(jp) = .false.
    End IF
 End Do

!  Do resistivities.

 KVAR = 0
 ZVAR = 0
 CVAR(1:120) = '*'
 CALL WRITE_MESSAGE (NW,50,KVAR,NPAR,ZVAR,CVAR)
 DO I = 1, NLYR
   T1 = CRAO(I) * SIGMA
   KVAR(1) = I
   ZVAR(1) = RES(I)
   ZVAR(2) = IMPORT(I)
   IF( HOLD(I) ) THEN
     CALL WRITE_MESSAGE (NW,51,KVAR,NPAR,ZVAR,CVAR)
   ELSE IF (T1 > 14.) THEN
     CALL WRITE_MESSAGE (NW,52,KVAR,NPAR,ZVAR,CVAR)
   ELSE
     T = EXP (T1)
     W1 = RES(I) / T
     W2 = RES(I) * T
     ZVAR(3) = W1
     ZVAR(4) = W2
     CALL WRITE_MESSAGE (NW,53,KVAR,NPAR,ZVAR,CVAR)
   END IF
 END DO

 IF (IWHZ > 0) THEN  !  Do thicknesses

   CALL WRITE_MESSAGE (NW,54,KVAR,NPAR,ZVAR,CVAR)
   DO I = 1, NLYR-1
     KVAR(1) = I
     ZVAR(1) = THK(I)
     ZVAR(2) = IMPORT(NLYR+I)
     T1 = CRAO(NLYR+I) * SIGMA
     IF (HOLD(NLYR+I) ) THEN
       CALL WRITE_MESSAGE (NW,51,KVAR,NPAR,ZVAR,CVAR)
     ELSE IF (T1 > 14.) THEN
       CALL WRITE_MESSAGE (NW,52,KVAR,NPAR,ZVAR,CVAR)
     ELSE
       T = EXP (T1)
       W1 = THK(I) / T
       W2 = THK(I) * T
       ZVAR(3) = W1
       ZVAR(4) = W2
       CALL WRITE_MESSAGE (NW,53,KVAR,NPAR,ZVAR,CVAR)
     END IF
   END DO

 ELSE  !  Do depths to layer bases

   CALL WRITE_MESSAGE (NW,55,KVAR,NPAR,ZVAR,CVAR)
   DO I = 1, NLYR-1
     KVAR(1) = I
     ZVAR(1) = DEPTH(I)
     ZVAR(2) = IMPORT(NLYR+I)
     T1 = CRAO(NLYR+I) * SIGMA
     IF (HOLD(NLYR+I) ) THEN
       CALL WRITE_MESSAGE (NW,51,KVAR,NPAR,ZVAR,CVAR)
     ELSE IF (T1 > 14.) THEN
       CALL WRITE_MESSAGE (NW,52,KVAR,NPAR,ZVAR,CVAR)
     ELSE
       T = EXP (T1)
       W1 = DEPTH(I) / T
       W2 = DEPTH(I) * T
       ZVAR(3) = W1
       ZVAR(4) = W2
      CALL WRITE_MESSAGE (NW,53,KVAR,NPAR,ZVAR,CVAR)
     END IF
   END DO
 END IF
END SUBROUTINE SBND

SUBROUTINE WRITE_MESSAGE (NW,NMSG,KVAR,NPAR,ZVAR,CVAR)
!------------------------------------------------------

! Prints messages from various subroutines

!***  Called by AIRBEO, NLSQ2, SBND, STATS, ESVD

 IMPLICIT NONE
 INTEGER NW,NMSG,KVAR(4),NPAR
 REAL ZVAR(2*NPAR+4)
 CHARACTER (LEN=120) CVAR
 INTENT (IN) NW,NMSG,KVAR,ZVAR,CVAR

 IF (NMSG == 1) THEN
   WRITE(NW,1) KVAR(1)
   WRITE(*, 1) KVAR(1)
 END IF
 IF (NMSG == 2) THEN
   WRITE(NW,2) KVAR(1), KVAR(1)-1
   WRITE(*, 2) KVAR(1), KVAR(1)-1
 END IF

 IF (NMSG == 22) THEN
   WRITE(NW,22) CVAR,KVAR(1),KVAR(2)
   WRITE(*,22)  CVAR,KVAR(1),KVAR(2)
   IF (KVAR(3) == 1) THEN
     IF (KVAR(4) == 1) THEN
       WRITE(NW,'(/A,A,A/)') ' Time-domain inversion based on the in-line component.'
       WRITE(*,'(/A,A,A/)')  ' Time-domain inversion based on the in-line component.'
     ELSE IF (KVAR(4) == 3) THEN
       WRITE(NW,'(/A,A,A/)') ' Time-domain inversion based on the vertical component.'
       WRITE(*,'(/A,A,A/)')  ' Time-domain inversion based on the vertical component.'
     ELSE IF (KVAR(4) == 4) THEN
       WRITE(NW,'(/A,A,A/)') ' Time-domain inversion based on the total component.'
       WRITE(*,'(/A,A,A/)')  ' Time-domain inversion based on the total component.'
     ELSE IF (KVAR(4) == 2) THEN
       WRITE(NW,'(/A/)') ' Joint time-domain inversion based on the vertical & in-line components.'
       WRITE(*,'(/A/)') ' Joint time-domain inversion based on the vertical & in-line components.'
     END IF
   ELSE
     WRITE(NW,'(/A/)') ' Frequency-domain inversion based on in-phase & quadrature components.'
     WRITE(*,'(/A/)') ' Frequency-domain inversion based on in-phase & quadrature components.'
   END IF
 END IF

 IF (NMSG == 13) THEN
   IF (KVAR(1) == 1) THEN
     WRITE(NW,13) ZVAR(1), ZVAR(2)
     WRITE(*, 13) ZVAR(1),ZVAR(2)
   ELSE IF (KVAR(1) == 2) THEN
     WRITE(NW,14) ZVAR(1), ZVAR(2)
     WRITE(*, 14) ZVAR(1),ZVAR(2)
   END IF
 END IF

 IF (NMSG == 24) THEN
   WRITE(NW,24) KVAR(1)
   WRITE(*,24)  KVAR(1)
 END IF
 IF (NMSG == 10) WRITE (NW,10)

 IF (NMSG == 25) THEN
   WRITE(NW,25) KVAR(1)
   WRITE(*,25)  KVAR(1)
 END IF

 IF (NMSG == 26) THEN
   WRITE(NW,26) KVAR(1)
   WRITE(*,26)  KVAR(1)
 END IF

 IF (NMSG == 28) THEN
   WRITE(NW,28) ZVAR(1),ZVAR(2)
   WRITE(*,28)  ZVAR(1),ZVAR(2)
 END IF

 IF (NMSG == 29) THEN
   WRITE(NW,29) ZVAR(1)
   WRITE(*,29)  ZVAR(1)
 END IF

 IF (NMSG == 30) THEN
   WRITE(NW,30) KVAR(1)
   WRITE(*,30) KVAR(1)
 END IF

 IF (NMSG == 31) THEN
   WRITE (NW,31) KVAR(1),KVAR(2),ZVAR(1)
   WRITE (*,31)  KVAR(1),KVAR(2),ZVAR(1)
 END IF

 IF (NMSG == 32) THEN
   WRITE(NW,32) ZVAR(1)
   WRITE(*,32)  ZVAR(1)
 END IF

 IF (NMSG == 34) THEN
   WRITE (NW,34) ZVAR(2),ZVAR(3)
   WRITE (*,34)  ZVAR(2),ZVAR(3)
 END IF

 IF (NMSG == 40) WRITE (NW,40) ZVAR(1)
 IF (NMSG == 41) WRITE(NW,41) ZVAR(1)

 IF (NMSG == 43) THEN
   IF (KVAR(1) == 0) WRITE (NW,43) ZVAR(1)
   IF (KVAR(1) == 1) WRITE (NW,44)
 END IF

 IF (NMSG == 45) THEN
   IF (KVAR(1) == 1) WRITE (NW,45)
   IF (KVAR(1) == 2) WRITE (NW,46)
 END IF

 IF (NMSG == 47) WRITE (NW,47) ZVAR(1)
 IF (NMSG == 48) THEN
   WRITE (NW,'(//3X,A/)') 'Eigenvalues of Jacobian'
   WRITE (NW,48) ZVAR(1:NPAR)
   WRITE (NW,'(//3X,A/)') 'Cramer-Rao Multipliers'
   WRITE (NW,48) ZVAR(NPAR+1: 2*NPAR)
 END IF

! SBND output

 IF (NMSG == 50) WRITE (NW,50)
 IF (NMSG == 51) WRITE (NW,51) KVAR(1),ZVAR(1)
 IF (NMSG == 52) WRITE (NW,52) KVAR(1),ZVAR(1),ZVAR(2)
 IF (NMSG == 53) WRITE (NW,53) KVAR(1),ZVAR(1),ZVAR(3),ZVAR(4),ZVAR(2)
 IF (NMSG == 54) WRITE (NW,54)
 IF (NMSG == 55) WRITE (NW,55)
 IF (NMSG == 55) WRITE (NW,60) KVAR(1)

 1 FORMAT(//T3,'INVERSION FOR STATION',I5,' STARTING FROM THE SPECIFIED INITIAL MODEL'/T3,66('='))
 2 FORMAT( /T3,'INVERSION FOR STATION',I5,' STARTING FROM THE FINAL MODEL OF STATION',I5/T3,69('='))
 10 FORMAT(/T3,'An alternative starting guess may achieve better results'/)
 13 FORMAT (/T3,'Initial standard error = ',G12.4,' percent' &
            /T3,'Relative singular value threshold (RSVT) = ',F8.3)
 14 FORMAT (/T3,'Initial RMS error = ',G12.4,' percent' &
            /T3,'Relative singular value threshold (RSVT) = ',F8.3)
 22 FORMAT(//1X,A//' INVERSION OUTPUT FOR DATA SET',I3,'.  ITERATION LIMIT =',I3)
 24 FORMAT (//T3,57('=')/T3,'Convergence on predicted decrease - ',I2,' iterations'/)
 25 FORMAT(///1X,80('$')/1X,80('$')//&
         ' There is an overflow/underflow problem with parameter ',I2,// &
         ' It is probably caused by the combination of a bad initial'/ &
         ' model guess and the fact that one or more of the parameters'/ &
         ' has been held fixed.  Thus, you should either change your'/ &
         ' initial guess or let all of the parameters be free to vary.')
 26 FORMAT (/' The solution is trapped.  ICNT = ',I2/ &
             ' Another starting guess may yield a better result.'/)
 28 FORMAT (/T3,'Convergence within RMS error threshold of',F7.2, &
            ' has been achieved'/T3,'RMS error =',F7.2,' percent.'/)
 29 FORMAT (//T3,66('=')/T3,'The inversion was unable to achieve an RMS error' &
             /T3,'within the specified threshold of',F7.2,' percent.' &
             /T3,'An alternative starting guess may achieve better results.')
 30 FORMAT (/T3,'MAXIMUM ITERATIONS REACHED!  ITS =',I3)
 31 FORMAT (//T2,57('=')/T2,'ITERATION',I3,10X,'ICNT = ',I2,10X,'RSVT = ',F8.3)
 32 FORMAT(/T3,'RMS error = ',G12.4,' percent')
 34 FORMAT(/T3,'Standard error =',F8.2,' percent.' &
           /T3,'     RMS error =',F8.2,' percent.')
 40 FORMAT(//80('=')/3X,'STATISTICAL ANALYSIS AFTER INVERSION' &
           //T3,'Standard error =',F7.3,' percent.')
 41 FORMAT(/T3,'Noise to signal ratio =',F7.3,' percent.')
 43 FORMAT(/T3,'Average predicted residual error (APRE) = ',F7.2,' percent'/)
 44 FORMAT(///1X,20('#'),3X,'APRE is not defined for this case.',3X,20('#')///)
 45 FORMAT(1X,50('=')/'   Layer Thickness Parameter Sensitivity Analysis'/1X,50('-'))
 46 FORMAT(1X,48('=')/'   Depth to Base Parameter Sensitivity Analysis'/1X,48('-'))
 47 FORMAT (/' The number of effective parameters is',F10.3)
 48 FORMAT (2X,4(5G12.4/))
 50 FORMAT (/1X,37('*')/' * ERROR BOUNDS FOR LAYER PARAMETERS *'/1X,37('*')//2X, &
           ' LAYER RESISTIVITIES  -  68 PERCENT CONFIDENCE INTERVAL (UNDAMPED)'// &
           ' LAYER',6X,'RES(I)',7X,'BOUND(1)',6X,'BOUND(2)',5X,'IMPORTANCE'/ &
           ' -----',6X,'------',7X,'--------',6X,'--------',5X,'----------')
 51 FORMAT (I4,G17.4,5X,'THIS PARAMETER HAS BEEN HELD CONSTANT')
 52 FORMAT (I4,3X,G14.4,11X,'UNBOUNDED',13X,F5.2)
 53 FORMAT (I4,3X,3G14.4,5X,F5.2)
 54 FORMAT (//2X,' LAYER THICKNESSES  -  68 PERCENT CONFIDENCE INTERVAL (UNDAMPED)' &
            //' LAYER',5X,'THICK(I)',6X,'BOUND(1)',6X,'BOUND(2)',5X,'IMPORTANCE'/ &
              ' -----',5X,'--------',6X,'--------',6X,'--------',5X, '----------')
 55 FORMAT (//2X,' LAYER DEPTHS (TO BASE)  -  68 PERCENT CONFIDENCE INTERVAL (UNDAMPED)' &
            //' LAYER',5X,'DEPTH(I)',6X,'BOUND(1)',6X,'BOUND(2)',5X,'IMPORTANCE'/ &
             ' -----',5X,'--------',6X,'--------',6X,'--------',5X,'----------')
 60 FORMAT (/' MESSAGE FROM ESVD'/' Maximum iterations exceeded for singular value.',I5/)

END SUBROUTINE WRITE_MESSAGE

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
