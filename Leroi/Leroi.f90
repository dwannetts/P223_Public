!   PROGRAM Leroi
!-----------------------------------------------------------------------------------------
 Module LG_Metadata
!--------------
!
! Module designed to hold 'constant character options'
    Implicit NONE

    Character (Len = 40), Parameter :: PNAME = 'Leroi'
    Character (Len = 40), Parameter :: PVERS = '8.0.5'
    Character (Len = 40), Parameter :: PDATE = '12 June, 2024'
    Character (Len = 40), Parameter :: PAUT1 = 'CSIRO Electromagnetic Modelling Group'
    Character (Len = 40), Parameter :: PAUT2 = 'Art Raiche, Fred Sugeng and Glenn Wilson'
    Character (Len = 40), Parameter :: PAUT3 = 'David Annetts (david.annetts@csiro.au)'
    Character (Len = 40), Parameter :: PPROJ = 'AMIRA P223F'
    Character (Len = 80), Parameter :: PRELS = 'GPL-2.0 (Full text at https://opensource.org/licenses/gpl-2.0.php)'

End Module LG_Metadata
!-----------------------------------------------------------------------------------------
!
! Changes
!
!	8.0.5
!		1. restored multiplate modelling capability but effectivly modelling them will require
!		   placing multiple plates in a dummy layer above the basement
!
!	8.0.4
!		1.	started move to better module structure
!		2.	changed default plotting point to Rx
!		3.	corrected array access error in SET_SWYTD
!
!	8.0.3
!		1.	Include model in MF1 file
!-----------------------------------------------------------------------------------------
!    Leroi 8.0.1 represents a major improvrment in Leroi capabilities.
!    Plates can be in any layer but cannot cross layers.
!
!    Errors, which have been present since the major revision of the P223E version,
!    have been discovered and corrected as part of the major new development of
!    The Green;s function subroutines.
!
!    =========================================================================
!
!    For modelling tasks only, plates are no longer confined to basement if
!      DO3D is set to 3 in RECORD 2.  For this option:
!
!         Plates can be in any layer.
!
!         All plates must have zero plunge.  Leroi will enforce this condition
!         by setting all plunge ot zero if necessary..
!
!         Plates cannot cross layers.  Leroi will enforce this condition by
!         by decreasing the depthextent of all offending plates.
!
!         Inversion is not permitted unless all paltes are in the basement.
!
!    =========================================================================
!
!================
!   DESCRIPTION |
!================
!
!      The Model
!      ---------
!  Leroi models the response of and can invert for one or more 3D thin plate targets
!  in a horizontally layered host.
!
!  In theory, the term "thin plate" means zero thickness, ie, a conductance
!  of zero volume imposed upon the host region.  This implies that all the
!  induced currents lie in the planes defined by the plates.  In practice, the
!  thin plate algorithm will be accurate when the target is electrically thin;
!  ie, when its thickness is a fraction of the incident wavelength.
!
!  A 5 m thick, dike of high conductance will not be a thin target at high
!  frequencies or equivalently early delay times.  A 40 m thick moderately
!  conducting dyke can be regarded as electrically thin, especially at lower
!  frequencies.  Conductive layers will filter out high frequencies which
!  serves to extend the applicability of the thin plate assumption.
!
!  The plates are defined by a conductance, a plate length, a dip width, the
!  east, north and depth coordinates of the plate reference point plus azimuthal
!  dip and plunge rotations about that point.  For a zero plunge plate, the
!  terms strike length and plate length are synonymous.
!
!=================================================================================
!  NOTE ON STRIKE LENGTH, STRIKE & DIP AZIMUTH
!  -------------------------------------------
!
!   In previous versions of Leroi, before non-zero PLUNGE was allowed the terms
!   STRIKE LENGTH and STRIKE were used to describe the length and azimuth of the
!   plate.  These terms are still used in relevant places in the documentation of
!   many subroutines.
!
!   STRIKE LENGTH - PLATE LENGTH when PLUNGE = 0
!
!   STRIKE ANGLE = DIP AZIMUTH - 90 degrees
!                = azimuth of the top of the (unplunged) plate
!
!   Internally, the program converts DIP AZIMUTH to STRIKE ANGLE to avoid the
!   extensive and unnecessary redevelopment of all the subroutines that deal
!   with the position and orientation of the cells.
!
!=================================================================================
!
!
!                     2 _____________________ 3
!                      |                     |
!                      |       N             |
!                      |                     |
!                  ^   |       ^             |
!                  |   |       |             |
!                  |   |       |             |
!                      |        ---> E       |
!                 ETA  |                     |
!                      |                     |
!                      |_____________________|
!                     1           *           4
!                                PRP
!
!                           <--  XI
!
!  Each plate modelled by Leroi starts as a horizontal rectangular plate whose
!  strike edge runs east-west; ie. the dip azimuth points north.  The southern
!  edge is designated as the XI axis and the PRE or plate reference edge.
!  Initially XI points west.  The down-dip coordinate, ETA, initially points
!  north.  The midpoint of the XI axis, or PRE, is designated as the PRP or
!  plate reference point.  All rotations are performed about axes that pass
!  through the PRP.
!
!  The plate orientation is described in terms of dip azimuth, dip and plunge.
!  From the observer standpoint, the plate orientation is achieved by first
!  applying an azimuthal rotation (clockwise looking dowm) about the vertical
!  axis, followed by a clockwise dip rotation about the rotated XI axis (PRE)
!  followed by a clockwise plunge rotation about the plate normal.
!
!  In practice, SUBROUTINE SET_CELLS describes and uses a mathematical equivalent
!  based on rotations about fixed axes in reverse order.
!
!      Systems
!      -------
!  Leroi can be used in frequency or time-domain mode to model most ground or
!  borehole systems for controlled source EM.  It can be used for magnetic
!  dipole, closed loop or dipole transmitters.  It can be used
!  for magnetic dipole, closed loop or dipole receivers.
!  The vertices of closed and open loop transmitters and receivers must have
!  the same elevation (or depth) but they and magnetic dipole transmitters can
!  be on, above or below the surface in any layer including basement.
!
!  There are basically two source-receiver specification modes.
!  Independently specified transmitter and receiver groupings or
!  fixed offset surveys.

!  All transmitters in a survey must be of the same type: either, open or
!  closed loops or magnetic dipoles.
!
!  Regardless of transmitter type, all types of receivers can be used in the same
!  survey as long as each group has only one type of receiver.
!
!
!====================
!   FILE CONVENTIONS
!====================
!
!   INPUT FILES:
!   -----------
!
!   The input control file, named Leroi.cfl is read
!   from logical unit number NR = 3.
!
!   For inversion the data to be inverted must be contained in
!   Leroi.inv, read from logical unit NRI = 13
!
!
!   INPUT-OUTPUT FILES:
!   ------------------
!
!   For forward modelling only, frequency-domain output data, for reuse in
!   the time-domain restart option, is written to and read from
!   Leroi.frq, on logical unit ND = 7
!
!
!   VERBOSE-OUTPUT FILES:
!   --------------------
!
!   The Leroi.out is written to logical unit NW = 4
!
!   Messages about data or runtime errors are written in a file
!   called Leroi.log on logical unit NLG = 9
!
!
!   OUTPUT FILES FOR PLOTTING:
!   --------------------------
!
!   The AMIRA format file, Leroi.amx has been replaced by Leroi.mf1
!   (forward modelling only)
!
!   Terse inversion output for plotting is written to
!   logical unit NI = 14
!
!                                                                         Forward
!    UNIT #   UNIT ID      FILE ID      Function                 Invert    Model
!    ------   -------      -------      --------                 ------   -------
!       3       NR       Leroi.cfl      Input control file          x       x
!       4       NW       Leroi.out      Verbose output data         x       x
!       7       ND       Leroi.frq      F-D data for T-D reuse              x
!       9       NLG      Leroi.log      Data error messages         x       x
!      13       NRI      Leroi.inv      Data to be inverted         x
!      14       np      Leroi.mv1      Inversion plotting output   x
!      14       np      Leroi.mf1      Forward model output                x
!
!------------------------------------------------------------------------
!
!   Survey Geometry Conventions
!   --------------------------
!
!   There are basically two options: fixed loop
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
!   Plate Geometry
!   --------------
!   Initially, each plate starts as an east-west oriented horizontal plate.
!   The plate reference edge (PRE) is defined as the southern edge.  The PRP
!   (plate reference point) is the midpoint of this edge.  The user enters
!   the coordinates of this point in terms of Easting, Northing and RL.
!
!   The plate orientation will henceforth be specified by the direction of the
!   normal to the plate and the relative plate rotation about that normal.  This
!   avoids the ambiguity of the strike-dip-plunge description used in previous
!   versions.
!
!------------------------------------------------------------------------
!
!   Time-domain waveform sign convention
!   ------------------------------------
!
!  Leroi assumes that the specified transmitter current will start at 0 or
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
!** RECORD 1:  TITLE - up to 200 characters
!
!** RECORD 2:  TDFD, DO3D, ISYS, PRFL, ISTOP
!
!      TDFD = 1 => time-domain modelling - STANDARD OPTION
!           = 2 => frequency-domain modelling
!
!           = 0 => time-domain modelling - USER CONTROL OPTION
!
!         The default spectrum for the 1D host computation extends 0.001 Hz. to 100 MHz.
!
!         The default spectrum for the 3D part is between 0.1 KHz and 100 KHz. at 6 PPD
!         (points per decade) above 10 Hz and 3 PPD below.  It reaches a decade lower for
!         ground and downhole programs than for AEM because of the longer pulse length.
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
!==========================================================================
!      DO3D = -1  INVERSION
!                 In this case, NPLATE = 0 is not allowed.
!
!==========================================================================
!
!      DO3D =  1, 2, 3 or 0 for MODELLING
!
!      DO3D =  1  computes response of 3-D heterogeneities and prints
!                 voltages as measured by receivers.
!
!           =  2  (time-domain only)
!                 instead of computing the frequency-domain responses
!                 (95-99 percent of Leroi usual computation time) use
!                 previously computed frequency-domain responses contained
!                 in file Leroi.frq to calculate 3-D time-domain responses.
!
!           =  3  Plates can be in any layer; ie not restricted to basement.
!                 PLUNGE must be 0 for all plates.
!                 Inversion hasn't been implemented for this geometry.
!                 These restrictions are in place only because the 31-01-08
!                 project deadline does not permit further work.
!
!           =  0  compute layered earth model only
!
!      ISYS = 0 : Standard output processing
!           = 2 : Sampo processing => ABS (Bz/Bx) = vertical / radial
!           = 4 : Utem processing
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
!    For coincident loop or electric dipole receivers or Sampo, Leroi sets CMP = 1
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
!     This version of Leroi requires that electric dipole receivers be horizontal.
!     Thus only one depth is required. RXZ must be < or = 0.
!     The output is voltage computed as the integral of the electric field.
!     Enter the easting (RXE), northing (RXN) for each electrode followed by receiver depth.
!
!**   RECORD 9.J.I  RXE(I,J,1), RXN(I,J,1), RXE(I,J,2), RXN(I,J,2), RXZ(I,J)
!
!     For point electric field output:
!     -------------------------------
!
!     This is used for measurements down a hole referenced to a single point.
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
!           SDZ0(J) = elevation of loop J
!                    > 0 for loops in air
!                    < 0 for loops below air-earth or air-sea interface)
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
!           SDZ0(J) : Magnetic dipole Tx elevation for Line J
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
!          Lithology & structure for Leroi & LeroiAir
!          ==========================================
!
!** RECORD 10:  NLAYER, NPLATE, NLITH
!
!      NLAYER - number of layers including basement.
!
!      NPLATE - number of thin plates
!
!       NLITH - number of layer plus plate lithologies.  Any number of
!               lithologies may be defined.  Be careful not to use
!               layer lithologies for plates and vice versa
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
!     NOTE:  For layers, SIG_T must equal -1
!            For plates, RES must = -1;   RMU must = 1;   REPS must = 1
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
!   In many cases, CELLW = 25 is a good compromise between accuracy and speed.
!
!   LEROI will divide each target plate into a minimum of 2 cells
!   along in each direction.  If a plate is 525 m. long and 100 m. down dip
!   and CELLW has been set to 100 m., then the program
!   will model the target as 6 by 2 cells, each of dimension 87.5 by 50 m.
!
!   The input value for CELLW represents a trade-off between speed and accuracy.
!   Decreasing CELLW can substantially increase run times but will produce
!   better accuracy.  When the plate is very large, CELLW = 25 may result in
!   unacceptable runtimes and CELLW = 40 may give fast and reasonable results,
!   In other cases, especially for strongly interacting multiple plates, a finer
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
!           LITH(J) = integer which assigns the conductance and other
!                     physical properties from the list of RECORD 11
!                     Make sure that a plate rather than a layer
!                     lithology is specified.
!
!
!  |        CNTR_East(J), CNTR_North(J) are the east and north coordinates of
!  |        the PRP, PLATE REFERENCE POINT of plate J
!  |
!  |        PLTOP = RL of designated edge of plate J.
!  |                Therefore PLTOP now increases NEGATIVELY downwards
!  |
!  |          TARGET SIZES & ORIENTATIONS
!  |          ---------------------------
!  | *** RECORD 15:  PLNGTH(J), DPWDTH(J), DZM(J), DIP(J), PLG(J)
!  |
!  |             PLNGTH - length of plate J.  If PLUNGE = 0, PLNGTH = strike length.
!  |             DPWDTH - width of plate J along dip
!  |                    = depth extent if the plate is vertical.
!  |
!  |                DZM - dip azimuth, (angle plate normal makes with north)
!  |                      in degrees east of north for plate J.
!  |                      If DIP = 0, DZM = 0.                       (0 <= DZM <= 180).
!  |
!  |
!  |                DIP - dip angle (in degrees) of plate J.         ( 0 =< DIP < 180 )
!  |
!  |                PLG - plunge rotation about plate normal.        ( -90 <= PLG <= 90 )
!  |                      For a vertical plate with dip azimuth = 0,
!  |                      PLUNGE is positive clockwise looking north
!  |
!  |
!  |   Initially, each plate starts as a west-east oriented horizontal plate.
!  |   The PRE (plate reference edge) is defined as the southern edge.  The PRP,
!  |   is the midpoint of this edge.  The user enters the coordinates of this
!  |   point in terms of Easting, Northing and RL.  The PRP is held fixed for
!  |   all rotations.
!  |
!  |   All rotation axes pass through the PRP.  Physically, the plate is oriented by
!  |   an azimuth rotation about the vertical axis, a dip rotation about the PRE and
!  |   a plunge rotation about the re-oriented plate normal.  In practice
!  |   north (using the PRE as a hinge) followed by an azimuthal rotation DZM,
!  |   about the vertical axis.  A plate rotation is made about an axis normal to
!  |   the plate.
!  |
!  |   In practice a mathematical equivalent is used where rotations are made about
!  |   fixed spatial axes in reverse order as described in subroutines SET_CELLS
!  |   and RPLT2XYZ
!  |
!  |_____   END READ OVER NPLATE
!
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
!    Leroi computes sensitivities using a numerical derivative.  The
!    default uses a sequence of 5 and 3 percent step to compute this.
!    Leroi allows the user to specify a different derivative step through
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
!                  Leroi.out and Leroi.mv1 after each iteration.
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
!     Sensitivity Computations

!  In theory, the sensitivity matrix, for each iteration would be computed using
!  the analytic derivatives of the model with respect to each unfixed parameter.
!  In practice, a numerical derivative scheme is faster and more stable.  In
!  Leroi, this is done by computing models where each unfixed parameter in turn
!  is increased by a specified percentage and then subtracting the original model
!  data from each result.  Since the inversion is a non-linear process, the exact
!  derivative is not necessarily the most efficient choice.  The inversion path
!  (but not always the result) will be guided by the size of the difference step.
!  The default step regime in Leroi computes sensitivities initially using a
!  5 percent difference.  When subsequent iterations can no longer reduce the
!  error significantly, a 3 percent difference scheme is tested.  If the error is
!  reduced, it is used for the rest of the inversion.
!
!  If in RECORD 16, CNVRG is set to 10 or 20, the user can change this by specifying
!  the number and value of the difference steps to be tested during inversion.  The
!  first KPCT value will be used until the error can no longer be the reduced and
!  then the next values will be used similarly.  Thus the time required by the i
!  nversion will be increased according to how many KPCT values are specified.  At
!  least 1 value is required (NDSTP = 1)  The suggested maximum value of NDSTP is 3.
!  The values for KPCT should be in the interval from 2 to 15.  A KPCT value can be
!  used more than once but it would be silly to use the same values sequentially.
!
!     Example (Default scheme)
!     2         ! NDSTP
!     5 3       ! KPCT (1:NDSTP)
!
!     Example of three step scheme
!     3         ! NDSTP
!     3 5 8     ! KPCT (1:NDSTP)
!     _____________________________________________________________
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
!          = 3 => PLNGTH - length of plate JP along strike (if PLUNGE = 0)
!          = 4 => PLWDTH - dip width of plate JP
!          = 5 => YCNTR  - east coordinate of reference point of plate JP
!          = 6 => XCNTR  - north coordinate of reference point of plate JP
!          = 7 => PLAZM  - dip azimuth of plate JP      (degrees)
!          = 8 => PLDIP  - dip angle of plate JP        (degrees)
!          = 9 => PLUNJ  - plunge angle of plate JP     (degrees)
!
!     For PLT_INDX < 0   (LAYERS)
!     ----------------
!
!     PLT_INDX = -J     => layer J
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
!     END OF ENTRIES FOR Leroi.cfl     =
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
!=      BEGIN DESCRIPTON FOR Leroi.inv (Logical unit NRI = 13)                   =!
!=      ------------------------------------------------------                   =!
!=                                                                               =!
!=      Any number of comment lines can be inserted at the beginning of the      =!
!=      .inv file by using either / or \ as the first character of the line.     =!
!=                                                                               =!
!=      Leroi will start reading at the first line not containing / or \         =!
!=      as the first character.  Thereafter, any line beginning with  / or \     =!
!=      will cause an error and an end to execution.                              =!
!=                                                                               =!
!_________________________________________________________________________________!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!=================================================================================!
!
!             DATA DESCRIPTION & WEIGHTING  - read from Leroi.inv
!             ============================
!
!  Leroi.inv allows comment lines only at the start of the file.
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
!  order corresponding to the order in which they were specified in Leroi.cfl
!
!  Each data record must be grouped component by component.  For example, if, in
!  RECORD 2, KMP(J) = 312, then all the Z-component data channels must be followed
!  by all the X-component data channels followed by all the Y-component data channels.
!
!  Leroi requires that all lines specified in Leroi.cfl will be inverted and
!  that data will be presented in the same order as the line specification.
!  All components specified by CMP in Leroi.cfl must be present in the data.
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
!  | **  RECORD 2: LINE_CHK, NSTAT, KMP(J)
!  |
!  |         LINE_CHK - Line number for Line J, (error checking purposes)
!  |                    It must match Line(J) in Leroi.cfl to enable the inversion.
!  |
!  |         NSTAT     - Number of stations for Line J, (error checking purposes)
!  |                    It must match NRX(J) in Leroi.cfl to enable the inversion.
!  |
!  |         KMP(J) is NOT required for coincident loop surveys, Sampo (ISYS = 2),
!  |         or lines using electric dipole receivers (RX_TYPE = 2)
!  |         In these cases, they are all set to 1.
!  |
!  |         KMP(J) IS required for magnetic dipole receivers (RX_TYPE = 1)
!  |         (except for Sampo) and point E field receivers (RX_TYPE = 3)
!  |         KMP governs which data components are present in Leroi.inv
!  |         and the order in which they appear
!  |
!  |
!  |     (ONLY IF RX_TYPE (J) = 1 or 3  - magnetic dipole receivers or point E field receivers
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
!  |        KMP =   1 : Leroi.inv will contain only X data
!  |            =   2 : Leroi.inv will contain only Y data
!  |            =   3 : Leroi.inv will contain only Z data
!  |            =  13 : X data followed by Z data will be included in Leroi.inv.
!  |            =  31 : Z data followed by X data will be included in Leroi.inv.
!  |            =  23 : Y data followed by Z data will be included in Leroi.inv.
!  |            =  32 : Z data followed by Y data will be included in Leroi.inv.
!  |            = 123 : X data followed by Y data followed by Z data will be included in Leroi.inv.
!  |            = 312 : Z data followed by X data followed by Y data will be included in Leroi.inv.
!  |
!  |        KMP = 12, 21, 132, 213, 231, 321 are similarly defined.
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
!  |              DATA ENTRY   (read from Leroi.inv)
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

 MODULE LG_Filter_Coefficients
!--------------------------

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

 END MODULE LG_Filter_Coefficients

 MODULE LG_Frequency_select
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

 END MODULE LG_Frequency_select

 MODULE LG_Input_routines
!--------------------------

!** CONTAINS: READ_SYSTEM_DATA, READ_MODEL_DATA, SET_FRQ

 Use iso_Fortran_env
 Use LG_Metadata
 IMPLICIT NONE

! SYSTEM & LITHOLOGY DIMENSIONS
! -----------------------------

 INTEGER, PARAMETER :: NPROP=7, QL=SELECTED_REAL_KIND(p = 18)
 Integer, Parameter :: FVERS = 690
 REAL, PARAMETER :: PI=3.141592654, PI2=PI/2., R2D=180./PI,  D2R=PI/180.
 INTEGER NR,NW,ND,NLG,NRI,np,MSG,MXERR,DO3D,TDFD,IPPD,STEP,NSX,PRFL,ISTOP,KRXW, &
         MCHNL,NCHNL,NFRQ,NFT,CMPMT(4),KMPMT(4),MCMP,KHSQ,SOURCE_TYPE,SURVEY_TYPE,   &
         NLINES,MLINES,NTX,MXVRTX,MQVR,MXRS,ISYS,KTX,K1,MXTX,NTXL,J,JS,JT,JF,JV,JR,  &
         MRXTX,MRXL,NLITH,NPULS,NTYRP,NTYPLS,NPPD,MD1,MD2,NHID
 INTEGER, ALLOCATABLE, DIMENSION(:) :: LINE,IPLT,IDH,NVRTX,UNITS,KNORM,NRX,RX_TYPE,CMP,NRGTX,NRXTX, &
                                       HEADER_ID,KHID
 INTEGER, ALLOCATABLE, DIMENSION(:,:)  :: KRGTX,RXID,KNORM2,NCTD,LNTR
 REAL MIN_FREQ,MAX_FREQ,T0,T0SX,OFFTYM,REFTYM,PULSE,RXFMNT,TXFREQ,TXMNT,ZMAX,ZMIN,SV_AZM
 REAL, ALLOCATABLE, DIMENSION(:) :: TXON,WAVEFORM,CURNT,TRP,TMS,WTMS,TOPN,TCLS,FREQ,SWX,TXZ,SXZ,SVAZM,SDZ0, &
                                    TXLNGTH,TXWDTH,TXCLN,TXAZM,SXDIP,SXAZM,RHOTRP,RXMNT,RXOFF,RXOTX
 REAL, ALLOCATABLE, DIMENSION(:,:) :: SWY,LYTH,SXE,SXN,ZRXTX,RXDIP,RXAZM,BHDIP,BHAZM,RXZ,XRXOF,YRXOF,ZRXOF,DSTAT
 REAL, ALLOCATABLE, DIMENSION(:,:,:) :: XRXTX,YRXTX,RXE,RXN
 REAL, ALLOCATABLE :: BFTL(:,:,:,:)
 REAL(KIND=QL) ECNTRD,NCNTRD,QD,QFRQ1,QFRQ2,FQQ
 REAL(KIND=QL), ALLOCATABLE :: SDN0(:),SDE0(:)
 REAL(KIND=QL), ALLOCATABLE, DIMENSION(:,:)   :: SXED,SXND,CLCD
 REAL(KIND=QL), ALLOCATABLE, DIMENSION(:,:,:) :: YXZPLT,RXED,RXND
 LOGICAL PRTSEC, INVERT
 CHARACTER (LEN=200) INP,TITLE
 CHARACTER(LEN=60) LTXT
 Integer :: tvals(8)
 DATA NR,NW,ND,NLG,NRI,np /3,4,7,9,13,14/

 DATA LTXT     /'----------------------------------------'/

! Inversion specific parameters
! -----------------------------
 INTEGER KPRT,NDATA,KCHNL,MAXITS,CNVRG,INVPRT
 INTEGER, ALLOCATABLE :: RWTS(:,:,:,:)
 INTEGER, ALLOCATABLE, DIMENSION(:) :: CXPAR,NCMPL,KMP,PLYR
 REAL PCTCNV,PARPCT
 REAL, ALLOCATABLE, DIMENSION(:) :: XDATA,XMODL,UBND,LBND,ELAS
 REAL, ALLOCATABLE :: RDATA(:,:,:,:)
 LOGICAL, ALLOCATABLE :: SINGLE(:)

! Specific parameters for Leroi
! ------------------------------
 INTEGER NLYR,NPLT,JL,JP,MXAB,BEG,FIN,JA,JB,JAB,NAB,NAJ,NBJ,JP2,JAB2, &
         NAB2,MXRHO,NRMGT,MXCL2,NPAR,NDSTP
 INTEGER,ALLOCATABLE, DIMENSION(:) :: LITHP,KPCT
 REAL CELLW
 REAL, ALLOCATABLE, DIMENSION(:) :: RES,THK,RMU,REPS,CHRG,CTAU,CFREQ,SIG_T,CHRGP,CTAUP, &
                                    CFREQP,PLTOP,XCNTR,YCNTR,PLNGTH,PLWDTH,DZM,PLAZM,PLDIP,  &
                                    DIP,PLG,PLUNJ,MPAR
 REAL, ALLOCATABLE, DIMENSION(:,:) :: XCELL,YCELL,ZCELL
 REAL(KIND=QL),ALLOCATABLE, DIMENSION(:) :: RMUD,THKD
 LOGICAL INTRUDE

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
!
!    SOURCE_TYPE = 1 : general loop
!                = 2 : grounded wire
!                = 3 : magnetic dipole
!                = 4 : coincident loop
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
 INTRUDE = .FALSE.
 MRXTX = 1
 MXVRTX = 4
 SV_AZM = 0.
 CMPMT = 1
 KMPMT = 1
 MCMP = 3               !  Number of possible spatial components

!  Reproduce input data with no assignments and rewind file.
Call Date_and_time(Values = tvals)
Write (NW, 1) PNAME, PVERS, PDATE, PAUT1, PAUT2, PAUT3, PPROJ, PRELS, &
              compiler_version(), compiler_options(), &
              tvals(1:3), tvals(5:7)
Write ( *, 1) PNAME, PVERS, PDATE, PAUT1, PAUT2, PAUT3, PPROJ, PRELS, &
              compiler_version(), compiler_options(), &
              tvals(1:3), tvals(5:7)
! WRITE(NW,'(T11,A/T11,A/)') 'INPUT DATA', '----------'
! REFLECT_DATA: DO J = 1,200
!   READ(NR,'(A)',END = 100) INP
!   WRITE(NW,'(1X,A)') INP
! END DO REFLECT_DATA

 100 REWIND NR
     ! WRITE(NW,2)

 READ(NR,'(A)') TITLE
 WRITE(NW,'(/1X,A)') TRIM (ADJUSTL (TITLE))

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
 IF (DO3D > 2) THEN
   INTRUDE = .TRUE.
   DO3D = 1
   IF (DO3D > 3) CALL WRITE_LOG_FILE (NLG,3,MXERR,1)
 END IF

!   TDFD = 0, 1 or 2 for user controlled TD, standard TD or FD respectively.
!   DO3D = 2 => use old FD data from Leroi.frq.
!        = 1 or 3 => compute new  plate model.
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

 IF (PRFL /= 0 .AND. PRFL /= 1 .AND. PRFL /= 10 .AND. PRFL /= 11) THEN
   CALL WRITE_LOG_FILE (NLG,2,MXERR,1)
   PRFL = 1
 END IF

 STEP = 2
 IF (TDFD < 2) THEN
   IF (DO3D == 1) THEN
     IF (.NOT. INVERT) OPEN(ND,FILE = 'Leroi.frq',STATUS = 'REPLACE')
   ELSE IF (DO3D == 2) THEN
     OPEN(ND,FILE = 'Leroi.frq',STATUS = 'OLD')
   ELSE IF (DO3D > 3) THEN
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
     WRITE(NW,'(7X,I4,5f12.4)') J,TOPN(J),TCLS(J),WTMS(J),TMS(J)+REFTYM,TMS(J)
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

   IF (ISYS /= 6) THEN
     WRITE(NW,9)
     DO J = 1, NFRQ
       READ(NR,*) FREQ(J),CURNT(J)
       WRITE(NW,'(3X,I6,f13.5,5X,f13.5)') J,FREQ(J),CURNT(J)
     END DO
   ELSE
     WRITE(NW,39)
     DO J = 1, NFRQ
       READ(NR,*) FREQ(J)
       WRITE(NW,'(3X,I3,f12.4)') J,FREQ(J)
     END DO
   END IF
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

 CASE (6)
   CALL WRITE_LOG_FILE (NLG,20,MXERR,2)   !  Write fatal error message
   WRITE(*,50)
   STOP
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
       If (iplt(jl) .eq. 0) iplt(jl) = 1				! default plotting point as Rx
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
     READ(NR,*) KTXP,TXLNGTH(JS),TXWDTH(JS),SV_AZM,SDE0(JS),SDN0(JS),SDZ0(JS)
     IF (KTXP > MRXL) THEN
       CALL WRITE_LOG_FILE (NLG,14,MXERR,1)
       KTXP = MRXL
       WRITE (NLG,34) JS,MRXL
     END IF

     READ(NR,*) DSTAT(2:KTXP,JS)
     WRITE(NW,27) JS,KTXP,TXLNGTH(JS),TXWDTH(JS),SDE0(JS),SDN0(JS),SDZ0(JS),SV_AZM,DSTAT(2:KTXP,JS)
     K1 = NTX + 1
     NTX = NTX + KTXP

     DO JR = 1,MRXTX
       JL = JR + (JS-1) * MRXTX       ! Line index
       SVAZM(JL) = SV_AZM * D2R
       NRX(JL) = KTXP
       READ(NR,*) LINE(JL),CMP(JL),XRXOF(JR,JS),YRXOF(JR,JS),ZRXOF(JR,JS),RXMNT(JL),UNITS(JL),KNORM(JL),IPLT(JL)
       If (iplt(jl) .eq. 0) iplt(jl) = 1				! default plotting point as Rx
       CALL VALIDATE_CMP (NLG,MXERR,JL,NLINES,CMP,NCMPL, ISYS)
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
       If (iplt(jl) .eq. 0) iplt(jl) = 1				! default plotting point as Rx
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
     If (iplt(jl) .eq. 0) iplt(jl) = 1				! default plotting point as Rx
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
 DO JL = 1,NLINES
   IF (UNITS(JL) > 5) RXMNT(JL) = 1.0
 END DO

 IF (MAXVAL (KNORM) > 0 .AND. STEP == 0) THEN
   KNORM = 0
   CALL WRITE_LOG_FILE (NLG,13,MXERR,1)
 END IF

 MLINES = NLINES
 ALLOCATE (BFTL(MCHNL,MRXL,MCMP,MLINES),RDATA(MCHNL,MRXL,MCMP,MLINES),RWTS(MCHNL,MRXL,MCMP,MLINES),      &
           SXN(MXVRTX,NTX),SXE(MXVRTX,NTX),XRXTX(MRXTX,NTX,MQVR),YRXTX(MRXTX,NTX,MQVR),ZRXTX(MRXTX,NTX), &
           YXZPLT(3,MRXL,NLINES),NCTD(MRXTX,NTX),RXID(MRXTX,NTX),KNORM2(MRXTX,NTX))

 BFTL = 0.;  RDATA = 0.
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
    /, 2x, '|', &
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
 11 FORMAT(/T3,'NLINES =',I3,';   MRXL =',I4,';   NTX =',I3,';   SOURCE_TYPE =',I2,';   MXVRTX =',I4,';   NTRN =',I3)

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
           /T3,'          to compute the HALFSPACE time-domain response, Leroi will now use' &
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
           /T3,'Initial position (E,N,Z) :',3F11.1 /T13,'Line_azimuth :',F6.1/T8,'Station_intervals :',2048F8.1)
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
 50 FORMAT(/T3,'This version of Leroi does not include magnetotellurics.' &
           /T3,'It is for controlled sources only.')

    END SUBROUTINE READ_SYSTEM_AND_SURVEY_DATA

   SUBROUTINE READ_MODEL_DATA
!  --------------------------

!***  Called by MAIN
!***  Calls SET_CELLS

 IMPLICIT NONE
 REAL, PARAMETER ::  CELLW_MIN=0.01, CELLW_MAX=200.
 INTEGER KP,KP1,KP2
 INTEGER, ALLOCATABLE :: LITHL(:)
 REAL A1
 REAL, ALLOCATABLE, DIMENSION(:) ::  PLTOPD
 REAL(KIND=QL), ALLOCATABLE, DIMENSION(:) :: XCNTRD,YCNTRD

 ECNTRD = 0.D0; NCNTRD = 0.D0

!  Layered Model Specification
!  ---------------------------

 READ(NR,*) NLYR, NPLT, NLITH
 ! NPLT = 1
 WRITE(NW,1) NLYR, NPLT, NLITH
 IF (NLYR < 1) CALL WRITE_LOG_FILE (NLG,25,MXERR,2)
 IF (NLYR == 1) INTRUDE = .FALSE.
 IF (NPLT == 0) THEN
   DO3D = 0
   PRTSEC = .FALSE.
 END IF
 IF (DO3D == 0) THEN
   NPLT = 0
   IF (SURVEY_TYPE == 1) THEN
     ECNTRD = MAXVAL (RXED) - MINVAL (RXED)
     NCNTRD = MAXVAL (RXND) - MINVAL (RXND)
     IF (ABS (ECNTRD) < 9.D4) ECNTRD = 0.D0
     IF (ABS (NCNTRD) < 9.D4) NCNTRD = 0.D0
   END IF
 END IF
 IF (DO3D == 0 .AND. NPLT > 0) WRITE(NW,10) NPLT
 NPAR = 9*NPLT + 2*NLYR-1

 ALLOCATE (LYTH(NLITH,NPROP),LITHL(NLYR),RES(NLYR),RMU(NLYR),RMUD(0:NLYR),REPS(NLYR),CHRG(NLYR), &
           CTAU(NLYR),CFREQ(NLYR),THK(NLYR),THKD(NLYR),MPAR(NPAR))

 THK=1.E5; RES=0; CHRG=0; CTAU=0; CFREQ=1; RMU=1; RMUD=1._QL
 REPS=1; LITHL=0; MPAR=0.

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


!*************************
!*************************

 IF (DO3D == 0) RETURN

!*************************
!*************************


!  Start reading plate variables if DO3D > 0

 ALLOCATE (LITHP(NPLT),SIG_T(NPLT),CHRGP(NPLT),CTAUP(NPLT),CFREQP(NPLT),PLTOP(NPLT),PLTOPD(NPLT), &
           XCNTR(NPLT),YCNTR(NPLT),XCNTRD(NPLT),YCNTRD(NPLT),PLNGTH(NPLT),PLWDTH(NPLT),DZM(NPLT), &
           PLAZM(NPLT),DIP(NPLT),PLDIP(NPLT),PLG(NPLT),PLUNJ(NPLT),PLYR(NPLT))

 LITHP=0; SIG_T=-1.; CHRGP=0; CTAUP=0; CFREQP=0; PLTOP=0; XCNTR=0; YCNTR=0;  PLNGTH=0;
 PLWDTH=0; DZM=0; DIP=0; PLG=0; PLYR = NLYR

 READ(NR,*) CELLW

 WRITE(NW,4) CELLW

 CELLW = MIN (CELLW_MAX, CELLW)
 CELLW = MAX (CELLW_MIN, CELLW)

 DO JP = 1, NPLT
   WRITE(NW,5) JP

! To maintain historical continuity, the input data description calls for the
! plate locator CNTR_East, CNTR_North read in as the east, north coordinates.
! Substitute YCNTR and XCNTR as the east, north coordinates in order to use
! a right-handed system with Z downwards.

   READ(NR,*) LITHP(JP),YCNTRD(JP),XCNTRD(JP),PLTOPD(JP)
   PLTOPD(JP) = -ABS (PLTOPD(JP))   ! No plates above ground
   WRITE(NW,'(2I4,2F13.2,F11.2,3X,A)') JP,LITHP(JP),YCNTRD(JP),XCNTRD(JP),PLTOPD(JP), &
                                      'JP, LITHP,  YCNTRD, XCNTRD,  PLTOP'
   J = LITHP(JP)
   SIG_T(JP) = LYTH(J,2)
   IF (SIG_T(JP) < 0) CALL WRITE_LOG_FILE (NLG,27,MXERR,2)

   READ(NR,*) PLNGTH(JP), PLWDTH(JP), DZM(JP), DIP(JP), PLG(JP)
   WRITE(NW,'(2F9.1,3F8.1,3X,A)') PLNGTH(JP),PLWDTH(JP),DZM(JP),DIP(JP),PLG(JP), &
                                 'PLNGTH, PLWDTH, DZM, DIP, PLG'
   IF (DZM(JP) < 0. .OR. DZM(JP) > 180.) THEN
     CALL WRITE_LOG_FILE (NLG,100,MXERR,2)
     WRITE(NLG,6) JP ;  WRITE(*,6) JP
   END IF
   IF (DIP(JP) < 0. .OR. DIP(JP) > 180.) THEN
     CALL WRITE_LOG_FILE (NLG,100,MXERR,2)
     WRITE(NLG,7) JP ;  WRITE(*,7) JP
   END IF
   IF (PLG(JP) < -90. .OR. PLG(JP) > 90.) THEN
     CALL WRITE_LOG_FILE (NLG,100,MXERR,2)
     WRITE(NLG,8) JP ;  WRITE(*,8) JP
   END IF

 END DO

 A1 = MAXVAL (ABS ( PLG))
 IF (INTRUDE .AND. A1 > 0.001) THEN
   CALL WRITE_LOG_FILE (NLG,33,MXERR,1)
   PLG = 0.
   WRITE(NW,12)
 END IF

 KP = 9*NPLT
 KP1 = KP + NLYR
 KP2 = KP1 + NLYR - 1
 MPAR(KP+1:KP1)  = RES(1:NLYR)
 MPAR(KP1+1:KP2) = THK(1:NLYR-1)

 IF (NPLT > 0) THEN
   ECNTRD = 0.5D0 * (MAXVAL (YCNTRD) + MINVAL (YCNTRD) )  ! Set up body centred origin
   NCNTRD = 0.5D0 * (MAXVAL (XCNTRD) + MINVAL (XCNTRD) )
   IF (ABS (ECNTRD) < 9.D4) ECNTRD = 0.D0
   IF (ABS (NCNTRD) < 9.D4) NCNTRD = 0.D0
 END IF
 XCNTR = REAL (XCNTRD - NCNTRD)   !  Set up old Leroi world
 YCNTR = REAL (YCNTRD - ECNTRD)
 PLTOP = -PLTOPD

 DO JP = 1, NPLT
   KP = 9* (JP-1)
   MPAR(KP+1) = SIG_T(JP)
   MPAR(KP+2) = PLNGTH(JP)
   MPAR(KP+3) = PLWDTH(JP)
   MPAR(KP+4) = PLTOPD(JP)
   MPAR(KP+5) = REAL (YCNTRD(JP))
   MPAR(KP+6) = REAL (XCNTRD(JP))
   MPAR(KP+7) = DZM(JP)
   MPAR(KP+8) = DIP(JP)
   MPAR(KP+9) = PLG(JP)

   J = LITHP(JP)
   IF (J < 1 .OR. J > NLITH) THEN
     WRITE(NW,'(T3,A,I2,A,I4)') 'LITHP(',JP,') =',J
     CALL WRITE_LOG_FILE (NLG,26,MXERR,2)
   END IF

   CHRGP(JP) =  LYTH(J,5)
   CTAUP(JP) =  LYTH(J,6)
   CFREQP(JP) = LYTH(J,7)
   IF ((DIP(JP) < 0.) .OR. (DIP(JP) > 179.99)) CALL WRITE_LOG_FILE (NLG,29,MXERR,2)
 END DO

 PLAZM = (DZM -90.) * D2R
 PLDIP = DIP * D2R
 PLUNJ = PLG * D2R

  1 FORMAT(//T3,'NLAYER =',I3,';   NPLATE =',I3,';   NLITH =',I3)
  2 FORMAT(//T27,'LITHOLOGY PROPERTIES'/T27,'--------------------' &
           //T35,'Relative   Relative     Cole-Cole Parameters'    &
            /T9,'Resistivity  Conductance     MU     Dielectric   CHRG    CTAU       CFREQ'/)
  3 FORMAT(//T3,'LAYERED EARTH INPUT DATA'/T3,'------------------------'/)
  4 FORMAT(//T3,'CELLW =',F7.2)
  5 FORMAT(/T3,'Input Data for Plate',I3/T3,'-----------------------')
  6 FORMAT(/T3,'Error in Plate',I3,'.  Dip azimuth must be between 0 and 180 degrees.')
  7 FORMAT(/T3,'Error in Plate',I3,'.  Dip must be between 0 and 180 degrees.')
  8 FORMAT(/T3,'Error in Plate',I3,'.  Plunge must be between -90 and 90 degrees.')
 10 FORMAT(/T3,'Although NPLATE =',I3,' DO3D = 0.  Thus only the layered half-space' &
           /T3,'response will be computed')
 12 FORMAT(/T3,'DO3D = 3 requires the plunge of all plates to be set to 0')

   END SUBROUTINE READ_MODEL_DATA

   SUBROUTINE READ_PARAMETER_CONTROL
!  ---------------------------------

!***  Called by: MAIN
!***      Calls: WRITE_LOG_FILE

 INTEGER NFIX,CTYPE,PLT_INDX,LYR_INDX,KPAR,J1
 REAL E1,E2,E3,A1,A2
 CHARACTER(LEN=12) PLATE_PRM(9),LYR_PRM(2)
 DATA LYR_PRM /  'Resistivity','Thickness'/
 DATA PLATE_PRM /'Conductance','Depth to top','Plate length','Dip width','CNTR_East', &
                 'CNTR_North','Dip azimuth','Dip angle','Plunge'/

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
 WRITE(NW,2) PCTCNV,MAXITS ; WRITE(*,2) PCTCNV,MAXITS
 WRITE(NW,3) NDSTP,KPCT(1:NDSTP)

 ALLOCATE (ELAS(NPAR),LBND(NPAR),UBND(NPAR),CXPAR(NPAR))
 CXPAR = 0
 IF (NFIX > 0) THEN
   WRITE(NW,4)
   DO JP = 1, NFIX
     E1 = 0.
     E2 = -1.E10
     E3 =  1.E10
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
       WRITE(NLG,20) JP
       CYCLE
     ELSE IF (PLT_INDX > 0) THEN
       IF (KPAR < 1 .OR. KPAR > 9) THEN
         CALL WRITE_LOG_FILE (NLG,203,MXERR,1)
         WRITE(NLG,20) JP
         CYCLE
       END IF
     ELSE IF (PLT_INDX < -NLYR) THEN
       CALL WRITE_LOG_FILE (NLG,210,MXERR,1)
       WRITE(NLG,20) JP
       CYCLE
     ELSE IF (PLT_INDX < 0) THEN
       IF (KPAR < 1 .OR. KPAR > 2) THEN
         CALL WRITE_LOG_FILE (NLG,211,MXERR,1)
         WRITE(NLG,20) JP
         CYCLE
       END IF
     ELSE IF (PLT_INDX == 0) THEN
       CALL WRITE_LOG_FILE (NLG,212,MXERR,1)
       WRITE(NLG,20) JP
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
     A2 = 0.005 * (ABS (E2) + ABS (E3))    ! If bound interval is infinitesimal,
     IF (A1  < A2) E1 = 0.                 ! set elastcicty to 0.

     IF (PLT_INDX < 0) THEN
       LYR_INDX = ABS (PLT_INDX)
       J1 = 9*NPLT + LYR_INDX             !  Resistivity of layer LYR_INDX
       IF (KPAR == 2) J1 = J1 + NLYR     !  Thickness of layer LYR_INDX
       CXPAR (J1) = CTYPE
       IF (E1 < 0.05) CXPAR(J1) = 1
       IF (CTYPE == 3) THEN
         WRITE(NW,5) J1,LYR_INDX,KPAR,LYR_PRM(KPAR),E1,E2,E3,CXPAR(J1)
       ELSE
         WRITE(NW,6) J1,LYR_INDX,KPAR,LYR_PRM(KPAR),E1,CXPAR(J1)
       END IF
     ELSE
       J1 = 9* (PLT_INDX - 1) + KPAR
       CXPAR (J1) = CTYPE
       IF (E1 < 0.05) CXPAR(J1) = 1
       IF (CTYPE == 3) THEN
         WRITE(NW,7) J1,PLT_INDX,KPAR,PLATE_PRM(KPAR),E1,E2,E3,CXPAR(J1)
       ELSE
         WRITE(NW,8) J1,PLT_INDX,KPAR,PLATE_PRM(KPAR),E1,CXPAR(J1)
       END IF
     END IF
     ELAS(J1) = E1
     LBND(J1) = E2
     UBND(J1) = E3
   END DO
   WRITE(NW,9)
 ELSE
   WRITE(NW,10)
 END IF

!  Stablise angles

 DO JP = 1,NPLT
   J1 = 9*JP-2
   IF (CXPAR(J1) == 0) THEN  ! Restrict DIP_AZIMUTH between 0 and PI
     CXPAR(J1) = 3
     ELAS(J1) = 0.9
     LBND(J1) = 0.
     UBND(J1) = 180.
   ELSE
     LBND(J1) = MAX (LBND(J1), 0.)
     UBND(J1) = MIN (UBND(J1), 180.)
   END IF

   J1 = 9*JP-1
   IF (CXPAR(J1) == 0) THEN  ! Restrict DIP between 0 and PI
     CXPAR(J1) = 3
     ELAS(J1) = 0.9
     LBND(J1) = 0.
     UBND(J1) = 180.
   ELSE
     LBND(J1) = MAX (LBND(J1), 0.)
     UBND(J1) = MIN (UBND(J1), 180.)
   END IF
   J1 = 9*JP
   IF (CXPAR(J1) == 0) THEN  ! Restrict PLUNGE between -PI/2 and PI/2
     CXPAR(J1) = 3
     ELAS(J1) = 0.9
     LBND(J1) = -90.
     UBND(J1) =  90.
   ELSE
     LBND(J1) = MAX (LBND(J1), -90.)
     UBND(J1) = MIN (UBND(J1),  90.)
   END IF
 END DO

  1 FORMAT(//T3,'---------------------------------------------------' &
            /T3,'Inversion Controls for Plate Parameters using Leroi' &
            /T3,'---------------------------------------------------' &
           //T3,'NFIX =',I3,3X,'MAXITS =',I3,3X,'CNVRG =',I2,3X,'INVPRT =',I2)
  2 FORMAT(/T3,'The inversion will finish if the RMS error is less than',F5.1,' percent' &
           /T3,'or for some other as yet undisclosed reason.' &
           /T3,'A maximum of',I3,' iterations will be allowed.')
  3 FORMAT(T3,'The inversion sequence will use',I2,' numerical derivative steps' &
          /T3,'Values in percent:',10I3)
  4 FORMAT(//T12,'Constrained Parameters'/T12,'----------------------' &
           //T5,'Global  Plate  Layer  Parameter'                                                        &
            /T5,'Index   Index  Index   Index      Parameter      Elasticity  Lower Bound   Upper Bound   CTYPE' &
            /T5,'------  -----  -----  ---------   ---------      ----------  -----------   -----------   -----')
  5 FORMAT(T7,I2,T21,I2,T30,I1,T39,A,T58,F6.2,T66,G12.4,T80,G12.4,T94,I3)
  6 FORMAT(T7,I2,T21,I2,T30,I1,T39,A,T58,F6.2,T94,I3)
  7 FORMAT(T7,I2,T14,I2,T30,I1,T39,A,T58,F6.2,T66,G12.4,T80,G12.4,T94,I3)
  8 FORMAT(T7,I2,T14,I2,T30,I1,T39,A,T58,F6.2,T94,I3)
  9 FORMAT(/T3,90('-'))
 10 FORMAT(/T3,'All plate and host parameters will be allowed to vary during inversion')
 20 FORMAT(T3,'Constraint',I2,' ignored')

 END SUBROUTINE READ_PARAMETER_CONTROL

 SUBROUTINE PREPARE_INVRT_DATA
!-----------------------------

 INTEGER MCHNL,LINE_CHK,FD_ORDER,NSTAT,MD,NDT,JC,JL,JR,JF,J0,J1,J2,JRD,NCL,KPC(4)
 INTEGER, DIMENSION(NLINES):: KMP,NKMP
 REAL, ALLOCATABLE,DIMENSION (:) :: DATA_FLOOR,QDATA,Q2DATA
 LOGICAL KMP1
 CHARACTER (LEN=1) TCHR
 CHARACTER(LEN=20) CTXT(3),QL0

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
!  XDATA and XMODL are RDATA and BFTL, respectively, stacked into
!  1D array excluding all data weighted to zero.
! ----------------------------------------------------------------


!  Start reading from Leroi.inv on UNIT NRI = 13
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
     READ(NRI,*) LINE_CHK,NSTAT
     WRITE(NW,2) KMP(JL), CMP(JL)
     CALL VALIDATE_LINE (NLG,MXERR,LINE_CHK,NSTAT,JL,NLINES,LINE,NRX)

   ELSE IF (RX_TYPE(JL) == 1 .OR. RX_TYPE(JL) == 3) THEN
     READ(NRI,*) LINE_CHK,NSTAT,KMP(JL)
     CALL VALIDATE_CMP (NLG,MXERR,JL,NLINES,CMP,NCMPL,ISYS)
     WRITE(NW,2) KMP(JL), CMP(JL)
     CALL VALIDATE_LINE (NLG,MXERR,LINE_CHK,NSTAT,JL,NLINES,LINE,NRX)
     CALL VALIDATE_KMP (NLG,MXERR,JL,NLINES,CMP,KMP)
   ELSE
     READ(NRI,*) LINE_CHK,NSTAT
     WRITE(NW,2) KMP(JL), CMP(JL)
     CALL VALIDATE_LINE (NLG,MXERR,LINE_CHK,NSTAT,JL,NLINES,LINE,NRX)
   END IF
   IF (KMP(JL) > 9) NKMP(JL) = 2
   IF (KMP(JL) > 99) NKMP(JL) = 3

   IF (NSTAT /= NRX(JL)) THEN
     CALL WRITE_LOG_FILE(NLG,100,MXERR,2)
     WRITE(NLG,10) TRIM (ADJUSTL (CTXT(1)))
     WRITE(NLG,13) JL,NRX(J),NSTAT
   END IF

   IF (TDFD < 2) THEN
     READ(NRI,*) DATA_FLOOR(1)
     WRITE(NW,3) DATA_FLOOR(1)
     DATA_FLOOR(2:MCHNL) = DATA_FLOOR(1)
   ELSE IF (TDFD == 2) THEN
     READ(NRI,*) DATA_FLOOR(1:MCHNL)

     IF (FD_ORDER /= 0) THEN
       WRITE(NW,4)
       DO JF = 1,NFRQ
         WRITE(NW,6) JF,FREQ(JF),DATA_FLOOR (JF),DATA_FLOOR (JF+NFRQ)
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
   WRITE(NW,17) TRIM(ADJUSTL(CTXT(1)))
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

 KPRT = -1
 IF (TDFD == 2) THEN
   CALL WRITE_FD (NW,np,KPRT,NFRQ,MCHNL,NLINES,MRXL,MCMP,NRX,SURVEY_TYPE,LINE,IDH,RX_TYPE,UNITS, &
                  SVAZM,TITLE,ISYS,PRFL,IPLT,YXZPLT,FREQ,HEADER_ID,CMP,BFTL,RDATA,RWTS)
 ELSE
   TMS = 0.5 * (TOPN + TCLS)
   CALL WRITE_TD (NW,np,KPRT,NCHNL,NLINES,MRXL,MCMP,NRX,SURVEY_TYPE,LINE,IDH,RX_TYPE,UNITS, &
                  SVAZM,TITLE,ISYS,PRFL,IPLT,YXZPLT,TMS,HEADER_ID,CMP,BFTL,RDATA,RWTS)
 END IF

 NDT = MCHNL * MRXL * NLINES * 3
 ALLOCATE (QDATA(NDT))

 NDATA = 0                       ! Compact the data into a linear array.
 QDATA = 0                       ! Skip all data with weight 0
 DO JL = 1,NLINES
   DO JC = 1,MCMP
     DO JR = 1,NRX(JL)
       DO JF = 1,MCHNL
         IF (RWTS(JF,JR,JC,JL) > 0) THEN
           NDATA = NDATA + 1
           QDATA(NDATA) = RDATA(JF,JR,JC,JL)
         END IF
       END DO
     END DO
   END DO
 END DO
 WRITE(NW,20) NDATA
 IF (NDATA < NPAR) THEN
   CALL WRITE_LOG_FILE (NLG,100,MXERR,2)
   WRITE(NW,21) NPAR,NDATA
 END IF

 ALLOCATE (XDATA(NDATA),XMODL(NDATA))
 XDATA(1:NDATA) = QDATA(1:NDATA)
 DEALLOCATE (QDATA)

! Write the all the data not weighted to zero.


  1 FORMAT(/T2,'=======================================' &
           /T3,'Inversion controls for Line ',A)
  2 FORMAT(/T3,'KMP =',I4,4X,'CMP =',I4)
  3 FORMAT(/T3,'Time-Domain Data Floor =',G12.4,1X,A)
  4 FORMAT(/T8,'Frequency Domain Data Floors'/T8,'----------------------------' &
          //T8,'Freq     In-phase   Quadrature'/)
  5 FORMAT(/T8,'Frequency Domain Data Floors'/T8,'----------------------------' &
          //T8,'Freq     Floor'/)
  6 FORMAT(I4,F9.0,2G12.4)
  7 FORMAT(/T3,'FD_ORDER =',I2)
 10 FORMAT( T3,'Problem with Line ',A)
 13 FORMAT(T3,'NRX(',I2,') =',I2,4X,'NSTAT =',I2)
 17 FORMAT(/T3,'Weighted data for Line ',A/)
 20 FORMAT(//T3,'NDATA =',I5,' data points.' &
            /T3,'This excludes components not specified by CMP and data from the above set' &
            /T3,'that hasbeen weighted to zero using data floors.')
 21 FORMAT(/T3,'For Leroi, the number pf parameters cannot exceed the number of data points.' &
           /T3,'NPAR =',I3,4X,'NDATA =',I5)

   END SUBROUTINE PREPARE_INVRT_DATA


   SUBROUTINE SHOW_AND_TELL
!  ------------------------

! Prints out arrays and model in model-centred coordinates

!*** Called by MAIN

 IMPLICIT NONE
 REAL SXA,SXD,DEPTH(NLYR),DEL,XTNT

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

 WRITE(NW,31)
 DEPTH = 0.
 DO JL = 2, NLYR
   DEPTH(JL) = DEPTH(JL-1) + THK(JL-1)
 END DO
 DO JL = 1, NLYR
   IF(JL == NLYR) THEN
     WRITE(NW,32) NLYR,DEPTH(JL),RES(NLYR),RMU(NLYR),REPS(NLYR),CHRG(NLYR),CFREQ(NLYR),CTAU(NLYR)
   ELSE
     WRITE(NW,33) JL,THK(JL),DEPTH(JL),RES(JL),RMU(JL),REPS(JL),CHRG(JL),CFREQ(JL),CTAU(JL)
   END IF
 END DO

 PLYR = NLYR
 DO JP = 1,NPLT
   IF (INTRUDE) THEN
     DO JL = 1,NLYR
       IF (PLTOP(JP) + 0.01 > DEPTH(JL)) PLYR(JP) = JL      ! Find layer containing Plate JP
     END DO
     IF (PLYR(JP) < NLYR) THEN
       XTNT = PLWDTH(JP) * SIN (PLDIP(JP))
       DEL = PLTOP(JP) + XTNT - DEPTH(PLYR(JP)+1)
       IF (DEL > 0.) THEN
         CALL WRITE_LOG_FILE (NLG,100,MXERR,1)
         XTNT = XTNT - DEL
         PLWDTH(JP) = XTNT / SIN (PLDIP(JP))
         WRITE(NW,46) JP; WRITE(NLG,46) JP; WRITE(*,46) JP
       END IF
     END IF
   ELSE
     IF (PLTOP(JP) < DEPTH(NLYR)) THEN
       DEL = DEPTH(NLYR) - PLTOP(JP)
       PLTOP(JP) = DEPTH(NLYR) + 0.001
       CALL WRITE_LOG_FILE (NLG,100,MXERR,1)
       WRITE(NW,44) JP,DEL; WRITE(NLG,44) JP,DEL; WRITE(*,44) JP,DEL
     END IF
   END IF
   IF (ABS (XCNTR(JP)) < 0.001) XCNTR(JP) = 0.
   IF (ABS (YCNTR(JP)) < 0.001) YCNTR(JP) = 0.
   IF (PLYR(JP) < NLYR) THEN
     WRITE(NW,40) JP,PLYR(JP)
   ELSE
     WRITE(NW,47) JP
   END IF
   WRITE(NW,41) LITHP(JP),SIG_T(JP),CHRGP(JP),CFREQP(JP),CTAUP(JP)
   WRITE(NW,42) PLTOP(JP),DZM(JP),PLNGTH(JP),XCNTR(JP),DIP(JP),PLWDTH(JP),YCNTR(JP),PLG(JP)
 END DO

 IF (TDFD < 2 .AND. DO3D /= 0) WRITE(NW,51)  ! Time-domain option
 IF (TDFD == 2 .AND. DO3D /= 0) WRITE(NW,52)  ! Frequency-domain option

 WRITE(NW,53)                 ! End of input data description


 1 FORMAT(//T3,'Before computation begins, Leroi may transform array and model coordinates' &
           /T3,'from GPS coordimnates where elevation increases positive upwards to a'      &
           /T3,'body-centred system where depth increases positive downwards.'              &
           /T3,'In this system, the dip of magnetic dipole transmitters and receivers'      &
           /T3,'= 0 for vertical dipoles and 90 for horizontal dipoles.')
 2 FORMAT( /T3,'=============================================================================='  &
           /T3,'The new computational horizontal origin is over the centre of the model region.' &
          //T3,'The transmitter, receiver and plate locations below are given with respect to '  &
           /T3,'the new computation origin at: EAST: ',F12.2,';    NORTH: ',F12.2                &
           /T3,'==============================================================================')
 3 FORMAT( /T3,'The computational horizontal origin remains unchanged.')
 4 FORMAT(/T3,'Transmitter and receiver locations for Line',I7 &
           /T3,'Survey aximuth =',F5.0,'degrees clockwise from North.')
 5 FORMAT(/T3,'Vertex Locations for Loop Sources' &
          /T3,'---------------------------------')
 6 FORMAT(/T3,'Vertex Locations for Grounded Wire Sources' &
          /T3,'------------------------------------------')
 7 FORMAT(/T7,'Transmitter',I3,' has',I3,' vertices.'&
        //T10,'Easting   Northing'/)
 8 FORMAT(/T3,'Dipole Source Specification')
 9 FORMAT(/T9,'Easting   Northing      Depth    Txcln    Azimuth   Moment' &
          /T9,'-------   --------      -----    -----    -------   ------')
 10 FORMAT(/T9,'Easting   Northing      Depth   Moment' &
           /T9,'-------   --------      -----   ------')
 11 FORMAT(//T3,'Locations for Magnetic Dipole Receivers in Line',I7 &
            /T3,'------------------------------------------------------')
 12 FORMAT(//T3,'Electrode Positions for Electric Dipoles in Line',I7    &
            /T3,'-------------------------------------------------------' &
           /T10,'East 1    North 1     East 2    North 2     Depth'/)
 13 FORMAT(/T9,'Receiver & Transmitter Vertex Coordinates' &
           /T9,'-----------------------------------------' &
          //T25,'Transmitter             Receiver               Plot Point' &
           /T5,'Line',T23,'East      North',T47,'East      North',T71,'East      North')
 14 FORMAT(/T9,'Coincident Loop Vertex Coordinates' &
           /T9,'----------------------------------' &
          //T25,'Transmitter',T49,'Plot Point'                  &
           /T5,'Line',T23,'East      North',T47,'East      North')
 15 FORMAT(/I8,2I4,2F11.1,2(2X,2F11.1))
 16 FORMAT(/8X,2I4,2F11.1,2(2X,2F11.1))
 17 FORMAT(12X,I4,2F11.1,2X,2F11.1)
 18 FORMAT(/T17,'Receiver & Transmitter Dipole Coordinates' &
           /T17,'-----------------------------------------' &
         //T23,'Receiver',T58,'Transmitter'/T23,'--------',T58,'-----------'   &
           /T5,'Line          East       North     Depth         East       North      Depth     Incl    Azm',&
           /T5,'----          ----       -----     -----         ----       -----      -----     ----    ---')
 19 FORMAT(/I8,I4,2F12.1,F9.1,2X,2F12.1,F9.1,2X,2F7.0)
 20 FORMAT(/8X,I4,2F12.1,F9.1,2X,2F12.1,F9.1,2X,2F7.0)
 21 FORMAT(/T16,'Downhole Probe Coordinates & Orientation'         &
           /T16,'----------------------------------------'         &
          //T26,'Transmitter',T51,'Borehole',T75,'Receiver'     &
           /T26,'-----------',T51,'--------',T75,'--------'                  &
           /T19,          'East      North     Depth      Dip     Azm       East      North     Depth '   &
           /T5,'Line',T19,'----      -----     -----      ---     ---       ----      -----     -----')
 22 FORMAT(I8,I4,2F11.1,F10.1,1X,2F8.1,2F11.1,F10.1)
 23 FORMAT(8X,I4,2F11.1,F10.1,1X,2F8.1,2F11.1,F10.1)
 24 FORMAT(//T3,'Plot points for receivers on Line',I7, &
           //T13,'East        North     Elev'          &
            /T13,'----        -----     ----' /I4,2F13.1,F9.1)
 31 FORMAT(//T11,'+----------------------------------+'  &
            /T11,'+  Layered Earth Model Parameters  +'  &
            /T11,'+----------------------------------+'  &
           //T2,'                   Depth' &
            /T2,'Layer  Thickness   to Top    Resistivity   MU-R   EPS-R   CHRG    CFREQ    CTAU' &
            /T2,'-----  ---------   ------    -----------   ----   -----   ----    -----    ----')
 32 FORMAT(I4,11X,F11.1,G15.4,2F7.2,2F8.2,G13.2)
 33 FORMAT(I4,   2F11.1,G15.4,2F7.2,2F8.2,G13.2)
 40 FORMAT(//T3,'Input Data for Plate',I3,' - contained in Layer',I3 &
            /T3,'-----------------------------------------------')
 41 FORMAT(/T3,'LITHP =',I3,':   SIGMA_T =',F7.1,';   CHRGP =',F7.3,';   CFREQP =',F7.3,';   CTAUP =', G11.2)
 42 FORMAT(/T3,'Depth to top =',F9.1,';    Dip Azimuth =',F6.0,';   Plate length =',F7.1 &
           /T3,'Centre North =',F9.1,';    Dip         =',F6.0,';   Plate width  =',F7.1 &
           /T3,'Centre East  =',F9.1,';    Plunge      =',F6.0)
 44 FORMAT (/T3,'All plates must be in basement unless DO3D = 3' &
            /T3,'Plate',I3,' has been shifted down by',F8.2,' metres.')
 46 FORMAT (/T3,'Plates are not allowed to cross layer boundaries.'&
            /T3,'The width of Plate',I3,' has been shortened')
 47 FORMAT(//T3,'Input Data for Plate',I3,' - contained in Basement' &
            /T3,'-----------------------------------------------')
 51 FORMAT(/T3,'Leroi will compute 3D responses for a time-domain system.'/)
 52 FORMAT(/T3,'Leroi will compute 3D responses for a frequency-domain system.'/)
 53 FORMAT(/75('-')/T24,'END OF INPUT DATA DESCRIPTION'/75('-'))
 61 FORMAT(//T3,'Locations for Point E-field Receivers in Line',I7     &
            /T3,'----------------------------------------------------' &
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
     MAX_FREQD = 1.0E7_QL		! DWA 20180207 was 1.0E5_QL ...
     IF (T0 < .28E-3) PPDH = 6         ! Extend range to 1 MHz
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
     IF (FDUM(J) > 0.9999 * MAX_FREQD) EXIT
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

   SUBROUTINE SET_RHO
!  ------------------

!  Sets up horizontal interpolation array (12 digit precision) for Hankel transforms
!  from 0.1 m to 10 km

!***  Called by READ_LEROI_DATA

   USE LG_Filter_Coefficients

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

   Open (Unit = 66, File = 'Leroi.trp')
   Do jr = 1, MXRHO
   		Write (66, *) rhotrp(jr)
   	End Do
   Close (66)

   END SUBROUTINE SET_RHO

   SUBROUTINE SET_TRP
!  ------------------

!  Sets up interpolation times for FD -> TD transform which use the
!  exact 6 points per decade frequency-domain data plus 6 per decade
!  interpolated values.  These are based on a 12 point per decade
!  cosine filter derived from the Niels Christensen routine FILCOA
!  with OMEGA = .3 PI and shift 0.

!***  Called by: MAIN program
!***       Uses:  MODULE LG_Filter_Coefficients

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
!    =4 (coincident loop);     =5 (Sampo)
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
 CHARACTER(LEN=7) RES_MOD(9), THK_MOD(9)
 CHARACTER(LEN=20) SVTXT,SXTXT,QL,QL0
 CHARACTER(LEN=82) PLT_MOD(9)
 DATA RES_MOD /'  RES_1','  RES_2','  RES_3','  RES_4','  RES_5','  RES_6','  RES_7','  RES_8','  RES_9'/
 DATA THK_MOD /'  THK_1','  THK_2','  THK_3','  THK_4','  THK_5','  THK_6','  THK_7','  THK_8','  THK_9'/
 DATA PLT_MOD /'  SIG_T_1  PLNGTH_1  DIP_WDTH_1  DEPTH_1  EAST_1  NORTH_1  DIP_AZM_1  DIP_1  PLG_1', &
               '  SIG_T_2  PLNGTH_2  DIP_WDTH_2  DEPTH_2  EAST_2  NORTH_2  DIP_AZM_2  DIP_2  PLG_2', &
               '  SIG_T_3  PLNGTH_3  DIP_WDTH_3  DEPTH_3  EAST_3  NORTH_3  DIP_AZM_3  DIP_3  PLG_3', &
               '  SIG_T_4  PLNGTH_4  DIP_WDTH_4  DEPTH_4  EAST_4  NORTH_4  DIP_AZM_4  DIP_4  PLG_4', &
               '  SIG_T_5  PLNGTH_5  DIP_WDTH_5  DEPTH_5  EAST_5  NORTH_5  DIP_AZM_5  DIP_5  PLG_5', &
               '  SIG_T_6  PLNGTH_6  DIP_WDTH_6  DEPTH_6  EAST_6  NORTH_6  DIP_AZM_6  DIP_6  PLG_6', &
               '  SIG_T_7  PLNGTH_7  DIP_WDTH_7  DEPTH_7  EAST_7  NORTH_7  DIP_AZM_7  DIP_7  PLG_7', &
               '  SIG_T_8  PLNGTH_8  DIP_WDTH_8  DEPTH_8  EAST_8  NORTH_8  DIP_AZM_8  DIP_8  PLG_8', &
               '  SIG_T_9  PLNGTH_9  DIP_WDTH_9  DEPTH_9  EAST_9  NORTH_9  DIP_AZM_9  DIP_9  PLG_9'/

 WRITE(np,1) FVERS,Trim(PNAME),TRIM( ADJUSTL (TITLE))
 CALL GET_SURVEY_TEXT (SURVEY_TYPE,SVTXT)
 CALL GET_SOURCE_TEXT (SOURCE_TYPE,SXTXT)

 IF (TDFD < 2) THEN
   WRITE(np,2) TRIM (ADJUSTL (SVTXT)),TRIM (ADJUSTL (SXTXT))
   WRITE(np,3) NCHNL,REFTYM
   WRITE(np,4) TMS(1:NCHNL)
   WRITE(np,5) WTMS(1:NCHNL)
   IF (ISYS == 4) WRITE(np,15)
 ELSE
   WRITE(np,6) TRIM (ADJUSTL (SVTXT)),TRIM (ADJUSTL (SXTXT))
   WRITE(np,7) NFRQ
   WRITE(np,8) FREQ(1:NFRQ)
   IF (ISYS == 2) WRITE(np,14)
 END IF

 WRITE(np,9) NLINES

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

 ! WRITE(np,10) NLYR,NPLT
 ! WRITE(np,11) PLT_MOD(1:NPLT),RES_MOD(1:NLYR),THK_MOD(1:NLYR-1)
 ! WRITE(np,12) MPAR(1:NPAR)
  !
 ! write model
 Write (np, 10) nlith, nlyr, nplt
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
    Write (np, 25) jl, LITHP(jl),YCNTR(jl),XCNTR(jl),PLTOP(jl), PLNGTH(jl),PLWDTH(jl),DZM(jl),DIP(jl), PLG(jl)
    ! PLTOP(JP),DZM(JP),PLNGTH(JP),XCNTR(JP),DIP(JP),PLWDTH(JP),YCNTR(JP),PLG(JP)
 End Do
 Write (np, 30)

  1 FORMAT(T1,'/ ',I4.4,T15,'File version number'/T1,'/ PROGRAM_NAME: ',A/T1,'/ TITLE: ',A)
  2 FORMAT(T1,'/ Time-Domain Survey_Type = ',A,4X,'Source_Type = ',A)
  3 FORMAT(T1,'/ NCH=',I3.3,4X,'REFTYM(ms)=',G12.4)
  4 FORMAT(T1,'/ TIMES(ms)=',T17,100en15.6)
  5 FORMAT(T1,'/ CHNL_WDTH(ms)=',T17,100en15.6)
  6 FORMAT(T1,'/ Frequency-Domain Survey_Type = ',A,4X,'Source_Type = ',A)
  7 FORMAT(T1,'/ NFRQ=',I3.3)
  8 FORMAT(T1,'/ FREQS(Hz) =',100G13.4)
  9 FORMAT(T1,'/ NLINES =',I3)
 ! 10 FORMAT(T1,'/ LAYERS=',I2.2/T1,'/ PLATES=',I2.2)
  10 Format('/', /, &
           '/ Model', /, &
           '/ -----', /, &
           '/ Lithologies: ', i4, /, &
           '/ Layers:      ', i4, /, &
           '/ Targets:     ', i4, /)
 11 FORMAT(T1,'/ MODEL_HEADER'/T1,'/ ',120A)
 12 FORMAT(T1,'/ MODEL_00',84F13.2)
 14 FORMAT(T1,'/ SYSTEM: SAMPO')
 15 FORMAT(T1,'/ SYSTEM: UTEM')
 23 Format ('/ Lithology: ', i2, 7(2x, en13.4))
 24 Format ('/ Layer:     ', i2, 3(2x, en13.4))
 26 Format ('/ Base.:     ', i2, 2(2x, en13.4))
 25 Format ('/ Target:    ', i2, 4x, i2, 8(2x, en13.4))
 27 Format ('/             #', 4x, 'Resistivity', 9x, 'SigmaT', 9x, 'Rel.mu', 8x, 'Rel.eps', 8x, &
    'Charge.', 12x, 'Tau', 10x, 'Freq.')
 28 Format ('/             #', 4x, 'Resistivity', 6x, 'Thickness', 4x, 'Conductance')
 29 Format ('/             #', x, 'Lith.', 11x, 'East', 10x, 'North', 10x, 'Depth', &
    9x, 'Length', 10x, 'Width', 9x, 'Strike', 12x, 'Dip', 9x, 'Plunge')
 30 Format ('/')

   END SUBROUTINE WRITE_np_INITIAL

   SUBROUTINE WRITE_LINE_HEADER (QL,HID,CL,NCL)
!  -------------------------------------------
!
!    QL  - Line number in character form
!    HID - Header ID defining survey & line character
!    CL  - component control

!  For inversion of magnetic dipole or point electric receiver data, all three components of the
!  data are put into the mv1 file, even if some are given null status.

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

 CASE DEFAULT
   CALL WRITE_LOG_FILE (NLG,35,MXERR,2)   !  Write fatal error message
 END SELECT


 1 FORMAT(T1,'/ LINE_HEADER    LINE_ID ',A,4X,'HID:',I4,4X,'NCTD:',I2 /T1,'/ DatIndx  XLOC  YLOC  ZLOC  ',250(A,I3.3))

   END SUBROUTINE WRITE_LINE_HEADER
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

 END MODULE LG_Input_routines

 PROGRAM MAIN
!------------

!*** Calls FDREAD, HSBOSS, HSBOSS_FRQ, LEROI_3D, SET_SWYTD, TDEM_3D
!          TDEM_3D, WRITE_FD, WRITE_TD, WRITE_LOG_FILE

!*** Calls from LG_Input_routines:
!          READ_READ_SYSTEMY_DATA, READ_MODEL_DATA, SET_TRP, SET_FRQ,

 USE LG_Input_routines
 USE LG_Frequency_select

 IMPLICIT NONE
 INTEGER QQDT(8),QQHMS(2),NFRQHS,IPR
 COMPLEX, PARAMETER :: ZERO=(0.,0.)
 REAL, ALLOCATABLE :: FRQHS(:), BPRM(:,:)
 REAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: BTD,BTD_SCAT
 COMPLEX, ALLOCATABLE, DIMENSION(:,:,:,:) :: BFD_SCAT,BFD
 REAL CMP_start, CMP_final, CMP_delta


 OPEN(NR,FILE = 'Leroi.cfl',STATUS = 'OLD')
 OPEN(NW,FILE = 'Leroi.out',STATUS = 'REPLACE')

 CALL CPU_TIME (CMP_start)

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

 END SELECT
 DEALLOCATE (DSTAT,SDN0,SDE0,XRXOF,YRXOF)
 IF (SOURCE_TYPE == 1) CALL SET_VERTEX_ORDER (NTX,MXVRTX,NVRTX,SXN,SXE)

 IF (MAXVAL (KNORM) > 0) THEN     !  Set up DC primary fields
   IF (SOURCE_TYPE == 1) THEN
      CALL PRMDC_LP (NTX,MXVRTX,NVRTX,SXN,SXE,MRXTX,NRXTX,MQVR,RXID,XRXTX,YRXTX,ZRXTX,KNORM2,BPRM)
   ELSE IF (SOURCE_TYPE == 3) THEN
     CALL PRMDC_MD (NTX,SXN,SXE,SXZ,SXDIP,SXAZM,MRXTX,NRXTX,RXID,XRXTX,YRXTX,ZRXTX,KNORM2,BPRM)
   END IF
 END IF

 ! CALL SHOW_AND_TELL        ! Set & Print array & model coordinates in body-centred system
 IF (INVERT) THEN
   OPEN(NRI,FILE = 'Leroi.inv',STATUS = 'OLD')
   OPEN(np,FILE = 'Leroi.mv1',STATUS = 'REPLACE')
   CALL WRITE_np_INITIAL
   CALL READ_PARAMETER_CONTROL
   CALL PREPARE_INVRT_DATA
   CLOSE (NR)
 ELSE
   OPEN(np,FILE = 'Leroi.mf1',STATUS = 'REPLACE')
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
   CALL SET_SWYTD (nw, NSX,SWX,SWY,T0SX)  ! Compute dI/dt at the receiver
   CALL SET_TRP
   CALL SET_FRQ
 END IF

 IF (KHSQ == 1) THEN
   NFRQHS = NFRQ
   ALLOCATE (FRQHS(NFRQ))
   FRQHS(1:NFRQ) = FREQ(1:NFRQ)
 ELSE
   NFRQHS = NF_6PDE
   ALLOCATE (FRQHS(NFRQHS))
   FRQHS(1:NFRQHS) = FRQ_6PDE(1:NFRQHS)
 END IF

 IF (INVERT) THEN
   CALL NLSQ2 (NPAR,NDATA,XDATA,RDATA,RWTS,PCTCNV,NDSTP,KPCT,CXPAR,ELAS,LBND,UBND,MAXITS,INVPRT, &
               NW,np,TITLE,SURVEY_TYPE,SOURCE_TYPE,NLINES,MLINES,LINE,HEADER_ID,IPLT,YXZPLT,    &
               MCMP,CMP,MRXL,MCHNL,NCHNL,NFRQ,NFRQHS,TDFD,STEP,FREQ,FRQHS,NSX,SWX,SWY,NPULS,     &
               PULSE,NTYPLS,NTYRP,TRP,TOPN,TCLS,NFT,CURNT,ISYS,SVAZM,UNITS,RX_TYPE,RXMNT,IDH,    &
               MXTX,NTX,SXN,SXE,SXZ,SXDIP,SXAZM,MXVRTX,NVRTX,NRX,LNTR,NRXTX,MRXTX,RXID,MQVR,     &
               MXRS,XRXTX,YRXTX,ZRXTX,BPRM,MD1,MD2,RXAZM,RXDIP,KNORM2,NCTD,MXRHO,RHOTRP,NLYR,    &
               THK,RES,RMUD,REPS,CHRG,CTAU,CFREQ,NCNTRD,ECNTRD,NPLT,SIG_T,PLNGTH,PLWDTH,XCNTR,   &
               YCNTR,PLTOP,PLAZM,PLDIP,PLUNJ,CELLW,MXAB,CHRGP,CTAUP,CFREQP)
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

     IPR = NW
     CALL LEROI_3D (IPR,NFRQ,FREQ,SOURCE_TYPE,NTX,MXVRTX,NVRTX,SXN,SXE,SXZ,SXDIP,SXAZM,NRXTX, &
                    MRXTX,RXID,MQVR,MXRS,XRXTX,YRXTX,ZRXTX,NLYR,THKD,RES,RMUD,REPS,CHRG,CTAU, &
                    CFREQ,NPLT,MXAB,CELLW,PLNGTH,PLWDTH,XCNTR,YCNTR,PLTOP,PLAZM,PLDIP,PLUNJ,  &
                    INTRUDE,SIG_T,CHRGP,CTAUP,CFREQP,MXRHO,RHOTRP,INVERT,BFD_SCAT)

     CLOSE (NLG)

!  End of frequency stepping.
!  Write out the total frequency-domain scattered magnetic fields for each to UNIT ND.

     IF (TDFD < 2 .AND. DO3D == 1) THEN  ! Time-Domain
       DO JF = 1,NFRQ
         DO JS = 1, NTX
           DO JR = 1, NRXTX(JS)
             WRITE(ND,'(6E16.6,3X,A,G12.4,2I4)') BFD_SCAT(JF,JR,JS,1:3),'frq, jr, js',FREQ(JF),JR,JS
           END DO
         END DO
       END DO

       CLOSE (ND)
       WRITE(*, 10)
     END IF
   END IF NEW_3D_MODEL

   IF (TDFD == 2) THEN      !  Construct frequency-domain output
     ALLOCATE ( BFD(NFRQ,MRXTX,NTX,3))
     BFD = ZERO

      CALL HSBOSS (NFRQ,FREQ,SOURCE_TYPE,NTX,MXVRTX,NVRTX,SXN,SXE,SXZ,SXDIP,SXAZM, &
                   NRXTX,MRXTX,RXID,MQVR,XRXTX,YRXTX,ZRXTX,MXRHO,RHOTRP,NLYR,THKD, &
                   RES,RMUD,REPS,CHRG,CTAU,CFREQ,BFD)

!  Redefine BFD as the total response.
!  Reconfigure output from Tx to Line basis.
!  Perform additional output processing where required

     IF (DO3D > 0) BFD = BFD + BFD_SCAT

     CALL SET_OUTPUT_LINES_FD (NFRQ,MCHNL,NTX,MRXTX,NRXTX,NLINES,MRXL,MCMP,NRX,LNTR,KNORM2,RX_TYPE,RXMNT, &
                               UNITS,ISYS,IDH,SVAZM,MD1,MD2,RXAZM,RXDIP,CURNT,BPRM,BFD,BFTL)

     CALL WRITE_FD (NW,np,KPRT,NFRQ,MCHNL,NLINES,MRXL,MCMP,NRX,SURVEY_TYPE,LINE,IDH,RX_TYPE,UNITS, &
                    SVAZM,TITLE,ISYS,PRFL,IPLT,YXZPLT,FREQ,HEADER_ID,CMP,BFTL,RDATA,RWTS)

     IF (PRTSEC) THEN
       WRITE(NW,11)
       WRITE(np,12)
       CALL SET_OUTPUT_LINES_FD (NFRQ,MCHNL,NTX,MRXTX,NRXTX,NLINES,MRXL,MCMP,NRX,LNTR,KNORM2,RX_TYPE,RXMNT, &
                                 UNITS,ISYS,IDH,SVAZM,MD1,MD2,RXAZM,RXDIP,CURNT,BPRM,BFD_SCAT,BFTL)

       LINE = LINE + 900000000
       CALL WRITE_FD (NW,np,KPRT,NFRQ,MCHNL,NLINES,MRXL,MCMP,NRX,SURVEY_TYPE,LINE,IDH,RX_TYPE,UNITS, &
                      SVAZM,TITLE,ISYS,PRFL,IPLT,YXZPLT,FREQ,HEADER_ID,CMP,BFTL,RDATA,RWTS)

     END IF

   ELSE IF (TDFD < 2) THEN   ! Time-Domain.  Compute BTD_SCAT, the Scattering Response

     ALLOCATE ( BTD(NCHNL,MRXTX,NTX,3), BTD_SCAT(NCHNL,MRXTX,NTX,3))
     BTD = 0.
     BTD_SCAT = 0.

     IF (DO3D > 0) THEN
       IF (DO3D == 2) THEN
         CALL FDREAD (ND,NFRQ,NTX,MRXTX,NRXTX,NCTD,BFD_SCAT)  !  Read old frequency-domain data
         CLOSE (ND)
       END IF

       CALL TDEM_3D (STEP,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL,TOPN,TCLS, &
                     FREQ,NFRQ,NTX,MRXTX,NRXTX,RXID,NCTD,BFD_SCAT,BTD_SCAT)

       DEALLOCATE (BFD_SCAT)
     END IF

!  Compute BTD, the layered earth response convolved with the excitation waveform

     CALL HSBOSS_TD (NFRQHS,FRQHS,STEP,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL, &
                     TOPN,TCLS,SOURCE_TYPE,NTX,MXVRTX,NVRTX,SXN,SXE,SXZ,SXDIP,SXAZM,   &
                     NRXTX,RXID,MRXTX,MQVR,XRXTX,YRXTX,ZRXTX,MXRHO,RHOTRP,NLYR,THKD,   &
                     RES,RMUD,REPS,CHRG,CTAU,CFREQ,NCTD,BTD)

!  Redefine BTD as the total response.
!  Reconfigure output from Tx to Line basis.
!  Perform additional output processing where required

     IF (DO3D > 0) BTD = BTD + BTD_SCAT

     CALL SET_OUTPUT_LINES_TD (NCHNL,NTX,MRXTX,NRXTX,NLINES,MRXL,MCMP,NRX,LNTR,KNORM2,RX_TYPE,RXMNT, &
                               UNITS,ISYS,IDH,SVAZM,MD1,MD2,RXAZM,RXDIP,CURNT,BPRM,BTD,BFTL)

!  Write out the results.

     CALL WRITE_TD (NW,np,KPRT,NCHNL,NLINES,MRXL,MCMP,NRX,SURVEY_TYPE,LINE,IDH,RX_TYPE,UNITS, &
                    SVAZM,TITLE,ISYS,PRFL,IPLT,YXZPLT,TMS,HEADER_ID,CMP,BFTL,RDATA,RWTS)

     IF (PRTSEC) THEN
       WRITE(NW,13)
       WRITE(np,14)
       CALL SET_OUTPUT_LINES_TD (NCHNL,NTX,MRXTX,NRXTX,NLINES,MRXL,MCMP,NRX,LNTR,KNORM2,RX_TYPE,RXMNT, &
                                 UNITS,ISYS,IDH,SVAZM,MD1,MD2,RXAZM,RXDIP,CURNT,BPRM,BTD_SCAT,BFTL)

       LINE = LINE + 900000000
       CALL WRITE_TD (NW,np,KPRT,NCHNL,NLINES,MRXL,MCMP,NRX,SURVEY_TYPE,LINE,IDH,RX_TYPE,UNITS, &
                      SVAZM,TITLE,ISYS,PRFL,IPLT,YXZPLT,TMS,HEADER_ID,CMP,BFTL,RDATA,RWTS)
     END IF

   END IF
 END IF


 ! complete run time calcs & sign off ...
 call date_and_time(Values = tvals)
 call CPU_time(CMP_final)
 CMP_delta = CMP_final - CMP_start
 Write (np, 11) trim(PNAME), 'forward model', tvals(1:3), tvals(5:7), CMP_delta
 Write (nw, 12) trim(PNAME), 'forward model', tvals(1:3), tvals(5:7), CMP_delta
 Write ( *, 12) trim(PNAME), 'forward model', tvals(1:3), tvals(5:7), CMP_delta

 CLOSE (NW)
 CLOSE (NLG)

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
13 FORMAT(//T3,'*************************************' &
           //T3,'      SCATTERED FIELD OUTPUT'          &
           //T3,'*************************************')
14 FORMAT(T1,'/'/T1,'/ SCATTERED FIELDS'/'/')
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


 USE LG_Filter_Coefficients

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

 COMPLEX FUNCTION C2DINTRP (XV,NX,ZV,NZ,FR,FI,X1,Z1)
!---------------------------------------------------

!  2 dimensional interpolation of a complex function (FR,FI) on point (X1,Z1)
!  For each value of Z in ZV, a cubic spline repesents the functions in the X direction.
!  The cubic splines are evaluated at X1 for 4 different Z values, above and below Z1
!  A four point non-uniform Lagrange interpolation is used inhe Z direction.
!
!***  Called by
!***  Calls INTERV, INTERV_Z
!
!  XV(NX) - increasing array of X values
!  ZV(NZ) - increasing array of Z values
!  FR(1:4,KX,KZ) - real function values at XV(KX), ZV(KZ)
!  FI(1:4,KX,KZ) - imaginary function values at XV(KX), ZV(KZ)

 IMPLICIT NONE
 INTEGER KX,KZ,MFL,NFL,NX,NZ,J1,JZ
 REAL X1,Z1,XV(NX),ZV(NZ),FR(4,NX,NZ),FI(4,NX,NZ),H,HZ,D1,D4,P,P2,VR,VI
 REAL,DIMENSION(4) :: CR,CI,A

!  Find index KX of largest breakpoint to the left of X1.

 CALL INTERV (XV, NX-1, X1, KX, MFL)
 IF (MFL == -1) H = 0.
 H = X1 - XV(KX)

!  Find index KZ of largest ZV above Z1.

 CALL INTERV_Z (ZV, NZ-1, Z1, KZ, NFL)
 HZ = ZV(KZ+1) - ZV(KZ)
 D1 = (ZV(KZ) - ZV(KZ-1)) / HZ
 D4 = (ZV(KZ+2) - ZV(KZ)) / HZ
 P = (Z1 - ZV(KZ)) / HZ
 P2 = P*P

 A(1) = (P2-P) * (D4-P) / (D1 * (1.+D1) * (D1+D4))
 A(2) = (D1+P) * (1.-P) * (D4-P) / (D1 *D4)
 A(3) = (D1*P+P2) * (D4-P) / ((1.+D1) * (D4-1.))
 A(4) = (D1+P) * (P2-P) / (D4 * (D1+D4) * (D4-1.))

 DO J1 = 1,4
   JZ = KZ - 2 + J1
   CR(J1) = ((FR(4,KX,JZ)*H/3.0 + FR(3,KX,JZ) )*0.5*H + FR(2,KX,JZ) )*H + FR(1,KX,JZ)
   CI(J1) = ((FI(4,KX,JZ)*H/3.0 + FI(3,KX,JZ) )*0.5*H + FI(2,KX,JZ) )*H + FI(1,KX,JZ)
 END DO
 VR = DOT_PRODUCT (A,CR)
 VI = DOT_PRODUCT (A,CI)

 C2DINTRP = CMPLX (VR,VI)

 END FUNCTION C2DINTRP

 SUBROUTINE CUBVALRZ (X_ARRAY, NRVAL, NZVAL, FUN_R, FUN_I, X1, JZ, C2)
!-------------------------------------------------------------------

!  Uses method of CUBSPL to create complex C2 at X1 from two
!  real splined functions of depth index JZ.

!***  Called by
!***  Calls INTERV.  On exit from INTERV
!
!******************************************************************************

  IMPLICIT NONE
  INTEGER I,MFLAG,NRVAL,NZVAL,JZ
  REAL X_ARRAY(NRVAL),FUN_R(4,NRVAL,NZVAL),FUN_I(4,NRVAL,NZVAL),X1R
  REAL X1,H,CR,CI,A(4),B(4)
  COMPLEX C2

!  Find index I of largest breakpoint to the left of X1.

  X1R = REAL (X1)
  CALL INTERV ( X_ARRAY, NRVAL-1, X1R, I, MFLAG )
  H = X1 - X_ARRAY(I)
  IF (MFLAG == -1) H = 0.
  A(1:4) = FUN_R(1:4,I,JZ)
  B(1:4) = FUN_I(1:4,I,JZ)
  CR = ((A(4)*H/3. + A(3))*0.5*H + A(2))*H + A(1)
  CI = ((B(4)*H/3. + B(3))*0.5*H + B(2))*H + B(1)
  C2 = CMPLX (CR,CI)

 END SUBROUTINE CUBVALRZ

 SUBROUTINE CCUBVAL (X_ARRAY, NVAL, FUN_R, FUN_I, X1, C2)
!-----------------------------------------------------------

!  Uses method of CUBSPL to create complex CD2 at X1 from two
!  real splined functions

!***  Called by
!***  Calls INTERV.  On exit from INTERV
!
!******************************************************************************

  IMPLICIT NONE
  INTEGER I,MFLAG,NVAL
  REAL X_ARRAY(NVAL),FUN_R(4,NVAL),FUN_I(4,NVAL),X1R
  REAL X1,H,CR,CI,A(4),B(4)
  COMPLEX C2

!  Find index I of largest breakpoint to the left of X1.

  X1R = REAL (X1)
  CALL INTERV ( X_ARRAY, NVAL-1, X1R, I, MFLAG )
  H = X1 - X_ARRAY(I)
  IF (MFLAG == -1) H = 0.
  A(1:4) = FUN_R(1:4,I)
  B(1:4) = FUN_I(1:4,I)
  CR = ((A(4)*H/3. + A(3))*0.5*H + A(2))*H + A(1)
  CI = ((B(4)*H/3. + B(3))*0.5*H + B(2))*H + B(1)
  C2 = CMPLX (CR,CI)

 END SUBROUTINE CCUBVAL

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

  DO JF = 1,NFRQ
    DO JS = 1, NTX
      DO JR = 1, NRXTX(JS)
        NC = NCTD(JR,JS)
        NC2 = 2*NC
        READ(ND,*) A(1:NC2)
        DO JC = 1,NC
          BFD_SCAT(JF,JR,JS,JC) = CMPLX (A(2*JC-1), A(2*JC))
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

 SUBROUTINE INTERV_Z (XT, LXT, X, LEFT, MFLAG)
!---------------------------------------------

!  Copy of INTERV to allow efficient searches in two directions.

!***   Called by
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

 END SUBROUTINE INTERV_Z

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

 SUBROUTINE SET_OUTPUT_LINES_FD (NFRQ,MCHNL,NTX,MRXTX,NRXTX,NLINES,MRXL,MCMP,NRX,LNTR,KNORM2,RX_TYPE,RXMNT, &
                                 UNITS,ISYS,IDH,SVAZM,MD1,MD2,RXAZM,RXDIP,CURNT,BPRM,BFD,BFTL)
!---------------------------------------------------------------------------------------------------------

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


 IMPLICIT NONE
 INTEGER MCHNL,MCMP,NFRQ,NTX,MRXTX,NRXTX(NTX),NLINES,MRXL,MD1,MD2,LNTR(4,NLINES), &
         KNORM2(MRXTX,NTX),ISYS,JS,JR,JL,JRL,JC,JF1,JF2
 INTEGER, DIMENSION(NLINES) :: NRX,UNITS,RX_TYPE,IDH
 REAL, DIMENSION(NLINES) :: SVAZM,RXMNT
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
   A1 = A1 * RXMNT(JL)

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
           BFTL(JF1,JR,1,JL) = -SDP * QR(3) + CDP * QXR        ! local u component
           BFTL(JF2,JR,1,JL) = -SDP * QI(3) + CDP * QXI        ! local u component
           BFTL(JF1,JR,1,JL) = -BFTL(JF1,JR,1,JL)                ! local u component sign change to amuse EMIT
           BFTL(JF2,JR,1,JL) = -BFTL(JF2,JR,1,JL)                ! local u component sign change to amuse EMIT
           BFTL(JF1,JR,2,JL) = QYR                             ! local v component
           BFTL(JF2,JR,2,JL) = QYI                             ! local v component

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

 SUBROUTINE SET_OUTPUT_LINES_TD (NCHNL,NTX,MRXTX,NRXTX,NLINES,MRXL,MCMP,NRX,LNTR,KNORM2,RX_TYPE,RXMNT, &
                                 UNITS,ISYS,IDH,SVAZM,MD1,MD2,RXAZM,RXDIP,CURNT,BPRM,BTD,BFTL)
!----------------------------------------------------------------------------------------------------

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
         ISYS,JS,JR,JL,JRL,JC,JT,JTR
 INTEGER, DIMENSION(NLINES) :: NRX,UNITS,RX_TYPE,IDH
 REAL, DIMENSION(NLINES) :: SVAZM,RXMNT
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

 BFTL = 0.
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
   A1 = A1 * RXMNT(JL)

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
           BFTL(JT,JR,1,JL) = -SDP * QZ + CDP * QX          ! local U component
           BFTL(JT,JR,1,JL) = -BFTL(JT,JR,1,JL)             ! local u component sign change to amuse EMIT
           BFTL(JT,JR,2,JL) = QY                            ! local V component

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
           JTR = NCHNL - JT + 1
           QT(JT) = BFTL(JTR,JR,JC,JL)
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

 SUBROUTINE SET_SWYTD (nw, NSX,SWX,SWY,T0SX)
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
 ! DWA 20180206: was T0_MIN = 1e-7 ...
 REAL, PARAMETER :: T0_MIN=1.E-9
 INTEGER NSX,JT
 REAL SWX(NSX),SWY(NSX,3),DELT,T0SX
 Integer :: nw

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

! DWA 20180206
! Write Tx waveform & derivatives
Write (nw, 2)
Do jt = 1, NSX
	Delt = 0.
    If (jt .gt. 1) DELT = SWX(JT) - SWX(JT-1)		! correct array access issue when jt == 1
    IF (DELT > T0_MIN) THEN
     SWY(JT,1) = SWY(JT,2) / DELT
   END IF
    Write (nw, 1) jt, swx(jt), delt, swy(jt, 1:3)
End Do
Write (nw, 3)
1 Format (2x, i4, 5(2x, en15.6))
2 Format (//, 2x, '-------------------------', &
	      /, 2x, 'Waveform with derivatives', &
	      /, 5x, '#', 16x, 't', 10x, 'Delta_I', 12x, 'dI/dt', 8x, 'Delta_IxI', 12x, 'Input')
3 Format (2x, '-------------------------', /)

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
!  only used for Leroi at present.  Finally arrays are constructed containing
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
!  assigning the coordinates of the receivers by group.  This development is
!  only used for Leroi at present.  Finally arrays are constructed containing
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

 SUBROUTINE SET_Z (IACC,NPPD,ZMIN,ZMAX,SPAN,DEL0,NQS,QSTORE,NZ1)
!---------------------------------------------------------------

!  Sets up 4 point vertical interpolation array QSTORE(NZ1) between ZMIN and
!  ZMAX such that two points precede ZMIN and that 2 points follow ZMAX.
!  Intially it covers the interval SPAN with NPPD logarithmically points
!  until the interval between points, DEL1, exceeds DELO.
!  From then on a uniform interval of DEL1 is used.
!  Numerical 4 point cubic interpolation experiments with Green's function integrals
!  suggested that SPAN = MIN (skin depth, 10.) and DEL0 = skin depth / 5
!  If IACC > 1, then halve the intervals logarithmically.

 IMPLICIT NONE
 INTEGER NQS,NPPD,NZ1,JZ,NZ,IACC
 REAL QZ,ZMIN,ZMAX,SPAN,DEL0,DEL1,QSTORE(NQS)
 LOGICAL ADDPOINTS

 ADDPOINTS = .TRUE.
 QZ = LOG (SPAN) / REAL (NPPD)
 QZ = EXP (QZ)
 QSTORE(3) = ZMIN
 QSTORE(2) = ZMIN / QZ
 QSTORE(1) = QSTORE(2) / QZ

 DO JZ = 4,NQS
   NZ1=JZ
   QSTORE(JZ) = QSTORE(JZ-1) * QZ
!---------------------------
   IF (QSTORE(JZ-1) > ZMAX) THEN
     ADDPOINTS = .FALSE.
     EXIT
   END IF
!---------------------------

   DEL1 = QSTORE(JZ) - QSTORE(JZ-1)
   IF (DEL1 > DEL0) EXIT
 END DO

 IF (ADDPOINTS) THEN
   NZ = NZ1+1
   DO JZ = NZ, NQS
     NZ1 = JZ
     QSTORE(JZ) = QSTORE(JZ-1) + DEL1
     IF (QSTORE(JZ-1) > ZMAX) EXIT
   END DO
 END IF

 IF (IACC == 2) THEN   ! Halve intervals for improved interpolation accuracy
   NZ = NZ1
   NZ1 = 2*NZ1 - 1
   DO JZ = NZ,2,-1
     QSTORE (2*JZ-1) = QSTORE(JZ)
   END DO
   DO JZ = 2,NZ1-1,2
     QSTORE (JZ) = SQRT (QSTORE(JZ-1) * QSTORE(JZ+1))
   END DO
 END IF
 END SUBROUTINE SET_Z

 SUBROUTINE SET_KFG (I1,NLYR,SXLYR,RXLYR,KFG)
!--------------------------------------------

!  Determines the value of KFG needed for layer coefficient computation
!  I1 = 1 for magnetic source or 2 for electric source.

 IMPLICIT NONE
 INTEGER I1,I2,I3,SXLYR,RXLYR,NLYR,KFG

 I2 = 1; I3 = 1
 IF (SXLYR == 0) I2 = 0
 IF (RXLYR == 0) I3 = 0
 IF (I2 == 1 .AND. I3 == 1) THEN
   IF (RXLYR > SXLYR) I3 = 2
   IF (RXLYR < SXLYR) I2 = 2
 END IF
 IF (SXLYR == NLYR) I2 = 3
 IF (RXLYR == NLYR) I3 = 3
 KFG = 100*I1 + 10*I2 + I3

 END SUBROUTINE SET_KFG

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

 ! Print *, ntyrp, ntypls, NPULS, pulse
 Open (Unit = 66, File = 'Leroi.trp')
 Do jt = 1, NTYRP
 	Write (66, 1) jt, trp(jt), ypls(1, jt), ypls(2, jt), ypls(3, jt), ypls(4, jt)
 End Do
 Close(66)

 CALL CUBSPL (TRP,YPLS,NTYRP)
 FOLD = 0.
 DO JT = 1,NTYPLS
   X = TRP(JT)
   XP = X + PULSE
   FOLD(JT) = CUBVAL (TRP,YPLS,NTYRP,X) - CUBVAL (TRP,YPLS,NTYRP,XP)
   ! FOLD(JT) = CUBVAL (TRP,YPLS,NTYRP,X) - CUBVAL (TRP,YPLS,NTYRP,XP)

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

 ! DWA 20180206
 ! Write wave
 Open (Unit = 66, File = 'Leroi.wave')
 Do jt = 1, NTYPLS
 	Write (66, 1) jt, trp(jt), ypls(1, jt), ypls(2, jt), ypls(3, jt), ypls(4, jt)
 End Do
 Close(66)
1 Format (2x, i4, 5(2x, en15.6))

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
!  of the earth.  In Leroi, it is used for electric field computations.
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

 SUBROUTINE VALIDATE_LINE (NLG,MXERR,LINE_CHK,NSTAT,JL,NLINES,LINE,NRX)
!----------------------------------------------------------------------

!*** Call WRITE_LOG_FILE
!*** Called by PREPARE_INVRT_DATA

! Validates Line number LINE_CHK aagainst Line(J).
! Validates NSTAT against NRX, the number of stations on Line J

 IMPLICIT NONE
 INTEGER NLG,NLINES,LINE_CHK,NSTAT,JL,LINE(NLINES),NRX(NLINES),MXERR
 CHARACTER(LEN=20) CTXT(2),QL0

 IF (LINE_CHK /= LINE(JL)) THEN
   CALL WRITE_LOG_FILE (NLG,50,MXERR,2)
   WRITE(QL0,*) LINE(JL)
   READ(QL0,'(A)') CTXT(1)
   WRITE(QL0,*) LINE_CHK
   READ(QL0,'(A)') CTXT(2)
   WRITE(NLG,90) JL,CTXT(1:2)
 END IF

 IF (NSTAT /= NRX(JL)) THEN
   CALL WRITE_LOG_FILE (NLG,51,MXERR,2)
   WRITE(NLG,91) JL,NRX(JL),NSTAT
 END IF

 90 FORMAT(T3,'LINE(',I2,') =',A,4X,'LINE_CHK =',A)
 91 FORMAT(T3,'NRX(',I2,') =',I4,4X,'NSTAT =',I4)

 END SUBROUTINE VALIDATE_LINE

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
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
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
   WRITE(NW,'(/T3,A)') TRIM( ADJUSTL (TITLE))
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
   CTXT(2) = PLT_PT(IPLT(JL))
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
     IF (SURVEY_TYPE == 4) WRITE(NW,30) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
     IF (RX_TYPE(JL) == 2)  WRITE(NW,31) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
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
 10 FORMAT(/T3,'Line ',A,4X,'Survey azimuth =',I4,' degrees',4X,'Units = ',A,4X,'Plot point: ',A)
 11 FORMAT(/T3,'Line ',A,4X,'Magnetic dipole Rx    Survey azimuth =',I4,' degrees',4X,'Units = ',A,4X,'Plot point: ',A)
 12 FORMAT(/T3,'Line ',A,4X,'Electric dipole Rx',4X,'Units = ',A,4X,'Plot point: ',A)
 15 FORMAT(I5,2F12.1,F9.1,2048(2x, en15.6))
 16 FORMAT(/T3,'Line ',A,4X,'HID:',I4,4X,'SVAZ:',I4,4X,'Units: ',A,4X,'PP: ',A)
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
 29 FORMAT(/T10,'W : Axial Component ',A,' for Line ',A &
           /T10,'------------------------------------------')
 30 FORMAT(/T10,'Coincident Loop ',A,' for Line ',A &
           /T10,'--------------------------------------')
 31 FORMAT(/T10,'Electric Dipole ',A,' for Line ',A &
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

 1 FORMAT(/T10,'Percent Misfit for Component',I2/T10,'----------------------')

   END SUBROUTINE WRITE_TD_MISFIT

 END SUBROUTINE WRITE_TD

 SUBROUTINE WRTDP (NW,PRFL,NCHNL,TMS,MRXL,NRX1,RXPLT,YTR)
!--------------------------------------------------------

!***  Called by: WRITE_TD
!***      CallS: nil

!  Writes time-domain output in profile form.

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
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
     WRITE(NW,'(I4,F10.3,T18,50G13.4)') JT,TMS(JT),YTR(JT,1:NRX1)
   END DO
 ELSE

   WRITE(NW,1) CHN(1:NCHNL)
   WRITE(NW,2) TMS(1:NCHNL)
   WRITE(NW,'(3X)')
   DO JR = 1, NRX1
     WRITE(NW,3) JR,RXPLT(1:3,JR),YTR(1:NCHNL,JR)
   END DO
 END IF

 1 FORMAT(/T11,'RECEIVER POSITIONS',6X,35(:5X,A))
 2 FORMAT(T9,'Easting    Northing     Elev',T37,50G13.4)
 3 FORMAT(I3,2F12.1,F9.1,2048G13.4)
 4 FORMAT(/T20,'RECEIVER COORDINATES (Top to Bottom): Easting, Northing, Elevation')
 5 FORMAT(/T2,'       Window',T16,2048F13.2)
 6 FORMAT( T2,'      Centres',T16,2048F13.2)
 7 FORMAT( T2,'Chnl   (ms)  ',T16,2048F13.2)

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
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
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
 IF (KPRT == 0) WRITE(NW,'(/T3,A)') TRIM( ADJUSTL (TITLE))
 IF (ISYS == 2) THEN
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
 15 FORMAT(I5,2F12.1,F9.1,2048G13.4)
 16 FORMAT(I5,2F12.1,F9.1,2048F13.2)
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

 1 FORMAT(/T10,'Percent Inphase Misfit for Component',I2 &
          /T10,'--------------------------------------')
 2 FORMAT(/T10,'Percent Quadrature Misfit for Component',I2 &
          /T10,'------==---------------------------------')
 3 FORMAT(/T10,'Percent Sampo Misfit' &
          /T10,'--------------------')

   END SUBROUTINE WRITE_FD_MISFIT

 END SUBROUTINE WRITE_FD

 SUBROUTINE WRFDP (NW,PRFL,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)
!------------------------------------------------------------

!***  Called by WRSLV

!  Writes frequency-domain output in profile form.  IPPM = format control

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
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
     WRITE(NW,'(I4,G12.4,T18,50G13.4)') JF,FREQ(JF),YTR(JF,1:NRX1)
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
  5 FORMAT(/T16,35F13.2)
  6 FORMAT(T16,35F13.2)
  7 FORMAT( T6,'Frequency',T16,35F13.4)
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

 IF (MXERR == 0) OPEN (NLG,FILE = 'Leroi.log',STATUS = 'REPLACE')

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
 IF (MSG == 25) WRITE(NLG,25)
 IF (MSG == 26) WRITE(NLG,26)
 IF (MSG == 27) WRITE(NLG,27)
 IF (MSG == 28) WRITE(NLG,28)
 IF (MSG == 29) WRITE(NLG,29)
 IF (MSG == 30) WRITE(NLG,30)
 IF (MSG == 31) WRITE(NLG,31)
 IF (MSG == 32) WRITE(NLG,32)
 IF (MSG == 33) WRITE(NLG,33)
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
  2 FORMAT(/T3,'The allowed values for PRFL are: 1 or 11 for profile mode, 0 or 10 otherwise.' &
           /T3,'PRFL has been set to 1.')
  3 FORMAT(/T3,'The value for DO3D is outside the permitted range of -1, 0, 1, 2 or 3.' &
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
 10 FORMAT(/T3,'SURVEY_TYPE is only allowed values: 1, 2, 3, 4 or 5.')
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
 20 FORMAT(/T3,'This version of Leroi does not include magnetotellurics.' &
           /T3,'It is for controlled sources only.')
 21 FORMAT(/T3,'This lithology index is invalid.' &
           /T3,'No resistivity or conductance has been specified.')
 22 FORMAT(/T3,'Layer lithology indices must be an integer between 1 & NLITH')
 23 FORMAT(/T3,'Layer resistivities must be positive.')
 24 FORMAT(/T3,'The value given to CMP is not allowed.' &
           /T3,'CMP values must be 1, 2, 3, 12, 13, 23, or 123')
 25 FORMAT(/T3,'NLYR = number of layers including basement.' &
           /T3,'Thus NLYR must be an integer > 0.')
 26 FORMAT(/T3,'Plate lithology indices must be an integer between 1 & NLITH')
 27 FORMAT(/T3,'Plate conductance must be positive.')
 28 FORMAT(/T3,'Each plate must be contained entirely within the basement unless DO3D = 3.' &
           /T3,'One or more plates have been shifted downwards to comply with this rule.')
 29 FORMAT(/T3,'LEROI requires 0 <= DIP < 180 degrees.')
 30 FORMAT(/T3,'The point E-field receiver is not allowed for time-domain modelling')
 31 FORMAT(/T3,'The number of magnetic receivers must not exceed the number of electric receivers.')
 32 FORMAT(/T3,'The number of receivers NRX on at least one line exceeds MRXL.')
 33 FORMAT(/T3,'Plunge has been set to zero.  Setting DO3D = 3 allows plates in layers above' &
           /T3,'basement but requires all plates. to have zero plunge to facilitate computational' &
           /T3,'efficiency.  This restriction can be obviated by the perspicacious user.')
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
           /T3,'in the receiver specification in Leroi.inv')
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
                         RES,RMUD,REPS,CHRG,CTAU,CFREQ,NCTD,BTD)
!----------------------------------------------------------------------------------------

!  Computes BTD, the time-domain layered earth response convolved with the
!  excitation waveform and the receiver channels per unit receiver area.
!  For impulse response, it computes dB/dt in T / s which is the same as
!  volts per unit area.

!  For step response, it computes B in Teslas.

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
              RES,RMUD,REPS,CHRG,CTAU,CFREQ,BFD)
 DO JS = 1,NTX
   DO JR = 1, NRXTX(JS)
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
   END DO
 END DO
 END SUBROUTINE HSBOSS_TD

 SUBROUTINE HSBOSS (NFRQ,FREQ,SOURCE_TYPE,NTX,MXVRTX,NVRTX,SXN,SXE,SXZ,SXDIP,SXAZM, &
                    NRXTX,MRXTX,RXID,MQVR,XRXTX,YRXTX,ZRXTX,MXRHO,RHOTRP,NLYR,THKD,  &
                    RES,RMUD,REPS,CHRG,CTAU,CFREQ,BFD)
!---------------------------------------------------------------------------------------

!***  Called by MAIN
!***  Calls SXMDB

!  Computes BFD, the frequency-domain layered earth response.
!  Magnetic fields are in Teslas per unit amp.  Electrode response is in volts.
!
! For magnetic dipole receivers, BFD(JF,JR,JS,1:3) contains the
! 1: north, 2: east & 3:vertical components for frequency JF, transmitter JS, receiver JR.
!
! The response for electric dipoles and loop receivers is contained in BFD_SCAT(JF,JR,JS,1),
! BFD(JF,JR,JS,2:3) is set to zero.
!
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
 INTEGER NFRQ,SOURCE_TYPE,NTX,MXVRTX,RXLYR,SXLYR,NVRTX(NTX),MRXTX,MXRHO,NRHS,JS,JR, &
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

 CALL SET_NRHS

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
   DO JS = 1,NTX
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

     DO JR = 1, NRXTX(JS)                           ! Compute response for
       IF (RXID(JR,JS) == 1) MDRX = .TRUE.          !    Magnetic dipole receivers
       IF (RXID(JR,JS) == 2) EDRX = .TRUE.          !    Electric dipole receivers
       IF (RXID(JR,JS) == 3) EPRX = .TRUE.          !    Electric field at a point
     END DO

     IF (MDRX) CALL HSLPB (NFRQ,FREQ,SOURCE_TYPE,NTX,JS,SXLYR,NVRL,SXNL,SXEL,ZS,MRXTX,NRXTX,MQVR,RXID,XRXTX, &
                           YRXTX,ZRXTX,NRHS,RHOTRP,NLYR,THKD,DPTHL,RES,RMUD,REPS,CHRG,CTAU,CFREQ,BFD)

     IF (EPRX) CALL HSLPE (NFRQ,FREQ,SOURCE_TYPE,NTX,JS,SXLYR,NVRL,SXNL,SXEL,ZS,MRXTX,NRXTX,MQVR,RXID,XRXTX, &
                           YRXTX,ZRXTX,NRHS,RHOTRP,NLYR,THKD,DPTHL,RES,RMUD,REPS,CHRG,CTAU,CFREQ,BFD)

     IF (EDRX) CALL HSLPED (NFRQ,FREQ,SOURCE_TYPE,NTX,JS,SXLYR,NVRL,SXNL,SXEL,ZS,MRXTX,NRXTX,MQVR,RXID,XRXTX, &
                            YRXTX,ZRXTX,NRHS,RHOTRP,NLYR,THKD,DPTHL,RES,RMUD,REPS,CHRG,CTAU,CFREQ,BFD)
   END DO

 CASE (3)                    ! Magnetic dipole sources
   DO JS = 1,NTX
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

     DO JR = 1, NRXTX(JS)
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

     END DO                 ! End of receivers for transmitter JS
   END DO

 CASE (4)                     ! Coincident loop option
   DO JS = 1,NTX
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
     CALL HS_CDNT (NFRQ,FREQ,SXNL,SXEL,NRHS,RHOTRP,NLYR,THKD,RES, &
                   RMUD,REPS,CHRG,CTAU,CFREQ,JS,NTX,BFD)
   END DO

 END SELECT

 CONTAINS

   SUBROUTINE SET_NRHS
!  -------------------

   IMPLICIT NONE
   INTEGER JV,JRV,NRVR
   REAL HSMX,R1

!  Dimension RHOMAX FOR halfspace computations.  First set
!  maximum distance between transmitters and receivers.

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

   NRHS = 1
   DO JR = 2, MXRHO
     IF (RHOTRP(JR) < HSMX) NRHS = JR + 1
   END DO

   END SUBROUTINE SET_NRHS

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

 SUBROUTINE HS_CDNT (NFRQ,FREQ,SXNL,SXEL,NRHS,RHOTRP,NLYR,THKD,RES, &
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
!       RHOTRP - interpolation array of dimension NRHS
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
 INTEGER NFRQ,NRHS,NLYR,NTX,JF,JS,JX,JY,JR,JW,JD, &
         NDIP(4),NDPLP,NDPX,NDPY
 REAL FREQ(NFRQ),FRQ,SXNL(4),SXEL(4),RHOTRP(NRHS),X1,XLOC,XSL,YSL,DPX,DPY,WTMD,XWRX2,WYRL(4),DIPL(4)
 REAL, DIMENSION(NLYR) :: RES,REPS,CTAU,CFREQ,CHRG
 REAL, DIMENSION(4,NRHS) :: HL1R,HL1I
 REAL(KIND=QL) RMUD(0:NLYR),THKD(NLYR),YD,R1,DIPLD
 REAL, ALLOCATABLE :: YLOC(:),XRXMD(:),YRXMD(:),YWRX(:,:),RXRHO(:,:,:)
 COMPLEX HLYR(NRHS),BFD(NFRQ,1,NTX,3)
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

   CALL HSLPLP_HNK (NRHS,RHOTRP,NLYR,KSQL,RMUD,THKD,HLYR)
   HL1R(1,1:NRHS) = REAL (HLYR(1:NRHS))
   CALL CUBSPL (RHOTRP,HL1R,NRHS)
   HL1I(1,1:NRHS) = AIMAG (HLYR(1:NRHS))
   CALL CUBSPL (RHOTRP,HL1I,NRHS)

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
         CALL CDCUBVAL (RHOTRP,HL1R,HL1I,NRHS,R1,CDS1)
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

 SUBROUTINE HSLPLP_HNK (NRHS,RHOTRP,NLYR,KSQL,RMUD,THKD,HLYR)
!------------------------------------------------------------

!***  Calls HS_JMP, HSLPLP_KER
!***  Called by HSLPLP

!  VALID FOR ANY NUMBER OF LAYERS
!  LOOP TRANSMITTER FLAT ON EARTH SURFACE
!  MAGNETIC DIPOLE RECEIVER IN AIR OR AT ANY DEPTH IN ANY LAYER

!  Uses flow through Hankel transform to compute transform integrals HLYRD(NRHS)
!  which are used to compute vertical and horizontal frequency-domain magnetic
!  field components at the RX from pseudo X oriented  HED source.  The Hankel
!  uses a 15 points per decade filter coefficient set derived from Christensen's
!  FLTGEN program.

!  RHOTRP - array of NRHS logrithmically spaced (15 / decade) horizontal distances
!           at same spacing as lambda values for Hankel transform.
!     KER - stores kernel values from HSLPLP_KER

!  NLYR,KSQL,RMUD,THKD
!  are described in HSLPLP
!
!    OUTPUT is HLYR(1:NRHS,)  forward model components

 USE LG_Filter_Coefficients

 IMPLICIT NONE
 REAL(KIND=QL), PARAMETER :: VFAC0=1.0D-7
 INTEGER NRHS,NLYR,K,L,JR,K1,KBOT,KMAX,KMIN,LMAX,LMIN
 REAL(KIND=QL) DELTA,Y,Y1,LMBDA,THKD(NLYR),RMUD(0:NLYR),RHOD
 REAL RHOTRP(NRHS)
 COMPLEX HLYR(NRHS)
 COMPLEX(KIND=QL) KSQL(NLYR),HLYRD(NRHS),KER(JNLO-NRHS:JNHI)
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
   CALL HSLPLP_KER (K,JR,L,LMBDA,NLYR,KSQL,RMUD,THKD,NRHS,KER,HLYRD,JUMP)
   IF (JUMP .AND. L > -40) EXIT
 END DO

 JUMP = .FALSE.           ! Finish off the low end for RHOTRP(1)
 DO L = -51, JNLO, -1
   LMIN = L                   ! Maximum filter index used
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
   K = L + 1 - JR             ! Compute the kernel index.
   CALL HSLPLP_KER (K,JR,L,LMBDA,NLYR,KSQL,RMUD,THKD,NRHS,KER,HLYRD,JUMP)
   IF (JUMP .AND. L < -60) EXIT
 END DO

 KMIN = LMIN + 1 - JR  !  Define the range of kernel values
 KMAX = LMAX + 1 - JR  !  used for RHOTRP(1)

! Complete definition of kernel values by evaluating transform of
! maximum RHO = RHOTRP (NRHS)

 JR = NRHS
 RHOD = REAL (RHOTRP(NRHS),KIND=QL)
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
     CALL HSLPLP_KER (K,JR,L,LMBDA,NLYR,KSQL,RMUD,THKD,NRHS,KER,HLYRD,JUMP)
     IF (JUMP) EXIT
   END DO
 END IF

 DO JR = 2, NRHS-1          !  Compute transforms for all other RHO values
   DO K = KMIN, KMAX         !  using previously computed kernel values.
     L = K - 1 + JR
     HLYRD(JR) = HLYRD(JR) + KER(K) * WJ1(L)
   END DO
 END DO

 DO JR = 1,NRHS
   RHOD = REAL (RHOTRP(JR),KIND=QL)
   HLYRD(JR) = VFAC0 * HLYRD(JR) / RHOD
 END DO
 HLYR = CMPLX (HLYRD)

 END SUBROUTINE HSLPLP_HNK

 SUBROUTINE HSLPLP_KER (K,JR,L,LMBDA,NLYR,KSQL,RMUD,THKD,NRHS,KER,HLYRD,JUMP)
!----------------------------------------------------------------------------

!***  Called by HSLPLP__HNK

!  Computes the G potential kernels  for a flat surface loop transmitter
!  and a vertical component pseudo-receiver on the surface
!
!          Input
!          -----
!    NRHS - number of logrithmically horizontal distances
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
!      KER(NRHS) - kernel values for transform
!           JUMP - logical convergence indicator
!

 USE LG_Filter_Coefficients

 IMPLICIT NONE
 REAL(KIND=QL), PARAMETER :: EXP_TOL=80.D0, TOL=1.D-6, TOL2=1.D-35
 COMPLEX(KIND=QL), PARAMETER :: ONE=(1._QL,0._QL),ZERO=(0._QL,0._QL)
 INTEGER NRHS,K,JR,L,NLYR,J
 REAL(KIND=QL) THKD(NLYR),LMBDA,RMUD(0:NLYR),RMUSQ(NLYR),QR,QI
 COMPLEX(KIND=QL) LMBSQ,S0,T0,F0,XP1,FW,KER(JNLO-NRHS:JNHI),HLYRD(NRHS)
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
                   YRXTX,ZRXTX,NRHS,RHOTRP,NLYR,THKD,DPTHL,RES,RMUD,REPS,CHRG,CTAU,CFREQ,BFD)
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
!       RHOTRP - NRHS size interpolation array for horizontal distances
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
 INTEGER NTX,MRXTX,NRXTX(NTX),MQVR,RXID(MRXTX,NTX),SXLYR,RXLYR,NFRQ,NRHS,SOURCE_TYPE,NVRL,NVRLS,NLYR, &
         J1,JS,JR,JF,JV,JV1,JD,NDIP(NVRL),I1,KFG,GAM
 REAL FREQ(NFRQ),FRQ,ZRXTX(MRXTX,NTX),RXVRT,SXNL(NVRL),SXEL(NVRL),XRX,YRX,RHOTRP(NRHS),X1,XWRX(NVRL,2,MRXTX), &
      BRFD(3),BIFD(3),BRFDM,BIFDM,A1,A2
 REAL, DIMENSION(MRXTX,NTX,MQVR) :: XRXTX,YRXTX
 REAL, DIMENSION(NLYR) :: RES,REPS,CTAU,CFREQ,CHRG
 REAL, DIMENSION(NVRL) :: CPHI,SPHI,WYRL,DIPL
 REAL, DIMENSION(NVRL,MRXTX) :: YWRX
 REAL, DIMENSION(MXDIP,NVRL,MRXTX) :: XRXD,RXRHO
 REAL, DIMENSION(4,NRHS) :: HL1R,HL2R,HL3R,HL1I,HL2I,HL3I
 REAL(KIND=QL) ZR,ZS,ZPRV,RMUD(0:NLYR),THKD(NLYR),DPTHL(NLYR),YD,X1D,X2D,RHO1,RHO2,DIPLD,CPHID,SPHID
 COMPLEX HLYR(NRHS,3),BFD(NFRQ,MRXTX,NTX,3)
 COMPLEX(KIND=QL) SIGL(NLYR),KSQL(NLYR),CDS1,CDS2,BX,BY,BZ,BX0,BY0,BFDD(3)
 Logical :: WRITE_FRQ = .True.

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
         CALL HSGWB_HNK (NRHS,RHOTRP,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,KFG,GAM,SXLYR,RXLYR,HLYR)
       ELSE
         CALL HSCLB_HNK (NRHS,RHOTRP,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,KFG,GAM,SXLYR,RXLYR,HLYR)
       END IF

       HL1R(1,1:NRHS) = REAL (HLYR(1:NRHS,1))   ! For integration of non-conservative components over connected segments
       CALL CUBSPL (RHOTRP,HL1R,NRHS)
       HL1I(1,1:NRHS) = AIMAG (HLYR(1:NRHS,1))
       CALL CUBSPL (RHOTRP,HL1I,NRHS)
       HL2R(1,1:NRHS) = REAL (HLYR(1:NRHS,2))
       CALL CUBSPL (RHOTRP,HL2R,NRHS)
       HL2I(1,1:NRHS) = AIMAG (HLYR(1:NRHS,2))
       CALL CUBSPL (RHOTRP,HL2I,NRHS)
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
         CALL CDCUBVAL (RHOTRP,HL1R,HL1I,NRHS,RHO1,CDS1)
         BY = BY + CDS1
         CALL CDCUBVAL (RHOTRP,HL2R,HL2I,NRHS,RHO1,CDS2)
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
       HL3R(1,1:NRHS) = REAL (HLYR(1:NRHS,3))
       CALL CUBSPL (RHOTRP,HL3R,NRHS)
       HL3I(1,1:NRHS) = AIMAG (HLYR(1:NRHS,3))
       CALL CUBSPL (RHOTRP,HL3I,NRHS)

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

       CALL CDCUBVAL (RHOTRP,HL3R,HL3I,NRHS,RHO1,CDS1)
       CALL CDCUBVAL (RHOTRP,HL3R,HL3I,NRHS,RHO2,CDS2)
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

 If(WRITE_FRQ) Then
    Open(Unit = 66, File = 'Leroi.flayer', Status = 'Unknown')
    Do jf = 1, nfrq
        Write (66, 1) freq(jf), bfd(jf, 1, 1, 1), bfd(jf, 1, 1, 2), bfd(jf, 1, 1, 3)
    End Do
    Close(66)
 End If

1   Format (en13.4, 6(2x, en15.6))

END SUBROUTINE HSLPB

 SUBROUTINE HSCLB_HNK (NRHS,RHOTRP,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,KFG,GAM,SXLYR,RXLYR,HLYR)
!-----------------------------------------------------------------------------------------------

!***  Calls HSCLB_KER
!***  Called by HSLPB


!  Uses flow through Hankel transform to compute electric dipole response integrals
!  HLYR(NRHS,1:NKR) used to compute the path-dependent frequency-domain magnetic
!  field components at the RX from pseudo X oriented  HED source.  The Hankel transform
!  uses a 15 points per decade filter coefficient set derived from Christensen's
!  FLTGEN program.

!          Input
!          -----
!    NRHS - number of logrithmically horizontal distances
!  RHOTRP - array of NRHS logrithmically spaced (15 / decade) horizontal distances
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

!    OUTPUT is HLYR(1:NRHS,1:3)

 USE LG_Filter_Coefficients

 IMPLICIT NONE
 INTEGER, PARAMETER :: NKR=2
 REAL(KIND=QL), PARAMETER :: VFAC0=1.0D-7
 INTEGER KFG,NRHS,NLYR,GAM,RXLYR,SXLYR,K,L,JR,K1,KBOT,KMAX,KMIN,LMAX,LMIN
 REAL RHOTRP(NRHS)
 REAL(KIND=QL) ZS,ZR,DELTA,Y,Y1,LMBDA,RMUD(0:NLYR),RHOD
 REAL(KIND=QL), DIMENSION (NLYR) :: THKD,DPTHL
 COMPLEX HLYR(NRHS,3)
 COMPLEX(KIND=QL) HLYRD(NRHS,3),KER(JNLO-NRHS:JNHI,NKR)
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
    CALL HSCLB_KER (NRHS,K,JR,L,LMBDA,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                    KFG,GAM,SXLYR,RXLYR,NKR,KER,HLYRD,JUMP)
   IF (JUMP .AND. L > -40) EXIT
 END DO

 JUMP = .FALSE.           ! Finish off the low end for RHOTRP(1)
 DO L = -51, JNLO, -1
   LMIN = L                   ! Maximum filter index used
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
   K = L + 1 - JR             ! Compute the kernel index.
   CALL HSCLB_KER (NRHS,K,JR,L,LMBDA,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                   KFG,GAM,SXLYR,RXLYR,NKR,KER,HLYRD,JUMP)
   IF (JUMP .AND. L < -60) EXIT
 END DO

 KMIN = LMIN + 1 - JR  !  Define the range of kernel values
 KMAX = LMAX + 1 - JR  !  used for RHOTRP(1)

! Complete definition of kernel values by evaluating transform of
! maximum RHO = RHOTRP (NRHS)

 JR = NRHS
 RHOD = REAL (RHOTRP(NRHS),KIND=QL)
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
     CALL HSCLB_KER (NRHS,K,JR,L,LMBDA,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                     KFG,GAM,SXLYR,RXLYR,NKR,KER,HLYRD,JUMP)
     IF (JUMP) EXIT
   END DO
 END IF

 DO JR = 2, NRHS-1          !  Compute transforms for all other RHO values
   DO K = KMIN, KMAX         !  using previously computed kernel values.
     L = K - 1 + JR
     HLYRD(JR,1) = HLYRD(JR,1) + KER(K,1) * WJ0(L)
     HLYRD(JR,2) = HLYRD(JR,2) + KER(K,2) * WJ1(L)
   END DO
 END DO

 DO JR = 1,NRHS
   RHOD = REAL (RHOTRP(JR),KIND=QL)
   HLYRD(JR,2) = HLYRD(JR,2) / RHOD
   HLYRD(JR,1:2) = VFAC0 * RMUD(RXLYR) * HLYRD(JR,1:2) / RHOD
 END DO
 HLYR = CMPLX (HLYRD)

END SUBROUTINE HSCLB_HNK

 SUBROUTINE HSCLB_KER (NRHS,K,JR,L,LMBDA,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                       KFG,GAM,SXLYR,RXLYR,NKR,KER,HLYRD,JUMP)
!----------------------------------------------------------------------------

!***  Called by HSLPM0__HNK

!  Closed loop kernels and dipole response integrals for the path-dependent response from a
!  flat closed-loop transmitter and a surface or downhole magnetic dipole receiver.
!  Correction of long-standing error: GAM is necessary in KER(K,1)
!
!          Input
!          -----
!    NRHS - number of logrithmically horizontal distances
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
!    KER(NRHS,NKR) - kernel values for transform
!            HLYRD - dipole response integral
!             JUMP - logical convergence indicator

 USE LG_Filter_Coefficients

 IMPLICIT NONE
 REAL(KIND=QL), PARAMETER :: TOL=1.D-6, TOL2=1.D-35
 INTEGER NRHS,K,JR,L,NLYR,KFG,GAM,SXLYR,RXLYR,NKR,JINT
 REAL(KIND=QL) LMBDA,LMBDA2,ZR,ZS,RMUD(0:NLYR),QR,QI
 REAL(KIND=QL), DIMENSION (NLYR) :: THKD,DPTHL
 COMPLEX(KIND=QL) S(0:NLYR),XI_V,F_V,F_H,ETA_V,G_V,G_H,SL,SM,XPDIR,FW(NKR), &
                  FACV,KER(JNLO-NRHS:JNHI,NKR),HLYRD(NRHS,3)
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL
 LOGICAL JUMP

 CALL EDSX_COEF (SXLYR,RXLYR,KFG,LMBDA,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
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

 SUBROUTINE HSGWB_HNK (NRHS,RHOTRP,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,KFG,GAM,SXLYR,RXLYR,HLYR)
!------------------------------------------------------------------------------------------------

!***  Calls HSGWB_KER
!***  Called by HSLPB


!  Uses flow through Hankel transform to compute electric dipole response integrals
!  HLYR(NRHS,1:NKR) used to compute the closed and openloop frequency-domain magnetic
!  field components at the RX from pseudo X oriented  HED source.  The Hankel transform
!  uses a 15 points per decade filter coefficient set derived from Christensen's
!  FLTGEN program.

!          Input
!          -----
!    NRHS - number of logrithmically horizontal distances
!  RHOTRP - array of NRHS logrithmically spaced (15 / decade) horizontal distances
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

!    OUTPUT is HLYR(1:NRHS,1:3)

 USE LG_Filter_Coefficients

 IMPLICIT NONE
 INTEGER, PARAMETER :: NKR=3
 REAL(KIND=QL), PARAMETER :: VFAC0=1.0D-7
 INTEGER KFG,NRHS,NLYR,GAM,RXLYR,SXLYR,K,L,JR,K1,KBOT,KMAX,KMIN,LMAX,LMIN
 REAL RHOTRP(NRHS)
 REAL(KIND=QL) ZS,ZR,DELTA,Y,Y1,LMBDA,RMUD(0:NLYR),RHOD
 REAL(KIND=QL), DIMENSION (NLYR) :: THKD,DPTHL
 COMPLEX HLYR(NRHS,3)
 COMPLEX(KIND=QL) HLYRD(NRHS,3),KER(JNLO-NRHS:JNHI,NKR)
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
    CALL HSGWB_KER (NRHS,K,JR,L,LMBDA,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                    KFG,GAM,SXLYR,RXLYR,NKR,KER,HLYRD,JUMP)
   IF (JUMP .AND. L > -40) EXIT
 END DO

 JUMP = .FALSE.           ! Finish off the low end for RHOTRP(1)
 DO L = -51, JNLO, -1
   LMIN = L                   ! Maximum filter index used
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
   K = L + 1 - JR             ! Compute the kernel index.
   CALL HSGWB_KER (NRHS,K,JR,L,LMBDA,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                   KFG,GAM,SXLYR,RXLYR,NKR,KER,HLYRD,JUMP)
   IF (JUMP .AND. L < -60) EXIT
 END DO

 KMIN = LMIN + 1 - JR  !  Define the range of kernel values
 KMAX = LMAX + 1 - JR  !  used for RHOTRP(1)

! Complete definition of kernel values by evaluating transform of
! maximum RHO = RHOTRP (NRHS)

 JR = NRHS
 RHOD = REAL (RHOTRP(NRHS),KIND=QL)
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
     CALL HSGWB_KER (NRHS,K,JR,L,LMBDA,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                     KFG,GAM,SXLYR,RXLYR,NKR,KER,HLYRD,JUMP)
     IF (JUMP) EXIT
   END DO
 END IF

 DO JR = 2, NRHS-1          !  Compute transforms for all other RHO values
   DO K = KMIN, KMAX         !  using previously computed kernel values.
     L = K - 1 + JR
     HLYRD(JR,1)   = HLYRD(JR,1) + WJ0(L) * KER(K,1)
     HLYRD(JR,2:3) = HLYRD(JR,2) + WJ1(L) * KER(K,2:3)
   END DO
 END DO

 DO JR = 1,NRHS
   RHOD = REAL (RHOTRP(JR),KIND=QL)
   HLYRD(JR,2:3) = HLYRD(JR,2:3) / RHOD
   HLYRD(JR,1:3) = VFAC0 * RMUD(RXLYR) * HLYRD(JR,1:3) / RHOD
 END DO
 HLYR = CMPLX (HLYRD)

END SUBROUTINE HSGWB_HNK

 SUBROUTINE HSGWB_KER (NRHS,K,JR,L,LMBDA,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                       KFG,GAM,SXLYR,RXLYR,NKR,KER,HLYRD,JUMP)
!----------------------------------------------------------------------------

!***  Called by HSLPM0__HNK
!***  Calls EDSX_COEF

!  Kernels and magnetic dipole response integrals for response from aclosed or grounded
!  loop transmitter and a surface or downhole magnetic dipole receiver.
!
!          Input
!          -----
!    NRHS - number of logrithmically horizontal distances
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
!    KER(NRHS,NKR) - kernel values for transform
!            HLYRD - dipole response integral
!             JUMP - logical convergence indicator

 USE LG_Filter_Coefficients

 IMPLICIT NONE
 REAL(KIND=QL), PARAMETER :: TOL=1.D-6, TOL2=1.D-35
 INTEGER NRHS,K,JR,L,NLYR,KFG,GAM,RXLYR,SXLYR,NKR,JINT
 REAL(KIND=QL) LMBDA,LMBDA2,ZR,ZS,RMUD(0:NLYR),QR,QI
 REAL(KIND=QL), DIMENSION (NLYR) :: THKD,DPTHL
 COMPLEX(KIND=QL) S(0:NLYR),XI_V,F_V,F_H,ETA_V,G_V,G_H,XP,XPDIR,SL,SM,FW(NKR), &
                  FACV,KER(JNLO-NRHS:JNHI,NKR),HLYRD(NRHS,NKR)
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL
 LOGICAL JUMP

 CALL EDSX_COEF (SXLYR,RXLYR,KFG,LMBDA,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
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
                   YRXTX,ZRXTX,NRHS,RHOTRP,NLYR,THKD,DPTHL,RES,RMUD,REPS,CHRG,CTAU,CFREQ,BFD)
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
!       RHOTRP - NRHS size interpolation array for horizontal distances
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
 INTEGER NTX,MRXTX,NRXTX(NTX),MQVR,RXID(MRXTX,NTX),SXLYR,RXLYR,NFRQ,NRHS,SOURCE_TYPE,NVRL,NVRLS,NLYR, &
         J1,JF,JS,JR,JV,JV1,JD,NDIP(NVRL),KFG,I1
 REAL FREQ(NFRQ),FRQ,ZRXTX(MRXTX,NTX),RXVRT,RHOTRP(NRHS),SXNL(NVRL),SXEL(NVRL),X1,XWRX(NVRL,2,MRXTX),YWRX(NVRL,MRXTX)
 REAL, DIMENSION(MRXTX,NTX,MQVR) :: XRXTX,YRXTX
 REAL, DIMENSION(NLYR) :: RES,REPS,CTAU,CFREQ,CHRG
 REAL, DIMENSION(NVRL) :: CPHI,SPHI,WYRL,DIPL
 REAL, DIMENSION(MXDIP,NVRL,MRXTX) :: XRXD,RXRHO
 REAL, DIMENSION(4,NRHS) :: HL1R,HL2R,HL3R,HL1I,HL2I,HL3I
 REAL, DIMENSION(MRXTX) :: XR,YR
 REAL(KIND=QL) ZR,ZPRV,ZS,RMUD(0:NLYR),THKD(NLYR),DPTHL(NLYR),YD,X1D,X2D,RHO1,RHO2,DIPLD,CPHID,SPHID
 COMPLEX HLYR(NRHS,3),BFD(NFRQ,MRXTX,NTX,3)
 COMPLEX(KIND=QL) CDS1,CDS2,EX1,EY1,EX2,EY2,EZ2,KSQL(NLYR),SIGL(NLYR)
 Logical :: WRITE_FRQ = .True.

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

       CALL HSLPE_HNK (NRHS,RHOTRP,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,KFG,SXLYR,RXLYR,HLYR)

       HL1R(1,1:NRHS) = REAL (HLYR(1:NRHS,1))
       CALL CUBSPL (RHOTRP,HL1R,NRHS)
       HL1I(1,1:NRHS) = AIMAG (HLYR(1:NRHS,1))
       CALL CUBSPL (RHOTRP,HL1I,NRHS)

       IF (SOURCE_TYPE == 2) THEN                      ! Include conservative terms for open loop source.
         HL2R(1,1:NRHS) = REAL (HLYR(1:NRHS,2))
         CALL CUBSPL (RHOTRP,HL2R,NRHS)
         HL2I(1,1:NRHS) = AIMAG (HLYR(1:NRHS,2))
         CALL CUBSPL (RHOTRP,HL2I,NRHS)

         HL3R(1,1:NRHS) = REAL (HLYR(1:NRHS,3))
         CALL CUBSPL (RHOTRP,HL3R,NRHS)
         HL3I(1,1:NRHS) = AIMAG (HLYR(1:NRHS,3))
         CALL CUBSPL (RHOTRP,HL3I,NRHS)
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
         CALL CDCUBVAL (RHOTRP,HL1R,HL1I,NRHS,RHO1,CDS1)
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
       CALL CDCUBVAL (RHOTRP,HL2R,HL2I,NRHS,RHO1,CDS1)
       CALL CDCUBVAL (RHOTRP,HL2R,HL2I,NRHS,RHO2,CDS2)
       CPHID = REAL (CPHI(NVRL),KIND=QL)
       SPHID = REAL (SPHI(NVRL),KIND=QL)

       EX1 = X2D * CDS2 - X1D * CDS1    ! Fields in rotated system
       EY1 = YD * (CDS2 - CDS1)
       EX2 = EX1 * CPHID - EY1 * SPHID                    ! Fields in survey system
       EY2 = EY1 * CPHID + EX1 * SPHID

       CALL CDCUBVAL (RHOTRP,HL3R,HL3I,NRHS,RHO1,CDS1)
       CALL CDCUBVAL (RHOTRP,HL3R,HL3I,NRHS,RHO2,CDS2)
       EZ2 = CDS2 - CDS1

       BFD(JF,JR,JS,1) = BFD(JF,JR,JS,1) + CMPLX (EX2)  ! add charge & induction contributions
       BFD(JF,JR,JS,2) = BFD(JF,JR,JS,2) + CMPLX (EY2)
       BFD(JF,JR,JS,3) = BFD(JF,JR,JS,3) + CMPLX (EZ2)
     END DO
   END IF
 END DO     !  Next frequency

 If(WRITE_FRQ) Then
    Open(Unit = 66, File = 'Leroi.flayer', Status = 'Unknown')
    Do jf = 1, nfrq
        Write (66, 1) freq(jf), bfd(jf, 1, 1, 1), bfd(jf, 1, 1, 2), bfd(jf, 1, 1, 3)
    End Do
    Close(66)
 End If

1   Format (en13.4, 6(2x, en15.6))

 END SUBROUTINE HSLPE

 SUBROUTINE HSLPE_HNK (NRHS,RHOTRP,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,KFG,SXLYR,RXLYR,HLYR)
!--------------------------------------------------------------------------------------------

!***  Calls HS_JMP, HSLPE_KER
!***  Called by HSLPE

!  Uses flow through Hankel transform to compute electric dipole response integrals
!  HLYR(NRHS,1:3) used to compute the closed and openloop frequency-domain electric
!  field components at the RX from pseudo X oriented  HED source.  The Hankel transform
!  uses a 15 points per decade filter coefficient set derived from Christensen's
!  FLTGEN program.

!          Input
!          -----
!    NRHS - number of logrithmically horizontal distances
!  RHOTRP - array of NRHS logrithmically spaced (15 / decade) horizontal distances
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

!    OUTPUT is HLYR(1:NRHS,1:3)

 USE LG_Filter_Coefficients

 IMPLICIT NONE
 REAL(KIND=QL), PARAMETER :: FOURPI = 12.56637061_QL
 INTEGER KFG,GAM,NRHS,NLYR,RXLYR,SXLYR,K,L,JR,K1,KBOT,KMAX,KMIN,LMAX,LMIN
 REAL RHOTRP(NRHS)
 REAL(KIND=QL) ZS,ZR,DELTA,Y,Y1,LMBDA,RMUD(0:NLYR),RHOD
 REAL(KIND=QL), DIMENSION (NLYR) :: THKD,DPTHL
 COMPLEX HLYR(NRHS,3)
 COMPLEX(KIND=QL) HLYRD(NRHS,3),KER(JNLO-NRHS:JNHI,3)
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
   CALL HSLPE_KER (NRHS,K,JR,L,LMBDA,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,KFG,GAM,SXLYR,RXLYR,KER,HLYRD,JUMP)
   IF (JUMP .AND. L > -40) EXIT
 END DO

 JUMP = .FALSE.           ! Finish off the low end for RHOTRP(1)
 DO L = -51, JNLO, -1
   LMIN = L                   ! Maximum filter index used                            r4cs
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
   K = L + 1 - JR             ! Compute the kernel index.
   CALL HSLPE_KER (NRHS,K,JR,L,LMBDA,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,KFG,GAM,SXLYR,RXLYR,KER,HLYRD,JUMP)
   IF (JUMP .AND. L < -60) EXIT
 END DO

 KMIN = LMIN + 1 - JR  !  Define the range of kernel values
 KMAX = LMAX + 1 - JR  !  used for RHOTRP(1)

! Complete definition of kernel values by evaluating transform of
! maximum RHO = RHOTRP (NRHS)

 JR = NRHS
 RHOD = REAL (RHOTRP(NRHS),KIND=QL)
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
     CALL HSLPE_KER (NRHS,K,JR,L,LMBDA,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,KFG,GAM,SXLYR,RXLYR,KER,HLYRD,JUMP)
     IF (JUMP) EXIT
   END DO
 END IF

 DO JR = 2, NRHS-1          !  Compute transforms for all other RHO values
   DO K = KMIN, KMAX         !  using previously computed kernel values.
     L = K - 1 + JR
     HLYRD(JR,1) = HLYRD(JR,1) + KER(K,1) * WJ0(L)
     HLYRD(JR,2) = HLYRD(JR,2) + KER(K,2) * WJ1(L)
     HLYRD(JR,3) = HLYRD(JR,3) + KER(K,3) * WJ0(L)
   END DO
 END DO

 DO JR = 1,NRHS
   RHOD = REAL (RHOTRP(JR),KIND=QL)
   HLYRD(JR,1:3) =  HLYRD(JR,1:3) / (FOURPI * SIGL(RXLYR) * RHOD)
   HLYRD(JR,2) = HLYRD(JR,2) / RHOD
 END DO
 HLYR = CMPLX (HLYRD)

 END SUBROUTINE HSLPE_HNK

 SUBROUTINE HSLPE_KER (NRHS,K,JR,L,LMBDA,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                       KFG,GAM,SXLYR,RXLYR,KER,HLYRD,JUMP)
!----------------------------------------------------------------------------

!***  Called by HSLPE__HNK
!***  Calls EDSX_COEF

!  Kernels and magnetic dipole response integrals for response from a closed or grounded
!  loop transmitter and a surface or downhole horizontal electric dipole receiver.
!
!          Input
!          -----
!    NRHS - number of logrithmically horizontal distances
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
!    KER(NRHS,3) - kernel values for transform
!            HLYRD - dipole response integral
!             JUMP - logical convergence indicator

 USE LG_Filter_Coefficients

 IMPLICIT NONE
 REAL(KIND=QL), PARAMETER :: TOL=1.D-6, TOL2=1.D-35
 INTEGER NRHS,K,JR,L,NLYR,KFG,GAM,RXLYR,SXLYR,J1
 REAL(KIND=QL) LMBDA,ZR,ZS,RMUD(0:NLYR),QR,QI
 REAL(KIND=QL), DIMENSION (NLYR) :: THKD,DPTHL
 COMPLEX(KIND=QL) S(0:NLYR),SL,XI_V,F_V,F_H,ETA_V,G_V,G_H,XP,XPDIR,FACV,KV,KS,FW(3), &
                  KER(JNLO-NRHS:JNHI,3),HLYRD(NRHS,3)
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL
 LOGICAL JUMP

 CALL EDSX_COEF (SXLYR,RXLYR,KFG,LMBDA,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
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
                    YRXTX,ZRXTX,NRHS,RHOTRP,NLYR,THKD,DPTHL,RES,RMUD,REPS,CHRG,CTAU,CFREQ,BFD)
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
!       RHOTRP - NRHS size interpolation array for horizontal distances
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
 INTEGER NTX,MRXTX,NRXTX(NTX),MQVR,RXID(MRXTX,NTX),SXLYR,RXLYR,NFRQ,NRHS,SOURCE_TYPE,NVRL,NVRLS,NLYR, &
         J1,JF,JS,JR,JG,JV,JV1,JD,NDIP(NVRL),KFG,I1
 REAL FREQ(NFRQ),FRQ,ZRXTX(MRXTX,NTX),RXVRT,RHOTRP(NRHS),SXNL(NVRL),SXEL(NVRL),X1,CSRX(MRXTX),SNRX(MRXTX), &
      WRXD(5,MRXTX),DELR,DELX,DELY,XWRX(NVRL,2,5,MRXTX),YWRX(NVRL,5,MRXTX)
 REAL, DIMENSION(MRXTX,NTX,MQVR) :: XRXTX,YRXTX
 REAL, DIMENSION(NLYR) :: RES,REPS,CTAU,CFREQ,CHRG
 REAL, DIMENSION(NVRL) :: CPHI,SPHI,WYRL,DIPL
 REAL, DIMENSION(MXDIP,NVRL,5,MRXTX) :: XRXD,RXRHO
 REAL, DIMENSION(4,NRHS) :: HL1R,HL2R,HL1I,HL2I
 REAL, DIMENSION(5,MRXTX) :: XRXED,YRXED
 REAL(KIND=QL) ZR,ZPRV,ZS,RMUD(0:NLYR),THKD(NLYR),DPTHL(NLYR),YD,X1D,X2D,RHO1,RHO2,DIPLD,CPHID,SPHID
 COMPLEX HLYR(NRHS,3),BFD(NFRQ,MRXTX,NTX,3)
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

       CALL HSLPE_HNK (NRHS,RHOTRP,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,KFG,SXLYR,RXLYR,HLYR)

       HL1R(1,1:NRHS) = REAL (HLYR(1:NRHS,1))
       CALL CUBSPL (RHOTRP,HL1R,NRHS)
       HL1I(1,1:NRHS) = AIMAG (HLYR(1:NRHS,1))
       CALL CUBSPL (RHOTRP,HL1I,NRHS)

       IF (SOURCE_TYPE == 2) THEN                      ! Include conservative terms for open loop source.
         HL2R(1,1:NRHS) = REAL (HLYR(1:NRHS,2))
         CALL CUBSPL (RHOTRP,HL2R,NRHS)
         HL2I(1,1:NRHS) = AIMAG (HLYR(1:NRHS,2))
         CALL CUBSPL (RHOTRP,HL2I,NRHS)
       END IF

!      HL3R(1,1:NRHS) = REAL (HLYR(1:NRHS,3))
!      CALL CUBSPL (RHOTRP,HL3R,NRHS)
!      HL3I(1,1:NRHS) = AIMAG (HLYR(1:NRHS,3))
!      CALL CUBSPL (RHOTRP,HL3I,NRHS)
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
           CALL CDCUBVAL (RHOTRP,HL1R,HL1I,NRHS,RHO1,CDS1)
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
         CALL CDCUBVAL (RHOTRP,HL2R,HL2I,NRHS,RHO1,CDS1)
         CALL CDCUBVAL (RHOTRP,HL2R,HL2I,NRHS,RHO2,CDS2)
         CPHID = REAL (CPHI(NVRL),KIND=QL)
         SPHID = REAL (SPHI(NVRL),KIND=QL)

         EX1 = X2D * CDS2 - X1D * CDS1    ! Fields in rotated system
         EY1 = YD * (CDS2 - CDS1)
         EX2 = EX1 * CPHID - EY1 * SPHID                    ! Fields in survey system
         EY2 = EY1 * CPHID + EX1 * SPHID

         EXL = EXL + WRXD(JG,JR) * EX2                      ! Simpson integration
         EYL = EYL + WRXD(JG,JR) * EY2

!        CALL CDCUBVAL (RHOTRP,HL3R,HL3I,NRHS,RHO1,CDS1)
!        CALL CDCUBVAL (RHOTRP,HL3R,HL3I,NRHS,RHO2,CDS2)
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
 Logical :: WRITE_FRQ = .True.

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


If(WRITE_FRQ) Then
	Open(Unit = 66, File = 'Leroi.flayer', Status = 'Unknown')
	Do jf = 1, nfrq
	    Write (66, 1) freq(jf), bfd(jf, 1, 1, 1), bfd(jf, 1, 1, 2), bfd(jf, 1, 1, 3)
	End Do
	Close(66)
End If

1   Format (en13.4, 6(2x, en15.6))

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
     ! BZZD = FAC * (THREE*ZRD**2 - ONE)
     BZZD = FAC * (THREE*ZRD**2)
     BXZD = THREE * FAC * XRD * ZRD
     BYZD = THREE * FAC * YRD * ZRD
     ! BXXD = FAC * (THREE*XRD**2 - one)
     BXXD = FAC * (THREE*XRD**2)
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

 USE LG_Filter_Coefficients

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
   ! DWA 20180206: all the way ...
   ! IF (JUMP .AND. L > -40) EXIT
 END DO

 JUMP = .FALSE.           ! Finish off the low end for RHOTRP(1)
 DO L = -51, JNLO, -1
   Y = RHO_JN + DBLE (L) * DEL_JN
   LMBDA = EXP (Y)
   CALL HSMDB_KER (L,LMBDA,NLYR,THKD,DPTHL,SIGL,KSQL,RMUD,KFG,RXLYR,SXLYR,ZS,ZR,RHOD,NINTG,HLYR,JUMP)
   ! DWA 20180206: all the way ...
   ! IF (JUMP .AND. L < -60) EXIT
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

 USE LG_Filter_Coefficients
 IMPLICIT NONE
 REAL(KIND=QL), PARAMETER :: TOL=1.D-6, TOL2=1.D-35
 COMPLEX(KIND=QL), PARAMETER :: ZERO=(0._QL,0._QL)
 INTEGER L,NLYR,KFG,RXLYR,SXLYR,J,NINTG
 REAL(KIND=QL) RMUD(0:NLYR),LMBDA,LMBDA2,LMBDA3,ZS,ZR,RHOD,QR,QI,RM,RL
 REAL(KIND=QL), DIMENSION (NLYR) :: THKD,DPTHL
 COMPLEX(KIND=QL) XI_V,XI_H,F_V,ETA_V,ETA_H,G_V,QFD(5),XP1,SL,SM,KSQL(NLYR), &
                  SIGL(NLYR),S(0:NLYR),FW(NINTG),HLYR(NINTG)
 LOGICAL JUMP

 CALL MDSX_COEF (SXLYR,RXLYR,KFG,LMBDA,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
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

 SUBROUTINE EDSX_COEF (SXLYR,RXLYR,KFG,LMBDA,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
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

 SUBROUTINE MDSX_COEF (SXLYR,RXLYR,KFG,LMBDA,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
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

!==========================================================================================
!*****************************
!       BEGIN 3D CODE
!       -------------
!*****************************

 SUBROUTINE LEROI_3D (IPR,NFRQ,FREQ,SOURCE_TYPE,NTX,MXVRTX,NVRTX,SXN,SXE,SXZ,SXDIP,SXAZM,NRXTX, &
                      MRXTX,RXID,MQVR,MXRS,XRXTX,YRXTX,ZRXTX,NLYR,THKD,RES,RMUD,REPS,CHRG,CTAU, &
                      CFREQ,NPLT,MXAB,CELLW,PLNGTH,PLWDTH,XCNTR,YCNTR,PLTOP,PLAZM,PLDIP,PLUNJ,  &
                      INTRUDE,SIG_T,CHRGP,CTAUP,CFREQP,MXRHO,RHOTRP,INVERT,BFD_SCAT)
!----------------------------------------------------------------------------------------------

!***  Called by MAIN
!***  Calls SET_MGT, SET_MAX_INTRP

! Main routine for 3D Leroi computation for the scattered field.
! For magnetic dipole and loop receivers, BFD_SCAT in Teslas per unit amp.
!
! For magnetic dipole receivers, BFD_SCAT(JF,JR,JS,1:3) contains the
! 1: north, 2: east & 3:vertical components for frequency JF, transmitter JS, receiver JR.
!
! The response for electric dipoles and loop receivers is contained in BFD_SCAT(JF,JR,JS,1),
! BFD_SCAT(JF,JR,JS,2:3) is set to zero.
!
!   NFRQ         - number of frequencies
!   FREQ         - array of frequencies
!   SOURCE_TYPE  = 1 => general loop
!                = 2 => grounded wire
!                = 3 => magnetic dipole
!                = 4 => coincident loop
!   NTX          - number of transmitter positions
!   MXVRTX       - maximum number of vertices for any transmitter
!   NVRTX(J)     - number of vertices for transmitter J
!   SXE(K,J)     = local east coordinate of vertex K for loop position J
!   SXN(K,J)     = local coordinate of vertex K for loop position J
!   SXZ(J)       = depth of Tx J
!   SXDIP(J)     = dip (in radians) of dipole J (eg; vertical = 0, horizontal = 90)
!   SXAZM(J)     = azimuth (in radians) of dipole J (north = 0, east = 90)
!   NRXTX(J)     - number of receivers for transmitter J
!   MRXTX        - maximum number of receivers per transmitter
!   RXID(I,J)    - RX_TYPE of receiver I for transmitter J. 1 => MD; 2 => ED; 4=> cdnt loop.
!   MQVR         - maximum number of vertices for all receivers (= 1 if all sources are magnetic dipoles)
!   XRXTX(I,J,K) - north coordinate of the Kth vertex of the Ith receiver of transmitter J
!   YRXTX(I,J,K) - east coordinate of the Kth vertex of the Ith receiver of transmitter J
!   ZRXTX(I,J)   - depth of the Ith receiver of transmitter J
!                - K = 1 only for mag dipole Rx; 1 to 2 for electric dipole Rx; 1 to 4 for loop Rx
!   NLYR         - number of layers
!   RES          - layer conductivities
!   RMUD         - mu(i) / mu(0)
!   REPS         - array of relative dislectric constants
!   THKD         - thicknesses of NLYR -1 layers above basement
!   CHRG         - C-C chargeability
!   CTAU         - array of layer relaxation times (sec).
!   CFREQ        - array of layer frequency parameters.
!   RHOTRP       - horizontal distance interpolation array (15 pts / decade) of dimension MXRHO
!
!           Parameters for NPLT plates
!           --------------------------
!
!   MXAB   - Number of cells in biggest plate
!   CELLW  - nominal cell dimension
!   PLNGTH - strike length for each plate
!   PLWDTH - dip width for each plate
!   XCNTR  - north coordinates of plate reference & pivot point
!   YCNTR  - east coordinates of plate reference & pivot point
!   PLTOP  - depth to top from surface
!   PLAZM  - strike angle (radians) = dip azimuth - pi/2
!   PLDIP  - dip angle
!   PLUNJ  - plunge rotation
!   SIG_T  - conductivity thickness product
!   CHRGP  - Cole Cole chargeability
!   CTAUP  - Cole Cole time constant
!   CFREQP - Cole Cole frequency constant
!

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18), NMG1=3, NQS=1000
 COMPLEX, PARAMETER :: ZERO=(0.,0.)
 INTEGER NLYR,NPLT,NTPL,MXAB,MXB,NCELL2(0:NPLT),ICCL(NLYR),IPR,NFRQ,SOURCE_TYPE,NTX, &
         MXVRTX,NVRTX(NTX),NRXTX(NTX),MRXTX,NRS(MRXTX,NTX),MXRS,MQVR,MXRHO,RXID(MRXTX,NTX), &
         MXCL2,NMGP,NRPRM,NREGT,NRMGT,NPPD,ACC,NZ1,NZ2,NZ3,IFIN,J1,J2,JF,JS,JR,JP,JB,JL
 INTEGER, DIMENSION(NPLT) :: NA,NB,ICCP,PLYR,IDPL
 REAL FRQ,FREQ(NFRQ),CELLW,DPTHB,RHOTRP(MXRHO),ZMIN,ZMAX,ZMIN2,ZMAX2,SKN,SPAN,DELZ, &
      QSTORE(NQS),DEL
 REAL, DIMENSION(NLYR) :: RES,REPS,CTAU,CFREQ,CHRG
 REAL, DIMENSION(NTX) :: SXDIP,SXAZM,SXZ
 REAL, DIMENSION(NPLT) :: SIG_T,CHRGP,CTAUP,CFREQP,XCNTR,YCNTR,PLTOP,PLWDTH, &
                          PLNGTH,PLAZM,PLDIP,PLUNJ,DA,DB,WMG
 REAL, DIMENSION(MXVRTX,NTX) :: SXN,SXE
 REAL, DIMENSION(MRXTX,NTX) :: EDCS,EDSN,ZRXTX
 REAL, DIMENSION(MRXTX,NTX,MQVR) :: XRXTX,YRXTX
 REAL, DIMENSION(MXRS,MRXTX,NTX) :: XRS,YRS,WTRS    ! Rx subnet
 COMPLEX BFD_SCAT(NFRQ,MRXTX,NTX,3)
 COMPLEX, DIMENSION(NPLT) :: KSQT, SIGT

 REAL(KIND=QL) RMUD(0:NLYR)
 REAL(KIND=QL), DIMENSION(NLYR) :: THKD,DPTHL
 COMPLEX(KIND=QL), DIMENSION(NLYR) :: KSQL, SIGL
 LOGICAL MDRX,EDRX,QUIT,DCMP_FAIL,INVERT,INTRUDE

 REAL, ALLOCATABLE, DIMENSION(:) :: ZV1,ZV2,ZC1L
 REAL, ALLOCATABLE, DIMENSION(:,:) :: XCELL,YCELL,ZCELL,ZC1,ZC3,XMG,YMG,ZMG
 COMPLEX, ALLOCATABLE, DIMENSION(:,:,:,:) :: J_SCAT,E_PRYM
 Integer (Kind = 8) :: tvals(8)
 Real :: fprog

! Depth interpolation arrays for basement plates:
! -----------------------------------------------
!  ZV1(NZ1) will be used for MGT & EGTRX for 2D interpolation relating basement sources to receivers
!  These are distances from the surface.
!
!  ZV2(NZ2) is used for EGT_BOSS for interpolating z + z - 2 * DPTHB
!
! Depth interpolation arrays for plates in layers above basement:
! --------------------------------------------------------------
! ZC1 (MXB,NTPL) will be used for PRM_BOSS.  It contains the cell row centres of plates above basement

! Construct plate representation

 DPTHL = 0._QL
 DO JL = 2,NLYR
   DPTHL(JL) = DPTHL(JL-1) + THKD(JL-1)     ! Depth to top of layer
 END DO
 DPTHB = REAL (DPTHL(NLYR))                 ! Depth to basement

 NTPL = 0
 IDPL = 0
 MXB = 1
 PLYR = NLYR
 IF (INTRUDE) THEN
   DO JP = 1,NPLT
     DO JL = 1,NLYR
       IF (PLTOP(JP) + 0.01 > REAL (DPTHL(JL))) PLYR(JP) = JL      ! Find layer containing Plate JP
     END DO
     DEL = PLTOP(JP) - REAL (DPTHL(PLYR(JP)))
     IF (DEL < 0.01) PLTOP(JP) = REAL (DPTHL(PLYR(JP))) + 0.01
     IF (PLYR(JP) < NLYR) THEN
       NTPL = NTPL + 1           !  mumber of plates above basement
       IDPL(NTPL) = JP           !  ID of each plate above basement
     END IF
   END DO
 END IF

! Adjust plates and set cell dimensions
 CALL SET_CELLS_1 (IPR,DPTHB,NLYR,NPLT,PLYR,CELLW,PLNGTH,PLWDTH,PLTOP,PLAZM, &
                   PLDIP,PLUNJ,NA,NB,DA,DB,MXCL2,MXAB,NCELL2,ZMIN,ZMAX, INTRUDE) ! DWA 2024-06-12

 DO J1 = 1,NTPL
   JP = IDPL(J1)
   MXB = MAX (MXB,NB(JP))
 END DO
 NZ3 = 3 * MXB

 ALLOCATE (XCELL(MXAB,NPLT),YCELL(MXAB,NPLT),ZCELL(MXAB,NPLT),ZC1(MXB,NTPL),ZC3(NZ3,NTPL), &
           ZC1L(MXB),J_SCAT(2,MXAB,NTX,NPLT),E_PRYM(2,MXAB,NTX,NPLT))

! Set cell locations
 CALL SET_CELLS_2 (IPR,NPLT,MXAB,XCNTR,YCNTR,PLTOP,PLAZM,PLDIP, &
                   PLUNJ,NA,NB,DA,DB,XCELL,YCELL,ZCELL)
 ZC1 = 0.
 ZC3 = 0.
 DO J1 = 1,NTPL
   JP = IDPL(J1)
   DELZ = SIN (PLDIP(JP)) * DB(JP) / 3.
   DO JB = 1,NB(JP)
     J2 = (JB-1) * NA(JP) + 1   ! index of first cell of row JB of Plate JP for plates above basement
     ZC1(JB,JP) = ZCELL(J2,JP)  ! depth of centre of cell row JB of Plate JP for plates above basement
     ZC3(3*JB-1,JP) = ZC1(JB,JP)
     ZC3(3*JB-2,JP) = ZC1(JB,JP) - DELZ
     ZC3(3*JB,JP)   = ZC1(JB,JP) + DELZ
   END DO
 END DO


 EDRX = .FALSE.       ! Compute electric field on surface?
 MDRX = .FALSE.       ! Compute magnetic field on surface?

! Set up receiver subnets for loop and electric dipole receivers

 IF (SOURCE_TYPE == 4) THEN
   MDRX = .TRUE.

 ELSE
   DO JS = 1,NTX
     DO JR = 1,NRXTX(JS)
       IF (RXID(JR,JS) == 1) MDRX = .TRUE.
       IF (RXID(JR,JS) == 2 .OR. RXID(JR,JS) == 3) EDRX = .TRUE.
     END DO
   END DO
 END IF

 XRS = 0;  YRS = 0;   WTRS = 1;  EDCS = 0.;  EDSN = 0.

 IF (SOURCE_TYPE == 4) THEN
   CALL SET_RX_SUBNET_CL (NTX,SXN,SXE,MXRS,NRS,XRS,YRS,WTRS)
 ELSE
   CALL SET_RX_SUBNET (NTX,MRXTX,NRXTX,MXRS,NRS,MQVR,RXID,XRXTX,YRXTX,XRS,YRS,WTRS,EDCS,EDSN)
 END IF

! Set up horizontal distance descriptors and arrays for
! primary field, & electric & magnetic Green's functions.

! Set up uniform integration for magnetic field computation

 NMGP = NMG1**2
 ALLOCATE (XMG(NMGP,NPLT),YMG(NMGP,NPLT),ZMG(NMGP,NPLT))

 CALL SET_MGT (NPLT,NMG1,NMGP,DA,DB,PLAZM,PLDIP,PLUNJ,XMG,YMG,ZMG,WMG)

 CALL SET_MAX_INTRP (MXRHO,RHOTRP,MXAB,NPLT,NA,NB,DA,PLNGTH,XCELL,YCELL,NTX,MXVRTX, &
                     NVRTX,SXN,SXE,MRXTX,NRXTX,MXRS,NRS,XRS,YRS,NRMGT,NRPRM,NREGT)

!  Set depth interpolation

 ZMIN2 = 2.* (ZMIN - DPTHB)
 ZMAX2 = 2.* (ZMAX - DPTHB)
 ZMIN2 = MAX (ZMIN2, 0.02)

 NPPD = 15
 ACC = 1

 BFD_SCAT = ZERO

 IF (.NOT. INVERT) WRITE(*,1) NFRQ
 ICCL = 0; ICCP = 0
 DO J1 = 1,NPLT
  IF (CTAUP(J1) > 1.E-8) ICCP(J1) = 1
 END DO
 DO J1 = 1,NLYR
  IF (CTAU(J1) > 1.E-8) ICCL(J1) = 1
 END DO

 QUIT = .FALSE.
 FREQUENCY_LOOP: DO JF = 1,NFRQ
   IFIN = INT (100. * (JF-1) / REAL (NFRQ) )
   fprog = 100. * (JF-1) / REAL (NFRQ)
   FRQ = FREQ(JF)
   Call Date_and_time(Values = tvals)
   ! IF (.NOT. INVERT) WRITE(*,2) JF, FRQ, NFRQ, 100. * (JF-1) / REAL (NFRQ)
   If (.not. INVERT) Write (*, 2) tvals(1:3), tvals(5:7), JF, NFRQ, FRQ, fprog

! Set up complex conductivities.  Then compute primary fields, Green's tensors,
! scattering matrices and scattering currents for each plate individually.

   CALL COLRES_3D (FRQ,NLYR,RMUD,RES,REPS,ICCL,CHRG,CTAU,CFREQ,NPLT, &
                   SIG_T,CHRGP,CTAUP,CFREQP,ICCP,SIGL,KSQL,SIGT,KSQT)

   SKN = 1.0 / REAL (SQRT(KSQL(NLYR)))
   SPAN = MIN (SKN,10.)
   SPAN = MAX (SPAN,5.)
   DELZ = 0.2 * SKN

!  Set basement interpolation array for MGT & PRM routines.
!  ZV1 measures depth extending from DEPTHB downwards

   CALL SET_Z (ACC,NPPD,ZMIN,ZMAX,SPAN,DELZ,NQS,QSTORE,NZ1)
   ALLOCATE (ZV1(NZ1))
   ZV1(1:NZ1) = QSTORE(1:NZ1)
   IF (ZV1(1) < DPTHB) THEN
     ZV1(2) = (ZMIN + DPTHB) / 2.
     ZV1(1) = (ZV1(2) + DPTHB) / 2.
   END IF

! Set basement interpolation array for EGT routines
! ZV2 measure reflection distance in basement from DEPTHB

   CALL SET_Z (ACC,NPPD,ZMIN2,ZMAX2,SPAN,DELZ,NQS,QSTORE,NZ2)
   ALLOCATE (ZV2(NZ2))
   ZV2(1:NZ2) = QSTORE(1:NZ2)

!  Compute the layered earth electric fields on the target, E_PRYM.

   E_PRYM = ZERO
   IF (SOURCE_TYPE == 3) THEN   ! Magnetic dipole
     IF (MAXVAL (PLYR) == NLYR) &    ! basement plates
       CALL PRM_BOSS_MD (FRQ,NTX,SXN,SXE,SXZ,SXDIP,SXAZM,NRPRM,RHOTRP,NLYR,THKD,DPTHL, &
                         RMUD,SIGL,KSQL,NPLT,PLYR,PLAZM,PLDIP,PLUNJ,MXAB,NB,NA,XCELL,  &
                         YCELL,ZCELL,NZ1,ZV1,E_PRYM)

     DO J1 = 1, NTPL          ! plates above basement
       JP = IDPL(J1)
       ZC1L(1:MXB) = ZC1(1:MXB,JP)
       CALL PRM_BOSS_UL_MD (JP,FRQ,NTX,SXN,SXE,SXZ,SXDIP,SXAZM,NRPRM,RHOTRP,NLYR,THKD, &
                            DPTHL,RMUD,SIGL,KSQL,NPLT,PLYR,PLAZM,PLDIP,PLUNJ,MXAB,MXB, &
                            NB,NA,XCELL,YCELL,ZC1L,E_PRYM)
     END DO
   ELSE                          ! Open or closed loop
     IF (MAXVAL (PLYR) == NLYR) &    ! basement plates
       CALL PRM_BOSS_LP (SOURCE_TYPE,NTX,MXVRTX,NVRTX,SXN,SXE,SXZ,NRPRM,RHOTRP,NLYR,  &
                         THKD,DPTHL,RMUD,SIGL,KSQL,NPLT,PLYR,PLAZM,PLDIP,PLUNJ,MXAB, &
                         NB,NA,XCELL,YCELL,ZCELL,NZ1,ZV1,E_PRYM)

     DO J1 = 1, NTPL          ! plates above basement
       JP = IDPL(J1)
       ZC1L(1:MXB) = ZC1(1:MXB,JP)
       CALL PRM_BOSS_UL_LP (JP,SOURCE_TYPE,NTX,MXVRTX,NVRTX,SXN,SXE,SXZ,NRPRM,RHOTRP,NLYR, &
                            THKD,DPTHL,RMUD,SIGL,KSQL,NPLT,PLYR,PLAZM,PLDIP,PLUNJ,MXAB,    &
                            MXB,NB,NA,XCELL,YCELL,ZC1L,E_PRYM)
     END DO
   END IF

!  Set up scattering matrix SCAT_MTRX as an LU decomposition.

   CALL SCAT_MTRX_BOSS (NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,NPLT,NTPL,IDPL,PLYR,MXB,MXAB,NCELL2, &
                        MXCL2,NA,NB,DA,DB,SIGT,KSQT,PLTOP,PLAZM,PLDIP,PLUNJ,XCELL,YCELL,    &
                        ZCELL,NZ2,ZV2,ZC1,NREGT,RHOTRP,NTX,E_PRYM,J_SCAT,DCMP_FAIL)


   IF (DCMP_FAIL) THEN
     WRITE(*,3)
     STOP
   END IF

!  Compute BFD_SCAT, the scattered frequency-domain magnetic fields.

   CALL SCAT_EM (JF,NFRQ,NPLT,PLYR,NTPL,IDPL,MXAB,MXB,NA,NB,DA,DB,PLAZM,PLDIP,PLUNJ,XCELL,  &
                 YCELL,ZCELL,NMGP,XMG,YMG,ZMG,WMG,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,NTX,MRXTX, &
                 NRXTX,RXID,MXRS,NRS,XRS,YRS,ZRXTX,WTRS,EDCS,EDSN,NRMGT,RHOTRP,NZ1,ZV1,NZ3, &
                 ZC3,MDRX,EDRX,J_SCAT,BFD_SCAT,QUIT)

   ! IF (QUIT) EXIT    !  If scattered fields are negligible, end frequency stepping

 DEALLOCATE (ZV1,ZV2)
 END DO FREQUENCY_LOOP
 DEALLOCATE (XMG,YMG,ZMG,XCELL,YCELL,ZCELL,J_SCAT,E_PRYM)

 1 FORMAT(/T3,'A maximum of ',I3,' 3D frequency-domain responses will be computed ....'/)
 ! 2 FORMAT(T3,'Frequency ', I3,'  = ',en10.2, 'Hz; ', f8.2,'% done')
 2 FORMAT(T3, i4.4, '-', i2.2, '-', i2.2, 'T', i2.2, ':', i2.2, ':', i2.2, &
                   ': Frequency ',i4, ' of ', i4, '  =',1x, en10.2,' Hz; ', f5.2,' % complete')
 3 FORMAT (//T3,'An evil spirit has entered SCAT_MTRX_LU_DCMP causing the matrix to be singular.', &
            /T3,'The model leading to this crash is Described in Leroi.out.' &
            /T3,'COMPUTATION HALTED.  SEEK HELP.  (art.raiche@optusnet.com.au)')

 END SUBROUTINE LEROI_3D

 SUBROUTINE COLRES_3D (FRQ,NLYR,RMUD,RES,REPS,ICCL,CHRG,CTAU,CFREQ,NPLT, &
                   SIG_T,CHRGP,CTAUP,CFREQP,ICCP,SIGL,KSQL,SIGT,KSQT)
!------------------------------------------------------------------------

!  Computes SIGL, the complex conductivities of layers and SIGT, the complex
!  conductances of plates, at frequency FRQ using the layered earth Cole-Cole
!  parameters, CHRG, CTAU, CFREQ, the plate Cole-Cole parameters: CHRGP,
!  CTAUP, CFREQP,the NLYR real layer resistivities, RES, and the real conductance,
!  SIG_T, SIGL and SIGT include displacement currents.

!  KSQL = iwu * SIGL = the layered earth propagation constants.
!  KSQT = iwu * SIGT = the propagation constants for plates.

!***  Called by MAIN

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 REAL, PARAMETER :: TWOPI=6.2831853, MU0=12.56637E-7, EPS0=8.854156E-12
 COMPLEX, PARAMETER :: ONE=(1.,0.)
 INTEGER J,NLYR,NPLT,ICCL(NLYR),ICCP(NPLT)
 REAL, DIMENSION(NLYR) :: RES,REPS,CHRG,CALF,CTAU,CFREQ
 REAL FRQ
 REAL, DIMENSION(NPLT) :: SIG_T,CHRGP,CALFP,CTAUP,CFREQP
 REAL(KIND=QL) RMUD(0:NLYR)
 COMPLEX A1,IW,P
 COMPLEX(KIND=QL), DIMENSION(NLYR) :: KSQL, SIGL
 COMPLEX, DIMENSION(NPLT) :: KSQT, SIGT

 CALF = 1. - CHRG
 CALFP = 1. - CHRGP
 IW = TWOPI * CMPLX (0.,FRQ)
 DO J = 1,NLYR                ! Layers
   A1 = CMPLX (1./RES(J), 0.)
   P = ICCL(J) * (IW * CTAU(J) )**CFREQ(J)
   A1 = A1 * (ONE + P) / (ONE + CALF(J)*P)
   A1 = A1 + IW * EPS0 * REPS(J)  !  Add in displacement term
   SIGL(J) = CMPLX (A1,KIND=QL)
   A1 = IW * MU0 * A1
   KSQL(J) = RMUD(J) * CMPLX (A1,KIND=QL)
 END DO

 DO J = 1,NPLT                ! Plates
   SIGT(J) = CMPLX (SIG_T(J), 0.)
   P = ICCP(J) * (IW * CTAUP(J) )**CFREQP(J)
   SIGT(J) = SIGT(J) * (ONE + P) / (ONE + CALFP(J)*P)
   KSQT(J) = IW * MU0 * REAL (RMUD(NLYR)) * SIGT(J)
 END DO

 END SUBROUTINE COLRES_3D

!======================================================================================
!======================================================================================
!
!        SINGLE PLATE GREENS FUNCTION ROUTINES
!        -------------------------------------


 SUBROUTINE EGT_BOSS (NAL,NBL,NAB,DAL,DBL,KSQN,DPTHB,XCEL1,YCEL1,ZCEL1,CDP,SDP,CPL, &
                      SPL,NZ2,ZV2,NREGT,RHOTRP,GR1Z,GI1Z,GR2Z,GI2Z,GR3Z,GI3Z,GR4Z,  &
                      GI4Z,GR5Z,GI5Z,GR6Z,GI6Z,SAA,SBA,SBB,VAA,VAB,VBA,VBB)
! ---------------------------------------------------------------------------------

!***  Calls EGTDIR
!***  Called by SCAT_MTRX_BOSS

!         INPUT
!         -----
!
!     NBL,NAL - number of cell rows and columns respectively
!     DBL,DAL - cell dimensions down dip and along strike respectively
!        KSQN - basement propagation constant
!       DPTHB - depth to basement
!    CDP, SDP - dip cosine and sine
!    CPL, SPL - rotation cosine and sine
!    XCEL1,YCEL1,ZCEL1 - X,Y,Z coordinates of cells on plate
!    ZV2 - array of NZ2 depths used for vertical interpolation
!    RHOTRP - array of NREGT logrithmically spaced points used for horizontalal interpolation
!    GR1Z to GR6Z - 2D grid of real component of 6 Green's tensor components integrals
!    GI1Z to GI6Z - 2D grid of imaginary component of 6 Green's tensor components integrals
!
!         OUPUT
!         ------
!
!    SAA, SBB, SBA - divergence free integrated Green's tensor elements
!    VAA, VBB, VAB, VBA - curl free integrated Green's tensor elements
!                         A => along strike
!                         B => down dip
!
!  EGT_BOSS computes the integrals over the "source" cell of the Green's tensor
!  elements ( GTE ) for single plates contained in a uniform half-space or
!  entirely inthe bottom layer of a two layer half-space.  The direct portion is
!  computed separately from the reflected/transmitted scattered terms.

!  In what follows, the GTE have the units of electric field divided by a
!  factor (-iwu) because this factor is explicitly included as a multiplier
!  outside the integral over area when solving the integral equation.
!
!  In the formulation the integrated Green's tensor G relates the field at r'
!  to the current at r by
!                                           _                      _
!                                          |  Gaa(r.r')  Gab(r.r')  |
!  { Ea(r'), Eb(r') } =  { Ja(r), Jb(r) }  |                        |
!                                          |  Gba(r.r')  Gbb(r.r')  |
!                                          |_                      _|
!
!  These GTE, Gij were derived by computing the i'th component of the field at r
!  due to a j-oriented unit electric dipole at r'.  The integral of the GTE is
!  over r so we have the contradiction of designating the cells over which we
!  integrate the product of the GTE and scattering curent as the receiver cells
!  and the cell where we evaluate the field as the source cell.  Let's change
!  this by first putting the above form into the more expected operator form by
!  writing the GTE matrix in transposed form as:
!
!   _        _       _                      _    _       _
!  |  Ea(r')  |     |  Gaa(r,r')  Gba(r,r')  |  |  Ja(r)  |
!  |          |  =  |                        |  |         |
!  |  Eb(r')  |     |  Gab(r,r')  Gbb(r,r')  |  |  Jb(r)  |
!  |_        _|     |_                      _|  |_       _|
!
!
!  Using the self-adjoint nature of the GTE, we can rewrite this as the transpose
!  by switching the role of "source" and "receiver" in the GTE.
!
!
!   _        _        _                       _    _       _
!  |  Ea(r')  |      |   Gaa(r',r)  Gab(r',r)  |  |  Ja(r)  |
!  |          |   =  |                         |  |         |
!  |  Eb(r')  |      |   Gba(r',r)  Gbb(r',r)  |  |  Jb(r)  |
!  |_        _|      |_                       _|  |_       _|
!
!    Now, Gij(r',r) = i'th component at r' due to j-oriented source at r
!    Thus the second operator form relates the observed field at r'
!    to the source at r in the more expected way

!    Although the Gij are computed initially as the received field at r due to
!    a source at r', the expression above requires that the Gij are to be used
!    in a system centred at the receiver point rather than the spource point.
!    This affects only the cross terms with z.  Gxz(r',r) = -Gxz(r,r')
!    Gzx(r',r) = -Gxz(r',r)
!
!    Thus in what follows, the integration over r is now referred to as the
!    integration over the source cell rather than the receiver cell due to the
!    use of the adjoint form.
!
!  Weidelt's solution for the induced scattering current Js(r') requies that
!  the integrated Green's functions be computed in the form
!
!  Gaa = Saa + Haa /Ksq_bas;  Gab = Sab + Hab /Ksq_bas;
!                             Gba = Sba + Hba /Ksq_bas  &
!  Gbb = Sbb + Hbb /Ksq_bas   where  Ksq_bas :=  i * omega * mu / res(basement).
!
!  Saa, Sab=Sba, & Sbb are the induced (divergence free) part of the GTE,
!  corresponding to currents enclosed within the plate.
!  Haa, Hab, Hba, & Hbb are the curl-free or current channelling part
!  of the GTE which are closed outside the plate.
!
!    OUTPUT:
!   The inoitial "reversed" Green's functions are expressed as:
!
!  VAAI (JCR,JCS) - relate the along strike component of the electric field at
!  SAAI (JCR,JCS)   cell JCR to the along strike component of the scattering
!                   current of cell JCS
!
!  VBAI (JCR,JCS) - relate the down dip component of the electric field at
!  SBAI (JCR,JCS)   cell JCR due to the along strike component of the
!                   scattering current of cell JCS
!
!  VABI (JCR,JCS) - relate the along strike component of the electric field at
!  SABI (JCR,JCS)   cell JCR due to the down dip component of the scattering
!                   current of cell JCS
!
!  VBBI (JCR,JCS) - relate the down dip component of the electric field at
!  SBBI (JCR,JCS)   cell JCR due to the down dip component of the scattering
!                   current of cell JCS of plate JPS
!
!  the  OUTPUT is  Sij and converts Hij such that Hij -> Hij + Ksq_bas * Sij
!
!  the forms used by the subroutine SCAT_MTRX_BUILD
!
!  Symmetry is used to reduce computation.  If the source is in row JB and the
!  receiver point in row KB, the GTE will be function of horizontal separation
!  (translation independent).  In what follows, KB and JB are the row indices
!  of the receiver and source cells respectively.  JA is a column index
!  denoting horizontal separation.
!
!  Thus, SAB(KB,JB,JA) is the electric field in the XI-oriented (along-strike)
!  field at row KB, due to a ETA (down dip)) oriented electric dipole current
!  integrated over a cell in row JB, column spearation JA.
!  SAA is the XI electric field due to a XI-oriented dipole.
!  SBB is the ETA electric field caused by an ETA-oriented electric dipole.
!  Similarly for VAA, VAB, VBA, & VBB.
!
!  In what follows, XI is the coordinate along strike which is defined as a
!  clockwise rotation (East of North) from the X (North) axis).
!  ETA is the coordinate down dip.  ETA = X*CDIP + Z*SDIP.
!  The "reeiver cell" has cell numbers (KB,1) in the (ETA,XI) direction.
!  The "source cell" has cell numbers (JB,JA) in the (ETA,XI) direction.
!  Integration is over the source cell.
!
!  NAL & NBL are the number of cells along strike and down dip respectively.
!  DAL & DBL are the corresponding lengths of the cells.
!  DPTHB is depth to basement.
!  PLTOP is the distance to the top of the plate and (CDIP, SDIP) refer
!  to the dip.
!  A vertical body has these as (0,1) and a flat lying plate as (1,0)
!  VBBT * VABT are temporary storage variables.
!
! -------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER, PARAMETER :: NGL=5
 COMPLEX, PARAMETER :: ZERO=(0.,0.)
 INTEGER NAL,NBL,NAB,NREGT,NZ2,J1,J2,KB,KA,JB,JA,JB_KB,JA_KA,JAB,KAB
 REAL, DIMENSION(4,NREGT,NZ2) :: GR1Z,GI1Z,GR2Z,GI2Z,GR3Z,GI3Z,GR4Z,GI4Z,GR5Z,GI5Z,GR6Z,GI6Z
 REAL, DIMENSION(NGL) :: GLX,GLW,WGHTA,WGHTB,XI_GL,ETA_GL(NGL)
 REAL RHOTRP(NREGT),ZV2(NZ2),DPTHB,DAL,DALH,DBL,DBLH,CDP,SDP,CPL,SPL,CAZ,SAZ,X,XC, &
      XGL,Y,YC,YGL,ZCR,ZGL,ZTR,RHO,XBAR,XBARSQ,YBAR,YBARSQ,R23(2,3),R32(3,2)
 REAL, DIMENSION(NAB) :: XCEL1,YCEL1,ZCEL1
 COMPLEX, DIMENSION(NBL,NAL) :: SAAD,VAAD,VABD,VBBD
 COMPLEX, DIMENSION(NBL,NBL,NAL,NAL) :: SAA,SBA,SBB,VAA,VAB,VBA,VBB
 COMPLEX GAB(2,2),VAB22(2,2),SAB22(2,2),GXYZ(3,3),RTMP(3,2)
 COMPLEX C2DINTRP,KSQN,KBASE,AAS,AAV,ABV,BBV,GC1,GC2,GC3,GC4,GC5,GC6

 DATA GLX / -.9061798, -.5384693,    0.,    .5384693, .9061798/
 DATA GLW /  .2369269,  .4786287, .5688888, .4786287, .2369269/

 SAA=ZERO; SBA=ZERO; SBB=ZERO; VAA=ZERO; VAB=ZERO; VBA=ZERO; VBB=ZERO;

! Set up propagation constants, integration weights, and interpolation range for
! Gauss-Legendre integration in ETA direction at density of 1/5 modified skin depth.

 CAZ = 1.
 SAZ = 0.
 CALL RXYZ2PLT (CAZ,SAZ,CDP,SDP,CPL,SPL,R23)
 CALL RPLT2XYZ (CAZ,SAZ,CDP,SDP,CPL,SPL,R32)

 WGHTB = GLW * DBL /2.  ! Set weight array
 DBLH = DBL / 2.
 ETA_GL = GLX * DBLH
 DALH = DAL / 2.
 KBASE = SQRT (KSQN)
 IF (REAL (KBASE) < 0) KBASE = -KBASE

! Compute the direct component for use after indirect component integration
! Because it depends only on the intercell distance and not plate position,
! it can be computed directly in plate system, requiring no rotation.
! VBA picks up the direct component from VAB further down.

 DO JA = 1,NAL
   DO JB = 1, NBL
     CALL EGTDIR (JB,JA,KBASE,DAL,DBL,AAS,AAV,ABV,BBV)
     SAAD(JB,JA) = AAS
     VAAD(JB,JA) = AAV
     VBBD(JB,JA) = BBV
     VABD(JB,JA) = ABV
   END DO
 END DO

!  ETA, ETAS, ETAR refer to distances down dip from designated top edge of plate.
!  They are in the "plate" coordinate system and are independent of dip & rotation.
!  Similarly, XI refers to distances along strike in the "plate" coordinate system.

 DO KB = 1, NBL                                  ! receiver cell row loop
   DO JB = KB, NBL                               ! source cell row loop
     DO J1 = 1, NGL
       YGL = ETA_GL(J1) * CDP
       ZGL = ETA_GL(J1) * SDP

       KA = 1                     !     JA = KA.  Use symmetry wrt XI.
       KAB = KA + (KB-1)* NAL
       JAB = KA + (JB-1)* NAL
       XC = XCEL1(JAB) - XCEL1(KAB)
       YC = YCEL1(JAB) - YCEL1(KAB)
       ZCR = ZCEL1(KAB) + ZCEL1(JAB) - 2.* DPTHB

       Y = YC + YGL

       XI_GL = (GLX + 1.) * DALH / 2.
       WGHTA = GLW * DAL /4.  ! Set weight array

       SAB22 = ZERO;  VAB22 = ZERO
       DO J2 = 1,NGL
         XGL =  XI_GL(J2)
         X = XC  + XGL * CPL - ZGL * SPL
         ZTR = ZCR + XGL * SPL + ZGL * CPL

         RHO = SQRT (X**2 + Y**2)
         XBAR = X / RHO
         XBARSQ = XBAR**2
         YBAR = Y / RHO
         YBARSQ = YBAR**2

         GC1 = C2DINTRP (RHOTRP,NREGT,ZV2,NZ2,GR1Z,GI1Z,RHO,ZTR)
         GC2 = C2DINTRP (RHOTRP,NREGT,ZV2,NZ2,GR2Z,GI2Z,RHO,ZTR)
         GC3 = C2DINTRP (RHOTRP,NREGT,ZV2,NZ2,GR3Z,GI3Z,RHO,ZTR)
         GC4 = C2DINTRP (RHOTRP,NREGT,ZV2,NZ2,GR4Z,GI4Z,RHO,ZTR)
         GC5 = C2DINTRP (RHOTRP,NREGT,ZV2,NZ2,GR5Z,GI5Z,RHO,ZTR)

         GXYZ = ZERO; GAB = ZERO

         GXYZ(1,1) = (1. - 2.*YBARSQ) * GC4 + YBARSQ * GC1             ! Sxx
         GXYZ(2,2) = (1. - 2.*XBARSQ) * GC4 + XBARSQ * GC1             ! Syy
         GXYZ(3,3) = ZERO                                              ! Szz

         RTMP = MATMUL (GXYZ, R32)
         GAB =  MATMUL (R23, RTMP)
         SAB22 = SAB22 + WGHTA(J2) * GAB

         GXYZ = ZERO; GAB = ZERO

         GXYZ(1,1) = (1. - 2.*XBARSQ) * GC5 + XBARSQ * GC2             ! Vxx
         GXYZ(2,2) = (1. - 2.*YBARSQ) * GC5 + YBARSQ * GC2             ! Vyy
         GXYZ(3,3) = -GC3                                              ! Vzz

         RTMP = MATMUL (GXYZ, R32)
         GAB =  MATMUL (R23, RTMP)
         VAB22 = VAB22 + WGHTA(J2) * GAB
       END DO

       SAA(KB,JB,KA,KA) = SAA(KB,JB,KA,KA) + 2. * WGHTB(J1) * SAB22(1,1)
       SBA(KB,JB,KA,KA) = SBA(KB,JB,KA,KA) + 2. * WGHTB(J1) * SAB22(2,1)
       SBB(KB,JB,KA,KA) = SBB(KB,JB,KA,KA) + 2. * WGHTB(J1) * SAB22(2,2)
       VAA(KB,JB,KA,KA) = VAA(KB,JB,KA,KA) + 2. * WGHTB(J1) * VAB22(1,1)
       VBB(KB,JB,KA,KA) = VBB(KB,JB,KA,KA) + 2. * WGHTB(J1) * VAB22(2,2)
       VAB(KB,JB,KA,KA) = VAB(KB,JB,KA,KA) + 2. * WGHTB(J1) * VAB22(1,2)
       VBA(KB,JB,KA,KA) = VBA(KB,JB,KA,KA) + 2. * WGHTB(J1) * VAB22(2,1)

       XI_GL = GLX * DALH
       WGHTA = GLW * DALH                      ! Set weight array
       DO JA = 2, NAL                       ! receiver cell column loop
         JAB = JA + (JB-1)* NAL

         XC = XCEL1(JAB) - XCEL1(KAB)
         YC = YCEL1(JAB) - YCEL1(KAB)
         ZCR = ZCEL1(KAB) + ZCEL1(JAB) - 2.* DPTHB

         Y = YC + YGL

         SAB22 = ZERO;  VAB22 = ZERO
         DO J2 = 1, NGL
           XGL =  XI_GL(J2)
           X = XC  + XGL * CPL - ZGL * SPL
           ZTR = ZCR + XGL * SPL + ZGL * CPL

           RHO = SQRT (X**2 + Y**2)
           XBAR = X / RHO
           YBAR = Y / RHO
           XBARSQ = XBAR**2
           YBARSQ = YBAR**2

           GC1 = C2DINTRP (RHOTRP,NREGT,ZV2,NZ2,GR1Z,GI1Z,RHO,ZTR)
           GC2 = C2DINTRP (RHOTRP,NREGT,ZV2,NZ2,GR2Z,GI2Z,RHO,ZTR)
           GC3 = C2DINTRP (RHOTRP,NREGT,ZV2,NZ2,GR3Z,GI3Z,RHO,ZTR)
           GC4 = C2DINTRP (RHOTRP,NREGT,ZV2,NZ2,GR4Z,GI4Z,RHO,ZTR)
           GC5 = C2DINTRP (RHOTRP,NREGT,ZV2,NZ2,GR5Z,GI5Z,RHO,ZTR)
           GC6 = C2DINTRP (RHOTRP,NREGT,ZV2,NZ2,GR6Z,GI6Z,RHO,ZTR)

           GXYZ = ZERO; GAB = ZERO

           GXYZ(1,1) = (1. - 2.*YBARSQ) * GC4 + YBARSQ * GC1             ! Sxx
           GXYZ(2,2) = (1. - 2.*XBARSQ) * GC4 + XBARSQ * GC1             ! Syy
           GXYZ(3,3) = ZERO                                              ! Szz
           GXYZ(1,2) = XBAR * YBAR * (2.* GC4 - GC1)                     ! Sxy
           GXYZ(2,1) = GXYZ(1,2)                                         ! Syx

           RTMP = MATMUL (GXYZ, R32)
           GAB =  MATMUL (R23, RTMP)
           SAB22 = SAB22 + WGHTA(J2) * GAB
           GXYZ = ZERO; GAB = ZERO

           GXYZ(1,1) = (1. - 2.*XBARSQ) * GC5 + XBARSQ * GC2             ! Vxx
           GXYZ(2,2) = (1. - 2.*YBARSQ) * GC5 + YBARSQ * GC2             ! Vyy
           GXYZ(3,3) = -GC3                                              ! Vzz
           GXYZ(1,2) = XBAR * YBAR * (GC2 - 2.* GC5)                     ! Vxy
           GXYZ(2,1) = GXYZ(1,2)                                         ! VyX
           GXYZ(3,1) = XBAR * GC6                                        ! Vzx
           GXYZ(3,2) = YBAR * GC6                                        ! Vzy
           GXYZ(1,3) = -XBAR * GC6                                       ! Vxz
           GXYZ(2,3) = -YBAR * GC6                                       ! Vyz

           RTMP = MATMUL (GXYZ, R32)
           GAB =  MATMUL (R23, RTMP)
           VAB22 = VAB22 + WGHTA(J2) * GAB
         END DO

         SAA(KB,JB,KA,JA) = SAA(KB,JB,KA,JA) + WGHTB(J1) * SAB22(1,1)
         SBA(KB,JB,KA,JA) = SBA(KB,JB,KA,JA) + WGHTB(J1) * SAB22(2,1)
         SBB(KB,JB,KA,JA) = SBB(KB,JB,KA,JA) + WGHTB(J1) * SAB22(2,2)
         VAA(KB,JB,KA,JA) = VAA(KB,JB,KA,JA) + WGHTB(J1) * VAB22(1,1)
         VBB(KB,JB,KA,JA) = VBB(KB,JB,KA,JA) + WGHTB(J1) * VAB22(2,2)
         VAB(KB,JB,KA,JA) = VAB(KB,JB,KA,JA) + WGHTB(J1) * VAB22(1,2)
         VBA(KB,JB,KA,JA) = VBA(KB,JB,KA,JA) + WGHTB(J1) * VAB22(2,1)
       END DO                   ! JA Loop: 1 to NAL
     END DO                   ! J1 Loop: 1 to NGL down dip

     DO KA = 2,NAL
       DO JA = KA,NAL
         JA_KA = 1 + ABS (JA - KA)
         SAA(KB,JB,KA,JA) = SAA(KB,JB,1,JA_KA)
         SBB(KB,JB,KA,JA) = SBB(KB,JB,1,JA_KA)
         VAA(KB,JB,KA,JA) = VAA(KB,JB,1,JA_KA)
         VBB(KB,JB,KA,JA) = VBB(KB,JB,1,JA_KA)
         SBA(KB,JB,KA,JA) = SBA(KB,JB,1,JA_KA)
         VBA(KB,JB,KA,JA) = VBA(KB,JB,1,JA_KA)
         VAB(KB,JB,KA,JA) = VAB(KB,JB,1,JA_KA)
       END DO
     END DO

!  Add in the direct contributions for JB >= KB & JA >= KA.
!  Use symmetry to fill in the rest of the matrix.

!  Symmetry:  VAB = A* Vxy + B* Vxz.  VBA = A* Vxy - B* Vxz = A* Vxy + B* Vzx
!  Thus under interchange of A (or x) VAB -> -VAB  & VBA -> -VBA
!  But under interchange of B (or y) VAB -> -VBA  & VBA -> -VAB

     JB_KB = JB - KB + 1
     DO KA = 1,NAL
       DO JA = KA,NAL
         JA_KA = JA - KA + 1

         SAA(KB,JB,KA,JA) = SAA(KB,JB,KA,JA) + SAAD(JB_KB,JA_KA)
         SBB(KB,JB,KA,JA) = SBB(KB,JB,KA,JA) + SAAD(JB_KB,JA_KA)
         VAA(KB,JB,KA,JA) = VAA(KB,JB,KA,JA) + VAAD(JB_KB,JA_KA)
         VBB(KB,JB,KA,JA) = VBB(KB,JB,KA,JA) + VBBD(JB_KB,JA_KA)
         VAB(KB,JB,KA,JA) = VAB(KB,JB,KA,JA) + VABD(JB_KB,JA_KA)
         VBA(KB,JB,KA,JA) = VBA(KB,JB,KA,JA) + VABD(JB_KB,JA_KA)


         SAA(KB,JB,JA,KA) =  SAA(KB,JB,KA,JA)
         SBB(KB,JB,JA,KA) =  SBB(KB,JB,KA,JA)
         VAA(KB,JB,JA,KA) =  VAA(KB,JB,KA,JA)
         VBB(KB,JB,JA,KA) =  VBB(KB,JB,KA,JA)
         SBA(KB,JB,JA,KA) = -SBA(KB,JB,KA,JA)
         VBA(KB,JB,JA,KA) = -VBA(KB,JB,KA,JA)
         VAB(KB,JB,JA,KA) = -VAB(KB,JB,KA,JA)
       END DO
     END DO
   END DO                   ! JB Loop: 1 to NBL

   DO JB = KB+1, NBL
     SAA(JB,KB,1:NAL,1:NAL) =  SAA(KB,JB,1:NAL,1:NAL)
     SBB(JB,KB,1:NAL,1:NAL) =  SAA(KB,JB,1:NAL,1:NAL)
     VAA(JB,KB,1:NAL,1:NAL) =  VAA(KB,JB,1:NAL,1:NAL)
     VBB(JB,KB,1:NAL,1:NAL) =  VBB(KB,JB,1:NAL,1:NAL)
     SBA(JB,KB,1:NAL,1:NAL) = -SBA(KB,JB,1:NAL,1:NAL)
     VAB(JB,KB,1:NAL,1:NAL) = -VBA(KB,JB,1:NAL,1:NAL)
     VBA(JB,KB,1:NAL,1:NAL) = -VAB(KB,JB,1:NAL,1:NAL)
   END DO
 END DO                   ! KB Loop: 1 to NBL

! SET UP S-V SUM AND STORE IT IN V.

 VAA = VAA + SAA * KSQN
 VAB = VAB + SBA * KSQN
 VBA = VBA + SBA * KSQN
 VBB = VBB + SBB * KSQN

 END SUBROUTINE EGT_BOSS

 SUBROUTINE EGT_PL_BOSS (NAL,NBL,NAB,DAL,DBL,KSQN,DPTHB,PTOPL,CDP,SDP,CPL,SPL,NZ2,ZV2, &
                         NREGT,RHOTRP,GR1Z,GI1Z,GR2Z,GI2Z,GR3Z,GI3Z,GR4Z,GI4Z,GR5Z,    &
                         GI5Z,GR6Z,GI6Z,SAA,SBA,SBB,VAA,VAB,VBA,VBB)
! ---------------------------------------------------------------------------------

!***  Calls EGTDIR
!***  Called by SCAT_MTRX_BOSS

!         INPUT
!         -----
!
!     NBL,NAL - number of cell rows and columns respectively
!     DBL,DAL - cell dimensions down dip and along strike respectively
!        KSQN - basement propagation constant
!       DPTHB - depth to basement
!    CDP, SDP - dip cosine and sine
!    CPL, SPL - rotation cosine and sine
!    ZV2 - array of NZ2 depths used for vertical interpolation
!    RHOTRP - array of NREGT logrithmically spaced points used for horizontalal interpolation
!    GR1Z to GR6Z - 2D grid of real component of 6 Green's tensor components integrals
!    GI1Z to GI6Z - 2D grid of imaginary component of 6 Green's tensor components integrals
!
!         OUPUT
!         ------
!
!    SAA, SBB, SBA - divergence free integrated Green's tensor elements
!    VAA, VBB, VAB, VBA - curl free integrated Green's tensor elements
!                         A => along strike
!                         B => down dip
!
!  EGT_BOSS computes the integrals over the "source" cell of the Green's tensor
!  elements ( GTE ) for single plates contained in a uniform half-space or
!  entirely inthe bottom layer of a two layer half-space.  The direct portion is
!  computed separately from the reflected/transmitted scattered terms.
!  Azimuth is irrelevant for single plate EGT
!
!  In what follows, the GTE have the units of electric field divided by a
!  factor (-iwu) because this factor is explicitly included as a multiplier
!  outside the integral over area when solving the integral equation.
!
!  In the formulation the integrated Green's tensor G relates the field at r'
!  to the current at r by
!                                           _                      _
!                                          |  Gaa(r.r')  Gab(r.r')  |
!  { Ea(r'), Eb(r') } =  { Ja(r), Jb(r) }  |                        |
!                                          |  Gba(r.r')  Gbb(r.r')  |
!                                          |_                      _|
!
!  These GTE, Gij were derived by computing the i'th component of the field at r
!  due to a j-oriented unit electric dipole at r'.  The integral of the GTE is
!  over r so we have the contradiction of designating the cells over which we
!  integrate the product of the GTE and scattering curent as the receiver cells
!  and the cell where we evaluate the field as the source cell.  Let's change
!  this by first putting the above form into the more expected operator form by
!  writing the GTE matrix in transposed form as:
!
!   _        _       _                      _    _       _
!  |  Ea(r')  |     |  Gaa(r,r')  Gba(r,r')  |  |  Ja(r)  |
!  |          |  =  |                        |  |         |
!  |  Eb(r')  |     |  Gab(r,r')  Gbb(r,r')  |  |  Jb(r)  |
!  |_        _|     |_                      _|  |_       _|
!
!
!  Using the self-adjoint nature of the GTE, we can rewrite this as the transpose
!  by switching the role of "source" and "receiver" in the GTE.
!
!
!   _        _        _                       _    _       _
!  |  Ea(r')  |      |   Gaa(r',r)  Gab(r',r)  |  |  Ja(r)  |
!  |          |   =  |                         |  |         |
!  |  Eb(r')  |      |   Gba(r',r)  Gbb(r',r)  |  |  Jb(r)  |
!  |_        _|      |_                       _|  |_       _|
!
!    Now, Gij(r',r) = i'th component at r' due to j-oriented source at r
!    Thus the second operator form relates the observed field at r'
!    to the source at r in the more expected way

!    Although the Gij are computed initially as the received field at r due to
!    a source at r', the expression above requires that the Gij are to be used
!    in a system centred at the receiver point rather than the spource point.
!    This affects only the cross terms with z.  Gxz(r',r) = -Gxz(r,r')
!    Gzx(r',r) = -Gxz(r',r)
!
!    Thus in what follows, the integration over r is now referred to as the
!    integration over the source cell rather than the receiver cell due to the
!    use of the adjoint form.
!
!  Weidelt's solution for the induced scattering current Js(r') requies that
!  the integrated Green's functions be computed in the form
!
!  Gaa = Saa + Haa /Ksq_bas;  Gab = Sab + Hab /Ksq_bas;
!                             Gba = Sba + Hba /Ksq_bas  &
!  Gbb = Sbb + Hbb /Ksq_bas   where  Ksq_bas :=  i * omega * mu / res(basement).
!
!  Saa, Sab=Sba, & Sbb are the induced (divergence free) part of the GTE,
!  corresponding to currents enclosed within the plate.
!  Haa, Hab, Hba, & Hbb are the curl-free or current channelling part
!  of the GTE which are closed outside the plate.
!
!    OUTPUT:
!   The inoitial "reversed" Green's functions are expressed as:
!
!  VAAI (JCR,JCS) - relate the along strike component of the electric field at
!  SAAI (JCR,JCS)   cell JCR to the along strike component of the scattering
!                   current of cell JCS
!
!  VBAI (JCR,JCS) - relate the down dip component of the electric field at
!  SBAI (JCR,JCS)   cell JCR due to the along strike component of the
!                   scattering current of cell JCS
!
!  VABI (JCR,JCS) - relate the along strike component of the electric field at
!  SABI (JCR,JCS)   cell JCR due to the down dip component of the scattering
!                   current of cell JCS
!
!  VBBI (JCR,JCS) - relate the down dip component of the electric field at
!  SBBI (JCR,JCS)   cell JCR due to the down dip component of the scattering
!                   current of cell JCS of plate JPS
!
!  the  OUTPUT is  Sij and converts Hij such that Hij -> Hij + Ksq_bas * Sij
!
!  the forms used by the subroutine SCAT_MTRX_BUILD
!
!  Thus, SAB(KB,JB,JA) is the electric field in the XI-oriented (along-strike)
!  field at row KB, due to a ETA (down dip)) oriented electric dipole current
!  integrated over a cell in row JB, column spearation JA.
!  SAA is the XI electric field due to a XI-oriented dipole.
!  SBB is the ETA electric field caused by an ETA-oriented electric dipole.
!  Similarly for VAA, VAB, VBA, & VBB.
!
!  In what follows, XI is the coordinate along strike which is defined as a
!  clockwise rotation (East of North) from the X (North) axis).
!  ETA is the coordinate down dip.  ETA = X*CDIP + Z*SDIP.
!  The "reeiver cell" has cell numbers (KB,1) in the (ETA,XI) direction.
!  The "source cell" has cell numbers (JB,JA) in the (ETA,XI) direction.
!  Integration is over the source cell.
!
!  NAL & NBL are the number of cells along strike and down dip respectively.
!  DAL & DBL are the corresponding lengths of the cells.
!  DPTHB is depth to basement.
!  PLTOP is the distance to the top of the plate and (CDIP, SDIP) refer
!  to the dip.
!  A vertical body has these as (0,1) and a flat lying plate as (1,0)
!  VBBT * VABT are temporary storage variables.
!
! -------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER, PARAMETER :: NGL=5
 COMPLEX, PARAMETER :: ZERO=(0.,0.)
 INTEGER NAL,NBL,NAB,NREGT,NZ2,J1,J2,KB,KA,JB,JA,JB_KB,JA_KA,JAB,KAB
 REAL, DIMENSION(4,NREGT,NZ2) :: GR1Z,GI1Z,GR2Z,GI2Z,GR3Z,GI3Z,GR4Z,GI4Z,GR5Z,GI5Z,GR6Z,GI6Z
 REAL, DIMENSION(NGL) :: GLX,GLW,WGHTA,WGHTB,XI_GL,ETA_GL(NGL)
 REAL RHOTRP(NREGT),ZV2(NZ2),DPTHB,PTOPL,DAL,DALH,DBL,DBLH,CDP,SDP,CPL,SPL,CAZ,SAZ,X, &
      Y,ZTR,RHO,XBAR,XBARSQ,YBAR,YBARSQ,R23(2,3),R32(3,2), &
      XI2D(2),XI3D(3),RCL2D(2),RCL3D(3),ETA0,XI0,WGHTAB
 REAL, DIMENSION(NAB) :: XCELL,YCELL
 COMPLEX, DIMENSION(NBL,NAL) :: SAAD,VAAD,VABD,VBBD
 COMPLEX, DIMENSION(NBL,NBL,NAL,NAL) :: SAA,SBA,SBB,VAA,VAB,VBA,VBB
 COMPLEX GAB(2,2),VAB22(2,2),SAB22(2,2),GXYZ(3,3),RTMP(3,2)
 COMPLEX C2DINTRP,KSQN,KBASE,AAS,AAV,ABV,BBV,GC1,GC2,GC3,GC4,GC5,GC6

 DATA GLX / -.9061798, -.5384693,    0.,    .5384693, .9061798/
 DATA GLW /  .2369269,  .4786287, .5688888, .4786287, .2369269/

 SAA=ZERO; SBA=ZERO; SBB=ZERO; VAA=ZERO; VAB=ZERO; VBA=ZERO; VBB=ZERO;

! Set up propagation constants, integration weights, and interpolation range for
! Gauss-Legendre integration in ETA direction at density of 1/5 modified skin depth.

 DALH = DAL / 2.
 DBLH = DBL / 2.
 XI_GL = GLX * DALH
 ETA_GL = GLX * DBLH
 WGHTA = GLW * DALH
 WGHTB = GLW * DBLH  ! Set weight array
 KBASE = SQRT (KSQN)
 IF (REAL (KBASE) < 0) KBASE = -KBASE

!  The single plate EGT is independent of plate azimuth and horizontal origin.
!  Set up flat plate with strike along X-axis with origin at the midpoint of
!  west side

! Then compute the direct component for use after indirect component integration.
! Since it depends only on the intercell distance and not plate position, it can
! be computed directly in plate system, requiring no dip or rotation.
! VBA picks up the direct component from VAB further down.

 XI0 = -(NAL + 1) * DALH
 ETA0 =  -DBLH
 DO JB = 1,NBL
   Y = ETA0 + JB * DBL
   DO JA = 1,NAL
     X = XI0 + JA * DAL
     JAB = JA + (JB-1) * NAL
     XCELL(JAB) = X
     YCELL(JAB) = Y

     CALL EGTDIR (JB,JA,KBASE,DAL,DBL,AAS,AAV,ABV,BBV)
     SAAD(JB,JA) = AAS
     VAAD(JB,JA) = AAV
     VBBD(JB,JA) = BBV
     VABD(JB,JA) = ABV
   END DO
 END DO

!  Compute the rotation matrices for transfer between (X,Y,Z) and (XI,ETA) representations.

 CAZ = 1.
 SAZ = 0.
 CALL RXYZ2PLT (CAZ,SAZ,CDP,SDP,CPL,SPL,R23)
 CALL RPLT2XYZ (CAZ,SAZ,CDP,SDP,CPL,SPL,R32)


!  ETA, ETAS, ETAR refer to distances down dip from designated top edge of plate.
!  They are in the "plate" coordinate system and are independent of dip & rotation.
!  Similarly, XI refers to distances along strike in the "plate" coordinate system.

 DO KB = 1, NBL                      ! receiver cell row loop
   DO JB = KB, NBL                    ! source cell row loop
     DO KA = 1, NAL                    ! receiver cell column loop
       KAB = KA + (KB-1)* NAL
       RCL2D(1) = XCELL(KAB)            ! Pre-rotated coordinates of receiver cell
       RCL2D(2) = YCELL(KAB)
       RCL3D = MATMUL (R32,RCL2D)        ! 3D rotated receiver cell coordinates
       DO JA = KA, NAL
         JAB = JA + (JB-1)* NAL
         SAB22 = ZERO;  VAB22 = ZERO
         DO J1 = 1, NGL
           XI2D(1) = XI_GL(J1) + XCELL(JAB)
           DO J2 = 1, NGL
             WGHTAB = WGHTA(J2) * WGHTB(J1)
             XI2D(2) = ETA_GL(J2) + YCELL(JAB)
             XI3D = MATMUL (R32,XI2D)
             X = XI3D(1) - RCL3D(1)
             Y = XI3D(2) - RCL3D(2)
             ZTR = XI3D(3) + RCL3D(3)  + 2.* (PTOPL - DPTHB)

             RHO = SQRT (X**2 + Y**2)
             XBAR = 0. ; YBAR = 0.
             IF (RHO > 0.01) THEN
               XBAR = X / RHO
               YBAR = Y / RHO
             END IF
             XBARSQ = XBAR**2
             YBARSQ = YBAR**2

             GC1 = C2DINTRP (RHOTRP,NREGT,ZV2,NZ2,GR1Z,GI1Z,RHO,ZTR)
             GC2 = C2DINTRP (RHOTRP,NREGT,ZV2,NZ2,GR2Z,GI2Z,RHO,ZTR)
             GC3 = C2DINTRP (RHOTRP,NREGT,ZV2,NZ2,GR3Z,GI3Z,RHO,ZTR)
             GC4 = C2DINTRP (RHOTRP,NREGT,ZV2,NZ2,GR4Z,GI4Z,RHO,ZTR)
             GC5 = C2DINTRP (RHOTRP,NREGT,ZV2,NZ2,GR5Z,GI5Z,RHO,ZTR)
             GC6 = C2DINTRP (RHOTRP,NREGT,ZV2,NZ2,GR6Z,GI6Z,RHO,ZTR)

             GXYZ = ZERO; GAB = ZERO

             GXYZ(1,1) = (1. - 2.*YBARSQ) * GC4 + YBARSQ * GC1             ! Sxx
             GXYZ(2,2) = (1. - 2.*XBARSQ) * GC4 + XBARSQ * GC1             ! Syy
             GXYZ(3,3) = ZERO                                              ! Szz
             GXYZ(1,2) = XBAR * YBAR * (2.* GC4 - GC1)                     ! Sxy
             GXYZ(2,1) = GXYZ(1,2)                                         ! Syx

             RTMP = MATMUL (GXYZ, R32)
             GAB =  MATMUL (R23, RTMP)
             SAB22 = SAB22 + WGHTAB * GAB
             GXYZ = ZERO; GAB = ZERO

             GXYZ(1,1) = (1. - 2.*XBARSQ) * GC5 + XBARSQ * GC2             ! Vxx
             GXYZ(2,2) = (1. - 2.*YBARSQ) * GC5 + YBARSQ * GC2             ! Vyy
             GXYZ(3,3) = -GC3                                              ! Vzz
             GXYZ(1,2) = XBAR * YBAR * (GC2 - 2.* GC5)                     ! Vxy
             GXYZ(2,1) = GXYZ(1,2)                                         ! Vyx
             GXYZ(3,1) = XBAR * GC6                                        ! Vzx
             GXYZ(3,2) = YBAR * GC6                                        ! Vzy
             GXYZ(1,3) = -XBAR * GC6                                       ! Vxz
             GXYZ(2,3) = -YBAR * GC6                                       ! Vyz

             RTMP = MATMUL (GXYZ, R32)
             GAB =  MATMUL (R23, RTMP)
             VAB22 = VAB22 + WGHTAB * GAB
           END DO

           SAA(KB,JB,KA,JA) = SAA(KB,JB,KA,JA) + SAB22(1,1)
           SBA(KB,JB,KA,JA) = SBA(KB,JB,KA,JA) + SAB22(2,1)
           SBB(KB,JB,KA,JA) = SBB(KB,JB,KA,JA) + SAB22(2,2)
           VAA(KB,JB,KA,JA) = VAA(KB,JB,KA,JA) + VAB22(1,1)
           VBB(KB,JB,KA,JA) = VBB(KB,JB,KA,JA) + VAB22(2,2)
           VAB(KB,JB,KA,JA) = VAB(KB,JB,KA,JA) + VAB22(1,2)
           VBA(KB,JB,KA,JA) = VBA(KB,JB,KA,JA) + VAB22(2,1)
         END DO               ! JA Loop: 1 to NAL
       END DO                 ! J1 Loop: 1 to NGL down dip
     END DO                   ! KA Loop: 1 to NAL


!  Add in the direct contributions for JB >= KB & JA >= KA.
!  Use symmetry to fill in the rest of the matrix.

!  Symmetry:  VAB = A* Vxy + B* Vxz.  VBA = A* Vxy - B* Vxz = A* Vxy + B* Vzx
!  Thus under interchange of A (or x) VAB -> -VAB  & VBA -> -VBA
!  But under interchange of B (or y) VAB -> -VBA  & VBA -> -VAB

     JB_KB = JB - KB + 1
     DO KA = 1,NAL
       DO JA = KA,NAL
         JA_KA = JA - KA + 1

         SAA(KB,JB,KA,JA) = SAA(KB,JB,KA,JA) + SAAD(JB_KB,JA_KA)
         SBB(KB,JB,KA,JA) = SBB(KB,JB,KA,JA) + SAAD(JB_KB,JA_KA)
         VAA(KB,JB,KA,JA) = VAA(KB,JB,KA,JA) + VAAD(JB_KB,JA_KA)
         VBB(KB,JB,KA,JA) = VBB(KB,JB,KA,JA) + VBBD(JB_KB,JA_KA)
         VAB(KB,JB,KA,JA) = VAB(KB,JB,KA,JA) + VABD(JB_KB,JA_KA)
         VBA(KB,JB,KA,JA) = VBA(KB,JB,KA,JA) + VABD(JB_KB,JA_KA)


         SAA(KB,JB,JA,KA) =  SAA(KB,JB,KA,JA)
         SBB(KB,JB,JA,KA) =  SBB(KB,JB,KA,JA)
         VAA(KB,JB,JA,KA) =  VAA(KB,JB,KA,JA)
         VBB(KB,JB,JA,KA) =  VBB(KB,JB,KA,JA)
         SBA(KB,JB,JA,KA) = -SBA(KB,JB,KA,JA)
         VBA(KB,JB,JA,KA) = -VBA(KB,JB,KA,JA)
         VAB(KB,JB,JA,KA) = -VAB(KB,JB,KA,JA)
       END DO
     END DO
   END DO                   ! JB Loop: 1 to NBL

   DO JB = KB+1, NBL
     SAA(JB,KB,1:NAL,1:NAL) =  SAA(KB,JB,1:NAL,1:NAL)
     SBB(JB,KB,1:NAL,1:NAL) =  SAA(KB,JB,1:NAL,1:NAL)
     VAA(JB,KB,1:NAL,1:NAL) =  VAA(KB,JB,1:NAL,1:NAL)
     VBB(JB,KB,1:NAL,1:NAL) =  VBB(KB,JB,1:NAL,1:NAL)
     SBA(JB,KB,1:NAL,1:NAL) = -SBA(KB,JB,1:NAL,1:NAL)
     VAB(JB,KB,1:NAL,1:NAL) = -VBA(KB,JB,1:NAL,1:NAL)
     VBA(JB,KB,1:NAL,1:NAL) = -VAB(KB,JB,1:NAL,1:NAL)
   END DO
 END DO                   ! KB Loop: 1 to NBL

! SET UP S-V SUM AND STORE IT IN V.

 VAA = VAA + SAA * KSQN
 VAB = VAB + SBA * KSQN
 VBA = VBA + SBA * KSQN
 VBB = VBB + SBB * KSQN

 END SUBROUTINE EGT_PL_BOSS

 SUBROUTINE EGTDIR (JB,JA,KBASE,DAL,DBL,AAS,AAV,ABV,BBV)
!-------------------------------------------------------

!**  Called by EGT_BOSS

!  Computes the integral of the direct (or full space) Green's
!  tensor elements over the receiver cell (JB,JA) for a tangential
!  unit source dipole in cell (1,1).  In the code below, "a" refers
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
! GAA = AAS + AAV/C2;   GAB = ABV / C2;   GBA = GAB;   GBB = AAS + BBV/C2
!
!     Defining Q  as EXP (-KBASE * R) / (4*PI * R), then
! AAS = INT {Q} da db       ABV = -Q
! AAV = -INT {dQ/da} db     BBV = -INT {dQ/db} da
!
! For same cell integration AAS, = INT (1/r) + INT (exp(-kr) / r  - 1/r)
!
!  1/r can be integrated analytically and the second term -> -k as r -> 0


 IMPLICIT NONE
 COMPLEX, PARAMETER :: ZERO=(0.,0.), ONE=(1.,0.), FRPIC=(12.56637, 0.)
 INTEGER, PARAMETER :: NGL=5
 INTEGER JB,JA,J1,J2
 REAL A,B,DAL,DBL,DAH,DBH,XL,XR,ZT,ZB,ASR,RR
 REAL, DIMENSION (NGL) :: GLX,WGL,X,Z,WGX,WGZ
 COMPLEX KBASE,QB,AAS,AAV,BBV,ABV
 LOGICAL PARTS

 INTENT (IN) JB,JA,KBASE,DAL,DBL
 INTENT (OUT) AAS,AAV,ABV,BBV

 DATA GLX / -.9061798, -.5384693, 0., .5384693, .9061798/
 DATA WGL / .2369269, .4786287, .5688888, .4786287, .2369269/

! Set inter-cell distance and Gauss-Legendre integration points and
! weights.  For the sake of exposition, regarding B as in the Z direction,
! B is positive (negative) if 'source" is above (below) receiver cell.
! XL and XR will refer to the left and right cell boundaries
! and ZT and ZB to the top and bottom boundaries respectively.

 A = DAL* (JA-1)
 B = DBL* (JB-1)
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

! Zero out functions.  Evaluate ABV analytically.  If same cell or nearest
! neighbours, integrate AAV and BBV by parts and subtract 1/R part from
! AAS.  Then integrate remainder.  Otherwise, integrate full functions.
! Z is positive downwards so integrals go from top to bottom
! and from left to right of receiver cell.

 AAS = ZERO; AAV = ZERO; BBV = ZERO
 IF ((JA == 1) .OR. (JB == 1)) THEN
   ABV = ZERO
 ELSE
   ABV = Q (KBASE,XL,ZB) + Q (KBASE,XR,ZT) - Q (KBASE,XR,ZB) - Q (KBASE,XL,ZT)
 END IF

 PARTS = .FALSE.
 IF ( JB < 3 .AND. JA < 3) PARTS = .TRUE.
 IF (PARTS) THEN
   AAV = AAVP1 (KBASE,XL,ZT) + AAVP1 (KBASE,XR,ZB) &
      - AAVP1 (KBASE,XR,ZT) - AAVP1 (KBASE,XL,ZB )
   BBV = AAVP1 (KBASE,ZT,XL) + AAVP1 (KBASE,ZB,XR) &
      - AAVP1 (KBASE,ZB,XL) - AAVP1 (KBASE,ZT,XR )
   ASR = RINT (XL,ZT) + RINT (XR,ZB) - RINT (XR,ZT) - RINT (XL,ZB)
   AAS = CMPLX (ASR, 0.)
   DO J1 = 1, NGL
     AAV = AAV + WGZ(J1) * ( AAVP2 (KBASE,XR,Z(J1)) - &
                            AAVP2 (KBASE,XL,Z(J1)) )
     BBV = BBV + WGX(J1) * ( AAVP2 (KBASE,ZB,X(J1)) - &
                            AAVP2 (KBASE,ZT,X(J1)) )
     DO J2 = 1, NGL
       RR = R (X(J1),Z(J2) )
       QB = - KBASE
       IF (RR > 1.0E-10) QB = (EXP (-KBASE*RR) -ONE) /RR
       AAS = AAS + QB * WGX(J1) * WGZ(J2)
     END DO
   END DO

 ELSE

   DO J1 = 1, NGL
     AAV = AAV + WGZ(J1) * ( DQDX (KBASE,XR,Z(J1)) - DQDX (KBASE,XL,Z(J1)) )
     BBV = BBV + WGX(J1) * ( DQDX (KBASE,ZB,X(J1)) - DQDX (KBASE,ZT,X(J1)) )
     DO J2 = 1, NGL
       AAS = AAS + WGX(J1) * WGZ(J2) * Q (KBASE,X(J1),Z(J2))
     END DO
   END DO
 END IF

 AAS = AAS/ FRPIC;  AAV = AAV/ FRPIC;  ABV = ABV/ FRPIC;  BBV = BBV/ FRPIC

! Define in-line whole space functions.  RINT is the integral of 1/R.

 CONTAINS

   REAL FUNCTION R(X1,Z1)
!  ----------------------

   REAL X1,Z1
   R = SQRT (X1*X1 + Z1*Z1)
   END FUNCTION R

   REAL FUNCTION RINT (X1,Z1)
!  --------------------------

!  Integral of 1/r dx dz

   REAL X1,Z1,RQ
   RQ = SQRT (X1*X1 + Z1*Z1)
   RINT = Z1* ALOG (RQ + X1) + X1*ALOG (RQ + Z1)
   END FUNCTION RINT

   COMPLEX FUNCTION AAVP1 (KBASE,X1,Z1)
!  ------------------------------------

   REAL X1,Z1,RQ
   COMPLEX KBASE

   RQ = SQRT (X1*X1 + Z1*Z1)
   AAVP1 = Z1 * EXP (-KBASE*RQ) / (X1*RQ)

   END FUNCTION AAVP1

   COMPLEX FUNCTION AAVP2 (KBASE,X1,Z1)
!  ------------------------------------

   REAL X1,Z1,RQ
   COMPLEX KBASE

   RQ = SQRT (X1*X1 + Z1*Z1)
   AAVP2 = KBASE * EXP (-KBASE*RQ) / X1

   END FUNCTION AAVP2

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

 SUBROUTINE EGT_CSPL (NREGT,RHOTRP,NLYR,THKD,RMUD,SIGL,KSQL,ZTR,GR1,GI1, &
                      GR2,GI2,GR3,GI3,GR4,GI4,GR5,GI5,GR6,GI6)
!----------------------------------------------------------------------

!***  Calls EGT_HNK, CUBSPL
!***  Called by SCAT_MTRX_BOSS

! Sets up cubic spline representations, GRj & GIj (j = 1,6), for the five
! Hankel integrals needed to compute the electric Green's tensor elements
! as a function of RHOTRP.

!   RHOTRP - interpolation array of rho values
!    NREGT - number of interpolation points needed for EGT
!     NLYR - number of layers
!     THKD - array of layer thicknesses
!     RMUD - relative permeability of layers
!     SIGL - complex resistivity of layers
!     KSQL - propagation constants
!      ZTR = the reflected distance between "source" & "receiver"
!            points off bottom of overburden.

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 INTEGER JR,NREGT,NLYR
 REAL, DIMENSION (4,NREGT) :: GR1,GI1,GR2,GI2,GR3,GI3,GR4,GI4,GR5,GI5,GR6,GI6
 REAL RHOTRP(NREGT),ZTR
 REAL (KIND=QL) RMUD(0:NLYR),THKD(NLYR)
 COMPLEX(KIND=QL) KSQL(NLYR),SIGL(NLYR),EHRI(NREGT,6)

 GR1 = 0; GR2 = 0; GR3 = 0; GR4 = 0; GR5 = 0; GR6 = 0
 GI1 = 0; GI2 = 0; GI3 = 0; GI4 = 0; GI5 = 0; GI6 = 0

 CALL EGT_HNK (NREGT,RHOTRP,NLYR,THKD,RMUD,SIGL,KSQL,ZTR,EHRI)

 DO JR = 1, NREGT
   GR1(1,JR) = REAL (EHRI(JR,1))
   GI1(1,JR) = REAL (AIMAG (EHRI(JR,1)) )
   GR2(1,JR) = REAL (EHRI(JR,2))
   GI2(1,JR) = REAL (AIMAG (EHRI(JR,2)) )
   GR3(1,JR) = REAL (EHRI(JR,3))
   GI3(1,JR) = REAL (AIMAG (EHRI(JR,3)) )
   GR4(1,JR) = REAL (EHRI(JR,4))
   GI4(1,JR) = REAL (AIMAG (EHRI(JR,4)) )
   GR5(1,JR) = REAL (EHRI(JR,5))
   GI5(1,JR) = REAL (AIMAG (EHRI(JR,5)) )
   GR6(1,JR) = REAL (EHRI(JR,6))
   GI6(1,JR) = REAL (AIMAG (EHRI(JR,6)) )
 END DO

 CALL CUBSPL (RHOTRP,GR1,NREGT)
 CALL CUBSPL (RHOTRP,GI1,NREGT)
 CALL CUBSPL (RHOTRP,GR2,NREGT)
 CALL CUBSPL (RHOTRP,GI2,NREGT)
 CALL CUBSPL (RHOTRP,GR3,NREGT)
 CALL CUBSPL (RHOTRP,GI3,NREGT)
 CALL CUBSPL (RHOTRP,GR4,NREGT)
 CALL CUBSPL (RHOTRP,GI4,NREGT)
 CALL CUBSPL (RHOTRP,GR5,NREGT)
 CALL CUBSPL (RHOTRP,GI5,NREGT)
 CALL CUBSPL (RHOTRP,GR6,NREGT)
 CALL CUBSPL (RHOTRP,GI6,NREGT)

 END SUBROUTINE EGT_CSPL

 SUBROUTINE EGT_HNK (NRHO,RHOTRP,NLYR,THKD,RMUD,SIGL,KSQL,ZTR,EHRI)
!-----------------------------------------------------------------

!***  Called by EGT_CSPL
!***  Calls EGT_KER

! Sets up the five integrals EHRI (JR,J1), J1 = 1,6, needed to compute the
! electric Green's tensor elements in EGT_BOSS.  It uses the flow through
! Hankel transform method to compute values at RHOTRP(JR), (15 point per decade
! spacing).  It uses a 15 point per decade filter coefficient set derived from
! Christensen's program, FLTGEN.

!   RHOTRP - interpolation array of rho values
!     NRHO - number of interpolation points needed for EGT
!     THKD - array of layer thicknesses
!     RMUD - relative permeability of layers
!     SIGL - complex resistivity of layers
!     KSQL - propagation constants
!      ZTR = the reflected distance between "source" & "receiver"
!            points off bottom of overburden.
!     EHRI - inverse Hankel transform (J = 1,6)

 USE LG_Filter_Coefficients

 IMPLICIT NONE
 REAL(KIND=QL), PARAMETER :: FOURPI=12.56637_QL
 COMPLEX (KIND=QL), PARAMETER :: ZERO=(0._QL, 0._QL)
 INTEGER NLYR,NRHO,L,JR,LMAX,LMIN,K,KMAX,KMIN,KBOT,K1
 REAL RHOTRP(NRHO),ZTR
 REAL(KIND=QL) DELTA,Y1,Y,RD,RMUD(0:NLYR),THKD(NLYR),LMBDA,ZTRD,RHOD
 COMPLEX(KIND=QL) KSQL(NLYR),SIGL(NLYR),KER(JNLO-NRHO:JNHI,6),EHRI(NRHO,6)
 LOGICAL JUMP

 ZTRD = REAL (ZTR,QL)

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
   CALL EGT_KER (NRHO,K,JR,L,LMBDA,NLYR,THKD,RMUD,SIGL,KSQL,ZTRD,KER,EHRI,JUMP)
   IF (JUMP) EXIT
 END DO

 Y = Y1                       ! Finish off the low end for RHOTRP(1)
 DO L = -51, JNLO, -1
   LMIN = L                   ! Minimum filter index used
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
   K = L + 1 - JR             ! Compute the kernel index.
   CALL EGT_KER (NRHO,K,JR,L,LMBDA,NLYR,THKD,RMUD,SIGL,KSQL,ZTRD,KER,EHRI,JUMP)
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
     EHRI(JR,1:3) = EHRI(JR,1:3) + KER(K,1:3) * WJ0(L)
     EHRI(JR,4:6) = EHRI(JR,4:6) + KER(K,4:6) * WJ1(L)
 END DO

 IF (K1 > KBOT) THEN    !  Add low end kernel values until convergence.
   DO K = K1-1, KBOT, -1
     KMIN = K
     L = K - 1 + JR
     Y = Y1 + DBLE (L) * DELTA
     LMBDA = EXP (Y)
     CALL EGT_KER (NRHO,K,JR,L,LMBDA,NLYR,THKD,RMUD,SIGL,KSQL,ZTRD,KER,EHRI,JUMP)
     IF (JUMP) EXIT
   END DO
 END IF

 DO JR = 2, NRHO-1          !  Compute transforms for all other RHO values
   DO K = KMIN, KMAX         !  using previously computed kernel values.
     L = K - 1 + JR
     IF (L > JNHI .OR. L < JNLO) CYCLE
     EHRI(JR,1:3) = EHRI(JR,1:3) + KER(K,1:3) * WJ0(L)
     EHRI(JR,4:6) = EHRI(JR,4:6) + KER(K,4:6) * WJ1(L)
   END DO
 END DO

 DO JR = 1,NRHO
   RHOD = REAL (RHOTRP(JR),KIND=QL)
   EHRI(JR,4:5) = EHRI(JR,4:5) / RHOD
   EHRI(JR,1:6) = EHRI(JR,1:6) / (FOURPI * RHOD)
 END DO
 END SUBROUTINE EGT_HNK

 SUBROUTINE EGT_KER (NRHO,K,JR,L,LMBDA,NLYR,THKD,RMUD,SIGL,KSQL,ZTRD,KER,EHRI,JUMP)
!---------------------------------------------------------------------------------

!***  Called by EGT_HNK

! Accumulates the integrals for EHRI(J), the five inverse Hankel transforms
! needed for evaluation of Green's tensor elements

!     NRHO - number of points in 15 point / decade array
!        K - filter kernel index
!       JR - RHO interpolation index
!        L - filter point index
!    LMBDA - Hankel transform variable
!     NLYR - number of layers
!     THKD - array of layer thicknesses
!     RMUD - relative permeability of layers
!     SIGL - complex resistivity of layers
!     KSQL - propagation constants
!     ZTRD = (ZS - THKD) + (ZR - THKD) = the reflected distance between
!                 "source" & "receiver" points off bottom of overburden.
!      KER - stored integral kernels
!  EHRI(J) - accumulated complex integrals for inverse Hankel transform (J = 1,6)
!     JUMP - keep integrating if false.

 USE LG_Filter_Coefficients

 IMPLICIT NONE
 REAL, PARAMETER :: TOL=1.E-5
 COMPLEX (KIND=QL), PARAMETER :: ONE = (1._QL,0._QL)
 INTEGER NRHO,K,JR,L,NLYR,J
 REAL (KIND=QL) LMBDA,THKD(NLYR),RMUD(0:NLYR),RMUSQ(NLYR),ZTRD,EHR,EHI,AR,AI
 COMPLEX (KIND=QL) LMBSQ,SN,EHRI(NRHO,6),KER(JNLO-NRHO:JNHI,6),G,ETA,KS,KV,TXP2,TMP(6)
 COMPLEX (KIND=QL), DIMENSION(NLYR) :: SIGL,KSQL,CHI,AF,AG
 COMPLEX (KIND=QL), DIMENSION(0:NLYR) :: S,R,T
 LOGICAL TOO_BIG,JUMP

 LMBSQ = CMPLX (LMBDA * LMBDA, 0._QL, KIND=QL)
 S(0) = CMPLX (LMBDA, 0._QL,KIND=QL)
 DO J = 1,NLYR
   S(J) = SQRT (KSQL(J) + LMBSQ)
   RMUSQ(J) = RMUD(J) * RMUD(J)
 END DO
 SN = S(NLYR)
 T(0) = ( (RMUSQ(1) - 1._QL) * LMBSQ - KSQL(1) ) / ( RMUD(1)*S(0) + S(1) )**2
 R(0) = ONE
 DO J = 1,NLYR-1
   T(J) = (RMUSQ(J+1) - RMUSQ(J)) * LMBSQ + (RMUSQ(J+1) * KSQL(J) - RMUSQ(J) * KSQL(J+1)) &
        / (RMUD(J+1) * S(J) + RMUD(J) * S(J+1))**2
   R(J) = (SIGL(J+1) * S(J) - SIGL(J) * S(J+1)) / (SIGL(J+1) * S(J) + SIGL(J) * S(J+1))
   CHI(J) = EXP (-2._QL * S(J) * THKD(J))
 END DO

 AF(1) = -R(0)           ! ref SUBROUTINE EDSX_COEF
 AG(1) = -T(0)
 DO J = 1, NLYR-1
   AF(J+1) = (AF(J) * CHI(J) - R(J)) / (ONE -  R(J) * AF(J) * CHI(J))
   AG(J+1) = (AG(J) * CHI(J) - T(J)) / (ONE -  T(J) * AG(J) * CHI(J))
 END DO

 TXP2 = EXP (-SN * ZTRD)
 G   = TXP2 * AF(NLYR)
 ETA = TXP2 * AG(NLYR)
 KS = ETA / SN
 KV = -G * SN

 KER(K,1) = KS * LMBDA             ! Sxx, Syx, Syy, Sxy
 KER(K,2) = KV * LMBDA             ! Vxx, Vyx, Vyy, Vyx
 KER(K,3) = G   * LMBDA**3 / SN    ! Vzz
 KER(K,4) = KS                     ! Sxx, Syx, Syy, Sxy
 KER(K,5) = KV                     ! Vxx, Vyx, Vyy, Vyx
 KER(K,6) = G   * LMBSQ            ! Vzx

 TMP(1:3) = WJ0(L) * KER(K,1:3)
 TMP(4:6) = WJ1(L) * KER(K,4:6)

 JUMP = .TRUE.
 DO J = 1,6
   EHRI(JR,J) = EHRI(JR,J) + TMP(J)
   AR = ABS (REAL (TMP(J)))
   AI = ABS (AIMAG (TMP(J)))
   EHR = ABS (REAL (EHRI(JR,J)))
   EHI = ABS (AIMAG (EHRI(JR,J)))
   TOO_BIG = .FALSE.
   IF (AR > TOL* EHR) TOO_BIG = .TRUE.
   IF (AI > TOL* EHI) TOO_BIG = .TRUE.
   IF (TOO_BIG) JUMP = .FALSE.
 END DO
 END SUBROUTINE EGT_KER

 SUBROUTINE EGT_UL_BOSS (NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,NAL,NBL,NAB,DAL,DBL,XCEL1,YCEL1, &
                         ZC1L,RXLYR,CDP,SDP,NREGT,RHOTRP,SAA,SBA,SBB,VAA,VAB,VBA,VBB)
! --------------------------------------------------------------------------------------

!***  Calls EGTDIR
!***  Called by SCAT_MTRX_BOSS

!         INPUT
!         -----
!
!     NBL,NAL - number of cell rows and columns respectively
!     DBL,DAL - cell dimensions down dip and along strike respectively
!        KSQL - propagation constant for layers
!       DPTHL - depth to top of each layer
!    CDP, SDP - dip cosine and sine
!    XCEL1,YCEL1,ZC1L - X,Y,Z coordinates of cells on plate
!    RHOTRP - array of NREGT logrithmically spaced points used for horizontalal interpolation
!
!         OUPUT
!         ------
!
!    SAA, SBB, SBA - divergence free integrated Green's tensor elements
!    VAA, VBB, VAB, VBA - curl free integrated Green's tensor elements
!                         A => along strike
!                         B => down dip
!
!  EGT_BOSS computes the integrals over the "source" cell of the Green's tensor
!  elements ( GTE ) for single plates contained in a uniform half-space or
!  entirely inthe bottom layer of a two layer half-space.  The direct portion is
!  computed separately from the reflected/transmitted scattered terms.

!  In what follows, the GTE have the units of electric field divided by a
!  factor (-iwu) because this factor is explicitly included as a multiplier
!  outside the integral over area when solving the integral equation.
!
!  In the formulation the integrated Green's tensor G relates the field at r'
!  to the current at r by
!                                           _                      _
!                                          |  Gaa(r.r')  Gab(r.r')  |
!  { Ea(r'), Eb(r') } =  { Ja(r), Jb(r) }  |                        |
!                                          |  Gba(r.r')  Gbb(r.r')  |
!                                          |_                      _|
!
!  These GTE, Gij were derived by computing the i'th component of the field at r
!  due to a j-oriented unit electric dipole at r'.  The integral of the GTE is
!  over r so we have the contradiction of designating the cells over which we
!  integrate the product of the GTE and scattering curent as the receiver cells
!  and the cell where we evaluate the field as the source cell.  Let's change
!  this by first putting the above form into the more expected operator form by
!  writing the GTE matrix in transposed form as:
!
!   _        _       _                      _    _       _
!  |  Ea(r')  |     |  Gaa(r,r')  Gba(r,r')  |  |  Ja(r)  |
!  |          |  =  |                        |  |         |
!  |  Eb(r')  |     |  Gab(r,r')  Gbb(r,r')  |  |  Jb(r)  |
!  |_        _|     |_                      _|  |_       _|
!
!
!  Using the self-adjoint nature of the GTE, we can rewrite this as the transpose
!  by switching the role of "source" and "receiver" in the GTE.
!
!
!   _        _        _                       _    _       _
!  |  Ea(r')  |      |   Gaa(r',r)  Gab(r',r)  |  |  Ja(r)  |
!  |          |   =  |                         |  |         |
!  |  Eb(r')  |      |   Gba(r',r)  Gbb(r',r)  |  |  Jb(r)  |
!  |_        _|      |_                       _|  |_       _|
!
!    Now, Gij(r',r) = i'th component at r' due to j-oriented source at r
!    Thus the second operator form relates the observed field at r'
!    to the source at r in the more expected way

!    Although the Gij are computed initially as the received field at r due to
!    a source at r', the expression above requires that the Gij are to be used
!    in a system centred at the receiver point rather than the spource point.
!    This affects only the cross terms with z.  Gxz(r',r) = -Gxz(r,r')
!    Gzx(r',r) = -Gxz(r',r)
!
!    Thus in what follows, the integration over r is now referred to as the
!    integration over the source cell rather than the receiver cell due to the
!    use of the adjoint form.
!
!  Weidelt's solution for the induced scattering current Js(r') requies that
!  the integrated Green's functions be computed in the form
!
!  Gaa = Saa + Haa /Ksq_bas;  Gab = Sab + Hab /Ksq_bas;
!                             Gba = Sba + Hba /Ksq_bas  &
!  Gbb = Sbb + Hbb /Ksq_bas   where  Ksq_bas :=  i * omega * mu / res(basement).
!
!  Saa, Sab=Sba, & Sbb are the induced (divergence free) part of the GTE,
!  corresponding to currents enclosed within the plate.
!  Haa, Hab, Hba, & Hbb are the curl-free or current channelling part
!  of the GTE which are closed outside the plate.
!
!    OUTPUT:
!   The inoitial "reversed" Green's functions are expressed as:
!
!  VAAI (JCR,JCS) - relate the along strike component of the electric field at
!  SAAI (JCR,JCS)   cell JCR to the along strike component of the scattering
!                   current of cell JCS
!
!  VBAI (JCR,JCS) - relate the down dip component of the electric field at
!  SBAI (JCR,JCS)   cell JCR due to the along strike component of the
!                   scattering current of cell JCS
!
!  VABI (JCR,JCS) - relate the along strike component of the electric field at
!  SABI (JCR,JCS)   cell JCR due to the down dip component of the scattering
!                   current of cell JCS
!
!  VBBI (JCR,JCS) - relate the down dip component of the electric field at
!  SBBI (JCR,JCS)   cell JCR due to the down dip component of the scattering
!                   current of cell JCS of plate JPS
!
!  the  OUTPUT is  Sij and converts Hij such that Hij -> Hij + Ksq_bas * Sij
!
!  the forms used by the subroutine SCAT_MTRX_BUILD
!
!  Symmetry is used to reduce computation.  If the source is in row JB and the
!  receiver point in row KB, the GTE will be function of horizontal separation
!  (translation independent).  In what follows, KB and JB are the row indices
!  of the receiver and source cells respectively.  JA is a column index
!  denoting horizontal separation.
!
!  Thus, SAB(KB,JB,JA) is the electric field in the XI-oriented (along-strike)
!  field at row KB, due to a ETA (down dip)) oriented electric dipole current
!  integrated over a cell in row JB, column spearation JA.
!  SAA is the XI electric field due to a XI-oriented dipole.
!  SBB is the ETA electric field caused by an ETA-oriented electric dipole.
!  Similarly for VAA, VAB, VBA, & VBB.
!
!  In what follows, XI is the coordinate along strike which is defined as a
!  clockwise rotation (East of North) from the X (North) axis).
!  ETA is the coordinate down dip.  ETA = X*CDIP + Z*SDIP.
!  The "reeiver cell" has cell numbers (KB,1) in the (ETA,XI) direction.
!  The "source cell" has cell numbers (JB,JA) in the (ETA,XI) direction.
!  Integration is over the source cell.
!
!  NAL & NBL are the number of cells along strike and down dip respectively.
!  DAL & DBL are the corresponding lengths of the cells.
!  PLTOP is the distance to the top of the plate and (CDIP, SDIP) refer
!  to the dip.
!  A vertical body has these as (0,1) and a flat lying plate as (1,0)
!  VBBT * VABT are temporary storage variables.
!
! -------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18), NGL=5
 COMPLEX, PARAMETER :: ZERO=(0.,0.)
 INTEGER NAL,NBL,NAB,NREGT,NLYR,SXLYR,RXLYR,KFG,J1,J2,KB,KA,JB,JA,JB_KB,JA_KA,JAB,KAB
 REAL, DIMENSION(4,NREGT) :: GR1,GI1,GR2,GI2,GR3,GI3,GR4,GI4,GR5,GI5,GR6,GI6,GR7,GI7
 REAL, DIMENSION(NGL) :: GLX,GLW,WGHTA,WGHTB,XI_GL,ETA_GL(NGL)
 REAL RHOTRP(NREGT),DAL,DALH,DBL,ZC1L(NBL),CDP,SDP,CAZ,SAZ,X,XC,XGL,Y,YC,YGL, &
      ZGL,RHO,XBAR,XBARSQ,YBAR,YBARSQ,R23(2,3),R32(3,2)
 REAL, DIMENSION(NAB) :: XCEL1,YCEL1
 REAL (KIND=QL) RMUD(0:NLYR),THKD(NLYR),DPTHL(NLYR),ZR,ZS
 COMPLEX, DIMENSION(NBL,NAL) :: SAAD,VAAD,VABD,VBBD
 COMPLEX, DIMENSION(NBL,NBL,NAL,NAL) :: SAA,SBA,SBB,VAA,VAB,VBA,VBB
 COMPLEX GAB(2,2),VAB22(2,2),SAB22(2,2),GXYZ(3,3),RTMP(3,2)
 COMPLEX KSQN,KBASE,AAS,AAV,ABV,BBV,GC1,GC2,GC3,GC4,GC5,GC6,GC7
 COMPLEX(KIND=QL) KSQL(NLYR),SIGL(NLYR)

 DATA GLX / -.9061798, -.5384693,    0.,    .5384693, .9061798/
 DATA GLW /  .2369269,  .4786287, .5688888, .4786287, .2369269/

 SAA=ZERO; SBA=ZERO; SBB=ZERO; VAA=ZERO; VAB=ZERO; VBA=ZERO; VBB=ZERO;

! Set up propagation constants, integration weights, and interpolation range for
! Gauss-Legendre integration in ETA direction at density of 1/5 modified skin depth.

 IF (RXLYR == NLYR) THEN
   WRITE(4,1); WRITE(*,1)
   STOP
 END IF
 SXLYR = RXLYR
 KFG = 211

 CAZ = 1.
 SAZ = 0.
 CALL RXYZ2PLT (CAZ,SAZ,CDP,SDP,1.,0.,R23)
 CALL RPLT2XYZ (CAZ,SAZ,CDP,SDP,1.,0.,R32)

 ETA_GL(1:NGL) = GLX(1:NGL) * DBL /2.
 DALH = DAL / 2.
 WGHTB = GLW * DBL /2.                         ! Set weight array for cells in basement

! Compute the direct (or full space) Green's tensor elements over the receiver
! cell (JB,JA) for a tangential unit source dipole in cell (1,1).  Because it
! depends only on the intercell distance and not plate position, it can be
! computed directly in plate system, requiring no rotation.

 KSQN = CMPLX (KSQL(RXLYR))
 KBASE = SQRT (KSQN)                        !  Direct term for layer RXLYR
 IF (REAL (KBASE) < 0) KBASE = -KBASE
 DO JB = 1, NBL
   DO JA = 1,NAL
     CALL EGTDIR (JB,JA,KBASE,DAL,DBL,AAS,AAV,ABV,BBV)
     SAAD(JB,JA) = AAS
     VAAD(JB,JA) = AAV
     VBBD(JB,JA) = BBV
     VABD(JB,JA) = ABV
   END DO
 END DO

! Plunge must = 0 for this subroutine to be called.
! Index cell depth as a function of row number.

!  ETA, ETAS, ETAR refer to distances down dip from designated top edge of plate.
!  They are in the "plate" coordinate system and are independent of dip & rotation.
!  Similarly, XI refers to distances along strike in the "plate" coordinate system.

 DO KB = 1, NBL                                  ! receiver cell row loop
   ZR = ZC1L(KB)
   DO JB = KB, NBL                               ! source cell row loop
     DO J1 = 1, NGL
         YGL = ETA_GL(J1) * CDP
         ZGL = ETA_GL(J1) * SDP
         ZS = ZC1L(JB) + ZGL
         CALL EGT_UL_CSPL (NREGT,RHOTRP,SXLYR,RXLYR,KFG,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,ZS,ZR, &
                           GR1,GI1,GR2,GI2,GR3,GI3,GR4,GI4,GR5,GI5,GR6,GI6,GR7,GI7)
       KA = 1
       KAB = KA + (KB-1)* NAL
       JAB = KA + (JB-1)* NAL
       XC = XCEL1(JAB) - XCEL1(KAB)
       YC = YCEL1(JAB) - YCEL1(KAB)

       Y = YC + YGL

       XI_GL = (GLX + 1.) * DALH / 2.
       WGHTA = GLW * DAL /4.  ! Set weight array

       SAB22 = ZERO;  VAB22 = ZERO
       DO J2 = 1,NGL
         XGL =  XI_GL(J2)
         X = XC  + XGL

         RHO = SQRT (X**2 + Y**2)
         XBAR = X / RHO
         XBARSQ = XBAR**2
         YBAR = Y / RHO
         YBARSQ = YBAR**2

         CALL CCUBVAL (RHOTRP,NREGT,GR1,GI1,RHO,GC1)
         CALL CCUBVAL (RHOTRP,NREGT,GR2,GI2,RHO,GC2)
         CALL CCUBVAL (RHOTRP,NREGT,GR3,GI3,RHO,GC3)
         CALL CCUBVAL (RHOTRP,NREGT,GR4,GI4,RHO,GC4)
         CALL CCUBVAL (RHOTRP,NREGT,GR5,GI5,RHO,GC5)

         GXYZ = ZERO; GAB = ZERO

         GXYZ(1,1) = (1. - 2.* YBARSQ) * GC4 + YBARSQ * GC1        ! Sxx
         GXYZ(2,2) = (1. - 2.* XBARSQ) * GC4 + XBARSQ * GC1        ! Syy
         GXYZ(3,3) = ZERO                                          ! Szz

         RTMP = MATMUL (GXYZ, R32)
         GAB =  MATMUL (R23, RTMP)
         SAB22 = SAB22 + WGHTA(J2) * GAB

         GXYZ = ZERO; GAB = ZERO

         GXYZ(1,1) = (1. - 2.*XBARSQ) * GC5 + XBARSQ * GC2             ! Vxx
         GXYZ(2,2) = (1. - 2.*YBARSQ) * GC5 + YBARSQ * GC2             ! Vyy
         GXYZ(3,3) = -GC3                                              ! Vzz

         RTMP = MATMUL (GXYZ, R32)
         GAB =  MATMUL (R23, RTMP)
         VAB22 = VAB22 + WGHTA(J2) * GAB
       END DO

       SAA(KB,JB,KA,KA) = SAA(KB,JB,KA,KA) + 2. * WGHTB(J1) * SAB22(1,1)
       SBA(KB,JB,KA,KA) = SBA(KB,JB,KA,KA) + 2. * WGHTB(J1) * SAB22(2,1)
       SBB(KB,JB,KA,KA) = SBB(KB,JB,KA,KA) + 2. * WGHTB(J1) * SAB22(2,2)
       VAA(KB,JB,KA,KA) = VAA(KB,JB,KA,KA) + 2. * WGHTB(J1) * VAB22(1,1)
       VBB(KB,JB,KA,KA) = VBB(KB,JB,KA,KA) + 2. * WGHTB(J1) * VAB22(2,2)
       VAB(KB,JB,KA,KA) = VAB(KB,JB,KA,KA) + 2. * WGHTB(J1) * VAB22(1,2)
       VBA(KB,JB,KA,KA) = VBA(KB,JB,KA,KA) + 2. * WGHTB(J1) * VAB22(2,1)

       XI_GL = GLX * DALH
       WGHTA = GLW * DALH                      ! Set weight array
       DO JA = KA+1, NAL                       ! receiver cell column loop
         JAB = JA + (JB-1)* NAL

         XC = XCEL1(JAB) - XCEL1(KAB)
         YC = YCEL1(JAB) - YCEL1(KAB)

         Y = YC + YGL

         SAB22 = ZERO;  VAB22 = ZERO
         DO J2 = 1, NGL
           XGL =  XI_GL(J2)
           X = XC  + XGL

           RHO = SQRT (X**2 + Y**2)
           XBAR = X / RHO
           YBAR = Y / RHO
           XBARSQ = XBAR**2
           YBARSQ = YBAR**2

           CALL CCUBVAL (RHOTRP,NREGT,GR1,GI1,RHO,GC1)
           CALL CCUBVAL (RHOTRP,NREGT,GR2,GI2,RHO,GC2)
           CALL CCUBVAL (RHOTRP,NREGT,GR3,GI3,RHO,GC3)
           CALL CCUBVAL (RHOTRP,NREGT,GR4,GI4,RHO,GC4)
           CALL CCUBVAL (RHOTRP,NREGT,GR5,GI5,RHO,GC5)
           CALL CCUBVAL (RHOTRP,NREGT,GR6,GI6,RHO,GC6)
           CALL CCUBVAL (RHOTRP,NREGT,GR7,GI7,RHO,GC7)

           GXYZ = ZERO; GAB = ZERO

           GXYZ(1,1) = (1. - 2.* YBARSQ) * GC4 + YBARSQ * GC1            ! Sxx
           GXYZ(2,2) = (1. - 2.* XBARSQ) * GC4 + XBARSQ * GC1            ! Syy
           GXYZ(3,3) = ZERO                                              ! Szz
           GXYZ(1,2) = XBAR * YBAR * (2.* GC4 - GC1)                     ! Sxy
           GXYZ(2,1) = GXYZ(1,2)                                         ! Syx

           RTMP = MATMUL (GXYZ, R32)
           GAB =  MATMUL (R23, RTMP)
           SAB22 = SAB22 + WGHTA(J2) * GAB
           GXYZ = ZERO; GAB = ZERO

           GXYZ(1,1) = (1. - 2.*XBARSQ) * GC5 + XBARSQ * GC2             ! Vxx
           GXYZ(2,2) = (1. - 2.*YBARSQ) * GC5 + YBARSQ * GC2             ! Vyy
           GXYZ(3,3) = -GC3                                              ! Vzz
           GXYZ(1,2) = XBAR * YBAR * (GC2 - 2.* GC5)                     ! Vxy
           GXYZ(2,1) = GXYZ(1,2)                                         ! Vxy
           GXYZ(3,1) = XBAR * GC6                                        ! Vzx
           GXYZ(3,2) = YBAR * GC6                                        ! Vzy
           GXYZ(1,3) = XBAR * GC7                                      ! Vxz
           RTMP = MATMUL (GXYZ, R32)
           GAB =  MATMUL (R23, RTMP)
           VAB22 = VAB22 + WGHTA(J2) * GAB
         END DO

         SAA(KB,JB,KA,JA) = SAA(KB,JB,KA,JA) + WGHTB(J1) * SAB22(1,1)
         SBA(KB,JB,KA,JA) = SBA(KB,JB,KA,JA) + WGHTB(J1) * SAB22(2,1)
         SBB(KB,JB,KA,JA) = SBB(KB,JB,KA,JA) + WGHTB(J1) * SAB22(2,2)
         VAA(KB,JB,KA,JA) = VAA(KB,JB,KA,JA) + WGHTB(J1) * VAB22(1,1)
         VBB(KB,JB,KA,JA) = VBB(KB,JB,KA,JA) + WGHTB(J1) * VAB22(2,2)
         VAB(KB,JB,KA,JA) = VAB(KB,JB,KA,JA) + WGHTB(J1) * VAB22(1,2)
         VBA(KB,JB,KA,JA) = VBA(KB,JB,KA,JA) + WGHTB(J1) * VAB22(2,1)
       END DO                   ! JA Loop: 1 to NAL
     END DO                   ! J1 Loop: 1 to NGL down dip

!  Fill in KA-JA values for JA >= KA

     DO KA = 2,NAL
       DO JA = KA,NAL
         JA_KA = JA - KA + 1
         SAA(KB,JB,KA,JA) = SAA(KB,JB,1,JA_KA)
         SBB(KB,JB,KA,JA) = SBB(KB,JB,1,JA_KA)
         VAA(KB,JB,KA,JA) = VAA(KB,JB,1,JA_KA)
         VBB(KB,JB,KA,JA) = VBB(KB,JB,1,JA_KA)
         SBA(KB,JB,KA,JA) = SBA(KB,JB,1,JA_KA)
         VBA(KB,JB,KA,JA) = VBA(KB,JB,1,JA_KA)
         VAB(KB,JB,KA,JA) = VAB(KB,JB,1,JA_KA)
       END DO
     END DO

!  Add in the direct contributions if both Tx & Rx are in basement (KB > 1)
!  or if both in layer above basement (JB = KB = 1)

     JB_KB = JB - KB + 1
     DO KA = 1,NAL
       DO JA = KA,NAL
         JA_KA = JA - KA + 1

         SAA(KB,JB,KA,JA) = SAA(KB,JB,KA,JA) + SAAD(JB_KB,JA_KA)
         SBB(KB,JB,KA,JA) = SBB(KB,JB,KA,JA) + SAAD(JB_KB,JA_KA)
         VAA(KB,JB,KA,JA) = VAA(KB,JB,KA,JA) + VAAD(JB_KB,JA_KA)
         VBB(KB,JB,KA,JA) = VBB(KB,JB,KA,JA) + VBBD(JB_KB,JA_KA)
         VAB(KB,JB,KA,JA) = VAB(KB,JB,KA,JA) + VABD(JB_KB,JA_KA)
         VBA(KB,JB,KA,JA) = VBA(KB,JB,KA,JA) + VABD(JB_KB,JA_KA)

!  Use horizontal symmetry to fill in the cell row aspect of the matrix.

         SAA(KB,JB,JA,KA) =  SAA(KB,JB,KA,JA)
         SBB(KB,JB,JA,KA) =  SBB(KB,JB,KA,JA)
         VAA(KB,JB,JA,KA) =  VAA(KB,JB,KA,JA)
         VBB(KB,JB,JA,KA) =  VBB(KB,JB,KA,JA)
         SBA(KB,JB,JA,KA) = -SBA(KB,JB,KA,JA)
         VBA(KB,JB,JA,KA) = -VBA(KB,JB,KA,JA)
         VAB(KB,JB,JA,KA) = -VAB(KB,JB,KA,JA)
       END DO
     END DO
   END DO                   ! JB Loop: 1 to NBL

   DO JB = KB+1, NBL
     SAA(JB,KB,1:NAL,1:NAL) =  SAA(KB,JB,1:NAL,1:NAL)
     SBB(JB,KB,1:NAL,1:NAL) =  SAA(KB,JB,1:NAL,1:NAL)
     VAA(JB,KB,1:NAL,1:NAL) =  VAA(KB,JB,1:NAL,1:NAL)
     VBB(JB,KB,1:NAL,1:NAL) =  VBB(KB,JB,1:NAL,1:NAL)
     SBA(JB,KB,1:NAL,1:NAL) = -SBA(KB,JB,1:NAL,1:NAL)
     VAB(JB,KB,1:NAL,1:NAL) = -VBA(KB,JB,1:NAL,1:NAL)
     VBA(JB,KB,1:NAL,1:NAL) = -VAB(KB,JB,1:NAL,1:NAL)
   END DO
 END DO                   ! KB Loop: 1 to NBL

! SET UP S-V SUM AND STORE IT IN V.

 VAA = VAA + SAA * KSQN
 VAB = VAB + SBA * KSQN
 VBA = VBA + SBA * KSQN
 VBB = VBB + SBB * KSQN

 1 FORMAT (T3,' An evil spirit has entered EGT_UL_BOSS.  Seek help !')

 END SUBROUTINE EGT_UL_BOSS

 SUBROUTINE EGT_UL_CSPL (NREGT,RHOTRP,SXLYR,RXLYR,KFG,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,ZS, &
                         ZR,QR1,QI1,QR2,QI2,QR3,QI3,QR4,QI4,QR5,QI5,QR6,QI6,QR7,QI7)
!--------------------------------------------------------------------------------------------

!***  Calls EGT_HNK, CUBSPL
!***  Called by SCAT_MTRX_BOSS

! Sets up cubic spline representations, QRj & QIj (j = 1,6), for the five
! Hankel integrals needed to compute the electric Green's tensor elements
! as a function of RHOTRP.

!   RHOTRP - interpolation array of rho values
!    NREGT - number of interpolation points needed for EGT
!     NLYR - number of layers
!     THKD - array of layer thicknesses
!     RMUD - relative permeability of layers
!     SIGL - complex resistivity of layers
!     KSQL - propagation constants

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 INTEGER JR,NREGT,NLYR,SXLYR,RXLYR,KFG
 REAL, DIMENSION (4,NREGT) :: QR1,QI1,QR2,QI2,QR3,QI3,QR4,QI4,QR5,QI5,QR6,QI6,QR7,QI7
 REAL RHOTRP(NREGT)
 REAL (KIND=QL) RMUD(0:NLYR),ZS,ZR
 REAL(KIND=QL), DIMENSION (NLYR) :: THKD,DPTHL
 COMPLEX(KIND=QL) KSQL(NLYR),SIGL(NLYR),GTED(NREGT,7)

 QR1 = 0; QR2 = 0; QR3 = 0; QR4 = 0; QR5 = 0; QR6 = 0; QR7 = 0.
 QI1 = 0; QI2 = 0; QI3 = 0; QI4 = 0; QI5 = 0; QI6 = 0; QI7 = 0.

 CALL EGT_UL_HNK (NREGT,RHOTRP,SXLYR,RXLYR,KFG,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,ZS,ZR,GTED)

 DO JR = 1, NREGT
   QR1(1,JR) = REAL (GTED(JR,1))
   QI1(1,JR) = REAL (AIMAG (GTED(JR,1)) )
   QR2(1,JR) = REAL (GTED(JR,2))
   QI2(1,JR) = REAL (AIMAG (GTED(JR,2)) )
   QR3(1,JR) = REAL (GTED(JR,3))
   QI3(1,JR) = REAL (AIMAG (GTED(JR,3)) )
   QR4(1,JR) = REAL (GTED(JR,4))
   QI4(1,JR) = REAL (AIMAG (GTED(JR,4)) )
   QR5(1,JR) = REAL (GTED(JR,5))
   QI5(1,JR) = REAL (AIMAG (GTED(JR,5)) )
   QR6(1,JR) = REAL (GTED(JR,6))
   QI6(1,JR) = REAL (AIMAG (GTED(JR,6)) )
   QR7(1,JR) = REAL (GTED(JR,7))
   QI7(1,JR) = REAL (AIMAG (GTED(JR,7)) )
 END DO

 CALL CUBSPL (RHOTRP,QR1,NREGT)
 CALL CUBSPL (RHOTRP,QI1,NREGT)
 CALL CUBSPL (RHOTRP,QR2,NREGT)
 CALL CUBSPL (RHOTRP,QI2,NREGT)
 CALL CUBSPL (RHOTRP,QR3,NREGT)
 CALL CUBSPL (RHOTRP,QI3,NREGT)
 CALL CUBSPL (RHOTRP,QR4,NREGT)
 CALL CUBSPL (RHOTRP,QI4,NREGT)
 CALL CUBSPL (RHOTRP,QR5,NREGT)
 CALL CUBSPL (RHOTRP,QI5,NREGT)
 CALL CUBSPL (RHOTRP,QR6,NREGT)
 CALL CUBSPL (RHOTRP,QI6,NREGT)
 CALL CUBSPL (RHOTRP,QR7,NREGT)
 CALL CUBSPL (RHOTRP,QI7,NREGT)

 END SUBROUTINE EGT_UL_CSPL

 SUBROUTINE EGT_UL_HNK (NREGT,RHOTRP,SXLYR,RXLYR,KFG,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,ZS,ZR,GTED)
!------------------------------------------------------------------------------------------------

!***  Calls EGT_UL_KER
!***  Called by PRM_BOSS_LP

!  Computes the integrals for the electric Green's tensor elements when the top
!  row of the plate is in the layer above basement.  (RXLYR set to NLYR - 1)
!  It uses the flow through Hankel transform method to evaluate the three Hankel
!  integrals at discrete depths.  The Hankel transform integral uses a 15 point
!  per decade filter coefficient set derived from Christensen's FLTGEN program.

!       NREGT - number of horizontal interpolation points
!      RHOTRP - 15 points per decade interpolation array
!         KFG - Case selector variable
!       RXLYR - "receiver" layer (NLYR or NLYR-1)
!       SXLYR - "source" layer (NLYR or NLYR-1)
!        NLYR - number of layers including basement. (1 or 2 only)
!        THKD - thickness of each layer above basement
!       DPTHL - depth to top of each layer including basement
!        RMUD - relative magetic permeability
!        SIGL - complex xconductivity for all layers including Cole-Cole & dielectric
!        KSQL - iwu * SIGL for all layers (includes Cole-Cole)
!          ZS - transmitter point depth
!          ZR - receiver point depth
!
!  GTED(JR,7) - integrals for Green's tensor elements
!

 USE LG_Filter_Coefficients

 IMPLICIT NONE
 REAL(KIND=QL), PARAMETER :: FOURPI = 12.56637061_QL
 COMPLEX(KIND=QL), PARAMETER :: ZERO=(0._QL,0._QL)
 INTEGER L,LMAX,LMIN,K,KMAX,KMIN,KBOT,K1,JR,NLYR,SXLYR,RXLYR,KFG,NREGT
 REAL RHOTRP(NREGT)
 REAL(KIND=QL) RMUD(0:NLYR),ZS,ZR,DELTA,Y1,Y,RD,LMBDA,RHOD
 REAL(KIND=QL), DIMENSION (NLYR) :: THKD,DPTHL
 COMPLEX(KIND=QL) GTED(NREGT,7),KER(JNLO-NREGT:JNHI,7)
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL
 LOGICAL JUMP

 GTED = ZERO
 KER = ZERO
 DELTA = LOG (10.D0)/ DBLE (NDEC_JN)

! Basic integral for open and closed loops.
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
   LMBDA = EXP (SNGL(Y))
   CALL EGT_UL_KER (NREGT,K,JR,L,LMBDA,SXLYR,RXLYR,KFG,NLYR,THKD, &
                       DPTHL,RMUD,SIGL,KSQL,ZS,ZR,KER,GTED,JUMP)
   IF (JUMP) EXIT
   END DO

 Y = Y1                       ! Finish off the low end for RHOTRP(1)
 DO L = -51, JNLO, -1
   LMIN = L                   ! Minimum filter index used
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
   K = L + 1 - JR             ! Compute the kernel index.
   CALL EGT_UL_KER (NREGT,K,JR,L,LMBDA,SXLYR,RXLYR,KFG,NLYR,THKD, &
                       DPTHL,RMUD,SIGL,KSQL,ZS,ZR,KER,GTED,JUMP)
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

   GTED(JR,1:3) =  GTED(JR,1:3) + KER(K,1:3) * WJ0(L)
   GTED(JR,4:7) =  GTED(JR,4:7) + KER(K,4:7) * WJ1(L)
 END DO

 IF (K1 > KBOT) THEN    !  Add low end kernel values until convergence.
   DO K = K1-1, KBOT, -1
     KMIN = K
     L = K - 1 + JR
     Y = Y1 + DBLE (L) * DELTA
     LMBDA = EXP (SNGL(Y))
     CALL EGT_UL_KER (NREGT,K,JR,L,LMBDA,SXLYR,RXLYR,KFG,NLYR,THKD, &
                         DPTHL,RMUD,SIGL,KSQL,ZS,ZR,KER,GTED,JUMP)
     IF (JUMP) EXIT
   END DO
 END IF

 DO JR = 2, NREGT-1          !  Compute transforms for all other RHO values
   DO K = KMIN, KMAX         !  using previously computed kernel values.
     L = K - 1 + JR
     GTED(JR,1:3) =  GTED(JR,1:3) + KER(K,1:3) * WJ0(L)
     GTED(JR,4:7) =  GTED(JR,4:7) + KER(K,4:7) * WJ1(L)
   END DO
 END DO

 DO JR = 1, NREGT
   RHOD = REAL (RHOTRP(JR),KIND=QL)
   GTED(JR,1:7) = GTED(JR,1:7) / (FOURPI * RHOD)
   GTED(JR,4:5) = GTED(JR,4:5) / RHOD
 END DO

 END SUBROUTINE EGT_UL_HNK

 SUBROUTINE EGT_UL_KER (NREGT,K,JR,L,LMBDA,SXLYR,RXLYR,KFG,NLYR,THKD, &
                           DPTHL,RMUD,SIGL,KSQL,ZS,ZR,KER,GTED,JUMP)
!----------------------------------------------------------------------

!***  Called by EGT_UL_HNK_

!  Compute the kernels for EGT for plates that intrude above basement
!  The receiver is the row of cells above basement.

!     NREGT - number of points in 15 point / decade array
!         K - filter kernel index
!        JR - RHO interpolation index
!         L - filter point index
!     LMBDA - Hankel transform variable
!        ZS - source location  (negative in air, positive in ground)
!      NLYR - number of layers including basement. (1 or 2 only)
!      SIGL - complex xconductivity for all layers including Cole-Cole & dielectric
!      KSQL - iwu * SIGL for all layers (includes Cole-Cole)
!      RMUD - relative magetic permeability
!      THKD - thickness of each layer above basement
!     DPTHL - depth to top of each layer including basement
!       KFG - Case selector variable
!       KER - kernels for hankel integraion
!
!   GTED(*,1,*) - needed for both closed and open loops
!   GTED(*,2,*) - used only for grounded wire segments
!   GTED(*,3,*) - used only for grounded wire segments
!
!           JUMP - keep integrating if false.

 USE LG_Filter_Coefficients
 IMPLICIT NONE
 REAL, PARAMETER :: TOL=1.E-5
 COMPLEX(KIND=QL), PARAMETER :: ZERO=(0._QL,0._QL)
 INTEGER JR,L,K,NLYR,SXLYR,RXLYR,KFG,NREGT,JI
 REAL(KIND=QL) RMUD(0:NLYR),LMBDA,LMBSQ,ZS,ZR,AR,AI,GTEDR,GTEDI
 REAL(KIND=QL), DIMENSION (NLYR) :: THKD,DPTHL
 COMPLEX (KIND=QL) XI_V,F_V,F_H,ETA_V,G_V,G_H,KER(JNLO-NREGT:JNHI,7), &
                   S(0:NLYR),SL,SM,KV,KS,XPU,XPD,GTED(NREGT,7),TMP(7)
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL
 LOGICAL TOO_BIG,JUMP

! Compute layered-earth coefficients.

 KER(K,1:7) = ZERO

 LMBSQ = LMBDA * LMBDA
 CALL EDSX_COEF (SXLYR,RXLYR,KFG,LMBDA,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                 ZS,S,XI_V,F_V,F_H,ETA_V,G_V,G_H)
 SL = S(RXLYR)
 SM = S(SXLYR)
 XPU = EXP (SL * (ZR - DPTHL(RXLYR+1)))
 XPD = EXP (SL * (DPTHL(RXLYR) - ZR))

! Convert from squiggle to hat form.  (Green's function notes, Raiche,2008)

 XI_V = XI_V * XPU
 F_V  =  F_V * XPU
 F_H  =  F_H * XPU

 ETA_V = ETA_V * XPD
 G_V   =   G_V * XPD
 G_H   =   G_H * XPD
 TMP = ZERO

 KS = (XI_V + ETA_V) * RMUD(SXLYR) / (RMUD(RXLYR) * SM)
 KV = (F_H - G_H) * SL

 KER(K,1) = KS * LMBDA                                    ! Sxx, Sxy & Syy
 KER(K,2) = KV * LMBDA                                    ! Vxx, Vxy & Vyy
 KER(K,3) = (F_V + G_V) * LMBDA**3 / SM
 KER(K,4) = KS                          ! Sxx, Sxy & Syy
 KER(K,5) = KV                          ! Vxx, Vxy & Vyy
 KER(K,6) = (F_H + G_H) * LMBSQ
 KER(K,7) = (F_V - G_V) * LMBSQ * SL /SM
 JUMP = .TRUE.
 TMP(1:3) = KER(K,1:3) * WJ0(L)
 TMP(4:7) = KER(K,4:7) * WJ1(L)

 GTED(JR,1:7) = GTED(JR,1:7) + TMP(1:7)
 DO JI = 1,7
   AR = ABS ( REAL (TMP(JI)) )
   AI = ABS (AIMAG (TMP(JI)) )
   TOO_BIG = .FALSE.
   GTEDR = ABS ( REAL (GTED(JR,JI)) )
   GTEDI = ABS (AIMAG (GTED(JR,JI)) )
   IF (AR > TOL* GTEDR) TOO_BIG = .TRUE.
   IF (AI > TOL* GTEDI) TOO_BIG = .TRUE.
   IF (TOO_BIG) JUMP = .FALSE.
 END DO

 END SUBROUTINE EGT_UL_KER

!======================================================================================
!======================================================================================
!
!        MULTI-PLATE GREENS FUNCTION ROUTINES
!        ------------------------------------

 SUBROUTINE INTER_EGT_BOSS (JPS,JPR,MXAB,NPLT,NA,NB,DPTHB,KSQN,PLAZM,PLDIP,PLUNJ,XCELL,YCELL,ZCELL, &
                            NZ2,ZV2,NREGT,RHOTRP,GR1Z,GI1Z,GR2Z,GI2Z,GR3Z,GI3Z,GR4Z,GI4Z,GR5Z,GI5Z, &
                            GR6Z,GI6Z,VAAI,VABI,VBAI,VBBI,SAAI,SABI,SBAI,SBBI)
!-------------------------------------------------------------------------------------------------------

!***  Calls:  RXYZ2PLT, RPLT2XYZ, INTER_EGT_DIR
!***  Called by: SCAT_MTRX_BOSS

!  Computes the Green's tensor elements which give the response at plate JPR
!  due to electric currents in plate JPS.  In what follows, the GTE have the
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
!  SIJ is the divergence-free part.
!
!  In the formulation the integrated Green's tensor G relates the field at r'
!  to the current at r by
!                                           _                      _
!                                          |  Gaa(r.r')  Gab(r.r')  |
!  { Ea(r'), Eb(r') } =  { Ja(r), Jb(r) }  |                        |
!                                          |  Gba(r.r')  Gbb(r.r')  |
!                                          |_                      _|
!
!  This can be put into the more expected operator form by writing this in
!  transposed form as:
!
!   _        _       _                      _    _       _
!  |  Ea(r')  |     |  Gaa(r,r')  Gba(r,r')  |  |  Ja(r)  |
!  |          |  =  |                        |  |         |
!  |  Eb(r')  |     |  Gab(r,r')  Gbb(r,r')  |  |  Jb(r)  |
!  |_        _|     |_                      _|  |_       _|
!
!                    _                       _    _       _
!                   |   Gaa(r',r)  Gab(r',r)  |  |  Ja(r)  |
!                =  |                         |  |         |
!                   |   Gba(r',r)  Gbb(r',r)  |  |  Jb(r)  |
!                   |_                       _|  |_       _|
!
!                            _                                  _    _        _
!         _             _   |   Gxx(r',r)  Gxy(r',r)  Gxz(r',r)  |  |          |   _       _
!        |               |  |                                    |  |          |  |  Ja(r)  |
!     =  |   R23 (r')    |  |   Gyx(r',r)  Gyy(r',r)  Gyz(r',r)  |  |  R32 (r) |  |         |
!        |               |  |                                    |  |          |  |  Jb(r)  |
!        |_             _|  |   Gzx(r',r)  Gzy(r',r)  Gzz(r',r)  |  |          |  |_       _|
!                           |_                                  _|  |_        _|
!
!
!    Gij(r,r') = ith component at r due to j oriented source at r'
!    Thus the second operator form relates the observed field at r'
!    to the source at r in the more expected way BUT note that the
!    Gij are derived as the field at the source point.  This affects only
!    the cross terms with z.  Gxz(r',r) = -Gxz(r,r')
!    Gzx(r',r) = -Gxz(r',r)
!
!    We can use the second form by modifying the normaly computed GTEs
!
!  NA & NB are the number of cells along strike and down dip respectively.
!  SIG contains the complex layer conductivities.
!  KSQN = iwu * SIG(basement)
!  DPTHB is the depth to basement.
!
!    OUTPUT:
!   The adjoint Green's functions are expressed as:
!
!  VAAI (JCR,JCS) - relate the along strike component of the electric field at
!  SAAI (JCR,JCS)   cell JCR of plate JPR due to the along strike component
!                   of the scattering current of cell JCS of plate JPS
!
!  VBAI (JCR,JCS) - relate the down dip component of the electric field at
!  SBAI (JCR,JCS)   cell JCR of plate JPR due to the along strike component
!                   of the scattering current of cell JCS of plate JPS
!
!  VABI (JCR,JCS) - relate the along strike component of the electric field at
!  SABI (JCR,JCS)   cell JCR of plate JPR due to the down dip component
!                   of the scattering current of cell JCS of plate JPS
!
!  VBBI (JCR,JCS) - relate the down dip component of the electric field at
!  SBBI (JCR,JCS)   cell JCR of plate JPR due to the down dip component
!                   of the scattering current of cell JCS of plate JPS
!
! -------------------------------------------------------------------------

 IMPLICIT NONE
 COMPLEX, PARAMETER :: ZERO=(0.,0.)
 INTEGER JPS,JPR,MXAB,NPLT,NA(NPLT),NB(NPLT),NREGT,NZ2,JCS,JCR, &
         JB1,JB2,JA1,JA2
 REAL XD,YD,ZD,ZTR,RHO,XBAR,XBARSQ,YBAR,YBARSQ,DPTHB,ZV2(NZ2),R23(2,3),R32(3,2), &
      XCELL(MXAB,NPLT),YCELL(MXAB,NPLT),ZCELL(MXAB,NPLT),CD2,SD2,CD1,SD1,CP1,    &
      CP2,SP1,SP2,CA2,SA2,CA1,SA1,RHOTRP(NREGT)
 REAL, DIMENSION (NPLT) :: PLAZM,PLDIP,PLUNJ
 REAL, DIMENSION(4,NREGT,NZ2) :: GR1Z,GI1Z,GR2Z,GI2Z,GR3Z,GI3Z,GR4Z,GI4Z,GR5Z,GI5Z,GR6Z,GI6Z
 COMPLEX KSQN,KBASE,Q1,Q2,Q3,Q4,Q5,Q6
 COMPLEX SXXD,VXXD,VYYD,VZZD,VXYD,VYZD,VZXD,C2DINTRP
 COMPLEX GXYZ(3,3),GAB(2,2),RTMP(3,2)
 COMPLEX, DIMENSION(MXAB,MXAB) :: VAAI,VABI,VBAI,VBBI,SAAI,SABI,SBAI,SBBI

 CA1 = COS (PLAZM(JPS)); CA2 = COS (PLAZM(JPR)); SA1 = SIN (PLAZM(JPS)); SA2 = SIN (PLAZM(JPR))
 CD1 = COS (PLDIP(JPS)); CD2 = COS (PLDIP(JPR)); SD1 = SIN (PLDIP(JPS)); SD2 = SIN (PLDIP(JPR))
 CP1 = COS (PLUNJ(JPS)); CP2 = COS (PLUNJ(JPR)); SP1 = SIN (PLUNJ(JPS)); SP2 = SIN (PLUNJ(JPR))


 CALL RXYZ2PLT (CA2,SA2,CD2,SD2,CP2,SP2,R23)
 CALL RPLT2XYZ (CA1,SA1,CD1,SD1,CP1,SP1,R32)


 KBASE = SQRT (KSQN)
 IF (REAL (KBASE) < 0) KBASE = -KBASE

 VAAI = ZERO;  VBAI = ZERO;  VABI = ZERO;  VBBI = ZERO
 SAAI = ZERO;  SBAI = ZERO;  SABI = ZERO;  SBBI = ZERO

! Set up splines for integrals for indirect terms

 DO JB1 = 1,NB(JPS)                             ! Step over source plate depths.
   DO JA1 = 1,NA(JPS)
     JCS = JA1 + (JB1-1) * NA(JPS)
     DO JB2 = 1,NB(JPR)                           ! Step over receiver plate depths.


       DO JA2 = 1,NA(JPR)
         JCR = JA2 + (JB2-1) * NA(JPR)
         XD =  XCELL(JCR,JPR) - XCELL(JCS,JPS)
         YD =  YCELL(JCR,JPR) - YCELL(JCS,JPS)
         ZD =  ZCELL(JCR,JPR) - ZCELL(JCS,JPS)
         ZTR = ZCELL(JCR,JPR) + ZCELL(JCS,JPS) - 2. * DPTHB
         RHO = SQRT (XD**2 + YD**2)
         XBAR = 0.;  YBAR = 0.
         IF (RHO > 1.E-6) THEN
           XBAR = XD / RHO
           YBAR = YD / RHO
         END IF
         XBARSQ = XBAR**2
         YBARSQ = YBAR**2

! Get direct terms for each cell pair.

         CALL INTER_EGT_DIR (XD,YD,ZD,KBASE,KSQN,SXXD,VXXD,VYYD,VZZD,VXYD,VYZD,VZXD)

! Evaluate the integrals and reflected terms for each cell pair

         Q1 = C2DINTRP (RHOTRP,NREGT,ZV2,NZ2,GR1Z,GI1Z,RHO,ZTR)
         Q2 = C2DINTRP (RHOTRP,NREGT,ZV2,NZ2,GR2Z,GI2Z,RHO,ZTR)
         Q3 = C2DINTRP (RHOTRP,NREGT,ZV2,NZ2,GR3Z,GI3Z,RHO,ZTR)
         Q4 = C2DINTRP (RHOTRP,NREGT,ZV2,NZ2,GR4Z,GI4Z,RHO,ZTR)
         Q5 = C2DINTRP (RHOTRP,NREGT,ZV2,NZ2,GR5Z,GI5Z,RHO,ZTR)
         Q6 = C2DINTRP (RHOTRP,NREGT,ZV2,NZ2,GR6Z,GI6Z,RHO,ZTR)

! Compute the divergence free indirect terms, add direct terms and
! rotate them into the plate system.

         GXYZ = ZERO
         GXYZ(1,1) = SXXD + (1. - 2.*YBARSQ)  * Q4 + YBARSQ * Q1
         GXYZ(2,2) = SXXD + (1. - 2.*XBARSQ)  * Q4 + XBARSQ * Q1
         GXYZ(3,3) = SXXD
         GXYZ(1,2) = XBAR * YBAR * (2.* Q4 - Q1)
         GXYZ(2,1) = GXYZ(1,2)

         RTMP = MATMUL (GXYZ, R32)
         GAB =  MATMUL (R23, RTMP)

         SAAI(JCR,JCS) = GAB(1,1)
         SABI(JCR,JCS) = GAB(1,2)
         SBAI(JCR,JCS) = GAB(2,1)
         SBBI(JCR,JCS) = GAB(2,2)

! Compute the curl free indirect terms,  add direct terms and
! rotate them into the plate system.

         GXYZ(1,1) = VXXD + KSQN * GXYZ(1,1) + (1. - 2.*XBARSQ) * Q5 + XBARSQ * Q2
         GXYZ(2,2) = VYYD + KSQN * GXYZ(2,2) + (1. - 2.*YBARSQ) * Q5 + YBARSQ * Q2
         GXYZ(3,3) = VZZD + KSQN * GXYZ(3,3) - Q3
         GXYZ(1,2) = VXYD + KSQN * GXYZ(1,2) + XBAR * YBAR * (Q2 - 2.* Q5)
         GXYZ(2,1) = GXYZ(1,2)
         GXYZ(3,1) = VZXD + XBAR * Q6
         GXYZ(1,3) = VZXD - XBAR * Q6
         GXYZ(3,2) = VYZD + YBAR * Q6
         GXYZ(2,3) = VYZD - YBAR * Q6

         RTMP = MATMUL (GXYZ, R32)
         GAB =  MATMUL (R23, RTMP)

         VAAI(JCR,JCS) = GAB(1,1)
         VABI(JCR,JCS) = GAB(1,2)
         VBAI(JCR,JCS) = GAB(2,1)
         VBBI(JCR,JCS) = GAB(2,2)

       END DO   ! receiver plate columns
     END DO     ! source plate columns
   END DO       ! receiver plate rows
 END DO         ! source plate rows

 END SUBROUTINE INTER_EGT_BOSS

 SUBROUTINE INTER_EGT_BOSS_UL (JPS,JPR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,NPLT,PLYR,MXAB,  &
                               NA,NB,PLAZM,PLDIP,PLUNJ,XCELL,YCELL,ZCELL,NREGT,RHOTRP, &
                               VAAI,VABI,VBAI,VBBI,SAAI,SABI,SBAI,SBBI)
!-------------------------------------------------------------------------------------

!***  Calls:  RXYZ2PLT, RPLT2XYZ, INTER_EGT_DIR
!***  Called by: SCAT_MTRX_BOSS

!  Computes the Green's tensor elements which give the response at plate JPR
!  due to electric currents in plate JPS.  In what follows, the GTE have the
!  units of electric field divided by a factor (-iwu) because this factor is
!  explicitly included as a multiplier outside the integral over area when
!  solving the integral equation.
!
!  This formulation uses a different breakup of GIJ = SIJ + HIJ / KSQ from
!  that of EGT_BOSS.  Two conventions of EGT_BOSS are used.  Firstly, HIJ
!  is redefined to be HIJ = HIJ' + KSQ * SIJ is output where HIJ' is the
!  curl free part and SIJ is the divergence-free part.
!
!  In the formulation the integrated Green's tensor G relates the field at r'
!  to the current at r by
!                                           _                      _
!                                          |  Gaa(r.r')  Gab(r.r')  |
!  { Ea(r'), Eb(r') } =  { Ja(r), Jb(r) }  |                        |
!                                          |  Gba(r.r')  Gbb(r.r')  |
!                                          |_                      _|
!
!  This can be put into the more expected operator form by writing this in
!  transposed form as:
!
!   _        _       _                      _    _       _
!  |  Ea(r')  |     |  Gaa(r,r')  Gba(r,r')  |  |  Ja(r)  |
!  |          |  =  |                        |  |         |
!  |  Eb(r')  |     |  Gab(r,r')  Gbb(r,r')  |  |  Jb(r)  |
!  |_        _|     |_                      _|  |_       _|
!
!                    _                       _    _       _
!                   |   Gaa(r',r)  Gab(r',r)  |  |  Ja(r)  |
!                =  |                         |  |         |
!                   |   Gba(r',r)  Gbb(r',r)  |  |  Jb(r)  |
!                   |_                       _|  |_       _|
!
!                            _                                  _    _        _
!         _             _   |   Gxx(r',r)  Gxy(r',r)  Gxz(r',r)  |  |          |   _       _
!        |               |  |                                    |  |          |  |  Ja(r)  |
!     =  |   R23 (r')    |  |   Gyx(r',r)  Gyy(r',r)  Gyz(r',r)  |  |  R32 (r) |  |         |
!        |               |  |                                    |  |          |  |  Jb(r)  |
!        |_             _|  |   Gzx(r',r)  Gzy(r',r)  Gzz(r',r)  |  |          |  |_       _|
!                           |_                                  _|  |_        _|
!
!
!    Gij(r,r') = ith component at r due to j oriented source at r'
!    Thus the second operator form relates the observed field at r'
!    to the source at r in the more expected way BUT note that the
!    Gij are derived as the field at the source point.  This affects only
!    the cross terms with z.  Gxz(r',r) = -Gxz(r,r')
!    Gzx(r',r) = -Gxz(r',r)
!
!    We can use the second form by modifying the normaly computed GTEs
!
!  NA & NB are the number of cells along strike and down dip respectively.
!  SIG contains the complex layer conductivities.
!  KSQN = iwu * SIG(basement)
!
!    OUTPUT:
!   The adjoint Green's functions are expressed as:
!
!  VAAI (JCR,JCS) - relate the along strike component of the electric field at
!  SAAI (JCR,JCS)   cell JCR of plate JPR due to the along strike component
!                   of the scattering current of cell JCS of plate JPS
!
!  VBAI (JCR,JCS) - relate the down dip component of the electric field at
!  SBAI (JCR,JCS)   cell JCR of plate JPR due to the along strike component
!                   of the scattering current of cell JCS of plate JPS
!
!  VABI (JCR,JCS) - relate the along strike component of the electric field at
!  SABI (JCR,JCS)   cell JCR of plate JPR due to the down dip component
!                   of the scattering current of cell JCS of plate JPS
!
!  VBBI (JCR,JCS) - relate the down dip component of the electric field at
!  SBBI (JCR,JCS)   cell JCR of plate JPR due to the down dip component
!                   of the scattering current of cell JCS of plate JPS
!
! -------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 COMPLEX, PARAMETER :: ZERO=(0.,0.)
 INTEGER NLYR,SXLYR,RXLYR,KFG,JPS,JPR,MXAB,NPLT,NREGT, &
         JCS,JCR,JB1,JB2,JA1,JA2,JABR,JABS,I1
 INTEGER, DIMENSION (NPLT) :: NA,NB,PLYR
 REAL XD,YD,ZD,RHO,XBAR,XBARSQ,YBAR,YBARSQ,R23(2,3),R32(3,2), &
      XCELL(MXAB,NPLT),YCELL(MXAB,NPLT),ZCELL(MXAB,NPLT),CD2,SD2,CD1,SD1,CP1,    &
      CP2,SP1,SP2,CA2,SA2,CA1,SA1,RHOTRP(NREGT)
 REAL, DIMENSION (NPLT) :: PLAZM,PLDIP,PLUNJ
 REAL, DIMENSION(4,NREGT) :: QR1,QI1,QR2,QI2,QR3,QI3,QR4,QI4,QR5,QI5,QR6,QI6,QR7,QI7
 REAL (KIND=QL) RMUD(0:NLYR),THKD(NLYR),DPTHL(NLYR),ZR,ZS
 COMPLEX KSQN,KBASE,Q1,Q2,Q3,Q4,Q5,Q6,Q7
 COMPLEX SXXD,VXXD,VYYD,VZZD,VXYD,VYZD,VZXD
 COMPLEX GXYZ(3,3),GAB(2,2),RTMP(3,2)
 COMPLEX, DIMENSION(MXAB,MXAB) :: VAAI,VABI,VBAI,VBBI,SAAI,SABI,SBAI,SBBI
 COMPLEX(KIND=QL) KSQL(NLYR),SIGL(NLYR)

 CA1 = COS (PLAZM(JPS)); CA2 = COS (PLAZM(JPR)); SA1 = SIN (PLAZM(JPS)); SA2 = SIN (PLAZM(JPR))
 CD1 = COS (PLDIP(JPS)); CD2 = COS (PLDIP(JPR)); SD1 = SIN (PLDIP(JPS)); SD2 = SIN (PLDIP(JPR))
 CP1 = COS (PLUNJ(JPS)); CP2 = COS (PLUNJ(JPR)); SP1 = SIN (PLUNJ(JPS)); SP2 = SIN (PLUNJ(JPR))

 CALL RXYZ2PLT (CA2,SA2,CD2,SD2,CP2,SP2,R23)
 CALL RPLT2XYZ (CA1,SA1,CD1,SD1,CP1,SP1,R32)

 VAAI = ZERO;  VBAI = ZERO;  VABI = ZERO;  VBBI = ZERO
 SAAI = ZERO;  SBAI = ZERO;  SABI = ZERO;  SBBI = ZERO

 RXLYR = PLYR(JPR)
 SXLYR = PLYR(JPS)
 I1 = 2
 CALL SET_KFG (I1,NLYR,SXLYR,RXLYR,KFG)

 KSQN = CMPLX (KSQL(RXLYR))
 KBASE = SQRT (KSQN)
 IF (REAL (KBASE) < 0) KBASE = -KBASE

 DO JB1 = 1,NB(JPS)                             ! Step over source plate depths.
   JABS = 1 + (JB1 -1) * NA(JPS)
   ZS = ZCELL(JABS,JPS)                          ! depth of row JB1
   DO JB2 = 1,NB(JPR)                           ! Step over receiver plate depths.
     JABR = 1 + (JB2 -1) * NA(JPR)
     ZR = ZCELL(JABR,JPR)                        ! depth of row JB1 cell centre

     CALL EGT_UL_CSPL (NREGT,RHOTRP,SXLYR,RXLYR,KFG,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,ZS, &
                       ZR,QR1,QI1,QR2,QI2,QR3,QI3,QR4,QI4,QR5,QI5,QR6,QI6,QR7,QI7)

! Set up splines for integrals for indirect terms

     DO JA1 = 1,NA(JPS)
       JCS = JA1 + (JB1-1) * NA(JPS)
       DO JA2 = 1,NA(JPR)
         JCR = JA2 + (JB2-1) * NA(JPR)
         XD =  XCELL(JCR,JPR) - XCELL(JCS,JPS)
         YD =  YCELL(JCR,JPR) - YCELL(JCS,JPS)
         ZD =  ZCELL(JCR,JPR) - ZCELL(JCS,JPS)
         RHO = SQRT (XD**2 + YD**2)
         XBAR = 0.;  YBAR = 0.
         IF (RHO > 1.E-6) THEN
           XBAR = XD / RHO
           YBAR = YD / RHO
         END IF
         XBARSQ = XBAR**2
         YBARSQ = YBAR**2

! Get direct terms for each cell pair that are in the same layer.

         SXXD = ZERO
         VXXD = ZERO
         VYYD = ZERO
         VZZD = ZERO
         VXYD = ZERO
         VYZD = ZERO
         VZXD = ZERO
         IF (RXLYR == SXLYR) &
           CALL INTER_EGT_DIR (XD,YD,ZD,KBASE,KSQN,SXXD,VXXD,VYYD,VZZD,VXYD,VYZD,VZXD)

! Evaluate the integrals and reflected terms for each cell pair

         CALL CCUBVAL (RHOTRP,NREGT,QR1,QI1,RHO,Q1)
         CALL CCUBVAL (RHOTRP,NREGT,QR2,QI2,RHO,Q2)
         CALL CCUBVAL (RHOTRP,NREGT,QR3,QI3,RHO,Q3)
         CALL CCUBVAL (RHOTRP,NREGT,QR4,QI4,RHO,Q4)
         CALL CCUBVAL (RHOTRP,NREGT,QR5,QI5,RHO,Q5)
         CALL CCUBVAL (RHOTRP,NREGT,QR6,QI6,RHO,Q6)
         CALL CCUBVAL (RHOTRP,NREGT,QR7,QI7,RHO,Q7)

! Compute the divergence free indirect terms, add direct terms and
! rotate them into the plate system.

         GXYZ = ZERO
         GXYZ(1,1) = SXXD + (1. - 2.*YBARSQ) * Q4 + YBARSQ * Q1
         GXYZ(2,2) = SXXD + (1. - 2.*XBARSQ) * Q4 + XBARSQ * Q1
         GXYZ(3,3) = SXXD
         GXYZ(1,2) = XBAR * YBAR * (2.* Q4 - Q1)
         GXYZ(2,1) = GXYZ(1,2)

         RTMP = MATMUL (GXYZ, R32)
         GAB =  MATMUL (R23, RTMP)

         SAAI(JCR,JCS) = GAB(1,1)
         SABI(JCR,JCS) = GAB(1,2)
         SBAI(JCR,JCS) = GAB(2,1)
         SBBI(JCR,JCS) = GAB(2,2)

! Compute the curl free indirect terms,  add direct terms and
! rotate them into the plate system.

         GXYZ(1,1) = VXXD + KSQN * GXYZ(1,1) + (1. - 2.*XBARSQ) * Q5 + XBARSQ * Q2
         GXYZ(2,2) = VYYD + KSQN * GXYZ(2,2) + (1. - 2.*YBARSQ) * Q5 + YBARSQ * Q2
         GXYZ(3,3) = VZZD + KSQN * GXYZ(3,3) - Q3
         GXYZ(1,2) = VXYD + KSQN * GXYZ(1,2) + XBAR * YBAR * (Q2 - 2.* Q5)
         GXYZ(2,1) = GXYZ(1,2)
         GXYZ(3,1) = VZXD + XBAR * Q6
         GXYZ(3,2) = VYZD + YBAR * Q6
         GXYZ(1,3) = VZXD + XBAR * Q7
         GXYZ(2,3) = VYZD + YBAR * Q7

         RTMP = MATMUL (GXYZ, R32)
         GAB =  MATMUL (R23, RTMP)

         VAAI(JCR,JCS) = GAB(1,1)
         VABI(JCR,JCS) = GAB(1,2)
         VBAI(JCR,JCS) = GAB(2,1)
         VBBI(JCR,JCS) = GAB(2,2)

       END DO   ! receiver plate columns
     END DO     ! source plate columns
   END DO       ! receiver plate rows
 END DO         ! source plate rows

 END SUBROUTINE INTER_EGT_BOSS_UL

 SUBROUTINE INTER_EGT_DIR (XD,YD,ZD,KBASE,KSQN,SXXD,HXXD,HYYD,HZZD,HXYD,HYZD,HZXD)
!---------------------------------------------------------------------------------

!***  Called by INTER_EGT_BOSS

!   Computes the direct (or full space) Green's tensor elements:
!
!  XD, YD, ZD - receiver coordinates relative to source dipole
!        KSQN - i omega mu sigma (basement)
!       KBASE - sqrt (KSQN)
!
!   Leroi uses an integral equation formulation where an explicit factor (-iwu)
!   appears outside the integral.  Thus the Green's tensor elements, GIJ are in
!   units of electric field divided by this factor.  Moreover, these modified
!   GIJ can be expressed as the sum SIJ + HIJ / KSQN, where SIJ and HIJ are the
!   divergence free and curl free parts respectively.  This is done for reasons
!   explained in EGT_BOSS and SCAT_MTRX_BUILD.
!   This subroutine returns SIJ and HIJ.  It does NOT return HIJ in the form
!   HIJ:= KSQN * GIJ = KSQN * SIJ + HIJ
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
 COMPLEX KBASE,KSQN,K2R,FAC1,FAC2,BASE,SXXD,HXXD,HYYD,HZZD,HXYD,HYZD,HZXD

 INTENT (IN) XD,YD,ZD,KBASE,KSQN
 INTENT (OUT) SXXD,HXXD,HYYD,HZZD,HXYD,HYZD,HZXD

 RSQ = XD**2 + YD**2 + ZD**2
 R = SQRT (RSQ)
 K2R = KBASE * R

 SXXD = EXP (-K2R) / (4. * 3.141592654 * R)

 BASE = SXXD / RSQ
 FAC1 = ONE + K2R
 FAC2 = BASE * (3.*FAC1 + KSQN*RSQ) /RSQ
 FAC1 = FAC1 * BASE

 HXXD = FAC1 - FAC2 * XD**2
 HYYD = FAC1 - FAC2 * YD**2
 HZZD = FAC1 - FAC2 * ZD**2

 HXYD = -FAC2 * XD * YD
 HYZD = -FAC2 * YD * ZD
 HZXD = -FAC2 * XD * ZD

 END SUBROUTINE INTER_EGT_DIR

 SUBROUTINE MGT_BOSS (NPLT,PLYR,MXAB,NA,NB,PLAZM,PLDIP,PLUNJ,XCELL,YCELL,ZCELL,NLYR,THKD, &
                      DPTHL,RMUD,SIGL,KSQL,NTX,MRXTX,NRXTX,RXID,MXRS,NRS,XRS,YRS,ZRXTX,   &
                      WTRS,NRMGT,RHOTRP,NZ1,ZV1,NMGP,XMG,YMG,ZMG,WMG,HA,HB)
!----------------------------------------------------------------------------------------

!***  Called by SCAT_EM
!***  Calls MGT_CSPL

!  This rutine is used for basement plates.  It is based on interpolated Green's
!  that can be used for all plates in the basement simultaneously.
!
!  Integrated magnetic Green's tensor computation for relating the along strike
!  and down dip components of the scattering current for each plate to the
!  X,Y,Z magnetic fields at each receiver of each transmitter.  For loop receivers
!  this is done by subnets.  A 9 point per cell rule with uniform weights is used.
!
!                        INPUT
!                        -----
!
!                 NPLT - Number of plates
!                 PLYR - layer containing plate
!                 MXAB - Number of cells in biggest plate
!               NA, NB - number of cells along strike and down dip respectively
!                PLAZM - plate strike angles
!                PLDIP - plate dips
!                PLUNJ - rotation
!                  WMG - integration weights for each plate
!                 NMGP - number of integration points per cell
!        XMG, YMG, ZMG - integration points relative to cell centre, in plane of plate
!  XCELL, YCELL, ZCELL - cell centre locations in the observer system.
!
!        NLYR - number of layers including overburden.
!        THKD - QL precision layer thicknesses
!       DPTHL - QL depth to top of each layer
!        RMUD - QL precision relative magetic permeability
!        SIGL - QL precision complex xconductivity for all layers including Cole-Cole & dielectric
!        KSQL - QL precision iwu * SIGL for all layers (includes Cole-Cole)
!         NTX - number of transmitters
!       NRXTX - number of receivers per transmitter (max = MRXTX)
!        RXID - identifies type of receiver: 1 = mag dipole;  2 = electric dipole
!         NRS - number of subnet receivers per receiver (max = MXRS)
!    XRS, YRS - North, East coordinates of receiver subnet for plate JP
!        WTRS - subnet receiver weights
!        NMGP - number of cell integration weights
!       NRMGT - Dimension for MGT interpolations
!      RHOTRP - horizontal interpolation array for MGT
!    ZV1(NZ1) - vertical interpolatin array
!
!               OUTPUT
!               ------
!  HA(JAB,I,JR,JS) is the I-th component of the magnetic field of receiver JR of
!                    transmitter JS produced by a unitary electric dipole in the
!                    strike direction in the centre of plate cell number JAB.

!  HB(JAB,I,JR,JS) is the I-th component of the magnetic field of receiver JR of
!                    transmitter JS produced by a unitary electric dipole in the
!                    dip direction in the centre of plate cell number JAB.

!                    I = 1   is the North component
!                    I = 2   is the East component
!                    I = 3   is the vertical component

!  HA & HB are thus combinations of magnetic Green's tensor elements
!  integrated over the area of individual cells.  Units are Teslas per unit amp.

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 COMPLEX, PARAMETER :: ZERO=(0.,0.)
 INTEGER NPLT,MXAB,NLYR,SXLYR,RXLYR,KFG,NTX,MRXTX,NRXTX(NTX),MXRS,NRS(MRXTX,NTX), &
         NRMGT,NZ1,NMGP,JA,JB,JP,JR,JR1,JS,JAB,J1,I1,RXID(MRXTX,NTX)
 INTEGER, DIMENSION (NPLT) :: NA,NB,PLYR
 REAL RHOTRP(NRMGT),ZV1(NZ1),R32(3,2),SD,CD,CP,SP,CS,SS,XS,YS,ZS,XD,YD,ZD,RHO, &
      XBAR,YBAR,MUB,RXVRT
 REAL, DIMENSION (NPLT) :: PLAZM,PLDIP,PLUNJ,WMG
 REAL, DIMENSION (MXAB,NPLT) :: XCELL,YCELL,ZCELL
 REAL, DIMENSION (MXRS,MRXTX,NTX) ::  XRS,YRS,WTRS
 REAL, DIMENSION (MRXTX,NTX) :: ZRXTX
 REAL(KIND=QL) ZR,ZPRV,THKD(NLYR),DPTHL(NLYR),RMUD(0:NLYR)
 REAL, DIMENSION (4,NRMGT,NZ1) :: QB1R,QB1I,QB2R,QB2I,QB3R,QB3I,QB4R,QB4I,QB5R,QB5I
 REAL, DIMENSION (NMGP,NPLT) :: XMG,YMG,ZMG
 COMPLEX(KIND=QL), DIMENSION(NLYR) :: KSQL, SIGL
 COMPLEX MGT(3,3),MGTR(3,2),QQ(5),PSI(3),KB,KB0,C2DINTRP
 COMPLEX, DIMENSION (MXAB,3,MRXTX,NTX,NPLT) :: HA,HB

 MUB = REAL (RMUD(NLYR))
 KB = CMPLX (SQRT (KSQL(NLYR)) )
 KB0 = ZERO
 IF (REAL (KB) < 0.) KB = -KB
 IF (REAL (KB0) < 0.) KB0 = -KB0

!  Step down dip cell row by cell row, and within each row, one integration
!  line at a time.  Then step through receivers.  Set up the base integrals,
!  QB*j for each line - receiver height combination.
!  If next receiver has the same height, retain base integrals.

 ZPRV = -9.D4
 I1 = 2                               ! electric source
 DO JS = 1,NTX                        ! Loop over transmitters
   DO JR = 1, NRXTX(JS)               ! Loop over receivers
     IF (RXID(JR,JS) == 2 .OR.  RXID(JR,JS) == 3) CYCLE       ! This subroutine is for magnetic dipoles only.
     ZR = REAL (ZRXTX(JR,JS),QL)
     RXVRT = REAL (ABS (ZPRV - ZR))
     IF (RXVRT > .01_QL) THEN
       ZPRV = ZR
       RXLYR = 0
       DO J1 = NLYR,1,-1
         IF (ZR > DPTHL(J1)) THEN
           RXLYR = J1
           EXIT
         END IF
       END DO
       SXLYR = NLYR
       CALL SET_KFG (I1,NLYR,SXLYR,RXLYR,KFG)
       CALL MGT_CSPL (NRMGT,RHOTRP,SXLYR,RXLYR,KFG,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,ZR, &
                      NZ1,ZV1,QB1R,QB1I,QB2R,QB2I,QB3R,QB3I,QB4R,QB4I,QB5R,QB5I)
     END IF

     DO JP = 1,NPLT                    ! Loop over plates
       IF (PLYR(JP) < NLYR) CYCLE
       CS = COS (PLAZM(JP))
       SS = SIN (PLAZM(JP))
       CD = COS (PLDIP(JP))
       SD = SIN (PLDIP(JP))
       CP = COS (PLUNJ(JP))
       SP = SIN (PLUNJ(JP))
       CALL RPLT2XYZ (CS,SS,CD,SD,CP,SP,R32)  ! Rotation matrix

       DO JB = 1,NB(JP)                       ! Loop over cell rows
         DO JA = 1, NA(JP)                    ! Loop over cell columns
           JAB = JA + ((JB-1) * NA(JP))
           MGT = ZERO
           DO J1 = 1,NMGP                     ! Loop over cell integration points
             XS = XCELL(JAB,JP) + XMG(J1,JP)
             YS = YCELL(JAB,JP) + YMG(J1,JP)
             ZS = ZCELL(JAB,JP) + ZMG(J1,JP)

             DO JR1 = 1, NRS(JR,JS)           ! Loop over receiver subnet
               XD = XRS(JR1,JR,JS) - XS
               YD = YRS(JR1,JR,JS) - YS

! For "source" layer receivers, add in the direct term.  Note that C2DINTRP uses ZS
! rather than ZD because MGT_CSPL is called as a function of ZR.  The interpolants
! are then stored as a function of ZS for each value of ZR.

               ZD = REAL (ZR) - ZS
               PSI(1:3) = ZERO
               IF (RXLYR == NLYR) CALL MGTDIR (XD,YD,ZD,KB,MUB,PSI)

               RHO = SQRT (XD**2 + YD**2)
               RHO = MAX (RHO, RHOTRP(1))
               QQ(1) = C2DINTRP (RHOTRP,NRMGT,ZV1,NZ1,QB1R,QB1I,RHO,ZS)
               QQ(2) = C2DINTRP (RHOTRP,NRMGT,ZV1,NZ1,QB2R,QB2I,RHO,ZS)
               QQ(3) = C2DINTRP (RHOTRP,NRMGT,ZV1,NZ1,QB3R,QB3I,RHO,ZS)
               QQ(1:3) = QQ(1:3) * WTRS(JR1,JR,JS)
               XBAR = 0.
               YBAR = 0.
               IF (RHO > 1.E-6) THEN
                 XBAR = XD / RHO
                 YBAR = YD / RHO
               END IF
               MGT(1,1) = MGT(1,1) + XBAR * YBAR * (QQ(2) - 2.* QQ(1))

               IF (RXLYR == 0) THEN
                 MGT(2,1) = MGT(2,1) + (1. - 2.*YBAR**2) * QQ(1) + YBAR**2 * QQ(2)
                 MGT(1,2) = MGT(1,2) - (1. - 2.*XBAR**2) * QQ(1) - XBAR**2 * QQ(2)
                 MGT(3,1) = MGT(3,1) + YBAR * QQ(3)
                 MGT(3,2) = MGT(3,2) - XBAR * QQ(3)
                 MGT(1:2,3) = ZERO

               ELSE
                 QQ(4) = C2DINTRP (RHOTRP,NRMGT,ZV1,NZ1,QB4R,QB4I,RHO,ZS)
                 QQ(5) = C2DINTRP (RHOTRP,NRMGT,ZV1,NZ1,QB5R,QB5I,RHO,ZS)
                 QQ(4:5) = QQ(4:5) * WTRS(JR1,JR,JS)

                 MGT(2,1) = MGT(2,1) + (1. - 2.*YBAR**2) * QQ(1) + YBAR**2 * QQ(2) + QQ(4) - PSI(3)
                 MGT(1,2) = MGT(1,2) + (1. - 2.*YBAR**2) * QQ(1) - XBAR**2 * QQ(2) - QQ(4) + PSI(3)
                 MGT(3,1) = MGT(3,1) + YBAR * QQ(3) + PSI(2)
                 MGT(3,2) = MGT(3,2) - XBAR * QQ(3) - PSI(1)
                 MGT(1,3) = MGT(1,3) - YBAR * QQ(5) - PSI(2)
                 MGT(2,3) = MGT(2,3) + XBAR * QQ(5) + PSI(1)
               END IF
             END DO                ! End JR1 loop over receiver subnet
           END DO                  ! End J1 loop over integration points

! Rotate MGT from XYZ-XYZ TO XYZ-PLT system.  Apply plate weighting for each cell.

           MGT(2,2) = -MGT(1,1)
           MGTR = MATMUL (MGT, R32) * WMG(JP)

           HA(JAB,1:3,JR,JS,JP) = HA(JAB,1:3,JR,JS,JP) + MGTR(1:3,1)
           HB(JAB,1:3,JR,JS,JP) = HB(JAB,1:3,JR,JS,JP) + MGTR(1:3,2)

         END DO                    ! End JA loop over columns
       END DO                      ! End JB loop over rows
     END DO                        ! End JP loop over plates
   END DO                          ! End JR loop over receivers
 END DO                            ! End JS loop over transmitters

 END SUBROUTINE MGT_BOSS

 SUBROUTINE MGT_BOSS_UL (JP,SXLYR,NPLT,MXAB,NAL,NBL,DAL,DBL,PLAZM,PLDIP,XCELL,YCELL,NLYR, &
                         THKD,DPTHL,RMUD,SIGL,KSQL,NTX,MRXTX,NRXTX,RXID,MXRS,NRS,XRS,YRS, &
                         ZRXTX,WTRS,NRMGT,RHOTRP,NZ3L,ZC3L,HA,HB)
!----------------------------------------------------------------------------------------

!***  Called by SCAT_EM
!***  Calls MGT_CSPL

!  This rutine is used for plates above basement one at a time.
!
!  Integrated magnetic Green's tensor computation for relating the along strike
!  and down dip components of the scattering current for each plate to the
!  X,Y,Z magnetic fields at each receiver of each transmitter.  For loop receivers
!  this is done by subnets.  A 9 point per cell rule with uniform weights is used.
!
!                 INPUT
!                 -----
!
!            JP - plate index
!          NPLT - Number of plates
!          PLYR - layer containing plate
!          MXAB - Number of cells in biggest plate
!        NA, NB - number of cells along strike and down dip respectively
!         PLAZM - plate strike angles
!         PLDIP - plate dips
!  XCELL, YCELL - cell centre locations in the observer system.
!          NLYR - number of layers including overburden.
!          THKD - QL precision layer thicknesses
!         DPTHL - QL depth to top of each layer
!          RMUD - QL precision relative magetic permeability
!          SIGL - QL precision complex xconductivity for all layers including Cole-Cole & dielectric
!          KSQL - QL precision iwu * SIGL for all layers (includes Cole-Cole)
!           NTX - number of transmitters
!         NRXTX - number of receivers per transmitter (max = MRXTX)
!          RXID - identifies type of receiver: 1 = mag dipole;  2 = electric dipole
!           NRS - number of subnet receivers per receiver (max = MXRS)
!      XRS, YRS - North, East coordinates of receiver subnet for plate JP
!          WTRS - subnet receiver weights
!         NRMGT - Dimension for MGT interpolations
!        RHOTRP - horizontal interpolation array for MGT
!    ZV3L(NZ3L) - vertical interpolatin array
!
!               OUTPUT
!               ------
!  HA(JAB,I,JR,JS) is the I-th component of the magnetic field of receiver JR of
!                    transmitter JS produced by a unitary electric dipole in the
!                    strike direction in the centre of plate cell number JAB.

!  HB(JAB,I,JR,JS) is the I-th component of the magnetic field of receiver JR of
!                    transmitter JS produced by a unitary electric dipole in the
!                    dip direction in the centre of plate cell number JAB.

!                    I = 1   is the North component
!                    I = 2   is the East component
!                    I = 3   is the vertical component

!  HA & HB are thus combinations of magnetic Green's tensor elements
!  integrated over the area of individual cells.  Units are Teslas per unit amp.

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 COMPLEX, PARAMETER :: ZERO=(0.,0.)
 INTEGER NAL,NBL,NZ3L,NPLT,MXAB,NLYR,SXLYR,RXLYR,KFG,NTX,MRXTX,NRXTX(NTX),MXRS, &
         NRS(MRXTX,NTX),NRMGT,JA,JB,JP,JR,JZ,JR1,JS,JAB,J1,J2,I1,RXID(MRXTX,NTX)
 REAL DAL,DBL,RHOTRP(NRMGT),ZC3L(NZ3L),R32(3,2),SD,CD,CP,SP,CS,SS,XS,YS,ZS,XD,YD,ZD,RHO, &
      XBAR,YBAR,MUB,RXVRT,DELX,DELY,WMG
 REAL, DIMENSION (NPLT) :: PLAZM,PLDIP
 REAL, DIMENSION (MXAB,NPLT) :: XCELL,YCELL
 REAL, DIMENSION (MXRS,MRXTX,NTX) ::  XRS,YRS,WTRS
 REAL, DIMENSION (MRXTX,NTX) :: ZRXTX
 REAL(KIND=QL) ZR,ZPRV,THKD(NLYR),DPTHL(NLYR),RMUD(0:NLYR)
 REAL, DIMENSION (4,NRMGT,NZ3L) :: G1R,G1I,G2R,G2I,G3R,G3I,G4R,G4I,G5R,G5I
 COMPLEX MGT(3,3),MGTR(3,2),QQ(5),PSI(3),KB
 COMPLEX(KIND=QL), DIMENSION(NLYR) :: KSQL, SIGL
 COMPLEX, DIMENSION (MXAB,3,MRXTX,NTX,NPLT) :: HA,HB

 MUB = REAL (RMUD(SXLYR))
 KB = CMPLX (SQRT (KSQL(SXLYR)) )
 IF (REAL (KB) < 0.) KB = -KB
 CS = COS (PLAZM(JP))
 SS = SIN (PLAZM(JP))
 CD = COS (PLDIP(JP))
 SD = SIN (PLDIP(JP))
 CP = 1.0
 SP = 0.0
 CALL RPLT2XYZ (CS,SS,CD,SD,CP,SP,R32)  ! Rotation matrix

 DELX = DAL / 4.
 DELY = CD * DBL / 4.
 WMG = DAL * DBL / 9.     ! 9 point rule

!  Step down dip cell row by cell row, and within each row, one integration
!  line at a time.  Then step through receivers.  Set up the base integrals,
!  QB*j for each line - receiver height combination.
!  If next receiver has the same height, retain base integrals.

 ZPRV = -9.D4
 I1 = 2                               ! electric source
 DO JS = 1,NTX                        ! Loop over transmitters
   DO JR = 1, NRXTX(JS)               ! Loop over receivers
     IF (RXID(JR,JS) == 2 .OR.  RXID(JR,JS) == 3) CYCLE       ! This subroutine is for magnetic dipoles only.
     ZR = REAL (ZRXTX(JR,JS),QL)
     RXVRT = REAL (ABS (ZPRV - ZR))
     IF (RXVRT > .01_QL) THEN
       ZPRV = ZR
       RXLYR = 0
       DO J1 = NLYR,1,-1
         IF (ZR > DPTHL(J1)) THEN
           RXLYR = J1
           EXIT
         END IF
       END DO
       CALL SET_KFG (I1,NLYR,SXLYR,RXLYR,KFG)
       CALL MGT_CSPL (NRMGT,RHOTRP,SXLYR,RXLYR,KFG,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,ZR, &
                      NZ3L,ZC3L,G1R,G1I,G2R,G2I,G3R,G3I,G4R,G4I,G5R,G5I)
     END IF

     DO JB = 1,NBL
       DO JA = 1, NAL                    ! Loop over cell columns
         JAB = JA + (JB-1) * NAL
         MGT = ZERO
         DO J1 = -1,1                         ! Loop over horizontal cell integration points
           JZ = 3*JB + J1 - 1
           ZS = ZC3L(JZ)
           YS = YCELL(JAB,JP) + J1 * DELY
           ZD = REAL (ZR) - ZS
           DO J2 = -1,1                     ! Loop over horizontal cell integration points
             XS = XCELL(JAB,JP) + J2 * DELX
             DO JR1 = 1, NRS(JR,JS)           ! Loop over receiver subnet
               XD = XRS(JR1,JR,JS) - XS
               YD = YRS(JR1,JR,JS) - YS
               RHO = SQRT (XD**2 + YD**2)
               RHO = MAX (RHO, RHOTRP(1))


               CALL CUBVALRZ (RHOTRP,NRMGT,NZ3L,G1R,G1I,RHO,JZ,QQ(1))
               CALL CUBVALRZ (RHOTRP,NRMGT,NZ3L,G2R,G2I,RHO,JZ,QQ(2))
               CALL CUBVALRZ (RHOTRP,NRMGT,NZ3L,G3R,G3I,RHO,JZ,QQ(3))
               QQ(1:3) = QQ(1:3) * WTRS(JR1,JR,JS)
               XBAR = 0.
               YBAR = 0.
               IF (RHO > 1.E-6) THEN
                 XBAR = XD / RHO
                 YBAR = YD / RHO
               END IF
               MGT(1,1) = MGT(1,1) + XBAR * YBAR * (QQ(2) - 2.* QQ(1))

               IF (RXLYR == 0) THEN
                 MGT(2,1) = MGT(2,1) + (1. - 2.*YBAR**2) * QQ(1) + YBAR**2 * QQ(2)
                 MGT(1,2) = MGT(1,2) - (1. - 2.*XBAR**2) * QQ(1) - XBAR**2 * QQ(2)
                 MGT(3,1) = MGT(3,1) + YBAR * QQ(3)
                 MGT(3,2) = MGT(3,2) - XBAR * QQ(3)
                 MGT(1:2,3) = ZERO

               ELSE
                 PSI(1:3) = ZERO
                 IF (RXLYR == SXLYR) CALL MGTDIR (XD,YD,ZD,KB,MUB,PSI)  ! direct whole space term
                 CALL CUBVALRZ (RHOTRP,NRMGT,NZ3L,G4R,G4I,RHO,JZ,QQ(4))
                 CALL CUBVALRZ (RHOTRP,NRMGT,NZ3L,G5R,G5I,RHO,JZ,QQ(5))
                 QQ(4:5) = QQ(4:5) * WTRS(JR1,JR,JS)

                 MGT(2,1) = MGT(2,1) + (1. - 2.*YBAR**2) * QQ(1) + YBAR**2 * QQ(2) + QQ(4) - PSI(3)
                 MGT(1,2) = MGT(1,2) + (1. - 2.*YBAR**2) * QQ(1) - XBAR**2 * QQ(2) - QQ(4) + PSI(3)
                 MGT(3,1) = MGT(3,1) + YBAR * QQ(3) + PSI(2)
                 MGT(3,2) = MGT(3,2) - XBAR * QQ(3) - PSI(1)
                 MGT(1,3) = MGT(1,3) - YBAR * QQ(5) - PSI(2)
                 MGT(2,3) = MGT(2,3) + XBAR * QQ(5) + PSI(1)
               END IF
             END DO              ! End JR1 loop over receiver subnet
           END DO                ! End J2 loop over horizontal integration points
         END DO                  ! End J1 loop over vertical integration points

! Rotate MGT from XYZ-XYZ TO XYZ-PLT system.  Apply plate weighting for each cell.

         MGT(2,2) = -MGT(1,1)
         MGTR = MATMUL (MGT, R32) * WMG       ! 9 point per cell integration

         HA(JAB,1:3,JR,JS,JP) = HA(JAB,1:3,JR,JS,JP) + MGTR(1:3,1)
         HB(JAB,1:3,JR,JS,JP) = HB(JAB,1:3,JR,JS,JP) + MGTR(1:3,2)

       END DO                    ! End JA loop over columns
     END DO                      ! End JB loop over rows
   END DO                        ! End JR loop over receivers
 END DO                          ! End JS loop over transmitters

 END SUBROUTINE MGT_BOSS_UL

  SUBROUTINE MGT_CSPL (NRMGT,RHOTRP,SXLYR,RXLYR,KFG,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,ZR, &
                       NZ,ZV,QB1R,QB1I,QB2R,QB2I,QB3R,QB3I,QB4R,QB4I,QB5R,QB5I)
! ---------------------------------------------------------------------------------

!  Computes QBjR, and QBjI (j = 1,5) the real and imaginary parts of the
!  base Hankel transform integrals for specified ZR and ZS for the full
!  range of horizontal distances between receivers and points in the plate.
!  These are splined and used to compute magnetic Green's tensor elements.

!                  INPUT
!                  -----
!         NRMGT - number of points in RHOTRP needed for MGT computation
!        RHOTRP - abscissa array for interpolation in horizontal direction
!          NLYR - number of layers including overburden.
!          THKD - layer thicknesses
!         DPTHL - depth to top of each layer
!          RMUD - QL precision relative magetic permeability
!          SIGL - complex conductivities of air, overburden and host
!          KSQL - i * omega * mu * conductivity for each layer
!         RXLYR = layer containing receiver
!            ZR - receiver depth (negative for receiver in air)
!            ZS - depth of induced "source point" in plate


!***  Called by MGT_BOSS
!***  Calls MGT_HNK, CUBSPL

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 INTEGER NRMGT,NZ,NLYR,SXLYR,RXLYR,KFG,JZ
 REAL (KIND=QL) ZR,ZS,THKD(NLYR),DPTHL(NLYR),RMUD(0:NLYR)
 REAL RHOTRP(NRMGT),ZV(NZ)
 REAL, DIMENSION (4,NRMGT) :: QR,QI
 REAL, DIMENSION (4,NRMGT,NZ) :: QB1R,QB2R,QB3R,QB4R,QB5R,QB1I,QB2I,QB3I,QB4I,QB5I
 COMPLEX (KIND=QL) KSQL(NLYR),SIGL(NLYR),MHRI(NRMGT,5)

 QB1R = 0.; QB2R = 0.; QB3R = 0.; QB4R = 0.; QB5R = 0.;
 QB1I = 0.; QB2I = 0.; QB3I = 0.; QB4I = 0.; QB5I = 0.;


 DO JZ = 1,NZ
   ZS = REAL(ZV(JZ),QL)
   CALL MGT_HNK (NRMGT,RHOTRP,SXLYR,RXLYR,KFG,NLYR,THKD,DPTHL,SIGL,KSQL,RMUD,ZR,ZS,MHRI)

   QR(1,1:NRMGT) =        REAL (MHRI(1:NRMGT,1))
   QI(1,1:NRMGT) = REAL (AIMAG (MHRI(1:NRMGT,1)))
   CALL CUBSPL (RHOTRP,QR,NRMGT)
   CALL CUBSPL (RHOTRP,QI,NRMGT)
   QB1R(1:4,1:NRMGT,JZ) = QR(1:4,1:NRMGT)
   QB1I(1:4,1:NRMGT,JZ) = QI(1:4,1:NRMGT)

   QR(1,1:NRMGT) =        REAL (MHRI(1:NRMGT,2))
   QI(1,1:NRMGT) = REAL (AIMAG (MHRI(1:NRMGT,2)))
   CALL CUBSPL (RHOTRP,QR,NRMGT)
   CALL CUBSPL (RHOTRP,QI,NRMGT)
   QB2R(1:4,1:NRMGT,JZ) = QR(1:4,1:NRMGT)
   QB2I(1:4,1:NRMGT,JZ) = QI(1:4,1:NRMGT)

   QR(1,1:NRMGT) =        REAL (MHRI(1:NRMGT,3))
   QI(1,1:NRMGT) = REAL (AIMAG (MHRI(1:NRMGT,3)))
   CALL CUBSPL (RHOTRP,QR,NRMGT)
   CALL CUBSPL (RHOTRP,QI,NRMGT)
   QB3R(1:4,1:NRMGT,JZ) = QR(1:4,1:NRMGT)
   QB3I(1:4,1:NRMGT,JZ) = QI(1:4,1:NRMGT)

   IF (RXLYR > 0) THEN
     QR(1,1:NRMGT) =        REAL (MHRI(1:NRMGT,4))
     QI(1,1:NRMGT) = REAL (AIMAG (MHRI(1:NRMGT,4)))
     CALL CUBSPL (RHOTRP,QR,NRMGT)
     CALL CUBSPL (RHOTRP,QI,NRMGT)
     QB4R(1:4,1:NRMGT,JZ) = QR(1:4,1:NRMGT)
     QB4I(1:4,1:NRMGT,JZ) = QI(1:4,1:NRMGT)

     QR(1,1:NRMGT) =        REAL (MHRI(1:NRMGT,5))
     QI(1,1:NRMGT) = REAL (AIMAG (MHRI(1:NRMGT,5)))
     CALL CUBSPL (RHOTRP,QR,NRMGT)
     CALL CUBSPL (RHOTRP,QI,NRMGT)
     QB5R(1:4,1:NRMGT,JZ) = QR(1:4,1:NRMGT)
     QB5I(1:4,1:NRMGT,JZ) = QI(1:4,1:NRMGT)
   END IF
 END DO

 END SUBROUTINE MGT_CSPL

 SUBROUTINE MGTDIR (XD,YD,ZD,KB,MUB,PSI)
!---------------------------------------

!***  Called by MGT_BOSS

!  Computes PSI, the direct whole space magnetic field (B in Teslas) when magnetic
!  dipole receivers are in the same layer as the electric dipole sources.
!  KB & MUB are the propagation constant * relative megnetic permeability
!  of the whole space.
!
!  XD,YD,ZD are the x,y & z spacings between electric dipole Tx
!           and magnetic dipole receiver
!
!  PSI is the magnetic field in Teslas
!  It uses VFAC = mu0 * RMUD(NLYR) / (4 Pi) = 1.e-7 * 4*Pi / (4 Pi)
!               = 1.e-7 * RMUD(NLYR)

!  PSI(1) = whole space Myz (+) and Mzy (-).
!  PSI(2) = whole space Mzx (+) and Mxz (-).
!  PSI(3) = whole space Mxy (+) and Myx (-).

 IMPLICIT NONE
 REAL, PARAMETER :: VFAC0=1.e-7
 REAL XD,YD,ZD,R,MUB
 COMPLEX PSI(3),KB,BASE


 R = SQRT (XD**2 + YD**2 + ZD**2)
 IF (R < 1.E-10) THEN
   WRITE(*,'(/3X,A)') 'Magnetic dipole receiver cannot touch plate.  Computation aborted!'
   STOP
 END IF

 BASE = MUB * VFAC0 * ((1.,0.) + (KB*R)) * EXP(-KB*R) / R**3
 PSI(1) = XD * BASE
 PSI(2) = YD * BASE
 PSI(3) = ZD * BASE

 END SUBROUTINE MGTDIR

  SUBROUTINE MGT_HNK (NRMGT,RHOTRP,SXLYR,RXLYR,KFG,NLYR,THKD,DPTHL,SIGL,KSQL,RMUD,ZR,ZS,MHRI)
! ------------------------------------------------------------------------------------------

!***  Called by MGT_CSPL
!***  Calls MGT_KER

!  Sets up the five integrals MHRI (JR,J1), J1 = 1,5, needed to compute the
!  magnetic Green's tensor elements in EGT_BOSS.  It uses the flow through
!  Hankel transform method to compute values at RHOTRP(JR), (15 point per decade
!  spacing).  It uses a 15 point per decade filter coefficient set derived from
!  Christensen's program, FLTGEN.

!  This subroutine returns MHRI for a magnetic field in Teslas
!  It uses VFAC = mu0 / (4 Pi) = 1.e-7 * 4*Pi / (4 Pi) = 1.e-7

!              INPUT
!              -----
!    RHOTRP - interpolation array of rho values
!     NRMGT - number of interpolation points needed for MGT
!      NLYR - number of layers
!      THKD - layer thicknesses
!     DPTHL - QL depth to top of each layer
!      RMUD - QL precision relative magetic permeability
!      SIGL - complex conductivity of layers
!      KSQL - propagation constants
!     RXLYR = layer containing receiver
!        ZR - receiver depth (negative for receiver in air)
!        ZS - depth of induced "source point" in plate

  USE LG_Filter_Coefficients

 IMPLICIT NONE
 COMPLEX(KIND=QL), PARAMETER :: ZERO=(0._QL, 0._QL)
 REAL (KIND=QL), PARAMETER :: VFAC0=1.E-7_QL         ! Mu0 / 4 Pi
 INTEGER NLYR,SXLYR,RXLYR,KFG,NRMGT,L,JR,LMAX,LMIN,K,KMAX,KMIN,KBOT,K1,NINTG
 REAL RHOTRP(NRMGT)
 REAL (KIND=QL) DELTA,Y1,Y,RD,LMBDA,THKD(NLYR),DPTHL(NLYR),ZR,ZS,RMUD(0:NLYR),VFAC
 COMPLEX (KIND=QL), DIMENSION(NLYR) :: KSQL, SIGL
 COMPLEX (KIND=QL) KER(JNLO-NRMGT:JNHI,5),MHRI(NRMGT,5)
 LOGICAL JUMP

!  Initialise variables

 NINTG = 3
 VFAC = VFAC0

 IF (RXLYR > 0) THEN
   NINTG = 5
   VFAC = VFAC0 * REAL (RMUD(RXLYR))
 END IF

 MHRI = ZERO
 KER = ZERO
 DELTA = LOG (10._QL)/ REAL (NDEC_JN,QL)

! Set up KER for JR = 1 corresponding to minimum value of RHO.  This will
! compute most of the needed kernel range from the high end.  Note that
! the filter is defined between JNLO < L < JNHI

 JR = 1
 RD = REAL (RHOTRP(1),QL)
 Y1 = -LOG (RD) - DBLE (JR-1) * DELTA - SHFTJN

 DO L = -50, JNHI             ! Start at L = -50 to pick up low values.
   LMAX = L                   ! Maximum filter index used
   K = L + 1 - JR             ! K is the kernel index.
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
   CALL MGT_KER (NRMGT,K,JR,L,LMBDA,SXLYR,RXLYR,KFG,NLYR,THKD,DPTHL, &
                 SIGL,KSQL,RMUD,ZR,ZS,NINTG,KER,MHRI,JUMP)

   IF (JUMP) EXIT
 END DO

 Y = Y1                       ! Finish off the low end for RHOTRP(1)
 DO L = -51, JNLO, -1
   LMIN = L                   ! Minimum filter index used
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
   K = L + 1 - JR             ! Compute the kernel index.
   CALL MGT_KER (NRMGT,K,JR,L,LMBDA,SXLYR,RXLYR,KFG,NLYR,THKD,DPTHL, &
                 SIGL,KSQL,RMUD,ZR,ZS,NINTG,KER,MHRI,JUMP)
   IF (JUMP) EXIT
 END DO

 KMIN = LMIN + 1 - JR  !  Define the renge of kernel values
 KMAX = LMAX + 1 - JR  !  used for RHOTRP(1)

! Complete definition of kernel values by evaluating transform of
! maximum RHO = RHOTRP (NRMGT)

 JR = NRMGT
 Y1 = -LOG (RD) - DBLE (JR-1) * DELTA - SHFTJN
 KBOT = JNLO + 1 - JR
 K1 = MAX (KBOT,KMIN)

 DO K = K1, KMAX          ! Compute EHR for maximum RHO using previously
   L = K - 1 + JR         ! computed kernel values.
   MHRI(JR,1) = MHRI(JR,1) + KER(K,1) * WJ1(L)         ! Used for Mxx, Myy, Mxy, Myx
   MHRI(JR,2) = MHRI(JR,2) + KER(K,2) * WJ0(L)         ! Used for Mxx, Myy, Mxy, Myx
   MHRI(JR,3) = MHRI(JR,3) + KER(K,3) * WJ1(L)         ! Used for Mzx, Mzy
   MHRI(JR,4) = MHRI(JR,4) + KER(K,4) * WJ0(L)         ! Used for Mxy, Myx
   MHRI(JR,5) = MHRI(JR,5) + KER(K,5) * WJ1(L)         ! Used for Mxz, Myz
 END DO

 IF (K1 > KBOT) THEN    !  Add low end kernel values until convergence.
   DO K = K1-1, KBOT, -1
     KMIN = K
     L = K - 1 + JR
     Y = Y1 + DBLE (L) * DELTA
     LMBDA = EXP (Y)
     CALL MGT_KER (NRMGT,K,JR,L,LMBDA,SXLYR,RXLYR,KFG,NLYR,THKD,DPTHL, &
                   SIGL,KSQL,RMUD,ZR,ZS,NINTG,KER,MHRI,JUMP)
     IF (JUMP) EXIT
   END DO
 END IF

 DO JR = 2, NRMGT-1          !  Compute transforms for all other RHO values
   DO K = KMIN, KMAX         !  using previously computed kernel values.
     L = K - 1 + JR
     MHRI(JR,1) = MHRI(JR,1) + KER(K,1) * WJ1(L)
     MHRI(JR,2) = MHRI(JR,2) + KER(K,2) * WJ0(L)
     MHRI(JR,3) = MHRI(JR,3) + KER(K,3) * WJ1(L)
     MHRI(JR,4) = MHRI(JR,4) + KER(K,4) * WJ0(L)
     MHRI(JR,5) = MHRI(JR,5) + KER(K,5) * WJ1(L)
   END DO
 END DO

 DO JR = 1,NRMGT
   MHRI(JR,1) = MHRI(JR,1) / RHOTRP(JR)
   MHRI(JR,1:5) = VFAC * MHRI(JR,1:5) / RHOTRP(JR)
 END DO

 END SUBROUTINE MGT_HNK

 SUBROUTINE MGT_KER (NRMGT,K,JR,L,LMBDA,SXLYR,RXLYR,KFG,NLYR,THKD,DPTHL, &
                     SIGL,KSQL,RMUD,ZR,ZS,NINTG,KER,MHRI,JUMP)
!-------------------------------=--------------------------------------

!***  Called by MGT_HNK

!  Accumulates the integrals MHRI(J) (J=1,5) for 5 inverse Hankel transforms
!  needed for evaluation of magnetic Green's tensor elements.

!     NRMGT - number of points in 15 point / decade array
!         K - filter kernel index
!        JR - RHO interpolation index
!         L - filter point index
!     LMBDA - Hankel transform variable
!     RXLYR = layer containing receiver
!       KFG = 0, 3, or 1 if Rx is in air, basement or intermediate layer respectively
!      NLYR - number of layers
!      THKD - layer thicknesses
!     DPTHL - QL depth to top of each layer
!      SIGL - complex conductivity of layers
!      KSQL - propagation constants
!        ZR - receiver depth (negative for receiver in air)
!        ZS - depth of induced "source point" in plate

!   MHRI(J) - accumulated integrals for real and imaginary parts
!             of inverse Hankel transform (J = 1,5)
!      JUMP - keep integrating if false.

 USE LG_Filter_Coefficients

 IMPLICIT NONE
 REAL, PARAMETER :: TOL=1.E-5
 COMPLEX(KIND=QL), PARAMETER :: ZERO=(0._QL,0._QL)
 INTEGER NINTG,NRMGT,K,JR,L,SXLYR,RXLYR,NLYR,KFG,J
 REAL(KIND=QL) RMUD(0:NLYR),LMBDA,ZS,ZR,AR,AI,MHR,MHI
 REAL(KIND=QL), DIMENSION (NLYR) ::  THKD,DPTHL
 COMPLEX(KIND=QL) S(0:NLYR),SM,SL,XI_V,F_V,F_H,ETA_V,G_V,G_H,FACV,KVS,KS,KV,EU,ED, &
                  FACJ1,KER(JNLO-NRMGT:JNHI,5),MHRI(NRMGT,5),TMP(5)
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL
 LOGICAL TOO_BIG,JUMP

 KER(K,1:5) = ZERO
 CALL EDSX_COEF (SXLYR,RXLYR,KFG,LMBDA,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                 ZS,S,XI_V,F_V,F_H,ETA_V,G_V,G_H)

 SM = S(SXLYR)
 SL = S(RXLYR)
 FACV = RMUD(SXLYR) * SL / (RMUD(RXLYR) * SM)
 FACJ1 = LMBDA**2 / SM

 IF (RXLYR == 0) THEN                            ! Rx in Air
   KER(K,1) = FACV * XI_V * EXP (SL * ZR)
   KER(K,2) = KER(K,1) * LMBDA
   KER(K,3) = KER(K,2)

 ELSE IF (RXLYR == NLYR) THEN                    ! Rx in Basement
   ED = EXP (SM * (DPTHL(NLYR) - ZR))
   ETA_V = ED * ETA_V
   G_V   = ED * G_V
   G_H   = ED * G_H
   KVS = -(FACV * ETA_V + G_H)
   KER(K,1) = KVS
   KER(K,2) = KVS * LMBDA
   KER(K,3) = ETA_V * FACJ1
   KER(K,4) = G_H * LMBDA
   KER(K,5) = G_V * FACJ1

 ELSE                                            ! Rx in intermediate layer
   EU = EXP (SL * (ZR - DPTHL(RXLYR+1)))
   ED = EXP (SL * (DPTHL(RXLYR) - ZR))

   XI_V  = EU * XI_V
   ETA_V = ED * ETA_V
   F_V   = EU * F_V
   G_V   = ED * G_V
   F_H   = EU * F_H
   G_H   = ED * G_H
   KS = F_H + G_H
   KV = FACV * (XI_V - ETA_V)
   KVS = KV - KS
   KER(K,1) = KVS
   KER(K,2) = KVS * LMBDA
   KER(K,3) = (XI_V + ETA_V) * FACJ1
   KER(K,4) = KS * LMBDA
   KER(K,5) = (F_V + G_V) * FACJ1
 END IF

 TMP(1) =  KER(K,1) * WJ1(L)
 TMP(2) =  KER(K,2) * WJ0(L)
 TMP(3) =  KER(K,3) * WJ1(L)
 TMP(4) =  KER(K,4) * WJ0(L)
 TMP(5) =  KER(K,5) * WJ1(L)

! Accumulate 5 (3 for receiver in air) integrals simultaneously
! until convergence of all.

 JUMP = .TRUE.
 DO J = 1, NINTG
   MHRI(JR,J) = MHRI(JR,J) + TMP(J)
   AR = ABS (REAL (TMP(J)))
   AI = ABS (AIMAG (TMP(J)))
   MHR = ABS (REAL (MHRI(JR,J)))
   MHI = ABS (AIMAG (MHRI(JR,J)))
   TOO_BIG = .FALSE.
   IF (AR > TOL* MHR) TOO_BIG = .TRUE.
   IF (AI > TOL* MHI) TOO_BIG = .TRUE.
   IF (TOO_BIG) JUMP = .FALSE.
 END DO

 END SUBROUTINE MGT_KER

 SUBROUTINE EGTRX_BOSS (NPLT,PLYR,MXAB,NA,NB,PLAZM,PLDIP,PLUNJ,XCELL,YCELL,ZCELL,NLYR,THKD, &
                        DPTHL,RMUD,SIGL,KSQL,NTX,MRXTX,NRXTX,RXID,MXRS,NRS,XRS,YRS,ZRXTX,   &
                        WTRS,NRMGT,RHOTRP,NZ1,ZV1,NMGP,XMG,YMG,ZMG,WMG,HA,HB)
!--------------------------------------------------------------------------------------------

!***  Called by SCAT_EM
!***  Calls EGTRX_CSPL

!  Integrated electric Green's tensor computation for relating the along strike
!  and down dip components of the scattering current for each plate to the
!  X, Y electric fields at each surface receiver of each transmitter.
!  A 9 point per cell rule with uniform weights is used.

!                        INPUT
!                        -----
!
!                 NPLT - Number of plates
!                 PLYR - layer containing plate
!                 MXAB - Number of cells in biggest plate
!               NA, NB - number of cells along strike and down dip respectively
!                PLAZM - strike angle
!                PLDIP - plate dipS (radians)
!                PLUNJ - rotation
!                  WMG - integration weights for each plate
!                 NMGP - number of integration points per cell
!        XMG, YMG, ZMG - integration points relative to cell centre, in plane of plate
!  XCELL, YCELL, ZCELL - cell centre locations in the observer system.
!
!        NLYR - number of layers including overburden.
!        THKD - QL precision layer thicknesses
!       DPTHL - QL depth to top of each layer
!        RMUD - QL precision relative magetic permeability
!        SIGL - QL precision complex xconductivity for all layers including Cole-Cole & dielectric
!        KSQL - QL precision iwu * SIGL for all layers (includes Cole-Cole)
!         NTX - QL number of transmitters
!       NRXTX - number of receivers per transmitter (max = MRXTX)
!        RXID - identifies type of receiver: 1 = mag dipole;  2 = electric dipole
!         NRS - number of subnet receivers per receiver (max = MXRS)
!    XRS, YRS - North, East coordinates of receiver subnet for plate JP
!        WTRS - subnet receiver weights
!       NRMGT - Dimension for MGT interpolations
!      RHOTRP - horizontal interpolation array for MGT
!    ZV1(NZ1) - vertical interpolation array

!               OUTPUT
!               ------

!  HA(JAB,I,JR,JS) is the I-th component of the electric field of receiver JR of
!                    transmitter JS produced by a unitary electric dipole in the
!                    strike direction in the centre of plate cell number JAB.

!  HB(JAB,I,JR,JS) is the I-th component of the electric field of receiver JR of
!                    transmitter JS produced by a unitary electric dipole in the
!                    dip direction in the centre of plate cell number JAB.
!
!                    I = 1   is the North component
!                    I = 2   is the East component
!                    I = 3   is the vertical component

!  HA & HB are thus combinations of electric Green's tensor elements
!  integrated over the area of individual cells.

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 COMPLEX, PARAMETER :: ZERO=(0.,0.)
 INTEGER NINTG,NPLT,MXAB,NLYR,SXLYR,RXLYR,KFG,NTX,MRXTX,NRXTX(NTX),MXRS, &
         NRS(MRXTX,NTX),NRMGT,NZ1,NMGP,JA,JB,JP,JR,JR1,JS,JAB,J1,I1,RXID(MRXTX,NTX)
 INTEGER, DIMENSION (NPLT) :: NA,NB,PLYR
 REAL RHOTRP(NRMGT),ZV1(NZ1),R32(3,2),CD,SD,CP,SP,CA,SA,XD,YD,ZS,ZD,RHO,XBAR,YBAR,RXVRT
 REAL, DIMENSION (MXRS,MRXTX,NTX) :: XRS,YRS,WTRS
 REAL, DIMENSION (MRXTX,NTX) :: ZRXTX
 REAL (KIND=QL) THKD(NLYR),DPTHL(NLYR),RMUD(0:NLYR),ZPRV,ZR
 REAL, DIMENSION (NPLT) :: PLAZM,PLDIP,PLUNJ,WMG
 REAL, DIMENSION (MXAB,NPLT) :: XCELL,YCELL,ZCELL
 REAL, DIMENSION (NMGP,NPLT) :: XMG,YMG,ZMG
 REAL, DIMENSION (4,NRMGT,NZ1) :: QB1R,QB1I,QB2R,QB2I,QB3R,QB3I,QB4R,QB4I,QB5R,QB5I,QB6R,QB6I
 COMPLEX KB,EGT(3,3),EGTR(3,2),QQ(6),C2DINTRP,EXX,EXY,EXZ,EYY,EYZ,EZZ
 COMPLEX, DIMENSION (MXAB,3,MRXTX,NTX,NPLT) :: HA,HB
 COMPLEX(KIND=QL) KSQL(NLYR), SIGL(NLYR)

 KB = CMPLX (SQRT (KSQL(NLYR)) )
 IF (REAL (KB) < 0.) KB = -KB

!  Step down dip cell row by cell row, and within each row, one integration
!  line at a time.  Then step through receivers.  Set up the base integrals,
!  QB*j for each line - receiver height combination.
!  If next receiver has the same height, retain base integrals.

 ZPRV = -9.D4
 DO JS = 1,NTX                        ! Loop over transmitters
   DO JR = 1, NRXTX(JS)               ! Loop over receivers
     IF (RXID(JR,JS) == 1) THEN
       CYCLE                          ! This subroutine is only used for electric fields
     ELSE IF (RXID(JR,JS) == 2) THEN  ! Horizontal electric dipole receiver
       NINTG = 4
     ELSE IF (RXID(JR,JS) == 3) THEN  ! 3 components of the electric field
       NINTG = 6
     END IF

     ZR = REAL (ZRXTX(JR,JS),QL)
     ZR = MAX (0.001D0,ZR)               ! Electrodes must be in contact with earth
     RXVRT = REAL (ABS (ZPRV - ZR))
     IF (RXVRT > .01_QL) THEN
       ZPRV = ZR
       RXLYR = 1
       DO J1 = NLYR,2,-1
         IF (ZR > DPTHL(J1)) THEN
           RXLYR = J1
           EXIT
         END IF
       END DO
       I1 = 2
       SXLYR = NLYR
       CALL SET_KFG (I1,NLYR,SXLYR,RXLYR,KFG)
       CALL EGTRX_CSPL (NRMGT,RHOTRP,SXLYR,RXLYR,KFG,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,ZR,NZ1,ZV1, &
                        NINTG,QB1R,QB1I,QB2R,QB2I,QB3R,QB3I,QB4R,QB4I,QB5R,QB5I,QB6R,QB6I)
     END IF


     DO JP = 1,NPLT                        ! Loop over plates
       IF (PLYR(JP) < NLYR) CYCLE
       CA = COS (PLAZM(JP))
       SA = SIN (PLAZM(JP))
       CD = COS (PLDIP(JP))
       SD = SIN (PLDIP(JP))
       CP = COS (PLUNJ(JP))
       SP = SIN (PLUNJ(JP))
       CALL RPLT2XYZ (CA,SA,CD,SD,CP,SP,R32)  ! Rotation matrix

       DO JB = 1,NB(JP)                       ! Loop over cell rows
         DO JA = 1, NA(JP)                    ! Loop over cell columns
           JAB = JA + ((JB-1) * NA(JP))
           EGT = ZERO
           DO J1 = 1,NMGP                ! Loop over cell integration points
             ZS = ZCELL(JAB,JP) + ZMG(J1,JP)
             ZD = REAL (ZR) - ZS
             DO JR1 = 1, NRS(JR,JS)      ! Loop over receiver segments
               XD = XRS(JR1,JR,JS) - (XCELL(JAB,JP) + XMG(J1,JP))
               YD = YRS(JR1,JR,JS) - (YCELL(JAB,JP) + YMG(J1,JP))

               RHO = SQRT (XD**2 + YD**2)
               RHO = MAX (RHO, RHOTRP(1))

! Note that C2DINTRP uses ZS rather than ZD because MGT_CSPL is called as a
! function of ZR.  The interpolants are then stored as a function of ZS
! for each value of ZR.    Of course, this is a moot point since the
! restriction ZR = 0 is used in this version.

               QQ(1) = C2DINTRP (RHOTRP,NRMGT,ZV1,NZ1,QB1R,QB1I,RHO,ZS)
               QQ(2) = C2DINTRP (RHOTRP,NRMGT,ZV1,NZ1,QB2R,QB2I,RHO,ZS)
               QQ(3) = C2DINTRP (RHOTRP,NRMGT,ZV1,NZ1,QB3R,QB3I,RHO,ZS)
               QQ(4) = C2DINTRP (RHOTRP,NRMGT,ZV1,NZ1,QB4R,QB4I,RHO,ZS)
               QQ(1:4) = QQ(1:4) * WTRS(JR1,JR,JS)   !  weight receiver subnet points
               XBAR = 0.
               YBAR = 0.
               IF (RHO > 1.E-6) THEN
                 XBAR = XD / RHO
                 YBAR = YD / RHO
               END IF

               EXX = ZERO; EYY = ZERO; EZZ = ZERO
               EXY = ZERO; EXZ = ZERO; EYZ = ZERO
               IF (RXLYR == NLYR) CALL EGTRX_DIR (XD,YD,ZD,KB,EXX,EXY,EXZ,EYY,EYZ,EZZ)

               EGT(1,1) = EGT(1,1) + QQ(1)* (1. - 2.*XBAR**2) + QQ(2) * XBAR**2 - QQ(3) + EXX
               EGT(2,2) = EGT(2,2) + QQ(1)* (1. - 2.*YBAR**2) + QQ(2) * YBAR**2 - QQ(3) + EYY
               EGT(1,2) = EGT(1,2) + XBAR * YBAR * (QQ(2) - 2.* QQ(1)) + EXY
               EGT(1,3) = EGT(1,3) - XBAR * QQ(4) + EXZ
               EGT(2,3) = EGT(2,3) - YBAR * QQ(4) + EYZ

               IF (NINTG == 6) THEN       ! Compute vertical field components
                 QQ(5) = C2DINTRP (RHOTRP,NRMGT,ZV1,NZ1,QB5R,QB5I,RHO,ZS)
                 QQ(6) = C2DINTRP (RHOTRP,NRMGT,ZV1,NZ1,QB6R,QB6I,RHO,ZS)
                 EGT(3,1) = EXZ - QQ(5) * XBAR
                 EGT(3,2) = EYZ - QQ(5) * YBAR
                 EGT(3,3) = EZZ + QQ(6)
               END IF
             END DO                       ! End JR1 loop over receiver subnet
           END DO                         ! End J1 loop over cell integration points

! Rotate EGT from XYZ-XYZ TO XYZ-PLT system.  Apply plate weighting for each cell.

           EGT(2,1) = EGT(1,2)
           EGTR = MATMUL (EGT, R32) * WMG(JP)
           HA(JAB,1:3,JR,JS,JP) = HA(JAB,1:3,JR,JS,JP) + EGTR(1:3,1)
           HB(JAB,1:3,JR,JS,JP) = HB(JAB,1:3,JR,JS,JP) + EGTR(1:3,2)

         END DO                           ! End JR loop over cell columns
       END DO                             ! End JS loop over cell rows
     END DO                               ! End JB loop over plates
   END DO                                 ! End JB loop over receivers
 END DO                                   ! End JP loop over transmitters

!  HA(JAB,3,JR,JS,JP) = ZERO ;  HB(JAB,3,JR,JS,JP) = ZERO

 END SUBROUTINE EGTRX_BOSS

 SUBROUTINE EGTRX_BOSS_UL (JP,SXLYR,NPLT,MXAB,NAL,NBL,DAL,DBL,PLAZM,PLDIP,XCELL,YCELL,NLYR, &
                           THKD,DPTHL,RMUD,SIGL,KSQL,NTX,MRXTX,NRXTX,RXID,MXRS,NRS,XRS,YRS, &
                           ZRXTX,WTRS,NRMGT,RHOTRP,NZ3L,ZC3L,HA,HB)
!--------------------------------------------------------------------------------------------

!***  Called by SCAT_EM
!***  Calls EGTRX_CSPL

!  This rutine is used for plates above basement one at a time.
!
!  Integrated electric Green's tensor computation for relating the along strike
!  and down dip components of the scattering current for each plate to the
!  X, Y electric fields at each surface receiver of each transmitter.
!  A 9 point per cell rule with uniform weights is used.

!                 INPUT
!                 -----
!
!            JP - plate index
!          NPLT - Number of plates
!          PLYR - layer containing plate
!          MXAB - Number of cells in biggest plate
!        NA, NB - number of cells along strike and down dip respectively
!         PLAZM - plate strike angles
!         PLDIP - plate dips
!  XCELL, YCELL - cell centre locations in the observer system.
!          NLYR - number of layers including overburden.
!          THKD - QL precision layer thicknesses
!         DPTHL - QL depth to top of each layer
!          RMUD - QL precision relative magetic permeability
!          SIGL - QL precision complex xconductivity for all layers including Cole-Cole & dielectric
!          KSQL - QL precision iwu * SIGL for all layers (includes Cole-Cole)
!           NTX - number of transmitters
!         NRXTX - number of receivers per transmitter (max = MRXTX)
!          RXID - identifies type of receiver: 1 = mag dipole;  2 = electric dipole
!           NRS - number of subnet receivers per receiver (max = MXRS)
!      XRS, YRS - North, East coordinates of receiver subnet for plate JP
!          WTRS - subnet receiver weights
!         NRMGT - Dimension for MGT interpolations
!        RHOTRP - horizontal interpolation array for MGT
!    ZV3L(NZ3L) - vertical interpolatin array
!
!               OUTPUT
!               ------
!  HA(JAB,I,JR,JS) is the I-th component of the electric field of receiver JR of
!                    transmitter JS produced by a unitary electric dipole in the
!                    strike direction in the centre of plate cell number JAB.

!  HB(JAB,I,JR,JS) is the I-th component of the electric field of receiver JR of
!                    transmitter JS produced by a unitary electric dipole in the
!                    dip direction in the centre of plate cell number JAB.

!                    I = 1   is the North component
!                    I = 2   is the East component
!                    I = 3   is the vertical component

!  HA & HB are thus combinations of electric Green's tensor elements
!  integrated over the area of individual cells.

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 COMPLEX, PARAMETER :: ZERO=(0.,0.)
 INTEGER NAL,NBL,NZ3L,NINTG,NPLT,MXAB,NLYR,SXLYR,RXLYR,KFG,NTX,MRXTX,NRXTX(NTX),MXRS, &
         NRS(MRXTX,NTX),NRMGT,JA,JB,JP,JR,JZ,JR1,JS,JAB,J1,J2,I1,RXID(MRXTX,NTX)
 REAL DAL,DBL,RHOTRP(NRMGT),ZC3L(NZ3L),R32(3,2),SD,CD,CP,SP,CS,SS,XS,YS,ZS,XD,YD,ZD,RHO, &
      XBAR,YBAR,RXVRT,DELX,DELY,WMG
 REAL, DIMENSION (NPLT) :: PLAZM,PLDIP
 REAL, DIMENSION (MXAB,NPLT) :: XCELL,YCELL
 REAL, DIMENSION (MXRS,MRXTX,NTX) :: XRS,YRS,WTRS
 REAL, DIMENSION (MRXTX,NTX) :: ZRXTX
 REAL (KIND=QL) ZR,ZPRV,THKD(NLYR),DPTHL(NLYR),RMUD(0:NLYR)
 REAL, DIMENSION (4,NRMGT,NZ3L) :: G1R,G1I,G2R,G2I,G3R,G3I,G4R,G4I,G5R,G5I,G6R,G6I
 COMPLEX KB,EGT(3,3),EGTR(3,2),QQ(6),EXX,EXY,EXZ,EYY,EYZ,EZZ
 COMPLEX(KIND=QL) KSQL(NLYR), SIGL(NLYR)
 COMPLEX, DIMENSION (MXAB,3,MRXTX,NTX,NPLT) :: HA,HB

 KB = CMPLX (SQRT (KSQL(SXLYR)) )
 IF (REAL (KB) < 0.) KB = -KB
 CS = COS (PLAZM(JP))
 SS = SIN (PLAZM(JP))
 CD = COS (PLDIP(JP))
 SD = SIN (PLDIP(JP))
 CP = 1.0
 SP = 0.0
 CALL RPLT2XYZ (CS,SS,CD,SD,CP,SP,R32)  ! Rotation matrix

 DELX = DAL / 4.
 DELY = CD * DBL / 4.
 WMG = DAL * DBL / 9.     ! 9 point rule

!  Step down dip cell row by cell row, and within each row, one integration
!  line at a time.  Then step through receivers.  Set up the base integrals,
!  G*j for each line - receiver height combination.
!  If next receiver has the same height, retain base integrals.

 ZPRV = -9.D4
 DO JS = 1,NTX                        ! Loop over transmitters
   DO JR = 1, NRXTX(JS)               ! Loop over receivers
     IF (RXID(JR,JS) == 1) THEN
       CYCLE                          ! This subroutine is only used for electric fields
     ELSE IF (RXID(JR,JS) == 2) THEN  ! Horizontal electric dipole receiver
       NINTG = 4
     ELSE IF (RXID(JR,JS) == 3) THEN  ! 3 components of the electric field
       NINTG = 6
     END IF

     ZR = REAL (ZRXTX(JR,JS),QL)
     ZR = MAX (0.001D0,ZR)               ! Electrodes must be in contact with earth
     RXVRT = REAL (ABS (ZPRV - ZR))
     IF (RXVRT > .01_QL) THEN
       ZPRV = ZR
       RXLYR = 1
       DO J1 = NLYR,2,-1
         IF (ZR > DPTHL(J1)) THEN
           RXLYR = J1
           EXIT
         END IF
       END DO
       I1 = 2
       CALL SET_KFG (I1,NLYR,SXLYR,RXLYR,KFG)
       CALL EGTRX_CSPL (NRMGT,RHOTRP,SXLYR,RXLYR,KFG,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,ZR,NZ3L, &
                        ZC3L,NINTG,G1R,G1I,G2R,G2I,G3R,G3I,G4R,G4I,G5R,G5I,G6R,G6I)
     END IF

     DO JB = 1,NBL
       DO JA = 1, NAL                    ! Loop over cell columns
         JAB = JA + (JB-1) * NAL
         EGT = ZERO
         DO J1 = -1,1                         ! Loop over horizontal cell integration points
           JZ = 3*JB + J1 - 1
           ZS = ZC3L(JZ)
           YS = YCELL(JAB,JP) + J1 * DELY
           ZD = REAL (ZR) - ZS
           DO J2 = -1,1                     ! Loop over horizontal cell integration points
             XS = XCELL(JAB,JP) + J2 * DELX
             DO JR1 = 1, NRS(JR,JS)           ! Loop over receiver subnet
               XD = XRS(JR1,JR,JS) - XS
               YD = YRS(JR1,JR,JS) - YS

               RHO = SQRT (XD**2 + YD**2)
               RHO = MAX (RHO, RHOTRP(1))

               CALL CUBVALRZ (RHOTRP,NRMGT,NZ3L,G1R,G1I,RHO,JZ,QQ(1))
               CALL CUBVALRZ (RHOTRP,NRMGT,NZ3L,G2R,G2I,RHO,JZ,QQ(2))
               CALL CUBVALRZ (RHOTRP,NRMGT,NZ3L,G3R,G3I,RHO,JZ,QQ(3))
               CALL CUBVALRZ (RHOTRP,NRMGT,NZ3L,G4R,G4I,RHO,JZ,QQ(4))
               QQ(1:4) = QQ(1:4) * WTRS(JR1,JR,JS)   !  weight receiver subnet points
               XBAR = 0.
               YBAR = 0.
               IF (RHO > 1.E-6) THEN
                 XBAR = XD / RHO
                 YBAR = YD / RHO
               END IF

               EXX = ZERO; EYY = ZERO; EZZ = ZERO
               EXY = ZERO; EXZ = ZERO; EYZ = ZERO
               IF (RXLYR == SXLYR) CALL EGTRX_DIR (XD,YD,ZD,KB,EXX,EXY,EXZ,EYY,EYZ,EZZ)

               EGT(1,1) = EGT(1,1) + QQ(1)* (1. - 2.*XBAR**2) + QQ(2) * XBAR**2 - QQ(3) + EXX
               EGT(2,2) = EGT(2,2) + QQ(1)* (1. - 2.*YBAR**2) + QQ(2) * YBAR**2 - QQ(3) + EYY
               EGT(1,2) = EGT(1,2) + XBAR * YBAR * (QQ(2) - 2.* QQ(1)) + EXY
               EGT(1,3) = EGT(1,3) - XBAR * QQ(4) + EXZ
               EGT(2,3) = EGT(2,3) - YBAR * QQ(4) + EYZ

               IF (NINTG == 6) THEN       ! Compute vertical field components
                 CALL CUBVALRZ (RHOTRP,NRMGT,NZ3L,G5R,G5I,RHO,JZ,QQ(5))
                 CALL CUBVALRZ (RHOTRP,NRMGT,NZ3L,G6R,G6I,RHO,JZ,QQ(6))
                 EGT(3,1) = EXZ - QQ(5) * XBAR
                 EGT(3,2) = EYZ - QQ(5) * YBAR
                 EGT(3,3) = EZZ + QQ(6)
               END IF
             END DO              ! End JR1 loop over receiver subnet
           END DO                ! End J2 loop over horizontal integration points
         END DO                  ! End J1 loop over vertical integration points

! Rotate EGT from XYZ-XYZ TO XYZ-PLT system.  Apply plate weighting for each cell.

         EGT(2,1) = EGT(1,2)
         EGTR = MATMUL (EGT, R32) * WMG
         HA(JAB,1:3,JR,JS,JP) = HA(JAB,1:3,JR,JS,JP) + EGTR(1:3,1)
         HB(JAB,1:3,JR,JS,JP) = HB(JAB,1:3,JR,JS,JP) + EGTR(1:3,2)

       END DO                    ! End JA loop over columns
     END DO                      ! End JB loop over rows
   END DO                        ! End JR loop over receivers
 END DO                          ! End JS loop over transmitters

!  HA(JAB,3,JR,JS,JP) = ZERO ;  HB(JAB,3,JR,JS,JP) = ZERO

 END SUBROUTINE EGTRX_BOSS_UL

 SUBROUTINE EGTRX_DIR (XD,YD,ZD,KB,EXX,EXY,EXZ,EYY,EYZ,EZZ)
!-----------------------------------------------------------
!
!*** Called by EGTRX_BOSS

!  Computes the whole space dipole electric fields for cases when the receiver
!  and 'source are in the same layer.

 REAL XD,YD,ZD,XB,YB,ZB,R
 COMPLEX KB,EXX,EXY,EXZ,EYY,EYZ,EZZ,KR,R11,R12,KRX
 COMPLEX, PARAMETER :: ONE=(1.,0.), THREE=(3.,0.)

 R = SQRT (XD**2 + YD**2 + ZD**2)
 XB = XD / R
 YB = YD / R
 ZB = ZD / R
 IF (R < 1.E-10) THEN
   WRITE(*,'(/3X,A)') 'Grounded wire receiver cannot touch plate.  Computation aborted!'
   STOP
 END IF
 KR = KB * R

 R11 = ONE + KR + KR**2
 R12 = THREE + THREE * KR + KR**2
 KRX = EXP (-KR) / KR**3

 EXX = KRX * (XB**2 * R12 - R11)
 EYY = KRX * (YB**2 * R12 - R11)
 EZZ = KRX * (ZB**2 * R12 - R11)
 EXY = XB * YB * KRX * R12
 EXZ = XB * ZB * KRX * R12
 EYZ = YB * ZB * KRX * R12

 END SUBROUTINE EGTRX_DIR

 SUBROUTINE EGTRX_CSPL (NRMGT,RHOTRP,SXLYR,RXLYR,KFG,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,ZR,NZ,ZV, &
                        NINTG,QB1R,QB1I,QB2R,QB2I,QB3R,QB3I,QB4R,QB4I,QB5R,QB5I,QB6R,QB6I)
!---------------------------------------------------------------------------------------------

!  Computes QBjR, and QBjI (j = 1,4) the real and imaginary parts of the
!  base Hankel transform integrals for specified ZS for the full
!  range of horizontal distances between receivers and points in the plate.
!  These are splined and used to compute electric Green's tensor elements.

!                  INPUT
!                  -----
!         NRMGT - number of points in RHOTRP needed for EGTRX computation
!        RHOTRP - abscissa array for interpolation in horizontal direction
!          NLYR - number of layers including overburden.
!          SIGL - complex conductivities of air, overburden and host
!          KSQL - i * omega * mu * conductivity for each layer
!          THKD - layer thicknesses
!            ZS - depth of induced "source point" in plate
!         NINTG - number of integrals for filter evaluation


!***  Called by EGTRX_BOSS
!***  Calls EGTRX_HNK, CUBSPL

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 INTEGER NRMGT,KFG,SXLYR,RXLYR,NLYR,JZ,NZ,NINTG
 REAL (KIND=QL) ZS,ZR,THKD(NLYR),DPTHL(NLYR),RMUD(0:NLYR)
 REAL RHOTRP(NRMGT),ZV(NZ)
 REAL, DIMENSION (4,NRMGT) :: QR,QI
 REAL, DIMENSION (4,NRMGT,NZ) :: QB1R,QB1I,QB2R,QB2I,QB3R,QB3I,QB4R,QB4I,QB5R,QB5I,QB6R,QB6I
 COMPLEX (KIND=QL) EHRI(NRMGT,6)
 COMPLEX(KIND=QL), DIMENSION(NLYR) :: KSQL, SIGL

 QB1R = 0.; QB2R = 0.; QB3R = 0.; QB4R = 0.; QB5R = 0.; QB6R = 0.
 QB1I = 0.; QB2I = 0.; QB3I = 0.; QB4I = 0.; QB5I = 0.; QB6I = 0.

 DO JZ = 1,NZ
   ZS = REAL(ZV(JZ),QL)
   CALL EGTRX_HNK (NRMGT,RHOTRP,SXLYR,RXLYR,KFG,NLYR,THKD,DPTHL,SIGL,KSQL,RMUD,ZR,ZS,NINTG,EHRI)

   QR(1,1:NRMGT) =        REAL (EHRI(1:NRMGT,1))
   QI(1,1:NRMGT) = REAL (AIMAG (EHRI(1:NRMGT,1)))
   CALL CUBSPL (RHOTRP,QR,NRMGT)
   CALL CUBSPL (RHOTRP,QI,NRMGT)
   QB1R(1:4,1:NRMGT,JZ) = QR(1:4,1:NRMGT)
   QB1I(1:4,1:NRMGT,JZ) = QI(1:4,1:NRMGT)

   QR(1,1:NRMGT) =        REAL (EHRI(1:NRMGT,2))
   QI(1,1:NRMGT) = REAL (AIMAG (EHRI(1:NRMGT,2)))
   CALL CUBSPL (RHOTRP,QR,NRMGT)
   CALL CUBSPL (RHOTRP,QI,NRMGT)
   QB2R(1:4,1:NRMGT,JZ) = QR(1:4,1:NRMGT)
   QB2I(1:4,1:NRMGT,JZ) = QI(1:4,1:NRMGT)

   QR(1,1:NRMGT) =        REAL (EHRI(1:NRMGT,3))
   QI(1,1:NRMGT) = REAL (AIMAG (EHRI(1:NRMGT,3)))
   CALL CUBSPL (RHOTRP,QR,NRMGT)
   CALL CUBSPL (RHOTRP,QI,NRMGT)
   QB3R(1:4,1:NRMGT,JZ) = QR(1:4,1:NRMGT)
   QB3I(1:4,1:NRMGT,JZ) = QI(1:4,1:NRMGT)

   QR(1,1:NRMGT) =        REAL (EHRI(1:NRMGT,4))
   QI(1,1:NRMGT) = REAL (AIMAG (EHRI(1:NRMGT,4)))
   CALL CUBSPL (RHOTRP,QR,NRMGT)
   CALL CUBSPL (RHOTRP,QI,NRMGT)
   QB4R(1:4,1:NRMGT,JZ) = QR(1:4,1:NRMGT)
   QB4I(1:4,1:NRMGT,JZ) = QI(1:4,1:NRMGT)


   IF (NINTG == 6) THEN
     QR(1,1:NRMGT) =        REAL (EHRI(1:NRMGT,5))
     QI(1,1:NRMGT) = REAL (AIMAG (EHRI(1:NRMGT,5)))
     CALL CUBSPL (RHOTRP,QR,NRMGT)
     CALL CUBSPL (RHOTRP,QI,NRMGT)
     QB5R(1:4,1:NRMGT,JZ) = QR(1:4,1:NRMGT)
     QB5I(1:4,1:NRMGT,JZ) = QI(1:4,1:NRMGT)

     QR(1,1:NRMGT) =        REAL (EHRI(1:NRMGT,6))
     QI(1,1:NRMGT) = REAL (AIMAG (EHRI(1:NRMGT,6)))
     CALL CUBSPL (RHOTRP,QR,NRMGT)
     CALL CUBSPL (RHOTRP,QI,NRMGT)
     QB6R(1:4,1:NRMGT,JZ) = QR(1:4,1:NRMGT)
     QB6I(1:4,1:NRMGT,JZ) = QI(1:4,1:NRMGT)
   END IF

 END DO

 END SUBROUTINE EGTRX_CSPL

 SUBROUTINE EGTRX_HNK (NRMGT,RHOTRP,SXLYR,RXLYR,KFG,NLYR,THKD,DPTHL,SIGL,KSQL,RMUD,ZR,ZS,NINTG,EHRI)
!----------------------------------------------------------------------------------------------------

!***  Called by EGTRX_CSPL
!***  Calls EGTRX_KER

!  Sets up the five integrals EHRI (JR,J1), J1 = 1,4, needed to compute the
!  electric Green's tensor elements in EGTRX_BOSS.  It uses the flow through
!  Hankel transform method to compute values at RHOTRP(JR), (15 point per decade
!  spacing).  It uses a 15 point per decade filter coefficient set derived from
!  Christensen's program, FLTGEN.

!  This subroutine returns EHRI for a electric field in nT
!  It uses VFAC = 1.e9 * mu0 / (4 Pi) = 1.e9 * 1.e-7 * 4*Pi / (4 Pi) = 100

!              INPUT
!              -----
!    RHOTRP - interpolation array of rho values
!     NRMGT - number of interpolation points needed for EGTRX
!      NLYR - number of layers
!      SIGL - complex conductivity of layers
!      KSQL - propagation constants
!      THKD - layer thicknesses
!        ZS - depth of induced "source point" in plate
!     NINTG - number of integrals for filter evaluation

  USE LG_Filter_Coefficients

 IMPLICIT NONE
 REAL(KIND=QL), PARAMETER :: FOURPI=12.56637_QL
 COMPLEX(KIND=QL), PARAMETER :: ZERO=(0._QL, 0._QL)
 INTEGER NINTG,KFG,SXLYR,RXLYR,NLYR,NRMGT,L,JR,LMAX,LMIN,K,KMAX,KMIN,KBOT,K1,J
 REAL RHOTRP(NRMGT)
 REAL (KIND=QL) DELTA,Y1,Y,RD,LMBDA,ZR,ZS,THKD(NLYR),DPTHL(NLYR),RMUD(0:NLYR),RHOD
 COMPLEX (KIND=QL) KER(JNLO-NRMGT:JNHI,6),EHRI(NRMGT,6)
 COMPLEX(KIND=QL), DIMENSION(NLYR) :: KSQL, SIGL
 LOGICAL JUMP

!  Initialise variables

 EHRI = ZERO
 KER = ZERO
 DELTA = LOG (10._QL)/ REAL (NDEC_JN,QL)

! Set up KER for JR = 1 corresponding to minimum value of RHO.  This will
! compute most of the needed kernel range from the high end.  Note that
! the filter is defined between JNLO < L < JNHI

 JR = 1
 RD = REAL (RHOTRP(1),QL)
 Y1 = -LOG (RD) - DBLE (JR-1) * DELTA - SHFTJN

 DO L = -50, JNHI             ! Start at L = -50 to pick up low values.
   LMAX = L                   ! Maximum filter index used
   K = L + 1 - JR             ! K is the kernel index.
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
   CALL EGTRX_KER (NRMGT,K,JR,L,LMBDA,SXLYR,RXLYR,KFG,NLYR,THKD,DPTHL, &
                   SIGL,KSQL,RMUD,ZR,ZS,NINTG,KER,EHRI,JUMP)

   IF (JUMP) EXIT
 END DO

 Y = Y1                       ! Finish off the low end for RHOTRP(1)
 DO L = -51, JNLO, -1
   LMIN = L                   ! Minimum filter index used
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
   K = L + 1 - JR             ! Compute the kernel index.
   CALL EGTRX_KER (NRMGT,K,JR,L,LMBDA,SXLYR,RXLYR,KFG,NLYR,THKD,DPTHL, &
                   SIGL,KSQL,RMUD,ZR,ZS,NINTG,KER,EHRI,JUMP)
   IF (JUMP) EXIT
 END DO

 KMIN = LMIN + 1 - JR  !  Define the renge of kernel values
 KMAX = LMAX + 1 - JR  !  used for RHOTRP(1)

! Complete definition of kernel values by evaluating transform of
! maximum RHO = RHOTRP (NRMGT)

 JR = NRMGT
 Y1 = -LOG (RD) - DBLE (JR-1) * DELTA - SHFTJN
 KBOT = JNLO + 1 - JR
 K1 = MAX (KBOT,KMIN)

 DO K = K1, KMAX          ! Compute EHR for maximum RHO using previously
   L = K - 1 + JR         ! computed kernel values.
   EHRI(JR,1) = EHRI(JR,1) + KER(K,1) * WJ1(L)
   EHRI(JR,2) = EHRI(JR,2) + KER(K,2) * WJ0(L)
   EHRI(JR,3) = EHRI(JR,3) + KER(K,3) * WJ0(L)
   DO J = 4,NINTG
     EHRI(JR,J) = EHRI(JR,J) + KER(K,J) * WJ1(L)
   END DO
 END DO

 IF (K1 > KBOT) THEN    !  Add low end kernel values until convergence.
   DO K = K1-1, KBOT, -1
     KMIN = K
     L = K - 1 + JR
     Y = Y1 + DBLE (L) * DELTA
     LMBDA = EXP (Y)
     CALL EGTRX_KER (NRMGT,K,JR,L,LMBDA,SXLYR,RXLYR,KFG,NLYR,THKD,DPTHL, &
                     SIGL,KSQL,RMUD,ZR,ZS,NINTG,KER,EHRI,JUMP)
     IF (JUMP) EXIT
   END DO
 END IF

 DO JR = 2, NRMGT-1          !  Compute transforms for all other RHO values
   DO K = KMIN, KMAX         !  using previously computed kernel values.
     L = K - 1 + JR
     EHRI(JR,1) = EHRI(JR,1) + KER(K,1) * WJ1(L)
     EHRI(JR,2) = EHRI(JR,2) + KER(K,2) * WJ0(L)
     EHRI(JR,3) = EHRI(JR,3) + KER(K,3) * WJ0(L)
     DO J = 4,NINTG
       EHRI(JR,J) = EHRI(JR,J) + KER(K,J) * WJ1(L)
     END DO
   END DO
 END DO

 DO JR = 1,NRMGT
   RHOD = REAL (RHOTRP(JR),KIND=QL)
   EHRI(JR,1) = EHRI(JR,1) / RHOD
   EHRI(JR,1:NINTG) = EHRI(JR,1:NINTG) / (FOURPI * SIGL(RXLYR) * RHOD)
 END DO

 END SUBROUTINE EGTRX_HNK

 SUBROUTINE EGTRX_KER (NRMGT,K,JR,L,LMBDA,SXLYR,RXLYR,KFG,NLYR,THKD,DPTHL, &
                       SIGL,KSQL,RMUD,ZR,ZS,NINTG,KER,EHRI,JUMP)
! --------------------------------------------------------------------------

!***  Called by EGTRX_HNK

!  Accumulates the integrals EHRI(J) (J=1,5) for 5 inverse Hankel transforms
!  needed for evaluation of electric Green's tensor elements.

!     NRMGT - number of points in 15 point / decade array
!         K - filter kernel index
!        JR - RHO interpolation index
!         L - filter point index
!     LMBDA - Hankel transform variable
!      NLYR - number of layers
!      SIGL - complex conductivity of layers
!      KSQL - propagation constants
!      THKD - layer thicknesses
!        ZS - depth of induced "source point" in plate
!     NINTG - number of integrals for filter evaluation
!       KER - kernel holding filter computations
!   EHRI(J) - accumulated integrals for real and imaginary parts
!             of inverse Hankel transform (J = 1,5)
!      JUMP - keep integrating if false.

 USE LG_Filter_Coefficients

 IMPLICIT NONE
 REAL, PARAMETER :: TOL=1.E-5
 COMPLEX(KIND=QL), PARAMETER :: ZERO=(0._QL, 0._QL)
 INTEGER NINTG,NRMGT,K,JR,L,SXLYR,RXLYR,NLYR,KFG,J
 REAL (KIND=QL) RMUD(0:NLYR),LMBDA,LMBSQ,ZS,ZR,AR,AI,MHR,MHI
 REAL(KIND=QL), DIMENSION (NLYR) ::  THKD,DPTHL
 COMPLEX (KIND=QL) S(0:NLYR),SM,SL,XI_V,F_V,F_H,ETA_V,G_V,G_H,EU,ED,FACV,KF,KG,K4,K5,K6, &
                   KER(JNLO-NRMGT:JNHI,6),EHRI(NRMGT,6),TMP(6)
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL
 LOGICAL TOO_BIG,JUMP

 TMP = ZERO
 KER(K,1:6) = ZERO
 SXLYR = NLYR
 CALL EDSX_COEF (SXLYR,RXLYR,KFG,LMBDA,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                 ZS,S,XI_V,F_V,F_H,ETA_V,G_V,G_H)

 LMBSQ = LMBDA**2
 SM = S(SXLYR)
 SL = S(RXLYR)
 FACV = RMUD(SXLYR) * KSQL(RXLYR) / (RMUD(RXLYR) * SM)

 IF (RXLYR == NLYR) THEN                       ! Rx in Basement
   ED = EXP (SM * (DPTHL(NLYR) - ZR))
   ETA_V = ED * ETA_V
   G_V   = ED * G_V
   G_H   = ED * G_H
   KF = -SL * G_H
   KG = FACV * ETA_V
   K4 = -G_V
   K5 = G_H
   K6 = G_V

 ELSE                                           ! Rx in intermediate layer
   EU = EXP (SL * (ZR - DPTHL(RXLYR+1)))
   ED = EXP (SL * (DPTHL(RXLYR) - ZR))
   XI_V  = EU * XI_V
   ETA_V = ED * ETA_V
   F_V   = EU * F_V
   G_V   = ED * G_V
   F_H   = EU * F_H
   G_H   = ED * G_H
   KG = FACV * (XI_V + ETA_V)
   KF = SL * (F_H - G_H)
   K4 = F_V - G_V
   K5 = F_H + G_H
   K6 = F_V + G_V

 END IF

 KER(K,1) = KG - KF
 KER(K,2) = KER(K,1) * LMBDA
 KER(K,3) = KG * LMBDA
 KER(K,4) = K4 * LMBSQ * SL / SM
 KER(K,5) = K5 * LMBSQ
 KER(K,6) = K6 * LMBDA**3 / SM

 TMP(1) =  KER(K,1) * WJ1(L)
 TMP(2) =  KER(K,2) * WJ0(L)
 TMP(3) =  KER(K,3) * WJ0(L)
 DO J = 4,NINTG
   TMP(J) =  KER(K,J) * WJ1(L)
 END DO

! Accumulate 4 integrals simultaneously until convergence of all.

 JUMP = .TRUE.
 DO J = 1, NINTG
   EHRI(JR,J) = EHRI(JR,J) + TMP(J)
   AR = ABS (REAL (TMP(J)))
   AI = ABS (AIMAG (TMP(J)))
   MHR = ABS (REAL (EHRI(JR,J)))
   MHI = ABS (AIMAG (EHRI(JR,J)))
   TOO_BIG = .FALSE.
   IF (AR > TOL* MHR) TOO_BIG = .TRUE.
   IF (AI > TOL* MHI) TOO_BIG = .TRUE.
   IF (TOO_BIG) JUMP = .FALSE.
 END DO

 END SUBROUTINE EGTRX_KER

 SUBROUTINE PRM_BOSS_LP (SOURCE_TYPE,NTX,MXVRTX,NVRTX,SXN,SXE,SXZ,NRPRM,RHOTRP,NLYR,  &
                         THKD,DPTHL,RMUD,SIGL,KSQL,NPLT,PLYR,PLAZM,PLDIP,PLUNJ,MXAB, &
                         NB,NA,XCELL,YCELL,ZCELL,NZ1,ZV1,E_PRYM)
! ----------------------------------------------------------------------------------

!***  Called by MAIN
!***  Calls PRM_HNK_LP, PRM_HNK_FZ, CUBSPL, C2DINTRP, SETPRM

!  This rutine is used for basement plates.  It is based on interpolated Green's
!  that can be used for all plates in the basement simultaneously.
!
!  Computes the primary electric field, E_PRYM, due to a closed loop or grounded wire
!  (open loop) source, in NPLT plates lying in the basement of a host with NLYR-1
!  layers above basement.  The computation is modelled as an integrtion around the
!  loop perimeter.  In this version, the loops must be horizontal but can be at any
!  depth.  Open loops must have grounded electrodes.
!
!  The initial computations occur in a coordinate system whose X component lies in
!  the direction of each loop segment in turn.  The coordinate system is rotated
!  as each loop segment is considered.  For closed loop sources, the non-conservative
!  part of the Gzx potential contributions are integrated around the loop.
!  For grouned wire sources, the Fzx potential and thye conservative part of the Gxz
!  potentials are path independent allowing evaluation across the open side instead
!  of between all the wire segments.

!  Each target (plate) is discretised along strike into NA cells of length DA
!  and down dip into NB cells of width DB.
!
!  For a vertical plate along the X axis, the cells are numbered along strike
!  from the South to the North, first along the top row and then in the same
!  direction along lower rows.  The last cell would be in the bottom
!  North corner.  In plan view, the X and Y components of E_PRYM are positive
!  to the North and East respectively.
!
!          INPUT
!          -----
!
!          NTX - number of source positions
!       MXVRTX - maximum number of vertices for any transmitter
!     NVRTX(J) - number of vertices for transmitter J
!     SXE(I,J) = local east coordinate of vertex I for loop position J
!     SXN(I,J) = local coordinate of vertex I for loop position J
!      RHOTRP - horizontal interpolation array (15 points / decade) of dimension NRPRM
!        NLYR - number of layers including basement. (1 or 2 only)
!        PLYR - layer containing plate
!        THKD - layer thicknesses
!        RMUD - relative magetic permeability
!        SIGL - complex xconductivity for all layers including Cole-Cole & dielectric
!        KSQL - iwu * SIGL for all layers (includes Cole-Cole)
!        NPLT - number of plates
!       PLAZM - strike angle in radians
!       PLDIP - dip angle
!       PLUNJ - plunge angle
!        MXAB - maximum number of cells in one plate
!      NA, NB - number of cells along strike & down dip respectively for each plate
!  XCELL(k,*) - north coordinate of centre of cell k
!  YCELL(k,*) - east coordinate of centre of cell k
!  ZCELL(k,*) - depth of centre of cell k
!
!          OUTPUT
!          ------
!
!  E_PRYM(1,*) - complex primary electric fields along strike.
!  E_PRYM(2,*) - complex primary electric fields down-dip.
!
!    The first NAB components of E_PRYM are the fields along strike and
!    the second group of NA*NB components are along dip.  Thus for a strike
!    along the X axis, the first NA*NB components would be the X component
!    of the primary electric field and the second NA*NB components
!    would be the Y component multiplied by the cosine of the dip angle.
! -------------------------------------------------------------------------

 USE LG_Filter_Coefficients
 IMPLICIT NONE
 INTEGER, PARAMETER :: NDIP0=5, MXDIP=100
 REAL, PARAMETER :: DIPL0 = 5.
 COMPLEX, PARAMETER :: ZERO = (0., 0.)
 INTEGER NTX,MXVRTX,NVRTX(NTX),NVRL,NVRLS,NRPRM,NLYR,NPLT,PLYR(NPLT),MXAB,NA(NPLT),NZ1,JZ, &
         NB(NPLT),JAB,JP,JS,JB,JA,JD,JV,JV1,SOURCE_TYPE,SXLYR,KFG,NDIP(MXVRTX,NTX),I1
 REAL, DIMENSION (MXVRTX,NTX) :: SXN,SXE,WYRL,DIPL,CPHI,SPHI
 REAL, DIMENSION (NPLT) :: PLAZM,PLDIP,PLUNJ
 REAL SXZ(NTX),RHOTRP(NRPRM),CSTR,SSTR,CDIP,SDIP,CPLN,SPLN,ZV1(NZ1),XCELL(MXAB,NPLT), &
      YCELL(MXAB,NPLT),ZCELL(MXAB,NPLT),CPHI2,SPHI2,RHO1,RHO2,X1,X1D,X2D,YD,ZR,RSQ,R23(2,3)
 REAL, ALLOCATABLE, DIMENSION(:,:,:) :: G1R,G1I,G2R,G2I,G3R,G3I
 REAL(KIND=QL) THKD(NLYR),DPTHL(NLYR),RMUD(0:NLYR),ZS,ZPRV
 REAL, ALLOCATABLE :: QR(:,:),QI(:,:)
 REAL(KIND=QL), ALLOCATABLE :: ZV1Q(:)
 COMPLEX CCS1,CCS2,KBSQ,KBASE,EX0,EX1,EY1,EZ1,E_PRYM(2,MXAB,NTX,NPLT),C2DINTRP,EAB(2),EXYZ(3)
 COMPLEX(KIND=QL), DIMENSION(NLYR) :: KSQL, SIGL
 COMPLEX(KIND=QL), DIMENSION(:,:,:), ALLOCATABLE :: HLYRD

!  Set depth array over which to interpolate Green's function integrals

 ALLOCATE (G1R(4,NRPRM,NZ1),G1I(4,NRPRM,NZ1),G2R(4,NRPRM,NZ1),G2I(4,NRPRM,NZ1),G3R(4,NRPRM,NZ1), &
           G3I(4,NRPRM,NZ1),HLYRD(NRPRM,3,NZ1),QR(4,NRPRM),QI(4,NRPRM),ZV1Q(NZ1))

 ZV1Q(1:NZ1) = REAL (ZV1(1:NZ1),QL)
 HLYRD = (0._QL, 0._QL)
 E_PRYM = (0.,0.)
 ZPRV = -9.D4
 I1 = 2                    ! electric source

 KBSQ = CMPLX (KSQL(NLYR))
 KBASE = SQRT (KBSQ)
 IF (REAL (KBASE) < 0.) KBASE = -KBASE

 SOURCE_LOOP: DO JS = 1,NTX
   ZS = REAL (SXZ(JS),QL)
   IF (ABS (ZS - ZPRV) > 1.D-4) THEN
     ZPRV = ZS
       SXLYR = 0
       DO JZ = NLYR,1,-1
         IF (ZS > DPTHL(JZ)) THEN
           SXLYR = JZ
           EXIT
         END IF
       END DO
     CALL SET_KFG (I1,NLYR,SXLYR,NLYR,KFG)

     IF (SOURCE_TYPE == 1) THEN   ! Closed loop
       CALL PRM_HNK_CL (NRPRM,RHOTRP,NZ1,ZV1Q,ZS,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,KFG,SXLYR,HLYRD)
     ELSE                         ! Open loop
       CALL PRM_HNK_OL (NRPRM,RHOTRP,NZ1,ZV1Q,ZS,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,KFG,SXLYR,HLYRD)
     END IF

     DO JZ = 1,NZ1
       QR(1,1:NRPRM) =        REAL (HLYRD(1:NRPRM,1,JZ))
       QI(1,1:NRPRM) = REAL (AIMAG (HLYRD(1:NRPRM,1,JZ)) )   ! HLYRD is in QL precision
       CALL CUBSPL (RHOTRP,QR,NRPRM)
       CALL CUBSPL (RHOTRP,QI,NRPRM)
       G1R(1:4,1:NRPRM,JZ) = QR(1:4,1:NRPRM)
       G1I(1:4,1:NRPRM,JZ) = QI(1:4,1:NRPRM)

       IF (SOURCE_TYPE == 2) THEN
         QR(1,1:NRPRM) =        REAL (HLYRD(1:NRPRM,2,JZ))
         QI(1,1:NRPRM) = REAL (AIMAG (HLYRD(1:NRPRM,2,JZ)) )
         CALL CUBSPL (RHOTRP,QR,NRPRM)
         CALL CUBSPL (RHOTRP,QI,NRPRM)
         G2R(1:4,1:NRPRM,JZ) = QR(1:4,1:NRPRM)
         G2I(1:4,1:NRPRM,JZ) = QI(1:4,1:NRPRM)

         QR(1,1:NRPRM) =        REAL (HLYRD(1:NRPRM,3,JZ))
         QI(1,1:NRPRM) = REAL (AIMAG (HLYRD(1:NRPRM,3,JZ)) )
         CALL CUBSPL (RHOTRP,QR,NRPRM)
         CALL CUBSPL (RHOTRP,QI,NRPRM)
         G3R(1:4,1:NRPRM,JZ) = QR(1:4,1:NRPRM)
         G3I(1:4,1:NRPRM,JZ) = QI(1:4,1:NRPRM)
       END IF
     END DO
   END IF

   NVRL = NVRTX(JS)
   DO JV = 1, NVRL
     JV1 = JV + 1             ! Wire JV goes from vertex JV to vertex JV1
     IF (JV == NVRL) JV1 = 1

! Length & orientation of Wire JV between vertices JV & JV1

     WYRL(JV,JS) = SQRT ( (SXN(JV,JS) - SXN(JV1,JS))**2 + (SXE(JV,JS) - SXE(JV1,JS))**2)
     CPHI(JV,JS) = (SXN(JV1,JS) - SXN(JV,JS) ) / WYRL(JV,JS)
     SPHI(JV,JS) = (SXE(JV1,JS) - SXE(JV,JS) ) / WYRL(JV,JS)

! Divide each wire into segment lengths of 5 m with a minimum of 5 segments per wire.

     NDIP(JV,JS) = CEILING (WYRL(JV,JS) / DIPL0)  ! 5 m initial dipole length
     NDIP(JV,JS) = MAX (NDIP(JV,JS), NDIP0)       ! At least 5 dipoles per segment
     NDIP(JV,JS) = MIN (NDIP(JV,JS), MXDIP)
     DIPL(JV,JS) = WYRL(JV,JS) / REAL (NDIP(JV,JS))
   END DO
   IF (SOURCE_TYPE == 2) THEN
     CPHI(NVRL,JS) = (SXN(NVRL,JS) - SXN(1,JS) ) / WYRL(NVRL,JS)
     SPHI(NVRL,JS) = (SXE(NVRL,JS) - SXE(1,JS) ) / WYRL(NVRL,JS)
   END IF

!  Step through depths.  Add up contributions for each dipole for each transmitter.

   PLATE_LOOP: DO JP = 1, NPLT
     IF (PLYR(JP) < NLYR) CYCLE
     CSTR = COS (PLAZM(JP) )
     SSTR = SIN (PLAZM(JP) )
     CDIP = COS (PLDIP(JP) )
     SDIP = SIN (PLDIP(JP) )
     CPLN = COS (PLUNJ(JP) )
     SPLN = SIN (PLUNJ(JP) )

     DEPTH_STEP: DO JB = 1, NB(JP)
       NVRL = NVRTX(JS)
       NVRLS = NVRL
       IF (SOURCE_TYPE == 2) NVRLS = NVRL -1
       STRYK_STEP: DO JA = 1, NA(JP)
         JAB = JA + (JB-1)*NA(JP)

         DO JV = 1,NVRLS
           JV1 = JV + 1             ! Wire JV goes from vertex JV to vertex JV1
           IF (JV == NVRL) JV1 = 1  ! Closed loop only

           EX0 = ZERO
           CPHI2 = CSTR * CPHI(JV,JS) + SSTR * SPHI(JV,JS)  !  strike angle wrt loop segment
           SPHI2 = SSTR * CPHI(JV,JS) - CSTR * SPHI(JV,JS)
           YD = (YCELL(JAB,JP) - SXE(JV,JS)) * CPHI(JV,JS) &
              - (XCELL(JAB,JP) - SXN(JV,JS)) * SPHI(JV,JS)
           X1D = (XCELL(JAB,JP) - SXN(JV,JS) ) * CPHI(JV,JS) &
               + (YCELL(JAB,JP) - SXE(JV,JS) ) * SPHI(JV,JS)
           ZR = ZCELL (JAB,JP)

           DO JD = 1, NDIP(JV,JS)
             X1 = X1D - (JD - 0.5) * DIPL(JV,JS)
             RSQ = X1**2 + YD**2
             RHO1 = SQRT (RSQ)
             CCS1 = C2DINTRP (RHOTRP,NRPRM,ZV1,NZ1,G1R,G1I,RHO1,ZR)
             EX0 = EX0 + CCS1
           END DO
           EX0 = DIPL(JV,JS) * EX0

           CALL RXYZ2PLT (CPHI2,SPHI2,CDIP,SDIP,CPLN,SPLN,R23)
           EXYZ = ZERO
           EXYZ(1) = EX0
           EAB = MATMUL (R23, EXYZ)

           E_PRYM(1,JAB,JS,JP) = E_PRYM(1,JAB,JS,JP) + EAB(1)
           E_PRYM(2,JAB,JS,JP) = E_PRYM(2,JAB,JS,JP) + EAB(2) !  No vertical field for closed loop

         END DO
         IF (SOURCE_TYPE == 2 ) THEN    ! Integrate over open side for GW source.
           X1D = (XCELL(JAB,JP) - SXN(1,JS)   ) * CPHI(NVRL,JS) &
               + (YCELL(JAB,JP) - SXE(1,JS)   ) * SPHI(NVRL,JS)
           X2D = (XCELL(JAB,JP) - SXN(NVRL,JS)) * CPHI(NVRL,JS) &
               + (YCELL(JAB,JP) - SXE(NVRL,JS)) * SPHI(NVRL,JS)
           YD = (YCELL(JAB,JP) - SXE(1,JS))    * CPHI(NVRL,JS) &
              - (XCELL(JAB,JP) - SXN(1,JS))    * SPHI(NVRL,JS)

           RHO1 = SQRT (X1D**2 + YD**2)
           RHO2 = SQRT (X2D**2 + YD**2)

           CPHI2 = CSTR * CPHI(NVRL,JS) + SSTR * SPHI(NVRL,JS)  !  strike angle wrt loop segment
           SPHI2 = SSTR * CPHI(NVRL,JS) - CSTR * SPHI(NVRL,JS)
           CCS1 = C2DINTRP (RHOTRP,NRPRM,ZV1,NZ1,G2R,G2I,RHO1,ZR)
           CCS2 = C2DINTRP (RHOTRP,NRPRM,ZV1,NZ1,G2R,G2I,RHO2,ZR)

           EX1 = X2D * CCS2 - X1D * CCS1
           EY1 = YD * (CCS2 - CCS1)
           CCS1 = C2DINTRP (RHOTRP,NRPRM,ZV1,NZ1,G3R,G3I,RHO1,ZR)
           CCS2 = C2DINTRP (RHOTRP,NRPRM,ZV1,NZ1,G3R,G3I,RHO2,ZR)
           EZ1 = CCS2 - CCS1

           CALL RXYZ2PLT (CPHI2,SPHI2,CDIP,SDIP,CPLN,SPLN,R23)
           EXYZ(1) = EX1;   EXYZ(2) = EY1;  EXYZ(3) = EZ1
           EAB = MATMUL (R23, EXYZ)

           E_PRYM(1,JAB,JS,JP) = E_PRYM(1,JAB,JS,JP) + EAB(1)
           E_PRYM(2,JAB,JS,JP) = E_PRYM(2,JAB,JS,JP) + EAB(2)
         END IF
       END DO STRYK_STEP
     END DO DEPTH_STEP
   END DO PLATE_LOOP
 END DO SOURCE_LOOP

 DEALLOCATE (HLYRD,QR,QI,ZV1Q,G1R,G1I,G2R,G2I,G3R,G3I)

 END SUBROUTINE PRM_BOSS_LP

 SUBROUTINE PRM_HNK_CL (NRPRM,RHOTRP,NZ1,ZV1Q,ZS,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,KFG,SXLYR,HLYRD)
!-----------------------------------------------------------------------------------------------

!***  Calls PRM_KER_CL
!***  Called by PRM_BOSS_LP

!  Computes integrals needed to evaluate the primary electric field in the
!  basement due to a closed loop source at any depth or elevation.
!  It uses the flow through Hankel transform method to evaluate the three Hankel
!  integrals at discrete depths.  The Hankel transform integral uses a 15 point
!  per decade filter coefficient set derived from Christensen's FLTGEN program.

!       NRPRM - number of horizontal interpolation points
!      RHOTRP - 15 points per decade interpolation array
!         NZ1 - number of interpolation depths
!        ZV1Q - array of interpolation depths
!          ZS - transmitter depth
!        NLYR - number of layers including basement. (1 or 2 only)
!        SIGL - complex xconductivity for all layers including Cole-Cole & dielectric
!        KSQL - iwu * SIGL for all layers (includes Cole-Cole)
!        RMUD - relative magetic permeability
!        THKD - thickness of each layer above basement
!       DPTHL - depth to top of each layer including basement
!         KFG - Case selector variable
!
!  HLYRD(JR,1,JB) - only component needed for both closed & open loop
!  HLYRD(JR,2,JB) - integrands for grounded wire segment
!

 USE LG_Filter_Coefficients

 IMPLICIT NONE
 REAL(KIND=QL), PARAMETER :: FOURPI = 12.56637061_QL
 COMPLEX(KIND=QL), PARAMETER :: ZERO=(0._QL,0._QL)
 INTEGER L,LMAX,LMIN,K,KMAX,KMIN,KBOT,K1,JR,NZ1,NLYR,KFG,SXLYR,JB,NRPRM
 REAL RHOTRP(NRPRM)
 REAL(KIND=QL) RMUD(0:NLYR),ZS,DELTA,Y1,Y,RD,LMBDA,ZV1Q(NZ1),RHOD
 REAL(KIND=QL), DIMENSION (NLYR) :: THKD,DPTHL
 COMPLEX(KIND=QL) HLYRD(NRPRM,3,NZ1),KER(JNLO-NRPRM:JNHI,3,NZ1)
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL
 LOGICAL JUMP

 KER = ZERO
 DELTA = LOG (10.D0)/ DBLE (NDEC_JN)

! Basic integral for open and closed loops.
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
   LMBDA = EXP (SNGL(Y))
   CALL PRM_KER_CL (NRPRM,K,JR,L,LMBDA,NZ1,ZV1Q,ZS,NLYR,THKD,DPTHL, &
                    RMUD,SIGL,KSQL,KFG,SXLYR,KER,HLYRD,JUMP)
   IF (JUMP) EXIT
   END DO

 Y = Y1                       ! Finish off the low end for RHOTRP(1)
 DO L = -51, JNLO, -1
   LMIN = L                   ! Minimum filter index used
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
   K = L + 1 - JR             ! Compute the kernel index.
   CALL PRM_KER_CL (NRPRM,K,JR,L,LMBDA,NZ1,ZV1Q,ZS,NLYR,THKD,DPTHL, &
                    RMUD,SIGL,KSQL,KFG,SXLYR,KER,HLYRD,JUMP)
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
   HLYRD(JR,1,1:NZ1) =  HLYRD(JR,1,1:NZ1) + KER(K,1,1:NZ1) * WJ0(L)
 END DO

 IF (K1 > KBOT) THEN    !  Add low end kernel values until convergence.
   DO K = K1-1, KBOT, -1
     KMIN = K
     L = K - 1 + JR
     Y = Y1 + DBLE (L) * DELTA
     LMBDA = EXP (SNGL(Y))
     CALL PRM_KER_CL (NRPRM,K,JR,L,LMBDA,NZ1,ZV1Q,ZS,NLYR,THKD,DPTHL, &
                   RMUD,SIGL,KSQL,KFG,SXLYR,KER,HLYRD,JUMP)
     IF (JUMP) EXIT
   END DO
 END IF

 DO JR = 2, NRPRM-1          !  Compute transforms for all other RHO values
   DO K = KMIN, KMAX         !  using previously computed kernel values.
     L = K - 1 + JR
     HLYRD(JR,1,1:NZ1) =  HLYRD(JR,1,1:NZ1) + KER(K,1,1:NZ1) * WJ0(L)
   END DO
 END DO

 DO JB = 1,NZ1
   DO JR = 1, NRPRM
     RHOD = REAL (RHOTRP(JR),KIND=QL)
     HLYRD(JR,1,JB) = HLYRD(JR,1,JB) / (FOURPI * RHOD * SIGL(NLYR))
   END DO
 END DO

 END SUBROUTINE PRM_HNK_CL

 SUBROUTINE PRM_KER_CL (NRPRM,K,JR,L,LMBDA,NZ1,ZV1Q,ZS,NLYR,THKD,DPTHL, &
                        RMUD,SIGL,KSQL,KFG,SXLYR,KER,HLYRD,JUMP)
!----------------------------------------------------------------------

!***  Called by PRM_HNK_LP

! Compute the kernels for closed loop integration.
! This involves only G potential terms.

!     NRPRM - number of points in 15 point / decade array
!         K - filter kernel index
!        JR - RHO interpolation index
!         L - filter point index
!     LMBDA - Hankel transform variable
!       NZ1 - number of interpolation depths
!      ZV1Q - array of interpolation depths
!        ZS - source location  (negative in air, positive in ground)
!      NLYR - number of layers including basement. (1 or 2 only)
!      SIGL - complex xconductivity for all layers including Cole-Cole & dielectric
!      KSQL - iwu * SIGL for all layers (includes Cole-Cole)
!      RMUD - relative magetic permeability
!      THKD - thickness of each layer above basement
!     DPTHL - depth to top of each layer including basement
!       KFG - Case selector variable
!       KER - kernels for hankel integraion
!
!   HLYRD(*,1,*) - needed for both closed and open loops
!   HLYRD(*,2,*) - used only for grounded wire segments
!   HLYRD(*,3,*) - used only for grounded wire segments
!
!           JUMP - keep integrating if false.

 USE LG_Filter_Coefficients
 IMPLICIT NONE
 REAL, PARAMETER :: TOL=1.E-5
 COMPLEX(KIND=QL), PARAMETER :: ZERO=(0._QL,0._QL)
 INTEGER JR,L,K,NZ1,NLYR,SXLYR,RXLYR,KFG,JZ,NRPRM
 REAL(KIND=QL) RMUD(0:NLYR),LMBDA,ZS,AR,AI,HLYRDR,HLYRDI,ZV1Q(NZ1),ZDR
 REAL(KIND=QL), DIMENSION (NLYR) :: THKD,DPTHL
 COMPLEX (KIND=QL) XI_V,F_V,F_H,ETA_V,G_V,G_H,XP,XPDIR,SL,KER(JNLO-NRPRM:JNHI,3,NZ1), &
                   FACV,KV,S(0:NLYR),HLYRD(NRPRM,3,NZ1),TMP(NZ1)
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL
 LOGICAL TOO_BIG,JUMP

 RXLYR = NLYR
 CALL EDSX_COEF (SXLYR,RXLYR,KFG,LMBDA,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                 ZS,S,XI_V,F_V,F_H,ETA_V,G_V,G_H)

 KER(K,1,1:NZ1) = ZERO
 TMP = ZERO
 SL = S(RXLYR)
 FACV = RMUD(SXLYR) * KSQL(RXLYR) / (RMUD(NLYR) * S(SXLYR))
 KV = FACV * ETA_V

 DO JZ = 1, NZ1
   XP = EXP (SL * (DPTHL(NLYR) - ZV1Q(JZ)))
   KER(K,1,JZ) = -XP * KV * LMBDA
 END DO

 IF (SXLYR == NLYR) THEN     !  Transmitter in basement
   DO JZ = 1, NZ1
     ZDR = ZS - ZV1Q(JZ)
     XPDIR = EXP (-SL * ABS (ZDR))
     KV = FACV * XPDIR
     KER(K,1,JZ) = KER(K,1,JZ) - KV * LMBDA
   END DO
 END IF

 JUMP = .TRUE.
 TMP(1:NZ1) = KER(K,1,1:NZ1) * WJ0(L)

 HLYRD(JR,1,1:NZ1) = HLYRD(JR,1,1:NZ1) + TMP(1:NZ1)
 DO JZ = 1, NZ1
   AR = ABS ( REAL (TMP(JZ)) )
   AI = ABS (AIMAG (TMP(JZ)) )
   TOO_BIG = .FALSE.
   HLYRDR = ABS ( REAL (HLYRD(JR,1,JZ)) )
   HLYRDI = ABS (AIMAG (HLYRD(JR,1,JZ)) )
   IF (AR > TOL* HLYRDR) TOO_BIG = .TRUE.
   IF (AI > TOL* HLYRDI) TOO_BIG = .TRUE.
   IF (TOO_BIG) JUMP = .FALSE.
 END DO

 END SUBROUTINE PRM_KER_CL

 SUBROUTINE PRM_HNK_OL (NRPRM,RHOTRP,NZ1,ZV1Q,ZS,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,KFG,SXLYR,HLYRD)
!-----------------------------------------------------------------------------------------------

!***  Calls PRM_KER_OL
!***  Called by PRM_BOSS_LP

!  Computes integrals needed to evaluate the primary electric field in the
!  basement due to an open loop source at any depth or elevation.
!  It uses the flow through Hankel transform method to evaluate the three Hankel
!  integrals at discrete depths.  The Hankel transform integral uses a 15 point
!  per decade filter coefficient set derived from Christensen's FLTGEN program.

!       NRPRM - number of horizontal interpolation points
!      RHOTRP - 15 points per decade interpolation array
!         NZ1 - number of interpolation depths
!        ZV1Q - array of interpolation depths
!          ZS - transmitter depth
!        NLYR - number of layers including basement. (1 or 2 only)
!        SIGL - complex xconductivity for all layers including Cole-Cole & dielectric
!        KSQL - iwu * SIGL for all layers (includes Cole-Cole)
!        RMUD - relative magetic permeability
!        THKD - thickness of each layer above basement
!       DPTHL - depth to top of each layer including basement
!         KFG - Case selector variable
!
!  HLYRD(JR,1,JB) - only component needed for both closed & open loop
!  HLYRD(JR,2,JB) - integrands for grounded wire segment
!

 USE LG_Filter_Coefficients

 IMPLICIT NONE
 REAL(KIND=QL), PARAMETER :: FOURPI = 12.56637061_QL
 COMPLEX(KIND=QL), PARAMETER :: ZERO=(0._QL,0._QL)
 INTEGER L,LMAX,LMIN,K,KMAX,KMIN,KBOT,K1,JR,NZ1,NLYR,KFG,SXLYR,JB,NRPRM
 REAL RHOTRP(NRPRM)
 REAL(KIND=QL) RMUD(0:NLYR),ZS,DELTA,Y1,Y,RD,LMBDA,ZV1Q(NZ1),RHOD
 REAL(KIND=QL), DIMENSION (NLYR) :: THKD,DPTHL
 COMPLEX(KIND=QL) HLYRD(NRPRM,3,NZ1),KER(JNLO-NRPRM:JNHI,3,NZ1)
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL
 LOGICAL JUMP

 KER = ZERO
 DELTA = LOG (10.D0)/ DBLE (NDEC_JN)

! Basic integral for open and closed loops.
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
   LMBDA = EXP (SNGL(Y))
   CALL PRM_KER_OL (NRPRM,K,JR,L,LMBDA,NZ1,ZV1Q,ZS,NLYR,THKD,DPTHL, &
                    RMUD,SIGL,KSQL,KFG,SXLYR,KER,HLYRD,JUMP)
   IF (JUMP) EXIT
   END DO

 Y = Y1                       ! Finish off the low end for RHOTRP(1)
 DO L = -51, JNLO, -1
   LMIN = L                   ! Minimum filter index used
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
   K = L + 1 - JR             ! Compute the kernel index.
   CALL PRM_KER_OL (NRPRM,K,JR,L,LMBDA,NZ1,ZV1Q,ZS,NLYR,THKD,DPTHL, &
                   RMUD,SIGL,KSQL,KFG,SXLYR,KER,HLYRD,JUMP)
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

   HLYRD(JR,1,1:NZ1) =  HLYRD(JR,1,1:NZ1) + KER(K,1,1:NZ1) * WJ0(L)
   HLYRD(JR,2,1:NZ1) =  HLYRD(JR,2,1:NZ1) + KER(K,2,1:NZ1) * WJ1(L)
   HLYRD(JR,3,1:NZ1) =  HLYRD(JR,3,1:NZ1) + KER(K,3,1:NZ1) * WJ0(L)
 END DO

 IF (K1 > KBOT) THEN    !  Add low end kernel values until convergence.
   DO K = K1-1, KBOT, -1
     KMIN = K
     L = K - 1 + JR
     Y = Y1 + DBLE (L) * DELTA
     LMBDA = EXP (SNGL(Y))
     CALL PRM_KER_OL (NRPRM,K,JR,L,LMBDA,NZ1,ZV1Q,ZS,NLYR,THKD,DPTHL, &
                      RMUD,SIGL,KSQL,KFG,SXLYR,KER,HLYRD,JUMP)
     IF (JUMP) EXIT
   END DO
 END IF

 DO JR = 2, NRPRM-1          !  Compute transforms for all other RHO values
   DO K = KMIN, KMAX         !  using previously computed kernel values.
     L = K - 1 + JR
     HLYRD(JR,1,1:NZ1) =  HLYRD(JR,1,1:NZ1) + KER(K,1,1:NZ1) * WJ0(L)
     HLYRD(JR,2,1:NZ1) =  HLYRD(JR,2,1:NZ1) + KER(K,2,1:NZ1) * WJ1(L)
     HLYRD(JR,3,1:NZ1) =  HLYRD(JR,3,1:NZ1) + KER(K,3,1:NZ1) * WJ0(L)
   END DO
 END DO

 DO JB = 1,NZ1
   DO JR = 1, NRPRM
     RHOD = REAL (RHOTRP(JR),KIND=QL)
     HLYRD(JR,1:3,JB) = HLYRD(JR,1:3,JB) / (FOURPI * RHOD * SIGL(NLYR))
     HLYRD(JR,2,JB) = HLYRD(JR,2,JB) / RHOD
   END DO
 END DO

 END SUBROUTINE PRM_HNK_OL

 SUBROUTINE PRM_KER_OL (NRPRM,K,JR,L,LMBDA,NZ1,ZV1Q,ZS,NLYR,THKD,DPTHL,RMUD, &
                        SIGL,KSQL,KFG,SXLYR,KER,HLYRD,JUMP)
!---------------------------------------------------------------------------

!***  Called by PRM_HNK_LP

!  Compute the kernels for grounded wire sources.
!  This is a mixture of G & F potential terms.

!     NRPRM - number of points in 15 point / decade array
!         K - filter kernel index
!        JR - RHO interpolation index
!         L - filter point index
!     LMBDA - Hankel transform variable
!       NZ1 - number of interpolation depths
!      ZV1Q - array of interpolation depths
!        ZS - source location  (negative in air, positive in ground)
!      NLYR - number of layers including basement. (1 or 2 only)
!      SIGL - complex xconductivity for all layers including Cole-Cole & dielectric
!      KSQL - iwu * SIGL for all layers (includes Cole-Cole)
!      RMUD - relative magetic permeability
!      THKD - thickness of each layer above basement
!     DPTHL - depth to top of each layer including basement
!       KFG - Case selector variable
!       KER - kernels for hankel integraion
!
!   HLYRD(*,1,*) - needed for both closed and open loops
!   HLYRD(*,2,*) - used only for grounded wire segments
!   HLYRD(*,3,*) - used only for grounded wire segments
!
!           JUMP - keep integrating if false.

 USE LG_Filter_Coefficients
 IMPLICIT NONE
 REAL, PARAMETER :: TOL=1.E-5
 COMPLEX(KIND=QL), PARAMETER :: ZERO=(0._QL,0._QL)
 INTEGER JR,L,K,NZ1,NLYR,RXLYR,SXLYR,KFG,JZ,NRPRM,JI
 REAL(KIND=QL) RMUD(0:NLYR),LMBDA,ZS,AR,AI,HLYRDR,HLYRDI,ZV1Q(NZ1),GAM,ZDR
 REAL(KIND=QL), DIMENSION (NLYR) :: THKD,DPTHL
 COMPLEX (KIND=QL) XI_V,F_V,F_H,ETA_V,G_V,G_H,XP,XPDIR,KER(JNLO-NRPRM:JNHI,3,NZ1), &
                   S(0:NLYR),SL,FACV,KV,KVS,HLYRD(NRPRM,3,NZ1),TMP(3,NZ1)
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL
 LOGICAL TOO_BIG,JUMP

! Compute layered-earth coefficients.  RXLYR = NLYR

 RXLYR = NLYR
 CALL EDSX_COEF (SXLYR,RXLYR,KFG,LMBDA,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                 ZS,S,XI_V,F_V,F_H,ETA_V,G_V,G_H)
 KER(K,1:3,1:NZ1) = ZERO
 TMP = ZERO
 SL = S(RXLYR)

 FACV = RMUD(SXLYR) * KSQL(RXLYR) / (RMUD(NLYR) * S(SXLYR))
 KV = FACV * ETA_V
 KVS = KV + G_H * SL

 DO JZ = 1, NZ1
   XP = EXP (SL * (DPTHL(NLYR) - ZV1Q(JZ)))
   KER(K,1,JZ) = -XP * KV * LMBDA
   KER(K,2,JZ) = -XP * KVS
   KER(K,3,JZ) = -XP * G_H * LMBDA
 END DO

 IF (SXLYR == NLYR) THEN     !  Transmitter in basement
   DO JZ = 1, NZ1
     ZDR = ZS - ZV1Q(JZ)
     GAM = SIGN (1.0_QL, ZDR)
     XPDIR = EXP (-SL * ABS (ZDR))
     KV = FACV * XPDIR
     KVS = KV - XPDIR * SL

     KER(K,1,JZ) = KER(K,1,JZ) - KV * LMBDA
     KER(K,2,JZ) = KER(K,2,JZ) - KVS
     KER(K,3,JZ) = KER(K,3,JZ) - GAM * XPDIR * LMBDA
   END DO
 END IF

 JUMP = .TRUE.
 TMP(1,1:NZ1) = KER(K,1,1:NZ1) * WJ0(L)
 TMP(2,1:NZ1) = KER(K,2,1:NZ1) * WJ1(L)
 TMP(3,1:NZ1) = KER(K,3,1:NZ1) * WJ0(L)

 HLYRD(JR,1:3,1:NZ1) = HLYRD(JR,1:3,1:NZ1) + TMP(1:3,1:NZ1)
 DO JZ = 1, NZ1
   DO JI = 1,3
     AR = ABS ( REAL (TMP(JI,JZ)) )
     AI = ABS (AIMAG (TMP(JI,JZ)) )
     TOO_BIG = .FALSE.
     HLYRDR = ABS ( REAL (HLYRD(JR,JI,JZ)) )
     HLYRDI = ABS (AIMAG (HLYRD(JR,JI,JZ)) )
     IF (AR > TOL* HLYRDR) TOO_BIG = .TRUE.
     IF (AI > TOL* HLYRDI) TOO_BIG = .TRUE.
     IF (TOO_BIG) JUMP = .FALSE.
   END DO
 END DO
 END SUBROUTINE PRM_KER_OL

 SUBROUTINE PRM_BOSS_UL_LP (JP,SOURCE_TYPE,NTX,MXVRTX,NVRTX,SXN,SXE,SXZ,NRPRM,RHOTRP,NLYR, &
                            THKD,DPTHL,RMUD,SIGL,KSQL,NPLT,PLYR,PLAZM,PLDIP,PLUNJ,MXAB,    &
                            MXB,NB,NA,XCELL,YCELL,ZC1L,E_PRYM)
! ---------------------------------------------------------------------------------------

!***  Called by MAIN
!***  Calls PRM_HNK1_LP, PRM_HNK1_FZ, CUBSPL, SETPRM

!  This rutine is used for plates above basement.  It deals with one plate at a time.
!
!  Computes the primary electric field, E_PRYM, due to a closed loop or grounded wire
!  (open loop) source for a plate lying above thebasement of a host with NLYR-1
!  layers above basement.  The computation is modelled as an integrtion around the
!  loop perimeter.  In this version, the loops must be horizontal but can be at any
!  depth.  Open loops must have grounded electrodes.
!
!  The initial computations occur in a coordinate system whose X component lies in
!  the direction of each loop segment in turn.  The coordinate system is rotated
!  as each loop segment is considered.  For closed loop sources, the non-conservative
!  part of the Gzx potential contributions are integrated around the loop.
!  For grouned wire sources, the Fzx potential and thye conservative part of the Gxz
!  potentials are path independent allowing evaluation across the open side instead
!  of between all the wire segments.

!  Each target (plate) is discretised along strike into NA cells of length DA
!  and down dip into NB cells of width DB.
!
!  For a vertical plate along the X axis, the cells are numbered along strike
!  from the South to the North, first along the top row and then in the same
!  direction along lower rows.  The last cell would be in the bottom
!  North corner.  In plan view, the X and Y components of E_PRYM are positive
!  to the North and East respectively.
!
!          INPUT
!          -----
!
!          NTX - number of source positions
!       MXVRTX - maximum number of vertices for any transmitter
!     NVRTX(J) - number of vertices for transmitter J
!     SXE(I,J) = local east coordinate of vertex I for loop position J
!     SXN(I,J) = local coordinate of vertex I for loop position J
!      RHOTRP - horizontal interpolation array (15 points / decade) of dimension NRPRM
!        NLYR - number of layers including basement. (1 or 2 only)
!        THKD - layer thicknesses
!        RMUD - relative magetic permeability
!        SIGL - complex xconductivity for all layers including Cole-Cole & dielectric
!        KSQL - iwu * SIGL for all layers (includes Cole-Cole)
!        NPLT - number of plates
!       PLAZM - strike angle in radians
!       PLDIP - dip angle
!       PLUNJ - plunge angle
!        MXAB - maximum number of cells in one plate
!         MXB - maximum number of cell rows of any plate above basement
!      NA, NB - number of cells along strike & down dip respectively for each plate
!  XCELL(k,*) - north coordinate of centre of cell k
!  YCELL(k,*) - east coordinate of centre of cell k
!        ZC1L - depth of row centres in Plate JP
!
!          OUTPUT
!          ------
!
!  E_PRYM(1,*) - complex primary electric fields along strike.
!  E_PRYM(2,*) - complex primary electric fields down-dip.
!
!    The first NAB components of E_PRYM are the fields along strike and
!    the second group of NA*NB components are along dip.  Thus for a strike
!    along the X axis, the first NA*NB components would be the X component
!    of the primary electric field and the second NA*NB components
!    would be the Y component multiplied by the cosine of the dip angle.
! -------------------------------------------------------------------------

 USE LG_Filter_Coefficients
 IMPLICIT NONE
 INTEGER, PARAMETER :: NDIP0=5, MXDIP=100
 REAL, PARAMETER :: DIPL0 = 5.
 COMPLEX, PARAMETER :: ZERO = (0., 0.)
 INTEGER NTX,MXVRTX,NVRTX(NTX),NVRL,NVRLS,NRPRM,NLYR,NPLT,PLYR(NPLT),MXAB,MXB,NA(NPLT),NB(NPLT), &
         NROW,JZ,JAB,JP,JS,JB,JA,JD,JV,JV1,SOURCE_TYPE,SXLYR,RXLYR,KFG,NDIP(MXVRTX,NTX),I1
 REAL, DIMENSION (MXVRTX,NTX) :: SXN,SXE,WYRL,DIPL,CPHI,SPHI
 REAL, DIMENSION (NPLT) :: PLAZM,PLDIP,PLUNJ
 REAL SXZ(NTX),RHOTRP(NRPRM),CSTR,SSTR,CDIP,SDIP,CPLN,SPLN,XCELL(MXAB,NPLT), &
      YCELL(MXAB,NPLT),ZC1L(MXB),CPHI2,SPHI2,RHO1,RHO2,X1,X1D,X2D,YD,RSQ,R23(2,3)
 REAL, ALLOCATABLE, DIMENSION(:,:) :: G1R,G1I,G2R,G2I,G3R,G3I
 REAL(KIND=QL) THKD(NLYR),DPTHL(NLYR),RMUD(0:NLYR),ZS,ZPRV
 COMPLEX CCS1,CCS2,EX0,EX1,EY1,EZ1,E_PRYM(2,MXAB,NTX,NPLT),EAB(2),EXYZ(3)
 COMPLEX(KIND=QL), DIMENSION(NLYR) :: KSQL, SIGL
 COMPLEX(KIND=QL), DIMENSION(:,:,:), ALLOCATABLE :: HLYRD

!  Set depth array over which to interpolate Green's function integrals

 NROW = NB(JP)
 RXLYR = PLYR(JP)
 IF (RXLYR == NLYR) THEN
   WRITE(4,1); WRITE(*,1)
   STOP
 END IF

 ALLOCATE (HLYRD(NRPRM,3,NROW),G1R(4,NRPRM),G1I(4,NRPRM),G2R(4,NRPRM), &
           G2I(4,NRPRM),G3R(4,NRPRM),G3I(4,NRPRM))

 I1 = 2                    ! electric source
 HLYRD = (0._QL, 0._QL)
 E_PRYM(1:2,1:MXAB,1:NTX,JP) = (0.,0.)
 ZPRV = -9.D4

 SOURCE_LOOP: DO JS = 1,NTX
   ZS = REAL (SXZ(JS),QL)
   IF (ABS (ZS - ZPRV) > 1.D-4) THEN
     ZPRV = ZS
       SXLYR = 0
       DO JZ = NLYR,1,-1
         IF (ZS > DPTHL(JZ)) THEN
           SXLYR = JZ
           EXIT
         END IF
       END DO
     CALL SET_KFG (I1,NLYR,SXLYR,RXLYR,KFG)

     IF (SOURCE_TYPE == 1 .OR. SOURCE_TYPE == 4) THEN   ! Closed loop
       CALL PRM_HNK1_CL (NRPRM,RHOTRP,NROW,ZC1L,ZS,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,SXLYR,RXLYR,KFG,HLYRD)
     ELSE IF (SOURCE_TYPE == 2) THEN                     ! Open loop
       CALL PRM_HNK1_OL (NRPRM,RHOTRP,NROW,ZC1L,ZS,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,SXLYR,RXLYR,KFG,HLYRD)
     END IF
   END IF

   NVRL = NVRTX(JS)
   DO JV = 1, NVRL
     JV1 = JV + 1             ! Wire JV goes from vertex JV to vertex JV1
     IF (JV == NVRL) JV1 = 1

! Length & orientation of Wire JV between vertices JV & JV1

     WYRL(JV,JS) = SQRT ( (SXN(JV,JS) - SXN(JV1,JS))**2 + (SXE(JV,JS) - SXE(JV1,JS))**2)
     CPHI(JV,JS) = (SXN(JV1,JS) - SXN(JV,JS) ) / WYRL(JV,JS)
     SPHI(JV,JS) = (SXE(JV1,JS) - SXE(JV,JS) ) / WYRL(JV,JS)

! Divide each wire into segment lengths of 5 m with a minimum of 5 segments per wire.

     NDIP(JV,JS) = CEILING (WYRL(JV,JS) / DIPL0)  ! 5 m initial dipole length
     NDIP(JV,JS) = MAX (NDIP(JV,JS), NDIP0)       ! At least 5 dipoles per segment
     NDIP(JV,JS) = MIN (NDIP(JV,JS), MXDIP)
     DIPL(JV,JS) = WYRL(JV,JS) / REAL (NDIP(JV,JS))
   END DO
   IF (SOURCE_TYPE == 2) THEN
     CPHI(NVRL,JS) = (SXN(NVRL,JS) - SXN(1,JS) ) / WYRL(NVRL,JS)
     SPHI(NVRL,JS) = (SXE(NVRL,JS) - SXE(1,JS) ) / WYRL(NVRL,JS)
   END IF

!  Step through depths.  Add up contributions for each dipole for each transmitter.

   CSTR = COS (PLAZM(JP) )
   SSTR = SIN (PLAZM(JP) )
   CDIP = COS (PLDIP(JP) )
   SDIP = SIN (PLDIP(JP) )
   CPLN = COS (PLUNJ(JP) )
   SPLN = SIN (PLUNJ(JP) )

   DEPTH_STEP: DO JB = 1, NROW
     G1R(1,1:NRPRM) = REAL (HLYRD(1:NRPRM,1,JB))
     G1I(1,1:NRPRM) = REAL (AIMAG (HLYRD(1:NRPRM,1,JB)) )   ! HLYRD is in QL precision
     CALL CUBSPL (RHOTRP,G1R,NRPRM)
     CALL CUBSPL (RHOTRP,G1I,NRPRM)
     NVRL = NVRTX(JS)
     NVRLS = NVRL

     IF (SOURCE_TYPE == 2) THEN
       G2R(1,1:NRPRM) =        REAL (HLYRD(1:NRPRM,2,JB))
       G2I(1,1:NRPRM) = REAL (AIMAG (HLYRD(1:NRPRM,2,JB)) )
       G3R(1,1:NRPRM) =        REAL (HLYRD(1:NRPRM,3,JB))
       G3I(1,1:NRPRM) = REAL (AIMAG (HLYRD(1:NRPRM,3,JB)) )
       CALL CUBSPL (RHOTRP,G2R,NRPRM)
       CALL CUBSPL (RHOTRP,G2I,NRPRM)
       CALL CUBSPL (RHOTRP,G3R,NRPRM)
       CALL CUBSPL (RHOTRP,G3I,NRPRM)
       NVRLS = NVRL -1
     END IF

     STRYK_STEP: DO JA = 1, NA(JP)
       JAB = JA + (JB-1)*NA(JP)

       DO JV = 1,NVRLS
         JV1 = JV + 1             ! Wire JV goes from vertex JV to vertex JV1
         IF (JV == NVRL) JV1 = 1  ! Closed loop only

         EX0 = ZERO
         CPHI2 = CSTR * CPHI(JV,JS) + SSTR * SPHI(JV,JS)  !  strike angle wrt loop segment
         SPHI2 = SSTR * CPHI(JV,JS) - CSTR * SPHI(JV,JS)
         YD = (YCELL(JAB,JP) - SXE(JV,JS)) * CPHI(JV,JS) &
            - (XCELL(JAB,JP) - SXN(JV,JS)) * SPHI(JV,JS)
         X1D = (XCELL(JAB,JP) - SXN(JV,JS) ) * CPHI(JV,JS) &
             + (YCELL(JAB,JP) - SXE(JV,JS) ) * SPHI(JV,JS)

         DO JD = 1, NDIP(JV,JS)
           X1 = X1D - (JD - 0.5) * DIPL(JV,JS)
           RSQ = X1**2 + YD**2
           RHO1 = SQRT (RSQ)
           CALL CCUBVAL (RHOTRP,NRPRM,G1R,G1I,RHO1,CCS1)
           EX0 = EX0 + CCS1
         END DO
         EX0 = DIPL(JV,JS) * EX0

         CALL RXYZ2PLT (CPHI2,SPHI2,CDIP,SDIP,CPLN,SPLN,R23)
         EXYZ = ZERO
         EXYZ(1) = EX0
         EAB = MATMUL (R23, EXYZ)

         E_PRYM(1,JAB,JS,JP) = E_PRYM(1,JAB,JS,JP) + EAB(1)
         E_PRYM(2,JAB,JS,JP) = E_PRYM(2,JAB,JS,JP) + EAB(2) !  No vertical field for closed loop

       END DO
       IF (SOURCE_TYPE == 2 ) THEN    ! Integrate over open side for GW source.
         X1D = (XCELL(JAB,JP) - SXN(1,JS)   ) * CPHI(NVRL,JS) &
             + (YCELL(JAB,JP) - SXE(1,JS)   ) * SPHI(NVRL,JS)
         X2D = (XCELL(JAB,JP) - SXN(NVRL,JS)) * CPHI(NVRL,JS) &
             + (YCELL(JAB,JP) - SXE(NVRL,JS)) * SPHI(NVRL,JS)
         YD = (YCELL(JAB,JP) - SXE(1,JS))    * CPHI(NVRL,JS) &
            - (XCELL(JAB,JP) - SXN(1,JS))    * SPHI(NVRL,JS)

         RHO1 = SQRT (X1D**2 + YD**2)
         RHO2 = SQRT (X2D**2 + YD**2)

         CPHI2 = CSTR * CPHI(NVRL,JS) + SSTR * SPHI(NVRL,JS)  !  strike angle wrt loop segment
         SPHI2 = SSTR * CPHI(NVRL,JS) - CSTR * SPHI(NVRL,JS)
         CALL CCUBVAL (RHOTRP,NRPRM,G2R,G2I,RHO1,CCS1)
         CALL CCUBVAL (RHOTRP,NRPRM,G2R,G2I,RHO2,CCS2)
         EX1 = X2D * CCS2 - X1D * CCS1
         EY1 = YD * (CCS2 - CCS1)

         CALL CCUBVAL (RHOTRP,NRPRM,G3R,G3I,RHO1,CCS1)
         CALL CCUBVAL (RHOTRP,NRPRM,G3R,G3I,RHO2,CCS2)
         EZ1 = CCS2 - CCS1

         CALL RXYZ2PLT (CPHI2,SPHI2,CDIP,SDIP,CPLN,SPLN,R23)
         EXYZ(1) = EX1;   EXYZ(2) = EY1;  EXYZ(3) = EZ1
         EAB = MATMUL (R23, EXYZ)

         E_PRYM(1,JAB,JS,JP) = E_PRYM(1,JAB,JS,JP) + EAB(1)
         E_PRYM(2,JAB,JS,JP) = E_PRYM(2,JAB,JS,JP) + EAB(2)
       END IF
     END DO STRYK_STEP
   END DO DEPTH_STEP
 END DO SOURCE_LOOP
 DEALLOCATE (HLYRD,G1R,G1I,G2R,G2I,G3R,G3I)

 1 FORMAT (T3,' An evil spirit has entered PRM_BOSS_UL_LP.  Seek help !')

 END SUBROUTINE PRM_BOSS_UL_LP

 SUBROUTINE PRM_HNK1_CL (NRPRM,RHOTRP,NROW,ZC1L,ZS,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                         SXLYR,RXLYR,KFG,HLYRD)
!-------------------------------------------------------------------------------

!***  Calls PRM_KER1_CL
!***  Called by PRM_BOSS_LP

!  Computes integrals needed to evaluate the primary electric field in one of the
!  layers above basement due to a closed loop source at any depth or elevation.
!  It uses the flow through Hankel transform method to evaluate the three Hankel
!  integrals at discrete depths.  The Hankel transform integral uses a 15 point
!  per decade filter coefficient set derived from Christensen's FLTGEN program.

!       NRPRM - number of horizontal interpolation points
!      RHOTRP - 15 points per decade interpolation array
!        NROW - number of rows
!        ZC1L - depth of row centres in Plate JP
!          ZS - transmitter depth
!        NLYR - number of layers including basement. (1 or 2 only)
!        SIGL - complex xconductivity for all layers including Cole-Cole & dielectric
!        KSQL - iwu * SIGL for all layers (includes Cole-Cole)
!        RMUD - relative magetic permeability
!        THKD - thickness of each layer above basement
!       DPTHL - depth to top of each layer including basement
!         KFG - Case selector variable

!             KER - kernel functions
!  HLYRD(JR,1,JB) - only component needed for both closed & open loop
!  HLYRD(JR,2,JB) - integrands for grounded wire segment
!

 USE LG_Filter_Coefficients

 IMPLICIT NONE
 REAL(KIND=QL), PARAMETER :: FOURPI = 12.56637061_QL
 COMPLEX(KIND=QL), PARAMETER :: ZERO=(0._QL,0._QL)
 INTEGER L,LMAX,LMIN,K,KMAX,KMIN,KBOT,K1,JR,NROW,NLYR,SXLYR,RXLYR,KFG,JB,NRPRM
 REAL RHOTRP(NRPRM),ZC1L(NROW)
 REAL(KIND=QL) RMUD(0:NLYR),ZS,DELTA,Y1,Y,RD,LMBDA,RHOD
 REAL(KIND=QL), DIMENSION (NLYR) :: THKD,DPTHL
 COMPLEX(KIND=QL) HLYRD(NRPRM,3,NROW),KER(JNLO-NRPRM:JNHI,3,NROW)
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL
 LOGICAL JUMP

 KER = ZERO
 DELTA = LOG (10.D0)/ DBLE (NDEC_JN)

! Basic integral for open and closed loops.
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
   LMBDA = EXP (SNGL(Y))
   CALL PRM_KER1_CL (NRPRM,K,JR,L,LMBDA,NROW,ZC1L,ZS,NLYR,THKD,DPTHL,RMUD, &
                     SIGL,KSQL,SXLYR,RXLYR,KFG,KER,HLYRD,JUMP)
   IF (JUMP) EXIT
   END DO

 Y = Y1                       ! Finish off the low end for RHOTRP(1)
 DO L = -51, JNLO, -1
   LMIN = L                   ! Minimum filter index used
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
   K = L + 1 - JR             ! Compute the kernel index.
   CALL PRM_KER1_CL (NRPRM,K,JR,L,LMBDA,NROW,ZC1L,ZS,NLYR,THKD,DPTHL,RMUD, &
                     SIGL,KSQL,SXLYR,RXLYR,KFG,KER,HLYRD,JUMP)
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
   HLYRD(JR,1,1:NROW) =  HLYRD(JR,1,1:NROW) + KER(K,1,1:NROW) * WJ0(L)
 END DO

 IF (K1 > KBOT) THEN    !  Add low end kernel values until convergence.
   DO K = K1-1, KBOT, -1
     KMIN = K
     L = K - 1 + JR
     Y = Y1 + DBLE (L) * DELTA
     LMBDA = EXP (SNGL(Y))
     CALL PRM_KER1_CL (NRPRM,K,JR,L,LMBDA,NROW,ZC1L,ZS,NLYR,THKD,DPTHL,RMUD, &
                       SIGL,KSQL,SXLYR,RXLYR,KFG,KER,HLYRD,JUMP)
     IF (JUMP) EXIT
   END DO
 END IF

 DO JR = 2, NRPRM-1          !  Compute transforms for all other RHO values
   DO K = KMIN, KMAX         !  using previously computed kernel values.
     L = K - 1 + JR
     HLYRD(JR,1,1:NROW) =  HLYRD(JR,1,1:NROW) + KER(K,1,1:NROW) * WJ0(L)
   END DO
 END DO

 DO JB = 1,NROW
   DO JR = 1, NRPRM
     RHOD = REAL (RHOTRP(JR),KIND=QL)
     HLYRD(JR,1,JB) = HLYRD(JR,1,JB) / (FOURPI * RHOD * SIGL(NLYR))
   END DO
 END DO

 END SUBROUTINE PRM_HNK1_CL

 SUBROUTINE PRM_KER1_CL (NRPRM,K,JR,L,LMBDA,NROW,ZC1L,ZS,NLYR,THKD,DPTHL,RMUD, &
                         SIGL,KSQL,SXLYR,RXLYR,KFG,KER,HLYRD,JUMP)
!---------------------------------------------------------------------------

!***  Called by PRM_HNK1_LP

! Compute the kernels for closed loop integration for layers above basement.
! This involves only G potential terms.

!     NRPRM - number of points in 15 point / decade array
!         K - filter kernel index
!        JR - RHO interpolation index
!         L - filter point index
!     LMBDA - Hankel transform variable
!      ZC1L - depth of row centres in Plate JP
!        ZS - source location  (negative in air, positive in ground)
!      NLYR - number of layers including basement. (1 or 2 only)
!      THKD - thickness of each layer above basement
!     DPTHL - depth to top of each layer including basement
!      SIGL - complex xconductivity for all layers including Cole-Cole & dielectric
!      KSQL - iwu * SIGL for all layers (includes Cole-Cole)
!      RMUD - relative magetic permeability
!       KFG - Case selector variable for cells above basewment
!     SXLYR - Source layer
!
!
!   KER          - kernels for Hankel integraion
!   HLYRD(*,1,*) - needed for both closed and open loops
!   HLYRD(*,2,*) - used only for grounded wire segments
!   HLYRD(*,3,*) - used only for grounded wire segments
!
!           JUMP - keep integrating if false.

 USE LG_Filter_Coefficients
 IMPLICIT NONE
 REAL, PARAMETER :: TOL=1.E-5
 COMPLEX(KIND=QL), PARAMETER :: ZERO=(0._QL,0._QL)
 INTEGER JR,L,K,NROW,NLYR,SXLYR,RXLYR,KFG,JZ,NRPRM
 REAL ZC1L(NROW)
 REAL(KIND=QL) RMUD(0:NLYR),LMBDA,ZS,ZR,AR,AI,HLYRDR,HLYRDI,ZDR
 REAL(KIND=QL), DIMENSION (NLYR) :: THKD,DPTHL
 COMPLEX (KIND=QL) XI_V,F_V,F_H,ETA_V,G_V,G_H,XPU,XPD,XPDIR,SL,KER(JNLO-NRPRM:JNHI,3,NROW), &
                   FACV,KV,S(0:NLYR),HLYRD(NRPRM,3,NROW),TMP(NROW)
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL
 LOGICAL TOO_BIG,JUMP

 CALL EDSX_COEF (SXLYR,RXLYR,KFG,LMBDA,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                 ZS,S,XI_V,F_V,F_H,ETA_V,G_V,G_H)

 KER(K,1,1:NROW) = ZERO
 TMP = ZERO
 SL = S(RXLYR)
 FACV = RMUD(SXLYR) * KSQL(RXLYR) / (RMUD(RXLYR) * S(SXLYR))

 DO JZ = 1, NROW
   ZR = REAL (ZC1L(JZ),QL)
   XPU = EXP (SL * (ZR - DPTHL(RXLYR+1)))
   XPD = EXP (SL * (DPTHL(RXLYR) - ZR))
   KV = FACV * (XI_V * XPU + ETA_V * XPD)
   KER(K,1,JZ) = -KV * LMBDA
 END DO

 IF (SXLYR == RXLYR) THEN     !  Transmitter in receiver layer
   DO JZ = 1, NROW
     ZDR = ABS (ZS - ZR)
     XPDIR = EXP (-SL * ZDR)
     KV = FACV * XPDIR
     KER(K,1,JZ) = KER(K,1,JZ) - KV * LMBDA
   END DO
 END IF

 JUMP = .TRUE.
 TMP(1:NROW) = KER(K,1,1:NROW) * WJ0(L)

 HLYRD(JR,1,1:NROW) = HLYRD(JR,1,1:NROW) + TMP(1:NROW)
 DO JZ = 1, NROW
   AR = ABS ( REAL (TMP(JZ)) )
   AI = ABS (AIMAG (TMP(JZ)) )
   TOO_BIG = .FALSE.
   HLYRDR = ABS ( REAL (HLYRD(JR,1,JZ)) )
   HLYRDI = ABS (AIMAG (HLYRD(JR,1,JZ)) )
   IF (AR > TOL* HLYRDR) TOO_BIG = .TRUE.
   IF (AI > TOL* HLYRDI) TOO_BIG = .TRUE.
   IF (TOO_BIG) JUMP = .FALSE.
 END DO

 END SUBROUTINE PRM_KER1_CL

 SUBROUTINE PRM_HNK1_OL (NRPRM,RHOTRP,NROW,ZC1L,ZS,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                         SXLYR,RXLYR,KFG,HLYRD)
!------------------------------------------------------------------------------

!***  Calls PRM_KER_OL
!***  Called by PRM_BOSS_LP

!  Computes integrals needed to evaluate the primary electric field in one of the
!  layers above basement due to an open loop source at any depth or elevation.
!  It uses the flow through Hankel transform method to evaluate the three Hankel
!  integrals at discrete depths.  The Hankel transform integral uses a 15 point
!  per decade filter coefficient set derived from Christensen's FLTGEN program.

!       NRPRM - number of horizontal interpolation points
!      RHOTRP - 15 points per decade interpolation array
!        NROW - number of rows
!        ZC1L - depth of rows in Plate JP
!          ZS - transmitter depth
!        NLYR - number of layers including basement. (1 or 2 only)
!        SIGL - complex xconductivity for all layers including Cole-Cole & dielectric
!        KSQL - iwu * SIGL for all layers (includes Cole-Cole)
!        RMUD - relative magetic permeability
!        THKD - thickness of each layer above basement
!       DPTHL - depth to top of each layer including basement
!         KFG - Case selector variable
!
!             KER - kernel functions
!  HLYRD(JR,1,JB) - only component needed for both closed & open loop
!  HLYRD(JR,2,JB) - integrands for grounded wire segment
!

 USE LG_Filter_Coefficients

 IMPLICIT NONE
 REAL(KIND=QL), PARAMETER :: FOURPI = 12.56637061_QL
 COMPLEX(KIND=QL), PARAMETER :: ZERO=(0._QL,0._QL)
 INTEGER L,LMAX,LMIN,K,KMAX,KMIN,KBOT,K1,JR,NROW,NLYR,SXLYR,RXLYR,KFG,JB,NRPRM
 REAL RHOTRP(NRPRM),ZC1L(NROW)
 REAL(KIND=QL) RMUD(0:NLYR),ZS,DELTA,Y1,Y,RD,LMBDA,RHOD
 REAL(KIND=QL), DIMENSION (NLYR) :: THKD,DPTHL
 COMPLEX(KIND=QL) HLYRD(NRPRM,3,NROW),KER(JNLO-NRPRM:JNHI,3,NROW)
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL
 LOGICAL JUMP

 KER = ZERO
 DELTA = LOG (10.D0)/ DBLE (NDEC_JN)

! Basic integral for open and closed loops.
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
   LMBDA = EXP (SNGL(Y))
   CALL PRM_KER1_OL (NRPRM,K,JR,L,LMBDA,NROW,ZC1L,ZS,NLYR,THKD,DPTHL,RMUD, &
                     SIGL,KSQL,SXLYR,RXLYR,KFG,KER,HLYRD,JUMP)
   IF (JUMP) EXIT
   END DO

 Y = Y1                       ! Finish off the low end for RHOTRP(1)
 DO L = -51, JNLO, -1
   LMIN = L                   ! Minimum filter index used
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
   K = L + 1 - JR             ! Compute the kernel index.
   CALL PRM_KER1_OL (NRPRM,K,JR,L,LMBDA,NROW,ZC1L,ZS,NLYR,THKD,DPTHL,RMUD, &
                     SIGL,KSQL,SXLYR,RXLYR,KFG,KER,HLYRD,JUMP)
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

   HLYRD(JR,1,1:NROW) =  HLYRD(JR,1,1:NROW) + KER(K,1,1:NROW) * WJ0(L)
   HLYRD(JR,2,1:NROW) =  HLYRD(JR,2,1:NROW) + KER(K,2,1:NROW) * WJ1(L)
   HLYRD(JR,3,1:NROW) =  HLYRD(JR,3,1:NROW) + KER(K,3,1:NROW) * WJ0(L)
 END DO

 IF (K1 > KBOT) THEN    !  Add low end kernel values until convergence.
   DO K = K1-1, KBOT, -1
     KMIN = K
     L = K - 1 + JR
     Y = Y1 + DBLE (L) * DELTA
     LMBDA = EXP (SNGL(Y))
     CALL PRM_KER1_OL (NRPRM,K,JR,L,LMBDA,NROW,ZC1L,ZS,NLYR,THKD,DPTHL,RMUD, &
                       SIGL,KSQL,SXLYR,RXLYR,KFG,KER,HLYRD,JUMP)
     IF (JUMP) EXIT
   END DO
 END IF

 DO JR = 2, NRPRM-1          !  Compute transforms for all other RHO values
   DO K = KMIN, KMAX         !  using previously computed kernel values.
     L = K - 1 + JR
     HLYRD(JR,1,1:NROW) =  HLYRD(JR,1,1:NROW) + KER(K,1,1:NROW) * WJ0(L)
     HLYRD(JR,2,1:NROW) =  HLYRD(JR,2,1:NROW) + KER(K,2,1:NROW) * WJ1(L)
     HLYRD(JR,3,1:NROW) =  HLYRD(JR,3,1:NROW) + KER(K,3,1:NROW) * WJ0(L)
   END DO
 END DO

 DO JB = 1,NROW
   DO JR = 1, NRPRM
     RHOD = REAL (RHOTRP(JR),KIND=QL)
     HLYRD(JR,1:3,JB) = HLYRD(JR,1:3,JB) / (FOURPI * RHOD * SIGL(NLYR))
     HLYRD(JR,2,JB) = HLYRD(JR,2,JB) / RHOD
   END DO
 END DO

 END SUBROUTINE PRM_HNK1_OL

 SUBROUTINE PRM_KER1_OL (NRPRM,K,JR,L,LMBDA,NROW,ZC1L,ZS,NLYR,THKD,DPTHL,RMUD, &
                         SIGL,KSQL,SXLYR,RXLYR,KFG,KER,HLYRD,JUMP)
!---------------------------------------------------------------------------

!***  Called by PRM_HNK1_LP

!  Compute the kernels for grounded wire sources.
!  This is a mixture of G & F potential terms.

!     NRPRM - number of points in 15 point / decade array
!         K - filter kernel index
!        JR - RHO interpolation index
!         L - filter point index
!     LMBDA - Hankel transform variable
!      ZC1L - depth of rows in Plate JP
!        ZS - source location  (negative in air, positive in ground)
!      NLYR - number of layers including basement. (1 or 2 only)
!      THKD - thickness of each layer above basement
!     DPTHL - depth to top of each layer including basement
!      SIGL - complex xconductivity for all layers including Cole-Cole & dielectric
!      KSQL - iwu * SIGL for all layers (includes Cole-Cole)
!      RMUD - relative magetic permeability
!       KFG - Case selector variable
!     SXLYR - layer containing source
!
!   KER          - kernels for Hankel integraion
!   HLYRD(*,1,*) - needed for both closed and open loops
!   HLYRD(*,2,*) - used only for grounded wire segments
!   HLYRD(*,3,*) - used only for grounded wire segments
!
!           JUMP - keep integrating if false.

 USE LG_Filter_Coefficients
 IMPLICIT NONE
 REAL, PARAMETER :: TOL=1.E-5
 COMPLEX(KIND=QL), PARAMETER :: ZERO=(0._QL,0._QL)
 INTEGER JR,L,K,NROW,NLYR,SXLYR,RXLYR,KFG,JZ,NRPRM,JI
 REAL ZC1L(NROW)
 REAL(KIND=QL) RMUD(0:NLYR),LMBDA,ZS,ZR,AR,AI,HLYRDR,HLYRDI,GAM,ZDR
 REAL(KIND=QL), DIMENSION (NLYR) :: THKD,DPTHL
 COMPLEX (KIND=QL) XI_V,F_V,F_H,ETA_V,G_V,G_H,XPU,XPD,XPDIR,KER(JNLO-NRPRM:JNHI,3,NROW), &
                   S(0:NLYR),SL,FACV,KV,KS,HLYRD(NRPRM,3,NROW),TMP(3,NROW)
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL
 LOGICAL TOO_BIG,JUMP

 CALL EDSX_COEF (SXLYR,RXLYR,KFG,LMBDA,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                 ZS,S,XI_V,F_V,F_H,ETA_V,G_V,G_H)
 KER(K,1:3,1:NROW) = ZERO
 TMP = ZERO
 SL = S(RXLYR)
 FACV = RMUD(SXLYR) * KSQL(RXLYR) / (RMUD(RXLYR) * S(SXLYR))

 DO JZ = 1, NROW
   ZR = REAL (ZC1L(JZ),QL)
   XPU = EXP (SL * (ZR - DPTHL(RXLYR+1)))
   XPD = EXP (SL * (DPTHL(RXLYR) - ZR))
   KV = FACV * (XI_V * XPU + ETA_V * XPD)
   KS = SL * (F_H * XPU - G_H * XPD)
   KER(K,1,JZ) = -KV * LMBDA
   KER(K,2,JZ) = -(KV - KS)
   KER(K,3,JZ) = -(F_H * XPU + G_H * XPD)  * LMBDA
 END DO

 IF (SXLYR == RXLYR) THEN     !  Transmitter in receiver layer
   DO JZ = 1, NROW
     ZDR = ZS - ZR
     GAM = SIGN (1.0_QL, ZDR)
     XPDIR = EXP (-SL * ABS (ZDR))
     KV = FACV * XPDIR
     KS = SL * XPDIR

     KER(K,1,JZ) = KER(K,1,JZ) - KV * LMBDA
     KER(K,2,JZ) = KER(K,2,JZ) - (KV - KS)
     KER(K,3,JZ) = KER(K,3,JZ) - GAM * XPDIR * LMBDA
   END DO
 END IF

 JUMP = .TRUE.
 TMP(1,1:NROW) = KER(K,1,1:NROW) * WJ0(L)
 TMP(2,1:NROW) = KER(K,2,1:NROW) * WJ1(L)
 TMP(3,1:NROW) = KER(K,3,1:NROW) * WJ0(L)

 HLYRD(JR,1:3,1:NROW) = HLYRD(JR,1:3,1:NROW) + TMP(1:3,1:NROW)
 DO JZ = 1, NROW
   DO JI = 1,3
     AR = ABS ( REAL (TMP(JI,JZ)) )
     AI = ABS (AIMAG (TMP(JI,JZ)) )
     TOO_BIG = .FALSE.
     HLYRDR = ABS ( REAL (HLYRD(JR,JI,JZ)) )
     HLYRDI = ABS (AIMAG (HLYRD(JR,JI,JZ)) )
     IF (AR > TOL* HLYRDR) TOO_BIG = .TRUE.
     IF (AI > TOL* HLYRDI) TOO_BIG = .TRUE.
     IF (TOO_BIG) JUMP = .FALSE.
   END DO
 END DO
 END SUBROUTINE PRM_KER1_OL

 SUBROUTINE PRM_BOSS_MD (FRQ,NTX,SXN,SXE,SXZ,SXDIP,SXAZM,NRPRM,RHOTRP,NLYR,THKD,DPTHL, &
                         RMUD,SIGL,KSQL,NPLT,PLYR,PLAZM,PLDIP,PLUNJ,MXAB,NB,NA,XCELL,  &
                         YCELL,ZCELL,NZ1,ZV1,E_PRYM)
! -----------------------------------------------------------------------------------

!***  Called by LEROI
!***  Calls PRM_HNK_MD, CUBSPL, C2DINTRP, SETPRM

!  Computes the primary electric field, E_PRYM, due to a magnetic unit dipole
!  source, in NPLT plates lying in the basement of a host with NLYR-1 layers
!  above basement.  R23 is used to rotate the fields into the components
!  tangential to each plate.
!
!  Exy, Eyy and Ezy, Exz are not computed because E_PRM is computed in a
!  coordinate system where the source lies in the X-Z plane; ie, the source
!  has is no Y component in this system.
!
!  The target (plate) is discretised along strike into NAL cells of length DA
!  and down dip into NBL cells of width DB.
!
!  For a vertical plate along the X axis, the cells are numbered along strike
!  from the South to the North, first along the top row and then in the same
!  direction along lower rows.  The last cell would be in the bottom
!  North corner.  In plan view, the X and Y components of E_PRYM are positive
!  to the North and East respectively.
!
!          INPUT
!          -----
!         FRQ - frequency
!         NTX - number of source positions
!     SXN,SXE - North, East coordinate of each source position
!         SXZ - depth of each source position.  (SXZ < 0 in air; SXZ > 0 below surface)
!       SXDIP - TX inclination of magnetic dipole sources in the vertical plane.
!       SXAZM - azimuths of magnetic dipole sources in radians. (north = 0; east = PI/2)
!      RHOTRP - horizontal interpolation array (15 points / decade) of dimension NRPRM
!        NLYR - number of layers including basement. (1 or 2 only)
!        PLYR - layer containing plate
!        SIGL - complex xconductivity for all layers including Cole-Cole & dielectric
!        KSQL - iwu * SIGL for all layers (includes Cole-Cole)
!        RMUD - relative magetic permeability
!        THKD - thickness of each layer above basement
!      DEPTHL - depth to top of each layer including basement
!        NPLT - number of plates
!       PLAZM - strike angle in radians
!       PLDIP - dip angle
!       PLUNJ - plunge angles
!        MXAB - maximum number of cells in one plate
!      NA, NB - number of cells along strike & down dip respectively for each plate
!  XCELL(k,*) - north coordinate of centre of cell k
!  YCELL(k,*) - east coordinate of centre of cell k
!  ZCELL(k,*) - depth of centre of cell k
!
!          OUTPUT
!          ------
!
!  E_PRYM(1,*) - complex primary electric fields along strike.
!  E_PRYM(2,*) - complex primary electric fields down-dip.
!
!    The first NAB components of E_PRYM are the fields along strike and
!    the second group of NA*NB components are along dip.  Thus for a strike
!    along the X axis, the first NA*NB components would be the X component
!    of the primary electric field and the second NA*NB components
!    would be the Y component multiplied by the cosine of the dip angle.
! -------------------------------------------------------------------------

 USE LG_Filter_Coefficients
 IMPLICIT NONE
 COMPLEX(KIND=QL), PARAMETER :: ZERO = (0._QL, 0._QL)
 REAL, PARAMETER :: MU0=12.56637E-7, TOL0=1.E-3
 COMPLEX, PARAMETER :: ONE=(1.,0.)
 INTEGER NTX,NRPRM,NLYR,NPLT,PLYR(NPLT),MXAB,NA(NPLT),NB(NPLT),NBL,NAL,JAB,NINTG, &
         JP,JS,JB,JA,JZ,NZ1,I1,KFG,SXLYR,RXLYR
 REAL, DIMENSION(NTX) :: SXN,SXE,SXZ,SXAZM,SXDIP
 REAL, DIMENSION(4,NRPRM) :: QR,QI
 REAL FRQ,RHOTRP(NRPRM),CSDP,SNDP,CSAZ,SNAZ,SAZM,CAZM,CDIP,SDIP,CPLN,SPLN, &
      XCELL(MXAB,NPLT),YCELL(MXAB,NPLT),ZCELL(MXAB,NPLT),RHO,RHOSQ,XBAR0,XBAR, &
      YBAR0,YBAR,XB2,YB2,RAD,XRAD,YRAD,ZRAD,ZTR,ZV1(NZ1),R23(2,3)
 REAL, DIMENSION (NPLT) :: PLAZM,PLDIP,PLUNJ
 REAL(KIND=QL) RMUD(0:NLYR),ZS
 REAL(KIND=QL), DIMENSION(NLYR) :: THKD,DPTHL
 REAL, ALLOCATABLE, DIMENSION(:,:,:) :: G1R,G2R,G3R,G4R,G5R,G1I,G2I,G3I,G4I,G5I
 REAL(KIND=QL), ALLOCATABLE :: ZV1Q(:)
 COMPLEX EFAC,EFAC0,A0,KR,EX,EY,EZ,EXX,EYX,EZX,EXZ,EYZ,EYXP,EZXP,EXZP,EYZP, &
         E_PRYM(2,MXAB,NTX,NPLT),QQ(5),C2DINTRP,EXYZ(3),EAB(2)
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL
 COMPLEX(KIND=QL), DIMENSION(:,:,:), ALLOCATABLE :: FXX
 LOGICAL TILT

 EFAC0 = (0.,1.) * FRQ * MU0  / 2.  ! iwu0 / 4 PI
 E_PRYM = (0.,0.)
 I1 = 1            ! magnetic source

!  Set depth array over which to interpolate Green's function integrals

 ALLOCATE (G1R(4,NRPRM,NZ1),G1I(4,NRPRM,NZ1),G2R(4,NRPRM,NZ1),G2I(4,NRPRM,NZ1),G3R(4,NRPRM,NZ1), &
           G3I(4,NRPRM,NZ1),G4R(4,NRPRM,NZ1),G4I(4,NRPRM,NZ1),G5R(4,NRPRM,NZ1),G5I(4,NRPRM,NZ1), &
           FXX(NRPRM,5,NZ1),ZV1Q(NZ1))

 ZV1Q(1:NZ1) = REAL (ZV1(1:NZ1),QL)

!  Set up array FXX which contains values of the F-potential as a function of
!  of horizontal distance and depth.  Step through different altitude levels.

 RXLYR = NLYR     !  This version restricts the target to be contained in the basement.

 SOURCE_LOOP: DO JS = 1,NTX
   ZS = REAL (SXZ(JS), KIND=QL)
   SXLYR = 0
   DO JZ = NLYR,1,-1
     IF (ZS > DPTHL(JZ)) THEN
       SXLYR = JZ
       EXIT
     END IF
   END DO
   CALL SET_KFG (I1,NLYR,SXLYR,NLYR,KFG)
   EFAC = EFAC0 * REAL (RMUD(SXLYR))

   NINTG = 1
   SNDP = 0.
   CSDP = 1.
   SNAZ = 0.
   CSAZ = 1.
   TILT = .FALSE.
   IF (ABS (SXDIP(JS)) > TOL0) THEN
     TILT = .TRUE.
     SNDP = SIN (SXDIP(JS))
     CSDP = COS (SXDIP(JS))
     NINTG = 3
     IF (SXLYR > 0) NINTG = 5
   END IF
   IF (ABS (SXAZM(JS)) > TOL0) THEN
     SNAZ = SIN (SXAZM(JS))
     CSAZ = COS (SXAZM(JS))
   END IF

   FXX = ZERO
   CALL PRM_HNK_MD (NRPRM,RHOTRP,NZ1,ZV1Q,ZS,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,KFG,RXLYR,SXLYR,NINTG,FXX)

   G1R = 0. ; G2R = 0. ; G3R = 0. ; G4R = 0. ; G5R = 0.
   G1I = 0. ; G2I = 0. ; G3I = 0. ; G4I = 0. ; G5I = 0.

   DO JZ = 1,NZ1
     QR(1,1:NRPRM) =        REAL (FXX(1:NRPRM,1,JZ))
     QI(1,1:NRPRM) = REAL (AIMAG (FXX(1:NRPRM,1,JZ)) )   ! FXX is in QL precision
     CALL CUBSPL (RHOTRP,QR,NRPRM)
     CALL CUBSPL (RHOTRP,QI,NRPRM)
     G1R(1:4,1:NRPRM,JZ) = QR(1:4,1:NRPRM)
     G1I(1:4,1:NRPRM,JZ) = QI(1:4,1:NRPRM)

     IF (NINTG > 1) THEN                                ! set up horizontal transmitter components
       QR(1,1:NRPRM) =        REAL (FXX(1:NRPRM,2,JZ))
       QI(1,1:NRPRM) = REAL (AIMAG (FXX(1:NRPRM,2,JZ)) )
       CALL CUBSPL (RHOTRP,QR,NRPRM)
       CALL CUBSPL (RHOTRP,QI,NRPRM)
       G2R(1:4,1:NRPRM,JZ) = QR(1:4,1:NRPRM)
       G2I(1:4,1:NRPRM,JZ) = QI(1:4,1:NRPRM)

       QR(1,1:NRPRM) =        REAL (FXX(1:NRPRM,3,JZ))
       QI(1,1:NRPRM) = REAL (AIMAG (FXX(1:NRPRM,3,JZ)) )
       CALL CUBSPL (RHOTRP,QR,NRPRM)
       CALL CUBSPL (RHOTRP,QI,NRPRM)
       G3R(1:4,1:NRPRM,JZ) = QR(1:4,1:NRPRM)
       G3I(1:4,1:NRPRM,JZ) = QI(1:4,1:NRPRM)

       IF (NINTG > 3) THEN                                ! set up F potential induced current contribution
         QR(1,1:NRPRM) =        REAL (FXX(1:NRPRM,4,JZ))
         QI(1,1:NRPRM) = REAL (AIMAG (FXX(1:NRPRM,4,JZ)) )
         CALL CUBSPL (RHOTRP,QR,NRPRM)
         CALL CUBSPL (RHOTRP,QI,NRPRM)
         G4R(1:4,1:NRPRM,JZ) = QR(1:4,1:NRPRM)
         G4I(1:4,1:NRPRM,JZ) = QI(1:4,1:NRPRM)

         QR(1,1:NRPRM) =        REAL (FXX(1:NRPRM,5,JZ))
         QI(1,1:NRPRM) = REAL (AIMAG (FXX(1:NRPRM,5,JZ)) )
         CALL CUBSPL (RHOTRP,QR,NRPRM)
         CALL CUBSPL (RHOTRP,QI,NRPRM)
         G5R(1:4,1:NRPRM,JZ) = QR(1:4,1:NRPRM)
         G5I(1:4,1:NRPRM,JZ) = QI(1:4,1:NRPRM)
       END IF
     END IF
   END DO

   PLATE_LOOP: DO JP = 1, NPLT
     IF (PLYR(JP) < NLYR) CYCLE
     NAL = NA(JP)
     NBL = NB(JP)
     CAZM = COS (PLAZM(JP) - SXAZM(JS))
     SAZM = SIN (PLAZM(JP) - SXAZM(JS))
     CDIP = COS (PLDIP(JP))
     SDIP = SIN (PLDIP(JP))
     CPLN =  COS (PLUNJ(JP))
     SPLN =  SIN (PLUNJ(JP))
     CALL RXYZ2PLT (CAZM,SAZM,CDIP,SDIP,CPLN,SPLN,R23)

!  Step through depths, splining FXX for each depth as a function of RHO, the
!  horizontal distance.  Add up contributions for each dipole for each
!  transmitter.

     DEPTH_STEP: DO JB = 1, NBL
       STRYK_STEP: DO JA = 1, NAL
         JAB = JA + (JB-1)*NAL
         ZTR = ZCELL (JAB,JP)
         EX = (0.,0.) ; EXX = (0.,0.) ; EXZ = (0.,0.)
         EY = (0.,0.) ; EYX = (0.,0.) ; EYZ = (0.,0.)
         EZ = (0.,0.) ; EZX = (0.,0.)
         QQ = (0.,0.)
         RHOSQ = (XCELL(JAB,JP) - SXN(JS) )**2 + (YCELL(JAB,JP) - SXE(JS) )**2
         RHO = SQRT (RHOSQ)
         RHO = MAX (0.1, RHO)

!  Compute components in system with X along source azimuth at source position.

         XBAR0 = (XCELL(JAB,JP) - SXN(JS)) / RHO
         YBAR0 = (YCELL(JAB,JP) - SXE(JS)) / RHO
         XBAR = XBAR0 * CSAZ + YBAR0 * SNAZ
         YBAR = YBAR0 * CSAZ - XBAR0 * SNAZ
         XB2 = XBAR**2
         YB2 = YBAR**2

!  Compute direct primary fields if source and receiver are in the same layer.
!  RXLYR > 0 since target cannot be in air.

         EYXP = (0.,0.); EZXP = (0.,0.); EXZP = (0.,0.); EYZP = (0.,0.)
         IF (SXLYR == RXLYR) THEN
           RAD = SQRT (RHOSQ + (ZCELL(JAB,JP) - SXZ(JS))**2)
           RAD = MAX (0.1, RAD)
           XRAD = XBAR * RHO / RAD
           YRAD = YBAR * RHO / RAD
           ZRAD = (ZCELL(JAB,JP) - SXZ(JS) ) / RAD
           KR = CMPLX (SQRT (KSQL(NLYR))) * RAD
           A0 = (ONE + KR) * EXP (-KR) / RAD**2
           EXZP = YRAD * A0;  EZXP = -EXZP
           EYXP = ZRAD * A0
           EYZP = -XRAD * A0
         END IF

         QQ(1) = C2DINTRP (RHOTRP,NRPRM,ZV1,NZ1,G1R,G1I,RHO,ZTR)
         EXZ =  QQ(1) * YBAR + EXZP
         EYZ = -QQ(1) * XBAR + EYZP

         IF (TILT) THEN                          ! set up horizontal transmitter components
           QQ(2) = C2DINTRP (RHOTRP,NRPRM,ZV1,NZ1,G2R,G2I,RHO,ZTR)
           QQ(3) = C2DINTRP (RHOTRP,NRPRM,ZV1,NZ1,G3R,G3I,RHO,ZTR)
           EXX =  XBAR * YBAR * (QQ(2) - 2.*QQ(3))

           IF (SXLYR > 0) THEN                           ! Source is subsurface
             QQ(4) = C2DINTRP (RHOTRP,NRPRM,ZV1,NZ1,G4R,G4I,RHO,ZTR)
             QQ(5) = C2DINTRP (RHOTRP,NRPRM,ZV1,NZ1,G5R,G5I,RHO,ZTR)
             EYX = (1. - 2.*YB2) * QQ(3) + YB2 * QQ(2) - QQ(4) + EYXP
             EZX = -YBAR * QQ(5) + EZXP
           ELSE
             EYX = -( (1. - 2.*XB2) * QQ(3) + XB2 * QQ(2) )    ! KS = 0
           END IF
         END IF

         EX = EXX * SNDP + EXZ * CSDP
         EY = EYX * SNDP + EYZ * CSDP
         EZ = EZX * SNDP

         EXYZ(1) = EX;  EXYZ(2) = EY;  EXYZ(3) = EZ
         EAB = EFAC * MATMUL (R23, EXYZ)
         E_PRYM(1,JAB,JS,JP) = EAB(1)
         E_PRYM(2,JAB,JS,JP) = EAB(2)
       END DO STRYK_STEP
     END DO DEPTH_STEP
   END DO PLATE_LOOP
 END DO SOURCE_LOOP

 DEALLOCATE (FXX,ZV1Q,G1R,G2R,G3R,G4R,G5R,G1I,G2I,G3I,G4I,G5I)

 END SUBROUTINE PRM_BOSS_MD

 SUBROUTINE PRM_HNK_MD (NRPRM,RHOTRP,NZ1,ZV1Q,ZS,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,KFG,RXLYR,SXLYR,NINTG,FXX)
!---------------------------------------------------------------------------------------------------------

!***  Calls PRM_KER_MD
!***  Called by PRM_BOSS

!  Sets up values of G & F potentials for VMD and HMD sources as a function of
!  horizontal distance at discrete depths.  Using the flow through Hankel
!  transform method, it evaluates the Hankel transform integral using a 15 point
!  per decade filter coefficient set derived from Christensen's FLTGEN program.
!  See PRM_BOSS for variable definitions not appearing below.

!       NRPRM - number of horizontal interpolation points
!      RHOTRP - 15 points per decade interpolation array
!         NZ1 - number of interpolation depths
!        ZV1Q - array of interpolation depths
!          ZS - transmitter depth
!        NLYR - number of layers including basement. (1 or 2 only)
!        SIGL - complex xconductivity for all layers including Cole-Cole & dielectric
!        KSQL - iwu * SIGL for all layers (includes Cole-Cole)
!        RMUD - relative magetic permeability
!        THKD - thickness of each layer above basement
!       DPTHL - depth to top of each layer including basement
!         KFG - Case selector variable
!       NINTG - number of integrals needed.
!
!         KER - kernels for Hankel integraion
!  FXX(*,1,*) - integrals for EXZ: horizontal electric field from VMD potential
!               sufficient for VMD source
!
!  FXX(*,2,*) - integrals for EXX the inline and EYX the transverse
!  FXX(*,3,*) - horizontal electric field from HMD potential
!
!  FXX(*,4,*) - integrals for EYX from the VED potential
!  FXX(*,5,*) - integrals for EXZ: horizontal electric field from VED potential
!               (The VED potential is due only from a subsurfact non-vertical HMD source

 USE LG_Filter_Coefficients

 IMPLICIT NONE
 COMPLEX(KIND=QL), PARAMETER :: ZERO=(0._QL,0._QL)
 INTEGER L,LMAX,LMIN,K,KMAX,KMIN,KBOT,K1,JR,NZ1,NLYR,KFG,RXLYR,SXLYR,JB,NRPRM,NINTG
 REAL RHOTRP(NRPRM)
 REAL(KIND=QL) RMUD(0:NLYR),ZS,DELTA,Y1,Y,RD,LMBDA,ZV1Q(NZ1)
 REAL(KIND=QL), DIMENSION (NLYR) :: THKD,DPTHL
 COMPLEX(KIND=QL) FSG,FXX(NRPRM,5,NZ1),KER(JNLO-NRPRM:JNHI,5,NZ1)
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL
 LOGICAL JUMP

 KER = ZERO
 FXX = ZERO
 FSG = ZERO
 IF (SXLYR > 0) FSG = SIGL(SXLYR) / SIGL(RXLYR)

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
   LMBDA = EXP (SNGL(Y))
   CALL PRM_KER_MD (NRPRM,K,JR,L,LMBDA,NZ1,ZV1Q,ZS,NLYR,THKD,DPTHL,RMUD, &
                    FSG,SIGL,KSQL,KFG,RXLYR,SXLYR,NINTG,KER,FXX,JUMP)
     IF (JUMP) EXIT
   END DO

 Y = Y1                       ! Finish off the low end for RHOTRP(1)
 DO L = -51, JNLO, -1
   LMIN = L                   ! Minimum filter index used
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (SNGL(Y))
   K = L + 1 - JR             ! Compute the kernel index.
   CALL PRM_KER_MD (NRPRM,K,JR,L,LMBDA,NZ1,ZV1Q,ZS,NLYR,THKD,DPTHL,RMUD, &
                    FSG,SIGL,KSQL,KFG,RXLYR,SXLYR,NINTG,KER,FXX,JUMP)
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

   FXX(JR,1,1:NZ1) =  FXX(JR,1,1:NZ1) + KER(K,1,1:NZ1) * WJ1(L)
   FXX(JR,2,1:NZ1) =  FXX(JR,2,1:NZ1) + KER(K,2,1:NZ1) * WJ0(L)
   FXX(JR,3,1:NZ1) =  FXX(JR,3,1:NZ1) + KER(K,3,1:NZ1) * WJ1(L)
   FXX(JR,4,1:NZ1) =  FXX(JR,4,1:NZ1) + KER(K,4,1:NZ1) * WJ0(L)
   FXX(JR,5,1:NZ1) =  FXX(JR,5,1:NZ1) + KER(K,5,1:NZ1) * WJ1(L)
 END DO

 IF (K1 > KBOT) THEN    !  Add low end kernel values until convergence.
   DO K = K1-1, KBOT, -1
     KMIN = K
     L = K - 1 + JR
     Y = Y1 + DBLE (L) * DELTA
     LMBDA = EXP (SNGL(Y))
     CALL PRM_KER_MD (NRPRM,K,JR,L,LMBDA,NZ1,ZV1Q,ZS,NLYR,THKD,DPTHL,RMUD, &
                      FSG,SIGL,KSQL,KFG,RXLYR,SXLYR,NINTG,KER,FXX,JUMP)
     IF (JUMP) EXIT
   END DO
 END IF

 DO JR = 2, NRPRM-1          !  Compute transforms for all other RHO values
   DO K = KMIN, KMAX         !  using previously computed kernel values.
     L = K - 1 + JR
     FXX(JR,1,1:NZ1) =  FXX(JR,1,1:NZ1) + KER(K,1,1:NZ1) * WJ1(L)
     FXX(JR,2,1:NZ1) =  FXX(JR,2,1:NZ1) + KER(K,2,1:NZ1) * WJ0(L)
     FXX(JR,3,1:NZ1) =  FXX(JR,3,1:NZ1) + KER(K,3,1:NZ1) * WJ1(L)
     FXX(JR,4,1:NZ1) =  FXX(JR,4,1:NZ1) + KER(K,4,1:NZ1) * WJ0(L)
     FXX(JR,5,1:NZ1) =  FXX(JR,5,1:NZ1) + KER(K,5,1:NZ1) * WJ1(L)
   END DO
 END DO

 DO JB = 1,NZ1
   DO L = 1,NINTG
     FXX(1:NRPRM,L,JB) = FXX(1:NRPRM,L,JB) / RHOTRP(1:NRPRM)
   END DO
   IF (NINTG > 1) FXX(1:NRPRM,3,JB) = FXX(1:NRPRM,3,JB) / RHOTRP(1:NRPRM)
 END DO

 END SUBROUTINE PRM_HNK_MD

 SUBROUTINE PRM_KER_MD (NRPRM,K,JR,L,LMBDA,NZ1,ZV1Q,ZS,NLYR,THKD,DPTHL,RMUD, &
                        FSG,SIGL,KSQL,KFG,RXLYR,SXLYR,NINTG,KER,FXX,JUMP)
!---------------------------------------------------------------------------

!***  Called by PRM_HNK_MD
!***  Calls MDSX_COEF   (RXLYR = NLYR)

!  Accumulates the integrals at each depth point for array FXX due to magnetic dipole.

!     NRPRM - number of points in 15 point / decade array
!         K - filter kernel index
!        JR - RHO interpolation index
!         L - filter point index
!     LMBDA - Hankel transform variable
!       NZ1 - number of interpolation depths
!      ZV1Q - array of interpolation depths
!        ZS - source location  (negative in air, positive in ground)
!      NLYR - number of layers including basement. (1 or 2 only)
!       FSG = SIGL(SXLYR) / SIGL(RXLYR)
!      SIGL - complex xconductivity for all layers including Cole-Cole & dielectric
!      KSQL - iwu * SIGL for all layers (includes Cole-Cole)
!      RMUD - relative magetic permeability
!      THKD - thickness of each layer above basement
!     DPTHL - depth to top of each layer including basement
!       KFG - Case selector variable
!     NINTG = number of required integrals = 1 for vertical source.  Otherwise NINTG
!           = 3 for source in air layer and 5 for subsurface source
!
!       KER - kernels for Hankel integraion
!   FXX(*,1,*) - integrals for EXZ: horizontal electric field from VMD potential
!                sufficient for VMD source
!
!          FXX - accumulated potential array
!   FXX(*,2,*) - integrals for EXX the inline and EYX the transverse
!   FXX(*,3,*) - horizontal electric field from HMD potential
!
!   FXX(*,4,*) - integrals for EYX from the VED potential
!   FXX(*,5,*) - integrals for EXZ: horizontal electric field from VED potential
!                (The VED potential is due only from a subsurfact non-vertical HMD source
!
!      JUMP - keep integrating if false.

 USE LG_Filter_Coefficients
 IMPLICIT NONE
 REAL, PARAMETER :: TOL=1.E-5
 COMPLEX(KIND=QL), PARAMETER :: ZERO=(0._QL,0._QL)
 INTEGER JR,L,K,NZ1,NLYR,RXLYR,SXLYR,KFG,JZ,NRPRM,NINTG,JINT
 REAL(KIND=QL) RMUD(0:NLYR),LMBDA,LMBDA2,ZS,AR,AI,FXXR,FXXI,ZV1Q(NZ1)
 REAL(KIND=QL), DIMENSION (NLYR) :: THKD,DPTHL
 COMPLEX (KIND=QL) XI_V,XI_H,F_V,ETA_V,ETA_H,G_V,XP1,FSG,SL,SM,K0,K1,KER(JNLO-NRPRM:JNHI,5,NZ1), &
                   S(0:NLYR),FXX(NRPRM,5,NZ1),TMP(5,NZ1)
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL
 LOGICAL TOO_BIG,JUMP

! Compute the kernels for the J0, J1 integrations.  Eliminate underflows
! due to exponentials.

 TMP = ZERO
 KER(K,1:5,1:NZ1) = ZERO

 CALL MDSX_COEF (SXLYR,RXLYR,KFG,LMBDA,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                 ZS,S,XI_V,XI_H,F_V,ETA_V,ETA_H,G_V)
 SL = S(RXLYR)
 SM = S(SXLYR)
 LMBDA2 = LMBDA**2

 SELECT CASE (NINTG)
 CASE(1)
   DO JZ = 1, NZ1
     XP1 = EXP (SL * (DPTHL(RXLYR) - ZV1Q(JZ)))
     ETA_V = ETA_V
     KER(K,1,JZ) = XP1 * ETA_V * LMBDA2 / SM
   END DO

 CASE(3)
   DO JZ = 1, NZ1
     XP1 = EXP (SL * (DPTHL(RXLYR) - ZV1Q(JZ)))
     KER(K,1,JZ) = XP1 * ETA_V * LMBDA2 / SM
     KER(K,2,JZ) = XP1 * ETA_H * LMBDA
     KER(K,3,JZ) = XP1 * ETA_H
   END DO

 CASE(5)
   K0 = -G_V * FSG * SL / SM
   K1 = ETA_H - K0
   DO JZ = 1, NZ1
     XP1 = EXP (SL * (DPTHL(RXLYR) - ZV1Q(JZ)))

     KER(K,1,JZ) = XP1 * ETA_V * LMBDA2 / SM
     KER(K,2,JZ) = XP1 * K1 * LMBDA
     KER(K,3,JZ) = XP1 * K1
     KER(K,4,JZ) = XP1 * ETA_H * LMBDA
     KER(K,5,JZ) = XP1 * G_V * FSG * LMBDA2 / SM
   END DO
 END SELECT

 JUMP = .TRUE.
 TMP(1,1:NZ1) = KER(K,1,1:NZ1) * WJ1(L)
 TMP(2,1:NZ1) = KER(K,2,1:NZ1) * WJ0(L)
 TMP(3,1:NZ1) = KER(K,3,1:NZ1) * WJ1(L)
 TMP(4,1:NZ1) = KER(K,4,1:NZ1) * WJ0(L)
 TMP(5,1:NZ1) = KER(K,5,1:NZ1) * WJ1(L)

 FXX(JR,1:NINTG,1:NZ1) = FXX(JR,1:NINTG,1:NZ1) + TMP(1:NINTG,1:NZ1)
 DO JZ = 1, NZ1
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

 END SUBROUTINE PRM_KER_MD

 SUBROUTINE PRM_BOSS_UL_MD (JP,FRQ,NTX,SXN,SXE,SXZ,SXDIP,SXAZM,NRPRM,RHOTRP,NLYR,THKD, &
                            DPTHL,RMUD,SIGL,KSQL,NPLT,PLYR,PLAZM,PLDIP,PLUNJ,MXAB,MXB, &
                            NB,NA,XCELL,YCELL,ZC1L,E_PRYM)
! ---------------------------------------------------------------------------------

!***  Called by LEROI
!***  Calls PRM_HNK_MD, CUBSPL, SETPRM

!  This rutine is used for plates above basement.  It deals with one plate at a time.
!
!  Computes the primary electric field, E_PRYM, due to a magnetic unit dipole
!  source, in NPLT plates lying above basement of a host with NLYR-1 layers
!  above basement.  R23 is used to rotate the fields into the components
!  tangential to each plate.
!
!  Exy, Eyy and Ezy, Exz are not computed because E_PRM is computed in a
!  coordinate system where the source lies in the X-Z plane; ie, the source
!  has is no Y component in this system.
!
!  The target (plate) is discretised along strike into NAL cells of length DA
!  and down dip into NBL cells of width DB.
!
!  For a vertical plate along the X axis, the cells are numbered along strike
!  from the South to the North, first along the top row and then in the same
!  direction along lower rows.  The last cell would be in the bottom
!  North corner.  In plan view, the X and Y components of E_PRYM are positive
!  to the North and East respectively.
!
!          INPUT
!          -----
!          JP - Plate ID
!         FRQ - frequency
!         NTX - number of source positions
!     SXN,SXE - North, East coordinate of each source position
!         SXZ - depth of each source position.  (SXZ < 0 in air; SXZ > 0 below surface)
!       SXDIP - TX inclination of magnetic dipole sources in the vertical plane.
!       SXAZM - azimuths of magnetic dipole sources in radians. (north = 0; east = PI/2)
!      RHOTRP - horizontal interpolation array (15 points / decade) of dimension NRPRM
!        NLYR - number of layers including basement. (1 or 2 only)
!        SIGL - complex xconductivity for all layers including Cole-Cole & dielectric
!        KSQL - iwu * SIGL for all layers (includes Cole-Cole)
!        RMUD - relative magetic permeability
!        THKD - thickness of each layer above basement
!      DEPTHL - depth to top of each layer including basement
!        NPLT - number of plates
!       PLAZM - strike angle in radians
!       PLDIP - dip angle
!       PLUNJ - plunge angles
!        MXAB - maximum number of cells in one plate
!      NA, NB - number of cells along strike & down dip respectively for each plate
!  XCELL(k,*) - north coordinate of centre of cell k
!  YCELL(k,*) - east coordinate of centre of cell k
!     ZC1L(k) - depth of centre of cell row k
!
!          OUTPUT
!          ------
!
!  E_PRYM(1,*) - complex primary electric fields along strike.
!  E_PRYM(2,*) - complex primary electric fields down-dip.
!
!    The first NAB components of E_PRYM are the fields along strike and
!    the second group of NA*NB components are along dip.  Thus for a strike
!    along the X axis, the first NA*NB components would be the X component
!    of the primary electric field and the second NA*NB components
!    would be the Y component multiplied by the cosine of the dip angle.
! -------------------------------------------------------------------------

 USE LG_Filter_Coefficients
 IMPLICIT NONE
 COMPLEX(KIND=QL), PARAMETER :: ZERO = (0._QL, 0._QL)
 REAL, PARAMETER :: MU0=12.56637E-7, TOL0=1.E-3
 COMPLEX, PARAMETER :: ONE=(1.,0.)
 INTEGER NTX,NRPRM,NLYR,NPLT,MXAB,MXB,NROW,NA(NPLT),NB(NPLT),NAL,JAB,NINTG, &
         JP,JS,JB,JA,JL,I1,KFG,SXLYR,RXLYR,PLYR(NPLT)
 REAL, DIMENSION(NTX) :: SXN,SXE,SXZ,SXAZM,SXDIP
 REAL FRQ,RHOTRP(NRPRM),CSDP,SNDP,CSAZ,SNAZ,SAZM,CAZM,CDIP,SDIP,CPLN,SPLN, &
      XCELL(MXAB,NPLT),YCELL(MXAB,NPLT),RHO,RHOSQ,XBAR0,XBAR,YBAR0,YBAR,   &
      XB2,YB2,RAD,XRAD,YRAD,ZRAD,ZR,R23(2,3),ZC1L(MXB)
 REAL, DIMENSION (NPLT) :: PLAZM,PLDIP,PLUNJ
 REAL(KIND=QL) RMUD(0:NLYR),ZS
 REAL(KIND=QL), DIMENSION(NLYR) :: THKD,DPTHL
 REAL, ALLOCATABLE, DIMENSION(:,:) :: G1R,G2R,G3R,G4R,G5R,G1I,G2I,G3I,G4I,G5I
 COMPLEX EFAC,EFAC0,A0,KR,EX,EY,EZ,EXX,EYX,EZX,EXZ,EYZ,EYXP,EZXP,EXZP,EYZP, &
         E_PRYM(2,MXAB,NTX,NPLT),Q1,Q2,Q3,Q4,Q5,EXYZ(3),EAB(2)
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL
 COMPLEX(KIND=QL), DIMENSION(:,:,:), ALLOCATABLE :: FXX
 LOGICAL TILT

 EFAC0 = (0.,1.) * FRQ * MU0  / 2.  ! iwu0 / 4 PI
 E_PRYM(1:2,1:MXAB,1:NTX,JP) = (0.,0.)
 I1 = 1            ! magnetic source

 NROW = NB(JP)
 RXLYR = PLYR(JP)
 IF (RXLYR == NLYR) THEN
   WRITE(4,1); WRITE(*,1)
   STOP
 END IF

!  Set depth array over which to interpolate Green's function integrals

 ALLOCATE (G1R(4,NRPRM),G1I(4,NRPRM),G2R(4,NRPRM),G2I(4,NRPRM),G3R(4,NRPRM),G3I(4,NRPRM), &
           G4R(4,NRPRM),G4I(4,NRPRM),G5R(4,NRPRM),G5I(4,NRPRM),FXX(NRPRM,5,MXB))

!  Set up array FXX which contains values of the F-potential as a function of
!  of horizontal distance and depth.  Step through different altitude levels.

 SOURCE_LOOP: DO JS = 1,NTX
   ZS = REAL (SXZ(JS), KIND=QL)
   SXLYR = 0
   DO JL = NLYR,1,-1
     IF (ZS > DPTHL(JL)) THEN
       SXLYR = JL
       EXIT
     END IF
   END DO
   EFAC = EFAC0 * REAL (RMUD(SXLYR))

   NINTG = 1
   SNDP = 0.
   CSDP = 1.
   SNAZ = 0.
   CSAZ = 1.
   TILT = .FALSE.
   IF (ABS (SXDIP(JS)) > TOL0) THEN
     TILT = .TRUE.
     SNDP = SIN (SXDIP(JS))
     CSDP = COS (SXDIP(JS))
     NINTG = 3
     IF (SXLYR > 0) NINTG = 5
   END IF
   IF (ABS (SXAZM(JS)) > TOL0) THEN
     SNAZ = SIN (SXAZM(JS))
     CSAZ = COS (SXAZM(JS))
   END IF

   FXX = ZERO
   CALL SET_KFG (I1,NLYR,SXLYR,RXLYR,KFG)
   CALL PRM_HNK1_MD (NRPRM,RHOTRP,NROW,ZC1L,ZS,NLYR,THKD,DPTHL, &
                     RMUD,SIGL,KSQL,SXLYR,RXLYR,KFG,NINTG,FXX)
   G1R = 0. ; G2R = 0. ; G3R = 0. ; G4R = 0. ; G5R = 0.
   G1I = 0. ; G2I = 0. ; G3I = 0. ; G4I = 0. ; G5I = 0.

   DO JB = 1,NROW
     G1R(1,1:NRPRM) =        REAL (FXX(1:NRPRM,1,JB))
     G1I(1,1:NRPRM) = REAL (AIMAG (FXX(1:NRPRM,1,JB)) )   ! FXX is in QL precision
     CALL CUBSPL (RHOTRP,G1R,NRPRM)
     CALL CUBSPL (RHOTRP,G1I,NRPRM)

     IF (NINTG > 1) THEN                                ! set up horizontal transmitter components
       G2R(1,1:NRPRM) =        REAL (FXX(1:NRPRM,2,JB))
       G2I(1,1:NRPRM) = REAL (AIMAG (FXX(1:NRPRM,2,JB)) )
       G3R(1,1:NRPRM) =        REAL (FXX(1:NRPRM,3,JB))
       G3I(1,1:NRPRM) = REAL (AIMAG (FXX(1:NRPRM,3,JB)) )
       CALL CUBSPL (RHOTRP,G2R,NRPRM)
       CALL CUBSPL (RHOTRP,G2I,NRPRM)
       CALL CUBSPL (RHOTRP,G3R,NRPRM)
       CALL CUBSPL (RHOTRP,G3I,NRPRM)

       IF (NINTG > 3) THEN                                ! set up F potential induced current contribution
         G4R(1,1:NRPRM) =        REAL (FXX(1:NRPRM,4,JB))
         G4I(1,1:NRPRM) = REAL (AIMAG (FXX(1:NRPRM,4,JB)) )
         G5R(1,1:NRPRM) =        REAL (FXX(1:NRPRM,5,JB))
         G5I(1,1:NRPRM) = REAL (AIMAG (FXX(1:NRPRM,5,JB)) )
         CALL CUBSPL (RHOTRP,G4R,NRPRM)
         CALL CUBSPL (RHOTRP,G4I,NRPRM)
         CALL CUBSPL (RHOTRP,G5R,NRPRM)
         CALL CUBSPL (RHOTRP,G5I,NRPRM)
       END IF
     END IF

     NAL = NA(JP)
     CAZM = COS (PLAZM(JP) - SXAZM(JS))
     SAZM = SIN (PLAZM(JP) - SXAZM(JS))
     CDIP = COS (PLDIP(JP))
     SDIP = SIN (PLDIP(JP))
     CPLN =  COS (PLUNJ(JP))
     SPLN =  SIN (PLUNJ(JP))
     CALL RXYZ2PLT (CAZM,SAZM,CDIP,SDIP,CPLN,SPLN,R23)

     STRYK_STEP: DO JA = 1, NAL
       JAB = JA + (JB-1)*NAL
       EX = (0.,0.) ; EXX = (0.,0.) ; EXZ = (0.,0.)
       EY = (0.,0.) ; EYX = (0.,0.) ; EYZ = (0.,0.)
       EZ = (0.,0.) ; EZX = (0.,0.)
       RHOSQ = (XCELL(JAB,JP) - SXN(JS) )**2 + (YCELL(JAB,JP) - SXE(JS) )**2
       RHO = SQRT (RHOSQ)
       RHO = MAX (0.1, RHO)

!  Compute components in system with X along source azimuth at source position.

       XBAR0 = (XCELL(JAB,JP) - SXN(JS)) / RHO
       YBAR0 = (YCELL(JAB,JP) - SXE(JS)) / RHO
       XBAR = XBAR0 * CSAZ + YBAR0 * SNAZ
       YBAR = YBAR0 * CSAZ - XBAR0 * SNAZ
       XB2 = XBAR**2
       YB2 = YBAR**2

!  Compute direct primary fields if source and receiver are in the same layer.
!  RXLYR > 0 since target cannot be in air.

       EYXP = (0.,0.); EZXP = (0.,0.); EXZP = (0.,0.); EYZP = (0.,0.)
       IF (SXLYR == RXLYR) THEN
         ZR = ZC1L(JB)
         RAD = SQRT (RHOSQ + (ZR - SXZ(JS))**2)
         RAD = MAX (0.1, RAD)
         XRAD = XBAR * RHO / RAD
         YRAD = YBAR * RHO / RAD
         ZRAD = (ZR - SXZ(JS) ) / RAD
         KR = CMPLX (SQRT (KSQL(NLYR))) * RAD
         A0 = (ONE + KR) * EXP (-KR) / RAD**2
         EXZP = YRAD * A0;  EZXP = -EXZP
         EYXP = ZRAD * A0
         EYZP = -XRAD * A0
       END IF

       CALL CCUBVAL (RHOTRP,NRPRM,G1R,G1I,RHO,Q1)
       EXZ =  Q1 * YBAR + EXZP
       EYZ = -Q1 * XBAR + EYZP

       IF (TILT) THEN                          ! set up horizontal transmitter components
         CALL CCUBVAL (RHOTRP,NRPRM,G2R,G2I,RHO,Q2)
         CALL CCUBVAL (RHOTRP,NRPRM,G3R,G3I,RHO,Q3)
         EXX =  XBAR * YBAR * (Q2 - 2.*Q3)

         IF (SXLYR > 0) THEN                           ! Source is subsurface
           CALL CCUBVAL (RHOTRP,NRPRM,G4R,G5I,RHO,Q4)
           CALL CCUBVAL (RHOTRP,NRPRM,G5R,G5I,RHO,Q5)
           EYX = (1. - 2.*YB2) * Q3 + YB2 * Q2 - Q4 + EYXP
           EZX = -YBAR * Q5 + EZXP
         ELSE
           EYX = -( (1. - 2.*XB2) * Q3 + XB2 * Q2 )    ! KS = 0
         END IF
       END IF

       EX = EXX * SNDP + EXZ * CSDP
       EY = EYX * SNDP + EYZ * CSDP
       EZ = EZX * SNDP

       EXYZ(1) = EX;  EXYZ(2) = EY;  EXYZ(3) = EZ
       EAB = EFAC * MATMUL (R23, EXYZ)
       E_PRYM(1,JAB,JS,JP) = EAB(1)
       E_PRYM(2,JAB,JS,JP) = EAB(2)
     END DO STRYK_STEP
   END DO

 END DO SOURCE_LOOP
 DEALLOCATE (FXX,G1R,G2R,G3R,G4R,G5R,G1I,G2I,G3I,G4I,G5I)

 1 FORMAT (T3,' An evil spirit has entered PRM_BOSS_UL_MD.  Seek help !')

 END SUBROUTINE PRM_BOSS_UL_MD

 SUBROUTINE PRM_HNK1_MD (NRPRM,RHOTRP,NROW,ZC1L,ZS,NLYR,THKD,DPTHL, &
                         RMUD,SIGL,KSQL,SXLYR,RXLYR,KFG,NINTG,FXX)
!-----------------------------------------------------------------

!***  Calls PRM_KER_MD
!***  Called by PRM_BOSS

!  Sets up values of G & F potentials for VMD and HMD sources as a function of
!  horizontal distance at discrete depths.  Using the flow through Hankel
!  transform method, it evaluates the Hankel transform integral using a 15 point
!  per decade filter coefficient set derived from Christensen's FLTGEN program.
!  See PRM_BOSS for variable definitions not appearing below.

!       NRPRM - number of horizontal interpolation points
!      RHOTRP - 15 points per decade interpolation array
!        NROW - number of cell rows in Plate JP
!        ZC1L - depth of row centres in Plate JP
!          ZS - transmitter depth
!        NLYR - number of layers including basement. (1 or 2 only)
!        SIGL - complex xconductivity for all layers including Cole-Cole & dielectric
!        KSQL - iwu * SIGL for all layers (includes Cole-Cole)
!        RMUD - relative magetic permeability
!        THKD - thickness of each layer above basement
!       DPTHL - depth to top of each layer including basement
!         KFG - Case selector variable
!       NINTG - number of integrals needed.
!         KER - kernels for Hankel integraion
!
!  FXX(*,1,*) - integrals for EXZ: horizontal electric field from VMD potential
!               sufficient for VMD source
!
!  FXX(*,2,*) - integrals for EXX the inline and EYX the transverse
!  FXX(*,3,*) - horizontal electric field from HMD potential
!
!  FXX(*,4,*) - integrals for EYX from the VED potential
!  FXX(*,5,*) - integrals for EXZ: horizontal electric field from VED potential
!               (The VED potential is due only from a subsurfact non-vertical HMD source

 USE LG_Filter_Coefficients

 IMPLICIT NONE
 COMPLEX(KIND=QL), PARAMETER :: ZERO=(0._QL,0._QL)
 INTEGER L,LMAX,LMIN,K,KMAX,KMIN,KBOT,K1,JR,NROW,NLYR,SXLYR,RXLYR,KFG,JB,NRPRM,NINTG
 REAL RHOTRP(NRPRM),ZC1L(NROW)
 REAL(KIND=QL) RMUD(0:NLYR),ZS,DELTA,Y1,Y,RD,LMBDA
 REAL(KIND=QL), DIMENSION (NLYR) :: THKD,DPTHL
 COMPLEX(KIND=QL) FXX(NRPRM,5,NROW),KER(JNLO-NRPRM:JNHI,5,NROW)
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL
 LOGICAL JUMP

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
   LMBDA = EXP (SNGL(Y))
   CALL PRM_KER1_MD (NRPRM,K,JR,L,LMBDA,NROW,ZC1L,ZS,NLYR,THKD,DPTHL,RMUD, &
                     SIGL,KSQL,SXLYR,RXLYR,KFG,NINTG,KER,FXX,JUMP)
   IF (JUMP) EXIT
 END DO

 Y = Y1                       ! Finish off the low end for RHOTRP(1)
 DO L = -51, JNLO, -1
   LMIN = L                   ! Minimum filter index used
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (SNGL(Y))
   K = L + 1 - JR             ! Compute the kernel index.
   CALL PRM_KER1_MD (NRPRM,K,JR,L,LMBDA,NROW,ZC1L,ZS,NLYR,THKD,DPTHL,RMUD, &
                     SIGL,KSQL,SXLYR,RXLYR,KFG,NINTG,KER,FXX,JUMP)
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

   FXX(JR,1,1:NROW) =  FXX(JR,1,1:NROW) + KER(K,1,1:NROW) * WJ1(L)
   FXX(JR,2,1:NROW) =  FXX(JR,2,1:NROW) + KER(K,2,1:NROW) * WJ0(L)
   FXX(JR,3,1:NROW) =  FXX(JR,3,1:NROW) + KER(K,3,1:NROW) * WJ1(L)
   FXX(JR,4,1:NROW) =  FXX(JR,4,1:NROW) + KER(K,4,1:NROW) * WJ0(L)
   FXX(JR,5,1:NROW) =  FXX(JR,5,1:NROW) + KER(K,5,1:NROW) * WJ1(L)
 END DO

 IF (K1 > KBOT) THEN    !  Add low end kernel values until convergence.
   DO K = K1-1, KBOT, -1
     KMIN = K
     L = K - 1 + JR
     Y = Y1 + DBLE (L) * DELTA
     LMBDA = EXP (SNGL(Y))
     CALL PRM_KER1_MD (NRPRM,K,JR,L,LMBDA,NROW,ZC1L,ZS,NLYR,THKD,DPTHL,RMUD, &
                       SIGL,KSQL,SXLYR,RXLYR,KFG,NINTG,KER,FXX,JUMP)
     IF (JUMP) EXIT
   END DO
 END IF

 DO JR = 2, NRPRM-1          !  Compute transforms for all other RHO values
   DO K = KMIN, KMAX         !  using previously computed kernel values.
     L = K - 1 + JR
     FXX(JR,1,1:NROW) =  FXX(JR,1,1:NROW) + KER(K,1,1:NROW) * WJ1(L)
     FXX(JR,2,1:NROW) =  FXX(JR,2,1:NROW) + KER(K,2,1:NROW) * WJ0(L)
     FXX(JR,3,1:NROW) =  FXX(JR,3,1:NROW) + KER(K,3,1:NROW) * WJ1(L)
     FXX(JR,4,1:NROW) =  FXX(JR,4,1:NROW) + KER(K,4,1:NROW) * WJ0(L)
     FXX(JR,5,1:NROW) =  FXX(JR,5,1:NROW) + KER(K,5,1:NROW) * WJ1(L)
   END DO
 END DO

 DO JB = 1,NROW
   DO L = 1,NINTG
     FXX(1:NRPRM,L,JB) = FXX(1:NRPRM,L,JB) / RHOTRP(1:NRPRM)
   END DO
   IF (NINTG > 1) FXX(1:NRPRM,3,JB) = FXX(1:NRPRM,3,JB) / RHOTRP(1:NRPRM)
 END DO

 END SUBROUTINE PRM_HNK1_MD

 SUBROUTINE PRM_KER1_MD (NRPRM,K,JR,L,LMBDA,NROW,ZC1L,ZS,NLYR,THKD,DPTHL,RMUD, &
                         SIGL,KSQL,SXLYR,RXLYR,KFG,NINTG,KER,FXX,JUMP)
!---------------------------------------------------------------------------

!***  Called by PRM_HNK_MD
!***  Calls MDSX_COEF   (RXLYR = NLYR -1 )

!  Accumulates the integrals for primary E-field in at each depth point in the layer
!  above basement for array FXX due to magnetic dipole.

!     NRPRM - number of points in 15 point / decade array
!         K - filter kernel index
!        JR - RHO interpolation index
!         L - filter point index
!     LMBDA - Hankel transform variable
!      NROW - number of cell rows in Plate JP
!      ZC1L - depth of row centres in Plate JP
!        ZS - source location  (negative in air, positive in ground)
!      NLYR - number of layers including basement.
!      SIGL - complex xconductivity for all layers including Cole-Cole & dielectric
!      KSQL - iwu * SIGL for all layers (includes Cole-Cole)
!      RMUD - relative magetic permeability
!      THKD - thickness of each layer above basement
!     DPTHL - depth to top of each layer including basement
!       KFG - Case selector variable
!     NINTG = number of required integrals = 1 for vertical source.  Otherwise NINTG
!           = 3 for source in air layer and 5 for subsurface source
!
!       KER - kernels for Hankel integraion
!   FXX(*,1,*) - integrals for EXZ: horizontal electric field from VMD potential
!                sufficient for VMD source
!
!          FXX - accumulated potential array
!   FXX(*,2,*) - integrals for EXX the inline and EYX the transverse
!   FXX(*,3,*) - horizontal electric field from HMD potential
!
!   FXX(*,4,*) - integrals for EYX from the VED potential
!   FXX(*,5,*) - integrals for EXZ: horizontal electric field from VED potential
!                (The VED potential is due only from a subsurfact non-vertical HMD source
!
!      JUMP - keep integrating if false.

 USE LG_Filter_Coefficients
 IMPLICIT NONE
 REAL, PARAMETER :: TOL=1.E-5
 COMPLEX(KIND=QL), PARAMETER :: ZERO=(0._QL,0._QL)
 INTEGER JR,L,K,NROW,NLYR,RXLYR,SXLYR,KFG,JB,NRPRM,NINTG,JINT
 REAL ZC1L(NROW)
 REAL(KIND=QL) RMUD(0:NLYR),LMBDA,LMBDA2,ZS,ZR,AR,AI,FXXR,FXXI
 REAL(KIND=QL), DIMENSION (NLYR) :: THKD,DPTHL
 COMPLEX (KIND=QL) XI_V,XI_H,F_V,ETA_V,ETA_H,G_V,XPU,XPD,FV1,FV2,KV,KS,SL,SM,S(0:NLYR), &
                   KER(JNLO-NRPRM:JNHI,5,NROW),FXX(NRPRM,5,NROW),TMP(5,NROW)
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL
 LOGICAL TOO_BIG,JUMP

! Compute the kernels for layer above basement for the J0, J1 integrations.
! Eliminate underflows due to exponentials.

 TMP = ZERO
 KER(K,1:5,1:NROW) = ZERO

 CALL MDSX_COEF (SXLYR,RXLYR,KFG,LMBDA,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                 ZS,S,XI_V,XI_H,F_V,ETA_V,ETA_H,G_V)
 SL = S(RXLYR)
 SM = S(SXLYR)
 LMBDA2 = LMBDA**2

 SELECT CASE (NINTG)
 CASE(1)
   DO JB = 1, NROW
     ZR = REAL (ZC1L(JB),QL)
     XPU = EXP (SL * (ZR - DPTHL(RXLYR+1)))
     XPD = EXP (SL * (DPTHL(RXLYR) - ZR))
     KER(K,1,JB) = (XI_V * XPU + ETA_V * XPD) * LMBDA2 / SM
   END DO

 CASE(3)
   DO JB = 1, NROW
     ZR = REAL (ZC1L(JB),QL)
     XPU = EXP (SL * (ZR - DPTHL(RXLYR+1)))
     XPD = EXP (SL * (DPTHL(RXLYR) - ZR))
     KV = XI_H * XPU + ETA_H * XPD

     KER(K,1,JB) = (XI_V * XPU + ETA_V * XPD) * LMBDA2 / SM
     KER(K,2,JB) = KV * LMBDA
     KER(K,3,JB) = KV
   END DO

 CASE(5)
   FV1 = SIGL(SXLYR) / SIGL(RXLYR)
   FV2 = FV1 * S(RXLYR) / S(SXLYR)
   DO JB = 1, NROW
     ZR = REAL (ZC1L(JB),QL)
     XPU = EXP (SL * (ZR - DPTHL(RXLYR+1)))
     XPD = EXP (SL * (DPTHL(RXLYR) - ZR))
     KV = XI_H * XPU + ETA_H * XPD
     KS = FV2 * (F_V * XPU - G_V * XPD)

     KER(K,1,JB) = (XI_V * XPU + ETA_V * XPD) * LMBDA2 / SM
     KER(K,3,JB) = (KV - KS)
     KER(K,2,JB) = KER(K,3,JB) * LMBDA
     KER(K,4,JB) = KV * LMBDA
     KER(K,5,JB) = (F_V* XPU + G_V * XPD) * FV1 * LMBDA2 / SM
   END DO
 END SELECT

 JUMP = .TRUE.
 TMP(1,1:NROW) = KER(K,1,1:NROW) * WJ1(L)
 TMP(2,1:NROW) = KER(K,2,1:NROW) * WJ0(L)
 TMP(3,1:NROW) = KER(K,3,1:NROW) * WJ1(L)
 TMP(4,1:NROW) = KER(K,4,1:NROW) * WJ0(L)
 TMP(5,1:NROW) = KER(K,5,1:NROW) * WJ1(L)

 FXX(JR,1:NINTG,1:NROW) = FXX(JR,1:NINTG,1:NROW) + TMP(1:NINTG,1:NROW)
 DO JB = 1, NROW
   DO JINT = 1,NINTG
     AR = ABS ( REAL (TMP(JINT,JB)) )
     AI = ABS (AIMAG (TMP(JINT,JB)) )
     TOO_BIG = .FALSE.
     FXXR = ABS ( REAL (FXX(JR,JINT,JB)) )
     FXXI = ABS (AIMAG (FXX(JR,JINT,JB)) )
     IF (AR > TOL* FXXR) TOO_BIG = .TRUE.
     IF (AI > TOL* FXXI) TOO_BIG = .TRUE.
     IF (TOO_BIG) JUMP = .FALSE.
   END DO
 END DO

 END SUBROUTINE PRM_KER1_MD

 SUBROUTINE RPLT2XYZ (CA,SA,CD,SD,CP,SP,R32)
!-------------------------------------------

! Builds rotation matrix R32 to transform plate XI, ETA, coordinates or
! components to global X,Y,Z representation.  These rotations are based on the
! original Leroi convention where the plate azimuthal angle is the user-
! specified azimuth - PI/2.  This convention is used to preserve the strong
! nexus between the structure of the electric and magnetic Green's tensor
! development and the initial XI = north convention for the pre-rotated pate.

! All rotations are about axes passing through X0, Y0, Z0 at the midpoint of the
! westernern edge of a flat plate whose initial XI coordinate points north and
! whose initial ETA coordinate points east.  Plate orientation begins with an
! azimuth rotation (positive clockwise) about the Z axis (positive down,
! north = 0) followed by a dip rotation, positive clockwise about the rotated XI
! axis followed by a plunge rotation about the plate normal.
!
! In practice a mathematical equivalent is used which consists of rotations
! about fixed axes in reverse order.  The PLUNGE rotation is performed about the
! Z axis followed by a DIP rotation about the X axis followed by the azimuth
! rotation PHI about the Z axis
!
!  CA, SA - cosine and sine of the redefined plate azimuth
!  CD, SD - cosine and sine of plate dip
!  CP, SP - cosine and sine of plate plunge

 IMPLICIT NONE
 REAL CA,SA,CP,SP,CD,SD,R32(3,2),A1(3,3),D1(3,2),P1(2,2),S1(3,2)

 P1 = RESHAPE ((/CP, SP,      -SP, CP               /),  (/2,2/))
 D1 = RESHAPE ((/1., 0., 0.,   0., CD, SD           /),  (/3,2/))
 A1 = RESHAPE ((/CA, SA, 0.,  -SA, CA, 0.,  0.,0.,1./),  (/3,3/))

 S1 =  MATMUL (D1,P1)
 R32 = MATMUL (A1,S1)

 END SUBROUTINE RPLT2XYZ

 SUBROUTINE RXYZ2PLT (CA,SA,CD,SD,CP,SP,R23)
!-------------------------------------------

!***  Called by SET_CELLS

! Builds rotation matrix R23 to transform global X,Y,Z coordinates or components
! to plate XI, ETA representation.  Following the convention described in
! SUBROUTINE RPLT2XYZ, this is done by reversing the rotations described in
! RPLT2XYZ.
!
!  CA, SA - cosine and sine of plate azimuth
!  CD, SD - cosine and sine of plate dip
!  CP, SP - cosine and sine of plate rotation


 REAL CA,SA,CP,SP,CD,SD,R23(2,3),P2(2,2),D2(2,3),A2(3,3),S2(2,3)

 A2 = RESHAPE ((/CA, -SA, 0.,  SA, CA, 0.,  0., 0., 1./), (/3,3/))
 D2 = RESHAPE ((/1., 0.,  0., CD,  0., SD             /), (/2,3/))
 P2 = RESHAPE ((/CP, -SP,      SP, CP                 /), (/2,2/))

 S2 =  MATMUL (D2,A2)
 R23 = MATMUL (P2,S2)

 END SUBROUTINE RXYZ2PLT

 SUBROUTINE SET_CELLS_1 (IPR,DPTHB,NLYR,NPLT,PLYR,CELLW,PLNGTH,PLWDTH,PLTOP,PLAZM, &
                         PLDIP,PLUNJ,NA,NB,DA,DB,MXCL2,MXAB,NCELL2,ZMIN,ZMAX,intrude)
!-------------------------------------------------------------------------------------
!
!	2024-06-12: intrude arguement added so that target celles are computed regardless.
!		  : hooks to print target corners are retained but commented out.  These can be useful.
!
!***  Called by LEROI
!***  Calls RPLT2XYZ

!  This routine sets up the dimensions for the plate cells, establishes vertical
!  boundaries for interpolation within the basement and shifts basement plates
!  downwards if plunge would cause a corner to protrude above basement.
!
!  Initially, Leroi was set up assuming that prior to rotation the initial
!  plate strike direction, XI pointed north and ETA pointed east.  After
!  rotation, the plate azimuth was defined by XI, the direction of the top edge
!  of the plate.  In order to comply with the way that surfaces are specified
!  mathematically and geologically, this was changed so that plate azimuth was
!  defined by the normal to plate, thus implying that the initial direction of
!  XI was west.
!
!  This is accomplished by the simple kludge of letting the user specify azimuth
!  by the plate normal and then converting this to the original azimuth
!  (designated by XI) by subtracting PI/2.  This allows the preservation of the
!  strong nexus between XI & X and between ETA & Y in the electric and magnetic
!  Green's tensor computations.
!  Y axis.
!
!  Thus the plates will be defined by the user in a coordinate system where XI
!  points west, ETA points north and Z points down.  All rotations are about
!  axes that pass through (X0, Y0, Z0).  They are positive clockwise looking
!  along the positive axis direction.
!
!
!           INPUT AZIMUTH = PHI                       TRANSFORMED AZIMUTH = PHI - 90
!
!                                                                    N
!                    N
!                                                                    ^
!                    ^                                               |
!                    |                                               |
!                    |
!                                                           2 _______________ 3
!        3 _____________________ 4                           |               |
!         |                     |                            |   ETA -- >    |
!         |                     |                            |               |
!         |                     |                            |               |
!     ^   |                     |                            |               |
!     |   |                     |                            |  ^            |
!     |   |                     |  --> E                     |  |            |
!         |                     |                  X0,Y0,Z0  *  |            |   --> E
!    ETA  |                     |                            |  XI           |
!         |                     |                            |               |
!         |                     |                            |               |
!         |__________*__________|                            |               |
!        2                       1                           |               |
!                 X0,Y0,Z0                                   |               |
!                                                            |_______________|
!               <-- XI                                      1                  4
!
!
!  The plate position can be described by an azimuthal rotation (PLAZM) about
!  the vertical axis, (positive clockwise) a dip rotation (PLDIP) about the new
!  XI axis, (positive clockwise looking along the XI axis) followed by a
!  clockwise plunge rotation (PLUNJ) about the new Z axis.  PLAZM is the user-
!  specified azimuth (in radians) - PI/2.
!
!  Mathematically, this is equivalent to rotations about the fixed Z and X axes.
!  Thus in this routine the plate position is described by an initial plunge
!  (PLUNJ) rotation about the Z axis followed by PLDIP about the X axis followed
!  by PLAZM about the Z axis.
!
!  -90 <= PLUNJ  <=  90  is applied about the Z axis followed by
!    0 <= PLDIP  <= 180  applied about the X axis followed by
!    0 <= PLAZM  <= 180  applied about the spatial Z axis
!
!        Input variables
!        ---------------
!
!   IPR       = output unit number for Leroi.out if printout is enabled
!             = 0 if printout is suppressed
!   DPTHB     - depth to basement
!   NLYR      - Number of Layers
!   NPLT      - Number of plates
!   PLYR      - Layer number for each plate
!   CELLW     - nominal cell dimension
!   PLNGTH    - strike length for each plate
!   PLWDTH    - dip width for each plate
!   PLTOP     - depth to plate reference & pivot point
!   PLAZM     - plate azimuths
!   PLDIP     - plate dip angles
!   PLUNJ     - plunge angles
!
!          Output variables
!          ----------------
!
!   NA, NB      - number of cells along strike and down dip respectively
!   DA, DB      - dimension of cells along strike and down dip respectively
!   MXAB        - Number of cells in biggest plate
!   NCELL2(J)   - 2 * the number of cells in plates 1 to J
!   MXCL2       - 2 * total number of cells in all plates = NCELL2(NPLT)
!   ZMIN, ZMAX  - minimum and maximum levels for Green's function interpolation
!                 in vertical direction for basement plates.

 IMPLICIT NONE
 INTEGER IPR,NW,MXAB,MXCL2,NLYR,NPLT,PLYR(NPLT),NCELL2(0:NPLT),JP,JB,KMPL
 INTEGER, DIMENSION(NPLT) :: NA,NB
 REAL CELLW,P(2),XYZ(3),R32(3,2),DPTHB,CA,SA,CP,SP,CD,SD,ZMINP,ZMIN,ZMAX,TMPL
 REAL, DIMENSION(4) :: XCNR,YCNR,ZCNR
 REAL, DIMENSION(NPLT) :: PLNGTH, PLWDTH,PLTOP,SHIFT,PLAZM,PLUNJ,PLDIP,DA,DB
 LOGICAL :: prt, INTRUDE
! INTEGER :: newu

 !Open (File = 'Leroi.trg', Newunit = newu, Status = 'Unknown')

 NCELL2 = 0 ; MXAB = 0

 IF (MAXVAL (PLYR) < NLYR) THEN
   ZMIN = 1.0
   ZMAX = 2.0
 ELSE
   ZMIN = 1.E6 ;  ZMAX = -1.E6
 END IF

 IF (IPR == 0) THEN
   NW = -9
   PRT = .FALSE.
 ELSE
   NW = IPR
   PRT = .TRUE.
 END IF

 SHIFT = 0.
 DO JP = 1, NPLT      !  Sum over plates
   TMPL = MIN (PLNGTH(JP), PLWDTH(JP)) / 2.   ! Cell numbers and sizes
   TMPL = MIN (TMPL, CELLW) + .01
   NA(JP) = CEILING (PLNGTH(JP) / TMPL)
   NA(JP) = MAX (2,NA(JP))
   DA(JP) = PLNGTH(JP) / REAL (NA(JP))


   NB(JP) = CEILING (PLWDTH(JP) / TMPL)
   NB(JP) = MAX (2,NB(JP))                  ! Number of rows
   DB(JP) = PLWDTH(JP) / REAL (NB(JP))      ! Width of rows
   KMPL = NA(JP) * NB(JP)
   MXAB = MAX (MXAB,KMPL)
   NCELL2(JP) = NCELL2(JP-1) + 2* KMPL

   ! IF (PLYR(JP) == NLYR) THEN               ! Restrict basement plates to basement

     CA = COS (PLAZM(JP));   SA = SIN (PLAZM(JP))
     CD = COS (PLDIP(JP));   SD = SIN (PLDIP(JP))
     CP = COS (PLUNJ(JP));   SP = SIN (PLUNJ(JP))

     CALL RPLT2XYZ (CA,SA,CD,SD,CP,SP,R32)  ! Rotation matrix

     XCNR(2:3) = PLNGTH(JP) / 2.
     XCNR(1) = -XCNR(2);  XCNR(4) = XCNR(1)
     YCNR(1:2) = 0.
     YCNR(3:4) = PLWDTH(JP)

     DO JB = 1,4
       P(1) = XCNR(JB)
       P(2) = YCNR(JB)
       XYZ = MATMUL (R32,P)
       ZCNR(JB) = XYZ(3) + PLTOP(JP)
       xcnr(jb) = xyz(1)
       ycnr(jb) = xyz(2)
     END DO
     ZMINP = MINVAL (ZCNR)

     If (.not. INTRUDE) Then
         IF (ZMINP < DPTHB) THEN
           SHIFT(JP) = DPTHB - ZMINP
           ZCNR(1:4) = ZCNR(1:4) + SHIFT(JP)
           PLTOP(JP) = PLTOP(JP) + SHIFT(JP)
         END IF
     END IF

     IF (PRT .AND. SHIFT(JP) > 0.01) THEN
       WRITE(*,1)   JP,SHIFT(JP)
       WRITE(NW,1)   JP,SHIFT(JP)
     END IF
     ZMIN = MIN (ZMIN, MINVAL (ZCNR))
     ZMAX = MAX (ZMAX, MAXVAL (ZCNR))
   ! END IF
!   Write (newu, *) xcnr
!   Write (newu, *) ycnr
!   Write (newu, *) zcnr
 END DO
 ZMIN = MAX (ZMIN, DPTHB + 0.01)
 MXCL2 = NCELL2(NPLT)
! Close(newu)

 1 FORMAT (//T3,'********************************************************'     &
            /T3,'Plate',I3,' has been shifted down by',F7.2,' m to prevent it' &
            /T3,'from extending above basement.'                               &
            /T3,'********************************************************')

 END SUBROUTINE SET_CELLS_1

 SUBROUTINE SET_CELLS_2 (IPR,NPLT,MXAB,XCNTR,YCNTR,PLTOP,PLAZM,PLDIP, &
                         PLUNJ,NA,NB,DA,DB,XCELL,YCELL,ZCELL)
!-------------------------------------------------------------------

!***  Called by LEROI
!***  Calls RPLT2XYZ

!        Input variables
!        ---------------
!
!   IPR       = output unit number for Leroi.out if printout is enabled
!             = 0 if printout is suppressed
!   NPLT      - Number of plates
!   MXAB      - Number of cells in biggest plate
!   XCNTR     - north coordinates of plate reference & pivot point
!   YCNTR     - east coordinates of plate reference & pivot point
!   PLTOP     - depth to plate reference & pivot point
!   PLAZM     - plate azimuths
!   PLDIP     - plate dip angles
!   PLUNJ     - plunge angles
!   NA, NB    - number of cells along strike and down dip respectively
!   DA, DB    - dimension of cells along strike and down dip respectively
!
!          Output variables
!          ----------------
!
!   XCELL(k,JP) - X (north) coordinate of centre of cell k of plate JP.
!   YCELL(k,JP) - Y (east) coordinate of centre of cell k of plate JP.
!   ZCELL(i,JP) - depth of cell centre in row i of plate nrelative to surface.

 IMPLICIT NONE
 REAL, PARAMETER :: TOL=0.001
 INTEGER IPR,NW,MXAB,NPLT,JP,JA,JB,JAB,NBL
 INTEGER, DIMENSION(NPLT) :: NA,NB
 REAL XI0,ETA0,P(2),XYZ(3),R32(3,2),CA,SA,CP,SP,CD,SD
 REAL, DIMENSION(NPLT) :: XCNTR,YCNTR,PLTOP,PLAZM,PLUNJ,PLDIP,DA,DB
 REAL, DIMENSION(MXAB,NPLT) :: XCELL,YCELL,ZCELL
 LOGICAL PRT

!  Lay out initial cell pattern in coordinate system centred 1t (XCNTR, YCNTR, PLTOP)
!  Compute corner coordinates as well.

 XCELL = 0. ; YCELL = 0. ; ZCELL = 0.

 IF (IPR == 0) THEN
   NW = -9
   PRT = .FALSE.
 ELSE
   NW = IPR
   PRT = .TRUE.
 END IF

 DO JP = 1, NPLT      !  Sum over plates

   CA = COS (PLAZM(JP));   SA = SIN (PLAZM(JP))
   CD = COS (PLDIP(JP));   SD = SIN (PLDIP(JP))
   CP = COS (PLUNJ(JP));   SP = SIN (PLUNJ(JP))

   CALL RPLT2XYZ (CA,SA,CD,SD,CP,SP,R32)  ! Rotation matrix

   JAB = 0
   XI0 =  -(NA(JP) + 1) * DA(JP) / 2.
   ETA0 = -DB(JP) /2.
   NBL = NB(JP)

   DO JB = 1, NBL
     DO JA = 1, NA(JP)
       JAB = JAB + 1
       P(1) = XI0 + JA* DA(JP)
       P(2) = ETA0 + JB* DB(JP)
       XYZ = MATMUL (R32,P)

       XCELL(JAB,JP) = XYZ(1) + XCNTR(JP)
       YCELL(JAB,JP) = XYZ(2) + YCNTR(JP)
       ZCELL(JAB,JP) = XYZ(3) + PLTOP(JP)
       IF (ABS (XCELL(JAB,JP)) < TOL) XCELL(JAB,JP) = 0.
       IF (ABS (YCELL(JAB,JP)) < TOL) YCELL(JAB,JP) = 0.
     END DO
   END DO
 END DO

 IF (PRT) THEN
   DO JP = 1,NPLT
     WRITE(NW,4) JP,NA(JP),DA(JP),NB(JP),DB(JP)
     WRITE(NW,5) JP
     DO JB = 1, NB(JP)
       DO JA = 1, NA(JP)
         JAB = JA + ((JB-1) * NA(JP))
         IF (ABS (XCELL(JAB,JP)) < TOL) XCELL(JAB,JP) = 0.
         IF (ABS (YCELL(JAB,JP)) < TOL) YCELL(JAB,JP) = 0.
         IF (JA == 1) THEN
           IF (JB > 1) WRITE(NW,'(A)') '  '
           WRITE(NW,'(I5,I7,6X,3G13.4)') JB,JA,YCELL(JAB,JP),XCELL(JAB,JP),ZCELL(JAB,JP)
         ELSE
           WRITE(NW,'(5X,I7,6X,3G13.4)')    JA,YCELL(JAB,JP),XCELL(JAB,JP),ZCELL(JAB,JP)
         END IF
       END DO
     END DO
   END DO
 END IF

 4 FORMAT(/T3,'Plate',I3,' is discretised into:', &
          /T6,I3,' columns of cells of length',F7.2,' metres along strike and' &
          /T6,I3,' rows of cells of length',F7.2,' metres down dip.')
 5 FORMAT (//11X,'Target Cell Centre Locations for Plate',I2         &
            /11X,'----------------------------------------'          &
           //'   Dip   Strike                      '                 &
            /'  Index  Index        (East)      (North)       Depth' &
            /'  -----  ------       ------      -------       -----')
 END SUBROUTINE SET_CELLS_2

 SUBROUTINE SCAT_MTRX_BOSS (NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,NPLT,NTPL,IDPL,PLYR,MXB,MXAB,NCELL2, &
                            MXCL2,NA,NB,DA,DB,SIGT,KSQT,PLTOP,PLAZM,PLDIP,PLUNJ,XCELL,YCELL,    &
                            ZCELL,NZ2,ZV2,ZC1,NREGT,RHOTRP,NTX,E_PRYM,J_SCAT,DCMP_FAIL)
!----------------------------------------------------------------------------------------------
!  Sets up the global multi-plate system matrix SCAT_MTRX from the Green's
!  tensor elements.  Using Weidelt's formalism to construct the system matrix,
!  SCAT_MTRX_LU_DCMP is called to replace SCAT_MTRX by its LU decomposition.
!  Finally, SCAT_CRNT is called to yield the scattering currents on each plate.

!***  Called by MAIN
!***  Calls EGT_BOSS, INTER_EGT_BOSS, SCAT_MTRX_LU_DCMP, SCAT_CRNT

!  In what follows, the GTE have the units of electric field divided by a
!  factor (-iwu) because this factor is explicitly included as a multiplier
!  outside the integral over area when solving the integral equation.
!
!  These modified integrated Green's functions are initially computed in the form:
!
!  GAA = SAA + VAA /KSQN;  GAB = SAB + VAB /KSQN;
!                             GBA = SAB + VBA /KSQN  &
!  GBB = SBB + VBB /KSQN   where  KSQN :=  i * omega * mu / RES(NLYR).
!
!  SAA, SAB=SBA, & SBB are the induced (divergence free) part of the GTE,
!  corresponding to currents enclosed within the plate.
!  VAA, VAB, VBA, & VBB were originally the curl-free or current channelling
!  part of the GTE.
!
!  SAA(KS,KR,IR) is the electric field in the XI (strike) direction, integrated
!  over receiver cell (IR,KR) due to a XI-oriented electric dipole at cell (1,KS).
!  Similarly, SAB(KS,KR,IR) is the electric field in the ETA direction caused
!  by the same dipole.  SBB is the ETA electric field caused by an
!  ETA-oriented electric dipole.  Similarly for VAA, VAB, VBA, & VBB.
!
!  The solution vector is expressed as the curl and gradient of two
!  potential functions in the form CURL (*C* PSI) + KSQN * GRAD (PHI) where
!  *C* is a unit vector perpendicular to the plate surface.  The actual form of
!  the solution vector consists of the values of the PSI and PHI potentials on
!  the corners of the plate cells.  Thus in building the global matrix,
!  SCAT_MTRX_BUILD has to account for the curl and grad operations as well as
!  including the Green's tensor elements/
!
!  The big advantage of Weidelt's formulation is that the 1 / KSQ_LYR term can be
!  removed since the surface integrals of HIJ * CURL (*C* PSI) are identically
!  zero.  SUBROUTINE SCAT_MTRX_BUILD thus uses the redefined
!  HIJ - HIJ + SIJ* KSQN input from EGT_BOSS.
!
!  For E := either E_PRYM or J_SCAT -
!
!  E (1,JCELL,JS,JP) is the field (current) in the strike direction in the
!                       centre of plate cell number JCELL of plate JP due
!                       to transmitter JS
!  E (2,JCELL,JS,JP) is the field (current) in the down-dip direction in the
!                       centre of plate cell number JCELL of plate JP due
!                       to transmitter JS
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
!        SIGT(J) - complex conductivity thickness product of plate J
!        KSQT(J) - iwu * SIGT
!        SIGL(J) - complex conductivity thickness product of layer J
!        KSQL(J) - iwu * SIGL
!          DPTHB - depth to basement
!          PLDIP - dip angle for each plate
!          PLUNJ - plunge angles
!         RHOTRP - interpolation array for electric Green's functions
!          NREGT - maximum index for RHOTRP
!           NTX - number of transmitter locations
!         E_PRYM - primary electric field incident of each plate.
!
!         INTERNAL COMPUTATIONS
!         ---------------------
!  SAA, SBA, SBB - complex Green's kernels from EGT_BOSS for vortex currents
!                  (closed in the plate)
!  VAA, VAB, VBA - complex Green's kernels from EGT_BOSS for current gathering
!          & VBB   (currents closed outside the plate) + the SIJ * BAS
!      SCAT_MTRX - LU decomposed form of system matrix
!           INDX - pivoting vector information

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 COMPLEX, PARAMETER :: ZERO=(0.,0.)
 INTEGER NTX,NLYR,RXLYR,NPLT,NTPL,NAL,NBL,NAB,MXCL2,MCELL2,NCELL2(0:NPLT),MXAB,MXA,MXB,IPHI(4),IPSI(4), &
         KB,JB,KA,JA,JZ,L,IDX(4),INDX(MXCL2),NREGT,JP,JPS,JPR,JSX,JRX,JSXRX,KAB,KABG,JAB,NZ2
 INTEGER, DIMENSION(NPLT) :: NA,NB,PLYR,IDPL
 REAL AD(4),BD(4),GA,GB,DAL,DBL,DPTHB,PTOPL,CAZM,SAZM,CDP,SDP,CPL,SPL, &
      RHOTRP(NREGT),ZTR,ZV2(NZ2),ZC1(MXB,NTPL),ZC1L(MXB)
 REAL(KIND=QL) RMUD(0:NLYR),THKD(NLYR),DPTHL(NLYR)
 REAL, DIMENSION (NPLT) :: DA,DB,PLTOP,PLAZM,PLDIP,PLUNJ
 REAL, DIMENSION (MXAB,NPLT) :: XCELL,YCELL,ZCELL,AREA
 REAL, DIMENSION(4,NREGT) :: GR1,GI1,GR2,GI2,GR3,GI3,GR4,GI4,GR5,GI5,GR6,GI6
 REAL, DIMENSION(4,NREGT,NZ2) :: GR1Z,GI1Z,GR2Z,GI2Z,GR3Z,GI3Z,GR4Z,GI4Z,GR5Z,GI5Z,GR6Z,GI6Z
 REAL, ALLOCATABLE, DIMENSION(:) :: XCEL1,YCEL1,ZCEL1
 COMPLEX KSQN,SCAT_MTRX(MXCL2,MXCL2),E_PRYM(2,MXAB,NTX,NPLT),J_SCAT(2,MXAB,NTX,NPLT), &
         AAS,ABSS,BAS,BBS,AAV,ABV,BAV,BBV,APSI(4),BPSI(4),APHI(4),BPHI(4)
 COMPLEX,DIMENSION(NPLT) :: KSQT, SIGT
 COMPLEX(KIND=QL), DIMENSION(NLYR) :: KSQL, SIGL
 COMPLEX,DIMENSION(:,:,:,:),ALLOCATABLE :: SAA,SBA,SBB,VAA,VAB,VBA,VBB
 COMPLEX,DIMENSION(:,:),ALLOCATABLE :: VAAI,VABI,VBAI,VBBI,SAAI,SABI,SBAI,SBBI
 LOGICAL DCMP_FAIL

 KSQN = CMPLX (KSQL(NLYR))
 DPTHB = REAL (DPTHL(NLYR))              ! Depth to basement

 SCAT_MTRX = ZERO

!  Set depth array over which to interpolate Green's function integrals

 GR1Z = 0; GR2Z = 0; GR3Z = 0; GR4Z = 0; GR5Z = 0; GR6Z = 0
 GI1Z = 0; GI2Z = 0; GI3Z = 0; GI4Z = 0; GI5Z = 0; GI6Z = 0

 DO JZ = 1,NZ2
   ZTR = ZV2(JZ)
   CALL EGT_CSPL (NREGT,RHOTRP,NLYR,THKD,RMUD,SIGL,KSQL,ZTR,GR1,GI1, &
                  GR2,GI2,GR3,GI3,GR4,GI4,GR5,GI5,GR6,GI6)

   GR1Z(1:4,1:NREGT,JZ) = GR1(1:4,1:NREGT)
   GR2Z(1:4,1:NREGT,JZ) = GR2(1:4,1:NREGT)
   GR3Z(1:4,1:NREGT,JZ) = GR3(1:4,1:NREGT)
   GR4Z(1:4,1:NREGT,JZ) = GR4(1:4,1:NREGT)
   GR5Z(1:4,1:NREGT,JZ) = GR5(1:4,1:NREGT)
   GR6Z(1:4,1:NREGT,JZ) = GR6(1:4,1:NREGT)
   GI1Z(1:4,1:NREGT,JZ) = GI1(1:4,1:NREGT)
   GI2Z(1:4,1:NREGT,JZ) = GI2(1:4,1:NREGT)
   GI3Z(1:4,1:NREGT,JZ) = GI3(1:4,1:NREGT)
   GI4Z(1:4,1:NREGT,JZ) = GI4(1:4,1:NREGT)
   GI5Z(1:4,1:NREGT,JZ) = GI5(1:4,1:NREGT)
   GI6Z(1:4,1:NREGT,JZ) = GI6(1:4,1:NREGT)
 END DO

! Set up the single plate - host interactions ; ie the block diagonal
! part of the scattering matrix.

 PLATE_LOOP: DO JP = 1,NPLT
   PTOPL = PLTOP(JP)
   NAL = NA(JP)  ! Set up local dimensions for single plate operations
   NBL = NB(JP)
   NAB = NAL * NBL
   DAL = DA(JP)
   DBL = DB(JP)
   DO JAB = 1,NAB
     AREA(JAB,JP) = DAL * DBL
   END DO
   CAZM = COS (PLAZM(JP))
   SAZM = SIN (PLAZM(JP))
   CDP = COS (PLDIP(JP))
   SDP = SIN (PLDIP(JP))
   CPL = COS (PLUNJ(JP))
   SPL = SIN (PLUNJ(JP))
   MCELL2 = NCELL2(JP-1)    ! Index to start each plate

! Compute electric Green's tensor integrals,

   ALLOCATE (SAA(NBL,NBL,NAL,NAL),SBA(NBL,NBL,NAL,NAL),SBB(NBL,NBL,NAL,NAL), &
             VAA(NBL,NBL,NAL,NAL),VAB(NBL,NBL,NAL,NAL),VBA(NBL,NBL,NAL,NAL), &
             VBB(NBL,NBL,NAL,NAL),XCEL1(NAB),YCEL1(NAB),ZCEL1(NAB))


   IF (ABS (SPL) > 0.01) THEN    ! Plunge must be taken into account
     CALL EGT_PL_BOSS (NAL,NBL,NAB,DAL,DBL,KSQN,DPTHB,PTOPL,CDP,SDP,CPL,SPL,NZ2,ZV2, &
                       NREGT,RHOTRP,GR1Z,GI1Z,GR2Z,GI2Z,GR3Z,GI3Z,GR4Z,GI4Z,GR5Z,    &
                       GI5Z,GR6Z,GI6Z,SAA,SBA,SBB,VAA,VAB,VBA,VBB)

   ELSE

!  Use XI symmetry.  Azimuth is irrelevant for single plate EGT
!  Define local cell separations along strike!

     DO JAB = 1,NAB
       XCEL1(JAB) = XCELL(JAB,JP) * CAZM + YCELL(JAB,JP) * SAZM
       YCEL1(JAB) = YCELL(JAB,JP) * CAZM - XCELL(JAB,JP) * SAZM
       ZCEL1(JAB) = ZCELL(JAB,JP)
     END DO

     IF (PLYR(JP) == NLYR) THEN

       CALL EGT_BOSS (NAL,NBL,NAB,DAL,DBL,KSQN,DPTHB,XCEL1,YCEL1,ZCEL1,CDP,SDP,CPL, &
                      SPL,NZ2,ZV2,NREGT,RHOTRP,GR1Z,GI1Z,GR2Z,GI2Z,GR3Z,GI3Z,GR4Z,  &
                      GI4Z,GR5Z,GI5Z,GR6Z,GI6Z,SAA,SBA,SBB,VAA,VAB,VBA,VBB)
     ELSE
       RXLYR = PLYR(JP)
       JPS = IDPL(JP)
       ZC1L(1:NBL) = ZC1(1:NBL,JPS)
       CALL EGT_UL_BOSS (NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,NAL,NBL,NAB,DAL,DBL,XCEL1,YCEL1, &
                         ZC1L,RXLYR,CDP,SDP,NREGT,RHOTRP,SAA,SBA,SBB,VAA,VAB,VBA,VBB)
     END IF
   END IF

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
         LOOP_KB_NBL: DO KB = 1,NBL
           KAB = MCELL2   + 2* ((KB-1) *NAL + KA)

! SAB, VAB, & VBA change sign if the XI (source cell) > XI (receiver cell);
! ie, EGT_BOSS assumes the source to the left of the receiver.

           AAS = SAA(KB,JB,KA,JA)
           BAS = SBA(KB,JB,KA,JA)
           BBS = SBB(KB,JB,KA,JA)
           AAV = VAA(KB,JB,KA,JA)
           ABV = VAB(KB,JB,KA,JA)
           BAV = VBA(KB,JB,KA,JA)
           BBV = VBB(KB,JB,KA,JA)

           APSI(1) = KSQT(JP) * (- GB*AAS + GA*BAS)   ! cell corner PSI contribution
           APSI(2) = KSQT(JP) * (- GB*AAS - GA*BAS)   ! to along strike field
           APSI(3) = -APSI(2)
           APSI(4) = -APSI(1)
           BPSI(1) = KSQT(JP) * (-GB*BAS + GA*BBS)    ! cell corner PSI contribution
           BPSI(2) = KSQT(JP) * (-GB*BAS - GA*BBS)    ! to down dip field
           BPSI(3) = -BPSI(2)
           BPSI(4) = -BPSI(1)
           APHI(1) = KSQT(JP) * (-GA*AAV - GB*ABV)    ! cell corner PHI contribution
           APHI(2) = KSQT(JP) * (+GA*AAV - GB*ABV)    ! to along strike field
           APHI(3) = -APHI(2)
           APHI(4) = -APHI(1)
           BPHI(1) = KSQT(JP) * (-GA*BAV - GB*BBV)    ! cell corner PHI contribution
           BPHI(2) = KSQT(JP) * (+GA*BAV - GB*BBV)    ! to down dip field
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
                 SCAT_MTRX(KAB-1,IPHI(L)) = SCAT_MTRX(KAB-1,IPHI(L)) + CMPLX (AD(L), 0.) * KSQN
                 SCAT_MTRX(KAB,  IPHI(L)) = SCAT_MTRX(KAB,  IPHI(L)) + CMPLX (BD(L), 0.) * KSQN
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
   DEALLOCATE (SAA,SBA,SBB,VAA,VAB,VBA,VBB,XCEL1,YCEL1,ZCEL1)

 END DO PLATE_LOOP

! Set up the plate-plate interactions; ie. the off diagonal part of the
! scattering matrix.

 MULTI_SHT: IF (NPLT > 1) THEN
   ALLOCATE (VAAI(MXAB,MXAB),VABI(MXAB,MXAB),VBAI(MXAB,MXAB),VBBI(MXAB,MXAB), &
             SAAI(MXAB,MXAB),SABI(MXAB,MXAB),SBAI(MXAB,MXAB),SBBI(MXAB,MXAB))

   VAAI=ZERO; VABI=ZERO; VBAI=ZERO; VBBI=ZERO; SAAI=ZERO; SABI=ZERO; SBAI=ZERO; SBBI=ZERO

   TX_SHT: DO JPS = 1, NPLT-1
     RX_SHT: DO JPR = JPS+1, NPLT

       IF (JPS == NLYR .AND. JPR == NLYR) THEN

         CALL INTER_EGT_BOSS (JPS,JPR,MXAB,NPLT,NA,NB,DPTHB,KSQN,PLAZM,PLDIP,PLUNJ,XCELL,YCELL,ZCELL, &
                              NZ2,ZV2,NREGT,RHOTRP,GR1Z,GI1Z,GR2Z,GI2Z,GR3Z,GI3Z,GR4Z,GI4Z,GR5Z,GI5Z, &
                              GR6Z,GI6Z,VAAI,VABI,VBAI,VBBI,SAAI,SABI,SBAI,SBBI)
        ELSE
          CALL INTER_EGT_BOSS_UL (JPS,JPR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,NPLT,PLYR,MXAB,  &
                                  NA,NB,PLAZM,PLDIP,PLUNJ,XCELL,YCELL,ZCELL,NREGT,RHOTRP, &
                                  VAAI,VABI,VBAI,VBBI,SAAI,SABI,SBAI,SBBI)
        END IF

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
         AREA = DA(JSX) * DB(JSX)

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
                 AAS  = SAAI(JAB,KAB) * AREA(KAB,JSX)
                 ABSS = SABI(JAB,KAB) * AREA(KAB,JSX)
                 BAS  = SBAI(JAB,KAB) * AREA(KAB,JSX)
                 BBS  = SBBI(JAB,KAB) * AREA(KAB,JSX)
                 AAV  = VAAI(JAB,KAB) * AREA(KAB,JSX)
                 ABV  = VABI(JAB,KAB) * AREA(KAB,JSX)
                 BAV  = VBAI(JAB,KAB) * AREA(KAB,JSX)
                 BBV  = VBBI(JAB,KAB) * AREA(KAB,JSX)
               ELSE
                 AAS  = SAAI(KAB,JAB) * AREA(KAB,JSX)
                 ABSS = SBAI(KAB,JAB) * AREA(KAB,JSX)
                 BAS  = SABI(KAB,JAB) * AREA(KAB,JSX)
                 BBS  = SBBI(KAB,JAB) * AREA(KAB,JSX)
                 AAV  = VAAI(KAB,JAB) * AREA(KAB,JSX)
                 ABV  = VBAI(KAB,JAB) * AREA(KAB,JSX)
                 BAV  = VABI(KAB,JAB) * AREA(KAB,JSX)
                 BBV  = VBBI(KAB,JAB) * AREA(KAB,JSX)
               END IF

               APSI(1) = KSQT(JRX) * (- GB*AAS + GA*ABSS)   ! cell corner PSI contribution
               APSI(2) = KSQT(JRX) * (- GB*AAS - GA*ABSS)   ! to along strike field
               APSI(3) = -APSI(2)
               APSI(4) = -APSI(1)
               BPSI(1) = KSQT(JRX) * (-GB*BAS + GA*BBS)    ! cell corner PSI contribution
               BPSI(2) = KSQT(JRX) * (-GB*BAS - GA*BBS)    ! to down dip field
               BPSI(3) = -BPSI(2)
               BPSI(4) = -BPSI(1)
               APHI(1) = KSQT(JRX) * (-GA*AAV - GB*ABV)    ! cell corner PHI contribution
               APHI(2) = KSQT(JRX) * (+GA*AAV - GB*ABV)    ! to along strike field
               APHI(3) = -APHI(2)
               APHI(4) = -APHI(1)
               BPHI(1) = KSQT(JRX) * (-GA*BAV - GB*BBV)    ! cell corner PHI contribution
               BPHI(2) = KSQT(JRX) * (+GA*BAV - GB*BBV)    ! to down dip field
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
   DEALLOCATE (VAAI,VABI,VBAI,VBBI,SAAI,SABI,SBAI,SBBI)
 END IF MULTI_SHT

! Do LU decomposition and then compute scattering currents, J_SCAT
! for all transmitter positions

 CALL SCAT_MTRX_LU_DCMP (MXCL2,SCAT_MTRX,INDX,DCMP_FAIL)

 MXA = MAXVAL (NA);  MXB = MAXVAL (NB)
 CALL SCAT_CRNT (NPLT,NTX,MXAB,NCELL2,MXCL2,MXA,MXB,NA,NB,DA,DB, &
                 KSQN,SIGT,INDX,SCAT_MTRX,E_PRYM,J_SCAT)

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

 SUBROUTINE SCAT_CRNT (NPLT,NTX,MXAB,NCELL2,MXCL2,MXA,MXB,NA,NB,DA,DB, &
                       KSQN,SIGT,INDX,SCAT_MTRX,E_PRYM,J_SCAT)
!------------------------------------------------------------------------
!
!  Computes the scattering currents, J_SCAT for all
!  transmitter positions, by applying the LU decomposed matrix SCAT_MTRX
!  to the incident layered earth primary electric fields, E_PRYM.

!***  Called by SCAT_MTRX_BOSS

!  For E := either E_PRYM or J_SCAT -

!  E (1,JCELL,JS,JP) is the field (current) in the strike direction in the
!                       centre of plate cell number JCELL of plate JP due
!                       to transmitter JS
!  E (2,JCELL,JS,JP) is the field (current) in the down-dip direction in the
!                       centre of plate cell number JCELL of plate JP due
!                       to transmitter JS
!
!        NPLT - number of plates
!         NTX - number of transmitter positions on profile line.
!         MXA - number of cells along strike of longest plate
!         MXB - maximum number of cells down dip of widest plate
!        MXAB - the number of cells in the biggest plate (not necessarily MXA * MXB)
!   NCELL2(J) - 2 * the number of cells in plates 1 to J
!       MXCL2 - 2 * total number of cells for all plates
!     NA & NB - number of cells along strike and down dip for plate
!     DA & DB - cell dimensions along strike and down dip respectively
!     SIGT(J) - complex conductivity thickness product of plate J
!     KSQT(J) - iwu * SIGT
!        INDX - pivoting vector information from SCAT_MTRX_LU_DCMP

 IMPLICIT NONE
 COMPLEX, PARAMETER :: ZERO=(0.,0.)
 INTEGER JP,NPLT,JS,NTX,JA,JB,JAB,J,J1,J2,MXAB,MXA,MXB,NAL,NA(NPLT), &
         NBL,NB(NPLT),IBACK,I,MXCL2,INDX(MXCL2),NCELL2(0:NPLT),MCELL2
 REAL GA,GB,DA(NPLT),DB(NPLT)
 COMPLEX SCAT_MTRX(MXCL2,MXCL2),E_PRYM(2,MXAB,NTX,NPLT),J_SCAT(2,MXAB,NTX,NPLT), &
         X(MXCL2),B(MXCL2),PHI(MXB+1,MXA+1),PSI(MXB+1,MXA+1),KSQN, &
         SIGT(NPLT),ES_TMP,SUM

 INTENT (IN) NPLT,NTX,MXAB,NCELL2,MXCL2,MXA,MXB,NA,NB,DA,DB,KSQN, &
             SIGT,INDX,SCAT_MTRX,E_PRYM
 INTENT (OUT) J_SCAT

 TX_LOOP: DO JS = 1,NTX
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
         ES_TMP = KSQN * (PHI(JB,JA+1) + PHI(JB+1,JA+1) - PHI(JB,JA) - PHI(JB+1,JA)) *GA &
                       + (PSI(JB+1,JA) + PSI(JB+1,JA+1) - PSI(JB,JA) - PSI(JB,JA+1)) *GB

         J_SCAT(1,JAB,JS,JP) = SIGT(JP) * ES_TMP

         ES_TMP = KSQN * (PHI(JB+1,JA) + PHI(JB+1,JA+1) - PHI(JB,JA) - PHI(JB,JA+1)) *GB &
                       - (PSI(JB,JA+1) + PSI(JB+1,JA+1) - PSI(JB,JA) - PSI(JB+1,JA)) *GA
         J_SCAT(2,JAB,JS,JP) = SIGT(JP) * ES_TMP
       END DO
     END DO
   END DO PLATE_LOOP

 END DO TX_LOOP

 END SUBROUTINE SCAT_CRNT

 SUBROUTINE SCAT_EM (JF,NFRQ,NPLT,PLYR,NTPL,IDPL,MXAB,MXB,NA,NB,DA,DB,PLAZM,PLDIP,PLUNJ,XCELL,  &
                     YCELL,ZCELL,NMGP,XMG,YMG,ZMG,WMG,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,NTX,MRXTX, &
                     NRXTX,RXID,MXRS,NRS,XRS,YRS,ZRXTX,WTRS,EDCS,EDSN,NRMGT,RHOTRP,NZ1,ZV1,NZ3, &
                     ZC3,MDRX,EDRX,J_SCAT,BFD_SCAT,QUIT)
!---------------------------------------------------------------------------------------------
!
!***  Called by LEROI_3D
!***  Calls MGT_BOSS, EGTRX_BOSS
!
!  Computes BFD_SCAT(JF,JR,JS.JC), the scattered electric and magnetic fields
!  for frequency JF at receiver, JR of transmitter JS.
!
!  For magnetic dipole receivers, BFD_SCAT is B in Teslas for North, East &
!  Vertical components corresponding to JC = 1,2,3
!
!  For closed loop receivers, BFD_SCAT(JC=1) is B the vertical magnetic field in Teslas
!         BFD_SCAT(JC=2,3) is set to zero
!
!  For electric dipole receivers, BFD_SCAT(JC=1) is volts,; ie the E field
!  integrated along receiver dipole
!         BFD_SCAT(JC=2,3) is set to zero
!
!
!        Input variables
!        ---------------
!
!          JF - index of NFRQ frequencies
!        NPLT - Number of plates
!        NTPL - Number of plates above basement
!        IDPL - Indices for plates above basement
!        PLYR - layer containing plate
!        MXAB - Number of cells in biggest plate
!      NA, NB - number of cells along strike and down dip respectively
!       PLAZM - strike angles for each plate
!       PLDIP - dip anglefor each plate
!       PLUNJ - plunge angles
!        NLYR - number of layers including overburden.
!       DPTHB - depth to basement (= 0 if NLYR = 1)
!        RMUD - double precision relative magetic permeability
!        SIGL - double precision complex xconductivity for all layers including Cole-Cole & dielectric
!        KSQL - double precision iwu * SIGL for all layers (includes Cole-Cole)
!         NTX - number of transmitters
!       NRXTX - number of receivers per transmitter (max = MRXTX)
!        RXID - identifies type of receiver: 1 = mag dipole;  2 = electric dipole
!         NRS - number of subnet receivers per receiver (max = MXRS)
!    XRS, YRS - North, East coordinates of receiver subnet
!        WTRS - subnet receiver weights
!  EDCS, EDSN - cosine & sine of angle from north made by grounded wire receiver
!         MAG - magnetic field computation required if true
!     SURFACE - magnetic field computation required on surface if true
!       NRMGT - Dimension for MGT interpolations
!      RHOTRP - horizontal interpolation array for MGT & EGTRX
!    ZV1(NZ1) - vertical interpolatin array for basement
!       NCPTS - number of integration points per dimension per cell.
!       NRMGT - Dimension for receiver Green's function interpolations

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 COMPLEX, PARAMETER :: ZERO=(0.,0.)
 INTEGER JF,NFRQ,JP,NPLT,NTPL,SXLYR,MXAB,MXB,NAL,NBL,NLYR,NTX,MRXTX,NRXTX(NTX), &
         RXID(MRXTX,NTX),MXRS,NRS(MRXTX,NTX),NRMGT,NZ1,NZ3,NZ3L,NMGP,J1,JS,JR,JC,JCL
 INTEGER, DIMENSION (NPLT) :: NA,NB,IDPL,PLYR
 REAL RHOTRP(NRMGT),ZV1(NZ1),DAL,DBL,EDCS(MRXTX,NTX),EDSN(MRXTX,NTX),SCRATCH(MRXTX,NTX,3), &
      WTRS(MXRS,MRXTX,NTX),ZRXTX(MRXTX,NTX),ZC3(3*MXB,NTPL),ZC3L(3*MXB)
 REAL(KIND=QL) RMUD(0:NLYR),THKD(NLYR),DPTHL(NLYR)
 REAL, DIMENSION (NPLT) :: DA,DB,PLAZM,PLDIP,PLUNJ,WMG
 REAL, DIMENSION (MXAB,NPLT) :: XCELL,YCELL,ZCELL
 REAL, DIMENSION (MXRS,MRXTX,NTX) :: XRS,YRS
 REAL, DIMENSION (NMGP,NPLT) :: XMG,YMG,ZMG
 COMPLEX(KIND=QL), DIMENSION(NLYR) :: SIGL,KSQL
 COMPLEX HA(MXAB,3,MRXTX,NTX,NPLT),HB(MXAB,3,MRXTX,NTX,NPLT),J_SCAT(2,MXAB,NTX,NPLT),TMP(3), &
         BFD_SCAT(NFRQ,MRXTX,NTX,3)
 LOGICAL EDRX,MDRX,QUIT

!  Compute the magnetic Green's tensor integrals for each plate and combine
!  them with the scattering currents and sum.

 HA = ZERO;  HB = ZERO
 IF (MAXVAL (PLYR) == NLYR) THEN    ! basement plates

   IF (MDRX) CALL MGT_BOSS (NPLT,PLYR,MXAB,NA,NB,PLAZM,PLDIP,PLUNJ,XCELL,YCELL,ZCELL,NLYR,THKD, &
                            DPTHL,RMUD,SIGL,KSQL,NTX,MRXTX,NRXTX,RXID,MXRS,NRS,XRS,YRS,ZRXTX,   &
                            WTRS,NRMGT,RHOTRP,NZ1,ZV1,NMGP,XMG,YMG,ZMG,WMG,HA,HB)

   IF (EDRX) CALL EGTRX_BOSS (NPLT,PLYR,MXAB,NA,NB,PLAZM,PLDIP,PLUNJ,XCELL,YCELL,ZCELL,NLYR,THKD, &
                              DPTHL,RMUD,SIGL,KSQL,NTX,MRXTX,NRXTX,RXID,MXRS,NRS,XRS,YRS,ZRXTX,   &
                              WTRS,NRMGT,RHOTRP,NZ1,ZV1,NMGP,XMG,YMG,ZMG,WMG,HA,HB)
 END IF

 DO J1 = 1, NTPL          ! plates above basement
   JP = IDPL(J1)
   NAL = NA(JP)
   NBL = NB(JP)
   DAL = DA(JP)
   DBL = DB(JP)
   SXLYR = PLYR(JP)
   NZ3L = 3*NBL
   ZC3L(1:NZ3) = ZC3(1:NZ3,JP)

   IF (MDRX) &
     CALL MGT_BOSS_UL (JP,SXLYR,NPLT,MXAB,NAL,NBL,DAL,DBL,PLAZM,PLDIP,XCELL,YCELL,NLYR, &
                       THKD,DPTHL,RMUD,SIGL,KSQL,NTX,MRXTX,NRXTX,RXID,MXRS,NRS,XRS,YRS, &
                       ZRXTX,WTRS,NRMGT,RHOTRP,NZ3L,ZC3L,HA,HB)

   IF (EDRX) &
     CALL EGTRX_BOSS_UL (JP,SXLYR,NPLT,MXAB,NAL,NBL,DAL,DBL,PLAZM,PLDIP,XCELL,YCELL,NLYR, &
                         THKD,DPTHL,RMUD,SIGL,KSQL,NTX,MRXTX,NRXTX,RXID,MXRS,NRS,XRS,YRS, &
                         ZRXTX,WTRS,NRMGT,RHOTRP,NZ3L,ZC3L,HA,HB)
 END DO

 PLATE_LOOP: DO JP = 1,NPLT

   TX_LOOP: DO JS = 1,NTX

!  Multiply the magnetic Green's tensor elements times the strike and
!  downdip currents.  J1 = 1,2,3 represents the along strike, horizontal
!  cross strike and vertical components respectively.  This has to be rotated
!  so that J1 = 1,2,3 represent the East, North, and vertical component
!  respectively. Strike angle is defined as positive, clockwise from North.
!  Strike angle = dip azimuth - pi/2

     RX_LOOP: DO JR = 1,NRXTX(JS)
       TMP = ZERO
       DO JC = 1,3
         DO JCL = 1, NA(JP) * NB(JP)
           TMP(JC) = TMP(JC) + J_SCAT(1,JCL,JS,JP) * HA(JCL,JC,JR,JS,JP) &
                             + J_SCAT(2,JCL,JS,JP) * HB(JCL,JC,JR,JS,JP)
         END DO
       END DO

       IF (RXID(JR,JS) == 2) THEN
         TMP(1) = TMP(1) * EDCS(JR,JS) + TMP(2) * EDSN(JR,JS)
         TMP(2:3) = ZERO
       END IF
       BFD_SCAT(JF,JR,JS,1:3) = BFD_SCAT(JF,JR,JS,1:3) + TMP(1:3)
     END DO RX_LOOP
   END DO TX_LOOP
 END DO PLATE_LOOP

! For coincident loop receivers, put the response into component 1

 DO JS = 1,NTX
   DO JR = 1,NRXTX(JS)
     SCRATCH(JR,JS,1:3) = ABS (BFD_SCAT(JF,JR,JS,1:3))
     IF (RXID(JR,JS) == 4) THEN
       BFD_SCAT(JF,JR,JS,1) = BFD_SCAT(JF,JR,JS,3)
       BFD_SCAT(JF,JR,JS,2:3) = ZERO
     END IF
   END DO
 END DO
 IF (MAXVAL (SCRATCH) < 1.E-30) QUIT = .TRUE.

 END SUBROUTINE SCAT_EM

 SUBROUTINE SET_MAX_INTRP (MXRHO,RHOTRP,MXAB,NPLT,NA,NB,DA,PLNGTH,XCELL,YCELL,NTX,MXVRTX, &
                           NVRTX,SXN,SXE,MRXTX,NRXTX,MXRS,NRS,XRS,YRS,NRMGT,NRPRM,NREGT)
!---------------------------------------------------------------------------------------------

! Sets the size for the horizontal interpolation arrays in PRM_BOSS, EGT_BOSS and MGT_BOSS

!*** Called by LEROI_3D

!                 INPUT
!                 -----
!
!        RHOTRP - horizontal interpolation array of MXRHO points (15 points / decade)
!          MXAB - maximum number of cells per plate
!          NPLT - number of plates
!        NA, NB - the number of cells along strike and down dip respectively for each plate
!        DA, DB - cell dimensions along strike and down dip respectively for each plate
!        PLNGTH - array of plate strike lengths
!    XCELL(k,n) - X (north) coordinate of centre of cell k of plate n.
!    YCELL(k,n) - Y (east) coordinate of centre of cell k of plate n.
!           NTX - number of stations
!        MXVRTX - maximum number of vertices for any transmitter
!         NVRTX - number of vertices for any transmitter
!         MRXTX - maximum number of receivers per transmitter
!      SXN, SXE - arrays of transmitter (North, East) coordinates
!         MRXTX - number of receivers for each transmitter position
!         NRXTX - number of receivers for each transmitter
!     MXRS, NRS - maximum and actual number of subnet receivers per receiver
!           NRS - number of subnet receivers per receiver (max = MXRS)
!      XRS, YRS - (North, East) coordinates of receiver subnet

!      OUTPUT
!      ------
!
!    NREGT - maximum dimension of RHOTRP for electric Green's functions
!    NRMGT - maximum dimension of RHOTRP for magnetic Green's functions
!    NRPRM - maximum dimension of RHOTRP for primary field at targets

 INTEGER MXRHO,MXAB,JP,NPLT,NAB2,NA(NPLT),NB(NPLT),NAB,NTX,MRXTX,MXVRTX,NVRTX(NTX),NRXTX(NTX), &
         MXRS,NRS(MRXTX,NTX),JS,JR,JAB,JP2,JAB2,JV,J1,NRMGT,NRPRM,NREGT
 REAL RHOTRP(MXRHO),PRMX,EGTMX,EGTSQ,MGTMX,R1,XCELL(MXAB,NPLT),YCELL(MXAB,NPLT), &
      PLNGTH(NPLT),DA(NPLT),SXN(MXVRTX,NTX),SXE(MXVRTX,NTX),BIGRHO,CELLRAD
 REAL, DIMENSION (MXRS,MRXTX,NTX) :: XRS,YRS


 PRMX = 0.                ! initialise maximum source - cell separation
 EGTMX = MAXVAL (PLNGTH)  ! initialise maximum inter-plate cell separation
 EGTSQ = EGTMX**2
 MGTMX = EGTSQ
 CELLRAD = 0.75 * MAXVAL(DA)

 DO JP = 1, NPLT      !  Sum over plates
   NAB = NA(JP) * NB(JP)
   DO JAB = 1, NAB
     DO JS = 1,NTX
       DO JV = 1,NVRTX(JS)
         R1 = SQRT( (SXN(JV,JS) - XCELL(JAB,JP))**2 + (SXE(JV,JS) - YCELL(JAB,JP))**2)
         PRMX = MAX (R1, PRMX)
       END DO

       DO JR = 1, NRXTX(JS)
         DO J1 = 1, NRS(JR,JS)
           R1 = (XRS(J1,JR,JS) - XCELL(JAB,JP))**2 + (YRS(J1,JR,JS) - YCELL(JAB,JP))**2
           MGTMX = MAX (R1, MGTMX)
         END DO
       END DO
     END DO

     IF ( NPLT > 1 .AND. JP < NPLT ) THEN
       DO JP2 = JP+1,NPLT
         NAB2 = NA(JP2) * NB(JP2)
         DO JAB2 = 1, NAB2
           R1 = (XCELL(JAB,JP) - XCELL(JAB2,JP2))**2 + &
                (YCELL(JAB,JP) - YCELL(JAB2,JP2))**2
           EGTSQ = MAX (R1, EGTSQ)
         END DO
       END DO
     END IF
   END DO
 END DO
 MGTMX = SQRT (MGTMX) + CELLRAD
 EGTMX = SQRT (EGTSQ) + CELLRAD
 BIGRHO = MAX (PRMX,MGTMX,EGTMX)

 NRPRM = 1;  NRMGT = 1; NREGT = 1
 DO JR = 2, MXRHO
   IF (RHOTRP(JR) < MGTMX) NRMGT = JR + 1
   IF (RHOTRP(JR) < PRMX)  NRPRM = JR + 1
   IF (RHOTRP(JR) < EGTMX) NREGT = JR + 1
   IF (RHOTRP(JR) > BIGRHO) EXIT
 END DO

 END SUBROUTINE SET_MAX_INTRP

 SUBROUTINE SET_MGT (NPLT,NX,NMGP,DA,DB,PLAZM,PLDIP,PLUNJ,XMG,YMG,ZMG,WMG)
!------------------------------------------------------------------------

!  Sets up a 9 point / cell stencil, relative to the cell centre for integrating
!  the magnetic Green's tensor elements.  For each plate, the 9 points are
!  allocated to a flat cell and then rotated into the plane of the plate.  The
!  integration coordicates are obtained by adding the these to the cell centres.
!
!***  Called by LEROI_3D
!
!           NPLT - number of plates
!             NX - Number of points per dimension per cell for MGT integration
!           NMGP - Number of points per cell for MGT integration = NX^2
!         DA, DB -  cell dimensions along strike and down dip
!          PLAZM - strike angles
!          PLDIP - dip angle
!          PLUNJ - plunge angles
!      MXRS, NRS - maximum and actual number of subnet receivers per receiver
!
!                  OUTPUT
!                  ------
!            WMG - total integration weight for each point for plate JP
!  XMG, YMG, ZMG - coordinates for integration down dip for each plate

 INTEGER NPLT,NX,NMGP,JP,JA,JB,JAB
 REAL X0,Y0,XS,X2(NMGP),Y2(NMGP),R32(3,2),CA,SA,CD,SD,CP,SP,DELX,DELY
 REAL, DIMENSION (NMGP,NPLT) :: XMG,YMG,ZMG
 REAL, DIMENSION(NPLT) :: WMG,DA,DB,PLAZM,PLDIP,PLUNJ

 XMG = 0.;  YMG = 0.;  ZMG = 0.; WMG = 0.

! Start with 9 point uniform rule on a flat cell

 DO JP = 1,NPLT
   IF (DB(JP) < 0.01) CYCLE
   CA = COS (PLAZM(JP))
   SA = SIN (PLAZM(JP))
   CD = COS (PLDIP(JP))
   SD = SIN (PLDIP(JP))
   CP = COS (PLUNJ(JP))
   SP = SIN (PLUNJ(JP))
   WMG(JP) = DA(JP) * DB(JP) / REAL (NMGP)
   CALL RPLT2XYZ (CA,SA,CD,SD,CP,SP,R32)

   DELX = DA(JP) / REAL(NX)
   DELY = DB(JP) / REAL(NX)
   X0 = -(DA(JP) + DELX) / 2.
   Y0 = -(DB(JP) + DELY) / 2.

   DO JA = 1,NX
     XS = X0 + JA * DELX
     DO JB = 1,NX
       JAB = JA + (JB-1) * NX
       X2(JAB) = XS
       Y2(JAB) = Y0 + JB * DELY
       XMG(JAB,JP) = R32(1,1) * X2(JAB) + R32(1,2) * Y2(JAB)
       YMG(JAB,JP) = R32(2,1) * X2(JAB) + R32(2,2) * Y2(JAB)
       ZMG(JAB,JP) = R32(3,1) * X2(JAB) + R32(3,2) * Y2(JAB)
     END DO
   END DO
 END DO
 END SUBROUTINE SET_MGT

 SUBROUTINE SET_RX_SUBNET (NTX,MRXTX,NRXTX,MXRS,NRS,MQVR,RXID,XRXTX,YRXTX,XRS,YRS,WTRS,EDCS,EDSN)
!-----------------------------------------------------------------------------------------------

! Sets up virtual receivers for magnetic dipole, grounded wire and loop receivers.
! All receivers except magnetic dipoles must lie on the surface in this version.

!*** Called by LEROI_3D

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

!*** Called by LEROI_3D
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

 SUBROUTINE NLSQ2 (NPAR,NDATA,XDATA,RDATA,RWTS,PCTCNV,NDSTP,KPCT,CXPAR,ELAS,LBND,UBND,MAXITS,INVPRT, &
                   NW,np,TITLE,SURVEY_TYPE,SOURCE_TYPE,NLINES,MLINES,LINE,HEADER_ID,IPLT,YXZPLT,    &
                   MCMP,CMP,MRXL,MCHNL,NCHNL,NFRQ,NFRQHS,TDFD,STEP,FREQ,FRQHS,NSX,SWX,SWY,NPULS,     &
                   PULSE,NTYPLS,NTYRP,TRP,TOPN,TCLS,NFT,CURNT,ISYS,SVAZM,UNITS,RX_TYPE,RXMNT,IDH,    &
                   MXTX,NTX,SXN,SXE,SXZ,SXDIP,SXAZM,MXVRTX,NVRTX,NRX,LNTR,NRXTX,MRXTX,RXID,MQVR,     &
                   MXRS,XRXTX,YRXTX,ZRXTX,BPRM,MD1,MD2,RXAZM,RXDIP,KNORM2,NCTD,MXRHO,RHOTRP,NLYR,    &
                   THK,RES,RMUD,REPS,CHRG,CTAU,CFREQ,NCNTRD,ECNTRD,NPLT,SIG_T,PLNGTH,PLWDTH,XCNTR,   &
                   YCNTR,PLTOP,PLAZM,PLDIP,PLUNJ,CELLW,MXAB,CHRGP,CTAUP,CFREQP)
!--------------------------------------------------------------------------------------------------

!***  Called by: MAIN
!***      Calls: CNVRT_BOUNDS, CNVRT2_XPAR, ESVD, FORJAC, INDEX_MPAR,
!                NOISE_2_SIGR, SOLVE2, WRITE_INVMDL, WRITE_DATA,
!                WRITE_TD, WRITE_FD
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
!  NPAR        : number of parameters; = 2*NLYR -1 + NPLT * 9 for Leroi
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
!  np         = plot file unit number
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
!  SOURCE_TYPE = 1 : general loop
!              = 2 : grounded wire
!              = 3 : magnetic dipole
!              = 4 : coincident loop
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
!     NCNTRD    - north offset from real world to model coordinate
!     ECNTRD     - east offset from real world to model coordinate
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
 INTEGER NPAR,NDATA,MAXITS,ITS,ICNT,NW,np,INVPRT,CXPAR(NPAR),  &
         NSV,NLINES,MLINES,MRXL,MCMP,MCHNL,NCHNL,NFRQ,NFRQHS,NFT,TDFD,STEP,NSX,NPULS, &
         NTYPLS,NTYRP,ISYS,SURVEY_TYPE,SOURCE_TYPE,MXTX,NTX,MXVRTX,NVRTX(MXTX),MRXTX, &
         NRXTX(MXTX),RXID(MRXTX,NTX),MQVR,MXRS,LNTR(4,NLINES),MD1,MD2,                &
         MXRHO,KNORM2(MRXTX,NTX),JP,NCTD(MRXTX,NTX)
 INTEGER, DIMENSION(NLINES) :: RX_TYPE,UNITS,NRX,IDH,CMP,LINE,HEADER_ID,IPLT
 INTEGER RWTS(MCHNL,MRXL,MCMP,MLINES)
 REAL PCTCNV,A(NDATA,NPAR+1),UMAT(NDATA,NPAR),VMAT(NPAR,NPAR),WSP(3*NPAR),NSR,DELT,DELTOLD, &
      RSVT,ZERO,FNM,GTEST,SQPDRE,PDRE,SSQNEW,SUMSQ,RMSERR,ERR1P,B1,B2,B3,B4,BMID
 REAL, DIMENSION(NDATA) :: VERR,XMODL,XDATA
 REAL, DIMENSION(NPAR) :: SV,XPAR,MPAR,DELPAR,GXPAR,ELAS,LBND,UBND,XLBND,XUBND
 REAL, DIMENSION(MCHNL,MRXL,MCMP,NLINES) :: RMODL,RDATA
 REAL, DIMENSION(MD1,MD2) :: RXAZM,RXDIP
 REAL FREQ(NFRQ),FRQHS(NFRQHS),SWX(NSX),SWY(NSX,3),PULSE,TRP(NTYRP),TOPN(NCHNL),TCLS(NCHNL),   &
      CURNT(NFT),XRXTX(MRXTX,NTX,MQVR),YRXTX(MRXTX,NTX,MQVR),ZRXTX(MRXTX,NTX),SXN(MXVRTX,NTX), &
      SXE(MXVRTX,NTX),SXZ(MXTX),SXDIP(MXTX),SXAZM(MXTX),RHOTRP(MXRHO),BPRM(MRXTX,NTX),         &
      SVAZM(NLINES),RXMNT(NLINES),FNEW,FOLD
 REAL(KIND=QL) NCNTRD,ECNTRD,YXZPLT(3,MRXL,NLINES)
 LOGICAL,PARAMETER :: WITHU=.TRUE., WITHV=.TRUE.
 LOGICAL JCBN,FINAL,MOVE
 CHARACTER (LEN=120) TITLE

! Model Dependent Variables

 INTEGER NLYR,NPLT,MXAB,IPAR,NDSTP,KPCT(NDSTP)
 REAL CELLW,PFAC(NDSTP),PARFAC
 REAL(KIND=QL) RMUD(0:NLYR)
 REAL, DIMENSION(NLYR) :: RES,REPS,CHRG,CTAU,CFREQ,THK
 REAL, DIMENSION(NPLT) :: SIG_T,PLNGTH,PLWDTH,XCNTR,YCNTR,PLTOP,PLAZM,PLDIP,PLUNJ, &
                          CHRGP,CTAUP,CFREQP,XYNORM

!  Preset threshold parameters and index workspace, but return if problem
!  size in error.

 ZERO =BND * BND
 IF (ZERO < ETA) ZERO = ETA

 A = 0.      !  Initialise arrays
 VERR = 0.
 FINAL = .FALSE.

 ITS = 0
 RSVT = MAX (BND, RSVT0)   ! Initialise eigenvalue damping at 10 percent
 WRITE(NW,'(/T3,A)') TRIM( ADJUSTL (TITLE))
 WRITE(*,'(/T3,A)') TRIM( ADJUSTL (TITLE))
 WRITE(NW,1) MAXITS,KPCT(1);  WRITE(*,1) MAXITS,KPCT(1)

!----------------------------------------------------------------------
!  Start of main loop.  Call FORJAC to get error and Jacobian.
!  First, tranform physical model into transformed parameters.
!  Call ESVD to find the S.V.D. of the Jacobian.
!----------------------------------------------------------------------

 XYNORM(1:NPLT) = MAX (PLNGTH(1:NPLT), PLWDTH(1:NPLT))
 CALL CNVRT2_XPAR (NLYR,NPLT,NPAR,RES,THK,SIG_T,PLTOP,PLNGTH,PLWDTH, &
                   XCNTR,YCNTR,XYNORM,PLAZM,PLDIP,PLUNJ,XPAR)
 CALL CNVRT_BOUNDS (NPLT,NLYR,NPAR,LBND,UBND,XYNORM,CXPAR,XLBND,XUBND)

 IPAR = 1
 PFAC = 0.01 * KPCT

 ITER_LOOP: DO ITS = 1, MAXITS
   PARFAC = PFAC(IPAR)
   IF (ITS == 1) THEN
     WRITE(*,'(//T3,A)') 'Compute initial Sensitivity Matrix.'
   ELSE
     WRITE(*,'(//T3,A,I3)') 'Compute Sensitivity Matrix based on model following iteration',ITS-1
   END IF

   JCBN = .TRUE.
   CALL FORJAC (NDATA,NPAR,NLINES,MLINES,MCMP,MRXL,MCHNL,NCHNL,NFRQ,NFRQHS,TDFD,STEP,FREQ, &
                FRQHS,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,TOPN,TCLS,NFT,CURNT,ISYS,   &
                SVAZM,UNITS,RX_TYPE,RXMNT,SOURCE_TYPE,IDH,MXTX,NTX,MXVRTX,NVRTX,SXN,SXE,   &
                SXZ,SXDIP,SXAZM,NRX,LNTR,NRXTX,MRXTX,RXID,MQVR,MXRS,XRXTX,YRXTX,ZRXTX,MD1, &
                MD2,RXAZM,RXDIP,KNORM2,NCTD,MXRHO,RHOTRP,NLYR,RMUD,REPS,CHRG,CTAU,CFREQ,   &
                NPLT,XYNORM,CELLW,MXAB,CHRGP,CTAUP,CFREQP,PARFAC,XPAR,CXPAR,BPRM,RWTS,     &
                XDATA,RMODL,XMODL,SUMSQ,JCBN,A,VERR,NW)

   RMSERR = 100. * SQRT (SUMSQ / REAL (NDATA))
   FNM = 0.01 * SQRT (SUMSQ)
   FNM = MAX (FNM, ETA)
   CALL INDEX_MPAR
   IF (ITS == 1) THEN
     WRITE(np,28) RMSERR,RSVT
     WRITE(np,29) MPAR(1:NPAR)
   END IF
   WRITE(NW,4) ITS,RMSERR,RSVT
   WRITE(*,4) ITS,RMSERR,RSVT
   IF (INVPRT == 3) CALL WRITE_DATA

!  Load the error vector into the NPAR+1 column of A.  On return from ESVD,
!  this column will contain the transformed error vector; i.e.,
!  VERR = U * VERR

   A(1:NDATA,NPAR+1) = VERR(1:NDATA)
   WRITE(*,'(/T3,A)') 'Compute SVD'
   CALL ESVD (A,NDATA,NPAR,IP,WITHU,WITHV,SV,UMAT,VMAT,WSP,ETA,TOL,NW)

   IF (ABS (SV(1)) < 1.E-30) THEN
     WRITE(NW,99); WRITE(*,99)
     RETURN
   END IF

   VERR(1:NDATA) = A(1:NDATA,NPAR+1)

!  Solve for the correction vector, and test for convergence on
!  predicted decrease.  Loop over internal iterations.

   WRITE(*,'(T3,A/)') 'Solve for new parameters & test convergence.'
   ICNT_LOOP: DO ICNT = 0, MAXITS_INT
     CALL SOLVE2 (NPAR,RSVT,ZERO,VMAT,VERR,SV,NSV,DELPAR,WSP,SQPDRE)
     PDRE = SQRT (SQPDRE)
     IF (PDRE < FNM) THEN

!  Change KPCT or terminate if predicted residual decrease < 1 percent of RMS error

       IF (IPAR < NDSTP) THEN
         WRITE(NW,17) KPCT(IPAR); WRITE(*,17) KPCT(IPAR)
         IPAR = IPAR + 1
         WRITE(NW,21) ;  WRITE(*,21)
         IF (INVPRT > 2) THEN
           WRITE(NW,3) ITS,ICNT,RSVT,RMSERR
           WRITE(*,3) ITS,ICNT,RSVT,RMSERR
         END IF
         WRITE(NW,16) KPCT(IPAR) ; WRITE(*,16) KPCT(IPAR)
         RSVT = MIN (RSVT, 0.1)
         CYCLE ITER_LOOP
       ELSE
         WRITE(NW,21) ;  WRITE(*,21)
         WRITE(NW,15) ;  WRITE(*,15)
         IF (INVPRT > 2)  THEN
           WRITE(NW,3) ITS,ICNT,RSVT,RMSERR
           WRITE(*,3) ITS,ICNT,RSVT,RMSERR
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
         B4 = XPAR(JP) + ELAS(JP) * (B2 - XPAR(JP))
         B3 = XPAR(JP) + ELAS(JP) * (B1 - XPAR(JP))
         GXPAR(JP) = XPAR(JP) + DELPAR(JP)
         GXPAR(JP) = MIN (GXPAR(JP), B4)
         GXPAR(JP) = MAX (GXPAR(JP), B3)
       END SELECT
     END DO

!  Get the error for model with corrected parameters.  Test for improvement (decrease) in
!  residual on ratio of actual to computed decrease in error.  If it fails, reduce step
!  and try again.  Give up and return after MAXITS_INT "internal" iterations.

     JCBN = .FALSE.
     CALL FORJAC (NDATA,NPAR,NLINES,MLINES,MCMP,MRXL,MCHNL,NCHNL,NFRQ,NFRQHS,TDFD,STEP,FREQ, &
                  FRQHS,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,TOPN,TCLS,NFT,CURNT,ISYS,   &
                  SVAZM,UNITS,RX_TYPE,RXMNT,SOURCE_TYPE,IDH,MXTX,NTX,MXVRTX,NVRTX,SXN,SXE,   &
                  SXZ,SXDIP,SXAZM,NRX,LNTR,NRXTX,MRXTX,RXID,MQVR,MXRS,XRXTX,YRXTX,ZRXTX,MD1, &
                  MD2,RXAZM,RXDIP,KNORM2,NCTD,MXRHO,RHOTRP,NLYR,RMUD,REPS,CHRG,CTAU,CFREQ,   &
                  NPLT,XYNORM,CELLW,MXAB,CHRGP,CTAUP,CFREQP,PARFAC,GXPAR,CXPAR,BPRM,RWTS,    &
                  XDATA,RMODL,XMODL,SSQNEW,JCBN,A,VERR,NW)

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
           WRITE(*,22) ICNT, RSVT
         END IF
         IPAR = IPAR + 1
         WRITE(NW,16) KPCT(IPAR) ; WRITE(*,16) KPCT(IPAR)
         RSVT = MIN (RSVT, 0.1)
         CYCLE ITER_LOOP
       ELSE
         WRITE(NW,22) ICNT, RSVT ; WRITE(*,22) ICNT, RSVT
         WRITE(NW,15) ;  WRITE(*,15)
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
       WRITE(NW,14);  WRITE(*,14)
     ELSE IF (DELT < 0.01* FOLD) THEN
       WRITE(NW,23); WRITE(*,23)
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
       WRITE(NW,16) KPCT(IPAR) ; WRITE(*,16) KPCT(IPAR)
       RSVT = MIN (RSVT, 0.1)
       CYCLE ITER_LOOP
     ELSE
       WRITE(NW,6) ITS,RMSERR,RSVT; WRITE(*,6) ITS,RMSERR,RSVT
       WRITE(NW,15) ; WRITE(NW,15)
!*******************************************
       EXIT ITER_LOOP
!*******************************************
     END IF
   END IF
   IF (RMSERR < PCTCNV) THEN
     WRITE(NW,20) PCTCNV
     WRITE(NW,6) ITS,RMSERR,RSVT; WRITE(*,6) ITS,RMSERR,RSVT
!*******************************************
     EXIT ITER_LOOP
!*******************************************
   END IF
   IF (ITS == MAXITS) THEN
     WRITE(NW,24)
     WRITE(NW,6) ITS,RMSERR,RSVT; WRITE(*,6) ITS,RMSERR,RSVT
!*******************************************
     EXIT ITER_LOOP
!*******************************************
   END IF

   WRITE(np,30) ITS,RMSERR,RSVT
   WRITE(*,6) ITS,RMSERR,RSVT ; WRITE(NW,6) ITS,RMSERR,RSVT
   CALL INDEX_MPAR
   WRITE(np,31) ITS,MPAR(1:NPAR)
   CALL WRITE_INVMDL (NW,FINAL,ITS,NLYR,NPLT,NPAR,XPAR,XYNORM,NCNTRD,ECNTRD)
   IF (INVPRT == 3) CALL WRITE_DATA
   CLOSE (np)                                                            ! Flush Buffer
   OPEN(np,FILE = 'Leroi.mv1',STATUS = 'OLD', POSITION = 'APPEND')
 END DO ITER_LOOP   !  END OF MAIN LOOP.  Write final model and exit.
 IF (RMSERR > 15.) WRITE(NW,7)

 CALL NOISE_2_SIGR (NPAR,NDATA,XMODL,XDATA,NSR)

 WRITE(NW,12)
 FINAL = .TRUE.
 WRITE(NW,13) RMSERR,NSR
 CALL WRITE_INVMDL (NW,FINAL,ITS,NLYR,NPLT,NPAR,XPAR,XYNORM,NCNTRD,ECNTRD)
 WRITE(np,32) ITS,RMSERR,NSR,RSVT
 CALL INDEX_MPAR
 WRITE(np,33) MPAR(1:NPAR)
 IF (INVPRT > 1) CALL WRITE_DATA

 1 FORMAT(/T3,'Begin Inversion.  Maximum iterations =',I3,3X,'Derivative step =',I3,' percent.')
 2 FORMAT(/T11,'ICNT:',I3,3X,'RSVT:',F8.3,3X,'Test RMS error =',F9.2,' percent.')
 3 FORMAT(/T3,'ITS:',I3,3X,'ICNT:',I3,3X,'RSVT',F8.3,3X,'RMSERR =',F9.2)
 4 FORMAT(/T3,'Begin iteration',I3,' with RMS error =',F8.2,' percent.  RSVT =',F8.3)
 6 FORMAT(/I4,' Iterations completed.  RMS error =',F8.2,' percent.  RSVT =',F8.3)
 7 FORMAT(/T3,'An alternative starting guess might achieve better results.')
 12 FORMAT(//T3,50('='))
 13 FORMAT(/T23,'RMS error =',F8.2,' percent.' &
           /T22,'Noise to signal ratio =',F8.3)
 14 FORMAT(/T3,'Error reduction after last 2 iterations < 0.5 percent.')
 15 FORMAT(T3,'Inversion terminated')
 16 FORMAT(T3,'Test derivative step =',I3,' percent.')
 17 FORMAT(T3,'No further error ruduction can occur using a',I3,' percent derivative step.')
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
 32 FORMAT(T1,'/'/T1,'/ FINAL_ITERATION  ',I2.2/T1,'/ PERCENT_RMS_ERROR:',F8.2,3X,'NSR:',F7.3,3X,'RSVT:',F7.3)
 33 FORMAT(/T1,'/ FINAL_MODEL',84F13.2)

 99 FORMAT(//T3,'Singular value decomposition failure.  INVERSION HALTED')

 CONTAINS

   SUBROUTINE INDEX_MPAR
!  ---------------------

!***  Called by NLSQ2

!  Converts XPAR to USER parameters rather then MODEL parameters as in CNVRT2_MPAR
!  PLTOP is negative downwards and plate north, east location is as specified.

 INTEGER KP,J2
 DO J2 = 1,NPLT
   KP = 9* (J2-1)
   MPAR(KP+1) = EXP (XPAR(KP+1))             ! SIG_T(J2)
   MPAR(KP+2) = EXP (XPAR(KP+2))             ! PLNGTH(J2)
   MPAR(KP+3) = EXP (XPAR(KP+3))             ! PLWDTH(J2)
   MPAR(KP+4) = -EXP (XPAR(KP+4))            ! PLTOP(J2) in RL
   MPAR(KP+5) = XYNORM(J2) * XPAR(KP+5) + REAL (ECNTRD)   ! YCNTR(J2)
   MPAR(KP+6) = XYNORM(J2) * XPAR(KP+6) + REAL (NCNTRD)   ! XCNTR(J2)
   MPAR(KP+7) = 90. + 180. * XPAR(KP+7)               ! AZM(J2) in degrees = PLAZM + 90
   MPAR(KP+8) = 180. * XPAR(KP+8)                     ! DIP(J2) in degrees
   MPAR(KP+9) = 180. * XPAR(KP+9)                     ! PLUNJ(J2) in degrees
 END DO
 KP = 9* NPLT
 DO J2 = KP+1, NPAR                   ! Layer resistivities and thicknesses
   MPAR(J2) = EXP (XPAR(J2))
 END DO

   END SUBROUTINE INDEX_MPAR

   SUBROUTINE WRITE_DATA
!  ---------------------

   IMPLICIT NONE
   INTEGER, PARAMETER :: PRFL=1, KPRT=1
   REAL TMS(NCHNL)

   IF (TDFD == 2) THEN
     CALL WRITE_FD (NW,np,KPRT,NFRQ,MCHNL,NLINES,MRXL,MCMP,NRX,SURVEY_TYPE,LINE,IDH,RX_TYPE,UNITS, &
                    SVAZM,TITLE,ISYS,PRFL,IPLT,YXZPLT,FREQ,HEADER_ID,CMP,RMODL,RDATA,RWTS)
   ELSE IF (TDFD < 2) THEN
     TMS = 0.5 * (TOPN + TCLS)
     CALL WRITE_TD (NW,np,KPRT,NCHNL,NLINES,MRXL,MCMP,NRX,SURVEY_TYPE,LINE,IDH,RX_TYPE,UNITS, &
                    SVAZM,TITLE,ISYS,PRFL,IPLT,YXZPLT,TMS,HEADER_ID,CMP,RMODL,RDATA,RWTS)
   END IF
   END SUBROUTINE WRITE_DATA

END SUBROUTINE NLSQ2

 SUBROUTINE FORJAC (NDATA,NPAR,NLINES,MLINES,MCMP,MRXL,MCHNL,NCHNL,NFRQ,NFRQHS,TDFD,STEP,FREQ, &
                    FRQHS,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,TOPN,TCLS,NFT,CURNT,ISYS,   &
                    SVAZM,UNITS,RX_TYPE,RXMNT,SOURCE_TYPE,IDH,MXTX,NTX,MXVRTX,NVRTX,SXN,SXE,   &
                    SXZ,SXDIP,SXAZM,NRX,LNTR,NRXTX,MRXTX,RXID,MQVR,MXRS,XRXTX,YRXTX,ZRXTX,MD1, &
                    MD2,RXAZM,RXDIP,KNORM2,NCTD,MXRHO,RHOTRP,NLYR,RMUD,REPS,CHRG,CTAU,CFREQ,   &
                    NPLT,XYNORM,CELLW,MXAB,CHRGP,CTAUP,CFREQP,PARFAC,XPAR,CXPAR,BPRM,RWTS,     &
                    XDATA,RMODL,XMODL,SUMSQ,JCBN,A,VERR,NW)
!-------------------------------------------------------------------------------------------

!  Sets up and implements computation.
!  It also calculates the Jacobian and error vector if required
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
!       CXPAR = 0 => parameter is completely free to vary as dictated by inversion step
!             = 1 => parameter is fixed
!             = 2 => parameter is constrained by elasticity.
!             = 3 => parameter bounds are buffered.
!
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
!
!        NLINES : number of lines of data
!          MCMP = 1 for Sampo and coincident loop, 3 otherwise
!          MRXL : maximum number for receivers per line
!         SVAZM : survey azimuth for each line
!         UNITS : units for each line
!       RX_TYPE : receiver type for each line(1, 2 or 3)
!   SOURCE_TYPE : closed loop, open loop (grounded ends) or magnetic dipole
!           IDH : identifies standard or Utem downhole processing where reauired
!           NTX : number of distinct transmitter positions (max = MXTX)
!         NVRTX : number of vertices for source loop (maximum = MXVRTX)
!      SXN, SXE : north and east coordinates of loop vertices or magnetic dipole Tx
!           SXZ : source depth
!  SXDIP, SXAZM : dipole Tx dip and azimuth
!           NRX : number of receivers for each line
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
!        NPLT : Number of plates
!      XYNORM : maximum plate dimension for normalisation of plate X-Y location.
!       CELLW : maximum cell dimension
!        MXAB : Number of cells in biggest plate
!        CHRGP, CTAUP & CFREQP are the plate Cole-Cole parameters.
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
!      9*NPLT + 1:NLYR           : layer resistivities
!      9*NPLT + NLYR + 1:NLYR-1  : layer thicknesses
!
!
!          TRANSFORMED PARAMETERS FOR INVERSION
!          ------------------------------------
!
!   SIG_T, PLTOP, PLWDTH, PLNGTH, RES & THK are represented logarithmically
!   XCNTR * YCNTR are normalised to maximum plate dimension.
!   PLAZM, PLDIP & PLUNJ are normalised to PI


 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 REAL, PARAMETER :: PI=3.141592654
 INTEGER NDATA,NPAR,NLINES,MCMP,MRXL,MCHNL,NCHNL,NFRQ,NFRQHS,NFT,TDFD,STEP,NSX,NPULS,NTYPLS, &
         NTYRP,ISYS,SOURCE_TYPE,MXTX,NTX,MXVRTX,NVRTX(MXTX),NRXTX(MXTX),MRXTX,NCTD(MRXTX,NTX),   &
         RXID(MRXTX,NTX),MQVR,MXRS,LNTR(4,NLINES),MD1,MD2,CXPAR(NPAR),  &
         KNORM2(MRXTX,NTX),MXRHO,MLINES,JC,JR,JD,JP,JL,LP,JF,K0,NW
 INTEGER, DIMENSION(NLINES) :: RX_TYPE,UNITS,NRX,IDH
 INTEGER RWTS(MCHNL,MRXL,MCMP,MLINES)
 REAL FREQ(NFRQ),FRQHS(NFRQHS),SWX(NSX),SWY(NSX,3),PULSE,TRP(NTYRP),TOPN(NCHNL),TCLS(NCHNL),   &
      CURNT(NFT),XRXTX(MRXTX,NTX,MQVR),YRXTX(MRXTX,NTX,MQVR),ZRXTX(MRXTX,NTX),SXN(MXVRTX,NTX), &
      SXE(MXVRTX,NTX),SXZ(MXTX),SXDIP(MXTX),SXAZM(MXTX),RHOTRP(MXRHO),XPAR(NPAR),SUMSQ,VM,VJ,  &
      VD,A(NDATA,NPAR+1),BTD(NCHNL,MRXTX,NTX,3),BTD_SCAT(NCHNL,MRXTX,NTX,3),BPRM(MRXTX,NTX),   &
      SVAZM(NLINES),RXMNT(NLINES),DENOM,V1,DELTA,DELXY,DELZ,XP0,PARFAC,DELPHI
 REAL, DIMENSION(MCHNL,MRXL,MCMP,MLINES) :: BFTL,RMODL
 REAL, DIMENSION(MD1,MD2) :: RXAZM,RXDIP
 REAL, DIMENSION(NDATA) :: XMODL,XMODL0,XDATA,VERR
 COMPLEX, DIMENSION(NFRQ,MRXTX,NTX,3) :: BFD, BFD_SCAT
 LOGICAL JCBN

! Model dependent variables

 INTEGER NLYR,NPLT,MXAB
 REAL CELLW
 REAL, DIMENSION(NLYR) :: THK,RES,REPS,CHRG,CTAU,CFREQ
 REAL(KIND=QL) RMUD(0:NLYR)
 REAL, DIMENSION(NPLT) :: SIG_T,PLNGTH,PLWDTH,XCNTR,YCNTR,PLTOP,PLAZM,PLDIP,PLUNJ, &
                          CHRGP,CTAUP,CFREQP,XYNORM

! Compute initial model & compute error

 CALL CNVRT2_MPAR (NLYR,NPLT,NPAR,RES,THK,SIG_T,PLTOP,PLNGTH,PLWDTH, &
                   XCNTR,YCNTR,XYNORM,PLAZM,PLDIP,PLUNJ,XPAR)
 DELPHI = PARFAC * PI

 LP = 0;
 CALL GET_FWD_MODL(LP,PARFAC)
 XMODL0 = XMODL
 RMODL = BFTL

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
   DO JP = 1,NPLT
     DELXY = PARFAC * XYNORM(JP)
     WRITE(*,'(/T3,A,I3/)') 'Compute sensitivities for plate',JP
     K0 = 9*(JP-1)
     LP = K0 + 1
     IF (CXPAR(LP) /= 1) THEN
       WRITE(*,'(T3,A)') 'Vary plate conductance'
       XP0 = SIG_T(JP)
       SIG_T(JP) = (1. + PARFAC) * SIG_T(JP)
       CALL GET_FWD_MODL(LP,PARFAC)
       SIG_T = XP0
     END IF
     LP = K0 + 2
     IF (CXPAR(LP) /= 1) THEN
       WRITE(*,'(T3,A)') 'Vary plate length'
       XP0 = PLNGTH(JP)
       PLNGTH(JP) = (1. + PARFAC) * PLNGTH(JP)
       CALL GET_FWD_MODL(LP,PARFAC)
       PLNGTH(JP) = XP0
     END IF
     LP = K0 + 3
     IF (CXPAR(LP) /= 1) THEN
       WRITE(*,'(T3,A)') 'Vary dip width'
       XP0 = PLWDTH(JP)
       PLWDTH(JP) = (1. + PARFAC) * PLWDTH(JP)
       CALL GET_FWD_MODL(LP,PARFAC)
       PLWDTH(JP) = XP0
     END IF
     LP = K0 + 4
     IF (CXPAR(LP) /= 1) THEN
       WRITE(*,'(T3,A)') 'Vary plate depth'
       XP0 = PLTOP(JP)
       DELZ = PARFAC * PLTOP(JP)
       PLTOP(JP) = PLTOP(JP) + DELZ
       CALL GET_FWD_MODL(LP,PARFAC)
       PLTOP(JP) = XP0
     END IF
     LP = K0 + 5
     IF (CXPAR(LP) /= 1) THEN
       WRITE(*,'(T3,A)') 'Vary PRP east coordinate'
       XP0 = YCNTR(JP)
       YCNTR(JP) = YCNTR(JP) + DELXY
       CALL GET_FWD_MODL(LP,PARFAC)
       YCNTR(JP) = XP0
     END IF
     LP = K0 + 6
     IF (CXPAR(LP) /= 1) THEN
       WRITE(*,'(T3,A)') 'Vary PRP north coordinate'
       XP0 = XCNTR(JP)
       XCNTR(JP) = XCNTR(JP) + DELXY
       CALL GET_FWD_MODL(LP,PARFAC)
       XCNTR(JP) = XP0
     END IF
     LP = K0 + 7
     IF (CXPAR(LP) /= 1) THEN
       WRITE(*,'(T3,A)') 'Vary dip azimuth'
       XP0 = PLAZM(JP)
       PLAZM(JP) = PLAZM(JP) + DELPHI
       CALL GET_FWD_MODL(LP,PARFAC)
       PLAZM(JP) = XP0
     END IF
     LP = K0 + 8
     IF (CXPAR(LP) /= 1) THEN
       WRITE(*,'(T3,A)') 'Vary dip'
       XP0 = PLDIP(JP)
       PLDIP(JP) = PLDIP(JP) + DELPHI
       CALL GET_FWD_MODL(LP,PARFAC)
       PLDIP(JP) = XP0
     END IF
     LP = K0 + 9
     IF (CXPAR(LP) /= 1) THEN
       WRITE(*,'(T3,A)') 'Vary plunge'
       XP0 = PLUNJ(JP)
       PLUNJ(JP) = PLUNJ(JP) + DELPHI
       CALL GET_FWD_MODL(LP,PARFAC)
       PLUNJ(JP) = XP0
     END IF
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

 INTEGER LP,J1,IPR
 REAL PARFAC
 REAL(KIND=QL) THKD(NLYR)
 LOGICAL INVERT,INTRUDE
 DATA INVERT /.TRUE./

 THKD = REAL (THK,KIND=QL)
 INTRUDE = .FALSE.

 IPR = 0
 CALL LEROI_3D (IPR,NFRQ,FREQ,SOURCE_TYPE,NTX,MXVRTX,NVRTX,SXN,SXE,SXZ,SXDIP,SXAZM,NRXTX, &
                MRXTX,RXID,MQVR,MXRS,XRXTX,YRXTX,ZRXTX,NLYR,THKD,RES,RMUD,REPS,CHRG,CTAU, &
                CFREQ,NPLT,MXAB,CELLW,PLNGTH,PLWDTH,XCNTR,YCNTR,PLTOP,PLAZM,PLDIP,PLUNJ,  &
                INTRUDE,SIG_T,CHRGP,CTAUP,CFREQP,MXRHO,RHOTRP,INVERT,BFD_SCAT)

       !  Construct frequency-domain output

 IF (TDFD == 2) THEN  !  Construct the frequency-domain response

   CALL HSBOSS (NFRQ,FREQ,SOURCE_TYPE,NTX,MXVRTX,NVRTX,SXN,SXE,SXZ,SXDIP,SXAZM, &
                NRXTX,MRXTX,RXID,MQVR,XRXTX,YRXTX,ZRXTX,MXRHO,RHOTRP,NLYR,THKD, &
                RES,RMUD,REPS,CHRG,CTAU,CFREQ,BFD)

!  Redefine BFD as the total response.
!  Reconfigure output from Tx to Line basis.

   BFD = BFD + BFD_SCAT  !  Redefine BFD as the total response.
   CALL SET_OUTPUT_LINES_FD (NFRQ,MCHNL,NTX,MRXTX,NRXTX,NLINES,MRXL,MCMP,NRX,LNTR,KNORM2,RX_TYPE,RXMNT, &
                             UNITS,ISYS,IDH,SVAZM,MD1,MD2,RXAZM,RXDIP,CURNT,BPRM,BFD,BFTL)

 ELSE IF (TDFD < 2) THEN   ! Time-Domain.  Compute BTD_SCAT, the Scattering Response

   CALL TDEM_3D (STEP,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL,TOPN,TCLS, &
                 FREQ,NFRQ,NTX,MRXTX,NRXTX,RXID,NCTD,BFD_SCAT,BTD_SCAT)

!  Compute BTD, the layered earth response convolved with the excitation waveform

   CALL HSBOSS_TD (NFRQHS,FRQHS,STEP,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL, &
                   TOPN,TCLS,SOURCE_TYPE,NTX,MXVRTX,NVRTX,SXN,SXE,SXZ,SXDIP,SXAZM,   &
                   NRXTX,RXID,MRXTX,MQVR,XRXTX,YRXTX,ZRXTX,MXRHO,RHOTRP,NLYR,THKD,   &
                   RES,RMUD,REPS,CHRG,CTAU,CFREQ,NCTD,BTD)

!  Redefine BTD as the total response.
!  Reconfigure output from Tx to Line basis.

   BTD = BTD + BTD_SCAT
   CALL SET_OUTPUT_LINES_TD (NCHNL,NTX,MRXTX,NRXTX,NLINES,MRXL,MCMP,NRX,LNTR,KNORM2,RX_TYPE,RXMNT, &
                             UNITS,ISYS,IDH,SVAZM,MD1,MD2,RXAZM,RXDIP,CURNT,BPRM,BTD,BFTL)
 END IF

 JD = 0
 DO J1 = 1,MLINES
   DO JC = 1,MCMP
     DO JR = 1,NRX(J1)
       DO JF = 1,MCHNL
         IF (RWTS(JF,JR,JC,J1) > 0) THEN
           JD = JD + 1
           XMODL(JD) = BFTL(JF,JR,JC,J1)
         END IF
       END DO
     END DO
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

 SUBROUTINE CNVRT2_MPAR (NLYR,NPLT,NPAR,RES,THK,SIG_T,PLTOP,PLNGTH,PLWDTH, &
                         XCNTR,YCNTR,XYNORM,PLAZM,PLDIP,PLUNJ,XPAR)
!-------------------------------------------------------------------------

!*** Called by: NLSQ2
!***     Calls: FIX_AZM

!  Converts XPAR to MODEL parameters rather then USER parameters as in INDEX_MPAR
!  PLTOP is positive downwards
!  and plate north, east location may have been shifted by NCNTRD,ECNTRD
!
!      NLYR : number of layers
!      NPLT : number of thin plates
!      NPAR : 2*NLYR-1 + 9*NPLT
!       RES : layer resistivities
!       THK : layer thicknesses
!     SIG_T : plate conductances
!    PLNGTH : plate strike lengths
!    PLWDTH : plate widths along dip
!     PLTOP : depth to reference point on top edge of plate
!     YCNTR :  east coordinates of plate reference points
!     XCNTR : north coordinates of plate reference points
!     PLAZM : plate azimuths in radians (0 = north)
!     PLDIP : plate dip angle in radians
!      XPAR : transformed parameters


! The parameters are ordered in groups of 9 corresponding to the parameters
! for each plate - followed by host resistivity, overburden resistivity &
! thickness.
!
!     Define KP = 9* (JP-1) where JP = plate index.
!
!  Then KPAR, the parameter index is DEFINED / ordered as
!
!     KP + 1 - SIG_T  = conductance of plate JP
!        + 2 - PLNGTH = strike length of plate JP
!        + 3 - PLWDTH = dip width of plate JP
!        + 4 - PLTOP  = depth of reference point of plate JP
!        + 5 - YCNTR  = east coordinate of reference point of plate JP
!        + 6 - XCNTR  = north coordinate of reference point of plate JP
!        + 7 - PLAZM  = dip azimuth of plate JP
!        + 8 - PLDIP  = dip angle of plate JP
!        + 9 - PLUNJ  = rotation angle of plate JP
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
!   XCNTR * YCNTR are normalised to maximum plate dimension.
!   PLAZM, PLDIP & PLUNJ are normalised to PI

 REAL, PARAMETER :: PI=3.141592654
 INTEGER  NLYR,NPLT,NPAR,KP,KP1,JP
 REAL, DIMENSION(NPLT) :: SIG_T,XCNTR,YCNTR,PLTOP,PLNGTH,PLWDTH,PLAZM,PLDIP,PLUNJ
 REAL XPAR(NPAR),RES(NLYR),THK(NLYR),XYNORM(NPLT)

 KP = 9*NPLT
 KP1 = KP + NLYR
 RES(1:NLYR) = EXP (XPAR(KP+1:KP1))
 DO JP = 1,NLYR-1
   THK(JP) = EXP (XPAR(KP1+JP))
 END DO
 THK(NLYR) = 1.E5

  DO JP = 1,NPLT
   KP = 9*(JP-1)
   SIG_T(JP)  = EXP (XPAR(KP+1))
   PLWDTH(JP) = EXP (XPAR(KP+2))
   PLNGTH(JP) = EXP (XPAR(KP+3))
   PLTOP(JP)  = EXP (XPAR(KP+4))
   YCNTR(JP) = XYNORM(JP) * XPAR(KP+5)
   XCNTR(JP) = XYNORM(JP) * XPAR(KP+6)
   PLAZM(JP) = PI * XPAR(KP+7)
   PLDIP(JP) = PI * XPAR(KP+8)
   PLUNJ(JP) = PI * XPAR(KP+9)
 END DO

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
!     YCNTR = east coordinates of plate reference points
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
!        + 2 - PLNGTH = strike length of plate JP
!        + 3 - PLWDTH = plate width of plate JP
!        + 4 - PLTOP = depth of reference point of plate JP
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
!   XCNTR * YCNTR are normalised to maximum plate dimension.
!   PLAZM, PLDIP & PLUNJ are normalised to PI

 REAL, PARAMETER :: PI=3.141592654
 INTEGER NLYR,NPLT,NPAR,KP,KP1,JP
 REAL RES(NLYR),THK(NLYR),XYNORM(NPLT),XPAR(NPAR)
 REAL,DIMENSION(NPLT) :: SIG_T,XCNTR,YCNTR,PLTOP,PLNGTH,PLWDTH,PLAZM,PLDIP,PLUNJ

 KP = 9*NPLT
 KP1 = KP + NLYR
 XPAR(KP+1:KP1) = LOG (RES(1:NLYR))
 DO JP = 1,NLYR-1
   XPAR(KP1+JP) = LOG (THK(JP))
 END DO

 DO JP = 1,NPLT
   KP = 9*(JP-1)
   XPAR(KP+1) = LOG (SIG_T(JP))
   XPAR(KP+2) = LOG (PLNGTH(JP))
   XPAR(KP+3) = LOG (PLWDTH(JP))
   XPAR(KP+4) = LOG (PLTOP(JP))
   XPAR(KP+5) = YCNTR(JP) / XYNORM(JP)
   XPAR(KP+6) = XCNTR(JP) / XYNORM(JP)
   XPAR(KP+7) = PLAZM(JP) / PI
   XPAR(KP+8) = PLDIP(JP) / PI
   XPAR(KP+9) = PLUNJ(JP) / PI
 END DO

 END SUBROUTINE CNVRT2_XPAR

 SUBROUTINE CNVRT_BOUNDS (NPLT,NLYR,NPAR,LBND,UBND,XYNORM,CXPAR,XLBND,XUBND)
!---------------------------------------------------------------------------

!*** Called by NLSQ2

! Converts the LBND & UBND specified by the user into the form used
! by the inversion, XLBND & XUBND.

! Bounds are only applied to plate or layer parameters for CXPAR = 3

!  INPUT:  NPLT,NLYR,NPAR,LBND,UBND,XYNORM,CXPAR
! OUTPUT:  XLBND,XUBND

 INTEGER NPLT,NLYR,NL1,NPAR,CXPAR(NPAR),JP,JP1,J1
 REAL XYNORM(NPLT)
 REAL,DIMENSION(NPAR) :: LBND,UBND,XLBND,XUBND

 DO JP = 1,NPLT
   DO J1 = 1,9
     JP1 = J1 + 9* (JP-1)
     IF (CXPAR(JP1) == 3) THEN
       IF (J1 < 5) THEN
         XLBND(JP1) = LOG (LBND(JP1))
         XUBND(JP1) = LOG (UBND(JP1))
       ELSE IF (J1 == 5 .OR. J1 == 6) THEN
         XLBND(JP1) = LBND(JP1) / XYNORM(JP)
         XUBND(JP1) = UBND(JP1) / XYNORM(JP)
       ELSE IF (J1 > 6) THEN
         XLBND(JP1) = LBND(JP1) / 180.    ! = Radians / PI
         XUBND(JP1) = UBND(JP1) / 180.
       END IF
     END IF
   END DO
 END DO

 NL1 = 2*NLYR - 1
 DO J1 = 1, NL1
   JP1 = J1 + 9*NPLT
   IF (CXPAR(JP1) == 3) THEN
     XLBND(JP1) = LOG (LBND(JP1))
     XUBND(JP1) = LOG (UBND(JP1))
   END IF
 END DO

END SUBROUTINE CNVRT_BOUNDS

 SUBROUTINE WRITE_INVMDL (NW,FINAL,ITS,NLYR,NPLT,NPAR,XPAR,XYNORM,NCNTRD,ECNTRD)
!------------------------------------------------------------------------------

!*** Called by: NLSQ2

! Transforms XPAR to user parameters and writes intermediate and final models
!
!  NW     : output unit
!  FINAL  : true if final model, false if intermediate model.
!  ITS    : print model after iteration ITS
!  NLYR   : number of layers
!  NPLT   : Number of plates (index = JP)
!  NPAR   = 9*NPLT + 2*NLYR-1plate conductances
!  XPAR   : transformed parameter array
!  XYNORM : maximum plate dimension for normalisation of plate X-Y location.
!  NCNTRD : north offset between model coordinates and real world coordinates.
!  ECNTRD : east offset between model coordinates and real world coordinates.
!
!    XPAR(K0+1)  <->  SIG_T
!    XPAR(K0+2)  <->  PLWDTH
!    XPAR(K0+3)  <->  PLNGTH
!    XPAR(K0+4)  <->  PLTOP
!    XPAR(K0+5)  <->  YCNTR
!    XPAR(K0+6)  <->  XCNTR
!    XPAR(K0+7)  <->  PLAZM
!    XPAR(K0+8)  <->  PLDIP
!    XPAR(K0+9)  <->  PLUNJ

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(p = 18)
 INTEGER NW,NLYR,NPLT,NPAR,ITS,ANG(3),K0,JP,J,K1,KP,KP1,JL
 REAL PLTE(9),ZP,XYNORM(NPLT),A
 REAL, DIMENSION(NLYR) :: RES, THK
 REAL, DIMENSION(NPAR) :: XPAR
 REAL(KIND=QL) DPLTE(2),NCNTRD,ECNTRD
 LOGICAL FINAL

 THK = 1.E5;  PLTE = 0. ; DPLTE = 0.

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
 DO JP = 1,NLYR - 1
   THK(JP) = EXP (XPAR(KP1+JP))
 END DO
 THK(NLYR) = 1.E5
 IF (NLYR > 1) THEN
   WRITE(NW,2,ADVANCE='NO') ('Lyr',JL,JL = 1,NLYR-1);
   WRITE(NW,'(7X,A)') 'Basement'
   WRITE(*,2) ('Lyr',JL,JL = 1,NLYR)
 ELSE
   WRITE(NW,'(T21,A)') 'Basement'; WRITE(*,'(T21,A)') 'Basement'
 END IF
 WRITE(NW,3) ('-----',JL = 1,NLYR);  WRITE(*,3) ('-----',JL = 1,NLYR)

 IF (FINAL) THEN
   WRITE(NW,4) RES(1:NLYR);   WRITE(*,4) RES(1:NLYR)
   WRITE(NW,14); WRITE(*,14)
   IF (NLYR > 1) THEN
     WRITE(NW,5) THK(1:NLYR-1); WRITE(*,5) THK(1:NLYR-1)
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
   DO J = 1,4   ! SIG_T, PLNGTH, PLWDTH, PLTOP
     PLTE(J) = EXP (XPAR(K0+J))
   END DO

   DO J = 5,6                ! ECNTR, NCNTR
     PLTE(J) = XYNORM(JP) * XPAR(K0+J)
   END DO

   DO J = 7,9                ! Azimuth, dip & plunge in degrees
     K1 = J - 6
     A = 180. * XPAR(K0+J)           ! XPAR * PI * (180 / PI)
     IF (J == 7) A = A + 90.         ! Convert from plate azimuth to dip azimuth
     ANG(K1) = INT (A)
   END DO

!  Parameter order: SIG_T, PLNGTH, PLWDTH, PLTOP, ECNTR,  NCNTR,  PLAZM, PLDIP, PLUNJ
!    Writing order: SIG_T, PLTOP,  ECNTR,  NCNTR, PLNGTH, PLWDTH, PLAZM, PLDIP, PLUNJ

   ZP = - PLTE(4)
   DPLTE(1) = REAL (PLTE(5),QL) + ECNTRD
   DPLTE(2) = REAL (PLTE(6),QL) + NCNTRD
   WRITE(*,7)  JP,PLTE(1),ZP,DPLTE(1:2),PLTE(2:3),ANG(1:3)
   WRITE(NW,7) JP,PLTE(1),ZP,DPLTE(1:2),PLTE(2:3),ANG(1:3)
 END DO

 1 FORMAT(//T9,'Model Description After',I3,' Iterations' &
           /T9,'====================================='/)
 2 FORMAT(T13,20(8X,A,I2:))
 3 FORMAT(T13,20(8X,A))
 4 FORMAT(T5,'Resistivity:',20G13.4)
 5 FORMAT(T5,'  Thickness:',20G13.4)
 7 FORMAT(T3,'Plate',I2,':',F7.0,F10.1,2F12.1,F10.1,F9.1,I9,2I7)
 10 FORMAT(//T9,'Final Model After',I3,' Iterations' &
            /T9,'==============================='//)
 11 FORMAT(//T14,'        ____ Plate Reference Point ____                     ______  Angles  ________'  &
            /T14,'       |                               |                   |                        |' &
            /T14,'         Depth      Centre      Centre    Plate     Plate     Dip' &
            /T14,'SIG_T     RL         East       North     Length    Width   Azimuth   Dip   Plunge' &
            /T14,'-----    -----      ------      ------    ------    -----   -------   ---   ------')
 14 FORMAT(2X)

 END SUBROUTINE WRITE_INVMDL

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
   DELPAR(I) = DPROD1 (NPAR,NPAR,1,0.0,VMAT(I,1),EIGPAR)
   WSP(I) = R(I) * R(I)
 END DO
 SQPDRE = DPROD1(NPAR,1,1,0.0,WSP(1),WSP(NPAR+1))

END SUBROUTINE SOLVE2


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
!


 SUBROUTINE NOISE_2_SIGR (NPAR,NDATA,XMODL,XDATA,NSR)
!----------------------------------------------------

!***  Called by NLSQ2

!  Calculates and prints the noise to signal ratio.

!***  Called by: MAIN

!      NPAR - the number of free variables
!     NDATA - the number of data values
!     XMODL - model data in microvolts
!     XDATA - data to be inverted in microvolts
!       NSR - noise to signal ratio

 IMPLICIT NONE
 INTEGER NPAR,NDATA,J1
 REAL, PARAMETER :: TOL=.1E-5
 REAL XMODL(NDATA),XDATA(NDATA),YM(NDATA),YD(NDATA),PK,PKM,BASE,ZM, &
      ZD,YBAR,NSR,CUMM,CUMD,CWMD

 INTENT (IN) NPAR,NDATA,XMODL,XDATA
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
 END DO

 YBAR = CUMM / NDATA

 CUMM = 0.
 CUMD = 0.
 DO J1 = 1, NDATA
   CUMM = CUMM + (YM(J1) - YBAR)**2
   CWMD = YM(J1) - YD(J1)
   CUMD = CUMD + CWMD**2
 END DO

 CUMM = CUMM / (NDATA - 1)
 CUMD = CUMD / (NDATA - NPAR)

 NSR = SQRT (CUMD / CUMM)

END SUBROUTINE NOISE_2_SIGR
