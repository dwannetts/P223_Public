! PROGRAM ArjunAir
!----------------------------------------------------------------------------------------
Module AA_Metadata
!--------------
! 
! Module designed to hold 'constant character options'
    Implicit NONE

    Character (Len = 40), Parameter :: PNAME = 'ArjunAir'
    Character (Len = 40), Parameter :: PVERS = '7.0.11'
    Character (Len = 40), Parameter :: PDATE = '02 February, 2021'
    Character (Len = 40), Parameter :: PAUT1 = 'CSIRO Electromagnetic Modelling Group'
    Character (Len = 40), Parameter :: PAUT2 = 'Art Raiche, Fred Sugeng and Glenn Wilson'
    Character (Len = 40), Parameter :: PAUT3 = 'David Annetts (david.annetts@csiro.au)'
    Character (Len = 40), Parameter :: PPROJ = 'AMIRA P223F'
    Character (Len = 80), Parameter :: PRELS = 'GPL-2.0 (Full text at https://opensource.org/licenses/gpl-2.0.php)'

End Module AA_Metadata
!----------------------------------------------------------------------------------------
!
!
!                        LANGUAGE: ANSI Standard Fortran 95
!                        ----------------------------------
!
!                        PREVIOUS P223F RELEASES
!                        -----------------------
!                        Version 7.0.9   2013-02-06
!                        Version 7.0.8   2012-09-21
!                        Version 7.0.7   2012-05-29
!                        Version 7.0.1   2006-10-23
!                        Version 7.0.0   2006-06-29
!                        Version 6.2.8   2006-03-03
!                        Version 6.1.2   2005-11-10
!                        Version 6.1.1   2005-10-17
!                        Version 6.1     2005-09-30
!                        Version 6.0     2005-08-18
!
!
!  ##############################################################################
!  #****************************************************************************#
!  #*                                                                          *#
!  #*    This program has been released strictly for use by:                   *#
!  #*                                                                          *#
!  #*    1. Sponsors of AMIRA project P223F                                    *#
!  #*                                                                          *#
!  #*    2. Authorised agents/contractors (P223F sponsor work only)            *#
!  #*                                                                          *#
!  #*    3. Universities formally included in the P223F collaboration scheme   *#
!  #*                                                                          *#
!  #*    4. Authorised CSIRO personnel                                         *#
!  #*                                                                          *#
!  #*    It is an explicit condition of use of this program that no part of    *#
!  #*    this program may be incorporated into other software without          *#
!  #*    the written permission of the P223F Project Manager, Dr. Art Raiche.  *#
!  #*                                                                          *#
!  #****************************************************************************#
!  ##############################################################################
!
!
!             Principal authors: Glenn Wilson, Art Raiche and Fred Sugeng
!
!             The inversion subroutines were developed by Glenn
!             The system and modelling framework was developed by Art
!             The finite element core was developed by Fred
!
!             RELEASED BY:
!
!             Electromagnetic Modelling Group
!             CSIRO Division of Exploration and Mining
!             PO Box 136
!             North Ryde NSW 1670
!             AUSTRALIA
!
!             Contact: Dr. Art Raiche
!                      Phone: 61 2 9498 8392
!                      Email: art.raiche@optusnet.com.au
!
!
!===========================================================================!
!                                                                           !
!                            Version History                                !
!                                                                           !
!===========================================================================!
!
!    CHANGE: 7.0.8 from 7.0.7
!    ------------------------
!	 Correct output for various inversions
!  	 Better formatting of numbers, especially of responses
!
!    CHANGE: 7.0.7 from 7.0.2
!    ------------------------
!    Better screen output
!    Better formatting of numbers, especially of responses
!
!    CHANGE: 7.0.2 from 7.0.1
!    ------------------------
!
!    Mesh coordinates written to MF1 file. Mesh lithologies written to both
!    MV1 and MF1 files.
!
!    CHANGE: 7.0.1   23 October 2006  (Time-domain only)
!    -------------------------------
!
!    A new option to allow the user to control the spectrum selection for time-domain
!    computation can be accessed by setting TDFD = 0.  Then in a new RECORD 2.1, the
!    user specifies the range and density of frequencies to be used.  In fact, two
!    ranges can be specified to allow a less dense sampling at lower frequencies.
!    In all cases, the program extrapolates the lower frequency response to DC before
!    using the Hankel filter to transform to time domain.
!
!    Changes in Version 7.0.0 and salient changes from recent versions
!    -----------------------------------------------------------------
!
!    Version 7.0.0 was the first major release for Project P223F.  It is the
!    first version capable of inversion for both time and frequency-domain
!    systems.  Previous changes for P223F releases are located in the HISTORY
!    section at the end of ArjunAir.f90.
!
!    1. During inversion, the element resistivities are written to ArjunAir.res
!       after each successful iteration.  If NLITH is negative, ArjunAir will
!       replace the element resistivities defined by the respective lithologies
!       with those contained in ArjunAir.res.
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
!       ArjunAir writes both the P-norm misfit and S-norm misfit to the screen
!       and ArjunAir.out files to enable comparison of the two inversion
!       options when the true model is not known.
!
!    3. Input has changed in RECORD 10.  The user must specify EASTD1, NORTHD1
!       the coordinates of the left-most mesh point.  This allows an arbitrary
!       flight path bearing to be registered with the appropriate 2D mesh.
!
!    4. The mesh concept has changed somewhat.  Instead of being a west to east
!       section, it is now the section directly under the flight path.
!       Positive X is now defined as both the flight path direction and the
!       horizontal coordinate of the mesh moving from left to right.
!
!       Therefore for a north to south flight path, the left-most node of the
!       user-defined mesh must be regarded as its northern-most point. Similarly
!       for an east to west flight path, the left-most node of the user-defined
!       mesh must be regarded as its eastern-most point.
!
!       In previous versions, the left-most point of the mesh was rigidly
!       regarded as corresponding to its western-most point.
!
!    5. Both time-domain and frequency-domain inversion are enabled.  However
!       only one line can be inverted.  If the flight path bearing deviates more
!       than 18 degrees, ArjunAir interprets that as a second line and the
!       program aborts with a kindly error message.
!
!    6. Additional convergence criterion: After the 10th iteration, the inversion
!       will stop if two successive iterations lower the RMS error by less than
!       1%. It is best to set MAXITS, the maximum number of iterations, to 90 so as
!       to not unduly restrict the inversion. It rare for ITS, the actual number
!       of iterations, to exceed 20 before convergence of some description is
!       achieved.
!
!    7. For inversion, the constraint scheme offers three options for each
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
!    8. The definitions for KCMP and ORDER have changed for frequency-domain
!       inversion to allow ArjunAir.inv files to be displayed more easily.
!
!    9. For option, SURVEY = 3: Specification of variable aircraft PITCH
!       has been replaced by specification of variable transmitter
!       inclination, TXCLN.
!
!   10. On input, frequency-domain stations are now defined by Tx-Rx midpoints,
!       On output, the mv1 (plotting) file contains both Tx and Rx coordinates
!
!   11. Forward model computations are now effective receiver positions located
!       on air nodes and then inperpolated back to the original positions to
!       supress spurious oscillations.
!
!   12. np Frequency format changed from G13.4 to F13.2.
!
!
!===========================================================================!
!                                                                           !
!                             Program Description                           !
!                                                                           !
!===========================================================================!
!
!
!   Forward modelling
!   -----------------
!
!   ArjunAir models the 3D airborne EM response of a general heterogeneous 2D
!   structure (cross strike section). This section lies in the vertical plane
!   under the flight path. The lithology is allowed to vary in this plane
!   (cross strike) but for purposes of 3D simulation, it is constant along
!   strike transverse to the flight line. ArjunAir is capable of modelling
!   high contrasts accurately.
!
!   The airborne system is modelled as a magnetic dipole transmitter with one
!   magnetic dipole receiver.  The receiver offset and orientation can vary
!   as a function of frequency in frequency domain or as a function of
!   position in time domain.  Up to three components are computed: vertical,
!   horizontal in-line, and horizontal transverse. The transmitter dipole can
!   be either vertical, horizontal, or tilted at any angle in the flight path
!   of the plane.
!
!   Frequency-domain solutions are obtained by transformation into a mixed
!   spatial-Fourier domain (x,Ky,z,f) where Ky is the wavenumber corresponding
!   to the strike direction. In this domain, an isoparametric finite-element
!   method is used to solve a coupled system of differential equations for the
!   along-strike components of the magnetic and electric fields. The global
!   matrix equation is solved using the frontal solution method. The vertical
!   and transverse strike magnetic field components are then computed in this
!   domain.  After obtaining solutions for 21 wavenumber values, the three
!   magnetic field components are then transformed into the (x,y,z,f) domain.
!
!   Time-domain responses are obtained from frequency-domain responses computed
!   at 6 points / decade from 10 Hz to 100 KHz. and at 3 points / decade
!   from 1 Hz to 10 Hz.  After extrapolating the step response back to DC, the
!   imaginary component is cubic-splined over the spectrum from DC to 100 KHz.
!   A specially designed 15 point / decade Hankel filter is used to compute the
!   time-domain response out to 5 complete cycles.  It is then folded and
!   convolved with the transmitter system waveform.
!
!   On-time and off-time channels are permitted.  The on-time channels work well
!   for B field but on-time dB/dt modelling requires the application of the
!   stripping algorithms.
!
!   In time domain, the user can specify the source in terms of either current
!   input or the receiver calibration waveform.  In each case, this is
!   converted to the effective dipole dI/dt.  When the calibration waveform is
!   used as the source input, the Tx - Rx geometry is used to compute the
!   effective dipolar dI/dt source.  This includes the effect of the airframe
!   currents as well as the direct source input.
!
!   In time-domain modelling, ArjunAir has a feature that allows the effects of
!   different waveforms, different channels and conversions between B & dB/dt
!   to be tested with negligible additional computation as long as the system
!   geometry, flight path and model are kept constant.  This is accomplished
!   by using the ArjunAir.frq file in which all of the frequency-domain
!   computations have been saved.
!
!   Inversion
!   ---------
!
!   ArjunAir inverts for the cell resistivities of the user-defined mesh.
!   The inherently non-linear inversion is performed as a number of linearised
!   steps based on based on a damped singular value decomposition of the
!   sensitivity matrix.  The first step in constructing this matrix is the use
!   of a reciprocity method to compute the first-order derivatives that comprise
!   the basic Jacobian.  To improve the condition and dynamic balance , these
!   derivatives are normalised by the cell conductivities, which is equivalent
!   to differentiating with respect to log resistivity.  The sensitivity matrix
!   is then constructed by normalising each row by by the average absolute data
!   value for the frequency or time channel corresponding to that row.
!
!   ArjunAir allows the user to constrain the inversion through the application
!   of any combination of friction or buffered-bounds constraints.
!
!
!   Mesh Design
!   ===========
!
!   Mesh design is a compromise between accuracy and runtime.  Runtime increases
!   as the cube of the number of nodes.  Accurate mesh design depends upon the
!   resistivities and geometries of the structures.  Based on a number of tests
!   against known solutions the following is offered as a guide.
!
!   For towed bird time-domain system start with a mesh 2 km along flight path
!   using 50 columns of 40 m width  For frequency-domain HEM systems, start with
!   a 1 k m long mesh using 40 columns of 25 m width.
!
!   In each case, the first four rows should be of thickness 12 m.  From there
!   on down, use a 20 m thick row and increase thickness of each subsequent row
!   at 6 percent down to a depth of 800 m for time-domain and 500 m for FD HEM
!   modelling.
!
!   Adjust row thicknesses and column widths to accommodate the model.  Columns
!   can be added near severe conductivity contrasts to widths of 20 or even
!   10 m.  The number of nodes in the model can be kept the same by eliminating
!   columns near the left and right edges and then readjusting the column widths
!   using exponential interpolation.
!
!   Although the number of cells can probably be kept constant, for very
!   resistive hosts, the wavelengths (skin depths) will be larger so that the
!   mesh will need to be extended but the cells can be made proportionally
!   bigger.  In the same vein, for very conductive hosts, the cells should be
!   made smaller but the mesh can be contracted due to increased attenuation.
!
!   Minimal mesh design to achieve good accuracy and comparatively short
!   computation times remains very much an art guided by expoerience.
!
!
!===========================================================================!
!                                                                           !
!                            Geometry Conventions                           !
!                                                                           !
!===========================================================================!
!
!
!   In time domain, the source coordinates consist of the transmitter easting,
!   northing and altitude.  For frequency-domain HEM systems, the source
!   location is taken as the transmitter-receiver midpoint.
!
!   The mesh coordinates are entered in terms of horizontal distance (X) and
!   elevation (negative downwards).  The mesh is assumed to lie under the flight
!   path.  In order to tie the aircraft positions to the 2D mesh, the easting
!   and northing of the top left-most node of the user-defined mesh must be
!   specified in ArjunAir.cfl.  The "3D locations" of model produced by
!   ArjunAir inversion will be computed by rotating the X (transverse strike)
!   mesh coordinates into the bearing of the flight path.
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
!   current or primary B at the receiver, the program assumes that current
!   or magnetic field will start at 0 or some low value, and rise to a
!   positive maximum before going to zero or oscillating about zero with
!   small magnitudes. In this case, dI/dt will be computed using the
!   negative I or B so that the early off-time response is positive for
!   vertical fields.
!
!   If the user specifies the excitation waveform as dB/dt at the receiver,
!   the program assumes that the response will rise to a positive maximum
!   followed by a negative maximum before oscillating about zero with
!   smaller amplitudes. In this case dI/dt is derived by reversing the sign
!   of the input primary dB/dt and dividing by the geometric coupling
!   factor. Again, this procedure is designed so that the early off-time
!   response is positive for vertical fields.
!
!
!===========================================================================!
!                                                                           !
!                             File Conventions                              !
!                                                                           !
!===========================================================================!
!
!
!   The input control data must be read from ArjunAir.cfl, which is assigned to
!   logical unit 3.
!
!   The data to be inverted, along with weighting and similar information, must
!   be read from ArjunAir.inv which is assigned to logical unit 13.
!   The format of ArjunAir.inv is identical to that of LeroiAir.inv, SamAir.inv
!   and Airbeo.inv to enable ease of portability between inversion programs.
!
!   Frequency-domain output data is written to ArjunAir.frq and assigned to
!   logical unit 7.  This is useful for the restart option that enables different
!   time-domain waveforms and receiver window options to be tested without
!   having to repeat the explicit model computations.  Note that this option
!   requires that the model and AEM geometry and locations be unchanged.
!
!   The complete output data file including input variable assignment for both
!   forward modelling and inversion is written to ArjunAir.out and assigned to
!   logical unit number 4.
!
!   Messages about data or runtime errors are now written to the file
!   ArjunAir.log and assigned to logical unit 9.
!
!   For plotting forward modelling results, succinct output data is written to
!   ArjunAir.mf1 and assigned to logical unit 14.
!
!   For inversion plotting and imaging, succinct output data is written to
!   ArjunAir.mv1 and assigned to logical unit 14.
!
!
!     UNIT #   UNIT ID      FILE ID           FUNCTION
!     ------   -------      -------           --------
!       3        NR       ArjunAir.cfl    Input control file
!       13       NRI      ArjunAir.inv    Inversion data
!       4        NW       ArjunAir.out    Complete output description
!       7        ND       ArjunAir.frq    Frequency-domain data for restart option
!       8        NS       ArjunAir.sty    Frequency-domain sensitivity during inversion
!       9        NLG      ArjunAir.log    Runtime and data error messages
!       10       NM       ArjunAir.res    Element resistivity
!       14       NWI      ArjunAir.mf1    Output data from forward modelling in plot format
!       14       NWI      ArjunAir.mv1    Output data from inversion in plot format
!
!
!===========================================================================!
!                                                                           !
!              Description of data records for ARJUNAIR.CFL                 !
!                                                                           !
!===========================================================================!
!
!
!  All records are in list directed (free) format except for the TITLE
!  in RECORD 1.
!
!  NOTE:  All distances and locations are to be specified in metres.
!         All angles are to be specified in degrees.
!
!----------------------------------------------------------------------------
!
!
!**  RECORD 1:  TITLE - up to 120 characters.
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
!            This is over-ridden by setting TDFD = 0 in which case the user needs to
!            specify five integers in RECORD 2.1: KLO, KHI, KMD, PPD1, PPD2
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
!                 (95-99 percent of ArjunAir usual computation time) use
!                 previously computed frequency-domain responses contained
!                 in file ArjunAir.frq to calculate 3-D time-domain responses.
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
!    Each time ArjunAir is run with DO3D = 1, a file called ArjunAir.frq
!    is created on logical unit ND = 7.  ArjunAir reads this file to
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
!**  RECORD 2.1:  KLO, KHI, KMD, PPD1, PPD2
!
!       KLO - Lowest frequency expressed as an integer power of 10
!             For example setting LOW = -1, 0 or 1 would mean that the lowest
!             frequency would be either 0.1, 1.0, or 10.0 Hz repectively.
!
!       KHI - Highest frequency expressed as an integer power of 10
!             For example setting HIGH = 5 or 6 would mean that the highest
!             frequency would be either 0.1 or 1.0 MHz repectively.
!
!       KMD - Frequencies below MID are spaced at PPD1 points per decade
!             Frequencies above MID are spaced at PPD2 points per decade
!
!       The allowed values for both PPD1 and PPD2 are 3, 6 or 12.
!       If PPD1 < 3, it is changed to 3
!       If 3 < PPD1 < 6, it is changed to 6
!       If PPD1 > 6, it is set to 12
!
!      Thus, depending upon the first channel time, for the default case of TDFD = 1,
!
!      KLO = 0,  KHI = 5,  KMD = 1,  PPD1 = 3 and PPD2 = 6 => NFRQ = 28   or
!      KLO = 0,  KHI = 6,  KMD = 1,  PPD1 = 3 and PPD2 = 6 => NFRQ = 34
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
!            have the option to apply this algorithm to ArjunAir model output.
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
!  FLIGHT PATH INFORMATION
!  =======================
!
!  RECORD 9.1 and 9.2 are used only for forward modelling, DO3D = 0 or 1.
!
!
!    INVERSION:
!    ----------
!
!  For inversion, make a single entry for RECORD 9.0 and go on to RECORD 10.
!
!
!**  RECORD 9.0: NPASS
!
!      Only for inversion - NPASS should be set to 0 or 1.
!
!      NPASS > 1 will the cause program to ignore the next NPASS records.
!
!
!    FORWARD MODELLING (DO3D = 0, 1, or 2):
!    --------------------------------------
!
!
!**  RECORD 9.0:  NSTAT, SURVEY, BAROMTRC
!
!      NSTAT  - Number of transmitter positions for single line
!               NSTAT must be > 1
!
!      SURVEY = 2 => Variable altitude & course but constant Tx-Rx geometry.
!
!             = 3 => Variable altitude, course, transmitter pitch
!                    and receiver offset (time-domain only).
!
!     BAROMTRC = 0 => Altitudes are ground clearance in metres.
!
!              = 1 => Altitudes are either GPS or barometric; ie, metres above sea level.
!
!
!   Variable course and altitude; NSTAT records to follow):
!   -------------------------------------------------------
!
!     Enter  RECORD 9.J, J = 1,NSTAT - i.e., one for each station:
!
!   If SURVEY = 2
!**  RECORD 9.J:  ALINE, EAST(J), NORTH(J), ALT(J)
!
!
!   If SURVEY = 3 (Time-domain only):
!**  RECORD 9.J:  ALINE, EAST(J), NORTH(J), ALT(J), TXCLN(J), ZRX(J), XRX(J), YRX(J)
!
!
!      ALINE    - line number
!      EAST(J)  - East coordinate of transmitter at station J.
!      NORTH(J) - North coordinate of transmitter at station J.
!
!      --------------------------------------------------------------------
!      In frequency domain, these are interpreted as the Tx-Rx midpoint.
!      The Tx position for all frequencies are based on the offset for
!      the first frequency.  The Rx position for each frequency is computed
!      using the offset for that frequency relative to the Tx position for
!      frequency 1.
!      --------------------------------------------------------------------
!
!      ALT(J) - Altitude of transmitter at station J.
!      ZRX(J) - Vertical offset of receiver at station J,
!               below = positive.
!
!      XRX(J) - In-line horizontal offset of receiver at station J,
!               behind = positive.
!      YRX(J) - Transverse horizontal offset of receiver at station J,
!               Left = positive.
!
!
!===========================================================================!
!
!
!                  LITHOLOGY AND MODEL STRUCTURE FOR ARJUNAIR
!                  ------------------------------------------
!
!
!   The heterogeneous domain must be divided into a mesh that is defined
!   by the user. The following records set the mesh size reference.
!
!** RECORD 10:  NZ, NX, NLITH, EASTD1, NORTHD1
!
!
!     NZ - Number of horizontal rows of domain nodes
!     NX - Number of vertical columns of domain nodes
!
!     NLITH - Number of distinct lithological units.
!
!             If NLITH is negative, ArjunAir reads the element resistivities
!             from ArjunAir.res (logical unit NM) and internally over-writes
!             the resistivity defined by each element's lithology with those
!             read from ArjunAir.res.
!
!     EASTD1  = east coordinate of the top left node of user specified mesh.
!     NORTHD1 = north coordinate of the top left node of user specified mesh.
!
!*********************************************************************************
!                       CRUCIAL NOTE
!                       ------------
!
!  This version of ArjunAir defines positive X as being along the flight direction.
!  This refers to mesh coordinates which are taken as being directly below the
!  flight path.  Therefore, positive direction along the mesh must correlate to
!  positive distance along the flight path.
!
!  Example 1: If the flight path were east to west, then the mesh would similarly
!             run west to east and the left-most top node, (EASTD1,NORTHD1) would
!             also be the western-most mesh point.
!
!  Example 2: If the flight path were north to south, then the mesh would run
!             north to south and the left-most top node, (EASTD1,NORTHD1) would
!             be the northern-most mesh point.
!
!*********************************************************************************
!
!
!    Define lithologies:
!    -------------------
!
!
!**  RECORD 11.1:  RES(1), SIG_T(1), RMU(1), REPS(1), CHRG(1), CTAU(1), CFREQ(1)
!**  RECORD 11.2:  RES(2), SIG_T(2), RMU(2), REPS(2), CHRG(2), CTAU(2), CFREQ(2)
!        :           :        :        :        :        :        :        :
!        :           :        :        :        :        :        :        :
!**  RECORD 11.N:  RES(N), SIG_T(N), RMU(N), REPS(N), CHRG(N), CTAU(N), CFREQ(N)
!
!
!      N = NLITH; Number of distinct lithological units.
!
!      RES(I) - cell resistivity
!
!      SIG_T(I) - NOT USED IN ARJUNAIR: Conductance (conductivity-thickness
!                 product). A value for SIG_T must be entered even though it
!                 isn't used.
!
!      RMU(I) - Relative magnetic permeability for LITH_INDEX(I).
!      REPS(I) - Relative dielectric constant (permittivity for LITH_INDEX(I).
!      CHRG(I) - Cole-Cole chargeability for LITH_INDEX(I).
!      CTAU(I) - Cole-Cole time constant for LITH_INDEX(I).
!      CFREQ(I) - Cole-Cole frequency constant for LITH_INDEX(I).
!
!          Default values: RMU = 1, REPS = 1, CHRG = 0, CTAU = 0, CFREQ = 1.
!
!          The default means no magnetic permeability contrast (MU = 4 PI * 10^(-7)),
!          no dielectric constant contrast (EPSILON = 8.854215E-12) and no IP effects
!          (no Cole-Cole).
!
!
!    Node locations:
!    ---------------
!
!    Nodes and associated lithologies are read one row at a time from the top
!    (surface) to the bottom. The lithology of each four cornered cell is
!    associated with the top, western node defining that cell.
!
!    There are NZ * NX records to be read in below; one for each permutation
!    of J and K.
!
!
!**  RECORDS 12.(J,K):  K, J, ZLOC, XLOC, LITH
!
!
!      1 <= K <= NZ is the node index increasing from surface to bottom.
!      1 <= J <= NX is the node index increasing from left to right.
!
!      ZLOC(J,K) = The relative level of node (J,K) in metres.
!                  For fixed J, it must increase negatively with
!                  increasing K.
!
!      XLOC(J,K) = The horizontal coordinate of node (J,K) in metres.
!                  For fixed K, it must increase with increasing J.
!
!        Each cell (or mesh element) is defined by its four corner nodes.
!        Each cell is identified by the indices of the node of its top,
!        western corner.
!
!        Thus LITH (J,K) will be the lithology index of the cell
!        defined by nodes:
!
!          (K, J),   (K+1, J),   (K+1, J),   (K+1, J+1)
!
!        LITH is not read if J = NX or if K = NZ
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
!                       INVERSION INPUT FOR ARJUNAIR
!                       ----------------------------
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
!      MV1PRT refers to the output print level in Airbeo.mv1.
!
!      OUTPRT refers to the output print level in Airbeo.out.
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
!       latitude than parameter fixation.
!
!       For CTYPE = 3 where the model parameter NM is associated with LITH_INDX, then
!       DELPAR(NM)' = ELAS * DELPAR(NM) provided that LBND =< DELPAR(NM)' =< UBND. If it
!       exceeds either bound, it is set to that bound.
!
!
!   END OF ARJUNAIR.CFL
!
!
!===========================================================================!
!                                                                           !
!              Description of data records for ARJUNAIR.INV                 !
!                                                                           !
!===========================================================================!
!
!   Note:
!   -----
!
!   The format of ArjunAir.inv is identical to LeroiAir.inv, SamAir.inv and
!   Airbeo.inv to enable ease of portability between inversion programs.
!
!   Any number of comment lines can be inserted ONLY at the beginning of
!   ArjunAir.inv. These must be designated by making the first character
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
!                  In-Line response of CMP = 11
!                  Transverse response of CMP = 12
!           = NCHNL+1 to 2*NCHNL refer to in-line response If CMP = 2
!           = 2*NCHNL+1 to 3*NCHNL refer to transverse response of CMP = 3
!
!
!**  RECORD 1: NSTAT, SURVEY, BAROMTRC, KCMP, ORDER
!
!      NSTAT  - number of stations to be read in from ArjunAir.inv.
!
!      SURVEY = 2 => variable altitude and course but constant Tx-Rx geometry.
!      SURVEY = 3 => variable altitude, course, transmitter pitch and receiver
!                    offset (time-domain only).
!
!      BAROMTRC = 0 => altitudes are ground clearance in metres.
!               = 1 => altitudes are barometric; ie, metres above sea level.
!
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
!      Frequency-domain:
!
!      KCMP(1:NFRQ) : specify components for each frequency. These must be in the
!                     order as the frequencies in SamAir.cfl
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
!**  RECORD 2: DATA_FLOOR
!
!      Any data value whose absolute magnitude is less than DATA_FLOOR will be
!      weighted to zero.
!
!      For time-domain, only one value is required.
!
!      For frequency-domain, 2 * NFRQ values must be entered.
!
!      If KCMP = 12: The first NFRQ values refer to in-phase measurements for
!                    each frequency followed by NFRQ values for each quadrature
!                    measurement.
!
!      If KCMP = 21: The first NFRQ values refer to quadrature measurements for
!                    each frequency followed by NFRQ values for each in-phase
!                    measurement.
!
!      ArjunAir assumes that each group will start from the floor for the
!      lowest frequency and proceed sequentially to that for the highest.
!
!
!**  RECORD 3: N0STAT, N0CHNL, N0PTS
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
!**  RECORD 3.1:  K0STAT(1:K0STAT) - Indices of stations for which all data
!                                     will be weighted to 0.
!
!
!    Only if N0CHNL > 0:
!    -------------------
!
!**  RECORD 3.2:  K0CHNL(1:N0CHNL) - Indices of PCHNLs for which all data
!                                     will be weighted to 0.
!
!
!    Only if N0PTS > 0:
!    ------------------
!
!**  RECORD 3.3:  (J0CH(I),J0ST(I)), I = 1,N0PTS)
!
!      PCHNL and station indices of individual points to be weighted to 0
!      using the above PCHNL ordering convention.
!
!
!   DATA ENTRY
!   ==========
!
!   NSTAT records:
!
!   For SURVEY = 2:
!   ---------------------
!**  RECORD 4.J: ALINE, EAST(J), NORTH(J), ALT(J), DATA(1:NPCHNL, J)
!
!
!   For SURVEY = 3 (time domain only):
!   ---------------------------------
!**  RECORD 4.J: ALINE, EAST(J), NORTH(J), ALT(J), TXCLN(J), ZRX(J), XRX(J), YRX(J), DATA(1:NPCHNL,J)
!
!      DATA(I,J) = datum of PCHNL I at station J
!
!      Note:
!      -----
!
!      Unnormalised B data is in pT and unnormalised dB/dt data is in nT/s
!      unless otherwise specified in RECORD 2.2.
!
!      For frequency-domain inversion or if KPPM > 0, normalised data are
!      expressed in ppm unless otherwise specified using NPPF /= 3.
!
!      ZRX(J), XRX(J) = Vertical and inline offsets at station J.
!
!
!   END OF ARJUNAIR.INV
!
!
!===========================================================================!
!
!
!  There are no sample control files in this documentation because of
!  their length. Refer to the example control files supplied with this
!  release of ArjunAir.
!
!
!===========================================================================!
!                                                                           !
!                              P223F Modules                                !
!                                                                           !
!===========================================================================!

 MODULE AA_Filter_coefficients_QL

!----------------------------------------------------------------------------
!
!  Niels Christensen shifted cosine filter:
!  12 points per decade; OMEGA = 0.3 PI.
!  These are based on using extended precision.
!
!----------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL = SELECTED_REAL_KIND(12,80)
 INTEGER J9
 REAL(KIND=QL) WCOS(-200:99),DELCOS
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

 END MODULE AA_Filter_coefficients_QL
!============================================================================

 MODULE AA_Filter_coefficients
!----------------------------------------------------------------------------
!
!*** Used by: SET_TRP,COSTRN,EHFLD,HYEY,SENS_FD_CONSTRUCT
!
! Niels Christensen shifted cosine and sine filters:
! 12 points per decade; OMEGA = 0.3 PI; Shift = 0
!
!----------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER, PARAMETER :: NLO = -214, NHI = 85, NDEC = 12
 INTEGER LL

 REAL WCOS(NLO:NHI), WSIN(NLO:NHI)
 DATA (WCOS(LL), LL = NLO, -65)/ &
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

 DATA (WCOS(LL), LL = -64, NHI)/ &
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

 DATA (WSIN(LL), LL = NLO, -65)/ &
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

 DATA (WSIN(LL), LL = -64 ,NHI)/ &
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

 END MODULE AA_Filter_coefficients

!============================================================================

 MODULE AA_Input_routines

!----------------------------------------------------------------------------
!
! Called by: MAIN
!
! Contains: READ_SYSTEM_DATA,READ_MODEL_DATA,SET_AUTO_TX_POST,SET_MESH_COMPLETION
!           LITHMAP,SET_FRQ,SET_TRP,READ_INVERSION_CONTROLS,
!           READ_INVERSION_DATA
!
! Read airborne system and 2.5-D model parameters for ArjunAir; forward
! modelling and inversion parameters and data.
!
!----------------------------------------------------------------------------

 ! Generic P223F variables
 ! -----------------------

 Use iso_Fortran_env
 Use AA_Metadata
 IMPLICIT NONE

 LOGICAL INVERT,NEWF,JCBN,RESID,WRITE_FWD,INVERT_RX_DATA,READ_RES_FILE
 REAL, PARAMETER :: T0_MIN = 1.E-7, PI = 3.141592654, DATA_TOL = 1.E-24, &
                    TURN = PI/10.
 INTEGER, PARAMETER :: NAIR=11, NPROP = 7, NCPTS = 3, QL = SELECTED_REAL_KIND(12,80)
 Integer, Parameter :: FVERS = 600
 INTEGER NM,NR,NW,NLG,ND,NS,NRI,NDR,np,KS,TDFD,STEP,DO3D,ISW,PRFL,NCHNL,KPPM,ISTOP, &
         NLITH,NSX,NSX1,JF,JT,JS,JC,JP,JL,JR,NFRQ,NTYRP,NSTAT,NRX,NRXST,NRXSTQ,NTRN, &
         MTXRX,NPULS,NTYPLS,SURVEY,CMP,KRXW,MSG,MXERR,I,J,GSTRP,MAXITS,A2C,ASTRP,    &
         IUNITS,NPPF,MCHNL,NDATA,NPAR,MV1PRT,OUTPRT,CNVRG,BAROMTRC,LINE,KP,KCMP, &
         IFL,NCOMP,KLO,KHI,KMD,KQ(5),PPD1,PPD2
 INTEGER, ALLOCATABLE,DIMENSION(:) :: LITH,KFIX,CXPAR,CFG1
 INTEGER, ALLOCATABLE, DIMENSION(:,:) :: RWTS, LITHM(:,:)
 REAL TXAMPL,TXFREQ,PULSE,PKCUR,OFFTYM,ALF,DELT,ALT1,GND_LVL,TXCLN0,XRX0, &
      YRX0,ZRX0,CSF,SNF,PRM_TD(3),BFFAC,PPFAC,TXAREA,CMIN,CMAX,T0,QCND,   &
      PCTCNV,DSTAT,FANGLE,ALINE,BIG,S1
 REAL, DIMENSION(:), ALLOCATABLE :: FREQ,WAVEFORM,TXON,SWX,TMS,WTMS,TOPN,TCLS,SZ,SZQ,SXQ,ZRX, &
                                    XRX,YRX,FNGL,THK,RES,RMU,REPS,CHRG,TRP,CALF,CTAU,CFREQ, &
                                    TXCLN,TXDEG,PRM_FD,UBND,LBND,ELAS,MPAR,DATA_FLOOR
 REAL, DIMENSION(:,:), ALLOCATABLE :: SWY,RX,RY,RZ,RXQ,RYQ,RZQ,LYTH,RDATA,GEOELEC_PAR
 REAL(KIND=QL) QFRQ1,QFRQ2,FQQ,EAST1,NORTH1
 REAL(KIND=QL), ALLOCATABLE :: SXD(:),SYD(:),RXD(:,:),RYD(:,:)
 CHARACTER (LEN = 3), ALLOCATABLE :: CONFIG(:)
 CHARACTER (LEN = 1)   TCHR
 CHARACTER (LEN = 120) INP,TITLE
 CHARACTER (LEN = 60)  PVC
 CHARACTER (LEN = 4)   QUNIT,BUNIT,PUNIT
 Integer :: tvals(8)
 Logical :: txa90

 ! ArjunAir specific variables
 ! ---------------------------

 INTEGER NX,NZ,NXZ,NPN,NPM,NZM,NXM
 REAL RZ0,SXD1,SYD1,SZD1
 REAL,ALLOCATABLE,DIMENSION(:) :: ZSURF,XSURF,XSURFD
 REAL,ALLOCATABLE,DIMENSION(:,:) :: XLOC,XLOCD,ZLOC,COORD
 INTEGER,ALLOCATABLE,DIMENSION(:,:) :: LITHD,LNODS,IREL
 REAL(KIND=QL) XCNTRD, EASTD1, NORTHD1

 SAVE

 CONTAINS ! SUBROUTINES  *  READ_SYSTEM_AND_SURVEY
          !              *  SET_FRQ
          !              *  SET_TRP
          !              *  READ_MODEL_DATA  -  ArjunAir specific.
          !              *  SET_NODE_ROWS    -  ArjunAir specific.
          !              *  LITHMAP          -  ArjunAir specific.
          !              *  READ_INVRT_CNTRL
          !              *  READ_INVRT_DATA

   !-----------------------------------!
   ! MODULE: INPUT_DATA_FOR_ARJUNA_AIR !
   !-----------------------------------!

   SUBROUTINE READ_SYSTEM_AND_SURVEY
!  ---------------------------------

!***  Called by MAIN
!***  Calls CUBSPL, CALL CONFIG_ID, WRITE_LOG_FILE

   REAL BFFACS(6),PPFACS(4),RHO,DELX,DELY,A1
   CHARACTER (LEN = 4) PUNITS(4),BUNITS(6)
   CHARACTER (LEN = 19) WVFRM(3)
   DATA WVFRM /'Transmitter current','Vertical receiver  ','Horizontal receiver'/
   DATA PUNITS /'pct ','ppt ','ppm ','ppb '/
   DATA BUNITS /'nT/s','pT/s','fT/s','nT  ','pT  ','fT  '/
   DATA BFFACS /1.,1000.,1.E6,1.,1000.,1.E6/
   DATA PPFACS /1.E2,1.E3,1.E6,1.E9/

   NR =  3     ! Input unit number for ArjunAir.cfl
   NW =  4     ! Output unit number for ArjunAir.out
   ND =  7     ! Input/Output unit number for ArjunAir.frq
   NS =  8     ! Output unit number for ArjunAir.sty
   NLG = 9     ! Log file unit number for ArjunAir.log
   NM = 10     ! Unit number for ArjunAir.res.
   NRI = 13    ! Inversion data for ArjunAir.inv
   np = 14    ! Output unit number for ArjunAir.mv1 (inversion)
               ! or ArjunAir.mf1 (forward model)

   OPEN(NR,FILE = 'ArjunAir.cfl',STATUS = 'OLD')
   OPEN(NW,FILE = 'ArjunAir.out',STATUS = 'REPLACE')

   ! Initialise some variables.

   MXERR = 0             !  Initialise input error flag
   NCHNL = 1             !  Initialise dimensions
   NPPF = 3              !  ppm default
   IUNITS = 5            !  pT default
   GSTRP = 0
   ASTRP = 0
   TXAREA = 1.
   PRM_TD = 0.
   INVERT = .FALSE.
   NEWF = .FALSE.
   NDR = NR              !  Read from ArjunAir.cfl

   ! Reproduce input data with no assignments and rewind file.

   REWIND NR
	Call Date_and_time(Values = tvals)
	Write (NW, 1) PNAME, PVERS, PDATE, PAUT1, PAUT2, PAUT3, PPROJ, PRELS, &
              compiler_version(), compiler_options(), &
              tvals(1:3), tvals(5:7)
	Write ( *, 1) PNAME, PVERS, PDATE, PAUT1, PAUT2, PAUT3, PPROJ, PRELS, &
              compiler_version(), compiler_options(), &
              tvals(1:3), tvals(5:7)

   READ(NR,'(A)') TITLE               ! RECORD 1
   WRITE(NW,'(/1X,A)') TRIM (TITLE)

! Read model controls and print parameters.

!  TDFD = 0, 1 or 2 for user controlled TD, standard TD or FD respectively.
!  DO3D =  2 => use old FD data from ArjunAir.frq.
!       =  1 => compute new 2.5D forward model.
!       = -1 => do 2.5D inversion with S norm.
!       = -2 => do 2.5D inversion with P norm.
!  PRFL - indicates profile or decay curve output.
! ISTOP - read data and stop if ISTOP = 1.
!       - used as a frequency setter later on in this routine.

   READ(NR,*)  TDFD, DO3D, PRFL, ISTOP        ! RECORD 2
   WRITE(NW,3) TDFD, DO3D, PRFL, ISTOP
   IF (DO3D < 0) INVERT = .TRUE.

   IF (PRFL == 0 .OR. PRFL == 2 .OR. PRFL == 10) THEN
     PRFL = 2
   ELSE
     PRFL = 1
   END IF

   IF (TDFD < 0 .OR. TDFD > 2) CALL WRITE_LOG_FILE (NLG,1,MXERR,2)
   IF (TDFD == 2 .AND. DO3D == 2) THEN
     CALL WRITE_LOG_FILE (NLG,20,MXERR,1)
     DO3D = 1
   ELSE IF (DO3D < -2 .OR. DO3D > 2) THEN
     CALL WRITE_LOG_FILE (NLG,2,MXERR,1)
     DO3D = 1
   END IF

   IF (TDFD < 2) THEN
     IF (DO3D == 1) THEN
       NEWF = .TRUE.
       OPEN(ND,FILE='ArjunAir.frq',STATUS='REPLACE')
     ELSE IF (DO3D == 2) THEN ! Data to be read in from ArjunAir.frq:
       OPEN(ND,FILE='ArjunAir.frq',STATUS='OLD')
     END IF

! Time-domain transmitter system information.

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
     READ(NR,*)  ISW, NSX1, STEP, IUNITS, NCHNL, KRXW, OFFTYM         ! RECORD 3 (Time Domain)
     WRITE(NW,4) ISW, NSX1, STEP, IUNITS, NCHNL, KRXW, OFFTYM

     IF (IUNITS < 1 .OR. IUNITS > 3) CALL WRITE_LOG_FILE (NLG,3,MXERR,2)
     IF (STEP == 1 .AND. IUNITS < 4) IUNITS = IUNITS + 3
     IF (ISW == 4)  THEN
       IUNITS = 6                         ! Default.
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

     ALLOCATE (TXON(NSX1+1),WAVEFORM(NSX1+1),TMS(NCHNL),WTMS(NCHNL), &
               TOPN(NCHNL),TCLS(NCHNL))

     TXON = 0. ; WAVEFORM = 0. ; TMS = 0. ; WTMS = 0. ; TOPN = 0. ; TCLS = 0.

     IF (ISW == 4) THEN
       READ (NR,*) TXFREQ, TXAMPL   ! RECORD 4 (Time Domain) Step B response for full duty cycle rectangular pulse
       SWX(1) = 0.
       PULSE = .5 / TXFREQ
       SWY(1,1) = TXAMPL
       IF (NPULS == 1) THEN
         WRITE(NW,11) TXAMPL
       ELSE
         WRITE(NW,12) TXAMPL, TXFREQ, 1000. * PULSE
       END IF
     ELSE
       IF (ISW == 1) THEN
         WRITE(NW,13) WVFRM(1),'amps'
       ELSE IF (ISW == 10 .OR. ISW == 11) THEN ! In-line component.
         WRITE(NW,13) WVFRM(3),BUNIT
       ELSE IF (ISW > 11) THEN                 ! Vertical comonent.
         WRITE(NW,13) WVFRM(2),BUNIT
       END IF

       READ(NR,*) (TXON(J),WAVEFORM(J), J = 1,NSX1)  ! Read in source waveform.
       NSX = NSX1

       IF (TXON(1) > 1000. * T0_MIN) THEN ! Fill in 0 point if not in original data.
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
       READ(NR,*) (TOPN(J) ,TCLS(J), J = 1,NCHNL)  ! RECORD 5 (Time Domain)
       TMS = (TOPN + TCLS) / 2.
       WTMS = TCLS - TOPN
     ELSE
       READ(NR,*) TMS(1:NCHNL)                     ! RECORD 6
       READ(NR,*) WTMS(1:NCHNL)
       TCLS= TMS + WTMS /2.
       TOPN= TMS - WTMS /2.
     END IF
     WRITE(NW,14)

     DO JT = 1, NCHNL
       WRITE(NW,'(8X,I3,2F12.3,F11.3,F12.3)') JT,TOPN(JT),TCLS(JT),WTMS(JT),TMS(JT)
       IF ( TOPN(JT) <= 0) CALL WRITE_LOG_FILE (NLG,7,MXERR,2)
     END DO

     TOPN = 1.E-3 * TOPN
     TCLS = 1.E-3 * TCLS

! End of time-domain system input data. If a new frequency-domain
! step data set is to be created, save the rest of the input data
! to unit number ND.  If pre-existing frequency-domain data is to
! be re-worked, read from NDR.
! Read in Tx area, turns and the number of receivers.

     READ(NR,*) TXCLN0,CMP,KPPM      ! RECORD 7 (Time Domain)
     IF (KPPM > 0) THEN
       IF (KPPM /= 1 .AND. KPPM /= 3 .AND. KPPM /= 123 .AND. KPPM /= 4) CALL WRITE_LOG_FILE (NLG,10,MXERR,2)
       READ(NR,*) NPPF                       ! RECORD 7.1 (Time Domain)
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
         CMP = 2
         CALL WRITE_LOG_FILE (NLG,9,MXERR,1)
       END IF
     END IF

! Normalisation isn't defined for step output and dB/dt waveform
! calibration or impulse output and B calibration. It isn't used
! for the pure rectangular step waveform.

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
       READ(NR,*) TXAREA,NTRN             ! RECORD 7.2 (Time Domain)
       WRITE(NW,16) NINT(TXAREA),NTRN
     ELSE IF (ISW > 100) THEN        !  ISW > 100 => central loop system
       READ(NR,*) TXAREA
       WRITE(NW,17) NINT(TXAREA)
     END IF

     IF (ISW == 1) WAVEFORM = WAVEFORM * NTRN * TXAREA

     READ(NDR,*)  ZRX0, XRX0, YRX0      ! RECORD 8 (Time Domain)
     WRITE(NW,18) ZRX0, XRX0, YRX0
     RHO = ABS (XRX0) + ABS (YRX0)
     IF (RHO < 1. .AND. KPPM > 0) KPPM = 3

   ELSE IF (TDFD == 2) THEN

     IF (DO3D == 1) OPEN(ND,FILE='ArjunAir.frq',STATUS='REPLACE')

     ! Frequency-domain transmitter system information.

     ! Allocate TD arrays to unit dimension and null quantity.

     NCHNL = 1;  NSX = 1;  NTYRP=1
     ALLOCATE (SWX(NSX),SWY(NSX,3),TRP(NTYRP),WAVEFORM(NSX),TMS(NCHNL),WTMS(NCHNL),TOPN(NCHNL),TCLS(NCHNL))
     SWX=0.;  SWY=0.;  TRP=0.; WAVEFORM=0.; TMS=0.; WTMS=0.; TOPN=0.; TCLS=0.

     WRITE(NW,7)

     IF (IUNITS < 4) IUNITS = IUNITS + 3   ! Convert to B. Default was 5.

     BUNIT = BUNITS(IUNITS)

     READ(NR,*) NFRQ, CMP, NPPF              ! RECORD 3 (Frequency Domain)
     IF (NPPF < 1 .OR. NPPF > 4) NPPF = 3

	 IF (CMP == -1) THEN
       TXA90 = .TRUE.
       CMP = 1
       WRITE(NW,8)
       IF (NPPF < 1 .OR. NPPF > 4) NPPF = 3
     END IF

     IF (CMP < 1 .OR. CMP > 3) CALL WRITE_LOG_FILE (NLG,13,MXERR,1)
     IF (INVERT) THEN
       IF (CMP /= 1) CALL WRITE_LOG_FILE (NLG,13,MXERR,2)
     ELSE
       IF (CMP < 1 .OR. CMP > 3) CALL WRITE_LOG_FILE (NLG,14,MXERR,1)
       CMP = 1
     END IF

     PUNIT = PUNITS(NPPF)
     QUNIT = PUNIT
     IF (CMP > 1) QUNIT = BUNIT
     PPFAC = PPFACS(NPPF)
     BFFAC = BFFACS(IUNITS)

     IF (DO3D < 0) THEN
       WRITE(NW,24) NFRQ,CMP,NPPF
     ELSE
       WRITE(NW,19) NFRQ,CMP,NPPF,QUNIT
     END IF

     NRX = NFRQ
     NRXST = NFRQ

     ALLOCATE (PRM_FD(NFRQ),FREQ(NFRQ),ZRX(NFRQ),XRX(NFRQ),YRX(NFRQ), &
               TXCLN(NFRQ),TXDEG(NFRQ),CONFIG(NFRQ),CFG1(NFRQ))

     ZRX = 0. ; XRX = 0. ; YRX = 0. ; FREQ = 0. ; TXCLN = 0. ; PRM_FD = 0.

     DO JF = 1,NFRQ
       READ(NR,*) FREQ(JF),ZRX(JF),XRX(JF),YRX(JF),TXCLN(JF)    ! RECORD 4 (Frequency Domain)
     END DO

     CALL CONFIG_ID (NFRQ,TXCLN,txa90, XRX,YRX,ZRX,CONFIG,CFG1)

     A1 = 0.
     DO JF = 1,NFRQ
       IF (CMP < 2) THEN
         WRITE(NW,21) JF,FREQ(JF),TXCLN(JF),A1,ZRX(JF),XRX(JF),YRX(JF),CONFIG(JF)
       ELSE
         WRITE(NW,21) JF,FREQ(JF),TXCLN(JF),A1,ZRX(JF),XRX(JF),YRX(JF)
       END IF
     END DO
   END IF

! Flight path details for forward modelling only. Convert TXCLN to radians.

   IF (INVERT) THEN

     READ(NR,*) NSTAT         ! RECORD 9
     IF (NSTAT > 1) THEN
       DO JF = 1,NSTAT
         READ(NR,'(A)') INP
       END DO
     END IF

   ELSE

     READ(NDR,*)  NSTAT,SURVEY,BAROMTRC
     WRITE(NW,22) NSTAT,SURVEY,BAROMTRC

     IF (NSTAT < 2) CALL WRITE_LOG_FILE (NLG,15,MXERR,2)
     IF (TDFD == 2 .AND. SURVEY /= 2) CALL WRITE_LOG_FILE (NLG,16,MXERR,2)
     IF (TDFD < 2 .AND. SURVEY /= 2 .AND. SURVEY /= 3) CALL WRITE_LOG_FILE (NLG,17,MXERR,2)

! Read Tx and Rx positions.
! NRXST is used to dimension the transmitter-receiver offsets and transmitter
! orientation. For time-domain systems, the offset is constant with frequency
! but can vary with station => NRXST = NSTAT

! With frequency-domain systems, the offset is constant along the survey but
! can vary with frequency => NRXST = NFRQ

! NRX is used to dimension absolute receiver locations as a function of
! frequency, so NRX = NFRQ for frequency-domain systems but
! NRX = 1 for time-domain systems

     IF (TDFD < 2) THEN
       NRXST = NSTAT
       ALLOCATE (ZRX(NSTAT),XRX(NSTAT),YRX(NSTAT),TXCLN(NSTAT),TXDEG(NSTAT))
       TXCLN = TXCLN0; ZRX = ZRX0; XRX = XRX0; YRX = YRX0;
     END IF

     ALLOCATE (FNGL(NSTAT),SXD(NSTAT),SYD(NSTAT),SZ(NSTAT),RX(NSTAT,NRX), &
               RY(NSTAT,NRX),RZ(NSTAT,NRX),RXD(NSTAT,NRX),RYD(NSTAT,NRX))

     FNGL = 0. ; SXD = 0. ; SYD = 0. ; SZ = 0. ; RX = 0. ; RY = 0. ; RZ = 0.
     RXD = 0. ; RYD = 0.

     ! Read in course for forward modelling only.

! RECORD 9.J
! ----------

     DO JS = 1, NSTAT
       IF (SURVEY == 2) THEN
         READ(NDR,*) ALINE,SXD(JS),SYD(JS),SZ(JS)
       ELSE IF (SURVEY == 3) THEN
         READ(NDR,*) ALINE,SXD(JS),SYD(JS),SZ(JS),TXCLN(JS),ZRX(JS),XRX(JS),YRX(JS)
       END IF
       LINE = FLOOR (ALINE)
       IF (JS > 1) THEN
         DELX = REAL (SXD(JS) - SXD(JS-1))
         DELY = REAL (SYD(JS) - SYD(JS-1))
         RHO = SQRT (DELX**2 + DELY**2)
         IF (RHO > 0.01) FNGL(JS) = ATAN2 (DELY,DELX)
       END IF
     END DO

     FNGL(1) = FNGL(2)
     IFL = 0
     DO JS = 2, NSTAT-1
       IF (ABS (FNGL(JS+1) - FNGL(JS)) > TURN) IFL = 1
     END DO
     FANGLE = 0.
     IF (IFL == 1) THEN
       CALL WRITE_LOG_FILE(NLG,18,MXERR,2)
     ELSE
       FANGLE = SUM (FNGL) / REAL (NSTAT)
     END IF
     DEALLOCATE (FNGL)

     TXDEG = TXCLN
     TXCLN = TXCLN * PI / 180.

     IF (TDFD < 2) THEN
       WRITE(NW,25) NSTAT
       DO  JS = 1, NSTAT
         WRITE(NW,26) LINE,JS,SXD(JS),SYD(JS),SZ(JS),TXdeg(JS),ZRX(JS),XRX(JS),YRX(JS)
       END DO
     ELSE
       WRITE(NW,27) NSTAT
       DO JS = 1, NSTAT
         WRITE(NW,28) LINE,JS,SXD(JS),SYD(JS),SZ(JS)
       END DO
     END IF

     If ((cos(fangle) + 1.0) .le. tiny(1.0)) Then
         sxd = -1.0 * sxd(nstat:1:-1)
         sz(1: nstat) = sz(nstat:1:-1)
         Write (*,  29)
         Write (nw, 29); Write (nw, 30); WRITE(NW, 25) NSTAT
         Do js = 1, nstat
             Write (nw, 26) line, js, sxd(js), syd(js), sz(js), txcln(js), &
                                      zrx(js), xrx(js), yrx(js)
         End Do
     End If

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
 3 FORMAT(/T3,'TDFD =',I3,3X,'DO3D =',I3,3X,'PRFL =',I3,4X,'ISTOP =',I2)
 4 FORMAT(/T3,'ISW =',I4,T15,'NSX =',I4,T27,'STEP =',I2,T39,'UNITS =',I4,T52,'NCHNL =',I4, &
         /T3,'KRXW =',I3,T15,'OFFTYM =',G12.4)
 5 FORMAT(//T10,'+-----------------------------------------+' &
           /T10,'+   Airborne System Information           +' &
           /T10,'+   100 Percent Duty Cycle STEP Response  +' &
           /T10,'+   for Rectangular Waveform              +' &
           /T10,'+   B output will be in ',A,'             +' &
           /T10,'+-----------------------------------------+')
 6 FORMAT(//T10,'+-----------------------------------------+' &
           /T10,'+   Airborne System Information           +' &
           /T10,'+   100 Percent Duty Cycle Response       +' &
           /T10,'+   for Rectangular Waveform              +' &
           /T10,'+   dB/dt output will be in ',A,'         +' &
           /T10,'+-----------------------------------------+')
 7 FORMAT(/10X,'+------------------------------------------------+' &
          /10X,'+  Frequency-Domain Airborne System Information  +' &
          /10X,'+------------------------------------------------+')
 8 FORMAT(/T3,'System orientation = vertical coplanar broadside')
 9 FORMAT(//T10,'+----------------------------------------------+' &
           /T10,'+    Time-Domain AEM Impulse System Input Data +' &
           /T10,'+          dB/dt output will be in ',A,'       +' &
           /T10,'+----------------------------------------------+')
 10 FORMAT(//T10,'+---------------------------------------------+' &
            /T10,'+    Time-Domain AEM Step System Input Data   +' &
            /T10,'+        B output will be in ',A,'            +' &
            /T10,'+---------------------------------------------+')
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
           /T3,'Data will be expressed in ',A, &
          //T3,'Frequencies, Tx Angles and Receiver Offset(s)' &
          //T6,'Frequency  TXCLN  TXAZM   ZRX   XRX   YRX   CONFIG' &
           /T6,'---------  -----  -----   ---   ---   ---   ------')
 20 FORMAT(T3,'KLO =',I3,T15,'KHI =',I3,T27,'KMD =',I3,T40,'PPD1 =',I2,3X,'PPD2 =',I2)
 21 FORMAT(I3,F9.0,F8.0,F7.0,F7.1,2F6.1,T51,A)
 22 FORMAT(//T3,'NSTAT =',I4,3X,'SURVEY =',I2,3X,'BAROMTRC =',I2)
 24 FORMAT(/T3,'NFRQ =',I3,';  CMP =',I2,';  NPPF =',I2 &
          //T3,'Frequencies, Tx Angles and Receiver Offset(s)' &
          //T6,'Frequency  TXCLN  TXAZM   ZRX   XRX   YRX   CONFIG' &
           /T6,'---------  -----  -----   ---   ---   ---   ------')
 25 FORMAT(/T7,I3,' transmitter positions along the flight path' &
          //T6,'Line   Stat     East       North       Alt         Pitch    ZRX      XRX     YRX' &
           /T6,'----   ----      ----       -----       ---     -----      ---    ---      ---')
 26 FORMAT(T1,I9,I7,2F12.1,2F10.1,F9.1,3F8.1)
 27 FORMAT(/T7,I3,' transmitter positions along the flight path' &
          //T6,'Line   Stat       East        North       Alt' &
           /T6,'----   ----       ----        -----       ---')
 28 FORMAT(T1,I9,I6,2X,2F12.1,2F10.1)
 29 Format (/, 2x, '-----------------------------------------------------------', &
            /, 2x, 'The flight line has been calculated at running East to West', &
            /, 2x, 'In order for ArjunAir to work with this input file:', &
            /, 2x, ' * the flight line will be reversed and', &
            /, 2x, ' * the mesh will be reflected about the origin.', &
            /, 2x, '-----------------------------------------------------------')
 30 Format (/, 2x, 'The new survey is : ')            
 41 FORMAT(/T3,'Geotem / Questem stripping algorithm will be applied to computations.')
 42 FORMAT(/T3,'Geotem / Questem stripping algorithm is not applied for B field output.')
 43 FORMAT(/T3,'Geotem / Questem stripping algorithm is not applied for B field input.')
 44 FORMAT(/T3,'Geotem / Questem stripping algorithm is not applied for ISW = 1 or 4.')

   END SUBROUTINE READ_SYSTEM_AND_SURVEY

!-----------------------------------!
! MODULE: INPUT_DATA_FOR_ARJUNA_AIR !
!-----------------------------------!

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
 3 FORMAT(/T3,'ArjunAir will compute time-domain responses from the' &
          /T3,'frequency-domain data contained in file ArjunAir.frq')

   END SUBROUTINE SET_FRQ

   !-----------------------------------!
   ! MODULE: INPUT_DATA_FOR_ARJUNA_AIR !
   !-----------------------------------!

   SUBROUTINE SET_TRP
!  ------------------

!*** Called by: MAIN
!
!  Sets up interpolation times for FD to TD transformations which use the
!  exact 6 points per decade frequency-domain data plus 6 per decade
!  interpolated values. These are based on a 12 point per decade cosine
!  filter derived from the Niels Christensen routine FILCOA with
!  OMEGA = 0.3 * PI and shift = 0.
!
!  OUTPUT:
!
!   TRP    = Array of time values for FD to TD transformations.
!   NTYRP  = Number of values in TRP.
!   EXTENT = The latest time for which time-domain output is required.
!   PULSE  = Time length of one signal pulse.
!   NTYPLS = Number of TRP values in one (1) PULSE.

   REAL, PARAMETER :: TWOPI = 6.2831853
   INTEGER MXTYM,J1
   REAL T0,EXTENT
   REAL,ALLOCATABLE :: QQQ(:)
   DOUBLE PRECISION TBASE,QTYM,TQ

   MXTYM = 200
   ALLOCATE (QQQ(MXTYM))
   QQQ = 0.

   QTYM = LOG(10.D0)/12.D0
   QTYM = EXP(QTYM)
   EXTENT = 2.0 * NPULS * PULSE

   T0 = MINVAL (TOPN) - SWX(NSX)
   T0 = MAX (T0, T0_MIN)
   TBASE = 1.D0 / REAL (TWOPI,8)
   DO J1 = 1,MXTYM
     IF (TBASE < T0) EXIT
     TBASE = TBASE / QTYM
   END DO

   TQ = TBASE
   QQQ(1) = REAL(TQ)
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

!-----------------------------------!
! MODULE: INPUT_DATA_FOR_ARJUNA_AIR !
!-----------------------------------!

   SUBROUTINE READ_MODEL_DATA
!  --------------------------

!***  Called by MAIN
!
!  Read in the nodes in double precision.
!  Remember to change the vertical sign.
!  Define XCNTRD as the leftmost surface node defined by the user.
!  LITH is set up as LITH (i.e., in iz).

   INTEGER, PARAMETER :: NXL = 2, NXR = 2, NZB = 2
   INTEGER TOT_NODES,JX,JZ,UR,UL,UT,UB,I,K,L,J1,J,KLITH
   REAL CSF,SNF,N2,E2,Z1, xmax
   REAL(KIND=QL) N1,E1
   Real, Parameter :: pi = 3.14159265359

   READ_RES_FILE = .FALSE.
   WRITE(NW,1)
   READ(NR,*)  NZM, NXM, NLITH, EASTD1, NORTHD1     ! RECORD 10
   IF (NLITH < 0) THEN
     READ_RES_FILE = .TRUE.  ! Read ArjunAir.res during inversion.
     NLITH = ABS(NLITH)
   END IF
   WRITE(NW,2) NZM, NXM, NLITH, EASTD1, NORTHD1

! Initialise lithology lists:

   ALLOCATE (LYTH(NLITH+1,NPROP))
   WRITE(NW,3)
   WRITE(np,8) NLITH
   DO J = 1,NLITH
     READ (NR,*) LYTH(J,1:NPROP)                                     ! RECORD 11
     WRITE(NW,'(I4,T8,G12.4,T22,F7.1,F12.3,F11.3,F10.2,G12.3,F8.2)') J,LYTH(J,1:NPROP)
     WRITE(np,9) J,LYTH(J,1:NPROP)
     IF ( LYTH(J,3) < 0.01 ) LYTH(J,3) = 1.  ! Default RMU.
     IF ( LYTH(J,4) < 0.01 ) LYTH(J,4) = 1.  ! Default REPS.
     IF ( LYTH(J,5) < 1.E-3 .OR. LYTH(J,6) < 1.E-15 .OR. LYTH(J,7) < 1.E-6 ) THEN
       LYTH(J,5) = 0   ! default CHRG
       LYTH(J,6) = 0   ! default CTAU
       LYTH(J,7) = 1   ! default CFRQ
     END IF
   END DO

   LYTH(NLITH+1,1) = 1.E10 ! Air resistivity indicator.
   LYTH(NLITH+1,2) = -1.   ! Air conductance (SIG_T) indicator.
   LYTH(NLITH+1,3) = 1.    ! Air Relative magnetic permeabilities.
   LYTH(NLITH+1,4) = 1.    ! Air Relative dielectric constants.
   LYTH(NLITH+1,5) = 0.    ! Air Chargeabilities.
   LYTH(NLITH+1,6) = 0.    ! Air CTAUs.
   LYTH(NLITH+1,7) = 1.    ! Air CFREQs.

! The user interface will read in location indices (K,J) ordered
! as VERTICAL and then HORIZONTAL. Arjuna will refer to these as JZ and JX.

! X = horizontal; Z = down.

   TOT_NODES = NXM * NZM
   IF (TOT_NODES > 60000) CALL WRITE_LOG_FILE (NLG,50,MXERR,1)

   NX  = NXL  + NXM + NXR        ! Number of nodes in horizontal direction.
   NZ  = NAIR + NZM + NZB        ! Number of nodes in vertical direction.
   NXZ = (NX-1) * (NZ-1)         ! Number of elements.
   NPN = (2*NX-1)*(2*NZ-1) - NXZ ! Number of nodes.
   NPM = (NX-1)*(NZ-NAIR-1)      ! Number of non-air elements.

   NPAR = (NX-5)*(NZ-NAIR-3)     ! Number of elements in 2.5D finite element mesh.

   ALLOCATE (LITHD(NX,NZ),LITHM(NXM,NZM),XLOC(NX,NZ),ZLOC(NX,NZ),COORD(NPN,2), &
             LNODS(NXZ,8),ZSURF(NXM),XSURF(NXM),XLOCD(NX,NZ),XSURFD(NXM))

   XLOC = 0.
   XLOCD = 0.
   ZLOC = 0.
   LITHD = NLITH + 1

   DO J1 = 1, TOT_NODES

     READ (NR,*) JZ,JX,ZLOC(NXL+JX,NAIR+JZ),XLOCD(NXL+JX,NAIR+JZ),LITHM(JX,JZ)       ! RECORD 12

     LITHD(NXL+JX,NAIR+JZ) = LITHM(JX,JZ)

     IF ( JZ == 1 ) THEN                 ! Set up surface node array.
       XSURFD(JX) = XLOCD(NXL+JX,NAIR+1)
       ZSURF(JX)  = ZLOC(NXL+JX,NAIR+1)  ! Keep positive up convention for use in FIND_TX_RX.
     END IF

   END DO ! End TOT_NODES loop.

   !
   ! Do we need to flip the mesh?   
   If ((Cos(fangle) + 1.0) .le. Tiny(1.0)) Then
       xsurfd = -1.0 * xsurfd
       xsurfd(1:nxm) = xsurfd(nxm:1:-1)
       Do jz = 1, nz
           xlocd(1:nx, jz) = -1.0 * xlocd(nx:1:-1, jz)
       End Do
       Eastd1 = minval(xsurfd)
       fangle = fangle - pi
   End If
   
 
!  Transform to single precision computation coordinates with origin at leftmost
!  top node.  The source and receiver arrays will be revised such that within
!  the limits of the survey as originally specified, a receiver position will be
!  created over each surface node with a corresponding displacement of the
!  transmitter position.

   XCNTRD = MINVAL (XSURFD)
   XLOC  = REAL (XLOCD - XCNTRD)
   XSURF = REAL (XSURFD - XCNTRD)
   ZLOC  = -ZLOC

! Write mesh to the ArjunAir.mv1 or ArjunAir.mf1 header.

   CSF = COS (FANGLE)
   SNF = SIN (FANGLE)

   WRITE(np,5) TOT_NODES,NXM,NZM

   DO JX = 1, NXM
     DO JZ = 1, NZM
       N2 = SNF * (REAL(XLOCD(NXL+JX,NAIR+JZ) - XCNTRD))
       E2 = CSF * (REAL(XLOCD(NXL+JX,NAIR+JZ) - XCNTRD))
       N1 = REAL(N2,QL) + NORTHD1
       E1 = REAL(E2,QL) + EASTD1
       Z1 = ZLOC(NXL+JX,NAIR+JZ)
       WRITE (np,6) JZ,JX,E1,N1,Z1,LITHD(NXL+JX,NAIR+JZ)
     END DO
   END DO

! Identify source and receiver positions which are either subsurface or not
! over the user mesh. Adjust altitudes for draped surveys.

   CALL LITHMAP (NW,NZM,NXM,LITHM)
   Write (nw, 10)
   Write (nw, 11) xsurfd

! Extend the mesh and the resistivities to the left, right, top and bottom.
! Define global indices for user specified mesh

   UL = NXL + 1          ! left limit of user-defined mesh
   UR = NXL + NXM        ! right limit of user-defined mesh
   UT = NAIR + 1         ! top limit of user-defined mesh
   UB = NAIR + NZM       ! bottom limit of user-defined mesh

! Define the left and right limits outside the user mesh. These will be two
! columns of width 1000 and 10,000 either side. The Z coordinates for the
! exensions will be the same at those of the user defined edges.

   XLOC(UL-1,UT:UB) = XLOC(UL,UT:UB) -  1000.
   XLOC(UL-2,UT:UB) = XLOC(UL,UT:UB) - 11000.
   XLOC(UR+1,UT:UB) = XLOC(UR,UT:UB) +  1000.
   XLOC(UR+2,UT:UB) = XLOC(UR,UT:UB) + 11000.

   ZLOC(UL-1,UT:UB) = ZLOC(UL,UT:UB)
   ZLOC(UL-2,UT:UB) = ZLOC(UL,UT:UB)
   ZLOC(UR+1,UT:UB) = ZLOC(UR,UT:UB)
   ZLOC(UR+2,UT:UB) = ZLOC(UR,UT:UB)
   
! Define the node locations 2 lines below the user defined mesh
! he air node locations will be defined by calling SET_MESH_COMPLETION
! after the pseudo survey coordinates have been defined.

   XLOC(1:NX,UB+1) = XLOC(1:NX,UB)
   XLOC(1:NX,UB+2) = XLOC(1:NX,UB)
   ZLOC(1:NX,UB+1) = ZLOC(1:NX,UB) +  1000.
   ZLOC(1:NX,UB+2) = ZLOC(1:NX,UB) + 11000.
  

   DO JZ = UT, UB-1
     LITHD(1:NXL,JZ) = LITHD(UL,JZ)
     LITHD(UR:UR+NXR-1,JZ) = LITHD(UR-1,JZ)
   END DO

   LITHD(1:NX-1,UB)   = LITHD(1:NX-1,UB-1)
   LITHD(1:NX-1,UB+1) = LITHD(1:NX-1,UB-1)

   ! Assign element numbering:

   L = 0
   K = 1
   DO JX = 1, NX-1
     DO JZ = 1, NZ-1
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

! In an element, the following node numbering is used:
!
!      (1)---(2)---(3)
!       |           |
!      (8)         (4)
!       |           |
!      (7)---(6)---(5)

   JZ = 0
   DO I = 1, (NX-1)*(NZ-1)
     LNODS(I,2) = LNODS(I,1) + (2*NZ-1) - JZ
     LNODS(I,6) = LNODS(I,2) + 1
     JZ = JZ + 1
     IF (MOD(I,NZ-1) == 0)   JZ = 0
   END DO

   ALLOCATE (GEOELEC_PAR(NXZ,6)) ; GEOELEC_PAR = 0.

   DO I = 1, NX - 1
     DO J = 1, NZ - 1
       KLITH = LITHD(I,J)
       J1 = (I-1)*(NZ-1) + J
       GEOELEC_PAR(J1,1) = LYTH(KLITH,1) ! Resistivity
       GEOELEC_PAR(J1,2) = LYTH(KLITH,3) ! Permeability
       GEOELEC_PAR(J1,3) = LYTH(KLITH,4) ! Permittivity
       GEOELEC_PAR(J1,4) = LYTH(KLITH,5) ! CHRB
       GEOELEC_PAR(J1,5) = LYTH(KLITH,6) ! CTAU
       GEOELEC_PAR(J1,6) = LYTH(KLITH,7) ! CFRQ
     END DO
   END DO

 1 FORMAT(//T6,'LITHOLOGY AND MESH DESCRIPTION' &
           /T6,'------------------------------')
 2 FORMAT(/, 3x, 'Number of node rows    =',I4, &    
          /, 3x, 'Number of node columns =',I4, &
          /, 3x, 'Number of lithologies  =',I3, &
          //, 3x, 'Mesh reference point', &
          /, 3x, 'East =  ', g13.6 &
          /, 3x, 'North = ', g13.6)
 3 FORMAT(//T27,'LITHOLOGY PROPERTIES' &
           /T27,'--------------------' &
          //T35,'Relative   Relative     Cole-Cole Parameters'    &
           /T9,'Resistivity  Conductance     MU     Dielectric   CHRG    CTAU       CFREQ'/)
 5 FORMAT( T1,'/', &
          /t1, '/ TOTAL_NODES=',I4.4,' NX=',I3.3,' NZ=',I3.3, &
          /T1,'/    JZ',4x,'JX',9x,'EASTD        NORTHD          ZLOC    LITH')
 6 Format ('/', 2(2x, i4), 3(2x, f12.4), 2x, i6)
 8 FORMAT( T1,'/', &
          /t1, '/ NLITH=',I2.2, &
          /T1,'/ LITH    RES           Cond        MU         EPS       CHRG    CTAU        CFREQ')
 9 FORMAT(T1,'/',I4,T8,G12.4,T22,F7.1,F12.3,F11.3,F10.2,G12.3,F8.2)
10 Format (//, 'Mesh surface nodes at Eastings')
11 Format (2048(2x, g12.4))
15 Format (2(i4, 2x), 2(2x, en15.6))

   END SUBROUTINE READ_MODEL_DATA

!-----------------------------------
! MODULE: INPUT_DATA_FOR_ARJUNA_AIR !
!-----------------------------------!

   SUBROUTINE LITHMAP (NW,NZ,NX,LITHM)
!  -----------------------------------
!
!*** Called by: READ_2D_DATA
!
!  Produces a 2D character map of lithologies.
!
!  NX and NZ are the number of horizontal and vertical cells respectively.
!
!  LITH(J,I) = lithology associated with node in column I and row J.
!
!------------------------------------------------------------------------

   IMPLICIT NONE
   INTEGER NW,NZ,NX,I,J,LITHM(NX,NZ)
   CHARACTER (LEN=1) LITHCHAR(52), MAP(NX,NZ)
   DATA LITHCHAR /'1','2','3','4','5','6','7','8','9','0','A','B','C','D','E','F', &
                  'G','H','J','K','L','M','N','P','R','S','T','U','W','X','Y','a', &
                  'b','c','d','e','f','g','h','j','k','l','m','n','p','r','s','t', &
                  'u','w','x','y'/

   WRITE(NW,'(/T15,A)') '2D LITHOLOGY MAP'
   WRITE(NW,'(T15,A)')  '----------------'
   WRITE(NW,'(T10,A/)') '(reduced to rectangular grid)'

   MAP = '_'
   DO I = 1, NZ-1
     DO J = 1, NX-1
       MAP(J,I) = LITHCHAR(LITHM(J,I))
     END DO
     WRITE(NW,'(2X,2048A)') MAP(1:NX-1,I)
   END DO

   END SUBROUTINE LITHMAP

!------------------------------------------------------------------------
! MODULE: INPUT_DATA_FOR_ARJUNA_AIR !
!-----------------------------------!

   SUBROUTINE READ_INVRT_CNTRL
!  ---------------------------
!
!***  Called by MAIN
!
!  Set inversion controls.
!
!--------------------------------------------

   INTEGER NFIX,JP,LITH_INDX,CTYPE,KPAR,J1
   REAL E1,E2,E3,A1,A2,RDUM

   READ(NR,*) RDUM,CNVRG,NFIX,MV1PRT,OUTPRT        ! RECORD 13
   MAXITS = FLOOR(RDUM)
   WRITE(NW,10) MAXITS,CNVRG,NFIX,MV1PRT,OUTPRT

   IF (MV1PRT < 0 .OR. MV1PRT > 3) MV1PRT = 1
   IF (OUTPRT < 0 .OR. OUTPRT > 3) OUTPRT = 1

   IF (CNVRG /= 1 .AND. CNVRG /= 2) THEN
     CNVRG = 1
     CALL WRITE_LOG_FILE (NLG,201,MXERR,1)
   END IF

   PCTCNV = 1.
   IF (CNVRG == 2) READ(NR,*) PCTCNV            ! RECORD 13.1
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

! E1 - Elasticity
! E2 - Lower bound on resistivity.
! E3 - Upper bound on resistivity.

   IF (NFIX > 0) THEN
     WRITE(NW,17)
     DO JP = 1, NFIX
       E1 = 1.              ! Default values:
       E2 = 1.E-8
       E3 = 1.E+8

       READ(NR,*) CTYPE     ! RECORD 14
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

 10 FORMAT(//T3, '-------------------------------------------------------------------', &
            /T3, 'Inversion control', &
            /T3,'MAXITS =',I3,3X,'CNVRG =',I2,3X,'NFIX =',I3,3X,'MV1PRT =',I2,3X,'OUTPRT =',I2)
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

!-----------------------------------
! MODULE: INPUT_DATA_FOR_ARJUNA_AIR !
!-----------------------------------!

   SUBROUTINE READ_INVRT_DATA
!  --------------------------
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
   Logical :: isComment
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
       WRITE(*,105)
       WRITE(NW,105) ! Write ERROR comment - 3 component inversion not currently available in ArjunAir.
       STOP          ! Exit ArjunAir.
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

   ! Start reading from ArjunAir.inv on UNIT NRI = 13. First, skip over
   ! all comment lines.

   DO
     READ (NRI,'(A)') TCHR
     If (.not.(isComment(tchr))) exit
     ! IF (TCHR /= '\' .AND. TCHR /= '/') EXIT
   END DO
   BACKSPACE (NRI)

   IF (TDFD < 2) THEN
     READ(NRI,*)  NSTAT, SURVEY, BAROMTRC, KCMP(1), ORDER    ! RECORD 1

     WRITE(NW,21) NSTAT, SURVEY, BAROMTRC, KCMP(1), ORDER
     SELECT CASE (CMP)
     CASE (11)
       IF (KCMP(1) == 3) CALL WRITE_LOG_FILE (NLG,202,MXERR,2)
     CASE (13)
       IF (KCMP(1) == 1) CALL WRITE_LOG_FILE (NLG,203,MXERR,2)
     CASE (2)
       IF (KCMP(1) == 1) CALL WRITE_LOG_FILE (NLG,204,MXERR,2)
       IF (KCMP(1) == 3) CALL WRITE_LOG_FILE (NLG,205,MXERR,2)
     CASE (3)
       IF (KCMP(1) == 1) CALL WRITE_LOG_FILE (NLG,206,MXERR,2)
       IF (KCMP(1) == 3) CALL WRITE_LOG_FILE (NLG,207,MXERR,2)
       IF (KCMP(1) > 4 .AND. KCMP(1) < 100) CALL WRITE_LOG_FILE (NLG,208,MXERR,2)
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
         CALL WRITE_LOG_FILE (NLG,209,MXERR,2)
         WRITE(NLG,45) KCMP(KCMP(1:NFRQ))
         WRITE(NLG,46) KCMPC(KCMP(1:NFRQ))
         WRITE(NLG,47) CFG1(1:NFRQ)
         WRITE(NLG,46) KCMPC(CFG1(1:NFRQ))
         EXIT
       END IF
     END DO
   END IF

   IF (NSTAT < 2) CALL WRITE_LOG_FILE (NLG,15,MXERR,2)
   IF (TDFD == 2 .AND. SURVEY /= 2) CALL WRITE_LOG_FILE (NLG,16,MXERR,2)
   IF (TDFD < 2 .AND. SURVEY /= 2 .AND. SURVEY /= 3) CALL WRITE_LOG_FILE (NLG,17,MXERR,2)

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
     TXCLN = TXCLN0 ; ZRX = ZRX0 ; XRX = XRX0 ; YRX = YRX0
   END IF

   ALLOCATE (FNGL(NSTAT),SXD(NSTAT),SYD(NSTAT),SZ(NSTAT),RX(NSTAT,NRX), &
             RY(NSTAT,NRX),RZ(NSTAT,NRX),RXD(NSTAT,NRX),RYD(NSTAT,NRX))

   RX = 0. ; RY = 0. ; RZ = 0. ; RXD = 0. ; RYD = 0. ; FNGL = 0.

   NDATA = MCHNL * NSTAT

   IF (TDFD < 2) THEN

     IF (KCMP(1) /= 1 .AND. KCMP(1) /= 3 .AND. KCMP(1) /=13 .AND. KCMP(1) /=31 .AND. KCMP(1) /=123 &
                   .AND. KCMP(1) /=321) CALL WRITE_LOG_FILE (NLG,210,MXERR,2)
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

     READ(NRI,*) DATA_FLOOR(1)                        ! RECORD 2
     WRITE(NW,22) ABS (DATA_FLOOR(1)),TRIM (QUNIT)
     DATA_FLOOR = DATA_FLOOR(1)

     NDCMP = 3
     IF (KCMP(1) < 100) NDCMP = 2
     IF (KCMP(1) < 13) NDCMP = 1
     MDCHNL = NDCMP * NCHNL

   ELSE IF (TDFD == 2) THEN

     MDCHNL = NDATA
     READ(NRI,*) DATA_FLOOR(1:2*NFRQ)
     WRITE(NW,23) TRIM (QUNIT)
     DO JP = 1, NFRQ
       WRITE(NW,24) JP,FREQ(JP),ABS(DATA_FLOOR(JP)),ABS(DATA_FLOOR(JP+NFRQ))
     END DO

   END IF

   ALLOCATE (RDATA(MCHNL,NSTAT),RWTS(MCHNL,NSTAT),QDATA(MDCHNL),Q2DATA(MDCHNL))

   RDATA = 0. ; RWTS = 1

   READ(NRI,*)  N0STAT, N0CHNL, N0PTS   ! RECORD 3
   WRITE(NW,25) N0STAT, N0CHNL, N0PTS

   IF (N0STAT > 0) THEN
     ALLOCATE (K0STAT(N0STAT))

     READ(NRI,*) K0STAT(1:N0STAT)       ! RECORD 3.1
     WRITE(NW,26) K0STAT(1:N0STAT)
     DO J1 = 1, N0STAT
       RWTS(1:MCHNL,K0STAT(J1)) = 0
     END DO
     DEALLOCATE (K0STAT)
   END IF
   IF (N0CHNL > 0) THEN
     ALLOCATE (K0CHNL(N0CHNL))

     READ(NRI,*) K0CHNL(1:N0CHNL)      ! RECORD 3.2
     WRITE(NW,27) K0CHNL(1:N0CHNL)
     DO J1 = 1, N0CHNL
       RWTS(K0CHNL(J1),1:NSTAT) = 0
     END DO
     DEALLOCATE (K0CHNL)
   END IF

   IF (N0PTS > 0) THEN
     ALLOCATE (K0STAT(N0PTS),K0CHNL(N0PTS))

     READ(NRI,*) (K0CHNL(J1),K0STAT(J1), J1=1,N0PTS)   ! RECORD 3.3
     WRITE(NW,28)
     DO J1 = 1, N0PTS
       WRITE(NW,'(T3,2I4)') K0CHNL(J1),K0STAT(J1)
       RWTS(K0CHNL(J1),K0STAT(J1)) = 0
     END DO
     DEALLOCATE (K0STAT,K0CHNL)
   END IF
   IF ((N0STAT + N0CHNL + N0PTS) == 0) WRITE(NW,29)

! Data entry.

   DO JS = 1, NSTAT

     IF (SURVEY == 2) THEN
       READ(NRI,*) ALINE, SXD(JS), SYD(JS), SZ(JS), QDATA(1:MCHNL)         ! RECORD 4.J
     ELSE IF (SURVEY == 3) THEN
       READ(NRI,*) ALINE, SXD(JS), SYD(JS), SZ(JS), TXCLN(JS), ZRX(JS), XRX(JS), YRX(JS), QDATA(1:MCHNL)
     END IF
     LINE = FLOOR (ALINE)
     IF (JS > 1) THEN
       DELX = REAL (SXD(JS) - SXD(JS-1))
       DELY = REAL (SYD(JS) - SYD(JS-1))
       RHO = SQRT (DELX**2 + DELY**2)
       IF (RHO > 0.01) FNGL(JS) = ATAN2 (DELY,DELX)
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
         CALL WRITE_LOG_FILE (NLG,211,MXERR,2)
       END SELECT
     END IF
   END DO

! Frequency-domain TXDEG and TXCLN already established in READ_SYSTEM_AND_SURVEY
! for both forward modelling and inversion.

   TXDEG = TXCLN
   TXCLN = TXCLN * PI / 180.

   FNGL(1) = FNGL(2)
   IFL = 0
   DO JS = 2, NSTAT-1
     IF (ABS (FNGL(JS+1) - FNGL(JS)) > TURN) IFL = 1
   END DO
   FANGLE = 0.
   IF (IFL == 1) THEN
     CALL WRITE_LOG_FILE(NLG,18,MXERR,2)
   ELSE
     FANGLE = SUM (FNGL) / REAL (NSTAT)
   END IF

   DEALLOCATE (QDATA,Q2DATA)

! Write the data and weights in blocked format to make checking easier.

 IF (TDFD < 2) THEN
   IF (CMP == 11) THEN
     WRITE(NW,41) ! In-line component header.
   ELSE
     WRITE(NW,43) ! Vertical component header.
   END IF
   DO JS = 1,NSTAT
     WRITE(NW,40) LINE,JS,SXD(JS),SYD(JS),SZ(JS),RDATA(1:NCHNL,JS) ! First component
   END DO
   IF (CMP == 2 .OR. CMP == 3) THEN
     N2B = NCHNL+1;  N2E = 2*NCHNL
     WRITE(NW,41)
     DO JS = 1,NSTAT
       WRITE(NW,40) LINE,JS,SXD(JS),SYD(JS),SZ(JS),RDATA(N2B:N2E,JS) ! TD in-line data
     END DO
   END IF
   IF (CMP == 3) THEN
     N3B = 2*NCHNL+1;  N3E = 3*NCHNL
     WRITE(NW,42)
     DO JS = 1,NSTAT
       WRITE(NW,40) LINE,JS,SXD(JS),SYD(JS),SZ(JS),RDATA(N3B:N3E,JS) ! TD transverse data
     END DO
   END IF

   WRITE(NW,22) ABS (DATA_FLOOR(1)),TRIM (QUNIT)  !  Weights
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
     WRITE(NW,40) LINE,JS,FANGLE,SXD(JS),SYD(JS),SZ(JS),RDATA(1:2*NFRQ,JS)
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
 
 !
 ! Write out some station stats
 Write (nw, 70)
 Do js = 1, nstat
    Write (nw, 71) js, sxd(js), syd(js), sz(js)
 End Do
 Write (nw, 72) nstat, &
                minval(sxd), minval(syd), minval(sz), &
                maxval(sxd), maxval(syd), maxval(sz), &
                maxval(sxd) - minval(sxd), maxval(syd) - minval(syd), &
                maxval(sz) - minval(sz)

 DEALLOCATE (FNGL)

  1 FORMAT(//T3,'--------------------------------------' &
            /T3,'Inversion Controls & Data for ArjunAir' &
            /T3,'--------------------------------------')
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
 29 FORMAT(/T3,'All data have the weighting RWTS= 1')
 32 FORMAT(//T3,'NSTAT =',I4,3X,'SURVEY =',I2,3X,'BAROMTRC =',I2,3X,'ORDER = ',I4)
 33 FORMAT(/T3,'Inversion components:',20(I5,': ',A,F8.1))
 34 FORMAT(/T3,'Inversion components:',20(I5,': ',A,F10.1))
 45 FORMAT(/T3,'Components from ArjunAir.inv:',I4,20I5)
 46 FORMAT(/T32,20(2X,A))
 47 FORMAT(/T3,'Components from ArjunAir.cfl:',I4,20I5)
 40 FORMAT(I9,I7,3f11.2,1024en17.6)
 41 FORMAT(//T6,'Line   Station   East       North     Alt',T68,'Horizontal In-line Component Data' &
            /T6,'----   -------   ----       -----     ---',T68,'----------------------------------')
 42 FORMAT(//T6,'Line   Station   East       North     Alt',T68,'Horizontal Transverse Component Data' &
            /T6,'----   -------   ----       -----     ---',T68,'------------------------------------')
 43 FORMAT(//T6,'Line   Station   East       North     Alt',T68,'Vertical Component Data' &
            /T6,'----   -------   ----       -----     ---',T68,'-----------------------')
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
 70 Format (/, 2x, 'Stations', &
            /, 11x, 'East', 11x, 'North', 7x, 'Altitude')            
 71 Format (i5, 3(2x, g13.6))
 72 Format (/, 2x, 'Number of stations          : ', i4, &
            /, 2x, 'Minimum Station coordinates : ', 3(g13.6, 2x), &
            /, 2x, 'Maximum Station coordinates : ', 3(g13.6, 2x), &
            /, 2x, '                  Variation : ', 3(g13.6, 2x), &
            /, 2x, '----------------------------- ', /)
 101 FORMAT(T3,'Three cpomponent inversion has been specified (CMP = 3)' &
           /T3,'but three components are not read in: KCMP = ',I4)
 102 FORMAT(T3,'Two component inversion has been specified (CMP = 2)' &
           /T3,'but two components are not read in: KCMP = ',I4)
 103 FORMAT(T3,'Vertical component inversion has been specified (CMP = 13)' &
           /T3,'but this component is not read in: KCMP = ',I4)
 104 FORMAT(T3,'In-line component inversion has been specified (CMP = 11)' &
           /T3,'but this component is not read in: KCMP = ',I4)
 105 FORMAT(/T3,'INVERSION FAILUTRE: Joint inversion of time-domain three component data'  &
            /T3,'is not available in this version of ArjunAir. Remove the transverse data' &
            /T3,'so as to only invert for two component data and try again.')

   END SUBROUTINE READ_INVRT_DATA

!-----------------------------------
! MODULE: INPUT_DATA_FOR_ARJUNA_AIR !
!-----------------------------------!

   SUBROUTINE WRITE_np_HEADER

   WRITE(np,1) FVERS,Trim(PNAME),TRIM(TITLE)

 1 FORMAT(T1,'/ ',I4.4,T15,'File version number'/T1,'/ PROGRAM_NAME=',A/T1,'/ TITLE: ',A)

   END SUBROUTINE WRITE_np_HEADER

!------------------------------------
! MODULE: INPUT_DATA_FOR_ARJUNA_AIR !
!-----------------------------------!

   SUBROUTINE WRITE_np_SYS_DATA
!  -----------------------------

!***  Called by: MAIN
!
! Sets up the initial part of the output plotting file for inversion.
!
!------------------------------------------------------------------------

   INTEGER NCMP
   REAL VERR(MCHNL)
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
       IF (JS == 1) THEN
         WRITE(QL0,*) LINE
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
  4 FORMAT(T1,'/ TIMES(ms)=',T17,1024(2x, en15.6))
  5 FORMAT(T1,'/ CHNL_WDTH(ms)=',T17,1024(2x, en15.6))
  6 FORMAT(T1,'/ SURVEY=TD_AEM  PP=RX_POS')
  7 FORMAT(T1,'/ LINE_HEADER'/T1,'/ DatIndx  EastTx  NorthTx  AltTx  Txcln  EastRx  NorthRx  AltRx',350(A,I3.3))
  9 FORMAT(T1,I5,2F12.1,F8.1,F6.1,2F12.1,F8.1,1024(2x, en15.6:))
 10 FORMAT(T1,I5,2F12.1,F8.1,2F12.1,F8.1,1024(2x, en15.6:))
 11 FORMAT(T2,'Line ',A)
 13 FORMAT(T1,'/ UNITS=',A,3X,'NSTAT=',I3.3,3X,'NFRQ=',I2.2,3X,'NCMP=1')
 14 FORMAT(T1,'/ FREQS(Hz) =',60F13.2)
 16 FORMAT(T1,'/ SURVEY=FD_AEM  PP=RX_POS')
 17 FORMAT(T1,'/ LINE_HEADER'/T1,'/ DatIndx  EastTx  NorthTx  AltTx  EastRx  NorthRx  AltRx  ',100(A,I2.2))

   END SUBROUTINE WRITE_np_SYS_DATA

 END MODULE AA_Input_routines

!============================================================================

 PROGRAM MAIN
!------------

!*** Calls: FDREAD,ARJUNA_2D,SET_SOURCE,TDEM_3D,TXDRCT,TDEM_3D,WRITE_FD,
!           WRITE_TD,WRITE_LOG_FILE,NLSQ.
!*** Calls from AA_Input_routines: READ_SYSTEM_AND_DATA,
!                                                READ_MODEL_DATA,SET_TRP,SET_FRQ,
!                                                READ_INVERSION_CONTROLS,
!                                                WRITE_INVERSION_DATA.
!
! Main ArjunAir program for 2.5-D airborne electromagnetic modelling and
! inversion.
!
!----------------------------------------------------------------------------

 USE AA_Input_routines

 IMPLICIT NONE
 INTEGER IDER,KNRM,QQDT(8),QQHMS(2),NSTATQ,JMIN,JMAX
 REAL CMP_start,CMP_final,CMP_delta
 REAL, DIMENSION(:), ALLOCATABLE :: NORM,ZGC,TXCLNQ
 REAL, DIMENSION(:,:,:), ALLOCATABLE :: BTDQ
 COMPLEX,DIMENSION(:,:,:), ALLOCATABLE :: BFDQ
 LOGICAL INV_FAIL,WRT_np

 CALL READ_SYSTEM_AND_SURVEY

 CALL CPU_TIME (CMP_start)

 IF (INVERT) THEN
   OPEN(NRI,FILE = 'ArjunAir.inv',STATUS = 'OLD')
   OPEN(np,FILE = 'ArjunAir.mv1',STATUS = 'REPLACE')
   OPEN(ND,FILE = 'ArjunAir.frq',STATUS = 'REPLACE')
   OPEN(NS,FILE = 'ArjunAir.sty',STATUS = 'REPLACE')
   CALL READ_INVRT_DATA
 ELSE
   OPEN(np,FILE = 'ArjunAir.mf1',STATUS = 'REPLACE')
 END IF

 CALL WRITE_np_HEADER

 CALL READ_MODEL_DATA

 IF (INVERT) CALL READ_INVRT_CNTRL

 CALL SET_SURVEY_1 (TDFD,NORTHD1,EASTD1,NXM,XSURF,FANGLE,NSTAT,SXD,SYD,SZ,NRXST, &
                    NRX,XRX,YRX,ZRX,RX,RY,RZ,RXD,RYD,NSTATQ,JMIN,JMAX,IFL)

 IF (IFL == 0) THEN
   ALLOCATE (SXQ(NSTATQ),SZQ(NSTATQ),RXQ(NSTATQ,NRX),RYQ(NSTATQ,NRX),RZQ(NSTATQ,NRX), &
             ZGC(NSTATQ))
 ELSE
   ! Write (*, 89)
   CALL WRITE_LOG_FILE(NLG,19,MXERR,2)
   STOP
 END IF

 CALL SET_SURVEY_2 (NW,TDFD,BAROMTRC,NSTAT,NRXST,XRX,YRX,ZRX,NRX,RX,RY,RZ,NXM, &
                    JMIN,JMAX,XSURF,ZSURF,NSTATQ,RXQ,RYQ,RZQ,SXQ,SZQ,ZGC,IFL)

 IF (IFL == 1) CALL WRITE_LOG_FILE(NLG,20,MXERR,2)

! NSTATQ is the dimension of the user defined mesh. The computational mesh adds
! two rows on either side.

 IF (IFL == 0) THEN
   ALLOCATE (IREL(NRX,NSTATQ)) ; IREL = 0
   CALL SET_MESH_COMPLETION (NX,NZ,NXZ,NPN,NAIR,JMIN,NSTATQ,NRX,RXQ,RZQ,ZGC, &
                             XLOC,ZLOC,LNODS,COORD,IREL)
 END IF

 DEALLOCATE (XSURF,XSURFD,ZSURF,XLOC,ZLOC,LITHM)

 IF (TDFD < 2) NRXSTQ = NSTATQ
 IF (TDFD == 2) NRXSTQ = NFRQ
 ALLOCATE (TXCLNQ(NRXSTQ)) ; TXCLNQ = 0.
 CALL SET_TXCLN (TDFD,TXCLN,NRXST,TXCLNQ,NRXSTQ,RXQ,RX,NSTAT,NSTATQ,NRX)

 ! Determine the number of Rx components.

 IF (TDFD < 2) THEN
   IF (INVERT) THEN
     IF (CMP == 11) NCOMP = 1  ! X
     IF (CMP == 13) NCOMP = 1  ! Z
     IF (CMP ==  2) NCOMP = 2  ! Z, X
   ELSE
     NCOMP = 1
   END IF
 ELSE IF (TDFD == 2) THEN
   NCOMP = 1
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

 IF (TDFD < 2) THEN   ! Time-Domain

! For time-domain, set up frequencies and interpolation times.
! Call SET_SOURCE to compute dI/dt at the transmitter using the
! DC coupling if waveform at the receiver has been specified.
! Convert PRM_TD to the peak primary dB/dt in nT if the impulse
! response is output or B in pT for step response.
!
! SWY will be in amps/s  * Tx area * NTRN
!
! IDER = 0 => that current derivative must be computed: ISW = 1, 11 or 31
!      = 1 => that current derivative has specified through voltage
!             calibration: ISW = 10 or 30
!      = 4 => ISW = 4  (pure rectangular pulse)

   IDER = 0

   IF (ISW == 10 .OR. ISW == 30 .OR. ISW == 130) IDER = 1
   IF (ISW == 4) IDER = 4

   CALL SET_FRQ

   KP = 0
   CALL SET_TRP

   KNRM = 3
   ALLOCATE (NORM(KNRM)) ; NORM = 0.

   IF (ISW == 4) THEN
     NORM = 1.E6
   ELSE
     CALL DCPRM_TD (XRX0,YRX0,ZRX0,TXCLN0,TXAREA,PRM_TD)
     CALL SET_SOURCE (STEP,ISW,BFFAC,WAVEFORM,NSX,SWX,SWY,PRM_TD)
     IF (INVERT) CALL SET_NORM_TD (NW,BUNIT,BFFAC,KPPM,PUNIT,PPFAC,PRM_TD,NORM)
   END IF

   ALLOCATE (PRM_FD(NFRQ))

 ELSE

   KNRM = NFRQ
   ALLOCATE (NORM(KNRM)) ; NORM = 0.

   CALL DCPRM_FD (NFRQ,XRX,YRX,ZRX,TXCLN,PRM_FD,PPFAC,NORM)

 END IF

 WRT_np = .TRUE.
 IF (TDFD == 2 .AND. CMP > 1) WRT_np = .FALSE.
 IF (WRT_np) CALL WRITE_np_SYS_DATA

 IF (INVERT) THEN

!---------------------------------------------------------------------!
!                                                                     !
!                     ArjunAir Inversion                              !
!                                                                     !
!---------------------------------------------------------------------!

   INVERT_RX_DATA = .TRUE.

   CALL NLSQ (NM,ND,NS,NW,np,NLG,MV1PRT,OUTPRT,MAXITS,CNVRG,PCTCNV,NDATA, &
              RDATA,RWTS,DATA_FLOOR,NPAR,CXPAR,ELAS,LBND,UBND,TDFD,CMP, &
              STEP,IDER,NSX,SWX,SWY,NTYRP,TRP,NPULS,PULSE,NTYPLS,MCHNL, &
              NCHNL,TOPN,TCLS,GSTRP,ASTRP,NFRQ,FREQ,TXCLN,TXCLNQ,NRX, &
              NRXST,NRXSTQ,NSTAT,NSTATQ,SXQ,SZQ,RX,RZ,RXQ,RYQ,RZQ, &
              INV_FAIL,NX,NZ,NAIR,NXZ,NPM,NPN,IREL,LNODS,LITHD,COORD, &
              KNRM,NORM,BFFAC,PPFAC,KPPM,PRM_FD,PRM_TD,SXD,SYD,SZ,RXD, &
              RYD,LINE,GEOELEC_PAR,NLITH,WRITE_FWD,NCOMP,DO3D, &
              INVERT_RX_DATA,READ_RES_FILE)

 ELSE

!---------------------------------------------------------------------!
!                                                                     !
!                     ArjunAir Forward Modelling                      !
!                                                                     !
!---------------------------------------------------------------------!

   CLOSE (NR)
   IF (DO3D == 1) THEN ! Compute the 2.5D responses using the Arjuna algorithm.

     ! WRITE(NW,1)

     IF (READ_RES_FILE) THEN
       OPEN(NM,FILE='ArjunAir.res',STATUS='OLD')
       DO I = 3, NX - 3
         DO J = NAIR+1, NZ - 3
           JC = (I-1)*(NZ-1) + J
           READ(NM,*) JP, S1
           GEOELEC_PAR(JC,1) = S1
         END DO
       END DO
       CLOSE(NM)
     END IF

     RESID = .TRUE. ; JCBN = .FALSE. ; WRITE_FWD = .TRUE.

     CALL ARJUNA_2D (NLG,ND,NS,NX,NZ,NAIR,NXZ,NPN,NPM,LNODS,COORD, &
                     GEOELEC_PAR,NCOMP,NSTATQ,SXQ,SZQ,RXQ,RYQ,RZQ,IREL, &
                     NFRQ,FREQ,RESID,JCBN,TXCLNQ,NRXSTQ,NPAR,NRX,CMP,TDFD, &
                     WRITE_FWD)

   END IF

! If DO3D = 2, then read frequency-domain results from ArjunAir.frq file.
! Read frequency-domain data from ArjunAir output.

   ALLOCATE (BFDQ(NFRQ,NSTATQ,3)) ; BFDQ = (0.,0.)

   CALL FDREAD (ND,NFRQ,NSTATQ,BFDQ)

   CLOSE (ND)

   IF (TDFD < 2) THEN ! Write out the time-domain results.

     ALLOCATE (BTDQ(NCHNL,NSTATQ,3)) ; BTDQ = 0.

     CALL TDEM_3D (STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL, &
                   TOPN,TCLS,FREQ,NFRQ,NSTATQ,BFDQ,GSTRP,ASTRP,BTDQ)

     DEALLOCATE (BFDQ)

     CALL WRITE_TD (NW,np,TITLE,NSTAT,NSTATQ,LINE,SXD,SYD,SZ,TXDEG,RXD,RYD,RZ, &
                    RX,RXQ,NRX,XRX,YRX,ZRX,NCHNL,TMS,PRFL,QUNIT,BUNIT,PPFAC,BFFAC, &
                    PRM_TD,CMP,KPPM,BTDQ)

     DEALLOCATE (BTDQ)

   ELSE ! Write out the frequency-domain results.

     CALL WRITE_FD (NW,np,TITLE,NSTAT,NSTATQ,LINE,TXCLN,SXD,SYD,SZ,RXD,RYD,RZ, &
                    RX,RXQ,NRX,CONFIG,NFRQ,FREQ,PRFL,QUNIT,PPFAC,BFFAC,PRM_FD,CMP, &
                    BFDQ)

     DEALLOCATE (BFDQ)

   END IF ! End of AjurnAir forward modelling.

 END IF

!---------------------------------------------------------------------!
!                                                                     !
!                        End of Modelling Loop                        !
!                                                                     !
!---------------------------------------------------------------------!

 !
 ! complete run time calcs & sign off ...
 call date_and_time(Values = tvals)
 call CPU_time(CMP_final)
 CMP_delta = CMP_final - CMP_start

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

!===========================================================================!
!                                                                           !
!         Generic real functions and subroutines for P223F programs.        !
!                                                                           !
!===========================================================================!

 REAL FUNCTION COSTRN (WF,YFRQ,NFRQ,KFRQ,T)

!----------------------------------------------------------------------------
!
!***  Calls CUBVAL
!***  Called by TDEM3D, SENS_FD2TD
!
! LAST MODIFICATION DATE: October, 2001.
!
!----------------------------------------------------------------------------
!
!  Produces time-domain value at time T by cosine transformation of NFRQ
!  frequency-domain values contained in cubic spline array YFRQ.
!  KFRQ is the high frequency cutoff, less than or equal to NFRQ.
!  Array WF contains the LOG (base e) of the angular frequency values.
!
!  The routine uses filter coefficients derived from the Niels Christensen
!  fast Hankel transform routine FILCOA at a spacing of 12 points per decade
!  and omega = 0.3.  Various filters were tested using a vertical magnetic
!  dipole receiver in a very large circular for which accurate frequency
!  and time-domain solutions were programmed.  This particular filter gave
!  the overall best accuracy for 1/2 spaces ranging in resistivity from
!  .1 to 10,000 ohm-m for times ranging from .01 to 50 msec.
!
!    K(W,T) = (2/PI) * F(W) * COS(WT) dW
!
!  Letting X = WT, the above becomes
!
!    K(W,T) = (2/PI*T) * F(X/T) * COS(X) dX
!
!  From Abramowitz and Stegun, COS(X) = SQRT(X*PI/2) * J(-1/2:X).

!  Filter Coefficients are used to represent X**(1/2) * J(-1/2:X)
!
!    COSTRN = SQRT (2/PI) * SUM(i) { WCOS(i) * F [X(i) /T] }
!
! The accumulation is done using 12 digit precision
!
!----------------------------------------------------------------------------

 USE AA_Filter_coefficients_QL
 IMPLICIT NONE
 INTEGER, PARAMETER :: NDEC_COS = 12, KFLOW = -200, KFHIGH = 99
 REAL, PARAMETER :: FAC = 0.7978846, TOL = 1.0E-6
 INTEGER J1,NFRQ,KFRQ
 REAL WF(NFRQ),YFRQ(4,NFRQ),T,YS,CUBVAL,V1
 REAL(KIND=QL) DELTA,Y1,Y,TD,YTYM,VAL

 DELTA = LOG (10._QL) / REAL (NDEC_COS, KIND=QL)
 TD = REAL (T, KIND=QL)
 YTYM = 0.
 Y1 = -LOG (TD) -DELCOS

 ! Begin right side convolution at weight 0. Stop when frequency domain array
 ! is exhausted.

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

 ! Begin left side convolution at weight -1. When log angular frequency is
 ! less than WF(3), check convergence. Continue left using the fact that
 ! impulse B is inversely proportional to frequency as freq -> 0; i.e., step
 ! response B is constant.

 MOVE_LOW: DO J1 = -1, KFLOW, -1
   Y = Y1 + J1 * DELTA
   YS = REAL(Y)
   IF (YS > WF(KFRQ)) CYCLE
   IF (YS < WF(1)) YS = WF(1)
   V1 = CUBVAL (WF,YFRQ,NFRQ,YS)
   VAL = WCOS(J1) * REAL (V1,KIND=QL)
   YTYM = YTYM + VAL
   IF ( Y < WF(3) ) THEN
     IF ( ABS(VAL) < (TOL * ABS(YTYM)) ) THEN
       EXIT MOVE_LOW
     END IF
   END IF
 END DO MOVE_LOW

 COSTRN = FAC * REAL (YTYM) / T

 END FUNCTION COSTRN

!============================================================================

 REAL FUNCTION CUBDER (XKNOT,COEF,KNOT,X1)

!----------------------------------------------------------------------------
!
!*** Called by: TDSET
!*** Calls: INTERV. On exit from INTERV
!
!  Evaluates the first derivative of a function from its cubic spline
!  interpolation.
!
!----------------------------------------------------------------------------
!
!     INPUT / OUTPUT:
!
!   MFLAG = -1  => X is to the left of interpolated range
!         =  1  => X is to the right of interpolated range
!         =  0  => X is in the interpolated range
!   KNOT  = Total number of knots including endpoints.
!   XKNOT(I), I = 1,KNOT  = location of the knots. The rightmost data
!                           point used to calculate coefficients is not
!                           used.
!   COEF(J,I), J = 1,4; I = 1,KNOT = Jth derivative at H = 0 where
!                                    H = X - XKNOT(I)
!
!----------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER I,MFLAG,KNOT
 REAL XKNOT(KNOT), COEF(4,KNOT), X1, H

 ! Find index I of largest breakpoint to the left of X1.

 CALL INTERV (XKNOT,KNOT-1,X1,I,MFLAG)
 H = X1 - XKNOT(I)
 IF (MFLAG == -1) H = 0.

 CUBDER = ( COEF(4,I)*H/2. + COEF(3,I) ) * H + COEF(2,I)

 END FUNCTION CUBDER

!============================================================================

 REAL FUNCTION CUBINT (XKNOT,COEF,KNOT,X1,X2)

!----------------------------------------------------------------------------
!
!*** Called by: TXCNVD, TXCNVL
!*** Calls: INTERV. On exit from INTERV
!
!  Integrates a function from X1 to X2 using its cubic spline representation.
!
!  This is a modification of the subroutine PPVALU from the book: "A
!  PRACTICAL GUIDE TO SPLINES" by C. De Boor.
!
!----------------------------------------------------------------------------
!
!     INPUT /OUTPUT:
!
!   MFLAG = -1  => X is to the left of interpolated range
!         =  1  => X is to the right of interpolated range
!         =  0  => X is in the interpolated range
!   KNOT  = Total number of knots including endpoints.
!   XKNOT(I), I = 1,KNOT - Location of the knots.  The rightmost data
!                          point used to calculate coefficients is not
!                          included.
!   COEF(J,I), J = 1,4; I = 1,KNOT; The coefficients of the cubic spline
!                                   represent the indefinite integral of F,
!                                   on the I'th interval, as:
!
!     INTGR [ F(X) ] = COEF(4,I)/24 * H**4 + COEF(3,I)/6 * H**3 +
!                      COEF(2,I)/2 * H**2 + COEF(1,I) * H
!
!             With H = X - XKNOT(K).
!
!----------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER I,I1,I2,MFLAG,KNOT
 REAL H,H1,H2,X1,X2,XKNOT(KNOT),COEF(4,KNOT)

 ! Find the indices I1 and I2 of largest breakpoints to the left of X1
 ! and X2 respectively.

 CALL INTERV (XKNOT,KNOT-1,X1,I1,MFLAG)
 CALL INTERV (XKNOT,KNOT-1,X2,I2,MFLAG)

 H1 = X1 - XKNOT(I1)

 IF ( MFLAG == -1 ) H1 = 0.

 H2 = X2 - XKNOT(I2)

 CUBINT = (((COEF(4,I2)*H2/4.0 + COEF(3,I2) )*H2/3.0 + &
             COEF(2,I2) )*H2/2.0 + COEF(1,I2) )*H2 &
        - (((COEF(4,I1)*H1/4.0 + COEF(3,I1) )*H1/3.0 + &
             COEF(2,I1) )*H1/2.0 + COEF(1,I1) )*H1

 ! Include integrals over intervening intervals.

 IF ( I2 > I1 ) THEN
   DO I = I1, I2-1
     H = XKNOT(I+1) - XKNOT(I)
     CUBINT = CUBINT + (((COEF(4,I)*H/4.0 + COEF(3,I) )*H/3.0 + &
                          COEF(2,I) )*H/2.0 + COEF(1,I) )*H
   END DO
 END IF

 END FUNCTION CUBINT

!============================================================================

  REAL FUNCTION CUBVAL (XKNOT,COEF,KNOT,X1)

!----------------------------------------------------------------------------
!
!*** Called by: COSTRN, FOLD_AND_CONVOLVE, TXCNVD, TXCNVL
!*** Calls: INTERV; on exit from INTERV
!
!  Evaluates a function at X1 from from its cubic spline representation.
!
!  The interval index I, appropriate for X, is found through a call to
!  INTERV. The formula for F is evaluated using nested multiplication.
!
!  This is a modification of the subroutine PPVALU from the book "A
!  PRACTICAL GUIDE TO SPLINES" by C. De Boor.
!
!----------------------------------------------------------------------------
!
!     INPUT / OUTPUT:
!
!   MFLAG = -1  => X is to the left of interpolated range
!         =  1  => X is to the right of interpolated range
!         =  0  => X is in the interpolated range
!   KNOT - total number of knots including endpoints.
!   XKNOT(I), I = 1,KNOT - location of the knots.  The rightmost data
!                          point used to calculate coefficients is not
!                          included.
!   COEF(J,I), J = 1,4; I = 1,KNOT; The coefficients of the cubic spline on
!                                   the I'th interval represent F as:
!
!              F(X) = COEF(4,I)/6 * H**3  +  COEF(3,I)/2 * H**2  +
!                     COEF(2,I) * H  +  COEF(1,I)
!
!              with  H = X - XKNOT(I)
!
!----------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER I,MFLAG,KNOT
 REAL XKNOT(KNOT),COEF(4,KNOT),X1,H

 ! Find index I of largest breakpoint to the left of X1.

 CALL INTERV (XKNOT,KNOT-1,X1,I,MFLAG)

 H = X1 - XKNOT(I)

 IF ( MFLAG == -1 ) H = 0.

 CUBVAL = ((COEF(4,I)*H/3.0 + COEF(3,I) )*0.5*H + COEF(2,I) )*H + COEF(1,I)

 END FUNCTION CUBVAL

!============================================================================

 REAL FUNCTION LINVAL (NX,XVAL,YVAL,X1,IDER)

!----------------------------------------------------------------------------
!
!***  Called by TXCNVD
!***  Calls INTERV.  On exit from INTERV
!
!  Evaluates a function at X1 from from its linear representation.
!
!  The interval index I, appropriate for X, is found through a call to INTERV.
!  The formula for F is evaluated using nested multiplication.
!
!----------------------------------------------------------------------------
!
!     INPUT / OUTPUT:
!
!   MFLAG = -1  => X is to the left of interpolated range
!         =  1  => X is to the right of interpolated range
!         =  0  => X is in the interpolated range
!
!   XVAL(1:NX) - location of the abscissa knots.  The rightmost data point
!                used to calculate coefficients is not included.
!
!   YVAL(1:NX,1) = function values.
!   YVAL(1:NX,2)   may be populated but aren't used.
!
!   If IDER = 0, the value in the interval is that of the leftmost knot.
!                because the derivative has been computed using two knot
!                values and stored at the left node.
!
!   If IDER = 1, the value is a linear interpolation between the knots.
!
!----------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER I,MFLAG,NX,IDER
 REAL XVAL(NX),YVAL(NX,3),X1,H

 ! Find index I of largest breakpoint to the left of X1.

 CALL INTERV (XVAL,NX-1,X1,I,MFLAG)

 IF ( IDER == 0 ) THEN
   ! Computed derivative values stored at right node (26.01.00)
   LINVAL = YVAL(I+1,1)
 ELSE
   H = X1 - XVAL(I)
   IF ( MFLAG == -1 ) H = 0.
   LINVAL = YVAL(I,1) + H * (YVAL(I+1,1) - YVAL(I,1)) / (XVAL(I+1) - XVAL(I))
 END IF

 END FUNCTION LINVAL

!============================================================================

 SUBROUTINE CUBSPL (XNOT,C,N,IBCBEG,IBCEND)

!----------------------------------------------------------------------------
!
!***  Called by FOLD_AND_CONVOLVE, READ_SYSTEM_DATA, TXCNVD
!
!  Calculates coefficients for cubic spline interpolation.
!
!  Call real function CUBVAL to evaluate function values after interpolation.
!  From "A Pracitcal Guide to Splines"  by Carl de Boor.
!
!----------------------------------------------------------------------------
!
!     INPUT:
!
!   N = number of data points. assumed to be > 1.
!
!   (XNOT(I), C(1,I), I=1,...,N) = Abscissae and ordinates of the data
!                                  points. XNOT is assumed to be strictly
!                                  increasing.
!
!   IBCBEG, IBCEND = boundary condition indicators, and
!   C(2,1), C(2,N) = boundary condition information. Specifically:
!
!   IBCBEG = 0  No boundary condition at XNOT(1) is given.  In this case,
!               the not-a-knot condition is used, i.e. the jump in the
!               third derivative across XNOT(2) is forced to zero.  Thus
!               first and the second cubic polynomial pieces are made to
!               coincide.
!   IBCBEG = 1  the slope at XNOT(1) is made to equal C(2,1),
!               supplied by input.
!   IBCBEG = 2  the second derivative at XNOT(1) is made to equal C(2,1),
!               supplied by input.
!
!   IBCEND = 0, 1, or 2 has analogous meaning concerning the boundary
!               condition at XNOT(n), with the additional information
!               taken from C(2,n).
!
!     OUTPUT:
!
!   C(J,I), J=1,...,4; I=1,...,L (= N-1) = Polynomial coefficients
!       of the cubic interpolating spline with interior knots (or joints)
!       XNOT(2), ..., XNOT(N-1).
!
!      In the interval: (XNOT(I) - XNOT(I+1)), the spline F is given by:
!
!      F(X) = C(1,I) + H* (C(2,I) + H* (C(3,I) + H* C(4,I)/3.) /2.)
!
!      where H = X - XNOT(I). Function CUBVAL may be used to evaluate F
!      or its derivatives from XNOT,C, L = N-1 and K=4.
!
!----------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER IBCBEG,IBCEND,N,I,J,L,M
 REAL C(4,N),XNOT(N),DIVDF1,DIVDF3,DXNOT,G
 SAVE

 ! A tridiagonal linear system for the unknown slopes S(I) of F at
 ! XNOT(I), I=1,...,N, is generated and then solved by Gauss elimination,
 ! with S(I) ending up in C(2,I), ALL I. C(3,.) AND C(4,.) are used initially
 ! for temporary storage.

 ! Compute first differences of XNOT sequence and store in C(3,.). Also,
 ! compute first divided difference of data and store in C(4,.).

 L = N - 1
 !dir$ loop count min(32)
 DO M = 2,N
   C(3,M) = XNOT(M) - XNOT(M-1)
   C(4,M) = (C(1,M) - C(1,M-1)) /C(3,M)
 END DO

 ! Construct first equation from the boundary condition, of the form
 ! C(4,1)*S(1) + C(3,1)*S(2) = C(2,1)

 IF ( IBCBEG < 1 ) THEN

   IF ( N > 2 ) THEN

     ! Not-a-knot condition at left end and N > 2.

     C(4,1) = C(3,3)
     C(3,1) = C(3,2) + C(3,3)
     C(2,1) = ((C(3,2) + 2.* C(3,1)) * C(4,2)*C(3,3) &
              + C(3,2)**2 * C(4,3)) /C(3,1)
     GO TO 100

   ELSE

     ! No condition at left end and N = 2.
     C(4,1) = 1.
     C(3,1) = 1.
     C(2,1) = 2. * C(4,2)
     GO TO 300

   END IF

 ELSE IF ( IBCBEG == 1 ) THEN

   ! Slope prescribed at left end.
   C(4,1) = 1.
   C(3,1) = 0.

 ELSE

   ! Second derivative prescribed at left end.
   C(4,1) = 2.
   C(3,1) = 1.
   C(2,1) = 3.* C(4,2) - C(3,2) * C(2,1) /2.

 END IF

 IF ( N == 2 ) GO TO 300

 ! If there are interior knots, generate the corresponding equations and
 ! perform the forward pass of Gauss elimination, after which the m-th
 ! equation reads: C(4,M)*S(M) + C(3,M)*S(M+1) = C(2,M).

 100 DO M = 2,L
   G = -C(3,M+1) / C(4,M-1)
   C(2,M) = G*C(2,M-1) &
            + 3.* (C(3,M)*C(4,M+1) + C(3,M+1)*C(4,M))
   C(4,M) = G* C(3,M-1) + 2.* (C(3,M) + C(3,M+1))
 END DO

 ! Construct last equation from the second boundary condition, of the
 ! form: (-G*C(4,N-1))*S(N-1) + C(4,N)*S(N) = C(2,N).

 ! If slope is prescribed at right end, one can go directly to back-
 ! substitution, since C array happens to be set up just right for it
 ! at this point.

 IF ( IBCEND < 1 ) THEN

   IF ( N /=3 .OR. IBCBEG /=0 ) THEN

     ! Not-a-knot and N > 2, and either N > 3 or also not-a-knot at
     ! left end point.
     G = C(3,N-1) + C(3,N)
     C(2,N) = ((C(3,N) + 2.*G) *C(4,N)*C(3,N-1) + C(3,N)**2 &
              *(C(1,N-1) - C(1,N-2)) /C(3,N-1))/G
     G = -G / C(4,N-1)
     C(4,N) = C(3,N-1)
     GO TO 350

   END IF

 ELSE IF ( IBCEND == 1 ) THEN

   GO TO 400

 ELSE

   GO TO 250

 END IF

 ! Either (N=3 and not-a-knot also at left) or (N=2 and not not-a-
 ! knot at left end point).

 200 C(2,N) = 2. * C(4,N)
     C(4,N) = 1.
     G = -1. / C(4,N-1)
     GO TO 350

 ! Second derivative prescribed at right endpoint.

 250 C(2,N) = 3.*C(4,N) + C(3,N)*C(2,N)/2.
     C(4,N) = 2.
     G = -1. / C(4,N-1)
     GO TO 350

 300 IF ( IBCEND < 1 ) THEN

   IF ( IBCBEG > 0 ) GO TO 200

   ! Not-a-knot at right endpoint and at left endpoint and N = 2.
   C(2,N) = C(4,N)
   GO TO 400

 ELSE IF ( IBCEND == 1 ) THEN

   GO TO 400

 ELSE

   GO TO 250

 END IF

 ! Complete forward pass of Gauss elimination.

 350 C(4,N) = G*C(3,N-1) + C(4,N)
     C(2,N) = (G*C(2,N-1) + C(2,N)) /C(4,N)

 ! Perform back substitution.

 400 J = L
 450 C(2,J) = (C(2,J) - C(3,J) *C(2,J+1)) /C(4,J)
     J = J - 1

 IF ( J > 0 ) GO TO 450

 ! Generate cubic coefficients in each interval, i.e., the derivatives
 ! at its left endpoint, from value and slope at its endpoints.

 DO I = 2,N
   DXNOT = C(3,I)
   DIVDF1 = (C(1,I) - C(1,I-1)) /DXNOT
   DIVDF3 = C(2,I - 1) + C(2,I) - 2.*DIVDF1
   C(3,I-1) = 2.* (DIVDF1 - C(2,I-1) - DIVDF3) /DXNOT
   C(4,I-1) = (DIVDF3/DXNOT) * (6./DXNOT)
 END DO

 END SUBROUTINE CUBSPL

!============================================================================

 SUBROUTINE INTERV (XT,LXT,X,LEFT,MFLAG)

!----------------------------------------------------------------------------
!
!*** Called by: CUBVAL, CUBINT
!*** Restructured April, 1997.
!
!  Computes: LEFT = MAX( I , 1 <= I <= LXT  .AND.  XT(I) <= X ).
!
!  The program is designed to be efficient in the common situation that
!  it is called repeatedly, with X taken from an increasing or decreasing
!  sequence. This will happen, e.g., when a pp function is to be grapged.
!  The first guess for LEFT is therefore taken to be the value returned at
!  the previous call and stored in the LOCAL variable ILO. A first
!  check ascertains that ILO < LXT (This is necessary since the present
!  call may have nothing to do with the previous call).
!
!  Then, if XT(ILO) <= XT(ILO+1), we set LEFT = ILO and are done after
!  just three comparisons. Otherwise, we repeatedly double the difference
!  ISTEP = IHI - ILO while also moving ILO and IHI in the direction of X,
!  until XT(ILO) <= X < XT(IHI), after which we use bisection to get, in
!  addition, ILO+1 = IHI. LEFT = ILO is then returned.
!
!  From "A Practical Guide to Splines" by C. De Boor.
!
!----------------------------------------------------------------------------
!
!     INPUT:
!
!   XT  - a real sequence, of length  LXT, assumed to be non-decreasing.
!   LXT - number of terms in the sequence XT.
!   X   - the point whose location with respect to the sequence XT is
!         to be determined.
!
!             OUTPUT
!             ------
!   LEFT and MFLAG are both integers, whose values are:
!
!        1     -1      IF             X <  XT(1)
!        I      0      IF   XT(I)  <= X < XT(I+1)
!       LXT     1      IF  XT(LXT) <= X
!
!   In particular, MFLAG = 0 is the 'usual' case.  MFLAG /= 0 indicates
!   that X  lies outside the halfopen interval XT(1) <= Y < XT(LXT). The
!   asymmetric treatment of the interval is due to the decision to make
!   all pp functions are continuous from the right.
!
!----------------------------------------------------------------------------

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

 IF ( X >= XT(LXT) ) THEN
   MFLAG = 1
   LEFT = LXT
   RETURN
 END IF

 MFLAG = 0
 IF ( ILO >= LXT ) ILO = LXT-1
 IHI = ILO + 1

 ! Trivial return when X is already in the interval.

 IF ( (X <= XT(IHI)) .AND. (X >= XT(ILO)) ) THEN
   LEFT = ILO
   RETURN
 END IF

!------------------------------------------------

 IF ( X <= XT(ILO) ) THEN ! Decrease ILO to capture X.

   ISTEP = 1
   DO J1 = 1,LXT
     IHI = ILO
     ILO = IHI - ISTEP
     ILO = MAX(1, ILO)
     IF ( (X >= XT(ILO)) .OR. (ILO == 1) ) EXIT
     ISTEP = ISTEP*2
   END DO

 ELSE IF ( X >= XT(IHI) ) THEN ! Increase IHI to capture X.

   ISTEP = 1
   DO J1 = 1,LXT
     ILO = IHI
     IHI = ILO + ISTEP
     IHI = MIN (IHI,LXT)
     IF ( (X <= XT(IHI)) .OR. (IHI == LXT) ) EXIT
     ISTEP = ISTEP*2
   END DO

 END IF

 ! Now XT(ILO) <= X < XT(IHI). Narrow the interval.

 DO J1 = 1,LXT
   MIDDLE = ( ILO + IHI ) / 2
   IF ( MIDDLE == ILO ) EXIT
   IF ( X < XT(MIDDLE) ) THEN
     IHI = MIDDLE
   ELSE
     ILO = MIDDLE
   END IF
 END DO

 ! Task complete.

 LEFT = ILO

 END SUBROUTINE INTERV

!===========================================================================!
!                                                                           !
!                   General P223F modelling subroutines.                    !
!                                                                           !
!===========================================================================!

 SUBROUTINE SET_NORM_TD (NW,BUNIT,BFFAC,KPPM,PUNIT,PPFAC,PRM_TD,NORM)

!----------------------------------------------------------------------------
!
!** Called from MAIN
!
!   ArjunAir computes all fields in nT or nT/s.  In order to match field data for
!   inversion, it computes factors (NORM) to convert computed data into pT, pT/s,
!   fT, fT/s, pct, ppt, ppm, ppb as required.
!
!----------------------------------------------------------------------------
!
!             Input
!             -----
!      BUNIT, BFAC, PUNIT & PPFAC are set in SUBROUTINE READ_SYSTEM_SETUP
!      BUNIT can have values nT/s, nT, pT, pT/s, fT or fT/s
!      BFFAC is the conversion factor needed to achieve this from nT or nT/s
!            = 1, 1000, or 1E6
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
!       NORM - Factor to convert time-domain response in nT or nT/s into
!              relevant units.
!
!----------------------------------------------------------------------------

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

 ! Primary fields, PRM_TD, are computed in nT, NORM, the normalisation. If the
 ! field data are not in nT, NORM, must be converted to pT or fT as required.

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

!============================================================================

 SUBROUTINE SET_SOURCE (STEP,ISW,BFFAC,WAVEFORM,NSX,SWX,SWY,PRM_TD)

!----------------------------------------------------------------------------
!
!*** Called by MAIN
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
!----------------------------------------------------------------------------
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
!----------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER ISW,STEP,NSX,JT
 REAL BFFAC,SWX(NSX),WAVEFORM(NSX),SWY(NSX,3),PKSX,DELT,COUPLING,PRM_TD(3)

 IF (SWX(2) - SWX(1) < 0.5E-7) SWX(2) = SWX(1) + 0.5E-7
 IF (SWX(NSX) - SWX(NSX-1) < 0.5E-7) SWX(NSX-1) = SWX(NSX) - 0.5E-7

 ! Remove the receiver coupling if the receiver voltage or magnetic field is
 ! to be used to get dI/dt.  Ensure that the input waveform is converted to
 ! nT or nT/s

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

 SUBROUTINE SET_SURVEY_1 (TDFD,NORTHD1,EASTD1,NXM,XSURF,FANGLE,NSTAT,SXD,SYD,SZ,NRXST, &
                          NRX,XRX,YRX,ZRX,RX,RY,RZ,RXD,RYD,NSTATQ,JMIN,JMAX,IFL)
!------------------------------------------------------------------------------------

!*** Called by MAIN
!*** Calls FIND_TX_RX

!  Transforms flight path survey coordinates SXD,SYD into coordinate system
!  (SX, 0) whose origin is the leftmost top node of the user defined mesh
!  (NORTHD1, EASTD1).  The flight path direction also defines the direction of
!  the horizontal mesh variation in the 2.5D aproximation.  SX is then defined
!  as the transmitter location along the flight path (and by implication along
!  the mesh) relative to the origin (NORTHD1, EASTD1).  Thus the SX can be
!  specified in single precision.
!
!  On entry, in time-domain, (SXD,SYD) is the survey transmitter position,
!  whereas in frequency domain, it is the Tx-Rx midpoint.  On exit, (SXD,SYD)
!  is the transmitter position in the survey system and SX is the transmitter
!  position in the transformed system.
!
!  SET_SURVEY computes receiver positions in survey and transformed systems
!
!  The computation will be done at pseudo-receiver positions located on nodes
!  and then splined to the receiver positions.  SET_SURVEY_1 computes the array
!  of horizontal locations to be used for this.
!
!----------------------------------------------------------------------------
!
!                Input
!                -----
!
!  NW       = Output unit number for .OUT output files.
!  NLG      = Output unit number for errror messages.
!  TDFD     = 1 for time domain and 2 for frequency domain
!  NORTHD1  = north coordinates of new origin at leftmost top node of user defined mesh.
!  EASTD1   = east coordinates of new origin
!  NXM      = Number of node columns in user-defined mesh
!  XSURF    = Horizontal coordinates of mesh surface.
!  FANGLE   = angle in radians that the survey makes with north (positive clockwise)
!  NSTAT    = Number of stations in original survey
!  SXD      = North coordinates of survey in survey system
!  SYD      = East coordinates of survey in survey system
!  NRX      = Number of receivers per transmitter position
!  NRXST    = NRX in frequency domain;  = NSTAT in time domain
!  XRX      = In-line receiver offset. XRX is positive behind the transmitter
!  YRX      = Transverse receiver offset. YRX is positive left
!  ZRX      = Vertical receiver offset. ZRX is positive below the transmitter
!  RX       = horizontal (along mesh) receiver coordinates
!  RY       = Transverse receiver offset
!  RZ       = barometric elevation of receivers.
!  RXD      = North survey receiver coordinates
!  RYD      = East survey receiver coordinates
!
!                Output
!                ------
!  SXD      = North coordinates of transmitter in survey system
!  SYD      = East coordinates of transmitter in survey system
!  RXD      = North coordinates of receiver in survey system
!  RYD      = East coordinates of receiver in survey system
!
!  RX      = North coordinates of receiver in computation system
!  RY      = East coordinates of receiver in computation system
!  NSTATQ  = number of node-based survey positions for computation
!          = number of surface nodes within survey plus two end members
!  JMIN    = XSURF index of first node-based receiver
!  JMAX    = XSURF index of last node-based receiver
!  IFL     = 0 : operation successful;  = 1 : survey outside mesh. Abort.
!----------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL = SELECTED_REAL_KIND(12,80)
 INTEGER IFL,TDFD,NSTAT,NRX,NRXST,NXM,JS,JR,JX,JMAX,JMIN,NSTATQ
 REAL XSURF(NXM),CSF,SNF,XR1,YR1,XRJR,YRJR,SX,SX1,SY1,X1,X2,SZ(NSTAT),FANGLE
 REAL, DIMENSION(NRXST) :: XRX,YRX,ZRX
 REAL, DIMENSION(NSTAT,NRX) :: RX,RY,RZ
 REAL(KIND=QL) NORTHD1,EASTD1,SXD(NSTAT),SYD(NSTAT),RXD(NSTAT,NRX),RYD(NSTAT,NRX)

 ! Compute receiver coordinates in both body centred and survey systems.

 DO JS = 1, NSTAT
   CSF = COS (FANGLE)
   SNF = SIN (FANGLE)
   SX1 = REAL (SXD(JS) - EASTD1)   ! Set survey origin at leftmost top user mesh node.
   SY1 = REAL (SYD(JS) - NORTHD1)
   XR1 = XRX(1) * CSF - YRX(1) * SNF
   YR1 = XRX(1) * SNF + YRX(1) * CSF
   IF (TDFD < 2) THEN
     SX = SX1 * CSF + SY1 * SNF       ! Define SX as Tx position along mesh from new origin
     RX(JS,1) = SX - XRX(1)
     RY(JS,1) = YRX(1)
     RZ(JS,1) = SZ(JS) - ZRX(1)
     RXD(JS,1) = SXD(JS) - REAL (XR1,QL)  ! Receiver position in survey coordinates
     RYD(JS,1) = SYD(JS) - REAL (YR1,QL)
   ELSE
     SX = CSF*(SX1 + 0.5*XRX(1)) + SNF*(SY1 + 0.5*YRX(1))
     SXD(JS) = SXD(JS) + 0.5D0 * REAL (XR1,QL)   ! Specify Tx position in system coordinates
     SYD(JS) = SYD(JS) + 0.5D0 * REAL (YR1,QL)
     DO JR = 1, NRX
       RX(JS,JR)  = SX - XRX(JR)
       RY(JS,JR)  = YRX(JR)
       RZ(JS,JR)  = SZ(JS) - ZRX(JR)
       XRJR = XRX(JR) * CSF - YRX(JR) * SNF
       YRJR = XRX(JR) * SNF + YRX(JR) * CSF
       RXD(JS,JR) = SXD(JS) - REAL (XRJR,QL)
       RYD(JS,JR) = SYD(JS) - REAL (YRJR,QL)
     END DO
   END IF
 END DO

 JMIN = 0 ; JMAX = 0
 X1 = MINVAL (RX(1:NSTAT,1))
 X2 = MAXVAL (RX(1:NSTAT,1))

 DO JX = 1, NXM-1
   IF (XSURF(JX) < X1 .AND. XSURF(JX+1) >= X1) JMIN = JX
   IF (XSURF(JX) < X2 .AND. XSURF(JX+1) >= X2) THEN
     JMAX = JX + 1
     EXIT
   END IF
 END DO
 jmin = jmin - 1;
 jmax = jmax + 1;
!  Write (*, *) x1, x2, jmin, jmax

 IFL = 0
 IF (JMIN > 0 .AND. JMAX > 0) THEN
   NSTATQ = JMAX - JMIN + 1
 ELSE
   IFL = 1
 END IF

 END SUBROUTINE SET_SURVEY_1

 SUBROUTINE SET_SURVEY_2 (NW,TDFD,BAROMTRC,NSTAT,NRXST,XRX,YRX,ZRX,NRX,RX,RY,RZ,NXM, &
                          JMIN,JMAX,XSURF,ZSURF,NSTATQ,RXQ,RYQ,RZQ,SXQ,SZQ,ZGC,IFL)
!---------------------------------------------------------------------------------

!*** Called by MAIN
!*** Calls FIND_TX_RX

!  Computes RXQ, RYQ, RZQ, receiver positions corresponding to mesh nodes
!  encompassing the survey positions.  RZ, the receiver altitudes based on
!  actual aircraft receiver positions is splined nad used to compute RZQ, the
!  interpolated altitude at the horizontal nodes.  Using RZQ, SZQ and SXQ, the
!  node-based transmitter altitudes and positions along the survey are computed.
!
!  For frequency domain, RXQ(*,1), RZQ(*,1) will be at a node location.
!
!  RZQ(*,1) will be used subesequently to construct an additional node layer in
!  air such that the receiver position corresponds to a node, thus rendering better
!  better accuracy and eliminating oscillations.
!
!                Input
!                -----
!
!  NW       = Output unit number for .OUT output files.
!  TDFD     = 1 for time domain;  = 2 for frequency domain
!  BAROMTRC = 0 => RZ is expressed as ground clearance
!           = 1 => RZ is expressed as barometric altitude
!  NSTAT    = Number of stations in AEM survey
!  NRXST    = NRX in frequency domain;  = NSTAT in time domain
!  NRX      = Number of receivers per transmitter position
!  XRX      = In-line receiver offset. XRX is positive behind the transmitter
!  YRX      = Transverse receiver offset. YRX is positive left
!  ZRX      = Vertical receiver offset. ZRX is positive below the transmitter
!  RX       = horizontal (along mesh) survey receiver coordinates
!  RY       = Transverse survey receiver offsets
!  RZ       = barometric elevation of receivers.
!  NXM      = Number of mesh nodes East-West.
!  JMIN     = XSURF index of first node-based receiver
!  JMAX     = XSURF index of last node-based receiver
!  XSURF    = Horizontal Coordinates of mesh surface. (XSURF(1) = 0.
!  ZSURF    = Height of mesh surface in RLs (positive above sea level, negative below.
!  NSTATQ   = number of node-based survey positions for computation
!           = number of surface nodes within survey plus two end members
!
!                Output (node-based system)
!                ------
!  RZQ      :  GPS receiver altitudes in the node based receiver system
!  ZGC      :  ground clearance of first receiver at each node station
!  RXQ, RZQ :  receiver coordinates in the node based receiver system
!  SXQ, SZQ :  transmitter coordinates in the node based receiver system
!
!  IFL      = 1 if any of the receivers are below ground
!           = 0 otherwise
!
!----------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER NW,TDFD,BAROMTRC,NSTAT,NRXST,NRX,NXM,JMIN,JMAX,NSTATQ,IFL,NDIRT,JX,JM,JR,I
 REAL X,CUBVAL,ZS
 REAL, DIMENSION (NRXST) :: XRX,YRX,ZRX
 REAL, DIMENSION (NSTATQ) :: SXQ,SZQ,ZGC
 REAL, DIMENSION (NSTAT,NRX) :: RX,RY,RZ
 REAL, DIMENSION (NSTATQ,NRX) :: RXQ,RYQ,RZQ
 REAL, DIMENSION (NXM) :: XSURF,ZSURF
 REAL, ALLOCATABLE :: XC(:), ZC4(:,:)

 IFL = 0
 ALLOCATE (ZC4(4,NSTAT),XC(NSTAT))

 RXQ(1:NSTATQ,1) = XSURF(JMIN:JMAX)  !  Position RXQ(*,1) at horizontal node locations
 XC(1:NSTAT) = RX(1:NSTAT,1)
 ZC4(1,1:NSTAT) = RZ(1:NSTAT,1)
 CALL CUBSPL (XC,ZC4,NSTAT,0,0)      ! Splined RZ
 DO JX = 2, NSTATQ - 1               ! Compute RZQ at the nodes
   X = RXQ(JX,1)
   RZQ(JX,1) = CUBVAL (XC,ZC4,NSTAT,X)
 END DO

 RZQ(1,1) = RZQ(2,1)
 RZQ(NSTATQ,1) = RZQ(NSTATQ-1,1)

!  Convert ground clearance RZQ to GPS.  Check for below surface receivers
!  Compute positions for transmitters and additional receivers

 NDIRT = 0
 DO JX = 1, NSTATQ
   JM = JX + JMIN - 1
   SXQ(JX) = RXQ(JX,1) + XRX(1)
   SZQ(JX) = RZQ(JX,1) + ZRX(1)
   IF (BAROMTRC == 0) THEN
     ZS = 0.
     DO I = 1, NXM - 1
       IF (XSURF(I) < SXQ(JX) .AND. XSURF(I+1) >= SXQ(JX)) THEN
         ZS = ZSURF(I) + (SXQ(JX)-XSURF(I)) * (ZSURF(I+1)-ZSURF(I)) / (XSURF(I+1)-XSURF(I))
         EXIT
       END IF
     END DO
     SZQ(JX) = SZQ(JX) + ZS
     RZQ(JX,1) = RZQ(JX,1) + ZS
   END IF
   DO JR = 2, NRX
     RXQ(JX,JR) = SXQ(JX) - XRX(JR)
     RZQ(JX,JR) = SZQ(JX) - ZRX(JR)
   END DO
   ZGC(JX) = RZQ(JX,1) - ZSURF(JM)
   IF (ZGC(JX) < 0.) NDIRT = NDIRT + 1
 END DO
 IF (NDIRT > 0) IFL = 1

!  Set Y coordinates of receivers

 IF (TDFD < 2) THEN
   ZC4(1,1:NSTAT) = RY(1:NSTAT,1)
   CALL CUBSPL (XC,ZC4,NSTAT,0,0)      ! Splined RY
   DO JX = 2, NSTATQ - 1               ! Compute RYQ at the nodes
     X = RXQ(JX,1)
     RYQ(JX,1) = CUBVAL (XC,ZC4,NSTAT,X)
   END DO
   RYQ(1,1) = RYQ(2,1)
   RYQ(NSTATQ,1) = RYQ(NSTATQ-1,1)
 ELSE
   DO JX = 1,NSTATQ
     RYQ(JX,1:NRX) = - YRX(1:NRX)
   END DO
 END IF

 DEALLOCATE (XC,ZC4)

!  Write out transmitter and first receiver positions in GPS + RZQ(*,1) in ground clearance.

 WRITE(NW,1)
 IF (NDIRT > 0) WRITE(NW,2) NDIRT
 WRITE(NW,3)
 DO JX = 1,NSTATQ
   WRITE(NW,4) JX,SXQ(JX),SZQ(JX),RXQ(JX,1),RYQ(JX,1),RZQ(JX,1),ZGC(JX)
 END DO

 1 FORMAT(/T15,'Node-based Pseudo Transmitter and First Receiver Positions' &
          /T15,'----------------------------------------------------------')
 2 FORMAT(/T3,I8,T10,'Receiver Positions are below the earth in order to facilitate mining.')
 3 FORMAT(/T20, 'Transmitter                         Receiver'/T82,'Rx Ground' &
          /T2,'Index', 10x, 'East', 7x, 'GPS Alt', 10x, 'East', 9x, 'North', 7x, 'GPS Alt', 5x, 'Clearance',&
          /x,'-----', 10x, '----', 7x, '-------', 10x, '----', 9x, '-----', 7x, '-------', 5x, '---------')
 4 FORMAT(2x, i4, 6(2x, f12.4))

 END SUBROUTINE SET_SURVEY_2

 SUBROUTINE SET_MESH_COMPLETION (NX,NZ,NXZ,NPN,NAIR,JMIN,NSTATQ,NRX,RXQ,RZQ,ZGC, &
                                 XLOC,ZLOC,LNODS,COORD,IREL)
!-------------------------------------------------------------------------------

!***  Called by MAIN
!
!  Sets node rows in air. with one row at the height of the first receiver.
!  This assumes that NAIR = 11
!
!  Computes element coordinate array identifies elements corresponding to receivers.
!
!    INPUT
!    -----
!
!   NX       : number of horizontal nodes used to define cell corners
!   NZ       : number of vertical nodes used to define cell corners
!   NXZ =    : Number of elements.
!   NPN      : number of nodes needed to define serendipitous elements
!            = (2*NX-1)*(2*NZ-1) - NXZ
!   NAIR     = 11 : number of node rows above surface
!   JMIN     = index of first pseudo-receiver (user-defined mesh)
!   NSTATQ   : number of horizontal nodes serving as pseudo-receivers
!   NRX      : nomber o receivers per source position
!   RXQ, RZQ :  receiver coordinates in the node based receiver system
!   ZGC      : pseudo-receiver ground clearance
!   XLOC     : horizontal node locations for element corners (expanded mesh sans air nodes)
!   ZLOC     : vertical node locations for element corners (expanded mesh sans air nodes)
!   LNODS    : array of element node numbers.
!
!    OUTPUT
!    ------
!
!   COORD  : element coordinate array
!   IREL   : array which identifies the mesh element (air) which
!            contains each receiver position along the survey.


 INTEGER NX,NZ,NXZ,NPN,NAIR,JMIN,NSTATQ,NRX,JZ,LNODS(NXZ,8),IREL(NRX,NSTATQ), &
         K1,K2,L,JX,I,K,JS,JR,LX,LZ
 REAL ZGC(NSTATQ),ZAVG,ZAIR0(11),XLOC(NX,NZ),ZLOC(NX,NZ),COORD(NPN,2)
 REAL, DIMENSION (NSTATQ,NRX) :: RXQ,RZQ
 REAL, ALLOCATABLE :: ZAIR(:,:)
 DATA ZAIR0 /-10000.,-1000.,-500.,-300.,-200.,-120.,-90.,-60.,-30.,-10.,-5./

 ALLOCATE (ZAIR(NX,NAIR))
 ZAVG = SUM (ZGC) / REAL (NSTATQ)

 K1 = JMIN + 2;  K2 = K1 + NSTATQ-1    ! Origin = 2 nodes left of user-defined mesh.

 DO JZ = 1,11
   ZAIR(1:NX,JZ) = ZAIR0(JZ)
 END DO

 zair(k1:k2,  6) = min(zair0(6), -2 * zgc(1:nstatq))
 zair(k1:k2,  7) = min(zair0(7), -5 * zgc(1:nstatq) /3.)
 zair(k1:k2,  8) = -4 * zgc(1:nstatq) /3. 
 zair(k1:k2,  9) = -zgc(1:nstatq)
 zair(k1:k2, 10) = -2 * zgc(1:nstatq) /3.
 zair(k1:k2, 11) = -1 * zgc(1:nstatq) /3.
 
! IF (ZAVG > 85.) THEN
!   ZAIR (K1:K2, 7) = -ZGC(1:NSTATQ)
!   ZAIR (K1:K2, 8) = -60.
!   ZAIR (K1:K2, 9) = -30.
!   ZAIR (K1:K2,10) = -10.
!   ZAIR (K1:K2,11) = -5.

! ELSE IF (ZAVG .ge. 60.) THEN
!   ZAIR (K1:K2, 7) = -90.
!   ZAIR (K1:K2, 8) = -ZGC(1:NSTATQ)
!   ZAIR (K1:K2, 9) = -40.
!   ZAIR (K1:K2,10) = -15.
!   ZAIR (K1:K2,11) = -5.

! ELSE IF (ZAVG .ge. 30.) THEN
!   ZAIR (K1:K2, 7) = -90.
!   ZAIR (K1:K2, 8) = -60.
!   ZAIR (K1:K2, 9) = -ZGC(1:NSTATQ)
!   ZAIR (K1:K2,10) = -15.
!   ZAIR (K1:K2,11) = -5.

! ELSE IF (ZAVG .ge. 20.) THEN
!   ZAIR (K1:K2, 7) = -90.
!   ZAIR (K1:K2, 8) = -60.
!   ZAIR (K1:K2, 9) = -40.
!   ZAIR (K1:K2,10) = -ZGC(1:NSTATQ)
!   ZAIR (K1:K2,11) = -8.

! ELSE IF (ZAVG .gt.  5.) THEN
!   ZAIR (K1:K2, 7) = -90.
!   ZAIR (K1:K2, 8) = -60.
!   ZAIR (K1:K2, 9) = -30.
!   ZAIR (K1:K2,10) = -ZGC(1:NSTATQ)
!   ZAIR (K1:K2,11) = -5.

! ELSE
!   ZAIR (K1:K2, 7) = -90.
!   ZAIR (K1:K2, 8) = -60.
!   ZAIR (K1:K2, 9) = -30.
!   ZAIR (K1:K2,10) = -12.
!   ZAIR (K1:K2,11) = -ZGC(1:NSTATQ)
! END IF

 DO JZ = 7,11
   ZAIR(1:K1-1,JZ) = ZAIR(3,JZ)
   ZAIR(K2+1:NX,JZ) = ZAIR(K2,JZ)
 END DO

 DO JZ = 1, NAIR
   ZLOC(1:NX,JZ) = ZLOC(1:NX,NAIR+1) + ZAIR(1:NX,JZ)
   XLOC(1:NX,JZ) = XLOC(1:NX,NAIR+1)
 END DO

! Calculate element coordinates:

 L = 1
 DO JX = 1, NX
   DO JZ = 1, NZ
     COORD(L,1) = XLOC(JX,JZ)
     COORD(L,2) = ZLOC(JX,JZ)
     L = L + 2
   END DO
   L = L + (NZ-1)
 END DO

 DO K = 1, 2
   DO I = 1, (NX-1)*(NZ-1)
     COORD(LNODS(I,2),K) = (COORD(LNODS(I,1),K) + COORD(LNODS(I,3),K)) / 2.
     COORD(LNODS(I,4),K) = (COORD(LNODS(I,3),K) + COORD(LNODS(I,5),K)) / 2.
     COORD(LNODS(I,6),K) = (COORD(LNODS(I,5),K) + COORD(LNODS(I,7),K)) / 2.
     COORD(LNODS(I,8),K) = (COORD(LNODS(I,7),K) + COORD(LNODS(I,1),K)) / 2.
   END DO
 END DO

! Assign the receivers to an element number:

 DO JS = 1,NSTATQ
   DO JR  = 1, NRX
     DO JX = 1, NX
       IF (ABS(XLOC(JX,1)-RXQ(JS,JR)) < 1.E-4) THEN
         LX = JX
         EXIT
       END IF
       IF (XLOC(JX,1) > RXQ(JS,JR)) THEN
         LX = JX - 1
         EXIT
       END IF
     END DO
     DO JZ = 1, NZ
       IF (ABS(ZLOC(LX,JZ) + RZQ(JS,JR)) < 1.E-4) THEN
         LZ = JZ
         EXIT
       END IF
       IF (ZLOC(LX,JZ) > -RZQ(JS,JR)) THEN
         LZ = JZ - 1
         EXIT
       END IF
     END DO
     LZ = JZ - 1
     IREL(JR,JS) = (LX-1)*(NZ-1) + LZ
   END DO
 END DO

 DEALLOCATE (ZAIR)
 
!
! Dump entire mesh for debufgging ..
  Open(Unit = 99, File = 'ArjunAir.msh', Status = 'Replace')   
  Do jx = 1, nx
    Do jz = 1, nz
        Write (99, 15) jx, jz, xloc(jx, jz), zloc(jx, jz)
    End Do
  End Do
  Close (99)
15 Format (2(2x, i8), 2(2x, en15.6))  

 END SUBROUTINE SET_MESH_COMPLETION

!==============================================================================

 SUBROUTINE SET_DATA_FD (NFRQ,NCOMP,NSTATQ,NSTAT,BFDQ,RXQ,RX,BFD,NRX)

!------------------------------------------------------------------------------
!
!*** Called by: FD_FIELD, WRITE_FD
!
!  Interpolates the field BFDQ based on NSTATQ receivers at (RXQ,RZQ) into
!  BFD based on NSTAT receivers at (RX,RZ).
!
!  NCOMP = number of components
!  NFRQ = number of frequencies
!
!------------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER NFRQ,NSTAT,NSTATQ,NCOMP,JC,JF,JS,NRX,JRX
 REAL RX(NSTAT,NRX),RXQ(NSTATQ,NRX),QR(4,NSTATQ),QI(4,NSTATQ),CUBVAL,A1,A2,X, &
      XC(NSTATQ)
 COMPLEX BFD(NFRQ,NSTAT,NCOMP),BFDQ(NFRQ,NSTATQ,NCOMP)

 DO JC = 1, NCOMP
   DO JF = 1, NFRQ
     JRX = JF
     XC(1:NSTATQ) = RXQ(1:NSTATQ,JRX)
     DO JS = 1, NSTATQ
       QR(1,JS) = REAL (BFDQ(JF,JS,JC))
       QI(1,JS) = AIMAG(BFDQ(JF,JS,JC))
     END DO
     CALL CUBSPL(XC,QR,NSTATQ,0,0)
     CALL CUBSPL(XC,QI,NSTATQ,0,0)
     DO JS = 1,NSTAT
       X = RX(JS,JRX)
       A1 = CUBVAL (XC,QR,NSTATQ,X)
       A2 = CUBVAL (XC,QI,NSTATQ,X)
       BFD(JF,JS,JC) = CMPLX (A1,A2)
     END DO
   END DO
 END DO

 END SUBROUTINE SET_DATA_FD

!==============================================================================

 SUBROUTINE SET_DATA_TD (NCHNL,NCOMP,NSTATQ,NSTAT,BTDQ,RXQ,RX,BTD,NRX)

!------------------------------------------------------------------------------
!
!*** Called by: TD_FIELD, WRITE_TD
!
!  Interpolates the field BTDQ based on NSTATQ receivers at (RXQ,RZQ) into
!  BTD based on NSTAT receivers at (RX,RZ).
!
!  NCOMP = number of components
!  NCHNL = number of frequencies
!
!------------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER NCHNL,NSTAT,NSTATQ,NCOMP,JC,JT,JS,NRX,JRX
 REAL RX(NSTAT,NRX),RXQ(NSTATQ,NRX),QR(4,NSTATQ),CUBVAL,X,XC(NSTATQ), &
      BTD(NCHNL,NSTAT,NCOMP),BTDQ(NCHNL,NSTATQ,NCOMP)

 JRX = 1
 DO JC = 1, NCOMP
   DO JT = 1, NCHNL
     XC(1:NSTATQ) = RXQ(1:NSTATQ,JRX)
     DO JS = 1, NSTATQ
       QR(1,JS) = BTDQ(JT,JS,JC)
     END DO
     CALL CUBSPL(XC,QR,NSTATQ,0,0)
     DO JS = 1,NSTAT
       X = RX(JS,JRX)
       BTD(JT,JS,JC) = CUBVAL (XC,QR,NSTATQ,X)
     END DO
   END DO
 END DO

 END SUBROUTINE SET_DATA_TD

!==============================================================================

 SUBROUTINE SET_TXCLN (TDFD,TXCLN,NRXST,TXCLNQ,NRXSTQ,RXQ,RX,NSTAT,NSTATQ,NRX)

!------------------------------------------------------------------------------
!
!*** Called by: MAIN
!*** Calls: CUBSPL, CUBVAL
!
!  Interpolates the transmitter angle TXCLN based on NRXST angles into
!  TXCLNQ based on NRXSTQ angles.
!
!  For TD, NRXST = NSTAT --> NRXSTQ = NSTATQ
!  For FD, NRXST = NFRQ  --> NRXSTQ = NFRQ   (No change)
!
!------------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER TDFD,NRXST,NRXSTQ,JS,JF,NRX,NSTAT,NSTATQ
 REAL TXCLN(NRXST),TXCLNQ(NRXSTQ),QR(4,NRXST),XC(NRXST),CUBVAL,X, &
      RXQ(NSTATQ,NRX),RX(NSTAT,NRX)

 IF (TDFD < 2) THEN                   ! Time domain.
   XC(1:NRXST) = RX(1:NRXST,1)
   QR(1,1:NRXST) = TXCLN(1:NRXST)
   CALL CUBSPL (XC,QR,NRXST,0,0)      ! Splined RY
   DO JS = 1, NSTATQ               ! Compute RYQ at the nodes
     X = RXQ(JS,1)
     TXCLNQ(JS) = CUBVAL (XC,QR,NSTAT,X)
   END DO
 ELSE                                  ! Frequency domain.
   DO JF = 1, NRXST
     TXCLNQ(JF) = TXCLN(JF)
   END DO
 END IF

 END SUBROUTINE SET_TXCLN

!==============================================================================

 SUBROUTINE DCPRM_FD (NFRQ,XRX,YRX,ZRX,TXCLN,PRM_FD,PPFAC,NORM)

!--------------------------------------------------------------
!
!*** Called by: MAIN
!
!  In frequency-domain, it computes the maximally coupled component of B at each
!  receiver location for each frequency assuming unit dipoles and current
!  transmitters are co-oriented.
!
!  SIGN CONVENTION:
!  ----------------
!  The normal layered earth field coordinate system used in this
!  subroutine has X (JC=1) positive along the flight path, Y (JC=2)
!  positive to right, and Z (JC=3) positive down.
!
!----------------------------------------------------------------------------
!
!                               INPUT
!                               -----
!       NFRQ - number of frequencies
!        ZRX - vertical offset of RX relative to transmitter (below = + ).
!        XRX - in-line offset of RX relative to transmitter (behind = + ).
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
!
!----------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER NFRQ,JF
 REAL SNTX,CSTX,XBD,YBD,ZBD,RBRD,RSQ,RSQ1,BFAC,FAC,FACZX,INLINE,VERT,PPFAC
 REAL, DIMENSION(NFRQ) :: TXCLN,XRX,YRX,ZRX,PRM_FD,NORM
 LOGICAL COPLANAR

 ! BFAC = 1.0E9 * MU / (4 * PI)  =>  NANOTESLAS

 PRM_FD = 0.
 BFAC = 100.

 DO JF = 1,NFRQ
   SNTX = SIN(TXCLN(JF))
   CSTX = COS(TXCLN(JF))
   XBD = -XRX(JF)  !  XRX is defined as positive behind the TX.
   YBD = YRX(JF)
   RBRD = SQRT (XBD**2 + YBD**2)
   ZBD = ZRX(JF)
   RSQ = ZBD**2 + RBRD**2
   RSQ1 = SQRT (RSQ)
   COPLANAR = .FALSE.
   IF (ABS(SNTX) < .01) COPLANAR = .TRUE.
   IF (COPLANAR) THEN
     PRM_FD(JF) = -BFAC / RSQ1**3
   ELSE
     FAC = BFAC / RSQ1**5
     FACZX = 3.*FAC*XBD*ZBD
     VERT = FAC*CSTX*(3.* ZBD**2 - RSQ) + SNTX*FACZX
     INLINE = FAC*SNTX*(3.* XBD**2 - RSQ) + CSTX*FACZX
     PRM_FD(JF) = CSTX*VERT + SNTX*INLINE
   END IF
   NORM(JF) = PPFAC / ABS (PRM_FD(JF))
 END DO

 END SUBROUTINE DCPRM_FD

!============================================================================

 SUBROUTINE DCPRM_TD (XRX0,YRX0,ZRX0,TXCLN0,TXAREA,PRM_TD)

!----------------------------------------------------------------------------
!
!*** Called by: READ_INVERSION_CONTROL
!
!  For time-domain, PRM_TD is the 3 component Tx-Rx dc coupling factor
!  per unit dipole moment, expressed in NANOTESLAS per unit amp
!  Multiply it by dI/dt and get nanovolts per m^2 which is the
!  same as nT/s.  Multiply it by current and get nT.
!
!  SIGN CONVENTION:
!  ----------------
!  The normal layered earth field coordinate system used in this
!  subroutine has X (JC=1) positive along the flight path, Y (JC=2)
!  positive to right, and Z (JC=3) positive down.
!
!----------------------------------------------------------------------------
!
!                               INPUT
!                               -----
!  TXCLN0 - angle in degrees that the transmitting dipole makes with vertical
!           (climb = positive for VMD transmitter)
!  TXAREA - transmitter area in sq. metres
!  ZRX0, XRX0 & YRX0 are the initial vertical, in-line and transverse offsets
!                    of the receiver relative to transmitter
!                    below = + ;  behind = + ;  left = +
!
!                          OUTPUT (time domain)
!                           --------------------
!  PRM_TD = primary field coupling factor for B in nT (unit TX moment)
!  PRM_TD(1) = in-line B
!  PRM_TD(2) = transverse B
!  PRM_TD(3) = vertical B for station
!
!----------------------------------------------------------------------------

 IMPLICIT NONE
 REAL, PARAMETER :: PI=3.141592654
 REAL SNTX,CSTX,XBD,YBD,ZBD,RBRD,RSQ,RSQ1,BFAC,FAC,FACZX,TXAREA, &
      TXCLN0,THETA,XRX0,YRX0,ZRX0,INLINE,VERT,TRANS,PRM_TD(3)

 ! BFAC = 1.0E9 * MU / (4 * PI) for time domain  =>  NANOTESLAS

 PRM_TD = 0.

 ! In-loop time-domain HEM

 IF (TXAREA > 1.) THEN
   RBRD = ABS (XRX0) + ABS (YRX0)
   IF (RBRD < 1.) THEN
     ZBD = SQRT (ZRX0**2 + TXAREA / PI)
     PRM_TD(3) = 200. / ZBD**3       ! 1.0E9 * MU / (2 * PI) = 200. (nT)
     RETURN
   END IF
 END IF

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
!         ZRX - vertical receiver offset for each frequency; below = positive
!         XRX - in-line receiver offset for each frequency;  behind = positive
!         YRX - transverse receiver offset for each frequency; left = positive.
!
!----------------------------------------------------------------------------

 INTEGER NFRQ,JF,TXO,CFG1(NFRQ)
 REAL XABS,YABS,ZABS,RABS
 REAL, DIMENSION (NFRQ) :: TXCLN,XRX,YRX,ZRX
 Logical :: txa90
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

!============================================================================

 SUBROUTINE FDREAD (ND,NFRQ,NSTAT,BFD)

!----------------------------------------------------------------------------
!
!***  Called by MAIN
!
!  Reads frequency-domain scattered impulse magnetic field data
!  (real & imaginary components) from logical UNIT ND into
!  array BFD for conversion to time-domain by  TDEM_OUT.
!
!          NFRQ - number of frequencies
!         NSTAT - number of transmitter positions
!  BFD(I,J,K) - Kth component of the complex frequency-domain impulse
!                      response magnetic field (H) at transmitter J
!                      for frequency I.  (nT)
!
!----------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER ND,NFRQ,NSTAT,JF,JS,JC
 REAL A(6)
 COMPLEX BFD(NFRQ,NSTAT,3)

 REWIND(ND)

 DO JF = 1, NFRQ
   DO JS = 1, NSTAT
     READ(ND,*) A(1:6)
     DO JC = 1,3
       BFD(JF,JS,JC) = CMPLX(A(2*JC-1),A(2*JC))
     END DO
   END DO
 END DO

 END SUBROUTINE FDREAD

!===========================================================================!
!                                                                           !
!        Subroutines for frequency- to time-domain transformations.         !
!                                                                           !
!===========================================================================!

 SUBROUTINE TDEM_3D (STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP, &
                     NCHNL,TOPN,TCLS,FREQ,NFRQ,NSTAT,BFD,GSTRP,ASTRP,BTD)

!----------------------------------------------------------------------------
!
!***  Called by MAIN
!***  Calls CUBSPL, COSTRN,FOLD_CONVOLVE
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
!----------------------------------------------------------------------------
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
! BFD(I,J,K) - Kth component of the complex frequency-domain impulse
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
!----------------------------------------------------------------------------

 IMPLICIT NONE
 REAL, PARAMETER :: TWOPI = 6.283185
 INTEGER GSTRP,ASTRP,IDER,STEP,NSX,NPULS,NTYPLS,NTYRP,NCHNL,NFRQ,NSTAT, &
         JS,JF,JC,JT
 REAL, DIMENSION(:,:), ALLOCATABLE :: YSCAT,YFRQ,BFD_QUAD
 REAL PULSE,FREQ(NFRQ),T0_MIN,WF(NFRQ),SWX(NSX),SWY(NSX,3),COSTRN,T, &
      YCUM(NCHNL),TOPN(NCHNL),TCLS(NCHNL),TRP(NTYRP),BTD(NCHNL,NSTAT,3), &
      OMEGA(NFRQ)
 COMPLEX BFD(NFRQ,NSTAT,3)

 T0_MIN = 0.1 / MAXVAL (FREQ)
 BTD = 0.

 ALLOCATE (YSCAT(4,NTYRP),YFRQ(4,NFRQ),BFD_QUAD(NFRQ,3))
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
 WF = LOG(OMEGA)
 OMEGA = -OMEGA     ! Division by -iw for step response.

 DO JS = 1,NSTAT    ! Station loop.
   DO JF = 1, NFRQ
     BFD_QUAD(JF,1) = AIMAG(BFD(JF,JS,1)) / OMEGA(JF)
     BFD_QUAD(JF,2) = AIMAG(BFD(JF,JS,2)) / OMEGA(JF)
     BFD_QUAD(JF,3) = AIMAG(BFD(JF,JS,3)) / OMEGA(JF)
   END DO

   ! Above: Conversion from impulse to step current turn-off.
   ! For each component at each receiver station, compute the TOTAL response
   ! by splining the imaginary part of the frequency-domain response, converting
   ! it to time-domain step function response and folding the NPULS bipolar decay
   ! curve into a combined pulse decay curve of length PULSE. Convolve this with
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
 DEALLOCATE (BFD_QUAD,YSCAT,YFRQ)

 END SUBROUTINE TDEM_3D

!============================================================================

 SUBROUTINE FOLD_AND_CONVOLVE (STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,TRP,NTYPLS, &
                               NTYRP,NCHNL,TOPN,TCLS,YPLS,GSTRP,ASTRP,YCUM)

!----------------------------------------------------------------------------
!
!*** Called by TDEM3D
!*** Calls: CUBVAL,CUBSPL,TXCNVD,TXCNVL,TQSTRIP
!
!  Computes the "observed" response YCUM by convolving the splined earth
!  response function, YPLS, with the TX waveform.
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
!----------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER JT,NTYRP,NTYPLS,NPULS,IDER,STEP,NSX,NCHNL,JGL,JP,GSTRP,ASTRP,MXCNV,IPL
 REAL PULSE,TRP(NTYRP),SWX(NSX),SWY(NSX,3),TOPN(NCHNL),TCLS(NCHNL),T1,T2,WIDTH, &
      TF,TFH,HWIDTH,YC1,TC(3),GLX(3),GLW(3),YPLS(4,NTYRP),X,XP,YCUM(NCHNL),CUBVAL, &
      CUBDER,TXCNVL,TXCNVD,WT,FOLD(NTYPLS),YCNV(4,NSX)
 DATA GLW(1:3) /.5555556, .8888889, .5555556/, GLX(1:3) /-.7745967, 0., .7745967/

 ! Accumulate the results of NPULS bipolar cycles by splining the instantaneous
 ! response and folding the positive and negative parts of each cycle back
 ! into a single pulse.

 CALL CUBSPL (TRP,YPLS,NTYRP,0,0)

 FOLD = 0.
 IPL = 1

 IF ( NPULS == 1 ) IPL = 0

 IF ( STEP == 1 ) THEN
   DO JT = 1,NTYPLS
     X = TRP(JT)
     XP = X + PULSE
     FOLD(JT) = CUBVAL (TRP,YPLS,NTYRP,X) - IPL * CUBVAL (TRP,YPLS,NTYRP,XP)
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
     FOLD(JT) = IPL * CUBDER (TRP,YPLS,NTYRP,XP) - CUBDER (TRP,YPLS,NTYRP,X)
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

 ! Begin convolution. If Geotem / Questem primary field stripping is required
 ! the convolution must be done for all points in the waveform.
 ! Otherwise, convolve only for those points needed in the windows.

 ! The layered earth field is in IMPULSE form if dB/dt is desired
 ! or in STEP form if B is to be computed.

 MXCNV = NTYPLS + NSX
 TF = SWX(NSX)
 TFH = 0.5 * TF

 IF ( GSTRP == 1 ) CALL TQSTRIP (IDER,NTYPLS,TRP,YPLS,NSX,SWX,SWY,YCNV)

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

     IF ( GSTRP == 1 .AND. T1 < TF ) THEN

       YC1 = CUBVAL (SWX,YCNV,NSX,T1)

     ELSE IF ( IDER == 0 ) THEN ! Waveform input as I or B field (derived dI/dt).

       YC1 = TXCNVL (T1,NTYPLS,TRP,YPLS,NSX,SWX,SWY)
       IF ( ASTRP == 1 .AND. T1 < TF ) THEN ! Asymmetric stripping.
         T2 = T1 - TFH
         YC1 = YC1 + TXCNVL (T2,NTYPLS,TRP,YPLS,NSX,SWX,SWY)
       END IF

     ELSE IF ( IDER == 1 ) THEN ! Waveform input as voltage (known dI/dt).

       YC1 = TXCNVD (MXCNV,T1,NTYPLS,TRP,YPLS,NSX,SWX,SWY)

     ELSE IF (IDER == 4) THEN ! Pure on-time step.

       YC1 = SWY(1,1) * CUBVAL (TRP,YPLS,NTYPLS,T1)

     END IF

     YCUM(JT) = YCUM(JT) + (WT * YC1)

   END DO

 END DO ! End channel loop.

 END SUBROUTINE FOLD_AND_CONVOLVE

!============================================================================

 SUBROUTINE TQSTRIP (IDER,NTYPLS,TRP,YPLS,NSX,SWX,SWY,YCNV)

!----------------------------------------------------------------------------
!
!*** Called by: FOLD_AND_CONVOLVE
!*** Calls: CUBINT,CUBSPL,CUBVAL
!
!  A stripped down version of TXCNVD is used to convolve the earth response
!  with the receiver waveform for every point on that waveform.
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
!----------------------------------------------------------------------------

 IMPLICIT NONE
 REAL, PARAMETER :: T0_MIN=1.E-7
 INTEGER IDER,NTYPLS,NSX,JT,MXCNV
 REAL T,TRP(NTYPLS),YPLS(4,NTYPLS),SWX(NSX),SWY(NSX,3),YCNV(4,NSX),TXCNVL, &
      TXCNVD,A1,B1,ALPHA

 A1 = 0.
 B1 = 0.
 YCNV = 0.

 MXCNV = NTYPLS + NSX

 DO JT = 2,NSX ! Convolve NSW points using the derived waveform.

   T = SWX(JT)

   IF ( T < T0_MIN ) CYCLE

   IF ( IDER == 0 ) THEN     ! Waveform input as I or B field (derived dI/dt)
     YCNV(1,JT) = TXCNVL (T,NTYPLS,TRP,YPLS,NSX,SWX,SWY)
   ELSE                      ! Waveform input as voltage (known dI/dt)
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

!============================================================================

 REAL FUNCTION TXCNVD (MXCNV,T,NTYPLS,TRP,YPLS,NSX,SWX,SWY)

!----------------------------------------------------------------------------
!
!***  Called by FOLD_AND_CONVOLVE
!***  Calls CUBINT, CUBSPL, CUBVAL, LINVAL, TXCMRG
!
!  Convolves impulse B (step dB/dt) earth response function (ERF) with the
!  specified derivative of the source waveform at NSX points to produce
!  the system dB/dt response of the earth.
!
!       MXCNV = NTYPLS + NSX
!           T = convolution time in sec measured from the beginning
!               of the source waveform.
!   TRP, YPLS = abscissa & ordinate values of earth response function to
!               be convolved.
!      NTYPLS = number of values in TRP and YPLS
!         SWX = abscissa of time values of source waveform in sec.
!         SWY = dI/dt values derived from receiver dB/dt.
!         NSX = number of points in SWX & in each waveform stored in SWY
!
!  Defining T1 = MIN {T, signal length,}, the convolution is formally
!  computed as
!
!   TXCNVD (T) = INT (T0 -> T) { YPLS (tau) * SWY (T-tau)  d tau }

!  where T0 = MAX { TRP(1), T - SWX (NSX)}
!
!  ONTIME RESPONSE:
!  ----------------
!
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
!
!----------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER MXCNV,NTYPLS,NSX,N1,J1,N2,J2,NCNV
 REAL T,TC,T0,TRP(NTYPLS),YPLS(4,NTYPLS),SWX(NSX),SWY(NSX,3),YCNV(4,MXCNV), &
      XCNV(MXCNV),X1(MXCNV),Y1(MXCNV),X2(MXCNV),Y2(MXCNV),CUBVAL,CUBINT,LINVAL

 ! Set up X1,Y1, the N1 values of SWX, SWY * YPLS for signal ontime < T.
 ! where X1, the conjugate signal time, contains T-SWX values.
 ! Set up X2,Y2, the N2 values of TRP, YPLS * SWY for ERF points  <= T.

 N1 = 0
 DO J1 = NSX, 1, -1
   TC = T - SWX(J1)
   IF ( TC < 0. ) CYCLE
   N1 = N1 + 1
   X1(N1) = TC
   Y1(N1) = SWY(J1,1) * CUBVAL (TRP,YPLS,NTYPLS,TC)
 END DO

 T0 = T - SWX(NSX)
 T0 = MAX (T0, TRP(1))/ 1.0001
 N2 = 0
 DO J2 = 1,NTYPLS
   IF ( (TRP(J2) > T0) .AND. (TRP(J2) < T) ) THEN
     N2 = N2 + 1
     X2(N2) = TRP(J2)
     TC = T - TRP(J2)
     Y2(N2) = YPLS(1,J2) * LINVAL(NSX,SWX,SWY,TC,1)
   END IF
 END DO

 ! Merge the two lists into XCNV, YCNV of length NCNV.
 ! Then spline and integrate

 CALL TXCMRG (MXCNV,X1,Y1,N1,X2,Y2,N2,XCNV,YCNV,NCNV)

 CALL CUBSPL (XCNV,YCNV,NCNV,0,0)

 TXCNVD = CUBINT (XCNV,YCNV,NCNV,T0,T)

 END FUNCTION TXCNVD

!============================================================================

 REAL FUNCTION TXCNVL (T,NTYPLS,TRP,YPLS,NSX,SWX,SWY)

!----------------------------------------------------------------------------
!
!***  Called by FOLD_AND_CONVOLVE
!***  Calls CUBINT, CUBVAL
!
!  Computes the system dB/dt response by convolving the computed dI/dt with
!  the impulse B response of the earth. For step current drops, system dB/dt
!  is computed asthe product of instantaneous current drop times the
!  earth step dB/dt.
!
!  This routine assumes that the source waveform is composed of NSX linear
!  segments. Thus NSX-1 constant dI/dt values are contained in SWY(*,1).
!
!  The input earth response function (step dB/dt or equivalently, impulse B)
!  must be contained in a splined array of NTYPLS values of time (abscissa) TRP
!  and ordinate YPLS.  System dB/dt is computed by integrating YPLS between
!  the SWX points of constant dI/dt segments.
!
!----------------------------------------------------------------------------
!
!      INPUT / OUTPUT:
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
!----------------------------------------------------------------------------

 IMPLICIT NONE
 REAL, PARAMETER :: T0_MIN=1.E-7
 INTEGER NTYPLS,NSX,JT
 REAL T,TF,CNV,TB,DELT,SEG,TRP(NTYPLS),YPLS(4,NTYPLS),SWX(NSX),SWY(NSX,3),TEND, &
      CUBINT,CUBVAL
 LOGICAL DER

 TF = T - TRP(1)
 CNV = 0.

 DO JT = 2, NSX

   IF ( SWX(JT-1) > TF ) EXIT

   TB = T - MIN (TF, SWX(JT))
   DELT = SWX(JT) - SWX(JT-1)
   DER = .FALSE.

   IF ( DELT > T0_MIN ) THEN
     TEND = T - SWX(JT-1)
     DER = .TRUE.
   END IF

   ! For an instantaneous step drop in current, SEG is YPLS times SWY(*,2),
   ! since YPLS is already the dB/dt step response. Otherwise SEG is the
   ! integral of YPLS * constant dI/dt SWY(*,1) since YPLS is also impulse B.

   IF ( DER ) THEN
     SEG = SWY(JT,1) * CUBINT (TRP,YPLS,NTYPLS,TB,TEND)
   ELSE
     SEG = SWY(JT,2) * CUBVAL (TRP,YPLS,NTYPLS,TB)
   END IF

   CNV = CNV + SEG

 END DO

 TXCNVL = CNV

 END FUNCTION TXCNVL

!============================================================================

 SUBROUTINE TXCMRG (MXCNV,X1,Y1,N1,X2,Y2,N2,XCNV,YCNV,NCNV)

!----------------------------------------------------------------------------
!
!*** Called by: TXCNVD
!
!  Merges two previously sorted list pairs X1, Y1 of length N1 and X2, Y2 of
!  length N2 into list pair XCNV, YCNV of length NCNV into ascending values of
!  XCNV.
!
!----------------------------------------------------------------------------

 IMPLICIT NONE
 REAL, PARAMETER :: TOL = 1.E-3
 INTEGER MXCNV,N1,N2,NCNV,K1,K2,N,J1
 REAL DELT,TL1,XCNV(MXCNV),X1(MXCNV),Y1(MXCNV),X2(MXCNV),Y2(MXCNV), &
      YCNV(4,MXCNV)
 LOGICAL LIST1,LIST2

 LIST1 = .TRUE.
 LIST2 = .TRUE.
 K1 = 1
 K2 = 1
 N = N1 + N2

 DO J1 = 1, N
   IF ( LIST1 .AND. LIST2 ) THEN
     IF ( X1(K1) < X2(K2) ) THEN
       XCNV(J1) = X1(K1)
       YCNV(1,J1) = Y1(K1)
       K1 = K1 + 1
       IF ( K1 > N1 ) LIST1 = .FALSE.
     ELSE
       XCNV(J1) = X2(K2)
       YCNV(1,J1) = Y2(K2)
       K2 = K2 + 1
       IF ( K2 > N2 ) LIST2 = .FALSE.
     END IF
   ELSE IF ( LIST1 ) THEN
     XCNV(J1) = X1(K1)
     YCNV(1,J1) = Y1(K1)
     K1 = K1 + 1
     IF ( K1 > N1 ) LIST1 = .FALSE.
   ELSE IF ( LIST2 ) THEN
     XCNV(J1) = X2(K2)
     YCNV(1,J1) = Y2(K2)
     K2 = K2 + 1
     IF ( K2 > N2 ) LIST2 = .FALSE.
   END IF
 END DO

 NCNV = 1 ! Clean up list.
 DO J1 = 2, N
   DELT = XCNV(J1) - XCNV(NCNV)
   TL1 = TOL * XCNV(J1)
   IF ( DELT > TL1 ) THEN
     NCNV = NCNV + 1
     XCNV(NCNV) = XCNV(J1)
     YCNV(1,NCNV) = YCNV(1,J1)
   END IF
 END DO

 END SUBROUTINE TXCMRG

!===========================================================================!
!                                                                           !
!           Subroutines for writing data and information to file.           !
!                                                                           !
!===========================================================================!

 SUBROUTINE WRITE_LOG_FILE (NLG,MSG,MXERR,ERR_LVL)

!----------------------------------------------------------------------------
!
! This subroutine prints out warning and fatal error messages on the LOG
! file.
!
! NLG = output unit index
! MSG refers to error message index
! ERR_LVL = 1 for warnings;  = 2 for fatal errors
! MXERR = MAX (ERR_LVL)
!
!----------------------------------------------------------------------------

 INTEGER ERR_LVL,MSG,NLG,MXERR

 IF (MXERR == 0) OPEN (NLG,FILE = 'ArjunAir.log',STATUS = 'REPLACE')

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
 IF (MSG == 25) WRITE(NLG,25)
 IF (MSG == 50) WRITE(NLG,50)
 IF (MSG == -99) WRITE(NLG,'(/)')
 IF (MSG == 201) WRITE(NLG,201)
 IF (MSG == 202) WRITE(NLG,202)
 IF (MSG == 203) WRITE(NLG,203)
 IF (MSG == 204) WRITE(NLG,204)
 IF (MSG == 205) WRITE(NLG,205)
 IF (MSG == 207) WRITE(NLG,206)
 IF (MSG == 207) WRITE(NLG,207)
 IF (MSG == 208) WRITE(NLG,208)
 IF (MSG == 209) WRITE(NLG,209)
 IF (MSG == 210) WRITE(NLG,210)
 IF (MSG == 211) WRITE(NLG,211)

   1 FORMAT(/T3,'The value for TDFD is outside the permitted range.' &
            /T3,'The allowed values are: 0 & 1 for time-domain or 2 for frequency domain.'/)
   2 FORMAT( T3,'The value for DO3D is outside the permitted range.'         &
            /T3,'For time-domain options, the allowed values are -1, 1 or 2' &
            /T3,'For frequency-domain options, the allowed values are -1, or 1')
   3 FORMAT(/T3,'UNITS must be 1, 2 or 3')
   4 FORMAT(/T3,'The value for ISW is outside the permitted range.  The allowed ' &
            /T3,'absolute values are: 1, 10, 11, 30, 31, 130, 131, or 4.'/)
   5 FORMAT( T3,'The value for STEP is outside the permitted range.' &
            /T3,'The allowed values are: 0 or 1.'/)
   6 FORMAT(/T3,'The value for KRXW is outside the permitted range.' &
            /T3,'The allowed values are: 1 or 2.'/)
   7 FORMAT(/T3,'This value for TOPN is outside the permitted range.' &
            /T3,'It must be > 0.'/)
   8 FORMAT(/T3,'For time-domain inversion CMP must be 11, 13, 2, 3 or 4.')
   9 FORMAT(/T3,'CMP must be 11, 13, 2, 3 for time-domain forward modelling, .' &
            /T3,'It has been reset to 2')
  10 FORMAT(/T3,'KPPM must be 0, 1, 3, 123, or 4.')
  11 FORMAT(/T3,'B output is not allowed for ISW = 10, 30, or 130', &
            /T3,'STEP has been reset to 0')
  12 FORMAT(/T3,'dB/dt output is not allowed for ISW = 11, 31, or 131', &
            /T3,'STEP has been reset to 1')
  13 FORMAT(/T3,'Frequency-domain inversion is allowed only for the maximally coupled component.' &
            /T3,'CMP must = 1')
  14 FORMAT(/T3,'CMP must be 1, 2 or 3 for frequency-domain forward modelling, .' &
            /T3,'It has been reset to 1')
  15 FORMAT(/T3,'There must be at least 2 survey stations.')
  16 FORMAT(/T3,'SURVEY must = 2 for frequency domain.' &
            /T3,'The SURVEY = 1 option has been eliminated from this version.')
  17 FORMAT(/T3,'SURVEY must = 2 or 3.  The SURVEY = 1 option does not exist in this version.')
  18 FORMAT(/T3,'ArjunAir allows only one line at a time to be modelled or inverted.' &
            /T3,'The flight path should reasonably approximate a staight line.')
  19 FORMAT(/T3,'All receiver positions on the flight line must be within the horizontal limits' &
            /T3,'of the mesh.  Either remove points from the flight line or expand the mesh.')
  20 FORMAT(/T3,'The restart option is not available for frequency domain.' &
            /T3,'DO3D has been reset to 1')
  50 FORMAT( T3,'This warning is generated, because you are using more than 60,000 cells')

 201 FORMAT(/T3,'CNVRG must be 1 or 2.  It has been reset to 1.')
 202 FORMAT(/T3,'X component inversion was requested but ArjunAir.inv contains only Z component data.')
 203 FORMAT(/T3,'Z component inversion was requested but ArjunAir.inv contains only X component data.')
 204 FORMAT(/T3,'X & Z component inversion was requested but ArjunAir.inv contains only X component data.')
 205 FORMAT(/T3,'X & Z component inversion was requested but ArjunAir.inv contains only Z component data.')
 206 FORMAT(/T3,'3 component inversion was requested but ArjunAir.inv contains only X component data.')
 207 FORMAT(/T3,'3 component inversion was requested but ArjunAir.inv contains only Z component data.')
 208 FORMAT(/T3,'3 component inversion was requested but ArjunAir.inv contains no Y component data.')
 209 FORMAT(/T3,'There is a component discrepency between the cfl and inv files.')
 210 FORMAT(/T3,'KCMP is restricted to the values: 1, 3, 13, 31, 123, 312')
 211 FORMAT(/T3,'Order must be 1122 (for IIQQ),  1212 (for IQIQ),  2211 (for QQII),  2121 (for QIQI)')

   25 FORMAT(/T6,'All transmitter positions are invalid for one or more', &
             /T6,'of the following reasons:' &
            //T3,'1. The transmitter is underground' &
             /T3,'2. The receiver is underground' &
             /T3,'3. The transmitter is not over the defined mesh position')
 501 FORMAT(/T2,'WARNING'/T2,'-------'/)
 502 FORMAT(/T2,'FATAL ERROR'/T2,'----- -----'/)


END SUBROUTINE  WRITE_LOG_FILE

!============================================================================

 SUBROUTINE WRITE_FD (NW,np,TITLE,NSTAT,NSTATQ,LINE,TXCLN,SXD,SYD,SZ,RXD,RYD,RZ, &
                      RX,RXQ,NRX,CONFIG,NFRQ,FREQ,PRFL,QUNIT,PPFAC,BFFAC,PRM_FD,CMP, &
                      BFDQ)

!--------------------------------------------------------------------------------------

!***  Called by: MAIN
!***      Calls: WRSLV_FD

!  Prints the results of FEM computations.
!
!                NW - output unit number
!               np - unit number for mf1 file
!             NSTAT - Number of stations
!              LINE - Line number
!         TXCLN(JF) - transmitter orientation at frequency JF (in radians)
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
!       BFD(JF,JS,JC) - Total magnetic field for frequency JF, source position JS,
!                       component JC (nT) in aircraft components

 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80)
 INTEGER NW,np,NSTAT,NSTATQ,LINE,NFRQ,CMP,PRFL,TXD(NFRQ),JF,JS,NRX
 REAL FREQ(NFRQ),RZ(NSTAT,NFRQ),SZ(NSTAT),MPZ1(NSTAT),PRM_FD(NFRQ),PRM4,NORM(NFRQ), &
      YTR(NFRQ,NSTAT),TXCLN(NFRQ),PPFAC,BFFAC,RX(NSTAT,NRX),RXQ(NSTATQ,NRX)
 REAL(KIND=QL) RXD(NSTAT,NFRQ),RYD(NSTAT,NFRQ),MXD(NSTAT),MYD(NSTAT),SXD(NSTAT),SYD(NSTAT)
 COMPLEX BFDQ(NFRQ,NSTATQ,3)
 COMPLEX, ALLOCATABLE, DIMENSION(:,:,:) :: BFD1,BFD2
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
 !                   vertical fields(3) respectively (in pT) for
 !                   frequency JF and station JS

 !   BFD1(JF,JS,4) will contain the maximally coupled field (ppm)
 !                 normalised to the parallel primary component.

 ! Print results at Tx-Rx midpoint.

 MXD(1:NSTAT) = (RXD(1:NSTAT,1) + SXD(1:NSTAT)) / 2.
 MYD(1:NSTAT) = (RYD(1:NSTAT,1) + SYD(1:NSTAT)) / 2.
 MPZ1(1:NSTAT) = (RZ(1:NSTAT,1) + SZ(1:NSTAT))  / 2.

 ALLOCATE (BFD1(NFRQ,NSTATQ,4)) ; BFD1 = CMPLX(0.,0.)

 IF (CMP == 1) WRITE(NW,10)
 DO JF = 1, NFRQ
   PRM4 = BFFAC * ABS (PRM_FD(JF))                   ! coupled primary in nT, pT or fT
   NORM(JF) = PPFAC / PRM4                         ! for pct, ppt, ppm or ppb output

   IF (CMP == 1) WRITE(NW,'(I6,T17,G12.4,T34,g12.4)')  JF,PRM4,NORM(JF)

   ! NOTE The - sign below is a consequence of using the sine transform for a +iwt
   !      sign convention.  It is thus consistant with the convention used for the
   !      layered half space and in TDEM for time-domain scattered fields.
   !      The check is that over a layer, the coplanar and coaxial response for a
   !      Dighem configuration should be positive, at least at low frequencies.

   BFD1(JF,1:NSTATQ,1) = -BFFAC * BFDQ(JF,1:NSTATQ,1) ! Total field in nT, pT or fT
   BFD1(JF,1:NSTATQ,2) = -BFFAC * BFDQ(JF,1:NSTATQ,2)
   BFD1(JF,1:NSTATQ,3) = -BFFAC * BFDQ(JF,1:NSTATQ,3)

 END DO

 ALLOCATE (BFD2(NFRQ,NSTAT,4)) ; BFD2 = (0.,0.)

 CALL SET_DATA_FD (NFRQ,4,NSTATQ,NSTAT,BFD1,RXQ,RX,BFD2,NRX)

 DEALLOCATE (BFD1)

 ! Normalise the fields as indicated in input data file.

 ! For CMP = 1, compute component along Tx direction

 TXD(1:NFRQ) = NINT ( 180. * TXCLN(1:NFRQ) / 3.1416)

 WRITE(NW,1)

 IF (CMP > 1) WRITE(NW,8)
 IF (CMP > 2) WRITE(NW,9)

 ! Maximally coupled response.

 DO JF = 1, NFRQ
   BFD2(JF,1:NSTAT,4) =   BFD2(JF,1:NSTAT,1) * SIN (TXCLN(JF)) &
                        + BFD2(JF,1:NSTAT,3) * COS (TXCLN(JF))
   BFD2(JF,1:NSTAT,4) = NORM(JF) * BFD2(JF,1:NSTAT,4)
 END DO

 IF (CMP == 1) THEN
   WRITE(NW,15) TRIM (TITLE)
   WRITE(NW,3)

   WRITE(NW,4) QR,TRIM (QUNIT)
   YTR(1:NFRQ,1:NSTAT) = REAL (BFD2(1:NFRQ,1:NSTAT,4))
   CALL WRSLV_FD (NW,PRFL,MXD,MYD,MPZ1,NSTAT,NFRQ,FREQ,TXD,CMP,CONFIG,YTR)

   WRITE(NW,4) QI,TRIM (QUNIT)
   YTR(1:NFRQ,1:NSTAT) = AIMAG (BFD2(1:NFRQ,1:NSTAT,4))
   CALL WRSLV_FD (NW,PRFL,MXD,MYD,MPZ1,NSTAT,NFRQ,FREQ,TXD,CMP,CONFIG,YTR)
 END IF

 IF (CMP > 1) THEN
   WRITE(NW,'(/3X,A)') TRIM (TITLE)     !  Vertical component
   WRITE(NW,14) QR,CMP3,QUNIT
   YTR(1:NFRQ,1:NSTAT) = REAL(BFD2(1:NFRQ,1:NSTAT,3))
   CALL WRSLV_FD (NW,PRFL,MXD,MYD,MPZ1,NSTAT,NFRQ,FREQ,TXD,CMP,CONFIG,YTR)

   WRITE(NW,14) QI,CMP3,QUNIT
   YTR(1:NFRQ,1:NSTAT) = AIMAG(BFD2(1:NFRQ,1:NSTAT,3))
   CALL WRSLV_FD (NW,PRFL,MXD,MYD,MPZ1,NSTAT,NFRQ,FREQ,TXD,CMP,CONFIG,YTR)

   WRITE(NW,'(//3X,A)') TRIM (TITLE)     !  In-line component
   WRITE(NW,14) QR,CMP1,QUNIT
   YTR(1:NFRQ,1:NSTAT) = REAL (BFD2(1:NFRQ,1:NSTAT,1))
   CALL WRSLV_FD (NW,PRFL,MXD,MYD,MPZ1,NSTAT,NFRQ,FREQ,TXD,CMP,CONFIG,YTR)

   WRITE(NW,14) QI,CMP1,QUNIT
   YTR(1:NFRQ,1:NSTAT) = AIMAG (BFD2(1:NFRQ,1:NSTAT,1))
   CALL WRSLV_FD (NW,PRFL,MXD,MYD,MPZ1,NSTAT,NFRQ,FREQ,TXD,CMP,CONFIG,YTR)
 END IF

 IF (CMP == 3) THEN
   WRITE(NW,'(//3X,A)') TRIM (TITLE)     ! Transverse component
   YTR(1:NFRQ,1:NSTAT) = REAL(BFD2(1:NFRQ,1:NSTAT,2))
   WRITE(NW,14) QR,CMP2,QUNIT
   CALL WRSLV_FD (NW,PRFL,MXD,MYD,MPZ1,NSTAT,NFRQ,FREQ,TXD,CMP,CONFIG,YTR)

   YTR(1:NFRQ,1:NSTAT) = AIMAG(BFD2(1:NFRQ,1:NSTAT,2))
   WRITE(NW,14) QI,CMP2,QUNIT
   CALL WRSLV_FD (NW,PRFL,MXD,MYD,MPZ1,NSTAT,NFRQ,FREQ,TXD,CMP,CONFIG,YTR)
 END IF

!  Finish writing ArjunAir.mf1

 IF (CMP > 1) RETURN
 DO JS = 1,NSTAT
   IF (JS == 1) THEN
     WRITE(QL0,'(I10)') LINE
     READ(QL0,'(A)') QL1
     WRITE(np,'(2A)') 'Line ',TRIM (ADJUSTL (QL1))
   END IF

   WRITE(np,20) JS,SXD(JS),SYD(JS),SZ(JS),RXD(JS,1),RYD(JS,1),RZ(JS,1), &
                 REAL (BFD2(1:NFRQ,JS,4)), AIMAG (BFD2(1:NFRQ,JS,4))
 END DO

 DEALLOCATE (BFD2)

 1 FORMAT(/T3,'FREQUENCY-DOMAIN ArjunAir OUTPUT' /T3,33('-'))
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
 20 FORMAT(I5,2F12.1,F8.1,2F12.1,F8.1,1024(2x, en15.6))

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
       WRITE(NW,3) JS,KRX,KRY,KRZ,YTR(1:NFRQ,JS)
     ELSE
       WRITE(NW,6) JS,KRX,KRY,KRZ,YTR(1:NFRQ,JS)
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
   WRITE(NW,'(T13,A,F9.0,39F13.0)') 'Positions  E',MXD(JB:JF)
   WRITE(NW,'(T24,A,F9.0,39F13.0)') 'N',MYD(JB:JF)
   WRITE(NW,'(T7,A)') 'Freq      TXCLN'
   WRITE(NW,'(3X)')

!dir$ loop count min(256)
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

 SUBROUTINE WRITE_TD (NW,np,TITLE,NSTAT,NSTATQ,LINE,SXD,SYD,SZ,TXDEG,RXD,RYD,RZ, &
                      RX,RXQ,NRX,XRX,YRX,ZRX,NCHNL,TMS,PRFL,QUNIT,BUNIT,PPFAC,BFFAC, &
                      PRM_TD,CMP,KPPM,BTDQ)

!----------------------------------------------------------------------------------------------

!***  Called by: MAIN
!***      Calls: WRSLV
!
!  Prints the results of TEM computations.
!
!                NW - output unit number
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

 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80)
 REAL, PARAMETER ::  TOL=1.E-3
 INTEGER NW,np,PRFL,NSTAT,NSTATQ,LINE,NCHNL,MCHNL,CMP,KPPM,JC,JS,NRX
 REAL TMS(NCHNL),RZ(NSTAT,1),PRM_TD(3),XBD,ZBD,XRN,NORM(4),PPFAC,BFFAC, &
      YTR(NCHNL,NSTAT),XQ(4),RX(NSTAT,NRX),RXQ(NSTATQ,NRX)
 REAL, DIMENSION(NSTAT) :: SZ,XRX,YRX,ZRX,TXDEG
 REAL, DIMENSION(NCHNL,NSTATQ,3) :: BTDQ
 REAL, DIMENSION(NCHNL,NSTAT,3) :: BTD
 REAL, ALLOCATABLE :: QDATA(:)
 REAL(KIND=QL) SXD(NSTAT),SYD(NSTAT),RXD(NSTAT,1),RYD(NSTAT,1)
 LOGICAL PRTYBD,PRTX,PRTY,PRTZ
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
 IF (CMP /= 3)  PRTY = .FALSE.
 IF (CMP == 11) PRTZ = .FALSE.
 IF (CMP == 13) PRTX = .FALSE.

 NORM = 1.
 IF (KPPM == 0) THEN                 !  Compute fields in requied units.
   BTDQ = BFFAC * BTDQ
 ELSE IF (KPPM > 0) THEN             !  Compute normalised response.
   NORM(1) = ABS (PRM_TD(1))
   NORM(2) = ABS (PRM_TD(2))
   NORM(3) = ABS (PRM_TD(3))
   NORM(4) = SQRT (NORM(1)**2 + NORM(2)**2 + NORM(3)**2)
   DO JC = 1,3
     XRN = NORM(JC) / NORM (4)
     IF (XRN < TOL) NORM(JC) = NORM(4)
   END DO
   IF (KPPM == 4) NORM(1:3) = NORM(4)                   ! Total primary normalisation.
   IF (KPPM == 1 .OR. KPPM == 3) NORM(1:3) = NORM(KPPM) ! Vertical or in-line normalisation.
   DO JC = 1,3
     BTDQ(1:NCHNL,1:NSTATQ,JC) = PPFAC * BTDQ(1:NCHNL,1:NSTATQ,JC) / NORM(JC)
   END DO
 END IF

 BTD = 0.
 CALL SET_DATA_TD (NCHNL,3,NSTATQ,NSTAT,BTDQ,RXQ,RX,BTD,NRX)

 XBD = ABS(XRX(1))
 ZBD = ABS(ZRX(1))
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

 IF (PRTZ) THEN   !  Total vertical component
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

 IF (PRTY) THEN   !  Total transverse component
   WRITE(NW,7) CMP2,TRIM (QUNIT)
   YTR(1:NCHNL,1:NSTAT) = BTD(1:NCHNL,1:NSTAT,2)
   CALL WRSLV (NW,PRFL,RZ,RXD,RYD,NSTAT,SZ,SXD,SYD,NCHNL,TMS,YTR)
 END IF

 ! Finish writing ArjunAir.mf1

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

   IF (JS == 1) THEN
     WRITE(QL0,'(I10)') LINE
     READ(QL0,'(A)') QL1
     WRITE(np,'(2A)') 'Line ',TRIM (ADJUSTL (QL1))
   END IF
   WRITE(np,20) JS,SXD(JS),SYD(JS),SZ(JS),TXDEG(JS),RXD(JS,1),RYD(JS,1),RZ(JS,1),QDATA(1:MCHNL)

 END DO

 1 FORMAT(//T3,'TIME-DOMAIN ArjunAir OUTPUT'/T3,28('-') &
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
 12 FORMAT(/T3,'Each component is normalised to its coresponding primary field')
 13 FORMAT(/T3,'Each component is normalised to the vertical primary field')
 14 FORMAT(/T3,'Each component is normalised to the total primary field')
 15 FORMAT(//T2,'TITLE:  ',A/T2,'-----')
 20 FORMAT(I5,2F12.1,F8.1,F6.1,2F12.1,F8.1,1024(2x, en15.6))
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
     WRITE(NW,3) JS,KRX,KRY,KRZ,YTR(JB:JF,JS)
   END DO
   WRITE(NW,5)
 END DO

 1 FORMAT(T10,'TRANSMITTER POSITION',T35,30(A:,5X))
 2 FORMAT(T10,'EAST     NORTH    ALT',F10.3,29F13.3)
 3 FORMAT(I3,2I10,I7,1X,30G13.4)
 5 FORMAT (85('-')/)

 END SUBROUTINE WRSLVP

!============================================================================

 SUBROUTINE WRITE_INVMDL (NW,FINAL,ITS,NX,NZ,NAIR,NPAR,XPAR,IMPORT)

!----------------------------------------------------------------------------
!
!***  Called by: NLSQ
!
!  Write the inversion output, including importance for the model
!  parameters.
!
!----------------------------------------------------------------------------
!
!     SYNTAX:
!
!   WRITE_INVMDL (NW,NWI,FINAL,ITS,NLYR,NCELLS,XPAR,SV,IMPORT)
!
!     INPUT:
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
 INTEGER NW,ITS,JC1,I1,J1,NX,NZ,NAIR,NPAR,I2,J2
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
   DO I1 = 3, NX-3
     DO J1 = NAIR+1, NZ-3
       JC1 = (I1-3)*(NZ-NAIR-3) + (J1-NAIR) ! Model non-air cell number.
       RES = XPAR(JC1)
       IMP = IMPORT(JC1)
       I2 = I1 - 2
       J2 = J1 - NAIR
       WRITE(NW,4) I2,J2,JC1,RES,IMP
     END DO
   END DO
 ELSE IF (.NOT. FINAL) THEN ! Only write intermediate model.
   WRITE(NW,5) ! Write output header.
   DO I1 = 3, NX-3
     DO J1 = NAIR+1, NZ-3
       JC1 = (I1-3)*(NZ-NAIR-3) + (J1-NAIR) ! Model non-air cell number.
       RES = XPAR(JC1)
       I2 = I1 - 2
       J2 = J1 - NAIR
       WRITE(NW,6) I2,J2,JC1,RES
     END DO
   END DO
 END IF

 ! Inversion format.

 1 FORMAT(//T9,'FINAL MODEL AFTER ',I3,' ITERATIONS' &
           /T9,'===================================')
 2 FORMAT(//T9,'MODEL DESCRIPTION AFTER ',I3,' ITERATIONS' &
           /T9,'=========================================')
 3 FORMAT(//T9,'------------------------------------------------------------' &
           /T9,'  Element    Element    Element     Element     Parameter   ' &
           /T9,'  Column       Row       Number   Resistivity   Importance  ' &
           /T9,'                                    (Ohm-m)                 ' &
           /T9,'------------------------------------------------------------')
 4 FORMAT(T12,I4,'       ',I4,'       ',I4,'      ',G12.4,'  ',G12.4)
 5 FORMAT(//T9,'-------------------------------------------------------' &
           /T9,'  Element       Element       Element        Element   ' &
           /T9,'  Column          Row         Number       Resistivity ' &
           /T9,'                                              (Ohm-m)  ' &
           /T9,'-----------   -----------   -----------   -------------')
 6 FORMAT(T12,I4,'          ',I4,'          ',I4,'        ',G12.4)

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

!===========================================================================!
!                                                                           !
! Structure of the inversion subroutines                                    !
! --------------------------------------                                    !
!                                                                           !
! ArjunAir can solve for either the over-determined (NDATA > NPAR) or the   !
! under-determined (NDATA < NPAR) inverse problem. This makes no difference !
! to the way the Jacobian matrix is constructed. It does make a difference  !
! however to the way the Jacobian matrix is decomposed via S.V.D.           !
!                                                                           !
! In the following inversion routines, there are subroutines specific to    !
! both the over- and under-determined inverse problems. These relate to the !
! SOLVE_UNDER and SOLVE_OVER (for damping singular values and solving       !
! updated model parameters) and SV_ANALYSIS_UNDER and SV_ANALYSIS_OVER (for !
! parameter importance analysis).                                           !
!                                                                           !
! The general structure of the subroutines are as follows:                  !
!                                                                           !
! NLSQ               - Nonlinear least squares                              !
!       RESJAC       - Residual vector and Jacobian matrix construction     !
!       ESVD         - Singular value decomposition of the Jacobian matrix  !
!       SOLVE        - Solve for updated model parameters                   !
!       SV_ANALYSIS  - Singular value analysis for parameter importance     !
!       NOISE_2_SIGR - Compute the noise to signal ratio for final model    !
!                                                                           !
! For ArjunAir inversion, RESJAC computes the residual vector using the     !
! ArjunAir forward modelling algorithm, and computes the Jacobian matrix    !
! using the adjoint operator method.                                        !
!                                                                           !
!===========================================================================!

 SUBROUTINE NLSQ (NM,ND,NS,NW,np,NLG,MV1PRT,OUTPRT,MAXITS,CNVRG,PCTCNV,NDATA, &
                  RDATA,RWTS,DATA_FLOOR,NPAR,CXPAR,ELAS,LBND,UBND,TDFD,CMP, &
                  STEP,IDER,NSX,SWX,SWY,NTYRP,TRP,NPULS,PULSE,NTYPLS,MCHNL, &
                  NCHNL,TOPN,TCLS,GSTRP,ASTRP,NFRQ,FREQ,TXCLN,TXCLNQ,NRX, &
                  NRXST,NRXSTQ,NSTAT,NSTATQ,SXQ,SZQ,RX,RZ,RXQ,RYQ,RZQ, &
                  INV_FAIL,NX,NZ,NAIR,NXZ,NPM,NPN,IREL,LNODS,LITHD,COORD, &
                  KNRM,NORM,BFFAC,PPFAC,KPPM,PRM_FD,PRM_TD,SXD,SYD,SZ,RXD, &
                  RYD,LINE,GEOELEC_PAR,NLITH,WRITE_FWD,NCOMP,DO3D, &
                  INVERT_RX_DATA,READ_RES_FILE)

!----------------------------------------------------------------------------
!
!*** Called by: MAIN
!*** Calls: RESJAC, ESVD, SOLVE, WRITE_MODEL, WRITE_MISFIT
!
!  Nonlinear least square inversion
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
!  REFERENCES:
!
!  D.L.B. Jupp and K. Vozoff, 1975, Stable iterative methods for the inversion
!  of geophysical data: Geophysical Journal of the Royal Astronomical Society,
!  vol. 42, n. X, pp. X-X.
!
!  A.P. Raiche, D.L.B. Jupp, H. Rutter and K. Vozoff, 1985, The joint use of
!  coincident loop transient electromagnetic and Schlumberger sounding to
!  resolve layered structures: Geophysics, vol. 50, pp. 1618-1627.
!
!----------------------------------------------------------------------------
!
!     INPUT:
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
 INTEGER, PARAMETER :: IP = 0, QL = SELECTED_REAL_KIND(12,80)
 REAL, PARAMETER :: BND = 0.01, EXPND = 4., RSVT0 = 0.2, ETA = 1.E-7, &
                    TOL = 0.5E-31
 INTEGER ND,NS,NW,np,NLG,NM,MV1PRT,OUTPRT,ITS,CNVRG,NDATA,NPAR,TDFD,CMP,STEP,IDER, &
         NSX,NPULS,NTYPLS,NTYRP,NCHNL,MCHNL,GSTRP,ASTRP,NFRQ,NSTAT,NSTATQ, &
         NRXST,NRXSTQ,NRX,ICNT,MAXITS,NSV,JP,I1,J1,KLITH,NLITH,CXPAR(NLITH), &
         MODFIX(NPAR),NX,NZ,NXZ,NAIR,NPM,NPN,LNODS(NXZ,8),LITHD(NX,NZ), &
         IREL(NRX,NSTATQ),NCOMP,KNRM,KPPM,FITS,K0,LINE,MAXITS_INT,JC1,JC2, &
         RWTS(MCHNL,NSTAT),DO3D,NDATA1
 LOGICAL JCBN,RESID,INV_FAIL,FINAL,WRITE_FWD,INVERT_RX_DATA,READ_RES_FILE
 REAL PCTCNV,SUMSQ,SSQNEW,FNM,GTEST,PDRE,WGTSUM,PULSE,DELT,RSVT,ZERO,NSR, &
      SWX(NSX),SWY(NSX,3),TRP(NTYRP),FREQ(NFRQ),B1,B2,COORD(NPN,2), &
      NORM(KNRM),PPFAC,BFFAC,PRM_TD(3),PRM_FD(NFRQ),DRMS,RMSE,MP,GCRIT, &
      GEOELEC_PAR(NXZ,6),SSQR(2),RDATA(MCHNL,NSTAT),DATA_FLOOR(MCHNL),A
 REAL, DIMENSION(NPAR) :: XPAR,DELPAR,GXPAR,IMPORT,MODWGT,MUBND,MLBND
 REAL, DIMENSION(NCHNL) :: TOPN,TCLS
 REAL, DIMENSION(NLITH) :: ELAS,LBND,UBND
 REAL, DIMENSION(NSTAT) :: SZ,RZ0
 REAL, DIMENSION(NSTATQ) :: SXQ,SZQ
 REAL, DIMENSION(NRXST) :: TXCLN
 REAL, DIMENSION(NRXSTQ) :: TXCLNQ
 REAL, DIMENSION(NSTAT,NRX) :: RX,RZ
 REAL, DIMENSION(NSTATQ,NRX) :: RXQ,RYQ,RZQ
 INTEGER, ALLOCATABLE, DIMENSION(:) :: XWTS,XWTS1
 REAL, ALLOCATABLE, DIMENSION(:) :: SV,WSP,RMSERR,UTRES,XDATA,XMODL,RES,DNORM, &
                                    XDATA1,XMODL1,RES1,DNORM1
 REAL, ALLOCATABLE, DIMENSION(:,:) :: UMAT,VMAT,JAC,JACT,VT
 REAL(KIND=QL), DIMENSION(NSTAT) :: SXD,SYD
 REAL(KIND=QL), DIMENSION(NSTAT,NRX) :: RXD,RYD
 Integer :: tvals(8)

 !INVERT_RX_DATA = .FALSE.

 IF (TDFD == 2) THEN
   WRITE(NW,1) ; WRITE(*,1)
 ELSE
   IF (CMP == 11) CVAR = 'In-line component'
   IF (CMP == 13) CVAR = 'Vertical component'
   IF (CMP == 2)  CVAR = 'Joint vertical and in-line components'
   WRITE(NW,2) TRIM (CVAR) ; WRITE(*,2)  TRIM (CVAR)
 END IF
 WRITE(NW,3) MAXITS ; WRITE(*,3) MAXITS

 ! Set up data and data weight vectors, XDATA and XWTS. XDATA1 and XMODL1
 ! contain data at Rx positions for writing to ArjunAir.mv1.

 NDATA1 = NSTAT*MCHNL
 IF (INVERT_RX_DATA) THEN
   NDATA  = NSTAT*MCHNL
   WRITE(NW,27) ; WRITE(*,27)
 ELSE
   NDATA = NSTATQ*MCHNL
   WRITE(NW,28) ; WRITE(*,28)
 END IF

 ALLOCATE(XDATA(NDATA),XMODL(NDATA),RES(NDATA),DNORM(0:NDATA), &
          XWTS(NDATA),XDATA1(NDATA1),XMODL1(NDATA1),RES1(NDATA1), &
          XWTS1(NDATA1),DNORM1(NDATA1))

 XDATA = 0. ; XMODL = 0. ; RES = 0. ; DNORM = 1. ; XWTS = 1 ; XDATA1 = 0.
 XMODL1 = 0. ; RES1 = 0. ; XWTS1 = 1 ; DNORM1 = 1.

 CALL PREPARE_INV_DATA

 ! DNORM(0) = 1 : Point norm (P) used for fitting error and sensitivity matrix.
 ! DNORM(0) = 2 : Survey norm (S) used for fitting error and sensitivity matrix.

 DNORM(0) = 1.
 IF (DO3D < -1) DNORM(0) = 2.

 IF (DNORM(0) > 1.1) THEN
   WRITE(NW,25) ; WRITE(*,25)
 ELSE
   WRITE(NW,26) ; WRITE(*,26)
 END IF

 ! Set up the model parameter, weight and bounds vectors.

 IF (READ_RES_FILE) THEN
   WRITE(*,29) ; WRITE(NW,29)
   OPEN(NM,FILE='ArjunAir.res',STATUS='OLD')
   DO I1 = 3, NX - 3
     DO J1 = NAIR+1, NZ - 3
       JC1 = (I1-1)*(NZ-1) + J1
       READ(NM,*) JP, A
       GEOELEC_PAR(JC1,1) = A
     END DO
   END DO
   CLOSE(NM)
 END IF

 DO I1 = 3, NX - 3
   DO J1 = NAIR+1, NZ - 3
     KLITH = LITHD(I1,J1)
     JC1 = (I1-1)*(NZ-1) + J1
     JC2 = (I1-3)*(NZ-NAIR-3) + (J1-NAIR)
     XPAR(JC2)   = GEOELEC_PAR(JC1,1)  ! Resistivity vector.
     MODWGT(JC2) = ELAS(KLITH)         ! Conductivity weights.
     MODFIX(JC2) = CXPAR(KLITH)        ! Vector of integer flags for updating model parameters.
     MUBND(JC2)  = UBND(KLITH)         ! Conductivity upper bound vector.
     MLBND(JC2)  = LBND(KLITH)         ! Conductivity lower bound vector.
   END DO
 END DO
 GXPAR(1:NPAR) = XPAR(1:NPAR)

 ! Determine if the inversion is under-determined or over-determined and
 ! allocate appropriately.

 IF (NDATA >= NPAR) THEN
   ALLOCATE (UMAT(NDATA,NPAR),VMAT(NPAR,NPAR),SV(NPAR),WSP(3*NPAR), &
             UTRES(NPAR),JAC(NDATA,NPAR))
 ELSE
   ALLOCATE (UMAT(NPAR,NDATA),VMAT(NDATA,NDATA),SV(NDATA),WSP(3*NDATA), &
             UTRES(NDATA),JAC(NDATA,NPAR))
 END IF

 UMAT = 0. ; VMAT = 0. ; SV = 0. ; WSP = 0. ; UTRES = 0.

 ! Preset the threshold parameters.

 ZERO = BND*BND
 IF (ETA >= ZERO) ZERO = ETA
 GCRIT = SQRT(ETA)

 ! Set the relative singular value threshold, RSVT. Initialise the eigenvalue
 ! damping at 10 percent, RSVT0 = 0.1.

 RSVT = MAX(BND,RSVT0)

 ! Initialise arrays and logical flags.

 IMPORT = 0. ; INV_FAIL = .FALSE. ; FINAL = .FALSE. ; WGTSUM = REAL(SUM(XWTS))
 ITS = 0 ; MAXITS_INT = 6 ; NSV = 0 ; DRMS = 0.

 ALLOCATE(RMSERR(MAXITS)) ; RMSERR = 0.

 !------------------------------!
 ! START OF MAIN INVERSION LOOP !
 !------------------------------!

 INV_LOOP: DO ITS = 1, MAXITS

   WRITE(*,'(/T3,A,I3)') 'Begin iteration =',ITS

   WRITE(*,45) ! Compute the residual error vector (RES) and Jacobian matrix (JAC).

   JCBN = .TRUE. ; RESID = .TRUE. ; WRITE_FWD = .TRUE. ; SUMSQ = 0.
   RES = 0. ; JAC = 0. ; SSQR = 0.

   CALL RESJAC (ND,NS,NLG,NDATA,XDATA,XMODL,XWTS,NPAR,XPAR,SUMSQ,SSQR, &
             JCBN,JAC,RES,TDFD,STEP,IDER,NSX,SWX,SWY,NTYRP,TRP,NPULS, &
             PULSE,NTYPLS,NCHNL,TOPN,TCLS,GSTRP,ASTRP,NFRQ,FREQ,TXCLN, &
             TXCLNQ,CMP,NRX,NRXST,NRXSTQ,NSTAT,NSTATQ,SXQ,SZQ,RX,RXQ,RYQ, &
             RZQ,NX,NZ,NAIR,NXZ,NPN,NPM,GEOELEC_PAR,LNODS,IREL,COORD, &
             NCOMP,KNRM,NORM,RESID,BFFAC,PPFAC,KPPM,PRM_FD,PRM_TD,DNORM, &
             WRITE_FWD,INVERT_RX_DATA,XMODL1,XDATA1,XWTS1,RES1,DNORM1, &
             NDATA1,MODFIX)

   RMSERR(ITS) = 100. * SQRT(SUMSQ/WGTSUM)
   SSQR(1:2) = 100. * SQRT(SSQR(1:2)/WGTSUM)
   RES1 = 100. * RES1
   FNM = 0.01 * SQRT(SUMSQ)
   FNM = MAX(FNM,ETA)

   IF (ITS == 1) THEN
     FITS = 0
     IF (MV1PRT > 1) THEN
       WRITE(np,30) FITS,RMSERR(ITS),RSVT
     END IF
     WRITE(*,4) SSQR(1),SSQR(2),RSVT
     IF (OUTPRT > 1) WRITE(NW,4) SSQR(1),SSQR(2),RSVT
     IF (MV1PRT == 3) CALL WRITE_MDATA(FITS)
     IF (OUTPRT == 3) THEN
       IF (INVERT_RX_DATA) THEN
         CALL WRITE_MISFIT (NW,FITS,NSTAT,NDATA,MCHNL,TDFD,NFRQ, &
                            FREQ,CMP,NCHNL,RES,XMODL,XDATA)
       ELSE
         CALL WRITE_MISFIT (NW,FITS,NSTATQ,NDATA,MCHNL,TDFD,NFRQ, &
                            FREQ,CMP,NCHNL,RES,XMODL,XDATA)
       END IF
     END IF
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
       INV_FAIL = .TRUE.
       WRITE(NW,99)
       WRITE(*,99)
       RETURN
     END IF
     WRITE(*,42)

     ALLOCATE (VT(NPAR,NDATA))
     VT = TRANSPOSE(UMAT)
     UTRES = MATMUL(VT,RES)
     DEALLOCATE (VT)

     ! Solve for the update to the model parameters: DELPAR = VMAT * SV * UTRES
     ! and test for convergence on the predicted decrease. Loop over internal
     ! iterations.

     ICNT_LOOP1: DO ICNT = 1, MAXITS_INT

       IF (FINAL) EXIT ICNT_LOOP1

	   Call Date_and_Time(Values = tvals)
       WRITE(*,44) tvals(5:7), tvals(3:1:-1), ICNT, MAXITS_INT

       ! Solve for the updated model parameters.

       CALL SOLVE_OVER (NPAR,NSV,RSVT,ZERO,VMAT,UTRES,SV,DELPAR,WSP,PDRE)

       CALL UPDATE_MODEL_PARAMETERS

       !CALL WRITE_INVMDL (NW,FINAL,ITS,NX,NZ,NAIR,NPAR,GXPAR,IMPORT)

       ! If the predicted residual decrease < 1 percent of RMS error,
       ! terminate iterations. Inversion won't improve.

       DELT = SQRT(PDRE)
   	   Call Date_and_Time(Values = tvals)
!       IF (DELT < FNM) THEN
!         WRITE(NW,5) ; WRITE(*,5)
!         WRITE(NW,6) tvals(5:7), tvals(3:1:-1), ITS,SSQR(1),SSQR(2),RSVT
!         WRITE(*,6)  tvals(5:7), tvals(3:1:-1), ITS,SSQR(1),SSQR(2),RSVT
!         IF (CNVRG == 1 .AND. RMSERR(ITS) > 15.)    WRITE(NW,7)
!         IF (CNVRG == 2 .AND. RMSERR(ITS) > PCTCNV) WRITE(NW,8) PCTCNV
!         ! EXIT INV_LOOP
!       END IF

       ! Get the error for model with corrected parameters. Test for improvement
       ! (decrease) in residual. If it fails, reduce step and try again. Give up
       ! and return after MAXITS_INT "internal" iterations.

       JCBN = .FALSE. ; RESID = .TRUE. ; WRITE_FWD = .FALSE. ; SSQNEW = 0.
       RES = 0. ; JAC = 0. ; SSQR = 0.

       CALL RESJAC (ND,NS,NLG,NDATA,XDATA,XMODL,XWTS,NPAR,GXPAR,SSQNEW,SSQR, &
                    JCBN,JAC,RES,TDFD,STEP,IDER,NSX,SWX,SWY,NTYRP,TRP,NPULS, &
                    PULSE,NTYPLS,NCHNL,TOPN,TCLS,GSTRP,ASTRP,NFRQ,FREQ,TXCLN, &
                    TXCLNQ,CMP,NRX,NRXST,NRXSTQ,NSTAT,NSTATQ,SXQ,SZQ,RX,RXQ,RYQ, &
                    RZQ,NX,NZ,NAIR,NXZ,NPN,NPM,GEOELEC_PAR,LNODS,IREL,COORD, &
                    NCOMP,KNRM,NORM,RESID,BFFAC,PPFAC,KPPM,PRM_FD,PRM_TD,DNORM, &
                    WRITE_FWD,INVERT_RX_DATA,XMODL1,XDATA1,XWTS1,RES1,DNORM1, &
                    NDATA1,MODFIX)

       RMSE = 100. * SQRT(SSQNEW/WGTSUM)
       SSQR(1:2) = 100. * SQRT(SSQR(1:2)/WGTSUM)
       RES1 = 100. * RES1
       WRITE(*,16) RMSE

       GTEST = SUMSQ - SSQNEW
       IF (GTEST > GCRIT*PDRE) THEN   !  Error reduced using smaller step.
         IF (ICNT == 0) THEN
           RSVT = RSVT / EXPND        !  Decrease eigenvalue threshold damping.
           RSVT = MAX(BND,RSVT)
         END IF
         EXIT ICNT_LOOP1     ! Start next iteration.
       END IF

       RSVT = RSVT * EXPND            !  No error decrease. Raise threshold.

       IF (ICNT == MAXITS_INT)  THEN
         WRITE(NW,9) ICNT             !  No improvement possible. Maybe another starting guess?
         EXIT INV_LOOP
       END IF

     END DO ICNT_LOOP1

   ELSE

     !-----------------------------------------------!
     ! Solve for under-determined case, NDATA < NPAR !
     !-----------------------------------------------!

     ! If NDATA < NPAR, then the Jacobian matrix has more columns than it does have
     ! rows. Doing the SVD on that size matrix leads to expanded U and V matrices
     ! as the null space of the zero singular values are included. We can eliminate
     ! those null spaces by computing the SVD of the transposed Jacobian matrix.
     ! That is, the transpose of the Jacobian matrix has more rows than columns and
     ! the U and V matrices are swapped.

     ALLOCATE(JACT(NPAR,NDATA))
     JACT = TRANSPOSE(JAC)
     WRITE(*,41)
     CALL ESVD (JACT,NPAR,NDATA,IP,WITHU,WITHV,SV,UMAT,VMAT,WSP,ETA,TOL,NW)
     IF ( ABS(SV(1)) < 1.E-30 ) THEN
       INV_FAIL = .TRUE.
       WRITE(NW,99)
       WRITE(*,99)
       RETURN
     END IF
     DEALLOCATE(JACT)
     WRITE(*,42)

     ALLOCATE(VT(NDATA,NDATA))
     VT = TRANSPOSE(VMAT)
     UTRES = MATMUL(VT,RES)
     DEALLOCATE(VT)

     ! Solve for the update to the model parameters: DELPAR = VMAT * SV * UTRES
     ! and test for convergence on the predicted decrease. Loop over internal
     ! iterations.

     ICNT_LOOP2: DO ICNT = 1, MAXITS_INT

       IF (FINAL) EXIT ICNT_LOOP2

	   Call Date_and_Time(Values = tvals)
       WRITE(*,44) tvals(5:7), tvals(3:1:-1), ICNT, MAXITS_INT

       ! Solve for the updated model parameters.

       CALL SOLVE_UNDER (NDATA,NPAR,NSV,RSVT,ZERO,UMAT,UTRES,SV,DELPAR,WSP,PDRE)

       CALL UPDATE_MODEL_PARAMETERS

       !CALL WRITE_INVMDL (NW,FINAL,ITS,NX,NZ,NAIR,NPAR,GXPAR,IMPORT)

       ! If the predicted residual decrease < 1 percent of RMS error,
       ! terminate iterations. Inversion won't improve.

       DELT = SQRT(PDRE)
       Call Date_and_Time(Values = tvals)
!       IF (DELT < FNM) THEN
!         WRITE(NW,5) ; WRITE(*,5)
!         WRITE(NW,6) tvals(5:7), tvals(3:1:-1), ITS,SSQR(1),SSQR(2),RSVT
!         WRITE(*,6)  tvals(5:7), tvals(3:1:-1), ITS,SSQR(1),SSQR(2),RSVT
!         IF (CNVRG == 1 .AND. RMSERR(ITS) > 15.)    WRITE(NW,7)
!         IF (CNVRG == 2 .AND. RMSERR(ITS) > PCTCNV) WRITE(NW,8) PCTCNV
!         ! EXIT INV_LOOP
!       END IF

       ! Get the error for model with corrected parameters. Test for improvement
       ! (decrease) in residual. If it fails, reduce step and try again. Give up
       ! and return after MAXITS_INT "internal" iterations.

       JCBN = .FALSE. ; RESID = .TRUE. ; WRITE_FWD = .FALSE. ; SSQNEW = 0.
       RES = 0. ; JAC = 0. ; SSQR = 0.

       CALL RESJAC (ND,NS,NLG,NDATA,XDATA,XMODL,XWTS,NPAR,GXPAR,SSQNEW,SSQR, &
                    JCBN,JAC,RES,TDFD,STEP,IDER,NSX,SWX,SWY,NTYRP,TRP,NPULS, &
                    PULSE,NTYPLS,NCHNL,TOPN,TCLS,GSTRP,ASTRP,NFRQ,FREQ,TXCLN, &
                    TXCLNQ,CMP,NRX,NRXST,NRXSTQ,NSTAT,NSTATQ,SXQ,SZQ,RX,RXQ,RYQ, &
                    RZQ,NX,NZ,NAIR,NXZ,NPN,NPM,GEOELEC_PAR,LNODS,IREL,COORD, &
                    NCOMP,KNRM,NORM,RESID,BFFAC,PPFAC,KPPM,PRM_FD,PRM_TD,DNORM, &
                    WRITE_FWD,INVERT_RX_DATA,XMODL1,XDATA1,XWTS1,RES1,DNORM1, &
                    NDATA1,MODFIX)

       RMSE = 100. * SQRT(SSQNEW/WGTSUM)
       SSQR(1:2) = 100. * SQRT(SSQR(1:2)/WGTSUM)
       RES1 = 100. * RES1
       WRITE(*,16) RMSE

       GTEST = SUMSQ - SSQNEW
       IF (GTEST > GCRIT*PDRE) THEN   !  Error reduced using smaller step.
         IF (ICNT == 0) THEN
           RSVT = RSVT / EXPND        !  Decrease eigenvalue threshold damping.
           RSVT = MAX(BND,RSVT)
         END IF
         EXIT ICNT_LOOP2     ! Start next iteration.
       END IF

       RSVT = RSVT * EXPND            !  No error decrease. Raise threshold.

       IF (ICNT == MAXITS_INT)  THEN
         WRITE(NW,9) ICNT             !  No improvement possible. Maybe another starting guess?
         EXIT INV_LOOP
       END IF

     END DO ICNT_LOOP2

   END IF ! End UNDER/OVER determined IF construct.

   XPAR(1:NPAR) = GXPAR(1:NPAR)

   !
   ! Write inversion result to ArjunAir.res.
   OPEN(NM,FILE='ArjunAir.res',STATUS='REPLACE')
   DO JP = 1, NPAR
     WRITE(NM,50) JP,XPAR(JP)
   END DO
   CLOSE(NM)

   ! The error has been reduced so accept the step, write out inversion summary
   ! and test convergence.

   DELT = SQRT(ABS(GTEST))
   SUMSQ = SSQNEW
   RMSERR(ITS) = 100. * SQRT(SUMSQ/WGTSUM)
   FNM = 0.01 * SQRT(SUMSQ)
   FNM = MAX(FNM,ETA)

   ! Write out the current model and continue iterating up until IT = MAXIT.
!   IF (DELT < FNM) THEN
!     WRITE(NW,5) ; WRITE(*,5)
!     WRITE(NW,6) ITS,tvals(5:7), tvals(3:1:-1),SSQR(1),SSQR(2),RSVT
!     WRITE(*, 6) ITS,tvals(5:7), tvals(3:1:-1),SSQR(1),SSQR(2),RSVT
!     IF (CNVRG == 1 .AND. RMSERR(ITS) > 15.)    WRITE(NW,7)
!     IF (CNVRG == 2 .AND. RMSERR(ITS) > PCTCNV) WRITE(NW,8) PCTCNV
!     EXIT INV_LOOP
!   END IF
   IF (CNVRG == 2 .AND. RMSERR(ITS) < PCTCNV) THEN
     WRITE(NW,10) PCTCNV
     WRITE(NW,6) ITS,SSQR(1),SSQR(2),RSVT ; WRITE(*,6) ITS,SSQR(1),SSQR(2),RSVT
     EXIT INV_LOOP
   END IF
   IF (ITS > 3) DRMS = RMSERR(ITS-2) - RMSERR(ITS)
   IF (ITS == MAXITS) THEN
     WRITE(NW,11)
     WRITE(NW,6) ITS,SSQR(1),SSQR(2),RSVT ; WRITE(*,6) ITS,SSQR(1),SSQR(2),RSVT
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

!	year, month, day, time difference in minutes, hours, minutes, seconds and milliseconds
   Call Date_and_Time(Values = tvals)
   IF (ITS == 1) THEN
     WRITE(*,61) ITS,tvals(5:7), tvals(3:1:-1),SSQR(1),SSQR(2),RSVT
     IF (OUTPRT > 0) WRITE(NW,61) ITS,tvals(5:7), tvals(3:1:-1),SSQR(1),SSQR(2),RSVT
   ELSE
     WRITE(*,6) ITS,tvals(5:7), tvals(3:1:-1),SSQR(1),SSQR(2),RSVT
     IF (OUTPRT > 0) WRITE(NW,6) ITS,tvals(5:7), tvals(3:1:-1),SSQR(1),SSQR(2),RSVT
   END IF

   IF (OUTPRT > 1) CALL WRITE_INVMDL (NW,FINAL,ITS,NX,NZ,NAIR,NPAR,XPAR,IMPORT)

   IF (OUTPRT == 3 .AND. ITS < MAXITS) THEN
     IF (INVERT_RX_DATA) THEN
       CALL WRITE_MISFIT (NW,ITS,NSTAT,NDATA,MCHNL,TDFD,NFRQ, &
                          FREQ,CMP,NCHNL,RES,XMODL,XDATA)
     ELSE
       CALL WRITE_MISFIT (NW,ITS,NSTATQ,NDATA,MCHNL,TDFD,NFRQ, &
                          FREQ,CMP,NCHNL,RES,XMODL,XDATA)
     END IF
   END IF

!   IF (ITS > 10 .AND. DRMS < 1.) THEN
!     WRITE(NW,15) ITS
!     WRITE(*,15)  ITS
!     EXIT INV_LOOP
!   END IF

 END DO INV_LOOP ! End of main inversion loop. Write the final model and exit.

 !
 ! 2012.07.13 DWA: Moved this outside the main inversion loop to
 !                 guarantee that the *.res file gets written no
 !                 matter how many iterations were completed
 ! Write inversion result to ArjunAir.res.
 OPEN(NM,FILE='ArjunAir.res',STATUS='REPLACE')
 DO JP = 1, NPAR
   WRITE(NM,50) JP,XPAR(JP)
 END DO
 CLOSE(NM)

 !---------------------------------------------------!
 ! Singular value analysis for parameter importance. !
 !---------------------------------------------------!

 IF (NDATA >= NPAR) THEN
   CALL SV_ANALYSIS_OVER (NPAR,VMAT,WSP,IMPORT)
 ELSE IF (NPAR > NDATA) THEN
   CALL SV_ANALYSIS_UNDER (NDATA,NPAR,UMAT,WSP,IMPORT)
 END IF

 DEALLOCATE (UMAT,VMAT,SV,WSP,UTRES,JAC)

 !------------------------------------------------------------!
 ! Compute the noise to signal ratio (NSR) and write to file. !
 !------------------------------------------------------------!

 CALL NOISE_2_SIGR (NPAR,NDATA,XMODL,XDATA,XWTS,NSR)
 WRITE(NW,12)
 WRITE(NW,13) SSQR(1),SSQR(2),NSR

 IF (DNORM(0) > 1.1) THEN
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
 CALL WRITE_INVMDL (NW,FINAL,ITS,NX,NZ,NAIR,NPAR,XPAR,IMPORT)

 WRITE(np,35)
 WRITE(np,33) XPAR(1:NPAR)
 WRITE(np,34) IMPORT(1:NPAR)
 IF (MV1PRT > 0) CALL WRITE_MDATA(FITS)

 !-------------------------------------------------!
 ! Write the final misfit at Rx positions to file. !
 !-------------------------------------------------!

 IF (INVERT_RX_DATA) THEN
   CALL WRITE_MISFIT (NW,FITS,NSTAT,NDATA,MCHNL,TDFD,NFRQ,FREQ,CMP, &
                      NCHNL,RES,XMODL,XDATA)
 ELSE
   CALL WRITE_MISFIT (NW,FITS,NSTAT,NDATA1,MCHNL,TDFD,NFRQ,FREQ,CMP, &
                      NCHNL,RES1,XMODL1,XDATA1)
 END IF

 DEALLOCATE (RMSERR,XDATA,XDATA1,XMODL,XMODL1,RES,RES1,XWTS,XWTS1,DNORM,DNORM1)

 !-------------------!
 ! Print statements. !
 !-------------------!

 1  FORMAT(/T3,'Begin frequency-domain inversion.')
 2  FORMAT(/T3,'Begin time-domain inversion on ',A)
 3  FORMAT(/T3,'Maximum iterations =',I3)
 4  FORMAT(/T3,'Initial Point-norm RMS error  =',F8.2,' percent.' &
           /T3,'Initial Survey-norm RMS error =',F8.2,' percent.' &
           /T3,'Initial Relative SV threshold =',F8.3)
 5  FORMAT(/, 2x,'Convergence on predicted decrease ...')
 61 FORMAT(/I4,' Iteration completed at ' , &
                i2.2, ':', i2.2, ':', i2.2, ' on ', i2.2, '/', i2.2, '/', i4.4, &
           /T6,'Point-norm RMS error  =',F8.2,' percent.' &
           /T6,'Survey-norm RMS error =',F8.2,' percent.' &
           /T6,'Relative SV threshold =',F8.3, &
           /t6,'------------------------------')
 6  FORMAT(/I4,' Iterations completed at ', &
                i2.2, ':', i2.2, ':', i2.2, ' on ', i2.2, '/', i2.2, '/', i4.4, &
           /T6,'Point-norm RMS error  =',F8.2,' percent.' &
           /T6,'Survey-norm RMS error =',F8.2,' percent.' &
           /T6,'Relative SV threshold =',F8.3, &
           /t6,'------------------------------')
 7  FORMAT(/T3,'An alternative starting guess might achieve better results.')
 8  FORMAT(/T3,'The inversion was unable to achieve an RMS error' &
           /T3,'within the specified threshold of',F9.2,' percent.' &
           /T3,'An alternative starting guess may achieve better results.')
 9  FORMAT(/T3,'The solution is trapped. ICNT = ',I2/ &
           /T3,'Another starting guess may yield a better result.')
 10 FORMAT(/T3,'Convergence within RMS error threshold of',F7.2,' has been achieved.')
 11 FORMAT(//T3,'Inversion finished after maximum number of iterations.')
 12 FORMAT(//T3,50('='))
 13 FORMAT(/T12,'Point-norm RMS error  =',F8.2,' percent.' &
           /T12,'Survey-norm RMS error =',F8.2,' percent.' &
           /T12,'Noise to signal ratio =',F8.3)
 14 FORMAT(/T3,'------------------------------------------------------------------' &
           /T3,'The reduction in the RMS error during the last two iterations was ' &
           /T3,F5.2,' percent.  A better result may be achieved by using the final' &
           /T3,'model from this inversion as the starting guess for another run.'   &
           /T3,'------------------------------------------------------------------')
 15 FORMAT(/T3,'---------------------------------------------------------------------' &
           /T3,'Inversion terminated after',I3,' iterations because, the reduction in' &
           /T3,'the RMS error from the last two iterations was less than 1 percent.  ' &
           /T3,'---------------------------------------------------------------------')
 16 FORMAT(2x, 'RMS error =', F9.2, ' percent.')
 25 FORMAT(//T3,'RMS error reduction based on using the Survey norm' &
            /T3,'--------------------------------------------------')
 26 FORMAT(//T3,'RMS error reduction based on symmetric point normalisation'&
            /T3,'----------------------------------------------------------')
 27 FORMAT(//T3,'Inversion executed with field data at reciever positions'&
            /T3,'--------------------------------------------------------')
 28 FORMAT(//T3,'Inversion executed with field data interpolated to'&
            /T3,'node-based receiver positions                     '&
            /T3,'--------------------------------------------------')
 29 FORMAT(//T3,'Element resistivities read from file, ArjunAir.res.')
 30 FORMAT(T1,'/'/T1,'/ ITERATION  ',I2.2/T1,'/ PERCENT_RMS_ERROR:',F9.2,3X,'RSVT:',F8.3)
 31 FORMAT(T1,'/'/T1,'/ MODEL_',I2.2)
 36 FORMAT(T1,'/ MODEL ',T15,100G13.4)
 32 FORMAT(T1,'/'/T1,'/ FINAL_ITERATION  ',I2.2/T1,'/ PERCENT_RMS_ERROR:',F8.2,3X,'NSR:',F7.3,3X,'RSVT:',F7.3)
 35 FORMAT(T1,'/')
 33 FORMAT(T1,'/ FINAL_MODEL',T15,100G13.4)
 34 FORMAT(T1,'/ IMPORTANCE ',T15,100G13.4)
 41 FORMAT(/T3,'SVD of the Jacobian matrix commenced ...')
 42 FORMAT( T3,'SVD of the Jacobian matrix completed ...')
 44 FORMAT(/, 2x, i2.2, ':', i2.2, ':', i2.2, ' on ', i2.2, '/', i2.2, '/', i4.4, &
               ': Testing model updates for error reduction, internal iteration ',I2.2, ' of ', i2.2)
 45 FORMAT(/T3,'Computing forward models for primal and adjoint fields:')
 50 FORMAT(I4,G12.4)
 99 FORMAT(//T3,'Singular value decomposition failure. INVERSION HALTED.')

 CONTAINS

   !-----------------------------!
   ! Internal subroutine of NLSQ !
   !-----------------------------!

   SUBROUTINE PREPARE_INV_DATA

     INTEGER JD,JS,JT
     REAL BIG,S1
     INTEGER, ALLOCATABLE, DIMENSION(:,:) :: RWTSQ
     REAL, ALLOCATABLE, DIMENSION(:,:) :: RDATAQ

     DO JS = 1, NSTAT
       DO JT = 1, MCHNL
         JD = JT + (JS-1) * MCHNL
         XDATA1(JD) = RDATA(JT,JS)  ! Data for writing to ArjunAir.inv.
         XWTS1(JD) = RWTS(JT,JS)
       END DO
     END DO

     BIG = MAXVAL(ABS(XDATA))       ! Construct S norm normalisation vector
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
         DNORM1(JD) = S1
       END DO
     END DO

     IF (INVERT_RX_DATA) THEN         ! USE DATA AT RECIEVER POSITIONS

       XDATA(1:NDATA) = XDATA1(1:NDATA) ! Construct the data and data weight vectors.
       XWTS(1:NDATA) = XWTS1(1:NDATA)
       DNORM(1:NDATA) = DNORM1(1:NDATA)

     ELSE                             ! USE DATA AT NODE POSITIONS

       ALLOCATE (RDATAQ(MCHNL,NSTATQ),RWTSQ(MCHNL,NSTATQ))
       RDATAQ = 0. ; RWTSQ = 1

       CALL SET_INV_DATA(NSTAT,NSTATQ,MCHNL,RDATA,RWTS,RDATAQ,RWTSQ,RX,RXQ,NRX,TDFD)

       DO JS = 1, NSTATQ              ! Construct the data and data weight vectors.
         DO JT = 1, MCHNL
           JD = JT + (JS-1) * MCHNL
           XDATA(JD) = RDATAQ(JT,JS)
           XWTS(JD) = RWTSQ(JT,JS)
         END DO
       END DO

       BIG = MAXVAL(ABS(XDATA))       ! Construct S norm normalisation vector.
       DO JT = 1, MCHNL
         S1 = 0.
         DO JS = 1, NSTATQ
           S1 = S1 + ABS(RDATAQ(JT,JS))
         END DO
         S1 = S1/REAL(NSTATQ)
         S1 = MAX(S1,DATA_FLOOR(JT))
         IF (S1 < 1.0E-7 * BIG) S1 = 1.0E7 * BIG  ! Eliminate cross-over fluctuations.
         DO JS = 1, NSTATQ
           JD = JT + (JS-1) * MCHNL
           DNORM(JD) = S1
         END DO
       END DO

       DEALLOCATE (RDATAQ,RWTSQ)

     END IF

   END SUBROUTINE PREPARE_INV_DATA

   !-----------------------------!
   ! Internal subroutine of NLSQ !
   !-----------------------------!

   SUBROUTINE UPDATE_MODEL_PARAMETERS

     ! Solve for updated model parameters.

     DO JP = 1, NPAR
       SELECT CASE (MODFIX(JP))
       CASE(0)                                         ! Free to vary.
         MP = LOG(XPAR(JP)) + DELPAR(JP)
         GXPAR(JP) = EXP(MP)
       CASE(1)                                         ! Fixed at a priori value.
         GXPAR(JP) = XPAR(JP)
       CASE(2)                                         ! Constrained by elasticity.
         MP = LOG(XPAR(JP)) + MODWGT(JP) * DELPAR(JP)
         GXPAR(JP) = EXP(MP)
       CASE(3)                                         ! Constrained by elasticity and bounds.
         B1 = LOG(MLBND(JP))
         B2 = LOG(MUBND(JP))
         MP = LOG(XPAR(JP)) + MODWGT(JP) * DELPAR(JP)
         MP = MIN(MP,B2)
         MP = MAX(MP,B1)
         GXPAR(JP) = EXP(MP)
       END SELECT
     END DO

     DO I1 = 3, NX - 3
       DO J1 = NAIR+1, NZ - 3
         JC1 = (I1-1)*(NZ-1) + J1
         JC2 = (I1-3)*(NZ-NAIR-3) + (J1-NAIR)
         GEOELEC_PAR(JC1,1) = GXPAR(JC2)
       END DO
     END DO

   END SUBROUTINE UPDATE_MODEL_PARAMETERS

   !-----------------------------!
   ! Internal subroutine of NLSQ !
   !-----------------------------!

   SUBROUTINE WRITE_MDATA(KTS)

     REAL, PARAMETER :: RAD2DEG = 180./3.141592654
     REAL, DIMENSION(NRXST) :: TXDEG
     INTEGER JS,KTS
     CHARACTER (LEN=80) QL0,QL1

     TXDEG = RAD2DEG * TXCLN

     DO JS = 1, NSTAT
       RZ0(JS) = RZ(JS,1)
       K0 = (JS-1) * MCHNL
       IF (JS == 1) THEN
         IF (KTS < 0) THEN
           WRITE(QL0,2) LINE
         ELSE
           WRITE(QL0,1) LINE,KTS
         END IF
         READ(QL0,'(A)') QL1
         WRITE(np,3) TRIM (ADJUSTL (QL1))
       END IF
       IF (TDFD < 2) THEN
         WRITE(np,4) JS,SXD(JS),SYD(JS),SZ(JS),TXDEG(JS),RXD(JS,1),RYD(JS,1),RZ0(JS), &
                      XMODL1(K0+1:K0+MCHNL),RES1(K0+1:K0+MCHNL)
       ELSE
         WRITE(np,5) JS,SXD(JS),SYD(JS),SZ(JS),RXD(JS,1),RYD(JS,1),RZ0(JS), &
                      XMODL1(K0+1:K0+MCHNL),RES1(K0+1:K0+MCHNL)
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

 SUBROUTINE SET_INV_DATA (NSTAT,NSTATQ,MCHNL,RDATA,RWTS,RDATAQ,RWTSQ,RX, &
                          RXQ,NRX,TDFD)

!----------------------------------------------------------------------------
!
!*** Called by: NLSQ
!
!  Interpolate inversion data from RDATA (at RX positions) to RDATAQ (at node
!  positions.
!
!----------------------------------------------------------------------------

  IMPLICIT NONE
  INTEGER NSTAT,NSTATQ,NRX,MCHNL,NFRQ,JS,JT,JR,TDFD,RWTS(MCHNL,NSTAT), &
          RWTSQ(MCHNL,NSTATQ)
  REAL RDATA(MCHNL,NSTAT),RDATAQ(MCHNL,NSTATQ),W,X,XC(NSTAT),QR(4,NSTAT), &
       CUBVAL,RX(NSTAT,NRX),RXQ(NSTATQ,NRX)

  ! Interpolate RWTS (from RX positions) to RWTSQ (at node positions).

  DO JT = 1, MCHNL
    IF (TDFD < 2) THEN
      JR = 1
    ELSE IF (TDFD == 2) THEN
      NFRQ = MCHNL/2
      JR = JT
      IF (JT > NFRQ) JR = JT - NFRQ
    END IF
    XC(1:NSTAT) = RX(1:NSTAT,JR)
    DO JS = 1, NSTAT
      QR(1,JS) = REAL(RWTS(JT,JS))
    END DO
    CALL CUBSPL(XC,QR,NSTAT,0,0) ! Spline data weights at RX positions.
    DO JS = 1, NSTATQ
      X = RXQ(JS,JR)
      W = CUBVAL(XC,QR,NSTAT,X)  ! Interpolate data weights at node positions.
      IF (X < 0.5) THEN
        RWTSQ(JT,JS) = FLOOR(W)
      ELSE
        RWTSQ(JT,JS) = CEILING(W)
      END IF
    END DO
  END DO

  ! Interpolate RDATA (from RX positions) to RDATAQ (at node positions).

  DO JT = 1, MCHNL
    IF (TDFD < 2) THEN
      JR = 1
    ELSE IF (TDFD == 2) THEN
      NFRQ = MCHNL/2
      JR = JT
      IF (JT > NFRQ) JR = JT - NFRQ
    END IF
    XC(1:NSTAT) = RX(1:NSTAT,JR)
    DO JS = 1, NSTAT
      QR(1,JS) = RDATA(JT,JS)
    END DO
    CALL CUBSPL(XC,QR,NSTAT,0,0) ! Spline data at RX positions.
    DO JS = 1, NSTATQ
      X = RXQ(JS,JR)
      RDATAQ(JT,JS) = CUBVAL(XC,QR,NSTAT,X) ! Interpolate data at node positions.
    END DO
  END DO

 END SUBROUTINE SET_INV_DATA

!============================================================================

 SUBROUTINE RESJAC (ND,NS,NLG,NDATA,XDATA,XMODL,XWTS,NPAR,XPAR,SUMSQ,SSQR,JCBN, &
                    JAC,RES,TDFD,STEP,IDER,NSX,SWX,SWY,NTYRP,TRP,NPULS,PULSE, &
                    NTYPLS,NCHNL,TOPN,TCLS,GSTRP,ASTRP,NFRQ,FREQ,TXCLN,TXCLNQ, &
                    CMP,NRX,NRXST,NRXSTQ,NSTAT,NSTATQ,SXQ,SZQ,RX,RXQ,RYQ,RZQ, &
                    NX,NZ,NAIR,NXZ,NPN,NPM,GEOELEC_PAR,LNODS,IREL,COORD,NCOMP, &
                    KNRM,NORM,RESID,BFFAC,PPFAC,KPPM,PRM_FD,PRM_TD,DNORM, &
                    WRITE_FWD,INVERT_RX_DATA,XMODL1,XDATA1,XWTS1,RES1,DNORM1, &
                    NDATA1,MODFIX)

!----------------------------------------------------------------------------
!
!*** Called by: NLSQ
!*** Calls: ARJUNA_2DI,FDREAD,TDEM,SENS2D_FD,SENS_FD2TD
!
!  Construct the residual error vector (RES) and Jacobian matrix (JAC).
!
!----------------------------------------------------------------------------
!
!  DESCRIPTION:
!
!  Calls for forward model computation using Ajurna 2.5D finite elements.
!  It also computes the residual error vector and Jacobian matrix as
!  required.
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
!  The Jacobian matrix is then logarithmically transformed. This means that
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
 INTEGER ND,NS,NDATA,NPAR,TDFD,STEP,IDER,NSX,NPULS,NTYPLS,NTYRP,NCHNL,GSTRP, &
         ASTRP,NFRQ,NSTAT,NSTATQ,NRXST,NRXSTQ,NRX,NX,NZ,NXZ,NAIR,NPM,NPN, &
         LNODS(NXZ,8),IREL(NRX,NSTATQ),JD,JP,KPPM,NLG,NCOMP,CMP,KNRM,NDATA1, &
         MODFIX(NPAR)
 LOGICAL JCBN,RESID,WRITE_FWD,INVERT_RX_DATA
 REAL SUMSQ,PULSE,SWX(NSX),SWY(NSX,3),TRP(NTYRP),FREQ(NFRQ),COORD(NPN,2),BFFAC, &
      PPFAC,VD,VM,RES_ERR,PRM_FD(NFRQ),PRM_TD(3),NORM(KNRM),GEOELEC_PAR(NXZ,6), &
      DNORM(0:NDATA),DENOM,SSQR(2),V1,V2,MODWGT
 REAL, DIMENSION(NDATA) :: XDATA,XMODL,RES
 REAL, DIMENSION(NDATA1) :: XMODL1,XDATA1,RES1,DNORM1
 INTEGER, DIMENSION(NDATA) :: XWTS
 INTEGER, DIMENSION(NDATA1) :: XWTS1
 REAL, DIMENSION(NPAR)  :: XPAR
 REAL, DIMENSION(NCHNL) :: TOPN,TCLS
 REAL, DIMENSION(NSTATQ) :: SXQ,SZQ
 REAL, DIMENSION(NRXST) :: TXCLN
 REAL, DIMENSION(NRXSTQ) :: TXCLNQ
 REAL, DIMENSION(NDATA,NPAR) :: JAC
 REAL, DIMENSION(NSTAT,NRX) :: RX
 REAL, DIMENSION(NSTATQ,NRX) :: RXQ,RYQ,RZQ
 COMPLEX, ALLOCATABLE :: BFDQ(:,:,:),SFDQ(:,:,:,:)

 ! COMPUTE FORWARD MODEL FOR PRIMAL AND/OR ADJOINT PROBLEMS
 ! --------------------------------------------------------

 CALL ARJUNA_2D (NLG,ND,NS,NX,NZ,NAIR,NXZ,NPN,NPM,LNODS,COORD, &
                 GEOELEC_PAR,NCOMP,NSTATQ,SXQ,SZQ,RXQ,RYQ,RZQ,IREL, &
                 NFRQ,FREQ,RESID,JCBN,TXCLNQ,NRXSTQ,NPAR,NRX,CMP,TDFD, &
                 WRITE_FWD)

! RESIDUAL ERROR VECTOR CONSTRUCTION
! ----------------------------------

 IF (RESID) THEN

   ALLOCATE (BFDQ(NFRQ,NSTATQ,3)) ; BFDQ = (0.,0.)

   CALL FDREAD (ND,NFRQ,NSTATQ,BFDQ)

   IF (TDFD < 2) THEN  ! Compute XMODL from BTDQ.

     CALL TD_FIELD (STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,  &
                    NCHNL,TOPN,TCLS,FREQ,NFRQ,NSTAT,NSTATQ,BFDQ,GSTRP, &
                    ASTRP,XMODL,NCOMP,PRM_TD,BFFAC,PPFAC,CMP,KPPM,NDATA, &
                    KNRM,NORM,RXQ,RX,NRX,INVERT_RX_DATA,XMODL1,NDATA1)

   ELSE IF (TDFD == 2) THEN  ! Compute XMODL from BFDQ.

     CALL FD_FIELD (XMODL,NDATA,NSTAT,NSTATQ,NRX,NFRQ,BFDQ,PRM_FD,BFFAC, &
                    PPFAC,TXCLN,NRXST,KNRM,NORM,RXQ,RX,INVERT_RX_DATA, &
                    XMODL1,NDATA1)

   END IF

   DEALLOCATE (BFDQ)

   ! Compute the residual error vector and the sum squared scaled symmetric
   ! error; convert to the log of residual errors.

   ! DNORM(0) = 1 : Point norm (P) used for fitting error and sensitivity matrix.
   ! DNORM(0) = 2 : Survey norm (S) used for fitting error and sensitivity matrix.

   SUMSQ = 0. ; SSQR = 0.
   DO JD = 1, NDATA1
     VD = XDATA1(JD)                        ! Observed data.
     VM = XMODL1(JD)                        ! Predicted data.
     RES_ERR = REAL(XWTS1(JD)) * (VD - VM)  ! Weighed residual error.
     DENOM = SQRT((VM**2 + VD**2)/2.)
     V1 = RES_ERR / DENOM                   ! Point symmetric RMS error.
     SSQR(1) = SSQR(1) + V1**2
     V2 = RES_ERR / DNORM1(JD)              ! Survey norm RMS error.
     SSQR(2) = SSQR(2) + V2**2
     IF (DNORM(0) > 1.1) THEN
       SUMSQ = SSQR(2)
       RES1(JD) = V2
     ELSE
       SUMSQ = SSQR(1)
       RES1(JD) = V1
     END IF
   END DO

   IF (INVERT_RX_DATA) THEN       ! Use residual error at Rx positions.
     RES(1:NDATA) = RES1(1:NDATA)
   ELSE                           ! Use residual error at node positions.
     SUMSQ = 0. ; SSQR = 0.
     DO JD = 1, NDATA
       VD = XDATA(JD)
       VM = XMODL(JD)
       RES_ERR = REAL(XWTS(JD)) * (VD - VM)
       DENOM = SQRT((VM**2 + VD**2)/2.)
       V1 = RES_ERR / DENOM                   ! Point symmetric RMS error.
       SSQR(1) = SSQR(1) + V1**2
       V2 = RES_ERR / DNORM1(JD)              ! Survey norm RMS error.
       SSQR(2) = SSQR(2) + V2**2
       IF (DNORM(0) > 1.1) THEN
         SUMSQ = SSQR(2)
         RES(JD) = V2
       ELSE
         SUMSQ = SSQR(1)
         RES(JD) = V1
       END IF
     END DO
   END IF

 END IF

! JACOBIAN MATRIX CONSTRUCTION
! ----------------------------

 IF (JCBN) THEN

   ALLOCATE (SFDQ(NFRQ,NSTATQ,NCOMP,NPAR)) ; SFDQ = (0.,0.)

   CALL SFDREAD (NS,NFRQ,NSTATQ,NCOMP,NPAR,SFDQ)

   IF (TDFD < 2) THEN ! Construct the time-domain Jacobian matrix:

     CALL TD_SENS (STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP, &
                   NCHNL,TOPN,TCLS,FREQ,NFRQ,NSTAT,NSTATQ,SFDQ,JAC,NPAR, &
                   NDATA,NCOMP,PRM_TD,BFFAC,PPFAC,CMP,KPPM,KNRM,NORM,NRX, &
                   GSTRP,ASTRP,RXQ,RX,INVERT_RX_DATA)

   ELSE IF (TDFD == 2) THEN ! Construct the frequency-domain Jacobian matrix:

     CALL FD_SENS (JAC,NDATA,NPAR,NSTAT,NSTATQ,NFRQ,NRX,SFDQ,PRM_FD,BFFAC, &
                   PPFAC,NCOMP,KNRM,NORM,RXQ,RX,INVERT_RX_DATA)

   END IF

   DEALLOCATE (SFDQ)

   ! Convert to sensitivity wrt resistivity instead of sensitivity wrt
   ! conductivity. Convert to log of the sensitivities wrt resisitivity and
   ! data. Multiply by data and model weights.

   DO JD = 1, NDATA
     IF (DNORM(0) < 1.1) THEN   ! Normalise by the survey norm.
       DENOM = DNORM(JD)
     ELSE                       ! Normalise by the point symmetric data.
       VD = XDATA(JD)
       VM = XMODL(JD)
       DENOM = SQRT((VM**2 + VD**2)/2.)
     END IF
     DO JP = 1, NPAR
       MODWGT = 1.
       IF (MODFIX(JP) == 1) MODWGT = 0.
       JAC(JD,JP) = -JAC(JD,JP) * REAL(XWTS(JD)) * MODWGT / (XPAR(JP) * DENOM)
     END DO
   END DO

 END IF

 END SUBROUTINE RESJAC

!===========================================================================

 SUBROUTINE SFDREAD (NS,NFRQ,NSTATQ,NCOMP,NPAR,SFDQ)

!---------------------------------------------------------------------------
!
! Reads frequency domain sensitivity data (real and imaginary components)
! from logical unit NS into array SFDQ.
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER NS,NFRQ,NSTATQ,NCOMP,NPAR,JF,JS,JC,JP
 REAL A,B
 COMPLEX SFDQ(NFRQ,NSTATQ,NCOMP,NPAR)

 REWIND(NS)

 DO JF = 1, NFRQ
   DO JS = 1, NSTATQ
     DO JC = 1, NCOMP
       DO JP = 1, NPAR
         READ(NS,*) A,B
         SFDQ(JF,JS,JC,JP) = CMPLX(A,B)
       END DO
     END DO
   END DO
 END DO

 END SUBROUTINE SFDREAD

!===========================================================================

 SUBROUTINE TD_FIELD (STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,  &
                      NCHNL,TOPN,TCLS,FREQ,NFRQ,NSTAT,NSTATQ,BFDQ,GSTRP, &
                      ASTRP,XMODL,NCOMP,PRM_TD,BFFAC,PPFAC,CMP,KPPM,NDATA, &
                      KNRM,NORM,RXQ,RX,NRX,INVERT_RX_DATA,XMODL1,NDATA1)

!---------------------------------------------------------------------------
!
!*** Called by RESJAC
!
!  Compute and normalise the predicted time-domain data vector, XMODL.
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 REAL, PARAMETER :: TOL = 1.E-3
 LOGICAL INVERT_RX_DATA
 INTEGER NDATA,NCHNL,STEP,IDER,NSX,NPULS,NTYPLS,NTYRP,NFRQ,GSTRP,ASTRP, &
         KPPM,JS,JC,JT,JD,NCOMP,CMP,NSTAT,NSTATQ,KNRM,NRX,NDATA1
 REAL XMODL(NDATA),BTDQ(NCHNL,NSTATQ,3),TOPN(NCHNL),TCLS(NCHNL),FREQ(NFRQ), &
      NORM(KNRM),PRM_TD(3),BFFAC,PPFAC,XRN,TRP(NTYPLS),SWX(NSX), &
      SWY(NSX,3),PULSE,RXQ(NSTATQ,NRX),RX(NSTAT,NRX),XMODL1(NDATA1)
 REAL, ALLOCATABLE :: BTD(:,:,:)
 COMPLEX BFDQ(NFRQ,NSTATQ,3)

 ! Compute BTD, the time-domain field response via cosine transform and
 ! convolution with the system waveform.

 CALL TDEM_3D (STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL, &
               TOPN,TCLS,FREQ,NFRQ,NSTATQ,BFDQ,GSTRP,ASTRP,BTDQ)

 ! Normalise BTD.

 IF (KPPM == 0) THEN          ! Compute fields in requied units.
   BTDQ = BFFAC * BTDQ
 ELSE IF (KPPM > 0) THEN      ! Compute normalised response.
   NORM(1) = ABS (PRM_TD(1))
   NORM(2) = ABS (PRM_TD(2))
   NORM(3) = ABS (PRM_TD(3))
   NORM(4) = SQRT (NORM(1)**2 + NORM(2)**2 + NORM(3)**2)
   DO JC = 1, 3
     XRN = NORM(JC) / NORM (4)
     IF (XRN < TOL) NORM(JC) = NORM(4)
   END DO
   IF (KPPM == 4) NORM(1:3) = NORM(4)                   ! Total primary normalisation
   IF (KPPM == 1 .OR. KPPM == 3) NORM(1:3) = NORM(KPPM) ! Vertical or in-line normalisation
   DO JC = 1,3
     BTDQ(1:NCHNL,1:NSTATQ,JC) = PPFAC * BTDQ(1:NCHNL,1:NSTATQ,JC) / NORM(JC)
   END DO
 END IF

 ALLOCATE (BTD(NCHNL,NSTAT,3)) ; BTD = 0.

 CALL SET_DATA_TD (NCHNL,3,NSTATQ,NSTAT,BTDQ,RXQ,RX,BTD,NRX)

 DO JS = 1, NSTAT ! Model data for writing to ArjunAir.inv.
   DO JC = 1, NCOMP
     DO JT = 1, NCHNL
       JD = JT + (JC-1)*NCHNL + (JS-1)*NCOMP*NCHNL
       IF (CMP == 11) THEN
         XMODL1(JD) = BTD(JT,JS,1)        ! Inline component of predicted data.
       ELSE IF (CMP == 13) THEN
         XMODL1(JD) = BTD(JT,JS,3)        ! Vertical component of predicted data.
       ELSE IF (CMP == 2) THEN
         IF (NCOMP == 1) THEN
           XMODL1(JD) = BTD(JT,JS,3)      ! Vertical component of predicted data.
         ELSE IF (NCOMP == 2) THEN
           XMODL1(JD) = BTD(JT,JS,1)      ! In-line component of predicted data.
         END IF
       END IF
     END DO
   END DO
 END DO

 IF (INVERT_RX_DATA) THEN ! Model data at Rx positions.

   XMODL(1:NDATA) = XMODL1(1:NDATA)

 ELSE

   DO JS = 1, NSTATQ ! Model data at node positions.
     DO JC = 1, NCOMP
       DO JT = 1, NCHNL
         JD = JT + (JC-1)*NCHNL + (JS-1)*NCOMP*NCHNL
         IF (CMP == 11) THEN
           XMODL(JD) = BTDQ(JT,JS,1)        ! Inline component of predicted data.
         ELSE IF (CMP == 13) THEN
           XMODL(JD) = BTDQ(JT,JS,3)        ! Vertical component of predicted data.
         ELSE IF (CMP == 2) THEN
           IF (NCOMP == 1) THEN
             XMODL(JD) = BTDQ(JT,JS,3)      ! Vertical component of predicted data.
           ELSE IF (NCOMP == 2) THEN
             XMODL(JD) = BTDQ(JT,JS,1)      ! In-line component of predicted data.
           END IF
         END IF
       END DO
     END DO
   END DO

 END IF

 DEALLOCATE (BTD)

 END SUBROUTINE TD_FIELD

!===========================================================================

 SUBROUTINE TD_SENS (STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP, &
                     NCHNL,TOPN,TCLS,FREQ,NFRQ,NSTAT,NSTATQ,SFDQ,JAC,NPAR, &
                     NDATA,NCOMP,PRM_TD,BFFAC,PPFAC,CMP,KPPM,KNRM,NORM,NRX, &
                     GSTRP,ASTRP,RXQ,RX,INVERT_RX_DATA)

!----------------------------------------------------------------------------
!
!*** Called by: RESJAC
!
!  Compute the TD sensitivity and construct the Jacobian matrix.
!
!----------------------------------------------------------------------------

 IMPLICIT NONE
 REAL, PARAMETER :: TOL = 1.E-3
 LOGICAL INVERT_RX_DATA
 INTEGER NDATA,NCHNL,STEP,IDER,NSX,NPULS,NTYPLS,NTYRP,NFRQ,GSTRP,ASTRP, &
         KPPM,JS,JC,JC1,JT,JD,JP,NCOMP,CMP,NSTAT,NSTATQ,NPAR,KNRM,NRX
 REAL STDQ(NCHNL,NSTATQ,NCOMP,NPAR),TOPN(NCHNL),TCLS(NCHNL),FREQ(NFRQ), &
      PRM_TD(3),BFFAC,PPFAC,XRN,JAC(NDATA,NPAR),TRP(NTYRP),PULSE, &
      SWX(NSX),SWY(NSX,3),NORM(KNRM),RXQ(NSTATQ,NRX),RX(NSTAT,NRX)
 REAL, ALLOCATABLE, DIMENSION (:,:,:) :: STD1,STD
 COMPLEX SFDQ(NFRQ,NSTATQ,NCOMP,NPAR)

 ! Transform complex frequency-domain sensitivities SFD to real time-domain
 ! sensitivites STD:

 CALL SENS_FD2TD (STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP, &
                  NCHNL,TOPN,TCLS,FREQ,NFRQ,NSTATQ,NCOMP,NPAR,SFDQ,STDQ, &
                  ASTRP,GSTRP)

 ! Normalise STD:

 IF (KPPM == 0) THEN            ! Compute fields in requied units.
   STDQ = BFFAC * STDQ
 ELSE IF (KPPM > 0) THEN        ! Compute normalised response.
   NORM(1) = ABS (PRM_TD(1))
   NORM(2) = ABS (PRM_TD(2))
   NORM(3) = ABS (PRM_TD(3))
   NORM(4) = SQRT (NORM(1)**2 + NORM(2)**2 + NORM(3)**2)
   DO JC = 1, 3
     XRN = NORM(JC) / NORM (4)
     IF (XRN < TOL) NORM(JC) = NORM(4)
   END DO
   IF (KPPM == 4) NORM(1:3) = NORM(4)                   ! Total primary normalisation
   IF (KPPM == 1 .OR. KPPM == 3) NORM(1:3) = NORM(KPPM) ! vertical or in-line normalisation
   DO JC = 1, NCOMP
     IF (CMP == 11) THEN
       JC1 = 1                  ! In-line normalisation.
     ELSE IF (CMP == 13) THEN
       JC1 = 3                  ! Vertical normalisation.
     ELSE IF (CMP == 2) THEN
       IF (JC == 1) JC1 = 1     ! In-line normalisation.
       IF (JC == 2) JC1 = 3     ! Vertical normalisation.
     END IF
     STDQ(1:NCHNL,1:NSTATQ,JC,1:NPAR) = PPFAC * STDQ(1:NCHNL,1:NSTATQ,JC,1:NPAR) / NORM(JC1)
   END DO
 END IF

 IF (INVERT_RX_DATA) THEN ! Interpolate sensitivity from nodes to RX positions.

   ALLOCATE (STD1(NCHNL,NSTATQ,NCOMP),STD(NCHNL,NSTAT,NCOMP))
   STD1 = 0. ; STD = 0.

   DO JP = 1, NPAR
     STD1(1:NCHNL,1:NSTATQ,1:NCOMP) = STDQ(1:NCHNL,1:NSTATQ,1:NCOMP,JP)
     CALL SET_SENS_TD (NCHNL,NCOMP,NSTATQ,NSTAT,STD1,STD,RXQ,RX,NRX)
     DO JS = 1, NSTAT
       DO JC = 1, NCOMP
         DO JT = 1, NCHNL
           JD = JT + (JC-1)*NCHNL + (JS-1)*NCOMP*NCHNL
           JAC(JD,JP) = STD(JT,JS,JC)
         END DO
       END DO
     END DO
   END DO

   DEALLOCATE (STD1,STD)

 ELSE ! Compute sensitivity at nodes.

   DO JP = 1, NPAR
     DO JS = 1, NSTATQ
       DO JC = 1, NCOMP
         DO JT = 1, NCHNL
           JD = JT + (JC-1)*NCHNL + (JS-1)*NCOMP*NCHNL
           JAC(JD,JP) = STDQ(JT,JS,JC,JP)
         END DO
       END DO
     END DO
   END DO

 END IF

 END SUBROUTINE TD_SENS

!============================================================================

 SUBROUTINE SET_SENS_TD (NCHNL,NCOMP,NSTATQ,NSTAT,STD1,STD,RXQ,RX,NRX)

!---------------------------------------------------------------------------
!
!*** Called by: TD_SENS
!
!  Interpolates the sensitivity STD1 based on NSTATQ receivers at (RXQ,RZQ)
!  into STD based on NSTAT receivers at (RX,RZ).
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER NCHNL,NSTAT,NSTATQ,NCOMP,NRX,JC,JT,JS,JRX
 REAL RX(NSTAT,NRX),RXQ(NSTATQ,NRX),QR(4,NSTATQ),CUBVAL,X,XC(NSTATQ), &
      STD(NCHNL,NSTAT,NCOMP),STD1(NCHNL,NSTATQ,NCOMP)

 DO JC = 1, NCOMP
   DO JT = 1, NCHNL
     JRX = 1
     XC(1:NSTATQ) = RXQ(1:NSTATQ,JRX)
     DO JS = 1, NSTATQ
       QR(1,JS) = STD1(JT,JS,JC)
     END DO
     CALL CUBSPL(XC,QR,NSTATQ,0,0)
     DO JS = 1, NSTAT
       X = RX(JS,JRX)
       STD(JT,JS,JC) = CUBVAL(XC,QR,NSTATQ,X)
     END DO
   END DO
 END DO

 END SUBROUTINE SET_SENS_TD

!============================================================================

 SUBROUTINE SENS_FD2TD (STEP,IDER,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP, &
                        NCHNL,TOPN,TCLS,FREQ,NFRQ,NSTATQ,NCOMP,NPAR,SFD,STD, &
                        ASTRP,GSTRP)

!----------------------------------------------------------------------------
!
!***  Called by TD_SENS
!***  Calls CUBSPL, COSTRN, FOLD_CONVOLVE
!
!  Transform the unit dipole frequency-domain sensitivity values to system
!  time-domain sensitivities. This is identical to TDEM_3D.
!
!----------------------------------------------------------------------------

 IMPLICIT NONE
 REAL, PARAMETER :: TWOPI = 6.283185
 INTEGER GSTRP,ASTRP,IDER,STEP,NSX,NPULS,NTYPLS,NTYRP,NCHNL,NFRQ,NSTATQ, &
         JS,JF,JC,JT,NCOMP,JP,NPAR
 REAL, DIMENSION(:,:), ALLOCATABLE :: YSCAT,YFRQ,SFD_QUAD
 REAL PULSE,FREQ(NFRQ),T0_MIN,WF(NFRQ),SWX(NSX),SWY(NSX,3),COSTRN,T, &
      YCUM(NCHNL),TOPN(NCHNL),TCLS(NCHNL),TRP(NTYRP),OMEGA(NFRQ),    &
      STD(NCHNL,NSTATQ,NCOMP,NPAR)
 COMPLEX SFD(NFRQ,NSTATQ,NCOMP,NPAR)

 T0_MIN = 0.1 / MAXVAL (FREQ)
 STD = 0.

 ALLOCATE (YSCAT(4,NTYRP),YFRQ(4,NFRQ),SFD_QUAD(NFRQ,NCOMP))
 YSCAT = 0. ; YFRQ = 0. ; SFD_QUAD = 0.

 ! step dB/dt (nT/s) = H * 1.0E9 * MU * dI/dt * TXMMNT / (I * 2*PI * FREQ)
 ! Originally VFAC was = 1.0E9 * MU * TX MOMENT / (2*PI);
 ! From 9/01, input is B in nT and 1/(2 Pi) is now absorbed in OMEGA
 ! If STEP = 1, output is in nanoteslas consistent with HYEY_INV.
 ! In this case, conversion to pT or fT occurs in TD_SENS.

 OMEGA(1:NFRQ) = TWOPI * FREQ(1:NFRQ)
 WF = LOG (OMEGA)
 OMEGA = -OMEGA     ! Division by -iw for step response.

 DO JP = 1, NPAR
   DO JS = 1, NSTATQ    ! Station loop.
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

 SUBROUTINE FD_FIELD (XMODL,NDATA,NSTAT,NSTATQ,NRX,NFRQ,BFDQ,PRM_FD,BFFAC, &
                      PPFAC,TXCLN,NRXST,KNRM,NORM,RXQ,RX,INVERT_RX_DATA, &
                      XMODL1,NDATA1)

!----------------------------------------------------------------------------
!
!*** Called by: RESJAC.
!
!  Construct the FD model data vector - Maximally coupled response only.
!
!----------------------------------------------------------------------------

 IMPLICIT NONE
 LOGICAL INVERT_RX_DATA
 INTEGER NDATA,NSTAT,NSTATQ,NFRQ,NRX,JD1,JD2,JS,JF,NRXST,KNRM,NDATA1
 REAL PRM_FD(NFRQ),TXCLN(NRXST),BFFAC,PPFAC,PRM4,NORM(KNRM),XMODL(NDATA), &
      RXQ(NSTATQ,NRX),RX(NSTAT,NRX),XMODL1(NDATA1)
 COMPLEX BFDQ(NFRQ,NSTATQ,3)
 COMPLEX, ALLOCATABLE :: BFD(:,:,:),BFD1(:,:)

 ! NOTE The - sign below is a consequence of using the sine transform for a +iwt
 !      sign convention. It is thus consistant with the convention used for the
 !      layered half space and in TDEM for time-domain scattered fields.
 !      The check is that over a layer, the coplanar and coaxial response for a
 !      Dighem configuration should be positive, at least at low frequencies.

 DO JF = 1, NFRQ
   PRM4 = BFFAC * ABS (PRM_FD(JF))                        ! Coupled primary in nT, pT or fT.
   NORM(JF) = PPFAC / PRM4                                ! For pct, ppt, ppm or ppb output.
   BFDQ(JF,1:NSTATQ,1:3) = -BFFAC * BFDQ(JF,1:NSTATQ,1:3) ! Total field in nT, pT or fT
 END DO

 ALLOCATE (BFD(NFRQ,NSTAT,3)) ; BFD = (0.,0.)

 CALL SET_DATA_FD (NFRQ,3,NSTATQ,NSTAT,BFDQ,RXQ,RX,BFD,NRX)

 ALLOCATE (BFD1(NFRQ,NSTAT)) ; BFD1 = (0.,0.)
 DO JF = 1, NFRQ
   DO JS = 1, NSTAT
     BFD1(JF,JS) = BFD(JF,JS,1) * SIN (TXCLN(JF)) + BFD(JF,JS,3) * COS (TXCLN(JF))
   END DO
   BFD1(JF,1:NSTAT) = NORM(JF) * BFD1(JF,1:NSTAT)
 END DO
 DEALLOCATE (BFD)

 DO JS = 1, NSTAT ! Model data for writing to ArjunAir.inv.
   DO JF = 1, NFRQ
     JD1 = JF + 2*(JS-1)*NFRQ
     JD2 = JF + 2*(JS-1)*NFRQ + NFRQ
     XMODL1(JD1) = REAL (BFD1(JF,JS))
     XMODL1(JD2) = AIMAG(BFD1(JF,JS))
   END DO
 END DO

 DEALLOCATE (BFD1)

 IF (INVERT_RX_DATA) THEN ! Model data at Rx positions.

   XMODL(1:NDATA) = XMODL1(1:NDATA)

 ELSE ! Model data at node positions.

   ALLOCATE (BFD1(NFRQ,NSTATQ)) ; BFD1 = (0.,0.)
   DO JF = 1, NFRQ
     DO JS = 1, NSTATQ
       BFD1(JF,JS) = BFDQ(JF,JS,1) * SIN (TXCLN(JF)) + BFDQ(JF,JS,3) * COS (TXCLN(JF))
     END DO
     BFD1(JF,1:NSTATQ) = NORM(JF) * BFD1(JF,1:NSTATQ)
   END DO
   DO JS = 1, NSTATQ
     DO JF = 1, NFRQ
       JD1 = JF + 2*(JS-1)*NFRQ
       JD2 = JF + 2*(JS-1)*NFRQ + NFRQ
       XMODL(JD1) = REAL (BFD1(JF,JS))
       XMODL(JD2) = AIMAG(BFD1(JF,JS))
     END DO
   END DO

   DEALLOCATE (BFD1)

 END IF

 END SUBROUTINE FD_FIELD

!============================================================================

 SUBROUTINE FD_SENS (JAC,NDATA,NPAR,NSTAT,NSTATQ,NFRQ,NRX,SFDQ,PRM_FD,BFFAC, &
                     PPFAC,NCOMP,KNRM,NORM,RXQ,RX,INVERT_RX_DATA)

!----------------------------------------------------------------------------
!
!*** Called by: RESJAC
!
!  Compute the FD sensitivity and construct the Jacobian matrix.
!
!----------------------------------------------------------------------------

 IMPLICIT NONE
 LOGICAL INVERT_RX_DATA
 INTEGER NDATA,NPAR,NSTAT,NSTATQ,NFRQ,NRX,JD1,JD2,JS,JF,JP,NCOMP,KNRM
 REAL JAC(NDATA,NPAR),PRM_FD(NFRQ),BFFAC,PPFAC,PRM4,NORM(KNRM),RXQ(NSTATQ,NRX), &
      RX(NSTAT,NRX)
 COMPLEX SFDQ(NFRQ,NSTATQ,NCOMP,NPAR)
 COMPLEX, ALLOCATABLE, DIMENSION(:,:,:) :: SFD1,SFD

 ! NOTE The - sign below is a consequence of using the sine transform for a +iwt
 !      sign convention.  It is thus consistant with the convention used for the
 !      layered half space and in TDEM for time-domain scattered fields.
 !      The check is that over a layer, the coplanar and coaxial response for a
 !      Dighem configuration should be positive, at least at low frequencies.

 DO JF = 1, NFRQ
   PRM4 = BFFAC * ABS (PRM_FD(JF))    ! Coupled primary in nT, pT or fT.
   NORM(JF) = PPFAC / PRM4            ! For pct, ppt, ppm or ppb output.
   SFDQ(JF,1:NSTATQ,1:NCOMP,1:NPAR) = -BFFAC * SFDQ(JF,1:NSTATQ,1:NCOMP,1:NPAR)
 END DO

 IF (INVERT_RX_DATA) THEN ! Interpolate sensitivity from nodes to RX positions.

   ALLOCATE (SFD1(NFRQ,NSTATQ,NCOMP),SFD(NFRQ,NSTAT,NCOMP))
   SFD1 = (0.,0.) ; SFD = (0.,0.)

   DO JP = 1, NPAR
     SFD1(1:NFRQ,1:NSTATQ,1:NCOMP) = SFDQ(1:NFRQ,1:NSTATQ,1:NCOMP,JP)
     CALL SET_SENS_FD (NFRQ,NCOMP,NSTATQ,NSTAT,SFD1,SFD,RXQ,RX,NRX)
     DO JF = 1, NFRQ
       SFD(JF,1:NSTAT,1:NCOMP) = NORM(JF) * SFD(JF,1:NSTAT,1:NCOMP)
     END DO
     DO JS = 1, NSTAT
       DO JF = 1, NFRQ
         JD1 = JF + 2*(JS-1)*NFRQ
         JD2 = JF + 2*(JS-1)*NFRQ + NFRQ
         JAC(JD1,JP) = REAL (SFD(JF,JS,1))
         JAC(JD2,JP) = AIMAG(SFD(JF,JS,1))
       END DO
     END DO
   END DO

   DEALLOCATE (SFD1,SFD)

 ELSE ! Compute sensitivity at nodes.

   DO JP = 1, NPAR
     DO JF = 1, NFRQ
       SFDQ(JF,1:NSTATQ,1:NCOMP,JP) = NORM(JF) * SFDQ(JF,1:NSTATQ,1:NCOMP,JP)
     END DO
     DO JS = 1, NSTATQ
       DO JF = 1, NFRQ
         JD1 = JF + 2*(JS-1)*NFRQ
         JD2 = JF + 2*(JS-1)*NFRQ + NFRQ
         JAC(JD1,JP) = REAL (SFDQ(JF,JS,1,JP))
         JAC(JD2,JP) = AIMAG(SFDQ(JF,JS,1,JP))
       END DO
     END DO
   END DO

 END IF

 END SUBROUTINE FD_SENS

!===========================================================================

 SUBROUTINE SET_SENS_FD (NFRQ,NCOMP,NSTATQ,NSTAT,SFD1,SFD,RXQ,RX,NRX)

!---------------------------------------------------------------------------
!
!*** Called by: FD_SENS
!
!  Interpolates the sensitivity SFD1 based on NSTATQ receivers at (RXQ,RZQ)
!  into SFD based on NSTAT receivers at (RX,RZ).
!
!---------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER NFRQ,NSTAT,NSTATQ,NCOMP,NRX,JC,JF,JS,JRX
 REAL RX(NSTAT,NRX),RXQ(NSTATQ,NRX),QR(4,NSTATQ),QI(4,NSTATQ),CUBVAL,A1,A2,X, &
      XC(NSTATQ)
 COMPLEX SFD(NFRQ,NSTAT,NCOMP),SFD1(NFRQ,NSTATQ,NCOMP)

 DO JC = 1, NCOMP
   DO JF = 1, NFRQ
     JRX = JF
     XC(1:NSTATQ) = RXQ(1:NSTATQ,JRX)
     DO JS = 1, NSTATQ
       QR(1,JS) = REAL (SFD1(JF,JS,JC))
       QI(1,JS) = AIMAG(SFD1(JF,JS,JC))
     END DO
     CALL CUBSPL(XC,QR,NSTATQ,0,0)
     CALL CUBSPL(XC,QI,NSTATQ,0,0)
     DO JS = 1, NSTAT
       X = RX(JS,JRX)
       A1 = CUBVAL(XC,QR,NSTATQ,X)
       A2 = CUBVAL(XC,QI,NSTATQ,X)
       SFD(JF,JS,JC) = CMPLX(A1,A2)
     END DO
   END DO
 END DO

 END SUBROUTINE SET_SENS_FD

!===========================================================================!
!                                                                           !
!             Generic inversion subroutines for P223F programs.             !
!                                                                           !
!===========================================================================!

 SUBROUTINE ESVD (AJAC,NDATA,NPAR,KP,WITHU,WITHV,SV,UMAT,VMAT,WSP,ETA,TOL,NW)

!----------------------------------------------------------------------------
!
!***  Called by: NLSQ
!***  Calls: DPROD1, WRITE_MESSAGE
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
!     INPUT:
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
!     OUTPUT:
!
!   SV    = Vector of the ordered NPAR singular values of AJAC.
!   UMAT  = Matrix of data (LHS) eigenvectors.
!   VMAT  = Matrix of model (RHS) eigenvectors.
!   WSP   = Workspace array of 3*NPAR.
!
!----------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER, PARAMETER :: ITMAX = 51
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

!============================================================================

 SUBROUTINE SOLVE_OVER (NPAR,NSV,RSVT,ZERO,VMAT,R,SV,DELPAR,WSP,PDRE)

!----------------------------------------------------------------------------
!
!***  Called by: NLSQ
!
!  DESCRIPTION:
!
!  Solve for the updated model parameters from the SVD of the Jacobian
!  matrix for over-determined systems, NDATA > NPAR:
!
!  DELPAR = VMAT * SV * RES
!
!  where RES = TRANSPOSE(UMAT) * RES.
!
!  The singular values, SV, are damped.
!
!     INPUT / OUTPUT:
!
!   NPAR   =  The number of model parameters
! * NSV    =  The returned number of singular values used in the solution.
!   RSVT   =  The relative singular value threshold for the current iteration.
!   ZERO   =  Rejection level for relative singular values; i.e., truncation
!             instead of damping for RSV less than zero. In NLSQ, ZERO is
!             set to the square of the minimum allowed value of RSVT.
!   VMAT   =  The NPAR by NPAR V matrix of model eigenvectors returned from ESVD.
!   R      =  The transformed error vector output by ESVD. It is the product
!             of the transposed U matrix times the observed error vector.
!   SV     =  The (ordered) array of NPAR singular values returned by ESVD;
!             i.e., SV(1) > SV(2) > SV(3), etc.
! * DELPAR =  The returned solution (changes in model parameters).
! * WSP    =  WSP (1:NPAR) contains the squared residual vector.
!             WSP (NPAR:2*NPAR) contains the damping factors.
!
! * indicates value returned by SOLVE_OVER.
!
!----------------------------------------------------------------------------

 INTEGER NSV,I,NPAR
 REAL EIGPAR(NPAR),R(NPAR),SV(NPAR),DELPAR(NPAR),WSP(3*NPAR), &
      VMAT(NPAR,NPAR),Q,DMPFAC,RSVT,ZERO,PDRE

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
   WSP(I) = R(I)*R(I)               ! Store the error.
   WSP(NPAR+I) = DMPFAC             ! Store the damping factors.
   EIGPAR(I) = (DMPFAC/SV(I))*R(I)  ! Eigenparameter calculation.
 END DO

 ! Calculate change in physical parameters from eigenparameters.

 DELPAR = MATMUL(VMAT,EIGPAR)

 ! Predicted decrease in residual error.

 PDRE = DOT_PRODUCT(WSP(1:NPAR),WSP(NPAR+1:2*NPAR))

 END SUBROUTINE SOLVE_OVER

!============================================================================

 SUBROUTINE SOLVE_UNDER (NDATA,NPAR,NSV,RSVT,ZERO,VMAT,R,SV,DELPAR,WSP,PDRE)

!----------------------------------------------------------------------------
!
!***  Called by: NLSQ
!
!  DESCRIPTION:
!
!  Solve for the updated model parameters from the SVD of the Jacobian
!  matrix:
!
!  DELPAR = VMAT * SV * RES
!
!  where RES = TRANSPOSE(UMAT) * RES. The singular values, SV, are damped.
!
!----------------------------------------------------------------------------
!
!     INPUT / OUTPUT:
!
!   NPAR   =  The number of model parameters
! * NSV    =  The returned number of singular values used in the solution.
!   RSVT   =  The relative singular value threshold for the current iteration.
!   ZERO   =  Rejection level for relative singular values; i.e., truncation
!             instead of damping for RSV less than zero. In NLSQ, ZERO is
!             set to the square of the minimum allowed value of RSVT.
!   VMAT   =  The NPAR by NPAR V matrix of model eigenvectors returned from ESVD.
!   R      =  The transformed error vector output by ESVD. It is the product
!             of the transposed U matrix times the observed error vector.
!   SV     =  The (ordered) array of NPAR singular values returned by ESVD;
!             i.e., SV(1) > SV(2) > SV(3), etc.
! * DELPAR =  The returned solution (changes in model parameters).
! * WSP    =  WSP (1:NPAR) contains the squared residual vector.
!             WSP (NPAR:2*NPAR) contains the damping factors.
!
! * indicates value returned by SOLVE_UNDER.
!
!----------------------------------------------------------------------------

 INTEGER NSV,I,NPAR,NDATA
 REAL EIGPAR(NDATA),R(NDATA),SV(NDATA),DELPAR(NPAR),WSP(3*NDATA), &
      VMAT(NPAR,NDATA),Q,DMPFAC,RSVT,ZERO,PDRE

 NSV = 0
 DO I = 1,NDATA
   WSP(I) = 0.
   WSP(NDATA+I) = 0.
   EIGPAR(I) = 0.
   Q = SV(I) / SV(1)
   IF (Q <= ZERO) CYCLE
   NSV = NSV + 1
   IF (Q < RSVT) THEN
     Q = (Q/RSVT)**4
     DMPFAC = Q/(1.0 + Q)
   ELSE
     DMPFAC = 1.0 / (1.0 + (RSVT/Q)**4)
   END IF
   WSP(I) = R(I)*R(I)               ! Store the error.
   WSP(NDATA+I) = DMPFAC            ! Store the damping factors.
   EIGPAR(I) = (DMPFAC/SV(I))*R(I)  ! Eigenparameter calculation.
 END DO

 ! Calculate change in physical parameters from eigenparameters.

 DELPAR = MATMUL(VMAT,EIGPAR)

 ! Predicted decrease in residual error.

 PDRE = DOT_PRODUCT(WSP(1:NDATA),WSP(NDATA+1:2*NDATA))

 END SUBROUTINE SOLVE_UNDER

!============================================================================

 SUBROUTINE SV_ANALYSIS_OVER (NPAR,VMAT,WSP,IMPORT)

!----------------------------------------------------------------------------
!
!***  Called by: NLSQ
!
!  Singular value analysis of the Jacobian matrix for parameter sensitivity
!  for an over-determined system.
!
!----------------------------------------------------------------------------
!
!     INPUT:
!
!   NPAR  = Number of model parameters.
!   VMAT  = Vector of model parameter eigenvectors from the SVD of the Jacobian
!           matrix.
!   SV    = Singular values of the Jacobian matrix.
!   BND   = Set minimum singular value threshold.
!   SIGMA = Symmetric standard error.
!
!     OUTPUT:
!
!   IMPORT = Model parameter importance; i.e., damping factors rotated into
!            physical parameter space.
!
!----------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER NPAR,J1,J2
 REAL VMAT(NPAR,NPAR),WSP(3*NPAR),IMPORT(NPAR),CUM_IMP

 DO J1 = 1, NPAR
   CUM_IMP = 0.0
   DO J2= 1, NPAR
     CUM_IMP = CUM_IMP + (VMAT(J1,J2) * WSP(NPAR+J2))**2
   END DO
   IMPORT(J1) = SQRT(CUM_IMP)
 END DO

 END SUBROUTINE SV_ANALYSIS_OVER

!============================================================================

 SUBROUTINE SV_ANALYSIS_UNDER (NDATA,NPAR,VMAT,WSP,IMPORT)

!----------------------------------------------------------------------------
!
!***  Called by: NLSQ
!
!  Singular value analysis of the Jacobian matrix for parameter sensitivity
!  for an under-determined system.
!
!----------------------------------------------------------------------------
!
!     INPUT:
!
!   NPAR  = Number of model parameters.
!   VMAT  = Vector of model parameter eigenvectors from the SVD of the Jacobian
!           matrix.
!   SV    = Singular values of the Jacobian matrix.
!   BND   = Set minimum singular value threshold.
!   SIGMA = Symmetric standard error.
!
!     OUTPUT:
!
!   IMPORT = Model parameter importance; i.e., damping factors rotated into
!            physical parameter space.
!
!----------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER NPAR,NDATA,J1,J2
 REAL VMAT(NPAR,NDATA),WSP(3*NDATA),IMPORT(NPAR),CUM_IMP

 DO J1 = 1, NPAR
   CUM_IMP = 0.0
   DO J2 = 1, NDATA
     CUM_IMP = CUM_IMP + (VMAT(J1,J2) * WSP(NDATA+J2))**2
   END DO
   IMPORT(J1) = SQRT(CUM_IMP)
 END DO

 END SUBROUTINE SV_ANALYSIS_UNDER

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
!     INPUT:
!
!   NPAR  = Number of model parameters.
!   NDATA = Number of data points.
!   XMODL = Model data in microvolts.
!   XDATA = Observed data in microvolts.
!   XWTS  = Weights for data points.
!
!     OUTPUT:
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

!===========================================================================!
!                                                                           !
!   The following subroutines are specific to ArjunAir forward modelling.   !
!                                                                           !
!===========================================================================!

 SUBROUTINE ARJUNA_2D (NLG,ND,NS,NX,NZ,NAIR,NXZ,NPN,NPM,LNODS,COORD, &
                       GEOELEC_PAR,NCOMP,NSTATQ,SXQ,SZQ,RXQ,RYQ,RZQ,IREL, &
                       NFRQ,FREQ,RESID,JCBN,TXCLNQ,NRXSTQ,NPAR,NRX,CMP,TDFD, &
                       WRITE_FWD)

!----------------------------------------------------------------------------
!
!*** Called by: RESJAC
!*** Calls: SHAPEF,FRONT_INV,HYEY_INV,SENS_FD_CONSTRUCT
!
!  Main subroutine for calling the Arjuna finite-element solutions for
!  inversion.
!
!  NOTE: All following computations are for node-based reciever positions!
!
!----------------------------------------------------------------------------
!
!     INPUT :
!
!   NLG   - log fle unit number
!   ND    - I/O unit for frequency domain data
!   NX    - total number of vertical node columns (user plus boundaries)
!   NZ    - total number of horizontal node rows (user plus air plus bottom boundary
!   NAIR  - number of nodes in the air
!   NXZ   - total number of cells in mesh  (NX-1) * (NZ-1)
!   NPN   - (2*NX-1)*(2*NZ-1) - NXZ
!   LNODS - array of element node numbers.
!   COORD - element coordinate array
!   NPROP - number of lithology properties
!   NLITH - number of lithologies defined for mesh
!   LYTH  - properties array of lithology
!   LITHD - element lithology array
!   ATX   - array of distinct transmitter orientations.
!   NKY   - number of wave numbers
!   KY    - array of wave numbers in North direction
!   NSTAT - number of stations
!   SXQ,SZQ - horizontal coordinate and altitude of transmitter at each station
!   NRX   - number of Tx-Rx offsets (= 1 for TD & NFRQ for FD
!   RX,RZ - horizontal coordinate and altitude of receiver at each station
!   RY    - Tx-Rx offset transverse to flight path
!   IREL  - array which identifies the mesh element (air) which contains
!           each receiver position along the survey.
!   FREQ  - array of NFRQ frequencies
!
!----------------------------------------------------------------------------

 IMPLICIT NONE
 LOGICAL JCBN,RESID,WRITE_FWD
 INTEGER, PARAMETER :: KYDEC = 5, NKY = 21 ! Set the number of Kx transform values.
 INTEGER ND,NS,NT,NFRQ,NLG,NX,NZ,NAIR,NXZ,NPN,LNODS(NXZ,8),NPM,NRX,NRXSTQ,CMP,NSTATQ, &
         IREL(NRX,NSTATQ),J,NCOMP,NTXQ,NPAR,TDFD
 REAL COORD(NPN,2),FREQ(NFRQ),KY(NKY),AST,GEOELEC_PAR(NXZ,6)
 REAL, DIMENSION(NRXSTQ) :: TXCLNQ
 REAL, DIMENSION(NSTATQ) :: SXQ,SZQ
 REAL, DIMENSION(NSTATQ,NRX) :: RXQ,RYQ,RZQ
 REAL, DIMENSION(:,:), ALLOCATABLE :: XTX,ZTX,ATX
 REAL, DIMENSION(:,:,:), ALLOCATABLE :: F1,F2,F3,F4,F5
 COMPLEX, ALLOCATABLE :: SKY(:,:,:,:,:)

 ! Set transform values in Kx domain.

 KY(1) = 1.E-5
 AST = 1. / REAL (KYDEC)
 DO J = 2, NKY
   KY(J) = KY(J-1) * 10.**AST
 END DO

 NT = 19 ; OPEN(NT,STATUS='SCRATCH') ; REWIND(ND) ; REWIND(NS)

 ! Set up the Tx positions and angles for the primal and adjoint (if required)
 ! sources for the ArjunAir forward solution.

 NTXQ = NSTATQ
 IF (JCBN) NTXQ = NSTATQ*(1+NCOMP)

 ALLOCATE (XTX(NTXQ,NRX),ZTX(NTXQ,NRX),ATX(NTXQ,NRX))
 XTX = 0. ; ZTX = 0. ; ATX = 0.

 CALL FWD_TX_INC_POS (JCBN,NTXQ,NSTATQ,NCOMP,XTX,ZTX,ATX,TXCLNQ,NRXSTQ, &
                      NRX,SXQ,SZQ,RXQ,RZQ,CMP,TDFD)

 ALLOCATE (F1(8,8,NXZ),F2(8,8,NXZ),F3(8,8,NXZ),F4(8,8,NXZ),F5(8,8,NXZ))

 CALL SHAPEF (NLG,COORD,LNODS,NXZ,NPN,NX,NZ,F1,F2,F3,F4,F5)

 ! Solve the forward problem.

 ALLOCATE (SKY(NKY,NFRQ,NSTATQ,NCOMP,NPAR)) ; SKY = (0.,0.)

 ! Frontal solution for Ex and Hx at all Rx points.

 CALL FRONT (NT,NX,NZ,NPN,NPM,NXZ,NAIR,XTX,ZTX,ATX,NTXQ,NSTATQ,LNODS, &
             NFRQ,FREQ,NKY,KY,GEOELEC_PAR,IREL,F1,F2,F3,F4,F5, &
             COORD,JCBN,RESID,TDFD,NRX,NCOMP,NPAR,SKY,WRITE_FWD)

 ! Compute Hx, Hy and Hz at the recievers.

 CALL HYEY (NT,ND,NPN,NXZ,LNODS,COORD,NFRQ,NKY,KY,IREL,NRX,RXQ,RYQ,RZQ, &
            NSTATQ,TDFD)

 ! Compute frequency-domain sensitivities.

 IF (JCBN) CALL SENS_FD_CONSTRUCT (NS,NFRQ,NSTATQ,NCOMP,NPAR,NKY,KY,NRX, &
                                   RYQ,TDFD,SKY)

 DEALLOCATE (F1,F2,F3,F4,F5,XTX,ZTX,ATX,SKY)

 CLOSE (NT)

 END SUBROUTINE ARJUNA_2D

!============================================================================

 SUBROUTINE SHAPEF (NLG,COORD,LNODS,NXZ,NPN,NX,NZ,F1,F2,F3,F4,F5)

!----------------------------------------------------------------------------
!
!*** Called by: ARJUNA_2D,ARJUNA_2DI
!
! Calculate the isoparametric shape functions for the 2D FEM mesh.
!
!----------------------------------------------------------------------------
!
!  Input:   NLG - log fle unit number
!           COORD - element coordinates array
!           LNODS - element node number array
!           NXZ   - maximum allowable number of elements
!           NPN   - dimension of coord
!           NX,NZ - number of nodes in X and Z directions respectively
!
! Output  : F1,F2,F3,F4,F5 - Derivative of the shape function arrays
!
!----------------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER NLG,NX,NZ,NXZ,NPN,I,J,I1,J1,IG,JG,ILM,LNODS(NXZ,8)
 REAL COORD(NPN,2),GS(3),GT(3),WG(3),S,T,DET,WEIGHT,DXDS,DZDS,DXDT,DZDT
 REAL, DIMENSION(8) :: SHP,DPDS,DPDT,DPDX,DPDZ,S0,T0
 REAL, DIMENSION(8,8,NXZ) :: F1,F2,F3,F4,F5
 DATA S0/-1., 0.,+1.,+1.,+1., 0.,-1.,-1./
 DATA T0/+1.,+1.,+1., 0.,-1.,-1.,-1., 0./
 DATA GS(1),GS(2),GS(3)/-0.774596669241483,0.,+0.774596669241483/
 DATA GT(1),GT(2),GT(3)/+0.774596669241483,0.,-0.774596669241483/
 DATA WG(1),WG(2),WG(3)/0.555555555555555,0.888888888888888,0.555555555555555/

 F1 = 0. ; F2 = 0. ; F3 = 0. ; F4 = 0. ; F5 = 0.

 DO I1 = 1, NX-1
   DO J1 = 1, NZ-1
     ILM = (I1-1)*(NZ-1) + J1
     DO IG = 1, 3
       DO JG = 1, 3

         S = GS(IG) ; T = GT(JG) ; WEIGHT = WG(IG) * WG(JG)

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
            DZDS = DZDS - DPDS(I) * COORD(ABS(LNODS(ILM,I)),2)
            DXDT = DXDT + DPDT(I) * COORD(ABS(LNODS(ILM,I)),1)
            DZDT = DZDT - DPDT(I) * COORD(ABS(LNODS(ILM,I)),2)
         END DO

         DET = DXDS*DZDT - DZDS*DXDT

         IF (ABS(DET) < 1.E-10)  THEN
           WRITE(*,1)   ILM,(COORD(ILM,J),J=1,2)
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

 1 FORMAT(//T3,'WARNING! Mesh error. DET = 0. at element',I8,'X=',E12.4,'Z=',E12.4 &
           /T3,'- EXECUTION HALTED -')

 END SUBROUTINE SHAPEF

!============================================================================

 SUBROUTINE FWD_TX_INC_POS (JCBN,NTXQ,NSTATQ,NCOMP,XTX,ZTX,ATX,TXCLNQ,NRXSTQ, &
                            NRX,SXQ,SZQ,RXQ,RZQ,CMP,TDFD)

!----------------------------------------------------------------------------
!
!*** Called by: ARJUNA_2D_INV
!
!  Set up the vectors of the Tx positions and angles for the primal and
!  adjoint sources.
!
!----------------------------------------------------------------------------

 IMPLICIT NONE
 LOGICAL JCBN
 REAL, PARAMETER :: PI = 3.14159265359
 INTEGER NRX,NTXQ,NSTATQ,NCOMP,CMP,NRXSTQ,TDFD,JTX,JS,JC,JR
 REAL TXCLNQ(NRXSTQ)
 REAL, DIMENSION(NSTATQ) :: SXQ,SZQ
 REAL, DIMENSION(NSTATQ,NRX) :: RXQ,RZQ
 REAL, DIMENSION(NTXQ,NRX) :: XTX,ZTX,ATX

 ! Set up the vectors of the Tx positions for the primal and adjoint problems.

 IF (JCBN) THEN             ! Include adjoint Tx positions.
   DO JS = 1, NSTATQ        ! Up to NSTATQ are the primal Tx positions.
     DO JR = 1, NRX
       XTX(JS,JR) = SXQ(JS)       ! Primal horizontal Tx position
       ZTX(JS,JR) = SZQ(JS)       ! Primal Tx position in Z.
     END DO
   END DO
   DO JC = 1, NCOMP         ! After NSTATQ, they are the adjoint Tx positions,
     DO JS = 1, NSTATQ      ! which are the Rx positions from the primal problem.
       JTX = JS + JC*NSTATQ
       DO JR = 1, NRX
         XTX(JTX,JR) = RXQ(JS,JR) ! Adjoint horizontal Tx position
         ZTX(JTX,JR) = RZQ(JS,JR) ! Adjoint Tx position in Z.
       END DO
     END DO
   END DO
 ELSE                       ! Primal problem only; no adjoint transmitters.
   DO JS = 1, NSTATQ
     DO JR = 1, NRX
       XTX(JS,JR) = SXQ(JS)       ! Primal horizontal Tx position
       ZTX(JS,JR) = SZQ(JS)       ! Primal Tx position in Z.
     END DO
   END DO
 END IF

 ! Set up the vector of the Tx pitch for the primal and adjoint problems.

 IF (JCBN) THEN                   ! Include adjoint Tx positions.
   DO JS = 1, NSTATQ              ! Up to NSTATQ are the primal Tx positions.
     IF (TDFD < 2) THEN
       ATX(JS,1) = TXCLNQ(JS)     ! Primal Tx pitch for TD system.
     ELSE IF (TDFD == 2) THEN
       DO JR = 1, NRXSTQ
         ATX(JS,JR) = TXCLNQ(JR)  ! Primal Tx angle for FD system.
       END DO
     END IF
   END DO
   DO JC = 1, NCOMP               ! After NSTATQ, they are the adjoint Tx positions,
     DO JS = 1, NSTATQ            ! which are the Rx positions from the primal problem.
       JTX = JS + JC*NSTATQ
       IF (TDFD < 2) THEN
         IF (CMP == 11) ATX(JTX,1) = 90. * PI / 180. ! Pitch = 90 degrees for in-line.
         IF (CMP == 13) ATX(JTX,1) = 0.              ! Pitch =  0 degrees for vertical.
         IF (CMP == 2) THEN
           IF (JTX < 2*NSTATQ+1) THEN             ! Vertical first, then in-line.
             ATX(JTX,1) = 0.                         ! Pitch =  0 degrees for vertical.
           ELSE
             ATX(JTX,1) = 90. * PI / 180             ! Pitch = 90 degrees for in-line.
           END IF
         END IF
       ELSE IF (TDFD == 2) THEN
         DO JR = 1, NRXSTQ
           ATX(JTX,JR) = TXCLNQ(JR) ! Angle for maximally coupled component = Angle of primal Tx.
         END DO
       END IF
     END DO
   END DO
 ELSE                            ! Primal problem only; no adjoint fields.
   DO JS = 1, NSTATQ
     IF (TDFD < 2) THEN
       ATX(JS,1) = TXCLNQ(JS)    ! Primal Tx pitch for TD system.
     ELSE IF (TDFD == 2) THEN
       DO JR = 1, NRXSTQ
         ATX(JS,JR) = TXCLNQ(JR) ! Primal Tx angle for FD system.
       END DO
     END IF
   END DO
 END IF

 END SUBROUTINE FWD_TX_INC_POS

!============================================================================

 SUBROUTINE FRONT (NT,NX,NZ,NPN,NPM,NXZ,NAIR,XTX,ZTX,ATX,NTXQ,NSTATQ, &
                   LNODS,NFRQ,FREQ,NKY,KY,GEOELEC_PAR,IREL,F1,F2,F3,F4,F5, &
                   COORD,JCBN,RESID,TDFD,NRX,NCOMP,NPAR,SKY,WRITE_FWD)

!----------------------------------------------------------------------------
!
!*** Called by: ARJUNA_2D_INV
!*** Calls: EHFLD_INV, SENS_KY_CONSTRUCT
!*** Uses: AA_Filter_coefficients
!
!  Assemble the element matrices and solves the global system of equations
!  by the frontal solution. The source factor SFAC = i*w*u/(4*PI) is applied
!  near the end of the Kx loop rather than in the source subroutine EHFILD to
!  save computational time. If required, compute the KY sensitivities.
!
!----------------------------------------------------------------------------
!
!     INPUT :
!
!    NX,NZ - number of vertical node columns and horizontal node rows respectively
!    NXZ   - total number of cells in mesh  (NX-1) * (NZ-1)
!    NAIR  - number of nodes in the air
!    NPN   - (2*NX-1)*(2*NZ-1) - NXZ
!    NPM   - number of elements not in air
!    SXQ,SZQ - horizontal & altitude coordinates of transmitter stations
!    NSTAT - number of primal + adjoint transmitter stations
!    NTX   - number of primal transmitter stations.
!    LNODS - array of element node numbers.
!    FREQ  - array of NFRQ frequencies
!    NKY   - number of wave numbers
!    KY    - array of wave numbers in North direction
!    NPROP - number of lithology properties
!    NLITH - number of lithologies defined for mesh
!    LYTH  - properties array of lithology
!    LITHD - element lithology array
!    IREL  - array which identifies the mesh element (air) which contains
!            each receiver position along the survey.
!    NRX   - number of Tx-Rx offsets
!    ARGTX - array of distinct transmitter orientations in radians.
!    COORD - element coordinate array
!  F1,F2,F3,F4,F5 - Derivative of the shape function arrays
!
!     OUTPUT:
!
!   HYY   - Wave domain H along strike at Rx locations written to unit NT.
!   SKY   - Wave domain H sensitivities at Rx locations.
!
!----------------------------------------------------------------------------

 USE AA_Filter_coefficients
 IMPLICIT NONE
 REAL, PARAMETER :: EPS0 = 8.854156E-12, MU0 = 12.56637E-7, PI = 3.14159265359
 COMPLEX, PARAMETER :: ONE=(1.,0.), CI=(0.,1.)
 LOGICAL JCBN,RESID,WRITE_FWD
 INTEGER NT,NFRQ,NKY,MFRON,NPSR,I,J,K,I1,J1,II,ITX,IFRON,JFRON,ILM,KELV1,NX, &
         NZ,NN,NAIR,NSTATQ,NTXQ,IKY,NPOS,NPN,NXZ,NLOC,ILM1,NEMAX(2),LNODS(NXZ,8), &
         LOCEL(16,NXZ),NDEST(16,NXZ),JF,NRX,NPM,NSOL,JSOL,INODEL,IDEST,TDFD, &
         KRX,IREL(NRX,NSTATQ),NPAR,NCOMP
 INTEGER, ALLOCATABLE, DIMENSION(:,:) :: NLOCA,NAC
 INTEGER, ALLOCATABLE, DIMENSION(:) :: NFUNC,IFRO1,NIKN1,ID,NACVA
 REAL SIGR,KY1SQ,KY(NKY),FRQ,KY1,COORD(NPN,2),FREQ(NFRQ),OMEGA,MU,EPS,CALF, &
      CTAU,CFRQ,WMU,YY(NLO:NHI),GEOELEC_PAR(NXZ,6)
 REAL, DIMENSION(NTXQ,NRX) :: XTX,ZTX,ATX
 REAL, ALLOCATABLE, DIMENSION(:,:,:) :: EX,EY,EZ
 REAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: SRC1,SRC2,SRC3
 REAL, DIMENSION(8,8,NXZ) :: F1,F2,F3,F4,F5
 COMPLEX, ALLOCATABLE :: SR(:)
 COMPLEX, ALLOCATABLE, DIMENSION(:,:) :: RR,EQRH1,EQUAT,VECRV
 COMPLEX PIVOT,PP,QQ,SS,ER(16,16),CWMU,CKY,TMP,KY2,SIG,SFAC,CC,DEN,HYY(8), &
         SKY(NKY,NFRQ,NSTATQ,NCOMP,NPAR)
 DOUBLE PRECISION DELTA
 Integer :: tvals(8)
 
 Save

 REWIND(NT)

 MFRON = 2 * (2*NZ+4)
 NPSR  = MFRON*(MFRON+1)/2
 
 ALLOCATE (NFUNC(MFRON),NACVA(MFRON),NAC(MFRON,NXZ),NLOCA(MFRON,MFRON), &
           IFRO1(2*NPN),NIKN1(2*NPN),ID(NPN),SRC1(NTXQ,8,NPM,NRX),   &
           SRC2(NTXQ,8,NPM,NRX),SRC3(NTXQ,8,NPM,NRX))

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

 ! Assign negative values to "FRONT-NODES"

 DO I = 1, (2*NX-1)*(2*NZ-1) - (NX-1)*(NZ-1)
   DO ILM = 1, (NX-1)*(NZ-1)
     DO NN = 1, 8
       IF (LNODS(ILM,NN) == I)  THEN
         I1 = ILM
         J1 = NN
       END IF
      END DO
   END DO
   LNODS(I1,J1) = -I
 END DO

 ! Assign variable node numbering.

 DO I = 1, NX-1
   DO J = 1, NZ-1
     ILM = (I-1)*(NZ-1) + J
     DO NN = 1, 8
       LOCEL(2*NN  ,ILM) = 2*LNODS(ILM,NN)
       LOCEL(2*NN-1,ILM) = 2*LNODS(ILM,NN) - 1
       IF (LNODS(ILM,NN) < 0)  LOCEL(2*NN-1,ILM) = 2*LNODS(ILM,NN) + 1
     END DO
   END DO
 END DO

 NEMAX(1) = 2*NX - 1
 NEMAX(2) = NX
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

 ! Frontal solution method book keeping.

 NACVA =  0
 DO I1 = 1, NX-1
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
         IF ( NACVA(IFRON) == 0 ) THEN
           NDEST(I,ILM) = IFRON
           NACVA(IFRON) = INODEL
           CYCLE ILOOP
         END IF
       END DO
     END DO ILOOP

     NAC(1:MFRON,ILM) = NACVA(1:MFRON)

     DO K = 1, 16
       IF ( LOCEL(K,ILM) < 0 ) THEN
         DO IFRON = 1, MFRON
           IF ( NACVA(IFRON) == -LOCEL(K,ILM) ) NACVA(IFRON) = 0
         END DO
       END IF
     END DO
   END DO
 END DO

 ! Coordinates for cosine and sine transform.

 DELTA = LOG (10.D0) / REAL (NDEC,8)
 DO I = NLO, NHI
   YY(I) = REAL(EXP(I*DELTA))
 END DO

 NSOL = NKY * NFRQ
 KY_LOOP: DO IKY = 1, NKY ! Solve for every Kx-value and frequencies.
   KY1 = KY(IKY)
   Call Date_and_Time(Values = tvals)
   IF (WRITE_FWD) WRITE(*, 1, Advance = 'No') tvals(5:7), tvals(3:1:-1), IKY, NKY, nfrq ! ,KY1
   KY1SQ = KY1 * KY1
   KY2 = CMPLX (KY1SQ, 0.)
   CKY = CMPLX (0., KY1)

   ALLOCATE (EX(NTXQ,NRX,NPN),EY(NTXQ,NRX,NPN),EZ(NTXQ,NRX,NPN))
   EX = 0. ; EY = 0. ; EZ = 0.

   CALL EHFLD (NX,NZ,NPN,NPM,NXZ,NAIR,XTX,ZTX,NTXQ,COORD,KY1,NRX,ATX, &
               LNODS,F2,F4,F5,SRC1,SRC2,SRC3,ID,YY,EX,EY,EZ)

   ALLOCATE (EQRH1(NTXQ,2*NPN),VECRV(NTXQ,2*NPN),EQUAT(MFRON,2*NPN), &
             RR(NTXQ,2*NPN),SR(NPSR))

   FREQUENCY_LOOP: DO JF = 1, NFRQ

     FRQ = FREQ(JF)
     OMEGA = 2. * PI * FRQ
     JSOL = NFRQ * (IKY-1) + JF - 1
     ! IFIN = 100. * JSOL / REAL (NSOL)
     If (write_FWD) Write (*, 3, Advance = 'No')

     IF (TDFD < 2) KRX = 1
     IF (TDFD == 2) KRX = JF

     ! Initialize the frontal solution arrays.

     EQRH1 = (0.,0.) ; EQUAT = (0.,0.) ; RR = (0.,0.) ; SR = (0.,0.)
     KELV1 = 0 ; ILM1 = 0

     DO I1 = 1, NX-1
       DO J1 = 1, NZ-1
         ILM = (I1-1)*(NZ-1) + J1

         SIGR = 1./GEOELEC_PAR(ILM,1)
         MU   = MU0 * GEOELEC_PAR(ILM,2)
         EPS  = EPS0 * GEOELEC_PAR(ILM,3)
         CALF = 1. - GEOELEC_PAR(ILM,4)
         CTAU = GEOELEC_PAR(ILM,5)
         CFRQ = GEOELEC_PAR(ILM,6)

         SIG = CMPLX (SIGR, 0.) ! sigr + iw eps0
         CC = (CI * OMEGA * CTAU)**CFRQ
         SIG = SIGR * (ONE + CC) / (ONE + CALF*CC)
         SIG = SIG + CI * OMEGA * EPS
         WMU = OMEGA * MU       !  wu
         CWMU = CI * WMU        !  iwu
         SFAC = CWMU / (4.*PI)  !  iwu / 4 PI

         DEN = KY2 + CWMU*SIG
         PP  = -CWMU / DEN
         QQ  = -CKY  / DEN
         SS  = -SIG  / DEN

         ! Calculate the Left-hand side of the element system matrix.

         DO I = 1, 8
           DO J = 1, 8
             ER(2*I-1,2*J-1) =  PP*F1(I,J,ILM) - CWMU*F2(I,J,ILM)
             ER(2*I  ,2*J  ) =  SS*F1(I,J,ILM) -  SIG*F2(I,J,ILM)
             ER(2*I-1,2*J  ) =  QQ*F3(I,J,ILM)
             ER(2*I  ,2*J-1) = -QQ*F3(I,J,ILM)
           END DO
         END DO

         DO I = 1, 16
           DO J = I, 16
             NLOC = NLOCA(NDEST(I,ILM),NDEST(J,ILM))
             SR(NLOC) = SR(NLOC) + ER(I,J)
           END DO
         END DO

         ! Calculate the Right-hand side of the element system matrix.

         IF ( J1 > NAIR ) THEN
           ILM1 = ILM1 + 1
           DO I = 1, 8
             IDEST = NDEST(2*I,ILM)
             DO ITX = 1, NTXQ
               RR(ITX,IDEST-1) = RR(ITX,IDEST-1) + SIG* PP * SRC1(ITX,I,ILM1,KRX)*CI
               RR(ITX,IDEST  ) = RR(ITX,IDEST  ) + SIG*(QQ * SRC2(ITX,I,ILM1,KRX)*CI &
                                                 +     CMPLX(SRC3(ITX,I,ILM1,KRX),0.))
             END DO
           END DO
         END IF

         ! Solve the equations using frontal solution method.

         DO K = 1, 16
           IF ( LOCEL(K,ILM) < 0 ) THEN
             DO IFRON = 1, MFRON
               IF ( NAC(IFRON,ILM) == -LOCEL(K,ILM) ) THEN
                 KELV1 = KELV1 + 1
                 IFRO1(KELV1) =  IFRON
                 NIKN1(KELV1) = -LOCEL(K,ILM)
                 DO JFRON = 1, MFRON
                   J = NLOCA(IFRON,JFRON)
                   EQUAT(JFRON,KELV1) = SR(J)
                   SR(J) = (0.,0.)
                 END DO
                 EQRH1(1:NTXQ,KELV1) = RR(1:NTXQ,IFRON)
                 RR(1:NTXQ,IFRON) = (0.,0.)
                 PIVOT = EQUAT(IFRON,KELV1)
                 EQUAT(IFRON,KELV1) = (0.,0.)
                 DO I = 1, MFRON
                   TMP = EQUAT(I,KELV1) / PIVOT
                   DO J = 1, I
                     II = NFUNC(I) + J
                     SR(II) = SR(II) - TMP*EQUAT(J,KELV1)
                   END DO
                   RR(1:NTXQ,I) = RR(1:NTXQ,I) - TMP*EQRH1(1:NTXQ,KELV1)
                 END DO
                 EQUAT(IFRON,KELV1) = PIVOT
               END IF
             END DO
           END IF
         END DO

       END DO
     END DO

     ! Backsubtitution using frontal solution method

     VECRV = (0.,0.)
     DO K = KELV1, 1, -1
       IFRON = IFRO1(K)
       PIVOT = EQUAT(IFRON,K)
       EQUAT(IFRON,K) = (0.,0.)
       DO ITX = 1, NTXQ
         TMP = (0.,0.)
         DO J = 1, MFRON
           TMP = TMP + VECRV(ITX,J)*EQUAT(J,K)
         END DO
         VECRV(ITX,IFRON) = (EQRH1(ITX,K)-TMP) / PIVOT
         RR(ITX,NIKN1(K)) = VECRV(ITX,IFRON)
       END DO
     END DO

     ! Write Hx fields at elements with Rx to file.

     IF (RESID) THEN
       DO ITX = 1, NSTATQ ! Tx loop; primal positions only!
         HYY = (0.,0.)
         DO J = 1, 8
           NPOS = ABS(LNODS(IREL(KRX,ITX),J))
           HYY(J) = SFAC * RR(ITX,2*NPOS-1)
         END DO
         WRITE (NT,8) (HYY(J),J=1,8)
       END DO
     END IF

     IF (JCBN) CALL SENS_KY_CONSTRUCT (NX,NZ,NAIR,NXZ,NPN,NPAR,JF,FRQ,NFRQ,IKY, &
                                       KY1,NKY,NRX,LNODS,COORD,NSTATQ,NTXQ,NCOMP, &
                                       GEOELEC_PAR,TDFD,RR,SKY,EX,EY,EZ)

   END DO FREQUENCY_LOOP

   DEALLOCATE (RR,EQRH1,VECRV,EQUAT,SR,EX,EY,EZ)

 END DO KY_LOOP

 DO ILM = 1, (NX-1)*(NZ-1)
   LNODS(ILM,1:8) = ABS(LNODS(ILM,1:8))
 END DO
 Write (*, 4)

 DEALLOCATE (NFUNC,NACVA,NAC,IFRO1,NIKN1,SRC1,SRC2,SRC3,ID,NLOCA)

 !1 FORMAT(/2X,'IKY =',I3,' of',I3,'.  Solving for KY =',G12.3)
 1 FORMAT(/2X, i2.2, ':', i2.2, ':', i2.2, ' on ', i2.2, '/', i2.2, '/', i4.4, &
                 ': Wavenumber ', I3, ' of ', I3, ' (', i2, ' frequencies) : ')
 2 FORMAT(6X,'Frequency',I3,' =',G12.4,F8.1,' percent done.')
 3 Format ('.')
 4 Format ('')
 8 FORMAT(16E14.6)

 END SUBROUTINE FRONT

!============================================================================

 SUBROUTINE EHFLD (NX,NZ,NPN,NPM,NXZ,NAIR,XTX,ZTX,NTXQ,COORD,KY1,NRX,ATX, &
                   LNODS,F2,F4,F5,SRC1,SRC2,SRC3,ID,YY,EX,EY,EZ)

!----------------------------------------------------------------------------
!
!*** Called by: FRONT
!*** Calls: CUBSPL
!*** Uses: AA_Filter_coefficients
!
!  Calculate the halfspace electric and magnetic fields in wave number domain.
!
!  In KY domain, each of these should have a factor: SFAC = iwu / (4 PI)
!  This is applied in subroutine FRONT instead of here to speed computation
!  time.
!
!----------------------------------------------------------------------------
!
! Input:    NX,NZ - number of vertical node columns and horizontal node rows
!                   respectively
!           NPN   - (2*NX-1)*(2*NZ-1) - NXZ
!           NPM   - number of elements not in air
!           NAIR  - number of node rows in the air
!           SXQ,SZQ - horizontal & altitude coordinates of transmitter stations
!           NTXQ - number of transmitter stations
!           COORD - arrays of element coordinates.
!           KY1   - wave number in North direction
!           NTXOR - number of distinct transmitter angles
!           TXOR  - array of distinct transmitter angles in radians
!           LNODS - arrays of element node numbers.
!        F2,F4,F5 - derivative of shape function arrays
!
! Output  : SRC1,SRC2,SRC3 - source fields in wave-domain
!
!----------------------------------------------------------------------------

 USE AA_Filter_coefficients
 IMPLICIT NONE
 INTEGER NAIR,NX,NZ,NPN,NPM,NXZ,NTXQ,I,J,I1,J1,JS,KTX,NN,IA(8),ILM,ILM1, &
         LNODS(NXZ,8),NEMAX(2),ID(NPN),IM,IDD,NRX
 REAL, DIMENSION(8,8,NXZ) :: F2,F4,F5
 REAL COORD(NPN,2),Y,Z,R,R2,R3,TRC,TRS,GS(2),S(3,2),D(3,2),DJX1,DJX2,DJZ1, &
      DJZ2,KY1,YY(NLO:NHI),G(NLO:NHI),Y2,Z2
 REAL, DIMENSION (NTXQ,NRX) :: ATX,SNTX,CSTX,XTX,ZTX
 REAL, DIMENSION (NTXQ,8,NPM,NRX) :: SRC1,SRC2,SRC3
 REAL, DIMENSION (NTXQ,NRX,NPN) :: EX,EY,EZ
 REAL  ED1,ED2,ED3,ED4,ED5,ED6,ED7,ED8
 DATA GS(1),GS(2)/-0.577350269189626,+0.577350269189626/

 DO I = 1, 2 ! Line shape funtions

   S(1,I) = (GS(I)*GS(I) + GS(I))/2.
   S(2,I) = (1. - GS(I)*GS(I))
   S(3,I) = (GS(I)*GS(I) - GS(I))/2.

   D(1,I) = (2*GS(I) + 1.)/2.
   D(2,I) = -2*GS(I)
   D(3,I) = (2*GS(I) - 1.)/2.

 END DO

 DO JS = 1, NTXQ
   DO KTX = 1, NRX
     SNTX(JS,KTX) = SIN(ATX(JS,KTX))
     CSTX(JS,KTX) = COS(ATX(JS,KTX))
   END DO
 END DO

 G = YY / KY1
 NEMAX(1) = 2*NX - 1
 NEMAX(2) = NX
 NN = 0

 EX = 0. ; EY = 0. ; EZ = 0.

 DO J1 = 2*NAIR+1, 2*NZ-1
   IM = 1
   IF (MOD(J1,2) == 0)  IM = 2
   DO I1 = 1, NEMAX(IM)
     NN  = NN + 1
     IDD = ID(NN)
     DO JS = 1, NTXQ
       DO KTX = 1, NRX
         Y = COORD(IDD,1) - XTX(JS,KTX)
         Z = COORD(IDD,2) + ZTX(JS,KTX)
         Y2 = Y*Y
         Z2 = Z*Z
         TRC = 0. ; TRS = 0.
         DO I = NLO, NHI
           R2 = G(I)*G(I) + Y2 + Z2
           R  = SQRT(R2)
           R3 = R*R2
           TRC = TRC + WCOS(I) / R3
           TRS = TRS + WSIN(I) * G(I) / R3
         END DO
         EX(JS,KTX,IDD) =  TRS * CSTX(JS,KTX)                       ! Transverse to strike E field
         EY(JS,KTX,IDD) =  TRC * (Y*CSTX(JS,KTX) - Z*SNTX(JS,KTX))  ! Along strike E field
         EZ(JS,KTX,IDD) = -TRS * SNTX(JS,KTX)                       ! Vertical E field
       END DO
     END DO
   END DO
 END DO

 EX = EX / KY1
 EY = EY / KY1
 EZ = EZ / KY1

 SRC1 = 0. ; SRC2 = 0. ; SRC3 = 0.

 ILM1 = 0
 DO I1 = 1, NX-1
   DO J1 = NAIR+1, NZ-1
     ILM  = (I1-1)*(NZ-1) + J1
     ILM1 = ILM1 + 1
     IA(1:8) = ABS(LNODS(ILM,1:8))
     DO KTX = 1, NRX
       DO I = 1, 8
         DO J = 1, 8
           DO JS = 1, NTXQ
             SRC1(JS,I,ILM1,KTX) = SRC1(JS,I,ILM1,KTX) + (F5(I,J,ILM) * EX(JS,KTX,IA(J)) &
                                                       -  F4(I,J,ILM) * EZ(JS,KTX,IA(J)))
             SRC2(JS,I,ILM1,KTX) = SRC2(JS,I,ILM1,KTX) + (F4(I,J,ILM) * EX(JS,KTX,IA(J)) &
                                                       +  F5(I,J,ILM) * EZ(JS,KTX,IA(J)))
             SRC3(JS,I,ILM1,KTX) = SRC3(JS,I,ILM1,KTX) +  F2(I,J,ILM) * EY(JS,KTX,IA(J))
           END DO
         END DO
       END DO
     END DO

     DO I = 1, 2
       DJX1 = D(1,I) * COORD(IA(1),1) + D(2,I)*COORD(IA(2),1) + D(3,I)*COORD(IA(3),1)
       DJX2 = D(1,I) * COORD(IA(7),1) + D(2,I)*COORD(IA(6),1) + D(3,I)*COORD(IA(5),1)
       DJZ1 = D(1,I) * COORD(IA(3),2) + D(2,I)*COORD(IA(4),2) + D(3,I)*COORD(IA(5),2)
       DJZ2 = D(1,I) * COORD(IA(1),2) + D(2,I)*COORD(IA(8),2) + D(3,I)*COORD(IA(7),2)
       DO KTX = 1, NRX
         DO JS = 1, NTXQ
           ED1 = DJX1 * (S(1,I)*EX(JS,KTX,IA(1)) + S(2,I)*EX(JS,KTX,IA(2)) + S(3,I)*EX(JS,KTX,IA(3)))
           ED2 = DJX2 * (S(1,I)*EX(JS,KTX,IA(7)) + S(2,I)*EX(JS,KTX,IA(6)) + S(3,I)*EX(JS,KTX,IA(5)))
           ED3 = DJZ1 * (S(1,I)*EZ(JS,KTX,IA(3)) + S(2,I)*EZ(JS,KTX,IA(4)) + S(3,I)*EZ(JS,KTX,IA(5)))
           ED4 = DJZ2 * (S(1,I)*EZ(JS,KTX,IA(1)) + S(2,I)*EZ(JS,KTX,IA(8)) + S(3,I)*EZ(JS,KTX,IA(7)))
           ED5 = DJZ1 * (S(1,I)*EX(JS,KTX,IA(3)) + S(2,I)*EX(JS,KTX,IA(4)) + S(3,I)*EX(JS,KTX,IA(5)))
           ED6 = DJZ2 * (S(1,I)*EX(JS,KTX,IA(1)) + S(2,I)*EX(JS,KTX,IA(8)) + S(3,I)*EX(JS,KTX,IA(7)))
           ED7 = DJX1 * (S(1,I)*EZ(JS,KTX,IA(1)) + S(2,I)*EZ(JS,KTX,IA(2)) + S(3,I)*EZ(JS,KTX,IA(3)))
           ED8 = DJX2 * (S(1,I)*EZ(JS,KTX,IA(7)) + S(2,I)*EZ(JS,KTX,IA(6)) + S(3,I)*EZ(JS,KTX,IA(5)))

           SRC1(JS,1,ILM1,KTX) = SRC1(JS,1,ILM1,KTX) + S(1,I) * ED1 - S(1,I) * ED4
           SRC1(JS,2,ILM1,KTX) = SRC1(JS,2,ILM1,KTX) + S(2,I) * ED1
           SRC1(JS,3,ILM1,KTX) = SRC1(JS,3,ILM1,KTX) + S(3,I) * ED1 + S(1,I) * ED3
           SRC1(JS,4,ILM1,KTX) = SRC1(JS,4,ILM1,KTX)                + S(2,I) * ED3
           SRC1(JS,5,ILM1,KTX) = SRC1(JS,5,ILM1,KTX) - S(3,I) * ED2 + S(3,I) * ED3
           SRC1(JS,6,ILM1,KTX) = SRC1(JS,6,ILM1,KTX) - S(2,I) * ED2
           SRC1(JS,7,ILM1,KTX) = SRC1(JS,7,ILM1,KTX) - S(1,I) * ED2 - S(3,I) * ED4
           SRC1(JS,8,ILM1,KTX) = SRC1(JS,8,ILM1,KTX)                - S(2,I) * ED4

           SRC2(JS,1,ILM1,KTX) = SRC2(JS,1,ILM1,KTX) + S(1,I) * ED6 + S(1,I) * ED7
           SRC2(JS,2,ILM1,KTX) = SRC2(JS,2,ILM1,KTX)                + S(2,I) * ED7
           SRC2(JS,3,ILM1,KTX) = SRC2(JS,3,ILM1,KTX) - S(1,I) * ED5 + S(3,I) * ED7
           SRC2(JS,4,ILM1,KTX) = SRC2(JS,4,ILM1,KTX) - S(2,I) * ED5
           SRC2(JS,5,ILM1,KTX) = SRC2(JS,5,ILM1,KTX) - S(3,I) * ED5 - S(3,I) * ED8
           SRC2(JS,6,ILM1,KTX) = SRC2(JS,6,ILM1,KTX)                - S(2,I) * ED8
           SRC2(JS,7,ILM1,KTX) = SRC2(JS,7,ILM1,KTX) + S(3,I) * ED6 - S(1,I) * ED8
           SRC2(JS,8,ILM1,KTX) = SRC2(JS,8,ILM1,KTX) + S(2,I) * ED6
         END DO
       END DO
     END DO
   END DO
 END DO

 END SUBROUTINE EHFLD

!============================================================================

 SUBROUTINE SENS_KY_CONSTRUCT (NX,NZ,NAIR,NXZ,NPN,NPAR,JF,FRQ,NFRQ,JKY,KY,NKY, &
                               NRX,LNODS,COORD,NSTATQ,NTXQ,NCOMP,GEOELEC_PAR, &
                               TDFD,RR,SKY,EX,EY,EZ)

!----------------------------------------------------------------------------
!
!*** Called by: ARJUNA_2D_INV
!*** Uses: AA_Filter_coefficients
!
! Computes sensitivities for node-placed recievers!
!
! Compute the volume integral of the dot product of the primal and adjoint
! electric fields in every cell in the Fourier transform domain. Primary
! and scattered Ex, Ey and Ez fields are computed at all Gaussian integration
! points. The scattered fields are computed by shape function interpolation
! and differentiation. The dot product of the primal and adjoint fields is
! then evaluated and summed as per 9 point Gaussian integration in 2-D.
!
!----------------------------------------------------------------------------

 IMPLICIT NONE
 REAL, PARAMETER :: PI = 3.14159265359, MU0 = 12.56637E-7, EPS0 = 8.854156E-12
 COMPLEX, PARAMETER :: CI = (0.,1.), ONE = (1.,0.)
 INTEGER NFRQ,NKY,NPN,NX,NZ,NXZ,NAIR,JC1,JC2,JF,JKY,JTX,JS,NSTATQ,JCOMP, &
         NCOMP,I,I1,J1,LNODS(NXZ,8),NPAR,NTXQ,NRX,TDFD,IG,JG,KTX,NPOS
 REAL COORD(NPN,2),DPDX(8),DPDZ(8),DPDS(8),DPDT(8),DET,S,T,DXDS,DZDS,DXDT, &
      DZDT,S0(8),T0(8),KY,FRQ,KY1,WMU,OMEGA,SIGR,MU,EPS,CALF, &
      CTAU,CFRQ,SHP(8),GS(3),GT(3),WG(3),WEIGHT,GEOELEC_PAR(NXZ,6), &
      EX(NTXQ,NRX,NPN),EY(NTXQ,NRX,NPN),EZ(NTXQ,NRX,NPN)
 COMPLEX SIG,CC,CWMU,DEN,PP,QQ,SS,CKY,KY2,SFAC,EX_TP,EX_TA,EY_TP,EY_TA,EZ_TP, &
         EZ_TA,EY_SP,EY_SA,HY_SP,HY_SA,EX_PP,EX_PA,EY_PP,EY_PA,EZ_PP,EZ_PA, &
         DOT_E(8),INT_E,INT1,RR(NTXQ,2*NPN),SKY(NKY,NFRQ,NSTATQ,NCOMP,NPAR)
 DATA S0/-1., 0.,+1.,+1.,+1., 0.,-1.,-1./
 DATA T0/+1.,+1.,+1., 0.,-1.,-1.,-1., 0./
 DATA GS(1),GS(2),GS(3)/-0.774596669241483,0.,+0.774596669241483/
 DATA GT(1),GT(2),GT(3)/+0.774596669241483,0.,-0.774596669241483/
 DATA WG(1),WG(2),WG(3)/0.555555555555555,0.888888888888888,0.555555555555555/

 IF (TDFD < 2) KTX = 1
 IF (TDFD == 2) KTX = JF

 OMEGA = 2.*PI*FRQ

 ! Correct EY and HY values for nodes at the air-earth interface.

 CALL SURFACE_EYHY

 ! Compute KY sensitivities for each element.

 I_LOOP: DO I1 = 3, NX-3
   J_LOOP: DO J1 = NAIR+1, NZ-3

     JC1 = (I1-1)*(NZ-1) + J1              ! Total element number.
     JC2 = (I1-3)*(NZ-NAIR-3) + (J1-NAIR)  ! Inversion domain element number.

     SIGR = 1./GEOELEC_PAR(JC1,1)
     MU   = MU0 * GEOELEC_PAR(JC1,2)
     EPS  = EPS0 * GEOELEC_PAR(JC1,3)
     CALF = 1. - GEOELEC_PAR(JC1,4)
     CTAU = GEOELEC_PAR(JC1,5)
     CFRQ = GEOELEC_PAR(JC1,6)

     SIG   = CMPLX(SIGR,0.)
     CC    = (CI * OMEGA * CTAU)**CFRQ
     SIG   = SIGR * (ONE + CC) / (ONE + CALF*CC)
     SIG   = SIG + CI * OMEGA * EPS
     WMU   = OMEGA * MU
     CWMU  = CI * WMU
     SFAC  = CWMU / (4.*PI)

     CKY = CI * CMPLX(KY,0.)
     KY1 = KY*KY
     KY2 = CMPLX(KY1,0.)
     DEN = KY2 + CWMU*SIG
     PP  = CWMU / DEN
     QQ  = CKY / DEN
     SS  = CWMU * SIGR / DEN

     TX_LOOP: DO JS = 1, NSTATQ
       RX_LOOP: DO JCOMP = 1, NCOMP

         JTX = JS + JCOMP*NSTATQ     ! Adjoint transmitter index.

         ! Compute dot product of primal and adjoint electric fields at node points.
         ! This is the most accurate position to compute the derivatives and fields.

         DOT_E = (0.,0.)
         NODE_LOOP: DO IG = 1, 8

           S = S0(IG) ; T = T0(IG)

           SHP(1) = .25 * (1.+S0(1)*S)*(1.+T0(1)*T)*(S0(1)*S+T0(1)*T-1.)
           SHP(3) = .25 * (1.+S0(3)*S)*(1.+T0(3)*T)*(S0(3)*S+T0(3)*T-1.)
           SHP(5) = .25 * (1.+S0(5)*S)*(1.+T0(5)*T)*(S0(5)*S+T0(5)*T-1.)
           SHP(7) = .25 * (1.+S0(7)*S)*(1.+T0(7)*T)*(S0(7)*S+T0(7)*T-1.)
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
             DXDS = DXDS + DPDS(I) * COORD(ABS(LNODS(JC1,I)),1)
             DZDS = DZDS - DPDS(I) * COORD(ABS(LNODS(JC1,I)),2)
             DXDT = DXDT + DPDT(I) * COORD(ABS(LNODS(JC1,I)),1)
             DZDT = DZDT - DPDT(I) * COORD(ABS(LNODS(JC1,I)),2)
           END DO

           DET = DXDS*DZDT - DZDS*DXDT  ! Element determinant.

           DPDX(1:8) = ( DZDT*DPDS(1:8) - DZDS*DPDT(1:8)) / DET
           DPDZ(1:8) = (-DXDT*DPDS(1:8) + DXDS*DPDT(1:8)) / DET

           EX_TP = (0.,0.) ; EY_TP = (0.,0.) ; EZ_TP = (0.,0.)
           EX_TA = (0.,0.) ; EY_TA = (0.,0.) ; EZ_TA = (0.,0.)

           DO I = 1, 8

             NPOS = ABS(LNODS(JC1,I))

             ! Total primal fields (TP) in Kx domain.

             EY_SP = RR(JS,2*NPOS)
             HY_SP = RR(JS,2*NPOS-1)

             EX_PP = CMPLX(0.,EX(JS,KTX,NPOS))
             EY_PP = CMPLX(EY(JS,KTX,NPOS),0.)
             EZ_PP = CMPLX(0.,EZ(JS,KTX,NPOS))

             EX_TP = EX_TP - QQ*DPDX(I)*EY_SP - PP*DPDZ(I)*HY_SP - SHP(I)*(SS-ONE)*EX_PP
             EY_TP = EY_TP + SHP(I)*(EY_SP+EY_PP)
             EZ_TP = EZ_TP - QQ*DPDZ(I)*EY_SP + PP*DPDX(I)*HY_SP - SHP(I)*(SS-ONE)*EZ_PP

             ! Total adjoint fields (TA) in Kx domain.

             EY_SA = RR(JTX,2*NPOS)
             HY_SA = RR(JTX,2*NPOS-1)

             EX_PA = CMPLX(0.,EX(JTX,KTX,NPOS))
             EY_PA = CMPLX(EY(JTX,KTX,NPOS),0.)
             EZ_PA = CMPLX(0.,EZ(JTX,KTX,NPOS))

             EX_TA = EX_TA - QQ*DPDX(I)*EY_SA - PP*DPDZ(I)*HY_SA - SHP(I)*(SS-ONE)*EX_PA
             EY_TA = EY_TA + SHP(I)*(EY_SA+EY_PA)
             EZ_TA = EZ_TA - QQ*DPDZ(I)*EY_SA + PP*DPDX(I)*HY_SA - SHP(I)*(SS-ONE)*EZ_PA

           END DO

           EX_TP = SFAC * EX_TP ; EY_TP = SFAC * EY_TP ; EZ_TP = SFAC * EZ_TP
           EX_TA = SFAC * EX_TA ; EY_TA = SFAC * EY_TA ; EZ_TA = SFAC * EZ_TA

           DOT_E(IG) = ((-1.,0.)/CWMU) * (-EX_TP*EX_TA + EY_TP*EY_TA - EZ_TP*EZ_TA)

         END DO NODE_LOOP

         ! Compute the area integral of the electric field dot product. This is
         ! done using a nine point Gaussian integration method.

         INT_E = (0.,0.)
         GAUSS_LOOP1: DO JG = 1, 3
           GAUSS_LOOP2: DO IG = 1, 3

             S = GS(IG) ; T = GT(JG) ; WEIGHT = WG(IG) * WG(JG)

             SHP(1) = .25 * (1.+S0(1)*S)*(1.+T0(1)*T)*(S0(1)*S+T0(1)*T-1.)
             SHP(3) = .25 * (1.+S0(3)*S)*(1.+T0(3)*T)*(S0(3)*S+T0(3)*T-1.)
             SHP(5) = .25 * (1.+S0(5)*S)*(1.+T0(5)*T)*(S0(5)*S+T0(5)*T-1.)
             SHP(7) = .25 * (1.+S0(7)*S)*(1.+T0(7)*T)*(S0(7)*S+T0(7)*T-1.)
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
               DXDS = DXDS + DPDS(I) * COORD(ABS(LNODS(JC1,I)),1)
               DZDS = DZDS - DPDS(I) * COORD(ABS(LNODS(JC1,I)),2)
               DXDT = DXDT + DPDT(I) * COORD(ABS(LNODS(JC1,I)),1)
               DZDT = DZDT - DPDT(I) * COORD(ABS(LNODS(JC1,I)),2)
             END DO

             DET = DXDS*DZDT - DZDS*DXDT  ! Element determinant.

             INT1 = (0.,0.)
             DO I = 1, 8
               INT1 = INT1 + SHP(I)*DOT_E(I)
             END DO
             INT_E = INT_E + WEIGHT*DET*INT1

           END DO GAUSS_LOOP2
         END DO GAUSS_LOOP1

         SKY(JKY,JF,JS,JCOMP,JC2) = INT_E

       END DO RX_LOOP
     END DO TX_LOOP
   END DO J_LOOP
 END DO I_LOOP

 DO I = 1, NXZ
   LNODS(I,1:8) = ABS(LNODS(I,1:8))
 END DO

 CONTAINS

   SUBROUTINE SURFACE_EYHY

   ! Compute the Ex and Hx fields at the air-earth interface using an
   ! irregular four point (cubic) Lagrangian interpolation polynomial.

   INTEGER JN,NR1,NR2,NR3,NR4,NR,E1,E2,E3,E4,E,NPOS1,NPOS2,NPOS3,NPOS4,NPOS, &
           NJ,NN(8)
   REAL Z1,Z2,Z3,Z4,Z,A1,A2,A3,A4,EY1R,EY1I,EY2R,EY2I,EY3R,EY3I,EY4R,EY4I, &
        EYI,EYR,HY1R,HY1I,HY2R,HY2I,HY3R,HY3I,HY4R,HY4I,HYR,HYI
   DATA NN/1,2,3,4,5,6,7,8/

   DO JTX = 1, NTXQ
     DO I1 = 3, NX-3

       NR1 = NAIR - 4   ! Row of elements three above the air-earth interface.
       NR2 = NAIR - 3   ! Row of elements two above the air-earth interface.
       NR  = NAIR + 1   ! Row of elements at air-earth interface.
       NR3 = NAIR + 3   ! Row of elements two below the air-earth interface.
       NR4 = NAIR + 4   ! Row of elements three below the air-earth interface.

       E1 = (I1-1)*(NZ-1) + NR1
       E2 = (I1-1)*(NZ-1) + NR2
       E  = (I1-1)*(NZ-1) + NR
       E3 = (I1-1)*(NZ-1) + NR3
       E4 = (I1-1)*(NZ-1) + NR4

       DO NJ = 1, 8

         JN = NN(NJ)

         NPOS1 = ABS(LNODS(E1,JN)) ; Z1 = COORD(NPOS1,2)
         NPOS2 = ABS(LNODS(E2,JN)) ; Z2 = COORD(NPOS2,2)
         NPOS  = ABS(LNODS(E, JN)) ; Z  = COORD(NPOS, 2)
         NPOS3 = ABS(LNODS(E3,JN)) ; Z3 = COORD(NPOS3,2)
         NPOS4 = ABS(LNODS(E4,JN)) ; Z4 = COORD(NPOS4,2)

         A1 = ((Z-Z2)*(Z-Z3)*(Z-Z4)) / ((Z1-Z2)*(Z1-Z3)*(Z1-Z4))
         A2 = ((Z-Z1)*(Z-Z3)*(Z-Z4)) / ((Z2-Z1)*(Z2-Z3)*(Z2-Z4))
         A3 = ((Z-Z1)*(Z-Z2)*(Z-Z4)) / ((Z3-Z1)*(Z3-Z2)*(Z3-Z4))
         A4 = ((Z-Z1)*(Z-Z2)*(Z-Z3)) / ((Z4-Z1)*(Z4-Z2)*(Z4-Z3))

         EY1R = REAL(RR(JTX,2*NPOS1)) ; EY1I = AIMAG(RR(JTX,2*NPOS1))
         EY2R = REAL(RR(JTX,2*NPOS2)) ; EY2I = AIMAG(RR(JTX,2*NPOS2))
         EY3R = REAL(RR(JTX,2*NPOS3)) ; EY3I = AIMAG(RR(JTX,2*NPOS3))
         EY4R = REAL(RR(JTX,2*NPOS4)) ; EY4I = AIMAG(RR(JTX,2*NPOS4))

         HY1R = REAL(RR(JTX,2*NPOS1-1)) ; HY1I = AIMAG(RR(JTX,2*NPOS1-1))
         HY2R = REAL(RR(JTX,2*NPOS2-1)) ; HY2I = AIMAG(RR(JTX,2*NPOS2-1))
         HY3R = REAL(RR(JTX,2*NPOS3-1)) ; HY3I = AIMAG(RR(JTX,2*NPOS3-1))
         HY4R = REAL(RR(JTX,2*NPOS4-1)) ; HY4I = AIMAG(RR(JTX,2*NPOS4-1))

         EYR = A1*EY1R + A2*EY2R + A3*EY3R + A4*EY4R
         EYI = A1*EY1I + A2*EY2I + A3*EY3I + A4*EY4I
         HYR = A1*HY1R + A2*HY2R + A3*HY3R + A4*HY4R
         HYI = A1*HY1I + A2*HY2I + A3*HY3I + A4*HY4I

         RR(JTX,2*NPOS)   = CMPLX(EYR,EYI)
         RR(JTX,2*NPOS-1) = CMPLX(HYR,HYI)

       END DO
     END DO
   END DO

   END SUBROUTINE SURFACE_EYHY

 END SUBROUTINE SENS_KY_CONSTRUCT

!============================================================================

 SUBROUTINE HYEY (NT,ND,NPN,NXZ,LNODS,COORD,NFRQ,NKY,KY,IREL,NRX,RXQ,RYQ,RZQ, &
                  NSTATQ,TDFD)

!----------------------------------------------------------------------------
!
!**** Called by: ARJUNA_2D,ARJUNA_2DI
!**** Uses: AA_Filter_coefficients
!
!  Calculate the inverse Fourier tranform of Hx, Hy and Hz components from
!  (X,Ky,Z) domain to the (X,Y,Z) domain at the receiver positions and writes
!  them to unit ND. It then writes them to unit ND in nanoteslas.
!
!----------------------------------------------------------------------------
!
! Input:   NPN     - (2*NX-1)*(2*NZ-1) - NXZ
!          NXZ     - total number of cells in mesh  (NX-1) * (NZ-1)
!          LNODS   - arrays of element node numbers.
!          COORD   - arrays of element coordinates.
!          FREQ    - array of NFRQ frequencies
!          NKY     - numbers of kx-values
!          KY      - wave number in North direction
!          IREL    - array which identifies the mesh element (air) which
!                    contains each receiver position along the survey.
!          NRX     - number of Tx-Rx offsets (= NFRQ for FD)
!          RY      - transmitter receiver offset transverse to flight path
!          RX, RZ  - receiver horizontal coordinates and altitudes
!          SXQ, SZQ  - transmitter horizontal coordinates and altitudes
!          NSTAT   - number of transmitter stations
!
!----------------------------------------------------------------------------

 USE AA_Filter_coefficients
 IMPLICIT NONE
 REAL, PARAMETER :: PI = 3.14159265359, Y_OFFSET = 0.001
 INTEGER NT,ND,NPN,NXZ,NRX,NSTATQ,NKY,NFRQ,ITX,IKY,I,I1,I2,I8,J1,JF,LNODS(NXZ,8), &
         IREL(NRX,NSTATQ),SYMFAC,TDFD,KTX
 REAL Y1A,YS,KY(NKY),HYRR,HYRI,COORD(NPN,2),HXRS,HXIS,HYRA,HYIA,HZRS,HZIS, &
      DET,S,T,DXDS,DZDS,DXDT,DZDT,CUBVAL,CRI(16),FAC_NT,YY(NLO:NHI)
 REAL, DIMENSION(NSTATQ,NRX) :: RXQ,RYQ,RZQ
 REAL, DIMENSION(4,NKY) :: CXR,CXI,CYR,CYI,CZR,CZI
 REAL, DIMENSION(8) :: SHP,DPDX,DPDZ,DPDS,DPDT,S0,T0
 COMPLEX,ALLOCATABLE,DIMENSION(:,:,:,:) :: HYY
 DOUBLE PRECISION DELTA
 DATA S0/-1., 0.,+1.,+1.,+1., 0.,-1.,-1./
 DATA T0/+1.,+1.,+1., 0.,-1.,-1.,-1., 0./

 ALLOCATE (HYY(8,NFRQ,NKY,NSTATQ)) ; HYY = (0.,0.)

 FAC_NT = 400. * PI   ! 1.e9 * mu0 : Conversion from H to B in nT.

 ! Coordinates for cos- and sin-transform.

 DELTA = LOG (10.D0) / REAL (NDEC,8)
 DO I = NLO, NHI
   YY(I) = REAL(EXP(I*DELTA))
 END DO

 ! Read HY(KY) values from scratch file.

 REWIND(NT)
 DO IKY = 1, NKY
   DO JF = 1, NFRQ
     DO ITX = 1, NSTATQ
       READ (NT,*) CRI(1:16)
       DO J1 = 1,8
         HYY(J1,JF,IKY,ITX) = CMPLX (CRI(2*J1-1),CRI(2*J1))
       END DO
     END DO
   END DO
 END DO

 ! Compute HX, HY and HZ at reciever positions from shape function
 ! interpolation of HY(KY).

 FREQUENCY_LOOP: DO JF = 1,NFRQ
   IF (TDFD < 2) KTX = 1
   IF (TDFD == 2) KTX = JF
   TX_LOOP: DO ITX = 1, NSTATQ

     SYMFAC = -1
     Y1A = ABS (RYQ(ITX,KTX))
     IF (Y1A < 0.) SYMFAC = 1
     Y1A = MAX (Y1A,Y_OFFSET)

     I1  = LNODS(IREL(KTX,ITX),1)
     I2  = LNODS(IREL(KTX,ITX),2)
     I8  = LNODS(IREL(KTX,ITX),8)

     S =  (RXQ(ITX,KTX) - COORD(I2,1)) / ABS(COORD(I2,1) - COORD(I1,1))
     T =  (RZQ(ITX,KTX) + COORD(I8,2)) / ABS(COORD(I1,2) - COORD(I8,2))

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
       DXDS = DXDS + DPDS(I) * COORD(LNODS(IREL(KTX,ITX),I),1)
       DXDT = DXDT + DPDT(I) * COORD(LNODS(IREL(KTX,ITX),I),1)
       DZDS = DZDS + DPDS(I) * COORD(LNODS(IREL(KTX,ITX),I),2)
       DZDT = DZDT + DPDT(I) * COORD(LNODS(IREL(KTX,ITX),I),2)
     END DO

     DET = DXDS*DZDT - DZDS*DXDT

     DPDX(1:8) = ( DZDT*DPDS(1:8) - DZDS*DPDT(1:8)) / DET
     DPDZ(1:8) = (-DXDT*DPDS(1:8) + DXDS*DPDT(1:8)) / DET

     CXR = 0. ; CXI = 0. ; CYR = 0. ; CYI = 0. ; CZR = 0. ; CZI = 0.

     DO IKY = 1, NKY
       DO I = 1, 8

         HYRR = REAL (HYY(I,JF,IKY,ITX))
         HYRI = AIMAG(HYY(I,JF,IKY,ITX))

         ! The derivatives introduce a factor of i.

         CXR(1,IKY) = CXR(1,IKY) + DPDX(I) * HYRI / KY(IKY)
         CXI(1,IKY) = CXI(1,IKY) - DPDX(I) * HYRR / KY(IKY)
         CYR(1,IKY) = CYR(1,IKY) + SHP(I) * HYRR
         CYI(1,IKY) = CYI(1,IKY) + SHP(I) * HYRI
         CZR(1,IKY) = CZR(1,IKY) + DPDZ(I) * HYRI / KY(IKY)
         CZI(1,IKY) = CZI(1,IKY) - DPDZ(I) * HYRR / KY(IKY)

       END DO
     END DO

     CALL CUBSPL (KY,CXR,NKY,1,1)
     CALL CUBSPL (KY,CXI,NKY,1,1)
     CALL CUBSPL (KY,CYR,NKY,1,1)
     CALL CUBSPL (KY,CYI,NKY,1,1)
     CALL CUBSPL (KY,CZR,NKY,1,1)
     CALL CUBSPL (KY,CZI,NKY,1,1)

     ! The inverse Fourier transforms are done on the basis of
     ! inverse cosine transforms for the parts which are transverse
     ! symmetric and inverse sine transforms for the parts which are
     ! transverse anti-symetric. The sine transforms require a factor
     ! of 1/i.

     ! This version is limited to dipole transmitters in the vertical
     ! and in-line plane only. Therefore, we have the symmetries:

     !  * The in-line magnetic field, HX, is symmetric (S).
     !  * The transverse magnetic field, HY, is anti-symmetric (A).
     !  * The vertical magnetic field, HZ, is symmetric (S).

     ! Future versions without this restriction will need to compute
     ! the anti-symmetric bits of HX and HZ and the symmetric bit of HY.

     HXRS = 0. ; HXIS = 0. ; HYRA = 0. ; HYIA = 0. ; HZRS = 0. ; HZIS = 0.

     DO I = NLO, NHI
       YS = YY(I) / Y1A
       IF ( (YS >= KY(1)) .AND. (YS <= KY(NKY)) ) THEN
         HXRS = HXRS + CUBVAL(KY,CXR,NKY,YS) * WCOS(I)
         HXIS = HXIS + CUBVAL(KY,CXI,NKY,YS) * WCOS(I)
         HYRA = HYRA + CUBVAL(KY,CYI,NKY,YS) * WSIN(I)
         HYIA = HYIA - CUBVAL(KY,CYR,NKY,YS) * WSIN(I)
         HZRS = HZRS + CUBVAL(KY,CZR,NKY,YS) * WCOS(I)
         HZIS = HZIS + CUBVAL(KY,CZI,NKY,YS) * WCOS(I)
       END IF
     END DO

     HXRS =  FAC_NT * HXRS / Y1A
     HXIS =  FAC_NT * HXIS / Y1A
     HYRA =  FAC_NT * HYRA / Y1A * SYMFAC  ! Change sign for x < 0 for
     HYIA =  FAC_NT * HYIA / Y1A * SYMFAC  ! anti-symmetric part.
     HZRS =  FAC_NT * HZRS / Y1A
     HZIS =  FAC_NT * HZIS / Y1A

     WRITE(ND,'(6E16.7)') HXRS,HXIS,HYRA,HYIA,HZRS,HZIS

   END DO TX_LOOP
 END DO FREQUENCY_LOOP

 DEALLOCATE (HYY)

 DO I = 1, NXZ
   LNODS(I,1:8) = ABS(LNODS(I,1:8))
 END DO

 END SUBROUTINE HYEY

!============================================================================

 SUBROUTINE SENS_FD_CONSTRUCT (NS,NFRQ,NSTATQ,NCOMP,NPAR,NKY,KY,NRX,RYQ,TDFD,SKY)

!----------------------------------------------------------------------------
!
!**** Called by: ARJUNA_2D_INV
!**** Uses: AA_Filter_coefficients
!
! Transform sensitivities from KY domain to Y domain.
!
!----------------------------------------------------------------------------

 USE AA_Filter_coefficients
 IMPLICIT NONE
 REAL, PARAMETER :: PI = 3.14159265359, Y_OFFSET = 0.001
 INTEGER NS,NPAR,NFRQ,NKY,NSTATQ,NCOMP,NRX,JF,JC,JP,JS,I,TDFD,KTX,JKY
 REAL FAC_NT,KY(NKY),Y1A,RYQ(NSTATQ,NRX),TR(4,NKY),TI(4,NKY),YY(NLO:NHI), &
      CUBVAL,YS,S_YR,S_YI
 COMPLEX SKY(NKY,NFRQ,NSTATQ,NCOMP,NPAR),SFD
 DOUBLE PRECISION DELTA

 WRITE(*,1)

 FAC_NT = 400. * PI ! 1.e9 * mu0 : Conversion from H to B in nT.

! Coordinates for cos transform.

 DELTA = LOG (10.D0) / REAL(NDEC,8)
 DO I = NLO, NHI
   YY(I) = REAL(EXP(I*DELTA))
 END DO

 DO JF = 1, NFRQ
   IF (TDFD < 2) KTX = 1
   IF (TDFD == 2) KTX = JF
   DO JS = 1, NSTATQ
     Y1A = ABS(RYQ(JS,KTX))
     Y1A = MAX(Y1A,Y_OFFSET)   ! Tx-Rx offset.
     DO JC = 1, NCOMP
       DO JP = 1, NPAR

         ! Spline the KY sensitivities.

         TR = 0. ; TI = 0.
         DO JKY = 1, NKY
           TR(1,JKY) = REAL (SKY(JKY,JF,JS,JC,JP))
           TI(1,JKY) = AIMAG(SKY(JKY,JF,JS,JC,JP))
         END DO
         CALL CUBSPL(KY,TR,NKY,1,1)
         CALL CUBSPL(KY,TI,NKY,1,1)

         ! Cosine transform the KY sensitivities to Y domain.

         S_YR = 0. ; S_YI = 0.
         DO I = NLO, NHI
           YS = YY(I) / Y1A
           IF ( (YS >= KY(1)) .AND. (YS <= KY(NKY)) ) THEN
             S_YR = S_YR + CUBVAL(KY,TR,NKY,YS) * WCOS(I)
             S_YI = S_YI + CUBVAL(KY,TI,NKY,YS) * WCOS(I)
           END IF
         END DO
         S_YR = FAC_NT * S_YR / Y1A
         S_YI = FAC_NT * S_YI / Y1A

         SFD = CMPLX(S_YR,S_YI)

         WRITE(NS,2) SFD

       END DO
     END DO
   END DO
 END DO

 1 FORMAT(//T3,'Constructing the Jacobian matrix.')
 2 FORMAT(2E16.7)

 END SUBROUTINE SENS_FD_CONSTRUCT

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

!===========================================================================!
!                                                                           !
!                        ArjunAir Reference Structure                       !
!                                                                           !
!===========================================================================!
!
!
!       1  MAIN   ARJUNA_2D     FRONT  EHFLD
!       2                              SENS_KY_CONSTRUCT
!       3                       FWD_TX_INC_POS
!       4                       HYEY   CUBSPL
!       5                              CUBVAL INTERV
!       6                       SENS_FD_CONSTRUCT    CUBSPL
!       7                                            CUBVAL > 5
!       8                       SHAPEF
!       9         CPU_TIME
!      10         DCPRM_FD
!      11         DCPRM_TD
!      12         FDREAD
!      13         NLSQ   ESVD   DPROD1
!      14                NOISE_2_SIGR
!      15                RESJAC ARJUNA_2D     > 1
!      16                       FDREAD
!      17                       FD_FIELD      SET_DATA_FD   CUBSPL
!      18                                                   CUBVAL > 5
!      19                       FD_SENS       SET_SENS_FD   CUBSPL
!      20                                                   CUBVAL > 5
!      21                       SFDREAD
!      22                       TD_FIELD      SET_DATA_TD   CUBSPL
!      23                                                   CUBVAL > 5
!      24                                     TDEM_3D       > 1.1
!      25                       TD_SENS       SENS_FD2TD    > 2.1
!      26                                     SET_SENS_TD   CUBSPL
!      27                                                   CUBVAL > 5
!      28                SET_INV_DATA  CUBSPL
!      29                              CUBVAL > 5
!      30                SOLVE_OVER
!      31                SOLVE_UNDER
!      32                SV_ANALYSIS_OVER
!      33                SV_ANALYSIS_UNDER
!      34                WRITE_INVMDL
!      35                WRITE_MISFIT
!      36         AA_Input_routines%READ_INVRT_CNTRL > 3.1
!      37         AA_Input_routines%READ_INVRT_DATA  > 4.1
!      38         AA_Input_routines%READ_MODEL_DATA  > 5.1
!      39         AA_Input_routines%READ_SYSTEM_AND_SURVEY  > 6.1
!      40         AA_Input_routines%SET_FRQ
!      41         SET_MESH_COMPLETION
!      42         SET_NORM_TD
!      43         SET_SOURCE
!      44         SET_SURVEY_1
!      45         SET_SURVEY_2  CUBSPL
!      46                       CUBVAL > 5
!      47         AA_Input_routines%SET_TRP
!      48         SET_TXCLN     CUBSPL
!      49                       CUBVAL > 5
!      50         TDEM_3D       > 1.1
!      51         WRITE_FD      SET_DATA_FD   > 17
!      52                       WRSLV_FD      WRSLVS_FD
!      53         WRITE_LOG_FILE
!      54         AA_Input_routines%WRITE_np_HEADER
!      55         AA_Input_routines%WRITE_np_SYS_DATA
!      56         WRITE_TD      SET_DATA_TD   > 22
!      57                       WRSLV  WRSLVP
!
!
!     1.1 >COSTRN CUBVAL > 5
!     1.2  CUBSPL
!     1.3  FOLD_AND_CONVOLVE    CUBDER INTERV
!     1.4                       CUBSPL
!     1.5                       CUBVAL > 5
!     1.6                       TQSTRIP       CUBSPL
!     1.7                                     TXCNVD CUBINT INTERV
!     1.8                                            CUBSPL
!     1.9                                            CUBVAL > 5
!    1.10                                            LINVAL INTERV
!    1.11                                            TXCMRG
!    1.12                                     TXCNVL CUBINT > 1.7
!    1.13                                            CUBVAL > 5
!    1.14                       TXCNVD > 1.7
!    1.15                       TXCNVL > 1.12
!
!
!     2.1 >COSTRN > 1.1
!     2.2  CUBSPL
!     2.3  FOLD_AND_CONVOLVE    > 1.3
!
!
!     3.1 >WRITE_LOG_FILE
!
!
!     4.1 >WRITE_LOG_FILE
!
!
!     5.1 >AA_Input_routines%LITHMAP
!     5.2  WRITE_LOG_FILE
!
!
!     6.1 >CONFIG_ID
!     6.2  WRITE_LOG_FILE
!
!
!===========================================================================!
!                                                                           !
!                           ArjunAir Release History                        !
!                                                                           !
!===========================================================================!
!
!
!    Pre P223F Releases
!    ---------------------
!    Version 5.0.1    03 August    2004 (P223E Final)
!    Version 4.8.2    12 February  2003
!    Version 4.8.1    29 November  2002
!    Version 4.6.0    23 July      2002
!    Version 4.4.1    11 February  2002
!    Version 4.3      23 August    2001
!    Version 4.1       ? December  2000
!    Version 4.0       ? July      2000
!    Version 3.2.1P    ? August    1998
!
!
!    Previous releases in FORTRAN 77
!    -------------------------------
!    VERSION 3.1       ? June      1998
!    VERSION 2.0       ? September 1996
!    VERSION 1.0       ? ?         199?
!
!
!    CHANGES: 6.2.8 from 6.1.2
!    -----------------------
!
!    1. Inversion features now enabled for frequency-domain systems.
!       Time-domain inversion has been disabled.
!
!    2. Correction to the number of frequencies required for time-domain
!       transformation.
!
!    3. The definitions for KCMP and ORDER have changed for frequency-domain
!       inversion to allow ArjunAir.inv files to be displayed more easily.
!
!    4. For option, SURVEY = 3: Specification of variable aircraft PITCH
!       has been replaced by specification of variable transmitter
!       inclination, TXCLN.
!
!
!    CHANGES: 6.1.2 from 6.1.1
!    -------------------------
!
!    1. Separate INVPRT options for ArjunAir.out and ArjunAir.mv1 (RECORD 16; for
!       when the inversion feature is enabled).
!
!    2. The option to invert for AEM system geometry has been removed. Rather
!       than making inversion for AEM system geometry a feature in each P223F
!       program, a separate program 'Raven' will be introduced at a later date.
!
!    3. Frequency-domain stations are now defined by Tx-Rx midpoints, not Tx
!       positions.
!
!    4. For inversion, the constraint scheme was modified to offer three options
!       for each constrained parameter. Elasticities are always positive (RECORD
!       16.2).
!
!         * Fixed      CTYPE = 1  => Neither elasticity nor bounds are required.
!         * Restraint  CTYPE = 2  => Elasticity (valued 0 to 1) is specified but
!                                    no bounds are required.
!         * Buffered   CTYPE = 3  => Elasticity (valued 0 to 1) and upper and lower
!                                    bounds are required.
!
!    5. Additional convergence criterion: After the 10th iteration, the inversion
!       will stop if two successive iterations lower the RMS error by less than
!       1%. It is best to set MAXITS, the maximum number of iterations, to 90 so as
!       to not unduly restrict the inversion. It rare for ITS, the actual number
!       of iterations, to exceed 20 before convergence of some description is
!       achieved.
!
!
!    CHANGES: 6.1.1 from 6.1
!    -----------------------
!
!    1. A bug in the time-domain re-start option has been corrected.
!
!
!    CHANGES: 6.1 from 6.0
!    ---------------------
!
!    1. The Auto Survey option has been re-installed for forward modelling.
!       Whilst adapting AjurnAir to the new input format, this option was
!       neglected. Please refer to the release notes at RECORD 9 for detailed
!       descriptions of how to enable the Auto Survey option.
!
!
!    CHANGES: 6.0 from 5.0.1   Input and Output formats have changed!
!    -----------------------   ======================================
!
!
!    This release is for forward modelling ONLY. Inversion has NOT been
!    enabled but full descriptions are provided of inversion inputs and
!    outputs.
!
!
!    Vertical co-planar broadside arrays can NOT be modelled using ArjunAir.
!
!
!    1. Revised input format both for inversion and forward modelling as
!       of the P223F goal to build a common inversion structure for Airbeo,
!       LeroiAir, ArjunAir and SamAir.
!
!       Maxwell will be modified to accommodate the new input.
!
!       Control files created using EMGui have to be modified with minor
!       text editing in order to be valid.
!
!       System, model and inversion controls ae read from ArjunAir.cfl.
!       Inversion data and data format are read from ArjunAir.inv.
!       Forward modelling results for plotting are contained in ArjunAir.mf1.
!       Inversion results for plotting are contained in ArjunAir.mv1.
!
!       The new *.mf1 (forward models results) and *.mv1 (inversion results)
!       plotting files are designed for both a new version of Maxwell and
!       for easy importation into Geosoft.
!
!    2. Data weights are now restricted to be 0 or 1 (accept or reject).
!       The use of weights to balance magnitudes is unnecessary because
!       error computation is performed using scaled quantities. Weights
!       based on standard deviation is recommended on a theoretical basis
!       but in practice, this leads to unphysical bias.
!
!    3. Unit specification for magnetic field and dB/dt is now required.
!       The options are: nT, nT/s, pT, pT/s, fT & fT/s.
!
!    4. Unit specification for normalisation is now required. The options
!       are: ppm, ppb, ppt & pct
!
!    5. Line numbers for data to be inverted are now required. Line numbers
!       for forward modelling are optional
!
!    6. Additional inversion convergence tests based on RMS error reduction.
!
!==============================================================================
