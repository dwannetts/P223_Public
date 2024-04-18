
#      TDFD = 1 = > time - domain modelling - STANDARDOPTION
#           = 2 = > frequency - domain modelling
TIME_DOMAIN_OR_FREQ_DOMAIN = 1

#  # !      DO3D =  1, 2, 3 or 0 for modeling_mode
#     # !
#     # !      DO3D =  1  computes response of 3-D heterogeneities and prints
#     # !                 voltages as measured by receivers.
#     # !
#     # !           =  2  (time-domain only)
#     # !                 instead of computing the frequency-domain responses
#     # !                 (95-99 percent of Leroi usual computation time) use
#     # !                 previously computed frequency-domain responses contained
#     # !                 in file Leroi.frq to calculate 3-D time-domain responses.
#     # !
#     # !           =  3  Plates can be in any layer; ie not restricted to basement.
#     # !                 PLUNGE must be 0 for all plates.
#     # !                 Inversion hasn't been implemented for this geometry.
#     # !                 These restrictions are in place only because the 31-01-08
#     # !                 project deadline does not permit further work.
#     # !
#     # !           =  0  compute layered earth model only
MODELING_MODE = 1
# !      ISYS = 0 : Standard output processing
# !           = 2 : Sampo processing => ABS (Bz/Bx) = vertical / radial
# !           = 4 : Utem processing
# !
STANDARD_PROCESSING = 0

# !      PRFL = 1  prints response in profile mode.
# !                Each column contains the response for one channel
# !                (or frequency) for all stations on the profile.
# !
# !           = 11 as above but include scattered fields in .OUT file
# !
# !           = 0  prints responses in temporal or frequency mode.
# !                Each column contains the responses for one receiver
# !                position for either all channels (if TDFD = 1) or
# !                for all frequencies (if TDFD = 2).
# !
# !           = 10 as above but include scattered fields in .OUT file
# !
PRINT_RESPONSE_MODE = 1

# !     ISTOP = 0  read the input data and run the specified models.
# !           = 1  read the input data, print the model description
# !                  and STOP so that the model description can be verified.
# !                  REMEMBER to change ISTOP to 0 once the models have been
# !                  verified.
TEST_MODE = 0


# !      KRXW = 1 : receiver channels will be read in terms of start and end
# !                 times in ms relative to REFTYM.
# !
# !           = 2 : receiver channels will be read in terms of midpoints (relative
# !                  to REFTYM) and channel widths.
# !
RECEIVER_CHANNEL_READ_TIMES = 1
# !      SURVEY_TYPE = 1 : GENERAL OPTION for separate setup of transmitter and
# !                        receiver arrays.  Open and closed loops are not shape
# !                        restricted.   Inductive and galvanic sources &
# !                        receivers are permitted.
# !                        This would be the correct choice for downhole surveys
# !                        using surface loop transmitters or for CSAMT..
# !
# !                  = 2 : MOVING LOOP SURVEY with one or more magnetic dipole
# !                        receivers moving at fixed horizontal offsets with
# !                        respect to rectangular loop.
# !                        (Central loop = 1 receiver at zero offset)
# !
# !                  = 3 : SURFACE MAGNETIC DIPOLE-DIPOLE SURVEY with one or more
# !                        magnetic dipole receivers moving at fixed horizontal
# !                        offsets with respect to magnetic dipole transmitter on
# !                        or above ground
# !
# !                  = 4 : COINCIDENT LOOP SURVEY with rectangular loop
# !
# !                  = 5 : BOREHOLE MAGNETIC DIPOLE-DIPOLE SURVEY
# !                        Single magnetic dipole receiver moving downhole at
# !                        fixed offset with a magnetic dipole transmitter
# !
# NOTE: in the future we should be able to handle other types
SURVEY_TYPE = 1
# !      IPLT(J) = 1 : plot response of Line J at receiver location
# !              = 2 : plot response of Line J at transmitter-receiver midpoint
# !              = 3 : plot response of Line J at transmitter location
PLOT_RESPONSE_AT_RECEIVER_LOCATION = 1
