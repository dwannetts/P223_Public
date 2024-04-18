import numpy as np
import pandas as pd
from itertools import product
from typing import List
from machine_prospector.physics.tdem.response import TDEMResponse
from SimPEG.electromagnetics.time_domain.receivers import PointMagneticFluxDensity, PointMagneticFluxTimeDerivative
from leroi_constants import (STANDARD_PROCESSING, TIME_DOMAIN_OR_FREQ_DOMAIN, TEST_MODE,
                             PRINT_RESPONSE_MODE, MODELING_MODE, RECEIVER_CHANNEL_READ_TIMES,
                             PLOT_RESPONSE_AT_RECEIVER_LOCATION)

class LeroiLithologyLayers:
    def __init__(self, layer_resistivities: List, layer_conductances: List, layer_relative_magnetic_permeabilities: List,
                 layer_relative_dielectric_constants: List, layer_chargeabilities: List, layer_time_constants: List,
                 layer_freq_constants: List, layer_thicknesses_m: List):
        """
        Default values:  RMU = 1   REPS = 1   CHRG = 0   CTAU = 0   CFREQ = 1

        :param layer_resistivities: resistivity in each layer
        :param layer_conductances: conductance in each layer (T)
        :param layer_relative_magnetic_permeabilities: relative layer magnetic permeability
        :param layer_relative_dielectric_constants: relative layer dielectric constant (permittivity
        :param layer_chargeabilities: Cole-Cole layer chargeability
        :param layer_time_constants: Cole-Cole layer time constant
        :param layer_freq_constants: Cole-Cole layer frequency constant
        :param layer_thicknesses_m: layer thickness in meters
        """
        # ++++++++++++++++++++++++++++++++++++++++++++
        # Lithology layer inputs (one of the layers is the plate with -1.0 resistivity,
        # all non-plate layers will have -1 conductance)
        # !       NLITH - number of layer plus plate lithologies.  Any number of
        # !               lithologies may be defined.  Be careful not to use
        # !               layer lithologies for plates and vice versa
        # TODO assert all lithology layer inputs have the same length (number of layers)
        # !      NLAYER - number of layers including plate and basement.
        self.n_layers = len(layer_resistivities)

        self.lithology_properties_df = pd.DataFrame({'layer_index': np.range(self.n_layers),
                                                     'resistivity': layer_resistivities,
                                                     'conductance_T': layer_conductances,
                                                     'relative_magnetic_permeability': layer_relative_magnetic_permeabilities,
                                                     'relative_dielectric_constant': layer_relative_dielectric_constants,
                                                     'chargeability': layer_chargeabilities,
                                                     'time_constant': layer_time_constants,
                                                     'layer_freq_constant': layer_freq_constants,
                                                     'layer_thicknesses': layer_thicknesses_m})

        self.lithologies = self.lithology_properties_df[['resistivity', 'conductance_T',
                                               'relative_magnetic_permeability', 'relative_dielectric_constant',
                                               'chargeability', 'time_constant', 'layer_freq_constant']].values
        # last one is basement lithology
        # example of layers and thicknesses [[3, 25.0], [5, " "]]
        self.layers_to_use_and_their_thicknesses = [[idx, thickness] for idx, thickness in enumerate(layer_thicknesses_m)]


# PlateGeometry takes a center [x, y, z], axes (halfwidth in the x and y directions prior to any rotation), and rotation
class LeroiSubsurface:
    """ The fortran code comments describing the lithology layers and the plate are below:
    --------------------------------------------------------------------------------------

    """
    def __init__(self, tdemsurvey_object: TDEMResponse, kobold_plates: PlateGeometry,
                 leroi_lithology: LeroiLithologyLayers,
                 cell_width_m: float, loop_or_dipole_source: int
                 ):
        """
        :param tdemsurvey_object: TDEMResponse.survey object
        :param kobold_plates: list of PlateGeometry object(s)
        :param leroi_lithology: lithology layers as described by LeroiLithology
        :param cell_width_m:  dimension of target cells for all plates
        :param loop_or_dipole_source: =1 if using a general loop - vertex locations will be specified
                                      =3 if using a magnetic dipole - location & orientation will be specified
        """
        self.survey = tdemsurvey_object.survey
        self.leroi_lithology = leroi_lithology
        # ++++++++++++++++++++++++++++++++++++++++++
        # likely non-changing inputs
        self.time_domain_or_freq_domain = TIME_DOMAIN_OR_FREQ_DOMAIN
        self.modeling_mode = MODELING_MODE
        self.standard_processing = STANDARD_PROCESSING
        self.test_mode = TEST_MODE
        self.print_response_mode = PRINT_RESPONSE_MODE
        self.receiver_channel_read_times = RECEIVER_CHANNEL_READ_TIMES
        self.plot_response_at_receiver_location = PLOT_RESPONSE_AT_RECEIVER_LOCATION


        self.source_type = loop_or_dipole_source

        # +++++++++++++++++++++++++++++++++++++++++++
        # plate attributes
        self.plate_data = [self.kobold_plate_geometry_to_leroi_plate_geometry(kobold_plate) for kobold_plate in enumerate(kobold_plates)]
        # Example plate data below
        #             JP, LITHP,  YCNTRD, XCNTRD,  PLTOP, PLNGTH, PLWDTH, DZM, DIP, PLG
        # plate_data = [[2, 0.00, 0.00, -50.00, 400.0, 200.0, 90.0, 45.0, 0.0]]

        # !      NPLATE - number of thin plates
        self.n_plate = len(kobold_plates)
        # !        CELLW - dimension of target cells for all plates.
        # !
        # !   In many cases, CELLW = 25 is a good compromise between accuracy and speed.
        # !
        # !   LEROI will divide each target plate into a minimum of 2 cells
        # !   along in each direction.  If a plate is 525 m. long and 100 m. down dip
        # !   and CELLW has been set to 100 m., then the program
        # !   will model the target as 6 by 2 cells, each of dimension 87.5 by 50 m.
        # !
        # !   The input value for CELLW represents a trade-off between speed and accuracy.
        # !   Decreasing CELLW can substantially increase run times but will produce
        # !   better accuracy.  When the plate is very large, CELLW = 25 may result in
        # !   unacceptable runtimes and CELLW = 40 may give fast and reasonable results,
        # !   In other cases, especially for strongly interacting multiple plates, a finer
        # !   discretisation (eg; CELLW = 20) may be required for sufficient accuracy,
        self.cell_width_m = cell_width_m

        # ++++++++++++++++++++++++++++++++++++++++++++++++++++
        # survey attributes
        # dB_dt_or_B is referred to as STEP in Leroi.f90 code.
        # It is set as = 0 : Compute dB/dt for all magnetic dipole receivers.
        #             = 1 : Compute B for all magnetic dipole receivers.
        # Note we are assuming all receivers in the list are of the same class
        for receiver in self.survey.source_list[0].receiver_list[0]:
            if isinstance(receiver.locations, PointMagneticFluxDensity):
                # Compute B
                self.dB_dt_or_B = 1
            elif isinstance(receiver.locations, PointMagneticFluxTimeDerivative):
                # Compute dB/dt
                self.dB_dt_or_B = 0
            else:
                raise AttributeError('the receivers must be either dB/dt or B')

        # n_receiver_channels is referred to as NCHNL - number of receiver channels in Leroi.f90
        self.n_receiver_channels = len(self.survey.time_channels)

        # receiver_off_time_ms is OFFTIME/OFFTYM in Leroi.f90
        # time (milliseconds) between end of one pulse and the start of
        # the next pulse (of opposite sign) since a bipolar waveform is
        # assumed.  For systems which have a signal which is always on,
        # OFFTIME = 0.
        self.receiver_off_time_ms = self.survey.waveform.off_time

        # Transmitter
        loop_coordinates_xyzs = self.survey.loop_coordinates.xyzs
        self.transmitter_vertices = {1: {'vertices': loop_coordinates_xyzs[:, 0:2],
                                         'elevation': loop_coordinates_xyzs[:, 2]}}
        # transmitter_on_time_and_current_amps
        # TXON(J) = digitised time (in milliseconds)
        # In most cases, TXON(1) = 0, TXON(NSX) = pulse on-time
        # TXAMP(J) = transmitter current in amps at time TXON(J)
        # TXON (ms) and Transmitter current (amps)
        loop_current = self.survey.loop_current
        # The example looked like the following
        # tx_on_ms_and_transmitter_current_amps = [[0.000, 0.000],
        #                                  [0.100, 1.000],
        #                                  [1000.000, 1.000],
        #                                  [1000.010, 0.000],
        #                                  [2000.000, 0.000]]
        self.tx_on_ms_and_transmitter_current_amps = self.survey.waveform.ramp_on  # TODO This isnt right

        # !      OFFTIME - time (milliseconds) between end of one pulse and the start of
        # !                the next pulse (of opposite sign) since a bipolar waveform is
        # !                assumed.  For systems which have a signal which is always on,
        # !                OFFTIME = 0.
        # note: this is also referred to as OFFTYM in the f90 code
        self.receiver_off_time_ms = self.survey.waveform.off_time
        # !      REFTYM - Time (in ms) from which TMS or TOPN & TCLS are measured.  For
        # !               example, this could be signal off-time or start of downward ramp.
        # !        TOPN - time at which receiver channel I opens.
        # !        TCLS - time at which receiver channel I closes.
        self.receiver_channel_open_close_time_ms = 0.000

        # !      TXON(J) = digitised time (in milliseconds)
        # !                In most cases, TXON(1) = 0, TXON(NSX) = pulse on-time
        # !
        # !      TXAMP(J) = transmitter current in amps at time TXON(J)

        # TODO below is not complete, these attributes need to be read from the TDEMResponse survey
        # TXON (ms) and Transmitter current (amps)
        self.tx_on_ms_and_transmitter_current_amps = [[0.000, 0.000],
                                                 [0.100, 1.000],
                                                 [1000.000, 1.000],
                                                 [1000.010, 0.000],
                                                 [2000.000, 0.000]]
        # !      NSX =  number of points needed to describe 1/2 cycle of the transmitter
        # !             waveform.  A bipolar waveform is assumed.  Thus for a system
        # !             like Sirotem or EM37, NSX = 4, one point each for the start
        # !             and end of the two ramps.
        # !             For an ideal step turnoff system set NSX = 1
        self.n_points_for_half_cycle = len(self.tx_on_ms_and_transmitter_current_amps)

        #        Receiver channel origin INPUT is shifted by    0.000 ms from signal origin.
        #
        #          Receiver Window Specifications (ms - referenced to signal origin)
        #          ----------------------------------------------------------------

        self.receiver_windows = [f'{1000 + i + 0.01}  {i + 1 + 1000.01}' for i in range(50)]

        # !      NLINES - number of lines of data to be modelled or inverted.
        # !               For this option, a line of data consists of specifying a
        # !               single transmitter plus a line of receivers.
        # !
        # !               Different RECEIVER types are allowed for different lines
        # !               but ALL receivers in any line must be of the same type:
        # !               magnetic dipoles, electric dipoles or rectangular loops.
        # !
        # !               The same SOURCE type must be used for all lines in the
        # !               modelling or inversion project
        self.n_lines = 1
        # MRXL - maximum number of receiver positions per line.
        self.n_max_receivers_per_line = 441
        # !
        # !      NTX = number of distinct transmitter positions for source
        # !            (loop, magnetic dipole, or electric bipole or dipole)
        # !
        self.n_transmitter_positions = 1
        # !      SOURCE_TYPE = 1 : general loop    - vertex locations will be specified
        # !                  = 2 : grounded wire   - path + endpoints will be specified
        # !                  = 3 : magnetic dipole - location & orientation will be specified
        # !
        self.source_type = 1

        # !      MXVRTX - maximum number of vertices for all sources.
        # !               If SOURCE_TYPE = 3, magnetic dipole, set MXVRTX = 1
        # !
        self.n_max_source_vertices = 4
        # !        NTRN = number of turns in transmitter loop
        self.n_transmitter_turns = 1

        # transmitter elevation is 0 and it has four vertices
        # !            NTX = number of transmitters specified
        # !       NVRTX(J) = number of vertices for transmitter J
        # transmitter 1 has four vertices
        self.transmitter_vertices = {1: {'vertices': [[-500., -500.], [-500., 500.], [500., 500.], [500., -500.]],
                                    'elevation': 0}}
        self.number_of_transmitters = len(self.transmitter_vertices)
        self.transmitter_numbers = [[len(tv['vertices']), tv['elevation']] for tv in self.transmitter_vertices.values()]
        # !
        # !               SURVEY_TYPE = 1
        # !               ---------------
        # !   LNTR(1,L) : Tx index for Line L.   LNTR(2,L) = LNTR(1,L)
        # !   LNTR(3,L) : Rx index  for start of Line L
        # !   LNTR(4,L) : Rx index for finish of Line L
        # !
        # !               SURVEY_TYPE > 1
        # !               ---------------
        # !   LNTR(1,L) : Tx index for start of Line L
        # !   LNTR(2,L) : Tx index for finish of Line L
        # !   LNTR(3,L) : Rx index for Line L.   LNTR(4,L) = LNTR(3,L)
        self.survey_type = 1
        self.line_tx = 1
        self.tx_index = 1
        # receiver type Line L - mag dipole (1); electric dipole (2)
        # !       RX_TYPE : receiver type for each line(1, 2 or 3)
        self.rx_type = 1
        # !    NRX(I)          - the number of receivers in Line I.
        self.n_receivers = 441
        # !    UNITS(I)        - units for line I
        self.units = 21
        # !    Define CMP(J), the component selection for magnetic dipole and point electric receivers.
        # !    For inversion, this will define the data components of Line J that are to be inverted.
        # !    For modelling, these will govern output for Line J.
        # !
        # !    For coincident loop or electric dipole receivers or Sampo, Leroi sets CMP = 1
        # !
        # !    In what follows, depending upon the value of IDH:
        # !
        #     CMP(J) =   1 : model or invert on X (U,N) data only for Line(J)
        # !             =   2 : model or invert on Y (V,S) data only for Line(J)
        # !             =   3 : model or invert on Z (A,W) data only for Line(J)
        # !             =  12 : model or invert on X (U,N) and Y (V,S) data for Line(J)
        # !             =  13 : model or invert on Z (A,W) and X (U,N) data for Line(J)
        # !             =  23 : model or invert on Z (A,W) and Y (V,S) data for Line(J)
        # !             = 123 : model or invert on all three Line(J) components
        # !
        self.component_selection = 123
        # !  KNORM = 0 :  BPRM (JR,JS) = 1 Tesla
        # !  KNORM = 1 :  BPRM (JR,JS) = Total DC Field
        # !  KNORM = 2 :  BPRM (JR,JS) = Vertical DC Field
        # !    KNORM(I)        - normalisation indicator for line I
        self.normalization_indicator = 0

        # !      IDH = 0 for all surface surveys.
        # !          = 1 or 2 for downhole magnetic dipole receivers only
        # !
        self.surface_or_bh = 0
        # !       SV_AZM(J) : azimuth of survey line J.  SV_AZM = 0 pointing north.
        # !                   It is positive clockwise.
        # !
        # !           For surface surveys, SV_AZM orients the X & Y components.
        # !               The X (radial) component lies along SV_AZM
        # !               The Y (transverse) component is perpendicular to SV_AZM
        # !               Z = vertical component.
        # !
        # !           If IDH = 2, SV_AZM orients the U & V components.
        # !
        self.az_survey_line = 0
        # !       RXMNT(J) - dipole receiver moment (area * turns) for Line J
        # !                  (magnetic dipole only)
        self.dipole_receiver_moment = 0.1000E+05
        # easting, northing, elevation

        self.receiver_locations = [f'{p[0]} {p[1]} 0.' for p in product(range(-1000, 1100, 100), repeat=2)]

    def kobold_plate_geometry_to_leroi_plate_geometry(self, kobold_plate):
        return np.na, np.na, np.na, np.na, np.na, np.na
        # TODO
        #returns center_x, center_y, plate_lith_row, plate_top, plate_length, plate_width


    def write_to_cfl(self):
        lines = ['Leroi version of MGF file',
                 f'{self.time_domain_or_freq_domain} {self.modeling_mode} {self.standard_processing} {self.print_response_mode} {self.test_mode}',
                 f'{self.dB_dt_or_B} {self.n_points_for_half_cycle} {self.n_receiver_channels} {self.receiver_channel_read_times} {self.receiver_channel_open_close_time_ms} {self.receiver_off_time_ms}',
                 '\n'.join([' '.join(map(str, thing)) for thing in self.tx_on_ms_and_transmitter_current_amps]),
                 '\n'.join(self.receiver_windows),
                 str(self.survey_type),
                 f'{self.n_lines} {self.n_max_receivers_per_line} {self.number_of_transmitters} {self.source_type} {self.n_max_source_vertices} {self.n_transmitter_turns}',
                 '\n'.join([' '.join(map(str, thing)) for thing in self.transmitter_numbers]),
                 '\n'.join([' '.join(map(str, thing)) for thing in self.transmitter_vertices[0]]),
                 f'{self.line_tx} {self.tx_index} {self.rx_type} {self.n_receivers} {self.units}',
                 f'{self.component_selection} {self.normalization_indicator} {self.surface_or_bh} {self.plot_response_at_receiver_location}  {self.az_survey_line} {self.dipole_receiver_moment}',
                 '\n'.join(self.receiver_locations),
                 f'{self.leroi_lithology.n_layers} {self.n_plate} {self.leroi_lithology.n_layers}',
                 '\n'.join([' '.join(map(str, thing)) for thing in self.leroi_lithology.lithologies]),
                 '\n'.join([' '.join(map(str, thing)) for thing in self.leroi_lithology.layers_to_use_and_their_thicknesses]),
                 str(self.cell_width_m),
                 '\n'.join([' '.join(map(str, this_plate[:4])) for this_plate in self.plate_data]),
                 '\n'.join([' '.join(map(str, this_plate[4:9])) for this_plate in self.plate_data]),
                 ]

        with open('filename.txt', 'w') as f:
            f.write('\n'.join(lines))
