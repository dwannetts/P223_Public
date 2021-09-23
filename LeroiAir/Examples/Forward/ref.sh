#!/bin/sh
#
# compare current output to reference output.
#
# unless compilation switches have been changed, differences should be confined
# to the final two lines in each mf1 file where date and run time are written.

diff LAF_Dighem.mf1 LAF_Dighem.mf1.reference >> LAF_Dighem.check &
diff LAF_Gtk.mf1 LAF_Gtk.mf1.reference >> LAF_Gtk.check &
diff LAF_Vcb.mf1 LAF_Vcb.mf1.reference >> LAF_Vcb.check &
diff LAT_Aerotem.mf1 LAT_Aerotem.mf1.reference >> LAT_Aerotem.check &
diff LAT_Geotem.mf1 LAT_Geotem.mf1.reference >> LAT_Geotem.check &
diff LAT_Spectrem.mf1 LAT_Spectrem.mf1.reference >> LAT_Spectrem.check &
diff LAT_Tempest.mf1 LAT_Tempest.mf1.reference >> LAT_Tempest.check &
diff LAT_VTEM.mf1 LAT_VTEM.mf1.reference >> LAT_VTEM.check &
