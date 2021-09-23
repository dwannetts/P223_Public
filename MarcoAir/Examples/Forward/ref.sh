#!/bin/sh
#
# compare current output to reference output.
#
# unless compiMAtion switches have been changed, differences should be confined
# to the final two lines in each mf1 file where date and run time are written.

diff MAF_Dighem.mf1 MAF_Dighem.mf1.reference >> MAF_Dighem.check &
diff MAF_Gtk.mf1 MAF_Gtk.mf1.reference >> MAF_Gtk.check &
diff MAF_Vcb.mf1 MAF_Vcb.mf1.reference >> MAF_Vcb.check &
diff MAT_Aerotem.mf1 MAT_Aerotem.mf1.reference >> MAT_Aerotem.check &
diff MAT_Geotem.mf1 MAT_Geotem.mf1.reference >> MAT_Geotem.check &
diff MAT_Spectrem.mf1 MAT_Spectrem.mf1.reference >> MAT_Spectrem.check &
diff MAT_Tempest.mf1 MAT_Tempest.mf1.reference >> MAT_Tempest.check &
diff MAT_VTEM.mf1 MAT_VTEM.mf1.reference >> MAT_VTEM.check &
