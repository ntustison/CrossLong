#! /usr/bin/sh

# unnormed

antsVol -i sum.nii.gz -d unnormed_posterior3d_90x90x0.png[1.5,90x90x0,255x255x255] -f T_template0UnnormedRatios.mha
antsVol -i sum.nii.gz -d unnormed_superior3d_90x180x0.png[1.5,90x180x0,255x255x255] -f T_template0UnnormedRatios.mha
antsVol -i sum.nii.gz -d unnormed_right3d_180x0x90.png[1.5,180x0x90,255x255x255] -f T_template0UnnormedRatios.mha
antsVol -i sum.nii.gz -d unnormed_left3d_0x0x270.png[1.5,0x0x270,255x255x255] -f T_template0UnnormedRatios.mha
antsVol -i sum.nii.gz -d unnormed_frontal3d_90x270x0.png[1.5,90x270x0,255x255x255] -f T_template0UnnormedRatios.mha


# normed

antsVol -i sum.nii.gz -d normed_posterior3d_90x90x0.png[1.5,90x90x0,255x255x255] -f T_template0NormedRatios.mha
antsVol -i sum.nii.gz -d normed_superior3d_90x180x0.png[1.5,90x180x0,255x255x255] -f T_template0NormedRatios.mha
antsVol -i sum.nii.gz -d normed_right3d_180x0x90.png[1.5,180x0x90,255x255x255] -f T_template0NormedRatios.mha
antsVol -i sum.nii.gz -d normed_left3d_0x0x270.png[1.5,0x0x270,255x255x255] -f T_template0NormedRatios.mha
antsVol -i sum.nii.gz -d normed_frontal3d_90x270x0.png[1.5,90x270x0,255x255x255] -f T_template0NormedRatios.mha

antsVol -i sum.nii.gz -d [1.5,90x270x0,255x255x255] -f T_template0UnnormedRatios.mha

CreateTiledMosaic -d 2 \
                  -i T_template0.nii.gz \
                  -r T_template0NormedRatios.mha \
                  -x T_template0DktLabelsMask.nii.gz \
                  -a 0.5 \
                  -o tiledMosaic.png \
                  -s [2,mask-2,mask+2] \
                  -p mask+5 \
                  -g 1

