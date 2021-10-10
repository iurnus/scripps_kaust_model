The SKRIPS is under continous development. 

The most recent stable version is v1.2. It is available here: [download v1.2](https://github.com/iurnus/scripps_kaust_model/releases/tag/v1.2)

The older versions are available here: [older versions](https://github.com/iurnus/scripps_kaust_model/releases)

To insall the code, please refer to [code documentation](https://skrips.readthedocs.io/en/v1.2/).


********************************************************************************************

This is the repository for the Scripps-KAUST Regional Integrated Prediction System (SKRIPS). 
It is a regional coupled atmosphere-ocean model that includes the following components:

* Atmosphere Solver: [WRF](https://github.com/wrf-model/WRF/releases/tag/v4.1.3) (version 4.1.3)
* Ocean Solver: [MITgcm](https://github.com/MITgcm/MITgcm/releases/tag/checkpoint67m) (version c67m)
* Driver (coupler): [ESMF](https://www.earthsystemcog.org/projects/esmf/download_800) (version 8.0.0)

The instructions on installing each component are detailed in the [code documentation](https://skrips.readthedocs.io/en/v1.2/). 
There are several tutorial cases for testing the coupled model.

Users can also extend this solver using some pre-requisite knowledge of the underlying method and programming techniques.

In this repository, we have:

* Allmake.ring.sh - *script to compile the code on local desktop ring using PGI compiler*
* Allmake.shaheen.sh - *script to compile the code on Shaheen-II supercomputer using Intel compiler*
* Allclean.sh - *script to uninstall the code*
* coupler/ - *all coupler source code and examples*
* esmf\_test\_application/ - *ESMF test cases*
* installOption\_WRF/ - *WRF scripts with different install options*
* installOption\_OTH/ - *scripts to install other code*
* license\_statements/ - *license statements of the model components*
* README.md - *readme file*

**Reference**

Sun, R., Subramanian, A. C., Miller, A. J., Mazloff, M. R., Hoteit, I., and Cornuelle, B. D.: SKRIPS v1.0: a regional coupled ocean–atmosphere modeling framework (MITgcm–WRF) using ESMF/NUOPC, description and preliminary results for the Red Sea, Geoscientific Model Development, 12, 4221–4244, [https://doi.org/10.5194/gmd-12-4221-2019](https://doi.org/10.5194/gmd-12-4221-2019), 2019.

**Other papers**

Sun, R., Subramanian, A. C., Cornuelle, B. D., Mazloff, M. R., Miller, A. J., Ralph, F. M., Seo, H., and Hoteit, I.: The role of air–sea interactions in atmospheric rivers: Case studies using the SKRIPS regional coupled model, Journal of Geophysical Research: Atmospheres, 126(6), e2020JD032885, [https://doi.org/10.1029/2020JD032885](https://doi.org/10.1029/2020JD032885), 2021.
