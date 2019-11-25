The SKRIPS is under continous development. 

The most recent stable version is v1.0. It is available here: [download v1.0](https://github.com/iurnus/scripps_kaust_model/releases/tag/v1.0)

The Scripps-KAUST Regional Integrated Prediction System (SKRIPS)

This is a documentation for the regional coupled atmosphere-ocean model.
This model is designed to be a state-of-the-art coupled atmosphere-ocean
modeling system based on MITgcm and WRF. It also supports the new
components by using the ESMF coupler.

The designed modeling system currently includes the following models:

* Atmosphere Solver: [WRF](https://github.com/wrf-model/WRF/releases/tag/v4.1.2) (version 4.1.2)
* Ocean Solver: [MITgcm](https://github.com/MITgcm/MITgcm/releases/tag/checkpoint67m) (version c67m)
* Driver (coupler): [ESMF](https://www.earthsystemcog.org/projects/esmf/download_800) (version 8.0.0)

The features of the system includes:

* Multiple coupling time step
* Multiple execution styles (concurrent and sequential execution)

Install the project requires the installation of MITgcm, WRF ESMF, and their
dependencies. The instructions on the installation of each component and the
coupler are detailed in the code documentation. Moreover, running the coupled
code and post-processing tutorial examples are also included.

Users can also extend this solver, utilities and libraries of this
coupled-solver, using some pre-requisite knowledge of the underlying method,
physics and programming techniques involved.

There are several folders in this GIT repository:

* Allmake.ring.sh - *script to compile the code on local desktop ring using PGI compiler*
* Allmake.shaheen.sh - *script to compile the code on Shaheen-II supercomputer using Intel compiler*
* Allclean.sh - *script to uninstall the code*
* coupler/ - *all coupler source code and examples*
* esmf\_test\_application/ - *ESMF test cases*
* installOption\_WRF/ - *WRF scripts with different install options*
* installOption\_OTH/ - *install options for WPS ESMF MITgcm or other software*
* license\_statements/ - *license statements of the model components*
* README.md - *readme file*

The source code of MITgcm, WRF and ESMF (not supported in the GIT repository) 
should be added to finish the installation:

* esmf/ - *ESMF code*
* MITgcm\_c67m/ - *MITgcm source code*
* WPS/ - *WRF preprocessor*
* WRFV412\_AO/ - *WRF source code*

To compile the coupled solver (use PGI compiler), please run:

```
sh Allmake.ring.sh
```

A more detailed introduction of the code and tutorial cases can be found in the
code [documentation](https://scripps-coupled-atmosphere-ocean-model.readthedocs.io).

**Reference**

Sun, R., Subramanian, A. C., Miller, A. J., Mazloff, M. R., Hoteit, I., and Cornuelle, B. D.: SKRIPS v1.0: a regional coupled ocean–atmosphere modeling framework (MITgcm–WRF) using ESMF/NUOPC, description and preliminary results for the Red Sea, Geosci. Model Dev., 12, 4221–4244, [https://doi.org/10.5194/gmd-12-4221-2019](https://doi.org/10.5194/gmd-12-4221-2019), 2019.
