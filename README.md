The Scripps-KAUST Regional Integrated Prediction System (SKRIPS)

This is a documentation for the regional coupled atmosphere-ocean model.
This model is designed to be a state-of-the-art coupled atmosphere-ocean
modeling system based on MITgcm and WRF. It also supports the new
components by using the ESMF coupler.

The designed modeling system currently includes the following models:

* Atmosphere Solver: [WRF](http://www2.mmm.ucar.edu/wrf/users/download/get_source.html) (version 3.9.1.1)
* Ocean Solver: [MITgcm](http://mitgcm.org/public/source_code.html) (version c66h)
* Driver (coupler): [ESMF](https://www.earthsystemcog.org/projects/esmf/download/) (version 7.0.0)

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
* MITgcm\_c66h/ - *MITgcm source code*
* WPS/ - *WRF preprocessor*
* WRFV3911\_AO/ - *WRF source code*

To compile the coupled solver (use PGI compiler), please run:

```
sh Allmake.ring.sh
```

A more detailed introduction of the code and tutorial cases can be found in the
code [documentation](https://scripps-coupled-atmosphere-ocean-model.readthedocs.io).

**Reference**

Sun, R., Subramanian, A. C., Miller, A. J., Mazloff, M. R., Hoteit, I., and Cornuelle, B. D.: SKRIPS v1.0: a regional coupled ocean–atmosphere modeling framework (MITgcm–WRF) using ESMF/NUOPC, description and preliminary results for the Red Sea, Geosci. Model Dev., 12, 4221–4244, [https://doi.org/10.5194/gmd-12-4221-2019](https://doi.org/10.5194/gmd-12-4221-2019), 2019.
