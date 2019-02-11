The scripps coupled regional atmosphere-ocean model

This is a documentation for the regional coupled atmosphere-ocean model.
This model is designed to be a state-of-the-art coupled atmosphere-ocean
modeling system based on MITgcm and WRF. It also supports the new
components by using the ESMF coupler.

The designed modeling system currently includes the following models:

Atmosphere Solver: WRF (version 3.9.1.1)
Ocean Solver: MITgcm (version c66h)
Driver (coupler): ESMF (version 7.0.0)

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

* Allmake.sh - Script to make ESMF, WRF, MITgcm, and the coupled solver
* coupler/ - All coupler source code and examples
* esmf\_test\_application/ - ESMF test cases
* installOption\_WRF/ - WRF scripts with different install options
* installOption\_OTH/ - install options for WPS ESMF MITgcm or other software
* README.md - readme file

The other folders that should be added to finish the installation:
(They are not supported in the current GIT repository)

* esmf/ - ESMF code
* MITgcm\_c66h/ - MITgcm source code
* WPS/ - WRF preprocessor
* WRFV3911\_AO/ - WRF source code

To compile the coupled solver, please run:

  sh Allmake.sh

A more detailed introduction of the code and tutorial cases can be found in the
code documentation.
