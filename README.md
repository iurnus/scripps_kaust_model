The SKRIPS is under continuous development. The current stable version is [v2.0](https://github.com/iurnus/scripps_kaust_model/releases/tag/v2.0).

To insall and run the code, please refer to [code documentation](https://skrips.readthedocs.io/en/latest/).

To build the WRF model or the coupled model, please refer to [other documentation](https://github.com/iurnus/coupled_model_other_docs).

The previous versions are available here: [older versions](https://github.com/iurnus/scripps_kaust_model/releases).
********************************************************************************************

This is the repository for the Scripps-KAUST Regional Integrated Prediction System (SKRIPS). 
It is a regional coupled atmosphere-ocean model that includes WRF, MITgcm, WaveWatch III, and ESMF coupler.

Now all the components are available from GitHub:
* Atmosphere Solver: [WRF](https://github.com/wrf-model/WRF/releases/tag/v4.5.1) (version 4.5.1)
* Ocean Solver: [MITgcm](https://github.com/MITgcm/MITgcm/releases/tag/checkpoint68r) (version c68r)
* Wave Solver: [WaveWatch III](https://github.com/NOAA-EMC/WW3/archive/refs/tags/6.07.1.zip) (version 6.07.1)
* Driver (coupler): [ESMF](https://github.com/esmf-org/esmf/releases/tag/v8.6.0) (version 8.6.0)

The instructions on installing each component are detailed in the [code documentation](https://skrips.readthedocs.io/en/latest/). We also attached several [tutorial cases](https://github.com/iurnus/scripps_kaust_model/tree/master/coupler) for testing the coupled model.

In this repository, we have:
* coupler/ - *all coupler source code and examples*
* esmf\_test/ - *ESMF test cases*
* license\_statements/ - *license statements of the model components*
* README.md - *readme file*
* scripts/ - *scripts to install the model*

**Reference**

Sun, R., Subramanian, A. C., Miller, A. J., Mazloff, M. R., Hoteit, I., and Cornuelle, B. D.: SKRIPS v1.0: a regional coupled ocean–atmosphere modeling framework (MITgcm–WRF) using ESMF/NUOPC, description and preliminary results for the Red Sea, Geoscientific Model Development, 12, 4221–4244, [https://doi.org/10.5194/gmd-12-4221-2019](https://doi.org/10.5194/gmd-12-4221-2019), 2019.

Sun, R., Cobb, A., Villas Bôas, A. B., Langodan, S., Subramanian, A. C., Mazloff, M. R., Cornuelle, B. D., Miller, A. J., Pathak, R., and Hoteit, I.: Waves in SKRIPS: WAVEWATCH III coupling implementation and a case study of Tropical Cyclone Mekunu, Geosci. Model Dev., 16, 3435–3458, [https://doi.org/10.5194/gmd-16-3435-2023](https://doi.org/10.5194/gmd-16-3435-2023), 2023.

**Other papers**

Malyarenko, A., Gossart, A., Sun, R., and Krapp, M.: Conservation of heat and mass in P-SKRIPS version 1: the coupled atmosphere–ice–ocean model of the Ross Sea, Geosci. Model Dev., 16, 3355–3373, [https://doi.org/10.5194/gmd-16-3355-2023](https://doi.org/10.5194/gmd-16-3355-2023), 2023.

Cerovečki, I., Sun, R., Bromwich, D.H., Zou, X., Mazloff, M.R. and Wang, S.H., 2022. Impact of downward longwave radiative deficits on Antarctic sea-ice extent predictability during the sea ice growth period. Environmental Research Letters, 17(8), p.084008.

Sun, R., Subramanian, A. C., Cornuelle, B. D., Mazloff, M. R., Miller, A. J., Ralph, F. M., Seo, H., and Hoteit, I.: The role of air–sea interactions in atmospheric rivers: Case studies using the SKRIPS regional coupled model, Journal of Geophysical Research: Atmospheres, 126(6), e2020JD032885, [https://doi.org/10.1029/2020JD032885](https://doi.org/10.1029/2020JD032885), 2021.

Sun, R., Sanikommu, S., Subramanian, A.C., Mazloff, M.R., Cornuelle, B.D., Gopalakrishnan, G., Miller, A.J. and Hoteit, I., 2024. Enhanced regional ocean ensemble data assimilation through atmospheric coupling in the SKRIPS model. Ocean Modelling, p.102424.
