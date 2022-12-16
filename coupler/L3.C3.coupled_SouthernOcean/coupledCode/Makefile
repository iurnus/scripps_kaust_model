# GNU Makefile template for user ESMF application

################################################################################
################################################################################
## This Makefile must be able to find the "esmf.mk" Makefile fragment in the  ##
## 'include' line below. Following the ESMF User's Guide, a complete ESMF     ##
## installation should ensure that a single environment variable "ESMFMKFILE" ##
## is made available on the system. This variable should point to the         ##
## "esmf.mk" file.                                                            ##
##                                                                            ##
## This example Makefile uses the "ESMFMKFILE" environment variable.          ##
##                                                                            ##
## If you notice that this Makefile cannot find variable ESMFMKFILE then      ##
## please contact the person responsible for the ESMF installation on your    ##
## system.                                                                    ##
## As a work-around you can simply hardcode the path to "esmf.mk" in the      ##
## include line below. However, doing so will render this Makefile a lot less ##
## flexible and non-portable.                                                 ##
################################################################################

ifneq ($(origin ESMFMKFILE), environment)
$(error Environment variable ESMFMKFILE was not set.)
endif

include $(ESMFMKFILE)
include ./wrflib.mk

################################################################################
################################################################################

.SUFFIXES: .f90 .F90 .c .C

%.o : %.f90
	$(ESMF_F90COMPILER) -byteswapio -r8 -Mnodclchk -Mextend -fast -fastsse -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(WRF_INC) $(ESMF_F90COMPILEFREENOCPP) $< -I${PWD} -L${PWD} $(WRF_LIB) -lmitgcm_org_ocn -lmitgcmrtl

%.o : %.F90
	$(ESMF_F90COMPILER) -byteswapio -r8 -Mnodclchk -Mextend -fast -fastsse -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(WRF_INC) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) $< -I${PWD} -L${PWD} $(WRF_LIB) -lmitgcm_org_ocn -lmitgcmrtl

% : %.c
	$(ESMF_CXXCOMPILER) -byteswapio -r8 -Mnodclchk -Mextend -fast -fastsse -c $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<

% : %.C
	$(ESMF_CXXCOMPILER) -byteswapio -r8 -Mnodclchk -Mextend -fast -fastsse -c $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<



# -----------------------------------------------------------------------------
esmf_application: mod_config.o mod_types.o mod_esmf_ocn.o mod_esmf_atm.o mod_esmf_cpl.o mod_esmf_esm.o mitgcm_wrf.o libmitgcm_org_ocn.a libmitgcmrtl.a
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) $(WRF_INC) -L${PWD} -o $@ $^ $(ESMF_F90ESMFLINKLIBS) $(WRF_LIB) setrlstk.o sigreg.o -lmitgcm_org_ocn -lmitgcmrtl

# module dependencies:
mod_esmf_esm.o: mod_esmf_ocn.o mod_esmf_atm.o mod_types.o 
mod_esmf_cpl.o: 
mod_esmf_ocn.o: mod_types.o
mod_esmf_atm.o: mod_types.o
mod_config.o: mod_types.o 
mod_types.o:

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
.PHONY: clean distclean
clean:
	rm -f esmf_application *.o 
distclean: clean
	rm -f PET*.ESMF_LogFile mod*.mod *.a mit*.mod
