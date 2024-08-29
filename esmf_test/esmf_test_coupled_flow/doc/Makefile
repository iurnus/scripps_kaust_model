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

################################################################################
################################################################################

#TODO: implement error checking for ESMF_INTERNAL_ variable and inform user
#TODO: that this may not work for them and where to find doc.

DOCSCRIPTSDIR   = $(ESMF_INTERNAL_DIR)/scripts/doc_templates/templates
PROTEX		= $(DOCSCRIPTSDIR)/protex 
CC_PROTEX       = $(DOCSCRIPTSDIR)/scripts/do_ccprotex 
CH_PROTEX       = $(DOCSCRIPTSDIR)/scripts/do_chprotex 
F_PROTEX        = $(DOCSCRIPTSDIR)/scripts/do_fprotex 
DO_LATEX	= $(DOCSCRIPTSDIR)/scripts/do_latex
DO_L2H		= $(DOCSCRIPTSDIR)/scripts/do_l2h

################################################################################
################################################################################

.SUFFIXES: .f90 .F90 .c .C .h .dvi

%_fapi.tex : ../%.f90
	export PROTEX=$(PROTEX) ;\
	$(F_PROTEX) $* $<

%_fapi.tex : ../%.F90
	export PROTEX=$(PROTEX) ;\
	$(F_PROTEX) $* $<
        
%_chapi.tex : ../%.h
	export PROTEX=$(PROTEX) ;\
	$(CH_PROTEX) $* $<

%_ccapi.tex : ../%.c
	export PROTEX=$(PROTEX) ;\
	$(CC_PROTEX) $* $<

%_ccapi.tex : ../%.C
	export PROTEX=$(PROTEX) ;\
	$(CC_PROTEX) $* $<
        
%.pdf : %.dvi
	dvipdf $< $@


TEXFILES_TO_MAKE = $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../*.F90))))

# -----------------------------------------------------------------------------
ESMF_CoupledFlow.pdf: ESMF_CoupledFlow.dvi

ESMF_CoupledFlow.dvi: ESMF_CoupledFlow.tex flow_intro.tex flow_pieces.tex flow_struct.eps $(TEXFILES_TO_MAKE)
	rm -f title_alldoc.tex
	cp $(DOCSCRIPTSDIR)/title_alldoc.tex .
	latex $*
	latex $*

# module dependencies:
CoupledFlowApp.o: CoupledFlowDemo.o
CoupledFlowDemo.o: FlowSolverMod.o InjectorMod.o CouplerMod.o
FlowSolverMod.o: FlowArraysMod.o
InjectorMod.o: InjectArraysMod.o

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
.PHONY: dust clean distclean
dust:
	rm -f *.aux *.bbl *.blg *.log *.toc
clean:
	rm -f $(TEXFILES_TO_MAKE) *.dvi *.pdf title_alldoc.tex
distclean: dust clean
