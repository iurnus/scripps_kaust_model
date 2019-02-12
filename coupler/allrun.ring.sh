# Level 1 and 2 (L1 & L2) cases are test cases
# Level 3 cases are applications

echo "running allrun.ring.sh"

cd L1.C1.mitgcm_case_CA2009
  ./install.sh
cd ..

cd L1.C2.esmf_coupled_test
  ./install.sh
cd ..

cd L2.C1.mitgcm_case_CA2009_mpiESMF_1cpu
  ./install.sh
cd ..

cd L2.C2.mitgcm_case_CA2009_mpiESMF_2cpu
  ./install.sh
cd ..

cd L3.C1.coupled_RS2012_ring
  ./install.sh
cd ..

cd L3.C2.coupled_CA2018_ring
  ./install.sh
cd ..
