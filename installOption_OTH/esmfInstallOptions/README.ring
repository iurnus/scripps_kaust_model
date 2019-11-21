To install ESMF on ring:

  cp configure.esmf.ring ${esmf_DIR}
  cd ${esmf_DIR}

then:
  . configure.esmf.ring
  gmake info
  gmake
  gmake all_tests
