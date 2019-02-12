To install ESMF on ring:

  cp build_rules.mk.ring ${esmf_DIR}/build_config/Linux.pgi.default/build_rules.mk
  cp version.pgCC.ring ${esmf_DIR}/scripts/version.pgCC
  cp configure.esmf.ring ${esmf_DIR}

then:
  gmake info
  gmake
  gmake all_tests
