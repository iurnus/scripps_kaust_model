;   Example script - plot terrain (using some basic WRF scripts)
;   Map info is now added via the special "wrf_map_overlays" function
;   November 2008

load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"  ; Add special wrf functions

begin

  a = addfile("./wrfout_d01_2009-01-01_00:00:00","r")
  b = addfile("../save/wrfout_ca.nc","r")

  ; type = "x11"
  type = "pdf"
  ; type = "png"
  ; type = "ps"
  ; type = "ncgm"
    
  wks = gsn_open_wks(type,"plt_comp")       ; Create a plot workstation

  ;  What times and how many time steps are in the data set?
  times  = wrf_user_list_times(a)  ; get times in the file
  ntimes = dimsizes(times)         ; number of times in the file

  print(a)

  res = True
  res@MainTitle = "GEOGRID FIELDS"

  do it = 0,ntimes-1,1

    time = it
    t2 = wrf_user_getvar(a,"T2",time)
    t2b = wrf_user_getvar(b,"T2",time)

    u10 = wrf_user_getvar(a,"U10",time)
    u10b = wrf_user_getvar(b,"U10",time)
    v10 = wrf_user_getvar(a,"V10",time)
    v10b = wrf_user_getvar(b,"V10",time)

    ; calculate SLP
    T  = a->T(time,:,:,:)
    th = T + 300.

    P  = a->P(time,:,:,:)
    PB = a->PB(time,:,:,:)
    p  = ( P + PB )

    tk = wrf_tk( p , th )

    QVAPOR = a->QVAPOR(time,:,:,:)
    PH     = a->PH(time,:,:,:)
    PHB    = a->PHB(time,:,:,:)
    var    = ( PH + PHB ) / 9.81
    dim    = dimsizes(var)
    z      = 0.5 * ( var(0:dim(0)-2,:,:) + var(1:dim(0)-1,:,:) )

    slp   = wrf_slp( z, tk, p, QVAPOR )

    ; calculate SLPb
    T  = b->T(time,:,:,:)
    th = T + 300.

    P  = b->P(time,:,:,:)
    PB = b->PB(time,:,:,:)
    p  = ( P + PB )

    tk = wrf_tk( p , th )

    QVAPOR = b->QVAPOR(time,:,:,:)
    PH     = b->PH(time,:,:,:)
    PHB    = b->PHB(time,:,:,:)
    var    = ( PH + PHB ) / 9.81
    dim    = dimsizes(var)
    z      = 0.5 * ( var(0:dim(0)-2,:,:) + var(1:dim(0)-1,:,:) )

    slpb   = wrf_slp( z, tk, p, QVAPOR )


    ; plotting options for T
    opts = res
    opts@cnFillOn = True
    opts@TimeLabel = times(it)
    opts@ContourParameters = (/ -1.0, 1.0, 0.1 /)
    opts@cnFillPalette = "temp_diff_18lev"
    contour_t2 = wrf_contour(a,wks,t2-t2b,opts)
    delete(opts)

    ; ; plotting options for slp
    ; opts = res
    ; opts@cnLineColor = "Blue"
    ; opts@cnHighLabelsOn = True
    ; opts@cnLowLabelsOn = True
    ; contour_slp = wrf_contour(a,wks,slp-slpb,opts)
    ; delete(opts)

    ; ; plotting options for wind vectors
    ; opts = res
    ; opts@FieldTitle = "Winds"
    ; opts@NumVectors = 47
    ; vector_uv = wrf_vector(a,wks,u10-u10b,v10-v10b,opts)
    ; delete(opts)

    pltres = True
    mpres = True
    ; plot = wrf_map_overlays(a,wks,(/contour_t2,contour_slp,vector_uv/),pltres,mpres)
    plot = wrf_map_overlays(a,wks,(/contour_t2/),pltres,mpres)

  end do

end


