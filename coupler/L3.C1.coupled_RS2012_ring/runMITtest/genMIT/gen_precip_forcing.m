clear all
close all
clc

wrf_results = '../../runCase/wrfout_d01_2012-06-01_00:00:00';
opath = './';
fpath = opath;

eval ([ 'load ' fpath 'FMT.mat']);

ctl = {'RAINC','RAINNC','RAINSH'}

prec='double';
deltaT = 60.0;

np = length(ctl);
tmpFields = {};
for jp = 1:np
    jp
    param_wrf = [ctl{jp}];
    
    file = wrf_results;
        
    ncid = netcdf.open(file,'NOWRITE');
    [numdims, numvars, numglobalatts, unlimdimID] = netcdf.inq(ncid);

    for varid =1:numvars
        varlist(varid) = {netcdf.inqVar(ncid,varid-1)};
    end
    
    ind=find(ismember(varlist,param_wrf));
    tmp = netcdf.getVar(ncid,ind-1,prec);
    
    [varname vartype vardimIDs varatts] = netcdf.inqVar(ncid,ind-1);
    
    for varatid =1:varatts
        attlist(varatid) = {netcdf.inqAttName(ncid,ind-1,varatid-1)};
    end
    
    % miss = netcdf.getAtt(ncid,ind-1,'missing_value',prec)
    % if miss < 0
    %     tmp(tmp <= miss) = NaN;
    % else
    %     tmp(tmp >= miss) = NaN;
    % end
    
    if isempty(find(ismember(attlist,'scale_factor')))
        scale = 1.0;
    else
        scale = netcdf.getAtt(ncid,ind-1,'scale_factor',prec);
    end
    
    if isempty(find(ismember(attlist,'add_offset')))
        offset = 0.0;
    else
        offset = netcdf.getAtt(ncid,ind-1,'add_offset',prec);
    end
    
    tmp = offset + (tmp.*scale);

    tmpFields{jp} = tmp;

    clear p param_* tmp dpath
end

tmp_total = tmpFields{1} + tmpFields{2} + tmpFields{3};
tmp = tmp_total;

nTime = length(tmp(1,1,:));
for nT = 1:nTime-1
  tmp(:,:,nT+1) = (tmp_total(:,:,nT+1)-tmp_total(:,:,nT))/60.0/1000.0;
end

%% we need to flip as NCEPlat is flipped
% tmp = flipdim(tmp,2);
size(tmp)
wrslice([opath 'wrf_precip_1min_2012' ],tmp,1,fmt,Ieee);
netcdf.close(ncid);
