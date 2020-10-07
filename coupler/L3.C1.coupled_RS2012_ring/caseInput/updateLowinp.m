clear all;

% update SST
hycomSSTFile = './hycom_T_ccs_newtopo_01Jun2012.bin'
hycomUFile = './hycom_U_ccs_newtopo_01Jun2012.bin'
hycomVFile = './hycom_V_ccs_newtopo_01Jun2012.bin'
fileID = fopen(hycomSSTFile);
iniT = fread(fileID,128*128*40,'real*4','b');
fclose(fileID);
fileID = fopen(hycomUFile);
iniU = fread(fileID,128*128*40,'real*4','b');
fclose(fileID);
fileID = fopen(hycomVFile);
iniV = fread(fileID,128*128*40,'real*4','b');
fclose(fileID);

% update wrf input file
iniT_ = reshape(iniT,[128,128,40]);
iniU_ = reshape(iniU,[128,128,40]);
iniV_ = reshape(iniV,[128,128,40]);
SST_MITgcm = iniT_(:,:,1) + 273.15;
U_MITgcm = iniU_(:,:,1);
V_MITgcm = iniV_(:,:,1);
sst = ncread('wrfinput_d01','SST');
sst_input = ncread('wrfinput_d01','SST_INPUT');
tsk = ncread('wrfinput_d01','TSK');
uoce = ncread('wrfinput_d01','UOCE');
voce = ncread('wrfinput_d01','VOCE');
landmask = ncread('wrfinput_d01','LANDMASK');

xMITgcm = 1:1:128;
yMITgcm = 1:1:128;
[xMITgrid,yMITgrid] = meshgrid(xMITgcm,yMITgcm);
xWRF = 1:1:128;
yWRF = 1:1:128;
[xWRFgrid,yWRFgrid] = meshgrid(xWRF,yWRF);
SST_MITgcm_interpl = interp2(xMITgrid,yMITgrid,SST_MITgcm,xWRFgrid,yWRFgrid);
U_MITgcm_interpl = interp2(xMITgrid,yMITgrid,U_MITgcm,xWRFgrid,yWRFgrid);
V_MITgcm_interpl = interp2(xMITgrid,yMITgrid,V_MITgcm,xWRFgrid,yWRFgrid);

for i = 1:128
  for j = 1:128
    deltaT = SST_MITgcm_interpl(i,j) - sst(i,j);
    % if landmask(i,j) < 0.5 && abs(deltaT) < 5
    if landmask(i,j) < 0.5
      sst(i,j) = SST_MITgcm_interpl(i,j);
      tsk(i,j) = SST_MITgcm_interpl(i,j);
      uoce(i,j) = U_MITgcm_interpl(i,j);
      voce(i,j) = V_MITgcm_interpl(i,j);
    end
    sst_input(i,j) = sst(i,j);
  end
end
ncwrite('wrfinput_d01','SST',sst);
ncwrite('wrfinput_d01','SST_INPUT',sst_input);
ncwrite('wrfinput_d01','TSK',tsk);
ncwrite('wrfinput_d01','UOCE',uoce);
ncwrite('wrfinput_d01','VOCE',voce);

% update wrf low input file
sst_low = ncread('wrflowinp_d01','SST');
sst_input_low = ncread('wrflowinp_d01','SST_INPUT');

[nx,ny,nt] = size(sst_low)
for k = 1:nt
  if k == 1
    sst_input_low(:,:,k) = sst(:,:);
  else
    sst_input_low(:,:,k) = sst_low(:,:,k);
  end
end
ncwrite('wrflowinp_d01','SST_INPUT',sst_input_low);

