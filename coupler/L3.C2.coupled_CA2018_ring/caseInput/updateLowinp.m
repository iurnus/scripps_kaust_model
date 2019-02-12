clear all;

% update SST
hycomSSTFile = './hycom_T_ccs_newtopo_27Jan2018.bin'
hycomUFile = './hycom_U_ccs_newtopo_27Jan2018.bin'
hycomVFile = './hycom_V_ccs_newtopo_27Jan2018.bin'
fileID = fopen(hycomSSTFile);
iniT = fread(fileID,432*256*40,'real*4','b');
fclose(fileID);
fileID = fopen(hycomUFile);
iniU = fread(fileID,432*256*40,'real*4','b');
fclose(fileID);
fileID = fopen(hycomVFile);
iniV = fread(fileID,432*256*40,'real*4','b');
fclose(fileID);

iniT_ = reshape(iniT,[432,256,40]);
iniU_ = reshape(iniU,[432,256,40]);
iniV_ = reshape(iniV,[432,256,40]);
SST_MITgcm = iniT_(:,:,1) + 273.15;
U_MITgcm = iniU_(:,:,1);
V_MITgcm = iniV_(:,:,1);
sst = ncread('wrfinput_d01','SST');
sst_input = ncread('wrfinput_d01','SST_INPUT');
tsk = ncread('wrfinput_d01','TSK');
uoce = ncread('wrfinput_d01','UOCE');
voce = ncread('wrfinput_d01','VOCE');
landmask = ncread('wrfinput_d01','LANDMASK');

xMITgcm = 0.5:431.5;
yMITgcm = 0.5:255.5;
[xMITgrid,yMITgrid] = meshgrid(xMITgcm,yMITgcm);
xWRF = 0.5:431.5;
yWRF = 0.5:255.5;
[xWRFgrid,yWRFgrid] = meshgrid(xWRF,yWRF);
SST_MITgcm_interpl = interp2(xMITgrid,yMITgrid,SST_MITgcm',xWRFgrid,yWRFgrid);
U_MITgcm_interpl = interp2(xMITgrid,yMITgrid,U_MITgcm',xWRFgrid,yWRFgrid);
V_MITgcm_interpl = interp2(xMITgrid,yMITgrid,V_MITgcm',xWRFgrid,yWRFgrid);

SST_MITgcm_interpl = SST_MITgcm_interpl';
U_MITgcm_interpl = U_MITgcm_interpl';
V_MITgcm_interpl = V_MITgcm_interpl';

deltaT = abs(SST_MITgcm_interpl - sst);
for i = 1:432
  for j = 1:256
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
