clear all;

% update SST
sst       = ncread('cplFlux','SST');
sst_input = ncread('cplFlux','SST_INPUT');
uoce      = ncread('cplFlux','UOCE');
voce      = ncread('cplFlux','VOCE');
albbck    = ncread('cplFlux','ALBBCK');
lai       = ncread('cplFlux','LAI');
vegfra    = ncread('cplFlux','VEGFRA');
ocnmask   = ncread('cplFlux','OCNMASK');

[nx,ny,nz] = size(sst);
sst_save       = sst;
sst_input_save = sst_input;
uoce_save      = uoce;
voce_save      = voce;
albbck_save    = albbck;
lai_save       = lai;
vegfra_save    = vegfra;
ocnmask_save   = ocnmask;

for k = 1:nz-1
  sst(:,:,k) = sst_save(:,:,k+1);
  sst_input(:,:,k) = sst_input_save(:,:,k+1);
  uoce(:,:,k) = uoce_save(:,:,k+1);
  voce(:,:,k) = voce_save(:,:,k+1);
  albbck(:,:,k) = albbck_save(:,:,k+1);
  lai(:,:,k) = lai_save(:,:,k+1);
  vegfra(:,:,k) = vegfra_save(:,:,k+1);
  ocnmask(:,:,k) = ocnmask_save(:,:,k+1);
end

ncwrite('cplFlux','SST',sst);
ncwrite('cplFlux','SST_INPUT',sst_input);
ncwrite('cplFlux','UOCE',uoce);
ncwrite('cplFlux','VOCE',voce);
ncwrite('cplFlux','ALBBCK',albbck);
ncwrite('cplFlux','LAI',lai);
ncwrite('cplFlux','VEGFRA',vegfra);
ncwrite('cplFlux','OCNMASK',ocnmask);
