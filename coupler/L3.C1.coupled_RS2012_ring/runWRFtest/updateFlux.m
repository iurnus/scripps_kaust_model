clear all;

% update SST
sst = ncread('cplFlux','SST');
uoce = ncread('cplFlux','UOCE');
voce = ncread('cplFlux','VOCE');
albbck = ncread('cplFlux','ALBBCK');
lai = ncread('cplFlux','LAI');
vegfra = ncread('cplFlux','VEGFRA');

[nx,ny,nz] = size(sst);
sst_save = sst;
uoce_save = uoce;
voce_save = voce;
albbck_save = albbck;
lai_save = lai;
vegfra_save = vegfra;

for k = 1:nz-1
  sst(:,:,k) = sst_save(:,:,k+1);
  uoce(:,:,k) = uoce_save(:,:,k+1);
  voce(:,:,k) = voce_save(:,:,k+1);
  albbck(:,:,k) = albbck_save(:,:,k+1);
  lai(:,:,k) = lai_save(:,:,k+1);
  vegfra(:,:,k) = vegfra_save(:,:,k+1);
end

ncwrite('cplFlux','SST',sst);
ncwrite('cplFlux','UOCE',uoce);
ncwrite('cplFlux','VOCE',voce);
ncwrite('cplFlux','ALBBCK',albbck);
ncwrite('cplFlux','LAI',lai);
ncwrite('cplFlux','VEGFRA',vegfra);
