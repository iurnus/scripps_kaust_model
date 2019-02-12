clear all;

% update SST
sst = ncread('cplFlux','SST');
uoce = ncread('cplFlux','UOCE');
voce = ncread('cplFlux','VOCE');

[nx,ny,nz] = size(sst);
sst_save = sst;
uoce_save = uoce;
voce_save = voce;

for k = 1:nz-1
  % sst(:,:,k+1) = sst_save(:,:,k);
  % uoce(:,:,k+1) = uoce_save(:,:,k);
  % voce(:,:,k+1) = voce_save(:,:,k);
  sst(:,:,k) = sst_save(:,:,k+1);
  uoce(:,:,k) = uoce_save(:,:,k+1);
  voce(:,:,k) = voce_save(:,:,k+1);
end

ncwrite('cplFlux','SST',sst);
ncwrite('cplFlux','UOCE',uoce);
ncwrite('cplFlux','VOCE',voce);
