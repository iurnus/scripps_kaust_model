clear all

load('colorbrewer.mat')
% set(0,'DefaultFigureColormap',cbrewer('seq','YlOrRd',64))

% pickup = rdmds('pickup.0000000504');
% pickup2 = rdmds('../../test_california_fully_coupled/wrfRunCA_CPL/pickup.0000000504');
% diagFileName = 'diag2dKPP.0000000504';
% diagFileName2 = '../../test_california_fully_coupled/wrfRunCA_CPL/diag2dKPP.0000000504';
pickup = rdmds('pickup.0000000360');
pickup2 = rdmds('../../test_california_mitgcm/mitRun/pickup.0000000360');
diagFileName = 'diag2dKPP.0000000360';
diagFileName2 = '../../test_california_mitgcm/mitRun/diag2dKPP.0000000360';

U=pickup(:,:,1);
V=pickup(:,:,17);
T=pickup(:,:,33);
eta=pickup(:,:,97);
S=pickup(:,:,49);
diag=rdmds(diagFileName);
mxlHeight = diag(:,:,3);

U_2=pickup2(:,:,1);
V_2=pickup2(:,:,17);
T_2=pickup2(:,:,33);
eta_2=pickup2(:,:,97);
S_2=pickup2(:,:,49);
diag2=rdmds(diagFileName2);
mxlHeight2 = diag2(:,:,3);

uDiff = (U-U_2);
vDiff = (V-V_2);
tDiff = (T-T_2);
sDiff = (S-S_2);
etaDiff = (eta-eta_2);
uMag = (uDiff(:,:,1).^2+vDiff(:,:,1).^2).^0.5;
mxlDiff = mxlHeight - mxlHeight2;

XC=rdmds('XC');
YC=rdmds('YC');
maskInC=rdmds('maskInC');

xlo = 230.125;
xhi = 245.875;
ylo = 27.325;
yhi = 39.825;

delta = 0.05;

for i = 1:size(U,1)
  for j = 1:size(U,2)
    if (maskInC(i,j) == 0)
        uDiff(i,j) = NaN;
        vDiff(i,j) = NaN;
        tDiff(i,j) = NaN;
        etaDiff(i,j) = NaN;
        sDiff(i,j) = NaN;
        uMag(i,j) = NaN;
        mxlDiff(i,j) = NaN;
    end
  end
end

% plot temperature
fieldList = {tDiff(:,:,1), etaDiff, sDiff(:,:,1), mxlDiff, uMag};
titleList = {'Sea Surface Temperature', 'Sea Surface Height', 'Salinity', 'Mixed Layer Depth', 'Velocity'};
caxisList = {[-0.2,0.2],[-0.001,0.001],[-0.01,0.01],[-5,5],[0,0.1]};
fileNameList = {'sst_cpl_diff.png','ssh_cpl_diff.png','s_cpl_diff.png','mxl_depth_diff.png','vel_cpl_diff.png'};
plotVector = {0,0,0,0,0};

for n = 1:5
  figure()
  h_t = surf(XC,YC,fieldList{n}); hold on;
  set(h_t,'edgecolor','none')
  set(h_t,'facecolor','interp')
  set(gca,'FontSize', 18)
  view([0,90])
  caxis(caxisList{n})
  colormap(flipud(cbrewer('div','RdBu',16)))
  % colormap(flipud(cbrewer('div','Spectral',16)))
  % colormap(cbrewer('seq','YlOrRd',16))
  title(titleList{n})
  xlabel('Longitude')
  ylabel('Latitude')
  xticks([232,236,240,244])
  xticklabels({'128^{\circ}W','124^{\circ}W','120^{\circ}W','116^{\circ}W'})
  yticks([28,30,32,34,36,38])
  yticklabels({'28^{\circ}N','30^{\circ}N','32^{\circ}N','34^{\circ}N','36^{\circ}N','38^{\circ}N'})
  xlim([230.125,245.875])
  ylim([27.325,39.825])
  colorbar
  grid off
  set(gcf,'Color',[1 1 1]); set(gca,'Color',[.8 .8 .8]); set(gcf,'InvertHardCopy','off');
  set(gca,'TickDir','out')

  if (plotVector{n} == 1)
    nSkip = 3;
    XCq = XC(1:nSkip:end,1:nSkip:end);
    YCq = YC(1:nSkip:end,1:nSkip:end);
    Uq = U(1:nSkip:end,1:nSkip:end,1);
    Vq = V(1:nSkip:end,1:nSkip:end,1);
    h_q = quiver3(XCq, YCq, XCq-XCq+100, Uq, Vq, Uq-Uq, 'r-', 'linewidth', 2);
    view([0,90])
  end

  saveas(gcf,fileNameList{n},'png')

end
