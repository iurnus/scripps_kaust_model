clear all

run '~/matlab_bin/pathdef.m'

load('colorbrewer.mat')
% set(0,'DefaultFigureColormap',cbrewer('seq','YlOrRd',64))

pickup = rdmds('pickup.0000000360');
diagFileName = 'diag2dKPP.0000000360';

U=pickup(:,:,1);
V=pickup(:,:,17);
T=pickup(:,:,33);
eta=pickup(:,:,97);
S=pickup(:,:,49);
diag=rdmds(diagFileName);
mxlHeight = diag(:,:,3);

XC=rdmds('XC');
YC=rdmds('YC');
maskInC=rdmds('maskInC');

xlo = 230.125;
xhi = 245.875;
ylo = 27.325;
yhi = 39.825;

delta = 0.05;

uMag = (U(:,:,1).^2+V(:,:,1).^2).^0.5;

for i = 1:size(U,1)
  for j = 1:size(U,2)
    if (maskInC(i,j) == 0)
        U(i,j) = NaN;
        V(i,j) = NaN;
        T(i,j) = NaN;
        depth(i,j) = NaN;
        eta(i,j) = NaN;
        S(i,j) = NaN;
        uMag(i,j) = NaN;
        mxlHeight(i,j) = NaN;
    end
  end
end

% plot temperature
fieldList = {T(:,:,1), eta, S(:,:,1), mxlHeight, uMag};
titleList = {'Sea Surface Temperature', 'Sea Surface Height', 'Salinity', 'Mixed Layer Depth', 'Velocity'};
caxisList = {[10,19],[-0.2,0.2],[32.5,34.5],[0,90],[0,1]};
fileNameList = {'sst_cpl.png','ssh_cpl.png','s_cpl.png','mxl_depth.png','vel_cpl.png'};
plotVector = {0,0,0,0,1};

for n = 1:5
  figure()
  h_t = surf(XC,YC,fieldList{n}); hold on;
  set(h_t,'edgecolor','none')
  set(h_t,'facecolor','interp')
  set(gca,'FontSize', 18)
  view([0,90])
  caxis(caxisList{n})
  colormap(flipud(cbrewer('div','Spectral',16,'PCHIP')))
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
