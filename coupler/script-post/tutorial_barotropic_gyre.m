% plot the figures using matlab

U=rdmds('U');
V=rdmds('V');
XG=rdmds('XG');
YG=rdmds('YG');

% temperature
T=rdmds('T');

% plot U contour
figure()
contourf(XG,YG,U(:,:))
title('velocity Ux')
xlabel('X')
ylabel('Y')
colorbar

% plot V contour
figure()
title('velocity Uy')
contourf(XG,YG,V(:,:))
xlabel('X')
ylabel('Y')
colorbar

% plot the velocity 
figure()
title('velocity quiver plot')
quiver(XG(1:5:end,1:5:end),YG(1:5:end,1:5:end),U(1:5:end,1:5:end),V(1:5:end,1:5:end))
xlabel('X')
ylabel('Y')
xlim([0,1200000])
ylim([0,1200000])