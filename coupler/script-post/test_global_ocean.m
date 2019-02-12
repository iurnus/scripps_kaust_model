% plot the figures using matlab

U=rdmds('U');
V=rdmds('V');
XG=rdmds('XG');
YG=rdmds('YG');

% temperature
T=rdmds('T');

% plot U contour
figure()
contourf(XG,YG,U(:,:,1))
title('velocity Ux')
xlabel('X')
ylabel('Y')
colorbar

% plot V contour
figure()
title('velocity Uy')
contourf(XG,YG,V(:,:,1))
xlabel('X')
ylabel('Y')
colorbar

% plot the velocity 
figure()
title('velocity quiver plot')
quiver(XG(1:5:end,1:5:end),YG(1:5:end,1:5:end),U(1:5:end,1:5:end,1),V(1:5:end,1:5:end,1))
xlabel('X')
ylabel('Y')
xlim([0,60])
ylim([0,60])

% plot the velocity 
figure()
title('velocity quiver plot')
quiver(XG(1:5:end,1:5:end),YG(1:5:end,1:5:end),U(1:5:end,1:5:end,2),V(1:5:end,1:5:end,2))
xlabel('X')
ylabel('Y')
xlim([0,60])
ylim([0,60])

% plot the velocity 
figure()
title('velocity quiver plot')
quiver(XG(1:5:end,1:5:end),YG(1:5:end,1:5:end),U(1:5:end,1:5:end,3),V(1:5:end,1:5:end,3))
xlabel('X')
ylabel('Y')
xlim([0,60])
ylim([0,60])

% plot the velocity 
figure()
title('velocity quiver plot')
quiver(XG(1:5:end,1:5:end),YG(1:5:end,1:5:end),U(1:5:end,1:5:end,4),V(1:5:end,1:5:end,4))
xlabel('X')
ylabel('Y')
xlim([0,60])
ylim([0,60])