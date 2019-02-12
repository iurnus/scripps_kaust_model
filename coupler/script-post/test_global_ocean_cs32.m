clear all

% plot the figures using matlab
nX = 192
nY = 32

fileNames = {'pickup.0000072001','pickup.0000072005','pickup.0000072010','pickup.0000072020',}

for j = 1:4
    pickup=rdmds(fileNames{j});
    U = pickup(:,:,1:15);
    V = pickup(:,:,16:30);
    theta = pickup(:,:,31:46);

    XC=rdmds('XC');
    YC=rdmds('YC');
    deltaX = 4;
    deltaY = 4;
    xi = -180-deltaX:deltaX:180;
    yi = -90-deltaY:deltaY:90;
    [X,Y] = meshgrid(xi,yi);

    for i = 1:1

    uName=['u',num2str(j,'%04i'),num2str(i,'%04i'),'.png'];
    vName=['v',num2str(j,'%04i'),num2str(i,'%04i'),'.png'];
    uvName=['uv',num2str(j,'%04i'),num2str(i,'%04i'),'.png'];
    thetaName=['theta',num2str(j,'%04i'),num2str(i,'%04i'),'.png'];

    % plot U contour
    figure()
    Uq = cube2latlon(XC,YC,U,xi,yi);
    h = surf(X,Y,Uq(:,:,i)');
    set(h,'edgecolor','none')
    view([0,90])
    caxis([-0.15,0.15])
    colormap(jet)
    title('horizontal velocity')
    xlabel('X')
    ylabel('Y')
    xlim([-30,60])
    ylim([0,60])
    colorbar
    saveas(gcf,uName,'png')
    close

    % plot V contour
    figure()
    Vq = cube2latlon(XC,YC,V,xi,yi);
    h = surf(X,Y,Vq(:,:,i)');
    set(h,'edgecolor','none')
    view([0,90])
    caxis([-0.15,0.15])
    colormap(jet)
    title('vertical velocity')
    xlabel('X')
    ylabel('Y')
    xlim([-30,60])
    ylim([0,60])
    colorbar
    saveas(gcf,vName,'png')
    close

    % plot theta contour
    figure()
    thetaq = cube2latlon(XC,YC,theta,xi,yi);
    h = surf(X,Y,thetaq(:,:,i)');
    set(h,'edgecolor','none')
    view([0,90])
    caxis([0,25])
    colormap(jet)
    title('Theta')
    xlabel('X')
    ylabel('Y')
    xlim([-30,60])
    ylim([0,60])
    colorbar
    saveas(gcf,thetaName,'png')
    close

    % plot uv quiver
    figure()
    h = quiver(X,Y,Uq(:,:,i)',Vq(:,:,i)');
    title('velocity')
    xlabel('X')
    ylabel('Y')
    xlim([-180,180])
    ylim([-90,90])
    saveas(gcf,uvName,'png')
    close

    end
end
