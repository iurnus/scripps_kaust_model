clear all

% plot the figures using matlab
nX = 192
nY = 32

fileNames1 = {'pickup.0000072001','pickup.0000072005','pickup.0000072010','pickup.0000072020',}
fileNames2 = {'../../global_ocean.cs32x15/run/pickup.0000072001','../../global_ocean.cs32x15/run/pickup.0000072005','../../global_ocean.cs32x15/run/pickup.0000072010','../../global_ocean.cs32x15/run/pickup.0000072020',}

for j = 1:4
    pickup1=rdmds(fileNames1{j});
    U1 = pickup1(:,:,1:15);
    V1 = pickup1(:,:,16:30);
    theta1 = pickup1(:,:,31:46);
    XC=rdmds('XC');
    YC=rdmds('YC');
    deltaX = 4;
    deltaY = 4;
    xi = -180-deltaX:deltaX:180;
    yi = -90-deltaY:deltaY:90;
    [X,Y] = meshgrid(xi,yi);

    pickup2=rdmds(fileNames2{j});
    U2 = pickup2(:,:,1:15);
    V2 = pickup2(:,:,16:30);
    theta2 = pickup2(:,:,31:46);


    for i = 1:1

    uName=['u_diff',num2str(j,'%04i'),num2str(i,'%04i'),'.png'];
    vName=['v_diff',num2str(j,'%04i'),num2str(i,'%04i'),'.png'];
    uvName=['uv_diff',num2str(j,'%04i'),num2str(i,'%04i'),'.png'];
    thetaName=['theta_diff',num2str(j,'%04i'),num2str(i,'%04i'),'.png'];

    % plot U contour
    figure()
    Uq1 = cube2latlon(XC,YC,U1,xi,yi);
    Uq2 = cube2latlon(XC,YC,U2,xi,yi);
    h = surf(X,Y,Uq1(:,:,i)'-Uq2(:,:,i)');
    set(h,'edgecolor','none')
    view([0,90])
    caxis([-0.0015,0.0015])
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
    Vq1 = cube2latlon(XC,YC,V1,xi,yi);
    Vq2 = cube2latlon(XC,YC,V2,xi,yi);
    h = surf(X,Y,Vq1(:,:,i)'-Vq2(:,:,i)');
    set(h,'edgecolor','none')
    view([0,90])
    caxis([-0.0015,0.0015])
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
    thetaq1 = cube2latlon(XC,YC,theta1,xi,yi);
    thetaq2 = cube2latlon(XC,YC,theta2,xi,yi);
    h = surf(X,Y,thetaq1(:,:,i)'-thetaq2(:,:,i)');
    set(h,'edgecolor','none')
    view([0,90])
    caxis([0,0.25])
    colormap(jet)
    title('Theta')
    xlabel('X')
    ylabel('Y')
    xlim([-30,60])
    ylim([0,60])
    colorbar
    saveas(gcf,thetaName,'png')
    close

    end
end
