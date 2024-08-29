U = ncread('U_velocity.nc', 'U');
V = ncread('V_velocity.nc', 'V');

figure;
contourf(U(:,:,20))
figure;
contourf(V(:,:,20))
