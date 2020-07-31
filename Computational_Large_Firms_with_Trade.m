% Define parameters 
a_lf = 1;
a_lh = 1;
sigma = 7;

a_Lf = 1.2;
a_Lh = 1.2;
tau = 1.05;

fun = @(params)getOptPriceLargeFirmsWithTrade(params, sigma,tau, a_lf, a_lh, a_Lf,a_Lh);
tic;
options = optimset('TolX',1e-8);
params = fminsearch(fun,[1,1,1],options);
toc;

params

% Define parameters 
a_lf = 1;
a_lh = 1;
sigma = 7;

a_Lf = 1.2;
a_Lh = 1.3;
tau = 1.05;

fun = @(params)getOptPriceLargeFirmsWithTrade(params, sigma,tau, a_lf, a_lh, a_Lf,a_Lh);
tic;
options = optimset('TolX',1e-8);
params = fminsearch(fun,[1,1,1],options);
toc;

params