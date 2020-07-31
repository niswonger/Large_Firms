function V = getOptPriceLargeFirmsWithTrade(theta,sigma,tau, a_lf, a_lh, a_Lf,a_Lh)
  % pull out parameters that we are estimating
  E_h = theta(1);
  E_f = theta(2);
  w_f = theta(3);
  r_h = theta(4);
  r_f = theta(5);
  
  w_h = 1;
  % Given the wage rates 
  p_lhh = sigma/(sigma-1)*a_lh*w_h; % Price of home good at home for small firms
  p_lhf = sigma/(sigma-1)*a_lh*tau*w_h; % Price of home good abroad for small firms
  p_lfh = sigma/(sigma-1)*a_lf*tau*w_f; % Price of foreign good at home for small firms
  p_lff = sigma/(sigma-1)*a_lf*w_f; % Price of foreign good abroad for small firms
  
  % set tolerance 
    tolerance = 10^-10; 
  % Start contraction mapping
    p0 = ones(2,2);
    p = zeros(size(p0));
    % Perform contraction mapping
    while norm(p-p0)>tolerance
        p = p0;
        % Estimate Shares
        P_h = (p_lhh^(1-sigma)+(tau*p_lfh)^(1-sigma)+p(1,1)^(1-sigma)+p(1,2)^(1-sigma))^(1/(1-sigma));
        P_f = (p_lff^(1-sigma)+(tau*p_lhf)^(1-sigma)+p(2,1)^(1-sigma)+p(2,2)^(1-sigma))^(1/(1-sigma));
        % Calculate shares at this Price indeces given assumed price
        s_hh = p(1,1)^(1-sigma)/P_h^(1-sigma);
        s_hf = p(2,1)^(1-sigma)/P_f^(1-sigma);
        s_fh = p(1,2)^(1-sigma)/P_h^(1-sigma);
        s_ff = p(2,2)^(1-sigma)/P_f^(1-sigma);
        % Calculate new prices given shares
        p0_hh = (sigma-(sigma-1)*s_hh)/((sigma-1)*(1-s_hh))*w_h*a_Lh;
        p0_hf = (sigma-(sigma-1)*s_hf)/((sigma-1)*(1-s_hf))*w_h*tau*a_Lh;
        p0_fh = (sigma-(sigma-1)*s_fh)/((sigma-1)*(1-s_fh))*w_f*tau*a_Lf;
        p0_ff = (sigma-(sigma-1)*s_ff)/((sigma-1)*(1-s_ff))*w_f*a_Lf;
        p0 = [p0_hh,p0_fh;p0_hf,p0_ff];
    end
    A_h = E_h*P_h^(sigma-1);
    A_f = E_f*P_f^(sigma-1);
    V = [E_h - w_h-A_h*p0_hh^(-sigma)*(p0_hh-w_h*a_Lh)-A_f*p0_hf^(-sigma)*(p0_hf-w_h*tau*a_Lh),...
         E_f - w_f-A_f*p0_ff^(-sigma)*(p0_ff-w_f*a_Lf)-A_h*p0_fh^(-sigma)*(p0_fh-w_f*tau*a_Lf),...
        1- A_h*p_lhh^(-sigma)*a_lh-A_f*(tau*p_lhf)^(-sigma)*tau*a_lh-...
        A_h*p0_hh^(-sigma)*a_Lh-A_f*(p0_hf)^(-sigma)*tau*a_lh,...
        1- A_f*p_lff^(-sigma)*a_lf-A_h*(tau*p_lfh)^(-sigma)*tau*a_lf-...
        A_f*p0_ff^(-sigma)*a_Lf-A_h*(p0_fh)^(-sigma)*tau*a_lf];
    V = V*V';
end