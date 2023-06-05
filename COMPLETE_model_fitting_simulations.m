function [lik, a, r, c, Q, V] = COMPLETE_model_fitting_simulations(params,s,A,R,C,model,optsim)

% Parameters
beta    = params(1); % choice temperature
alphaQf = params(2); % alpha update Q-values
alphaQc = params(3); % alpha update Q-values (NOT USED IN THIS VERSION)
alphaV  = params(4); % alpha update context value (NOT USED IN THIS VERSION)
w       = params(5); % weighting parameter (NOT USED IN IN THIS VERSION)
exputi  = params(6); % utility parameter freed (homologous to utility)
forget = params(7); % Forgetting parameter (for complete model)
% exputi  = 1;


Q = zeros(8,2);     % Q-values matrix
Qa = zeros(8,2);    % hybrid model: absolute learner
Qr = zeros(8,2);    % hybrid model: relative learner
V  = zeros(8,1);      % relative model: state value
Rmin = zeros(8,1);  % range model: Reward max
Rmax = zeros(8,1);  % range model: Reward min

lik = 0;
r = zeros(length(s),1);
c = zeros(length(s),1);
r2 = zeros(length(s),1);
c2 = zeros(length(s),1);


for i = 1:length(s)
    
    if i == length(s)/2 +1 % first trial of post test
        
        Q(5,:) = [Q(3,2),Q(1,2)] * forget;
        Q(6,:) = [Q(3,1),Q(1,1)] * forget;
        Q(7,:) = [Q(4,1),Q(2,2)] * forget;
        Q(8,:) = [Q(4,2),Q(2,1)] * forget;
        
    end
    
    if optsim == 1 % optimization / fitting
        
        a(i) = A(i);
        
%         if s(i)<5   % fit learning test only
            if ~isnan(a(i)); lik = lik + beta * Q(s(i),a(i)) - log(sum(exp(beta * Q(s(i),:)))); end               % the likelihood of the choice
%         end
        
    elseif optsim == 2 % simulations
        
        Pc(i)=1/(1+exp((Q(s(i),1)-Q(s(i),2))*(beta)));  % probability of choosing best option
        
        if rand <= Pc(i)
            a(i) = 2;   % good choice
        else
            a(i) = 1;   % bad choice
        end       
    end    
    
    if ~isnan(a(i))
        
        if s(i)<5 % learning test
            
            if optsim == 1
                r(i) = R(i)+9*(s(i)<3 & (R(i)==1));
                if r(i) == 10
                    r(i) = r(i) * exputi;
                else
                    r(i) = r(i);
                end
                c(i) = C(i)+9*(s(i)<3 & (C(i)==1));
                if c(i) == 10
                    c(i) = c(i) * exputi;
                else
                    c(i) = c(i);
                end
            elseif optsim == 2
                
                % reward probability = 0.75 if good option, 0.25 if bad option
                p_reward  = 0.25+(a(i)-1)*0.50;
                
                if s(i) < 3 % big magnitude
                    if rand <= p_reward ;  r(i)=10*exputi; end
                    if rand <= 1-p_reward ;  c(i)=10*exputi; end
                elseif s(i) > 2 % small magnitude
                    if rand <= p_reward ;  r(i)=1; end
                    if rand <= 1-p_reward ;  c(i)=1; end
                end
            end
            
        elseif s(i) > 4 % post test
            
            if optsim == 1
                if R(i)==1 
                r(i) = R(i)+(a(i)-1)*9; 
                if r(i) == 10
                    r(i) = r(i) * exputi;
                else
                    r(i) = r(i);
                end
                end

                if C(i)==1 
                    c(i) = C(i)+(a(i)-1)*9; 
                if c(i) == 10
                    c(i) = c(i) * exputi;
                else
                    c(i) = c(i);
                end
                end

            elseif optsim == 2
                if s(i) == 5
                    p_reward  = 0.75;
                    if rand <= p_reward ;  r(i)=1+(a(i)-1)*9; end
                    if r(i) == 10; r(i)= r(i)*exputi; end
                    if rand <= p_reward ;  c(i)=10-(a(i)-1)*9; end
                    if c(i) == 10; c(i)= c(i)*exputi; end
                elseif s(i) == 6
                    p_reward  = 0.25;
                    if rand <= p_reward ;  r(i)=1+(a(i)-1)*9; end
                    if r(i) == 10; r(i)= r(i)*exputi; end
                    if rand <= p_reward ;  c(i)=10-(a(i)-1)*9; end
                    if c(i) == 10; c(i)= c(i)*exputi; end
                elseif s(i) == 7
                    p_reward  = 0.25+(a(i)-1)*0.50;
                    if rand <= p_reward ;  r(i)=1+(a(i)-1)*9; end
                    if r(i) == 10; r(i)= r(i)*exputi; end
                    if rand <= 1-p_reward ;  c(i)=10-(a(i)-1)*9; end
                    if c(i) == 10; c(i)= c(i)*exputi; end
                elseif s(i) == 8
                    p_reward  = 0.75-(a(i)-1)*0.50;
                    if rand <= p_reward ;  r(i)=1+(a(i)-1)*9; end
                    if r(i) == 10; r(i)= r(i)*exputi; end
                    if rand <= 1-p_reward ;  c(i)=10-(a(i)-1)*9; end
                    if c(i) == 10; c(i)= c(i)*exputi; end
                end
            end
        end
        
        if model==1 % Qlearning
            
            
            if s(i) < 5
                deltaI(i) = r(i) - Q(s(i),a(i)) ;
                Q(s(i),a(i)) =  Q(s(i),a(i))   + alphaQf * deltaI(i);
                
                deltaC(i) = c(i) - Q(s(i),3-a(i)) ;
%                 Q(s(i),3-a(i)) =  Q(s(i),3-a(i))   + alphaQc * deltaC(i);
                Q(s(i),3-a(i)) =  Q(s(i),3-a(i))   + alphaQf * deltaC(i);
                
            end
            
        elseif model==2 % RELATIVE 2018
            
            if s(i) < 5
                
                % V takes the value of the first non-zero outcome
                if V(s(i))==0
                    if r(i)==0
                        if c(i)~=0
                            V(s(i))=c(i);
                        end
                    else
                        V(s(i))=r(i);
                    end
                else
                    % range adaptation (magnitude) and reference-point centering (valence, not useful in this task)
                    r2(i) = r(i)/abs(V(s(i))) + max(0,-V(s(i))/abs(V(s(i))));
                    c2(i) = c(i)/abs(V(s(i))) + max(0,-V(s(i))/abs(V(s(i))));
                end
                
                deltaI(i) = r2(i) - Q(s(i),a(i)) ;
                Q(s(i),a(i)) =  Q(s(i),a(i))   + alphaQf * deltaI(i);
                
                deltaC(i) = c2(i) - Q(s(i),3-a(i)) ;
                Q(s(i),3-a(i)) =  Q(s(i),3-a(i))   + alphaQc * deltaC(i);
                
            end
            
        elseif model==3 % HYBRID            
            
            if s(i) < 5  
                
                % hybrid weighted sum
                Q(s(i),:) = w * Qa(s(i),:) + (1-w) * Qr(s(i),:);
                
                % absolute learner
                
                deltaI(i) = r(i) - Qa(s(i),a(i)) ;
                Qa(s(i),a(i)) =  Qa(s(i),a(i))   + alphaQf * deltaI(i);
                
                deltaC(i) = c(i) - Qa(s(i),3-a(i)) ;
                Qa(s(i),3-a(i)) =  Qa(s(i),3-a(i))   + alphaQc * deltaC(i);
                
                % relative learner
                
                if V(s(i))==0
                    if r(i)==0
                        if c(i)~=0
                            V(s(i))=c(i);
                        end
                    else
                        V(s(i))=r(i);
                    end
                end
                
                if V(s(i))~=0
                    r2(i) = r(i)/abs(V(s(i))) + max(0,-V(s(i))/abs(V(s(i))));
                    c2(i) = c(i)/abs(V(s(i))) + max(0,-V(s(i))/abs(V(s(i))));
                end
                
                deltaI(i) = r2(i) - Qr(s(i),a(i)) ;
                Qr(s(i),a(i)) =  Qr(s(i),a(i))   + alphaQf * deltaI(i);
                
                deltaC(i) = c2(i) - Qr(s(i),3-a(i)) ;
                Qr(s(i),3-a(i)) =  Qr(s(i),3-a(i))   + alphaQc * deltaC(i);
                
            end
            
            
        elseif model==4 % range adaptation
            
            if s(i) < 5
                
                if c(i)> Rmax(s(i))
                    if r(i)>c(i)
                        Rmax(s(i)) = Rmax(s(i)) + alphaV * (r(i) - Rmax(s(i)));
                    else
                        Rmax(s(i)) = Rmax(s(i)) + alphaV * (c(i) - Rmax(s(i)));
                    end
                elseif r(i)> Rmax(s(i))
                    Rmax(s(i)) = Rmax(s(i)) + alphaV * (r(i) - Rmax(s(i)));
                end
                
                if c(i)< Rmin(s(i))
                    if r(i)<c(i)
                        Rmin(s(i)) = Rmin(s(i)) + alphaV * (r(i) - Rmin(s(i)));
                    else
                        Rmin(s(i)) = Rmin(s(i)) + alphaV * (c(i) - Rmin(s(i)));
                    end
                elseif r(i)< Rmin(s(i))
                    Rmin(s(i)) = Rmin(s(i)) + alphaV * (r(i) - Rmin(s(i)));
                end
                
                V(s(i),i:end) = Rmax(s(i))-Rmin(s(i));
                
                r2(i) = (r(i))/(1+Rmax(s(i)));
                
                deltaI(i) =  r2(i) - Q(s(i),a(i)) ;
                Q(s(i),a(i))   = Q(s(i),a(i))   + alphaQf * deltaI(i);
                
                c2(i) = (c(i))/(1+Rmax(s(i)));
                
                deltaC(i) =  c2(i) - Q(s(i),3-a(i)) ;
                Q(s(i),3-a(i))   = Q(s(i),3-a(i))   + alphaQc * deltaC(i);
                
            end
        end
    end
end

%% Priors on parameters

if optsim == 1
    
    lik = -lik;
    
    % Prior penalization
    
    beta    = params(1); % choice temperature
    alphaQf = params(2); % alpha update Q-values
    alphaQc = params(3); % alpha update Q-values
    alphaV  = params(4); % alpha update context value
    w       = params(5); % weighting parameter
    exputi  = params(6); % utility parameter freed (to mimic hybrid)
    forget = params(7); % Forgetting parameter

    
    % the parameters are distrubution with mean + variance (different shapes) (?)
    pbeta       = log(gampdf(beta,1.2,5));
    palphaQf    = log(betapdf(alphaQf,1.1,1.1));
    palphaQc    = log(betapdf(alphaQc,1.1,1.1));
    palphaV     = log(betapdf(alphaV,1.1,1.1));
    pw          = log(betapdf(w,1.1,1.1));
    pexputi     = log(betapdf(exputi,1.1,1.1));
    pforget     = log(betapdf(forget,1.1,1.1));

    if model == 1       %
%         p = [pbeta palphaQf palphaQc];
         p = [pbeta palphaQf pexputi pforget];
    elseif model == 2   %
        p = [pbeta palphaQf palphaQc];
    elseif model == 3   %
        p = [pbeta palphaQf palphaQc pw];
    elseif model == 4   %
        p = [pbeta palphaQf palphaQc palphaV];
    end
    
    p = -sum(p);
    
    lik = p + lik;
    
end

end
