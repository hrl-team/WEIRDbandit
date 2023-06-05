function sqds=riskmodelRecovery(params,c)
% decision problems in the explicit task 

uti=(params(1));
bet=(params(2));

%the experiment uses conditions, here's how conditions and decision pairs
%match (8 decision pairs in total): 

%Big magnitude on the right, small magnitude on the left

p10=[.75 .25 .75 .25 .25 .50 .75 1.0]; % big magnitude option
p1 =[.75 .25 .25 .75 1.0 1.0 1.0 1.0]; % small magnitude option 
 


%% how the simulations were working 

% P(m,n)=1/(1+exp((p1(m)-u10*p10(m))*bet(n))); % softmax choice probability of choosing the big mag opt
% C(m,n)=(P(m,n)>rand)+1; % binary choice of picking of picking the high mag

%%

u10=10*uti;

V(:,1)=p1;

V(:,2)=p10.*u10;



sqds=0;

for m=1:8     % loop over the decision problems
    
    sqds=sqds + bet * V(m,c(m)) - log(sum(exp(bet * V(m,:))));          % the likelihood of the choice

 %   phigh(m)=1/(1+exp((V(m,1)-V(m,2))*bet)); % generate prob high magn
    
  %  sqds=sqds +    (phigh(m) - (c(m)-1))^2;   % square distance between the two
end
sqds=-sqds;
% [phigh' c];
% [uti bet];

end
