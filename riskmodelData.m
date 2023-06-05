function lik=riskmodelData(params,c)
% decision problems in the explicit task 

uti=(params(1));
bet=(params(2));


%so we're gonna change it to something that makes more sense (in terms on
%how conditions are ordered)

tp1 =[.75 .25 .25 .75 1.0 1.0 1.0 1.0]; % small magnitude option 
tp10=[.75 .25 .75 .25 .25 .50 .75 1.0]; % big magnitude option

p1=[repmat(0.75,1,4) repmat(0.25,1,4) repmat(0.25,1,4) repmat(0.75,1,4) repmat(1,1,4) repmat(1,1,4) repmat(1,1,4) repmat(1,1,4)];
p10=[repmat(0.75,1,4) repmat(0.25,1,4) repmat(0.75,1,4) repmat(0.25,1,4) repmat(0.25,1,4) repmat(0.5,1,4) repmat(0.75,1,4) repmat(1,1,4)];
% 
% p10=repmat(tp10,1,4);
% p1=repmat(tp1,1,4);

%% how the simulations were working 

% P(m,n)=1/(1+exp((p1(m)-u10*p10(m))*bet(n))); % softmax choice probability of choosing the big mag opt
% C(m,n)=(P(m,n)>rand)+1; % binary choice of picking of picking the high mag

%%

u10=10*uti;

V(:,1)=p1;

V(:,2)=p10.*u10;



lik=0;

for m=1:32     % loop over the decision problems
    
    lik=lik + bet * V(m,c(m)) - log(sum(exp(bet * V(m,:))));          % the likelihood of the choice

 %   phigh(m)=1/(1+exp((V(m,1)-V(m,2))*bet)); % generate prob high magn
    
  %  sqds=sqds +    (phigh(m) - (c(m)-1))^2;   % square distance between the two
end
lik=-lik;
% [phigh' c];
% [uti bet];

end
