%% Simulations & fitting (RL)
%by Hernan Anllo (subcomarc@gmail.com)
%HEAVILY based on Sophie Bavard's scripts

%requires files: 
% 
% "COMPLETE_model_fitting_simulations.m"
% "WEIRD__CDep_NoExclusions.csv"
% "riskmodelData.m"
% "riskmodelRecovery.m"


clear all %housekeeping
close all
data = readtable("WEIRD__CDep_NoExclusions.csv");
%save("WEIRD__CDep_NoExclusions4mat.mat", "data") %just in case
%load("WEIRDmatlaClean_latest.mat");

%create "raw" data for safety
rawData = data;
%data = rawData; %for quick recovery
data = rawData(rawData.WhichPhase == 1 | rawData.WhichPhase == 2,:); %Learning and transfer together

%SANITY CHECK: are blocks consistent for all participants?
partindex = unique(data.Participant_Private_ID);
whois=[];
countercons=0;
for n = 1:length(partindex)
    Indpart = data(data.Participant_Private_ID == partindex(n),:);
    Indpart = sortrows(Indpart, 'Condition');
    if height(Indpart) == 240 %expected number of total trials
        countercons=countercons+1;
    else 
        whois=[whois,unique(Indpart.Participant_Private_ID)];
    end
end

%if you are ok, countercons=n, and whois=0
data.ChoseGoodorBadRecoded = data.ChoseGoodorBad + 1; %small tweak for sims

%Remember:
% C1 = [0.25,10] vs [0.75,10];
% C2 = [0.25,10] vs [0.75,10];
% C3 = [0.25,1] vs [0.75,1];
% C4 = [0.25,1] vs [0.75,1];
% C5 = [0.75,1] vs [0.75,10];
% C6 = [0.25,1] vs [0.25,10];
% C7 = [0.25,1] vs [0.75,10];
% C8 = [0.75,1] vs [0.25,10];
% C9 = [1,1] vs [0.25,10]; %from here down, explicit "economic" decisions only
% C10 = [1,1] vs [0.5,10];
% C11 = [1,1] vs [0.75,10];
% C12 = [1,1] vs [1,10];

%%%%FITTING AND SIMULATIONS START%%%

%First, build dataframe from real data

meanPfromdataAllcond = varfun(@mean,data,'InputVariables','ChoseGoodorBad',...
       'GroupingVariables','Participant_Private_ID');
meanPfromdataAllcond = renamevars(meanPfromdataAllcond, ['mean_ChoseGoodorBad'], ['meanAllconditions']);
meanPfromdataAllcond = removevars(meanPfromdataAllcond, 'GroupCount');

meanPfromdataAlldec = varfun(@mean,data,'InputVariables','ChoseGoodorBad',...
       'GroupingVariables', 'Condition');
semPfromdataAlldec = varfun(@sem,data,'InputVariables','ChoseGoodorBad',...
       'GroupingVariables', 'Condition');
MeanSemperCond = join(meanPfromdataAlldec, semPfromdataAlldec);

meanPfromdataAllparts = varfun(@mean,data,'InputVariables','ChoseGoodorBad',...
       'GroupingVariables',{'Participant_Private_ID'});
meanPfromdata = varfun(@mean,data,'InputVariables','ChoseGoodorBad',...
       'GroupingVariables',{'Participant_Private_ID', 'Condition'});

%Time to fit the SCALING model (requires file:
%"COMPLETE_model_fitting_simulations.m")

%Fit with utility as a free param 

partindex = unique(data.Participant_Private_ID);
clearvars parameters lik %just in case

options = optimset('Algorithm', 'interior-point', 'Display', 'iter-detailed', 'MaxIter', 1000); % These increase the number of iterations to ensure the convergence

for n = 1:length(partindex)
    Indpart = data(data.Participant_Private_ID == partindex(n),:);
    Indpart = sortrows(Indpart, 'Condition');
      [parameters(n,:),lik(n),~,~,~] =fmincon(@(x) ...
            COMPLETE_model_fitting_simulations(x,table2array(Indpart(:,'Condition')),table2array(Indpart(:,'ChoseGoodorBadRecoded')),table2array(Indpart(:,'RewardGoodorBad')), table2array(Indpart(:,'OtherRewardGoodorBad')),1,1),[0.5 0.5 0.5 0.5 0.5 0.5 0.5],[],[],[],[],[0 0 0 0 0 0 0],[Inf 1 1 1 1 1 1],[], options);
end
% 

parametersSCALINGRL=parameters; %keep the parameters and likelihood


% but let's get BIC and AIC too, for completion

parametersSCALINGRL= [parametersSCALINGRL lik']; 
nfpm=[4];
        bic=-2*-lik+nfpm*log(240); % l2 is already positive
        aic=lik+2*nfpm;% l2 is already positive
parametersSCALINGRL= [parametersSCALINGRL bic' aic']; %liks, bic, aic


%Simulate data using the extracted parameters

partindex = unique(data.Participant_Private_ID);
clearvars parameters lik

PartID=[];
AllConds=[];
Answ=[];
Rew=[];
Rew2=[];
Lik=[];
for n = 1:length(partindex)
    Indpart = data(data.Participant_Private_ID == partindex(n),:);
    Indpart = sortrows(Indpart, 'Condition');
          [lik, a, r, r2, Q, V] = COMPLETE_model_fitting_simulations(parametersSCALINGRL(n,:),table2array(Indpart(:,'Condition')),table2array(Indpart(:,'ChoseGoodorBadRecoded')),table2array(Indpart(:,'RewardGoodorBad')), table2array(Indpart(:,'OtherRewardGoodorBad')),1,2);
       PartID=[PartID; repmat(unique(Indpart.Participant_Private_ID),height(Indpart),1)];
       AllConds=[AllConds; table2array(Indpart(:,'Condition'))];
       Answ=[Answ; a'];
       Rew=[Rew;r];
       Rew2=[Rew2;r2];
       Lik=[Lik;lik];
end
% 


Rew= (Rew ~= 0); %render columns interpretable
Rew2= (Rew2 ~= 0);
AnswRecoded=Answ;
Answ=Answ-1;
SimmedataSCALINGRL = table(PartID, AllConds, Answ, AnswRecoded, Rew, Rew2);
bckpSimmedataSCALINGRL = SimmedataSCALINGRL;

%%% Check recovery for SCALING model
clearvars parameters lik

options = optimset('Algorithm', 'interior-point', 'Display', 'iter-detailed', 'MaxIter', 1000); % These increase the number of iterations to ensure the convergence

for n = 1:length(partindex)
    Indpart = SimmedataSCALINGRL(SimmedataSCALINGRL.PartID == partindex(n),:);
    Indpart = sortrows(Indpart, 'AllConds');
        [parametersRECOVERED3(n,:),lik(n),~,~,~] =fmincon(@(x) ...
        COMPLETE_model_fitting_simulations(x,table2array(Indpart(:,'AllConds')),table2array(Indpart(:,'AnswRecoded')),table2array(Indpart(:,'Rew')), table2array(Indpart(:,'Rew2')),1,1),[0.5 0.5 0.5 0.5 0.5 0.5 0.5],[],[],[],[],[0 0 0 0 0 0 0],[Inf 1 1 1 1 1 1],[], options);

end
% 

% but let's get BIC and AIC too, for completion

parametersRECOVERED3= [parametersRECOVERED3 lik']; 
nfpm=[4];
        bic=-2*-lik+nfpm*log(240); % l2 is already positive
        aic=lik+2*nfpm;% l2 is already positive
parametersRECOVERED3= [parametersRECOVERED3 bic' aic']; %liks, bic, aic


%%%Some quick plots to check parameter recovery

figure;
subplot(1,4,1)
scattercorr(parametersSCALINGRL(:,1),parametersRECOVERED3(:,1),1,1) % Beta
subplot(1,4,2)
% scattercorr(parametersABS3(parametersABS3(:,2)<0.4,2),parametersRECOVERED3(parametersABS3(:,2)<0.4,2),1,1) % Alpha
scattercorr(parametersSCALINGRL(:,2),parametersRECOVERED3(:,2),1,1) % Alpha
subplot(1,4,3)
scattercorr(parametersSCALINGRL(:,6),parametersRECOVERED3(:,6),1,1) % Utility Free
subplot(1,4,4)
scattercorr(parametersSCALINGRL(:,7),parametersRECOVERED3(:,7),1,1) % Forget
title({'Recovery of temperature';'learning rate'; 'utility free'; 'and forgetting parameters'; 'Fitted on learning AND transfer'});

%build DF

meanPfromsim = varfun(@mean,SimmedataSCALINGRL,'InputVariables','Answ',...
       'GroupingVariables',{'AllConds' 'PartID'});
SimmedataSCALINGRL = join(SimmedataSCALINGRL, meanPfromsim(:,[1 2 4]));

globmeanfromsim = varfun(@mean,SimmedataSCALINGRL,'InputVariables','mean_Answ',...
       'GroupingVariables',{'AllConds'});

stdPfromsim = varfun(@sem,SimmedataSCALINGRL,'InputVariables','mean_Answ',...
       'GroupingVariables',{'AllConds'});


%plot and compare

figure

errorbar(1:8,MeanSemperCond.mean_ChoseGoodorBad,MeanSemperCond.sem_ChoseGoodorBad,'Color','k','Linewidth',3,'Markersize',5);
hold on
errorbar([1:8],table2array(globmeanfromsim(:,3)),table2array(stdPfromsim(:,3)),'Color','r','Linewidth',3,'Linestyle','none','Markersize',5);
plot(0:9,repmat(.5,1,10),'k:','Linewidth',3);
axis([0 9 0 1])
legend('data (black)','Absolute+Utiliry as free param (red)');
set(gca,'FontSize',14,...
    'XTickLabel',{'','75vs25(10p)','75vs25(10p)','75vs25(1p)','75vs25(1p)','75(10)vs75(1)','25(10)vs25(1)','75(10)vs25(1)','75(1)vs25(10)',''},...
    'XTick',[0:9]);
xlabel('Decision problems')
ylabel('p(high mag opt)')
title(' + Forgetting parameter. Fitted on learning AND transfer ')
title({'Actual part. behav (black) vs Model';': Abs + Utiliry as free param (red) + Forgetting'; 'Fitted on learning AND transfer'});

%If you did everything right, the recovery and sims should both look awesome


%Save these for before going back to R
%picking each column by hand, for clarity

fittedSCALING=table;
fittedSCALING.PartID=partindex;
fittedSCALING.BetafromUtiFree=parametersSCALINGRL(:,1);
fittedSCALING.AlphafromUtiFree=parametersSCALINGRL(:,2);
fittedSCALING.UtifromUtiFree=parametersSCALINGRL(:,6);
fittedSCALING.ForgetPar=parametersSCALINGRL(:,7);
fittedSCALING.lik=parametersSCALINGRL(:,8);
fittedSCALING.bic=parametersSCALINGRL(:,9);
fittedSCALING.aic=parametersSCALINGRL(:,10);
fittedSCALING.BetafromUtiFreeREC=parametersRECOVERED3(:,1);
fittedSCALING.AlphafromUtiFreeREC=parametersRECOVERED3(:,2);
fittedSCALING.UtifromUtiFreeREC=parametersRECOVERED3(:,6);
fittedSCALING.ForgetParREC=parametersRECOVERED3(:,7);
fittedSCALING.likREC=parametersRECOVERED3(:,8);
fittedSCALING.bicREC=parametersRECOVERED3(:,9);
fittedSCALING.aicREC=parametersRECOVERED3(:,10);

SimmedataSCALINGRL = join(SimmedataSCALINGRL, fittedSCALING);
SimmedataSCALINGRL = renamevars(SimmedataSCALINGRL,["mean_Answ"], ["mean_Ans_Utifree"]);

writetable(SimmedataSCALINGRL, 'SimmedataSCALINGRL.csv')

%% Simulations & fitting (LOTTERY)

% get data
data = rawData(rawData.WhichPhase == 3,:);
data.ChoseGoodorBadRecoded = data.ChoseGoodorBad + 1;


%fitting
%requires: "riskmodelData"
maxbeta=20%to bound the simulation
partindex = unique(data.Participant_Private_ID);

clearvars parameters lik

options = optimset('Algorithm', 'interior-point', 'Display', 'iter-detailed', 'MaxIter', 1000); % These increase the number of iterations to ensure the convergence

% for h = 1:length(initbeta)
for n = 1:length(partindex)
    Indpart = data(data.Participant_Private_ID == partindex(n),:);
    Indpart = sortrows(Indpart, 'Condition');
    [parameters(n,:),lik(n),~,~,~]       = fmincon(@(x) riskmodelData(x,table2array(Indpart(:,'ChoseGoodorBadRecoded'))),[0.5 0.5],[],[],[],[],[0 0],[1 maxbeta],[],options);

end


%Build dataframe

meanPfromdataAllcond = varfun(@mean,data,'InputVariables','ChoseGoodorBad',...
       'GroupingVariables','Participant_Private_ID');
meanPfromdataAllcond = renamevars(meanPfromdataAllcond, ['mean_ChoseGoodorBad'], ['meanAllconditions']);
meanPfromdataAllcond = removevars(meanPfromdataAllcond, 'GroupCount');

meanPfromdataAlldec = varfun(@mean,data,'InputVariables','ChoseGoodorBad',...
       'GroupingVariables', 'Condition');
semPfromdataAlldec = varfun(@sem,data,'InputVariables','ChoseGoodorBad',...
       'GroupingVariables', 'Condition');
MeanSemperCond = join(meanPfromdataAlldec, semPfromdataAlldec);

meanPfromdataAllparts = varfun(@mean,data,'InputVariables','ChoseGoodorBad',...
       'GroupingVariables',{'Participant_Private_ID'});
meanPfromdata = varfun(@mean,data,'InputVariables','ChoseGoodorBad',...
       'GroupingVariables',{'Participant_Private_ID', 'Condition'});


%store information from fitting
explicitParamsPart = table;
explicitParamsPart.Participant_Private_ID = partindex;
explicitParamsPart.Utility = parameters(:,1);
explicitParamsPart.Beta = parameters(:,2); 

ExplMeansparams = join(meanPfromdata, explicitParamsPart);
ExplMeansparams = join(ExplMeansparams, meanPfromdataAllcond);

% now we generate the simulation based in the fitted parameters and we compare the two 

nparts=length(unique(ExplMeansparams.Participant_Private_ID)); %  number of simulations  / nsub
Pdatasim=[];
ActualChoice=[];

% Set the magnitudes and probabilities for simulation

p10=[.75 .25 .75 .25 .25 .50 .75 1.0]; % big magnitude option
p1 =[.75 .25 .25 .75 1.0 1.0 1.0 1.0]; % small magnitude option 



for n=1:nparts    % loop over the simulated agents 
    
    Indpart = ExplMeansparams(ExplMeansparams.Participant_Private_ID == partindex(n),:);
    u10=10*unique(Indpart.Utility); % as you can see we are now using the fitted paramaters 
    
    for m=1:8     % loop over the decision problems 
        
        Pdatasim(m,n)=1/(1+exp((p1(m)-u10*p10(m))*unique(Indpart.Beta))); % softmax choice probability of choosing the big mag opt
        C(m,n)=(Pdatasim(m,n)>rand); % binary choice of picking of picking the high mag
        Cf(m,n)=C(m,n);
        
        % here I generate four choices for each decision problem as in our data for the fitting 
        for ind=1:3 
            Cf(m+ind*8,n)=(Pdatasim(m,n)>rand);
        end
    end
end

%Let's plot and see what happened

Explparamsplotted = figure,
errorbar(1:8,MeanSemperCond.mean_ChoseGoodorBad,MeanSemperCond.sem_ChoseGoodorBad,'Color','k','Linewidth',3,'Markersize',5),
hold on,
errorbar([1:8],mean(Pdatasim'),sem(Pdatasim'),'Color','r','Linewidth',3,'Linestyle','none','Markersize',5),
plot(0:9,repmat(.5,1,10),'k:','Linewidth',3),
axis([0 9 0 1]),
legend('data','simulation','random'),
axis([0 9 0 1]),
legend('data','simulation','random'),


set(gca,'FontSize',14,...
    'XTickLabel',{'','6.25','2.25','7.25','1.75','1.5','4.0','6.5','9.0',''},...
    'XTick',[0:9]),
xlabel('Decision problems'),
ylabel('p(high mag opt)');

%fit on simulated data
%requires: "riskmodelRecovery"

ChoiceDataSim = binornd(1,Pdatasim)+1;

clearvars lik parametersReFitted


options = optimset('Algorithm', 'interior-point', 'Display', 'iter-detailed', 'MaxIter', 1000); % These increase the number of iterations to ensure the convergence

for n=1:nparts 
    [parametersReFitted(n,:),lik(n),~,~,~]       = fmincon(@(x) riskmodelRecovery(x,ChoiceDataSim(:,n)),[0.5 0.5],[],[],[],[],[0 0],[1 maxbeta],[],options);

end

% How good is parameter recovery?

figure;
subplot(1,2,1)
scattercorr(parameters(:,1),parametersReFitted(:,1),1,1) % Utility
subplot(1,2,2)
scattercorr(parameters(:,2),parametersReFitted(:,2),1,1) % Beta

%figure;
%scattercorr(parameters(:,1),parametersReFitted(:,1),1,1) % Beta Maxrel


% integrate all data from Lottery for sending to R
testo=Pdatasim';
SimmedataSCALINGLOT=table;
SimmedataSCALINGLOT.Participant_Private_ID = [explicitParamsPart.Participant_Private_ID;explicitParamsPart.Participant_Private_ID;explicitParamsPart.Participant_Private_ID;explicitParamsPart.Participant_Private_ID;explicitParamsPart.Participant_Private_ID;explicitParamsPart.Participant_Private_ID;explicitParamsPart.Participant_Private_ID;explicitParamsPart.Participant_Private_ID];
SimmedataSCALINGLOT.Condition = [repmat(5,561,1);repmat(6,561,1);repmat(7,561,1);repmat(8,561,1);repmat(9,561,1);repmat(10,561,1);repmat(11,561,1);repmat(12,561,1)];
SimmedataSCALINGLOT.UtilityfromExpl = [explicitParamsPart.Utility;explicitParamsPart.Utility;explicitParamsPart.Utility;explicitParamsPart.Utility;explicitParamsPart.Utility;explicitParamsPart.Utility;explicitParamsPart.Utility;explicitParamsPart.Utility];
SimmedataSCALINGLOT.BetafromExpl = [explicitParamsPart.Beta;explicitParamsPart.Beta;explicitParamsPart.Beta;explicitParamsPart.Beta;explicitParamsPart.Beta;explicitParamsPart.Beta;explicitParamsPart.Beta;explicitParamsPart.Beta];
SimmedataSCALINGLOT.MeanSimmedExpl = [testo(:,1); testo(:,2);testo(:,3);testo(:,4);testo(:,5);testo(:,6);testo(:,7);testo(:,8)];
SimmedataSCALINGLOT.UtilityfromExplREC = [parametersReFitted(:,1);parametersReFitted(:,1);parametersReFitted(:,1);parametersReFitted(:,1);parametersReFitted(:,1);parametersReFitted(:,1);parametersReFitted(:,1);parametersReFitted(:,1)];
SimmedataSCALINGLOT.BetafromExplREC = [parametersReFitted(:,2);parametersReFitted(:,2);parametersReFitted(:,2);parametersReFitted(:,2);parametersReFitted(:,2);parametersReFitted(:,2);parametersReFitted(:,2);parametersReFitted(:,2)];
SimmedataSCALINGLOT.WhichPhase = repmat(3, height(SimmedataSCALINGLOT),1); 

flippedsimchoice=Cf';
LOTSimmChoices=table;
LOTSimmChoices.Participant_Private_ID = repmat(explicitParamsPart.Participant_Private_ID, 32, 1);
LOTSimmChoices.Answers = [flippedsimchoice(:,1); flippedsimchoice(:,2);flippedsimchoice(:,3);flippedsimchoice(:,4);flippedsimchoice(:,5);flippedsimchoice(:,6);flippedsimchoice(:,7);flippedsimchoice(:,8)
flippedsimchoice(:,9); flippedsimchoice(:,10);flippedsimchoice(:,11);flippedsimchoice(:,12);flippedsimchoice(:,13);flippedsimchoice(:,14);flippedsimchoice(:,15);flippedsimchoice(:,16);    
flippedsimchoice(:,17); flippedsimchoice(:,18);flippedsimchoice(:,19);flippedsimchoice(:,20);flippedsimchoice(:,21);flippedsimchoice(:,22);flippedsimchoice(:,23);flippedsimchoice(:,24);
flippedsimchoice(:,25); flippedsimchoice(:,26);flippedsimchoice(:,27);flippedsimchoice(:,28);flippedsimchoice(:,29);flippedsimchoice(:,30);flippedsimchoice(:,31);flippedsimchoice(:,32)];
LOTSimmChoices.Condition = repmat([repmat(5,561,1); repmat(6,561,1); repmat(7,561,1); repmat(8,561,1); repmat(9,561,1); repmat(10,561,1); repmat(11,561,1); repmat(12,561,1)], 4, 1);
LOTSimmChoices.deltaEVfactor = repmat([repmat(6.75,561,1); repmat(2.25,561,1); repmat(7.25,561,1); repmat(1.75,561,1); repmat(1.5,561,1); repmat(4,561,1); repmat(6.5,561,1); repmat(9,561,1)], 4, 1);
LOTSimmChoices.WhichPhase = repmat(3, length(LOTSimmChoices.Answers), 1);

%save the fitting parameters (and recovery) for lottery, and then also save
%simulated choice performance 
writetable(SimmedataSCALINGLOT, 'SimmedataSCALINGLOT.csv')
writetable(LOTSimmChoices, 'LOTSimmChoices.csv')
