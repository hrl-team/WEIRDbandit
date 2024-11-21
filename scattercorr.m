function [ RHO PVAL b stats ] = scatterCorr( X, Y, stat, markersize)

%Scatter Plot + corr (coef + pValue)

% Corr
[RHO,PVAL] = corr(X,Y,'type','Spearman');

% RobustFit
[b,stats] = robustfit(X, Y);

% Text variable
if stat==1
    test=['R= ' num2str(RHO) ' / P= ' num2str(PVAL)];
elseif stat==2
    test=['RobustFit pval= ' num2str(stats.p(2,1))];
else
    test=['no test performed']
end


% Scatter
plot(X, Y,'ko','Linestyle','none',...
    'MarkerFaceColor',[.5 .5 .5],...
    'MarkerSize',10)
% title(Title,...
%     'FontSize',12)


P = polyfit(X,Y,1);
Yf = polyval(P,X);
hold on
plot(X,Yf,'k','Linewidth',3);
% Text position
xPoint=xlim;
yPoint=ylim;
x=(xPoint(1,1)+xPoint(1,2))/2;
 y=(yPoint(1,1)+yPoint(1,2))/2;
%x=.75;
%y=.75;
text(x,y,test,'BackgroundColor',[1 1 1],...
    'FontSize',16);
set(gca,'FontSize',16)
% ylim([min(Y) max(Y)]);
% xlim([min(X) max(X)]);
end