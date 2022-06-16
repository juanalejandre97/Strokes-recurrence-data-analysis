%% Ajuste

%% HISTOGRAMA SIMPLE AJUSTADO A EXPONENCIAL%
excel = 'TiempoIngresos.xlsx';
tiempoingreso = xlsread(excel,'A01:A1409');
histfit(tiempoingreso,14,'exponential')
hold on 
%f = @(x) 281*exp(-x/1000);
%f = @(x) 768*exp(-x/1000);  MEJOR CUADRADA
f = @(x) 768*exp(-x/700);
t = linspace(1,6000,6000);
plot(t,f(t),'g')


%% CURVA DE EXTREMOS DEL HISTOGRAMA
excel1 = 'funciondistribsegunda.xlsx';
%rangotiempo = 'A02:A927';
%rangoprob = 'C02:C927';
rangotiempo = 'A02:A1069';
rangoprob = 'C02:C1069';

tiemporep = xlsread(excel1,rangotiempo);
probabi = xlsread(excel1,rangoprob);

% Primera representacion %
a = plot(tiemporep,probabi);

%% REPRESENTACION
excel2 = 'dataframerepres.xlsx';
rangotiempo = 'A02:A927';
rangosr = 'B02:B927';
rangohiper = 'C02:C927';
rangodiab = 'D02:D927';
rangofa = 'E02:E927';
rangohlipi = 'F02:F927';
rangofum = 'G02:G927';

tiemporep = xlsread(excel2,rangotiempo);
sr = xlsread(excel2,rangosr);
hiper = xlsread(excel2,rangohiper);
diab = xlsread(excel2,rangodiab);
fa = xlsread(excel2,rangofa);
hlipi = xlsread(excel2,rangohlipi);
fum = xlsread(excel2,rangofum);

figure;
plot (tiemporep,sr,'rs','MarkerSize',6)
%plot (tiemporep,sr,'--gs',...
    %'LineWidth',2,...
    %'MarkerSize',10,...
    %'MarkerEdgeColor','b',...
    %'MarkerFaceColor',[0.5,0.5,0.5])
hold on
plot (tiemporep,hiper,'bs','MarkerSize',9)
plot (tiemporep,diab,'gs','MarkerSize',9)
plot (tiemporep,fa,'ks','MarkerSize',9)
plot (tiemporep,hlipi,'cs','MarkerSize',9)
plot (tiemporep,fum,'ms','MarkerSize',9)
%hold on
%plot(energia,yfit,'b-')
%errorbar(energia,permbanda1,erry,'ko')
%errxpos = 1;
%errxneg = 1;
%errorbar(energia,qambagua,errxpos,errxneg,'horizontal','ko')
title('\fontsize{20} \bf \color{blue} Función de distribución empírica')
subtitle('\fontsize{15} \bf \color{blue} Comparativa con factores de riesgo')
axis tight;
legend( '\fontsize{15} Sin factores de riesgo','\fontsize{15} Hipertensión', '\fontsize{15} Diabetes','\fontsize{15} Fibrilación auricular','\fontsize{15} Hiperlipidemia','\fontsize{15} Tabaquismo', 'Location', 'Southeast')
ax = gca;
ax.FontSize = 14;

xlabel('\fontsize{16} Tiempo de repetición (días)');
ylabel('\fontsize{16} Probabilidad acumulada');
