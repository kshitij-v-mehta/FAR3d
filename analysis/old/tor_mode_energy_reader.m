clear;
disp('Select file');
[FILENAME,PATH]=uigetfile('*','Select file','*')
fname=[PATH FILENAME];
T= readtable(fname,'ReadVariableNames',true,'PreserveVariableNames',true);
%% 
%tauA = 6.0/1.269e+7;     %vA0/R0 - ITER Baseline case
tauA = 2.4249e-07;     %vA0/R0 - ITER Steady State case
time = T.(1)*tauA;
% Can add or remove eke's here and in the semilogy plot call depending on how
% many toroidal mode numbers are in the FAR3d simulation (this is set up
% for 20 toroidal modes + n = 0)
eke_0 = T.(2);
eke_1 = T.(14);
eke_2 = T.(3);
eke_3 = T.(4);
eke_4 = T.(5);
eke_5 = T.(6);
eke_6 = T.(7);
eke_7 = T.(8);
eke_8 = T.(9);
eke_9 = T.(10);
eke_10 = T.(11);
eke_11 = T.(12);
eke_12 = T.(13);
eke_13 = T.(15);
eke_14 = T.(16);
eke_15 = T.(17);
semilogy(time,eke_0,time,eke_1,time,eke_2,time,eke_3,time,eke_4,time,eke_5,time,eke_6,time,eke_7,time,eke_8,time,eke_9,time,eke_10,time,eke_11,time,eke_12,time,eke_13,time,eke_14,time,eke_15,'LineWidth',2);
 legend('n = 0','n =1','n = 2','n = 3','n = 4','n = 5','n = 6','n = 7','n = 8','n = 9','n = 10','n = 11','n = 12','n = 13','n = 14','n = 15');
 ylabel('Volume integrated Magnetic Energy','Fontsize',20,'FontWeight','bold');
 xlabel('time (sec)','Fontsize',20,'FontWeight','bold');
 ax=gca;
 ax.FontSize = 16;
 time_end = time(end)
 %xlim([0 1.2*time_end]);