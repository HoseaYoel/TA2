v_1 = csvread('1V_tau_Angola.csv');
v_2 = csvread('2V_tau_Angola.csv');
v_3 = csvread('3V_tau_Angola.csv');
v_4 = csvread('4V_tau_Angola.csv');
v_5 = csvread('5V_tau_Angola.csv');
v_6 = csvread('6V_tau_Angola.csv');
v_7 = csvread('7V_tau_Angola.csv');
v_8 = csvread('8V_tau_Angola.csv');
v_9 = csvread('9V_tau_Angola.csv');
%v_10 = csvread('10V_1_Nigeria.csv');

X = 1 : 90;
T = 1:10;
[T, X] = meshgrid(T, X);

for j = 1:6
i = j+1+24;
v = [zeros(90,10)];
v_1a = v_1(2:91,i);
v_2a = v_2(2:91,i);
v_3a = v_3(2:91,i);
v_4a = v_4(2:91,i);
v_5a = v_5(2:91,i);
v_6a = v_6(2:91,i);
v_7a = v_7(2:91,i);
v_8a = v_8(2:91,i);
v_9a = v_9(2:91,i);
v_10a = v_10(:,i);

v(:,2) = v_1a;
v(:,3) = v_2a;
v(:,4) = v_3a;
v(:,5) = v_4a;
v(:,6) = v_5a;
v(:,7) = v_6a;
v(:,8) = v_7a;
v(:,9) = v_8a;
v(:,10) = v_9a;

subplot(3,2,j);
surf(X, T, v)
title(j+1989+24)
set(gca,"fontsize", 16)
 end