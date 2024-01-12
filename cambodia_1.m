v_1 = csvread('1V_1_cambodia.csv');
v_2 = csvread('2V_1_cambodia.csv');
v_3 = csvread('3V_1_cambodia.csv');
v_4 = csvread('4V_1_cambodia.csv');
v_5 = csvread('5V_1_cambodia.csv');
v_6 = csvread('6V_1_cambodia.csv');
v_7 = csvread('7V_1_cambodia.csv');
v_8 = csvread('8V_1_cambodia.csv');
v_9 = csvread('9V_1_cambodia.csv');
v_10 = csvread('10V_1_cambodia.csv');

v_0 = [zeros(90,30)];
v_1 = v_1(2:91,2:31);
v_2 = v_2(2:91,2:31);
v_3 = v_3(2:91,2:31);
v_4 = v_4(2:91,2:31);
v_5 = v_5(2:91,2:31);
v_6 = v_6(2:91,2:31);
v_7 = v_7(2:91,2:31);
v_8 = v_8(2:91,2:31);
v_9 = v_9(2:91,2:31);
v_10 = v_10(:,2:31);

X = 1 : 90;
T = 1990 : 1 : 2019;
[T, X] = meshgrid(T, X);


 surf(X, T, v_0)
 hold on
 pause
 surf(X, T, v_1)
 pause
 surf(X, T, v_2)
 pause
 surf(X, T, v_3)
 pause
 surf(X, T, v_4)
 pause
 surf(X, T, v_5)
 pause
 surf(X, T, v_6)
 pause
 surf(X, T, v_7)
 pause
 surf(X, T, v_8)
 pause
 surf(X, T, v_9)
 hold off
% pause
% surf(X, T, v_10)