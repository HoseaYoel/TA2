library(readxl)
library(writexl)

# read data
data = read_excel("Proccessed2 Cambodia - Copy.xlsx", sheet = "PSD c")
data = data.frame(data)
loop0 = read_excel("Proccessed2 Cambodia - Copy.xlsx", sheet = "qx (tau) c")
loop0 = loop0[,-1]
loop0 = as.matrix.data.frame(loop0)
loop0 = loop0[1:99,1:40]
# -------------------------------------------------------------------

# k|qx
# k = 1, i = x, j = t

loop1 = matrix(1, nrow = 99, ncol = 40)

for (i in 1: 99){
  for (j in 1:40){
    loop1[i,j] = data[99*(j-1)+i,5] * data[99*j+i+1,4]
  }
}

# k = 2, i = x, j = t

loop2 = matrix(1, nrow = 99, ncol = 40)

for (i in 1: 99){
  for (j in 1:40){
    loop2[i,j] = data[99*(j-1)+i,5] * data[99*j+i+1,5] * data[99*(j+1)+i+2,4]
  }
}

# k = 3, i = x, j = t

loop3 = matrix(1, nrow = 99, ncol = 40)

for (i in 1: 99){
  for (j in 1:40){
    loop3[i,j] = data[99*(j-1)+i,5] * data[99*j+i+1,5] * data[99*(j+1)+i+2,5] * data[99*(j+2)+i+3,4]
  }
}

# k = 4, i = x, j = t

loop4 = matrix(1, nrow = 99, ncol = 40)

for (i in 1: 99){
  for (j in 1:40){
    loop4[i,j] = data[99*(j-1)+i,5] * data[99*j+i+1,5] * data[99*(j+1)+i+2,5] * data[99*(j+2)+i+3,5] * data[99*(j+3)+i+4,4]
  }
}

# k = 5, i = x, j = t

loop5 = matrix(1, nrow = 99, ncol = 40)

for (i in 1: 99){
  for (j in 1:40){
    loop5[i,j] = data[99*(j-1)+i,5] * data[99*j+i+1,5] * data[99*(j+1)+i+2,5] * data[99*(j+2)+i+3,5] * data[99*(j+3)+i+4,5] * data[99*(j+4)+i+5,4]
  }
}

# k = 6, i = x, j = t

loop6 = matrix(1, nrow = 99, ncol = 40)

for (i in 1: 99){
  for (j in 1:40){
    loop6[i,j] = data[99*(j-1)+i,5] * data[99*j+i+1,5] * data[99*(j+1)+i+2,5] * data[99*(j+2)+i+3,5] * data[99*(j+3)+i+4,5] * data[99*(j+4)+i+5,5] * data[99*(j+5)+i+6,4]
  }
}

# k = 7, i = x, j = t

loop7 = matrix(1, nrow = 99, ncol = 40)

for (i in 1: 99){
  for (j in 1:40){
    loop7[i,j] = data[99*(j-1)+i,5] * data[99*j+i+1,5] * data[99*(j+1)+i+2,5] * data[99*(j+2)+i+3,5] * data[99*(j+3)+i+4,5] * data[99*(j+4)+i+5,5] * data[99*(j+5)+i+6,5] * data[99*(j+6)+i+7,4]
  }
}

# k = 8, i = x, j = t

loop8 = matrix(1, nrow = 99, ncol = 40)

for (i in 1: 99){
  for (j in 1:40){
    loop8[i,j] = data[99*(j-1)+i,5] * data[99*j+i+1,5] * data[99*(j+1)+i+2,5] * data[99*(j+2)+i+3,5] * data[99*(j+3)+i+4,5] * data[99*(j+4)+i+5,5] * data[99*(j+5)+i+6,5] * data[99*(j+6)+i+7,5] * data[99*(j+7)+i+8,4]
  }
}

# k = 9, i = x, j = t

loop9 = matrix(1, nrow = 99, ncol = 40)

for (i in 1: 99){
  for (j in 1:40){
    loop9[i,j] = data[99*(j-1)+i,5] * data[99*j+i+1,5] * data[99*(j+1)+i+2,5] * data[99*(j+2)+i+3,5] * data[99*(j+3)+i+4,5] * data[99*(j+4)+i+5,5] * data[99*(j+5)+i+6,5] * data[99*(j+6)+i+7,5] * data[99*(j+7)+i+8,5] * data[99*(j+8)+i+9,4]
  }
}

# k = 10, i = x, j = t

loop10 = matrix(1, nrow = 99, ncol = 40)

for (i in 1: 99){
  for (j in 1:40){
    loop10[i,j] = data[99*(j-1)+i,5] * data[99*j+i+1,5] * data[99*(j+1)+i+2,5] * data[99*(j+2)+i+3,5] * data[99*(j+3)+i+4,5] * data[99*(j+4)+i+5,5] * data[99*(j+5)+i+6,5] * data[99*(j+6)+i+7,5] * data[99*(j+7)+i+8,5] * data[99*(j+8)+i+9,5] * data[99*(j+9)+i+10,4]
  }
}

colnames(loop1) = c(1990:2029)
colnames(loop2) = c(1990:2029)
colnames(loop3) = c(1990:2029)
colnames(loop4) = c(1990:2029)
colnames(loop5) = c(1990:2029)
colnames(loop6) = c(1990:2029)
colnames(loop7) = c(1990:2029)
colnames(loop8) = c(1990:2029)
colnames(loop9) = c(1990:2029)
colnames(loop10) = c(1990:2029)

v = 1.06^(-1)
Axn = v*loop0 + v^2*loop1 + v^3*loop2 + v^4*loop3 + v^5*loop4 + v^6*loop5 + v^7*loop6 + v^8*loop7 + v^9*loop8 + v^10*loop9 
colnames(Axn) = c(1990:2029)
# ---------------------------------------------------------------------------------------------------
# axn

# k = 0, i = x, j = t
px_0 = matrix(1, nrow = 99, ncol = 40)

# k = 1, i = x, j = t

px_1 = matrix(1, nrow = 99, ncol = 40)

for (i in 1: 99){
  for (j in 1:40){
    px_1[i,j] = data[99*(j-1)+i,5]
  }
}

# k = 2, i = x, j = t

px_2 = matrix(1, nrow = 99, ncol = 40)

for (i in 1: 99){
  for (j in 1:40){
    px_2[i,j] = data[99*(j-1)+i,5] * data[99*j+i+1,5] 
  }
}

# k = 3, i = x, j = t

px_3 = matrix(1, nrow = 99, ncol = 40)

for (i in 1: 99){
  for (j in 1:40){
    px_3[i,j] = data[99*(j-1)+i,5] * data[99*j+i+1,5] * data[99*(j+1)+i+2,5] 
  }
}

# k = 4, i = x, j = t

px_4 = matrix(1, nrow = 99, ncol = 40)

for (i in 1: 99){
  for (j in 1:40){
    px_4[i,j] = data[99*(j-1)+i,5] * data[99*j+i+1,5] * data[99*(j+1)+i+2,5] * data[99*(j+2)+i+3,5] 
  }
}

# k = 5, i = x, j = t

px_5 = matrix(1, nrow = 99, ncol = 40)

for (i in 1: 99){
  for (j in 1:40){
    px_5[i,j] = data[99*(j-1)+i,5] * data[99*j+i+1,5] * data[99*(j+1)+i+2,5] * data[99*(j+2)+i+3,5] * data[99*(j+3)+i+4,5] 
  }
}

# k = 6, i = x, j = t

px_6 = matrix(1, nrow = 99, ncol = 40)

for (i in 1: 99){
  for (j in 1:40){
    px_6[i,j] = data[99*(j-1)+i,5] * data[99*j+i+1,5] * data[99*(j+1)+i+2,5] * data[99*(j+2)+i+3,5] * data[99*(j+3)+i+4,5] * data[99*(j+4)+i+5,5] 
  }
}

# k = 7, i = x, j = t

px_7 = matrix(1, nrow = 99, ncol = 40)

for (i in 1: 99){
  for (j in 1:40){
    px_7[i,j] = data[99*(j-1)+i,5] * data[99*j+i+1,5] * data[99*(j+1)+i+2,5] * data[99*(j+2)+i+3,5] * data[99*(j+3)+i+4,5] * data[99*(j+4)+i+5,5] * data[99*(j+5)+i+6,5]
  }
}

# k = 8, i = x, j = t

px_8 = matrix(1, nrow = 99, ncol = 40)

for (i in 1: 99){
  for (j in 1:40){
    px_8[i,j] = data[99*(j-1)+i,5] * data[99*j+i+1,5] * data[99*(j+1)+i+2,5] * data[99*(j+2)+i+3,5] * data[99*(j+3)+i+4,5] * data[99*(j+4)+i+5,5] * data[99*(j+5)+i+6,5] * data[99*(j+6)+i+7,5] 
  }
}

# k = 9, i = x, j = t

px_9 = matrix(1, nrow = 99, ncol = 40)

for (i in 1: 99){
  for (j in 1:40){
    px_9[i,j] = data[99*(j-1)+i,5] * data[99*j+i+1,5] * data[99*(j+1)+i+2,5] * data[99*(j+2)+i+3,5] * data[99*(j+3)+i+4,5] * data[99*(j+4)+i+5,5] * data[99*(j+5)+i+6,5] * data[99*(j+6)+i+7,5] * data[99*(j+7)+i+8,5]
  }
}

axn = 1*px_0 + v^1*px_1 + v^2*px_2 + v^3*px_3 + v^4*px_4 + v^5*px_5 + v^6*px_6 + v^7*px_7 + v^8*px_8 + v^9*px_9 
colnames(axn) = c(1990:2029)

premi_tau = Axn/axn

# m = 2
loop0_2 = loop0[2:99,2:40]
loop1_2 = loop1[2:99,2:40]
loop2_2 = loop2[2:99,2:40]
loop3_2 = loop3[2:99,2:40]
loop4_2 = loop4[2:99,2:40]
loop5_2 = loop5[2:99,2:40]
loop6_2 = loop6[2:99,2:40]
loop7_2 = loop7[2:99,2:40]
loop8_2 = loop8[2:99,2:40]

row.names(loop0_2) = c(2:99)
row.names(loop1_2) = c(2:99)
row.names(loop2_2) = c(2:99)
row.names(loop3_2) = c(2:99)
row.names(loop4_2) = c(2:99)
row.names(loop5_2) = c(2:99)
row.names(loop6_2) = c(2:99)
row.names(loop7_2) = c(2:99)
row.names(loop8_2) = c(2:99)

Axn_2 = v*loop0_2 + v^2*loop1_2 + v^3*loop2_2 + v^4*loop3_2 + v^5*loop4_2 + v^6*loop5_2 + v^7*loop6_2 + v^8*loop7_2 + v^9*loop8_2

px_0_2 = px_0[2:99,2:40]
px_1_2 = px_1[2:99,2:40]
px_2_2 = px_2[2:99,2:40]
px_3_2 = px_3[2:99,2:40]
px_4_2 = px_4[2:99,2:40]
px_5_2 = px_5[2:99,2:40]
px_6_2 = px_6[2:99,2:40]
px_7_2 = px_7[2:99,2:40]
px_8_2 = px_8[2:99,2:40]

row.names(px_0_2) = c(2:99)
row.names(px_1_2) = c(2:99)
row.names(px_2_2) = c(2:99)
row.names(px_3_2) = c(2:99)
row.names(px_4_2) = c(2:99)
row.names(px_5_2) = c(2:99)
row.names(px_6_2) = c(2:99)
row.names(px_7_2) = c(2:99)
row.names(px_8_2) = c(2:99)

axn_2 = 1*px_0_2 + v^1*px_1_2 + v^2*px_2_2 + v^3*px_3_2 + v^4*px_4_2 + v^5*px_5_2 + v^6*px_6_2 + v^7*px_7_2 + v^8*px_8_2 
colnames(axn_2) = c(1991:2029)

premi_tau_2 = premi_tau[1:98,1:39]
V_1 = Axn_2 - premi_tau_2 * axn_2

V_1 = as.data.frame(V_1)

write_xlsx(V_1, "D:\\Hosea\\1V.xlsx")

V_1_new = (-1*v * loop0 + premi_tau)/ (v*px_1)

V_1_new = V_1_new[1:98,1:39]
colnames(V_1_new) = c(1991:2029)
row.names(V_1_new) = c(2:99)
V_2_new = (V_1_new + (-1)*v * loop0_2 + premi_tau_2)/ (v*px_1_2)

loop0_3 = loop0[3:99,3:40]
loop1_3 = loop1[3:99,3:40]
loop2_3 = loop2[3:99,3:40]
loop3_3 = loop3[3:99,3:40]
loop4_3 = loop4[3:99,3:40]
loop5_3 = loop5[3:99,3:40]
loop6_3 = loop6[3:99,3:40]
loop7_3 = loop7[3:99,3:40]

row.names(loop0_3) = c(3:99)
row.names(loop1_3) = c(3:99)
row.names(loop2_3) = c(3:99)
row.names(loop3_3) = c(3:99)
row.names(loop4_3) = c(3:99)
row.names(loop5_3) = c(3:99)
row.names(loop6_3) = c(3:99)
row.names(loop7_3) = c(3:99)

px_0_3 = px_0[3:99,3:40]
px_1_3 = px_1[3:99,3:40]
px_2_3 = px_2[3:99,3:40]
px_3_3 = px_3[3:99,3:40]
px_4_3 = px_4[3:99,3:40]
px_5_3 = px_5[3:99,3:40]
px_6_3 = px_6[3:99,3:40]
px_7_3 = px_7[3:99,3:40]
px_8_3 = px_8[3:99,3:40]

row.names(px_0_3) = c(3:99)
row.names(px_1_3) = c(3:99)
row.names(px_2_3) = c(3:99)
row.names(px_3_3) = c(3:99)
row.names(px_4_3) = c(3:99)
row.names(px_5_3) = c(3:99)
row.names(px_6_3) = c(3:99)
row.names(px_7_3) = c(3:99)
row.names(px_8_3) = c(3:99)

Axn_3 = v*loop0_3 + v^2*loop1_3 + v^3*loop2_3 + v^4*loop3_3 + v^5*loop4_3 + v^6*loop5_3 + v^7*loop6_3 + v^8*loop7_3
axn_3 = 1*px_0_3 + v^1*px_1_3 + v^2*px_2_3 + v^3*px_3_3 + v^4*px_4_3 + v^5*px_5_3 + v^6*px_6_3 + v^7*px_7_3
colnames(axn_3) = c(1992:2029)

premi_tau_3 = premi_tau[1:97,1:38]
V_2 = Axn_3 - premi_tau_3 * axn_3

#---------------------------------------------------------
V_2_new = V_2_new[1:97,1:38]
colnames(V_2_new) = c(1992:2029)
row.names(V_2_new) = c(3:99)

V_3_new = (V_2_new + (-1)*v * loop0_3 + premi_tau_3)/ (v*px_1_3)
V_3_new = V_3_new[1:96,1:37]
colnames(V_3_new) = c(1993:2029)
row.names(V_3_new) = c(4:99)

loop0_4 = loop0[4:99,4:40]
row.names(loop0_4) = c(4:99)
premi_tau_4 = premi_tau[1:96,1:37]
px_1_4 = px_1[4:99,4:40]

V_4_new = (V_3_new + (-1)*v * loop0_4 + premi_tau_4)/ (v*px_1_4)
V_4_new = V_4_new[1:95,1:36]
colnames(V_4_new) = c(1994:2029)
row.names(V_4_new) = c(5:99)

loop0_5 = loop0[5:99,5:40]
row.names(loop0_5) = c(5:99)
premi_tau_5 = premi_tau[1:95,1:36]
px_1_5 = px_1[5:99,5:40]

V_5_new = (V_4_new + (-1)*v * loop0_5 + premi_tau_5)/ (v*px_1_5)
V_5_new = V_5_new[1:94,1:35]
colnames(V_5_new) = c(1995:2029)
row.names(V_5_new) = c(6:99)

loop0_6 = loop0[6:99,6:40]
row.names(loop0_6) = c(6:99)
premi_tau_6 = premi_tau[1:94,1:35]
px_1_6 = px_1[6:99,6:40]

V_6_new = (V_5_new + (-1)*v * loop0_6 + premi_tau_6)/ (v*px_1_6)
V_6_new = V_6_new[1:93,1:34]
colnames(V_6_new) = c(1996:2029)
row.names(V_6_new) = c(7:99)

loop0_7 = loop0[7:99,7:40]
row.names(loop0_7) = c(7:99)
premi_tau_7 = premi_tau[1:93,1:34]
px_1_7 = px_1[7:99,7:40]

V_7_new = (V_6_new + (-1)*v * loop0_7 + premi_tau_7)/ (v*px_1_7)
V_7_new = V_7_new[1:92,1:33]
colnames(V_7_new) = c(1997:2029)
row.names(V_7_new) = c(8:99)

loop0_8 = loop0[8:99,8:40]
row.names(loop0_8) = c(8:99)
premi_tau_8 = premi_tau[1:92,1:33]
px_1_8 = px_1[8:99,8:40]

V_8_new = (V_7_new + (-1)*v * loop0_8 + premi_tau_8)/ (v*px_1_8)
V_8_new = V_8_new[1:91,1:32]
colnames(V_8_new) = c(1998:2029)
row.names(V_8_new) = c(9:99)

loop0_9 = loop0[9:99,9:40]
row.names(loop0_9) = c(9:99)
premi_tau_9 = premi_tau[1:91,1:32]
px_1_9 = px_1[9:99,9:40]

V_9_new = (V_8_new + (-1)*v * loop0_9 + premi_tau_9)/ (v*px_1_9)
V_9_new = V_9_new[1:90,1:31]
colnames(V_9_new) = c(1999:2029)
row.names(V_9_new) = c(10:99)

loop0_10 = loop0[10:99,10:40]
row.names(loop0_10) = c(10:99)
premi_tau_10 = premi_tau[1:90,1:31]
px_1_10 = px_1[10:99,10:40]

V_10_new = (V_9_new + (-1)*v * loop0_10 + premi_tau_10)/ (v*px_1_10)


#-------------------------------------------------------------------------------------------------
# read data
datam = read_excel("Proccessed2 Cambodia - Copy.xlsx", sheet = "PSD c")
datam = data.frame(datam)
loop0_m = read_excel("Proccessed2 Cambodia - Copy.xlsx", sheet = "qx (1) c")
loop0_m = loop0_m[,-1]
loop0_m = as.matrix.data.frame(loop0_m)
loop0_m = loop0_m[1:99,1:40]
# -------------------------
loop0_m_new = c(loop0_m)
datam[,4] = loop0_m_new

V_1_new_m = (-1*v * loop0_m + premi_tau)/ (v*px_1)
V_1_new_m = V_1_new_m[1:98,1:39]
colnames(V_1_new_m) = c(1991:2029)
row.names(V_1_new_m) = c(2:99)

loop0_2_m = loop0_m[2:99,2:40]
row.names(loop0_2_m) = c(2:99)

V_2_new_m = (V_1_new_m + (-1)*v * loop0_2_m + premi_tau_2)/ (v*px_1_2)
V_2_new_m = V_2_new_m[1:97,1:38]
colnames(V_2_new_m) = c(1992:2029)
row.names(V_2_new_m) = c(3:99)

loop0_3_m = loop0_m[3:99,3:40]
row.names(loop0_3_m) = c(3:99)

V_3_new_m = (V_2_new_m + (-1)*v * loop0_3_m + premi_tau_3)/ (v*px_1_3)
V_3_new_m = V_3_new_m[1:96,1:37]
colnames(V_3_new_m) = c(1993:2029)
row.names(V_3_new_m) = c(4:99)

loop0_4_m = loop0_m[4:99,4:40]
row.names(loop0_4_m) = c(4:99)

V_4_new_m = (V_3_new_m + (-1)*v * loop0_4_m + premi_tau_4)/ (v*px_1_4)
V_4_new_m = V_4_new_m[1:95,1:36]
colnames(V_4_new_m) = c(1994:2029)
row.names(V_4_new_m) = c(5:99)

loop0_5_m = loop0_m[5:99,5:40]
row.names(loop0_5_m) = c(5:99)

V_5_new_m = (V_4_new_m + (-1)*v * loop0_5_m + premi_tau_5)/ (v*px_1_5)
V_5_new_m = V_5_new_m[1:94,1:35]
colnames(V_5_new_m) = c(1995:2029)
row.names(V_5_new_m) = c(6:99)

loop0_6_m = loop0_m[6:99,6:40]
row.names(loop0_6_m) = c(6:99)

V_6_new_m = (V_5_new_m + (-1)*v * loop0_6_m + premi_tau_6)/ (v*px_1_6)
V_6_new_m = V_6_new_m[1:93,1:34]
colnames(V_6_new_m) = c(1996:2029)
row.names(V_6_new_m) = c(7:99)

loop0_7_m = loop0_m[7:99,7:40]
row.names(loop0_7_m) = c(7:99)

V_7_new_m = (V_6_new_m + (-1)*v * loop0_7_m + premi_tau_7)/ (v*px_1_7)
V_7_new_m = V_7_new_m[1:92,1:33]
colnames(V_7_new_m) = c(1997:2029)
row.names(V_7_new_m) = c(8:99)

loop0_8_m = loop0_m[8:99,8:40]
row.names(loop0_8_m) = c(8:99)

V_8_new_m = (V_7_new_m + (-1)*v * loop0_8_m + premi_tau_8)/ (v*px_1_8)
V_8_new_m = V_8_new_m[1:91,1:32]
colnames(V_8_new_m) = c(1998:2029)
row.names(V_8_new_m) = c(9:99)

loop0_9_m = loop0_m[9:99,9:40]
row.names(loop0_9_m) = c(9:99)

V_9_new_m = (V_8_new_m + (-1)*v * loop0_9_m + premi_tau_9)/ (v*px_1_9)
V_9_new_m = V_9_new_m[1:90,1:31]
colnames(V_9_new_m) = c(1999:2029)
row.names(V_9_new_m) = c(10:99)

loop0_10_m = loop0_m[10:99,10:40]
row.names(loop0_10_m) = c(10:99)

V_10_new_m = (V_9_new_m + (-1)*v * loop0_10_m + premi_tau_10)/ (v*px_1_10)

#----------------------------
Axn = as.data.frame(Axn)
axn = as.data.frame(axn)
premi_tau = as.data.frame(premi_tau)
V_1_new = as.data.frame(V_1_new)
V_2_new = as.data.frame(V_2_new)
V_3_new = as.data.frame(V_3_new)
V_4_new = as.data.frame(V_4_new)
V_5_new = as.data.frame(V_5_new)
V_6_new = as.data.frame(V_6_new)
V_7_new = as.data.frame(V_7_new)
V_8_new = as.data.frame(V_8_new)
V_9_new = as.data.frame(V_9_new)
V_10_new = as.data.frame(V_10_new)

write_xlsx(Axn, "D:\\Axn tau cambodia.xlsx")
write_xlsx(axn, "D:\\a_xn tau cambodia.xlsx")  
write_xlsx(premi_tau, "D:\\premi tau cambodia.xlsx")
write_xlsx(V_1_new, "D:\\1V tau cambodia.xlsx")
write_xlsx(V_2_new, "D:\\2V tau cambodia.xlsx")
write_xlsx(V_3_new, "D:\\3V tau cambodia.xlsx")
write_xlsx(V_4_new, "D:\\4V tau cambodia.xlsx")
write_xlsx(V_5_new, "D:\\5V tau cambodia.xlsx")
write_xlsx(V_6_new, "D:\\6V tau cambodia.xlsx")
write_xlsx(V_7_new, "D:\\7V tau cambodia.xlsx")
write_xlsx(V_8_new, "D:\\8V tau cambodia.xlsx")
write_xlsx(V_9_new, "D:\\9V tau cambodia.xlsx")
write_xlsx(V_10_new, "D:\\10V tau cambodia.xlsx")
