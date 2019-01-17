#Code for Bisection method. Find the 95-quantile of t dist with 5 dof

xl_0 = 1.291
xr_0 = 2.582

while(xr_0 - xl_0 >10^-4){
  xm_0 = (xl_0+xr_0)/2.
  if(pt(xm_0,5)>0.95){
    xr_0 = xm_0
  }
  else{
    xl_0 = xm_0
  }
}

print(xm_0) 