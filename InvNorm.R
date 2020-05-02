####################
# InvNorm.R
####################
# Inverse normal distribution function approximation using inverse transform method
# detailed in Glasserman.

a = c(2.50662823884,-18.61500062529,41.39119773534,-25.44106049637);
b = c(-8.47351093090,23.08336743743,-21.06224101826,3.13082909833);
c_ = c(0.3374754822726147,0.9761690190917186,0.1607979714918209,0.0276438810333863,0.0038405729373609,0.0003951896511919,0.0000321767881768,0.0000002888167364,0.0000003960315187);

inv_norm = function(unif, mu = 0, var = 1)
{
    out = 0;
    y = unif - .5;
    if (abs(y) < .42)
    {
        r = y * y;
        out = y * (((a[4] * r + a[3]) * r + a[2]) * r + a[1]) / ((((b[4] * r + b[3]) * r + b[2]) * r + b[1]) * r + 1);
    }
    else
    {
        r = unif;
        if (y > 0) r = 1 - unif;
        r = log(-log(r));
        out = c_[1] + r * (c_[2] + r * (c_[3] + r * (c_[4] + r * (c_[5]
        + r * (c_[6] + r * (c_[7] + r * (c_[8] + r * c_[9])))))));
        if (y < 0) out = -out;
    }
    return(out * sqrt(var) + mu);
}