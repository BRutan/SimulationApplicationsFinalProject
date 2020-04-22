####################
# BlackScholes.R
####################
# Black-Scholes-Merton function for pricing
# European options.

black_scholes = function(s, k, q, r, t, iv, isCall) {
    d_1 = d_1(s, k, q, r, t, iv);
    d_2 = d_2(s, k, q, r, t, iv);
    val = s * exp(-q * t) * pnorm(d_1) - k * exp(-r * t) * pnorm(d_2);
    if (isCall == FALSE) {
        val = val + k * exp(-r * t) - s * exp(-q * t);
    }
    return(val);
}
vega = function(s, k, q, r, t, iv) {
    return(s * exp(-q * t) * sqrt(t) * dnorm(d_1(s, k, q, r, t, iv)));
}
d_1 = function(s, k, q, r, t, iv) {
    return((log(s / k) + (r - q + iv * iv / 2) * t) / (iv * sqrt(t)));
}
d_2 = function(s, k, q, r, t, iv) {
    return(d_1(s, k, q, r, t, iv) - iv * sqrt(t));
}