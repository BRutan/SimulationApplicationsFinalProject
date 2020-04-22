####################
# Generator.R
####################
# LCNG function proposed in "Efficient and Portable Combined Random Number
# Generators" (L'Ecuyer 1988).

gen.env = new.env();
gen.env$a_1 = 40014;
gen.env$a_2 = 40692;
gen.env$q_1 = 53668;
gen.env$q_2 = 52774;
gen.env$r_1 = 12211;
gen.env$r_2 = 3791;
gen.env$m_1 = 2147483563;
gen.env$m_2 = 2147483399;

number_generator_single = function(seed_1, seed_2)
{
    k = seed_1 %/% gen.env$q_1;
    seed_1 = gen.env$a_1 * (seed_1 - k * gen.env$q_1) - k * gen.env$r_1;
    if (seed_1 < 0) {
        seed_1 = seed_1 + gen.env$m_1;
    }
    k = seed_2 %/% gen.env$q_2;
    seed_2 = gen.env$a_2 * (seed_2 - k * gen.env$q_2) - k * gen.env$r_2;
    if (seed_2 < 0) {
        seed_2 = seed_2 + gen.env$m_2;
    }
    z = seed_1 - seed_2;
    if (z < 1) {
        z = z + gen.env$m_1 - 1;
    }
    out = z * 4.656613E-10
    return(list("number" = out, "seed_1" = seed_1, "seed_2" = seed_2));
}

number_generator = function(n, seed_1 = as.numeric(Sys.time()), seed_2 = as.numeric(Sys.time()) - 100, numvars = 1)
{
    # Inputs:
    # * n: Number of elements to return.
    # Optional:
    # * seed_1, seed_2: Starting seeds for generation process. Uses system clock if not specified.
    # by default.
    # * numvars: Number of random variates to return per element.
    i = 1;
    generated = numeric(n * numvars);
    while (i <= n)
    {
        k = seed_1 %/% gen.env$q_1;
        seed_1 = gen.env$a_1 * (seed_1 - k * gen.env$q_1) - k * gen.env$r_1;
        if (seed_1 < 0) {
            seed_1 = seed_1 + gen.env$m_1;
        }
        k = seed_2 %/% gen.env$q_2;
        seed_2 = gen.env$a_2 * (seed_2 - k * gen.env$q_2) - k * gen.env$r_2;
        if (seed_2 < 0) {
            seed_2 = seed_2 + gen.env$m_2;
        }
        z = seed_1 - seed_2;
        if (z < 1) {
            z = z + gen.env$m_1 - 1;
        }
        generated[i] = z * 4.656613E-10;       
        i = i + 1;
    }
    return(list("rands" = generated, "seed_1" = seed_1, "seed_2" = seed_2));
}

number_array_generator = function(dims, seed_1 = as.numeric(Sys.time()), seed_2 = -as.numeric(Sys.time()), numvars = 1)
{
    if (numvars > 1)
    {
        dims[2] = dims[2] * numvars;
    }
    generated = array(dim = dims);
    row = 1;
    while (row <= dims[1])
    {
        results = number_generator(dims[2], seed_1, seed_2);
        seed_1 = results$seed_1;
        seed_2 = results$seed_2;
        generated[row, 1:dims[2]] = results$rands[1:dims[2]];
        row = row + 1;
    }
    return(list("rands" = generated, "seed_1" = seed_1, "seed_2" = seed_2));
}

gen_var = function(seed_1, seed_2, numvars) {
    num = 1;
    generated = vector(length = numvars);
    while (num <= numvars) {
        k = seed_1 %/% gen.env$q_1;
        seed_1 = gen.env$a_1 * (seed_1 - k * gen.env$q_1) - k * gen.env$r_1;
        if (seed_1 < 0) {
            seed_1 = seed_1 + gen.env$m_1;
        }
        k = seed_2 %/% gen.env$q_2;
        seed_2 = gen.env$a_2 * (seed_2 - k * gen.env$q_2) - k * gen.env$r_2;
        if (seed_2 < 0) {
            seed_2 = seed_2 + gen.env$m_2;
        }
        z = seed_1 - seed_2;
        if (z < 1) {
            z = z + gen.env$m_1 - 1;
        }
        generated[num] = z * 4.656613E-10;
        num = num + 1;
    }
    return(list("generated" = generated, "seed_1" = seed_1, "seed_2" = seed_2));
}