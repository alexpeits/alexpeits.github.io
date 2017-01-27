date: 2017/01/24
title: Introducing saguine

# Introducing saguine


[Saguine][saguine-git] is a very simple static content generator. All it does for now is create a
structure of depth 1 (meaning either a heading that leads to a page, or a heading that leads to a
list of pages).

It can also render code!

    ::c
    float Q_rsqrt( float number )
    {
        long i;
        float x2, y;
        const float threehalfs = 1.5F;

        x2 = number * 0.5F;
        y  = number;
        i  = * ( long * ) &y;                       // evil floating point bit level hacking
        i  = 0x5f3759df - ( i >> 1 );               // what the fuck? 
        y  = * ( float * ) &i;
        y  = y * ( threehalfs - ( x2 * y * y ) );   // 1st iteration
    //	y  = y * ( threehalfs - ( x2 * y * y ) );   // 2nd iteration, this can be removed

        return y;
    }


[saguine-git]: https://github.com/alexpeits/saguine
