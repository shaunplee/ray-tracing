# ray-tracing
An attempt at Ray Tracing in One Weekend (https://raytracing.github.io/books/RayTracingInOneWeekend.html) using beginner Haskell.

![Final image](./finalimage.png)

    time stack run > finalimage.ppm
    Rendering row 400 of 400
    Done.
    ________________________________________________________
    Executed in   21.52 mins   fish           external
       usr time  1330.13 secs  150.00 micros  1330.13 secs
       sys time   80.66 secs  975.00 micros   80.66 secs

with -O2:

    time stack run > testrandombigger2.ppm
    Rendering row 400 of 400
    Done.
    ________________________________________________________
    Executed in   17.51 mins   fish           external
       usr time  1117.18 secs  142.00 micros  1117.18 secs
       sys time   46.12 secs  780.00 micros   46.12 secs

with -fexcess-precision

    time stack run > testrandombiggerfexcessprecision.ppm
    Rendering row 400 of 400
    Done.

    ________________________________________________________
    Executed in   17.48 mins   fish           external
       usr time  1111.92 secs  132.00 micros  1111.92 secs
       sys time   54.15 secs  761.00 micros   54.15 secs

with -optc-O3

    time stack run > testrandombiggerfexcessprecisionO3.ppm
    Rendering row 400 of 400
    Done.

    ________________________________________________________
    Executed in   16.70 mins   fish           external
       usr time  1062.45 secs  134.00 micros  1062.45 secs
       sys time   45.71 secs  873.00 micros   45.71 secs

with -optc-ffast-math

    time stack run > testrandombiggerfexcessprecisionO3ffastmath.ppm
    Rendering row 400 of 400
    Done.

    ________________________________________________________
    Executed in   16.72 mins   fish           external
       usr time  1070.17 secs  146.00 micros  1070.17 secs
       sys time   46.30 secs  996.00 micros   46.30 secs

with -optc-ffast-math, without -fexcess-precision

    time stack run > testrandombiggerO3ffastmath.ppm
    Rendering row 400 of 400
    Done.

    ________________________________________________________
    Executed in   17.32 mins   fish           external
       usr time  1092.28 secs   93.33 millis  1092.19 secs
       sys time   46.46 secs   14.31 millis   46.45 secs

destructuring:
    time stack run > testrandomDestructured.ppm
    Rendering row 400 of 400
    Done.

    ________________________________________________________
    Executed in   19.41 mins   fish           external
       usr time  1141.58 secs  148.00 micros  1141.58 secs
       sys time   29.44 secs  1105.00 micros   29.44 secs

destructuring and ST:
    time stack run > testrandomSTDetypeclass.ppm
    Rendering row 400 of 400
    Done.

    ________________________________________________________
    Executed in   17.69 mins   fish           external
       usr time  1162.89 secs  123.00 micros  1162.89 secs
       sys time   38.63 secs  794.00 micros   38.63 secs
