Program FastFT
    use FastFourierTransform
    implicit none
    
    integer N 
    integer i
    Double Complex :: X(511), XC(511)
    double complex a
    double precision t
    double precision f
    t = 0.5
    N = 511
    do 100 i = 1, N
    f = i
    x(i)%re = sin(f)
    x(i)%im = 0.0
100    end do
    call FFT(1,2,N,t, X)
    
    end program
    