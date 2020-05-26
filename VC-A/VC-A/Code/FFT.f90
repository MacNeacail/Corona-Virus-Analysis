    Module FastFourierTransform
    
    contains
    
    Subroutine fourFFT()
    
    
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
    
    end subroutine 
    
    Subroutine FFT(File1, FileOut, N, time, X)
    ! Fortran example.
    ! 1D complex to complex, and real to conjugate-even
    Use MKL_DFTI
    
    implicit none
    integer n
    integer FileOut
    Double Complex :: X(n), XC(N)
    Double precision :: Y(n+2), U,  SUM, DF, W, SUMW, Res
    Double precision :: YStart, time, FFTRatio, A,B,C, phi
    integer i,j, File1,n1
    type(DFTI_DESCRIPTOR), POINTER :: My_Desc1_Handle, My_Desc2_Handle
    Integer :: Status
    
    
    !...put input data into X(1),...,X(32); Y(1),...,Y(32)
    ! Perform a complex to complex transform
    j = 1
    YStart = 0.0d0
    FFTRatio = time
    DF = 1.0d0/(time*dfloat(n))
    sum = 0.0d0
    sumw = 0.0d0
    phi = 0.0d0
    
    do i =1,(N-1)
!        y(i) = real(x(i))
        write(*,*)i,x(i)
   end do
    Status = DftiCreateDescriptor( My_Desc1_Handle, DFTI_DOUBLE,DFTI_COMPLEX, 1, N )
    Status = DftiCommitDescriptor( My_Desc1_Handle )
    Status = DftiComputeForward( My_Desc1_Handle, X )
    Status = DftiFreeDescriptor(My_Desc1_Handle)
    
    do i =1,((N-1)/2)+1
        x(i) = FFTRatio*x(i)
        XC(i) = CONJG(x(i))
        U = x(i)*xc(I)
        sum = sum+U*df
        sumw = sumw+(y(i)*y(i))*time
        n1 = 1
        a = 1.0d0
        b = 2.0d0
        call vdhypot( n1, real(x(i)), aimag(x(i)), c )
        call vdAtan2(n1,aimag(x(i)),real(x(i)),phi)
         write(*,10)i,(df*float(i)),(real(x(i))),aimag(x(i))
10       format(i5,2(",",F14.8),",",F14.8)
    end do
    ! result is given by {X(1),X(2),...,X(32)}
    ! Perform a real to complex conjugate-even transform
     Status = DftiCreateDescriptor(My_Desc2_Handle, DFTI_DOUBLE, DFTI_REAL, 1, N)
     Status = DftiCommitDescriptor(My_Desc2_Handle)
     Status = DftiComputeForward(My_Desc2_Handle, Y)
    Status = DftiFreeDescriptor(My_Desc2_Handle)
    ! result is given in CCS format.
    return
    end subroutine FFT
    END module