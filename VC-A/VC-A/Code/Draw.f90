    Module Draw
    use base
    use Elements


    contains

    subroutine DrawDeaths(ModelA,titledraw,i)
    use Base
    implicit none

    TYPE (Model),       TARGET :: ModelA

    INTEGER, PARAMETER :: N=100
    REAL , DIMENSION (N) :: XRAY,Y1RAY,Y2RAY
    REAL  :: FPI,STEP,X, a, b, a1,b1
    CHARACTER (LEN=4) :: CDEV
    CHARACTER (LEN=1) :: C
    INTEGER :: IOPT=1
    Integer I,j, k, ia, IB
    REAL , PARAMETER :: PIA=3.1415926
    integer :: flag = 0
    integer :: j1 = 0
    integer :: j2 = 0
    integer :: flag1 = 0
    integer :: ik = 0
    integer dum(200)
    integer counter
    character*200 titledraw
    character*200 titledrawA
    REAL , allocatable :: XDATA(:)
    REAL , allocatable :: YDATA(:)
    REAL , allocatable :: ZDATA(:)
    REAL , allocatable :: DDATA(:)
    Integer NA

    Logical Done
    logical Exists

    Real :: graphlength  = 20.0
    real :: graphstart = 0.0
    character*60  nameC
    character*70 nameD
    character*70 nameE
    integer namelength
    character*100 eqA,eqB
    character*50 asd
    character*4 id
    real :: e = 2.71828
    real q
    counter = 0


    write(*,100)
100 Format(/'                Linear Regression Analysis Data',//,'      I      Count     Deaths    j1      j2     flag ',/)


    k = ModelA%counter(i)
    do 90 j = k,1,-1
        counter = counter + 1
        write(*,130)counter, j,(ModelA%Deaths(j,i)),j1,j2,flag
130     Format('   ',i4,'       ',i4,'   ',i6,'   ',i4,'     ', i4,'     ',i4)
        dum(j) = ModelA%Deaths(j,i)

        if(((ModelA%Deaths(j,i) .eq. 0) .and. flag .eq. 0)) then

        else if(((ModelA%Deaths(j,i) .ne. 0) .and. flag .eq. 0)) then
            j1 = ModelA%counter(i) - j - 1
            flag = 1
            j2 = k
            NA = j2 - j1 + 1
        end if
90  end do


    write(*,140)j1,j2,NA,flag
140 Format(/'      Index Counts for Selcting Array Data',//,  '      Value of J1 :: ',i4,/,'      Value of J2 :: ',i4,/'      Value of NA :: ',i4,/'      Flag Value  :: ',i4)
    ALLOCATE (YDATA(NA), XDATA(NA), ZDATA(NA), DDATA(NA), STAT=astat)


    do 110 ik = NA,1,-1
        flag1 = flag1 + 1
        XDATA(flag1) = REAL(flag1)
        if(dum(ik) .eq. 0) then
            YDATA(flag1) = REAL(dum(ik))+ 0.1
        else
            YDATA(flag1) = REAL(dum(ik))
        end if
110 end do






    IF (astat.NE.0) then
        STOP 'Could not allocate Codes or ID Matrices'
    endif


    write(*,*)'Inside DrawDeaths'

    !   set graph length for the count of the data

    graphlength = real(NA)

    call Xfitheader()

    call xfit(i,xdata,ydata,na, a, b, q)

    ModelA%FFTDaTA = ZERO
    
    write(*,*)a,b
    a1 = 10**a
    b1 = b*(log10(e))
    write(eqA,*)a1
    write(eqB,*)b1
    
    do 200 iB=1,NA

        ZDATA(iB) = (a) + ((b)*iB)
        ZDATA(iB) = 10**ZDATA(iB)
        write(*,*)ydata(iB),zdata(iB),zdata(iB)-ydata(iB)
        DDATA(iB) = zdata(iB)-ydata(iB)
        ModelA%FFTData(iB) = DDATA(iB)

200 end do

    !190 END DO

    nameC = ModelA%CountryName(i)
    namelength = ModelA%NameLen(i)
    nameD = trim(namec)//'LR.tif'
    nameD = trim(nameD)

    ! CALL HEADER(CDEV)
    CDEV = 'TIFF'
    CALL METAFL(CDEV)
    CALL SETPAG('DA4L')
    CALL SCRMOD('REVERS')
    CALL SETFIL(nameD)
    CALL DISINI()
    CALL PAGERA()
    CALL HWFONT()
    CALL AXSPOS(450,1800)
    CALL AXSLEN(2200,1200)

    CALL NAME('Day since First Recorded Death in Country','X')
    CALL NAME('Fatality Count per Day','Y')
    CALL AXSSCL('LOG','Y')

    CALL LABDIG(-1,'X')
    CALL TICKS(5,'X')
    CALL TICPOS('REVERS','XY')
    CALL INCMRK(-1)
    CALL MARKER(8)

    CALL TITLIN('Country Daily Fatality Rate',1)
    CALL TITLIN(trim(nameC),2)
    CALL TITLIN('Exponential Regression Model ',3)

    CALL GRAF(graphstart,graphlength,0.0,5.0,-1.,4.,-1.,1.0)
    CALL TITLE()

    CALL COLOR('WHITE')
    CALL CURVE(XDATA,YDATA,NA)

    CALL INCMRK(0)

    CALL COLOR('WHITE')
    CALL CURVE(XDATA,ZDATA,NA)

    CALL COLOR('FORE')
    CALL DASH()
    CALL XAXGIT()

    CALL TEXMOD('ON')
    CALL MESSAG('$y = {ce^{dx}}',700,700)
    asd = '$c = { \small '

    asd = (trim (asd))//(trim(eqA))//'}'
    CALL MESSAG(asd,700,750)
    asd = '$d = { \small '

    asd = (trim (asd))//(trim(eqB))//'}'
    CALL MESSAG(asd,700,800)
    write(id, 123) i
123 format (I4)
    asd = '$ { \tiny '//'Country :: '//id//'}'
    CALL MESSAG(asd,100,100)

    titledrawA = '$ { \tiny '//titledraw//'}'
    CALL MESSAG(titledrawA,100,150)

    CALL DISFIN()



    j1 = 0
    j2 = 0
    NA = 0
    flag = 0
    flag1 = 0
    dum = 0.0

    return
    end subroutine DrawDeaths

    !--------------------------------------------------------------------------------------------
    !
    !
    !
    !--------------------------------------------------------------------------------------------

    subroutine DrawFFT(ModelA,titledraw,k)

    implicit none

    TYPE (Model),       TARGET :: ModelA                            !   Model for the Country Data

    real dataa(1024)
    integer n, NA,i, j, NN,k
    CHARACTER (LEN=4) :: CDEV
    character*60  nameC
    character*70 nameD
    character*70 nameE
    integer namelength
    character*200 titledraw
    character*200 titledrawA
    real max, min, stepA, stepB

    REAL , allocatable :: XDATA(:)
    REAL , allocatable :: YDATA(:)
    REAL , allocatable :: ZDATA(:)
    REAL , allocatable :: DDATA(:)

    NN = 512
    NA = NN/2
    max = -300000.0
    min = 300000.0
    max = -3000000000000.0
    min = 30000000000000.0
    stepA = 0.0
    stepB = 0.0
    
    
    do 15 i = 1,1024
    
    dataa(i) = 0.0
15 end do    

    write(sa,16)
16  Format('     FFT Input Data ',//'     Count     Value ')    


    do 101 i = 1,512
        dataa(i) = real(ModelA%FFTDATA(i))
101 end do

    CALL FOUR1(dataa,NN,1)

    ALLOCATE (YDATA(NA), XDATA(NA), ZDATA(NA), DDATA(NA), STAT=astat)

    
    do 90 i = 1,NA
    
    YDATA(i) = 0.0
    XDATA(i) = 0.0
    DDATA(i) = 0.0
    ZDATA(i) = 0.0
    
90  end do    
    
    
    do 100 i =1,NA
        XDATA(i) = i
100 end do
    j = 1
    do 120 i = 1, NA

        YDATA(i) = dataa(j)
        ZDATA(i) = dataa(j+1)
        DDATA(i) = SQRT(dataa(j)**2 + dataa(j+ 1)**2)
        if(min .gt. YDATA(i)) then
            min = ydata(i)
        endif
        if(min .gt. ZDATA(i)) then
            min = ydata(i)
        endif
        if(min .gt. DDATA(i)) then
            min = ddata(i)
        endif

        if(max .lt. YDATA(i)) then
            max = ydata(i)
        endif
        if(max .lt. ZDATA(i)) then
            max = ydata(i)
        endif

        if(max .lt. DDATA(i)) then
            max = ddata(i)
        endif
        j = j+2
120 end do

    max = ceiling(max)
    write(*,*)i,min,max
    
    write(sa,*)k,min,max
    
    if(abs(min) .gt. max) then     
        max = abs(min)
    end if
    
    call setelements(max,stepA,stepB)

    nameC = ModelA%CountryName(k)
    namelength = ModelA%NameLen(k)
    nameD = trim(namec)//'FFT.tif'
    nameD = trim(nameD)

    ! CALL HEADER(CDEV)
    CDEV = 'TIFF'
    CALL METAFL(CDEV)
    CALL SETPAG('DA4L')
    CALL SCRMOD('REVERS')
    CALL SETFIL(nameD)
    CALL DISINI()
    CALL PAGERA()
    CALL HWFONT()
    CALL AXSPOS(450,1800)
    CALL AXSLEN(2200,1200)

    CALL NAME('FFT Enumeration','X')
    CALL NAME('FFT Amplitude','Y')
    ! CALL AXSSCL('LOG','Y')

    CALL LABDIG(-1,'X')
    !   CALL TICKS(5,'X')

    CALL TITLIN('Country FFT of the Residual Values',1)
    
    CALL TITLIN(trim(nameC),2)
    CALL TITLIN('Linear ',3)

    CALL GRAF(0.0,256.0,0.0,32.0,-max,max,stepB,stepA)
    CALL TITLE()

    CALL COLOR('RED')
    CALL CURVE(XDATA,YDATA,NA)


    CALL COLOR('BLUE')
    CALL CURVE(XDATA,ZDATA,NA)


    CALL COLOR('GREEN')
    CALL CURVE(XDATA,DDATA,NA)
    CALL COLOR('FORE')
    CALL DASH()
    CALL XAXGIT()
    
    CALL TEXMOD('ON')
    titledrawA = '$ { \tiny '//titledraw//'}'
    CALL MESSAG(titledrawA,100,150)

    CALL DISFIN()

    end subroutine DrawFFT
    
     
    subroutine setelements(max,stepA,stepB)
    
    implicit none 
    
    real max,stepA, stepB, delta
    
    delta = 0.0001
    
    if((max+delta) .lt. 2) then 
    
    stepA = 0.25
    stepB = -1.0
    max = 1.0
    
    else if ((max+delta) .lt. 3) then 
    
    stepA = 0.5
    stepB = -2.0
    max = 2.0
    
    else if ((max+delta) .lt. 4) then 
    
    
    stepA = 1.0
    stepB = -3.0
    max = 3.0
    
    
    else if ((max+delta) .lt. 5) then 
    
    stepA = 1.0
    stepB = -4.0
    max = 4.0
    
    
    else if ((max+delta) .lt. 6) then 
    
    stepA = 1.25
    stepB = -5.0
    max = 5.0
    
    else if ((max+delta) .lt. 7) then 
    
    stepA = 2.0
    stepB = -6.0
    max = 6.0
    
    
    else if ((max+delta) .lt. 8) then 
    
    
    stepA = 1.75
    stepB = -7.0
    max = 7.0
    
    else if ((max+delta) .lt. 9) then 
    
    stepA = 2.0
    stepB = -8.0
    max = 8.0
    
    else if ((max+delta) .lt. 10) then 
    
    stepA = 3
    stepB = -9.0
    max = 9.0
    
    else if ((max+delta) .lt. 11) then 
    
    stepA = 2.5
    stepB = -10.0
    max = 10.0
    
    else if ((max+delta) .lt. 16) then 
    
    stepA = 5.0
    stepB = -15.0
    max = 15.0
    
    else if ((max+delta) .lt. 21) then 
    
    stepA = 5.0
    stepB = -20.0
    max = 20.0
    
    else if ((max+delta) .lt. 26) then 
    
    stepA = 6.25
    stepB = -25.0
    max = 25.0
    
    else if ((max+delta) .lt. 31) then 
    
    stepA = 10.0
    stepB = -30.0
    max = 30.0
    
    else if ((max+delta) .lt. 41) then 
    
    stepA = 10.0
    stepB = -40.0
    max = 40.0
    
    else if ((max+delta) .lt. 51) then 
    
    stepA = 12.5
    stepB = -50.0
    max = 50.0
    
    else if ((max+delta) .lt. 61) then 
    
    stepA = 15.0
    stepB = -60.0
    max = 60.0
    
    else if ((max+delta) .lt. 71) then 
    
    stepA = 17.5
    stepB = -70.0
    max = 70.0
    
    
    else if ((max+delta) .lt. 81) then 
    
    stepA = 20.0
    stepB = -80.0
    max = 80.0
    
    else if ((max+delta) .lt. 91) then 
    
    stepA = 30.0
    stepB = -90.0
    max = 90.0
    
    else if ((max+delta) .lt. 101) then 
    
    stepA = 25.0
    stepB = -100.0
    max = 100.0
    
    else if ((max+delta) .lt. 151) then 
    
    stepA = 25.0
    stepB = -150.0
    max = 150.0
    
    else if ((max+delta) .lt. 201) then 
    
    stepA = 50.0
    stepB = -200.0
    max = 200.0
    
    else if ((max+delta) .lt. 251) then 
    
    stepA = 50.0
    stepB = -250.0
    max = 250.0
    
    else if ((max+delta) .lt. 301) then 
    
    stepA = 100.0
    stepB = -300.0
    max = 300.0
    
    else if ((max+delta) .lt. 401) then 
    
    stepA = 100.0
    stepB = -400
    max = 400.0
    
    else if ((max+delta) .lt. 501) then 
    
    stepA = 250.0
    stepB = -500.0
    max = 500.0
    
    else if ((max+delta) .lt. 751) then 
    
    stepA = 250.0
    stepB = -750.0
    max = 750.0
    
    else if ((max+delta) .lt. 1001) then 
    
    stepA = 250.0
    stepB = -1000.0
    max = 1000.0
    
    else if ((max+delta) .lt. 1501) then 
    
    stepA = 500.0
    stepB = -1500.0
    max = 1500.0
    
    else if ((max+delta) .lt. 2001) then 
    
    stepA = 500.0
    stepB = -2000.0
    max = 2000.0
    
    else if ((max+delta) .lt. 3001) then 
    
    stepA = 1000.0
    stepB = -3000.0
    max = 3000.0
    
    
    else if ((max+delta) .lt. 4001) then 
    
    stepA = 1000.0
    stepB = -4000.0
    max = 4000.0
    
    
    else if ((max+delta) .lt. 5001) then 
    
    stepA = 1000.0
    stepB = -5000.0
    max = 5000.0
    
    else if ((max+delta) .lt. 6001) then 
    
    stepA = 2000.0
    stepB = -6000.0
    max = 6000.0
    
    else if ((max+delta) .lt. 7001) then 
    
    stepA = 1750.0
    stepB = -7000.0
    max = 7000.0
    
    else if ((max+delta) .lt. 8001) then 
    
    stepA = 2000.0
    stepB = -8000.0
    max = 8000.0
    
    else if ((max+delta) .lt. 9001) then 
    
    stepA = 3000.0
    stepB = -9000.0
    max = 9000.0
    
    else if ((max+delta) .lt. 10001) then 
    
    stepA = 2500.0
    stepB = -10000.0
    max = 10000.0
    
    else if ((max+delta) .lt. 15001) then 
    
    stepA = 5000.0
    stepB = -15000.0
    max = 15000.0
    
    else if ((max+delta) .lt. 20001) then 
    
    stepA = 5000.0
    stepB = -20000.0
    max = 20000.0
    
    else if ((max+delta) .lt. 25001) then 
    
    stepA = 5000.0
    stepB = -25000.0
    max = 25000.0
    
    else if ((max+delta) .lt. 30001) then 
    
    stepA = 10000.0
    stepB = -30000.0
    max = 30000.0
    
    else if ((max+delta) .lt. 35001) then 
    
    stepA = 7000.0
    stepB = -35000.0
    max = 35000.0
    
    else if ((max+delta) .lt. 40001) then 
    
    stepA = 10000.0
    stepB = -40000.0
    max = 40000.0
    
    else if ((max+delta) .lt. 50001) then 
    
    stepA = 20000.0
    stepB = -60000.0
    max = 60000.0
    else if ((max+delta) .lt. 70001) then 
    
    stepA = 17500.0
    stepB = -70000.0
    max = 70000.0
    else if ((max+delta) .lt. 80001) then 
    
    stepA = 20000.0
    stepB = -80000.0
    max = 80000.0
    else if ((max+delta) .lt. 90001) then 
    
    stepA = 30000.0
    stepB = -90000.0
    max = 90000.0
    
    else if ((max+delta) .lt. 100001) then 
    
    stepA = 25000.0
    stepB = -100000.0
    max = 100000.0
    
    else if ((max+delta) .lt. 150001) then 
    
    stepA = 25000.0
    stepB = -150000.0
    max = 150000.0
    
    else if ((max+delta) .lt. 200001) then 
    
    stepA = 50000.0
    stepB = -200000.0
    max = 200000.0
    
    else if ((max+delta) .lt. 250001) then 
    
    stepA = 50000.0
    stepB = -250000.0
    max = 250000.0
    
    else if ((max+delta) .lt. 50001) then 
    
    stepA = 100000.0
    stepB = -300000.0
    max = 300000.0
    
    else if ((max+delta) .lt. 50001) then 
    
    stepA = 70000.0
    stepB = -350000.0
    max = 350000.0
    
    else if ((max+delta) .lt. 400001) then 
    
    stepA = 100000.0
    stepB = -400000.0
    max = 400000.0
    
    else if ((max+delta) .lt. 500001) then 
    
    stepA = 100000.0
    stepB = -500000.0
    max = 500000.0
    
    else if ((max+delta) .lt. 600001) then 
    
    stepA = 200000.0
    stepB = -600000.0
    max = 600000.0
    
    else if ((max+delta) .lt. 700001) then 
    
    stepA = 140000.0
    stepB = -700000.0
    max = 700000.0
    
    else if ((max+delta) .lt. 800001) then 
    
    stepA = 200000.0
    stepB = -800000.0
    max = 8-0000.0
    
    else if ((max+delta) .lt. 900001) then 
    
    stepA = 300000.0
    stepB = -900000.0
    max = 900000.0
    
    else if ((max+delta) .lt. 1000001) then 
    
    stepA = 200000.0
    stepB = -1000000.0
    max = 1000000.0
    
    else if ((max+delta) .lt. 5000001) then 
    
    stepA =  1000000.0
    stepB = -5000000.0
    max = 5000000.0
    
    else if ((max+delta) .lt. 10000001) then 
    
    stepA = 2000000.0
    stepB = -10000000.0
    max = 10000000.0
    
    else if ((max+delta) .lt. 50000001) then 
    
    stepA =  10000000.0
    stepB = -50000000.0
    max = 50000000.0
    
    else if ((max+delta) .lt. 100000001) then 
    
    stepA = 20000000.0
    stepB = -100000000.0
    max = 100000000.0
    
    else if ((max+delta) .lt. 500000001) then 
    
    stepA = 100000000.0
    stepB = -500000000.0
    max = 500000000.0
    
    else if ((max+delta) .lt. 1000000001) then 
    
    stepA = 200000000.0
    stepB = -1000000000.0
    max = 1000000000.0
    
    else if ((max+delta) .lt. 5000000001) then 
    
    stepA = 1000000000.0
    stepB = -5000000000.0
    max = 5000000000.0
    
    else if ((max+delta) .lt. 10000000001) then 
    
    stepA = 2000000000.0
    stepB = -10000000000.0
    max = 10000000000.0
    
    else if ((max+delta) .lt. 2000000001) then 
    
    stepA = 5000000000.0
    stepB = -20000000000.0
    max = 20000000000.0
    
    else if ((max+delta) .lt. 3000000001) then 
    
    stepA = 10000000000.0
    stepB = -30000000000.0
    max = 30000000000.0
    
    else if ((max+delta) .lt. 4000000001) then 
    
    stepA = 20000000000.0
    stepB = -40000000000.0
    max = 40000000000.0
    
    else if ((max+delta) .lt. 5000000001) then 
    
    stepA = 10000000000.0
    stepB = -50000000000.0
    max = 50000000000.0
    
    else if ((max+delta) .lt. 6000000001) then 
    
    stepA = 30000000000.0
    stepB = -60000000000.0
    max = 60000000000.0
    
    else if ((max+delta) .lt. 7000000001) then 
    
    stepA =  14000000000.0
    stepB = -70000000000.0
    max = 70000000000.0
    
    else if ((max+delta) .lt. 8000000001) then 
    
    stepA =  20000000000.0
    stepB = -80000000000.0
    max = 80000000000.0
    
    else if ((max+delta) .lt. 9000000001) then 
    
    stepA = 30000000000.0
    stepB = -90000000000.0
    max = 90000000000.0
    
    
    else if ((max+delta) .lt. 10000000001) then 
    
    stepA = 250000000000.0
    stepB = -100000000000.0
    max = 100000000000.0
    
   
                               
    else if ((max+delta) .lt. 50000000001) then 
    
    stepA =  100000000000.0
    stepB = -500000000000.0
    max = 500000000000.0
                                   
    else if ((max+delta) .lt. 50000000000001) then 
    
    stepA =  100000000000000.0
    stepB = -500000000000000.0
    max = 500000000000000.0
    
    else
    write(*,*)max
    pause
    endif
    
    
    return
    end subroutine setelements
    end module Draw