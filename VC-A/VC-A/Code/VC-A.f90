    !  CV.f90
    !
    !  FUNCTIONS:
    !  CV - Entry point of console application.
    !

    !****************************************************************************
    !
    !  PROGRAM: CV
    !
    !  PURPOSE:  Entry point for the console application.
    !
    !****************************************************************************

    program CV

    USE USER32
    use gdi32
    use Base
    use DISLIN
    use Elements
    use Draw

    implicit none
    character*100   inputline
    character*200  TitleDraw
    integer i, j
    REAL     time(2)         !   Holds the numbers for the run time calculation
    real dataa(1024)
    integer flag



    integer num
    Integer MType
    integer NL
    CHARACTER ZFILE1*60
    TYPE (Model),       TARGET :: ModelA                            !   Model for the Country Data


    flag = 0

    CALL cpu_time(time(1))

    !-------------------------------------------------------------------------------------------------------------------
    !
    !       allocate model arrays
    !
    !-------------------------------------------------------------------------------------------------------------------


    ALLOCATE (ModelA%FFtData(ModelA%FFTNumber),ModelA%codes(ModelA%MaxCountryNumber), ModelA%IDS(ModelA%MaxCountryNumber),&
        ModelA%Counter(ModelA%MaxCountryNumber),ModelA%deaths(ModelA%MaxCountryNumber,ModelA%MaxDayNumber), STAT=astat)
    allocate(ModelA%CountryName(ModelA%MaxCountryNumber),STAT = astat)
    allocate (ModelA%NameLen(ModelA%MaxCountryNumber),STAT = astat)


    IF (astat.NE.0) then
        STOP 'Could not allocate Codes or ID Matrices'
    endif

    !-------------------------------------------------------------------------------------------------------------------
    !
    !       Reset basic model
    !
    !-------------------------------------------------------------------------------------------------------------------


    do 100 i = 1, ModelA%MaxCountryNumber

        ModelA%Codes(i) = 0
        ModelA%IDS(i) = "00"
        ModelA%CountryName(i) = "00"
        ModelA%NameLen(i) = 0
        ModelA%Counter(i) = 0

        do 90 j = 1, ModelA%MaxDayNumber

            ModelA%deaths(i,j) = 0

90  end do
100 end do


    do while (flag .eq. 0)
        call Menu(zfile1, num, MType, NL)
        if(num .eq. 1) then
            call Header(titledraw)
            call ReadFile(ModelA,1)
            Call OutputResults(ModelA,1)
            call cls()
        else if(num .eq. 2) then

            call Header(titledraw)
            call ReadFile(ModelA,1)
            Call OutputResults(ModelA,1)

            do 120 i = 1,202
                call DrawDeaths(ModelA,titledraw,i)
                call DrawFFT(ModelA,titledraw,i)
120     end do
        flag = 1
    elseif(num .eq. 3) then

        call Header(titledraw)
        call ReadFile(ModelA,2)
        Call OutputResults(ModelA,2)
        do 130 i= 1,55
                call DrawDeaths(ModelA,titledraw,i)
                call DrawFFT(ModelA,titledraw,i)
130     end do                
        
        flag = 1
    elseif(num .eq. 5) then
        flag = 1
    endif

    end do


    Call cpu_time(time(2))

    WRITE(*,110) (time(2)-time(1))


110 Format(//"Program Execution CPU time     : ", F20.3,//)
    end program CV


    !      ****************************************************************
    !
    SUBROUTINE CLS()
    !
    !      ****************************************************************

    call system('CLS')

    RETURN
    END