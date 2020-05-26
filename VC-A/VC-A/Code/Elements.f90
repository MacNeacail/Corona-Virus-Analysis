    !-------------------------------------------------------------------
    !
    !
    !
    !-------------------------------------------------------------------

    Module Elements
    use Base
    !-------------------------------------------------------------------
    !
    !
    !
    !-------------------------------------------------------------------
    implicit none
    integer, parameter ::   L = 3 !, nt = 120
    INTEGER astat

    !-------------------------------------------------------------------
    !
    !
    !
    !-------------------------------------------------------------------
    Type Model
        Integer             id
        Integer             NumCountries
        integer , allocatable :: codes(:)
        integer , allocatable :: counter(:)
        integer       ::    MaxCountryNumber = 300
        integer       ::    MaxDayNumber = 220
        integer       ::    FFTNumber = 512
        integer             MaxNumber
        integer             LineCount
        integer             FatalCount
        integer             InfectCount
        character*2, allocatable :: IDS(:)
        character*60, allocatable :: CountryName(:)
        integer , allocatable :: deaths(:,:)
        integer , allocatable :: NameLen(:)
        REAL (KIND=dp), allocatable ::  FFTDATA(:)

        !------------------------------------------------------------------------
        !   Dummies

    END TYPE



    contains

    subroutine Xfitheader()
    implicit none

    write(sw,10)
10  Format(//,'---------------------------------------------------------------------------------------------------------------------------------------------',//,&
        '           Linear Regression Data Fit Results',/&
        '   ',/,&
        '           Notes:',/,&
        '           1.     The raw data consists of the date data - integer converted to real.',/,&
        '           2.     The Fatality data consists of the daily counts - integer converted to real.',/,&
        '           3.     The fatality data is converted to logarithm base 10 data.',/,&
        '           4.     The assumption is made that a zero fatilty count is set to 0.1, to avoid zero problems with the logarithm analysis.',/,&
        '           5.     The ft routine is the Fortran 77 code from Numerical Recipes in Fortran -- this does not interfere with DISLIN.',/,&
        '           6.     The exponential equation is established using standard transforms for the linear results on the linear - log space.',/,&
        '           7.     The linear equation is y = ax + b - using the standard model.   ',/,&
        '           8.     The y data set is log(deaths).',/,&
        '           9.     The exponential equation is z = c*(e**d).    The symbols ** is the power function.',/,&
        '           10.    The transformations are  c = (10 ** b) and d = a/(log(e) :  e is the number 2.718281828459045',/,&
        '           11.    The regression analysis fits the data by minimizing chi squared.',/,&
        '           12.    Two loops are used for the analysis:',/,&
        '                  a.   The first assumes unknown standard deviations',/,&
        '                  b.   The second assumes estiamted standard deviations.',/,&
        '           13.    The information returned as a and b and the uncertainity sigma (a) and signma(b), chi squared and goodness of probablity of q.',/,&
        '           14.    If the SD are unknown the q is returned as 1.0 and the nomralization of chi squared is to unit standard deviaitons. ',/,&
        '                                                                                                                            ',///,&
        '|    Country   |  Standard  |  Estimated  |   Equation Constants  |   Signif. | Equation Slope/Exponent |  Signif. |   Chi    |    Probablity |',/&
        '|     Code     |  Deviation |  Stan. Dev. |       c        a      |     a     |       d          b      |    b     | Squared  |        q      |',/,&
        '|              |            |             |                       |           |                         |          |          |               |')


    return
    end subroutine

    !--------------------------------------------------------------------------------------------------------------------------
    !
    !   Write the header for the files.
    !
    !--------------------------------------------------------------------------------------------------------------------------

    subroutine header(title)

    implicit none
    INTEGER DATE_TIME (8)
    integer flag
    CHARACTER (LEN = 12) REAL_CLOCK (3)
    CHARACTER(LEN = 200) Title

    CALL DATE_AND_TIME (REAL_CLOCK (1), REAL_CLOCK (2), REAL_CLOCK (3), DATE_TIME)

    flag = 0

    if(Date_Time(4) .lt. 0) then
        flag = 1
    end if

    if(flag .eq. 0) then
        write(sw,140)Date_Time(3), Date_Time(2), Date_Time(1), Date_Time(5),Date_Time(6),Date_Time(7),abs(Date_Time(4))/60,'AHEAD GMT'
        write(title,150)Date_Time(3), Date_Time(2), Date_Time(1), Date_Time(5),Date_Time(6),Date_Time(7),abs(Date_Time(4))/60,'AHEAD GMT'
    elseif(flag .eq. 1) then
        write(sw,140)Date_Time(3), Date_Time(2), Date_Time(1), Date_Time(5),Date_Time(6),Date_Time(7),abs(Date_Time(4))/60,'BEHIND GMT'
        write(title,150)Date_Time(3), Date_Time(2), Date_Time(1), Date_Time(5),Date_Time(6),Date_Time(7),abs(Date_Time(4))/60,'BEHIND GMT'
    endif

150 Format( ' Analysis - d/m/year  :: ',i2,'/',i2,'/',i4,' Time  :: ',i2,':',i2,':',i2,' UTC Diff (hours) :: ',i2,' ',A10)

140 Format( //,'----------------------------------------------------------------------------------------------------------------',//&
        '            Corona Virus Statistical Analysis Death Data ',/,&
        '            Coded : J. M. Nichols 31/3/2020',/,&
        '            Version : 1.1.1.1',/,&
        '            Date of Analysis d/m/year  :: ',i2,'/',i2,'/',i4,/,&
        '            Time of Analysis           :: ',i2,':',i2,':',i2,/,&
        '            UTC Difference  (hours)    :: ',i2,'   ',A10,//,&
        '----------------------------------------------------------------------------------------------------------------',/,/,&
        '         Notes:',//,&
        '         1.     File source :: https://www.ecdc.europa.eu/en/geographical-distribution-2019-ncov-cases ',/,&
        '         2.     Source organization :: European Centre for Disease Prevention and Control ',/,&
        '         3.     Country Code is assigned by the program based on numerical order in original file. ',/,&
        '         4.     Statistical Codes are used for the Regression Analysis and the FFT Analysis.',/&
        '         5.     Record date is based on European Base Time for European Union.',/,&
        '         6.     Line data format is a code based on the actual number formats.',/,&
        '         7.     infection Count for each day.',/)

    call lineblankA()

    write(sw,110)

110 Format(' | Country |    Statistical   |  Line    |       Record Date        |    Line     |  Infection  |   Death  | GEOD |                   Country                 |   Name  |',/,&
        ' | Code    |  Code 1  Code 2  |  Number  |  Day     Month    Year   | Data Format |   Count     |   Count  |  ID  |                    Name                   |  Length |')
    return
    end subroutine header


    subroutine ReadFile(ModelA, k)


    implicit none

    TYPE (Model),       TARGET :: ModelA

    Logical Done
    logical Exists
    CHARACTER *130 iline
    character*60 nameA, mon
    character*2 nomen, nomenst
    character*4 name2
    character*5 name5
    character*6 name6
    character*7 name7
    character*8 name8
    character*9 name9
    character*10 name10
    character*11 name11
    character*12 name12
    character*13 name13
    character*14 name14
    character*15 name15
    character*16 name16
    character*17 name17
    character*18 name18
    character*19 name19
    character*20 name20
    character*21 name21
    character*22 name22
    character*23 name23
    character*24 name24
    character*25 name25
    character*26 name26
    character*27 name27
    character*28 name28
    character*29 name29
    character*30 name30
    character*31 name31
    character*32 name32
    character*33 name33
    character*34 name34
    character*35 name35
    character*36 name36
    character*37 name37
    character*38 name38
    character*39 name39
    character*40 name40
    character*41 name41
    character*42 name42
    character*43 name43
    character*44 name44



    integer flag, flag1, flag2, flag3,flag4, flag5
    integer loc

    integer yyyy,mm,dd, rd,md, yd, countA, countB,length, k, countC, NUM, numA, oldC



    ModelA%FatalCount = 0
    ModelA%InfectCount = 0

    flag = -1
    flag1 = 0
    flag2 = 1
    flag3 = 0
    flag4 = 0
    flag5 = 0
    num = 0
    numa = 1
    mon = "START"
    Done = .false.
    nameA = "NULL"
    nomenst = "NU"
    oldC = 0

    if(k .eq. 1) then


        DO WHILE (.NOT. done)              ! Loop for all the data lines
            READ (srB, '(A)', ERR=1000, END=400) iline    ! Read line

            if (((iline(2:2) .eq. '-') .or. (iline(2:2) .eq. '/'))) then
                iline = "0"//iline
            endif
            flag = flag + 1
            IF ((iline(1:2)) .EQ.'DA' .OR. (iline(1:2)) .EQ.'da' .or. (iline(4:5) .eq. 'da')) THEN    ! If line tagged as node
                write(*,100)iline
100             Format(A130)



            elseif (((iline(3:3) .eq. '-') .or. (iline(3:3) .eq. '/'))) then
                read(iline(1:2),120)dd
120             format(i2)
                flag2 = flag2 + 1
                flag3 = flag3 + 1
                if ((iline(6:6) .eq. '-') .or. (iline(6:6) .eq. '/')) then
                    read(iline(4:5),140)mm
140                 format(i2)

                    if((iline(11:11) .eq. ',')) then
                        read(iline(7:11),150)yyyy
150                     Format(i4)
                        if((iline(13:13) .eq. ',')) then
                            read(iline(12:12),160)rd
160                         format(i1)
                            if((iline(15:15) .eq. ',')) then
                                read(iline(14:14),160)md
                                loc = 15
                            elseif((iline(16:16) .eq. ',')) then
                                read(iline(14:15),120)md
                                loc = 16
                            endif
                        elseif((iline(14:14) .eq. ',')) then
                            read(iline(12:13),140)rd
                            if((iline(16:16) .eq. ',')) then
                                read(iline(15:15),160)md
                                loc = 16
                            elseif((iline(17:17) .eq. ',')) then
                                read(iline(15:16),120)md
                                loc = 17
                            endif
                        endif

                    end if
                endif

                if((iline(loc+5:loc+5) .eq. ',')) then
                    read(iline(loc+1:loc+4),150)yd
                endif

                if((iline(loc+7:loc+7) .eq. ',')) then
                    read(iline(loc+6:loc+6),160)countA
                    loc = loc + 7
                elseif((iline(loc+8:loc+8) .eq. ',')) then
                    read(iline(loc+6:loc+7),140)countA
                    loc = loc + 8
                elseif((iline(loc+9:loc+9) .eq. ',')) then
                    read(iline(loc+6:loc+8),170)countA
                    loc = loc + 9
170                 format(i3)
                elseif((iline(loc+10:loc+10) .eq. ',')) then
                    read(iline(loc+6:loc+9),150)countA
                    loc = loc + 10
                elseif((iline(loc+11:loc+11) .eq. ',')) then
                    read(iline(loc+6:loc+10),180)countA
                    loc = loc + 11
180                 format(i5)
                elseif((iline(loc+12:loc+12) .eq. ',')) then
                    read(iline(loc+6:loc+11),190)countA
                    loc = loc + 12
190                 format(i6)
                endif


                if((iline(loc+2:loc+2) .eq. ',')) then
                    read(iline(loc+1:loc+1),160)countB
                    loc = loc + 2
                elseif((iline(loc+3:loc+3) .eq. ',')) then
                    read(iline(loc+1:loc+2),140)countB
                    loc = loc + 3
                elseif((iline(loc+4:loc+4) .eq. ',')) then
                    read(iline(loc+1:loc+3),170)countB
                    loc = loc + 4
                elseif((iline(loc+5:loc+5) .eq. ',')) then
                    read(iline(loc+1:loc+4),150)countB
                    loc = loc + 5
                elseif((iline(loc+6:loc+6) .eq. ',')) then
                    read(iline(loc+1:loc+5),180)countB
                    loc = loc + 6
                elseif((iline(loc+7:loc+7) .eq. ',')) then
                    read(iline(loc+1:loc+6),190)countB
                    loc = loc + 7
                endif


                if((iline(loc+5:loc+5) .eq. ',')) then
                    read(iline(loc+1:loc+4),210)name2
                    nameA = name2
210                 format(A4)
                    loc = loc + 5
                elseif((iline(loc+6:loc+6) .eq. ',')) then
                    read(iline(loc+1:loc+5),220)name5
                    nameA = name5
220                 format(A5)
                    loc = loc + 6
                elseif((iline(loc+7:loc+7) .eq. ',')) then
                    read(iline(loc+1:loc+6),*)name6
                    nameA = name6
230                 format( A6)
                    loc = loc + 7
                elseif((iline(loc+8:loc+8) .eq. ',')) then
                    read(iline(loc+1:loc+7),240)name7
                    nameA = name7
240                 format(A7)
                    loc = loc + 8
                elseif((iline(loc+9:loc+9) .eq. ',')) then
                    read(iline(loc+1:loc+8),250)name8
                    nameA = name8
250                 format(A8)
                    loc = loc + 9
                elseif((iline(loc+10:loc+10) .eq. ',')) then
                    read(iline(loc+1:loc+9),260)name9
                    nameA = name9
260                 format(A9)
                    loc = loc + 10
                elseif((iline(loc+11:loc+11) .eq. ',')) then
                    read(iline(loc+1:loc+10),270)name10
                    nameA = name10
270                 format(A10)
                    loc = loc + 11
                elseif((iline(loc+12:loc+12) .eq. ',')) then
                    read(iline(loc+1:loc+11),280)name11
                    nameA = name11
280                 format(A11)
                    loc = loc + 12
                elseif((iline(loc+13:loc+13) .eq. ',')) then
                    read(iline(loc+1:loc+12),290)name12
                    nameA = name12
290                 format(A12)
                    loc = loc + 13
                elseif((iline(loc+14:loc+14) .eq. ',')) then
                    read(iline(loc+1:loc+13),300)name13
                    nameA = name13
300                 format(A13)
                    loc = loc + 14
                elseif((iline(loc+15:loc+15) .eq. ',')) then
                    read(iline(loc+1:loc+14),310)name14
                    nameA = name14
310                 format(A14)
                    loc = loc + 15
                elseif((iline(loc+16:loc+16) .eq. ',')) then
                    read(iline(loc+1:loc+15),320)name15
                    nameA = name15
320                 format(A15)
                    loc = loc + 16
                elseif((iline(loc+17:loc+17) .eq. ',')) then
                    read(iline(loc+1:loc+16),330)name16
                    nameA = name16
330                 format(A16)
                    loc = loc + 17
                elseif((iline(loc+18:loc+18) .eq. ',')) then
                    read(iline(loc+1:loc+17),340)name17
                    nameA = name17
340                 format(A17)
                    loc = loc + 18
                elseif((iline(loc+19:loc+19) .eq. ',')) then
                    read(iline(loc+1:loc+18),350)name18
                    nameA = name18
350                 format(A18)
                    loc = loc + 19
                elseif((iline(loc+20:loc+20) .eq. ',')) then
                    read(iline(loc+1:loc+19),360)name19
                    nameA = name19
360                 format(A19)
                    loc = loc + 20
                elseif((iline(loc+21:loc+21) .eq. ',')) then
                    read(iline(loc+1:loc+20),370)name20
                    nameA = name20
370                 format(A20)
                    loc = loc + 21
                elseif((iline(loc+22:loc+22) .eq. ',')) then
                    read(iline(loc+1:loc+21),380)name21
                    nameA = name21
380                 format(A21)
                    loc = loc + 22
                elseif((iline(loc+23:loc+23) .eq. ',')) then
                    read(iline(loc+1:loc+22),390)name22
                    nameA = name22
390                 format(A22)
                    loc = loc + 23
                elseif((iline(loc+24:loc+24) .eq. ',')) then
                    read(iline(loc+1:loc+23),420)name23
                    nameA = name23
420                 format(A23)
                    loc = loc + 24
                elseif((iline(loc+25:loc+25) .eq. ',')) then
                    read(iline(loc+1:loc+24),410)name24
                    nameA = name24
410                 format(A24)
                    loc = loc + 25
                elseif((iline(loc+26:loc+26) .eq. ',')) then
                    read(iline(loc+1:loc+25),421)name25
                    nameA = name25
421                 format(A25)
                    loc = loc + 26
                elseif((iline(loc+27:loc+27) .eq. ',')) then
                    read(iline(loc+1:loc+26),430)name26
                    nameA = name26
430                 format(A26)
                    loc = loc + 27
                elseif((iline(loc+28:loc+28) .eq. ',')) then
                    read(iline(loc+1:loc+27),440)name27
                    nameA = name27
440                 format(A27)
                    loc = loc + 28
                elseif((iline(loc+29:loc+29) .eq. ',')) then
                    read(iline(loc+1:loc+28),450)name28
                    nameA = name28
450                 format(A28)
                    loc = loc + 29
                elseif((iline(loc+30:loc+30) .eq. ',')) then
                    read(iline(loc+1:loc+29),460)name29
                    nameA = name29
460                 format(A29)
                    loc = loc + 30
                elseif((iline(loc+31:loc+31) .eq. ',')) then
                    read(iline(loc+1:loc+30),470)name30
                    nameA = name30
470                 format(A30)
                    loc = loc + 31
                elseif((iline(loc+32:loc+32) .eq. ',')) then
                    read(iline(loc+1:loc+31),480)name31
                    nameA = name31
480                 format(A31)
                    loc = loc + 32
                elseif((iline(loc+33:loc+33) .eq. ',')) then
                    read(iline(loc+1:loc+32),490)name32
                    nameA = name32
490                 format(A32)
                    loc = loc + 33
                elseif((iline(loc+34:loc+34) .eq. ',')) then
                    read(iline(loc+1:loc+33),500)name33
                    nameA = name33
500                 format(A33)
                    loc = loc + 34
                elseif((iline(loc+35:loc+35) .eq. ',')) then
                    read(iline(loc+1:loc+34),510)name34
                    nameA = name34
510                 format(A34)
                    loc = loc + 35
                elseif((iline(loc+36:loc+36) .eq. ',')) then
                    read(iline(loc+1:loc+35),520)name35
                    nameA = name35
520                 format(A35)
                    loc = loc + 36
                elseif((iline(loc+37:loc+37) .eq. ',')) then
                    read(iline(loc+1:loc+36),530)name36
                    nameA = name36
530                 format(A36)
                    loc = loc + 37
                elseif((iline(loc+38:loc+38) .eq. ',')) then
                    read(iline(loc+1:loc+37),540)name37
                    nameA = name37
540                 format(A37)
                    loc = loc + 38
                elseif((iline(loc+39:loc+39) .eq. ',')) then
                    read(iline(loc+1:loc+38),550)name38
                    nameA = name38
550                 format(A38)
                    loc = loc + 39
                elseif((iline(loc+40:loc+40) .eq. ',')) then
                    read(iline(loc+1:loc+39),560)name39
                    nameA = name39
560                 format(A39)
                    loc = loc + 40
                elseif((iline(loc+41:loc+41) .eq. ',')) then
                    read(iline(loc+1:loc+40),570)name40
                    nameA = name40
570                 format(A40)
                    loc = loc + 41
                elseif((iline(loc+42:loc+42) .eq. ',')) then
                    read(iline(loc+1:loc+41),580)name41
                    nameA = name41
580                 format(A41)
                    loc = loc + 42
                elseif((iline(loc+43:loc+43) .eq. ',')) then
                    read(iline(loc+1:loc+42),590)name42
                    nameA = name42
590                 format(A42)
                    loc = loc + 43
                elseif((iline(loc+44:loc+44) .eq. ',')) then
                    read(iline(loc+1:loc+43),600)name43
                    nameA = name43
600                 format(A43)
                    loc = loc + 44
                elseif((iline(loc+45:loc+45) .eq. ',')) then
                    read(iline(loc+1:loc+44),610)name44
                    nameA = name44
610                 format(A44)
                    loc = loc + 45
                end if
                if((iline(loc+3:loc+3) .eq. ',')) then
                    read(iline(loc+1:loc+2),620)nomen
620                 format(A2)
                end if


                if(nomen .ne. nomenST) then


                    if(nomenST .ne. "NU") then
                        ModelA%counter(flag1) = flag2-1
                    endif
                    flag1 = flag1 + 1
                    ModelA%codes(flag1) = flag1
                    ModelA%IDS(flag1) = nomen
                    nomenST = nomen
                    ModelA%maxNumber = flag1
                    ModelA%LineCount = flag
                    ModelA%Countryname(flag1) = nameA
                    ModelA%NameLen = len(trim(nameA))
                    flag2 = 1
                    flag3 = 0
                endif
                length = len(trim(nameA))
                if(length .lt. 5) then
                    write(*,121)flag1, flag3, flag2, flag, dd, mm, yyyy,loc,countA, countB, nomen, trim(nameA), length
                    write(sw,121)flag1, flag3, flag2,flag, dd, mm,yyyy, loc,countA, countB,nomen, trim(nameA), length

                else if(length .lt. 6) then
                    write(*,122)flag1, flag3, flag2, flag, dd, mm, yyyy,loc,countA, countB, nomen, trim(nameA), length
                    write(sw,122)flag1, flag3, flag2,flag, dd, mm,yyyy, loc,countA, countB,nomen, trim(nameA), length

                else if(length .lt. 7) then
                    write(*,123)flag1, flag3, flag2, flag, dd, mm, yyyy,loc,countA, countB, nomen, trim(nameA), length
                    write(sw,123)flag1, flag3, flag2,flag, dd, mm,yyyy, loc,countA, countB,nomen, trim(nameA), length

                else if(length .lt. 8) then
                    write(*,124)flag1, flag3, flag2, flag, dd, mm, yyyy,loc,countA, countB, nomen, trim(nameA), length
                    write(sw,124)flag1, flag3, flag2,flag, dd, mm,yyyy, loc,countA, countB,nomen, trim(nameA), length

                else if(length .lt. 9) then
                    if(nameA(2:4) .eq. "ura") then
                        write(*,148)flag1, flag3, flag2, flag, dd, mm, yyyy,loc,countA, countB, nomen, trim(nameA), length
                        write(sw,148)flag1, flag3, flag2,flag, dd, mm,yyyy, loc,countA, countB,nomen, trim(nameA), length
                    else
                        write(*,125)flag1, flag3, flag2, flag, dd, mm, yyyy,loc,countA, countB, nomen, trim(nameA), length
                        write(sw,125)flag1, flag3, flag2,flag, dd, mm,yyyy, loc,countA, countB,nomen, trim(nameA), length
                    endif

                else if(length .lt. 10) then
                    write(*,126)flag1, flag3, flag2, flag, dd, mm, yyyy,loc,countA, countB, nomen, trim(nameA), length
                    write(sw,126)flag1, flag3, flag2,flag, dd, mm,yyyy, loc,countA, countB,nomen, trim(nameA), length

                else if(length .lt. 11) then
                    write(*,127)flag1, flag3, flag2, flag, dd, mm, yyyy,loc,countA, countB, nomen, trim(nameA), length
                    write(sw,127)flag1, flag3, flag2,flag, dd, mm,yyyy, loc,countA, countB,nomen, trim(nameA), length

                else if(length .lt. 12) then
                    write(*,128)flag1, flag3, flag2, flag, dd, mm, yyyy,loc,countA, countB, nomen, trim(nameA), length
                    write(sw,128)flag1, flag3, flag2,flag, dd, mm,yyyy, loc,countA, countB,nomen, trim(nameA), length

                else if(length .lt. 13) then
                    write(*,129)flag1, flag3, flag2, flag, dd, mm, yyyy,loc,countA, countB, nomen, trim(nameA), length
                    write(sw,129)flag1, flag3, flag2,flag, dd, mm,yyyy, loc,countA, countB,nomen, trim(nameA), length

                else if(length .lt. 14) then
                    write(*,130)flag1, flag3, flag2, flag, dd, mm, yyyy,loc,countA, countB, nomen, trim(nameA), length
                    write(sw,130)flag1, flag3, flag2,flag, dd, mm,yyyy, loc,countA, countB,nomen, trim(nameA), length

                else if(length .lt. 15) then
                    write(*,131)flag1, flag3, flag2, flag, dd, mm, yyyy,loc,countA, countB, nomen, trim(nameA), length
                    write(sw,131)flag1, flag3, flag2,flag, dd, mm,yyyy, loc,countA, countB,nomen, trim(nameA), length

                else if(length .lt. 16) then
                    write(*,132)flag1, flag3, flag2, flag, dd, mm, yyyy,loc,countA, countB, nomen, trim(nameA), length
                    write(sw,132)flag1, flag3, flag2,flag, dd, mm,yyyy, loc,countA, countB,nomen, trim(nameA), length

                else if(length .lt. 17) then
                    write(*,133)flag1, flag3, flag2, flag, dd, mm, yyyy,loc,countA, countB, nomen, trim(nameA), length
                    write(sw,133)flag1, flag3, flag2,flag, dd, mm,yyyy, loc,countA, countB,nomen, trim(nameA), length

                else if(length .lt. 18) then
                    write(*,134)flag1, flag3, flag2, flag, dd, mm, yyyy,loc,countA, countB, nomen, trim(nameA), length
                    write(sw,134)flag1, flag3, flag2,flag, dd, mm,yyyy, loc,countA, countB,nomen, trim(nameA), length

                else if(length .lt. 19) then
                    write(*,135)flag1, flag3, flag2, flag, dd, mm, yyyy,loc,countA, countB, nomen, trim(nameA), length
                    write(sw,135)flag1, flag3, flag2,flag, dd, mm,yyyy, loc,countA, countB,nomen, trim(nameA), length
                else if(length .lt. 20) then
                    write(*,136)flag1, flag3, flag2, flag, dd, mm, yyyy,loc,countA, countB, nomen, trim(nameA), length
                    write(sw,136)flag1, flag3, flag2,flag, dd, mm,yyyy, loc,countA, countB,nomen, trim(nameA), length
                else if(length .lt. 21) then
                    write(*,137)flag1, flag3, flag2, flag, dd, mm, yyyy,loc,countA, countB, nomen, trim(nameA), length
                    write(sw,137)flag1, flag3, flag2,flag, dd, mm,yyyy, loc,countA, countB,nomen, trim(nameA), length
                else if(length .lt. 22) then
                    write(*,138)flag1, flag3, flag2, flag, dd, mm, yyyy,loc,countA, countB, nomen, trim(nameA), length
                    write(sw,138)flag1, flag3, flag2,flag, dd, mm,yyyy, loc,countA, countB,nomen, trim(nameA), length
                else if(length .lt. 23) then
                    write(*,139)flag1, flag3, flag2, flag, dd, mm, yyyy,loc,countA, countB, nomen, trim(nameA), length
                    write(sw,139)flag1, flag3, flag2,flag, dd, mm,yyyy, loc,countA, countB,nomen, trim(nameA), length
                else if(length .lt. 24) then
                    write(*,141)flag1, flag3, flag2, flag, dd, mm, yyyy,loc,countA, countB, nomen, trim(nameA), length
                    write(sw,141)flag1, flag3, flag2,flag, dd, mm,yyyy, loc,countA, countB,nomen, trim(nameA), length
                else if(length .lt. 25) then
                    write(*,142)flag1, flag3, flag2, flag, dd, mm, yyyy,loc,countA, countB, nomen, trim(nameA), length
                    write(sw,142)flag1, flag3, flag2,flag, dd, mm,yyyy, loc,countA, countB,nomen, trim(nameA), length
                else if(length .lt. 26) then
                    write(*,143)flag1, flag3, flag2, flag, dd, mm, yyyy,loc,countA, countB, nomen, trim(nameA), length
                    write(sw,143)flag1, flag3, flag2,flag, dd, mm,yyyy, loc,countA, countB,nomen, trim(nameA), length
                else if(length .lt. 27) then
                    write(*,144)flag1, flag3, flag2, flag, dd, mm, yyyy,loc,countA, countB, nomen, trim(nameA), length
                    write(sw,144)flag1, flag3, flag2,flag, dd, mm,yyyy, loc,countA, countB,nomen, trim(nameA), length
                else if(length .lt. 28) then
                    write(*,145)flag1, flag3, flag2, flag, dd, mm, yyyy,loc,countA, countB, nomen, trim(nameA), length
                    write(sw,145)flag1, flag3, flag2,flag, dd, mm,yyyy, loc,countA, countB,nomen, trim(nameA), length
                else if(length .lt. 28) then
                    write(*,147)flag1, flag3, flag2, flag, dd, mm, yyyy,loc,countA, countB, nomen, trim(nameA), length
                    write(sw,147)flag1, flag3, flag2,flag, dd, mm,yyyy, loc,countA, countB,nomen, trim(nameA), length
                else if(length .lt. 29) then
                    write(*,149)flag1, flag3, flag2, flag, dd, mm, yyyy,loc,countA, countB, nomen, trim(nameA), length
                    write(sw,149)flag1, flag3, flag2,flag, dd, mm,yyyy, loc,countA, countB,nomen, trim(nameA), length
                else if(length .eq. 32) then
                    write(*,146)flag1, flag3, flag2, flag, dd, mm, yyyy,loc,countA, countB, nomen, trim(nameA), length
                    write(sw,146)flag1, flag3, flag2,flag, dd, mm,yyyy, loc,countA, countB,nomen, trim(nameA), length
                else
                    write(*,110)flag1, flag3, flag2, flag, dd, mm, yyyy,loc,countA, countB, nomen, trim(nameA), length
                    write(sw,110)flag1, flag3, flag2,flag, dd, mm,yyyy, loc,countA, countB,nomen, trim(nameA), length
                endif

                ModelA%Deaths(flag2,flag1) = countB
                flag4 = flag4 + countA
                flag5 = flag5 + countB
110             Format(' | ', i4, '    |  ',i4, '    ',i4,'    |   ' i6,' |   ', i2  '     ', i2  &
                    , '       ',i4,  &
                    '   |       ',i2, '    |   ',i6, '    |  ',i5, '   |  ',A2, '  | ',A42, '|   ', i2 '    |')

121             Format(' | ', i4, '    |  ',i4, '    ',i4,'    |   ' i6,' |   ', i2  '     ', i2  &
                    , '       ',i4,  &
                    '   |       ',i2, '    |   ',i6, '    |  ',i5, '   |  ',A2, '  | ',A20, '                      |   ', i2 '    |')

122             Format(' | ', i4, '    |  ',i4, '    ',i4,'    |   ' i6,' |   ', i2  '     ', i2  &
                    , '       ',i4,  &
                    '   |       ',i2, '    |   ',i6, '    |  ',i5, '   |  ',A2, '  | ',A20, '                      |   ', i2 '    |')

123             Format(' | ', i4, '    |  ',i4, '    ',i4,'    |   ' i6,' |   ', i2  '     ', i2  &
                    , '       ',i4,  &
                    '   |       ',i2, '    |   ',i6, '    |  ',i5, '   |  ',A2, '  | ',A20, '                      |   ', i2 '    |')

124             Format(' | ', i4, '    |  ',i4, '    ',i4,'    |   ' i6,' |   ', i2  '     ', i2  &
                    , '       ',i4,  &
                    '   |       ',i2, '    |   ',i6, '    |  ',i5, '   |  ',A2, '  | ',A20, '                      |   ', i2 '    |')

125             Format(' | ', i4, '    |  ',i4, '    ',i4,'    |   ' i6,' |   ', i2  '     ', i2  &
                    , '       ',i4,  &
                    '   |       ',i2, '    |   ',i6, '    |  ',i5, '   |  ',A2, '  |  ',A19, '                      |   ', i2 '    |')

126             Format(' | ', i4, '    |  ',i4, '    ',i4,'    |   ' i6,' |   ', i2  '     ', i2  &
                    , '       ',i4,  &
                    '   |       ',i2, '    |   ',i6, '    |  ',i5, '   |  ',A2, '  | ',A20, '                      |   ', i2 '    |')

127             Format(' | ', i4, '    |  ',i4, '    ',i4,'    |   ' i6,' |   ', i2  '     ', i2  &
                    , '       ',i4,  &
                    '   |       ',i2, '    |   ',i6, '    |  ',i5, '   |  ',A2, '  |       ',A18, '                  |   ', i2 '    |')

128             Format(' | ', i4, '    |  ',i4, '    ',i4,'    |   ' i6,' |   ', i2  '     ', i2  &
                    , '       ',i4,  &
                    '   |       ',i2, '    |   ',i6, '    |  ',i5, '   |  ',A2, '  |     ',A18, '                    |   ', i2 '    |')

129             Format(' | ', i4, '    |  ',i4, '    ',i4,'    |   ' i6,' |   ', i2  '     ', i2  &
                    , '       ',i4,  &
                    '   |       ',i2, '    |   ',i6, '    |  ',i5, '   |  ',A2, '  |      ',A17, '                    |   ', i2 '    |')

130             Format(' | ', i4, '    |  ',i4, '    ',i4,'    |   ' i6,' |   ', i2  '     ', i2  &
                    , '       ',i4,  &
                    '   |       ',i2, '    |   ',i6, '    |  ',i5, '   |  ',A2, '  |      ',A17, '                    |   ', i2 '    |')

131             Format(' | ', i4, '    |  ',i4, '    ',i4,'    |   ' i6,' |   ', i2  '     ', i2  &
                    , '       ',i4,  &
                    '   |       ',i2, '    |   ',i6, '    |  ',i5, '   |  ',A2, '  |          ',A17, '                |   ', i2 '    |')

132             Format(' | ', i4, '    |  ',i4, '    ',i4,'    |   ' i6,' |   ', i2  '     ', i2  &
                    , '       ',i4,  &
                    '   |       ',i2, '    |   ',i6, '    |  ',i5, '   |  ',A2, '  |      ',A17, '                    |   ', i2 '    |')

133             Format(' | ', i4, '    |  ',i4, '    ',i4,'    |   ' i6,' |   ', i2  '     ', i2  &
                    , '       ',i4,  &
                    '   |       ',i2, '    |   ',i6, '    |  ',i5, '   |  ',A2, '  |          ',A17, '                |   ', i2 '    |')

134             Format(' | ', i4, '    |  ',i4, '    ',i4,'    |   ' i6,' |   ', i2  '     ', i2  &
                    , '       ',i4,  &
                    '   |       ',i2, '    |   ',i6, '    |  ',i5, '   |  ',A2, '  |      ',A18, '                   |   ', i2 '    |')

135             Format(' | ', i4, '    |  ',i4, '    ',i4,'    |   ' i6,' |   ', i2  '     ', i2  &
                    , '       ',i4,  &
                    '   |       ',i2, '    |   ',i6, '    |  ',i5, '   |  ',A2, '  |      ',A19, '                  |   ', i2 '    |')

136             Format(' | ', i4, '    |  ',i4, '    ',i4,'    |   ' i6,' |   ', i2  '     ', i2  &
                    , '       ',i4,  &
                    '   |       ',i2, '    |   ',i6, '    |  ',i5, '   |  ',A2, '  |        ',A20, '               |   ', i2 '    |')

137             Format(' | ', i4, '    |  ',i4, '    ',i4,'    |   ' i6,' |   ', i2  '     ', i2  &
                    , '       ',i4,  &
                    '   |       ',i2, '    |   ',i6, '    |  ',i5, '   |  ',A2, '  |     ',A21, '                 |   ', i2 '    |')

138             Format(' | ', i4, '    |  ',i4, '    ',i4,'    |   ' i6,' |   ', i2  '     ', i2  &
                    , '       ',i4,  &
                    '   |       ',i2, '    |   ',i6, '    |  ',i5, '   |  ',A2, '  |     ',A22, '                |   ', i2 '    |')

139             Format(' | ', i4, '    |  ',i4, '    ',i4,'    |   ' i6,' |   ', i2  '     ', i2  &
                    , '       ',i4,  &
                    '   |       ',i2, '    |   ',i6, '    |  ',i5, '   |  ',A2, '  |      ',A23, '              |   ', i2 '    |')

141             Format(' | ', i4, '    |  ',i4, '    ',i4,'    |   ' i6,' |   ', i2  '     ', i2  &
                    , '       ',i4,  &
                    '   |       ',i2, '    |   ',i6, '    |  ',i5, '   |  ',A2, '  |      ',A25, '               |   ', i2 '    |')

142             Format(' | ', i4, '    |  ',i4, '    ',i4,'    |   ' i6,' |   ', i2  '     ', i2  &
                    , '       ',i4,  &
                    '   |       ',i2, '    |   ',i6, '    |  ',i5, '   |  ',A2, '  |      ',A26, '           |   ', i2 '    |')

143             Format(' | ', i4, '    |  ',i4, '    ',i4,'    |   ' i6,' |   ', i2  '     ', i2  &
                    , '       ',i4,  &
                    '   |       ',i2, '    |   ',i6, '    |  ',i5, '   |  ',A2, '  |      ',A27, '          |   ', i2 '    |')

144             Format(' | ', i4, '    |  ',i4, '    ',i4,'    |   ' i6,' |   ', i2  '     ', i2  &
                    , '       ',i4,  &
                    '   |       ',i2, '    |   ',i6, '    |  ',i5, '   |  ',A2, '  |      ',A28, '                   |   ', i2 '    |')

145             Format(' | ', i4, '    |  ',i4, '    ',i4,'    |   ' i6,' |   ', i2  '     ', i2  &
                    , '       ',i4,  &
                    '   |       ',i2, '    |   ',i6, '    |  ',i5, '   |  ',A2, '  |      ',A28, '         |   ', i2 '    |')

146             Format(' | ', i4, '    |  ',i4, '    ',i4,'    |   ' i6,' |   ', i2  '     ', i2  &
                    , '       ',i4,  &
                    '   |       ',i2, '    |   ',i6, '    |  ',i5, '   |  ',A2, '  |    ',A32, '       |   ', i2 '    |')

147             Format(' | ', i4, '    |  ',i4, '    ',i4,'    |   ' i6,' |   ', i2  '     ', i2  &
                    , '       ',i4,  &
                    '   |       ',i2, '    |   ',i6, '    |  ',i5, '   |  ',A2, '  |      ',A28, '         |   ', i2 '    |')

148             Format(' | ', i4, '    |  ',i4, '    ',i4,'    |   ' i6,' |   ', i2  '     ', i2  &
                    , '       ',i4,  &
                    '   |       ',i2, '    |   ',i6, '    |  ',i5, '   |  ',A2, '  |              ',A8, '                      |   ', i2 '    |')

149             Format(' | ', i4, '    |  ',i4, '    ',i4,'    |   ' i6,' |   ', i2  '     ', i2  &
                    , '       ',i4,  &
                    '   |       ',i2, '    |   ',i6, '    |  ',i5, '   |  ',A2, '  |       ',A28, '        |   ', i2 '    |')
            ELSE



400             ModelA%counter(flag1) = flag2
                ModelA%LineCount = flag
                ModelA%InfectCount = flag4
                ModelA%FatalCount = flag5
                call lineblankA()
                done = .TRUE.
            endif

        end do

    else if(k .eq. 2) then
        write(*,*)'here'
        DO WHILE (.NOT. done)              ! Loop for all the data lines
            loc = 0
            READ (srB, '(A)', ERR=1000, END=1410) iline    ! Read line
            !write(*,*)iline
            dd = 0
            mm = 0
            yyyy = 0
            flag = flag + 1
            IF ((iline(1:2)) .EQ.'DA' .OR. (iline(1:2)) .EQ.'da' .or. (iline(4:5) .eq. 'da')) THEN    ! If line tagged as node
                flag1 = flag1 + 1
1100            Format(A130)
            else

                if (((iline(2:2) .eq. '-') .or. (iline(2:2) .eq. '/'))) then


                    read(iline(1:1),1119)dd
1119                format(i1)
                    flag2 = flag2 + 1
                    flag3 = flag3 + 1
                    loc = loc + 2
                elseif (((iline(3:3) .eq. '-') .or. (iline(3:3) .eq. '/'))) then


                    read(iline(1:2),1120)dd
1120                format(i2)
                    flag2 = flag2 + 1
                    flag3 = flag3 + 1
                    loc = loc+3
                endif

                !  write(*,*)iline(loc+3:loc+3),iline(loc+4:loc+4)


                if ((iline(loc+3:loc+3) .eq. '-') .or. (iline(loc+3:loc+3) .eq. '/')) then
                    !       write(*,*)iline(loc+1:loc+1),iline(loc+2:loc+2)
                    read(iline(loc+1:loc+2),1140)mm
1140                format(i2)
                    if((iline(loc+8:loc+8) .eq. ',')) then
                        read(iline(loc+4:loc+10),1150)yyyy
1150                    Format(i4)
                        loc = loc + 8

                        !    write(*,*)loc
                    end if

                    if((iline(loc+5:loc+5) .eq. ',')) then
                        read(iline(loc+1:loc+4),210)name2
                        nameA = name2
                        loc = loc + 5

                    elseif((iline(loc+6:loc+6) .eq. ',')) then
                        read(iline(loc+1:loc+5),220)name5
                        nameA = name5
                        loc = loc + 6

                    elseif((iline(loc+7:loc+7) .eq. ',')) then
                        read(iline(loc+1:loc+6),230)name6
                        nameA = name6
                        loc = loc + 7
                    elseif((iline(loc+8:loc+8) .eq. ',')) then
                        read(iline(loc+1:loc+7),240)name7
                        nameA = name7
                        loc = loc + 8
                    elseif((iline(loc+9:loc+9) .eq. ',')) then
                        read(iline(loc+1:loc+8),250)name8
                        nameA = name8
                        loc = loc + 9
                    elseif((iline(loc+10:loc+10) .eq. ',')) then
                        read(iline(loc+1:loc+9),260)name9
                        nameA = name9
                        loc = loc + 10
                    elseif((iline(loc+11:loc+11) .eq. ',')) then
                        read(iline(loc+1:loc+10),270)name10
                        nameA = name10
                        loc = loc + 11
                    elseif((iline(loc+12:loc+12) .eq. ',')) then
                        read(iline(loc+1:loc+11),280)name11
                        nameA = name11
                        loc = loc + 12
                    elseif((iline(loc+13:loc+13) .eq. ',')) then
                        read(iline(loc+1:loc+12),290)name12
                        nameA = name12
                        loc = loc + 13
                    elseif((iline(loc+14:loc+14) .eq. ',')) then
                        read(iline(loc+1:loc+13),300)name13
                        nameA = name13
                        loc = loc + 14
                    elseif((iline(loc+15:loc+15) .eq. ',')) then
                        read(iline(loc+1:loc+14),310)name14
                        nameA = name14
                        loc = loc + 15
                    elseif((iline(loc+16:loc+16) .eq. ',')) then
                        read(iline(loc+1:loc+15),320)name15
                        nameA = name15
                        loc = loc + 16
                    elseif((iline(loc+17:loc+17) .eq. ',')) then
                        read(iline(loc+1:loc+16),330)name16
                        nameA = name16
                        loc = loc + 17
                    elseif((iline(loc+18:loc+18) .eq. ',')) then
                        read(iline(loc+1:loc+17),340)name17
                        nameA = name17
                        loc = loc + 18
                    elseif((iline(loc+19:loc+19) .eq. ',')) then
                        read(iline(loc+1:loc+18),350)name18
                        nameA = name18
                        loc = loc + 19
                    elseif((iline(loc+20:loc+20) .eq. ',')) then
                        read(iline(loc+1:loc+19),360)name19
                        nameA = name19
                        loc = loc + 20
                    elseif((iline(loc+21:loc+21) .eq. ',')) then
                        read(iline(loc+1:loc+20),370)name20
                        nameA = name20
                        loc = loc + 21
                    elseif((iline(loc+22:loc+22) .eq. ',')) then
                        read(iline(loc+1:loc+21),380)name21
                        nameA = name21
                        loc = loc + 22
                    elseif((iline(loc+23:loc+23) .eq. ',')) then
                        read(iline(loc+1:loc+22),390)name22
                        nameA = name22
                        loc = loc + 23
                    elseif((iline(loc+24:loc+24) .eq. ',')) then
                        read(iline(loc+1:loc+23),420)name23
                        nameA = name23
                        loc = loc + 24
                    elseif((iline(loc+25:loc+25) .eq. ',')) then
                        read(iline(loc+1:loc+24),410)name24
                        nameA = name24
                        loc = loc + 25
                    endif


                end if
                countA = 0
                if((iline(loc+1:loc+1) .eq. ',')) then
                    read(iline(loc:loc),161)countA
                    loc = loc + 1
161                 format(i1)
                ELSEif((iline(loc+2:loc+2) .eq. ',')) then
                    read(iline(loc+1:loc+1),160)countA
                    loc = loc + 2
                elseif((iline(loc+3:loc+3) .eq. ',')) then
                    read(iline(loc+1:loc+2),140)countA
                    loc = loc + 3
                endif

                if((iline(loc+2:loc+2) .eq. ',')) then
                    read(iline(loc+1:loc+1),160)countB
                    loc = loc + 2
                elseif((iline(loc+3:loc+3) .eq. ',')) then
                    read(iline(loc+1:loc+2),170)countB
                    loc = loc + 3
                elseif((iline(loc+4:loc+4) .eq. ',')) then
                    read(iline(loc+1:loc+3),170)countB
                    loc = loc + 4
                elseif((iline(loc+5:loc+5) .eq. ',')) then
                    read(iline(loc+1:loc+4),180)countB
                    loc = loc + 5
                elseif((iline(loc+6:loc+6) .eq. ',')) then
                    read(iline(loc+1:loc+5),190)countB
                    loc = loc + 6
                elseif((iline(loc+7:loc+7) .eq. ',')) then
                    read(iline(loc+1:loc+6),1190)countB
                    loc = loc + 7
                elseif((iline(loc+8:loc+8) .eq. ',')) then
                    read(iline(loc+1:loc+7),2190)countB
                    loc = loc + 7
                endif
                if((iline(loc+2:loc+2) .eq. ' ')) then
                    read(iline(loc+1:loc+1),160)countC
                    loc = loc + 2
                elseif((iline(loc+3:loc+3) .eq. ' ')) then
                    read(iline(loc+1:loc+2),165)countC
165                 format(i4)
                    loc = loc + 3
                elseif((iline(loc+4:loc+4) .eq. ' ')) then
                    read(iline(loc+1:loc+3),170)countC
                    loc = loc + 4
                elseif((iline(loc+5:loc+5) .eq. ' ')) then
                    read(iline(loc+1:loc+4),180)countC
                    loc = loc + 5
                elseif((iline(loc+6:loc+6) .eq. ' ')) then
                    read(iline(loc+1:loc+5),190)countC
                    loc = loc + 6
                elseif((iline(loc+7:loc+7) .eq. ' ')) then
                    read(iline(loc+1:loc+6),1190)countC
                    loc = loc + 7
1190            Format(i7)                    
                elseif((iline(loc+8:loc+8) .eq. ' ')) then
                    read(iline(loc+1:loc+7),2190)countC
                    loc = loc + 8
2190            Format(i8)             
                endif

                length = len(trim(nameA))

                write(*,1130)num,dd ,mm,yyyy,trim(nameA), countA,countB, countC
1130            Format(i3,'  ', i2,'  ',i2,'   ',i4,'  ', (A), '   ',i4,'   ',i6,'   ',i8)

                if((trim(nameA)) .ne.  trim(mon)) then
                    mon = nameA
                    oldC = countC
                    
                    num = num + 1
                    ModelA%Countryname(num) = nameA
                    ModelA%NameLen = len(trim(nameA))
                    ModelA%maxNumber = num
                    if(num .gt. 1) then 
                    ModelA%counter(num-1) = (flag2 - 1)
                    end if
                    numA = 1
                    flag2 = 1
                else
                    numA = numA + 1
                    ModelA%Deaths(numa-1,num) = oldC - countC
                    oldC = countc
                end if
            end if
        end do


1410    write(*,1420)
1420    Format(///,'     At end of data input',//)
    end if
    return
1000 Stop ' Input Error in Country Data.'
    return


    end subroutine ReadFile


    !------------------------------------------------------------------------------------------------------------------------------------------------------
    !
    !   Read in the standard beams
    !
    !------------------------------------------------------------------------------------------------------------------------------------------------------


    subroutine OutputResults(ModelA, m)


    use Base
    implicit none

    integer i,j, k, m
    character*12 name

    TYPE (Model),       TARGET :: ModelA

    k = 0
    write(sw,10)ModelA%LineCount,ModelA%FatalCount,ModelA%InfectCount
10  Format(////'            Fatality Count Details',/,&
        '            Number of Records         :: ',i6,/,&
        '            Number of deaths          :: ',i6,/,&
        '            Number of people infected :: ',i6,//,&
        '            Notes:',/,&
        '            1.    Records limited to the last ninety days.',/,&
        '            2.    Number of country records can include zeros at the start.',/,&
        '            3.    European Data missing 20 March - US - 41.')



    call lineblankB()
    if(m .eq. 1) then
        write(sw,11)
11      format(/,' | Country | GEOD  | # Country |                     Fatality Count per day in Reverse Order        ',/,&
            ' |  Code   |  ID   |  Records  |     1     2     3     4     5 |      6     7     8     9    10 |     11    12    13    14    15 |     16    17    18    19    20 |     21    22    23    24    25 |     26    27    28    29    30 |     31    32    33    34    35 |     36    37    38    39    40 |     41    42    43    44    45 |     46    47    48    49    50 |     51    52    53    54    55 |     56    57    58    59    60 |     61    62    63    64    65 |     66    67    68    69    70 |     71    72   73     74    75 |     76    77    78    79    80 |     81    82    83    84    85 |     86    87    88    89    90 | ',/,&
            ' |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |')
    else

        write(sw,12)
12      format(/,' | Country |    GEOD      | # Country |                                          Fatality Count per day in Reverse Order        ',/,&
            ' |  Code   |     ID       |  Records  |     1     2     3     4     5 |      6     7     8     9    10 |     11    12    13    14    15 |     16    17    18    19    20 |     21    22    23    24    25 |     26    27    28    29    30 |     31    32    33    34    35 |     36    37    38    39    40 |     41    42    43    44    45 |     46    47    48    49    50 |     51    52    53    54    55 |     56    57    58    59    60 |     61    62    63    64    65 |     66    67    68    69    70 |     71    72   73     74    75 |     76    77    78    79    80 |     81    82    83    84    85 |     86    87    88    89    90 | ',/,&
            ' |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |')

    end if

    do 100 i = 1,ModelA%MaxNumber
        if(m .eq. 1) then

            write(sw,20)i, ModelA%IDS(i), ModelA%counter(i),&
                (ModelA%Deaths(j,i),j=1,5),&
                (ModelA%Deaths(j,i),j=6,10),&
                (ModelA%Deaths(j,i),j=11,15),&
                (ModelA%Deaths(j,i),j=16,20),&
                (ModelA%Deaths(j,i),j=21,25),&
                (ModelA%Deaths(j,i),j=26,30),&
                (ModelA%Deaths(j,i),j=31,35),&
                (ModelA%Deaths(j,i),j=36,40),&
                (ModelA%Deaths(j,i),j=41,45),&
                (ModelA%Deaths(j,i),j=46,50),&
                (ModelA%Deaths(j,i),j=51,55),&
                (ModelA%Deaths(j,i),j=56,60),&
                (ModelA%Deaths(j,i),j=61,65),&
                (ModelA%Deaths(j,i),j=66,70),&
                (ModelA%Deaths(j,i),j=71,75),&
                (ModelA%Deaths(j,i),j=76,80),&
                (ModelA%Deaths(j,i),j=81,85),&
                (ModelA%Deaths(j,i),j=86,90)

        else
            read(ModelA%CountryName(i)(1:12),'(A)')name
            write(sw,120)i, name, ModelA%counter(i),&
                (ModelA%Deaths(j,i),j=1,5),&
                (ModelA%Deaths(j,i),j=6,10),&
                (ModelA%Deaths(j,i),j=11,15),&
                (ModelA%Deaths(j,i),j=16,20),&
                (ModelA%Deaths(j,i),j=21,25),&
                (ModelA%Deaths(j,i),j=26,30),&
                (ModelA%Deaths(j,i),j=31,35),&
                (ModelA%Deaths(j,i),j=36,40),&
                (ModelA%Deaths(j,i),j=41,45),&
                (ModelA%Deaths(j,i),j=46,50),&
                (ModelA%Deaths(j,i),j=51,55),&
                (ModelA%Deaths(j,i),j=56,60),&
                (ModelA%Deaths(j,i),j=61,65),&
                (ModelA%Deaths(j,i),j=66,70),&
                (ModelA%Deaths(j,i),j=71,75),&
                (ModelA%Deaths(j,i),j=76,80),&
                (ModelA%Deaths(j,i),j=81,85),&
                (ModelA%Deaths(j,i),j=86,90)

        end if
        k = k + ModelA%counter(i)

100 end do

20  Format(' | ',i6,'  |  ', A2, '   |  ', i6, '   |' 18(5(' ', i5),' | '))

120 Format(' | ',i6,'  | ', A12, ' |  ', i6, '   |' 18(5(' ', i5),' | '))


    call lineblankB()


    write(sw,200)k
200 Format('         Line count : ',i6)


    end subroutine OutputResults

    end Module Elements