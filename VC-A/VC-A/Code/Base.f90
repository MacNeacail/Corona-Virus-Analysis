    !---------------------------------------------------------------------------------------------------------------------------
    !
    !   Four modules are called from the main program.
    !   Base holds the underlying FORTRAN elements such as dp etc.
    !   Subroutines
    !                   OpenFiles (i,j,k,l,m)
    !                   LineBlank(sw)
    !                   Line(sw)
    !                   Timingline(sw)
    !
    !---------------------------------------------------------------------------------------------------------------------------

    Module Base

    INTEGER, PARAMETER :: dp = selected_real_kind(15, 307)

    INTEGER, PARAMETER :: sw = 2                    !   Output file
    INTEGER, PARAMETER :: srA = 15                  !   output.txt file
    INTEGER, PARAMETER :: srB = 16                  !   output.txt file
    INTEGER, PARAMETER :: st = 14
    INTEGER, PARAMETER :: sCAD = 12
    INTEGER, PARAMETER :: sa = 3                    !   Output file
    INTEGER, PARAMETER :: smWrite = 4
    INTEGER, PARAMETER :: si = 1
    Integer, parameter :: slog = 9                  !   Log file
    Integer, parameter :: nta = 200                  !   Log file
    Integer, parameter :: outNode = 63                  !   Log file
    Integer, parameter :: inNode = 0                  !   Log file
    integer, parameter :: nt1 = 2000
    integer, parameter :: mt1 = 2000        !   Number of members
    integer, parameter :: mn1 = 2
    integer, parameter :: ml1 = 3
    integer, parameter :: ml2 = 4
    integer, parameter :: ml30 = 9000
    integer, parameter :: limit = 9000

    REAL (KIND=dp), PARAMETER :: gr = 9.806_DP, pi = 3.14159265_DP  !   Standard parameters
    REAL (KIND=dp), PARAMETER :: delta = 0.001_DP     !   Error number of checking for zero
    REAL (KIND=dp), PARAMETER :: deltafreq = 0.00001_DP     !   Error number of checking for zero
    REAL (KIND=dp), PARAMETER :: ZERO = 0.0_DP
    REAL (KIND=dp), PARAMETER :: ONE = 1.0_DP
    REAL (KIND=dp), PARAMETER :: MinusONE = -1.0_DP
    REAL (KIND=dp), PARAMETER :: TWO = 2.0_DP
    REAL (KIND=dp), PARAMETER :: THREE = 3.0_DP
    REAL (KIND=dp), PARAMETER :: THIRTY = 30.0_DP
    REAL (KIND=dp), PARAMETER :: FOUR = 4.0_DP
    REAL (KIND=dp), PARAMETER :: SIX = 6.0_DP
    REAL (KIND=dp), PARAMETER :: TWELVE = 12.0_DP
    REAL (KIND=dp), PARAMETER :: PointFIVE = 0.5_DP
    REAL (KIND=dp), PARAMETER :: PointTWOFIVE = 0.25_DP
    REAL (KIND=dp), PARAMETER :: OneHundred = 100.0_DP
    REAL (KIND=dp), PARAMETER :: DeltaFr = 15000000.0_DP
    REAL (KIND=dp), PARAMETER :: FTR = 1.0_DP
    REAL (KIND=dp), PARAMETER :: FTRA = 360.0_DP
    REAL (KIND=dp), PARAMETER :: SMALLMASS = 0.0001_DP
    REAL (KIND=dp), PARAMETER :: SMALLMASS1 = 0.0000000000000000001_DP
    REAL (KIND=dp), PARAMETER :: SMALLNUMBER = 0.0001_DP
    REAL (KIND=dp), PARAMETER :: SMALLNUMBER1 = 0.0000000061_DP

    INTEGER, PARAMETER :: mn_3 = 3,mn_9 = 9, mn_2 = 2, mn_10 = 10, mn_18 = 18, mn_6 = 6, mn_12 = 12, mn_4 = 4

    contains


    !---------------------------------------------------------------------------------------------------------------------------
    !
    !   Open files called from Borr Main Program Unit
    !   call OpenFiles(si,sw,sa,slog)
    !
    !---------------------------------------------------------------------------------------------------------------------------

    Subroutine OpenFiles(i,j,k,l,m)

    implicit none

    integer i,j,k,l,m
    CHARACTER*12 fname
    LOGICAL exists

    !    This program prompts for the name of a data file.
    !    The INQUIRE statement then determines whether
    !    the file exists. If it does not, the program
    !    prompts for another file name.

    !     Get the name of a file:

100 WRITE (*, 110)
    WRITE (sw, 110)
    READ (*, 120) fname
120 format((A))

    !     INQUIRE about file's existence:

    INQUIRE (FILE = fname, EXIST = exists)

    IF (.NOT. exists) THEN
        WRITE (*,'(2A/)') ' >> Cannot find file ', fname
        GOTO 100
    END IF


    open(i, file=fname, status = 'UNKNOWN')
    open(j, file='ad.rpt', status = 'UNKNOWN')
    open(k, file='ad.eig', status = 'UNKNOWN')
    OPEN(m,FILE='ad.TXT',STATUS='UNKNOWN')

    INQUIRE (FILE = 'beam.org', EXIST = exists)
    if(exists) then
        open(l, file='beam.org', status = 'UNKNOWN')
    endif
    WRITE (j, 130)
    return

110 Format(1X, '---------------------------------------------------------------------------------------------------------------------------------------',//&
        '                      Plate Analysis Program',/,&
        '                      Triangular Element Test Program',/,&
        '                      Element is 18 DOF Triangular Element from Carlos Felippa, 1986',/,&
        '                      Additional Coding J.M. Nichols (C) 2016',//,&
        '                      Enter the data file name: ' \)

130 Format(1X, '---------------------------------------------------------------------------------------------------------------------------------------',//&
        '                      Plate Analysis Program',/,&
        '                      Triangular Element Test Program',/,&
        '                      Element is 18 DOF Triangular Element from Carlos Felippa, 1986',/,&
        '                      Additional Coding J.M. Nichols (C) 2016',//)
    end subroutine


    !-------------------------------------------------------------------
    !
    !       Braws a blank line
    !
    !-------------------------------------------------------------------

    SUBROUTINE lineblank(sw)

    IMPLICIT NONE

    INTEGER :: sw       !   File number

    WRITE (sw, 100)
    WRITE (sa, 100)

    RETURN
100 FORMAT ('         ')

    END SUBROUTINE

    !-------------------------------------------------------------------
    !
    !       Write a line to a file sw
    !
    !-------------------------------------------------------------------

    SUBROUTINE line(sw)

    IMPLICIT NONE
    INTEGER :: sw

    WRITE (sw, 100)
    WRITE (sw, 110)
    WRITE (sa, 100)
    WRITE (sa, 110)
    RETURN
100 FORMAT ('---------------------------------------------------------------------------------------------------------------------------------------')
110 FORMAT ('                             ')

    !-------------------------------------------------------------------
    !
    !       Write a line to a file sw
    !
    !-------------------------------------------------------------------

    end subroutine

    subroutine lineblankA()

    IMPLICIT NONE

    ! WRITE (*, 100)
    !  WRITE (*, 110)
    WRITE (sa, 100)
    WRITE (sa, 110)
    WRITE (sw, 100)
    WRITE (sw, 110)
    RETURN
100 FORMAT (/,'---------------------------------------------------------------------------------------------------------------------------------------------------------------------------')
110 FORMAT ('                             ')



    END SUBROUTINE

    subroutine lineblankB

    implicit none

    write(sw,110)
110 format(/,'-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------',/)

    return
    end subroutine

    !------------------------------------------------------------------------------------------
    !
    !   PURPOSE Print (m x n) matrix as 2D array, 9 items/line
    !
    !------------------------------------------------------------------------------------------

    subroutine   MATRIXPRINT (a, m, n)

    implicit none
    integer           m, n, i, j, jref
    double precision  a(m,n)

    do 2000  jref = 0,n-1,9

        write(sa,100)
        write(sw,100)
100     Format(    '  ')
        !print '(24X,9I8)',(j,j=jref+1,min(jref+9,n))
        write(sw,200) (j,j=jref+1,min(jref+9,n))
        write(sa,200) (j,j=jref+1,min(jref+9,n))
200     Format(24X,9I8)
        do 1500  i = 1,m
            ! print '(20x,I5,1x,9F8.2)',i,(a(i,j),j=jref+1,min(jref+9,n))
            write(sw,300)i,(a(i,j),j=jref+1,min(jref+9,n))
            write(sa,300)i,(a(i,j),j=jref+1,min(jref+9,n))
300         Format(20x,I5,1x,9F8.2)
1500    continue
2000 continue

    return
    end

    !------------------------------------------------------------------------------------------
    !
    !   PURPOSE Print (m x n) matrix as 2D array, 9 items/line
    !
    !------------------------------------------------------------------------------------------

    subroutine   MATRIXPRINTA (a, m, n)
    implicit none
    integer           m, n, i, j, jref
    double precision  a(m,n)

    do 2000  jref = 0,n-1,9
        !print '(3X,9I8)',(j,j=jref+1,min(jref+9,n))
        write(sw,200) (j,j=jref+1,min(jref+9,n))
        write(sa,200) (j,j=jref+1,min(jref+9,n))
200     Format(3X,9I8)
        do 1500  i = 1,m
            ! print '(I5,9F8.2)',i,(a(i,j),j=jref+1,min(jref+9,n))
            write(sw,300)i,(a(i,j),j=jref+1,min(jref+9,n))
            write(sa,300)i,(a(i,j),j=jref+1,min(jref+9,n))
300         Format(I5,1x,9F8.2)
1500    continue
2000 continue

    return
    end

    !------------------------------------------------------------------------------------------
    !
    !   PURPOSE Print (m x n) matrix as 2D array, 9 items/line
    !
    !------------------------------------------------------------------------------------------

    subroutine   MATRIXPRINTB (a, m, n)

    implicit none
    integer           m, n, i, j, jref
    double precision  a(m,n)

    do 2000  jref = 0,n-1,6
        !print '  '
        !print '(24X,6I9)',(j,j=jref+1,min(jref+6,n))
        write(sw,200) (j,j=jref+1,min(jref+9,n))
        write(sa,200) (j,j=jref+1,min(jref+9,n))
200     Format(24X,9I8)
        do 1500  i = 1,m
            !  print '(20x,I5,1x,6F9.2)',i,(a(i,j),j=jref+1,min(jref+6,n))
            write(sw,300)i,(a(i,j),j=jref+1,min(jref+9,n))
            write(sa,300)i,(a(i,j),j=jref+1,min(jref+9,n))
300         Format(20x,I5,1x,9F8.2)
1500    continue
2000 continue

    return
    end


    !------------------------------------------------------------------------------------------
    !
    !   PURPOSE Print (m x n) matrix as 2D array, 9 items/line
    !
    !------------------------------------------------------------------------------------------

    subroutine   MATRIXPRINTF (a, m, n)

    implicit none
    integer           m, n, i, j, jref
    double precision  a(m,n)

    do 2000  jref = 0,n-1,6
        ! print '  '
        !  print '(24X,6I8)',(j,j=jref+1,min(jref+6,n))
        write(sw,200) (j,j=jref+1,min(jref+9,n))
        write(sa,200) (j,j=jref+1,min(jref+9,n))
200     Format(24X,9I8)
        do 1500  i = 1,m
            !    print '(20x,I5,1x,6F8.2)',i,(a(i,j),j=jref+1,min(jref+6,n))
            write(sw,300)i,(a(i,j),j=jref+1,min(jref+9,n))
            write(sa,300)i,(a(i,j),j=jref+1,min(jref+9,n))
300         Format(20x,I5,1x,9F8.2)
1500    continue
2000 continue

    return
    end

    !-----------------------------------------------------
    !
    !       Write out date and time of job and other headers
    !
    !-----------------------------------------------------

    SUBROUTINE timingline(sw, i)

    IMPLICIT NONE
    INTEGER :: sw
    Integer :: I
    INTEGER :: tmpday, tmpmonth, tmpyear
    INTEGER :: tmphour, tmpminute, tmpsecond, tmphund, values(8)
    CHARACTER *2 :: mer

    CALL date_and_time(values=values)
    tmpyear = values(1)
    tmpmonth = values(2)
    tmpday = values(3)
    tmphour = values(5)
    tmpminute = values(6)
    tmpsecond = values(7)
    tmphund = values(8)
    IF (tmphour.GT.12) THEN
        mer = 'pm'
        tmphour = tmphour - 12
    ELSE
        mer = 'am'
    END IF
    WRITE (sw, 100) tmpday, tmpmonth, tmpyear
    WRITE (sw, 110) tmphour, tmpminute, tmpsecond, tmphund, mer
    WRITE (sa, 100) tmpday, tmpmonth, tmpyear
    WRITE (sa, 110) tmphour, tmpminute, tmpsecond, tmphund, mer
    !  WRITE (*, 100) tmpday, tmpmonth, tmpyear
    !  WRITE (*, 110) tmphour, tmpminute, tmpsecond, tmphund, mer
    WRITE (sw, 120)
    if(i .gt. 0) then
        WRITE (sw, 130)
        !       WRITE (*, 130)
        WRITE (sa, 130)
    endif
    RETURN
100 FORMAT ('                      Analysis Date : ', I2, '/', I2.2, '/', I4.4)
110 FORMAT ('                      Time : ', I2, ':', I2.2, ':', I2.2, ':', I3.3,' ', A,/)
120 FORMAT ('        ')
130 FORMAT(/,1X, '---------------------------------------------------------------------------------------------------------------------------------------',//&
        '                      Triangle Data ',//&
        '| Number|      Nodes     |                     Node Coordinates                        |     Node Restraints    |',/,&
        '                                    A                    B                    C',/,&
        '        |   A    B    C  |   X      Y     Z   |   X      Y     Z   |   X      Y     Z  |   A       B        C   |',/)
    END SUBROUTINE


    !-----------------------------------------------------------------------------------------------------------------------------------------------------------------
    !
    !
    !
    !-----------------------------------------------------------------------------------------------------------------------------------------------------------------
    Subroutine Menu(zfile1, num, MType, NL)


    CHARACTER*8  :: date
    CHARACTER*10 :: clock_time
    CHARACTER*5  :: zone
    CHARACTER ZFILE1*60
    !       Date and time for printout purposes
    INTEGER, DIMENSION(8) :: datetimenumber
    integer num
    integer MType, NL

    CHARACTER*1 VFILE(9)

    CHARACTER DH1

    DH1=CHAR(32)
    Mtype = 1
    NL = 0

    !     Get the name of a file:
    WRITE (*, 171)
171 Format(1x,//,'----------------------------------------------------------',/'      Analysis Program for Corona Virus Data',/'         Version 2020.1 - Fortran Version',/)

    !****************************************************************************************
    !       Determine the date and time to mark files.
    CALL DATE_AND_TIME (date, clock_time, zone, datetimenumber)

    !****************************************************************************************
    WRITE (*,200)datetimenumber(1), datetimenumber(2),datetimenumber(3),datetimenumber(5), datetimenumber(6),datetimenumber(7)

200 Format('         Start Time ',2x,I4,'.',I2,'.',I2,4x,'at ',I2,':',I2,':',I2)

301 Write(*,300)
300 Format(/'---------------------------------------------------------',//'          Menu',//&
        '             Read Data File                  ::   1',/,&
        '             Plot Daily Death Data           ::   2',/,&
        '             Plot US State Daily Death Data  ::   3',/,&
        '             Help                            ::   4',/,&
        '             Halt Program                    ::   5',//,&
        '                Selection :: ',\)

    READ(*,*,ERR=11)num
    if(num .eq. 1) then
        NL = 1
    endif
    if(num .eq. 1 .or. num .eq. 2 .or. num .eq. 3) then
        MTYPE = 1
    endif


    if(num .eq. 4) then
        Write(*,600)
600     Format(/'-------------------------------------------------------------------------------',//'          Balor Help File',//&
            '           The program is based on the concepts outlined by Harrison in 1973.',/,&
            '           2   - Constant Mass Matrix Offsets added.'/,&
            '           3   - Distributed Mass Matrix [Harrison].'/,&
            '           4   - Distributed Mass Matrix [Traditional].'/,&
            '           5   - Basic Diagonal Mass Matrix (Lothuur) [Traditional].'/)
        goto 301
    endif
    if(num .eq. 15)then
        Stop
    endif

    if(num .lt. 1 .or. num .gt. 5) then
        Write(*,302)num
302     Format(/'             Error in input',/'             Number must be valid :: ',I4)
        goto 301
    endif

11  WRITE (*, 180)
180 Format(4x,/'              Enter the data file name: ',\)
    READ(*,30,ERR=11)zFILE1
30  FORMAT((A))



    !
    !       RECOVER FILE NAME
    !

    READ(ZFILE1,111)vFILE(1),vFILE(2),vFILE(3),vFILE(4),vFILE(5),vFILE(6),vFILE(7),vFILE(8)
111 FORMAT(8A)
    !
    !       FORCE CR/LF
    !

    vFILE(9)=DH1


    CALL FOPENNEW(sRB,1,'.csv',vFILE)
    CALL FOPENNEW(sw,2,'.txt',vFILE)
    CALL FOPENNEW(sa,2,'.out',vFILE)
    return
    end subroutine


    !      ****************************************************************
    !
    SUBROUTINE FOPENNEW(I,J,XFILE,VFILE)
    !
    !      ***************************************************************
    Implicit None

    CHARACTER*1 VFILE(9)
    Integer I,J
    Character*1 Xfile(4)

    CHARACTER FNAME*14
    LOGICAL EXISTS

    CHARACTER CH
    character dh
    character dh1

    CH=CHAR(27)
    dh=char(13)
    DH1=CHAR(32)


    CALL FILCRENEW ( VFILE,XFILE,FNAME)



    IF (J .EQ. 1 ) THEN
        INQUIRE(FILE= FNAME, EXIST = EXISTS)
        !write(*,*)"here"
        IF (.NOT. EXISTS) THEN
            !CALL CLS
            WRITE(*,90)FNAME
90          FORMAT( ////////,' Unable to find file => ',(A))
            STOP ' Please create the data files.'
        else
            OPEN(I,FILE=FNAME,STATUS='OLD')
        ENDIF
    ELSEIF(J .EQ. 2) THEN
        OPEN(I,FILE=FNAME,STATUS='UNKNOWN')
    ENDIF




    RETURN
    END


    !      ****************************************************************
    !
    SUBROUTINE CLOSE_FILES(I)
    !
    !      ***************************************************************
    Implicit None
    INTEGER FILE_CODE, I

    File_CODE = 1

    IF(I .EQ. 1) THEN
        CLOSE(1,STATUS='KEEP')
        CLOSE(2,STATUS='KEEP')
        CLOSE(3,STATUS='KEEP')
        CLOSE(4,STATUS='KEEP')
        CLOSE(5,STATUS='KEEP')
        CLOSE(6,STATUS='KEEP')
        CLOSE(7,STATUS='KEEP')
    ENDIF



    RETURN
    END


    !       ***************************************************************
    !
    SUBROUTINE FILCRENEW (VFILE,XFILE,FNAME)
    !
    !       ***************************************************************
    Implicit None


    CHARACTER*1 XFILE(4)
    CHARACTER*1 FNAME(14)

    CHARACTER*1 VFILE(9)

    CHARACTER CH
    character dh
    character dh1
    INTEGER I

    CH=CHAR(27)
    dh=char(13)
    DH1=CHAR(32)


    DO 10 I=1,9

        IF (vFILE(I) .NE. DH1) THEN
            FNAME(I)=vFILE(I)
        ELSEIF (vFILE(I) .EQ. DH1) THEN
            IF (I .EQ. 1) THEN
                FNAME(1)=XFILE(1)
                FNAME(2)=XFILE(2)
                FNAME(3)=XFILE(3)
                FNAME(4)=XFILE(4)
            ELSEIF (I .EQ. 2) THEN
                FNAME(2)=XFILE(1)
                FNAME(3)=XFILE(2)
                FNAME(4)=XFILE(3)
                FNAME(5)=XFILE(4)
            ELSEIF (I .EQ. 3) THEN
                FNAME(3)=XFILE(1)
                FNAME(4)=XFILE(2)
                FNAME(5)=XFILE(3)
                FNAME(6)=XFILE(4)
            ELSEIF (I .EQ. 4) THEN
                FNAME(4)=XFILE(1)
                FNAME(5)=XFILE(2)
                FNAME(6)=XFILE(3)
                FNAME(7)=XFILE(4)
            ELSEIF (I .EQ. 5) THEN
                FNAME(5)=XFILE(1)
                FNAME(6)=XFILE(2)
                FNAME(7)=XFILE(3)
                FNAME(8)=XFILE(4)
            ELSEIF (I .EQ. 6) THEN
                FNAME(6)=XFILE(1)
                FNAME(7)=XFILE(2)
                FNAME(8)=XFILE(3)
                FNAME(9)=XFILE(4)
            ELSEIF (I .EQ. 7) THEN
                FNAME(7)=XFILE(1)
                FNAME(8)=XFILE(2)
                FNAME(9)=XFILE(3)
                FNAME(10)=XFILE(4)
            ELSEIF (I .EQ. 8) THEN
                FNAME(8)=XFILE(1)
                FNAME(9)=XFILE(2)
                FNAME(10)=XFILE(3)
                FNAME(11)=XFILE(4)
            ELSEIF (I .EQ. 9) THEN
                FNAME(9)=XFILE(1)
                FNAME(10)=XFILE(2)
                FNAME(11)=XFILE(3)
                FNAME(12)=XFILE(4)
            ENDIF
30          FORMAT(A4)
            GOTO 40
        ENDIF

10  CONTINUE
40  CONTINUE

20  FORMAT(A1)


    RETURN



    end subroutine
    end module