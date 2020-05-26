        !COMPILER-GENERATED INTERFACE MODULE: Thu May 07 17:21:40 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE MOMENT__genmod
          INTERFACE 
            SUBROUTINE MOMENT(DATA,N,AVE,ADEV,SDEV,VAR,SKEW,CURT)
              INTEGER(KIND=4) :: N
              REAL(KIND=4) :: DATA(N)
              REAL(KIND=4) :: AVE
              REAL(KIND=4) :: ADEV
              REAL(KIND=4) :: SDEV
              REAL(KIND=4) :: VAR
              REAL(KIND=4) :: SKEW
              REAL(KIND=4) :: CURT
            END SUBROUTINE MOMENT
          END INTERFACE 
        END MODULE MOMENT__genmod
