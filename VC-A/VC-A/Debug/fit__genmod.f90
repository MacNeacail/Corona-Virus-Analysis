        !COMPILER-GENERATED INTERFACE MODULE: Sat May 16 12:13:13 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE FIT__genmod
          INTERFACE 
            SUBROUTINE FIT(X,Y,NDATA,SIG,MWT,A,B,SIGA,SIGB,CHI2,Q)
              INTEGER(KIND=4) :: NDATA
              REAL(KIND=4) :: X(NDATA)
              REAL(KIND=4) :: Y(NDATA)
              REAL(KIND=4) :: SIG(NDATA)
              INTEGER(KIND=4) :: MWT
              REAL(KIND=4) :: A
              REAL(KIND=4) :: B
              REAL(KIND=4) :: SIGA
              REAL(KIND=4) :: SIGB
              REAL(KIND=4) :: CHI2
              REAL(KIND=4) :: Q
            END SUBROUTINE FIT
          END INTERFACE 
        END MODULE FIT__genmod
