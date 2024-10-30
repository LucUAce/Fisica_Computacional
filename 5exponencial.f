C Distribucio EXPONENCIAL(xlambda), pel metode CANVI VARIABLE
C   IN: ndat, xlambda
C   OUT: xexpo(ndat) nombres aleatoris amb dist exponencial
       SUBROUTINE subexpo(ndat,xlambda,xexpo)
              IMPLICIT NONE
              INTEGER ndat, i
              Real*8 xlambda, xexpo(ndat)

              DO I=1, ndat
                     xexpo(i) = -1.d0*log(rand())/xlambda
              ENDDO

       RETURN
       END

