C METODE D’ACCEPTACIO I REBUIG
C   IN: a i b limits del suport, M cota superior, fun(x) function
C   OUT: xnums(ndat) nombres aleatoris amb distribucio fun
       SUBROUTINE acceptrebuig (ndat, xnums, a, b, M, fun)
              IMPLICIT NONE

              INTEGER ndat, i, ISEED
              Real*8 xnums(ndat), a, b, M, x, p
              Real*8 fun
C             Real*8 var, mitja

              ISEED=20231492
              CALL SRAND(ISEED)

              i=1
999           CONTINUE
              x= (b-a)*rand() +a
              p= M*rand()
              IF(fun(x).GE.p) THEN
                     xnums(i) = x
                     i=i+1
              ENDIF
              IF(i.LE.ndat) GO TO 999

C              var=0.d0
C              mitja=0.d0
C              DO I=1, ndat
C                     var = var + xnums(i)*xnums(i)
C                     mitja = mitja + xnums(i)
C              ENDDO
C              var= var/dble(ndat)
C              mitja= mitja/dble(ndat)
C              var= var-mitja*mitja

C              WRITE(15,*)'#valor mitja, varirancia, desviacio estandard'
C              WRITE(15,150) mitja, var, dsqrt(var)
C150           FORMAT(3(f20.12, 2x))

       RETURN
       END

