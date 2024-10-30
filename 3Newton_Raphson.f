
C METODE NEWTON-RAPHSON, per trobar arrel de funció fun
C IN: x0 valor inicial, eps tolerància, fun subrutina amb in: x, out: f(x), f'(x)
C OUT: niter iteracions, xarrel valor de l'arrel
C      escriu en fitxer 15 niter, xarrel i (xarrel-x0) per cada iteracio
      SUBROUTINE NewtonRap (x0, eps, fun, niter, xarrel)
              IMPLICIT NONE
              Real*8 x0, eps, xarrel, fu, dfu
              Integer niter, NMAX
              NMAX = 10000


              DO NITER=1, NMAX
                     CALL fun(x0, fu, dfu)
                     xarrel = x0 - fu/dfu
                     WRITE(15, 400) niter, xarrel, dabs(xarrel-x0)
400                  FORMAT(i6, 2x, e20.12, e20.12)

                     IF ((dabs(xarrel-X0)).GE.(dabs(eps)).AND.
     & (dabs(fu).GE.(dabs(EPS)))) THEN
                            x0=xarrel
                            ELSE
                                   RETURN
                     ENDIF
              ENDDO

       RETURN
       END



C UNA ITERACIO METODE NEWTON-RAPHSON, per trobar arrel de funció fun
C IN: x0 valor inicial, eps tolerància, fun subrutina amb in: x, out: f(x), f'(x)
C OUT: xarrel valor de l'arrel
      SUBROUTINE NewtonRap (x0, eps, fun, xarrel)
              IMPLICIT NONE
              Real*8 x0, xarrel, fu, dfu

              CALL fun(x0, fu, dfu)
              xarrel = x0 - fu/dfu
C      PARA MAIN?
C              WRITE(15, 400) xarrel, dabs(xarrel-x0)
C 400           FORMAT(e20.12,2x, e20.12)

C      IF((dabs(xarrel-X0)).GE.(dabs(eps)).AND.(dabs(fu).GE.(dabs(EPS)))) THEN
C             x0=xarrel
C      ELSE
C              RETURN
C      ENDIF

       RETURN
       END
