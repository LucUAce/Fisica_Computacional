
C METODE BISECCIO, per trobar arrel funció fun
C IN: A,B valors inicials, eps tolerància, fun(x) funció;
C OUT: niter iteracions, xarrel valor de l'arrel
       SUBROUTINE Bisection (A, B, eps, fun, niter, xarrel)
              IMPLICIT NONE
              Real*8 A,B, eps, xarrel, itmax, fun
              Integer niter
              EXTERNAL fun

              IF((fun(A)*fun(B)).GE.(0.d0)) THEN
                     WRITE(*,*) 'A I B NO SON VALORS ACCEPTABLES PER REA
     &LITZAR EL METODE DE LA BISECCIÓ'
                     RETURN
              ENDIF

              itmax= dlog((B-A)/dabs(eps))/dlog(2.d0)

              DO niter = 1, int(itmax)+1
                     xarrel= (B+A)/2.d0

                   IF((dabs(fun(X)).LE.eps).OR.(dabs(B-A).LE.eps)) THEN
                            RETURN
                   ELSE IF((fun(A)*fun(X)).LE.(0.d0)) THEN
                            B=xarrel
                   ELSE IF((fun(X)*fun(B)).LE.(0.d0)) THEN
                            A=xarrel
                   ENDIF
              ENDDO

       RETURN
       END



C METODE BISECCIO, per trobar arrel funció fun
C IN: A,B valors inicials, eps tolerància, fun subrutina amb in: x, out: f(x), f'(x);
C OUT: niter iteracions, xarrel valor de l'arrel
       SUBROUTINE Bisection (A, B, eps, fun, niter, xarrel)
              IMPLICIT NONE
              Real*8 A,B, eps, xarrel, fuA, fuB, fuX, dfu, itmax
              Integer niter

              CALL fun(A, fuA, dfu)
              CALL fun(B, fuB, dfu)
              IF((fuA*fuB).GE.(0.d0)) THEN
                     WRITE(*,*) 'A I B NO SON VALORS ACCEPTABLES PER REA
     &LITZAR EL METODE DE LA BISECCIÓ'
                     RETURN
              ENDIF

              itmax= dlog((B-A)/dabs(eps))/dlog(2.d0)
              DO niter = 1, int(itmax)+1
                     xarrel= (B+A)/2.d0

                     CALL fun(A, fuA, dfu)
                     CALL fun(B, fuB, dfu)
                     CALL fun(xarrel, fuX, dfu)
                     IF((dabs(fuX).LE.eps).OR.(dabs(B-A).LE.eps)) THEN
                             RETURN
                             ELSE IF((fuA*fuX).LE.(0.d0)) THEN
                                   B=xarrel
                                   ELSE IF((fuX*fuB).LE.(0.d0)) THEN
                                          A=xarrel

                     ENDIF
              ENDDO

       RETURN
       END
