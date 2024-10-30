C RK2 retorna el següent pas d'integració
C IN: X, Yin(NEQU) valors d'entrada variable indep. i dependent, resp.; h pas integracio; NEQU nombre de var. dependents
C OUT: Yout(NEQU) vector amb els valors de sortida
c en aquest cas NEQU=2
C Crida la subrutina DERIVADES
       SUBROUTINE RK2(X, Yin, h, NEQU, Yout)
       IMPLICIT NONE
       INTEGER NEQU
       REAL*8 X, H, fe1, fd1, fd2
       REAL*8 Yin(NEQU), Yout(NEQU), f(NEQU), fd(NEQU)


       fe1= x+h/2.d0

       CALL DERIVADES(NEQU, X, Yin, f)
       fd1 = Yin(1) + 0.5d0* h* f(1)
       fd2 = Yin(2) + 0.5d0* h* f(2)
       fd(1)=fd1
       fd(2)=fd2

       CALL DERIVADES(NEQU, fe1, fd, f)
       YOUT(1) = Yin(1) + h* f(1)
       YOUT(2) = Yin(2) + h* f(2)


       RETURN
       END



C      AL MAIN
       h= (15.d0*Tn-T0)/dble(N-1)
       WRITE(15,333) T0, phin(1), phin(2)
       Ti=T0
       DO i=1, N
C             RK2
              CALL RK2(Ti, phin, h, 2, phiout)
              phin(1)= phiout(1)
              phin(2) = phiout(2)

C             Actualitzem temps
              Ti = T0 + dble(i-1)*h

              WRITE(15,333) Ti, phin(1), phin(2)
       ENDDO

       WRITE(15, *)
       WRITE(15, *)

