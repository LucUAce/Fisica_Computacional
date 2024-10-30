
C Calcula integral de funci per metode TRAPEZIS composta amb 2^k intervals.
C IN: x1, x2 limits integral, k, funci(x) function
C OUT: integral (valor aprox de la integral)
      SUBROUTINE trapezoids(x1,x2,k,funci,integral)
              IMPLICIT NONE
C declaracio de variables, parametres, external, common,...
              Real*8 x1, x2, funci, integral, h
              Integer k, i

              h= dabs(x2-x1)/dble(2.d0**k)

              integral = (funci(x1)+funci(x2))*0.5d0

              DO i=1, 2**k-1
                     integral=integral + funci(x1+h*dble(i))
              ENDDO

              integral = dble(h)*integral

       RETURN
       END

