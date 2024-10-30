
C Calcula integral de funci per metode SIMPSON composta amb 2^k intervals.
C IN: x1, x2 limits integral, k, funci(x) function
C OUT: integral (valor aprox de la integral)
      SUBROUTINE simpson(x1,x2,k,funci,integral)
              IMPLICIT NONE
C declaracio de variables, parametres, external, common,...
              Real*8 x1, x2, funci, integral, h
              Integer k, i

              h= dabs(x2-x1)/dble(2.d0**k)

              integral = funci(x1)+funci(x2)

              DO i=1, 2**k-1
                     IF(mod(i, 2).EQ.1) THEN
                            integral=integral+4.d0*funci(x1+h*dble(i))
                     ELSE
                            integral=integral+2.d0*funci(x1+h*dble(i))
                     ENDIF
              ENDDO

              integral = dble(h)*integral/3.d0

       RETURN
       END
