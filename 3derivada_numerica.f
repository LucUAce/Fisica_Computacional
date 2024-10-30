
C DERIVADA NUMERICA.
C IN: ndat dimensio/numero punts, x(ndat) VECTOR amb els punts, fu(ndat) VECTOR amb la funció evaluada en els punts
c OUT: dfu(ndat) VECTOR amb la derivada de fu en els punts
       SUBROUTINE derfun (ndat, x,fu, dfu)
              IMPLICIT NONE
              Integer ndat, i
              Real*8 x(ndat), fu(ndat), dfu(ndat), h

              h= x(2)-x(1)
              dfu(1) = (fu(2)-fu(1))/h
              dfu(ndat) = (fu(ndat)-fu(ndat-1))/h

              DO i=2, ndat-1
                     dfu(i)=(fu(i+1)-fu(i-1))/(2.d0*h)
              ENDDO

       RETURN
       END
