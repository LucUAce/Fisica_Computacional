
C Metode RK4 (una iteració)
C IN:  x yyin(NEQUS) valors d'entrada variable indep. i vector de depenents, resp.;
C      NEQUS nombre de var. dependents, dx pas integració (h)
C OUT: yyout(NEQUS) vector amb els valors de sortida
C NECESITA SUBRUTINA DERIVADES
       SUBROUTINE  RK4 (x,dx,NEQUS,yyin,yyout)
              IMPLICIT NONE
              Real*8 x, dx
              INTEGER NEQUS, i
              Real*8 yyin(NEQUS), yyout(NEQUS), youtaux(NEQUS)
              Real*8 k1(NEQUS), k2(NEQUS), k3(NEQUS), k4(NEQUS)

C                    Calcul k1
                     CALL DERIVADES(NEQUS, x, yyin, yyout)
                     DO i=1, NEQUS
                            k1(i) = yyout(i)
C                    'Preprarem' els arguments per k2
                            youtaux(i) = yyin(i) + dx*k1(i)/2.d0
                     ENDDO

C                    Calcul k2 (+'preparar' per k3)
                     CALL DERIVADES(NEQUS, x+dx/2.d0, youtaux, yyout)
                     DO i=1 , NEQUS
                            k2(i) = yyout(i)

                            youtaux(i) = yyin(i) + dx*k2(i)/2.d0
                     ENDDO

C                    Calcul k3 ('preparar' per k4)
                     CALL DERIVADES(NEQUS, x+dx/2.d0, youtaux, yyout)
                     DO i=1 , NEQUS
                            k3(i) = yyout(i)

                            youtaux(i) = yyin(i) + dx*k3(i)
                     ENDDO

C                    Calcul k4
                     CALL DERIVADES(NEQUS, x+dx, youtaux, yyout)
                     DO i=1 , NEQUS
                            k4(i) = yyout(i)
                     ENDDO

C                    Calcul yyout
                     DO i=1 , NEQUS
                            yyout(i) = k1(i)+2.d0*k2(i)+2.d0*k3(i)+k4(i)
                            yyout(i) = yyin(i) + yyout(i)*dx/6.d0
                     ENDDO

       RETURN
       END

