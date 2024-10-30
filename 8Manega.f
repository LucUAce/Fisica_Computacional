

111    FORMAT( '#Per N =', I3)
222    FORMAT('#Busquem energia estat nº =', I3)
333    FORMAT(f18.7, 2x, f18.7)

       DO k=1, 2
              WRITE (15,111) N(k)
              DO j=1, 4
                     WRITE(15, 222) j
C                    Valor 'arbitrari' (és el valor exacte analitic) CANVIAR
                     E = -1.3d0 + dble(j)**2*8.d0*datan(1.d0)**2
                     E1=E
                     x0=0.d0
                     xmax=1.d0

C                    CAlculem E1 (No he calculat E1 i E2 junts només per no declarar un vector més)
                     yyin(1)=0.d0
                     yyin(2)= 0.25d0
                     xi=x0
                     h = (xmax-x0)/dble(N(k)-1)
                     DO i=1, N(k)
                            CALL RK4 (xi,h,2,yyin,yyout)

                            yyin(1)=yyout(1)
                            yyin(2)=yyout(2)

                            xi= x0 + dble(i)*h
                     ENDDO
                     phiE1 = yyout(1)
                     WRITE (15, *) E1

C                    CAlculem E2
                     E = E+0.2d0
                     E2=E
                     yyin(1)=0.d0
                     yyin(2)= 0.25d0
                     xi=x0
                     DO i=1, N(k)
                            CALL RK4 (xi,h,2,yyin,yyout)

                            yyin(1)=yyout(1)
                            yyin(2)=yyout(2)

                            xi= x0 + dble(i)*h
                     ENDDO
                     phiE2 = yyout(1)
                     WRITE (15, *) E2

777                  CONTINUE

C                     Calculem E3
                     E = (E1*phiE2-E2*phiE1)/(phiE2-phiE1)
                     WRITE(15,*) E

                     yyin(1)=0.d0
                     yyin(2)= 0.25d0
                     xi=x0
                     DO i=1, N(k)
                            CALL RK4 (xi,h,2,yyin,yyout)

                            yyin(1)=yyout(1)
                            yyin(2)=yyout(2)

                            vep(i) = yyout(1)

                            xi= x0 + dble(i)*h
                     ENDDO

                     IF (dabs(yyout(1)).GT.(0.00001d0)) THEN
                            E1 = E2
                            phiE1 = phiE2
                            E2 = E
                            phiE2 = yyout(1)
                            GO TO 777
                     ENDIF
                     WRITE(15, *)
                     WRITE(15, *)
                     DO i=1, N(k)
                            vep2(i) = dabs(vep(i))**2
                     ENDDO
                     CALL simpson(x0,xmax,N(k),vep2,Normal)
                     WRITE(15,*) '#Valors autovector'
                     DO i=1, N(k)
                            vep(i) = vep(i)/dsqrt(Normal)
                            WRITE(15,333) x0+dble(i-1)*h, vep(i)
                     ENDDO
                     WRITE(15, *)
                     WRITE(15, *)
              ENDDO
              WRITE(15, *)
              WRITE(15, *)
        ENDDO
