

C Metode BOX-MULLER per generar valors dist. gaussiana
C    IN: ndat nombre de variables a generar, esperat valor esperat/mitja, desv desviacio estardard
C    OUT: xgaus(ndat) vector amb valors dist. gausiana
       SUBROUTINE BoxMuller(ndat, esperat, desv, xgaus)
              Real*8 desv, esperat, phi, r, y1, y2, pi
              Integer ndat, ISEED
              Real*8 xgaus(ndat)

              ISEED=20231492
              CALL SRAND(ISEED)

              pi= 4.d0*datan(1.d0)

              DO i=1, ndat-1, 2
                     phi=2.d0*pi*rand()
                     r=dsqrt(-2.d0*dlog(dble(rand())))

                     y1= r*dsin(phi)
                     y2= r*dcos(phi)
                     y1= y1*desv + esperat
                     y2= y2*desv + esperat

                     xgaus(i) = y1
                     xgaus(i+1) = y2
              ENDDO

       RETURN
       END

