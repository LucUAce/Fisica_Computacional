
C INTERPOLACIO ORDRE 0. ENTRAR COORDENADAS (TI, XI) POR COMMON
c t ES EL TIEMPO (X) PARA EL QUE QUEREMOS INTERPOLAR, IT NUMERO DE PUNTOS
       REAL*8 FUNCTION xinterpo0(t)
              Integer i, IT
              PARAMETER (IT=80)
              Real*8 t, TI(IT+1), XI(IT+1)
              COMMON/POSIS/XI,TI

c NO CAL, PERO COMPROVA SI PODEM INTERPOLAR LA FUNCIO EN EL TEMPS t
c              IF((t.LT.TI(1)).OR.(t.GT.TI(IT+1))) THEN
c                     WRITE(*,300)
c300                  FORMAT('TEMPS FORA DEL DOMINI')
c                     STOP
c              ENDIF

c BUSQUEM L'INTERVAL ON ESTA t
              DO I= 1, IT
                     IF(TI(I+1).GT.t) EXIT
              ENDDO

c INTERPOLACIO ordre zero
              xinterpo0 = XI(I)

       RETURN
       END



c INTERPOLACIO LINEAL, ENTRAR COORDENADAS (TI, XI) POR COMMON
c t ES EL TIEMPO (X) PARA EL QUE QUEREMOS INTERPOLAR, IT NUMERO DE PUNTOS
       REAL*8 FUNCTION xinterpo(t)
              Integer i, IT
              PARAMETER (IT=80)
              Real*8 t, p1, p2, TI(IT+1), XI(IT+1)
              COMMON/POSIS/XI,TI

c NO CAL, PERO COMPROVA SI PODEM INTERPOLAR LA FUNCIO EN EL TEMPS t
c              IF((t.LT.TI(1)).OR.(t.GT.TI(IT+1))) THEN
c                     WRITE(*,300)
c300                  FORMAT('TEMPS FORA DEL DOMINI')
c                     STOP
c              ENDIF

c BUSQUEM L'INTERVAL ON ESTA t (realment p1 i p2 son rebundants, pero aixi em sembla mes entenedor)
              DO I= 1, IT
                     p1=TI(I)
                     p2=TI(I+1)
                     IF(P2.GT.t) EXIT
              ENDDO

c INTERPOLACIO LINEAL
              xinterpo = XI(I) + (t-P1)*(XI(I+1)-XI(I))/(p2-p1)

       RETURN
       END
