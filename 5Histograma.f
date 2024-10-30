C GENERA HISTOGRAMA
C   IN: XDATA(NDAT) valors originals, XA i XB valors limit, NBOX numero de caixes
C   OUTPUT: XHIS(NBOX) valor central caixes, VHIS(NBOX) altura caixes,
C           ERRHIS(NBOX) estimacio error, BOXSIZE tamany de cada caixa
C Subrutina inspirada en proposada al CampusVirtual, de BJD 2018
       SUBROUTINE HISTOGRAMA(NDAT,XDATA,XA,XB,NBOX,XHIS,VHIS,ERRHIS,
     & BOXSIZE)
              IMPLICIT NONE

              INTEGER NDAT,NBOX, I,IBOX,ICOUNT
              Real*8 XDATA(NDAT),XA,XB, BOXSIZE
              Real*8 XHIS(NBOX),VHIS(NBOX),ERRHIS(NBOX)

C             ICOUNT sera el numero de punts dins l'interval
              BOXSIZE=(XB-XA)/NBOX
              ICOUNT=0

              DO I=1,NBOX
                     VHIS(I)=0
                     ERRHIS(I)=0
              ENDDO

C             IBOX caixa a la que "estem". Calculem les altures de les caixes.
              DO I=1,NDAT
                     IF (XDATA(I).GE.XA.AND.XDATA(I).LE.XB) THEN
                            IBOX=INT((XDATA(I)-XA)/BOXSIZE)+1
                     IF (IBOX.EQ.NBOX+1) IBOX=NBOX
                     VHIS(IBOX)=VHIS(IBOX)+1
                     ICOUNT=ICOUNT+1
                     ENDIF
              ENDDO

C             Escriu quants punts hi ha dins l'interval
              PRINT*,"ACCEPTED:",ICOUNT," OUT OF:",NDAT

C             Calcula els valors centrals, els erros i normalitza les altures
              DO I=1,NBOX
                     XHIS(I)=XA+BOXSIZE/2.D0+dble(I-1)*BOXSIZE
                   ERRHIS(I)=dSQRT(VHIS(I)/ICOUNT*(1.D0-VHIS(I)/ICOUNT))
                     ERRHIS(I)=ERRHIS(I)/(BOXSIZE*dSQRT(DBLE(ICOUNT)))
                     VHIS(I)=VHIS(I)/(dble(ICOUNT)*BOXSIZE)
              ENDDO

        RETURN
        END

C PARA ESCRIBIR EN EL MAIN:
C      DO I=1,nbox
C              WRITE(15,200) XHIS(I),VHIS(I),ERRHIS(I)
C 200           FORMAT(3(f20.12, 2x))
C       ENDDO

