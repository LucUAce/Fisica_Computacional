C  Retorna les derivades dels valors del vector Y()
C IN: NEQU nombre d'ecuacions, X variable independent, Y(NEQU) vector amb les variables depenends
C OUT: DY(NEQU) vector amb les derivades de Y()
C En aquest cas NEQU=2, i apliquem la funcio del PENDOL, amb un common per les constants
C  Adaptació de la subrutina que apareix a les diapositives de classe
       SUBROUTINE DERIVADES(NEQU,X,Y,DY)
       IMPLICIT NONE
       INTEGER NEQU
       Real*8 x,Y(NEQU),DY(NEQU)
       Real*8 M,L,G
       COMMON/dades/ M,L, G

       DY(1)=Y(2)
       DY(2)=-G*dsin(Y(1))/L

       RETURN
       END
