
C Energia cinetica
       Real*8 FUNCTION EKIN(dphi)

       IMPLICIT NONE
       Real*8 M,L,G, dphi
       COMMON/DADES/M,L,G

       EKIN=M*dphi*dphi*L*L/2.d0

       RETURN
       END


C energia potencial
       Real*8 FUNCTION EPOT(phi)

       IMPLICIT NONE
       Real*8 M,L,G, phi
       COMMON/DADES/M,L,G

       EPOT = -M*G*L*dcos(phi)

       RETURN
       END
