
c      AL MAIN, EULER (+RK2)
       N =1500
       phi0= pi-0.025d0
       dphi0=0.12d0
       phin(1) = phi0
       phin(2) = dphi0

       WRITE(15,*)'#Metode Euler i RK2, phi(0) = pi-0.025, dphi(0)=0.12'
       WRITE(15,*)'# temps t, phi(t) Euler, phi(t) RK2'

       h= (7.d0*Tn-T0)/dble(N-1)
       WRITE(15,333)T0,phi0,phin(1)
       Ti=T0
       DO i=1, N
C             Euler
              phi1=phi0
              phi0 = phi0+h*dphi0
              dphi0= dphi0-h*G*dsin(phi1)/L

C             RK2
              CALL RK2(Ti, phin, h, 2, phiout)
              phin(1)= phiout(1)
              phin(2) = phiout(2)

C             Actualitzem temps
              Ti = T0 + dble(i-1)*h

              WRITE(15,333)T0,phi0,phin(1)
       ENDDO

       WRITE(15, *)
       WRITE(15, *)




C      AL MAIN EULER MILLORAT
       phi0=pi-0.035d0
       dphi0=0.d0
       WRITE(15, *)
       WRITE(15, *)
       WRITE(15,*)'#a. Metode Euler Millorat, phi(0) = pi-0.035'
       WRITE(15,*)'# temps t, phi(t), derivada phi(t)'

       WRITE(15,333) T0, phi0, dphi0
       Ti= T0+ h
       phi1=phi0
       dphi1=dphi0
       phi0 = phi1+h*dphi1
       dphi0= dphi1+h*ddphi(phi1)

       WRITE(15,333) Ti, phi0, dphi0

       DO i=3, N
              Ti = Ti + h

              phi2 = phi1
              dphi2 = dphi1
              phi1=phi0
              dphi1 = dphi0

              phi0 = phi2+2.d0*h*dphi1
              dphi0= dphi2+2.d0*h*ddphi(phi1)

              WRITE(15,333) Ti, phi0, dphi0
       ENDDO
