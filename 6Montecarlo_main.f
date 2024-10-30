C MONTECARLO CRU 1 FUNCION, K, DINS EL MAIN
C ((ABANS CAL FER CANVI DE VARIABLE EN K))
C escriu en FITXER 15
       Real*8 h1up, h2up,x,nup, desv1
       Integer N, Nmax
       Real*8 K
       External K
       Real*8 M,L,g, w0

       Nmax=350

       h1up=0.d0
       h2up=0.d0
       DO N=1, Nmax1

C             Para MONTECARLO ASSAIG IMPORTANCIA, poner x = distribucion (vector con argumento N)
C             Para MONTECARLO MULTIDIMENSIONAL multiplicar N por la dim. i restar 0,1,2,..N-1
              x= rand()
              nup = K(x)
C             SI no s'ha fet canvi variable abans: nup= K((x2-x1)*X+x1)*(x2-x1)
              h1up = h1up + nup
              h2up = h2up + nup*nup

C             Escriu cada 150 pasos
              IF(mod(N,150).eq.0) THEN
                     nup= h1up/dble(N)

                     desv1 = dsqrt(h2up-h1up*h1up/dble(N))/dble(N)

                     WRITE(15,140)N, nup, desv1
140                  FORMAT(I7, 2(2x, f20.12))
              ENDIF
       ENDDO








C MONTECARLO CRU para 2 FUNCIONES, up i down, DINS EL MAIN
C escriu en FITXER 15

       Real*8 h1up, h2up, h1d, h2d,x,nup, nd, desv1, desv2
       Integer N, Nmax
       Real*8 up, down
       External up, down


       h1up=0.d0
       h2up=0.d0
       h1d=0.d0
       h2d=0.d0
       DO N=1, Nmax1

C             Para MONTECARLO ASSAIG IMPORTANCIA, poner x = distribucion (vector con argumento N)
C             Para MONTECARLO MULTIDIMENSIONAL multiplicar N por la dim. i restar 0,1,2,..N-1
              x= rand()
              nup = up(x)
              h1up = h1up + nup
              h2up = h2up + nup*nup

              x= rand()
              nd = down(x)
              h1d = h1d + nd
              h2d = h2d + nd*nd

              IF(mod(N,150).eq.0) THEN
                     nup= h1up/dble(N)
                     nd= h1d/dble(N)

                     desv1 = dsqrt(h2up-h1up*h1up/dble(N))/dble(N)
                     desv2 = dsqrt(h2d-h1d*h1d/dble(N))/dble(N)

                     WRITE(15,140)N, nup, desv1, nd, desv2
140                  FORMAT(I7, 4(2x, f20.12))
              ENDIF
       ENDDO


