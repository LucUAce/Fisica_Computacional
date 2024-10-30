C Metode ENCERT/FALLADA dins el main!!!


       Real*8 M, x, p, a,b,nup, nd, desv1
       Integer N, Ndins
       Real*8 f
       EXTERNAL f


C      M cota de la funció f
       M=2.d0
       Ndins=0

       DO N= 1, 300000
              x = rand()*(b-a)+a
              p = rand()*M

              IF(f(x).GE.p) Ndins=Ndins+1

              IF(mod(N,10000).eq.0) THEN
                     nup = dble(Ndins)/dble(N)
                     nd = M*(b-a)*nup

                     desv1 = M*(b-a)*dsqrt(nup*(1.d0-nup)/dble(N))

                     WRITE(15,160)N, nd, desv1
              ENDIF

       ENDDO
