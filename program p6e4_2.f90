program p6e4_2
    use metodo_integrales 
    implicit none 
!lo mismo que el otro programa pero usando el método de simpsn
    integer :: N, i 
    real(8) :: err, integ, a, b, pi, Iexac, aux
    !err= error de la integral, integ=valor de la integral calculado por los métodos
    ! a y b son los extremos de la integral y N la cantidad de subdivisiones del intervalo, Iexac= valor exacto de la integral

    !en este programa no sabemos con exactitud N, lo tenemos que determinar 

    pi=4.0*atan(1.0)
    Iexac=pi/4.0 
    !err=2*10**(-6)
    err=0.000001
    !write(*,*)"error", err 
    a=0.0
    b=1.0

    !seteo para que arranque en un valor mínimo de N
    N=1 

    !ahora creo un bucle que se detenga cuando el error sea el requerido 
    do 
        ! write(*,*)"holaa"
        if(mod(N,2)==0)then 
            N=N+1
        else 
         call simpson(a, b, integ, N)
         aux=abs(Iexac-integ)
         ! write(*,*)aux, N 
       
         if(aux<err)then 
             exit
         else 
             N=N+1 
         endif
        endif 
     enddo 
   

    write(*,*)"El valor de la integral es", integ 
    write(*,*)"Con un error de :", aux
    write(*,*)"la cantidad de subdivisiones que se hicieron fue", N 