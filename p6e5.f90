program p6e5
    use metodo_integrales 
    implicit none 

    integer :: N 
    real(8) :: Iexac, a, b, err, integ, pi 

    pi=4.0*atan(1.0)
    a=0
    b=pi
    Iexac=0.644005 
    err=0.0001 

    N=1 

    do 
        
        call trapecio(a, b, integ, N)
        write(*,*)abs(Iexac-integ), N 
        if(abs(Iexac-integ)<err)then 
            exit 
        else 
            N=N+1 
        endif
    enddo 
    write(*,*)"El valor de la integral es", integ, "con un error de", abs(Iexac-integ)
    write(*,*)"El número de subintervalos", N 





end program 