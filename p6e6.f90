program p6e5 
    use metodo_integrales 
    implicit none 

    integer :: N, i 
    Real(8) :: a, G, pi, h
    real(8), dimension(21) :: x
    !en este caso G=integ(para los problemas anteriores) asi tiene coherencia con el enunciado 
    pi=4.0*atan(1.0)
    h=pi/21
    
    do i=0, 21
        x(i)=pi/2+i*h 
    enddo

    N=51
    a=0.0 !extremo inferior de la integral 
    open(unit=10 , file="resultadose6.dat")
    do i=1, 21 
        call simpson(a, x(i), N, G)
        write(*,*)x(i), cos(x(i)), G 
        write(10,*)x(i), cos(x(i)), G 


    enddo 
    close(11)





end program 