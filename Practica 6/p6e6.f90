program p6e6
    use metodo_integrales 
    implicit none 

    integer :: N, i 
    Real(8) :: a, pi, h, G_int, tol, raiz, err, iter 
    real(8), dimension(21) :: x
    
    !en este caso G=integ(para los problemas anteriores) asi tiene coherencia con el enunciado 
    pi=4.0*atan(1.0)
    h=pi/21
    tol=0.0001
    do i=0, 21
        x(i)=pi/2+i*h 
    enddo

    N=51
    a=0.0 !extremo inferior de la integral 
    open(unit=10 , file="g_p6e6.dat")
    open(unit=11 , file="f_p6e6.dat")
    do i=1, 21 
        call simpson(a, x(i), N, G_int)
        write(11,*)x(i), cos(x(i)) !aca pongo los datos de la funcion f  (coseno)
        write(10,*)x(i), G_int !y aca los datos de la funcion g (integral)


    enddo 
    close(11)
    close(10)
!ejercicio 7
    call biseccion(x(1), x(21), tol, raiz, err, iter)
    write(*,*)"El cero se encuentran en el punto", raiz, "con un error de", err 
    write(*,*)"Se realizaron", iter, "iteraciones"




end program 



subroutine biseccion(a, b, tol, x, err, iter)
    
    implicit none 
    real(8), intent(inout) :: a, b, tol 
    real(8), intent(out) ::x, err 
    real(8) :: c, iter, f_b

    !me fijo si se le puede aplicar este método 
    if(sign(1.0_8,f_b(a))==sign(1.0_8,f_b(b)))then 
        write(*,*)"No se le puede aplicar el método"
        stop  
    endif 

    !construyo la iteración 
    c=(b+a)/2 !seteo el primer valor de c
    iter=0 
   
    do 
    
         if(abs(b-a)/2<tol .and. abs(f_b(c))<tol)then
             x=c 
            err=abs(b-a)/2
            exit 
        elseif(sign(1.0_8,f_b(c))==sign(1.0_8,f_b(a)))then 
            
            a=c
        else      !cambio los intervalos
            
            b=c
        endif 
        c=(b+a)/2 !cambio c 
        iter=iter+1.0
    enddo 

end subroutine 

real(8) function f_b(x)
use metodo_integrales
    implicit none 
    real(8), intent(in) :: x 
    !real(8), intent(out) :: f_b 
    real(8) :: a, integ 
    integer :: N 
    N=51
    a=0.0 
    call simpson(a, x, N, integ)
    f_b=integ
end function