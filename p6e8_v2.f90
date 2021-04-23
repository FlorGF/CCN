program p6e8
    implicit none
    real(8) :: a, b, integ 
    integer :: N 
    a=-25.0 
    b=25.0 
    N=51 
    call simpson(a, b, N, integ)
    write(*,*)"El valor de la inercia es", integ 



end program 
!----------------------------------------------------------------------------------------------------------
subroutine simpson(a, b, N, integ) 
    implicit none
    integer, intent(in) :: N 
    real(8), intent(in) :: a, b 
    real(8), intent(out) :: integ 
    !variables auxiliares
    real(8) :: h, sum, y, f 
    integer :: i, alfa 
    !este método tiene la restricción de q N es impar, primero hay que chequear esto 
    if(mod(N,2)==0)then 
        write(*,*)"N es par no se puede aplicar el método"
        stop !salgo del programa 
    else 
        write(*,*)"N es impar, estamos bien" 
    endif 
    h=(b-a)/(N-1) 
    sum=0 

    do i=2, N  !esta suma no deme incluir los valores extremos (los agrego a mano despues)
        y=a+(i-1)*h 
        if(mod(i,2)==0)then 
            sum=sum+2.0*f(y)
        else
            sum=sum+4.0*f(y)
        endif 
       ! write(*,*)"sum=", sum 
    enddo 
    sum=sum+f(a)+f(b)
    integ=sum*h/3.0 

end subroutine 
!----------------------------------------------------------------------------------------------
function f(y) !esta función es la que queda cuando se integra en x
    implicit none 
    real(8), intent(in) :: y
    real(8) :: f 
    real(8) :: a, b, sum, h, x  !extremos de la integral interna 
    integer :: N, i 
    real(8) :: g 
    N=51
    a=-sqrt(25**2-y**2) !calculo los extremos de la integral 
    b=sqrt(25**2-y**2)
    h=(b-a)/real(N-1)
    !hago metodo de simpson
    do i=2, N  
        x=a+(i-1)*h 
        if(mod(i,2)==0)then 
            sum=sum+2.0*g(x,y)
        else
            sum=sum+4.0*g(x,y)
        endif 
       
    enddo 
    sum=sum+g(a,y)+g(b,y)
    f=sum*h/3.0 
end function 
!----------------------------------------------------------------------------------------------
function g(x,y)
    implicit none 
    real(8), intent(in) :: x , y 
    real(8) :: g 
    !la función g va a ser la función que estamos integrando 
    !g=180.0 !para la masa 
    g=180.0*(x**2+y**2)!para la inercia 
end function 

