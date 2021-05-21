module funciones
    implicit none 
contains 

function f(x)
    
    real(8) :: f
    real(8), intent(in) :: x 
    real(8), parameter :: pi=4.0*atan(1.0)
    f=x*x-pi*pi 
end function 

function g(x,i)
    real(8) :: g 
    real(8), intent(in) :: x
    integer, intent(in) :: i 
   ! real(8) :: f 
    g=f(x)*cos(i*x)

end function 

end module 