module derivs
    implicit none 

contains 
function deriv(x, y)
    implicit none 
    real(8), intent(in) :: x 
    real(8), intent(in), dimension(2) :: y
    real(8), dimension(2) :: deriv
    deriv(1)=y(2)
    deriv(2)=-2*y(2)/x+2*y(1)/x**2+sin(log(x))/x**2 
end function 
end module 