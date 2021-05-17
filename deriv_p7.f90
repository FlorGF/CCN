module deriv_p7 
implicit none
contains 
function deriv(x, y)
real(8), intent(in) :: x
real(8), intent(in), dimension(2) :: y 
real(8), dimension(2) :: deriv 

!problema 2
!deriv(1)=y(2)
!deriv(2)=0.15*y(1)
!problema 7 
!deriv(1)=y(2)
!deriv(2)=x+3.0+2.0*x*y(2)-y(1)*x**2
!problema uno final 26/2/2021
deriv(1)=y(2)
deriv(2)=0.2*x*x-3*sqrt(x)
end function
!problema 7 



end module 