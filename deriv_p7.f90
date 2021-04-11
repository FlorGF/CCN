module deriv_p7 
implicit none
contains 
function deriv(x, y)
real(8), intent(in) :: x
real(8), intent(in), dimension(2) :: y 
real(8), dimension(2) :: deriv 

!problema 2
deriv(1)=y(2)
deriv(2)=0.15*y(1)
end function
!


end module 