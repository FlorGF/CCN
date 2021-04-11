module my_funcs_parcial
implicit none 
integer, parameter :: order=2
contains 
function deriv(x, y)
    real(8), intent(in) :: x 
    real(8), dimension(order), intent(in) :: y !y=(x,v)
    real(8), dimension(order) :: deriv
    !defino cosntantes 
    real(8) :: c1, c2 
    !c2 es la constante definida como b/m y c1=k/m 
    c1=225.0  
    c2=1.0  

    deriv(1)=y(2)
    deriv(2)=-c2*y(2)-c1*y(1)




end function 

end module 
