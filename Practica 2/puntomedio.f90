subroutine puntomedio(x, y, h)
    use my_funcs 
    implicit none 
    real(8), intent(inout) :: x
    real(8), intent(inout), dimension(order) :: y 
    real(8), intent(in) :: h !este es el paso 
    real(8), dimension(order) ::  k1, k2

    k1=deriv(x,y)
    k2=deriv(x+h/2,y+h/2) 
    y=y+k2*h
    x=x+h

end subroutine 