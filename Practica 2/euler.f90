subroutine euler(x, y, h) 
    !use my_funcs 
    implicit none 
    real(8), intent(inout) :: x, y
   ! real(8), intent(inout), dimension(order) :: y 
    real(8), intent(in) :: h !este es el paso 
    real(8) :: deriv2 !problema 2
    !real(8), dimension(order) :: deriv !esta es la función donde esta la derivada 

    !construyo el metodo 
    y=y+deriv2(x,y)*h 
    x=x+h 

end subroutine 