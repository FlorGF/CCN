real (8) function deriv2(x,y) !esta es la funcion deriv del problema 2
    implicit none 
    real(8), intent(in) :: x, y 
    real(8) :: deriv 
    deriv=-2.0*x**3+12.0*x**2-20.0*x+8.5

    end function deriv2 