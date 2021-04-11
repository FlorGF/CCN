subroutine rk4(x,y,h)
      ! Runge-Kutta de orden 4
      use my_funcs_parcial
      implicit none
      !Declaracion de argumentos
      real(8), intent(inout) :: x, y(order)
      real(8), intent(in) :: h

      !Declaracion de variables internas
      real(8), dimension(order) :: k1, k2, k3, k4

      k1 = deriv(x, y)
      k2 = deriv(x+h/2.0, y+h/2.0*k1)
      k3 = deriv(x+h/2.0, y+h/2.0*k2)
      k4 = deriv(x+h, y+h*k3)

      y = y + (k1+2.0*(k2+k3)+k4)*h/6.0
      x = x + h
end subroutine 
