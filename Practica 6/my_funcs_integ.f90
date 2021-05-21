module my_funcs_integ
    implicit none 
    contains 

    function f(x)
        real(8) :: f  
        real(8), intent(in) :: x 
        !funcion de prueba 
        !f=x*x

        !función ejercicio 4, practica 6 
        !f=sqrt(1-x*x)

        !ejercicio 5 practica 6 
        !f=sin(exp(x))

        !ejercicio 6 practica 6
        f=cos(x)

        !ejercicio 8 practica 6, masa 
        !f=1.0 



    end function

  






end module 