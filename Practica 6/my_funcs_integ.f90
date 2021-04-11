module my_funcs_integ
    implicit none 
    contains 

    function f(x)
        real(8) :: f 
        real(8), intent(in) :: x 
        !funcion de prueba 
        f=x**2 

    end function






end module 