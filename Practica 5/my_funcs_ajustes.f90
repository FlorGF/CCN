module my_funcs_ajustes
    implicit none 
    contains 
    function f(x, n_param) !esta es para el método de ajuestes
        integer, intent(in) :: n_param 
        real(8), intent(in) :: x
        real(8):: f(n_param)
        !---------------------------------
        !ejemplo de las diapositivas, valido tambien para el ejercicio dos y tres(en el ejemplo y ejercicio dos van solo f1 y f2)
        f(1)=1 
        f(2)=x 
        f(3)=x*x


    end function 
    function f_n(x, a, n_param) !esta es para el metodo de newton
        integer, intent(in) :: n_param 
        real(8), intent(in) :: x, a(n_param)
        real(8):: f_n
        f_n=a(1)-a(1)*exp(-a(2)*x)
    end function 
    function jacobiano(x, a, n_param)
        integer, intent(in) :: n_param 
        real(8), intent(in) :: x, a(n_param)
        real(8):: jacobiano(n_param)
        jacobiano(1)=1-exp(-a(2)*x)
        jacobiano(2)=a(1)*x*exp(-a(2)*x)






    end function 

        


end module