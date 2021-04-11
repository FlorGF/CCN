module funciones 
    implicit none 
    contains 
real(8) function f(x)
    real(8) :: x
    real(8), parameter :: pi=4.0*atan(1.0)


   ! f=sin(log(2*x))
    f=(x-1)*(x-sqrt(2.0))*(x-2*pi)


end function 

function df(x)  !para el método de newton 

    real(8) :: df, x
    df=cos(log(2*x))/x

end function 



end module 
