module my_funcs_sistemas 
    implicit none 
    integer, parameter :: order=3 !la cantidad de ecuaciones 

contains 
function f(x)

    real(8), intent(in), dimension(order) :: x !incognitas
    real(8), dimension(order) :: f !el vector de las funciones generatrices
    !esta funcion es para los metodos de punto fijo y gauss seidel
     
!ejemplo del apunte de la parabola y la elipse x=x,y 
    !f(1)=(x(1)**2-x(2)+0.5)/2.0 
    !f(2)=(-x(1)**2-4.0*x(2)**2+8.0*x(2)+4.0)/8.0 

!mismo ejemplo que antes pero con gauss seidel 
  !  f(1)=(x(1)**2-x(2)+0.5)/2.0 
   ! f(2)=(-f(1)**2-4.0*x(2)**2+8.0*x(2)+4.0)/8.0

!ejercicio 6 practica 4 
    f(1)=-9+3*x(3)+3*x(3)**2-x(3)**3+x(2)-X(2)**2-X(1)**2
    f(2)=(6-x(3)-x(2)**2+2*x(1)-x(1)**2+8*x(2))/8.0
    f(3)=(-74+3*x(3)**2-x(3)**3-x(2)+2*x(1)**2-x(1)**3)/(-25.0)


end function 

function fun(x)
    real(8), intent(in), dimension(order) :: x 
    real(8), dimension(order) :: fun 
    !esta funcion es para la subrutina de newton raphson, son las funciones del sistema de ecuaciones

!ejemplo del apunte pag 82 
    !fun(1)=x(1)**2-2.0*x(1)-x(2)+0.5 
    !fun(2)=x(1)**2+4.0*x(2)**2-4.0 

!ejercicio 6 practica 4 
    fun(1)=x(1)**2+x(1)+x(2)**2-x(2)+x(3)**3-3*x(3)**2-13*x(3)+9.0
    fun(2)=x(1)**2-2.0*x(1)+x(2)**2+x(3)-6.0 
    fun(3)=x(1)**3-2*x(1)**2+x(2)+x(3)**3-3*x(3)**2-25*x(3)+74

end function 

function jacobiano(x)
    real(8), intent(in), dimension(order) :: x 
    real(8), dimension(order,order) :: jacobiano 
!ejemplo del apunte pag 82
   ! jacobiano(1,1)=2.0*x(1)-2.0
   ! jacobiano(1,2)=-1.0
   ! jacobiano(2,1)=2.0*x(1)
   ! jacobiano(2,2)=8.0*x(2)
!ejercicio 6 
    jacobiano(1,1)=2*x(1)+1.0
    jacobiano(1,2)=2.0*x(2)-1.0
    jacobiano(1,3)=3.0*x(3)**2-6.0*x(3)-13.0 
    jacobiano(2,1)=2*x(1)-2.0 
    jacobiano(2,2)=2*x(2)
    jacobiano(2,3)=1.0 
    jacobiano(3,1)=3*x(1)**2-4*x(1)
    jacobiano(3,2)=1.0 
    jacobiano(3,3)=3*x(3)**2-6*x(3)-25.0
end function 
end module 