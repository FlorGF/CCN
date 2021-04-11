module my_funcs 
implicit none 
integer, parameter :: order=2 !cuantas variables tengo que encontrar 
 

!aca hago deriv
contains 
function deriv(x, y)
    real(8), intent(in) :: x
    real(8),intent(in), dimension(order) :: y !se define como vector por los distintas cosas de la edo 
    real(8), dimension(order):: deriv
    !real(8) :: k, b, m1, m2 , l1, l2, m
    real(8) :: m, k1, k2, pi , alpha
    pi=4.0*atan(1.0)
    alpha=pi/6.0
    m=0.5 
    k1=0.12*9.8*sin(alpha)
    k2=9.8*sin(alpha)
   ! real(8) :: g, l
   ! l=0.2 
   ! g=9.8
   ! k=100.0
   ! m1=0.5
   ! m2=0.5
   ! l1=0.5
    !l2=0.5 
   ! b=2.0
   ! m=0.25

    !aca tengo que definir que es deriv si es necsario declarar constantes
 !---------------------------------------------------------------   
    !problema 5
    !deriv(1)=y(2)
    !deriv(2)=-9.8/0.5*y(1)
!-------------------------------------------------------------
    !problema 8 
    !deriv(1)=y(2)
   ! deriv(2)=-k*y(1)/m-b*y(2)/m
!-----------------------------------------------------------
    !problema 10 
   ! deriv(1)=y(3)
    !deriv(2)=y(4)
   ! deriv(3)=-b*y(3)/m 
   ! deriv(4)=-9.8-b*y(4)/m

!---------------------------------------------------------------------
    !problema 7 
    !deriv(1)=y(3)
    !deriv(2)=y(4)
    !deriv(3)=(m2*b*y(4)*sen(y(1)))/(m1+m2-m2*cos(y(3)**2))
    !deriv(4)=(-cos(y(1))*m2*b*y(4)**2*sen(y(1))+m2*9.8*sen(y(1))*cos(y(1)))/(m1+m2-m2*cos(y(3)**2))-9.8*sen(y(1))/b

!------------------------------------------------------------------------------
    !problema 6 
    !deriv(1)=y(3)
    !deriv(2)=y(4)
   ! deriv(3)=(-9.8*(2*m1+m2)*sin(y(1))-m2*9.8*sin(y(1)-2*y(2))-2*sin(y(1)-y(2))*m2*(y(4)**2*l2+y(3)**2*l1*cos(y(1)-y(2))))/(l1*(2*m1+m2-m2*cos(2*(y(1)-y(2)))))
   ! deriv(4)=(2*sin(y(1)-2*y(2))*(y(3)**2*l1*(m1+m2)+9.8(m1+m2)*cos(y(1))+m2*y(4)**2*l2*cos(y(1)-y(2))))/(l2*(2*m1+m2-m2*cos(2*(y(1)-y(2)))))

!----------------------------------------------------------
    !problema 6 practica 3
   ! deriv(1)=y(2)
   ! deriv(2)=-g*sin(y(1))/l

!--------------------------------------------------------
    !problema 5 practica 3 
    deriv(1)=y(2)
    deriv(2)=20*exp(-0.2*x)/m-k1-k2 

end function 



end module 
