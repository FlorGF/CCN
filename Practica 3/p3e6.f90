program p3e6 
    implicit none 
    real(8) ::  tol, per_ideal, tmin, tmax, x
    real(8), parameter :: l=0.2, g=9.8 , pi=4.0*atan(1.0)
    tol=0.0001
    per_ideal=2.0*pi*sqrt(l/g)

   
    tmin=0.7*per_ideal 
    tmax=1.3*per_ideal 

    call biseccion(tmin, tmax, tol, x)
    write(*,*)"El periodo es"
    write(*,*)x
end program 


!-----------------------------------------------------------
subroutine biseccion(a, b, tol, x)
    implicit none 
    real(8), intent(inout) :: a, b, tol 
    real(8), intent(out) :: x
    real(8) :: c, fa, fc
    do 
        call fun(a,fa)
        call fun(c, fc)
        if(abs(b-a)/2<tol .and. abs(fc)<tol)then
            x=c 
           
           exit 
       elseif(sign(1.0_8,fc)==sign(1.0_8,fa))then 
           
           a=c
       else      !cambio los intervalos
           
           b=c
       endif 
       c=(b+a)/2 !cambio c 
       
   enddo 
end subroutine 

!---------------------------------------------
   subroutine fun(t,omega)
    implicit none 
    integer :: i, n_step 
    real(8) :: t_int, omega, t, h
    real(8), dimension(2) :: y 
    n_step=100
    h=t/n_step
    t_int=0.0
    y(1)=0.78 !theta inicial 
    y(2)=0!omega inicial

    do i=1, n_step
        call rk4(t_int, y, h)
    enddo 
    omega=y(2)


end subroutine 