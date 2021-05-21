program p1_final28 
    use funciones 
    implicit none 
    real(8) ::  s_f , h
    integer :: i, n 
    real(8), parameter :: pi=4.0*atan(1.0) 
    real(8),  dimension(20) :: x 
    
    n=10 !esto hay que generalizarlo 
    h=2*pi/20 
    do i=1, 21 
        x(i)=-pi+h*(i-1) 
    enddo 
    
    open(unit=10,file="resultados-p1-final28.dat")
    do i=1, 20
         call s_fn(x(i), n ,s_f)
         write(10,*)x(i), f(x(i)), s_f, f(x(i))-s_f
    enddo 
        


end program 
!----------------------------------------------------------------------------------

!----------------------------------------------------------------------------------
function a_i(i)
    implicit none 
    real(8) :: a_i 
    integer, intent(in) :: i 
    real(8), parameter :: pi=4.0*atan(1.0)
    real(8) :: a, b, integ 
    a=-pi 
    b=pi 
    call trapecio(a, b, integ, i)
    a_i=integ/pi 
end function 
!---------------------------------------------------------------------------------
    !MÉTODO DEL TRAPECIO
subroutine trapecio(a, b, integ, i)
    use funciones 
    implicit none  
    real(8), intent(in) :: a, b 
    real(8), intent(out) :: integ 
    real(8) :: h, sum, x_ant, x_new
    integer, intent(in) :: i 
    real(8), parameter :: pi=4.0*atan(1.0)
    integer ::  j , n 
    
    if(i==0)then 
        h=2*pi/15 
    else 
        h=2*pi/(15*i)
    endif 
    N=(b-a)/h 
   
    x_ant=a  
    sum=0.0 !seteo inicial 
    integ=0 
    
    do j=1, N 
        x_new=a+(j-1)*h
        
        sum=sum+(g(x_ant,i)+g(x_new,i))
        
        x_ant=x_new !reseteo de nuevo
    enddo 
    integ=sum*h/2.0 
end subroutine 
!----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
subroutine s_fn(x, n, s_f)

    implicit none 
    real(8),intent(in) ::  x
    integer, intent(in) :: n
    real(8), intent(out) :: s_f 
    integer :: j 
    real(8) :: a_i

    s_f=a_i(0)/2.0 
    do j=1,n 
        s_f=s_f+a_i(j)*cos(j*x)
    enddo 


end subroutine 