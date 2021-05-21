program p1_final
    implicit none 
    real(8) :: f, s_f, h 
    real(8), parameter :: pi=4.0*atan(1.0)
    integer :: i, N, m 
    real(8), allocatable, dimension(:) :: x
    n=10 
    m=20 
    h=2*pi/N 
    allocate(x(N))
    x(1)=-pi
    do i=2, m 
        x(i)=-pi+i*h 
    enddo 
    open(unit=10,file="resultados-GomezFava_1.dat")
    do i=1, m
        call s_fn(x(i), n, s_f)
        write(10,*)x(i), f(x(i)), s_f, f(x(i))-s_f
    enddo 
    
    close(10)
    deallocate(x)




end program 
!--------------------------------------------------------------------------------
function f(x)
    implicit none 
    real(8) :: f 
    real(8), intent(in) :: x 
    real(8), parameter :: pi=4.0*atan(1.0)
    f=x**2-pi**2
end function
!-----------------------------------------------------------------------------
function a_i(i)
    implicit none 
    real(8) :: a_i 
    integer, intent(in) :: i 
    real(8), parameter :: pi=4.0*atan(1.0)
    real(8) :: a, b, integ 
    a=-pi 
    b=pi 
    call trapecio(a, b, i, integ)
    a_i=integ/pi 

end function 

!------------------------------------------------------------------------------------
subroutine trapecio(a, b, i, integ)
    implicit none 
    integer, intent(in) :: i 
    real(8), intent(out) :: integ
    real(8), intent(in) :: a, b 
    real(8), parameter :: pi=4.0*atan(1.0) 
    real(8) :: h, g 
    integer :: n, j 
    real(8) :: sum, x_ant, x_new

    if(i==0)then 
        h=2*pi/15 
    else 
        h=2*pi/(15*i)
    endif 
    
    n=(b-a)/h 
    sum=0.0 
    x_ant=a
    do j=1, n 
        x_new=a+(j-1)*h
        sum=sum+g(x_ant,i)+g(x_new,i)
        x_ant=x_new 
    enddo 
    integ=sum*h/2.0 
end subroutine 
!------------------------------------------------------------------------------
function g(x,i)
    implicit none 
    real(8) :: g 
    real(8), intent(in) :: x
    integer, intent(in) :: i 
    real(8) :: f 
    g=f(x)*cos(i*x)
end function 
!-------------------------------------------------------------------------------
subroutine s_fn(x, n, s_f)
    implicit none 
    real(8), intent(in) :: x 
    integer, intent(in) :: n 
    real(8), intent(out) :: s_f 
    integer :: j 
    real(8) :: a_i 
    s_f=a_i(0)/2 
    do j=1, n 
        s_f=s_f+a_i(j)*cos(j*x)
    enddo 

end subroutine 