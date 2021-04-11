module interpolacion 
    implicit none 

contains 
real(8) function lagrange( x_dat, y_dat, x)
real(8), intent(in) :: x_dat(:), y_dat(:), x 
integer :: k, n 
real(8) :: sum 
n=size(x_dat)
sum=0.0 
do k=1, n 
    sum=sum+y_dat(k)*A_k(x,x_dat,k)/A_k(x_dat(k),x_dat,k)
enddo 
lagrange=sum 



end function 

real(8) function A_k(x, x_dat, k)
    real(8), intent(in) :: x_dat(:), x 
    integer, intent(in) :: k 
    real(8) :: prod 
    integer :: j , n 
    n=size(x_dat)
    prod=1.0
    do j=1, n 
        if(j==k)cycle 
        prod=prod*(x-x_dat(j))
    enddo 
    A_k=prod 

end function 
end module 