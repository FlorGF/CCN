real(8) function logaritmo_1(N,x)
implicit none
integer(8) :: N, i
real(8) :: x, s
s=0

    do i=1, N 
        s=s+((-1)**(i-1)*(x**i))/i
   enddo


logaritmo_1=s
end function 