subroutine matriz(m, n , a)
    integer(8), intent(in) :: m, n 
    real(8), intent(out), dimension(m,n) :: a 
    integer :: i, j

do i=1, m
    do j=1, n 
        a(i,j)=1.0/(1.0+j)
        if(mod(2*i+j,3)==0)then 
            a(i,j)=-a(i,j)
        endif
    enddo
enddo 

end subroutine 