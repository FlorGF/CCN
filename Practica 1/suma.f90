subroutine suma(a, m , n, s)
    integer(8) , intent(in):: m, n
    real(8), intent(in), dimension(m,n) :: a
    real(8), intent(out), dimension(n) :: s
    integer :: i, j
    do j=1, n
    s(j)=0
    enddo
    do i=1, n
        do j=1, m
            s(i)=s(i)+a(j,i)
        enddo
    enddo 
end subroutine 