subroutine ordenarmenormayor (arr, n)
    integer(4) :: n, i ,j
    real (4), dimension(n) :: arr
    real(4) :: a
    do i=1, n-1
        do j=i+1, n
            if(arr(i)>arr(j))then
                a=arr(i)
                arr(i)=arr(j)
                arr(j)=a
            end if 
        enddo
    enddo
    end subroutine 