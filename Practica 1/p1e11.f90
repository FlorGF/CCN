program p1e11
    implicit none
    integer(4) :: i
    INTEGER(4), PARAMETER :: n=15
    real(4), dimension(n) :: arreglo
    
    open(unit=10,file="datose8.txt")
  do i=1,15
    read(10,*)arreglo(i) 
  enddo
  close(unit=10)
  write(*,*)arreglo
  call ordenarmenormayor (arreglo, n)
  write(*,*)arreglo
end program p1e11 