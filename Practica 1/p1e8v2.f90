real function minimo(n, arreglo)
  IMPLICIT NONE
    INTEGER(4) :: n, i
    REAL(4) :: c
    REAL(4), DIMENSION(n) :: arreglo
  c=arreglo(1)
do i=2,15
  if(arreglo(i)<c)then
    c=arreglo(i)
  end if
end do
minimo=c
end function minimo

program p1e8v2
  INTEGER(4), PARAMETER :: n=15
  REAL(4) :: minimo
  REAL(4), DIMENSION(n) :: arr
open(unit=10,file="datose8.txt")
  do i=1,15
    read(10,*)arr(i)
    
  enddo
 
  close(unit=10)

  write(*,*)arr
  write(*,*) "el minimo es"
write(*,*) minimo(n, arr)
end program p1e8v2