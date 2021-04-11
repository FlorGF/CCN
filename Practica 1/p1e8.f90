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

program p1e8
  INTEGER(4), PARAMETER :: n=15
  REAL(4) :: minimo
  REAL(4), DIMENSION(n) :: arr

arr=(/ 23.33, 8.08, 44.99, 21.95, 44.93, 29.82, 41.08, 28.57, 4.39, 43.79, 20.02, 26.59, 41.13, 39.32, 26.85 /)

write(*,*) "el minimo es"
write(*,*) minimo(n, arr)
end program p1e8
