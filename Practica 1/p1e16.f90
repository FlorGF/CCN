program p1e16
implicit none
integer(8) :: m, n, sumatoria
write(*,*)"Ingrese el límite inferior de la sumatoria"
read(*,*)m
write(*,*)"Ingrese el límite superior de la sumatoria"
read(*,*)n 
write(*,*)"El resultado de la sumatoria es"
write(*,*)sumatoria(m,n)
end program 