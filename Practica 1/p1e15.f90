program p1e15
    real(8) :: a, b, c, x_1, x_2 
write(*,*)"Ingrese a"
read(*,*)a
write(*,*)"Ingrese b"
read(*,*)b
write(*,*)"Ingrese c"
read(*,*)c
    x_1=(-b+sqrt(b**2-4*a*c))/(2*a)
    x_2=(-b-sqrt(b**2-4*a*c))/(2*a)
write(*,*)"Los ceros son"
write(*,*)x_1, x_2
end program 