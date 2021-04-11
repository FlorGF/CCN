program prueba_raices 
implicit none
real(8) :: x_0, x_1, tol, err, x 
x_0=1.0

tol=0.001 
call newton(x_0, tol, x, err)

write(*,*)"La raiz es:"
write(*,*)x
write(*,*)"Su error es:"
write(*,*)err 


end program 
