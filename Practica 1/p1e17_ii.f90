program p1e17
    implicit none
    integer(8) :: i 
    real(8) :: s, c, e, pi 
    pi=4.0*atan(1.0)
    s=0
    c=pi**4/90.0
    do i=10000, 1, -1
        s=s+1.0/i**4
    enddo 
write(*,*)"El valor de la suma es" 
write(*,*)s 
e=(abs(s-c)/c)*100 
write(*,*)"El error porcentual es"
write(*,*)e 
end program 

