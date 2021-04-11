program p1e20
    implicit none
    integer(8) :: i, fac, n
    write(*,*)"Ingrese el número para hacerle el factorial"
    read(*,*)n 
    fac=1
    do i=1, n 
        fac=fac*i 
    enddo 
    write(*,*)"El factorial es" 
    write(*,*)fac
end program 