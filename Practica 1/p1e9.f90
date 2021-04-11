program p1e9_i
    implicit none 
    real(4) :: a, b
    write(*,*) "Ingrese el primer número"
    read(*,*)a
    write(*,*) "Ingrese el segundo número"
    read(*,*)b
    write(*,*) "Los números ingresados son"
    write(*,*)a, b
    call intercambio(a,b)
    write(*,*)a, b
    open(unit=2,file="resultadop1e9")
    write(2,*)a, b
    close(unit=2)
    end program 