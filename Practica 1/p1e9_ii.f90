program p1e9_ii
    implicit none
    real(4):: a, b
    open(unit=3,file="datose9.txt")
    read(3,*)a, b
    close(unit=3)
    write(*,*)"Los números son"
    write(*,*)a, b 
    call intercambio(a,b)
    write(*,*)a, b
    open(unit=2,file="resultadop1e9_ii")
    write(2,*)a, b
    close(unit=2)
end program p1e9_ii