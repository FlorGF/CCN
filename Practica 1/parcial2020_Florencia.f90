program parcial2020
    use def_N
    implicit none 
    real(8), allocatable, dimension(:) :: r, x
    integer, allocatable,dimension(:) :: c
    integer:: i
    integer, parameter :: s=11
    allocate(x(s), c(s))
    open(unit=10, file="parcial1-2020.dat", status="old")
    !leo la dimensión 
    read(10,*)N 
    !le defino la dimension a los arreglos 
    allocate(r(N))
    !leo los arreglos
    do i=1, N 
        read(10,*)r(i)
    enddo 
    close(10) 
    !write(*,*)N 
    ! write(*,*)"El arreglo es:"
     !write(*,*)r 

    !llamo a la subrutinas 
    call histograma(r, s, x, c)
    open(unit=11,file="resultados-gomezfava.dat")
    do i=1, s 
        write(11,*)x(i), c(i)
    enddo 
    close(11)

    deallocate(r)
    deallocate(x)
    deallocate(c)
end program 