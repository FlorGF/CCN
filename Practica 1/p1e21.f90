program p1e21
    implicit none 
    integer :: i, j, k, n
    real :: sum 
    real, allocatable :: a(:,:), aux(:)
!primero tengo que leer la dimensión y los valores 
    open(unit=10, file="dominante.txt",status="old")
    read(10,*)n 
    
!le pongo las dimensiones a los arreglos
    allocate(a(n,n), aux(n))

!leo la matriz y la escribo
    write(*,*)"La matriz a evaluar es:"
    do i=1,n
    read(10,*)a(i,:)
    write(*,*)a(i,:)
    enddo 
    close(10) 

!de aca en adelante chequeamos si es dominante
    do k=1,n !elijo una fila
        do i=k, n !veo todas las filas menos la anterior para buscar con cual intercambio
            sum=0
            do j=1, n !recorro todos los números de la fila i
                if(k /= j)then
                sum=sum+abs(a(i,j))
                endif 
            enddo 
        if(abs(a(i,k)) > sum)then !chequeo si cumple
            aux(:)=a(k,:)            
            a(k,:)=a(i,:)   !aca intercambio
            a(i,:)=aux(:)
            exit
        elseif(i==n)then
            write(*,*)"La matriz no es diagonalmente dominante" 
            stop
        endif 
        enddo 
    enddo

!aca muestro la matriz dminante
write(*,*)"la matriz domimanteada es:"

    do i=1, n 
        write(*,*)a(i,:)
    enddo 
deallocate(a, aux)
end program 

