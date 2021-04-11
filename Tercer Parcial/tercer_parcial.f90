program tercer_parcial
    use fitting 
    implicit none 
    real(8), allocatable :: x_dat(:), y_dat(:), a(:), aux(:)
    integer :: n_dat, i, n_param
    real(8) :: tol 
    real(8) :: sum 
   
    open(unit=11,file="datos_parcial_3.in", status="old")
    read(11,*)n_dat 
    allocate(x_dat(n_dat),y_dat(n_dat), aux(n_dat))
    do i=1, n_dat
        read(11,*)x_dat(i), y_dat(i)
    enddo
    read(11,*)tol 
    close(10)
    write(*,*)"señal5"


   ! sum=0.0
    n_param=1.0 
    do 
        allocate(a(n_param))
        write(*,*)"señal 1"
        call calculo(x_dat, y_dat,n_dat, n_param, a, aux)

        write(*,*)"señal 2"
        
        write(*,*)"señal 22"
        sum=0.0
        do i=1, n_dat
            write(*,*)y_dat(i), aux(i)
            sum=sum+abs((y_dat(i)-aux(i)))**2
            write(*,*)sum, "suma"
        enddo 
        if(sum<=tol)then 
            exit 
        else 
            n_param=n_param+1 
           
        endif 
        deallocate(a)
    enddo 
    
    write(*,*)"señal6"
   !allocate(a(n_param))
    write(*,*)"la cantidad de parametros usados es"
    write(*,*)n_param 
    write(*,*)"los parametros son"
    write(*,*)a 
    
!--------------------------------------------------------------------
   






end program 
!-------------------------------------------------------------
