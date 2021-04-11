program parcial_ej1 
    implicit none 
    real(8), dimension(2) :: y
    real(8) :: t, h, v_i, x_i, t_i, v_new, v_old
    real(8), dimension(10) :: x_max , t_max
    integer :: k, i 
    open(unit=10,file="datos_parcialej1.dat",status="old")
    read(10,*)h 
    read(10,*)t_i
    read(10,*)x_i
    read(10,*)v_i

    close(10)
    !seteo las condiciones iniciales 
    y(1)=x_i
    y(2)=v_i 
    t=t_i 
    v_old=v_i 
    k=1.0 


    do 
!	write(*,*)"ee"
        call rk4(t,y,h)
        v_new=y(2)
        if(sign(1.0_8,v_new)<0 .and. sign(1.0_8,v_old)>0)then 
            x_max(k)=y(1)
            t_max(k)=t 
            k=k+1
	        v_old=v_new
        else 
            v_old=v_new 
        endif 
        if(k==11)then 
            exit 
        endif 
    enddo 

    open(unit=11,file="resultados_ej1.dat")
    do i=1, 10 
        write(11,*)t_max(i), x_max(i)
    enddo 





end program 
